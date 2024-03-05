Wojciech Rzepliński (438709)
# Zadanie 1

Kroki, które wykonałem w celu zmodifykowania gry:

1. Wyszukałem wszystkie wystąpienia słowa "passphrase", które fragmenty kodu odwołują się do tego napisu. To wskazało funkcję `doPC_attemptPassword`, która wywołuje `pc_check_luks_password`. Wewnątrz niej, z widoku pamięci `.data` zauważyłem sekwencję bajtów, która zwiera takie same bajty jak wpisane hasło.
2. Poniżej globalnej zmiennej oznaczającej hasło znajdował się długi (do 3600 znaków) łańcuch znaków, który zawierał cały tekst konsoli. Sprawdziłem, jakie funkcje odwołują się do niego i znalazłem funkcję `pc_putch`. Z analizy kodu wynika, że liczy ona długość tekstu konsoli i wstawia na końcu napisu gwiazdkę.
3. Jest ona wywoływania przez `pc_keypress`. Odpowiada ono za akcję po wciśnięciu klawisza. Jest on przechowywany w rejestrach `rcx` i `r9`. Okazuje się, że wewnątrz `pc_putch` w r9 nadal znajduje się wartość wciśniętego znaku.
4. Zatem odpowiednią łatką jest zastąpienie polecenia ```  mov     word ptr [rcx+rdx], 2Ah``` na

    ```  mov     [rcx+rdx], r9b```

# Zadanie 2

Sposób hashowania hasła opisuje poniższy kod (z funkcji `hash_password`):
```c
int hash = 0;
for (int i = 0; i < repetitions; i++){
    hash = hash_table[ uint8_t(hash ^ password[i]) ] ^ (hash >> 8);
}
```
Gdzie `hash_table` to znana tablica 256 wartości `uint64_t` (możemy wartości odczytać z oprogramowania IDA).

Widzimy, że najbardziej znaczące 8 bitów będzie zawsze wynikiem wartości z tablicy hashy oraz zer, zatem znając prawidłowy hash `0x3E4C3F5BDF80E7D3`, wiemy, wartość hashu z tablicy musi mieć również `3E` na najbardziej znajczących 8 bitach. Tak się składa, że jest jedna taka wartość w tablicy (3E847F9DC45F37C0 na indeksie 112). Zatem wiedząc, że poprzedni hash (z iteracji 7 pętli) oraz znana nam wartość z tablicy są xorowane do otrzymania kolejnego hashu, możemy tą operację odwrócić (korzystając z wkłaściwości xor'a):
```python
# z pierwotnej funkcji hashującej
hash_8 == hash_table[idx_8] ^ (hash_7 >> 8)
# po przekształceniu xora
(hash_7 >> 8) == hash_table[idx_8] ^ hash_8
# i po zamianie przesunięcia bitowego
hash_7 ~= (hash_table[idx_8] ^ hash_8) << 8 # najmniej znaczące 2 bajty się nie będą poprawne, ale najbardziej znaczące 2 bajty będą
```
Możemy w ten sposób powtórzyć tą operację i znaleźć tablicę indeksów dla 8 iteracji pętli.
Teraz aby znaleźć wartości znaków hasła, zauważmy, że pierwszy hash (który moglibyśmy oznaczyć jako `hash_0`) ma wartość 0. A będziemy go xorować z kolejnymi wartościami z tablicy (które już znamy), tzn. idąc razem z funkcją i indeksami do przodu, możemy odtworzyć pełną wartość hasha w każdej iteracji pętli hashującej.
```python
saved_hash_values = []
forward_hash = 0
for i in range(8):
    forward_hash = hash_table[hash_table_index[i]] ^ (forward_hash >> 8)
    saved_hash_values.append(forward_hash)
```

Teraz znając wartości hashy oraz wykorzystywane indeksy tablicy hashy w każdym powtórzeniu pętli, możemy odtworzyć znaki hasła:
 
```python
# z pierwotnej funkcji hashującej
hash_table_indices[i] = uint8(saved_hash_values[i]) ^ password[i]
# po przekształceniu xora
password[i] = uint8(saved_hash_values[i]) ^ hash_table_indices[i]
```
Co daje nam hasło "p455w04d".

Zgadza się ono z szybkim sprawdzeniem hasła w funkcji `pc_check_luks_password`, które odfiltrowuje hasła, które zawierają cyfry w złych miejscach.

# Zadanie 3

Główna nieskończona pętla programu z funkcji main, wywołuje między innymi funkcję `update_overworld`, która sprawdza wartość globalnej zmiennej `gScript`. Jeśli jest ona niezerowa, to znaczy, że znajduje się w niej wskaźnik na akcję fabuły programu, np. na `mom_walk_up` (akcja po wejściu do salonu na początku gry). Funkcja ta umieszona ona zostaje w `gScript` przez funkcję `on_enter_1`, która zostaje wywoływana w momencie wchodzenia do salonu.

Funkcja `on_enter_1` wykonuje sprawdzenie stanu globalnego porzez wywołania `check`. `check` sprawdza bity pewnej globalnej zmiennej, tzn. czy zwraca wartośc jednego bitu na tej pozycji. Po analizie okazało się, że wartość tej stałej globelnej możemy zmienić poprzez funkcje `mark` oraz `clear` (podając jako argument numer bitu). Sposób wykorzystywania tych funkcji wskazuje, że zmienna ta służy ona oznaczania postępu w fabule. Debugowanie pokazało, że po rozmowie z mamą wartość tej zmiennej globalnej jest równa `0x747`.

Zatem modyfikacja kodu polega na usunięciu wywołań funkcji `mark` na w funkcji `SDL_main`, a wstawieniu tam bezpośredniego ustawienia zmiennej globalnej na wartość `0x747` oraz wielu `nop`.
```asm
mov eax, 0x3
call check
mov eax, 0x5
call check
...
```
Po podmianie zostało zamienione na:
```asm
mov     rax, 747h
or      cs:dialog_state, rax
nop
nop
...
```

# Zadanie 4

Strategia przeciwnika wywołuje się w funkcji `strategy_endless_healing`. Wykonuje ona akcję naszego pokemona za pomocą `execute_action(0)`, które odczytuje z pamięci numer wybranej przez nas akcji (od nas dwie do wyboru, od 0 do 1) oraz jej typ (atak (0), albo item z sakiewki (1)) i zapisuje je w tablicy efektów akcji z pomocą funkcji `later`. Tablica wygląda w formie par: (wskaźnik na zmieniane pole pokemona, nowa wartość), ponieważ oprócz zmiany poziomu życia pokemona nasze ataki słabną. Następnie strategia odczytuje te efekty, i jeśli poziom życia przeciwnikia spadnie do zera, postanawia użyć mikstury leczniczej. W przeciwnym wypadku normalnie wybiera strategię za pomocą `strategy_status()`. Nieuczciwą strategią jest funkcja `predict`, która potrafi przewidzieć nasz ruch zanim on się wykona.

Nieuczciwa przewaga pojawia się również w funkcji `who_goes_first()`, która sprawia, że jeśli przeciwnik (po wybraniu naszego ruchu, a przed rozpoczęciem wykonywania go) używa mikstury, to odbywa się ona przed naszym atakiem. Zapisuje ona wartość w pewnej zmiennej globalnej (nazwałem ją "change_battle_order" nie wiem czy dobrze), która zmienia stronę, która wykonuje teraz akcję na inną za pomocą xora w `go_execute`.

Zmiana, którą zaaplikowałem polega na usunięciu ifa, który sprawdza,czy wynik funkcji `predict` (poziom życia przeciwnika po ataku), jest większy niż zero. Zamieniłem go na jmp, czyli niezależnie od jego wartości wykona się `strategy_status()`.
