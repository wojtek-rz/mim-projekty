# Wojciech Rzepliński 438709

## Zadanie 1 NCG
#### flag{still-not-a-csprng}

Dwie główne podadności systemu:
- stała wartość modulo, które jest potęgą dwójki
- xor jako uniknięcie liniowości w generowaniu liczb

Pomimo, że xorowanie niweluje liniowość, to działa on bardzo lokalnie - ostatnie bity c wpływają tylko na odpowiadające bity wyniku mnożenia. Podobnie możenie na ostatnich bitach (najmniej znaczących) jest prostą, lokalną operacją w ten sposób, że bardziej znaczące bity nie mają wpływu na nie.

Rozwiązanie polega na sprytnym brute-force. Ponieważ ostatnie n bitów (np. ostatnie 4) wynika tylko z ostatnich n bitów stałych `a` i `c`. Sprawdzamy wszystkie kombinacje ostatnich bitów `a` i `c` (2 ** (2n)) i filtrujemy z nich te, które mogą pasować do liczb, które dostaliśmy na wejściu (patrzymy na ostatnie n bitów tych liczb).

Otrzymumemy kilka (najczęściej osiem) pasujących par `a` i `c`. Doświadczalnie zauważamy, że ostatni bit w każdej patrze dla `a` i dla `c` jest stały, a zatem ustaliliśmy z pewnością ostatni bit liczby. Jeśli nie otrzymaliśmy jednoznacznie ostatniego bitu, możemy zwiększyć liczbę bitów generowanych `a` i `c`.

Skoro ustaliliśmy ostatni bit, to możemy operację powtórzyć dla ostatnich 5 bitów, gdzie ostatni mamy już ustalony. Posuwając się w ten sposób do tyłu w każdym kroku będziemy mieć skończoną, niewielką liczbę kombinacji `a` i `c` do przeanalizowania.


## Zadanie 2 CBC
#### flag{sh0rt-fl4G}
#### flag{p4dding-1s-h4rd-but-re4ly-just-s1gn-y0ur-c1phert3xts}

Główną podatnością w zadaniu jest niezabezpieczanie w żaden sposób wektora inicjalizacyjnego. Dzięki temu odbiorca może go dowolnie podmienić, a jeśli zna treść choć jednej ustalonej wiadomości (np. wiadomości powitalnej), to może tak spreparować IV za pomocą xorowania, że prześle dowolną wiadomość.

Jednak do odszyfrowania flag potrzebujemy czegoś więcej. Serwer odpowiada nam jedynie informacją o prefixie otrzymanej wiadomośc. Na przykład, jeśli wiadomośc zaczyna się od słowa "flag?" to odpowie flagą, której zaszyfrowaną wersję możemy poznać poprzez spreparowany IV z pierwszego akapitu. A jeśli prefix jest inny, to serwer odpowie nam zaszyfrowaną wiadomością o treści "nie rozpoznaję takiego polecenia".

Jednak tak się składa, że w skrypcie po odszyfrowaniu została umieszczona funkcja z pythona "strip", która w połączeniu z wiadomościami powyżej pozwala nam dowolnie modyfikować prefiks wiadomości, jeśli znamy jej pierwsze kilka bajtów. Np. jeśli znamy pierwsze 4, to wystarczy przesyłać ten komunikat z kolejnymi IV różniącymi się na 5 bajcie, aż otrzymamy flagę i dowiemy się, że wysłaliśmy zaszyfrowaną wiadomość "flag?", którą serwer zrozumiał. Teraz możemy zrobić następujący trick:
```
    known_bytes|unknown_bytes
           flag|unknown_bytes
```
czyli tak dobrać IV, by pierwsze bajty ustawić na spacje i aby przesunąc słowo kluczowe flag żeby kończyło się w na bajcie, którego już nie znamy. Modyfikując ten bajt przez IV będziemy dostawać odpowiedzi serwera "unknown command" aż do momentu, kiedy dostaniemy flagę.

Znalezienie pierwszych 4 bajtów umożliwia nam błednie napisany unpad, który umożliwia skasowanie z wiadomości dowolnej ilości bajtów (do 256). Możemy znależć taką wiadomość, której ostatni bajt będzie wynosił `x` i jeśli odeślemy tą zaszyfrowaną wiadomość bez paddingu, unpad usunie nam ostatnich `x` bajtów.

Schemat odszyfrowania pierwszych bajtów bloku wygląda następująco. Sklejamy ze znanych komponentów wiadomość:

```
    encrypted_hash_string + block + encrypted_unpad_trick
    # po odszyfrowaniu przed unpad powinniśmy dostać:
          (spacje) hash? + nieznane + nieważne + 47
    # po usunięciu paddingu:
          (spacje) hash? + c <--- jeden bajt z nieznanego bloku
```

jeśli potem powysyłamy wiadomości z różnymi jednobajtowym hashami (o treści "hash?\x00" np.), to w końcu dostaniemy nasz hash ze uciętego nieznanego bloku i będziemy wiedzieć, jaki jest pierwszy bajt bloku.

Następnie możey powtórzyć kroki, znając pierwszy bajt bloku. Wtedy ucinamy blok tak, żeby zostały dwa bajty, zapisujemy odpowiedź serwera, a następnie przepytujemy serwer z potencjalnie kolejnych 256 hashy z prefikasmi długości dwa, w których pierwszy bajt znamy.

Jak znajdziemy pierwsze 4 bajty to możemy wrócić do znajdywania kolejnych bajtów z pierwszego sposobu.