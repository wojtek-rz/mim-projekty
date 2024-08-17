# Opis języka
### __Autor__: Wojciech Rzepliński (438709)


Język Emilia jest imperatywnym językiem programowania podobnym do języka Python, wzbogaconym o statyczne typowanie i uproszczony system inferencji typów. Cechuje się wysoką czytelnoscią, którą zapewniają między innymi typy oraz oryginalne elementy składniowe. Program jest listą instrukcji wykonywaną sekwencyjnie, a instrukcje mogą być deklaracjami zmiennych. 

## Instalacja
Skorzystanie z `Makefile` w projekcie:

```
make
```

Utworzy plik `emilia-lang-exe` w katalogu projektu, jest to plik wykonywalny. Aby uruchomić przykłady można skorzystać ze skryptu:

```
./run_examples.sh
```

Projekt korzysta z narzędzia `stack`, które wczytuje plik konfiguracyjny `package.yaml` i generuje pliki `stack.yaml` oraz `emilia-lang.cabal`. Aby zbudować i uruchomić projekt narzędziem `stack` należy wpisać:
```
stack build
stack exec emilia-lang-exe
```

Pobiera on jednak konkretną wersję ghc z internetu, szybszą opcją jest skorzystanie z cabala.
Aby zbudować i uruchomić narzędziem cabal: 
```
cabal build emilia-lang-exe
cabal exec emilia-lang-exe
```

## Uruchomienie


```bash
Usage: emilia-lang-exe <file>
       emilia-lang-exe --repl
       emilia-lang-exe
If no file is provided, executable will read from stdin.
```

Została zaimplementowana obsługa REPL'a (read eval print loop), którą należy uruchomić za pomocą opcji `--repl`.

## Gramatyka

Znajduje się w pliku `grammar.cf`

## Opis głównych cech

### Statyczne typowanie

Aby jednak ulżyć programiście w pisaniu typow został wprowadzony
mały system inferencji typów. 
Pozwala on z deklaracji `x := 15` odczytać, że zmienna `x` ma typ `Int`. Oznacza to, że każda zmienna musi zostać zainicjowana i nie może mieć wartości `Void`. Funkcje muszą mieć zadeklarowane typy argumentów oraz typ wyniku, aby uniemożliwić nietypowe rekurencyjne konstrucje o dziwnym typie. Funkcje oraz ich typy są "pełnoprawnymi obywatelami"  i mogą być przekazywane jako argumenty innych funkcji oraz być zwracane przez inne funkcje.

### Funkcje anonimowe

Język Emilia opiera się w bardzo dużym stopniu na funkcjach anonimowych. Deklarowanie zwykłej funkcji polega na pisaniu funkcji anonimowej i przypisanie jej do zmiennej, co jest krótkie i wygodne.
```go
// deklaracja funkcji przyjmującej Int i zwracającej Int
addTen := (x | Int) -> Int:
    return x + 10
```

Problemem w takim podejściu jest jednak rekurencja. Ponieważ funkcja anonimowa nie wie, do jakiego identyfikatora zostanie przypisane (jeśli w ogóle), lambdy nie są rekurencyjne. Dlatego został wprowadzony wyjątek, który mówi, że jeśli wyrażenie w deklaracji zmiennej będzie zawierać
funkcję lambda, to wtedy wewnątrz niej identyfikator z deklaracji będzie widoczny. Po spojrzeniu na przykład jest bardzo naturalne.

```go
power := (n | Int) -> Int:
    if n = 1:
        return 1
    else:
        return n * power(n - 1)
```

Język posiada również lambda wyrażenia, które posiadają prostą składnię:
```go
lambda_expr := (x | Int) -> Int ( 2 * x)
```
Rekurencja w lambda wyrażeniach nie jest dozwolona.

### Indentacja jako oznaczanie początku i końca bloku
Skorzystałem z opcji 
"layout" generator BNFC, która pozwala zamiast nawiasów klamrowych 
oraz średników wykorzystać wcięcie (indentacje) jako oznaczenie początku i końcu bloku.
Oznacza to, że moją składnię definiuję za pomocą "{", "}" i ";", ale parser umożliwia alternatywny sposób oznaczania bloków przez indentację, a znaki klamrowe i 
średniki zostają dodane w warstwie pośredniej. Zarezerwowany znak, po którym można napisać wcięcie zamiast nawiasu klamrowego, to ":".

Składnia zdefiniowana w gramatyce:
```go
{
  if a >= 80 :
  {
    grade := 5;
    printInt (grade);
  }
  else :
  {
    pass;
  };
  res := True;
  printStr ("Grading done.");
}
```
Składnia alternatywna, za pomocą indentacji:
```go
if a >= 80:
    grade := 5
    printInt(grade)
else:
    pass
res := True
printStr("Grading done.")
```
Obydwie formy są przyjmowane przez parser.


### Deklaracje i przypisania

Częstą trudnością dla początkujących programistów jest operator 
deklaracji oraz przypisania w większości języków programowania, czyli znak równości `=`. Nie oddaje on asymetrycznej istoty tych operacji,
co w połączeniu z dynamicznym typowaniem w Pythonie utrudnia zrozumienie
działania tego operatora. Dlatego w języku Emilia deklaracja jest oznaczana operatorem asymetrycznym `:=`, a przypisanie `<-`. Zwolniony znak równości stał się operatorem logicznym oznaczającym równoważność.

## Przykłady składniowe

Wiele przykładowych składni znajduje się w folderze `codes/good` oraz `codes/bad`, których odpaleniem zajmuje się skrypt `run_examples.sh`. 

### Punkty 1,2,3

```go
counter := 0 // Deklaracja zmiennej globalnej.
text := "Ala ma kota"
boolValue := False
coutner <- counter + 1 // Operacja przypisania.
```
Realizuje:
- 1. Co najmniej trzy typy wartości
- 2. Literały, arytmetyka, porównania.
- 3. Zmienne, operacja przypisania

### Punkty 4, 5

```go
if age > 18:
    printStr("Pass")
else:
    i := 15
    while i > 0:
        i <-i - 1
        printStr("...")
```
Realizuje:
- 4. Jawne wypisywanie wartości na wyjście
- 5. while, if (z else)

### Punkty 6,7

```go
fun := (valueArg | Int, refArg | * Str) -> Int:
    printStr(refArg)
    return valueArg + 1
```

Realizuje:
- 6. Funkcje lub procedury (bez zagnieżdżania), rekurencja.
- 7. co najmniej dwa sposoby przekazywania parametrów (przez zmienną, przez wartość)

### Punkty 9, 10, 11
Przykłady w folderze `codes`:
- 9. Przesłanianie identyfikatorów ze statycznym ich wiązaniem
- 10. Obsługa błędów wykonania, np. dzielenie przez zero - tutaj nie ma przykładu
- 11. Funkcje przyjmujące i zwracające wartość dowolnych obsługiwanych typów (tylko funkcje – jak w języku C).
### Punkty 12, 13
Do 30 punktów:
- 12. Statyczne typowanie (tj. zawsze terminująca faza kontroli typów przed rozpoczęciem wykonania programu)
- 13. Dowolnie zagnieżdżone definicje funkcji / procedur z zachowaniem poprawności statycznego
wiązania identyfikatorów (jak w Pascalu)

### Punkt 17
```
chainFuns := (f | (Int) -> Int, g | (Int) -> Int) -> (Int) -> Int:
    chain := (x | Int) -> Int:
        return f(g(x))
    return chain


chainedFun := chainFuns((x | Int) -> Int (2 * x), 
                        (x | Int) -> Int (21 + x))

printInt(chainedFun(0)) // should be 42
```
- 17. Funkcje jako parametry, zwracanie funkcji w wyniku, domknięcia à la JavaScript, funkcje anonimowe 


## Tabelka

Oczekiwana liczba punktów: 30 pkt.

```
  Na 15 punktów
  01 (trzy typy)
  02 (literały, arytmetyka, porównania)
  03 (zmienne, przypisanie)
  04 (print)
  05 (while, if)
  06 (funkcje lub procedury, rekurencja)
  07 (przez zmienną i przez wartość)
  Na 20 punktów
  09 (przesłanianie i statyczne wiązanie)
  10 (obsługa błędów wykonania)
  11 (funkcje zwracające wartość)
  Na 30 punktów
  12 (4) (statyczne typowanie)
  13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
  17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)

Razem: 30
```
