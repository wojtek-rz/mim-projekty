# Opis języka
__Autor__: Wojciech Rzepliński (438709)

Dzień dobry, poniżej przedstawiam założenia języka, 
przykładowe kody programów w moim języku wraz z punktami, które 
realizują. Na końcu znajduje się wypełniona tabelka cech.

Język czerpie ze składni Pythona. Skorzystałem z opcji 
"layout" generator BNFC, która pozwala zamiast nawiasów klamrowych 
oraz średników wykorzystać wcięcie jako oznaczenie początku i końcu bloku
(ta funkcjonalnośc działa jedynie dla znaków "{", "}" i ";"). Oznacza to, że
moją składnię definiuję za pomocą "{", "}" i ";", ale parser umożliwia alternatywny
sposób oznaczania bloków przez wcięcia, a znaki klamrowe i średniki zostają dodane
w warstwie pośredniej. Zarezerwowany znak, po którym można napisać wcięcie zamiast 
nawiasu klamrowego, to ":".

Składnia zdefiniowana w gramatyce:
```go
code :
{
  if a >= 80 :
  {
    grade := 5;
    msg := "Nice!";
    print (msg);
  }
  else :
  {
    pass;
  };
  res := true;
  check := check + 1;
  print ("Grading done.");
}
```
Składnia alternatywna, za pomocą indentacji:
```go
code:
    if a >= 80:
        grade := 5
        msg := "Nice!"
        print(msg)
    else:
        pass
    res := true
    check := check + 1
    print("Grading done.")
```
Obydwie formy są przyjmowane przez parser.

Język, w przeciwieństwie do Pythona, będzie statycznie typowany. Dodatkowo,
aby zwiększyć czytelność pliku i ułatwić szukanie początku programu, każdy plik ma mieć ustaloną strukturę:

```
ProgramMain.     Program ::= [VarDecl] MainFunc [FuncDecl];
```

Gdzie `VarDecl` to deklaracje zmiennych globalnych, a `FuncDecl` to deklaracje funkcji. Każda funkcja i zmienna tutaj zadeklarowana jest widoczna wszędzie.

## Przykłady z punktami

Przy deklaracji nie piszemy typu zmiennej. Powinna wynikać ona z wartości,
na którą inicjalizujemy tą zmienną, dlatego też język wprowadza wymóg inicjalizacji zmiennych przy ich deklaracji, za pomocą składni `a := 5`.

### Punkty 1,2,3

```go
counter := 0 // Deklaracja zmiennej globalnej.
arch := "x86_64-pc-linux-gnu" // Musi być inicjalizacja.
debug := false // Typ wydedukowany z wartości inicjowanej.

code:
    coutner <- counter + 1 // Operacja przypisania.
    print_logs := debug // Czy inferencja typów jest prosta do zaimplementowania?
```
Realizuje:
- 1. Co najmniej trzy typy wartości
- 2. Literały, arytmetyka, porównania.
- 3. Zmienne, operacja przypisania

### Punkty 4, 5

```go
code:
    if age > 18:
        print("Pass")
    else:
        i := 15
        while i > 0:
            i <-i - 1
            print("...")
```
Realizuje:
- 4. Jawne wypisywanie wartości na wyjście
- 5. while, if (z else)


### Punkty 6,7

```go
fibonacci := (n | int) -> int:
    // Argumenty mają forme [ident typ].
    if n == 0:
        return 0
    if n == 1:
        return 1
    return fibonacci(n - 1) + fibonacci(n - 2)

parse_file := (name | string, errno  | * int) -> void:
    // Przekazywanie przez zmienną "Ident | * Typ", a przez wartość "Ident | Typ"
    if name == "do_not_parse":
        errno <- 1
        return
    errno <- 0
```

Realizuje:
- 6. Funkcje lub procedury (bez zagnieżdżania), rekurencja.
- 7. co najmniej dwa sposoby przekazywania parametrów (przez zmienną, przez wartość)

### Punkty 9, 10, 11

```go
x := 20

code:
    modify_local_x() // Wypisze 101.
    print(x) // Wypisze 20.
    modify_global_x() // Wypisze 45.

modify_global_x := () -> void:
    x <- x + 25
    print(x)

modify_local_x := () -> void:
    x := 1
    x <- x + 100
    print(x)

return_date := () -> string:
    return "Today"
```

Realizuje:
- 9. Przesłanianie identyfikatorów ze statycznym ich wiązaniem
- 10. Obsługa błędów wykonania, np. dzielenie przez zero - tutaj nie ma przykładu

### Punkt 12

```go
debug := True
cpu_cores := 12
multi_core := cpu_cores && !debug // Parse error, can and "and" on int and bool.
estimated_time := 1000 / (cpu_cores * 2)
```

Realizuje:
- 12. Statyczne typowanie (tj. zawsze terminująca faza kontroli typów przed rozpoczęciem wykonania programu) - 4 pkt

### Punkt 13

```go
code:
    make_adder := (value | int) -> (int) -> int:
        adder := (x | int) -> int:
            return value + x
        
        return adder

    add_five := make_adder(5)
    add_ten := make_adder(10)
    print(add_five(100))
    f := chain(add_five, add_ten)
    print(f(100)) // Prints 115. 

chain := (f | (int) -> int, g | (int) -> int) -> (int) -> int:
    fg := (x | int) -> int:
        return f(g(x))
    return fg
```

Realizuje:
- 13. Dowolnie zagnieżdżone definicje funkcji / procedur z zachowaniem poprawności statycznego wiązania identyfikatorów - 2 pkt

### Punkt 17

```go
code:
    first := chain((x | int) -> (x * 2 | int),
                   (x | int) -> (x + 5 | int))

    second := chain((x | int) -> (x * 2 | int),
        (x | int) -> ({
            if x > 10: {return 0}
            else: {return 1}
        } | int))

    print(first(10)) // 30


chain := (f | (int) -> int, g | (int) -> int) -> (int) -> int:
    fg := (x | int) -> int:
        return f(g(x))
    return fg
```

- 17. Funkcje jako parametry, zwracanie funkcji w wyniku, domknięcia à la JavaScript, funkcje anonimowe - 4 pkt

Czy do pełnej funkcjonalności funkcji anonimowych musimy dać im możliwość
ifów, argumenty przez zmienną itd.? Czy wystarczy funkcja licząca `Expr`, jak pierwszy przykład w kodzie?

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