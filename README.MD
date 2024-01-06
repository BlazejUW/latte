# Kompilator języka Latte do LLVM

Kompilator języka Latte napisany w Haskellu. Kompilator generuje kod LLVM, który można podlinkować z kodem z katalogu runtime. Kompilator obsługuje wszystkie podtawowe funkcjonalności języka Latte. Dodatkowo, zastosowałem następujące optymalizacje:
* eliminacja nieosiągalnego kodu
* kod jest w postaci SSA
* nie używa się funkcji alloca, wszystkie zmienne trzymane są w rejestrach i wykorzystuje się także funkcje phi
* LCSE

W kolejnym terminie planuję oddać jeszcze następujące optymalizacje i rozszerzenia:
* function inlining
* tablice
* struktury
* obiekty
* odśmiecanie
## Szczegóły kompilacji
1. Budowanie Drzewa Składniowego:
Kompilacja rozpoczyna się od budowania drzewa składniowego za pomocą BNFC (Bison Normal Form Converter), który wczytuje zdefiniowaną gramatykę języka Latte. Narzędzie to generuje lekser i parser, które służą do analizy i przekształcenia kodu źródłowego w strukturę drzewa, ułatwiającą dalszą obróbkę.
2. Parsowanie:
Kolejnym etapem jest parsowanie, gdzie wczytany kod źródłowy jest przekształcany przez lekser i parser w drzewo składniowe. Dzięki temu możliwe jest dokładne zrozumienie struktury i elementów kodu źródłowego. Umożliwia mi to w dalszej części kompilacji poruszanie się po drzewie i wykonywanie odpowiednich operacji.
3. Typechecking:
Po zbudowaniu drzewa składniowego następuje etap typecheckingu, realizowany przez moduł Typechecker, znajdujący się w głównym katalogu kodu projektu. Typechecker analizuje drzewo pod kątem poprawności typów, sprawdzając zgodność deklaracji i użycia zmiennych oraz funkcji. Sprawdza także obecność return w funkcjach. W tym etapie zbierane są informacje o napotkanych funkcjach oraz typach każdego wyrażenia. Informacje te są przechowywane w mapie exprTypes, umożliwiając łatwy dostęp do typów wyrażeń w trakcie kompilacji.
4. Kompilacja:
Właściwa kompilacja znajduje się w module Compiler. Polega na przetworzeniu drzewa składniowego w kod pośredni LLVM. Proces ten odbywa się poprzez rekursywne przechodzenie po drzewie składniowym i generowanie odpowiednich instrukcji LLVM. Znaczącą cechą implementacji jest brak użycia instrukcji alloca dla alokacji pamięci, co jest realizowane przez techniki znane z kompilacji SSA (Static Single Assignment). Zaimplemenotwane jest również zastępowanie napotkanego już kodu, co jest realizowane przez technikę LCSE (Local Common Subexpression Elimination). W trakcie kompilacji wykorzystywane są informacje o typach wyrażeń, które zostały zebrane w etapie typecheckingu. Przy napotkaniu nowego wyrażenia sprawdzam, czy było już ono wcześniej obliczane. Umożliwia mi to zaimplementowany mechanizm porównywania wyrażeń. Wyrażenia są porównywane na podstawie ich typu oraz wartości. Jeśli wyrażenie zostało już wcześniej obliczone, to zamiast jego obliczania, wstawiam do kodu LLVM instrukcję przypisania do zmiennej, która przechowuje wynik obliczenia tego wyrażenia. Dzięki temu unikam wielokrotnego obliczania tego samego wyrażenia, co przyspiesza działanie programu. Nie dodałem jednak zastępowania wyrażeń, które są wywołaniami funkcji, ponieważ w tym przypadku nie jestem w stanie sprawdzić, czy funkcja jest czysta, czy nie. 
5. Obsługa Pętli while:
W przypadku pętli while, stosowana jest specyficzna technika polegająca na dwukrotnym przejściu przez pętlę. Pierwsze przejście służy do identyfikacji tych zmiennych, które w bloku zmieniają swoją wartość, następnie umieszczam blok phi na początku pętli. Po pierwszym przejściu przywracam stan sprzed "sztucznego" przejścia. Drugi obrót realizuje właściwą kompilację pętli. Docelowo zamierzam wykorzystać tę technikę do implementacji optymalizacji pętli, czyli dodanie zmiennych indukcyjnych i redukcji mocy.
6. Struktura kompilatora:
Cała logika kompilacji jest umieszczona w modułach Compiler, Typechecker, i RunCompile, a struktura programu jest zorganizowana w sposób umożliwiający łatwe śledzenie procesu kompilacji od parsowania po generację kodu pośredniego LLVM.
## Środowisko
* Haskell: Program został napisany w języku Haskell. Do jego kompilacji i uruchomienia potrzebne jest środowisko Haskell, w tym kompilator GHC (Glasgow Haskell Compiler).
## Używane Narzędzia i Biblioteki
Projekt kompilatora Latte do LLVM korzysta z następujących narzędzi i bibliotek:

### Narzędzia:

* BNFC (Bison Normal Form Converter): Wykorzystywane do generacji leksera i parsera na podstawie zdefiniowanej gramatyki języka Latte. BNFC jest kluczowe w procesie budowania drzewa składniowego i umożliwia łatwe przetwarzanie kodu źródłowego.
* GHC (Glasgow Haskell Compiler): Kompilator języka Haskell, używany do kompilacji kodu źródłowego kompilatora. GHC zapewnia wsparcie dla nowoczesnych funkcjonalności Haskell, takich jak monady, typy danych, i więcej.
* Happy: Generator parserów dla Haskell, używany do przetwarzania plików .y. Happy jest stosowany do generowania parsera na podstawie zdefiniowanej składni języka Latte.
* Alex: Generator lekserów dla Haskell, używany do przetwarzania plików .x. Alex jest wykorzystywany do generowania leksera, który jest pierwszym etapem w analizie kodu źródłowego.
### Biblioteki Haskell:

* Control.Monad: Zawiera funkcje monadyczne, które ułatwiają obsługę stanu i błędów w aplikacji. Jest kluczowe dla zarządzania stanem kompilatora i przepływu danych.
* Data.Map: Używana do tworzenia i zarządzania mapami (słownikami), które przechowują informacje o zmiennych, typach i funkcjach. Jest niezbędna dla efektywnego przechowywania i dostępu do informacji w trakcie typecheckingu i kompilacji.
* Latte.Abs, Latte.Lex, Latte.Par, Latte.Print: Moduły wygenerowane przez BNFC, które są bezpośrednio zaangażowane w proces parsowania i budowania drzewa składniowego.
* System.Environment, System.Exit, Control.Monad: Biblioteki używane do interakcji z systemem operacyjnym i zarządzania procesem kompilacji, w tym obsługi argumentów linii poleceń i zakończenia programu.
* Prelude: Podstawowa biblioteka Haskell, zawierająca definicje często używanych funkcji i operatorów.
### Struktura Projektu:
* Typechecker.hs: Moduł odpowiedzialny za sprawdzanie typów w drzewie składniowym. Zbiera informacje o typach wyrażeń i funkcji, które są później wykorzystywane w procesie kompilacji.
* Compiler.hs: Główny moduł kompilatora, który przetwarza drzewo składniowe na kod pośredni LLVM. Zawiera logikę kompilacji, w tym obsługę pętli, konstrukcji warunkowych i innych elementów języka.
* RunCompile.hs: Punkt wejścia aplikacji, który zarządza procesem parsowania, typecheckingu i kompilacji. Obsługuje argumenty linii poleceń i wyświetla wyniki działania kompilatora.


<!-- make
```
w katalogu głównym. To wyprodukuje plik wykonywalny o nazwie `RunCompile` w katalogu Latte. Testy odpala się skryptem:
```
./run_tests.sh
```

Żeby podlinkować do pliku llvm funkcje z katalogu runtime należy użyć polecenia:
```
llvm-link plik-z-kodem.ll ./runtime/runtime.ll -S -o nazwa-dla-podlinkowanego-programu.ll
```
Jeśli chcemy użyć interpretera LLVM do uruchomienia programu należy użyć polecenia:
```
lli nazwa-dla-podlinkowanego-programu.ll
```

Jeśli nie chcemy tworzyć osobnego pliku z podlinkowanym kodem, ale chcemy podlinkować "w locie" to możemy użyć polecenia:
```
llvm-link plik-z-kodem.ll ./runtime/runtime.ll -S | lli
``` -->