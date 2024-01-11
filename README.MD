# Kompilator języka Latte do LLVM

Kompilator języka Latte napisany w Haskellu. Kompilator generuje kod LLVM, który można podlinkować z kodem z katalogu runtime. Kompilator obsługuje wszystkie podtawowe funkcjonalności języka Latte. Aktualna wersja kompilatora obsługuje następujące funkcjonalności:
1. Frontend: 3/4 pkt (?)
2. Backend: 8 pkt
3. Użycie rejestrów i phi zamiast alloc w LLVM: 2 pkt
4. LCSE: 3 pkt
 Aktualna punktacja: 16 pkt (?)

W kolejnym terminie planuję oddać jeszcze następujące optymalizacje i rozszerzenia:
* function inlining: 2 pkt
* zmienne indukcyjne pętli i redukcja mocy: 2 pkt
* GSCE: 2 pkt
* tablice: 1 pkt
* struktury: 2 pkt
* obiekty: 3 pkt
* odśmiecanie: 2 pkt
 Suma: 30 pkt
 Do uzyskania w drugim terminie: 15 pkt
 Cel: 22.3 pkt
 Brakuje do zaliczenia bez egzaminu: 6.3 pkt
Cel w drugim terminie: min 10 pkt

## Uruchamianie
Aby uruchomić kompilator należy:
* użyć polecenia `make` w katalogu głównym projektu. W wyniku jego działania powstaje plik wykonywalny latc
* użyć polecenia `./latc nazwa_pliku.lat` w celu skompilowania pliku z rozszerzeniem .lat. W wyniku na strumień stderr zostanie zwrócony komunikat o powodzeniu lub niepowodzeniu kompilacji. W wyniku powodzenia na standardowe wyjście zostanie wypisany kod LLVM.
* użyć polecenia `./latc_llvm nazwa_pliku.lat` w celu skompilowania pliku z rozszerzeniem .lat. W wyniku na strumień stderr zostanie zwrócony komunikat o powodzeniu lub niepowodzeniu kompilacji. W wyniku działania tego polecenia powstaje plik LLVM o nazwie program.ll, a także plik program.bc, będący podlinkowanym plikiem LLVM z runtime.bc. Plik program.bc można wykorzystywać za pomocą lli

### Struktura Projektu:
1. Katalog `src/Latte`: Główny katalog zawierający kod źródłowy kompilatora. W tym katalogu znajdują się wszystkie niezbędne moduły Haskell, takie jak moduł analizy składniowej (Abs.hs), lekser (Lex.hs), parser (Par.hs), moduł drukujący (Print.hs), moduł pomocniczy (Helpers.hs). Zamieszczam te pakiety jako źródłowe, gdyż na serwerze students używane przeze mnie w projekcie polecenie bnfc nie działa prawidłowo. Z tego względu dołączam pliki wygenreowane przez bnfc. W tym katalogu znajdują się także moduły Compiler.hs, Typechecker.hs i RunCompile.hs:
* `Typechecker.hs`: Moduł odpowiedzialny za sprawdzanie typów w drzewie składniowym. Zbiera informacje o typach wyrażeń i funkcji, które są później wykorzystywane w procesie kompilacji.
* `Compiler.hs`: Główny moduł kompilatora, który przetwarza drzewo składniowe na kod pośredni LLVM. Zawiera logikę kompilacji, w tym obsługę pętli, konstrukcji warunkowych i innych elementów języka.
* `RunCompile.hs`: Punkt wejścia aplikacji, który zarządza procesem parsowania, typecheckingu i kompilacji. Obsługuje argumenty linii poleceń i wyświetla wyniki działania kompilatora.
2. Katalog `lib`: Zawiera pliki pomocnicze runtime.bc, który zawiera predefiniowane funkcje w LLVM używane podczas kompilacji i runtime.c, który zawiera kod źródłowy tych funkcji.
3. Plik `latc_llvm`: Skrypt bashowy, który uruchamia kompilator plikiem latc i podlinkowuje wygenerowany kod LLVM z kodem z katalogu lib. W wyniku jego działania w katalogu w którym został użyty powstaje plik LLVM o nazwie program.ll, a także plik program.bc, będący podlinkowanym plikiem LLVM z runtime.bc. Plik program.bc można wykorzystywać za pomocą lli
4. Plik `src/Makefile`: Plik makefile, który kompiluje kompilator i tworzy plik wykonywalny RunCompile w katalogu src/Latte.
5. Plik `Makefile`: Plik makefile, który wywołuje make w katalogu src i tworzy plik wykonywalny latc w katalogu głównym.

6. * Plik `latc`: Plik wykonywalny, powstający po uruchomieniu komendy make uruchamiający kompilator. Nie jest dołączony do projektu, ale postanawiam umieścić go w README, żeby zaznaczyć, że plik ten jest linkiem do pliku RunCompile w katalogu src/Latte.

## Szczegóły kompilacji
1. Budowanie Drzewa Składniowego:
Kompilacja rozpoczynała się od budowania drzewa składniowego za pomocą BNFC (Bison Normal Form Converter), który wczytywał zdefiniowaną gramatykę języka Latte. Jak zaznaczyłem wcześniej, krok ten został wykonany u mnie lokalnie, przez co wygenerowane przez to narzędzie lekser i parser służą do analizy i przekształcenia kodu źródłowego w strukturę drzewa, ułatwiającą dalszą obróbkę.
2. Parsowanie:
Kolejnym etapem jest parsowanie, gdzie wczytany kod źródłowy jest przekształcany przez lekser i parser w drzewo składniowe. Umożliwia mi to w dalszej części kompilacji poruszanie się po drzewie i wykonywanie odpowiednich operacji.
3. Typechecking:
Po zbudowaniu drzewa składniowego następuje etap typecheckingu, realizowany przez moduł Typechecker. Typechecker analizuje drzewo pod kątem poprawności typów, sprawdzając zgodność deklaracji i użycia zmiennych oraz funkcji. Sprawdza także obecność return w funkcjach. W tym etapie zbierane są informacje o napotkanych funkcjach oraz typach każdego wyrażenia. Informacje te są przechowywane w mapie exprTypes, umożliwiając łatwy dostęp do typów wyrażeń w trakcie kompilacji.
4. Kompilacja:
Właściwa kompilacja znajduje się w module Compiler. Polega na przetworzeniu drzewa składniowego w kod pośredni LLVM. Proces ten odbywa się poprzez rekursywne przechodzenie po drzewie składniowym i generowanie odpowiednich instrukcji LLVM. Znaczącą cechą implementacji jest brak użycia instrukcji alloca dla alokacji pamięci, co jest realizowane przez techniki znane z kompilacji SSA (Static Single Assignment). Zaimplemenotwane jest również zastępowanie napotkanego już kodu, co jest realizowane przez technikę LCSE (Local Common Subexpression Elimination). W trakcie kompilacji wykorzystywane są informacje o typach wyrażeń, które zostały zebrane w etapie typecheckingu. Przy napotkaniu nowego wyrażenia sprawdzam, czy było już ono wcześniej obliczane. Umożliwia mi to zaimplementowany mechanizm porównywania wyrażeń. Jeśli wyrażenie zostało już wcześniej obliczone, to zamiast jego obliczania, wstawiam do kodu LLVM instrukcję przypisania do zmiennej, która przechowuje wynik obliczenia tego wyrażenia. Dzięki temu unikam wielokrotnego obliczania tego samego wyrażenia, co przyspiesza działanie programu. Nie dodałem jednak zastępowania wyrażeń, które są wywołaniami funkcji, ponieważ w tym przypadku nie jestem w stanie sprawdzić, czy funkcja jest czysta, czy nie. 
* Obsługa Pętli while:
W przypadku pętli while, stosowana jest specyficzna technika polegająca na dwukrotnym przejściu przez pętlę. Pierwsze przejście służy do identyfikacji tych zmiennych, które w bloku zmieniają swoją wartość, następnie umieszczam blok phi na początku pętli. Po pierwszym przejściu przywracam stan sprzed "sztucznego" przejścia. Drugi obrót realizuje właściwą kompilację pętli. Docelowo zamierzam wykorzystać tę technikę do implementacji optymalizacji pętli, czyli dodanie zmiennych indukcyjnych i redukcji mocy.
5. Struktura kompilatora:
Cała napisane przeze mnie logika kompilacji jest umieszczona w modułach Compiler, Typechecker, i RunCompile, a struktura programu jest zorganizowana w sposób umożliwiający łatwe śledzenie procesu kompilacji od parsowania po generację kodu pośredniego LLVM.
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