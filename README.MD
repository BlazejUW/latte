# Kompilator języka Latte do LLVM

Kompilator języka Latte napisany w Haskellu. Kompilator generuje kod LLVM, który można podlinkować z kodem z katalogu runtime. Kompilator obsługuje wszystkie podtawowe funkcjonalności języka Latte. Aktualna wersja kompilatora obsługuje następujące funkcjonalności:
1. Frontend: 4 pkt
2. Backend: 8 pkt
3. Użycie rejestrów i phi zamiast alloc w LLVM: 2 pkt
4. LCSE: 3 pkt
5. GCSE: 2 pkt
6. function inlining: 3 pkt (wszystkie funkcje poza main i rekurancyjnymi są inlinowane)
7. zmienne indukcyjne pętli i redukcja mocy: 2 pkt

Aktualna maksymalna punktacja: 24 pkt

W kolejnym terminie planuję oddać jeszcze następujące optymalizacje i rozszerzenia:
* tablice: 1 pkt
* struktury: 2 pkt
* odśmiecanie: 2 pkt


## Uruchamianie
Aby uruchomić kompilator należy:
* użyć polecenia `make` w katalogu głównym projektu. W wyniku jego działania powstaje plik wykonywalny latc
* użyć polecenia `./latc nazwa_pliku.lat` w celu skompilowania pliku z rozszerzeniem .lat. W wyniku na strumień stderr zostanie zwrócony komunikat o powodzeniu lub niepowodzeniu kompilacji. W wyniku powodzenia na standardowe wyjście zostanie wypisany kod LLVM.
* użyć polecenia `./latc_llvm nazwa_pliku.lat` w celu skompilowania pliku z rozszerzeniem .lat. W wyniku na strumień stderr zostanie zwrócony komunikat o powodzeniu lub niepowodzeniu kompilacji. W wyniku działania tego polecenia powstaje plik LLVM o nazwie program.ll, a także plik program.bc, będący podlinkowanym plikiem LLVM z runtime.bc. Plik program.bc można wykorzystywać za pomocą lli
* dodatkowo, dodałem flagę `-no_inline`, która wyłącza inlining funkcji. Dzięki temu można sprawdzić, czy program działa poprawnie bez inliningu. W tym celu należy użyć polecenia `./latc -no_inline nazwa_pliku.lat`, dzięki czemu na standardowe wyjście zostanie wypisany kod LLVM bez inliningu funkcji. 
To polecenie sprawia, że kod LLVM jest podlinkowany z runtime.bc i uruchamiany za pomocą lli.
Można też użyć polecenia: `./latc_llvm -no_inline nazwa_pliku.lat` w wyniku którego powstanie plik LLVM o nazwie program.ll, a także plik program.bc, będący podlinkowanym plikiem LLVM z runtime.bc.
Dodatkowo, można przetestować kod no_inline bezpśrednio za pomocą komendy:
```
./latc -no_inline nazwa_pliku.lat | llvm-link -S ./lib/runtime.bc - | lli
```
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
Właściwa kompilacja znajduje się w module Compiler. Polega na przetworzeniu drzewa składniowego w kod pośredni LLVM. Proces ten odbywa się poprzez rekursywne przechodzenie po drzewie składniowym i generowanie odpowiednich instrukcji LLVM. Znaczącą cechą implementacji jest brak użycia instrukcji alloca dla alokacji pamięci, co jest realizowane przez techniki znane z kompilacji SSA (Static Single Assignment). W trakcie kompilacji wykorzystywane są informacje o typach wyrażeń, które zostały zebrane w etapie typecheckingu. Przy napotkaniu nowego wyrażenia sprawdzam, czy było już ono wcześniej obliczane. **Poniżej umieszczam szczegóły implementacyjne i algorytmiczne zrobionych przeze mnie optymalizacji**.
* Obsługa Pętli while:
W przypadku pętli while, stosowana jest technika polegająca na dwukrotnym przejściu przez pętlę. Pierwsze przejście służy do identyfikacji tych zmiennych, które w bloku zmieniają swoją wartość, następnie umieszczam blok phi na początku pętli. Po pierwszym przejściu przywracam stan sprzed "sztucznego" przejścia. Drugi obrót realizuje właściwą kompilację pętli. Wcześniej analizuje także występujące w ciele funkcji wyrażenia i podwyrażenia, które kwalifikują się do redukcji mocy oraz zmienne indukcyjne.
5. Struktura kompilatora:
Cała napisane przeze mnie logika kompilacji jest umieszczona w modułach Compiler, Typechecker, i RunCompile, a struktura programu jest zorganizowana w sposób umożliwiający łatwe śledzenie procesu kompilacji od parsowania po generację kodu pośredniego LLVM. W pliku Helpers.hs trzymam niemonadyczne funkcje używane w modułach kompilatora.


## Opis realizacji optymalizacji
***W następnym podrozdziale znajduje się opis przykładów dołączonych przeze mnie do projektu, wizualizujących działanie optymalizacji.***
### LCSE
W trakcie kompilacji wykorzystywane są informacje o typach wyrażeń, które zostały zebrane w etapie typecheckingu. Przy napotkaniu nowego wyrażenia sprawdzam, czy było już ono wcześniej obliczane. Umożliwia mi to zaimplementowany mechanizm porównywania wyrażeń. Jeśli wyrażenie zostało już wcześniej obliczone, to zamiast jego obliczania, wstawiam do kodu LLVM instrukcję przypisania do zmiennej, która przechowuje wynik obliczenia tego wyrażenia. Dzięki temu unikam wielokrotnego obliczania tego samego wyrażenia, co przyspiesza działanie programu. Nie dodałem jednak zastępowania wyrażeń, które są wywołaniami funkcji, ponieważ w tym przypadku nie jestem w stanie sprawdzić, czy funkcja jest czysta, czy nie. Po zobaczeniu instrukcji przypisania do zmiennej usuwam ze zbioru wyrażeń wszystkie te, które były powiązane z tą zmienną, co zapewnia mi, że nie zastąpię wyrażenia innym, które jest już nieaktualne. 
W aktualnej wersji kompilatora ramka LCSE jest czyszczona po wyjściu z bloku. 

### GCSE
Działa tak samo jak LCSE, z tą różnicą, że jeśli podwyrażenie jest dostępne w każdej możliwej ścieżce (a dokładniej: jest obliczane zarówno w gałęzi if jak i else przypadku CondElse) to również jest ono zapamiętane i reużywane. W przypadku, gdy podwyrażenie jest dostępne tylko w jednej gałęzi if lub else, to nie jest ono zapamiętane.

### Function inlining
Na etapie typecheckingu zbieram informacje o tym, jakie funkcje są wywoływane w ciele każdej z funkcji. Tworzę skierowany graf wywołań, w którym węzłami są funkcje a krawędziami wywołania. Następnie szukam w tym grafie cykli. Początkowo zakładam, że wszystkie funkcje są inlinowalne. Gdy znajdę cykl usuwam ze zbioru funkcji inlinowanych wszyskite funkcje z tego cyklu. Pozwala mi to zapobiec inlinowaniu funkcji rekurencyjnych i wzajemnie rekurencyjnych. Przekazuję do kompilatora informacje o tym, które funkcje są inlinowalne i gdy w trakcie kompilacji napotkam wywołanie funkcji, to sprawdzam, czy jest ona inlinowalna. Jeśli tak, to wstawiam do kodu LLVM kod funkcji, którą wywołuje, a jeśli nie, to wstawiam instrukcję wywołania funkcji. Kod funkcji inlinowalnej różni się tym, że nie zwraca nic przez return. Po napotkaniu takiej instrukcji (trochę inny mechanizm w przypadku if) wstawiam instrukcję skoku do końca funkcji inlinowanej. **Poprzed generowanie odpowiednich komentarzy zadbałem o to, żeby były widoczne: początek funkcji inlinowanej, których rejestrów używa jako argumentów i końca funkcji inlinowanej.**
### Zmienne indukcyjne pętli i redukcja mocy
- **⁠Zmienne indukcyjne pętli**:
W celu zidentyfikowania zmiennych indukcyjnych przechodzę przez ciało pętli w poszukiwaniu statemeantów, które są postaci ``i = i + C`` lub ``i++`` lub ``i--``, gdzie i to identyfikator zmiennej, a C jakaś stała. Zgodnie z definicją z zajęć, przypisanie na zmienną indukcyjną może odbywać się dokładnie raz. W myśl tej zasady, jeśli napotkam drugie przypisanie do zmiennej indukcyjnej, to usuwam ją ze zbioru zmiennych indukcyjnych. Usuwam ze zbioru także każdą zmienną indukcyjną, która wystąpiła w bloku if lub ifelse, bo nie mam pewności, czy pojawi się dokładnie raz i będzie w ten sam sposób inkrementowana. ⁠
- **Redukcja mocy**:
W moim kompilatorze redukuję każde wyrażenie i podwyrażenie, które kwalifikuje się do redukcji, czyli takie, które jest postaci ``A*i``, gdzie A to stała, a i to zmienna indukcyjna. Gdy posiadam już informację o zmiennych indukcyjnych danej pętli, rozpoczynam poszukiwania wyrażeń i podwyrażeń do redukcji. W tym celu rekurencyjnie rozważam każde wyrażenie używane w pętli (poza przypadkiem zagnieżdżonej pętli while, która to posiada swój własny stos redukcji). Jeśli napotkam wyrażenie, które kwalifikuje się do redukcji, to zapisuje to wyrażenie do przetworzenia, przypisuję mu nową zmienną redukcyjną i zastępuje to wyrażenie wyrażeniem EVar z tą zmienną. Po przejściu całego ciała pętli tworzę blok deklaracji zmiennych redukcyjnych, który umieszczam przed pętlą while. Zmienne redukcyjne są zainicjalizowane na wyrażenie, które zastąpiły. Na przykład, jeśli znaleźliśmy wyrażenie `i*4`, to tworzymy nową zmienną redukcyjną, powiedzmy k, i przed ciałem pętli umieścimy:
``
int k = i*4;
``
Samo mnożenie zredukowane dodawaniem zapisujemy zaraz po inkrementacji zmiennej indukcyjnej. Jeśli zmienna indukcyjna wyglądała i = i + A, a zmienna redukcyjna ``k = i*B``, to po napotkaniu instrukcji ``i = i+ A`` umieszczamy przypisanie: ``k = k + A*B``, gdzie A*B to stała, którą wymnażamy na etapie kompilacji.

## Opis programów wizualizujących działanie optymalizacji
**Wszyskie programy wizualizujące działanie optymalizacji są umieszczone w katalogu `optymalizacje_przyklady/`**
### LCSE: `lcse.lat`
W przykładzie demonstuję wykorzystywanie już wcześniej obliczonych podwyrażeń. Program rozpoczyna się od deklaracji i inicjalizacji zmiennych, po czym przechodzi do serii obliczeń, w których pojawiają się powtarzające się podwyrażenia. Centralnym punktem jest identyfikacja i optymalizacja tych powtarzających się fragmentów kodu. W kodzie umieściłem kilka podwyrażeń, które są dobrymi kandydatami do LCSE. Są to `a + b` i `a - b`, używane w różnych miejscach programu. Kompilator wykrywa te powtarzające się podwyrażenia i oblicza je tylko raz na początku bloku kodu, a następnie wielokrotnie wykorzystuje wyniki tych obliczeń. Natomiast po zmianie wartości zmiennej `a` kompilator powtarza proces obliczania podwyrażeń i nie używa starych wyników, ponieważ są one nieaktualne. Nowe podwyrażenia są ponownie zapamiętywane i używane.
### GCSE: `gcse.lat`
Podobnie jak w LCSE, GCSE polega na wykorzystywaniu już wcześniej obliczonych podwyrażeń, lecz różni się tym, że dotyczy to całej funkcji, a nie tylko pojedynczego bloku kodu. Obrazuje to pokazując reużycie podwyrażenia policzonego w obu gałęziach ifelse. Warto zauważyć, że chodzi o podwyrażenia, a nie przypisania. Jak widać w przykładzie, podwyrażenia `a-b+1` jest przypisywane po pętli na na rejestr `%phi_value_1` i następne użycia tego podwyrażenia są zastępowane przez ten rejestr. Warto zauważyć, że podwyrażenie `b+c` nie jest reużywane, bo nie było wyliczone w obu gałęziach ifelsea.
### Function inlining: `inline_functions.lat`
W tym przykładzie demonstruję inlinowanie funkcji.
Program składa się z 3 funkcji możliwych do inlinowania (`do_nothing_and_sum`, `square` i `root`) i 3 niemożliwych (`factorial`, `even`, `odd`).
 Warto zauważyć, że realizuję inlining funkcji także zagnieżdżonych, wywoływanych w innych funkcjach. Takim przykładem jest wywołanie funkcji `do_nothing_and_sum`, podczas którego inlinuje także w main dwie pozostałe funkcje. Funkcja `factorial` jest rekurencyjna, więc nie używam na niej inliningu. Wartym zauważenia jest to, że wykrywam rekurencję wzajemną, więc nie inlinuje również funkcji `even`, natomiast już w definicji funkcji `odd` inlinuje funkcję `square`, (tylko po to, że w funkcjach rekurencyjnych mogę inlinować funkcje nierekurencyjne). 
### Zmienne indukcyjne pętli i redukcja mocy: `redukcja_mocy_petli.lat`
W przykładzie demonstruję **wykrywanie różnych zmiennych indukcyjnych** oraz **redukcje mocy w pętli na różnych rodzajach wyrażeń i podwyrażeń**. Przed pętlą deklaruje wszystkie używane zmienne. W pętli while używam dwóch zmiennych indukcyjnych, jedna jest inkrementowana za pomocą `++`, a druga zmienia się o ujemną liczbę co obrót. Prezentuje także redukcje mocy dla każdego podwyrażenia spośród pokazanych możliwego do zredukowania: w przypisaniu, w wywołaniu funkcji, w zwykłym wyrażeniu. W ogólności redukcja działa w każdym przypadku zgodnie z algorytmem pokazanym na wykładzie. 


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