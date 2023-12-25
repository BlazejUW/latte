Pierwszy krok, żeby uruchomić kompilator to użyć polecenia:
```
make
```
w katalogu głównym. To wyprodukuje plik wykonywalny o nazwie `RunCompile` w katalogu Latte. Testy odpala się skryptem:
```
./run_tests.sh
```

Żeby podlinkować do pliku llvm funkcję z katalogu runtime należy użyć polecenia:
```
llvm-link plik-z-kodem.ll /ścieżka/do/runtime/nazwa-funkcji.ll -S -o nazwa-dla-podlinkowanego-programu.ll
```
Jeśli chcemy użyć interpretera LLVM do uruchomienia programu należy użyć polecenia:
```
lli nazwa-dla-podlinkowanego-programu.ll
```

Jeśli nie chcemy tworzyć osobnego pliku z podlinkowanym kodem, ale chcemy podlinkować "w locie" to możemy użyć polecenia:
```
llvm-link plik-z-kodem.ll /ścieżka/do/runtime/nazwa-funckji.ll -S | lli
```