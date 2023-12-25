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