declare i32 @printString(i8* %str)
declare i32 @printInt(i32 %i)
define i32 @main() {
    %a = alloca i32
    store i32 1, i32* %a
    %b = alloca i32
    %c = alloca i32
    store i32 3, i32* %c
    %d = alloca i32
    store i32 4, i32* %d
    ret i32 0
}
