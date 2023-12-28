declare i32 @printString(i8* %str)
declare i32 @printInt(i32 %i)
define i32 @main() {
    %a = alloca i32
    store i32 1, i32* %a
    %b = alloca i32
    store i32 2, i32* %b
    %c = alloca i32
    %1 = load i32, i32* %a
    %2 = load i32, i32* %b
    %3 = add i32 %1, %2
    store i32 %3, i32* %c
    %d = alloca i32
    %4 = load i32, i32* %c
    %5 = add i32 %4, 1
    store i32 %5, i32* %d
    %6 = load i32, i32* %d
    call void @printInt(i32 %6)
    ret i32 0
}

