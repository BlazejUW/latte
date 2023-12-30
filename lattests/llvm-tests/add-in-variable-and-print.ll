declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main() {
    %1 = alloca i32
    store i32 1, i32* %1
    %2 = alloca i32
    store i32 2, i32* %2
    %3 = alloca i32
    %4 = load i32, i32* %1
    %5 = load i32, i32* %2
    %6 = add i32 %4, %5
    store i32 %6, i32* %3
    %7 = alloca i32
    %8 = load i32, i32* %3
    %9 = add i32 %8, 1
    store i32 %9, i32* %7
    %10 = load i32, i32* %7
    call void @printInt(i32 %10)
    ret i32 0
}

