declare void @printString(i8* %str) 
declare void @printInt(i32 %i) 
declare void @error() 
declare i32 @readInt() 
declare i8* @readString() 
define i32 @main() { 
    %1 = alloca i32 store i32 1, 
    i32* %1 %2 = alloca i32 
    store i32 2, i32* %2 
    %3 = load i32, i32* %1 
    %4 = load i32, i32* %2 
    %5 = add i32 %3, %4 
    %6 = alloca i32 
    store i32 %5, i32* %6 
    %7 = load i32, i32* %6 
    %8 = add i32 %7, 1 
    %9 = alloca i32 
    store i32 %8, i32* %9 
    %10 = load i32, i32* %9 
    call void @printInt(i32 %10) 
    ret i32 0 
}