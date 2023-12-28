declare i32 @printString(i8* %str)
declare i32 @printInt(i32 %i)
define i32 @main() {
    %k = alloca i32
    store i32 5, i32* %k
    %1 = load i32, i32* %k
    call void @printInt(i32 %1)
    ret i32 0
}