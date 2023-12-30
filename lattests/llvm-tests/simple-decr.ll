declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main() {
    %1 = alloca i32
    store i32 1, i32* %1
    %2 = load i32, i32* %1
    %3 = sub i32 %2, 1
    store i32 %3, i32* %1
    ret i32 0
}