declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main() {
    %alicjaMakota = alloca i32
    store i32 5, i32* %alicjaMakota
    %1 = load i32, i32* %alicjaMakota
    %2 = add i32 %1, 1
    store i32 %2, i32* %alicjaMakota
    ret i32 0
}