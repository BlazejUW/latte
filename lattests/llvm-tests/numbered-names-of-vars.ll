declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main() {
    %a = alloca i32
    store i32 1, i32* %a
    %a1 = alloca i32
    store i32 2, i32* %a1
    %1 = load i32, i32* %a
    store i32 %1, i32* %a1
    %2 = load i32, i32* %a1
    store i32 %2, i32* %a
    ret i32 0
}
