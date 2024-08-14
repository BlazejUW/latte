declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main() {
    %1 = alloca i1
    store i1 1, i1* %1
    %2 = alloca i1
    ret i32 0
}
