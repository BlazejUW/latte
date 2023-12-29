declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main() {
    %a = alloca i1
    store i1 1, i1* %a
    %b = alloca i1
    ret i32 0
}
