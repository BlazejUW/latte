declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @foo(i32 %a, i1 %b) {
    %1 = add i32 %a, 1
    ret i32 0
}

define i32 @main() {
    %1 = call i32 @foo(i32 1, i1 1)
    ret i32 %1
}
