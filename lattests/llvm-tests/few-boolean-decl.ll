declare i32 @printString(i8* %str)
declare i32 @printInt(i32 %i)
define i32 @main() {
    %a = alloca i1
    store i1 1, i1* %a
    %b = alloca i1
    ret i32 0
}
