@str0 = private unnamed_addr constant [14 x i8] c"Hello World!\0A\00"

declare i32 @printString(i8* %str)
declare i32 @printInt(i32 %i)

define i32 @main() {
    %1 = getelementptr inbounds [14 x i8], [14 x i8]* @str0, i32 0, i32 0
    call void @printString(i8* %1)
    ret i32 0
}
