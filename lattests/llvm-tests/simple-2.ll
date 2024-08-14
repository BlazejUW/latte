@str0 = private unnamed_addr constant [14 x i8] c"Hello World!\0A\00"

declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @main() {
    %1 = getelementptr inbounds [14 x i8], [14 x i8]* @str0, i32 0, i32 0
    call void @printString(i8* %1)
    ret i32 0
}
