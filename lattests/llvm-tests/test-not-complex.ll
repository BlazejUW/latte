@str0 = private unnamed_addr constant [8 x i8] c"a+b==3\0A\00"
declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main()
{
    %a = alloca i32
    store i32 1, i32* %a
    %b = alloca i32
    store i32 2, i32* %b
    %c = alloca i32
    store i32 3, i32* %c
    %1 = load i32, i32* %a
    %2 = load i32, i32* %b
    %3 = add i32 %1, %2
    %4 = load i32, i32* %c
    %5 = icmp eq i32 %3, %4
    %6 = xor i1 %5, 1
    br i1 %6, label %if_true_1, label %if_false_1
if_true_1:
    ret i32 1
if_false_1:
    %7 = getelementptr inbounds [8 x i8], [8 x i8]* @str0, i32 0, i32 0
    call void @printString(i8* %7)
    br label %if_end_1
if_end_1:
    ret i32 0

}