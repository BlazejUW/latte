@str0 = private unnamed_addr constant [8 x i8] c"a+b==3\0A\00"
declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main()
{
    %1 = alloca i32
    store i32 1, i32* %1
    %2 = alloca i32
    store i32 2, i32* %2
    %3 = alloca i32
    store i32 3, i32* %3
    %4 = load i32, i32* %1
    %5 = load i32, i32* %2
    %6 = add i32 %4, %5
    %7 = load i32, i32* %3
    %8 = icmp eq i32 %6, %7
    %9 = xor i1 %8, 1
    br i1 %9, label %if_true_1, label %if_false_1
if_true_1:
    ret i32 1
if_false_1:
    %10 = getelementptr inbounds [8 x i8], [8 x i8]* @str0, i32 0, i32 0
    call void @printString(i8* %10)
    br label %if_end_1
if_end_1:
    ret i32 0

}