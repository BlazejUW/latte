declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main()
{
%1 = alloca i32
store i32 2, i32* %1
%2 = alloca i32
store i32 7, i32* %2
%3 = alloca i32
store i32 0, i32* %3
%4 = alloca i32
store i32 1, i32* %4
br label %while_cond_1
while_cond_1:
%5 = load i32, i32* %3
%6 = load i32, i32* %2
%7 = icmp slt i32 %5, %6
br i1 %7, label %while_body_1, label %while_end_1
while_body_1:
%8 = load i32, i32* %4
%9 = load i32, i32* %1
%10 = mul i32 %8, %9
store i32 %10, i32* %4
%11 = load i32, i32* %3
%12 = add i32 %11, 1
store i32 %12, i32* %3
br label %while_cond_1
while_end_1:
%13 = load i32, i32* %4
call void @printInt(i32 %13)

ret i32 0
}