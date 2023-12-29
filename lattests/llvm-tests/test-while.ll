declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main()
{
%a = alloca i32
store i32 2, i32* %a
%b = alloca i32
store i32 7, i32* %b
%i = alloca i32
store i32 0, i32* %i
%result = alloca i32
store i32 1, i32* %result
br label %while_cond_1
while_cond_1:
%1 = load i32, i32* %i
%2 = load i32, i32* %b
%3 = icmp slt i32 %1, %2
br i1 %3, label %while_body_1, label %while_end_1
while_body_1:
%4 = load i32, i32* %result
%5 = load i32, i32* %a
%6 = mul i32 %4, %5
store i32 %6, i32* %result
%7 = load i32, i32* %i
%8 = add i32 %7, 1
store i32 %8, i32* %i
br label %while_cond_1
while_end_1:
%9 = load i32, i32* %result
call void @printInt(i32 %9)

ret i32 0
}