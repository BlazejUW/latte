declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main()
{
%a = alloca i1
store i1 1, i1* %a
%1 = load i1, i1* %a
%2 = xor i1 %1, 1
br i1 %2, label %if_true_1, label %if_false_1
if_true_1:
call void @printInt(i32 0)

br label %if_end_1
if_false_1:
call void @printInt(i32 1)

br label %if_end_1
if_end_1:
ret i32 0
}