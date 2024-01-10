@str0 = private unnamed_addr constant [42 x i8] c"Expected a non-negative integer, but got:\00"
declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @fibonacci(i32 %n)
{
entry:
%0 = icmp sle i32 %n, 1
br i1 %0, label %if_true_1, label %if_end_1
if_true_1:
ret i32 %n
if_end_1:
%1 = add i32 0, 0
%2 = add i32 0, 1
%3 = add i32 0, 0
%4 = add i32 0, 2
br label %while_cond_2
while_cond_2:
%phi_value_1 = phi i32 [ %1, %if_end_1 ], [ %7, %while_body_2 ]
%phi_value_2 = phi i32 [ %2, %if_end_1 ], [ %8, %while_body_2 ]
%phi_value_3 = phi i32 [ %4, %if_end_1 ], [ %9, %while_body_2 ]
%phi_value_4 = phi i32 [ %3, %if_end_1 ], [ %6, %while_body_2 ]
%5 = icmp sle i32 %phi_value_3, %n
br i1 %5, label %while_body_2, label %while_end_2
while_body_2:
%6 = add i32 %phi_value_2, %phi_value_1
%7 = add i32 0, %phi_value_2
%8 = add i32 0, %6
%9 = add i32 %phi_value_3, 1
br label %while_cond_2
while_end_2:
ret i32 %phi_value_2
}
define i32 @main()
{
entry:
%0 = add i32 0, 23
%1 = add i32 0, 1
%2 = icmp sge i32 %0, 0
br i1 %2, label %if_true_3, label %if_false_3
if_true_3:
%3 = call i32 @fibonacci(i32 %0)
call void @printInt(i32 %3)
ret i32 0
if_false_3:
%4 = getelementptr inbounds [42 x i8], [42 x i8]* @str0, i32 0, i32 0
call void @printString(i8* %4)
call void @printInt(i32 %0)
ret i32 1
}
