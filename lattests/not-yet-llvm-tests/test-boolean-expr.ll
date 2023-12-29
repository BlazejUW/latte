declare i32 @printInt(i32)
declare i32 @printString(i8*)
define i32 @main() {
    %a = alloca i32
    store i32 1, i32* %a
    %1 = load i32, i32* %a
    %2 = icmp eq i32 %1, 1
    br i1 %2, label %if_true_1, label %if_end_1

if_true_1:
    %2 = load i32, i32* %a
    %3 = icmp sgt i32 %2, 3
    %4 = load i32, i32* %a
    %5 = icmp slt i32 %4, 1
    %6 = or i1 %%3, %5
    br i1 %6, label %if_true_2, label %if_end_1

if_true_2:
    %7 = load i32, i32* %a
    %%8 = icmp ne i32 %7, 3
    br i1 %%8, label %if_true_3, label %if_end_1

if_true_3:
    %9 = load i32, i32* %a
    %10 = icmp eq i32 %9, 0
    %11 = load i32, i32* %a
    %12 = icmp eq i32 %11, 1
    %13 = and i1 %10, %12
    br i1 %13, label %if_true_4, label %if_false_4

if_true_4:
    store i32 1, i32* %a
    br label %if_end_1

if_false_4:
    store i32 2, i32* %a
    %14 = load i32, i32* %a
    call void @printInt(i32 %14)
    br label %if_end_1

if_end_1:
    ret i32 0
}
