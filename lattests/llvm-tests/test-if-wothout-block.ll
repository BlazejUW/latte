declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main() {
    %a = alloca i32
    store i32 1, i32* %a
    %1 = load i32, i32* %a
    %2 = icmp slt i32 %1, 1
    br i1 %2, label %if_true_1, label %if_false_1
if_true_1:
    ret i32 1

if_false_1:
    store i32 3, i32* %a
    br label %if_end_1

if_end_1:
    ret i32 0
}