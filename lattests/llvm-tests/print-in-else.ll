declare i32 @printString(i8* %str)
declare i32 @printInt(i32 %i)

define i32 @main() {
    %i = alloca i32
    store i32 3, i32* %i
    br i1 0, label %if_true_1, label %if_false_1
if_true_1:    
    br label %if_end_1
if_false_1:
    %1 = load i32, i32* %i
    %2 = sub i32 %1, 1
    store i32 %2, i32* %i
    %3 = load i32, i32* %i
    call void @printInt(i32 %3)
    br label %if_end_1
if_end_1:
    ret i32 0
}
