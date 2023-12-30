declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @main() {
    %1 = alloca i32
    store i32 3, i32* %1
    br i1 0, label %if_true_1, label %if_false_1
if_true_1:    
    br label %if_end_1
if_false_1:
    %2 = load i32, i32* %1
    %3 = sub i32 %2, 1
    store i32 %3, i32* %1
    %4 = load i32, i32* %1
    call void @printInt(i32 %4)
    br label %if_end_1
if_end_1:
    ret i32 0
}
