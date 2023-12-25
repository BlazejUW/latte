declare i32 @printInt(i32)
define i32 @main() {
    %a0 = alloca i32
    store i32 1, i32* %a0
    %1 = load i32, i32* %a0
    %a1 = add i32 %1, 1
    store i32 %a1, i32* %a0
    %2 = load i32, i32* %a0
    %3 = call i32 @printInt(i32 %2)
    ret i32 0
}