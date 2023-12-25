define i32 @main() {
    %a0 = alloca i32
    store i32 1, i32* %a0
    %b0 = alloca i32
    store i32 2, i32* %b0
    
    %a1 = load i32, i32* %a0
    %b1 = load i32, i32* %b0

    %sum = add i32 %a1, %b1
    %c0 = alloca i32
    store i32 %sum, i32* %c0

    ret i32 0
}
