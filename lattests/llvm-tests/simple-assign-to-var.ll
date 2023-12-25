define i32 @main() { 
    %a0 = alloca i32 
    store i32 1, i32* %a0 
    store i32 2, i32* %a0 
    %a1 = load i32, i32* %a0 
    ret i32 0 
    }