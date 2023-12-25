define i32 @main() {
    %a0 = alloca i32
    store i32 1, i32* %a0
    %b0 = alloca i32
    %c0 = alloca i32
    store i32 3, i32* %c0
    %d0 = alloca i32
    store i32 4, i32* %d0
    ret i32 0
}
