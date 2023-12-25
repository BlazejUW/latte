define i32 @main() {
    %a0 = alloca i1
    store i1 1, i1* %a0
    %b0 = alloca i1
    ret i32 0
}
