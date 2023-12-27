define i32 @main() {
    %a = alloca i1
    store i1 1, i1* %a
    %b = alloca i1
    ret i32 0
}
