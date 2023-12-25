declare i32 @printInt(i32 %x)

define i32 @main() {
    %1 = alloca i32
    store i32 5, i32* %1
    %2 = load i32, i32* %1
    call void @printInt(i32 %2)
    ret i32 0
}