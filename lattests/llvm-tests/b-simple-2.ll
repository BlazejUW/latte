@helloWorldStr = constant [14 x i8] c"Hello World!\0A\00"
declare i32 @printString(i8* %str)

define i32 @main() {
    %str = getelementptr [14 x i8], [14 x i8]* @helloWorldStr, i64 0, i64 0
    call void @printString(i8* %str)
    ret i32 0
}
