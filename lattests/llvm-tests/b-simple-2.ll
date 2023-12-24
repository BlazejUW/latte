@helloWorldStr = constant [14 x i8] c"Hello World!\0A\00"
; pomysł: moze trzymać osobną mapkę ze stringami i na początku programu wypisywać je jako const
declare i32 @printf(i8*, ...)

define void @printString(i8* %str) {
entry:
    call i32 (i8*, ...) @printf(i8* %str)
    ret void
}
define i32 @main() {
    %str = getelementptr [14 x i8], [14 x i8]* @helloWorldStr, i64 0, i64 0
    call void @printString(i8* %str)
    ret i32 0
}
