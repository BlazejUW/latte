declare i32 @printf(i8*, ...)

define void @printString(i8* %str) {
    call i32 (i8*, ...) @printf(i8* %str)
    ret void
}