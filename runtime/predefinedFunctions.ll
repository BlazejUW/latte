declare i32 @printf(i8*, ...)

@strInt = constant [3 x i8] c"%d\0A", align 1

define void @printInt(i32 %x) {
  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @strInt, i64 0, i64 0), i32 %x)
  ret void
}

define void @printString(i8* %str) {
    call i32 (i8*, ...) @printf(i8* %str)
    ret void
}