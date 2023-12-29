@strInt = constant [3 x i8] c"%d\0A", align 1
@loadStrInt = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@errorMsg = private unnamed_addr constant [15 x i8] c"runtime error\0A\00", align 1
; @buffer = common global [256 x i8] zeroinitializer, align 1
declare i32 @scanf(i8*, ...)
declare i32 @printf(i8*, ...)
declare void @exit(i32)
; declare i8* @fgets(i8*, i32, i8*)
; declare i8* @llvm.get.stdin() ; External function to get stdin

; define i8* @readString() {
;   %buffer_ptr = getelementptr inbounds [256 x i8], [256 x i8]* @buffer, i32 0, i32 0
;   %stdin = call i8* @llvm.get.stdin()
;   call i8* @fgets(i8* %buffer_ptr, i32 256, i8* %stdin)
;   ret i8* %buffer_ptr
; }

define void @printInt(i32 %x) {
  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @strInt, i64 0, i64 0), i32 %x)
  ret void
}

define void @printString(i8* %str) {
    call i32 (i8*, ...) @printf(i8* %str)
    ret void
}

define i32 @readInt() {
    %num = alloca i32, align 4
    %1 = call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @loadStrInt, i32 0, i32 0), i32* %num)
    %2 = load i32, i32* %num, align 4
    ret i32 %2
}

define void @error() {
    call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @errorMsg, i32 0, i32 0))
    call void @exit(i32 1)
    ret void
}