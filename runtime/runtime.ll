@strInt = constant [4 x i8] c"%d\0A\00", align 1
@loadStrInt = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@errorMsg = private unnamed_addr constant [15 x i8] c"runtime error\0A\00", align 1
@newline = private unnamed_addr constant [2 x i8] c"\0A\00"
@__stdinp = external global ptr, align 8
declare i32 @scanf(i8*, ...)
declare i32 @printf(i8*, ...)
declare void @exit(i32)
declare void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i1)
declare ptr @malloc(i64 noundef)
declare ptr @fgets(ptr noundef, i32 noundef, ptr noundef)
declare void @free(ptr noundef) 
declare i64 @strlen(i8*)


define void @printInt(i32 %x) {
  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @strInt, i64 0, i64 0), i32 %x)
  ret void
}

define void @printString(i8* %str) {
    call i32 (i8*, ...) @printf(i8* %str)
    %newline = getelementptr inbounds [2 x i8], [2 x i8]* @newline, i32 0, i32 0
    call i32 (i8*, ...) @printf(i8* %newline)

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

define ptr @readString() #0 {
  %1 = alloca ptr, align 8
  %2 = alloca i64, align 8
  %3 = alloca ptr, align 8
  store i64 256, ptr %2, align 8
  %4 = load i64, ptr %2, align 8
  %5 = call ptr @malloc(i64 noundef %4) #3
  store ptr %5, ptr %3, align 8
  %6 = load ptr, ptr %3, align 8
  %7 = load i64, ptr %2, align 8
  %8 = trunc i64 %7 to i32
  %9 = load ptr, ptr @__stdinp, align 8
  %10 = call ptr @fgets(ptr noundef %6, i32 noundef %8, ptr noundef %9)
  %11 = icmp eq ptr %10, null
  br i1 %11, label %12, label %14

12:
  %13 = load ptr, ptr %3, align 8
  call void @free(ptr noundef %13)
  store ptr null, ptr %1, align 8
  br label %16

14:
  %15 = load ptr, ptr %3, align 8
  store ptr %15, ptr %1, align 8
  br label %16

16:
  %17 = load ptr, ptr %1, align 8
  ret ptr %17
}



define i8* @doNotUseThatNameConcat(i8* %str1, i8* %str2) {
    %str1_len = call i64 @strlen(i8* %str1)
    %str2_len = call i64 @strlen(i8* %str2)
    %total_len = add i64 %str1_len, %str2_len
    %total_len_inc_null = add i64 %total_len, 1
    %new_str = call i8* @malloc(i64 %total_len_inc_null)
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* %new_str, i8* %str1, i64 %str1_len, i1 false)
    %str2_dest = getelementptr i8, i8* %new_str, i64 %str1_len
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* %str2_dest, i8* %str2, i64 %str2_len, i1 false)
    %end_str = getelementptr i8, i8* %new_str, i64 %total_len
    store i8 0, i8* %end_str
    ret i8* %new_str
}