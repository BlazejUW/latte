@strInt = constant [4 x i8] c"%d\0A\00", align 1
@loadStrInt = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@errorMsg = private unnamed_addr constant [15 x i8] c"runtime error\0A\00", align 1
@newline = private unnamed_addr constant [2 x i8] c"\0A\00"
; @buffer = common global [256 x i8] zeroinitializer, align 1
declare i32 @scanf(i8*, ...)
declare i32 @printf(i8*, ...)
declare void @exit(i32)
declare void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i1)

declare i64 @strlen(i8*)
declare i8* @malloc(i32)
declare i8* @strcpy(i8*, i8*)


; declare i8* @fgets(i8*, i32, i8*)
; declare i8* @llvm.get.stdin() ; External function to get stdin

; define i8* @readString() {
;   %buffer_ptr = getelementptr inbounds [256 x i8], [256 x i8]* @buffer, i32 0, i32 0
;   %stdin = call i8* @llvm.get.stdin()
;   call i8* @fgets(i8* %buffer_ptr, i32 256, i8* %stdin)
;   ret i8* %buffer_ptr
; }

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

; define i8* @concatStrings(i8* %str1, i8* %str2) {
;     %len1 = call i32 @strlen(i8* %str1)
;     %len2 = call i32 @strlen(i8* %str2)

;     %newLen = add i32 %len1, %len2
;     %newStr = call i8* @malloc(i32 %newLen)
;     call void @strcpy(i8* %newStr, i8* %str1)
;     %newStrEnd = getelementptr i8, i8* %newStr, i32 %len1
;     call void @strcpy(i8* %newStrEnd, i8* %str2)
;     ret i8* %newStr
; }
define i8* @concat(i8* %str1, i8* %str2) {
    ; Oblicz długość str1
    %str1_len = call i64 @strlen(i8* %str1)

    ; Oblicz długość str2
    %str2_len = call i64 @strlen(i8* %str2)

    ; Oblicz łączną długość (dodaj 1 na null terminator)
    %total_len = add i64 %str1_len, %str2_len
    %total_len_inc_null = add i64 %total_len, 1

    ; Alokuj pamięć na nowy łańcuch
    %new_str = call i8* @malloc(i64 %total_len_inc_null)

    ; Skopiuj str1 do nowej pamięci
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* %new_str, i8* %str1, i64 %str1_len, i1 false)

    ; Skopiuj str2 do nowej pamięci (za str1)
    %str2_dest = getelementptr i8, i8* %new_str, i64 %str1_len
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* %str2_dest, i8* %str2, i64 %str2_len, i1 false)

    ; Dodaj null terminator
    %end_str = getelementptr i8, i8* %new_str, i64 %total_len
    store i8 0, i8* %end_str

    ; Zwróć nowy łańcuch
    ret i8* %new_str
}