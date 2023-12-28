declare i32 @printString(i8* %str)
declare i32 @printInt(i32 %i)
define i32 @main() {
  call void @printInt(i32 5)
  ret i32 0
}
