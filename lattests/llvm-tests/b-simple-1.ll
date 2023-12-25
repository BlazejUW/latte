declare i32 @printInt(i32 %x)

define i32 @main() {
  call void @printInt(i32 3)
  ret i32 0
}
