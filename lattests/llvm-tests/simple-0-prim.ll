declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main() {
  %a = alloca i32
  store i32 1, i32* %a
  ret i32 0
}

