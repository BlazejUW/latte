declare void @printString(i8* %str)
declare void @printInt(i32 %i)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
define i32 @main()
{
entry:
%0 = add i32 0, 5
%1 = add i32 0, 6
; Inline function call: inline__addAndPrint(IntegerInteger)
; Arguments: %0, %1
; inline function block: 
;[Decl (Just (10,5)) (Int (Just (10,5))) [Init (Just (10,9)) (Ident "c") (EAdd (Just (10,13)) (EVar (Just (10,13)) (Ident "a")) (Plus (Just (10,15))) (EVar (Just (10,17)) (Ident "b")))],SExp (Just (11,5)) (EApp (Just (11,5)) (Ident "printInt") [EVar (Just (11,14)) (Ident "c")]),Ret (Just (12,5)) (EVar (Just (12,12)) (Ident "c"))]
; compile:  Decl (Just (10,5)) (Int (Just (10,5))) [Init (Just (10,9)) (Ident "c") (EAdd (Just (10,13)) (EVar (Just (10,13)) (Ident "a")) (Plus (Just (10,15))) (EVar (Just (10,17)) (Ident "b")))]
%2 = add i32 %0, %1
; compile:  SExp (Just (11,5)) (EApp (Just (11,5)) (Ident "printInt") [EVar (Just (11,14)) (Ident "c")])
call void @printInt(i32 %2)
; return EVar (Just (12,12)) (Ident "c")
; return %2
%3 = add i32 %2, 1
call void @printInt(i32 %3)
ret i32 0
}
define i32 @inline__addAndPrint(i32 %a, i32 %b)
{
entry:
%0 = add i32 %a, %b
call void @printInt(i32 %0)
ret i32 %0
}
