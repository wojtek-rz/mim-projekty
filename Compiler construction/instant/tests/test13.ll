@dnl = internal constant [4 x i8] c"%d\0A\00"declare i32 @printf(i8*, ...)
define void @printInt(i32 %x) {
   %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
   ret void
}
define i32 @main() {
%r0 = mul i32 11, 2
%r1 = add i32 30, 14
%r2 = add i32 2, 2
%r3 = sdiv i32 %r1, %r2
%r4 = sdiv i32 %r0, %r3
call void @printInt(i32 %r4)
ret i32 0
}
