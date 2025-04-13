@dnl = internal constant [4 x i8] c"%d\0A\00"declare i32 @printf(i8*, ...)
define void @printInt(i32 %x) {
   %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
   ret void
}
define i32 @main() {
%r0 = alloca i32

%r1 = sub i32 1, 1000000000
store i32 %r1, i32* %r0
%r2 = load i32,  i32* %r0
%r3 = sub i32 %r2, 1
call void @printInt(i32 %r3)
%r4 = load i32,  i32* %r0
%r5 = sub i32 1, %r4
call void @printInt(i32 %r5)
ret i32 0
}
