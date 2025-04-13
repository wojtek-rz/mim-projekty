@dnl = internal constant [4 x i8] c"%d\0A\00"declare i32 @printf(i8*, ...)
define void @printInt(i32 %x) {
   %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
   ret void
}
define i32 @main() {
%r0 = sub i32 1, 1
%r1 = sub i32 1, 1
%r2 = sub i32 1, 1
%r3 = sub i32 1, 1
%r4 = sub i32 1, 1
%r5 = sub i32 1, 1
%r6 = sub i32 1, 1
%r7 = sub i32 1, 1
%r8 = sub i32 1, 1
%r9 = sub i32 1, 1
%r10 = sub i32 1, 1
%r11 = sub i32 1, 1
%r12 = sub i32 1, 1
%r13 = sub i32 1, 1
%r14 = sub i32 1, 1
%r15 = sub i32 1, 1
%r16 = sub i32 1, 1
%r17 = sub i32 1, 1
%r18 = sub i32 1, 1
%r19 = add i32 %r17, %r18
%r20 = add i32 %r16, %r19
%r21 = add i32 %r15, %r20
%r22 = add i32 %r14, %r21
%r23 = add i32 %r13, %r22
%r24 = add i32 %r12, %r23
%r25 = add i32 %r11, %r24
%r26 = add i32 %r10, %r25
%r27 = add i32 %r9, %r26
%r28 = add i32 %r8, %r27
%r29 = add i32 %r7, %r28
%r30 = add i32 %r6, %r29
%r31 = add i32 %r5, %r30
%r32 = add i32 %r4, %r31
%r33 = add i32 %r3, %r32
%r34 = add i32 %r2, %r33
%r35 = add i32 %r1, %r34
%r36 = add i32 %r0, %r35
%r37 = add i32 1, %r36
call void @printInt(i32 %r37)
ret i32 0
}
