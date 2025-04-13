@dnl = internal constant [4 x i8] c"%d\0A\00"declare i32 @printf(i8*, ...)
define void @printInt(i32 %x) {
   %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
   ret void
}
define i32 @main() {
%r0 = add i32 1, 1
%r1 = add i32 1, %r0
%r2 = add i32 1, %r1
%r3 = add i32 1, %r2
%r4 = add i32 1, %r3
%r5 = add i32 1, %r4
%r6 = add i32 1, %r5
%r7 = add i32 1, %r6
%r8 = add i32 1, %r7
%r9 = add i32 1, %r8
%r10 = add i32 1, %r9
%r11 = add i32 1, %r10
%r12 = add i32 1, %r11
%r13 = add i32 1, %r12
%r14 = add i32 1, %r13
%r15 = add i32 1, %r14
%r16 = add i32 1, %r15
%r17 = add i32 1, %r16
%r18 = add i32 1, %r17
%r19 = add i32 1, %r18
%r20 = add i32 1, %r19
%r21 = add i32 1, %r20
%r22 = add i32 1, %r21
%r23 = add i32 1, %r22
%r24 = add i32 1, %r23
%r25 = add i32 1, %r24
%r26 = add i32 1, %r25
%r27 = add i32 1, %r26
%r28 = add i32 1, %r27
%r29 = add i32 1, %r28
%r30 = add i32 1, %r29
%r31 = add i32 1, %r30
%r32 = add i32 1, %r31
%r33 = add i32 1, %r32
%r34 = add i32 1, %r33
%r35 = add i32 1, %r34
%r36 = add i32 1, %r35
%r37 = add i32 1, %r36
%r38 = add i32 1, %r37
%r39 = add i32 1, %r38
%r40 = add i32 1, %r39
call void @printInt(i32 %r40)
ret i32 0
}
