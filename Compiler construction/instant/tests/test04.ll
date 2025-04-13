@dnl = internal constant [4 x i8] c"%d\0A\00"declare i32 @printf(i8*, ...)
define void @printInt(i32 %x) {
   %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
   ret void
}
define i32 @main() {
%r0 = alloca i32

%r1 = alloca i32

store i32 1, i32* %r0
store i32 2, i32* %r1
%r2 = load i32,  i32* %r1
%r3 = load i32,  i32* %r0
%r4 = load i32,  i32* %r0
%r5 = load i32,  i32* %r0
%r6 = load i32,  i32* %r0
%r7 = load i32,  i32* %r0
%r8 = load i32,  i32* %r0
%r9 = load i32,  i32* %r0
%r10 = load i32,  i32* %r0
%r11 = load i32,  i32* %r0
%r12 = load i32,  i32* %r0
%r13 = load i32,  i32* %r1
%r14 = load i32,  i32* %r0
%r15 = load i32,  i32* %r0
%r16 = load i32,  i32* %r0
%r17 = load i32,  i32* %r0
%r18 = load i32,  i32* %r0
%r19 = load i32,  i32* %r0
%r20 = load i32,  i32* %r0
%r21 = load i32,  i32* %r0
%r22 = load i32,  i32* %r0
%r23 = load i32,  i32* %r0
%r24 = load i32,  i32* %r0
%r25 = load i32,  i32* %r0
%r26 = load i32,  i32* %r0
%r27 = load i32,  i32* %r0
%r28 = load i32,  i32* %r0
%r29 = load i32,  i32* %r0
%r30 = load i32,  i32* %r0
%r31 = load i32,  i32* %r0
%r32 = load i32,  i32* %r0
%r33 = load i32,  i32* %r1
%r34 = add i32 %r32, %r33
%r35 = add i32 1, %r34
%r36 = add i32 %r31, %r35
%r37 = add i32 %r30, %r36
%r38 = add i32 1, %r37
%r39 = add i32 %r29, %r38
%r40 = add i32 %r28, %r39
%r41 = add i32 1, %r40
%r42 = add i32 %r27, %r41
%r43 = add i32 %r26, %r42
%r44 = add i32 %r25, %r43
%r45 = add i32 %r24, %r44
%r46 = add i32 1, %r45
%r47 = add i32 %r23, %r46
%r48 = add i32 %r22, %r47
%r49 = add i32 %r21, %r48
%r50 = add i32 %r20, %r49
%r51 = add i32 %r19, %r50
%r52 = add i32 %r18, %r51
%r53 = add i32 %r17, %r52
%r54 = add i32 %r16, %r53
%r55 = add i32 %r15, %r54
%r56 = add i32 %r14, %r55
%r57 = add i32 1, %r56
%r58 = add i32 %r13, %r57
%r59 = add i32 %r12, %r58
%r60 = add i32 %r11, %r59
%r61 = add i32 %r10, %r60
%r62 = add i32 1, %r61
%r63 = add i32 %r9, %r62
%r64 = add i32 %r8, %r63
%r65 = add i32 %r7, %r64
%r66 = add i32 %r6, %r65
%r67 = add i32 %r5, %r66
%r68 = add i32 1, %r67
%r69 = add i32 %r4, %r68
%r70 = add i32 %r3, %r69
%r71 = add i32 %r2, %r70
call void @printInt(i32 %r71)
ret i32 0
}
