@dnl = internal constant [4 x i8] c"%d\0A\00"declare i32 @printf(i8*, ...)
define void @printInt(i32 %x) {
   %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
   ret void
}
define i32 @main() {
%r0 = alloca i32

%r1 = alloca i32

%r2 = alloca i32

%r3 = alloca i32

%r4 = alloca i32

%r5 = alloca i32

%r6 = alloca i32

%r7 = alloca i32

%r8 = alloca i32

%r9 = alloca i32

%r10 = alloca i32

%r11 = alloca i32

%r12 = alloca i32

%r13 = alloca i32

store i32 0, i32* %r0
store i32 1, i32* %r1
store i32 0, i32* %r2
store i32 1, i32* %r3
store i32 0, i32* %r4
store i32 1, i32* %r5
store i32 0, i32* %r6
store i32 1, i32* %r7
%r14 = load i32,  i32* %r0
%r15 = load i32,  i32* %r1
%r16 = mul i32 %r14, %r15
%r17 = load i32,  i32* %r2
%r18 = load i32,  i32* %r3
%r19 = mul i32 %r17, %r18
%r20 = load i32,  i32* %r4
%r21 = load i32,  i32* %r5
%r22 = load i32,  i32* %r6
%r23 = load i32,  i32* %r7
%r24 = add i32 %r22, %r23
%r25 = add i32 %r21, %r24
%r26 = add i32 %r20, %r25
%r27 = add i32 %r19, %r26
%r28 = add i32 %r16, %r27
call void @printInt(i32 %r28)
store i32 1, i32* %r0
store i32 2, i32* %r1
store i32 1, i32* %r2
store i32 2, i32* %r3
store i32 1, i32* %r4
store i32 2, i32* %r5
store i32 1, i32* %r6
store i32 2, i32* %r7
store i32 1, i32* %r8
store i32 2, i32* %r9
store i32 1, i32* %r10
store i32 2, i32* %r11
store i32 1, i32* %r12
store i32 2, i32* %r13
%r29 = load i32,  i32* %r0
%r30 = mul i32 2, %r29
%r31 = load i32,  i32* %r1
%r32 = sdiv i32 %r31, 2
%r33 = load i32,  i32* %r2
%r34 = load i32,  i32* %r3
%r35 = load i32,  i32* %r4
%r36 = load i32,  i32* %r5
%r37 = load i32,  i32* %r6
%r38 = load i32,  i32* %r7
%r39 = load i32,  i32* %r8
%r40 = load i32,  i32* %r9
%r41 = sdiv i32 %r40, 2
%r42 = load i32,  i32* %r10
%r43 = load i32,  i32* %r11
%r44 = load i32,  i32* %r12
%r45 = load i32,  i32* %r13
%r46 = add i32 %r44, %r45
%r47 = add i32 %r43, %r46
%r48 = add i32 %r42, %r47
%r49 = add i32 %r41, %r48
%r50 = add i32 %r39, %r49
%r51 = add i32 %r38, %r50
%r52 = add i32 %r37, %r51
%r53 = add i32 %r36, %r52
%r54 = add i32 %r35, %r53
%r55 = add i32 %r34, %r54
%r56 = add i32 %r33, %r55
%r57 = add i32 %r32, %r56
%r58 = add i32 %r30, %r57
%r59 = sdiv i32 %r58, 10
call void @printInt(i32 %r59)
ret i32 0
}
