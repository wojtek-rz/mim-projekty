%__arr__ = type {
    i8*,
    i32
}



define i32 @main() {
entry:
    %temp_0 = call i8* @malloc(i32 12)
    %temp_1 = bitcast i8* %temp_0 to %__arr__*
    %temp_2 = mul i32 3, 4
    %temp_3 = call i8* @malloc(i32 %temp_2)
    %temp_4 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    store i8* %temp_3, i8** %temp_4
    %temp_6 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 1
    store i32 3, i32* %temp_6
    %temp_8 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    %temp_9 = load i8*, i8** %temp_8
    %temp_10 = mul i32 0, 4
    %temp_11 = getelementptr i8, i8* %temp_9, i32 %temp_10
    store i32 11, %__arr__* %temp_11
    %temp_13 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    %temp_14 = load i8*, i8** %temp_13
    %temp_15 = mul i32 1, 4
    %temp_16 = getelementptr i8, i8* %temp_14, i32 %temp_15
    store i32 12, %__arr__* %temp_16
    %temp_18 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    %temp_19 = load i8*, i8** %temp_18
    %temp_20 = mul i32 2, 4
    %temp_21 = getelementptr i8, i8* %temp_19, i32 %temp_20
    store i32 13, %__arr__* %temp_21
    br label %cond_while_2
cond_while_2:
    %a_0_3 = phi %__arr__* [%temp_1, %entry], [%a_0_3, %body_while_1]
    %i_1_3 = phi i32 [0, %entry], [%temp_31, %body_while_1]
    %x_2_3 = phi i32 [0, %entry], [%temp_30, %body_while_1]
    %temp_23 = getelementptr %__arr__, %__arr__* %a_0_3, i32 0, i32 1
    %temp_24 = load i32, i32* %temp_23
    %temp_25 = icmp slt i32 %i_1_3, %temp_24
    br i1 %temp_25, label %body_while_1, label %after_while_3
body_while_1:
    %temp_26 = getelementptr %__arr__, %__arr__* %a_0_3, i32 0, i32 0
    %temp_27 = load i8*, i8** %temp_26
    %temp_28 = mul i32 %i_1_3, 4
    %temp_29 = getelementptr i8, i8* %temp_27, i32 %temp_28
    %temp_30 = load i32, %__arr__* %temp_29
    %temp_31 = add i32 %i_1_3, 1
    call void @printInt(i32 %temp_30)
    br label %cond_while_2
after_while_3:
    ret i32 0
}

@_dnl = internal constant [4 x i8] c"%d\0A\00"
@_d   = internal constant [3 x i8] c"%d\00"
@_s  = internal constant [3 x i8] c"%s\00"
@runtime_error = private constant [15 x i8] c"Runtime error\0A\00"

declare i32 @printf(i8*, ...)
declare i32 @scanf(i8*, ...)
declare i32 @puts(i8*)
declare void @exit(i32)

define void @printInt(i32 %x) {
   %t0 = getelementptr [4 x i8], [4 x i8]* @_dnl, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
   ret void
}

define void @printString(i8* %s) {
    call i32 @puts(i8* %s)
    ret void
}

define i32 @readInt() {
    %res = alloca i32
    %t1 = getelementptr [3 x i8], [3 x i8]* @_d, i32 0, i32 0
    call i32 (i8*, ...) @scanf(i8* %t1, i32* %res)
    %t2 = load i32, i32* %res
    ret i32 %t2
}

define i8* @readString() {
    %res = call i8* @malloc(i32 1024)
    %t1 = getelementptr [3 x i8], [3 x i8]* @_s, i32 0, i32 0
    call i32 (i8*, ...) @scanf(i8* %t1, i8* %res)
    ret i8* %res
}

define void @error() {
    %msg = getelementptr [15 x i8], [15 x i8]* @runtime_error, i32 0, i32 0
    call i32 @puts(i8* %msg)
    call void @exit(i32 1)
    unreachable
}

declare i8* @malloc(i32)
declare i32 @strlen(i8*)
declare void @memcpy(i8*, i8*, i32)
declare i32 @strcmp(i8*, i8*)

define i8* @_concat_strings(i8* %str1, i8* %str2) {
entry:
    %len1 = call i32 @strlen(i8* %str1)
    %len2 = call i32 @strlen(i8* %str2)
    %sum_len = add i32 %len1, %len2
    %total_len = add i32 %sum_len, 1
    %new_str = call i8* @malloc(i32 %total_len)
    call void @memcpy(i8* %new_str, i8* %str1, i32 %len1)
    %offset = getelementptr i8, i8* %new_str, i32 %len1
    call void @memcpy(i8* %offset, i8* %str2, i32 %len2)
    %end = getelementptr i8, i8* %new_str, i32 %sum_len
    store i8 0, i8* %end
    ret i8* %new_str
}

