%__arr__ = type {
    i8*,
    i32
}



define %__arr__* @createArr() {
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
    %temp_10 = bitcast i8* %temp_9 to i32*
    %temp_11 = getelementptr i32, i32* %temp_10, i32 0
    store i32 0, i32* %temp_11
    %temp_13 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    %temp_14 = load i8*, i8** %temp_13
    %temp_15 = bitcast i8* %temp_14 to i32*
    %temp_16 = getelementptr i32, i32* %temp_15, i32 1
    store i32 1, i32* %temp_16
    %temp_18 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    %temp_19 = load i8*, i8** %temp_18
    %temp_20 = bitcast i8* %temp_19 to i32*
    %temp_21 = getelementptr i32, i32* %temp_20, i32 2
    store i32 2, i32* %temp_21
    %temp_23 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    %temp_24 = load i8*, i8** %temp_23
    %temp_25 = bitcast i8* %temp_24 to i32*
    %temp_26 = getelementptr i32, i32* %temp_25, i32 1
    %temp_27 = load i32, i32* %temp_26
    %temp_28 = add i32 %temp_27, 1
    %temp_29 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    %temp_30 = load i8*, i8** %temp_29
    %temp_31 = bitcast i8* %temp_30 to i32*
    %temp_32 = getelementptr i32, i32* %temp_31, i32 1
    store i32 %temp_28, i32* %temp_32
    %temp_34 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    %temp_35 = load i8*, i8** %temp_34
    %temp_36 = bitcast i8* %temp_35 to i32*
    %temp_37 = getelementptr i32, i32* %temp_36, i32 1
    %temp_38 = load i32, i32* %temp_37
    %temp_39 = add i32 %temp_38, 1
    %temp_40 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    %temp_41 = load i8*, i8** %temp_40
    %temp_42 = bitcast i8* %temp_41 to i32*
    %temp_43 = getelementptr i32, i32* %temp_42, i32 1
    store i32 %temp_39, i32* %temp_43
    ret %__arr__* %temp_1
}

define i32 @main() {
entry:
    %temp_0 = call i8* @malloc(i32 12)
    %temp_1 = bitcast i8* %temp_0 to %__arr__*
    %temp_2 = mul i32 10, 4
    %temp_3 = call i8* @malloc(i32 %temp_2)
    %temp_4 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    store i8* %temp_3, i8** %temp_4
    %temp_6 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 1
    store i32 10, i32* %temp_6
    %temp_8 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    %temp_9 = load i8*, i8** %temp_8
    %temp_10 = bitcast i8* %temp_9 to i32*
    %temp_11 = getelementptr i32, i32* %temp_10, i32 0
    store i32 20, i32* %temp_11
    %temp_13 = call %__arr__* @createArr()
    call void @printArr(%__arr__* %temp_13, i32 3)
    ret i32 0
}

define void @printArr(%__arr__* %arr_0_1, i32 %size_1_1) {
entry:
    br label %cond_while_2
cond_while_2:
    %arr_0_2 = phi %__arr__* [%arr_0_1, %entry], [%arr_0_2, %body_while_1]
    %i_2_3 = phi i32 [0, %entry], [%temp_7, %body_while_1]
    %size_1_2 = phi i32 [%size_1_1, %entry], [%size_1_2, %body_while_1]
    %temp_0 = icmp slt i32 %i_2_3, %size_1_2
    br i1 %temp_0, label %body_while_1, label %after_while_3
body_while_1:
    %temp_1 = getelementptr %__arr__, %__arr__* %arr_0_2, i32 0, i32 0
    %temp_2 = load i8*, i8** %temp_1
    %temp_3 = bitcast i8* %temp_2 to i32*
    %temp_4 = getelementptr i32, i32* %temp_3, i32 %i_2_3
    %temp_5 = load i32, i32* %temp_4
    call void @printInt(i32 %temp_5)
    %temp_7 = add i32 %i_2_3, 1
    br label %cond_while_2
after_while_3:
    ret void
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

