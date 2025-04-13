

define i32 @main() {
entry:
    br label %cond_while_2
cond_while_2:
    %b_0_3 = phi i32 [16, %entry], [%b_0_9, %continue_6]
    %c_1_3 = phi i32 [1, %entry], [%c_1_9, %continue_6]
    %d_2_3 = phi i32 [2, %entry], [%d_2_10, %continue_6]
    %temp_0 = icmp sgt i32 %b_0_3, 0
    br i1 %temp_0, label %body_while_1, label %after_while_3
body_while_1:
    %temp_1 = sub i32 %b_0_3, 1
    %temp_2 = mul i32 2, %c_1_3
    %temp_3 = icmp sgt i32 %temp_2, 10000
    br i1 %temp_3, label %true_4, label %or_middle_7
or_middle_7:
    %temp_4 = icmp slt i32 %temp_2, 100
    br i1 %temp_4, label %true_4, label %false_5
true_4:
    %d_2_7 = add i32 %d_2_3, 1
    br label %continue_6
false_5:
    %d_2_9 = sub i32 %d_2_3, 1
    br label %continue_6
continue_6:
    %b_0_9 = phi i32 [%temp_1, %true_4], [%temp_1, %false_5]
    %c_1_9 = phi i32 [%temp_2, %true_4], [%temp_2, %false_5]
    %d_2_10 = phi i32 [%d_2_7, %true_4], [%d_2_9, %false_5]
    br label %cond_while_2
after_while_3:
    call void @printInt(i32 %c_1_3)
    call void @printInt(i32 %d_2_3)
    ret i32 0
}

@dnl = internal constant [4 x i8] c"%d\0A\00"
@d   = internal constant [3 x i8] c"%d\00"
@runtime_error = private constant [15 x i8] c"Runtime error\0A\00"

declare i32 @printf(i8*, ...)
declare i32 @scanf(i8*, ...)
declare i32 @puts(i8*)
declare void @exit(i32)

define void @printInt(i32 %x) {
   %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
   ret void
}

define void @printString(i8* %s) {
    call i32 @puts(i8* %s)
    ret void
}

define i32 @readInt() {
    %res = alloca i32
    %t1 = getelementptr [3 x i8], [3 x i8]* @d, i32 0, i32 0
    call i32 (i8*, ...) @scanf(i8* %t1, i32* %res)
    %t2 = load i32, i32* %res
    ret i32 %t2
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
    %total_len = add i32 %len1, %len2
    %new_str = call i8* @malloc(i32 %total_len)
    call void @memcpy(i8* %new_str, i8* %str1, i32 %len1)
    %offset = getelementptr i8, i8* %new_str, i32 %len1
    call void @memcpy(i8* %offset, i8* %str2, i32 %len2)
    ret i8* %new_str
}

