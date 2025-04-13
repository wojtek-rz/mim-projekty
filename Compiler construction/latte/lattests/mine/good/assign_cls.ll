%Rect = type {
    i32,
    i32,
    i8*
}

@_global_0 = internal constant [7 x i8] c"Emilka\00"

define %Rect* @createRectangle() {
entry:
    %temp_0 = call i8* @malloc(i32 16)
    %temp_1 = bitcast i8* %temp_0 to %Rect*
    %temp_2 = getelementptr %Rect, %Rect* %temp_1, i32 0, i32 0
    store i32 20, i32* %temp_2
    %temp_4 = getelementptr %Rect, %Rect* %temp_1, i32 0, i32 1
    store i32 30, i32* %temp_4
    %temp_6 = getelementptr [7 x i8], [7 x i8]* @_global_0, i32 0, i32 0
    %temp_7 = getelementptr %Rect, %Rect* %temp_1, i32 0, i32 2
    store i8* %temp_6, i8** %temp_7
    ret %Rect* %temp_1
}

define i32 @main() {
entry:
    %temp_0 = call %Rect* @createRectangle()
    call void @printRectangle(%Rect* %temp_0)
    ret i32 0
}

define void @printRectangle(%Rect* %rect_0_1) {
entry:
    %temp_0 = getelementptr %Rect, %Rect* %rect_0_1, i32 0, i32 0
    %temp_1 = load i32, i32* %temp_0
    call void @printInt(i32 %temp_1)
    %temp_3 = getelementptr %Rect, %Rect* %rect_0_1, i32 0, i32 1
    %temp_4 = load i32, i32* %temp_3
    call void @printInt(i32 %temp_4)
    %temp_6 = getelementptr %Rect, %Rect* %rect_0_1, i32 0, i32 2
    %temp_7 = load i8*, i8** %temp_6
    call void @printString(i8* %temp_7)
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

