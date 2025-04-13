%Company = type {
    %__arr__*,
    i8*
}
%Person = type {
    i32,
    i8*,
    %Company*
}
%__arr__ = type {
    i8*,
    i32
}

@_global_0 = internal constant [8 x i8] c"netflix\00"
@_global_1 = internal constant [5 x i8] c"Adam\00"
@_global_2 = internal constant [7 x i8] c"Michal\00"

define %__arr__* @appendCompany(%__arr__* %companies_0_1, %Company* %val_1_1) {
entry:
    %temp_0 = call i8* @malloc(i32 12)
    %temp_1 = bitcast i8* %temp_0 to %__arr__*
    %temp_3 = getelementptr %__arr__, %__arr__* %companies_0_1, i32 0, i32 1
    %temp_4 = load i32, i32* %temp_3
    %temp_2 = mul i32 %temp_4, 16
    %temp_5 = call i8* @malloc(i32 %temp_2)
    %temp_6 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    store i8* %temp_5, i8** %temp_6
    %temp_8 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 1
    store i32 %temp_4, i32* %temp_8
    br label %cond_while_2
cond_while_2:
    %__array___6_3 = phi %__arr__* [%companies_0_1, %entry], [%__array___6_3, %body_while_1]
    %__i___4_3 = phi i32 [0, %entry], [%temp_18, %body_while_1]
    %c_5_3 = phi %Company* [null, %entry], [%temp_17, %body_while_1]
    %companies_0_2 = phi %__arr__* [%companies_0_1, %entry], [%companies_0_2, %body_while_1]
    %extended_2_3 = phi %__arr__* [%temp_1, %entry], [%extended_2_3, %body_while_1]
    %i_3_3 = phi i32 [0, %entry], [%temp_24, %body_while_1]
    %val_1_2 = phi %Company* [%val_1_1, %entry], [%val_1_2, %body_while_1]
    %temp_10 = getelementptr %__arr__, %__arr__* %__array___6_3, i32 0, i32 1
    %temp_11 = load i32, i32* %temp_10
    %temp_12 = icmp slt i32 %__i___4_3, %temp_11
    br i1 %temp_12, label %body_while_1, label %after_while_3
body_while_1:
    %temp_13 = getelementptr %__arr__, %__arr__* %__array___6_3, i32 0, i32 0
    %temp_14 = load i8*, i8** %temp_13
    %temp_15 = bitcast i8* %temp_14 to %Company**
    %temp_16 = getelementptr %Company*, %Company** %temp_15, i32 %__i___4_3
    %temp_17 = load %Company*, %Company** %temp_16
    %temp_18 = add i32 %__i___4_3, 1
    %temp_19 = getelementptr %__arr__, %__arr__* %extended_2_3, i32 0, i32 0
    %temp_20 = load i8*, i8** %temp_19
    %temp_21 = bitcast i8* %temp_20 to %Company**
    %temp_22 = getelementptr %Company*, %Company** %temp_21, i32 %i_3_3
    store %Company* %temp_17, %Company** %temp_22
    %temp_24 = add i32 %i_3_3, 1
    br label %cond_while_2
after_while_3:
    %temp_25 = getelementptr %__arr__, %__arr__* %extended_2_3, i32 0, i32 0
    %temp_26 = load i8*, i8** %temp_25
    %temp_27 = bitcast i8* %temp_26 to %Company**
    %temp_28 = getelementptr %Company*, %Company** %temp_27, i32 %i_3_3
    store %Company* %val_1_2, %Company** %temp_28
    ret %__arr__* %extended_2_3
}

define %__arr__* @appendPerson(%__arr__* %people_0_1, %Person* %val_1_1) {
entry:
    %temp_0 = call i8* @malloc(i32 12)
    %temp_1 = bitcast i8* %temp_0 to %__arr__*
    %temp_3 = getelementptr %__arr__, %__arr__* %people_0_1, i32 0, i32 1
    %temp_4 = load i32, i32* %temp_3
    %temp_5 = add i32 %temp_4, 1
    %temp_2 = mul i32 %temp_5, 20
    %temp_6 = call i8* @malloc(i32 %temp_2)
    %temp_7 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 0
    store i8* %temp_6, i8** %temp_7
    %temp_9 = getelementptr %__arr__, %__arr__* %temp_1, i32 0, i32 1
    store i32 %temp_5, i32* %temp_9
    br label %cond_while_2
cond_while_2:
    %__array___6_3 = phi %__arr__* [%people_0_1, %entry], [%__array___6_3, %body_while_1]
    %__i___4_3 = phi i32 [0, %entry], [%temp_19, %body_while_1]
    %c_5_3 = phi %Person* [null, %entry], [%temp_18, %body_while_1]
    %extended_2_3 = phi %__arr__* [%temp_1, %entry], [%extended_2_3, %body_while_1]
    %i_3_3 = phi i32 [0, %entry], [%temp_25, %body_while_1]
    %people_0_2 = phi %__arr__* [%people_0_1, %entry], [%people_0_2, %body_while_1]
    %val_1_2 = phi %Person* [%val_1_1, %entry], [%val_1_2, %body_while_1]
    %temp_11 = getelementptr %__arr__, %__arr__* %__array___6_3, i32 0, i32 1
    %temp_12 = load i32, i32* %temp_11
    %temp_13 = icmp slt i32 %__i___4_3, %temp_12
    br i1 %temp_13, label %body_while_1, label %after_while_3
body_while_1:
    %temp_14 = getelementptr %__arr__, %__arr__* %__array___6_3, i32 0, i32 0
    %temp_15 = load i8*, i8** %temp_14
    %temp_16 = bitcast i8* %temp_15 to %Person**
    %temp_17 = getelementptr %Person*, %Person** %temp_16, i32 %__i___4_3
    %temp_18 = load %Person*, %Person** %temp_17
    %temp_19 = add i32 %__i___4_3, 1
    %temp_20 = getelementptr %__arr__, %__arr__* %extended_2_3, i32 0, i32 0
    %temp_21 = load i8*, i8** %temp_20
    %temp_22 = bitcast i8* %temp_21 to %Person**
    %temp_23 = getelementptr %Person*, %Person** %temp_22, i32 %i_3_3
    store %Person* %temp_18, %Person** %temp_23
    %temp_25 = add i32 %i_3_3, 1
    br label %cond_while_2
after_while_3:
    %temp_26 = getelementptr %__arr__, %__arr__* %extended_2_3, i32 0, i32 0
    %temp_27 = load i8*, i8** %temp_26
    %temp_28 = bitcast i8* %temp_27 to %Person**
    %temp_29 = getelementptr %Person*, %Person** %temp_28, i32 %i_3_3
    store %Person* %val_1_2, %Person** %temp_29
    ret %__arr__* %extended_2_3
}

define %Person* @createPerson(i32 %a_0_1, i8* %n_1_1, %Company* %c_2_1) {
entry:
    %temp_0 = call i8* @malloc(i32 20)
    %temp_1 = bitcast i8* %temp_0 to %Person*
    %temp_2 = getelementptr %Person, %Person* %temp_1, i32 0, i32 0
    store i32 %a_0_1, i32* %temp_2
    %temp_4 = getelementptr %Person, %Person* %temp_1, i32 0, i32 1
    store i8* %n_1_1, i8** %temp_4
    %temp_6 = getelementptr %Person, %Person* %temp_1, i32 0, i32 2
    store %Company* %c_2_1, %Company** %temp_6
    ret %Person* %temp_1
}

define i32 @main() {
entry:
    %temp_0 = call i8* @malloc(i32 16)
    %temp_1 = bitcast i8* %temp_0 to %Company*
    %temp_2 = getelementptr [8 x i8], [8 x i8]* @_global_0, i32 0, i32 0
    %temp_3 = getelementptr %Company, %Company* %temp_1, i32 0, i32 1
    store i8* %temp_2, i8** %temp_3
    %temp_5 = call i8* @malloc(i32 12)
    %temp_6 = bitcast i8* %temp_5 to %__arr__*
    %temp_7 = mul i32 0, 20
    %temp_8 = call i8* @malloc(i32 %temp_7)
    %temp_9 = getelementptr %__arr__, %__arr__* %temp_6, i32 0, i32 0
    store i8* %temp_8, i8** %temp_9
    %temp_11 = getelementptr %__arr__, %__arr__* %temp_6, i32 0, i32 1
    store i32 0, i32* %temp_11
    %temp_13 = getelementptr %Company, %Company* %temp_1, i32 0, i32 0
    store %__arr__* %temp_6, %__arr__** %temp_13
    %temp_15 = getelementptr [5 x i8], [5 x i8]* @_global_1, i32 0, i32 0
    %temp_16 = call %Person* @createPerson(i32 10, i8* %temp_15, %Company* %temp_1)
    %temp_17 = getelementptr [7 x i8], [7 x i8]* @_global_2, i32 0, i32 0
    %temp_18 = call %Person* @createPerson(i32 12, i8* %temp_17, %Company* %temp_1)
    %temp_19 = getelementptr %Company, %Company* %temp_1, i32 0, i32 0
    %temp_20 = load %__arr__*, %__arr__** %temp_19
    %temp_21 = call %__arr__* @appendPerson(%__arr__* %temp_20, %Person* %temp_16)
    %temp_22 = getelementptr %Company, %Company* %temp_1, i32 0, i32 0
    store %__arr__* %temp_21, %__arr__** %temp_22
    %temp_24 = getelementptr %Company, %Company* %temp_1, i32 0, i32 0
    %temp_25 = load %__arr__*, %__arr__** %temp_24
    %temp_26 = call %__arr__* @appendPerson(%__arr__* %temp_25, %Person* %temp_18)
    %temp_27 = getelementptr %Company, %Company* %temp_1, i32 0, i32 0
    store %__arr__* %temp_26, %__arr__** %temp_27
    call void @printCompany(%Company* %temp_1)
    ret i32 0
}

define void @printCompany(%Company* %c_0_1) {
entry:
    %temp_0 = getelementptr %Company, %Company* %c_0_1, i32 0, i32 1
    %temp_1 = load i8*, i8** %temp_0
    call void @printString(i8* %temp_1)
    %temp_3 = getelementptr %Company, %Company* %c_0_1, i32 0, i32 0
    %temp_4 = load %__arr__*, %__arr__** %temp_3
    br label %cond_while_2
cond_while_2:
    %__array___3_3 = phi %__arr__* [%temp_4, %entry], [%__array___3_3, %body_while_1]
    %__i___1_3 = phi i32 [0, %entry], [%temp_13, %body_while_1]
    %c_0_2 = phi %Company* [%c_0_1, %entry], [%c_0_2, %body_while_1]
    %p_2_3 = phi %Person* [null, %entry], [%temp_12, %body_while_1]
    %temp_5 = getelementptr %__arr__, %__arr__* %__array___3_3, i32 0, i32 1
    %temp_6 = load i32, i32* %temp_5
    %temp_7 = icmp slt i32 %__i___1_3, %temp_6
    br i1 %temp_7, label %body_while_1, label %after_while_3
body_while_1:
    %temp_8 = getelementptr %__arr__, %__arr__* %__array___3_3, i32 0, i32 0
    %temp_9 = load i8*, i8** %temp_8
    %temp_10 = bitcast i8* %temp_9 to %Person**
    %temp_11 = getelementptr %Person*, %Person** %temp_10, i32 %__i___1_3
    %temp_12 = load %Person*, %Person** %temp_11
    %temp_13 = add i32 %__i___1_3, 1
    call void @printPerson(%Person* %temp_12)
    br label %cond_while_2
after_while_3:
    ret void
}

define void @printPerson(%Person* %p_0_1) {
entry:
    %temp_0 = getelementptr %Person, %Person* %p_0_1, i32 0, i32 0
    %temp_1 = load i32, i32* %temp_0
    call void @printInt(i32 %temp_1)
    %temp_3 = getelementptr %Person, %Person* %p_0_1, i32 0, i32 1
    %temp_4 = load i8*, i8** %temp_3
    call void @printString(i8* %temp_4)
    %temp_6 = getelementptr %Person, %Person* %p_0_1, i32 0, i32 2
    %temp_7 = load %Company*, %Company** %temp_6
    %temp_8 = getelementptr %Company, %Company* %temp_7, i32 0, i32 1
    %temp_9 = load i8*, i8** %temp_8
    call void @printString(i8* %temp_9)
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

