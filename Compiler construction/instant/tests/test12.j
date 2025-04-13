.class public test12
.super java/lang/Object

.method public <init>()V
    aload_0
    invokespecial java/lang/Object/<init>()V
    return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 3
.limit stack 3
iconst_1
ldc 1000000000
isub
istore_0
iload_0
iconst_1
isub
getstatic java/lang/System/out Ljava/io/PrintStream;
swap

invokevirtual java/io/PrintStream/println(I)V
iconst_1
iload_0
isub
getstatic java/lang/System/out Ljava/io/PrintStream;
swap

invokevirtual java/io/PrintStream/println(I)V

return
.end method
