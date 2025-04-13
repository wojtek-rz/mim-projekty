.class public test06
.super java/lang/Object

.method public <init>()V
    aload_0
    invokespecial java/lang/Object/<init>()V
    return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 16
.limit stack 4
iconst_0
istore_0
iconst_1
istore_1
iconst_0
istore_2
iconst_1
istore_3
iconst_0
istore 4
iconst_1
istore 5
iconst_0
istore 6
iconst_1
istore 7
iload_2
iload_3
imul
iload 6
iload 7
iadd
iload 5
iadd
iload 4
iadd
iadd
iload_0
iload_1
imul
iadd
getstatic java/lang/System/out Ljava/io/PrintStream;
swap

invokevirtual java/io/PrintStream/println(I)V
iconst_1
istore_0
iconst_2
istore_1
iconst_1
istore_2
iconst_2
istore_3
iconst_1
istore 4
iconst_2
istore 5
iconst_1
istore 6
iconst_2
istore 7
iconst_1
istore 8
iconst_2
istore 9
iconst_1
istore 10
iconst_2
istore 11
iconst_1
istore 12
iconst_2
istore 13
iload_1
iconst_2
idiv
iload 9
iconst_2
idiv
iload 12
iload 13
iadd
iload 11
iadd
iload 10
iadd
iadd
iload 8
iadd
iload 7
iadd
iload 6
iadd
iload 5
iadd
iload 4
iadd
iload_3
iadd
iload_2
iadd
iadd
iconst_2
iload_0
imul
iadd
bipush 10
idiv
getstatic java/lang/System/out Ljava/io/PrintStream;
swap

invokevirtual java/io/PrintStream/println(I)V

return
.end method
