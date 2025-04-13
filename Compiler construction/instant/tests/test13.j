.class public test13
.super java/lang/Object

.method public <init>()V
    aload_0
    invokespecial java/lang/Object/<init>()V
    return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 2
.limit stack 4
bipush 30
bipush 14
iadd
iconst_2
iconst_2
iadd
idiv
bipush 11
iconst_2
imul
swap
idiv
getstatic java/lang/System/out Ljava/io/PrintStream;
swap

invokevirtual java/io/PrintStream/println(I)V

return
.end method
