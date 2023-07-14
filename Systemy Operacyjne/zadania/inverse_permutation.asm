global inverse_permutation
section .text

FIRST_BIT_ON equ 0x80000000
FIRT_BIT_OFF equ 0x7fffffff

correct_permutation:
        ; rdi - długość permutacji,
        ; rsi - wskaźnik na permutację.
        ; Wykorzystywane rejestry: eax, ecx, edx.
        cmp rdi, FIRT_BIT_OFF           ; Sprawdzam, permutacja jest mniejsza niż 32-bitowy INT_MAX.
        ja .false_exit
        cmp edi, 0
        je .false_exit
        mov ecx, edi
        
.loop:  
        mov eax, DWORD [rsi + rcx * 4 - 4]  ; eax := p[ecx - 1]
        and eax, FIRT_BIT_OFF           ; Ustawiam pierwszy bit eax na zero, aby odczytać wartość.
        cmp eax, edi                    ; Sprawdzam, czy wartość jest mniejsza od długości permutacji.
        jae .restore_array

        mov edx, DWORD [rsi + rax * 4]  ; Sprawdzam, czy pierszy bit p[eax] jest ustawiony na 1.
        shl edx, 1
        jc .restore_array               ; Jeśli tak, to permutacja nie jest poprawna.
        
        or [rsi + rax * 4], DWORD FIRST_BIT_ON  ; Ustawiam pierwszy bit p[eax] na 1.

        ; rcx - licznik, rdi - długość permutacji
        dec ecx
        jnz .loop

.true:
        jmp inverse_permutation_main

.restore_array:
        ; Cofanie zmian, których dokonaliśmy w tablicy.
        ; ecx - indeks, do którego udało się dojść (ale poprawianie powinniśmy zacząć od ecx+1),
        ; edi - długość permutacji,
        ; rsi - wskaźnik na permutację.
        cmp ecx, edi
        je .false_exit

.restore_loop:
        inc ecx
        mov eax, [rsi + rcx * 4 - 4]            ; eax := p[ecx - 1] 
        and eax, 0x7FFFFFFF                     ; Ustawiam pierwszy bit eax na zero, aby odczytać wartość.
        and [rsi + rax * 4], DWORD 0x7FFFFFFF   ; Ustawiam pierwszy bit p[eax] na 0.
        
        cmp ecx, edi    ; Jeśli osiągnęliśmy długość permutacji, to kończymy. 
        jb .restore_loop

.false_exit:
        xor rax, rax
        ret


inverse_permutation:
        ; rdi : size_t = długość permutacji,
        ; rsi : int* = wskaźnik na permutację.
        jmp correct_permutation

inverse_permutation_main:
        xor rcx, rcx

.for_loop:
        ; Jeśli pierwszy bit jest ustawiony na 0, to znaczy, że już odwiedziliśmy ten element.
        ; W przeciwnym wypadku przechodzimy po cyklu.
        mov eax, [rsi + rcx * 4] ; eax = p[ecx]
        shl eax, 1
        jnc .loop_condition ; Pierwszy bit równy zero, więc zwiększamy ecx.
        shr eax, 1
        mov r8d, ecx        ; Zapiszmy w r8d miejsce, od którego zaczynamy cykl.

.cycle_loop:
        ; Niezmiennik pętli: eax == p[ecx], pierwszy bit eax jest równy 0.
        mov edx, DWORD [rsi + rax * 4]  ; Zapisujemy p[eax] na przyszłość.
        mov [rsi + rax * 4], DWORD ecx  ; p[eax] = ecx
        mov ecx, eax                    ; Update ecx.
        mov eax, edx                    ; Update eax.
        and eax, FIRT_BIT_OFF           ; Naprawiamy pierwszy bit eax.
        cmp ecx, r8d                    ; Jeśli wróciliśmy do początku, to kończymy.
        jne .cycle_loop

.loop_condition:
        inc ecx
        cmp ecx, edi
        jb .for_loop

.exit:
        mov eax, 0x1
        ret