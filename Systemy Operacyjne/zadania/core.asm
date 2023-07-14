global core
extern put_value
extern get_value

; Calls a function, and aligns the stack pointer to 16 bytes
; safe_call <function>
; Uses:
;   [rbx] - temporary register
%macro safe_call 1
    mov rbx, rsp
    and rbx, 0xF
    sub rsp, rbx

    call %1
    add rsp, rbx
%endmacro

section .data

align 8
waiting_for:
    times N dq N

section .bss

value: resq N


section .text

core:
    ; [rdi] - pointer to the core number
    ; [rsi] - pointer to the string
    push rbx
    push r12
    push r13
    push r15

    mov r15, rsp    ; [r15] - pointer to the base of the stack
    mov r12, rdi    ; [r12] - pointer to the core number
    mov r13, rsi    ; [r13] - pointer to the string

core_switch_start:
    xor eax, eax
    mov al, byte [r13]


; ================== Functions that don't require a value on the stack ==============

end_of_string:
    cmp al, 0
    jne push_core_number

    pop rax         ; pop the return value
    mov rsp, r15

    pop r15
    pop r13
    pop r12
    pop rbx

    ret


push_core_number:   ; Push the core number to the stack
    cmp al, 'n'
    jne push_digit

    push r12
    jmp add_values


push_digit:         ; If [al] is a digit, push it to the stack
    cmp al, '0'
    jl exec_get_value 
    cmp al, '9'
    ja exec_get_value

    sub al, byte '0'
    push rax
    jmp add_values


exec_get_value:     ; Execute get_value function and push the result to the stack.
    cmp al, 'G'
    jne needs_value_functions
    
    mov rdi, r12

    safe_call get_value

    push rax
    jmp add_values


needs_value_functions:
    pop rcx     ; [rcx] - value to be used by all below functions

; =============== Funcions that require a value on the stack ==============

add_values:         ; Take two top values from the stack, add them, and push the result to the stack.
    cmp al, '+'
    jne mult_values
    add [rsp], rcx
    

mult_values:        ; Take two top values from the stack, multiply them, and push the result to the stack.
    cmp al, '*'
    jne neg_value

    imul rcx, [rsp]
    mov [rsp], rcx 


neg_value:          ; Take the top value from the stack, negate it, and push the result to the stack.
    cmp al, '-'
    jne exec_put_value

    neg rcx
    push rcx


exec_put_value:     ; Take the top value from the stack and pass it to the put_value function as an argument.
    cmp al, 'P'
    jne jump_to

    mov rdi, r12
    mov rsi, rcx
    
    safe_call put_value


jump_to:            ; Take the top value from the stack, and moves the pointer to the string by that value (treated as U2).
    cmp al, 'B'
    jne duplicate

    add rcx, r13
    cmp qword [rsp], 0
    cmovne r13, rcx

duplicate:          ; Duplicate the top value from the stack.
    cmp al, 'D'
    jne swap_values

    push rcx
    push rcx


swap_values:        ; Swap the top two values from the stack.
    cmp al, 'E'
    jne sync

    xchg [rsp], rcx
    push rcx


sync:               ; Synchronize with another core. First value is the core number to sync with, second value is the value to swap.
    cmp al, 'S'
    jne core_switch_end

;  [r12] - core number of the current core
;  [rcx] - core number to sync with
;  [rdx] - value to swap
    pop rdx
    lea rdi, [rel waiting_for]      ; [rdi] - pointer to the waiting_for array
    lea rsi, [rel value]            ; [rsi] - pointer to the value array

    mov [rsi + r12 * 8], rdx        ; give the value of our stack

    mov [rdi + r12 * 8], rcx        ; signal that we set our value

.spin_lock:                         ; wait for the other core to set his value
    mov rax, [rdi + rcx * 8]
    cmp rax, r12
    jne .spin_lock

    mov rdx, [rsi + rcx * 8]        ; get the value of the other core
    mov qword [rdi + rcx * 8], N    ; set his waiting_for value to N (meaning we're done)

.spin_lock_confirm:                 ; wait for the other core to confirm that he's done
    mov rax, [rdi + r12 * 8]
    cmp rax, N
    jne .spin_lock_confirm

    push rdx                        ; push the value of the other core to our stack


core_switch_end:
    inc r13
    jmp core_switch_start