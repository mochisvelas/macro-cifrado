;Macro to print text
write_text macro text, white_space
        invoke StdOut, addr text
        invoke StdOut, addr white_space
endm
;------------------------------------------------
;Macro to read text
read_text macro text
        invoke StdIn, addr text,10
endm
;------------------------------------------------
.386
.model flat,stdcall

option casemap:none

INCLUDE \masm32\include\windows.inc
INCLUDE \masm32\include\masm32.inc
INCLUDE \masm32\include\masm32rt.inc
INCLUDE \masm32\include\kernel32.inc

;Used to clear screen
locate PROTO :DWORD,:DWORD

.data
        out_main        db "-- Main menu --",0
        opt1       	    db "1. Cipher a message with main method",0
        opt2            db "2. Cipher a message with the variant method",0
        opt3       	    db "3. Decipher a message",0
        opt4       	    db "4. Try break cipher",0

        out_option      db "Insert option number:",0

        in_option       db 0,0

        new_line        db 0Ah,0
        new_space       db 20h,0

.const

.code
program:

	;Main program
	t_main:

	   ;Output main prompt
        write_text out_main, new_line
        write_text opt1, new_line
        write_text opt2, new_line
        write_text opt3, new_line
        write_text opt4, new_line

        ;Ask and read main option number
        write_text out_option, new_space
        read_text in_option

        xor bx,bx
        mov bl,in_option

        ;Clear screen
        call clear_screen

        ;Jump to cipher if 1
        cmp bl,31h
        je t_cipher

        ;Jump to cipher_2 if 2
        cmp bl,32h
        je t_cipher_2

        ;Jump to decipher if 3
        cmp bl,33h
        je t_decipher

        ;Jump to break_cipher if 4
        cmp bl,34h
        je t_break_cipher
	   
        ;Exit if not an option
        jmp t_exit

        ;Call cipher and return to main
        t_cipher:
        call proc_cipher
        jmp t_main

        ;Call cipher_2 and return to main
        t_cipher_2:
        call proc_cipher_2
        jmp t_main

        ;Call decipher and return to main
        t_decipher:
        call proc_decipher
        jmp t_main

        ;Call decipher and return to main
        t_break_cipher:
        call proc_break_cipher
        jmp t_main
;------------------------------------------------
;Procedure to cipher with main method
proc_cipher proc near


	ret
proc_cipher endp
;------------------------------------------------
;Procedure to cipher with variant method
proc_cipher_2 proc near


	ret
proc_cipher_2 endp
;------------------------------------------------
;Procedure to decipher
proc_decipher proc near


	ret
proc_decipher endp
;------------------------------------------------
;Procedure to try to break cipher
proc_break_cipher proc near


	ret
proc_break_cipher endp
;------------------------------------------------
;Procedure to clear screen found in m32lib
clear_screen proc

    LOCAL hOutPut:DWORD
    LOCAL noc    :DWORD
    LOCAL cnt    :DWORD
    LOCAL sbi    :CONSOLE_SCREEN_BUFFER_INFO

    invoke GetStdHandle,STD_OUTPUT_HANDLE
    mov hOutPut, eax

    invoke GetConsoleScreenBufferInfo,hOutPut,ADDR sbi

    mov eax, sbi.dwSize

    push ax
    rol eax, 16
    mov cx, ax
    pop ax
    mul cx
    cwde
    mov cnt, eax

    invoke FillConsoleOutputCharacter,hOutPut,32,cnt,NULL,ADDR noc

    invoke locate,0,0

    ret

clear_screen endp
;------------------------------------------------

	t_exit:

	;Exit program
     invoke ExitProcess,0
END program
