;Print text
write_text macro text, white_space
        invoke StdOut, addr text
        invoke StdOut, addr white_space
endm
;------------------------------------------------
;Print num
write_num macro num
	add num,30h
	invoke StdOut, addr num
endm
;------------------------------------------------
;Read text
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
        out_main        db "-- Menu principal --",0
        opt1       	db "1. Cifrar mensaje con primer metodo",0
        opt2            db "2. Cifrar mensaje con segundo metodo",0
        opt3       	db "3. Descifrar mensaje",0
        opt4       	db "4. Romper cifrado",0

        out_option      db "Inserte el numero de opcion:",0
        out_string      db "Ingrese el mensaje:",0

        in_option       db 0,0

	   tmp 		dd 0

	   letters 	db 41h,42h,43h,44h,45h,46h,47h,48h,49h,4Ah,4Bh,4Ch,4Dh,4Eh,4Fh,50h,51h,52h,53h,54h,55h,56h,57h,58h,59h,5Ah
	   odds   	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	   in_string 	db 500 dup('$')

	   total_str 	db 0,0

	   units 	db 0,0
	   tens 	db 0,0

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
	
	write_text out_string,new_space
	invoke StdIn, addr in_string,499

	lea esi, in_string

	mov total_str,00h

	l_string:

		lea edi,odds

		xor ebx,ebx
		xor ax,ax
		mov bl,[esi]

		cmp bl,al
		je ret_odds

		sub bl,41h

		add edi,ebx

		mov eax,[edi]
		inc eax
		mov [edi],eax
		mov bl,[edi]

		inc total_str
		inc esi

	jmp l_string

	ret_odds:
	call print_odds

	ret
proc_break_cipher endp
;------------------------------------------------
;Procedure to print odds of letter ocurrences in in_string
print_odds proc near

	lea esi,letters
	lea edi,odds

	l_print:

	xor ebx,ebx
	mov bl,[esi]

	cmp bl,41h
	jl ret_print_odds

	mov edx,edi
	lea edi,tmp
	mov [edi],ebx
	mov edi,edx
	write_text tmp,new_space

	invoke StdOut, addr new_space

	xor bx,bx
	xor ax,ax
	mov bl,[edi]

	mov al,bl
	mov bl,64h
	mul bl
	mov bl,total_str
	div bl
	mov bl,al

	call print_num

	inc esi
	inc edi

	invoke StdOut, addr new_line

	jmp l_print

	ret_print_odds:
	read_text in_option
	call clear_screen
	call reset_odds

	ret
print_odds endp
;------------------------------------------------
;Procedure to reset odds array
reset_odds proc near
	
	lea edi,odds

	mov cl,00h

	mov bl,00h

	l_reset:

	cmp cl,1Ah
	jg ret_reset

	mov [edi],bl

	inc cl
	inc edi

	jmp l_reset

	ret_reset:

	ret
reset_odds endp
;------------------------------------------------
;Procedure to print number
print_num proc near

        ;Reset tens
        mov tens,00h

        ;If single digit printcont
        cmp bl,09h
        jle printcont


        ;If is not a single digit sub tens
        jmp subtens

        ;Count tens in result if any
        subtens:

        cmp bl,0Ah
        jl printcont

        sub bl,0Ah

        inc tens

        jmp subtens

        ;Print number
        printcont:

        ;Print tens
        write_num tens

        ;Print units
        mov units,bl
        write_num units

        ret_print:

        ret
print_num endp
;------------------------------------------------
;Procedure to clear screen found in masm32\m32lib\clearscr.asm
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
