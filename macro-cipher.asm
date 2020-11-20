write_text MACRO text
        INVOKE StdOut, ADDR text
ENDM
;------------------------------------------------
;Print num
write_num MACRO num
	add num,30h
	INVOKE StdOut, ADDR num
ENDM
;------------------------------------------------
;Read text
read_text MACRO text
        INVOKE StdIn, ADDR text,10
ENDM
;------------------------------------------------
;Chance letters to capital letters
Cambiar_Minusculas MACRO Letter
	SUB Letter, 32d
ENDM
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
        out_main			db "-- Menu principal --",0
        opt1       			db "1. Cifrar mensaje con primer metodo",0
        opt2				db "2. Cifrar mensaje con segundo metodo",0
        opt3       			db "3. Descifrar mensaje con el metodo principal",0
        opt4       			db "4. Descifrar mensaje con el segundo metodo",0
        opt5       			db "5. Romper cifrado",0
        opt6       			DB "6. Salir del programa",0
        out_option			db "Inserte el numero de opcion:",0
		;Mensajes cifrado forma 1 y 2
        CifradoMensaje1		DB "El mensaje no debe exceder de los 100 caracteres", 0
        CifradoMensaje2		DB "Ingrese Mensaje: ", 0
        CifradoMensaje3		DB "La clave no debe de llevar espacios", 0
        CifradoMensaje4		DB "Ingrese Clave: ", 0
        CifradoMensaje5		DB "Su clave es: ", 0
        CifradoMensaje6		DB "Su Mensaje Cifrado es: ", 0
		;Mensajes desifrado 1
        DesifradoMensaje1		DB "El mensaje nop debe exceder dec los 100 caracteres ymmword nop debe tener espacios ni caracteres diferentes aaa letras", 0
        DesifradoMensaje2		DB "Ingrese Mensaje Cifrado: ", 0
        DesifradoMensaje3		DB "La clave no debe de llevar espacios", 0
        DesifradoMensaje4		DB "Ingrese Clave: ", 0
        DesifradoMensaje5		DB "Su clave es: ", 0
        DesifradoMensaje6		DB "Su Mensaje Desifrado es: ", 0
		;Mensaje Intentar romper cifrado
        out_string			DB "Ingrese el mensaje:",0
		;Variables
        in_option			DB 0,0
		;Variables Cifrado forma 1
        MensajeEncriptado	DB 100 DUP(?)
        Mensaje				DB 100 DUP(?)
        Clave				DB 100 DUP(?)
		Posicion			DD 0
		ContadorRecorrido	DB 0
		ContadorEspacios	DB 0
		ContadorFila		DB 65,0
		ContadorColumna		DB 91,0
		LETRA_AUX			DB 0,0
		MensajeLength		DB 0
		ClaveLength			DB 0
		;Variables Tratar de romper cifrado
		tmp 		DD 0
		letters 	DB 41h,42h,43h,44h,45h,46h,47h,48h,49h,4Ah,4Bh,4Ch,4Dh,4Eh,4Fh,50h,51h,52h,53h,54h,55h,56h,57h,58h,59h,5Ah,24h
		odds    	DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		percent   DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		in_string 	DB 500 dup('$')
		total_str 	DB 0,0
		units  		DB 0,0
		tens   	 	DB 0,0
		hundreds 		DB 0,0
		remainder 	DB 0,0
		whole 		DB 0,0
		cursor 		DD 0,0
		;Variables para nueva line y espacio
		new_space		DB 20h,0
		new_line 		DB 0Ah,0
.DATA?
		MatrizdeCifrado	dd 676 DUP(?)
.const

.code
program:
	CALL InicializarMatriz
	;Main program
	t_main:
	;Output main prompt
        write_text out_main
		print chr$(10, 13)
        write_text opt1
		print chr$(10, 13)
        write_text opt2
		print chr$(10, 13)
        write_text opt3
		print chr$(10, 13)
        write_text opt4
		print chr$(10, 13)
        write_text opt5
		print chr$(10, 13)
        write_text opt6

    ;Ask and read main option number
		print chr$(10, 13)
        write_text out_option
        read_text in_option

        xor bx,bx
        mov bl,in_option

        ;Clear screen
        call clear_screen

        ;Jump to cipher if 1
        CMP bl,31h
        JE t_cipher

        ;Jump to cipher_2 if 2
        CMP bl,32h
        JE t_cipher_2

        ;Jump to decipher if 3
        CMP bl,33h
        JE t_decipher1

        ;Jump to decipher if 4
        CMP bl,34h
        JE t_decipher2

        ;Jump to break_cipher if 5
        CMP bl,35h
        JE t_break_cipher
	   
        ;Exit if 6 option
        CMP bl,36h
        JE t_exit
		JMP t_main	

        ;Call cipher and return to main
        t_cipher:
        call proc_cipher
        jmp t_main

        ;Call cipher_2 and return to main
        t_cipher_2:
        call proc_cipher_2
        jmp t_main

        ;Call decipher and return to main
        t_decipher1:
        call proc_decipher1
        jmp t_main

        ;Call decipher 2 and return to main
        t_decipher2:
        call proc_decipher2
        jmp t_main

        ;Call decipher and return to main
        t_break_cipher:
        call proc_break_cipher
        jmp t_main
;------------------------------------------------
;Procedure to cipher with main method
proc_cipher proc near
	LEA ESI, Mensaje
	LEA EDI, Clave

	MOV BL, 0
	LOOPVACIO1:
		CMP BL, 100
		JE LOOPVACIOEND1
		
		MOV AL, 0
		MOV [ESI], AL
		MOV [EDI], AL

		INC BL
		INC ESI
		INC EDI
		JMP LOOPVACIO1
	LOOPVACIOEND1:
	
	LEA ESI, MensajeEncriptado
	
	MOV BL, 0
	LOOPVACIO2:
		CMP BL, 100
		JE LOOPVACIOEND2
		
		MOV AL, 0
		MOV [ESI], AL

		INC BL
		INC ESI
		JMP LOOPVACIO2
	LOOPVACIOEND2:

	write_text CifradoMensaje1
	print chr$(10, 13)
	write_text CifradoMensaje2
	INVOKE	StdIn, ADDR Mensaje, 102
	print chr$(10, 13)

	write_text CifradoMensaje3
	print chr$(10, 13)
	write_text CifradoMensaje4
	INVOKE	StdIn, ADDR Clave, 102
	print chr$(10, 13)

	MOV MensajeLength, 0
	MOV ClaveLength, 0
	CALL TamanoMensaje	
	CALL TamanoClave

	MOV AL, ClaveLength
	CMP AL, MensajeLength
	JNE RellenarClave
NoRellenar:
	
	CALL Cifrar	
	write_text CifradoMensaje6
	write_text MensajeEncriptado
	print chr$(10, 13)

	JMP TerminarProceso
	RellenarClave:
		CALL RellenarClaveConClave	
		write_text CifradoMensaje5
		write_text Clave
		print chr$(10, 13)
		JMP NoRellenar
	TerminarProceso:

	call ResetearClave

	ret
proc_cipher endp
;------------------------------------------------
;Procedure to cipher with variant method
proc_cipher_2 proc near
	LEA ESI, Mensaje
	LEA EDI, Clave

	MOV BL, 0
	LOOPVACIO1:
		CMP BL, 100
		JE LOOPVACIOEND1
		
		MOV AL, 0
		MOV [ESI], AL
		MOV [EDI], AL

		INC BL
		INC ESI
		INC EDI
		JMP LOOPVACIO1
	LOOPVACIOEND1:
	
	LEA ESI, MensajeEncriptado
	
	MOV BL, 0
	LOOPVACIO2:
		CMP BL, 100
		JE LOOPVACIOEND2
		
		MOV AL, 0
		MOV [ESI], AL

		INC BL
		INC ESI
		JMP LOOPVACIO2
	LOOPVACIOEND2:

	write_text CifradoMensaje1
	print chr$(10, 13)
	write_text CifradoMensaje2
	INVOKE	StdIn, ADDR Mensaje, 102
	print chr$(10, 13)

	write_text CifradoMensaje3
	print chr$(10, 13)
	write_text CifradoMensaje4
	INVOKE	StdIn, ADDR Clave, 102
	print chr$(10, 13)

	MOV MensajeLength, 0
	MOV ClaveLength, 0
	CALL TamanoMensaje	
	CALL TamanoClave

	MOV AL, ClaveLength
	CMP AL, MensajeLength
	JNE RellenarClave
NoRellenar:
	
	CALL Cifrar	
	write_text CifradoMensaje6
	write_text MensajeEncriptado
	print chr$(10, 13)

	JMP TerminarProceso
	RellenarClave:
	CALL RellenarClaveConMensaje		
	write_text CifradoMensaje5
	write_text Clave
	print chr$(10, 13)
	JMP NoRellenar
	TerminarProceso:

	call ResetearClave


	ret
proc_cipher_2 endp
;------------------------------------------------
;Procedure to decipher method 1
proc_decipher1 proc near
	LEA ESI, Mensaje
	LEA EDI, Clave

	MOV BL, 0
	LOOPVACIO1:
		CMP BL, 100
		JE LOOPVACIOEND1
		
		MOV AL, 0
		MOV [ESI], AL
		MOV [EDI], AL

		INC BL
		INC ESI
		INC EDI
		JMP LOOPVACIO1
	LOOPVACIOEND1:
	
	LEA ESI, MensajeEncriptado
	
	MOV BL, 0
	LOOPVACIO2:
		CMP BL, 100
		JE LOOPVACIOEND2
		
		MOV AL, 0
		MOV [ESI], AL

		INC BL
		INC ESI
		JMP LOOPVACIO2
	LOOPVACIOEND2:

	write_text DesifradoMensaje1
	print chr$(10, 13)
	write_text DesifradoMensaje2
	INVOKE	StdIn, ADDR MensajeEncriptado, 102
	print chr$(10, 13)

	write_text DesifradoMensaje3
	print chr$(10, 13)
	write_text DesifradoMensaje4
	INVOKE	StdIn, ADDR Clave, 102
	print chr$(10, 13)

	MOV MensajeLength, 0
	MOV ClaveLength, 0
	CALL TamanoCifrado	
	CALL TamanoClave
	
	MOV AL, ClaveLength
	CMP AL, MensajeLength
	JNE RellenarClave
NoRellenar:
	
	CALL DesifrarConClaveCompleta	
	write_text DesifradoMensaje6
	write_text MensajeEncriptado
	print chr$(10, 13)

	JMP TerminarProceso
	RellenarClave:
		CALL RellenarClaveConClave	
		write_text DesifradoMensaje5
		write_text Clave
		print chr$(10, 13)
		JMP NoRellenar
	TerminarProceso:

	ret
proc_decipher1 endp
;------------------------------------------------
;Procedure to decipher method 2
proc_decipher2 proc near
	LEA ESI, Mensaje
	LEA EDI, Clave

	MOV BL, 0
	LOOPVACIO1:
		CMP BL, 100
		JE LOOPVACIOEND1
		
		MOV AL, 0
		MOV [ESI], AL
		MOV [EDI], AL

		INC BL
		INC ESI
		INC EDI
		JMP LOOPVACIO1
	LOOPVACIOEND1:
	
	LEA ESI, MensajeEncriptado
	
	MOV BL, 0
	LOOPVACIO2:
		CMP BL, 100
		JE LOOPVACIOEND2
		
		MOV AL, 0
		MOV [ESI], AL

		INC BL
		INC ESI
		JMP LOOPVACIO2
	LOOPVACIOEND2:

	write_text DesifradoMensaje1
	print chr$(10, 13)
	write_text DesifradoMensaje2
	INVOKE	StdIn, ADDR MensajeEncriptado, 102
	print chr$(10, 13)

	write_text DesifradoMensaje3
	print chr$(10, 13)
	write_text DesifradoMensaje4
	INVOKE	StdIn, ADDR Clave, 102
	print chr$(10, 13)

	MOV MensajeLength, 0
	MOV ClaveLength, 0
	CALL TamanoCifrado	
	CALL TamanoClave
	
	CALL DesifrarConClaveParcial	

	write_text DesifradoMensaje5
	write_text Clave
	print chr$(10, 13)

	write_text DesifradoMensaje6
	write_text MensajeEncriptado
	print chr$(10, 13)
	ret
proc_decipher2 endp
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

		cmp bl,41h
		jl not_letter

		cmp bl,5Ah
		jg not_letter

		sub bl,41h

		add edi,ebx

		mov eax,[edi]
		inc eax
		mov [edi],eax
		mov bl,[edi]

		inc total_str

		not_letter:

		inc esi

	jmp l_string

	ret_odds:
	mov bl,total_str
	cmp bl,00h
	je empty_str

	call print_odds

	empty_str:

	read_text in_option
	call clear_screen

	ret
proc_break_cipher endp
;------------------------------------------------
;Procedure to print odds of letter ocurrences in in_string
print_odds proc near

	lea esi, percent
	mov cursor,esi
	lea esi,letters
	lea edi,odds

	l_print:

	xor ebx,ebx

	mov bl,[edi]

	mov cl,00h
	cmp bl,cl
	je skip_letter

	mov bl,[esi]

	cmp bl,41h
	jl ret_print_odds

	mov edx,edi
	lea edi,tmp
	mov [edi],ebx
	mov edi,edx
	write_text tmp

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
	mov whole,al
	mov remainder,ah

	;Print odds
	call print_num

	;Print .
	mov bl,2Eh
	mov edx,edi
	lea edi,tmp
	mov [edi],ebx
	mov edi,edx
	write_text tmp

	xor bx,bx
	mov bl,remainder

	;Print remainder
	call print_num

	;Print % and new_line
	mov bl,25h
	mov edx,edi
	lea edi,tmp
	mov [edi],ebx
	mov edi,edx
	write_text tmp
	invoke StdOut, addr new_line

	skip_letter:

	mov bl,whole
	mov [edi],bl

	mov edx,esi
	mov esi,cursor
	mov bl,remainder
	mov [esi],bl
	mov esi,edx

	inc esi
	inc edi
	inc cursor

	jmp l_print

	ret_print_odds:

	call match_letter

	call reset_odds

	ret
print_odds endp
;------------------------------------------------
;Procedure to match letters according to their odds
match_letter proc near

	lea esi,in_string
	mov cursor,esi

	l_match_letter:

	lea esi,odds
	lea edi,percent

	xor ebx,ebx
	mov al,00h
	mov edx,esi
	mov esi,cursor
	mov bl,[esi]
	mov esi,edx

	cmp bl,al
	je finalize_match

	sub bl,41h
	add esi,ebx
	add edi,ebx

	mov bl,[esi]
	mov whole,bl

	mov bl,[edi]
	mov remainder,bl

	cmp whole,01h
	jl add_jfzk
	
	je add_ybhvg

	cmp whole,02h
	je add_mpq

	cmp whole,03h
	je add_tc

	cmp whole,04h
	je add_u

	cmp whole,05h
	je add_ild

	cmp whole,06h
	je add_nr

	cmp whole,08h
	jle add_s

	cmp whole,0Bh
	jle add_o
	
	cmp whole,0Dh
	jle add_a

	cmp whole,0Eh
	je add_e
	jg add_unknown

	;JFZK
	add_jfzk:

	cmp remainder,01h
	jl add_k
	
	cmp remainder,04h
	jl add_z

	cmp remainder,05h
	jl add_f

	;J
	mov bl,4Ah
	jmp ret_match

	;K
	add_k:
	mov bl,4Bh
	jmp ret_match

	;Z
	add_z:
	mov bl,5Ah
	jmp ret_match

	;F
	add_f:
	mov bl,46h
	jmp ret_match

	;YBHVG
	add_ybhvg:

	cmp remainder,01h
	jl add_g

	je add_v

	cmp remainder,02h
	je add_h

	cmp remainder,05h
	jl add_y

	;B
	mov bl,42h
	jmp ret_match

	;G
	add_g:
	mov bl,47h
	jmp ret_match

	;V
	add_v:
	mov bl,56h
	jmp ret_match

	;H
	add_h:
	mov bl,48h
	jmp ret_match

	;Y
	add_y:
	mov bl,59h
	jmp ret_match

	;MPQ
	add_mpq:

	cmp remainder,02h
	jl add_q

	cmp remainder,07h
	jl add_p

	;M
	mov bl,4Dh
	jmp ret_match

	;Q
	add_q:
	mov bl,51h
	jmp ret_match

	;P
	add_p:
	mov bl,50h
	jmp ret_match

	;TC
	add_tc:

	cmp remainder,06h
	jl add_c

	;T
	mov bl,54h
	jmp ret_match

	;C
	add_c:
	mov bl,43h
	jmp ret_match

	;U
	add_u:
	mov bl,55h
	jmp ret_match

	;ILD
	add_ild:

	cmp remainder,03h
	jl add_d

	cmp remainder,04h
	jl add_l

	;I
	mov bl,49h
	jmp ret_match

	;D
	add_d:
	mov bl,44h
	jmp ret_match

	;L
	add_l:
	mov bl,4Ch
	jmp ret_match

	;NR
	add_nr:

	cmp remainder,02h
	jl add_r

	;N
	mov bl,4Eh
	jmp ret_match

	;R
	add_r:
	mov bl,52h
	jmp ret_match

	;S
	add_s:
	mov bl,53h
	jmp ret_match

	;O
	add_o:
	mov bl,4Fh
	jmp ret_match

	;A
	add_a:
	mov bl,41h
	jmp ret_match

	;E
	add_e:
	mov bl,45h
	jmp ret_match

	;Add ? to the possible_message
	add_unknown:

	mov bl,3Fh

	ret_match:

	mov units,bl
	write_text units

	inc cursor

	jmp l_match_letter

	finalize_match:
	
	ret
match_letter endp
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

        ;Reset conts
        mov tens,00h
	   mov hundreds,00h

        ;If single digit printcont
        cmp bx,09h
        jle printcont

	   ;Count hundreds
	   subhundreds:

        cmp bx,64h
        jl subtens

        sub bx,64h

        inc hundreds

	   jmp subhundreds

        ;Count tens
        subtens:

        cmp bx,0Ah
        jl printcont

        sub bx,0Ah

        inc tens

        jmp subtens

        ;Print number
        printcont:

	   xor ax,ax
	   
	   cmp hundreds,al
	   je skip_hundreds

        ;Print hundreds
        write_num hundreds

	   skip_hundreds:

	   xor ax,ax

	   cmp tens,al
	   je skip_tens

	   mov al,tens
	   mov remainder,al

        ;Print tens
        write_num tens

	   skip_tens:
	   
        ;Print units
        mov units,bl

	   xor ax,ax
	   cmp units,al
	   je ret_print

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
ResetearClave proc near

	lea esi, Clave
	xor edx,edx

	l_reset_clave:
		
		mov dl,[esi]
		cmp dl,00h
		je ret_reset_clave

		mov dl,00h

		mov [esi],dl

		inc esi

	jmp l_reset_clave

	ret_reset_clave:

	ret
ResetearClave endp
;------------------------------------------------
InicializarMatriz PROC NEAR
	LEA ESI, MatrizdeCifrado
	MOV AL, ContadorFila ;AL se usara como contador para escribir la letra en el Vector

	FOR1:
		FOR2:
			MOV [ESI], AL
            INC ESI
            INC AL

            CMP ContadorColumna, 5Bh
            JZ IF_Menor_Z
            JMP IF_MENOR_LETRA_INICIO
            IF_Menor_Z:
				CMP AL, ContadorColumna
                JNE FOR2
				SUB ContadorColumna, 25d
				JMP ENDINGFOR1
			IF_MENOR_LETRA_INICIO:
				CMP AL, 5Bh
				JNE ELSE_MAYOR_Z
				IF_MAYOR_Z:
					MOV AL, 65d
					JMP FOR2
				ELSE_MAYOR_Z:
					CMP AL, ContadorColumna
					JNE FOR2
					INC ContadorColumna
	ENDINGFOR1:
		CMP ContadorFila, 5Ah
		JZ ENDCICLOS

		INC ContadorFila
		MOV AL, ContadorFila

        JMP FOR1
	ENDCICLOS:
RET
InicializarMatriz endp
;------------------------------------------------
TamanoMensaje PROC NEAR
	LEA ESI, Mensaje
	ForRecorrido:
		MOV AL, [ESI]
		CMP AL, 0
		JE EndFor
		CMP AL, 65d
		JL IfIgualEspacio

		INC MensajeLength
		INC ESI
		JMP ForRecorrido

		IfIgualEspacio:
			INC ESI
			JMP ForRecorrido

	EndFor:
RET
TamanoMensaje ENDP
;------------------------------------------------
TamanoClave PROC NEAR
	LEA ESI, Clave
	ForRecorrer:
		MOV AL, [ESI]
		CMP AL, 0
		JE EndFor

		INC ESI
		INC ClaveLength

		JMP ForRecorrer

	EndFor:

RET
TamanoClave ENDP
TamanoCifrado PROC NEAR
	LEA ESI, MensajeEncriptado
	ForRecorrido:
		MOV AL, [ESI]
		CMP AL, 0
		JE EndFor
		CMP AL, 65d
		JL IfIgualEspacio

		INC MensajeLength
		INC ESI
		JMP ForRecorrido

		IfIgualEspacio:
			INC ESI
			JMP ForRecorrido

	EndFor:
RET
TamanoCifrado ENDP

;------------------------------------------------
RellenarClaveConClave PROC NEAR
	LEA ESI, Clave
	LEA EDI, Clave

 	MOV BL, 0
	MOV CL, 0
	MOV DL, MensajeLength
	ForRecorrer:
		MOV AL, [ESI]
		MOV LETRA_AUX, AL
		CMP LETRA_AUX, 0
		JE EndRecorrer

		CMP LETRA_AUX, 91
		JA CambiarMinusculas

		INC ESI
		INC BL
		JMP ForRecorrer
		CambiarMinusculas:
			Cambiar_Minusculas LETRA_AUX
			MOV AL, LETRA_AUX
			MOV [ESI], AL
			
			INC ESI
			INC BL
			JMP ForRecorrer
	EndRecorrer:

	ForRellenar:
		CMP CL, ClaveLength
		JE ReiniciarPuntero

		MOV AL, [EDI]
		MOV [ESI], AL
		INC BL
		INC CL

		INC EDI
		INC ESI

		CMP BL, DL
		JL ForRellenar
		JMP EndRellenar

		ReiniciarPuntero:
			LEA EDI, Clave
			MOV CL, 0
			JMP ForRellenar

	EndRellenar:

RET
RellenarClaveConClave ENDP
;------------------------------------------------
RellenarClaveConMensaje PROC NEAR
	LEA ESI, Clave
	LEA EDI, Mensaje

 	MOV BL, 0
	MOV CL, 0
	MOV DL, MensajeLength
	ForRecorrer:
		MOV AL, [ESI]
		MOV LETRA_AUX, AL
		CMP LETRA_AUX, 0
		JE EndRecorrer

		CMP LETRA_AUX, 91
		JA CambiarMinusculas

		INC ESI
		INC BL
		JMP ForRecorrer
		CambiarMinusculas:
			Cambiar_Minusculas LETRA_AUX
			MOV AL, LETRA_AUX
			MOV [ESI], AL
			
			INC ESI
			INC BL
			JMP ForRecorrer
	EndRecorrer:
	
	ForRellenar:
		MOV AL, [EDI]
		MOV LETRA_AUX, AL
		CMP AL, 32d
		JE SaltarEspacio

		CMP AL, 91
		JA CambiarMinusculas2
		Continuar:

		MOV [ESI], AL
		INC BL
		INC CL

		INC EDI
		INC ESI

		CMP BL, DL
		JL ForRellenar
		JMP EndRellenar

		SaltarEspacio:
			INC EDI
			JMP ForRellenar

		CambiarMinusculas2:
			Cambiar_Minusculas LETRA_AUX
			MOV AL, LETRA_AUX
			JMP Continuar
	EndRellenar:
RET
RellenarClaveConMensaje ENDP
;------------------------------------------------
Cifrar PROC NEAR
		MOV ContadorRecorrido, 0
		MOV ContadorEspacios, 0
	ForRecorrer:
		LEA ESI, Mensaje
		LEA EDI, Clave

		MOV AL, ContadorRecorrido
		CMP AL, MensajeLength
		JE Terminar

		MOV EAX, 0
		MOV AL, ContadorRecorrido

		ADD ESI, EAX
		ADD EDI, EAX

		MOV AL, [ESI]
		MOV BL, [EDI]

		MOV DL, 0

		cmp ContadorEspacios, 0
		JA Incrementar
		CMP AL, 32d
		JNE RestarParaSaltar

		SumadorEspacios:
			INC ContadorEspacios

		Incrementar:
			INC DL
			INC ESI
			MOV AL, [ESI]
			CMP DL, ContadorEspacios
			JNE Incrementar
			CMP AL, 32d
			JE SumadorEspacios

		RestarParaSaltar:
			CMP AL, 91
			JA CambiarMinusculas
		Cambiada:
			SUB AL, 65d
			SUB BL, 65d

		JMP CalcularPosicion
		Continuar:
		MOV LETRA_AUX, AL
		LEA ESI, MensajeEncriptado
		
		MOV EAX, 0
		MOV AL, ContadorRecorrido

		ADD ESI, EAX

		MOV AL, LETRA_AUX
		MOV [ESI], AL

		INC ContadorRecorrido
		JMP ForRecorrer
	EndFor:
	JMP Terminar
CambiarMinusculas:	
	MOV LETRA_AUX, AL
	Cambiar_Minusculas LETRA_AUX
	MOV AL, LETRA_AUX
	JMP Cambiada

CalcularPosicion:
	MOV ESI, 0
	LEA ESI, MatrizdeCifrado
	MOV DL, 26d
	MUL DL
	MOV Posicion, EAX
	MOV EAX, 0
	MOV AL, BL
	ADD Posicion, EAX
	MOV EAX, Posicion
	ADD ESI, EAX

	MOV AL, [ESI]
	JMP Continuar
Terminar:
RET
Cifrar ENDP
;------------------------------------------------
DesifrarConClaveCompleta PROC NEAR
	MOV ContadorRecorrido, 0
	For1:
		MOV DL, ContadorRecorrido
		CMP DL, MensajeLength
		JE ForEnd1

		LEA ESI, MensajeEncriptado
		LEA EDI, Clave
	
		MOV EAX, 0
		MOV AL, ContadorRecorrido

		ADD ESI, EAX
		ADD EDI, EAX

		MOV AL, [ESI]
		MOV BL, [EDI]

		CMP AL, BL
		JL RestarBL
		CMP AL, BL
		JA RestarAL
		CMP AL, BL
		JE RestarAL
	Continuar:
		LEA EDI, Mensaje
	
		MOV EAX, 0
		MOV AL, ContadorRecorrido

		ADD EDI, EAX

		MOV AL, LETRA_AUX
		MOV [ESI], AL

		INC ContadorRecorrido
		JMP For1

		RestarBL:
			MOV CL, 91
			SUB CL, BL
			SUB AL, 65
			ADD AL, CL
			ADD AL, 65
			MOV LETRA_AUX, AL
			JMP Continuar

		RestarAL:
			SUB AL, BL
			ADD AL, 65
			MOV LETRA_AUX, AL
			JMP Continuar
	ForEnd1:
RET
DesifrarConClaveCompleta ENDP

;------------------------------------------------
DesifrarConClaveParcial PROC NEAR
	MOV ContadorRecorrido, 0
	For1:
		MOV DL, ContadorRecorrido
		CMP DL, MensajeLength
		JE ForEnd1

		LEA ESI, MensajeEncriptado
		LEA EDI, Clave
	
		MOV EAX, 0
		MOV AL, ContadorRecorrido

		ADD ESI, EAX
		ADD EDI, EAX

		MOV AL, [ESI]
		MOV BL, [EDI]
		
		CMP AL, 91
		JA CambiarMinusculasAL
	RevisarMinusculas:
		
		CMP BL, 91
		JA CambiarMinusculasBL
	LetrasCambiadas:

		MOV [ESI], AL
		MOV [EDI], BL

		CMP AL, BL
		JL RestarBL
		CMP AL, BL
		JA RestarAL
		CMP AL, BL
		JE RestarAL
	Continuar:
		LEA EDI, Mensaje
	
		MOV EAX, 0
		MOV AL, ContadorRecorrido

		ADD EDI, EAX

		MOV AL, LETRA_AUX
		MOV [ESI], AL

		INC ContadorRecorrido
		MOV AL, ClaveLength
		CMP AL, MensajeLength
		JL Rellenar

		JMP For1

		RestarBL:
			MOV CL, 91
			SUB CL, BL
			SUB AL, 65
			ADD AL, CL
			ADD AL, 65
			MOV LETRA_AUX, AL
			JMP Continuar

		RestarAL:
			SUB AL, BL
			ADD AL, 65
			MOV LETRA_AUX, AL
			JMP Continuar

		Rellenar:
			LEA EDI, Clave
			MOV EAX, 0
			MOV AL, ClaveLength

			ADD EDI, EAX

			MOV BL, LETRA_AUX
			MOV [EDI], BL

			INC ClaveLength
			JMP For1

		CambiarMinusculasAL:
			MOV LETRA_AUX, AL
			Cambiar_Minusculas LETRA_AUX
			MOV AL, LETRA_AUX
			JMP RevisarMinusculas
			
		CambiarMinusculasBL:
			MOV LETRA_AUX, BL
			Cambiar_Minusculas LETRA_AUX
			MOV BL, LETRA_AUX
			JMP RevisarMinusculas
	ForEnd1:
RET
DesifrarConClaveParcial ENDP
	t_exit:

	;Exit program
     invoke ExitProcess,0
END program
