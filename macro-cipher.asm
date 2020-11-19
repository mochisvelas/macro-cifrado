;Print text (Lo modifique por que encontre un Bug que cuando introducias varios caracteres en el loob infinito de la opcion imprimia de forma extrañana el menu)
write_text MACRO text
        INVOKE StdOut, ADDR text
ENDM
;------------------------------------------------
;Macro to read text
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
        out_main			db "-- Menu principal --",0
        opt1       			db "1. Cifrar mensaje con primer metodo",0
        opt2				db "2. Cifrar mensaje con segundo metodo",0
        opt3       			db "3. Descifrar mensaje con el metodo principal",0
        opt4       			db "4. Descifrar mensaje con el segundo metodo",0
        opt5       			db "5. Romper cifrado",0
        opt6       			DB "6. Salir del programa",0
        out_option			db "Inserte el numero de opcion:",0
		;Mensajes cifrado forma 1 y 2
        CifradoMensaje1		DB "El mensaje nop debe exceder dec los 100 caracteres", 0
        CifradoMensaje2		DB "Ingrese Mensaje: ", 0
        CifradoMensaje3		DB "La clave no debe de llevar espacios", 0
        CifradoMensaje4		DB "Ingrese Clave: ", 0
        CifradoMensaje5		DB "Su clave es: ", 0
        CifradoMensaje6		DB "Su Mensaje Cifrado es: ", 0
		;Mensaje (Brenner pon nombre del inciso porfa)
        out_string			DB "Ingrese el mensaje:",0
		;Variables
        in_option			DB 0,0
		new_space			DB 32d
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
		;Variables (Brenner pon nombre del inciso porfa)
		tmp 		dd 0
		;Bro une los espacios o idetenfica por que estan separados para poder llevar mejor control de la cantidad de codigo que llevamos
		letters 	db 41h,42h,43h,44h,45h,46h,47h,48h,49h,4Ah,4Bh,4Ch,4Dh,4Eh,4Fh,50h,51h,52h,53h,54h,55h,56h,57h,58h,59h,5Ah
		odds    	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		;Bro une los espacios o idetenfica por que estan separados para poder llevar mejor control de la cantidad de codigo que llevamos
		in_string 	db 500 dup('$')
		;Bro une los espacios o idetenfica por que estan separados para poder llevar mejor control de la cantidad de codigo que llevamos
		total_str 	db 0,0
		;Bro une los espacios o idetenfica por que estan separados para poder llevar mejor control de la cantidad de codigo que llevamos
		units  		db 0,0
		tens   	 	db 0,0
.DATA?
		MatizdeCifrado	dd 676 DUP(?)
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

	ret
proc_cipher endp
;------------------------------------------------
;Procedure to cipher with variant method
proc_cipher_2 proc near
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


	ret
proc_cipher_2 endp
;------------------------------------------------
;Procedure to decipher method 1
proc_decipher1 proc near


	ret
proc_decipher1 endp
;------------------------------------------------
;Procedure to decipher method 2
proc_decipher2 proc near


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

	;Print odds
	call print_num

	;Print % and new_line
	mov bl,25h
	mov edx,edi
	lea edi,tmp
	mov [edi],ebx
	mov edi,edx
	write_text tmp,new_line

	skip_letter:

	inc esi
	inc edi

	jmp l_print

	ret_print_odds:

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


InicializarMatriz PROC NEAR
	LEA ESI, MatizdeCifrado
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

TamanoMensaje PROC NEAR
	LEA ESI, Mensaje
	ForRecorrido:
		MOV AL, [ESI]
		CMP AL, 32d
		JE IfIgualEspacio
		CMP AL, 0
		JE EndFor

		INC MensajeLength
		INC ESI
		JMP ForRecorrido

		IfIgualEspacio:
			INC ESI
			JMP ForRecorrido

	EndFor:
RET
TamanoMensaje ENDP

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

RellenarClaveConMensaje PROC NEAR
	LEA ESI, Clave
	LEA EDI, Mensaje

 	MOV BL, 0
	MOV CL, 0
	MOV DL, MensajeLength
	ForRecorrer:
		MOV AL, [ESI]
		CMP AL, 0
		JE EndRecorrer

		INC ESI
		INC BL
		JMP ForRecorrer
	EndRecorrer:
	

	ForRellenar:
		MOV AL, [EDI]
		CMP AL, 32d
		JE SaltarEspacio


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

	EndRellenar:
RET
RellenarClaveConMensaje ENDP

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

CalcularPosicion:
	MOV ESI, 0
	LEA ESI, MatizdeCifrado
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


DesifrarConClaveCompleta PROC NEAR
	;en proceso
RET
DesifrarConClaveCompleta ENDP
	
	t_exit:

	;Exit program
     invoke ExitProcess,0
END program