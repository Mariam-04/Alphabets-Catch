[org 0x0100]

jmp start

tickcount: dw 0
tt: dw 0
oldtime:dd 0
flagl: dw 0
flagr: dw 0
oldkb: dd 0
boxloc: dw 3920
frog: dw 1
randalpha: dw 0
alphloc: dw 0,0,0,0,0
timecount: dw 0
score: dw 0
miss:dw 0
scorestr: db 'score: ','0'
missstr: db 'missed: ','0'
terminate:dw 0

rand: dw 0
randnum: dw 0

msg: db 'GAME OVER','0'
printstr:

push bp
mov bp,sp
pusha
push es
mov si,[bp+6]
mov ax,0xB800
mov es, ax
mov di,[bp+4]
mov ah,0x07
loop3: mov al,[si]
	inc si
	mov [es:di],ax
	add di,2
	cmp byte [si],'0'
	jne loop3
pop es
popa
pop bp
ret 4

; taking n as parameter, generate random number from 0 to n nad return in the stack
randG:
   push bp
   mov bp, sp
   pusha
   cmp word [rand], 0
   jne next

  MOV     AH, 00h   ; interrupt to get system timer in CX:DX 
  INT     1AH
  inc word [rand]
  mov     [randnum], dx
  jmp next1

  next:
  mov     ax, 25173          ; LCG Multiplier
  mul     word  [randnum]     ; DX:AX = LCG multiplier * seed
  add     ax, 13849          ; Add LCG increment value
  ; Modulo 65536, AX = (multiplier*seed+increment) mod 65536
  mov     [randnum], ax          ; Update seed = return value

 next1:xor dx, dx
 mov ax, [randnum]
 mov cx, [bp+4]
 inc cx
 div cx
 
 mov [bp+6], dx
 popa
 pop bp
 ret 2


printnum: push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+6] ; load number in ax
mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
nextdigit: mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit ; if no divide it again
mov di, [bp+4] ; point di to 70th column
nextpos: pop dx ; remove a digit from the stack
mov dh, 0x07 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 4


printscore:
pusha

mov ax, 0xB800
mov es, ax

mov ah,0x07
mov al,'s'
mov di,66
mov word [es:di],ax
mov al,'c'
add di,2
mov word [es:di],ax
mov al,'o'
add di,2
mov word [es:di],ax
mov al,'r'
add di,2
mov word [es:di],ax
mov al,'e'
add di,2
mov word [es:di],ax
mov al,':'
add di,2
mov word [es:di],ax


popa
ret

printmissed:
    pusha
    
    ; Set up segment registers
    mov ax, 0xB800
    mov es, ax

    ; Set character attributes (color)
    mov ah, 0x07

    ; Print each character in the string 'missed: '
    mov al, 'm'
    mov di, 124
    mov word [es:di], ax

    mov al, 'i'
    add di, 2
    mov word [es:di], ax

    mov al, 's'
    add di, 2
    mov word [es:di], ax

    mov al, 's'
    add di, 2
    mov word [es:di], ax

    mov al, 'e'
    add di, 2
    mov word [es:di], ax

    mov al, 'd'
    add di, 2
    mov word [es:di], ax

    mov al, ':'
    add di, 2
    mov word [es:di], ax

    ; Restore registers and return
    popa
    ret

clearscr:
    push ax
    push es
    push di
    push cx

	mov ax,0xB800
	mov es,ax
	mov di,0
    mov cx,2000
    mov ax,0x0720
    rep stosw 
    
    pop cx
    pop di
    pop es
    pop ax

	ret
	
clearscr1:
    push ax
    push es
    push di
    push cx

	mov ax,0xB800
	mov es,ax
	mov di,0
    mov cx,1920
    mov ax,0x0720
    rep stosw 
    
    pop cx
    pop di
    pop es
    pop ax

	ret
	
kbisr: pusha 
	in al,0x60
	
	cmp al,0x4B   ;if left array key pressed, flagleft is set
	jne nextcmp
	
	mov word [cs:flagl],1
	jmp exit
	
	nextcmp:
	cmp al,0x4D   ;if right array key pressed, flagright is set
	jne nextcmp2
	
	mov word [cs:flagr],1
	jmp exit
	
	nextcmp2:
	cmp al,0xCB   ;if left shift key released, flagl reset
	jne nextcmp3
	
	mov word [cs:flagl],0 
	jmp exit
	
	nextcmp3: 
	cmp al, 0xCD   ;if right shift key released, flagr reset
	jne nomatch
	
	mov word [cs: flagr],0
	jmp exit
	
	nomatch:popa
	jmp far[cs:oldkb]
	
	exit:
	mov al,0x20
	out 0x20,al
	popa
	iret
	
	
	
timer:
	pusha
	push es
	
	
	cmp word [cs:frog],1
	jne checkleft
	mov di,3920
	;add di,160
	mov ax, 0xB800
	mov es,ax
	mov word [es:di],0x07DC
	mov word [cs:frog],0
	mov word [cs:miss],0
	
	
	
	;check if flagl is set. if so, check is location-2 is greater than 3840. if so,location of box dec by 2, and print
	checkleft:
	cmp word [cs:flagl], 1
	jne checkright

    sub word [cs:boxloc],2
	cmp word [cs:boxloc],3840
	jae printleft	;if below we will move box to 4000
	
	mov di,[cs:boxloc]
	mov ax, 0xB800
	mov es,ax
	add di,2
	mov word [es:di],0x0720
	mov word [cs:boxloc],3998
	mov di,[cs:boxloc]
	mov word [es:di],0x07DC
	
	jmp checkright
	
	printleft:
    mov ax, 0xB800
	mov es,ax
	mov di,[cs:boxloc]
	mov word [es:di],0x07DC
	add di,2
	mov word [es:di],0x0720 
	
	checkright:
	cmp word [cs:flagr], 1
	jne nomama

    add word [cs:boxloc],2
	cmp word [cs:boxloc],4000
	jb printright	;if below we will move box to 4000
	
	mov di,[cs:boxloc]
	mov ax, 0xB800
	mov es,ax
	sub di,2
	mov word [es:di],0x0720
	mov word [cs:boxloc],3840
	mov di,[cs:boxloc]
	mov word [es:di],0x07DC
	
	jmp nomama
	
	printright:
    mov ax, 0xB800
	mov es,ax
	mov di,[cs:boxloc]
	mov word [es:di],0x07DC
	sub di,2
	mov word [es:di],0x0720 
	
	nomama:
	mov ax,0xB800
	mov es,ax
	cmp word [cs:timecount],510
	jne abey
	mov word [cs:timecount],0
	
	abey:
	
	
	inc word [cs:timecount]
	push word [cs:timecount]
	mov ax,10
	push ax
	call printnum
	
	;print score
	call printscore
	
	mov ax,[cs:score]
	push ax
	mov ax, 80
	push ax
	call printnum
	
	;print missing
	call printmissed
	
	mov ax,[cs:miss]
	push ax
	mov ax, 140
	push ax
	call printnum
	
	mov ax,[cs:timecount]
	mov bx,7
	div bl
	cmp ah,0
	jne nexttime   ;every 7 ticks
	
	mov di,[cs:alphloc]
	cmp di,3840
	jb not1
	cmp di,3998
	ja not1
	inc word [cs:miss]

	
	not1:
	
	mov di,[cs:alphloc]
	mov dx,[es:di]
	mov word [es:di],0x0720
	add word [cs:alphloc],160
	mov di,[cs:alphloc]
	cmp di,[cs:boxloc]  ;compare with box location
	jne paratha1
        inc word [cs:score]
paratha1: cmp di,3998
	ja alph1
	mov [es:di],dx 
	jmp nexttime

	
	alph1:
	;new random alpha
	mov bx,0
	sub sp,2

	push 79
	call randG
	pop di
	mov bl,1
	mov ax,80
	mul bl
	add ax,di
	shl ax,1
	mov di,ax
 
	sub sp,2
	push 25
	call randG
	pop bx
	mov bh,0x07
	add bx,'A'
	mov [es:di],bx
	
	mov [cs:alphloc ],di
	
	nexttime:
	
	mov ax,[cs:timecount]
	mov bx,8
	div bl
	cmp ah,0
	jne nexttime1   ;every 2 ticks
	
	mov di,[cs:alphloc+2]
	cmp di,3840
	jb not2
	cmp di,3998
	ja not2
	
	inc word [cs:miss]

	not2:
	mov di,[cs:alphloc+2]
	mov dx,[es:di]
	mov word [es:di],0x0720
	add word [cs:alphloc+2],160
	mov di,[cs:alphloc+2]
	cmp di,[cs:boxloc]  ;compare with box location
	jne paratha2
        inc word [cs:score]
paratha2:
	cmp di,3998
	ja alph2
	mov [es:di],dx 
	jmp nexttime1
	alph2:
	
	mov bx,0
	sub sp,2

	push 79
	call randG
	pop di
	mov bl,1
	mov ax,80
	mul bl
	add ax,di
	shl ax,1
	mov di,ax
 
	sub sp,2
	push 25
	call randG
	pop bx
	mov bh,0x07
	add bx,'A'
	mov [es:di],bx
	
	mov [cs:alphloc+2 ],di
	
	nexttime1:
	
	mov ax,[cs:timecount]
	mov bx,9
	div bl
	cmp ah,0
	jne nexttime2   ;every 2 ticks
	
	mov di,[cs:alphloc+4]
	cmp di,3840
	jb not3
	cmp di,3998
	ja not3
	inc word [cs:miss]

	
	not3:
	
	mov di,[cs:alphloc+4]
	mov dx,[es:di]
	mov word [es:di],0x0720
	add word [cs:alphloc+4],160
	mov di,[cs:alphloc+4]
	cmp di,[cs:boxloc]  ;compare with box location
	jne paratha3
        inc word [cs:score]
paratha3:
	cmp di,3998
	ja alph3
	mov [es:di],dx 
	jmp nexttime2
	alph3:
	
	mov bx,0
	sub sp,2

	push 79
	call randG
	pop di
	mov bl,1
	mov ax,80
	mul bl
	add ax,di
	shl ax,1
	mov di,ax
 
	sub sp,2
	push 25
	call randG
	pop bx
	mov bh,0x07
	add bx,'A'
	mov [es:di],bx
	
	mov [cs:alphloc+4 ],di
	
	nexttime2:
	
	mov ax,[cs:timecount]
	mov bx,10
	div bl
	cmp ah,0
	jne nexttime3   ;every 2 ticks
	
	mov di,[cs:alphloc+6]
	cmp di,3840
	jb not4
	cmp di,3998
	ja not4
	
	inc word [cs:miss]

	
	not4:
	
	mov di,[cs:alphloc+6]
	mov dx,[es:di]
	mov word [es:di],0x0720
	add word [cs:alphloc+6],160
	mov di,[cs:alphloc+6]
	cmp di,[cs:boxloc]  ;compare with box location
	jne paratha4
        inc word [cs:score]
paratha4:
	cmp di,3998
	ja alph4
	mov [es:di],dx 
	jmp nexttime3
	alph4:
	
	mov bx,0
	sub sp,2

	push 79
	call randG
	pop di
	mov bl,1
	mov ax,80
	mul bl
	add ax,di
	shl ax,1
	mov di,ax
 
	sub sp,2
	push 25
	call randG
	pop bx
	mov bh,0x07
	add bx,'A'
	mov [es:di],bx
	
	mov [cs:alphloc+6 ],di
	
	nexttime3:
	
	mov ax,[cs:timecount]
	mov bx,11
	div bl
	cmp ah,0
	jne nexttime4   ;every 2 ticks
	
	mov di,[cs:alphloc+8]
	cmp di,3840
	jb not5
	cmp di,3998
	ja not5
	
	inc word [cs:miss]

	
	not5:
	
	mov di,[cs:alphloc+8]
	mov dx,[es:di]
	mov word [es:di],0x0720
	add word [cs:alphloc+8],160
	mov di,[cs:alphloc+8]
	cmp di,[cs:boxloc]  ;compare with box location
	jne paratha5
        inc word [cs:score]
paratha5:
	cmp di,3998
	ja alph5
	mov [es:di],dx 
	jmp nexttime4
	alph5:
	
	mov bx,0
	sub sp,2

	push 79
	call randG
	pop di
	mov bl,1
	mov ax,80
	mul bl
	add ax,di
	shl ax,1
	mov di,ax
 
	sub sp,2
	push 25
	call randG
	pop bx
	mov bh,0x07
	add bx,'A'
	mov [es:di],bx
	
	mov [cs:alphloc+8 ],di
	
	nexttime4:
	
	exit1:
	cmp word [cs:miss],10
	jb pooo
	;call clearscr1
	mov word[terminate],1
	pooo:
	;mov word[terminate],1
	mov di,[cs:boxloc]
	mov word [es:di],0x07DC  ;AAAAAA
	mov al, 0x20
	out 0x20, al ; end of interrupt
	pop es
	popa
	iret


start: call clearscr
	mov ax, 0xB800
	mov es,ax
	 
	mov di,320
	mov cx,5
	mov si,0
loop2:
	mov bx,0
	sub sp,2

	push 79
	call randG
	pop di
	mov bl,2
	mov ax,80
	mul bl
	add ax,di
	shl ax,1
	mov di,ax
 
	sub sp,2
	push 25
	call randG
	pop bx
	mov bh,0x07
	add bx,'A'
	mov [es:di],bx
	;sub di,160           ;MOLRFMEORIF
	mov [cs:alphloc + si],di
	add si,2
;add di,20
loop loop2

	
xor ax, ax
mov es, ax ; point es to IVT base

mov ax,[es:9*4]
mov [cs:oldkb],ax
mov ax,[es:9*4+2]
mov [cs:oldkb+2],ax


cli ; disable interrupts

mov word [es:9*4], kbisr; store offset at n*4
mov [es:9*4+2], cs ; store segment at n*4+2

mov ax, [es:8*4]
	mov bx,[es:8*4+2]
	mov [oldtime],ax
	mov [oldtime+2],bx
mov word [es:8*4], timer; store offset at n*4
mov [es:8*4+2], cs ; store segment at n*4+2

sti ; enable interrupts
loopss:
	cmp word[cs:terminate],1
	jne loopss
call clearscr




;mov dx, start ; end of resident portion
;add dx, 15 ; round up to next para
;mov cl, 4
;shr dx, cl ; number of paras
cli
mov ax, [cs:oldtime]
mov  [es:8*4],ax
mov ax, [cs:oldtime+2]
mov  [es:8*4+2],ax

mov ax,[cs:oldkb]
mov [es:9*4],ax
mov ax,[cs:oldkb+2]
mov [es:9*4+2],ax
sti

mov ax,msg
push ax
push word 220
call printstr

mov ax,scorestr
push ax
push word 380
call printstr

push word [cs:score]
mov ax,400
push ax
call printnum

mov ax,missstr
push ax
push word 540
call printstr

push word [cs:miss]
mov ax,560
push ax
call printnum

mov ax, 0x4c00 ; terminate and stay resident
int 0x21



