DATAS SEGMENT
	sunX DW 160
	sunY DW 100
	earthX DW 50
	earthY DW 160
	roundA DW 10
	roundB DW 0
	earthXMAX DW 70
	earthYMAX DW 180
	time DB 0
   R    DB  100
   XN   DB  0
   YN   DB  100
   N    DW  ?
   X14  DW  ?
   X23  DW  ?
   Y12  DW  ?
   Y34  DW  ?
   R1    DB  100
   XN1   DW  0
   YN1   DW  100
   N1    DW  ?
   X  DW  ?
   Y  DW  ?
   FLAG DB 0
     mus_freg  dw 524,262,524,988,3 dup (988)     ;频率表
               dw 3 dup (784),659,587,392
               dw 784,294,784,294,4 dup (587)
               dw 988,294,988,294,988,-1
     mus_time  dw 26 dup (25)                  ;节拍表26
DATAS ENDS

STACKS SEGMENT
    ;此处输入堆栈段代码
STACKS ENDS
ADDRESS MACRO A,B
     LEA SI,A
     LEA BP,DS:B
ENDM
CODES SEGMENT
    ASSUME CS:CODES,DS:DATAS,SS:STACKS
START:    
        MOV AX,DATAS
        MOV DS,AX
        MOV AX,0013H;设置320*200图形模式
        INT 10H
    	address mus_freg, mus_time    	
        CALL DRAWCIRCLE
        CALL DRAW
        push AX
        pushf
HAHA:   
		popf
		POP AX
		CALL COMPUTE
		CALL DRAWEARTH
		call music
		call SOFTDLY
		call clearEARTH
		CALL DRAWCIRCLE
		mov XN,0
		MOV YN,100
		pushf
		push ax
		mov ah,01
		int 16h
		cmp al,1bh
        JNZ HAHA
        MOV AH,4CH
        INT 21H
;
SOFTDLY PROC                      ;定义延时子程序
	MOV BX,50
DELAY:MOV CX,2801
WAITS:LOOP WAITS
      DEC BX
      JNZ DELAY
	  RET
SOFTDLY ENDP
;
SOFTDLY1 PROC                      ;定义延时子程序
	MOV BX,10
DELAY:MOV CX,2801
WAITS:LOOP WAITS
      DEC BX
      JNZ DELAY
	  RET
SOFTDLY1 ENDP
;
DRAWCIRCLE PROC
        JMP XS   
NEXT:   MOV XN,0
        MOV AL,XN
        MUL AL
        MOV BL,4
        MUL BL
        MOV BX,AX
        MOV AL,R
        MUL AL
        SUB BX,AX
        MOV AL,YN
        DEC AL
        MUL AL
        ADD AX,BX
        MOV N,AX
        JGE XUAND
        JS XUANH
XUAND:;命中
        DEC YN
        JMP XS
XUANH:  MOV AL,XN
		MUL AL
		MOV CX,4
		MOV BX,AX
HEREH:  INC XN
		MOV AL,XN
      	MUL AL
        SUB AX,BX
        MUL CX
        ADD AX,N
        JS HEREH
        DEC YN
XS:
        MOV DH,0;圆心从（0,0）转换到（160,100）
        MOV CH,0
        MOV CL,XN
        MOV DL,YN
        mov ax,100
        mov bx,160
        add ax,cx
        add bx,dx
        mov X14,ax
        mov Y12,bx  
        
        MOV CX,0;计算X23
        MOV CL,XN
        ADD CX,CX
        SUB AX,CX
        MOV X23,AX

        MOV CL,YN;计算Y34
        ADD CX,CX
        SUB BX,CX
        MOV Y34,BX     
OVER2:             
        MOV AL,07h;第一象限的点
        MOV AH,0CH
        MOV Dx,X14
        MOV Cx,Y12    
        INT 10H
        
        MOV Dx,X23;二   
        INT 10H      
        
        MOV Cx,Y34;三
        INT 10H
        
        MOV Dx,X14;四     
        INT 10H
        
        MOV AL,YN
        MOV AH,1
        SUB AL,AH
        JZ  EXIT
        JMP NEXT
EXIT:  
DRAWCIRCLE ENDP
RET
;
DRAW PROC
    MOV DX,70
AGAIN1:
    MOV CX,130
AGAIN_X1:
    PUSH CX
    PUSH DX 
    CMP CX,160
    JB XBELOW1
    SUB CX,160
    JMP XSQUARE1
XBELOW1:
    MOV BX,160
    SUB BX,CX
    MOV CX,BX
XSQUARE1:
    MOV AX,CX
    MUL CX
    MOV CX,AX
AGAIN_Y1:
    POP DX
    PUSH DX
    CMP DX,100
    JB YBELOW1
    SUB DX,100
    JMP YSQUARE1
YBELOW1:
    MOV BX,100
    SUB BX,DX
    MOV DX,BX
YSQUARE1:
    MOV AX,DX
    MUL DX
    MOV DX,AX
HANDLE1:
    ADD CX,DX
    MOV BX,CX
    POP DX
    POP CX
    CMP BX,900
    JA LAST1
    MOV AH,0CH
    MOV AL,0100B
    INT 10H
LAST1:
    INC CX
    CMP CX,190
    JB AGAIN_X1
    INC DX
    CMP DX,130
    JB AGAIN1
DRAW ENDP
RET
;

clearEARTH PROC
	MOV DX,earthX;传参
    SUB DX,10
    MOV CX,earthY;传参
    SUB CX,10
CLEARcx:
	CLEARdx:
	    MOV AH,0CH
    	MOV AL,0000B
    	INT 10H
    	INC DX
		CMP DX,earthXMAX
		JGE NEXT
    	JMP CLEARdx	
NEXT:
	MOV DX,earthX;传参
	SUB DX,10
	INC CX
    CMP CX,earthYMAX
    JGE EXIT 
	JMP CLEARcx
EXIT:RET
clearEARTH ENDP
;
DRAWEARTH PROC
	dec roundA
	dec roundB
	cmp roundB,-40
	JG COUTINUE
	mov roundA,20
	mov roundB,10
COUTINUE:	MOV DX,earthX;传参
    SUB DX,10
AGAIN:
    MOV CX,earthY;传参
    SUB CX,10
AGAIN_X:
    PUSH CX
    PUSH DX
    CMP CX,earthY
    JB XBELOW
    SUB CX,earthY
    JMP XSQUARE
XBELOW:
    MOV BX,earthY
    SUB BX,CX
    MOV CX,BX
XSQUARE:
    MOV AX,CX
    MUL CX
    MOV CX,AX
AGAIN_Y:
    POP DX
    PUSH DX
    CMP DX,earthX
    JB YBELOW
    SUB DX,earthX
    JMP YSQUARE
YBELOW:
    MOV BX,earthX
    SUB BX,DX
    MOV DX,BX
YSQUARE:
    MOV AX,DX
    MUL DX
    MOV DX,AX
HANDLE:
    ADD CX,DX
    MOV BX,CX
    POP DX
    POP CX
    CMP BX,100
    JA LAST
    MOV AH,0CH
    MOV BX,earthY
    SUB BX,CX
    cmp BX,roundA
    JG OTHERCOLOR
    cmp BX,roundB
    JL OTHERCOLOR
    MOV AL,0001B
    JMP INPUT
OTHERCOLOR:MOV AL,0010B
INPUT:INT 10H
LAST:
    INC CX
    MOV BX,earthY
    ADD BX,10
    CMP CX,BX
    JB AGAIN_X
    INC DX
    MOV BX,earthX
    ADD BX,10
    CMP DX,BX
    JB AGAIN
    RET
DRAWEARTH ENDP
;
COMPUTE PROC
NEXT:   MOV XN1,0
        MOV AX,XN1
        MUL AX
        TEST AX,AX
        JNS OK4
OK4:    NEG AX
        MOV BL,4
        MUL BL
        MOV BX,AX
        MOV AL,R1
        MUL AL
	    SUB BX,AX
        MOV AX,YN1
        CMP FLAG,0
        JE FIRST
        INC AX
        JMP SECOND
FIRST:  DEC AX
SECOND: MUL AX
        TEST AX,AX
        JNS OK
        NEG AX
OK:     ADD AX,BX
        MOV N1,AX
        JGE XUAND
        JS XUANH
XUAND:;命中
        CMP FLAG,0
        JE FIRST1
        INC AX
		JMP SECOND1
FIRST1: DEC YN1
SECOND1:JMP XS
;
XUANH:  MOV AX,XN1
		MUL AX
      	TEST AX,AX
        JNS OK2
        NEG AX
OK2:	MOV CX,4
		MOV BX,AX
HEREH:  
		CMP FLAG,0
        JE FIRST2
        DEC XN1
		JMP SECOND2
FIRST2: INC XN1
SECOND2:MOV AX,XN1
      	MUL AX
      	CMP AX,0
      	JG CHNEG
      	NEG AX
CHNEG:  SUB AX,BX
        MUL CX
        ADD AX,N1
        JS HEREH
        CMP FLAG,0
        JE FIRST3
        INC YN1
        JMP XS
FIRST3: DEC YN1
XS:
        MOV CX,XN1
        MOV DX,YN1
        CMP YN1,-100
        JG CHFLAG
        MOV FLAG,1
CHFLAG:CMP YN1,98
        JLE NEXTTURN
        MOV FLAG,0
NEXTTURN:
        mov ax,100
        mov bx,160
        add ax,cx
        add bx,dx
        mov X,ax
        mov Y,bx  
        mov BX,X
        MOV earthX,BX
        ADD BX,10
        MOV earthXMAX,BX 
        mov BX,Y
        mov earthY,BX
        ADD BX,10
		MOV earthYMAX,BX
EXIT:	RET
COMPUTE ENDP
gensound proc near
     push ax
     push bx
     push cx
     push dx
     push di

     mov al, 0b6H
     out 43h, al 
     mov dx, 12h
     mov ax, 348ch
     div di
     out 42h, al;改变频率

     mov al, ah
     out 42h, al

     in al, 61h
     mov ah, al
     or al, 3
     out 61h, al;允许驱动扬声器
wait1:
     mov cx, 3314
     call waitf
delay1:
     dec bx
     jnz wait1;节拍

     mov al, ah
     out 61h, al

     pop di
     pop dx
     pop cx
     pop bx
     pop ax
     ret 
gensound endp

;--------------------------
waitf proc near
      push ax
waitf1:
      loop waitf1
      pop ax
      ret
waitf endp
;--------------发声调用函数----------------
music proc near
      xor ax, ax
freg:
      mov di, [si]
      cmp di, 0FFFFH
      jne continueGen
      address mus_freg, mus_time
continueGen:mov bx, ds:[bp]
      call gensound
      add si, 2
      add bp, 2
end_mus:
      ret
music endp
CODES ENDS
    END START
;





















