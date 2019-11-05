; guessing-game.asm
; Steven L. Garverick

; Cycles output bits RB2-0 according to the right-hand tbird tail lights
; Cycles output bits RB5-3 according to the left-hand tbird tail lights
; Reads Haz input from RA2
; Reads Left input from RA1
; Reads Right input from RA0

; There is a delay of approximately 0.5 seconds from one state to the next
; This delay is created using a 16*256 double loop
; The loop delay is approx 16 * 256 * 3 CPU cycles 
; Using an oscillator frequeny of 100 kHz, a CPU cycle is 40 usec
; Therefore, the loop delay is about 492 msec 
 
; CPU configuration
; (16F84 with RC osc, watchdog timer off, power-up timer on)

	processor 16f84A
	include <p16F84A.inc>
	__config _RC_OSC & _WDT_OFF & _PWRTE_ON

; some handy macro definitions

IFSET macro fr,bit,label
	btfss fr,bit ; if (fr.bit) then execute code following macro
	goto label ; else goto label	
      endm

IFCLR macro fr,bit,label
	btfsc fr,bit ; if (! fr.bit) then execute code following macro
	goto label ; else goto label
	  endm

IFEQ macro fr,lit,label
	movlw lit
	xorwf fr,W
	btfss STATUS,Z ; (fr == lit) then execute code following macro
	goto label ; else goto label
	 endm

IFNEQ macro fr,lit,label
	movlw lit
	xorwf fr,W
	btfsc STATUS,Z ; (fr != lit) then execute code following macro
	goto label ; else goto label
	 endm

MOVLF macro lit,fr
	movlw lit
	movwf fr
	  endm

MOVFF macro from,to
	movf from,W
	movwf to
  	  endm

; file register variables

nextS equ 0x0C 	; next state (output)
octr equ 0x0D	; outer-loop counter for delays
ictr equ 0x0E	; inner-loop counter for delays

; state definitions for Port B

S1 equ B'00000001'
S2 equ B'00000010'
S3 equ B'00000100'
S4 equ B'00001000'
SOK equ B'00000000'
SERR equ B'10000000'

; input bits on Port A
G4 equ 3
G3 equ 2
G2 equ 1
G1 equ 0
 
; beginning of program code

	org 0x00	; reset at address 0
reset:	goto	init	; skip reserved program addresses	

	org	0x08 	; beginning of user code

init:
; set up RB4-0 as outputs
    bsf STATUS, RP0
    movlw B'01110000'
    movwf TRISB
    bcf STATUS, RP0
; initialize state variables
    MOVLF S1, nextS ; nextS = S1
mloop: ; here begins the main program loop
    movf nextS,w
    movwf PORTB
    call delay
; test inputs and compute next state
    IFCLR PORTA, G1, yesG1 ; if(!G1)
    
noG1:
    IFCLR PORTA, G2, yesG2 ; else if (!G2)
	IFCLR PORTA, G3, yesG3 ; if (!G3)
	IFCLR PORTA, G4, yesG4 ; if (!G4)
	IFEQ PORTB, S1, xS2 ; if(state == S1)
	    MOVLF S2, nextS ; nextS = S2
		goto mloop 
yesG1:
    IFEQ PORTB, S1, yesG11 ; if(state == S1)
	IFCLR PORTA, G2, xSERR ; if(!G2)
	IFCLR PORTA, G3, xSERR ; if(!G3)
	IFCLR PORTA, G4, xSERR ; if((!G3)
	    MOVLF SOK, nextS ; nextS = SOK
		goto mloop
		
yesG11:
    IFEQ PORTB, SOK, xSERR ; if (state == SOK)
	MOVLF SOK, nextS ; nextS = SOK
	    goto mloop
 
yesG2:
   IFEQ PORTB, SOK, yesG22 ; if(state = SOK)
	MOVLF SOK, nextS ; nextS = SOK
	    goto mloop

yesG22:
    IFEQ PORTB, S2, xSERR ; if(state == S2)
	IFCLR PORTA, G3, xSERR ; if(!G3)
	IFCLR PORTA, G4, xSERR ; if(!G4)
	    MOVLF SOK, nextS ; nextS = SOK
		goto mloop

yesG3:
    IFEQ PORTB, SOK, yesG33 ; if(state == SOK)
	MOVLF SOK, nextS ; nextS = SOK
	    goto mloop

yesG33:
    IFEQ PORTB, S3, xSERR ; if(state == S3)
	IFCLR PORTA, G4, xSERR ; if(!G4)
	    MOVLF SOK, nextS ; nextS = SOK
		goto mloop

yesG4:
    IFEQ PORTB, SOK, yesG44 ; if(state == SOK)
	MOVLF SOK, nextS ; nextS = SOK
	    goto mloop
	 
yesG44:
    IFEQ PORTB, S4, xSERR ; if(state == S4)
	MOVLF SOK, nextS ; nextS = SOK
	    goto mloop
    
xS1:
    MOVLF SOK, nextS ; nextS = SOK
	goto mloop
    
xS2:
    IFEQ PORTB, S2, xS3 ; else if (state == S2)
	MOVLF S3,nextS ; nextS = S3
	    goto mloop
	
xS3:
    IFEQ PORTB, S3, xS4 ; else if (state == S3)
	MOVLF S4, nextS ; nextS = S4
	    goto mloop
	    
xS4:
    IFEQ PORTB, S4, SS ; else if (state == S4)
	MOVLF S1, nextS ; nextS = S1
	    goto mloop
	    
SS:
    MOVLF S1, nextS ; nextS = S1
	goto mloop
xSERR:
    MOVLF SERR, nextS ; nextS = SEER
	goto mloop
    
delay: ; create a delay of about 0.5 seconds
	MOVLF	d'128',octr ; initialize outer loop counter to 16
d1:	clrf	ictr	; initialize inner loop counter to 256
d2: decfsz	ictr,F	; if (--ictr != 0) loop to d2
	goto 	d2		 	
	decfsz	octr,F	; if (--octr != 0) loop to d1 
	goto	d1 
	return
endloop: ; end of main loop
	goto	mloop

	end		; end of program code		
