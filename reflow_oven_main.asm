
; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P1.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'CLEAR' push button connected to P1.5 is pressed.
$NOLIST
$MODN76E003
$LIST

;  N76E003 pinout:
;                               -------
;       PWM2/IC6/T0/AIN4/P0.5 -|1    20|- P0.4/AIN5/STADC/PWM3/IC3
;               TXD/AIN3/P0.6 -|2    19|- P0.3/PWM5/IC5/AIN6
;               RXD/AIN2/P0.7 -|3    18|- P0.2/ICPCK/OCDCK/RXD_1/[SCL]
;                    RST/P2.0 -|4    17|- P0.1/PWM4/IC4/MISO
;        INT0/OSCIN/AIN1/P3.0 -|5    16|- P0.0/PWM3/IC3/MOSI/T1
;              INT1/AIN0/P1.7 -|6    15|- P1.0/PWM2/IC2/SPCLK
;                         GND -|7    14|- P1.1/PWM1/IC1/AIN7/CLO
;[SDA]/TXD_1/ICPDA/OCDDA/P1.6 -|8    13|- P1.2/PWM0/IC0
;                         VDD -|9    12|- P1.3/SCL/[STADC]
;            PWM5/IC7/SS/P1.5 -|10   11|- P1.4/SDA/FB/PWM1
;                               -------
;

CLK           EQU 16600000 ; Microcontroller system frequency in Hz
BAUD              EQU 115200 ; Baud rate of UART in bps
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))
TIMER1_RELOAD     EQU (0x100-(CLK/(16*BAUD)))

Ramp_To_Soak_Time EQU 60 ;60 SECONDS
Ramp_To_Soak_Temp EQU 50 ;50 Degrees Celcius

PWM_OUT equ P1.0 ;Logic 1= Oven On
S_R   equ P1.6 ;switch between soak and reflow 
TIMER  equ P1.5 ;set time
TEMP  equ P0.4 ;set temp
START equ P1.2 ;start program
; equ P0.4 ;kill switch

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
DSEG at 0x30
Time_Min: ds 2
Temp_Min: ds 2
Oven_Rounded_Temp: ds 3
Oven_Temp: ds 4
Soak_Temp: ds 4
Reflow_Temp: ds 4
Count1ms:     ds 2 ; Used to determine when half second has passed
Reflow_Timer_counter: ds 2
Soak_Timer_counter: ds 2
Live_Clock_1: ds 2
Live_Clock_2: ds 2
sum: ds 4
ambient: ds 4
ambient_sum: ds 4
VLED_ADC: ds 2

x:   ds 4
y:   ds 4
bcd: ds 5

pwm_counter:  ds 1 ; Free running counter 0, 1, 2, ..., 100, 0
pwm:          ds 1 ; pwm percentage
seconds:      ds 1 ; a seconds counter attached to Timer 2 ISR

BSEG
mf: dbit 1
temp_min_flag_execute: dbit 1
temp_min_flag: dbit 1
soak_temp_flag_execute: dbit 1
soak_temp_flag: dbit 1
reflow_temp_flag_execute: dbit 1
reflow_temp_flag: dbit 1
cooldown_temp_flag_execute: dbit 1
cooldown_temp_flag: dbit 1

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1

cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P1.3
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P1.4
LCD_D4 equ P0.0
LCD_D5 equ P0.1
LCD_D6 equ P0.2
LCD_D7 equ P0.3

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

$NOLIST
$include(project1_functions.inc) ;
$LIST

$NOLIST
$include(math32.inc)
$LIST

;                     1234567890123456    <- This helps determine the location of the counter

Temp_Message:  db 'To=00C Tj=00C   ', 0
Set_Message:  db 's000,000r000,000', 0

State_1_Top_Text: db 'To=00C Tj=00C   ', 0
State_1_Bottom_Text: db 'Time:0000 S:[1] ', 0

State_2_Top_Text: db 'To=00C Tj=00C   ', 0
State_2_Bottom_Text: db 'Time:0000 S:[2] ', 0

State_3_Top_Text: db 'To=00C Tj=00C   ', 0
State_3_Bottom_Text: db 'Time:0000 S:[3] ', 0

State_4_Top_Text: db 'To=00C Tj=00C   ', 0
State_4_Bottom_Text: db 'Time:0000 S:[4] ', 0

State_5_Top_Text: db 'To=00C Tj=00C   ', 0
State_5_Bottom_Text: db 'Time:0000 S:[5] ', 0

Error_Top_Text: db 'ERROR           ',0
Error_Bottom_Text: db 'Prog Terminated ', 0

Test_Text: db 'TEST            ',0

Open_Oven: db 'Open Oven       ',0
Empty_Message: db '                ',0
Success_Message: db 'Success!        ',0

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	orl CKCON, #0b00001000 ; Input for timer 0 is sysclk/1
	mov a, TMOD
	anl a, #0xf0 ; 11110000 Clear the bits for timer 0
	orl a, #0x01 ; 00000001 Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz wave at pin SOUND_OUT   ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	; Timer 0 doesn't have 16-bit auto-reload, so
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	setb TR0
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	orl T2MOD, #0x80 ; Enable timer 2 autoreload
	mov RCMP2H, #high(TIMER2_RELOAD)
	mov RCMP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	mov pwm_counter, #0
	; Enable the timer and interrupts
	orl EIE, #0x80 ; Enable timer 2 interrupt ET2=1
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in the ISR.  It is bit addressable.
	cpl P0.4 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1
	
	inc pwm_counter
	clr c
	mov a, pwm
	subb a, pwm_counter ; If pwm_counter <= pwm then c=1
	cpl c
	mov PWM_OUT, c
	
	mov a, pwm_counter
	cjne a, #100, Timer2_ISR_done_Jump1
	mov pwm_counter, #0
	
	ljmp Inc_Done
Timer2_ISR_done_jump1:
	ljmp Timer2_ISR_done
Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done_jump1 ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done_jump1
	
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov x+0, Live_Clock_1
	mov x+1, #0x00
	mov x+2, #0x00
	mov x+3, #0x00
	mov y+0, #0x01
	mov y+1, #0x00
	mov y+2, #0x00
	mov y+3, #0x00
	lcall add32
	mov Live_Clock_1, x+0
	mov a, R6
	cjne a, #0x00, Display_Clock_Check
	ljmp Timer2_ISR_done
Display_Clock_Check:
	cjne a, #0x05, Display_Clock
	ljmp Timer2_ISR_done
Display_Clock:
	ljmp Timer2_ISR_done_Print
;Timer2_ISR_da:
;	da a ; Decimal adjust instruction.  Check datasheet for more details!
;	mov Live_Clock, a
Timer2_ISR_done:
	pop psw
	pop acc
	reti
Timer2_ISR_done_Print:
	;Set_Cursor(2,3)
	;Display_BCD(Live_Clock_1);PRINTS OUT CLOCK IN HEX
	lcall hex2bcd
	Set_Cursor(2,6)
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)
	pop psw
	pop acc
	reti

Init_All:
	orl	CKCON, #0x10 ; CLK is the input for timer 1
	orl	PCON, #0x80 ; Bit SMOD=1, double baud rate
	mov	SCON, #0x52
	anl	T3CON, #0b11011111
	anl	TMOD, #0x0F ; Clear the configuration bits for timer 1
	orl	TMOD, #0x20 ; Timer 1 Mode 2
	mov	TH1, #TIMER1_RELOAD ; TH1=TIMER1_RELOAD;
	setb TR1
	
	
	; Initialize the pins used by the ADC (P1.1, P1.7, P3.0) as input.
	orl	P1M1, #0b11000010
	anl	P1M2, #0b00111101
	
	; Initialize and start the ADC:
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x07 ; Select channel 7
	; AINDIDS select if some pins are analog inputs or digital I/O:
	mov AINDIDS, #0x00 ; Disable all analog inputs
	orl AINDIDS, #0b10000001 ; Activate AIN0 and AIN7 analog inputs
	orl ADCCON1, #0x01 ; Enable ADC
	ret

Read_ADC:
	clr ADCF
	setb ADCS ;  ADC start trigger signal
    jnb ADCF, $ ; Wait for conversion complete
    
    ; Read the ADC result and store in [R1, R0]
    mov a, ADCRL
    anl a, #0x0f
    mov R0, a
    mov a, ADCRH   
    swap a
    push acc
    anl a, #0x0f
    mov R1, a
    pop acc
    anl a, #0xf0
    orl a, R0
    mov R0, A
	ret	

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret
    
Copy_Data mac
	mov %0+0, %1+0
	mov %0+1, %1+1
	mov %0+2, %1+2
	mov %0+3, %1+3
	endmac

Check_Temp_Call:
	Check_Temp(VLED_ADC,x,y,Oven_Temp,ambient,ambient_sum,R5,sum,R0,R1,Oven_Rounded_Temp)
	ret 	  	
main:
	mov R6, #0x00 ;State Variable
	mov R5, #100 
	initialize_program(SP,P0M1,P0M2,P1M1,P1M2,P3M2,P3M2,R7,EA,Soak_Temp,Soak_Timer_Counter,Reflow_Temp,Reflow_Timer_Counter,#Temp_Message,#Set_Message)
	lcall Timer0_Init
    lcall Timer2_Init
    lcall Init_All
    
	;NOTE: INITIALIZATION DOES NOT SET THE SOAK/REFLOW TIME OR TEMP (That is done in State0)
	
	mov sum+0, #0
	mov sum+1, #0
	mov sum+2, #0
	mov sum+3, #0
	
	mov Live_Clock_1, #0x00
	
	Copy_Data(ambient_sum,#0)
	Copy_Data(ambient,#0)
	
	;mov Temp_Min, #0x0E
	mov Temp_Min, #0x3C ;60 Degress celcius min in HEX
	mov Time_Min, #0x32 ;50 Seconds min time in HEX
	mov a, Temp_Min
	mov x+0, a
	mov x+1, #0x00
	mov x+2, #0x00
	mov x+3, #0x00
	Load_y(0)
	lcall add32
	mov Temp_Min, x+0
	mov a, Time_Min
	mov x+0, a
	mov x+1, #0x00
	mov x+2, #0x00
	mov x+3, #0x00
	Load_y(0)
	lcall add32
	mov Time_Min, x+0
	
	clr temp_min_flag_execute
	clr soak_temp_flag_execute
	clr reflow_temp_flag_execute
	clr cooldown_temp_flag_execute
	
	clr temp_min_flag
	clr soak_temp_flag
	clr reflow_temp_flag
	clr cooldown_temp_flag
	
	;--------------------------;
	;STATES (REGISTER 6/R6)    ;
	;--------------------------;
	;State 0: Select parameters;
	;State 1: Ramp to Soak	   ;
	;State 2: Preheat/Soak     ;
	;State 3: Ramp to Peak     ;
	;State 4: Reflow           ;
	;State 5: Cooling          ;
	;--------------------------;
	ljmp State0
	
State1_Jump_1:
	ljmp State1

main_help0:
	ljmp main
	
State0:
	mov pwm, #0
	;jnb KILL, main_help0
	lcall Check_Temp_Call
	mov a, R6
	cjne a, #0x00, State1_Jump_1
	buttons(S_R,R7,TEMP,TIMER,Soak_Temp,Soak_Timer_Counter,Reflow_Temp,Reflow_Timer_Counter,x,bcd)
	jnb START, State_0_To_1
	lcall Check_Temp_Call
	ljmp State0	
		
State_0_To_1:
	;WriteCommand(#0x01) ;Clear screen
	mov R6, #0x01
	Set_Cursor(1,1)
	Send_Constant_String(#State_1_Top_Text)
	Set_Cursor(2,1)
	Send_Constant_String(#State_1_Bottom_Text)
	Wait_Milli_Seconds(#250)
	mov Live_Clock_1, #0x00
	ljmp State1

State2_Jump1:
	ljmp State2
State1:
	mov pwm, #100
	;jnb KILL, main_help1
	lcall Check_Temp_Call
	Wait_Milli_Seconds(#50)
	mov a, R6
	cjne a, #0x01, State2_Jump1
	setb soak_temp_flag_execute
	lcall Check_Temp_Call
	Wait_Milli_Seconds(#50)
	;Check if Oven_Temp > Soak_Temp
	jb mf,State1_Jump_2
	ljmp State_1_To_2	
State1_Jump_2:
	ljmp State1
State_1_To_2:
	mov R6, #0x02
	Set_Cursor(1,1)
	Send_Constant_String(#State_2_Top_Text)
	Set_Cursor(2,1)
	Send_Constant_String(#State_2_Bottom_Text)
	mov Live_Clock_1, #0x00
	ljmp State2
State3_Jump1:
	ljmp State3
State2:
	mov pwm, #20
	lcall Check_Temp_Call
	Wait_Milli_Seconds(#50)
	mov a, R6
	cjne a, #0x02, State3_Jump1
	mov x+0, Live_Clock_1
	mov x+1, #0x00
	mov x+2, #0x00
	mov x+3, #0x00
	mov y+0, Soak_Timer_Counter
	mov y+1, #0x00
	mov y+2, #0x00
	mov y+3, #0x00
	mov a, Live_Clock_1
	clr c
	subb a, Soak_Timer_Counter
	jc State2_Jump2
	ljmp State_2_To_3
State2_Jump2:
	ljmp State2
State_2_To_3:
	mov R6, #0x03
	Set_Cursor(1,1)
	Send_Constant_String(#State_3_Top_Text)
	Set_Cursor(2,1)
	Send_Constant_String(#State_3_Bottom_Text)
	mov Live_Clock_1, #0x00
	ljmp State3
State4_Jump1:
	ljmp State4
State3:
	mov pwm, #100
	lcall Check_Temp_Call
	Wait_Milli_Seconds(#100)
	setb reflow_temp_flag_execute
	jb mf, State_3_To_4
	ljmp State2
State3_Jump2:
	ljmp State3
State_3_To_4:
	mov R6, #0x04
	Set_Cursor(1,1)
	Send_Constant_String(#State_4_Top_Text)
	Set_Cursor(2,1)
	Send_Constant_String(#State_4_Bottom_Text)
	mov Live_Clock_1, #0x00
	ljmp State4
State5_Jump1:
	ljmp State5
State4:
	mov pwm, #20
	clr reflow_temp_flag_execute
	clr soak_temp_flag_execute
	clr temp_min_flag_execute
	lcall Check_Temp_Call
	Wait_Milli_Seconds(#50)
	mov a, R6
	cjne a, #0x04, State5_Jump1
	mov x+0, Live_Clock_1
	mov x+1, #0x00
	mov x+2, #0x00
	mov x+3, #0x00
	mov y+0, Reflow_Timer_Counter
	mov y+1, #0x00
	mov y+2, #0x00
	mov y+3, #0x00
	mov a, Live_Clock_1
	clr c
	subb a, Reflow_Timer_Counter
	jc State4_Jump2
	ljmp State_4_To_5
State4_Jump2:
	ljmp State4
State_4_To_5:
	mov R6, #0x05
	Set_Cursor(1,1)
	Send_Constant_String(#State_5_Top_Text)
	Set_Cursor(2,1)
	Send_Constant_String(#State_5_Bottom_Text)
	mov Live_Clock_1, #0x00
	ljmp State5
main_jump:
	ljmp main
State5:
	mov pwm, #0
	mov a, R6
	cjne a, #0x05, main_jump
	setb cooldown_temp_flag_execute
	Wait_Milli_Seconds(#50)
	lcall Check_Temp_Call
	Wait_Milli_Seconds(#50)
	jb mf, Cooldown_Done
	
	ljmp State5
Cooldown_Done:
	Set_Cursor(1,1)
	Send_Constant_String(#Open_Oven)
	Set_Cursor(2,1)
	Send_Constant_String(#Open_Oven)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	ljmp main
	
END
