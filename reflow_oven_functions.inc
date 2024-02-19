cseg
    
;--------------------------------------;
; Check Button of S_R (Soak or Reflow) ;
;--------------------------------------;

Display_formated_BCD mac
	lcall hex2bcd
	Set_Cursor(%1,%2)
	Display_BCD(%0+1)
	Display_BCD(%0+0)
	Set_Cursor(2,1)
	Display_char(#'s')
	Set_Cursor(2,5)
	Display_char(#',')
	Set_Cursor(2,9)
	Display_char(#'r')
	Set_Cursor(2,13)
	Display_char(#',')
endmac

Display_Clock_BCD mac
	lcall hex2bcd
	Set_Cursor(%1,%2)
	Display_BCD(%0+1)
	Display_BCD(%0+0)
endmac

Display_formated_BCD_OvenTemp mac
	mov Oven_Temp+0, x+2
	mov Oven_Temp+1, x+1
	mov Oven_Temp+2, x+0
	;Set_Cursor(2,10)
	;Display_BCD(Oven_Temp+0)
	;Display_BCD(Oven_Temp+1)
	;Display_BCD(Oven_Temp+2)
	lcall hex2bcd
	Set_Cursor(1,4)
	Display_BCD(bcd+2)
	Display_BCD(bcd+1)
	;SEND TO PUTTY
	Send_BCD(bcd+2)
	Send_BCD(bcd+1)
	mov a, #'.'
	lcall putchar
	Send_BCD(bcd+0)
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar
	
	mov x+0, Oven_Temp+2
	mov x+1, Oven_Temp+1
	mov x+2, Oven_Temp+0
	mov x+3, #0x00
	
	mov y+0, #0x64
	mov y+1, #0x00
	mov y+2, #0x00
	mov y+3, #0x00
	lcall div32

	mov Oven_Temp+0,x+0
	mov Oven_Temp+1, x+1
	mov Oven_Temp+2, #0x00
	Set_Cursor(1,15)
	;Display_BCD(Oven_Temp+1);2nd Right Most Bit in HEX
	;Display_BCD(Oven_Temp+0) ; Right Most Bit in HEX
	jnb temp_min_flag_execute, next1%M
	Set_Cursor(1,14)
	Display_char(#'T')
	mov x+0, Oven_Temp+0
	mov x+1, Oven_Temp+1
	mov x+2, #0x00
	mov x+3, #0x00
	
	mov y+0, Temp_Min
	mov y+1, #0x00
	mov y+2, #0x00
	mov y+3, #0x00
	lcall x_lt_y
	mov temp_min_flag, mf
	clr temp_min_flag_execute
	ljmp next4%M
next1%M:
		jnb soak_temp_flag_execute, next2%M
		Set_Cursor(1,15)
		Display_char(#'M')
		mov x+0, Oven_Temp+0
		mov x+1, #0x00
		mov x+2, #0x00
		mov x+3, #0x00
	;	Copy_Data(x,Oven_Temp)
	;	Copy_Data(y,Soak_Temp)
		mov y+0, Soak_Temp+0
		mov y+1, #0x00
		mov y+2, #0x00
		mov y+3, #0x00
		lcall x_lt_y
		mov soak_temp_flag, mf
		clr soak_temp_flag_execute
		ljmp next4%M
	next2%M:
		jnb reflow_temp_flag_execute, next3%M
		Set_Cursor(1,14)
		Display_char(#'K')
		clr c
		clr mf
		mov x+0, Oven_Temp+0
		mov x+1, #0x00
		mov x+2, #0x00
		mov x+3, #0x00
	
		mov y+0, Reflow_Temp+0
		mov y+1, #0x00
		mov y+2, #0x00
		mov y+3, #0x00
		lcall x_gt_y
		mov reflow_temp_flag, mf
		clr reflow_temp_flag_execute
		ljmp next4%M
	next3%M:
		Set_Cursor(1,14)
		Display_char(#'L')
	
		clr c
		clr mf
		mov x+0, Oven_Temp+0
		mov x+1, Oven_Temp+1
		mov x+2, #0x00
		mov x+3, #0x00
	
		mov y+0, #0x3C
		mov y+1, #0x00
		mov y+2, #0x00
		mov y+3, #0x00
		lcall x_lt_y
		mov cooldown_temp_flag, mf
		clr cooldown_temp_flag_execute
		ljmp next4%M
	next4%M:
	
	next5%M:
endmac

Display_formated_BCD_Ambient mac
	lcall hex2bcd
	Set_Cursor(1,11)
	Display_BCD(bcd+1)
endmac

Print_State_Text mac
	Set_Cursor(1,1)
	Send_Constant_String(%0)
	Set_Cursor(2,1)
	Send_Constant_String(%1)
endmac

buttons mac
	jb %0, check_jump%M
	Wait_Milli_Seconds(#50)
	jb %0, check_jump%M 
	ljmp add_flag%M	
	check_jump%M:
		ljmp check%M
	add_flag%M:
		mov a, %1
   		cjne a, #0x00, add_one%M
		mov a, #0x01
		mov %1, A
		Wait_Milli_Seconds(#50)
		ljmp check%M
		
  	add_one%M:
		Wait_Milli_Seconds(#50)
  		mov a, #0x00
 		mov %1, a
	
	check%M:
		mov a, %1
		Wait_Milli_Seconds(#50)
		cjne a, #0x00, change_reflow_temp_jump%M
		ljmp change_soak_temp%M
		
	change_reflow_temp_jump%M:
		ljmp change_reflow_temp%M
		
	Soak_Help_Loop%M:
		ljmp Soak_Timer_Button%M
	
	change_soak_temp%M:
		setb %2 ; Before using as input...
		jb %2, Soak_Help_Loop%M
		Wait_Milli_Seconds(#50) ; Wait and check again
		jb %2, Soak_Help_Loop%M
  
		mov a, %4
  		mov %8+0, a
  		mov %8+1, #0x00
  		mov %8+2, #0x00
  		mov %8+3, #0x00
  		Load_y(1)
  		lcall add32
	  	mov %4, x+0
		Display_Formated_BCD(%9,2,1) ;refer lab 2 code	
  		mov a, %4
		cjne a, #0xFF, Soak_Timer_Button_Jump%M
		mov %4, #0x00
		mov a, %4
		mov %8+0, a
  		mov %8+1, #0x00
  		mov %8+2, #0x00
  		mov %8+3, #0x00
		Display_Formated_BCD(%9,2,1)
  		
  		ljmp Soak_Timer_Button_Jump%M
	Soak_Timer_Button_Jump%M:
		ljmp Soak_Timer_Button%M
		
	Reflow_Timer_Button_Jump1%M:
		ljmp Reflow_Timer_Button%M
	
	change_reflow_temp%M:
		setb %2 ; Before using as input...
		jb %2, Reflow_Timer_Button_Jump1%M
		Wait_Milli_Seconds(#50) ; Wait and check again
		jb %2, Reflow_Timer_Button_Jump1%M
  
  
		mov a, %6
  		mov %8+0, a
  		mov %8+1, #0x00
  		mov %8+2, #0x00
  		mov %8+3, #0x00
  		Load_y(1)
  		lcall add32
	  	mov %6, x+0
		Display_Formated_BCD(%9,2,9) ;refer lab 2 code	
  		mov a, %6
  		
		cjne a, #0xFF, Reflow_Timer_Button_Jump2%M
		mov %6, #0x00
		mov a, %6
		mov %8+0, a
  		mov %8+1, #0x00
  		mov %8+2, #0x00
  		mov %8+3, #0x00
		Display_Formated_BCD(%9,2,9)
	
	Reflow_Timer_Button_Jump2%M:
		ljmp Reflow_Timer_Button%M
		
	Forever_Help_Loop%M:
		ljmp end_func%M
		
	Soak_Timer_Button%M:
		setb %3 ; Before using as input...
		jb %3, Forever_Help_Loop%M
		Wait_Milli_Seconds(#50) ; Wait and check again
		jb %3, Forever_Help_Loop%M
		; Wait for the button to be released
		
		mov a, %5
  		mov %8+0, a
  		mov %8+1, #0x00
  		mov %8+2, #0x00
  		mov %8+3, #0x00
  		Load_y(1)
  		lcall add32
	  	mov %5, x+0
		Display_Formated_BCD(%9,2,5) ;refer lab 2 code

		cjne a, #0xFF, end_func_jump%M
		mov %5, #0x00
		mov a, %5
		mov %8+0, a
  		mov %8+1, #0x00
  		mov %8+2, #0x00
  		mov %8+3, #0x00
		Display_Formated_BCD(%9,2,5)
		ljmp Reflow_Timer_Button%M
		
	end_func_jump%M:
		ljmp end_func%M
	Reflow_Timer_Button%M:
		mov a, #0x00
		setb %3 ; Before using as input...
		jb %3, end_func_jump%M
		Wait_Milli_Seconds(#50) ; Wait and check again
		jb %3, end_func_jump%M
		; Wait for the button to be released
	
		mov a, %7
  		mov %8+0, a
  		mov %8+1, #0x00
  		mov %8+2, #0x00
  		mov %8+3, #0x00
  		Load_y(1)
  		lcall add32
	  	mov %7, x+0
		Display_Formated_BCD(%9,2,13) ;refer lab 2 code
		cjne a, #0xFF, end_func%M
		mov %7, #0x00
		mov a, %7
		mov %8+0, a
  		mov %8+1, #0x00
  		mov %8+2, #0x00
  		mov %8+3, #0x00
		Display_Formated_BCD(%9,2,13)
	
	end_func%M:
		;Return back
endmac

initialize_program mac
	; Initialization
	
    mov %0, #0x7F
    mov %1, #0x00
    mov %2, #0x00
    mov %3, #0x00
    mov %4, #0x00
    mov %5, #0x00
    mov %6, #0x00
    
    mov %7, #0x00 ;SR_Flag
	
    setb %8   ; Enable Global interrupts
	;stop alarm timer
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
    
    mov %9, #0x00
    mov %11, #0x00
    mov %10, #0x00
    mov %12, #0x00
    ;mov S_R_flag, #0x0
    
        
    Set_Cursor(1, 1)
    Send_Constant_String(%13)
    Set_Cursor(2, 1)
    Send_Constant_String(%14)

	
    Load_x(0)
    Load_y(0)
    Display_Formated_BCD(%9,2,3)
	Set_Cursor(2,7) ;refer lab 2 code
	Display_BCD(%10) ;refer lab 2 code
	Display_Formated_BCD(%11,2,11)
	Set_Cursor(2,15) ;refer lab 2 code
	Display_BCD(%12) ;refer lab 2 code
endmac

Check_Temp mac
	; Read the 2.08V LED voltage connected to AIN0 on pin 6
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x00 ; Select channel 0

	lcall Read_ADC
	; Save result for later use
	mov %0+0, %8
	mov %0+1, %9

	; Read the signal connected to AIN1
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x01 ; Select channel 1
	lcall Read_ADC
    
    ; Convert to voltage
	mov %1+0, %8
	mov %1+1, %9
	; Pad other bits with zero
	mov %1+2, #0
	mov %1+3, #0
	Load_y(20740) ; The MEASURED LED voltage: 2.074V, with 4 decimal places
	lcall mul32
	; Retrive the ADC LED value
	mov %2+0, %0+0
	mov %2+1, %0+1
	; Pad other bits with zero
	mov %2+2, #0
	mov %2+3, #0
	lcall div32
	
	Load_y(10000)
	lcall mul32
	
	Load_y(99000)
	lcall div32
	
	Load_y(217)
	lcall mul32
	
	Load_y(41)
	lcall div32
	mov %10, x+1
	mov %3+0, %1+0
	mov %3+1, %1+1
	
	; Read the signal connected to AIN7
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x07 ; Select channel 7
	lcall Read_ADC
	
	
	; Convert to voltage
	mov %1+0, %8
	mov %1+1, %9
	; Pad other bits with zero
	mov %1+2, #0
	mov %1+3, #0
	Load_y(20740) ; The MEASURED LED voltage: 2.074V, with 4 decimal places
	lcall mul32
	; Retrive the ADC LED value
	mov %2+0, %0+0
	mov %2+1, %0+1
	; Pad other bits with zero
	mov %2+2, #0
	mov %2+3, #0
	lcall div32
	
	Load_y(27300)
	lcall sub32
	
	Copy_Data(%4,%1)
	
	mov %2+0, %3+0
	mov %2+1, %3+1
	
	lcall add32 
	
	mov %2+0, %1+0
	mov %2+1, %1+1
	mov %2+2, %1+2
	mov %2+3, %1+3
	
	mov %1+0, %7+0
	mov %1+1, %7+1
	mov %1+2, %7+2
	mov %1+3, %7+3
	
	lcall add32
	
	mov %7+0, %1+0
	mov %7+1, %1+1
	mov %7+2, %1+2
	mov %7+3, %1+3
	
	; Ambient Temp Summer
	mov %1+0, %5+0
	mov %1+1, %5+1
	mov %1+2, %5+2
	mov %1+3, %5+3
	
	mov %2+0, %4+0
	mov %2+1, %4+1
	mov %2+2, %4+2
	mov %2+3, %4+3
	
	lcall add32
	
	Copy_Data(%5,%1)
	
	djnz %6, end_func_jump%M
	ljmp continue_1%M
end_func_jump%M:
	ljmp end_func%M
continue_1%M:
	mov %1+0, %7+0
	mov %1+1, %7+1
	mov %1+2, %7+2
	mov %1+3, %7+3
	Load_y(100)
	lcall div32
	
	mov %7+0, #0
	mov %7+1, #0
	mov %7+2, #0
	mov %7+3, #0
	mov %6, #100
	; Convert to BCD and display 
	Display_formated_BCD_OvenTemp() ;Oven Temp
	
	Copy_Data(%1,%5) ;Ambient Temp
	Load_y(100)
	lcall div32
	Load_y(4620)
	lcall sub32
	Copy_Data(%5,#0)
	
	Display_formated_BCD_Ambient()
	end_func%M:
endmac
