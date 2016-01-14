  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  
;; Enemy struct
	.rsset $0000
E_spriteId					.RS 1
E_health						.RS 1
E_speed							.RS 1
E_moveFrames  			.RS 1
E_waitFrames  			.RS 1
E_hMove							.RS 1
E_vMove							.RS 1
E_currentAnimFrame 	.RS 1
EnemyStructSize			.RS 1

;hMove || 0 = none || 1 = left ||  2 = right
;vMove || 0 = none || 1 = up || 2 = down


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.rsset $0000 ;start variables at RAM location 0
buttons1						.RS 1 ; player 1 gamepad buttons
buttons2						.RS 1 ; player 2 gamepad buttons

checkVar						.RS 1

hDistance						.RS 1
vDistance						.RS 1

FrameCounter					.RS 1

arrayIndex					.RS 1
tempVal4						.RS 1
tempVal5						.RS 1

tempVal1							.RS 1 ; variable used in stuff
tempVal2							.RS 1 ; variable used in stuff´
tempVal3							.RS 1 ; variable used in stuff´

EnemyList						.RS EnemyStructSize * 10	

BASE_SPR_ADDR		= $0200

MARIO_RUN_SPEED	= $03
MARIO_WALK_SPEED	= $02

NUM_ENEMIES	= $04

AddrLow:  .rs 1
AddrHigh:  .rs 1


;;;;;;;;;;;;;;;

    
  .bank 0
  .ORG $C000 
  
vblankwait:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait
  
  RTS
  
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

  JSR vblankwait

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
  JSR vblankwait


LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down

	JSR LoadSprites
              
LoadBackground:
  LDA $2002
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
 
  LDA #low(background)
  STA AddrLow
  LDA #high(background)
  STA AddrHigh
 
  LDX #$04              ; Loop X 4 times
  LDY #$00              ; Loop Y 256 times
LoadBackgroundsLoop:
  LDA [AddrLow],y
  STA $2007
  INY
  BNE LoadBackgroundsLoop
; Outer loop
  INC AddrHigh           ; increment high byte of address backg to next 256 byte chunk
  DEX                    ; one chunk done so X = X - 1.
  BNE LoadBackgroundsLoop   ; if X isn't zero, do again
              
              
LoadAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA attribute, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$40              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop  ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SET initial stats here 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  
  
  
  JSR LoadCharacterData
  

Forever:
  JMP Forever     ;jump back to Forever, infinite loop
  
 
NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer
  
  
 ; JSR DrawScore

  
  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005
  
  JSR CheckForReset
  
   ;;;all graphics updates done by here, run game engine
  JSR UpdateFrameCounter

  JSR ReadController1 ; Get controller data for P1
  JSR ReadController2 ; Controller 2
	
  JSR UpdateNPC
	
  JSR SetMoveSpeed
	
  JSR UpdateMario  ;;Reads controller and moves mario if needed
  
  LDX #$00
  
  JSR UpdateAnimation
  
  JSR CheckCollision
  
  JSR showCPUUsageBar
  
	RTI             ; return from interrupt
	

LoadSprites:
  LDX #$00              ; start at 0
  
	LoadSpritesLoop:
  	LDA sprites, x        ; load data from address (sprites +  x)
 	 	STA $0200, x          ; store into RAM address ($0200 + x)
  	INX                   ; X = X + 1
  	CPX #$10 * ( NUM_ENEMIES  )           ; Compare X to hex $10, decimal 32
  	BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
  RTS

LoadCharacterData:

	LDX #$00
	setupEnemiesLoop:
		LDA enemyData, X
		STA EnemyList,X
		
		INX
		CPX #EnemyStructSize * NUM_ENEMIES
		
		BNE setupEnemiesLoop
	
	RTS
	
ResetGame:
	JSR LoadSprites
	JSR LoadCharacterData
	
	JSR ResetEnemyMovement
	
	RTS
	
showCPUUsageBar:
  ldx #%00011111  ; sprites + background + monochrome (i.e. WHITE)
  stx $2001
  ldy #21  ; add about 23 for each additional line (leave it on WHITE for one scan line)
.loop
    dey
    bne .loop
  dex    ; sprites + background + NO monochrome  (i.e. #%00011110)
  stx $2001
  RTS

UpdateFrameCounter:
	INC FrameCounter
  LDA #$3B
  CMP FrameCounter
  BEQ ResetCounter
  	JMP FrameCounterDone
ResetCounter:
  LDA #$00
  STA FrameCounter 
FrameCounterDone:
	RTS
	
CheckCollision:
	LDA #EnemyStructSize
	STA arrayIndex
	
	CheckCollisionLoop:
		LDA BASE_SPR_ADDR + 3 ; x-value
		STA tempVal1 ; playerX
	
		LDA BASE_SPR_ADDR ; Y-value
		STA tempVal2 ; playerY
	
		LDA arrayIndex
		CLC
		ADC #E_spriteId
		TAX
		
		LDA EnemyList, X
		TAY
		
		LDA BASE_SPR_ADDR + 3, Y ; x-value
		CLC
		ADC #$10
		
		SEC 
		SBC tempVal1
		;if( enemy.x + eenemy.width < player.x ) 
				;continue 
		BMI NoCollision
			
			LDA BASE_SPR_ADDR + 3, Y ; x-value
			TAX
			
			LDA tempVal1
			CLC 
			ADC #$10
			STA tempVal1
			
			TXA 
			SEC
			SBC tempVal1
			
		;	if( enemy.x > ( player.x + player.width ) ) 
		; 	continue
			BPL NoCollision
				
				LDA BASE_SPR_ADDR, Y ; x-value
				CLC
				ADC #$10
				
				SEC
				SBC tempVal2
				
				;if( enemy.y + enemy.height < player.y ) 
				;continue 
				BMI NoCollision
				
					LDA BASE_SPR_ADDR, Y ; x-value
					TAX
					
					LDA tempVal2
					CLC 
					ADC #$10
					STA tempVal2
					
					TXA 
					SEC
					SBC tempVal2
					
					;if( enemy.y > player.y + player.width ) 
					;continue
					BPL NoCollision
						;collision found! 
						JSR ResetGame
						RTS
		NoCollision:
	
		;Do the increment dance
		LDX arrayIndex
		TXA
		CLC
		ADC #EnemyStructSize
		TAX	
		STX arrayIndex
		CPX #EnemyStructSize * NUM_ENEMIES
		BNE CheckCollisionLoop

	RTS

; x - arrayIndex of character to update
UpdateAnimation:
	TXA ; arrayIndex
	CLC
	ADC #E_hMove
	TAY
	LDA EnemyList, Y
	BEQ NoHMove
		JSR PlayAnimation
	NoHMove: 
	
	TXA ; arrayIndex
	CLC
	ADC #E_vMove
	TAY
	LDA EnemyList, Y
	BEQ NoVMove
		JSR PlayAnimation
	NoVMove:
	RTS

UpdateNPC: 
	
	LDX #EnemyStructSize ;the first one is mario so we skip one index
	STX arrayIndex
	
	NPCUpdateLoop:
		
		JSR TickNPC
		
		JSR UpdateEnemies
		
		LDX arrayIndex
		
		JSR UpdateAnimation
		
		;Do the increment dance
		LDX arrayIndex
		TXA
		CLC
		ADC #EnemyStructSize
		TAX	
		STX arrayIndex
		CPX #EnemyStructSize * NUM_ENEMIES
		BNE NPCUpdateLoop
		
		RTS

TickNPC:	

	LDA arrayIndex
	CLC
	ADC #E_waitFrames
	TAY
	
	LDA EnemyList, Y
	BEQ TickMoveFrames
		;tick down wait frames
		SEC
		SBC #$01
		STA EnemyList, Y
		
		BNE NoUpdate
			;last frame of waiting, set move direction once
			JSR SetEnemyDirection
		NoUpdate:
	
		
		;Done updating the npc
		RTS
	
	
	TickMoveFrames:
	LDA arrayIndex
	CLC
	ADC #E_moveFrames
	TAY
	
	LDA EnemyList, Y
	
	CLC
	ADC #$01	
	
	STA EnemyList, Y ;store value in struct after increment
	
	CMP #$40
	BNE TickDone
		;last frame of movement
		;LDX arrayIndex; 
		;JSR PlayIdle
		
		LDA arrayIndex
		CLC
		ADC #E_waitFrames
		TAY
		
		; set how many frames to wait
		LDA #$78
		
		STA EnemyList, Y
		
		LDA arrayIndex
		CLC
		ADC #E_moveFrames
		TAY
		
		;Reset moveCOunter
		LDA #0
		
		STA EnemyList, Y
		
		JSR ResetEnemyMovement
TickDone:	
	RTS
	
ResetEnemyMovement:
	LDA #$00
	STA hDistance
	STA vDistance

  LDA arrayIndex
	CLC
	ADC #E_hMove
	TAY
	
	LDA #$00						; value to set on the variable we specify in the next line
  STA EnemyList,Y		; Completing the variable set instruction using data specified above
  
  
  LDA arrayIndex
	CLC
	ADC #E_vMove
	TAY
	
	LDA #$00						; value to set on the variable we specify in the next line
  STA EnemyList,Y		; Completing the variable set instruction using data specified above
  
  LDX #$0C
  STX tempVal4
  
	RTS
	
SetEnemyDirection:
	JSR ResetEnemyMovement

	JSR GetHorizontalValues
	
	BEQ HorizontalMoveDone

;;;;;;;;;;;;;;;;;;;;;;;;	
	BPL FoundHAbs
		LDA #0
		SEC
		SBC tempVal3
		
	FoundHAbs:
	STA hDistance
	LDA tempVal3
;;;;;;;;;;;;;;;;;;;;;;;

	BMI SetMoveRight
		
		LDA arrayIndex
		CLC
		ADC #E_hMove
		TAY
		
		;move left
		LDA #$01
		
		STA EnemyList, Y
		JMP HorizontalMoveDone
	SetMoveRight:
	
	LDA arrayIndex
	CLC
	ADC #E_hMove
	TAY
	
	LDA #$02
	
	STA EnemyList, Y
	
	HorizontalMoveDone:
	
	LDA arrayIndex
	CLC
	ADC #E_spriteId
	TAX
	
	LDA EnemyList, X
	TAX
	
	LDA arrayIndex
	CLC
	ADC #E_hMove
	TAY
	LDA EnemyList, Y
		
	LDY #$00
		
	SEC
	SBC #$01
		
	BNE NoNPCMirror
		LDY #$01
	NoNPCMirror:
		
	JSR MirrorSprite
	
	JSR GetVerticalValues

	BEQ VerticalMoveDone

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		BPL FoundVAbs
			LDA #0
			SEC
			SBC tempVal3
	FoundVAbs:
	STA vDistance
	LDA tempVal3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	LDA vDistance
	CMP hDistance
	BCS SetVerticalMovement ;branch when vDistance >= hDistance 
		RTS

SetVerticalMovement:
	LDA arrayIndex
	CLC
	ADC #E_hMove
	TAY
	
	LDA #$00						; value to set on the variable we specify in the next line
	STA EnemyList,Y		; Completing the variable set instruction using data specified above

	LDA tempVal3
	BMI SetMoveDown
	
	LDA arrayIndex
	CLC
	ADC #E_vMove
	TAY
	
	;move up
	LDA #$01
	STA EnemyList, Y
	
	JMP VerticalMoveDone
SetMoveDown:
	LDA arrayIndex
	CLC
	ADC #E_vMove
	TAY
	
	LDA #$02
	STA EnemyList, Y
VerticalMoveDone:
	RTS
	
	
;when done A contains enemyX - playerX
GetHorizontalValues:
	LDA arrayIndex
	CLC
	ADC #E_spriteId
	TAY
	
	LDX EnemyList, Y
	LDA BASE_SPR_ADDR+3, X ; enemies x value
	STA tempVal1
	LDA BASE_SPR_ADDR+3 ;the players X value
	STA tempVal2
	
	SEC					 
	LDA tempVal1  
	SBC tempVal2 
	STA tempVal3 ;tempVal3 is the result tempVal1 - tempVal2
	
	RTS
	
;when done A contains enemyY - playerY
GetVerticalValues:
	LDA arrayIndex
	CLC
	ADC #E_spriteId
	TAY
	
	LDX EnemyList, Y
	LDA BASE_SPR_ADDR, X
	STA tempVal1
	LDA BASE_SPR_ADDR
	STA tempVal2
	
	SEC					 
	LDA tempVal1  
	SBC tempVal2 
	STA tempVal3
	
	RTS


ReadController1:
	LDA #$01
	STA $4016
	LDA #$00
	STA $4016
	LDX #$08
ReadController1Loop:
	LDA $4016
	LSR A 						;shift bits in A accumulator right. Bit0 -> carry
	ROL buttons1			; shift bits in buttons1 left. carry is set as bit0
	DEX
	BNE ReadController1Loop
	RTS
	
ReadController2:
	LDA #$01
	STA $4016
	LDA #$00
	STA $4016
	LDX #$08
ReadController2Loop:
	LDA $4016
	LSR A 						;shift bits in A accumulator right. Bit0 -> carry
	ROL buttons2			;shift bits in buttons1 left. carry is set as bit0
	DEX
	BNE ReadController2Loop
	RTS
	
CheckForReset:
	LDA buttons1
	AND #%00100000
	
	BEQ ReadSelectDone
		 JSR ResetGame
		 JMP ReadSelectDone 
	
	ReadSelectDone:
	RTS
	
SetMoveSpeed:
	LDA #MARIO_WALK_SPEED
	
	LDY #E_speed
	STA EnemyList, Y
	
	LDA buttons1
	AND #%10000000
	
	BEQ ReadADone
	
		LDA #MARIO_RUN_SPEED
	
		LDY #E_speed
		STA EnemyList, Y
	
	ReadADone:
	RTS

UpdateEnemies:
	LDA arrayIndex
	CLC
	ADC #E_hMove
	TAY
	
	LDA EnemyList,Y
	CMP #$02
	BNE MoveRightDone
		LDA arrayIndex
		CLC
		ADC #E_spriteId
		TAY
		
		LDA EnemyList, Y
		TAX
		
		LDA arrayIndex
		CLC
		ADC #E_speed
		TAY
		
		LDA EnemyList, Y
		TAY
		
		JSR Move4SpriteRight
MoveRightDone:
	LDA arrayIndex
	CLC
	ADC #E_hMove
	TAY
	
	LDA EnemyList,Y
	CMP #$01
	BNE MoveLeftDone
		LDA arrayIndex
		CLC
		ADC #E_spriteId
		TAY
		
		LDA EnemyList, Y
		TAX
		
		LDA arrayIndex
		CLC
		ADC #E_speed
		TAY
		
		LDA EnemyList, Y
		TAY
		
		JSR Move4SpriteLeft
MoveLeftDone:
	
	LDA arrayIndex
	CLC
	ADC #E_vMove
	TAY
	
	LDA EnemyList, Y
	CMP #$01
	BEQ MoveDownDone
		LDA arrayIndex
		CLC
		ADC #E_spriteId
		TAY
		
		LDA EnemyList, Y
		TAX
		
		LDA arrayIndex
		CLC
		ADC #E_speed
		TAY
		
		LDA EnemyList, Y
		TAY
		
		JSR Move4SpriteDown
MoveDownDone:

	LDA arrayIndex
	CLC
	ADC #E_vMove
	TAY
	
	LDA EnemyList, Y
	CMP #$02
	BEQ MoveUpDone
		LDA arrayIndex
		CLC
		ADC #E_spriteId
		TAY
		
		LDA EnemyList, Y
		TAX
		
		LDA arrayIndex
		CLC
		ADC #E_speed
		TAY
		
		LDA EnemyList, Y
		TAY

		JSR Move4SpriteUp
MoveUpDone:
	RTS

UpdateMario:
	LDA #$00
	LDY #E_hMove
	STA EnemyList, Y ; reset marios moving variable
	
	LDA #$00
	LDY #E_vMove
	STA EnemyList, Y ; reset marios moving variable

	LDA buttons1
	AND #%00001000
	BEQ ReadUpDone
	
	LDA #$01
	LDY #E_vMove
	STA EnemyList, Y
	
	LDY #E_spriteId
	LDA EnemyList, Y
	TAX
	
	LDY #E_speed
	LDA EnemyList, Y
	TAY

	JSR Move4SpriteUp
ReadUpDone:
	
	LDA buttons1
	AND #%00000100
	BEQ ReadDownDone
	
	LDA #$02
	LDY #E_vMove
	STA EnemyList, Y
	
	LDY #E_spriteId
	LDA EnemyList, Y
	TAX
	
	LDY #E_speed
	LDA EnemyList, Y
	TAY

	JSR Move4SpriteDown
ReadDownDone:

	LDA buttons1
	AND #%00000010
	BEQ ReadLeftDone
		LDA #$00
		LDY #E_vMove
		STA EnemyList, Y
	
		LDA #$01
		LDY #E_hMove
		STA EnemyList, Y
		
		LDY #E_spriteId
		LDA EnemyList, Y
		TAX
		
		LDY #$01
		
		JSR MirrorSprite
		
		LDY #E_spriteId
		LDA EnemyList, Y
		TAX
		
		LDY #E_speed
		LDA EnemyList, Y
		TAY

		
	
		JSR Move4SpriteLeft
	ReadLeftDone:

	LDA buttons1
	AND #%00000001
	BEQ ReadRightDone
		LDA #$00
		LDY #E_vMove
		STA EnemyList, Y
		
		LDA #$02
		LDY #E_hMove
		STA EnemyList, Y
		
		LDY #E_spriteId
		LDA EnemyList, Y
		TAX
		LDY #$00
		
		JSR MirrorSprite
		
		LDY #E_spriteId
		LDA EnemyList, Y
		TAX
		
		LDY #E_speed
		LDA EnemyList, Y
		TAY

	
		JSR Move4SpriteRight
ReadRightDone:
	RTS
	
; A mod B
; tempVal1 = A
; tempVal2 = B
; result = Register A
Mod:
		LDA tempVal1  ; memory addr A
		SEC
Modulus:	
		SBC tempVal2 ; memory addr B
		BCS Modulus
		ADC tempVal2
	RTS

;X -register  = index in array
PlayAnimation:	
	STX tempVal4 				; index in array of character to update
	
	LDX FrameCounter 		;
	STX tempVal1				;
	
	LDX #$08						; only update every 8th frame
	STX tempVal2				;
	
	JSR Mod							;
	
	BEQ ContinueAnimation				; if the FrameCOunter % 8 is not zero we do not update
		RTS
	
	ContinueAnimation:
		LDX #$00
		
		LDA tempVal4
		CLC
		ADC #E_currentAnimFrame
		TAY 
		LDA EnemyList, Y
		CMP #$00
		BEQ DoAnimation
		
		CMP #$01
		BNE SetFrame2
		
	SetFrame2: 
		LDX #$04
		JMP DoAnimation
	
 	DoAnimation:
 		;choose animation 
 		
		LDA tempVal4
		CLC
		ADC #E_hMove
		TAY 
		LDA EnemyList, Y
		CMP #$00
		
		BEQ NoRunVAnimation
			TXA 
			ADC #$07
			TAX
			JMP NoRunUpAnim
		NoRunVAnimation:
		
		LDA tempVal4
		CLC
		ADC #E_vMove
		TAY 
		LDA EnemyList, Y
		CMP #$01
		BNE NoRunUpAnim
			TXA 
			ADC #$0F
			TAX
		NoRunUpAnim:
		
		
 	
 		;tempVal4 = arrayIndex
 		JSR RemoveMirrorBit ;returns the unmirrored attribute in Y-register
 		
 		STY tempVal1 ; Contains the unmirrored attribute now
 		
 		LDA tempVal4
		CLC
		ADC #E_spriteId
		TAY
		LDA EnemyList, Y
		TAY
 
		LDA linkAnimations,X
		STA BASE_SPR_ADDR+1, Y
		
		LDA tempVal1
		STA BASE_SPR_ADDR+2, Y
		INX
		
		LDA linkAnimations,X
		STA BASE_SPR_ADDR+5, Y
		
		LDA tempVal1
		STA BASE_SPR_ADDR+6, Y
		INX
		
		LDA linkAnimations,X
		STA BASE_SPR_ADDR+9, Y
		
		LDA tempVal1
		STA BASE_SPR_ADDR+10, Y
		INX
		
		LDA linkAnimations,X
		STA BASE_SPR_ADDR+13, Y
		
		LDA tempVal1
		STA BASE_SPR_ADDR+14, Y
		INX
		
		LDA tempVal4
		CLC
		ADC #E_spriteId
		TAY
		LDA EnemyList, Y
		TAX
		
		LDA tempVal4
		CLC
		ADC #E_hMove
		TAY
		LDA EnemyList, Y
		
		LDY #$00
		
		SEC
		SBC #$01
		
		BNE NoMirror
			LDY #$01
		NoMirror:
		
	  JSR MirrorSprite	
		
		LDA tempVal4
		CLC
		ADC #E_currentAnimFrame
		TAY
		LDA EnemyList, Y
		CLC
		ADC #$01
		STA EnemyList, Y
		
		CMP #$02									; CMP takes X - var
		BEQ ResetMoveAnimCounter  
			JMP AnimDone
		
		ResetMoveAnimCounter:
			LDA #$00
		  STA EnemyList, Y
		  JMP AnimDone
	AnimDone:
	RTS
	
;X -register  = index in array
PlayIdle:
	STX tempVal4
	
	;tempVal4 = arrayIndex
	JSR RemoveMirrorBit ; returns unmirrored attribute in Y-register
	STY tempVal1 ; Contains the unmirrored attribute now
	
	LDA tempVal4
	CLC
	
	
	ADC #E_spriteId
	TAY
	
	LDA EnemyList, Y
	TAY
	
	LDX #$00
	
	
	
	LDA linkAnimations,X
	STA BASE_SPR_ADDR+1, Y
	
	LDA tempVal1
	STA BASE_SPR_ADDR+2, Y
	INX
	
	LDA linkAnimations,X
	STA BASE_SPR_ADDR+5, Y
	
	LDA tempVal1
	STA BASE_SPR_ADDR+6, Y
	INX
	
	LDA linkAnimations,X
	STA BASE_SPR_ADDR+9, Y
	
	LDA tempVal1
	STA BASE_SPR_ADDR+10, Y
	INX
	
	LDA linkAnimations,X
	STA BASE_SPR_ADDR+13, Y
	
	LDA tempVal1
	STA BASE_SPR_ADDR+14, Y
	INX
	
	
	
	LDA tempVal4
	CLC
	ADC #E_spriteId
	TAY
	
	LDA EnemyList, Y
	TAX
	
	LDA tempVal4
	CLC
	ADC #E_hMove
	TAY
	LDA EnemyList, Y
	
	LDY #$00
	
	SEC
	SBC #$01
	
	BNE NoMirrorIdle
		LDY #$01
	NoMirrorIdle:
	
	JSR MirrorSprite	
	
	RTS

; tempVal4 = index in array
; returns new atribute in Y register
RemoveMirrorBit:
	LDA tempVal4
	CLC
	ADC #E_spriteId
	TAY
	LDA EnemyList, Y
	TAY
	
	LDA BASE_SPR_ADDR+2, Y
	ORA #$C0
	
	SEC
	SBC #$C0
	TAY
	
	RTS
	
;; X - Sprite number
;; Y - speed
Move4SpriteUp:
	LDA BASE_SPR_ADDR,X
		
	STY tempVal1	
	SEC
  SBC tempVal1										;move speed is -1 in Y
  
  TAY
  JSR UpdateSpriteV
	RTS 

;; X - Sprite number
;; Y - speed
Move4SpriteDown:
	LDA BASE_SPR_ADDR,X
		
	STY tempVal1	
	CLC
  ADC tempVal1										;move speed is 1 
  
  TAY
  JSR UpdateSpriteV
	RTS 
	
;; X - Sprite number
;; Y - speed
Move4SpriteRight:
	LDA BASE_SPR_ADDR+3,X
		
	STY tempVal1	
	CLC
  ADC tempVal1							;move speed is 1 
  
  TAY
  JSR UpdateSpriteH
	RTS 

;; X - Sprite number
;; Y - speed
Move4SpriteLeft:
	LDA BASE_SPR_ADDR+3,X
	
	STY tempVal1	
	SEC
  SBC tempVal1									;move speed is 1 
  
  TAY
  JSR UpdateSpriteH
	RTS 
	
; X - sprite ID
; Y - 0 = no mirror( right ), 1 = mirror ( left ) 
MirrorSprite:
	STY tempVal1
	
	LDA BASE_SPR_ADDR+2, X 
	AND #%01000000
	BEQ BaseNotMirrored ; do we have mirrored set already?
		; Yes
		TYA
		BEQ DoMirror ; continue if Y-value is opposite of current
			RTS
	
	BaseNotMirrored: ; no
		TYA
		BNE DoMirror 
			RTS
	
	DoMirror:
	

	LDA BASE_SPR_ADDR+9, X ; Tile
	PHA
	
	LDA BASE_SPR_ADDR+13, X
	PHA

	LDA BASE_SPR_ADDR+1, X
	PHA

	LDA BASE_SPR_ADDR+5, X
	PHA


	;Store done
	LDA BASE_SPR_ADDR+2,X ; get the attribute byte
	;set flip bit
	CPY #$00
	BNE LookLeft
		;look right
		AND #%10111111
		JMP FlipBitDone
	LookLeft:
		ORA #%01000000	
	FlipBitDone:
	
	STA tempVal1
	
	LDA tempVal1
	STA BASE_SPR_ADDR+2,X ; attribute
	PLA
	STA BASE_SPR_ADDR+1,X ; Tile

	LDA tempVal1
	STA BASE_SPR_ADDR+6,X
	PLA
	STA BASE_SPR_ADDR+5,X
	
	LDA tempVal1
	STA BASE_SPR_ADDR+10,X
	PLA
	STA BASE_SPR_ADDR+9,X
	
	LDA tempVal1
	STA BASE_SPR_ADDR+14,X
	PLA
	STA BASE_SPR_ADDR+13,X
	
	RTS

	
	
	
	
; X = The sprite offset	
; Y = new X value for sprite	
UpdateSpriteV:
	TYA
	
	STA BASE_SPR_ADDR,X				;Increments the y-value of the first sprite
  STA BASE_SPR_ADDR+4,X				;- Sprite #2 should have the same y-value as the first
  
  CLC												
  ADC #$08									; Sprite #3 and #4 will have sprite #1s Y value + 8
  STA BASE_SPR_ADDR+8,X				;-
  STA BASE_SPR_ADDR+12,X			;-
  
	RTS

;; X = The sprite offset	
;; Y - new Y value for sprite	
UpdateSpriteH:
	TYA

	STA BASE_SPR_ADDR+3,X				;Increments the y-value of the first sprite
  STA BASE_SPR_ADDR+11,X				;- Sprite #2 should have the same y-value as the first
  
  CLC												
  ADC #$08									; Sprite #3 and #4 will have sprite #1s Y value + 8
  STA BASE_SPR_ADDR+7,X				;-
  STA BASE_SPR_ADDR+15,X			;-
	
	RTS

;;;;;;;;;;;;;;  
  
  .bank 1
  .org $E000
palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$16,$29,$27,  $22,$16,$12,$27,  $22,$16,$30,$27,  $22,$16,$3F,$27    ;;sprite palette

sprites:
     ;vert tile attr horiz
  
  ;Mario
  .db $80, $00, $00, $80   ;sprite 0 head tile idle
  .db $80, $01, $00, $88   ;sprite 1 head tile idle
  .db $88, $10, $00, $80   ;sprite 2 foot idle
  .DB $88, $11, $00, $88   ;sprite 3 foot idle
  
  ;enemy
  .db $24, $00, $01, $20   ;sprite 0 head tile big step
  .db $24, $01, $01, $28   ;sprite 1 head tile big step
  .db $2B, $10, $01, $20   ;sprite 2 foot tile big step
  .DB $2B, $11, $01, $28   ;sprite 3 foot tile big step
  
  ;enemy
  .db $B0, $00, $02, $20   ;sprite 0 head tile feet together
  .db $B0, $01, $02, $28   ;sprite 1 head tile feet together
  .db $B8, $10, $02, $20   ;sprite 2 foot together
  .DB $B8, $11, $02, $28   ;sprite 3 foot together
  
  ;enemy
  .db $60, $00, $03, $C0   ;sprite 0 head tile small step
  .db $60, $01, $03, $C8   ;sprite 1 head tile big step
  .db $68, $10, $03, $C0   ;sprite 2 foot small step
  .DB $68, $11, $03, $C8   ;sprite 3 foot small step
  
  ;enemy
  .db $80, $00, $01, $40   ;sprite 0
  .db $80, $01, $01, $48   ;sprite 1
  .db $88, $10, $01, $40   ;sprite 2
  .DB $88, $11, $01, $48   ;sprite 3
  
  ;enemy
  .db $20, $00, $02, $40   ;sprite 0
  .db $20, $01, $02, $48   ;sprite 1
  .db $28, $10, $02, $40   ;sprite 2
  .DB $28, $11, $02, $48   ;sprite 3
  
linkAnimations:
	.db $00, $01, $10, $11 ;running down
	.DB $02, $03, $12, $13 ;running down
	
	.DB $0C, $0D, $1C, $1D ;running right
	.DB $0E, $0F, $1E, $1F ;running right
	
	.db $04, $05, $14, $15 ;running up
	.DB $06, $07, $16, $17 ;running up


;spriteindex, HP, speed, moveFrames, waitFrames, hMove, vMove, currentAnimFrame
enemyData:
	.DB $00, $00, $02, $00, $00, $00, $00, $00
	.DB $10, $00, $01, $00, $00, $00, $00, $00
	.DB $20, $00, $01, $00, $00, $00, $00, $00
	.DB $30, $00, $01, $00, $00, $00, $00, $00
	.DB $40, $00, $01, $20, $00, $00, $00, $00
	.DB $50, $00, $01, $50, $00, $00, $00, $00
	
	
background:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
  
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;row 2
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45 ;;all sky
 
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 3
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 1
 
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 8
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .DB $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47  ;;all sky
  
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 4
  .db $24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$24,$24,$24,$24,$45,$45  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$38,$24,$24,$24,$47,$47  ;;some brick tops
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 6
  .db $24,$24,$53,$54,$24,$24,$24,$39,$3A,$3B,$3C,$24,$24,$24,$45,$45  ;;brick bottoms
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 7
  .db $24,$24,$55,$56,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47  ;;all sky
 
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;row 1
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45 ;;all sky
 
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 1
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 1
 
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky



; räknat från vänster representerar två bitar attributet för ett mario-block
;3 2 ;
;1 0 ;
attribute:
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
	.db %00010001, %00000000, %11111111, %00010000, %00000000, %10101010, %10101010, %01000100
	
	.db %00010001, %00010000, %01010000, %00010000, %00000000, %10101010, %10101010, %01000100
	.db %00010001, %00010000, %01010000, %00010000, %00000000, %10100000, %10101010, %01000100
	
	.db %00010001, %00010000, %01010000, %00010000, %00000000, %10100000, %10101010, %01000100
	.db %00010001, %00010000, %01010000, %00010000, %00000000, %10100000, %10101010, %01000100
	
	.db %01010101, %01010000, %01010000, %01010000, %01010000, %01010000, %01010000, %01010101
	.db %00000101, %00000101, %00000101, %00000101, %00000101, %00000101, %00000101, %01010101

  .ORG $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "zelda.chr"   ;includes 8KB graphics file from SMB1