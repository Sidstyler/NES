  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;; Enemy struct
	.rsset $0000
E_spriteId			.RS 1
E_moveRight			.RS 1
E_moveLeft			.RS 1
E_moveUp			.RS 1
E_moveDown			.RS 1
E_health			.RS 1
E_speed				.RS 1
EnemyStructSize	.RS 1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.rsset $0000 ;start variables at RAM location 0
buttons1						.RS 1 ; player 1 gamepad buttons
buttons2						.RS 1 ; player 2 gamepad buttons

marioSprite					.RS 1 ; marios start adress
marioCurrentSpeed		.RS 1 ; marios current move speed

tempVal1							.RS 1 ; variable used in stuff
tempVal2							.RS 1 ; variable used in stuff

FirstEnemy						.RS EnemyStructSize

BASE_SPR_ADDR		= $0200

MARIO_RUN_SPEED	= $03
MARIO_WALK_SPEED	= $02


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



LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 32
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
              
              
              
LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0
LoadBackgroundLoop:
  LDA background, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$80              ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down
              
              
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
  CPX #$08              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop  ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SET initial stats here 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  LDA $00
  STA marioSprite
	
  LDA #MARIO_WALK_SPEED
  STA marioCurrentSpeed
              
              
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  
  LDA #$10				; Second characters sprites start at 16 ( 4 sprites a 4 bytes )
  LDY #E_spriteId
  STA FirstEnemy,Y
  
  LDA #$00						; value to set on the variable we specify in the next line
  LDY #E_moveRight		; the offset ( aka variable we want to save to )
  STA FirstEnemy,Y		; Completing the variable set instruction using data specified above
  
  LDA #$00
  LDY #E_moveLeft
  STA FirstEnemy,Y
  
  LDA #$01
  LDY #E_speed
  STA FirstEnemy,Y
  

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
  
   ;;;all graphics updates done by here, run game engine
  

  JSR ReadController1 ; Get controller data for P1
  JSR ReadController2 ; Controller 2
	
  JSR UpdateNPC
	
  JSR SetMoveSpeed
	
  JSR UpdateSprites  ;;set ball/paddle sprites from positions
	
	
	RTI             ; return from interrupt
	

UpdateNPC:
  LDA #$00						; value to set on the variable we specify in the next line
  LDY #E_moveRight		; the offset ( aka variable we want to save to )
  STA FirstEnemy,Y		; Completing the variable set instruction using data specified above
  
  LDA #$00						; value to set on the variable we specify in the next line
  LDY #E_moveLeft			; the offset ( aka variable we want to save to )
  STA FirstEnemy,Y		; Completing the variable set instruction using data specified above
  
  LDA #$00						; value to set on the variable we specify in the next line
  LDY #E_moveUp			; the offset ( aka variable we want to save to )
  STA FirstEnemy,Y		; Completing the variable set instruction using data specified above
  
  LDA #$00						; value to set on the variable we specify in the next line
  LDY #E_moveDown			; the offset ( aka variable we want to save to )
  STA FirstEnemy,Y		; Completing the variable set instruction using data specified above


;HORIZONTAL MOVEMENT
  LDY #E_spriteId
  LDX FirstEnemy,Y	; save ID for FirstEnemy in X
  LDA BASE_SPR_ADDR+3, X ; load x value of FirstEnemy
  STA tempVal1 ; x val of FirstEnemy

  LDA BASE_SPR_ADDR+3
  STA tempVal2

  SEC	
  LDA tempVal1
  SBC tempVal2
  BEQ HorizontalMoveDone

  LDA tempVal2
  CMP tempVal1 
  BCC RightDone	 ;mario.X < than FirstEnemy.x?
  LDA #$01						; value to set on the variable we specify in the next line
  LDY #E_moveRight		; the offset ( aka variable we want to save to )
  STA FirstEnemy,Y		; Completing the variable set instruction using data specified above

  JMP HorizontalMoveDone
RightDone: ; mario.x < enemy.x
	
	LDA tempVal1
	CMP tempVal2
	BCC LeftDone	 
		LDA #$01						; value to set on the variable we specify in the next line
  	LDY #E_moveLeft		; the offset ( aka variable we want to save to )
  	STA FirstEnemy,Y		; Completing the variable set instruction using data specified above
LeftDone:
	JMP HorizontalMoveDone
	
	
	;	tempVal1 = enemy.x
	;	tempVal2 = mario.x
	SEC	
	LDA tempVal1
	SBC tempVal2
	
	BMI SetMoveRight
	;moveLeft
	LDA #$01						; value to set on the variable we specify in the next line
	LDY #E_moveLeft		; the offset ( aka variable we want to save to )
	STA FirstEnemy,Y		; Completing the variable set instruction using data specified above
	
	JMP HorizontalMoveDone
SetMoveRight:
		LDA #$01						; value to set on the variable we specify in the next line
  	LDY #E_moveRight		; the offset ( aka variable we want to save to )
  	STA FirstEnemy,Y		; Completing the variable set instruction using data specified above
HorizontalMoveDone:

;VERTICAL MOVEMENT
	LDY #E_spriteId
	LDX FirstEnemy, Y
	LDA BASE_SPR_ADDR, X
	STA tempVal1
	LDA BASE_SPR_ADDR
	STA tempVal2
	
	SEC					 
	LDA tempVal1  
	SBC tempVal2 
	BEQ VerticalMoveDone
	
	BMI SetMoveDown
	;move up
	LDA #$01
	LDY #E_moveUp
	STA FirstEnemy, Y
	JMP VerticalMoveDone
SetMoveDown:
	LDA #$01
	LDY #E_moveDown
	STA FirstEnemy, Y
VerticalMoveDone:
	RTS

;---------------TESTCODE----------------
;	CMP tempVal1 
;	BCC There	 ;mario.X < than FirstEnemy.x?
;There: ; mario.x < enemy.x
;-----/

ReadController1:
	LDA #$01
	STA $4016
	LDA #$00
	STA $4016
	LDX #$08
ReadController1Loop:
	LDA $4016
	LSR A 						;shift bits in A accumulator right. Bit0 -> carry
	ROL buttons1			; sshift bits in buttons1 left. carry is set as bit0
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
	
SetMoveSpeed:
	LDA #MARIO_WALK_SPEED
	STA marioCurrentSpeed
	
	LDA buttons1
	AND #%10000000
	
	BEQ ReadADone
	
	LDA #MARIO_RUN_SPEED
	STA marioCurrentSpeed
ReadADone:
	RTS

UpdateSprites:
	JSR UpdateMario
	JSR UpdateEnemies
	RTS

UpdateEnemies:
	LDY #E_moveRight
	LDA FirstEnemy,Y
	BEQ MoveRightDone
		LDY #E_spriteId
		LDA FirstEnemy, Y
		
		LDX #E_speed
		LDY FirstEnemy, X
		
		TAX
		JSR Move4SpriteRight
MoveRightDone:
	LDY #E_moveLeft
	LDA FirstEnemy,Y
	BEQ MoveLeftDone
		LDY #E_spriteId
		LDA FirstEnemy, Y
		
		LDX #E_speed
		LDY FirstEnemy, X
		
		TAX
		JSR Move4SpriteLeft
MoveLeftDone:
	
	LDY #E_moveDown
	LDA FirstEnemy, Y
	BEQ MoveDownDone
		LDY #E_spriteId
		LDA FirstEnemy, Y
		
		LDX #E_speed
		LDY FirstEnemy, X
		
		TAX
		JSR Move4SpriteDown
MoveDownDone:

	LDY #E_moveUp
	LDA FirstEnemy, Y
	BEQ MoveUpDone
		LDY #E_spriteId
		LDA FirstEnemy, Y
		
		LDX #E_speed
		LDY FirstEnemy, X
		
		TAX
		JSR Move4SpriteUp
MoveUpDone:
	RTS

UpdateMario:
	LDA buttons1
	AND #%00001000
	BEQ ReadUpDone
	
	LDX marioSprite
	LDY marioCurrentSpeed
	JSR Move4SpriteUp
ReadUpDone:
	
	LDA buttons1
	AND #%00000100
	BEQ ReadDownDone
	
	LDX marioSprite
	LDY marioCurrentSpeed
	JSR Move4SpriteDown
ReadDownDone:

	LDA buttons1
	AND #%00000010
	BEQ ReadLeftDone
	
	LDX marioSprite
	LDY marioCurrentSpeed
	
	JSR Move4SpriteLeft
ReadLeftDone:

	LDA buttons1
	AND #%00000001
	BEQ ReadRightDone
	
	LDX marioSprite
	LDY marioCurrentSpeed

	JSR Move4SpriteRight
ReadRightDone:
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
  .db $22,$16,$28,$18,  $22,$30,$28,$29,  $22,$1C,$15,$14,  $22,$02,$38,$3C    ;;sprite palette

sprites:
     ;vert tile attr horiz
  
  ;Mario
  .db $80, $32, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1
  .db $88, $34, $00, $80   ;sprite 2
  .DB $88, $35, $00, $88   ;sprite 3
  
  ;enemy
  .db $10, $32, $01, $20   ;sprite 0
  .db $10, $33, $01, $28   ;sprite 1
  .db $18, $38, $01, $20   ;sprite 2
  .DB $18, $39, $01, $28   ;sprite 3
  
  updateconstants:         ;constants for the use of the sprite_RAM constant           
  .db $00,$10,$20,$30      ;4 sprites for each meta sprite, so add $10 for each meta sprite we process


background:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 3
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 4
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;brick bottoms

attribute:
  .db %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000




  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "mario.chr"   ;includes 8KB graphics file from SMB1