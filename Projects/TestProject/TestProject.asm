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

backgroundPtr						.RS 1

xIndex						.RS 1
yIndex						.RS 1

tempVal1							.RS 1 ; variable used in stuff
tempVal2							.RS 1 ; variable used in stuff�
tempVal3							.RS 1 ; variable used in stuff�

hDistance						.RS 1
vDistance						.RS 1

FrameCounter					.RS 1

arrayIndex					.RS 1
tempVal4						.RS 1
tempVal5						.RS 1



EnemyList						.RS EnemyStructSize * 10	

BASE_SPR_ADDR		= $0200

; World position of every enemy, indexed by sprite offset ($10, $20, ...).
; The sprite memory only holds what is visible on the current screen.
WORLD_X_LO	= $0300
WORLD_X_HI	= $0350 ; 0 = first screen, 1 = second screen
WORLD_Y		= $03A0
WORLD_DEAD	= $03F0 ; non-zero when the enemy has been removed

MARIO_RUN_SPEED	= $03
MARIO_WALK_SPEED	= $02

NUM_ENEMIES	= $05 ; Mario + 3 walkers + the charger

CHARGER_INDEX	= EnemyStructSize * 4 ; index in EnemyList
CHARGER_SPR	= $40 ; offset in the sprite memory
CHARGE_SPEED	= $04
CHARGE_WALK_SPEED	= $01 ; speed while lining up with Mario
CHARGE_ALIGN_MARGIN	= $03 ; y distance that counts as level
CHARGE_WAIT	= $3C ; frames to wait before charging (1 second)

AddrLow:  .rs 1
AddrHigh:  .rs 1

scrollX:  .rs 1 ; horizontal scroll, low 8 bits (0-255)
scrollNT: .rs 1 ; 0 or 1, the 9th scroll bit (selects the base nametable)

chargeState: .rs 1 ; 0 = turn towards player, 1 = wait, 2 = charge
chargeTimer: .rs 1
chargeH:     .rs 1 ; charge direction, same values as E_hMove
chargeV:     .rs 1 ; charge direction, same values as E_vMove
chargeOldX:  .rs 1
chargeOldY:  .rs 1

worldMode:   .rs 1 ; 1 while an enemy is updated in world coordinates (sprite x = world x low byte)
worldNT:     .rs 1 ; screen (0/1) of the enemy being updated
probeNT:     .rs 1 ; screen to probe for collision, $FF = use worldNT

SCROLL_THRESHOLD = $80 ; Mario stops at this screen x and the world scrolls instead


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
  ; first screen -> nametable 0 ($2000), attributes at $23C0
  LDA #$20
  LDX #low(background)
  LDY #high(background)
  JSR LoadNametable

  ; second screen -> nametable 1 ($2400), attributes at $27C0
  LDA #$24
  LDX #low(background2)
  LDY #high(background2)
  JSR LoadNametable


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
  ORA scrollNT     ; base nametable (9th scroll bit)
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA scrollX     ;;horizontal background scroll
  STA $2005
  LDA #$00        ;;no vertical scroll
  STA $2005
  
  JSR CheckForReset
  
   ;;;all graphics updates done by here, run game engine
  JSR UpdateFrameCounter

  JSR ReadController1 ; Get controller data for P1
  JSR ReadController2 ; Controller 2
	
  JSR UpdateNPC
  
  JSR UpdateCharger

  JSR SetMoveSpeed
	
  JSR UpdateMario  ;;Reads controller and moves mario if needed
  
  LDX #$00
  
  JSR UpdateAnimation
  
  JSR RenderEnemies
  
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

                          ; the enemies start out at their sprite positions in the world
                          LDX #$10
                          InitWorldLoop:
                            LDA BASE_SPR_ADDR+3, X
                            STA WORLD_X_LO, X
                            LDA BASE_SPR_ADDR, X
                            STA WORLD_Y, X
                            LDA #$00
                            STA WORLD_X_HI, X
                            STA WORLD_DEAD, X
    TXA
                            CLC
                            ADC #$10
                            TAX
                            CPX #$10 * NUM_ENEMIES
                            BNE InitWorldLoop
                          LDA #$00
                          STA worldMode
                          STA worldNT
                          LDA #$FF
                          STA probeNT
                          RTS

; A - high byte of the nametable address ($20 or $24)
; X/Y - low/high byte of the 1024 byte nametable data
LoadNametable:
  STX AddrLow
  STY AddrHigh
  PHA
  BIT $2002             ; reset the PPU high/low latch
  STA $2006
  LDA #$00
  STA $2006
  LDX #$04              ; 4 pages of 256 bytes
  LDY #$00
.tileLoop:
  LDA [AddrLow],y
  STA $2007
  INY
  BNE .tileLoop
  INC AddrHigh
  DEX
  BNE .tileLoop

  PLA
  CLC
  ADC #$03              ; attribute table is at nametable + $3C0
  BIT $2002
  STA $2006
  LDA #$C0
  STA $2006
  LDX #$00
.attrLoop:
  LDA attribute, x
  STA $2007
  INX
  CPX #$40
  BNE .attrLoop
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
	
; The charger turns towards Mario, waits a second and then charges in a straight line
; until it hits a wall (hitting Mario is handled by CheckCollision)
UpdateCharger:
	LDX #CHARGER_SPR
	JSR EnterWorld
	JSR ChargerLogic
	LDX #CHARGER_SPR
	JSR ExitWorld
	RTS

ChargerLogic:
	LDA chargeState
	BNE .NotAlign
		JMP .Turn
	.NotAlign:
	CMP #$01
	BNE .NotWait
		JMP .Wait
	.NotWait:
	JMP .Charge
	
	.Turn:
		; first walk up/down until we are level with Mario
		LDA #CHARGE_WALK_SPEED
		STA EnemyList + CHARGER_INDEX + E_speed
		LDA #$00
		STA EnemyList + CHARGER_INDEX + E_hMove
		
		LDA BASE_SPR_ADDR + CHARGER_SPR
		STA chargeOldY
		SEC
		SBC BASE_SPR_ADDR ; Mario's y
		BEQ .Aligned
		BCC .MarioBelow
			; Mario is above
			CMP #CHARGE_ALIGN_MARGIN
			BCC .Aligned
			LDA #$01
			STA EnemyList + CHARGER_INDEX + E_vMove
			LDX #CHARGER_INDEX
			JSR Move4SpriteUp
			JMP .AlignStep
		.MarioBelow:
			EOR #$FF
			CLC
			ADC #$01
			CMP #CHARGE_ALIGN_MARGIN
			BCC .Aligned
			LDA #$02
			STA EnemyList + CHARGER_INDEX + E_vMove
			LDX #CHARGER_INDEX
			JSR Move4SpriteDown
		.AlignStep:
		LDA BASE_SPR_ADDR + CHARGER_SPR
		CMP chargeOldY
		BEQ .Aligned ; blocked by a wall, charge from here
		LDX #CHARGER_INDEX
		JSR UpdateAnimation
		RTS
		
	.Aligned:
		LDA #$00
		STA EnemyList + CHARGER_INDEX + E_vMove
		STA chargeV
		
		; face Mario horizontally
		LDA #CHARGER_INDEX
		STA arrayIndex
		JSR GetHorizontalValues ; charger x - Mario x in world coordinates
		BMI .FaceRight
			LDA #$01
			LDY #$01
			JMP .SetFacing
		.FaceRight:
			LDA #$02
			LDY #$00
		.SetFacing:
		STA chargeH
		STA EnemyList + CHARGER_INDEX + E_hMove
		LDX #CHARGER_SPR
		JSR MirrorSprite
		
		; show the facing direction right away: force an animation update
		LDA FrameCounter
		PHA
		LDA #$00
		STA FrameCounter
		LDX #CHARGER_INDEX
		JSR UpdateAnimation
		PLA
		STA FrameCounter
		
		LDA #$00
		STA EnemyList + CHARGER_INDEX + E_hMove
		STA EnemyList + CHARGER_INDEX + E_vMove
		
		LDA #CHARGE_WAIT
		STA chargeTimer
		LDA #$01
		STA chargeState
		RTS
	
	.Wait:
		DEC chargeTimer
		BNE .WaitDone
			LDA chargeH
			STA EnemyList + CHARGER_INDEX + E_hMove
			LDA chargeV
			STA EnemyList + CHARGER_INDEX + E_vMove
			LDA #CHARGE_SPEED
			STA EnemyList + CHARGER_INDEX + E_speed
			LDA #$02
			STA chargeState
		.WaitDone:
		RTS
	
	.Charge:
		; stop at the edge of the visible screen
		LDA BASE_SPR_ADDR + CHARGER_SPR + 3
		SEC
		SBC scrollX
		STA tempVal1 ; screen x
		LDA worldNT
		SBC scrollNT
		BNE .ScreenEdge
		LDA chargeH
		CMP #$01
		BNE .NotLeftEdge
			LDA tempVal1
			CMP #CHARGE_SPEED
			BCC .ScreenEdge
		.NotLeftEdge:
		LDA chargeH
		CMP #$02
		BNE .NotRightEdge
			LDA tempVal1
			CMP #$F0 - CHARGE_SPEED + 1
			BCS .ScreenEdge
		.NotRightEdge:
		JMP .DoCharge
		.ScreenEdge:
			LDA #$00
			STA EnemyList + CHARGER_INDEX + E_hMove
			STA EnemyList + CHARGER_INDEX + E_vMove
			STA chargeState
			RTS
		.DoCharge:
		LDA BASE_SPR_ADDR + CHARGER_SPR + 3
		STA chargeOldX
		LDA BASE_SPR_ADDR + CHARGER_SPR
		STA chargeOldY
		
		LDA chargeH
		CMP #$01
		BNE .NotLeft
			LDX #CHARGER_INDEX
			JSR Move4SpriteLeft
		.NotLeft:
		LDA chargeH
		CMP #$02
		BNE .NotRight
			LDX #CHARGER_INDEX
			JSR Move4SpriteRight
		.NotRight:
		LDA chargeV
		CMP #$01
		BNE .NotUp
			LDX #CHARGER_INDEX
			JSR Move4SpriteUp
		.NotUp:
		LDA chargeV
		CMP #$02
		BNE .NotDown
			LDX #CHARGER_INDEX
			JSR Move4SpriteDown
		.NotDown:
		
		; did it move?
		LDA BASE_SPR_ADDR + CHARGER_SPR + 3
		CMP chargeOldX
		BNE .Moved
		LDA BASE_SPR_ADDR + CHARGER_SPR
		CMP chargeOldY
		BNE .Moved
			; hit a wall, stop and turn towards Mario again
			LDA #$00
			STA EnemyList + CHARGER_INDEX + E_hMove
			STA EnemyList + CHARGER_INDEX + E_vMove
			STA chargeState
			RTS
		.Moved:
		
		JSR ChargerHitCheck
		BEQ .NoEnemyHit
			; ran into another enemy, it's removed and the charger stops
			LDA #$00
			STA EnemyList + CHARGER_INDEX + E_hMove
			STA EnemyList + CHARGER_INDEX + E_vMove
			STA chargeState
			RTS
		.NoEnemyHit:
		
		LDX #CHARGER_INDEX
		JSR UpdateAnimation
		RTS

; Z set when the enemy at arrayIndex is still alive
EnemyDeadCheck:
	LDA arrayIndex
	CLC
	ADC #E_spriteId
	TAY
	LDX EnemyList, Y
	LDA WORLD_DEAD, X
	RTS

; Removes the walkers the charger runs into. Returns A = 1 on a hit
ChargerHitCheck:
	LDX #$10
	LDY #$00 ; hit count
	.HitLoop:
		LDA WORLD_DEAD, X
		BNE .NextHit
		
		; only enemies that are completely on screen can be hit
		LDA WORLD_X_LO, X
		SEC
		SBC scrollX
		STA tempVal1
		LDA WORLD_X_HI, X
		SBC scrollNT
		BNE .NextHit
		LDA tempVal1
		CMP #$F1
		BCS .NextHit
		
		; horizontal distance in world coordinates, must be within -15..15
		LDA BASE_SPR_ADDR + CHARGER_SPR + 3
		SEC
		SBC WORLD_X_LO, X
		STA tempVal1
		LDA worldNT
		SBC WORLD_X_HI, X
		BEQ .XPos
		CMP #$FF
		BNE .NextHit
		LDA tempVal1
		CMP #$F1
		BCC .NextHit
		JMP .XClose
		.XPos:
		LDA tempVal1
		CMP #$10
		BCS .NextHit
		.XClose:
		
		; vertical distance, must be within -15..15
		LDA BASE_SPR_ADDR + CHARGER_SPR
		SEC
		SBC WORLD_Y, X
		BCS .YPos
		CMP #$F1
		BCC .NextHit
		JMP .YClose
		.YPos:
		CMP #$10
		BCS .NextHit
		.YClose:
		
		LDA #$01
		STA WORLD_DEAD, X
		INY
		
		.NextHit:
		TXA
		CLC
		ADC #$10
		TAX
		CPX #CHARGER_SPR
		BNE .HitLoop
	TYA
	RTS

ResetGame:
	LDA #$00
	STA scrollX
	STA scrollNT
	STA chargeState
	
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
	
; X - XValue
; Y - yValue
; return
; A = collision boolean
CheckBGCollision:
	LDA worldMode
	BNE .WorldPos
	; world x = screen x + scroll, bit 8 of it selects the nametable
	TXA
	CLC
	ADC scrollX
	STA tempVal1
	LDA #$00
	ADC scrollNT
	AND #$01
	STA tempVal2 ; 0 = first screen, 1 = second screen
	JMP .HavePos
	.WorldPos:
	; an enemy: x is already the world x, the screen is given
	STX tempVal1
	LDA probeNT
	BPL .UseProbe
		LDA worldNT
	.UseProbe:
	STA tempVal2
	LDA #$FF
	STA probeNT
	.HavePos:
	
	LDA tempVal1
	LSR A
	LSR A
	LSR A
	STA xIndex ; tile column
	
	TYA
	LSR A
	LSR A
	LSR A
	STA yIndex ; tile row
	
	; offset = row * 32 + column
	AND #$07
	ASL A
	ASL A
	ASL A
	ASL A
	ASL A
	ORA xIndex
	STA AddrLow
	LDA yIndex
	LSR A
	LSR A
	LSR A
	STA AddrHigh
	
	LDA tempVal2
	BNE .secondScreen
		LDA #low(background)
		LDX #high(background)
		JMP .addBase
	.secondScreen:
		LDA #low(background2)
		LDX #high(background2)
	.addBase:
	CLC
	ADC AddrLow
	STA AddrLow
	TXA
	ADC AddrHigh
	STA AddrHigh
	
	LDY #$00
	LDA [AddrLow],y
	STA checkVar ; the background tile at this position
	
		LDX #$00
  collisionLoop:
  
  LDA collisionData,X
  
  CMP checkVar
  BEQ DidCollide
  	INX
  	CPX #$02
  	BNE collisionLoop
  	JMP NoCollide

  DidCollide:
  	LDA #$01
  	JMP CollisionDone
  NoCollide:
  	LDA #$00
  
  CollisionDone:
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
		
		LDA BASE_SPR_ADDR, Y
		CMP #$F8
		BNE .EnemyVisible
			JMP NoCollision ; hidden, off screen
		.EnemyVisible:
		
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
		
		JSR EnemyDeadCheck
		BEQ .EnemyAlive
			JMP .NextNPC
		.EnemyAlive:
		
		JSR EnterEnemy
		
		JSR TickNPC
		
		JSR UpdateEnemies
		
		LDX arrayIndex
		
		JSR UpdateAnimation
		
		JSR ExitEnemy
		
		.NextNPC:
		;Do the increment dance
		LDX arrayIndex
		TXA
		CLC
		ADC #EnemyStructSize
		TAX	
		STX arrayIndex
		CPX #EnemyStructSize * ( NUM_ENEMIES - 1 ) ; the charger is updated separately
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
	
	
;when done A contains enemyX - playerX (world coordinates, limited to -128..127)
GetHorizontalValues:
	LDA arrayIndex
	CLC
	ADC #E_spriteId
	TAY
	
	LDX EnemyList, Y
	LDA BASE_SPR_ADDR+3, X ; enemies world x low byte
	STA tempVal1
	LDA BASE_SPR_ADDR+3 ; the players screen x
	CLC
	ADC scrollX
	STA tempVal2 ; the players world x low byte
	LDA scrollNT
	ADC #$00
	STA tempVal5 ; the players world x high byte
	
	SEC					 
	LDA tempVal1  
	SBC tempVal2 
	STA tempVal3
	LDA worldNT
	SBC tempVal5 ; high byte of the difference
	BEQ .HiZero
	BMI .HiNeg
		LDA #$7F
		STA tempVal3
		JMP .HDone
	.HiNeg:
		LDA tempVal3
		BMI .HDone
		LDA #$80
		STA tempVal3
		JMP .HDone
	.HiZero:
		LDA tempVal3
		BPL .HDone
		LDA #$7F
		STA tempVal3
	.HDone:
	LDA tempVal3 ; sets the flags for the caller
	RTS

; X = sprite offset. Loads the enemy's world position into the sprite memory
EnterWorld:
	LDA WORLD_X_LO, X
	STA BASE_SPR_ADDR+3, X
	LDA WORLD_Y, X
	STA BASE_SPR_ADDR, X
	LDA WORLD_X_HI, X
	STA worldNT
	LDA #$01
	STA worldMode
	RTS

; X = sprite offset. Stores the updated world position
ExitWorld:
	LDA BASE_SPR_ADDR+3, X
	STA WORLD_X_LO, X
	LDA BASE_SPR_ADDR, X
	STA WORLD_Y, X
	LDA worldNT
	STA WORLD_X_HI, X
	LDA #$00
	STA worldMode
	RTS

EnterEnemy:
	LDA arrayIndex
	CLC
	ADC #E_spriteId
	TAY
	LDX EnemyList, Y
	JMP EnterWorld

ExitEnemy:
	LDA arrayIndex
	CLC
	ADC #E_spriteId
	TAY
	LDX EnemyList, Y
	JMP ExitWorld

; Places the enemies on the screen from their world position and the scroll
; Enemies that are not completely on screen are hidden
RenderEnemies:
	LDX #$10
	.RenderLoop:
		LDA WORLD_DEAD, X
		BNE .Hide
		LDA WORLD_X_LO, X
		SEC
		SBC scrollX
		STA tempVal1
		LDA WORLD_X_HI, X
		SBC scrollNT
		BNE .Hide
		LDA tempVal1
		CMP #$F1
		BCS .Hide
			TAY
			JSR UpdateSpriteH
			LDY WORLD_Y, X
			JSR UpdateSpriteV
			JMP .RenderNext
		.Hide:
			LDA #$F8
			STA BASE_SPR_ADDR, X
			STA BASE_SPR_ADDR+4, X
			STA BASE_SPR_ADDR+8, X
			STA BASE_SPR_ADDR+12, X
		.RenderNext:
		TXA
		CLC
		ADC #$10
		TAX
		CPX #$10 * NUM_ENEMIES
		BNE .RenderLoop
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
	
; X - arrayIndex
MoveMetaSprite:
	TXA 
	
	CLC
	ADC #E_hMove
	TAY
	
	LDA EnemyList,Y
	CMP #$02
	BNE MoveRightDone
		JSR Move4SpriteRight
	RTS

UpdateEnemies:
	LDA arrayIndex
	CLC
	ADC #E_hMove
	TAY
	
	LDA EnemyList,Y
	CMP #$02
	BNE MoveRightDone
		LDX arrayIndex
		JSR Move4SpriteRight
		
MoveRightDone:
	LDA arrayIndex
	CLC
	ADC #E_hMove
	TAY
	
	LDA EnemyList,Y
	CMP #$01
	BNE MoveLeftDone
		LDX arrayIndex
		
		JSR Move4SpriteLeft
MoveLeftDone:
	
	LDA arrayIndex
	CLC
	ADC #E_vMove
	TAY
	
	LDA EnemyList, Y
	CMP #$01
	BEQ MoveDownDone
		LDX arrayIndex
		JSR Move4SpriteDown
MoveDownDone:

	LDA arrayIndex
	CLC
	ADC #E_vMove
	TAY
	
	LDA EnemyList, Y
	CMP #$02
	BEQ MoveUpDone
		LDX arrayIndex
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
	
	LDX #$00
	JSR Move4SpriteUp
ReadUpDone:
	
	LDA buttons1
	AND #%00000100
	BEQ ReadDownDone
	
	LDA #$02
	LDY #E_vMove
	STA EnemyList, Y
	
	LDX #$00
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
		
		LDX #$00
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
		
		LDX #$00
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
	
;X - arrayIndex
Move4SpriteUp:
	JSR GetSpriteAndSpeed
	
	LDA BASE_SPR_ADDR,X ; old y value
	
	
	STY tempVal3	; speed
	SEC
  SBC tempVal3		
  STA tempVal2 ; new yValue	
  
  TXA					; sprite ID
  PHA
  LDA tempVal2 ; new yValue
  PHA
  
  
  LDA BASE_SPR_ADDR+3,X
  TAX
  LDY tempVal2
  
  JSR CheckBGCollision
  STA tempVal3
  
  PLA 
  STA tempVal2
  PLA
  STA tempVal1
  
  LDA tempVal3
  BEQ .NoUpCollide
  	RTS
  
  .NoUpCollide:
	
  LDX tempVal1
  LDY tempVal2

  JSR UpdateSpriteV
	RTS 

;; X - arrrayIndex
Move4SpriteDown:
	JSR GetSpriteAndSpeed
	
	LDA BASE_SPR_ADDR,X ; old y value
	CLC
	ADC #$10 ;offset 16px right
	
	
	STY tempVal3	; speed
	CLC
  ADC tempVal3		
  STA tempVal2 ; new yValue	
  
  TXA					; sprite ID
  PHA
  LDA tempVal2 ; new yValue
  PHA
  
  
  LDA BASE_SPR_ADDR+3,X
  TAX
  LDY tempVal2
  
  JSR CheckBGCollision
  STA tempVal3
  
  PLA 
  STA tempVal2
  PLA
  STA tempVal1
  
  LDA tempVal3
  BEQ .NoDownCollide
  	RTS
  
  .NoDownCollide:
	
  LDX tempVal1
  LDY tempVal2
  TYA
  SEC
  SBC #$10
  TAY
  
  
  JSR UpdateSpriteV
	RTS 
	
;; X arrayIndex
Move4SpriteRight:
  JSR GetSpriteAndSpeed
	
	LDA BASE_SPR_ADDR+3,X ; old x value
	
	STY tempVal3	; speed
	TYA
	CLC
	ADC #$10 ; the right edge is 16px right
	CLC
	ADC BASE_SPR_ADDR+3,X
	STA tempVal2 ; new right edge
	BCC .RightOnScreen
		LDA worldMode
		BEQ .BlockRight ; Mario can't leave the screen
		LDA worldNT
		BNE .BlockRight ; the end of the world
		LDA #$01
		STA probeNT ; the right edge is on the second screen
		JMP .RightOnScreen
	.BlockRight:
		RTS
	.RightOnScreen:
  
  TXA					; sprite ID
  PHA
  LDA tempVal2 ; new XValue
  PHA
  
  
  LDA BASE_SPR_ADDR,X
  TAY
  LDX tempVal2
  
  JSR CheckBGCollision
  STA tempVal3
  
  PLA 
  STA tempVal2
  PLA
  STA tempVal1
  
  LDA tempVal3
  BEQ .NoRightCollide
  	RTS
  
  .NoRightCollide:
	
  LDX tempVal1
  LDY tempVal2
  TYA
  SEC
  SBC #$10
  TAY
  
  ; an enemy whose left edge wrapped around moved to the second screen
  TYA
  CMP BASE_SPR_ADDR+3,X
  BCS .NoRightCross
  	LDA #$01
  	STA worldNT
  .NoRightCross:
  
  ; Mario (sprite 0) past the scroll threshold: scroll the world instead of moving him
  CPX #$00
  BNE .MoveRight
  CPY #SCROLL_THRESHOLD + 1
  BCC .MoveRight
  LDA scrollNT
  BNE .MoveRight
  	TYA
  	SEC
  	SBC #SCROLL_THRESHOLD
  	STA tempVal3 ; scroll amount
  	LDY #SCROLL_THRESHOLD
  	JSR UpdateSpriteH
  	JSR ScrollWorld
  	RTS
  .MoveRight:
  
  JSR UpdateSpriteH
	RTS 

; tempVal3 = pixels to scroll right
ScrollWorld:
	LDA scrollX
	CLC
	ADC tempVal3
	STA scrollX
	BCC .ScrollDone
		; reached the end of the second screen
		LDA #$00
		STA scrollX
		LDA #$01
		STA scrollNT
	.ScrollDone:
	RTS

;; X -array index
Move4SpriteLeft:
	JSR GetSpriteAndSpeed
	
	LDA BASE_SPR_ADDR+3,X ; old x value
	
	
	STY tempVal3	; speed
	SEC
  SBC tempVal3		
  STA tempVal2 ; new xValue
  BCS .LeftOnScreen
  	LDA worldMode
  	BEQ .BlockLeft ; Mario can't leave the screen
  	LDA worldNT
  	BEQ .BlockLeft ; the start of the world
  	LDA #$00
  	STA probeNT ; moving back to the first screen
  	JMP .LeftOnScreen
  .BlockLeft:
  	RTS
  .LeftOnScreen:
  
  TXA					; sprite ID
  PHA
  LDA tempVal2 ; new XValue
  PHA
  
  
  LDA BASE_SPR_ADDR,X
  TAY
  LDX tempVal2
  
  JSR CheckBGCollision
  STA tempVal3
  
  PLA 
  STA tempVal2
  PLA
  STA tempVal1
  
  LDA tempVal3
  BEQ .NoLeftCollide
  	RTS
  
  .NoLeftCollide:
	
  LDX tempVal1
  LDY tempVal2
  
  ; an enemy whose x wrapped around moved back to the first screen
  TYA
  CMP BASE_SPR_ADDR+3,X
  BCC .NoLeftCross
  BEQ .NoLeftCross
  	LDA #$00
  	STA worldNT
  .NoLeftCross:
  
  ; Mario (sprite 0) left of the scroll threshold while scrolled: scroll back instead of moving him
  CPX #$00
  BNE .MoveLeft
  CPY #SCROLL_THRESHOLD
  BCS .MoveLeft
  LDA scrollNT
  ORA scrollX
  BEQ .MoveLeft
  	STY tempVal3
  	LDA #SCROLL_THRESHOLD
  	SEC
  	SBC tempVal3
  	STA tempVal3 ; scroll amount
  	LDY #SCROLL_THRESHOLD
  	JSR UpdateSpriteH
  	JSR ScrollWorldLeft
  	RTS
  .MoveLeft:
  
  JSR UpdateSpriteH
	RTS 

; tempVal3 = pixels to scroll left
ScrollWorldLeft:
	LDA scrollNT
	BEQ .SubLow
		; at the far right: wrap back into the first screen
		LDA #$00
		SEC
		SBC tempVal3
		STA scrollX
		LDA #$00
		STA scrollNT
		RTS
	.SubLow:
	LDA scrollX
	SEC
	SBC tempVal3
	BCS .StoreLow
		LDA #$00 ; don't scroll past the start
	.StoreLow:
	STA scrollX
	RTS
	
;X - arrayIndex
;returns
; X - spriteID
; Y - speed
GetSpriteAndSpeed:
	TXA
	CLC
	ADC #E_spriteId
	TAY
	
	LDA EnemyList, Y
	STA tempVal1 ;sprite ID
	
	TXA
	CLC
	ADC #E_speed
	TAY
	
	LDA EnemyList, Y
	TAY							; Y = speed

	LDX tempVal1
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

	
	
	
	
; X = The sprite ID
; Y = new Y value for sprite	
UpdateSpriteV:
	TYA
	
	STA BASE_SPR_ADDR,X				;Increments the y-value of the first sprite
  STA BASE_SPR_ADDR+4,X				;- Sprite #2 should have the same y-value as the first
  
  CLC												
  ADC #$08									; Sprite #3 and #4 will have sprite #1s Y value + 8
  STA BASE_SPR_ADDR+8,X				;-
  STA BASE_SPR_ADDR+12,X			;-
  
	RTS

;; X = The sprite ID
;; Y - new X value for sprite	
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
  .db $22,$16,$29,$27,  $22,$16,$12,$27,  $22,$16,$30,$27,  $22,$16,$15,$27    ;;sprite palette

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
  .db $60, $00, $02, $C0   ;sprite 0 head tile small step
  .db $60, $01, $02, $C8   ;sprite 1 head tile big step
  .db $68, $10, $02, $C0   ;sprite 2 foot small step
  .DB $68, $11, $02, $C8   ;sprite 3 foot small step
  
  ;enemy
  .db $A0, $00, $03, $D0   ;sprite 0
  .db $A0, $01, $03, $D8   ;sprite 1
  .db $A8, $10, $03, $D0   ;sprite 2
  .DB $A8, $11, $03, $D8   ;sprite 3
  
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
	
collisionData:
	.DB $45, $47


;spriteindex, HP, speed, moveFrames, waitFrames, hMove, vMove, currentAnimFrame
enemyData:
	.DB $00, $00, $02, $00, $00, $00, $00, $00
	.DB $10, $00, $01, $00, $00, $00, $00, $00
	.DB $20, $00, $01, $00, $00, $00, $00, $00
	.DB $30, $00, $01, $00, $00, $00, $00, $00
	.DB $40, $00, $01, $20, $00, $00, $00, $00
	.DB $50, $00, $01, $50, $00, $00, $00, $00
	
	
background:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 0
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
  
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;row 1
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45 ;;all sky
 
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 2
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 1
 
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 3
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 4
  .DB $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
  
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 6
  .db $24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$38,$24,$24,$24,$24,$24  ;;some brick tops
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 7
  .db $24,$24,$53,$54,$24,$24,$24,$39,$3A,$3B,$3C,$24,$24,$24,$24,$24  ;;brick bottoms
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 8
  .db $24,$24,$55,$56,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 9
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 10
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 11
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 12
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 13
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 14
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 15
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 16
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 17
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 18
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 19
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 20
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 21
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 22
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 23
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 25
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 26
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;row 1
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45 ;;all sky
 
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 1
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 1
 
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky



; r�knat fr�n v�nster representerar tv� bitar attributet f�r ett mario-block
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

;; Second screen (right). Has an open left edge and the end-of-level wall on the right.
background2:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 0
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;row 1
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 2
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 3
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 4
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47
  .db $24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$24,$24,$24,$24,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45
  .db $24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$38,$24,$24,$24,$24,$24  ;;row 6
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47
  .db $24,$24,$24,$24,$24,$24,$24,$39,$3A,$3B,$3C,$24,$24,$24,$24,$24  ;;row 7
  .db $24,$24,$24,$24,$53,$54,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 8
  .db $24,$24,$24,$24,$55,$56,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 9
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 10
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 11
  .db $24,$24,$24,$24,$24,$24,$35,$25,$25,$38,$24,$24,$24,$24,$45,$45
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 12
  .db $24,$24,$24,$24,$24,$24,$39,$3A,$3B,$3C,$24,$24,$24,$24,$47,$47
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 13
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 14
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 15
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 16
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 17
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 18
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 19
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 20
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 21
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 22
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 23
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 25
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 26
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;row 27
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 28
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 29
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

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