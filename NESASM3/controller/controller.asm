  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

    
  .bank 0
  .org $C000 
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

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0200, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0300, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


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
  CPX #$20              ; Compare X to hex 20, decimal 32 - copying 32 bytes 
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down



LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $10, decimal 16
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
LoadBackground:
	LDA $2002 ; read PPU status to reset high/low latch
	LDA #$20
	STA $2006 ; write the high byte
	LDA #$00
	STA $2006 ; write low byte 
	
	LDX #$00 ; start at zero
	
LoadBackgroundLoop:
	LDA background, x
	STA $2007 ; write to PPU
	
	INX 
	CPX #$C0 ; compare x to 192
	
	BNE LoadBackgroundLoop
	
LoadAttribute:
	LDA $2002 ; read to reset 
	LDA #$23
	STA $2006
	LDA #$C0
	STA $2006
	LDX #$00
	
LoadAttributeLoop:
	LDA attribute, x
	STA $2007
	INX
	CPX #$10 ; hex for 16 
	BNE LoadAttributeLoop
	
		                        


  LDA #%10000000   ; enable NMI, sprites from Pattern Table 1
  STA $2000

  LDA #%00010000   ; enable sprites
  STA $2001
  
SpriteDone:

Forever:
  JMP Forever     ;jump back to Forever, infinite loop
  
 

NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer


LatchController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016       ; tell both the controllers to latch buttons


ReadA: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)

ReadADone:        ; handling this button is done
  

ReadB: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  
ReadBDone:        ; handling this button is done

ReadSelect:
  LDA $4016		; P 1 - Select
  AND #%00000001
  BEQ ReadSelectDone  
ReadSelectDone: 

ReadStart:
  LDA $4016	;	P1 - Start 
  AND #%00000001
  BEQ ReadStartDone
ReadStartDone: 

ReadUp:
  LDA $4016	; P1 - Up
  AND #%0000001
  BEQ ReadUpDone
  
  LDA $0200
  SEC
  SBC #$01
  STA $0200
  
  LDA $0204
  SEC
  SBC #$01
  STA $0204
  
  LDA $0208
  SEC
  SBC #$01
  STA $0208
  
  LDA $020c
  SEC
  SBC #$01
  STA $020c
ReadUpDone:

ReadDown:
  LDA $4016	; P1 - Down
  AND #%00000001
  BEQ ReadDownDone
  
  LDA $0200
  CLC
  ADC #$01
  STA $0200
  
  LDA $0204
  CLC
  ADC #$01
  STA $0204
  
  LDA $0208
  CLC
  ADC #$01
  STA $0208
  
  LDA $020c
  CLC
  ADC #$01
  STA $020c
  
ReadDownDone:

ReadLeft:
  LDA $4016	; P1 - Left
  AND #%00000001
  BEQ ReadLeftDone
  
  LDA $0203
  SEC
  SBC #$01
  STA $0203
  
  LDA $0207
  SEC
  SBC #$01
  STA $0207
  
  LDA $020B
  SEC
  SBC #$01
  STA $020B
  
  LDA $020F
  SEC
  SBC #$01
  STA $020F
  
ReadLeftDone:

ReadRight:
  LDA $4016	; P1 - Right
  AND #%00000001
  BEQ ReadRightDone
  
  LDA $0203
  CLC
  ADC #$01
  STA $0203
  
  LDA $0207
  CLC
  ADC #$01
  STA $0207
  
  LDA $020B
  CLC
  ADC #$01
  STA $020B
  
  LDA $020F
  CLC
  ADC #$01
  STA $020F
  ;JSR MoveMarioRight
ReadRightDone:

;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005

  RTI             ; return from interrupt
 
;;;;;;;;;;;;;;  
  
  .bank 1
  .org $E000
palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$16,$28,$18,  $31,$02,$38,$3C,  $0F,$1C,$15,$14,  $31,$02,$38,$3C 

sprites:
     ;vert tile attr horiz
  .db $80, $32, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1
  .db $88, $34, $00, $80   ;sprite 2
  .DB $88, $35, $00, $88   ;sprite 3
  
  background:
	.db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky ($24 = sky)



  .db $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky



  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;row 1
  
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;all sky ($24 = sky)



  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 2
  
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;all sky ($24 = sky)
  
  
  
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;row 1
  
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;all sky ($24 = sky)



  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 2
  
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;all sky ($24 = sky)
  
  attribute:
  
  .DB %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  
  .DB %00000100, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  


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
  
  ;
