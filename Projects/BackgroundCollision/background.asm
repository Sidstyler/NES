  .inesprg $02   ; 2x 16KB PRG code
  .ineschr $00   ; 0x  8KB CHR data
  .inesmap $00   ; mapper 0 = NROM, no bank swapping
  .inesmir $01   ; background mirroring

;-----------------------------------------------------------------------------------------------------

  .include "Subfiles/VariablesandConstants.asm"

;-----------------------------------------------------------------------------------------------------

;;;;;;;;;;;;
;;;;;;;;;;;;
    
  .bank 0
  .org $8000 

;;;;;;;;;;;;
;;;;;;;;;;;;

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

vblankwait
  BIT $2002
  BPL vblankwait

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

vblankwait2
  BIT $2002
  BPL vblankwait2

  LDA #$32                         ;here we set up any start points for enemies
  STA sprite_RAM                   ;in practice, these locations would be determined by your background
  STA sprite_RAM+3
 
  LDA #$5F
  STA sprite_RAM+16
  STA sprite_RAM+19
  
  LDA #$AE
  STA sprite_RAM+32
  STA sprite_RAM+35

  LDA #$0E
  STA sprite_RAM+48
  STA sprite_RAM+51

  LDA #LOW(crewman_graphics)       ;here we set up the pointer data for the different enemy types
  STA enemy_pointer                ;in practice, you wouldn't hard code these, they would be part of the loading routines
  LDA #HIGH(crewman_graphics+1)
  STA enemy_pointer+1

  LDA #LOW(Punisher_graphics)
  STA enemy_pointer+2
  LDA #HIGH(Punisher_graphics+1)
  STA enemy_pointer+3

  LDA #LOW(ArseFace_graphics)
  STA enemy_pointer+4
  LDA #HIGH(ArseFace_graphics+1)
  STA enemy_pointer+5

  LDA #LOW(McBoobins_graphics)
  STA enemy_pointer+6
  LDA #HIGH(McBoobins_graphics+1)
  STA enemy_pointer+7

LoadSpritePalettes:                ;load pallettes for sprites, background isn't used, so we don't need to populate it
  LDA $2002
  LDA #$3F
  STA $2006
  LDA #$10
  STA $2006
  LDX #$00
LoadSpritePalettesLoop:
  LDA spritepalette, x
  STA $2007
  INX
  CPX #$10
  BNE LoadSpritePalettesLoop

LoadPalettes:
  LDA $2002                        ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006                        ; write the high byte of $3F00 address
  LDA #$00
  STA $2006                        ; write the low byte of $3F00 address
  LDX #$00
LoadPalettesLoop:
  LDA palette, x        
  STA $2007
  INX
  CPX #$10
  BNE LoadPalettesLoop

LoadAttribute:
  LDA $2002                        ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006                        ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006                        ; write the low byte of $23C0 address
  LDX #$00 
  LDA #%11111111
LoadAttributeLoop:
  STA $2007
  INX
  CPX #$40
  BNE LoadAttributeLoop

  LDA #$01
  STA Y_coord
  LDA #$01
  STA X_coord

  JSR LoadbgBackground

  LDA #$00
  JSR GameStateUpdate

  LDA #$00
  STA $2003                         ; set the low byte (00) of the RAM address

  LDX #$00
  JSR LoadCompleteBank              ;load the sprite data

  LDX #$02
  JSR LoadCompleteBank              ;load the background data

  JSR PartialBankSetUp

  LDA #%10010000                    ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

;-----------------------------------------------------------------------------------------------------
;-----------------------START MAIN PROGRAM------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------

Forever:
  INC sleeping                     ;wait for NMI

.loop
  LDA sleeping
  BNE .loop                        ;wait for NMI to clear out the sleeping flag

  LDA #$01
  STA updating_background          ;this is for when you are changing rooms or something, not really needed here
                                   ;it will skip the NMI updates so as not to mess with your room loading routines

  JSR strobe_controllers

  JSR GameStateIndirect

  LDA GameState
  CMP GameStateOld
  BEQ .next

  JSR GameStateUpdate

.next

  LDA #$00
  STA updating_background 

  LDA updating_background+1        ;we put this in because the usage bar TURNS ON THE BACKGROUND
  BNE .next1

  JSR showCPUUsageBar              ;this is the correct place for the usage bar...oops.

.next1

  JMP Forever                      ;jump back to Forever, and go back to sleep

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;--------THE FUCKIN' NMI ROUTINE, RECOGNIZE BITCH!---------------------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NMI:
  PHA                              ;protect the registers
  TXA
  PHA
  TYA
  PHA

nmi_start:

  LDA updating_background          ;check to be sure that the main program isn't busy
  BNE skip_graphics_updates
  
  LDA #$02
  STA $4014                        ;set the high byte (02) of the RAM address, start the transfer

  JSR GameStateNMIIndirect

  LDA #$00                         ;tell the ppu there is no background scrolling
  STA $2005
  STA $2005
  STA $2006
  STA $2006

  LDA #%00011110                   ;enable sprites, enable background, no clipping on left side
  STA $2001

  LDA #$00
  STA sleeping                     ;wake up the main program
  STA updating_background+1

skip_graphics_updates:

  PLA                              ;restore the registers
  TAY 
  PLA
  TAX
  PLA

  RTI                              ;return from interrupt

;-----------------------------------------------------------------------------------------------------
;-----------------------GAME STATE LOADING ROUTINE----------------------------------------------------
;-----------------------------------------------------------------------------------------------------

GameStateIndirect:                 ;indirect jump to main program
  JMP [Main_Pointer]

GameStateNMIIndirect:              ;indirect jump to NMI routine
  JMP [NMI_Pointer]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;00=Main Top Down View
;01=Paused

GameStates:
  .word GameState0,GameState1

GameStateNMIs:
  .word GameStateNMI0,GameStateNMI1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GameStateUpdate:                   ;load the game state and NMI pointers
  LDA GameState
  STA GameStateOld
  ASL A                            ;multiply by two
  TAX

  LDA GameStates,x                 ;Load the Main Program Pointer
  STA Main_Pointer
  LDA GameStates+1,x
  STA Main_Pointer+1

  LDA GameStateNMIs,x
  STA NMI_Pointer
  LDA GameStateNMIs+1,x
  STA NMI_Pointer+1

  RTS  

;-----------------------------------------------------------------------------------------------------
;-----------------------MAIN TOP DOWN VIEW GS #$00----------------------------------------------------
;-----------------------------------------------------------------------------------------------------

GameState0:

  INC random_direction2            ;counter for random sprite directions

  JSR random

  LDA #$00
  STA enemy_number
  STA enemy_ptrnumber

  JSR handle_input

  JSR transfer_location_info

  JSR Collision_Detection

  JSR restore_location_info

  JSR Enemys_Animation

  JSR Enemys_Sprite_Loading

  JSR update_enemy_sprites

  JSR transfer_location_info

  JSR arrow

arrow_dec:
  LDA A_timer
  BEQ .done
  DEC A_timer
.done

  JSR mace

mace_dec:
  LDA B_timer
  BEQ .done
  DEC B_timer
.done

  JSR PC_transfer                  ;transfer the coordinates of the Playable Character
  JSR hammer_transfer              ;transfer the coordinates of the hammer
  JSR arrow_transfer               ;transfer the coordinates of the arrow

UpdateENEMIES:
  LDA #$01                         ;this loop updates the enemies one at a time in a loop
  STA enemy_number                 ;start with enemy one
  LDA #$02
  STA enemy_ptrnumber
.loop
  JSR enemy_update                 ;move the sprites based on which direction they are headed

  JSR transfer_location_info

  JSR Collision_Detection

  JSR restore_location_info

  JSR Enemys_Animation             ;find out which frame the enemy animation is on
  JSR Enemys_Sprite_Loading        ;update the enemy meta tile graphics
  JSR update_enemy_sprites         ;update position

  LDY #$00            ;arrow       ;sprite collision detection routines
  JSR enemy_collision              ;note that 00, 02, and 04 are to specify the variables of each weapon
  LDY #$02            ;mace
  JSR enemy_collision
  LDY #$04            ;Playable Character
  JSR enemy_collision

  INC enemy_number                 ;incriment the enemy number for the next trip through the loop
  INC enemy_ptrnumber              ;these are addresses for the graphics data, so we need to keep it at 2x the enemy number
  INC enemy_ptrnumber
  LDA enemy_number
  CMP #$04                         ;if it is 4, we have updated enemies 0,1,2,3 so we are done
  BNE .loop
UpdateENEMIESdone:

  JSR PartialBankSetUp             ;set up the CHR addresses for the data for the next frame

  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;-----NMI ROUTINE, MAIN TOP DOWN VIEW GAME STATE #$00------------------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GameStateNMI0:

  JSR LoadPartialBank              ;load the CHR_ROM data into RAM.  This can only be done during NMI!!

  RTS

;-----------------------------------------------------------------------------------------------------
;-----------------------MAIN PROGRAM GS #$01-PAUSED---------------------------------------------------
;-----------------------------------------------------------------------------------------------------

GameState1:

;ReadStart: 
  LDA joypad1_pressed       ; player 1 - start
  AND #%00010000
  BEQ .ReadStartDone

  LDA GameState+1
  STA GameState
 
.ReadStartDone:

  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;--------NMI ROUTINE, PAUSED GAME STATE #$01-PAUSED--------------------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GameStateNMI1:

  RTS

;-----------------------------------------------------------------------------------------------------

; read_joypad will capture the current button state and store it in joypad1.  
;       Off-to-on transitions will be stored in joypad1_pressed
strobe_controllers:
    lda joypad1
    sta joypad1_old ;save last frame's joypad button states
    
    lda #$01
    sta $4016
    lda #$00
    sta $4016
    
    ldx #$08
.loop:    
    lda $4016
    lsr a
    rol joypad1  ;A, B, select, start, up, down, left, right
    dex
    bne .loop
    
    lda joypad1_old ;what was pressed last frame.  EOR to flip all the bits to find ...
    eor #$FF    ;what was not pressed last frame
    and joypad1 ;what is pressed this frame
    sta joypad1_pressed ;stores off-to-on transitions
    
    rts

;-----------------------------------------------------------------------------------------------------

showCPUUsageBar:

  ldx #%00011111  ; sprites + background + monochrome
  stx $2001
  ldy #21  ; add about 23 for each additional line
.loop
    dey
    bne .loop
  dex    ; sprites + background + NO monochrome
  stx $2001
  rts

;-----------------------------------------------------------------------------------------------------

random_table:
  .db $01,$00,$03,$02,$02,$01,$00,$00,$01,$03

random:
  INC random_direction1
  LDA random_direction1
  CMP #$0A
  BNE .next
  LDA enemy_direction
  STA random_direction1
.next
  LDX random_direction1
  LDA random_table,X
  STA random_direction
  RTS

;-----------------------------------------------------------------------------------------------------
;-----------------------GRAPHICS STUFF----------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------

spritepalette:
  .db $0F,$04,$1C,$10, $0F,$05,$11,$08, $0F,$16,$29,$08, $0F,$28,$31,$0B      ;sprite palette

palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;background palette

;-----------------------------------------------------------------------------------------------------

graphicspointers:                 ;addresses of the CHR_ROM Data
  .word Sprite_Data,CHR_Data

;-----------------------------------------------------------------------------------------------------

LoadCompleteBank:                 ;load in the graphics

  LDA graphicspointers,x          ;this is all CHR-RAM stuff
  STA tile_loader_ptr            
  LDA graphicspointers+1,x
  STA tile_loader_ptr+1

  LDY #$00
  LDA $2002
  LDA [tile_loader_ptr],y
  STA $2006
  INC tile_loader_ptr
  LDA [tile_loader_ptr],y
  STA $2006
  INC tile_loader_ptr
  LDX #$00
  LDY #$00
.LoadBank:
  LDA [tile_loader_ptr],y
  STA $2007
  INY
  CPY #$00
  BNE .LoadBank
  INC tile_loader_ptr+1
  INX
  CPX #$10
  BNE .LoadBank

  RTS

;-----------------------------------------------------------------------------------------------------

PartialBankSetUp:                  ;load the address that we want to write to.  See the tip in the write up.
  LDA #$15
  STA tile_loader_addy
  LDA #$30
  STA tile_loader_addy+1

  LDA #$40                         ;load the number of entries.  #$10 for each tile to load.
  STA tile_loader_stop

  LDA tile_loader_counter          ;load the pointer to the information
  ASL A
  TAX
  LDA TileUpdates,X
  STA tile_loader_ptr
  LDA TileUpdates+1,X
  STA tile_loader_ptr+1

  INC tile_loader_counter          ;handle the counters, so that the next frame loads next time
  INC tile_loader_counter+1        ;This variable is used when you are loading a different number of tiles
  INC tile_loader_counter+1        ;to different addresses.  Not really needed here.
  INC tile_loader_counter+1

  LDA tile_loader_counter          ;reset if we get to the last frame.
  CMP #$10
  BNE .done
  LDA #$00
  STA tile_loader_counter
  STA tile_loader_counter+1

.done
  RTS

;-----------------------------------------------------------------------------------------------------

LoadPartialBank:

  LDA $2002
  LDA tile_loader_addy             ;input the address
  STA $2006
  LDA tile_loader_addy+1
  STA $2006
  LDY #$00
.LoadBank:
  LDA [tile_loader_ptr],y          ;load the data to the CHR space DURING NMI
  STA $2007
  INY
  CPY tile_loader_stop
  BNE .LoadBank

  RTS

;-----------------------------------------------------------------------------------------------------
;-----------------------USER INPUT--------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------

handle_input:

ReadA: 
  LDA joypad1_pressed       ; player 1 - A
  AND #%10000000  ; only look at bit 0
  BEQ ReadADone

  LDA Weapons_RAM+4     ;see if the arrow is ready to fire again
  CMP #$FE 
  BNE ReadADone

  LDA #Atimer   ;reset the timer so that the arrow is fired next frame
  STA A_timer

ReadADone:        ; handling this button is done
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
ReadB: 
  LDA joypad1_pressed      ; player 1 - B
  AND #%01000000  ; only look at bit 0
  BEQ ReadBDone

  LDA B_timer     ;check and see if the mace is still out
  BNE ReadBDone

  LDA #Btimer        ;reset the timer if it was not pressed last time, but is this time
  STA B_timer
  
ReadBDone:        ; handling this button is done
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
ReadSelect: 
  LDA joypad1_pressed       ; player 1 - select
  AND #%00100000  ; only look at bit 6
  BEQ ReadSelectDone   ; branch to ReadSelectDone if button is NOT pressed (0)

ReadSelectDone:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 
ReadStart: 
  LDA joypad1_pressed       ; player 1 - start
  AND #%00010000  ; only look at bit 5
  BEQ ReadStartDone

  LDA GameState
  STA GameState+1
 
  LDA #PauseState
  STA GameState

ReadStartDone:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadUp:
  LDA joypad1	;Up
  AND #%00001000
  BEQ ReadUpDone
  JSR UpMovement

  JMP ReadRightDone

ReadUpDone:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadDown:
  LDA joypad1	;Down
  AND #%00000100
  BEQ ReadDownDone
  JSR DownMovement

  JMP ReadRightDone

ReadDownDone:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadLeft:
  LDA joypad1	;Left
  AND #%00000010
  BEQ ReadLeftDone
  JSR LeftMovement

  JMP ReadRightDone

ReadLeftDone:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ReadRight:
  LDA joypad1	;Right
  AND #%00000001
  BEQ ReadRightDone
  JSR RightMovement

ReadRightDone:

  RTS

;-----------------------------------------------------------------------------------------------------

UpMovement:
  LDA #$00                        ;load the appropriate direction into the direction flag
  STA enemy_direction

  LDA sprite_RAM                  ;move the character
  SEC
  SBC #enemy_speed 
  STA sprite_RAM

  INC Enemy_Animation             ;incriment the animation counter

  LDA sprite_RAM                  ;check to see if we are at the top of the screen
  CMP #top_exit
  BCS .done                       ;if not, we are done.  If so, we need to switch rooms

  DEC Y_coord                     ;DEC the Y coordinate of the world map

  LDA #$BF                        ;move our character so that it looks like he is walking across the screen
  STA sprite_RAM

  JSR update_enemy_sprites        ;update the meta tile
  JSR LoadbgBackground            ;using the new Y_coord, update the background to the new room

.done

  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DownMovement:                     ;note that the comments for "UP" are basically the same for the other directions
  LDA #$01
  STA enemy_direction

  LDA sprite_RAM
  CLC
  ADC #enemy_speed 
  STA sprite_RAM

  INC Enemy_Animation

  LDA sprite_RAM
  CMP #bottom_exit
  BCC .done

  INC Y_coord

  LDA #$0E
  STA sprite_RAM

  JSR update_enemy_sprites
  JSR LoadbgBackground

.done

  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RightMovement:

  LDA #$02
  STA enemy_direction

  LDA sprite_RAM+3
  CLC
  ADC #enemy_speed 
  STA sprite_RAM+3

  INC Enemy_Animation

  LDA sprite_RAM+3
  CMP #right_exit
  BCC .done

  INC X_coord

  LDA #$0D
  STA sprite_RAM+3

  JSR update_enemy_sprites
  JSR LoadbgBackground

.done

  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LeftMovement:

  LDA #$03
  STA enemy_direction

  LDA sprite_RAM+3
  SEC
  SBC #enemy_speed 
  STA sprite_RAM+3

  INC Enemy_Animation

  LDA sprite_RAM+3
  CMP #left_exit
  BCS .done

  DEC X_coord

  LDA #$E3
  STA sprite_RAM+3

  JSR update_enemy_sprites
  JSR LoadbgBackground

.done

  RTS

;-----------------------------------------------------------------------------------------------------
;------Background Loading-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------

backgroundpointer:                     ;room data for our room_index to reference to
  .word Room1,Room2,Room3,Room4

;-----------------------------------------------------------------------------------------------------


LoadbgBackground:

  LDA #$01
  STA updating_background              ;"disable" NMI

  LDA #%00000110                       ;disable sprites, disable background, no clipping on left side
  STA $2001

.Find_Room_Index: 
  LDA Y_coord                          ;load the coordinates we set in our movement routines
  LDX X_coord

  CPX #$00                             ;if we are in the first column, the Y coord is the room index
  BEQ .done

.loop:  ;for each X, add 2 to the index  ;note that this would change depending on your map configuration
  CLC
  ADC #$02
  DEX
  CPX #$00
  BNE .loop

.done

  ASL A                                ;indexing to a table of words

  STA room_index
 
  LDX room_index                       ;using the room index, load the pointer to the correct background data
  LDA backgroundpointer,X
  STA ptr1
  STA collideptr                       ;store the room data for collision detection
  LDA backgroundpointer+1,X
  STA ptr1+1
  STA collideptr+1

  LDA $2002                            ;read PPU status to reset the high/low latch
  LDA #$20
  STA $2006                            ;write the high byte of $2000 address
  LDA #$00
  STA $2006                            ;write the low byte of $2000 address
  LDY #$00                             ;start out at 00
  STY data_y
  LDX #$00
  STX data_x

background_start:

  LDA repeat_meta_tile                 ;see if you are repeating a tile
  BEQ .jump 
  DEC repeat_meta_tile                 ;decrement the repeated tile and jump back to the decompression routine
  JMP decompress

.jump

  LDY data_y
  LDA [ptr1],y
  CMP #$FF                             ;see if it is at the end of the data string
  BNE .next
  JMP background_complete
.next
  LDA [ptr1],y
  AND #%10000000                       ;see if this is a repeated tile or a switch of meta tile banks
  BNE .next1
  JMP .loadtile

.next1
  LDA [ptr1],y
  AND #%01000000                       ;see if this is a switch of the meta tile banks
  BEQ .loadtile
  BNE .next2
  JMP .repeat

.next2
  LDA [ptr1],y
  AND #%00111111                       ;switch meta tile banks
  ASL A
  TAX

  LDA meta_tile_sets,x                 ;load the new meta tile bank
  STA meta_tile_sets_ptr
  LDA meta_tile_sets+1,x
  STA meta_tile_sets_ptr+1
  INY
  STY data_y 
  JMP background_start                 ;jump to the next entry in the data table and return to the start of the routine

.repeat
  LDA [ptr1],y
  AND #%00111111                       ;load number of repeats for this meta tile 
  STA repeat_meta_tile                 ;note that this is set up so that you need to have the number of repeats - 1
  INY
  STY data_y

.loadtile
  LDA [ptr1],y                         ;load the meta tile number
  INY
  STY data_y
  ASL A                                ;this is a table of addresses, so multiply by 2
  TAY

  LDA [meta_tile_sets_ptr],y           ;load the address of the meta tile in question
  STA meta_tile_ptr
  INY
  LDA [meta_tile_sets_ptr],y
  STA meta_tile_ptr+1
  
decompress:
  LDY #$00
  LDX data_x
  LDA [meta_tile_ptr],y                ;write the first tile of the meta tile to 2007
  STA $2007
  INY
  LDA [meta_tile_ptr],y                ;write the second tile of the meta tile to 2007
  STA $2007
  INY
  LDA [meta_tile_ptr],y                ;write the third tile of the meta tile to memory for later use
  STA background_row,x
  INY
  INX
  LDA [meta_tile_ptr],y                ;write the forth tile of the meta tile to memory for later use
  STA background_row,x
  INX
  STX data_x
  CPX #$20                             ;see if we are at the end of a row.  If so, we need to use the tiles stored in memory, if not, jump to the top.
  BEQ .next
  JMP background_start
  
.next
  LDY #$00
row_start:                             ;write the tiles stored in memory to the background
  LDA background_row,y
  STA $2007
  INY
  CPY #$20
  BNE row_start
  LDX #$00
  STX data_x

  JMP background_start                 ;jump back to the start of the routine and continue on

background_complete:

  LDA #$00                             ;"enable" NMI
  STA updating_background

  RTS

;;;;;;;;;;;;
;;;;;;;;;;;; 
  
  .bank 1
  .org $A000

;;;;;;;;;;;;
;;;;;;;;;;;;

;-----------------------------------------------------------------------------------------------------

updateconstants:                  ;constants for the use of the sprite_RAM constant            
  .db $00,$10,$20,$30             ;4 sprites for each meta sprite, so add $10 for each meta sprite we process

;-----------------------------------------------------------------------------------------------------

randomenemydirection:             ;these are random numbers used with the random_direction2 to make enemies switch direction
  .db $57,$CD,$AF,$05,$BC

;-----------------------------------------------------------------------------------------------------
;------Enemy Graphics Updates-------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------



enemy_update:
  LDX enemy_number
  INC Enemy_Animation,x          ;incriment the counter for the animation routine 

  LDA random_direction2          ;check random_direction2 with the values stored in randomenemydirection
  CMP randomenemydirection,x     ;this is in place of the routine you would have in collision detection
  BEQ .down
  CMP randomenemydirection+1,x
  BNE .done
.down

  LDA random_direction           ;if the values match, switch direction the counter random_direction1
  STA enemy_direction,x

.done
  LDA enemy_direction,x          ;for the various directions, move the sprites around the background
  BNE .next 
  JMP enemy_up                   ;note JMP not JSR
.next
  CMP #$01
  BNE .next1
  JMP enemy_down
.next1
  CMP #$02
  BNE .next2
  JMP enemy_right
.next2
  JMP enemy_left

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

enemy_up:

  LDX enemy_number                ;move the sprite up
  LDY updateconstants,x
  LDA sprite_RAM,y
  SEC
  SBC #enemy_speed
  STA sprite_RAM,y

  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

enemy_down:

  LDX enemy_number                ;move the sprite down
  LDY updateconstants,x
  LDA sprite_RAM,y
  CLC
  ADC #enemy_speed
  STA sprite_RAM,y

  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

enemy_right:

  LDX enemy_number                ;move the sprite right
  LDY updateconstants,x
  LDA sprite_RAM+3,y
  CLC
  ADC #enemy_speed
  STA sprite_RAM+3,y

  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

enemy_left:

  LDX enemy_number                ;move the sprite left
  LDY updateconstants,x
  LDA sprite_RAM+3,y
  SEC
  SBC #enemy_speed
  STA sprite_RAM+3,y

  RTS  

;-----------------------------------------------------------------------------------------------------

Enemys_Animation:                 ;this routine updates the frame that is displayed for each enemy 1,2,1,or 3

  LDX enemy_number

  LDA Enemy_Animation,x           ;compare to constants
  CMP #enemyFrames1               ;you can change these around to make the animation faster or slower on the constants page
  BEQ .Animation1
  CMP #enemyFrames2
  BEQ .Animation2
  CMP #enemyFrames3
  BEQ .Animation1
  CMP #enemyFrames4
  BEQ .Animation3
  JMP .AnimationDone

.Animation1:                      ;load the various frames
  LDA #$00 
  STA Enemy_Frame,x
  JMP .AnimationDone
.Animation2:
  LDA #$01 
  STA Enemy_Frame,x
  JMP .AnimationDone
.Animation3:
  LDA #$02
  STA Enemy_Frame,x
.AnimationDone

  LDA Enemy_Animation,x           ;reset the counter when it gets to the end
  CMP #enemyFrames4
  BNE .AnimationFinished
  SEC
  SBC #enemyFrames4
  STA Enemy_Animation,x
.AnimationFinished
  RTS

;-----------------------------------------------------------------------------------------------------

PC_up:
  .db $00,$08,$10                ;the indexes to the various frames in the spritegraphics file
PC_down:
  .db $18,$20,$28
PC_right:
  .db $30,$38,$40
PC_left:
  .db $48,$50,$58

;-----------------------------------------------------------------------------------------------------

Enemys_Sprite_Loading:           ;this routine updates the sprite graphics every frame based on data set by the other routines

  LDX enemy_number
  LDA enemy_direction,x
  BEQ .next  ;up                 ;find out which direction it is going
  CMP #$01
  BEQ .next1 ;down
  CMP #$02
  BEQ .next2 ;right
  JMP .next3 ;left

.next                            ;UP
  LDA Enemy_Frame,x              ;load the spritegraphics index based on the frame number set in the animation routine
  TAX                            ;some of this is redundant because I removed some of the more complex code here
  LDA PC_up,x
  TAY
  JMP enemyspritesupdate         ;update graphics 

.next1                           ;DOWN
  LDA Enemy_Frame,x
  TAX
  LDA PC_down,x
  TAY
  JMP enemyspritesupdate

.next2                           ;RIGHT
  LDA Enemy_Frame,x
  TAX
  LDA PC_right,x
  TAY
  JMP enemyspritesupdate

.next3                           ;LEFT
  LDA Enemy_Frame,x
  TAX
  LDA PC_left,x
  TAY
  JMP enemyspritesupdate

;-----------------------------------------------------------------------------------------------------

enemyspritesupdate:               ;this routine updates the tiles and attributes for the enemies

  LDX enemy_ptrnumber             ;load in the pointer for the graphics data
  LDA enemy_pointer,x
  STA enemygraphicspointer
  INX
  LDA enemy_pointer,x
  STA enemygraphicspointer+1

subenemyspritesupdate:            ;we put this here incase we want to have some sort of special update, like shooting graphics, it is not used here
  LDX enemy_number                
  LDA updateconstants,x
  TAX
  
  LDA [enemygraphicspointer],y    ;read the tile from the "spritegraphics" sub file and store it in memory
  STA sprite_RAM+1, x
  INY
  LDA [enemygraphicspointer],y
  STA sprite_RAM+5, x
  INY
  LDA [enemygraphicspointer],y
  STA sprite_RAM+9, x
  INY
  LDA [enemygraphicspointer],y
  STA sprite_RAM+13, x
  INY
  LDA [enemygraphicspointer],y
  STA sprite_RAM+2, x
  INY
  LDA [enemygraphicspointer],y
  STA sprite_RAM+6, x
  INY
  LDA [enemygraphicspointer],y
  STA sprite_RAM+10, x
  INY
  LDA [enemygraphicspointer],y
  STA sprite_RAM+14, x

  RTS

;-----------------------------------------------------------------------------------------------------

update_enemy_sprites:             ;this routine updates the position of the meta sprites relative to each other
  LDX enemy_number
  LDA updateconstants,x
  TAX

  LDA sprite_RAM,x                ;vertical updates
  STA sprite_RAM+4,x
  CLC
  ADC #$08
  STA sprite_RAM+8,x
  STA sprite_RAM+12,x

  LDA sprite_RAM+3,x              ;horizontal updates
  STA sprite_RAM+11,x
  CLC 
  ADC #$08
  STA sprite_RAM+7,x
  STA sprite_RAM+15,x

  RTS

;-----------------------------------------------------------------------------------------------------
;------Weapons Graphics Updates and Sprite Collision--------------------------------------------------
;-----------------------------------------------------------------------------------------------------

transfer_location_info:
  LDX enemy_number
  LDA updateconstants,x
  TAX
  LDA sprite_RAM,x
  STA sprite_vertical
  LDA sprite_RAM+3,x
  STA sprite_horizontal
  RTS

;-----------------------------------------------------------------------------------------------------

restore_location_info:
  LDX enemy_number
  LDA updateconstants,x
  TAX
  LDA sprite_vertical
  STA sprite_RAM,x
  LDA sprite_horizontal
  STA sprite_RAM+3,x
  RTS

;-----------------------------------------------------------------------------------------------------

arrow:

  LDA Weapons_RAM+4         ;see if the arrow is active
  CMP #$FE
  BNE .next3

  JMP .loadarrow

.next3
  LDA arrow_direction
  BNE .down                 ;moving in the up direction
  LDA Weapons_RAM+4
  SEC
  SBC #arrow_speed
  STA Weapons_RAM+4

  JSR arrow_collision

  JMP .loadarrow

.down
  CMP #$01
  BNE .right                ;moving down
  LDA Weapons_RAM+4
  CLC
  ADC #arrow_speed
  STA Weapons_RAM+4

  JSR arrow_collision

  JMP .loadarrow

.right
  CMP #$02
  BNE .left                 ;moving right
  LDA Weapons_RAM+7
  CLC
  ADC #arrow_speed
  STA Weapons_RAM+7

  JSR arrow_collision

  JMP .loadarrow

.left
  LDA Weapons_RAM+7         ;moving left
  SEC
  SBC #arrow_speed
  STA Weapons_RAM+7

  JSR arrow_collision

  JMP .loadarrow

.loadarrow
  LDA A_timer               ;load the starting point for the arrow
  CMP #Atimer
  BEQ .down1
  JMP .graphics

.down1

  LDA enemy_direction
  STA arrow_direction
  CMP #$00
  BNE .nexts

  LDA #$1E                  ;load the UP graphics
  STA Weapons_RAM+5 
  LDA #%00000010
  STA Weapons_RAM+6

  LDA sprite_vertical
  SEC
  SBC #$08
  STA Weapons_RAM+4
  
  LDA sprite_horizontal
  SEC
  SBC #$01
  STA Weapons_RAM+7 
  JMP .graphics

.nexts
  CMP #$01
  BNE .nexts1

  LDA #$1E                   ;load the DOWN graphics
  STA Weapons_RAM+5
  LDA #%10000010
  STA Weapons_RAM+6

  LDA sprite_vertical
  CLC
  ADC #$0D
  STA Weapons_RAM+4
  
  LDA sprite_horizontal
  SEC
  SBC #$01
  STA Weapons_RAM+7  

  JMP .graphics
.nexts1
  CMP #$02
  BNE .nexts2

  LDA #$1F                  ;load the RIGHT graphics
  STA Weapons_RAM+5
  LDA #%00000010
  STA Weapons_RAM+6

  LDA sprite_vertical
  CLC
  ADC #$05
  STA Weapons_RAM+4
  
  LDA sprite_horizontal
  CLC
  ADC #$10
  STA Weapons_RAM+7 

  JMP .graphics

.nexts2

  LDA #$1F                  ;load the left graphics
  STA Weapons_RAM+5
  LDA #%01000010
  STA Weapons_RAM+6

  LDA sprite_vertical
  CLC
  ADC #$05
  STA Weapons_RAM+4
  
  LDA sprite_horizontal
  SEC
  SBC #$08
  STA Weapons_RAM+7

  JMP .graphics

.graphics                   ;make him look like he is shooting
  LDA A_timer
  BEQ .done
  JSR shooting_graphics
.done
  RTS

;-----------------------------------------------------------------------------------------------------

arrow_collision:
  LDA Weapons_RAM+4         ;arrow collision detection with the walls
  CMP #$08
  BCC .next
  CMP #$E8
  BCS .next
  LDA Weapons_RAM+7
  CMP #$02
  BCC .next
  CMP #$FE
  BCS .next
  JMP .end
.next
  LDA #$FE
  STA Weapons_RAM+4
  STA Weapons_RAM+7
.end
  RTS

;-----------------------------------------------------------------------------------------------------

mace:                        ;graphics updates for the mace
  LDA B_timer
  BNE .down
  RTS

.down
  JSR shooting_graphics

  LDA B_timer
  CMP #$01
  BNE .next3
  LDA #$FE
  STA Weapons_RAM
  STA Weapons_RAM+3
  RTS

.next3
  LDA enemy_direction
  CMP #$00
  BNE .next

  LDA #%00000010             ;update the UP graphics
  STA Weapons_RAM+2
  LDA #$1C
  STA Weapons_RAM+1

  LDA sprite_vertical        ;save sprite Y position
  SEC 
  SBC #$07
  STA Weapons_RAM 

  LDA sprite_horizontal      ;load sprite X position
  SEC
  SBC #$00
  STA Weapons_RAM+3
  RTS

.next
  CMP #$01
  BNE .next1

  LDA #%10000010             ;update the DOWN graphics
  STA Weapons_RAM+2
  LDA #$1C
  STA Weapons_RAM+1

  LDA sprite_vertical        ;save sprite Y position
  CLC
  ADC #$0C
  STA Weapons_RAM 

  LDA sprite_horizontal      ;load sprite X position
  CLC
  ADC #$00
  STA Weapons_RAM+3
  RTS

.next1
  CMP #$02
  BNE .next2

  LDA #%00000010             ;update the RIGHT graphics
  STA Weapons_RAM+2
  LDA #$1D
  STA Weapons_RAM+1

  LDA sprite_vertical        ;save sprite Y position
  CLC
  ADC #$06
  STA Weapons_RAM 

  LDA sprite_horizontal      ;load sprite X position
  CLC
  ADC #$0F
  STA Weapons_RAM+3
  RTS

.next2
  LDA #%01000010             ;update the LEFT graphics
  STA Weapons_RAM+2
  LDA #$1D
  STA Weapons_RAM+1

  LDA sprite_vertical       ;save sprite Y position
  CLC
  ADC #$06
  STA Weapons_RAM 

  LDA sprite_horizontal     ;load sprite X position
  SEC
  SBC #$07
  STA Weapons_RAM+3
.done
  RTS

;-----------------------------------------------------------------------------------------------------

shooting_graphics:

  LDA enemy_direction
  CMP #$00
  BNE .next
  LDY #$60                        ;up
  JSR enemyspritesupdate
  JSR update_enemy_sprites
  RTS
.next
  CMP #$01
  BNE .next1
  LDY #$78                        ;down
  JSR enemyspritesupdate
  JSR update_enemy_sprites
  RTS
.next1
  CMP #$02
  BNE .next2
  LDY #$68                        ;right
  JSR enemyspritesupdate
  JSR update_enemy_sprites
  RTS
.next2
  LDY #$70                        ;left
  JSR enemyspritesupdate
  JSR update_enemy_sprites
  RTS

;-----------------------------------------------------------------------------------------------------
;------------Sprite Location Routines-----------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------

arrow_a:                         ;outlines of the arrowhead
  .db $00,$00,$01,$01
arrow_b:
  .db $08,$08,$06,$06
arrow_c:
  .db $01,$01,$00,$00
arrow_d:
  .db $06,$06,$08,$08

;;;;;;;;;;;;;

arrow_transfer:

  LDY arrow_direction
  LDA Weapons_RAM+4              ;transfer arrow collision coordinates
  CLC
  ADC arrow_a,y
  STA arrow_vertical
  LDA Weapons_RAM+4
  CLC
  ADC arrow_b,y
  STA arrow_vertical+1
  LDA Weapons_RAM+7
  CLC
  ADC arrow_c,y
  STA arrow_horizontal
  LDA Weapons_RAM+7
  CLC
  ADC arrow_d,y
  STA arrow_horizontal+1
  RTS

;-----------------------------------------------------------------------------------------------------

hammer_a:  
  .db $01,$03,$00,$00
hammer_b:
  .db $05,$03,$05,$05
hammer_c:
  .db $00,$00,$03,$01
hammer_d:
  .db $05,$05,$03,$01

;;;;;;;;;;;;;

hammer_transfer:

  LDY enemy_direction
  LDA Weapons_RAM              ;transfer hammer collision coordinates
  CLC
  ADC hammer_a,y
  STA hammer_vertical
  LDA Weapons_RAM
  CLC
  ADC hammer_b,y
  STA hammer_vertical+1
  LDA Weapons_RAM+3
  CLC
  ADC hammer_c,y
  STA hammer_horizontal
  LDA Weapons_RAM+3
  CLC
  ADC hammer_d,y
  STA hammer_horizontal+1
  RTS

;-----------------------------------------------------------------------------------------------------

PC_transfer:

  LDA sprite_RAM              ;transfer Nightman collision coordinates
  CLC                         ;note that if you make your character different sizes from the side/front/back,
  ADC #$01                    ;you'll need to change this routine from hard code to something like the arrow/mace code.
  STA PC_vertical
  LDA sprite_RAM
  CLC
  ADC #$0E
  STA PC_vertical+1
  LDA sprite_RAM+3
  CLC
  ADC #$03
  STA PC_horizontal
  LDA sprite_RAM+3
  CLC
  ADC #$0C
  STA PC_horizontal+1
  RTS

;-----------------------------------------------------------------------------------------------------
;------------Sprite Collision Detection---------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------

enemy_collision:              ;check collision with the arrows, mace, PC

  LDX enemy_number
  LDA updateconstants,X       ;check the outline of the enemy vs. the weapon
  TAX
  LDA sprite_RAM,X
  CLC
  ADC #$01
  CMP arrow_vertical+1,y
  BCC .next
  RTS
.next
  LDA sprite_RAM,X
  CLC
  ADC #$0E
  CMP arrow_vertical,y
  BCS .next1
  RTS
.next1
  LDA sprite_RAM+3,X
  CLC
  ADC #$03
  CMP arrow_horizontal+1,y
  BCC .next2
  RTS
.next2
  LDA sprite_RAM+3,X
  CLC
  ADC #$0C
  CMP arrow_horizontal,y
  BCS .next3
  RTS
.next3
  CPY #$00                    ;do this if it is an impact with each weapon
  BNE .nexts
  JMP arrow_hit
.nexts
  CPY #$02
  BNE .nexts1
  JMP mace_hit
.nexts1
  JMP PC_hit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

mace_hit:

  JSR direction_change

  LDA #$01
  STA B_timer

  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

arrow_hit:
  LDA #$FE
  STA Weapons_RAM+4
  STA Weapons_RAM+7

  JSR direction_change
 
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PC_hit:

  JSR direction_change

  RTS

;-----------------------------------------------------------------------------------------------------

direction_change:                 ;this is just a random routine to make something happen when a collision occurs
  LDX enemy_number
  LDA random_direction
  STA enemy_direction,x
  RTS

;-----------------------------------------------------------------------------------------------------

  .include "Subfiles/spritegraphics.asm"

;-----------------------------------------------------------------------------------------------------

  .include "Subfiles/Background_Info.asm"

;----------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------
;----------MAIN COLLISION ROUTINE--------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------

vertical_positions:
  .db $00,$0F,$1F,$2F,$3F,$4F,$5F,$6F,$7F,$8F,$9F,$AF,$BF,$CF,$DF        ;this is what we set the position data to when we hit something
horizontal_positions:
  .db $00,$0D,$1D,$2D,$3D,$4D,$5D,$6D,$7D,$8D,$9D,$AD,$BD,$CD,$DD,$ED

;----------------------------------------------------------------------------------------------------------

Find_Collision_Meta_Tiles:

.sprite_transferBGCD:

  LDA sprite_vertical              ;transfer correct collision box coords
  CLC
  ADC #$02                         ;to change this, must change DEC below to keep him from flashing
  STA collide_vertical             ;here we set the boundries of the collision detection for the metasprites
  STA collide_vertical+1           ;note that here we assume that all of the sprites are the same size

  LDA sprite_vertical              ;if your sprites are different, you'll have to add a subroutine here
  CLC
  ADC #$10
  STA collide_vertical+2
  STA collide_vertical+3

  LDA sprite_horizontal
  CLC
  ADC #$03
  STA collide_horizontal
  STA collide_horizontal+2

  LDA sprite_horizontal
  CLC
  ADC #$0C
  STA collide_horizontal+1
  STA collide_horizontal+3

  LDX #$00

.find_collision_meta_tiles

  LDA collide_vertical,x          ;find the index for the upper left corner of the sprite intersect
;  SEC
;  SBC #$40                       ;take off the HUD if you are using one.  You'll have to change the vertical positions as well
.loop
  CMP #$10                        ;Here we use the sprite vertical/horizontal positions to find the look up index in the room data table
  BCC .next                       ;use the vertical position to find the first part
  SEC
  SBC #$10                        ;take one row off of our vertical position
  TAY
  LDA collision_index,x
  CLC
  ADC #$10                        ;add $10 to our index or skip a row in the data table
  STA collision_index,x
  TYA
  INC collide_position,x          ;set up the value of the position marker (vertical_position)
  JMP .loop  

.next                             ;include the horizontal portion of our position
  LDA collide_horizontal,x
.loop2
  CMP #$10                        ;for each meta tile we are from the left, incriment our index
  BCC .next1
  SEC
  SBC #$10
  INC collision_index,x
  INC collide_position+1,x        ;set up the position marker
  JMP .loop2

.next1
  TAY                             ;find the opposite corner, the right corner is always a FIXED distance from the left corner
  LDA collision_index,x
  STA collision_index+1,x         
  TYA
  CLC
  ADC #$09
  CMP #$10
  BCC .next2                      ;are the left and right corners in the same meta tile??
  INC collision_index+1,x

.next2  
  INC collision_index,x           ;skip the meta tile bank entries in the table
  INC collision_index+1,x
                                  ;our index should be correct now!  PowerYay!!
  
.load_meta_tile_data

  LDY collision_index,x           ;load the meta tile number from the room data block
  LDA [collideptr],y
  STA collide_metatile,x          ;store the meta tile info

  LDY collision_index+1,x         ;load the meta tile number from the room data block
  LDA [collideptr],y
  STA collide_metatile+1,x        ;store the meta tile info

  INX                             ;first loop is for the top of the metasprite
  INX                             ;second is for the bottom of the metasprite
  CPX #$04
  BNE .find_collision_meta_tiles

.position_finder
  LDX collide_position            ;find the various stop places in the grid using our positions above
  LDA vertical_positions,x
  STA collide_position
  INX
  LDA vertical_positions,x
  LDX collide_position+1
  STA collide_position+1
  DEC collide_position+1          ;must change this if guy flashes
  LDA horizontal_positions,x
  CLC
  ADC #$06                        ;must change this if guy flashes
  STA collide_position+2
  INX
  LDA horizontal_positions,x
  STA collide_position+3

  LDY #$00
  LDA [collideptr],y
  AND #%00111111                 ;load the correct meta tile bank address
  ASL A
  TAX

  LDA meta_tile_sets,x           ;load the meta tile set pointer 
  STA meta_tile_sets_ptr
  LDA meta_tile_sets+1,x
  STA meta_tile_sets_ptr+1

  LDX #$00                       ;start out at X = zero

.load_collision_data:

  LDA collide_metatile,x         ;load the meta tile found earlier
  ASL A
  TAY

  LDA [meta_tile_sets_ptr],y     ;load the pointer info for that metatile
  STA meta_tile_ptr
  INY
  LDA [meta_tile_sets_ptr],y
  STA meta_tile_ptr+1

  LDY #$04                       ;load the collision data from the meta tile string
  LDA [meta_tile_ptr],y
  STA collide_data,x             ;store it for later use

  INX
  CPX #$04                       ;repeat for each corner of the meta sprite
  BNE .load_collision_data
  RTS

;----------------------------------------------------------------------------------------------------------

Collision_Detection:

  LDA #$00                        ;clear out the indexes 
  STA collision_index
  STA collision_index+1
  STA collision_index+2
  STA collision_index+3
  STA collide_position
  STA collide_position+1
  STA collide_position+2
  STA collide_position+3

  JSR Find_Collision_Meta_Tiles   ;find the collision data from the meta tiles

  LDX enemy_number                ;we only need to check the direction we are currently traveling
  LDA enemy_direction,x
  BEQ .up
  CMP #$01
  BEQ .down
  CMP #$02
  BEQ .right
  JMP .left

.right
  LDA collide_data+1              ;load the data we found
  AND #%00000010                  ;check only the right direction data
  BEQ .right1                     ;no impact, check the bottom of the sprite
  LDA collide_position+2          ;load the position setting we found
  STA sprite_horizontal           ;store it in the horizontal position of the sprite
  JMP .continue_on                ;jump to the next step
.right1
  LDA collide_data+3              ;same thing on the bottom
  AND #%00000010
  BEQ .right2
  LDA collide_position+2
  STA sprite_horizontal
  JMP .continue_on
.right2                           ;no impact?  Don't do shit!!
  RTS

.left
  LDA collide_data                ;same as above.
  AND #%00000001
  BEQ .left1
  LDA collide_position+3
  STA sprite_horizontal
  JMP .continue_on
.left1
  LDA collide_data+2
  AND #%00000001
  BEQ .left2
  LDA collide_position+3
  STA sprite_horizontal
  JMP .continue_on
.left2
  RTS

.up 
  LDA collide_data
  AND #%00001000
  BEQ .up1
  LDA collide_position+1
  STA sprite_vertical
  JMP .continue_on
.up1
  LDA collide_data+1
  AND #%00001000
  BEQ .up2
  LDA collide_position+1
  STA sprite_vertical
  JMP .continue_on
.up2
  RTS
  
.down
  LDA collide_data+2
  AND #%00000100
  BEQ .down1
  LDA collide_position
  STA sprite_vertical
  JMP .continue_on
.down1
  LDA collide_data+3
  AND #%00000100
  BEQ .down2
  LDA collide_position
  STA sprite_vertical
  JMP .continue_on
.down2
  RTS

.continue_on
  LDA enemy_number
  BEQ .done
  JSR direction_change                          ;need to add this in here so that the enemies don't just sit there and run at the walls
.done                                           ;errrr....helments!!   
  RTS

;-----------------------------------------------------------------------------------------------------

;;;;;;;;;;;;
;;;;;;;;;;;;

  .bank 2
  .org $C000

;;;;;;;;;;;;
;;;;;;;;;;;;

CHR_Data:
  .db $10,$00                                   ;background address in the PPU

  .incbin "CHR_Files/mario1.chr"

TileUpdates:
  .word Question1,Question2,Question3,Question4,Question5,Question6,Question7,Question8
  .word Question9,Question10,Question11,Question12,Question13,Question14,Question15,Question16

Question1:                                      ;this is all the data for the various ? block frames
  .incbin "CHR_Files/Question1.chr"

Question2:
  .incbin "CHR_Files/Question2.chr"

Question3:
  .incbin "CHR_Files/Question3.chr"

Question4:
  .incbin "CHR_Files/Question4.chr"

Question5:
  .incbin "CHR_Files/Question5.chr"

Question6:
  .incbin "CHR_Files/Question6.chr"

Question7:
  .incbin "CHR_Files/Question7.chr"

Question8:
  .incbin "CHR_Files/Question8.chr"

Question9:
  .incbin "CHR_Files/Question9.chr"

Question10:
  .incbin "CHR_Files/Question10.chr"

Question11:
  .incbin "CHR_Files/Question11.chr"

Question12:
  .incbin "CHR_Files/Question12.chr"

Question13:
  .incbin "CHR_Files/Question13.chr"

Question14:
  .incbin "CHR_Files/Question14.chr"

Question15:
  .incbin "CHR_Files/Question15.chr"

Question16:
  .incbin "CHR_Files/Question16.chr"

;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;

  .bank 3
  .org $E000

;;;;;;;;;;;;
;;;;;;;;;;;;

Sprite_Data:
  .db $00,$00                             ;sprite address in the PPU

  .incbin "CHR_Files/SpriteMovement.chr"  ;include the sprite graphics data

;;;;;;;;;;;;

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial

;;;;;;;;;;;;
;;;;;;;;;;;;




