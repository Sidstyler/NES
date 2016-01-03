;;;;;;;;;;;;  VARIABLES------------

  .rsset $0000  ;start from location 0

tile_loader_ptr  .rs 2        ;for the chr rom data and tile loader routine
tile_loader_addy  .rs 2       ;address to the tile data

enemy_pointer  .rs 8          ;pointer for the graphics data for Enemys
enemygraphicspointer  .rs 2   ;pointer for the graphics updates

Main_Pointer  .rs 2           ;pointer to the main program loop
NMI_Pointer  .rs 2            ;pointer to the NMI routine

ptr1  .rs 2                   ;pointer to the background information

meta_tile_sets_ptr  .rs 2     ;pointer to the meta tile sets
meta_tile_ptr  .rs 2          ;finding the meta tile that is active

collideptr  .rs 2             ;pointer to the room info for collision detection

;---------------------------------------------------

  .rsset $0100  ;stack

  .rsset $0200  ;sprites

  .rsset $0300  ;sound

;---------------------------------------------------

  .rsset $0400  ;other variables

GameState  .rs 2              ;current game state, and state that is saved
GameStateOld  .rs 1           ;old game state

sleeping  .rs 1               ;main program flag
updating_background  .rs 2    ;0 = nothing, 1 = main program is updating the room

Enemy_Animation  .rs 4        ;Animation Counters
Enemy_Frame  .rs 4            ;Animation Frame Number

random_direction  .rs 1       ;random direction counter
random_direction1  .rs 1      ;random direction counter
random_direction2  .rs 1      ;random direction counter

enemy_direction  .rs 4        ;direction for enemys; 0=up,1=down,2=right,3=left
enemy_number  .rs 1           ;enemy number for direction routine 0=Crewman, 1=punisher, 2=McBoobins, 3=ArseFace
enemy_ptrnumber  .rs 1        ;enemy pointer number (i.e. 2x the enemy number, 0=Crewman, 2=punisher, 4=McBoobins, 6=ArseFace)

joypad1  .rs 1                ;see the "strobe controllers" routine
joypad1_old  .rs 1
joypad1_pressed  .rs 1

sprite_vertical  .rs 1        ;vertical location of the active sprite
sprite_horizontal  .rs 1      ;horizontal location of the active sprite

arrow_direction  .rs 1        ;direction that the arrow is traveling

A_timer  .rs 1                ;timer for the A button
B_timer  .rs 1                ;timer for the B button

arrow_vertical  .rs 2         ;vertical position of the arrow for collision
hammer_vertical  .rs 2        ;hammer
PC_vertical  .rs 2            ;PC

arrow_horizontal  .rs 2       ;horizontal position of the arrow for collision
hammer_horizontal  .rs 2      ;hammer
PC_horizontal  .rs 2          ;PC

tile_loader_counter  .rs 2    ;counters for the tile loading
tile_loader_stop  .rs 1       ;how many tiles to load
repeat_meta_tile  .rs 1       ;times to repeat the current meta tile

room_index  .rs 1             ;which room to load
data_y  .rs 1                 ;y counter
data_x  .rs 1                 ;x counter
background_row  .rs $20       ;the second row that each meta tile contains

Y_coord  .rs 1                ;Y map coordinate
X_coord  .rs 1                ;X map coordinate

collision_index  .rs 4        ;indexes for the look up of meta tile info in the main collision routine
collide_metatile  .rs 4       ;place to store collision meta tile data
collide_vertical  .rs 4       ;store the index data for the sprite positions in the main collision routine 
collide_horizontal  .rs 4

collide_data  .rs 4           ;actual data from the meta tile streams
collide_position  .rs 4       ;the location to set him to if he hits something
 






;;;;;;;;;;;;;;;;;;  CONSTANTS----------

enemyFrames1 = $0C    ;enemy's counter resets
enemyFrames2 = $18
enemyFrames3 = $24
enemyFrames4 = $30

sprite_RAM = $0200    ;constant for enemy updates
Weapons_RAM = $0240   ;constant for the weapons updates

enemy_speed = $01
arrow_speed = $02

TopDownView = $00     ;Main top down view is game state #0
PauseState = $01      ;Paused state is game state #1

Btimer = $0F          ;timer for the hammer 
Atimer = $0C          ;shooting pose timer for the arrows

top_exit = $0B        ;room switching locations
bottom_exit = $D8
left_exit = $0C
right_exit = $F0
