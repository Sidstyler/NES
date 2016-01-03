;;;;;;;;;;;;  VARIABLES------------

  .rsset $0000  ;start from location 0

tile_loader_ptr  .rs 2        ;for the chr rom data

sleeping  .rs 1               ;main program flag
updating_background  .rs 2    ;0 = nothing, 1 = main program is updating the room

Enemy_Animation  .rs 4        ;Animation Counters
Enemy_Frame  .rs 4            ;Animation Frame Number

random_direction1  .rs 1      ;random direction counter
random_direction2  .rs 1      ;random direction counter

enemy_direction  .rs 4        ;direction for enemys; 0=up,1=down,2=right,3=left

enemy_pointer  .rs $08        ;pointer for the graphics data for Enemys
enemygraphicspointer  .rs 02  ;pointer for the graphics updates

enemy_number  .rs 1           ;enemy number for direction routine 0=Crewman, 1=punisher, 2=McBoobins, 3=ArseFace
enemy_ptrnumber  .rs 1        ;enemy pointer number (i.e. 2x the enemy number, 0=Crewman, 2=punisher, 4=McBoobins, 6=ArseFace

;;;;;;;;;;;;;;;;;;  CONSTANTS----------

enemyFrames1 = $0C  ;enemy's counter resets
enemyFrames2 = $18
enemyFrames3 = $24
enemyFrames4 = $30

sprite_RAM = $0200   ;constant for sprite updates

enemy_speed = $01