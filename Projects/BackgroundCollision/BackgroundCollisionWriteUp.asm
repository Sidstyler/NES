9/15/10


INTRODUCTION

This week we are going to add in the (in my opinion) hardest part so far.  This one takes some math skills, the ability to visualize exactly what the program is doing, and a complete understanding of how our pointers/data tables are set up.  If you understand everything up to this point, you should be okay.  If not, you're Fucked.  But don't dispair, if you get stuck or don't understand, just ask questions.  

A couple things that I need to say before we get started:

1.  The concept for this routine is 100% original, as is the actual code and the process.  I had hints from people on NA and nesdev (mostly it was them telling me that I was attempting to do it wrong at the start), but this I came up with pretty much on my own.

2.  There are a couple things that could be corrected in the program.  Mainly that there are no walls in certain places and the routine doesn't exactly know what to do with the end of the screen, so it makes the NPCs jump to a random point when it hits the edge of the screen.  Easily corrected though.

3.  This program is easily modified for use in a platformer.  If I get time in the near future, I'll go through that as well.

4.  This program could also be modified to support scrolling.  I might do this too if I ever get around to it.

5.  Other than the collision routine its self, which isn't all that complex, all the PRG space that this takes up is what we need in the way of collision data: one (1) byte per meta tile.  :D

6.  This routine is based on the meta tiles and meta sprites that we've built up to this point.  You could modify it to support different ways of doing things, but then you'd loose the compression effects.

7.  Ever thus to deadbeats.


CONCEPTS

First we need to lay out what we're aiming for.  We want some sort of routine that will detect when a sprite comes into contact with a piece of background that we define as "solid" and/or do something when it impacts a certain part of the screen.  Then we need to tell the program what to do when this impact(s) occur(s).  So, if we think about this, we need to somehow have the following steps.

1.  Calculate the meta sprite's position on the screen.

2.  Pull some sort of collision data based on this position and the current background.

3.  Determine from this collision data if we need to move the character (or do something else).

4.  Move the character.

5.  Reset the graphics.

6.  Repeat for each character/meta tile. 

7.  ???

8.  Profit. 

Note that we are not going to cover anything besides "simple collision" here.  Once you have the program working that pulls the collision data, the various add ons (or op-codes) are fairly simple to input.


COLLISION DATA

I suppose that the best place to start would be for us to decide how we are going to define our collision data.  Well, this is pretty simple.  Remember our meta tiles??  Let's jump back to them.  Remember this picture:

INSERT GRAPHICS2 PICTURE

Here, we are going to keep everything simple.  "Solid" meta tile blocks and "hollow" meta tile blocks, however the process is exactly the same for something that only impacts in one direction.  (Think the bricks in Mario Bros. that you can jump up through, but can't fall down through.)  We are going to store all the data for each meta tile in one byte.  Basically:

%   0   ,   0   ,   0   ,   0   , 0,  0 ,  0  ,  0
%Op-Code,Op-Code,Op-Code,Op-Code,Up,Down,Right,Left

Where, for each bit:
0 = hollow
-and-
1 = solid

-or-

0 = do nothing
-and-
1 = activate op-code (Note that this limits you to 4 op codes, you may add more pretty easily.)

So, if we wanted something to only register an impact when we are:

-going up,         %00001000
-going down,       %00000100 (heh, going down, get it?)
-going right,      %00000010
-going left,       %00000001
-going up/down,    %00001100
-goint right/left, %00000011
-etc.

Nice and simple.  And it keeps our data easy to read, which is always a plus.  


META TILE COLLISION DATA

Again, for our purposes, we are going to keep it simple.  While the program fully supports the various settings described in the previous section, we are going to stick with %00000000 and %00001111, hollow or solid.  Once you complete this, you should go back and change some of these settings to see what it actually does.

So, if we make the only "hollow" meta tile the "sky" tile and the rest of them "solid", our meta tile bank turns into this:

;------------------------------------------------------------------------------------

MetaTileSeta00
  .db $44,$49,$5F,$7A,%00001111

MetaTileSeta01
  .db $AB,$AD,$AC,$AE,%00001111

MetaTileSeta02
  .db $53,$54,$55,$56,%00001111

MetaTileSeta03
  .db $B0,$B2,$B1,$B3,%00001111

MetaTileSeta04
  .db $A5,$A6,$A7,$A8,%00001111

MetaTileSeta05
  .db $7B,$7C,$7D,$7E,%00001111

MetaTileSeta06
  .db $24,$24,$24,$24,%00000000

MetaTileSeta07
  .db $45,$45,$47,$47,%00001111

MetaTileSeta08
  .db $47,$47,$47,$47,%00001111

MetaTileSeta09
  .db $CA,$CB,$CC,$CD,%00001111

MetaTileSeta0A
  .db $C6,$C7,$C8,$C9,%00001111

MetaTileSeta0B
  .db $6B,$6C,$70,$71,%00001111

MetaTileSeta0C
  .db $6D,$6C,$72,$71,%00001111

MetaTileSeta0D
  .db $6D,$6F,$72,$74,%00001111

;----------------------------------------------------------------------------------

See, this is all we have to specify our collision data.  You could either have this monstorous set of "CMP"s for each direction (as in the sprite collisions), that takes on the order of 750 bytes per room, or one byte per meta tile.  You decide.


THE "collideptr" POINTER

One thing that we need to add that is kind of random is a pointer that supports the collision routine.  This needs to be set up in the "Room Loading" routine.  Note that technically here you could use the "ptr1" that we are using in the decompression routine, but as your program gets more complex...well, it's just better to have something as important as collision detection use its own variables.  So, in the "LoadbgBackground" routine:

;----------------------------------------------------------------------------------

;;;;;;;;;;;;;insert;;;;;;;;;;;;;

  ASL A                                ;indexing to a table of words

  STA room_index
 
  LDX room_index                       ;using the room index, load the pointer to the correct background data
  LDA backgroundpointer,X
  STA ptr1
  LDA backgroundpointer+1,X
  STA ptr1+1

;;;;;;;;;;;;;insert;;;;;;;;;;;;;

-becomes-

;;;;;;;;;;;;;insert;;;;;;;;;;;;;

  ASL A                                ;indexing to a table of words

  STA room_index
 
  LDX room_index                       ;using the room index, load the pointer to the correct background data
  LDA backgroundpointer,X
  STA ptr1
  STA collideptr                       ;store the room data for collision detection
  LDA backgroundpointer+1,X
  STA ptr1+1
  STA collideptr+1

;;;;;;;;;;;;;insert;;;;;;;;;;;;;

;----------------------------------------------------------------------------------

Notice that our collideptr is pointing to the background data table for the room that we are loading!  i.e. the table of meta tiles...see where we are going??


COLLISION POSITION REQUIREMENTS

If we take a step back and think about this for a moment, we have a bit of a problem.  Consider the following picture:

INSERT COLLISIONEXAMPLE PICTURE

Assume that the red square represents our metasprite and that the block/sky is the space that it is currently occupying over the background.  You can see in each example picture, we are impacting the same 4 background meta tiles, but depending on EXACTLY where the 4 corners of our metasprite are, we would require completely different results from our collision routine.  

Take the upper left hand example in the picture.  If we used the bottom of the meta sprite as the collision coordinates for moving to the right, we wouldn't register an impact, but the top would.  Same with the upper, 2nd from the left.  Or, if we were trying to move up, if we used the coordinates of the left side of the meta sprite, we wouldn't register a hit but the left side would.  Work your way through each of the 8 pictures to be sure you understand this point.  So what do we do about this little problem?

Panic?  Sure.  Next take a breather and consider the following.  We just need our program to compute the location of each of the 4 corners.  Then if we were using the upper left picture and moving right, we would simply say:

-Look at the bottom, right corner.  No impact.  
-Look at the top, right corner.  Oh shit!  An impact!!

Simple.  Understand?  Same with moving up:

-Look at the top, left corner.  No impact.
-Look at the top, right corner.  COLLISION! Slap ass crazy.

So, now that we know that we have to compute the location of 4 different corners, we can define a few variables to help us along our way.  I'm just going to put these out there and define them better as we go along.

;----------------------------------------------------------------------------------

collision_index  .rs 4        ;indexes for the look up of meta tile info in the main collision routine
collide_metatile  .rs 4       ;place to store collision meta tile data
collide_vertical  .rs 4       ;store the index data for the sprite positions in the main collision routine 
collide_horizontal  .rs 4

collide_data  .rs 4           ;actual data from the meta tile streams
collide_position  .rs 4       ;the location to set him to if he hits something

;----------------------------------------------------------------------------------


DEFINING THE META SPRITE COLLISION BOUNDARIES

First, we need to find the positions that we will be using to define our collision coordinates for these 4 corners.  Remember how before we referenced everything to the "sprite_vertical" and "sprite_horizontal"?  Well this is why.  When we loop our different characters through this routine, we will be using all of our registers in another fashion.  So, we need to simply transfer their coordinates to these variables before we call the collision routine, then, if there is a collision, modify these variables, then use these variables to transfer the data back into the RAM space.  

In this routine, we are going to need to use these two variables to specify the location of each of the 4 corners.  Note that here we are assuming that all of our meta sprites are the same.  If you use different sizes, you'll have to add something based on enemy type for this.  Consider this picture and the variables that we introduced above:

INSERT COLLISION BOUNDARIES PICTURE

You can set your boundaries however you want, but if you don't set the bottom to the bottom of the feet, your character will "float" and if you don't leave a couple pixels at the top, your character will have trouble fitting between meta tiles.  Sameish for the right/left.  So, to load our values for our collide_horizontal and collide_vertical variables, we simply write:

;----------------------------------------------------------------------------------

.sprite_transferBGCD:

  LDA sprite_vertical              ;transfer correct collision box coords
  CLC
  ADC #$02                         ;to change this, must change DEC below to keep him from flashing (comes later)
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

;----------------------------------------------------------------------------------

Note that in the following, some of this is not necessary.  However, I'm leaving it in here because if you expand the program, you will need all these variables later.


FINDING META TILES 

Now that we have our dude's location on the background entered in temporary variables, we need to use them to calculate some sort of index to pull the meta tile information from our room data.  And after we have the meta tile info, we use that to pull the collision data we specified eariler...but one thing at a time.  

Let's start with the top left corner of our meta sprite and a picture.  

INSERT ROOM EXAMPLE PICTURE

We see that a room is composed of 16x16 (or $10x$10) pixel meta tiles.  Consider this room and the data table that the room loading routine pulls data from.  We see that each $10 pixels we go down, we are going another $10 entries into this table (or 1 row down, hence formatting the room data talbes this way)...or, for the left most meta tile:

-row 0 (top row) = $00
-row 1 = $10
-row 2 = $20
-etc.  

So, to specify which meta tile the collision coordinates are occupying in the vertical direction, we simply take the vertical position:

-compare it to $10
	-if lower, we have found our meta tile
	-if higher
		-subtract $10 from the vertical position
		-add $10 to our collision_index variable then jump back to the start of the loop

Now, we've specified which row our guy is in, so now we need to account for the horizontal portion of the position.  This is pretty much the same as the vertical part, except that for every $10 you go to the right, you only add $01 entry to our collision_index variable.  Make sense?

Think about it.  If we are in:

-column 0 = $X0
-column 1 = $X1
-column 2 = $X2
-etc.  

So, to add in the horizontal part of our position, we simply take the horizontal position and:

-compare it to $10
	-if lower, we have found our meta tile
	-if higher
		-subtract $10 from the horizontal position
		-add $01 to our collision_index variable then jump back to the start of the loop

So, now we've found the value for "collision_index".  Does this make sense?  This is basically the hardest part, so if you don't get it, stop and ask questions.  All we're doing is using the location of our meta sprite and translating that to an index to pull data from the specific room we are in's background data table.  

Now, we have 4 corners that we need to consider when running collision detection.  So, this implies that we will need to run this little loop 4 times, 1 time for each corner.  However, this is resource intensive, it will basically take our program twice as long to run...so we are going to take a short cut.  Remember how our collision box above always keeps the right side a fixed distance from the left side?  Well, we can use that information to cut the 2nd and 4th loop out of the program.

So, if we take the value for collision_index (upper left corner) and store it in collision_index+1, we can modify collision_index+1 to specify the upper right corner.  Think back to the last time through the loop for the horizontal portion.  We compare the value to $10 and if it is less than this, we just disregard this unused portion and jump down to the next step...however, if we do it correctly, the remainder should still be in the register, in this case "Y".  So, if we take this remainder, add the fixed horizontal distance that we specified in our meta tile collision boundaries, we will come up with the horizontal location of our right corner!!  So now, we simply take this value and:

-compare it to $10
	-if lower, collide_index = collide_index+1 (both sides of the meta sprite are on top of the same meta tile)
	-if higher
		-add $01 to our collision_index+1 variable
		-jump to the next part (we see that collision_index+1 will always equal collision_index or be 1 greater than)

Now if we've done this correctly, we should have an accurate location of each of the top corners with respect to the background data.  However, if we look back at our background data, we have one minor problem...the first entry in the table is always the meta tile bank number.  So we need to INC collision_index and collision_index+1 to account for this fact.

NOW we have the correct location of the data we are seeking.

The next step is simple.  Use these index numbers to pull the meta tiles from the room data.  Then store these meta tiles into the variables we set up, collision_metatile and collision_metatile+1.  

Finally, we jump back to the start and repeat the process for the two bottom corners.  

Simply stated in code, we do the following:

;----------------------------------------------------------------------------------

  LDX #$00

.find_collision_meta_tiles

  LDA collide_vertical,x          ;find the index for the upper left corner of the sprite intersect
;  SEC
;  SBC #$40                       ;take off the HUD if you are using one to save loop time.  You'll have to change the vertical positions as well
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

;----------------------------------------------------------------------------------


SPECIFING POSITIONS TO RESET META SPRITES 

You'll notice the variable set "collide_position" in this routine.  These are the variables that we use to specify where we want our meta sprite to be reset to given an impact in a certain direction.  You'll see that the values of collide_position and collide_position+1 are modified each time we go through our loops when we try to use the vertical and horizontal parts of our position.  So, we are counting meta tiles towards the bottom from the top of the screen and to the right of the left side of the screen.  Basically, we are setting up a grid that our sprites will snap to if they hit something.  

Note that you only need the data acquired in the first time through the loop but you have to leave it in there cause we are using the "X" index.  

Now, after we've set up a variable to track the position like this, we need some sort of data set that translates this info into pixel coordinates.  Like so:

;----------------------------------------------------------------------------------

vertical_positions:
  .db $00,$0F,$1F,$2F,$3F,$4F,$5F,$6F,$7F,$8F,$9F,$AF,$BF,$CF,$DF        ;this is what we set the position data to when we hit something
horizontal_positions:
  .db $00,$0D,$1D,$2D,$3D,$4D,$5D,$6D,$7D,$8D,$9D,$AD,$BD,$CD,$DD,$ED

;----------------------------------------------------------------------------------

This is a somewhat iterative process that I did by trial and error after freaking error.  I'm sure if you tried hard enough, you could make this part cleaner, but at some point you just say "Fuck It" and move on.  By using the same data sets as described above for, say, up and down collision, you run into a slight "flashing" problem.  Basically, your dude runs through a wall before the program registers the impact, then the next frame the impact registers, then it moves the guy for that frame, then you move into the wall again, on and on, flashing one frame in each location, i.e.:

 Frame 1               Frame 2                     Frame 3
   |                    |                            |
  o|ooo register hit    |oooo keep moving sprite    o|ooo
  o|ooo ------------>   |oooo ------------------>   o|ooo and so on.
  o|ooo reset sprite    |oooo to the left           o|ooo
   |                    |                            |

So, we have to make some minor corrections to the vertical_positions and horizontal_positions data to make this flashing stop.  In the end, to save variable space, we overwrite these position variables that we calculated above with the actual values that we will set our meta sprite positions to.  This is a pretty simple part of the routine, suffice it to say that when it's done, you'll end up with this:

collision_position = position to set the meta sprite to if moving DOWN
collision_position+1 = position to set the meta sprite to if moving UP
collision_position+2 = position to set the meta sprite to if moving RIGHT
collision_position+3 = position to set the meta sprite to if moving LEFT

-or, the actual code-

;----------------------------------------------------------------------------------

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

;----------------------------------------------------------------------------------


PULLING COLLISION DATA FROM META TILE BANKS

Now that we have everything pretty much set up, we need to do some simple looks ups to pull data from our meta tables.  i.e. the 1 byte per meta tile that we specified in the first part of this write up.  This part of the routine would run as follows:

-load the meta tile bank number (first entry of the room data stream) and store the pointer info in our "meta_tile_sets_ptr" pointer
-start a loop for X = 0, 1, 2, 3
-load the collide_metatile+X that we found above and pull the value from our room data table
-using our meta_tile_sets_ptr and the collide_metatile values, load the pointer meta_tile_ptr with the address of the meta tile we are checking collision with
-pull the 4th entry from the meta tile data stream and store it in the variable collide_data+X for later use
-jump back to the start of the loop unless X = 3

-or-

;----------------------------------------------------------------------------------

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

;----------------------------------------------------------------------------------

Simply put, this uses the data that we've compiled throughout this exercise and finally pulls the collision data from our game's memory.  If we've made it this far, we are just about done!  Just need to tie up some loose ends and viola!  Success.  


COLLISION DETECTION

Now that we've got this, it is a simple matter of checking a couple positions and determining if we need to move our meta sprite or not.  

Each time we run our Collision_Detection subroutine, the first thing we need to do is clear out some variables: the collision_index variable set and the collide_position set.  You'll notice on these variables we never actually store a value to them.  We either INC them or ADC to them.  So, if we don't reset them every time, we'd have some residual data that would fuck up our routine.

;----------------------------------------------------------------------------------

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

;----------------------------------------------------------------------------------

Next, we JSR to the "Find_Collision_Meta_Tiles" subroutine (i.e. find the collision data) that we just constructed above.  Note that you don't need to JSR here, but if you modify this for platformer use, you'll have to.

Then, if you think about it, in this "top-down" type game, you only run in one direction at any given time...so you only need to check collision in the direction you're moving.  This saves us some effort.  (Again, with a platformer, you are moving up/down and right/left at the SAME time...keep that in mind.)  So, we need ot construct a simple routine that tells the program which direction we want to check:

;----------------------------------------------------------------------------------

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

;----------------------------------------------------------------------------------

Then we will jump to the various directions.  Note that these directions are all pretty much the same, so we'll just go over one of them.  But first, remember the AND function that we learned about from MetalSlime?  We use it here.  You'll remember that we specified the solid/hollow collision data with one bit for each direction in a single byte to save space.  So, we'll have to use the AND command to pull the data for a single direction from this byte.  So, to check collision if we are moving UP:

-load the variable "collide_data" into A (note that this is the upper LEFT corner)
-AND #%00001000 (the UP bit)
	-if A=0, there is no collision, jump to "up1"
	-if A is not = 0, we have a collision!!!  
		-load the correct "collide_position" value that we calculated into A (+1 in this case)
		-store A in sprite_vertical (move the meta sprite to the collision boundary)
		-jump to .continue_on
.up1
-load the variable "collide_data+1" into A (note that this is the upper RIGHT corner)
-AND #%00001000 (the UP bit)
	-if A=0, there is no collision, jump to "up2" (a RTS command)
	-if A is not = 0, we have a collision!!!  
		-load the correct "collide_position" value that we calculated into A (+1 in this case)
		-store A in sprite_vertical (move the meta sprite to the collision boundary)
		-jump to .continue_on
.continue_on (this is just to make the meta sprites do something when they hit a wall, otherwise, they'd just sit there and keep running at the wall)
-check to see if it is the PC (if it is, we don't want to mess with it because we control it)
	-yes, RTS
	-no, continue
-JSR to our direction_change subroutine so that the meta sprite "bounces" off the wall.  However, you can put whatever you want here.  
-RTS

We do this for all four directions and we're done!!

;----------------------------------------------------------------------------------

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

;----------------------------------------------------------------------------------


CALLING THE COLLISION DETECTION ROUTINE

Now that we've written the routine, we'll need to call it either in NMI or our Main Program.  Putting lenghtly stuff in NMI is bad, so we'll put it in the main program.  Think about where we should put it.  We'll have one place for our PC JSR group and on place in our NPC loop.  

For the PC, if you think about it, the place that makes sense is right after we "handle_input".  That way we move our dude based on the controller input, check to see if he hits a wall, then run the graphics updates.  If you put it after the graphics updates, you'd have to run them again if you moved the meta sprite during collision which is a waste of time.  

Note that these "transfer_location" routines are necessary to run the Collision_Detection routine we've set up, but you don't have to make them their own routine.  I do it this way because when I make my own programs, I reference EVERYTHING to the sprite_vertical and sprite_horizontal variables (rather than the Sprite_RAM+X stuff we use here).  I think that the Sprite_RAM stuff is easier to grasp and visualize when you are first starting.  It is a little more difficult to grasp when you use the same variables for everything.  But referencing everything to these two variables makes your routines faster and easier to loop through, IMHO.  But to each his own.

Back on topic, we update our PC JSR group as follows:

;----------------------------------------------------------------------------------

;;;;;;;;;;;;;insert;;;;;;;;;;;;;

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

;;;;;;;;;;;;;insert;;;;;;;;;;;;;

;----------------------------------------------------------------------------------

Then it's basically the same thing in the enemy update routine:

;----------------------------------------------------------------------------------

;;;;;;;;;;;;;insert;;;;;;;;;;;;;

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

;;;;;;;;;;;;;insert;;;;;;;;;;;;;

;----------------------------------------------------------------------------------


END GAME

I think that's about it.  You should have a pretty good start to a background collision engine.  I would suggest that you take and modify the rooms to see that it infact updates the collision detection automatically.  Then change some of the collision properties of the various meta tiles.  Then add in some simple op-codes like infront/behind some hollow background stuff or something.  Play around and see what you can come up with.  Maybe expand the map to be sure you understand the room loading/switching routines.

Modifying this for use in a platformer is pretty simple.  I'll try to sit down and write something up for that when I get time.  Scrolling is somewhat more complex.  Basically you'd just have to use your scroll values to modify your index that pulls data from the background tables and the collide_position variables will need changing.  If I do this one, it will be a while as scrolling and I don't really see eye to eye...YET!  

Please enjoy and let me know if you have any questions.

Until next time 8==D!!
