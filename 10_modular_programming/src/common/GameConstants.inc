.ifndef GAME_CONSTANTS_INC
GAME_CONSTANTS_INC = 1
; -----------------------------------------------------------------------------
;   File: GameConstants.inc
;   Description: Constant values used by this demo
; -----------------------------------------------------------------------------

;----- Game Constants ----------------------------------------------------------
    ; we use these constants to check for collisions with the screen boundaries
SCREEN_LEFT     = $00   ; left screen boundary = 0
SCREEN_RIGHT    = $ff   ; right screen boundary = 255
SCREEN_TOP      = $00   ; top screen boundary = 0
SCREEN_BOTTOM   = $df   ; bottom screen boundary = 223
    ; a simple constant to define the sprite movement speed
SPRITE_SPEED    = $02   ; the sprites will move 2 pixel per frame
    ; this makes the code a bit more readable
SPRITE_SIZE     = $08   ; sprites are 8 by 8 pixel
OAMMIRROR_SIZE  = $0220 ; OAMRAM can hold data for 128 sprites, 4 bytes each
    ; constants to use as masks
UP_BUTTON       = $0800
DOWN_BUTTON     = $0400
LEFT_BUTTON     = $0200
RIGHT_BUTTON    = $0100
;-------------------------------------------------------------------------------

.endif
