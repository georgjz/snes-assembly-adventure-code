; -----------------------------------------------------------------------------
;   File: Main.s
;   Description: Displays a sprite movable with the dpad
; -----------------------------------------------------------------------------

;----- Export ------------------------------------------------------------------
.export     ResetHandler        ; export the entry point of the demo
.export     NMIHandler
;-------------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
;-------------------------------------------------------------------------------

;----- Includes ----------------------------------------------------------------
.include "Assets.inc"
.include "GameConstants.inc"
.include "Init.inc"
.include "Joypad.inc"
.include "MemoryMapWRAM.inc"
.include "PPU.inc"
.include "Registers.inc"
;-------------------------------------------------------------------------------

.segment "CODE"
;-------------------------------------------------------------------------------
;   This is the entry point of the demo
;-------------------------------------------------------------------------------
.proc   ResetHandler
        sei                     ; disable interrupts
        clc                     ; clear the carry flag
        xce                     ; switch the 65816 to native (16-bit mode)
        rep #$10                ; set X and Y to 16-bit
        sep #$20                ; set A to 8-bit
        lda #$8f                ; force v-blanking
        sta INIDISP
        stz NMITIMEN            ; disable NMI
        ; set the stack pointer to $1fff
        ldx #$1fff              ; load X with $1fff
        txs                     ; copy X to stack pointer
        jsr InitDemo            ; initialize the demo
        jmp GameLoop            ; enter main game loop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Executed during V-Blank
;-------------------------------------------------------------------------------
.proc   GameLoop
        wai                                 ; wait for NMI / V-Blank

        ; read joypad 1
        ; push addresses of joy pad data to stack
        ldx #JOYHELD1
        phx
        ldx #JOYTRIGGER1
        phx
        ldx #JOYRAW1
        phx
        jsr ReadJoypad1
        ; stack clean up
        plx
        plx
        plx

        ; check the dpad, if any of the directional buttons where pressed or held,
        rep #$20                            ; set A to 16-bit
        ; move the sprites accordingly
CheckUpButton:
        lda #$0000                          ; set A to zero
        ora JOYTRIGGER1                     ; check whether the up button was pressed this frame...
        ora JOYHELD1                        ; ...or held from last frame
        and #UP_BUTTON
        beq CheckUpButtonDone               ; if neither has occured, move on
        ; else, move sprites up
        ldy #$0000                          ; Y is the loop counter
        ldx #$0001                          ; set offset to 1, to manipulate sprite vertical positions
        sep #$20                            ; set A to 8-bit
MoveSpritesUp:
        lda OAMMIRROR, X
        sec
        sbc VER_SPEED
        bcc CorrectVerticalPositionDown     ; if vertical position is below zero, correct it down
        sta OAMMIRROR, X
        inx                                 ; increment X by 4
        inx
        inx
        inx
        iny
        cpy #$0004                          ; unless Y = 4, continue loop
        bne MoveSpritesUp
CheckUpButtonDone:
        rep #$20                            ; set A to 16-bit

CheckDownButton:
        lda #$0000                          ; set A to zero
        ora JOYTRIGGER1                     ; check whether the down button was pressed this frame...
        ora JOYHELD1                        ; ...or held from last frame
        and #DOWN_BUTTON
        beq CheckDownButtonDone             ; if neither has occured, move on
        ; else, move sprites down
        ldy #$0000                          ; Y is the loop counter
        ldx #$0001                          ; set offset to 1, to manipulate sprite vertical positions
        sep #$20                            ; set A to 8-bit
        ; check if sprites move below buttom boundry
        lda OAMMIRROR, X
        clc
        adc VER_SPEED
        cmp #(SCREEN_BOTTOM - 2 * SPRITE_SIZE)
        bcs CorrectVerticalPositionUp
MoveSpritesDown:
        lda OAMMIRROR, X
        clc
        adc VER_SPEED
        sta OAMMIRROR, X
        inx                                 ; increment X by 4
        inx
        inx
        inx
        iny
        cpy #$0004                          ; unless Y = 4, continue loop
        bne MoveSpritesDown
CheckDownButtonDone:
        rep #$20                            ; set A to 16-bit
        jmp CheckLeftButton                 ; continue input check

CorrectVerticalPositionDown:
        sep #$20                            ; set A to 8-bit
        stz OAMMIRROR + 1                   ; sprite 1, vertical position
        stz OAMMIRROR + 5                   ; sprite 3, vertical position
        lda #SPRITE_SIZE
        sta OAMMIRROR + 9                   ; sprite 2, vertical position
        sta OAMMIRROR + 13                  ; sprite 4, vertical position
        rep #$20                            ; set A to 16-bit
        jmp CheckLeftButton                 ; continue input check

CorrectVerticalPositionUp:
        sep #$20                            ; set A to 8-bit
        lda #(SCREEN_BOTTOM - 2 * SPRITE_SIZE)
        sta OAMMIRROR + 1                   ; sprite 1, vertical position
        sta OAMMIRROR + 5                   ; sprite 3, vertical position
        lda #(SCREEN_BOTTOM - SPRITE_SIZE)
        sta OAMMIRROR + 9                   ; sprite 2, vertical position
        sta OAMMIRROR + 13                  ; sprite 4, vertical position
        rep #$20                            ; set A to 16-bit

CheckLeftButton:
        lda #$0000                          ; set A to zero
        ora JOYTRIGGER1                     ; check whether the up button was pressed this frame...
        ora JOYHELD1                        ; ...or held from last frame
        and #LEFT_BUTTON
        beq CheckLeftButtonDone             ; if neither has occured, move on
        ; else, move sprites up
        ldy #$0000                          ; Y is the loop counter
        ldx #$0000                          ; set offset to 0, to manipulate sprite horizontal positions
        sep #$20                            ; set A to 8-bit
MoveSpritesLeft:
        lda OAMMIRROR, X
        sec
        sbc HOR_SPEED
        bcc CorrectHorizontalPositionRight
        sta OAMMIRROR, X
        inx                                 ; increment X by 4
        inx
        inx
        inx
        iny
        cpy #$0004                          ; unless Y = 4, continue loop
        bne MoveSpritesLeft
CheckLeftButtonDone:
        rep #$20                            ; set A to 16-bit

CheckRightButton:
        lda #$0000                          ; set A to zero
        ora JOYTRIGGER1                     ; check whether the down button was pressed this frame...
        ora JOYHELD1                        ; ...or held from last frame
        and #RIGHT_BUTTON
        beq CheckRightButtonDone            ; if neither has occured, move on
        ; else, move sprites down
        ldy #$0000                          ; Y is the loop counter
        ldx #$0000                          ; set offset to 0, to manipulate sprite horizontal positions
        sep #$20                            ; set A to 8-bit
        ; check whether sprites move beyond right boundry
        lda OAMMIRROR, X
        clc
        adc HOR_SPEED
        cmp #(SCREEN_RIGHT - 2 * SPRITE_SIZE)
        bcs CorrectHorizontalPositionLeft
MoveSpritesRight:
        lda OAMMIRROR, X
        clc
        adc HOR_SPEED
        sta OAMMIRROR, X
        inx                                 ; increment X by 4
        inx
        inx
        inx
        iny
        cpy #$0004                          ; unless Y = 4, continue loop
        bne MoveSpritesRight
CheckRightButtonDone:
        rep #$20                            ; set A to 16-bit
        jmp InputDone

CorrectHorizontalPositionRight:
        sep #$20                            ; set A to 8-bit
        stz OAMMIRROR + 0                   ; sprite 1, horizontal position
        stz OAMMIRROR + 8                   ; sprite 2, horizontal position
        lda #SPRITE_SIZE
        sta OAMMIRROR + 4                   ; sprite 3, horizontal position
        sta OAMMIRROR + 12                  ; sprite 4, horizontal position
        jmp InputDone

CorrectHorizontalPositionLeft:
        sep #$20                            ; set A to 8-bit
        lda #(SCREEN_RIGHT - 2 * SPRITE_SIZE)
        sta OAMMIRROR + 0                   ; sprite 1, horizontal position
        sta OAMMIRROR + 8                   ; sprite 2, horizontal position
        lda #(SCREEN_RIGHT - SPRITE_SIZE)
        sta OAMMIRROR + 4                   ; sprite 3, horizontal position
        sta OAMMIRROR + 12                  ; sprite 4, horizontal position

InputDone:
        sep #$20                            ; set A back to 8-bit

        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Will be called during V-Blank every frame
;-------------------------------------------------------------------------------
.proc   NMIHandler
        lda RDNMI               ; read NMI status, acknowledge NMI

        tsx                     ; save old stack pointer
        pea OAMMIRROR           ; push mirror address to stack
        jsr UpdateOAMRAM        ; update OAMRAM
        txs                     ; restore old stack pointer

        rti
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Is not used in this program
;-------------------------------------------------------------------------------
.proc   IRQHandler
        ; code
        rti
.endproc
;-------------------------------------------------------------------------------
