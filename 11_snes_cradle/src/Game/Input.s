; -----------------------------------------------------------------------------
;   File: Input.s
;   Description: Subroutines to handle input
; -----------------------------------------------------------------------------

;----- Export ------------------------------------------------------------------
.export     HandleInput
;-------------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
.a8
.i16
;-------------------------------------------------------------------------------

;----- Includes ----------------------------------------------------------------
.include "GameConstants.inc"
.include "MemoryMapWRAM.inc"
;-------------------------------------------------------------------------------

.segment "CODE"
;-------------------------------------------------------------------------------
;   This subroutines handles all input
;   Parameters: Raw: .addr, Trigger: .addr, Held: .addr
;-------------------------------------------------------------------------------
.proc   HandleInput
        phx                                 ; save old stack pointer
        ; create frame pointer
        phd                                 ; push Direct Register to stack
        tsc                                 ; transfer Stack to... (via Accumulator)
        tcd                                 ; ...Direct Register.
        ; use constants to access arguments on stack with Direct Addressing
        Raw         = $07                   ; address to store raw input data
        Trigger     = $09                   ; address to store triggered buttons
        Held        = $0b                   ; address to store held buttons

        rep #$20                            ; set A to 16-bit
        ; check the dpad, if any of the directional buttons where pressed or held,
        ; move the sprites accordingly
CheckUpButton:
        lda #$0000                          ; set A to zero
        ora (Trigger)                       ; check whether the up button was pressed this frame...
        ora (Held)                          ; ...or held from last frame
        and #UP_BUTTON
        beq CheckUpButtonDone               ; if neither has occured, move on
        ; else, move sprites up
        ldx #$0000                          ; X is the loop counter
        ldy #$0001                          ; Y is the offset into the OAM mirror
        sep #$20                            ; set A to 8-bit
MoveSpritesUp:
        lda OAMMIRROR, Y
        sec
        sbc VER_SPEED
        bcc CorrectVerticalPositionDown     ; if vertical position is below zero, correct it down
        sta OAMMIRROR, Y
        iny                                 ; increment Y by 4
        iny
        iny
        iny
        inx
        cpx #$0004                          ; unless X = 4, continue loop
        bne MoveSpritesUp
CheckUpButtonDone:
        rep #$20                            ; set A to 16-bit

CheckDownButton:
        lda #$0000                          ; set A to zero
        ora (Trigger)                       ; check whether the down button was pressed this frame...
        ora (Held)                          ; ...or held from last frame
        and #DOWN_BUTTON
        beq CheckDownButtonDone             ; if neither has occured, move on
        ; else, move sprites down
        ldx #$0000                          ; X is the loop counter
        ldy #$0001                          ; Y is the offset into the OAM mirror
        sep #$20                            ; set A to 8-bit
        ; check if sprites move below buttom boundry
        lda OAMMIRROR, Y
        clc
        adc VER_SPEED
        cmp #(SCREEN_BOTTOM - 2 * SPRITE_SIZE)
        bcs CorrectVerticalPositionUp
MoveSpritesDown:
        lda OAMMIRROR, Y
        clc
        adc VER_SPEED
        sta OAMMIRROR, Y
        iny                                 ; increment Y by 4
        iny
        iny
        iny
        inx
        cpx #$0004                          ; unless X = 4, continue loop
        bne MoveSpritesDown
CheckDownButtonDone:
        rep #$20                            ; set A to 16-bit
        jmp CheckLeftButton                 ; continue input check

CorrectVerticalPositionDown:
        sep #$20                            ; set A to 8-bit
        lda #$0000                          ; set A to zero
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
        ora (Trigger)                       ; check whether the up button was pressed this frame...
        ora (Held)                          ; ...or held from last frame
        and #LEFT_BUTTON
        beq CheckLeftButtonDone             ; if neither has occured, move on
        ; else, move sprites up
        ldx #$0000                          ; X is the loop counter
        ldy #$0000                          ; Y is the offset into the OAM mirror
        sep #$20                            ; set A to 8-bit
MoveSpritesLeft:
        lda OAMMIRROR, Y
        sec
        sbc HOR_SPEED
        bcc CorrectHorizontalPositionRight
        sta OAMMIRROR, Y
        iny                                 ; increment X by 4
        iny
        iny
        iny
        inx
        cpx #$0004                          ; unless Y = 4, continue loop
        bne MoveSpritesLeft
CheckLeftButtonDone:
        rep #$20                            ; set A to 16-bit

CheckRightButton:
        lda #$0000                          ; set A to zero
        ora (Trigger)                       ; check whether the down button was pressed this frame...
        ora (Held)                          ; ...or held from last frame
        and #RIGHT_BUTTON
        beq CheckRightButtonDone            ; if neither has occured, move on
        ; else, move sprites down
        ldx #$0000                          ; X is the loop counter
        ldy #$0000                          ; Y is the offset into the OAM mirror
        sep #$20                            ; set A to 8-bit
        ; check whether sprites move beyond right boundry
        lda OAMMIRROR, Y
        clc
        adc HOR_SPEED
        cmp #(SCREEN_RIGHT - 2 * SPRITE_SIZE)
        bcs CorrectHorizontalPositionLeft
MoveSpritesRight:
        lda OAMMIRROR, Y
        clc
        adc HOR_SPEED
        sta OAMMIRROR, Y
        iny                                 ; increment X by 4
        iny
        iny
        iny
        inx
        cpx #$0004                          ; unless Y = 4, continue loop
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
        pld                                 ; restore D...
        plx                                 ; ...and X registers

        rts
.endproc
;-------------------------------------------------------------------------------
