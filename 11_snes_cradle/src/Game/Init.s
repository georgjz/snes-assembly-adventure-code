; -----------------------------------------------------------------------------
;   File: Init.s
;   Description: Holds subroutines to initialize the demo
; -----------------------------------------------------------------------------

;----- Export ------------------------------------------------------------------
.export     InitDemo
;-------------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
.a8
.i16
;-------------------------------------------------------------------------------

;----- Includes ----------------------------------------------------------------
.include "Assets.inc"
.include "GameConstants.inc"
.include "MemoryMapWRAM.inc"
.include "PPU.inc"
.include "Registers.inc"
;-------------------------------------------------------------------------------

.segment "CODE"
;-------------------------------------------------------------------------------
;   This initializes the demo
;-------------------------------------------------------------------------------
.proc   InitDemo
        ; load sprites into VRAM
        tsx                             ; save current stack pointer
        pea $0000                       ; push VRAM destination address to stack
        pea SpriteData                  ; push sprite data source address to stack
        lda #$80                        ; number of bytes (128/$80) to transfer
        pha
        jsr LoadVRAM                    ; transfer sprite data to VRAM
        txs                             ; restore old stack pointer

        ; load color data into CGRAM
        tsx                             ; save current stack pointer
        lda #$80                        ; destination address in CGRAM
        pha
        pea ColorData                   ; color data source address
        lda #$20                        ; number of bytes (32/$20) to transfer
        pha
        jsr LoadCGRAM                   ; transfer color data into CGRAM
        txs                             ; restore old stack pointer

        ; initialize OAMRAM mirror
        ldx #$00
        ; upper-left sprite
        lda #(SCREEN_RIGHT/2 - SPRITE_SIZE) ; sprite 1, horizontal position
        sta OAMMIRROR, X
        inx                                 ; increment index
        lda #(SCREEN_BOTTOM/2 - SPRITE_SIZE); sprite 1, vertical position
        sta OAMMIRROR, X
        inx
        lda #$00                            ; sprite 1, name
        sta OAMMIRROR, X
        inx
        lda #$00                            ; no flip, palette 0
        sta OAMMIRROR, X
        inx
        ; upper-right sprite
        lda #(SCREEN_RIGHT/2)               ; sprite 3, horizontal position
        sta OAMMIRROR, X
        inx                                 ; increment index
        lda #(SCREEN_BOTTOM/2 - SPRITE_SIZE); sprite 3, vertical position
        sta OAMMIRROR, X
        inx
        lda #$01                            ; sprite 3, name
        sta OAMMIRROR, X
        inx
        lda #$00                            ; no flip, palette 0
        sta OAMMIRROR, X
        inx
        ; lower-left sprite
        lda #(SCREEN_RIGHT/2 - SPRITE_SIZE) ; sprite 2, horizontal position
        sta OAMMIRROR, X
        inx                                 ; increment index
        lda #(SCREEN_BOTTOM/2)              ; sprite 2, vertical position
        sta OAMMIRROR, X
        inx
        lda #$02                            ; sprite 2, name
        sta OAMMIRROR, X
        inx
        lda #$00                            ; no flip, palette 0
        sta OAMMIRROR, X
        inx
        ; lower-right sprite
        lda #(SCREEN_RIGHT/2)                ; sprite 4, horizontal position
        sta OAMMIRROR, X
        inx                                 ; increment index
        lda #(SCREEN_BOTTOM/2)              ; sprite 4, vertical position
        sta OAMMIRROR, X
        inx
        lda #$03                            ; sprite 4, name
        sta OAMMIRROR, X
        inx
        lda #$00                            ; no flip, palette 0
        sta OAMMIRROR, X
        inx
        ; move the other sprites off screen
        rep #$20                            ; set A to 16-bit
        lda #$f180                          ; Y = 241, X = -128
OAMLoop:
        sta OAMMIRROR, X
        inx
        inx
        cpx #(OAMMIRROR_SIZE - $20)
        bne OAMLoop
        ; correct bit 9 of horizontal/X position, set size to 8x8
        lda #$5555
OBJLoop:
        sta OAMMIRROR, X
        inx
        inx
        cpx #OAMMIRROR_SIZE
        bne OBJLoop

        sep #$20                            ; set A to 8-bit
        ; correct extra OAM byte for first four sprites
        ldx #$0200
        lda #$00
        sta OAMMIRROR, X

        ; set initial horizontal and vertical speed
        lda #SPRITE_SPEED
        sta HOR_SPEED
        sta VER_SPEED

        ; make Objects visible
        lda #$10
        sta TM
        ; release forced blanking, set screen to full brightness
        lda #$0f
        sta INIDISP
        ; enable NMI, turn on automatic joypad polling
        lda #$81
        sta NMITIMEN

        ; jmp GameLoop            ; all initialization is done
        rts                     ; all initialization is done
.endproc
;-------------------------------------------------------------------------------
