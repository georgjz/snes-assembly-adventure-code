; -----------------------------------------------------------------------------
;   File: BouncingSprite.s
;   Description: Displays a sprite that bounces off the edges
; -----------------------------------------------------------------------------

;----- Aliases/Labels ----------------------------------------------------------
; these are aliases for the Memory Mapped Registers we will use
INIDISP     = $2100     ; inital settings for screen
OBJSEL      = $2101     ; object size $ object data area designation
OAMADDL     = $2102     ; address for accessing OAM
OAMADDH     = $2103
OAMDATA     = $2104     ; data for OAM write
VMAINC      = $2115     ; VRAM address increment value designation
VMADDL      = $2116     ; address for VRAM read and write
VMADDH      = $2117
VMDATAL     = $2118     ; data for VRAM write
VMDATAH     = $2119
CGADD       = $2121     ; address for CGRAM read and write
CGDATA      = $2122     ; data for CGRAM write
TM          = $212c     ; main screen designation
NMITIMEN    = $4200     ; enable flaog for v-blank
MDMAEN      = $420b     ; DMA enable register
RDNMI       = $4210     ; read the NMI flag status
DMAP0       = $4300     ; DMA control register, channel 0
BBAD0       = $4301     ; DMA destination register, channel 0
A1T0L       = $4302     ; DMA source address register low, channel 0
A1T0H       = $4303     ; DMA source address register high, channel 0
A1T0B       = $4304     ; DMA source address register bank, channel 0
DAS0L       = $4305     ; DMA size register low, channel 0
DAS0H       = $4306     ; DMA size register high, channel 0
;-------------------------------------------------------------------------------

;----- Memory Map WRAM ---------------------------------------------------------
HOR_SPEED   = $0300     ; the horizontal speed
VER_SPEED   = $0301     ; the vertical speed
OAMMIRROR   = $0400     ; location of OAMRAM mirror in WRAM
;-------------------------------------------------------------------------------

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
;-------------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
;-------------------------------------------------------------------------------

;----- Includes ----------------------------------------------------------------
.segment "SPRITEDATA"
SpriteData: .incbin "Sprites.vra"
ColorData:  .incbin "SpriteColors.pal"
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

        ; load sprites into VRAM
        tsx                     ; save current stack pointer
        pea $0000               ; push VRAM destination address to stack
        pea SpriteData          ; push sprite source address to stack
        lda #$80                ; push number of bytes (128/$80) to transfer to stack
        pha
        jsr LoadVRAM            ; transfer VRAM data
        txs                     ; "delete" data on stack by restoring old stack pointer

        ; load color data into CGRAM
        tsx                     ; save current stack pointer
        lda #$80                ; push CGRAM destination address to stack
        pha
        pea ColorData           ; push color source address to stack
        lda #$20                ; push number of bytes (32/$20) to transfer to stack
        pha
        jsr LoadCGRAM           ; transfer color data into CGRAM
        txs                     ; "delete" data on stack by restoring old stack pointer

        ; set up initial data in the OAMRAM mirror, use X as index
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
        lda #(SCREEN_RIGHT/2)               ; sprite 2, horizontal position
        sta OAMMIRROR, X
        inx                                 ; increment index
        lda #(SCREEN_BOTTOM/2 - SPRITE_SIZE); sprite 2, vertical position
        sta OAMMIRROR, X
        inx
        lda #$01                            ; sprite 2, name
        sta OAMMIRROR, X
        inx
        lda #$00                            ; no flip, palette 0
        sta OAMMIRROR, X
        inx
        ; lower-left sprite
        lda #(SCREEN_RIGHT/2 - SPRITE_SIZE) ; sprite 3, horizontal position
        sta OAMMIRROR, X
        inx                                 ; increment index
        lda #(SCREEN_BOTTOM/2)              ; sprite 3, vertical position
        sta OAMMIRROR, X
        inx
        lda #$02                            ; sprite 3, name
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
        lda #$ff                ; set the coordinates to (255, 255), which is off screen
OAMLoop:
        sta OAMMIRROR, X
        inx
        cpx #OAMMIRROR_SIZE
        bne OAMLoop

        ; correct extra OAM byte for first four sprites
        ldx #$0200
        lda #$00
        sta OAMMIRROR, X

        ; set initial horizontal and vertical speed
        lda #SPRITE_SPEED
        sta HOR_SPEED
        sta VER_SPEED

        ; .byte $42, $00          ; debugger breakpoint

        ; make Objects visible
        lda #$10
        sta TM
        ; release forced blanking, set screen to full brightness
        lda #$0f
        sta INIDISP
        ; enable NMI, turn on automatic joypad polling
        lda #$81
        sta NMITIMEN

        jmp GameLoop            ; all initialization is done
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   After the ResetHandler will jump to here
;-------------------------------------------------------------------------------
; .smart ; keep track of registers widths
.proc   GameLoop
        wai                     ; wait for NMI / V-Blank

        .byte $42, $00          ; debugger breakpoint
        ; Game Logic: Move the sprites
        ; move sprite 1 horizontally
        ; check collision left boundary
        lda HOR_SPEED
        bpl RightBoundaryCheck  ; if sprites are moving right, skip left boundary check
        lda OAMMIRROR
        clc
        adc HOR_SPEED
        bcs UpdateHorPosition
            ; else, reposition sprite 1 to horizontal position to zero
            stz OAMMIRROR           ; reposition sprite 1
            bra InvertHorSpeed      ; invert the horizontal speed
        ; check right boundary
RightBoundaryCheck:
        lda OAMMIRROR
        clc
        adc HOR_SPEED
        cmp #(SCREEN_RIGHT - 2 * SPRITE_SIZE)
        bcc UpdateHorPosition   ; if sprite 1 is two sprites-wide to the left of right boundary, no collision
            ; else, reposition sprite 1 horizontally
            lda #(SCREEN_RIGHT - 2 * SPRITE_SIZE)
            sta OAMMIRROR           ; reposition sprite 1 horizontally
            bra InvertHorSpeed      ; invert the horizontal speed
UpdateHorPosition:
        sta OAMMIRROR           ; if no collision happened, store new position
        bra VerticalCheck       ; check vertical collision
        ; invert the horizontal speed after bouncing of the left/right screen boundary
InvertHorSpeed:
        lda HOR_SPEED           ; load current horizontal speed
        eor #$ff                ; flip all bits
        clc                     ; add 1 to inverted speed
        adc #$01
        sta HOR_SPEED           ; store inverted speed

        ; move sprite 1 vertically
VerticalCheck:
        ; check collision upper boundary
        lda VER_SPEED
        bpl CheckLowerBoundary  ; if sprites are moving down, skip upper boundary check
        lda OAMMIRROR + $01
        clc
        adc VER_SPEED
        bcs UpdateVerPosition
            ; else, reposition sprite 1 to horizontal position to zero
            stz OAMMIRROR + $01     ; reposition sprite 1
            bra InvertVerSpeed      ; invert the horizontal speed
        ; check lower boundary
CheckLowerBoundary:
        lda OAMMIRROR + $01
        clc
        adc VER_SPEED
        cmp #(SCREEN_BOTTOM - 2 * SPRITE_SIZE)
        bcc UpdateVerPosition
            ; else, reposition sprite 1 horizontally
            lda #(SCREEN_BOTTOM - 2 * SPRITE_SIZE)
            sta OAMMIRROR + $01     ; reposition sprite 1 horizontally
            bra InvertVerSpeed      ; invert the horizontal speed
UpdateVerPosition:
        sta OAMMIRROR + $01     ; if no collision happened, store new position
        bra UpdateOtherSprites  ; check vertical collision
        ; invert the horizontal speed after bouncing of the left/right screen boundary
InvertVerSpeed:
        lda VER_SPEED           ; load current horizontal speed
        eor #$ff                ; flip all bits
        clc                     ; add 1 to inverted speed
        adc #$01
        sta VER_SPEED           ; store inverted speed

UpdateOtherSprites:
        lda OAMMIRROR           ; get new horizontal position of sprite 1
        sta OAMMIRROR + $08     ; update sprite 3
        clc
        adc #SPRITE_SIZE
        sta OAMMIRROR + $04     ; update sprite 2
        sta OAMMIRROR + $0c     ; update sprite 4
        ; vertical position
        lda OAMMIRROR + $01     ; get new horizontal position of sprite 1
        sta OAMMIRROR + $05     ; update sprite 2
        clc
        adc #SPRITE_SIZE
        sta OAMMIRROR + $09     ; update sprite 3
        sta OAMMIRROR + $0d     ; update sprite 4

        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Will be called during V-Blank every frame
;-------------------------------------------------------------------------------
.proc   NMIHandler
        lda RDNMI               ; read NMI status, acknowledge NMI

        ; this is where we would do graphics update
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

;-------------------------------------------------------------------------------
;   Load sprite data into VRAM
;   Parameters: NumBytes: .byte, SrcPointer: .addr, DestPointer: .addr
;-------------------------------------------------------------------------------
.proc   LoadVRAM
        phx                     ; save old stack pointer
        ; create frame pointer
        phd                     ; push Direct Register to stack
        tsc                     ; transfer Stack to... (via Accumulator)
        tcd                     ; ...Direct Register.
        ; use constants to access arguments on stack with Direct Addressing
        NumBytes    = $07       ; number of bytes to transfer
        SrcPointer  = $08       ; source address of sprite data
        DestPointer = $0a       ; destination address in VRAM

        ; set destination address in VRAM, and address increment after writing to VRAM
        ldx DestPointer         ; load the destination pointer...
        stx VMADDL              ; ...and set VRAM address register to it
        lda #$80
        sta VMAINC              ; increment VRAM address by 1 when writing to VMDATAH

        ; loop through source data and transfer to VRAM
        ldy #$0000              ; set register Y to zero, we will use Y as a loop counter and offset
VRAMLoop:
        lda (SrcPointer, S), Y  ; get bitplane 0/2 byte from the sprite data
        sta VMDATAL             ; write the byte in A to VRAM
        iny                     ; increment counter/offset
        lda (SrcPointer, S), Y  ; get bitplane 1/3 byte from the sprite data
        sta VMDATAH             ; write the byte in A to VRAM
        iny                     ; increment counter/offset
        cpy NumBytes            ; check whether we have written $04 * $20 = $80 bytes to VRAM (four sprites)
        bcc VRAMLoop            ; if X is smaller than $80, continue the loop

        ; all done
        pld                     ; restore caller's frame pointer
        plx                     ; restore old stack pointer
        rts
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Load color data into CGRAM
;   NumBytes: .byte, SrcPointer: .byte, DestPointer: .addr
;-------------------------------------------------------------------------------
.proc   LoadCGRAM
        phx                     ; save old stack pointer
        ; create frame pointer
        phd                     ; push Direct Register to stack
        tsc                     ; transfer Stack to... (via Accumulator)
        tcd                     ; ...Direct Register.
        ; use constants to access arguments on stack with Direct Addressing
        NumBytes    = $07       ; number of bytes to transfer
        SrcPointer  = $08       ; source address of sprite data
        DestPointer = $0a       ; destination address in VRAM

        ; set CGDRAM destination address
        lda DestPointer         ; get destination address
        sta CGADD               ; set CGRAM destination address

        ldy #$0000              ; set Y to zero, use it as loop counter and offset
CGRAMLoop:
        lda (SrcPointer, S), Y  ; get the color low byte
        sta CGDATA              ; store it in CGRAM
        iny                     ; increase counter/offset
        lda (SrcPointer, S), Y  ; get the color high byte
        sta CGDATA              ; store it in CGRAM
        iny                     ; increase counter/offset
        cpy NumBytes            ; check whether 32/$20 bytes were transfered
        bcc CGRAMLoop           ; if not, continue loop

        ; all done
        pld                     ; restore caller's frame pointer
        plx                     ; restore old stack pointer
        rts
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Copies the OAMRAM mirror into OAMRAM
;-------------------------------------------------------------------------------
.proc   UpdateOAMRAM
        phx                     ; save old stack pointer
        ; create frame pointer
        phd                     ; push Direct Register to stack
        tsc                     ; transfer Stack to... (via Accumulator)
        tcd                     ; ...Direct Register.
        ; use constants to access arguments on stack with Direct Addressing
        MirrorAddr  = $07       ; address of the mirror we want to copy

        ; set up DMA channel 0 to transfer data to OAMRAM
        lda #%00000010          ; set DMA channel 0
        sta DMAP0
        lda #$04                ; set destination to OAMDATA
        sta BBAD0
        ldx MirrorAddr          ; get address of OAMRAM mirror
        stx A1T0L               ; set low and high byte of address
        stz A1T0B               ; set bank to zero, since the mirror is in WRAM
        ldx #$0220              ; set the number of bytes to transfer
        stx DAS0L

        lda #$01                ; start DMA transfer
        sta MDMAEN

        ; OAMRAM update is done, restore frame and stack pointer
        pld                     ; restore caller's frame pointer
        plx                     ; restore old stack pointer
        rts
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Interrupt and Reset vectors for the 65816 CPU
;-------------------------------------------------------------------------------
.segment "VECTOR"
; native mode   COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           NMIHandler, $0000,      $0000

.word           $0000, $0000    ; four unused bytes

; emulation m.  COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           $0000,      ResetHandler, $0000
;-------------------------------------------------------------------------------