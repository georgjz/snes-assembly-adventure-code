; -----------------------------------------------------------------------------
;   File: SpriteDemo.s
;   Description: Displays four simple sprites
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
VMDATAH     = $2119     ; data for VRAM write
CGADD       = $2121     ; address for CGRAM read and write
CGDATA      = $2122     ; data for CGRAM write
TM          = $212c     ; main screen designation
NMITIMEN    = $4200     ; enable flaog for v-blank
RDNMI       = $4210     ; read the NMI flag status
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
        lda #$8f                ; force v-blanking
        sta INIDISP
        stz NMITIMEN            ; disable NMI

        ; transfer VRAM data
        stz VMADDL              ; set the VRAM address to $0000
        stz VMADDH
        lda #$80
        sta VMAINC              ; increment VRAM address by 1 when writing to VMDATAH
        ldx #$00                ; set register X to zero, we will use X as a loop counter and offset
VRAMLoop:
        lda SpriteData, X       ; get bitplane 0/2 byte from the sprite data
        sta VMDATAL             ; write the byte in A to VRAM
        inx                     ; increment counter/offset
        lda SpriteData, X       ; get bitplane 1/3 byte from the sprite data
        sta VMDATAH             ; write the byte in A to VRAM
        inx                     ; increment counter/offset
        cpx #$80                ; check whether we have written $04 * $20 = $80 bytes to VRAM (four sprites)
        bcc VRAMLoop            ; if X is smaller than $80, continue the loop

        ; transfer CGRAM data
        lda #$80
        sta CGADD               ; set CGRAM address to $80
        ldx #$00                ; set X to zero, use it as loop counter and offset
CGRAMLoop:
        lda ColorData, X        ; get the color low byte
        sta CGDATA              ; store it in CGRAM
        inx                     ; increase counter/offset
        lda ColorData, X        ; get the color high byte
        sta CGDATA              ; store it in CGRAM
        inx                     ; increase counter/offset
        cpx #$20                ; check whether 32/$20 bytes were transfered
        bcc CGRAMLoop           ; if not, continue loop

        .byte $42, $00          ; debugger breakpoint

        ; set up OAM data
        stz OAMADDL             ; set the OAM address to ...
        stz OAMADDH             ; ...at $0000
        ; OAM data for first sprite
        lda # (256/2 - 8)       ; horizontal position of first sprite
        sta OAMDATA
        lda # (224/2 - 8)       ; vertical position of first sprite
        sta OAMDATA
        lda #$00                ; name of first sprite
        sta OAMDATA
        lda #$00                ; no flip, prio 0, palette 0
        sta OAMDATA
        ; OAM data for second sprite
        lda # (256/2)           ; horizontal position of second sprite
        sta OAMDATA
        lda # (224/2 - 8)       ; vertical position of second sprite
        sta OAMDATA
        lda #$01                ; name of second sprite
        sta OAMDATA
        lda #$00                ; no flip, prio 0, palette 0
        sta OAMDATA
        ; OAM data for third sprite
        lda # (256/2 - 8)       ; horizontal position of third sprite
        sta OAMDATA
        lda # (224/2)           ; vertical position of third sprite
        sta OAMDATA
        lda #$02                ; name of third sprite
        sta OAMDATA
        lda #$00                ; no flip, prio 0, palette 0
        sta OAMDATA
        ; OAM data for fourth sprite
        lda # (256/2)           ; horizontal position of fourth sprite
        sta OAMDATA
        lda # (224/2)           ; vertical position of fourth sprite
        sta OAMDATA
        lda #$03                ; name of fourth sprite
        sta OAMDATA
        lda #$00                ; no flip, prio 0, palette 0
        sta OAMDATA

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

        ; here we would place all of the game logic
        ; and loop forever

        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Will be called during V-Blank
;-------------------------------------------------------------------------------
.proc   NMIHandler
        lda RDNMI               ; read NMI status, acknowledge NMI

        ; this is where we would do graphics update

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