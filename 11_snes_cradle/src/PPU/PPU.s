; -----------------------------------------------------------------------------
;   File: PPU.s
;   Description: A collection of subroutines for interacting with VRAM, CGRAM,
;   OAMRAM, and the PPU.
; -----------------------------------------------------------------------------

;----- Export ------------------------------------------------------------------
.export     LoadVRAM
.export     LoadCGRAM
.export     UpdateOAMRAM
;-------------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
.A8                             ; set accumulator to 8-bit
.I16                            ; set index registers to 16-bit
;-------------------------------------------------------------------------------

;----- Includes ----------------------------------------------------------------
.include "Registers.inc"
;-------------------------------------------------------------------------------

.segment "CODE"
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
