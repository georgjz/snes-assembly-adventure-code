; -----------------------------------------------------------------------------
;   File: Subroutine.s
;   Description: Shows an example subroutine on a 65816
; -----------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816                           ; this is 65816 code
.i16                            ; X and Y registers are 16 bit
.a8                             ; A register is 8 bit
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   This is where execution starts
;-------------------------------------------------------------------------------
.segment "CODE"
.proc   ResetHandler            ; program entry point
        sei                     ; disable interrupts
        clc                     ; clear the carry flag...
        xce                     ; ...and switch to native mode

        lda #$81                ; enable...
        sta $4200               ; ...non-maskable interrupt

        rep #$10                ; clear the Index Register Select flag
        sep #$20                ; set the Memory/Accumulator Select flag

        ; load values into X and Y
        ldx #$1212              ; X = $1212
        ldy #$cccc              ; Y = $cccc
        jsr AddXtoY             ; jump to subroutine

        jmp GameLoop            ; initialisation done, jump to game loop
.endproc

;-------------------------------------------------------------------------------
;   Main Game Loop: All code here runs while the SNES is drawing
;-------------------------------------------------------------------------------
.proc   GameLoop                ; The main game loop
        wai                     ; wait for NMI interrupt
        jmp GameLoop            ; jump to beginning of main game loop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   NMI Routine: All code here runs during vblank
;-------------------------------------------------------------------------------
.proc   NMIHandler              ; NMIHandler, called every frame/V-blank
        lda $4210               ; read NMI status
        rti                     ; interrupt done, return to main game loop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   AddXtoY: Example subroutine
;-------------------------------------------------------------------------------
.proc   AddXtoY
        rep #$20                ; set A to 16-bit
        phx                     ; save X on stack
        tya                     ; copy Y into A
        clc                     ; clear carry flag for addition
        adc $01,S               ; add X to A, X is stored on stack
        tay                     ; copy A into Y, result now in Y
        plx                     ; clean up stack pointer
        rts                     ; return from subroutine
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
.addr           $0000 ,     ResetHandler, $0000
;-------------------------------------------------------------------------------
