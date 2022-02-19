; -----------------------------------------------------------------------------
;   File: nihil.s
;   Description: Your very first SNES game!
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

        jmp GameLoop            ; initialisation done, jump to game loop
.endproc
;-------------------------------------------------------------------------------

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