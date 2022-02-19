; -----------------------------------------------------------------------------
;   File: extralife.s
;   Description: a simple coin counter example
; -----------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816                           ; this is 65816 code
.i16                            ; X and Y registers are 16 bit
.a8                             ; A register is 8 bit
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   This is were the magic happens
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
;   Main Game Loop: All code here runs while the SNES is drawing
;-------------------------------------------------------------------------------
.proc   GameLoop                ; The main game loop
        wai                     ; wait for NMI interrupt
        jmp GameLoop            ; jump to beginning of main game loop
.endproc

;-------------------------------------------------------------------------------
;   NMI Routine: All code here runs during vblank
;-------------------------------------------------------------------------------
.proc   NMIHandler              ; NMIHandler, called every frame/V-blank
        lda $4210               ; read NMI status
        rti                     ; interrupt done, return to main game loop
.endproc

.proc   ExtraLife
        ; We initalize two "variables" with zero and three.
        ; We will use the memory locations $00:0000 and $00:0001
        ; to store the number of coins and lives.
        lda #$00                ; load the A register with zero...
        sta $0000               ; ...and store in memory at $00:0000
        lda #$03                ; load the A register with three...
        sta $0001               ; ...and store in memory at $00:0001

        ; ...here goes some game code...

        ; now let's check the number of coins
        lda $0000               ; load the number of coins into the accumulator
        cmp #100                ; compare the number in accumulator to 100
        bcc Done                ; if the number in accumulator is less than 100, jump to done
        lda #$00                ; else, load the accumulator with zero...
        sta $0000               ; ...and reset the coin counter
        lda $0001               ; next, get the current number of lives...
        clc                     ; ...clear the carry flag...
        adc #$01                ; ...and add one to the number in A
        sta $0001               ; store the new number of lives
Done:                           ; all done
.endproc

; ...some more game code...
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
