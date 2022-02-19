; -----------------------------------------------------------------------------
;   File: CollatzConjecture.s
;   Description: Calculate the total stopping time of the Collatz Conjecture on a SNES
; -----------------------------------------------------------------------------
NMITIMEN    = $4200     ; enable flaog for v-blank
RDNMI       = $4210     ; read the NMI flag status

.segment "CODE"
.proc   ResetHandler
        ; switch to emulation mode, set X and Y to 16-bit
        clc                     ; clear the carry flag...
        xce                     ; ...and exchange with emulation flag
        rep #$10                ; clear the Index Register Select flag
        sep #$20                ; set the Memory/Accumulator Select flag
        ; set the stack pointer to $1fff
        ldx #$1fff              ; load X with $1fff
        txs                     ; copy X to stack pointer

        ; calculate total stopping time of 27/$1b
        lda #$1b                ; start value n = 27
        pha                     ; store argument on stack
        lda #$00                ; push a empty byte to stack to store return value
        pha
        jsr Collatz             ; jump to subroutine
        pla                     ; A = $6f = 111
        .byte $42, $00          ; breakpoint for bsnes+
        pla                     ; reset stack pointer
Loop:   jmp Loop                ; loop forever
.endproc

;-------------------------------------------------------------------------------
;   Description: This subroutine will calculate the total stopping time for a given
;   8-bit integer.
;   Parameters: n : .byte
;   Returns: r : .byte
;-------------------------------------------------------------------------------
.proc   Collatz
        phd                     ; push Direct Register to stack
        tsc                     ; transfer Stack to... (via Accumulator)
        tcd                     ; ...Direct Register.
        ; use constants to access variables on stack with Direct Addressing
        StepCount = $05         ; resulting number of steps
        Input = $06             ; input number n
        ; create local variable for calculations
        lda Input               ; get input n
        rep #$20                ; set A to 16-bit
        and #$00ff              ; clear B/upper nibble of Accumulator
        pha                     ; create local variable on stack
        ; check if the result is one
CheckOne:
        lda $01, S              ; load current value
        dec                     ; decrement it by one
        beq Return              ; if current value is one, return to caller
        ; else, check if current value is even
CheckEven:
        lda #$0001              ; A = $0001
        and $01, S              ; bit-wise AND $0001 with current value
        bne CheckOdd            ; if bit 0 is set, then current value is odd
        pla                     ; pull the current value from stack
        lsr                     ; divide it by 2
        pha                     ; push it back to the stack
        inc StepCount           ; increment result by one
        bra CheckOne            ; go check if the new value is equal to 1
        ; if the current value is neither 1 or even, then it is odd
CheckOdd:
        jsr MulByThree          ; multiply current value by three
        pla                     ; pull current value from stack
        inc                     ; increment by one
        pha                     ; push new value back to stack
        inc StepCount           ; increment step count
        bra CheckEven           ; check if new value is even
        ; calculating steps/total stepping time done
Return:
        pla                     ; pull current value to restore stack pointer
        sep #$20                ; set A back to 8-bit
        pld                     ; restore caller's frame pointer
        rts                     ; return to caller
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Description: This subroutine multiplies a given 16-bit value by three
;   Parameters: n : .word
;-------------------------------------------------------------------------------
.proc   MulByThree
        ; create frame pointer for this subroutine
        phd                     ; push Direct Register to stack
        tsc                     ; transfer Stack to... (via Accumulator)
        tcd                     ; ...Direct Register.
        ; constants to access input
        Input = $05
        ; 3 * n = n + n + n
        lda Input               ; get the current value
        clc                     ; clear carry flag
        adc Input               ; add value to itself twice
        adc Input
        sta Input               ; store input back on stack
        pld                     ; restore caller's frame pointer
        rts                     ; return to caller
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