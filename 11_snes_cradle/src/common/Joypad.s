; -----------------------------------------------------------------------------
;   File: Joypad.s
;   Description: Subroutines to read the current state of the joypads
; -----------------------------------------------------------------------------

;----- Export ------------------------------------------------------------------
.export     ReadJoypad1
.export     ReadJoypad2
;-------------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
.a8
.i16
;-------------------------------------------------------------------------------

;----- Includes ----------------------------------------------------------------
.include "MemoryMapWRAM.inc"
.include "Registers.inc"
;-------------------------------------------------------------------------------

.segment "CODE"
;-------------------------------------------------------------------------------
;   Read and update the state of joypad 1
;   Parameters: Raw: .addr, Trigger: .addr, Held: .addr
;-------------------------------------------------------------------------------
.proc   ReadJoypad1
        phx                                 ; save old stack pointer
        ; create frame pointer
        phd                                 ; push Direct Register to stack
        tsc                                 ; transfer Stack to... (via Accumulator)
        tcd                                 ; ...Direct Register.
        ; use constants to access arguments on stack with Direct Addressing
        Raw         = $07                   ; address to store raw input data
        Trigger     = $09                   ; address to store triggered buttons
        Held        = $0b                   ; address to store held buttons

        ; check whether joypad is ready
WaitForJoypad:
        lda HVBJOY                          ; get joypad status
        and #$01                            ; check whether joypad done reading...
        beq WaitForJoypad                   ; ...if not, wait a bit more

        ; first, check for newly pressed buttons since last frame
        ; A - current frame
        ; Y - auxiliar
        rep #$20                            ; set A to 16-bit
        lda (Raw)                           ; get last frame's input...
        tay                                 ; ...and store in Y
        lda JOY1L                           ; get new input from this frame
        sta (Raw)                           ; store new input from this frame
        tya                                 ; check for newly pressed buttons...
        eor (Raw)                           ; filter buttons that were not pressed last frame
        and (Raw)                           ; filter held buttons from last frame
        sta (Trigger)                       ; ...and store them
        ; second, check for buttons held from last frame
        tya                                 ; get input from last frame
        and (Raw)                           ; filter held buttons from last frame...
        sta (Held)                          ; ...store them

        ; done
        sep #$20                            ; set A back to 8-bit
        pld                                 ; restore D...
        plx                                 ; ...and X registers
        rts
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Read and update the state of joypad 2
;   Parameters: Raw: .addr, Trigger: .addr, Held: .addr
;-------------------------------------------------------------------------------
.proc   ReadJoypad2
        phx                                 ; save old stack pointer
        ; create frame pointer
        phd                                 ; push Direct Register to stack
        tsc                                 ; transfer Stack to... (via Accumulator)
        tcd                                 ; ...Direct Register.
        ; use constants to access arguments on stack with Direct Addressing
        Raw         = $07                   ; address to store raw input data
        Trigger     = $09                   ; address to store triggered buttons
        Held        = $0b                   ; address to store held buttons

        ; check whether joypad is ready
WaitForJoypad:
        lda HVBJOY                          ; get joypad status
        and #$01                            ; check whether joypad done reading...
        beq WaitForJoypad                   ; ...if not, wait a bit more

        ; first, check for newly pressed buttons since last frame
        ; A - current frame
        ; Y - auxiliar
        rep #$20                            ; set A to 16-bit
        lda (Raw)                           ; get last frame's input...
        tay                                 ; ...and store in Y
        lda JOY2L                           ; get new input from this frame
        sta (Raw)                           ; store new input from this frame
        tya                                 ; check for newly pressed buttons...
        eor (Raw)                           ; filter buttons that were not pressed last frame
        and (Raw)                           ; filter held buttons from last frame
        sta (Trigger)                       ; ...and store them
        ; second, check for buttons held from last frame
        tya                                 ; get input from last frame
        and (Raw)                           ; filter held buttons from last frame...
        sta (Held)                          ; ...store them

        ; done
        sep #$20                            ; set A back to 8-bit
        pld                                 ; restore D...
        plx                                 ; ...and X registers

        rts
.endproc
;-------------------------------------------------------------------------------
