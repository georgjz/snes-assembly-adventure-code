; -----------------------------------------------------------------------------
;   File: Vector.s
;   Description: Interrupt and reset vectors for the 65816 CPU
; -----------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
;-------------------------------------------------------------------------------

;----- Imports -----------------------------------------------------------------
.import     NMIHandler          ; We need these two addresses so the SNES ...
.import     ResetHandler        ; ... knows where you start execution
;-------------------------------------------------------------------------------

.segment "VECTOR"
;-------------------------------------------------------------------------------
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
