; Copyright (C) 2018 Georg Ziegler
;
; Permission is hereby granted, free of charge, to any person obtaining a copy of
; this software and associated documentation files (the "Software"), to deal in
; the Software without restriction, including without limitation the rights to
; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
; of the Software, and to permit persons to whom the Software is furnished to do
; so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
; -----------------------------------------------------------------------------
;   File: SNESHeader.s
;   Author(s): Georg Ziegler
;   Description: This file contains SNES Header information and the
;   interrupt/reset vectors needed by the CPU
;

;-------------------------------------------------------------------------------
;   Imports from other files
;-------------------------------------------------------------------------------
.import     ResetHandler        ; The entry point after start up/reset
.import     NMIHandler          ; Interrupt called on V-Blank
.import     IRQHandler          ; IRQ is not used in this project
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   ROM Register Data are stored in $00:ffb0 through $00:ffdf, 48/$30 bytes
;-------------------------------------------------------------------------------
.segment "HEADER"
.byte   "GZ"                    ; Maker Code, I use my initals
.byte   "NEKO"                  ; Game Code
.byte   $00, $00, $00, $00      ; 7 bytes of zero/$00
.byte   $00, $00, $00
.byte   $00                     ; No expansion RAM used
.byte   $00                     ; No special version
.byte   $00                     ; Sub-Cartridge number, no used
        ;0123456789abcdef01234
.byte   "SNES Neko Library    " ; Name Title, 21 bytes long
.byte   $20                     ; Map Mode, Mode 20 2.68 MHz (normal speed)
.byte   $00                     ; Cartridge Type, ROM only, no co-processor
.byte   $09                     ; ROM Size, 3 ~ 4 MBit
.byte   $00                     ; RAM Size, no RAM
.byte   $02                     ; Destination Code, Europe
.byte   $33                     ; Fixed Value
.byte   $00                     ; ROM Revision Number, start at $00
.word   $aaaa                   ; Complement Check
.word   $5555                   ; Check Sum, hope to write a script to automate this

;-------------------------------------------------------------------------------
;   Interrupt and Reset vectors for the 65816 CPU
;-------------------------------------------------------------------------------
.segment "VECTOR"
; native mode   COP,        BRK,        ABT,
.addr           CopHandler, BrkHandler, AbtHandler
;               NMI,        RST,        IRQ
.addr           NMIStub,    $ffff,      IRQStub

.word    $0000, $0000           ; four used bytes

; emulation m.  COP,        BRK,        ABT
.addr           eCopHandler,eBrkHandler,eAbtHandler
;               NMI,        RST,        IRQ
.addr           eNMIHandler,ResetStub,  eIRQHandler

;-------------------------------------------------------------------------------
;   Stubs for the vectors
;-------------------------------------------------------------------------------
.segment "RESETVECTOR"
; These stubs make sure that the Reset, NMI, and IRQ handlers are found even
; if they are located in another bank than $00/$80
NMIStub:
    jml NMIHandler

ResetStub:
    jml ResetHandler            ; jump to game's reset handler

IRQStub:
    jml IRQHandler

; Most interrupts are not used by the SNES
CopHandler:
BrkHandler:
AbtHandler:
eCopHandler:
eBrkHandler:
eAbtHandler:
eNMIHandler:
eIRQHandler:
    rti
