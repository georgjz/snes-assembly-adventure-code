.ifndef INIT_SNES_INC
INIT_SNES_INC = 1
; -----------------------------------------------------------------------------
;   File: InitSNES.inc
;   Description: Reset the SNES to a known state
; -----------------------------------------------------------------------------

;----- Import ------------------------------------------------------------------
.import     ClearRegisters      ; Clear all PPU and CPU registers of the SNES
.import     ClearVRAM           ; Clear the complete VRAM to $00
.import     ClearCGRAM          ; Clear CG-RAM to $00 (black)
.import     ClearOAMRAM         ; Clear OAM-RAM to $ff (all sprites off screen)
;-------------------------------------------------------------------------------

.endif
