.ifndef REGISTERS_INC
REGISTERS_INC = 1
; -----------------------------------------------------------------------------
;   File: Registers.inc
;   Description: Symbols representing the memory mapped registers of the SNES
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
VMDATAH     = $2119
CGADD       = $2121     ; address for CGRAM read and write
CGDATA      = $2122     ; data for CGRAM write
TM          = $212c     ; main screen designation
HVBJOY      = $4212     ; H/V blank flags and standard controller enable flag
STDCNTRL1L  = $4218     ; data for standard controller I
STDCNTRL1H  = $4219
NMITIMEN    = $4200     ; enable flaog for v-blank
MDMAEN      = $420b     ; DMA enable register
RDNMI       = $4210     ; read the NMI flag status
DMAP0       = $4300     ; DMA control register, channel 0
BBAD0       = $4301     ; DMA destination register, channel 0
A1T0L       = $4302     ; DMA source address register low, channel 0
A1T0H       = $4303     ; DMA source address register high, channel 0
A1T0B       = $4304     ; DMA source address register bank, channel 0
DAS0L       = $4305     ; DMA size register low, channel 0
DAS0H       = $4306     ; DMA size register high, channel 0
;-------------------------------------------------------------------------------

.endif
