.ifndef REGISTERS_INC
REGISTERS_INC = 1
; -----------------------------------------------------------------------------
;   File: Registers.inc
;   Description: Symbols representing the memory mapped registers of the SNES
; -----------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   PPU Registers
;-------------------------------------------------------------------------------
    INIDISP     := $2100    ; Display Control 1
    OBJSEL      := $2101    ; Object Size & Object Base
    OAMADDL     := $2102    ; OAM Address, lower byte
    OAMADDH     := $2103    ; OAM Address, bit 9 & Priority Rotation
    OAMDATA     := $2104    ; OAM Data Write
    BGMODE      := $2105    ; BG Mode & BG Character Size
    MOSAIC      := $2106    ; Mosaic Size & Mosaic Enable
    BG1SC       := $2107    ; BG1 Screen Base & Screen Size
    BG2SC       := $2108    ; BG2 Screen Base & Screen Size
    BG3SC       := $2109    ; BG3 Screen Base & Screen Size
    BG4SC       := $210A    ; BG4 Screen Base & Screen Size
    BG12NBA     := $210B    ; BG1/BG2 Character Data Area Designation
    BG34NBA     := $210C    ; BG3/BG4 Character Data Area Designation
    BG1HOFS     := $210D    ; BG1 Horizontal Scroll (X) / M7HOFS
    BG1VOFS     := $210E    ; BG1 Vertical   Scroll (Y) / M7VOFS
    BG2HOFS     := $210F    ; BG2 Horizontal Scroll (X)
    BG2VOFS     := $2110    ; BG2 Vertical   Scroll (Y)
    BG3HOFS     := $2111    ; BG3 Horizontal Scroll (X)
    BG3VOFS     := $2112    ; BG3 Vertical   Scroll (Y)
    BG4HOFS     := $2113    ; BG4 Horizontal Scroll (X)
    BG4VOFS     := $2114    ; BG4 Vertical   Scroll (Y)
    VMAINC      := $2115    ; VRAM Address Increment Mode
    VMADDL      := $2116    ; VRAM Address, lower byte
    VMADDH      := $2117    ; VRAM Address, higher byte
    VMDATAL     := $2118    ; VRAM Data Write, lower byte
    VMDATAH     := $2119    ; VRAM Data Write, higher byte
    M7SEL       := $211A    ; Mode7 Rot/Scale Mode Settings
    M7A         := $211B    ; Mode7 Rot/Scale A (COSINE A) & Maths 16-Bit Operand
    M7B         := $211C    ; Mode7 Rot/Scale B (SINE A)   & Maths  8-bit Operand
    M7C         := $211D    ; Mode7 Rot/Scale C (SINE B)
    M7D         := $211E    ; Mode7 Rot/Scale D (COSINE B)
    M7X         := $211F    ; Mode7 Rot/Scale Center Coordinate X
    M7Y         := $2120    ; Mode7 Rot/Scale Center Coordinate Y
    CGADD       := $2121    ; Palette CGRAM Address
    CGDATA      := $2122    ; Palette CGRAM Data Write
    W12SEL      := $2123    ; Window BG1/BG2  Mask Settings
    W34SEL      := $2124    ; Window BG3/BG4  Mask Settings
    WOBJSEL     := $2125    ; Window OBJ/MATH Mask Settings
    WH0         := $2126    ; Window 1 Left  Position (X1)
    WH1         := $2127    ; Window 1 Right Position (X2)
    WH2         := $2128    ; Window 2 Left  Position (X1)
    WH3         := $2129    ; Window 2 Right Position (X2)
    WBGLOG      := $212A    ; Window 1/2 Mask Logic (BG1..BG4)
    WOBJLOG     := $212B    ; Window 1/2 Mask Logic (OBJ/MATH)
    TM          := $212C    ; Main Screen Designation
    TS          := $212D    ; Sub  Screen Designation
    TMW         := $212E    ; Window Area Main Screen Disable
    TSW         := $212F    ; Window Area Sub  Screen Disable
    CGSWSEL     := $2130    ; Color Math Control Register A
    CGADSUB     := $2131    ; Color Math Control Register B
    COLDATA     := $2132    ; Color Math Sub Screen Backdrop Color
    SETINI      := $2133    ; Display Control 2
; PPU Ports (Read-Only)
    MPYL        := $2134    ; PPU1 Signed Multiply Result (Lower  8-Bit)
    MPYM        := $2135    ; PPU1 Signed Multiply Result (Middle 8-Bit)
    MPYH        := $2136    ; PPU1 Signed Multiply Result (Upper  8-Bit)
    SLHV        := $2137    ; PPU1 Latch H/V-Counter By Software (Read=Strobe)
    RDOAM       := $2138    ; PPU1 OAM  Data Read
    RDVRAML     := $2139    ; PPU1 VRAM  Data Read, lower byte
    RDVRAMH     := $213A    ; PPU1 VRAM  Data Read, higher byte
    RDCGRAM     := $213B    ; PPU2 CGRAM Data Read
    OPHCT       := $213C    ; PPU2 Horizontal Counter Latch
    OPVCT       := $213D    ; PPU2 Vertical   Counter Latch
    STAT77      := $213E    ; PPU1 Status & PPU1 Version Number
    STAT78      := $213F    ; PPU2 Status & PPU2 Version Number
; APU Ports (Read/Write)
    APUIO0      := $2140    ; Main CPU To Sound CPU Communication Port 0
    APUIO1      := $2141    ; Main CPU To Sound CPU Communication Port 1
    APUIO2      := $2142    ; Main CPU To Sound CPU Communication Port 2
    APUIO3      := $2143    ; Main CPU To Sound CPU Communication Port 3
; WRAM Ports
    WMDATA      := $2180    ; WRAM Data Read/Write
    WMADDL      := $2181    ; WRAM Address (Lower  8-Bit)
    WMADDM      := $2182    ; WRAM Address (Middle 8-Bit)
    WMADDH      := $2183    ; WRAM Address (Upper  1-Bit)
; NES Style Joypad Ports
    JOYWR       := $4016    ; Joypad Output
    JOYA        := $4016    ; Joypad Input Register A
    JOYB        := $4017    ; Joypad Input Register B
;-------------------------------------------------------------------------------
;   CPU Registers
;-------------------------------------------------------------------------------
    NMITIMEN    := $4200    ; Interrupt Enable & Joypad Request
    WRIO        := $4201    ; Programmable I/O Port (Open-Collector Output)
    WRMPYA      := $4202    ; Set Unsigned  8-Bit Multiplicand
    WRMPYB      := $4203    ; Set Unsigned  8-Bit Multiplier & Start Multiplication
    WRDIVL      := $4204    ; Set Unsigned 16-Bit Dividend, lower byte
    WRDIVH      := $4205    ; Set Unsigned 16-Bit Dividend, higher byte
    WRDIVB      := $4206    ; Set Unsigned  8-Bit Divisor & Start Division
    HTIMEL      := $4207    ; H-Count Timer Setting, lower byte
    HTIMEH      := $4208    ; H-Count Timer Setting (Upper 1bit)
    VTIMEL      := $4209    ; V-Count Timer Setting, lower byte
    VTIMEH      := $420A    ; V-Count Timer Setting (Upper 1-Bit)
    MDMAEN      := $420B    ; Select General Purpose DMA Channels & Start Transfer
    HDMAEN      := $420C    ; Select H-Blank DMA (H-DMA) Channels
    MEMSEL      := $420D    ; Memory-2 Waitstate Control
    RDNMI       := $4210    ; V-Blank NMI Flag and CPU Version Number (Read/Ack)
    TIMEUP      := $4211    ; H/V-Timer IRQ Flag (Read/Ack)
    HVBJOY      := $4212    ; H/V-Blank Flag & Joypad Busy Flag
    RDIO        := $4213    ; Joypad Programmable I/O Port (Input)
    RDDIVL      := $4214    ; Unsigned Div Result (Quotient), lower byte
    RDDIVH      := $4215    ; Unsigned Div Result (Quotient), higher byte
    RDMPYL      := $4216    ; Unsigned Div Remainder / Mul Product, lower byte
    RDMPYH      := $4217    ; Unsigned Div Remainder / Mul Product, higher byte
    JOY1L       := $4218    ; Joypad 1 (Gameport 1, Pin 4), lower byte
    JOY1H       := $4219    ; Joypad 1 (Gameport 1, Pin 4), higher byte
    JOY2L       := $421A    ; Joypad 2 (Gameport 2, Pin 4), lower byte
    JOY2H       := $421B    ; Joypad 2 (Gameport 2, Pin 4), higher byte
    JOY3L       := $421C    ; Joypad 3 (Gameport 1, Pin 5), lower byte
    JOY3H       := $421D    ; Joypad 3 (Gameport 1, Pin 5), higher byte
    JOY4L       := $421E    ; Joypad 4 (Gameport 2, Pin 5), lower byte
    JOY4H       := $421F    ; Joypad 4 (Gameport 2, Pin 5), higher byte
;-------------------------------------------------------------------------------
;   DMA Registers
;-------------------------------------------------------------------------------
    DMAP0       := $4300    ; DMA0 DMA/HDMA Parameters
    BBAD0       := $4301    ; DMA0 DMA/HDMA I/O-Bus Address (PPU-Bus AKA B-Bus)
    A1T0L       := $4302    ; DMA0 DMA/HDMA Table Start Address, lower byte
    A1T0H       := $4303    ; DMA0 DMA/HDMA Table Start Address, higher byte
    A1T0B       := $4304    ; DMA0 DMA/HDMA Table Start Address (Bank)
    DAS0L       := $4305    ; DMA0 DMA Count, lower byte
    DAS0H       := $4306    ; DMA0 DMA Count, higher byte
    DAS0B       := $4307    ; DMA0 Indirect HDMA Address (Bank)
    A2A0L       := $4308    ; DMA0 HDMA Table Address, lower byte
    A2A0H       := $4309    ; DMA0 HDMA Table Address, higher byte
    NTRL0       := $430A    ; DMA0 HDMA Line-Counter

    DMAP1       := $4310    ; DMA1 DMA/HDMA Parameters
    BBAD1       := $4311    ; DMA1 DMA/HDMA I/O-Bus Address (PPU-Bus AKA B-Bus)
    A1T1L       := $4312    ; DMA1 DMA/HDMA Table Start Address, lower byte
    A1T1H       := $4313    ; DMA1 DMA/HDMA Table Start Address, higher byte
    A1T1B       := $4314    ; DMA1 DMA/HDMA Table Start Address (Bank)
    DAS1L       := $4315    ; DMA1 DMA Count, lower byte
    DAS1H       := $4316    ; DMA1 DMA Count, higher byte
    DAS1B       := $4317    ; DMA1 Indirect HDMA Address (Bank)
    A2A1L       := $4318    ; DMA1 HDMA Table Address, lower byte
    A2A1H       := $4319    ; DMA1 HDMA Table Address, higher byte
    NTRL1       := $431A    ; DMA1 HDMA Line-Counter

    DMAP2       := $4320    ; DMA2 DMA/HDMA Parameters
    BBAD2       := $4321    ; DMA2 DMA/HDMA I/O-Bus Address (PPU-Bus AKA B-Bus)
    A1T2L       := $4322    ; DMA2 DMA/HDMA Table Start Address, lower byte
    A1T2H       := $4323    ; DMA2 DMA/HDMA Table Start Address, higher byte
    A1T2B       := $4324    ; DMA2 DMA/HDMA Table Start Address (Bank)
    DAS2L       := $4325    ; DMA2 DMA Count, lower byte
    DAS2H       := $4326    ; DMA2 DMA Count, higher byte
    DAS2B       := $4327    ; DMA2 Indirect HDMA Address (Bank)
    A2A2L       := $4328    ; DMA2 HDMA Table Address, lower byte
    A2A2H       := $4329    ; DMA2 HDMA Table Address, higher byte
    NTRL2       := $432A    ; DMA2 HDMA Line-Counter

    DMAP3       := $4330    ; DMA3 DMA/HDMA Parameters
    BBAD3       := $4331    ; DMA3 DMA/HDMA I/O-Bus Address (PPU-Bus AKA B-Bus)
    A1T3L       := $4332    ; DMA3 DMA/HDMA Table Start Address, lower byte
    A1T3H       := $4333    ; DMA3 DMA/HDMA Table Start Address, higher byte
    A1T3B       := $4334    ; DMA3 DMA/HDMA Table Start Address (Bank)
    DAS3L       := $4335    ; DMA3 DMA Count, lower byte
    DAS3H       := $4336    ; DMA3 DMA Count, higher byte
    DAS3B       := $4337    ; DMA3 Indirect HDMA Address (Bank)
    A2A3L       := $4338    ; DMA3 HDMA Table Address, lower byte
    A2A3H       := $4339    ; DMA3 HDMA Table Address, higher byte
    NTRL3       := $433A    ; DMA3 HDMA Line-Counter

    DMAP4       := $4340    ; DMA4 DMA/HDMA Parameters
    BBAD4       := $4341    ; DMA4 DMA/HDMA I/O-Bus Address (PPU-Bus AKA B-Bus)
    A1T4L       := $4342    ; DMA4 DMA/HDMA Table Start Address, lower byte
    A1T4H       := $4343    ; DMA4 DMA/HDMA Table Start Address, higher byte
    A1T4B       := $4344    ; DMA4 DMA/HDMA Table Start Address (Bank)
    DAS4L       := $4345    ; DMA4 DMA Count, lower byte
    DAS4H       := $4346    ; DMA4 DMA Count, higher byte
    DAS4B       := $4347    ; DMA4 Indirect HDMA Address (Bank)
    A2A4L       := $4348    ; DMA4 HDMA Table Address, lower byte
    A2A4H       := $4349    ; DMA4 HDMA Table Address, higher byte
    NTRL4       := $434A    ; DMA4 HDMA Line-Counter

    DMAP5       := $4350    ; DMA5 DMA/HDMA Parameters
    BBAD5       := $4351    ; DMA5 DMA/HDMA I/O-Bus Address (PPU-Bus AKA B-Bus)
    A1T5L       := $4352    ; DMA5 DMA/HDMA Table Start Address, lower byte
    A1T5H       := $4353    ; DMA5 DMA/HDMA Table Start Address, higher byte
    A1T5B       := $4354    ; DMA5 DMA/HDMA Table Start Address (Bank)
    DAS5L       := $4355    ; DMA5 DMA Count, lower byte
    DAS5H       := $4356    ; DMA5 DMA Count, higher byte
    DAS5B       := $4357    ; DMA5 Indirect HDMA Address (Bank)
    A2A5L       := $4358    ; DMA5 HDMA Table Address, lower byte
    A2A5H       := $4359    ; DMA5 HDMA Table Address, higher byte
    NTRL5       := $435A    ; DMA5 HDMA Line-Counter

    DMAP6       := $4360    ; DMA6 DMA/HDMA Parameters
    BBAD6       := $4361    ; DMA6 DMA/HDMA I/O-Bus Address (PPU-Bus AKA B-Bus)
    A1T6L       := $4362    ; DMA6 DMA/HDMA Table Start Address, lower byte
    A1T6H       := $4363    ; DMA6 DMA/HDMA Table Start Address, higher byte
    A1T6B       := $4364    ; DMA6 DMA/HDMA Table Start Address (Bank)
    DAS6L       := $4365    ; DMA6 DMA Count, lower byte
    DAS6H       := $4366    ; DMA6 DMA Count, higher byte
    DAS6B       := $4367    ; DMA6 Indirect HDMA Address (Bank)
    A2A6L       := $4368    ; DMA6 HDMA Table Address, lower byte
    A2A6H       := $4369    ; DMA6 HDMA Table Address, higher byte
    NTRL6       := $436A    ; DMA6 HDMA Line-Counter

    DMAP7       := $4370    ; DMA7 DMA/HDMA Parameters
    BBAD7       := $4371    ; DMA7 DMA/HDMA I/O-Bus Address (PPU-Bus AKA B-Bus)
    A1T7L       := $4372    ; DMA7 DMA/HDMA Table Start Address, lower byte
    A1T7H       := $4373    ; DMA7 DMA/HDMA Table Start Address, higher byte
    A1T7B       := $4374    ; DMA7 DMA/HDMA Table Start Address (Bank)
    DAS7L       := $4375    ; DMA7 DMA Count, lower byte
    DAS7H       := $4376    ; DMA7 DMA Count, higher byte
    DAS7B       := $4377    ; DMA7 Indirect HDMA Address (Bank)
    A2A7L       := $4378    ; DMA7 HDMA Table Address, lower byte
    A2A7H       := $4379    ; DMA7 HDMA Table Address, higher byte
    NTRL7       := $437A    ; DMA7 HDMA Line-Counter
;-------------------------------------------------------------------------------
;   Flags, ordered from most to least significant byte
;-------------------------------------------------------------------------------
    ; $2100 : INIDISP
    FORCED_BLANKING_OFF = $00   ; Turn forced blanking off
    FORCED_BLANKING_ON  = $80   ; Turn forced blanking on
    SCR_BRG_MASK        = $0f   ; Mask for screen brightness
    ; $2101 : OBJSEL
    OBJ_SIZE_8_16       = $00   ; Object size, small 8x8, large 16x16
    OBJ_SIZE_8_32       = $20   ; Object size, small 8x8, large 32x32
    OBJ_SIZE_8_64       = $40   ; Object size, small 8x8, large 64x64
    OBJ_SIZE_16_32      = $60   ; Object size, small 16x16, large 32x32
    OBJ_SIZE_16_64      = $80   ; Object size, small 16x16, large 64x64
    OBJ_SIZE_32_64      = $a0   ; Object size, small 32x32, large 64x64
    ; $2103 : OAMADDH
    OAM_PRIO_ROTATION   = $80   ; OAM priority rotation
    ; $2105 : BGMODE
    BG4_SIZE_8          = $00   ; Background 4 tile size 8x8
    BG4_SIZE_16         = $80   ; Background 4 tile size 16x16
    BG3_SIZE_8          = $00   ; Background 3 tile size 8x8
    BG3_SIZE_16         = $40   ; Background 3 tile size 16x16
    BG2_SIZE_8          = $00   ; Background 2 tile size 8x8
    BG2_SIZE_16         = $20   ; Background 2 tile size 16x16
    BG1_SIZE_8          = $00   ; Background 1 tile size 8x8
    BG1_SIZE_16         = $10   ; Background 1 tile size 16x16
    BG3_PRIO_OFF        = $00   ; Background 3 not highest priority in Mode 0 and 1
    BG3_PRIO_ON         = $08   ; Background 3 highest priority in Mode 0 and 1
    BG_MODE_0           = $00   ; Background Mode 0
    BG_MODE_1           = $01   ; Background Mode 1
    BG_MODE_2           = $02   ; Background Mode 2
    BG_MODE_3           = $03   ; Background Mode 3
    BG_MODE_4           = $04   ; Background Mode 4
    BG_MODE_5           = $05   ; Background Mode 5
    BG_MODE_6           = $06   ; Background Mode 6
    BG_MODE_7           = $07   ; Background Mode 7
    ; $2106 : MOSAIC
    MOSAIC_SIZE_1       = $00   ; Mosaic size 1x1
    MOSAIC_SIZE_2       = $10   ; Mosaic size 2x2
    MOSAIC_SIZE_3       = $20   ; Mosaic size 3x3
    MOSAIC_SIZE_4       = $30   ; Mosaic size 4x4
    MOSAIC_SIZE_5       = $40   ; Mosaic size 5x5
    MOSAIC_SIZE_6       = $50   ; Mosaic size 6x6
    MOSAIC_SIZE_7       = $60   ; Mosaic size 7x7
    MOSAIC_SIZE_8       = $70   ; Mosaic size 8x8
    MOSAIC_SIZE_9       = $80   ; Mosaic size 9x9
    MOSAIC_SIZE_10      = $90   ; Mosaic size 10x10
    MOSAIC_SIZE_11      = $a0   ; Mosaic size 11x11
    MOSAIC_SIZE_12      = $b0   ; Mosaic size 12x12
    MOSAIC_SIZE_13      = $c0   ; Mosaic size 13x13
    MOSAIC_SIZE_14      = $d0   ; Mosaic size 14x14
    MOSAIC_SIZE_15      = $e0   ; Mosaic size 15x15
    MOSAIC_SIZE_16      = $f0   ; Mosaic size 16x16
    BG4_MOSAIC_OFF      = $00   ; Turn off mosaic for background 4
    BG4_MOSAIC_ON       = $08   ; Turn on mosaic for background 4
    BG3_MOSAIC_OFF      = $00   ; Turn off mosaic for background 3
    BG3_MOSAIC_ON       = $04   ; Turn on mosaic for background 3
    BG2_MOSAIC_OFF      = $00   ; Turn off mosaic for background 2
    BG2_MOSAIC_ON       = $02   ; Turn on mosaic for background 2
    BG1_MOSAIC_OFF      = $00   ; Turn off mosaic for background 1
    BG1_MOSAIC_ON       = $01   ; Turn on mosaic for background 1
    ; $2107 : BG1SC
    BG1_SC_SIZE_32      = $00   ; Background 1 screen size 32x32 tiles
    BG1_SC_SIZE_64_32   = $01   ; Background 1 screen size 64x32 tiles
    BG1_SC_SIZE_32_64   = $02   ; Background 1 screen size 32x64 tiles
    BG1_SC_SIZE_64      = $03   ; Background 1 screen size 64x64 tiles
    ; $2108 : BG2SC
    BG2_SC_SIZE_32      = $00   ; Background 2 screen size 32x32 tiles
    BG2_SC_SIZE_64_32   = $01   ; Background 2 screen size 64x32 tiles
    BG2_SC_SIZE_32_64   = $02   ; Background 2 screen size 32x64 tiles
    BG2_SC_SIZE_64      = $03   ; Background 2 screen size 64x64 tiles
    ; $2109 : BG3SC
    BG3_SC_SIZE_32      = $00   ; Background 3 screen size 32x32 tiles
    BG3_SC_SIZE_64_32   = $01   ; Background 3 screen size 64x32 tiles
    BG3_SC_SIZE_32_64   = $02   ; Background 3 screen size 32x64 tiles
    BG3_SC_SIZE_64      = $03   ; Background 3 screen size 64x64 tiles
    ; $210a : BG3SC
    BG4_SC_SIZE_32      = $00   ; Background 4 screen size 32x32 tiles
    BG4_SC_SIZE_64_32   = $01   ; Background 4 screen size 64x32 tiles
    BG4_SC_SIZE_32_64   = $02   ; Background 4 screen size 32x64 tiles
    BG4_SC_SIZE_64      = $03   ; Background 4 screen size 64x64 tiles
    ; $2115 : VMAINC
    INC_TIMING_LOW      = $00   ; Increment address after write to $2118 or $2139
    INC_TIMING_HIGH     = $80   ; Increment address after write to $2119 or $213a
    VRAM_INC_2BIT       = $04   ; Increment value by 8 (for 32 times), 2bpp format
    VRAM_INC_4BIT       = $08   ; Increment value by 8 (for 64 times), 4bpp format
    VRAM_INC_8BIT       = $0c   ; Increment value by 8 (for 128 times), 8bpp format
    VRAM_INC_1BY1       = $00   ; Address increments 1 by 1
    VRAM_INC_32BY32     = $01   ; Address increments 32 by 32
    VRAM_INC_128BY128   = $02   ; Address increments 128 by 128

.endif
