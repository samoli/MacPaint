
          .INCLUDE MAC:GRAFTYPES.TEXT
          .INCLUDE MAC:SYSEQU.TEXT
          .INCLUDE MAC:SYSMACS.TEXT
          .INCLUDE MAC:TOOLEQU.TEXT
          .INCLUDE MAC:GRAFEQU.TEXT
          .INCLUDE MAC:TOOLMACS.TEXT



;------------------------------------------------------
;
;  -->  PAINTASM.TEXT,   ASSY LANGUAGE FOR MACPAINT
;



         .SEG '        '
         .PROC EjectReset
;---------------------------------------------------------------------
;
;  PROCEDURE EjectReset;
;
;  eject default drive and re-boot Macintosh
;
         LINK     A6,#-IOVQElSize            ;allocate an IOBlock
         MOVE.L   SP,A0                      ;point to IOBlock
         CLR.L    IOVNPtr(A0)                ;Nil name pointer
         CLR.W    IOVolIndex(A0)             ;not by index
         CLR.W    IOVDrvNum(A0)              ;default drive
         _EJECT                              ;call OS
         JMP      $40000A                    ;nasty ROM address



        .PROC   %%%INIT
        .DEF    %_BEGIN,%_END,%_INIT,%_TERM

%_BEGIN CLR.L   $10(A7)                 ; fake out pascal LINK A5,...
        RTS

%_END   RTS

%_INIT  MOVE.L  (A7)+,A0                ; pop ret add
        UNLK    A5
        JMP     (A0)

%_TERM  MOVE.L  (SP)+,A0                ; get ret add
        LINK    A5,#0
        JMP     (A0)




         .SEG '       '
         .FUNC PixelTrue
;---------------------------------------------------------------------
;
;  FUNCTION PixelTrue(h,v: INTEGER; bits: BitMap): BOOLEAN;
;
         MOVE.L   (SP)+,A0                   ;POP RETURN ADDR
         MOVE.L   (SP)+,A1                   ;POP ADDR OF BITMAP
         MOVE     (SP)+,D0                   ;POP VERT COORD
         SUB      BOUNDS+TOP(A1),D0          ;CONVERT TO GLOBAL
         MULS     ROWBYTES(A1),D0            ;TIMES ROWBYTES
         MOVE     (SP)+,D1                   ;GET HORIZ COORD
         SUB      BOUNDS+LEFT(A1),D1         ;CONVERT TO GLOBAL
         MOVE.L   BASEADDR(A1),A1            ;POINT TO BASEADDR
         ADD.L    D0,A1                      ;ADD VERT OFFSET
         MOVE     D1,D0                      ;COPY HORIZ
         ASR      #3,D1                      ;DIV BY 8 FOR BYTE
         NOT      D0                         ;INVERT BITNUM
         BTST     D0,0(A1,D1.L)              ;TEST THE BIT
         SNE      (SP)                       ;STORE RESULT
         NEG.B    (SP)                       ;CONVERT TO PASCAL BOOLEAN
         JMP      (A0)                       ;AND RETURN



         .SEG '       '
         .FUNC Monkey
;---------------------------------------------------------------------
;
;  FUNCTION Monkey: BOOLEAN;
;
         TST      MonkeyLives                ;IS THE MONKEY ACTIVE ?
         SGE      4(SP)                      ;YES IF >= ZERO
         NEG.B    4(SP)                      ;CONVERT TO PASCAL BOOLEAN
         RTS



         .SEG 'SegPrint'
         .PROC Stretch2x
;---------------------------------------------------------------
;
;  PROCEDURE Stretch2x(srcBuf,dstBuf: QDPtr);
;
;  FOR HI-RES PRINTING.
;  TAKES AN 11 TALL BY 72 BYTES WIDE BITMAP, STRETCHES AND
;  FILTERS IT TO MAKE A 21 TALL BY 144 BYTES WIDE RESULT.
;
PARAMSIZE         .EQU     8                 ;TOTAL BYTES OF PARAMS
SRCBUF            .EQU     PARAMSIZE+8-4     ;LONG
DSTBUF            .EQU     SRCBUF-4          ;LONG

BUF2END           .EQU     -4                ;LONG
BUF2              .EQU     BUF2END-4         ;LONG
BUF1              .EQU     BUF2-4            ;LONG
VARSIZE           .EQU     BUF1              ;TOTAL LOCALS

SRCHEIGHT         .EQU     11                ;SRC IS 11 SCANLINES TALL
SRCROW            .EQU     72                ;SRC IS 72 BYTES WIDE
DSTROW            .EQU     144               ;DST IS 144 BYTES WIDE


         LINK     A6,#VARSIZE                 ;ALLOCATE STACK FRAME
         MOVEM.L  D3-D7/A2-A4,-(SP)           ;SAVE REGS
;
;  SET UP ASSORTED HANDY VARIABLES IN REGS
;
         MOVE.L   SRCBUF(A6),A0              ;POINT TO SRCBUF
         MOVE.L   DSTBUF(A6),A1              ;POINT TO DSTBUF
         MOVE     #DSTROW,D4                 ;GET DST ROWBYTES
         MOVE     D4,D5                      ;COPY IT
         ADD      D5,D5                      ;MAKE 2*DSTROW
;
;  ALLOCATE BUF1 AND BUF2 SCANLINE BUFFERS
;
         MOVE.L   SP,BUF2END(A6)             ;REMEMBER END OF BUF2
         SUB      D4,SP                      ;ALLOCATE BUF2
         MOVE.L   SP,BUF2(A6)                ;REMEMBER START OF BUF2
         SUB      D4,SP                      ;ALLOCATE BUF1
         MOVE.L   SP,BUF1(A6)                ;REMEMBER START OF BUF1

;
;  STRETCH LINES HORIZONTALLY FOR SRCHEIGHT ROWS
;
         MOVE     #SRCHEIGHT-1,D3            ;INIT DBRA ROWCOUNT
NEXTROW  CLR      D0                         ;GET READY FOR BYTES
         MOVE     #SRCROW,D2                 ;GET SRC ROW
         SUBQ     #1,D2                      ;INIT DBRA COUNT OF BYTES
NEXTBYTE MOVE.B   (A0),D0                    ;GET A BYTE FROM SRC
         LSR      #4,D0                      ;GET HI NIBBLE
         MOVE.B   TABLE(D0),(A1)+            ;STRETCH TO A BYTE
         MOVE.B   (A0)+,D0                   ;GET SAME BYTE OF SRC
         AND      #$F,D0                     ;GET LO NIBBLE
         MOVE.B   TABLE(D0),(A1)+            ;STRETCH TO A BYTE
         DBRA     D2,NEXTBYTE                ;LOOP FOR ALL BYTES IN ROW
         BRA.S    HTHIN                      ;CONTINUE

TABLE    .BYTE    $00,$03,$0C,$0F            ;DOUBLING TABLE
         .BYTE    $30,$33,$3C,$3F
         .BYTE    $C0,$C3,$CC,$CF
         .BYTE    $F0,$F3,$FC,$FF


;-----------------------------------------------
;
;  LOCAL ROUTINE TO SHIFT SCANLINE LEFT
;  ENTER WITH SRCSTART IN A2, PUTS RESULT IN BUF1, CLOBBERING D0,D1,A3
;  LEAVES WITH A2 UNCHANGED, A3 AT START OF BUF1
;
LSHIFT   ADD      D4,A2                      ;POINT TO END OF SRC
         MOVE.L   BUF2(A6),A3                ;POINT TO END OF BUF1
         MOVE     D6,D1                      ;INIT DBRA LONGCOUNT
         SUB      D0,D0                      ;CLEAR X-BIT
LSHIFT2  MOVE.L   -(A2),D0                   ;GET A LONG
         ROXL.L    #1,D0                     ;SHIFT LEFT WITH CARRY
         MOVE.L   D0,-(A3)                   ;STORE IN BUF1
         DBRA     D1,LSHIFT2                 ;LOOP ALL LONGS
         RTS                                 ;AND RETURN


;-----------------------------------------------
;
;  LOCAL ROUTINE TO SHIFT SCANLINE RIGHT
;  ENTER WITH SRCPTR IN A2, PUTS RESULT IN BUF2, CLOBBERING D0,D1,A3
;  LEAVES WITH A2 UNCHANGED, A3 AT END OF BUF2
;
RSHIFT   MOVE.L   BUF2(A6),A3                ;POINT TO START OF BUF1
         MOVE     D6,D1                      ;INIT DBRA LONGCOUNT
         SUB      D0,D0                      ;CLEAR X-BIT
RSHIFT2  MOVE.L   (A2)+,D0                   ;GET A LONG
         ROXR.L   #1,D0                      ;SHIFT RIGHT WITH CARRY
         MOVE.L   D0,(A3)+                   ;STORE IN BUF2
         DBRA     D1,RSHIFT2                 ;LOOP ALL LONGS
         SUB      D4,A2                      ;RESTORE SRCPTR
         RTS                                 ;AND RETURN

;
;  THIN EACH ROW HORIZONTALLY, ODD BITS ONE ONLY IF BOTH NEIGHBORS ARE ONE
;
HTHIN    MOVE     D4,D6                      ;COPY DSTROW
         LSR      #2,D6                      ;DIV BY 4 FOR LONGS
         SUB      #1,D6                      ;INIT DBRA COUNT, CLEAR X-BIT
         MOVE     D6,D2                      ;INIT DBRA LONG COUNT
NEXTLONG MOVE.L   -(A1),D0                   ;GET A LONG OF DST
         ADDX.L   D0,D0                      ;SHIFT LEFT WITH CARRY
         AND.L    D0,(A1)                    ;AND WITH ORIGINAL
         DBRA     D2,NEXTLONG                ;LOOP ALL LONGS

         ADD      D5,A1                      ;BUMP DSTPTR TWO ROWS DOWN
         DBRA     D3,NEXTROW                 ;LOOP FOR SRCHEIGHT ROWS

;
;  COMPUTE ODD SCANLINES BY ANDING EVEN SCANS AND KILLING ODD BITS
;
         MOVE     #SRCHEIGHT-2,D3            ;INIT DBRA ROWCOUNT
         MOVE.L   DSTBUF(A6),A0              ;POINT TO TOP SCANLINE
         LEA      0(A0,D4),A1                ;POINT TO THIS SCANLINE
VERTROW  LEA      0(A1,D4),A2                ;POINT TO SCANLINE BELOW
         MOVE     D6,D2                      ;INIT DBRA LONG COUNT
VERTLONG MOVE.L   (A0)+,D0                   ;GET FROM SCAN ABOVE
         AND.L    (A2)+,D0                   ;AND WITH SCAN BELOW
         AND.L    #$AAAAAAAA,D0              ;KILL ALL THE ODD BITS
         MOVE.L   D0,(A1)+                   ;STORE INTO SCAN BELOW
         DBRA     D2,VERTLONG                ;LOOP ALL LONGS IN ROW
         MOVE.L   A1,A0
         MOVE.L   A2,A1
         DBRA     D3,VERTROW                 ;LOOP SRCHEIGHT-1 ROWS

;
;  INTERPOLATE PIXELS ON LEFT AND RIGHT DIAGONALS
;  (first for left slant, then loop back for right)
;
         CLR      D7                         ;FIRST TIME THRU FLAG
AGAIN    MOVE     #SRCHEIGHT-2,D3            ;INIT DBRA ROW COUNT
         MOVE.L   DSTBUF(A6),A0              ;POINT TO TOP SCANLINE
SLANT    LEA      0(A0,D4),A1                ;POINT TO THIS SCANLINE
         LEA      0(A1,D4),A2                ;POINT TO SCANLINE BELOW
         TST      D7                         ;FIRST TIME THRU ?
         BEQ.S    OK1                        ;YES, CONTINUE
         EXG      A0,A2                      ;NO, EXCHANGE ABOVE AND BELOW
OK1      BSR      LSHIFT                     ;LEFT SHIFT SCAN BELOW INTO BUF1
         MOVE     D6,D2                      ;INIT DBRA LONG COUNT
         SUB      D0,D0                      ;CLEAR X-BIT
SLANT2   MOVE.L   (A0)+,D0                   ;GET FROM SCAN ABOVE
         ROXR.L   #1,D0                      ;SHIFT RIGHT WITH CARRY
         AND.L    (A3)+,D0                   ;AND WITH SCANBUF
         AND.L    #$55555555,D0              ;MASK FOR ODD BITS ONLY
         EOR.L    D0,(A1)+                   ;XOR RESULT INTO DST
         DBRA     D2,SLANT2                  ;LOOP ALL LONGS IN ROW
         MOVE.L   A1,A0                      ;POINT TO NEW TOP LINE
         DBRA     D3,SLANT                   ;LOOP SRCHEIGHT-1 ROWS
         NOT      D7                         ;WAS THIS THE FIRST TIME THRU ?
         BNE      AGAIN                      ;YES, GO AGAIN FOR RIGHT SLANT


;-------------------------------------------------------
;
;  THE REST OF THIS PROCEDURE CLEARS OUT PIXELS
;  THAT ARE ZERO SURROUNDED BY FOUR ONES
;
;  FOR THIS WE NEED TO LOOK AT 5 EXPANDED SCANLINES
;
;             ORIGINAL:           RESULT:
;
;  SCAN1      X   1   X           X   1   X
;  SCAN2                            0 0 0
;  SCAN3      1   0   1    -->    1 0 0 0 1
;  SCAN4                            0 0 0
;  SCAN5      X   1   X           X   1   X
;
         MOVE     #SRCHEIGHT-3,D3            ;INIT DBRA ROW COUNT
         MOVE.L   DSTBUF(A6),A0              ;POINT TO INITIAL SCAN1
;
; CALC LINE3 SHIFTED LEFT 2 INTO BUF1
;
CLEAR    LEA      0(A0,D5),A2                ;POINT TO SCAN3
         BSR      LSHIFT                     ;LEFT SHIFT SCAN3 INTO BUF1
         MOVE.L   BUF1(A6),A2                ;POINT TO BUF1
         BSR      LSHIFT                     ;SHIFT LEFT A SECOND TIME
;
; CALC LINE3 SHIFTED RIGHT 2 INTO BUF2
;
         LEA      0(A0,D5),A2                ;POINT TO SCAN3
         BSR.S    RSHIFT                     ;RIGHT SHIFT SCAN3 INTO BUF2
         MOVE.L   BUF2(A6),A2                ;POINT TO BUF2
         BSR.S    RSHIFT                     ;SHIFT LEFT A SECOND TIME
;
; CALC (LSHIFT AND RSHIFT) AND NOT ORIGINAL, PUT RESULT IN BUF1
;
         MOVE.L   BUF1(A6),A1                ;POINT TO LSHIFT
         MOVE.L   BUF2(A6),A2                ;POINT TO RSHIFT
         LEA      0(A0,D5),A3                ;POINT TO SCAN3
         MOVE     D6,D2                      ;INIT DBRA LONG COUNT
ANDLP    MOVE.L   (A3)+,D0                   ;GET ORIGINAL
         NOT.L    D0                         ;FORM NOT ORIGINAL
         AND.L    (A2)+,D0                   ;AND WITH BUF2
         AND.L    D0,(A1)+                   ;AND RESULT INTO BUF1
         DBRA     D2,ANDLP                   ;LOOP ALL LONGS IN ROW
;
;  AND SCAN1 AND SCAN5 INTO RESULT IN BUF1, THEN SMEAR RIGHT
;
         MOVE.L   BUF1(A6),A1                ;POINT TO BUF1
         MOVE.L   A0,A2                      ;POINT TO SCAN1
         LEA      0(A0,D5),A3
         ADD      D5,A3                      ;POINT TO SCAN5
         MOVE     D6,D2                      ;INIT DBRA LONG COUNT
         SUB      D0,D0                      ;CLEAR X-BIT
RSMEAR   MOVE.L   (A1),D0                    ;GET FROM BUF1
         AND.L    (A2)+,D0                   ;AND WITH SCAN1
         AND.L    (A3)+,D0                   ;AND WITH SCAN5
         MOVE.L   D0,D1                      ;COPY RESULT
         ROXR.L   #1,D1                      ;SHIFT COPY RIGHT WITH CARRY
         OR.L     D1,D0                      ;SMEAR RIGHT
         MOVE.L   D0,(A1)+                   ;STORE RESULT IN BUF1
         DBRA     D2,RSMEAR                  ;LOOP ALL LONGS IN ROW

;
;  SMEAR BUF1 LEFT AND USE IT TO TRIM SCAN2 AND SCAN 4
;
         LEA      0(A0,D5),A2                ;POINT TO END OF SCAN2
         LEA      0(A2,D5),A3                ;POINT TO END OF SCAN4
         MOVE     D6,D2                      ;INIT DBRA LONG COUNT
         SUB      D0,D0                      ;CLEAR X-BIT
LSMEAR   MOVE.L   -(A1),D0                   ;GET FROM BUF1
         MOVE.L   D0,D1                      ;COPY RESULT
         ROXL.L   #1,D1                      ;SHIFT COPY LEFT WITH CARRY
         OR.L     D1,D0                      ;SMEAR LEFT
         NOT.L    D0                         ;FORM NOTMASK FOR BIC
         AND.L    D0,-(A2)                   ;TRIM SCAN2
         AND.L    D0,-(A3)                   ;TRIM SCAN4
         DBRA     D2,LSMEAR                  ;LOOP ALL LONGS IN ROW

;
;  LOOP FOR ALL ROWS
;
         ADD      D5,A0                      ;POINT TO NEW TOP LINE
         DBRA     D3,CLEAR                   ;LOOP SRCHEIGHT-2 ROWS


         ADD      D5,SP                      ;RELEASE BUF1 AND BUF2
         MOVEM.L  (SP)+,D3-D7/A2-A4          ;RESTORE REGS
         UNLK     A6                         ;RELEASE STACK FRAME
         MOVE.L   (SP)+,A0                   ;POP RETURN ADDR
         ADD      #PARAMSIZE,SP              ;STRIP PARAMS
         JMP      (A0)                       ;AND RETURN




         .SEG '       '
         .PROC MySetItemStyle
         .REF  SetItemStyle
;---------------------------------------------------------------------
;
;  PROCEDURE MySetItemStyle(menu: menuHandle; item: INTEGER;
;                           txFace: INTEGER);
;
;  *** just type-coerce txFace to INTEGER to avoid %_ADJ SET OPERATOR  ***
;
         JMP      SetItemStyle



         .SEG '       '
         .PROC MyGetItem
         .REF  GetItem
;---------------------------------------------------------------------
;
;  PROCEDURE MyGetItem(menu: menuHandle; item: INTEGER; VAR itemString: Str63);
;
;  *** just type-coerce VAR Str255 to Str63 ***
;
         JMP      GetItem



           .SEG '        '
         .FUNC Trunc8
;---------------------------------------------------------------------
;
;  FUNCTION  Trunc8(i: INTEGER): INTEGER;
;  truncate toward zero to the next multiple of eight.
;
         MOVE.L   (SP)+,A0                   ;POP RETURN ADDR
         MOVE.W   (SP)+,D0                   ;POP INPUT PARAM
         BMI.S    NEGVAL                     ;BRANCH IF NEGATIVE
         AND      #$FFF8,D0                  ;MASK TO MULT OF 8
         BRA.S    DONE                       ;AND CONTINUE
NEGVAL   NEG      D0                         ;TAKE ABS VALUE
         AND      #$FFF8,D0                  ;MASK TO MULT OF 8
         NEG      D0                         ;RESTORE TO NEG
DONE     MOVE     D0,(SP)                    ;STORE RESULT
         JMP      (A0)                       ;AND RETURN


         .SEG '        '
         .FUNC GetDoubleTime
;---------------------------------------------------------------------
;
;  FUNCTION  GetDoubleTime: LONGINT;
;
         MOVE.L   doubleTime,4(SP)                    ;just examine low mem
         RTS                                          ;and return


         .SEG '        '
         .FUNC GetCaretTime
;---------------------------------------------------------------------
;
;  FUNCTION  GetCaretTime: LONGINT;
;
         MOVE.L   caretTime,4(SP)                     ;just examine low mem
         RTS                                          ;and return


         .SEG '        '
         .FUNC DiskSpace
;---------------------------------------------------------------------
;
;  FUNCTION DiskSpace(drive: INTEGER): LongInt;
;
;  returns total bytes free on the specified drive
;
PARAMSIZE         .EQU     2
RESULT            .EQU     PARAMSIZE+8
DRIVE             .EQU     RESULT-2          ;WORD

         LINK     A6,#-IOVQElSize            ;allocate an IOBlock
         MOVE.L   SP,A0                      ;point to IOBlock
         CLR.L    IOVNPtr(A0)                ;Nil name pointer
         CLR.W    IOVolIndex(A0)             ;not by index
         MOVE.W   DRIVE(A6),IOVDrvNum(A0)    ;specified drive
         _GetVolInfo                         ;call OS
         MOVE.W   IOVFrBlk(A0),D0            ;get count of free blocks
         MOVE.L   IOVAlBlkSize(A0),D1        ;get bytes per block
         MULU     D1,D0                      ;calc total bytes free
         MOVE.L   D0,RESULT(A6)              ;store into function result
         UNLK     A6                         ;release stack frame
         MOVE.L   (SP)+,A0                   ;pop return addr
         ADD      #PARAMSIZE,SP              ;strip param
         JMP      (A0)                       ;and return



         .SEG 'SegSym  '
         .FUNC MapSym
;---------------------------------------------------------------------
;
;  FUNCTION  MapSym(hSym,vSym,hvSym,vhSym: BOOLEAN): QDByte;
;
;  maps 4 symmetry flags into an 8 bit code telling
;  which of 8 possible positions to plot.
;
         MOVE.L   (SP)+,A1                   ;pop return addr
         CLR      D0
         ADD.B    (SP)+,D0                   ;add boolean to sum
         ADD      D0,D0                      ;shift left
         ADD.B    (SP)+,D0                   ;repeat all 4 booleans
         ADD      D0,D0
         ADD.B    (SP)+,D0
         ADD      D0,D0
         ADD.B    (SP)+,D0
         MOVE.B   TABLE(D0),(SP)             ;look up result
         JMP      (A1)                       ;and return

TABLE    .BYTE    $80,$C0,$A0,$F0
         .BYTE    $88,$FF,$FF,$FF
         .BYTE    $81,$FF,$FF,$FF
         .BYTE    $99,$FF,$FF,$FF



         .SEG '        '
         .FUNC StackPtr
;---------------------------------------------------------------------
;
;  FUNCTION StackPtr: LongInt;
;
         MOVE.L   SP,4(SP)
         RTS



         .SEG '        '
         .PROC FlushVol
;---------------------------------------------------------------------
;
;  PROCEDURE FlushVol(driveNum: INTEGER);
;
         MOVE.L   (SP)+,A1                   ;pop return addr
         MOVE     (SP)+,D0                   ;pop driveNum
         LINK     A6,#-IOQElSize             ;allocate a command block
         MOVE.L   SP,A0                      ;point to the cmd block
         CLR.L    IOFileName(A0)             ;ptr to vol name = Nil
         MOVE     D0,IODrvNum(A0)            ;install driveNum
         _FlushVol                           ;flush file buffers
         UNLK     A6                         ;release command block
         JMP      (A1)                       ;and return



         .SEG '        '
         .PROC FlushEvents
;---------------------------------------------------------------------
;
;  PROCEDURE FlushEvents(whichMask,stopMask: INTEGER);
;
         MOVE.L   (SP)+,A0                   ;pop return addr
         MOVE.L   (SP)+,D0                   ;get whichMask and stopMask
         MOVE.L   A0,-(SP)                   ;push return addr
         _FlushEvents                        ;flush event queue
         RTS                                 ;and return



         .SEG '        '
         .FUNC EqualString
;---------------------------------------------------------------------
;
;  FUNCTION EqualString(str1,str2: Str255): BOOLEAN;
;
         MOVE.L   (SP)+,D0                   ;POP RETURN ADDR
         MOVE.L   (SP)+,A1                   ;POP ADDR OF STR2
         MOVE.L   (SP)+,A0                   ;POP ADDR OF STR1
         CLR      D1                         ;GET READY FOR BYTE
         MOVE.B   (A0),D1                    ;GET LENGTH BYTE 0..255
LOOP     CMPM.B   (A0)+,(A1)+                ;IS THIS BYTE THE SAME ?
         DBNE     D1,LOOP                    ;LOOP TILL LENGTH+1 OR <>
         SEQ      (SP)                       ;RETURN RESULT
         NEG.B    (SP)                       ;CONVERT $FF TO $01
         MOVE.L   D0,A0                      ;GET RETURN ADDR
         JMP      (A0)                       ;AND RETURN



         .SEG '        '
         .PROC ExpandPat
;--------------------------------------------------------------------------
;
;  PROCEDURE ExpandPat(pat: Pattern; VAR exPat: Longs24; hShift: INTEGER);
;
;  pre-rotate, invert, and expand pattern into 24 longs
;
         MOVE.L   (SP)+,D0                   ;pop return addr
         MOVE     (SP)+,D2                   ;pop shift count
         MOVE.L   (SP)+,A1                   ;pop addr of exPat
         MOVE.L   (SP)+,A0                   ;pop addr of pat
         MOVE.L   D0,-(SP)                   ;push return addr
         MOVEQ    #7,D1                      ;init count of 8 bytes
         AND      D1,D2                      ;treat shiftcount mod 8
exLoop   MOVE.B   (A0)+,D0                   ;get a byte of pattern
         NOT.B    D0                         ;make inverse of pattern
         ROL.B    D2,D0                      ;align to local coords
         MOVE.B   D0,(A1)+                   ;put one byte
         MOVE.B   D0,(A1)+                   ;put another to make a word
         MOVE.W   -2(A1),(A1)+               ;stretch word out to long
         MOVE.L   -4(A1),32-4(A1)            ;duplicate it 8 longs later
         MOVE.L   -4(A1),64-4(A1)            ;duplicate it 16 longs later
         DBRA     D1,exLoop                  ;loop all 8 bytes
         RTS                                 ;and return



         .SEG '        '
         .PROC DrawBrush
;------------------------------------------------------------
;
;  PROCEDURE DrawBrush(brush: Bits16; exPat: Longs24;
;                      hCenter,vCenter: INTEGER;
;                      dstBits: BitMap; clip: Rect; orMode: BOOLEAN);
;
;  Draw 16 by 16 image centered at h,v
;
paramSize         .EQU     22                ;total bytes of parameters
brush             .EQU     paramSize+8-4     ;long, addr of Bits16
exPat             .EQU     brush-4           ;long, addr of pattern
horiz             .EQU     exPat-2           ;word
vert              .EQU     horiz-2           ;word
dstBits           .EQU     vert-4            ;long, addr of bitMap
clipRect          .EQU     dstBits-4         ;long, addr of rect
orMode            .EQU     clipRect-2        ;byte, BOOLEAN


         LINK     A6,#0                      ;allocate stack frame
         MOVEM.L  D3-D6/A2-A4,-(SP)          ;save registers
         SUB      #8,horiz(A6)               ;convert hCenter to left
         SUB      #8,vert(A6)                ;convert vCenter to top
         MOVE.L   dstBits(A6),A3             ;point to dstBits
         MOVE.L   clipRect(A6),A0            ;point to clipRect
         MOVE     rowBytes(A3),D6            ;get dstBits.rowBytes
;
;  check for clipping at left
;
         MOVEQ    #-1,D5                     ;init mask = all ones
         MOVE     horiz(A6),D0               ;get brush left
         CMP      left(A0),D0                ;is brush left < clip Left ?
         BGE.S    LEFTOK                     ;no, continue
         MOVE.L   #$0000FFFF,D5              ;yes, new mask
         ADD      #16,D0                     ;calc brush right
         CMP      left(A0),D0                ;is brush right < clip left ?
         BLE      GOHOME                     ;yes, ignore whole thing
LEFTOK

;
;  check for clipping at right
;
         MOVE     horiz(A6),D0               ;get brush left
         CMP      right(A0),D0               ;is brush left > clip right ?
         BGE      GOHOME                     ;yes, ignore whole thing
         ADD      #16,D0                     ;calc brush right
         CMP      right(A0),D0               ;is brush right > clip right ?
         BLE.S    RIGHTOK                    ;no, continue
         CLR.W    D5                         ;yes, mask = $FFFF0000
         CMP      #8,D6                      ;are we drawing into fatBits ?
         BNE.S    RIGHTOK                    ;no, continue
         MOVE.W   #$F000,D5                  ;yes, mask = $FFFFF000
RIGHTOK

;
;  check for clipping at top
;
         MOVE.L   brush(A6),A4               ;point to brush data
         MOVE     #16,D3                     ;init height = 16
         MOVE     top(A0),D0                 ;get clip top
         SUB      vert(A6),D0                ;skip := clipTop - brush top
         BLE.S    TOPOK                      ;continue if skip <= 0
         ADD      D0,vert(A6)                ;else trim brush top
         SUB      D0,D3                      ;trim height to match
         ADD      D0,A4                      ;adjust brush pointer too
         ADD      D0,A4                      ;twice for brush rowbytes = 2
TOPOK
;
;  check for clipping at bottom
;
         MOVE     vert(A6),D0                ;get brush top
         ADD      D3,D0                      ;add height for brush bottom
         SUB      bottom(A0),D0              ;skip := brushBot - clipBot
         BLE.S    BOTOK                      ;continue if skip <= 0
         SUB      D0,D3                      ;else trim height
BOTOK
;
;  set up dstPtr
;
         MOVE.L   baseAddr(A3),A2            ;dstPtr := dstBits.baseAddr
         MOVE     horiz(A6),D0               ;get horiz coord
         SUB      bounds+left(A3),D0         ;convert to global coords
         ASR      #4,D0                      ;div by 16 for word
         ADD      D0,D0                      ;double for bytes
         ADD      D0,A2                      ;add horiz offset to dstPtr
         MOVE     vert(A6),D0                ;get vert coord
         SUB      bounds+top(A3),D0          ;convert to global coords
         MULU     D6,D0                      ;mult times dst rowBytes
         ADD.L    D0,A2                      ;add vert offset to dstPtr
;
; set up shiftcount (and adjust dstPtr)
;
         MOVE     bounds+left(A3),D2         ;get dstBits.bounds.left
         SUB      horiz(A6),D2               ;calc -1*(global coords)
         AND      #$F,D2                     ;treat mod 16 for shift
         BNE.S    notZero                    ;continue if not zero
         SUB      #2,A2                      ;else back up dstPtr

notZero  MOVE.L   exPat(A6),A1               ;point to expanded pattern
         MOVE     vert(A6),D0                ;get dst vert
         AND      #7,D0                      ;treat mod 8
         LSL      #2,D0                      ;quad for longs
         ADD      D0,A1                      ;add vert offset to pat ptr

         SUB      #1,D3                      ;dbra count = height - 1
         BLT.S    GOHOME                     ;quit if count neg
         TST.B    ORMODE(A6)                 ;are we in OR-mode ?
         BEQ.S    NXTROW                     ;no, do normal loop
;
;  loop all 16 rows, using mask to deposit pattern in or-mode
;
orRow    CLR.L    D0                         ;get ready for word
         MOVE     (A4)+,D0                   ;get word of brush
         LSL.L    D2,D0                      ;align mask to dst
         AND.L    D5,D0                      ;clip mask
         MOVE.L   (A1)+,D1                   ;get long of inverse pattern
         NOT.L    D1                         ;invert back to normal
         AND.L    D1,D0                      ;and with pattern data
         OR.L     D0,(A2)                    ;or result into dst
         ADD      D6,A2                      ;bump to next row down
         DBRA     D3,orRow                   ;loop 16 rows
         BRA.S    GOHOME                     ;and quit
;
;  loop all 16 rows, using mask to deposit pattern
;
nxtRow   MOVE.L   (A1)+,D4                   ;get a long of pattern data
         MOVE.L   (A2),D0                    ;read data from dst
         EOR.L    D4,D0                      ;xor with inverse pattern data
         CLR.L    D1                         ;get ready for word
         MOVE     (A4)+,D1                   ;get word of brush
         LSL.L    D2,D1                      ;align mask to dst
         AND.L    D5,D1                      ;clip mask
         OR.L     D1,D0                      ;use mask to punch hole
         EOR.L    D4,D0                      ;xor with inverse pattern data
         MOVE.L   D0,(A2)                    ;write result to dst
         ADD      D6,A2                      ;bump to next row down
         DBRA     D3,nxtRow                  ;loop 16 rows

GOHOME   MOVEM.L  (SP)+,D3-D6/A2-A4          ;restore registers
         UNLK     A6                         ;release stack frame
         MOVE.L   (SP)+,A0                   ;pop return addr
         ADD      #paramSize,SP              ;strip parameters
         JMP      (A0)                       ;and return


         .SEG '        '
         .FUNC NearPt
;-----------------------------------------------------------
;
;  FUNCTION NearPt(pt1,pt2: Point; tol: INTEGER): BOOLEAN;
;
;  NearPt:=((ABS(pt1.h-pt2.h) < tol) AND (ABS(pt1.v-pt2.v) < tol));
;
         MOVE.L   (SP)+,A0                   ;pop return addr
         MOVE     (SP)+,D0                   ;pop tolerance
         MOVE.L   (SP)+,D1                   ;pop pt2
         MOVE.L   (SP)+,D2                   ;pop pt1
         CLR.B    (SP)                       ;assume result FALSE
         SUB.W    D1,D2                      ;calc delta horiz
         BGE.S    DHPOS                      ;continue if dh positive
         NEG.W    D2                         ;else negate for abs value
DHPOS    CMP.W    D0,D2                      ;is ABS(dh) < tol ?
         BGE.S    FALSE                      ;no, return false
         SWAP     D1                         ;get pt2.v
         SWAP     D2                         ;get pt1.v
         SUB.W    D1,D2                      ;calc delta vert
         BGE.S    DVPOS                      ;continue if dv positive
         NEG.W    D2                         ;else negate for abs value
DVPOS    CMP.W    D0,D2                      ;is ABS(dv) < tol ?
         BGE.S    FALSE                      ;no, return FALSE
         MOVE.B   #1,(SP)                    ;result := TRUE
FALSE    JMP      (A0)                       ;and return



         .SEG '        '
         .FUNC FinderDoc
;----------------------------------------------------------------
;
;  FUNCTION FinderDoc(VAR drive,version: INTEGER; strPtr: QDPtr): INTEGER;
;
paramSize         .EQU     12
result            .EQU     paramSize+8       ;word
drive             .EQU     result-4          ;long, pointer to integer
version           .EQU     drive-4           ;long, pointer to integer
fileName          .EQU     version-4         ;long, pointer to string

         LINK     A6,#0                      ;no locals
         MOVE.L   AppParmHandle,A0           ;get AppParmHandle
         MOVE.L   (A0),A0                    ;de-reference it
         MOVE.W   (A0)+,result(A6)           ;get finder message
         MOVE.W   (A0)+,D1                   ;get docCount
         MOVE.L   DRIVE(A6),A1               ;point to drive
         BRA.S    TRYDOC                     ;go to loop start

NXTDOC   MOVE.W   (A0)+,(A1)                 ;get drive number
         CMP.L    #'PNTG',(A0)+              ;is this a 'PNTG' document ?
         BEQ.S    FOUND                      ;yes, we found one.
         ADD      #2,A0                      ;no, skip version
         MOVE.B   (A0),D0                    ;get string length
         ADD      #2,D0                      ;round (length+1) up
         AND      #$00FE,D0                  ;to next mult of 2
         ADD      D0,A0                      ;skip past the string
TRYDOC   DBRA     D1,NXTDOC                  ;loop for all documents

         MOVE     #-1,RESULT(A6)             ;result = failure
         CLR      (A1)                       ;return drive = 0
         MOVE.L   version(A6),A1             ;point to version
         CLR      (A1)                       ;return version = 0
         MOVE.L   fileName(A6),A1            ;point to string
         CLR.B    (A1)                       ;return empty string
         BRA.S    GOHOME

FOUND    CLR.L    -4(A0)                     ;clear docType for next time
         MOVE     (A0)+,D0                   ;get version (in hi byte)
         LSR      #8,D0                      ;put in lo byte
         MOVE.L   version(A6),A1             ;point to version
         MOVE     D0,(A1)                    ;return version
         MOVE.B   (A0),D0                    ;get str len (hi byte 0 from above)
         MOVE.L   fileName(A6),A1            ;point to result string
NXTBYTE  MOVE.B   (A0)+,(A1)+                ;copy a byte
         DBRA     D0,NXTBYTE                 ;copy (length+1) bytes

GOHOME   UNLK     A6                         ;release stack frame
         MOVE.L   (SP)+,A0                   ;pop return addr
         ADD      #paramsize,SP              ;strip params
         JMP      (A0)                       ;and return



         .SEG 'SegPrint'
         .FUNC CountFiles
;----------------------------------------------------------------
;
;  FUNCTION CountFiles( drive: INTEGER; VAR str: STR255 ): INTEGER;
;
;  Returns total number of files on given drive.  (0 if no drive)
;
;  A6 offsets of params and locals after link:
;
paramSize         .EQU     6
Result            .EQU     paramSize+8        ; return number of files
DriveNum          .EQU     Result-2           ; disk drive number
DiskName          .EQU     DriveNum-4         ; address to store name of disk

ioBlk             .EQU     -IOFQElSize
locals            .EQU     ioBlk

         LINK     A6,#locals                  ; set up frame, etc.
         LEA      ioBlk(A6),A0                ; point to IO buffer
         MOVE.L   DiskName(A6),IOfileName(A0) ; Set the volume name ptr
         CLR.W    IOVolIndex(A0)              ; Use drive number(not index mode)
         MOVE.W   DriveNum(A6),IODrvNum(A0)   ; use the given volume
         _GetVolInfo                          ; call OS for vol info
         BEQ.S    noError                     ; did volume exist ?
         CLR      IOVNmFls(A0)                ; no, set number of files to zero
noError  MOVE.W   IOVNmFls(A0),Result(A6)     ; return number of files
         UNLK     A6
         MOVE.L   (SP)+,A0
         ADD      #paramSize,SP
         JMP      (A0)



         .SEG 'SegPrint'
         .FUNC NextFile
;--------------------------------------------------------------
;
; FUNCTION NextFile(drive,index: INTEGER; fileType: ResType;
;                   VAR str: STR255; VAR version: INTEGER): BOOLEAN;
;
; Returns file name(index) and boolean if matches given type.
;
; A6 offsets of params and locals after link:
;
paramSize         .EQU     16
Result            .EQU     paramSize+8        ; word, number of files
DriveNum          .EQU     Result-2           ; word, drive number
Index             .EQU     DriveNum-2         ; word, index of next file
FileType          .EQU     Index-4            ; long, restype of file needed
FileName          .EQU     FileType-4         ; long, addr of fileName string
version           .EQU     fileName-4         ; long, addr of integer

ioBlk             .EQU     -IOFQElSize
locals            .EQU     ioBlk

         LINK     A6,#locals                  ; allocate stack frame
         LEA      ioBlk(A6),A0                ; point to IO buffer
         MOVE.L   FileName(A6),IOfileName(A0) ; Set the file name ptr
         MOVE.W   DriveNum(A6),IODrvNum(A0)   ; use the given volume
         MOVE.W   Index(A6), IOFDirIndex(A0)  ; And use the given index.
         _GetFileInfo                         ; get info about this file
         CLR      D0                          ; get ready for byte
         MOVE.B   IOFFlType(A0),D0            ; get output version byte
         MOVE.L   VERSION(A6),A1              ; point to VAR version
         MOVE.W   D0,(A1)                     ; store into version
         MOVE.L   FileType(A6),D0             ; get passed file type
         CMP.L    IOFlUsrWds(A0),D0           ; see if type matches
         SEQ      Result(A6)                  ; return true if match
         NEG.B    Result(A6)                  ; convert to Pascal boolean
         UNLK     A6                          ; release stack frame
         MOVE.L   (SP)+,A0                    ; pop return addr
         ADD      #paramSize,SP               ; strip params
         JMP      (A0)                        ; and return


         .SEG '        '
         .PROC SetDocType
;--------------------------------------------------------------------------------
;
;  PROCEDURE SetDocType(drive,version: INTEGER; docName: Str255);
;
paramSize         .EQU     8
drive             .EQU     paramSize+8-2     ;word
version           .EQU     drive-2           ;word, lo byte used
fName             .EQU     version-4         ;long, pointer to string

         LINK     A6,#-IOFQElSize            ;allocate command block
         MOVE.L   SP,A0                      ;Point to command block
         MOVE.L   fName(A6),IOFileName(A0)   ;install file name pointer
         MOVE.B   version+1(A6),IOFileType(A0) ;install version byte
         MOVE     drive(A6),IODrvNum(A0)     ;use the specified drive.
         CLR.W    IOFDirIndex(A0)            ;And use the given file name.
         _GetFileInfo                        ;Call OS GetInfo
         MOVE.L   #'PNTG',IOFlUsrWds(A0)     ;install file type
         MOVE.L   #'MPNT',IOFlUsrWds+4(A0)   ;install creator
         _SetFileInfo                        ;Call OS SetInfo
         UNLK     A6
         MOVE.L   (SP)+, A0                  ;Get return address
         ADD      #paramSize,SP              ;Pop parameters
         JMP      (A0)                       ;and return.


         .SEG '        '
         .FUNC CheckPntg
;--------------------------------------------------------------------------------
;
;  FUNCTION CheckPntg(drive,version: INTEGER; docName: Str255): BOOLEAN;
;  returns true if given file is of type 'PNTG'
;
paramSize         .EQU     8
result            .EQU     paramsize+8       ;boolean
drive             .EQU     result-2          ;word
version           .EQU     drive-2           ;word, lo byte used
fName             .EQU     version-4         ;long, pointer to string

         LINK     A6,#-IOFQElSize            ;allocate command block
         MOVE.L   SP,A0                      ;Point to command block
         MOVE.L   fName(A6),IOFileName(A0)   ;install file name pointer
         MOVE.B   version+1(A6),IOFileType(A0) ;install version byte
         MOVE     drive(A6),IODrvNum(A0)     ;use the specified drive.
         CLR.W    IOFDirIndex(A0)            ;And use the given file name.
         _GetFileInfo                        ;Call OS GetInfo
         CMP.L    #'PNTG',IOFlUsrWds(A0)     ;is type PNTG ?
         SEQ      result(A6)                 ;update result
         NEG.B    result(A6)                 ;adjust for Pascal boolean
         UNLK     A6
         MOVE.L   (SP)+, A0                  ;Get return address
         ADD      #paramSize,SP              ;Pop parameters
         JMP      (A0)                       ;and return.


         .SEG '        '
         .PROC FileInfo
;--------------------------------------------------------------
;
;  PROCEDURE FileInfo(drive,version: INTEGER; fName: Str63;
;                     VAR fileDate,fileSize: LongInt);
;
paramSize         .EQU     16
drive             .EQU     paramsize+8-2     ;word
version           .EQU     drive-2           ;word, lo byte used
fName             .EQU     version-4         ;long, pointer to string
fDate             .EQU     fName-4           ;long, pointer to long
fSize             .EQU     fDate-4           ;long, pointer to long

         LINK     A6,#-IOFQElSize            ;allocate command block
         MOVE.L   SP,A0                      ;Point to command block
         MOVE.L   fName(A6),IOFileName(A0)   ;install file name pointer
         MOVE.B   version+1(A6),IOFileType(A0) ;install version byte
         MOVE     drive(A6),IODrvNum(A0)     ;use the specified drive.
         CLR.W    IOFDirIndex(A0)            ;And use the given file name.
         _GetFileInfo                        ;Call OS GetInfo
         MOVE.L   fDate(A6),A1               ;get var addr
         MOVE.L   IOFlMdDat(A0),(A1)         ;get modification data
         MOVE.L   fSize(A6),A1               ;get var addr
         MOVE.L   IOFlPyLen(A0),(A1)         ;get physical length
         UNLK     A6                         ;release stack frame
         MOVE.L   (SP)+,A0                   ;pop return addr
         ADD      #paramSize,SP              ;strip params
         JMP      (A0)                       ;and return



         .SEG 'SegPrint'
         .PROC HToneRow
;----------------------------------------------------
;
; PROCEDURE HToneRow(thisRow,nextRow:        QDPtr;
;                    dstBits:                BitMap;
;                    hStart,vStart,hSize:    INTEGER;
;                    whiteLevel,blackLevel:  INTEGER;
;                    threshold:              BOOLEAN);
;
;
;  A6 OFFSETS OF PARAMETERS AND VARIABLES AFTER LINK:
;
paramSize         .EQU     24
thisRow           .EQU     paramSize+8-4     ;Pointer
nextRow           .EQU     thisRow-4         ;Pointer
dstBits           .EQU     nextRow-4         ;Pointer
hStart            .EQU     dstBits-2         ;Integer
vStart            .EQU     hStart-2          ;Integer
hSize             .EQU     vStart-2          ;Integer
whiteLevel        .EQU     hSize-2           ;Integer
blackLevel        .EQU     whiteLevel-2      ;Integer
threshold         .EQU     blackLevel-2      ;Boolean

fullScale         .EQU     -2                ;blackLevel-whiteLevel
halfScale         .EQU     fullScale-2       ;fullScale/2
varSize           .EQU     halfScale


         LINK     A6,#varSize                ;ALLOCATE STACK FRAME
         MOVEM.L  D3-D7/A2-A4,-(SP)          ;SAVE REGS
         MOVE.L   dstBits(A6),A3             ;get dst bitmap
         MOVE     hSize(A6),D1               ;get horizontal size
         BLE      GOHOME                     ;quit if no pixels
         MOVE     hStart(A6),D0              ;get horiz start coord
         SUB      bounds+left(A3),D0         ;make it global
         ASR      #4,D0                      ;div by 16 for word
         ADD      D0,D0                      ;double for byte offset
         MOVE.L   baseAddr(A3),A0            ;get baseAddr
         ADD      D0,A0                      ;add offset for hStart

         MOVE     vStart(A6),D0              ;get vert start
         SUB      bounds+top(A3),D0          ;make it global
         MULU     rowBytes(A3),D0            ;times rowBytes for offset
         ADD.L    D0,A0                      ;add vertical offset

         MOVE.L   thisRow(A6),A1             ;point to thisRow
         MOVE.L   nextRow(A6),A2             ;point to nextRow
         MOVE     blackLevel(A6),D4          ;get blackLevel
         MOVE     whiteLevel(A6),D5          ;get whiteLevel
         MOVE     D4,D7
         SUB      D5,D7                      ;fullScale := blackLevel - whiteLevel
         MOVE     D7,fullScale(A6)
         MOVE     D7,D6
         ASR      #1,D6                      ;halfScale
         MOVE     D6,halfScale(A6)
         MOVEQ    #-1,D3                     ;D3 := all 1's
;
; Now, to set up D0 as the a-building data word, we have to put the
; leading 1 on it.  That is, we put a one in the right place so as
; to cause the one to come flying out the end when we want to write
; this word.

         CLR.L    D0
         MOVE     hStart(A6),D2              ;get horiz start coord
         SUB      bounds+left(A3),D2         ;make it global
         AND      #$000F,D2                  ;use bottom 4 bits
         BSET     D2,D0                      ;to set a bit in dst accumulator
         BRA      MORE                       ;go to main loop start

NXTPIXEL CLR      D2                         ;get ready for byte
         MOVE.B   (A1)+,D2                   ;D2 := This pixel value
         SUB      D5,D2                      ;D2 := pixel - whiteLevel ( = Error)
         CMP      halfScale(A6),D2           ;is error >= halfScale ?
         BLT.S    ItsWhite                   ;no, continue
ItsBlack SUB      fullScale(A6),D2           ;yes, error := error - fullScale
         ASR      #1,D3                      ;Set the X flag
         BRA.S    Addit                      ;and output a black pixel
ItsWhite SUB      #0,D2                      ;Clear the X flag for white pixel
Addit    ADDX     D0,D0                      ;shift x-bit in, flag bit out
         BCC.S    NoWrite                    ;br if not time to write
         OR       D0,(A0)+                   ;or a word into dst
         MOVE     #1,D0                      ;and reset flag bit

NoWrite  TST.B    threshold(A6)              ;If we're thresholding...
         BNE      MORE                       ;...then don't propagate error
         ASR      #2,D2                      ;calc error div 4
         CLR      D7                         ;get ready for byte
         MOVE.B   (A1),D7                    ;get next pixel in this row
         ADD      D2,D7                      ;add error div 4 to next pixel

;
; Clip to interval [whiteLevel...blackLevel]
;
         CMP      D5,D7                      ;Is result < whiteLevel
         BGE.S    @10                        ;No-0K
         MOVE     D5,D7                      ;Yes- Set to whiteLevel
         BRA.S    @12                        ;...and go store it
@10      CMP      D4,D7                      ;Is result > blackLevel
         BLE.S    @12                        ;No- Go store it
         MOVE     D4,D7                      ;Yes- Set to blackLevel
@12      MOVE.B   D7,(A1)                    ;store clipped result

;
; Now propagate error down
;
         CLR      D7                         ;get ready for byte
         MOVE.B   (A2),D7                    ;get pixel value below
         ADD      D2,D7                      ;add error div 4 to below
;
; Clip to interval [whiteLevel...blackLevel]
;
         CMP      D5,D7                      ;Is result < whiteLevel
         BGE.S    @20                        ;No-0K
         MOVE     D5,D7                      ;Yes- Set to whiteLevel
         BRA.S    @22                        ;...and go store it
@20      CMP      D4,D7                      ;Is result > blackLevel
         BLE.S    @22                        ;No- Go store it
         MOVE     D4,D7                      ;Yes- Set to blackLevel
@22      MOVE.B   D7,(A2)+                   ;store clipped result and bump
;
; Now propagate error down and over
;
         ASR      #1,D2                      ;calc error div 8
         CLR      D7                         ;get ready for byte
         MOVE.B   (A2),D7                    ;get pixel down and over
         ADD      D2,D7                      ;add error div 8 to pixel

;
; Clip to interval [whiteLevel...blackLevel]
;
         CMP      D5,D7                      ;Is result < whiteLevel
         BGE.S    @30                        ;No-0K
         MOVE     D5,D7                      ;Yes- Set to whiteLevel
         BRA.S    @32                        ;...and go store it
@30      CMP      D4,D7                      ;Is result > blackLevel
         BLE.S    @32                        ;No- Go store it
         MOVE     D4,D7                      ;Yes- Set to blackLevel
@32      MOVE.B   D7,(A2)                    ;store clipped result


MORE     DBRA     D1,NXTPIXEL                ;loop for all pixels

CLEANUP  ADD      D0,D0                      ;shift left
         BCC      CLEANUP                    ;till flag bit pops out
         OR       D0,(A0)                    ;then or the word into dst

GOHOME   MOVEM.L  (SP)+,D3-D7/A2-A4          ;RESTORE REGS
         UNLK     A6                         ;RELEASE STACK FRAME
         MOVE.L   (SP)+,A0                   ;POP RETURN ADDR
         ADD      #paramSize,SP              ;STRIP PARAMS
         JMP      (A0)                       ;AND RETURN



        .SEG 'SegPrint'
        .PROC SampleBits
;---------------------------------------------------------------
;
;  PROCEDURE SampleBits(src,dst: QDPtr);
;
;  counts the number of black bits in each 8 by 8 chunk of a bitmap
;  input is a 576 by 8 bit image, output is 72 bytes of 0..64
;
ROW     .EQU    72

        MOVE.L  (SP)+,D0                ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                ;POP DST ADDR
        MOVE.L  (SP)+,A0                ;POP SRC ADDR
        MOVE.L  D0,-(SP)                ;PUSH RETURN ADDR
        MOVEM.L A2,-(SP)                ;SAVE REGS
        LEA     COUNTS,A2               ;POINT TO BIT COUNT TABLE
        CLR     D0                      ;GET READY FOR BYTES
        MOVEQ   #71,D2                  ;INIT COUNT OF 72 SRC BYTES
NXTSRC  CLR.B   D1                      ;INIT COUNT = 0
        MOVE.B  0*ROW(A0),D0            ;GET A BYTE OF SRC
        ADD.B   0(A2,D0),D1             ;SUM ITS BLACK BITS
        MOVE.B  1*ROW(A0),D0
        ADD.B   0(A2,D0),D1
        MOVE.B  2*ROW(A0),D0            ;REPEAT FOR ALL 8 BYTES
        ADD.B   0(A2,D0),D1
        MOVE.B  3*ROW(A0),D0
        ADD.B   0(A2,D0),D1
        MOVE.B  4*ROW(A0),D0
        ADD.B   0(A2,D0),D1
        MOVE.B  5*ROW(A0),D0
        ADD.B   0(A2,D0),D1
        MOVE.B  6*ROW(A0),D0
        ADD.B   0(A2,D0),D1
        MOVE.B  7*ROW(A0),D0
        ADD.B   0(A2,D0),D1
        LSL.B   #2,D1                   ;QUAD 0..64 FOR 0..256
        BCC.S   OK
        MOVEQ   #-1,D1                  ;MAKE 256 INTO 255
OK      MOVE.B  D1,(A1)+                ;PUT COUNT TO DST
        ADD     #1,A0                   ;BUMP SRC TO NEXT BYTE
        DBRA    D2,NXTSRC               ;LOOP FOR 72 BYTES OF SRC
        MOVEM.L (SP)+,A2                ;RESTORE REGS
        RTS                             ;AND RETURN

Counts  .BYTE   0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4         ;256 BYTE TABLE
        .BYTE   1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5         ;WITH COUNT OF 1 BITS
        .BYTE   1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5         ;FOR EACH BYTE
        .BYTE   2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6
        .BYTE   1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5
        .BYTE   2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6
        .BYTE   2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6
        .BYTE   3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7
        .BYTE   1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5
        .BYTE   2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6
        .BYTE   2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6
        .BYTE   3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7
        .BYTE   2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6
        .BYTE   3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7
        .BYTE   3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7
        .BYTE   4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8



        .SEG '        '
        .PROC DeleteChar
;---------------------------------------------------------------
;
;  PROCEDURE DeleteChar(VAR str: Str255);
;
        MOVE.L  (SP)+,A0                ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                ;POP ADDR OF STRING
        SUB.B   #1,(A1)                 ;SUBTRACT ONE FROM LENGTH
        JMP     (A0)                    ;AND RETURN



        .SEG '        '
        .PROC AppendChar
;---------------------------------------------------------------
;
;  PROCEDURE AppendChar(ch: CHAR; VAR str: str255);
;
        MOVE.L  (SP)+,A0                ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                ;POP ADDR OF STRING
        MOVE    (SP)+,D1                ;GET CHARACTER
        ADD.B   #1,(A1)                 ;BUMP STRING LENGTH
        CLR     D0                      ;GET READY FOR BYTE
        MOVE.B  (A1),D0                 ;GET NEW LENGTH
        MOVE.B  D1,0(A1,D0)             ;INSTALL CHARACTER
        JMP     (A0)                    ;AND RETURN





        .SEG '        '
        .PROC WindPaint
;---------------------------------------------------------------
;
;  PROCEDURE WindPaint(flag: BOOLEAN);
;
        MOVE.L  (SP)+,A0                ;POP RETURN ADDR
        MOVE.B  (SP)+,PaintWhite        ;SET OR CLEAR LO-MEM FLAG
        JMP     (A0)                    ;RETURN



        .SEG '        '
        .PROC TrimBBox
;---------------------------------------------------------------
;
;  PROCEDURE TrimBBox(dstBuf: QDPtr; VAR dstRect: Rect);
;
PARAMSIZE       .EQU    8
DSTBUF          .EQU    PARAMSIZE+8-4           ;LONG
DSTRECT         .EQU    DSTBUF-4                ;LONG

        LINK    A6,#0                           ;NO LOCALS
        MOVEM.L D3-D7/A2-A4,-(SP)               ;SAVE REGS
        MOVE.L  DSTRECT(A6),A4                  ;GET ADDR OF DSTRECT
        MOVE.L  DSTBUF(A6),A3                   ;GET ADDR OF DSTBUF

        MOVE    BOTTOM(A4),D7                   ;GET DSTRECT BOTTOM
        SUB     TOP(A4),D7                      ;SUB TOP FOR HEIGHT
        BLE     EMPTY                           ;SKIP IF EMPTY

;
;  SCAN FOR TOP NON-ZERO OR EMPTY
;
        MOVE    TOP(A4),D0                      ;GET DSTRECT TOP
        MULU    #52,D0                          ;TIMES ROW FOR OFFSET
        MOVE.L  A3,A0                           ;COPY BUFSTART
        ADD.L   D0,A0                           ;POINT TO RECT TOPLEFT
        MOVE.L  A0,A2                           ;SAVE FOR LATER
        MOVE    D7,D1                           ;GET HEIGHT
        MULU    #13,D1                          ;TIMES 13 FOR LONGS
        SUB     #1,D1                           ;MINUS 1 FOR DBRA COUNT
TOPLP   TST.L   (A0)+                           ;IS THIS LONG ZERO ?
        DBNE    D1,TOPLP                        ;LOOP TILL NON-ZERO OR EMPTY
        BEQ     EMPTY                           ;QUIT IF EMPTY

        SUB     #4,A0                           ;UNDO AUTO-INCR
        MOVE.L  A0,D0                           ;COPY ADDRESS
        SUB.L   A3,D0                           ;SUBTRACT START ADDRESS
        DIVU    #52,D0                          ;DIV BY ROW FOR VERT COORD
        MOVE    D0,TOP(A4)                      ;STORE TOP COORD INTO DSTRECT

;
;  FIND BOTTOM NON-ZERO
;
        MOVE    BOTTOM(A4),D0                   ;GET DSTRECT BOTTOM
        MULU    #52,D0                          ;TIMES ROW FOR OFFSET
        MOVE.L  A3,A0                           ;COPY BUFSTART
        ADD.L   D0,A0                           ;POINT TO RECT BOTTOM
BOTLP   TST.L   -(A0)                           ;IS THIS LONG ZERO ?
        BEQ.S   BOTLP                           ;YES, LOOP TILL NON-ZERO
        MOVE.L  A0,D0                           ;COPY ADDRESS
        SUB.L   A3,D0                           ;SUBTRACT START ADDRESS
        DIVU    #52,D0                          ;DIV BY ROW FOR VERT COORD
        ADD     #1,D0                           ;ADD 1 FOR BBOX BOTTOM
        MOVE    D0,BOTTOM(A4)                   ;STORE BOTTOM INTO DSTRECT

;
;  FIND LEFTMOST ONE-BIT
;
        SUB     #1,D7                           ;CALC HEIGHT-1 FOR DBRA

        MOVE    LEFT(A4),D2                     ;GET DSTRECT LEFT
        AND     #$FFF0,D2                       ;TRUNCATE TO LEFT WORD
        MOVE    D2,D0                           ;MAKE A COPY
        LSR     #3,D0                           ;DIV BY 8 FOR BYTE OFFSET
        MOVE.L  A2,A1                           ;POINT TO RECT TOP
        ADD     D0,A1                           ;OFFSET TO LEFT

LMORE   MOVE.L  A1,A0                           ;POINT TO TOP OF COLUMN
        MOVE    D7,D1                           ;GET HEIGHT-1
        CLR     D0                              ;CLEAR ACCUMULATOR
LSCAN   OR      (A0),D0                         ;OR A WORD INTO ACCUMULATOR
        ADD     #52,A0                          ;BUMP TO NEXT ROW
        DBRA    D1,LSCAN                        ;LOOP HEIGHT ROWS
        BNE.S   LFOUND                          ;BR IF COL NOT BLANK
        ADD     #16,D2                          ;SKIP BLANK WORD
        ADD     #2,A1
        BRA     LMORE                           ;TRY ANOTHER COLUMN

LFOUND  ADD     D0,D0                           ;GET A BIT FROM LEFT
        BCS.S   LDONE                           ;QUIT IF ITS A ONE
        ADD     #1,D2                           ;ELSE BUMP DSTRECT LEFT
        BRA     LFOUND                          ;AND LOOP FOR MORE
LDONE   MOVE    D2,LEFT(A4)                     ;UPDATE NEW LEFT COORD


;
;  FIND RIGHTMOST ONE-BIT
;
        MOVE    RIGHT(A4),D2                    ;GET DSTRECT RIGHT
        ADD     #15,D2
        AND     #$FFF0,D2                       ;ROUND UP TO RIGHT WORD
        MOVE    D2,D0                           ;MAKE A COPY
        LSR     #3,D0                           ;DIV BY 8 FOR BYTE OFFSET
        LEA     -2(A2,D0),A1                    ;POINT TO RECT TOP RIGHT

RMORE   MOVE.L  A1,A0                           ;POINT TO TOP OF COLUMN
        MOVE    D7,D1                           ;GET HEIGHT-1
        CLR     D0                              ;CLEAR ACCUMULATOR
RSCAN   OR      (A0),D0                         ;OR A WORD INTO ACCUMULATOR
        ADD     #52,A0                          ;BUMP TO NEXT ROW
        DBRA    D1,RSCAN                        ;LOOP HEIGHT ROWS
        BNE.S   RFOUND                          ;BR IF COL NOT BLANK
        SUB     #16,D2                          ;SKIP BLANK WORD
        SUB     #2,A1                           ;BUMP TO NEXT COL LEFT
        BRA     RMORE                           ;TRY ANOTHER COLUMN

RFOUND  LSR     #1,D0                           ;GET A BIT FROM RIGHT
        BCS.S   RDONE                           ;QUIT IF ITS A ONE
        SUB     #1,D2                           ;ELSE BUMP RIGHT COORD
        BRA     RFOUND                          ;AND LOOP FOR MORE
RDONE   MOVE    D2,RIGHT(A4)                    ;UPDATE NEW RIGHT COORD

        BRA.S   DONE


EMPTY   CLR.L   (A4)+                           ;RETURN DSTRECT = (0,0,0,0)
        CLR.L   (A4)+

DONE    MOVEM.L (SP)+,D3-D7/A2-A4               ;RESTORE REGS
        UNLK    A6                              ;RELEASE STACK FRAME
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        ADD     #PARAMSIZE,SP                   ;STRIP PARAMS
        JMP     (A0)                            ;AND RETURN



        .SEG '        '
        .FUNC Min
;---------------------------------------------------------------
;
;  FUNCTION Min(i,j: INTEGER): INTEGER;
;
        MOVE.L  (SP)+,A0                ;POP RETURN ADDR
        MOVE    (SP)+,D1                ;POP J
        MOVE    (SP)+,D0                ;POP I
        CMP     D0,D1                   ;IS J < I ?
        BGE.S   DONE                    ;NO, CONINUE
        EXG     D0,D1                   ;YES, SWAP I AND J
DONE    MOVE    D0,(SP)                 ;STORE RESULT
        JMP     (A0)                    ;RETURN



        .SEG '        '
        .FUNC Max
;---------------------------------------------------------------
;
;  FUNCTION Max(i,j: INTEGER): INTEGER;
;
        MOVE.L  (SP)+,A0                ;POP RETURN ADDR
        MOVE    (SP)+,D1                ;POP J
        MOVE    (SP)+,D0                ;POP I
        CMP     D0,D1                   ;IS J > I ?
        BLE.S   DONE                    ;NO, CONINUE
        EXG     D0,D1                   ;YES, SWAP I AND J
DONE    MOVE    D0,(SP)                 ;STORE RESULT
        JMP     (A0)                    ;RETURN



        .SEG '        '
        .FUNC PinWord
        .DEF  PinPt
;---------------------------------------------------------------
;
;  FUNCTION PinWord(i,min,max: INTEGER): INTEGER;
;
        MOVE.L  (SP)+,A0                ;POP RETURN ADDR
        MOVE    (SP)+,D2                ;POP MAX
        MOVE    (SP)+,D1                ;POP MIN
        MOVE    (SP)+,D0                ;POP I
        BSR.S   PINASM                  ;CALL LOCAL ROUTINE
        MOVE    D0,(SP)                 ;STORE RESULT
        JMP     (A0)                    ;AND RETURN


;---------------------------------------------------------------
;
;  PROCEDURE PinPt(VAR pt: Point; pinRect: Rect);
;
PinPt   MOVE.L  (SP)+,D0                ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                ;POP ADDR OF PINRECT
        MOVE.L  (SP)+,A0                ;POP ADDR OF PT
        MOVE.L  D0,-(SP)                ;PUSH RETURN ADDR

        MOVE    (A0),D0                 ;GET PT.V
        MOVE    TOP(A1),D1              ;GET PINRECT.TOP
        MOVE    BOTTOM(A1),D2           ;GET PINRECT.BOTTOM
        BSR.S   PINASM                  ;PIN RESULT IN D0
        MOVE    D0,(A0)+                ;STORE PINNED PT.V

        MOVE    (A0),D0                 ;GET PT.H
        MOVE    LEFT(A1),D1             ;GET PINRECT.LEFT
        MOVE    RIGHT(A1),D2            ;GET PINRECT.RIGHT
        BSR.S   PINASM                  ;PIN RESULT IN D0
        MOVE    D0,(A0)+                ;STORE PINNED PT.H
        RTS                             ;RETURN

;
;  LOCAL ROUTINE TO PIN D0 AGAINST D1,D2
;
PINASM  CMP     D1,D0                   ;IS I < MIN ?
        BGE.S   MINOK                   ;NO, CONTINUE
        MOVE    D1,D0                   ;YES, PIN RESULT AT MIN
MINOK   CMP     D2,D0                   ;IS I > MAX ?
        BLE.S   MAXOK                   ;NO, CONTINUE
        MOVE    D2,D0                   ;YES, PIN RESULT AT MAX
MAXOK   RTS



        .SEG '        '
        .PROC SwapBool
        .DEF  SwapWord,SwapPt,SwapRect
;---------------------------------------------------------------
;
;  PROCEDURE SwapBool(VAR a,b: BOOLEAN);
;
        MOVEQ   #0,D1                   ;ONE BYTE
        BRA.S   SWAPASM                 ;SHARE CODE

;
;  PROCEDURE SwapWord(VAR a,b: INTEGER);
;
SWAPWORD MOVEQ  #1,D1                   ;TWO BYTES
         BRA.S  SWAPASM                 ;SHARE CODE

;
;  PROCEDURE SwapPt(VAR a,b: Point);
;
SWAPPT  MOVEQ   #3,D1                   ;FOUR BYTES
        BRA.S   SWAPASM                 ;SHARE CODE

;
;  PROCEDURE SwapRect(VAR a,b: Rect);
;
SWAPRECT MOVEQ  #7,D1                   ;EIGHT BYTES
                                        ;SHARE CODE
;
;  ENTER HERE WITH BYTECOUNT-1 IN D1
;
SWAPASM MOVE.L  (SP)+,D0                ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                ;POP DST ADDR
        MOVE.L  (SP)+,A0                ;POP SRC ADDR
        MOVE.L  D0,-(SP)                ;PUSH RETURN ADDR
LOOP    MOVE.B  (A1),D0                 ;GET A DST BYTE
        MOVE.B  (A0),(A1)+              ;COPY A BYTE FROM SRC TO DST
        MOVE.B  D0,(A0)+                ;PUT DST BYTE INTO SRC
        DBRA    D1,LOOP                 ;LOOP FOR ALL BYTES
        RTS                             ;AND RETURN



        .SEG '        '
        .FUNC EqualLongs
;---------------------------------------------------------------
;
;  FUNCTION EqualLongs(srcPtr,dstPtr: QDPtr; longCount: INTEGER): BOOLEAN;
;
        MOVE.L  (SP)+,D0                ;POP RETURN ADDR
        MOVE    (SP)+,D1                ;POP COUNT
        MOVE.L  (SP)+,A1                ;POP DSTPTR
        MOVE.L  (SP)+,A0                ;POP SRCPTR
        MOVE.L  D0,-(SP)                ;PUSH RETURN ADDR
        SUB     #1,D1                   ;INIT DBRA COUNT
LOOP    CMPM.L  (A0)+,(A1)+             ;COMPARE A LONG
        DBNE    D1,LOOP                 ;LOOP TILL DIFFERENT OR COUNT
        SEQ     4(SP)                   ;SET BOOLEAN RESULT
        NEG.B   4(SP)                   ;CONVERT $FF TO $01 PASCAL BOOLEAN
        RTS



        .SEG '        '
        .PROC CopyLongs
;---------------------------------------------------------------
;
;  PROCEDURE CopyLongs(srcPtr,dstPtr: QDPtr; longCount: INTEGER);
;
        MOVE.L  (SP)+,D0                ;POP RETURN ADDR
        MOVE    (SP)+,D1                ;POP COUNT
        MOVE.L  (SP)+,A1                ;POP DSTPTR
        MOVE.L  (SP)+,A0                ;POP SRCPTR
        MOVE.L  D0,-(SP)                ;PUSH RETURN ADDR
        BRA.S   START                   ;GO TO LOOP START
LOOP    MOVE.L  (A0)+,(A1)+             ;COPY A LONG
START   DBRA    D1,LOOP                 ;LOOP FOR ALL LONGS
        RTS



        .SEG '        '
        .PROC GridMouse
;---------------------------------------------------------------
;
;  PROCEDURE GridMouse(hMask,vMask,hOffset,vOffset: INTEGER);
;
        MOVE.L  (SP)+,A0                ;POP RETURN ADDR
        MOVE.L  (SP)+,MouseOffset       ;INSTALL OFFSET
        MOVE.L  (SP)+,MouseMask         ;INSTALL MASK
        JMP     (A0)                    ;AND RETURN


        .SEG '        '
        .PROC GetSelPat
;---------------------------------------------------------------
;
;  PROCEDURE GetSelPat(patIndex: INTEGER; VAR pat: Pattern);
;  given index, return striped "marching ants" pattern
;
        MOVE.L  (SP)+,A0                ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                ;POP ADDR OF PATTERN
        MOVE    (SP)+,D0                ;POP PATINDEX
        AND     #7,D0                   ;TREAT PATINDEX MOD 8
        MOVE.B  #$F8,D1                 ;INIT PATTERN DATA
        ROR.B   D0,D1                   ;USE PATINDEX TO ROTATE
        MOVE    #7,D2                   ;INIT LOOP COUNT
NXTBYTE MOVE.B  D1,(A1)+                ;STORE BYTE OF PATTERN
        ROL.B   #1,D1                   ;ROTATE FOR NEXT BYTE
        DBRA    D2,NXTBYTE              ;LOOP 8 BYTES
        JMP     (A0)                    ;AND RETURN



        .SEG '        '
        .PROC CalcEdges
;----------------------------------------------------------------
;
;   PROCEDURE CalcEdges(srcBuf,dstBuf: QDPtr;
;                       topVert,height: INTEGER;
;                       lassoBlack: BOOLEAN);
;
;   CALC THE EDGES OF SRCBUF INTO DSTBUF
;
ROW               .EQU    52                 ;BYTES IN A ROW
LONGS             .EQU    13                 ;LONGS IN A ROW

PARAMSIZE         .EQU     14
SRC               .EQU     PARAMSIZE+8-4     ;LONG
DST               .EQU     SRC-4             ;LONG
TOPVERT           .EQU     DST-2             ;WORD, (GLOBAL coordinate)
HEIGHT            .EQU     TOPVERT-2         ;WORD
LASSOBLK          .EQU     HEIGHT-2          ;BOOLEAN


         LINK     A6,#-ROW                   ;ALLOCATE SCANBUF
         MOVEM.L  A2/D3/D4,-(SP)             ;SAVE REGS
         CLR.L    D4                         ;CLEAR INVERTFLAG
         TST.B    LASSOBLK(A6)               ;IS LASSOBLACK TRUE ?
         BEQ.S    NOTBLK                     ;NO, CONTINUE
         NOT.L    D4                         ;YES, INVERTFLAG := ALL ONES
NOTBLK   MOVE     TOPVERT(A6),D0             ;GET TOP VERT
         MULU     #ROW,D0                    ;TIMES ROWBYTES FOR OFFSET
         MOVE.L   SRC(A6),A0                 ;POINT TO SRC
         ADD.L    D0,A0                      ;OFFSET SRC TO TOPVERT
         MOVE.L   DST(A6),A1                 ;POINT TO DST
         ADD.L    D0,A1                      ;OFFSET DST TO TOPVERT
         MOVE.L   SP,A2                      ;POINT TO SCANBUF
         MOVE     HEIGHT(A6),D3              ;GET SCANLINE COUNT
         BRA      START                      ;GO TO LOOP START
;
;  AND SRC WITH ABOVE, BELOW, RIGHT-SHIFT, STORING INTO SCANBUF
;
NXTROW   MOVE     #LONGS-1,D2                ;INIT LOOP COUNT
         SUB      D0,D0                      ;CLEAR X-BIT
LOOP1    MOVE.L   (A0)+,D0                   ;GET A LONG FROM SRC
         AND.L    -ROW-4(A0),D0              ;AND WITH SCAN ABOVE
         AND.L    ROW-4(A0),D0               ;AND WITH SCAN BELOW
         MOVE.L   D0,D1                      ;COPY RESULT
         ROXR.L   #1,D1                      ;SHIFT RIGHT WITH CARRY
         AND.L    D1,D0                      ;AND WITH RIGHT-SHIFT
         MOVE.L   D0,(A2)+                   ;STORE RESULT INTO SCANBUF
         DBRA     D2,LOOP1                   ;LOOP 13 LONGS
;
;  AND WITH LEFT SHIFT AND DIFF WITH ORIGINAL FOR EDGES
;
         ADD      #ROW,A1                    ;BUMP DSTPTR TO END OF ROW
         MOVE     #LONGS-1,D2                ;INIT LOOP COUNT
         SUB      D0,D0                      ;CLEAR X-BIT
LOOP2    MOVE.L   -(A2),D0                   ;GET LONG FROM SCANBUF
         ROXL.L   #1,D0                      ;SHIFT LEFT WITH CARRY
         AND.L    (A2),D0                    ;AND WITH UNSHIFTED
         NOT.L    D0                         ;INVERT THE INSET RESULT
         AND.L    -(A0),D0                   ;AND WITH ORIGINAL SRC
         EOR.L    D4,-(A1)                   ;INVERT DST IF LASSOBLACK
         AND.L    D0,(A1)                    ;AND RESULT INTO DST
         DBRA     D2,LOOP2                   ;LOOP 13 LONGS

         ADD      #ROW,A0                    ;BUMP SRC PTR TO NEXT ROW
         ADD      #ROW,A1                    ;BUMP DSTPTR TO NEXT ROW
START    DBRA     D3,NXTROW                  ;LOOP HEIGHT ROWS

         MOVEM.L  (SP)+,A2/D3/D4             ;RESTORE REGS
         UNLK     A6                         ;RELEASE STACK FRAME
         MOVE.L   (SP)+,A0                   ;POP RETURN ADDR
         ADD      #PARAMSIZE,SP              ;STRIP PARAMS
         JMP      (A0)                       ;AND RETURN



        .SEG '        '
        .PROC InvertChunk
;-------------------------------------------------------------------
;
;  PROCEDURE InvertChunk(dstAddr: LongInt; wordWd,height: INTEGER);
;
        MOVE.L  (SP)+,D0                        ;POP RETURN ADDR
        MOVE    (SP)+,D2                        ;GET SCANLINE HEIGHT
        MOVE    (SP)+,D1                        ;GET WORDCOUNT
        MOVE.L  (SP)+,A1                        ;GET DSTADDR
        MOVE.L  D0,-(SP)                        ;PUSH RETURN ADDR

        MOVE    #52,A0                          ;GET ROWBYTES
        SUB     D1,A0                           ;SUBTRACT WORDCNT TWICE
        SUB     D1,A0                           ;FOR END OF ROW ADJUST
        SUBQ    #1,D1                           ;WORDCNT-1 FOR DBRA
        BRA.S   START                           ;GO TO LOOP START

NXTROW  MOVE    D1,D0                           ;INIT WORD LOOP COUNTER
NXTWORD NOT     (A1)+                           ;INVERT A WORD
        DBRA    D0,NXTWORD                      ;LOOP ALL WORDS IN ROW
        ADD.W   A0,A1                           ;ADJUST DSTPTR AT END OF SCAN
START   DBRA    D2,NXTROW                       ;LOOP ALL ROWS
        RTS



        .SEG '        '
        .PROC AntsToScrn
        .REF  HideCursor,ShowCursor
;----------------------------------------------------------------
;
;  PROCEDURE AntsToScrn(edgeBuf,srcnPtr: QDPtr;
;                       maskRect: Rect;
;                       patIndex: INTEGER);
;
PARAMSIZE       .EQU    14
EDGEBUF         .EQU    PARAMSIZE+8-4           ;LONG
SCRNPTR         .EQU    EDGEBUF-4               ;LONG
MASKRECT        .EQU    SCRNPTR-4               ;LONG
PATINDEX        .EQU    MASKRECT-2              ;WORD

        LINK    A6,#0                           ;NO LOCALS
        MOVEM.L D3-D7/A2-A4,-(SP)               ;SAVE REGS
        MOVE.L  EDGEBUF(A6),A3                  ;POINT TO EDGES
        MOVE.L  SCRNPTR(A6),A4                  ;POINT TO SCREEN
        MOVE.L  MASKRECT(A6),A1                 ;POINT TO MASKRECT
;
;  COMPUTE HIDEVERT AND SHOWVERT FOR CURSOR SHIELDING
;
        MOVE    #1000,D6                        ;HIDEVERT := NEVER
        MOVE    #1000,D7                        ;SHOWVERT := NEVER

        MOVE    CRSRRECT+LEFT,D0                ;GET CRSRLEFT
        SUB     #80,D0                          ;MAKE WINDOW RELATIVE
        CMP     RIGHT(A1),D0                    ;IS CURSOR OFF ON RIGHT ?
        BGE.S   CRSROK                          ;YES, DONT HIDE IT

        MOVE    CRSRRECT+RIGHT,D0
        SUB     #80,D0                          ;MAKE WINDOW RELATIVE
        CMP     LEFT(A1),D0                     ;IS CURSOR OFF ON LEFT ?
        BLE.S   CRSROK                          ;YES, DONT HIDE IT

        MOVE    CRSRRECT+TOP,D0                 ;GET CURSOR TOP
        SUB     #48,D0                          ;MAKE WINDOW RELATIVE
        CMP     BOTTOM(A1),D0                   ;IS CRSRTOP >= DSTBOT ?
        BGE.S   CRSROK                          ;YES, LEAVE IT ALONE

        MOVE    CRSRRECT+BOTTOM,D1              ;GET CURSOR BOTTOM
        SUB     #48,D1                          ;MAKE WINDOW RELATIVE
        CMP     TOP(A1),D1                      ;IS CRSRBOT <= DSTTOP ?
        BLE.S   CRSROK                          ;YES, LEAVE IT
;
;  CURSOR DOES INTERSECT MASKRECT, ADJUST HIDEVERT AND SHOWVERT
;
        MOVE    D0,D6                           ;HIDEVERT := CRSRTOP
        CMP     TOP(A1),D6                      ;IS HIDEVERT < MASK TOP ?
        BGE.S   HIDEOK                          ;NO, CONTINUE
        MOVE    TOP(A1),D6                      ;YES, PIN AT MASK TOP
HIDEOK
        MOVE    D1,D7                           ;SHOWVERT := CRSRBOT
        CMP     BOTTOM(A1),D7                   ;IS SHOWVERT > MASK BOTTOM ?
        BLE.S   SHOWOK                          ;NO, CONTINUE
        MOVE    BOTTOM(A1),D7                   ;YES, PIN AT MASK BOTTOM
SHOWOK

CRSROK

        MOVE    TOP(A1),D0                      ;GET MASKRECT.TOP
        MULU    #52,D0                          ;TIMES BUFROW
        ADD.L   D0,A3                           ;OFFSET EDGESPTR

        MOVE    TOP(A1),D0                      ;GET MASKRECT.TOP
        MULU    screenRow,D0                    ;TIMES SCRNROW
        ADD.L   D0,A4                           ;OFFSET SCRNPTR

        MOVE    PATINDEX(A6),D0                 ;GET PATTERN INDEX
        MOVE.L  #$07070707,D1                   ;INIT PATTERN DATA
        ROR.L   D0,D1                           ;PRE-ROTATE PATTERN DATA
        MOVE    TOP(A1),D3                      ;VERT := TOP VERT
        BRA.S   START                           ;GO TO LOOP START

NXTROW  MOVE    #12,D2                          ;INIT LONGCOUNT
NXTLONG MOVE.L  (A4),D0                         ;GET A LONG FROM SCREEN
        EOR.L   D1,D0                           ;XOR WITH PATTERN DATA
        OR.L    (A3)+,D0                        ;OR WITH EDGES
        EOR.L   D1,D0                           ;XOR WITH PATTERN DATA
        MOVE.L  D0,(A4)+                        ;PUT RESULT BACK TO SCREEN
        DBRA    D2,NXTLONG                      ;LOOP 13 LONGS

        ADD     screenRow,A4
        SUB     #52,A4                          ;BUMP SCRNPTR TO NEXT ROW
        ROL.L   #1,D1                           ;ROTATE PATTERN FOR NEXT ROW
        ADD     #1,D3                           ;VERT := VERT + 1

START   CMP     D6,D3                           ;TIME TO HIDE CURSOR
        BNE.S   NOHIDE                          ;NO, CONTINUE
        JSR     HIDECURSOR
NOHIDE  CMP     D7,D3                           ;TIME TO RESTORE CURSOR ?
        BNE.S   NOSHOW                          ;NO, CONTINUE
        JSR     SHOWCURSOR                      ;YES, SHOW IT
NOSHOW  CMP     BOTTOM(A1),D3                   ;IS VERT >= BOTTOM VERT ?
        BLT.S   NXTROW                          ;NO, LOOP ALL ROWS

        MOVEM.L (SP)+,D3-D7/A2-A4               ;RESTORE REGS
        UNLK    A6                              ;RELEASE STACK FRAME
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        ADD     #PARAMSIZE,SP                   ;STRIP PARAMETERS
        JMP     (A0)                            ;AND RETURN



        .SEG '        '
        .PROC MASKIT
        .DEF  VSeed
;------------------------------------------------------------
;
;   PROCEDURE MaskIt(dstStart,dstLimit,srcOffset: LongInt;
;                    hLim,row: INTEGER;
;                    readPtr,donePtr: EntryPtr;
;                    VAR writePtr: EntryPtr);
;
PARAMSIZE       .EQU    28                      ;TOTAL BYTES OF PARAMS
DSTSTART        .EQU    PARAMSIZE+8-4           ;LONG
DSTLIMIT        .EQU    DSTSTART-4              ;LONG
SRCOFFSET       .EQU    DSTLIMIT-4              ;LONG
HLIM            .EQU    SRCOFFSET-2             ;WORD
ROW             .EQU    HLIM-2                  ;WORD
READPTR         .EQU    ROW-4                   ;LONG
DONEPTR         .EQU    READPTR-4               ;LONG
WRITEPTR        .EQU    DONEPTR-4               ;LONG, VAR
;
;  OFFSETS WITHIN A QUEUE ENTRY
;
ADDR            .EQU    0                       ;LONG
BUMP            .EQU    4                       ;WORD, +2 OR -2
TWOH            .EQU    6                       ;WORD, WORD COORD * 2
QMASK           .EQU    8                       ;WORD
ENTRYSIZE       .EQU    10                      ;TOTAL BYTES


        LINK    A6,#0                           ;ALLOCATE STACK FRAME
        MOVEM.L D3-D7/A2-A4,-(SP)               ;SAVE REGISTERS
        MOVE.L  READPTR(A6),A3                  ;GET READPTR
        MOVE.L  WRITEPTR(A6),A0                 ;GET VAR ADDR
        MOVE.L  (A0),A4                         ;GET WRITEPTR
        MOVE.L  SRCOFFSET(A6),D7                ;GET SRCOFFSET
        MOVEQ   #-1,D3                          ;USE D3 TO HOLD BLACK
        BRA     START                           ;GO TO LOOP START

MORE    MOVE.L  ADDR(A3),A2                     ;GET READPTR^.ADDR
        MOVE    QMASK(A3),D4                    ;GET READPTR^.MASK
        MOVE    BUMP(A3),D5                     ;GET READPTR^.BUMP
        OR      D4,(A2)                         ;OR MASK INTO DST
;
;  CHECK ONCE FOR NEW SEED IN OPPOSITE HORIZONTAL DIRECTION
;
        NEG     D5                              ;CALC NEGBUMP
        MOVE    TWOH(A3),D0                     ;GET READPTR^.TWOH
        ADD     D5,D0                           ;ADD NEGBUMP FOR NEWHORIZ
        BLT.S   FIRSTOK                         ;SKIP IF NEWHORIZ NEG
        CMP     HLIM(A6),D0                     ;IS NEWHORIZ < HLIM ?
        BGE.S   FIRSTOK                         ;NO, SKIP
        ADD     D5,A2                           ;ADD NEGBUMP FOR NEWADDR
        CMP     (A2),D3                         ;IS DST ALREADY BLACK ?
        BEQ.S   FIRSTOK                         ;YES, SKIP

        MOVE    0(A2,D7.L),D0                   ;GET SRC DATA FROM NEWADDR OFFSET
        BSR     HSeed                           ;CALC HORIZ SEED
        MOVE    (A2),D0                         ;GET DST WORD
        OR      D6,(A2)                         ;OR SEED INTO DST
        CMP     (A2),D0                         ;HAS ANYTHING CHANGED ?
        BEQ.S   FIRSTOK                         ;NO, SKIP
;
;  PUSH LEFT OR RIGHT ENTRY
;
        MOVE.L  A2,(A4)+                        ;PUT NEWADDR
        MOVE    D5,(A4)+                        ;PUT NEGBUMP
        MOVE    TWOH(A3),D0                     ;GET READPTR^.TWOH
        ADD     D5,D0                           ;ADD NEGBUMP FOR NEWHORIZ
        MOVE    D0,(A4)+                        ;PUT NEWHORIZ
        MOVE    D6,(A4)+                        ;PUT MASK = SEED


FIRSTOK MOVE.L  ADDR(A3),A2                     ;GET READPTR^.ADDR AGAIN
        MOVE    BUMP(A3),D5                     ;GET BUMP AGAIN

;
;  CHECK FOR NEW SEED BELOW
;
LOOP    ADD     ROW(A6),A2                      ;NEWADDR := ADDR + ROW
        CMP.L   DSTLIMIT(A6),A2                 ;IS NEWADDR < DSTLIMIT ?
        BHS.S   OKBELOW                         ;NO, SKIP
        CMP     (A2),D3                         ;IS DST ALREADY BLACK ?
        BEQ.S   OKBELOW                         ;YES, SKIP

        MOVE    0(A2,D7.L),D1                   ;GET SRC WORD AT NEWADDR OFFSET
        BSR     VSeed                           ;CALC VERTSEED
        MOVE    (A2),D0                         ;GET DST WORD AT NEWADDR
        OR      D6,(A2)                         ;OR SEED INTO DST
        CMP     (A2),D0                         ;DID ANYTHING CHANGE ?
        BEQ.S   OKBELOW                         ;NO, SKIP
;
;  PUSH BELOW ENTRY
;
        MOVE.L  A2,(A4)+                        ;PUT NEWADDR
        MOVE    D5,(A4)+                        ;PUT BUMP
        MOVE    TWOH(A3),(A4)+                  ;PUT TWOH
        MOVE    D6,(A4)+                        ;PUT MASK = SEED

OKBELOW


;
;  CHECK FOR NEW SEED ABOVE
;
        MOVE.L  ADDR(A3),A2                     ;GET READPTR^.ADDR
        SUB     ROW(A6),A2                      ;NEWADDR := ADDR - ROW
        CMP.L   DSTSTART(A6),A2                 ;IS NEWADDR >= DSTSTART ?
        BLO     OKABOVE                         ;NO, SKIP ABOVE
        CMP     (A2),D3                         ;IS DST ALREADY BLACK ?
        BEQ.S   OKABOVE                         ;YES, SKIP

        MOVE    0(A2,D7.L),D1                   ;GET SRCWORD AT NEWADDR OFFSET
        BSR     VSeed                           ;CALC VERTSEED
        MOVE    (A2),D0                         ;GET DSTWORD AT NEWADDR
        OR      D6,(A2)                         ;OR SEED INTO DST
        CMP     (A2),D0                         ;DID ANYTHING CHANGE ?
        BEQ.S   OKABOVE                         ;NO, SKIP
;
;  PUSH ABOVE ENTRY
;
        MOVE.L  A2,(A4)+                        ;PUT NEWADDR
        MOVE    D5,(A4)+                        ;PUT BUMP
        MOVE    TWOH(A3),(A4)+                  ;PUT TWOH
        MOVE    D6,(A4)+                        ;PUT MASK = SEED
OKABOVE


;
;  TRY TO CONTINUE ACTIVE ENTRY AS LONG AS POSSIBLE
;
        MOVE    TWOH(A3),D0                     ;GET TWOH
        ADD     D5,D0                           ;NEWHORIZ := TWOH + BUMP
        BLT.S   NXTSRC                          ;SKIP IF NEWHORIZ NEG
        CMP     HLIM(A6),D0                     ;IS NEWHORIZ < HLIM ?
        BGE.S   NXTSRC                          ;NO, SKIP

        MOVE.L  ADDR(A3),A2                     ;GET READPTR^.ADDR
        ADD     D5,A2                           ;ADD BUMP FOR NEWADDR
        CMP     (A2),D3                         ;IS DST ALREADY BLACK ?
        BEQ.S   NXTSRC                          ;YES, SKIP

        MOVE    0(A2,D7.L),D0                   ;GET SRC DATA AT NEWADDR OFFSET
        BSR     HSeed                           ;CALC HORIZ SEED
;
;  CONTINUE CURRENT ENTRY LEFT OR RIGHT
;
        MOVE.L  A2,ADDR(A3)                     ;ADDR := NEWADDR
        ADD     D5,TWOH(A3)                     ;TWOH := TWOH + BUMP
        MOVE    D6,D4                           ;MASK := SEED

        MOVE    (A2),D0                         ;GET DST DATA
        OR      D4,(A2)                         ;OR MASK INTO DST
        CMP     (A2),D0                         ;HAS DST CHANGED ?
        BNE     LOOP                            ;NO, LOOP FOR MORE


;----------------------------------------------------------
;
;  BUMP READPTR AND LOOP FOR ALL ENTRIES IN SRC QUEUE
;
NXTSRC  ADD     #ENTRYSIZE,A3                   ;BUMP READPTR TO NEXT ENTRY
START   CMP.L   DONEPTR(A6),A3                  ;IS READPTR = DONEPTR ?
        BNE     MORE                            ;NO, GET NEXT QUEUE ENTRY
        MOVE.L  WRITEPTR(A6),A0                 ;GET VAR ADDR
        MOVE.L  A4,(A0)                         ;UPDATE WRITEPTR
        MOVEM.L (SP)+,D3-D7/A2-A4               ;RESTORE REGISTERS
        UNLK    A6                              ;RELEASE STACK FRAME
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        ADD     #PARAMSIZE,SP                   ;STRIP PARAMETERS
        JMP     (A0)                            ;AND RETURN
        .ASCII  'MASKIT  '



;------------------------------------------------------------
;
;  LOCAL FUNCTION HSeed(mask,data: INTEGER): INTEGER;
;
;  ENTER WITH:  D5  BUMP DIRECTION
;               D0  DATA
;               D4  MASK
;               D3  ALL ONES
;
;  RETURNS:     D6  SEED
;
;  CLOBBERS:    D0,D6
;
HSEED   MOVE    D3,D6                           ;INIT SEED TO ALL ONES
        TST     D5                              ;IS BUMP POSITIVE ?
        BPL.S   RSEED                           ;YES, CALC RIGHTSEED
;
;  CALC LEFTSEED
;
LSEED   TST     D4                              ;IS LEFT BIT OF MASK ON ?
        BPL.S   HINVERT                         ;NO RETURN ZERO
        CMP     D6,D0                           ;IS DATA ALL ONES ?
        BNE.S   LSTART                          ;NO, GO TO LOOP START
        RTS                                     ;YES, RETURN ALL ONES
HLEFT   ADD     D6,D6                           ;SHIFT A ZERO INTO SEED
LSTART  LSR     #1,D0                           ;GET LO BIT OF DATA
        BCS     HLEFT                           ;LOOP UNTIL BIT FALSE
        BRA.S   HINVERT                         ;INVERT RESULT AND QUIT
;
;  CALC RIGHTSEED
;
RSEED   BTST    #0,D4                           ;IS RIGHT BIT OF MASK ON ?
        BEQ     HINVERT                         ;NO, RETURN ZERO
        CMP     D6,D0                           ;IS DATA ALL ONES ?
        BNE.S   RSTART                          ;NO, GO TO LOOP START
        RTS                                     ;YES, RETURN ALL ONES
HRIGHT  LSR     #1,D6                           ;SHIFT A ZERO INTO SEED
RSTART  ADD     D0,D0                           ;GET HI BIT OF DATA
        BCS     HRIGHT                          ;LOOP UNTIL BIT FALSE
HINVERT NOT     D6                              ;INVERT SEED ANSWER
        RTS



;---------------------------------------------------
;
;  LOCAL FUNCTION VSeed(mask,data: INTEGER ): INTEGER;
;
;  ENTER WITH:  D4 MASK
;               D1 DATA
;               D3 ALL ONES
;
;  RETURNS      D6 SEED
;
;  CLOBBERS     D0,D1,D2
;
VSEED   MOVE    D4,D6                           ;COPY MASK
        AND     D1,D6                           ;SEED := MASK AND DATA
        BNE.S   NOTZERO                         ;CONTINUE IF RESULT NONZERO
        RTS                                     ;ELSE RETURN ZERO

NOTZERO CMP     D3,D1                           ;IS DATA ALL ONES ?
        BNE.S   NOTONES                         ;NO, CONTINUE
        MOVE    D1,D6                           ;YES, RESULT = ONES
        RTS                                     ;AND QUIT

NOTONES CMP     D1,D6                           ;IS SEED = DATA ?
        BNE.S   RIGHTLP                         ;NO, CONTINUE
        RTS                                     ;YES WE'RE DONE

RIGHTLP MOVE    D6,D2                           ;REMEMBER OLDSEED
        MOVE    D6,D0                           ;COPY SEED
        LSR     #1,D0                           ;SHIFT SEED RIGHT
        OR      D0,D6                           ;LEAK SEED TO RIGHT
        AND     D1,D6                           ;AND WITH DATA TO LIMIT
        CMP     D6,D2                           ;IS SEED SAME AS OLD SEED ?
        BNE     RIGHTLP                         ;NO TRY SOME MORE

LEFTLP  MOVE    D6,D2                           ;REMEMBER OLDSEED
        MOVE    D6,D0                           ;COPY SEED
        ADD     D0,D0                           ;SHIFT SEED LEFT
        OR      D0,D6                           ;LEAK SEED TO LEFT
        AND     D1,D6                           ;AND WITH DATA TO LIMIT
        CMP     D6,D2                           ;IS SEED SAME AS OLD SEED ?
        BNE     LEFTLP                          ;NO TRY SOME MORE
        RTS                                     ;AND RETURN



        .SEG '        '
        .FUNC   VertSeed
        .REF    VSeed
;-----------------------------------------------------------
;
;  FUNCTION  VertSeed(mask,data: INTEGER): INTEGER;
;  Used only to compute initial seed.
;
PARAMSIZE       .EQU    4                       ;TOTAL BYTES
RESULT          .EQU    PARAMSIZE+8             ;WORD
XMASK           .EQU    RESULT-2                ;WORD
XDATA           .EQU    XMASK-2                  ;WORD

        LINK    A6,#0
        MOVEM.L D3-D7/A2-A4,-(SP)               ;SAVE REGS
        MOVE    XMASK(A6),D4
        MOVE    XDATA(A6),D1
        MOVEQ   #-1,D3
        JSR     VSeed
        MOVE    D6,RESULT(A6)
        MOVEM.L (SP)+,D3-D7/A2-A4               ;RESTORE REGS
        UNLK    A6
        MOVE.L  (SP)+,(SP)                      ;STRIP PARAMS
        RTS



        .SEG '        '
        .PROC ShieldWindow
        .REF  ShieldCursor
;------------------------------------------------------
;
;  PROCEDURE ShieldWindow;
;
MYRECT  .EQU    -8
VARSIZE .EQU    MYRECT

        LINK    A6,#VARSIZE                     ;ALLOCATE STACK FRAME
        LEA     MYRECT(A6),A0                   ;POINT TO MYRECT
        MOVE    #48,(A0)+                       ;TOP = 48
        MOVE    #80,(A0)+                       ;LEFT = 80
        MOVE    #288,(A0)+                      ;BOTTOM = 288
        MOVE    #496,(A0)+                      ;RIGHT = 496
        PEA     MYRECT(A6)                      ;PUSH ADDR OF MYRECT
        CLR.L   -(SP)                           ;PUSH OFFSET = (0,0)
        JSR     ShieldCursor                    ;SHIELD THE CURSOR
        UNLK    A6                              ;RELEASE STACK FRAME
        RTS                                     ;AND RETURN



        .SEG '        '
        .PROC InvertBuf,1
;--------------------------------------------------------
;
;  PROCEDURE InvertBuf(bufPtr: Ptr);
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                        ;GET BUFPTR
        MOVE    #3119,D0                        ;INIT COUNT
CLRNXT  NOT.L   (A1)+                           ;INVERT A LONG
        DBRA    D0,CLRNXT                       ;LOOP 3120 LONGS
        JMP     (A0)                            ;AND RETURN



        .SEG '        '
        .PROC ZeroBuf,1
;--------------------------------------------------------
;
;  PROCEDURE ZeroBuf(bufPtr: Ptr);
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                        ;GET BUFPTR
        MOVE    #3119,D0                        ;INIT COUNT
CLRNXT  CLR.L   (A1)+                           ;CLEAR A LONG
        DBRA    D0,CLRNXT                       ;LOOP 144 LONGS
        JMP     (A0)                            ;AND RETURN



        .SEG '        '
        .PROC ZeroMem,2
;--------------------------------------------------------
;
;  PROCEDURE ZeroMem(bandPtr: Ptr; byteCount: INTEGER);
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE    (SP)+,D0                        ;POP BYTECOUNT
        LSR     #2,D0                           ;QUARTER FOR LONGCOUNT
        MOVE.L  (SP)+,A1                        ;GET BANDPTR
        BRA.S   START                           ;GO TO LOOP START
CLRNXT  CLR.L   (A1)+                           ;CLEAR A LONG
START   DBRA    D0,CLRNXT                       ;LOOP 480 LONGS
        JMP     (A0)                            ;AND RETURN



        .SEG '        '
        .PROC ZeroFat,1
;--------------------------------------------------------
;
;  PROCEDURE ZeroFat(fatPtr: Ptr);
;
          MOVE.L    4(SP),A0                 ;GET FATPTR
          MOVE      #29,D0                   ;INIT COUNT
CLRNXT    CLR.L     (A0)+                    ;CLEAR 32 BITS
          MOVE.L    #$00000800,(A0)+         ;INSTALL RIGHT BORDER
          DBRA      D0,CLRNXT                ;LOOP ALL 30 ROWS
          MOVE.L    #$FFFFFFFF,(A0)+         ;INSTALL BOTTOM BORDER
          MOVE.L    #$FFFFF800,(A0)+
          CLR.L     (A0)+                    ;CLEAR EXTRA LONG AT END
          CLR.L     (A0)+
          MOVE.L    (SP)+,(SP)               ;STRIP PARAM
          RTS                                ;AND RETURN



        .SEG '        '
        .PROC ScrnToBuf,2
        .REF  ShieldWindow,ShowCursor
;--------------------------------------------------------
;
;  PROCEDURE ScrnToBuf(scrnPtr,bufPtr: Ptr);
;
          JSR       ShieldWindow             ;PROTECT CURSOR
          MOVE.L    (SP)+,D0                 ;POP RETURN ADDR
          MOVE.L    (SP)+,A1                 ;POP DST = BUFPTR
          MOVE.L    (SP)+,A0                 ;POP SRC = SCRNPTR
          MOVE.L    D0,-(SP)                 ;PUSH RETURN ADDR
          MOVEM.L   D3-D7/A2-A6,-(SP)        ;SAVE REGS
          MOVE      #240,-(SP)               ;INIT ROW COUNT
NXTROW    MOVEM.L   (A0),D0-D7/A2-A6         ;SUCK UP 13 LONGS FROM SCREEN
          MOVEM.L   D0-D7/A2-A6,(A1)         ;SPIT THEM OUT TO BUF
          ADD       screenRow,A0             ;BUMP SCREENPTR
          ADD       #52,A1                   ;BUMP BUFPTR
          SUB       #1,(SP)                  ;DECREMENT ROWCOUNT
          BNE       NXTROW                   ;LOOP 240 ROWS
          TST       (SP)+                    ;POP ROW COUNT
          MOVEM.L   (SP)+,D3-D7/A2-A6        ;RESTORE REGS
          JSR       ShowCursor               ;RESTORE CURSOR
          RTS                                ;AND RETURN



        .SEG '        '
        .PROC BufToScrn,2
;--------------------------------------------------------
;
;  PROCEDURE BufToScrn(bufPtr,scrnPtr: Ptr; top,bottom: INTEGER);
;
;  top and bottom coords are relative to start of buffer
;
;  cursor has already been hidden.
;
          MOVE.L    (SP)+,D0                 ;POP RETURN ADDR
          MOVE      (SP)+,D1                 ;POP BOTTOM
          MOVE      (SP)+,D2                 ;POP TOP
          MOVE.L    (SP)+,A1                 ;POP SCRNPTR
          MOVE.L    (SP)+,A0                 ;POP BUFPTR
          MOVE.L    D0,-(SP)                 ;PUSH RETURN ADDR
          MOVEM.L   D3-D7/A2-A6,-(SP)        ;SAVE REGS
          SUB       D2,D1                    ;CALC HEIGHT
          BLE.S     GOHOME                   ;QUIT IF COUNT <= 0
          MOVE      D1,-(SP)                 ;INIT ROW COUNT
          MOVE      D2,D1                    ;COPY TOP COORD
          MULU      #52,D1                   ;CALC SRC OFFSET
          ADD.L     D1,A0                    ;OFFSET SRCPTR
          MULU      screenRow,D2             ;CALC SCRN OFFSET
          ADD.L     D2,A1                    ;OFFSET SCRNPTR
NXTROW    MOVEM.L   (A0),D0-D7/A2-A6         ;SUCK UP 13 LONGS FROM BUF
          MOVEM.L   D0-D7/A2-A6,(A1)         ;SPIT THEM OUT TO SCREEN
          ADD       #52,A0                   ;BUMP SRCPTR
          ADD       screenRow,A1             ;BUMP SCREENPTR
          SUB       #1,(SP)                  ;DECREMENT ROWCOUNT
          BNE       NXTROW                   ;LOOP 240 ROWS
          TST       (SP)+                    ;POP ROW COUNT
GOHOME    MOVEM.L   (SP)+,D3-D7/A2-A6        ;RESTORE REGS
          RTS                                ;AND RETURN



        .SEG '        '
        .PROC BufToBuf,2
;--------------------------------------------------------
;
;  PROCEDURE BufToBuf(srcPtr,dstPtr: Ptr);
;
          MOVE.L    (SP)+,D0                 ;POP RETURN ADDR
          MOVE.L    (SP)+,A1                 ;POP DSTPTR
          MOVE.L    (SP)+,A0                 ;POP SRCPTR
          MOVE.L    D0,-(SP)                 ;PUSH RETURN ADDR
          MOVEM.L   D3-D7/A2-A6,-(SP)        ;SAVE REGS
          MOVE      #240,-(SP)               ;INIT ROW COUNT
NXTROW    MOVEM.L   (A0),D0-D7/A2-A6         ;SUCK UP 13 LONGS FROM SRCBUF
          MOVEM.L   D0-D7/A2-A6,(A1)         ;SPIT THEM OUT TO DSTBUF
          ADD       #52,A0                   ;BUMP SRCPTR
          ADD       #52,A1                   ;BUMP DSTPTR
          SUB       #1,(SP)                  ;DECREMENT ROWCOUNT
          BNE       NXTROW                   ;LOOP 240 ROWS
          TST       (SP)+                    ;POP ROW COUNT
          MOVEM.L   (SP)+,D3-D7/A2-A6        ;RESTORE REGS
          RTS                                ;AND RETURN



        .SEG '        '
        .PROC BufOrBuf,2
;--------------------------------------------------------
;
;  PROCEDURE BufOrBuf(srcPtr,dstPtr: Ptr);
;
        MOVE.L  (SP)+,D0                ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                ;POP DSTPTR
        MOVE.L  (SP)+,A0                ;POP SRCPTR
        MOVE.L  D0,-(SP)                ;PUSH RETURN ADDR
        MOVE    #3119,D1                ;INIT COUNT
NXTLONG MOVE.L  (A0)+,D0                ;GET A LONG OF SRC
        OR.L    D0,(A1)+                ;OR IT INTO DST
        DBRA    D1,NXTLONG              ;LOOP 3120 LONGS
        RTS                             ;AND RETURN


        .SEG '        '
        .PROC BufXorBuf,2
;--------------------------------------------------------
;
;  PROCEDURE BufXorBuf(srcPtr,dstPtr: Ptr);
;
        MOVE.L  (SP)+,D0                ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                ;POP DSTPTR
        MOVE.L  (SP)+,A0                ;POP SRCPTR
        MOVE.L  D0,-(SP)                ;PUSH RETURN ADDR
        MOVE    #3119,D1                ;INIT COUNT
NXTLONG MOVE.L  (A0)+,D0                ;GET A LONG OF SRC
        EOR.L   D0,(A1)+                ;XOR IT INTO DST
        DBRA    D1,NXTLONG              ;LOOP 3120 LONGS
        RTS                             ;AND RETURN



        .SEG '        '
        .PROC BufAndBuf,2
;--------------------------------------------------------
;
;  PROCEDURE BufAndBuf(srcPtr,dstPtr: Ptr);
;
        MOVE.L  (SP)+,D0                ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                ;POP DSTPTR
        MOVE.L  (SP)+,A0                ;POP SRCPTR
        MOVE.L  D0,-(SP)                ;PUSH RETURN ADDR
        MOVE    #3119,D1                ;INIT COUNT
NXTLONG MOVE.L  (A0)+,D0                ;GET A LONG OF SRC
        AND.L   D0,(A1)+                ;AND IT INTO DST
        DBRA    D1,NXTLONG              ;LOOP 3120 LONGS
        RTS                             ;AND RETURN



        .SEG '        '
        .PROC SwapBuf,2
;--------------------------------------------------------
;
;  PROCEDURE SwapBuf(srcBuf,dstBuf: Ptr);
;
          MOVE.L    (SP)+,D0                 ;POP RETURN ADDR
          MOVE.L    (SP)+,A1                 ;POP DSTBUF
          MOVE.L    (SP)+,A0                 ;POP SRCBUF
          MOVE.L    D0,-(SP)                 ;PUSH RETURN ADDR
          MOVE      #3119,D1                 ;INIT LONG COUNT
NXTLONG   MOVE.L    (A0),D0                  ;GET A LONG FROM SRCBUF
          MOVE.L    (A1),(A0)+               ;COPY FROM DSTBUF TO SRCBUF
          MOVE.L    D0,(A1)+                 ;AND FROM SRCBUF TO DSTBUF
          DBRA      D1,NXTLONG               ;LOOP 3120 LONGS
          RTS                                ;AND RETURN



        .SEG '        '
        .PROC FatToScrn,2
        .REF  ShieldWindow,ShowCursor
;--------------------------------------------------------
;
;  PROCEDURE FatToScrn(fatPtr,scrnPtr: Ptr);
;
          JSR       ShieldWindow             ;PROTECT CURSOR
          MOVE.L    8(SP),A0                 ;GET FATPTR
          MOVE.L    4(SP),A1                 ;GET SCRNPTR
          MOVEQ     #31,D2                   ;INIT DBRA ROWCOUNT
NXTSMALL  MOVE.L    (A0)+,(A1)+              ;COPY 32 BITS
          MOVE.W    (A0)+,(A1)+              ;COPY 16 BITS
          MOVE.B    (A0)+,(A1)+              ;COPY 8 BITS
          ADD       #1,A0                    ;SKIP ONE BYTE
          SUB       #7,A1                    ;back up to row start
          ADD       screenRow,A1             ;BUMP DST TO NEXT ROW
          DBRA      D2,NXTSMALL              ;LOOP FOR 32 ROWS

          MOVE.L    8(SP),A0                 ;GET FATPTR
          MOVE.L    4(SP),A1                 ;GET SCRNPTR
          MOVEQ     #29,D2                   ;INIT FATROW COUNT
          MOVE.L    A2,-(SP)                 ;SAVE REG
          LEA       FATNIB,A2                ;POINT TO TABLE
          CMP       #90,screenRow            ;are we running on a LISA ?
          BNE       MACROW
;
;  for speed, rowBytes is hard-coded into screen offsets,
;  and we have two versions of the loop for MAC and LISA.
;
LISAROW   MOVEQ     #12,D1                   ;INIT NIBBLE COUNT
          CMP       #26,D2                   ;ARE WE IN FIRST 4 FATROWS ?
          BLT.S     LISANIB                  ;NO, CONTINUE
          SUB       #2,D1                    ;YES, SKIP 2 NIBBLES
          ADD       #8,A1                    ;BUMP DSTPTR
          MOVE.B    (A0)+,D0                 ;GET SRC BYTE
          AND       #1,D0                    ;IS LO BIT SET ?
          BEQ.S     @1                       ;NO, CONTINUE
          NOT.B     D0                       ;YES, GET A FAT DOT
@1        MOVE.B    D0,-1(A1)                ;INSTALL 8 BYTES
          MOVE.B    D0,90-1(A1)
          MOVE.B    D0,180-1(A1)
          MOVE.B    D0,270-1(A1)
          MOVE.B    D0,360-1(A1)
          MOVE.B    D0,450-1(A1)
          MOVE.B    D0,540-1(A1)
          CLR.B     630-1(A1)

LISANIB   MOVE.B    (A0),D0                  ;GET A BYTE OF SRC
          BTST      #0,D1                    ;IS NIBBLE COUNT ODD ?
          BNE.S     LOLISA                   ;YES DO LO NIBBLE
HILISA    AND       #$F0,D0                  ;MASK FOR HI NIBBLE
          LSR       #2,D0                    ;SHIFT FOR TABLE INDEX
          BRA.S     LISA8                    ;PUT 8 LONGS TO SCREEN
LOLISA    AND       #$0F,D0                  ;MASK FOR LO NIBBLE
          LSL       #2,D0                    ;SHIFT FOR TABLE INDEX
          ADD       #1,A0                    ;BUMP SRCPTR
LISA8     MOVE.L    0(A2,D0),D0              ;EXPAND NIBBLE TO LONG
          MOVE.L    D0,(A1)+                 ;PUT FIRST LONG TO DST
          MOVE.L    D0,90-4(A1)              ;PUT SAME IN NEXT 6 SCANS
          MOVE.L    D0,180-4(A1)
          MOVE.L    D0,270-4(A1)
          MOVE.L    D0,360-4(A1)
          MOVE.L    D0,450-4(A1)
          MOVE.L    D0,540-4(A1)
          CLR.L     630-4(A1)                ;CLEAR LAST LONG FOR LINES
          DBRA      D1,LISANIB               ;LOOP FOR 13 NIBBLES
          ADD       #2,A0                    ;BUMP SRC TO NEXT ROW
          ADD       #720-52,A1               ;BUMP DST TO NEXT ROW
          DBRA      D2,LISAROW               ;LOOP 30 FAT ROWS
          BRA       DONE

;
;  MAC version of same code as above, rowBytes = 64.
;
MACROW    MOVEQ     #12,D1                   ;INIT NIBBLE COUNT
          CMP       #26,D2                   ;ARE WE IN FIRST 4 FATROWS ?
          BLT.S     MACNIB                   ;NO, CONTINUE
          SUB       #2,D1                    ;YES, SKIP 2 NIBBLES
          ADD       #8,A1                    ;BUMP DSTPTR
          MOVE.B    (A0)+,D0                 ;GET SRC BYTE
          AND       #1,D0                    ;IS LO BIT SET ?
          BEQ.S     @1                       ;NO, CONTINUE
          NOT.B     D0                       ;YES, GET A FAT DOT
@1        MOVE.B    D0,-1(A1)                ;INSTALL 8 BYTES
          MOVE.B    D0,64-1(A1)
          MOVE.B    D0,128-1(A1)
          MOVE.B    D0,192-1(A1)
          MOVE.B    D0,256-1(A1)
          MOVE.B    D0,320-1(A1)
          MOVE.B    D0,384-1(A1)
          CLR.B     448-1(A1)

MACNIB    MOVE.B    (A0),D0                  ;GET A BYTE OF SRC
          BTST      #0,D1                    ;IS NIBBLE COUNT ODD ?
          BNE.S     LOMAC                    ;YES DO LO NIBBLE
HIMAC     AND       #$F0,D0                  ;MASK FOR HI NIBBLE
          LSR       #2,D0                    ;SHIFT FOR TABLE INDEX
          BRA.S     MAC8                     ;PUT 8 LONGS TO SCREEN
LOMAC     AND       #$0F,D0                  ;MASK FOR LO NIBBLE
          LSL       #2,D0                    ;SHIFT FOR TABLE INDEX
          ADD       #1,A0                    ;BUMP SRCPTR
MAC8      MOVE.L    0(A2,D0),D0              ;EXPAND NIBBLE TO LONG
          MOVE.L    D0,(A1)+                 ;PUT FIRST LONG TO DST
          MOVE.L    D0,64-4(A1)              ;PUT SAME IN NEXT 6 SCANS
          MOVE.L    D0,128-4(A1)
          MOVE.L    D0,192-4(A1)
          MOVE.L    D0,256-4(A1)
          MOVE.L    D0,320-4(A1)
          MOVE.L    D0,384-4(A1)
          CLR.L     448-4(A1)                ;CLEAR LAST LONG FOR LINES
          DBRA      D1,MACNIB                ;LOOP FOR 13 NIBBLES
          ADD       #2,A0                    ;BUMP SRC TO NEXT ROW
          ADD       #512-52,A1               ;BUMP DST TO NEXT ROW
          DBRA      D2,MACROW                ;LOOP 30 FAT ROWS

DONE      JSR       ShowCursor               ;RESTORE CURSOR
          MOVE.L    (SP)+,A2                 ;RESTORE REG
          MOVE.L    (SP)+,A0                 ;POP RETURN ADDR
          ADD       #8,SP                    ;STRIP PARAMS
          JMP       (A0)                     ;AND RETURN

FATNIB   .LONG   $00000000,$000000FE,$0000FE00,$0000FEFE
         .LONG   $00FE0000,$00FE00FE,$00FEFE00,$00FEFEFE
         .LONG   $FE000000,$FE0000FE,$FE00FE00,$FE00FEFE
         .LONG   $FEFE0000,$FEFE00FE,$FEFEFE00,$FEFEFEFE




        .SEG 'SegFlip '
        .PROC VFlipBuf,2
;--------------------------------------------------------
;
;  PROCEDURE VFlipBuf(srcBuf,dstBuf: Ptr);
;
          MOVE.L    (SP)+,D0                 ;POP RETURN ADDR
          MOVE.L    (SP)+,A1                 ;POP DSTPTR
          MOVE.L    (SP)+,A0                 ;POP SRCPTR
          MOVE.L    D0,-(SP)                 ;PUSH RETURN ADDR
          MOVEM.L   D3-D7/A2-A6,-(SP)        ;SAVE REGS
          ADD       #12428,A0                ;OFFSET SRC TO BOTTOM (239 * 52)
          MOVE      #240,-(SP)               ;INIT ROW COUNT
NXTROW    MOVEM.L   (A0),D0-D7/A2-A6         ;SUCK UP 13 LONGS FROM SRCBUF
          MOVEM.L   D0-D7/A2-A6,(A1)         ;SPIT THEM OUT TO DSTBUF
          SUB       #52,A0                   ;BUMP SRCPTR UPWARD
          ADD       #52,A1                   ;BUMP DSTPTR DOWNWARD
          SUB       #1,(SP)                  ;DECREMENT ROWCOUNT
          BNE       NXTROW                   ;LOOP 240 ROWS
          TST       (SP)+                    ;POP ROW COUNT
          MOVEM.L   (SP)+,D3-D7/A2-A6        ;RESTORE REGS
          RTS                                ;AND RETURN


        .SEG 'SegFlip '
        .PROC HFlipBuf,4
;--------------------------------------------------------
;
;  PROCEDURE HFlipBuf(srcBuf,dstBuf: Ptr; top,bot: INTEGER);
;
          MOVE.L    (SP)+,D0                 ;POP RETURN ADDR
          MOVE      (SP)+,D1                 ;POP BOTTOM
          MOVE      (SP)+,D2                 ;POP TOP
          MOVE.L    (SP)+,A1                 ;POP DSTPTR
          MOVE.L    (SP)+,A0                 ;POP SRCPTR
          MOVE.L    D0,-(SP)                 ;PUSH RETURN ADDR

          MOVEM.L   D3-D4,-(SP)              ;SAVE REGS
          SUB       D2,D1                    ;HEIGHT := BOT - TOP
          MULU      #52,D2                   ;CALC ROWBYTES * TOP
          ADD       D2,A0                    ;OFFSET SRCPTR TO TOP
          ADD       D2,A1                    ;OFFSET DSTPTR TO TOP
          ADD       #52,A0                   ;OFFSET SRC TO RIGHT
          SUB       #1,D1                    ;INIT ROWCOUNT = HEIGHT - 1
NXTROW    MOVEQ     #25,D3                   ;INIT WORDCOUNT
NXTLONG   MOVE.W    -(A0),D0                 ;GET A WORD OF SRC
          MOVEQ     #15,D2                   ;INIT BITCOUNT
NXTBIT    LSR       #1,D0                    ;GET A BIT OF SRC
          ADDX      D4,D4                    ;SHIFT INTO DST
          DBRA      D2,NXTBIT                ;LOOP 16 BITS
          MOVE      D4,(A1)+                 ;WRITE WORD TO DST
          DBRA      D3,NXTLONG               ;LOOP 26 WORDS
          ADD       #104,A0                  ;BUMP SRCPTR TO NEXT ROW
          DBRA      D1,NXTROW                ;LOOP HEIGHT ROWS

          MOVEM.L   (SP)+,D3-D4              ;RESTORE REGS
          RTS                                ;AND RETURN



        .SEG 'SegFlip '
        .PROC RotBuf,3
;--------------------------------------------------------
;
;  PROCEDURE RotBuf(srcBuf,dstBuf: Ptr; height: INTEGER);
;
          MOVE.L    (SP)+,D0                 ;POP RETURN ADDR
          MOVE      (SP)+,D2                 ;POP HEIGHT
          MOVE.L    (SP)+,A1                 ;POP DSTPTR
          MOVE.L    (SP)+,A0                 ;POP SRCPTR
          MOVE.L    D0,-(SP)                 ;PUSH RETURN ADDR
          TST       D2                       ;IS HEIGHT > 0 ?
          BLE.S     DONE                     ;NO, QUIT
          MOVEM.L   D3-D4,-(SP)              ;SAVE REGS
          ADD       #15,D2                   ;ROUND HEIGHT UP TO MULT OF 16
          LSR       #4,D2                    ;CALC (HEIGHT + 15) DIV 16
          SUB       #52,A1                   ;COMPENSATE DST FOR FIRST
          BRA.S     START                    ;GO TO LOOP START

NXTBAND   MOVE      #$8000,D1                ;INIT MASK FOR DST
NXTROW    ADD       #240*52,A1               ;POINT DST TO BOTTOM
          MOVEQ     #14,D3                   ;INIT WORD COUNT
NXTWORD   MOVE      (A0)+,D0                 ;GET A WORD OF SRC
          MOVEQ     #15,D4                   ;INIT BIT COUNT
NXTBIT    ADD       D0,D0                    ;GET A BIT OF SRC
          BCC.S     WHITE1
          OR        D1,(A1)                  ;SET A BIT OF DST
WHITE1    SUB       #52,A1                   ;BUMP DST UP ONE ROW
          DBRA      D4,NXTBIT                ;LOOP 16 BITS
          DBRA      D3,NXTWORD               ;LOOP 15 WORDS
          ADD       #22,A0                   ;SKIP NEXT 11 SRC WORDS
          LSR       #1,D1                    ;SHIFT MASK BIT RIGHT
          BNE.S     NXTROW                   ;CONTINUE IF NOT ZERO
          ADD       #2,A1                    ;BUMP DST TO RIGHT
START     DBRA      D2,NXTBAND               ;LOOP ALL BANDS
          MOVEM.L   (SP)+,D3-D4              ;RESTORE REGS
DONE      RTS                                ;AND RETURN



        .SEG '        '
        .FUNC GetRgnMax
;--------------------------------------------------------
;
;  FUNCTION  GetRgnMax: INTEGER;
;
          MOVE.L    GRAFGLOBALS(A5),A0
          MOVE      RGNMAX(A0),4(SP)         ;GET RGNMAX
          RTS



        .SEG '        '
        .FUNC CreateFile
;---------------------------------------------------------------
;
;  FUNCTION CreateFile(drive,version: INTEGER; VAR fileName: STR80): INTEGER;
;
PARAMSIZE   .EQU    8                           ;TOTAL BYTES OF PARAMS
RESULT      .EQU    PARAMSIZE+8                 ;WORD
DRIVE       .EQU    RESULT-2                    ;WORD
VERSION     .EQU    DRIVE-2                     ;WORD, LO BYTE USED
FILENAME    .EQU    VERSION-4                   ;LONG

CMDBLOCK    .EQU    -IOQELSIZE                  ;IO COMMAND BLOCK
VARSIZE     .EQU    CMDBLOCK                    ;TOTAL LOCAL VARS

         LINK     A6,#VARSIZE                   ;ALLOCATE STACK FRAME
         MOVE.L   SP,A0                         ;POINT TO COMMAND BLOCK
         MOVE.L   FILENAME(A6),IOFileName(A0)   ;INSTALL PTR TO FILENAME
         MOVE     DRIVE(A6),IODrvNum(A0)        ;USE SPECIFIED DRIVE
         MOVE.B   VERSION+1(A6),IOFileType(A0)  ;INSTALL VERSION BYTE
         CLR.B    IOPermssn(A0)                 ;CLEAR PERMISSIONS
         _CREATE                                ;CREATE THE FILE
         MOVE     D0,RESULT(A6)                 ;RETURN RESULT CODE
         UNLK     A6                            ;RELEASE STACK FRAME
         MOVE.L   (SP)+,A0                      ;POP RETURN ADDR
         ADD      #PARAMSIZE,SP                 ;STRIP PARAMS
         JMP      (A0)                          ;AND RETURN



        .SEG '        '
        .FUNC DeleteFile
;---------------------------------------------------------------
;
;  FUNCTION DeleteFile(drive,version: INTEGER; VAR fileName: STR80): INTEGER;
;
PARAMSIZE   .EQU    8                           ;TOTAL BYTES OF PARAMS
RESULT      .EQU    PARAMSIZE+8                 ;WORD
DRIVE       .EQU    RESULT-2                    ;WORD
VERSION     .EQU    DRIVE-2                     ;WORD
FILENAME    .EQU    VERSION-4                   ;LONG

CMDBLOCK    .EQU    -IOQELSIZE                  ;IO COMMAND BLOCK
VARSIZE     .EQU    CMDBLOCK                    ;TOTAL LOCAL VARS

         LINK     A6,#VARSIZE                   ;ALLOCATE STACK FRAME
         MOVE.L   SP,A0                         ;POINT TO COMMAND BLOCK
         MOVE.L   FILENAME(A6),IOFileName(A0)   ;INSTALL PTR TO FILENAME
         MOVE     DRIVE(A6),IODrvNum(A0)        ;USE SPECIFIED DRIVE
         MOVE.B   VERSION+1(A6),IOFileType(A0)  ;INSTALL VERSION BYTE
         CLR.B    IOPermssn(A0)                 ;CLEAR PERMISSIONS
         _DELETE                                ;DELETE THE FILE
         MOVE     D0,RESULT(A6)                 ;RETURN RESULT CODE
         UNLK     A6                            ;RELEASE STACK FRAME
         MOVE.L   (SP)+,A0                      ;POP RETURN ADDR
         ADD      #PARAMSIZE,SP                 ;STRIP PARAMS
         JMP      (A0)                          ;AND RETURN



        .SEG '        '
        .FUNC OpenFile
;---------------------------------------------------------------------------
;
;  FUNCTION OpenFile(drive,version: INTEGER; fileName: STR80;
;                    ownBuf: Ptr; VAR refNum: INTEGER): INTEGER;
;
;
PARAMSIZE   .EQU    16                          ;TOTAL BYTES OF PARAMS
RESULT      .EQU    PARAMSIZE+8                 ;WORD
DRIVE       .EQU    RESULT-2                    ;WORD
VERSION     .EQU    DRIVE-2                     ;WORD
FILENAME    .EQU    VERSION-4                   ;LONG
OWNBUF      .EQU    FILENAME-4                  ;LONG
REFNUM      .EQU    OWNBUF-4                    ;LONG

CMDBLOCK    .EQU    -IOQELSIZE                  ;IO COMMAND BLOCK
VARSIZE     .EQU    CMDBLOCK                    ;TOTAL LOCAL VARS


        LINK    A6,#VARSIZE                     ;ALLOCATE STACK FRAME
        LEA     CMDBLOCK(A6),A0                 ;POINT TO COMMAND BLOCK
        MOVE.L  FILENAME(A6),IOFileName(A0)     ;INSTALL PTR TO FILENAME
        MOVE    DRIVE(A6),IODrvNum(A0)          ;USE SPECIFIED DRIVE
        MOVE.B  VERSION+1(A6),IOFileType(A0)    ;INSTALL VERSION BYTE
        CLR.B   IOPermssn(A0)                   ;CLEAR PERMISSIONS
        MOVE.L  OWNBUF(A6),IOOwnBuf(A0)         ;INSTALL PTR TO BUFFER
        _OPEN                                   ;OPEN THE FILE
        MOVE.L  REFNUM(A6),A1                   ;GET ADDR OF REFNUM
        MOVE    IORefNum(A0),(A1)               ;INSTALL INTO REFNUM
        MOVE    D0,RESULT(A6)                   ;RETURN RESULT CODE
        UNLK    A6                              ;RELEASE STACK FRAME
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        ADD     #PARAMSIZE,SP                   ;STRIP PARAMS
        JMP     (A0)                            ;AND RETURN



        .SEG '        '
        .FUNC CloseFile
;---------------------------------------------------------------
;
;  FUNCTION CloseFile(refNum: INTEGER): INTEGER;
;
;
PARAMSIZE   .EQU    2                           ;TOTAL BYTES OF PARAMS
RESULT      .EQU    PARAMSIZE+8                 ;WORD
REFNUM      .EQU    RESULT-2                    ;WORD

CMDBLOCK    .EQU    -IOQELSIZE                  ;IO COMMAND BLOCK
VARSIZE     .EQU    CMDBLOCK                    ;TOTAL LOCAL VARS


        LINK    A6,#VARSIZE                     ;ALLOCATE STACK FRAME
        LEA     CMDBLOCK(A6),A0                 ;POINT TO COMMAND BLOCK
        MOVE    REFNUM(A6),IORefNum(A0)         ;INSTALL REFNUM
        _CLOSE                                  ;CLOSE THE FILE
        MOVE    D0,RESULT(A6)                   ;RETURN RESULT CODE
        UNLK    A6                              ;RELEASE STACK FRAME
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        ADD     #PARAMSIZE,SP                   ;STRIP PARAMS
        JMP     (A0)                            ;AND RETURN



        .SEG '        '
        .FUNC SetEOF
;---------------------------------------------------------------
;
;  FUNCTION SetEOF(refNum: INTEGER; byteCount: LongInt): INTEGER;
;
;
PARAMSIZE   .EQU    6                           ;TOTAL BYTES OF PARAMS
RESULT      .EQU    PARAMSIZE+8                 ;WORD
REFNUM      .EQU    RESULT-2                    ;WORD
BYTECNT     .EQU    REFNUM-4                    ;LONG

CMDBLOCK    .EQU    -IOQELSIZE                  ;IO COMMAND BLOCK
VARSIZE     .EQU    CMDBLOCK                    ;TOTAL LOCAL VARS


        LINK    A6,#VARSIZE                     ;ALLOCATE STACK FRAME
        LEA     CMDBLOCK(A6),A0                 ;POINT TO COMMAND BLOCK
        MOVE    REFNUM(A6),IORefNum(A0)         ;INSTALL REFNUM
        MOVE.L  BYTECNT(A6),IOLEOF(A0)          ;INSTALL FILE LENGTH
        _SETEOF                                 ;SET END OF FILE
        MOVE    D0,RESULT(A6)                   ;RETURN RESULT CODE
        UNLK    A6                              ;RELEASE STACK FRAME
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        ADD     #PARAMSIZE,SP                   ;STRIP PARAMS
        JMP     (A0)                            ;AND RETURN



        .SEG '        '
        .FUNC WriteData
        .DEF  ReadData
;---------------------------------------------------------------
;
;  FUNCTION WriteData(refNum:        INTEGER;
;                     VAR startByte: LongInt;
;                     byteCount:     LongInt;
;                     bufPtr: Ptr):  INTEGER;
;
PARAMSIZE   .EQU    14                          ;TOTAL BYTES OF PARAMS
RESULT      .EQU    PARAMSIZE+8                 ;WORD
REFNUM      .EQU    RESULT-2                    ;WORD
STARTBYTE   .EQU    REFNUM-4                    ;LONG, VAR
BYTECOUNT   .EQU    STARTBYTE-4                 ;LONG
BUF         .EQU    BYTECOUNT-4                 ;LONG

CMDBLOCK    .EQU    -IOQELSIZE                  ;IO COMMAND BLOCK
VARSIZE     .EQU    CMDBLOCK                    ;TOTAL LOCAL VARS


        MOVEQ   #1,D1                           ;OPERATION = WRITE
        BRA.S   DOIT                            ;SHARE CODE

READDATA CLR    D1                              ;OPERATION = READ

DOIT    LINK    A6,#VARSIZE                     ;ALLOCATE STACK FRAME
        LEA     CMDBLOCK(A6),A0                 ;POINT TO IO COMMAND BLOCK
        MOVE    REFNUM(A6),IORefNum(A0)         ;INSTALL REFNUM
        MOVE.L  BUF(A6),IOBuffer(A0)            ;INSTALL BUFPTR
        MOVE.L  STARTBYTE(A6),A1                ;GET VAR ADDR
        MOVE.L  BYTECOUNT(A6),D0                ;GET BYTECOUNT
        MOVE.L  (A1),IOPosOffset(A0)            ;INSTALL STARTBYTE
        ADD.L   D0,(A1)                         ;UPDATE STARTBYTE
        MOVE.L  D0,IOByteCount(A0)              ;INSTALL BYTECOUNT
        MOVE.W  #1,IOPosMode(A0)                ;RELATIVE TO FILE START

        TST     D1                              ;IS THIS A READ OR A WRITE ?
        BEQ.S   READIT
        _WRITE                                  ;WRITE THE DATA
        BRA.S   DONE

READIT  _READ                                   ;READ THE DATA

DONE    MOVE    D0,RESULT(A6)                   ;RETURN RESULT CODE
        UNLK    A6                              ;RELEASE STACK FRAME
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        ADD     #PARAMSIZE,SP                   ;STRIP PARAMS
        JMP     (A0)                            ;AND RETURN




          .END
