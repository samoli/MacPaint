PROGRAM MacPaint;

{  BitMap Painting Program by Bill Atkinson }

{$D- }
{$R- }
{$X- }


USES {$U obj:HeapZone    }  HeapZone,
     {$U obj:QuickDraw   }  QuickDraw,
     {$U obj:MacFMgr     }  Fonts,
     {$U obj:GrafUtil    }  GrafUtil,
     {$U obj:MacEMgr     }  Events,
     {$U obj:MacWMgr     }  Windows,
     {$U obj:MacDskMgr   }  DeskMgr,
     {$U obj:TextEdit    }  TextEdit,
     {$U obj:MacDMgr     }  Dialogs,
     {$U obj:MacRMgr     }  Resources,
     {$U obj:MacSMgr     }  ScrapMgr,
     {$U obj:MacMMgr     }  Menus,
     {$U obj:MacCMgr     }  Controls,
     {$U obj:StdFile     }  StdFile,
     {$U Obj:MacPrint    }  MacPrint;


CONST paintFont    =  249;
      minDiskSpace =  16384;
      firstPat     =  0;
      lastPat      =  37;
      corner       =  18;        { diameter of curvature for roundRects }

      shiftCode    =  63;
      optionCode   =  61;
      featureCode  =  48;
      alphaCode    =  62;
      bsCode       =  52;
      enterCode    =  51;
      undoCode     =  53;
      tabCode      =  55;

      printMsg     =   1;

      endOfFile    = -39;
      fileExists   = -48;

      appleMenu     = 1;

      fileMenu       = 2;
        newItem      = 1;
        openItem     = 2;
        closeItem    = 3;
        saveItem     = 4;
        saveAsItem   = 5;
        revertItem   = 6;
        prDraftItem  = 7;
        prFinalItem  = 8;
        prCatItem    = 9;
        quitItem     = 10;

      editMenu      = 3;
        undoItem    = 1;
        blank0Item  = 2;
        cutItem     = 3;
        copyItem    = 4;
        pasteItem   = 5;
        clearItem   = 6;
        blank1Item  = 7;
        invertItem  = 8;
        fillItem    = 9;
        edgesItem   = 10;
        hFlipItem   = 11;
        vFlipItem   = 12;
        rotateItem  = 13;

      aidsMenu      = 4;
        gridItem    = 1;
        fatItem     = 2;
        pageItem    = 3;
        patEdItem   = 4;
        brushItem   = 5;
        symItem     = 6;
        introItem   = 7;
        shortItem   = 8;

      fontMenu      = 5;
        firstFont   = 1;

      sizeMenu      = 6;
        firstSize   = 1;
        lastSize    = 9;

      styleMenu     = 7;
        plainItem   = 1;
        boldItem    = 2;
        italicItem  = 3;
        underItem   = 4;
        outlineItem = 5;
        shadowItem  = 6;
        blank2Item  = 7;
        leftItem    = 8;
        centerItem  = 9;
        rightItem   = 10;

      firstMenu     = appleMenu;
      lastMenu      = styleMenu;

      docWidth      = 576;
      docHeight     = 720;
      docRow        = 72;

      bandHeight    = 48;
      bandCount     = 15;

      patLeft       = 77;       { pattern rectangle }
      patRight      = 500;
      patTop        = 300;
      patBottom     = 335;
      patMid        = 317;
      patSpace      = 20;
      patRow        = 19;       { how many patterns each row }
      patStart      = 120;      { patRight - patRow*patSpace }

      lineLeft      = 15;       { line attributes rectangle }
      lineRight     = 68;
      lineTop       = 258;
      lineBottom    = 335;
      lineMid       = 40;       { (lineLeft + lineRight) DIV 2 }

      toolLeft      = 15;       { tool rectangle }
      toolRight     = 68;
      toolTop       = 28;
      toolBottom    = 249;
      toolMid       = 41;       { (toolLeft + toolRight) DIV 2  }
      toolSpace     = 22;

      lassoTool     = 0;
      selectTool    = 1;
      grabberTool   = 2;
      textTool      = 3;
      fillTool      = 4;
      sprayTool     = 5;
      brushTool     = 6;
      pencilTool    = 7;
      lineTool      = 8;
      eraseTool     = 9;
      rectTool      = 11;

      closeDlog     = 2001;
      quitDlog      = 2002;
      patEdDlog     = 2003;
      brushDlog     = 2004;
      symDlog       = 2005;
      paintDlog     = 2006;

      revertAlrt    = 2101;
      diskAlrt      = 2102;
      wFullAlrt     = 2103;
      wProtAlrt     = 2104;
      wErrAlrt      = 2105;
      rErrAlrt      = 2106;
      rScrapAlrt    = 2107;
      wScrapAlrt    = 2108;
      workSpaceAlrt = 2109;
      memFullAlrt   = 2110;
      fileLockAlrt  = 2111;
      notPntgAlrt   = 2112;
      bigScrapAlrt  = 2113;
      notRoomAlrt   = 2114;
      oldPrAlrt     = 2115;


      titleString    = 2201;
      fontCmdString  = 2202;
      headerString   = 2203;
      untitledString = 2204;
      rescueString   = 2205;
      saveString     = 2206;

      patListID      = 0;      { use system patlist }

      introPic       = 2400;
      shortPic       = 2401;

      okBtn          = 2501;
      cancelBtn      = 2502;

      buttonCorner  = 8;   { diameter for button roundRects }

      maxLines      = 20;   { how many lines of type-in text }



TYPE Str63 = String[63];

     DiskBlock = ARRAY[0..255] OF INTEGER;

     ShapeCode = (rectShape,rRectShape,ovalShape);

     Longs24 = ARRAY[1..24] OF LongInt;

     EntryPtr = ^QueueEntry;    { for mask stuff }
     QueueEntry = RECORD
                    addr:  LongInt;
                    bump:  INTEGER;     { +2 or -2 for right or left }
                    twoH:  INTEGER;     { 2 times horiz word coord }
                    mask:  INTEGER;
                  END;



VAR heapJam:            BOOLEAN;
    tempWord:           INTEGER;
    tempLong:           LongInt;
    finderMsg:          INTEGER;

    active:             BOOLEAN;
    ornFlag:            BOOLEAN;
    scrnFlag:           BOOLEAN;        { TRUE when screen in altbuf }
    quitFlag:           BOOLEAN;
    theEvent:           EventRecord;

    okPrint:            BOOLEAN;

    workFrom,workTo:    INTEGER;  { file refNums of working files }
    rescueFlag:         BOOLEAN;

    docName:            Str63;
    docDrive:           INTEGER;
    docVersion:         INTEGER;
    workDrive:          INTEGER;
    workSize:           LongInt;  { current size of workFrom file }

    scrapFile:          INTEGER;
    scrapByte:          LongInt;

    docDirty:           BOOLEAN;
    workDirty:          BOOLEAN;
    windOpen:           BOOLEAN;
    heapTop:            LongInt;

    myWind:             WindowRecord;
    deskWind:           WindowRecord;
    okButton,cancelButton: ControlHandle;

    theKey:             CHAR;

    shiftFlag:          BOOLEAN;
    featureFlag:        BOOLEAN;
    optionFlag:         BOOLEAN;
    hiResFlag:          BOOLEAN;

    theMenu:            INTEGER;
    theItem:            INTEGER;

    textFlag:           BOOLEAN;
    textLoc:            Point;
    textLeft:           INTEGER;
    caretLoc:           Point;
    caretState:         BOOLEAN;
    nextCaretTime:      LongInt;
    textLines:          INTEGER;
    textHandles:        ARRAY[1..maxLines] OF StringHandle;
    prevSizeChar:       CHAR;
    nextSizeChar:       CHAR;

    clickTime:          LongInt;
    clickCount:         INTEGER;
    clickLoc:           Point;
    killDouble:         BOOLEAN;
    skipDouble:         BOOLEAN;

    myMenus:            ARRAY[firstMenu..lastMenu] OF MenuHandle;

    patterns:           ARRAY[firstPat..lastPat] OF Pattern;
    lineSize:           INTEGER;
    halfLineSize:       INTEGER;
    borderFlag:         BOOLEAN;

    thePat:             Pattern;
    thePatIndex:        INTEGER;

    gridOn:             BOOLEAN;        { is the grid requested ? }
    toolGrid:           BOOLEAN;        { does the current tool grid ? }

    toolCursor:         Cursor;
    cursorFlag:         BOOLEAN;        { true for new cursor shape }
    inWindow:           BOOLEAN;
    inSel:              BOOLEAN;
    fatScroll:          BOOLEAN;

    txScrapHndl:        Handle;
    txScrapSize:        LongInt;
    txMinWidth:         INTEGER;
    txMinHeight:        INTEGER;
    txMaxCount:         INTEGER;

    theFont:            INTEGER;
    lastFont:           INTEGER;
    theFontID:          INTEGER;
    fontSizes:          ARRAY[firstSize..lastSize] OF INTEGER;
    theFontSize:        INTEGER;
    txJustItem:         INTEGER;
    textJust:           INTEGER;
    info:               FontInfo;

    zeroRect:           Rect;
    pageRect:           Rect;
    docRect:            Rect;

    patRect:            Rect;
    fillSample:         Rect;
    lineRect:           Rect;
    toolRect:           Rect;

    whiteFlag:          BOOLEAN;
    acceptFlag:         BOOLEAN;
    selFlag:            BOOLEAN;
    selRect:            Rect;
    pivot:              Point;

    edgeFlag:           BOOLEAN;        { true if edges in altBits }
    lassoBlack:         BOOLEAN;
    maskFlag:           BOOLEAN;        { true if mask in maskBits }
    maskBits:           BitMap;
    maskHandle:         Handle;
    maskPaste:          BOOLEAN;

    oldWhiteFlag:       BOOLEAN;
    oldAcceptFlag:      BOOLEAN;
    oldSelFlag:         BOOLEAN;
    oldSelRect:         Rect;
    oldPivot:           Point;
    oldMaskFlag:        BOOLEAN;
    oldLassoBlack:      BOOLEAN;
    selIndex:           INTEGER;

    theTool:            INTEGER;
    prevTool:           INTEGER;
    theBrush:           INTEGER;

    hSymFlag:           BOOLEAN;
    vSymFlag:           BOOLEAN;
    hvSymFlag:          BOOLEAN;
    vhSymFlag:          BOOLEAN;
    symByte:            QDByte;

    fatFlag:            BOOLEAN;
    fatBits:            BitMap;
    fatBuf:             ARRAY[0..31,0..1] OF LongInt;
    fatCenter:          Point;

    docBits:            BitMap;
    mainBits:           BitMap;
    mainBuf:            ARRAY[0..239,0..12] OF LongInt;

    altBits:            BitMap;
    altBuf:             ARRAY[0..239,0..12] OF LongInt;

    scrnPtr:            QDPtr;
    myPinRect:          Rect;
    hConstrain:         BOOLEAN;        { used by HVConstrain }
    vConstrain:         BOOLEAN;        { used by HVConstrain }
    ptConstrain:        Point;          { used by HVConstrain }

    rwShftDh:           INTEGER;        { inputs to ReadWrite }
    rwShftDv:           INTEGER;
    rwResult:           INTEGER;        { IO result from ReadWrite }
    patEdPt:            Point;
    whichMenu:          INTEGER;        { for EnableOne }


PROCEDURE EjectReset;                                             EXTERNAL;
FUNCTION  PixelTrue(h,v: INTEGER; bits: BitMap): BOOLEAN;         EXTERNAL;
PROCEDURE MapCursor(tool,lineSize,brush: INTEGER; dstPtr: QDPtr); EXTERNAL;
FUNCTION  Monkey: BOOLEAN;                                        EXTERNAL;
PROCEDURE Stretch2x(srcPtr,dstPtr: QDPtr);                        EXTERNAL;
PROCEDURE MySetItemStyle(menu: menuHandle; item: INTEGER;
                         txFace: INTEGER);                        EXTERNAL;
PROCEDURE MyGetItem(menu: menuHandle; item: INTEGER;
                    VAR itemString: Str63);                       EXTERNAL;
PROCEDURE MoreMasters;                                            EXTERNAL;
PROCEDURE ExitToShell;                                            EXTERNAL;
FUNCTION  Trunc8(i: INTEGER): INTEGER;                            EXTERNAL;
FUNCTION  GetDoubleTime: LONGINT;                                 EXTERNAL;
FUNCTION  GetCaretTime: LONGINT;                                  EXTERNAL;
PROCEDURE SystemBeep(duration: INTEGER);                          EXTERNAL;
PROCEDURE Moov(dh,dv: INTEGER);                                   EXTERNAL;
FUNCTION  MapSym(hSym,vSym,hvSym,vhSym: BOOLEAN): QDByte;         EXTERNAL;
FUNCTION  StackPtr: LongInt;                                      EXTERNAL;
PROCEDURE ShieldCursor(shieldRect: Rect; offset: Point);          EXTERNAL;
FUNCTION  WorstStack: LongInt;                                    EXTERNAL;
FUNCTION  EqualString(str1,str2: Str255): BOOLEAN;                EXTERNAL;
PROCEDURE ExpandPat(pat: Pattern; VAR exPat: Longs24;
                    hShift: INTEGER);                             EXTERNAL;

PROCEDURE DrawBrush(brush: Bits16; exPat: Longs24;
                    hCenter,vCenter: INTEGER;
                    dstBits: BitMap; clip: Rect; orMode: BOOLEAN); EXTERNAL;

FUNCTION  NearPt(pt1,pt2: Point; tol: INTEGER): BOOLEAN;          EXTERNAL;
PROCEDURE HToneRow(thisRow,nextRow:        QDPtr;
                   dstBits:                BitMap;
                   hStart,vStart,hSize:    INTEGER;
                   whiteLevel,blackLevel:  INTEGER;
                   threshold:              BOOLEAN);              EXTERNAL;
PROCEDURE SampleBits(src,dst: QDPtr);                             EXTERNAL;
PROCEDURE DeleteChar(str: Str255);                                EXTERNAL;
PROCEDURE AppendChar(ch: CHAR; str: str255);                      EXTERNAL;
PROCEDURE WindPaint(flag: BOOLEAN);                               EXTERNAL;
PROCEDURE TrimBBox(dstBuf: QDPtr; VAR dstRect: Rect);             EXTERNAL;
PROCEDURE GridMouse(hMask,vMask,hOffset,vOffset: INTEGER);        EXTERNAL;
PROCEDURE UnloadSeg(segPtr: QDPtr);                               EXTERNAL;
PROCEDURE GetSelPat(patIndex: INTEGER; VAR pat: Pattern);         EXTERNAL;
PROCEDURE CalcEdges(srcBuf,dstBuf: QDPtr; topVert,height: INTEGER;
                    lassoBlack: BOOLEAN);                         EXTERNAL;
PROCEDURE AntsToScrn(edgeBuf,srcnPtr: QDPtr;
                     maskRect: Rect; patIndex: INTEGER);          EXTERNAL;
PROCEDURE InvertChunk(dstAddr: LongInt; wordWd,height: INTEGER);  EXTERNAL;
PROCEDURE PackBits(VAR srcPtr,dstPtr: QDPtr; srcBytes: INTEGER);  EXTERNAL;
PROCEDURE UnpackBits(VAR srcPtr,dstPtr: QDPtr; dstBytes: INTEGER); EXTERNAL;
PROCEDURE InvertBuf(bufPtr: QDPtr);                               EXTERNAL;
PROCEDURE ZeroBuf(bufPtr: QDPtr);                                 EXTERNAL;
PROCEDURE ZeroFat(fatPtr: QDPtr);                                 EXTERNAL;
PROCEDURE ZeroMem(bandPtr: QDPtr; byteCount: INTEGER);            EXTERNAL;
PROCEDURE CopyLongs(srcPtr,dstPtr: QDPtr; longCount: INTEGER);    EXTERNAL;
FUNCTION  EqualLongs(srcPtr,dstPtr: QDPtr; longCount: INTEGER): BOOLEAN; EXTERNAL;
PROCEDURE BufToBuf(srcPtr,dstPtr: QDPtr);                         EXTERNAL;
PROCEDURE BufOrBuf(srcPtr,dstPtr: QDPtr);                         EXTERNAL;
PROCEDURE BufXorBuf(srcPtr,dstPtr: QDPtr);                        EXTERNAL;
PROCEDURE BufAndBuf(srcPtr,dstPtr: QDPtr);                        EXTERNAL;
PROCEDURE ScrnToBuf(scrnPtr,bufPtr: QDPtr);                       EXTERNAL;
PROCEDURE BufToScrn(bufPtr,scrnPtr: QDPtr; top,bottom: INTEGER);  EXTERNAL;
PROCEDURE SwapBuf(srcBuf,dstBuf: QDPtr);                          EXTERNAL;
PROCEDURE FatToScrn(fatPtr,scrnPtr: QDPtr);                       EXTERNAL;
PROCEDURE VFlipBuf(srcBuf,dstBuf: QDPtr);                         EXTERNAL;
PROCEDURE HFlipBuf(srcBuf,dstBuf: QDPtr; top,bot: INTEGER);       EXTERNAL;
PROCEDURE RotBuf(srcBuf,dstBuf: QDPtr; height: INTEGER);          EXTERNAL;
FUNCTION  GetRgnMax: INTEGER;                                     EXTERNAL;
FUNCTION  VertSeed(mask,data: INTEGER): INTEGER;                  EXTERNAL;
PROCEDURE MaskIt(dstStart,dstLimit,srcOffset: LongInt;
                 hLim,row: INTEGER;
                 readPtr,donePtr: EntryPtr;
                 VAR writePtr: EntryPtr);                         EXTERNAL;



{ I/O routines }
PROCEDURE FlushVol(driveNum: INTEGER);                                   EXTERNAL;
FUNCTION  FinderDoc(VAR drive,version: INTEGER; strPtr: QDPtr): INTEGER; EXTERNAL;
PROCEDURE SetDocType(drive,version: INTEGER; docName: Str63);            EXTERNAL;
FUNCTION  CheckPntg(drive,version: INTEGER; docName: Str63): BOOLEAN;    EXTERNAL;
PROCEDURE FileInfo(drive,version: INTEGER; fName: Str63;
                   VAR fileDate,fileSize: LongInt);                      EXTERNAL;
FUNCTION  DiskSpace(drive: INTEGER): LongInt;                            EXTERNAL;
FUNCTION  CountFiles(drive: INTEGER; VAR str: Str63): INTEGER;           EXTERNAL;
FUNCTION  NextFile(drive: INTEGER; index: INTEGER; fileType: ResType;
                   VAR str: Str63; VAR version: INTEGER): BOOLEAN;       EXTERNAL;
FUNCTION  CreateFile(drive,version: INTEGER; fileName: Str63): INTEGER;  EXTERNAL;
FUNCTION  DeleteFile(drive,version: INTEGER; fileName: Str63): INTEGER;  EXTERNAL;
FUNCTION  OpenFile(drive,version: INTEGER; fileName: Str63;
                   ownBuf: QDPtr; VAR refNum: INTEGER): INTEGER;         EXTERNAL;
FUNCTION  CloseFile(refNum: INTEGER): INTEGER;                           EXTERNAL;
FUNCTION  SetEOF(refNum: INTEGER; byteCount: LongInt): INTEGER;          EXTERNAL;
FUNCTION  ReadData(refNum:        INTEGER;
                   VAR startByte: LongInt;
                   byteCount:     LongInt;
                   bufPtr: QDPtr):  INTEGER;                             EXTERNAL;
FUNCTION  WriteData(refNum:        INTEGER;
                    VAR startByte: LongInt;
                    byteCount:     LongInt;
                    bufPtr: QDPtr):  INTEGER;                     EXTERNAL;



FUNCTION  Min(i,j: INTEGER): INTEGER;                             EXTERNAL;
FUNCTION  Max(i,j: INTEGER): INTEGER;                             EXTERNAL;
FUNCTION  PinWord(i,min,max: INTEGER): INTEGER;                   EXTERNAL;
PROCEDURE PinPt(VAR pt: Point; pinRect: Rect);                    EXTERNAL;
PROCEDURE SwapBool(VAR a,b: BOOLEAN);                             EXTERNAL;
PROCEDURE SwapWord(VAR a,b: INTEGER);                             EXTERNAL;
PROCEDURE SwapPt(VAR a,b: Point);                                 EXTERNAL;
PROCEDURE SwapRect(VAR a,b: Rect);                                EXTERNAL;



{$S         }
FUNCTION KeyIsDown(keyCode: INTEGER): BOOLEAN;
VAR keys: KeyMap;
BEGIN
  GetKeys(keys);
  KeyIsDown := BitTst(@keys,keyCode);
END;


{$S         }
PROCEDURE GetFatMouse(VAR pt: Point);
BEGIN
  GetMouse(pt);
  IF fatFlag THEN
    BEGIN
      IF theTool <> eraseTool THEN
        BEGIN
          pt.h := pt.h - 4;    { center on pixel }
          pt.v := pt.v - 4;
        END;
      MapPt(pt,myWind.port.portRect,fatBits.bounds);
    END;
END;


{$S         }
PROCEDURE GridPoint(VAR pt: Point);
BEGIN
  IF gridOn AND toolGrid THEN
    BEGIN
      pt.h := Trunc8(pt.h + 4);
      pt.v := Trunc8(pt.v + 4);
    END;
END;


{$S         }
PROCEDURE GetGridMouse(VAR pt: Point);
BEGIN
  GetFatMouse(pt);
  GridPoint(pt);
END;


{$S         }
PROCEDURE PinGridMouse(VAR pt: Point);
BEGIN
  GetGridMouse(pt);
  PinPt(pt,myPinRect);
END;



{$S         }
PROCEDURE InitConstrain(startPt: Point);
BEGIN
  hConstrain := TRUE;
  vConstrain := TRUE;
  ptConstrain := startPt;
END;


{$S         }
PROCEDURE HVConstrain(VAR newPt: Point);
VAR dh,dv: INTEGER;
BEGIN
  IF shiftFlag THEN  { constrain to horiz or vert }
    BEGIN
      IF hConstrain AND vConstrain THEN  { still chosing direction }
        BEGIN
          dh := ABS(newPt.h-ptConstrain.h);
          dv := ABS(newPt.v-ptConstrain.v);
          IF (dh > dv) AND (dh > 1) THEN vConstrain := FALSE;
          IF (dv > dh) AND (dv > 1) THEN hConstrain := FALSE;
        END;
      IF hConstrain THEN newPt.v := ptConstrain.v;  { horiz }
      IF vConstrain THEN newPt.h := ptConstrain.h;  { vert  }
    END;
END;


{$S         }
PROCEDURE CursorNormal;
BEGIN
  InitCursor;             { arrow, not hidden }
  GridMouse(-1,-1,0,0);   { no grid  }
END;



{$S         }
PROCEDURE BusyCursor;
TYPE crsrPtr  = ^Cursor;
     crsrHndl = ^CrsrPtr;
VAR  hourGlass: crsrHndl;
BEGIN
  GridMouse(-1,-1,0,0);   { no grid  }
  cursorFlag := TRUE;
  hourGlass := Pointer(ORD(GetCursor(4)));
  SetCursor(hourGlass^^);
END;



{$S         }
PROCEDURE SetPinRect(size: INTEGER);
VAR halfSize: INTEGER;
BEGIN
  halfSize := size DIV 2;
  myPinRect := myWind.port.portRect;
  IF fatFlag THEN myPinRect := fatBits.bounds;
  WITH myPinRect DO
    BEGIN
      left := left + halfSize;
      top := top + halfSize;
      right := right - size + halfSize;
      bottom := bottom - size + halfSize;
      IF gridOn AND toolGrid THEN
        BEGIN
          left := Trunc8(left + 7);
          top  := Trunc8(top + 7);
          right := Trunc8(right);
          bottom := Trunc8(bottom);
        END;
    END;
END;


{$S         }
PROCEDURE SetLineSize(size: INTEGER);
BEGIN
  lineSize := size;
  halfLineSize := lineSize DIV 2;
  SetPinRect(size);
END;


{$S         }
PROCEDURE JamLine;
BEGIN
  PenNormal;
  PenSize(lineSize,lineSize);
  IF optionFlag THEN PenPat(thePat);
  IF featureFlag THEN PenMode(patOr);
END;


{$S         }
PROCEDURE JamFill;
BEGIN
  PenNormal;
  PenPat(thePat);
  IF featureFlag THEN PenMode(patOr);
END;


{$S         }
PROCEDURE SetSelRect(r: Rect; newPivot: BOOLEAN);
VAR center: Point;
    dstRect: Rect;
    bool:   BOOLEAN;
BEGIN
  dstRect := myWind.port.portRect;
  IF fatFlag THEN dstRect := fatBits.bounds;
  bool := SectRect(r,dstRect,selRect);
  center.h := (selRect.left + selRect.right) DIV 2;
  center.v := (selRect.top + selRect.bottom) DIV 2;
  IF newPivot THEN pivot := center;
  IF NOT fatFlag THEN fatCenter := center;
END;


{$S         }
PROCEDURE BandToScrn(srcBits: BitMap; top,bottom: INTEGER);
{ top and bottom are in local coords of document }
VAR shieldRect: Rect;
BEGIN
  IF NOT windOpen THEN EXIT(BandToScrn);

  IF fatFlag THEN
    BEGIN
      CopyBits(srcBits,fatBits,fatBits.bounds,fatBits.bounds,srcCopy,Nil);
      FatToScrn(@fatBuf,scrnPtr);
    END
  ELSE
    BEGIN
      shieldRect := myWind.port.portRect;
      shieldRect.top := top;
      shieldRect.bottom := bottom;

      { calc buffer relative coords for BufToScrn }
      top := top - myWind.port.portRect.top;
      bottom := bottom - myWind.port.portRect.top;
      IF top < 0 THEN top := 0;
      IF bottom > 240 THEN bottom := 240;

      ShieldCursor(shieldRect,myWind.port.portBits.bounds.topLeft);
      BufToScrn(Pointer(ORD(srcBits.baseAddr)),scrnPtr,top,bottom);
      ShowCursor;
    END;
END;



{$S         }
PROCEDURE BitsToScrn(srcBits: BitMap);
BEGIN
  BandToScrn(srcBits, myWind.port.portRect.top,
                      myWind.port.portRect.bottom);
END;


{$S         }
PROCEDURE AltToScrn;
VAR savePort: GrafPtr;
BEGIN
  GetPort(savePort);
  SetPort(@myWind);
  SetPortBits(docBits);
  BitsToScrn(altBits);
  SetPort(savePort);
END;


{$S         }
PROCEDURE ScrnToAlt;
BEGIN
  IF fatFlag
  THEN CopyBits(fatBits,altBits,fatBits.bounds,fatBits.bounds,srcCopy,Nil)
  ELSE ScrnToBuf(scrnPtr,altBits.baseAddr);
END;


{$S         }
PROCEDURE MaskToAlt(srcRect,dstRect: Rect; mode: INTEGER);
{ share common code }
BEGIN
  maskBits.baseAddr := Pointer(ORD(maskHandle^));
  CopyBits(maskBits,altBits,srcRect,dstRect,mode,Nil);
END;


{$S         }
PROCEDURE MainToAlt;
VAR saveBits: BitMap;
BEGIN
  BufToBuf(@mainBuf,@altBuf);
  scrnFlag := FALSE;
  IF whiteFlag THEN
    BEGIN
      IF selFlag THEN
        BEGIN
          saveBits := thePort^.portBits;
          SetPortBits(altBits);
          EraseRect(oldSelRect);
          SetPortBits(saveBits);
        END;
      IF maskFlag THEN MaskToAlt(selRect,oldSelRect,srcBic);
    END;
END;


{$S        }
PROCEDURE InvertCaret;
VAR savePort: GrafPtr;
BEGIN
  GetPort(savePort);
  SetPort(@myWind);
  IF fatFlag THEN SetPortBits(fatBits);
  PenNormal;
  PenMode(patXor);
  MoveTo(caretLoc.h,caretLoc.v-info.ascent);
  LineTo(caretLoc.h,caretLoc.v+info.descent-1);
  IF fatFlag THEN FatToScrn(@fatBuf,scrnPtr);
  SetPortBits(docBits);
  SetPort(savePort);
  caretState := NOT caretState;
END;



{$S         }
PROCEDURE AcceptEdits;
BEGIN
  BufToBuf(@altBuf,@mainBuf);
  oldAcceptFlag := acceptFlag;
  oldWhiteFlag  := whiteFlag;
  oldMaskFlag   := maskFlag;
  oldSelFlag    := selFlag;
  oldSelRect    := selRect;
  oldPivot      := pivot;
  oldLassoBlack := lassoBlack;
  acceptFlag    := FALSE;
END;


{$S         }
PROCEDURE ScrollMask(dh,dv: INTEGER);
BEGIN
  HLock(maskHandle);
  maskBits.baseAddr := Pointer(ORD(maskHandle^));
  SetPortBits(maskBits);
  ScrollRect(maskBits.bounds,dh,dv,myWind.port.clipRgn);
  ClipRect(pageRect);
  SetPortBits(docBits);
  HUnlock(maskHandle);
END;



{$S         }
FUNCTION AllocMask: BOOLEAN;
BEGIN
  maskHandle := NewHandle(12480);
  IF maskHandle = Nil THEN
    BEGIN
      AllocMask := FALSE;
      heapJam := TRUE;
    END
  ELSE
    BEGIN
      AllocMask := TRUE;
      hLock(maskHandle);
      maskBits := altBits;
      maskBits.baseAddr := Pointer(ORD(maskHandle^));
    END;
END;



{$S         }
PROCEDURE KillMask;
BEGIN
  IF maskHandle <> Nil THEN
    BEGIN
      DisposHandle(maskHandle);
      maskHandle := Nil;
    END;
  maskFlag := FALSE;
  oldMaskFlag := FALSE;  { can't undo it }
END;


{$S         }
PROCEDURE KillStuff;
VAR line:  INTEGER;
BEGIN
  IF txScrapHndl <> Nil THEN
    BEGIN
      DisposHandle(txScrapHndl);
      txScrapHndl := Nil;
      txScrapSize := 0;
    END;

  FOR line := 1 TO textLines DO
    DisposHandle(Pointer(ORD(textHandles[line])));
  textLines := 0;
  textFlag := FALSE;

  KillMask;
  selFlag := FALSE;
  AltToScrn;           { hide selection }
END;


{$S         }
PROCEDURE Undo;
BEGIN
  IF textFlag OR (txScrapHndl <> Nil) THEN KillStuff;
  SwapBuf(@mainBuf,@altBuf);
  SwapBool(acceptFlag,oldAcceptFlag);
  SwapBool(whiteFlag,oldWhiteFlag);
  SwapBool(maskFlag,oldMaskFlag);
  SwapBool(lassoBlack,oldLassoBlack);
  SwapBool(selFlag,oldSelFlag);
  SwapRect(selRect,oldSelRect);
  SwapPt(pivot,oldPivot);
  IF maskFlag OR oldMaskFlag THEN
    BEGIN
      IF NOT EqualRect(selRect,oldSelRect)
      THEN ScrollMask(selRect.left-oldSelRect.left,selRect.top-oldSelRect.top);
    END;
  AltToScrn;
END;



{$S         }
PROCEDURE Constrain(startPt: Point; VAR newPt: Point; hvOk: BOOLEAN);
VAR dh,dv,absDh,absDv,delta: INTEGER;
    oldPt: Point;
BEGIN
  oldPt := newPt;

  dh := newPt.h - startPt.h;
  dv := newPt.v - startPt.v;
  absDh := ABS(dh);
  absDv := ABS(dv);
  delta := Min(absDh,absDv);
  IF dh < 0 THEN dh := -delta ELSE dh := delta;
  IF dv < 0 THEN dv := -delta ELSE dv := delta;
  newPt.h := startPt.h + dh;
  newPt.v := startPt.v + dv;

  IF hvOK THEN
    BEGIN
      IF absDh > 2*absDv THEN
        BEGIN
          newPt.h := oldPt.h;
          newPt.v := startPt.v;      { horizontal }
        END;
      IF absDv > 2*absDh THEN
        BEGIN
          newPt.h := startPt.h;      { vertical   }
          newPt.v := oldPt.v;
        END;
    END;
END;



{$S         }
PROCEDURE CreateShape(shape: ShapeCode; filled: BOOLEAN);
VAR startPt,newPt,oldPt: Point;
    dstRect:    Rect;
BEGIN
  JamLine;
  PinGridMouse(startPt);
  dstRect.topLeft := startPt;
  dstRect.botRight := startPt;
  oldPt.h := 1000;   { fake out first time }
  WHILE StillDown DO
    BEGIN
      PinGridMouse(newPt);
      IF shiftFlag THEN Constrain(startPt,newPt,FALSE);  { drag square }
      IF NOT EqualPt(newPt,oldPt) THEN
        BEGIN
          MainToAlt;
          Pt2Rect(startPt,newPt,dstRect);
          WITH dstRect DO
            BEGIN
              left := left - halfLineSize;
              top  := top - halfLineSize;
              right := right + lineSize - halfLineSize;
              bottom := bottom + lineSize - halfLineSize;
            END;
          SetPortBits(altBits);

          IF filled THEN
            BEGIN
              JamFill;
              CASE shape OF
                rectShape:  PaintRect(dstRect);
                rRectShape: PaintRoundRect(dstRect,corner,corner);
                ovalShape:  PaintOval(dstRect);
              END;
            END;

          IF borderFlag OR NOT filled THEN
            BEGIN
              JamLine;
              CASE shape OF
                rectShape:  FrameRect(dstRect);
                rRectShape: FrameRoundRect(dstRect,corner,corner);
                ovalShape:  FrameOval(dstRect);
              END;
            END;

          AltToScrn;  { returns to docBits }
          oldPt:=newPt;
        END;
    END;
END;



{$S SegPrint }
FUNCTION PrintRect(srcBits: BitMap; srcRect: Rect): BOOLEAN;
VAR event:              EventRecord;
    saveOrigin:         Point;
    theControl:         ControlHandle;
BEGIN
  PrintRect := TRUE;
  PrCtlCall(iPrBitsCtl,ORD(@srcBits),ORD(@srcRect),lPaintBits+2*ORD(hiResFlag));

  { check for cancel button }
  IF GetNextEvent(everyEvent,event) AND (event.what = mouseDown) THEN
    BEGIN
      saveOrigin := myWind.port.portRect.topLeft;
      SetOrigin(0,0);
      GlobalToLocal(event.where);
      IF FindControl(event.where,@myWind,theControl) = 0 THEN SystemBeep(8)
      ELSE IF TrackControl(theControl,event.where,Nil) <> 0 THEN
        BEGIN
          PrintRect := FALSE;
          SetOrigin(saveOrigin.h,saveOrigin.v);
          EXIT(PrintRect);
        END;
      SetOrigin(saveOrigin.h,saveOrigin.v);
    END;
END;



{$S        }
PROCEDURE PatchText(count: INTEGER; textPtr: QDPtr; numer,denom: Point);
{ draw text in chunks to cut stack needs, preventing croak }
VAR myCount: INTEGER;
BEGIN
  WHILE count > 0 DO
    BEGIN
      myCount := Min(count,txMaxCount);
      StdText(myCount,textPtr,numer,denom);
      textPtr := Pointer(ORD(textPtr) + myCount);
      count := count - myCount;
    END;
END;



{$S        }
PROCEDURE DrawTxScrap(dstRect: Rect);
VAR i:          INTEGER;
    myProcs:    QDProcs;
BEGIN
  KillMask;   { not enough room in heap for mask and font }
  ClipRect(dstRect);
  EraseRect(dstRect);
  InsetRect(dstRect,2,0);
  dstRect.right := Max(dstRect.right,dstRect.left + txMinWidth);
  dstRect.bottom := Max(dstRect.bottom,dstRect.top + txMinHeight);
  SetStdProcs(myProcs);
  thePort^.grafProcs := @myProcs;
  myProcs.textProc := @PatchText;
  i := CharWidth('A');  { swap in font }
  HLock(txScrapHndl);
  TextBox(txScrapHndl^,txScrapSize,dstRect,textJust);
  HUnlock(txScrapHndl);
  ClipRect(pageRect);
  thePort^.grafProcs := Nil;   { restore to normal }
END;


{$S        }
PROCEDURE UpdateText;
LABEL 9;
VAR line,horiz,vert,width,lineHeight: INTEGER;
    h:          StringHandle;
    dstRect:    Rect;
    savePort:   GrafPtr;
    i:          INTEGER;
    myProcs:    QDProcs;

BEGIN
  GetPort(savePort);
  SetPort(@myWind);

  IF txScrapHndl <> Nil THEN  { update text scrap }
    BEGIN
      SetPortBits(altBits);
      DrawTxScrap(selRect);
      AltToScrn;
      GOTO 9;
    END;

  IF (NOT textFlag) OR (textLines < 1) THEN GOTO 9;

  KillMask;   { not enough room in heap for mask and font }
  SetPortBits(altBits);
  MainToAlt;
  lineHeight := info.ascent + info.descent + info.leading;
  IF gridOn THEN lineHeight := Trunc8(lineHeight + 7);

  SetStdProcs(myProcs);
  thePort^.grafProcs := @myProcs;
  myProcs.textProc := @PatchText;

  vert := textLoc.v;
  FOR line := 1 TO textLines DO
    BEGIN
      horiz := textLeft;
      IF line = 1 THEN horiz := textLoc.h;
      h := textHandles[line];
      IF Length(h^^) > 0 THEN
        BEGIN
          i := CharWidth('A');  { swap in font }
          HLock(Pointer(ORD(h)));
          width := StringWidth(h^^);
          IF BitTst(@thePort^.txFace,6) THEN width := width + 5;
          { IF italic IN thePort^.txFace THEN width := width + 5;  }

          IF textJust = teFillRight THEN horiz := horiz - width;
          IF textJust = teFillCenter THEN horiz := horiz - (width DIV 2);
          MoveTo(horiz,vert);
          WITH dstRect DO
            BEGIN
              left := horiz - 1;
              right := horiz + width + 1;
              top := vert - info.ascent;
              bottom := vert + info.descent;
              horiz := right - 1;
            END;
          EraseRect(dstRect);
          DrawString(h^^);
          HUnlock(Pointer(ORD(h)));
        END;
      vert := vert + lineHeight;
    END;

  thePort^.grafProcs := Nil;   { restore to normal }
  caretLoc.h := horiz;
  caretLoc.v := vert - lineHeight;

  AltToScrn;

9: SetPort(savePort);
   nextCaretTime := 0;  { make caret show }
END;


{$S SegUpdate }
PROCEDURE DrawPat;
VAR i,h: INTEGER;
    tempRect:  Rect;
    savePort: GrafPtr;
BEGIN
  GetPort(savePort);
  SetPort(@deskWind);
  OffsetRect(patRect,1,1);
  PaintRect(patRect);
  OffsetRect(patRect,-1,-1);
  EraseRect(patRect);
  FrameRect(patRect);

  thePat := patterns[thePatIndex];
  SetRect(fillSample,patLeft+8,patTop+6,patStart-7,patBottom-6);
  FrameRect(fillSample);
  InsetRect(fillSample,1,1);
  FillRect(fillSample,thePat);

  SetRect(tempRect,patStart-1,patTop+1,patRight-1,patBottom-1);
  PaintRect(tempRect);

  h := patStart;
  FOR i := 0 TO patRow-1 DO
    BEGIN
      tempRect.left := h;
      h := h + patSpace;
      tempRect.right := h - 1;
      tempRect.top := patTop+1;
      tempRect.bottom := patMid;
      FillRect(tempRect,patterns[i]);
      tempRect.top := patMid + 1;
      tempRect.bottom := patBottom-1;
      FillRect(tempRect,patterns[i+patRow]);
    END;
  SetPort(savePort);
END;



{$S         }
FUNCTION MyAlert(whichAlert: INTEGER): INTEGER;
BEGIN
  CursorNormal;  { arrow, no grid }
  MyAlert := Alert(whichAlert,Nil);
  cursorFlag := TRUE;
END;


{$S         }
PROCEDURE BorrowAlt;
BEGIN
  ScrnToBuf(scrnPtr,@altBuf);           { stash screen image in alt }
  scrnFlag := TRUE;                     { remember what's in alt    }
END;


PROCEDURE UpdateWindow(whichWindow: WindowPtr; okToSmash: BOOLEAN);  FORWARD;

FUNCTION SaveAlert(whichAlert: INTEGER): INTEGER;
BEGIN
  BorrowAlt;                            { save screen image for refresh }
  SaveAlert := MyAlert(whichAlert);
  UpdateWindow(@myWind,FALSE);
  MainToAlt;                            { get rid of saved screen image }
END;


{$S         }
FUNCTION WriteError(result: INTEGER; discard: Handle): BOOLEAN;
VAR aResult,whichAlert: INTEGER;
BEGIN
  WriteError := FALSE;
  IF (result <> 0) AND (result <> fileExists) THEN
    BEGIN
      whichAlert := wErrAlrt;
      IF result = -34 THEN whichAlert := wFullAlrt;
      IF result = -44 THEN whichAlert := wProtAlrt;
      IF result = -61 THEN whichAlert := fileLockAlrt;
      IF discard <> Nil THEN DisposHandle(discard);  { make room for alert }
      aResult := MyAlert(whichAlert);
      WriteError := TRUE;
    END;
END;


{$S         }
FUNCTION ReadError(result: INTEGER; discard: Handle): BOOLEAN;
VAR aResult: INTEGER;
BEGIN
  ReadError := FALSE;
  IF (result <> 0) AND (result <> endOfFile) THEN
    BEGIN
      { alert trouble reading }
      IF discard <> Nil THEN DisposHandle(discard);   { make room for alert }
      aResult := MyAlert(rErrAlrt);
      ReadError := TRUE;
    END;
END;



{$S         }
PROCEDURE ReadWrite(srcFile,dstFile: INTEGER;
                    fromMain,toAlt,toShrink,toPrint,newPat: BOOLEAN);

{ read and unpack from srcFile, operate on, then pack and write to dstFile }
{ srcFile and dstFile should already be opened }
{ IF dstFile = 0, then don't write to dstFile }
{ IF newPat, then read patterns from srcFile }
{ returns rwResult = 0 for SUCCESS }

LABEL 9;

CONST dstBlocks = 6;     { how many blocks in dstbuf }
      bandSize  = 3584;  { docRow * bandHeight + 128 }
      heapSlop  = 1024;

TYPE  Bytes72 = PACKED ARRAY[1..docRow] OF QDByte;

VAR dstBuf:     ARRAY[1..dstBlocks] OF DiskBlock;
    bandSlop:   PACKED ARRAY[1..128] OF QDByte;    { must follow bandbuf ! }
    bandBuf:    ARRAY[0..50] OF Bytes72;
    bandBits:              BitMap;
    result,band,scanLine:  INTEGER;
    srcByte,dstByte:       LongInt;
    dstDrive:              INTEGER;
    srcPtr,dstPtr,bandPtr: QDPtr;
    mainRect,altRect:      Rect;
    hitMain,hitAlt:        BOOLEAN;
    shrinkRect:            Rect;
    vert:                  INTEGER;
    grow:                  LongInt;
    srcSize:               LongInt;
    srcHandle:             Handle;
    srcFirst,srcLast:      ^DiskBlock;
    srcRect,dstRect,erasRect: Rect;
    srcScan,srcGoal:       INTEGER;
    byteCount:             LongInt;
    saveOrigin:            Point;
    oldPtr:                QDPtr;
    badDoc:                BOOLEAN;

    hiResBits:             BitMap;
    hiResHndl:             Handle;
    hiResPtr:              QDPtr;
    hiResBand:             INTEGER;



   FUNCTION GetScan: BOOLEAN;
   { unpack the next scanline, checking for bad data }
   BEGIN
     GetScan := TRUE;
     IF (srcGoal < 0) OR (srcGoal > 719) THEN   { blank line }
       BEGIN
         ZeroMem(bandPtr,docRow);
         bandPtr := Pointer(ORD(bandPtr)+docRow);
       END
     ELSE WHILE srcScan <= srcGoal DO
       BEGIN
         IF NOT badDoc THEN
           BEGIN
             oldPtr := bandPtr;
             UnpackBits(srcPtr,bandPtr,docRow);
             IF ORD(bandPtr) - ORD(oldPtr) <> docRow THEN
               BEGIN
                 SystemBeep(8);
                 badDoc := TRUE;
                 bandPtr := oldPtr;
               END;
           END;

         IF badDoc THEN
           BEGIN
             ZeroMem(bandPtr,docRow);
             bandPtr := Pointer(ORD(bandPtr)+docRow);
           END;

         IF srcScan < srcGoal THEN bandPtr := @bandBuf[1];
         srcScan := srcScan + 1;
         IF ORD(srcPtr) >= ORD(srcLast) THEN
           BEGIN
             srcFirst^ := srcLast^;
             result := ReadData(srcFile,srcByte,srcSize-512,
                       Pointer(ORD(srcFirst)+512));
             IF ReadError(result,srcHandle) THEN
               BEGIN
                 badDoc := TRUE;
                 GetScan := FALSE;
               END;
             srcPtr := Pointer(ORD(srcPtr) - srcSize + 512);
           END;
       END;
     srcGoal := srcGoal + 1;
   END;



BEGIN
  rwResult := -1;    { assume failure }
  BusyCursor;
  SetPort(@myWind);
  okPrint := toPrint;
  IF toAlt THEN ScrnToAlt;

  IF toShrink THEN
    BEGIN
      FillRect(myWind.port.portRect,ltGray);
      shrinkRect := myWind.port.portRect;
      shrinkRect.left := shrinkRect.left + 208 - 96;
      shrinkRect.right := shrinkRect.left + 192;
      PenNormal;
      EraseRect(shrinkRect);
      InsetRect(shrinkRect,-1,-1);
      FrameRect(shrinkRect);
      InsetRect(shrinkRect,1,1);
      shrinkRect.bottom := shrinkRect.top + bandHeight DIV 3;
    END;

  IF toPrint THEN
    BEGIN
      { draw cancel button }
      saveOrigin := myWind.port.portRect.topLeft;
      SetOrigin(0,0);
      ShowControl(cancelButton);
      SetOrigin(saveOrigin.h,saveOrigin.v);

      IF hiResFlag THEN  { allocate hiResBuf and init hiResBits }
        BEGIN
          hiResHndl := NewHandle(3024);  { 21 scanlines of 144 bytes each }
          WITH hiResBits DO
            BEGIN
              rowBytes := 144;
              bounds.left := 0;
              bounds.top := 0;
              bounds.right := 1152;
              bounds.bottom := 16;
            END;
        END;
    END;

  WITH bandBits DO
    BEGIN
      baseAddr := @bandBuf[1];
      rowBytes := docRow;
      bounds.left := 0;
      bounds.top := 0;
      bounds.right := docWidth;
      bounds.bottom := bandHeight;
    END;

  dstByte := 0;
  srcByte := 0;

  result  := ReadData(srcFile,srcByte,512,@dstBuf);       { read header }
  IF ReadError(result,Nil) THEN GOTO 9;

  IF dstBuf[1,1] < 2 THEN  { old format, has no patterns }
    BEGIN
      newPat := FALSE;          { will install patterns below }
      dstBuf[1,1] := 2;         { new format }
    END;

  IF newPat THEN IF NOT EqualLongs(@dstBuf[1,2],@patterns,38*2) THEN
    BEGIN
      CopyLongs(@dstBuf[1,2],@patterns,38*2);
      DrawPat;
    END;

  CopyLongs(@patterns,@dstBuf[1,2],38*2);

  srcSize := BitAnd(MaxMem(grow)-heapSlop,$FFFFFE00);   { trunc to mult of 512 }
  srcHandle := NewHandle(srcSize);
  HLock(srcHandle);
  srcFirst := Pointer(ORD(srcHandle^));
  srcLast := Pointer(ORD(srcFirst) + srcSize - 512);

  { prime srcBuf }
  result := ReadData(srcFile,srcByte,srcSize,Pointer(ORD(srcFirst)));
  IF ReadError(result,srcHandle) THEN GOTO 9;

  srcPtr  := Pointer(ORD(srcFirst));
  dstPtr  := @dstBuf[2];  { put packed bits after header }
  srcScan := 0;
  srcGoal := -rwShftDv;

  ZeroMem(@bandBuf[0],docRow);    { clear first fake scan line }
  bandPtr := @bandBuf[1];
  badDoc := FALSE;
  IF NOT GetScan THEN GOTO 9; { get 1st real scanline into bandBuf[1] }
  IF NOT GetScan THEN GOTO 9; { get 2nd real scanline into bandBuf[2] }

  FOR band := 1 to bandCount DO
    BEGIN
      bandPtr := @bandBuf[3];
      { read and unpack into bandBuf[3] .. bandBuf[50] }
      FOR scanLine := 1 TO 48 DO IF NOT GetScan THEN GOTO 9;

      { now operate on unpacked band buffer }

      hitMain := SectRect(mainBits.bounds,bandBits.bounds,mainRect);
      IF fromMain AND hitMain THEN  { merge new data from mainBits }
        BEGIN
          CopyBits(mainBits,bandBits,mainRect,mainRect,srcCopy,Nil);
        END;

      IF rwShftDh <> 0 THEN
        BEGIN
          srcRect := bandBits.bounds;
          dstRect := bandBits.bounds;
          erasRect := bandBits.bounds;
          IF rwShftDh > 0 THEN
            BEGIN
              srcRect.right := srcRect.right - rwShftDh;
              dstRect.left := dstRect.left + rwShftDh;
              erasRect.right := erasRect.left + rwShftDh;
            END
          ELSE
            BEGIN
              srcRect.left := srcRect.left - rwShftDh;
              dstRect.right := dstRect.right + rwShftDh;
              erasRect.left := erasRect.right + rwShftDh;
            END;
          CopyBits(bandBits,bandBits,srcRect,dstRect,srcCopy,Nil);
          CopyBits(bandBits,bandBits,erasRect,erasRect,srcXor,Nil);  { erase }
        END;


      IF toShrink THEN  { draw shrunken view }
        BEGIN
          CopyBits(bandBits,docBits,bandBits.bounds,shrinkRect,srcCopy,Nil);
          OffsetRect(shrinkRect,0,bandHeight DIV 3);
        END;

      IF okPrint THEN
        BEGIN
          IF hiResFlag THEN
            BEGIN
              HLock(hiResHndl);
              hiResPtr := Pointer(ORD(hiResHndl^));
              hiResBits.baseAddr := Pointer(ORD(hiResPtr)+288);
              bandPtr := @bandBuf;
              FOR hiResBand := 1 TO 6 DO IF okPrint THEN
                BEGIN
                  Stretch2x(bandPtr,hiResPtr);
                  okPrint := PrintRect(hiResBits,hiResBits.bounds);
                  bandPtr := Pointer(ORD(bandPtr)+576);
                END;
            END
          ELSE  { print loRes }
            BEGIN
              okPrint := PrintRect(bandBits,bandBits.bounds);
            END;
        END;

      hitAlt := SectRect(altBits.bounds,bandBits.bounds,altRect);
      IF (toAlt AND hitAlt) THEN
        BEGIN
          CopyBits(bandBits,altBits,altRect,altRect,srcCopy,Nil);
          BandToScrn(altBits,altRect.top,altRect.bottom);
        END;

      OffsetRect(bandBits.bounds,0,bandHeight);

      bandPtr := @bandBuf[1];
      IF dstFile <> 0 THEN FOR scanLine := 1 TO bandHeight DO
        BEGIN
          PackBits(bandPtr,dstPtr,docRow);
          IF ORD(dstPtr) >= ORD(@dstBuf) + SizeOf(dstBuf)-512 THEN
            BEGIN
              result  := WriteData(dstFile,dstByte,SizeOf(dstBuf)-512,@dstBuf);
              IF WriteError(result,srcHandle) THEN GOTO 9;
              dstBuf[1] := dstBuf[dstBlocks];
              dstPtr  := Pointer(ORD(dstPtr) - SizeOf(dstBuf) + 512);
            END;
        END;

      CopyLongs(@bandBuf[48],@bandBuf[0],54);   { move up last 3 scans }
    END;     { for band }


  IF dstFile <> 0 THEN
    BEGIN
      { flush partial dstBuf, round up to full block }
      byteCount := BitAnd(ORD(dstPtr) - ORD(@dstBuf) + 511,$FFFFFE00);
      result := WriteData(dstFile,dstByte,byteCount,@dstBuf);
      IF WriteError(result,srcHandle) THEN GOTO 9;
      dstDrive := workDrive;
      IF dstFile <> workTo THEN
        BEGIN
          dstDrive := docDrive;
          result := SetEOF(dstFile,dstByte);
        END;
      FlushVol(dstDrive);
    END;

  rwResult := 0;    { success }
  DisposHandle(srcHandle);

9: IF dstFile = workTo THEN workSize := dstByte;
   IF toPrint THEN  { 'Hide' button without leaving white }
     BEGIN
       IF hiResFlag THEN DisposHandle(hiResHndl);
       HidePen;
       HideControl(cancelButton);
       ShowPen;
     END;
END;



{$S         }
PROCEDURE ShowEdges;
VAR tempRect: Rect;
BEGIN
  IF NOT edgeFlag THEN EXIT(ShowEdges);   { edges aren't in altBits }

  IF fatFlag THEN
    BEGIN
      SetPortBits(fatBits);
      PenMode(patXor);
      GetSelPat(selIndex,myWind.port.pnPat);
      PaintRect(selRect);
      CopyBits(altBits,myWind.port.portBits,selRect,selRect,srcBic,Nil);
      PaintRect(selRect);
      SetPortBits(docBits);
      FatToScrn(@fatBuf,scrnPtr);
    END
  ELSE
    BEGIN
      tempRect := selRect;
      OffsetRect(tempRect,-altBits.bounds.left,-altBits.bounds.top);
      AntsToScrn(@altBuf,scrnPtr,tempRect,selIndex);
    END
END;



{$S SegScrap }
FUNCTION WScrapError(result: INTEGER): BOOLEAN;
BEGIN
  WScrapError := FALSE;
  IF result <> 0 THEN
    BEGIN
      { alert trouble writing scrap }
      result := MyAlert(wScrapAlrt);
      WScrapError := TRUE;
    END;
END;


{$S SegScrap }
PROCEDURE MyPutPic(dataPtr: QDPtr; byteCount: INTEGER);
VAR result: INTEGER;
    reply: BOOLEAN;
BEGIN
  result := WriteData(scrapFile,scrapByte,byteCount,dataPtr);
  reply := WScrapError(result);
END;



{$S SegScrap }
PROCEDURE CutOrCopy(cutFlag: BOOLEAN);
LABEL 9;
VAR result:    INTEGER;
    myProcs:   QDProcs;
    dummyPic:  PicHandle;
    scrapType: ResType;
    scrapWord: INTEGER;
    scrapLong: LongInt;
    scrapRect: Rect;
    scrapPtr:  pScrapStuff;
    srcRect:   Rect;
    maxHeight: INTEGER;

BEGIN
  IF NOT (selFlag OR maskFlag) THEN EXIT(CutOrCopy);
  IF ZeroScrap <> 0 THEN EXIT(CutOrCopy);
  IF UnloadScrap <> 0 THEN EXIT(CutOrCopy);
  IF acceptFlag THEN AcceptEdits;

  scrapPtr := InfoScrap;
  result := OpenFile(0,0,scrapPtr^.scrapName^,Nil,scrapFile);
  scrapRect := selRect;
  scrapByte := 0;
  scrapType := 'PICT';
  result := WriteData(scrapFile,scrapByte,4,@scrapType);
  IF WScrapError(result) THEN GOTO 9;

  { put long of object Length=zero, and word of picSize=zero }
  result := WriteData(scrapFile,scrapByte,6,@zeroRect);
  IF WScrapError(result) THEN GOTO 9;

  { put PicFrame }
  result := WriteData(scrapFile,scrapByte,8,@scrapRect);
  IF WScrapError(result) THEN GOTO 9;

  { and maskBits with altBits }
  IF maskFlag THEN MaskToAlt(selRect,selRect,notSrcBic);

  SetPort(@myWind);
  SetStdProcs(myProcs);
  myProcs.putPicProc := @MyPutPic;
  thePort^.grafProcs := @myProcs;
  dummyPic := OpenPicture(scrapRect);
  IF maskFlag THEN PicComment(12345,0,Nil);
  ClipRect(pageRect);


  { worst case bitmap of 3584 bytes, 4 bands }
  maxHeight := (selRect.right - selRect.left + 32) DIV 7;
  srcRect := selRect;
  WHILE NOT EmptyRect(srcRect) DO
    BEGIN
      srcRect.bottom := Min(srcRect.bottom, srcRect.top + maxHeight);
      CopyBits(altBits,thePort^.portBits,srcRect,srcRect,srcCopy,Nil);
      srcRect.top := srcRect.bottom;
      srcRect.bottom := selRect.bottom;
    END;

  ClosePicture;
  KillPicture(dummyPic);
  thePort^.grafProcs := Nil;   { restore to normal }

  IF maskFlag THEN  { restore clobbered altBits }
    BEGIN
      IF fatFlag THEN MainToAlt;
      ScrnToAlt;
    END;

  result := SetEOF(scrapFile,scrapByte);

  scrapPtr^.scrapSize := scrapByte;

  scrapLong := scrapByte - 8;
  scrapWord := scrapLong;
  scrapByte := 4;

  { backpatch scrap object length }
  result := WriteData(scrapFile,scrapByte,4,@scrapLong);
  IF WScrapError(result) THEN GOTO 9;

  { backpatch picSize word }
  result := WriteData(scrapFile,scrapByte,2,@scrapWord);
  IF WScrapError(result) THEN GOTO 9;

9: result := CloseFile(scrapFile);

  FlushVol(0);

  IF cutFlag THEN
    BEGIN
      MainToAlt;
      AltToScrn;
      selFlag := FALSE;
      maskFlag := FALSE;
    END;
END;



{$S SegPaste }
FUNCTION RScrapError(result: INTEGER): BOOLEAN;
BEGIN
  RScrapError := FALSE;
  IF (result <> 0) AND (result <> -49) THEN
    BEGIN
      { alert trouble reading scrap }
      result := MyAlert(rScrapAlrt);
      RScrapError := TRUE;
    END;
END;


{$S SegPaste }
PROCEDURE MyGetPic(dataPtr: QDPtr; byteCount: INTEGER);
VAR result: INTEGER;
    reply: BOOLEAN;
BEGIN
  result := ReadData(scrapFile,scrapByte,byteCount,dataPtr);
  reply := RScrapError(result);
END;


{$S SegPaste }
PROCEDURE MyPicComment(kind,dataSize: INTEGER; dataHandle: QDHandle);
BEGIN
  IF kind = 12345 THEN
    BEGIN
      maskPaste := TRUE;
      ZeroBuf(@altBuf);
    END;
END;


{$S SegPaste }
PROCEDURE CenterScrap(VAR r: Rect);
{ Center scrap rectangle in current window.     }
{ Exit fatBits if rect is too wide or too tall. }
VAR width,height: INTEGER;
    windRect:   Rect;
    saveGrid:   BOOLEAN;
BEGIN
  saveGrid := toolGrid;
  toolGrid := TRUE;
  width := r.right - r.left;
  height := r.bottom - r.top;

  IF (width > 48) OR (height > 30) THEN  { won't fit in fatBits }
    BEGIN
      fatFlag := FALSE;
      CheckItem(myMenus[aidsMenu],fatItem,fatFlag);
    END;

  windRect := altBits.bounds;
  IF fatFlag THEN windRect := fatBits.bounds;
  WITH r DO
    BEGIN
      left := (windRect.left + windRect.right - width) DIV 2;
      top  := (windRect.top + windRect.bottom - height) DIV 2;
      IF NOT fatFlag THEN GridPoint(topLeft);  { dont let fat get off screen }
      right := left + width;
      bottom := top + height;
    END;

  toolGrid := saveGrid;
END;


PROCEDURE InvTool;                              FORWARD;
PROCEDURE SetToolCursor;                        FORWARD;
PROCEDURE CalcMask(srcBits,dstBits: BitMap;
                   limitRect: Rect;
                   firstPt: Point;
                   firstBlack: BOOLEAN;
                   invertDst: BOOLEAN);         FORWARD;



{$S         }
FUNCTION HalfHeap: LongInt;
CONST rgnSlop = 4840;        { 240 * 16 for last line, plus 1000 slop }
VAR grow: LongInt;
BEGIN
  HalfHeap := BitShift((MaxMem(grow)-rgnSlop),-1);
END;


{$S SegPaste }
PROCEDURE Paste;
LABEL 2,5;
VAR result:             INTEGER;
    srcRect:            Rect;
    dstRect:            Rect;
    grow:               LongInt;
    scrapHandle:        Handle;
    scrapPic:           PicHandle;
    scrapSize:          LongInt;
    scrapStart:         LongInt;
    scrapFound:         BOOLEAN;
    myProcs:            QDProcs;
    dummyPic:           PicHandle;
    wasSel:             BOOLEAN;
    scrapPtr:           pScrapStuff;

BEGIN
  SetPort(@myWind);
  wasSel := selFlag;
  selFlag := FALSE;
  AcceptEdits;
  KillStuff;  { get rid of text, sel, mask }
  maskPaste := FALSE;
  scrapFound := FALSE;
  whiteFlag := FALSE;

  scrapPtr := InfoScrap;

  { check for picture scrap }
  scrapSize := GetScrap(Nil,'PICT',scrapStart);
  IF scrapSize > 0 THEN
    BEGIN
      result := OpenFile(0,0,scrapPtr^.scrapName^,Nil,scrapFile);
      IF rScrapError(result) THEN EXIT(Paste);
      scrapByte := scrapStart + 2;
      result := ReadData(scrapFile,scrapByte,8,@srcRect);
      IF rScrapError(result) THEN GOTO 2;
      dummyPic := OpenPicture(srcRect);
      ClosePicture;
      SetStdProcs(myProcs);
      myProcs.getPicProc := @MyGetPic;
      myProcs.commentProc := @MyPicComment;
      thePort^.grafProcs := @myProcs;
      dstRect := selRect;
      IF NOT wasSel THEN
        BEGIN
          dstRect := srcRect;
          CenterScrap(dstRect);
        END;
      SetPortBits(altBits);
      EraseRect(dstRect);
      DrawPicture(dummyPic,dstRect);
      thePort^.grafProcs := Nil;     { restore to normal }
      KillPicture(dummyPic);
2:    result := CloseFile(scrapFile);
      scrapFound := TRUE;
      IF NOT maskPaste THEN GOTO 5;

      IF NOT AllocMask THEN
        BEGIN
          MainToAlt;
          EXIT(Paste);
        END;

      CalcMask(altBits,maskBits,dstRect,dstRect.topLeft,FALSE,TRUE);
      maskFlag := TRUE;
      lassoBlack := FALSE;
      oldLassoBlack := FALSE;

      { restore altBuf everywhere except for mask area }
      BufXorBuf(@mainBuf,@altBuf);
      BufAndBuf(maskBits.baseAddr,@altBuf);
      BufXorBuf(@mainBuf,@altBuf);
      HUnlock(maskHandle);
      GOTO 5;
    END;

  { no picture in scrap, check for text }
  scrapSize := GetScrap(Nil,'TEXT',scrapStart);
  IF (scrapSize > 0) AND (scrapSize < HalfHeap) THEN
    BEGIN
      txScrapHndl := NewHandle(10);
      txScrapSize := GetScrap(txScrapHndl,'TEXT',scrapStart);
      dstRect := selRect;
      IF NOT wasSel THEN
        BEGIN    { draw once just to get bounding box }
          dstRect := altBits.bounds;
          InsetRect(dstRect,50,0);
          ZeroBuf(@altBuf);
          SetPortBits(altBits);
          DrawTxScrap(dstRect);
          OffsetRect(dstRect,-altBits.bounds.left,-altBits.bounds.top);
          TrimBBox(@altBuf,dstRect);
          OffsetRect(dstRect,altBits.bounds.left,altBits.bounds.top);
          CenterScrap(dstRect);
          InsetRect(dstRect,-6,-4);
          MainToAlt;
        END;

      { now draw for real }
      SetPortBits(altBits);
      DrawTxScrap(dstRect);
      scrapFound := TRUE;
    END;


5: AltToScrn;
   IF scrapFound THEN
     BEGIN
       SetSelRect(dstRect,TRUE);
       oldSelRect := selRect;      { to avoid ScrollMask in Undo }
       SetPort(@deskWind);
       InvTool;
       IF maskPaste THEN
         BEGIN
           maskFlag := TRUE;
           theTool := lassoTool;
         END
       ELSE
         BEGIN
           selFlag := TRUE;
           theTool := selectTool;
         END;
       InvTool;
       SetToolCursor;
       toolGrid := TRUE;
     END;
END;



{$S SegFlip }
PROCEDURE TraceEdges;
VAR srcRect,dstRect: Rect;
    delta: INTEGER;
BEGIN
  IF acceptFlag THEN AcceptEdits;

  delta := 2;
  IF KeyIsDown(shiftCode) THEN delta := 3;   { asymmetric shadow }

  srcRect := selRect;
  WITH srcRect DO
    BEGIN
      left := left + 1;
      right := right - delta + 1;
      top := top + 1;
      bottom := bottom - delta + 1;
    END;

  IF NOT AllocMask THEN EXIT(TraceEdges);
  BufToBuf(@altBuf,maskBits.baseAddr);

  { smear horizontally }
  dstRect := srcRect;
  OffsetRect(dstRect,-1,0);
  CopyBits(altBits,altBits,srcRect,dstRect,srcOr,Nil);      { smear left  }
  OffsetRect(dstRect,delta,0);
  CopyBits(altBits,altBits,srcRect,dstRect,srcOr,Nil);      { smear right }

  { smear vertically }
  dstRect := srcRect;
  OffsetRect(dstRect,0,-1);
  CopyBits(altBits,altBits,srcRect,dstRect,srcOr,Nil);      { smear up   }
  OffsetRect(dstRect,0,delta);
  CopyBits(altBits,altBits,srcRect,dstRect,srcOr,Nil);      { smear down }

  { punch hole in middle }
  MaskToAlt(srcRect,srcRect,srcBic);
  KillMask;

  AltToScrn;
END;


{$S SegFlip }
PROCEDURE HFlip;
VAR srcRect: Rect;
BEGIN
  IF acceptFlag THEN AcceptEdits;
  IF NOT AllocMask THEN EXIT(HFlip);
  HFlipBuf(@altBuf,maskBits.baseAddr,
           selRect.top-altBits.bounds.top,
           selRect.bottom-altBits.bounds.top);
  srcRect := selRect;
  srcRect.left := altBits.bounds.left + altBits.bounds.right - selRect.right;
  srcRect.right := altBits.bounds.left + altBits.bounds.right - selRect.left;
  MaskToAlt(srcRect,selRect,srcCopy);
  AltToScrn;
  KillMask;
END;


{$S SegFlip }
PROCEDURE VFlip;
VAR srcRect: Rect;
BEGIN
  IF acceptFlag THEN AcceptEdits;
  IF NOT AllocMask THEN EXIT(VFlip);
  VFlipBuf(@altBuf,maskBits.baseAddr);
  srcRect := selRect;
  srcRect.top := altBits.bounds.top + altBits.bounds.bottom - selRect.bottom;
  srcRect.bottom := altBits.bounds.top + altBits.bounds.bottom - selRect.top;
  MaskToAlt(srcRect,selRect,srcCopy);
  AltToScrn;
  KillMask;
END;


{$S SegFlip }
PROCEDURE Rotate;
VAR bufRect:      Rect;
    flipRect:     Rect;
    flopRect:     Rect;
    bool:         BOOLEAN;
BEGIN
  IF acceptFlag THEN AcceptEdits;

  IF (selRect.right - selRect.left) = (selRect.bottom - selRect.top) THEN
    BEGIN  { square selection }
      flipRect := selRect;
      flopRect := flipRect;
    END
  ELSE
    BEGIN  { selection is not square }
      WITH flipRect DO
        BEGIN
          left   := pivot.h - (pivot.v - selRect.top);
          right  := pivot.h - (pivot.v - selRect.bottom);
          top    := pivot.v + (pivot.h - selRect.right);
          bottom := pivot.v + (pivot.h - selRect.left);
        END;
      bool := SectRect(flipRect,altBits.bounds,flipRect);
      WITH flopRect DO
        BEGIN
          left   := pivot.h + pivot.v - flipRect.bottom;
          right  := pivot.h + pivot.v - flipRect.top;
          top    := pivot.v - pivot.h + flipRect.left;
          bottom := pivot.v - pivot.h + flipRect.right;
        END;
    END;

  IF NOT AllocMask THEN EXIT(Rotate);
  ZeroBuf(maskBits.baseAddr);

  bufRect := flopRect;
  OffsetRect(bufRect,altBits.bounds.left - flopRect.left,
                     altBits.bounds.top - flopRect.top);
  CopyBits(altBits,altBits,flopRect,bufRect,srcCopy,Nil);

  RotBuf(@altBuf,maskBits.baseAddr,bufRect.bottom - bufRect.top);

  MainToAlt;
  bufRect := flipRect;
  OffsetRect(bufRect,altBits.bounds.left - flipRect.left,
                     altBits.bounds.bottom - flipRect.bottom);
  MaskToAlt(bufRect,flipRect,srcCopy);
  AltToScrn;
  KillMask;
  SetSelRect(flipRect,FALSE);
END;



{$S         }
PROCEDURE BrushPaint(brush: Bits16; pat: Pattern);

VAR newPt,oldPt: Point;
    exPat:      Longs24;
    dstBits:    BitMap;
    clip:       Rect;
    hMid,vMid:  INTEGER;
    dh,dv,tol:  INTEGER;


    PROCEDURE BrushAt(h,v: INTEGER);
    BEGIN
      DrawBrush(brush,exPat,h+hMid,v+vMid,dstBits,clip,featureFlag);
    END;


    PROCEDURE SymBrush(h,v: INTEGER);
    VAR symCopy: QDByte;
    BEGIN
      h := h - hMid;
      v := v - vMid;

      BrushAt(h,v);

      symCopy := symByte + symByte;

      IF fatFlag
      OR (symCopy = 0)
      OR (theTool <> brushTool)
      THEN EXIT(SymBrush);

      IF symCopy < 0 THEN BrushAt(-h,v);

      symCopy := symCopy + symCopy;
      IF symCopy < 0 THEN BrushAt(h,-v);

      symCopy := symCopy + symCopy;
      IF symCopy < 0 THEN BrushAt(-h,-v);

      symCopy := symCopy + symCopy;
      IF symCopy < 0 THEN BrushAt(v,h);

      symCopy := symCopy + symCopy;
      IF symCopy < 0 THEN BrushAt(-v,h);

      symCopy := symCopy + symCopy;
      IF symCopy < 0 THEN BrushAt(v,-h);

      symCopy := symCopy + symCopy;
      IF symCopy < 0 THEN BrushAt(-v,-h);
    END;


    PROCEDURE PaintBrush(pt: Point);
    BEGIN
      HideCursor;
      SymBrush(pt.h,pt.v);
      IF fatFlag THEN FatToScrn(@fatBuf,scrnPtr);
      ShowCursor;
    END;


    PROCEDURE BrushLine;
    VAR dh,dv,step,nSteps: INTEGER;
        horiz,vert,deltaH,deltaV: Fixed;
    BEGIN
      dh := newPt.h - oldPt.h;
      dv := newPt.v - oldPt.v;
      nSteps := Max(ABS(dh),ABS(dv));
      deltaH := FixRatio(dh,nSteps);
      deltaV := FixRatio(dv,nSteps);
      horiz  := BitShift(oldPt.h,16) + $00008000;
      vert   := BitShift(oldPt.v,16) + $00008000;
      HideCursor;
      FOR step := 1 TO nSteps DO
        BEGIN
          horiz := horiz + deltaH;
          vert  := vert  + deltaV;
          SymBrush(HiWord(horiz),HiWord(vert));
        END;
      IF fatFlag THEN FatToScrn(@fatBuf,scrnPtr);
      ShowCursor;
    END;


BEGIN
  IF theTool = eraseTool THEN featureFlag := FALSE;  { don't allow orMode }

  hMid := (myWind.port.portRect.left + myWind.port.portRect.right) DIV 2;
  vMid := (myWind.port.portRect.top + myWind.port.portRect.bottom) DIV 2;

  dstBits := docBits;
  clip := myWind.port.portRect;
  IF fatFlag THEN
    BEGIN
      dstBits := fatBits;
      clip := fatBits.bounds;
    END;

  ExpandPat(pat,exPat,dstBits.bounds.left);
  GetFatMouse(oldPt);
  PaintBrush(oldPt);
  InitConstrain(oldPt);

  tol := 1;
  IF theTool = eraseTool THEN tol := 0;
  IF theTool = sprayTool THEN tol := 2;

  REPEAT
    GetFatMouse(newPt);
    HVConstrain(newPt);

    dh := ABS(newPt.h - oldPt.h);
    dv := ABS(newPt.v - oldPt.v);
    IF dh + dv > tol THEN
      BEGIN
        IF theTool = sprayTool
        THEN PaintBrush(newPt)
        ELSE BrushLine;
        oldPt := newPt;
      END;

  UNTIL NOT StillDown;
  ScrnToAlt;
END;



{$S         }
PROCEDURE AltBufLine(pt1,pt2,oldPt: Point);
VAR lineTop,lineBot: INTEGER;
BEGIN
  lineTop := Min(oldPt.v, Min(pt1.v,pt2.v)) - halfLineSize;
  lineBot := Max(oldPt.v, Max(pt1.v,pt2.v)) - halfLineSize + lineSize;
  SetPortBits(altBits);
  MoveTo(pt1.h-halfLineSize,pt1.v-halfLineSize);
  LineTo(pt2.h-halfLineSize,pt2.v-halfLineSize);
  SetPortBits(docBits);
  BandToScrn(altBits,lineTop,lineBot);
END;


PROCEDURE ScrollDoc;                              FORWARD;
PROCEDURE MenuCommand(theMenu,theItem: INTEGER);  FORWARD;

{$S         }
PROCEDURE PencilPaint;
VAR newPt,oldPt,startPt: Point;
    saveSize:  INTEGER;
BEGIN
  IF fatScroll THEN
    BEGIN
      ScrollDoc;
      EXIT(PencilPaint);
    END;

  IF featureFlag THEN  { enter or leave fatBits }
    BEGIN
      MenuCommand(aidsMenu,fatItem);
      EXIT(PencilPaint);
    END;

  saveSize := lineSize;
  SetLineSize(1);
  PinGridMouse(startPt);
  PenNormal;
  IF PixelTrue(startPt.h,startPt.v,altBits) THEN PenPat(white);
  AltBufLine(startPt,startPt,startPt);
  InitConstrain(startPt);
  oldPt := startPt;
  REPEAT
    PinGridMouse(newPt);
    HVConstrain(newPt);
    IF NOT EqualPt(oldPt,newPt) THEN
      BEGIN
        AltBufLine(oldPt,newPt,oldPt);
        oldPt := newPt;
      END;
  UNTIL NOT StillDown;

  SetLineSize(saveSize);
END;



{$S         }
PROCEDURE EraseSome;
VAR brush: Bits16;
BEGIN
  brush := toolCursor.mask;   { all ones }
  IF fatFlag THEN  { only 2 by 2 }
    BEGIN
      ZeroMem(@brush,32);
      brush[7] := $0180;
      brush[8] := $0180;
    END;
  BrushPaint(brush,white);
END;



{$S         }
PROCEDURE CalcMask(* srcBits,dstBits: BitMap;
                     limitRect: Rect;
                     firstPt: Point;
                     firstBlack: BOOLEAN;
                     invertDst: BOOLEAN *);

{ given data in srcBits, compute mask into dstBits }
{ WARNING: srcBits and dstBits must be the wired-in size }

LABEL 9;

CONST row  = 52;         { rowbytes }
      queueSize = 400;   { how many entries big }

VAR aQueue,bQueue: ARRAY[0..queueSize] OF QueueEntry;
    readPtr:   EntryPtr;
    writePtr:  EntryPtr;
    donePtr:   EntryPtr;
    dstStart,dstLimit,srcOffset: LongInt;
    firstByte,firstMask: INTEGER;
    firstPtr: ^INTEGER;
    dh,dv:    INTEGER;
    leftByte,rightByte:     INTEGER;
    wordsWide,height: INTEGER;

BEGIN
  ZeroBuf(dstBits.baseAddr);

  IF NOT firstBlack THEN InvertBuf(srcBits.baseAddr);

  dh := -dstBits.bounds.left;
  dv := -dstBits.bounds.top;
  OffsetRect(limitRect,dh,dv);  { make global }
  firstPt.h := firstPt.h + dh;
  firstPt.v := firstPt.v + dv;

  limitRect.left := BitAnd(limitRect.left,$FFF0);       { round down to word }
  limitRect.right := BitAnd(limitRect.right+15,$FFF0);  { round up to word }

  leftByte  := limitRect.left DIV 8;
  rightByte := limitRect.right DIV 8;

  dstStart := ORD(dstBits.baseAddr) + limitRect.top * row + leftByte;
  dstLimit := ORD(dstBits.baseAddr) + limitRect.bottom * row + leftByte;
  srcOffset := ORD(srcBits.BaseAddr) - ORD(dstBits.baseAddr);

  firstByte := 2*(firstPt.h DIV 16);
  firstPtr := Pointer(ORD(srcBits.baseAddr) + firstPt.v * row + firstByte);

  firstMask := 0;
  BitSet(@firstMask,BitAnd(firstPt.h,15));
  firstMask := VertSeed(firstMask,firstPtr^);
  IF firstMask = 0 THEN GOTO 9;

  { Prime "aQueue" with seed at first point }

  writePtr := @aQueue;
  WITH writePtr^ DO
    BEGIN
      addr := ORD(firstPtr)-srcOffset;
      bump := 2;
      twoH := firstByte - leftByte;
      mask := firstMask;
    END;
  writePtr := Pointer(ORD(writePtr) + SizeOf(queueEntry));

 { Ping pong between the two Queues.  Read each entry from one queue }
 { and push all the untried ones it spawns onto the other queue.     }

  REPEAT

    donePtr := writePtr;
    readPtr := @aQueue;                         { read from queue A }
    writePtr := @bQueue;                        { push into queue B }
    MaskIt(dstStart,dstLimit,srcOffset,
           rightByte-leftByte,row,readPtr,donePtr,writePtr);

    donePtr := writePtr;
    readPtr := @bQueue;                         { read from queue B }
    writePtr := @aQueue;                        { push into queue A }
    MaskIt(dstStart,dstLimit,srcOffset,
           rightByte-leftByte,row,readPtr,donePtr,writePtr);

  UNTIL writePtr = @aQueue;                     { until aQueue is empty }

9: IF NOT firstBlack THEN InvertBuf(srcBits.baseAddr);   { restore src }

  IF invertDst THEN
    BEGIN
      height := limitRect.bottom - limitRect.top;
      wordsWide := (limitRect.right - limitRect.left) DIV 16;
      InvertChunk(dstStart,wordsWide,height);
    END;
END;


{$S         }
PROCEDURE SeedFill(startPt: Point);
VAR firstBlack: BOOLEAN;
BEGIN
  firstBlack := PixelTrue(startPt.h,startPt.v,mainBits);
  CalcMask(mainBits,altBits,altBits.bounds,startPt,firstBlack,FALSE);

  SetPortBits(altBits);
  PenPat(thePat);
  IF firstBlack THEN
    BEGIN
      PenMode(patBic);
      PaintRect(altBits.bounds);
      InvertBuf(@altBuf);
      BufAndBuf(@mainBuf,@altBuf);
    END
  ELSE
    BEGIN
      PenMode(notPatBic);
      PaintRect(altBits.bounds);
      BufOrBuf(@mainBuf,@altBuf);
    END;

  PenNormal;
  AltToScrn;
  clickTime := TickCount;
  GetMouse(clickLoc);
  LocalToGlobal(clickLoc);
  killDouble := TRUE;
END;


{$S         }
PROCEDURE HollowCurve;
VAR newPt,oldPt: Point;
    tol: INTEGER;
BEGIN
  tol := 3;
  IF fatFlag THEN tol := 1;
  JamLine;
  PinGridMouse(oldPt);
  AltBufLine(oldPt,oldPt,oldPt);
  REPEAT
    PinGridMouse(newPt);
    IF NOT NearPt(oldPt,newPt,tol) THEN
      BEGIN
        AltBufLine(oldPt,newPt,oldPt);
        oldPt := newPt;
      END;
  UNTIL NOT StillDown;
END;



{$S         }
FUNCTION GetRgn(fillFlag: BOOLEAN): RgnHandle;
CONST ptMax = 1000;
VAR firstPt,oldPt,newPt: Point;
    tempRgn:  RgnHandle;
    memLimit,grow: LongInt;
    ptArray: ARRAY[0..ptMax] OF Point;
    i,dh,dv,ptIndex: INTEGER;
    dstBits:    BitMap;
BEGIN
  IF fillFlag THEN JamLine;
  dstBits := docBits;
  IF fatFlag THEN dstBits := fatBits;
  memLimit := HalfHeap;  { leave half for packed region }
  IF memLimit > 20000 THEN memLimit := 20000;
  OpenRgn;
  ShowPen;
  PinGridMouse(firstPt);
  MoveTo(firstPt.h-halfLineSize,firstPt.v-halfLineSize);
  SetPortBits(dstBits);
  Line(0,0);
  SetPortBits(docBits);
  IF fatFlag THEN FatToScrn(@fatBuf,scrnPtr);

  oldPt := firstPt;
  ptIndex := 1;
  REPEAT
    PinGridMouse(newPt);
    dh := ABS(newPt.h - oldPt.h);
    dv := ABS(newPt.v - oldPt.v);
    IF (dh + dv) > 1 THEN
      BEGIN
        SetPortBits(dstBits);
        LineTo(newPt.h-halfLineSize,newPt.v-halfLineSize);
        SetPortBits(docBits);
        IF fatFlag THEN FatToScrn(@fatBuf,scrnPtr);
        ptArray[ptIndex] := newPt;
        ptIndex := ptIndex + 1;
        oldPt := newPt;
      END;
  UNTIL (NOT StillDown) OR (GetRgnMax > memLimit) OR (ptIndex = ptMax);

  { close the curve }
  SetPortBits(dstBits);
  LineTo(firstPt.h-halfLineSize,firstPt.v-halfLineSize);
  SetPortBits(docBits);
  IF fatFlag THEN FatToScrn(@fatBuf,scrnPtr);
  HidePen;
  tempRgn:=NewRgn;
  CloseRgn(tempRgn);
  OffsetRgn(tempRgn,halfLineSize,halfLineSize);

  IF fillFlag THEN
    BEGIN
      SetPortBits(altBits);
      JamFill;
      PaintRgn(tempRgn);
      JamLine;
      IF NOT borderFlag THEN PenPat(thePat);
      MoveTo(firstPt.h-halfLineSize,firstPt.v-halfLineSize);
      FOR i := 1 to ptIndex - 1 DO
        LineTo(ptArray[i].h-halfLineSize,ptArray[i].v-halfLineSize);
      LineTo(firstPt.h-halfLineSize,firstPt.v-halfLineSize);
    END;

  GetRgn := tempRgn;
  AltToScrn;
END;



{$S         }
PROCEDURE KillEdges;
{ discard edgeMask, restoring altBits to most recent edits }
VAR dstBits:    BitMap;
    mode:       INTEGER;
BEGIN
  IF edgeFlag THEN
    BEGIN
      mode := srcOr;
      IF lassoBlack then mode := srcBic;

      { replace ants with black or white }
      dstBits := docBits;
      IF fatFlag THEN dstBits := fatBits;
      CopyBits(altBits,dstBits,selRect,selRect,mode,Nil);
      IF fatFlag THEN MainToAlt;
      ScrnToAlt;
      IF fatFlag THEN AltToScrn;
      edgeFlag := FALSE;
    END;
END;



{$S         }
PROCEDURE ClearSel;
BEGIN
  IF selFlag OR maskFlag THEN
    BEGIN
      IF acceptFlag THEN AcceptEdits;
      MainToAlt;
      AltToScrn;
      IF txScrapHndl <> Nil THEN KillStuff;
      selFlag := FALSE;
      maskFlag := FALSE;
    END;
END;



{$S         }
PROCEDURE InvertSel;
BEGIN
  IF acceptFlag THEN AcceptEdits;

  IF selFlag THEN
    BEGIN
      SetPortBits(altBits);
      InvertRect(selRect);
      SetPortBits(docBits);
    END;

  IF maskFlag THEN
    BEGIN
      MaskToAlt(maskBits.bounds,altBits.bounds,srcXor);
      lassoBlack := NOT lassoBlack;
    END;

  AltToScrn;
END;



{$S         }
PROCEDURE FillSel;
BEGIN
  IF acceptFlag THEN AcceptEdits;
  SetPortBits(altBits);
  PenNormal;
  PenPat(thePat);

  IF selFlag THEN PaintRect(selRect);

  IF maskFlag THEN
    BEGIN
      PenMode(patXor);
      PaintRect(selRect);
      MaskToAlt(maskBits.bounds,altBits.bounds,srcBic);
      PaintRect(selRect);
    END;

  PenNormal;
  SetPortBits(docBits);
  AltToScrn;
END;



{$S         }
PROCEDURE StraightLine;
VAR newPt,oldPt,startPt: Point;
    lineTop,lineBot: INTEGER;
BEGIN
  JamLine;
  PinGridMouse(startPt);
  oldPt.h := 1000;      { force first time }
  REPEAT
    PinGridMouse(newPt);
    IF shiftFlag THEN Constrain(startPt,newPt,TRUE);
    IF NOT EqualPt(newPt,oldPt) THEN
      BEGIN
        MainToAlt;        { erase old }
        AltBufLine(startPt,newPt,oldPt);
        oldPt := newPt;
      END;
  UNTIL NOT StillDown;
END;



{$S         }
FUNCTION PolyLine(startPt: Point; VAR endPt: Point): BOOLEAN;

{ returns TRUE if aborted }

VAR newPt,oldPt: Point;
    sync:       LongInt;
    event:      EventRecord;
    abort:      BOOLEAN;

BEGIN
  oldPt.h := 1000;    { force first time }

  REPEAT
    PinGridMouse(newPt);
    IF shiftFlag THEN Constrain(startPt,newPt,TRUE);

    IF NOT EqualPt(newPt,oldPt) THEN
      BEGIN
        AltToScrn;   { restore screen and fatBits }
        IF fatFlag THEN
          BEGIN
            SetPortBits(fatBits);
            MoveTo(startPt.h-halfLineSize,startPt.v-halfLineSize);
            LineTo(newPt.h-halfLineSize,newPt.v-halfLineSize);
            SetPortBits(docBits);
            FatToScrn(@fatBuf,scrnPtr);
          END
        ELSE
          BEGIN
            MoveTo(startPt.h-halfLineSize,startPt.v-halfLineSize);
            LineTo(newPt.h-halfLineSize,newPt.v-halfLineSize);
          END;
        sync := TickCount;
        REPEAT UNTIL TickCount > sync + 2;
        oldPt := newPt;
      END;

    event.what := nullEvent;
    abort := FALSE;

    IF GetNextEvent(everyEvent,event) THEN
      BEGIN
        abort := (event.what = keyDown)
        OR ((event.what = mouseDown) AND NOT PtInRect(event.where,docRect));
      END;

  UNTIL abort OR (event.what = mouseUp);

  IF event.what = mouseUp THEN
    BEGIN
      clickTime := event.when;
      clickLoc := event.where;
    END;

  endPt := newPt;
  IF abort THEN endPt := startPt;
  PolyLine := abort;
END;



{$S         }
PROCEDURE CreatePoly(filled: BOOLEAN);
CONST ptMax = 1000;
VAR startPt,oldPt,newPt: Point;
    tempRgn:  RgnHandle;
    event:    EventRecord;
    done:     BOOLEAN;
    closed:   BOOLEAN;
    fillState: QDHandle;
    memLimit,grow: LongInt;
    ptArray:  ARRAY[0..ptMax] OF Point;
    i,ptIndex:  INTEGER;

BEGIN
  IF clickCount > 1 THEN EXIT(CreatePoly);
  memLimit := HalfHeap;  { leave half for packed region }
  IF memLimit > 20000 THEN memLimit := 20000;

  JamLine;
  fillState := Nil;
  IF filled THEN
    BEGIN
      OpenRgn;
      ShowPen;
      fillState := myWind.port.rgnSave;
      myWind.port.rgnSave := Nil;       { disable during dragging }
    END;

  PinGridMouse(startPt); { get initial starting point }
  oldPt := startPt;
  ptIndex := 1;

  REPEAT
    done := PolyLine(oldPt,newPt);

    IF NOT done THEN
      BEGIN
        ScrnToAlt;  { consolidate line into altBits }
        ptArray[ptIndex] := newPt;
        ptIndex := ptIndex + 1;
        myWind.port.rgnSave := fillState;
        HidePen;
        MoveTo(oldPt.h,oldPt.v);
        LineTo(newPt.h,newPt.v);
        ShowPen;
        myWind.port.rgnSave := Nil;
      END;

    closed := FALSE;
    IF ptIndex > 2 THEN
      BEGIN
        closed := NearPt(newPt,startPt,4*lineSize);
        done := done OR closed;
        done := done OR NearPt(newPt,oldPt,4*lineSize);
      END;

    done := done OR (ptIndex = ptMax);
    IF filled THEN done := done OR (GetRgnMax > memLimit);
    oldPt := newPt;
  UNTIL done;

  IF filled OR closed THEN AltBufLine(newPt,startPt,newPt);

  IF filled THEN
    BEGIN
      myWind.port.rgnSave := fillState;
      HidePen;
      MoveTo(newPt.h,newPt.v);
      LineTo(startPt.h,startPt.v);   { close region definition }
      tempRgn := NewRgn;
      CloseRgn(tempRgn);
      SetPortBits(altBits);

      JamFill;
      PaintRgn(tempRgn);
      DisposeRgn(tempRgn);

      JamLine;
      IF NOT borderFlag THEN PenPat(thePat);
      MoveTo(startPt.h-halfLineSize,startPt.v-halfLineSize);
      FOR i := 1 TO ptIndex - 1 DO
        LineTo(ptArray[i].h-halfLineSize,ptArray[i].v-halfLineSize);
      LineTo(startPt.h-halfLineSize,startPt.v-halfLineSize);
    END;

  AltToScrn;
  killDouble := TRUE;
END;



{$S         }
PROCEDURE ScrollFat;
VAR startPt,oldPt,newPt: Point;
    dstRect: Rect;
    dh,dv: INTEGER;
    minDh,maxDh,minDv,maxDv: INTEGER;
    saveFat: Rect;
BEGIN
  saveFat := fatBits.bounds;
  GetMouse(startPt);
  InitConstrain(startPt);
  oldPt := startPt;

  maxDh := fatBits.bounds.left   - myWind.port.portRect.left;
  minDh := fatBits.bounds.right  - myWind.port.portRect.right;
  maxDv := fatBits.bounds.top    - myWind.port.portRect.top;
  minDv := fatBits.bounds.bottom - myWind.port.portRect.bottom;

  REPEAT
    GetMouse(newPt);
    HVConstrain(newPt);
    IF NOT EqualPt(newPt,oldPt) THEN
      BEGIN
        dh := (newPt.h - startPt.h) DIV 3;
        dv := (newPt.v - startPt.v) DIV 3;
        dh := PinWord(dh, minDh, maxDh);
        dv := PinWord(dv, minDv, maxDv);
        fatBits.bounds := saveFat;
        OffsetRect(fatBits.bounds,-dh,-dv);
        BitsToScrn(mainBits);
        oldPt:=newPt;
      END;

  UNTIL NOT StillDown;
  fatCenter.h := (fatBits.bounds.left + fatBits.bounds.right) DIV 2;
  fatCenter.v := (fatBits.bounds.top + fatBits.bounds.bottom) DIV 2;
END;


{$S         }
PROCEDURE ScrollTo(newOrigin: Point);
VAR pinRect: Rect;
    dstFile: INTEGER;
BEGIN
  SetPort(@myWind);
  SetRect(pinRect,0,0,docWidth-416,docHeight-240);
  PinPt(newOrigin,pinRect);
  SetOrigin(newOrigin.h,newOrigin.v);
  docBits := myWind.port.portBits;
  altBits.bounds := myWind.port.portRect;
  dstFile := 0;
  IF workDirty THEN dstFile := workTo;
  ReadWrite(workFrom,dstFile,workDirty,TRUE,FALSE,FALSE,FALSE);
  IF workDirty THEN
    BEGIN
      SwapWord(workFrom,workTo);
      workDirty := FALSE;
    END;
  mainBits.bounds := altBits.bounds;
  maskBits.bounds := altBits.bounds;
  AcceptEdits;
  fatCenter.h := newOrigin.h+208;
  fatCenter.v := newOrigin.v+120;
END;



{$S         }
PROCEDURE ScrollDoc;
VAR startPt,oldPt,newPt: Point;
    oldOrigin,newOrigin: Point;
    pinRect: Rect;
BEGIN
  IF fatFlag THEN
    BEGIN
      ScrollFat;
      EXIT(ScrollDoc);
    END;

  SetPort(@myWind);
  oldOrigin := myWind.port.portRect.topLeft;
  newOrigin := oldOrigin;
  SetRect(pinRect,0,0,docWidth-416,docHeight-240);
  AcceptEdits;

  GetGridMouse(startPt);
  LocalToGlobal(startPt);
  InitConstrain(startPt);
  oldPt := startPt;
  REPEAT
    GetGridMouse(newPt);
    LocalToGlobal(newPt);
    HVConstrain(newPt);

    IF NOT EqualPt(newPt,oldPt) THEN
      BEGIN
        newOrigin := oldOrigin;
        SubPt(newPt,newOrigin);
        AddPt(startPt,newOrigin);
        PinPt(newOrigin,pinRect);
        SetOrigin(newOrigin.h,newOrigin.v);
        docBits := myWind.port.portBits;
        altBits.bounds := myWind.port.portRect;
        ZeroBuf(@altBuf);
        CopyBits(mainBits,altBits,mainBits.bounds,mainBits.bounds,srcCopy,Nil);
        AltToScrn;
        oldPt:=newPt;
      END;
  UNTIL NOT StillDown;

  IF NOT EqualPt(newOrigin,oldOrigin) THEN ScrollTo(newOrigin);
END;


{$S         }
PROCEDURE DragImage;
LABEL 9;
VAR startPt,oldPt,newPt,tempPt: Point;
    srcRect,dstRect,oldDstRect: Rect;
    dh,dv,minDh,maxDh,minDv,maxDv: INTEGER;
    anchorPt:  Point;
    fromRect,toRect: Rect;
    hSame,vSame: BOOLEAN;
    third:       INTEGER;
    saveGrid:    BOOLEAN;
    multiFlag:   BOOLEAN;
    copyFlag:    BOOLEAN;
    stretchFlag: BOOLEAN;
    stretchText: BOOLEAN;
    tol:         INTEGER;
    tempRect:    Rect;
    srcBits:     BitMap;

BEGIN
  workDirty := TRUE;
  docDirty := TRUE;
  saveGrid := toolGrid;
  toolGrid := TRUE;      { make grid work for lassoTool drag }
  multiFlag   := optionFlag AND featureFlag;
  copyFlag    := optionFlag AND NOT featureFlag;
  stretchFlag := featureFlag AND NOT optionFlag;

  IF copyFlag THEN acceptFlag := TRUE;
  IF acceptFlag THEN AcceptEdits;
  IF optionFlag THEN whiteFlag := FALSE;   { copy or multicopy }
  IF multiFlag THEN acceptFlag := TRUE;

  tempRect := myWind.port.portRect;
  IF fatFlag THEN tempRect := fatBits.bounds;
  minDh := tempRect.left - selRect.left;
  maxDh := tempRect.right - selRect.right;
  minDv := tempRect.top - selRect.top;
  maxDv := tempRect.bottom - selRect.bottom;

  PenNormal;
  GetSelPat(selIndex,myWind.port.pnPat);
  srcRect := selRect;
  dstRect := srcRect;
  GetGridMouse(startPt);
  InitConstrain(startPt);

  IF maskFlag THEN stretchFlag := FALSE;   { can't stretch a lasso selection }

  IF stretchFlag THEN WITH selRect DO   { set up anchor point for stretching }
    BEGIN
      hSame := FALSE;
      third := (right - left) DIV 3;
      IF startPt.h < left + third THEN anchorPt.h := right
      ELSE IF startPt.h > right - third THEN anchorPt.h := left
      ELSE hSame := TRUE;

      vSame := FALSE;
      third := (bottom - top) DIV 3;
      IF startPt.v < top + third THEN anchorPt.v := bottom
      ELSE IF startPt.v > bottom - third THEN anchorPt.v := top
      ELSE vSame := TRUE;

      IF hSame AND vSame THEN stretchFlag := FALSE;  { move instead of stretch }
    END;

  stretchText := stretchFlag AND (txScrapHndl <> Nil);
  IF (NOT maskFlag) AND (not stretchText) THEN
    BEGIN
      IF NOT AllocMask THEN GOTO 9;
      BufToBuf(@altBuf,maskBits.baseAddr);
    END;

  tol := 1;
  IF multiFlag THEN
    BEGIN
      tol := 2*lineSize;
      IF tol = 2 THEN tol := 1;
    END;

  srcBits := docBits;
  IF fatFlag THEN srcBits := fatBits;
  oldPt.h := 1000;   { force first time }
  REPEAT
    GetGridMouse(newPt);
    IF NOT stretchFlag THEN HVConstrain(newPt);

    IF NOT NearPt(newPt,oldPt,tol) THEN
      BEGIN
        oldDstRect := dstRect;
        dstRect := srcRect;
        IF stretchFlag THEN
          BEGIN
            tempPt := startPt;
            IF shiftFlag THEN  { preserve aspect }
              BEGIN
                Constrain(anchorPt,tempPt,FALSE);
                Constrain(anchorPt,newPt,FALSE);
              END;
            Pt2Rect(anchorPt,tempPt,fromRect);
            Pt2Rect(anchorPt,newPt,toRect);
            IF hSame THEN
              BEGIN
                toRect.left := fromRect.left;
                toRect.right := fromRect.right;
              END;
            IF vSame THEN
              BEGIN
                toRect.top := fromRect.top;
                toRect.bottom := fromRect.bottom;
              END;
            MapRect(dstRect,fromRect,toRect);
          END
        ELSE
          BEGIN
            dh := PinWord(newPt.h - startPt.h, minDh, maxDh);
            dv := PinWord(newPt.v - startPt.v, minDv, maxDv);
            IF gridOn AND toolGrid THEN
              BEGIN
                dh := Trunc8(dh);
                dv := Trunc8(dv);
              END;
            OffsetRect(dstRect,dh,dv);
          END;

        IF (NOT multiFlag) AND (NOT stretchText) THEN MainToAlt;

        IF selFlag THEN
          BEGIN
            SetPortBits(altBits);
            IF stretchText THEN
              BEGIN
                MainToAlt;
                DrawTxScrap(dstRect);
              END
            ELSE MaskToAlt(srcRect,dstRect,srcCopy);
            IF NOT multiFlag THEN FrameRect(dstRect);  { draw ants }
          END;

        IF maskFlag THEN
          BEGIN
            CopyBits(srcBits,altBits,oldDstRect,dstRect,srcXor,Nil);
            MaskToAlt(srcRect,dstRect,srcBic);
            CopyBits(srcBits,altBits,oldDstRect,dstRect,srcXor,Nil);
          END;

        AltToScrn;
        oldPt := newPt;
      END;  { if not NearPt }

  UNTIL NOT StillDown;

  IF selFlag THEN
    BEGIN
      SetSelRect(dstRect,TRUE);
      SetPortBits(altBits);
      IF stretchText THEN
        BEGIN
          MainToAlt;
          DrawTxScrap(dstRect);
        END
      ELSE IF NOT multiFlag THEN  { erase marching ants }
        BEGIN
          MainToAlt;
          MaskToAlt(srcRect,dstRect,srcCopy);
        END;
      AltToScrn;
    END;

  IF maskFlag THEN
    BEGIN
      selRect := dstRect;
      ScrollMask(dh,dv);
    END;

  IF NOT maskFlag THEN KillMask;

9: toolGrid := saveGrid;
END;



{$S         }
PROCEDURE SelDragImage;
LABEL 9;
VAR dstRect:    Rect;
    now:        INTEGER;
    saveSize:   INTEGER;
    oldIndex:   INTEGER;
    startPt,newPt,oldPt: Point;
BEGIN
  dstRect := myWind.port.portRect;
  IF fatFlag THEN dstRect := fatBits.bounds;
  IF insel AND (featureFlag OR NOT EqualRect(selRect,dstRect)) THEN
    BEGIN
      DragImage;
      EXIT(SelDragImage);
    END;

  { make a new rectangular selection }
  KillStuff;
  oldSelFlag := FALSE;
  oldMaskFlag := FALSE;

  saveSize := lineSize;
  SetLineSize(1);            { for PinGridMouse }
  PenNormal;

  PinGridMouse(startPt);
  dstRect.topLeft := startPt;
  dstRect.botRight := startPt;

  IF NOT AllocMask THEN GOTO 9;

  oldIndex := -1;  { force first }
  WHILE StillDown DO
    BEGIN
      now := LoWord(TickCount);
      IF fatFlag THEN now := now DIV 3;  { slow it down }
      selIndex := BitAnd(now,7);
      IF selIndex <> oldIndex THEN
        BEGIN
          PinGridMouse(newPt);
          Pt2Rect(startPt,newPt,dstRect);
          selFlag := NOT EmptyRect(dstRect);
          IF selFlag THEN
            BEGIN
              dstRect.right := dstRect.right + 1;
              dstRect.bottom := dstRect.bottom + 1;
            END;
          BufToBuf(@altBuf,maskBits.baseAddr);
          SetPortBits(maskBits);
          GetSelPat(selIndex,myWind.port.pnPat);
          FrameRect(dstRect);
          SetPortBits(docBits);
          bitsToScrn(maskBits);
          oldIndex := selIndex;
        END;
    END;
  KillMask;
  IF selFlag THEN SetSelRect(dstRect,TRUE);
  acceptFlag := TRUE;
  whiteFlag := TRUE;
9: SetLineSize(saveSize);
END;



{$S         }
PROCEDURE Lasso(startPt: Point);
LABEL 9;
VAR lassoRgn:   RgnHandle;
    dh,dv:      INTEGER;
    saveSize:   INTEGER;
    srcBits:    BitMap;
BEGIN
  KillStuff;
  whiteFlag     := FALSE;
  oldMaskFlag   := FALSE;
  lassoBlack    := FALSE;
  oldLassoBlack := FALSE;

  saveSize := lineSize;
  lineSize := 1;
  halfLineSize := 0;
  SetPinRect(2);
  PenNormal;
  lassoRgn := GetRgn(FALSE);

  selRect := lassoRgn^^.rgnBBox;
  IF EmptyRgn(lassoRgn) THEN
    BEGIN
      DisposeRgn(lassoRgn);
      GOTO 9;
    END;

  InsetRect(selRect,-1,-1);
  ZeroBuf(@altBuf);
  srcBits := docBits;
  IF fatFlag THEN srcBits := fatBits;
  CopyBits(srcBits,altBits,selRect,selRect,srcCopy,lassoRgn);
  DisposeRgn(lassoRgn);
  IF NOT AllocMask THEN GOTO 9;
  CalcMask(altBits,maskBits,selRect,selRect.topLeft,FALSE,TRUE);
  maskFlag := TRUE;
  OffsetRect(selRect,-maskBits.bounds.left,-maskBits.bounds.top);
  TrimBBox(maskBits.baseAddr,selRect);
  OffsetRect(selRect,maskBits.bounds.left,maskBits.bounds.top);
  HUnlock(maskHandle);
  IF EmptyRect(selRect) THEN KillMask;
  IF fatFlag THEN BufToBuf(@mainBuf,@altBuf);
  ScrnToAlt;   { restore alt }
  InsetRect(selRect,-1,-1);
  SetSelRect(selRect,TRUE);   { establish fatCenter }

9: whiteFlag := TRUE;
   acceptFlag := TRUE;
   oldSelRect := selRect;   { avoid ScrollMask in Undo }
   SetLineSize(saveSize);
END;



{$S         }
PROCEDURE PlaceCaret(startPt: Point; newLeft: BOOLEAN);
BEGIN
  { avoid accidental consolidation from menu double click }
  IF (theEvent.when < clickTime + GetDoubleTime)
  AND NearPt(theEvent.where,clickLoc,4)
  THEN EXIT(PlaceCaret);

  AcceptEdits;
  KillStuff;
  GridPoint(startPt);
  textLoc := startPt;
  caretLoc := startPt;
  IF newLeft THEN textLeft := startPt.h;
  textFlag := TRUE;
  textLines := 0;
  ObscureCursor;
END;


{$S         }
PROCEDURE EditDoc;
VAR pt: Point;
BEGIN
  IF (theTool > textTool) OR (theTool = grabberTool) THEN
    BEGIN
      { kill the text ? }
      AcceptEdits;   { kill selection ants }
      whiteFlag := FALSE;
      acceptFlag := TRUE;
    END;

  IF NOT fatFlag THEN GetMouse(fatCenter);
  GetFatMouse(pt);      { not gridded }

  IF theTool > textTool THEN
    BEGIN
      workDirty := TRUE;
      docDirty := TRUE;
    END;

  CASE theTool OF
    0: IF inSel THEN DragImage ELSE Lasso(pt);          { lasso or drag     }
    1: SelDragImage;                                    { select or drag    }
    2: ScrollDoc;                                       { scroll document   }
    3: PlaceCaret(pt,TRUE);                             { start new text    }
    4: SeedFill(pt);                                    { seed fill         }
    5: BrushPaint(toolCursor.data,thePat);              { spray paint       }
    6: BrushPaint(toolCursor.data,thePat);              { paint with brush  }
    7: PencilPaint;                                     { pencil            }
    8: StraightLine;                                    { straight line     }
    9: EraseSome;                                       { eraser            }
   10: CreateShape(rectShape,FALSE);                    { hollow rect       }
   11: CreateShape(rectShape,TRUE);                     { filled rect       }
   12: CreateShape(rRectShape,FALSE);                   { hollow roundRect  }
   13: CreateShape(rRectShape,TRUE);                    { filled roundRect  }
   14: CreateShape(ovalShape,FALSE);                    { hollow oval       }
   15: CreateShape(ovalShape,TRUE);                     { filled oval       }
   16: HollowCurve;                                     { hollow curve      }
   17: DisposeRgn(GetRgn(TRUE));                        { filled curve      }
   18: CreatePoly(FALSE);                               { hollow polygon    }
   19: CreatePoly(TRUE);                                { filled polygon    }
  END;

END;



{$S         }
PROCEDURE TrackCursor;

LABEL 1;

VAR wasInWindow:  BOOLEAN;
    wasInSel:     BOOLEAN;
    wasFatScroll: BOOLEAN;
    oldIndex:     INTEGER;
    ticks:        LongInt;
    now:          INTEGER;
    hOffset,vOffset: INTEGER;
    bitNum:       LongInt;
    mousePt:      Point;
    dh,dv:        INTEGER;
    saveTool:     INTEGER;

BEGIN
  ticks := TickCount;
  IF textFlag AND (ticks > nextCaretTime) THEN
    BEGIN
      InvertCaret;
      nextCaretTime := ticks + GetCaretTime;
    END;

  wasInWindow := inWindow;
  wasInSel    := inSel;
  wasFatScroll := fatScroll;

  fatScroll := fatFlag AND KeyIsDown(optionCode) AND (theTool = pencilTool);
  IF fatScroll <> wasFatScroll THEN
    BEGIN
      saveTool := theTool;
      IF fatScroll THEN theTool := grabberTool;
      SetToolCursor;
      theTool := saveTool;
    END;

  SetPort(@myWind);
  SetPortBits(docBits);

  GetMouse(mousePt);
  inWindow := windOpen AND PtInRect(mousePt,myWind.port.portRect);
  GetFatMouse(mousePt);

  IF maskFlag THEN   { lasso selection }
    BEGIN
      IF NOT edgeFlag THEN
        BEGIN
          maskBits.baseAddr := Pointer(ORD(maskHandle^));
          CalcEdges(Pointer(ORD(maskHandle^)),@altBuf,
                    selRect.top-altBits.bounds.top,
                    selRect.bottom-selRect.top,lassoBlack);
          edgeFlag := TRUE;
        END;

      maskBits.baseAddr := Pointer(ORD(maskHandle^));
      inSel := TRUE;
      FOR dh := -2 TO 2 DO
        FOR dv := -2 TO 2 DO
          IF PixelTrue(mousePt.h + dh, mousePt.v + dv, maskBits) THEN GOTO 1;

      inSel := FALSE;  { none of the pixels was true }
1:
    END
  ELSE inSel := selFlag AND PtInRect(mousePt,selRect);

  IF cursorFlag
  OR (inWindow <> wasInWindow)
  OR (inSel <> wasInSel) THEN      { time for a new cursor }
    BEGIN
      IF NOT inWindow THEN CursorNormal
      ELSE
        BEGIN
          IF gridOn AND toolGrid THEN
            BEGIN
              hOffset := BitAnd(-myWind.port.portBits.bounds.left,7);
              vOffset := BitAnd(-myWind.port.portBits.bounds.top,7);
              GridMouse(-8,-8,hOffset,vOffset);
            END;

          IF inSel
          THEN SetCursor(arrow)
          ELSE SetCursor(toolCursor);
        END;
    END;

    oldIndex := selIndex;
    now := LoWord(ticks);
    IF fatFlag THEN now := now DIV 3;  { slow down ants }
    selIndex := BitAnd(now,7);
    IF selIndex <> oldIndex THEN
      BEGIN
        IF edgeFlag THEN ShowEdges;
        IF selFlag THEN
          BEGIN
            PenNormal;
            GetSelPat(selIndex,myWind.port.pnPat);
            IF fatFlag THEN
              BEGIN
                SetPortBits(fatBits);
                FrameRect(selRect);
                SetPortBits(docBits);
                FatToScrn(@fatBuf,scrnPtr);
              END
            ELSE FrameRect(selRect);
          END;
      END;

  cursorFlag := FALSE;
END;


{$S       }
PROCEDURE TextChar;
VAR textPtr:  QDPtr;
    thisLine: StringHandle;
    len:      INTEGER;
    txHndl:   StringHandle;
BEGIN
  IF NOT textFlag THEN EXIT(TextChar);
  workDirty := TRUE;
  docDirty := TRUE;
  IF acceptFlag THEN AcceptEdits;
  whiteFlag := FALSE;

  IF theKey = CHR(3)  { enter } THEN
    BEGIN
      PlaceCaret(caretLoc,FALSE);
      EXIT(TextChar);
    END;

  IF textLines = 0 THEN   { create first stringhandle }
    BEGIN
      txHndl := Pointer(ORD(NewHandle(82)));
      IF txHndl = Nil THEN EXIT(TextChar);
      textHandles[1] := txHndl;
      txHndl^^ := '';   { install empty string }
      textLines := 1;
    END;

  IF theKey = CHR(13) THEN
    BEGIN
      { start a new line }
      IF textLines = maxLines THEN EXIT(TextChar);
      txHndl := Pointer(ORD(NewHandle(82)));
      IF txHndl = Nil THEN EXIT(TextChar);
      textLines := textLines + 1;
      textHandles[textLines] := txHndl;
      txHndl^^ := '';   { install empty string }
    END;

  IF theKey = CHR(8) THEN
    BEGIN
      { backspace over a character }
      thisLine := textHandles[textLines];
      len := Length(thisLine^^);
      IF len > 0 THEN DeleteChar(thisLine^^)
      ELSE IF textLines > 0 THEN
        BEGIN
          DisposHandle(Pointer(ORD(thisLine)));
          textLines := textLines - 1;
        END;
    END;

  IF theKey >= ' ' THEN
    BEGIN
      { add theKey to text and re-draw }
      thisLine := textHandles[textLines];
      len := Length(thisLine^^);
      IF len < 80 THEN AppendChar(theKey,thisLine^^);
    END;

  { redraw this line of text }
  ObscureCursor;
  UpdateText;
END;


{$S         }
PROCEDURE InvTool;
VAR tempRect: Rect;
BEGIN
  WITH tempRect DO
    BEGIN
      top := toolTop + 1 + (theTool DIV 2) * toolSpace;
      bottom := top + toolSpace - 1;
      IF ODD(theTool) THEN
        BEGIN
          left := toolMid+1;
          right := toolRight-1
        END
      ELSE
        BEGIN
          left := toolLeft+1;
          right := toolMid;
        END;
    END;  { with tempRect }

  InvertRect(tempRect);
END;


{$S SegUpdate }
PROCEDURE DrawTool;
VAR i,h,v,whichChar: INTEGER;
    tempRect:  Rect;
BEGIN
  OffsetRect(toolRect,1,1);
  PaintRect(toolRect);
  OffsetRect(toolRect,-1,-1);
  EraseRect(toolRect);
  FrameRect(toolRect);

  MoveTo(toolMid,toolTop);
  LineTo(toolMid,toolBottom);
  v := toolTop;
  whichChar := ORD('E');
  FOR i := 0 TO 9 DO
    BEGIN
      MoveTo(toolLeft + 14 - CharWidth(CHR(whichChar)) DIV 2, v + 4);
      DrawChar(CHR(whichChar));
      whichChar := whichChar + 1;

      MoveTo(toolMid + 14 - CharWidth(CHR(whichChar)) DIV 2, v + 4);
      DrawChar(CHR(whichChar));
      whichChar := whichChar + 1;

      v := v + toolSpace;
      MoveTo(toolLeft,v);
      IF i <> 9 THEN LineTo(toolRight-1,v);
    END;
  InvTool;
END;


PROCEDURE DrawLine;  FORWARD;

{$S         }
PROCEDURE UpdateWindow(* whichWindow: WindowPtr; okToSmash: BOOLEAN *);
BEGIN
  SetPort(whichWindow);
  BeginUpdate(whichWindow);

  IF whichWindow = @myWind THEN
    BEGIN
      IF okToSmash AND NOT scrnFlag THEN BitsToScrn(altBits)
      ELSE CopyBits(altBits,myWind.port.portBits,
                    altBits.bounds,myWind.port.portRect,srcCopy,Nil)
    END

  ELSE IF whichWindow = @deskWind THEN
    BEGIN
      FillRect(deskWind.port.portRect,gray);
      IF RectInRgn(toolRect,deskWind.port.visRgn) THEN DrawTool;
      IF RectInRgn(lineRect,deskWind.port.visRgn) THEN DrawLine;
      IF RectInRgn(patRect,deskWind.port.visRgn) THEN DrawPat;
    END;

  EndUpdate(whichWindow);
END;


{$S         }
PROCEDURE SetToolCursor;
VAR saveBits:           BitMap;
    savePort:           GrafPtr;
    dataCh,maskCh:      CHAR;
    hotH,hotV:          INTEGER;

BEGIN
  hotH := 8;
  hotV := 8;
  maskCh := CHR(97);     { all zeros }

  CASE theTool OF
    lassoTool: BEGIN
                 dataCh := CHR(69);
                 hotH := 2;
                 hotV := 15;
               END;

    selectTool:
      BEGIN
        dataCh := CHR(114);
        maskCh := CHR(115);
      END;

    grabberTool:
      BEGIN
        dataCh := CHR(71);
        maskCh := CHR(109);
      END;

    textTool:
      BEGIN
        dataCh := CHR(110);
        hotV := 13;
      END;

    fillTool:
      BEGIN
        dataCh := CHR(73);
        maskCh := CHR(93);
        hotH := 13;
        hotV := 16;
      END;

    sprayTool: dataCh := CHR(111);

    brushTool: dataCh := CHR(120 + theBrush);

    pencilTool:
      BEGIN
        dataCh := CHR(76);
        maskCh := CHR(116);
        hotH := 3;
        hotV := 16;
      END;

    eraseTool:
      BEGIN
        dataCh := CHR(119);   { erase box }
        maskCh := CHR(120);   { all ones }
      END;

    OTHERWISE CASE lineSize OF
                1: dataCh := CHR(89);
                2: dataCh := CHR(90);
                4: dataCh := CHR(91);
                8: dataCh := CHR(92);
              END;

  END;

  GetPort(savePort);
  SetPort(@deskWind);
  saveBits := deskWind.port.portBits;
  ZeroMem(@toolCursor.data,64);         { clear data and mask }
  deskWind.port.portBits.baseAddr := @toolCursor.data;
  deskWind.port.portBits.rowBytes := 2;
  SetRect(deskWind.port.portBits.bounds,toolLeft,toolTop,toolLeft+16,toolTop+16);
  MoveTo(toolLeft,toolTop);
  DrawChar(dataCh);
  deskWind.port.portBits.baseAddr := @toolCursor.mask;
  MoveTo(toolLeft,toolTop);
  DrawChar(maskCh);
  toolCursor.hotSpot.h := hotH;
  toolCursor.hotSpot.v := hotv;
  deskWind.port.portBits := saveBits;
  SetPort(savePort);
  cursorFlag := TRUE;
END;



{$S SegBrush }
PROCEDURE ChooseBrush;

CONST hLeft  = 16;
      vTop   = 16;
      hSpace = 32;
      vSpace = 32;
      hLeft2 = 24;  { hLeft + hSpace DIV 2 - 8 }
      vTop2  = 24;  { vTop + vSpace DIV 2 - 8 }
      nRows  = 4;   { beware, wired into BitAnd below }
      nCols  = 8;
      maxRow = 3;   { nRows - 1 }
      maxCol = 7;   { nCols - 1 }

VAR myDialog:   DialogPtr;
    dResult:    INTEGER;
    row,col:    INTEGER;
    pt:         Point;
    horiz,vert,brush: INTEGER;

    PROCEDURE InvBrush;
    VAR tempRect: Rect;
    BEGIN
      WITH tempRect DO
        BEGIN
          left := hLeft + col * hSpace;
          right := left + hSpace;
          top := vTop + row * vSpace;
          bottom := top + vSpace;
          PenSize(2,2);
          PenMode(patXor);
          FrameRect(tempRect);
          PenNormal;
        END;
    END;

BEGIN
  AcceptEdits;
  myDialog := GetNewDialog(brushDlog, Nil, Pointer(-1));
  SetPort(myDialog);

  TextFont(paintFont);
  brush := 120;
  horiz := hLeft2;
  FOR col := 0 TO maxCol DO
    BEGIN
      vert := vTop2;
      FOR row := 0 TO maxRow DO
        BEGIN
          MoveTo(horiz,vert);
          DrawChar(CHR(brush));
          vert := vert + vSpace;
          brush := brush + 1;
        END;
      horiz := horiz + hSpace;
    END;
  TextFont(systemFont);

  col := theBrush DIV nRows;
  row := BitAnd(theBrush,3);
  InvBrush; { highlight old }

  ModalDialog(Nil,dResult);
  GetMouse(pt);
  InvBrush;                   { unhighlight old }
  row := PinWord((pt.v - vTop) DIV vSpace,0,maxRow);
  col := PinWord((pt.h - hLeft) DIV hSpace,0,maxCol);
  InvBrush;   { highlight new }
  theBrush := row + col*nRows;
  SetToolCursor;
  REPEAT UNTIL NOT StillDown;

  DisposDialog(myDialog);
  killDouble := TRUE;
END;


PROCEDURE ShowPage;                               FORWARD;


{$S         }
PROCEDURE ChooseTool(pt: Point);
VAR tempRect:   Rect;
    cheapSet:   LongInt;

BEGIN
  KillStuff;
  oldSelFlag := FALSE;
  acceptFlag := FALSE;
  fatScroll := FALSE;
  SetPort(@deskWind);

  IF theTool <> eraseTool THEN prevTool := theTool;
  InvTool;    { deselect old tool }
  theTool := 2*((pt.v - toolTop) DIV toolSpace);
  IF theTool > 18 then theTool := 18;
  IF pt.h > toolMid THEN theTool := theTool + 1;

  tempRect := myWind.port.portRect;
  IF fatFlag THEN tempRect := fatBits.bounds;
  IF (theTool = eraseTool) AND (clickCount > 1) AND windOpen
  THEN   { erase all }
    BEGIN
      AcceptEdits;
      SetPort(@myWind.port);
      SetPortBits(altBits);
      EraseRect(tempRect);
      AltToScrn;
      SetPort(@deskWind);
      theTool := prevTool;      { we wont need the eraser anymore }
      clickCount := 1;          { for below }
      workDirty := TRUE;
      docDirty := TRUE;
    END;

  InvTool;    { select new tool }
  SetToolCursor;
  cheapSet := $50BF3000;
  toolGrid := BitTst(@cheapSet,theTool);
  { toolGrid := theTool IN [1,3,8,10,11,12,13,14,15,18,19]; }

  IF theTool = textTool THEN
    BEGIN
      SetPort(@myWind);
      GetFontInfo(info);   { swap in font }
    END;

  IF clickCount > 1 THEN
    BEGIN
      IF theTool = brushTool THEN ChooseBrush;
      IF windOpen THEN
        BEGIN
          IF theTool = selectTool THEN   { select entire window }
            BEGIN
              tempRect.right := tempRect.right + 1;
              tempRect.bottom := tempRect.bottom + 1;
              SetSelRect(tempRect,TRUE);
              selFlag := TRUE;
              oldSelRect := selRect;
              whiteFlag := TRUE;
              acceptFlag := TRUE;
            END;
          IF theTool = grabberTool THEN ShowPage;
          IF theTool = pencilTool THEN MenuCommand(aidsMenu,fatItem);
        END;
    END;
END;


{$S         }
PROCEDURE InvLnSize;
CONST check = 65;
VAR v: INTEGER;
BEGIN
  SetPort(@deskWind);
  v := lineTop + 5;
  IF borderFlag THEN CASE lineSize OF
    1: v := lineTop + 15;
    2: v := lineTop + 26;
    4: v := lineTop + 38;
    8: v := lineTop + 52;
  END;

  MoveTo(lineLeft + 2, v);
  TextMode(srcXor);
  DrawChar(CHR(check));
  TextMode(srcOr);
END;


{$S         }
PROCEDURE ChooseLine(pt: Point);
BEGIN
  InvLnSize;  { un-check old line size }
  lineSize := 1;
  borderFlag := (pt.v > lineTop + 18);
  IF pt.v > lineTop + 28 THEN lineSize := 2;
  IF pt.v > lineTop + 39 THEN lineSize := 4;
  IF pt.v > lineTop + 52 THEN lineSize := 8;
  SetLineSize(lineSize);
  InvLnSize;  { check new line size }
  IF theTool >= lineTool THEN SetToolCursor;    { update toolCursor }
END;



{$S SegUpdate }
PROCEDURE DrawLine;
VAR i,h,v: INTEGER;
    tempRect:  Rect;
BEGIN
  OffsetRect(lineRect,1,1);
  PaintRect(lineRect);
  OffsetRect(lineRect,-1,-1);
  EraseRect(lineRect);
  FrameRect(lineRect);

  WITH tempRect DO
    BEGIN
      right := lineRight - 7;
      left := lineLeft + 17;
      top := lineTop + 12;
      bottom := top + 1;
      FillRect(tempRect,ltGray);  { no border }

      top := lineTop + 23;
      bottom := top + 1;
      PaintRect(tempRect);      { line size 1 }

      top := lineTop + 33;
      bottom := top + 2;
      PaintRect(tempRect);      { line size 2 }

      top := lineTop + 44;
      bottom := top + 4;
      PaintRect(tempRect);      { line size 4 }

      top := lineTop + 57;
      bottom := top + 8;
      PaintRect(tempRect);      { line size 8 }
    END;

  InvLnSize;
END;


{$S SegPatEdit }
FUNCTION MyFilter(theDialog: DialogPtr;
                  VAR event: EventRecord;
                  VAR item: INTEGER): BOOLEAN;
VAR windPeek: WindowPeek;
BEGIN
  MyFilter := FALSE;
  IF event.what = mouseDown THEN
    BEGIN
      windPeek := Pointer(ORD(theDialog));
      IF PtInRect(event.where,docRect)
      AND NOT PtInRgn(event.where,windPeek^.strucRgn) THEN
        BEGIN
          patEdPt := event.where;
          item := 4;
          MyFilter := TRUE;
        END;
    END;
END;


{$S SegPatEdit }
PROCEDURE EditPat;

VAR myDialog:   DialogPtr;
    dResult:    INTEGER;
    i,j:        INTEGER;
    editRect,sampleRect,tempRect: Rect;

    savePort:   GrafPtr;
    newPat:     Pattern;

    newPt,oldPt: Point;
    bitNum:     LongInt;
    whiteFlg:   BOOLEAN;


    PROCEDURE GetPoint(VAR pt: Point);
    BEGIN
      GetMouse(pt);
      pt.h := PinWord((pt.h - editRect.left) DIV 8, 0,7);
      pt.v := PinWord((pt.v - editRect.top)  DIV 8, 0,7);
    END;


    PROCEDURE PlotDot(ix,iy: INTEGER);
    VAR tempRect: Rect;
    BEGIN
      WITH tempRect DO
        BEGIN
          left := editRect.left + ix * 8;
          right := left + 7;
          top := editRect.top + iy * 8;
          bottom := top + 7;
        END;
      PaintRect(tempRect);
    END;


BEGIN
  GetPort(savePort);   { needed ? }
  KillStuff;
  AcceptEdits;
  newPat := patterns[thePatIndex];
  myDialog := GetNewDialog(patEdDlog, Nil, POINTER(-1));
  SetPort(myDialog);

  PenNormal;
  SetRect(editRect,15,15,80,80);
  FrameRect(editRect);
  InsetRect(editRect,1,1);
  bitNum := 0;
  FOR j := 0 TO 7 DO
    FOR i := 0 to 7 DO
      BEGIN
        IF BitTst(@newPat,bitNum) THEN PlotDot(i,j);
        bitNum := bitNum + 1;
      END;

  SetRect(sampleRect,95,15,160,80);
  FrameRect(sampleRect);
  InsetRect(sampleRect,1,1);
  FillRect(sampleRect,newPat);

  REPEAT
    ModalDialog(@MyFilter,dResult);
    CASE dResult OF

      1: BEGIN   { hit OK }
           IF NOT EqualLongs(@newPat,@thePat,2) THEN
             BEGIN
               patterns[thePatIndex] := newPat;
               workDirty := TRUE;
               docDirty  := TRUE;
               DrawPat;
             END;
         END;

      2: BEGIN   { hit CANCEL }
         END;

      3: BEGIN   { click in 8 by 8 }
           GetPoint(oldPt);
           bitNum := 8 * oldPt.v + oldPt.h;
           whiteFlg := BitTst(@newPat,bitNum);
           PenNormal;
           IF whiteFlg THEN PenPat(white);
           PlotDot(oldPt.h,oldPt.v);
           IF whiteFlg THEN BitClr(@newPat,bitNum)
                       ELSE BitSet(@newPat,bitNum);
           FillRect(sampleRect,newPat);
           REPEAT
             GetPoint(newPt);
             IF NOT EqualPt(oldPt,newPt) THEN
               BEGIN
                 PlotDot(newPt.h,newPt.v);
                 bitNum := 8 * newPt.v + newPt.h;
                 IF whiteFlg THEN BitClr(@newPat,bitNum)
                             ELSE BitSet(@newPat,bitNum);
                 FillRect(sampleRect,newPat);
                 oldPt := newPt;
               END;
           UNTIL NOT StillDown;
         END;

      4: BEGIN    { pick up pattern }
           PenNormal;
           newPat := white;
           EraseRect(editRect);
           EraseRect(sampleRect);
           SetPort(@myWind);
           GlobalToLocal(patEdPt);
           patEdPt.h := Trunc8(patEdPt.h + 4);
           patEdPt.v := Trunc8(patEdPt.v + 4);
           bitNum := 0;
           FOR j := 0 TO 7 DO
             FOR i := 0 to 7 DO
               BEGIN
                 IF GetPixel(patEdPt.h+i,patEdPt.v+j) THEN
                   BEGIN
                     BitSet(@newPat,bitNum);
                     SetPort(myDialog);
                     PlotDot(i,j);
                     SetPort(@myWind);
                   END;
                 bitNum := bitNum + 1;
               END;
           SetPort(myDialog);
           FillRect(sampleRect,newPat);
         END;

    END;   { case dResult }

  UNTIL (dResult = 1) OR (dResult = 2);

  DisposDialog(myDialog);
  SetPort(savePort);
END;



{$S         }
PROCEDURE ChoosePat(pt: Point);
BEGIN
  IF pt.h > patStart THEN
    BEGIN
      thePatIndex := (pt.h - patStart) DIV patSpace;
      IF pt.v > patMid THEN thePatIndex := thePatIndex + patRow;
      thePat := patterns[thePatIndex];
      FillRect(fillSample,thePat);
    END;

  IF clickCount > 1 THEN EditPat;
END;



{$S SegSym }
PROCEDURE Symmetry;

CONST hLeft = 32;
      vTop  = 16;
      hRight = 144;
      vBottom = 128;
      hMid = 88;
      vMid = 72;

VAR myDialog:   DialogPtr;
    dResult:    INTEGER;
    savePort:   GrafPtr;
    clickPt:    Point;
    midPt:      Point;
    symRect:    Rect;


    PROCEDURE DrawSym;
    BEGIN
      SetRect(symRect,hLeft,vTop,hRight,vBottom);
      EraseRect(symRect);
      FrameRect(symRect);
      InsetRect(symRect,1,1);
      ClipRect(symRect);

      PenNormal;
      MoveTo(hMid,vTop);
      IF hSymFlag THEN
        BEGIN
          PenSize(3,3);
          Moov(-1,-1);
        END;
      Line(0,112);

      PenNormal;
      MoveTo(hLeft,vMid);
      IF vSymFlag THEN
        BEGIN
          PenSize(3,3);
          Moov(-1,-1);
        END;
      Line(112,0);

      PenNormal;
      MoveTo(hLeft,vTop);
      IF hvSymFlag THEN
        BEGIN
          PenSize(3,3);
          Moov(-1,-1);
        END;
      Line(112,112);

      PenNormal;
      MoveTo(hLeft,vBottom);
      IF vhSymFlag THEN
        BEGIN
          PenSize(3,3);
          Moov(-1,-1);
        END;
      Line(112,-112);

      ClipRect(thePort^.portRect);
      PenNormal;
    END;


BEGIN
  GetPort(savePort);   { needed ? }
  AcceptEdits;

  midPt.h := hMid;
  midPt.v := vMid;

  myDialog := GetNewDialog(symDlog, Nil, Pointer(-1));
  SetPort(myDialog);

  DrawSym;

  REPEAT
    ModalDialog(Nil,dResult);
    CASE dResult OF

      1: BEGIN   { hit OK }
         END;

      2: BEGIN   { hit CANCEL }
           hSymFlag  := FALSE;
           vSymFlag  := FALSE;
           hvSymFlag := FALSE;
           vhSymFlag := FALSE;
         END;

      3: BEGIN
           GetMouse(clickPt);
           Constrain(midPt,clickPt,TRUE);
           IF clickPt.h = midPt.h THEN hSymFlag := NOT hSymFlag
           ELSE IF clickPt.v = midPt.v THEN vSymFlag := NOT vSymFlag
           ELSE IF (clickPt.h-midPt.h) = (clickPt.v - midPt.v)
           THEN hvSymFlag := NOT hvSymFlag
           ELSE vhSymFlag := NOT vhSymFlag;
           DrawSym;
           REPEAT UNTIL NOT StillDown;
         END;


    END;   { case dResult }

  UNTIL (dResult = 1) OR (dResult = 2);

  symByte := MapSym(hSymFlag,vSymFlag,hvSymFlag,vhSymFlag);
  CheckItem(myMenus[aidsMenu],symItem,(symByte <> -128));

  DisposDialog(myDialog);
  SetPort(savePort);
END;



{$S SegPage }
PROCEDURE ShowPage;
LABEL 9;
VAR event:       EventRecord;
    newPt,oldPt: Point;
    smallPage:   Rect;
    windRect:    Rect;
    oldWindRect: Rect;
    newOrigin:   Point;
    windLeft,windTop: INTEGER;
    shiftDh:     INTEGER;
    shiftDv:     INTEGER;
    scrollDh:    INTEGER;
    scrollDv:    INTEGER;
    shiftPage,scrollPage: BOOLEAN;
    dstRect:     Rect;
    dstFile:     INTEGER;
    ok,cancel:   BOOLEAN;
    theControl:  ControlHandle;
    saveOrigin:  Point;

BEGIN
  AcceptEdits;
  fatFlag := FALSE;
  CheckItem(myMenus[aidsMenu],fatItem,fatFlag);

  { draw small image }
  dstFile := 0;
  IF workDirty THEN dstFile := workTo;
  ReadWrite(workFrom,dstFile,workDirty,FALSE,TRUE,FALSE,FALSE);
  IF rwResult <> 0 THEN
    BEGIN
      quitFlag := TRUE;
      EXIT(ShowPage);
    END;

  IF workDirty THEN
    BEGIN
      SwapWord(workFrom,workTo);
      workDirty := FALSE;
    END;

  InitCursor;  { get rid of hourglass }

  smallPage := myWind.port.portRect;
  smallPage.left := myWind.port.portRect.left + 208 - 96;
  smallPage.right := smallPage.left + 192;

  windRect := myWind.port.portRect;
  MapRect(windRect,pageRect,smallPage);
  oldWindRect := windRect;

  { allocate offscreen buffer for small image }
  IF NOT AllocMask THEN GOTO 9;

  saveOrigin := myWind.port.portRect.topLeft;
  SetOrigin(0,0);
  ShowControl(okButton);
  ShowControl(cancelButton);
  SetOrigin(saveOrigin.h,saveOrigin.v);

  ScrnToAlt;
  BufToBuf(@altBuf,maskBits.baseAddr);

  PenMode(patXor);
  PenPat(gray);
  SetPortBits(altBits);
  FrameRect(windRect);    { draw initial windRect in xor }
  AltToScrn;

  ok     := FALSE;
  cancel := FALSE;
  shiftDh := 0;
  shiftDv := 0;

  REPEAT
    REPEAT UNTIL GetNextEvent(everyEvent,event)   { ignore all events  }
    AND (event.what = mouseDown);                 { till a button down }
    shiftFlag := (BitAnd(event.modifiers,shiftKey) <> 0);
    GlobalToLocal(event.where);
    oldPt := event.where;

    IF NOT PtInRect(event.where,smallPage) THEN
      BEGIN
        LocalToGlobal(event.where);
        SetOrigin(0,0);
        GlobalToLocal(event.where);
        IF FindControl(event.where,@myWind,theControl) = 0 THEN SystemBeep(8)
        ELSE IF TrackControl(theControl,event.where,Nil) <> 0 THEN
          BEGIN
            IF theControl = okButton THEN ok := TRUE;
            IF theControl = cancelButton THEN cancel := TRUE;
          END;
        SetOrigin(saveOrigin.h,saveOrigin.v);
      END;

      InitConstrain(oldPt);
      windLeft := windRect.left;
      windTop := windRect.top;

      IF PtInRect(oldPt,windRect) THEN   { scroll page }
        REPEAT
          GetMouse(newPt);
          HVConstrain(newPt);
          IF NOT EqualPt(newPt,oldPt) THEN
            BEGIN
              SetPortBits(altBits);
              FrameRect(windRect);   { erase old windRect }
              windLeft := windLeft + newPt.h - oldPt.h;
              windTop  := windTop + newPt.v - oldPt.v;
              WITH windRect DO
                BEGIN
                  left := PinWord(windLeft,smallPage.left,smallPage.right-138);
                  top  := PinWord(windTop,smallPage.top,smallPage.bottom-80);
                  right := left + 138;
                  bottom := top + 80;
                END;
              FrameRect(windRect);   { draw new windRect }
              AltToScrn;
              oldPt := newPt;
            END;
        UNTIL NOT StillDown

      ELSE IF PtInRect(oldPt,smallPage) THEN   { shift page }
        BEGIN
          ClipRect(smallPage);
          REPEAT
            GetMouse(newPt);
            HVConstrain(newPt);
            IF NOT EqualPt(newPt,oldPt) THEN
              BEGIN
                shiftDh := shiftDh + newPt.h - oldPt.h;
                shiftDv := shiftDv + newPt.v - oldPt.v;
                dstRect := smallPage;
                OffsetRect(dstRect,shiftDh,shiftDv);
                SetPortBits(altBits);
                EraseRect(smallPage);
                MaskToAlt(smallPage,dstRect,srcCopy);
                FrameRect(windRect);
                AltToScrn;
                oldPt := newPt;
              END;
          UNTIL NOT StillDown;
          ClipRect(pageRect);
        END;

  UNTIL ok OR cancel;

  { 'Hide' buttons without leaving white }
  HidePen;
  HideControl(okButton);
  HideControl(cancelButton);
  ShowPen;

  PenNormal;
  KillMask;

  IF ok THEN
    BEGIN
      { always shift to nearest 8 to keep patterns aligned }
      shiftDh := ((3 * shiftDh) DIV 8) * 8;
      shiftDv := ((3 * shiftDv) DIV 8) * 8;
      shiftPage := (shiftDh <> 0) OR (shiftDv <> 0);

      scrollDh := 3 * (windRect.left - oldWindRect.left);
      scrollDv := 3 * (windRect.top - oldWindRect.top);
      scrollPage := (scrollDh <> 0) OR (scrollDv <> 0);

      IF shiftPage OR scrollPage THEN
        BEGIN
          newOrigin := myWind.port.portRect.topLeft;
          newOrigin.h := newOrigin.h + scrollDh;
          newOrigin.v := newOrigin.v + scrollDv;

          { pin at edges if less than 3 from edges }
          IF newOrigin.h < 3 THEN newOrigin.h := 0;
          IF newOrigin.v < 3 THEN newOrigin.v := 0;
          IF newOrigin.h > docWidth-419 THEN newOrigin.h := docWidth-416;
          IF newOrigin.v > docHeight-243 THEN newOrigin.v := docHeight-240;

          SetOrigin(newOrigin.h,newOrigin.v);
          docBits := myWind.port.portBits;
          altBits.bounds := myWind.port.portRect;

          OffsetRect(mainBits.bounds,0,shiftDv);  { for ReadWrite }

          dstFile := 0;
          IF shiftPage THEN dstFile := workTo;
          rwShftDh := shiftDh;
          rwShftDv := shiftDv;
          ReadWrite(workFrom,dstFile,TRUE,TRUE,FALSE,FALSE,FALSE);
          rwShftDh := 0;
          rwShftDv := 0;

          IF shiftPage THEN
            BEGIN
              SwapWord(workFrom,workTo);
              docDirty := TRUE;
            END;

          mainBits.bounds := altBits.bounds;
          maskBits.bounds := altBits.bounds;
          fatCenter.h := newOrigin.h+208;
          fatCenter.v := newOrigin.v+120;
          AcceptEdits;
        END;

    END;   { if OK }

9: MainToAlt;
   AltToScrn;
END;



{$S SegPrint }
FUNCTION OldPrDrvr: BOOLEAN;
{ Open print driver and check for old version. }
VAR aResult: INTEGER;
BEGIN
  PrDrvrOpen;
  PrNoPurge;
  OldPrDrvr := FALSE;
  IF PrDrvrVers < 2 THEN
    BEGIN
      OldPrDrvr := TRUE;
      PrPurge;
      PrDrvrClose;
      aResult := MyAlert(oldPrAlrt);
    END;
END;


{$S SegPrint }
PROCEDURE PrintDoc;
VAR dstFile: INTEGER;
BEGIN
  IF OldPrDrvr THEN EXIT(PrintDoc);

  AcceptEdits;
  dstFile := 0;
  IF workDirty THEN dstFile := workTo;

  PrCtlCall(iPrDevCtl,lPrReset,0,0);
  ReadWrite(workFrom,dstFile,workDirty,FALSE,TRUE,TRUE,FALSE);
  PrCtlCall(iPrDevCtl,lPrPageEnd,0,0);
  IF workDirty THEN
    BEGIN
      SwapWord(workFrom,workTo);
      workDirty := FALSE;
    END;
  AltToScrn;

  PrPurge;
  PrDrvrClose;
END;



{$S SegPrint }
PROCEDURE FinderPrint;
LABEL 9;
VAR result,srcFile: INTEGER;
BEGIN
  IF OldPrDrvr THEN EXIT(FinderPrint);

  REPEAT
    SetWTitle(@myWind,docName);
    result := OpenFile(docDrive,docVersion,docName,Nil,srcFile);
    IF ReadError(result,Nil) THEN GOTO 9;
    PrCtlCall(iPrDevCtl,lPrReset,0,0);
    ReadWrite(srcFile,0,FALSE,FALSE,TRUE,TRUE,FALSE);
    PrCtlCall(iPrDevCtl,lPrPageEnd,0,0);
    result := CloseFile(srcFile);
    AltToScrn;
    IF (NOT okPrint) OR (rwResult <> 0) THEN GOTO 9;
    finderMsg := FinderDoc(docDrive,docVersion,@docName);
  UNTIL finderMsg <> printMsg;

9:
  PrPurge;
  PrDrvrClose;
END;



{$S SegPrint }
PROCEDURE GetCatPic(drive,version,index: INTEGER; VAR name: Str63);
LABEL 9;
CONST bandSize = 704;   { docRow * 8 + 128 }

VAR bandBuf:               ARRAY[1..bandSize] OF QDByte;
    bandBits:              BitMap;
    result,band,scanLine:  INTEGER;
    srcByte:               LongInt;
    srcPtr,bandPtr,oldPtr: QDPtr;
    grow:                  LongInt;
    srcSize:               LongInt;
    srcHandle:             Handle;
    srcFirst,srcLast:      ^DiskBlock;
    srcBytes:              ARRAY[1..72] OF QDByte;
    srcFile:               INTEGER;
    dstRect:               Rect;
    thisRow,nextRow:       ARRAY[0..72] OF QDByte;   { one extra ! }

BEGIN
  WITH dstRect DO
    BEGIN
      left   := 28 + 112 * index;
      right  := left + 72;
      top    := 32;
      bottom := top + 90;
    END;

  FrameRect(dstRect);
  MoveTo(dstRect.left + 36 - StringWidth(name) DIV 2, dstRect.bottom + 16);
  DrawString(name);
  result := OpenFile(drive,version,name,Nil,srcFile);
  IF (result <> 0) THEN EXIT(GetCatPic);

  srcByte   := 512;    { skip header }
  srcSize   := BitAnd(MaxMem(grow),$FFFFFE00);    { trunc to mult of 512 }
  srcHandle := NewHandle(srcSize);
  HLock(srcHandle);
  srcFirst  := Pointer(ORD(srcHandle^));
  srcLast   := Pointer(ORD(srcFirst) + srcSize - 512);

  WITH bandBits DO
    BEGIN
      baseAddr := @bandBuf;
      rowBytes := docRow;
      bounds.left := 0;
      bounds.top := 0;
      bounds.right := docWidth;
      bounds.bottom := 8;
    END;

  { prime srcBuf }
  result := ReadData(srcFile,srcByte,srcSize,Pointer(ORD(srcFirst)));
  IF (result <> 0) AND (result <> endOfFile) THEN GOTO 9;

  srcPtr := Pointer(ORD(srcFirst));

  FOR band := 1 to 90 DO
    BEGIN
      bandPtr := @bandBuf;
      FOR scanLine := 1 TO 8 DO    { read and unpack 8 scanlines }
        BEGIN
          oldPtr := bandPtr;
          UnpackBits(srcPtr,bandPtr,docRow);
          IF ORD(bandPtr) - ORD(oldPtr) <> docRow THEN
            BEGIN
              SystemBeep(8);  { bad document }
              GOTO 9;
            END;

          IF ORD(srcPtr) >= ORD(srcLast) THEN
            BEGIN
              srcFirst^ := srcLast^;
              result := ReadData(srcFile,srcByte,srcSize-512,
                        Pointer(ORD(srcFirst)+512));
              IF (result <> 0) AND (result <> endOfFile) THEN GOTO 9;
              srcPtr := Pointer(ORD(srcPtr) - srcSize + 512);
            END;
        END;

      SampleBits(@bandBuf,@nextRow);
      IF band = 1 THEN thisRow := nextRow;
      HToneRow(@thisRow,@nextRow,thePort^.portBits,
               dstRect.left,dstRect.top,72,8,64,FALSE);
      thisRow := nextRow;
      OffsetRect(dstRect,0,1);
    END;

9: HUnlock(srcHandle);
   DisposHandle(srcHandle);
   result := CloseFile(srcFile);
END;



{$S SegPrint }
PROCEDURE PrintCatalog;
VAR i,horiz,vert,doc:   INTEGER;
    tempPort:           GrafPort;
    docCount:           INTEGER;
    fileName:           Str63;
    fileVersion:        INTEGER;
    headerHandle:       StringHandle;

BEGIN
  IF OldPrDrvr THEN EXIT(PrintCatalog);
  BusyCursor;

  OpenPort(@tempPort);
  TextFont(3);  { Ardmore }
  TextSize(12);

  WITH tempPort.portBits DO
    BEGIN
      baseAddr      := @altBuf;  { borrow memory from altBuf }
      rowBytes      := 72;
      bounds.left   := 0;
      bounds.right  := 576;
      bounds.top    := 0;
      bounds.bottom := 144;
    END;
  RectRgn(tempPort.visRgn,tempPort.portBits.bounds);

  ZeroBuf(@altBuf);
  MoveTo(28,10);
  headerHandle := GetString(headerString);
  i := CharWidth('A');  { swap font before de-referencing }
  DrawString(headerHandle^^);
  DrawChar(' ');
  docCount := CountFiles(docDrive,fileName);
  DrawString(fileName);
  SetFontLock(TRUE);

  horiz    := 0;
  vert     := 0;
  FOR doc := 1 TO docCount DO
    IF NextFile(docDrive,doc,'PNTG',fileName,fileVersion) THEN
      BEGIN
        IF NOT EqualString(fileName,'Paint1')
        AND NOT EqualString(fileName,'Paint2')
        THEN
          BEGIN
            GetCatPic(docDrive,fileVersion,horiz,fileName);
            horiz := horiz + 1;
            IF horiz > 4 THEN
              BEGIN
                IF vert = 0 THEN PrCtlCall(iPrDevCtl,lPrReset,0,0);
                okPrint := PrintRect(tempPort.portBits,tempPort.portBits.bounds);
                ZeroBuf(@altBuf);
                horiz := 0;
                vert := vert + 1;
                IF vert > 4 THEN
                  BEGIN
                    PrCtlCall(iPrDevCtl,lPrPageEnd,0,0);
                    vert := 0;
                  END;
              END;
          END;
      END;

  IF horiz <> 0 THEN  { print last partial row }
    BEGIN
      okPrint := PrintRect(tempPort.portBits,tempPort.portBits.bounds);
      vert := vert + 1;
    END;

  IF vert <> 0 THEN PrCtlCall(iPrDevCtl,lPrPageEnd,0,0);

  DrawChar(' ');       { make sure right font is unlocked. (disk swap) }
  SetFontLock(FALSE);
  MainToAlt;
  ClosePort(@tempPort);
  SetPort(@myWind);
  PrPurge;
  PrDrvrClose;
END;



{$S         }
PROCEDURE NewDocInit;
BEGIN
  mainBits.baseAddr := @mainBuf;
  mainBits.rowBytes := 52;
  SetRect(mainBits.bounds,80,120,80+416,120+240);
  ZeroBuf(@mainBuf);

  altBits := mainBits;
  altBits.baseAddr := @altBuf;
  ZeroBuf(@altBuf);

  fatBits.baseAddr := @fatBuf;
  fatBits.rowBytes := 8;
  SetRect(fatBits.bounds,80+208-26,120+120-15,80+208+26,120+120+15);
  fatCenter.h := 80+208;
  fatCenter.v := 120+120;
  pivot := fatCenter;
  ZeroFat(@fatBuf);   { install right and bottom borders }
  fatFlag := FALSE;
  CheckItem(myMenus[aidsMenu],fatItem,fatFlag);

  SetPort(@myWind);
  EraseRect(myWind.port.portRect);
  TextFont(theFontID);
  SetOrigin(mainBits.bounds.left,mainBits.bounds.top);
  docBits := myWind.port.portBits;

  textFlag   := FALSE;
  maskFlag   := FALSE;
  maskHandle := Nil;
  edgeFlag   := FALSE;
  maskBits   := mainBits;
  whiteFlag  := FALSE;
  acceptFlag := FALSE;
  selFlag    := FALSE;
  selRect    := zeroRect;
  lassoBlack := FALSE;
  textLoc    := zeroRect.topLeft;
  textLeft   := 0;
  caretLoc   := zeroRect.topLeft;

  oldWhiteFlag := whiteFlag;
  oldAcceptFlag := acceptFlag;
  oldMaskFlag := maskFlag;
  oldSelFlag  := selFlag;
  oldSelRect  := selRect;
  oldPivot    := pivot;
  oldLassoBlack := lassoBlack;

  selIndex   := 0;
  docDirty   := FALSE;
  workDirty  := FALSE;
END;



{$S        }
PROCEDURE BlankDoc(dstFile: INTEGER);
VAR result,scanLine: INTEGER;
    srcBuf: ARRAY[1..4] OF DiskBlock;
    srcPtr: QDPtr;
    dstByte: LongInt;
BEGIN
  ZeroMem(@srcBuf,SizeOf(srcBuf));
  srcBuf[1,1] := 2;  { version = 2 }
  CopyLongs(@patterns,@srcBuf[1,2],38*2);   { install current patterns }
  srcPtr := @srcBuf[2];
  FOR scanLine := 1 TO 720 DO               { fake up a blank packed document }
    BEGIN
      srcPtr^ := -71;                       { fill 72 opcode }
      srcPtr := Pointer(ORD(srcPtr)+1);
      srcPtr^ := 0;                         { fill data = 0 }
      srcPtr := Pointer(ORD(srcPtr)+1);
    END;
  dstByte := 0;
  result := WriteData(dstFile,dstByte,SizeOf(srcBuf),@srcBuf);
  IF dstFile = workFrom THEN workSize := dstByte;
  IF WriteError(result,Nil) THEN EXIT(BlankDoc);
  FlushVol(workDrive);
END;


{$S        }
FUNCTION SizeOfMain: LongInt;
{ how many bytes big is mainBuf when packed }
VAR srcPtr,dstPtr: QDPtr;
    dstBuf: PACKED ARRAY[0..53] OF QDByte;
    byteCount: LongInt;
    scanline: INTEGER;
BEGIN
  byteCount := 0;
  srcPtr := @mainBuf;
  FOR scanline := 1 TO 240 DO
    BEGIN
      dstPtr := @dstBuf;
      PackBits(srcPtr,dstPtr,52);
      byteCount := byteCount + ORD(dstPtr) - ORD(@dstBuf);
    END;
  SizeOfMain := byteCount;
END;


{$S        }
FUNCTION SaveDoc(askFirst: BOOLEAN): BOOLEAN;
LABEL 1;
VAR result:     INTEGER;
    newFile:    BOOLEAN;
    saveFile:   INTEGER;
    reply:      sfReply;
    where:      Point;
    promptHndl: StringHandle;
    bytesNeeded: LongInt;
    bytesAvail:  LongInt;
    fileDate:    LongInt;
    fileSize:    LongInt;

BEGIN
  SaveDoc := FALSE;  { assume failure }

  { set up reply in case SFPutFile not called }
  reply.fName := docName;
  reply.vRefnum := docDrive;
  reply.version := docVersion;

1: IF askFirst OR (Length(docName) = 0) THEN
    BEGIN
      AcceptEdits;
      BorrowAlt;                    { save screen image for refresh }
      where.h := 124;
      where.v := 100;
      promptHndl := GetString(saveString);
      HLock(Pointer(ORD(promptHndl)));
      SFPutFile(where,promptHndl^^,docName,Nil,@reply);
      HUnlock(Pointer(ORD(promptHndl)));
      UpdateWindow(@myWind,FALSE);
      MainToAlt;                        { restore saved screen image }
      IF NOT reply.good THEN EXIT(SaveDoc);  { hit cancel }
    END;

  bytesAvail := DiskSpace(reply.vRefnum);

  { create in case it didn't exist }
  result := CreateFile(reply.vRefnum,reply.version,reply.fName);
  newFile := (result <> fileExists);
  IF NOT newFile THEN
    BEGIN
      IF NOT CheckPntg(reply.vRefnum,reply.version,reply.fName) THEN
        BEGIN
          { existing doc is not a MacPaint document }
          result := MyAlert(notPntgAlrt);
          askFirst := TRUE;
          GOTO 1;  { try again }
        END;

      FileInfo(reply.vRefnum,reply.version,reply.fName,fileDate,fileSize);
      bytesAvail := bytesAvail + fileSize;  { since we will overwrite it }
    END;
  IF WriteError(result,Nil) THEN EXIT(SaveDoc);

  bytesNeeded := workSize;
  IF workDirty THEN bytesNeeded := bytesNeeded + SizeOfMain;

  IF bytesNeeded > bytesAvail THEN  { not enough room to save }
    BEGIN
      result := MyAlert(notRoomAlrt);
      IF newFile THEN
      result := DeleteFile(reply.vRefnum,reply.version,reply.fName);
      askFirst := TRUE;
      GOTO 1;  { try again }
    END;

  result := OpenFile(reply.vRefnum,reply.version,reply.fName,Nil,saveFile);
  IF WriteError(result,Nil) THEN EXIT(SaveDoc);

  ReadWrite(workFrom,saveFile,workDirty,FALSE,FALSE,FALSE,FALSE);

  result := CloseFile(saveFile);
  IF WriteError(result,Nil) OR (rwResult <> 0) THEN
    BEGIN
      IF newFile THEN
      result := DeleteFile(reply.vRefnum,reply.version,reply.fName);
      EXIT(SaveDoc);
    END;

  { only set docType if successfull }
  SetDocType(reply.vRefnum,reply.version,reply.fName);

  IF (docDrive <> reply.vRefNum)
  OR (docVersion <> reply.version)
  OR NOT EqualString(docName,reply.fName) THEN
    BEGIN
      docName := reply.fName;
      docDrive := reply.vRefnum;
      docVersion := reply.version;
      SetWTitle(@myWind,docName);
    END;

  FlushVol(reply.vRefnum);
  docDirty := FALSE;
  SaveDoc := TRUE;
END;



{$S         }
PROCEDURE RevertDoc;
VAR result,srcFile: INTEGER;
BEGIN
  result := SaveAlert(revertAlrt);  { are you sure }
  IF result <> 1 THEN EXIT(RevertDoc);

  IF Length(docName) = 0 THEN
    BEGIN
      NewDocInit;
      BlankDoc(workFrom);
    END
  ELSE
    BEGIN
      result := OpenFile(docDrive,docVersion,docName,Nil,srcFile);
      IF ReadError(result,Nil) THEN EXIT(RevertDoc);
      ReadWrite(srcFile,workTo,FALSE,TRUE,FALSE,FALSE,TRUE);
      result := CloseFile(srcFile);
      IF (rwResult <> 0) OR WriteError(result,Nil) THEN EXIT(RevertDoc);
      SwapWord(workFrom,workTo);
    END;

  docDirty := FALSE;
  workDirty := FALSE;
  AcceptEdits;
  AltToScrn;
END;



{$S         }
PROCEDURE OpenMyWind;
VAR i: INTEGER;
BEGIN
  ShowWindow(@myWind);
  SelectWindow(@myWind);
  SetPort(@myWind);
  ValidRect(myWind.port.portRect);
  windOpen := TRUE;
END;


{$S         }
FUNCTION ShouldSave(dialogNum: INTEGER): INTEGER;
{ returns 1 = save, 2 = forget, 3 = cancel }
VAR myDialog: DialogPtr;
    dResult:  INTEGER;
BEGIN
  AcceptEdits;
  BorrowAlt;                    { save screen image for refresh }
  myDialog := GetNewDialog(dialogNum, Nil, Pointer(-1));
  ModalDialog(Nil,dResult);
  DisposDialog(myDialog);
  UpdateWindow(@myWind,FALSE);
  MainToAlt;                    { restore saved screen image }
  ShouldSave := dResult;
END;



{$S         }
PROCEDURE CloseMyWind;
VAR result: INTEGER;
BEGIN
  IF docDirty AND windOpen THEN
    BEGIN
      result := ShouldSave(closeDlog);
      IF result = 3 { cancel } THEN EXIT(CloseMyWind);
      IF result = 1 { save } THEN
        BEGIN
          IF NOT SaveDoc(FALSE) THEN EXIT(CloseMyWind);
        END;
    END;
  HideWindow(@myWind);
  UpdateWindow(@deskWind,FALSE);
  windOpen := FALSE;
  selFlag := FALSE;   { so edit menu disabled }
  maskFlag := FALSE;
END;


{$S        }
PROCEDURE SetTitle(stringNumber: INTEGER);
VAR titleHandle: StringHandle;
BEGIN
  titleHandle := GetString(stringNumber);
  HLock(Pointer(ORD(titleHandle)));
  SetWTitle(@myWind,titleHandle^^);
  HUnlock(Pointer(ORD(titleHandle)));
END;


{$S SegInit }
PROCEDURE OpenFirstDoc;
VAR result,srcFile: INTEGER;
BEGIN
  { docName and docDrive were set up by FinderDoc }
  quitFlag := TRUE;   { assume failure }

  IF Length(docName) = 0 THEN
    BEGIN
      IF rescueFlag THEN
        BEGIN
          SetTitle(rescueString);
          ReadWrite(workFrom,0,FALSE,TRUE,FALSE,FALSE,TRUE);
          docDirty := TRUE;
        END
      ELSE SetTitle(untitledString);
    END
  ELSE
    BEGIN
      SetWTitle(@myWind,docName);
      result := OpenFile(docDrive,docVersion,docName,Nil,srcFile);
      IF ReadError(result,Nil) THEN EXIT(OpenFirstDoc);
      ReadWrite(srcFile,workTo,FALSE,TRUE,FALSE,FALSE,TRUE);
      result := CloseFile(srcFile);
      IF rwResult <> 0 THEN EXIT(OpenFirstDoc);  { with quitFlag TRUE }
      SwapWord(workFrom,workTo);
    END;

  quitFlag := FALSE;
  AcceptEdits;
  AltToScrn;
END;


{$S         }
PROCEDURE NewDoc;
VAR patHandle: Handle;
    patPtr: QDPtr;
BEGIN
  NewDocInit;
  docName := '';
  docDrive := -1;
  SetTitle(untitledString);
  OpenMyWind;

  { install default patterns }
  patHandle := GetResource('PAT#',patListID);
  patPtr := Pointer(ORD(patHandle^)+2);
  IF NOT EqualLongs(patPtr,@patterns,38*2) THEN
    BEGIN
      CopyLongs(patPtr,@patterns,38*2);
      DrawPat;
    END;

  BlankDoc(workFrom);
  AcceptEdits;
END;


{$S         }
PROCEDURE OpenDoc;
VAR result:     INTEGER;
    types:      sfTypeList;
    reply:      sfReply;
    srcFile:    INTEGER;
    where:      Point;
BEGIN
  where.h := 112;
  where.v := 70;
  types[0] := 'PNTG';
  reply.vRefnum := docDrive;
  SFGetFile(where,' ',Nil,1,@types,Nil,@reply);
  UpdateWindow(@deskWind,FALSE);
  IF NOT reply.good THEN EXIT(OpenDoc);
  NewDocInit;
  docDrive := reply.vRefnum;
  result := OpenFile(reply.vRefnum,reply.version,reply.fName,Nil,srcFile);
  IF ReadError(result,Nil) THEN EXIT(OpenDoc);
  docName := reply.fName;
  SetWTitle(@myWind,docName);
  OpenMyWind;
  ReadWrite(srcFile,workTo,FALSE,TRUE,FALSE,FALSE,TRUE);
  result := CloseFile(srcFile);
  IF rwResult <> 0 THEN EXIT(OpenDoc);
  IF WriteError(result,Nil) THEN EXIT(OpenDoc);
  SwapWord(workFrom,workTo);
  AcceptEdits;
  AltToScrn;
END;



{$S SegHelp }
PROCEDURE ShowPicture(whichPicture: INTEGER);
VAR myPic: PicHandle;
    event: EventRecord;
    saveOrigin: Point;
    theControl: ControlHandle;
BEGIN
  KillStuff;
  myPic := GetPicture(whichPicture);
  IF myPic = Nil THEN EXIT(ShowPicture);
  HNoPurge(Pointer(ORD(myPic)));
  DrawPicture(myPic,myWind.port.portRect);
  HPurge(Pointer(ORD(myPic)));
  saveOrigin := myWind.port.portRect.topLeft;
  SetOrigin(0,0);
  ShowControl(cancelButton);
  REPEAT
    REPEAT UNTIL GetNextEvent(everyEvent,event)  { ignore all events }
    AND (event.what = mouseDown);                { till mouse button }
    GlobalToLocal(event.where);
    IF FindControl(event.where,@myWind,theControl) = 0 THEN SystemBeep(8)
    ELSE IF TrackControl(theControl,event.where,Nil) <> 0 THEN
      BEGIN
        HideControl(cancelButton);
        SetOrigin(saveOrigin.h,saveOrigin.v);
        AltToScrn;
        EXIT(ShowPicture);
      END;
  UNTIL FALSE;  { until Cancel hit }
END;


{$S         }
PROCEDURE SetupText;
BEGIN
  GetFontInfo(info);
  txMinWidth := info.widMax+7;  { bold1,outL1,shad2,ital1,slop2 }
  txMinHeight := info.ascent + info.descent + info.leading;
  txMaxCount := 40;
  IF BitTst(@myWind.port.txFace,3)   { shadow  }
  OR BitTst(@myWind.port.txFace,4)   { outline }
  THEN txMaxCount := 16;
END;


{$S         }
PROCEDURE MenuCommand(* theMenu,theItem: INTEGER *);

VAR myDialog:   DialogPtr;
    dResult:    INTEGER;
    myString:   Str63;
    newFace:    Style;
    tempRect:   Rect;
    i,refNum:   INTEGER;
    styleWord:  INTEGER;
    bool:       BOOLEAN;

BEGIN
  IF theItem <= 0 THEN EXIT(MenuCommand);
  SetPort(@myWind);
  Case theMenu OF

    appleMenu: IF NOT Monkey THEN
    BEGIN
      IF active THEN
        BEGIN
          KillStuff;  { get rid of text, sel, mask }
          AcceptEdits;
          BorrowAlt;                   { save screen image for refresh }
        END;

      IF theItem = 1 THEN
        BEGIN
          { About MacPaint... }
          myDialog := GetNewDialog(paintDlog, Nil, Pointer(-1));
          ModalDialog(Nil,dResult);
          DisposDialog(myDialog);
          UpdateWindow(@myWind,FALSE);
          IF active THEN MainToAlt;  { restore saved screen image }
        END
      ELSE
        BEGIN
          CursorNormal;
          cursorFlag := TRUE;
          MyGetItem(myMenus[appleMenu],theItem,myString);
          tempLong := MaxMem(tempLong);   { purge and compact heap }
          refNum := OpenDeskAcc(myString);
          active := FALSE;   { so cursorNormal sticks }
          ornFlag := TRUE;
        END;
    END;


    fileMenu: IF NOT Monkey THEN
    BEGIN
      KillStuff;      { get rid of text, sel, mask }
      AcceptEdits;
      hiResFlag := FALSE;
      IF theItem = prFinalItem THEN hiResFlag := TRUE;
      CASE theItem OF
        newItem:    NewDoc;
        openItem:   OpenDoc;
        closeItem:  CloseMyWind;
        saveItem:   bool := SaveDoc(FALSE);
        saveAsItem: bool := SaveDoc(TRUE);
        revertItem: RevertDoc;
        prDraftItem,prFinalItem: PrintDoc;
        prCatItem:  PrintCatalog;
        quitItem:   quitFlag := TRUE;
      END;
    END;


    editMenu:
    BEGIN
      IF active AND (theItem <> copyItem) THEN
        BEGIN
          workDirty := TRUE;
          docDirty := TRUE;
        END;

      IF NOT SystemEdit(theItem-1) THEN
        CASE theItem OF
          undoItem:   Undo;
          clearItem:  ClearSel;
          copyItem:   CutOrCopy(FALSE);
          cutItem:    CutOrCopy(TRUE);
          pasteItem:  Paste;
          invertItem: InvertSel;
          fillItem:   FillSel;
          edgesItem:  TraceEdges;
          hFlipItem:  HFlip;
          vFlipItem:  VFlip;
          rotateItem: Rotate;
        END;
    END;


    aidsMenu:
    BEGIN
      CASE theItem OF
        gridItem:
          BEGIN
            gridOn := NOT gridOn;
            CheckItem(myMenus[aidsMenu],gridItem,gridOn);
            UpdateText;  { in case grid changes vert spacing }
          END;

        fatItem:
        BEGIN
          AcceptEdits;
          fatFlag := NOT fatFlag;
          CheckItem(myMenus[aidsMenu],fatItem,fatFlag);
          IF fatFlag THEN
            BEGIN
              IF maskFlag OR selFlag THEN
                BEGIN
                  fatCenter.h := (selRect.left + selRect.right) DIV 2;
                  fatCenter.v := (selRect.top + selRect.bottom) DIV 2;
                END;

              tempRect := myWind.port.portRect;
              InsetRect(tempRect,26,15);
              PinPt(fatCenter,tempRect);
              WITH fatBits.bounds DO
                BEGIN
                  left   := fatCenter.h - 26;
                  right  := fatCenter.h + 26;
                  top    := fatCenter.v - 15;
                  bottom := fatCenter.v + 15;
                END;

              bool := SectRect(selRect,fatBits.bounds,tempRect);
              IF NOT EqualRect(selRect,tempRect) THEN
                BEGIN
                  KillStuff;
                  oldSelFlag := FALSE;
                END;
            END;
          AltToScrn;
        END;

        pageItem:
        BEGIN
          KillStuff;
          ShowPage;
        END;

        brushItem:
        BEGIN
          KillStuff;
          ChooseBrush;
        END;

        patEdItem:
        BEGIN
          KillStuff;
          EditPat;
        END;

        symItem:
        BEGIN
          KillStuff;
          Symmetry;
        END;

        introItem:
        BEGIN
          ShowPicture(introPic);
        END;

        shortItem:
        BEGIN
          ShowPicture(shortPic);
        END;

      END;  { case theItem }
    END;  { aidsMenu }


    fontMenu:
      BEGIN  { choose font }
        KillMask;   { not enough room in heap for mask and font }
        CheckItem(myMenus[fontMenu],theFont,FALSE);
        theFont := theItem;
        CheckItem(myMenus[fontMenu],theFont,TRUE);
        MyGetItem(myMenus[fontMenu],theFont,myString);
        GetFNum(myString,theFontID);
        TextFont(theFontID);
        TextSize(fontSizes[theFontSize]);
        SetupText;
        UpdateText;

        FOR i := firstSize TO lastSize DO
          BEGIN
            styleWord := 0;
            IF RealFont(theFontID,fontSizes[i]) THEN styleWord := 8; {outline}
            MySetItemStyle(myMenus[sizeMenu],i,styleWord);
          END;
      END;

    sizeMenu:
      BEGIN     { choose font size }
        KillMask;   { not enough room in heap for mask and font }
        CheckItem(myMenus[sizeMenu],theFontSize,FALSE);
        theFontSize := theItem;
        CheckItem(myMenus[sizeMenu],theFontSize,TRUE);
        TextSize(fontSizes[theFontSize]);
        SetupText;
        UpdateText;
      END;

    styleMenu:
    BEGIN
      IF theItem < 7 THEN    { set TextFace }
        BEGIN
          IF theItem = plainItem
          THEN newFace := []
          ELSE
            BEGIN  { toggle a bit of style }
              newFace := myWind.port.txFace;
              IF BitTst(@newFace,9-theItem)
              THEN BitClr(@newFace,9-theItem)
              ELSE BitSet(@newFace,9-theItem);
            END;
          thePort^.txFace := newFace;
          SetupText;
          UpdateText;
          CheckItem(myMenus[styleMenu],plainItem,newFace=[]);
          FOR i := boldItem TO shadowItem DO
            CheckItem(myMenus[styleMenu],i,BitTst(@newFace,9-i));
        END
      ELSE
        BEGIN  { choose justification }
          txJustItem := theItem;
          textJust := teFillLeft;
          IF txJustItem = centerItem THEN textJust := teFillCenter;
          IF txJustItem = rightItem THEN textJust := teFillRight;
          UpdateText;
          FOR i := leftItem TO rightItem DO
            CheckItem(myMenus[styleMenu],i,i=txJustItem);
        END;
    END;

  END;  { case theMenu }

END;  { menucommand }



{$S         }
PROCEDURE EnableOne(whichItem: INTEGER; enable: BOOLEAN);
VAR menuHndl: MenuHandle;
BEGIN
  menuHndl := myMenus[whichMenu];
  IF enable
  THEN EnableItem(menuHndl,whichItem)
  ELSE DisableItem(menuHndl,whichItem);
END;



{$S         }
PROCEDURE CheckMenus;
VAR item: INTEGER;
    textOK,selOrMask: BOOLEAN;
BEGIN
  selOrMask := selFlag OR maskFlag;
  textOk := (txScrapHndl <> Nil) OR NOT selOrMask;

  whichMenu := fileMenu;
  EnableOne(newItem,NOT windOpen);
  EnableOne(openItem,NOT windOpen);
  EnableOne(saveItem,windOpen);
  EnableOne(saveAsItem,windOpen);
  EnableOne(revertItem,windOpen);
  EnableOne(prDraftItem,windOpen);
  EnableOne(prFinalItem,windOpen);
  EnableOne(closeItem,windOpen);

  whichMenu := editMenu;
  EnableOne(undoItem,windOpen);
  FOR item := cutItem TO clearItem DO EnableOne(item,selOrMask);
  EnableOne(invertItem,selOrMask);
  EnableOne(fillItem,selOrMask);
  FOR item := edgesItem TO rotateItem DO EnableOne(item,selFlag);
  EnableOne(pasteItem,windOpen);
  IF NOT active THEN FOR item := undoItem TO clearItem DO EnableOne(item,TRUE);
  EnableOne(blank0Item,FALSE);
  EnableOne(blank1Item,FALSE);

  { aids menu }
  whichMenu := aidsMenu;
  EnableOne(pageItem,windOpen);
  EnableOne(introItem,windOpen);
  EnableOne(shortItem,windOpen);

  whichMenu := fontMenu;
  FOR item := firstFont to lastFont DO EnableOne(item,textOk);

  whichMenu := sizeMenu;
  FOR item := firstSize to lastSize DO EnableOne(item,textOk);

  whichMenu := styleMenu;
  FOR item := plainItem to rightItem DO EnableOne(item,textOk);
  EnableOne(blank2Item,FALSE);
END;


{$S         }
PROCEDURE CommandChar;
{ a command key was pressed. Do appropriate command if any }
VAR minTime: LongInt;
    menuResult: LongInt;
    textOK:     BOOLEAN;
BEGIN
  textOk := (txScrapHndl <> Nil) OR NOT (selFlag OR maskFlag);
  IF textOK THEN
    BEGIN
      IF theKey = prevSizeChar THEN  { previous or smaller font }
        BEGIN
          IF shiftFlag THEN
            BEGIN
              IF theFont > firstFont
              THEN MenuCommand(fontMenu,theFont-1);      { previous font }
            END
          ELSE
            BEGIN
              IF theFontSize > firstSize
              THEN MenuCommand(sizeMenu,theFontSize-1);  { smaller fontSize }
            END;
          EXIT(CommandChar);
        END;

      IF theKey = nextSizeChar THEN  { next or bigger font }
        BEGIN
          IF shiftFlag THEN
            BEGIN
              IF theFont < lastFont
              THEN MenuCommand(fontMenu,theFont+1);  { next font  }
            END
          ELSE
            BEGIN
              IF theFontSize < lastSize
              THEN MenuCommand(sizeMenu,theFontSize+1);  { bigger fontSize }
            END;
          EXIT(CommandChar);
        END;
    END;  { if textOk }

  CheckMenus;
  menuResult := MenuKey(theKey);
  theMenu := HiWord(menuResult);
  theItem := LoWord(menuResult);
  HiLiteMenu(theMenu);
  minTime := TickCount+3;              { 1/20th of a second }
  MenuCommand(theMenu,theItem);
  REPEAT UNTIL TickCount > minTime;    { make sure the hilite shows }
  HiLiteMenu(0);
END;


{$S         }
PROCEDURE ProcessTheEvent;
VAR where:       Point;
    menuResult:  LongInt;
    i:           INTEGER;
    code:        INTEGER;
    whichWindow: WindowPtr;

BEGIN
  shiftFlag   := (BitAnd(theEvent.modifiers,shiftKey) <> 0);
  featureFlag := (BitAnd(theEvent.modifiers,cmdKey) <> 0);
  optionFlag  := (BitAnd(theEvent.modifiers,optionKey) <> 0);

  IF theEvent.what <> mouseUp THEN
    BEGIN
      IF caretState THEN InvertCaret;
      KillEdges;  { restore altBits if it was borrowed for mask edges }
    END;

  CASE theEvent.what OF

    mouseUp:
    BEGIN
      clickTime := theEvent.when;
      clickLoc := theEvent.where;
    END;

    mouseDown:
      BEGIN
        code := FindWindow(theEvent.where,whichWindow);

        IF (theEvent.when < clickTime + GetDoubleTime)
        AND NearPt(theEvent.where,clickLoc,4)
        THEN clickCount := clickCount + 1
        ELSE clickCount := 1;

        IF killDouble AND (clickCount > 1) THEN code := -1;  { ignore it ! }
        killDouble := FALSE;

        IF skipDouble THEN clickCount := 1;
        skipDouble := (clickCount > 1);

        SetPort(whichWindow);
        SetPinRect(lineSize);

        CASE code OF
          inSysWindow: SystemClick(theEvent,whichWindow);

          inMenuBar:
            BEGIN
              CheckMenus;   { enable or disable items }
              CursorNormal;
              menuResult := MenuSelect(theEvent.where);
              theMenu := HiWord(menuResult);
              theItem := LoWord(menuResult);
              MenuCommand(theMenu,theItem);
              HiLiteMenu(0);
              IF NOT Monkey THEN
              quitFlag := (theMenu = fileMenu) AND (theItem = quitItem);
              IF quitFlag THEN EXIT(ProcessTheEvent);
              clickTime := TickCount;
              GetMouse(clickLoc);
              LocalToGlobal(clickLoc);
              skipDouble := TRUE;
            END;

          inContent,inGrow:
          IF active THEN
            BEGIN
              IF whichWindow = @myWind THEN
                BEGIN
                  IF fatFlag
                  AND (theEvent.where.h <= 80+52+3)
                  AND (theEvent.where.v <= 48+30+1)
                  THEN MenuCommand(aidsMenu,fatItem)  { turn off FatBits }
                  ELSE EditDoc;
                  skipDouble := TRUE;   { force next click to single click }
                END
              ELSE IF whichWindow = @deskWind THEN
                BEGIN
                  IF PtInRect(theEvent.where,toolRect)
                  THEN ChooseTool(theEvent.where);
                  IF PtInRect(theEvent.where,lineRect)
                  THEN ChooseLine(theEvent.where);
                  IF PtInRect(theEvent.where,patRect)
                  THEN ChoosePat(theEvent.where);
                END;  { click in deskWind }
            END;    { inContent }

          inGoAway: IF active THEN
            BEGIN
              IF TrackGoAway(@myWind,theEvent.where)
              THEN MenuCommand(fileMenu,closeItem);
            END;

        END;  { case code }

      END;  { mouseDown }

     keyDown,autoKey:  IF active THEN
       BEGIN
         SetPort(@myWind);
         theKey := CHR(BitAnd(theEvent.message,255));

         IF featureFlag THEN
           BEGIN
             CommandChar;
             EXIT(ProcessTheEvent);
           END;

         IF (theKey = CHR(96)) AND NOT textFlag THEN
           BEGIN
             MenuCommand(editMenu,undoItem);
             EXIT(ProcessTheEvent);
           END;

         IF (theKey = CHR(8)) AND NOT textFlag THEN
           BEGIN
             MenuCommand(editMenu,clearItem);
             EXIT(ProcessTheEvent);
           END;

         TextChar;
       END;

    updateEvt:
      BEGIN
        whichWindow := Pointer(theEvent.message);
        UpdateWindow(whichWindow,active);
      END;

    activateEvt:
      BEGIN
        active := ODD(theEvent.modifiers);

        IF ornFlag THEN
          BEGIN
            FOR i := fileMenu to lastMenu DO
              BEGIN
                whichMenu := i;
                IF i <> editMenu THEN EnableOne(0,active);
              END;
            DrawMenuBar;
          END;

        IF active THEN
          BEGIN
            ornFlag := FALSE;             { no ornaments up }
            UpdateWindow(@myWind,active); { take care of pending updates }
            MainToAlt;                    { restore saved screen image }
          END;
      END;

   END; { case theEvent.what }

END;   { ProcessEvent }


{$S         }
FUNCTION MyGrowHeap(bytesNeeded: LongInt): LongInt;
BEGIN
  MyGrowHeap := 0;
END;


{$S SegInit }
PROCEDURE SetUpMenus;
{ once-only initialization for my menus }
VAR i:          INTEGER;
BEGIN
  InitMenus;   { initialize Unit Menus }
  FOR i:=firstMenu to lastMenu DO
    BEGIN
      myMenus[i] := GetMenu(i);

      IF i = appleMenu THEN
        BEGIN
          myMenus[i]^^.menuData[1] := CHR(20);  { apple character }
          AddResMenu(myMenus[appleMenu],'DRVR');
        END;

      IF i = fontMenu THEN
        BEGIN
          AddResMenu(myMenus[fontMenu],'FONT');
          lastFont := CountMItems(myMenus[fontMenu]);
        END;

      InsertMenu(myMenus[i],0);
    END;
END;


{$S SegInit }
PROCEDURE InitOnce;

VAR dummyHndl:  Handle;
    i:          INTEGER;
    wind:       GrafPtr;
    result:     INTEGER;
    refNum:     INTEGER;
    patHandle:  Handle;
    scrnOffset: LongInt;
    fontName:   Str255;
    itemString: Str63;
    fontCmdHndl: StringHandle;
    titleHndl:   StringHandle;
    grow:        LongInt;
    date1,date2: LongInt;
    size1,size2: LongInt;


    FUNCTION WorkError(result: INTEGER): BOOLEAN;
    VAR aResult: INTEGER;
    BEGIN
      WorkError := FALSE;
      IF (result <> 0) AND (result <> fileExists) THEN
        BEGIN
          aResult := MyAlert(workSpaceAlrt);
          WorkError := TRUE;
        END;
    END;


BEGIN
  TEInit;
  InitDialogs(Nil);     { no restart proc }

  heapJam := FALSE;
  quitFlag := TRUE;                     { assume failure }
  finderMsg := -1;                      { in case exit   }
  heapTop := StackPtr - 9216;           { save 9K for stack }
  SetApplLimit(Pointer(heapTop));
  IF MemError <> 0 THEN { oops, heap already too big }
    BEGIN
      result := MyAlert(bigScrapAlrt);
      EXIT(InitOnce);
    END;

  dummyHndl := NewHandle(10000000);     { force max heap grow now  }
  SetGrowZone(@MyGrowHeap);             { then take over grow proc }

  IF (FreeMem < 17000) THEN             { quit if not enough heap }
    BEGIN
      result := MyAlert(memFullAlrt);
      EXIT(InitOnce);
    END;

  IF DiskSpace(0) < minDiskSpace THEN   { quit if disk is full }
    BEGIN
      result := MyAlert(diskAlrt);
      EXIT(InitOnce);
    END;

  active   := TRUE;
  ornFlag  := TRUE;  { fake out first activate }
  scrnFlag := FALSE;

  SetRect(zeroRect,0,0,0,0);
  SetRect(pageRect,0,0,576,720);
  SetRect(docRect,80,48,496,288);

  SetRect(toolRect,toolLeft,toolTop,toolRight,toolBottom);
  SetRect(lineRect,lineLeft,lineTop,lineRight,lineBottom);
  SetRect(patRect,patLeft,patTop,patRight,patBottom);

  FlushEvents(everyEvent,0);
  patHandle := GetResource('PAT#',patListID);
  CopyLongs(Pointer(ORD(patHandle^)+2),@patterns,38*2);

  scrnOffset := docRect.top * screenBits.rowBytes + docRect.left DIV 8;
  scrnPtr := Pointer(ORD(screenBits.baseAddr) + scrnOffset);

  titleHndl := GetString(titleString);   { 'MacPaint by Bill Atkinson' }
  docName := titleHndl^^;
  wind := NewWindow(@myWind,docRect,docName,TRUE,documentProc,Nil,TRUE,1);
  okButton := GetNewControl(okBtn,@myWind);
  cancelButton := GetNewControl(cancelBtn,@myWind);
  windOpen := TRUE;
  SetPort(wind);
  ClipRect(pageRect);
  ValidRect(myWind.port.portRect);

  WindPaint(FALSE);   { disable paintWhite }
  wind := NewWindow(@deskWind,screenBits.bounds,'desk',
                    TRUE,documentProc,Nil,FALSE,1);
  WindPaint(TRUE);
  SetPort(@deskWind);
  FillRect(deskWind.port.portRect,gray);
  TextFont(paintFont);
  theTool := brushTool;
  prevTool := theTool;
  toolGrid := FALSE;
  theBrush := 7;       { small round brush }
  borderFlag := TRUE;
  SetLineSize(1);
  thePatIndex := 0;
  SetToolCursor;
  UpdateWindow(@deskWind,FALSE);
  SetUpMenus;
  nextCaretTime := 0;
  caretState := FALSE;
  clickTime  := 0;
  clickCount := 1;
  killDouble := FALSE;
  skipDouble := FALSE;
  gridOn     := FALSE;
  hSymFlag   := FALSE;
  vSymFlag   := FALSE;
  hvSymFlag  := FALSE;
  vhSymFlag  := FALSE;
  symByte    := -128;

  textLines := 0;      { no type-in yet }

  txScrapHndl := Nil;
  txScrapSize := 0;

  txJustItem := leftItem;
  textJust := teFillLeft;
  CheckItem(myMenus[styleMenu],plainItem,TRUE);
  CheckItem(myMenus[styleMenu],txJustItem,TRUE);

  fontSizes[1] := 9;
  fontSizes[2] := 10;
  fontSizes[3] := 12;
  fontSizes[4] := 14;
  fontSizes[5] := 18;
  fontSizes[6] := 24;
  fontSizes[7] := 36;
  fontSizes[8] := 48;
  fontSizes[9] := 72;
  theFontSize  := 3;     { text size 12 point }
  maskHandle := Nil;     { so KillMask won't croak }
  CheckItem(myMenus[sizeMenu],theFontSize,TRUE);

  fontCmdHndl := GetString(fontCmdString);
  prevSizeChar := fontCmdHndl^^[1];
  nextSizeChar := fontCmdHndl^^[2];

  theFont := 1;  { in case not found }
  GetFontName(1,fontName);
  FOR i := 1 TO lastFont DO
    BEGIN
      MyGetItem(myMenus[fontMenu],i,itemString);
      IF EqualString(fontName,itemString) THEN theFont := i;
    END;
  MenuCommand(fontMenu,theFont);  { choose initial font, highlight real sizes }

  inWindow   := FALSE;
  inSel      := FALSE;
  fatScroll := FALSE;
  cursorFlag := TRUE;

  hiResFlag := TRUE;    { default for finder print }

  rwShftDh := 0;
  rwShftDv := 0;
  rwResult := 0;

  finderMsg := FinderDoc(docDrive,docVersion,@docName);   { get file name from finder }
  IF finderMsg = printMsg THEN EXIT(InitOnce);

  workDrive := 0;
  result := CreateFile(workDrive,0,'Paint1');
  rescueFlag := (result = fileExists);
  result := OpenFile(workDrive,0,'Paint1',Nil,workFrom);
  IF WorkError(result) THEN EXIT(InitOnce);

  result := CreateFile(workDrive,0,'Paint2');
  rescueFlag := rescueFlag AND (result = fileExists);
  result := OpenFile(workDrive,0,'Paint2',Nil,workTo);
  IF WorkError(result) THEN EXIT(InitOnce);
  IF Length(docName) <> 0 THEN rescueFlag := FALSE;
  IF rescueFlag THEN
    BEGIN
      FileInfo(workDrive,0,'Paint1',date1,size1);
      FileInfo(workDrive,0,'Paint2',date2,size2);
      workSize := size1;
      IF date2 > date1 THEN  { use most recent file }
        BEGIN
          SwapWord(workTo,workFrom);
          workSize := size2;
        END;
    END
  ELSE BlankDoc(workFrom);
  BlankDoc(workTo);

  quitFlag := FALSE;   { we made it }
  CursorNormal;
END;



{$S         }
PROCEDURE QuitProgram;
VAR result: INTEGER;
BEGIN
  IF docDirty AND windOpen THEN
    BEGIN
      quitFlag := FALSE;   { in case we cancel quit }
      result := ShouldSave(quitDlog);
      IF result = 3 { cancel } THEN EXIT(QuitProgram);
      IF result = 1 { save } THEN
        BEGIN
          IF NOT SaveDoc(FALSE) THEN EXIT(QuitProgram);
        END;
      quitFlag := TRUE;
    END;

  BusyCursor;
  InitWindows;  { erase the screen }

  result := CloseFile(workFrom);
  result := CloseFile(workTo);
  result := DeleteFile(workDrive,0,'Paint1');
  result := DeleteFile(workDrive,0,'Paint2');
  FlushVol(workDrive);
  IF KeyIsDown(featureCode) THEN EjectReset;
END;


{$S         }
BEGIN  { main program }
  tempWord := UnloadScrap;
  MoreMasters;  { allocate 2nd block of 64 master pointers }
  MoreMasters;  { allocate 3rd block of 64 master pointers }
  MoreMasters;  { allocate 4th block of 64 master pointers }
  MoreMasters;  { allocate 5th block of 64 master pointers }
  InitGraf(@thePort);
  InitFonts;
  InitWindows;
  InitOnce;
  IF finderMsg = printMsg THEN
    BEGIN
      UnloadSeg(@InitOnce);
      NewDocInit;
      FinderPrint;
    END;
  IF quitFlag THEN EXIT(MacPaint);
  NewDocInit;
  OpenFirstDoc;

  REPEAT
    UnloadSeg(@InitOnce);       { SegInit    }
    UnloadSeg(@PrintDoc);       { SegPrint   }
    UnloadSeg(@CutOrCopy);      { SegScrap   }
    UnloadSeg(@Paste);          { SegPaste   }
    UnloadSeg(@Rotate);         { SegFlip    }
    UnloadSeg(@ShowPage);       { SegPage    }
    UnloadSeg(@DrawPat);        { SegUpdate  }
    UnloadSeg(@EditPat);        { SegPatEdit }
    UnloadSeg(@ChooseBrush);    { SegBrush   }
    UnloadSeg(@Symmetry);       { SegSym     }
    UnloadSeg(@ShowPicture);    { SegHelp    }

    IF heapJam THEN
      BEGIN
        tempLong := MaxMem(tempLong);   { purge heap }
        heapJam := FALSE;
      END;

    SystemTask;    { give desk accessories a chance }
    IF active THEN TrackCursor;
    IF GetNextEvent(everyEvent,theEvent) THEN ProcessTheEvent;

    IF active AND NOT quitFlag AND (DiskSpace(0) < minDiskSpace) THEN
      BEGIN
        tempWord := SaveAlert(diskAlrt);
        quitFlag := TRUE;
      END;

    IF quitFlag THEN QuitProgram;

  UNTIL quitFlag;
  ExitToShell;
END.
