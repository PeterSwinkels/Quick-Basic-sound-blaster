DEFINT A-Z

TYPE DTAStr
 Reserved AS STRING * 20
 Attribute AS INTEGER
 FileTime AS INTEGER
 FileDate AS INTEGER
 FileSize AS LONG
 FileName AS STRING * 13
END TYPE

TYPE RegTypeX
 ax AS INTEGER
 bx AS INTEGER
 cx AS INTEGER
 dx AS INTEGER
 bp AS INTEGER
 si AS INTEGER
 di AS INTEGER
 flags AS INTEGER
 ds AS INTEGER
 es AS INTEGER
END TYPE

TYPE SBSettingsStr
 BaseAddress AS INTEGER
 DMAAddress AS INTEGER
 DMAChannel AS INTEGER
 DMALength AS INTEGER
 DMAPage AS INTEGER
END TYPE

TYPE WVDataStr
 DataID AS STRING * 4
 DataSize AS LONG
END TYPE

TYPE WVFactStr
 Padding AS STRING * 2
 FactID AS STRING * 4
 FactSize AS LONG
 FactDataSize AS LONG
END TYPE

TYPE WVHeaderStr
 RIFFID AS STRING * 4
 RIFFSize AS LONG
 WAVEID AS STRING * 4
 FormatID AS STRING * 4
 FormatSize AS LONG
 Format AS INTEGER
 Channels AS INTEGER
 Frequency AS LONG
 ByteRate AS LONG
 Alignment AS INTEGER
 BitsPerSample AS INTEGER
END TYPE

TYPE WVParametersStr
 EMSHandle AS INTEGER
 Frequency AS LONG
 Length AS LONG
END TYPE

TYPE WVPlayerStr
 ChunkLength AS LONG
 ChunkPageCount AS INTEGER
 CurrentPage AS INTEGER
 Paused AS INTEGER
 Remainder AS LONG
END TYPE

CONST EMSPAGESIZE = &H4000&

DECLARE FUNCTION EMSAllocatePages (PageCount)
DECLARE FUNCTION EMSError$ (ErrorCode)
DECLARE FUNCTION EMSFreeHandles ()
DECLARE FUNCTION EMSFreePages ()
DECLARE FUNCTION EMSInstalled ()
DECLARE FUNCTION EMSPageFrameAddress ()
DECLARE FUNCTION FileExists (FileName$)
DECLARE FUNCTION SBBaseAddress ()
DECLARE FUNCTION SBDMAChannel ()
DECLARE FUNCTION SBInitialize (SB AS SBSettingsStr)
DECLARE FUNCTION SBInUse (SB AS SBSettingsStr)
DECLARE FUNCTION WVLoad (WVFile$, WVParameters AS WVParametersStr, EMSPagesReserved)
DECLARE SUB Delay (Interval!)
DECLARE SUB EMSCopyBaseToEMS (Length&, SrcSegment, SrcOffset, DstHandle, DstOffset, DstPage)
DECLARE SUB EMSDeallocatePages (Handle)
DECLARE SUB EMSMapPages (PhysicalStart, LogicalStart, PageCount, Handle)
DECLARE SUB Initialize ()
DECLARE SUB InterruptX (intnum AS INTEGER, inreg AS RegTypeX, outreg AS RegTypeX)
DECLARE SUB Main (WVFile$)
DECLARE SUB Quit (Message$, WVParameters AS WVParametersStr)
DECLARE SUB SBPause (SB AS SBSettingsStr)
DECLARE SUB SBPlayWave (WVParameters AS WVParametersStr, WVPlayer AS WVPlayerStr, SB AS SBSettingsStr)
DECLARE SUB SBResume (SB AS SBSettingsStr)
DECLARE SUB SBSpeakerOff (SB AS SBSettingsStr)
DECLARE SUB SBSpeakerOn (SB AS SBSettingsStr)
DECLARE SUB SBVolume (SB AS SBSettingsStr, LeftSide, RightSide)
DECLARE SUB SBWriteDSP (Byte, SB AS SBSettingsStr)

DIM SHARED EMSErrorCode

Initialize
Main COMMAND$

SUB Delay (Interval!)
DIM StartTime!

 StartTime! = TIMER
 DO: LOOP UNTIL TIMER >= StartTime! + Interval! OR TIMER <= Interval!
END SUB

FUNCTION EMSAllocatePages (PageCount)
DIM Registers AS RegTypeX

 Registers.ax = &H4300
 Registers.bx = PageCount
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100

 EMSAllocatePages = Registers.dx
END FUNCTION

SUB EMSCopyBaseToEMS (Length&, SrcSegment, SrcOffset, DstHandle, DstOffset, DstPage)
DIM Registers AS RegTypeX

 CopyInformation$ = MKL$(Length&) + CHR$(&H0) + MKI$(&H0) + MKI$(SrcOffset) + MKI$(SrcSegment) + CHR$(&H1) + MKI$(DstHandle) + MKI$(DstOffset) + MKI$(DstPage)

 Registers.ax = &H5700
 Registers.ds = VARSEG(CopyInformation$)
 Registers.si = SADD(CopyInformation$)
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

SUB EMSDeallocatePages (Handle)
DIM Registers AS RegTypeX

 Registers.ax = &H4500
 Registers.dx = Handle
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

FUNCTION EMSError$ (ErrorCode)
DIM Registers AS RegTypeX

 SELECT CASE EMSErrorCode
  CASE &H0: Message$ = "No error."
  CASE &H80: Message$ = "Internal error."
  CASE &H81: Message$ = "Hardware malfunction."
  CASE &H82: Message$ = "Busy."
  CASE &H83: Message$ = "Invalid handle."
  CASE &H84: Message$ = "Undefined function requested by application."
  CASE &H85: Message$ = "No more handles available."
  CASE &H86: Message$ = "Error in save or restore of mapping context."
  CASE &H87: Message$ = "Insufficient memory pages in system."
  CASE &H88: Message$ = "Insufficient memory pages available."
  CASE &H89: Message$ = "Zero pages requested."
  CASE &H8A: Message$ = "Invalid logical page number encountered."
  CASE &H8B: Message$ = "Invalid physical page number encountered."
  CASE &H8C: Message$ = "Page-mapping hardware state save area is full."
  CASE &H8D: Message$ = "Save of mapping context failed."
  CASE &H8E: Message$ = "Restore of mapping context failed."
  CASE &H8F: Message$ = "Undefined subfunction."
  CASE &H90: Message$ = "Undefined attribute type."
  CASE &H91: Message$ = "Feature not supported."
  CASE &H92: Message$ = "Successful, but a portion of the source region has been overwritten."
  CASE &H93: Message$ = "Length of source or destination region exceeds length of region allocated to either source or destination handle."
  CASE &H94: Message$ = "Conventional and expanded memory regions overlap."
  CASE &H95: Message$ = "Offset within logical page exceeds size of logical page."
  CASE &H96: Message$ = "Region length exceeds 1 MB"
  CASE &H97: Message$ = "Source and destination EMS regions have same handle and overlap."
  CASE &H98: Message$ = "Memory source or destination type undefined."
  CASE &H9A: Message$ = "Specified alternate map register or DMA register set not supported."
  CASE &H9B: Message$ = "All alternate map register or DMA register sets currently allocated."
  CASE &H9C: Message$ = "Alternate map register or DMA register sets not supported."
  CASE &H9D: Message$ = "Undefined or unallocated alternate map register or DMA register set."
  CASE &H9E: Message$ = "Dedicated DMA channels not supported."
  CASE &H9F: Message$ = "Specified dedicated DMA channel not supported."
  CASE &HA0: Message$ = "No such handle name."
  CASE &HA1: Message$ = "A handle found had no name, or duplicate handle name."
  CASE &HA2: Message$ = "Attempted to wrap around 1 MB conventional address space."
  CASE &HA3: Message$ = "Source array corrupted."
  CASE &HA4: Message$ = "Operating system denied access."
  CASE ELSE: Message$ = "Undefined error: 0x" + HEX$(EMSErrorCode) + "."
 END SELECT

 EMSError$ = Message$
END FUNCTION

FUNCTION EMSFreeHandles
DIM Registers AS RegTypeX

 Registers.ax = &H4B00
 InterruptX &H67, Registers, Registers
 UsedHandles = Registers.bx

 Registers.ax = &H5402
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
 TotalHandles = Registers.bx

 EMSFreeHandles = TotalHandles - UsedHandles
END FUNCTION

FUNCTION EMSFreePages
DIM Registers AS RegTypeX

 Registers.ax = &H4200
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
 EMSFreePages = Registers.bx
END FUNCTION

FUNCTION EMSInstalled
DIM Registers AS RegTypeX

 Registers.ax = &H3567
 InterruptX &H21, Registers, Registers

 DEF SEG = Registers.es
 FOR Position = &HA TO &H11
  EMM$ = EMM$ + CHR$(PEEK(Position))
 NEXT Position

 EMSInstalled = (EMM$ = "EMMXXXX0")
END FUNCTION

SUB EMSMapPages (PhysicalStart, LogicalStart, PageCount, Handle)
DIM Registers AS RegTypeX

 FOR Page = 0 TO PageCount - 1
  MapInformation$ = MapInformation$ + MKI$(LogicalStart + Page) + MKI$(PhysicalStart + Page)
 NEXT Page

 Registers.ax = &H5000
 Registers.cx = PageCount
 Registers.dx = Handle
 Registers.ds = VARSEG(MapInformation$)
 Registers.si = SADD(MapInformation$)
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

FUNCTION EMSPageFrameAddress
DIM Registers AS RegTypeX

 Registers.ax = &H4100
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
 EMSPageFrameAddress = Registers.bx
END FUNCTION

FUNCTION FileExists (FileName$)
DIM DTA AS DTAStr
DIM Registers AS RegTypeX

 Registers.ax = &H1A00
 Registers.dx = VARPTR(DTA)
 Registers.ds = VARSEG(DTA)
 InterruptX &H21, Registers, Registers

 Attributes = 0
 Pattern$ = FileName$ + CHR$(0)

 Registers.ax = &H4E00
 Registers.cx = Attributes
 Registers.dx = SADD(Pattern$)
 Registers.ds = VARSEG(Pattern$)
 InterruptX &H21, Registers, Registers

 FileExists = (INSTR(DTA.FileName, CHR$(0)) > 1)
END FUNCTION

SUB Initialize
 SCREEN 0
 WIDTH 80, 25
 PALETTE
 COLOR 7, 0
 CLS

 EMSErrorCode = &H0
END SUB

SUB Main (WVFile$)
DIM SB AS SBSettingsStr
DIM WVParameters AS WVParametersStr
DIM WVPlayer AS WVPlayerStr

 WVPlayer.Paused = 0
 SB.BaseAddress = SBBaseAddress
 SB.DMAChannel = SBDMAChannel
 SBVolume SB, &HF, &HF

 LOCATE 1, 1: PRINT "Wave Player v1.01 - by: Peter Swinkels ***2021***"

 IF FileExists(WVFile$) THEN
  IF SBInitialize(SB) THEN
   IF EMSInstalled THEN
    EMSPagesToReserve = EMSFreePages
    IF EMSPagesToReserve > 0 THEN
     WVParameters.EMSHandle = EMSAllocatePages(EMSPagesToReserve)
     IF WVLoad(WVFile$, WVParameters, EMSPagesToReserve) THEN
      PRINT
      PRINT " Playing: "; WVFile$
      PRINT
      PRINT " Keys: Escape = Quit  P = Pause/Resume."

      SBSpeakerOn SB
  
      WVPlayer.CurrentPage = 0
      WVPlayer.Remainder = WVParameters.Length

      DO
       LOCATE 7, 3: PRINT "Data remaining: "; LTRIM$(RTRIM$(STR$(WVPlayer.Remainder))); "/"; LTRIM$(RTRIM$(STR$(WVParameters.Length))); " bytes."; SPACE$(5)
      
       IF WVPlayer.Remainder = &H0& THEN
        EXIT DO
       ELSEIF WVPlayer.Remainder > &HFFFF& THEN
        WVPlayer.ChunkLength = &HFFFF&
       ELSE
        WVPlayer.ChunkLength = WVPlayer.Remainder
       END IF

       WVPlayer.ChunkPageCount = (WVPlayer.ChunkLength \ EMSPAGESIZE) + 1
       SBPlayWave WVParameters, WVPlayer, SB
      
       DO WHILE SBInUse(SB)
        Key$ = INKEY$
        SELECT CASE Key$
         CASE "p", "P"
          WVPlayer.Paused = NOT WVPlayer.Paused
          IF WVPlayer.Paused THEN SBPause SB ELSE SBResume SB
         CASE CHR$(27)
          EXIT DO
        END SELECT
       LOOP
    
       WVPlayer.CurrentPage = WVPlayer.CurrentPage + WVPlayer.ChunkPageCount
       WVPlayer.Remainder = WVPlayer.Remainder - WVPlayer.ChunkLength
      LOOP UNTIL Key$ = CHR$(27)
    
      SBSpeakerOff SB
      Quit "", WVParameters
     ELSE
      Quit WVFile$ + " - unsupported WAVE file format.", WVParameters
     END IF
    ELSE
     Quit "Not enough free EMS pages.", WVParameters
    END IF
   ELSE
    Quit "No EMS driver detected.", WVParameters
   END IF
  ELSE
   Quit "Could not initialize the Sound Blaster.", WVParameters
  END IF
 ELSE
  Quit "Could not open the file: " + WVFile$ + ".", WVParameters
 END IF
END SUB

SUB Quit (Message$, WVParameters AS WVParametersStr)
 IF (NOT EMSErrorCode = &H0) OR (NOT Message$ = "") THEN
  IF NOT Message$ = "" THEN PRINT Message$
  IF NOT EMSErrorCode = &H0 THEN
   PRINT "EMS Error code: 0x"; HEX$(EMSErrorCode); ""
   PRINT EMSError$(EMSErrorCode)
  END IF
  DO WHILE INKEY$ = ""
  LOOP
 END IF

 EMSDeallocatePages WVParameters.EMSHandle

 COLOR 7, 0
 CLS
 SYSTEM
END SUB

FUNCTION SBBaseAddress
 BaseAddress = &H220

 Settings$ = LTRIM$(RTRIM$(UCASE$(ENVIRON$("BLASTER"))))

 Position1 = INSTR(Settings$, "A")
 IF Position1 > 0 THEN
  Position2 = INSTR(Position1, Settings$, " ")
  BaseAddress = VAL("&H" + MID$(Settings$, Position1 + 1, (Position2 - Position1)))
 END IF

 SBBaseAddress = BaseAddress
END FUNCTION

FUNCTION SBDMAChannel
 DMAChannel = &H1

 Settings$ = LTRIM$(RTRIM$(UCASE$(ENVIRON$("BLASTER"))))

 Position1 = INSTR(Settings$, "D")
 IF Position1 > 0 THEN
  Position2 = INSTR(Position1, Settings$, " ")
  DMAChannel = VAL("&H" + MID$(Settings$, Position1 + 1, (Position2 - Position1)))
 END IF

 SBDMAChannel = DMAChannel
END FUNCTION

FUNCTION SBInitialize (SB AS SBSettingsStr)
 Success = -1

 OUT SB.BaseAddress + &H6, &H1
 OUT SB.BaseAddress + &H6, &H0

 Delay .03

 Success = ((INP(SB.BaseAddress + &HE) AND &H80) = &H80) AND (INP(SB.BaseAddress + &HA) = &HAA)

 IF Success THEN
  SB.DMAAddress = SB.DMAChannel + &H1
  SB.DMALength = SB.DMAChannel + &H2

  SELECT CASE SB.DMAChannel
   CASE 0
    SB.DMAPage = &H87
   CASE 1
    SB.DMAPage = &H83
   CASE 2
    SB.DMAPage = &H81
   CASE 3
    SB.DMAPage = &H82
   CASE ELSE
    Success = 0
  END SELECT
 END IF

 IF Success THEN
  OUT &HA, SB.DMAChannel + &H4
  OUT &HC, &H0
 END IF

 SBInitialize = Success
END FUNCTION

FUNCTION SBInUse (SB AS SBSettingsStr)
 OUT &HC, &H0

 BytesLeft& = INP(SB.DMALength) + INP(SB.DMALength) * &H100&

 SBInUse = NOT (BytesLeft& = &H0 OR BytesLeft& = &HFFFF&)
END FUNCTION

SUB SBPause (SB AS SBSettingsStr)
 SBWriteDSP &HD0, SB
END SUB

SUB SBPlayWave (WVParameters AS WVParametersStr, WVPlayer AS WVPlayerStr, SB AS SBSettingsStr)
 EMSMapPages 0, WVPlayer.CurrentPage, WVPlayer.ChunkPageCount, WVParameters.EMSHandle

 OUT &HA, SB.DMAChannel + &H4
 OUT &HC, &H0
 OUT &HB, SB.DMAChannel + &H48

 EMSPageFrameFlatAddress& = (&H10000 + EMSPageFrameAddress) * &H10&

 OUT SB.DMAAddress, EMSPageFrameFlatAddress& AND &HFF&
 OUT SB.DMAAddress, (EMSPageFrameFlatAddress& AND &HFF00&) \ &H100&

 OUT SB.DMAPage, (&H10000 + EMSPageFrameAddress) / &H1000&

 OUT SB.DMALength, (WVPlayer.ChunkLength - &H1&) AND &HFF&
 OUT SB.DMALength, ((WVPlayer.ChunkLength - &H1&) AND &HFF00&) \ &H100&

 OUT &HA, SB.DMAChannel

 SBWriteDSP &H40, SB
 SBWriteDSP ((&H100& - &HF4240) \ WVParameters.Frequency), SB

 SBWriteDSP &H14, SB
 SBWriteDSP ((WVPlayer.ChunkLength - &H1&) AND &HFF&), SB
 SBWriteDSP (((WVPlayer.ChunkLength - &H1&) AND &HFF00&) \ &H100&), SB
END SUB

SUB SBResume (SB AS SBSettingsStr)
 SBWriteDSP &HD4, SB
END SUB

SUB SBSpeakerOff (SB AS SBSettingsStr)
 SBWriteDSP &HD3, SB
END SUB

SUB SBSpeakerOn (SB AS SBSettingsStr)
 SBWriteDSP &HD1, SB
END SUB

SUB SBVolume (SB AS SBSettingsStr, LeftSide, RightSide)
 OUT SB.BaseAddress + &H4, &H22
 OUT SB.BaseAddress + &H5, (LeftSide * &H10 OR RightSide)
END SUB

SUB SBWriteDSP (Byte, SB AS SBSettingsStr)
 WAIT SB.BaseAddress + &HC, &H80, &H80
 OUT SB.BaseAddress + &HC, Byte
END SUB

FUNCTION WVLoad (WVFile$, WVParameters AS WVParametersStr, EMSPagesReserved)
DIM WVData AS WVDataStr
DIM WVFact AS WVFactStr
DIM WVHeader AS WVHeaderStr

 Success = -1

 FileH = FREEFILE
 OPEN WVFile$ FOR BINARY AS FileH
  GET #FileH, , WVHeader
  IF WVHeader.FormatSize = 18 THEN GET #FileH, , WVFact
  GET #FileH, , WVData

  IF WVHeader.RIFFID = "RIFF" AND WVHeader.WAVEID = "WAVE" AND WVHeader.FormatID = "fmt " AND WVHeader.Format = 1 AND WVHeader.Channels = 1 AND WVHeader.Frequency <= 32000 AND WVHeader.Alignment = 1 AND WVData.DataID = "data" THEN
   WVParameters.Frequency = WVHeader.Frequency
   WVParameters.Length = LOF(FileH) - LEN(WVHeader)
   MaximumLength& = EMSPagesReserved * EMSPAGESIZE
   IF WVParameters.Length > MaximumLength& THEN WVParameters.Length = MaximumLength&

   Buffer$ = STRING$(&H7FFF, &H0)
   DataRead& = 0
 
   DO
    IF DataRead& + BufferSize > WVParameters.Length THEN Buffer$ = STRING$(WVParameters.Length - DataRead&, &H0)
   
    GET #FileH, , Buffer$

    Page = DataRead& \ EMSPAGESIZE
    Offset = DataRead& - Page * EMSPAGESIZE
   
    EMSCopyBaseToEMS LEN(Buffer$), VARSEG(Buffer$), SADD(Buffer$), WVParameters.EMSHandle, Offset, Page
    DataRead& = DataRead& + LEN(Buffer$)
   LOOP UNTIL DataRead& >= WVParameters.Length
   Buffer$ = ""
  ELSE
   Success = 0
  END IF
 CLOSE FileH

 WVLoad = Success
END FUNCTION

