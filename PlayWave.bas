DEFINT A-Z

TYPE WVHeaderStr
 RIFF AS STRING * 4
 FileSize AS LONG
 WAVE AS STRING * 4
 FMT AS STRING * 4
 FMTSize AS LONG
 Format AS INTEGER
 Channels AS INTEGER
 SampleRate AS LONG
 ByteRate AS LONG
 Align AS INTEGER
 BitsPerSample AS LONG
 DataV AS STRING * 4
 DataSize AS LONG
END TYPE

DECLARE FUNCTION SBBaseAddress ()
DECLARE FUNCTION SBReset (BaseAddress)
DECLARE SUB Delay (Interval AS SINGLE)
DECLARE SUB SBOutputSample (BaseAddress, Sample)
DECLARE SUB SBVolume (BaseAdress, LeftSide, RightSide)
DECLARE SUB WVDisplayHeader (Header AS WVHeaderStr)
DECLARE SUB WVPlay (WVFile$, BaseAddress)

 SCREEN 0
 WIDTH 80, 25
 COLOR 7, 0
 CLS

 BaseAddress = SBBaseAddress

 IF SBReset(BaseAddress) THEN
  SBVolume BaseAddress, &HF, &HF
  WVPlay COMMAND$, BaseAddress
 ELSE
  PRINT "No Sound Blaster detected at "; HEX$(BaseAddress); "h."
 END IF

SUB Delay (Interval AS SINGLE)
DIM StartTime AS SINGLE

 StartTime = TIMER
 DO: LOOP UNTIL TIMER >= StartTime + Interval OR TIMER <= Interval
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

SUB SBOutputSample (BaseAddress, Sample)
 WAIT BaseAddress + &HC, &H80, &H80
 OUT BaseAddress + &HC, &H10
 WAIT BaseAddress + &HC, &H80, &H80
 OUT BaseAddress + &HC, Sample
END SUB

FUNCTION SBReset (BaseAddress)
 OUT BaseAddress + &H6, &H1
 OUT BaseAddress + &H6, &H0

 Delay .03

 SBReset = ((INP(BaseAddress + &HE) AND &H80) = &H80) AND (INP(BaseAddress + &HA) = &HAA)
END FUNCTION

SUB SBVolume (BaseAddress, LeftSide, RightSide)
 OUT BaseAddress + &H4, &H22
 OUT BaseAddress + &H5, (LeftSide * &H10 OR RightSide)
END SUB

SUB WVDisplayHeader (Header AS WVHeaderStr)
 PRINT "RIFF: "; Header.RIFF
 PRINT "File size: "; Header.FileSize + 8
 PRINT "Wave: "; Header.WAVE
 PRINT "Format: "; Header.FMT
 PRINT "Format size: "; Header.FMTSize
 PRINT "Format type: "; Header.Format
 PRINT "Channel count: "; Header.Channels
 PRINT "Sample rate: "; Header.SampleRate
 PRINT "Byte rate: "; Header.ByteRate
 PRINT "Align: "; Header.Align
 PRINT "Bits per sample: "; Header.BitsPerSample
 PRINT "Data: "; Header.DataV
 PRINT "Data size: "; Header.DataSize
END SUB

SUB WVPlay (WVFile$, BaseAddress)
DIM Header AS WVHeaderStr

 PRINT "Playing: "; WVFile$

 FileH = FREEFILE
 OPEN WVFile$ FOR INPUT AS FileH: CLOSE FileH

 FileH = FREEFILE
 OPEN WVFile$ FOR BINARY AS FileH
  GET #FileH, , Header
 
  IF Header.RIFF = "RIFF" AND Header.BitsPerSample = 8 AND Header.Channels = 1 AND Header.Format = &H1 AND Header.SampleRate <= 32000 THEN
   WVDisplayHeader Header

   DO UNTIL LOC(FileH) >= LOF(FileH)
    Chunk$ = ""
    Chunk$ = INPUT$(Header.SampleRate, FileH)
    FOR Sample = 1 TO LEN(Chunk$)
     SBOutputSample BaseAddress, ASC(MID$(Chunk$, Sample, 1))
    NEXT Sample
   LOOP
  ELSE
   PRINT WVFile$; " is not a 8-bit 8000-32000 bytes/second PCM mono wave file."
  END IF
 CLOSE FileH

END SUB

