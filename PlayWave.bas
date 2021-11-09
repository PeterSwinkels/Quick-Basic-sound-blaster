DEFINT A-Z

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

DECLARE FUNCTION SBBaseAddress ()
DECLARE FUNCTION SBReset (BaseAddress)
DECLARE SUB Delay (Interval AS SINGLE)
DECLARE SUB SBOutputSample (BaseAddress, Sample)
DECLARE SUB SBVolume (BaseAdress, LeftSide, RightSide)
DECLARE SUB WVDisplayHeader (WVHeader AS WVHeaderStr, WVFact AS WVFactStr, WVData AS WVDataStr)
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

SUB WVDisplayHeader (WVHeader AS WVHeaderStr, WVFact AS WVFactStr, WVData AS WVDataStr)
 PRINT "RIFF ID: "; WVHeader.RIFFID
 PRINT "File size: "; WVHeader.RIFFSize + 8
 PRINT "WAVE ID: "; WVHeader.WAVEID
 PRINT "Format ID: "; WVHeader.FormatID
 PRINT "Format size: "; WVHeader.FormatSize
 PRINT "Format: "; WVHeader.Format
 PRINT "Channel count: "; WVHeader.Channels
 PRINT "Frequency: "; WVHeader.Frequency
 PRINT "Byte rate: "; WVHeader.ByteRate
 PRINT "Alignment: "; WVHeader.Alignment
 PRINT "Bits per sample: "; WVHeader.BitsPerSample

 PRINT "Padding: "; WVFact.Padding
 PRINT "Fact ID: "; WVFact.FactID
 PRINT "Fact size: "; WVFact.FactSize
 PRINT "Fact data size: "; WVFact.FactDataSize

 PRINT "Data ID: "; WVData.DataID
 PRINT "Data size: "; WVData.DataSize
END SUB

SUB WVPlay (WVFile$, BaseAddress)
DIM WVData AS WVDataStr
DIM WVFact AS WVFactStr
DIM WVHeader AS WVHeaderStr

 PRINT "Playing: "; WVFile$

 FileH = FREEFILE
 OPEN WVFile$ FOR INPUT AS FileH: CLOSE FileH

 FileH = FREEFILE
 OPEN WVFile$ FOR BINARY AS FileH
  GET #FileH, , WVHeader
  IF WVHeader.FormatSize = 18 THEN GET #FileH, , WVFact
  GET #FileH, , WVData
 
  IF WVHeader.RIFFID = "RIFF" AND WVHeader.BitsPerSample = 8 AND WVHeader.Channels = 1 AND WVHeader.Format = &H1 AND WVHeader.Frequency <= 32000 THEN
   WVDisplayHeader WVHeader, WVFact, WVData

   DO UNTIL LOC(FileH) >= LOF(FileH)
    Chunk$ = ""
    Chunk$ = INPUT$(WVHeader.Frequency, FileH)
    FOR Sample = 1 TO LEN(Chunk$)
     SBOutputSample BaseAddress, ASC(MID$(Chunk$, Sample, 1))
    NEXT Sample
   LOOP
  ELSE
   PRINT WVFile$; " is not a 8-bit 8000-32000 bytes/second PCM mono wave file."
  END IF
 CLOSE FileH

END SUB

