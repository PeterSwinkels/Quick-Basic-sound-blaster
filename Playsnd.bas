DEFINT A-Z
DECLARE FUNCTION SBBaseAddress ()
DECLARE FUNCTION SBInitialize ()
DECLARE SUB SBOutputSample (BaseAddress, Sample)
DECLARE SUB SBReset (BaseAddress)
DECLARE SUB SBVolume (BaseAdress, LeftSide, RightSide)

 SCREEN 0
 WIDTH 80, 25
 COLOR 7, 0
 CLS

 BaseAddress = SBInitialize
 SBVolume BaseAddress, &HF, &HF

 FOR Length = 0 TO 100
  FOR Sample = 0 TO Length
   SBOutputSample BaseAddress, &H0
  NEXT Sample

  FOR Sample = 0 TO Length
   SBOutputSample BaseAddress, &HFF
  NEXT Sample
 NEXT Length

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

FUNCTION SBInitialize
 BaseAddress = SBBaseAddress
 SBReset BaseAddress

 SBInitialize = BaseAddress
END FUNCTION

SUB SBOutputSample (BaseAddress, Sample)
 WAIT BaseAddress + &HC, &H80, &H80
 OUT BaseAddress + &HC, &H10
 WAIT BaseAddress + &HC, &H80, &H80
 OUT BaseAddress + &HC, Sample
END SUB

SUB SBReset (BaseAddress)
 OUT BaseAddress + &H6, &H1
 OUT BaseAddress + &H6, &H0
END SUB

SUB SBVolume (BaseAddress, LeftSide, RightSide)
 OUT BaseAddress + &H4, &H22
 OUT BaseAddress + &H5, (LeftSide * &H10 OR RightSide)
END SUB

