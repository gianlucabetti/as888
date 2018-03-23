000100980127     H DECEDIT('0,') DATEDIT(*YMD.)
000200981106      *---------------------------------------------------------------------*
000300010926     FFLFG760F  IF   E             DISK
000400010926     F                                     RENAME(FLFG76:FLFG760)
000500001023     F*----------
000600010926     FFLFG765F  O  A E             DISK
000700001023      *----------------------------------------------------------------
000800010926     IFLFG760
000900010926     I              FG7AAS                      IN7AAS
001000010926     I              FG7PER                      IN7PER
001100010926     I              FG7FIL                      IN7FIL
001200010926     I              FG7GDA                      IN7GDA
001300010926     I              FG7GNA                      IN7GNA
001400010926     I              FG7GAR                      IN7GAR
001500010926     I              FG7GRA                      IN7GRA
001600010926     I              FG7DDI                      IN7DDI
001700010926     I              FG7DRO                      IN7DRO
001800010926     I              FG7DNI                      IN7DNI
001900010926     I              FG7GRD                      IN7GRD
002000010926     I              FG7GDC                      IN7GDC
002100010926     I              FG7GNC                      IN7GNC
002200010926     I              FG7GCO                      IN7GCO
002300010926     I              FG7GRC                      IN7GRC
002400010926     I              FG7GSS                      IN7GSS
002500010926     I              FG7GAP                      IN7GAP
002600010926     I              FG7AAR                      IN7AAR
002700010926     I              FG7AGR                      IN7AGR
002800010926     I              FG7DDE                      IN7DDE
002900010926     I              FG7DER                      IN7DER
003000010926     I              FG7GRE                      IN7GRE
003100010926     I              FG7FTR                      IN7FTR
003200010926     I              FG7DTR                      IN7DTR
003300981106      *---------------------------------------------------------------------*
003400980127      * Ciclo principale
003500980127      *---------------------------------------------------------------------*
003600010926     C                   Read      FlFg760F
003700001023     C*
003800010926     C                   DOW       not %EOF(FlFg760F)
003900001024     C*
004000010926     c                   eval      FG7AAS = 2000 + IN7AAS
004100010926     c                   eval      FG7PER = IN7PER
004200010926     c                   eval      FG7FIL = IN7FIL
004300010926     c                   eval      FG7GDA = IN7GDA
004400010926     c                   eval      FG7GNA = IN7GNA
004500010926     c                   eval      FG7GAR = IN7GAR
004600010926     c                   eval      FG7GRA = IN7GRA
004700010926     c                   eval      FG7DDI = IN7DDI
004800010926     c                   eval      FG7DRO = IN7DRO
004900010926     c                   eval      FG7DNI = IN7DNI
005000010926     c                   eval      FG7GRD = IN7GRD
005100010926     c                   eval      FG7GDC = IN7GDC
005200010926     c                   eval      FG7GNC = IN7GNC
005300010926     c                   eval      FG7GCO = IN7GCO
005400010926     c                   eval      FG7GRC = IN7GRC
005500010926     c                   eval      FG7GSS = IN7GSS
005600010926     c                   eval      FG7GAP = IN7GAP
005700010926     c                   eval      FG7AAR = IN7AAR
005800010926     c                   eval      FG7AGR = IN7AGR
005900010926     c                   eval      FG7DDE = IN7DDE
006000010926     c                   eval      FG7DER = IN7DER
006100010926     c                   eval      FG7GRE = IN7GRE
006200010926     c                   eval      FG7FTR = IN7FTR
006300010926     c                   eval      FG7DTR = IN7DTR
006400010926     C                   write     FlFg76
006500001023     C*
006600010926     C                   Read      FlFg760F
006700001023     C                   EndDo
006800001024     C*
006900980128     C                   EVAL      *INLR = *ON
007000001024     C*------------------------------------------------------------------------
