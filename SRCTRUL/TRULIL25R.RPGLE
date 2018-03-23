000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300010201
000400030926     C* /MAIN
000500030926     C*
000600030926     C* Dal segnacollo cliente genero il barcode dell'etichetta secondo lo "standard"
000700030926     C* INTERLEAVE 2/5
000800030926     C                   Z-ADD     *zeros        wDiv              2 0
000900030926     C                   Z-ADD     *zeros        wLung             2 0
001000030926     C                   MOVEL     *blanks       wBarcodeAlfa     30
001100030926     C                   EVAL      wBarcodeAlfa = %trim(INBARCODE)
001200030926     C                   EVAL      wLung = %len(%trim(wBarcodeAlfa))
001300030926     C                   EVAL      wDiv  = %div(wLung:2)
001400030926     C                   IF        wLung - (2*wDiv) = 0
001500030926     C                   EVAL      wBarcodeAlfa = '0'+%trim(wBarcodeAlfa)
001600030926     C                   ENDIF
001700030926     C                   Z-ADD     *zeros        wSommaCifre       8 0
001800030926     C                   Z-ADD     1             indice            1 0
001900030926     C                   Z-ADD     *zeros        wCifraNum         1 0
002000030926     C                   MOVEL     *blanks       wCifraAlfa        1
002100030929     C                   Z-ADD     *zeros        wChkDgtNum        2 0
002200030929     C                   MOVEL     *blanks       wChkDgtAlfa       1
002300030926     C                   EVAL      wLung = %len(%trim(wBarcodeAlfa))
002400030926     C                   DOW       indice <= wLung
002500030926     C                   EVAL      wCifraAlfa = %subst(wBarcodeAlfa:indice:1)
002600030926     C                   MOVEL     wCifraAlfa    wCifraNum
002700030926     C                   EVAL      wDiv  = %div(indice:2)
002800030926     C                   IF        indice - (2*wDiv) = 0
002900030926     C                   EVAL      wSommaCifre = wSommaCifre + wCifraNum
003000030926     C                   ELSE
003100030926     C                   EVAL      wSommaCifre = wSommaCifre + (wCifraNum*3)
003200030926     C                   ENDIF
003300030926     C                   ADD       1             indice
003400030926     C                   ENDDO
003500030926     C                   EVAL      wDiv  = %div(wSommaCifre:10)
003600030926     C                   EVAL      wChkDgtNum = 10 - (wSommaCifre-(10*wDiv))
003700030929     C                   MOVE      wChkDgtNum    wChkDgtAlfa
003800030929     C                   EVAL      wBarcodeAlfa = %trim(wBarcodeAlfa) +
003900030929     C                                            wChkDgtAlfa
004000030226     C*
004100030415     C* Valorizzo il barcode
004200030926     C                   EVAL      OUTBARCODE = %trim(wBarcodeAlfa)
004300030226     C*
004400030926     C                   SETON                                        LR
004500030926     C***
004600990910
004700030926
004800030926
004900030926     C***
005000000613     C     *inzsr        BEGSR
005100990910     C*
005200990910     C     *entry        plist
005300030929     C                   parm                    INBARCODE        10
005400030929     C                   parm                    OUTBARCODE       30
005500000830     C*
005600000613     C                   ENDSR
