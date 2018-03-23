000100920317     H DATEDIT(*YMD)
000200020703     F* TNVR38R *----------------------------------------------------*
000300020703     F*           TRASCODIFICA FILES POTENZIALI                      *
000400981012     F* -------------------------------------------------------------*
000500020708     FTNCPI01L  UF   E           K DISK
000600020710     FTNCPS11L  UF   E           K DISK
000700020708     FTFNTC01L  UF   E           K DISK
000800020705     FTNCPO01L  IF   E           K DISK
000900920415     C*----------------------------------------------------------------*
001000941111     C* MAIN LINE
001100941111     C*----------------------------------------------------------------*
001200020710     c     *entry        plist
001300020710     C                   PARM                    SOLONOTE          1
001400020710      *
001500020710     c                   IF        SOLONOTE = *blanks
001600020710      *
001700020710      * TNCPI
001800020710      *
001900020710     C                   move      'P'           Apl               1
002000020710     C     APl           setll     tncpi01l
002100020710     C     Apl           reade     tncpi01l
002200020705      *
002300020710     C                   dow       not %EOF(TNCPI01L)
002400020704      *
002500020710     C                   IF        Wcpink1 <> Cpink1
002600020710     C                   movel     CpiNk1        Wcpink1          15
002700020710     C                   movel     CpiNk1        $campo11         11
002800020710     C                   move      $campo11      CpoCpo
002900020710     C     Cpocpo        chain     TnCpo01l                           32
003000020710     c                   ENDIF
003100020710      *
003200020710      * cancello record se non trovata la testata
003300020710     C   32              delete    TNCPI000
003400020710     C     Apl           reade     tncpi01l
003500020708   3-C                   enddo
003600020705      *
003700020710      * TNCPS
003800020710      *
003900020710     C     *loval        setll     tncps11l
004000020710     C                   read      tncps11l
004100020708      *
004200020710     C                   dow       NOT  %EOF(TNCPS11L)
004300020710     C                   IF        Wcpscpo <> Cpscpo
004400020710     c                              or cpscpo = *zeros
004500020710     C                   z-add     Cpscpo        Wcpscpo          11 0
004600020710     C     CpsCpo        chain     TnCpo01l                           32
004700020710     c                   ENDIF
004800020710      * pulisco
004900020710     C   32              delete    TNCPs000
005000020710   3- *
005100020710     C                   read      tncps11l
005200020708   3-C                   enddo
005300020708      *
005400020710     c                   ENDIF
005500020710      *
005600020710      * NOTE
005700020710      *
005800020710     C                   MOVE      'P'           NTCAPL
005900020710     C     NTCAPL        SETLL     TFNTC01L
006000020710     C     NTCAPL        READE     TFNTC01L
006100020710      * 30 on - Fine lettura
006200020710     C                   DOW       not %EOF(TFNTC01L)
006300020710      *
006400020710     C                   IF        Wntcnk1 <> ntcnk1
006500020710     C                   movel     ntcNk1        Wntcnk1          11
006600020710     C                   movel     NtcNk1        $campo11         11
006700020710     C                   move      $campo11      CpoCpo
006800020710     C     CpoCpo        chain     TnCpo01l                           32
006900020710     c                   ENDIF
007000020710      *
007100020710     C   32              DELETE    TFNTC01L
007200020710      *
007300020710     C     NTCAPL        READE     TFNTC01L
007400020710     C                   ENDDO
007500020710      *
007600020710     C                   eval      *inlr = *on
