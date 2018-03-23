000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200130212     H*          ----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400130212     H*         -  ELIMINA Dati per PDA OBSOLETI                     *
000500000000     H*--------------------------------------------------------------*
000600130212     FfidgS2D   CF   E             WORKSTN
000700070112      *
000800130211     Fazorg01L  iF   E           K DISK
000900130211     Ffidst01L  iF   E           K DISK
001000130211     Ffiapd01L  iF   E           K DISK
001100130211     Ffipdc03t  uF   E           K DISK
001200130211     Ffipdd01t  uF   E           K DISK
001300130211     Ffipdo01t  uF   E           K DISK
001400070112     H*--------------------------------------------------------------*
001500130211     D CNCR80        E DS
001600130211     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001700130211     H*--------------------------------------------------------------*
001800920810     D KPJBA         E DS
001900900521     C*****************************************************************
002000091104     C*
002100130211     C     VIDEO         TAG
002200070111     c                   setoff                                       99
002300130211     C                   EXFMT     ficns2A
002400070111     C** CMD3 -
002500070111     C   KC              GOTO      FINE
002600070111     C* CONTROLLI
002700130211     C                   EXSR      CONTR_A
002800070111     C  NKF
002900130211     COR 99              GOTO      VIDEO
003000070111     C*
003100070111     C                   EXSR      aggiorna
003200070111     C*
003300070111     C     FINE          TAG
003400070111     C                   SETON                                        LR
003500070111     C*---------------------------------------------------------------*
003600070112     C*- - CONTROLLI video    -                                      -*
003700070112     C*---------------------------------------------------------------*
003800130211     C     CONTR_A       BEGSR
003900070112     C*
004000070112     C                   SETOFF                                       99
004100130211     C* Filiale
004200130211     C                   clear                   VIDFID
004300130211     C                   z-add     vidfil        Kfil              3 0
004400130211     C     kfil          CHAIN     azorg01L
004500130211     C                   IF        not %Found(azorg01L)
004600130211     C                   SETON                                        1099
004700130211     C                   GOTO      ENDCT1
004800130211    2C                   END
004900130211     C                   movel     orgDES        VIDFID
005000130211     C*
005100070112     C* Autista Conteggi
005200130211     C                   clear                   VIDPDD
005300070112     C     kapd          CHAIN     fiapd01L
005400070112     C                   IF        not %Found(fiapd01L)
005500130211     C                   SETON                                        1299
005600070112     C                   GOTO      ENDCT1
005700070112    2C                   END
005800130211     C                   MOVEL     APDRSc        VIDPDD
005900070112      *
006000070112     C* CONTROLLO SE ESISTE LA SELEZIONE del documento se immesso
006100070112     c                   if        vidndc > 0
006200130211      *
006300130211     C     kDST          CHAIN     fidst01l
006400130211     C                   IF        not %Found(fidst01L)
006500130211     C                   SETON                                        1199
006600130211     C                   GOTO      ENDCT1
006700130211    2C                   ElsE
006800130211      *  se ancora aperta NON si deve cancellare nulla
006900130211     c                   If        dstfcf <> 'S'
007000130211     C                   SETON                                        1399
007100130211     C                   GOTO      ENDCT1
007200130211     c                   end
007300130211    2C                   END
007400130211      *
007500070112     c                   else
007600130211     C                   SETON                                        1199
007700130211     C                   GOTO      ENDCT1
007800070112     c                   end
007900070112     C*
008000070112     C     ENDCT1        ENDSR
008100070112     C*---------------------------------------------------------------*
008200070112     C*   Aggiorna gli archivi
008300070112     C*---------------------------------------------------------------*
008400070112     C     Aggiorna      BegSR
008500070112     C*
008600130211     C* Testata
008700130211     C     kPDC          setll     fipdc03t
008800130211     C     kPDC          reade     fipdc03t
008900130211     C                   Dow       not %EoF(fipdc03t)
009000130211     C*
009100130211     C*  cancella i correlati
009200130211     c                   exsr      AGG_PDD_PDO
009300130211     C*
009400130211     C                   delete    fipdc000
009500130211     C*
009600130211     C     kPDC          reade     fipdc03t
009700130211     C                   Enddo
009800070112     C*
009900070112     C                   EndSR
010000130211     C*****************************************************************
010100130211     C*
010200070112     C*****************************************************************
010300130211     c     AGG_PDD_PDO   Begsr
010400130211     C*
010500130211     C*-----------
010600130211     C* Dettaglio
010700130211     C     kPDD          setll     fipdd01t
010800130211     C     kPDD          reade     fipdd01t
010900130211     C                   Dow       not %EoF(fipdd01t)
011000130211     C*
011100130212     c                   if        PDDCODAUT = vidpdr
011200130211     C                   delete    fipdd000
011300130212     C                   End
011400130211     C*
011500130211     C     kPDD          reade     fipdd01t
011600130211     C                   Enddo
011700130211     C*-----------
011800130211     C* ORM
011900130211     C     kPDO          setll     fipdo01t
012000130211     C     kPDO          reade     fipdo01t
012100130211     C                   Dow       not %EoF(fipdo01t)
012200130211     C*
012300130211     C* se l'Autista coincide
012400130211     c                   if        PDOCODAUT = vidpdr
012500130211     C                   delete    fipdo000
012600130211     C                   End
012700130211     C*
012800130211     C     kPDO          reade     fipdo01t
012900130211     C                   Enddo
013000130211     C*
013100130211     C                   EndSR
013200130211     C*****************************************************************
013300070112     C     *inzsr        BegSR
013400070112      *
013500070112     C     *ENTRY        PLIST
013600070112     C                   PARM                    KPJBA
013700070112      *
013800070112     C                   Z-ADD     1             CODUT
013900070112     C                   CALL      'X§PARUT'
014000070112     C                   PARM                    UT§DSE
014100070112     C                   MOVEL     RAGUT         vidrsu
014200070112     C                   MOVEL     REC80         CNCR80
014300070112     C*---------------------------------------------------------------*
014400070112     C     kapd          KLIST
014500130211     C                   KFLD                    Ktip
014600130211     C                   KFLD                    VIDPDR
014700070112      *
014800130211     C     kDST          KLIST
014900130211     C                   KFLD                    TIPONPG           1 0
015000130211     C                   KFLD                    VIDNDC
015100130211     C                   KFLD                    VIDFIL
015200130211      *
015300130211     C     kPDC          KLIST
015400130211     C                   KFLD                    VIDFIL
015500130211     C                   KFLD                    VIDNDC
015600130211     C                   KFLD                    VIDPDR
015700130211      *
015800130211     C     kPDD          KLIST
015900130211     C                   KFLD                    PDCFGS
016000130211     C                   KFLD                    PDCNDC
016100130211      *
016200130211     C     kPDO          KLIST
016300130211     C                   KFLD                    PDCFGS
016400130211     C                   KFLD                    PDCNDC
016500130211      *
016600070112     C                   move      'A'           Ktip              1
016700130211     C                   Z-ADD     4             TIPONPG           1 0
016800070112     C*
016900070112     C* LA DATA DEL GIORNO LA PRENDO DA TIME
017000070112     C                   TIME                    W0140            14 0
017100070112     C                   MOVE      W0140         UDATE8            8 0
017200070112     C*
017300070112     C                   EndSR
017400070112     C*---------------------------------------------------------------*
