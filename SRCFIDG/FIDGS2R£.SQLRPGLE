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
001900130218     d VALUE_IND       s              5i 0
002000900521     C*****************************************************************
002100091104     C*
002200130211     C     VIDEO         TAG
002300070111     c                   setoff                                       99
002400130211     C                   EXFMT     ficns2A
002500070111     C** CMD3 -
002600070111     C   KC              GOTO      FINE
002700070111     C* CONTROLLI
002800130211     C                   EXSR      CONTR_A
002900070111     C  NKF
003000130211     COR 99              GOTO      VIDEO
003100070111     C*
003200070111     C                   EXSR      aggiorna
003300070111     C*
003400070111     C     FINE          TAG
003500070111     C                   SETON                                        LR
003600070111     C*---------------------------------------------------------------*
003700070112     C*- - CONTROLLI video    -                                      -*
003800070112     C*---------------------------------------------------------------*
003900130211     C     CONTR_A       BEGSR
004000070112     C*
004100070112     C                   SETOFF                                       99
004200130211     C* Filiale
004300130211     C                   clear                   VIDFID
004400130211     C                   z-add     vidfil        Kfil              3 0
004500130211     C     kfil          CHAIN     azorg01L
004600130211     C                   IF        not %Found(azorg01L)
004700130211     C                   SETON                                        1099
004800130211     C                   GOTO      ENDCT1
004900130211    2C                   END
005000130211     C                   movel     orgDES        VIDFID
005100130211     C*
005200070112     C* Autista Conteggi
005300130211     C                   clear                   VIDPDD
005400070112     C     kapd          CHAIN     fiapd01L
005500070112     C                   IF        not %Found(fiapd01L)
005600130211     C                   SETON                                        1299
005700070112     C                   GOTO      ENDCT1
005800070112    2C                   END
005900130211     C                   MOVEL     APDRSc        VIDPDD
006000070112      *
006100070112     C* CONTROLLO SE ESISTE LA SELEZIONE del documento se immesso
006200070112     c                   if        vidndc > 0
006300130211      *
006400130211     C     kDST          CHAIN     fidst01l
006500130211     C                   IF        not %Found(fidst01L)
006600130211     C                   SETON                                        1199
006700130211     C                   GOTO      ENDCT1
006800130211    2C                   ElsE
006900130211      *  se ancora aperta NON si deve cancellare nulla
007000130211     c                   If        dstfcf <> 'S'
007100130211     C                   SETON                                        1399
007200130211     C                   GOTO      ENDCT1
007300130211     c                   end
007400130211    2C                   END
007500130211      *
007600130218      * *****
007700130213      *  Controlla se vi sono dati sul file da cancellare x l'autista scelto
007800130213     C     kPDC          setll     fipdc03t
007900130213     c                   if        not %equal(Fipdc03t)
008000130213      *
008100130213     c                   move      'N'           trovato           1
008200130213      *
008300130213      *   ma non dovrei nemmeno avere dei records negli altri archivi
008400130213      *   x dire che la selezione fatta non è corretta.
008500130213     C/EXEC SQL
008600130213     C+ SELECT  'S' into :trovato INDICATOR :VALUE_IND
008700130213     C+    FROM fipdo00T
008800130213     C+            WHERE PDOCODAUT = :vidPDR
008900130213     C+              AND PDOFGS    = :vidFIL
009000130213     C+              AND PDONDC    = :vidNDC
009100130213     C/END-EXEC
009200130218     c                   if           SQLCOD = 100 and VALUE_IND = 0
009300130218      *
009400130218      ** SE NON VI FOSSERO DATI:    il dubbio è che,
009500130218      **  comunque, per Distinta ci fosse inserito un altro autista
009600130218      **   rispetto a quello impostato a video
009700130218     C     kPDC_2        setll     fipdc03t
009800130218     c                   if        %equal(Fipdc03t)
009900130218      **  ci sono records FILIALE / DISTINTA  ma l'autista non è corretto
010000130218     C                   SETON                                        1699
010100130218     C                   GOTO      ENDCT1
010200130218      *
010300130218     c                   else
010400130218      *
010500130218     C     kPDC_2        setll     fipdo01t
010600130218     c                   if        %equal(Fipdo01t)
010700130218      **  ci sono records FILIALE / DISTINTA  ma l'autista non è corretto
010800130218     C                   SETON                                        1699
010900130218     C                   GOTO      ENDCT1
011000130218      *
011100130218     c                   else
011200130218      *
011300130218      ** ALTRIMENTI NON VI SONO DATI IN ASSOLUTO
011400130213     C                   SETON                                        1799
011500130213     C                   GOTO      ENDCT1
011600130218     c                   end
011700130218      **
011800130213     c                   end
011900130218     c                   end
012000130213      *
012100130213     c                   end
012200130218      * *****
012300070112     c                   else
012400130211     C                   SETON                                        1199
012500130211     C                   GOTO      ENDCT1
012600070112     c                   end
012700070112     C*
012800070112     C     ENDCT1        ENDSR
012900070112     C*---------------------------------------------------------------*
013000070112     C*   Aggiorna gli archivi
013100070112     C*---------------------------------------------------------------*
013200070112     C     Aggiorna      BegSR
013300070112     C*
013400130211     C* Testata
013500130211     C     kPDC          setll     fipdc03t
013600130211     C     kPDC          reade     fipdc03t
013700130211     C                   Dow       not %EoF(fipdc03t)
013800130211     C                   delete    fipdc000
013900130211     C     kPDC          reade     fipdc03t
014000130211     C                   Enddo
014100130218     C*
014200130218     C*  cancella i correlati
014300130218     c                   exsr      AGG_PDD_PDO
014400070112     C*
014500070112     C                   EndSR
014600130211     C*****************************************************************
014700130211     C*
014800070112     C*****************************************************************
014900130211     c     AGG_PDD_PDO   Begsr
015000130211     C*
015100130211     C*-----------
015200130211     C* Dettaglio
015300130218     C     kPDD_O        setll     fipdd01t
015400130218     C     kPDD_O        reade     fipdd01t
015500130211     C                   Dow       not %EoF(fipdd01t)
015600130211     C*
015700130212     c                   if        PDDCODAUT = vidpdr
015800130211     C                   delete    fipdd000
015900130212     C                   End
016000130211     C*
016100130218     C     kPDD_O        reade     fipdd01t
016200130211     C                   Enddo
016300130211     C*-----------
016400130211     C* ORM
016500130218     C     kPDD_O        setll     fipdo01t
016600130218     C     kPDD_O        reade     fipdo01t
016700130211     C                   Dow       not %EoF(fipdo01t)
016800130211     C*
016900130211     C* se l'Autista coincide
017000130211     c                   if        PDOCODAUT = vidpdr
017100130211     C                   delete    fipdo000
017200130211     C                   End
017300130211     C*
017400130218     C     kPDD_O        reade     fipdo01t
017500130211     C                   Enddo
017600130211     C*
017700130218     C                   EXFMT     TUTTOk
017800130218     C*
017900130211     C                   EndSR
018000130211     C*****************************************************************
018100070112     C     *inzsr        BegSR
018200070112      *
018300070112     C     *ENTRY        PLIST
018400070112     C                   PARM                    KPJBA
018500070112      *
018600070112     C                   Z-ADD     1             CODUT
018700070112     C                   CALL      'X§PARUT'
018800070112     C                   PARM                    UT§DSE
018900070112     C                   MOVEL     RAGUT         vidrsu
019000070112     C                   MOVEL     REC80         CNCR80
019100070112     C*---------------------------------------------------------------*
019200070112     C     kapd          KLIST
019300130211     C                   KFLD                    Ktip
019400130211     C                   KFLD                    VIDPDR
019500070112      *
019600130211     C     kDST          KLIST
019700130211     C                   KFLD                    TIPONPG           1 0
019800130211     C                   KFLD                    VIDNDC
019900130211     C                   KFLD                    VIDFIL
020000130211      *
020100130211     C     kPDC          KLIST
020200130211     C                   KFLD                    VIDFIL
020300130211     C                   KFLD                    VIDNDC
020400130211     C                   KFLD                    VIDPDR
020500130218      *
020600130218     C     kPDC_2        KLIST
020700130218     C                   KFLD                    VIDFIL
020800130218     C                   KFLD                    VIDNDC
020900130211      *
021000130218     C     kPDD_O        KLIST
021100130218     C                   KFLD                    VIDFIL
021200130218     C                   KFLD                    VIDNDC
021300130211      *
021400130211      *
021500070112     C                   move      'A'           Ktip              1
021600130211     C                   Z-ADD     4             TIPONPG           1 0
021700070112     C*
021800070112     C* LA DATA DEL GIORNO LA PRENDO DA TIME
021900070112     C                   TIME                    W0140            14 0
022000070112     C                   MOVE      W0140         UDATE8            8 0
022100070112     C*
022200070112     C                   EndSR
022300070112     C*---------------------------------------------------------------*
