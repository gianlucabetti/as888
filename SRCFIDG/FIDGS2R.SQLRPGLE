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
001700140214      *  pulizia FICAU
001800140214     d fnlrx4ds      e ds                  INZ
001900130211     H*--------------------------------------------------------------*
002000920810     D KPJBA         E DS
002100130218     d VALUE_IND       s              5i 0
002200900521     C*****************************************************************
002300091104     C*
002400130211     C     VIDEO         TAG
002500070111     c                   setoff                                       99
002600130211     C                   EXFMT     ficns2A
002700070111     C** CMD3 -
002800070111     C   KC              GOTO      FINE
002900070111     C* CONTROLLI
003000130211     C                   EXSR      CONTR_A
003100070111     C  NKF
003200130211     COR 99              GOTO      VIDEO
003300070111     C*
003400070111     C                   EXSR      aggiorna
003500070111     C*
003600070111     C     FINE          TAG
003700070111     C                   SETON                                        LR
003800070111     C*---------------------------------------------------------------*
003900070112     C*- - CONTROLLI video    -                                      -*
004000070112     C*---------------------------------------------------------------*
004100130211     C     CONTR_A       BEGSR
004200070112     C*
004300070112     C                   SETOFF                                       99
004400130211     C* Filiale
004500130211     C                   clear                   VIDFID
004600130211     C                   z-add     vidfil        Kfil              3 0
004700130211     C     kfil          CHAIN     azorg01L
004800130211     C                   IF        not %Found(azorg01L)
004900130211     C                   SETON                                        1099
005000130211     C                   GOTO      ENDCT1
005100130211    2C                   END
005200130211     C                   movel     orgDES        VIDFID
005300130211     C*
005400070112     C* Autista Conteggi
005500130211     C                   clear                   VIDPDD
005600070112     C     kapd          CHAIN     fiapd01L
005700070112     C                   IF        not %Found(fiapd01L)
005800130211     C                   SETON                                        1299
005900070112     C                   GOTO      ENDCT1
006000070112    2C                   END
006100130211     C                   MOVEL     APDRSc        VIDPDD
006200070112      *
006300070112     C* CONTROLLO SE ESISTE LA SELEZIONE del documento se immesso
006400070112     c                   if        vidndc > 0
006500130211      *
006600130211     C     kDST          CHAIN     fidst01l
006700130211     C                   IF        not %Found(fidst01L)
006800130211     C                   SETON                                        1199
006900130211     C                   GOTO      ENDCT1
007000130211    2C                   ElsE
007100130211      *  se ancora aperta NON si deve cancellare nulla
007200130211     c                   If        dstfcf <> 'S'
007300130211     C                   SETON                                        1399
007400130211     C                   GOTO      ENDCT1
007500130211     c                   end
007600130211    2C                   END
007700130211      *
007800130218      * *****
007900130213      *  Controlla se vi sono dati sul file da cancellare x l'autista scelto
008000130213     C     kPDC          setll     fipdc03t
008100130213     c                   if        not %equal(Fipdc03t)
008200130213      *
008300130213     c                   move      'N'           trovato           1
008400130213      *
008500130213      *   ma non dovrei nemmeno avere dei records negli altri archivi
008600130213      *   x dire che la selezione fatta non è corretta.
008700130213     C/EXEC SQL
008800130213     C+ SELECT  'S' into :trovato INDICATOR :VALUE_IND
008900130213     C+    FROM fipdo00T
009000130213     C+            WHERE PDOCODAUT = :vidPDR
009100130213     C+              AND PDOFGS    = :vidFIL
009200130213     C+              AND PDONDC    = :vidNDC
009300130213     C/END-EXEC
009400130218     c                   if           SQLCOD = 100 and VALUE_IND = 0
009500130218      *
009600130218      ** SE NON VI FOSSERO DATI:    il dubbio è che,
009700130218      **  comunque, per Distinta ci fosse inserito un altro autista
009800130218      **   rispetto a quello impostato a video
009900130218     C     kPDC_2        setll     fipdc03t
010000130218     c                   if        %equal(Fipdc03t)
010100130218      **  ci sono records FILIALE / DISTINTA  ma l'autista non è corretto
010200130218     C                   SETON                                        1699
010300130218     C                   GOTO      ENDCT1
010400130218      *
010500130218     c                   else
010600130218      *
010700130218     C     kPDC_2        setll     fipdo01t
010800130218     c                   if        %equal(Fipdo01t)
010900130218      **  ci sono records FILIALE / DISTINTA  ma l'autista non è corretto
011000130218     C                   SETON                                        1699
011100130218     C                   GOTO      ENDCT1
011200130218      *
011300130218     c                   else
011400130218      *
011500130218      ** ALTRIMENTI NON VI SONO DATI IN ASSOLUTO
011600130213     C                   SETON                                        1799
011700130213     C                   GOTO      ENDCT1
011800130218     c                   end
011900130218      **
012000130213     c                   end
012100130218     c                   end
012200130213      *
012300130213     c                   end
012400130218      * *****
012500070112     c                   else
012600130211     C                   SETON                                        1199
012700130211     C                   GOTO      ENDCT1
012800070112     c                   end
012900070112     C*
013000070112     C     ENDCT1        ENDSR
013100070112     C*---------------------------------------------------------------*
013200070112     C*   Aggiorna gli archivi
013300070112     C*---------------------------------------------------------------*
013400070112     C     Aggiorna      BegSR
013500070112     C*
013600130211     C* Testata
013700130211     C     kPDC          setll     fipdc03t
013800130211     C     kPDC          reade     fipdc03t
013900130211     C                   Dow       not %EoF(fipdc03t)
014000130211     C                   delete    fipdc000
014100130211     C     kPDC          reade     fipdc03t
014200130211     C                   Enddo
014300130218     C*
014400130218     C*  cancella i correlati
014500130218     c                   exsr      AGG_PDD_PDO
014600140214      ***
014700140214     C*  Pulizia del FICAU
014800140214      *
014900140214     c                   clear                   fnlrx4ds
015000140214     c                   eval      IX4FGS =  vidfil
015100140214     c                   eval      IX4NFV =  vidndc
015200140214     c                   call      'FNLRX4R'
015300140214     c                   parm                    kpjba
015400140214     c                   parm                    fnlrx4ds
015500070112     C*
015600070112     C                   EndSR
015700130211     C*****************************************************************
015800130211     C*
015900070112     C*****************************************************************
016000130211     c     AGG_PDD_PDO   Begsr
016100130211     C*
016200130211     C*-----------
016300130211     C* Dettaglio
016400130218     C     kPDD_O        setll     fipdd01t
016500130218     C     kPDD_O        reade     fipdd01t
016600130211     C                   Dow       not %EoF(fipdd01t)
016700130211     C*
016800130212     c                   if        PDDCODAUT = vidpdr
016900130211     C                   delete    fipdd000
017000130212     C                   End
017100130211     C*
017200130218     C     kPDD_O        reade     fipdd01t
017300130211     C                   Enddo
017400130211     C*-----------
017500130211     C* ORM
017600130218     C     kPDD_O        setll     fipdo01t
017700130218     C     kPDD_O        reade     fipdo01t
017800130211     C                   Dow       not %EoF(fipdo01t)
017900130211     C*
018000130211     C* se l'Autista coincide
018100130211     c                   if        PDOCODAUT = vidpdr
018200130211     C                   delete    fipdo000
018300130211     C                   End
018400130211     C*
018500130218     C     kPDD_O        reade     fipdo01t
018600130211     C                   Enddo
018700130211     C*
018800130218     C                   EXFMT     TUTTOk
018900130218     C*
019000130211     C                   EndSR
019100130211     C*****************************************************************
019200070112     C     *inzsr        BegSR
019300070112      *
019400070112     C     *ENTRY        PLIST
019500070112     C                   PARM                    KPJBA
019600070112      *
019700070112     C                   Z-ADD     1             CODUT
019800070112     C                   CALL      'X§PARUT'
019900070112     C                   PARM                    UT§DSE
020000070112     C                   MOVEL     RAGUT         vidrsu
020100070112     C                   MOVEL     REC80         CNCR80
020200070112     C*---------------------------------------------------------------*
020300070112     C     kapd          KLIST
020400130211     C                   KFLD                    Ktip
020500130211     C                   KFLD                    VIDPDR
020600070112      *
020700130211     C     kDST          KLIST
020800130211     C                   KFLD                    TIPONPG           1 0
020900130211     C                   KFLD                    VIDNDC
021000130211     C                   KFLD                    VIDFIL
021100130211      *
021200130211     C     kPDC          KLIST
021300130211     C                   KFLD                    VIDFIL
021400130211     C                   KFLD                    VIDNDC
021500130211     C                   KFLD                    VIDPDR
021600130218      *
021700130218     C     kPDC_2        KLIST
021800130218     C                   KFLD                    VIDFIL
021900130218     C                   KFLD                    VIDNDC
022000130211      *
022100130218     C     kPDD_O        KLIST
022200130218     C                   KFLD                    VIDFIL
022300130218     C                   KFLD                    VIDNDC
022400130211      *
022500130211      *
022600070112     C                   move      'A'           Ktip              1
022700130211     C                   Z-ADD     4             TIPONPG           1 0
022800070112     C*
022900070112     C* LA DATA DEL GIORNO LA PRENDO DA TIME
023000070112     C                   TIME                    W0140            14 0
023100070112     C                   MOVE      W0140         UDATE8            8 0
023200070112     C*
023300070112     C                   EndSR
023400070112     C*---------------------------------------------------------------*
