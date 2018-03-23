000100991116     H*------------------------------------------------------------------------*
000200991116     H* SIMULAZIONE CONTO ECONOMICO - RICHIESTA PARAMETRI                      *
000300991116     H*------------------------------------------------------------------------*
000400991116     H DECEDIT('0,') DATEDIT(*DMY.)
000500991116     F*------------------------------------------------------------------------*
000600991116     F* DATA BASE
000700991116     F*------------------------------------------------------------------------*
000800091012     FTRECs10D  CF   E             WORKSTN INFDS(DEVDS1)
000900991116     F                                     SFILE(EC10DVM:MSGULT)
001000991116     FAZORG01L  IF   E           K DISK
001100991116     FCNACO00F  IF   E           K DISK
001200991116     D*------------------------------------------------------------------------*
001300991116     D* SCHIERE
001400991116     D*------------------------------------------------------------------------*
001500991116     D*-------------------
001600991116     D* SCHIERE A TEMPO DI COMPILAZIONE  PER VIDEO
001700991116     D*-------------------
001800991116     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001900991116     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
002000991116     D*-------------------
002100991116     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002200991116     D*-------------------
002300991116     D FFU             S              6    DIM(31)
002400991116     D FDE             S             19    DIM(31)
002500991116     D FAT             S              1    DIM(31)
002600991116     D*-------------------
002700991116     D* TABELLA DESCRIZIONI A VIDEO
002800991116     D*-------------------
002900991116     D FUV             S             19    DIM(8)
003000991116     D*-------------------
003100991116     D* TABELLA TASTI FUNZIONALI PREMUTI
003200991116     D*-------------------
003300991116     D KEYS            S              1    DIM(40)
003400991116     D*------------------
003500991116     D* TABELLE CAPICONTO
003600991116     D*------------------
003700991116     D RI              S              4  0 DIM(4)
003800991116     D*------------------------------------------------------------------------*
003900991116     D* INPUT
004000991116     D*------------------------------------------------------------------------*
004100991116     D*-------------------
004200991116     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004300991116     D*-------------------
004400991116     D DSFUN         E DS
004500991116     D  FUNRIG                 1     40
004600991116     D*-------------------
004700991116     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004800991116     D*-------------------
004900991116     D DSMSG         E DS
005000991116     D*-------------------
005100991116     D* DS PER GESTIONE TASTI FUNZIONALI
005200991116     D*-------------------
005300991116     D DSKEYS        E DS
005400991116     D  KEYRIG                 1     40
005500991116     D*-------------------
005600991116     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005700991116     D*-------------------
005800991116     D                SDS
005900991116     D  USER                 254    263
006000991116     D*-------------------
006100991116     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006200991116     D*-------------------
006300991116     D DEVDS1          DS
006400991116     D  KEY                  369    369
006500991116     D  CURSOR               370    371
006600991116     D*-------------------
006700991116     D* ARCHITETTURA
006800991116     D*-------------------
006900991116     D KPJBA         E DS
007000991116     D*-------------------
007100020923     D* DS REPERIMENTO DATI UTENTE
007200020923     D*-------------------
007300020923     D TIBS34DS      E DS
007400020923     D DDATIUTE      E DS
007500020923     D AZUTEDS       E DS                  extname(AZUTE00F)
007600991116     D*-------------------
007700991116     D* CONTROLLO DATA (8)
007800991116     D*-------------------
007900991116     D WLBDA8          DS                  INZ
008000991116     D  G08DAT                 1      8  0
008100991116     D  G08INV                 9     16  0
008200991116     D  G08ERR                17     17
008300991116     D  G08TGI                18     22  0
008400991116     D*-------------------
008500991116     D* PARAMETRI LANCIO
008600991116     D*-------------------
008700991116     D PARAM           DS                  INZ
008800991116     D  PARORD                 1      1
008900991116     D  PARFIL                 2      4  0
009000991116     D  PARKS1                 5     11  0
009100991116     D  PARDS1                12     19  0
009200991116     D  PARDS2                20     27  0
009300991116     D  PARDF1                28     35  0
009400991116     D  PARDF2                36     43  0
009500991116     D  PARKS2                44     50  0
009600991116     D  PARASI                51     54  0
009700110228     D  PARLIB                55     64
009800991116     C*------------------------------------------------------------------------*
009900991116     C* MAIN LINE
010000991116     C*------------------------------------------------------------------------*
010100991116     C*
010200991116     C* CICLO FINO A CHE:
010300991116DO  1C     $FINE         DOUEQ     '1'
010400991116     C*
010500991116     C                   MOVEL     '0'           $ERR
010600991116     C*
010700991116     C* CONTROLLA GESTIONE VIDEO
010800991116IF  2C     $GES          IFEQ      'V1'
010900991116     C                   EXSR      GESV1
011000991116E   2C                   ENDIF
011100991116     C*
011200991116E   1C                   ENDDO
011300991116     C*
011400991116     C                   SETON                                        LR
011500991116     C*------------------------------------------------------------------------*
011600991116     C* GESV1 - GESTIONE PRIMO VIDEO
011700991116     C*------------------------------------------------------------------------*
011800991116     C     GESV1         BEGSR
011900991116     C*
012000991116IF  1C     $TESVD        IFEQ      '1'
012100991116     C                   EXSR      CARV1
012200991116E   1C                   ENDIF
012300991116     C*
012400991116IF  1C     MSGULT        IFEQ      *ZEROS
012500991116     C                   MOVEL     *BLANKS       $MSG
012600991116     C                   EXSR      CARMSG
012700991116E   1C                   ENDIF
012800991116     C                   EXSR      OUTMSG
012900991116     C*
013000991116     C                   EXFMT     EC10DV1
013100991116     C*
013200991116     C                   EXSR      CONFUN
013300991116     C*
013400991116IF  1C     $ERR          IFEQ      '0'
013500991116SL  2C                   SELECT
013600991116     C     KEY           WHENEQ    ENTER
013700991116     C                   EXSR      ENTV1
013800991116     C     KEY           WHENEQ    F6
013900991116     C                   EXSR      F06GES
014000110314     C     KEY           WHENEQ    F8
014100110314     C                   EXSR      F06GES
014200991116     C                   OTHER
014300991116     C                   EXSR      FUNSTD
014400991116E   2C                   ENDSL
014500991116E   1C                   ENDIF
014600991116     C*
014700991116     C                   ENDSR
014800991116     C*------------------------------------------------------------------------*
014900991116     C* CARV1 - CARICA PRIMO VIDEO
015000991116     C*------------------------------------------------------------------------*
015100991116     C     CARV1         BEGSR
015200991116     C*
015300991116     C                   MOVEL     'V1'          $GES
015400991116     C*
015500991116     C                   EXSR      CARFUN
015600991116     C                   EXSR      OUTFUN
015700991116     C*
015800991116     C                   WRITE     EC10DVT
015900991116     C                   WRITE     EC10DVF
016000991116     C*
016100991116     C                   ENDSR
016200991116     C*------------------------------------------------------------------------*
016300991116     C* ENTV1 - GESTIONE TASTO ENTER PRIMO VIDEO
016400991116     C*------------------------------------------------------------------------*
016500991116     C     ENTV1         BEGSR
016600991116     C*
016700991116     C* CONTROLLA I CAMPI IMMESSI
016800991116     C                   EXSR      DECODI
016900991116     C*
017000991116     C                   ENDSR
017100991116     C*------------------------------------------------------------------------*
017200991116     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
017300991116     C*------------------------------------------------------------------------*
017400991116     C     F06GES        BEGSR
017500991116     C*
017600991116     C* CONTROLLA I CAMPI IMMESSI
017700991116     C                   EXSR      DECODI
017800991116     C*
017900110228     C* CONTROLLA Libreria e file sui quali simulare
018000110228     C                   EXSR      srwin
018100110228     C*
018200991116     C* IMPOSTA LA LISTA PARAMETRI E LANCIA IL PROGRAMMA
018300991116IF  1C     $ERR          IFEQ      '0'
018400991116     C                   MOVEL     V1CORD        PARORD
018500991116     C                   Z-ADD     V1CFIL        PARFIL
018600991116     C                   Z-ADD     V1CKS1        PARKS1
018700991116     C                   Z-ADD     V1CKS2        PARKS2
018800991116     C                   Z-ADD     DS1V1C        PARDS1
018900991116     C                   Z-ADD     DS2V1C        PARDS2
019000991116     C                   Z-ADD     DF1V1C        PARDF1
019100991116     C                   Z-ADD     DF2V1C        PARDF2
019200100511     C                   Z-ADD     v1casi        PARASI
019300110228     C*                  MOVEL     V1CCLR        PARCLR
019400110228     C                   MOVEL     lib           PARlib
019500991116     C* LANCIO
019600991116     C                   MOVEL     PARAM         KPJBU
019700110314     C     KEY           ifeq      F8
019800091016     C                   MOVEL     'ECS1'        KCOAZ
019900110314     C                   CALL      'BCH10'
020000110314     C                   PARM                    KPJBA
020100110314     c                   end
020200110314     C     KEY           ifeq      F6
020300110314     c                   call      'TRECS11C'
020400100422     C                   PARM                    KPJBA
020500110314     c                   end
020600991116E   1C                   ENDIF
020700991116     C*
020800991116     C                   ENDSR
020900991116     C*------------------------------------------------------------------------*
021000110228     C* richiede CONTROLLA CAMPI E DECODIFICA
021100991116     C*------------------------------------------------------------------------*
021200110228     C     srwin         BEGSR
021300991116     C*
021400110228     c                   call      'TRECS1R1'
021500110228     c                   parm                    lib              10
021600110228     c                   parm                    err               1
021700110228     c                   if        err <> ' '
021800110228     c                   eval      $err= err
021900110228     c                   select
022000110228     c                   when      err = '1'
022100110228     C                   eval      $MSG = 'Errore nella libreria sulla quale -
022200110228     c                             simulare'
022300110228     c                   when      err = '2'
022400110228     C                   eval      $MSG = 'Errore nella duplica dei file sui -
022500110228     c                             quali simulare'
022600110228     c                   when      err = '3'
022700110228     C                   eval      $MSG = 'Errore negli oggetti file sui -
022800110228     c                             quali simulare'
022900110228     c                   when      err = '1'
023000110228     C                   eval      $MSG = 'Errore nella pulizia dei file sui -
023100110228     c                             quali simulare'
023200110228     c                   endsl
023300110228     C                   EXSR      CARMSG
023400110228     c                   else
023500110228     c                   eval      $err= '0'
023600110228     c                   end
023700110228     C*
023800110228     C                   ENDSR
023900110228     C*------------------------------------------------------------------------*
024000110228     C* DECODI - CONTROLLA CAMPI E DECODIFICA
024100110228     C*------------------------------------------------------------------------*
024200110228     C     DECODI        BEGSR
024300110228     C*
024400991116     C* REIMPOSTA INIDICATORI
024500991116     C                   SETOFF                                       606162
024600991116     C                   SETOFF                                       636465
024700991116     C                   SETOFF                                       666768
024800991117     C                   SETOFF                                       6970
024900991116     C*
025000991116DO  1C                   DO
025100991116     C*---
025200991116     C* ORDINAMENTO
025300991116     C*---
025400991116IF  2C     V1CORD        IFEQ      '1'
025500991116     C     V1CORD        OREQ      '2'
025600991116     C     V1CORD        OREQ      '3'
025700991116X   2C                   ELSE
025800991116     C                   MOVEL     '1'           $ERR
025900991116     C                   MOVEL     MSG(10)       $MSG
026000991116     C                   EXSR      CARMSG
026100991116     C                   SETON                                        60
026200991116     C                   LEAVE
026300991116E   2C                   ENDIF
026400991116     C*---
026500991116     C* SELEZIONI
026600991116     C*---
026700991116     C* FILIALE
026800991116IF  2C     V1CFIL        IFGT      *ZEROS
026900991116     C                   Z-ADD     V1CFIL        WFIL
027000991116     C                   MOVEL     *BLANKS       WDFIL
027100991116     C                   EXSR      CHKFIL
027200991116     C                   MOVEL     WDFIL         V1DFIL
027300991116IF  3C     $ERR          IFEQ      '1'
027400991116     C                   MOVEL     '1'           $ERR
027500991116     C                   MOVEL     MSG(11)       $MSG
027600991116     C                   EXSR      CARMSG
027700991116     C                   SETON                                        61
027800991116     C                   LEAVE
027900991116E   3C                   ENDIF
028000991116X   2C                   ELSE
028100991116IF  3C     V1CORD        IFEQ      '1'
028200991116     C                   MOVEL     '1'           $ERR
028300991116     C                   MOVEL     MSG(16)       $MSG
028400991116     C                   EXSR      CARMSG
028500991116     C                   SETON                                        61
028600991116     C                   LEAVE
028700991116E   3C                   ENDIF
028800991116E   2C                   ENDIF
028900991116     C* CLIENTI
029000991116IF  2C     V1CKS1        IFGT      *ZEROS
029100991116     C                   Z-ADD     V1CKS1        WKSC
029200991116     C                   MOVEL     *BLANKS       WDKSC
029300991116     C                   EXSR      CHKKSC
029400991116     C                   MOVEL     WDKSC         V1DKS1
029500991116X   2C                   ELSE
029600991116IF  3C     V1CORD        IFEQ      '2'
029700991116     C                   MOVEL     '1'           $ERR
029800991116     C                   MOVEL     MSG(17)       $MSG
029900991116     C                   EXSR      CARMSG
030000991116     C                   SETON                                        62
030100991116     C                   LEAVE
030200991116E   3C                   ENDIF
030300991116E   2C                   ENDIF
030400991116     C*
030500991116IF  2C     V1CKS2        IFGT      *ZEROS
030600991116     C                   Z-ADD     V1CKS2        WKSC
030700991116     C                   MOVEL     *BLANKS       WDKSC
030800991116     C                   EXSR      CHKKSC
030900991116     C                   MOVEL     WDKSC         V1DKS2
031000991116X   2C                   ELSE
031100991116IF  3C     V1CORD        IFEQ      '2'
031200991116     C                   MOVEL     '1'           $ERR
031300991116     C                   MOVEL     MSG(17)       $MSG
031400991116     C                   EXSR      CARMSG
031500991116     C                   SETON                                        67
031600991116     C                   LEAVE
031700991116E   3C                   ENDIF
031800991116E   2C                   ENDIF
031900991116     C*
032000991116IF  2C     V1CKS2        IFLT      V1CKS1
032100991116     C                   MOVEL     '1'           $ERR
032200991116     C                   MOVEL     MSG(14)       $MSG
032300991116     C                   EXSR      CARMSG
032400991116     C                   SETON                                        6267
032500991116     C                   LEAVE
032600991116E   2C                   ENDIF
032700991116     C* DATA SPEDIZIONE
032800991116     C     *LIKE         DEFINE    V1CDS1        DS1V1C
032900991116     C                   Z-ADD     *ZEROS        DS1V1C
033000991116IF  2C     V1CDS1        IFGT      *ZEROS
033100991116     C                   Z-ADD     V1CDS1        G08DAT
033200991116     C                   Z-ADD     *ZEROS        G08INV
033300991116     C                   MOVEL     '0'           G08ERR
033400991116     C                   CALL      'XSRDA8'
033500991116     C                   PARM                    WLBDA8
033600991116     C                   Z-ADD     G08DAT        V1CDS1
033700991116     C                   Z-ADD     G08INV        DS1V1C
033800991116IF  3C     G08ERR        IFEQ      '1'
033900991116     C                   MOVEL     '1'           $ERR
034000991116     C                   MOVEL     MSG(13)       $MSG
034100991116     C                   EXSR      CARMSG
034200991116     C                   SETON                                        63
034300991116     C                   LEAVE
034400991116E   3C                   ENDIF
034500991116X   2C                   ELSE
034600991116     C                   MOVEL     '1'           $ERR
034700991116     C                   MOVEL     MSG(18)       $MSG
034800991116     C                   EXSR      CARMSG
034900991116     C                   SETON                                        63
035000991116     C                   LEAVE
035100991116E   2C                   ENDIF
035200991116     C*
035300991116     C     *LIKE         DEFINE    V1CDS2        DS2V1C
035400991116     C                   Z-ADD     *ZEROS        DS2V1C
035500991116IF  2C     V1CDS2        IFGT      *ZEROS
035600991116     C     V1CDS1        ORGT      *ZEROS
035700991116     C                   Z-ADD     V1CDS2        G08DAT
035800991116     C                   Z-ADD     *ZEROS        G08INV
035900991116     C                   MOVEL     '0'           G08ERR
036000991116     C                   CALL      'XSRDA8'
036100991116     C                   PARM                    WLBDA8
036200991116     C                   Z-ADD     G08DAT        V1CDS2
036300991116     C                   Z-ADD     G08INV        DS2V1C
036400991116IF  3C     G08ERR        IFEQ      '1'
036500991116     C                   MOVEL     '1'           $ERR
036600991116     C                   MOVEL     MSG(13)       $MSG
036700991116     C                   EXSR      CARMSG
036800991116     C                   SETON                                        64
036900991116     C                   LEAVE
037000991116E   3C                   ENDIF
037100991116X   2C                   ELSE
037200991116     C                   MOVEL     '1'           $ERR
037300991116     C                   MOVEL     MSG(18)       $MSG
037400991116     C                   EXSR      CARMSG
037500991116     C                   SETON                                        64
037600991116     C                   LEAVE
037700991116E   2C                   ENDIF
037800991116     C*
037900991116IF  2C     DS1V1C        IFGT      DS2V1C
038000991116     C                   MOVEL     '1'           $ERR
038100991116     C                   MOVEL     MSG(14)       $MSG
038200991116     C                   EXSR      CARMSG
038300991116     C                   SETON                                        6364
038400991116     C                   LEAVE
038500991116E   2C                   ENDIF
038600991116     C* DATA DATTURA
038700991116     C     *LIKE         DEFINE    V1CDF1        DF1V1C
038800991116     C                   Z-ADD     *ZEROS        DF1V1C
038900991116IF  2C     V1CDF1        IFGT      *ZEROS
039000991116     C                   Z-ADD     V1CDF1        G08DAT
039100991116     C                   Z-ADD     *ZEROS        G08INV
039200991116     C                   MOVEL     '0'           G08ERR
039300991116     C                   CALL      'XSRDA8'
039400991116     C                   PARM                    WLBDA8
039500991116     C                   Z-ADD     G08DAT        V1CDF1
039600991116     C                   Z-ADD     G08INV        DF1V1C
039700991116IF  3C     G08ERR        IFEQ      '1'
039800991116     C                   MOVEL     '1'           $ERR
039900991116     C                   MOVEL     MSG(13)       $MSG
040000991116     C                   EXSR      CARMSG
040100991116     C                   SETON                                        65
040200991116     C                   LEAVE
040300991116E   3C                   ENDIF
040400991116E   2C                   ENDIF
040500991116     C*
040600991116     C     *LIKE         DEFINE    V1CDF2        DF2V1C
040700991116     C                   Z-ADD     *ZEROS        DF2V1C
040800991116IF  2C     V1CDF2        IFGT      *ZEROS
040900991116     C     V1CDF1        ORGT      *ZEROS
041000991116     C                   Z-ADD     V1CDF2        G08DAT
041100991116     C                   Z-ADD     *ZEROS        G08INV
041200991116     C                   MOVEL     '0'           G08ERR
041300991116     C                   CALL      'XSRDA8'
041400991116     C                   PARM                    WLBDA8
041500991116     C                   Z-ADD     G08DAT        V1CDF2
041600991116     C                   Z-ADD     G08INV        DF2V1C
041700991116IF  3C     G08ERR        IFEQ      '1'
041800991116     C                   MOVEL     '1'           $ERR
041900991116     C                   MOVEL     MSG(13)       $MSG
042000991116     C                   EXSR      CARMSG
042100991116     C                   SETON                                        66
042200991116     C                   LEAVE
042300991116E   3C                   ENDIF
042400991116E   2C                   ENDIF
042500991116     C*
042600991116IF  2C     DF1V1C        IFGT      DF2V1C
042700991116     C                   MOVEL     '1'           $ERR
042800991116     C                   MOVEL     MSG(14)       $MSG
042900991116     C                   EXSR      CARMSG
043000991116     C                   SETON                                        6566
043100991116     C                   LEAVE
043200991116E   2C                   ENDIF
043300100512     C* ANNO
043400100512IF  2C     V1CASI        IFLT      1000
043500100512     C                   MOVEL     '1'           $ERR
043600100512     C                   MOVEL     MSG(19)       $MSG
043700100512     C                   EXSR      CARMSG
043800100512     C                   SETON                                        68
043900100512     C                   LEAVE
044000100512E   2C                   ENDIF
044100991117     C* PULIZIA FILES
044200110228IF  2C*    V1CCLR        IFEQ      'S'
044300110228     C*    V1CCLR        OREQ      'N'
044400110228X   2C*                  ELSE
044500110228     C*                  MOVEL     '1'           $ERR
044600110228     C*                  MOVEL     MSG(9)        $MSG
044700110228     C*                  EXSR      CARMSG
044800110228     C*                  SETON                                        70
044900110228     C*                  LEAVE
045000110228E   2C*                  ENDIF
045100991116     C*
045200991116E   1C                   ENDDO
045300991116     C*
045400991116     C                   ENDSR
045500991116     C*------------------------------------------------------------------------*
045600991116     C* CHKFIL - CONTROLLA E DECODIFCA LA FILIALE
045700991116     C*------------------------------------------------------------------------*
045800991116     C     CHKFIL        BEGSR
045900991116     C*
046000991116     C     *LIKE         DEFINE    V1CFIL        WFIL
046100991116     C     *LIKE         DEFINE    V1DFIL        WDFIL
046200991116     C*
046300991116     C                   Z-ADD     WFIL          KORFIL
046400991116     C     KEYORG        CHAIN     AZORG01L                           99
046500991116IF  1C     *IN99         IFEQ      *ON
046600991116     C                   MOVEL     '1'           $ERR
046700991116X   1C                   ELSE
046800991116     C                   MOVEL     ORGDES        WDFIL
046900991116E   1C                   ENDIF
047000991116     C*
047100991116     C                   ENDSR
047200991116     C*------------------------------------------------------------------------*
047300991116     C* CHKKSC - CONTROLLA E DECODIFCA IL CLIENTE
047400991116     C*------------------------------------------------------------------------*
047500991116     C     CHKKSC        BEGSR
047600991116     C*
047700991116     C     *LIKE         DEFINE    V1CKS1        WKSC
047800991116     C     *LIKE         DEFINE    V1DKS1        WDKSC
047900991116     C*
048000020923     C                   Z-ADD     1             KACKUT
048100020923     C                   Z-ADD     DUTKCI        KACKCC
048200991116     C                   Z-ADD     WKSC          KACKSC
048300991116     C     KEYACO        CHAIN     CNACO00F                           99
048400991116IF  1C     *IN99         IFEQ      *ON
048500991116     C                   MOVEL     *BLANKS       WDKSC
048600991116X   1C                   ELSE
048700991116     C                   MOVEL     ACORAG        WDKSC
048800991116E   1C                   ENDIF
048900991116     C*
049000991116     C                   ENDSR
049100991116     C*------------------------------------------------------------------------*
049200991116     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
049300991116     C*------------------------------------------------------------------------*
049400991116     C     IMPVID        BEGSR
049500991116     C*
049600991116     C                   MOVEL     '1'           V1CORD
049700110228     C*                  MOVEL     'N'           V1CCLR
049800991116     C*
049900991116     C                   ENDSR
050000020923     C*-------------------------------------------------------------------------
050100020923     C* REPERISCE I DATI UTENTE
050200020923     C*-------------------------------------------------------------------------
050300020923     C     REPDATIUTE    BEGSR
050400020923     C*
050500020923     C* INIZIALIZZA VARIABILI DI WRK
050600020923     C                   CLEAR                   TIBS34DS
050700020923     C                   CLEAR                   AZUTEDS
050800020923     C                   CLEAR                   DDATIUTE
050900020923     C*
051000020923     C     *DTAARA       DEFINE    §azute        azuteds
051100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
051200020923     C                   IN(E)     *DTAARA
051300020923if  1C                   IF        %Error
051400020923     c                   EVAL      I34Tla = 'L'
051500020923     C                   CALL      'TIBS34R'
051600020923     C                   PARM                    Tibs34Ds
051700020923     C                   IN        *DTAARA
051800020923e   1C                   ENDIF
051900020923     C*
052000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
052100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
052200020923     C*
052300020923     C                   ENDSR
052400991116     C*------------------------------------------------------------------------*
052500991116     C* *INZSR  - ROUTINE INIZIALE
052600991116     C*------------------------------------------------------------------------*
052700991116     C     *INZSR        BEGSR
052800991116     C*--------------------
052900991116     C* RICEVIMENTO PARAMETRI
053000991116     C*--------------------
053100991116     C     *ENTRY        PLIST
053200991116     C                   PARM                    KPJBA
053300991116     C*--------------------
053400991116     C* CAMPI RIFERITI AL DATA BASE
053500991116     C*--------------------
053600991116     C     *LIKE         DEFINE    ORGFIL        KORFIL
053700991116     C     *LIKE         DEFINE    ACOKUT        KACKUT
053800991116     C     *LIKE         DEFINE    ACOKCC        KACKCC
053900991116     C     *LIKE         DEFINE    ACOKSC        KACKSC
054000991116     C*--------------------
054100991116     C* CHIAVI DI LETTURA
054200991116     C*--------------------
054300991116     C* LETTURA AZORG01L - COMPLETA
054400991116     C     KEYORG        KLIST
054500991116     C                   KFLD                    KORFIL
054600991116     C* LETTURA CNACO00F - COMPLETA
054700991116     C     KEYACO        KLIST
054800991116     C                   KFLD                    KACKUT
054900991116     C                   KFLD                    KACKCC
055000991116     C                   KFLD                    KACKSC
055100991116     C*
055200991116     C* ESEGUE OPERAZIONI INIZIALI STANDARD
055300991116     C                   EXSR      INZSTD
055400991117     C*
055500991117     C* IMPOSTA DATA E ORA CORRENTE
055600991117     C                   TIME                    N14              14 0
055700991117     C                   MOVE      N14           N8                8 0
055800991117     C                   Z-ADD     N8            G08DAT
055900991117     C                   Z-ADD     *ZEROS        G08INV
056000991117     C                   MOVEL     '0'           G08ERR
056100991117     C                   CALL      'XSRDA8'
056200991117     C                   PARM                    WLBDA8
056300991117     C                   Z-ADD     G08INV        DATCOR            8 0
056400991116     C*
056500991116     C* IMPOSTA CAMPI VIDEO
056600991116     C                   EXSR      IMPVID
056700991116     C*
056800991116     C* EMETTE FORMATO VIDEO
056900991116     C                   EXSR      CARV1
057000991116     C*
057100991116     C                   ENDSR
057200991116     C*------------------------------------------------------------------------*
057300991116     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057400991116     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057500991116     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
057600991116     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
057700991116     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
057800991116     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057900991116     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
058000991116     C*------------------------------------------------------------------------*
058100991116     C* INZSTD - ROUTINE INIZIALE STANDARD
058200991116     C*------------------------------------------------------------------------*
058300991116     C     INZSTD        BEGSR
058400991116     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
058500991116     C                   CALL      'XKEYS'
058600991116     C                   PARM                    DSKEYS
058700991116     C                   MOVEA     KEYRIG        KEYS
058800991116     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
058900991116     C     XMSGPA        PLIST
059000991116     C                   PARM                    DSMSMS
059100991116     C                   PARM                    DSMSPG
059200991116     C                   PARM                    DSMSKE
059300991116     C                   PARM                    DSMSER
059400991116     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
059500991116     C                   MOVEL     'TREC10R'     DSMSPG
059600991116     C                   MOVEL     'TREC10R'     VMCQUE
059700991116     C                   MOVEL     *BLANKS       VMCKEY
059800991116     C                   MOVEL     *BLANKS       DSMSMS
059900991116     C* INIZIALIZZO FLAG CONTROLLO PGM
060000991116     C                   MOVEL     '0'           $ERR              1
060100991116     C                   MOVEL     '0'           $F03              1
060200991116     C                   MOVEL     *BLANKS       $GES              2
060300991116     C                   MOVEL     *BLANKS       $MSG             80
060400991116     C                   MOVEL     '0'           $FINE             1
060500991116     C                   MOVEL     '1'           $TESVD            1
060600991116     C                   MOVEL     'I'           $TGES             1
060700991116     C* LANCIO ROUTINE PER VERIFICARE SE
060800991116     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
060900991116     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
061000020923     C                   EXSR      REPDATIUTE
061100991116     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
061200991116     C                   BITOFF    '01234567'    POSMI1            1
061300991116     C                   BITOFF    '01234567'    POSMI2            1
061400991116     C                   BITOFF    '01234567'    POSMA1            1
061500991116     C                   BITOFF    '01234567'    POSMA2            1
061600991116     C                   BITON     '34'          POSMI1            1
061700991116     C                   BITON     '7'           POSMI2            1
061800991116     C                   BITON     '34'          POSMA1            1
061900991116     C                   BITON     '0'           POSMA2            1
062000991116     C                   MOVE      POSMI2        POSMIN            2
062100991116     C                   MOVEL     POSMI1        POSMIN
062200991116     C                   MOVE      POSMA2        POSMAX            2
062300991116     C                   MOVEL     POSMA1        POSMAX
062400991116     C                   ENDSR
062500991116     C*------------------------------------------------------------------------*
062600991116     C* FUNSTD - LANCIA SUBROUTINE STANDARD
062700991116     C*------------------------------------------------------------------------*
062800991116     C     FUNSTD        BEGSR
062900991116     C                   SELECT
063000991116     C     KEY           WHENEQ    F3
063100991116     C                   EXSR      F03STD
063200991116     C     KEY           WHENEQ    F12
063300991116     C                   EXSR      F12STD
063400991116     C     KEY           WHENEQ    F24
063500991116     C                   EXSR      F24STD
063600991116     C                   OTHER
063700991116     C                   MOVEL     MSG(2)        $MSG
063800991116     C                   EXSR      CARMSG
063900991116     C                   ENDSL
064000991116     C                   ENDSR
064100991116     C*------------------------------------------------------------------------*
064200991116     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
064300991116     C*------------------------------------------------------------------------*
064400991116     C     F03STD        BEGSR
064500991116     C*
064600991116     C* IMPOSTO FLAG DI FINE PROGRAMMA
064700991116     C                   MOVEL     '1'           $FINE
064800991116     C*
064900991116     C                   ENDSR
065000991116     C*------------------------------------------------------------------------*
065100991116     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
065200991116     C*------------------------------------------------------------------------*
065300991116     C     F12STD        BEGSR
065400991116     C* IMPOSTO FLAG DI FIN PGM
065500991116     C                   MOVEL     '1'           $FINE
065600991116     C                   ENDSR
065700991116     C*------------------------------------------------------------------------*
065800991116     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
065900991116     C*------------------------------------------------------------------------*
066000991116     C     F24STD        BEGSR
066100991116     C*
066200991116     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
066300991116     C                   EXSR      OUTFUN
066400991116     C*
066500991116     C                   ENDSR
066600991116     C*------------------------------------------------------------------------*
066700991116     C* CARMSG - CARICA MSG NEL SUBFILE MSG E LO INVIA ALLA CODA MSG DEL PGM
066800991116     C*------------------------------------------------------------------------*
066900991116     C     CARMSG        BEGSR
067000991116     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
067100991116     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
067200991116     C     MSGULT        IFEQ      0
067300991116     C                   MOVEL     '1'           *IN40
067400991116     C                   WRITE     EC10DVK
067500991116     C                   MOVEL     '0'           *IN40
067600991116     C                   ENDIF
067700991116     C* INVIA MESSAGGIO ALLA CODA DEL PGM
067800991116     C                   MOVEL     $MSG          DSMSMS
067900991116     C                   CALL      'XMSG'        XMSGPA
068000991116     C     DSMSER        IFEQ      0
068100991116     C                   MOVEL     DSMSKE        VMCKEY
068200991116     C                   ENDIF
068300991116     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
068400991116     C                   ADD       1             MSGULT            4 0
068500991116     C                   WRITE     EC10DVM
068600991116     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
068700991116     C                   MOVEL     *BLANKS       $MSG
068800991116     C                   ENDSR
068900991116     C*------------------------------------------------------------------------*
069000991116     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
069100991116     C*------------------------------------------------------------------------*
069200991116     C     OUTMSG        BEGSR
069300991116     C* VISUALIZZAZIONE MESSAGGIO
069400991116     C                   MOVEL     '1'           *IN44
069500991116     C                   WRITE     EC10DVK
069600991116     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
069700991116     C                   Z-ADD     *ZEROS        MSGULT
069800991116     C                   ENDSR
069900991116     C*------------------------------------------------------------------------*
070000991116     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
070100991116     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
070200991116     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
070300991116     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
070400991116     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
070500991116     C*------------------------------------------------------------------------*
070600991116     C     CARFUN        BEGSR
070700991116     C                   Z-ADD     *ZEROS        FUNULT            5 0
070800991116     C     1             DO        31            I                 5 0
070900991116     C                   MOVEL     FUN(I)        FUNRIG
071000991116     C                   MOVEL     FUNFUN        FFU(I)
071100991116     C                   MOVEL     FUNDES        FDE(I)
071200020923     C     DUTLPO        IFEQ      'S'
071300991116     C     $TGES         IFEQ      'G'
071400991116     C                   MOVEL     FUNASG        FAT(I)
071500991116     C                   ELSE
071600991116     C                   MOVEL     FUNASI        FAT(I)
071700991116     C                   ENDIF
071800991116     C                   ELSE
071900991116     C     $TGES         IFEQ      'G'
072000991116     C                   MOVEL     FUNAFG        FAT(I)
072100991116     C                   ELSE
072200991116     C                   MOVEL     FUNAFI        FAT(I)
072300991116     C                   ENDIF
072400991116     C                   ENDIF
072500991116     C     FAT(I)        IFEQ      'E'
072600991116     C                   MOVEL     USER          WEDP              3
072700991116     C     WEDP          IFEQ      'EDP'
072800991116     C                   MOVEL     'S'           FAT(I)
072900991116     C                   ELSE
073000991116     C                   MOVEL     'N'           FAT(I)
073100991116     C                   ENDIF
073200991116     C                   ENDIF
073300991116     C                   ENDDO
073400991116     C                   ENDSR
073500991116     C*------------------------------------------------------------------------*
073600991116     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
073700991116     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
073800991116     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
073900991116     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
074000991116     C*------------------------------------------------------------------------*
074100991116     C     OUTFUN        BEGSR
074200991116     C     1             DO        8             I
074300991116     C                   MOVEL     *BLANKS       FUV(I)
074400991116     C                   ENDDO
074500991116     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
074600991116     C                   Z-ADD     *ZEROS        FUNNAT            2 0
074700991116     C     1             DO        31            I
074800991116     C     FAT(I)        IFEQ      'S'
074900991116     C     FDE(I)        ANDNE     *BLANKS
075000991116     C                   ADD       1             FUNNAT
075100991116     C                   ENDIF
075200991116     C                   ENDDO
075300991116     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
075400991116     C     FUNNAT        IFGT      8
075500991116     C                   MOVEL     'S'           FAT(24)
075600991116     C                   ENDIF
075700991116     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
075800991116     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
075900991116     C                   Z-ADD     8             FUNVIS            5 0
076000991116     C     FAT(23)       IFEQ      'S'
076100991116     C                   SUB       1             FUNVIS
076200991116     C                   ENDIF
076300991116     C     FAT(24)       IFEQ      'S'
076400991116     C                   SUB       1             FUNVIS
076500991116     C                   ENDIF
076600991116     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
076700991116     C                   Z-ADD     *ZEROS        W                 5 0
076800991116     C                   ADD       1             FUNULT
076900991116     C     FUNULT        IFGT      31
077000991116     C                   Z-ADD     1             FUNULT
077100991116     C                   ENDIF
077200991116     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
077300991116     C     FUNULT        DO        31            I
077400991116     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
077500991116     C     W             IFLT      FUNVIS
077600991116     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
077700991116     C     FAT(I)        IFEQ      'S'
077800991116     C     FDE(I)        ANDNE     *BLANKS
077900991116     C     I             IFEQ      23
078000991116     C     I             OREQ      24
078100991116     C                   ELSE
078200991116     C                   ADD       1             W
078300991116     C                   MOVEL     FDE(I)        FUV(W)
078400991116     C                   ENDIF
078500991116     C                   ENDIF
078600991116     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
078700991116     C                   Z-ADD     I             FUNULT
078800991116     C                   ENDIF
078900991116     C                   ENDDO
079000991116     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
079100991116     C                   MOVEL     FUV(1)        VFD001
079200991116     C                   MOVEL     FUV(2)        VFD002
079300991116     C                   MOVEL     FUV(3)        VFD003
079400991116     C                   MOVEL     FUV(4)        VFD004
079500991116     C                   MOVEL     FUV(5)        VFD005
079600991116     C                   MOVEL     FUV(6)        VFD006
079700991116     C     FAT(23)       IFEQ      'N'
079800991116     C     FAT(24)       ANDEQ     'N'
079900991116     C                   MOVEL     FUV(7)        VFD007
080000991116     C                   MOVEL     FUV(8)        VFD008
080100991116     C                   ELSE
080200991116     C     FAT(23)       IFEQ      'S'
080300991116     C     FAT(24)       ANDEQ     'S'
080400991116     C                   MOVEL     FDE(23)       VFD007
080500991116     C                   MOVEL     FDE(24)       VFD008
080600991116     C                   ELSE
080700991116     C                   MOVEL     FUV(7)        VFD007
080800991116     C     FAT(23)       IFEQ      'S'
080900991116     C                   MOVEL     FDE(23)       VFD008
081000991116     C                   ELSE
081100991116     C                   MOVEL     FDE(24)       VFD008
081200991116     C                   ENDIF
081300991116     C                   ENDIF
081400991116     C                   ENDIF
081500991116     C                   MOVEL     '0'           $TESVD
081600991116     C                   ENDSR
081700991116     C*------------------------------------------------------------------------*
081800991116     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
081900991116     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
082000991116     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
082100991116     C*                   - ERRORE SE FUNZIONE NON ATTIVA
082200991116     C*------------------------------------------------------------------------*
082300991116     C     CONFUN        BEGSR
082400991116     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
082500991116     C     1             DO        31            I
082600991116     C     KEY           IFEQ      KEYS(I)
082700991116     C* CONTROLLO SE IL TASTO E' ATTIVO
082800991116     C     FAT(I)        IFNE      'S'
082900991116     C                   MOVEL     MSG(1)        $MSG
083000991116     C                   EXSR      CARMSG
083100991116     C                   MOVEL     '1'           $ERR
083200991116     C                   ENDIF
083300991116     C                   ENDIF
083400991116     C                   ENDDO
083500991116     C                   ENDSR
083600991116     O*------------------------------------------------------------------------*
083700991116** - MSG -
083800991116TREC10R- Tasto funzionale non valido.                                          1
083900991116TREC10R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
084000991116TREC10R- Opzione non valida.                                                   3
084100991116TREC10R- Opzione abilitata ma non gestita. Avvertire CED.                      4
084200991116TREC10R- Inizio elenco.                                                        5
084300991116TREC10R- Fine elenco.                                                          6
084400991116TREC10R- 7
084500991116TREC10R- 8
084600991117TREC10R- Immettere uno dei valori possibili                                    9
084700991116TREC10R- Scegliere il tipo ordinamento                                        10
084800991116TREC10R- Filiale errata                                                       11
084900991116TREC10R- Cliente errato                                                       12
085000991116TREC10R- Data errata                                                          13
085100991116TREC10R- L' AL deve essere maggiore o uguale al DAL                           14
085200991116TREC10R- Operare almeno una selezione                                         15
085300991116TREC10R- Per l'ordinamento prescelto, selezionare filiale                     16
085400991116TREC10R- Per l'ordinamento prescelto, selezionare il cliente                  17
085500140528TREC10R- selezionare la data spedizione                                       18
085600991116TREC10R- Anno errato                                                          19
085700991117TREC10R- Sistema informativo bolle errato                                     20
085800991116** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
085900991116F1    -                   -    NN     NN
086000991116F2    -                   -    NN     NN
086100991116F3    -F3=Fine            -    SS     SS
086200991116F4    -                   -    NN     NN
086300991116F5    -                   -    NN     NN
086400110314F6    -F6=Lancio Int.     -    SS     SS
086500110314F7    -                   -    NN     NN
086600110314F8    -F8=Lancio Batch    -    SS     SS
086700991116F9    -                   -    NN     NN
086800991116F10   -                   -    NN     NN
086900991116F11   -                   -    NN     NN
087000991116F12   -F12=Ritorno        -    SS     SS
087100991116F13   -                   -    NN     NN
087200991116F14   -                   -    NN     NN
087300991116F15   -                   -    NN     NN
087400991116F16   -                   -    NN     NN
087500991116F17   -                   -    NN     NN
087600991116F18   -                   -    NN     NN
087700991116F19   -                   -    NN     NN
087800991116F20   -                   -    NN     NN
087900991116F21   -                   -    NN     NN
088000991116F22   -                   -    NN     NN
088100991116F23   -                   -    NN     NN
088200991116F24   -F24=Altri tasti    -    NN     NN
088300991116ENTER -                   -    SS     SS
088400991116ROLLUP-                   -    NN     NN
088500991116ROLLDN-                   -    NN     NN
088600991116HELP  -                   -    NN     NN
088700991116PRINT -                   -    NN     NN
088800991116BSPACE-                   -    NN     NN
088900991116CLEAR -                   -    NN     NN
