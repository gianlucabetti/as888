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
013901110314     C     KEY           WHENEQ    F8
013902110314     C                   EXSR      F06GES
014000991116     C                   OTHER
014100991116     C                   EXSR      FUNSTD
014200991116E   2C                   ENDSL
014300991116E   1C                   ENDIF
014400991116     C*
014500991116     C                   ENDSR
014600991116     C*------------------------------------------------------------------------*
014700991116     C* CARV1 - CARICA PRIMO VIDEO
014800991116     C*------------------------------------------------------------------------*
014900991116     C     CARV1         BEGSR
015000991116     C*
015100991116     C                   MOVEL     'V1'          $GES
015200991116     C*
015300991116     C                   EXSR      CARFUN
015400991116     C                   EXSR      OUTFUN
015500991116     C*
015600991116     C                   WRITE     EC10DVT
015700991116     C                   WRITE     EC10DVF
015800991116     C*
015900991116     C                   ENDSR
016000991116     C*------------------------------------------------------------------------*
016100991116     C* ENTV1 - GESTIONE TASTO ENTER PRIMO VIDEO
016200991116     C*------------------------------------------------------------------------*
016300991116     C     ENTV1         BEGSR
016400991116     C*
016500991116     C* CONTROLLA I CAMPI IMMESSI
016600991116     C                   EXSR      DECODI
016700991116     C*
016800991116     C                   ENDSR
016900991116     C*------------------------------------------------------------------------*
017000991116     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
017100991116     C*------------------------------------------------------------------------*
017200991116     C     F06GES        BEGSR
017300991116     C*
017400991116     C* CONTROLLA I CAMPI IMMESSI
017500991116     C                   EXSR      DECODI
017600991116     C*
017601110228     C* CONTROLLA Libreria e file sui quali simulare
017602110228     C                   EXSR      srwin
017603110228     C*
017700991116     C* IMPOSTA LA LISTA PARAMETRI E LANCIA IL PROGRAMMA
017800991116IF  1C     $ERR          IFEQ      '0'
017900991116     C                   MOVEL     V1CORD        PARORD
018000991116     C                   Z-ADD     V1CFIL        PARFIL
018100991116     C                   Z-ADD     V1CKS1        PARKS1
018200991116     C                   Z-ADD     V1CKS2        PARKS2
018300991116     C                   Z-ADD     DS1V1C        PARDS1
018400991116     C                   Z-ADD     DS2V1C        PARDS2
018500991116     C                   Z-ADD     DF1V1C        PARDF1
018600991116     C                   Z-ADD     DF2V1C        PARDF2
018700100511     C                   Z-ADD     v1casi        PARASI
018800110228     C*                  MOVEL     V1CCLR        PARCLR
018801110228     C                   MOVEL     lib           PARlib
018900991116     C* LANCIO
019000991116     C                   MOVEL     PARAM         KPJBU
019001110314     C     KEY           ifeq      F8
019100091016     C                   MOVEL     'ECS1'        KCOAZ
019200110314     C                   CALL      'BCH10'
019201110314     C                   PARM                    KPJBA
019202110314     c                   end
019203110314     C     KEY           ifeq      F6
019301110314     c                   call      'TRECS11C'
019302100422     C                   PARM                    KPJBA
019303110314     c                   end
019400991116E   1C                   ENDIF
019500991116     C*
019600991116     C                   ENDSR
019700991116     C*------------------------------------------------------------------------*
019800110228     C* richiede CONTROLLA CAMPI E DECODIFICA
019900991116     C*------------------------------------------------------------------------*
020000110228     C     srwin         BEGSR
020100991116     C*
020101110228     c                   call      'TRECS1R1'
020102110228     c                   parm                    lib              10
020105110228     c                   parm                    err               1
020106110228     c                   if        err <> ' '
020107110228     c                   eval      $err= err
020108110228     c                   select
020109110228     c                   when      err = '1'
020111110228     C                   eval      $MSG = 'Errore nella libreria sulla quale -
020112110228     c                             simulare'
020113110228     c                   when      err = '2'
020114110228     C                   eval      $MSG = 'Errore nella duplica dei file sui -
020115110228     c                             quali simulare'
020116110228     c                   when      err = '3'
020117110228     C                   eval      $MSG = 'Errore negli oggetti file sui -
020118110228     c                             quali simulare'
020119110228     c                   when      err = '1'
020120110228     C                   eval      $MSG = 'Errore nella pulizia dei file sui -
020121110228     c                             quali simulare'
020122110228     c                   endsl
020123110228     C                   EXSR      CARMSG
020124110228     c                   else
020125110228     c                   eval      $err= '0'
020126110228     c                   end
020127110228     C*
020128110228     C                   ENDSR
020129110228     C*------------------------------------------------------------------------*
020130110228     C* DECODI - CONTROLLA CAMPI E DECODIFICA
020131110228     C*------------------------------------------------------------------------*
020132110228     C     DECODI        BEGSR
020133110228     C*
020200991116     C* REIMPOSTA INIDICATORI
020300991116     C                   SETOFF                                       606162
020400991116     C                   SETOFF                                       636465
020500991116     C                   SETOFF                                       666768
020600991117     C                   SETOFF                                       6970
020700991116     C*
020800991116DO  1C                   DO
020900991116     C*---
021000991116     C* ORDINAMENTO
021100991116     C*---
021200991116IF  2C     V1CORD        IFEQ      '1'
021300991116     C     V1CORD        OREQ      '2'
021400991116     C     V1CORD        OREQ      '3'
021500991116X   2C                   ELSE
021600991116     C                   MOVEL     '1'           $ERR
021700991116     C                   MOVEL     MSG(10)       $MSG
021800991116     C                   EXSR      CARMSG
021900991116     C                   SETON                                        60
022000991116     C                   LEAVE
022100991116E   2C                   ENDIF
022200991116     C*---
022300991116     C* SELEZIONI
022400991116     C*---
022500991116     C* FILIALE
022600991116IF  2C     V1CFIL        IFGT      *ZEROS
022700991116     C                   Z-ADD     V1CFIL        WFIL
022800991116     C                   MOVEL     *BLANKS       WDFIL
022900991116     C                   EXSR      CHKFIL
023000991116     C                   MOVEL     WDFIL         V1DFIL
023100991116IF  3C     $ERR          IFEQ      '1'
023200991116     C                   MOVEL     '1'           $ERR
023300991116     C                   MOVEL     MSG(11)       $MSG
023400991116     C                   EXSR      CARMSG
023500991116     C                   SETON                                        61
023600991116     C                   LEAVE
023700991116E   3C                   ENDIF
023800991116X   2C                   ELSE
023900991116IF  3C     V1CORD        IFEQ      '1'
024000991116     C                   MOVEL     '1'           $ERR
024100991116     C                   MOVEL     MSG(16)       $MSG
024200991116     C                   EXSR      CARMSG
024300991116     C                   SETON                                        61
024400991116     C                   LEAVE
024500991116E   3C                   ENDIF
024600991116E   2C                   ENDIF
024700991116     C* CLIENTI
024800991116IF  2C     V1CKS1        IFGT      *ZEROS
024900991116     C                   Z-ADD     V1CKS1        WKSC
025000991116     C                   MOVEL     *BLANKS       WDKSC
025100991116     C                   EXSR      CHKKSC
025200991116     C                   MOVEL     WDKSC         V1DKS1
025300991116X   2C                   ELSE
025400991116IF  3C     V1CORD        IFEQ      '2'
025500991116     C                   MOVEL     '1'           $ERR
025600991116     C                   MOVEL     MSG(17)       $MSG
025700991116     C                   EXSR      CARMSG
025800991116     C                   SETON                                        62
025900991116     C                   LEAVE
026000991116E   3C                   ENDIF
026100991116E   2C                   ENDIF
026200991116     C*
026300991116IF  2C     V1CKS2        IFGT      *ZEROS
026400991116     C                   Z-ADD     V1CKS2        WKSC
026500991116     C                   MOVEL     *BLANKS       WDKSC
026600991116     C                   EXSR      CHKKSC
026700991116     C                   MOVEL     WDKSC         V1DKS2
026800991116X   2C                   ELSE
026900991116IF  3C     V1CORD        IFEQ      '2'
027000991116     C                   MOVEL     '1'           $ERR
027100991116     C                   MOVEL     MSG(17)       $MSG
027200991116     C                   EXSR      CARMSG
027300991116     C                   SETON                                        67
027400991116     C                   LEAVE
027500991116E   3C                   ENDIF
027600991116E   2C                   ENDIF
027700991116     C*
027800991116IF  2C     V1CKS2        IFLT      V1CKS1
027900991116     C                   MOVEL     '1'           $ERR
028000991116     C                   MOVEL     MSG(14)       $MSG
028100991116     C                   EXSR      CARMSG
028200991116     C                   SETON                                        6267
028300991116     C                   LEAVE
028400991116E   2C                   ENDIF
028500991116     C* DATA SPEDIZIONE
028600991116     C     *LIKE         DEFINE    V1CDS1        DS1V1C
028700991116     C                   Z-ADD     *ZEROS        DS1V1C
028800991116IF  2C     V1CDS1        IFGT      *ZEROS
028900991116     C                   Z-ADD     V1CDS1        G08DAT
029000991116     C                   Z-ADD     *ZEROS        G08INV
029100991116     C                   MOVEL     '0'           G08ERR
029200991116     C                   CALL      'XSRDA8'
029300991116     C                   PARM                    WLBDA8
029400991116     C                   Z-ADD     G08DAT        V1CDS1
029500991116     C                   Z-ADD     G08INV        DS1V1C
029600991116IF  3C     G08ERR        IFEQ      '1'
029700991116     C                   MOVEL     '1'           $ERR
029800991116     C                   MOVEL     MSG(13)       $MSG
029900991116     C                   EXSR      CARMSG
030000991116     C                   SETON                                        63
030100991116     C                   LEAVE
030200991116E   3C                   ENDIF
030300991116X   2C                   ELSE
030400991116IF  3C     V1CORD        IFEQ      '1'
030500991116     C     V1CORD        OREQ      '3'
030600991116     C                   MOVEL     '1'           $ERR
030700991116     C                   MOVEL     MSG(18)       $MSG
030800991116     C                   EXSR      CARMSG
030900991116     C                   SETON                                        63
031000991116     C                   LEAVE
031100991116E   3C                   ENDIF
031200991116E   2C                   ENDIF
031300991116     C*
031400991116     C     *LIKE         DEFINE    V1CDS2        DS2V1C
031500991116     C                   Z-ADD     *ZEROS        DS2V1C
031600991116IF  2C     V1CDS2        IFGT      *ZEROS
031700991116     C     V1CDS1        ORGT      *ZEROS
031800991116     C                   Z-ADD     V1CDS2        G08DAT
031900991116     C                   Z-ADD     *ZEROS        G08INV
032000991116     C                   MOVEL     '0'           G08ERR
032100991116     C                   CALL      'XSRDA8'
032200991116     C                   PARM                    WLBDA8
032300991116     C                   Z-ADD     G08DAT        V1CDS2
032400991116     C                   Z-ADD     G08INV        DS2V1C
032500991116IF  3C     G08ERR        IFEQ      '1'
032600991116     C                   MOVEL     '1'           $ERR
032700991116     C                   MOVEL     MSG(13)       $MSG
032800991116     C                   EXSR      CARMSG
032900991116     C                   SETON                                        64
033000991116     C                   LEAVE
033100991116E   3C                   ENDIF
033200991116X   2C                   ELSE
033300991116IF  3C     V1CORD        IFEQ      '1'
033400991116     C     V1CORD        OREQ      '3'
033500991116     C                   MOVEL     '1'           $ERR
033600991116     C                   MOVEL     MSG(18)       $MSG
033700991116     C                   EXSR      CARMSG
033800991116     C                   SETON                                        64
033900991116     C                   LEAVE
034000991116E   3C                   ENDIF
034100991116E   2C                   ENDIF
034200991116     C*
034300991116IF  2C     DS1V1C        IFGT      DS2V1C
034400991116     C                   MOVEL     '1'           $ERR
034500991116     C                   MOVEL     MSG(14)       $MSG
034600991116     C                   EXSR      CARMSG
034700991116     C                   SETON                                        6364
034800991116     C                   LEAVE
034900991116E   2C                   ENDIF
035000991116     C* DATA DATTURA
035100991116     C     *LIKE         DEFINE    V1CDF1        DF1V1C
035200991116     C                   Z-ADD     *ZEROS        DF1V1C
035300991116IF  2C     V1CDF1        IFGT      *ZEROS
035400991116     C                   Z-ADD     V1CDF1        G08DAT
035500991116     C                   Z-ADD     *ZEROS        G08INV
035600991116     C                   MOVEL     '0'           G08ERR
035700991116     C                   CALL      'XSRDA8'
035800991116     C                   PARM                    WLBDA8
035900991116     C                   Z-ADD     G08DAT        V1CDF1
036000991116     C                   Z-ADD     G08INV        DF1V1C
036100991116IF  3C     G08ERR        IFEQ      '1'
036200991116     C                   MOVEL     '1'           $ERR
036300991116     C                   MOVEL     MSG(13)       $MSG
036400991116     C                   EXSR      CARMSG
036500991116     C                   SETON                                        65
036600991116     C                   LEAVE
036700991116E   3C                   ENDIF
036800991116E   2C                   ENDIF
036900991116     C*
037000991116     C     *LIKE         DEFINE    V1CDF2        DF2V1C
037100991116     C                   Z-ADD     *ZEROS        DF2V1C
037200991116IF  2C     V1CDF2        IFGT      *ZEROS
037300991116     C     V1CDF1        ORGT      *ZEROS
037400991116     C                   Z-ADD     V1CDF2        G08DAT
037500991116     C                   Z-ADD     *ZEROS        G08INV
037600991116     C                   MOVEL     '0'           G08ERR
037700991116     C                   CALL      'XSRDA8'
037800991116     C                   PARM                    WLBDA8
037900991116     C                   Z-ADD     G08DAT        V1CDF2
038000991116     C                   Z-ADD     G08INV        DF2V1C
038100991116IF  3C     G08ERR        IFEQ      '1'
038200991116     C                   MOVEL     '1'           $ERR
038300991116     C                   MOVEL     MSG(13)       $MSG
038400991116     C                   EXSR      CARMSG
038500991116     C                   SETON                                        66
038600991116     C                   LEAVE
038700991116E   3C                   ENDIF
038800991116E   2C                   ENDIF
038900991116     C*
039000991116IF  2C     DF1V1C        IFGT      DF2V1C
039100991116     C                   MOVEL     '1'           $ERR
039200991116     C                   MOVEL     MSG(14)       $MSG
039300991116     C                   EXSR      CARMSG
039400991116     C                   SETON                                        6566
039500991116     C                   LEAVE
039600991116E   2C                   ENDIF
039601100512     C* ANNO
039602100512IF  2C     V1CASI        IFLT      1000
039603100512     C                   MOVEL     '1'           $ERR
039604100512     C                   MOVEL     MSG(19)       $MSG
039605100512     C                   EXSR      CARMSG
039606100512     C                   SETON                                        68
039607100512     C                   LEAVE
039608100512E   2C                   ENDIF
040800991117     C* PULIZIA FILES
040900110228IF  2C*    V1CCLR        IFEQ      'S'
041000110228     C*    V1CCLR        OREQ      'N'
041100110228X   2C*                  ELSE
041200110228     C*                  MOVEL     '1'           $ERR
041300110228     C*                  MOVEL     MSG(9)        $MSG
041400110228     C*                  EXSR      CARMSG
041500110228     C*                  SETON                                        70
041600110228     C*                  LEAVE
041700110228E   2C*                  ENDIF
041800991116     C*
041900991116E   1C                   ENDDO
042000991116     C*
042100991116     C                   ENDSR
042200991116     C*------------------------------------------------------------------------*
042300991116     C* CHKFIL - CONTROLLA E DECODIFCA LA FILIALE
042400991116     C*------------------------------------------------------------------------*
042500991116     C     CHKFIL        BEGSR
042600991116     C*
042700991116     C     *LIKE         DEFINE    V1CFIL        WFIL
042800991116     C     *LIKE         DEFINE    V1DFIL        WDFIL
042900991116     C*
043000991116     C                   Z-ADD     WFIL          KORFIL
043100991116     C     KEYORG        CHAIN     AZORG01L                           99
043200991116IF  1C     *IN99         IFEQ      *ON
043300991116     C                   MOVEL     '1'           $ERR
043400991116X   1C                   ELSE
043500991116     C                   MOVEL     ORGDES        WDFIL
043600991116E   1C                   ENDIF
043700991116     C*
043800991116     C                   ENDSR
043900991116     C*------------------------------------------------------------------------*
044000991116     C* CHKKSC - CONTROLLA E DECODIFCA IL CLIENTE
044100991116     C*------------------------------------------------------------------------*
044200991116     C     CHKKSC        BEGSR
044300991116     C*
044400991116     C     *LIKE         DEFINE    V1CKS1        WKSC
044500991116     C     *LIKE         DEFINE    V1DKS1        WDKSC
044600991116     C*
044700020923     C                   Z-ADD     1             KACKUT
044800020923     C                   Z-ADD     DUTKCI        KACKCC
044900991116     C                   Z-ADD     WKSC          KACKSC
045000991116     C     KEYACO        CHAIN     CNACO00F                           99
045100991116IF  1C     *IN99         IFEQ      *ON
045200991116     C                   MOVEL     *BLANKS       WDKSC
045300991116X   1C                   ELSE
045400991116     C                   MOVEL     ACORAG        WDKSC
045500991116E   1C                   ENDIF
045600991116     C*
045700991116     C                   ENDSR
045800991116     C*------------------------------------------------------------------------*
045900991116     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
046000991116     C*------------------------------------------------------------------------*
046100991116     C     IMPVID        BEGSR
046200991116     C*
046300991116     C                   MOVEL     '1'           V1CORD
046800110228     C*                  MOVEL     'N'           V1CCLR
046900991116     C*
047000991116     C                   ENDSR
047100020923     C*-------------------------------------------------------------------------
047200020923     C* REPERISCE I DATI UTENTE
047300020923     C*-------------------------------------------------------------------------
047400020923     C     REPDATIUTE    BEGSR
047500020923     C*
047600020923     C* INIZIALIZZA VARIABILI DI WRK
047700020923     C                   CLEAR                   TIBS34DS
047800020923     C                   CLEAR                   AZUTEDS
047900020923     C                   CLEAR                   DDATIUTE
048000020923     C*
048100020923     C     *DTAARA       DEFINE    §azute        azuteds
048200020923     C     *DTAARA       DEFINE    §datiute      ddatiute
048300020923     C                   IN(E)     *DTAARA
048400020923if  1C                   IF        %Error
048500020923     c                   EVAL      I34Tla = 'L'
048600020923     C                   CALL      'TIBS34R'
048700020923     C                   PARM                    Tibs34Ds
048800020923     C                   IN        *DTAARA
048900020923e   1C                   ENDIF
049000020923     C*
049100020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
049200020923     C                   MOVEL(P)  RSUT          DSFIRS           20
049300020923     C*
049400020923     C                   ENDSR
049500991116     C*------------------------------------------------------------------------*
049600991116     C* *INZSR  - ROUTINE INIZIALE
049700991116     C*------------------------------------------------------------------------*
049800991116     C     *INZSR        BEGSR
049900991116     C*--------------------
050000991116     C* RICEVIMENTO PARAMETRI
050100991116     C*--------------------
050200991116     C     *ENTRY        PLIST
050300991116     C                   PARM                    KPJBA
050400991116     C*--------------------
050500991116     C* CAMPI RIFERITI AL DATA BASE
050600991116     C*--------------------
050700991116     C     *LIKE         DEFINE    ORGFIL        KORFIL
050800991116     C     *LIKE         DEFINE    ACOKUT        KACKUT
050900991116     C     *LIKE         DEFINE    ACOKCC        KACKCC
051000991116     C     *LIKE         DEFINE    ACOKSC        KACKSC
051100991116     C*--------------------
051200991116     C* CHIAVI DI LETTURA
051300991116     C*--------------------
051400991116     C* LETTURA AZORG01L - COMPLETA
051500991116     C     KEYORG        KLIST
051600991116     C                   KFLD                    KORFIL
051700991116     C* LETTURA CNACO00F - COMPLETA
051800991116     C     KEYACO        KLIST
051900991116     C                   KFLD                    KACKUT
052000991116     C                   KFLD                    KACKCC
052100991116     C                   KFLD                    KACKSC
052200991116     C*
052300991116     C* ESEGUE OPERAZIONI INIZIALI STANDARD
052400991116     C                   EXSR      INZSTD
052500991117     C*
052600991117     C* IMPOSTA DATA E ORA CORRENTE
052700991117     C                   TIME                    N14              14 0
052800991117     C                   MOVE      N14           N8                8 0
052900991117     C                   Z-ADD     N8            G08DAT
053000991117     C                   Z-ADD     *ZEROS        G08INV
053100991117     C                   MOVEL     '0'           G08ERR
053200991117     C                   CALL      'XSRDA8'
053300991117     C                   PARM                    WLBDA8
053400991117     C                   Z-ADD     G08INV        DATCOR            8 0
053500991116     C*
053600991116     C* IMPOSTA CAMPI VIDEO
053700991116     C                   EXSR      IMPVID
053800991116     C*
053900991116     C* EMETTE FORMATO VIDEO
054000991116     C                   EXSR      CARV1
054100991116     C*
054200991116     C                   ENDSR
054300991116     C*------------------------------------------------------------------------*
054400991116     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
054500991116     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
054600991116     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
054700991116     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
054800991116     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
054900991116     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
055000991116     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
055100991116     C*------------------------------------------------------------------------*
055200991116     C* INZSTD - ROUTINE INIZIALE STANDARD
055300991116     C*------------------------------------------------------------------------*
055400991116     C     INZSTD        BEGSR
055500991116     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
055600991116     C                   CALL      'XKEYS'
055700991116     C                   PARM                    DSKEYS
055800991116     C                   MOVEA     KEYRIG        KEYS
055900991116     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
056000991116     C     XMSGPA        PLIST
056100991116     C                   PARM                    DSMSMS
056200991116     C                   PARM                    DSMSPG
056300991116     C                   PARM                    DSMSKE
056400991116     C                   PARM                    DSMSER
056500991116     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
056600991116     C                   MOVEL     'TREC10R'     DSMSPG
056700991116     C                   MOVEL     'TREC10R'     VMCQUE
056800991116     C                   MOVEL     *BLANKS       VMCKEY
056900991116     C                   MOVEL     *BLANKS       DSMSMS
057000991116     C* INIZIALIZZO FLAG CONTROLLO PGM
057100991116     C                   MOVEL     '0'           $ERR              1
057200991116     C                   MOVEL     '0'           $F03              1
057300991116     C                   MOVEL     *BLANKS       $GES              2
057400991116     C                   MOVEL     *BLANKS       $MSG             80
057500991116     C                   MOVEL     '0'           $FINE             1
057600991116     C                   MOVEL     '1'           $TESVD            1
057700991116     C                   MOVEL     'I'           $TGES             1
057800991116     C* LANCIO ROUTINE PER VERIFICARE SE
057900991116     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
058000991116     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
058100020923     C                   EXSR      REPDATIUTE
058200991116     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
058300991116     C                   BITOFF    '01234567'    POSMI1            1
058400991116     C                   BITOFF    '01234567'    POSMI2            1
058500991116     C                   BITOFF    '01234567'    POSMA1            1
058600991116     C                   BITOFF    '01234567'    POSMA2            1
058700991116     C                   BITON     '34'          POSMI1            1
058800991116     C                   BITON     '7'           POSMI2            1
058900991116     C                   BITON     '34'          POSMA1            1
059000991116     C                   BITON     '0'           POSMA2            1
059100991116     C                   MOVE      POSMI2        POSMIN            2
059200991116     C                   MOVEL     POSMI1        POSMIN
059300991116     C                   MOVE      POSMA2        POSMAX            2
059400991116     C                   MOVEL     POSMA1        POSMAX
059500991116     C                   ENDSR
059600991116     C*------------------------------------------------------------------------*
059700991116     C* FUNSTD - LANCIA SUBROUTINE STANDARD
059800991116     C*------------------------------------------------------------------------*
059900991116     C     FUNSTD        BEGSR
060000991116     C                   SELECT
060100991116     C     KEY           WHENEQ    F3
060200991116     C                   EXSR      F03STD
060300991116     C     KEY           WHENEQ    F12
060400991116     C                   EXSR      F12STD
060500991116     C     KEY           WHENEQ    F24
060600991116     C                   EXSR      F24STD
060700991116     C                   OTHER
060800991116     C                   MOVEL     MSG(2)        $MSG
060900991116     C                   EXSR      CARMSG
061000991116     C                   ENDSL
061100991116     C                   ENDSR
061200991116     C*------------------------------------------------------------------------*
061300991116     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
061400991116     C*------------------------------------------------------------------------*
061500991116     C     F03STD        BEGSR
061600991116     C*
061700991116     C* IMPOSTO FLAG DI FINE PROGRAMMA
061800991116     C                   MOVEL     '1'           $FINE
061900991116     C*
062000991116     C                   ENDSR
062100991116     C*------------------------------------------------------------------------*
062200991116     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
062300991116     C*------------------------------------------------------------------------*
062400991116     C     F12STD        BEGSR
062500991116     C* IMPOSTO FLAG DI FIN PGM
062600991116     C                   MOVEL     '1'           $FINE
062700991116     C                   ENDSR
062800991116     C*------------------------------------------------------------------------*
062900991116     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
063000991116     C*------------------------------------------------------------------------*
063100991116     C     F24STD        BEGSR
063200991116     C*
063300991116     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
063400991116     C                   EXSR      OUTFUN
063500991116     C*
063600991116     C                   ENDSR
063700991116     C*------------------------------------------------------------------------*
063800991116     C* CARMSG - CARICA MSG NEL SUBFILE MSG E LO INVIA ALLA CODA MSG DEL PGM
063900991116     C*------------------------------------------------------------------------*
064000991116     C     CARMSG        BEGSR
064100991116     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
064200991116     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
064300991116     C     MSGULT        IFEQ      0
064400991116     C                   MOVEL     '1'           *IN40
064500991116     C                   WRITE     EC10DVK
064600991116     C                   MOVEL     '0'           *IN40
064700991116     C                   ENDIF
064800991116     C* INVIA MESSAGGIO ALLA CODA DEL PGM
064900991116     C                   MOVEL     $MSG          DSMSMS
065000991116     C                   CALL      'XMSG'        XMSGPA
065100991116     C     DSMSER        IFEQ      0
065200991116     C                   MOVEL     DSMSKE        VMCKEY
065300991116     C                   ENDIF
065400991116     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
065500991116     C                   ADD       1             MSGULT            4 0
065600991116     C                   WRITE     EC10DVM
065700991116     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
065800991116     C                   MOVEL     *BLANKS       $MSG
065900991116     C                   ENDSR
066000991116     C*------------------------------------------------------------------------*
066100991116     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
066200991116     C*------------------------------------------------------------------------*
066300991116     C     OUTMSG        BEGSR
066400991116     C* VISUALIZZAZIONE MESSAGGIO
066500991116     C                   MOVEL     '1'           *IN44
066600991116     C                   WRITE     EC10DVK
066700991116     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
066800991116     C                   Z-ADD     *ZEROS        MSGULT
066900991116     C                   ENDSR
067000991116     C*------------------------------------------------------------------------*
067100991116     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
067200991116     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
067300991116     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
067400991116     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
067500991116     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
067600991116     C*------------------------------------------------------------------------*
067700991116     C     CARFUN        BEGSR
067800991116     C                   Z-ADD     *ZEROS        FUNULT            5 0
067900991116     C     1             DO        31            I                 5 0
068000991116     C                   MOVEL     FUN(I)        FUNRIG
068100991116     C                   MOVEL     FUNFUN        FFU(I)
068200991116     C                   MOVEL     FUNDES        FDE(I)
068300020923     C     DUTLPO        IFEQ      'S'
068400991116     C     $TGES         IFEQ      'G'
068500991116     C                   MOVEL     FUNASG        FAT(I)
068600991116     C                   ELSE
068700991116     C                   MOVEL     FUNASI        FAT(I)
068800991116     C                   ENDIF
068900991116     C                   ELSE
069000991116     C     $TGES         IFEQ      'G'
069100991116     C                   MOVEL     FUNAFG        FAT(I)
069200991116     C                   ELSE
069300991116     C                   MOVEL     FUNAFI        FAT(I)
069400991116     C                   ENDIF
069500991116     C                   ENDIF
069600991116     C     FAT(I)        IFEQ      'E'
069700991116     C                   MOVEL     USER          WEDP              3
069800991116     C     WEDP          IFEQ      'EDP'
069900991116     C                   MOVEL     'S'           FAT(I)
070000991116     C                   ELSE
070100991116     C                   MOVEL     'N'           FAT(I)
070200991116     C                   ENDIF
070300991116     C                   ENDIF
070400991116     C                   ENDDO
070500991116     C                   ENDSR
070600991116     C*------------------------------------------------------------------------*
070700991116     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
070800991116     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
070900991116     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
071000991116     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
071100991116     C*------------------------------------------------------------------------*
071200991116     C     OUTFUN        BEGSR
071300991116     C     1             DO        8             I
071400991116     C                   MOVEL     *BLANKS       FUV(I)
071500991116     C                   ENDDO
071600991116     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
071700991116     C                   Z-ADD     *ZEROS        FUNNAT            2 0
071800991116     C     1             DO        31            I
071900991116     C     FAT(I)        IFEQ      'S'
072000991116     C     FDE(I)        ANDNE     *BLANKS
072100991116     C                   ADD       1             FUNNAT
072200991116     C                   ENDIF
072300991116     C                   ENDDO
072400991116     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
072500991116     C     FUNNAT        IFGT      8
072600991116     C                   MOVEL     'S'           FAT(24)
072700991116     C                   ENDIF
072800991116     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
072900991116     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
073000991116     C                   Z-ADD     8             FUNVIS            5 0
073100991116     C     FAT(23)       IFEQ      'S'
073200991116     C                   SUB       1             FUNVIS
073300991116     C                   ENDIF
073400991116     C     FAT(24)       IFEQ      'S'
073500991116     C                   SUB       1             FUNVIS
073600991116     C                   ENDIF
073700991116     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
073800991116     C                   Z-ADD     *ZEROS        W                 5 0
073900991116     C                   ADD       1             FUNULT
074000991116     C     FUNULT        IFGT      31
074100991116     C                   Z-ADD     1             FUNULT
074200991116     C                   ENDIF
074300991116     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
074400991116     C     FUNULT        DO        31            I
074500991116     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
074600991116     C     W             IFLT      FUNVIS
074700991116     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
074800991116     C     FAT(I)        IFEQ      'S'
074900991116     C     FDE(I)        ANDNE     *BLANKS
075000991116     C     I             IFEQ      23
075100991116     C     I             OREQ      24
075200991116     C                   ELSE
075300991116     C                   ADD       1             W
075400991116     C                   MOVEL     FDE(I)        FUV(W)
075500991116     C                   ENDIF
075600991116     C                   ENDIF
075700991116     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
075800991116     C                   Z-ADD     I             FUNULT
075900991116     C                   ENDIF
076000991116     C                   ENDDO
076100991116     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
076200991116     C                   MOVEL     FUV(1)        VFD001
076300991116     C                   MOVEL     FUV(2)        VFD002
076400991116     C                   MOVEL     FUV(3)        VFD003
076500991116     C                   MOVEL     FUV(4)        VFD004
076600991116     C                   MOVEL     FUV(5)        VFD005
076700991116     C                   MOVEL     FUV(6)        VFD006
076800991116     C     FAT(23)       IFEQ      'N'
076900991116     C     FAT(24)       ANDEQ     'N'
077000991116     C                   MOVEL     FUV(7)        VFD007
077100991116     C                   MOVEL     FUV(8)        VFD008
077200991116     C                   ELSE
077300991116     C     FAT(23)       IFEQ      'S'
077400991116     C     FAT(24)       ANDEQ     'S'
077500991116     C                   MOVEL     FDE(23)       VFD007
077600991116     C                   MOVEL     FDE(24)       VFD008
077700991116     C                   ELSE
077800991116     C                   MOVEL     FUV(7)        VFD007
077900991116     C     FAT(23)       IFEQ      'S'
078000991116     C                   MOVEL     FDE(23)       VFD008
078100991116     C                   ELSE
078200991116     C                   MOVEL     FDE(24)       VFD008
078300991116     C                   ENDIF
078400991116     C                   ENDIF
078500991116     C                   ENDIF
078600991116     C                   MOVEL     '0'           $TESVD
078700991116     C                   ENDSR
078800991116     C*------------------------------------------------------------------------*
078900991116     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
079000991116     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
079100991116     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
079200991116     C*                   - ERRORE SE FUNZIONE NON ATTIVA
079300991116     C*------------------------------------------------------------------------*
079400991116     C     CONFUN        BEGSR
079500991116     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
079600991116     C     1             DO        31            I
079700991116     C     KEY           IFEQ      KEYS(I)
079800991116     C* CONTROLLO SE IL TASTO E' ATTIVO
079900991116     C     FAT(I)        IFNE      'S'
080000991116     C                   MOVEL     MSG(1)        $MSG
080100991116     C                   EXSR      CARMSG
080200991116     C                   MOVEL     '1'           $ERR
080300991116     C                   ENDIF
080400991116     C                   ENDIF
080500991116     C                   ENDDO
080600991116     C                   ENDSR
080700991116     O*------------------------------------------------------------------------*
080800991116** - MSG -
080900991116TREC10R- Tasto funzionale non valido.                                          1
081000991116TREC10R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
081100991116TREC10R- Opzione non valida.                                                   3
081200991116TREC10R- Opzione abilitata ma non gestita. Avvertire CED.                      4
081300991116TREC10R- Inizio elenco.                                                        5
081400991116TREC10R- Fine elenco.                                                          6
081500991116TREC10R- 7
081600991116TREC10R- 8
081700991117TREC10R- Immettere uno dei valori possibili                                    9
081800991116TREC10R- Scegliere il tipo ordinamento                                        10
081900991116TREC10R- Filiale errata                                                       11
082000991116TREC10R- Cliente errato                                                       12
082100991116TREC10R- Data errata                                                          13
082200991116TREC10R- L' AL deve essere maggiore o uguale al DAL                           14
082300991116TREC10R- Operare almeno una selezione                                         15
082400991116TREC10R- Per l'ordinamento prescelto, selezionare filiale                     16
082500991116TREC10R- Per l'ordinamento prescelto, selezionare il cliente                  17
082600991116TREC10R- Per l'ordinamento prescelto, selezionare la data spedizione          18
082700991116TREC10R- Anno errato                                                          19
082800991117TREC10R- Sistema informativo bolle errato                                     20
082900991116** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
083000991116F1    -                   -    NN     NN
083100991116F2    -                   -    NN     NN
083200991116F3    -F3=Fine            -    SS     SS
083300991116F4    -                   -    NN     NN
083400991116F5    -                   -    NN     NN
083500110314F6    -F6=Lancio Int.     -    SS     SS
083600110314F7    -                   -    NN     NN
083700110314F8    -F8=Lancio Batch    -    SS     SS
083800991116F9    -                   -    NN     NN
083900991116F10   -                   -    NN     NN
084000991116F11   -                   -    NN     NN
084100991116F12   -F12=Ritorno        -    SS     SS
084200991116F13   -                   -    NN     NN
084300991116F14   -                   -    NN     NN
084400991116F15   -                   -    NN     NN
084500991116F16   -                   -    NN     NN
084600991116F17   -                   -    NN     NN
084700991116F18   -                   -    NN     NN
084800991116F19   -                   -    NN     NN
084900991116F20   -                   -    NN     NN
085000991116F21   -                   -    NN     NN
085100991116F22   -                   -    NN     NN
085200991116F23   -                   -    NN     NN
085300991116F24   -F24=Altri tasti    -    NN     NN
085400991116ENTER -                   -    SS     SS
085500991116ROLLUP-                   -    NN     NN
085600991116ROLLDN-                   -    NN     NN
085700991116HELP  -                   -    NN     NN
085800991116PRINT -                   -    NN     NN
085900991116BSPACE-                   -    NN     NN
086000991116CLEAR -                   -    NN     NN
