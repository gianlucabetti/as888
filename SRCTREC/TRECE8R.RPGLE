000100950616     H*------------------------------------------------------------------------*
000200950616     H* RICALCOLA LE COMPETENZE DI CONTO ECONOMICO - RICHIESTA PARAMETRI       *
000300950616     H*------------------------------------------------------------------------*
000400950616     H DECEDIT('0,') DATEDIT(*DMY.)
000500950616     F*------------------------------------------------------------------------*
000600950616     F* DATA BASE
000700950616     F*------------------------------------------------------------------------*
000800950616     FTRECE8D   CF   E             WORKSTN INFDS(DEVDS1)
000900950616     F                                     SFILE(ECE8DVM:MSGULT)
001000950616     FAZORG01L  IF   E           K DISK
001100950616     FCNACO00F  IF   E           K DISK
001200950616     D*------------------------------------------------------------------------*
001300950616     D* SCHIERE
001400950616     D*------------------------------------------------------------------------*
001500950616     D*-------------------
001600950616     D* SCHIERE A TEMPO DI COMPILAZIONE  PER VIDEO
001700950616     D*-------------------
001800950616     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001900950616     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
002000950616     D*-------------------
002100950616     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002200950616     D*-------------------
002300950616     D FFU             S              6    DIM(31)
002400950616     D FDE             S             19    DIM(31)
002500950616     D FAT             S              1    DIM(31)
002600950616     D*-------------------
002700950616     D* TABELLA DESCRIZIONI A VIDEO
002800950616     D*-------------------
002900950616     D FUV             S             19    DIM(8)
003000950616     D*-------------------
003100950616     D* TABELLA TASTI FUNZIONALI PREMUTI
003200950616     D*-------------------
003300950616     D KEYS            S              1    DIM(40)
003400950616     D*------------------
003500950616     D* TABELLE CAPICONTO
003600950616     D*------------------
003700950616     D RI              S              4  0 DIM(4)
003800950616     D*------------------------------------------------------------------------*
003900950616     D* INPUT
004000950616     D*------------------------------------------------------------------------*
004100950616     D*-------------------
004200950616     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004300950616     D*-------------------
004400950616     D DSFUN         E DS
004500950616     D  FUNRIG                 1     40
004600950616     D*-------------------
004700950616     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004800950616     D*-------------------
004900950616     D DSMSG         E DS
005000950616     D*-------------------
005100950616     D* DS PER GESTIONE TASTI FUNZIONALI
005200950616     D*-------------------
005300950616     D DSKEYS        E DS
005400950616     D  KEYRIG                 1     40
005500950616     D*-------------------
005600950616     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005700950616     D*-------------------
005800950616     D                SDS
005900950616     D  USER                 254    263
006000950616     D*-------------------
006100950616     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006200950616     D*-------------------
006300950616     D DEVDS1          DS
006400950616     D  KEY                  369    369
006500950616     D  CURSOR               370    371
006600950616     D*-------------------
006700950616     D* ARCHITETTURA
006800950616     D*-------------------
006900950616     D KPJBA         E DS
007000950616     D*-------------------
007100020923     D* DS REPERIMENTO DATI UTENTE
007200020923     D*-------------------
007300020923     D TIBS34DS      E DS
007400020923     D DDATIUTE      E DS
007500020923     D AZUTEDS       E DS                  extname(AZUTE00F)
007600950616     D*-------------------
007700950616     D* CONTROLLO DATA (8)
007800950616     D*-------------------
007900950616     D WLBDA8          DS                  INZ
008000950616     D  G08DAT                 1      8  0
008100950616     D  G08INV                 9     16  0
008200950616     D  G08ERR                17     17
008300950616     D  G08TGI                18     22  0
008400950616     D*-------------------
008500950616     D* PARAMETRI LANCIO
008600950616     D*-------------------
008700950616     D PARAM           DS                  INZ
008800950616     D  PARORD                 1      1
008900950616     D  PARFIL                 2      4  0
009000980206     D  PARKS1                 5     11  0
009100970606     D  PARDS1                12     19  0
009200950616     D  PARDS2                20     27  0
009300970606     D  PARDF1                28     35  0
009400970606     D  PARDF2                36     43  0
009500980206     D  PARKS2                44     50  0
009600020611     D  PARTAS                51     51
009700950616     C*------------------------------------------------------------------------*
009800950616     C* MAIN LINE
009900950616     C*------------------------------------------------------------------------*
010000950616     C*
010100950616     C* CICLO FINO A CHE:
010200950616DO  1C     $FINE         DOUEQ     '1'
010300950616     C*
010400950616     C                   MOVEL     '0'           $ERR
010500950616     C*
010600950616     C* CONTROLLA GESTIONE VIDEO
010700950616IF  2C     $GES          IFEQ      'V1'
010800950616     C                   EXSR      GESV1
010900950616E   2C                   ENDIF
011000950616     C*
011100950616E   1C                   ENDDO
011200950616     C*
011300950616     C                   SETON                                        LR
011400950616     C*------------------------------------------------------------------------*
011500950616     C* GESV1 - GESTIONE PRIMO VIDEO
011600950616     C*------------------------------------------------------------------------*
011700950616     C     GESV1         BEGSR
011800950616     C*
011900950616IF  1C     $TESVD        IFEQ      '1'
012000950616     C                   EXSR      CARV1
012100950616E   1C                   ENDIF
012200950616     C*
012300950616IF  1C     MSGULT        IFEQ      *ZEROS
012400950616     C                   MOVEL     *BLANKS       $MSG
012500950616     C                   EXSR      CARMSG
012600950616E   1C                   ENDIF
012700950616     C                   EXSR      OUTMSG
012800950616     C*
012900950616     C                   EXFMT     ECE8DV1
013000950616     C*
013100950616     C                   EXSR      CONFUN
013200950616     C*
013300950616IF  1C     $ERR          IFEQ      '0'
013400950616SL  2C                   SELECT
013500950616     C     KEY           WHENEQ    ENTER
013600950616     C                   EXSR      ENTV1
013700950616     C     KEY           WHENEQ    F6
013800950616     C                   EXSR      F06GES
013900950616     C                   OTHER
014000950616     C                   EXSR      FUNSTD
014100950616E   2C                   ENDSL
014200950616E   1C                   ENDIF
014300950616     C*
014400950616     C                   ENDSR
014500950616     C*------------------------------------------------------------------------*
014600950616     C* CARV1 - CARICA PRIMO VIDEO
014700950616     C*------------------------------------------------------------------------*
014800950616     C     CARV1         BEGSR
014900950616     C*
015000950616     C                   MOVEL     'V1'          $GES
015100950616     C*
015200950616     C                   EXSR      CARFUN
015300950616     C                   EXSR      OUTFUN
015400950616     C*
015500950616     C                   WRITE     ECE8DVT
015600950616     C                   WRITE     ECE8DVF
015700950616     C*
015800950616     C                   ENDSR
015900950616     C*------------------------------------------------------------------------*
016000950616     C* ENTV1 - GESTIONE TASTO ENTER PRIMO VIDEO
016100950616     C*------------------------------------------------------------------------*
016200950616     C     ENTV1         BEGSR
016300950616     C*
016400950616     C* CONTROLLA I CAMPI IMMESSI
016500950616     C                   EXSR      DECODI
016600950616     C*
016700950616     C                   ENDSR
016800950616     C*------------------------------------------------------------------------*
016900950616     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
017000950616     C*------------------------------------------------------------------------*
017100950616     C     F06GES        BEGSR
017200950616     C*
017300950616     C* CONTROLLA I CAMPI IMMESSI
017400950616     C                   EXSR      DECODI
017500950616     C*
017600950616     C* NON CI SONO ERRORI
017700950616IF  1C     $ERR          IFEQ      '0'
017800950616     C*
017900950616     C* LANCIA IL PROGRAMMA
018000950616     C                   MOVEL     V1CORD        PARORD
018100950616     C                   Z-ADD     V1CFIL        PARFIL
018200980206     C                   Z-ADD     V1CKS1        PARKS1
018300980206     C                   Z-ADD     V1CKS2        PARKS2
018400950616     C                   Z-ADD     DS1V1C        PARDS1
018500950616     C                   Z-ADD     DS2V1C        PARDS2
018600970606     C                   Z-ADD     DF1V1C        PARDF1
018700970606     C                   Z-ADD     DF2V1C        PARDF2
018800020611     C                   MOVEL     V1CTAS        PARTAS
018900950616     C*
019000950616     C                   MOVEL     PARAM         KPJBU
019100950616     C                   MOVEL     'ECE9'        KCOAZ
019200950616     C                   CALL      'BCH10'
019300950616     C                   PARM                    KPJBA
019400950616     C*
019500950616E   1C                   ENDIF
019600950616     C*
019700950616     C                   ENDSR
019800950616     C*------------------------------------------------------------------------*
019900950616     C* DECODI - CONTROLLA CAMPI E DECODIFICA
020000950616     C*------------------------------------------------------------------------*
020100950616     C     DECODI        BEGSR
020200950616     C*
020300950616     C* REIMPOSTA INIDICATORI
020400950616     C                   SETOFF                                       606162
020500980206     C                   SETOFF                                       636465
020600020611     C                   SETOFF                                       666768
020700950616     C*
020800950616DO  1C                   DO
020900950616     C*--------------------
021000950616     C* ORDINAMENTO
021100950616     C*--------------------
021200950616IF  2C     V1CORD        IFEQ      '1'
021300950616     C     V1CORD        OREQ      '2'
021400950616     C     V1CORD        OREQ      '3'
021500950616X   2C                   ELSE
021600950616     C                   MOVEL     '1'           $ERR
021700950616     C                   MOVEL     MSG(10)       $MSG
021800950616     C                   EXSR      CARMSG
021900950616     C                   SETON                                        60
022000950616     C                   LEAVE
022100950616E   2C                   ENDIF
022200020611     C*--------------------
022300020611     C* BOLLE IN ...
022400020611     C*--------------------
022500020611IF  2C     V1CTAS        IFEQ      '0'
022600020611     C     V1CTAS        OREQ      '1'
022700020611X   2C                   ELSE
022800020611     C                   MOVEL     '1'           $ERR
022900020611     C                   MOVEL     MSG(09)       $MSG
023000020611     C                   EXSR      CARMSG
023100020611     C                   SETON                                        68
023200020611     C                   LEAVE
023300020611E   2C                   ENDIF
023400950616     C*--------------------
023500950616     C* FILIALE
023600950616     C*--------------------
023700950616IF  2C     V1CFIL        IFGT      *ZEROS
023800950616     C                   Z-ADD     V1CFIL        WFIL
023900950616     C                   MOVEL     *BLANKS       WDFIL
024000950616     C                   EXSR      CHKFIL
024100950616     C                   MOVEL     WDFIL         V1DFIL
024200950616IF  3C     $ERR          IFEQ      '1'
024300950616     C                   MOVEL     '1'           $ERR
024400950616     C                   MOVEL     MSG(11)       $MSG
024500950616     C                   EXSR      CARMSG
024600950616     C                   SETON                                        61
024700950616     C                   LEAVE
024800950616E   3C                   ENDIF
024900950616X   2C                   ELSE
025000950616IF  3C     V1CORD        IFEQ      '1'
025100950616     C                   MOVEL     '1'           $ERR
025200950616     C                   MOVEL     MSG(16)       $MSG
025300950616     C                   EXSR      CARMSG
025400950616     C                   SETON                                        61
025500950616     C                   LEAVE
025600950616E   3C                   ENDIF
025700950616E   2C                   ENDIF
025800950616     C*--------------------
025900980206     C* CLIENTI
026000950616     C*--------------------
026100980206IF  2C*---       V1CKSC    IFGT *ZEROS                     *SELEZIONATO
026200980206     C*---                 Z-ADDV1CKSC    WKSC
026300980206     C*---                 MOVEL*BLANKS   WDKSC
026400980206     C*---                 EXSR CHKKSC
026500980206     C*---                 MOVELWDKSC     V1DKSC
026600980206IF  3C*---       $ERR      IFEQ '1'
026700980206     C*---                 MOVEL'1'       $ERR
026800980206     C*---                 MOVELMSG,12    $MSG
026900980206     C*---                 EXSR CARMSG
027000980206     C*---                 SETON                     62
027100980206     C*---                 LEAVE
027200980206E   3C*---                 ENDIF
027300980206X   2C*---                 ELSE                            *NON SELEZIONATO
027400980206IF  3C*---       V1CORD    IFEQ '2'
027500980206     C*---                 MOVEL'1'       $ERR
027600980206     C*---                 MOVELMSG,17    $MSG
027700980206     C*---                 EXSR CARMSG
027800980206     C*---                 SETON                     62
027900980206     C*---                 LEAVE
028000980206E   3C*---                 ENDIF
028100980206E   2C*---                 ENDIF
028200980206     C*
028300980206IF  2C     V1CKS1        IFGT      *ZEROS
028400980206     C                   Z-ADD     V1CKS1        WKSC
028500980206     C                   MOVEL     *BLANKS       WDKSC
028600980206     C                   EXSR      CHKKSC
028700980206     C                   MOVEL     WDKSC         V1DKS1
028800980206X   2C                   ELSE
028900980206IF  3C     V1CORD        IFEQ      '2'
029000980206     C                   MOVEL     '1'           $ERR
029100980206     C                   MOVEL     MSG(17)       $MSG
029200980206     C                   EXSR      CARMSG
029300980206     C                   SETON                                        62
029400980206     C                   LEAVE
029500980206E   3C                   ENDIF
029600980206E   2C                   ENDIF
029700980206     C*
029800980206IF  2C     V1CKS2        IFGT      *ZEROS
029900980206     C                   Z-ADD     V1CKS2        WKSC
030000980206     C                   MOVEL     *BLANKS       WDKSC
030100980206     C                   EXSR      CHKKSC
030200980206     C                   MOVEL     WDKSC         V1DKS2
030300980206X   2C                   ELSE
030400980206IF  3C     V1CORD        IFEQ      '2'
030500980206     C                   MOVEL     '1'           $ERR
030600980206     C                   MOVEL     MSG(17)       $MSG
030700980206     C                   EXSR      CARMSG
030800980206     C                   SETON                                        67
030900980206     C                   LEAVE
031000980206E   3C                   ENDIF
031100980206E   2C                   ENDIF
031200980206     C*
031300980206IF  2C     V1CKS2        IFLT      V1CKS1
031400980206     C                   MOVEL     '1'           $ERR
031500980206     C                   MOVEL     MSG(14)       $MSG
031600980206     C                   EXSR      CARMSG
031700980206     C                   SETON                                        6267
031800980206     C                   LEAVE
031900980206E   2C                   ENDIF
032000950616     C*--------------------
032100950616     C* DATA SPEDIZIONE
032200950616     C*--------------------
032300950616     C     *LIKE         DEFINE    V1CDS1        DS1V1C
032400950616     C                   Z-ADD     *ZEROS        DS1V1C
032500950616IF  2C     V1CDS1        IFGT      *ZEROS
032600950616     C                   Z-ADD     V1CDS1        G08DAT
032700950616     C                   Z-ADD     *ZEROS        G08INV
032800950616     C                   MOVEL     '0'           G08ERR
032900950616     C                   CALL      'XSRDA8'
033000950616     C                   PARM                    WLBDA8
033100950616     C                   Z-ADD     G08DAT        V1CDS1
033200950616     C                   Z-ADD     G08INV        DS1V1C
033300950616IF  3C     G08ERR        IFEQ      '1'
033400950616     C                   MOVEL     '1'           $ERR
033500950616     C                   MOVEL     MSG(13)       $MSG
033600950616     C                   EXSR      CARMSG
033700950616     C                   SETON                                        63
033800950616     C                   LEAVE
033900950616E   3C                   ENDIF
034000950616X   2C                   ELSE
034100950616IF  3C     V1CORD        IFEQ      '1'
034200950616     C     V1CORD        OREQ      '3'
034300950616     C                   MOVEL     '1'           $ERR
034400950616     C                   MOVEL     MSG(18)       $MSG
034500950616     C                   EXSR      CARMSG
034600950616     C                   SETON                                        63
034700950616     C                   LEAVE
034800950616E   3C                   ENDIF
034900950616E   2C                   ENDIF
035000950616     C*
035100950616     C     *LIKE         DEFINE    V1CDS2        DS2V1C
035200950616     C                   Z-ADD     *ZEROS        DS2V1C
035300950616IF  2C     V1CDS2        IFGT      *ZEROS
035400950616     C     V1CDS1        ORGT      *ZEROS
035500950616     C                   Z-ADD     V1CDS2        G08DAT
035600950616     C                   Z-ADD     *ZEROS        G08INV
035700950616     C                   MOVEL     '0'           G08ERR
035800950616     C                   CALL      'XSRDA8'
035900950616     C                   PARM                    WLBDA8
036000950616     C                   Z-ADD     G08DAT        V1CDS2
036100950616     C                   Z-ADD     G08INV        DS2V1C
036200950616IF  3C     G08ERR        IFEQ      '1'
036300950616     C                   MOVEL     '1'           $ERR
036400950616     C                   MOVEL     MSG(13)       $MSG
036500950616     C                   EXSR      CARMSG
036600950616     C                   SETON                                        64
036700950616     C                   LEAVE
036800950616E   3C                   ENDIF
036900950616X   2C                   ELSE
037000950616IF  3C     V1CORD        IFEQ      '1'
037100950616     C     V1CORD        OREQ      '3'
037200950616     C                   MOVEL     '1'           $ERR
037300950616     C                   MOVEL     MSG(18)       $MSG
037400950616     C                   EXSR      CARMSG
037500950616     C                   SETON                                        64
037600950616     C                   LEAVE
037700950616E   3C                   ENDIF
037800950616E   2C                   ENDIF
037900950616     C*
038000950616IF  2C     DS1V1C        IFGT      DS2V1C
038100950616     C                   MOVEL     '1'           $ERR
038200950616     C                   MOVEL     MSG(14)       $MSG
038300950616     C                   EXSR      CARMSG
038400950616     C                   SETON                                        6364
038500950616     C                   LEAVE
038600950616E   2C                   ENDIF
038700970606     C*--------------------
038800970606     C* DATA DATTURA
038900970606     C*--------------------
039000970606     C     *LIKE         DEFINE    V1CDF1        DF1V1C
039100970606     C                   Z-ADD     *ZEROS        DF1V1C
039200970606IF  2C     V1CDF1        IFGT      *ZEROS
039300970606     C                   Z-ADD     V1CDF1        G08DAT
039400970606     C                   Z-ADD     *ZEROS        G08INV
039500970606     C                   MOVEL     '0'           G08ERR
039600970606     C                   CALL      'XSRDA8'
039700970606     C                   PARM                    WLBDA8
039800970606     C                   Z-ADD     G08DAT        V1CDF1
039900970606     C                   Z-ADD     G08INV        DF1V1C
040000970606IF  3C     G08ERR        IFEQ      '1'
040100970606     C                   MOVEL     '1'           $ERR
040200970606     C                   MOVEL     MSG(13)       $MSG
040300970606     C                   EXSR      CARMSG
040400970606     C                   SETON                                        65
040500970606     C                   LEAVE
040600970606E   3C                   ENDIF
040700970606E   2C                   ENDIF
040800970606     C*
040900970606     C     *LIKE         DEFINE    V1CDF2        DF2V1C
041000970606     C                   Z-ADD     *ZEROS        DF2V1C
041100970606IF  2C     V1CDF2        IFGT      *ZEROS
041200970606     C     V1CDF1        ORGT      *ZEROS
041300970606     C                   Z-ADD     V1CDF2        G08DAT
041400970606     C                   Z-ADD     *ZEROS        G08INV
041500970606     C                   MOVEL     '0'           G08ERR
041600970606     C                   CALL      'XSRDA8'
041700970606     C                   PARM                    WLBDA8
041800970606     C                   Z-ADD     G08DAT        V1CDF2
041900970606     C                   Z-ADD     G08INV        DF2V1C
042000970606IF  3C     G08ERR        IFEQ      '1'
042100970606     C                   MOVEL     '1'           $ERR
042200970606     C                   MOVEL     MSG(13)       $MSG
042300970606     C                   EXSR      CARMSG
042400970606     C                   SETON                                        66
042500970606     C                   LEAVE
042600970606E   3C                   ENDIF
042700970606E   2C                   ENDIF
042800970606     C*
042900970606IF  2C     DF1V1C        IFGT      DF2V1C
043000970606     C                   MOVEL     '1'           $ERR
043100970606     C                   MOVEL     MSG(14)       $MSG
043200970606     C                   EXSR      CARMSG
043300970606     C                   SETON                                        6566
043400970606     C                   LEAVE
043500970606E   2C                   ENDIF
043600950616E   1C                   ENDDO
043700950616     C*
043800950616     C                   ENDSR
043900950616     C*------------------------------------------------------------------------*
044000950616     C* CHKFIL - CONTROLLA E DECODIFCA LA FILIALE
044100950616     C*------------------------------------------------------------------------*
044200950616     C     CHKFIL        BEGSR
044300950616     C*
044400950616     C     *LIKE         DEFINE    V1CFIL        WFIL
044500950616     C     *LIKE         DEFINE    V1DFIL        WDFIL
044600950616     C*
044700950616     C                   Z-ADD     WFIL          KORFIL
044800950616     C     KEYORG        CHAIN     AZORG01L                           99
044900950616IF  1C     *IN99         IFEQ      *ON
045000950616     C                   MOVEL     '1'           $ERR
045100950616X   1C                   ELSE
045200950616     C                   MOVEL     ORGDES        WDFIL
045300950616E   1C                   ENDIF
045400950616     C*
045500950616     C                   ENDSR
045600950616     C*------------------------------------------------------------------------*
045700950616     C* CHKKSC - CONTROLLA E DECODIFCA IL CLIENTE
045800950616     C*------------------------------------------------------------------------*
045900950616     C     CHKKSC        BEGSR
046000950616     C*
046100980206     C     *LIKE         DEFINE    V1CKS1        WKSC
046200980206     C     *LIKE         DEFINE    V1DKS1        WDKSC
046300950616     C*
046400020923     C                   Z-ADD     1             KACKUT
046500020923     C                   Z-ADD     DUTKCI        KACKCC
046600950616     C                   Z-ADD     WKSC          KACKSC
046700950616     C     KEYACO        CHAIN     CNACO00F                           99
046800950616IF  1C     *IN99         IFEQ      *ON
046900980206     C                   MOVEL     *BLANKS       WDKSC
047000950616X   1C                   ELSE
047100950616     C                   MOVEL     ACORAG        WDKSC
047200950616E   1C                   ENDIF
047300950616     C*
047400950616     C                   ENDSR
047500950616     C*------------------------------------------------------------------------*
047600950616     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
047700950616     C*------------------------------------------------------------------------*
047800950616     C     IMPVID        BEGSR
047900950616     C*
048000950616     C                   MOVEL     '1'           V1CORD
048100020611     C                   MOVEL     '1'           V1CTAS
048200950616     C*
048300950616     C                   ENDSR
048400020923     C*-------------------------------------------------------------------------
048500020923     C* REPERISCE I DATI UTENTE
048600020923     C*-------------------------------------------------------------------------
048700020923     C     REPDATIUTE    BEGSR
048800020923     C*
048900020923     C* INIZIALIZZA VARIABILI DI WRK
049000020923     C                   CLEAR                   TIBS34DS
049100020923     C                   CLEAR                   AZUTEDS
049200020923     C                   CLEAR                   DDATIUTE
049300020923     C*
049400020923     C     *DTAARA       DEFINE    §azute        azuteds
049500020923     C     *DTAARA       DEFINE    §datiute      ddatiute
049600020923     C                   IN(E)     *DTAARA
049700020923if  1C                   IF        %Error
049800020923     c                   EVAL      I34Tla = 'L'
049900020923     C                   CALL      'TIBS34R'
050000020923     C                   PARM                    Tibs34Ds
050100020923     C                   IN        *DTAARA
050200020923e   1C                   ENDIF
050300020923     C*
050400020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
050500020923     C                   MOVEL(P)  RSUT          DSFIRS           20
050600020923     C*
050700020923     C                   ENDSR
050800950616     C*------------------------------------------------------------------------*
050900950616     C* *INZSR  - ROUTINE INIZIALE
051000950616     C*------------------------------------------------------------------------*
051100950616     C     *INZSR        BEGSR
051200950616     C*--------------------
051300950616     C* RICEVIMENTO PARAMETRI
051400950616     C*--------------------
051500950616     C     *ENTRY        PLIST
051600950616     C                   PARM                    KPJBA
051700950616     C*--------------------
051800950616     C* CAMPI RIFERITI AL DATA BASE
051900950616     C*--------------------
052000950616     C     *LIKE         DEFINE    ORGFIL        KORFIL
052100950616     C     *LIKE         DEFINE    ACOKUT        KACKUT
052200950616     C     *LIKE         DEFINE    ACOKCC        KACKCC
052300950616     C     *LIKE         DEFINE    ACOKSC        KACKSC
052400950616     C*--------------------
052500950616     C* CHIAVI DI LETTURA
052600950616     C*--------------------
052700950616     C* LETTURA AZORG01L - COMPLETA
052800950616     C     KEYORG        KLIST
052900950616     C                   KFLD                    KORFIL
053000950616     C* LETTURA CNACO00F - COMPLETA
053100950616     C     KEYACO        KLIST
053200950616     C                   KFLD                    KACKUT
053300950616     C                   KFLD                    KACKCC
053400950616     C                   KFLD                    KACKSC
053500950616     C*
053600950616     C* ESEGUE OPERAZIONI INIZIALI STANDARD
053700950616     C                   EXSR      INZSTD
053800950616     C*
053900950616     C* IMPOSTA CAMPI VIDEO
054000950616     C                   EXSR      IMPVID
054100950616     C*
054200950616     C* EMETTE FORMATO VIDEO
054300950616     C                   EXSR      CARV1
054400950616     C*
054500950616     C                   ENDSR
054600950616     C*------------------------------------------------------------------------*
054700950616     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
054800950616     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
054900950616     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
055000950616     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
055100950616     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
055200950616     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
055300950616     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
055400950616     C*------------------------------------------------------------------------*
055500950616     C* INZSTD - ROUTINE INIZIALE STANDARD
055600950616     C*------------------------------------------------------------------------*
055700950616     C     INZSTD        BEGSR
055800950616     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
055900950616     C                   CALL      'XKEYS'
056000950616     C                   PARM                    DSKEYS
056100950616     C                   MOVEA     KEYRIG        KEYS
056200950616     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
056300950616     C     XMSGPA        PLIST
056400950616     C                   PARM                    DSMSMS
056500950616     C                   PARM                    DSMSPG
056600950616     C                   PARM                    DSMSKE
056700950616     C                   PARM                    DSMSER
056800950616     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
056900950616     C                   MOVEL     'TRECE8R'     DSMSPG
057000950616     C                   MOVEL     'TRECE8R'     VMCQUE
057100950616     C                   MOVEL     *BLANKS       VMCKEY
057200950616     C                   MOVEL     *BLANKS       DSMSMS
057300950616     C* INIZIALIZZO FLAG CONTROLLO PGM
057400950616     C                   MOVEL     '0'           $ERR              1
057500950616     C                   MOVEL     '0'           $F03              1
057600950616     C                   MOVEL     *BLANKS       $GES              2
057700950616     C                   MOVEL     *BLANKS       $MSG             80
057800950616     C                   MOVEL     '0'           $FINE             1
057900950616     C                   MOVEL     '1'           $TESVD            1
058000950616     C                   MOVEL     'I'           $TGES             1
058100950616     C* LANCIO ROUTINE PER VERIFICARE SE
058200950616     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
058300950616     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
058400020923     C                   EXSR      REPDATIUTE
058500950616     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
058600950616     C                   BITOFF    '01234567'    POSMI1            1
058700950616     C                   BITOFF    '01234567'    POSMI2            1
058800950616     C                   BITOFF    '01234567'    POSMA1            1
058900950616     C                   BITOFF    '01234567'    POSMA2            1
059000950616     C                   BITON     '34'          POSMI1            1
059100950616     C                   BITON     '7'           POSMI2            1
059200950616     C                   BITON     '34'          POSMA1            1
059300950616     C                   BITON     '0'           POSMA2            1
059400950616     C                   MOVE      POSMI2        POSMIN            2
059500950616     C                   MOVEL     POSMI1        POSMIN
059600950616     C                   MOVE      POSMA2        POSMAX            2
059700950616     C                   MOVEL     POSMA1        POSMAX
059800950616     C                   ENDSR
059900950616     C*------------------------------------------------------------------------*
060000950616     C* FUNSTD - LANCIA SUBROUTINE STANDARD
060100950616     C*------------------------------------------------------------------------*
060200950616     C     FUNSTD        BEGSR
060300950616     C                   SELECT
060400950616     C     KEY           WHENEQ    F3
060500950616     C                   EXSR      F03STD
060600950616     C     KEY           WHENEQ    F12
060700950616     C                   EXSR      F12STD
060800950616     C     KEY           WHENEQ    F24
060900950616     C                   EXSR      F24STD
061000950616     C                   OTHER
061100950616     C                   MOVEL     MSG(2)        $MSG
061200950616     C                   EXSR      CARMSG
061300950616     C                   ENDSL
061400950616     C                   ENDSR
061500950616     C*------------------------------------------------------------------------*
061600950616     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
061700950616     C*------------------------------------------------------------------------*
061800950616     C     F03STD        BEGSR
061900950616     C*
062000950616     C* IMPOSTO FLAG DI FINE PROGRAMMA
062100950616     C                   MOVEL     '1'           $FINE
062200950616     C*
062300950616     C                   ENDSR
062400950616     C*------------------------------------------------------------------------*
062500950616     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
062600950616     C*------------------------------------------------------------------------*
062700950616     C     F12STD        BEGSR
062800950616     C* IMPOSTO FLAG DI FIN PGM
062900950616     C                   MOVEL     '1'           $FINE
063000950616     C                   ENDSR
063100950616     C*------------------------------------------------------------------------*
063200950616     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
063300950616     C*------------------------------------------------------------------------*
063400950616     C     F24STD        BEGSR
063500950616     C*
063600950616     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
063700950616     C                   EXSR      OUTFUN
063800950616     C*
063900950616     C                   ENDSR
064000950616     C*------------------------------------------------------------------------*
064100950616     C* CARMSG - CARICA MSG NEL SUBFILE MSG E LO INVIA ALLA CODA MSG DEL PGM
064200950616     C*------------------------------------------------------------------------*
064300950616     C     CARMSG        BEGSR
064400950616     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
064500950616     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
064600950616     C     MSGULT        IFEQ      0
064700950616     C                   MOVEL     '1'           *IN40
064800950616     C                   WRITE     ECE8DVK
064900950616     C                   MOVEL     '0'           *IN40
065000950616     C                   ENDIF
065100950616     C* INVIA MESSAGGIO ALLA CODA DEL PGM
065200950616     C                   MOVEL     $MSG          DSMSMS
065300950616     C                   CALL      'XMSG'        XMSGPA
065400950616     C     DSMSER        IFEQ      0
065500950616     C                   MOVEL     DSMSKE        VMCKEY
065600950616     C                   ENDIF
065700950616     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
065800950616     C                   ADD       1             MSGULT            4 0
065900950616     C                   WRITE     ECE8DVM
066000950616     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
066100950616     C                   MOVEL     *BLANKS       $MSG
066200950616     C                   ENDSR
066300950616     C*------------------------------------------------------------------------*
066400950616     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
066500950616     C*------------------------------------------------------------------------*
066600950616     C     OUTMSG        BEGSR
066700950616     C* VISUALIZZAZIONE MESSAGGIO
066800950616     C                   MOVEL     '1'           *IN44
066900950616     C                   WRITE     ECE8DVK
067000950616     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
067100950616     C                   Z-ADD     *ZEROS        MSGULT
067200950616     C                   ENDSR
067300950616     C*------------------------------------------------------------------------*
067400950616     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
067500950616     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
067600950616     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
067700950616     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
067800950616     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
067900950616     C*------------------------------------------------------------------------*
068000950616     C     CARFUN        BEGSR
068100950616     C                   Z-ADD     *ZEROS        FUNULT            5 0
068200950616     C     1             DO        31            I                 5 0
068300950616     C                   MOVEL     FUN(I)        FUNRIG
068400950616     C                   MOVEL     FUNFUN        FFU(I)
068500950616     C                   MOVEL     FUNDES        FDE(I)
068600020923     C     DUTLPO        IFEQ      'S'
068700950616     C     $TGES         IFEQ      'G'
068800950616     C                   MOVEL     FUNASG        FAT(I)
068900950616     C                   ELSE
069000950616     C                   MOVEL     FUNASI        FAT(I)
069100950616     C                   ENDIF
069200950616     C                   ELSE
069300950616     C     $TGES         IFEQ      'G'
069400950616     C                   MOVEL     FUNAFG        FAT(I)
069500950616     C                   ELSE
069600950616     C                   MOVEL     FUNAFI        FAT(I)
069700950616     C                   ENDIF
069800950616     C                   ENDIF
069900950616     C     FAT(I)        IFEQ      'E'
070000950616     C                   MOVEL     USER          WEDP              3
070100950616     C     WEDP          IFEQ      'EDP'
070200950616     C                   MOVEL     'S'           FAT(I)
070300950616     C                   ELSE
070400950616     C                   MOVEL     'N'           FAT(I)
070500950616     C                   ENDIF
070600950616     C                   ENDIF
070700950616     C                   ENDDO
070800950616     C                   ENDSR
070900950616     C*------------------------------------------------------------------------*
071000950616     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
071100950616     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
071200950616     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
071300950616     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
071400950616     C*------------------------------------------------------------------------*
071500950616     C     OUTFUN        BEGSR
071600950616     C     1             DO        8             I
071700950616     C                   MOVEL     *BLANKS       FUV(I)
071800950616     C                   ENDDO
071900950616     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
072000950616     C                   Z-ADD     *ZEROS        FUNNAT            2 0
072100950616     C     1             DO        31            I
072200950616     C     FAT(I)        IFEQ      'S'
072300950616     C     FDE(I)        ANDNE     *BLANKS
072400950616     C                   ADD       1             FUNNAT
072500950616     C                   ENDIF
072600950616     C                   ENDDO
072700950616     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
072800950616     C     FUNNAT        IFGT      8
072900950616     C                   MOVEL     'S'           FAT(24)
073000950616     C                   ENDIF
073100950616     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
073200950616     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
073300950616     C                   Z-ADD     8             FUNVIS            5 0
073400950616     C     FAT(23)       IFEQ      'S'
073500950616     C                   SUB       1             FUNVIS
073600950616     C                   ENDIF
073700950616     C     FAT(24)       IFEQ      'S'
073800950616     C                   SUB       1             FUNVIS
073900950616     C                   ENDIF
074000950616     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
074100950616     C                   Z-ADD     *ZEROS        W                 5 0
074200950616     C                   ADD       1             FUNULT
074300950616     C     FUNULT        IFGT      31
074400950616     C                   Z-ADD     1             FUNULT
074500950616     C                   ENDIF
074600950616     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
074700950616     C     FUNULT        DO        31            I
074800950616     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
074900950616     C     W             IFLT      FUNVIS
075000950616     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
075100950616     C     FAT(I)        IFEQ      'S'
075200950616     C     FDE(I)        ANDNE     *BLANKS
075300950616     C     I             IFEQ      23
075400950616     C     I             OREQ      24
075500950616     C                   ELSE
075600950616     C                   ADD       1             W
075700950616     C                   MOVEL     FDE(I)        FUV(W)
075800950616     C                   ENDIF
075900950616     C                   ENDIF
076000950616     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
076100950616     C                   Z-ADD     I             FUNULT
076200950616     C                   ENDIF
076300950616     C                   ENDDO
076400950616     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
076500950616     C                   MOVEL     FUV(1)        VFD001
076600950616     C                   MOVEL     FUV(2)        VFD002
076700950616     C                   MOVEL     FUV(3)        VFD003
076800950616     C                   MOVEL     FUV(4)        VFD004
076900950616     C                   MOVEL     FUV(5)        VFD005
077000950616     C                   MOVEL     FUV(6)        VFD006
077100950616     C     FAT(23)       IFEQ      'N'
077200950616     C     FAT(24)       ANDEQ     'N'
077300950616     C                   MOVEL     FUV(7)        VFD007
077400950616     C                   MOVEL     FUV(8)        VFD008
077500950616     C                   ELSE
077600950616     C     FAT(23)       IFEQ      'S'
077700950616     C     FAT(24)       ANDEQ     'S'
077800950616     C                   MOVEL     FDE(23)       VFD007
077900950616     C                   MOVEL     FDE(24)       VFD008
078000950616     C                   ELSE
078100950616     C                   MOVEL     FUV(7)        VFD007
078200950616     C     FAT(23)       IFEQ      'S'
078300950616     C                   MOVEL     FDE(23)       VFD008
078400950616     C                   ELSE
078500950616     C                   MOVEL     FDE(24)       VFD008
078600950616     C                   ENDIF
078700950616     C                   ENDIF
078800950616     C                   ENDIF
078900950616     C                   MOVEL     '0'           $TESVD
079000950616     C                   ENDSR
079100950616     C*------------------------------------------------------------------------*
079200950616     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
079300950616     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
079400950616     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
079500950616     C*                   - ERRORE SE FUNZIONE NON ATTIVA
079600950616     C*------------------------------------------------------------------------*
079700950616     C     CONFUN        BEGSR
079800950616     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
079900950616     C     1             DO        31            I
080000950616     C     KEY           IFEQ      KEYS(I)
080100950616     C* CONTROLLO SE IL TASTO E' ATTIVO
080200950616     C     FAT(I)        IFNE      'S'
080300950616     C                   MOVEL     MSG(1)        $MSG
080400950616     C                   EXSR      CARMSG
080500950616     C                   MOVEL     '1'           $ERR
080600950616     C                   ENDIF
080700950616     C                   ENDIF
080800950616     C                   ENDDO
080900950616     C                   ENDSR
081000950616     O*------------------------------------------------------------------------*
081100950616** - MSG -
081200950616TRECE8R- Tasto funzionale non valido.                                          1
081300950616TRECE8R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
081400950616TRECE8R- Opzione non valida.                                                   3
081500950616TRECE8R- Opzione abilitata ma non gestita. Avvertire CED.                      4
081600950616TRECE8R- Inizio elenco.                                                        5
081700950616TRECE8R- Fine elenco.                                                          6
081800950616TRECE8R- 7
081900950616TRECE8R- 8
082000020611TRECE8R- Immettere uno dei valori possibili                                   09
082100950616TRECE8R- Scegliere il tipo ordinamento                                        10
082200950616TRECE8R- Filiale errata                                                       11
082300950616TRECE8R- Cliente errato                                                       12
082400950616TRECE8R- Data errata                                                          13
082500980206TRECE8R- L' AL deve essere maggiore o uguale al DAL                           14
082600950616TRECE8R- Operare almeno una selezione                                         15
082700950616TRECE8R- Per l'ordinamento prescelto, selezionare filiale                     16
082800980206TRECE8R- Per l'ordinamento prescelto, selezionare il cliente                  17
082900950616TRECE8R- Per l'ordinamento prescelto, selezionare la data spedizione          18
083000980206TRECE8R- 19
083100950616TRECE8R- 20
083200950616** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
083300950616F1    -                   -    NN     NN
083400950616F2    -                   -    NN     NN
083500950616F3    -F3=Fine            -    SS     SS
083600950616F4    -                   -    NN     NN
083700950616F5    -                   -    NN     NN
083800950616F6    -F6=Lancio          -    SS     SS
083900950616F7    -                   -    NN     NN
084000950616F8    -                   -    NN     NN
084100950616F9    -                   -    NN     NN
084200950616F10   -                   -    NN     NN
084300950616F11   -                   -    NN     NN
084400950616F12   -F12=Ritorno        -    SS     SS
084500950616F13   -                   -    NN     NN
084600950616F14   -                   -    NN     NN
084700950616F15   -                   -    NN     NN
084800950616F16   -                   -    NN     NN
084900950616F17   -                   -    NN     NN
085000950616F18   -                   -    NN     NN
085100950616F19   -                   -    NN     NN
085200950616F20   -                   -    NN     NN
085300950616F21   -                   -    NN     NN
085400950616F22   -                   -    NN     NN
085500950616F23   -                   -    NN     NN
085600950616F24   -F24=Altri tasti    -    NN     NN
085700950616ENTER -                   -    SS     SS
085800950616ROLLUP-                   -    NN     NN
085900950616ROLLDN-                   -    NN     NN
086000950616HELP  -                   -    NN     NN
086100950616PRINT -                   -    NN     NN
086200950616BSPACE-                   -    NN     NN
086300950616CLEAR -                   -    NN     NN
