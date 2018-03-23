000100000000     H*------------------------------------------------------------------------*
000200950323     H*  Nome programma:  TREC88R                                              *
000300950323     H*  Descrizione   :  Gestione file degli errori in creazione C/Economico  *
000400950323     H*  Autore        :  RAFFAELLA (EDPRN)                                    *
000500000000     H*------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700000000     F*------------------------------------------------------------------------*
000800000000     F* DATA BASE
000900000000     F*------------------------------------------------------------------------*
001000950323     FTREC88D   CF   E             WORKSTN INFDS(DEVDS1)
001100950323     F                                     SFILE(EC88DVM:MSGULT)
001200000000     F*-----------
001300950323     F* DAT+TIM+PRG
001400000000     F*-----------
001500950323     FECERR01L  UF   E           K DISK
001600990812     FTITAS30C  IF   E           K DISK
001700990812     F                                     IGNORE(TITAS000)
001800950323     FTNCSB03L  IF   E           K DISK
001900000000     FTABEL00F  IF   E           K DISK
002000000000     D*------------------------------------------------------------------------*
002100000000     D* SCHIERE
002200000000     D*------------------------------------------------------------------------*
002300000000     D* MESSAGGI E FUNZIONI
002400000000     D MSG             S             78    DIM(25) CTDATA PERRCD(1)
002500000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
002600000000     D* DECODIFICA OPZIONE RICHIAMATA
002700000000     D DOPZ            S             16    DIM(5) CTDATA PERRCD(1)
002800000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002900000000     D FFU             S              6    DIM(31)
003000000000     D FDE             S             19    DIM(31)
003100000000     D FAT             S              1    DIM(31)
003200000000     D* TABELLA DESCRIZIONI A VIDEO
003300000000     D FUV             S             19    DIM(8)
003400000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003500000000     D KEYS            S              1    DIM(40)
003600950519     D* DECODIFICA A VIDEO
003700950519     D DEC             S             15    DIM(5) CTDATA PERRCD(1)
003800000000     D*------------------------------------------------------------------------*
003900000000     D* INPUT
004000000000     D*------------------------------------------------------------------------*
004100000000     D*-------------------
004200000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004300000000     D*-------------------
004400000000     D DSFUN         E DS
004500000000     D  FUNRIG                 1     40
004600000000     D*-------------------
004700000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004800000000     D*-------------------
004900000000     D DSMSG         E DS
005000000000     D*-------------------
005100020923     D* DS REPERIMENTO DATI UTENTE
005200020923     D*-------------------
005300020923     D TIBS34DS      E DS
005400020923     D DDATIUTE      E DS
005500020923     D AZUTEDS       E DS                  extname(AZUTE00F)
005600000000     D*-------------------
005700000000     D* DS PER GESTIONE TASTI FUNZIONALI
005800000000     D*-------------------
005900000000     D DSKEYS        E DS
006000000000     D  KEYRIG                 1     40
006100000000     D*-------------------
006200000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006300000000     D*-------------------
006400000000     D                SDS
006500000000     D  USER                 254    263
006600000000     D*-------------------
006700000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006800000000     D*-------------------
006900000000     D DEVDS1          DS
007000000000     D* CHIAVE FUNZIONALE BATTUTA
007100000000     D  KEY                  369    369
007200000000     D* POSIZIONE CURSORE (IN HEX)
007300000000     D  CURSOR               370    371
007400000000     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
007500000000     D  FRTSFL               378    379B 0
007600000000     D*-------------------
007700000000     D* ARCHITETTURA
007800000000     D*-------------------
007900000000     D KPJBA         E DS
008000000000     D*---------------------
008100000000     D* DS DI PROCEDURA
008200000000     D*---------------------
008300950323     D DS87          E DS                  EXTNAME(TREC87DS)
008400000000     D*------------------
008500000000     D* DS CONTROLLO DATE (8)
008600000000     D*------------------
008700000000     D WLBDA8          DS                  INZ
008800000000     D  G08DAT                 1      8  0
008900000000     D  G08INV                 9     16  0
009000000000     D  G08ERR                17     17
009100000000     D  G08TGI                18     22  0
009200991223     D*------------------
009300991223     D* S/COMPOSIZIONE DATA
009400991223     D*------------------
009500991223     D                 DS
009600991223     D  DSANN6                 1      2  0
009700991223     D  DSMES6                 3      4  0
009800991223     D  DSGIO6                 5      6  0
009900991223     D  DSDAT6                 1      6  0
010000991223     D                 DS
010100991223     D  DSGIO                  1      2  0
010200991223     D  DSMES                  3      4  0
010300991223     D  DSANN                  5      6  0
010400991223     D  DSDAT                  1      6  0
010500950310     D*------------------
010600950323     D* DS TIPO BOLLA
010700950310     D*------------------
010800950323     D DSTB          E DS
010900950323     D*-------------------
011000950323     D* DS VALORIZZAZIONE IMMAGINE -PRIMA- E -DOPO- ERRORE
011100950323     D*-------------------
011200950323     D DS$CSB        E DS
011300950323     D  DCPDS1                 1    250
011400950323     D  DCPDS2               251    500
011500950323     D  DCDDS1               501    750
011600950323     D  DCDDS2               751   1000
011700000000     C*------------------------------------------------------------------------*
011800000000     C* MAIN LINE
011900000000     C*------------------------------------------------------------------------*
012000000000     C*
012100000000     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
012200000000IF  1C     $FINE         IFEQ      '1'
012300000000     C*
012400000000     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
012500950323     C                   MOVEL     '1'           D87ERR
012600000000     C*
012700000000     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
012800000000X   1C                   ELSE
012900000000     C*
013000000000     C* CICLO FINO A CHE:
013100000000DO  2C     $FINE         DOWEQ     '0'
013200000000     C                   MOVEL     '0'           $ERR
013300000000     C*
013400000000     C* CONTROLLA GESTIONE VIDEO
013500000000IF  3C     $GES          IFEQ      'V1'
013600000000     C                   EXSR      GESV1
013700000000E   3C                   ENDIF
013800000000     C*
013900000000E   2C                   ENDDO
014000000000E   1C                   ENDIF
014100000000     C*
014200950327     C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
014300950327IF  1C     $F03          IFEQ      '1'
014400950323     C                   MOVEL     '1'           D87F03
014500000000E   1C                   ENDIF
014600950327     C*
014700950327     C* SE E' STATO PREMUTO F12 IMPOSTO FLAG DELLA DS DI PROCEDURA
014800950519IF  1C*---       $FINE     IFEQ '1'
014900950519     C*---                 MOVEL'1'       D87F12
015000950519E   1C*---                 ENDIF
015100950327     C*
015200950323     C                   MOVEL     DS87          KPJBU
015300000000     C                   SETON                                        LR
015400000000     C*------------------------------------------------------------------------*
015500000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
015600000000     C*------------------------------------------------------------------------*
015700000000     C     CARV1         BEGSR
015800000000     C*
015900000000     C                   MOVEL     'V1'          $GES
016000000000     C*
016100000000     C                   EXSR      CARFUN
016200000000     C                   EXSR      OUTFUN
016300000000     C*
016400950323     C                   WRITE     EC88DVT
016500950323     C                   WRITE     EC88DVF
016600000000     C*
016700000000     C                   ENDSR
016800000000     C*------------------------------------------------------------------------*
016900000000     C* GESV1  - GESTIONE PRIMO VIDEO
017000000000     C*------------------------------------------------------------------------*
017100000000     C     GESV1         BEGSR
017200000000     C*
017300000000IF  1C     $TESVD        IFEQ      '1'
017400000000     C                   EXSR      CARV1
017500000000E   1C                   ENDIF
017600000000     C*
017700000000IF  1C     MSGULT        IFNE      *ZEROS
017800000000     C                   EXSR      OUTMSG
017900000000E   1C                   ENDIF
018000000000     C*
018100950323     C                   EXFMT     EC88DV1
018200000000     C                   EXSR      CONFUN
018300000000     C*
018400000000IF  1C     $ERR          IFEQ      '0'
018500000000SL  2C                   SELECT
018600000000     C     KEY           WHENEQ    ENTER
018700000000     C                   EXSR      ENTV1
018800000000     C     KEY           WHENEQ    F6
018900000000     C                   EXSR      F06GES
019000000000     C                   OTHER
019100000000     C                   EXSR      FUNSTD
019200000000E   2C                   ENDSL
019300000000E   1C                   ENDIF
019400000000     C*
019500000000     C                   ENDSR
019600000000     C*------------------------------------------------------------------------*
019700000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
019800000000     C*------------------------------------------------------------------------*
019900000000     C     ENTV1         BEGSR
020000000000     C*
020100000000     C* CONTROLLA I CAMPI IMMESSI
020200000000     C                   EXSR      DECODI
020300000000     C*
020400000000     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
020500950519IF  1C     D87OP1        IFEQ      'O05'
020600950519     C                   MOVEL     '1'           $FINE
020700950519E   1C                   ENDIF
020800000000     C*
020900000000     C                   ENDSR
021000000000     C*------------------------------------------------------------------------*
021100000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> CONFERMA OPERAZIONE
021200000000     C*------------------------------------------------------------------------*
021300000000     C     F06GES        BEGSR
021400000000     C*
021500000000     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
021600950323IF  1C     D87OP1        IFEQ      'O02'
021700000000     C                   EXSR      MODIFI
021800000000E   1C                   ENDIF
021900000000     C*
022000000000     C                   ENDSR
022100000000     C*------------------------------------------------------------------------*
022200000000     C* MODIFI - FUNZIONE DI MODIFICA
022300000000     C*------------------------------------------------------------------------*
022400000000     C     MODIFI        BEGSR
022500000000     C*
022600000000     C* CONTROLLA I CAMPI IMMESSI
022700000000     C                   EXSR      DECODI
022800950323     C*
022900950323     C* LEGGE IL CONTRASSEGNO
023000950323IF  1C     $ERR          IFEQ      '0'
023100950323     C                   Z-ADD     V1CAAS        KCSAAS
023200950323     C                   Z-ADD     V1CLNP        KCSLNP
023300950323     C                   Z-ADD     V1CNRS        KCSNRS
023400950323     C                   Z-ADD     V1CNSP        KCSNSP
023500950323     C     KEYCSB        CHAIN     TNCSB03L                           98
023600950323     C*
023700950323     C* TRASFERISCE L'IMMAGINE DEL CONTRASSEGNO IN QUELLA DELL'ERRORE
023800950323     C                   EXSR      APDCSQ
023900950323     C                   EXSR      ADDCSQ
024000950323     C                   EXSR      CPDCSQ
024100950323     C                   EXSR      CDDCSQ
024200950323     C                   MOVEL     DCPDS1        ERRRP1
024300950323     C                   MOVEL     DCPDS2        ERRRP2
024400950323     C                   MOVEL     DCDDS1        ERRRP1
024500950323     C                   MOVEL     DCDDS2        ERRRP2
024600950323     C*
024700950323     C* AGGIORNA IL FILE DEGLI ERRORI
024800950323     C                   EXCEPT    AGGERR
024900000000     C*
025000000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
025100000000     C                   MOVEL     '1'           $FINE
025200000000     C*
025300000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
025400950323     C                   MOVEL     MSG(18)       D87MSG
025500000000E   1C                   ENDIF
025600000000     C*
025700000000     C                   ENDSR
025800000000     C*------------------------------------------------------------------------*
025900000000     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
026000000000     C*------------------------------------------------------------------------*
026100000000     C     DECODI        BEGSR
026200000000     C*
026300000000     C* REIMPOSTA INDICATORI DI ERRORE
026400950323     C                   SETOFF                                       6061
026500000000     C*--------------------
026600950323     C* TIPO BOLLA
026700000000     C*--------------------
026800950519     C* SOLO SE MODIFICA
026900950519IF  1C     D87OP1        IFEQ      'O02'
027000950519IF  2C     $ERR          IFEQ      '0'
027100950323     C                   MOVEL     V1CTBL        WTBL
027200950323     C                   EXSR      CHKTBL
027300950323     C                   MOVEL     WTBL          V1CTBL
027400950519IF  3C     $ERR          IFEQ      '1'
027500950323     C                   MOVEL     MSG(7)        $MSG
027600000000     C                   EXSR      CARMSG
027700950323     C                   SETON                                        60
027800950519E   3C                   ENDIF
027900950519E   2C                   ENDIF
028000950323     C*--------------------
028100950323     C* BOLLA INESISTENTE
028200950323     C*--------------------
028300950519IF  2C     $ERR          IFEQ      '0'
028400950323     C                   Z-ADD     V1CAAS        KTAAAS
028500950323     C                   Z-ADD     V1CLNP        KTALNP
028600950323     C                   Z-ADD     V1CNRS        KTANRS
028700950323     C                   Z-ADD     V1CNSP        KTANSP
028800950323     C                   MOVEL     V1CTBL        KTATBL
028900990812     C     KEYTAS        CHAIN     TITAS30C                           97
029000950519IF  3C     *IN97         IFEQ      *ON
029100950323     C                   MOVEL     '1'           $ERR
029200950323     C                   MOVEL     MSG(8)        $MSG
029300950323     C                   EXSR      CARMSG
029400950323     C                   SETON                                        6061
029500950519E   3C                   ENDIF
029600950519E   2C                   ENDIF
029700950323     C*--------------------
029800950323     C* CONTRASSEGNO INESISTENTE
029900950323     C*--------------------
030000950519IF  2C     $ERR          IFEQ      '0'
030100950323     C                   Z-ADD     V1CAAS        KCSAAS
030200950323     C                   Z-ADD     V1CLNP        KCSLNP
030300950323     C                   Z-ADD     V1CNRS        KCSNRS
030400950323     C                   Z-ADD     V1CNSP        KCSNSP
030500950323     C     KEYCSB        CHAIN     TNCSB03L                           97
030600950519IF  3C     *IN97         IFEQ      *ON
030700950323     C     CSBTBL        ORNE      V1CTBL
030800950323     C                   MOVEL     '1'           $ERR
030900950323     C                   MOVEL     MSG(9)        $MSG
031000950323     C                   EXSR      CARMSG
031100950519     C                   SETON                                        6061
031200950519E   3C                   ENDIF
031300950519E   2C                   ENDIF
031400950323     C*
031500950519E   1C                   ENDIF
031600950519     C*
031700000000     C                   ENDSR
031800950323     C*------------------------------------------------------------------------*
031900950323     C* CHKTBL - CONTROLLA IL TIPO BOLLA
032000950323     C*------------------------------------------------------------------------*
032100950323     C     CHKTBL        BEGSR
032200950323     C*
032300950323     C     *LIKE         DEFINE    V1CTBL        WTBL
032400950323     C*
032500950323     C* CONTROLLA SE E' STATO INSERITO '?'
032600950323     C     '?'           SCAN      WTBL                                   99
032700950323     C*
032800950323     C* E' STATO INSERITO '?', LANCIA IL PGM DI INTERROGAZIONE TABELLE
032900950323IF  1C     *IN99         IFEQ      *ON
033000020923     C                   Z-ADD     1             §KUT
033100950323     C                   MOVEL     'TB'          §COD
033200950323     C                   MOVEL     *BLANKS       §KEY
033300950323     C                   MOVEL     *BLANKS       §DES
033400950323     C                   CALL      'X§TABER'
033500950323     C                   PARM                    §KUT              1 0
033600950323     C                   PARM                    §COD              2
033700950323     C                   PARM                    §KEY              8
033800950323     C                   PARM                    §DES             30
033900950323     C                   MOVEL     §KEY          WTBL
034000950323     C                   MOVEL     '1'           $TESVD
034100950323X   1C                   ELSE
034200950323     C*
034300950323     C* CONTROLLA CHE IL RECORD ESISTA IN TABELLA
034400020923     C                   Z-ADD     1             KTBKUT
034500950323     C                   MOVEL     'TB'          KTBCOD
034600950323     C                   MOVEL     *BLANKS       KTBKEY
034700950323     C                   MOVEL     WTBL          KTBKEY
034800950323     C     KEYTAB        CHAIN     TABEL00F                           99
034900950323IF  2C     *IN99         IFEQ      *ON
035000950323     C                   MOVEL     '1'           $ERR
035100950323E   2C                   ENDIF
035200950323E   1C                   ENDIF
035300950323     C*
035400950323     C                   ENDSR
035500000000     C*------------------------------------------------------------------------*
035600000000     C* RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
035700000000     C*------------------------------------------------------------------------*
035800000000     C     RECVID        BEGSR
035900000000     C*
036000950323     C                   Z-ADD     ERRAAS        V1CAAS
036100950323     C                   Z-ADD     ERRLNP        V1CLNP
036200950323     C                   Z-ADD     ERRNRS        V1CNRS
036300950323     C                   Z-ADD     ERRNSP        V1CNSP
036400950323     C                   MOVEL     ERRTBL        V1CTBL
036500950323     C                   Z-ADD     ERRDAT        G08INV
036600000000     C                   Z-ADD     *ZEROS        G08DAT
036700000000     C                   MOVEL     '3'           G08ERR
036800000000     C                   CALL      'XSRDA8'
036900000000     C                   PARM                    WLBDA8
037000950323     C                   Z-ADD     G08DAT        V1CDAT
037100950323     C                   Z-ADD     ERRTIM        V1CTIM
037200950323     C                   Z-ADD     ERRPRG        V1CPRG
037300950519IF  1C     ERRSTS        IFEQ      'I'
037400950519     C                   MOVEL     DEC(1)        V1DSTS
037500950519E   1C                   ENDIF
037600950519IF  1C     ERRSTS        IFEQ      'D'
037700950519     C                   MOVEL     DEC(2)        V1DSTS
037800950519E   1C                   ENDIF
037900950519IF  1C     ERRSTS        IFEQ      'U'
038000950519     C                   MOVEL     DEC(3)        V1DSTS
038100950519E   1C                   ENDIF
038200950519     C                   MOVEL     ERRFIL        V1CFIL
038300991223     C                   Z-ADD     ERRDAM        DSDAT6
038400991223     C                   Z-ADD     DSGIO6        DSGIO
038500991223     C                   Z-ADD     DSMES6        DSMES
038600991223     C                   Z-ADD     DSANN6        DSANN
038700991223     C                   Z-ADD     DSDAT         V1CDAM
038800950323     C                   Z-ADD     ERRORM        V1CORM
038900950323     C                   MOVEL     ERRUSR        V1CUSR
039000950323     C                   MOVEL     ERRPGF        V1CPGF
039100950323     C                   MOVEL     ERRCER        V1CCER
039200950323     C                   MOVEL     ERRDER        V1CDER
039300000000     C*
039400000000     C                   ENDSR
039500020923     C*-------------------------------------------------------------------------
039600020923     C* REPERISCE I DATI UTENTE
039700020923     C*-------------------------------------------------------------------------
039800020923     C     REPDATIUTE    BEGSR
039900020923     C*
040000020923     C* INIZIALIZZA VARIABILI DI WRK
040100020923     C                   CLEAR                   TIBS34DS
040200020923     C                   CLEAR                   AZUTEDS
040300020923     C                   CLEAR                   DDATIUTE
040400020923     C*
040500020923     C     *DTAARA       DEFINE    §azute        azuteds
040600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
040700020923     C                   IN(E)     *DTAARA
040800020923if  1C                   IF        %Error
040900020923     c                   EVAL      I34Tla = 'L'
041000020923     C                   CALL      'TIBS34R'
041100020923     C                   PARM                    Tibs34Ds
041200020923     C                   IN        *DTAARA
041300020923e   1C                   ENDIF
041400020923     C*
041500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
041600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
041700020923     C*
041800020923     C                   ENDSR
041900000000     C*------------------------------------------------------------------------*
042000000000     C*  *INZSR - ROUTINE INIZIALE
042100000000     C*------------------------------------------------------------------------*
042200000000     C     *INZSR        BEGSR
042300000000     C*--------------------
042400000000     C* RICEVIMENTO PARAMETRI
042500000000     C*--------------------
042600000000     C     *ENTRY        PLIST
042700000000     C                   PARM                    KPJBA
042800950323     C                   MOVEL     KPJBU         DS87
042900000000     C*--------------------
043000000000     C* CAMPI RIFERITI AL DATA BASE
043100000000     C*--------------------
043200950323     C     *LIKE         DEFINE    ERRDAT        KERDAT
043300950323     C     *LIKE         DEFINE    ERRTIM        KERTIM
043400950323     C     *LIKE         DEFINE    ERRPRG        KERPRG
043500950323     C     *LIKE         DEFINE    CSBAAS        KCSAAS
043600950323     C     *LIKE         DEFINE    CSBLNP        KCSLNP
043700950323     C     *LIKE         DEFINE    CSBNRS        KCSNRS
043800950323     C     *LIKE         DEFINE    CSBNSP        KCSNSP
043900990812     C     *LIKE         DEFINE    TASAAS        KTAAAS
044000000000     C     *LIKE         DEFINE    TASLNP        KTALNP
044100000000     C     *LIKE         DEFINE    TASNRS        KTANRS
044200000000     C     *LIKE         DEFINE    TASNSP        KTANSP
044300000000     C     *LIKE         DEFINE    TASTBL        KTATBL
044400000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
044500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
044600000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
044700000000     C*--------------------
044800000000     C* CHIAVI DI LETTURA
044900000000     C*--------------------
045000950323     C* CHIAVE LETTURA ECERR01L - COMPLETA
045100950323     C     KEYERR        KLIST
045200950323     C                   KFLD                    KERDAT
045300950323     C                   KFLD                    KERTIM
045400950323     C                   KFLD                    KERPRG
045500950323     C*
045600950323     C* CHIAVE LETTURA TNCSB03L - COMPLETA
045700950323     C     KEYCSB        KLIST
045800950323     C                   KFLD                    KCSAAS
045900950323     C                   KFLD                    KCSLNP
046000950323     C                   KFLD                    KCSNRS
046100950323     C                   KFLD                    KCSNSP
046200000000     C*
046300990812     C* CHIAVE LETTURA TITAS30C - COMPLETA
046400000000     C     KEYTAS        KLIST
046500000000     C                   KFLD                    KTAAAS
046600000000     C                   KFLD                    KTALNP
046700000000     C                   KFLD                    KTANRS
046800000000     C                   KFLD                    KTANSP
046900000000     C                   KFLD                    KTATBL
047000000000     C*
047100000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
047200000000     C     KEYTAB        KLIST
047300000000     C                   KFLD                    KTBKUT
047400000000     C                   KFLD                    KTBCOD
047500000000     C                   KFLD                    KTBKEY
047600000000     C*--------------------
047700000000     C* IMPOSTAZIONI VIDEO
047800000000     C*--------------------
047900000000     C*
048000000000     C* D02 --> GESTIONE;  D05 --> INTERROGAZIONE
048100950519IF  1C     D87OP1        IFEQ      'O02'
048200000000     C                   MOVEL     'G'           $TGES             1
048300950519X   1C                   ELSE
048400950519     C                   MOVEL     'I'           $TGES
048500950519E   1C                   ENDIF
048600000000     C*
048700000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
048800000000     C                   EXSR      INZSTD
048900000000     C*
049000000000     C* RAGIONE SOCIALE UTENTE
049100000000     C                   MOVEL     DSFIRS        VTDRSU
049200000000     C*
049300000000     C* DECODIFICA OPZIONE RICHIAMATA
049400950323IF  2C     D87OP1        IFEQ      'O02'
049500000000     C                   MOVEL     DOPZ(2)       VTDFUN
049600000000E   1C                   ENDIF
049700950519IF  2C     D87OP1        IFEQ      'O05'
049800950519     C                   MOVEL     DOPZ(5)       VTDFUN
049900950519     C                   SETON                                        90
050000950519E   1C                   ENDIF
050100000000     C*
050200950323     C* LEGGO IL RECORD: SE INESITENTE O ALLOCATO MANDO ERRORE AL PGM CHIAMANTE
050300950323     C                   Z-ADD     D87DAT        KERDAT
050400950323     C                   Z-ADD     D87TIM        KERTIM
050500950323     C                   Z-ADD     D87PRG        KERPRG
050600950323     C     KEYERR        CHAIN     ECERR01L                           9998
050700950323IF  1C     *IN99         IFEQ      *ON
050800000000     C                   MOVEL     '1'           $ERR
050900000000     C                   MOVEL     '1'           $FINE
051000950323     C                   MOVEL     MSG(20)       D87MSG
051100950323X   1C                   ELSE
051200950323IF  2C     *IN98         IFEQ      *ON
051300000000     C                   MOVEL     '1'           $ERR
051400000000     C                   MOVEL     '1'           $FINE
051500950323     C                   MOVEL     MSG(22)       D87MSG
051600950323X   2C                   ELSE
051700000000     C                   EXSR      RECVID
051800000000     C                   EXSR      DECODI
051900000000     C                   EXSR      CARV1
052000950323E   2C                   ENDIF
052100950323E   1C                   ENDIF
052200000000     C*
052300000000     C                   ENDSR
052400000000     C*------------------------------------------------------------------------*
052500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
052600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
052700000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
052800000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
052900000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
053000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053200000000     C*------------------------------------------------------------------------*
053300000000     C* INZSTD - ROUTINE INIZIALE STANDARD
053400000000     C*------------------------------------------------------------------------*
053500000000     C     INZSTD        BEGSR
053600000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
053700000000     C                   CALL      'XKEYS'
053800000000     C                   PARM                    DSKEYS
053900000000     C                   MOVEA     KEYRIG        KEYS
054000000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
054100000000     C     XMSGPA        PLIST
054200000000     C                   PARM                    DSMSMS
054300000000     C                   PARM                    DSMSPG
054400000000     C                   PARM                    DSMSKE
054500000000     C                   PARM                    DSMSER
054600000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
054700950323     C                   MOVEL     'TREC88R'     DSMSPG
054800950323     C                   MOVEL     'TREC88R'     VMCQUE
054900000000     C                   MOVEL     *BLANKS       VMCKEY
055000000000     C                   MOVEL     *BLANKS       DSMSMS
055100000000     C* INIZIALIZZO FLAG CONTROLLO PGM
055200000000     C                   MOVEL     '0'           $ERR              1
055300000000     C                   MOVEL     '0'           $F03              1
055400000000     C                   MOVEL     *BLANKS       $GES              2
055500000000     C                   MOVEL     *BLANKS       $MSG             80
055600000000     C                   MOVEL     '0'           $FINE             1
055700000000     C                   MOVEL     '0'           $TESVD            1
055800000000     C* LANCIO ROUTINE PER VERIFICARE SE
055900000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
056000000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
056100020923     C                   EXSR      REPDATIUTE
056200000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
056300000000     C                   BITOFF    '01234567'    POSMI1            1
056400000000     C                   BITOFF    '01234567'    POSMI2            1
056500000000     C                   BITOFF    '01234567'    POSMA1            1
056600000000     C                   BITOFF    '01234567'    POSMA2            1
056700000000     C                   BITON     '34'          POSMI1            1
056800000000     C                   BITON     '7'           POSMI2            1
056900000000     C                   BITON     '34'          POSMA1            1
057000000000     C                   BITON     '0'           POSMA2            1
057100000000     C                   MOVE      POSMI2        POSMIN            2
057200000000     C                   MOVEL     POSMI1        POSMIN
057300000000     C                   MOVE      POSMA2        POSMAX            2
057400000000     C                   MOVEL     POSMA1        POSMAX
057500000000     C                   ENDSR
057600000000     C*------------------------------------------------------------------------*
057700000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
057800000000     C*------------------------------------------------------------------------*
057900000000     C     FUNSTD        BEGSR
058000000000     C                   SELECT
058100000000     C     KEY           WHENEQ    F3
058200000000     C                   EXSR      F03STD
058300000000     C     KEY           WHENEQ    F12
058400000000     C                   EXSR      F12STD
058500000000     C     KEY           WHENEQ    F24
058600000000     C                   EXSR      F24STD
058700000000     C                   OTHER
058800000000     C                   MOVEL     MSG(2)        $MSG
058900000000     C                   EXSR      CARMSG
059000000000     C                   ENDSL
059100000000     C                   ENDSR
059200000000     C*------------------------------------------------------------------------*
059300000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
059400000000     C*------------------------------------------------------------------------*
059500000000     C     F03STD        BEGSR
059600000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
059700000000     C                   MOVEL     '1'           $F03
059800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
059900000000     C                   MOVEL     '1'           $FINE
060000000000     C                   ENDSR
060100000000     C*------------------------------------------------------------------------*
060200000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
060300000000     C*------------------------------------------------------------------------*
060400000000     C     F12STD        BEGSR
060500000000     C* IMPOSTO FLAG DI FIN PGM
060600000000     C                   MOVEL     '1'           $FINE
060700000000     C                   ENDSR
060800000000     C*------------------------------------------------------------------------*
060900000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
061000000000     C*------------------------------------------------------------------------*
061100000000     C     F24STD        BEGSR
061200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
061300000000     C                   EXSR      OUTFUN
061400000000     C                   ENDSR
061500000000     C*------------------------------------------------------------------------*
061600000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
061700000000     C*          CODA MESSAGGI DEL PGM
061800000000     C*------------------------------------------------------------------------*
061900000000     C     CARMSG        BEGSR
062000000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
062100000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
062200000000     C     MSGULT        IFEQ      0
062300000000     C                   MOVEL     '1'           *IN40
062400950323     C                   WRITE     EC88DVK
062500000000     C                   MOVEL     '0'           *IN40
062600000000     C                   ENDIF
062700000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
062800000000     C                   MOVEL     $MSG          DSMSMS
062900000000     C                   CALL      'XMSG'        XMSGPA
063000000000     C     DSMSER        IFEQ      0
063100000000     C                   MOVEL     DSMSKE        VMCKEY
063200000000     C                   ENDIF
063300000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
063400000000     C                   ADD       1             MSGULT            4 0
063500950323     C                   WRITE     EC88DVM
063600000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
063700000000     C                   MOVEL     *BLANKS       $MSG
063800000000     C                   ENDSR
063900000000     C*------------------------------------------------------------------------*
064000000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
064100000000     C*------------------------------------------------------------------------*
064200000000     C     OUTMSG        BEGSR
064300000000     C* VISUALIZZAZIONE MESSAGGIO
064400000000     C                   MOVEL     '1'           *IN44
064500950323     C                   WRITE     EC88DVK
064600000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
064700000000     C                   Z-ADD     *ZEROS        MSGULT
064800000000     C                   ENDSR
064900000000     C*------------------------------------------------------------------------*
065000000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
065100000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
065200000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
065300000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
065400000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
065500000000     C*------------------------------------------------------------------------*
065600000000     C     CARFUN        BEGSR
065700000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
065800000000     C     1             DO        31            I                 5 0
065900000000     C                   MOVEL     FUN(I)        FUNRIG
066000000000     C                   MOVEL     FUNFUN        FFU(I)
066100000000     C                   MOVEL     FUNDES        FDE(I)
066200020923     C     DUTLPO        IFEQ      'S'
066300000000     C     $TGES         IFEQ      'G'
066400000000     C                   MOVEL     FUNASG        FAT(I)
066500000000     C                   ELSE
066600000000     C                   MOVEL     FUNASI        FAT(I)
066700000000     C                   ENDIF
066800000000     C                   ELSE
066900000000     C     $TGES         IFEQ      'G'
067000000000     C                   MOVEL     FUNAFG        FAT(I)
067100000000     C                   ELSE
067200000000     C                   MOVEL     FUNAFI        FAT(I)
067300000000     C                   ENDIF
067400000000     C                   ENDIF
067500000000     C     FAT(I)        IFEQ      'E'
067600000000     C                   MOVEL     USER          WEDP              3
067700000000     C     WEDP          IFEQ      'EDP'
067800000000     C                   MOVEL     'S'           FAT(I)
067900000000     C                   ELSE
068000000000     C                   MOVEL     'N'           FAT(I)
068100000000     C                   ENDIF
068200000000     C                   ENDIF
068300000000     C                   ENDDO
068400000000     C                   ENDSR
068500000000     C*------------------------------------------------------------------------*
068600000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
068700000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
068800000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
068900000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
069000000000     C*------------------------------------------------------------------------*
069100000000     C     OUTFUN        BEGSR
069200000000     C     1             DO        8             I
069300000000     C                   MOVEL     *BLANKS       FUV(I)
069400000000     C                   ENDDO
069500000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
069600000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
069700000000     C     1             DO        31            I
069800000000     C     FAT(I)        IFEQ      'S'
069900000000     C     FDE(I)        ANDNE     *BLANKS
070000000000     C                   ADD       1             FUNNAT
070100000000     C                   ENDIF
070200000000     C                   ENDDO
070300000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
070400000000     C     FUNNAT        IFGT      8
070500000000     C                   MOVEL     'S'           FAT(24)
070600000000     C                   ENDIF
070700000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
070800000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
070900000000     C                   Z-ADD     8             FUNVIS            5 0
071000000000     C     FAT(23)       IFEQ      'S'
071100000000     C                   SUB       1             FUNVIS
071200000000     C                   ENDIF
071300000000     C     FAT(24)       IFEQ      'S'
071400000000     C                   SUB       1             FUNVIS
071500000000     C                   ENDIF
071600000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
071700000000     C                   Z-ADD     *ZEROS        W                 5 0
071800000000     C                   ADD       1             FUNULT
071900000000     C     FUNULT        IFGT      31
072000000000     C                   Z-ADD     1             FUNULT
072100000000     C                   ENDIF
072200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
072300000000     C     FUNULT        DO        31            I
072400000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
072500000000     C     W             IFLT      FUNVIS
072600000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
072700000000     C     FAT(I)        IFEQ      'S'
072800000000     C     FDE(I)        ANDNE     *BLANKS
072900000000     C     I             IFEQ      23
073000000000     C     I             OREQ      24
073100000000     C                   ELSE
073200000000     C                   ADD       1             W
073300000000     C                   MOVEL     FDE(I)        FUV(W)
073400000000     C                   ENDIF
073500000000     C                   ENDIF
073600000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
073700000000     C                   Z-ADD     I             FUNULT
073800000000     C                   ENDIF
073900000000     C                   ENDDO
074000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
074100000000     C                   MOVEL     FUV(1)        VFD001
074200000000     C                   MOVEL     FUV(2)        VFD002
074300000000     C                   MOVEL     FUV(3)        VFD003
074400000000     C                   MOVEL     FUV(4)        VFD004
074500000000     C                   MOVEL     FUV(5)        VFD005
074600000000     C                   MOVEL     FUV(6)        VFD006
074700000000     C     FAT(23)       IFEQ      'N'
074800000000     C     FAT(24)       ANDEQ     'N'
074900000000     C                   MOVEL     FUV(7)        VFD007
075000000000     C                   MOVEL     FUV(8)        VFD008
075100000000     C                   ELSE
075200000000     C     FAT(23)       IFEQ      'S'
075300000000     C     FAT(24)       ANDEQ     'S'
075400000000     C                   MOVEL     FDE(23)       VFD007
075500000000     C                   MOVEL     FDE(24)       VFD008
075600000000     C                   ELSE
075700000000     C                   MOVEL     FUV(7)        VFD007
075800000000     C     FAT(23)       IFEQ      'S'
075900000000     C                   MOVEL     FDE(23)       VFD008
076000000000     C                   ELSE
076100000000     C                   MOVEL     FDE(24)       VFD008
076200000000     C                   ENDIF
076300000000     C                   ENDIF
076400000000     C                   ENDIF
076500000000     C                   MOVEL     '1'           $TESVD
076600000000     C                   ENDSR
076700000000     C*------------------------------------------------------------------------*
076800000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
076900000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
077000000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
077100000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
077200000000     C*------------------------------------------------------------------------*
077300000000     C     CONFUN        BEGSR
077400000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
077500000000     C     1             DO        31            I
077600000000     C     KEY           IFEQ      KEYS(I)
077700000000     C* CONTROLLO SE IL TASTO E' ATTIVO
077800000000     C     FAT(I)        IFNE      'S'
077900000000     C                   MOVEL     MSG(1)        $MSG
078000000000     C                   EXSR      CARMSG
078100000000     C                   MOVEL     '1'           $ERR
078200000000     C                   ENDIF
078300000000     C                   ENDIF
078400000000     C                   ENDDO
078500000000     C                   ENDSR
078600950323     C*------------------------------------------------------------------------*
078700950323     C* ROUTINE GENERALIZZATA PASSAGGIO IMMAGINI BOLLA DI SEDE
078800950323     C*------------------------------------------------------------------------*
078900021206      /COPY GAITRASRC/SRCTREC,TRECDC
079000950323     O*------------------------------------------------------------------------*
079100950323     O* AGGIORNAMENTO RECORD ERRORE CON L'IMMAGINE -PRIMA- E -DOPO-
079200950323     O*------------------------------------------------------------------------*
079300950323     OECERR     E            AGGERR
079400950323     O                       ERRRP1
079500950323     O                       ERRRP2
079600950323     O                       ERRRD1
079700950323     O                       ERRRD2
079800000000     O*------------------------------------------------------------------------*
079900950519** ======== SCHIERA: MSG  (MESSAGGI)                       ================
080000950323TREC88R- Tasto funzionale non valido.                                          1
080100950323TREC88R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
080200950323TREC88R- Opzione non valida.                                                   3
080300950323TREC88R- Opzione abilitata ma non gestita. Avvertire CED.                      4
080400950323TREC88R- Inizio elenco.                                                        5
080500950323TREC88R- Fine elenco.                                                          6
080600950323TREC88R- Tipo bolla non valido                                                 7
080700950323TREC88R- Bolla inesistente                                                     8
080800950323TREC88R- Contrassegno inesistente                                              9
080900950323TREC88R- 10
081000950323TREC88R- 11
081100950323TREC88R- 12
081200950323TREC88R- 13
081300950323TREC88R- 14
081400950323TREC88R- 15
081500950323TREC88R- 16
081600950323TREC88R- 17
081700950327TREC88R- Modifica effettuata. Premere F05 per avere la situazione aggiornata  18
081800950323TREC88R- 19
081900950323TREC88R- 20
082000950323TREC88R- 21
082100950609TREC88R- Record momentaneamente allocato: riprovare più tardi                 22
082200950323TREC88R- 23
082300950323TREC88R- 24
082400950323TREC88R- 25
082500000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
082600000000F1    -                   -    NN     NN
082700000000F2    -                   -    NN     NN
082800000000F3    -F3=Fine            -    SS     SS
082900000000F4    -                   -    NN     NN
083000000000F5    -                   -    NN     NN
083100950323F6    -F6=Aggiornamento   -    SN     SN
083200000000F7    -                   -    NN     NN
083300000000F8    -                   -    NN     NN
083400000000F9    -                   -    NN     NN
083500000000F10   -                   -    NN     NN
083600000000F11   -                   -    NN     NN
083700950327F12   -F12=Ritorno        -    SS     SS
083800000000F13   -                   -    NN     NN
083900000000F14   -                   -    NN     NN
084000000000F15   -                   -    NN     NN
084100000000F16   -                   -    NN     NN
084200000000F17   -                   -    NN     NN
084300000000F18   -                   -    NN     NN
084400000000F19   -                   -    NN     NN
084500000000F20   -                   -    NN     NN
084600000000F21   -                   -    NN     NN
084700000000F22   -                   -    NN     NN
084800000000F23   -                   -    NN     NN
084900000000F24   -F24=Altri tasti    -    NN     NN
085000000000ENTER -                   -    SS     SS
085100000000ROLLUP-                   -    NN     NN
085200000000ROLLDN-                   -    NN     NN
085300000000HELP  -                   -    NN     NN
085400000000PRINT -                   -    NN     NN
085500000000BSPACE-                   -    NN     NN
085600000000CLEAR -                   -    NN     NN
085700000000** DOPZ  (OPZIONE RICHIAMATA)
085800000000* INSERIMENTO *                                                 1
085900000000*  VARIAZIONE  *                                                2
086000000000*CANCELLAZIONE*                                                 3
0861000000004
086200000000VISUALIZZAZIONE                                                 5
086300950519** DEC - DECODIFICHE A VIDEO
086400950519INSERIMENTO    1
086500950519CANCELLAZIONE  2
086600950519VARIAZIONE     3
0867009505194
0868009505195
