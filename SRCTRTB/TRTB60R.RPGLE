000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TRTB60R
000300000000      *  Descrizione   :  Gesione tabella riferimenti tariffa di c/e
000400000000      *                   Tabella 2Q
000500000000      *                   Programma visualizzazione sfl di scelta
000600000000      *  Autore        :  STEFANO (SC)
000700000000      *  Data creazione:  01 GIU 1994
000800000000      *------------------------------------------------------------------------*
000900000000     H DECEDIT('0,') DATEDIT(*DMY.)
001000000000     F*------------------------------------------------------------------------*
001100000000     F* DATA BASE
001200000000     F*------------------------------------------------------------------------*
001300000000     FTRTB60D   CF   E             WORKSTN INFDS(DEVDS1)
001400000000     F                                     SFILE(TB60DVS:SFLULT)
001500000000     F                                     SFILE(TB60DVM:MSGULT)
001600000000     FTABEL00F  IF   E           K DISK
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002100000000     D MSG             S             78    DIM(15) CTDATA PERRCD(1)
002200000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)
002300000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)
002400000000     D FUN             S             40    DIM(31)
002500000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002600000000     D FFU             S              6    DIM(31)
002700000000     D FDE             S             19    DIM(31)
002800000000     D FAT             S              1    DIM(31)
002900000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
003000000000     D OPD             S              2    DIM(24)
003100000000     D OPS             S              2    DIM(24)
003200000000     D ODE             S             19    DIM(24)
003300000000     D OAT             S              1    DIM(24)
003400000000     D* TABELLA DESCRIZIONI A VIDEO
003500000000     D FUV             S             19    DIM(8)
003600000000     D OPV             S             19    DIM(8)
003700000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003800000000     D KEYS            S              1    DIM(40)
003900000000     D*------------------------------------------------------------------------*
004000000000     D* INPUT
004100000000     D*------------------------------------------------------------------------*
004200000000     D*---------------------
004300000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
004400000000     D*---------------------
004500000000     D DSOPZ         E DS
004600000000     D  OPZRIG                 1     40
004700000000     D*---------------------
004800000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004900000000     D*---------------------
005000000000     D DSFUN         E DS
005100000000     D  FUNRIG                 1     40
005200000000     D*---------------------
005300000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005400000000     D*---------------------
005500000000     D DSMSG         E DS
005600000000     D*---------------------
005700020923     D* DS REPERIMENTO DATI UTENTE
005800020923     D*-------------------
005900020923     D TIBS34DS      E DS
006000020923     D DDATIUTE      E DS
006100020923     D AZUTEDS       E DS                  extname(AZUTE00F)
006200000000     D*---------------------
006300000000     D* DS PER GESTIONE TASTI FUNZIONALI
006400000000     D*---------------------
006500000000     D DSKEYS        E DS
006600000000     D  KEYRIG                 1     40
006700000000     D*---------------------
006800000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006900000000     D*---------------------
007000000000     D                SDS
007100000000     D  USER                 254    263
007200000000     D*---------------------
007300000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
007400000000     D*---------------------
007500000000     D DEVDS1          DS
007600000000     D  KEY                  369    369
007700000000     D  CURSOR               370    371
007800000000     D  FRTSFL               378    379B 0
007900980130     D*------------------
008000980130     D* DS CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
008100980130     D*------------------
008200980130     D UL12DS        E DS                  EXTNAME(TRUL12DS)
008300000000     D*---------------------
008400000000     D* DS RIFERIMENTI TARIFFA DI C/E
008500000000     D*---------------------
008600000000     D DS2Q          E DS
008700000000     D*---------------------
008800000000     D* DS ESTERNA DI PROCEDURA
008900000000     D*---------------------
009000000000     D DS60          E DS                  EXTNAME(TRTB60DS)
009100000000     D*---------------------
009200000000     D* ARCHITETTURA
009300000000     D*---------------------
009400000000     D KPJBA         E DS
009500000000     C*------------------------------------------------------------------------*
009600000000     C* MAIN LINE
009700000000     C*------------------------------------------------------------------------*
009800000000     C*
009900000000     C* CICLO FINO A CHE:
010000000000DO  1C     $FINE         DOUEQ     '1'
010100000000     C*
010200000000     C                   MOVEL     '0'           $ERR
010300000000     C*
010400000000     C* CONTROLLA GESTIONE VIDEO
010500000000IF  2C     $GES          IFEQ      'VS'
010600000000     C                   EXSR      GESVS
010700000000E   2C                   ENDIF
010800000000     C*
010900000000E   1C                   ENDDO
011000000000     C* FINE PGM
011100000000     C                   SETON                                        LR
011200000000     C*------------------------------------------------------------------------*
011300000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
011400000000     C*------------------------------------------------------------------------*
011500000000     C     CARVS         BEGSR
011600000000     C*
011700000000     C                   MOVEL     'VS'          $GES
011800000000     C*
011900000000     C                   EXSR      CAROPZ
012000000000     C                   EXSR      OUTOPZ
012100000000     C                   MOVEA     FUN2          FUN
012200000000     C                   EXSR      CARFUN
012300000000     C                   EXSR      OUTFUN
012400000000     C*
012500000000     C                   WRITE     TB60DVT
012600000000     C                   WRITE     TB60DVO
012700000000     C                   WRITE     TB60DVF
012800000000     C*
012900000000     C                   EXSR      POSSFL
013000000000     C*
013100000000     C                   ENDSR
013200000000     C*------------------------------------------------------------------------*
013300000000     C* GESVS  - GESTIONE SUBFILE
013400000000     C*------------------------------------------------------------------------*
013500000000     C     GESVS         BEGSR
013600000000     C*
013700000000IF  1C     $TESVD        IFEQ      '1'
013800000000     C                   WRITE     TB60DVT
013900000000     C                   WRITE     TB60DVO
014000000000     C                   WRITE     TB60DVF
014100000000E   1C                   ENDIF
014200000000     C*
014300000000     C                   EXSR      OUTSFL
014400000000     C*
014500000000     C                   EXSR      CONFUN
014600000000     C*
014700000000IF  1C     $ERR          IFEQ      '0'
014800000000SL  2C                   SELECT
014900000000     C     KEY           WHENEQ    F6
015000000000     C                   EXSR      F06GES
015100000000     C                   OTHER
015200000000     C                   EXSR      FUNSFL
015300000000E   2C                   ENDSL
015400000000E   1C                   ENDIF
015500000000     C*
015600000000     C                   ENDSR
015700000000     C*------------------------------------------------------------------------*
015800000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
015900000000     C*------------------------------------------------------------------------*
016000000000     C     F06GES        BEGSR
016100000000     C*
016200000000     C* LANCIO L'INSERIMENTO
016300000000     C                   MOVEL     *BLANKS       WCOD              2
016400000000     C                   MOVEL     'GES'         WOP0              3
016500000000     C                   MOVEL     'F06'         WOP1              3
016600000000     C                   MOVEL     'TRTB61R'     WPGM              8
016700000000     C                   EXSR      LANPGM
016800000000     C*
016900000000     C                   EXSR      POSSFL
017000000000     C*
017100000000     C                   ENDSR
017200000000     C*------------------------------------------------------------------------*
017300000000     C*  GESOPZ - GESTIONE OPZIONI
017400000000     C*------------------------------------------------------------------------*
017500000000     C     GESOPZ        BEGSR
017600000000     C*
017700000000SL  1C                   SELECT
017800000000     C     VSCOPZ        WHENEQ    ' 2'
017900000000     C                   EXSR      O02GES
018000000000     C     VSCOPZ        WHENEQ    ' 4'
018100000000     C                   EXSR      O04GES
018200000000     C     VSCOPZ        WHENEQ    ' 5'
018300000000     C                   EXSR      O05GES
018400000000     C     VSCOPZ        WHENEQ    ' 7'
018500000000     C                   EXSR      O07GES
018600000000     C                   OTHER
018700000000     C                   MOVEL     MSG(4)        $MSG
018800000000     C                   EXSR      CARMSG
018900000000E   1C                   ENDSL
019000000000     C*
019100000000     C                   ENDSR
019200000000     C*------------------------------------------------------------------------*
019300000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
019400000000     C*------------------------------------------------------------------------*
019500000000     C     O02GES        BEGSR
019600000000     C*
019700000000IF  1C     VSCATB        IFEQ      'A'
019800000000     C                   MOVEL     '1'           $ERR
019900000000     C                   MOVEL     MSG(12)       $MSG
020000000000     C                   EXSR      CARMSG
020100000000E   1C                   ENDIF
020200000000     C*
020300000000IF  1C     $ERR          IFEQ      '0'
020400000000     C*
020500000000     C* LANCIO LA MODIFICA
020600000000     C                   MOVEL     VSCCOD        WCOD
020700000000     C                   MOVEL     'GES'         WOP0
020800000000     C                   MOVEL     'O02'         WOP1
020900000000     C                   MOVEL     'TRTB61R'     WPGM
021000000000     C                   EXSR      LANPGM
021100000000     C*
021200000000     C                   Z-ADD     1             SWMODI
021300000000     C*
021400000000E   1C                   ENDIF
021500000000     C*
021600000000     C                   ENDSR
021700000000     C*------------------------------------------------------------------------*
021800000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
021900000000     C*------------------------------------------------------------------------*
022000000000     C     O04GES        BEGSR
022100000000     C*
022200000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
022300000000IF  1C     VSCATB        IFEQ      'A'
022400000000     C                   MOVEL     '1'           $ERR
022500000000     C                   MOVEL     MSG(12)       $MSG
022600000000     C                   EXSR      CARMSG
022700000000E   1C                   ENDIF
022800000000     C*
022900000000IF  1C     $ERR          IFEQ      '0'
023000000000     C*
023100000000     C* LANCIO LA CANCELLAZIONE
023200000000     C                   MOVEL     VSCCOD        WCOD
023300000000     C                   MOVEL     'GES'         WOP0
023400000000     C                   MOVEL     'O04'         WOP1
023500000000     C                   MOVEL     'TRTB61R'     WPGM
023600000000     C                   EXSR      LANPGM
023700000000     C*
023800000000     C                   Z-ADD     1             SWMODI
023900000000     C*
024000000000E   1C                   ENDIF
024100000000     C*
024200000000     C                   ENDSR
024300000000     C*------------------------------------------------------------------------*
024400000000     C*  O05GES - GESTIONE OPZIONE 2=MODIFICA
024500000000     C*------------------------------------------------------------------------*
024600000000     C     O05GES        BEGSR
024700000000     C*
024800000000     C* LANCIO LA VISUALIZZAZIONE
024900000000     C                   MOVEL     VSCCOD        WCOD
025000000000     C                   MOVEL     'GES'         WOP0
025100000000     C                   MOVEL     'O05'         WOP1
025200000000     C                   MOVEL     'TRTB61R'     WPGM
025300000000     C                   EXSR      LANPGM
025400000000     C*
025500000000     C                   ENDSR
025600000000     C*------------------------------------------------------------------------*
025700000000     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
025800000000     C*------------------------------------------------------------------------*
025900000000     C     O07GES        BEGSR
026000000000     C*
026100000000     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
026200000000IF  1C     VSCATB        IFNE      'A'
026300000000     C                   MOVEL     '1'           $ERR
026400000000     C                   MOVEL     MSG(13)       $MSG
026500000000     C                   EXSR      CARMSG
026600000000E   1C                   ENDIF
026700000000     C*
026800000000IF  1C     $ERR          IFEQ      '0'
026900000000     C*
027000000000     C* LANCIO IL RIPRISTINO
027100000000     C                   MOVEL     VSCCOD        WCOD
027200000000     C                   MOVEL     'GES'         WOP0
027300000000     C                   MOVEL     'O07'         WOP1
027400000000     C                   MOVEL     'TRTB61R'     WPGM
027500000000     C                   EXSR      LANPGM
027600000000     C*
027700000000     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
027800000000     C                   Z-ADD     1             SWMODI
027900000000     C*
028000000000E   1C                   ENDIF
028100000000     C*
028200000000     C                   ENDSR
028300000000     C*------------------------------------------------------------------------*
028400000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
028500000000     C*------------------------------------------------------------------------*
028600000000     C     NEWPOS        BEGSR
028700000000     C*
028800000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
028900000000     C*        1 = RICHIESTO POSIZIONAMENTO
029000000000     C                   Z-ADD     0             SWPOSI            7 0
029100000000     C*
029200000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
029300000000     C*            NON OCCORRE RICARICARE IL SUBFILE
029400000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
029500000000     C                   Z-ADD     0             SWMODI            1 0
029600000000     C*
029700000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
029800000000IF  1C     VCCCOD        IFNE      DEPCOD
029900000000     C*
030000000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
030100000000     C                   EXSR      CHKPAR
030200000000     C*
030300000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
030400000000IF  2C     $ERR          IFEQ      '0'
030500000000     C                   Z-ADD     1             SWPOSI
030600000000     C                   MOVEL     VCCCOD        DEPCOD
030700000000E   2C                   ENDIF
030800000000E   1C                   ENDIF
030900000000     C*
031000000000     C                   ENDSR
031100000000     C*------------------------------------------------------------------------*
031200000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
031300000000     C*------------------------------------------------------------------------*
031400000000     C     CHKPAR        BEGSR
031500000000     C*
031600000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
031700000000     C* CURSORE SUL CAMPO DEL CONTROL
031800000000IF  1C     $ERR          IFEQ      '1'
031900000000     C                   MOVEL     '0'           *IN43
032000000000X   1C                   ELSE
032100000000     C                   MOVEL     '1'           *IN43
032200000000E   1C                   ENDIF
032300000000     C*
032400000000     C                   ENDSR
032500000000     C*------------------------------------------------------------------------*
032600000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD TABELLE
032700000000     C*------------------------------------------------------------------------*
032800000000     C     SETFIL        BEGSR
032900000000     C*
033000000000     C                   MOVEL     '1'           *IN43
033100000000     C*
033200000000     C* POSIZIONAMENTO TABELLA COMMERCIALI
033300020923     C                   MOVEL     1             KTBKUT
033400000000     C                   MOVEL     '2Q'          KTBCOD
033500000000     C                   MOVEL     *BLANKS       KTBKEY
033600000000     C                   MOVEL     DEPCOD        KTBKEY
033700000000     C*
033800000000     C     KEYTAB        SETLL     TABEL00F                           99
033900000000     C*
034000000000     C  N99              EXSR      LETFIL
034100000000     C*
034200000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
034300000000IF  1C     *IN99         IFEQ      '1'
034400000000     C                   Z-ADD     1             SFLEOF
034500000000E   1C                   ENDIF
034600000000     C*
034700000000     C                   ENDSR
034800000000     C*------------------------------------------------------------------------*
034900000000     C* LETFIL - LEGGE RECORD SUCESSIVO TABELLE
035000000000     C*------------------------------------------------------------------------*
035100000000     C     LETFIL        BEGSR
035200000000     C*
035300000000     C* FINO A QUANDO NON TROVI UN RECORD VALIDO (RECOK = '1') STAI DENTRO
035400000000     C                   MOVEL     '0'           RECOK             1
035500000000DO  1C     RECOK         DOWEQ     '0'
035600000000     C*
035700000000     C* LETTURA RECORD TABELLA 'OSA'
035800000000     C                   READ      TABEL00F                               99
035900000000     C                   MOVEL     '1'           RECOK
036000000000     C*
036100000000     C* FINE FILE  O TABELLA DIVERSA
036200000000IF  2C     *IN99         IFEQ      '1'
036300000000     C     TBLKUT        ORNE      1
036400000000     C     TBLCOD        ORNE      '2Q'
036500000000     C                   Z-ADD     1             SFLEOF
036600000000E   2C                   ENDIF
036700000000E   1C                   ENDDO
036800000000     C*
036900000000     C                   ENDSR
037000000000     C*------------------------------------------------------------------------*
037100000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
037200000000     C*------------------------------------------------------------------------*
037300000000     C     IRESFL        BEGSR
037400000000     C*
037500000000     C* RIEMPIMENTO CAMPI SFILE
037600000000IF  1C     TBLFLG        IFNE      *BLANKS
037700000000     C                   MOVEL     'A'           VSCATB
037800000000X   1C                   ELSE
037900000000     C                   MOVEL     ' '           VSCATB
038000000000E   1C                   ENDIF
038100000000     C*
038200000000     C                   MOVEL     TBLKEY        VSCCOD
038300000000     C                   MOVEL     TBLUNI        DS2Q
038400000000     C                   MOVEL     §2QDES        VSCDES
038500000000     C                   MOVEL     §2QTTV        VSCTTV
038600000000     C*
038700000000     C                   ENDSR
038800000000     C*------------------------------------------------------------------------*
038900000000     C* LANPGM - LANCIA UN PROGRAMMA
039000000000     C*------------------------------------------------------------------------*
039100000000     C     LANPGM        BEGSR
039200000000     C*
039300000000     C*--------------------
039400000000     C* PARAMETRI INFORMATIVI
039500000000     C*--------------------
039600000000     C                   MOVEL     WOP0          D60OP0
039700000000     C                   MOVEL     WOP1          D60OP1
039800000000     C                   MOVEL     '0'           D60F03
039900000000     C                   MOVEL     '0'           D60ERR
040000000000     C                   MOVEL     *BLANKS       D60MSG
040100000000     C*--------------------
040200000000     C* PARAMETRI
040300000000     C*--------------------
040400000000     C                   MOVEL     WCOD          D60COD
040500000000     C* LANCIO
040600000000     C                   CALL      WPGM                                 99
040700000000     C                   PARM                    DS60
040800000000     C*
040900000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
041000000000IF  1C     D60F03        IFEQ      '1'
041100000000     C                   MOVEL     '1'           $F03
041200000000     C                   MOVEL     '1'           $ERR
041300000000     C                   MOVEL     '1'           $FINE
041400000000     C*
041500000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
041600000000X   1C                   ELSE
041700000000     C                   MOVEL     D60MSG        $MSG
041800000000     C                   EXSR      CARMSG
041900000000E   1C                   ENDIF
042000000000     C*
042100000000     C                   ENDSR
042200020923     C*-------------------------------------------------------------------------
042300020923     C* REPERISCE I DATI UTENTE
042400020923     C*-------------------------------------------------------------------------
042500020923     C     REPDATIUTE    BEGSR
042600020923     C*
042700020923     C* INIZIALIZZA VARIABILI DI WRK
042800020923     C                   CLEAR                   TIBS34DS
042900020923     C                   CLEAR                   AZUTEDS
043000020923     C                   CLEAR                   DDATIUTE
043100020923     C*
043200020923     C     *DTAARA       DEFINE    §azute        azuteds
043300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
043400020923     C                   IN(E)     *DTAARA
043500020923if  1C                   IF        %Error
043600020923     c                   EVAL      I34Tla = 'L'
043700020923     C                   CALL      'TIBS34R'
043800020923     C                   PARM                    Tibs34Ds
043900020923     C                   IN        *DTAARA
044000020923e   1C                   ENDIF
044100020923     C*
044200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
044300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
044400020923     C*
044500020923     C                   ENDSR
044600000000     C*------------------------------------------------------------------------*
044700000000     C*  *INZSR - ROUTINE INIZIALE
044800000000     C*------------------------------------------------------------------------*
044900000000     C     *INZSR        BEGSR
045000000000     C*
045100000000     C* RICEVIMENTO PARAMETRI
045200000000     C     *ENTRY        PLIST
045300000000     C                   PARM                    KPJBA
045400000000     C*
045500000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
045600000000     C     *LIKE         DEFINE    VCCCOD        DEPCOD
045700000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
045800000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
045900000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
046000000000     C*
046100000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
046200000000     C     KEYTAB        KLIST
046300000000     C                   KFLD                    KTBKUT
046400000000     C                   KFLD                    KTBCOD
046500000000     C                   KFLD                    KTBKEY
046600000000     C*
046700000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
046800000000     C     KEYTA2        KLIST
046900000000     C                   KFLD                    KTBKUT
047000000000     C                   KFLD                    KTBCOD
047100000000     C*
047200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
047300000000     C                   EXSR      INZSTD
047400000000     C*
047500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
047600000000     C                   EXSR      INZSFL
047700980130     C*
047800980130     C* CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
047900980130     C                   CLEAR                   UL12DS
048000980130     C                   MOVEL     'L'           I12TLA
048100980130     C                   MOVEL     '2Q'          I12COD
048200980130     C                   MOVEL     KNSIF         I12NSI
048300980130     C                   CALL      'TRUL12R'
048400980130     C                   PARM                    UL12DS
048500980130     C     O12ERR        IFEQ      '1'
048600980130     C                   MOVEL     '1'           $FINE
048700980130     C                   ENDIF
048800000000     C*
048900000000     C* IMPOSTO CAMPI TESTATA VIDEO
049000000000     C                   MOVEL     DSFIRS        VTDRSU
049100000000     C*
049200000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
049300000000     C                   MOVE      *BLANKS       DEPCOD
049400000000     C*
049500000000     C* EMETTO VIDEATA SUBFILE
049600000000     C                   EXSR      CARVS
049700000000     C*
049800000000     C                   ENDSR
049900000000     C*------------------------------------------------------------------------*
050000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
050100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
050200000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
050300000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
050400000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
050500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
050600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
050700000000     C*------------------------------------------------------------------------*
050800000000     C* INZSTD - ROUTINE INIZIALE STANDARD
050900000000     C*------------------------------------------------------------------------*
051000000000     C     INZSTD        BEGSR
051100000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
051200000000     C                   CALL      'XKEYS'
051300000000     C                   PARM                    DSKEYS
051400000000     C                   MOVEA     KEYRIG        KEYS
051500000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
051600000000     C     XMSGPA        PLIST
051700000000     C                   PARM                    DSMSMS
051800000000     C                   PARM                    DSMSPG
051900000000     C                   PARM                    DSMSKE
052000000000     C                   PARM                    DSMSER
052100000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
052200000000     C                   MOVEL     'TRTB60R'     DSMSPG
052300000000     C                   MOVEL     'TRTB60R'     VMCQUE
052400000000     C                   MOVEL     *BLANKS       VMCKEY
052500000000     C                   MOVEL     *BLANKS       DSMSMS
052600000000     C* INIZIALIZZO FLAG CONTROLLO PGM
052700000000     C                   MOVEL     '0'           $ERR              1
052800000000     C                   MOVEL     '0'           $F03              1
052900000000     C                   MOVEL     *BLANKS       $GES              2
053000000000     C                   MOVEL     *BLANKS       $MSG             80
053100000000     C                   MOVEL     '0'           $FINE             1
053200000000     C                   MOVEL     '0'           $TESVD            1
053300000000     C                   MOVEL     'G'           $TGES             1
053400000000     C* LANCIO ROUTINE PER VERIFICARE SE
053500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
053600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
053700020923     C                   EXSR      REPDATIUTE
053800000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
053900000000     C                   BITOFF    '01234567'    POSMI1            1
054000000000     C                   BITOFF    '01234567'    POSMI2            1
054100000000     C                   BITOFF    '01234567'    POSMA1            1
054200000000     C                   BITOFF    '01234567'    POSMA2            1
054300000000     C                   BITON     '34'          POSMI1            1
054400000000     C                   BITON     '7'           POSMI2            1
054500000000     C                   BITON     '34'          POSMA1            1
054600000000     C                   BITON     '0'           POSMA2            1
054700000000     C                   MOVE      POSMI2        POSMIN            2
054800000000     C                   MOVEL     POSMI1        POSMIN
054900000000     C                   MOVE      POSMA2        POSMAX            2
055000000000     C                   MOVEL     POSMA1        POSMAX
055100000000     C                   ENDSR
055200000000     C*------------------------------------------------------------------------*
055300000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
055400000000     C*------------------------------------------------------------------------*
055500000000     C     FUNSTD        BEGSR
055600000000     C                   SELECT
055700000000     C     KEY           WHENEQ    F3
055800000000     C                   EXSR      F03STD
055900000000     C     KEY           WHENEQ    F12
056000000000     C                   EXSR      F12STD
056100000000     C     KEY           WHENEQ    F24
056200000000     C                   EXSR      F24STD
056300000000     C                   OTHER
056400000000     C                   MOVEL     MSG(2)        $MSG
056500000000     C                   EXSR      CARMSG
056600000000     C                   ENDSL
056700000000     C                   ENDSR
056800000000     C*------------------------------------------------------------------------*
056900000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
057000000000     C*------------------------------------------------------------------------*
057100000000     C     F03STD        BEGSR
057200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
057300000000     C                   MOVEL     '1'           $F03
057400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
057500000000     C                   MOVEL     '1'           $FINE
057600000000     C                   ENDSR
057700000000     C*------------------------------------------------------------------------*
057800000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
057900000000     C*------------------------------------------------------------------------*
058000000000     C     F12STD        BEGSR
058100000000     C* IMPOSTO FLAG DI FIN PGM
058200000000     C                   MOVEL     '1'           $FINE
058300000000     C                   ENDSR
058400000000     C*------------------------------------------------------------------------*
058500000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
058600000000     C*------------------------------------------------------------------------*
058700000000     C     F24STD        BEGSR
058800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
058900000000     C                   EXSR      OUTFUN
059000000000     C                   ENDSR
059100000000     C*------------------------------------------------------------------------*
059200000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
059300000000     C*------------------------------------------------------------------------*
059400000000     C     INZSFL        BEGSR
059500000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
059600000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
059700000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
059800000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
059900000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
060000000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
060100000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
060200000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
060300000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
060400000000     C                   Z-ADD     12            SFLPAG
060500000000     C                   ENDSR
060600000000     C*------------------------------------------------------------------------*
060700000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
060800000000     C*------------------------------------------------------------------------*
060900000000     C     FUNSFL        BEGSR
061000000000     C                   SELECT
061100000000     C     KEY           WHENEQ    ENTER
061200000000     C                   EXSR      ENTSFL
061300000000     C     KEY           WHENEQ    F3
061400000000     C                   EXSR      F03STD
061500000000     C     KEY           WHENEQ    F5
061600000000     C                   EXSR      F05STD
061700000000     C     KEY           WHENEQ    F12
061800000000     C                   EXSR      F12STD
061900000000     C     KEY           WHENEQ    F17
062000000000     C                   EXSR      F17STD
062100000000     C     KEY           WHENEQ    F18
062200000000     C                   EXSR      F18STD
062300000000     C     KEY           WHENEQ    F23
062400000000     C                   EXSR      F23STD
062500000000     C     KEY           WHENEQ    F24
062600000000     C                   EXSR      F24STD
062700000000     C     KEY           WHENEQ    ROLLUP
062800000000     C                   EXSR      RUPSTD
062900000000     C     KEY           WHENEQ    ROLLDN
063000000000     C                   EXSR      RDNSTD
063100000000     C                   OTHER
063200000000     C                   MOVEL     MSG(2)        $MSG
063300000000     C                   EXSR      CARMSG
063400000000     C                   ENDSL
063500000000     C                   ENDSR
063600000000     C*------------------------------------------------------------------------*
063700000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
063800000000     C*------------------------------------------------------------------------*
063900000000     C     F05STD        BEGSR
064000000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
064100000000     C                   EXSR      POSSFL
064200000000     C                   ENDSR
064300000000     C*------------------------------------------------------------------------*
064400000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
064500000000     C*------------------------------------------------------------------------*
064600000000     C     F17STD        BEGSR
064700000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
064800000000     C                   Z-ADD     1             VCCNBR
064900000000     C                   ENDSR
065000000000     C*------------------------------------------------------------------------*
065100000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
065200000000     C*------------------------------------------------------------------------*
065300000000     C     F18STD        BEGSR
065400000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
065500000000     C                   Z-ADD     SFLULT        VCCNBR
065600000000     C                   ENDSR
065700000000     C*------------------------------------------------------------------------*
065800000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
065900000000     C*------------------------------------------------------------------------*
066000000000     C     F23STD        BEGSR
066100000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
066200000000     C                   EXSR      OUTOPZ
066300000000     C                   ENDSR
066400000000     C*------------------------------------------------------------------------*
066500000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
066600000000     C*------------------------------------------------------------------------*
066700000000     C     RUPSTD        BEGSR
066800000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
066900000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
067000000000     C     CURSOR        IFGE      POSMIN
067100000000     C     CURSOR        ANDLE     POSMAX
067200000000     C                   ELSE
067300000000     C                   EXSR      CARSFL
067400000000     C                   ENDIF
067500000000     C                   ENDSR
067600000000     C*------------------------------------------------------------------------*
067700000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
067800000000     C*------------------------------------------------------------------------*
067900000000     C     RDNSTD        BEGSR
068000000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
068100000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
068200000000     C     CURSOR        IFGE      POSMIN
068300000000     C     CURSOR        ANDLE     POSMAX
068400000000     C                   ELSE
068500000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
068600000000     C                   MOVEL     '1'           $ERR
068700000000     C                   MOVEL     MSG(5)        $MSG
068800000000     C                   EXSR      CARMSG
068900000000     C                   ENDIF
069000000000     C                   ENDSR
069100000000     C*------------------------------------------------------------------------*
069200000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
069300000000     C*------------------------------------------------------------------------*
069400000000     C     ENTSFL        BEGSR
069500000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
069600000000     C                   EXSR      NEWPOS
069700000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
069800000000     C     $ERR          IFEQ      '0'
069900000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
070000000000     C     SWPOSI        IFNE      0
070100000000     C                   EXSR      POSSFL
070200000000     C                   ELSE
070300000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
070400000000     C     SFLULT        IFNE      0
070500000000     C                   EXSR      OPZSFL
070600000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
070700000000     C     SWMODI        IFNE      0
070800000000     C                   EXSR      POSSFL
070900000000     C                   ENDIF
071000000000     C                   ENDIF
071100000000     C                   ENDIF
071200000000     C                   ENDIF
071300000000     C                   ENDSR
071400000000     C*------------------------------------------------------------------------*
071500000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
071600000000     C*------------------------------------------------------------------------*
071700000000     C     POSSFL        BEGSR
071800000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
071900000000     C                   EXSR      CLRSFL
072000000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
072100000000     C                   EXSR      SETFIL
072200000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
072300000000     C     SFLEOF        IFEQ      0
072400000000     C                   EXSR      CARSFL
072500000000     C                   ENDIF
072600000000     C                   ENDSR
072700000000     C*------------------------------------------------------------------------*
072800000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
072900000000     C*------------------------------------------------------------------------*
073000000000     C     OPZSFL        BEGSR
073100000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
073200000000     C                   MOVEL     'S'           OPZOK             1
073300000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
073400000000     C                   Z-ADD     0             SFLVAR            5 0
073500000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
073600000000     C                   Z-ADD     0             SFLERR
073700000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
073800000000     C                   Z-ADD     SFLULT        WFLULT
073900000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
074000000000     C                   READC     TB60DVS                                49
074100000000     C     *IN49         DOWEQ     '0'
074200000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
074300000000     C                   ADD       1             SFLVAR
074400000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
074500000000     C     OPZOK         IFEQ      'S'
074600000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
074700000000     C     VSCOPZ        IFNE      *BLANKS
074800000000     C                   EXSR      CONOPZ
074900000000     C     OPZOK         IFEQ      'S'
075000000000     C                   EXSR      GESOPZ
075100000000     C                   ENDIF
075200000000     C                   ENDIF
075300000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
075400000000     C*            2. PULIZIA SCELTA IN SFL
075500000000     C     OPZOK         IFEQ      'S'
075600000000     C                   MOVEL     '0'           *IN41
075700000000     C                   MOVEL     *BLANKS       VSCOPZ
075800000000     C                   ELSE
075900000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
076000000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
076100000000     C                   MOVEL     '1'           *IN41
076200000000     C                   Z-ADD     SFLULT        SFLERR
076300000000     C                   ENDIF
076400000000     C                   ELSE
076500000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
076600000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
076700000000     C                   MOVEL     '1'           *IN41
076800000000     C                   ENDIF
076900000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
077000000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
077100000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
077200000000     C     OPZOK         IFNE      'S'
077300000000     C                   Z-ADD     SFLERR        VCCNBR
077400000000     C                   ELSE
077500000000     C                   Z-ADD     SFLULT        VCCNBR
077600000000     C                   ENDIF
077700000000     C* AGGIORNO RECORD SFL
077800000000     C                   UPDATE    TB60DVS
077900000000     C                   READC     TB60DVS                                49
078000000000     C                   ENDDO
078100000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
078200000000     C                   Z-ADD     WFLULT        SFLULT
078300000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
078400000000     C     SFLVAR        IFEQ      0
078500000000     C                   MOVEL     '1'           $FINE
078600000000     C                   ENDIF
078700000000     C                   ENDSR
078800000000     C*------------------------------------------------------------------------*
078900000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
079000000000     C*------------------------------------------------------------------------*
079100000000     C     CLRSFL        BEGSR
079200000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
079300000000     C* (0=NON FINE FILE  1=FINE FILE)
079400000000     C                   Z-ADD     0             SFLEOF
079500000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
079600000000     C                   Z-ADD     0             SFLULT
079700000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
079800000000     C                   MOVEL     '1'           *IN40
079900000000     C                   WRITE     TB60DVC
080000000000     C                   MOVEL     '0'           *IN40
080100000000     C                   ENDSR
080200000000     C*------------------------------------------------------------------------*
080300000000     C* CARSFL - CARICAMENTO SFL
080400000000     C*------------------------------------------------------------------------*
080500000000     C     CARSFL        BEGSR
080600000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
080700000000     C     SFLULT        IFEQ      0
080800000000     C                   EXSR      SETFIL
080900000000     C                   ENDIF
081000000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
081100000000     C                   Z-ADD     SFLULT        WFLULT
081200000000     C* CICLO:
081300000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
081400000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
081500000000     C                   Z-ADD     1             SFLRIG
081600000000     C     SFLRIG        DOWLE     SFLPAG
081700000000     C     SFLEOF        ANDEQ     0
081800000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
081900000000     C                   MOVEL     *BLANKS       VSCOPZ
082000000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
082100000000     C                   EXSR      IRESFL
082200000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
082300000000     C                   ADD       1             SFLULT
082400000000     C* SCRITTURA RECORD SFL
082500000000     C                   WRITE     TB60DVS
082600000000     C                   ADD       1             SFLRIG
082700000000     C* LETTURA FILE DA CARICARE
082800000000     C                   EXSR      LETFIL
082900000000     C                   ENDDO
083000000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
083100000000     C     WFLULT        IFNE      SFLULT
083200000000     C                   Z-ADD     WFLULT        VCCNBR
083300000000     C                   ADD       1             VCCNBR
083400000000     C                   ENDIF
083500000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
083600000000     C     SFLEOF        IFEQ      1
083700000000     C     SFLULT        ANDGT     *ZEROS
083800000000     C                   MOVEL     '1'           $ERR
083900000000     C                   MOVEL     MSG(6)        $MSG
084000000000     C                   EXSR      CARMSG
084100000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
084200000000     C     SFLULT        CHAIN     TB60DVS                            99
084300000000     C     *IN99         IFEQ      '0'
084400000000     C                   SETON                                        45
084500000000     C                   UPDATE    TB60DVS
084600000000     C                   SETOFF                                       45
084700000000     C                   ENDIF
084800000000     C                   ENDIF
084900000000     C                   ENDSR
085000000000     C*------------------------------------------------------------------------*
085100000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
085200000000     C*------------------------------------------------------------------------*
085300000000     C     OUTSFL        BEGSR
085400000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
085500000000     C     MSGULT        IFNE      *ZEROS
085600000000     C                   EXSR      OUTMSG
085700000000     C                   ENDIF
085800000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
085900000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
086000000000     C     SFLEOF        IFEQ      0
086100000000     C                   MOVEL     '0'           *IN42
086200000000     C                   ELSE
086300000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
086400000000     C                   MOVEL     '1'           *IN42
086500000000     C                   ENDIF
086600000000     C* EMISSIONE SUBFILE
086700000000     C     SFLULT        IFLE      0
086800000000     C*  --> CASO DI SFL VUOTO
086900000000     C                   MOVEL     '0'           *IN43
087000000000     C                   WRITE     TB60DVV
087100000000     C                   WRITE     TB60DVC
087200000000     C                   READ      TB60DVC                                99
087300000000     C                   ELSE
087400000000     C*  --> CASO DI SFL CON RECORDS
087500000000     C                   WRITE     TB60DVC
087600000000     C                   READ      TB60DVC                                99
087700000000     C                   ENDIF
087800000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
087900000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
088000000000     C                   Z-ADD     FRTSFL        VCCNBR
088100000000     C                   ENDSR
088200000000     C*------------------------------------------------------------------------*
088300000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
088400000000     C*          CODA MESSAGGI DEL PGM
088500000000     C*------------------------------------------------------------------------*
088600000000     C     CARMSG        BEGSR
088700000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
088800000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
088900000000     C     MSGULT        IFEQ      0
089000000000     C                   MOVEL     '1'           *IN40
089100000000     C                   WRITE     TB60DVK
089200000000     C                   MOVEL     '0'           *IN40
089300000000     C                   ENDIF
089400000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
089500000000     C                   MOVEL     $MSG          DSMSMS
089600000000     C                   CALL      'XMSG'        XMSGPA
089700000000     C     DSMSER        IFEQ      0
089800000000     C                   MOVEL     DSMSKE        VMCKEY
089900000000     C                   ENDIF
090000000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
090100000000     C                   ADD       1             MSGULT            4 0
090200000000     C                   WRITE     TB60DVM
090300000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
090400000000     C                   MOVEL     *BLANKS       $MSG
090500000000     C                   ENDSR
090600000000     C*------------------------------------------------------------------------*
090700000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
090800000000     C*------------------------------------------------------------------------*
090900000000     C     OUTMSG        BEGSR
091000000000     C* VISUALIZZAZIONE MESSAGGIO
091100000000     C                   MOVEL     '1'           *IN44
091200000000     C                   WRITE     TB60DVK
091300000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
091400000000     C                   Z-ADD     *ZEROS        MSGULT
091500000000     C                   ENDSR
091600000000     C*------------------------------------------------------------------------*
091700000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
091800000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
091900000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
092000000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
092100000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
092200000000     C*------------------------------------------------------------------------*
092300000000     C     CARFUN        BEGSR
092400000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
092500000000     C     1             DO        31            I                 5 0
092600000000     C                   MOVEL     FUN(I)        FUNRIG
092700000000     C                   MOVEL     FUNFUN        FFU(I)
092800000000     C                   MOVEL     FUNDES        FDE(I)
092900020923     C     DUTLPO        IFEQ      'S'
093000000000     C     $TGES         IFEQ      'G'
093100000000     C                   MOVEL     FUNASG        FAT(I)
093200000000     C                   ELSE
093300000000     C                   MOVEL     FUNASI        FAT(I)
093400000000     C                   ENDIF
093500000000     C                   ELSE
093600000000     C     $TGES         IFEQ      'G'
093700000000     C                   MOVEL     FUNAFG        FAT(I)
093800000000     C                   ELSE
093900000000     C                   MOVEL     FUNAFI        FAT(I)
094000000000     C                   ENDIF
094100000000     C                   ENDIF
094200000000     C     FAT(I)        IFEQ      'E'
094300000000     C                   MOVEL     USER          WEDP              3
094400000000     C     WEDP          IFEQ      'EDP'
094500000000     C                   MOVEL     'S'           FAT(I)
094600000000     C                   ELSE
094700000000     C                   MOVEL     'N'           FAT(I)
094800000000     C                   ENDIF
094900000000     C                   ENDIF
095000000000     C                   ENDDO
095100000000     C                   ENDSR
095200000000     C*------------------------------------------------------------------------*
095300000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
095400000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
095500000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
095600000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
095700000000     C*------------------------------------------------------------------------*
095800000000     C     OUTFUN        BEGSR
095900000000     C     1             DO        8             I
096000000000     C                   MOVEL     *BLANKS       FUV(I)
096100000000     C                   ENDDO
096200000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
096300000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
096400000000     C     1             DO        31            I
096500000000     C     FAT(I)        IFEQ      'S'
096600000000     C     FDE(I)        ANDNE     *BLANKS
096700000000     C                   ADD       1             FUNNAT
096800000000     C                   ENDIF
096900000000     C                   ENDDO
097000000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
097100000000     C     FUNNAT        IFGT      8
097200000000     C                   MOVEL     'S'           FAT(24)
097300000000     C                   ENDIF
097400000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
097500000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
097600000000     C                   Z-ADD     8             FUNVIS            5 0
097700000000     C     FAT(23)       IFEQ      'S'
097800000000     C                   SUB       1             FUNVIS
097900000000     C                   ENDIF
098000000000     C     FAT(24)       IFEQ      'S'
098100000000     C                   SUB       1             FUNVIS
098200000000     C                   ENDIF
098300000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
098400000000     C                   Z-ADD     *ZEROS        W                 5 0
098500000000     C                   ADD       1             FUNULT
098600000000     C     FUNULT        IFGT      31
098700000000     C                   Z-ADD     1             FUNULT
098800000000     C                   ENDIF
098900000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
099000000000     C     FUNULT        DO        31            I
099100000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
099200000000     C     W             IFLT      FUNVIS
099300000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
099400000000     C     FAT(I)        IFEQ      'S'
099500000000     C     FDE(I)        ANDNE     *BLANKS
099600000000     C     I             IFEQ      23
099700000000     C     I             OREQ      24
099800000000     C                   ELSE
099900000000     C                   ADD       1             W
100000000000     C                   MOVEL     FDE(I)        FUV(W)
100100000000     C                   ENDIF
100200000000     C                   ENDIF
100300000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
100400000000     C                   Z-ADD     I             FUNULT
100500000000     C                   ENDIF
100600000000     C                   ENDDO
100700000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
100800000000     C                   MOVEL     FUV(1)        VFD001
100900000000     C                   MOVEL     FUV(2)        VFD002
101000000000     C                   MOVEL     FUV(3)        VFD003
101100000000     C                   MOVEL     FUV(4)        VFD004
101200000000     C                   MOVEL     FUV(5)        VFD005
101300000000     C                   MOVEL     FUV(6)        VFD006
101400000000     C     FAT(23)       IFEQ      'N'
101500000000     C     FAT(24)       ANDEQ     'N'
101600000000     C                   MOVEL     FUV(7)        VFD007
101700000000     C                   MOVEL     FUV(8)        VFD008
101800000000     C                   ELSE
101900000000     C     FAT(23)       IFEQ      'S'
102000000000     C     FAT(24)       ANDEQ     'S'
102100000000     C                   MOVEL     FDE(23)       VFD007
102200000000     C                   MOVEL     FDE(24)       VFD008
102300000000     C                   ELSE
102400000000     C                   MOVEL     FUV(7)        VFD007
102500000000     C     FAT(23)       IFEQ      'S'
102600000000     C                   MOVEL     FDE(23)       VFD008
102700000000     C                   ELSE
102800000000     C                   MOVEL     FDE(24)       VFD008
102900000000     C                   ENDIF
103000000000     C                   ENDIF
103100000000     C                   ENDIF
103200000000     C                   MOVEL     '1'           $TESVD
103300000000     C                   ENDSR
103400000000     C*------------------------------------------------------------------------*
103500000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
103600000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
103700000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
103800000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
103900000000     C*------------------------------------------------------------------------*
104000000000     C     CONFUN        BEGSR
104100000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
104200000000     C     1             DO        31            I
104300000000     C     KEY           IFEQ      KEYS(I)
104400000000     C* CONTROLLO SE IL TASTO E' ATTIVO
104500000000     C     FAT(I)        IFNE      'S'
104600000000     C                   MOVEL     MSG(1)        $MSG
104700000000     C                   EXSR      CARMSG
104800000000     C                   MOVEL     '1'           $ERR
104900000000     C                   ENDIF
105000000000     C                   ENDIF
105100000000     C                   ENDDO
105200000000     C                   ENDSR
105300000000     C*------------------------------------------------------------------------*
105400000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
105500000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
105600000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
105700000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
105800000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
105900000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
106000000000     C*------------------------------------------------------------------------*
106100000000     C     CAROPZ        BEGSR
106200000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
106300000000     C     1             DO        24            I
106400000000     C                   MOVEL     OPZ(I)        OPZRIG
106500000000     C                   MOVEL     OPZOPD        OPD(I)
106600000000     C                   MOVEL     OPZOPS        OPS(I)
106700000000     C                   MOVEL     OPZDES        ODE(I)
106800020923     C     DUTLPO        IFEQ      'S'
106900000000     C     $TGES         IFEQ      'G'
107000000000     C                   MOVEL     OPZASG        OAT(I)
107100000000     C                   ELSE
107200000000     C                   MOVEL     OPZASI        OAT(I)
107300000000     C                   ENDIF
107400000000     C                   ELSE
107500000000     C     $TGES         IFEQ      'G'
107600000000     C                   MOVEL     OPZAFG        OAT(I)
107700000000     C                   ELSE
107800000000     C                   MOVEL     OPZAFI        OAT(I)
107900000000     C                   ENDIF
108000000000     C                   ENDIF
108100000000     C     OAT(I)        IFEQ      'E'
108200000000     C                   MOVEL     USER          WEDP              3
108300000000     C     WEDP          IFEQ      'EDP'
108400000000     C                   MOVEL     'S'           OAT(I)
108500000000     C                   ELSE
108600000000     C                   MOVEL     'N'           OAT(I)
108700000000     C                   ENDIF
108800000000     C                   ENDIF
108900000000     C                   ENDDO
109000000000     C                   ENDSR
109100000000     C*------------------------------------------------------------------------*
109200000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
109300000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
109400000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
109500000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
109600000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
109700000000     C*------------------------------------------------------------------------*
109800000000     C     OUTOPZ        BEGSR
109900000000     C     1             DO        8             I
110000000000     C                   MOVEL     *BLANKS       OPV(I)
110100000000     C                   ENDDO
110200000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
110300000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
110400000000     C                   ADD       1             OPZULT
110500000000     C     OPZULT        IFGT      24
110600000000     C                   Z-ADD     1             OPZULT
110700000000     C                   END
110800000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
110900000000     C     1             DO        24            I
111000000000     C     OAT(I)        IFEQ      'S'
111100000000     C     ODE(I)        ANDNE     *BLANKS
111200000000     C                   ADD       1             OPZNAT
111300000000     C                   ENDIF
111400000000     C                   ENDDO
111500000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
111600000000     C                   Z-ADD     0             W
111700000000     C     OPZULT        DO        24            I
111800000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
111900000000     C     W             IFLT      8
112000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
112100000000     C     OAT(I)        IFEQ      'S'
112200000000     C     ODE(I)        ANDNE     *BLANKS
112300000000     C                   ADD       1             W
112400000000     C                   MOVEL     ODE(I)        OPV(W)
112500000000     C                   ENDIF
112600000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
112700000000     C                   Z-ADD     I             OPZULT
112800000000     C                   ENDIF
112900000000     C                   ENDDO
113000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
113100000000     C                   MOVEL     OPV(1)        VOD001
113200000000     C                   MOVEL     OPV(2)        VOD002
113300000000     C                   MOVEL     OPV(3)        VOD003
113400000000     C                   MOVEL     OPV(4)        VOD004
113500000000     C                   MOVEL     OPV(5)        VOD005
113600000000     C                   MOVEL     OPV(6)        VOD006
113700000000     C                   MOVEL     OPV(7)        VOD007
113800000000     C                   MOVEL     OPV(8)        VOD008
113900000000     C                   MOVEL     '1'           $TESVD
114000000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
114100000000     C     OPZNAT        IFGT      8
114200000000     C                   MOVEL     'S'           FAT(23)
114300000000     C                   ENDIF
114400000000     C                   ENDSR
114500000000     C*------------------------------------------------------------------------*
114600000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
114700000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
114800000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
114900000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
115000000000     C*------------------------------------------------------------------------*
115100000000     C     CONOPZ        BEGSR
115200000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
115300000000     C                   MOVEL     *BLANKS       OPZOK             1
115400000000     C     1             DO        24            I                 5 0
115500000000     C     VSCOPZ        IFEQ      OPD(I)
115600000000     C     VSCOPZ        OREQ      OPS(I)
115700000000     C     OAT(I)        IFEQ      'S'
115800000000     C                   MOVEL     OPD(I)        VSCOPZ
115900000000     C                   MOVEL     'S'           OPZOK
116000000000     C                   ELSE
116100000000     C                   MOVEL     'N'           OPZOK             1
116200000000     C                   ENDIF
116300000000     C                   ENDIF
116400000000     C                   ENDDO
116500000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
116600000000     C     OPZOK         IFNE      'S'
116700000000     C                   MOVEL     MSG(3)        $MSG
116800000000     C                   EXSR      CARMSG
116900000000     C                   MOVEL     '1'           $ERR
117000000000     C                   MOVEL     'N'           OPZOK             1
117100000000     C                   ENDIF
117200000000     C                   ENDSR
117300000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
117400000000TRTB60R- Tasto funzionale non valido.                                          1
117500000000TRTB60R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
117600000000TRTB60R- Opzione non valida.                                                   3
117700000000TRTB60R- Opzione abilitata ma non gestita. Avvertire CED.                      4
117800000000TRTB60R- Inizio elenco.                                                        5
117900000000TRTB60R- Fine elenco.                                                          6
118000000000TRTB60R- 7
118100000000TRTB60R- 8
118200000000TRTB60R- 9
118300000000TRTB60R- 10
118400000000TRTB60R- 11
118500000000TRTB60R- Codice elemento annullato, consentito solo il ripristino             12
118600000000TRTB60R- Codice elemento non annullato, impossibile ripristinare              13
118700000000TRTB60R- 14
118800000000TRTB60R- 15
118900000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
119000000000- 1-1 -                   -    NN     NN
119100000000- 2-2 -2=Modifica         -    SS     SS
119200000000- 3-3 -                   -    NN     NN
119300000000- 4-4 -4=Cancellazione    -    SS     SS
119400000000- 5-5 -5=Visualizzazione  -    SS     SS
119500000000- 6-6 -                   -    NN     NN
119600000000- 7-7 -7=Ripristino       -    SS     SS
119700000000- 8-8 -                   -    NN     NN
119800000000- 9-9 -                   -    NN     NN
119900000000-10-  -                   -    NN     NN
120000000000-11-  -                   -    NN     NN
120100000000-12-  -                   -    NN     NN
120200000000-13-  -                   -    NN     NN
120300000000-14-  -                   -    NN     NN
120400000000-15-  -                   -    NN     NN
120500000000-16-  -                   -    NN     NN
120600000000-17-  -                   -    NN     NN
120700000000-18-  -                   -    NN     NN
120800000000-19-  -                   -    NN     NN
120900000000-20-  -                   -    NN     NN
121000000000-21-  -                   -    NN     NN
121100000000-22-  -                   -    NN     NN
121200000000-23-  -                   -    NN     NN
121300000000-24-  -                   -    NN     NN
121400000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
121500000000F1    -                   -    NN     NN
121600000000F2    -                   -    NN     NN
121700000000F3    -F3=Fine            -    SS     SS
121800000000F4    -                   -    NN     NN
121900000000F5    -F5=Rivisualizzaz.  -    SS     SS
122000000000F6    -F6=Inserimento     -    SS     SS
122100000000F7    -                   -    NN     NN
122200000000F8    -                   -    NN     NN
122300000000F9    -                   -    NN     NN
122400000000F10   -                   -    NN     NN
122500000000F11   -                   -    NN     NN
122600000000F12   -F12=Ritorno        -    SS     SS
122700000000F13   -                   -    NN     NN
122800000000F14   -                   -    NN     NN
122900000000F15   -                   -    NN     NN
123000000000F16   -                   -    NN     NN
123100000000F17   -F17=Inizio dati    -    SS     SS
123200000000F18   -F18=Fine dati      -    SS     SS
123300000000F19   -                   -    NN     NN
123400000000F20   -                   -    NN     NN
123500000000F21   -                   -    NN     NN
123600000000F22   -                   -    NN     NN
123700000000F23   -F23=Altre opzioni  -    NN     NN
123800000000F24   -F24=Altri tasti    -    NN     NN
123900000000ENTER -                   -    SS     SS
124000000000ROLLUP-                   -    SS     SS
124100000000ROLLDN-                   -    SS     SS
124200000000HELP  -                   -    NN     NN
124300000000PRINT -                   -    NN     NN
124400000000BSPACE-                   -    NN     NN
124500000000CLEAR -                   -    NN     NN
