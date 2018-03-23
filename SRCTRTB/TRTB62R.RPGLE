000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TRTB62R
000300000000      *  Descrizione   :  Gesione tabella valori di riferimento di c/e
000400000000      *                   Tabella 2R
000500000000      *                   Programma visualizzazione sfl di scelta
000600000000      *  Autore        :  STEFANO (SC)
000700000000      *  Data creazione:  13 GIU 1994
000800000000      *------------------------------------------------------------------------*
000900000000     H DECEDIT('0,') DATEDIT(*DMY.)
001000000000     F*------------------------------------------------------------------------*
001100000000     F* DATA BASE
001200000000     F*------------------------------------------------------------------------*
001300000000     FTRTB62D   CF   E             WORKSTN INFDS(DEVDS1)
001400000000     F                                     SFILE(TB62DVS:SFLULT)
001500000000     F                                     SFILE(TB62DVM:MSGULT)
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
008600000000     D DS2R          E DS
008700000000     D*---------------------
008800000000     D* DS ESTERNA DI PROCEDURA
008900000000     D*---------------------
009000000000     D DS62          E DS                  EXTNAME(TRTB62DS)
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
012500000000     C                   WRITE     TB62DVT
012600000000     C                   WRITE     TB62DVO
012700000000     C                   WRITE     TB62DVF
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
013800000000     C                   WRITE     TB62DVT
013900000000     C                   WRITE     TB62DVO
014000000000     C                   WRITE     TB62DVF
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
016600000000     C                   MOVEL     'TRTB63R'     WPGM              8
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
020900000000     C                   MOVEL     'TRTB63R'     WPGM
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
023500000000     C                   MOVEL     'TRTB63R'     WPGM
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
025200000000     C                   MOVEL     'TRTB63R'     WPGM
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
027400000000     C                   MOVEL     'TRTB63R'     WPGM
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
033400000000     C                   MOVEL     '2R'          KTBCOD
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
036400000000     C     TBLCOD        ORNE      '2R'
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
038300000000     C                   MOVEL     TBLUNI        DS2R
038400000000     C                   MOVEL     §2RDES        VSCDES
038500000000     C     §2RRPV        IFEQ      'S'
038600000000     C                   MOVE      'SI'          VSCRPV
038700000000     C                   ELSE
038800000000     C                   MOVE      'NO'          VSCRPV
038900000000     C                   ENDIF
039000000000     C     §2RPIU        IFEQ      'S'
039100000000     C                   MOVE      'SI'          VSCPIU
039200000000     C                   ELSE
039300000000     C                   MOVE      'NO'          VSCPIU
039400000000     C                   ENDIF
039500000000     C                   MOVEL     §2RTVA        VSCTVA
039600000000     C*
039700000000     C                   ENDSR
039800000000     C*------------------------------------------------------------------------*
039900000000     C* LANPGM - LANCIA UN PROGRAMMA
040000000000     C*------------------------------------------------------------------------*
040100000000     C     LANPGM        BEGSR
040200000000     C*
040300000000     C*--------------------
040400000000     C* PARAMETRI INFORMATIVI
040500000000     C*--------------------
040600000000     C                   MOVEL     WOP0          D62OP0
040700000000     C                   MOVEL     WOP1          D62OP1
040800000000     C                   MOVEL     '0'           D62F03
040900000000     C                   MOVEL     '0'           D62ERR
041000000000     C                   MOVEL     *BLANKS       D62MSG
041100000000     C*--------------------
041200000000     C* PARAMETRI
041300000000     C*--------------------
041400000000     C                   MOVEL     WCOD          D62COD
041500000000     C* LANCIO
041600000000     C                   CALL      WPGM                                 99
041700000000     C                   PARM                    DS62
041800000000     C*
041900000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
042000000000IF  1C     D62F03        IFEQ      '1'
042100000000     C                   MOVEL     '1'           $F03
042200000000     C                   MOVEL     '1'           $ERR
042300000000     C                   MOVEL     '1'           $FINE
042400000000     C*
042500000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
042600000000X   1C                   ELSE
042700000000     C                   MOVEL     D62MSG        $MSG
042800000000     C                   EXSR      CARMSG
042900000000E   1C                   ENDIF
043000000000     C*
043100000000     C                   ENDSR
043200020923     C*-------------------------------------------------------------------------
043300020923     C* REPERISCE I DATI UTENTE
043400020923     C*-------------------------------------------------------------------------
043500020923     C     REPDATIUTE    BEGSR
043600020923     C*
043700020923     C* INIZIALIZZA VARIABILI DI WRK
043800020923     C                   CLEAR                   TIBS34DS
043900020923     C                   CLEAR                   AZUTEDS
044000020923     C                   CLEAR                   DDATIUTE
044100020923     C*
044200020923     C     *DTAARA       DEFINE    §azute        azuteds
044300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
044400020923     C                   IN(E)     *DTAARA
044500020923if  1C                   IF        %Error
044600020923     c                   EVAL      I34Tla = 'L'
044700020923     C                   CALL      'TIBS34R'
044800020923     C                   PARM                    Tibs34Ds
044900020923     C                   IN        *DTAARA
045000020923e   1C                   ENDIF
045100020923     C*
045200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
045300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
045400020923     C*
045500020923     C                   ENDSR
045600000000     C*------------------------------------------------------------------------*
045700000000     C*  *INZSR - ROUTINE INIZIALE
045800000000     C*------------------------------------------------------------------------*
045900000000     C     *INZSR        BEGSR
046000000000     C*
046100000000     C* RICEVIMENTO PARAMETRI
046200000000     C     *ENTRY        PLIST
046300000000     C                   PARM                    KPJBA
046400000000     C*
046500000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
046600000000     C     *LIKE         DEFINE    VCCCOD        DEPCOD
046700000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
046800000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
046900000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
047000000000     C*
047100000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
047200000000     C     KEYTAB        KLIST
047300000000     C                   KFLD                    KTBKUT
047400000000     C                   KFLD                    KTBCOD
047500000000     C                   KFLD                    KTBKEY
047600000000     C*
047700000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
047800000000     C     KEYTA2        KLIST
047900000000     C                   KFLD                    KTBKUT
048000000000     C                   KFLD                    KTBCOD
048100000000     C*
048200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
048300000000     C                   EXSR      INZSTD
048400000000     C*
048500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
048600000000     C                   EXSR      INZSFL
048700980130     C*
048800980130     C* CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
048900980130     C                   CLEAR                   UL12DS
049000980130     C                   MOVEL     'L'           I12TLA
049100980130     C                   MOVEL     '2R'          I12COD
049200980130     C                   MOVEL     KNSIF         I12NSI
049300980130     C                   CALL      'TRUL12R'
049400980130     C                   PARM                    UL12DS
049500980130     C     O12ERR        IFEQ      '1'
049600980130     C                   MOVEL     '1'           $FINE
049700980130     C                   ENDIF
049800000000     C*
049900000000     C* IMPOSTO CAMPI TESTATA VIDEO
050000000000     C                   MOVEL     DSFIRS        VTDRSU
050100000000     C*
050200000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
050300000000     C                   MOVE      *BLANKS       DEPCOD
050400000000     C*
050500000000     C* EMETTO VIDEATA SUBFILE
050600000000     C                   EXSR      CARVS
050700000000     C*
050800000000     C                   ENDSR
050900000000     C*------------------------------------------------------------------------*
051000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051200000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
051300000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
051400000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
051500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051700000000     C*------------------------------------------------------------------------*
051800000000     C* INZSTD - ROUTINE INIZIALE STANDARD
051900000000     C*------------------------------------------------------------------------*
052000000000     C     INZSTD        BEGSR
052100000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
052200000000     C                   CALL      'XKEYS'
052300000000     C                   PARM                    DSKEYS
052400000000     C                   MOVEA     KEYRIG        KEYS
052500000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
052600000000     C     XMSGPA        PLIST
052700000000     C                   PARM                    DSMSMS
052800000000     C                   PARM                    DSMSPG
052900000000     C                   PARM                    DSMSKE
053000000000     C                   PARM                    DSMSER
053100000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
053200000000     C                   MOVEL     'TRTB62R'     DSMSPG
053300000000     C                   MOVEL     'TRTB62R'     VMCQUE
053400000000     C                   MOVEL     *BLANKS       VMCKEY
053500000000     C                   MOVEL     *BLANKS       DSMSMS
053600000000     C* INIZIALIZZO FLAG CONTROLLO PGM
053700000000     C                   MOVEL     '0'           $ERR              1
053800000000     C                   MOVEL     '0'           $F03              1
053900000000     C                   MOVEL     *BLANKS       $GES              2
054000000000     C                   MOVEL     *BLANKS       $MSG             80
054100000000     C                   MOVEL     '0'           $FINE             1
054200000000     C                   MOVEL     '0'           $TESVD            1
054300000000     C                   MOVEL     'G'           $TGES             1
054400000000     C* LANCIO ROUTINE PER VERIFICARE SE
054500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
054600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
054700020923     C                   EXSR      REPDATIUTE
054800000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
054900000000     C                   BITOFF    '01234567'    POSMI1            1
055000000000     C                   BITOFF    '01234567'    POSMI2            1
055100000000     C                   BITOFF    '01234567'    POSMA1            1
055200000000     C                   BITOFF    '01234567'    POSMA2            1
055300000000     C                   BITON     '34'          POSMI1            1
055400000000     C                   BITON     '7'           POSMI2            1
055500000000     C                   BITON     '34'          POSMA1            1
055600000000     C                   BITON     '0'           POSMA2            1
055700000000     C                   MOVE      POSMI2        POSMIN            2
055800000000     C                   MOVEL     POSMI1        POSMIN
055900000000     C                   MOVE      POSMA2        POSMAX            2
056000000000     C                   MOVEL     POSMA1        POSMAX
056100000000     C                   ENDSR
056200000000     C*------------------------------------------------------------------------*
056300000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
056400000000     C*------------------------------------------------------------------------*
056500000000     C     FUNSTD        BEGSR
056600000000     C                   SELECT
056700000000     C     KEY           WHENEQ    F3
056800000000     C                   EXSR      F03STD
056900000000     C     KEY           WHENEQ    F12
057000000000     C                   EXSR      F12STD
057100000000     C     KEY           WHENEQ    F24
057200000000     C                   EXSR      F24STD
057300000000     C                   OTHER
057400000000     C                   MOVEL     MSG(2)        $MSG
057500000000     C                   EXSR      CARMSG
057600000000     C                   ENDSL
057700000000     C                   ENDSR
057800000000     C*------------------------------------------------------------------------*
057900000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
058000000000     C*------------------------------------------------------------------------*
058100000000     C     F03STD        BEGSR
058200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
058300000000     C                   MOVEL     '1'           $F03
058400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
058500000000     C                   MOVEL     '1'           $FINE
058600000000     C                   ENDSR
058700000000     C*------------------------------------------------------------------------*
058800000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
058900000000     C*------------------------------------------------------------------------*
059000000000     C     F12STD        BEGSR
059100000000     C* IMPOSTO FLAG DI FIN PGM
059200000000     C                   MOVEL     '1'           $FINE
059300000000     C                   ENDSR
059400000000     C*------------------------------------------------------------------------*
059500000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
059600000000     C*------------------------------------------------------------------------*
059700000000     C     F24STD        BEGSR
059800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
059900000000     C                   EXSR      OUTFUN
060000000000     C                   ENDSR
060100000000     C*------------------------------------------------------------------------*
060200000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
060300000000     C*------------------------------------------------------------------------*
060400000000     C     INZSFL        BEGSR
060500000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
060600000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
060700000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
060800000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
060900000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
061000000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
061100000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
061200000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
061300000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
061400000000     C                   Z-ADD     12            SFLPAG
061500000000     C                   ENDSR
061600000000     C*------------------------------------------------------------------------*
061700000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
061800000000     C*------------------------------------------------------------------------*
061900000000     C     FUNSFL        BEGSR
062000000000     C                   SELECT
062100000000     C     KEY           WHENEQ    ENTER
062200000000     C                   EXSR      ENTSFL
062300000000     C     KEY           WHENEQ    F3
062400000000     C                   EXSR      F03STD
062500000000     C     KEY           WHENEQ    F5
062600000000     C                   EXSR      F05STD
062700000000     C     KEY           WHENEQ    F12
062800000000     C                   EXSR      F12STD
062900000000     C     KEY           WHENEQ    F17
063000000000     C                   EXSR      F17STD
063100000000     C     KEY           WHENEQ    F18
063200000000     C                   EXSR      F18STD
063300000000     C     KEY           WHENEQ    F23
063400000000     C                   EXSR      F23STD
063500000000     C     KEY           WHENEQ    F24
063600000000     C                   EXSR      F24STD
063700000000     C     KEY           WHENEQ    ROLLUP
063800000000     C                   EXSR      RUPSTD
063900000000     C     KEY           WHENEQ    ROLLDN
064000000000     C                   EXSR      RDNSTD
064100000000     C                   OTHER
064200000000     C                   MOVEL     MSG(2)        $MSG
064300000000     C                   EXSR      CARMSG
064400000000     C                   ENDSL
064500000000     C                   ENDSR
064600000000     C*------------------------------------------------------------------------*
064700000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
064800000000     C*------------------------------------------------------------------------*
064900000000     C     F05STD        BEGSR
065000000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
065100000000     C                   EXSR      POSSFL
065200000000     C                   ENDSR
065300000000     C*------------------------------------------------------------------------*
065400000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
065500000000     C*------------------------------------------------------------------------*
065600000000     C     F17STD        BEGSR
065700000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
065800000000     C                   Z-ADD     1             VCCNBR
065900000000     C                   ENDSR
066000000000     C*------------------------------------------------------------------------*
066100000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
066200000000     C*------------------------------------------------------------------------*
066300000000     C     F18STD        BEGSR
066400000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
066500000000     C                   Z-ADD     SFLULT        VCCNBR
066600000000     C                   ENDSR
066700000000     C*------------------------------------------------------------------------*
066800000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
066900000000     C*------------------------------------------------------------------------*
067000000000     C     F23STD        BEGSR
067100000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
067200000000     C                   EXSR      OUTOPZ
067300000000     C                   ENDSR
067400000000     C*------------------------------------------------------------------------*
067500000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
067600000000     C*------------------------------------------------------------------------*
067700000000     C     RUPSTD        BEGSR
067800000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
067900000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
068000000000     C     CURSOR        IFGE      POSMIN
068100000000     C     CURSOR        ANDLE     POSMAX
068200000000     C                   ELSE
068300000000     C                   EXSR      CARSFL
068400000000     C                   ENDIF
068500000000     C                   ENDSR
068600000000     C*------------------------------------------------------------------------*
068700000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
068800000000     C*------------------------------------------------------------------------*
068900000000     C     RDNSTD        BEGSR
069000000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
069100000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
069200000000     C     CURSOR        IFGE      POSMIN
069300000000     C     CURSOR        ANDLE     POSMAX
069400000000     C                   ELSE
069500000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
069600000000     C                   MOVEL     '1'           $ERR
069700000000     C                   MOVEL     MSG(5)        $MSG
069800000000     C                   EXSR      CARMSG
069900000000     C                   ENDIF
070000000000     C                   ENDSR
070100000000     C*------------------------------------------------------------------------*
070200000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
070300000000     C*------------------------------------------------------------------------*
070400000000     C     ENTSFL        BEGSR
070500000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
070600000000     C                   EXSR      NEWPOS
070700000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
070800000000     C     $ERR          IFEQ      '0'
070900000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
071000000000     C     SWPOSI        IFNE      0
071100000000     C                   EXSR      POSSFL
071200000000     C                   ELSE
071300000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
071400000000     C     SFLULT        IFNE      0
071500000000     C                   EXSR      OPZSFL
071600000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
071700000000     C     SWMODI        IFNE      0
071800000000     C                   EXSR      POSSFL
071900000000     C                   ENDIF
072000000000     C                   ENDIF
072100000000     C                   ENDIF
072200000000     C                   ENDIF
072300000000     C                   ENDSR
072400000000     C*------------------------------------------------------------------------*
072500000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
072600000000     C*------------------------------------------------------------------------*
072700000000     C     POSSFL        BEGSR
072800000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
072900000000     C                   EXSR      CLRSFL
073000000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
073100000000     C                   EXSR      SETFIL
073200000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
073300000000     C     SFLEOF        IFEQ      0
073400000000     C                   EXSR      CARSFL
073500000000     C                   ENDIF
073600000000     C                   ENDSR
073700000000     C*------------------------------------------------------------------------*
073800000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
073900000000     C*------------------------------------------------------------------------*
074000000000     C     OPZSFL        BEGSR
074100000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
074200000000     C                   MOVEL     'S'           OPZOK             1
074300000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
074400000000     C                   Z-ADD     0             SFLVAR            5 0
074500000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
074600000000     C                   Z-ADD     0             SFLERR
074700000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
074800000000     C                   Z-ADD     SFLULT        WFLULT
074900000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
075000000000     C                   READC     TB62DVS                                49
075100000000     C     *IN49         DOWEQ     '0'
075200000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
075300000000     C                   ADD       1             SFLVAR
075400000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
075500000000     C     OPZOK         IFEQ      'S'
075600000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
075700000000     C     VSCOPZ        IFNE      *BLANKS
075800000000     C                   EXSR      CONOPZ
075900000000     C     OPZOK         IFEQ      'S'
076000000000     C                   EXSR      GESOPZ
076100000000     C                   ENDIF
076200000000     C                   ENDIF
076300000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
076400000000     C*            2. PULIZIA SCELTA IN SFL
076500000000     C     OPZOK         IFEQ      'S'
076600000000     C                   MOVEL     '0'           *IN41
076700000000     C                   MOVEL     *BLANKS       VSCOPZ
076800000000     C                   ELSE
076900000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
077000000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
077100000000     C                   MOVEL     '1'           *IN41
077200000000     C                   Z-ADD     SFLULT        SFLERR
077300000000     C                   ENDIF
077400000000     C                   ELSE
077500000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
077600000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
077700000000     C                   MOVEL     '1'           *IN41
077800000000     C                   ENDIF
077900000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
078000000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
078100000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
078200000000     C     OPZOK         IFNE      'S'
078300000000     C                   Z-ADD     SFLERR        VCCNBR
078400000000     C                   ELSE
078500000000     C                   Z-ADD     SFLULT        VCCNBR
078600000000     C                   ENDIF
078700000000     C* AGGIORNO RECORD SFL
078800000000     C                   UPDATE    TB62DVS
078900000000     C                   READC     TB62DVS                                49
079000000000     C                   ENDDO
079100000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
079200000000     C                   Z-ADD     WFLULT        SFLULT
079300000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
079400000000     C     SFLVAR        IFEQ      0
079500000000     C                   MOVEL     '1'           $FINE
079600000000     C                   ENDIF
079700000000     C                   ENDSR
079800000000     C*------------------------------------------------------------------------*
079900000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
080000000000     C*------------------------------------------------------------------------*
080100000000     C     CLRSFL        BEGSR
080200000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
080300000000     C* (0=NON FINE FILE  1=FINE FILE)
080400000000     C                   Z-ADD     0             SFLEOF
080500000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
080600000000     C                   Z-ADD     0             SFLULT
080700000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
080800000000     C                   MOVEL     '1'           *IN40
080900000000     C                   WRITE     TB62DVC
081000000000     C                   MOVEL     '0'           *IN40
081100000000     C                   ENDSR
081200000000     C*------------------------------------------------------------------------*
081300000000     C* CARSFL - CARICAMENTO SFL
081400000000     C*------------------------------------------------------------------------*
081500000000     C     CARSFL        BEGSR
081600000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
081700000000     C     SFLULT        IFEQ      0
081800000000     C                   EXSR      SETFIL
081900000000     C                   ENDIF
082000000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
082100000000     C                   Z-ADD     SFLULT        WFLULT
082200000000     C* CICLO:
082300000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
082400000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
082500000000     C                   Z-ADD     1             SFLRIG
082600000000     C     SFLRIG        DOWLE     SFLPAG
082700000000     C     SFLEOF        ANDEQ     0
082800000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
082900000000     C                   MOVEL     *BLANKS       VSCOPZ
083000000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
083100000000     C                   EXSR      IRESFL
083200000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
083300000000     C                   ADD       1             SFLULT
083400000000     C* SCRITTURA RECORD SFL
083500000000     C                   WRITE     TB62DVS
083600000000     C                   ADD       1             SFLRIG
083700000000     C* LETTURA FILE DA CARICARE
083800000000     C                   EXSR      LETFIL
083900000000     C                   ENDDO
084000000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
084100000000     C     WFLULT        IFNE      SFLULT
084200000000     C                   Z-ADD     WFLULT        VCCNBR
084300000000     C                   ADD       1             VCCNBR
084400000000     C                   ENDIF
084500000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
084600000000     C     SFLEOF        IFEQ      1
084700000000     C     SFLULT        ANDGT     *ZEROS
084800000000     C                   MOVEL     '1'           $ERR
084900000000     C                   MOVEL     MSG(6)        $MSG
085000000000     C                   EXSR      CARMSG
085100000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
085200000000     C     SFLULT        CHAIN     TB62DVS                            99
085300000000     C     *IN99         IFEQ      '0'
085400000000     C                   SETON                                        45
085500000000     C                   UPDATE    TB62DVS
085600000000     C                   SETOFF                                       45
085700000000     C                   ENDIF
085800000000     C                   ENDIF
085900000000     C                   ENDSR
086000000000     C*------------------------------------------------------------------------*
086100000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
086200000000     C*------------------------------------------------------------------------*
086300000000     C     OUTSFL        BEGSR
086400000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
086500000000     C     MSGULT        IFNE      *ZEROS
086600000000     C                   EXSR      OUTMSG
086700000000     C                   ENDIF
086800000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
086900000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
087000000000     C     SFLEOF        IFEQ      0
087100000000     C                   MOVEL     '0'           *IN42
087200000000     C                   ELSE
087300000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
087400000000     C                   MOVEL     '1'           *IN42
087500000000     C                   ENDIF
087600000000     C* EMISSIONE SUBFILE
087700000000     C     SFLULT        IFLE      0
087800000000     C*  --> CASO DI SFL VUOTO
087900000000     C                   MOVEL     '0'           *IN43
088000000000     C                   WRITE     TB62DVV
088100000000     C                   WRITE     TB62DVC
088200000000     C                   READ      TB62DVC                                99
088300000000     C                   ELSE
088400000000     C*  --> CASO DI SFL CON RECORDS
088500000000     C                   WRITE     TB62DVC
088600000000     C                   READ      TB62DVC                                99
088700000000     C                   ENDIF
088800000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
088900000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
089000000000     C                   Z-ADD     FRTSFL        VCCNBR
089100000000     C                   ENDSR
089200000000     C*------------------------------------------------------------------------*
089300000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
089400000000     C*          CODA MESSAGGI DEL PGM
089500000000     C*------------------------------------------------------------------------*
089600000000     C     CARMSG        BEGSR
089700000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
089800000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
089900000000     C     MSGULT        IFEQ      0
090000000000     C                   MOVEL     '1'           *IN40
090100000000     C                   WRITE     TB62DVK
090200000000     C                   MOVEL     '0'           *IN40
090300000000     C                   ENDIF
090400000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
090500000000     C                   MOVEL     $MSG          DSMSMS
090600000000     C                   CALL      'XMSG'        XMSGPA
090700000000     C     DSMSER        IFEQ      0
090800000000     C                   MOVEL     DSMSKE        VMCKEY
090900000000     C                   ENDIF
091000000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
091100000000     C                   ADD       1             MSGULT            4 0
091200000000     C                   WRITE     TB62DVM
091300000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
091400000000     C                   MOVEL     *BLANKS       $MSG
091500000000     C                   ENDSR
091600000000     C*------------------------------------------------------------------------*
091700000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
091800000000     C*------------------------------------------------------------------------*
091900000000     C     OUTMSG        BEGSR
092000000000     C* VISUALIZZAZIONE MESSAGGIO
092100000000     C                   MOVEL     '1'           *IN44
092200000000     C                   WRITE     TB62DVK
092300000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
092400000000     C                   Z-ADD     *ZEROS        MSGULT
092500000000     C                   ENDSR
092600000000     C*------------------------------------------------------------------------*
092700000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
092800000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
092900000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
093000000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
093100000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
093200000000     C*------------------------------------------------------------------------*
093300000000     C     CARFUN        BEGSR
093400000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
093500000000     C     1             DO        31            I                 5 0
093600000000     C                   MOVEL     FUN(I)        FUNRIG
093700000000     C                   MOVEL     FUNFUN        FFU(I)
093800000000     C                   MOVEL     FUNDES        FDE(I)
093900020923     C     DUTLPO        IFEQ      'S'
094000000000     C     $TGES         IFEQ      'G'
094100000000     C                   MOVEL     FUNASG        FAT(I)
094200000000     C                   ELSE
094300000000     C                   MOVEL     FUNASI        FAT(I)
094400000000     C                   ENDIF
094500000000     C                   ELSE
094600000000     C     $TGES         IFEQ      'G'
094700000000     C                   MOVEL     FUNAFG        FAT(I)
094800000000     C                   ELSE
094900000000     C                   MOVEL     FUNAFI        FAT(I)
095000000000     C                   ENDIF
095100000000     C                   ENDIF
095200000000     C     FAT(I)        IFEQ      'E'
095300000000     C                   MOVEL     USER          WEDP              3
095400000000     C     WEDP          IFEQ      'EDP'
095500000000     C                   MOVEL     'S'           FAT(I)
095600000000     C                   ELSE
095700000000     C                   MOVEL     'N'           FAT(I)
095800000000     C                   ENDIF
095900000000     C                   ENDIF
096000000000     C                   ENDDO
096100000000     C                   ENDSR
096200000000     C*------------------------------------------------------------------------*
096300000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
096400000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
096500000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
096600000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
096700000000     C*------------------------------------------------------------------------*
096800000000     C     OUTFUN        BEGSR
096900000000     C     1             DO        8             I
097000000000     C                   MOVEL     *BLANKS       FUV(I)
097100000000     C                   ENDDO
097200000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
097300000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
097400000000     C     1             DO        31            I
097500000000     C     FAT(I)        IFEQ      'S'
097600000000     C     FDE(I)        ANDNE     *BLANKS
097700000000     C                   ADD       1             FUNNAT
097800000000     C                   ENDIF
097900000000     C                   ENDDO
098000000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
098100000000     C     FUNNAT        IFGT      8
098200000000     C                   MOVEL     'S'           FAT(24)
098300000000     C                   ENDIF
098400000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
098500000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
098600000000     C                   Z-ADD     8             FUNVIS            5 0
098700000000     C     FAT(23)       IFEQ      'S'
098800000000     C                   SUB       1             FUNVIS
098900000000     C                   ENDIF
099000000000     C     FAT(24)       IFEQ      'S'
099100000000     C                   SUB       1             FUNVIS
099200000000     C                   ENDIF
099300000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
099400000000     C                   Z-ADD     *ZEROS        W                 5 0
099500000000     C                   ADD       1             FUNULT
099600000000     C     FUNULT        IFGT      31
099700000000     C                   Z-ADD     1             FUNULT
099800000000     C                   ENDIF
099900000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
100000000000     C     FUNULT        DO        31            I
100100000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
100200000000     C     W             IFLT      FUNVIS
100300000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
100400000000     C     FAT(I)        IFEQ      'S'
100500000000     C     FDE(I)        ANDNE     *BLANKS
100600000000     C     I             IFEQ      23
100700000000     C     I             OREQ      24
100800000000     C                   ELSE
100900000000     C                   ADD       1             W
101000000000     C                   MOVEL     FDE(I)        FUV(W)
101100000000     C                   ENDIF
101200000000     C                   ENDIF
101300000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
101400000000     C                   Z-ADD     I             FUNULT
101500000000     C                   ENDIF
101600000000     C                   ENDDO
101700000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
101800000000     C                   MOVEL     FUV(1)        VFD001
101900000000     C                   MOVEL     FUV(2)        VFD002
102000000000     C                   MOVEL     FUV(3)        VFD003
102100000000     C                   MOVEL     FUV(4)        VFD004
102200000000     C                   MOVEL     FUV(5)        VFD005
102300000000     C                   MOVEL     FUV(6)        VFD006
102400000000     C     FAT(23)       IFEQ      'N'
102500000000     C     FAT(24)       ANDEQ     'N'
102600000000     C                   MOVEL     FUV(7)        VFD007
102700000000     C                   MOVEL     FUV(8)        VFD008
102800000000     C                   ELSE
102900000000     C     FAT(23)       IFEQ      'S'
103000000000     C     FAT(24)       ANDEQ     'S'
103100000000     C                   MOVEL     FDE(23)       VFD007
103200000000     C                   MOVEL     FDE(24)       VFD008
103300000000     C                   ELSE
103400000000     C                   MOVEL     FUV(7)        VFD007
103500000000     C     FAT(23)       IFEQ      'S'
103600000000     C                   MOVEL     FDE(23)       VFD008
103700000000     C                   ELSE
103800000000     C                   MOVEL     FDE(24)       VFD008
103900000000     C                   ENDIF
104000000000     C                   ENDIF
104100000000     C                   ENDIF
104200000000     C                   MOVEL     '1'           $TESVD
104300000000     C                   ENDSR
104400000000     C*------------------------------------------------------------------------*
104500000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
104600000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
104700000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
104800000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
104900000000     C*------------------------------------------------------------------------*
105000000000     C     CONFUN        BEGSR
105100000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
105200000000     C     1             DO        31            I
105300000000     C     KEY           IFEQ      KEYS(I)
105400000000     C* CONTROLLO SE IL TASTO E' ATTIVO
105500000000     C     FAT(I)        IFNE      'S'
105600000000     C                   MOVEL     MSG(1)        $MSG
105700000000     C                   EXSR      CARMSG
105800000000     C                   MOVEL     '1'           $ERR
105900000000     C                   ENDIF
106000000000     C                   ENDIF
106100000000     C                   ENDDO
106200000000     C                   ENDSR
106300000000     C*------------------------------------------------------------------------*
106400000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
106500000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
106600000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
106700000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
106800000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
106900000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
107000000000     C*------------------------------------------------------------------------*
107100000000     C     CAROPZ        BEGSR
107200000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
107300000000     C     1             DO        24            I
107400000000     C                   MOVEL     OPZ(I)        OPZRIG
107500000000     C                   MOVEL     OPZOPD        OPD(I)
107600000000     C                   MOVEL     OPZOPS        OPS(I)
107700000000     C                   MOVEL     OPZDES        ODE(I)
107800020923     C     DUTLPO        IFEQ      'S'
107900000000     C     $TGES         IFEQ      'G'
108000000000     C                   MOVEL     OPZASG        OAT(I)
108100000000     C                   ELSE
108200000000     C                   MOVEL     OPZASI        OAT(I)
108300000000     C                   ENDIF
108400000000     C                   ELSE
108500000000     C     $TGES         IFEQ      'G'
108600000000     C                   MOVEL     OPZAFG        OAT(I)
108700000000     C                   ELSE
108800000000     C                   MOVEL     OPZAFI        OAT(I)
108900000000     C                   ENDIF
109000000000     C                   ENDIF
109100000000     C     OAT(I)        IFEQ      'E'
109200000000     C                   MOVEL     USER          WEDP              3
109300000000     C     WEDP          IFEQ      'EDP'
109400000000     C                   MOVEL     'S'           OAT(I)
109500000000     C                   ELSE
109600000000     C                   MOVEL     'N'           OAT(I)
109700000000     C                   ENDIF
109800000000     C                   ENDIF
109900000000     C                   ENDDO
110000000000     C                   ENDSR
110100000000     C*------------------------------------------------------------------------*
110200000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
110300000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
110400000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
110500000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
110600000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
110700000000     C*------------------------------------------------------------------------*
110800000000     C     OUTOPZ        BEGSR
110900000000     C     1             DO        8             I
111000000000     C                   MOVEL     *BLANKS       OPV(I)
111100000000     C                   ENDDO
111200000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
111300000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
111400000000     C                   ADD       1             OPZULT
111500000000     C     OPZULT        IFGT      24
111600000000     C                   Z-ADD     1             OPZULT
111700000000     C                   END
111800000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
111900000000     C     1             DO        24            I
112000000000     C     OAT(I)        IFEQ      'S'
112100000000     C     ODE(I)        ANDNE     *BLANKS
112200000000     C                   ADD       1             OPZNAT
112300000000     C                   ENDIF
112400000000     C                   ENDDO
112500000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
112600000000     C                   Z-ADD     0             W
112700000000     C     OPZULT        DO        24            I
112800000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
112900000000     C     W             IFLT      8
113000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
113100000000     C     OAT(I)        IFEQ      'S'
113200000000     C     ODE(I)        ANDNE     *BLANKS
113300000000     C                   ADD       1             W
113400000000     C                   MOVEL     ODE(I)        OPV(W)
113500000000     C                   ENDIF
113600000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
113700000000     C                   Z-ADD     I             OPZULT
113800000000     C                   ENDIF
113900000000     C                   ENDDO
114000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
114100000000     C                   MOVEL     OPV(1)        VOD001
114200000000     C                   MOVEL     OPV(2)        VOD002
114300000000     C                   MOVEL     OPV(3)        VOD003
114400000000     C                   MOVEL     OPV(4)        VOD004
114500000000     C                   MOVEL     OPV(5)        VOD005
114600000000     C                   MOVEL     OPV(6)        VOD006
114700000000     C                   MOVEL     OPV(7)        VOD007
114800000000     C                   MOVEL     OPV(8)        VOD008
114900000000     C                   MOVEL     '1'           $TESVD
115000000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
115100000000     C     OPZNAT        IFGT      8
115200000000     C                   MOVEL     'S'           FAT(23)
115300000000     C                   ENDIF
115400000000     C                   ENDSR
115500000000     C*------------------------------------------------------------------------*
115600000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
115700000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
115800000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
115900000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
116000000000     C*------------------------------------------------------------------------*
116100000000     C     CONOPZ        BEGSR
116200000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
116300000000     C                   MOVEL     *BLANKS       OPZOK             1
116400000000     C     1             DO        24            I                 5 0
116500000000     C     VSCOPZ        IFEQ      OPD(I)
116600000000     C     VSCOPZ        OREQ      OPS(I)
116700000000     C     OAT(I)        IFEQ      'S'
116800000000     C                   MOVEL     OPD(I)        VSCOPZ
116900000000     C                   MOVEL     'S'           OPZOK
117000000000     C                   ELSE
117100000000     C                   MOVEL     'N'           OPZOK             1
117200000000     C                   ENDIF
117300000000     C                   ENDIF
117400000000     C                   ENDDO
117500000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
117600000000     C     OPZOK         IFNE      'S'
117700000000     C                   MOVEL     MSG(3)        $MSG
117800000000     C                   EXSR      CARMSG
117900000000     C                   MOVEL     '1'           $ERR
118000000000     C                   MOVEL     'N'           OPZOK             1
118100000000     C                   ENDIF
118200000000     C                   ENDSR
118300000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
118400000000TRTB62R- Tasto funzionale non valido.                                          1
118500000000TRTB62R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
118600000000TRTB62R- Opzione non valida.                                                   3
118700000000TRTB62R- Opzione abilitata ma non gestita. Avvertire CED.                      4
118800000000TRTB62R- Inizio elenco.                                                        5
118900000000TRTB62R- Fine elenco.                                                          6
119000000000TRTB62R- 7
119100000000TRTB62R- 8
119200000000TRTB62R- 9
119300000000TRTB62R- 10
119400000000TRTB62R- 11
119500000000TRTB62R- Codice elemento annullato, consentito solo il ripristino             12
119600000000TRTB62R- Codice elemento non annullato, impossibile ripristinare              13
119700000000TRTB62R- 14
119800000000TRTB62R- 15
119900000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
120000000000- 1-1 -                   -    NN     NN
120100000000- 2-2 -2=Modifica         -    SS     SS
120200000000- 3-3 -                   -    NN     NN
120300000000- 4-4 -4=Cancellazione    -    SS     SS
120400000000- 5-5 -5=Visualizzazione  -    SS     SS
120500000000- 6-6 -                   -    NN     NN
120600000000- 7-7 -7=Ripristino       -    SS     SS
120700000000- 8-8 -                   -    NN     NN
120800000000- 9-9 -                   -    NN     NN
120900000000-10-  -                   -    NN     NN
121000000000-11-  -                   -    NN     NN
121100000000-12-  -                   -    NN     NN
121200000000-13-  -                   -    NN     NN
121300000000-14-  -                   -    NN     NN
121400000000-15-  -                   -    NN     NN
121500000000-16-  -                   -    NN     NN
121600000000-17-  -                   -    NN     NN
121700000000-18-  -                   -    NN     NN
121800000000-19-  -                   -    NN     NN
121900000000-20-  -                   -    NN     NN
122000000000-21-  -                   -    NN     NN
122100000000-22-  -                   -    NN     NN
122200000000-23-  -                   -    NN     NN
122300000000-24-  -                   -    NN     NN
122400000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
122500000000F1    -                   -    NN     NN
122600000000F2    -                   -    NN     NN
122700000000F3    -F3=Fine            -    SS     SS
122800000000F4    -                   -    NN     NN
122900000000F5    -F5=Rivisualizzaz.  -    SS     SS
123000000000F6    -F6=Inserimento     -    SS     SS
123100000000F7    -                   -    NN     NN
123200000000F8    -                   -    NN     NN
123300000000F9    -                   -    NN     NN
123400000000F10   -                   -    NN     NN
123500000000F11   -                   -    NN     NN
123600000000F12   -F12=Ritorno        -    SS     SS
123700000000F13   -                   -    NN     NN
123800000000F14   -                   -    NN     NN
123900000000F15   -                   -    NN     NN
124000000000F16   -                   -    NN     NN
124100000000F17   -F17=Inizio dati    -    SS     SS
124200000000F18   -F18=Fine dati      -    SS     SS
124300000000F19   -                   -    NN     NN
124400000000F20   -                   -    NN     NN
124500000000F21   -                   -    NN     NN
124600000000F22   -                   -    NN     NN
124700000000F23   -F23=Altre opzioni  -    NN     NN
124800000000F24   -F24=Altri tasti    -    NN     NN
124900000000ENTER -                   -    SS     SS
125000000000ROLLUP-                   -    SS     SS
125100000000ROLLDN-                   -    SS     SS
125200000000HELP  -                   -    NN     NN
125300000000PRINT -                   -    NN     NN
125400000000BSPACE-                   -    NN     NN
125500000000CLEAR -                   -    NN     NN
