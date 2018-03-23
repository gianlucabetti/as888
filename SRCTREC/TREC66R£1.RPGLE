000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC66R
000300000000      *  Descrizione   :  Gesione voci di maggiorazione di conto economico
000400000000      *                   Programma visualizzazione sfl di scelta
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:  18 MAG 1994
000700000000      *------------------------------------------------------------------------*
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000000     F*------------------------------------------------------------------------*
001000000000     F* DATA BASE
001100000000     F*------------------------------------------------------------------------*
001200000000     FTREC66D   CF   E             WORKSTN INFDS(DEVDS1)
001300000000     F                                     SFILE(EC66DVS:SFLULT)
001400000000     F                                     SFILE(EC66DVM:MSGULT)
001500000000     FECEVM01L  IF   E           K DISK
001600000000     FECEVT01L  IF   E           K DISK
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002100000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
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
007600000000     D* CHIAVE FUNZIONALE BATTUTA
007700000000     D  KEY                  369    369
007800000000     D* POSIZIONE CURSORE (IN HEX)
007900000000     D  CURSOR               370    371
008000000000     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
008100000000     D  FRTSFL               378    379B 0
008200000000     D*---------------------
008300000000     D* DS ESTERNA DI PROCEDURA
008400000000     D*---------------------
008500000000     D DS60          E DS                  EXTNAME(TREC60DS)
008600000000     D*---------------------
008700000000     D* ARCHITETTURA
008800000000     D*---------------------
008900000000     D KPJBA         E DS
009000000000     D*------------------
009100000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
009200000000     D*------------------
009300000000     D DESGES          C                   CONST('** GESTIONE **')
009400000000     D DESINT          C                   CONST('INTERROGAZIONE')
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
011000000000     C*
011100000000IF  1C     $F03          IFEQ      '1'
011200000000     C                   MOVEL     '1'           D60F03
011300000000E   1C                   ENDIF
011400000000     C* FINE PGM
011500000000     C                   MOVEL     DS60          KPJBU
011600000000     C                   SETON                                        LR
011700000000     C*------------------------------------------------------------------------*
011800000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
011900000000     C*------------------------------------------------------------------------*
012000000000     C     CARVS         BEGSR
012100000000     C*
012200000000     C                   MOVEL     'VS'          $GES
012300000000     C*
012400000000     C                   EXSR      CAROPZ
012500000000     C                   EXSR      OUTOPZ
012600000000     C                   MOVEA     FUN2          FUN
012700000000     C                   EXSR      CARFUN
012800000000     C                   EXSR      OUTFUN
012900000000     C*
013000000000     C                   WRITE     EC66DVT
013100000000     C                   WRITE     EC66DVO
013200000000     C                   WRITE     EC66DVF
013300000000     C*
013400000000     C                   EXSR      POSSFL
013500000000     C*
013600000000     C                   ENDSR
013700000000     C*------------------------------------------------------------------------*
013800000000     C* GESVS  - GESTIONE SUBFILE
013900000000     C*------------------------------------------------------------------------*
014000000000     C     GESVS         BEGSR
014100000000     C*
014200000000IF  1C     $TESVD        IFEQ      '1'
014300000000     C                   WRITE     EC66DVT
014400000000     C                   WRITE     EC66DVO
014500000000     C                   WRITE     EC66DVF
014600000000E   1C                   ENDIF
014700000000     C*
014800000000     C                   EXSR      OUTSFL
014900000000     C*
015000000000     C                   EXSR      CONFUN
015100000000     C*
015200000000IF  1C     $ERR          IFEQ      '0'
015300000000SL  2C                   SELECT
015400000000     C     KEY           WHENEQ    F6
015500000000     C                   EXSR      F06GES
015600000000     C                   OTHER
015700000000     C                   EXSR      FUNSFL
015800000000E   2C                   ENDSL
015900000000E   1C                   ENDIF
016000000000     C*
016100000000     C                   ENDSR
016200000000     C*------------------------------------------------------------------------*
016300000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
016400000000     C*------------------------------------------------------------------------*
016500000000     C     F06GES        BEGSR
016600000000     C*
016700000000     C* LANCIO L'INSERIMENTO
016800000000     C                   Z-ADD     *ZEROS        WVOM
016900000000     C                   Z-ADD     *ZEROS        WPRM
017000000000     C                   Z-ADD     *ZEROS        WVOC
017100000000     C                   Z-ADD     *ZEROS        WPRG
017200000000     C*
017300000000     C                   MOVEL     D60OP0        WOP0
017400000000     C                   MOVEL     'F06'         WOP1
017500000000     C                   MOVEL     'TREC67R'     WPGM              8
017600000000     C                   EXSR      LANPGM
017700000000     C*
017800000000     C                   EXSR      POSSFL
017900000000     C*
018000000000     C                   ENDSR
018100000000     C*------------------------------------------------------------------------*
018200000000     C*  GESOPZ - GESTIONE OPZIONI
018300000000     C*------------------------------------------------------------------------*
018400000000     C     GESOPZ        BEGSR
018500000000     C*
018600000000SL  1C                   SELECT
018700000000     C     VSCOPZ        WHENEQ    ' 2'
018800000000     C                   EXSR      O02GES
018900000000     C     VSCOPZ        WHENEQ    ' 4'
019000000000     C                   EXSR      O04GES
019100000000     C     VSCOPZ        WHENEQ    ' 5'
019200000000     C                   EXSR      O05GES
019300000000     C     VSCOPZ        WHENEQ    ' 7'
019400000000     C                   EXSR      O07GES
019500000000     C                   OTHER
019600000000     C                   MOVEL     MSG(4)        $MSG
019700000000     C                   EXSR      CARMSG
019800000000E   1C                   ENDSL
019900000000     C*
020000000000     C                   ENDSR
020100000000     C*------------------------------------------------------------------------*
020200000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
020300000000     C*------------------------------------------------------------------------*
020400000000     C     O02GES        BEGSR
020500000000     C*
020600000000IF  1C     VSCATB        IFEQ      'A'
020700000000     C                   MOVEL     '1'           $ERR
020800000000     C                   MOVEL     MSG(12)       $MSG
020900000000     C                   EXSR      CARMSG
021000000000E   1C                   ENDIF
021100000000     C*
021200000000IF  1C     $ERR          IFEQ      '0'
021300000000     C*
021400000000     C* LANCIO LA MODIFICA
021500000000     C                   Z-ADD     VSCVOM        WVOM
021600000000     C                   Z-ADD     VSCPRM        WPRM
021700000000     C                   Z-ADD     VSCVOC        WVOC
021800000000     C                   Z-ADD     VSCPRG        WPRG
021900000000     C*
022000000000     C                   MOVEL     D60OP0        WOP0
022100000000     C                   MOVEL     'O02'         WOP1
022200000000     C                   MOVEL     'TREC67R'     WPGM              8
022300000000     C                   EXSR      LANPGM
022400000000     C*
022500000000     C                   Z-ADD     1             SWMODI
022600000000     C*
022700000000E   1C                   ENDIF
022800000000     C*
022900000000     C                   ENDSR
023000000000     C*------------------------------------------------------------------------*
023100000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
023200000000     C*------------------------------------------------------------------------*
023300000000     C     O04GES        BEGSR
023400000000     C*
023500000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
023600000000IF  1C     VSCATB        IFEQ      'A'
023700000000     C                   MOVEL     '1'           $ERR
023800000000     C                   MOVEL     MSG(12)       $MSG
023900000000     C                   EXSR      CARMSG
024000000000E   1C                   ENDIF
024100000000     C*
024200000000IF  1C     $ERR          IFEQ      '0'
024300000000     C*
024400000000     C* LANCIO LA CANCELLAZIONE
024500000000     C                   Z-ADD     VSCVOM        WVOM
024600000000     C                   Z-ADD     VSCPRM        WPRM
024700000000     C                   Z-ADD     VSCVOC        WVOC
024800000000     C                   Z-ADD     VSCPRG        WPRG
024900000000     C*
025000000000     C                   MOVEL     D60OP0        WOP0
025100000000     C                   MOVEL     'O04'         WOP1
025200000000     C                   MOVEL     'TREC67R'     WPGM              8
025300000000     C                   EXSR      LANPGM
025400000000     C*
025500000000     C                   Z-ADD     1             SWMODI
025600000000     C*
025700000000E   1C                   ENDIF
025800000000     C*
025900000000     C                   ENDSR
026000000000     C*------------------------------------------------------------------------*
026100000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
026200000000     C*------------------------------------------------------------------------*
026300000000     C     O05GES        BEGSR
026400000000     C*
026500000000     C* LANCIO LA VISUALIZZAZIONE
026600000000     C                   Z-ADD     VSCVOM        WVOM
026700000000     C                   Z-ADD     VSCPRM        WPRM
026800000000     C                   Z-ADD     VSCVOC        WVOC
026900000000     C                   Z-ADD     VSCPRG        WPRG
027000000000     C*
027100000000     C                   MOVEL     D60OP0        WOP0
027200000000     C                   MOVEL     'O05'         WOP1
027300000000     C                   MOVEL     'TREC67R'     WPGM              8
027400000000     C                   EXSR      LANPGM
027500000000     C*
027600000000     C                   ENDSR
027700000000     C*------------------------------------------------------------------------*
027800000000     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
027900000000     C*------------------------------------------------------------------------*
028000000000     C     O07GES        BEGSR
028100000000     C*
028200000000     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
028300000000IF  1C     VSCATB        IFNE      'A'
028400000000     C                   MOVEL     '1'           $ERR
028500000000     C                   MOVEL     MSG(13)       $MSG
028600000000     C                   EXSR      CARMSG
028700000000E   1C                   ENDIF
028800000000     C*
028900000000IF  1C     $ERR          IFEQ      '0'
029000000000     C*
029100000000     C* LANCIO IL RIPRISTINO
029200000000     C                   Z-ADD     VSCVOM        WVOM
029300000000     C                   Z-ADD     VSCPRM        WPRM
029400000000     C                   Z-ADD     VSCVOC        WVOC
029500000000     C                   Z-ADD     VSCPRG        WPRG
029600000000     C*
029700000000     C                   MOVEL     D60OP0        WOP0
029800000000     C                   MOVEL     'O07'         WOP1
029900000000     C                   MOVEL     'TREC67R'     WPGM              8
030000000000     C                   EXSR      LANPGM
030100000000     C*
030200000000     C                   Z-ADD     1             SWMODI
030300000000     C*
030400000000E   1C                   ENDIF
030500000000     C*
030600000000     C                   ENDSR
030700000000     C*------------------------------------------------------------------------*
030800000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
030900000000     C*------------------------------------------------------------------------*
031000000000     C     NEWPOS        BEGSR
031100000000     C*
031200000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
031300000000     C*        1 = RICHIESTO POSIZIONAMENTO
031400000000     C                   Z-ADD     0             SWPOSI            7 0
031500000000     C*
031600000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
031700000000     C*            NON OCCORRE RICARICARE IL SUBFILE
031800000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
031900000000     C                   Z-ADD     0             SWMODI            1 0
032000000000     C*
032100000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
032200000000IF  1C     VCCVOM        IFNE      DEPVOM
032300000000IF  1C     VCCPRM        ORNE      DEPPRM
032400000000     C*
032500000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
032600000000     C                   EXSR      CHKPAR
032700000000     C*
032800000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
032900000000IF  2C     $ERR          IFEQ      '0'
033000000000     C                   Z-ADD     1             SWPOSI
033100000000     C                   Z-ADD     VCCVOM        DEPVOM
033200000000     C                   Z-ADD     VCCPRM        DEPPRM
033300000000E   2C                   ENDIF
033400000000E   1C                   ENDIF
033500000000     C*
033600000000     C                   ENDSR
033700000000     C*------------------------------------------------------------------------*
033800000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
033900000000     C*------------------------------------------------------------------------*
034000000000     C     CHKPAR        BEGSR
034100000000     C*
034200000000     C* REIMPOSTO INDICATORI DI ERRORE
034300000000     C                   SETOFF                                       80
034400000000     C*--------------------
034500000000     C*
034600000000     C*--------------------
034700000000     C*
034800000000     C*
034900000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
035000000000     C* SUL CAMPO DEL CONTROL
035100000000IF  1C     $ERR          IFEQ      '1'
035200000000     C                   MOVEL     '0'           *IN43
035300000000X   1C                   ELSE
035400000000     C                   MOVEL     '1'           *IN43
035500000000E   1C                   ENDIF
035600000000     C*
035700000000     C                   ENDSR
035800000000     C*------------------------------------------------------------------------*
035900000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD VOCE DI C/E
036000000000     C*------------------------------------------------------------------------*
036100000000     C     SETFIL        BEGSR
036200000000     C*
036300000000     C                   MOVEL     '1'           *IN43
036400000000     C*
036500000000     C* POSIZIONAMENTO ARCHIVIO
036600000000     C                   Z-ADD     VCCVOM        KVTVOM
036700000000     C                   Z-ADD     VCCPRM        KVTPRM
036800000000     C     KEYEVM        SETLL     ECEVM01L                           99
036900000000     C*
037000000000     C  N99              EXSR      LETFIL
037100000000     C*
037200000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
037300000000IF  1C     *IN99         IFEQ      '1'
037400000000     C                   Z-ADD     1             SFLEOF
037500000000E   1C                   ENDIF
037600000000     C*
037700000000     C                   ENDSR
037800000000     C*------------------------------------------------------------------------*
037900000000     C* LETFIL - LEGGE RECORD SUCESSIVO VOCE DI C/E
038000000000     C*------------------------------------------------------------------------*
038100000000     C     LETFIL        BEGSR
038200000000     C*
038300000000     C* LEGGO FINO A:
038400000000     C*     - FINE FILE
038500000000     C*     - TROVATO RECORD VALIDO
038600000000     C                   MOVEL     '0'           WRECOK            1
038700000000DO  1C     *IN99         DOWEQ     *OFF
038800000000     C     WRECOK        ANDEQ     '0'
038900000000     C*
039000000000     C* LETTURA
039100000000     C                   READ      ECEVM01L                               99
039200000000     C*
039300000000     C* FINE FILE  O TABELLA DIVERSA
039400000000IF  2C     *IN99         IFEQ      '1'
039500000000     C                   Z-ADD     1             SFLEOF
039600000000X   2C                   ELSE
039700000000     C                   EXSR      CHKREC
039800000000E   2C                   ENDIF
039900000000     C*
040000000000E   1C                   ENDDO
040100000000     C*
040200000000     C                   ENDSR
040300000000     C*------------------------------------------------------------------------*
040400000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
040500000000     C*------------------------------------------------------------------------*
040600000000     C     CHKREC        BEGSR
040700000000     C*
040800000000     C                   MOVEL     '1'           WRECOK
040900000000     C*
041000000000     C                   ENDSR
041100000000     C*------------------------------------------------------------------------*
041200000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
041300000000     C*------------------------------------------------------------------------*
041400000000     C     IRESFL        BEGSR
041500000000     C*
041600000000IF  1C     EVMATB        IFNE      *BLANKS
041700000000     C                   MOVEL     'A'           VSCATB
041800000000X   1C                   ELSE
041900000000     C                   MOVEL     ' '           VSCATB
042000000000E   1C                   ENDIF
042100000000     C*
042200000000     C                   Z-ADD     EVMVOM        VSCVOM
042300000000     C                   Z-ADD     EVMPRM        VSCPRM
042400000000     C                   Z-ADD     EVMVOC        VSCVOC
042500000000     C                   Z-ADD     EVMPRG        VSCPRG
042501100326     C                   Z-ADD     EVMViso       VSCVsal
042600000000     C*
042700000000     C*  DECODIFICO LE DUE VOCI
042800000000     C                   Z-ADD     EVMVOM        KVTVOM
042900000000     C                   Z-ADD     EVMPRM        KVTPRM
043000000000     C     KEYEVM        CHAIN     ECEVT01L                           98
043100000000     C  N98              MOVEL     EVTVOD        VSCVDM
043200000000     C   98              MOVE      *BLANKS       VSCVDM
043300000000     C*
043400000000     C                   Z-ADD     EVMVOC        KVTVOM
043500000000     C                   Z-ADD     EVMPRG        KVTPRM
043600000000     C     KEYEVM        CHAIN     ECEVT01L                           98
043700000000     C  N98              MOVEL     EVTVOD        VSCVDC
043800000000     C   98              MOVE      *BLANKS       VSCVDC
043900000000     C*
044000000000     C                   ENDSR
044100000000     C*------------------------------------------------------------------------*
044200000000     C* LANPGM - LANCIA UN PROGRAMMA
044300000000     C*------------------------------------------------------------------------*
044400000000     C     LANPGM        BEGSR
044500000000     C*--------------------
044600000000     C* PARAMETRI INFORMATIVI
044700000000     C*--------------------
044800000000     C                   MOVEL     WOP0          D60OP0
044900000000     C                   MOVEL     WOP1          D60OP1
045000000000     C                   MOVEL     '0'           D60F03
045100000000     C                   MOVEL     '0'           D60ERR
045200000000     C                   MOVEL     *BLANKS       D60MSG
045300000000     C*--------------------
045400000000     C* PARAMETRI
045500000000     C*--------------------
045600000000     C                   Z-ADD     WVOC          D60VOC
045700000000     C                   Z-ADD     WPRG          D60PRG
045800000000     C                   Z-ADD     WVOM          D60VOM
045900000000     C                   Z-ADD     WPRM          D60PRM
046000000000     C* LANCIO
046100000000     C                   MOVEL     DS60          KPJBU
046200000000     C                   CALL      WPGM                                 99
046300000000     C                   PARM                    KPJBA
046400000000     C                   MOVEL     KPJBU         DS60
046500000000     C*
046600000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
046700000000IF  1C     D60F03        IFEQ      '1'
046800000000     C                   MOVEL     '1'           $F03
046900000000     C                   MOVEL     '1'           $ERR
047000000000     C                   MOVEL     '1'           $FINE
047100000000     C*
047200000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
047300000000X   1C                   ELSE
047400000000     C                   MOVEL     D60MSG        $MSG
047500000000     C                   EXSR      CARMSG
047600000000E   1C                   ENDIF
047700000000     C*
047800000000     C                   ENDSR
047900020923     C*-------------------------------------------------------------------------
048000020923     C* REPERISCE I DATI UTENTE
048100020923     C*-------------------------------------------------------------------------
048200020923     C     REPDATIUTE    BEGSR
048300020923     C*
048400020923     C* INIZIALIZZA VARIABILI DI WRK
048500020923     C                   CLEAR                   TIBS34DS
048600020923     C                   CLEAR                   AZUTEDS
048700020923     C                   CLEAR                   DDATIUTE
048800020923     C*
048900020923     C     *DTAARA       DEFINE    §azute        azuteds
049000020923     C     *DTAARA       DEFINE    §datiute      ddatiute
049100020923     C                   IN(E)     *DTAARA
049200020923if  1C                   IF        %Error
049300020923     c                   EVAL      I34Tla = 'L'
049400020923     C                   CALL      'TIBS34R'
049500020923     C                   PARM                    Tibs34Ds
049600020923     C                   IN        *DTAARA
049700020923e   1C                   ENDIF
049800020923     C*
049900020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
050000020923     C                   MOVEL(P)  RSUT          DSFIRS           20
050100020923     C*
050200020923     C                   ENDSR
050300000000     C*------------------------------------------------------------------------*
050400000000     C*  *INZSR - ROUTINE INIZIALE
050500000000     C*------------------------------------------------------------------------*
050600000000     C     *INZSR        BEGSR
050700000000     C*
050800000000     C* RICEVIMENTO PARAMETRI
050900000000     C     *ENTRY        PLIST
051000000000     C                   PARM                    KPJBA
051100000000     C                   MOVEL     KPJBU         DS60
051200000000     C*
051300000000     C* DEFINIZIONE KLIST
051400000000     C     *LIKE         DEFINE    EVMVOM        KVTVOM
051500000000     C     *LIKE         DEFINE    EVMPRM        KVTPRM
051600000000     C     *LIKE         DEFINE    EVMVOC        KVTVOC
051700000000     C     *LIKE         DEFINE    EVMPRG        KVTPRG
051800000000     C*
051900000000     C     *LIKE         DEFINE    D60OP0        WOP0
052000000000     C     *LIKE         DEFINE    D60OP1        WOP1
052100000000     C     *LIKE         DEFINE    D60VOC        WVOC
052200000000     C     *LIKE         DEFINE    D60PRG        WPRG
052300000000     C     *LIKE         DEFINE    D60VOM        WVOM
052400000000     C     *LIKE         DEFINE    D60PRM        WPRM
052500000000     C*
052600000000     C* DEFINIZIONE DEPOSITI
052700000000     C     *LIKE         DEFINE    VCCVOM        DEPVOM
052800000000     C     *LIKE         DEFINE    VCCPRM        DEPPRM
052900000000     C*
053000000000     C* CHIAVE LETTURA ECEVM01L - COMPLETA
053100000000     C     KEYEVM        KLIST
053200000000     C                   KFLD                    KVTVOM
053300000000     C                   KFLD                    KVTPRM
053400000000     C*
053500000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
053600000000     C                   MOVE      D60OP0        INTGES            2
053700000000IF  1C     INTGES        IFEQ      '02'
053800000000     C                   MOVEL     'G'           $TGES             1
053900000000     C                   MOVEL     DESGES        VTDOPE
054000000000X   1C                   ELSE
054100000000     C                   MOVEL     'I'           $TGES             1
054200000000     C                   MOVEL     DESINT        VTDOPE
054300000000E   1C                   ENDIF
054400000000     C*
054500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
054600000000     C                   EXSR      INZSTD
054700000000     C*
054800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
054900000000     C                   EXSR      INZSFL
055000000000     C*
055100000000     C* IMPOSTO CAMPI TESTATA VIDEO
055200000000     C                   MOVEL     DSFIRS        VTDRSU
055300000000     C*
055400000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
055500000000     C* SE RICHIAMATO DA ALTRO PGM IMPOSTO I CAMPI PASSATI
055600000000     C                   MOVEL     D60OP0        $UNO              1
055700000000     C     $UNO          IFEQ      'M'
055800000000     C                   Z-ADD     *ZEROS        VCCVOM
055900000000     C                   Z-ADD     *ZEROS        DEPVOM
056000000000     C                   Z-ADD     *ZEROS        VCCPRM
056100000000     C                   Z-ADD     *ZEROS        DEPPRM
056200000000     C                   ELSE
056300000000     C                   Z-ADD     D60VOM        VCCVOM
056400000000     C                   Z-ADD     D60VOM        DEPVOM
056500000000     C                   Z-ADD     D60PRM        VCCPRM
056600000000     C                   Z-ADD     D60PRM        DEPPRM
056700000000     C                   ENDIF
056800000000     C*
056900000000     C* EMETTO VIDEATA SUBFILE
057000000000     C                   EXSR      CARVS
057100000000     C*
057200000000     C                   ENDSR
057300000000     C*------------------------------------------------------------------------*
057400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057600000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
057700000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
057800000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
057900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
058000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
058100000000     C*------------------------------------------------------------------------*
058200000000     C* INZSTD - ROUTINE INIZIALE STANDARD
058300000000     C*------------------------------------------------------------------------*
058400000000     C     INZSTD        BEGSR
058500000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
058600000000     C                   CALL      'XKEYS'
058700000000     C                   PARM                    DSKEYS
058800000000     C                   MOVEA     KEYRIG        KEYS
058900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
059000000000     C     XMSGPA        PLIST
059100000000     C                   PARM                    DSMSMS
059200000000     C                   PARM                    DSMSPG
059300000000     C                   PARM                    DSMSKE
059400000000     C                   PARM                    DSMSER
059500000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
059600000000     C                   MOVEL     'TREC66R'     DSMSPG
059700000000     C                   MOVEL     'TREC66R'     VMCQUE
059800000000     C                   MOVEL     *BLANKS       VMCKEY
059900000000     C                   MOVEL     *BLANKS       DSMSMS
060000000000     C* INIZIALIZZO FLAG CONTROLLO PGM
060100000000     C                   MOVEL     '0'           $ERR              1
060200000000     C                   MOVEL     '0'           $F03              1
060300000000     C                   MOVEL     *BLANKS       $GES              2
060400000000     C                   MOVEL     *BLANKS       $MSG             80
060500000000     C                   MOVEL     '0'           $FINE             1
060600000000     C                   MOVEL     '0'           $TESVD            1
060700000000     C* LANCIO ROUTINE PER VERIFICARE SE
060800000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
060900000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
061000020923     C                   EXSR      REPDATIUTE
061100000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
061200000000     C                   BITOFF    '01234567'    POSMI1            1
061300000000     C                   BITOFF    '01234567'    POSMI2            1
061400000000     C                   BITOFF    '01234567'    POSMA1            1
061500000000     C                   BITOFF    '01234567'    POSMA2            1
061600000000     C                   BITON     '34'          POSMI1            1
061700000000     C                   BITON     '7'           POSMI2            1
061800000000     C                   BITON     '34'          POSMA1            1
061900000000     C                   BITON     '0'           POSMA2            1
062000000000     C                   MOVE      POSMI2        POSMIN            2
062100000000     C                   MOVEL     POSMI1        POSMIN
062200000000     C                   MOVE      POSMA2        POSMAX            2
062300000000     C                   MOVEL     POSMA1        POSMAX
062400000000     C                   ENDSR
062500000000     C*------------------------------------------------------------------------*
062600000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
062700000000     C*------------------------------------------------------------------------*
062800000000     C     FUNSTD        BEGSR
062900000000     C                   SELECT
063000000000     C     KEY           WHENEQ    F3
063100000000     C                   EXSR      F03STD
063200000000     C     KEY           WHENEQ    F12
063300000000     C                   EXSR      F12STD
063400000000     C     KEY           WHENEQ    F24
063500000000     C                   EXSR      F24STD
063600000000     C                   OTHER
063700000000     C                   MOVEL     MSG(2)        $MSG
063800000000     C                   EXSR      CARMSG
063900000000     C                   ENDSL
064000000000     C                   ENDSR
064100000000     C*------------------------------------------------------------------------*
064200000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
064300000000     C*------------------------------------------------------------------------*
064400000000     C     F03STD        BEGSR
064500000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
064600000000     C                   MOVEL     '1'           $F03
064700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
064800000000     C                   MOVEL     '1'           $FINE
064900000000     C                   ENDSR
065000000000     C*------------------------------------------------------------------------*
065100000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
065200000000     C*------------------------------------------------------------------------*
065300000000     C     F12STD        BEGSR
065400000000     C* IMPOSTO FLAG DI FIN PGM
065500000000     C                   MOVEL     '1'           $FINE
065600000000     C                   ENDSR
065700000000     C*------------------------------------------------------------------------*
065800000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
065900000000     C*------------------------------------------------------------------------*
066000000000     C     F24STD        BEGSR
066100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
066200000000     C                   EXSR      OUTFUN
066300000000     C                   ENDSR
066400000000     C*------------------------------------------------------------------------*
066500000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
066600000000     C*------------------------------------------------------------------------*
066700000000     C     INZSFL        BEGSR
066800000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
066900000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
067000000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
067100000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
067200000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
067300000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
067400000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
067500000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
067600000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
067700000000     C                   Z-ADD     12            SFLPAG
067800000000     C                   ENDSR
067900000000     C*------------------------------------------------------------------------*
068000000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
068100000000     C*------------------------------------------------------------------------*
068200000000     C     FUNSFL        BEGSR
068300000000     C                   SELECT
068400000000     C     KEY           WHENEQ    ENTER
068500000000     C                   EXSR      ENTSFL
068600000000     C     KEY           WHENEQ    F3
068700000000     C                   EXSR      F03STD
068800000000     C     KEY           WHENEQ    F5
068900000000     C                   EXSR      F05STD
069000000000     C     KEY           WHENEQ    F12
069100000000     C                   EXSR      F12STD
069200000000     C     KEY           WHENEQ    F17
069300000000     C                   EXSR      F17STD
069400000000     C     KEY           WHENEQ    F18
069500000000     C                   EXSR      F18STD
069600000000     C     KEY           WHENEQ    F23
069700000000     C                   EXSR      F23STD
069800000000     C     KEY           WHENEQ    F24
069900000000     C                   EXSR      F24STD
070000000000     C     KEY           WHENEQ    ROLLUP
070100000000     C                   EXSR      RUPSTD
070200000000     C     KEY           WHENEQ    ROLLDN
070300000000     C                   EXSR      RDNSTD
070400000000     C                   OTHER
070500000000     C                   MOVEL     MSG(2)        $MSG
070600000000     C                   EXSR      CARMSG
070700000000     C                   ENDSL
070800000000     C                   ENDSR
070900000000     C*------------------------------------------------------------------------*
071000000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
071100000000     C*------------------------------------------------------------------------*
071200000000     C     F05STD        BEGSR
071300000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
071400000000     C                   EXSR      POSSFL
071500000000     C                   ENDSR
071600000000     C*------------------------------------------------------------------------*
071700000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
071800000000     C*------------------------------------------------------------------------*
071900000000     C     F17STD        BEGSR
072000000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
072100000000     C                   Z-ADD     1             VCCNBR
072200000000     C                   ENDSR
072300000000     C*------------------------------------------------------------------------*
072400000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
072500000000     C*------------------------------------------------------------------------*
072600000000     C     F18STD        BEGSR
072700000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
072800000000     C                   Z-ADD     SFLULT        VCCNBR
072900000000     C                   ENDSR
073000000000     C*------------------------------------------------------------------------*
073100000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
073200000000     C*------------------------------------------------------------------------*
073300000000     C     F23STD        BEGSR
073400000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
073500000000     C                   EXSR      OUTOPZ
073600000000     C                   ENDSR
073700000000     C*------------------------------------------------------------------------*
073800000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
073900000000     C*------------------------------------------------------------------------*
074000000000     C     RUPSTD        BEGSR
074100000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
074200000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
074300000000     C     CURSOR        IFGE      POSMIN
074400000000     C     CURSOR        ANDLE     POSMAX
074500000000     C                   ELSE
074600000000     C                   EXSR      CARSFL
074700000000     C                   ENDIF
074800000000     C                   ENDSR
074900000000     C*------------------------------------------------------------------------*
075000000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
075100000000     C*------------------------------------------------------------------------*
075200000000     C     RDNSTD        BEGSR
075300000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
075400000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
075500000000     C     CURSOR        IFGE      POSMIN
075600000000     C     CURSOR        ANDLE     POSMAX
075700000000     C                   ELSE
075800000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
075900000000     C                   MOVEL     '1'           $ERR
076000000000     C                   MOVEL     MSG(5)        $MSG
076100000000     C                   EXSR      CARMSG
076200000000     C                   ENDIF
076300000000     C                   ENDSR
076400000000     C*------------------------------------------------------------------------*
076500000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
076600000000     C*------------------------------------------------------------------------*
076700000000     C     ENTSFL        BEGSR
076800000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
076900000000     C                   EXSR      NEWPOS
077000000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
077100000000     C     $ERR          IFEQ      '0'
077200000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
077300000000     C     SWPOSI        IFNE      0
077400000000     C                   EXSR      POSSFL
077500000000     C                   ELSE
077600000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
077700000000     C     SFLULT        IFNE      0
077800000000     C                   EXSR      OPZSFL
077900000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
078000000000     C     SWMODI        IFNE      0
078100000000     C                   EXSR      POSSFL
078200000000     C                   ENDIF
078300000000     C                   ENDIF
078400000000     C                   ENDIF
078500000000     C                   ENDIF
078600000000     C                   ENDSR
078700000000     C*------------------------------------------------------------------------*
078800000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
078900000000     C*------------------------------------------------------------------------*
079000000000     C     POSSFL        BEGSR
079100000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
079200000000     C                   EXSR      CLRSFL
079300000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
079400000000     C                   EXSR      SETFIL
079500000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
079600000000     C     SFLEOF        IFEQ      0
079700000000     C                   EXSR      CARSFL
079800000000     C                   ENDIF
079900000000     C                   ENDSR
080000000000     C*------------------------------------------------------------------------*
080100000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
080200000000     C*------------------------------------------------------------------------*
080300000000     C     OPZSFL        BEGSR
080400000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
080500000000     C                   MOVEL     'S'           OPZOK             1
080600000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
080700000000     C                   Z-ADD     0             SFLVAR            5 0
080800000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
080900000000     C                   Z-ADD     0             SFLERR
081000000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
081100000000     C                   Z-ADD     SFLULT        WFLULT
081200000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
081300000000     C                   READC     EC66DVS                                49
081400000000     C     *IN49         DOWEQ     '0'
081500941121     C     $ERR          ANDEQ     '0'
081600000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
081700000000     C                   ADD       1             SFLVAR
081800000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
081900000000     C     OPZOK         IFEQ      'S'
082000000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
082100000000     C     VSCOPZ        IFNE      *BLANKS
082200000000     C                   EXSR      CONOPZ
082300000000     C     OPZOK         IFEQ      'S'
082400941121     C     $ERR          ANDEQ     '0'
082500000000     C                   EXSR      GESOPZ
082600000000     C                   ENDIF
082700000000     C                   ENDIF
082800000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
082900000000     C*            2. PULIZIA SCELTA IN SFL
083000000000     C     OPZOK         IFEQ      'S'
083100000000     C                   MOVEL     '0'           *IN41
083200000000     C                   MOVEL     *BLANKS       VSCOPZ
083300000000     C                   ELSE
083400000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
083500000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
083600000000     C                   MOVEL     '1'           *IN41
083700000000     C                   Z-ADD     SFLULT        SFLERR
083800000000     C                   ENDIF
083900000000     C                   ELSE
084000000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
084100000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
084200000000     C                   MOVEL     '1'           *IN41
084300000000     C                   ENDIF
084400000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
084500000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
084600000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
084700000000     C     OPZOK         IFNE      'S'
084800000000     C                   Z-ADD     SFLERR        VCCNBR
084900000000     C                   ELSE
085000000000     C                   Z-ADD     SFLULT        VCCNBR
085100000000     C                   ENDIF
085200000000     C* AGGIORNO RECORD SFL
085300000000     C                   UPDATE    EC66DVS
085400941121     C     $ERR          IFEQ      '0'
085500000000     C                   READC     EC66DVS                                49
085600941121     C                   ENDIF
085700000000     C                   ENDDO
085800000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
085900000000     C                   Z-ADD     WFLULT        SFLULT
086000000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
086100000000     C     SFLVAR        IFEQ      0
086200000000     C                   MOVEL     '1'           $FINE
086300000000     C                   ENDIF
086400000000     C                   ENDSR
086500000000     C*------------------------------------------------------------------------*
086600000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
086700000000     C*------------------------------------------------------------------------*
086800000000     C     CLRSFL        BEGSR
086900000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
087000000000     C* (0=NON FINE FILE  1=FINE FILE)
087100000000     C                   Z-ADD     0             SFLEOF
087200000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
087300000000     C                   Z-ADD     0             SFLULT
087400000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
087500000000     C                   MOVEL     '1'           *IN40
087600000000     C                   WRITE     EC66DVC
087700000000     C                   MOVEL     '0'           *IN40
087800000000     C                   ENDSR
087900000000     C*------------------------------------------------------------------------*
088000000000     C* CARSFL - CARICAMENTO SFL
088100000000     C*------------------------------------------------------------------------*
088200000000     C     CARSFL        BEGSR
088300000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
088400000000     C     SFLULT        IFEQ      0
088500000000     C                   EXSR      SETFIL
088600000000     C                   ENDIF
088700000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
088800000000     C                   Z-ADD     SFLULT        WFLULT
088900000000     C* CICLO:
089000000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
089100000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
089200000000     C                   Z-ADD     1             SFLRIG
089300000000     C     SFLRIG        DOWLE     SFLPAG
089400000000     C     SFLEOF        ANDEQ     0
089500000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
089600000000     C                   MOVEL     *BLANKS       VSCOPZ
089700000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
089800000000     C                   EXSR      IRESFL
089900000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
090000000000     C                   ADD       1             SFLULT
090100000000     C* SCRITTURA RECORD SFL
090200000000     C                   WRITE     EC66DVS
090300000000     C                   ADD       1             SFLRIG
090400000000     C* LETTURA FILE DA CARICARE
090500000000     C                   EXSR      LETFIL
090600000000     C                   ENDDO
090700000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
090800000000     C     WFLULT        IFNE      SFLULT
090900000000     C                   Z-ADD     WFLULT        VCCNBR
091000000000     C                   ADD       1             VCCNBR
091100000000     C                   ENDIF
091200000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
091300000000     C     SFLEOF        IFEQ      1
091400000000     C     SFLULT        ANDGT     *ZEROS
091500000000     C                   MOVEL     '1'           $ERR
091600000000     C                   MOVEL     MSG(6)        $MSG
091700000000     C                   EXSR      CARMSG
091800000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
091900000000     C     SFLULT        CHAIN     EC66DVS                            99
092000000000     C     *IN99         IFEQ      '0'
092100000000     C                   SETON                                        45
092200000000     C                   UPDATE    EC66DVS
092300000000     C                   SETOFF                                       45
092400000000     C                   ENDIF
092500000000     C                   ENDIF
092600000000     C                   ENDSR
092700000000     C*------------------------------------------------------------------------*
092800000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
092900000000     C*------------------------------------------------------------------------*
093000000000     C     OUTSFL        BEGSR
093100000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
093200000000     C     MSGULT        IFNE      *ZEROS
093300000000     C                   EXSR      OUTMSG
093400000000     C                   ENDIF
093500000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
093600000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
093700000000     C     SFLEOF        IFEQ      0
093800000000     C                   MOVEL     '0'           *IN42
093900000000     C                   ELSE
094000000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
094100000000     C                   MOVEL     '1'           *IN42
094200000000     C                   ENDIF
094300000000     C* EMISSIONE SUBFILE
094400000000     C     SFLULT        IFLE      0
094500000000     C*  --> CASO DI SFL VUOTO
094600000000     C                   MOVEL     '0'           *IN43
094700000000     C                   WRITE     EC66DVV
094800000000     C                   WRITE     EC66DVC
094900000000     C                   READ      EC66DVC                                99
095000000000     C                   ELSE
095100000000     C*  --> CASO DI SFL CON RECORDS
095200000000     C                   WRITE     EC66DVC
095300000000     C                   READ      EC66DVC                                99
095400000000     C                   ENDIF
095500000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
095600000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
095700000000     C                   Z-ADD     FRTSFL        VCCNBR
095800000000     C                   ENDSR
095900000000     C*------------------------------------------------------------------------*
096000000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
096100000000     C*          CODA MESSAGGI DEL PGM
096200000000     C*------------------------------------------------------------------------*
096300000000     C     CARMSG        BEGSR
096400000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
096500000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
096600000000     C     MSGULT        IFEQ      0
096700000000     C                   MOVEL     '1'           *IN40
096800000000     C                   WRITE     EC66DVK
096900000000     C                   MOVEL     '0'           *IN40
097000000000     C                   ENDIF
097100000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
097200000000     C                   MOVEL     $MSG          DSMSMS
097300000000     C                   CALL      'XMSG'        XMSGPA
097400000000     C     DSMSER        IFEQ      0
097500000000     C                   MOVEL     DSMSKE        VMCKEY
097600000000     C                   ENDIF
097700000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
097800000000     C                   ADD       1             MSGULT            4 0
097900000000     C                   WRITE     EC66DVM
098000000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
098100000000     C                   MOVEL     *BLANKS       $MSG
098200000000     C                   ENDSR
098300000000     C*------------------------------------------------------------------------*
098400000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
098500000000     C*------------------------------------------------------------------------*
098600000000     C     OUTMSG        BEGSR
098700000000     C* VISUALIZZAZIONE MESSAGGIO
098800000000     C                   MOVEL     '1'           *IN44
098900000000     C                   WRITE     EC66DVK
099000000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
099100000000     C                   Z-ADD     *ZEROS        MSGULT
099200000000     C                   ENDSR
099300000000     C*------------------------------------------------------------------------*
099400000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
099500000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
099600000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
099700000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
099800000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
099900000000     C*------------------------------------------------------------------------*
100000000000     C     CARFUN        BEGSR
100100000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
100200000000     C     1             DO        31            I                 5 0
100300000000     C                   MOVEL     FUN(I)        FUNRIG
100400000000     C                   MOVEL     FUNFUN        FFU(I)
100500000000     C                   MOVEL     FUNDES        FDE(I)
100600020923     C     DUTLPO        IFEQ      'S'
100700000000     C     $TGES         IFEQ      'G'
100800000000     C                   MOVEL     FUNASG        FAT(I)
100900000000     C                   ELSE
101000000000     C                   MOVEL     FUNASI        FAT(I)
101100000000     C                   ENDIF
101200000000     C                   ELSE
101300000000     C     $TGES         IFEQ      'G'
101400000000     C                   MOVEL     FUNAFG        FAT(I)
101500000000     C                   ELSE
101600000000     C                   MOVEL     FUNAFI        FAT(I)
101700000000     C                   ENDIF
101800000000     C                   ENDIF
101900000000     C     FAT(I)        IFEQ      'E'
102000000000     C                   MOVEL     USER          WEDP              3
102100000000     C     WEDP          IFEQ      'EDP'
102200000000     C                   MOVEL     'S'           FAT(I)
102300000000     C                   ELSE
102400000000     C                   MOVEL     'N'           FAT(I)
102500000000     C                   ENDIF
102600000000     C                   ENDIF
102700000000     C                   ENDDO
102800000000     C                   ENDSR
102900000000     C*------------------------------------------------------------------------*
103000000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
103100000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
103200000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
103300000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
103400000000     C*------------------------------------------------------------------------*
103500000000     C     OUTFUN        BEGSR
103600000000     C     1             DO        8             I
103700000000     C                   MOVEL     *BLANKS       FUV(I)
103800000000     C                   ENDDO
103900000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
104000000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
104100000000     C     1             DO        31            I
104200000000     C     FAT(I)        IFEQ      'S'
104300000000     C     FDE(I)        ANDNE     *BLANKS
104400000000     C                   ADD       1             FUNNAT
104500000000     C                   ENDIF
104600000000     C                   ENDDO
104700000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
104800000000     C     FUNNAT        IFGT      8
104900000000     C                   MOVEL     'S'           FAT(24)
105000000000     C                   ENDIF
105100000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
105200000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
105300000000     C                   Z-ADD     8             FUNVIS            5 0
105400000000     C     FAT(23)       IFEQ      'S'
105500000000     C                   SUB       1             FUNVIS
105600000000     C                   ENDIF
105700000000     C     FAT(24)       IFEQ      'S'
105800000000     C                   SUB       1             FUNVIS
105900000000     C                   ENDIF
106000000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
106100000000     C                   Z-ADD     *ZEROS        W                 5 0
106200000000     C                   ADD       1             FUNULT
106300000000     C     FUNULT        IFGT      31
106400000000     C                   Z-ADD     1             FUNULT
106500000000     C                   ENDIF
106600000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
106700000000     C     FUNULT        DO        31            I
106800000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
106900000000     C     W             IFLT      FUNVIS
107000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
107100000000     C     FAT(I)        IFEQ      'S'
107200000000     C     FDE(I)        ANDNE     *BLANKS
107300000000     C     I             IFEQ      23
107400000000     C     I             OREQ      24
107500000000     C                   ELSE
107600000000     C                   ADD       1             W
107700000000     C                   MOVEL     FDE(I)        FUV(W)
107800000000     C                   ENDIF
107900000000     C                   ENDIF
108000000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
108100000000     C                   Z-ADD     I             FUNULT
108200000000     C                   ENDIF
108300000000     C                   ENDDO
108400000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
108500000000     C                   MOVEL     FUV(1)        VFD001
108600000000     C                   MOVEL     FUV(2)        VFD002
108700000000     C                   MOVEL     FUV(3)        VFD003
108800000000     C                   MOVEL     FUV(4)        VFD004
108900000000     C                   MOVEL     FUV(5)        VFD005
109000000000     C                   MOVEL     FUV(6)        VFD006
109100000000     C     FAT(23)       IFEQ      'N'
109200000000     C     FAT(24)       ANDEQ     'N'
109300000000     C                   MOVEL     FUV(7)        VFD007
109400000000     C                   MOVEL     FUV(8)        VFD008
109500000000     C                   ELSE
109600000000     C     FAT(23)       IFEQ      'S'
109700000000     C     FAT(24)       ANDEQ     'S'
109800000000     C                   MOVEL     FDE(23)       VFD007
109900000000     C                   MOVEL     FDE(24)       VFD008
110000000000     C                   ELSE
110100000000     C                   MOVEL     FUV(7)        VFD007
110200000000     C     FAT(23)       IFEQ      'S'
110300000000     C                   MOVEL     FDE(23)       VFD008
110400000000     C                   ELSE
110500000000     C                   MOVEL     FDE(24)       VFD008
110600000000     C                   ENDIF
110700000000     C                   ENDIF
110800000000     C                   ENDIF
110900000000     C                   MOVEL     '1'           $TESVD
111000000000     C                   ENDSR
111100000000     C*------------------------------------------------------------------------*
111200000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
111300000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
111400000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
111500000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
111600000000     C*------------------------------------------------------------------------*
111700000000     C     CONFUN        BEGSR
111800000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
111900000000     C     1             DO        31            I
112000000000     C     KEY           IFEQ      KEYS(I)
112100000000     C* CONTROLLO SE IL TASTO E' ATTIVO
112200000000     C     FAT(I)        IFNE      'S'
112300000000     C                   MOVEL     MSG(1)        $MSG
112400000000     C                   EXSR      CARMSG
112500000000     C                   MOVEL     '1'           $ERR
112600000000     C                   ENDIF
112700000000     C                   ENDIF
112800000000     C                   ENDDO
112900000000     C                   ENDSR
113000000000     C*------------------------------------------------------------------------*
113100000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
113200000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
113300000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
113400000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
113500000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
113600000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
113700000000     C*------------------------------------------------------------------------*
113800000000     C     CAROPZ        BEGSR
113900000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
114000000000     C     1             DO        24            I
114100000000     C                   MOVEL     OPZ(I)        OPZRIG
114200000000     C                   MOVEL     OPZOPD        OPD(I)
114300000000     C                   MOVEL     OPZOPS        OPS(I)
114400000000     C                   MOVEL     OPZDES        ODE(I)
114500020923     C     DUTLPO        IFEQ      'S'
114600000000     C     $TGES         IFEQ      'G'
114700000000     C                   MOVEL     OPZASG        OAT(I)
114800000000     C                   ELSE
114900000000     C                   MOVEL     OPZASI        OAT(I)
115000000000     C                   ENDIF
115100000000     C                   ELSE
115200000000     C     $TGES         IFEQ      'G'
115300000000     C                   MOVEL     OPZAFG        OAT(I)
115400000000     C                   ELSE
115500000000     C                   MOVEL     OPZAFI        OAT(I)
115600000000     C                   ENDIF
115700000000     C                   ENDIF
115800000000     C     OAT(I)        IFEQ      'E'
115900000000     C                   MOVEL     USER          WEDP              3
116000000000     C     WEDP          IFEQ      'EDP'
116100000000     C                   MOVEL     'S'           OAT(I)
116200000000     C                   ELSE
116300000000     C                   MOVEL     'N'           OAT(I)
116400000000     C                   ENDIF
116500000000     C                   ENDIF
116600000000     C                   ENDDO
116700000000     C                   ENDSR
116800000000     C*------------------------------------------------------------------------*
116900000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
117000000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
117100000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
117200000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
117300000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
117400000000     C*------------------------------------------------------------------------*
117500000000     C     OUTOPZ        BEGSR
117600000000     C     1             DO        8             I
117700000000     C                   MOVEL     *BLANKS       OPV(I)
117800000000     C                   ENDDO
117900000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
118000000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
118100000000     C                   ADD       1             OPZULT
118200000000     C     OPZULT        IFGT      24
118300000000     C                   Z-ADD     1             OPZULT
118400000000     C                   END
118500000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
118600000000     C     1             DO        24            I
118700000000     C     OAT(I)        IFEQ      'S'
118800000000     C     ODE(I)        ANDNE     *BLANKS
118900000000     C                   ADD       1             OPZNAT
119000000000     C                   ENDIF
119100000000     C                   ENDDO
119200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
119300000000     C                   Z-ADD     0             W
119400000000     C     OPZULT        DO        24            I
119500000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
119600000000     C     W             IFLT      8
119700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
119800000000     C     OAT(I)        IFEQ      'S'
119900000000     C     ODE(I)        ANDNE     *BLANKS
120000000000     C                   ADD       1             W
120100000000     C                   MOVEL     ODE(I)        OPV(W)
120200000000     C                   ENDIF
120300000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
120400000000     C                   Z-ADD     I             OPZULT
120500000000     C                   ENDIF
120600000000     C                   ENDDO
120700000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
120800000000     C                   MOVEL     OPV(1)        VOD001
120900000000     C                   MOVEL     OPV(2)        VOD002
121000000000     C                   MOVEL     OPV(3)        VOD003
121100000000     C                   MOVEL     OPV(4)        VOD004
121200000000     C                   MOVEL     OPV(5)        VOD005
121300000000     C                   MOVEL     OPV(6)        VOD006
121400000000     C                   MOVEL     OPV(7)        VOD007
121500000000     C                   MOVEL     OPV(8)        VOD008
121600000000     C                   MOVEL     '1'           $TESVD
121700000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
121800000000     C     OPZNAT        IFGT      8
121900000000     C                   MOVEL     'S'           FAT(23)
122000000000     C                   ENDIF
122100000000     C                   ENDSR
122200000000     C*------------------------------------------------------------------------*
122300000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
122400000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
122500000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
122600000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
122700000000     C*------------------------------------------------------------------------*
122800000000     C     CONOPZ        BEGSR
122900000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
123000000000     C                   MOVEL     *BLANKS       OPZOK             1
123100000000     C     1             DO        24            I                 5 0
123200000000     C     VSCOPZ        IFEQ      OPD(I)
123300000000     C     VSCOPZ        OREQ      OPS(I)
123400000000     C     OAT(I)        IFEQ      'S'
123500000000     C                   MOVEL     OPD(I)        VSCOPZ
123600000000     C                   MOVEL     'S'           OPZOK
123700000000     C                   ELSE
123800000000     C                   MOVEL     'N'           OPZOK             1
123900000000     C                   ENDIF
124000000000     C                   ENDIF
124100000000     C                   ENDDO
124200000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
124300000000     C     OPZOK         IFNE      'S'
124400000000     C                   MOVEL     MSG(3)        $MSG
124500000000     C                   EXSR      CARMSG
124600000000     C                   MOVEL     '1'           $ERR
124700000000     C                   MOVEL     'N'           OPZOK             1
124800000000     C                   ENDIF
124900000000     C                   ENDSR
125000000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
125100000000TREC66R- Tasto funzionale non valido.                                          1
125200000000TREC66R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
125300000000TREC66R- Opzione non valida.                                                   3
125400000000TREC66R- Opzione abilitata ma non gestita. Avvertire CED.                      4
125500000000TREC66R- Inizio elenco.                                                        5
125600000000TREC66R- Fine elenco.                                                          6
125700000000TREC66R- 7
125800000000TREC66R- 8
125900000000TREC66R- 9
126000000000TREC66R- 10
126100000000TREC66R- 11
126200000000TREC66R- Codice voce annullato, consentito solo il ripristino                 12
126300000000TREC66R- Codice voce non annullato, impossibile ripristinare                  13
126400000000TREC66R- 14
126500000000TREC66R- 15
126600000000TREC66R- 16
126700000000TREC66R- 17
126800000000TREC66R- 18
126900000000TREC66R- 19
127000000000TREC66R- 20
127100000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
127200000000- 1-1 -                   -    NN     NN
127300000000- 2-2 -2=Modifica         -    SN     NN
127400000000- 3-3 -                   -    NN     NN
127500000000- 4-4 -4=Cancellazione    -    SN     NN
127600000000- 5-5 -5=Visualizzazione  -    SS     SS
127700000000- 6-6 -                   -    NN     NN
127800000000- 7-7 -7=Ripristino       -    SN     NN
127900000000- 8-8 -                   -    NN     NN
128000000000- 9-9 -                   -    NN     NN
128100000000-10-  -                   -    NN     NN
128200000000-11-  -                   -    NN     NN
128300000000-12-  -                   -    NN     NN
128400000000-13-  -                   -    NN     NN
128500000000-14-  -                   -    NN     NN
128600000000-15-  -                   -    NN     NN
128700000000-16-  -                   -    NN     NN
128800000000-17-  -                   -    NN     NN
128900000000-18-  -                   -    NN     NN
129000000000-19-  -                   -    NN     NN
129100000000-20-  -                   -    NN     NN
129200000000-21-  -                   -    NN     NN
129300000000-22-  -                   -    NN     NN
129400000000-23-  -                   -    NN     NN
129500000000-24-  -                   -    NN     NN
129600000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
129700000000F1    -                   -    NN     NN
129800000000F2    -                   -    NN     NN
129900000000F3    -F3=Fine            -    SS     SS
130000000000F4    -                   -    NN     NN
130100000000F5    -F5=Rivisualizza    -    SS     SS
130200000000F6    -F6=Inserimento     -    SN     NN
130300000000F7    -                   -    NN     NN
130400000000F8    -                   -    NN     NN
130500000000F9    -                   -    NN     NN
130600000000F10   -                   -    NN     NN
130700000000F11   -                   -    NN     NN
130800000000F12   -F12=Ritorno        -    SS     SS
130900000000F13   -                   -    NN     NN
131000000000F14   -                   -    NN     NN
131100000000F15   -                   -    NN     NN
131200000000F16   -                   -    NN     NN
131300961003F17   -F17=Inizio dati    -    NN     NN
131400961003F18   -F18=Fine dati      -    NN     NN
131500000000F19   -                   -    NN     NN
131600000000F20   -                   -    NN     NN
131700000000F21   -                   -    NN     NN
131800000000F22   -                   -    NN     NN
131900000000F23   -F23=Altre opzioni  -    NN     NN
132000000000F24   -F24=Altri tasti    -    NN     NN
132100000000ENTER -                   -    SS     SS
132200000000ROLLUP-                   -    SS     SS
132300000000ROLLDN-                   -    SS     SS
132400000000HELP  -                   -    NN     NN
132500000000PRINT -                   -    NN     NN
132600000000BSPACE-                   -    NN     NN
132700000000CLEAR -                   -    NN     NN
