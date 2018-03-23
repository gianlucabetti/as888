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
032400130910IF  1C     VCCvod        ORNE      DEPvod
032500000000     C*
032600000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
032700000000     C                   EXSR      CHKPAR
032800000000     C*
032900000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
033000000000IF  2C     $ERR          IFEQ      '0'
033100000000     C                   Z-ADD     1             SWPOSI
033200000000     C                   Z-ADD     VCCVOM        DEPVOM
033300000000     C                   Z-ADD     VCCPRM        DEPPRM
033400130910     c                   movel     vccvod        depvod
033500000000E   2C                   ENDIF
033600000000E   1C                   ENDIF
033700000000     C*
033800000000     C                   ENDSR
033900000000     C*------------------------------------------------------------------------*
034000000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
034100000000     C*------------------------------------------------------------------------*
034200000000     C     CHKPAR        BEGSR
034300000000     C*
034400000000     C* REIMPOSTO INDICATORI DI ERRORE
034500000000     C                   SETOFF                                       80
034600000000     C*--------------------
034700000000     C*
034800000000     C*--------------------
034900000000     C*
035000000000     C*
035100000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
035200000000     C* SUL CAMPO DEL CONTROL
035300000000IF  1C     $ERR          IFEQ      '1'
035400000000     C                   MOVEL     '0'           *IN43
035500000000X   1C                   ELSE
035600000000     C                   MOVEL     '1'           *IN43
035700000000E   1C                   ENDIF
035800000000     C*
035900000000     C                   ENDSR
036000000000     C*------------------------------------------------------------------------*
036100000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD VOCE DI C/E
036200000000     C*------------------------------------------------------------------------*
036300000000     C     SETFIL        BEGSR
036400000000     C*
036500000000     C                   MOVEL     '1'           *IN43
036600000000     C*
036700000000     C* POSIZIONAMENTO ARCHIVIO
036800000000     C                   Z-ADD     VCCVOM        KVTVOM
036900000000     C                   Z-ADD     VCCPRM        KVTPRM
037000000000     C     KEYEVM        SETLL     ECEVM01L                           99
037100000000     C*
037200000000     C  N99              EXSR      LETFIL
037300000000     C*
037400000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
037500000000IF  1C     *IN99         IFEQ      '1'
037600000000     C                   Z-ADD     1             SFLEOF
037700000000E   1C                   ENDIF
037800000000     C*
037900000000     C                   ENDSR
038000000000     C*------------------------------------------------------------------------*
038100000000     C* LETFIL - LEGGE RECORD SUCESSIVO VOCE DI C/E
038200000000     C*------------------------------------------------------------------------*
038300000000     C     LETFIL        BEGSR
038400000000     C*
038500000000     C* LEGGO FINO A:
038600000000     C*     - FINE FILE
038700000000     C*     - TROVATO RECORD VALIDO
038800000000     C                   MOVEL     '0'           WRECOK            1
038900000000DO  1C     *IN99         DOWEQ     *OFF
039000000000     C     WRECOK        ANDEQ     '0'
039100000000     C*
039200000000     C* LETTURA
039300000000     C                   READ      ECEVM01L                               99
039400000000     C*
039500000000     C* FINE FILE  O TABELLA DIVERSA
039600000000IF  2C     *IN99         IFEQ      '1'
039700000000     C                   Z-ADD     1             SFLEOF
039800000000X   2C                   ELSE
039900000000     C                   EXSR      CHKREC
040000000000E   2C                   ENDIF
040100000000     C*
040200000000E   1C                   ENDDO
040300000000     C*
040400000000     C                   ENDSR
040500000000     C*------------------------------------------------------------------------*
040600000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
040700000000     C*------------------------------------------------------------------------*
040800000000     C     CHKREC        BEGSR
040900000000     C*
041000000000     C                   MOVEL     '1'           WRECOK
041100130910     C* VERIFICO SE RICHIESTa LA descrizione
041200130910     c                   if        vccvod <> ' '
041300130910     C                   Z-ADD     EVMVOM        KVTVOM
041400130910     C                   Z-ADD     EVMPRM        KVTPRM
041500130910     c                   clear                   evtvod
041600130910     C     KEYEVM        CHAIN     ECEVT01L
041700130910     c                   z-add     0             pos               2 0
041800130910     C                   eval      pos = %scan(%trim(vccvod): evtvod)
041900130910     c                   if        pos =  0
042000130910     C                   MOVEL     '0'           WRECOK
042100130910     C                   ENDIF
042200130910     C                   ENDIF
042300000000     C*
042400000000     C                   ENDSR
042500000000     C*------------------------------------------------------------------------*
042600000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
042700000000     C*------------------------------------------------------------------------*
042800000000     C     IRESFL        BEGSR
042900000000     C*
043000000000IF  1C     EVMATB        IFNE      *BLANKS
043100000000     C                   MOVEL     'A'           VSCATB
043200000000X   1C                   ELSE
043300000000     C                   MOVEL     ' '           VSCATB
043400000000E   1C                   ENDIF
043500000000     C*
043600000000     C                   Z-ADD     EVMVOM        VSCVOM
043700000000     C                   Z-ADD     EVMPRM        VSCPRM
043800000000     C                   Z-ADD     EVMVOC        VSCVOC
043900000000     C                   Z-ADD     EVMPRG        VSCPRG
044000100326     C                   Z-ADD     EVMViso       VSCVsal
044100000000     C*
044200000000     C*  DECODIFICO LE DUE VOCI
044300000000     C                   Z-ADD     EVMVOM        KVTVOM
044400000000     C                   Z-ADD     EVMPRM        KVTPRM
044500000000     C     KEYEVM        CHAIN     ECEVT01L                           98
044600000000     C  N98              MOVEL     EVTVOD        VSCVDM
044700000000     C   98              MOVE      *BLANKS       VSCVDM
044800000000     C*
044900000000     C                   Z-ADD     EVMVOC        KVTVOM
045000000000     C                   Z-ADD     EVMPRG        KVTPRM
045100000000     C     KEYEVM        CHAIN     ECEVT01L                           98
045200000000     C  N98              MOVEL     EVTVOD        VSCVDC
045300000000     C   98              MOVE      *BLANKS       VSCVDC
045400000000     C*
045500000000     C                   ENDSR
045600000000     C*------------------------------------------------------------------------*
045700000000     C* LANPGM - LANCIA UN PROGRAMMA
045800000000     C*------------------------------------------------------------------------*
045900000000     C     LANPGM        BEGSR
046000000000     C*--------------------
046100000000     C* PARAMETRI INFORMATIVI
046200000000     C*--------------------
046300000000     C                   MOVEL     WOP0          D60OP0
046400000000     C                   MOVEL     WOP1          D60OP1
046500000000     C                   MOVEL     '0'           D60F03
046600000000     C                   MOVEL     '0'           D60ERR
046700000000     C                   MOVEL     *BLANKS       D60MSG
046800000000     C*--------------------
046900000000     C* PARAMETRI
047000000000     C*--------------------
047100000000     C                   Z-ADD     WVOC          D60VOC
047200000000     C                   Z-ADD     WPRG          D60PRG
047300000000     C                   Z-ADD     WVOM          D60VOM
047400000000     C                   Z-ADD     WPRM          D60PRM
047500000000     C* LANCIO
047600000000     C                   MOVEL     DS60          KPJBU
047700000000     C                   CALL      WPGM                                 99
047800000000     C                   PARM                    KPJBA
047900000000     C                   MOVEL     KPJBU         DS60
048000000000     C*
048100000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
048200000000IF  1C     D60F03        IFEQ      '1'
048300000000     C                   MOVEL     '1'           $F03
048400000000     C                   MOVEL     '1'           $ERR
048500000000     C                   MOVEL     '1'           $FINE
048600000000     C*
048700000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
048800000000X   1C                   ELSE
048900000000     C                   MOVEL     D60MSG        $MSG
049000000000     C                   EXSR      CARMSG
049100000000E   1C                   ENDIF
049200000000     C*
049300000000     C                   ENDSR
049400020923     C*-------------------------------------------------------------------------
049500020923     C* REPERISCE I DATI UTENTE
049600020923     C*-------------------------------------------------------------------------
049700020923     C     REPDATIUTE    BEGSR
049800020923     C*
049900020923     C* INIZIALIZZA VARIABILI DI WRK
050000020923     C                   CLEAR                   TIBS34DS
050100020923     C                   CLEAR                   AZUTEDS
050200020923     C                   CLEAR                   DDATIUTE
050300020923     C*
050400020923     C     *DTAARA       DEFINE    §azute        azuteds
050500020923     C     *DTAARA       DEFINE    §datiute      ddatiute
050600020923     C                   IN(E)     *DTAARA
050700020923if  1C                   IF        %Error
050800020923     c                   EVAL      I34Tla = 'L'
050900020923     C                   CALL      'TIBS34R'
051000020923     C                   PARM                    Tibs34Ds
051100020923     C                   IN        *DTAARA
051200020923e   1C                   ENDIF
051300020923     C*
051400020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
051500020923     C                   MOVEL(P)  RSUT          DSFIRS           20
051600020923     C*
051700020923     C                   ENDSR
051800000000     C*------------------------------------------------------------------------*
051900000000     C*  *INZSR - ROUTINE INIZIALE
052000000000     C*------------------------------------------------------------------------*
052100000000     C     *INZSR        BEGSR
052200000000     C*
052300000000     C* RICEVIMENTO PARAMETRI
052400000000     C     *ENTRY        PLIST
052500000000     C                   PARM                    KPJBA
052600000000     C                   MOVEL     KPJBU         DS60
052700000000     C*
052800000000     C* DEFINIZIONE KLIST
052900000000     C     *LIKE         DEFINE    EVMVOM        KVTVOM
053000000000     C     *LIKE         DEFINE    EVMPRM        KVTPRM
053100000000     C     *LIKE         DEFINE    EVMVOC        KVTVOC
053200000000     C     *LIKE         DEFINE    EVMPRG        KVTPRG
053300000000     C*
053400000000     C     *LIKE         DEFINE    D60OP0        WOP0
053500000000     C     *LIKE         DEFINE    D60OP1        WOP1
053600000000     C     *LIKE         DEFINE    D60VOC        WVOC
053700000000     C     *LIKE         DEFINE    D60PRG        WPRG
053800000000     C     *LIKE         DEFINE    D60VOM        WVOM
053900000000     C     *LIKE         DEFINE    D60PRM        WPRM
054000000000     C*
054100000000     C* DEFINIZIONE DEPOSITI
054200000000     C     *LIKE         DEFINE    VCCVOM        DEPVOM
054300000000     C     *LIKE         DEFINE    VCCPRM        DEPPRM
054400130910     C     *LIKE         DEFINE    VCCvod        DEPvod
054500000000     C*
054600000000     C* CHIAVE LETTURA ECEVM01L - COMPLETA
054700000000     C     KEYEVM        KLIST
054800000000     C                   KFLD                    KVTVOM
054900000000     C                   KFLD                    KVTPRM
055000000000     C*
055100000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
055200000000     C                   MOVE      D60OP0        INTGES            2
055300000000IF  1C     INTGES        IFEQ      '02'
055400000000     C                   MOVEL     'G'           $TGES             1
055500000000     C                   MOVEL     DESGES        VTDOPE
055600000000X   1C                   ELSE
055700000000     C                   MOVEL     'I'           $TGES             1
055800000000     C                   MOVEL     DESINT        VTDOPE
055900000000E   1C                   ENDIF
056000000000     C*
056100000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
056200000000     C                   EXSR      INZSTD
056300000000     C*
056400000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
056500000000     C                   EXSR      INZSFL
056600000000     C*
056700000000     C* IMPOSTO CAMPI TESTATA VIDEO
056800000000     C                   MOVEL     DSFIRS        VTDRSU
056900000000     C*
057000000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
057100000000     C* SE RICHIAMATO DA ALTRO PGM IMPOSTO I CAMPI PASSATI
057200000000     C                   MOVEL     D60OP0        $UNO              1
057300130910     c                   clear                   vccvod
057400130910     c                   clear                   depvod
057500000000     C     $UNO          IFEQ      'M'
057600000000     C                   Z-ADD     *ZEROS        VCCVOM
057700000000     C                   Z-ADD     *ZEROS        DEPVOM
057800000000     C                   Z-ADD     *ZEROS        VCCPRM
057900000000     C                   Z-ADD     *ZEROS        DEPPRM
058000000000     C                   ELSE
058100000000     C                   Z-ADD     D60VOM        VCCVOM
058200000000     C                   Z-ADD     D60VOM        DEPVOM
058300000000     C                   Z-ADD     D60PRM        VCCPRM
058400000000     C                   Z-ADD     D60PRM        DEPPRM
058500000000     C                   ENDIF
058600000000     C*
058700000000     C* EMETTO VIDEATA SUBFILE
058800000000     C                   EXSR      CARVS
058900000000     C*
059000000000     C                   ENDSR
059100000000     C*------------------------------------------------------------------------*
059200000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
059300000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
059400000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
059500000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
059600000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
059700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
059800000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
059900000000     C*------------------------------------------------------------------------*
060000000000     C* INZSTD - ROUTINE INIZIALE STANDARD
060100000000     C*------------------------------------------------------------------------*
060200000000     C     INZSTD        BEGSR
060300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
060400000000     C                   CALL      'XKEYS'
060500000000     C                   PARM                    DSKEYS
060600000000     C                   MOVEA     KEYRIG        KEYS
060700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
060800000000     C     XMSGPA        PLIST
060900000000     C                   PARM                    DSMSMS
061000000000     C                   PARM                    DSMSPG
061100000000     C                   PARM                    DSMSKE
061200000000     C                   PARM                    DSMSER
061300000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
061400000000     C                   MOVEL     'TREC66R'     DSMSPG
061500000000     C                   MOVEL     'TREC66R'     VMCQUE
061600000000     C                   MOVEL     *BLANKS       VMCKEY
061700000000     C                   MOVEL     *BLANKS       DSMSMS
061800000000     C* INIZIALIZZO FLAG CONTROLLO PGM
061900000000     C                   MOVEL     '0'           $ERR              1
062000000000     C                   MOVEL     '0'           $F03              1
062100000000     C                   MOVEL     *BLANKS       $GES              2
062200000000     C                   MOVEL     *BLANKS       $MSG             80
062300000000     C                   MOVEL     '0'           $FINE             1
062400000000     C                   MOVEL     '0'           $TESVD            1
062500000000     C* LANCIO ROUTINE PER VERIFICARE SE
062600000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
062700000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
062800020923     C                   EXSR      REPDATIUTE
062900000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
063000000000     C                   BITOFF    '01234567'    POSMI1            1
063100000000     C                   BITOFF    '01234567'    POSMI2            1
063200000000     C                   BITOFF    '01234567'    POSMA1            1
063300000000     C                   BITOFF    '01234567'    POSMA2            1
063400000000     C                   BITON     '34'          POSMI1            1
063500000000     C                   BITON     '7'           POSMI2            1
063600000000     C                   BITON     '34'          POSMA1            1
063700000000     C                   BITON     '0'           POSMA2            1
063800000000     C                   MOVE      POSMI2        POSMIN            2
063900000000     C                   MOVEL     POSMI1        POSMIN
064000000000     C                   MOVE      POSMA2        POSMAX            2
064100000000     C                   MOVEL     POSMA1        POSMAX
064200000000     C                   ENDSR
064300000000     C*------------------------------------------------------------------------*
064400000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
064500000000     C*------------------------------------------------------------------------*
064600000000     C     FUNSTD        BEGSR
064700000000     C                   SELECT
064800000000     C     KEY           WHENEQ    F3
064900000000     C                   EXSR      F03STD
065000000000     C     KEY           WHENEQ    F12
065100000000     C                   EXSR      F12STD
065200000000     C     KEY           WHENEQ    F24
065300000000     C                   EXSR      F24STD
065400000000     C                   OTHER
065500000000     C                   MOVEL     MSG(2)        $MSG
065600000000     C                   EXSR      CARMSG
065700000000     C                   ENDSL
065800000000     C                   ENDSR
065900000000     C*------------------------------------------------------------------------*
066000000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
066100000000     C*------------------------------------------------------------------------*
066200000000     C     F03STD        BEGSR
066300000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
066400000000     C                   MOVEL     '1'           $F03
066500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
066600000000     C                   MOVEL     '1'           $FINE
066700000000     C                   ENDSR
066800000000     C*------------------------------------------------------------------------*
066900000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
067000000000     C*------------------------------------------------------------------------*
067100000000     C     F12STD        BEGSR
067200000000     C* IMPOSTO FLAG DI FIN PGM
067300000000     C                   MOVEL     '1'           $FINE
067400000000     C                   ENDSR
067500000000     C*------------------------------------------------------------------------*
067600000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
067700000000     C*------------------------------------------------------------------------*
067800000000     C     F24STD        BEGSR
067900000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
068000000000     C                   EXSR      OUTFUN
068100000000     C                   ENDSR
068200000000     C*------------------------------------------------------------------------*
068300000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
068400000000     C*------------------------------------------------------------------------*
068500000000     C     INZSFL        BEGSR
068600000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
068700000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
068800000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
068900000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
069000000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
069100000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
069200000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
069300000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
069400000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
069500000000     C                   Z-ADD     12            SFLPAG
069600000000     C                   ENDSR
069700000000     C*------------------------------------------------------------------------*
069800000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
069900000000     C*------------------------------------------------------------------------*
070000000000     C     FUNSFL        BEGSR
070100000000     C                   SELECT
070200000000     C     KEY           WHENEQ    ENTER
070300000000     C                   EXSR      ENTSFL
070400000000     C     KEY           WHENEQ    F3
070500000000     C                   EXSR      F03STD
070600000000     C     KEY           WHENEQ    F5
070700000000     C                   EXSR      F05STD
070800000000     C     KEY           WHENEQ    F12
070900000000     C                   EXSR      F12STD
071000000000     C     KEY           WHENEQ    F17
071100000000     C                   EXSR      F17STD
071200000000     C     KEY           WHENEQ    F18
071300000000     C                   EXSR      F18STD
071400000000     C     KEY           WHENEQ    F23
071500000000     C                   EXSR      F23STD
071600000000     C     KEY           WHENEQ    F24
071700000000     C                   EXSR      F24STD
071800000000     C     KEY           WHENEQ    ROLLUP
071900000000     C                   EXSR      RUPSTD
072000000000     C     KEY           WHENEQ    ROLLDN
072100000000     C                   EXSR      RDNSTD
072200000000     C                   OTHER
072300000000     C                   MOVEL     MSG(2)        $MSG
072400000000     C                   EXSR      CARMSG
072500000000     C                   ENDSL
072600000000     C                   ENDSR
072700000000     C*------------------------------------------------------------------------*
072800000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
072900000000     C*------------------------------------------------------------------------*
073000000000     C     F05STD        BEGSR
073100000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
073200000000     C                   EXSR      POSSFL
073300000000     C                   ENDSR
073400000000     C*------------------------------------------------------------------------*
073500000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
073600000000     C*------------------------------------------------------------------------*
073700000000     C     F17STD        BEGSR
073800000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
073900000000     C                   Z-ADD     1             VCCNBR
074000000000     C                   ENDSR
074100000000     C*------------------------------------------------------------------------*
074200000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
074300000000     C*------------------------------------------------------------------------*
074400000000     C     F18STD        BEGSR
074500000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
074600000000     C                   Z-ADD     SFLULT        VCCNBR
074700000000     C                   ENDSR
074800000000     C*------------------------------------------------------------------------*
074900000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
075000000000     C*------------------------------------------------------------------------*
075100000000     C     F23STD        BEGSR
075200000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
075300000000     C                   EXSR      OUTOPZ
075400000000     C                   ENDSR
075500000000     C*------------------------------------------------------------------------*
075600000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
075700000000     C*------------------------------------------------------------------------*
075800000000     C     RUPSTD        BEGSR
075900000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
076000000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
076100000000     C     CURSOR        IFGE      POSMIN
076200000000     C     CURSOR        ANDLE     POSMAX
076300000000     C                   ELSE
076400000000     C                   EXSR      CARSFL
076500000000     C                   ENDIF
076600000000     C                   ENDSR
076700000000     C*------------------------------------------------------------------------*
076800000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
076900000000     C*------------------------------------------------------------------------*
077000000000     C     RDNSTD        BEGSR
077100000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
077200000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
077300000000     C     CURSOR        IFGE      POSMIN
077400000000     C     CURSOR        ANDLE     POSMAX
077500000000     C                   ELSE
077600000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
077700000000     C                   MOVEL     '1'           $ERR
077800000000     C                   MOVEL     MSG(5)        $MSG
077900000000     C                   EXSR      CARMSG
078000000000     C                   ENDIF
078100000000     C                   ENDSR
078200000000     C*------------------------------------------------------------------------*
078300000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
078400000000     C*------------------------------------------------------------------------*
078500000000     C     ENTSFL        BEGSR
078600000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
078700000000     C                   EXSR      NEWPOS
078800000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
078900000000     C     $ERR          IFEQ      '0'
079000000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
079100000000     C     SWPOSI        IFNE      0
079200000000     C                   EXSR      POSSFL
079300000000     C                   ELSE
079400000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
079500000000     C     SFLULT        IFNE      0
079600000000     C                   EXSR      OPZSFL
079700000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
079800000000     C     SWMODI        IFNE      0
079900000000     C                   EXSR      POSSFL
080000000000     C                   ENDIF
080100000000     C                   ENDIF
080200000000     C                   ENDIF
080300000000     C                   ENDIF
080400000000     C                   ENDSR
080500000000     C*------------------------------------------------------------------------*
080600000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
080700000000     C*------------------------------------------------------------------------*
080800000000     C     POSSFL        BEGSR
080900000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
081000000000     C                   EXSR      CLRSFL
081100000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
081200000000     C                   EXSR      SETFIL
081300000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
081400000000     C     SFLEOF        IFEQ      0
081500000000     C                   EXSR      CARSFL
081600000000     C                   ENDIF
081700000000     C                   ENDSR
081800000000     C*------------------------------------------------------------------------*
081900000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
082000000000     C*------------------------------------------------------------------------*
082100000000     C     OPZSFL        BEGSR
082200000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
082300000000     C                   MOVEL     'S'           OPZOK             1
082400000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
082500000000     C                   Z-ADD     0             SFLVAR            5 0
082600000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
082700000000     C                   Z-ADD     0             SFLERR
082800000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
082900000000     C                   Z-ADD     SFLULT        WFLULT
083000000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
083100000000     C                   READC     EC66DVS                                49
083200000000     C     *IN49         DOWEQ     '0'
083300941121     C     $ERR          ANDEQ     '0'
083400000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
083500000000     C                   ADD       1             SFLVAR
083600000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
083700000000     C     OPZOK         IFEQ      'S'
083800000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
083900000000     C     VSCOPZ        IFNE      *BLANKS
084000000000     C                   EXSR      CONOPZ
084100000000     C     OPZOK         IFEQ      'S'
084200941121     C     $ERR          ANDEQ     '0'
084300000000     C                   EXSR      GESOPZ
084400000000     C                   ENDIF
084500000000     C                   ENDIF
084600000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
084700000000     C*            2. PULIZIA SCELTA IN SFL
084800000000     C     OPZOK         IFEQ      'S'
084900000000     C                   MOVEL     '0'           *IN41
085000000000     C                   MOVEL     *BLANKS       VSCOPZ
085100000000     C                   ELSE
085200000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
085300000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
085400000000     C                   MOVEL     '1'           *IN41
085500000000     C                   Z-ADD     SFLULT        SFLERR
085600000000     C                   ENDIF
085700000000     C                   ELSE
085800000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
085900000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
086000000000     C                   MOVEL     '1'           *IN41
086100000000     C                   ENDIF
086200000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
086300000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
086400000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
086500000000     C     OPZOK         IFNE      'S'
086600000000     C                   Z-ADD     SFLERR        VCCNBR
086700000000     C                   ELSE
086800000000     C                   Z-ADD     SFLULT        VCCNBR
086900000000     C                   ENDIF
087000000000     C* AGGIORNO RECORD SFL
087100000000     C                   UPDATE    EC66DVS
087200941121     C     $ERR          IFEQ      '0'
087300000000     C                   READC     EC66DVS                                49
087400941121     C                   ENDIF
087500000000     C                   ENDDO
087600000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
087700000000     C                   Z-ADD     WFLULT        SFLULT
087800000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
087900000000     C     SFLVAR        IFEQ      0
088000000000     C                   MOVEL     '1'           $FINE
088100000000     C                   ENDIF
088200000000     C                   ENDSR
088300000000     C*------------------------------------------------------------------------*
088400000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
088500000000     C*------------------------------------------------------------------------*
088600000000     C     CLRSFL        BEGSR
088700000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
088800000000     C* (0=NON FINE FILE  1=FINE FILE)
088900000000     C                   Z-ADD     0             SFLEOF
089000000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
089100000000     C                   Z-ADD     0             SFLULT
089200000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
089300000000     C                   MOVEL     '1'           *IN40
089400000000     C                   WRITE     EC66DVC
089500000000     C                   MOVEL     '0'           *IN40
089600000000     C                   ENDSR
089700000000     C*------------------------------------------------------------------------*
089800000000     C* CARSFL - CARICAMENTO SFL
089900000000     C*------------------------------------------------------------------------*
090000000000     C     CARSFL        BEGSR
090100000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
090200000000     C     SFLULT        IFEQ      0
090300000000     C                   EXSR      SETFIL
090400000000     C                   ENDIF
090500000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
090600000000     C                   Z-ADD     SFLULT        WFLULT
090700000000     C* CICLO:
090800000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
090900000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
091000000000     C                   Z-ADD     1             SFLRIG
091100000000     C     SFLRIG        DOWLE     SFLPAG
091200000000     C     SFLEOF        ANDEQ     0
091300000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
091400000000     C                   MOVEL     *BLANKS       VSCOPZ
091500000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
091600000000     C                   EXSR      IRESFL
091700000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
091800000000     C                   ADD       1             SFLULT
091900000000     C* SCRITTURA RECORD SFL
092000000000     C                   WRITE     EC66DVS
092100000000     C                   ADD       1             SFLRIG
092200000000     C* LETTURA FILE DA CARICARE
092300000000     C                   EXSR      LETFIL
092400000000     C                   ENDDO
092500000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
092600000000     C     WFLULT        IFNE      SFLULT
092700000000     C                   Z-ADD     WFLULT        VCCNBR
092800000000     C                   ADD       1             VCCNBR
092900000000     C                   ENDIF
093000000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
093100000000     C     SFLEOF        IFEQ      1
093200000000     C     SFLULT        ANDGT     *ZEROS
093300000000     C                   MOVEL     '1'           $ERR
093400000000     C                   MOVEL     MSG(6)        $MSG
093500000000     C                   EXSR      CARMSG
093600000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
093700000000     C     SFLULT        CHAIN     EC66DVS                            99
093800000000     C     *IN99         IFEQ      '0'
093900000000     C                   SETON                                        45
094000000000     C                   UPDATE    EC66DVS
094100000000     C                   SETOFF                                       45
094200000000     C                   ENDIF
094300000000     C                   ENDIF
094400000000     C                   ENDSR
094500000000     C*------------------------------------------------------------------------*
094600000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
094700000000     C*------------------------------------------------------------------------*
094800000000     C     OUTSFL        BEGSR
094900000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
095000000000     C     MSGULT        IFNE      *ZEROS
095100000000     C                   EXSR      OUTMSG
095200000000     C                   ENDIF
095300000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
095400000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
095500000000     C     SFLEOF        IFEQ      0
095600000000     C                   MOVEL     '0'           *IN42
095700000000     C                   ELSE
095800000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
095900000000     C                   MOVEL     '1'           *IN42
096000000000     C                   ENDIF
096100000000     C* EMISSIONE SUBFILE
096200000000     C     SFLULT        IFLE      0
096300000000     C*  --> CASO DI SFL VUOTO
096400000000     C                   MOVEL     '0'           *IN43
096500000000     C                   WRITE     EC66DVV
096600000000     C                   WRITE     EC66DVC
096700000000     C                   READ      EC66DVC                                99
096800000000     C                   ELSE
096900000000     C*  --> CASO DI SFL CON RECORDS
097000000000     C                   WRITE     EC66DVC
097100000000     C                   READ      EC66DVC                                99
097200000000     C                   ENDIF
097300000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
097400000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
097500000000     C                   Z-ADD     FRTSFL        VCCNBR
097600000000     C                   ENDSR
097700000000     C*------------------------------------------------------------------------*
097800000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
097900000000     C*          CODA MESSAGGI DEL PGM
098000000000     C*------------------------------------------------------------------------*
098100000000     C     CARMSG        BEGSR
098200000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
098300000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
098400000000     C     MSGULT        IFEQ      0
098500000000     C                   MOVEL     '1'           *IN40
098600000000     C                   WRITE     EC66DVK
098700000000     C                   MOVEL     '0'           *IN40
098800000000     C                   ENDIF
098900000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
099000000000     C                   MOVEL     $MSG          DSMSMS
099100000000     C                   CALL      'XMSG'        XMSGPA
099200000000     C     DSMSER        IFEQ      0
099300000000     C                   MOVEL     DSMSKE        VMCKEY
099400000000     C                   ENDIF
099500000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
099600000000     C                   ADD       1             MSGULT            4 0
099700000000     C                   WRITE     EC66DVM
099800000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
099900000000     C                   MOVEL     *BLANKS       $MSG
100000000000     C                   ENDSR
100100000000     C*------------------------------------------------------------------------*
100200000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
100300000000     C*------------------------------------------------------------------------*
100400000000     C     OUTMSG        BEGSR
100500000000     C* VISUALIZZAZIONE MESSAGGIO
100600000000     C                   MOVEL     '1'           *IN44
100700000000     C                   WRITE     EC66DVK
100800000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
100900000000     C                   Z-ADD     *ZEROS        MSGULT
101000000000     C                   ENDSR
101100000000     C*------------------------------------------------------------------------*
101200000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
101300000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
101400000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
101500000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
101600000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
101700000000     C*------------------------------------------------------------------------*
101800000000     C     CARFUN        BEGSR
101900000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
102000000000     C     1             DO        31            I                 5 0
102100000000     C                   MOVEL     FUN(I)        FUNRIG
102200000000     C                   MOVEL     FUNFUN        FFU(I)
102300000000     C                   MOVEL     FUNDES        FDE(I)
102400020923     C     DUTLPO        IFEQ      'S'
102500000000     C     $TGES         IFEQ      'G'
102600000000     C                   MOVEL     FUNASG        FAT(I)
102700000000     C                   ELSE
102800000000     C                   MOVEL     FUNASI        FAT(I)
102900000000     C                   ENDIF
103000000000     C                   ELSE
103100000000     C     $TGES         IFEQ      'G'
103200000000     C                   MOVEL     FUNAFG        FAT(I)
103300000000     C                   ELSE
103400000000     C                   MOVEL     FUNAFI        FAT(I)
103500000000     C                   ENDIF
103600000000     C                   ENDIF
103700000000     C     FAT(I)        IFEQ      'E'
103800000000     C                   MOVEL     USER          WEDP              3
103900000000     C     WEDP          IFEQ      'EDP'
104000000000     C                   MOVEL     'S'           FAT(I)
104100000000     C                   ELSE
104200000000     C                   MOVEL     'N'           FAT(I)
104300000000     C                   ENDIF
104400000000     C                   ENDIF
104500000000     C                   ENDDO
104600000000     C                   ENDSR
104700000000     C*------------------------------------------------------------------------*
104800000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
104900000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
105000000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
105100000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
105200000000     C*------------------------------------------------------------------------*
105300000000     C     OUTFUN        BEGSR
105400000000     C     1             DO        8             I
105500000000     C                   MOVEL     *BLANKS       FUV(I)
105600000000     C                   ENDDO
105700000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
105800000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
105900000000     C     1             DO        31            I
106000000000     C     FAT(I)        IFEQ      'S'
106100000000     C     FDE(I)        ANDNE     *BLANKS
106200000000     C                   ADD       1             FUNNAT
106300000000     C                   ENDIF
106400000000     C                   ENDDO
106500000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
106600000000     C     FUNNAT        IFGT      8
106700000000     C                   MOVEL     'S'           FAT(24)
106800000000     C                   ENDIF
106900000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
107000000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
107100000000     C                   Z-ADD     8             FUNVIS            5 0
107200000000     C     FAT(23)       IFEQ      'S'
107300000000     C                   SUB       1             FUNVIS
107400000000     C                   ENDIF
107500000000     C     FAT(24)       IFEQ      'S'
107600000000     C                   SUB       1             FUNVIS
107700000000     C                   ENDIF
107800000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
107900000000     C                   Z-ADD     *ZEROS        W                 5 0
108000000000     C                   ADD       1             FUNULT
108100000000     C     FUNULT        IFGT      31
108200000000     C                   Z-ADD     1             FUNULT
108300000000     C                   ENDIF
108400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
108500000000     C     FUNULT        DO        31            I
108600000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
108700000000     C     W             IFLT      FUNVIS
108800000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
108900000000     C     FAT(I)        IFEQ      'S'
109000000000     C     FDE(I)        ANDNE     *BLANKS
109100000000     C     I             IFEQ      23
109200000000     C     I             OREQ      24
109300000000     C                   ELSE
109400000000     C                   ADD       1             W
109500000000     C                   MOVEL     FDE(I)        FUV(W)
109600000000     C                   ENDIF
109700000000     C                   ENDIF
109800000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
109900000000     C                   Z-ADD     I             FUNULT
110000000000     C                   ENDIF
110100000000     C                   ENDDO
110200000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
110300000000     C                   MOVEL     FUV(1)        VFD001
110400000000     C                   MOVEL     FUV(2)        VFD002
110500000000     C                   MOVEL     FUV(3)        VFD003
110600000000     C                   MOVEL     FUV(4)        VFD004
110700000000     C                   MOVEL     FUV(5)        VFD005
110800000000     C                   MOVEL     FUV(6)        VFD006
110900000000     C     FAT(23)       IFEQ      'N'
111000000000     C     FAT(24)       ANDEQ     'N'
111100000000     C                   MOVEL     FUV(7)        VFD007
111200000000     C                   MOVEL     FUV(8)        VFD008
111300000000     C                   ELSE
111400000000     C     FAT(23)       IFEQ      'S'
111500000000     C     FAT(24)       ANDEQ     'S'
111600000000     C                   MOVEL     FDE(23)       VFD007
111700000000     C                   MOVEL     FDE(24)       VFD008
111800000000     C                   ELSE
111900000000     C                   MOVEL     FUV(7)        VFD007
112000000000     C     FAT(23)       IFEQ      'S'
112100000000     C                   MOVEL     FDE(23)       VFD008
112200000000     C                   ELSE
112300000000     C                   MOVEL     FDE(24)       VFD008
112400000000     C                   ENDIF
112500000000     C                   ENDIF
112600000000     C                   ENDIF
112700000000     C                   MOVEL     '1'           $TESVD
112800000000     C                   ENDSR
112900000000     C*------------------------------------------------------------------------*
113000000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
113100000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
113200000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
113300000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
113400000000     C*------------------------------------------------------------------------*
113500000000     C     CONFUN        BEGSR
113600000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
113700000000     C     1             DO        31            I
113800000000     C     KEY           IFEQ      KEYS(I)
113900000000     C* CONTROLLO SE IL TASTO E' ATTIVO
114000000000     C     FAT(I)        IFNE      'S'
114100000000     C                   MOVEL     MSG(1)        $MSG
114200000000     C                   EXSR      CARMSG
114300000000     C                   MOVEL     '1'           $ERR
114400000000     C                   ENDIF
114500000000     C                   ENDIF
114600000000     C                   ENDDO
114700000000     C                   ENDSR
114800000000     C*------------------------------------------------------------------------*
114900000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
115000000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
115100000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
115200000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
115300000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
115400000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
115500000000     C*------------------------------------------------------------------------*
115600000000     C     CAROPZ        BEGSR
115700000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
115800000000     C     1             DO        24            I
115900000000     C                   MOVEL     OPZ(I)        OPZRIG
116000000000     C                   MOVEL     OPZOPD        OPD(I)
116100000000     C                   MOVEL     OPZOPS        OPS(I)
116200000000     C                   MOVEL     OPZDES        ODE(I)
116300020923     C     DUTLPO        IFEQ      'S'
116400000000     C     $TGES         IFEQ      'G'
116500000000     C                   MOVEL     OPZASG        OAT(I)
116600000000     C                   ELSE
116700000000     C                   MOVEL     OPZASI        OAT(I)
116800000000     C                   ENDIF
116900000000     C                   ELSE
117000000000     C     $TGES         IFEQ      'G'
117100000000     C                   MOVEL     OPZAFG        OAT(I)
117200000000     C                   ELSE
117300000000     C                   MOVEL     OPZAFI        OAT(I)
117400000000     C                   ENDIF
117500000000     C                   ENDIF
117600000000     C     OAT(I)        IFEQ      'E'
117700000000     C                   MOVEL     USER          WEDP              3
117800000000     C     WEDP          IFEQ      'EDP'
117900000000     C                   MOVEL     'S'           OAT(I)
118000000000     C                   ELSE
118100000000     C                   MOVEL     'N'           OAT(I)
118200000000     C                   ENDIF
118300000000     C                   ENDIF
118400000000     C                   ENDDO
118500000000     C                   ENDSR
118600000000     C*------------------------------------------------------------------------*
118700000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
118800000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
118900000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
119000000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
119100000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
119200000000     C*------------------------------------------------------------------------*
119300000000     C     OUTOPZ        BEGSR
119400000000     C     1             DO        8             I
119500000000     C                   MOVEL     *BLANKS       OPV(I)
119600000000     C                   ENDDO
119700000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
119800000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
119900000000     C                   ADD       1             OPZULT
120000000000     C     OPZULT        IFGT      24
120100000000     C                   Z-ADD     1             OPZULT
120200000000     C                   END
120300000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
120400000000     C     1             DO        24            I
120500000000     C     OAT(I)        IFEQ      'S'
120600000000     C     ODE(I)        ANDNE     *BLANKS
120700000000     C                   ADD       1             OPZNAT
120800000000     C                   ENDIF
120900000000     C                   ENDDO
121000000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
121100000000     C                   Z-ADD     0             W
121200000000     C     OPZULT        DO        24            I
121300000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
121400000000     C     W             IFLT      8
121500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
121600000000     C     OAT(I)        IFEQ      'S'
121700000000     C     ODE(I)        ANDNE     *BLANKS
121800000000     C                   ADD       1             W
121900000000     C                   MOVEL     ODE(I)        OPV(W)
122000000000     C                   ENDIF
122100000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
122200000000     C                   Z-ADD     I             OPZULT
122300000000     C                   ENDIF
122400000000     C                   ENDDO
122500000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
122600000000     C                   MOVEL     OPV(1)        VOD001
122700000000     C                   MOVEL     OPV(2)        VOD002
122800000000     C                   MOVEL     OPV(3)        VOD003
122900000000     C                   MOVEL     OPV(4)        VOD004
123000000000     C                   MOVEL     OPV(5)        VOD005
123100000000     C                   MOVEL     OPV(6)        VOD006
123200000000     C                   MOVEL     OPV(7)        VOD007
123300000000     C                   MOVEL     OPV(8)        VOD008
123400000000     C                   MOVEL     '1'           $TESVD
123500000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
123600000000     C     OPZNAT        IFGT      8
123700000000     C                   MOVEL     'S'           FAT(23)
123800000000     C                   ENDIF
123900000000     C                   ENDSR
124000000000     C*------------------------------------------------------------------------*
124100000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
124200000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
124300000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
124400000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
124500000000     C*------------------------------------------------------------------------*
124600000000     C     CONOPZ        BEGSR
124700000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
124800000000     C                   MOVEL     *BLANKS       OPZOK             1
124900000000     C     1             DO        24            I                 5 0
125000000000     C     VSCOPZ        IFEQ      OPD(I)
125100000000     C     VSCOPZ        OREQ      OPS(I)
125200000000     C     OAT(I)        IFEQ      'S'
125300000000     C                   MOVEL     OPD(I)        VSCOPZ
125400000000     C                   MOVEL     'S'           OPZOK
125500000000     C                   ELSE
125600000000     C                   MOVEL     'N'           OPZOK             1
125700000000     C                   ENDIF
125800000000     C                   ENDIF
125900000000     C                   ENDDO
126000000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
126100000000     C     OPZOK         IFNE      'S'
126200000000     C                   MOVEL     MSG(3)        $MSG
126300000000     C                   EXSR      CARMSG
126400000000     C                   MOVEL     '1'           $ERR
126500000000     C                   MOVEL     'N'           OPZOK             1
126600000000     C                   ENDIF
126700000000     C                   ENDSR
126800000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
126900000000TREC66R- Tasto funzionale non valido.                                          1
127000000000TREC66R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
127100000000TREC66R- Opzione non valida.                                                   3
127200000000TREC66R- Opzione abilitata ma non gestita. Avvertire CED.                      4
127300000000TREC66R- Inizio elenco.                                                        5
127400000000TREC66R- Fine elenco.                                                          6
127500000000TREC66R- 7
127600000000TREC66R- 8
127700000000TREC66R- 9
127800000000TREC66R- 10
127900000000TREC66R- 11
128000000000TREC66R- Codice voce annullato, consentito solo il ripristino                 12
128100000000TREC66R- Codice voce non annullato, impossibile ripristinare                  13
128200000000TREC66R- 14
128300000000TREC66R- 15
128400000000TREC66R- 16
128500000000TREC66R- 17
128600000000TREC66R- 18
128700000000TREC66R- 19
128800000000TREC66R- 20
128900000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
129000000000- 1-1 -                   -    NN     NN
129100000000- 2-2 -2=Modifica         -    SN     NN
129200000000- 3-3 -                   -    NN     NN
129300000000- 4-4 -4=Cancellazione    -    SN     NN
129400000000- 5-5 -5=Visualizzazione  -    SS     SS
129500000000- 6-6 -                   -    NN     NN
129600000000- 7-7 -7=Ripristino       -    SN     NN
129700000000- 8-8 -                   -    NN     NN
129800000000- 9-9 -                   -    NN     NN
129900000000-10-  -                   -    NN     NN
130000000000-11-  -                   -    NN     NN
130100000000-12-  -                   -    NN     NN
130200000000-13-  -                   -    NN     NN
130300000000-14-  -                   -    NN     NN
130400000000-15-  -                   -    NN     NN
130500000000-16-  -                   -    NN     NN
130600000000-17-  -                   -    NN     NN
130700000000-18-  -                   -    NN     NN
130800000000-19-  -                   -    NN     NN
130900000000-20-  -                   -    NN     NN
131000000000-21-  -                   -    NN     NN
131100000000-22-  -                   -    NN     NN
131200000000-23-  -                   -    NN     NN
131300000000-24-  -                   -    NN     NN
131400000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
131500000000F1    -                   -    NN     NN
131600000000F2    -                   -    NN     NN
131700000000F3    -F3=Fine            -    SS     SS
131800000000F4    -                   -    NN     NN
131900000000F5    -F5=Rivisualizza    -    SS     SS
132000000000F6    -F6=Inserimento     -    SN     NN
132100000000F7    -                   -    NN     NN
132200000000F8    -                   -    NN     NN
132300000000F9    -                   -    NN     NN
132400000000F10   -                   -    NN     NN
132500000000F11   -                   -    NN     NN
132600000000F12   -F12=Ritorno        -    SS     SS
132700000000F13   -                   -    NN     NN
132800000000F14   -                   -    NN     NN
132900000000F15   -                   -    NN     NN
133000000000F16   -                   -    NN     NN
133100961003F17   -F17=Inizio dati    -    NN     NN
133200961003F18   -F18=Fine dati      -    NN     NN
133300000000F19   -                   -    NN     NN
133400000000F20   -                   -    NN     NN
133500000000F21   -                   -    NN     NN
133600000000F22   -                   -    NN     NN
133700000000F23   -F23=Altre opzioni  -    NN     NN
133800000000F24   -F24=Altri tasti    -    NN     NN
133900000000ENTER -                   -    SS     SS
134000000000ROLLUP-                   -    SS     SS
134100000000ROLLDN-                   -    SS     SS
134200000000HELP  -                   -    NN     NN
134300000000PRINT -                   -    NN     NN
134400000000BSPACE-                   -    NN     NN
134500000000CLEAR -                   -    NN     NN
