000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC61R
000300000000      *  Descrizione   :  Gestione voci di conto economico
000400000000      *                   Programma inserimento/modifica maggiorazione
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:  06 MAG 1994
000700000000      *------------------------------------------------------------------------*
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000000     F*------------------------------------------------------------------------*
001000000000     F* DATA BASE
001100000000     F*------------------------------------------------------------------------*
001200000000     FTREC61D   CF   E             WORKSTN INFDS(DEVDS1)
001300000000     F                                     SFILE(EC61DVM:MSGULT)
001400040128     FECECT01L  IF   E           K DISK    rename(ecect:ecect1)
001500040128     FECECT02L  IF   E           K DISK
001600140114     FECEVD06L  UF   E           K DISK
001700000000     FECEVM01L  UF   E           K DISK
001800000000     FECEVT01L  UF A E           K DISK
001900130924     FECnot01L  UF A E           K DISK
002000000000     FTABEL00F  IF   E           K DISK
002100000000     D*------------------------------------------------------------------------*
002200000000     D* SCHIERE
002300000000     D*------------------------------------------------------------------------*
002400030423     D MSG             S             78    DIM(50) CTDATA PERRCD(1)
002500000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
002600000000     D*-------------------
002700000000     D* DECODIFICA OPZIONE RICHIAMATA
002800000000     D*-------------------
002900981015     D DOPZ            S             16    DIM(10) CTDATA PERRCD(1)
003000000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003100000000     D FFU             S              6    DIM(31)
003200000000     D FDE             S             19    DIM(31)
003300000000     D FAT             S              1    DIM(31)
003400000000     D* TABELLA DESCRIZIONI A VIDEO
003500000000     D FUV             S             19    DIM(8)
003600000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003700000000     D KEYS            S              1    DIM(40)
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
007600000000     D*---------------------
007700000000     D* DS ESTERNA DI PROCEDURA
007800000000     D*---------------------
007900000000     D DS60          E DS                  EXTNAME(TREC60DS)
008000010112     D DS61          E DS                  EXTNAME(TREC61DS)
008100000000     D*-------------------
008200000000     D* ARCHITETTURA
008300000000     D*-------------------
008400000000     D KPJBA         E DS
008500000000     D*------------------
008600000000     D* CALL XSRDA8 - CONTROLLO DATE
008700000000     D*------------------
008800000000     D WLBDAT          DS                  INZ
008900000000     D  G02DAT                 1      8  0
009000000000     D  G02INV                 9     16  0
009100000000     D  G02ERR                17     17
009200000000     D  G02TGI                18     22  0
009300021209     D*------------------
009400021209     D* DS GESTIONE RICERCA CONTROLLO TABELLE
009500021209     D*------------------
009600021209     D TIBS02DS      E DS
009700021209     D*------------------
009800021209     D* DS LETTURA TABELLA CO1
009900021209     D*------------------
010000021209     D DCO1          E DS
010100000000     D*---------------------
010200030422     D* DS x campo filler evtxxx
010300000000     D*---------------------
010400030422     D devtxxx       E DS
010500030422     D*---------------------
010600030422     D* DS DECODIFICHE CAMPI DELLA VOCE
010700030422     D*---------------------
010800030422     D DS9B          E DS
010900100421     D DS2o          E DS
011000100421     D DS2R          E DS
011100000000     D DS2Q          E DS
011200000000     D  FLAGS                 31     33
011300000000     C*------------------------------------------------------------------------*
011400000000     C* MAIN LINE
011500000000     C*------------------------------------------------------------------------*
011600000000     C*
011700000000     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
011800000000IF  1C     $FINE         IFEQ      '1'
011900000000     C*
012000000000     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
012100000000     C                   MOVEL     '1'           D60ERR
012200000000     C*
012300000000     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
012400000000X   1C                   ELSE
012500000000     C*
012600000000     C* CICLO FINO A CHE:
012700000000DO  2C     $FINE         DOWEQ     '0'
012800000000     C                   MOVEL     '0'           $ERR
012900000000     C*
013000000000     C* CONTROLLA GESTIONE VIDEO
013100000000IF  3C     $GES          IFEQ      'V1'
013200000000     C                   EXSR      GESV1
013300000000E   3C                   ENDIF
013400000000     C*
013500000000E   2C                   ENDDO
013600000000E   1C                   ENDIF
013700000000     C*
013800000000IF  1C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
013900000000     C     $F03          IFEQ      '1'
014000000000     C                   MOVEL     '1'           D60F03
014100000000E   1C                   ENDIF
014200000000     C* FINE PGM
014300000000     C                   MOVEL     DS60          KPJBU
014400000000     C                   SETON                                        LR
014500000000     C*------------------------------------------------------------------------*
014600000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
014700000000     C*------------------------------------------------------------------------*
014800000000     C     CARV1         BEGSR
014900000000     C*
015000000000     C                   MOVEL     'V1'          $GES
015100000000     C*
015200000000     C                   EXSR      CARFUN
015300000000     C                   EXSR      OUTFUN
015400000000     C*
015500000000     C                   WRITE     EC61DVT
015600000000     C                   WRITE     EC61DVF
015700000000     C*
015800000000     C                   ENDSR
015900000000     C*------------------------------------------------------------------------*
016000000000     C* GESV1  - GESTIONE PRIMO VIDEO
016100000000     C*------------------------------------------------------------------------*
016200000000     C     GESV1         BEGSR
016300000000     C*
016400000000IF  1C     $TESVD        IFEQ      '1'
016500000000     C                   EXSR      CARV1
016600000000E   1C                   ENDIF
016700000000     C*
016800000000IF  1C     MSGULT        IFNE      *ZEROS
016900000000     C                   EXSR      OUTMSG
017000000000E   1C                   ENDIF
017100000000     C*
017200000000     C                   EXFMT     EC61DV1
017300000000     C*
017400000000     C                   EXSR      CONFUN
017500000000     C*
017600000000IF  1C     $ERR          IFEQ      '0'
017700000000SL  2C                   SELECT
017800000000     C     KEY           WHENEQ    ENTER
017900000000     C                   EXSR      ENTV1
018000000000     C     KEY           WHENEQ    F6
018100000000     C                   EXSR      F06GES
018200000000     C     KEY           WHENEQ    F8
018300000000     C                   EXSR      F08GES
018400000000     C     KEY           WHENEQ    F9
018500000000     C                   EXSR      F09GES
018600000000     C     KEY           WHENEQ    F10
018700000000     C                   EXSR      F10GES
018800000000     C                   OTHER
018900000000     C                   EXSR      FUNSTD
019000000000E   2C                   ENDSL
019100000000E   1C                   ENDIF
019200000000     C*
019300000000     C                   ENDSR
019400000000     C*------------------------------------------------------------------------*
019500000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
019600000000     C*------------------------------------------------------------------------*
019700000000     C     ENTV1         BEGSR
019800000000     C*
019900000000     C* CONTROLLA I CAMPI IMMESSI
020000000000     C                   EXSR      DECODI
020100000000     C*
020200000000     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
020300000000IF  1C     D60OP1        IFEQ      'O05'
020400000000     C                   MOVEL     '1'           $FINE
020500000000E   1C                   ENDIF
020600000000     C*
020700000000     C                   ENDSR
020800000000     C*------------------------------------------------------------------------*
020900000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> CONFERMA OPERAZIONE
021000000000     C*------------------------------------------------------------------------*
021100000000     C     F06GES        BEGSR
021200000000     C*
021300000000     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
021400000000IF  1C     D60OP1        IFEQ      'F06'
021500000000     C                   EXSR      INSERI
021600000000E   1C                   ENDIF
021700000000IF  1C     D60OP1        IFEQ      'O02'
021800000000     C                   EXSR      MODIFI
021900000000E   1C                   ENDIF
022000010110IF  1C     D60OP1        IFEQ      'O03'
022100010110     C                   EXSR      DUPLIC
022200010110E   1C                   ENDIF
022300000000IF  1C     D60OP1        IFEQ      'O04'
022400000000     C                   EXSR      ANNULL
022500000000E   1C                   ENDIF
022600000000IF  1C     D60OP1        IFEQ      'O07'
022700000000     C                   EXSR      RIPRIS
022800000000E   1C                   ENDIF
022900010110     C*
023000010110     C* VERIFICO IL FLAG DI ERRORE ED INMPOSTO AD IL VALORE DI RITORNO
023100010110     C     $ERR          IFEQ      '0'
023200010115     C     D60ERR        ANDNE     '2'
023300010110     C                   MOVEL     '0'           D60ERR
023400010110     C                   ENDIF
023500000000     C*
023600000000     C                   ENDSR
023700000000     C*------------------------------------------------------------------------*
023800000000     C* F08GES -GEST.TASTO FUNZ. F8 ===> GESTIONE TARIFFE VOCI DI C/E
023900000000     C*------------------------------------------------------------------------*
024000000000     C     F08GES        BEGSR
024100000000     C*
024200000000     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
024300000000     C                   Z-ADD     V1CVOC        WVOC
024400000000     C                   Z-ADD     V1CPRG        WPRG
024500000000     C                   Z-ADD     *ZERO         WDDV
024600000000     C                   MOVE      *BLANKS       WTTV
024700000000     C                   Z-ADD     *ZERO         WFT1
024800000000     C                   Z-ADD     *ZERO         WFT2
024900000000     C                   Z-ADD     *ZEROS        WCLI
025000000000     C                   Z-ADD     *ZERO         WCTR
025100000000     C                   MOVE      *BLANKS       WCTS
025200131002     C                   MOVE      *BLANKS       Wnaz
025300131002     C                   MOVE      *BLANKS       WCad
025400000000     C                   Z-ADD     *ZERO         WCAP
025500000000     C                   Z-ADD     V1CDDV        WDDT
025600000000     C                   Z-ADD     *ZEROS        WSGI
025700000000     C                   Z-ADD     *ZERO         WVOM
025800000000     C                   Z-ADD     *ZERO         WPRM
025900000000     C                   Z-ADD     *ZERO         WCMP
026000000000     C                   Z-ADD     *ZERO         WNBC
026100000000     C                   Z-ADD     *ZERO         WDDC
026200000000     C                   Z-ADD     *ZERO         WKCC
026300000000     C                   Z-ADD     *ZERO         WKSC
026400000000     C                   MOVE      *BLANKS       WRIC
026500000000     C                   Z-ADD     *ZERO         WDDO
026600940719     C                   MOVE      *BLANKS       WFRA
026700940725     C                   MOVE      *BLANKS       WIOE
026800940719     C                   Z-ADD     *ZERO         WCCF
026900000000     C*
027000000000     C                   MOVEL     D60OP0        WOP0
027100000000     C                   MOVEL     D60OP1        WOP1
027200000000     C                   MOVEL     'TREC63R'     WPGM              8
027300000000     C                   EXSR      LANPGM
027400000000     C*
027500000000     C                   ENDSR
027600000000     C*------------------------------------------------------------------------*
027700000000     C* F09GES -GEST.TASTO FUNZ. F9 ===> GESTIONE COMPETENZE VOCI DI C/E
027800000000     C*------------------------------------------------------------------------*
027900000000     C     F09GES        BEGSR
028000000000     C*
028100000000     C* LANCIO LA GESTIONE DELLE COMPETENZE VOCI DI C/E
028200000000     C                   Z-ADD     *ZERO         WVOC
028300000000     C                   Z-ADD     *ZERO         WPRG
028400000000     C                   Z-ADD     *ZERO         WDDV
028500000000     C                   MOVE      *BLANKS       WTTV
028600000000     C                   Z-ADD     *ZERO         WFT1
028700000000     C                   Z-ADD     *ZERO         WFT2
028800000000     C                   Z-ADD     *ZEROS        WCLI
028900000000     C                   Z-ADD     *ZERO         WCTR
029000000000     C                   MOVE      *BLANKS       WCTS
029100131002     C                   MOVE      *BLANKS       Wnaz
029200131002     C                   MOVE      *BLANKS       WCad
029300000000     C                   Z-ADD     *ZERO         WCAP
029400000000     C                   Z-ADD     *ZEROS        WDDT
029500000000     C                   Z-ADD     *ZEROS        WSGI
029600000000     C                   Z-ADD     *ZERO         WVOM
029700000000     C                   Z-ADD     *ZERO         WPRM
029800000000     C                   Z-ADD     *ZERO         WCMP
029900000000     C                   Z-ADD     *ZERO         WNBC
030000000000     C                   Z-ADD     *ZERO         WDDC
030100000000     C                   Z-ADD     *ZERO         WKCC
030200000000     C                   Z-ADD     *ZERO         WKSC
030300000000     C                   MOVE      *BLANKS       WRIC
030400000000     C                   Z-ADD     *ZERO         WDDO
030500940719     C                   MOVE      *BLANKS       WFRA
030600940725     C                   MOVE      *BLANKS       WIOE
030700940719     C                   Z-ADD     *ZERO         WCCF
030800000000     C*
030900000000     C                   MOVEL     D60OP0        WOP0
031000000000     C                   MOVEL     D60OP1        WOP1
031100000000     C                   MOVEL     'TREC72R'     WPGM              8
031200000000     C                   EXSR      LANPGM
031300000000     C*
031400000000     C                   ENDSR
031500000000     C*------------------------------------------------------------------------*
031600000000     C* F10GES -GEST.TASTO FUNZ. F10 ===> GESTIONE MAGGIORAZIONI VOCI C/E
031700000000     C*------------------------------------------------------------------------*
031800000000     C     F10GES        BEGSR
031900000000     C*
032000000000     C* F10=MAGGIORAZIONI SOLO SE DI TIPO 'M'=MAGGIORAZIONI
032100000000IF  2C     V1CTVO        IFNE      'M'
032200000000     C                   MOVEL     '1'           $ERR
032300000000     C                   MOVEL     MSG(14)       $MSG
032400000000     C                   EXSR      CARMSG
032500000000E   2C                   ENDIF
032600000000     C*
032700000000     C* LANCIO LA GESTIONE DELLE MAGGIORAZIONI
032800000000IF  1C     $ERR          IFEQ      '0'
032900000000     C                   Z-ADD     *ZERO         WVOC
033000000000     C                   Z-ADD     *ZERO         WPRG
033100000000     C                   Z-ADD     V1CVOC        WVOM
033200000000     C                   Z-ADD     V1CPRG        WPRM
033300000000     C*
033400000000     C                   MOVEL     D60OP0        WOP0
033500000000     C                   MOVEL     D60OP1        WOP1
033600000000     C                   MOVEL     'TREC66R'     WPGM              8
033700000000     C                   EXSR      LANPGM
033800000000E   1C                   ENDIF
033900000000     C*
034000000000     C                   ENDSR
034100000000     C*------------------------------------------------------------------------*
034200000000     C* INSERI - FUNZIONE DI INSERIMENTO
034300000000     C*------------------------------------------------------------------------*
034400000000     C     INSERI        BEGSR
034500000000     C*
034600000000     C* CONTROLLA I CAMPI IMMESSI
034700000000     C                   EXSR      DECODI
034800000000     C*
034900000000IF  1C     $ERR          IFEQ      '0'
035000000000     C*
035100000000     C                   Z-ADD     V1CVOC        EVTVOC
035200000000     C                   Z-ADD     V1CPRG        EVTPRG
035300000000     C                   MOVEL     V1CVOD        EVTVOD
035400000000     C                   MOVEL     V1CTVO        EVTTVO
035500000000     C                   MOVEL     V1CVRV        EVTVRV
035600000000     C                   Z-ADD     V1CDDV        G02DAT
035700000000     C                   Z-ADD     *ZEROS        G02INV
035800000000     C                   MOVEL     '0'           G02ERR
035900000000     C                   CALL      'XSRDA8'
036000000000     C                   PARM                    WLBDAT
036100000000     C                   Z-ADD     G02INV        EVTDDV
036200000000     C                   Z-ADD     V1CDSV        G02DAT
036300000000     C                   Z-ADD     *ZEROS        G02INV
036400000000     C                   MOVEL     '0'           G02ERR
036500000000     C                   CALL      'XSRDA8'
036600000000     C                   PARM                    WLBDAT
036700000000     C                   Z-ADD     G02INV        EVTDSV
036800000000     C                   MOVEL     V1CTIV        EVTTIV
036900030422     c                   if        v1ctiv = '05'
037000030422     C                   MOVEL     V1CSPS        EVTSPS
037100030422     C                   Z-ADD     V1CRPV        EVTRPV
037200030422     c                   else
037300030423     c                   clear                   devtxxx
037400030422     c                   end
037500030423     C                   MOVEL(P)  DEVTXXX       EVTXXX
037600030422     C                   MOVEL     V1CSPT        EVTSPT
037700000000     C                   MOVEL     V1CRFI        EVTRFI
037800000000     C                   MOVEL     V1CRFE        EVTRFE
037900000000     C                   MOVEL     V1CRTI        EVTRTI
038000000000     C                   MOVEL     V1CRTE        EVTRTE
038100000000     C                   MOVEL     V1CRPI        EVTRPI
038200000000     C                   MOVEL     V1CRPE        EVTRPE
038300000000     C                   MOVEL     V1CRKI        EVTRKI
038400000000     C                   MOVEL     V1CRKE        EVTRKE
038500000000     C                   MOVEL     V1CCMP        EVTCMP
038600000000     C                   Z-ADD     V1CARF        EVTARF
038700000000     C                   Z-ADD     V1CARL        EVTARL
038800000000     C                   Z-ADD     V1CARO        EVTARO
038900970129     C                   MOVEL     V1CRDL        EVTRDL
039000021209     C                   MOVEL     V1CCOP        EVTCOP
039100021209     C                   MOVEL     V1CDAT        EVTDAT
039200021209     C                   MOVEL     V1CAID        EVTAID
039300000000     C*
039400000000     C                   MOVEL     *BLANKS       EVTATB
039500000000     C                   MOVEL     *BLANKS       EVTFTR
039600000000     C                   Z-ADD     *ZEROS        EVTDTR
039700000000     C*
039800000000     C                   WRITE     ECEVT
039900130924      *duplica note da progressivo precedente
040000130924     c                   exsr      dupnot
040100000000     C*
040200000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
040300000000     C                   MOVEL     '1'           $FINE
040400000000     C*
040500000000     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
040600000000     C                   MOVEL     MSG(17)       D60MSG
040700000000     C*
040800000000E   1C                   ENDIF
040900000000     C*
041000000000     C                   ENDSR
041100000000     C*------------------------------------------------------------------------*
041200010112     C* MODIFI - FUNZIONE DI MODIFICA
041300000000     C*------------------------------------------------------------------------*
041400000000     C     MODIFI        BEGSR
041500000000     C*
041600000000     C* CONTROLLA I CAMPI IMMESSI
041700000000     C                   EXSR      DECODI
041800000000     C*
041900000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
042000000000     C                   Z-ADD     D60VOC        KVTVOC
042100000000     C                   Z-ADD     D60PRG        KVTPRG
042200000000     C     KEYEVT        CHAIN     ECEVT01L                           99
042300000000IF  1C     *IN99         IFEQ      *OFF
042400000000     C*
042500000000IF  2C     $ERR          IFEQ      '0'
042600000000     C*
042700000000     C                   Z-ADD     V1CVOC        EVTVOC
042800000000     C                   Z-ADD     V1CPRG        EVTPRG
042900000000     C                   MOVEL     V1CVOD        EVTVOD
043000000000     C                   MOVEL     V1CTVO        EVTTVO
043100000000     C                   MOVEL     V1CVRV        EVTVRV
043200000000     C                   Z-ADD     V1CDDV        G02DAT
043300000000     C                   Z-ADD     *ZEROS        G02INV
043400000000     C                   MOVEL     '0'           G02ERR
043500000000     C                   CALL      'XSRDA8'
043600000000     C                   PARM                    WLBDAT
043700000000     C                   Z-ADD     G02INV        EVTDDV
043800000000     C                   Z-ADD     V1CDSV        G02DAT
043900000000     C                   MOVEL     '0'           G02ERR
044000000000     C                   CALL      'XSRDA8'
044100000000     C                   PARM                    WLBDAT
044200000000     C                   Z-ADD     G02INV        EVTDSV
044300000000     C                   MOVEL     V1CTIV        EVTTIV
044400000000     C                   MOVEL     V1CSPT        EVTSPT
044500030422     c                   if        v1ctiv = '05'
044600030422     C                   MOVEL     V1CSPS        EVTSPS
044700030422     C                   Z-ADD     V1CRPV        EVTRPV
044800030422     c                   else
044900030423     C                   clear                   dEVTXXX
045000030422     c                   end
045100030423     C                   MOVEL(P)  DEVTXXX       EVTXXX
045200000000     C                   MOVEL     V1CCMP        EVTCMP
045300000000     C                   MOVEL     V1CRFI        EVTRFI
045400000000     C                   MOVEL     V1CRFE        EVTRFE
045500000000     C                   MOVEL     V1CRTI        EVTRTI
045600000000     C                   MOVEL     V1CRTE        EVTRTE
045700000000     C                   MOVEL     V1CRPI        EVTRPI
045800000000     C                   MOVEL     V1CRPE        EVTRPE
045900000000     C                   MOVEL     V1CRKI        EVTRKI
046000000000     C                   MOVEL     V1CRKE        EVTRKE
046100000000     C                   MOVEL     V1CCMP        EVTCMP
046200000000     C                   Z-ADD     V1CARF        EVTARF
046300000000     C                   Z-ADD     V1CARL        EVTARL
046400000000     C                   Z-ADD     V1CARO        EVTARO
046500970129     C                   MOVEL     V1CRDL        EVTRDL
046600021209     C                   MOVEL     V1CCOP        EVTCOP
046700021209     C                   MOVEL     V1CDAT        EVTDAT
046800021209     C                   MOVEL     V1CAID        EVTAID
046900000000     C*
047000000000     C                   MOVEL     *BLANKS       EVTATB
047100000000     C                   MOVEL     *BLANKS       EVTFTR
047200970903     C*---                 Z-ADD*ZEROS    EVTDTR           *DATA TRASMISSIONE
047300000000     C*
047400010112     C                   UPDATE    ECEVT
047500010112     C*
047600010112     C* LANCIO PGM DI MODIFICA TARIFFE PER CAMBIO DATA SCADENZA VOCE
047700010112     C                   Z-ADD     V1CVOC        I61VOC
047800010112     C                   Z-ADD     V1CPRG        I61PRG
047900010112     C                   Z-ADD     DDVV1C        I61DAI
048000010112     C                   Z-ADD     DSVV1C        I61DAF
048100010112     C                   Z-ADD     *ZEROS        I61DRI
048200010112     C                   Z-ADD     *ZEROS        O61VOC
048300010112     C                   Z-ADD     *ZEROS        O61PRG
048400010112     C                   CALL      'TREC61R1'
048500010112     C                   PARM                    DS60
048600010112     C                   PARM                    DS61
048700010112     C* VERIFICO L'ESITO (INCONSISTENTE MA... NON SI SA MAI)
048800010112     C     D60ERR        IFEQ      '1'
048900010112     C                   MOVE      '1'           $ERR
049000010112     C                   MOVEL     MSG(41)       $MSG
049100010112     C                   EXSR      CARMSG
049200010112     C                   ENDIF
049300000000     C*
049400000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
049500000000     C                   MOVEL     '1'           $FINE
049600000000     C*
049700000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
049800000000     C                   MOVEL     MSG(18)       D60MSG
049900000000     C*
050000000000E   2C                   ENDIF
050100000000E   1C                   ENDIF
050200000000     C*
050300000000     C                   ENDSR
050400010110     C*------------------------------------------------------------------------*
050500010110     C* DUPLIC - FUNZIONE DI DUPLICAZIONE
050600010110     C*------------------------------------------------------------------------*
050700010110     C     DUPLIC        BEGSR
050800010110     C*
050900010110     C* CONTROLLA I CAMPI IMMESSI
051000010110     C                   EXSR      DECODI
051100010110     C*
051200010110IF  1C     $ERR          IFEQ      '0'
051300010110     C*
051400010110     C                   Z-ADD     V1CVOC        EVTVOC
051500010110     C                   Z-ADD     V1CPRG        EVTPRG
051600010110     C                   MOVEL     V1CVOD        EVTVOD
051700010110     C                   MOVEL     V1CTVO        EVTTVO
051800010110     C                   MOVEL     V1CVRV        EVTVRV
051900010110     C                   Z-ADD     V1CDDV        G02DAT
052000010110     C                   Z-ADD     *ZEROS        G02INV
052100010110     C                   MOVEL     '0'           G02ERR
052200010110     C                   CALL      'XSRDA8'
052300010110     C                   PARM                    WLBDAT
052400010110     C                   Z-ADD     G02INV        EVTDDV
052500010110     C                   Z-ADD     V1CDSV        G02DAT
052600010110     C                   Z-ADD     *ZEROS        G02INV
052700010110     C                   MOVEL     '0'           G02ERR
052800010110     C                   CALL      'XSRDA8'
052900010110     C                   PARM                    WLBDAT
053000010110     C                   Z-ADD     G02INV        EVTDSV
053100010110     C                   MOVEL     V1CTIV        EVTTIV
053200010110     C                   MOVEL     V1CSPT        EVTSPT
053300030422     c                   if        v1ctiv = '05'
053400030422     C                   MOVEL     V1CSPS        EVTSPS
053500030422     C                   Z-ADD     V1CRPV        EVTRPV
053600030422     c                   else
053700030423     C                   clear                   dEVTXXX
053800030422     c                   end
053900030423     C                   MOVEL(P)  DEVTXXX       EVTXXX
054000010110     C                   MOVEL     V1CRFI        EVTRFI
054100010110     C                   MOVEL     V1CRFE        EVTRFE
054200010110     C                   MOVEL     V1CRTI        EVTRTI
054300010110     C                   MOVEL     V1CRTE        EVTRTE
054400010110     C                   MOVEL     V1CRPI        EVTRPI
054500010110     C                   MOVEL     V1CRPE        EVTRPE
054600010110     C                   MOVEL     V1CRKI        EVTRKI
054700010110     C                   MOVEL     V1CRKE        EVTRKE
054800010110     C                   MOVEL     V1CCMP        EVTCMP
054900010110     C                   Z-ADD     V1CARF        EVTARF
055000010110     C                   Z-ADD     V1CARL        EVTARL
055100010110     C                   Z-ADD     V1CARO        EVTARO
055200010110     C                   MOVEL     V1CRDL        EVTRDL
055300021209     C                   MOVEL     V1CCOP        EVTCOP
055400021209     C                   MOVEL     V1CDAT        EVTDAT
055500021209     C                   MOVEL     V1CAID        EVTAID
055600010110     C*
055700010110     C                   MOVEL     *BLANKS       EVTATB
055800010110     C                   MOVEL     *BLANKS       EVTFTR
055900010110     C                   Z-ADD     *ZEROS        EVTDTR
056000010110     C*
056100010110     C                   WRITE     ECEVT
056200130924      *duplica note da progressivo precedente
056300130924     c                   exsr      dupnot
056400010110     C*
056500010110     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
056600010110     C                   MOVEL     '1'           $FINE
056700010110     C*
056800010110     C* AGGIORNO I CAMPI DELLA DS PASSATI
056900010111     C                   Z-ADD     EVTVOC        D60VOC
057000010111     C                   Z-ADD     EVTPRG        D60PRG
057100010111     C                   Z-ADD     EVTDDV        D60DDV
057200010111     C                   Z-ADD     EVTDSV        D60DDT
057300010115     C*
057400010115     C* VERIFICO EVENTUALI CAMBIAMENTI DEI DATI DI CORPO DELLA VOCE
057500010115     C     V1CTVO        IFNE      SAVTVO
057600010115     C     V1CVRV        ORNE      SAVVRV
057700010115     C     V1CTIV        ORNE      SAVTIV
057800010115     C     V1CSPT        ORNE      SAVSPT
057900030422     C     V1Csps        ORNE      SAVsps
058000030422     C     V1Crpv        ORNE      SAVrpv
058100010115     C     V1CCMP        ORNE      SAVCMP
058200010115     C     V1CRFI        ORNE      SAVRFI
058300010115     C     V1CRFE        ORNE      SAVRFE
058400010115     C     V1CRTI        ORNE      SAVRTI
058500010115     C     V1CRTE        ORNE      SAVRTE
058600010115     C     V1CRPI        ORNE      SAVRPI
058700010115     C     V1CRPE        ORNE      SAVRPE
058800010115     C     V1CRKI        ORNE      SAVRKI
058900010115     C     V1CRKE        ORNE      SAVRKE
059000010115     C     V1CARF        ORNE      SAVARF
059100010115     C     V1CARL        ORNE      SAVARL
059200010115     C     V1CARO        ORNE      SAVARO
059300010115     C     V1CRDL        ORNE      SAVRDL
059400021209     C     V1CCOP        ORNE      SAVCOP
059500021209     C     V1CDAT        ORNE      SAVDAT
059600021209     C     V1CAID        ORNE      SAVAID
059700010115     C                   MOVEL     '2'           D60ERR
059800010115     C                   ENDIF
059900010110     C*
060000010110     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
060100010110     C                   MOVEL     MSG(32)       D60MSG
060200010110     C*
060300010110E   1C                   ENDIF
060400010110     C*
060500010110     C                   ENDSR
060600000000     C*------------------------------------------------------------------------*
060700000000     C* ANNULL - FUNZIONE DI ANNULLAMENTO
060800000000     C*------------------------------------------------------------------------*
060900000000     C     ANNULL        BEGSR
061000000000     C*
061100000000     C* CONTROLLA I CAMPI IMMESSI
061200040126     C*                  EXSR      DECODI
061300000000     C*
061400000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
061500000000     C                   Z-ADD     D60VOC        KVTVOC
061600000000     C                   Z-ADD     D60PRG        KVTPRG
061700000000     C     KEYEVT        CHAIN     ECEVT01L                           99
061800000000IF  1C     *IN99         IFEQ      *OFF
061900000000     C*
062000000000IF  2C     $ERR          IFEQ      '0'
062100000000     C*--------------------
062200000000     C* ANNULLO VOCE
062300000000     C*--------------------
062400970903     C* SE NON E' STATO MAI TRASMESSO, CANCELLA FISICAMENTE IL RECORD
062500970903IF  3C     EVTDTR        IFGT      *ZEROS
062600000000     C                   MOVEL     'A'           EVTATB
062700000000     C                   MOVEL     *BLANKS       EVTFTR
062800970903     C*---                 Z-ADD*ZEROS    EVTDTR           *DATA TRASMISSIONE
062900000000     C                   UPDATE    ECEVT
063000970903X   3C                   ELSE
063100970903     C                   DELETE    ECEVT
063200970903E   3C                   ENDIF
063201160226     C*--------------------
063202160226     C* ANNULLO note VOCE
063203160226     C*--------------------
063204160226     c     knot          setll     ecnot01l
063205160226     c                   do        *hival
063206160226     c     knot          reade     ecnot01l
063207160226     c                   if        %eof(ecnot01l)
063208160226     c                   leave
063209160226     c                   endif
063210160226     c                   delete    ecnot000
063211160226     c                   enddo
063300000000     C*--------------------
063400000000     C* ANNULLO TARIFFE VOCE
063500000000     C*--------------------
063600970903     C* SE NON E' STATO MAI TRASMESSO, CANCELLA FISICAMENTE IL RECORD
063700000000     C                   Z-ADD     V1CVOC        KVDVOC
063800000000     C                   Z-ADD     V1CPRG        KVDPRG
063900140114     C     KEYEVD        SETLL     ECEVD06L
064000140114     C     KEYEVD        READE     ECEVD06L                               99
064100000000DO  3C     *IN99         DOWEQ     *OFF
064200000000     C*
064300000000IF  4C     EVDDDT        IFGE      EVTDDV
064400000000     C     EVDDST        ANDLE     EVTDSV
064500000000     C     EVDATB        ANDNE     'A'
064600970903IF  5C     EVDDTR        IFGT      *ZEROS
064700000000     C                   MOVEL     'A'           EVDATB
064800000000     C                   MOVEL     *BLANKS       EVDFTR
064900010808     C                   Z-ADD     DATCOR        EVDDUV
065000010808     C                   UPDATE    ECEVD000
065100970903X   5C                   ELSE
065200010808     C                   DELETE    ECEVD000
065300970903E   5C                   ENDIF
065400000000E   4C                   ENDIF
065500140114     C     KEYEVD        READE     ECEVD06L                               99
065600000000E   3C                   ENDDO
065700000000     C*--------------------
065800000000     C* ANNULLO LE MAGGIORAZIONI PER LA VOCE
065900000000     C*--------------------
066000970903     C* SE NON E' STATO MAI TRASMESSO, CANCELLA FISICAMENTE IL RECORD
066100000000     C                   Z-ADD     V1CVOC        KVDVOC
066200000000     C     KEYEVD        SETLL     ECEVM01L
066300000000     C     KEYEVD        READE     ECEVM01L                               99
066400000000DO  3C     *IN99         DOWEQ     *OFF
066500000000IF  4C     EVMATB        IFNE      'A'
066600970903IF  5C     EVMDTR        IFGT      *ZEROS
066700000000     C                   MOVEL     'A'           EVMATB
066800000000     C                   MOVEL     *BLANKS       EVMFTR
066900970903     C*---                 Z-ADD*ZEROS    EVMDTR           *DATA TRASMISSIONE
067000000000     C                   UPDATE    ECEVM
067100970903X   5C                   ELSE
067200970903     C                   DELETE    ECEVM
067300970903E   5C                   ENDIF
067400970903E   4C                   ENDIF
067500000000     C     KEYEVD        READE     ECEVM01L                               99
067600000000E   3C                   ENDDO
067700000000     C*
067800000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
067900000000     C                   MOVEL     '1'           $FINE
068000000000     C*
068100000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
068200000000     C                   MOVEL     MSG(19)       D60MSG
068300000000     C*
068400000000E   2C                   ENDIF
068500000000E   1C                   ENDIF
068600000000     C*
068700000000     C                   ENDSR
068800000000     C*------------------------------------------------------------------------*
068900000000     C* RIPRIS - FUNZIONE DI RIPRISTINO
069000000000     C*------------------------------------------------------------------------*
069100000000     C     RIPRIS        BEGSR
069200000000     C*
069300000000     C* CONTROLLA I CAMPI IMMESSI
069400000000     C                   EXSR      DECODI
069500000000     C*
069600000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
069700000000     C                   Z-ADD     D60VOC        KVTVOC
069800000000     C                   Z-ADD     D60PRG        KVTPRG
069900000000     C     KEYEVT        CHAIN     ECEVT01L                           99
070000000000IF  1C     *IN99         IFEQ      *OFF
070100000000     C*
070200000000IF  2C     $ERR          IFEQ      '0'
070300000000     C*--------------------
070400000000     C* RIPRISTINO VOCE
070500000000     C*--------------------
070600000000     C                   MOVEL     *BLANKS       EVTATB
070700000000     C                   MOVEL     *BLANKS       EVTFTR
070800970903     C*---                 Z-ADD*ZEROS    EVTDTR           *DATA TRASMISSIONE
070900000000     C*
071000000000     C                   UPDATE    ECEVT
071100000000     C*
071200000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
071300000000     C                   MOVEL     '1'           $FINE
071400000000     C*
071500000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
071600000000     C                   MOVEL     MSG(20)       D60MSG
071700000000     C*
071800000000E   2C                   ENDIF
071900000000E   1C                   ENDIF
072000000000     C*
072100000000     C                   ENDSR
072200000000     C*------------------------------------------------------------------------*
072300010112     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
072400000000     C*------------------------------------------------------------------------*
072500000000     C     DECODI        BEGSR
072600000000     C*
072700000000     C* REIMPOSTO INDICATORI DI ERRORE
072800000000     C                   SETOFF                                       606162
072900000000     C                   SETOFF                                       636465
073000000000     C                   SETOFF                                       666768
073100000000     C                   SETOFF                                       697071
073200000000     C                   SETOFF                                       727374
073300000000     C                   SETOFF                                       757677
073400000000     C                   SETOFF                                       787980
073500030422     C                   SETOFF                                       818283
073600030422     C                   SETOFF                                       84
073700040126     c                   if        d60op1 <> 'O04'
073800010110     C*
073900010110     C* IMPOSTO PER DEFAULT L'EERORE PER POTER GESTIRE DAL CHIAMANTE L'OK
074000010110     C                   MOVEL     '1'           D60ERR
074100000000     C*--------------------
074200000000     C* CODICE VOCE
074300000000     C*--------------------
074400000000IF  1C     V1CVOC        IFEQ      *ZEROS
074500000000     C                   MOVEL     '1'           $ERR
074600000000     C                   MOVEL     MSG(9)        $MSG
074700000000     C                   EXSR      CARMSG
074800000000     C                   SETON                                        60
074900000000E   1C                   ENDIF
075000000000     C*--------------------
075100000000     C* PROGRESSIVO
075200000000     C*--------------------
075300000000IF  1C     $ERR          IFEQ      '0'
075400000000IF  2C     V1CPRG        IFEQ      *ZEROS
075500000000     C                   MOVEL     '1'           $ERR
075600000000     C                   MOVEL     MSG(29)       $MSG
075700000000     C                   EXSR      CARMSG
075800000000     C                   SETON                                        80
075900000000E   2C                   ENDIF
076000000000E   1C                   ENDIF
076100951207     C*--------------------
076200951207     C* INSERIMENTO -> VOCE INESISTENTE
076300951207     C*--------------------
076400951207IF  1C     $ERR          IFEQ      '0'
076500951207IF  2C     D60OP1        IFEQ      'F06'
076600010111     C     D60OP1        OREQ      'O03'
076700951207     C                   Z-ADD     V1CVOC        KVTVOC
076800951207     C                   Z-ADD     V1CPRG        KVTPRG
076900951207     C     KEYEVT        SETLL     ECEVT01L                               99
077000951207IF  3C     *IN99         IFEQ      *ON
077100951207     C                   MOVEL     '1'           $ERR
077200951207     C                   MOVEL     MSG(13)       $MSG
077300951207     C                   EXSR      CARMSG
077400951207     C                   SETON                                        6080
077500951207E   3C                   ENDIF
077600951207E   2C                   ENDIF
077700951207E   1C                   ENDIF
077800000000     C*--------------------
077900000000     C* DATA DECORRENZA
078000000000     C*--------------------
078100000000IF  1C     $ERR          IFEQ      '0'
078200000000     C                   Z-ADD     V1CDDV        $DAT              8 0
078300000000     C                   EXSR      CHKDAT
078400000000     C                   Z-ADD     $DAT          V1CDDV
078500000000IF  2C     $ERR          IFEQ      '1'
078600000000     C                   EXSR      CARMSG
078700000000     C                   SETON                                        61
078800000000E   2C                   ENDIF
078900000000E   1C                   ENDIF
079000000000     C*--------------------
079100000000     C* DATA SCADENZA
079200000000     C*--------------------
079300000000IF  1C     $ERR          IFEQ      '0'
079400000000     C                   Z-ADD     V1CDSV        $DAT
079500000000     C                   EXSR      CHKDAT
079600000000     C                   Z-ADD     $DAT          V1CDSV
079700000000IF  2C     $ERR          IFEQ      '1'
079800000000     C                   EXSR      CARMSG
079900000000     C                   SETON                                        63
080000000000E   2C                   ENDIF
080100000000     C*
080200000000     C* MAGGIORE DELLA DECORRENZA
080300000000IF  2C     $ERR          IFEQ      '0'
080400000000     C                   Z-ADD     V1CDDV        G02DAT
080500000000     C                   Z-ADD     *ZEROS        G02INV
080600000000     C                   MOVEL     '0'           G02ERR
080700000000     C                   CALL      'XSRDA8'
080800000000     C                   PARM                    WLBDAT
080900000000     C                   Z-ADD     G02INV        DDVV1C            8 0
081000000000     C*
081100000000     C                   Z-ADD     V1CDSV        G02DAT
081200000000     C                   Z-ADD     *ZEROS        G02INV
081300000000     C                   MOVEL     '0'           G02ERR
081400000000     C                   CALL      'XSRDA8'
081500000000     C                   PARM                    WLBDAT
081600000000     C                   Z-ADD     G02INV        DSVV1C            8 0
081700000000     C*
081800000000IF  3C     DSVV1C        IFLT      DDVV1C
081900000000     C                   MOVEL     '1'           $ERR
082000000000     C                   MOVEL     MSG(23)       $MSG
082100000000     C                   EXSR      CARMSG
082200000000     C                   SETON                                        6163
082300000000E   3C                   ENDIF
082400000000E   2C                   ENDIF
082500000000E   1C                   ENDIF
082600000000     C*--------------------
082700000000     C* VALIDITA' VOCE
082800000000     C*--------------------
082900000000IF  1C     $ERR          IFEQ      '0'
083000000000     C*
083100000000     C                   Z-ADD     V1CVOC        KVTVOC
083200000000     C     KE2EVT        SETLL     ECEVT01L
083300000000     C     KE2EVT        READE(N)  ECEVT01L                               99
083400000000DO  2C     *IN99         DOWEQ     *OFF
083500951207IF  3C     EVTATB        IFEQ      *BLANKS
083600000000     C*
083700000000     C* SE SONO IN MODIFI/ANNULL/RIPRIS DAL CONTROLLO ESCLUDO LA VOCE IN LINEA
083800000000IF  4C     D60OP1        IFNE      'F06'
083900010111     C     D60OP1        ANDNE     'O03'
084000000000     C     V1CPRG        ANDEQ     EVTPRG
084100000000X   4C                   ELSE
084200000000     C*
084300000000     C* NESSUNA VOCE CON PERIODI SOVRAPPOSTI
084400000000IF  5C     DDVV1C        IFLE      EVTDSV
084500000000IF  6C     DSVV1C        IFLT      EVTDDV
084600000000X   6C                   ELSE
084700000000     C                   MOVEL     '1'           $ERR
084800000000     C                   MOVEL     MSG(24)       $MSG
084900000000     C                   EXSR      CARMSG
085000000000     C                   SETON                                        616399
085100000000E   6C                   ENDIF
085200000000E   5C                   ENDIF
085300000000     C*
085400000000     C* NON CI POSSONO ESSERE PROGRESSIVI DOPPI
085500000000IF  5C     V1CPRG        IFNE      EVTPRG
085600000000X   5C                   ELSE
085700000000     C                   MOVEL     '1'           $ERR
085800000000     C                   MOVEL     MSG(30)       $MSG
085900000000     C                   EXSR      CARMSG
086000000000     C                   SETON                                        80  99
086100000000E   5C                   ENDIF
086200000000     C*
086300000000     C* VERIFICO ORA CHE IL TIPO VOCE SIA UGUALE CON QUELLI GIA' ESISTENTI
086400000000IF  5C     V1CTVO        IFNE      *BLANKS
086500000000IF  6C     V1CTVO        IFEQ      EVTTVO
086600000000X   6C                   ELSE
086700000000     C                   MOVEL     '1'           $ERR
086800000000     C                   MOVEL     MSG(31)       $MSG
086900000000     C                   EXSR      CARMSG
087000000000     C                   SETON                                        64  99
087100000000E   6C                   ENDIF
087200000000E   5C                   ENDIF
087300000000     C*
087400000000E   4C                   ENDIF
087500951207E   3C                   ENDIF
087600000000     C  N99KE2EVT        READE(N)  ECEVT01L                               99
087700000000E   2C                   ENDDO
087800000000     C*
087900000000E   1C                   ENDIF
088000000000     C*--------------------
088100000000     C* DESCRIZIONE VOCE
088200000000     C*--------------------
088300000000IF  1C     $ERR          IFEQ      '0'
088400000000IF  2C     V1CVOD        IFEQ      *BLANKS
088500000000     C                   MOVEL     '1'           $ERR
088600000000     C                   MOVEL     MSG(10)       $MSG
088700000000     C                   EXSR      CARMSG
088800000000     C                   SETON                                        62
088900000000E   2C                   ENDIF
089000000000E   1C                   ENDIF
089100000000     C*--------------------
089200000000     C* TIPO VOCE
089300000000     C*--------------------
089400000000IF  1C     $ERR          IFEQ      '0'
089500000000IF  2C     V1CTVO        IFEQ      'V'
089600000000     C     V1CTVO        OREQ      'M'
089700000000X   2C                   ELSE
089800000000     C                   MOVEL     '1'           $ERR
089900000000     C                   MOVEL     MSG(11)       $MSG
090000000000     C                   EXSR      CARMSG
090100000000     C                   SETON                                        64
090200000000E   2C                   ENDIF
090300000000E   1C                   ENDIF
090400000000     C*--------------------
090500940726     C* TIPO TARIFFA
090600000000     C*--------------------
090700940726     C                   MOVEL     *BLANKS       V1DVRV
090800940726     C* SE E' 'VOCE ' IMMETTERE TIPO TARIFFA (OBBLIGATORIO)
090900000000IF  1C     $ERR          IFEQ      '0'
091000940726IF  2C     V1CVRV        IFNE      *BLANKS
091100940726     C     V1CTVO        OREQ      'V'
091200940726     C                   MOVEL     V1CVRV        $VRV              2
091300000000     C                   MOVEL     *BLANKS       $DVRV            20
091400141202     c                   MOVEL     'VRV'         §TIPO             3
091500000000     C                   EXSR      CHKVRV
091600000000     C                   MOVEL     $DVRV         V1DVRV
091700000000     C                   MOVEL     $VRV          V1CVRV
091800000000     C                   MOVEL     §2RPIU        $2RPIU            1
091900000000     C                   MOVEL     §2RRPV        $2RRPV            1
092000940725IF  3C     $ERR          IFEQ      '1'
092100000000     C                   EXSR      CARMSG
092200000000     C                   SETON                                        65
092300940725E   3C                   ENDIF
092400940726     C*
092500940726     C* SE E' MAGGIORAZIONE IMPONGO 'S' NEL FLAG RICHIESTA SCAGLIONI
092600940726X   2C                   ELSE
092700940726     C                   MOVEL     'S'           $2RPIU
092800940726     C                   MOVEL     'S'           $2RRPV
092900940725E   2C                   ENDIF
093000940725E   1C                   ENDIF
093100100421     C* in base al tipo di tariffa SOLO x le voci controllo che il cod.SICOP
093200100421     c* sia nel range corretto. per le maggiorazioni lo verifico nel
093300100421     c* pgm relativo
093400100421IF  1C                   IF        $err = '0'
093500100421     C                   EXSR      CHK2o
093600100421IF  3C     $ERR          IFEQ      '1'
093700100421     C                   EXSR      CARMSG
093800100421     C                   SETON                                        60
093900100421E   3C                   ENDIF
094000100421E   1C                   ENDIF
094100100421     C* NON si possono inserire scadenze per la voce isolata e nemmeno SICOP
094200100421     c* ricerca scaglioni ecc.
094300100421IF  1C     $ERR          IFEQ      '0'
094400100421     c                   if        v1cvrv = '26'
094500100421IF  2C                   IF        dsvv1c = 20391231 and
094600100421     c                             v1cprg = 1 and
094700100421     c                             v1ctvo = 'V' and
094800100421     c                             v1ctiv = ' ' and
094900100421     c                             v1cspt = ' ' and
095000150410     c*                            v1crdl = '001'  and
095100100421     c                             v1ccmp <> ' ' and
095200100421     c                             v1crfi = ' ' and
095300100421     c                             v1crfe = ' ' and
095400100421     c                             v1crti = ' ' and
095500100421     c                             v1crte = ' ' and
095600100421     c                             v1crpi = ' ' and
095700100421     c                             v1crpe = ' ' and
095800100421     c                             v1crki = ' ' and
095900100421     c                             v1crke = ' ' and
096000100421     c                             v1carl = 0 and
096100100421     c                             v1carf = 0 and
096200100421     c                             v1caro = 0 and
096300100421     c                             v1ccop = ' ' and
096400100427     c*                            v1cdat = ' ' and
096500100421     c                             v1caid = ' '
096600100421     c                   else
096700100421     C                   MOVEL     '1'           $ERR
096800100421     C                   MOVEL     MSG(47)       $MSG
096900100421     C                   EXSR      CARMSG
097000100421     C                   SETON                                        9960
097100100421E   2C                   ENDIF
097200100421E   2C                   ENDIF
097300100421E   2C                   ENDIF
097400961022     C*
097500141203     C* SE E' MAGGIORAZIONE NON ACCETTO TIPO TARIFFA se non abilitata
097600961022IF  1C     $ERR          IFEQ      '0'
097700961022IF  2C     V1CVRV        IFNE      *BLANKS
097800961022     C     V1CTVO        ANDEQ     'M'
097900141128     C     §2rmag        ANDEQ     ' '
098000961022     C                   MOVEL     MSG(36)       $MSG
098100961022     C                   EXSR      CARMSG
098200961022     C                   SETON                                        65
098300961022E   2C                   ENDIF
098400961022E   1C                   ENDIF
098500000000     C*--------------------
098600000000     C* TIPO RICERCA SCAGLIONE
098700000000     C*--------------------
098800940726     C                   MOVEL     *BLANKS       V1DTIV
098900940726     C* SE NON E' MAGGIORAZIONE INSERIRE IL TIPO RICERCA SOLO SE E' PRESENTE
099000940726     C* IL TIPO TARIFFA E SE IL TIPO TARIFFA VUOLE PIU' SCAGLIONI
099100000000IF  1C     $ERR          IFEQ      '0'
099200940726IF  2C     V1CTVO        IFEQ      'V'
099300940726IF  3C     V1CVRV        IFEQ      *BLANKS
099400940726IF  4C     V1CTIV        IFNE      *BLANKS
099500940725     C                   MOVEL     MSG(35)       $MSG
099600940725     C                   EXSR      CARMSG
099700940725     C                   SETON                                        67
099800940726E   4C                   ENDIF
099900940726X   3C                   ELSE
100000940726IF  4C     V1CTIV        IFNE      *BLANKS
100100940726IF  5C     $2RPIU        IFEQ      'N'
100200940726     C                   MOVEL     MSG(34)       $MSG
100300940726     C                   EXSR      CARMSG
100400940726     C                   SETON                                        67
100500940726E   5C                   ENDIF
100600940726E   4C                   ENDIF
100700940726E   3C                   ENDIF
100800940726E   2C                   ENDIF
100900940726E   1C                   ENDIF
101000940726     C*
101100940726     C* SE INSERITO, CONTROLLO
101200940726IF  1C     $ERR          IFEQ      '0'
101300940726IF  2C     V1CTIV        IFNE      *BLANKS
101400000000     C                   MOVEL     *BLANKS       V1DTIV
101500940726     C                   MOVEL     *BLANKS       $DVRV            20
101600000000     C                   MOVEL     V1CTIV        $VRV              2
101700030423     c                   MOVEL     'TIV'         $TIPO             3
101800000000     C                   EXSR      CHKVRV
101900000000     C                   MOVEL     $DVRV         V1DTIV
102000000000     C                   MOVEL     $VRV          V1CTIV
102100940726IF  3C     $ERR          IFEQ      '1'
102200000000     C                   EXSR      CARMSG
102300000000     C                   SETON                                        67
102400940726E   3C                   ENDIF
102500000000     C*
102600000000E   2C                   ENDIF
102700000000E   1C                   ENDIF
102800961022     C*
102900961022     C* SE E' MAGGIORAZIONE NON ACCETTO TIPO RICERCA SCAGLIONE
103000961022IF  1C     $ERR          IFEQ      '0'
103100961022IF  2C     V1CTIV        IFNE      *BLANKS
103200961022     C     V1CTVO        ANDEQ     'M'
103300961022     C                   MOVEL     MSG(36)       $MSG
103400961022     C                   EXSR      CARMSG
103500961022     C                   SETON                                        67
103600961022E   2C                   ENDIF
103700961022E   1C                   ENDIF
103800000000     C*--------------------
103900030422     C* RAPPORTO PESO VOL. E SVILUPPO PESO TASSAB. PER RICERCA SCAGLIONE 05
104000000000     C*--------------------
104100030422IF  1C     $ERR          IFEQ      '0'
104200030422IF  2C     V1CTIV        ANDEQ     '05'
104300030422IF  3C     V1CSPS        IFNE      '1'
104400030422     C     V1CSPS        ANDNE     '2'
104500030422     C     V1CSPS        ANDNE     '3'
104600030422     C                   MOVEL     '1'           $ERR
104700030422     C                   MOVEL     MSG(44)       $MSG
104800030422     C                   EXSR      CARMSG
104900030422     C                   SETON                                        83
105000030422E   3C                   ENDIF
105100030422     c                   else
105200030422     c                   clear                   v1csps
105300030422E   1C                   ENDIF
105400030422IF  1C     $ERR          IFEQ      '0'
105500030422IF  2C     V1CTIV        ANDEQ     '05'
105600030422IF  3C     V1CRPV        IFEQ      0
105700030422     C                   MOVEL     '1'           $ERR
105800030422     C                   MOVEL     MSG(45)       $MSG
105900030422     C                   EXSR      CARMSG
106000030422     C                   SETON                                        84
106100030422E   3C                   ENDIF
106200030422     c                   else
106300030422     c                   clear                   v1crpv
106400030422E   1C                   ENDIF
106500030422     C*--------------------
106600030422     C* SVILUPPO PESO TASSABILE
106700030422     C* LO ACCETTO SOLO SE C'E' "S=RAPP. PESO VOLUME" NEL TIPO TARIFFA
106800030422     C*--------------------
106900000000IF  1C     $ERR          IFEQ      '0'
107000000000IF  2C     $2RRPV        IFEQ      'S'
107100000000IF  3C     V1CSPT        IFNE      '1'
107200000000     C     V1CSPT        ANDNE     '2'
107300000000     C     V1CSPT        ANDNE     '3'
107400000000     C     V1CSPT        ANDNE     *BLANKS
107500000000     C                   MOVEL     '1'           $ERR
107600000000     C                   MOVEL     MSG(15)       $MSG
107700000000     C                   EXSR      CARMSG
107800000000     C                   SETON                                        69
107900000000E   3C                   ENDIF
108000000000     C*
108100000000X   2C                   ELSE
108200000000     C*
108300000000IF  3C     V1CSPT        IFNE      *BLANKS
108400000000     C                   MOVEL     '1'           $ERR
108500000000     C                   MOVEL     MSG(27)       $MSG
108600000000     C                   EXSR      CARMSG
108700000000     C                   SETON                                        69
108800000000E   3C                   ENDIF
108900000000E   2C                   ENDIF
109000000000E   1C                   ENDIF
109100970129     C*--------------------
109200970129     C* CODICE RAGGRUPPAMENTO DELTA
109300970129     C*--------------------
109400980318     C* OBBLIGATORIA SOLO SE TIPO VOCE = 'V'
109500980318     C                   MOVEL     *BLANKS       V1DRDL
109600970129IF  1C     $ERR          IFEQ      '0'
109700980318IF  2C     V1CTVO        IFEQ      'V'
109800970129     C                   MOVEL     V1CRDL        $RDL              3
109900970129     C                   MOVEL     *BLANKS       $DRDL            30
110000970129     C                   EXSR      CHKRDL
110100970129     C                   MOVEL     $DRDL         V1DRDL
110200970129     C                   MOVEL     $RDL          V1CRDL
110300980318IF  3C     $ERR          IFEQ      '1'
110400970129     C                   EXSR      CARMSG
110500970129     C                   SETON                                        81
110600980318E   3C                   ENDIF
110700980318X   2C                   ELSE
110800980318IF  3C     V1CRDL        IFGT      *ZEROS
110900980318     C                   MOVEL     '1'           $ERR
111000980318     C                   MOVEL     MSG(39)       $MSG
111100980318     C                   EXSR      CARMSG
111200980318     C                   SETON                                        81
111300980318E   3C                   ENDIF
111400980318E   2C                   ENDIF
111500980318E   1C                   ENDIF
111600000000     C*--------------------
111700000000     C* CODICE COMPETENZA
111800000000     C*--------------------
111900950203     C* OBBLIGATORIA SOLO SE TIPO VOCE = 'V'
112000950203IF  1C     $ERR          IFEQ      '0'
112100980318IF  2C     V1CTVO        IFEQ      'V'
112200980318IF  3C     V1CCMP        IFEQ      *BLANKS
112300950203     C     V1CCMP        OREQ      *ZEROS
112400950203     C                   MOVEL     '1'           $ERR
112500950203     C                   MOVEL     MSG(9)        $MSG
112600950203     C                   EXSR      CARMSG
112700950203     C                   SETON                                        66
112800980318E   3C                   ENDIF
112900980318X   2C                   ELSE
113000980318IF  3C     V1CCMP        IFNE      *BLANKS
113100980318     C                   MOVEL     '1'           $ERR
113200980318     C                   MOVEL     MSG(38)       $MSG
113300980318     C                   EXSR      CARMSG
113400980318     C                   SETON                                        66
113500980318E   3C                   ENDIF
113600980318E   2C                   ENDIF
113700950203E   1C                   ENDIF
113800950203     C*
113900000000IF  1C     $ERR          IFEQ      '0'
114000950203     C     V1CCMP        ANDGT     *BLANKS
114100000000     C*
114200000000     C* PER "PUNTO INTERROGATIVO" CHIAMO IL PROGRAMMA DI GESTIONE COMPETENZE
114300000000     C     '?'           SCAN      V1CCMP                                 99
114400000000IF  2C     *IN99         IFEQ      *ON
114500000000     C                   Z-ADD     D60VOC        WVOC
114600000000     C                   Z-ADD     D60DDV        WDDV
114700000000     C                   MOVEL     D60OP0        WOP0
114800000000     C                   MOVEL     D60OP1        $WOP1             3
114900000000     C                   MOVEL     'O01'         WOP1
115000000000     C                   MOVEL     'TREC72R'     WPGM
115100000000     C                   EXSR      LANPGM
115200000000     C                   MOVEL     $WOP1         D60OP1
115300000000     C                   MOVEL     D60CMP        V1CCMP
115400000000E   2C                   ENDIF
115500000000     C*
115600000000     C                   MOVEL     V1CCMP        KCTCMP
115700040128     C                   Z-ADD     V1CDSV        G02DAT
115800040128     C                   Z-ADD     *ZEROS        G02INV
115900040128     C                   MOVEL     '0'           G02ERR
116000040128     C                   CALL      'XSRDA8'
116100040128     C                   PARM                    WLBDAT
116200040128     C                   Z-ADD     G02INV        kctddc
116300040127     C     KEYECT        SETGT     ECECT02L
116400040128     C     Keyect1       READPE    ECECT02L                               66
116500040127     C                   MOVE      *BLANKS       V1CCMD
116600040127IF  2C     *IN66         IFEQ      *ON
116700040127     C     ECTATB        ORNE      *BLANKS
116800040127     C                   MOVEL     '1'           $ERR
116900040127     C                   MOVEL     MSG(9)        $MSG
117000040127     C                   EXSR      CARMSG
117100040127X   2C                   ELSE
117200040127     C                   MOVEL     ECTCMD        V1CCMD
117300040127E   2C                   ENDIF
117400000000     C*
117500000000E   1C                   ENDIF
117600000000     C*--------------------
117700000000     C* RIFERIMENTO TARIFFA FILIALE ITALIA
117800000000     C*--------------------
117900000000IF  1C     $ERR          IFEQ      '0'
118000000000     C                   MOVEL     *BLANKS       V1DRFI
118100000000IF  2C     V1CRFI        IFNE      *BLANKS
118200000000     C                   MOVE      'F'           §TTV
118300000000     C                   MOVEL     *BLANKS       §KEY
118400000000     C                   MOVEL     V1CRFI        §KEY
118500000000     C                   EXSR      CHKRIT
118600000000     C                   MOVEL     §KEY          V1CRFI
118700000000     C                   MOVEL     §DES          V1DRFI
118800000000IF  3C     $ERR          IFEQ      '1'
118900000000     C                   MOVEL     MSG(25)       $MSG
119000000000     C                   EXSR      CARMSG
119100000000     C                   SETON                                        68
119200000000X   3C                   ELSE
119300000000     C                   MOVEL     TBLUNI        DS2Q
119400000000     C*
119500000000     C* MEMORIZZO IN CAMPO DI COMODO I FLAG DELLA TABELLA 2Q
119600000000     C     *LIKE         DEFINE    FLAGS         $FLAGS
119700000000     C                   MOVE      FLAGS         $FLAGS
119800000000     C*
119900000000IF  4C     §2QTTV        IFNE      'F'
120000000000     C                   MOVE      '1'           $ERR
120100000000     C                   MOVEL     MSG(26)       $MSG
120200000000     C                   EXSR      CARMSG
120300000000     C                   SETON                                        68
120400000000E   4C                   ENDIF
120500000000E   3C                   ENDIF
120600000000E   2C                   ENDIF
120700000000E   1C                   ENDIF
120800000000     C*--------------------
120900000000     C* RIFERIMENTO TARIFFA FILIALE ESTERO
121000000000     C*--------------------
121100000000IF  1C     $ERR          IFEQ      '0'
121200000000     C                   MOVEL     *BLANKS       V1DRFE
121300000000IF  2C     V1CRFE        IFNE      *BLANKS
121400000000     C                   MOVE      'F'           §TTV
121500000000     C                   MOVEL     *BLANKS       §KEY
121600000000     C                   MOVEL     V1CRFE        §KEY
121700000000     C                   EXSR      CHKRIT
121800000000     C                   MOVEL     §KEY          V1CRFE
121900000000     C                   MOVEL     §DES          V1DRFE
122000000000IF  3C     $ERR          IFEQ      '1'
122100000000     C                   MOVEL     MSG(25)       $MSG
122200000000     C                   EXSR      CARMSG
122300000000     C                   SETON                                        70
122400000000X   3C                   ELSE
122500000000     C                   MOVEL     TBLUNI        DS2Q
122600000000IF  4C     §2QTTV        IFNE      'F'
122700000000     C                   MOVE      '1'           $ERR
122800000000     C                   MOVEL     MSG(26)       $MSG
122900000000     C                   EXSR      CARMSG
123000000000     C                   SETON                                        70
123100000000E   4C                   ENDIF
123200000000     C*
123300000000     C* I FLAG DELLA TABELLA 2Q DEVONO CORRISPONDERE AGLI STESSI FLAG (SEMPRE
123400000000     C* DELLA TABELLA 2Q) DELL'ELEMENTO PRECEDENTE (QUELLO PER L'ITALIA)
123500000000IF  4C     FLAGS         IFNE      $FLAGS
123600000000     C     V1CRFI        ANDNE     *BLANKS
123700000000     C     V1CRFE        ANDNE     *BLANKS
123800000000     C                   MOVE      '1'           $ERR
123900000000     C                   MOVEL     MSG(28)       $MSG
124000000000     C                   EXSR      CARMSG
124100000000     C                   SETON                                        6870
124200000000E   4C                   ENDIF
124300000000     C*
124400000000E   3C                   ENDIF
124500000000E   2C                   ENDIF
124600000000E   1C                   ENDIF
124700000000     C*--------------------
124800000000     C* RIFERIMENTO TARIFFA TASSAZIONE ITALIA
124900000000     C*--------------------
125000000000IF  1C     $ERR          IFEQ      '0'
125100000000     C                   MOVEL     *BLANKS       V1DRTI
125200000000IF  2C     V1CRTI        IFNE      *BLANKS
125300000000     C                   MOVE      'T'           §TTV
125400000000     C                   MOVEL     *BLANKS       §KEY
125500000000     C                   MOVEL     V1CRTI        §KEY
125600000000     C                   EXSR      CHKRIT
125700000000     C                   MOVEL     §KEY          V1CRTI
125800000000     C                   MOVEL     §DES          V1DRTI
125900000000IF  3C     $ERR          IFEQ      '1'
126000000000     C                   MOVEL     MSG(25)       $MSG
126100000000     C                   EXSR      CARMSG
126200000000     C                   SETON                                        71
126300000000X   3C                   ELSE
126400000000     C                   MOVEL     TBLUNI        DS2Q
126500000000     C*
126600000000     C* MEMORIZZO IN CAMPO DI COMODO I FLAG DELLA TABELLA 2Q
126700000000     C                   MOVE      FLAGS         $FLAGS
126800000000     C*
126900000000IF  4C     §2QTTV        IFNE      'T'
127000000000     C                   MOVE      '1'           $ERR
127100000000     C                   MOVEL     MSG(26)       $MSG
127200000000     C                   EXSR      CARMSG
127300000000     C                   SETON                                        71
127400000000E   4C                   ENDIF
127500000000E   3C                   ENDIF
127600000000E   2C                   ENDIF
127700000000E   1C                   ENDIF
127800000000     C*--------------------
127900000000     C* RIFERIMENTO TARIFFA TASSAZIONE ESTERO
128000000000     C*--------------------
128100000000IF  1C     $ERR          IFEQ      '0'
128200000000     C                   MOVEL     *BLANKS       V1DRTE
128300000000IF  2C     V1CRTE        IFNE      *BLANKS
128400000000     C                   MOVE      'T'           §TTV
128500000000     C                   MOVEL     *BLANKS       §KEY
128600000000     C                   MOVEL     V1CRTE        §KEY
128700000000     C                   EXSR      CHKRIT
128800000000     C                   MOVEL     §KEY          V1CRTE
128900000000     C                   MOVEL     §DES          V1DRTE
129000000000IF  3C     $ERR          IFEQ      '1'
129100000000     C                   MOVEL     MSG(25)       $MSG
129200000000     C                   EXSR      CARMSG
129300000000     C                   SETON                                        72
129400000000X   3C                   ELSE
129500000000     C                   MOVEL     TBLUNI        DS2Q
129600000000IF  4C     §2QTTV        IFNE      'T'
129700000000     C                   MOVE      '1'           $ERR
129800000000     C                   MOVEL     MSG(26)       $MSG
129900000000     C                   EXSR      CARMSG
130000000000     C                   SETON                                        72
130100000000E   4C                   ENDIF
130200000000     C*
130300000000     C* I FLAG DELLA TABELLA 2Q DEVONO CORRISPONDERE AGLI STESSI FLAG (SEMPRE
130400000000     C* DELLA TABELLA 2Q) DELL'ELEMENTO PRECEDENTE (QUELLO PER L'ITALIA)
130500000000IF  4C     FLAGS         IFNE      $FLAGS
130600000000     C     V1CRTI        ANDNE     *BLANKS
130700000000     C     V1CRTE        ANDNE     *BLANKS
130800000000     C                   MOVE      '1'           $ERR
130900000000     C                   MOVEL     MSG(28)       $MSG
131000000000     C                   EXSR      CARMSG
131100000000     C                   SETON                                        7172
131200000000E   4C                   ENDIF
131300000000     C*
131400000000E   3C                   ENDIF
131500000000E   2C                   ENDIF
131600000000E   1C                   ENDIF
131700000000     C*--------------------
131800000000     C* RIFERIMENTO TARIFFA C.A.P. ITALIA
131900000000     C*--------------------
132000000000IF  1C     $ERR          IFEQ      '0'
132100000000     C                   MOVEL     *BLANKS       V1DRPI
132200000000IF  2C     V1CRPI        IFNE      *BLANKS
132300000000     C                   MOVE      'P'           §TTV
132400000000     C                   MOVEL     *BLANKS       §KEY
132500000000     C                   MOVEL     V1CRPI        §KEY
132600000000     C                   EXSR      CHKRIT
132700000000     C                   MOVEL     §KEY          V1CRPI
132800000000     C                   MOVEL     §DES          V1DRPI
132900000000IF  3C     $ERR          IFEQ      '1'
133000000000     C                   MOVEL     MSG(25)       $MSG
133100000000     C                   EXSR      CARMSG
133200000000     C                   SETON                                        73
133300000000X   3C                   ELSE
133400000000     C                   MOVEL     TBLUNI        DS2Q
133500000000     C*
133600000000     C* MEMORIZZO IN CAMPO DI COMODO I FLAG DELLA TABELLA 2Q
133700000000     C                   MOVE      FLAGS         $FLAGS
133800000000     C*
133900000000IF  4C     §2QTTV        IFNE      'P'
134000000000     C                   MOVE      '1'           $ERR
134100000000     C                   MOVEL     MSG(26)       $MSG
134200000000     C                   EXSR      CARMSG
134300000000     C                   SETON                                        73
134400000000E   4C                   ENDIF
134500000000E   3C                   ENDIF
134600000000E   2C                   ENDIF
134700000000E   1C                   ENDIF
134800000000     C*--------------------
134900000000     C* RIFERIMENTO TARIFFA C.A.P. ESTERO
135000000000     C*--------------------
135100000000IF  1C     $ERR          IFEQ      '0'
135200000000     C                   MOVEL     *BLANKS       V1DRPE
135300000000IF  2C     V1CRPE        IFNE      *BLANKS
135400000000     C                   MOVE      'P'           §TTV
135500000000     C                   MOVEL     *BLANKS       §KEY
135600000000     C                   MOVEL     V1CRPE        §KEY
135700000000     C                   EXSR      CHKRIT
135800000000     C                   MOVEL     §KEY          V1CRPE
135900000000     C                   MOVEL     §DES          V1DRPE
136000000000IF  3C     $ERR          IFEQ      '1'
136100000000     C                   MOVEL     MSG(25)       $MSG
136200000000     C                   EXSR      CARMSG
136300000000     C                   SETON                                        74
136400000000X   3C                   ELSE
136500000000     C                   MOVEL     TBLUNI        DS2Q
136600000000IF  4C     §2QTTV        IFNE      'P'
136700000000     C                   MOVE      '1'           $ERR
136800000000     C                   MOVEL     MSG(26)       $MSG
136900000000     C                   EXSR      CARMSG
137000000000     C                   SETON                                        74
137100000000E   4C                   ENDIF
137200000000     C*
137300000000     C* I FLAG DELLA TABELLA 2Q DEVONO CORRISPONDERE AGLI STESSI FLAG (SEMPRE
137400000000     C* DELLA TABELLA 2Q) DELL'ELEMENTO PRECEDENTE (QUELLO PER L'ITALIA)
137500000000IF  4C     FLAGS         IFNE      $FLAGS
137600000000     C     V1CRPI        ANDNE     *BLANKS
137700000000     C     V1CRPE        ANDNE     *BLANKS
137800000000     C                   MOVE      '1'           $ERR
137900000000     C                   MOVEL     MSG(28)       $MSG
138000000000     C                   EXSR      CARMSG
138100000000     C                   SETON                                        7374
138200000000E   4C                   ENDIF
138300000000     C*
138400000000E   3C                   ENDIF
138500000000E   2C                   ENDIF
138600000000E   1C                   ENDIF
138700000000     C*--------------------
138800000000     C* RIFERIMENTO TARIFFA CLIENTE ITALIA
138900000000     C*--------------------
139000000000IF  1C     $ERR          IFEQ      '0'
139100000000     C                   MOVEL     *BLANKS       V1DRKI
139200000000IF  2C     V1CRKI        IFNE      *BLANKS
139300000000     C                   MOVE      'K'           §TTV
139400000000     C                   MOVEL     *BLANKS       §KEY
139500000000     C                   MOVEL     V1CRKI        §KEY
139600000000     C                   EXSR      CHKRIT
139700000000     C                   MOVEL     §KEY          V1CRKI
139800000000     C                   MOVEL     §DES          V1DRKI
139900000000IF  3C     $ERR          IFEQ      '1'
140000000000     C                   MOVEL     MSG(25)       $MSG
140100000000     C                   EXSR      CARMSG
140200000000     C                   SETON                                        75
140300000000X   3C                   ELSE
140400000000     C                   MOVEL     TBLUNI        DS2Q
140500000000     C*
140600000000     C* MEMORIZZO IN CAMPO DI COMODO I FLAG DELLA TABELLA 2Q
140700000000     C                   MOVE      FLAGS         $FLAGS
140800000000     C*
140900000000IF  4C     §2QTTV        IFNE      'K'
141000000000     C                   MOVE      '1'           $ERR
141100000000     C                   MOVEL     MSG(26)       $MSG
141200000000     C                   EXSR      CARMSG
141300000000     C                   SETON                                        75
141400000000E   4C                   ENDIF
141500000000E   3C                   ENDIF
141600000000E   2C                   ENDIF
141700000000E   1C                   ENDIF
141800000000     C*--------------------
141900000000     C* RIFERIMENTO TARIFFA CLIENTE ESTERO
142000000000     C*--------------------
142100000000IF  1C     $ERR          IFEQ      '0'
142200000000     C                   MOVEL     *BLANKS       V1DRKE
142300000000IF  2C     V1CRKE        IFNE      *BLANKS
142400000000     C                   MOVE      'K'           §TTV
142500000000     C                   MOVEL     *BLANKS       §KEY
142600000000     C                   MOVEL     V1CRKE        §KEY
142700000000     C                   EXSR      CHKRIT
142800000000     C                   MOVEL     §KEY          V1CRKE
142900000000     C                   MOVEL     §DES          V1DRKE
143000000000IF  3C     $ERR          IFEQ      '1'
143100000000     C                   MOVEL     MSG(25)       $MSG
143200000000     C                   EXSR      CARMSG
143300000000     C                   SETON                                        76
143400000000X   3C                   ELSE
143500000000     C                   MOVEL     TBLUNI        DS2Q
143600000000IF  4C     §2QTTV        IFNE      'K'
143700000000     C                   MOVE      '1'           $ERR
143800000000     C                   MOVEL     MSG(26)       $MSG
143900000000     C                   EXSR      CARMSG
144000000000     C                   SETON                                        76
144100000000E   4C                   ENDIF
144200000000     C*
144300000000     C* I FLAG DELLA TABELLA 2Q DEVONO CORRISPONDERE AGLI STESSI FLAG (SEMPRE
144400000000     C* DELLA TABELLA 2Q) DELL'ELEMENTO PRECEDENTE (QUELLO PER L'ITALIA)
144500000000IF  4C     FLAGS         IFNE      $FLAGS
144600000000     C     V1CRKI        ANDNE     *BLANKS
144700000000     C     V1CRKE        ANDNE     *BLANKS
144800000000     C                   MOVE      '1'           $ERR
144900000000     C                   MOVEL     MSG(28)       $MSG
145000000000     C                   EXSR      CARMSG
145100000000     C                   SETON                                        7576
145200000000E   4C                   ENDIF
145300000000     C*
145400000000E   3C                   ENDIF
145500000000E   2C                   ENDIF
145600000000E   1C                   ENDIF
145700000000     C*--------------------
145800000000     C* ARROTONDAMENTO FINO AL LIMITE E OLTRE IL LIMITE
145900940725     C* LO ACCETTO SOLO SE C'E' IL LIMITE
146000000000     C*--------------------
146100000000IF  1C     $ERR          IFEQ      '0'
146200940725     C*
146300940725IF  3C     V1CARL        IFGT      *ZEROS
146400940725     C*
146500940725IF  4C     V1CARF        IFLE      *ZERO
146600000000     C     V1CARF        ORGT      V1CARL
146700940725     C                   MOVE      '1'           $ERR
146800000000     C                   MOVEL     MSG(16)       $MSG
146900000000     C                   EXSR      CARMSG
147000000000     C                   SETON                                        78
147100940725E   4C                   ENDIF
147200940725IF  4C     V1CARO        IFLE      *ZERO
147300000000     C                   MOVE      '1'           $ERR
147400000000     C                   MOVEL     MSG(16)       $MSG
147500000000     C                   EXSR      CARMSG
147600000000     C                   SETON                                        79
147700940725E   4C                   ENDIF
147800940725X   3C                   ELSE
147900940725IF  4C     V1CARF        IFGT      *ZERO
148000940725     C                   MOVE      '1'           $ERR
148100940725     C                   MOVEL     MSG(33)       $MSG
148200940725     C                   EXSR      CARMSG
148300940725     C                   SETON                                        78
148400940725E   4C                   ENDIF
148500940725IF  4C     V1CARO        IFGT      *ZERO
148600940725     C                   MOVE      '1'           $ERR
148700940725     C                   MOVEL     MSG(33)       $MSG
148800940725     C                   EXSR      CARMSG
148900940725     C                   SETON                                        79
149000940725E   4C                   ENDIF
149100940725E   3C                   ENDIF
149200940725E   1C                   ENDIF
149300010112     C*--------------------
149400010112     C* LANCIO PGM DI VERIFICA TARIFFE PER CAMBIO DATA SCADENZA VOCE
149500010112     C*--------------------
149600010112     C     $ERR          IFEQ      '0'
149700010112     C     D60OP1        ANDEQ     'O02'
149800010112     C                   Z-ADD     V1CVOC        I61VOC
149900010112     C                   Z-ADD     V1CPRG        I61PRG
150000010112     C                   Z-ADD     DDVV1C        I61DAI
150100010112     C                   Z-ADD     DSVV1C        I61DAF
150200010112     C                   Z-ADD     *ZEROS        I61DRI
150300010112     C                   Z-ADD     *ZEROS        O61VOC
150400010112     C                   Z-ADD     *ZEROS        O61PRG
150500010112     C* MI MEMORIZZO L'OPZIONE DI ENTRATA
150600010112     C                   MOVEL     D60OP1        DEPOP1            3
150700010112     C* ED IMPOSTO L'OPZIONE PER IL LANCIO DI CONTROLLO
150800010112     C                   MOVEL     'CHK'         D60OP1
150900010112     C                   CALL      'TREC61R1'
151000010112     C                   PARM                    DS60
151100010112     C                   PARM                    DS61
151200010112     C* IN OGNI CASO RIPRISTINO IL VALORE DELLE OPZIONI
151300010112     C                   MOVEL     DEPOP1        D60OP1
151400010112     C* ... E VERIFICO L'ESITO
151500010112     C     D60ERR        IFEQ      '1'
151600010112     C                   MOVE      '1'           $ERR
151700010112     C                   MOVEL     MSG(40)       $MSG
151800010112     C                   EXSR      CARMSG
151900010112     C                   ENDIF
152000010112     C                   ENDIF
152100021209     C*--------------------
152200021209     C* TIPO DATA
152300021209     C*--------------------
152400021209     C     $ERR          IFEQ      '0'
152500021209     C                   CLEAR                   TIBS02DS
152600021209     C                   CLEAR                   DCO1
152700021209     C*
152800021209     C     '?'           SCAN      V1CDAT                                 30
152900021209     C*  Ricerca
153000021209     C                   EVAL      T02TLA = 'L'
153100021209     C                   IF        *IN30 = *ON
153200021209     C                   MOVEL     'R'           T02MOD
153300021209     C                   ELSE
153400021209     C                   MOVEL     'C'           T02MOD
153500021209     C                   MOVEL(P)  V1CDAT        T02KE1
153600021209     C                   ENDIF
153700021209     C                   MOVEL     'CO1'         T02COD
153800021209     C                   MOVEL     KNSIF         T02SIF
153900021209     C                   CALL      'TIBS02R'
154000021209     C                   PARM                    KPJBA
154100021209     C                   PARM                    TIBS02DS
154200021209     C
154300021209     C*
154400021209     C                   IF        T02ERR = *BLANKS
154500021209     C                   MOVEL     T02KE1        V1CDAT
154600021209     C                   MOVEL     T02UNI        DCO1
154700021209     C                   MOVEL     §CO1DES       V1DDAT
154800021209     C                   ELSE
154900021209     C                   MOVEL     '1'           $ERR
155000021209     C                   SETON                                        82
155100021209     C                   MOVEL     MSG(43)       $MSG
155200021220     C                   EXSR      CARMSG
155300021209     C                   ENDIF
155400021209     C                   ENDIF
155500031222     c* se non c'è errore controllo la congruenza del tipo data voce con
155600031222     c* il tipo data della competenza
155700040127     C                   Z-ADD     v1cdsv        G02dat
155800040127     C                   Z-ADD     *ZEROS        G02inv
155900040127     C                   MOVEL     '0'           G02ERR
156000040127     C                   CALL      'XSRDA8'
156100040127     C                   PARM                    WLBDAT
156200031222     C                   IF        $err = '0' and ectfdr <> v1cdat
156300040127     c                             and G02INV >= 20040100
156400031222     C                   MOVEL     '1'           $ERR
156500031222     C                   SETON                                        82
156600031222     C                   MOVEL     MSG(43)       $MSG
156700031222     C                   EXSR      CARMSG
156800031222     C                   ENDIF
156900040126     C                   ENDIF
157000000000     C*
157100000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
157200000000     C* CURSORE SUL CAMPO DEL CONTROL
157300000000IF  1C     $ERR          IFEQ      '1'
157400000000     C                   MOVEL     '0'           *IN43
157500000000X   1C                   ELSE
157600000000     C                   MOVEL     '1'           *IN43
157700000000E   1C                   ENDIF
157800010110     C*
157900000000     C                   ENDSR
158000000000     C*------------------------------------------------------------------------*
158100000000     C* CHKRIT - CONTROLLA UN ELEMENTO IN TABELLA
158200000000     C*          INPUT --> §TTV: TIPO TARIFFA
158300000000     C*                    $KEY: CODICE ELEMENTO
158400000000     C*------------------------------------------------------------------------*
158500000000     C     CHKRIT        BEGSR
158600000000     C*
158700000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
158800000000     C     '?'           SCAN      §KEY                                   99
158900000000     C     *IN99         IFEQ      *ON
159000020923     C                   MOVEL     1             §KUT
159100000000     C                   MOVE      *BLANKS       §DES
159200000000     C                   CALL      'TREC65R'
159300000000     C                   PARM                    §KUT              1 0
159400000000     C                   PARM                    §KEY              8
159500000000     C                   PARM                    §DES             30
159600000000     C                   PARM                    §TTV              1
159700000000     C                   MOVEL     '1'           $PUNTO            1
159800000000     C*
159900000000     C                   ENDIF
160000000000     C*
160100000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
160200020923     C                   MOVEL     1             KTBKUT
160300000000     C                   MOVEL     '2Q'          KTBCOD
160400000000     C                   MOVE      *BLANKS       KTBKEY
160500000000     C                   MOVEL     §KEY          KTBKEY
160600000000     C     KEYTAB        CHAIN     TABEL00F                           99
160700000000     C     *IN99         IFEQ      '1'
160800000000     C     TBLFLG        ORNE      *BLANKS
160900000000     C                   MOVEL     '1'           $ERR
161000000000     C                   MOVEL     *ALL'*'       §DES
161100000000     C                   ELSE
161200000000     C                   MOVEL     TBLUNI        §DES
161300000000     C                   ENDIF
161400000000     C*
161500000000     C*
161600000000     C                   ENDSR
161700000000     C*------------------------------------------------------------------------*
161800000000     C* CHKDAT - CONTROLLA LA DATA
161900000000     C*          INPUT --> $DAT:  DATA
162000000000     C*------------------------------------------------------------------------*
162100000000     C     CHKDAT        BEGSR
162200000000     C*
162300000000     C* OBBLIGATORIA
162400000000IF  1C     $DAT          IFEQ      *ZEROS
162500000000     C                   MOVEL     '1'           $ERR
162600000000     C                   MOVEL     MSG(8)        $MSG
162700000000X   1C                   ELSE
162800000000     C* CORRETTA
162900000000     C                   Z-ADD     $DAT          G02DAT
163000000000     C                   Z-ADD     *ZEROS        G02INV
163100000000     C                   MOVEL     '0'           G02ERR
163200000000     C                   CALL      'XSRDA8'
163300000000     C                   PARM                    WLBDAT
163400000000     C*
163500000000IF  2C     G02ERR        IFEQ      '1'
163600000000     C                   MOVEL     '1'           $ERR
163700000000     C                   MOVEL     MSG(8)        $MSG
163800000000     C                   ELSE
163900000000     C                   Z-ADD     G02DAT        $DAT
164000000000E   2C                   ENDIF
164100000000E   1C                   ENDIF
164200000000     C*
164300000000     C                   ENDSR
164400000000     C*------------------------------------------------------------------------*
164500000000     C* CHKVRV - CONTROLLA IL TIPO TARIFFA
164600000000     C*          INPUT --> $VRV : TIPO TARIFFA
164700000000     C*                    $DVRV: DECODIFICA TIPO TARIFFA
164800000000     C*------------------------------------------------------------------------*
164900000000     C     CHKVRV        BEGSR
165000000000     C*
165100000000     C* OBBLIGATORIO
165200000000IF  1C     $VRV          IFEQ      *BLANKS
165300000000     C                   MOVEL     '1'           $ERR
165400000000     C                   MOVEL     MSG(12)       $MSG
165500000000X   1C                   ELSE
165600000000     C*
165700000000     C* CONTROLLO SE E' STATO INSERITO '?'
165800000000     C                   SETOFF                                           99
165900000000     C     '?'           SCAN      $VRV                                   99
166000000000     C*
166100000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE VALORI
166200000000IF  2C     *IN99         IFEQ      '1'
166300020923     C                   Z-ADD     1             §KUT
166400000000     C                   MOVEL     '2R'          §COD
166500000000     C                   MOVEL     *BLANKS       §KEY
166600000000     C                   MOVEL     *BLANKS       §DES
166700000000     C                   CALL      'X§TABER'
166800000000     C                   PARM                    §KUT              1 0
166900000000     C                   PARM                    §COD              2
167000000000     C                   PARM                    §KEY              8
167100000000     C                   PARM                    §DES             30
167200000000     C*
167300000000     C                   MOVEL     §KEY          $VRV
167400000000     C                   MOVEL     §DES          $DVRV
167500000000     C                   MOVEL     '1'           $TESVD
167600000000E   2C                   ENDIF
167700000000     C*
167800000000     C* CONTROLLO CHE IL TIPO TARIFFA ESISTA IN TABELLA
167900000000     C                   CLEAR                   DS2R
168000020923     C                   Z-ADD     1             KTBKUT
168100000000     C                   MOVEL     '2R'          KTBCOD
168200000000     C                   MOVEL     *BLANKS       KTBKEY
168300000000     C                   MOVEL     $VRV          KTBKEY
168400000000     C     KEYTAB        CHAIN     TABEL00F                           99
168500000000IF  2C     *IN99         IFEQ      '1'
168600000000     C     TBLFLG        ORNE      *BLANKS
168700000000     C                   MOVEL     '1'           $ERR
168800000000     C                   MOVEL     MSG(12)       $MSG
168900000000X   2C                   ELSE
169000030423     c                   MOVEL     TBLUNI        DS2R
169100030423IF  3C     $TIPO         IFEQ      'VRV'
169200030423     C     §2RSRS        ANDEQ     'S'
169300030423     C                   MOVEL     '1'           $ERR
169400030423     C                   MOVEL     MSG(46)       $MSG
169500030423X   3C                   ELSE
169600000000     C                   MOVEL     TBLUNI        DS2R
169700000000     C                   MOVEL     §2RDES        $DVRV
169800030423E   3C                   ENDIF
169900030423E   2C                   ENDIF
170000000000E   1C                   ENDIF
170100000000     C*
170200000000     C                   ENDSR
170300100421     C*------------------------------------------------------------------------*
170400100421     C* CHK2O - CONTROLLA IL range di validità del codice voce
170500100421     C*------------------------------------------------------------------------*
170600100421     C     CHK2o         BEGSR
170700100421     C*
170800100421     C                   CLEAR                   DS2o
170900100421     C                   Z-ADD     1             KTBKUT
171000100421     C                   MOVEL     '2O'          KTBCOD
171100100421     C                   eval      ktbkey = v1ctvo + v1cvrv
171200100421     C     KEYTAB        CHAIN     TABEL00F                           99
171300100421IF  2C     *IN99         IFEQ      '1'
171400100421     C     TBLFLG        ORNE      *BLANKS
171500100421     C                   eval      ktbkey = v1ctvo
171600100421     C     KEYTAB        CHAIN     TABEL00F                           99
171700100421IF  2C     *IN99         IFEQ      '1'
171800100421     C     TBLFLG        ORNE      *BLANKS
171900100421     C                   MOVEL     '1'           $ERR
172000100421     C                   MOVEL     MSG(48)       $MSG
172100100421     c                   leavesr
172200100422X   3C                   End
172300100421X   3C                   End
172400100422     c* TROVATO
172500100421     c* verifico se il codice è nel range
172600100422     C                   MOVEL     TBLUNI        DS2o
172700100421     C                   if        §2ovocda <= v1cvoc and
172800100421     C                             §2ovoca >= v1cvoc
172900100422     c                   leavesr
173000100422     c                   end
173100100422     c* se il tipo voce è "M" tento anche con il cod. MI maggiorazioni x
173200100422     c* voci da isolare
173300100422     c                   if        v1ctvo = 'M'
173400100422     C                   eval      ktbkey = 'MI'
173500100422     C     KEYTAB        CHAIN     TABEL00F                           99
173600100422IF  2C     *IN99         IFEQ      '1'
173700100422     C     TBLFLG        ORNE      *BLANKS
173800100422     C                   MOVEL     '1'           $ERR
173900100422     C                   MOVEL     MSG(48)       $MSG
174000100422     c                   leavesr
174100100422X   3C                   End
174200100422     c* verifico se il codice è nel range
174300100422     C                   MOVEL     TBLUNI        DS2o
174400100422     C                   if        §2ovocda <= v1cvoc and
174500100422     C                             §2ovoca >= v1cvoc
174600100422     c                   leavesr
174700100422     c                   end
174800100422E   2C                   ENDIF
174900100421     C                   MOVEL     '1'           $ERR
175000100421     C                   MOVEL     MSG(48)       $MSG
175100100421     C                   ENDSR
175200970129     C*------------------------------------------------------------------------*
175300970129     C* CHKRDL - CONTROLLA IL RAGGRUPPAMENTO DELTA
175400970129     C*          INPUT --> $RDL : RAGGR.DELTA
175500970129     C*                    $DRDL: DECODIFICA TIPO TARIFFA
175600970129     C*------------------------------------------------------------------------*
175700970129     C     CHKRDL        BEGSR
175800970129     C*
175900970129     C* OBBLIGATORIO
176000970129IF  1C     $RDL          IFEQ      *BLANKS
176100970129     C                   MOVEL     '1'           $ERR
176200970129     C                   MOVEL     MSG(37)       $MSG
176300970129X   1C                   ELSE
176400970129     C*
176500970129     C* CONTROLLO SE E' STATO INSERITO '?'
176600970129     C                   SETOFF                                           99
176700970129     C     '?'           SCAN      $RDL                                   99
176800970129     C*
176900970129     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE VALORI
177000970129IF  2C     *IN99         IFEQ      '1'
177100020923     C                   Z-ADD     1             §KUT
177200970129     C                   MOVEL     '9B'          §COD
177300970129     C                   MOVEL     *BLANKS       §KEY
177400970129     C                   MOVEL     *BLANKS       §DES
177500970129     C                   CALL      'X§TABER'
177600970129     C                   PARM                    §KUT              1 0
177700970129     C                   PARM                    §COD              2
177800970129     C                   PARM                    §KEY              8
177900970129     C                   PARM                    §DES             30
178000970129     C*
178100970129     C                   MOVEL     §KEY          $RDL
178200970129     C                   MOVEL     §DES          $DRDL
178300970129     C                   MOVEL     '1'           $TESVD
178400970129E   2C                   ENDIF
178500970129     C*
178600970129     C* CONTROLLO CHE IL RAGGR.DELTA  ESISTA IN TABELLA
178700970129     C                   CLEAR                   DS9B
178800020923     C                   Z-ADD     1             KTBKUT
178900970129     C                   MOVEL     '9B'          KTBCOD
179000970129     C                   MOVEL     *BLANKS       KTBKEY
179100970129     C                   MOVEL     $RDL          KTBKEY
179200970129     C     KEYTAB        CHAIN     TABEL00F                           99
179300970129IF  2C     *IN99         IFEQ      '1'
179400970129     C     TBLFLG        ORNE      *BLANKS
179500970129     C                   MOVEL     '1'           $ERR
179600970129     C                   MOVEL     MSG(37)       $MSG
179700970129X   2C                   ELSE
179800970129     C                   MOVEL     TBLUNI        DS9B
179900970129     C                   MOVEL     §9BDES        $DRDL
180000970129E   2C                   ENDIF
180100970129E   1C                   ENDIF
180200970129     C*
180300970129     C                   ENDSR
180400000000     C*------------------------------------------------------------------------*
180500000000     C*  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
180600000000     C*------------------------------------------------------------------------*
180700000000     C     RECVID        BEGSR
180800000000     C*
180900000000     C                   Z-ADD     D60VOC        V1CVOC
181000000000     C                   Z-ADD     D60PRG        V1CPRG
181100000000     C                   Z-ADD     D60DDV        V1CDDV
181200000000     C                   Z-ADD     EVTDSV        G02INV
181300000000     C                   Z-ADD     *ZEROS        G02DAT
181400000000     C                   MOVEL     '3'           G02ERR
181500000000     C                   CALL      'XSRDA8'
181600000000     C                   PARM                    WLBDAT
181700000000     C                   Z-ADD     G02DAT        V1CDSV
181800000000     C                   MOVEL     EVTVOD        V1CVOD
181900000000     C                   MOVEL     EVTTVO        V1CTVO
182000000000     C                   MOVEL     EVTVRV        V1CVRV
182100000000     C                   MOVEL     EVTTIV        V1CTIV
182200030422     C                   MOVEL(P)  EVTXXX        DEVTXXX
182300030422     C                   movel     evtsps        v1csps
182400030422     C                   z-add     evtrpv        V1Crpv
182500030422     C                   MOVEL     EVTSPT        V1CSPT
182600000000     C                   MOVEL     EVTRFI        V1CRFI
182700000000     C                   MOVEL     EVTRFE        V1CRFE
182800000000     C                   MOVEL     EVTRTI        V1CRTI
182900000000     C                   MOVEL     EVTRTE        V1CRTE
183000000000     C                   MOVEL     EVTRPI        V1CRPI
183100000000     C                   MOVEL     EVTRPE        V1CRPE
183200000000     C                   MOVEL     EVTRKI        V1CRKI
183300000000     C                   MOVEL     EVTRKE        V1CRKE
183400010115     C                   MOVEL     EVTCMP        V1CCMP
183500950203     C     V1CCMP        IFEQ      *ZEROS
183600950203     C                   MOVEL     *BLANKS       V1CCMP
183700950203     C                   ENDIF
183800000000     C                   Z-ADD     EVTARF        V1CARF
183900000000     C                   Z-ADD     EVTARL        V1CARL
184000000000     C                   Z-ADD     EVTARO        V1CARO
184100970129     C                   MOVEL     EVTRDL        V1CRDL
184200021209     C                   MOVEL     EVTCOP        V1CCOP
184300021209     C                   MOVEL     EVTDAT        V1CDAT
184400021209     C                   MOVEL     EVTAID        V1CAID
184500981015IF  1C     EVTATB        IFNE      *BLANKS
184600981015     C                   MOVEL     DOPZ(6)       V1CATB
184700981015E   1C                   ENDIF
184800010115     C*
184900010115     C* SALVO IL CONTENUTO INIZIALE DEI CAMPI PER EFFETTUARE IL TEST SU
185000010115     C* EVENTUALI CAMBIAMENTI DEI DATI DI CORPO
185100010115     C                   MOVEL     EVTTVO        SAVTVO
185200010115     C                   MOVEL     EVTVRV        SAVVRV
185300010115     C                   MOVEL     EVTTIV        SAVTIV
185400010115     C                   MOVEL     EVTSPT        SAVSPT
185500030422     C                   MOVEL(P)  EVTXXX        DEVTXXX
185600030422     C                   movel     evtsps        savsps
185700030422     C                   z-add     evtrpv        savrpv
185800010115     C                   MOVEL     EVTRFI        SAVRFI
185900010115     C                   MOVEL     EVTRFE        SAVRFE
186000010115     C                   MOVEL     EVTRTI        SAVRTI
186100010115     C                   MOVEL     EVTRTE        SAVRTE
186200010115     C                   MOVEL     EVTRPI        SAVRPI
186300010115     C                   MOVEL     EVTRPE        SAVRPE
186400010115     C                   MOVEL     EVTRKI        SAVRKI
186500010115     C                   MOVEL     EVTRKE        SAVRKE
186600010115     C                   MOVEL     EVTCMP        SAVCMP
186700010115     C     SAVCMP        IFEQ      *ZEROS
186800010115     C                   MOVEL     *BLANKS       SAVCMP
186900010115     C                   ENDIF
187000010115     C                   Z-ADD     EVTARF        SAVARF
187100010115     C                   Z-ADD     EVTARL        SAVARL
187200010115     C                   Z-ADD     EVTARO        SAVARO
187300010115     C                   MOVEL     EVTRDL        SAVRDL
187400000000     C*
187500010115     C* PER ESSERE MENO INTRUSIVO POSSIBILE GESTISCO LA GESTIONE DELLA
187600010115     C* DUPLICA SOLO QUI IN FONDO
187700010110     C     D60OP1        IFEQ      'O03'
187800010110     C                   Z-ADD     *ZEROS        V1CVOC
187900010110     C                   Z-ADD     *ZEROS        V1CPRG
188000010110     C                   Z-ADD     *ZEROS        V1CDDV
188100010110     C                   Z-ADD     *ZEROS        V1CDSV
188200010110     C                   MOVEL     *BLANKS       V1CVOD
188300010110     C                   ENDIF
188400010110     C*
188500000000     C                   ENDSR
188600000000     C*------------------------------------------------------------------------*
188700000000     C*  AZZVID - PULISCE TUTTI ICAMPI DEL VIDEO
188800000000     C*------------------------------------------------------------------------*
188900000000     C     AZZVID        BEGSR
189000000000     C*
189100000000     C                   Z-ADD     *ZEROS        V1CVOC
189200000000     C                   Z-ADD     *ZERO         V1CPRG
189300000000     C                   Z-ADD     *ZEROS        V1CDDV
189400000000     C                   Z-ADD     *ZEROS        V1CDSV
189500000000     C                   MOVEL     *BLANKS       V1CVOD
189600000000     C                   MOVEL     *BLANKS       V1CTVO
189700000000     C                   MOVEL     *BLANKS       V1CVRV
189800000000     C                   MOVEL     *BLANKS       V1CTIV
189900000000     C                   MOVEL     *BLANKS       V1CSPT
190000030422     C                   Z-ADD     *ZEROS        V1CRPV
190100030422     C                   MOVEL     *BLANKS       V1CSPS
190200950203     C                   MOVEL     *BLANKS       V1CCMP
190300000000     C                   MOVEL     *BLANKS       V1CRFI
190400000000     C                   MOVEL     *BLANKS       V1CRFE
190500000000     C                   MOVEL     *BLANKS       V1CRTI
190600000000     C                   MOVEL     *BLANKS       V1CRTE
190700000000     C                   MOVEL     *BLANKS       V1CRPI
190800000000     C                   MOVEL     *BLANKS       V1CRPE
190900000000     C                   MOVEL     *BLANKS       V1CRKI
191000000000     C                   MOVEL     *BLANKS       V1CRKE
191100000000     C                   Z-ADD     *ZERO         V1CARF
191200000000     C                   Z-ADD     *ZERO         V1CARL
191300000000     C                   Z-ADD     *ZERO         V1CARO
191400970129     C                   MOVEL     *BLANKS       V1CCMP
191500970129     C                   MOVEL     *BLANKS       V1CRDL
191600981015     C                   MOVEL     *BLANKS       V1CATB
191700021209     C                   MOVEL     *BLANKS       V1CCOP
191800021209     C                   MOVEL     *BLANKS       V1CDAT
191900021209     C                   MOVEL     *BLANKS       V1CAID
192000000000     C*
192100000000     C                   ENDSR
192200000000     C*------------------------------------------------------------------------*
192300000000     C* LANPGM - LANCIA UN PROGRAMMA
192400000000     C*------------------------------------------------------------------------*
192500000000     C     LANPGM        BEGSR
192600000000     C*--------------------
192700000000     C* PARAMETRI INFORMATIVI
192800000000     C*--------------------
192900000000     C                   MOVEL     WOP0          D60OP0
193000000000     C                   MOVEL     WOP1          D60OP1
193100000000     C                   MOVEL     '0'           D60F03
193200010110     C                   MOVEL     '0'           D60ERR
193300000000     C                   MOVEL     *BLANKS       D60MSG
193400000000     C*--------------------
193500000000     C* PARAMETRI
193600000000     C*--------------------
193700000000     C                   Z-ADD     WVOC          D60VOC
193800000000     C                   Z-ADD     WPRG          D60PRG
193900000000     C                   Z-ADD     WDDV          D60DDV
194000000000     C                   MOVEL     WTTV          D60TTV
194100000000     C                   Z-ADD     WFT1          D60FT1
194200000000     C                   Z-ADD     WFT2          D60FT2
194300000000     C                   Z-ADD     WCLI          D60CLI
194400000000     C                   Z-ADD     WCTR          D60CTR
194500000000     C                   MOVE      WCTS          D60CTS
194600131002     C                   MOVEl     Wnaz          d60naz
194700131002     C                   MOVEl     WCad          d60cad
194800000000     C                   Z-ADD     WCAP          D60CAP
194900000000     C                   Z-ADD     WDDT          D60DDT
195000000000     C                   Z-ADD     WSGI          D60SGI
195100000000     C                   Z-ADD     WVOM          D60VOM
195200000000     C                   Z-ADD     WPRM          D60PRM
195300000000     C                   Z-ADD     WCMP          D60CMP
195400000000     C                   Z-ADD     WNBC          D60NBC
195500000000     C                   Z-ADD     WDDC          D60DDC
195600000000     C                   Z-ADD     WKCC          D60KCC
195700000000     C                   Z-ADD     WKSC          D60KSC
195800000000     C                   MOVE      WRIC          D60RIC
195900000000     C                   Z-ADD     WDDO          D60DDO
196000940719     C                   MOVE      WFRA          D60FRA
196100940725     C                   MOVE      WIOE          D60IOE
196200940719     C                   Z-ADD     WCCF          D60CCF
196300000000     C* LANCIO
196400000000     C                   MOVEL     DS60          KPJBU
196500000000     C                   CALL      WPGM                                 99
196600000000     C                   PARM                    KPJBA
196700000000     C                   MOVEL     KPJBU         DS60
196800000000     C*
196900000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
197000000000IF  1C     D60F03        IFEQ      '1'
197100000000     C                   MOVEL     '1'           $F03
197200000000     C                   MOVEL     '1'           $ERR
197300000000     C                   MOVEL     '1'           $FINE
197400000000     C*
197500000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
197600000000X   1C                   ELSE
197700000000     C                   MOVEL     D60MSG        $MSG
197800000000     C                   EXSR      CARMSG
197900000000E   1C                   ENDIF
198000000000     C*
198100000000     C                   ENDSR
198200020923     C*-------------------------------------------------------------------------
198300020923     C* REPERISCE I DATI UTENTE
198400020923     C*-------------------------------------------------------------------------
198500020923     C     REPDATIUTE    BEGSR
198600020923     C*
198700020923     C* INIZIALIZZA VARIABILI DI WRK
198800020923     C                   CLEAR                   TIBS34DS
198900020923     C                   CLEAR                   AZUTEDS
199000020923     C                   CLEAR                   DDATIUTE
199100020923     C*
199200020923     C     *DTAARA       DEFINE    §azute        azuteds
199300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
199400020923     C                   IN(E)     *DTAARA
199500020923if  1C                   IF        %Error
199600020923     c                   EVAL      I34Tla = 'L'
199700020923     C                   CALL      'TIBS34R'
199800020923     C                   PARM                    Tibs34Ds
199900020923     C                   IN        *DTAARA
200000020923e   1C                   ENDIF
200100030117     C*
200200030117     C* VERIFICO SE TRATTASI DI UTENTE EDP DI SEDE
200300030117     C                   IF        INTGES = '02' AND
200400040518IF  1C                             D60OP1 <> 'F06' and
200500030117     C                             UTEFUN <> 'EDPSE'
200600030117     C                   SETON                                        92
200700030117     C                   ELSE
200800030117     C                   SETOFF                                       92
200900030117     C                   ENDIF
201000020923     C*
201100020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
201200020923     C                   MOVEL(P)  RSUT          DSFIRS           20
201300020923     C*
201400020923     C                   ENDSR
201500000000     C*------------------------------------------------------------------------*
201600000000     C*  *INZSR - ROUTINE INIZIALE
201700000000     C*------------------------------------------------------------------------*
201800000000     C     *INZSR        BEGSR
201900000000     C*
202000000000     C* RICEVIMENTO PARAMETRI
202100000000     C     *ENTRY        PLIST
202200000000     C                   PARM                    KPJBA
202300000000     C                   MOVEL     KPJBU         DS60
202400040128     C* REPERISCO SUBITO L'ULTIMO BLOCCO (QUELLO VALIDO)
202500040128     C                   Z-ADD     *zeros        kctnbc
202600040128     C     *hival        SETGT     ECECT01L
202700040128     C                   READP     ECECT01L
202800040128     c                   if        not %eof(ecect01l)
202900040128     C                   Z-ADD     ECTNBC        kctnbc
203000040128     c                   end
203100000000     C*
203200000000     C* DEFINIZIONE VARIABILI
203300000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
203400000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
203500000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
203600000000     C     *LIKE         DEFINE    EVTVOC        KVTVOC
203700000000     C     *LIKE         DEFINE    EVTPRG        KVTPRG
203800000000     C     *LIKE         DEFINE    EVDVOC        KVDVOC
203900000000     C     *LIKE         DEFINE    EVDPRG        KVDPRG
204000000000     C     *LIKE         DEFINE    ECTCMP        KCTCMP
204100000000     C     *LIKE         DEFINE    ECTNBC        KCTNBC
204200040128     C     *LIKE         DEFINE    ECTddc        KCTddc
204300000000     C*
204400000000     C     *LIKE         DEFINE    D60OP0        WOP0
204500000000     C     *LIKE         DEFINE    D60OP1        WOP1
204600000000     C     *LIKE         DEFINE    D60VOC        WVOC
204700000000     C     *LIKE         DEFINE    D60PRG        WPRG
204800000000     C     *LIKE         DEFINE    D60DDV        WDDV
204900000000     C     *LIKE         DEFINE    D60TTV        WTTV
205000000000     C     *LIKE         DEFINE    D60FT1        WFT1
205100000000     C     *LIKE         DEFINE    D60FT2        WFT2
205200000000     C     *LIKE         DEFINE    D60CLI        WCLI
205300000000     C     *LIKE         DEFINE    D60CTR        WCTR
205400000000     C     *LIKE         DEFINE    D60CTS        WCTS
205500000000     C     *LIKE         DEFINE    D60CAP        WCAP
205600131002     C     *LIKE         DEFINE    D60CAd        WCAd
205700131002     C     *LIKE         DEFINE    D60naz        Wnaz
205800000000     C     *LIKE         DEFINE    D60DDT        WDDT
205900000000     C     *LIKE         DEFINE    D60SGI        WSGI
206000000000     C     *LIKE         DEFINE    D60VOM        WVOM
206100000000     C     *LIKE         DEFINE    D60PRM        WPRM
206200000000     C     *LIKE         DEFINE    D60CMP        WCMP
206300000000     C     *LIKE         DEFINE    D60NBC        WNBC
206400000000     C     *LIKE         DEFINE    D60DDC        WDDC
206500000000     C     *LIKE         DEFINE    D60KCC        WKCC
206600000000     C     *LIKE         DEFINE    D60KSC        WKSC
206700000000     C     *LIKE         DEFINE    D60RIC        WRIC
206800000000     C     *LIKE         DEFINE    D60DDO        WDDO
206900940719     C     *LIKE         DEFINE    D60FRA        WFRA
207000940725     C     *LIKE         DEFINE    D60IOE        WIOE
207100940719     C     *LIKE         DEFINE    D60CCF        WCCF
207200010115     C*
207300010115     C     *LIKE         DEFINE    V1CTVO        SAVTVO
207400010115     C     *LIKE         DEFINE    V1CVRV        SAVVRV
207500010115     C     *LIKE         DEFINE    V1CTIV        SAVTIV
207600010115     C     *LIKE         DEFINE    V1CSPT        SAVSPT
207700030422     C     *LIKE         DEFINE    v1csps        savsps
207800030422     C     *LIKE         DEFINE    v1crpv        savrpv
207900010115     C     *LIKE         DEFINE    V1CCMP        SAVCMP
208000010115     C     *LIKE         DEFINE    V1CRFI        SAVRFI
208100010115     C     *LIKE         DEFINE    V1CRFE        SAVRFE
208200010115     C     *LIKE         DEFINE    V1CRTI        SAVRTI
208300010115     C     *LIKE         DEFINE    V1CRTE        SAVRTE
208400010115     C     *LIKE         DEFINE    V1CRPI        SAVRPI
208500010115     C     *LIKE         DEFINE    V1CRPE        SAVRPE
208600010115     C     *LIKE         DEFINE    V1CRKI        SAVRKI
208700010115     C     *LIKE         DEFINE    V1CRKE        SAVRKE
208800010115     C     *LIKE         DEFINE    V1CARF        SAVARF
208900010115     C     *LIKE         DEFINE    V1CARL        SAVARL
209000010115     C     *LIKE         DEFINE    V1CARO        SAVARO
209100010115     C     *LIKE         DEFINE    V1CRDL        SAVRDL
209200021209     C     *LIKE         DEFINE    V1CCOP        SAVCOP
209300021209     C     *LIKE         DEFINE    V1CDAT        SAVDAT
209400021209     C     *LIKE         DEFINE    V1CAID        SAVAID
209500000000     C*
209600000000     C* CHIAVE LETTURA ECEVT01L - COMPLETA
209700000000     C     KEYEVT        KLIST
209800000000     C                   KFLD                    KVTVOC
209900000000     C                   KFLD                    KVTPRG
210000130924     C     Kevt          KLIST
210100130924     C                   KFLD                    evtVOC
210200130924     C                   KFLD                    evtPRG
210300130924     C     Knot          KLIST
210400130924     C                   KFLD                    tpana             1
210500130924     C                   KFLD                    tnota             1
210600130924     C                   KFLD                    evtVOC
210700130924     C                   KFLD                    evtPRG
210800130924     C                   move      'V'           tpana
210900000000     C*
211000000000     C* CHIAVE LETTURA ECEVT01L - PARZIALE
211100000000     C     KE2EVT        KLIST
211200000000     C                   KFLD                    KVTVOC
211300000000     C*
211400140114     C* CHIAVE LETTURA ECEVD06L - PARZIALE
211500000000     C     KEYEVD        KLIST
211600000000     C                   KFLD                    KVDVOC
211700000000     C                   KFLD                    KVDPRG
211800000000     C*
211900000000     C* CHIAVE LETTURA ECECT02L - PARZIALE
212000000000     C     KEYECT        KLIST
212100000000     C                   KFLD                    KCTCMP
212200000000     C                   KFLD                    KCTNBC
212300040128     C                   KFLD                    KCTddc
212400040128     C     KEYECT1       KLIST
212500040128     C                   KFLD                    KCTCMP
212600040128     C                   KFLD                    KCTNBC
212700000000     C*
212800000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
212900000000     C     KEYTAB        KLIST
213000000000     C                   KFLD                    KTBKUT
213100000000     C                   KFLD                    KTBCOD
213200000000     C                   KFLD                    KTBKEY
213300010808     C*
213400010808     C* IMPOSTA DATA E ORA CORRENTE
213500010808     C                   TIME                    N14              14 0
213600010808     C                   MOVE      N14           N8                8 0
213700010808     C                   Z-ADD     N8            G02DAT
213800010808     C                   Z-ADD     *ZEROS        G02INV
213900010808     C                   MOVEL     '0'           G02ERR
214000010808     C                   CALL      'XSRDA8'
214100010808     C                   PARM                    WLBDAT
214200010808     C                   Z-ADD     G02INV        DATCOR            8 0
214300010808     C                   MOVE      00            DATCO0            8 0
214400000000     C*
214500000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
214600000000     C                   MOVE      D60OP0        INTGES            2
214700000000IF  1C     INTGES        IFEQ      '02'
214800000000     C                   MOVEL     'G'           $TGES             1
214900000000X   1C                   ELSE
215000000000     C                   MOVEL     'I'           $TGES             1
215100000000E   1C                   ENDIF
215200000000     C*
215300000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
215400000000     C                   EXSR      INZSTD
215500000000     C*
215600000000     C* RAGIONE SOCIALE UTENTE
215700000000     C                   MOVEL     DSFIRS        VTDRSU
215800000000     C*
215900000000     C* DECODIFICA OPZIONE RICHIAMATA
216000000000IF  1C     D60OP1        IFEQ      'F06'
216100000000     C                   MOVEL     DOPZ(1)       VTDFUN
216200000000E   1C                   ENDIF
216300000000IF  2C     D60OP1        IFEQ      'O02'
216400000000     C                   MOVEL     DOPZ(2)       VTDFUN
216500000000E   1C                   ENDIF
216600010110IF  2C     D60OP1        IFEQ      'O03'
216700010110     C                   MOVEL     DOPZ(7)       VTDFUN
216800010110E   1C                   ENDIF
216900000000IF  1C     D60OP1        IFEQ      'O04'
217000000000     C                   MOVEL     DOPZ(3)       VTDFUN
217100000000E   1C                   ENDIF
217200000000IF  1C     D60OP1        IFEQ      'O07'
217300000000     C                   MOVEL     DOPZ(4)       VTDFUN
217400000000E   1C                   ENDIF
217500000000IF  1C     D60OP1        IFEQ      'O05'
217600000000     C                   MOVEL     DOPZ(5)       VTDFUN
217700000000E   1C                   ENDIF
217800000000     C*
217900000000     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
218000000000     C* 0=NON PROTETTO, 1=PROTETTO
218100000000     C*   90 - CAMPI CHIAVE
218200000000     C*   91 - ALTRI CAMPI
218300000000IF  1C     D60OP1        IFEQ      'F06'
218400010110     C     D60OP1        OREQ      'O03'
218500000000     C                   SETOFF                                       9091
218600000000E   1C                   ENDIF
218700000000IF  1C     D60OP1        IFEQ      'O02'
218800000000     C                   SETON                                        90
218900000000E   1C                   ENDIF
219000000000IF  1C     D60OP1        IFEQ      'O04'
219100000000     C     D60OP1        OREQ      'O07'
219200000000     C     D60OP1        OREQ      'O05'
219300000000     C                   SETON                                        9091
219400000000E   1C                   ENDIF
219500000000     C*
219600000000     C* SE VISUALIZZAZIONE TOLGO FUNZIONE F06=AGGIORNAMENTO
219700000000IF  1C     D60OP1        IFEQ      'O05'
219800000000     C                   MOVEL     FUN(6)        FUNRIG
219900000000     C                   MOVEL     'N'           FUNASG
220000000000     C                   MOVEL     'N'           FUNASI
220100000000     C                   MOVEL     'N'           FUNAFG
220200000000     C                   MOVEL     'N'           FUNAFG
220300000000     C                   MOVEL     FUNRIG        FUN(6)
220400000000E   1C                   ENDIF
220500000000     C*
220600010110     C* SE SONO IN MOD/DUPL/CANC/VISUAL/RIPR ==> LEGGO IL RECORD
220700000000IF  1C     D60OP1        IFEQ      'O02'
220800010110     C     D60OP1        OREQ      'O03'
220900000000     C     D60OP1        OREQ      'O04'
221000000000     C     D60OP1        OREQ      'O05'
221100000000     C     D60OP1        OREQ      'O07'
221200000000     C                   Z-ADD     D60VOC        KVTVOC
221300000000     C                   Z-ADD     D60PRG        KVTPRG
221400000000     C*
221500000000     C* SE IL RECORD NON ESISTE O E' ALLOCATO MANDO ERRORE AL PGM CHIAMANTE
221600000000     C     KEYEVT        CHAIN     ECEVT01L                           9998
221700000000IF  2C     *IN99         IFEQ      '1'
221800950203     C                   MOVEL     '1'           $ERR
221900000000     C                   MOVEL     '1'           $FINE
222000000000     C                   MOVEL     MSG(21)       D60MSG
222100000000X   2C                   ELSE
222200000000IF  3C     *IN98         IFEQ      '1'
222300000000     C                   MOVEL     '1'           $ERR
222400000000     C                   MOVEL     '1'           $FINE
222500000000     C                   MOVEL     MSG(22)       D60MSG
222600000000X   3C                   ELSE
222700000000     C*
222800000000     C                   EXSR      RECVID
222900000000     C                   EXSR      CARV1
223000000000E   3C                   ENDIF
223100000000E   2C                   ENDIF
223200000000E   1C                   ENDIF
223300000000     C*
223400000000     C* SE INSERIMENTO
223500000000IF  1C     D60OP1        IFEQ      'F06'
223600000000     C                   EXSR      AZZVID
223700000000     C                   EXSR      CARV1
223800000000     C                   ELSE
223900000000     C                   EXSR      DECODI
224000000000E   1C                   ENDIF
224100000000     C*
224200000000     C                   ENDSR
224300000000     C*------------------------------------------------------------------------*
224400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
224500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
224600000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
224700000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
224800000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
224900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
225000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
225100000000     C*------------------------------------------------------------------------*
225200000000     C* INZSTD - ROUTINE INIZIALE STANDARD
225300000000     C*------------------------------------------------------------------------*
225400000000     C     INZSTD        BEGSR
225500000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
225600000000     C                   CALL      'XKEYS'
225700000000     C                   PARM                    DSKEYS
225800000000     C                   MOVEA     KEYRIG        KEYS
225900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
226000000000     C     XMSGPA        PLIST
226100000000     C                   PARM                    DSMSMS
226200000000     C                   PARM                    DSMSPG
226300000000     C                   PARM                    DSMSKE
226400000000     C                   PARM                    DSMSER
226500000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
226600000000     C                   MOVEL     'TREC61R'     DSMSPG
226700000000     C                   MOVEL     'TREC61R'     VMCQUE
226800000000     C                   MOVEL     *BLANKS       VMCKEY
226900000000     C                   MOVEL     *BLANKS       DSMSMS
227000000000     C* INIZIALIZZO FLAG CONTROLLO PGM
227100000000     C                   MOVEL     '0'           $ERR              1
227200000000     C                   MOVEL     '0'           $F03              1
227300000000     C                   MOVEL     *BLANKS       $GES              2
227400000000     C                   MOVEL     *BLANKS       $MSG             80
227500000000     C                   MOVEL     '0'           $FINE             1
227600000000     C                   MOVEL     '0'           $TESVD            1
227700000000     C* LANCIO ROUTINE PER VERIFICARE SE
227800000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
227900000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
228000020923     C                   EXSR      REPDATIUTE
228100000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
228200000000     C                   BITOFF    '01234567'    POSMI1            1
228300000000     C                   BITOFF    '01234567'    POSMI2            1
228400000000     C                   BITOFF    '01234567'    POSMA1            1
228500000000     C                   BITOFF    '01234567'    POSMA2            1
228600000000     C                   BITON     '34'          POSMI1            1
228700000000     C                   BITON     '7'           POSMI2            1
228800000000     C                   BITON     '34'          POSMA1            1
228900000000     C                   BITON     '0'           POSMA2            1
229000000000     C                   MOVE      POSMI2        POSMIN            2
229100000000     C                   MOVEL     POSMI1        POSMIN
229200000000     C                   MOVE      POSMA2        POSMAX            2
229300000000     C                   MOVEL     POSMA1        POSMAX
229400000000     C                   ENDSR
229500130924     C*------------------------------------------------------------------------*
229600130924     C* dupnot - duplica note da precedente progressivo se esistono
229700130924     C*------------------------------------------------------------------------*
229800130924     C     dupnot        begsr
229900130924     c                   z-add     evtprg        newprg            3 0
230000130924     c     kevt          setll     ecevt01l
230100130924     c     evtvoc        readpe(n) ecevt01l
230200130924     c                   if        %eof(ecevt01l)
230300130924     c                   leavesr
230400130924     c                   endif
230500130924     c     knot          setll     ecnot01l
230600130924     c                   do        *hival
230700130924     c     knot          reade     ecnot01l
230800130924     c                   if        %eof(ecnot01l)
230900130924     c                   leave
231000130924     c                   endif
231100130924     c                   z-add     newprg        notprg
231200130924     c                   write     ecnot000
231300130924     c                   enddo
231400130924     c                   endsr
231500000000     C*------------------------------------------------------------------------*
231600000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
231700000000     C*------------------------------------------------------------------------*
231800000000     C     FUNSTD        BEGSR
231900000000     C                   SELECT
232000000000     C     KEY           WHENEQ    F3
232100000000     C                   EXSR      F03STD
232200000000     C     KEY           WHENEQ    F12
232300000000     C                   EXSR      F12STD
232400000000     C     KEY           WHENEQ    F24
232500000000     C                   EXSR      F24STD
232600000000     C                   OTHER
232700000000     C                   MOVEL     MSG(2)        $MSG
232800000000     C                   EXSR      CARMSG
232900000000     C                   ENDSL
233000000000     C                   ENDSR
233100000000     C*------------------------------------------------------------------------*
233200000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
233300000000     C*------------------------------------------------------------------------*
233400000000     C     F03STD        BEGSR
233500000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
233600000000     C                   MOVEL     '1'           $F03
233700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
233800000000     C                   MOVEL     '1'           $FINE
233900000000     C                   ENDSR
234000000000     C*------------------------------------------------------------------------*
234100000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
234200000000     C*------------------------------------------------------------------------*
234300000000     C     F12STD        BEGSR
234400000000     C* IMPOSTO FLAG DI FIN PGM
234500000000     C                   MOVEL     '1'           $FINE
234600000000     C                   ENDSR
234700000000     C*------------------------------------------------------------------------*
234800000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
234900000000     C*------------------------------------------------------------------------*
235000000000     C     F24STD        BEGSR
235100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
235200000000     C                   EXSR      OUTFUN
235300000000     C                   ENDSR
235400000000     C*------------------------------------------------------------------------*
235500000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
235600000000     C*          CODA MESSAGGI DEL PGM
235700000000     C*------------------------------------------------------------------------*
235800000000     C     CARMSG        BEGSR
235900000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
236000000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
236100000000     C     MSGULT        IFEQ      0
236200000000     C                   MOVEL     '1'           *IN40
236300000000     C                   WRITE     EC61DVK
236400000000     C                   MOVEL     '0'           *IN40
236500000000     C                   ENDIF
236600000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
236700000000     C                   MOVEL     $MSG          DSMSMS
236800000000     C                   CALL      'XMSG'        XMSGPA
236900000000     C     DSMSER        IFEQ      0
237000000000     C                   MOVEL     DSMSKE        VMCKEY
237100000000     C                   ENDIF
237200000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
237300000000     C                   ADD       1             MSGULT            4 0
237400000000     C                   WRITE     EC61DVM
237500000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
237600000000     C                   MOVEL     *BLANKS       $MSG
237700000000     C                   ENDSR
237800000000     C*------------------------------------------------------------------------*
237900000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
238000000000     C*------------------------------------------------------------------------*
238100000000     C     OUTMSG        BEGSR
238200000000     C* VISUALIZZAZIONE MESSAGGIO
238300000000     C                   MOVEL     '1'           *IN44
238400000000     C                   WRITE     EC61DVK
238500000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
238600000000     C                   Z-ADD     *ZEROS        MSGULT
238700000000     C                   ENDSR
238800000000     C*------------------------------------------------------------------------*
238900000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
239000000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
239100000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
239200000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
239300000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
239400000000     C*------------------------------------------------------------------------*
239500000000     C     CARFUN        BEGSR
239600000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
239700000000     C     1             DO        31            I                 5 0
239800000000     C                   MOVEL     FUN(I)        FUNRIG
239900000000     C                   MOVEL     FUNFUN        FFU(I)
240000000000     C                   MOVEL     FUNDES        FDE(I)
240100020923     C     DUTLPO        IFEQ      'S'
240200000000     C     $TGES         IFEQ      'G'
240300000000     C                   MOVEL     FUNASG        FAT(I)
240400000000     C                   ELSE
240500000000     C                   MOVEL     FUNASI        FAT(I)
240600000000     C                   ENDIF
240700000000     C                   ELSE
240800000000     C     $TGES         IFEQ      'G'
240900000000     C                   MOVEL     FUNAFG        FAT(I)
241000000000     C                   ELSE
241100000000     C                   MOVEL     FUNAFI        FAT(I)
241200000000     C                   ENDIF
241300000000     C                   ENDIF
241400000000     C     FAT(I)        IFEQ      'E'
241500000000     C                   MOVEL     USER          WEDP              3
241600000000     C     WEDP          IFEQ      'EDP'
241700000000     C                   MOVEL     'S'           FAT(I)
241800000000     C                   ELSE
241900000000     C                   MOVEL     'N'           FAT(I)
242000000000     C                   ENDIF
242100000000     C                   ENDIF
242200000000     C                   ENDDO
242300000000     C                   ENDSR
242400000000     C*------------------------------------------------------------------------*
242500000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
242600000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
242700000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
242800000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
242900000000     C*------------------------------------------------------------------------*
243000000000     C     OUTFUN        BEGSR
243100000000     C     1             DO        8             I
243200000000     C                   MOVEL     *BLANKS       FUV(I)
243300000000     C                   ENDDO
243400000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
243500000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
243600000000     C     1             DO        31            I
243700000000     C     FAT(I)        IFEQ      'S'
243800000000     C     FDE(I)        ANDNE     *BLANKS
243900000000     C                   ADD       1             FUNNAT
244000000000     C                   ENDIF
244100000000     C                   ENDDO
244200000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
244300000000     C     FUNNAT        IFGT      8
244400000000     C                   MOVEL     'S'           FAT(24)
244500000000     C                   ENDIF
244600000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
244700000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
244800000000     C                   Z-ADD     8             FUNVIS            5 0
244900000000     C     FAT(23)       IFEQ      'S'
245000000000     C                   SUB       1             FUNVIS
245100000000     C                   ENDIF
245200000000     C     FAT(24)       IFEQ      'S'
245300000000     C                   SUB       1             FUNVIS
245400000000     C                   ENDIF
245500000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
245600000000     C                   Z-ADD     *ZEROS        W                 5 0
245700000000     C                   ADD       1             FUNULT
245800000000     C     FUNULT        IFGT      31
245900000000     C                   Z-ADD     1             FUNULT
246000000000     C                   ENDIF
246100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
246200000000     C     FUNULT        DO        31            I
246300000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
246400000000     C     W             IFLT      FUNVIS
246500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
246600000000     C     FAT(I)        IFEQ      'S'
246700000000     C     FDE(I)        ANDNE     *BLANKS
246800000000     C     I             IFEQ      23
246900000000     C     I             OREQ      24
247000000000     C                   ELSE
247100000000     C                   ADD       1             W
247200000000     C                   MOVEL     FDE(I)        FUV(W)
247300000000     C                   ENDIF
247400000000     C                   ENDIF
247500000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
247600000000     C                   Z-ADD     I             FUNULT
247700000000     C                   ENDIF
247800000000     C                   ENDDO
247900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
248000000000     C                   MOVEL     FUV(1)        VFD001
248100000000     C                   MOVEL     FUV(2)        VFD002
248200000000     C                   MOVEL     FUV(3)        VFD003
248300000000     C                   MOVEL     FUV(4)        VFD004
248400000000     C                   MOVEL     FUV(5)        VFD005
248500000000     C                   MOVEL     FUV(6)        VFD006
248600000000     C     FAT(23)       IFEQ      'N'
248700000000     C     FAT(24)       ANDEQ     'N'
248800000000     C                   MOVEL     FUV(7)        VFD007
248900000000     C                   MOVEL     FUV(8)        VFD008
249000000000     C                   ELSE
249100000000     C     FAT(23)       IFEQ      'S'
249200000000     C     FAT(24)       ANDEQ     'S'
249300000000     C                   MOVEL     FDE(23)       VFD007
249400000000     C                   MOVEL     FDE(24)       VFD008
249500000000     C                   ELSE
249600000000     C                   MOVEL     FUV(7)        VFD007
249700000000     C     FAT(23)       IFEQ      'S'
249800000000     C                   MOVEL     FDE(23)       VFD008
249900000000     C                   ELSE
250000000000     C                   MOVEL     FDE(24)       VFD008
250100000000     C                   ENDIF
250200000000     C                   ENDIF
250300000000     C                   ENDIF
250400000000     C                   MOVEL     '1'           $TESVD
250500000000     C                   ENDSR
250600000000     C*------------------------------------------------------------------------*
250700000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
250800000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
250900000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
251000000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
251100000000     C*------------------------------------------------------------------------*
251200000000     C     CONFUN        BEGSR
251300000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
251400000000     C     1             DO        31            I
251500000000     C     KEY           IFEQ      KEYS(I)
251600000000     C* CONTROLLO SE IL TASTO E' ATTIVO
251700000000     C     FAT(I)        IFNE      'S'
251800000000     C                   MOVEL     MSG(1)        $MSG
251900000000     C                   EXSR      CARMSG
252000000000     C                   MOVEL     '1'           $ERR
252100000000     C                   ENDIF
252200000000     C                   ENDIF
252300000000     C                   ENDDO
252400000000     C                   ENDSR
252500981015** ======== SCHIERA: MSG  (MESSAGGI)                       ================
252600000000TREC61R- Tasto funzionale non valido.                                          1
252700000000TREC61R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
252800000000TREC61R- Opzione non valida.                                                   3
252900000000TREC61R- Opzione abilitata ma non gestita. Avvertire CED.                      4
253000000000TREC61R- Inizio elenco.                                                        5
253100000000TREC61R- Fine elenco.                                                          6
253200000000TREC61R- Immettere il codice voce                                              7
253300000000TREC61R- Immettere data formalmente valida                                     8
253400000000TREC61R- Immettere codice competenza esistente e non annullato                 9
253500000000TREC61R- Immettere la descrione per la voce                                   10
253600000000TREC61R- Immettere "V" per Voce o "M" per Maggiorazione                       11
253700000000TREC61R- Immettere tipo tariffa esistente e non annullato                     12
253800951207TREC61R- Voce già esistente                                                   13
253900000000TREC61R- La gestione e' consentita solo per voci di tipo 'M'=Maggiorazione    14
254000000000TREC61R- Immettere sviluppo peso tassabile valido                             15
254100940725TREC61R- Immettere un valore valido per l'arrotondamento                      16
254200000000TREC61R- Inserimento effettuato.                                              17
254300000000TREC61R- Modifica effettuata.                                                 18
254400000000TREC61R- Cancellazione voce, tariffe e maggiorazioni effettuata.              19
254500000000TREC61R- Ripristino effettuato.                                               20
254600000000TREC61R- Richiesta funzione per voce gia' esistente                           21
254700000000TREC61R- Codice voce momentaneamente allocato: riprovare più tardi            22
254800000000TREC61R- La scadenza voce deve essere maggiore o uguale della decorrenza      23
254900000000TREC61R- La validita' della voce si sovrappone ad una gia' in decorrenza      24
255000000000TREC61R- Immettere codice riferimento tariffa esistente e non annullato       25
255100000000TREC61R- Il riferimento tariffa immesso non e' del tipo richiesto             26
255200000000TREC61R- Per il tipo tariffa non immettere sviluppo peso tassabile            27
255300000000TREC61R- I due riferimenti tariffa (Italia ed estero) non sono compatibili    28
255400000000TREC61R- Immettere progressivo valido                                         29
255500000000TREC61R- E' gia' esistente, in archivio, la voce con questo progressivo       30
255600000000TREC61R- Gia' presenti altri progressivi con diverso tipo voce                31
255700010110TREC61R- Duplicazione effettuata.                                             32
255800940725TREC61R- Non immettere arrot. fino/oltre se non presente arrot. limite        33
255900940726TREC61R- Per questo tipo di tariffa non e'richiesta la ricerca per tipo scagl.34
256000940726TREC61R- Per 'Voce' inserire la ricerca scagl. solo se immesso il tipo tariffa35
256100141203TREC61R- Per le maggioraz.solo Tipo tariffa abilitato e no Tipo ricerca scagl.36
256200970129TREC61R- Immettere codice raggruppamento delta esistente e non annullato      37
256300980318TREC61R- Non immettere il codice competenza                                   38
256400980318TREC61R- Non immettere il codice raggruppamento delta                         39
256500010112TREC61R- Esistono tariffe con decorrenza oltre la data di scadenza voce.      40
256600010112TREC61R- Errore generico in agiornamento tariffe correlate                    41
256700010115TREC61R- Duplica effettuata ma verificare tariffe per eventuali incongruenze! 42
256800021209TREC61R- Tipo data indicato errato, correggere.                               43
256900030422TREC61R- Immettere sviluppo peso tassabile per ricerca                        44
257000030422TREC61R- Immettere rapporto peso volume per ricerca                           45
257100030423TREC61R- Tipo tariffa valida solo ai fini della ricerca scaglione             46
257200100421TREC61R- Per la voce di isolamento ci sono dei valori non corretti            47
257300100422TREC61R-Il cod. non è presente nel range x il tipo+riferim. scelto (tab.2O)   48
257400030423TREC61R-                                                                      49
257500030423TREC61R-                                                                      50
257600000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
257700000000F1    -                   -    NN     NN
257800000000F2    -                   -    NN     NN
257900000000F3    -F3=Fine            -    SS     SS
258000000000F4    -                   -    NN     NN
258100000000F5    -                   -    NN     NN
258200000000F6    -F6=Aggiornamento   -    SS     SS
258300000000F7    -                   -    NN     NN
258400000000F8    -F8=Tariffe         -    SS     SS
258500000000F9    -F9=Competenze      -    SS     SS
258600000000F10   -F10=Maggiorazioni  -    SS     SS
258700000000F11   -                   -    NN     NN
258800000000F12   -F12=Ritorno        -    SS     SS
258900000000F13   -                   -    NN     NN
259000000000F14   -                   -    NN     NN
259100000000F15   -                   -    NN     NN
259200000000F16   -                   -    NN     NN
259300000000F17   -                   -    NN     NN
259400000000F18   -                   -    NN     NN
259500000000F19   -                   -    NN     NN
259600000000F20   -                   -    NN     NN
259700000000F21   -                   -    NN     NN
259800000000F22   -                   -    NN     NN
259900000000F23   -                   -    NN     NN
260000000000F24   -F24=Altri tasti    -    NN     NN
260100000000ENTER -                   -    SS     SS
260200000000ROLLUP-                   -    NN     NN
260300000000ROLLDN-                   -    NN     NN
260400000000HELP  -                   -    NN     NN
260500000000PRINT -                   -    NN     NN
260600000000BSPACE-                   -    NN     NN
260700000000CLEAR -                   -    NN     NN
260800000000** DOPZ  (OPZIONE RICHIAMATA)
260900000000* INSERIMENTO *                                                 1
261000000000*  VARIAZIONE  *                                                2
261100000000*CANCELLAZIONE*                                                 3
261200000000*  RIPRISTINO  *                                                4
261300000000VISUALIZZAZIONE                                                 5
261400981015ANNULLATO                                                       6
261500010110* DUPLICAZIONE *
2616009810158
2617009810159
26180098101510
