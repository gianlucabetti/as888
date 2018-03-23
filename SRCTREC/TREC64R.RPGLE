000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC64R
000300000000      *  Descrizione   :  Gesione tariffe voci di conto economico
000400000000      *                   Programma di immissione, modifica, visualizzazione,...
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:     MAG 1994
000700131121     C*?    Compilare con ovrdbf OVRDBF FILE(ECEVDX6L) TOFILE(ECEVD06L)
000800000000      *------------------------------------------------------------------------*
000900000000     H DECEDIT('0,') DATEDIT(*DMY.)
001000000000     F*------------------------------------------------------------------------*
001100000000     F* DATA BASE
001200000000     F*------------------------------------------------------------------------*
001300000000     FTREC64D   CF   E             WORKSTN INFDS(DEVDS1)
001400000000     F                                     SFILE(EC64DVM:MSGULT)
001500941130     FAZCAS01L  IF   E           K DISK
001600000000     FAZORG01L  IF   E           K DISK
001700131121     FECEVD06L  UF A E           K DISK
001800131121     FECEVDx6L  if   E           K DISK    rename(ecevd000:vdx) prefix(x) usropn
001900000000     FECEVT01L  IF   E           K DISK
002000100422     FECEVM01l  iF   E           K DISK
002100000000     FTABEL00F  IF   E           K DISK
002200941128     FTNTAM01L  IF   E           K DISK
002300131122     Fazcpc01L  IF   E           K DISK
002400000000     D*------------------------------------------------------------------------*
002500000000     D* SCHIERE
002600000000     D*------------------------------------------------------------------------*
002700131122     D MSG             S             78    DIM(48) CTDATA PERRCD(1)
002800000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
002900000000     D*-------------------
003000000000     D* DECODIFICA OPZIONE RICHIAMATA
003100981015     D*-------------------
003200981015     D DOPZ            S             16    DIM(10) CTDATA PERRCD(1)
003300130920     D Cmd1            S             48    dim(1) ctdata perrcd(1)
003400130920     D Cmd2            S             48    dim(1) ctdata perrcd(1)
003500130920     D Cmd             S             48
003600000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003700000000     D FFU             S              6    DIM(31)
003800000000     D FDE             S             19    DIM(31)
003900000000     D FAT             S              1    DIM(31)
004000000000     D* TABELLA DESCRIZIONI A VIDEO
004100000000     D FUV             S             19    DIM(8)
004200000000     D* TABELLA TASTI FUNZIONALI PREMUTI
004300000000     D KEYS            S              1    DIM(40)
004400000000     D* TABELLA CAPICONTO
004500000000     D*------------------------------------------------------------------------*
004600000000     D* INPUT
004700000000     D*------------------------------------------------------------------------*
004800131122     D*-------------------
004900131122     D* DS ESTERNA TABELLA 15 (CODICI nazioni)
005000131122     D*-------------------
005100131122     D DS15          E DS
005200000000     D*-------------------
005300000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
005400000000     D*-------------------
005500000000     D DSFUN         E DS
005600000000     D  FUNRIG                 1     40
005700000000     D*-------------------
005800000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005900000000     D*-------------------
006000000000     D DSMSG         E DS
006100000000     D*-------------------
006200020923     D* DS REPERIMENTO DATI UTENTE
006300020923     D*-------------------
006400020923     D TIBS34DS      E DS
006500020923     D DDATIUTE      E DS
006600020923     D AZUTEDS       E DS                  extname(AZUTE00F)
006700000000     D*-------------------
006800000000     D* DS PER GESTIONE TASTI FUNZIONALI
006900000000     D*-------------------
007000000000     D DSKEYS        E DS
007100000000     D  KEYRIG                 1     40
007200000000     D*-------------------
007300000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
007400000000     D*-------------------
007500000000     D                SDS
007600000000     D  USER                 254    263
007700000000     D*-------------------
007800000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
007900000000     D*-------------------
008000000000     D DEVDS1          DS
008100000000     D* CHIAVE FUNZIONALE BATTUTA
008200000000     D  KEY                  369    369
008300000000     D* POSIZIONE CURSORE (IN HEX)
008400000000     D  CURSOR               370    371
008500000000     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
008600000000     D  FRTSFL               378    379B 0
008700000000     D*---------------------
008800000000     D* DS ESTERNA DI PROCEDURA
008900000000     D*---------------------
009000000000     D DS60          E DS                  EXTNAME(TREC60DS)
009100970627     D*---------------------
009200970627     D* DS ESTERNA DI PROCEDURA
009300970627     D*---------------------
009400970627     D DSSI95        E DS                  EXTNAME(TISI95DS)
009500971020     D*------------------
009600971020     D* DS "FNLV55R" - REPERIMENTO TERMINALs
009700971020     D*------------------
009800971020     D DSLV55        E DS                  EXTNAME(FNLV55DS)
009900000000     D*-------------------
010000000000     D* ARCHITETTURA
010100000000     D*-------------------
010200000000     D KPJBA         E DS
010300000000     D*------------------
010400000000     D* CALL XSRDA8 - CONTROLLO DATE
010500000000     D*------------------
010600000000     D WLBDAT          DS                  INZ
010700000000     D  G02DAT                 1      8  0
010800000000     D  G02INV                 9     16  0
010900000000     D  G02ERR                17     17
011000000000     D  G02TGI                18     22  0
011100000000     D*------------------
011200000000     D* TABELLE DI DECODIFICA
011300000000     D*------------------
011400000000     D DS2Q          E DS
011500000000     D DS2R          E DS
011600100422     D DS2O          E DS
011601150427     D DS2Omi        E DS                  extname(ds2o) prefix (mi_)
011700130806     D DSct          E DS
011800000000     D*------------------
011900950718     D* PARAMETRI CHIAMATA 'TNTA36R'
012000000000     D*------------------
012100000000     D PARAM4          DS                  INZ
012200000000     D  PARCOD                 6     12  0
012300000000     D  PARCTR                13     15
012400000000     D  PARFLG                16     16
012500000000     D  PARID                 17     17  0
012600000000     D  PARCCC                18     21  0
012700000000     D  PARDCV                22     36
012800000727     D*------------------
012900000727     D* DS REPERIMENTO DATI UTENTE
013000000727     D*-------------------
013100000727     D BS69DS        E DS                  EXTNAME(TIBS69DS)
013200000727     D ACODS         E DS                  EXTNAME(CNACO00F)
013300000727     D INDDS         E DS                  EXTNAME(CNIND00F)
013400000727     D CLPDS         E DS                  EXTNAME(CNCLP00F)
013500000727     D CLSDS         E DS                  EXTNAME(FNCLS00F)
013600000000     C*------------------------------------------------------------------------*
013700000000     C* MAIN LINE
013800000000     C*------------------------------------------------------------------------*
013900000000     C*
014000000000     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
014100000000IF  1C     $FINE         IFEQ      '1'
014200000000     C*
014300000000     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
014400000000     C                   MOVEL     '1'           D60ERR
014500000000     C*
014600000000     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
014700000000X   1C                   ELSE
014800000000     C*
014900000000     C* CICLO FINO A CHE:
015000000000DO  2C     $FINE         DOWEQ     '0'
015100000000     C                   MOVEL     '0'           $ERR
015200000000     C*
015300000000     C* CONTROLLA GESTIONE VIDEO
015400000000IF  3C     $GES          IFEQ      'V1'
015500000000     C                   EXSR      GESV1
015600000000E   3C                   ENDIF
015700000000     C*
015800000000E   2C                   ENDDO
015900000000E   1C                   ENDIF
016000000000     C*
016100000000IF  1C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
016200000000     C     $F03          IFEQ      '1'
016300000000     C                   MOVEL     '1'           D60F03
016400000000E   1C                   ENDIF
016500970627     C*
016600970627     C* OPERAZIONI FINALI
016700970627     C                   EXSR      FINSR
016800970627     C*
016900000000     C                   MOVEL     DS60          KPJBU
017000000000     C                   SETON                                        LR
017100000000     C*------------------------------------------------------------------------*
017200000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
017300000000     C*------------------------------------------------------------------------*
017400000000     C     CARV1         BEGSR
017500000000     C*
017600000000     C                   MOVEL     'V1'          $GES
017700000000     C*
017800000000     C                   EXSR      CARFUN
017900000000     C                   EXSR      OUTFUN
018000000000     C*
018100000000     C                   WRITE     EC64DVT
018200000000     C                   WRITE     EC64DVF
018300000000     C*
018400000000     C                   ENDSR
018500000000     C*------------------------------------------------------------------------*
018600000000     C* GESV1  - GESTIONE PRIMO VIDEO
018700000000     C*------------------------------------------------------------------------*
018800000000     C     GESV1         BEGSR
018900000000     C*
019000000000IF  1C     $TESVD        IFEQ      '1'
019100000000     C                   EXSR      CARV1
019200000000E   1C                   ENDIF
019300000000     C*
019400000000IF  1C     MSGULT        IFNE      *ZEROS
019500000000     C                   EXSR      OUTMSG
019600000000E   1C                   ENDIF
019700000000     C*
019800000000     C                   EXFMT     EC64DV1
019900000000     C*
020000000000     C                   EXSR      CONFUN
020100000000     C*
020200000000IF  1C     $ERR          IFEQ      '0'
020300000000SL  2C                   SELECT
020400000000     C     KEY           WHENEQ    ENTER
020500000000     C                   EXSR      ENTV1
020600000000     C     KEY           WHENEQ    F6
020700000000     C                   EXSR      F06GES
020800000000     C                   OTHER
020900000000     C                   EXSR      FUNSTD
021000000000E   2C                   ENDSL
021100000000E   1C                   ENDIF
021200000000     C*
021300000000     C                   ENDSR
021400000000     C*------------------------------------------------------------------------*
021500000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
021600000000     C*------------------------------------------------------------------------*
021700000000     C     ENTV1         BEGSR
021800000000     C*
021900000000     C* CONTROLLA I CAMPI IMMESSI
022000000000     C                   EXSR      DECODI
022100000000     C*
022200000000     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
022300000000IF  1C     D60OP1        IFEQ      'O05'
022400000000     C                   MOVEL     '1'           $FINE
022500000000E   1C                   ENDIF
022600000000     C*
022700000000     C                   ENDSR
022800000000     C*------------------------------------------------------------------------*
022900000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> CONFERMA OPERAZIONE
023000000000     C*------------------------------------------------------------------------*
023100000000     C     F06GES        BEGSR
023200000000     C*
023300000000     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
023400000000IF  1C     D60OP1        IFEQ      'F06'
023500000000     C                   EXSR      INSERI
023600000000E   1C                   ENDIF
023700000000IF  1C     D60OP1        IFEQ      'O02'
023800000000     C                   EXSR      MODIFI
023900000000E   1C                   ENDIF
024000010112IF  1C     D60OP1        IFEQ      'O03'
024100010112     C                   EXSR      DUPLIC
024200010112E   1C                   ENDIF
024300000000IF  1C     D60OP1        IFEQ      'O04'
024400000000     C                   EXSR      ANNULL
024500000000E   1C                   ENDIF
024600000000IF  1C     D60OP1        IFEQ      'O07'
024700000000     C                   EXSR      RIPRIS
024800000000E   1C                   ENDIF
024900000000     C*
025000000000     C                   ENDSR
025100000000     C*------------------------------------------------------------------------*
025200000000     C* INSERI - FUNZIONE DI INSERIMENTO
025300000000     C*------------------------------------------------------------------------*
025400000000     C     INSERI        BEGSR
025500000000     C*
025600000000     C* CONTROLLA I CAMPI IMMESSI
025700000000     C                   EXSR      DECODI
025800000000     C*
025900000000IF  1C     $ERR          IFEQ      '0'
026000000000     C*
026100000000     C                   Z-ADD     V1CVOC        EVDVOC
026200000000     C                   Z-ADD     V1CPRG        EVDPRG
026300000000     C                   MOVEL     V1CTTV        EVDTTV
026400000000     C                   MOVEL     V1CFT1        EVDFT1
026500000000     C                   MOVEL     V1CFT2        EVDFT2
026600000000     C                   MOVEL     V1CCLI        EVDCLI
026700000000     C                   MOVEL     V1CCTR        EVDCTR
026800000000     C                   MOVEL     V1CCTS        EVDCTS
026900131121     C                   movel     V1CCAd        EVDCAd
027000131121     C                   movel     V1Cnaz        EVDnaz
027100000000     C                   Z-ADD     V1CDDT        G02DAT
027200000000     C                   Z-ADD     *ZEROS        G02INV
027300000000     C                   MOVEL     '0'           G02ERR
027400000000     C                   CALL      'XSRDA8'
027500000000     C                   PARM                    WLBDAT
027600000000     C                   Z-ADD     G02INV        EVDDDT
027700000000     C                   Z-ADD     V1CDST        G02DAT
027800000000     C                   Z-ADD     *ZEROS        G02INV
027900000000     C                   MOVEL     '0'           G02ERR
028000000000     C                   CALL      'XSRDA8'
028100000000     C                   PARM                    WLBDAT
028200000000     C                   Z-ADD     G02INV        EVDDST
028300000000     C                   Z-ADD     V1CSGI        EVDSGI
028400000000     C                   Z-ADD     V1CSGF        EVDSGF
028500000000     C                   Z-ADD     V1CITR        EVDITR
028600000000     C                   Z-ADD     V1CIMI        EVDIMI
028700000000     C                   Z-ADD     V1CIMA        EVDIMA
028800000000     C                   MOVEL     V1CSEG        EVDSEG
028900000000     C                   Z-ADD     V1CPER        EVDPER
029000000000     C                   Z-ADD     V1CRPV        EVDRPV
029100000000     C*
029200000000     C                   MOVEL     *BLANKS       EVDATB
029300000000     C                   MOVEL     *BLANKS       EVDFTR
029400000000     C                   Z-ADD     *ZEROS        EVDDTR
029500010808     C                   Z-ADD     DATCOR        EVDDIR
029600010813     C                   Z-ADD     *ZEROS        EVDDUV
029700000000     C*
029800010808     C                   WRITE     ECEVD000
029900000000     C*
030000000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
030100000000     C                   MOVEL     '1'           $FINE
030200000000     C*
030300000000     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
030400000000     C                   MOVEL     MSG(17)       D60MSG
030500000000     C*
030600000000E   1C                   ENDIF
030700000000     C*
030800000000     C                   ENDSR
030900000000     C*------------------------------------------------------------------------*
031000000000     C* MODIFI - FUNZIONE DI MODIFICA
031100000000     C*------------------------------------------------------------------------*
031200000000     C     MODIFI        BEGSR
031300000000     C*
031400000000     C* CONTROLLA I CAMPI IMMESSI
031500000000     C                   EXSR      DECODI
031600000000     C*
031700000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
031800000000     C                   Z-ADD     D60VOC        KVDVOC
031900000000     C                   Z-ADD     D60PRG        KVDPRG
032000000000     C                   MOVE      TTVKTV        KVDTTV
032100000000     C                   Z-ADD     FT1KTV        KVDFT1
032200000000     C                   Z-ADD     FT2KTV        KVDFT2
032300000000     C                   Z-ADD     CLIKTV        KVDCLI
032400000000     C                   Z-ADD     CTRKTV        KVDCTR
032500000000     C                   MOVE      CTSKTV        KVDCTS
032600131121     C                   movel     CAdKTV        KVDCAd
032700131121     C                   movel     nazKTV        KVDnaz
032800000000     C                   Z-ADD     DDTKTV        KVDDDT
032900000000     C                   Z-ADD     SGIKTV        KVDSGI
033000131121     C     KEYEVD        CHAIN     ECEVD06L                           99
033100000000IF  1C     *IN99         IFEQ      *OFF
033200000000     C*
033300000000IF  2C     $ERR          IFEQ      '0'
033400000000     C*
033500000000     C                   Z-ADD     D60VOC        EVDVOC
033600000000     C                   Z-ADD     D60PRG        EVDPRG
033700000000     C                   MOVEL     V1CTTV        EVDTTV
033800000000     C                   MOVEL     V1CFT1        EVDFT1
033900000000     C                   MOVEL     V1CFT2        EVDFT2
034000000000     C                   MOVEL     V1CCLI        EVDCLI
034100000000     C                   MOVEL     V1CCTR        EVDCTR
034200000000     C                   MOVEL     V1CCTS        EVDCTS
034300131121     C                   movel     V1CCAd        EVDCAd
034400131121     C                   movel     V1Cnaz        EVDnaz
034500000000     C                   Z-ADD     V1CDDT        G02DAT
034600000000     C                   Z-ADD     *ZEROS        G02INV
034700000000     C                   MOVEL     '0'           G02ERR
034800000000     C                   CALL      'XSRDA8'
034900000000     C                   PARM                    WLBDAT
035000000000     C                   Z-ADD     G02INV        EVDDDT
035100000000     C                   Z-ADD     V1CDST        G02DAT
035200000000     C                   Z-ADD     *ZEROS        G02INV
035300000000     C                   MOVEL     '0'           G02ERR
035400000000     C                   CALL      'XSRDA8'
035500000000     C                   PARM                    WLBDAT
035600000000     C                   Z-ADD     G02INV        EVDDST
035700000000     C                   Z-ADD     V1CSGI        EVDSGI
035800000000     C                   Z-ADD     V1CSGF        EVDSGF
035900000000     C                   Z-ADD     V1CITR        EVDITR
036000000000     C                   Z-ADD     V1CIMI        EVDIMI
036100000000     C                   Z-ADD     V1CIMA        EVDIMA
036200000000     C                   MOVEL     V1CSEG        EVDSEG
036300000000     C                   Z-ADD     V1CPER        EVDPER
036400000000     C                   Z-ADD     V1CRPV        EVDRPV
036500000000     C*
036600000000     C                   MOVEL     *BLANKS       EVDATB
036700000000     C                   MOVEL     *BLANKS       EVDFTR
036800970903     C*---                 Z-ADD*ZEROS    EVDDTR           *DATA TRASMISSIONE
036900010808     C                   Z-ADD     DATCOR        EVDDUV
037000000000     C*
037100010808     C                   UPDATE    ECEVD000
037200000000     C*
037300000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
037400000000     C                   MOVEL     '1'           $FINE
037500000000     C*
037600000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
037700000000     C                   MOVEL     MSG(18)       D60MSG
037800000000     C*
037900000000E   2C                   ENDIF
038000000000E   1C                   ENDIF
038100000000     C*
038200000000     C                   ENDSR
038300010112     C*------------------------------------------------------------------------*
038400010112     C* DUPLIC - FUNZIONE DI DUPLICAZIONE
038500010112     C*------------------------------------------------------------------------*
038600010112     C     DUPLIC        BEGSR
038700010112     C*
038800010112     C* CONTROLLA I CAMPI IMMESSI
038900010112     C                   EXSR      DECODI
039000010112     C*
039100010112IF  1C     $ERR          IFEQ      '0'
039200010112     C*
039300010112     C                   Z-ADD     V1CVOC        EVDVOC
039400010112     C                   Z-ADD     V1CPRG        EVDPRG
039500010112     C                   MOVEL     V1CTTV        EVDTTV
039600010112     C                   MOVEL     V1CFT1        EVDFT1
039700010112     C                   MOVEL     V1CFT2        EVDFT2
039800010112     C                   MOVEL     V1CCLI        EVDCLI
039900010112     C                   MOVEL     V1CCTR        EVDCTR
040000010112     C                   MOVEL     V1CCTS        EVDCTS
040100131121     C                   movel     V1CCAd        EVDCAd
040200131121     C                   movel     V1Cnaz        EVDnaz
040300010112     C                   Z-ADD     V1CDDT        G02DAT
040400010112     C                   Z-ADD     *ZEROS        G02INV
040500010112     C                   MOVEL     '0'           G02ERR
040600010112     C                   CALL      'XSRDA8'
040700010112     C                   PARM                    WLBDAT
040800010112     C                   Z-ADD     G02INV        EVDDDT
040900010112     C                   Z-ADD     V1CDST        G02DAT
041000010112     C                   Z-ADD     *ZEROS        G02INV
041100010112     C                   MOVEL     '0'           G02ERR
041200010112     C                   CALL      'XSRDA8'
041300010112     C                   PARM                    WLBDAT
041400010112     C                   Z-ADD     G02INV        EVDDST
041500010112     C                   Z-ADD     V1CSGI        EVDSGI
041600010112     C                   Z-ADD     V1CSGF        EVDSGF
041700010112     C                   Z-ADD     V1CITR        EVDITR
041800010112     C                   Z-ADD     V1CIMI        EVDIMI
041900010112     C                   Z-ADD     V1CIMA        EVDIMA
042000010112     C                   MOVEL     V1CSEG        EVDSEG
042100010112     C                   Z-ADD     V1CPER        EVDPER
042200010112     C                   Z-ADD     V1CRPV        EVDRPV
042300010112     C*
042400010112     C                   MOVEL     *BLANKS       EVDATB
042500010112     C                   MOVEL     *BLANKS       EVDFTR
042600010112     C                   Z-ADD     *ZEROS        EVDDTR
042700010808     C                   Z-ADD     DATCOR        EVDDIR
042800140214     C* Mette oggi sull'ultima variazione fatta il:
042900140214     C                   Z-ADD     DATCOR        EVDDUV
043000010112     C*
043100010808     C                   WRITE     ECEVD000
043200010112     C*
043300010112     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
043400010112     C                   MOVEL     '1'           $FINE
043500010112     C*
043600010112     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
043700010112     C                   MOVEL     MSG(29)       D60MSG
043800010112     C*
043900010112E   1C                   ENDIF
044000010112     C*
044100010112     C                   ENDSR
044200000000     C*------------------------------------------------------------------------*
044300000000     C* ANNULL - FUNZIONE DI ANNULLAMENTO
044400000000     C*------------------------------------------------------------------------*
044500000000     C     ANNULL        BEGSR
044600000000     C*
044700000000     C* CONTROLLA I CAMPI IMMESSI
044800000000     C                   EXSR      DECODI
044900000000     C*
045000000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
045100000000     C                   Z-ADD     D60VOC        KVDVOC
045200000000     C                   Z-ADD     D60PRG        KVDPRG
045300000000     C                   MOVE      TTVKTV        KVDTTV
045400000000     C                   Z-ADD     FT1KTV        KVDFT1
045500000000     C                   Z-ADD     FT2KTV        KVDFT2
045600000000     C                   Z-ADD     CLIKTV        KVDCLI
045700000000     C                   Z-ADD     CTRKTV        KVDCTR
045800000000     C                   MOVE      CTSKTV        KVDCTS
045900131121     C                   MOVEL     CAdKTV        KVDCAd
046000131121     C                   MOVEL     nazKTV        KVDnaz
046100000000     C                   Z-ADD     DDTKTV        KVDDDT
046200000000     C                   Z-ADD     SGIKTV        KVDSGI
046300131121     C     KEYEVD        CHAIN     ECEVD06L                           99
046400000000IF  1C     *IN99         IFEQ      *OFF
046500950322     C*
046600950322     C* NON TESTA L'ERRORE PER POTER ANNULLARE UN RECORD SBAGLIATO
046700950322IF  2C*---       $ERR      IFEQ '0'
046800000000     C*--------------------
046900000000     C* ANNULLO VOCE
047000000000     C*--------------------
047100970903     C* SE NON E' STATO MAI TRASMESSO, CANCELLA FISICAMENTE IL RECORD
047200970903IF  3C     EVDDTR        IFGT      *ZEROS
047300000000     C                   MOVEL     'A'           EVDATB
047400000000     C                   MOVEL     *BLANKS       EVDFTR
047500970903     C*---                 Z-ADD*ZEROS    EVDDTR           *DATA TRASMISSIONE
047600010808     C                   Z-ADD     DATCOR        EVDDUV
047700010808     C                   UPDATE    ECEVD000
047800970903X   3C                   ELSE
047900010808     C                   DELETE    ECEVD000
048000970903E   3C                   ENDIF
048100000000     C*
048200000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
048300000000     C                   MOVEL     '1'           $FINE
048400000000     C*
048500000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
048600000000     C                   MOVEL     MSG(19)       D60MSG
048700000000     C*
048800950322E   2C*---                 ENDIF
048900000000E   1C                   ENDIF
049000000000     C*
049100000000     C                   ENDSR
049200000000     C*------------------------------------------------------------------------*
049300000000     C* RIPRIS - FUNZIONE DI RIPRISTINO
049400000000     C*------------------------------------------------------------------------*
049500000000     C     RIPRIS        BEGSR
049600000000     C*
049700000000     C* CONTROLLA I CAMPI IMMESSI
049800000000     C                   EXSR      DECODI
049900000000     C*
050000000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
050100000000     C                   Z-ADD     D60VOC        KVDVOC
050200000000     C                   Z-ADD     D60PRG        KVDPRG
050300000000     C                   Z-ADD     DDTKTV        KVDDDT
050400000000     C                   MOVEL     TTVKTV        KVDTTV
050500000000     C                   Z-ADD     CLIKTV        KVDCLI
050600000000     C                   Z-ADD     FT1KTV        KVDFT1
050700000000     C                   Z-ADD     FT2KTV        KVDFT2
050800000000     C                   Z-ADD     CTRKTV        KVDCTR
050900000000     C                   MOVE      CTSKTV        KVDCTS
051000131121     C                   MOVEL     CAdKTV        KVDCAd
051100131121     C                   MOVEL     nazKTV        KVDnaz
051200000000     C                   Z-ADD     DDTKTV        KVDDDT
051300000000     C                   Z-ADD     SGIKTV        KVDSGI
051400131121     C     KEYEVD        CHAIN     ECEVD06L                           99
051500000000IF  1C     *IN99         IFEQ      *OFF
051600000000     C*
051700000000IF  2C     $ERR          IFEQ      '0'
051800000000     C*--------------------
051900000000     C* RIPRISTINO VOCE
052000000000     C*--------------------
052100000000     C                   MOVEL     *BLANKS       EVDATB
052200000000     C                   MOVEL     *BLANKS       EVDFTR
052300970903     C*---                 Z-ADD*ZEROS    EVDDTR           *DATA TRASMISSIONE
052400010808     C                   Z-ADD     DATCOR        EVDDUV
052500000000     C*
052600010808     C                   UPDATE    ECEVD000
052700000000     C*
052800000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
052900000000     C                   MOVEL     '1'           $FINE
053000000000     C*
053100000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
053200000000     C                   MOVEL     MSG(20)       D60MSG
053300000000     C*
053400000000E   2C                   ENDIF
053500000000E   1C                   ENDIF
053600000000     C*
053700000000     C                   ENDSR
053800000000     C*------------------------------------------------------------------------*
053900000000     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
054000000000     C*------------------------------------------------------------------------*
054100000000     C     DECODI        BEGSR
054200000000     C*
054300000000     C* REIMPOSTO INDICATORI DI ERRORE
054400000000     C                   SETOFF                                       60  62
054500000000     C                   SETOFF                                       636465
054600000000     C                   SETOFF                                       666768
054700000000     C                   SETOFF                                       697071
054800000000     C                   SETOFF                                       727374
054900000000     C                   SETOFF                                       757677
055000940722     C                   SETOFF                                       10
055100940831     C                   SETOFF                                       7980
055200000000     C                   MOVE      *BLANKS       $PUNTO
055300000000     C*--------------------
055400000000     C* CODICE VOCE
055500000000     C*--------------------
055600000000     C                   MOVE      *BLANKS       V1CVOD
055700000000     C                   Z-ADD     *ZERO         V1CDDV
055800000000     C                   Z-ADD     *ZERO         V1CDSV
055900000000     C                   Z-ADD     V1CVOC        KVTVOC
056000000000     C                   Z-ADD     V1CPRG        KVTPRG
056100000000     C     KEYEVT        CHAIN     ECEVT01L                           60
056200000000     C     *IN60         IFEQ      *ON
056300000000     C                   MOVEL     '1'           $ERR
056400000000     C                   MOVEL     MSG(7)        $MSG
056500000000     C                   EXSR      CARMSG
056600000000     C                   ELSE
056700000000     C                   MOVEL     EVTVOD        V1CVOD
056800000000     C                   MOVE      EVTVRV        V1CVRV
056900000000     C                   MOVE      EVTTIV        V1CTIV
057000000000     C*
057100000000     C                   Z-ADD     EVTDDV        G02INV
057200000000     C                   Z-ADD     *ZEROS        G02DAT
057300000000     C                   MOVEL     '3'           G02ERR
057400000000     C                   CALL      'XSRDA8'
057500000000     C                   PARM                    WLBDAT
057600000000     C                   Z-ADD     G02DAT        V1CDDV
057700000000     C                   Z-ADD     EVTDSV        G02INV
057800000000     C                   Z-ADD     *ZEROS        G02DAT
057900000000     C                   MOVEL     '3'           G02ERR
058000000000     C                   CALL      'XSRDA8'
058100000000     C                   PARM                    WLBDAT
058200000000     C                   Z-ADD     G02DAT        V1CDSV
058300100422     C*
058400100422     C* AGGANCIO LA TABELLA 2O in caso di maggiorazione
058500100422     c* se x voce isolamente le tariffe devono essere tutte positivi o
058600100422     c* tutte negative
058700000000     C*
058800100422     c                   clear                   flgsegtar         1
058900100422     c                   if        evttvo = 'M'
059000100422     C                   MOVEL     1             KTBKUT
059100100422     C                   MOVEL     '2O'          KTBCOD
059200100422     C                   MOVEL     'MI'          KTBKEY
059300100422     C     KEYTAB        CHAIN     TABEL00F                           99
059400100422     C     *IN99         IFEQ      '1'
059500100422     C     TBLFLG        ORNE      *BLANKS
059600100422     C                   ELSE
059700100422     C                   MOVEL     TBLUNI        ds2O
059701150427     C                   MOVEL     TBLUNI        ds2Omi
059800100422     C                   if        §2ovocda <= v1cvoc and
059900100422     C                             §2ovoca >= v1cvoc
060000100422     c                   eval      flgsegtar='1'
060100100422     c                   end
060200100422     c                   end
060300100422     c                   end
060400000000     C* AGGANCIO LA TABELLA 2R PER IL TIPO RICERCA SCAGLIONE
060500000000     C                   MOVE      *BLANKS       V1CTID
060600000000     C     V1CTIV        IFNE      *BLANKS
060700020923     C                   MOVEL     1             KTBKUT
060800000000     C                   MOVEL     '2R'          KTBCOD
060900000000     C                   MOVE      *BLANKS       KTBKEY
061000000000     C                   MOVEL     V1CTIV        KTBKEY
061100000000     C     KEYTAB        CHAIN     TABEL00F                           99
061200000000     C     *IN99         IFEQ      '1'
061300000000     C     TBLFLG        ORNE      *BLANKS
061400000000     C                   ELSE
061500000000     C                   MOVEL     TBLUNI        V1CTID
061600000000     C                   ENDIF
061700000000     C                   ENDIF
061800000000     C*
061900000000     C* AGGANCIO LA TABELLA 2R PER IL TIPO TARIFFA
062000020923     C                   MOVEL     1             KTBKUT
062100000000     C                   MOVEL     '2R'          KTBCOD
062200000000     C                   MOVE      *BLANKS       KTBKEY
062300000000     C                   MOVEL     EVTVRV        KTBKEY
062400000000     C     KEYTAB        CHAIN     TABEL00F                           99
062500000000     C     *IN99         IFEQ      '1'
062600000000     C     TBLFLG        ORNE      *BLANKS
062700000000     C                   CLEAR                   DS2R
062800000000     C                   ELSE
062900000000     C                   MOVEL     TBLUNI        DS2R
063000000000     C                   MOVEL     §2RDES        V1CVRD
063100000000     C*
063200000000     C                   MOVE      *BLANKS       V1CIMP
063300000000     C     §2RTVA        IFEQ      'F'
063400000000     C     'Importo'     CAT       'fisso':1     V1CIMP
063500000000     C     V1CIMP        CAT       '..':1        V1CIMP
063600000000     C                   ELSE
063700000000     C     'Importo'     CAT       'unitar':1    V1CIMP
063800000000     C     V1CIMP        CAT       'io':0        V1CIMP
063900000000     C                   ENDIF
064000141202     c* per maggiorazioni con tipo tariffa forzo la tipologia a percentuale
064100141202     c* 0 % nessun calcolo con tipo tariffa - 100 % calcolo con tipo tariffa
064200141202     c                   if        evttvo = 'M'  and
064300141203     C                             evtvrv <> *BLANKS
064400141202     c                   eval      §2rtva = 'P'
064500141202     C                   ENDIF
064600000000     C*
064700000000     C* SE NELLA DS2R C'E' TIPO VALORIZZAZIONE P=PERCENTUALE VISUALIZZO LA %
064800000000     C     §2RTVA        IFEQ      'P'
064900000000     C                   SETOFF                                       92
065000000000     C                   ELSE
065100000000     C                   SETON                                        92
065200000000     C                   ENDIF
065300000000     C*
065400000000     C* SE NELLA DS2R C'E' PIU' DI UNO SCAGLIONE VISUALIZZO TALI CAMPI
065500000000     C     §2RPIU        IFEQ      'S'
065600000000     C                   SETOFF                                       93
065700000000     C                   ELSE
065800000000     C                   SETON                                        93
065900000000     C                   ENDIF
066000000000     C*
066100000000     C                   ENDIF
066200000000     C*
066300000000     C* NELLA DS2R C'E' UN FLAG PER CONSENTIRE UN SOLO, O PIU', SCAGLIONE
066400000000     C* CONTROLLO DA FARE SOLO IN INSERIMENTO
066500000000IF  2C     §2RPIU        IFNE      'S'
066600000000     C                   MOVE      V1CVOC        KVDVOC
066700000000     C                   MOVE      V1CPRG        KVDPRG
066800000000     C                   MOVE      V1CTTV        KVDTTV
066900131121     C     KE3EVD        SETLL     ECEVD06L
067000131121     C     KE3EVD        READE     ECEVD06L                               99
067100000000DO  4C     *IN99         DOWEQ     *OFF
067200000000IF  5C     EVDATB        IFEQ      *BLANKS
067300960130     C     EVDSGI        IFNE      V1CSGI
067400000000     C                   MOVEL     '1'           $ERR
067500000000     C                   MOVEL     MSG(24)       $MSG
067600000000     C                   EXSR      CARMSG
067700000000     C                   SETON                                        60  99
067800960130E   5C                   ENDIF
067900000000E   5C                   ENDIF
068000131121     C  N99KE3EVD        READE     ECEVD06L                               99
068100000000E   4C                   ENDDO
068200000000E   2C                   ENDIF
068300940726     C*
068400940726     C* SE NON E' PRESENTE IL TIPO RICERCA SCAGLIONE --> NON VISUALIZZARE SCAGL.
068500940726     C     EVTTIV        IFEQ      *BLANKS
068600940726     C                   SETON                                        93
068700940726     C                   ENDIF
068800000000     C*
068900000000E   1C                   ENDIF
069000000000     C*--------------------
069100000000     C* TIPO TARIFFA
069200000000     C*--------------------
069300000000     C                   MOVE      *BLANKS       V1CTTD
069400000000     C                   MOVEL     '2S'          §COD
069500000000     C                   MOVEL     *BLANKS       §KEY
069600000000     C                   MOVEL     V1CTTV        §KEY
069700000000     C                   EXSR      CHKTAB
069800000000     C                   MOVEL     §KEY          V1CTTV
069900000000     C                   MOVEL     §DES          V1CTTD
070000000000     C*
070100000000     C*?    V O C E    D I    C L I E N T E    ?
070200000000IF  1C     V1CTTV        IFEQ      'K'
070300000000     C*
070400000000     C* SE NELLA VOCE NON CI SONO I RIFERIMENTI TARIFFA X CLIENTE NON POSSO
070500000000     C* INSERIRE NESSUNA TARIFFA DI TIPO 'K'
070600000000     C     EVTRKI        IFEQ      *BLANKS
070700000000     C     EVTRKE        ANDEQ     *BLANKS
070800000000     C                   MOVEL     '1'           $ERR
070900000000     C                   MOVEL     MSG(40)       $MSG
071000000000     C                   EXSR      CARMSG
071100000000     C                   SETON                                        60
071200000000     C                   ENDIF
071300000000     C*--------------------
071400000000     C* CODICE CLIENTE (OBBLIGATORIO)
071500000000     C*--------------------
071600950718IF  2C     $ERR          IFEQ      '0'
071700000727     C                   CLEAR                   V1CRAG
071800000727     C* RICHIESTO ELENCO
071900950718     C                   SETOFF                                           99
072000000000     C     '?'           SCAN      V1CCLI                                 99
072100000727IF  3C     *IN99         IFEQ      *ON
072200950718     C                   MOVEL     DSFIRS        XALDUT           30
072300020923     C                   Z-ADD     1             XALKUT            1 0
072400950718     C                   MOVEL(P)  'A'           XALRAG           48
072500020923     C                   Z-ADD     DUTKCI        XALKCC            4 0
072600950718     C                   Z-ADD     9             XALSTA            1 0
072700980828     C                   MOVEL     *BLANKS       XALFLR           90
072800980828     C                   MOVEL     *BLANKS       XALDIT            3
072900000804     C                   Z-ADD     1             XALNUM
073000000804     C                   CALL      'XALFA3BR'
073100950718     C                   PARM                    XALDUT
073200950718     C                   PARM                    XALKUT
073300950718     C                   PARM                    XALRAG
073400950718     C                   PARM                    XALKCC
073500950718     C                   PARM                    XALSTA
073600980828     C                   PARM                    XALFLR
073700980828     C                   PARM                    XALDIT
073800000804     C                   PARM                    XALNUM            2 0
073900000804     C                   PARM                    XALKCM           80
074000000804     C                   PARM                    XALKSM          140
074100000804     C                   PARM                    XALKDM           60
074200000804     C     XALSTA        IFGT      -1
074300950718     C                   MOVEL(P)  XALRAG        V1CRAG
074400000804     C                   MOVEL(P)  XALKSM        V1CCLI
074500000804     C                   ELSE
074600000804     C                   CLEAR                   V1CCLI
074700000804     C                   ENDIF
074800000727E   3C                   ENDIF
074900000727     C* CONTROLLA VALIDITA' CODICE
075000000727     C                   MOVE      V1CCLI        A1                1
075100000727     C                   TESTN                   V1CCLI               28
075200000727IF  3C     *IN28         IFEQ      *OFF
075300000727     C     *IN28         OREQ      *ON
075400000727     C     A1            ANDLT     '0'
075500000727     C     V1CCLI        OREQ      *ZERO
075600000727     C                   MOVEL     '1'           $ERR
075700000727     C                   MOVEL     MSG(30)       $MSG
075800000727     C                   EXSR      CARMSG
075900000727     C                   SETON                                        62
076000000727X   3C                   ELSE
076100000727     C* RECUPERA DESCRIZIONE
076200000727     C                   CLEAR                   BS69DS
076300000727     C                   CLEAR                   ACODS
076400000727     C                   MOVEL     KNSIF         I69SIF
076500020923     C                   Z-ADD     DUTKCI        I69KCC
076600000727     C                   MOVEL     V1CCLI        I69KAC
076700000727     C                   CALL      'TIBS69R'
076800000727     C                   PARM                    BS69DS
076900000727     C                   PARM                    ACODS
077000000727     C                   PARM                    INDDS
077100000727     C                   PARM                    CLPDS
077200000727     C                   PARM                    CLSDS
077300000727     C                   MOVEL     ACORAG        V1CRAG
077400000727E   3C                   ENDIF
077500950718E   2C                   ENDIF
077600000000     C*--------------------
077700000000     C* TARIFFA CLIENTE
077800000000     C*--------------------
077900000000IF  2C     $ERR          IFEQ      '0'
078000000000     C                   MOVE      *BLANKS       V1CCTD
078100000000     C                   MOVEL     V1CCTR        $CTR              3
078200000000     C                   EXSR      CHKCTR
078300000000     C                   MOVEL     $CTR          V1CCTR
078400000000     C                   MOVEL     $CTD          V1CCTD
078500000000     C*
078600940722     C* ERRORE (CARATTERI ALFABETICI)
078700000000IF  3C     $ERR          IFEQ      '1'
078800000000     C                   MOVEL     MSG(12)       $MSG
078900000000     C                   EXSR      CARMSG
079000000000     C                   SETON                                        63
079100000000E   3C                   ENDIF
079200000000E   2C                   ENDIF
079300000000     C*
079400000000E   1C                   ENDIF
079500000000     C*
079600000000     C*?    V O C E    D I    C. A. P.    ?
079700000000IF  1C     V1CTTV        IFEQ      'P'
079800000000     C*
079900000000     C* SE NELLA VOCE NON CI SONO I RIFERIMENTI TARIFFA X C.A.P.  NON POSSO
080000000000     C* INSERIRE NESSUNA TARIFFA DI TIPO 'P'
080100000000     C     EVTRPI        IFEQ      *BLANKS
080200000000     C     EVTRPE        ANDEQ     *BLANKS
080300000000     C                   MOVEL     '1'           $ERR
080400000000     C                   MOVEL     MSG(40)       $MSG
080500000000     C                   EXSR      CARMSG
080600000000     C                   SETON                                        60
080700000000     C                   ENDIF
080800000000     C*--------------------
080900000000     C* CODICE C.A.P.
081000000000     C*--------------------
081100000000IF  2C     $ERR          IFEQ      '0'
081200131122     c                   clear                   v1dnaz
081300131122      **   essendo ITALIA definita sulla tab 15 come Blk
081400131122      **   deve sempre testare
081500131122     C                   MOVEL     1             KTBKUT
081600131122     C                   MOVEL     '15'          KTBCOD
081700131122     C                   MOVEl     v1cnaz        ktbkey
081800131122     C                   CLEAR                   DS15
081900131122     C     KEYTAB        CHAIN     TABEL00F                           99
082000131122   3 C                   IF        %found(tabel00f) and
082100131122     C                             tblflg = *BLANKS
082200131122     C                   MOVEL     TBLUNI        DS15
082300131122     C                   MOVEL     §15DES        v1dnaz
082400131122X    C                   ElsE
082500131122     C                   MOVEL     '1'           $ERR
082600131122     C                   MOVEL     MSG(48)       $MSG
082700131122     C                   EXSR      CARMSG
082800131122     C                   SETON                                        67
082900131122E    C                   ENDIF
083000131122      * x Italia
083100131122IF   C     $ERR          IFEQ      '0'
083200131122IF   C                   IF        v1cnaz = *blank and EVTRPI ='PP'
083300131122     c                               or
083400131122     C                             v1cnaz <>*blank and EVTRPE ='PP'
083500131122     c*
083600131122     c                   if         %len(%trim(%subst(v1cCAD:§15pcf)))
083700131122     c                                  > §15ecf
083800131122     C                   MOVEL     '1'           $ERR
083900131122     C                   MOVEL     MSG(47)       $MSG
084000131122     C                   EXSR      CARMSG
084100131122     C                   SETON                                        67
084200131122     c                   end
084300131122     c*
084400131122E    C                   endIF
084500131122E    C                   endIF
084600131122      *
084700131125IF  3C**** V1CCAd        IFne      *blank
084800131125     C**** $ERR          andEQ     '0'
084900131125     C     $ERR          IfeQ      '0'
085000131122     C                   MOVEL(p)  v1cNAZ        WNAR
085100131121     C                   MOVEL(P)  V1CCAd        WCAP
085200970627     C                   Z-ADD     *ZEROS        WDAT
085300970627     C                   EXSR      CHKCAP
085400970627IF  4C     $ERR          IFEQ      '1'
085500970627     C                   MOVEL     MSG(10)       $MSG
085600970627     C                   EXSR      CARMSG
085700970627     C                   SETON                                        67
085800970627E   4C                   ENDIF
085900970627E   3C                   ENDIF
086000970627E   2C                   ENDIF
086100970627     C*
086200000000E   1C                   ENDIF
086300000000     C*
086400000000     C*?    V O C E    D I    T A S S A Z I O N E    ?
086500000000IF  1C     V1CTTV        IFEQ      'T'
086600000000     C*
086700000000     C* SE NELLA VOCE NON CI SONO I RIFERIMENTI TARIFFA X TASSAZIONE NON POSSO
086800000000     C* INSERIRE NESSUNA TARIFFA DI TIPO 'T'
086900000000     C     EVTRTI        IFEQ      *BLANKS
087000000000     C     EVTRTE        ANDEQ     *BLANKS
087100000000     C                   MOVEL     '1'           $ERR
087200000000     C                   MOVEL     MSG(40)       $MSG
087300000000     C                   EXSR      CARMSG
087400000000     C                   SETON                                        60
087500000000     C                   ENDIF
087600000000     C*
087700000000     C*--------------------
087800000000     C* CODICE TASSAZIONE
087900000000     C*--------------------
088000000000IF  2C     $ERR          IFEQ      '0'
088100130911     c     '?'           scan      v1ccts                                 99
088200130911     c                   if        *in99
088300130911     c                   movel     'CT'          §COD
088400130911     c                   movel     v1ccts        §key
088500130911     c                   exsr      chktab
088600130911     c                   movel     §key          v1ccts
088700130911     c                   endif
088800130806     C                   MOVEL     'CT'          KTBCOD
088900130806     C                   MOVEL     v1ccts        KTBKEY
089000130806     C     KEYTAB        CHAIN     TABEL00F
089100130806   3 C                   IF        %found(tabel00f) and
089200130806     C                             tblflg = *BLANKS
089300130806     C                   MOVEL     TBLUNI        dsct
089400160725   4 C                   if        %subst(§ctcor:3:1) <> '9' OR
089401160725   4 C                             (%subst(§ctcor:3:1) = '9' AND
089402160725     C                             §CTUTF ='S')
089500130806     C                   MOVEL     §ctdes        V1DCTS
089600130806  x4 c                   else
089601160725     C                   MOVEL     '1'           $err
089700130806     C                   MOVEL     MSG(43)       $MSG
089800130806     C                   EXSR      CARMSG
089900130806  e4 c                   endif
090000130806  x4 c                   else
090100130806     C                   MOVEL     '1'           $err
090200130806  e4 c                   endif
090300130806X 4 3C     $ERR          IFEQ      '1'
090400000000     C                   MOVEL     MSG(16)       $MSG
090500000000     C                   EXSR      CARMSG
090600090429     c* se sono in inserimento blocco altrimenti avviso solo
090700130806IF5 1C                   IF        d60op1 <> 'O02' and
090800090429IF  1C                             d60op1 <> 'O04'
090900000000     C                   SETON                                        68
091000130806 x5  c                   else
091100090429     c                   eval      $err = '0'
091200130806E 5 3C                   ENDIF
091300130806E 4 3C                   ENDIF
091400130806E 3 2C                   ENDIF
091500000000     C*
091600130806E 2 1C                   ENDIF
091700000000     C*
091800000000     C*?    V O C E    D I    F I L I A L E    ?
091900000000IF  1C     V1CTTV        IFEQ      'F'
092000000000     C*
092100000000     C* SE NELLA VOCE NON CI SONO I RIFERIMENTI TARIFFA X FILIALE NON POSSO
092200000000     C* INSERIRE NESSUNA TARIFFA DI TIPO 'F'
092300000000     C     EVTRFI        IFEQ      *BLANKS
092400000000     C     EVTRFE        ANDEQ     *BLANKS
092500000000     C                   MOVEL     '1'           $ERR
092600000000     C                   MOVEL     MSG(40)       $MSG
092700000000     C                   EXSR      CARMSG
092800000000     C                   SETON                                        60
092900000000     C                   ENDIF
093000000000     C*
093100000000     C*--------------------
093200000000     C* CODICE FILIALE/TERMINAL
093300000000     C*--------------------
093400000000IF  2C     $ERR          IFEQ      '0'
093500000000     C                   MOVEL     *BLANKS       V1CFD1
093600000000     C     '?'           SCAN      V1CFT1                                 99
093700000000IF  4C     *IN99         IFEQ      *ON
093800971203     C                   CALL      'TNSD24R'
093900000000     C                   PARM                    §OFIL             3
094000000000     C                   PARM                    §OTIP             1
094100000000     C                   PARM                    §ODES            25
094200971203     C                   PARM                    §OPRA             3
094300000000     C                   MOVEL     §OFIL         V1CFT1
094400000000E   4C                   ENDIF
094500000000     C*
094600000000     C* AGGANCIO LA TABELLA 2Q CON IL RIFERIMENTO TARIFFA (ITALIA/ESTERO)
094700000000     C* E' SUFFICIENTE FARLO SOLO PER L'ITALIA, IN QUANTO I FLAG PER L'ITALIA E
094800000000     C* PER L'ESTERO DEVONO ESSERE UGUALI
094900020923     C                   MOVEL     1             KTBKUT
095000000000     C                   MOVEL     '2Q'          KTBCOD
095100000000     C                   MOVE      *BLANKS       KTBKEY
095200940728IF  4C     EVTRFI        IFNE      *BLANKS
095300000000     C                   MOVEL     EVTRFI        KTBKEY
095400940728X   4C                   ELSE
095500000000     C                   MOVEL     EVTRFE        KTBKEY
095600940728E   4C                   ENDIF
095700000000     C     KEYTAB        CHAIN     TABEL00F                           99
095800940728IF  4C     *IN99         IFEQ      '1'
095900000000     C     TBLFLG        ORNE      *BLANKS
096000000000     C                   CLEAR                   DS2Q
096100940728X   4C                   ELSE
096200000000     C                   MOVEL     TBLUNI        DS2Q
096300940728E   4C                   ENDIF
096400940726     C     §2QFAR        COMP      *BLANKS                                78
096500000000     C*
096600000000IF  4C     V1CFT1        IFGE      *ZERO
096700000000     C                   MOVEL     V1CFT1        KRGFIL
096800000000     C     KEYORG        CHAIN     AZORG01L                           64
096900000000IF  5C     *IN64         IFEQ      *ON
097000000000     C                   MOVEL     '1'           $ERR
097100000000     C                   MOVEL     MSG(14)       $MSG
097200000000     C                   EXSR      CARMSG
097300000000X   5C                   ELSE
097400000000     C                   MOVEL     ORGDES        V1CFD1
097500940831     C*------------------------------------------------------------------------*
097600940831     C* DALL' 30/08/94 I CONTROLLI SULLE FILIALI, SE ERRATI, PRODUCONO SOLO UN
097700940831     C* MESSAGGIO DI AVVERTIMENTO E NON FERMANO IL PGM (PER BOLLE PARTICOLARI)
097800940831     C*------------------------------------------------------------------------*
097900940831     C*
098000940729     C* SE NELLA DS2Q C'E' SCRITTO CHE LA 1^ FILIALE DEVE ESSERE TERMINAL E LA
098100940729     C* 2^ INVECE NO, CONTROLLO
098200940729IF  6C     §2QFPA        IFEQ      'T'
098300940729     C     §2QFAR        ANDNE     'T'
098400971020     C                   CLEAR                   DSLV55
098500971020     C                   MOVEL     *BLANKS       D55TLA
098600971020     C                   MOVEL     *BLANKS       D55TPT
098700971020     C                   Z-ADD     ORGFIL        D55LIN
098800971020     C                   Z-ADD     DATCOR        D55DRF
098900971020     C                   CALL      'FNLV55R'
099000971020     C                   PARM                    DSLV55
099100971020IF  7C     ORGFIL        IFNE      D55TFP
099200971020     C     ORGFIL        ANDNE     D55TFA
099300940729E   7C                   ENDIF
099400940729E   6C                   ENDIF
099500000000E   5C                   ENDIF
099600000000     C*
099700000000E   4C                   ENDIF
099800000000     C*
099900000000X   3C                   ELSE
100000000000     C                   MOVEL     '1'           $ERR
100100000000     C                   MOVEL     MSG(14)       $MSG
100200000000     C                   EXSR      CARMSG
100300000000     C                   SETON                                        64
100400000000E   2C                   ENDIF
100500000000     C*--------------------
100600000000     C* 2° FILIALE
100700000000     C*--------------------
100800000000IF  2C     $ERR          IFEQ      '0'
100900000000     C                   MOVEL     *BLANKS       V1CFD2
101000000000IF  3C     V1CFT2        IFNE      *BLANKS
101100000000     C     V1CFT2        ANDNE     *ZERO
101200000000     C     §2QFAR        ORNE      *BLANKS
101300000000     C     '?'           SCAN      V1CFT2                                 99
101400000000IF  4C     *IN99         IFEQ      *ON
101500971203     C                   CALL      'TNSD24R'
101600000000     C                   PARM                    §OFIL
101700000000     C                   PARM                    §OTIP
101800000000     C                   PARM                    §ODES
101900971203     C                   PARM                    §OPRA
102000000000     C                   MOVEL     §OFIL         V1CFT2
102100000000E   4C                   ENDIF
102200000000     C*
102300000000IF  4C     V1CFT2        IFGT      *ZERO
102400000000     C     §2QFAR        ORNE      *BLANKS
102500000000     C                   MOVEL     V1CFT2        KRGFIL
102600000000     C     KEYORG        CHAIN     AZORG01L                           65
102700000000IF  5C     *IN65         IFEQ      *ON
102800000000     C                   MOVEL     '1'           $ERR
102900000000     C                   MOVEL     MSG(14)       $MSG
103000000000     C                   EXSR      CARMSG
103100000000X   5C                   ELSE
103200000000     C                   MOVEL     ORGDES        V1CFD2
103300940729     C*
103400940729     C* SE NELLA DS2Q C'E' SCRITTO CHE LA 2^ FILIALE DEVE ESSERE TERMINAL E LA
103500940729     C* 1^ INVECE NO, CONTROLLO
103600940729IF  6C     §2QFPA        IFNE      'T'
103700940729     C     §2QFAR        ANDEQ     'T'
103800971020     C                   CLEAR                   DSLV55
103900971020     C                   MOVEL     *BLANKS       D55TLA
104000971020     C                   MOVEL     *BLANKS       D55TPT
104100971020     C                   Z-ADD     ORGFIL        D55LIN
104200971020     C                   Z-ADD     DATCOR        D55DRF
104300971020     C                   CALL      'FNLV55R'
104400971020     C                   PARM                    DSLV55
104500971020IF  7C     ORGFIL        IFNE      D55TFP
104600971020     C     ORGFIL        ANDNE     D55TFA
104700940729E   7C                   ENDIF
104800940729E   6C                   ENDIF
104900940729E   5C                   ENDIF
105000000000     C*
105100940729     C* NELLA DS2Q C'E' SCRITTO SE LE FILIALI DEVONO ESSERE TERMINAL-TERMINAL
105200940729IF  5C     §2QFPA        IFEQ      'T'
105300940729     C     §2QFAR        ANDEQ     'T'
105400941130     C                   MOVEL     'T'           KISPTA
105500940728     C                   MOVEL     V1CFT1        KISLNP
105600940728     C                   MOVEL     V1CFT2        KISLNA
105700941130     C     KEYCAS        CHAIN     AZCAS01L                           99
105800940729IF  6C     *IN99         IFEQ      *ON
105900941130E   6C                   ENDIF
106000940729E   5C                   ENDIF
106100000000     C*
106200000000     C* SE NELLA 2Q NON C'E' IL FLAG §2QFAR, NON DEVE ESSERCI IL COD.FILIALE
106300940729IF  5C     §2QFAR        IFEQ      *BLANKS
106400000000     C     V1CFT2        ANDNE     *BLANKS
106500000000     C     V1CFT2        ANDNE     *ZERO
106600000000     C                   MOVEL     '1'           $ERR
106700000000     C                   MOVEL     MSG(38)       $MSG
106800000000     C                   EXSR      CARMSG
106900000000     C                   SETON                                        65
107000940729E   5C                   ENDIF
107100000000     C*
107200000000E   4C                   ENDIF
107300000000E   3C                   ENDIF
107400000000     C*
107500000000E   2C                   ENDIF
107600940729E   1C                   ENDIF
107700000000     C*
107800000000     C*?    CAMPI  COMUNI  A  TUTTI  I  TIPI  DI  TARIFFA     ?
107900000000     C*--------------------
108000000000     C* DATA DECORRENZA (SOLO SE INSERIMENTO)
108100000000     C*--------------------
108200000000IF  2C     $ERR          IFEQ      '0'
108300000000     C                   Z-ADD     V1CDDT        $DAT              8 0
108400000000     C                   EXSR      CHKDAT
108500000000     C                   Z-ADD     $DAT          V1CDDT
108600000000IF  3C     $ERR          IFEQ      '1'
108700000000     C                   EXSR      CARMSG
108800000000     C                   SETON                                        69
108900000000E   3C                   ENDIF
109000000000E   2C                   ENDIF
109100000000     C*--------------------
109200000000     C* DATA SCADENZA
109300000000     C*--------------------
109400000000IF  1C     $ERR          IFEQ      '0'
109500000000     C                   Z-ADD     V1CDST        $DAT
109600000000     C                   EXSR      CHKDAT
109700000000     C                   Z-ADD     $DAT          V1CDST
109800000000IF  2C     $ERR          IFEQ      '1'
109900000000     C                   EXSR      CARMSG
110000000000     C                   SETON                                        71
110100000000E   2C                   ENDIF
110200000000     C*
110300000000     C* MAGGIORE DELLA DECORRENZA
110400000000IF  2C     $ERR          IFEQ      '0'
110500000000     C                   Z-ADD     V1CDDT        G02DAT
110600000000     C                   Z-ADD     *ZEROS        G02INV
110700000000     C                   MOVEL     '0'           G02ERR
110800000000     C                   CALL      'XSRDA8'
110900000000     C                   PARM                    WLBDAT
111000000000     C                   Z-ADD     G02INV        DDTV1C            8 0
111100000000     C*
111200000000     C                   Z-ADD     V1CDST        G02DAT
111300000000     C                   Z-ADD     *ZEROS        G02INV
111400000000     C                   MOVEL     '0'           G02ERR
111500000000     C                   CALL      'XSRDA8'
111600000000     C                   PARM                    WLBDAT
111700000000     C                   Z-ADD     G02INV        DSTV1C            8 0
111800000000     C*
111900000000IF  3C     DSTV1C        IFLT      DDTV1C
112000000000     C                   MOVEL     '1'           $ERR
112100000000     C                   MOVEL     MSG(23)       $MSG
112200000000     C                   EXSR      CARMSG
112300000000     C                   SETON                                        6971
112400100330     c                   else
112500100330     c* se voce da isolare non può avere una scdenza
112600100422     c                   if        evtvrv = '26' and dstv1c <> 20391231
112700100330     C                   MOVEL     '1'           $ERR
112800100330     C                   MOVEL     MSG(31)       $MSG
112900100330     C                   EXSR      CARMSG
113000100330     C                   SETON                                        71
113100000000E   2C                   ENDIF
113200100330E   2C                   ENDIF
113300000000E   1C                   ENDIF
113400100330E   3C                   ENDIF
113500951207     C*--------------------
113600010220     C* INSERIMENTO -> TARIFFA INESISTENTE
113700951207     C*--------------------
113800951207IF  2C     $ERR          IFEQ      '0'
113900951207IF  2C     D60OP1        IFEQ      'F06'
114000951207     C                   Z-ADD     D60VOC        KVDVOC
114100951207     C                   Z-ADD     D60PRG        KVDPRG
114200951207     C                   MOVE      TTVKTV        KVDTTV
114300010220     C                   Z-ADD     FT1KTV        KVDFT1
114400010220     C                   Z-ADD     FT2KTV        KVDFT2
114500951207     C                   Z-ADD     CLIKTV        KVDCLI
114600951207     C                   Z-ADD     CTRKTV        KVDCTR
114700951207     C                   MOVE      CTSKTV        KVDCTS
114800131121     C                   MOVEL     CAdKTV        KVDCAd
114900131121     C                   MOVEL     nazKTV        KVDnaz
115000951207     C                   Z-ADD     DDTKTV        KVDDDT
115100951207     C                   Z-ADD     SGIKTV        KVDSGI
115200131121     C     KEYEVD        SETLL     ECEVD06L                               99
115300951207IF  3C     *IN99         IFEQ      *ON
115400951207     C                   MOVEL     '1'           $ERR
115500951207     C                   MOVEL     MSG(11)       $MSG
115600951207     C                   EXSR      CARMSG
115700951207     C                   SETON                                        606263
115800951207     C                   SETON                                        6970
115900951207E   3C                   ENDIF
116000951207E   2C                   ENDIF
116100951207E   1C                   ENDIF
116200010220     C*--------------------
116300010220     C* DUPLICA -> TARIFFA INESISTENTE
116400010220     C*--------------------
116500010220IF  2C     $ERR          IFEQ      '0'
116600010220IF  2C     D60OP1        IFEQ      'O03'
116700020702     C                   Z-ADD     V1CVOC        KVDVOC
116800020702     C                   Z-ADD     V1CPRG        KVDPRG
116900020702     C                   MOVE      V1CTTV        KVDTTV
117000010220     C                   MOVE      V1CFT1        KVDFT1
117100010220     C                   MOVE      V1CFT2        KVDFT2
117200020702     C                   MOVE(P)   V1CCLI        KVDCLI
117300020702     C                   MOVE(P)   V1CCTR        KVDCTR
117400020702     C                   MOVE      V1CCTS        KVDCTS
117500131121     C                   movel     V1CCAd        KVDCAd
117600131121     C                   movel     V1Cnaz        KVDnaz
117700020702     C                   Z-ADD     V1CDDT        KVDDDT
117800020702     C                   Z-ADD     V1CSGI        KVDSGI
117900131121     C     KEYEVD        SETLL     ECEVD06L                               99
118000010220IF  3C     *IN99         IFEQ      *ON
118100010220     C                   MOVEL     '1'           $ERR
118200010220     C                   MOVEL     MSG(11)       $MSG
118300010220     C                   EXSR      CARMSG
118400010220     C                   SETON                                        606263
118500010220     C                   SETON                                        6970
118600010220E   3C                   ENDIF
118700010220E   2C                   ENDIF
118800010220E   1C                   ENDIF
118900000000     C*--------------------
119000000000     C* VALIDITA' VOCE PER NON AVERE PERIODI ACCAVALLATI IN ARCHIVIO
119100000000     C*--------------------
119200000000IF  1C     $ERR          IFEQ      '0'
119300000000     C*
119400000000     C                   Z-ADD     V1CVOC        KVDVOC
119500000000     C                   Z-ADD     V1CPRG        KVDPRG
119600000000     C                   MOVEL     V1CTTV        KVDTTV
119700000000     C                   MOVEL     V1CFT1        KVDFT1
119800000000     C                   MOVEL     V1CFT2        KVDFT2
119900000000     C                   MOVEL     V1CCLI        KVDCLI
120000000000     C                   MOVEL     V1CCTR        KVDCTR
120100000000     C                   MOVEL     V1CCTS        KVDCTS
120200131121     C                   movel     V1CCAd        KVDCAd
120300131121     C                   movel     V1Cnaz        KVDnaz
120400131121     C     KE2EVD        SETLL     ECEVD06L
120500131121     C     KE2EVD        READE(N)  ECEVD06L                               99
120600000000DO  2C     *IN99         DOWEQ     *OFF
120700951207IF  3C     EVDATB        IFEQ      *BLANKS
120800000000     C*
120900000000     C* SE SONO IN MODIFI/ANNULL/RIPRIS DAL CONTROLLO ESCLUDO LA VOCE IN LINEA
121000000000IF  4C     D60OP1        IFNE      'F06'
121100010220     C     D60OP1        ANDNE     'O03'
121200000000     C     DDTKTV        ANDEQ     EVDDDT
121300000000X   4C                   ELSE
121400000000     C*
121500000000IF  5C     DDTV1C        IFLE      EVDDST
121600000000IF  6C     DSTV1C        IFLT      EVDDDT
121700000000X   6C                   ELSE
121800940725IF  7C     EVDSGI        IFEQ      V1CSGI
121900000000     C                   MOVEL     '1'           $ERR
122000000000     C                   MOVEL     MSG(27)       $MSG
122100000000     C                   EXSR      CARMSG
122200000000     C                   SETON                                        697199
122300940725     C* APPENA TROVO L'ERRORE ESCO DAL CICLO
122400940725E   7C                   ENDIF
122500940725E   6C                   ENDIF
122600000000E   5C                   ENDIF
122700000000E   4C                   ENDIF
122800951207E   3C                   ENDIF
122900131121     C  N99KE2EVD        READE(N)  ECEVD06L                               99
123000000000E   2C                   ENDDO
123100000000     C*
123200000000E   1C                   ENDIF
123300000000     C*--------------------
123400000000     C* SI NARRA CHE ... IL PERDIODO IMMESSO DEVE ESSERE COMPRESO IN UNA DELLE
123500000000     C* VOCI PRESENTI IN TESTATA (ECEVT00F)
123600000000     C*--------------------
123700000000IF  1C     $ERR          IFEQ      '0'
123800000000     C*
123900000000     C* LEGGO IL RECORD DI ECEVT POSIZIONANDOMI PER DATA DECORRENZA
124000000000     C                   Z-ADD     V1CVOC        KVTVOC
124100000000     C                   Z-ADD     V1CPRG        KVTPRG
124200000000     C     KEYEVT        SETLL     ECEVT01L
124300000000     C     V1CVOC        READE     ECEVT01L                               99
124400000000     C*
124500000000     C* SE LA DATA SCADENZA LETTA (DA ECEVT) E' INFERIORE ALLA SCADENZA A VIDEO
124600000000IF  2C  N99DDTV1C        IFGE      EVTDDV
124700000000     C     DSTV1C        ANDLE     EVTDSV
124800000000     C                   ELSE
124900000000     C                   MOVEL     '1'           $ERR
125000000000     C                   MOVEL     MSG(39)       $MSG
125100000000     C                   EXSR      CARMSG
125200000000     C                   SETON                                        6971
125300000000E   2C                   ENDIF
125400000000     C*
125500000000E   1C                   ENDIF
125600000000     C*--------------------
125700940725     C* SCAGLIONE INFERIORE TARIFFA (ACCETTO ANCHE ZERO)
125800000000     C*--------------------
125900000000     C*--------------------
126000000000     C* SCAGLIONE SUPERIORE TARIFFA
126100000000     C*--------------------
126200000000IF  1C     $ERR          IFEQ      '0'
126300000000IF  2C     *IN93         IFEQ      *OFF
126400940726IF  3C     V1CSGF        IFLE      V1CSGI
126500000000     C                   MOVEL     '1'           $ERR
126600000000     C                   MOVEL     MSG(13)       $MSG
126700000000     C                   EXSR      CARMSG
126800000000     C                   SETON                                        66
126900000000E   3C                   ENDIF
127000000000X   2C                   ELSE
127100000000     C                   MOVEL     *ALL'9'       V1CSGF
127200000000E   2C                   ENDIF
127300000000E   1C                   ENDIF
127400130919IF  2C     *IN93         IFEQ      *OFF
127500130920IF  1C     D60OP1        andeq     'F06'
127600130919     C                   IF        V1CSGI > 0
127700130920      *
127800130920     c                   clear                   w1ccli
127900130920     c                   clear                   w1cft1
128000130920     c                   clear                   w1cft2
128100130920     c                   clear                   w1cctr
128200130920     c                   if        v1ccli <> *blank
128300130920     c                   move      v1ccli        w1ccli
128400130920     c                   endif
128500130920     c                   if        v1cft1 <> *blank
128600130920     c                   move      v1cft1        w1cft1
128700130920     c                   endif
128800130920     c                   if        v1cft2 <> *blank
128900130920     c                   move      v1cft2        w1cft2
129000130920     c                   endif
129100130920     c                   if        v1cctr <> *blank
129200130920     c                   move      v1cctr        w1cctr
129300130920     c                   endif
129400130920     C                   z-add     v1cddt        g02dat
129500130920     C                   z-add     *zeros        g02inv
129600130920     C                   movel     '0'           g02err
129700130920     C                   call      'XSRDA8'
129800130920     C                   parm                    wlbdat
129900130920     C                   z-add     g02inv        w1cddt
130000130920      *
130100131121     c     kscagl1       setgt     ecevdx6l
130200131121     C     kscagl        readpe    ecevdx6l
130300131121     c                   if        not %eof(ecevdx6l)
130400130920     c                   if        v1csgi <= xevdsgf
130500130919     C                   MOVEL     MSG(46)       $MSG
130600130919     C                   EXSR      CARMSG
130700130920     C                   SETON                                        70
130800130919     C                   endif
130900130919     C                   endif
131000131121     c     kscagl1       setgt     ecevdx6l
131100131121     C     kscagl        reade     ecevdx6l
131200131121     c                   if        not %eof(ecevdx6l)
131300130920     c                   if        v1csgf >= xevdsgi
131400130919     C                   MOVEL     MSG(46)       $MSG
131500130919     C                   EXSR      CARMSG
131600130919     C                   SETON                                        66
131700130919     C                   endif
131800130919     C                   endif
131900130920     C                   ENDIF
132000130919     C                   ENDIF
132100000000     C*--------------------
132200000000     C* IMPORTO UNIT. TARIFFA
132300940725     C* LO ACCETTO SE TIPO "QUANTITA'" O "IMPORTO FISSO" (DS2R)
132400000000     C*--------------------
132500000000IF  1C     $ERR          IFEQ      '0'
132600940726     C     *IN92         ANDEQ     *OFF
132700940726IF  2C     V1CTIV        IFNE      *BLANKS
132800940726IF  3C     V1CITR        IFGT      *ZEROS
132900940726     C                   MOVEL     '1'           $ERR
133000940726     C                   MOVEL     MSG(25)       $MSG
133100940726     C                   EXSR      CARMSG
133200940726     C                   SETON                                        72
133300940726E   3C                   ENDIF
133400940726E   2C                   ENDIF
133500000000E   1C                   ENDIF
133600000000     C*--------------------
133700000000     C* PERCENTUALE DA APPLICARE  (E' IN ALTERNATIVA ALL'IMPORTO UNIT.)
133800000000     C* SE NELLA VOCE NON C'E' "TIPO IMPONIBILE" NON DEVE ESSERCI LA %
133900000000     C*--------------------
134000000000IF  1C     $ERR          IFEQ      '0'
134100000000     C     *IN92         ANDEQ     *OFF
134200000000IF  2C     V1CPER        IFGT      100
134201150427     c* per le maggiorazioni di isolamento è ammessa la % > 100
134202150427     c                   if        evttvo = 'V' or
134203150427     c                             (evttvo = 'M' and (
134204150427     C                             v1cvoc < mi_§2ovocda or
134205150427     C                             v1cvoc > mi_§2ovoca ))
134300000000     C                   MOVEL     '1'           $ERR
134400000000     C                   MOVEL     MSG(41)       $MSG
134500000000     C                   EXSR      CARMSG
134600000000     C                   SETON                                        76
134701150427     c                   end
134702150427     c                   else
134800141202     c* per maggiorazioni con tipo tariffa forzo la tipologia a percentuale
134900141202     c* 0 % nessun calcolo con tipo tariffa - 100 % calcolo con tipo tariffa
135000141202     c                   if        evttvo = 'M'  and
135100141203     C                             evtvrv <> *BLANKS
135200141203     c                   if        v1cper <> 100
135300141202     c                             and v1cper <> 0
135400141203     c                             or v1cseg <> '+'
135500141202     C                   MOVEL     '1'           $ERR
135600141202     C                   MOVEL     MSG(44)       $MSG
135700141202     C                   EXSR      CARMSG
135800141202     C                   SETON                                        76
135900000000E   2C                   ENDIF
136000141203E   2C                   ENDIF
136100141202E   2C                   ENDIF
136200000000      *
136300000000E   1C                   ENDIF
136400000000     C*--------------------
136500000000     C* IMPORTO MINIMO TARIFFA
136600000000     C*--------------------
136700940725     C* ACCETTO SOLO SE C'E' LA PERCENTUALE, ALTRIMENTI VUOTO
136800000000IF  1C     $ERR          IFEQ      '0'
136900000000     C     *IN92         ANDEQ     *OFF
137000940725IF  2C     V1CPER        IFEQ      *ZERO
137100000000     C*
137200940725IF  3C     V1CIMI        IFNE      *ZERO
137300000000     C                   MOVEL     '1'           $ERR
137400000000     C                   MOVEL     MSG(32)       $MSG
137500000000     C                   EXSR      CARMSG
137600000000     C                   SETON                                        73
137700000000E   3C                   ENDIF
137800000000     C*
137900000000E   2C                   ENDIF
138000000000E   1C                   ENDIF
138100000000     C*--------------------
138200000000     C* IMPORTO MASSIMO TARIFFA
138300000000     C*--------------------
138400940725     C* ACCETTO SOLO SE C'E' LA PERCENTUALE, ALTRIMENTI VUOTO
138500000000IF  1C     $ERR          IFEQ      '0'
138600000000     C     *IN92         ANDEQ     *OFF
138700940725IF  2C     V1CPER        IFEQ      *ZERO
138800000000     C*
138900940725IF  3C     V1CIMA        IFNE      *ZERO
139000000000     C                   MOVEL     '1'           $ERR
139100000000     C                   MOVEL     MSG(32)       $MSG
139200000000     C                   EXSR      CARMSG
139300000000     C                   SETON                                        74
139400000000E   3C                   ENDIF
139500940725     C*
139600940725x   2C                   ELSE
139700940725     C*
139800940725IF  3C     V1CIMA        IFLT      V1CIMI
139900940725     C                   MOVEL     '1'           $ERR
140000940725     C                   MOVEL     MSG(36)       $MSG
140100940725     C                   EXSR      CARMSG
140200940725     C                   SETON                                        74
140300940725E   3C                   ENDIF
140400940725     C*
140500000000     C*
140600000000E   2C                   ENDIF
140700000000E   1C                   ENDIF
140800000000     C*--------------------
140900000000     C* SEGNO IMPORTO TARIFFA
141000000000     C*--------------------
141100000000IF  1C     $ERR          IFEQ      '0'
141200000000IF  2C     V1CSEG        IFEQ      *BLANKS
141300000000     C                   MOVEL     '1'           $ERR
141400000000     C                   MOVEL     MSG(33)       $MSG
141500000000     C                   EXSR      CARMSG
141600000000     C                   SETON                                        77
141700100422     c                   else
141800100422     c* se maggiorazione con voci da isolare le tariffe devono essere
141900100422     c* tutte positive o tutte negative a seconda del flag Applica S/N
142000100422     c                   if        flgsegtar = '1'
142100100422     C                   Z-ADD     V1CVOC        KVTVOC
142200100422     C                   Z-ADD     V1CPRG        KVTPRG
142300100422     c     keyevt        setll     ecevm01l
142400100422     c                   do        *hival
142500100422     c     keyevt        reade     ecevm01l
142600100422     c                   if        %eof(ecevm01l)
142700100422     c                   leave
142800100422     c                   end
142900100422     c                   if        (evmapp = 'S' and v1cseg = '+') or
143000100422     c                             (evmapp = 'N' and v1cseg = '-')
143100100422     C                   MOVEL     '1'           $ERR
143200100422     C                   MOVEL     MSG(42)       $MSG
143300100422     C                   EXSR      CARMSG
143400100422     C                   SETON                                        77
143500100422     c                   leave
143600100422     c                   end
143700100422     c                   enddo
143800100422     c                   end
143900000000E   2C                   ENDIF
144000000000E   1C                   ENDIF
144100000000     C*--------------------
144200000000     C* RAPPORTO PESO VOLUME
144300940725     C* LO ACCETTO SOLO SE NELLA VOCE LO CONSENTE IL TIPO RIFERIMENTO
144400000000     C*--------------------
144500000000IF  1C     $ERR          IFEQ      '0'
144600940725IF  2C     §2RRPV        IFEQ      'N'
144700000000IF  3C     V1CRPV        IFNE      *ZERO
144800000000     C                   MOVEL     '1'           $ERR
144900000000     C                   MOVEL     MSG(15)       $MSG
145000000000     C                   EXSR      CARMSG
145100000000     C                   SETON                                        75
145200000000E   3C                   ENDIF
145300961202X   2C                   ELSE
145400961202IF  3C     EVTSPT        IFGT      '1'
145500961202     C     V1CRPV        ANDEQ     *ZEROS
145600961202     C                   MOVEL     '1'           $ERR
145700961202     C                   MOVEL     MSG(26)       $MSG
145800961202     C                   EXSR      CARMSG
145900961202     C                   SETON                                        75
146000961202E   3C                   ENDIF
146100961202E   2C                   ENDIF
146200000000E   1C                   ENDIF
146300100331     C*--------------------
146400100331     C* SE VOCE DA ISOLARE PER MAGGIORAZ. si deve inserire solo cartello
146500100331     c* 100%
146600100331     C*--------------------
146700100331IF  1C     $ERR          IFEQ      '0'
146800100422     C                   IF        evtvrv = '26' and
146900100422     c                             (v1cttv <> 'C' or v1cper <> 100
147000100331     c                             or v1cseg <> '+')
147100100331     C                   MOVEL     '1'           $ERR
147200100331     C                   MOVEL     MSG(9)        $MSG
147300100331     C                   EXSR      CARMSG
147400100331     c                   end
147500100331     c                   end
147600000000     C*
147700000000     C*--------------------
147800000000     C* SE ESEGUITA ROUTINE DI PUNTO INTERROGATIVO SIMULO ERRORE PER RIEMETTERE
147900000000     C*--------------------
148000000000IF  1C     $PUNTO        IFEQ      '1'
148100000000     C                   MOVE      '1'           $ERR
148200000000     C                   MOVE      '1'           $TESVD
148300000000E   1C                   ENDIF
148400000000     C*
148500000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
148600000000     C* CURSORE SUL CAMPO DEL CONTROL
148700000000IF  1C     $ERR          IFEQ      '1'
148800000000     C                   MOVEL     '0'           *IN43
148900000000X   1C                   ELSE
149000000000     C                   MOVEL     '1'           *IN43
149100000000E   1C                   ENDIF
149200000000     C*
149300000000     C                   ENDSR
149400970627     C*------------------------------------------------------------------------*
149500970627     C* CHKCAP - CONTROLLA IL CAP
149600970627     C*------------------------------------------------------------------------*
149700970627     C     CHKCAP        BEGSR
149800970627     C*
149900970627     C     *LIKE         DEFINE    I95NAR        WNAR
150000970627     C     *LIKE         DEFINE    I95CAP        WCAP
150100970627     C     *LIKE         DEFINE    I95DAT        WDAT
150200131122     C     *LIKE         DEFINE    O95VER        Wver
150300970627     C*
150400970627     C* CONTROLLA IL CAP
150500970627     C                   CLEAR                   DSSI95
150600970627     C                   MOVEL     *BLANKS       I95TLA
150700970627     C                   MOVEL     '3'           I95TCN
150800970627     C                   MOVEL     WNAR          I95NAR
150900970627     C                   MOVEL     WCAP          I95CAP
151000970627     C                   Z-ADD     WDAT          I95DAT
151100970627     C                   CALL      'TISI95R'
151200970627     C                   PARM                    DSSI95
151300970627IF  1C     O95ERR        IFNE      *BLANKS
151400131122     c* verifico se è un cap fittizio
151500131122     c     kazcpc        chain     azcpc01l
151600131122     c                   if        not %found(azcpc01l)
151700970627     C                   MOVEL     '1'           $ERR
151800131122E   1C                   ENDIF
151900970627E   1C                   ENDIF
152000970627     C*
152100970627     C                   ENDSR
152200100422     C*------------------------------------------------------------------------*
152300100422     C* CHKCTR - CONTROLLA CODICE TARIFFA
152400100422     C*          INPUT --> $CTR  : CODICE TARIFFA ALFABETICO
152500100422     C*          OUTPUT--> $ERR  : 1-ERRORE, 0-NO ERRORE
152600100422     C*------------------------------------------------------------------------*
152700100422     C     CHKCTR        BEGSR
152800000000     C*
152900000000     C     '?'           SCAN      $CTR                                   99
153000940722IF  1C     *IN99         IFEQ      *ON
153100000000     C                   CLEAR                   PARAM4
153200950719     C                   MOVEL     V1CCLI        PARCOD
153300950719     C                   MOVEL     '1'           PARID
153400020923     C                   Z-ADD     DUTKCI        PARCCC
153500000000     C                   MOVEL     PARAM4        KPJBU
153600950718     C                   CALL      'TNTA36R'
153700000000     C                   PARM                    KPJBA
153800000000     C                   MOVEL     KPJBU         PARAM4
153900000000     C*
154000000000     C                   MOVEL     PARCTR        $CTR
154100000000     C                   MOVEL     PARDCV        $CTD
154200000000     C                   MOVEL     '1'           $PUNTO
154300000000     C*
154400000000     C* INSERITA TARIFFA CONTROLLO ESISTENZA
154500950719X   1C                   ELSE
154600950719IF  2C     $CTR          IFGE      *ZEROS
154700950719     C                   MOVEL     V1CCLI        KTAKSC
154800000000     C                   MOVEL     $CTR          KTACTR
154900000000     C*
155000941128     C     KEYTAM        CHAIN     TNTAM01L                           99
155100940722     C*
155200940722     C* SE INESISTENTE VA BENE LO STESSO: EVIDENZIO PERO' LA COSA
155300000000IF  4C     *IN99         IFEQ      *ON
155400940722     C     $CTR          ANDNE     '999'
155500940722     C                   MOVEL     MSG(45)       $CTD
155600940722     C                   SETON                                        10
155700000000X   4C                   ELSE
155800000000     C     *LIKE         DEFINE    TAMDCV        $CTD
155900000000     C                   MOVEL     TAMDCV        $CTD
156000000000E   4C                   ENDIF
156100000000     C*
156200000000     C* INSERITI CARATTERI ALFABETICI, ERRORE
156300000000X   3C                   ELSE
156400000000     C                   MOVEL     '1'           $ERR
156500000000E   3C                   ENDIF
156600000000     C*
156700000000E   2C                   ENDIF
156800000000     C*
156900000000     C                   ENDSR
157000000000     C*------------------------------------------------------------------------*
157100000000     C* CHKDAT - CONTROLLA LA DATA
157200000000     C*          INPUT --> $DAT:  DATA
157300000000     C*------------------------------------------------------------------------*
157400000000     C     CHKDAT        BEGSR
157500000000     C*
157600000000     C* OBBLIGATORIA
157700000000IF  1C     $DAT          IFEQ      *ZEROS
157800000000     C                   MOVEL     '1'           $ERR
157900000000     C                   MOVEL     MSG(8)        $MSG
158000000000X   1C                   ELSE
158100000000     C* CORRETTA
158200000000     C                   Z-ADD     $DAT          G02DAT
158300000000     C                   Z-ADD     *ZEROS        G02INV
158400000000     C                   MOVEL     '0'           G02ERR
158500000000     C                   CALL      'XSRDA8'
158600000000     C                   PARM                    WLBDAT
158700000000     C*
158800000000IF  2C     G02ERR        IFEQ      '1'
158900000000     C                   MOVEL     '1'           $ERR
159000000000     C                   MOVEL     MSG(8)        $MSG
159100000000X   2C                   ELSE
159200000000     C                   Z-ADD     G02DAT        $DAT
159300000000E   2C                   ENDIF
159400000000E   1C                   ENDIF
159500000000     C*
159600000000     C                   ENDSR
159700000000     C*------------------------------------------------------------------------*
159800000000     C* CHKTAB - CONTROLLA UN ELEMENTO IN TABELLA
159900000000     C*          INPUT --> §COD: CODICE TABELLA
160000000000     C*                    $KEY: CODICE ELEMENTO
160100000000     C*------------------------------------------------------------------------*
160200000000     C     CHKTAB        BEGSR
160300000000     C*
160400000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
160500000000     C     '?'           SCAN      §KEY                                   99
160600000000     C     *IN99         IFEQ      *ON
160700020923     C                   MOVEL     1             §KUT
160800000000     C                   MOVE      *BLANKS       §DES
160900000000     C                   CALL      'X§TABER'
161000000000     C                   PARM                    §KUT              1 0
161100000000     C                   PARM                    §COD              2
161200000000     C                   PARM                    §KEY              8
161300000000     C                   PARM                    §DES             30
161400000000     C                   MOVEL     '1'           $PUNTO            1
161500000000     C*
161600000000     C                   ENDIF
161700000000     C*
161800000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
161900020923     C                   MOVEL     1             KTBKUT
162000000000     C                   MOVEL     §COD          KTBCOD
162100000000     C                   MOVE      *BLANKS       KTBKEY
162200000000     C                   MOVEL     §KEY          KTBKEY
162300000000     C     KEYTAB        CHAIN     TABEL00F                           99
162400000000     C     *IN99         IFEQ      '1'
162500000000     C     TBLFLG        ORNE      *BLANKS
162600000000     C                   MOVEL     '1'           $ERR
162700000000     C                   MOVEL     *ALL'*'       §DES
162800000000     C                   ELSE
162900000000     C                   MOVEL     TBLUNI        §DES
163000000000     C                   ENDIF
163100000000     C*
163200000000     C                   ENDSR
163300000000     C*------------------------------------------------------------------------*
163400000000     C* CHKRIT - CONTROLLA UN ELEMENTO IN TABELLA
163500000000     C*          INPUT --> §TTV: TIPO TARIFFA
163600000000     C*                    $KEY: CODICE ELEMENTO
163700000000     C*------------------------------------------------------------------------*
163800000000     C     CHKRIT        BEGSR
163900000000     C*
164000000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
164100000000     C     '?'           SCAN      §KEY                                   99
164200000000     C     *IN99         IFEQ      *ON
164300020923     C                   MOVEL     1             §KUT
164400000000     C                   MOVE      *BLANKS       §DES
164500000000     C                   CALL      'TREC65R'
164600000000     C                   PARM                    §KUT              1 0
164700000000     C                   PARM                    §KEY              8
164800000000     C                   PARM                    §DES             30
164900000000     C                   PARM                    §TTV              1
165000000000     C                   MOVEL     '1'           $PUNTO            1
165100000000     C*
165200000000     C                   ENDIF
165300000000     C*
165400000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
165500020923     C                   MOVEL     1             KTBKUT
165600000000     C                   MOVEL     '2Q'          KTBCOD
165700000000     C                   MOVE      *BLANKS       KTBKEY
165800000000     C                   MOVEL     §KEY          KTBKEY
165900000000     C     KEYTAB        CHAIN     TABEL00F                           99
166000000000     C     *IN99         IFEQ      '1'
166100000000     C     TBLFLG        ORNE      *BLANKS
166200000000     C                   MOVEL     '1'           $ERR
166300000000     C                   MOVEL     *ALL'*'       §DES
166400000000     C                   ELSE
166500000000     C                   MOVEL     TBLUNI        §DES
166600000000     C                   ENDIF
166700000000     C*
166800000000     C*
166900000000     C                   ENDSR
167000000000     C*------------------------------------------------------------------------*
167100000000     C*  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
167200000000     C*------------------------------------------------------------------------*
167300000000     C     RECVID        BEGSR
167400000000     C*
167500000000     C                   Z-ADD     D60VOC        V1CVOC
167600000000     C                   Z-ADD     D60PRG        V1CPRG
167700000000     C                   Z-ADD     D60DDT        V1CDDT
167800000000     C                   MOVEL     EVDTTV        V1CTTV
167900000000     C                   MOVEL     EVDFT1        V1CFT1
168000000000     C                   MOVEL     EVDFT2        V1CFT2
168100010112     C                   MOVEL     EVDCLI        V1CCLI
168200000000     C                   MOVEL     EVDCTR        V1CCTR
168300000000     C                   MOVEL     EVDCTS        V1CCTS
168400131121     C                   movel     EVDCAd        V1CCAd
168500131121     C                   movel     EVDnaz        V1Cnaz
168600000000     C                   Z-ADD     EVDDDT        G02INV
168700000000     C                   Z-ADD     *ZEROS        G02DAT
168800000000     C                   MOVEL     '3'           G02ERR
168900000000     C                   CALL      'XSRDA8'
169000000000     C                   PARM                    WLBDAT
169100000000     C                   Z-ADD     G02DAT        V1CDDT
169200000000     C                   Z-ADD     EVDDST        G02INV
169300000000     C                   Z-ADD     *ZEROS        G02DAT
169400000000     C                   MOVEL     '3'           G02ERR
169500000000     C                   CALL      'XSRDA8'
169600000000     C                   PARM                    WLBDAT
169700000000     C                   Z-ADD     G02DAT        V1CDST
169800000000     C                   Z-ADD     EVDSGI        V1CSGI
169900000000     C                   Z-ADD     EVDSGF        V1CSGF
170000000000     C                   Z-ADD     EVDITR        V1CITR
170100000000     C                   Z-ADD     EVDIMI        V1CIMI
170200000000     C                   Z-ADD     EVDIMA        V1CIMA
170300000000     C                   MOVEL     EVDSEG        V1CSEG
170400000000     C                   Z-ADD     EVDPER        V1CPER
170500000000     C                   Z-ADD     EVDRPV        V1CRPV
170600981015IF  1C     EVDATB        IFNE      *BLANKS
170700981015     C                   MOVEL     DOPZ(6)       V1CATB
170800981015E   1C                   ENDIF
170900010112     C*
171000010112     C* IN CASO DI DUPLICA AZZERO COMUNQUE ALCUNI CAMPI
171100010112     C     D60OP1        IFEQ      'O03'
171200010112     C                   MOVEL     *BLANKS       V1CFT1
171300010112     C                   MOVEL     *BLANKS       V1CFT2
171400010112     C                   MOVEL     *BLANKS       V1CCLI
171500010112     C                   MOVEL     *BLANKS       V1CCTR
171600010112     C                   MOVEL     *BLANKS       V1CCTS
171700131121     C                   movel     *blank        V1CCAd
171800131121     C                   movel     *blank        V1Cnaz
171900010112     C                   MOVEL     *BLANKS       V1CRAG
172000131121     C                   MOVEL     *BLANKS       V1DCAD
172100131121     C                   MOVEL     *BLANKS       V1DNAZ
172200010112     C                   MOVEL     *BLANKS       V1CFD1
172300010112     C                   ENDIF
172400000000     C*
172500000000     C                   ENDSR
172600000000     C*------------------------------------------------------------------------*
172700000000     C*  AZZVID - PULISCE TUTTI ICAMPI DEL VIDEO
172800000000     C*------------------------------------------------------------------------*
172900000000     C     AZZVID        BEGSR
173000000000     C*
173100000000     C                   Z-ADD     D60VOC        V1CVOC
173200000000     C                   Z-ADD     D60PRG        V1CPRG
173300000000     C                   MOVE      D60TTV        V1CTTV
173400000000     C                   MOVE      D60FT1        V1CFT1
173500000000     C                   MOVE      D60FT2        V1CFT2
173600000000     C                   MOVE      D60CLI        V1CCLI
173700000000     C                   MOVE      D60CTR        V1CCTR
173800000000     C                   MOVE      D60CTS        V1CCTS
173900131121     C                   movel     D60CAd        V1CCAd
174000131121     C                   movel     D60naz        V1Cnaz
174100000000     C                   Z-ADD     D60DDT        V1CDDT
174200950202     C*                    Z-ADD*ZEROS    V1CDST           *SCADENZA (G/M/A)
174300060918     C                   Z-ADD     31122039      V1CDST
174400000000     C                   Z-ADD     *ZERO         V1CSGI
174500000000     C                   Z-ADD     *ZERO         V1CSGF
174600000000     C                   Z-ADD     *ZERO         V1CITR
174700000000     C                   Z-ADD     *ZERO         V1CIMI
174800000000     C                   Z-ADD     *ZERO         V1CIMA
174900000000     C                   MOVE      '+'           V1CSEG
175000000000     C                   Z-ADD     *ZERO         V1CPER
175100000000     C                   Z-ADD     *ZERO         V1CRPV
175200981015     C                   MOVEL     *BLANKS       V1CATB
175300000000     C*
175400000000     C                   ENDSR
175500970627     C*------------------------------------------------------------------------*
175600970627     C* FINSR - OPERAZIONI FINALI
175700970627     C*------------------------------------------------------------------------*
175800970627     C     FINSR         BEGSR
175900970627     C*
176000971020     C* TISI95R
176100970704     C                   CLEAR                   DSSI95
176200970627     C                   MOVEL     'C'           I95TLA
176300970627     C                   CALL      'TISI95R'
176400970627     C                   PARM                    DSSI95
176500971020     C* FNLV55R
176600971020     C                   CLEAR                   DSLV55
176700971020     C                   MOVEL     'C'           D55TLA
176800971020     C                   CALL      'FNLV55R'
176900971020     C                   PARM                    DSLV55
177000000727     C* TIBS69R
177100000727     C                   CLEAR                   BS69DS
177200000727     C                   CLEAR                   ACODS
177300000727     C                   CLEAR                   INDDS
177400000727     C                   CLEAR                   CLPDS
177500000727     C                   CLEAR                   CLSDS
177600000727     C                   MOVEL     'C'           I69TLA
177700000727     C                   MOVEL     KNSIF         I69SIF
177800000727     C                   CALL      'TIBS69R'
177900000727     C                   PARM                    BS69DS
178000000727     C                   PARM                    ACODS
178100000727     C                   PARM                    INDDS
178200000727     C                   PARM                    CLPDS
178300000727     C                   PARM                    CLSDS
178400970627     C*
178500970627     C                   ENDSR
178600020923     C*-------------------------------------------------------------------------
178700020923     C* REPERISCE I DATI UTENTE
178800020923     C*-------------------------------------------------------------------------
178900020923     C     REPDATIUTE    BEGSR
179000020923     C*
179100020923     C* INIZIALIZZA VARIABILI DI WRK
179200020923     C                   CLEAR                   TIBS34DS
179300020923     C                   CLEAR                   AZUTEDS
179400020923     C                   CLEAR                   DDATIUTE
179500020923     C*
179600020923     C     *DTAARA       DEFINE    §azute        azuteds
179700020923     C     *DTAARA       DEFINE    §datiute      ddatiute
179800020923     C                   IN(E)     *DTAARA
179900020923if  1C                   IF        %Error
180000020923     c                   EVAL      I34Tla = 'L'
180100020923     C                   CALL      'TIBS34R'
180200020923     C                   PARM                    Tibs34Ds
180300020923     C                   IN        *DTAARA
180400020923e   1C                   ENDIF
180500020923     C*
180600020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
180700020923     C                   MOVEL(P)  RSUT          DSFIRS           20
180800020923     C*
180900020923     C                   ENDSR
181000000000     C*------------------------------------------------------------------------*
181100000000     C*  *INZSR - ROUTINE INIZIALE
181200000000     C*------------------------------------------------------------------------*
181300000000     C     *INZSR        BEGSR
181400000000     C*
181500000000     C* RICEVIMENTO PARAMETRI
181600000000     C     *ENTRY        PLIST
181700000000     C                   PARM                    KPJBA
181800000000     C                   MOVEL     KPJBU         DS60
181900130920      * ovrdbf ecevdx1l
182000130920     c                   exsr      comando
182100131121     c                   open      ecevdx6l
182200000000     C*
182300000000     C* ACCENDO UN INDICATORE A SECONDA DEL TIPO TARIFFA
182400000000     C     D60TTV        COMP      'K'                                    01
182500000000     C     D60TTV        COMP      'F'                                    02
182600000000     C     D60TTV        COMP      'C'                                    03
182700000000     C     D60TTV        COMP      'P'                                    04
182800000000     C     D60TTV        COMP      'T'                                    05
182900000000     C*
183000000000     C* DEFINIZIONE VARIABILI
183100000000     C     *LIKE         DEFINE    ORGFIL        KRGFIL
183200970627     C     *LIKE         DEFINE    TAMKSC        KTAKSC
183300000000     C     *LIKE         DEFINE    TAMCTR        KTACTR
183400000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
183500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
183600000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
183700000000     C     *LIKE         DEFINE    EVDVOC        KVDVOC
183800000000     C     *LIKE         DEFINE    EVDPRG        KVDPRG
183900000000     C     *LIKE         DEFINE    EVDTTV        KVDTTV
184000000000     C     *LIKE         DEFINE    EVDFT1        KVDFT1
184100000000     C     *LIKE         DEFINE    EVDFT2        KVDFT2
184200000000     C     *LIKE         DEFINE    EVDCLI        KVDCLI
184300000000     C     *LIKE         DEFINE    EVDCTR        KVDCTR
184400000000     C     *LIKE         DEFINE    EVDCTS        KVDCTS
184500131121     C     *LIKE         DEFINE    EVDCAd        KVDCAD
184600131121     C     *LIKE         DEFINE    EVDnaz        KVDnaz
184700000000     C     *LIKE         DEFINE    EVDDDT        KVDDDT
184800000000     C     *LIKE         DEFINE    EVDSGI        KVDSGI
184900000000     C     *LIKE         DEFINE    EVTVOC        KVTVOC
185000000000     C     *LIKE         DEFINE    EVTPRG        KVTPRG
185100941130     C     *LIKE         DEFINE    CASPTA        KISPTA
185200941130     C     *LIKE         DEFINE    CASLNP        KISLNP
185300941130     C     *LIKE         DEFINE    CASLNA        KISLNA
185400000000     C*
185500000000     C     *LIKE         DEFINE    D60DDT        DDTKTV
185600000000     C     *LIKE         DEFINE    D60TTV        TTVKTV
185700000000     C     *LIKE         DEFINE    D60FT1        FT1KTV
185800000000     C     *LIKE         DEFINE    D60FT2        FT2KTV
185900000000     C     *LIKE         DEFINE    D60CLI        CLIKTV
186000000000     C     *LIKE         DEFINE    D60CTR        CTRKTV
186100000000     C     *LIKE         DEFINE    D60CTS        CTSKTV
186200131121     C     *LIKE         DEFINE    D60CAd        CAdKTV
186300131121     C     *LIKE         DEFINE    D60naz        nazKTV
186400000000     C     *LIKE         DEFINE    D60SGI        SGIKTV
186500000000     C*
186600000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
186700000000     C     KEYORG        KLIST
186800000000     C                   KFLD                    KRGFIL
186900000000     C*
187000000000     C* CHIAVE LETTURA ECEVD01L - COMPLETA
187100000000     C     KEYEVD        KLIST
187200000000     C                   KFLD                    KVDVOC
187300000000     C                   KFLD                    KVDPRG
187400000000     C                   KFLD                    KVDTTV
187500000000     C                   KFLD                    KVDFT1
187600000000     C                   KFLD                    KVDFT2
187700000000     C                   KFLD                    KVDCLI
187800000000     C                   KFLD                    KVDCTR
187900000000     C                   KFLD                    KVDCTS
188000131121     C                   KFLD                    KVDnaz
188100131121     C                   KFLD                    KVDCAd
188200000000     C                   KFLD                    KVDDDT
188300000000     C                   KFLD                    KVDSGI
188400130920     C     Kscagl        KLIST
188500130920     C                   KFLD                    v1cVOC
188600130920     C                   KFLD                    v1cPRG
188700130920     C                   KFLD                    v1cTTV
188800130920     C                   KFLD                    w1cFT1            3 0
188900130920     C                   KFLD                    w1cFT2            3 0
189000130920     C                   KFLD                    w1ccli            7 0
189100130920     C                   KFLD                    w1cCTR            3 0
189200130920     C                   KFLD                    v1cCTS
189300131121     C                   KFLD                    v1cnaz
189400131121     C                   KFLD                    v1cCAd
189500130920     C                   KFLD                    w1cDDT            8 0
189600130920      *
189700130920     C     Kscagl1       KLIST
189800130920     C                   KFLD                    v1cVOC
189900130920     C                   KFLD                    v1cPRG
190000130920     C                   KFLD                    v1cTTV
190100130920     C                   KFLD                    w1cFT1
190200130920     C                   KFLD                    w1cFT2
190300130920     C                   KFLD                    w1ccli
190400130920     C                   KFLD                    w1cCTR
190500130920     C                   KFLD                    v1cCTS
190600131121     C                   KFLD                    v1cnaz
190700131121     C                   KFLD                    v1cCAd
190800130920     C                   KFLD                    w1cDDT
190900130920     C                   KFLD                    v1cSGI
191000000000     C* CHIAVE LETTURA ECEVD01L - PARZIALE
191100000000     C     KE2EVD        KLIST
191200000000     C                   KFLD                    KVDVOC
191300000000     C                   KFLD                    KVDPRG
191400000000     C                   KFLD                    KVDTTV
191500000000     C                   KFLD                    KVDFT1
191600000000     C                   KFLD                    KVDFT2
191700000000     C                   KFLD                    KVDCLI
191800000000     C                   KFLD                    KVDCTR
191900000000     C                   KFLD                    KVDCTS
192000131121     C                   KFLD                    KVDnaz
192100131121     C                   KFLD                    KVDCAd
192200000000     C*
192300131121     C* CHIAVE LETTURA ECEVD06L - COMPLETA
192400000000     C     KE3EVD        KLIST
192500000000     C                   KFLD                    KVDVOC
192600000000     C                   KFLD                    KVDPRG
192700000000     C                   KFLD                    KVDTTV
192800000000     C*
192900000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
193000000000     C     KEYTAB        KLIST
193100000000     C                   KFLD                    KTBKUT
193200000000     C                   KFLD                    KTBCOD
193300000000     C                   KFLD                    KTBKEY
193400000000     C*
193500941128     C* CHIAVE LETTURA TNTAM01L - PARZIALE
193600000000     C     KEYTAM        KLIST
193700000000     C                   KFLD                    KTAKSC
193800000000     C                   KFLD                    KTACTR
193900000000     C*
194000000000     C* CHIAVE LETTURA ECEVT01L - COMPLETA
194100000000     C     KEYEVT        KLIST
194200000000     C                   KFLD                    KVTVOC
194300940728     C                   KFLD                    KVTPRG
194400940728     C*
194500941130     C* CHIAVE LETTURA AZCAS01L - PARZIALE
194600941130     C     KEYCAS        KLIST
194700941130     C                   KFLD                    KISPTA
194800940728     C                   KFLD                    KISLNP
194900940728     C                   KFLD                    KISLNA
195000131122     C*
195100131122     C* CHIAVE LETTURA azcpc01L - COMPLETA
195200131122     C     Kazcpc        KLIST
195300131122     C                   KFLD                    wver
195400131122     C                   KFLD                    wnar
195500131122     C                   KFLD                    wcap
195600971020     C*
195700971020     C* REPERISCE LA DATA CORRENTE
195800971020     C                   TIME                    N14              14 0
195900971020     C                   MOVE      N14           N8                8 0
196000971020     C                   Z-ADD     N8            G02DAT
196100971020     C                   Z-ADD     *ZEROS        G02INV
196200971020     C                   MOVEL     '0'           G02ERR
196300971020     C                   CALL      'XSRDA8'
196400971020     C                   PARM                    WLBDAT
196500971020     C                   Z-ADD     G02INV        DATCOR            8 0
196600000000     C*
196700000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
196800000000     C                   MOVE      D60OP0        INTGES            2
196900000000IF  1C     INTGES        IFEQ      '02'
197000000000     C                   MOVEL     'G'           $TGES             1
197100000000X   1C                   ELSE
197200000000     C                   MOVEL     'I'           $TGES             1
197300000000E   1C                   ENDIF
197400131122      *
197500131122     c* reperisco la versione del cappario
197600131122     C                   CLEAR                   wver
197700131122     C                   CLEAR                   DSSI95
197800131122     C                   MOVEL     '3'           I95TCN
197900131122     C                   MOVEL(p)  '40100'       I95CAP
198000131122     C                   Z-ADD     WDAT          I95DAT
198100131122     C                   CALL      'TISI95R'
198200131122     C                   PARM                    DSSI95
198300131122IF  1C     O95ERR        IFeq      *BLANKS
198400131122     C                   MOVEL     o95ver        wver
198500131122E   1C                   ENDIF
198600131122     C*
198700000000     C*
198800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
198900000000     C                   EXSR      INZSTD
199000000000     C*
199100000000     C* RAGIONE SOCIALE UTENTE
199200000000     C                   MOVEL     DSFIRS        VTDRSU
199300000000     C*
199400000000     C* DECODIFICA OPZIONE RICHIAMATA
199500000000IF  1C     D60OP1        IFEQ      'F06'
199600000000     C                   MOVEL     DOPZ(1)       VTDFUN
199700000000E   1C                   ENDIF
199800000000IF  2C     D60OP1        IFEQ      'O02'
199900000000     C                   MOVEL     DOPZ(2)       VTDFUN
200000000000E   1C                   ENDIF
200100010112IF  2C     D60OP1        IFEQ      'O03'
200200010112     C                   MOVEL     DOPZ(7)       VTDFUN
200300010112E   1C                   ENDIF
200400000000IF  1C     D60OP1        IFEQ      'O04'
200500000000     C                   MOVEL     DOPZ(3)       VTDFUN
200600000000E   1C                   ENDIF
200700000000IF  1C     D60OP1        IFEQ      'O07'
200800000000     C                   MOVEL     DOPZ(4)       VTDFUN
200900000000E   1C                   ENDIF
201000000000IF  1C     D60OP1        IFEQ      'O05'
201100000000     C                   MOVEL     DOPZ(5)       VTDFUN
201200000000E   1C                   ENDIF
201300000000     C*
201400010112     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
201500000000     C* 0=NON PROTETTO, 1=PROTETTO
201600000000     C*   90 - CAMPI CHIAVE
201700000000     C*   91 - ALTRI CAMPI
201800000000IF  1C     D60OP1        IFEQ      'F06'
201900020702     C                   SETOFF                                       909196
202000000000E   1C                   ENDIF
202100000000IF  1C     D60OP1        IFEQ      'O02'
202200010112     C                   SETON                                        9096
202300000000E   1C                   ENDIF
202400010112IF  1C     D60OP1        IFEQ      'O03'
202500010112     C                   SETON                                        90
202600010112E   1C                   ENDIF
202700000000IF  1C     D60OP1        IFEQ      'O04'
202800000000     C     D60OP1        OREQ      'O07'
202900000000     C     D60OP1        OREQ      'O05'
203000020702     C                   SETON                                        909196
203100000000E   1C                   ENDIF
203200000000     C*
203300000000     C* SE VISUALIZZAZIONE TOLGO FUNZIONE F06=AGGIORNAMENTO
203400000000     C* E METTO F4=DECODIFICA
203500000000IF  1C     D60OP1        IFEQ      'O05'
203600000000     C                   MOVEL     FUN(6)        FUNRIG
203700000000     C                   MOVEL     'N'           FUNASG
203800000000     C                   MOVEL     'N'           FUNASI
203900000000     C                   MOVEL     'N'           FUNAFG
204000000000     C                   MOVEL     'N'           FUNAFG
204100000000     C                   MOVEL     FUNRIG        FUN(6)
204200000000E   1C                   ENDIF
204300000000     C*
204400010112     C* SE SONO IN MOD/CANC/VISUAL/RIPR/DUPL LEGGO IL RECORD
204500000000IF  1C     D60OP1        IFEQ      'O02'
204600010112     C     D60OP1        OREQ      'O03'
204700000000     C     D60OP1        OREQ      'O04'
204800000000     C     D60OP1        OREQ      'O05'
204900000000     C     D60OP1        OREQ      'O07'
205000000000     C                   Z-ADD     D60VOC        KVDVOC
205100000000     C                   Z-ADD     D60PRG        KVDPRG
205200000000     C                   Z-ADD     D60DDT        G02DAT
205300000000     C                   Z-ADD     *ZEROS        G02INV
205400000000     C                   MOVEL     '0'           G02ERR
205500000000     C                   CALL      'XSRDA8'
205600000000     C                   PARM                    WLBDAT
205700000000     C                   Z-ADD     G02INV        KVDDDT
205800000000     C                   MOVEL     D60TTV        KVDTTV
205900000000     C                   Z-ADD     D60FT1        KVDFT1
206000000000     C                   Z-ADD     D60FT2        KVDFT2
206100000000     C                   Z-ADD     D60CLI        KVDCLI
206200000000     C                   Z-ADD     D60CTR        KVDCTR
206300000000     C                   MOVEL     D60CTS        KVDCTS
206400131121     C                   movel     D60CAd        KVDCAd
206500131121     C                   movel     D60naz        KVDnaz
206600000000     C                   Z-ADD     D60SGI        KVDSGI
206700000000     C                   Z-ADD     KVDDDT        DDTKTV
206800000000     C                   MOVEL     D60TTV        TTVKTV
206900000000     C                   Z-ADD     D60FT1        FT1KTV
207000000000     C                   Z-ADD     D60FT2        FT2KTV
207100000000     C                   Z-ADD     D60CLI        CLIKTV
207200000000     C                   Z-ADD     D60CTR        CTRKTV
207300000000     C                   MOVEL     D60CTS        CTSKTV
207400131121     C                   MOVEL     D60CAd        CAdKTV
207500131121     C                   MOVEL     D60naz        nazKTV
207600000000     C                   Z-ADD     D60SGI        SGIKTV
207700000000     C*
207800000000     C* SE IL RECORD NON ESISTE O E' ALLOCATO MANDO ERRORE AL PGM CHIAMANTE
207900131121     C     KEYEVD        CHAIN     ECEVD06L                           9998
208000000000IF  2C     *IN99         IFEQ      '1'
208100000000     C                   MOVEL     '1'           $ERR
208200000000     C                   MOVEL     '1'           $FINE
208300000000     C                   MOVEL     MSG(21)       D60MSG
208400000000X   2C                   ELSE
208500000000IF  3C     *IN98         IFEQ      '1'
208600000000     C                   MOVEL     '1'           $ERR
208700000000     C                   MOVEL     '1'           $FINE
208800000000     C                   MOVEL     MSG(22)       D60MSG
208900000000X   3C                   ELSE
209000000000     C*
209100000000     C                   EXSR      RECVID
209200000000     C                   EXSR      CARV1
209300000000E   3C                   ENDIF
209400000000E   2C                   ENDIF
209500000000E   1C                   ENDIF
209600000000     C*
209700000000     C* SE INSERIMENTO
209800000000IF  1C     D60OP1        IFEQ      'F06'
209900000000     C                   EXSR      AZZVID
210000000000     C                   EXSR      CARV1
210100000000E   1C                   ENDIF
210200940726     C*
210300940726     C* DECODIFICO SOLO SE C'E' SIA VOCE CHE PROGRESSIVO
210400940726     C     V1CVOC        IFGT      *ZERO
210500940726     C     V1CPRG        ANDGT     *ZERO
210600940726     C                   EXSR      DECODI
210700940726     C                   ENDIF
210800000000     C*
210900000000     C                   ENDSR
211000000000     C*------------------------------------------------------------------------*
211100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
211200000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
211300000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
211400000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
211500000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
211600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
211700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
211800000000     C*------------------------------------------------------------------------*
211900000000     C* INZSTD - ROUTINE INIZIALE STANDARD
212000000000     C*------------------------------------------------------------------------*
212100000000     C     INZSTD        BEGSR
212200000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
212300000000     C                   CALL      'XKEYS'
212400000000     C                   PARM                    DSKEYS
212500000000     C                   MOVEA     KEYRIG        KEYS
212600000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
212700000000     C     XMSGPA        PLIST
212800000000     C                   PARM                    DSMSMS
212900000000     C                   PARM                    DSMSPG
213000000000     C                   PARM                    DSMSKE
213100000000     C                   PARM                    DSMSER
213200000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
213300000000     C                   MOVEL     'TREC64R'     DSMSPG
213400000000     C                   MOVEL     'TREC64R'     VMCQUE
213500000000     C                   MOVEL     *BLANKS       VMCKEY
213600000000     C                   MOVEL     *BLANKS       DSMSMS
213700000000     C* INIZIALIZZO FLAG CONTROLLO PGM
213800000000     C                   MOVEL     '0'           $ERR              1
213900000000     C                   MOVEL     '0'           $F03              1
214000000000     C                   MOVEL     *BLANKS       $GES              2
214100000000     C                   MOVEL     *BLANKS       $MSG             80
214200000000     C                   MOVEL     '0'           $FINE             1
214300000000     C                   MOVEL     '0'           $TESVD            1
214400000000     C* LANCIO ROUTINE PER VERIFICARE SE
214500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
214600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
214700020923     C                   EXSR      REPDATIUTE
214800000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
214900000000     C                   BITOFF    '01234567'    POSMI1            1
215000000000     C                   BITOFF    '01234567'    POSMI2            1
215100000000     C                   BITOFF    '01234567'    POSMA1            1
215200000000     C                   BITOFF    '01234567'    POSMA2            1
215300000000     C                   BITON     '34'          POSMI1            1
215400000000     C                   BITON     '7'           POSMI2            1
215500000000     C                   BITON     '34'          POSMA1            1
215600000000     C                   BITON     '0'           POSMA2            1
215700000000     C                   MOVE      POSMI2        POSMIN            2
215800000000     C                   MOVEL     POSMI1        POSMIN
215900000000     C                   MOVE      POSMA2        POSMAX            2
216000000000     C                   MOVEL     POSMA1        POSMAX
216100000000     C                   ENDSR
216200000000     C*------------------------------------------------------------------------*
216300000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
216400000000     C*------------------------------------------------------------------------*
216500000000     C     FUNSTD        BEGSR
216600000000     C                   SELECT
216700000000     C     KEY           WHENEQ    F3
216800000000     C                   EXSR      F03STD
216900000000     C     KEY           WHENEQ    F12
217000000000     C                   EXSR      F12STD
217100000000     C     KEY           WHENEQ    F24
217200000000     C                   EXSR      F24STD
217300000000     C                   OTHER
217400000000     C                   MOVEL     MSG(2)        $MSG
217500000000     C                   EXSR      CARMSG
217600000000     C                   ENDSL
217700000000     C                   ENDSR
217800000000     C*------------------------------------------------------------------------*
217900000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
218000000000     C*------------------------------------------------------------------------*
218100000000     C     F03STD        BEGSR
218200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
218300000000     C                   MOVEL     '1'           $F03
218400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
218500000000     C                   MOVEL     '1'           $FINE
218600000000     C                   ENDSR
218700000000     C*------------------------------------------------------------------------*
218800000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
218900000000     C*------------------------------------------------------------------------*
219000000000     C     F12STD        BEGSR
219100000000     C* IMPOSTO FLAG DI FIN PGM
219200000000     C                   MOVEL     '1'           $FINE
219300000000     C                   ENDSR
219400000000     C*------------------------------------------------------------------------*
219500000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
219600000000     C*------------------------------------------------------------------------*
219700000000     C     F24STD        BEGSR
219800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
219900000000     C                   EXSR      OUTFUN
220000000000     C                   ENDSR
220100000000     C*------------------------------------------------------------------------*
220200000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
220300000000     C*          CODA MESSAGGI DEL PGM
220400000000     C*------------------------------------------------------------------------*
220500000000     C     CARMSG        BEGSR
220600000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
220700000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
220800000000     C     MSGULT        IFEQ      0
220900000000     C                   MOVEL     '1'           *IN40
221000000000     C                   WRITE     EC64DVK
221100000000     C                   MOVEL     '0'           *IN40
221200000000     C                   ENDIF
221300000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
221400000000     C                   MOVEL     $MSG          DSMSMS
221500000000     C                   CALL      'XMSG'        XMSGPA
221600000000     C     DSMSER        IFEQ      0
221700000000     C                   MOVEL     DSMSKE        VMCKEY
221800000000     C                   ENDIF
221900000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
222000000000     C                   ADD       1             MSGULT            4 0
222100000000     C                   WRITE     EC64DVM
222200000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
222300000000     C                   MOVEL     *BLANKS       $MSG
222400000000     C                   ENDSR
222500000000     C*------------------------------------------------------------------------*
222600000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
222700000000     C*------------------------------------------------------------------------*
222800000000     C     OUTMSG        BEGSR
222900000000     C* VISUALIZZAZIONE MESSAGGIO
223000000000     C                   MOVEL     '1'           *IN44
223100000000     C                   WRITE     EC64DVK
223200000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
223300000000     C                   Z-ADD     *ZEROS        MSGULT
223400000000     C                   ENDSR
223500000000     C*------------------------------------------------------------------------*
223600000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
223700000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
223800000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
223900000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
224000000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
224100000000     C*------------------------------------------------------------------------*
224200000000     C     CARFUN        BEGSR
224300000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
224400000000     C     1             DO        31            I                 5 0
224500000000     C                   MOVEL     FUN(I)        FUNRIG
224600000000     C                   MOVEL     FUNFUN        FFU(I)
224700000000     C                   MOVEL     FUNDES        FDE(I)
224800020923     C     DUTLPO        IFEQ      'S'
224900000000     C     $TGES         IFEQ      'G'
225000000000     C                   MOVEL     FUNASG        FAT(I)
225100000000     C                   ELSE
225200000000     C                   MOVEL     FUNASI        FAT(I)
225300000000     C                   ENDIF
225400000000     C                   ELSE
225500000000     C     $TGES         IFEQ      'G'
225600000000     C                   MOVEL     FUNAFG        FAT(I)
225700000000     C                   ELSE
225800000000     C                   MOVEL     FUNAFI        FAT(I)
225900000000     C                   ENDIF
226000000000     C                   ENDIF
226100000000     C     FAT(I)        IFEQ      'E'
226200000000     C                   MOVEL     USER          WEDP              3
226300000000     C     WEDP          IFEQ      'EDP'
226400000000     C                   MOVEL     'S'           FAT(I)
226500000000     C                   ELSE
226600000000     C                   MOVEL     'N'           FAT(I)
226700000000     C                   ENDIF
226800000000     C                   ENDIF
226900000000     C                   ENDDO
227000000000     C                   ENDSR
227100000000     C*------------------------------------------------------------------------*
227200000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
227300000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
227400000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
227500000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
227600000000     C*------------------------------------------------------------------------*
227700000000     C     OUTFUN        BEGSR
227800000000     C     1             DO        8             I
227900000000     C                   MOVEL     *BLANKS       FUV(I)
228000000000     C                   ENDDO
228100000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
228200000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
228300000000     C     1             DO        31            I
228400000000     C     FAT(I)        IFEQ      'S'
228500000000     C     FDE(I)        ANDNE     *BLANKS
228600000000     C                   ADD       1             FUNNAT
228700000000     C                   ENDIF
228800000000     C                   ENDDO
228900000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
229000000000     C     FUNNAT        IFGT      8
229100000000     C                   MOVEL     'S'           FAT(24)
229200000000     C                   ENDIF
229300000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
229400000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
229500000000     C                   Z-ADD     8             FUNVIS            5 0
229600000000     C     FAT(23)       IFEQ      'S'
229700000000     C                   SUB       1             FUNVIS
229800000000     C                   ENDIF
229900000000     C     FAT(24)       IFEQ      'S'
230000000000     C                   SUB       1             FUNVIS
230100000000     C                   ENDIF
230200000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
230300000000     C                   Z-ADD     *ZEROS        W                 5 0
230400000000     C                   ADD       1             FUNULT
230500000000     C     FUNULT        IFGT      31
230600000000     C                   Z-ADD     1             FUNULT
230700000000     C                   ENDIF
230800000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
230900000000     C     FUNULT        DO        31            I
231000000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
231100000000     C     W             IFLT      FUNVIS
231200000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
231300000000     C     FAT(I)        IFEQ      'S'
231400000000     C     FDE(I)        ANDNE     *BLANKS
231500000000     C     I             IFEQ      23
231600000000     C     I             OREQ      24
231700000000     C                   ELSE
231800000000     C                   ADD       1             W
231900000000     C                   MOVEL     FDE(I)        FUV(W)
232000000000     C                   ENDIF
232100000000     C                   ENDIF
232200000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
232300000000     C                   Z-ADD     I             FUNULT
232400000000     C                   ENDIF
232500000000     C                   ENDDO
232600000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
232700000000     C                   MOVEL     FUV(1)        VFD001
232800000000     C                   MOVEL     FUV(2)        VFD002
232900000000     C                   MOVEL     FUV(3)        VFD003
233000000000     C                   MOVEL     FUV(4)        VFD004
233100000000     C                   MOVEL     FUV(5)        VFD005
233200000000     C                   MOVEL     FUV(6)        VFD006
233300000000     C     FAT(23)       IFEQ      'N'
233400000000     C     FAT(24)       ANDEQ     'N'
233500000000     C                   MOVEL     FUV(7)        VFD007
233600000000     C                   MOVEL     FUV(8)        VFD008
233700000000     C                   ELSE
233800000000     C     FAT(23)       IFEQ      'S'
233900000000     C     FAT(24)       ANDEQ     'S'
234000000000     C                   MOVEL     FDE(23)       VFD007
234100000000     C                   MOVEL     FDE(24)       VFD008
234200000000     C                   ELSE
234300000000     C                   MOVEL     FUV(7)        VFD007
234400000000     C     FAT(23)       IFEQ      'S'
234500000000     C                   MOVEL     FDE(23)       VFD008
234600000000     C                   ELSE
234700000000     C                   MOVEL     FDE(24)       VFD008
234800000000     C                   ENDIF
234900000000     C                   ENDIF
235000000000     C                   ENDIF
235100000000     C                   MOVEL     '1'           $TESVD
235200000000     C                   ENDSR
235300000000     C*------------------------------------------------------------------------*
235400000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
235500000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
235600000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
235700000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
235800000000     C*------------------------------------------------------------------------*
235900000000     C     CONFUN        BEGSR
236000000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
236100000000     C     1             DO        31            I
236200000000     C     KEY           IFEQ      KEYS(I)
236300000000     C* CONTROLLO SE IL TASTO E' ATTIVO
236400000000     C     FAT(I)        IFNE      'S'
236500000000     C                   MOVEL     MSG(1)        $MSG
236600000000     C                   EXSR      CARMSG
236700000000     C                   MOVEL     '1'           $ERR
236800000000     C                   ENDIF
236900000000     C                   ENDIF
237000000000     C                   ENDDO
237100000000     C                   ENDSR
237200130920      *---------------------------------------------------------------------
237300130920     C     comando       BEGSR
237400130920      *---------------------------------------------------------------------
237500130920     c                   if        %subst(knsif:7:1) = 'P'
237600130920     C                   movea     cmd1          cmd
237700130920     c                   else
237800130920     C                   movea     cmd2          cmd
237900130920     c                   endif
238000130920     C                   movel(p)  cmd           comman
238100130920     C                   call      'QCMDEXC'
238200130920     C                   parm                    comman           80
238300130920     C                   parm      48            lung             15 5
238400130920     C*
238500130920     C                   ENDSR
238600981015** ======== SCHIERA: MSG  (MESSAGGI)                       ================
238700000000TREC64R- Tasto funzionale non valido.                                          1
238800000000TREC64R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
238900000000TREC64R- Opzione non valida.                                                   3
239000000000TREC64R- Opzione abilitata ma non gestita. Avvertire CED.                      4
239100000000TREC64R- Inizio elenco.                                                        5
239200000000TREC64R- Fine elenco.                                                          6
239300000000TREC64R- Immettere codice voce esistente                                       7
239400000000TREC64R- Immettere data formalmente valida                                     8
239500100330TREC64R- La tariffa per voce isolamento maggioraz. deve essere CARTELLO 100%   9
239600970627TREC64R- Immettere codice C.A.P. esistente e non annullato                    10
239700951207TREC64R- Tariffa già esistente                                                11
239800940722TREC64R- Immettere codice tariffa numerico                                    12
239900940726TREC64R- Immettere scaglione tariffa                                          13
240000000000TREC64R- Immettere codice filiale esistente                                   14
240100000000TREC64R- Non immettere il rapporto peso volume per il tipo riferimento voce   15
240200000000TREC64R- Immettere codice tassazione esistente e non annullato                16
240300000000TREC64R- Inserimento effettuato.                                              17
240400000000TREC64R- Modifica effettuata.                                                 18
240500000000TREC64R- Cancellazione effettuata.                                            19
240600000000TREC64R- Ripristino effettuato.                                               20
240700000000TREC64R- Richiesta funzione per voce già esistente                            21
240800000000TREC64R- Codice tariffa momentaneamente allocato: riprovare più tardi         22
240900000000TREC64R- La data scadenza deve essere maggiore o uguale della decorrenza      23
241000000000TREC64R- Per il Tipo Riferimento Voce non e' possibile aggiungere piu' voci   24
241100940726TREC64R- Immettere importo unitario solo se tipo tariffa a Quantità/Imp fisso 25
241200961202TREC64R- Rapporto peso volume obbligatorio                                    26
241300000000TREC64R- La validità della tariffa si sovrappone ad una già in decorrenza     27
241400000000TREC64R- Immettere filiali non uguali                                         28
241500010112TREC64R- Duplicazione effettuata                                              29
241600000000TREC64R- Immettere codice cliente esistente e non annullato                   30
241700100407TREC64R- La tariffa x voce isolamento maggior. deve avere scadenza 31/12/2039 31
241800000000TREC64R- Non immettere importo se non presente la percentuale                 32
241900000000TREC64R- Immettere il segno per la tariffa                                    33
242000000000TREC64R- Se una filiale e' 1° livello l'altra deve appartenere all'area       34
242100000000TREC64R- Immettere codice C.A.P. esistente e non annullato                    35
242200000000TREC64R- Immettere importo minimo inferiore al massimo                        36
242300940728TREC64R- Per il riferimento tariffa, la filiale deve essere terminal          37
242400000000TREC64R- Per il riferimento tariffa, non si puo' immettere la filiale         38
242500000000TREC64R- Il periodo immesso non e' compreso in quello della voce              39
242600000000TREC64R- Nella voce non vi e' alcun riferimento per il tipo tariffa           40
242700000000TREC64R- Immettere una percentuale valida                                     41
242800100422TREC64R- La voce di isolamento ha il flag Applica incongruente con il segno   42
242900130806TREC64R- Codice di tassazione non utilizzabile                                43
243000141203TREC64R- Se maggioraz. con tipo tariffa obbligo % a 0 o 100 positiva          44
243100950719*** INESISTENTE ***                                                           45
243200130919TREC64R- Scaglione incongruente con scaglioni esistenti                       46
243300131122TREC64R- Imserire un CAP generico correttamente                               47
243400131122TREC64R- Codice Nazione Inesistente                                           48
243500950719** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
243600000000F1    -                   -    NN     NN
243700000000F2    -                   -    NN     NN
243800000000F3    -F3=Fine            -    SS     SS
243900000000F4    -                   -    NN     NN
244000000000F5    -                   -    NN     NN
244100000000F6    -F6=Aggiornamento   -    SS     SS
244200000000F7    -                   -    NN     NN
244300000000F8    -                   -    NN     NN
244400000000F9    -                   -    NN     NN
244500000000F10   -                   -    NN     NN
244600000000F11   -                   -    NN     NN
244700000000F12   -F12=Ritorno        -    SS     SS
244800000000F13   -                   -    NN     NN
244900000000F14   -                   -    NN     NN
245000000000F15   -                   -    NN     NN
245100000000F16   -                   -    NN     NN
245200000000F17   -                   -    NN     NN
245300000000F18   -                   -    NN     NN
245400000000F19   -                   -    NN     NN
245500000000F20   -                   -    NN     NN
245600000000F21   -                   -    NN     NN
245700000000F22   -                   -    NN     NN
245800000000F23   -                   -    NN     NN
245900000000F24   -F24=Altri tasti    -    NN     NN
246000000000ENTER -                   -    SS     SS
246100000000ROLLUP-                   -    NN     NN
246200000000ROLLDN-                   -    NN     NN
246300000000HELP  -                   -    NN     NN
246400000000PRINT -                   -    NN     NN
246500000000BSPACE-                   -    NN     NN
246600000000CLEAR -                   -    NN     NN
246700000000** DOPZ  (OPZIONE RICHIAMATA)
246800000000* INSERIMENTO *                                                 1
246900000000*  VARIAZIONE  *                                                2
247000000000*CANCELLAZIONE*                                                 3
247100000000*  RIPRISTINO  *                                                4
247200000000VISUALIZZAZIONE                                                 5
247300981015ANNULLATO                                                       6
247400010112* DUPLICAZIONE *                                                7
2475009810158
2476009810159
24770098101510
247800130920**         CMD1
247900131121OVRDBF FILE(ecevdx6l) TOFILE(*libl/ecevd06l)
248000130920**         CMD2
248100131121OVRDBF FILE(ecevdx6l) TOFILE(*libl/ecevd06l)
