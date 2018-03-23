000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC83R
000300000000      *  Descrizione   :  Stampa voci e tariffe di conto economico.
000400000000      *                   Programma di stampa batch.
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:  16 GIU 1994
000700000000      *------------------------------------------------------------------------*
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000000     F*------------------------------------------------------------------------*
001000000000     F* DATA BASE
001100000000     F*------------------------------------------------------------------------*
001200000000     FAZORG01L  IF   E           K DISK
001300000000     FECECT02L  IF   E           K DISK
001400000209     FECECT01L  IF   E           K DISK    RENAME(ECECT:ECECTxxx)
001500000000     FECEVD01L  IF   E           K DISK
001600000000     FECEVM01L  IF   E           K DISK
001700000210     FECEVT01L  IF   E           K DISK    USROPN
001800000208     FECEVT02L  IF   E           K DISK    RENAME(ECEVT:ECEVTxxx)
001900010808     FECEVT0XL  IF   E           K DISK    RENAME(ECEVT:ECEVTyyy) PREFIX(w_)
002000010808     F                                     USROPN
002100000000     FTABEL00F  IF   E           K DISK
002101040130     Ftntbe01l  IF   E           K DISK
002200941129     FTNTAM01L  IF   E           K DISK
002300000000     FTREC83P   O    E             PRINTER OFLIND(*IN30)
002400000000     F                                     USROPN
002500000209     FTREC82P   O    E             PRINTER OFLIND(*IN31)
002600000209     F                                     USROPN
002700000000     D*------------------------------------------------------------------------*
002800000000     D* SCHIERE
002900000000     D*------------------------------------------------------------------------*
003000010808     D CMD             S             80    DIM(3) CTDATA PERRCD(1)
003100000000     D* TABELLA CAPICONTO
003200000214     D* SCHIERE PER STAMPA DELLE MAGGIORAZIONI
003300000000     D*------------------------------------------------------------------------*
003400000000     D* INPUT
003500000000     D*------------------------------------------------------------------------*
003600000000     D*------------------
003700020923     D* DS REPERIMENTO DATI UTENTE
003800020923     D*-------------------
003900040130     D dco1          E DS
003901040130     D TIBS34DS      E DS
004000020923     D DDATIUTE      E DS
004100020923     D AZUTEDS       E DS                  extname(AZUTE00F)
004200000000     D*-------------------
004300000000     D* ARCHITETTURA
004400000000     D*-------------------
004500000000     D KPJBA         E DS
004600000000     D*-------------------
004700000000     D* PARAMETRI PASSATI DAL FILTRO
004800000000     D*-------------------
004900000208     D PARAM           DS                  INZ
005000000208     D  PA1VOC                 1      3  0
005100000208     D  PA2VOC                 4      6  0
005200000208     D  PA1DDV                 7     14  0
005300000214     D  PA1CMP                15     17  0
005400000214     D  PA2CMP                18     20  0
005500000214     D  PA1SOD                21     21
005600000214     D  PA1COV                22     22
005700000214     D  PA2DDV                23     30  0
005800000214     D  PA1SON                31     31
005900000000     D*------------------
006000000000     D* STAMPA DELLE MAGGIORAZIONI (DS PER RIEMPIRE SCHIERE)
006100000000     D* 3 SCHIERE: FLAG ANNULLAMENTO, CODICE E DESCRIZIONE
006200000000     D*------------------
006300000000     D                 DS
006400000000     D  MTB                    1      4
006500000000     D                                     DIM(4)
006600000000     D  P3CAT1                 1      1
006700000000     D  P3CAT2                 2      2
006800000000     D  P3CAT3                 3      3
006900000000     D  P3CAT4                 4      4
007000000000     D                 DS
007100000000     D  MOC                    1     12  0
007200000000     D                                     DIM(4)
007300000000     D  P3CVO1                 1      3  0
007400000000     D  P3CVO2                 4      6  0
007500000000     D  P3CVO3                 7      9  0
007600000000     D  P3CVO4                10     12  0
007700000000     D                 DS
007800000000     D  MOD                    1    120
007900000000     D                                     DIM(4)
008000000000     D  P3DVO1                 1     30
008100000000     D  P3DVO2                31     60
008200000000     D  P3DVO3                61     90
008300000000     D  P3DVO4                91    120
008400000208     D*
008500000224     D WLBDA8          DS                  INZ
008600000000     D  G08DAT                 1      8  0
008700000000     D  G08INV                 9     16  0
008800000000     D  G08ERR                17     17
008900000000     D  G08TGI                18     22  0
009000000210     D*------------
009100000210     D* SCHIERE X MEMORIZZAZIONE VOCI IN MAGGIORAZIONE
009200000210     D*------------
009300040130     D  VOCMTE         s              3  0 DIM(500) INZ
009400040130     D  PRGMTE         s              3  0 DIM(500) INZ
009500040130     D  DESMTE         s             30    DIM(500) INZ
009600040130     D  TIPMTE         s              1    DIM(500) INZ
009700040130     D  VOCMTA         s              3  0 DIM(500) INZ
009800040130     D  PRGMTA         s              3  0 DIM(500) INZ
009900040130     D  DESMTA         s             30    DIM(500) INZ
010000040130     D  CODCMP         s              3  0 DIM(500) INZ
010100040130     D  DESCMP         s             30    DIM(500) INZ
010101040130     D  Cmpddc         s              6  0 DIM(500) INZ
010102040130     D  Cmpdsc         s              6  0 DIM(500) INZ
010103040130     D  Cmpcdt         s             20    DIM(500) INZ
010200011204     D*------------------
010300011204     D* DS REPERIMENTO DATI UTENTE
010400011204     D*-------------------
010500011204     D BS69DS        E DS                  EXTNAME(TIBS69DS)
010600011204     D ACODS         E DS                  EXTNAME(CNACO00F)
010700011204     D INDDS         E DS                  EXTNAME(CNIND00F)
010800011204     D CLPDS         E DS                  EXTNAME(CNCLP00F)
010900011204     D CLSDS         E DS                  EXTNAME(FNCLS00F)
010901030422     D*---------------------
010902030422     D* DS x campo filler evtxxx
010903030422     D*---------------------
010904030422     D devtxxx       E DS
011000000000     C*------------------------------------------------------------------------*
011100000000     C* MAIN LINE
011200000000     C*------------------------------------------------------------------------*
011300000000     C                   MOVEL     *BLANKS       COMMAN
011400000209     C   50              MOVEA     CMD(1)        COMMAN
011500000209     C  N50              MOVEA     CMD(2)        COMMAN
011600000000     C                   Z-ADD     80            LUNG
011700000000     C                   CALL      'QCMDEXC'                            99
011800000000     C                   PARM                    COMMAN           80
011900000000     C                   PARM                    LUNG             15 5
012000000000     C*
012100000000     C* APRO IL FILE DI STAMPA ED EMETTO L'INTESTAZIONE
012200000209     C   50              OPEN      TREC83P
012300000209     C  N50              OPEN      TREC82P
012400000209     C*
012500000209     C   50              WRITE     EC83T1
012600000209     C  N50              WRITE     EC82T1
012700000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
012800000210     C*
012900000210     C* RIAPRO IL FILE DI INPUT
013000000210     C                   OPEN      ECEVT01L
013100000210     C*
013200000000     C* LEGGO TUTTI I RECORD DAL FILE TESTATE COMPETENZE
013300000208     C                   Z-ADD     PA1CMP        $VTCMP
013400000000     C                   Z-ADD     PA1VOC        $VTVOC
013500000000     C                   Z-ADD     1             $VTPRG
013600000208     C     PA1COV        IFEQ      'C'
013700000208     C     KEYEV2        SETLL     ECEVT02L
013800000210     C                   READ      ECEVT02L                               20
013900000210     C  N20              Z-ADD     EVTCMP        SAVKEY
014000000208     C                   ELSE
014100000208     C     KEYEVT        SETLL     ECEVT01L
014200000208     C                   READ      ECEVT01L                               20
014300000210     C                   ENDIF
014400000000DO  1C     *IN20         DOWEQ     *OFF
014500000000     C*
014600000000     C* RIEMPO DEI CAMPI DI COMODO PER RIPOSIZIONARMI SUCCESSIVAMENTE
014700000208     C                   Z-ADD     EVTCMP        $VTCMP
014800000000     C                   Z-ADD     EVTVOC        $VTVOC
014900000000     C                   Z-ADD     EVTPRG        $VTPRG
015000000214     C*
015100000214     C* ESEGUO ROUTINE DI CONTROLLO RECORD
015200000000     C*
015300000214     C                   EXSR      CHKREC
015400000214     C*
015500000214 IF 2C     *IN55         IFEQ      *OFF
015600000000     C*
015700000209     C* SE LANCIATA X DETTAGLIO ESEGUO LA ROUTINE SOTTOSTANTE
015800000215 IF 3C     *IN50         IFEQ      *ON
015900000211     C                   EXSR      DETTAG
016000000209     C* ALTRIMENTI QUEST' ALTRA (SINTETICA)
016100000211     C                   ELSE
016200000211     C* SE LANCIO E STATO SINTETICO E ORD. X COMPETENZE
016300000211     C* ESEGUO CONSIDERAZIONI SULLE VOCI "IN MAGGIORAZIONE"
016400000211     C* AL CAMBIARE DELLA COMPETENZA
016500000215     C     PA1COV        IFEQ      'C'
016600000215     C     EVTCMP        ANDNE     *ZEROS
016700000215     C     EVTCMP        ANDNE     SAVCMP
016800000215     C                   EXSR      GESMAG
016900000215     C                   Z-ADD     EVTCMP        SAVCMP
017000000215     C                   Z-ADD     *ZEROS        PASSA             1 0
017100000215     C                   ENDIF
017200000215     C                   EXSR      SINTAG
017300000209     C*
017400000000E   3C                   ENDIF
017500000215     C                   ELSE
017600000215 IF 3C     *IN50         IFEQ      *OFF
017700000215     C* SE LANCIO E STATO SINTETICO E ORD. X COMPETENZE
017800000215     C* ESEGUO CONSIDERAZIONI SULLE VOCI "IN MAGGIORAZIONE"
017900000215     C* AL CAMBIARE DELLA COMPETENZA
018000000215 IF 2C     PA1COV        IFEQ      'C'
018100000215     C     EVTCMP        ANDNE     *ZEROS
018200000215     C     EVTCMP        ANDNE     SAVCMP
018300000215     C     PASSA         ANDEQ     *ZEROS
018400000215     C                   EXSR      GESMAG
018500000215     C                   Z-ADD     EVTCMP        SAVCMP
018600000215     C                   Z-ADD     1             PASSA
018700000215     C                   ENDIF
018800000000E   2C                   ENDIF
018900000215E   3C                   ENDIF
019000000000     C*
019100000209     C* DEVO, TUTTE LE VOLTE, RIPOSIZIONARMI POICHE' HO FATTO ALTRE CHAIN SU EVT
019200000208     C     PA1COV        IFEQ      'C'
019300000208     C     KEYEV2        SETGT     ECEVT02L
019400000208     C                   READ      ECEVT02L                               20
019500000208     C                   ELSE
019600000208     C     KEYEVT        SETGT     ECEVT01L
019700000208     C                   READ      ECEVT01L                               20
019800000208     C                   ENDIF
019900000000E   1C                   ENDDO
020000000000     C*
020100000000     C* VERIFICO SE HO STAMPATO QUALCOSA
020200000000     C     $ALMEN        IFGT      *ZERO
020300000209     C   50              WRITE     EC83E1
020400000209     C  N50              WRITE     EC82E1
020500000000     C                   ELSE
020600000209     C   50              WRITE     EC83E2
020700000209     C  N50              WRITE     EC82E2
020800000000     C                   ENDIF
020900000209     C   50              CLOSE     TREC83P
021000000209     C  N50              CLOSE     TREC82P
021100000000     C*
021200000210     C                   CLOSE     ECEVT01L
021300010808     C                   CLOSE     ECEVT0XL
021400000210     C*
021500000000     C* FINE PGM
021600000000     C                   SETON                                        LR
021700000214     C*------------------------------------------------------------------------*
021800000214     C* CHKREC - DETERMINAZIONE VALIDITA' RECORD
021900000214     C*------------------------------------------------------------------------*
022000000214     C     CHKREC        BEGSR
022100000214     C*
022200000214     C                   SETON                                        55
022300000214     C*
022400000214     C* VERIFICO I LIMITI
022500000214     C     EVTVOC        IFGE      PA1VOC
022600000214     C     EVTVOC        ANDLE     PA2VOC
022700000214     C     EVTDDV        ANDLE     PA1DDV
022800000214     C     EVTDSV        ANDGE     PA2DDV
022900000214     C     EVTATB        ANDEQ     *BLANKS
023000000214     C*
023100000214     C     EVTTVO        IFEQ      'M'
023200000214     C                   Z-ADD     EVTVOC        KVMVOC
023300000214     C                   Z-ADD     EVTPRG        KVMPRG
023400000214     C     KEYEVM        SETLL     ECEVM01L
023500000214     C     KEYEVM        READE     ECEVM01L                               21
023600000214     C     *IN21         DOWEQ     *OFF
023700000214     C*
023800000214     C     EVMATB        IFEQ      *BLANKS
023900010808     C     EVMVOC        SETLL     ECEVT0XL                           98
024000010808     C     EVMVOC        READE     ECEVT0XL                               98
024100000214     C     *IN98         DOWEQ     *OFF
024200000215     C     *IN55         ANDEQ     *ON
024300000214     C*
024400000215     C     w_EVTATB      IFEQ      *blanks
024500000215     C     w_EVTDDV      ANDLE     PA1DDV
024600000215     C     w_EVTDSV      ANDGE     PA2DDV
024700000215     C     w_EVTCMP      ANDGE     PA1CMP
024800000215     C     w_EVTCMP      ANDLE     PA2CMP
024900000215     C                   SETOFF                                       55
025000000214     C                   ELSE
025100010808     C     EVMVOC        READE     ECEVT0XL                               98
025200000214     C                   ENDIF
025300000214     C*
025400000214     C                   ENDDO
025500000215     C                   ENDIF
025600000214     C*
025700000214     C* PROCEDO LEGGENDO PROSSIVA VOCE MAGGIORATA DA VOCE MAGGIORANTE
025800000214     C     KEYEVM        READE     ECEVM01L                               21
025900000214     C                   ENDDO
026000000214     C*
026100000214     C                   ELSE
026200000214     C     EVTCMP        IFGE      PA1CMP
026300000214     C     EVTCMP        ANDLE     PA2CMP
026400000214     C                   SETOFF                                       55
026500000214     C                   ENDIF
026600000214     C                   ENDIF
026700000214     C*
026800000214     C                   ENDIF
026900000214     C*
027000000214     C                   ENDSR
027100000209     C*------------------------------------------------------------------------*
027200000209     C* VALTES - VALORIZZAZIONE CAMPI DI TESTATA
027300000209     C*------------------------------------------------------------------------*
027400000209     C     VALTES        BEGSR
027500000209     C*
027600000209     C* DATA
027700000209     C     PA2DDV        IFGT      *ZEROS
027800000209     C                   Z-ADD     SAVDAT        STPDAT
027900000209     C                   ELSE
028000000209     C                   Z-ADD     *ZEROS        STPDAT
028100000209     C                   ENDIF
028200000209     C*
028300000209     C* COMPETENZE
028400000209     C                   Z-ADD     PA1CMP        STPCMPDA
028500000209     C                   Z-ADD     PA2CMP        STPCMPAL
028600000209     C*
028700000209     C* VOCI
028800000209     C                   Z-ADD     PA1VOC        STPVOCDA
028900000209     C                   Z-ADD     PA2VOC        STPVOCAL
029000000209     C*
029100000209     C* TARIFFE
029200000209     C     PA1SON        IFEQ      'S'
029300000209     C                   MOVEL     'Incluse'     STPTAR
029400000209     C                   ELSE
029500000209     C                   MOVEL     'Escluse'     STPTAR
029600000209     C                   ENDIF
029700000209     C*
029800000209     C* TIPO DI STAMPA
029900000209     C     PA1SOD        IFEQ      'S'
030000000209     C                   MOVEL     'Sintetica'   STPSID
030100000209     C                   ELSE
030200000209     C                   MOVEL     'Dettaglio'   STPSID
030300000209     C                   ENDIF
030400000209     C*
030500000209     C* ORDINAMENTO
030600000209     C     PA1COV        IFEQ      'V'
030700000209     C                   MOVEL     'Voci'        STPORD
030800000209     C                   ELSE
030900000209     C                   MOVEL     'Competenze'  STPORD
031000000209     C                   ENDIF
031100000209     C*
031200000209     C                   ENDSR
031300000000     C*------------------------------------------------------------------------*
031400000000     C* DETTAG - ESAMINA IL DETTAGLIO COMPETENZE (FILE ECECD00F)
031500000000     C*------------------------------------------------------------------------*
031600000000     C     DETTAG        BEGSR
031700000000     C*
031800000000     C* RIEMPO I CAMPI DI STAMPA
031900000000     C                   MOVEL     EVTATB        P1CATB
032000000000     C                   Z-ADD     EVTVOC        P1CVOC
032100000000     C                   Z-ADD     EVTPRG        P1CPRG
032200000000     C                   MOVEL     EVTVOD        P1CVOD
032300000000     C                   MOVEL     EVTTVO        P1CTVO
032400000000     C                   MOVEL     EVTVRV        P1CVRV
032500021206     C                   MOVEL     EVTCOP        P1CCOP
032600021206     C                   MOVEL     EVTDAT        P1CDAT
032700021209     C                   MOVEL     EVTAID        P1CAID
032800000000     C                   MOVEL     '2R'          TBLCOD
032900000000     C                   MOVEL     EVTVRV        TBLKEY
033000000000     C     KEYTAB        CHAIN     TABEL00F                           99
033100000000     C  N99              MOVEL     TBLUNI        P1DVRV
033200000000     C   99              MOVEL     *BLANKS       P1DVRV
033300000000     C                   Z-ADD     EVTDDV        G08INV
033400000000     C                   Z-ADD     *ZEROS        G08DAT
033500000000     C                   MOVEL     '3'           G08ERR
033600000000     C                   CALL      'XSRDA8'
033700000224     C                   PARM                    WLBDA8
033800000000     C                   Z-ADD     G08DAT        P1CDDV
033900000000     C                   Z-ADD     EVTDSV        G08INV
034000000000     C                   Z-ADD     *ZEROS        G08DAT
034100000000     C                   MOVEL     '3'           G08ERR
034200000000     C                   CALL      'XSRDA8'
034300000224     C                   PARM                    WLBDA8
034400000000     C                   Z-ADD     G08DAT        P1CDSV
034500000000     C                   MOVEL     EVTTIV        P1CTIV
034600000000     C                   MOVEL     '2R'          TBLCOD
034700000000     C                   MOVEL     EVTTIV        TBLKEY
034800000000     C     KEYTAB        CHAIN     TABEL00F                           99
034900000000     C  N99              MOVEL     TBLUNI        P1DTIV
035000000000     C   99              MOVEL     *BLANKS       P1DTIV
035100000000     C                   MOVEL     EVTSPT        P1CSPT
035101030422     c                   if        evttiv = '05'
035102030422     c                   movel(p)  evtxxx        devtxxx
035103030422     c                   else
035104030422     c                   clear                   devtxxx
035105030422     c                   end
035106030422     C                   movel     evtsps        p1csps
035107030422     C                   z-add     evtrpv        p1Crpv
035201030422     C                   Z-ADD     EVTCMP        P1CCMP
035300000214     C                   Z-ADD     EVTCMP        $CTCMP
035400000214     C                   Z-ADD     SAVNBC        $CTNBC
035500040128     C*                  Z-ADD     EVTDDV        $CTDDC
035501040128     C                   Z-ADD     EVTDsv        $CTDDC
035600000214     C     KEYECT        SETGT     ECECT02L
035700000214     C                   READP     ECECT02L                               99
035800000214     C     *IN99         IFEQ      *OFF
035900000214     C     ECTCMP        ANDEQ     $CTCMP
036000000214     C     ECTNBC        ANDEQ     $CTNBC
036100000214     C                   MOVEL     ECTCMD        P1CCMD
036200000214     C                   ELSE
036300000214     C                   SETON                                        99
036400000214     C                   ENDIF
036500000214     C   99              MOVEL     *BLANKS       P1CCMD
036600000000     C*
036700000000     C* RIFERIMENTI TARIFFA
036800000000     C                   MOVEL     *BLANKS       P1DRFI
036900000000     C                   MOVEL     EVTRFI        P1CRFI
037000000000IF  1C     EVTRFI        IFNE      *BLANKS
037100000000     C                   MOVEL     '2Q'          TBLCOD
037200000000     C                   MOVEL     EVTRFI        TBLKEY
037300000000     C     KEYTAB        CHAIN     TABEL00F                           99
037400000000     C  N99              MOVEL     TBLUNI        P1DRFI
037500000000E   1C                   ENDIF
037600000000     C                   MOVEL     *BLANKS       P1DRFE
037700000000     C                   MOVEL     EVTRFE        P1CRFE
037800000000IF  1C     EVTRFE        IFNE      *BLANKS
037900000000     C                   MOVEL     '2Q'          TBLCOD
038000000000     C                   MOVEL     EVTRFE        TBLKEY
038100000000     C     KEYTAB        CHAIN     TABEL00F                           99
038200000000     C  N99              MOVEL     TBLUNI        P1DRFE
038300000000E   1C                   ENDIF
038400000000     C*
038500000000     C                   MOVEL     *BLANKS       P1DRTI
038600000000     C                   MOVEL     EVTRTI        P1CRTI
038700000000IF  1C     EVTRTI        IFNE      *BLANKS
038800000000     C                   MOVEL     '2Q'          TBLCOD
038900000000     C                   MOVEL     EVTRTI        TBLKEY
039000000000     C     KEYTAB        CHAIN     TABEL00F                           99
039100000000     C  N99              MOVEL     TBLUNI        P1DRTI
039200000000E   1C                   ENDIF
039300000000     C                   MOVEL     *BLANKS       P1DRTE
039400000000     C                   MOVEL     EVTRTE        P1CRTE
039500000000IF  1C     EVTRTE        IFNE      *BLANKS
039600000000     C                   MOVEL     '2Q'          TBLCOD
039700000000     C                   MOVEL     EVTRTE        TBLKEY
039800000000     C     KEYTAB        CHAIN     TABEL00F                           99
039900000000     C  N99              MOVEL     TBLUNI        P1DRTE
040000000000E   1C                   ENDIF
040100000000     C*
040200000000     C                   MOVEL     *BLANKS       P1DRPI
040300000000     C                   MOVEL     EVTRPI        P1CRPI
040400000000IF  1C     EVTRPI        IFNE      *BLANKS
040500000000     C                   MOVEL     '2Q'          TBLCOD
040600000000     C                   MOVEL     EVTRPI        TBLKEY
040700000000     C     KEYTAB        CHAIN     TABEL00F                           99
040800000000     C  N99              MOVEL     TBLUNI        P1DRPI
040900000000E   1C                   ENDIF
041000000000     C                   MOVEL     *BLANKS       P1DRPE
041100000000     C                   MOVEL     EVTRPE        P1CRPE
041200000000IF  1C     EVTRPE        IFNE      *BLANKS
041300000000     C                   MOVEL     '2Q'          TBLCOD
041400000000     C                   MOVEL     EVTRPE        TBLKEY
041500000000     C     KEYTAB        CHAIN     TABEL00F                           99
041600000000     C  N99              MOVEL     TBLUNI        P1DRPE
041700000000E   1C                   ENDIF
041800000000     C*
041900000000     C                   MOVEL     *BLANKS       P1DRKI
042000000000     C                   MOVEL     EVTRKI        P1CRKI
042100000000IF  1C     EVTRKI        IFNE      *BLANKS
042200000000     C                   MOVEL     '2Q'          TBLCOD
042300000000     C                   MOVEL     EVTRKI        TBLKEY
042400000000     C     KEYTAB        CHAIN     TABEL00F                           99
042500000000     C  N99              MOVEL     TBLUNI        P1DRKI
042600000000E   1C                   ENDIF
042700000000     C                   MOVEL     *BLANKS       P1DRKE
042800000000     C                   MOVEL     EVTRKE        P1CRKE
042900000000IF  1C     EVTRKE        IFNE      *BLANKS
043000000000     C                   MOVEL     '2Q'          TBLCOD
043100000000     C                   MOVEL     EVTRKE        TBLKEY
043200000000     C     KEYTAB        CHAIN     TABEL00F                           99
043300000000     C  N99              MOVEL     TBLUNI        P1DRKE
043400000000E   1C                   ENDIF
043500000000     C*
043600000000     C                   Z-ADD     EVTARL        P1CARL
043700000000     C                   Z-ADD     EVTARF        P1CARF
043800000000     C                   Z-ADD     EVTARO        P1CARO
043900000000     C*
044000000209     C   30              WRITE     EC83T1
044100000209     C   30              SETOFF                                       30
044200000209     C                   WRITE     EC83D1
044300000000     C*
044400000000     C* STAMPO I RIFERIMENTI SOLO SE CE N'E' ALMENO UNO
044500000000IF  1C     EVTRFI        IFNE      *BLANKS
044600000000     C     EVTRFE        ORNE      *BLANKS
044700000000     C     EVTRTI        ORNE      *BLANKS
044800000000     C     EVTRTE        ORNE      *BLANKS
044900000000     C     EVTRPI        ORNE      *BLANKS
045000000000     C     EVTRPE        ORNE      *BLANKS
045100000000     C     EVTRKI        ORNE      *BLANKS
045200000000     C     EVTRKE        ORNE      *BLANKS
045300000000     C   30              WRITE     EC83T1
045400000000     C   30              SETOFF                                       30
045500000000     C                   WRITE     EC83D9
045600000000E   1C                   ENDIF
045700000000     C*
045800000000     C                   Z-ADD     1             $ALMEN
045900000000     C*
046000000000     C* DEVO EMETTERE SOLO I RECORD DI ECEVD CHE HANNO:
046100000000     C* ..  STESSO COD.VOCE DI ECEVT
046200000000     C* ..  PERIODO DI VALIDITA' COMPRESO (O UGUALE) ALLA VALIDITA' SU ECEVT
046300000000     C*
046400000000     C                   Z-ADD     *ZERO         $TEST2            1 0
046500000000     C                   Z-ADD     EVTVOC        KVDVOC
046600000000     C                   Z-ADD     EVTPRG        KVDPRG
046700000000     C     KEYEVD        SETLL     ECEVD01L
046800000000     C     KEYEVD        READE     ECEVD01L                               21
046900000000     C     *IN21         DOWEQ     *OFF
047000000209     C*
047100000209     C* VERIFICO IN BASE ALLE RICHIESTE DI LANCIO IL PERIODO VALIDITA'
047200000209     C                   MOVEL     '0'           TAROK             1
047300000209     C*
047400000209     C     PA1SON        IFEQ      'S'
047500000209     C     EVDDDT        IFGE      EVTDDV
047600000209     C     EVDDST        ANDLE     EVTDSV
047700000209     C     EVDDST        ANDGE     PA2DDV
047800000209     C                   MOVEL     '1'           TAROK
047900000209     C                   ENDIF
048000000209     C                   ELSE
048100000209     C     EVDDDT        IFLE      PA1DDV
048200000209     C     EVDDST        ANDGE     PA2DDV
048300000209     C                   MOVEL     '1'           TAROK
048400000209     C                   ENDIF
048500000209     C                   ENDIF
048600000000     C*
048700000209     C* SE DATE VALIDITA' => OK
048800000209     C     TAROK         IFEQ      '1'
048900940727     C     EVDATB        ANDEQ     *BLANKS
049000000000     C                   EXSR      STAMP1
049100000000     C                   ENDIF
049200000000     C*
049300000000     C     KEYEVD        READE     ECEVD01L                               21
049400000000     C                   ENDDO
049500000000     C*
049600000000     C                   Z-ADD     *ZERO         $TEST3            1 0
049700000000     C*
049800000000     C* QUINDI E' LA VOLTA DELLE MAGGIORAZIONI
049900000000     C                   Z-ADD     *ZERO         $M
050000000000     C                   MOVEA     *BLANKS       MTB
050100000000     C                   Z-ADD     *ZERO         MOC
050200000000     C                   MOVEA     *BLANKS       MOD
050300000000     C                   Z-ADD     $VTVOC        KVMVOC
050400000000     C                   Z-ADD     $VTPRG        KVMPRG
050500000000     C     KEYEVM        SETLL     ECEVM01L
050600000000     C     KEYEVM        READE     ECEVM01L                               21
050700000000     C     *IN21         DOWEQ     *OFF
050800940727     C*
050900940727     C     EVMATB        IFEQ      *BLANKS
051000000000     C                   EXSR      STAMP2
051100940727     C                   ENDIF
051200940727     C*
051300000000     C     KEYEVM        READE     ECEVM01L                               21
051400000000     C                   ENDDO
051500000000     C*
051600000000     C* STAMPO LA RIGA CON LE ULTIME MAGGIORAZIONI, SE CI SONO
051700000000     C     $M            IFGT      *ZERO
051800000000     C                   WRITE     EC83D3
051900000000     C                   ENDIF
052000000000     C*
052100000000     C                   ENDSR
052200000000     C*------------------------------------------------------------------------*
052300000000     C* STAMP1 - STAMPA DEL DETTAGLIO (TARIFFE)
052400000000     C*------------------------------------------------------------------------*
052500000000     C     STAMP1        BEGSR
052600000000     C*
052700000000     C* TESTO L'OVERFLOW
052800000000     C     *IN30         IFEQ      *ON
052900000000     C                   WRITE     EC83T1
053000000000     C                   WRITE     EC83T2
053100000000     C                   SETOFF                                       30
053200940727     C                   Z-ADD     1             $TEST2
053300000000     C                   ENDIF
053400000000     C*
053500000000     C* RIEMPO I CAMPI DI STAMPA
053600000000     C                   MOVEL     EVDATB        P2CATB
053700000000     C                   MOVEL     EVDTTV        P2CTTV
053800000000     C                   MOVEL     '2S'          TBLCOD
053900000000     C                   MOVEL     *BLANKS       TBLKEY
054000000000     C                   MOVEL     EVDTTV        TBLKEY
054100000000     C     KEYTAB        CHAIN     TABEL00F                           99
054200000000     C  N99              MOVEL     TBLUNI        P2DTTV
054300000000     C   99              MOVEL     *BLANKS       P2DTTV
054400000000     C                   SETOFF                                       313233
054500000000     C                   SETOFF                                       34
054600000000     C     EVDTTV        IFEQ      'F'
054700000000     C                   SETON                                        31
054800000000     C                   Z-ADD     EVDFT1        P2CFT1
054900000000     C     EVDFT1        CHAIN     AZORG01L                           99
055000000000     C  N99              MOVEL     ORGDES        P2DFT1
055100000000     C   99              MOVEL     *BLANKS       P2DFT1
055200000000     C                   Z-ADD     EVDFT2        P2CFT2
055300000000     C     EVDFT2        CHAIN     AZORG01L                           99
055400000000     C  N99              MOVEL     ORGDES        P2DFT2
055500000000     C   99              MOVEL     *BLANKS       P2DFT2
055600000000     C                   ENDIF
055700000000     C     EVDTTV        IFEQ      'T'
055800000000     C                   SETON                                        32
055900000000     C                   MOVEL     EVDCTS        P2CCTS
056000000000     C                   MOVEL     'CT'          TBLCOD
056100000000     C                   MOVEL     EVDCTS        TBLKEY
056200000000     C     KEYTAB        CHAIN     TABEL00F                           99
056300000000     C  N99              MOVEL     TBLUNI        P2DCTS
056400000000     C   99              MOVEL     *BLANKS       P2DCTS
056500000000     C                   ENDIF
056600000000     C     EVDTTV        IFEQ      'P'
056700000000     C                   SETON                                        33
056800000000     C                   Z-ADD     EVDCAP        P2CCAP
056900000000     C                   ENDIF
057000000000     C     EVDTTV        IFEQ      'K'
057100000000     C                   SETON                                        34
057200000000     C                   Z-ADD     EVDCLI        P2CCLI
057300011204     C                   MOVEL     *BLANKS       P2DCLI
057400011204     C* RECUPERA DESCRIZIONE CLIENTE
057500011204     C                   CLEAR                   BS69DS
057600011204     C                   CLEAR                   ACODS
057700011204     C                   MOVEL     KNSIF         I69SIF
057800011204     C                   MOVEL     EVDCLI        I69KAC
057900011204     C                   CALL      'TIBS69R'
058000011204     C                   PARM                    BS69DS
058100011204     C                   PARM                    ACODS
058200011204     C                   PARM                    INDDS
058300011204     C                   PARM                    CLPDS
058400011204     C                   PARM                    CLSDS
058500011204     C                   MOVEL     ACORAG        P2DCLI
058600000000     C                   Z-ADD     EVDCTR        P2CCTR
058700941129     C                   MOVEL     *BLANKS       P2DCTR
058800941129     C     KEYTAM        SETLL     TNTAM01L
058900941129     C     KEYTAM        READE     TNTAM01L                               21
059000941129     C     *IN21         DOWEQ     *OFF
059100941129     C     TAMDDT        IFGE      EVDDDT
059200941129     C     TAMDST        ANDLE     EVDDST
059300941129     C  N99              MOVEL     TAMDCV        P2DCTR
059400941129     C                   ENDIF
059500941129     C     KEYTAM        READE     TNTAM01L                               21
059600941129     C                   ENDDO
059700000000     C                   ENDIF
059800000000     C*
059900000000     C                   Z-ADD     EVDDDT        G08INV
060000000000     C                   Z-ADD     *ZEROS        G08DAT
060100000000     C                   MOVEL     '3'           G08ERR
060200000000     C                   CALL      'XSRDA8'
060300000224     C                   PARM                    WLBDA8
060400000000     C                   Z-ADD     G08DAT        P2CDDT
060500000000     C                   Z-ADD     EVDDST        G08INV
060600000000     C                   Z-ADD     *ZEROS        G08DAT
060700000000     C                   MOVEL     '3'           G08ERR
060800000000     C                   CALL      'XSRDA8'
060900000224     C                   PARM                    WLBDA8
061000000000     C                   Z-ADD     G08DAT        P2CDST
061100000000     C                   Z-ADD     EVDSGI        P2CSGI
061200000000     C                   Z-ADD     EVDSGF        P2CSGF
061300000000     C                   Z-ADD     EVDITR        P2CITR
061400000000     C                   Z-ADD     EVDIMI        P2CIMI
061500000000     C                   Z-ADD     EVDIMA        P2CIMA
061600000000     C                   Z-ADD     EVDPER        P2CPER
061700000000     C                   MOVEL     EVDSEG        P2CSEG
061800000000     C                   Z-ADD     EVDRPV        P2CRPV
061900000000     C*
062000000000     C* SE NON HO ANCORA STAMPATO LA TESTATA, LO FACCIO ORA
062100000000     C     $TEST2        IFEQ      *ZERO
062200000000     C                   WRITE     EC83T2
062300000000     C                   Z-ADD     1             $TEST2
062400000000     C                   ENDIF
062500000000     C*
062600000000     C                   WRITE     EC83D2
062700000000     C*
062800000000     C                   ENDSR
062900000000     C*------------------------------------------------------------------------*
063000000000     C* STAMP2 - STAMPA DEL DETTAGLIO (MAGGIORAZIONI)
063100000000     C*------------------------------------------------------------------------*
063200000000     C     STAMP2        BEGSR
063300000000     C*
063400000000     C* TESTO L'OVERFLOW
063500000000     C     *IN30         IFEQ      *ON
063600000000     C                   WRITE     EC83T1
063700000000     C                   WRITE     EC83T3
063800000000     C                   SETOFF                                       30
063900000000     C                   ENDIF
064000000000     C*
064100000000     C* SE NON HO ANCORA STAMPATO LA TESTATA, LO FACCIO ORA
064200000000     C     $TEST3        IFEQ      *ZERO
064300000000     C                   WRITE     EC83T3
064400000000     C                   Z-ADD     1             $TEST3
064500000000     C                   ENDIF
064600000000     C*
064700000000     C* NON STAMPO MA AGGIUNGO NELLE SCHIERE POICHE' NE HO 4 PER RIGA
064800000000     C                   ADD       1             $M
064900000000     C                   MOVEL     EVMATB        MTB($M)
065000000000     C                   Z-ADD     EVMVOC        MOC($M)
065100000000     C     EVMVOC        CHAIN     ECEVT01L                           99
065200000000     C  N99              MOVEL     EVTVOD        MOD($M)
065300000000     C   99              MOVEL     *BLANKS       MOD($M)
065400000000     C*
065500000000     C* STAMPO LA RIGA SOLO SE HO RAGGIUNTO 4 MAGGIORAZIONI
065600000000     C     $M            IFEQ      4
065700000000     C                   WRITE     EC83D3
065800000000     C                   Z-ADD     *ZERO         $M                1 0
065900000000     C                   MOVEA     *BLANKS       MTB
066000000000     C                   Z-ADD     *ZERO         MOC
066100000000     C                   MOVEA     *BLANKS       MOD
066200000000     C                   ENDIF
066300000000     C*
066400000000     C                   ENDSR
066500000209     C*------------------------------------------------------------------------*
066600000209     C* SINTAG - ESAMINA IL SINTETICO VOCI
066700000209     C*------------------------------------------------------------------------*
066800000209     C     SINTAG        BEGSR
066900000209     C*
067000000211     C* "PULISCO" AD OGNI PASSAGGIO IL FORMATO RECORD DI STAMPA 1°
067100000211     C                   CLEAR                   EC82D2
067200000211     C*
067300000211     C     PA1COV        IFEQ      'C'
067400000211     C     EVTTVO        ANDEQ     'M'
067500000211     C*
067600000211     C                   ELSE
067700000211     C*
067800000209     C* RIEMPIO BUFFER X FORMATO RECORD VOCE 'V'
067900000209     C                   Z-ADD     EVTVOC        P1CVOC
068000000209     C                   Z-ADD     EVTPRG        P1CPRG
068100000209     C                   MOVEL     EVTVOD        P1CVOD
068200000209     C                   MOVEL     EVTTVO        P1CTVO
068300021206     C                   MOVEL     EVTCOP        P1CCOP
068400021209     C                   MOVEL     EVTDAT        P1CDAT
068401040130     c* decodifico il tipo data voce
068402040130     c                   movel     evtdat        tbeke1
068403040130     c                   exsr      srco1
068404040130     c                   movel     §co1des       p1cvdt
068500021209     C                   MOVEL     EVTAID        P1CAID
068600000209     C* COSTRUZIONE CHIAVE PER REPERIMENTO COMPETENZA
068700000209     C                   Z-ADD     EVTCMP        $CTCMP
068800000209     C                   Z-ADD     SAVNBC        $CTNBC
068900040128     C*                  Z-ADD     EVTDDV        $CTDDC
068901040128     C                   Z-ADD     EVTDSV        $CTDDC
069000000214     C     KEYECT        SETGT     ECECT02L
069100000214     C                   READP     ECECT02L                               88
069200000209     C     *IN88         IFEQ      *OFF
069300000214     C     ECTCMP        ANDEQ     $CTCMP
069400000214     C     ECTNBC        ANDEQ     $CTNBC
069500000210     C     ECTATB        ANDEQ     *blanks
069600000210     C     ECTDDC        ANDLE     PA1DDV
069700000210     C     ECTDSC        ANDGE     PA2DDV
069800000209     C                   Z-ADD     ECTCMP        P1CCMP
069900000209     C                   MOVEL     ECTCMD        P1CCMD
069901040130     c                   movel     ectfdr        tbeke1
069902040130     c                   exsr      srco1
069903040130     c                   movel     §co1des       p1ccdt
069904040130     C                   Z-ADD     Ectddc        G08INV
069905040130     C                   Z-ADD     *ZEROS        G08DAT
069906040130     C                   MOVEL     '3'           G08ERR
069907040130     C                   CALL      'XSRDA8'
069908040130     C                   PARM                    WLBDA8
069909040130     C                   move      G08DAT        P1cddc
069910040130     C                   Z-ADD     Ectdsc        G08INV
069911040130     C                   Z-ADD     *ZEROS        G08DAT
069912040130     C                   MOVEL     '3'           G08ERR
069913040130     C                   CALL      'XSRDA8'
069914040130     C                   PARM                    WLBDA8
069915040130     C                   move      G08DAT        P1cdsc
070000000210     C                   ELSE
070001040130     C                   clear                   P1cdsc
070002040130     C                   clear                   P1cddc
070003040130     c                   clear                   p1ccdt
070100040130     C                   z-add     0             P1CCMP
070200000211     C                   MOVEL     *blanks       P1CCMD
070300000211     C                   ENDIF
070400000210     C*
070500000209     C   31              WRITE     EC82T1
070600000209     C   31              SETOFF                                       31
070700000210     C     PA1COV        IFEQ      'C'
070800000210     C     $VTCMP        ANDNE     SAVKEY
070900000209     C                   WRITE     EC82RB
071000000210     C                   Z-ADD     $VTCMP        SAVKEY
071100000210     C                   ENDIF
071200000214     C                   WRITE     EC82D2
071300000214     C                   Z-ADD     1             $ALMEN
071400000210     C*
071500000210     C*-------------------------------------------------------------------*
071600000211     C*
071700000211     C* "PULISCO" AD OGNI PASSAGGIO IL FORMATO RECORD DI STAMPA 2°
071800000211     C                   CLEAR                   EC82D3
071900000211     C*
072000000209     C* VERIFICO IL TIPO DI VOCE "CORRENTE"
072100000209     C* SE VOCE DI MAGGIORAZIONE ESEGUO CONSIDERAZIONE E STAMPA PARTICOLARI
072200000209     C     EVTTVO        IFEQ      'M'
072300000209     C                   Z-ADD     EVTVOC        KVMVOC
072400000209     C                   Z-ADD     EVTPRG        KVMPRG
072500000209     C     KEYEVM        SETLL     ECEVM01L
072600000209     C     KEYEVM        READE     ECEVM01L                               21
072700000209     C     *IN21         DOWEQ     *OFF
072800000209     C*
072900000209     C* RIEMPIO BUFFER FORMATO RECORD X MAGGIORAZIONI
073000000215     C     EVMATB        IFEQ      *BLANKS
073100000209     C                   Z-ADD     EVMVOC        P1CVOC
073200000209     C* REPERISCO I DATI "VOCE" DELLA VOCE DA MAGGIORARE
073300000209     C                   Z-ADD     EVMVOC        $VTVOC3
073400010808     C     EVMVOC        SETLL     ECEVT0XL                           98
073500010808     C     EVMVOC        READE     ECEVT0XL                               98
073600000214     C                   MOVEL     '0'           CHKVOC            1
073700000214     C     *IN98         DOWEQ     *OFF
073800000214     C     CHKVOC        ANDEQ     '0'
073900000214     C*
074000000215     C     w_EVTDDV      IFLE      PA1DDV
074100000215     C     w_EVTDSV      ANDGE     PA2DDV
074200000214     C                   MOVEL     '1'           CHKVOC
074300000214     C                   ELSE
074400010808     C     EVMVOC        READE     ECEVT0XL                               98
074500000214     C                   ENDIF
074600000214     C*
074700000214     C                   ENDDO
074800000214     C*
074900000214     C     CHKVOC        IFEQ      '1'
075000000215     C     w_EVTATB      IFEQ      *BLANKS
075100000215     C**   w_EVTDDV      ANDLE     PA1DDV
075200000215     C**   w_EVTDSV      ANDGE     PA2DDV
075300000215     C                   MOVEL     w_EVTVOD      P1CVOD
075400000210     C* E REPERISCO I DATI "COMPETENZA" DELLA VOCE DA MAGGIORARE
075500000215     C                   Z-ADD     w_EVTCMP      $CTCMP
075600000210     C                   Z-ADD     SAVNBC        $CTNBC
075700040128     C*                  Z-ADD     w_EVTDDV      $CTDDC
075701040128     C                   Z-ADD     w_EVTDSV      $CTDDC
075800000214     C     KEYECT        SETGT     ECECT02L
075900000214     C                   READP     ECECT02L                               88
076000000211     C     *IN88         IFEQ      *OFF
076100000214     C     ECTCMP        IFEQ      $CTCMP
076200000214     C     ECTNBC        ANDEQ     $CTNBC
076300000214     C     ECTATB        ANDEQ     *blanks
076400000210     C     ECTDDC        ANDLE     PA1DDV
076500000210     C     ECTDSC        ANDGE     PA2DDV
076600000210     C                   Z-ADD     ECTCMP        P1CCMP
076700000210     C                   MOVEL     ECTCMD        P1CCMD
076701040130     c                   movel     ectfdr        tbeke1
076702040130     c                   exsr      srco1
076703040130     c                   movel     §co1des       p1ccdt
076704040130     C                   Z-ADD     Ectddc        G08INV
076705040130     C                   Z-ADD     *ZEROS        G08DAT
076706040130     C                   MOVEL     '3'           G08ERR
076707040130     C                   CALL      'XSRDA8'
076708040130     C                   PARM                    WLBDA8
076709040130     C                   move      G08DAT        P1cddc
076710040130     C                   Z-ADD     Ectdsc        G08INV
076711040130     C                   Z-ADD     *ZEROS        G08DAT
076712040130     C                   MOVEL     '3'           G08ERR
076713040130     C                   CALL      'XSRDA8'
076714040130     C                   PARM                    WLBDA8
076715040130     C                   move      G08DAT        P1cdsc
076800000210     C                   ELSE
076801040130     c                   movel     ectfdr        tbeke1
076802040130     c                   exsr      srco1
076803040130     c                   movel     §co1des       p1ccdt
076804040130     C                   Z-ADD     Ectddc        G08INV
076805040130     C                   Z-ADD     *ZEROS        G08DAT
076806040130     C                   MOVEL     '3'           G08ERR
076807040130     C                   CALL      'XSRDA8'
076808040130     C                   PARM                    WLBDA8
076809040130     C                   move      G08DAT        P1cddc
076810040130     C                   Z-ADD     Ectdsc        G08INV
076811040130     C                   Z-ADD     *ZEROS        G08DAT
076812040130     C                   MOVEL     '3'           G08ERR
076813040130     C                   CALL      'XSRDA8'
076814040130     C                   PARM                    WLBDA8
076815040130     C                   move      G08DAT        P1cdsc
076900000211     C                   Z-ADD     ECTCMP        P1CCMP
077000000210     C                   MOVEL     *all'*'       P1CCMD
077100000210     C                   ENDIF
077200000211     C                   ELSE
077300000211     C                   Z-ADD     *zeros        P1CCMP
077400000211     C                   MOVEL     *all'*'       P1CCMD
077401040130     C                   clear                   P1cdsc
077402040130     C                   clear                   P1cddc
077405040130     c                   clear                   p1ccdt
077500000211     C                   ENDIF
077600000211     C                   ELSE
077700000210     C                   MOVEL     *all'*'       P1CVOD
077800000211     C                   Z-ADD     *zeros        P1CCMP
077900000210     C                   MOVEL     *all'*'       P1CCMD
077901040130     C                   clear                   P1cdsc
077902040130     C                   clear                   P1cddc
077903040130     c                   clear                   p1ccdt
078000000211     C                   ENDIF
078100000211     C                   ELSE
078200000211     C                   Z-ADD     *zeros        P1CVOC
078300000211     C                   MOVEL     *all'*'       P1CVOD
078400000211     C                   Z-ADD     *zeros        P1CCMP
078500000211     C                   MOVEL     *all'*'       P1CCMD
078501040130     C                   clear                   P1cdsc
078502040130     C                   clear                   P1cddc
078503040130     c                   clear                   p1ccdt
078600000211     C                   ENDIF
078700000210     C                   ELSE
078800000211     C                   Z-ADD     *zeros        P1CVOC
078900000211     C                   MOVEL     *all'*'       P1CVOD
079000000211     C                   Z-ADD     *zeros        P1CCMP
079100000211     C                   MOVEL     *all'*'       P1CCMD
079101040130     C                   clear                   P1cdsc
079102040130     C                   clear                   P1cddc
079103040130     c                   clear                   p1ccdt
079200000210     C                   ENDIF
079300000209     C*
079400000209     C   31              WRITE     EC82T1
079500000209     C   31              SETOFF                                       31
079600000215     C     w_EVTCMP      IFGE      PA1CMP
079700000215     C     w_EVTCMP      ANDLE     PA2CMP
079800000210     C                   WRITE     EC82D3
079900000215     C                   ENDIF
080000000209     C*
080100000209     C* PROCEDO LEGGENDO PROSSIVA VOCE MAGGIORATA DA VOCE MAGGIORANTE
080200000209     C     KEYEVM        READE     ECEVM01L                               21
080300000209     C                   ENDDO
080400000209     C*
080500000211     C                   ENDIF
080600000211     C                   ENDIF
080700000209     C*
080800000209     C                   ENDSR
080900000210     C*------------------------------------------------------------------------*
081000000210     C* CARMAG - CARICA IN SCHIERA LE INFORMAZIONI RELATIVE ALLE VOCI IN MAGG.
081100000210     C*------------------------------------------------------------------------*
081200000210     C     CARMAG        BEGSR
081300000210     C*
081400000211     C                   z-add     1             i                 3 0
081500000210     C*
081600000210     C                   open      ECEVT01L
081700000210     C     *loval        setll     ECEVT01L                           66
081800000210     C     *in66         doweq     *off
081900000210     C                   read      ECEVT01L                               66
082000000210     C*
082100000210     C     *in66         ifeq      *off
082200000210     C     EVTATB        andeq     *blanks
082300000210     C     EVTTVO        andeq     'M'
082400000210     C     EVTDDV        andle     PA1DDV
082500000210     C     EVTDSV        andge     PA2DDV
082600000210     C* RIEMPIO VARIABILI DI DEPOSITO X RIPETIZIONE VOCI DI MAGGIORAZIONE
082700000210     C                   z-add     EVTVOC        VOCMTES
082800000210     C                   z-add     EVTPRG        PRGMTES
082900000210     C                   movel     EVTVOD        DESMTES
083000000210     C                   movel     EVTTVO        TIPMTES
083100000211     C*
083200000211     C                   Z-ADD     EVTVOC        KVMVOC
083300000211     C                   Z-ADD     EVTPRG        KVMPRG
083400000211     C     KEYEVM        SETLL     ECEVM01L
083500000211     C     KEYEVM        READE     ECEVM01L                               21
083600000211     C     *IN21         DOWEQ     *OFF
083700000211     C*
083800000211     C* IMPOSTO SEMPRE I CAMPI "COSTANTI" (X VOCE)
083900000211     C                   z-add     VOCMTES       VOCMTE(i)
084000000211     C                   z-add     PRGMTES       PRGMTE(i)
084100000211     C                   movel     DESMTES       DESMTE(i)
084200000211     C                   movel     TIPMTES       TIPMTE(i)
084300000211     C*
084400000211     C* RIEMPIO BUFFER FORMATO RECORD X MAGGIORAZIONI
084500000211     C     EVMATB        IFEQ      *BLANKS
084600000211     C                   z-add     EVMVOC        VOCMTA(i)
084700000211     C                   z-add     EVMPRM        PRGMTA(i)
084800000211     C* REPERISCO I DATI "VOCE" DELLA VOCE DA MAGGIORARE
084900000211     C                   Z-ADD     EVMVOC        $VTVOC3
085000000211     C                   Z-ADD     EVMPRG        $VTPRG3
085100010808     C     EVMVOC        SETLL     ECEVT0XL                           98
085200010808     C     EVMVOC        READE     ECEVT0XL                               98
085300000214     C                   MOVEL     '0'           CHKVOC            1
085400000214     C     *IN98         DOWEQ     *OFF
085500000214     C     CHKVOC        ANDEQ     '0'
085600000214     C*
085700000215     C     w_EVTDDV      IFLE      PA1DDV
085800000215     C     w_EVTDSV      ANDGE     PA2DDV
085900000214     C                   MOVEL     '1'           CHKVOC
086000000214     C                   ELSE
086100010808     C     EVMVOC        READE     ECEVT0XL                               98
086200000214     C                   ENDIF
086300000214     C*
086400000214     C                   ENDDO
086500000214     C*
086600000214     C     CHKVOC        IFEQ      '1'
086700000215     C     w_EVTATB      IFEQ      *BLANKS
086800000215     C**   w_EVTDDV      ANDLE     PA1DDV
086900000215     C**   w_EVTDSV      ANDGE     PA2DDV
087000000215     C                   movel     w_EVTVOD      DESMTA(i)
087100000211     C* E REPERISCO I DATI "COMPETENZA" DELLA VOCE DA MAGGIORARE
087200000215     C                   Z-ADD     w_EVTCMP      $CTCMP
087300000211     C                   Z-ADD     SAVNBC        $CTNBC
087400040128     C*                  Z-ADD     w_EVTDDV      $CTDDC
087401040128     C                   Z-ADD     w_EVTDSV      $CTDDC
087500000214     C     KEYECT        SETGT     ECECT02L
087600000214     C                   READP     ECECT02L                               88
087700000211     C     *IN88         IFEQ      *OFF
087800000214     C     ECTCMP        IFEQ      $CTCMP
087900000214     C     ECTNBC        ANDEQ     $CTNBC
088000000214     C     ECTATB        ANDEQ     *blanks
088100000211     C     ECTDDC        ANDLE     PA1DDV
088200000211     C     ECTDSC        ANDGE     PA2DDV
088300000211     C                   z-add     ECTCMP        CODCMP(i)
088400000211     C                   movel     ECTCMD        DESCMP(i)
088401040130     c                   movel     ectfdr        tbeke1
088402040130     c                   exsr      srco1
088403040130     c                   movel     §co1des       cmpcdt(i)
088404040130     C                   Z-ADD     Ectddc        G08INV
088405040130     C                   Z-ADD     *ZEROS        G08DAT
088406040130     C                   MOVEL     '3'           G08ERR
088407040130     C                   CALL      'XSRDA8'
088408040130     C                   PARM                    WLBDA8
088409040130     C                   move      G08DAT        cmpddc(i)
088410040130     C                   Z-ADD     Ectdsc        G08INV
088411040130     C                   Z-ADD     *ZEROS        G08DAT
088412040130     C                   MOVEL     '3'           G08ERR
088413040130     C                   CALL      'XSRDA8'
088414040130     C                   PARM                    WLBDA8
088415040130     C                   move      G08DAT        cmpdsc(i)
088500000211     C                   ELSE
088600000211     C                   Z-ADD     ECTCMP        CODCMP(i)
088700000211     C                   MOVEL     *all'*'       DESCMP(i)
088701040130     c                   movel     ectfdr        tbeke1
088702040130     c                   exsr      srco1
088703040130     c                   movel     §co1des       cmpcdt(i)
088704040130     C                   Z-ADD     Ectddc        G08INV
088705040130     C                   Z-ADD     *ZEROS        G08DAT
088706040130     C                   MOVEL     '3'           G08ERR
088707040130     C                   CALL      'XSRDA8'
088708040130     C                   PARM                    WLBDA8
088709040130     C                   move      G08DAT        cmpddc(i)
088710040130     C                   Z-ADD     Ectdsc        G08INV
088711040130     C                   Z-ADD     *ZEROS        G08DAT
088712040130     C                   MOVEL     '3'           G08ERR
088713040130     C                   CALL      'XSRDA8'
088714040130     C                   PARM                    WLBDA8
088715040130     C                   move      G08DAT        cmpdsc(i)
088800000211     C                   ENDIF
088900000211     C                   ELSE
088901040130     C                   clear                   cmpdsc(i)
088902040130     C                   clear                   cmpddc(i)
088903040130     c                   clear                   cmpcdt(i)
089000000211     C                   Z-ADD     *zeros        CODCMP(i)
089100000211     C                   MOVEL     *all'*'       DESCMP(i)
089200000211     C                   ENDIF
089300000211     C                   ELSE
089301040130     C                   clear                   cmpdsc(i)
089302040130     C                   clear                   cmpddc(i)
089303040130     c                   clear                   cmpcdt(i)
089400000211     C                   MOVEL     *all'*'       DESMTA(i)
089500000211     C                   Z-ADD     *zeros        CODCMP(i)
089600000211     C                   MOVEL     *all'*'       DESCMP(i)
089700000211     C                   ENDIF
089800000211     C                   ELSE
089801040130     C                   clear                   cmpdsc(i)
089802040130     C                   clear                   cmpddc(i)
089803040130     c                   clear                   cmpcdt(i)
089900000211     C                   Z-ADD     *zeros        VOCMTA(i)
090000000211     C                   MOVEL     *all'*'       DESMTA(i)
090100000211     C                   Z-ADD     *zeros        CODCMP(i)
090200000211     C                   MOVEL     *all'*'       DESCMP(i)
090300000211     C                   ENDIF
090400000211     C                   ELSE
090401040130     C                   clear                   cmpdsc(i)
090402040130     C                   clear                   cmpddc(i)
090403040130     c                   clear                   cmpcdt(i)
090500000211     C                   Z-ADD     *zeros        VOCMTA(i)
090600000211     C                   MOVEL     *all'*'       DESMTA(i)
090700000211     C                   Z-ADD     *zeros        CODCMP(i)
090800000211     C                   MOVEL     *all'*'       DESCMP(i)
090900000211     C                   ENDIF
091000000211     C*
091100000211     C* PROCEDO LEGGENDO PROSSIVA VOCE MAGGIORATA DA VOCE MAGGIORANTE
091200000211     C     KEYEVM        READE     ECEVM01L                               21
091300000211     C                   ADD       1             i
091400000211     C                   ENDDO
091500000211     C*
091600000211     C                   endif
091700000211     C                   enddo
091800000210     C*
091900000210     C                   close     ECEVT01L
092000000210     C*
092100000210     C                   ENDSR
092200000211     C*------------------------------------------------------------------------*
092300000211     C* GESMAG - GESTISCE LE VOCI IN MAGGIORAZIONE RISPETTO ALLE COMPETENZE
092400000211     C*------------------------------------------------------------------------*
092500000211     C     GESMAG        BEGSR
092600000211     C*
092700000211     C     1             DO        500           y                 3 0
092800000211     C     VOCMTE(y)     IFEQ      *zeros
092900000214     C     VOCMTA(y)     ANDEQ     *zeros
093000000214     C     CODCMP(y)     ANDEQ     *zeros
093100000211     C                   LEAVE
093200000211     C                   ELSE
093300000211     C*
093400000211     C* "PULISCO" AD OGNI PASSAGGIO IL FORMATO RECORD DI STAMPA 1°
093500000211     C                   CLEAR                   EC82D2
093600000214     C     CODCMP(y)     IFEQ      SAVCMP
093700000214     C     SAVCMP        ANDNE     *zeros
093800000211     C                   Z-ADD     VOCMTE(y)     P1CVOC
093900000211     C                   Z-ADD     PRGMTE(y)     P1CPRG
094000000211     C                   MOVEL     DESMTE(y)     P1CVOD
094100000211     C                   MOVEL     TIPMTE(y)     P1CTVO
094200000211     C                   Z-ADD     *ZEROS        P1CCMP
094300000211     C                   MOVEL     *BLANKS       P1CCMD
094400000211     C* STAMPO RIGA PRINCIPALE DI VOCE (GESTISCO ANCHE LA "PAG NUOVA")
094500000211     C   31              WRITE     EC82T1
094600000211     C   31              SETOFF                                       31
094700000215     C     CODCMP(y)     IFGE      PA1CMP
094800000215     C     CODCMP(y)     ANDLE     PA2CMP
094900000215     C                   WRITE     EC82D2
095000000215     C                   ENDIF
095100000211     C*
095200000211     C* "PULISCO" AD OGNI PASSAGGIO IL FORMATO RECORD DI STAMPA 2°
095300000211     C                   CLEAR                   EC82D3
095400000211     C                   Z-ADD     VOCMTA(y)     P1CVOC
095500000211     C                   MOVEL     DESMTA(y)     P1CVOD
095600000211     C                   Z-ADD     CODCMP(y)     P1CCMP
095700000211     C                   MOVEL     DESCMP(y)     P1CCMD
095701040130     C                   Z-ADD     cmpddc(y)     P1Cddc
095702040130     C                   Z-ADD     cmpdsc(y)     P1Cdsc
095703040130     C                   MOVEL     cmpcdt(y)     P1CCdt
095800000211     C* STAMPO RIGA VOCE MAGGIORATA (GESTISCO ANCHE LA "PAG NUOVA")
095900000211     C   31              WRITE     EC82T1
096000000211     C   31              SETOFF                                       31
096100000215     C     CODCMP(y)     IFGE      PA1CMP
096200000215     C     CODCMP(y)     ANDLE     PA2CMP
096300000211     C                   WRITE     EC82D3
096400000211     C                   ENDIF
096500000215     C                   ENDIF
096600000211     C                   ENDIF
096700000211     C                   ENDDO
096800000215     C*
096900000211     C                   ENDSR
097000020923     C*-------------------------------------------------------------------------
097100020923     C* REPERISCE I DATI UTENTE
097200020923     C*-------------------------------------------------------------------------
097300020923     C     REPDATIUTE    BEGSR
097400020923     C*
097500020923     C* INIZIALIZZA VARIABILI DI WRK
097600020923     C                   CLEAR                   TIBS34DS
097700020923     C                   CLEAR                   AZUTEDS
097800020923     C                   CLEAR                   DDATIUTE
097900020923     C*
098000020923     C     *DTAARA       DEFINE    §azute        azuteds
098100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
098200020923     C                   IN(E)     *DTAARA
098300020923if  1C                   IF        %Error
098400020923     c                   EVAL      I34Tla = 'L'
098500020923     C                   CALL      'TIBS34R'
098600020923     C                   PARM                    Tibs34Ds
098700020923     C                   IN        *DTAARA
098800020923e   1C                   ENDIF
098900020923     C*
099000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
099100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
099200020923     C*
099300020923     C                   ENDSR
099400000000     C*------------------------------------------------------------------------*
099500040130     C* srco1  - lettura tabella co1
099600000000     C*------------------------------------------------------------------------*
099700040130     C     srco1         BEGSR
099800000000     C*
099801040130     c                   movel     'CO1'         tbecod
099802040130     c                   clear                   dco1
099803040130     c     ktbe          chain     tntbe01l
099804040130     c                   if        %found(tntbe01l) and tbeatb = ' '
099805040130     c                   movel     tbeuni        dco1
099806040130     c                   end
099807040130     C*
099808040130     C                   ENDSR
099809040130     C*------------------------------------------------------------------------*
099810040130     C* *INZSR - IMPOSTAZIONI INIZIALI
099811040130     C*------------------------------------------------------------------------*
099812040130     C     *INZSR        BEGSR
099813040130     C*
099900000000     C* PARAMETRI IN INPUT AL PGM
100000000000     C     *ENTRY        PLIST
100100000000     C                   PARM                    KPJBA
100200010808     C                   MOVEL     KPJBU         PARAM
100300010808     C*
100400010808     C* OVRDBF PER LETTURA SECONDA CHIAVE DI INPUT
100500010808     C                   Z-ADD     80            LENGH            15 5
100600010808     C                   CALL      'QCMDEXC'
100700010808     C                   PARM                    CMD(3)
100800010808     C                   PARM                    LENGH
100900010808     C*
101000010808     C* DOPO DI CHE' APRO IL FILE VOCI DI "WRK"
101100010808     C                   OPEN      ECEVT0XL
101200000209     C*
101300000209     C* SE STAMPA RICHIESTA IN DETTAGLIO
101400000209     C     PA1SOD        IFEQ      'D'
101500000209     C                   SETON                                        50
101600000209     C                   ELSE
101700000209     C                   SETOFF                                       50
101800000209     C                   ENDIF
101900000208     C*
102000000209     C* RIGIRA LA 1° DATA PASSATA NEL PARAMETRO
102100000209     C     PA1DDV        IFGT      *ZEROS
102200000209     C                   Z-ADD     PA1DDV        SAVDAT            8 0
102300000208     C                   Z-ADD     PA1DDV        G08DAT
102400000208     C                   Z-ADD     *ZEROS        G08INV
102500000208     C                   MOVEL     '0'           G08ERR
102600000208     C                   CALL      'XSRDA8'
102700000224     C                   PARM                    WLBDA8
102800000208     C                   Z-ADD     G08INV        PA1DDV
102900000209     C                   ENDIF
103000000209     C*
103100000209     C* RIGIRA LA 2° DATA PASSATA NEL PARAMETRO
103200000209     C     PA2DDV        IFGT      *ZEROS
103300000209     C                   Z-ADD     PA2DDV        G08DAT
103400000209     C                   Z-ADD     *ZEROS        G08INV
103500000209     C                   MOVEL     '0'           G08ERR
103600000209     C                   CALL      'XSRDA8'
103700000224     C                   PARM                    WLBDA8
103800000209     C                   Z-ADD     G08INV        PA2DDV
103900000209     C                   ENDIF
104000000209     C*
104100000209     C* REPERISCO SUBITO L'ULTIMO BLOCCO (QUELLO VALIDO)
104200000209     C                   Z-ADD     *zeros        SAVNBC            5 0
104300000209     C     *hival        SETGT     ECECT01L
104400000209     C                   READP     ECECT01L                               60
104500000209     C  N60              Z-ADD     ECTNBC        SAVNBC
104600000000     C*
104700000000     C* RECUPERO DATI DELL'UTENTE
104800020923     C                   EXSR      REPDATIUTE
104900000208     C                   MOVEL     DSFIRS        PTCRSU
105000000000     C*
105100000210     C* VARIABILI RIFERITE AL DATABASE
105200000209     C     *LIKE         DEFINE    EVTVOC        $VTVOC3
105300000209     C     *LIKE         DEFINE    EVTPRG        $VTPRG3
105400000210     C     *LIKE         DEFINE    EVTCMP        SAVKEY
105500000208     C     *LIKE         DEFINE    EVTCMP        $VTCMP
105600000000     C     *LIKE         DEFINE    EVTVOC        $VTVOC
105700000000     C     *LIKE         DEFINE    EVTPRG        $VTPRG
105800000000     C     *LIKE         DEFINE    EVTVOC        KVDVOC
105900000000     C     *LIKE         DEFINE    EVTPRG        KVDPRG
106000000000     C     *LIKE         DEFINE    EVTVOC        KVMVOC
106100000000     C     *LIKE         DEFINE    EVTPRG        KVMPRG
106200000209     C     *LIKE         DEFINE    ECTCMP        $CTCMP
106300000209     C     *LIKE         DEFINE    ECTNBC        $CTNBC
106400000214     C     *LIKE         DEFINE    ECTDDC        $CTDDC
106500000210     C     *LIKE         DEFINE    EVTVOC        VOCMTES
106600000210     C     *LIKE         DEFINE    EVTPRG        PRGMTES
106700000210     C     *LIKE         DEFINE    EVTVOD        DESMTES
106800000210     C     *LIKE         DEFINE    EVTTVO        TIPMTES
106900000211     C*
107000000211     C* VARIABILI RIFERITE AL DATABASE
107100000211     C                   Z-ADD     *ZEROS        SAVCMP            3 0
107200020923     C                   Z-ADD     1             TBLKUT
107300000000     C*
107400040130     C     Ktbe          KLIST
107500040130     C                   KFLD                    tbecod
107600040130     C                   KFLD                    tbeke1
107601040130     C*
107602040130     C     KEYEVT        KLIST
107603040130     C                   KFLD                    $VTVOC
107604040130     C                   KFLD                    $VTPRG
107700000208     C*
107800000208     C     KEYEV2        KLIST
107900000208     C                   KFLD                    $VTCMP
108000000208     C                   KFLD                    $VTVOC
108100000208     C                   KFLD                    $VTPRG
108200000209     C*
108300000209     C     KEYEV3        KLIST
108400000209     C                   KFLD                    $VTVOC3
108500000209     C                   KFLD                    $VTPRG3
108600000209     C*
108700000209     C     KEYECT        KLIST
108800000209     C                   KFLD                    $CTCMP
108900000209     C                   KFLD                    $CTNBC
109000000214     C                   KFLD                    $CTDDC
109100000000     C*
109200000000     C     KEYEVD        KLIST
109300000000     C                   KFLD                    KVDVOC
109400000000     C                   KFLD                    KVDPRG
109500000000     C*
109600000000     C     KEYEVM        KLIST
109700000000     C                   KFLD                    KVMVOC
109800000000     C                   KFLD                    KVMPRG
109900000000     C*
110000000000     C     KEYTAB        KLIST
110100020923     C                   KFLD                    TBLKUT
110200000000     C                   KFLD                    TBLCOD
110300000000     C                   KFLD                    TBLKEY
110400000000     C*
110500000000     C     KEYTAM        KLIST
110600000000     C                   KFLD                    EVDCLI
110700000000     C                   KFLD                    EVDCTR
110800000211     C*
110900000211     C* CARICO LE VOCI DI MAGGIORAZIONE (E DATI CORRELATI)
111000000211     C     PA1COV        IFEQ      'C'
111100000211     C                   EXSR      CARMAG
111200000211     C                   ENDIF
111300000000     C*
111400000000     C* SE I PARAMETRI INIZIALE/FINALE SONO AMBEDUE VUOTI, IMPOSTO IL FINALE
111500000000     C     PA1VOC        IFEQ      *ZERO
111600000000     C     PA2VOC        ANDEQ     *ZERO
111700000000     C                   Z-ADD     *HIVAL        PA2VOC
111800000000     C                   ENDIF
111900000000     C*
112000000000     C     PA1CMP        IFEQ      *ZERO
112100000000     C     PA2CMP        ANDEQ     *ZERO
112200000000     C                   Z-ADD     *HIVAL        PA2CMP
112300000000     C                   ENDIF
112400000209     C* LANCIO ROUTINE DI VALORIZZAZIONE CAMPI DI TESTATA
112500000209     C                   EXSR      VALTES
112600000000     C*
112700000000     C                   ENDSR
112800000000     C*------------------------------------------------------------------------*
112900000000** OVERRIDE SUL FILE DI STAMPA
113000000000OVRPRTF FILE(TREC83P) USRDTA('Voci C/E')
113100000209OVRPRTF FILE(TREC82P) USRDTA('Voci C/E')
113200010808OVRDBF FILE(ECEVT0XL) TOFILE(ECEVT01L)
