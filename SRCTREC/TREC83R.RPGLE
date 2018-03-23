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
001500131203     FECEVD06L  IF   E           K DISK
001600000000     FECEVM01L  IF   E           K DISK
001700000210     FECEVT01L  IF   E           K DISK    USROPN
001800000208     FECEVT02L  IF   E           K DISK    RENAME(ECEVT:ECEVTxxx)
001900010808     FECEVT0XL  IF   E           K DISK    RENAME(ECEVT:ECEVTyyy) PREFIX(w_)
002000010808     F                                     USROPN
002100000000     FTABEL00F  IF   E           K DISK
002200040130     Ftntbe01l  IF   E           K DISK
002300941129     FTNTAM01L  IF   E           K DISK
002400000000     FTREC83P   O    E             PRINTER OFLIND(*IN30)
002500000000     F                                     USROPN
002600000209     FTREC82P   O    E             PRINTER OFLIND(*IN31)
002700000209     F                                     USROPN
002800000000     D*------------------------------------------------------------------------*
002900000000     D* SCHIERE
003000000000     D*------------------------------------------------------------------------*
003100010808     D CMD             S             80    DIM(3) CTDATA PERRCD(1)
003200000000     D* TABELLA CAPICONTO
003300000214     D* SCHIERE PER STAMPA DELLE MAGGIORAZIONI
003400000000     D*------------------------------------------------------------------------*
003500000000     D* INPUT
003600000000     D*------------------------------------------------------------------------*
003700000000     D*------------------
003800020923     D* DS REPERIMENTO DATI UTENTE
003900020923     D*-------------------
004000040130     D dco1          E DS
004100040130     D TIBS34DS      E DS
004200020923     D DDATIUTE      E DS
004300020923     D AZUTEDS       E DS                  extname(AZUTE00F)
004400000000     D*-------------------
004500000000     D* ARCHITETTURA
004600000000     D*-------------------
004700000000     D KPJBA         E DS
004800000000     D*-------------------
004900000000     D* PARAMETRI PASSATI DAL FILTRO
005000000000     D*-------------------
005100000208     D PARAM           DS                  INZ
005200000208     D  PA1VOC                 1      3  0
005300000208     D  PA2VOC                 4      6  0
005400000208     D  PA1DDV                 7     14  0
005500000214     D  PA1CMP                15     17  0
005600000214     D  PA2CMP                18     20  0
005700000214     D  PA1SOD                21     21
005800000214     D  PA1COV                22     22
005900000214     D  PA2DDV                23     30  0
006000000214     D  PA1SON                31     31
006100000000     D*------------------
006200000000     D* STAMPA DELLE MAGGIORAZIONI (DS PER RIEMPIRE SCHIERE)
006300000000     D* 3 SCHIERE: FLAG ANNULLAMENTO, CODICE E DESCRIZIONE
006400000000     D*------------------
006500000000     D                 DS
006600000000     D  MTB                    1      4
006700000000     D                                     DIM(4)
006800000000     D  P3CAT1                 1      1
006900000000     D  P3CAT2                 2      2
007000000000     D  P3CAT3                 3      3
007100000000     D  P3CAT4                 4      4
007200000000     D                 DS
007300000000     D  MOC                    1     12  0
007400000000     D                                     DIM(4)
007500000000     D  P3CVO1                 1      3  0
007600000000     D  P3CVO2                 4      6  0
007700000000     D  P3CVO3                 7      9  0
007800000000     D  P3CVO4                10     12  0
007900000000     D                 DS
008000000000     D  MOD                    1    120
008100000000     D                                     DIM(4)
008200000000     D  P3DVO1                 1     30
008300000000     D  P3DVO2                31     60
008400000000     D  P3DVO3                61     90
008500000000     D  P3DVO4                91    120
008600000208     D*
008700000224     D WLBDA8          DS                  INZ
008800000000     D  G08DAT                 1      8  0
008900000000     D  G08INV                 9     16  0
009000000000     D  G08ERR                17     17
009100000000     D  G08TGI                18     22  0
009200000210     D*------------
009300000210     D* SCHIERE X MEMORIZZAZIONE VOCI IN MAGGIORAZIONE
009400000210     D*------------
009500040130     D  VOCMTE         s              3  0 DIM(500) INZ
009600040130     D  PRGMTE         s              3  0 DIM(500) INZ
009700040130     D  DESMTE         s             30    DIM(500) INZ
009800040130     D  TIPMTE         s              1    DIM(500) INZ
009900040130     D  VOCMTA         s              3  0 DIM(500) INZ
010000040130     D  PRGMTA         s              3  0 DIM(500) INZ
010100040130     D  DESMTA         s             30    DIM(500) INZ
010200040130     D  CODCMP         s              3  0 DIM(500) INZ
010300040130     D  DESCMP         s             30    DIM(500) INZ
010400040130     D  Cmpddc         s              6  0 DIM(500) INZ
010500040130     D  Cmpdsc         s              6  0 DIM(500) INZ
010600040130     D  Cmpcdt         s             20    DIM(500) INZ
010700011204     D*------------------
010800011204     D* DS REPERIMENTO DATI UTENTE
010900011204     D*-------------------
011000011204     D BS69DS        E DS                  EXTNAME(TIBS69DS)
011100011204     D ACODS         E DS                  EXTNAME(CNACO00F)
011200011204     D INDDS         E DS                  EXTNAME(CNIND00F)
011300011204     D CLPDS         E DS                  EXTNAME(CNCLP00F)
011400011204     D CLSDS         E DS                  EXTNAME(FNCLS00F)
011500030422     D*---------------------
011600030422     D* DS x campo filler evtxxx
011700030422     D*---------------------
011800030422     D devtxxx       E DS
011900000000     C*------------------------------------------------------------------------*
012000000000     C* MAIN LINE
012100000000     C*------------------------------------------------------------------------*
012200000000     C                   MOVEL     *BLANKS       COMMAN
012300000209     C   50              MOVEA     CMD(1)        COMMAN
012400000209     C  N50              MOVEA     CMD(2)        COMMAN
012500000000     C                   Z-ADD     80            LUNG
012600000000     C                   CALL      'QCMDEXC'                            99
012700000000     C                   PARM                    COMMAN           80
012800000000     C                   PARM                    LUNG             15 5
012900000000     C*
013000000000     C* APRO IL FILE DI STAMPA ED EMETTO L'INTESTAZIONE
013100000209     C   50              OPEN      TREC83P
013200000209     C  N50              OPEN      TREC82P
013300000209     C*
013400000209     C   50              WRITE     EC83T1
013500000209     C  N50              WRITE     EC82T1
013600000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
013700000210     C*
013800000210     C* RIAPRO IL FILE DI INPUT
013900000210     C                   OPEN      ECEVT01L
014000000210     C*
014100000000     C* LEGGO TUTTI I RECORD DAL FILE TESTATE COMPETENZE
014200000208     C                   Z-ADD     PA1CMP        $VTCMP
014300000000     C                   Z-ADD     PA1VOC        $VTVOC
014400000000     C                   Z-ADD     1             $VTPRG
014500000208     C     PA1COV        IFEQ      'C'
014600000208     C     KEYEV2        SETLL     ECEVT02L
014700000210     C                   READ      ECEVT02L                               20
014800000210     C  N20              Z-ADD     EVTCMP        SAVKEY
014900000208     C                   ELSE
015000000208     C     KEYEVT        SETLL     ECEVT01L
015100000208     C                   READ      ECEVT01L                               20
015200000210     C                   ENDIF
015300000000DO  1C     *IN20         DOWEQ     *OFF
015400000000     C*
015500000000     C* RIEMPO DEI CAMPI DI COMODO PER RIPOSIZIONARMI SUCCESSIVAMENTE
015600000208     C                   Z-ADD     EVTCMP        $VTCMP
015700000000     C                   Z-ADD     EVTVOC        $VTVOC
015800000000     C                   Z-ADD     EVTPRG        $VTPRG
015900000214     C*
016000000214     C* ESEGUO ROUTINE DI CONTROLLO RECORD
016100000000     C*
016200000214     C                   EXSR      CHKREC
016300000214     C*
016400000214 IF 2C     *IN55         IFEQ      *OFF
016500000000     C*
016600000209     C* SE LANCIATA X DETTAGLIO ESEGUO LA ROUTINE SOTTOSTANTE
016700000215 IF 3C     *IN50         IFEQ      *ON
016800000211     C                   EXSR      DETTAG
016900000209     C* ALTRIMENTI QUEST' ALTRA (SINTETICA)
017000000211     C                   ELSE
017100000211     C* SE LANCIO E STATO SINTETICO E ORD. X COMPETENZE
017200000211     C* ESEGUO CONSIDERAZIONI SULLE VOCI "IN MAGGIORAZIONE"
017300000211     C* AL CAMBIARE DELLA COMPETENZA
017400000215     C     PA1COV        IFEQ      'C'
017500000215     C     EVTCMP        ANDNE     *ZEROS
017600000215     C     EVTCMP        ANDNE     SAVCMP
017700000215     C                   EXSR      GESMAG
017800000215     C                   Z-ADD     EVTCMP        SAVCMP
017900000215     C                   Z-ADD     *ZEROS        PASSA             1 0
018000000215     C                   ENDIF
018100000215     C                   EXSR      SINTAG
018200000209     C*
018300000000E   3C                   ENDIF
018400000215     C                   ELSE
018500000215 IF 3C     *IN50         IFEQ      *OFF
018600000215     C* SE LANCIO E STATO SINTETICO E ORD. X COMPETENZE
018700000215     C* ESEGUO CONSIDERAZIONI SULLE VOCI "IN MAGGIORAZIONE"
018800000215     C* AL CAMBIARE DELLA COMPETENZA
018900000215 IF 2C     PA1COV        IFEQ      'C'
019000000215     C     EVTCMP        ANDNE     *ZEROS
019100000215     C     EVTCMP        ANDNE     SAVCMP
019200000215     C     PASSA         ANDEQ     *ZEROS
019300000215     C                   EXSR      GESMAG
019400000215     C                   Z-ADD     EVTCMP        SAVCMP
019500000215     C                   Z-ADD     1             PASSA
019600000215     C                   ENDIF
019700000000E   2C                   ENDIF
019800000215E   3C                   ENDIF
019900000000     C*
020000000209     C* DEVO, TUTTE LE VOLTE, RIPOSIZIONARMI POICHE' HO FATTO ALTRE CHAIN SU EVT
020100000208     C     PA1COV        IFEQ      'C'
020200000208     C     KEYEV2        SETGT     ECEVT02L
020300000208     C                   READ      ECEVT02L                               20
020400000208     C                   ELSE
020500000208     C     KEYEVT        SETGT     ECEVT01L
020600000208     C                   READ      ECEVT01L                               20
020700000208     C                   ENDIF
020800000000E   1C                   ENDDO
020900000000     C*
021000000000     C* VERIFICO SE HO STAMPATO QUALCOSA
021100000000     C     $ALMEN        IFGT      *ZERO
021200000209     C   50              WRITE     EC83E1
021300000209     C  N50              WRITE     EC82E1
021400000000     C                   ELSE
021500000209     C   50              WRITE     EC83E2
021600000209     C  N50              WRITE     EC82E2
021700000000     C                   ENDIF
021800000209     C   50              CLOSE     TREC83P
021900000209     C  N50              CLOSE     TREC82P
022000000000     C*
022100000210     C                   CLOSE     ECEVT01L
022200010808     C                   CLOSE     ECEVT0XL
022300000210     C*
022400000000     C* FINE PGM
022500000000     C                   SETON                                        LR
022600000214     C*------------------------------------------------------------------------*
022700000214     C* CHKREC - DETERMINAZIONE VALIDITA' RECORD
022800000214     C*------------------------------------------------------------------------*
022900000214     C     CHKREC        BEGSR
023000000214     C*
023100000214     C                   SETON                                        55
023200000214     C*
023300000214     C* VERIFICO I LIMITI
023400000214     C     EVTVOC        IFGE      PA1VOC
023500000214     C     EVTVOC        ANDLE     PA2VOC
023600000214     C     EVTDDV        ANDLE     PA1DDV
023700000214     C     EVTDSV        ANDGE     PA2DDV
023800000214     C     EVTATB        ANDEQ     *BLANKS
023900000214     C*
024000000214     C     EVTTVO        IFEQ      'M'
024100000214     C                   Z-ADD     EVTVOC        KVMVOC
024200000214     C                   Z-ADD     EVTPRG        KVMPRG
024300000214     C     KEYEVM        SETLL     ECEVM01L
024400000214     C     KEYEVM        READE     ECEVM01L                               21
024500000214     C     *IN21         DOWEQ     *OFF
024600000214     C*
024700000214     C     EVMATB        IFEQ      *BLANKS
024800010808     C     EVMVOC        SETLL     ECEVT0XL                           98
024900010808     C     EVMVOC        READE     ECEVT0XL                               98
025000000214     C     *IN98         DOWEQ     *OFF
025100000215     C     *IN55         ANDEQ     *ON
025200000214     C*
025300000215     C     w_EVTATB      IFEQ      *blanks
025400000215     C     w_EVTDDV      ANDLE     PA1DDV
025500000215     C     w_EVTDSV      ANDGE     PA2DDV
025600000215     C     w_EVTCMP      ANDGE     PA1CMP
025700000215     C     w_EVTCMP      ANDLE     PA2CMP
025800000215     C                   SETOFF                                       55
025900000214     C                   ELSE
026000010808     C     EVMVOC        READE     ECEVT0XL                               98
026100000214     C                   ENDIF
026200000214     C*
026300000214     C                   ENDDO
026400000215     C                   ENDIF
026500000214     C*
026600000214     C* PROCEDO LEGGENDO PROSSIVA VOCE MAGGIORATA DA VOCE MAGGIORANTE
026700000214     C     KEYEVM        READE     ECEVM01L                               21
026800000214     C                   ENDDO
026900000214     C*
027000000214     C                   ELSE
027100000214     C     EVTCMP        IFGE      PA1CMP
027200000214     C     EVTCMP        ANDLE     PA2CMP
027300000214     C                   SETOFF                                       55
027400000214     C                   ENDIF
027500000214     C                   ENDIF
027600000214     C*
027700000214     C                   ENDIF
027800000214     C*
027900000214     C                   ENDSR
028000000209     C*------------------------------------------------------------------------*
028100000209     C* VALTES - VALORIZZAZIONE CAMPI DI TESTATA
028200000209     C*------------------------------------------------------------------------*
028300000209     C     VALTES        BEGSR
028400000209     C*
028500000209     C* DATA
028600000209     C     PA2DDV        IFGT      *ZEROS
028700000209     C                   Z-ADD     SAVDAT        STPDAT
028800000209     C                   ELSE
028900000209     C                   Z-ADD     *ZEROS        STPDAT
029000000209     C                   ENDIF
029100000209     C*
029200000209     C* COMPETENZE
029300000209     C                   Z-ADD     PA1CMP        STPCMPDA
029400000209     C                   Z-ADD     PA2CMP        STPCMPAL
029500000209     C*
029600000209     C* VOCI
029700000209     C                   Z-ADD     PA1VOC        STPVOCDA
029800000209     C                   Z-ADD     PA2VOC        STPVOCAL
029900000209     C*
030000000209     C* TARIFFE
030100000209     C     PA1SON        IFEQ      'S'
030200000209     C                   MOVEL     'Incluse'     STPTAR
030300000209     C                   ELSE
030400000209     C                   MOVEL     'Escluse'     STPTAR
030500000209     C                   ENDIF
030600000209     C*
030700000209     C* TIPO DI STAMPA
030800000209     C     PA1SOD        IFEQ      'S'
030900000209     C                   MOVEL     'Sintetica'   STPSID
031000000209     C                   ELSE
031100000209     C                   MOVEL     'Dettaglio'   STPSID
031200000209     C                   ENDIF
031300000209     C*
031400000209     C* ORDINAMENTO
031500000209     C     PA1COV        IFEQ      'V'
031600000209     C                   MOVEL     'Voci'        STPORD
031700000209     C                   ELSE
031800000209     C                   MOVEL     'Competenze'  STPORD
031900000209     C                   ENDIF
032000000209     C*
032100000209     C                   ENDSR
032200000000     C*------------------------------------------------------------------------*
032300000000     C* DETTAG - ESAMINA IL DETTAGLIO COMPETENZE (FILE ECECD00F)
032400000000     C*------------------------------------------------------------------------*
032500000000     C     DETTAG        BEGSR
032600000000     C*
032700000000     C* RIEMPO I CAMPI DI STAMPA
032800000000     C                   MOVEL     EVTATB        P1CATB
032900000000     C                   Z-ADD     EVTVOC        P1CVOC
033000000000     C                   Z-ADD     EVTPRG        P1CPRG
033100000000     C                   MOVEL     EVTVOD        P1CVOD
033200000000     C                   MOVEL     EVTTVO        P1CTVO
033300000000     C                   MOVEL     EVTVRV        P1CVRV
033400021206     C                   MOVEL     EVTCOP        P1CCOP
033500021206     C                   MOVEL     EVTDAT        P1CDAT
033600021209     C                   MOVEL     EVTAID        P1CAID
033700000000     C                   MOVEL     '2R'          TBLCOD
033800000000     C                   MOVEL     EVTVRV        TBLKEY
033900000000     C     KEYTAB        CHAIN     TABEL00F                           99
034000000000     C  N99              MOVEL     TBLUNI        P1DVRV
034100000000     C   99              MOVEL     *BLANKS       P1DVRV
034200000000     C                   Z-ADD     EVTDDV        G08INV
034300000000     C                   Z-ADD     *ZEROS        G08DAT
034400000000     C                   MOVEL     '3'           G08ERR
034500000000     C                   CALL      'XSRDA8'
034600000224     C                   PARM                    WLBDA8
034700000000     C                   Z-ADD     G08DAT        P1CDDV
034800000000     C                   Z-ADD     EVTDSV        G08INV
034900000000     C                   Z-ADD     *ZEROS        G08DAT
035000000000     C                   MOVEL     '3'           G08ERR
035100000000     C                   CALL      'XSRDA8'
035200000224     C                   PARM                    WLBDA8
035300000000     C                   Z-ADD     G08DAT        P1CDSV
035400000000     C                   MOVEL     EVTTIV        P1CTIV
035500000000     C                   MOVEL     '2R'          TBLCOD
035600000000     C                   MOVEL     EVTTIV        TBLKEY
035700000000     C     KEYTAB        CHAIN     TABEL00F                           99
035800000000     C  N99              MOVEL     TBLUNI        P1DTIV
035900000000     C   99              MOVEL     *BLANKS       P1DTIV
036000000000     C                   MOVEL     EVTSPT        P1CSPT
036100030422     c                   if        evttiv = '05'
036200030422     c                   movel(p)  evtxxx        devtxxx
036300030422     c                   else
036400030422     c                   clear                   devtxxx
036500030422     c                   end
036600030422     C                   movel     evtsps        p1csps
036700030422     C                   z-add     evtrpv        p1Crpv
036800030422     C                   Z-ADD     EVTCMP        P1CCMP
036900000214     C                   Z-ADD     EVTCMP        $CTCMP
037000000214     C                   Z-ADD     SAVNBC        $CTNBC
037100040128     C*                  Z-ADD     EVTDDV        $CTDDC
037200040128     C                   Z-ADD     EVTDsv        $CTDDC
037300000214     C     KEYECT        SETGT     ECECT02L
037400000214     C                   READP     ECECT02L                               99
037500000214     C     *IN99         IFEQ      *OFF
037600000214     C     ECTCMP        ANDEQ     $CTCMP
037700000214     C     ECTNBC        ANDEQ     $CTNBC
037800000214     C                   MOVEL     ECTCMD        P1CCMD
037900000214     C                   ELSE
038000000214     C                   SETON                                        99
038100000214     C                   ENDIF
038200000214     C   99              MOVEL     *BLANKS       P1CCMD
038300000000     C*
038400000000     C* RIFERIMENTI TARIFFA
038500000000     C                   MOVEL     *BLANKS       P1DRFI
038600000000     C                   MOVEL     EVTRFI        P1CRFI
038700000000IF  1C     EVTRFI        IFNE      *BLANKS
038800000000     C                   MOVEL     '2Q'          TBLCOD
038900000000     C                   MOVEL     EVTRFI        TBLKEY
039000000000     C     KEYTAB        CHAIN     TABEL00F                           99
039100000000     C  N99              MOVEL     TBLUNI        P1DRFI
039200000000E   1C                   ENDIF
039300000000     C                   MOVEL     *BLANKS       P1DRFE
039400000000     C                   MOVEL     EVTRFE        P1CRFE
039500000000IF  1C     EVTRFE        IFNE      *BLANKS
039600000000     C                   MOVEL     '2Q'          TBLCOD
039700000000     C                   MOVEL     EVTRFE        TBLKEY
039800000000     C     KEYTAB        CHAIN     TABEL00F                           99
039900000000     C  N99              MOVEL     TBLUNI        P1DRFE
040000000000E   1C                   ENDIF
040100000000     C*
040200000000     C                   MOVEL     *BLANKS       P1DRTI
040300000000     C                   MOVEL     EVTRTI        P1CRTI
040400000000IF  1C     EVTRTI        IFNE      *BLANKS
040500000000     C                   MOVEL     '2Q'          TBLCOD
040600000000     C                   MOVEL     EVTRTI        TBLKEY
040700000000     C     KEYTAB        CHAIN     TABEL00F                           99
040800000000     C  N99              MOVEL     TBLUNI        P1DRTI
040900000000E   1C                   ENDIF
041000000000     C                   MOVEL     *BLANKS       P1DRTE
041100000000     C                   MOVEL     EVTRTE        P1CRTE
041200000000IF  1C     EVTRTE        IFNE      *BLANKS
041300000000     C                   MOVEL     '2Q'          TBLCOD
041400000000     C                   MOVEL     EVTRTE        TBLKEY
041500000000     C     KEYTAB        CHAIN     TABEL00F                           99
041600000000     C  N99              MOVEL     TBLUNI        P1DRTE
041700000000E   1C                   ENDIF
041800000000     C*
041900000000     C                   MOVEL     *BLANKS       P1DRPI
042000000000     C                   MOVEL     EVTRPI        P1CRPI
042100000000IF  1C     EVTRPI        IFNE      *BLANKS
042200000000     C                   MOVEL     '2Q'          TBLCOD
042300000000     C                   MOVEL     EVTRPI        TBLKEY
042400000000     C     KEYTAB        CHAIN     TABEL00F                           99
042500000000     C  N99              MOVEL     TBLUNI        P1DRPI
042600000000E   1C                   ENDIF
042700000000     C                   MOVEL     *BLANKS       P1DRPE
042800000000     C                   MOVEL     EVTRPE        P1CRPE
042900000000IF  1C     EVTRPE        IFNE      *BLANKS
043000000000     C                   MOVEL     '2Q'          TBLCOD
043100000000     C                   MOVEL     EVTRPE        TBLKEY
043200000000     C     KEYTAB        CHAIN     TABEL00F                           99
043300000000     C  N99              MOVEL     TBLUNI        P1DRPE
043400000000E   1C                   ENDIF
043500000000     C*
043600000000     C                   MOVEL     *BLANKS       P1DRKI
043700000000     C                   MOVEL     EVTRKI        P1CRKI
043800000000IF  1C     EVTRKI        IFNE      *BLANKS
043900000000     C                   MOVEL     '2Q'          TBLCOD
044000000000     C                   MOVEL     EVTRKI        TBLKEY
044100000000     C     KEYTAB        CHAIN     TABEL00F                           99
044200000000     C  N99              MOVEL     TBLUNI        P1DRKI
044300000000E   1C                   ENDIF
044400000000     C                   MOVEL     *BLANKS       P1DRKE
044500000000     C                   MOVEL     EVTRKE        P1CRKE
044600000000IF  1C     EVTRKE        IFNE      *BLANKS
044700000000     C                   MOVEL     '2Q'          TBLCOD
044800000000     C                   MOVEL     EVTRKE        TBLKEY
044900000000     C     KEYTAB        CHAIN     TABEL00F                           99
045000000000     C  N99              MOVEL     TBLUNI        P1DRKE
045100000000E   1C                   ENDIF
045200000000     C*
045300000000     C                   Z-ADD     EVTARL        P1CARL
045400000000     C                   Z-ADD     EVTARF        P1CARF
045500000000     C                   Z-ADD     EVTARO        P1CARO
045600000000     C*
045700000209     C   30              WRITE     EC83T1
045800000209     C   30              SETOFF                                       30
045900000209     C                   WRITE     EC83D1
046000000000     C*
046100000000     C* STAMPO I RIFERIMENTI SOLO SE CE N'E' ALMENO UNO
046200000000IF  1C     EVTRFI        IFNE      *BLANKS
046300000000     C     EVTRFE        ORNE      *BLANKS
046400000000     C     EVTRTI        ORNE      *BLANKS
046500000000     C     EVTRTE        ORNE      *BLANKS
046600000000     C     EVTRPI        ORNE      *BLANKS
046700000000     C     EVTRPE        ORNE      *BLANKS
046800000000     C     EVTRKI        ORNE      *BLANKS
046900000000     C     EVTRKE        ORNE      *BLANKS
047000000000     C   30              WRITE     EC83T1
047100000000     C   30              SETOFF                                       30
047200000000     C                   WRITE     EC83D9
047300000000E   1C                   ENDIF
047400000000     C*
047500000000     C                   Z-ADD     1             $ALMEN
047600000000     C*
047700000000     C* DEVO EMETTERE SOLO I RECORD DI ECEVD CHE HANNO:
047800000000     C* ..  STESSO COD.VOCE DI ECEVT
047900000000     C* ..  PERIODO DI VALIDITA' COMPRESO (O UGUALE) ALLA VALIDITA' SU ECEVT
048000000000     C*
048100000000     C                   Z-ADD     *ZERO         $TEST2            1 0
048200000000     C                   Z-ADD     EVTVOC        KVDVOC
048300000000     C                   Z-ADD     EVTPRG        KVDPRG
048400131203     C     KEYEVD        SETLL     ECEVD06L
048500131203     C     KEYEVD        READE     ECEVD06L                               21
048600000000     C     *IN21         DOWEQ     *OFF
048700000209     C*
048800000209     C* VERIFICO IN BASE ALLE RICHIESTE DI LANCIO IL PERIODO VALIDITA'
048900000209     C                   MOVEL     '0'           TAROK             1
049000000209     C*
049100000209     C     PA1SON        IFEQ      'S'
049200000209     C     EVDDDT        IFGE      EVTDDV
049300000209     C     EVDDST        ANDLE     EVTDSV
049400000209     C     EVDDST        ANDGE     PA2DDV
049500000209     C                   MOVEL     '1'           TAROK
049600000209     C                   ENDIF
049700000209     C                   ELSE
049800000209     C     EVDDDT        IFLE      PA1DDV
049900000209     C     EVDDST        ANDGE     PA2DDV
050000000209     C                   MOVEL     '1'           TAROK
050100000209     C                   ENDIF
050200000209     C                   ENDIF
050300000000     C*
050400000209     C* SE DATE VALIDITA' => OK
050500000209     C     TAROK         IFEQ      '1'
050600940727     C     EVDATB        ANDEQ     *BLANKS
050700000000     C                   EXSR      STAMP1
050800000000     C                   ENDIF
050900000000     C*
051000131203     C     KEYEVD        READE     ECEVD06L                               21
051100000000     C                   ENDDO
051200000000     C*
051300000000     C                   Z-ADD     *ZERO         $TEST3            1 0
051400000000     C*
051500000000     C* QUINDI E' LA VOLTA DELLE MAGGIORAZIONI
051600000000     C                   Z-ADD     *ZERO         $M
051700000000     C                   MOVEA     *BLANKS       MTB
051800000000     C                   Z-ADD     *ZERO         MOC
051900000000     C                   MOVEA     *BLANKS       MOD
052000000000     C                   Z-ADD     $VTVOC        KVMVOC
052100000000     C                   Z-ADD     $VTPRG        KVMPRG
052200000000     C     KEYEVM        SETLL     ECEVM01L
052300000000     C     KEYEVM        READE     ECEVM01L                               21
052400000000     C     *IN21         DOWEQ     *OFF
052500940727     C*
052600940727     C     EVMATB        IFEQ      *BLANKS
052700000000     C                   EXSR      STAMP2
052800940727     C                   ENDIF
052900940727     C*
053000000000     C     KEYEVM        READE     ECEVM01L                               21
053100000000     C                   ENDDO
053200000000     C*
053300000000     C* STAMPO LA RIGA CON LE ULTIME MAGGIORAZIONI, SE CI SONO
053400000000     C     $M            IFGT      *ZERO
053500000000     C                   WRITE     EC83D3
053600000000     C                   ENDIF
053700000000     C*
053800000000     C                   ENDSR
053900000000     C*------------------------------------------------------------------------*
054000000000     C* STAMP1 - STAMPA DEL DETTAGLIO (TARIFFE)
054100000000     C*------------------------------------------------------------------------*
054200000000     C     STAMP1        BEGSR
054300000000     C*
054400000000     C* TESTO L'OVERFLOW
054500000000     C     *IN30         IFEQ      *ON
054600000000     C                   WRITE     EC83T1
054700000000     C                   WRITE     EC83T2
054800000000     C                   SETOFF                                       30
054900940727     C                   Z-ADD     1             $TEST2
055000000000     C                   ENDIF
055100000000     C*
055200000000     C* RIEMPO I CAMPI DI STAMPA
055300000000     C                   MOVEL     EVDATB        P2CATB
055400000000     C                   MOVEL     EVDTTV        P2CTTV
055500000000     C                   MOVEL     '2S'          TBLCOD
055600000000     C                   MOVEL     *BLANKS       TBLKEY
055700000000     C                   MOVEL     EVDTTV        TBLKEY
055800000000     C     KEYTAB        CHAIN     TABEL00F                           99
055900000000     C  N99              MOVEL     TBLUNI        P2DTTV
056000000000     C   99              MOVEL     *BLANKS       P2DTTV
056100000000     C                   SETOFF                                       313233
056200000000     C                   SETOFF                                       34
056300000000     C     EVDTTV        IFEQ      'F'
056400000000     C                   SETON                                        31
056500000000     C                   Z-ADD     EVDFT1        P2CFT1
056600000000     C     EVDFT1        CHAIN     AZORG01L                           99
056700000000     C  N99              MOVEL     ORGDES        P2DFT1
056800000000     C   99              MOVEL     *BLANKS       P2DFT1
056900000000     C                   Z-ADD     EVDFT2        P2CFT2
057000000000     C     EVDFT2        CHAIN     AZORG01L                           99
057100000000     C  N99              MOVEL     ORGDES        P2DFT2
057200000000     C   99              MOVEL     *BLANKS       P2DFT2
057300000000     C                   ENDIF
057400000000     C     EVDTTV        IFEQ      'T'
057500000000     C                   SETON                                        32
057600000000     C                   MOVEL     EVDCTS        P2CCTS
057700000000     C                   MOVEL     'CT'          TBLCOD
057800000000     C                   MOVEL     EVDCTS        TBLKEY
057900000000     C     KEYTAB        CHAIN     TABEL00F                           99
058000000000     C  N99              MOVEL     TBLUNI        P2DCTS
058100000000     C   99              MOVEL     *BLANKS       P2DCTS
058200000000     C                   ENDIF
058300000000     C     EVDTTV        IFEQ      'P'
058400000000     C                   SETON                                        33
058500131203     C                   movel     EVDnaz        P2Cnaz
058600131203     C                   movel     EVDCAd        P2CCAd
058700000000     C                   ENDIF
058800000000     C     EVDTTV        IFEQ      'K'
058900000000     C                   SETON                                        34
059000000000     C                   Z-ADD     EVDCLI        P2CCLI
059100011204     C                   MOVEL     *BLANKS       P2DCLI
059200011204     C* RECUPERA DESCRIZIONE CLIENTE
059300011204     C                   CLEAR                   BS69DS
059400011204     C                   CLEAR                   ACODS
059500011204     C                   MOVEL     KNSIF         I69SIF
059600011204     C                   MOVEL     EVDCLI        I69KAC
059700011204     C                   CALL      'TIBS69R'
059800011204     C                   PARM                    BS69DS
059900011204     C                   PARM                    ACODS
060000011204     C                   PARM                    INDDS
060100011204     C                   PARM                    CLPDS
060200011204     C                   PARM                    CLSDS
060300011204     C                   MOVEL     ACORAG        P2DCLI
060400000000     C                   Z-ADD     EVDCTR        P2CCTR
060500941129     C                   MOVEL     *BLANKS       P2DCTR
060600941129     C     KEYTAM        SETLL     TNTAM01L
060700941129     C     KEYTAM        READE     TNTAM01L                               21
060800941129     C     *IN21         DOWEQ     *OFF
060900941129     C     TAMDDT        IFGE      EVDDDT
061000941129     C     TAMDST        ANDLE     EVDDST
061100941129     C  N99              MOVEL     TAMDCV        P2DCTR
061200941129     C                   ENDIF
061300941129     C     KEYTAM        READE     TNTAM01L                               21
061400941129     C                   ENDDO
061500000000     C                   ENDIF
061600000000     C*
061700000000     C                   Z-ADD     EVDDDT        G08INV
061800000000     C                   Z-ADD     *ZEROS        G08DAT
061900000000     C                   MOVEL     '3'           G08ERR
062000000000     C                   CALL      'XSRDA8'
062100000224     C                   PARM                    WLBDA8
062200000000     C                   Z-ADD     G08DAT        P2CDDT
062300000000     C                   Z-ADD     EVDDST        G08INV
062400000000     C                   Z-ADD     *ZEROS        G08DAT
062500000000     C                   MOVEL     '3'           G08ERR
062600000000     C                   CALL      'XSRDA8'
062700000224     C                   PARM                    WLBDA8
062800000000     C                   Z-ADD     G08DAT        P2CDST
062900000000     C                   Z-ADD     EVDSGI        P2CSGI
063000000000     C                   Z-ADD     EVDSGF        P2CSGF
063100000000     C                   Z-ADD     EVDITR        P2CITR
063200000000     C                   Z-ADD     EVDIMI        P2CIMI
063300000000     C                   Z-ADD     EVDIMA        P2CIMA
063400000000     C                   Z-ADD     EVDPER        P2CPER
063500000000     C                   MOVEL     EVDSEG        P2CSEG
063600000000     C                   Z-ADD     EVDRPV        P2CRPV
063700000000     C*
063800000000     C* SE NON HO ANCORA STAMPATO LA TESTATA, LO FACCIO ORA
063900000000     C     $TEST2        IFEQ      *ZERO
064000000000     C                   WRITE     EC83T2
064100000000     C                   Z-ADD     1             $TEST2
064200000000     C                   ENDIF
064300000000     C*
064400000000     C                   WRITE     EC83D2
064500000000     C*
064600000000     C                   ENDSR
064700000000     C*------------------------------------------------------------------------*
064800000000     C* STAMP2 - STAMPA DEL DETTAGLIO (MAGGIORAZIONI)
064900000000     C*------------------------------------------------------------------------*
065000000000     C     STAMP2        BEGSR
065100000000     C*
065200000000     C* TESTO L'OVERFLOW
065300000000     C     *IN30         IFEQ      *ON
065400000000     C                   WRITE     EC83T1
065500000000     C                   WRITE     EC83T3
065600000000     C                   SETOFF                                       30
065700000000     C                   ENDIF
065800000000     C*
065900000000     C* SE NON HO ANCORA STAMPATO LA TESTATA, LO FACCIO ORA
066000000000     C     $TEST3        IFEQ      *ZERO
066100000000     C                   WRITE     EC83T3
066200000000     C                   Z-ADD     1             $TEST3
066300000000     C                   ENDIF
066400000000     C*
066500000000     C* NON STAMPO MA AGGIUNGO NELLE SCHIERE POICHE' NE HO 4 PER RIGA
066600000000     C                   ADD       1             $M
066700000000     C                   MOVEL     EVMATB        MTB($M)
066800000000     C                   Z-ADD     EVMVOC        MOC($M)
066900000000     C     EVMVOC        CHAIN     ECEVT01L                           99
067000000000     C  N99              MOVEL     EVTVOD        MOD($M)
067100000000     C   99              MOVEL     *BLANKS       MOD($M)
067200000000     C*
067300000000     C* STAMPO LA RIGA SOLO SE HO RAGGIUNTO 4 MAGGIORAZIONI
067400000000     C     $M            IFEQ      4
067500000000     C                   WRITE     EC83D3
067600000000     C                   Z-ADD     *ZERO         $M                1 0
067700000000     C                   MOVEA     *BLANKS       MTB
067800000000     C                   Z-ADD     *ZERO         MOC
067900000000     C                   MOVEA     *BLANKS       MOD
068000000000     C                   ENDIF
068100000000     C*
068200000000     C                   ENDSR
068300000209     C*------------------------------------------------------------------------*
068400000209     C* SINTAG - ESAMINA IL SINTETICO VOCI
068500000209     C*------------------------------------------------------------------------*
068600000209     C     SINTAG        BEGSR
068700000209     C*
068800000211     C* "PULISCO" AD OGNI PASSAGGIO IL FORMATO RECORD DI STAMPA 1°
068900000211     C                   CLEAR                   EC82D2
069000000211     C*
069100000211     C     PA1COV        IFEQ      'C'
069200000211     C     EVTTVO        ANDEQ     'M'
069300000211     C*
069400000211     C                   ELSE
069500000211     C*
069600000209     C* RIEMPIO BUFFER X FORMATO RECORD VOCE 'V'
069700000209     C                   Z-ADD     EVTVOC        P1CVOC
069800000209     C                   Z-ADD     EVTPRG        P1CPRG
069900000209     C                   MOVEL     EVTVOD        P1CVOD
070000000209     C                   MOVEL     EVTTVO        P1CTVO
070100021206     C                   MOVEL     EVTCOP        P1CCOP
070200021209     C                   MOVEL     EVTDAT        P1CDAT
070300040130     c* decodifico il tipo data voce
070400040130     c                   movel     evtdat        tbeke1
070500040130     c                   exsr      srco1
070600040130     c                   movel     §co1des       p1cvdt
070700021209     C                   MOVEL     EVTAID        P1CAID
070800000209     C* COSTRUZIONE CHIAVE PER REPERIMENTO COMPETENZA
070900000209     C                   Z-ADD     EVTCMP        $CTCMP
071000000209     C                   Z-ADD     SAVNBC        $CTNBC
071100040128     C*                  Z-ADD     EVTDDV        $CTDDC
071200040128     C                   Z-ADD     EVTDSV        $CTDDC
071300000214     C     KEYECT        SETGT     ECECT02L
071400000214     C                   READP     ECECT02L                               88
071500000209     C     *IN88         IFEQ      *OFF
071600000214     C     ECTCMP        ANDEQ     $CTCMP
071700000214     C     ECTNBC        ANDEQ     $CTNBC
071800000210     C     ECTATB        ANDEQ     *blanks
071900000210     C     ECTDDC        ANDLE     PA1DDV
072000000210     C     ECTDSC        ANDGE     PA2DDV
072100000209     C                   Z-ADD     ECTCMP        P1CCMP
072200000209     C                   MOVEL     ECTCMD        P1CCMD
072300040130     c                   movel     ectfdr        tbeke1
072400040130     c                   exsr      srco1
072500040130     c                   movel     §co1des       p1ccdt
072600040130     C                   Z-ADD     Ectddc        G08INV
072700040130     C                   Z-ADD     *ZEROS        G08DAT
072800040130     C                   MOVEL     '3'           G08ERR
072900040130     C                   CALL      'XSRDA8'
073000040130     C                   PARM                    WLBDA8
073100040130     C                   move      G08DAT        P1cddc
073200040130     C                   Z-ADD     Ectdsc        G08INV
073300040130     C                   Z-ADD     *ZEROS        G08DAT
073400040130     C                   MOVEL     '3'           G08ERR
073500040130     C                   CALL      'XSRDA8'
073600040130     C                   PARM                    WLBDA8
073700040130     C                   move      G08DAT        P1cdsc
073800000210     C                   ELSE
073900040130     C                   clear                   P1cdsc
074000040130     C                   clear                   P1cddc
074100040130     c                   clear                   p1ccdt
074200040130     C                   z-add     0             P1CCMP
074300000211     C                   MOVEL     *blanks       P1CCMD
074400000211     C                   ENDIF
074500000210     C*
074600000209     C   31              WRITE     EC82T1
074700000209     C   31              SETOFF                                       31
074800000210     C     PA1COV        IFEQ      'C'
074900000210     C     $VTCMP        ANDNE     SAVKEY
075000000209     C                   WRITE     EC82RB
075100000210     C                   Z-ADD     $VTCMP        SAVKEY
075200000210     C                   ENDIF
075300000214     C                   WRITE     EC82D2
075400000214     C                   Z-ADD     1             $ALMEN
075500000210     C*
075600000210     C*-------------------------------------------------------------------*
075700000211     C*
075800000211     C* "PULISCO" AD OGNI PASSAGGIO IL FORMATO RECORD DI STAMPA 2°
075900000211     C                   CLEAR                   EC82D3
076000000211     C*
076100000209     C* VERIFICO IL TIPO DI VOCE "CORRENTE"
076200000209     C* SE VOCE DI MAGGIORAZIONE ESEGUO CONSIDERAZIONE E STAMPA PARTICOLARI
076300000209     C     EVTTVO        IFEQ      'M'
076400000209     C                   Z-ADD     EVTVOC        KVMVOC
076500000209     C                   Z-ADD     EVTPRG        KVMPRG
076600000209     C     KEYEVM        SETLL     ECEVM01L
076700000209     C     KEYEVM        READE     ECEVM01L                               21
076800000209     C     *IN21         DOWEQ     *OFF
076900000209     C*
077000000209     C* RIEMPIO BUFFER FORMATO RECORD X MAGGIORAZIONI
077100000215     C     EVMATB        IFEQ      *BLANKS
077200000209     C                   Z-ADD     EVMVOC        P1CVOC
077300000209     C* REPERISCO I DATI "VOCE" DELLA VOCE DA MAGGIORARE
077400000209     C                   Z-ADD     EVMVOC        $VTVOC3
077500010808     C     EVMVOC        SETLL     ECEVT0XL                           98
077600010808     C     EVMVOC        READE     ECEVT0XL                               98
077700000214     C                   MOVEL     '0'           CHKVOC            1
077800000214     C     *IN98         DOWEQ     *OFF
077900000214     C     CHKVOC        ANDEQ     '0'
078000000214     C*
078100000215     C     w_EVTDDV      IFLE      PA1DDV
078200000215     C     w_EVTDSV      ANDGE     PA2DDV
078300000214     C                   MOVEL     '1'           CHKVOC
078400000214     C                   ELSE
078500010808     C     EVMVOC        READE     ECEVT0XL                               98
078600000214     C                   ENDIF
078700000214     C*
078800000214     C                   ENDDO
078900000214     C*
079000000214     C     CHKVOC        IFEQ      '1'
079100000215     C     w_EVTATB      IFEQ      *BLANKS
079200000215     C**   w_EVTDDV      ANDLE     PA1DDV
079300000215     C**   w_EVTDSV      ANDGE     PA2DDV
079400000215     C                   MOVEL     w_EVTVOD      P1CVOD
079500000210     C* E REPERISCO I DATI "COMPETENZA" DELLA VOCE DA MAGGIORARE
079600000215     C                   Z-ADD     w_EVTCMP      $CTCMP
079700000210     C                   Z-ADD     SAVNBC        $CTNBC
079800040128     C*                  Z-ADD     w_EVTDDV      $CTDDC
079900040128     C                   Z-ADD     w_EVTDSV      $CTDDC
080000000214     C     KEYECT        SETGT     ECECT02L
080100000214     C                   READP     ECECT02L                               88
080200000211     C     *IN88         IFEQ      *OFF
080300000214     C     ECTCMP        IFEQ      $CTCMP
080400000214     C     ECTNBC        ANDEQ     $CTNBC
080500000214     C     ECTATB        ANDEQ     *blanks
080600000210     C     ECTDDC        ANDLE     PA1DDV
080700000210     C     ECTDSC        ANDGE     PA2DDV
080800000210     C                   Z-ADD     ECTCMP        P1CCMP
080900000210     C                   MOVEL     ECTCMD        P1CCMD
081000040130     c                   movel     ectfdr        tbeke1
081100040130     c                   exsr      srco1
081200040130     c                   movel     §co1des       p1ccdt
081300040130     C                   Z-ADD     Ectddc        G08INV
081400040130     C                   Z-ADD     *ZEROS        G08DAT
081500040130     C                   MOVEL     '3'           G08ERR
081600040130     C                   CALL      'XSRDA8'
081700040130     C                   PARM                    WLBDA8
081800040130     C                   move      G08DAT        P1cddc
081900040130     C                   Z-ADD     Ectdsc        G08INV
082000040130     C                   Z-ADD     *ZEROS        G08DAT
082100040130     C                   MOVEL     '3'           G08ERR
082200040130     C                   CALL      'XSRDA8'
082300040130     C                   PARM                    WLBDA8
082400040130     C                   move      G08DAT        P1cdsc
082500000210     C                   ELSE
082600040130     c                   movel     ectfdr        tbeke1
082700040130     c                   exsr      srco1
082800040130     c                   movel     §co1des       p1ccdt
082900040130     C                   Z-ADD     Ectddc        G08INV
083000040130     C                   Z-ADD     *ZEROS        G08DAT
083100040130     C                   MOVEL     '3'           G08ERR
083200040130     C                   CALL      'XSRDA8'
083300040130     C                   PARM                    WLBDA8
083400040130     C                   move      G08DAT        P1cddc
083500040130     C                   Z-ADD     Ectdsc        G08INV
083600040130     C                   Z-ADD     *ZEROS        G08DAT
083700040130     C                   MOVEL     '3'           G08ERR
083800040130     C                   CALL      'XSRDA8'
083900040130     C                   PARM                    WLBDA8
084000040130     C                   move      G08DAT        P1cdsc
084100000211     C                   Z-ADD     ECTCMP        P1CCMP
084200000210     C                   MOVEL     *all'*'       P1CCMD
084300000210     C                   ENDIF
084400000211     C                   ELSE
084500000211     C                   Z-ADD     *zeros        P1CCMP
084600000211     C                   MOVEL     *all'*'       P1CCMD
084700040130     C                   clear                   P1cdsc
084800040130     C                   clear                   P1cddc
084900040130     c                   clear                   p1ccdt
085000000211     C                   ENDIF
085100000211     C                   ELSE
085200000210     C                   MOVEL     *all'*'       P1CVOD
085300000211     C                   Z-ADD     *zeros        P1CCMP
085400000210     C                   MOVEL     *all'*'       P1CCMD
085500040130     C                   clear                   P1cdsc
085600040130     C                   clear                   P1cddc
085700040130     c                   clear                   p1ccdt
085800000211     C                   ENDIF
085900000211     C                   ELSE
086000000211     C                   Z-ADD     *zeros        P1CVOC
086100000211     C                   MOVEL     *all'*'       P1CVOD
086200000211     C                   Z-ADD     *zeros        P1CCMP
086300000211     C                   MOVEL     *all'*'       P1CCMD
086400040130     C                   clear                   P1cdsc
086500040130     C                   clear                   P1cddc
086600040130     c                   clear                   p1ccdt
086700000211     C                   ENDIF
086800000210     C                   ELSE
086900000211     C                   Z-ADD     *zeros        P1CVOC
087000000211     C                   MOVEL     *all'*'       P1CVOD
087100000211     C                   Z-ADD     *zeros        P1CCMP
087200000211     C                   MOVEL     *all'*'       P1CCMD
087300040130     C                   clear                   P1cdsc
087400040130     C                   clear                   P1cddc
087500040130     c                   clear                   p1ccdt
087600000210     C                   ENDIF
087700000209     C*
087800000209     C   31              WRITE     EC82T1
087900000209     C   31              SETOFF                                       31
088000000215     C     w_EVTCMP      IFGE      PA1CMP
088100000215     C     w_EVTCMP      ANDLE     PA2CMP
088200000210     C                   WRITE     EC82D3
088300000215     C                   ENDIF
088400000209     C*
088500000209     C* PROCEDO LEGGENDO PROSSIVA VOCE MAGGIORATA DA VOCE MAGGIORANTE
088600000209     C     KEYEVM        READE     ECEVM01L                               21
088700000209     C                   ENDDO
088800000209     C*
088900000211     C                   ENDIF
089000000211     C                   ENDIF
089100000209     C*
089200000209     C                   ENDSR
089300000210     C*------------------------------------------------------------------------*
089400000210     C* CARMAG - CARICA IN SCHIERA LE INFORMAZIONI RELATIVE ALLE VOCI IN MAGG.
089500000210     C*------------------------------------------------------------------------*
089600000210     C     CARMAG        BEGSR
089700000210     C*
089800000211     C                   z-add     1             i                 3 0
089900000210     C*
090000000210     C                   open      ECEVT01L
090100000210     C     *loval        setll     ECEVT01L                           66
090200000210     C     *in66         doweq     *off
090300000210     C                   read      ECEVT01L                               66
090400000210     C*
090500000210     C     *in66         ifeq      *off
090600000210     C     EVTATB        andeq     *blanks
090700000210     C     EVTTVO        andeq     'M'
090800000210     C     EVTDDV        andle     PA1DDV
090900000210     C     EVTDSV        andge     PA2DDV
091000000210     C* RIEMPIO VARIABILI DI DEPOSITO X RIPETIZIONE VOCI DI MAGGIORAZIONE
091100000210     C                   z-add     EVTVOC        VOCMTES
091200000210     C                   z-add     EVTPRG        PRGMTES
091300000210     C                   movel     EVTVOD        DESMTES
091400000210     C                   movel     EVTTVO        TIPMTES
091500000211     C*
091600000211     C                   Z-ADD     EVTVOC        KVMVOC
091700000211     C                   Z-ADD     EVTPRG        KVMPRG
091800000211     C     KEYEVM        SETLL     ECEVM01L
091900000211     C     KEYEVM        READE     ECEVM01L                               21
092000000211     C     *IN21         DOWEQ     *OFF
092100000211     C*
092200000211     C* IMPOSTO SEMPRE I CAMPI "COSTANTI" (X VOCE)
092300000211     C                   z-add     VOCMTES       VOCMTE(i)
092400000211     C                   z-add     PRGMTES       PRGMTE(i)
092500000211     C                   movel     DESMTES       DESMTE(i)
092600000211     C                   movel     TIPMTES       TIPMTE(i)
092700000211     C*
092800000211     C* RIEMPIO BUFFER FORMATO RECORD X MAGGIORAZIONI
092900000211     C     EVMATB        IFEQ      *BLANKS
093000000211     C                   z-add     EVMVOC        VOCMTA(i)
093100000211     C                   z-add     EVMPRM        PRGMTA(i)
093200000211     C* REPERISCO I DATI "VOCE" DELLA VOCE DA MAGGIORARE
093300000211     C                   Z-ADD     EVMVOC        $VTVOC3
093400000211     C                   Z-ADD     EVMPRG        $VTPRG3
093500010808     C     EVMVOC        SETLL     ECEVT0XL                           98
093600010808     C     EVMVOC        READE     ECEVT0XL                               98
093700000214     C                   MOVEL     '0'           CHKVOC            1
093800000214     C     *IN98         DOWEQ     *OFF
093900000214     C     CHKVOC        ANDEQ     '0'
094000000214     C*
094100000215     C     w_EVTDDV      IFLE      PA1DDV
094200000215     C     w_EVTDSV      ANDGE     PA2DDV
094300000214     C                   MOVEL     '1'           CHKVOC
094400000214     C                   ELSE
094500010808     C     EVMVOC        READE     ECEVT0XL                               98
094600000214     C                   ENDIF
094700000214     C*
094800000214     C                   ENDDO
094900000214     C*
095000000214     C     CHKVOC        IFEQ      '1'
095100000215     C     w_EVTATB      IFEQ      *BLANKS
095200000215     C**   w_EVTDDV      ANDLE     PA1DDV
095300000215     C**   w_EVTDSV      ANDGE     PA2DDV
095400000215     C                   movel     w_EVTVOD      DESMTA(i)
095500000211     C* E REPERISCO I DATI "COMPETENZA" DELLA VOCE DA MAGGIORARE
095600000215     C                   Z-ADD     w_EVTCMP      $CTCMP
095700000211     C                   Z-ADD     SAVNBC        $CTNBC
095800040128     C*                  Z-ADD     w_EVTDDV      $CTDDC
095900040128     C                   Z-ADD     w_EVTDSV      $CTDDC
096000000214     C     KEYECT        SETGT     ECECT02L
096100000214     C                   READP     ECECT02L                               88
096200000211     C     *IN88         IFEQ      *OFF
096300000214     C     ECTCMP        IFEQ      $CTCMP
096400000214     C     ECTNBC        ANDEQ     $CTNBC
096500000214     C     ECTATB        ANDEQ     *blanks
096600000211     C     ECTDDC        ANDLE     PA1DDV
096700000211     C     ECTDSC        ANDGE     PA2DDV
096800000211     C                   z-add     ECTCMP        CODCMP(i)
096900000211     C                   movel     ECTCMD        DESCMP(i)
097000040130     c                   movel     ectfdr        tbeke1
097100040130     c                   exsr      srco1
097200040130     c                   movel     §co1des       cmpcdt(i)
097300040130     C                   Z-ADD     Ectddc        G08INV
097400040130     C                   Z-ADD     *ZEROS        G08DAT
097500040130     C                   MOVEL     '3'           G08ERR
097600040130     C                   CALL      'XSRDA8'
097700040130     C                   PARM                    WLBDA8
097800040130     C                   move      G08DAT        cmpddc(i)
097900040130     C                   Z-ADD     Ectdsc        G08INV
098000040130     C                   Z-ADD     *ZEROS        G08DAT
098100040130     C                   MOVEL     '3'           G08ERR
098200040130     C                   CALL      'XSRDA8'
098300040130     C                   PARM                    WLBDA8
098400040130     C                   move      G08DAT        cmpdsc(i)
098500000211     C                   ELSE
098600000211     C                   Z-ADD     ECTCMP        CODCMP(i)
098700000211     C                   MOVEL     *all'*'       DESCMP(i)
098800040130     c                   movel     ectfdr        tbeke1
098900040130     c                   exsr      srco1
099000040130     c                   movel     §co1des       cmpcdt(i)
099100040130     C                   Z-ADD     Ectddc        G08INV
099200040130     C                   Z-ADD     *ZEROS        G08DAT
099300040130     C                   MOVEL     '3'           G08ERR
099400040130     C                   CALL      'XSRDA8'
099500040130     C                   PARM                    WLBDA8
099600040130     C                   move      G08DAT        cmpddc(i)
099700040130     C                   Z-ADD     Ectdsc        G08INV
099800040130     C                   Z-ADD     *ZEROS        G08DAT
099900040130     C                   MOVEL     '3'           G08ERR
100000040130     C                   CALL      'XSRDA8'
100100040130     C                   PARM                    WLBDA8
100200040130     C                   move      G08DAT        cmpdsc(i)
100300000211     C                   ENDIF
100400000211     C                   ELSE
100500040130     C                   clear                   cmpdsc(i)
100600040130     C                   clear                   cmpddc(i)
100700040130     c                   clear                   cmpcdt(i)
100800000211     C                   Z-ADD     *zeros        CODCMP(i)
100900000211     C                   MOVEL     *all'*'       DESCMP(i)
101000000211     C                   ENDIF
101100000211     C                   ELSE
101200040130     C                   clear                   cmpdsc(i)
101300040130     C                   clear                   cmpddc(i)
101400040130     c                   clear                   cmpcdt(i)
101500000211     C                   MOVEL     *all'*'       DESMTA(i)
101600000211     C                   Z-ADD     *zeros        CODCMP(i)
101700000211     C                   MOVEL     *all'*'       DESCMP(i)
101800000211     C                   ENDIF
101900000211     C                   ELSE
102000040130     C                   clear                   cmpdsc(i)
102100040130     C                   clear                   cmpddc(i)
102200040130     c                   clear                   cmpcdt(i)
102300000211     C                   Z-ADD     *zeros        VOCMTA(i)
102400000211     C                   MOVEL     *all'*'       DESMTA(i)
102500000211     C                   Z-ADD     *zeros        CODCMP(i)
102600000211     C                   MOVEL     *all'*'       DESCMP(i)
102700000211     C                   ENDIF
102800000211     C                   ELSE
102900040130     C                   clear                   cmpdsc(i)
103000040130     C                   clear                   cmpddc(i)
103100040130     c                   clear                   cmpcdt(i)
103200000211     C                   Z-ADD     *zeros        VOCMTA(i)
103300000211     C                   MOVEL     *all'*'       DESMTA(i)
103400000211     C                   Z-ADD     *zeros        CODCMP(i)
103500000211     C                   MOVEL     *all'*'       DESCMP(i)
103600000211     C                   ENDIF
103700000211     C*
103800000211     C* PROCEDO LEGGENDO PROSSIVA VOCE MAGGIORATA DA VOCE MAGGIORANTE
103900000211     C     KEYEVM        READE     ECEVM01L                               21
104000000211     C                   ADD       1             i
104100000211     C                   ENDDO
104200000211     C*
104300000211     C                   endif
104400000211     C                   enddo
104500000210     C*
104600000210     C                   close     ECEVT01L
104700000210     C*
104800000210     C                   ENDSR
104900000211     C*------------------------------------------------------------------------*
105000000211     C* GESMAG - GESTISCE LE VOCI IN MAGGIORAZIONE RISPETTO ALLE COMPETENZE
105100000211     C*------------------------------------------------------------------------*
105200000211     C     GESMAG        BEGSR
105300000211     C*
105400000211     C     1             DO        500           y                 3 0
105500000211     C     VOCMTE(y)     IFEQ      *zeros
105600000214     C     VOCMTA(y)     ANDEQ     *zeros
105700000214     C     CODCMP(y)     ANDEQ     *zeros
105800000211     C                   LEAVE
105900000211     C                   ELSE
106000000211     C*
106100000211     C* "PULISCO" AD OGNI PASSAGGIO IL FORMATO RECORD DI STAMPA 1°
106200000211     C                   CLEAR                   EC82D2
106300000214     C     CODCMP(y)     IFEQ      SAVCMP
106400000214     C     SAVCMP        ANDNE     *zeros
106500000211     C                   Z-ADD     VOCMTE(y)     P1CVOC
106600000211     C                   Z-ADD     PRGMTE(y)     P1CPRG
106700000211     C                   MOVEL     DESMTE(y)     P1CVOD
106800000211     C                   MOVEL     TIPMTE(y)     P1CTVO
106900000211     C                   Z-ADD     *ZEROS        P1CCMP
107000000211     C                   MOVEL     *BLANKS       P1CCMD
107100000211     C* STAMPO RIGA PRINCIPALE DI VOCE (GESTISCO ANCHE LA "PAG NUOVA")
107200000211     C   31              WRITE     EC82T1
107300000211     C   31              SETOFF                                       31
107400000215     C     CODCMP(y)     IFGE      PA1CMP
107500000215     C     CODCMP(y)     ANDLE     PA2CMP
107600000215     C                   WRITE     EC82D2
107700000215     C                   ENDIF
107800000211     C*
107900000211     C* "PULISCO" AD OGNI PASSAGGIO IL FORMATO RECORD DI STAMPA 2°
108000000211     C                   CLEAR                   EC82D3
108100000211     C                   Z-ADD     VOCMTA(y)     P1CVOC
108200000211     C                   MOVEL     DESMTA(y)     P1CVOD
108300000211     C                   Z-ADD     CODCMP(y)     P1CCMP
108400000211     C                   MOVEL     DESCMP(y)     P1CCMD
108500040130     C                   Z-ADD     cmpddc(y)     P1Cddc
108600040130     C                   Z-ADD     cmpdsc(y)     P1Cdsc
108700040130     C                   MOVEL     cmpcdt(y)     P1CCdt
108800000211     C* STAMPO RIGA VOCE MAGGIORATA (GESTISCO ANCHE LA "PAG NUOVA")
108900000211     C   31              WRITE     EC82T1
109000000211     C   31              SETOFF                                       31
109100000215     C     CODCMP(y)     IFGE      PA1CMP
109200000215     C     CODCMP(y)     ANDLE     PA2CMP
109300000211     C                   WRITE     EC82D3
109400000211     C                   ENDIF
109500000215     C                   ENDIF
109600000211     C                   ENDIF
109700000211     C                   ENDDO
109800000215     C*
109900000211     C                   ENDSR
110000020923     C*-------------------------------------------------------------------------
110100020923     C* REPERISCE I DATI UTENTE
110200020923     C*-------------------------------------------------------------------------
110300020923     C     REPDATIUTE    BEGSR
110400020923     C*
110500020923     C* INIZIALIZZA VARIABILI DI WRK
110600020923     C                   CLEAR                   TIBS34DS
110700020923     C                   CLEAR                   AZUTEDS
110800020923     C                   CLEAR                   DDATIUTE
110900020923     C*
111000020923     C     *DTAARA       DEFINE    §azute        azuteds
111100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
111200020923     C                   IN(E)     *DTAARA
111300020923if  1C                   IF        %Error
111400020923     c                   EVAL      I34Tla = 'L'
111500020923     C                   CALL      'TIBS34R'
111600020923     C                   PARM                    Tibs34Ds
111700020923     C                   IN        *DTAARA
111800020923e   1C                   ENDIF
111900020923     C*
112000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
112100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
112200020923     C*
112300020923     C                   ENDSR
112400000000     C*------------------------------------------------------------------------*
112500040130     C* srco1  - lettura tabella co1
112600000000     C*------------------------------------------------------------------------*
112700040130     C     srco1         BEGSR
112800000000     C*
112900040130     c                   movel     'CO1'         tbecod
113000040130     c                   clear                   dco1
113100040130     c     ktbe          chain     tntbe01l
113200040130     c                   if        %found(tntbe01l) and tbeatb = ' '
113300040130     c                   movel     tbeuni        dco1
113400040130     c                   end
113500040130     C*
113600040130     C                   ENDSR
113700040130     C*------------------------------------------------------------------------*
113800040130     C* *INZSR - IMPOSTAZIONI INIZIALI
113900040130     C*------------------------------------------------------------------------*
114000040130     C     *INZSR        BEGSR
114100040130     C*
114200000000     C* PARAMETRI IN INPUT AL PGM
114300000000     C     *ENTRY        PLIST
114400000000     C                   PARM                    KPJBA
114500010808     C                   MOVEL     KPJBU         PARAM
114600010808     C*
114700010808     C* OVRDBF PER LETTURA SECONDA CHIAVE DI INPUT
114800010808     C                   Z-ADD     80            LENGH            15 5
114900010808     C                   CALL      'QCMDEXC'
115000010808     C                   PARM                    CMD(3)
115100010808     C                   PARM                    LENGH
115200010808     C*
115300010808     C* DOPO DI CHE' APRO IL FILE VOCI DI "WRK"
115400010808     C                   OPEN      ECEVT0XL
115500000209     C*
115600000209     C* SE STAMPA RICHIESTA IN DETTAGLIO
115700000209     C     PA1SOD        IFEQ      'D'
115800000209     C                   SETON                                        50
115900000209     C                   ELSE
116000000209     C                   SETOFF                                       50
116100000209     C                   ENDIF
116200000208     C*
116300000209     C* RIGIRA LA 1° DATA PASSATA NEL PARAMETRO
116400000209     C     PA1DDV        IFGT      *ZEROS
116500000209     C                   Z-ADD     PA1DDV        SAVDAT            8 0
116600000208     C                   Z-ADD     PA1DDV        G08DAT
116700000208     C                   Z-ADD     *ZEROS        G08INV
116800000208     C                   MOVEL     '0'           G08ERR
116900000208     C                   CALL      'XSRDA8'
117000000224     C                   PARM                    WLBDA8
117100000208     C                   Z-ADD     G08INV        PA1DDV
117200000209     C                   ENDIF
117300000209     C*
117400000209     C* RIGIRA LA 2° DATA PASSATA NEL PARAMETRO
117500000209     C     PA2DDV        IFGT      *ZEROS
117600000209     C                   Z-ADD     PA2DDV        G08DAT
117700000209     C                   Z-ADD     *ZEROS        G08INV
117800000209     C                   MOVEL     '0'           G08ERR
117900000209     C                   CALL      'XSRDA8'
118000000224     C                   PARM                    WLBDA8
118100000209     C                   Z-ADD     G08INV        PA2DDV
118200000209     C                   ENDIF
118300000209     C*
118400000209     C* REPERISCO SUBITO L'ULTIMO BLOCCO (QUELLO VALIDO)
118500000209     C                   Z-ADD     *zeros        SAVNBC            5 0
118600000209     C     *hival        SETGT     ECECT01L
118700000209     C                   READP     ECECT01L                               60
118800000209     C  N60              Z-ADD     ECTNBC        SAVNBC
118900000000     C*
119000000000     C* RECUPERO DATI DELL'UTENTE
119100020923     C                   EXSR      REPDATIUTE
119200000208     C                   MOVEL     DSFIRS        PTCRSU
119300000000     C*
119400000210     C* VARIABILI RIFERITE AL DATABASE
119500000209     C     *LIKE         DEFINE    EVTVOC        $VTVOC3
119600000209     C     *LIKE         DEFINE    EVTPRG        $VTPRG3
119700000210     C     *LIKE         DEFINE    EVTCMP        SAVKEY
119800000208     C     *LIKE         DEFINE    EVTCMP        $VTCMP
119900000000     C     *LIKE         DEFINE    EVTVOC        $VTVOC
120000000000     C     *LIKE         DEFINE    EVTPRG        $VTPRG
120100000000     C     *LIKE         DEFINE    EVTVOC        KVDVOC
120200000000     C     *LIKE         DEFINE    EVTPRG        KVDPRG
120300000000     C     *LIKE         DEFINE    EVTVOC        KVMVOC
120400000000     C     *LIKE         DEFINE    EVTPRG        KVMPRG
120500000209     C     *LIKE         DEFINE    ECTCMP        $CTCMP
120600000209     C     *LIKE         DEFINE    ECTNBC        $CTNBC
120700000214     C     *LIKE         DEFINE    ECTDDC        $CTDDC
120800000210     C     *LIKE         DEFINE    EVTVOC        VOCMTES
120900000210     C     *LIKE         DEFINE    EVTPRG        PRGMTES
121000000210     C     *LIKE         DEFINE    EVTVOD        DESMTES
121100000210     C     *LIKE         DEFINE    EVTTVO        TIPMTES
121200000211     C*
121300000211     C* VARIABILI RIFERITE AL DATABASE
121400000211     C                   Z-ADD     *ZEROS        SAVCMP            3 0
121500020923     C                   Z-ADD     1             TBLKUT
121600000000     C*
121700040130     C     Ktbe          KLIST
121800040130     C                   KFLD                    tbecod
121900040130     C                   KFLD                    tbeke1
122000040130     C*
122100040130     C     KEYEVT        KLIST
122200040130     C                   KFLD                    $VTVOC
122300040130     C                   KFLD                    $VTPRG
122400000208     C*
122500000208     C     KEYEV2        KLIST
122600000208     C                   KFLD                    $VTCMP
122700000208     C                   KFLD                    $VTVOC
122800000208     C                   KFLD                    $VTPRG
122900000209     C*
123000000209     C     KEYEV3        KLIST
123100000209     C                   KFLD                    $VTVOC3
123200000209     C                   KFLD                    $VTPRG3
123300000209     C*
123400000209     C     KEYECT        KLIST
123500000209     C                   KFLD                    $CTCMP
123600000209     C                   KFLD                    $CTNBC
123700000214     C                   KFLD                    $CTDDC
123800000000     C*
123900000000     C     KEYEVD        KLIST
124000000000     C                   KFLD                    KVDVOC
124100000000     C                   KFLD                    KVDPRG
124200000000     C*
124300000000     C     KEYEVM        KLIST
124400000000     C                   KFLD                    KVMVOC
124500000000     C                   KFLD                    KVMPRG
124600000000     C*
124700000000     C     KEYTAB        KLIST
124800020923     C                   KFLD                    TBLKUT
124900000000     C                   KFLD                    TBLCOD
125000000000     C                   KFLD                    TBLKEY
125100000000     C*
125200000000     C     KEYTAM        KLIST
125300000000     C                   KFLD                    EVDCLI
125400000000     C                   KFLD                    EVDCTR
125500000211     C*
125600000211     C* CARICO LE VOCI DI MAGGIORAZIONE (E DATI CORRELATI)
125700000211     C     PA1COV        IFEQ      'C'
125800000211     C                   EXSR      CARMAG
125900000211     C                   ENDIF
126000000000     C*
126100000000     C* SE I PARAMETRI INIZIALE/FINALE SONO AMBEDUE VUOTI, IMPOSTO IL FINALE
126200000000     C     PA1VOC        IFEQ      *ZERO
126300000000     C     PA2VOC        ANDEQ     *ZERO
126400000000     C                   Z-ADD     *HIVAL        PA2VOC
126500000000     C                   ENDIF
126600000000     C*
126700000000     C     PA1CMP        IFEQ      *ZERO
126800000000     C     PA2CMP        ANDEQ     *ZERO
126900000000     C                   Z-ADD     *HIVAL        PA2CMP
127000000000     C                   ENDIF
127100000209     C* LANCIO ROUTINE DI VALORIZZAZIONE CAMPI DI TESTATA
127200000209     C                   EXSR      VALTES
127300000000     C*
127400000000     C                   ENDSR
127500000000     C*------------------------------------------------------------------------*
127600000000** OVERRIDE SUL FILE DI STAMPA
127700000000OVRPRTF FILE(TREC83P) USRDTA('Voci C/E')
127800000209OVRPRTF FILE(TREC82P) USRDTA('Voci C/E')
127900010808OVRDBF FILE(ECEVT0XL) TOFILE(ECEVT01L)
