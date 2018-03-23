000100020322     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020322      *------------------------------------------------------------------------*
000300020322      * STAMPA UNITA' EDP - ESEGUE
000400020322      *------------------------------------------------------------------------*
000500020322
000600020322      *------------------------------------------------------------------------*
000700020322      * DATA BASE                                                              *
000800020322      *------------------------------------------------------------------------*
000900000000     FUNANA01L  IF   E           K DISK    USROPN
001000000000     FUNANA02L  IF   E           K DISK    USROPN
001100000000     F                                     RENAME(UNANA000:ANA02L)
001200000000     FUNANA03L  IF   E           K DISK    USROPN
001300000000     F                                     RENAME(UNANA000:ANA03L)
001400000000     FUNANA04L  IF   E           K DISK    USROPN
001500000000     F                                     RENAME(UNANA000:ANA04L)
001600000000     FUNANA05L  IF   E           K DISK    USROPN
001700000000     F                                     RENAME(UNANA000:ANA05L)
001800020531     FAZORG21l  IF   E           K DISK
001900020531     fazorg01l  if   e           k disk
002000020322     ftntbe01l  if   e           k disk
002100000000     FTNSDU7P   O    E             PRINTER OFLIND(*IN01)
002200090904
002300090904       //--------------------------------------------------------------
002400090904       // COSTANTI
002500090904       //--------------------------------------------------------------
002600090904
002700090904     d c_Logistica     c                   const('LOG')
002800020322
002900020322      *------------------------------------------------------------------------*
003000020322      * SCHIERE                                                                *
003100020322      *------------------------------------------------------------------------*
003200020322      *------------------
003300020322      * DECODIFICA PARAMETRI FILTRO
003400020322      *------------------
003500000000     D PAR             S             26    DIM(7) CTDATA PERRCD(1)
003600020322      *------------------
003700020322      * DECODIFICA TOTALI
003800020322      *------------------
003900000000     D TOT             S             20    DIM(8) CTDATA PERRCD(1)
004000020322      *------------------
004100020322      * INTESTAZIONE STAMPA SINTETICA
004200020322      *------------------
004300000000     D ISS             S             21    DIM(5) CTDATA PERRCD(1)
004400020322      *------------------
004500020322      * FILIALI
004600020322      *------------------
004700971124     D FIL             S              3  0 DIM(500)                             *CODICE FILIALE
004800971124     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
004900971124     D TFIL            S              1    DIM(500)                             *TIPO FILIALE
005000971124     D FEL             S              3  0 DIM(500)                             *FILIALE ELABORATORE
005100020322      *------------------
005200020322      * DECODIFICHE TIPI UNITA'
005300020322      *------------------
005400000000     D TIP             S              2  0 DIM(100)
005500000000     D DTIP            S             20    DIM(100)
005600090904
005700090904     d  skpoabi        s              3  0 dim(500)
005800020322
005900020322      *------------------------------------------------------------------------*
006000020322      * INPUT                                                                  *
006100020322      *------------------------------------------------------------------------*
006200020322      *------------------
006300020322      * ARCHITETTURA
006400020322      *------------------
006500000000     D KPJBA         E DS
006600000000     D  PROFIL                 1     10                                         UTENTE
006700020322      *------------------
006800020322      * PARAMETRI FILTRO
006900020322      *------------------
007000971013     D PARAM           DS
007100971013     D  PARORD                 1      1  0
007200971013     D  PARTIP                 2      3  0
007300971013     D  PARMAG                 4      6  0
007400971013     D  PARDSM                 7     14  0
007500971013     D  PARDS2                15     22  0
007600971013     D  PARMAR                23     37
007700971013     D  PARMOD                38     52
007800971013     D  PARSEL                 2     52
007900971013     D  PARTFI                53     56
008000971013     D  PARTF1                53     53
008100971013     D  PARTF2                54     54
008200971013     D  PARTF3                55     55
008300971013     D  PARTF4                56     56
008400971013     D  PARSOD                57     57
008500971013     D  PARTEC                58     58
008600971013     D  DECTIP                59     88
008700971013     D  DECMAG                89    118
008800971013     D  DECDSM               119    126  0
008900971013     D  DECDS2               127    134  0
009000971013     D  PARLIF               135    135
009100971013     D  PARTOT               136    136
009200971013     D  PARFOS               137    137
009300020322      *------------------
009400020322      * DS CONTROLLA DATA (8)
009500020322      *------------------
009600971013     D WLBDA8          DS
009700971013     D  G08DAT                 1      8  0
009800971013     D  G08INV                 9     16  0
009900971013     D  G08ERR                17     17
010000971013     D  G08TGI                18     22  0
010100020322      *---------------------
010200020322      * DS REPEIMENTO TERMINALs
010300020322      *---------------------
010400971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
010500020322      *------------------
010600020322      * DS RIGA DATI TECNICI
010700020322      *------------------
010800000000     D                 DS
010900000000     D  UNAEMU                 1     15
011000000000     D  UNAMVD                16     30
011100000000     D  UNAMTA                31     45
011200000000     D  UNAMDF                46     60
011300000000     D  UNAMRA                61     75
011400000000     D  UNAMDR                76     90
011500000000     D  UNAX25                91    105
011600000000     D  UNANOT               106    155
011700120412     d* Escludo i primi 6 caratteri del campo UNAEMU che contengono il numero ordine
011800120412     D  DSTEC                  7    155
011900020322
012000020322     d dsksc           ds
012100020322     d  prcksc                 1      7
012200020322     d  prdksc                 9     30
012300020325
012400020327     d tibs69ds      e ds
012500020327     d ds_cnaco      e ds                  extname(cnaco00f)
012600020327     d ds_cnind      e ds                  extname(cnind00f)
012700020327     d ds_cnclp      e ds                  extname(cnclp00f)
012800020327     d ds_fncls      e ds                  extname(fncls00f)
012900020322
013000020513     d ddatiute      e ds
013100020513     d azuteds       e ds                  extname(azute00f)
013200020513     d tibs34ds      e ds                  inz
013300020513     d dute01        e ds
013400020513     d dlat          e ds
013500020516     d trul31ds      e ds
013600021014     d  skpo31                10    759  0 dim(250)
013700020531     d trul26ds      e ds
013800020531     d  skpo                  11    760  0 dim(250)
013900020603     d  skpot                761   1510  0 dim(250)
014000090904      *
014100090904     d Og143         e ds                  inz
014200090904
014300090904
014400090904     d wabi            s                   like(uteaut)
014500090904     d wpocli          s              3  0
014600090904
014700090904     d ii              s              3  0
014800100908     d WFAG            s                   like(orgfag)
014900021014
015000000000     C*------------------------------------------------------------------------*
015100950126     C* MAIN LINE
015200000000     C*------------------------------------------------------------------------*
015300000000     C*
015400000000     C* STAMPA PARAMETRI FILTRO
015500000000     C                   EXSR      STAPAR
015600000000     C*
015700000000     C* SCELTA ELABORAZIONE
015800000000CS  1C     PARORD        CASEQ     1             ELORD1                         *ELABORAZIONE 1° ORD
015900000000     C     PARORD        CASEQ     2             ELORD2                         *ELABORAZIONE 2° ORD
016000000000     C     PARORD        CASEQ     3             ELORD3                         *ELABORAZIONE 3° ORD
016100000000     C     PARORD        CASEQ     4             ELORD4                         *ELABORAZIONE 4° ORD
016200000000     C     PARORD        CASEQ     5             ELORD5                         *ELABORAZIONE 5° ORD
016300000000E   1C                   ENDCS
016400000000     C*
016500000000     C* STAMPA RIGA DI FINE PROSPETTO
016600000000     C                   WRITE     SDU7FI
016700971128     C*
016800971128     C* OPERAZIONI FINALI
016900971128     C                   EXSR      FINSR
017000000000     C*
017100000000     C                   SETON                                        LR
017200971128     C*------------------------------------------------------------------------*
017300971128     C* FINSR - OPERAZIONI FINALI
017400971128     C*------------------------------------------------------------------------*
017500971128     C     FINSR         BEGSR
017600971128     C*
017700971128     C                   CLEAR                   DSLV55
017800971128     C                   MOVEL     'C'           D55TLA
017900971128     C                   CALL      'FNLV55R'
018000971128     C                   PARM                    DSLV55
018100971128     C*
018200971128     C                   ENDSR
018300000000     C*------------------------------------------------------------------------*
018400950126     C* ELORD1 - ELABORAZIONE UNITA' EDP PER RICHIESTA 1° ORDINAMENTO
018500000000     C*------------------------------------------------------------------------*
018600000000     C     ELORD1        BEGSR
018700000000     C*
018800000000     C* APERTURA FILE
018900000000     C                   OPEN      UNANA01L
019000000000     C*
019100000000     C* POSIZIONAMENTO E PRIMA LETTURA
019200000000     C                   EXSR      SETFIL
019300000000     C*-------------------
019400000000     C* FINCHE' FINE FILE
019500000000     C*-------------------
019600000000DO  1C     $FINE         DOWEQ     '0'
019700000000     C*
019800000000     C* OPERAZIONI PER NUOVO TIPO UNITA'
019900000000     C                   EXSR      NEWTIP
020000000000     C*-------------------
020100000000     C* FINCHE' ROTTURA TIPO UNITA'
020200000000     C*-------------------
020300000000DO  2C     $FINE         DOWEQ     '0'
020400000000     C     DEPTIP        ANDEQ     UNATIP
020500000000     C*
020600000000     C* CONTEGGIO UNITA' PER TIPO
020700000000     C                   ADD       1             TOTTIP            5 0          *TOTALE TIPO
020800000000     C                   ADD       1             TOTGEN            5 0          *TOTALE GENERALE
020900000000     C*
021000971022     C* FINE PAGINA, STAMPA TESTATA
021100971022IF  3C     *IN01         IFEQ      *ON
021200120412IF  4C     PARSOD        IFEQ      'S'                                          *INTESTAZ. SINTETICA
021300000000     C                   MOVEL     ISS(1)        WDES             21
021400000000E   4C                   ENDIF
021500000000     C                   EXSR      STATES
021600971022     C                   SETOFF                                       01
021700000000E   3C                   ENDIF
021800000000     C*
021900950331     C* SE RICHIESTO STAMPA RIGHE DI DETTAGLIO
022000000000IF  3C     PARSOD        IFEQ      'D'
022100000000     C                   EXSR      STADET                                       *STAMPA DETTAGLIO
022200000000E   3C                   ENDIF
022300000000     C*
022400000000     C* LETTURA SUCESSIVA UNITA' EDP
022500000000     C                   EXSR      LETFIL
022600000000E   2C                   ENDDO
022700000000     C*-------------------
022800000000     C* TOTALE  T I P O
022900000000     C*-------------------
023000950331IF  2C     PARSOD        IFEQ      'D'                                          *DETTAGLIO
023100950331     C     PARTOT        ANDEQ     'S'                                          *TOTALE
023200941227     C                   WRITE     SDU7RI                                       *TRATTINI
023300950331E   2C                   ENDIF
023400000000     C*
023500950331IF  2C     PARSOD        IFEQ      'D'                                          *DETTAGLIO
023600950331     C     PARTOT        ANDEQ     'S'                                          *TOTALE
023700950331     C     PARSOD        OREQ      'S'                                          *SINTETICA
023800000000     C                   MOVEL     TOT(1)        DESTOT           10
023900000000     C                   MOVEL     *BLANKS       CODTO1           15
024000000000     C                   MOVEL     DEPTIP        CODTO1
024100000000     C                   MOVEL     *BLANKS       CODTO2           20
024200000000     C                   MOVEL     DESTIP        CODTO2
024300000000     C                   Z-ADD     TOTTIP        TOTALE            5 0
024400000000     C                   EXSR      STATOT
024500950331E   2C                   ENDIF
024600000000     C*
024700941227     C* DETTAGLIO: DOPO TOTALE STAMPO RIGA TRATTINI
024800950331     C     PARSOD        IFEQ      'D'
024900950331     C     PARTOT        ANDEQ     'S'                                          *TOTALE
025000941227     C                   WRITE     SDU7RI                                       *TRATTINI
025100090907IF  3C     PARFOS        IFEQ      'F'                                          *SOLO PER FILIALE
025200971022     C                   SETON                                        01        *SALTO PAGINA
025300971022E   3C                   ENDIF
025400971022E   2C                   ENDIF
025500000000     C*
025600000000E   1C                   ENDDO
025700000000     C*-------------------
025800000000     C* TOTALE  G E N E R A L E
025900000000     C*-------------------
026000950331IF  1C     PARSOD        IFEQ      'S'                                          *SINTETICA
026100941227     C                   WRITE     SDU7RI                                       *TRATTINI
026200000000E   1C                   ENDIF
026300000000     C*
026400950331IF  1C     PARSOD        IFEQ      'D'                                          *SE DETTAGLIO
026500950331     C     PARTOT        ANDEQ     'S'                                          *CON TOTALI
026600950331     C     PARSOD        OREQ      'S'                                          *OPPURE SINTETICA
026700000000     C                   MOVEL     TOT(1)        DESTOT
026800000000     C                   MOVEL     *BLANKS       CODTO1
026900000000     C                   MOVEL     TOT(7)        CODTO1
027000000000     C                   MOVEL     *BLANKS       CODTO2
027100000000     C                   Z-ADD     TOTGEN        TOTALE
027200950331     C                   EXSR      STATOT
027300950331E   1C                   ENDIF
027400000000     C*
027500941227     C                   WRITE     SDU7RI                                       *TRATTINI
027600000000     C* CHIUSURA FILE
027700000000     C                   CLOSE     UNANA01L
027800950331     C*---
027900950331     C* NB. LA RICHIESTA "STAMPA TOTALI", VALE SOLO SE SI VUOLE ANCHE LA
028000950331     C*     "STAMPA IN DETTAGLIO" PERCHE' ALTRIMENTI NON HA SENSO
028100950331     C*---
028200000000     C                   ENDSR
028300000000     C*------------------------------------------------------------------------*
028400950126     C* ELORD2 - ELABORAZIONE UNITA' EDP PER RICHIESTA 2° ORDINAMENTO
028500000000     C*------------------------------------------------------------------------*
028600000000     C     ELORD2        BEGSR
028700000000     C*
028800000000     C* APERTURA FILE
028900000000     C                   OPEN      UNANA02L
029000000000     C*
029100000000     C* POSIZIONAMENTO E PRIMA LETTURA
029200000000     C                   EXSR      SETFIL
029300000000     C*-------------------
029400000000     C* FINCHE' FINE FILE
029500000000     C*-------------------
029600100621DO  0C     $FINE         DOWEQ     '0'
029700100621     C*
029800100621     C* OPERAZIONI PER NUOVO TIPO UNITA'
029900100621     C                   EXSR      NEWTIP
030000100621     C*-------------------
030100100621     C* FINCHE' ROTTURA TIPO UNITA'
030200100621     C*-------------------
030300100621DO  1C     $FINE         DOWEQ     '0'
030400100621     C     DEPTIP        ANDEQ     UNATIP
030500100621     C*
030600000000     C                   Z-ADD     *ZEROS        TOTMAT
030700000000     C*
030800000000     C* CONTEGGIO UNITA' PER TIPO
030900000000     C                   ADD       1             TOTMAT            5 0          *TOTALE MATRICOLA
031000100621     C                   ADD       1             TOTTIP                         *TOTALE TIPO
031100000000     C                   ADD       1             TOTGEN                         *TOT GENERALE (E'=)
031200000000     C*
031300000000     C* FINE PAGINA: STAMPO TESTATA
031400000000IF  2C     *IN01         IFEQ      *ON
031500000000IF  3C     PARSOD        IFEQ      'S'                                          *STAMPA SINTETICA
031600000000     C                   MOVEL     ISS(2)        WDES
031700941227     C                   MOVEL     *BLANKS       WCOD             15
031800000000E   3C                   ENDIF
031900000000     C                   EXSR      STATES
032000950126     C                   SETOFF                                       01
032100000000E   2C                   ENDIF
032200000000     C*
032300000000     C* SE RICHIESTO STAMPO RIGHE DI DETTAGLIO
032400000000IF  2C     PARSOD        IFEQ      'D'
032500000000     C                   EXSR      STADET                                       *STAMPA DETTAGLIO
032600000000     C*
032700000000     C* SE RICHIESTO STAMPO RIGHE SINTETICHE
032800000000X   2C                   ELSE
032900000000     C*
033000000000     C                   MOVEL     TOT(8)        DESTOT
033100000000     C                   MOVEL     *BLANKS       CODTO1
033200000000     C                   MOVEL     UNAMAT        CODTO1
033300000000     C                   MOVEL     *BLANKS       CODTO2
033400000000     C                   Z-ADD     TOTMAT        TOTALE
033500000000     C                   EXSR      STATOT                                       *STAMPA SINTETICA
033600950330E   2C                   ENDIF
033700000000     C*
033800000000     C* LETTURA SUCESSIVA UNITA' EDP
033900000000     C                   EXSR      LETFIL
034000000000     C*
034100000000E   1C                   ENDDO
034200000000     C*-------------------
034300000000     C* TOTALE  M A T R I C O L A
034400000000     C*-------------------
034500100621IF  1C*//  PARSOD        IFEQ      'D'                                          *DETTAGLIO
034600100621     C*//  PARTOT        ANDEQ     'S'                                          *E TOTALE
034700100621     C*//  PARSOD        OREQ      'S'                                          *OPPURE SINTETICA
034800100621     C*//                WRITE     SDU7RI                                       *RIGA
034900000000     C*
035000100621     C*//                MOVEL     TOT(8)        DESTOT
035100100621     C*//                MOVEL     *BLANKS       CODTO1
035200100621     C*//                MOVEL     TOT(7)        CODTO1
035300100621     C*//                MOVEL     *BLANKS       CODTO2
035400100621     C*//                Z-ADD     TOTGEN        TOTALE
035500100621     C*//                EXSR      STATOT
035600000000     C*
035700100621     C*//                WRITE     SDU7RI                                       *RIGA
035800100621E   1C*//                ENDIF
035900100621     C*-------------------
036000100621     C* TOTALE  T I P O
036100100621     C*-------------------
036200100621IF  1C     PARSOD        IFEQ      'D'                                          *DETTAGLIO
036300100621     C     PARTOT        ANDEQ     'S'                                          *TOTALE
036400100621     C                   WRITE     SDU7RI                                       *TRATTINI
036500100621E   1C                   ENDIF
036600100621     C*
036700100621IF  1C     PARSOD        IFEQ      'D'                                          *DETTAGLIO
036800100621     C     PARTOT        ANDEQ     'S'                                          *TOTALE
036900100621     C     PARSOD        OREQ      'S'                                          *SINTETICA
037000100621     C                   MOVEL     TOT(1)        DESTOT           10
037100100621     C                   MOVEL     *BLANKS       CODTO1           15
037200100621     C                   MOVEL     DEPTIP        CODTO1
037300100621     C                   MOVEL     *BLANKS       CODTO2           20
037400100621     C                   MOVEL     DESTIP        CODTO2
037500100621     C                   Z-ADD     TOTTIP        TOTALE            5 0
037600100621     C                   EXSR      STATOT
037700100621E   1C                   ENDIF
037800100621     C*
037900100621     C* DETTAGLIO: DOPO TOTALE STAMPO RIGA TRATTINI
038000100621IF  1C     PARSOD        IFEQ      'D'
038100100621     C     PARTOT        ANDEQ     'S'                                          *TOTALE
038200100621     C                   WRITE     SDU7RI                                       *TRATTINI
038300100621IF  2C     PARFOS        IFEQ      'F'                                          *SOLO PER FILIALE
038400100621     C                   SETON                                        01        *SALTO PAGINA
038500100621E   2C                   ENDIF
038600100621E   1C                   ENDIF
038700100621     C*
038800100621E   0C                   ENDDO
038900100621     C*-------------------
039000100621     C* TOTALE  G E N E R A L E
039100100621     C*-------------------
039200100621IF  1C     PARSOD        IFEQ      'S'                                          *SINTETICA
039300100621     C                   WRITE     SDU7RI                                       *TRATTINI
039400100621E   1C                   ENDIF
039500100621     C*
039600100621IF  1C     PARSOD        IFEQ      'D'                                          *SE DETTAGLIO
039700100621     C     PARTOT        ANDEQ     'S'                                          *CON TOTALI
039800100621     C     PARSOD        OREQ      'S'                                          *OPPURE SINTETICA
039900100621     C                   MOVEL     TOT(1)        DESTOT
040000100621     C                   MOVEL     *BLANKS       CODTO1
040100100621     C                   MOVEL     TOT(7)        CODTO1
040200100621     C                   MOVEL     *BLANKS       CODTO2
040300100621     C                   Z-ADD     TOTGEN        TOTALE
040400100621     C                   EXSR      STATOT
040500100621E   1C                   ENDIF
040600100621     C*
040700100621     C                   WRITE     SDU7RI                                       *TRATTINI
040800000000     C* CHIUSURA FILE
040900000000     C                   CLOSE     UNANA02L
041000950331     C*---
041100950331     C* NB. LA RICHIESTA "STAMPA TOTALI", VALE SOLO SE SI VUOLE ANCHE LA
041200950331     C*     "STAMPA IN DETTAGLIO" PERCHE' ALTRIMENTI NON HA SENSO
041300950331     C*---
041400000000     C                   ENDSR
041500000000     C*------------------------------------------------------------------------*
041600950126     C* ELORD3 - ELABORAZIONE UNITA' EDP PER RICHIESTA 3° ORDINAMENTO
041700000000     C*------------------------------------------------------------------------*
041800000000     C     ELORD3        BEGSR
041900000000     C*
042000000000     C* APERTURA FILE
042100000000     C                   OPEN      UNANA03L
042200000000     C*
042300000000     C* POSIZIONAMENTO TABELLA
042400000000IF  1C     PARMAG        IFGT      *ZEROS
042500000000     C                   Z-ADD     1             F
042600950126     C                   SETOFF                                       28
042700000000     C     PARMAG        LOOKUP    FIL(F)                                 28      MAGAZZINO RICHIEST
042800000000X   1C                   ELSE
042900000000     C                   Z-ADD     1             F                                DALL'INIZIO
043000000000E   1C                   ENDIF
043100000000     C*--------------------
043200000000     C* FINCHE' FINE TABELLA
043300000000     C*--------------------
043400000000DO  1C     F             DOWLE     NUMFIL
043500000000     C*
043600950125     C* OPERAZIONI NUOVA FILIALE ELABORATORE
043700950125     C                   EXSR      NEWFEL
043800000000     C*--------------------
043900950125     C* FINCHE' ROTTURA FILIALE ELABORATORE
044000000000     C*--------------------
044100000000DO  2C     F             DOWLE     NUMFIL
044200950125     C     DEPFEL        ANDEQ     WFEL
044300000000     C                   MOVEL     '0'           $FINE                          *REIMPOSTO FINE FILE
044400000000     C*
044500000000     C* POSIZIONAMENTO E PRIMA LETTURA UNITA' EDP
044600000000     C                   EXSR      SETFIL
044700000000     C*
044800000000     C* OPERAZIONI NUOVO MAGAZZINO
044900000000     C                   EXSR      NEWMAG
045000000000     C*--------------------
045100000000     C* FINCHE' ROTTURA MAGAZZINO
045200000000     C*--------------------
045300000000DO  3C     $FINE         DOWEQ     '0'
045400000000     C*
045500000000     C* OPERAZIONI NUOVO TIPO UNITA'
045600000000     C                   EXSR      NEWTIP
045700000000     C*--------------------
045800000000     C* FINCHE' ROTTURA TIPO UNITA'
045900000000     C*--------------------
046000000000DO  4C     $FINE         DOWEQ     '0'
046100000000     C     DEPTIP        ANDEQ     UNATIP
046200000000     C*
046300950125     C* PASSAGGIO MEMORIZZAZIONE UNITA' PER MAGAZZINO E PER FILIALE ELABORATORE
046400000000     C                   MOVEL     '1'           PASMAG
046500950125     C                   MOVEL     '1'           PASFEL
046600000000     C* CONTEGGIO UNITA'
046700000000     C                   ADD       1             TOTTIP                         *PER TIPO
046800000000     C                   ADD       1             TOTMAG                         *PER MAGAZZINO
046900950125     C                   ADD       1             TOTFEL                         *PER FIL. ELABORATOR
047000000000     C                   ADD       1             TOTGEN                         *GNENERALE
047100000000IF  5C     *IN01         IFEQ      *ON
047200000000     C*
047300971022     C* FINE PAGINA, STAMPA TESTATA
047400000000IF  6C     PARSOD        IFEQ      'S'                                          *INTESTAZ. SINTETICA
047500000000     C                   MOVEL     ISS(3)        WDES
047600000000     C                   MOVEL     *BLANKS       WCOD
047700000000E   6C                   ENDIF
047800000000     C                   EXSR      STATES
047900950126     C                   SETOFF                                       01
048000000000E   5C                   ENDIF
048100000000     C*
048200000000     C* SE RICHIESTO STAMPO RIGHE DI DETTAGLIO
048300000000IF  5C     PARSOD        IFEQ      'D'
048400000000     C                   EXSR      STADET                                       *STAMPA DETTAGLIO
048500000000E   5C                   ENDIF
048600000000     C*
048700000000     C* LETTURA SUCESSIVA UNITA' EDP
048800000000     C                   EXSR      LETFIL
048900000000     C*
049000000000E   4C                   ENDDO
049100000000     C*--------------------
049200000000     C* TOTALE  T I P O
049300000000     C*--------------------
049400950331IF  4C     PARTOT        IFEQ      'S'                                          *SI TOTALI
049500950331     C     PARTOT        OREQ      'N'                                          *NO TOTALI
049600950331     C     PARTIP        ANDEQ     *ZEROS                                       *MA NESSUNA SELEZION
049700950331     C     PARMAG        ORNE      *ZEROS                                       *SELEZ.SU ALTRO TOTA
049800950331     C     PARTOT        ANDEQ     'N'                                          *NO TOTALI
049900950331     C*
050000950331IF  5C     PARSOD        IFEQ      'D'
050100941227     C                   WRITE     SDU7RI                                       *RIGA
050200950331E   5C                   ENDIF
050300000000     C*
050400000000     C* PER SINTETICA O ROTTURA
050500000000     C                   MOVEL     TOT(1)        DESTOT
050600000000     C                   MOVEL     *BLANKS       CODTO1
050700000000     C                   MOVEL     DEPTIP        CODTO1
050800000000     C                   MOVEL     *BLANKS       CODTO2
050900000000     C                   MOVEL     DESTIP        CODTO2
051000000000     C                   Z-ADD     TOTTIP        TOTALE
051100000000     C                   EXSR      STATOT
051200000000     C*
051300950331IF  5C     PARSOD        IFEQ      'D'
051400000000     C                   WRITE     SDU7RI
051500950331E   5C                   ENDIF
051600950331E   4C                   ENDIF
051700000000     C*
051800000000E   3C                   ENDDO
051900000000     C*--------------------
052000000000     C* TOTALE  M A G A Z Z I N O
052100000000     C*--------------------
052200950331IF  3C     PARTOT        IFEQ      'S'                                          *SI TOTALI
052300950331     C     PARTOT        OREQ      'N'                                          *NO TOTALI
052400950331     C     PARMAG        ANDEQ     *ZEROS                                       *MA NESSUNA SELEZION
052500950331     C     PARTIP        ORNE      *ZEROS                                       *SELEZ.SU ALTRO TOTA
052600950331     C     PARTOT        ANDEQ     'N'                                          *NO TOTALI
052700000000     C*
052800000000     C* STAMPO TOTALE MAGAZZINO SOLO SE HO MEMORIZZATO QC
052900950331IF  4C     PASMAG        IFEQ      '1'
053000000000     C*
053100950331IF  5C     PARSOD        IFEQ      'S'
053200000000     C                   WRITE     SDU7RI
053300950331E   5C                   ENDIF
053400000000     C*
053500000000     C                   MOVEL     TOT(2)        DESTOT
053600000000     C                   MOVEL     *BLANKS       CODTO1
053700000000     C                   MOVEL     FIL(F)        CODTO1
053800000000     C                   MOVEL     *BLANKS       CODTO2
053900000000     C                   MOVEL     DESMAG        CODTO2
054000000000     C                   Z-ADD     TOTMAG        TOTALE
054100000000     C                   EXSR      STATOT
054200000000     C*
054300000000     C* DETTAGLIO: DOPO TOTALE STAMPO RIGA TRATTINI E SALTO PAGINA
054400000000     C* SINTETICA: DOPO TOTALE STAMPO SOLO RIGA TRATTINI
054500941227     C                   WRITE     SDU7RI                                       *TRATTINI
054600950331IF  5C     PARSOD        IFEQ      'D'
054700090907IF  6C     PARFOS        IFEQ      'F'                                          *SOLO PER FILIALE
054800971022     C                   SETON                                        01
054900971022E   6C                   ENDIF
055000971022E   5C                   ENDIF
055100950331E   4C                   ENDIF
055200950331E   3C                   ENDIF
055300000000     C*
055400000000     C* LETTURA PROSSIMO MAGAZZINO
055500000000     C                   ADD       1             F
055600950125     C                   Z-ADD     FEL(F)        WFEL
055700000000     C*
055800000000     C* SE SELEZIONATO UN SOLO MAGAZZINO, ESCO DAL CICLO
055900000000IF  3C     PARMAG        IFGT      *ZEROS
056000000000     C     NUMFIL        ADD       1             F
056100000000E   3C                   ENDIF
056200000000     C*
056300000000E   2C                   ENDDO
056400000000     C*--------------------
056500000000     C* TOTALE   A R E A
056600000000     C*--------------------
056700950331IF  2C     PARTOT        IFEQ      'S'                                          *SI TOTALI
056800000000     C*
056900950125     C* STAMPO TOTALE FILIALE ELABORATORE SOLO SE HO MEMORIZZATO QC
057000950331IF  3C     PASFEL        IFEQ      '1'
057100000000     C                   MOVEL     TOT(3)        DESTOT
057200000000     C                   MOVEL     *BLANKS       CODTO1
057300950125     C                   MOVEL     DEPFEL        CODTO1
057400000000     C                   MOVEL     *BLANKS       CODTO2
057500950125     C                   MOVEL     DESFEL        CODTO2
057600950125     C                   Z-ADD     TOTFEL        TOTALE
057700000000     C                   EXSR      STATOT
057800000000     C*
057900000000     C* DOPO TOTALE STAMPO RIGA TRATTINI E SALTO PAGINA
058000000000     C                   WRITE     SDU7RI
058100090907IF  4C     PARFOS        IFEQ      'F'                                          *SOLO PER FILIALE
058200971022     C                   SETON                                        01
058300971022E   4C                   ENDIF
058400971022E   3C                   ENDIF
058500950331E   2C                   ENDIF
058600000000     C*
058700000000E   1C                   ENDDO
058800000000     C*--------------------
058900000000     C* TOTALE  G E N E R A L E
059000000000     C*--------------------
059100950331IF  1C     PARTOT        IFEQ      'S'                                          *SI TOTALI
059200000000     C                   MOVEL     TOT(2)        DESTOT
059300000000     C                   MOVEL     *BLANKS       CODTO1
059400000000     C                   MOVEL     TOT(7)        CODTO1
059500000000     C                   MOVEL     *BLANKS       CODTO2
059600000000     C                   Z-ADD     TOTGEN        TOTALE
059700000000     C                   EXSR      STATOT
059800000000     C*
059900000000     C                   WRITE     SDU7RI                                       *RIGA
060000950331E   1C                   ENDIF
060100950331     C*---
060200950331     C* NB. I TOTALI LI STAMPA LO STESSO (ANCHE SE RICHIESTO "NO") SE NON SONO
060300950331     C*     STATE FATTE DELLE PARZIALIZZAZIONI PER TIPO UNITA'
060400950331     C*---
060500000000     C* CHIUSURA FILE
060600000000     C                   CLOSE     UNANA03L
060700950331     C*
060800000000     C                   ENDSR
060900000000     C*------------------------------------------------------------------------*
061000950126     C* ELORD4 - ELABORAZIONE UNITA' EDP PER RICHIESTA 4° ORDINAMENTO
061100000000     C*------------------------------------------------------------------------*
061200000000     C     ELORD4        BEGSR
061300000000     C*
061400000000     C* APERTURA FILE
061500000000     C                   OPEN      UNANA04L
061600000000     C*
061700000000     C* POSIZIONAMENTO E PRIMA LETTURA
061800000000     C                   EXSR      SETFIL
061900000000     C*-------------------
062000000000     C* FINCHE' FINE FILE
062100000000     C*-------------------
062200000000DO  1C     $FINE         DOWEQ     '0'
062300000000     C*
062400000000     C* OPERAZIONI PER NUOVA DATA SCADENZA
062500000000     C                   EXSR      NEWDSM
062600000000     C*-------------------
062700000000     C* FINCHE' ROTTURA DATA SCADENZA
062800000000     C*-------------------
062900000000DO  2C     $FINE         DOWEQ     '0'
063000000000     C     DEPDSM        ANDEQ     UNADSM
063100000000     C*
063200000000     C* CONTEGGIO UNITA' PER DATA SCADENZA
063300000000     C                   ADD       1             TOTDSM                         *TOTALE DATA SCADENZ
063400000000     C                   ADD       1             TOTGEN                         *TOTALE GENERALE
063500000000     C*
063600000000     C* FINE PAGINA
063700000000IF  3C     *IN01         IFEQ      *ON
063800000000     C*
063900000000     C* STAMPO TESTATA
064000000000IF  4C     PARSOD        IFEQ      'S'                                          *INTESTAZ. SINTETICA
064100000000     C                   MOVEL     ISS(4)        WDES
064200000000     C                   MOVEL     *BLANKS       WCOD
064300000000E   4C                   ENDIF
064400000000     C*
064500000000     C                   EXSR      STATES
064600950126     C                   SETOFF                                       01
064700000000E   3C                   ENDIF
064800000000     C*
064900000000     C* SE RICHIESTO STAMPO RIGHE DI DETTAGLIO
065000000000IF  3C     PARSOD        IFEQ      'D'
065100000000     C                   EXSR      STADET                                       *STAMPA DETTAGLIO
065200000000E   3C                   ENDIF
065300000000     C*
065400000000     C* LETTURA SUCESSIVA UNITA' EDP
065500000000     C                   EXSR      LETFIL
065600000000E   2C                   ENDDO
065700000000     C*-------------------
065800000000     C* TOTALE   D A T A
065900000000     C*-------------------
066000950331IF  2C     PARTOT        IFEQ      'S'
066100000000     C* RIGA
066200950331IF  3C     PARSOD        IFEQ      'D'
066300000000     C                   WRITE     SDU7RI                                       *RIGA
066400950331E   3C                   ENDIF
066500000000     C*
066600000000     C* PER SINTETICA O PER ROTTURA
066700000000     C                   MOVEL     TOT(4)        DESTOT
066800000000     C                   MOVEL     *BLANKS       CODTO1           15
066900000000     C                   MOVEL     DESDSM        CODTO1
067000000000     C                   MOVEL     *BLANKS       CODTO2
067100000000     C                   Z-ADD     TOTDSM        TOTALE
067200000000     C                   EXSR      STATOT
067300000000     C*
067400000000     C* DETTAGLIO: DOPO TOTALE STAMPO RIGA TRATTINI E SALTO PAGINA
067500950331IF  3C     PARSOD        IFEQ      'D'
067600000000     C                   WRITE     SDU7RI                                       *RIGA
067700090907IF  4C     PARFOS        IFEQ      'F'                                          *SOLO PER FILIALE
067800971022     C                   SETON                                        01        *SALTO PAGINA
067900971022E   4C                   ENDIF
068000971022E   3C                   ENDIF
068100950331E   2C                   ENDIF
068200000000     C*
068300000000E   1C                   ENDDO
068400000000     C*-------------------
068500000000     C* TOTALE  G E N E R A L E
068600000000     C*-------------------
068700950331IF  1C     PARTOT        IFEQ      'S'
068800950331     C*
068900950331IF  2C     PARSOD        IFEQ      'S'
069000000000     C                   WRITE     SDU7RI                                       *RIGA PER SINTETICA
069100950331E   2C                   ENDIF
069200000000     C*
069300000000     C                   MOVEL     TOT(4)        DESTOT
069400000000     C                   MOVEL     *BLANKS       CODTO1
069500000000     C                   MOVEL     TOT(7)        CODTO1
069600000000     C                   MOVEL     *BLANKS       CODTO2
069700950331IF  2C     DECDSM        IFGT      *ZEROS
069800000000     C                   Z-ADD     DECDSM        $DAT2
069900000000     C                   EXSR      RICDAT
070000000000     C                   MOVEL     $DAT3         CODTO2                         *DATA INIZIALE
070100000000     C*
070200000000     C                   Z-ADD     DECDS2        $DAT2
070300000000     C                   EXSR      RICDAT
070400000000     C                   MOVE      $DAT3         CODTO2                         *DATA FINALE
070500950331E   2C                   ENDIF
070600000000     C                   Z-ADD     TOTGEN        TOTALE
070700000000     C                   EXSR      STATOT
070800000000     C*
070900000000     C                   WRITE     SDU7RI                                       *RIGA
071000950331E   1C                   ENDIF
071100000000     C* CHIUSURA FILE
071200000000     C                   CLOSE     UNANA04L
071300950331     C*---
071400950331     C* NB. STAMPA I TOTALI SOLO SE RICHIESTI
071500950331     C*---
071600000000     C                   ENDSR
071700000000     C*------------------------------------------------------------------------*
071800950126     C* ELORD5 - ELABORAZIONE UNITA' EDP PER RICHIESTA 5° ORDINAMENTO
071900000000     C*------------------------------------------------------------------------*
072000000000     C     ELORD5        BEGSR
072100000000     C*
072200000000     C* APERTURA FILE
072300000000     C                   OPEN      UNANA05L
072400000000     C*
072500000000     C* POSIZIONAMENTO E PRIMA LETTURA
072600000000     C                   EXSR      SETFIL
072700000000     C*-------------------
072800000000     C* FINCHE' FINE FILE
072900000000     C*-------------------
073000000000DO  1C     $FINE         DOWEQ     '0'
073100000000     C*
073200000000     C* OPERAZIONI PER NUOVO TIPO UNITA'
073300000000     C                   EXSR      NEWTIP
073400000000     C*-------------------
073500000000     C* FINCHE' ROTTURA TIPO UNITA'
073600000000     C*-------------------
073700000000DO  2C     $FINE         DOWEQ     '0'
073800000000     C     DEPTIP        ANDEQ     UNATIP
073900000000     C*
074000000000     C* OPERAZIONI PER NUOVA MARCA
074100000000     C                   EXSR      NEWMAR
074200000000     C*-------------------
074300000000     C* FINCHE' ROTTURA MARCA
074400000000     C*-------------------
074500000000DO  3C     $FINE         DOWEQ     '0'
074600000000     C     DEPTIP        ANDEQ     UNATIP
074700000000     C     DEPMAR        ANDEQ     UNAMAR
074800000000     C*
074900000000     C* OPERAZIONI PER NUOVO MODELLO
075000000000     C                   EXSR      NEWMOD
075100000000     C*-------------------
075200000000     C* FINCHE' ROTTURA MODELLO
075300000000     C*-------------------
075400000000DO  4C     $FINE         DOWEQ     '0'
075500000000     C     DEPTIP        ANDEQ     UNATIP
075600000000     C     DEPMAR        ANDEQ     UNAMAR
075700000000     C     DEPMOD        ANDEQ     UNAMOD
075800000000     C*
075900000000     C* CONTEGGIO UNITA' PER TIPO
076000000000     C                   ADD       1             TOTMAR                         *TOTALE MARCA
076100000000     C                   ADD       1             TOTTIP                         *TOTALE TIPO
076200000000     C                   ADD       1             TOTGEN                         *TOTALE GENERALE
076300000000     C*
076400000000     C* FINE PAGINA
076500000000IF  5C     *IN01         IFEQ      *ON
076600000000     C*
076700000000     C* STAMPO TESTATA
076800000000IF  6C     PARSOD        IFEQ      'S'                                          *INTESTAZ. SINTETICA
076900000000     C                   MOVEL     ISS(5)        WDES
077000000000     C                   MOVEL     *BLANKS       WCOD
077100000000     C                   MOVEL     TOT(1)        WCOD
077200000000     C                   MOVE      DEPTIP        WCOD
077300000000E   6C                   ENDIF
077400000000     C*
077500000000     C                   EXSR      STATES
077600950126     C                   SETOFF                                       01
077700000000E   5C                   ENDIF
077800000000     C*
077900000000     C* SE RICHIESTO STAMPO RIGHE DI DETTAGLIO
078000000000IF  5C     PARSOD        IFEQ      'D'
078100000000     C                   EXSR      STADET                                       *STAMPA DETTAGLIO
078200000000E   5C                   ENDIF
078300000000     C*
078400000000     C* LETTURA SUCESSIVA UNITA' EDP
078500000000     C                   EXSR      LETFIL
078600000000E   4C                   ENDDO
078700000000E   3C                   ENDDO
078800000000     C*-------------------
078900000000     C* TOTALE  M A R C A
079000000000     C*-------------------
079100950331IF  3C     PARTOT        IFEQ      'S'
079200950331IF  4C     PARSOD        IFEQ      'D'
079300000000     C                   WRITE     SDU7RI                                       *RIGA
079400950331E   4C                   ENDIF
079500000000     C*
079600000000     C* PER SINTETICA O PER ROTTURA
079700000000     C                   MOVEL     TOT(5)        DESTOT
079800000000     C                   MOVEL     *BLANKS       CODTO1
079900000000     C                   MOVEL     DEPMAR        CODTO1
080000000000     C                   MOVEL     *BLANKS       CODTO2
080100000000     C                   Z-ADD     TOTMAR        TOTALE
080200000000     C                   EXSR      STATOT
080300000000     C*
080400000000     C* DETTAGLIO: DOPO TOTALE STAMPO RIGA TRATTINI E SALTO PAGINA
080500950331IF  4C     PARSOD        IFEQ      'D'
080600000000     C                   WRITE     SDU7RI                                       *RIGA
080700950331E   4C                   ENDIF
080800950331E   3C                   ENDIF
080900000000     C*
081000000000E   2C                   ENDDO
081100000000     C*-------------------
081200000000     C* TOTALE  T I P O
081300000000     C*-------------------
081400950331IF  2C     PARTOT        IFEQ      'S'
081500950331IF  3C     PARSOD        IFEQ      'S'
081600000000     C                   WRITE     SDU7RI                                       *RIGA
081700950331E   3C                   ENDIF
081800000000     C*
081900000000     C* PER SINTETICA O PER ROTTURA
082000000000     C                   MOVEL     TOT(1)        DESTOT
082100000000     C                   MOVEL     *BLANKS       CODTO1
082200000000     C                   MOVEL     DEPTIP        CODTO1
082300000000     C                   MOVEL     *BLANKS       CODTO2
082400000000     C                   MOVEL     DESTIP        CODTO2
082500000000     C                   Z-ADD     TOTTIP        TOTALE
082600000000     C                   EXSR      STATOT
082700000000     C*
082800000000     C* DOPO TOTALE STAMPO RIGA TRATTINI E SALTO PAGINA
082900000000     C                   WRITE     SDU7RI                                       *RIGA
083000090907IF  3C     PARFOS        IFEQ      'F'                                          *SOLO PER FILIALE
083100971022     C                   SETON                                        01        *SALTO PAGINA
083200971022E   3C                   ENDIF
083300971022E   2C                   ENDIF
083400000000     C*
083500000000E   1C                   ENDDO
083600000000     C*-------------------
083700000000     C* TOTALE  G E N E R A L E
083800000000     C*-------------------
083900950331IF  1C     PARTOT        IFEQ      'S'
084000000000     C                   MOVEL     TOT(1)        DESTOT
084100000000     C                   MOVEL     *BLANKS       CODTO1
084200000000     C                   MOVEL     TOT(7)        CODTO1
084300000000     C                   MOVEL     *BLANKS       CODTO2
084400000000     C                   Z-ADD     TOTGEN        TOTALE
084500000000     C                   EXSR      STATOT
084600000000     C*
084700000000     C                   WRITE     SDU7RI                                       *RIGA
084800950331E   1C                   ENDIF
084900000000     C* CHIUSURA FILE
085000000000     C                   CLOSE     UNANA05L
085100000000     C*
085200000000     C                   ENDSR
085300000000     C*------------------------------------------------------------------------*
085400950126     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA UNITA' EDP
085500000000     C*------------------------------------------------------------------------*
085600000000     C     SETFIL        BEGSR
085700000000     C*
085800000000     C* POSIZIONAMENTO 1° ORDINAMENTO
085900000000IF  1C     PARORD        IFEQ      1
086000000000     C                   Z-ADD     PARTIP        KUNTIP                         *RICHIESTA VIDEO
086100000000     C     KEYUN1        SETLL     UNANA01L                                     *FINE ARCHIVIO
086200000000E   1C                   ENDIF
086300000000     C*
086400000000     C* POSIZIONAMENTO 2° ORDINAMENTO
086500000000IF  1C     PARORD        IFEQ      2
086600000000     C                   Z-ADD     *ZEROS        KUNTIP                         *INIZIO FILE
086700000000     C                   MOVEL     *BLANKS       KUNMAT
086800000000     C     KEYUN2        SETLL     UNANA02L                                     *FINE ARCHIVIO
086900000000E   1C                   ENDIF
087000000000     C*
087100000000     C* POSIZIONAMENTO 3° ORDINAMENTO
087200000000IF  1C     PARORD        IFEQ      3
087300000000     C                   Z-ADD     FIL(F)        KUNMAG
087400000000     C                   Z-ADD     PARTIP        KUNTIP                         *RICHIESTA VIDEO
087500000000     C     KEYUN3        SETLL     UNANA03L                                     *FINE ARCHIVIO
087600000000E   1C                   ENDIF
087700000000     C*
087800000000     C* POSIZIONAMENTO 4° ORDINAMENTO
087900000000IF  1C     PARORD        IFEQ      4
088000000000     C                   Z-ADD     PARDSM        KUNDSM                         *DATA INIZIO
088100000000     C     KEYUN4        SETLL     UNANA04L                                     *FINE ARCHIVIO
088200000000E   1C                   ENDIF
088300000000     C*
088400000000     C* POSIZIONAMENTO 5° ORDINAMENTO
088500000000IF  1C     PARORD        IFEQ      5
088600000000     C                   Z-ADD     PARTIP        KUNTIP
088700000000     C                   MOVEL     PARMAR        KUNMAR
088800000000     C                   MOVEL     PARMOD        KUNMOD
088900000000     C     KEYUN5        SETLL     UNANA05L                                     *FINE ARCHIVIO
089000000000E   1C                   ENDIF
089100000000     C*
089200000000     C* P R I M A   L E T T U R A
089300000000     C*
089400000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
089500000000     C*
089600000000     C                   ENDSR
089700000000     C*------------------------------------------------------------------------*
089800950126     C* LETFIL - LETTURA PROSSIMO RECORD UNITA' EDP
089900000000     C*------------------------------------------------------------------------*
090000000000     C     LETFIL        BEGSR
090100000000     C*
090200000000     C* LEGGO FINO A:
090300000000     C*     - FINE FILE
090400000000     C*     - TROVATO RECORD VALIDO
090500000000     C                   MOVEL     'N'           WRECOK
090600000000     C*
090700000000DO  1C     $FINE         DOWEQ     '0'
090800000000     C     WRECOK        ANDEQ     'N'
090900000000     C*
091000000000     C* LETTURA 1° ORDINAMENTO
091100000000IF  1C     PARORD        IFEQ      1
091200000000     C                   READ      UNANA01L                               99
091300000000E   1C                   ENDIF
091400000000     C*
091500000000     C* LETTURA 2° ORDINAMENTO
091600000000IF  1C     PARORD        IFEQ      2
091700000000     C                   READ      UNANA02L                               99
091800000000E   1C                   ENDIF
091900000000     C*
092000000000     C* LETTURA 3° ORDINAMENTO
092100000000IF  1C     PARORD        IFEQ      3
092200000000     C     KUNMAG        READE     UNANA03L                               99
092300000000E   1C                   ENDIF
092400000000     C*
092500000000     C* LETTURA 4° ORDINAMENTO
092600000000IF  1C     PARORD        IFEQ      4
092700000000     C                   READ      UNANA04L                               99
092800000000E   1C                   ENDIF
092900000000     C*
093000000000     C* LETTURA 5° ORDINAMENTO
093100000000IF  1C     PARORD        IFEQ      5
093200000000     C                   READ      UNANA05L                               99
093300000000E   1C                   ENDIF
093400000000     C*
093500000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
093600000000     C                   MOVEL     '1'           $FINE
093700000000X   2C                   ELSE
093800000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
093900000000E   2C                   ENDIF
094000000000     C*
094100000000E   1C                   ENDDO
094200000000     C*
094300000000     C                   ENDSR
094400000000     C*------------------------------------------------------------------------*
094500950126     C* CHKREC - CONTROLLA VALIDITA' RECORD UNITA' EDP
094600000000     C*------------------------------------------------------------------------*
094700000000     C     CHKREC        BEGSR
094800000000     C*
094900000000     C* °I CAMPI SELEZIONATI (TIPO, DATA, MARCA, MODELLO) PER I QUALI E' STATO
095000000000     C*  RICHIESTO ANCHE L'ORDINAMENTO, SE RISULTANO DIVERSI DAL CAMPO RECORD
095100000000     C*  FANNO USCIRE DAL CICLO.
095200000000     C*
095300000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
095400950407     C*
095500950407     C* UNITA' ANNULLATA ESCLUDO
095600950407IF  1C     UNAATB        IFNE      *BLANKS
095700950407     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
095800950407E   1C                   ENDIF
095900000000     C*
096000000000     C* TIPO UNITA' DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
096100000000IF  1C     PARTIP        IFGT      *ZEROS
096200000000     C     PARTIP        ANDNE     UNATIP
096300000000IF  2C     PARORD        IFEQ      1                                            *ORDINAMENTO X TIPO
096400000000     C     PARORD        OREQ      5                                            *ORD X TIPO/MARCA ..
096500000000     C                   MOVEL     '1'           $FINE
096600000000X   2C                   ELSE
096700000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
096800000000E   2C                   ENDIF
096900000000E   1C                   ENDIF
097000000000     C*
097100000000     C* MAGAZZINO UBICAZIONE DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
097200000000IF  1C     PARMAG        IFGT      *ZEROS
097300000000     C     PARMAG        ANDNE     UNAMAG
097400000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
097500000000E   1C                   ENDIF
097600020513
097700020513      * controllo il cliente se richiesto magazzino 951 con ordinamento di tipo '3' e utente con
097800020513      * abilitazione diversa da 'AZ'
097900070206      * anche per ordinamento di tipo '2'
098000070206     c                   if        parmag = 951 and wabi <> 'AZ' and
098100070206     c                             (parord = 3 or parord = 2)
098200020513     c                   movel     unaksc        wpocli
098300020513     c     wpocli        lookup    skpoabi                                28
098400020513     c                   if        not *in28
098500020513     c                   eval      wrecok = 'N'
098600020513     c                   endif
098700020513     c                   endif
098800020328
098900000000     C*
099000000000     C* DATA SCADENZA MANUTENZIONE NON COMPRESA NEL RANGE, ESCLUDO
099100000000IF  1C     PARDSM        IFGT      *ZEROS
099200000000IF  2C     UNADSM        IFGE      PARDSM
099300000000     C     UNADSM        ANDLE     PARDS2
099400000000X   2C                   ELSE
099500000000IF  3C     PARORD        IFEQ      4                                            *ORDINAMENTO X TIPO
099600000000     C                   MOVEL     '1'           $FINE
099700000000X   3C                   ELSE
099800000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
099900000000E   3C                   ENDIF
100000000000E   2C                   ENDIF
100100000000E   1C                   ENDIF
100200000000     C*
100300000000     C* MARCA DIVERSA DA QUELLA SELEZIONATA, ESCLUDO
100400000000IF  1C     PARMAR        IFGT      *BLANKS
100500000000     C     PARMAR        ANDNE     UNAMAR
100600000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
100700000000E   1C                   ENDIF
100800000000     C*
100900000000     C* MODELLO DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
101000000000IF  1C     PARMOD        IFGT      *BLANKS
101100000000     C     PARMOD        ANDNE     UNAMOD
101200000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
101300000000E   1C                   ENDIF
101400000000     C*
101500000000     C* TIPO FILIALE DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
101600000000     C                   Z-ADD     1             I
101700950126     C                   SETOFF                                       28
101800000000     C     UNAMAG        LOOKUP    FIL(I)                                 28
101900000000IF  1C     *IN28         IFEQ      *OFF
102000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
102100000000E   1C                   ENDIF
102200000000     C*
102300000000     C                   ENDSR
102400000000     C*------------------------------------------------------------------------*
102500950126     C* NEWTIP - OPERAZIONI PER NUOVO TIPO UNITA'
102600000000     C*------------------------------------------------------------------------*
102700000000     C     NEWTIP        BEGSR
102800000000     C*
102900000000     C* MEMORIZZA IL NUOVO TIPO UNITA' IN UN DEPOSITO
103000000000     C                   Z-ADD     UNATIP        DEPTIP
103100000000     C*
103200000000     C* DECODIFICA IL TIPO UNITA'
103300000000     C                   Z-ADD     1             I
103400950126     C                   SETOFF                                       28
103500000000     C     DEPTIP        LOOKUP    TIP(I)                                 28
103600000000IF  1C     *IN28         IFEQ      *ON
103700000000     C                   MOVEL     DTIP(I)       DESTIP           20
103800000000X   1C                   ELSE
103900000000     C                   MOVEL     *ALL'*'       DESTIP
104000000000E   1C                   ENDIF
104100000000     C*
104200000000     C* AZZERA IL TOTALE TIPO UNITA'
104300000000     C                   Z-ADD     *ZEROS        TOTTIP            5 0
104400000000     C*
104500000000     C                   ENDSR
104600000000     C*------------------------------------------------------------------------*
104700950126     C* NEWFEL - OPERAZIONI PER NUOVA FILALE ELABORATORE
104800000000     C*------------------------------------------------------------------------*
104900950125     C     NEWFEL        BEGSR
105000000000     C*
105100950125     C* REPERISCO LA FILAILE ELABORATORE DEL MAGAZZINO
105200950125     C                   Z-ADD     FEL(F)        WFEL
105300000000     C*
105400950125     C* MEMORIZZA LA NUOVA FILIALE ELABORATORE IN UN DEPOSITO
105500950125     C                   Z-ADD     WFEL          DEPFEL
105600000000     C*
105700950125     C* DECODIFICA LA FILIALE ELABORATORE
105800951103     C                   MOVEL     *BLANKS       DESFEL           20
105900951103     C                   Z-ADD     1             I
106000951103     C                   SETOFF                                       28
106100951103     C     DEPFEL        LOOKUP    FIL(I)                                 28
106200951103IF  1C     *IN28         IFEQ      *OFF
106300951103     C                   MOVEL     'VARIE'       DESFEL
106400951103X   1C                   ELSE
106500951103     C                   MOVEL     DFIL(I)       DESFEL
106600951103E   1C                   ENDIF
106700000000     C*
106800950125     C* AZZERA IL TOTALE FILIALE ELABORATORE MAGAZZINO
106900950125     C                   Z-ADD     *ZEROS        TOTFEL            5 0
107000000000     C*
107100950125     C* REIMPOSTO FLAG MEMORIZZAZIONE UNITA' PER FILIALE ELABORATORE
107200950125     C                   MOVEL     '0'           PASFEL
107300000000     C*
107400000000     C                   ENDSR
107500000000     C*------------------------------------------------------------------------*
107600950126     C* NEWMAG - OPERAZIONI PER NUOVO MAGAZZINO UBICAZIONE
107700000000     C*------------------------------------------------------------------------*
107800000000     C     NEWMAG        BEGSR
107900000000     C*
108000000000     C* DECODIFICA IL MAGAZZINO
108100000000     C                   MOVEL     DFIL(F)       DESMAG           20
108200000000     C*
108300000000     C* AZZERA IL TOTALE MAGAZZINO
108400000000     C                   Z-ADD     *ZEROS        TOTMAG            5 0
108500000000     C*
108600000000     C* REIMPOSTO FLAG MEMORIZZAZIONE UNITA' PER MAGAZZINO
108700000000     C                   MOVEL     '0'           PASMAG
108800000000     C*
108900000000     C                   ENDSR
109000000000     C*------------------------------------------------------------------------*
109100950126     C* NEWDSM - OPERAZIONI PER NUOVO TIPO UNITA'
109200000000     C*------------------------------------------------------------------------*
109300000000     C     NEWDSM        BEGSR
109400000000     C*
109500000000     C* MEMORIZZA LA NUOVA DATA SCADENZA IN UN DEPOSITO
109600000000     C                   Z-ADD     UNADSM        DEPDSM
109700000000     C*
109800000000     C* SE LA DATA E' VUOTA IN DESCRIZIONE METTO 'NESSUNA'
109900000000IF  1C     DEPDSM        IFEQ      *ZEROS
110000971013     C                   MOVEL(P)  'NESSUNA'     DESDSM
110100971013     C*
110200971013     C* SE PIENA, ROVESCIA LA DATA IN G/M/A E LA RICOSTRUISCE CON ' /  /  '
110300000000X   1C                   ELSE
110400971013     C                   Z-ADD     UNADSM        G08INV
110500971013     C                   Z-ADD     *ZEROS        G08DAT
110600971013     C                   MOVEL     '3'           G08ERR
110700971013     C                   CALL      'XSRDA8'
110800971013     C                   PARM                    WLBDA8
110900971013     C                   Z-ADD     G08DAT        $DAT2             8 0
111000000000     C                   EXSR      RICDAT
111100971013     C                   MOVEL(P)  $DAT3         DESDSM           10
111200000000E   1C                   ENDIF
111300000000     C*
111400000000     C* AZZERA IL TOTALE DATA SCADENZA MANUTENZIONE
111500000000     C                   Z-ADD     *ZEROS        TOTDSM            5 0
111600000000     C*
111700000000     C                   ENDSR
111800000000     C*------------------------------------------------------------------------*
111900950126     C* NEWMAR - OPERAZIONI PER NUOVA MARCA
112000000000     C*------------------------------------------------------------------------*
112100000000     C     NEWMAR        BEGSR
112200000000     C*
112300000000     C* MEMORIZZA LA NUOVA MARCA IN UN DEPOSITO
112400000000     C                   MOVEL     UNAMAR        DEPMAR
112500000000     C*
112600000000     C* AZZERA IL TOTALE MARCA
112700000000     C                   Z-ADD     *ZEROS        TOTMAR            5 0
112800000000     C*
112900000000     C                   ENDSR
113000000000     C*------------------------------------------------------------------------*
113100950126     C* NEWMOD - OPERAZIONI PER NUOVO MODELLO
113200000000     C*------------------------------------------------------------------------*
113300000000     C     NEWMOD        BEGSR
113400000000     C*
113500000000     C* MEMORIZZA IL NUOVO MODELLO IN UN DEPOSITO
113600000000     C                   MOVEL     UNAMOD        DEPMOD
113700000000     C*
113800000000     C                   ENDSR
113900000000     C*------------------------------------------------------------------------*
114000950126     C* STATES - STAMPA TESTATA
114100000000     C*          INPUT --> WDES: DESCRIZIONE INTESTAZIONE
114200000000     C*                    WCOD: CODICE INTESTAZIONE
114300000000     C*------------------------------------------------------------------------*
114400000000     C     STATES        BEGSR
114500000000     C*
114600941227     C                   SETOFF                                       01
114700941227     C*
114800000000     C                   WRITE     SDU7PT                                       *TITOLO
114900000000     C                   WRITE     SDU7RI                                       *RIGA
115000000000     C*
115100000000     C* STAMPA IN DETTAGLIO
115200000000IF  1C     PARSOD        IFEQ      'D'
115300000000     C                   WRITE     SDU7PI                                       *DESCRIZIONE CAMPI
115400000000IF  2C     PARTEC        IFEQ      'S'
115500000000     C                   WRITE     SDU7PI2                                      *DESCR CAMPI TECNICI
115600000000E   2C                   ENDIF
115700000000     C*
115800000000     C* STAMPA SINTETICA
115900000000X   1C                   ELSE
116000000000     C                   CLEAR                   SDU7PI3
116100000000     C                   MOVEL     WDES          PI3DES
116200000000     C                   MOVEL     WCOD          PI3COD
116300000000     C                   WRITE     SDU7PI3                                      *DESCRIZIONE CAMPI
116400000000E   1C                   ENDIF
116500000000     C*
116600000000     C                   WRITE     SDU7RI                                       *RIGA
116700000000     C*
116800000000     C                   ENDSR
116900000000     C*------------------------------------------------------------------------*
117000950126     C* STADET - STAMPA RIGHE DI DETTAGLIO
117100000000     C*------------------------------------------------------------------------*
117200000000     C     STADET        BEGSR
117300950126     C*
117400950126     C* FINE PAGINA: STATMPA TESTATA
117500950126IF  1C     *IN01         IFEQ      *ON
117600950126     C                   EXSR      STATES
117700950126     C                   SETOFF                                       01
117800950126E   1C                   ENDIF
117900000000     C*
118000000000     C* MUOVO I CAMPI DEL FILE IN QUELLI DI STAMPA
118100000000     C                   Z-ADD     UNATIP        PRCTIP
118200000000     C*
118300000000     C* DECODIFICO TIPO UNITA'
118400000000     C                   Z-ADD     1             I
118500950126     C                   SETOFF                                       28
118600000000     C     UNATIP        LOOKUP    TIP(I)                                 28
118700000000IF  1C     *IN28         IFEQ      *ON
118800000000     C                   MOVEL     DTIP(I)       PRDTIP
118900000000X   1C                   ELSE
119000000000     C                   MOVEL     *ALL'*'       PRDTIP
119100000000E   1C                   ENDIF
119200020322
119300020322     c                   z-add     unarg2        prcrg2
119400020322      * decodifico raggruppamento
119500020322     c                   if        unarg2 <> *zeros
119600020322     c                   exsr      decrg2
119700020322     c                   endif
119800000000     C*
119900000000     C                   Z-ADD     UNACOD        PRCCOD
120000000000     C                   Z-ADD     UNAMAG        PRCMAG
120100950126     C*
120200000000     C* DECODIFICO MAGAZZINO UBICAZIONE
120300000000     C                   Z-ADD     1             I
120400950126     C                   SETOFF                                       28
120500000000     C     UNAMAG        LOOKUP    FIL(I)                                 28
120600000000IF  1C     *IN28         IFEQ      *ON
120700000000     C                   MOVEL     DFIL(I)       PRDMAG
120800000000X   1C                   ELSE
120900000000     C                   MOVEL     *ALL'*'       PRDMAG
121000000000E   1C                   ENDIF
121100000000     C*
121200000000     C* ROVESCIO IN G/M/A LA DATA SCADENZA MANUTENZIONE
121300971013     C                   Z-ADD     UNADSM        G08INV
121400971013     C                   Z-ADD     *ZEROS        G08DAT
121500971013     C                   MOVEL     '3'           G08ERR
121600971013     C                   CALL      'XSRDA8'
121700971013     C                   PARM                    WLBDA8
121800971013     C                   Z-ADD     G08DAT        PRCDSM
121900000000     C*
122000000000     C                   MOVEL     UNAMAR        PRCMAR
122100000000     C                   MOVEL     UNAMOD        PRCMOD
122200000000     C                   MOVEL     UNAMAT        PRCMAT
122300000000     C                   MOVEL     UNADES        PRCDES
122400020322      * se magazzino 951 metto il cliente e la ragione sociale
122500020322      * nella descrizione
122600020322     c                   if        unamag = 951 and unaksc > *zeros
122700020322     c                   exsr      decksc
122800020322     c                   movel(p)  dsksc         prcdes
122900020322     c                   endif
123000000000     C*
123100000000     C* ROVESCIO IN G/M/A LA DATA SCADENZA GARANZIA
123200971013     C                   Z-ADD     UNADSG        G08INV
123300971013     C                   Z-ADD     *ZEROS        G08DAT
123400971013     C                   MOVEL     '3'           G08ERR
123500971013     C                   CALL      'XSRDA8'
123600971013     C                   PARM                    WLBDA8
123700971013     C                   Z-ADD     G08DAT        PRCDSG
123800000000     C*
123900000000     C* DITTA IN MANUTENZIONE
124000960514IF  1C     PARFOS        IFEQ      'S'
124100950126     C                   Z-ADD     UNADMS        PRCDMN                         *PER LA SEDE
124200000000X   1C                   ELSE
124300000000     C                   Z-ADD     UNADMF        PRCDMN                         *PER LA FILIALE
124400000000E   1C                   ENDIF
124500950126     C* DECODIFICA
124600950126     C                   Z-ADD     1             I
124700950126     C                   SETOFF                                       28
124800950126     C     PRCDMN        LOOKUP    FIL(I)                                 28
124900950126IF  1C     *IN28         IFEQ      *ON
125000950126     C                   MOVEL     DFIL(I)       PRDDMN
125100950126X   1C                   ELSE
125200950126     C                   MOVEL     *ALL'*'       PRDDMN
125300950126E   1C                   ENDIF
125400000000     C*
125500950126     C* DITTA IN LEASING
125600000000     C                   MOVEL     UNALEA        PRCLEA
125700000000     C*
125800000000     C                   WRITE     SDU7PR
125900000000     C*
126000000000     C* SE RICHIESTI DATI TECNICI, STAMPO LA RIGA "TECNICA"
126100000000IF  1C     PARTEC        IFEQ      'S'
126200000000     C*
126300120411     C****               MOVEL     UNAEMU        P2CEMU
126400120411     c                   clear                   p2cemu
126500120412     c                   eval      p2cemu=%subst(unaemu:7:9)
126600000000     C                   MOVEL     UNAMVD        P2CMVD
126700000000     C                   MOVEL     UNAMTA        P2CMTA
126800000000     C                   MOVEL     UNAMDF        P2CMDF
126900000000     C                   MOVEL     UNAMRA        P2CMRA
127000000000     C                   MOVEL     UNAMDR        P2CMDR
127100000000     C                   MOVEL     UNAX25        P2CX25
127200000000     C                   MOVEL     UNANOT        P2CNOT
127300150422     C* ROVESCIO IN G/M/A LA DATA ultima rilevazione
127400150422     C                   Z-ADD     UNADSL        G08INV
127500150422     C                   Z-ADD     *ZEROS        G08DAT
127600150422     C                   MOVEL     '3'           G08ERR
127700150422     C                   CALL      'XSRDA8'
127800150422     C                   PARM                    WLBDA8
127900150422     C                   Z-ADD     G08DAT        PRCDSL
128000000000     C*
128100000000     C* STAMPO SOLO SE C'E' QUALCOSA DA STAMPARE
128200000000IF  2C     DSTEC         IFNE      *BLANKS
128300000000     C     DSTEC         ANDNE     *ZEROS
128400000000     C                   WRITE     SDU7P2
128500000000E   2C                   ENDIF
128600000000     C*
128700000000E   1C                   ENDIF
128800000000     C*
128900000000     C                   ENDSR
129000000000     C*------------------------------------------------------------------------*
129100950126     C* STATOT - STAMPA TOTALE UNITA'
129200000000     C*          INPUT --> DESTOT: DESCRIZIONE TOTALE
129300000000     C*                    CODTO1: CODICE TOTALE
129400000000     C*                    CODTO2: DECODIFICA CODICE TOTALE
129500000000     C*                    TOTALE: QUANTITA'
129600000000     C*------------------------------------------------------------------------*
129700000000     C     STATOT        BEGSR
129800000000     C*
129900000000     C                   MOVEL     DESTOT        TODDES
130000000000     C                   MOVEL     CODTO1        TODTO1
130100000000     C                   MOVEL     CODTO2        TODTO2
130200000000     C                   Z-ADD     TOTALE        TOCTOT
130300000000     C*
130400000000     C                   WRITE     SDU7TO                                       *TOTALE
130500000000     C*
130600000000     C                   ENDSR
130700000000     C*------------------------------------------------------------------------*
130800971013     C* RICDAT - RICOSTRUISCE LE DATE CON '  /  /    '
130900000000     C*          INPUT  --> $DAT2: DATA SENZA BARRETTE  (NUMERICA)
131000000000     C*          OUTPUT --> $DAT3 :DATA CON LE BARRETTE (ALFANUMERICA)
131100000000     C*------------------------------------------------------------------------*
131200000000     C     RICDAT        BEGSR
131300000000     C*
131400971013     C                   MOVEL     $DAT2         WDAT              8
131500971013     C                   MOVEL     *BLANKS       $DAT3            10
131600000000     C*
131700000000     C     2             SUBST     WDAT:1        SIN               2            *SINISTRA
131800000000     C     2             SUBST     WDAT:3        MEZ               2            *MEZZO
131900971013     C     4             SUBST     WDAT:5        DES               4            *DESTRA
132000000000     C*
132100000000     C                   MOVEL     SIN           SIN2              3
132200000000     C                   MOVE      '/'           SIN2                           ADESS0: 99/
132300000000     C*
132400971013     C                   MOVEL     '/'           DES2              5            ADESSO: /9999
132500971013     C                   MOVE      DES           DES2
132600000000     C*
132700000000     C                   MOVEL     SIN2          SEM               5
132800000000     C                   MOVE      MEZ           SEM                            ADESSO: 99/99
132900000000     C*
133000000000     C                   MOVEL     SEM           $DAT3
133100971013     C                   MOVE      DES2          $DAT3                          ADESSO: 99/99/9999
133200000000     C*
133300000000     C                   ENDSR
133400020322      *------------------------------------------------------------------------*
133500020322      * DECRG2 - DECODIFICO RAGGRUPPAMENTO
133600020322      *------------------------------------------------------------------------*
133700020322     c     decrg2        begsr
133800020322
133900020322     c                   movel     'UNR'         tbecod
134000020322     c                   movel(p)  unarg2        tbeke1
134100020322     c     ktntbe        chain     tntbe01l
134200020322     c                   if        not %found(tntbe01l) or tbeatb <> *blanks
134300020322     c                   eval      prdrg2 = *all'*'
134400020322     c                   else
134500020322     c                   movel     tbeuni        prdrg2
134600020322     c                   endif
134700020322
134800020322     c                   endsr
134900020322      *------------------------------------------------------------------------*
135000020322      * DECKSC - DECODIFICO CLIENTE
135100020322      *------------------------------------------------------------------------*
135200020322     c     decksc        begsr
135300020322
135400020327     c                   clear                   TIBS69DS
135500020327     c                   z-add     unaksc        I69kac
135600020327     c                   call      'TIBS69R'
135700020327     c                   parm                    tibs69DS
135800020327     c                   parm                    ds_cnaco
135900020327     c                   parm                    ds_cnind
136000020327     c                   parm                    ds_cnclp
136100020327     c                   parm                    ds_fncls
136200020327
136300020327     c                   if        o69err <> '1'
136400020327     c                   movel     acorag        prdksc
136500020322     c                   else
136600020327     c                   eval      prdksc = *all'*'
136700020322     c                   endif
136800020322
136900020322     c                   move      unaksc        prcksc
137000020322
137100020322     c                   endsr
137200000000     C*------------------------------------------------------------------------*
137300020516     C* CARORG - CARICA FILIALI
137400000000     C*------------------------------------------------------------------------*
137500000000     C     CARORG        BEGSR
137600950125     C*
137700950125     C                   Z-ADD     0             I                 3 0
137800020531
137900020531     c                   clear                   trul26ds
138000090907      /free
138100090907         IF  DUTntw = c_Logistica;
138200090907           For  ii = 1  To %elem(skPO);
138300090907             if  skPOabi(ii) = *zero;
138400090907               leave;
138500090907             endif;
138600090907             skPO(ii) = skPOabi(ii);
138700090907           EndFor;
138800090907           i = ii;
138900090907         ELSE;
139000090907      /end-free
139100020531     c                   eval      i26tpt = 'P'
139200020531     c                   eval      i26tla = 'L'
139300020531     c                   call      'TRUL26R'
139400020531     c                   parm                    trul26ds
139500090907      /free
139600090907         ENDIF;
139700090907      /end-free
139800020531      * Leggo la schiera dei terminal caricati
139900020531     c                   do        250           ii
140000020531     c                   if        skpo(ii) = *zeros
140100020531     c                   leave
140200020531     c                   endif
140300020523
140400020523      * se utente non ha abilitazione 'AZ' controllo se il p.o. è nella sk dei po abilitati
140500020523     c                   if        wabi <> 'AZ'
140600020531     c     skpo(ii)      lookup    skpoabi                                28
140700020523     c                   if        not *in28
140800020531     c                   iter
140900020523     c                   endif
141000020523     c                   endif
141100000000     C*
141200000000     C* CARICA SOLO LE FILIALI DEL TIPO SELEZIONATO A VIDEO
141300950126     C                   SETOFF                                       28
141400020531     c                   movel     skpo(ii)      korfil
141500020531     c     keyorg        chain     azorg01l
141600000000     C     ORGFAG        SCAN      PARTFI                                 28
141700000000IF  3C     *IN28         IFEQ      *ON
141800971128     C*
141900971128     C* REPERISCE IL TERMINAL
142000971128     C                   CLEAR                   DSLV55
142100971128     C                   MOVEL     'P'           D55TPT
142200971128     C                   Z-ADD     ORGFIL        D55LIN
142300971128     C                   Z-ADD     DATCOR        D55DRF
142400971128     C                   CALL      'FNLV55R'
142500971128     C                   PARM                    DSLV55
142600971128IF  4C     D55ERR        IFNE      *BLANKS
142700971128     C                   Z-ADD     ORGFIL        D55TFP
142800971128E   4C                   ENDIF
142900000000     C*
143000000000     C* CARICA SOLO LE FILIALI DEL LIVELLO SELEZIONATO A VIDEO
143100000000     C                   MOVEL     'N'           WFILOK            1
143200000000IF  4C     PARLIF        IFEQ      'T'
143300000000     C                   MOVEL     'S'           WFILOK            1
143400000000E   4C                   ENDIF
143500000000IF  4C     PARLIF        IFEQ      '1'
143600971128     C     ORGFIL        ANDEQ     D55TFP
143700000000     C                   MOVEL     'S'           WFILOK
143800000000E   4C                   ENDIF
143900000000IF  4C     PARLIF        IFEQ      '2'
144000971128     C     ORGFIL        ANDNE     D55TFP
144100000000     C                   MOVEL     'S'           WFILOK
144200000000E   4C                   ENDIF
144300000000IF  4C     WFILOK        IFEQ      'S'
144400000000     C                   ADD       1             I
144500000000     C                   Z-ADD     ORGFIL        FIL(I)                         *FILIALE
144600000000     C                   MOVEL     ORGDES        DFIL(I)                        *DESCRIZIONE FILIALE
144700000000     C                   MOVEL     ORGFAG        TFIL(I)                        *TIPO FILIALE
144800020531     C                   Z-ADD     d55tfp        FEL(I)                         *FILIALE ELABORATORE
144900000000E   4C                   ENDIF
145000000000E   3C                   ENDIF
145100020531
145200020516 e1  C                   ENDDO
145300090907      /free
145400090907         IF  DUTntw = c_Logistica;
145500090907           // - Numero Totale Filiali caricate
145600090907           NumFil = i;
145700090907           leavesr;
145800090907         ENDIF;
145900090907      /end-free
146000950125     C*--------------------
146100950125     C* VARIE UNITA'  (IMPOSTO FILIALE ELABORATORE = 999)
146200950125     C*--------------------
146300000000     C                   Z-ADD     *ZEROS        KORFIL
146400020531     C     KEYORg        SETLL     AZORG020
146500000000     C                   READ      AZORG020                               99
146600000000     C*
146700000000     C* CICLO FINO A FINE FILE
146800000000DO  1C     *IN99         DOWEQ     '0'
146900000000     C*
147000000000     C* ELIMINA RECORD ANNULLATI
147100000000IF  2C     ORGFVA        IFEQ      *BLANKS
147200000000     C*
147300000000     C* CARICA SOLO SE A VIDEO E' STATO RICHIESTO ANCHE IL TIPO FILIALE 'W'
147400950126     C                   SETOFF                                       28
147500100908     c                   if        orgfag='Y'
147600100908     c                   eval      wfag='W'
147700100908     c                   else
147800100908     c                   eval      wfag=orgfag
147900100908     c                   endif
148000100921     c
148100100921     c* il tipo "951" lo tratto come un "W" nella stampa
148200100921     c                   if        orgfil=951
148300100921     c                   eval      wfag='W'
148400100921     c                   endif
148500100921     c
148600100908     C     WFAG          SCAN      PARTFI                                 28
148700000000IF  3C     *IN28         IFEQ      *ON
148800000000     C*
148900000000     C* CARICA SOLO SE A VIDEO E' STATO RICHIESTO LIVELLO FILIALI = TUTTE
149000000000IF  4C     PARLIF        IFEQ      'T'
149100000000     C                   ADD       1             I
149200000000     C                   Z-ADD     ORGFIL        FIL(I)                         *FILIALE
149300000000     C                   MOVEL     ORGDES        DFIL(I)                        *DESCRIZIONE FILIALE
149400000000     C                   MOVEL     ORGFAG        TFIL(I)                        *TIPO FILIALE
149500950125     C                   Z-ADD     999           FEL(I)                         *FILIALE ELABORATORE
149600000000E   4C                   ENDIF
149700000000E   3C                   ENDIF
149800000000E   2C                   ENDIF
149900000000     C                   READ      AZORG020                               99
150000000000E   1C                   ENDDO
150100020716
150200020716      * Aggiungo sempre alla sk il p.o. sede
150300020716     c                   Eval      KorFil = 046
150400020716     c     KeyOrg        Chain     Azorg01l
150500021014IF  1c                   If        %Found(Azorg01l)
150600020716     c                   Add       1             i
150700020716     c                   Z-Add     OrgFil        Fil(i)                         Filiale
150800020716     c                   Movel     OrgDes        Dfil(i)                        Descrizione
150900020716     c                   Movel     OrgFag        Tfil(i)                        Tipo
151000020716     c                   Z-Add     046           Fel(i)                         Elaboratore
151100021014E   1c                   EndIf
151200021014      * Se abilitazione 'AZ' controllo di aver caricato tutti i magazzini che esistono
151300021014If  1c                   If        Wabi =  'AZ'
151400021014Do  2c     1             Do        500           ii
151500021014     c     Skpoabi(ii)   Lookup    Fil                                    28
151600021014      * Se l'ho già caricato torno a leggere
151700021014     c                   If        *In28
151800021014     c                   Iter
151900021014     c                   EndIf
152000021014     c                   Eval      *In28 = *Off
152100021014      * Carico solo il tipo selezionato a video
152200021014     c                   Movel     Skpoabi(ii)   Korfil
152300021014     c     keyorg        chain     azorg01l
152400021014     c     Orgfag        Scan      Partfi                                 28
152500021014IF  3c                   If        *In28   and
152600021014      * Carico solo se richieste tutte le filiali
152700021014     c                             Parlif = 'T'
152800021014     c                   Add       1             I
152900021014     C                   Z-ADD     ORGFIL        FIL(I)                         *FILIALE
153000021014     C                   MOVEL     ORGDES        DFIL(I)                        *DESCRIZIONE FILIALE
153100021014     C                   MOVEL     ORGFAG        TFIL(I)                        *TIPO FILIALE
153200021014     C                   Z-ADD     d55tfp        FEL(I)                         *FILIALE ELABORATORE
153300021014E   3C                   ENDIF
153400021014    2c                   EndDo
153500021014    3C                   ENDIF
153600000000     C*
153700000000     C* NUMERO TOTALE FILIALI CARICATE
153800000000     C                   Z-ADD     I             NUMFIL
153900000000     C*
154000000000     C                   ENDSR
154100000000     C*------------------------------------------------------------------------*
154200950126     C* CARTAB - CARICA TABELLE
154300000000     C*------------------------------------------------------------------------*
154400000000     C     CARTAB        BEGSR
154500000000     C*
154600020322     C                   MOVEL     'UNT'         tbeCOD
154700000000     C                   Z-ADD     *ZEROS        I
154800000000     C*
154900020322     C     tbecod        SETLL     tntbe01l
155000020322     C     tbecod        READE     tntbe01l                               99
155100000000     C*
155200000000DO  1C     *IN99         DOWEQ     *OFF
155300020322IF  2C     TBeatb        IFEQ      *BLANKS
155400000000     C                   ADD       1             I
155500020322     C                   MOVEL     tbeke1        TIP(I)                         *CODICE
155600020322     C                   MOVEL     tbeuni        DTIP(I)                        *DESCRIZIONE
155700000000E   2C                   ENDIF
155800000000     C*
155900020322     C     tbecod        READE     tntbe01l                               99
156000000000E   1C                   ENDDO
156100000000     C*
156200000000     C                   ENDSR
156300000000     C*------------------------------------------------------------------------*
156400950126     C* STAPAR - STAMPA PAGINA PARAMETRI
156500000000     C*------------------------------------------------------------------------*
156600000000     C     STAPAR        BEGSR
156700950331     C*--------------------
156800950331     C* D E C O D I F I C H E
156900950331     C*--------------------
157000950331     C*
157100950331     C* TIPO ORDINAMENTO
157200000000IF  1C     PARORD        IFEQ      1
157300000000     C                   MOVEL     PAR(1)        PACORD
157400000000E   1C                   ENDIF
157500000000IF  1C     PARORD        IFEQ      2
157600000000     C                   MOVEL     PAR(2)        PACORD
157700000000E   1C                   ENDIF
157800000000IF  1C     PARORD        IFEQ      3
157900950126     C                   MOVEL     PAR(3)        PACORD
158000000000E   1C                   ENDIF
158100000000IF  1C     PARORD        IFEQ      4
158200000000     C                   MOVEL     PAR(4)        PACORD
158300000000E   1C                   ENDIF
158400000000IF  1C     PARORD        IFEQ      5
158500000000     C                   MOVEL     PAR(5)        PACORD
158600000000E   1C                   ENDIF
158700000000     C*
158800000000     C* TIPO UNITA'
158900000000     C                   Z-ADD     PARTIP        PACTIP
159000000000     C                   MOVEL     DECTIP        PADTIP
159100000000     C*
159200000000     C* MAGAZZINO UBICAZIONE
159300000000     C                   Z-ADD     PARMAG        PACMAG
159400000000     C                   MOVEL     DECMAG        PADMAG
159500000000     C*
159600000000     C* DATA SCADENZA MANUTENZIONE DAL E AL
159700000000     C                   Z-ADD     DECDSM        PACDSM
159800000000     C                   Z-ADD     DECDS2        PACDS2
159900000000     C*
160000000000     C* MARCA
160100000000     C                   MOVEL     PARMAR        PACMAR
160200000000     C*
160300000000     C* MODELLO
160400000000     C                   MOVEL     PARMOD        PACMOD
160500000000     C*
160600000000     C* TIPI FILIALE
160700971013     C                   MOVEL     PARTF1        PACTF1
160800971013     C                   MOVEL     PARTF2        PACTF2
160900971013     C                   MOVEL     PARTF3        PACTF3
161000971013     C                   MOVEL     PARTF4        PACTF4
161100000000     C*
161200000000     C* LIVELLI FILIALE
161300000000IF  1C     PARLIF        IFEQ      'T'
161400000000     C                   MOVEL     'TUTTI'       PACLIF
161500000000E   1C                   ENDIF
161600000000IF  1C     PARLIF        IFEQ      '1'
161700000000     C                   MOVEL     '1 LIVEL.'    PACLIF
161800000000E   1C                   ENDIF
161900000000IF  1C     PARLIF        IFEQ      '2'
162000000000     C                   MOVEL     '2 LIVEL.'    PACLIF
162100000000E   1C                   ENDIF
162200000000     C*
162300000000     C* STAMPA SINTETICA O DETTAGLIATA
162400000000IF  1C     PARSOD        IFEQ      'S'
162500000000     C                   MOVEL     PAR(6)        PACSOD
162600000000X   1C                   ELSE
162700000000     C                   MOVEL     PAR(7)        PACSOD
162800000000E   1C                   ENDIF
162900000000     C*
163000000000     C* DATI TECNICI
163100000000IF  1C     PARTEC        IFEQ      'S'
163200000000     C                   MOVEL     'SI'          PACTEC
163300000000X   1C                   ELSE
163400000000     C                   MOVEL     'NO'          PACTEC
163500000000E   1C                   ENDIF
163600950331     C*
163700950331     C* STAMPA TOTALI
163800950331IF  1C     PARTOT        IFEQ      'S'
163900950331     C                   MOVEL     'SI'          PACTOT
164000950331X   1C                   ELSE
164100950331     C                   MOVEL     'NO'          PACTOT
164200950331E   1C                   ENDIF
164300960514     C*
164400960514     C* STAMPA FILIALE / SEDE
164500960514IF  1C     PARFOS        IFEQ      'S'
164600960514     C                   MOVEL     'SEDE'        PACFOS
164700960514X   1C                   ELSE
164800090907     C                   MOVEL     'FIL.'        PACFOS
164900960514E   1C                   ENDIF
165000950331     C*--------------------
165100950331     C* S T A M P A   PARAMETRI
165200950331     C*--------------------
165300000000     C                   WRITE     SDU7PT                                       *INTESTAZIONE
165400000000     C                   WRITE     SDU7PA                                       *PARAMENTRI
165500000000     C*
165600000000     C                   ENDSR
165700000000     C*------------------------------------------------------------------------*
165800950126     C* *INZSR - IMPOSTAZIONI INIZIALI
165900000000     C*------------------------------------------------------------------------*
166000000000     C     *INZSR        BEGSR
166100000000     C*
166200000000     C* PARAMETRI IN INPUT AL PGM
166300000000     C     *ENTRY        PLIST
166400000000     C                   PARM                    KPJBA
166500000000     C                   MOVEL     KPJBU         PARAM
166600020513
166700020513     c     *dtaara       define    §azute        azuteds
166800020513     c     *dtaara       define    §datiute      ddatiute
166900020513
167000020513     c                   in(E)     *dtaara
167100020513     c                   if        %Error or rsut = *blanks
167200020513     c                   clear                   tibs34ds
167300020513     c                   call      'TIBS34R'
167400020513     c                   parm                    Tibs34Ds
167500020513     c                   in        *dtaara
167600020513     c                   endif
167700020523      * controllo le abilitazioni del profilo
167800020513     c                   movel     utefaf        dute01
167900020513     c                   if        §uteiun <> *blanks
168000020513     c                   eval      wabi = §uteiun
168100020513     c                   else
168200020513     c                   eval      wabi = uteaut
168300020513     c                   endif
168400020513      * richiamo pgm che ritorna i p.o. abilitati
168500021014     c                   If        Wabi <> 'AZ'
168600020516     c                   clear                   trul31ds
168700020516     c                   eval      i31abi = wabi
168800020516     c                   eval      i31cdi = dutdis
168900020516     c                   eval      i31car = dutare
169000020516     c                   eval      i31cpo = dutpou
169100020516     c                   call      'TRUL31R'
169200020516     c                   parm                    kpjba
169300020516     c                   parm                    trul31ds
169400021014     c                   EndIf
169500090904
169600090904      /free
169700090904
169800090904         SELECT;
169900090904
170000090904           // - Reperimento filiali gestibili per utente della LOGISTICA
170100090904           WHEN  DUTntw = c_Logistica;
170200090904
170300090904             setll  *loval  AZORG;
170400090904             read   AZORG;
170500090904
170600090904             DoW  Not  %eof(AZORG01L);
170700090904
170800090904               if  ORGfva = *blank;
170900090904
171000090904                 Og143 = ORGde3;
171100090904                 if  §OgNtw = DUTntw;
171200090904
171300090904                   ii += 1;
171400090904                   skPOabi(ii) = ORGfil;
171500090904
171600090904                 endif;
171700090904
171800090904               endif;
171900090904
172000090904               read   AZORG;
172100090904
172200090904             EndDo;
172300090904
172400090904      /end-free
172500090904
172600021014      * per abilitazione 'AZ' devo caricare tutti i p.o. di azorg21l
172700090904Wh  1c                   WHEN      wabi = 'AZ'
172800021014     c     *loval        setll     azorg21l
172900021014Do  2c                   do        *hival
173000021014     c                   read      azorg21l
173100021014     c                   if        %eof
173200021014     c                   leave
173300021014     c                   endif
173400021014     c                   z-add     1             ii
173500021014     c     orgfil        lookup    skpoabi(ii)                            99
173600021014If  3c                   if        not *in99
173700021014     c                   z-add     1             ii
173800021014     c     *zeros        lookup    skpoabi(ii)                            99
173900021014If  4c                   if        *in99
174000021014     c                   movel     orgfil        skpoabi(ii)
174100021014    4c                   endif
174200021014    3c                   endif
174300021014    2c                   enddo
174400021014      * e tutti i p.o. di azorg01l
174500021014     c     *loval        setll     azorg01l
174600021014Do  2c                   do        *hival
174700021014     c                   read      azorg01l
174800021014     c                   if        %eof
174900021014     c                   leave
175000021014     c                   endif
175100021014     c                   z-add     1             ii
175200021014     c     orgfil        lookup    skpoabi(ii)                            99
175300021014If  3c                   if        not *in99
175400021014     c                   z-add     1             ii
175500021014     c     *zeros        lookup    skpoabi(ii)                            99
175600021014If  4c                   if        *in99
175700021014     c                   movel     orgfil        skpoabi(ii)
175800021014    4c                   endif
175900021014    3c                   endif
176000021014    2c                   enddo
176100090904      *
176200021014      * se non è abilitazione 'AZ' passo la sk tornata dal TRUL31 nella sk di 500 elementi
176300090904   x1c                   OTHER
176400021014     c                   movea     skpo31        skpoabi
176500021014     c                   z-add     1             ii
176600021014     c     951           lookup    skpoabi(ii)                            99
176700021014If  2c                   if        not *in99
176800021014     c                   z-add     1             ii
176900021014     c     *zeros        lookup    skpoabi(ii)                            99
177000021014     c                   if        *in99
177100021014     c                   movel     951           skpoabi(ii)
177200021014     c                   endif
177300021014    2c                   endif
177400090904      *
177500090904    1c                   ENDSL
177600000000     C*
177700000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
177800950125     C     *LIKE         DEFINE    ORGFIL        KORFIL
177900000000     C     *LIKE         DEFINE    UNATIP        KUNTIP
178000000000     C     *LIKE         DEFINE    UNATIP        DEPTIP
178100000000     C     *LIKE         DEFINE    UNAMAT        KUNMAT
178200000000     C     *LIKE         DEFINE    UNAMAG        KUNMAG
178300000000     C     *LIKE         DEFINE    UNADSM        KUNDSM
178400000000     C     *LIKE         DEFINE    UNADSM        DEPDSM
178500000000     C     *LIKE         DEFINE    UNAMAR        KUNMAR
178600000000     C     *LIKE         DEFINE    UNAMAR        DEPMAR
178700000000     C     *LIKE         DEFINE    UNAMOD        KUNMOD
178800000000     C     *LIKE         DEFINE    UNAMOD        DEPMOD
178900950125     C     *LIKE         DEFINE    ORGFEL        DEPFEL
179000950125     C     *LIKE         DEFINE    ORGFEL        WFEL
179100000000     C     *LIKE         DEFINE    I             NUMFIL
179200000000     C     *LIKE         DEFINE    I             F
179300000000     C*
179400020322     C* CHIAVE LETTURA TNTBE01L
179500020322     C     Ktntbe        KLIST
179600020322     C                   KFLD                    tbecod
179700020322     C                   KFLD                    tbeke1
179800000000     C*
179900020531     C* CHIAVE LETTURA AZORG01L/azorg21l - COMPLETA
180000000000     C     KEYORG        KLIST
180100020531     C                   KFLD                    KORFiL                         *FILIALE ELABORATORE
180200000000     C*
180300000000     C* CHIAVE LETTURA UNANA01L - COMPLETA
180400000000     C     KEYUN1        KLIST
180500000000     C                   KFLD                    KUNTIP                         *TIPO
180600000000     C*
180700000000     C* CHIAVE LETTURA UNANA02L - COMPLETA
180800000000     C     KEYUN2        KLIST
180900000000     C                   KFLD                    KUNTIP                         *TIPO
181000000000     C                   KFLD                    KUNMAT                         *MATRICOLA
181100000000     C*
181200000000     C* CHIAVE LETTURA UNANA03L - COMPLETA
181300000000     C     KEYUN3        KLIST
181400000000     C                   KFLD                    KUNMAG                         *MAGAZZINO
181500000000     C                   KFLD                    KUNTIP                         *TIPO
181600000000     C*
181700000000     C* CHIAVE LETTURA UNANA04L - COMPLETA
181800000000     C     KEYUN4        KLIST
181900000000     C                   KFLD                    KUNDSM                         *SCADENZA MANUTENZ.
182000000000     C*
182100000000     C* CHIAVE LETTURA UNANA05L - COMPLETA
182200000000     C     KEYUN5        KLIST
182300000000     C                   KFLD                    KUNTIP                         *TIPO
182400000000     C                   KFLD                    KUNMAR                         *MARCA
182500000000     C                   KFLD                    KUNMOD                         *MODELLO
182600000000     C*
182700000000     C* IMPOSTO A ZERO I FINE CICLI DI LETTURA
182800000000     C                   MOVEL     '0'           $FINE             1            *FINE UNITA' EDP
182900000000     C                   MOVEL     '0'           PASMAG            1            *MEMOR UNITA MAGAZZI
183000950125     C                   MOVEL     '0'           PASFEL            1            *MEMOR UNITA FIL.ELA
183100941227     C                   MOVEL     '0'           SISEL             1            *SELEZIONATO QC.
183200000000     C                   SETON                                        01        *SALTO PAGINA
183300971128     C*
183400971128     C* CALCOLA LA DATA CORRENTE
183500971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
183600971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
183700971128     C                   Z-ADD     WN8           DATA              8 0
183800971128     C                   Z-ADD     WN8           G08DAT
183900971128     C                   Z-ADD     *ZEROS        G08INV
184000971128     C                   MOVEL     '0'           G08ERR
184100971128     C                   CALL      'XSRDA8'
184200971128     C                   PARM                    WLBDA8
184300971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
184400941227     C*
184500941227     C* CONTROLLA SE HANNO SELEZIONATO QUALCOSA
184600971013IF  1C     PARSEL        IFNE      *BLANKS
184700971013     C     PARSEL        ANDNE     *ZEROS
184800941227     C                   MOVEL     '1'           SISEL                          *SELEZIONATO QC.
184900941227E   1C                   ENDIF
185000000000     C*
185100950126     C* CARICA ORGANIGRAMMA
185200000000     C                   EXSR      CARORG
185300000000     C*
185400950126     C* CARICA TABELLE
185500000000     C                   EXSR      CARTAB
185600000000     C*
185700000000     C                   ENDSR
185800950126     O*------------------------------------------------------------------------*
185900950126     O* SCHIERE A TEMPO DI COMPILAZIONE
186000000000     O*------------------------------------------------------------------------*
186100000000**   PAR - DECODIFICHE PARAMETRI
186200000000TIPO/CODICE                                                  1
186300000000TIPO/MATRICOLA                                               2
186400000000MAGAZZINO/TIPO/CODICE                                        3
186500000000DATA SCADENZA MANUTENZIONE                                   4
186600000000TIPO/MARCA/MODELLO                                           5
186700000000SINTETICA                                                    6
186800000000DETTAGLIATA                                                  7
186900000000**   TOT - DECODIFICHE TOTALI
187000000000TIPO                                                         1
187100000000MAGAZZINO                                                    2
187200950330AS/400                                                       3
187300000000SCADENZA                                                     4
187400000000MARCA                                                        5
187500000000                                                             6
187600000000GENERALE                                                     7
187700000000MATRICOLA                                                    8
187800000000**   ISS - INTESTAZIONE STAMPA SINTETICA
187900000000T I P O   U N I T A'                                         1
188000000000M A T R I C O L A                                            2
188100000000M A G A Z Z I N I                                            3
188200000000S C A D E N Z E                                              4
188300000000M A R C H E                                                  5
