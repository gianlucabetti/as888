000100990128     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990128     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300880111     H* OCO321R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500000000     H*             - STAMPA BOLLE SPEDIZIONE INTERATIVA -           *
000600000000     H*             --------------------------------------           *
000700880111     H*                                                              *
000800990128     H*         FILE : tmmoc01L - tmsmp01L - tmmmp00F                *
000900990128     H*                tmamp01L - CNACO00F - CNIND00F                *
001000021029     H*                ANTAB00F - FNSPE01L - FIVABWKF                *
001100000000     H*--------------------------------------------------------------*
001200990128     Ftmmoc01L  UF   E           K DISK
001300990128     Ftmmmp00F  O  A E           K DISK
001400990128     Ftmsmp01L  UF   E           K DISK
001500990128     Ftmamp01L  UF   E           K DISK
001600990128     Fancln01l  IF   E           K DISK
001700990128     Fansog01l  IF   E           K DISK
001800990128     Fanind01l  IF   E           K DISK
001900000000     FANTAB00F  IF   E           K DISK
002000980112     FAZORG01L  IF   E           K DISK
002100000000     F*--------- SPEDIZIONIERI --------------------------------------*
002200970801     FFNSPE01L  IF   E           K DISK
002300970801     F                                     RENAME(FNSPE000:ANSPE1)
002400000000     F*--------- LUOGHI DI DESTINAZIONE -----------------------------*
002500970801     FFNSPE03L  IF   E           K DISK
002600970801     F                                     RENAME(FNSPE000:ANSPE3)
002700021029     FFIVABWKF  O  A E           K DISK    USROPN
002800000000     FBOLLEPRT  O    F  132        PRINTER OFLIND(*INOF)
002900000000     F                                     USROPN
003000000000     D*--------------------------------------------------------------*
003100000000     D TOR             S              2    DIM(10)                              *TIPO ORDINE
003200000000     D DTOR            S             25    DIM(10)                              *DESCRIZIONE
003300000000     D COM             S             30    DIM(10)                              *COMMENTO
003400000000     D AZ              S              1    DIM(15)                              *AZIONI CAUS.
003500920916     D TAB$            S              1    DIM(10) CTDATA PERRCD(1)
003600920916     D TAB£            S              1    DIM(10) ALT(TAB$)
003700920916     D $I              S              1    DIM(15)
003800920916     D $O              S              1    DIM(17)
003900920925     D ST              S             25    DIM(2) CTDATA PERRCD(1)
004000921005     D*--------------------------------------------------------------*
004100020924     D GAI             S             10    DIM(2) CTDATA PERRCD(1)
004200000000     D*--------------------------------------------------------------*
004300000000     D DSMAG         E DS                  EXTNAME(ANTAB153)
004400000000     D*------------- TABELLA MAGAZZINI :
004500000000     D TB152         E DS                  EXTNAME(ANTAB152)
004600000000     D*------------- TABELLA CAUSALI :
004700000000     D  AZE                   30     34
004800000000     D                                     DIM(5)                               *ENTRATE
004900000000     D  AZU                   39     43
005000000000     D                                     DIM(5)                               *USCITE
005100000000     D  AZG                   48     52
005200000000     D                                     DIM(5)                               *GESTIONALI
005300990128     D DSSAL         E DS                  EXTNAME(tmsmp00F)
005400000000     D*------------- FILE SALDI:
005500990211     D  EN                    86    115P 2
005600000000     D                                     DIM(5)
005700990211     D  US                   116    145P 2
005800000000     D                                     DIM(5)
005900000000     D OFNDT2          DS           512
006000000000     D*------------- NUMERAT.CODICE INDIVIDUALE RIGA
006100000000     D  NUMCRG                 1      7  0
006200000000     D  FILDT2                 8    256
006300000000     D  FILDT3               257    512
006400000000     D MPHDT1          DS
006500000000     D*--- DATA AREA N.MOVIMENTO MAGAZZINO :
006600000000     D  NUMMOV                 1      5  0
006700000000     D  FILL1                  6      8
006800000000     D  DATDTA                 9     14  0
006900000000     D  FILL2                 15     17
007000000000     D  ANNDTA                18     19  0
007100000000     D  FIL03                 20     50
007200000000     D  DGIO                  51     56  0
007300000000     D  FIL04                 57    100
007400000000     D  FILL5                101    300
007500000000     D  GIOTOL               301    302  0
007600000000     D  FILL6                303    512
007700000000     D OCLBL1          DS
007800000000     D*--- DATA AREA N.BOLLA /DATA BOLLA :
007900000000     D  NUMBOL                 1      6  0
008000000000     D  DATABL                 7     12  0
008100000000     D DSBL1           DS
008200000000     D*--- DS TABELLA BL1
008300000000     D  TABCAU                 1      3
008400000000     D DSTAB           DS
008500000000     D*DESCRIZIONE TIPO ORDINE/FLAG VISIONE
008600000000     D  DESTOR                 1     25
008700000000     D  TABTOR                35     35
008800000000     D DSPOR           DS
008900000000     D*DESCRIZIONE DEL TRASPORTO + FLAG
009000000000     D  DESRES                 1     26
009100000000     D  T1                    41     41
009200000000     D  T2                    42     42
009300000000     D  T3                    43     43
009400000000     D PARAM2          DS
009500990128     D  BOLCLI                 1      8
009600990128     D  BOLNOR                 9     13  0
009700990128     D  BOLDEM                14     21  0
009800990128     D  BOLDOR                22     29  0
009900990128     D  socpas                30     32
010000000000     D KPJBA         E DS
010100990128     D soc001        E DS                  EXTNAME(xsoc001ds)
010200990128     D xsocds          DS          1000
010300990128     d dataiso         s               d   datfmt(*iso)
010400990128     d dataeur         s               d   datfmt(*eur)
010500000000     I*---- PASSAGGIO PARAMETRI
010600000000     C*----------------------------------------------------*
010700000000     C     *ENTRY        PLIST                                                  *
010800000000     C                   PARM                    KPJBA                          *
010900000000     C     *DTAARA       DEFINE                  OFNDT2                         *
011000000000     C     *DTAARA       DEFINE                  MPHDT1                         *
011100000000     C     *DTAARA       DEFINE                  OCLBL1                         *
011200000000     C******************************************************
011300000000RIC  C*-- RICERCA PARAMETRI VARI :                         *
011400000000     C                   MOVEL     *BLANKS       BOLCLI                         *
011500000000     C                   Z-ADD     0             BOLNOR                         *
011600000000     C                   Z-ADD     0             BOLDEM                         *
011700000000     C                   Z-ADD     0             BOLDOR                         *
011800000000     C                   MOVEL     KPJBU         PARAM2                         *
011900990128     C*---------- RICERCA DITTA :
012000990128     C                   MOVEL     'SOC001'      TIPXSC
012100990128     C                   MOVEL     *blank        SOCXSC
012200990128     C                   EXSR      REPSOC
012300990128     C     RTNXSC        IFNE      '1'
012400990128     C                   MOVEL     XSOCDS        SOC001
012500990128     C                   MOVEL     xscrgs        desdit           20
012600990129     c                   movel     xscvia        simvia           30
012700990129     c                   movel     xsccit        simcit           30
012800990128     c                   end
012900990128     c                   time                    como14           14 0
013000990128     c                   move      como14        woggi             8 0
013100921005     C* RICERCO SULLA TABELLA DELLA STAMPA PARTITA IVA
013200921005     C* E DELLA STAMPA LEGENDA CONVERSOINE
013300921005     C*
013400921005     C                   MOVE      '900'         TABGRU
013500990128     C                   MOVE      '99999999'    TABCOD
013600921005     C     KEYANT        CHAIN     ANTAB00F                           41
013700921005     C  N41              MOVEL     TABDES        STSINO            2
013800921005     C  N41              MOVEL     STSINO        STPIVA            1
013900921005     C  N41              MOVE      STSINO        STLEGE            1
014000921005     C   41              MOVEL     *BLANKS       STSINO
014100921005     C     *IN41         IFEQ      '1'
014200921005     C                   SETOFF                                       9394
014300921005     C                   END
014400921005     C     STPIVA        COMP      'S'                                    93
014500921005     C     STLEGE        COMP      'S'                                    94
014600921005     C*----------------------------------------------------*
014700921005     C* CONTROLLO SE IL SISTEMA INFORMATIVO  E' RIFERITO A *
014800921005     C* GAITRA201 (BARTOLINI)                              *
014900921005     C     KNSIF         IFEQ      GAI(1)                                       *
015000021029     C                   OPEN      FIVABWKF                                     *
015100921005     C                   Z-ADD     1             FLGAI             1 0
015200921005     C                   END                                                    *
015300000000     C******************************************************
015400990128     C*--- FILE tmmoc01L 'CODICE CLIENTE  N.ORDINE'        *
015500000000     C     KEYCLI        KLIST                                                  *
015600000000     C                   KFLD                    BOLCLI                         *
015700000000     C                   KFLD                    BOLNOR                         *
015800990128     C*--- FILE SALDI MAGAZZINO M.P. "tmsmp01L"            *
015900000000     C     KEYSAL        KLIST                                                  *
016000990128     C                   KFLD                    ANNDTA4                        *
016100000000     C                   KFLD                    MOCMAG                         *
016200000000     C                   KFLD                    MOCMAT                         *
016300000000     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F" -------------*
016400000000     C     KEYANT        KLIST                                                  *
016500000000     C                   KFLD                    TABGRU                         *
016600000000     C                   KFLD                    TABCOD                         *
016700990128     C*--- FILE ANAG. MAT. PRIME "tmamp01L"                *
016800000000     C     KEYMAT        KLIST                                                  *
016900000000     C                   KFLD                    MOCMAT                         *
017000000000     C*--- CHIAVE PER RICERCA TIPO ORDINI 'ANTAB00F'-------*
017100000000     C     KEYTAB        KLIST                                                  *
017200000000     C                   KFLD                    GRU1              3            *
017300000000     C*--- CHIAVE PER RICERCA SPEDIZIONIERE/LUOGO          *
017400000000     C     KEYSPE        KLIST                                                  *
017500970801     C                   KFLD                    SPEFLS                         *
017600000000     C                   KFLD                    SPECLI                         *
017700000000     C                   KFLD                    CODSPE            3            *
017800000000     C*--- RICERCA RAG. SOC.CLIENTE 'CNACO00F'             *
017900990128     C     KEYcln        KLIST                                                  *
018000990128     C                   KFLD                    socpas                         *
018100990128     C                   KFLD                    moccli                         *
018200000000     C*----------------------------------------------------*
018300000000     C*RIBALTAMENTO IN AAMMGG LA DATA DELL'ORDINE
018400000000     C*E DELLA DATA DI EMISSIONE
018500990128     c                   if        boldor > 0
018600990202     c                   move      boldor        dataeur
018700990202     c                   move      dataeur       dataiso
018800990202     c                   move      dataiso       datdor            8 0
018900990128     C                   end
019000990128     c                   if        boldem > 0
019100990202     c                   move      boldem        dataeur
019200990202     c                   move      dataeur       dataiso
019300990202     c                   move      dataiso       datdem            8 0
019400990128     C                   end
019500000000     C*RICERCA CAUSALE OPERATIVA
019600000000     C                   EXSR      RICCAU
019700000000     C*CARICAMENTO TIPOLOGIE ORDINI
019800000000     C                   EXSR      CARVIS
019900000000     C*POSIZIONAMENTO PER CLIENTE N.ORDINE
020000990128     C     KEYCLI        SETLL     tmmoc000                               04
020100000000     C  N04              GOTO      FINE
020200000000     C*LETTURA
020300000000     C     SU            TAG
020400990128     C     KEYCLI        READE     tmmoc000                               05
020500871219     C   05
020600871219     CAN 22              EXSR      STACOM
020700871219     C   05
020800871219     CAN 22              EXCEPT    FONDO
020900000000     C   05              GOTO      FINE
021000000000     C*RIBALTAMENTO IN GGMMAA DATA RIF.VS.ORD.
021100990128     c                   if        mocdrf > 0
021200990128     c                   move      mocdrf        dataiso
021300990128     c                   move      dataiso       dataeur
021400990128     c                   move      dataeur       datdrf            8 0
021500990128     c                   end
021600000000     C*
021700000000     C     MOCDOR        IFNE      DATDOR
021800000000     C                   GOTO      SU
021900000000     C                   END
022000000000     C     MOCTRG        CABEQ     '1'           LA
022100000000     C     MOCTRG        CABEQ     '6'           STAR
022200000000     C                   GOTO      SU
022300000000     C*----------------
022400000000     C     LA            TAG
022500000000     C     MOCTRG        IFEQ      '1'
022600000000     C     MOCFAS        CABNE     'AS'          SU
022700000000     C                   EXSR      LEGGI
022800000000     C                   END
022900000000     C     STAR          TAG
023000000000     C     MOCTRG        IFEQ      '6'
023100000000     C                   EXSR      MEMCOM
023200000000     C                   END
023300000000     C                   GOTO      SU
023400000000     C     FINE          TAG
023500000000     C                   SETON                                        LR
023600000000     C******************************************************
023700000000     C     RICCAU        BEGSR
023800000000     C**------------------------------
023900000000     C** RICERCA  CAUSALE OPERATIVA
024000000000     C**------------------------------
024100000000     C                   MOVE      '257'         TABGRU
024200000000     C                   MOVE      *ZEROS        TABCOD
024300000000     C                   MOVE      '1'           TABCOD
024400000000     C     KEYANT        CHAIN     ANTAB00F                           H1
024500000000     C   H1              GOTO      FINE
024600000000     C                   MOVEL     TABDES        DSBL1
024700000000     C*---------------
024800000000     C                   MOVE      '152'         TABGRU
024900000000     C                   MOVE      *ZEROS        TABCOD
025000000000     C                   MOVEL     'MP'          CINQUE            5
025100000000     C                   MOVE      TABCAU        CINQUE            5
025200000000     C                   MOVE      CINQUE        TABCOD
025300000000     C     KEYANT        CHAIN     ANTAB00F                           H1
025400000000     C   H1              GOTO      FINE
025500000000     C                   MOVEL     TABDES        TB152
025600000000     C                   MOVEA     AZE           AZ(1)
025700000000     C                   MOVEA     AZU           AZ(6)
025800000000     C                   MOVEA     AZG           AZ(11)
025900000000     C                   ENDSR
026000000000     C******************************************************
026100000000     C     CARVIS        BEGSR
026200000000     C**----------------------------------------
026300000000     C** CARICAMENTO TIPOLOGIE ORDINI CON FLAG
026400000000     C** SCARICO MAGAZZINO = N
026500000000     C**----------------------------------------
026600000000     C                   MOVEL     '162'         GRU1
026700000000     C     KEYTAB        SETLL     ANTAB000                               07
026800000000     C  N07              GOTO      ENDVIS
026900000000     C                   DO        *HIVAL
027000000000     C     KEYTAB        READE     ANTAB000                               06
027100000000     C   06              GOTO      ENDVIS
027200000000     C                   MOVEL     TABDES        DSTAB
027300000000     C     TABTOR        IFEQ      'N'
027400000000     C                   ADD       1             K                 3 0
027500000000     C                   MOVE      TABCOD        TOR(K)
027600000000     C                   MOVEL     DESTOR        DTOR(K)
027700000000     C                   END
027800000000     C                   END
027900000000     C     ENDVIS        ENDSR
028000000000     C*****************************************************
028100000000     C     PRENUM        BEGSR
028200000000     C**---------------------------------------------
028300000000     C** PRELEVO DALLE DATE AREE OCLBL1/ MPHDT1 I
028400000000     C** VALORI INIZIALI INCREMENTO I NUMERI
028500000000     C** ED ESEGUO L'AGGIORNAMENTO
028600000000     C**---------------------------------------------
028700000000     C                   Z-ADD     0             BOLNUO
028800000000     C                   Z-ADD     0             MOVNUO
028900000000     C*          -------------
029000000000     C     STARX         TAG
029100000000     C*          -------------
029200000000     C*DATA AREA NUMERO BOLLE
029300000000     C     *LOCK         IN        OCLBL1                               46
029400000000     C   46              GOTO      STARX
029500000000     C                   Z-ADD     NUMBOL        BOLNUO            6 0
029600000000     C                   ADD       1             BOLNUO
029700000000     C     BOLNUO        IFEQ      0
029800000000     C                   ADD       1             BOLNUO
029900000000     C                   END
030000000000     C                   Z-ADD     BOLNUO        NUMBOL
030100000000     C                   Z-ADD     DATDEM        DATABL
030200000000     C                   OUT       OCLBL1
030300000000     C*          -------------
030400000000     C     START         TAG
030500000000     C*          -------------
030600000000     C*DATA AREA NUMERO MOVIMENTI
030700000000     C     *LOCK         IN        MPHDT1                               46
030800000000     C   46              GOTO      START
030900990128     c                   z-add     anndta        anndta4           4 0
031000990128     c                   if        anndta > 40
031100990128     c                   add       1900          anndta4
031200990128     c                   else
031300990128     c                   add       2000          anndta4
031400990128     c                   end
031500000000     C                   Z-ADD     NUMMOV        MOVNUO            5 0
031600000000     C                   ADD       1             MOVNUO
031700000000     C     MOVNUO        IFEQ      0
031800000000     C                   ADD       1             MOVNUO
031900000000     C                   END
032000000000     C                   Z-ADD     MOVNUO        NUMMOV
032100000000     C                   Z-ADD     BOLDEM        DATDTA
032200000000     C                   OUT       MPHDT1
032300000000     C                   ENDSR
032400000000     C******************************************************
032500000000     C     RICANA        BEGSR
032600000000     C**----------------------------------------
032700000000     C** RICERCA DATI ANAGRAFICI DEL CLIENTE
032800000000     C**----------------------------------------
032900990128     C     KEYcln        CHAIN     ancln01l                           20
033000990128     C     clnsogg       CHAIN     ansog01l                           20
033100990128     C     clnsogg       CHAIN     anind01l                           21
033200000000     C                   MOVEL     *BLANKS       RAG
033300000000     C                   MOVEL     *BLANKS       RAG1
033400000000     C                   MOVEL     *BLANKS       VIA
033500990129     C                   move      *blank        CAP1
033600000000     C                   MOVEL     *BLANKS       CIT
033700000000     C                   MOVEL     *BLANKS       PRV
033800871223     C                   MOVEL     *BLANKS       RAGLUO
033900871223     C                   MOVEL     *BLANKS       RAGLU1
034000871223     C                   MOVEL     *BLANKS       INDLUO
034100871223     C                   MOVEL     *BLANKS       LOCLUO
034200871223     C                   MOVEL     *BLANKS       PROLUO
034300990129     C                   MOVEL     *blank        CAPLUO
034400990128     C  N20              MOVEL     sogdes        RAG              22
034500990129     C  N20              MOVE      sogdes        RAG1             22
034600990128     C  N21              MOVEL     INDindriz     VIA              30
034700990129     C  N21              movel     INDCAP        CAP1              5
034800990128     C  N21              MOVEL     INDlocalit    CIT              25
034900990128     C  N21              MOVEL     INDPRov       PRV               2
035000000000     C* LUOGO DI SPEDIZIONE UGUALE A CLIENTE
035100990128     C  N20              MOVEL     sogdes        RAGLUO           22
035200990128     C  N20              MOVE      sogdes        RAGLU1           22
035300990128     C  N21              MOVEL     INDindriz     INDLUO           30
035400990129     C  N21              MOVEL     INDCAP        CAPLUO            5
035500990128     C  N21              MOVEL     INDlocalit    LOCLUO           20
035600990128     C  N21              MOVEL     INDPRov       PROLUO            2
035700000000     C     MOCLUO        IFNE      *BLANKS
035800970801     C                   MOVEL     'L'           SPEFLS
035900000000     C                   MOVEL     MOCCLI        SPECLI
036000000000     C                   MOVEL     MOCLUO        CODSPE
036100000000     C     KEYSPE        CHAIN     ANSPE3                             23
036200000000     C  N23              DO
036300871223     C                   MOVEL     *BLANKS       RAGLUO
036400871223     C                   MOVEL     *BLANKS       RAGLU1
036500970801     C                   CLEAR                   CAPLUO
036600871223     C                   MOVEL     *BLANKS       LOCLUO
036700871223     C                   MOVEL     *BLANKS       PROLUO
036800871223     C*
036900871223     C                   MOVEL     SPERAG        RAGLUO
037000871223     C                   MOVE      SPERAG        RAGLU1
037100871223     C                   MOVEL     SPEIND        INDLUO
037200970801     C                   MOVEL     SPECAP        CAPLUO
037300871223     C                   MOVEL     SPELOC        LOCLUO
037400871223     C                   MOVEL     SPEPRO        PROLUO
037500000000     C                   END
037600000000     C                   END
037700000000     C** IMPOSTA LUOGO SE ESISTE SULL'ORDINE
037800000000     C*RICERCA DEL CODICE IMBALLO SULLA TABELLA '008'
037900000000     C                   MOVE      '008'         TABGRU
038000000000     C                   MOVE      *ZEROS        TABCOD
038100000000     C                   MOVE      MOCIMB        TABCOD
038200000000     C     KEYANT        CHAIN     ANTAB00F                           47
038300000000     C   47              MOVEL     *BLANKS       DESIMB
038400000000     C  N47              MOVEL     TABDES        DESIMB           26
038500000000     C*RICERCA DEL CODICE RESA SULLA TABELLA '019'
038600000000     C                   MOVE      '019'         TABGRU
038700000000     C                   MOVE      *ZEROS        TABCOD
038800000000     C                   MOVE      MOCRES        TABCOD
038900000000     C     KEYANT        CHAIN     ANTAB00F                           47
039000000000     C   47              MOVEL     *BLANKS       DSPOR
039100000000     C  N47              MOVEL     TABDES        DSPOR
039200000000     C*RICERCA CAUSALE DEL TRASPORTO
039300000000     C                   MOVE      '707'         TABGRU
039400000000     C                   MOVEL     *BLANKS       TABCOD
039500000000     C                   MOVE      MOCTRO        TABCOD
039600000000     C     KEYANT        CHAIN     ANTAB00F                           47
039700000000     C   47              MOVEL     *BLANKS       DESTRO
039800000000     C  N47              MOVEL     TABDES        DESTRO           30
039900000000     C*RICERCA DEL SPEDIZIONIERE
040000970801     C                   MOVEL     'S'           SPEFLS
040100990128     C                   MOVEL     9999999       SPECLI
040200000000     C                   MOVEL     MOCSPE        CODSPE
040300000000     C     KEYSPE        CHAIN     ANSPE1                             24
040400000000     C   24              DO
040500000000     C                   MOVEL     *BLANKS       DESSPE
040600000000     C                   MOVEL     *BLANKS       INDSPE
040700000000     C                   MOVEL     *BLANKS       LOCSPE
040800000000     C                   END
040900000000     C  N24              DO
041000871223     C                   MOVEL     SPERAG        DESSPE           20
041100871223     C                   MOVEL     SPEIND        INDSPE           20
041200871223     C                   MOVEL     SPELOC        LOCSPE           15
041300000000     C                   END
041400000000     C                   ENDSR
041500000000     C******************************************************
041600000000     C     MEMCOM        BEGSR
041700000000     C**----------------------------------------
041800000000     C** MEMORIZZAZIONE COMMENTI
041900000000     C**----------------------------------------
042000000000     C                   Z-ADD     0             A
042100000000     C                   DO        10            A                 3 0
042200000000     C     COM(A)        IFEQ      *BLANKS
042300000000     C                   MOVEL     MOCDMT        COM(A)
042400000000     C                   GOTO      ENDCOM
042500000000     C                   END
042600000000     C                   END
042700000000     C     ENDCOM        ENDSR
042800000000     C******************************************************
042900000000     C     STACOM        BEGSR
043000000000     C**----------------------------------------
043100000000     C** STAMPA COMMENTI
043200000000     C**----------------------------------------
043300000000     C                   DO        10            A
043400000000     C     COM(A)        IFNE      *BLANKS
043500000000     C                   EXCEPT    SCOM
043600000000     C                   END
043700000000     C                   END
043800000000     C                   MOVEA     *BLANKS       COM
043900000000     C                   ENDSR
044000000000     C******************************************************
044100000000     C     CALPRZ        BEGSR
044200000000     C**----------------------------
044300000000     C** CALCOLO DEL PREZZO SCONTATO
044400000000     C**----------------------------
044500000000     C                   Z-ADD     MOCPRZ        PRENET           11 2
044600000000     C     PRENET        MULT      MOCSC1        SCONTO           11 2
044700000000     C     SCONTO        DIV       100           SCONTO
044800000000     C     PRENET        SUB       SCONTO        PRENET
044900000000     C     PRENET        MULT      MOCSC2        SCONTO
045000000000     C     SCONTO        DIV       100           SCONTO
045100000000     C     PRENET        SUB       SCONTO        PRENET
045200000000     C     PRENET        MULT      MOCSC3        SCONTO
045300000000     C     SCONTO        DIV       100           SCONTO
045400000000     C     PRENET        SUB       SCONTO        PRENET
045500000000     C                   ENDSR
045600000000     C******************************************************
045700000000     C     CREMOV        BEGSR
045800000000     C**---------------------------------------
045900000000     C** CREAZIONE DEI MOVIVENTI DI MAGAZZINO
046000000000     C**---------------------------------------
046100000000     C     STARTV        TAG
046200000000     C*INCREMENTO E AGGIORNO DATA AREA OFNDT2
046300000000     C*NUMERATORE CODICE INDIVIDUALE DI RIGA
046400000000     C     *LOCK         IN        OFNDT2                               46
046500000000     C   46              GOTO      STARTV
046600000000     C                   Z-ADD     NUMCRG        NUMMAG            7 0
046700000000     C                   ADD       1             NUMMAG
046800000000     C     NUMMAG        IFEQ      0
046900000000     C                   ADD       1             NUMMAG
047000000000     C                   END
047100000000     C                   Z-ADD     NUMMAG        NUMCRG
047200000000     C                   OUT       OFNDT2
047300000000     C*----------
047400000000     C                   MOVEL     *BLANKS       MMPATT
047500000000     C                   Z-ADD     MOCUTE        MMPUTE
047600000000     C                   Z-ADD     DATDEM        MMPDMN
047700000000     C                   MOVEL     MOCMAG        MMPMAG
047800990128     C                   MOVE      ANNDTA4       MMPAAC
047900000000     C                   Z-ADD     DATDEM        MMPDMV
048000000000     C                   Z-ADD     MOVNUO        MMPNMV
048100000000     C     SETUP         TAG
048200990128     C     KEYMAT        CHAIN     tmamp01L                           0725
048300000000     C   25              GOTO      SETUP
048400000000     C  N07              DO
048500000000     C     AZ(7)         IFEQ      '+'
048600990128     C     DATDEM        IFGT      amptar
048700990128     C                   Z-ADD     DATDEM        amptar
048800990128     C                   UPDATE    tmamp000
048900000000     C                   END
049000000000     C                   END
049100000000     C                   END
049200000000     C** ULTIMA USCITA PER VENDITA
049300000000     C                   Z-ADD     DATDEM        MMPDDC
049400000000     C                   Z-ADD     BOLNUO        MMPNDC
049500000000     C                   MOVEL     TABCAU        MMPCAU
049600000000     C                   MOVEA     AZ            MMPOPC
049700000000     C                   MOVEL     MOCCDC        MMPCDC
049800000000     C                   MOVEL     MOCMAT        MMPMAT
049900000000     C                   MOVEL     MOCUMQ        MMPUMA
050000000000     C                   Z-ADD     MOCQTA        MMPQTA
050100000000     C                   Z-ADD     PRENET        MMPPRZ
050200000000     C                   MOVEL     MOCMAG        MMPMCP
050300000000     C                   MOVE      '153'         TABGRU
050400000000     C                   MOVEL     MOCMAG        TABCOD
050500000000     C     KEYANT        CHAIN     ANTAB00F                           58
050600000000     C  N58              DO
050700000000     C                   MOVEL     TABDES        DSMAG
050800000000     C                   MOVEL     TIG153        MMPFGQ
050900000000     C                   MOVE      TIG153        MMPFGV
051000000000     C                   MOVE      TIM153        MMPFGT
051100000000     C                   END
051200000000     C                   Z-ADD     NUMMAG        MMPCRG
051300000000     C                   MOVEL     *BLANKS       MMPFLG
051400990128     C                   WRITE     tmmmp000                             48
051500000000     C                   ENDSR
051600000000     C*-----------------------------------------------------
051700000000     C     AGGSAL        BEGSR
051800000000     C**---------------------------------------
051900000000     C** CALCOLO LA DISPONIBILITA' A MAGAZZINO
052000000000     C**---------------------------------------
052100000000     C     SULA          TAG
052200990128     C     KEYSAL        CHAIN     tmsmp000                           5226
052300000000     C   26              GOTO      SULA
052400000000     C     SMPQIM        SUB       MOCQTA        SMPQIM
052500000000     C*--------- AGGIORNAMENTO ENTRATA --------------------*
052600000000     C     1             DO        5             X                 2 0          *
052700000000     C     AZE(X)        COMP      '+'                                    92    *
052800000000     C     AZE(X)        COMP      '-'                                    91    *
052900000000     C   92EN(X)         ADD       MOCQTA        EN(X)                          *
053000000000     C   91EN(X)         SUB       MOCQTA        EN(X)                          *
053100000000     C                   END                                                    *
053200000000     C*--------- AGGIORNAMENTO USCITA ---------------------*
053300000000     C     1             DO        5             X                              *
053400000000     C     AZU(X)        COMP      '+'                                    92    *
053500000000     C     AZU(X)        COMP      '-'                                    91    *
053600000000     C   92US(X)         ADD       MOCQTA        US(X)                          *
053700000000     C   91US(X)         SUB       MOCQTA        US(X)                          *
053800000000     C                   END                                                    *
053900000000     C     AZG(5)        COMP      '+'                                    92    *
054000000000     C     AZG(5)        COMP      '-'                                    91    *
054100000000     C   92SMPQGF        ADD       MOCQTA        SMPQGF
054200000000     C   91SMPQGF        SUB       MOCQTA        SMPQGF
054300990128     C                   UPDATE    tmsmp000
054400000000     C                   ENDSR
054500000000     C******************************************************
054600000000     C     AGGORD        BEGSR
054700000000     C**-------------------------------------------
054800990128     C** AGGIORNAMENTO RIGHE DI tmmoc00F DA AS A BB
054900000000     C** AGGIORNAMENTO DEL N.BOLLA , DATA BOLLA
055000000000     C** E DEL LEGAME ORDINI/MAGAZZ.
055100000000     C**-------------------------------------------
055200000000     C                   MOVEL     'BB'          MOCFAS
055300000000     C                   Z-ADD     NUMBOL        MOCNBL
055400000000     C                   Z-ADD     DATDEM        MOCDBL
055500000000     C                   Z-ADD     NUMMAG        MOCCRG
055600990128     C                   UPDATE    tmmoc000
055700000000     C                   ENDSR
055800000000     C******************************************************
055900000000     C     LEGGI         BEGSR
056000000000     C**----------------------------------------
056100000000     C** LETTURA RIGHE
056200000000     C**----------------------------------------
056300000000     C     MOCCLI        IFNE      PRECLI
056400000000     C                   EXSR      RICANA
056500921005     C*PRELEVO E AGGIORNO I VALORI DALLE DATE AREE
056600921005     C                   EXSR      PRENUM
056700921005     C* SE FLGAI UGUALE A 1 (QUINDI SETRAS) E CAP DIVERSO DA 40138
056800921005     C* SCRIVO IL RECORD DELL'ARCHIVIO SPEDIZIONE
056900921005     C*
057000921005     C     FLGAI         IFEQ      1
057100020111     C*    CAPLUO        ANDNE     '40138'
057200921005     C                   EXSR      CARFBL
057300921005     C                   END
057400921005     C*
057500000000     C                   OPEN      BOLLEPRT
057600871219     C                   SETON                                        22
057700900718     C     MOCNRF        IFGT      0
057800900718     C                   SETON                                        25
057900900718     C                   END
058000000000     C                   EXCEPT    TESTA
058100000000     C                   EXCEPT    TESTA1
058200900718     C                   SETOFF                                       0625
058300000000     C                   Z-ADD     1             A                 3 0
058400000000     C     MOCTOR        LOOKUP    TOR(A)                                 06
058500000000     C   06              EXCEPT    RIGCOV
058600990202     C                   MOVEL     MOCCLI        PRECLI            8
058700000000     C                   END
058800871222     C   OF              DO
058900871222     C                   EXCEPT    SEGUE
059000871222     C                   EXCEPT    FONDO
059100871222     C                   EXCEPT    TESTA
059200871222     C                   END
059300920916     C* PRIMA DI STAMPARE LA QTA FACCIO LA CONVERSIONE IN LETTERE
059400920916     C* A.G. 16-9-92
059500920916     C                   MOVE      0             £CI              15 0
059600920916     C                   MOVE      MOCQTA        £CI
059700920916     C                   Z-ADD     2             $DC               2 0
059800920916     C***
059900920916     C                   EXSR      NUMALF
060000920916     C***
060100920916     C                   MOVE      $OUT17        STQTAL           11
060200920916     C** FINE VARAIAZIONE
060300000000     C                   EXCEPT    DETAIL
060400920916     C     MOCCOM        IFNE      *BLANKS
060500920916     C                   EXCEPT    DETCOM
060600920916     C                   END
060700000000     C*CALCOLO PREZZO
060800000000     C                   EXSR      CALPRZ
060900000000     C*CREAZIONE MOVIMENTI
061000000000     C                   EXSR      CREMOV
061100000000     C*AGGIORNAMENTO SALDI
061200000000     C                   EXSR      AGGSAL
061300000000     C*AGGIORNAMENTO RIGHE
061400000000     C                   EXSR      AGGORD
061500000000     C                   ENDSR
061600920916     C* ROUTINE CONVERSIONE NUMERI IN LETTERE ALFABETICHE
061700920916     C     NUMALF        BEGSR
061800920916     C*          ---------------
061900920916     C* PULISCO IL CAMPO FINALE
062000920916     C                   MOVE      *BLANKS       $O
062100920916     C* SE NUMERO NEGATIVO METTO SEGNO NELL'ULTIMO POSTO
062200920916     C     £CI           IFLT      0
062300920916     C                   MOVE      '-'           $O(17)
062400920916     C* LO RENDO POSISTIVO SE NO NON STAMPA L'ULTIMA LETTERA
062500920916     C     £CI           MULT      -1            £CI
062600920916     C                   ELSE
062700920916     C                   MOVE      ' '           $O(17)
062800920916     C                   END
062900920916     C* SI METTE IL CAMPO £CI NELLA SCHIERA DI INPUT $I
063000920916     C                   MOVE      £CI           £CI2             15
063100920916     C                   MOVEA     £CI2          $I
063200920916     C*
063300920916     C                   MOVE      0             £VIRG             1 0
063400920916     C                   MOVE      0             £VALID            1 0
063500920916     C* DETERMINO IN QUALE POSTO DEVO METTERE LA VIRGOLA
063600920916     C     15            SUB       $DC           £DE               2 0
063700920916     C     £DE           COMP      15                                     OD
063800920916     C* SE OD ACCESO NON DEVO STAMPARE LA VIRGOLA NELLA SCHIERA
063900920916     C* DI OUTPUT
064000920916     C  NOD              Z-ADD     1             J£                2 0
064100920916     C   OD              Z-ADD     2             J£
064200920916     C*
064300920916     C     1             DO        15            I£                2 0
064400920916     C     $I(I£)        COMP      '0'                                    OC
064500920916     C   OC£VALID        COMP      0                                      OC
064600920916     C   OC£VIRG         COMP      0                                      OC
064700920916     C   OC              MOVE      ' '           $O(J£)
064800920916     C*
064900920916     C  NOC              MOVE      1             £VALID
065000920916     C  NOC$I(I£)        LOOKUP    TAB$          TAB£                     OB
065100920916     C  NOC
065200920916     CAN OB              MOVE      TAB£          $O(J£)
065300920916     C  NOC
065400920916     CANNOB              MOVE      ' '           $O(J£)
065500920916     C* CONTROLLO SE DEVO STAMPARE LA VIRGOLA
065600920916     C  NODJ£            IFEQ      £DE
065700920916     C* CONTROLLO SE DEVO METTERE UNO ZERO PRIME DELLA VIRGOLA
065800920916     C     £VALID        IFEQ      0
065900920916     C     '0'           LOOKUP    TAB$          TAB£                     OB
066000920916     C   OB              MOVE      TAB£          $O(J£)
066100920916     C                   END
066200920916     C*
066300920916     C                   ADD       1             J£
066400920916     C                   MOVE      ','           $O(J£)
066500920916     C                   MOVE      1             £VIRG
066600920916     C                   END
066700920916     C*
066800920916     C                   ADD       1             J£
066900920916     C*
067000920916     C                   END
067100920916     C* ESEGUITA LA ROUTINE IL CAMPO $O CONTIENE LE LETTERE DA
067200920916     C* STAMPARE CON LA VIRGOLA O MENO. LO METTO IN UN CAMPO DI 17
067300920916     C                   MOVEA     $O            $OUT17           17
067400920916     C                   ENDSR
067500921005     C*-------------------------------------------------------------------
067600921005     C     CARFBL        BEGSR
067700021029     C                   CLEAR                   FIVAB000
067800921005     C*
067900921005     C* CODICE BOLLA PARTENZA
068000960108     C                   MOVE      'A '          VABCBO
068100921005     C* RAGIONE SOCIALE DESTINATARIO
068200960108     C                   MOVEL     RAGLUO        VABRSD
068300921005     C* INDIRIZZO DESTINATARIO
068400960108     C                   MOVEL     INDLUO        VABIND
068500921005     C* CAP DESTINATARIO
068600960108     C                   MOVEL     CAPLUO        VABCAD
068700921005     C* LOCALITA' DESTINATARIO
068800960108     C                   MOVEL     LOCLUO        VABLOD
068900921005     C* PROVINCIA DESTINATARIO
069000960108     C                   MOVEL     PROLUO        VABPRD
069100921005     C* TIPO SPEDIZIONE
069200980519     C                   MOVE      'C'           VABTSP
069300921005     C* NATURA MERCE
069400960108     C                   MOVE      GAI(2)        VABNAS
069500921005     C* RIFERIMENTO MITTENTE NUMERICO
069600960108     C                   Z-ADD     NUMBOL        VABRMN
069700921005     C* FLAG FERMO DEPOSITO
069800960108     C                   MOVE      'S'           VABFFD
069900980519     C* FORZO LNA E ZONA 00 SIA PER DESTINATARIO SDI CHE PER DESTIN.BAR
070000980112     C* (NON DOVREBBE VERIFICARSI QUESTO CASO PERCHE' PER SDI (COME PER
070100980112     C* BARTOLINI) VIENE STAMPATA SOLO UNA BOLLA INTERNA --> TRATTAMENT
070200980112     C* MERCEOLOGICO CHE NON PREVEDE LA BOLLA FISCALE)
070300980112     C                   MOVE      MOCCLI        W0030             3 0
070400990128     C                   MOVEL     MOCCLI        W0040             5 0
070500990128     C     W0040         IFEQ      00465
070600980112     C     W0030         CHAIN     AZORG01L                           30
070700980112     C     *IN30         IFEQ      *OFF
070800980112     C                   MOVE      ORGFIL        VABLNA
070900980112     C                   MOVE      00            VABZNC
071000980519     C     ORGDIT        IFEQ      'SDI'
071100980519     C                   MOVE      'D'           VABTSP
071200980112     C                   END
071300980519     C                   END
071400980112     C                   END
071500921005     C* SCRIVO IL RECORD
071600021029     C                   WRITE     FIVAB000
071700921005     C*
071800921005     C                   ENDSR
071900990128     C*----------------------------------------------------*
072000990128     C* Reperimento dati società
072100990128     C*----------------------------------------------------*
072200990128     C     REPSOC        BEGSR
072300990128     C*
072400990128     C                   CALLB     'XSOC'
072500990128     C                   PARM                    TIPXSC            6
072600990128     C                   PARM      socpas        SOCXSC            3
072700990128     C                   PARM                    CDSXSC            9 0
072800990128     C                   PARM                    MODXSC            3
072900990128     C                   PARM      *blanks       RTNXSC            1
073000990128     C                   PARM                    XSOCDS
073100990128     C                   PARM                    KPJBA
073200990128     C*
073300990128     C                   ENDSR
073400920916     C*-------------------------------------------------------------------
073500000000     O*--------------------------------------------------------------*
073600000000     O*     TESTATA                                                  *
073700000000     O*--------------------------------------------------------------*
073800871223     OBOLLEPRT  E            TESTA            01
073900940711     O               93      SIMVIA              31
074000940711     O               93                          37 'Tel.:'
074100990129     O               93      xsctel              59
074200000320     O                       NUMBOL        Z     68
074300000320     O                       BOLDEM              80 '0  /  /    '
074400940711     O          E            TESTA            02
074500940711     O               93      SIMCIT              31
074600990129     O               93      xsccap              41
074700990129     O               93      xscprv              44
074800990129     O               93                          49 'Fax:'
074900990129     O               93      xscfax              70
075000871228     O          E            TESTA            05
075100000000     O                       MOCCLI               8
075200871223     O                       RAGLUO              33
075300871223     O                       RAG                 69
075400871228     O          E            TESTA            06
075500871223     O                       RAGLU1              33
075600871223     O                       RAG1                69
075700871228     O          E            TESTA            07
075800871223     O                       INDLUO              39
075900871223     O                       VIA                 75
076000871228     O          E            TESTA            08
076100880111     O                       CAPLUO              14
076200871223     O                       LOCLUO              40
076300871223     O                       PROLUO              43
076400880111     O                       CAP1                50
076500871223     O                       CIT                 76
076600871223     O                       PRV                 79
076700871228     O          E            TESTA            11
076800871223     O                       DESSPE              24
076900871223     O                       INDSPE              45
077000871223     O                       LOCSPE              60
077100880105     O          E            TESTA            14
077200000000     O*--------------------------------------------------------------*
077300000000     O*     TESTATA DI DETTAGLIO                                     *
077400000000     O*--------------------------------------------------------------*
077500000000     O          E            TESTA1      1
077600990128     O                                           28 'NS.ORDINE N.'
077700990128     O                       MOCNOR        Z     34
077800990128     O                                           38 'DEL'
077900990128     O                       BOLDOR              49 '0  /  /    '
078000000320     O               25                          60 'RIF. VS. ORDINE'
078100000320     O               25      MOCNRF        Z     70
078200000320     O               25      DATDRF              80 '  /  /    '
078300000000     O*--------------------------------------------------------------*
078400000000     O*     DETTAGLIO                                                *
078500000000     O*--------------------------------------------------------------*
078600000000     O          E            DETAIL      1
078700871223     O                       MOCMAT              14
078800000000     O                       MOCDMT              48
078900000000     O                       MOCQTA        2     63
079000920916     O                       STQTAL              78
079100920916     O          E            DETCOM      1
079200000320     O                       MOCCOM              80
079300000000     O*----------------------------------------------------------------
079400000000     O          E            SEGUE
079500000320     O                                           80 '%'
079600000000     O*----------------------------------------------------------------
079700000000     O*--------------------------------------------------------------*
079800000000     O*     FONDO                                                    *
079900000000     O*--------------------------------------------------------------*
080000871228     O          E            FONDO            48
080100871228     O                       DESTRO              45
080200871228     O          E            FONDO            50
080300000000     O                       DESIMB              27
080400871228     O          E            FONDO            54
080500000000     O                       DESRES              27
080600871223     O                       T1                  30
080700871223     O                       T2                  34
080800871223     O                       T3                  38
080900921019     O          E            FONDO            58
081000921019     O               94      ST(1)               26
081100921019     O               94      ST(2)               50
081200000000     O*--------------------------------------------------------------*
081300000000     O*     STAMPA RIGA DI COMMENTO                                  *
081400000000     O*--------------------------------------------------------------*
081500000000     O          E            SCOM        1
081600000000     O                       COM(A)              48
081700000000     O          E            SCOM        0  0
081800000000     O                       COM(A)              48
081900000000     O*--------------------------------------------------------------*
082000000000     O*     STAMPA RIGA DI PROVA O IN CONTO VISIONE                  *
082100000000     O*--------------------------------------------------------------*
082200000000     O          E    06      RIGCOV      1
082300000000     O                       DTOR(A)             43
082400000000     O          E    06      RIGCOV      0  0
082500000000     O                       DTOR(A)             43
082600000000     O*--------------------------------------------------------------*
082700971210** TABELLA CORRELATA PER IL CAMBIO NUMERO LETTERA
0828009209160Z
0829009209161A
0830009209162E
0831009209163G
0832009209164H
0833009209165M
0834009209166P
0835009209167S
0836009209168T
0837009209169K
083800920925** ST TABELLA LEGENDA LEGGE MINISTERIALE
0839009209251=A,2=E,3=G,4=H,5=M,6=P,     ST,1
0840009209257=S,8=T,9=K,0=Z,VIRGOLA=,    ST,2
084100921005** GAI TABELLA CONTROLLO SISTEMA INFORMATIVO
084200921104GAITRA201
084300921005VEDI BAM
