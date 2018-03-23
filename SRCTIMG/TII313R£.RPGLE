000100990202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990202     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300990202     H* OCO361R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500000000     H*             - STAMPA BOLLE INTERNA    INTERATIVA -           *
000600000000     H*             --------------------------------------           *
000700000000     H*                                                              *
000800990202     H*         FILE : tmmoc01L - tmsmp01L - tmmmp00F                *
000900990202     H*                tmamp01L - CNACO00F - CNIND00F                *
001000021029     H*                ANTAB00F - FNSPE01L - FIVABWKF                *
001100000000     H*--------------------------------------------------------------*
001200990202     Ftmmoc01L  UF   E           K DISK
001300990202     Ftmmmp00F  O  A E           K DISK
001400990202     Ftmsmp01L  UF   E           K DISK
001500990202     Ftmamp01L  UF   E           K DISK
001600990202     Fancln01l  IF   E           K DISK
001700990202     Fansog01l  IF   E           K DISK
001800990202     Fanind01l  IF   E           K DISK
001900990202     Ftmclp01L  IF   E           K DISK
002000000000     FANTAB00F  IF   E           K DISK
002100980112     FAZORG01L  IF   E           K DISK
002200000000     F*--------- SPEDIZIONIERI --------------------------------------*
002300970801     FFNSPE01L  IF   E           K DISK
002400970801     F                                     RENAME(FNSPE000:ANSPE1)
002500000000     F*--------- LUOGHI DI DESTINAZIONE -----------------------------*
002600970801     FFNSPE03L  IF   E           K DISK
002700970801     F                                     RENAME(FNSPE000:ANSPE3)
002800021029     FFIVABWKF  O  A E           K DISK    USROPN
002900890609     FBOLLEINT  O    F  132        PRINTER OFLIND(*INOF)
003000890609     F                                     USROPN
003100000000     D*--------------------------------------------------------------*
003200000000     D TOR             S              2    DIM(10)                              *TIPO ORDINE
003300000000     D DTOR            S             25    DIM(10)                              *DESCRIZIONE
003400000000     D COM             S             30    DIM(10)                              *COMMENTO
003500000000     D AZ              S              1    DIM(15)                              *AZIONI CAUS.
003600890609     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 1
003700890609     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 2
003800890612     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              *NOTE 1
003900890612     D SK4             S             66    DIM(2) CTDATA PERRCD(1)              *NOTE 2
004000890720     D SK5             S             66    DIM(2) CTDATA PERRCD(1)              *NOTE 3
004100890720     D SK6             S             66    DIM(2) CTDATA PERRCD(1)              *NOTE 4
004200000000     D*--------------------------------------------------------------*
004300921104     D*--------------------------------------------------------------*
004400020925     D GAI             S             10    DIM(3) CTDATA PERRCD(1)
004500921104     D*--------------------------------------------------------------*
004600101014     D og148         E DS
004700101014     D DSMAG         E DS                  EXTNAME(ANTAB153)
004800000000     D*------------- TABELLA MAGAZZINI :
004900000000     D TB152         E DS                  EXTNAME(ANTAB152)
005000000000     D*------------- TABELLA CAUSALI :
005100000000     D  AZE                   30     34
005200000000     D                                     DIM(5)                               *ENTRATE
005300000000     D  AZU                   39     43
005400000000     D                                     DIM(5)                               *USCITE
005500000000     D  AZG                   48     52
005600000000     D                                     DIM(5)                               *GESTIONALI
005700990202     D DSSAL         E DS                  EXTNAME(tmsmp00F)
005800000000     D*------------- FILE SALDI:
005900990211     D  EN                    86    115P 2
006000000000     D                                     DIM(5)
006100990211     D  US                   116    145P 2
006200000000     D                                     DIM(5)
006300000000     D OFNDT2          DS           512
006400000000     D*------------- NUMERAT.CODICE INDIVIDUALE RIGA
006500000000     D  NUMCRG                 1      7  0
006600000000     D  FILDT2                 8    256
006700000000     D  FILDT3               257    512
006800000000     D MPHDT1          DS
006900000000     D*--- DATA AREA N.MOVIMENTO MAGAZZINO :
007000000000     D  NUMMOV                 1      5  0
007100000000     D  FILL1                  6      8
007200000000     D  DATDTA                 9     14  0
007300000000     D  FILL2                 15     17
007400000000     D  ANNDTA                18     19  0
007500000000     D  FIL03                 20     50
007600000000     D  DGIO                  51     56  0
007700000000     D  FIL04                 57    100
007800000000     D  FILL5                101    300
007900000000     D  GIOTOL               301    302  0
008000000000     D  FILL6                303    512
008100000000     D OCLBL2          DS
008200000000     D*--- DATA AREA N.BOLLA /DATA BOLLA :
008300000000     D  NUMBOL                 1      6  0
008400000000     D  DATABL                 7     12  0
008500000000     D DSBL1           DS
008600000000     D*--- DS TABELLA BL1
008700000000     D  TABCAU                 1      3
008800000000     D DSTAB           DS
008900000000     D*DESCRIZIONE TIPO ORDINE/FLAG VISIONE
009000000000     D  DESTOR                 1     25
009100000000     D  TABTOR                35     35
009200000000     D DSPOR           DS
009300000000     D*DESCRIZIONE DEL TRASPORTO + FLAG
009400000000     D  DESRES                 1     26
009500000000     D  T1                    41     41
009600000000     D  T2                    42     42
009700000000     D  T3                    43     43
009800000000     D PARAM2          DS
009900990202     D  BOLCLI                 1      8
010000990202     D  BOLNOR                 9     13  0
010100990202     D  BOLDEM                14     21  0
010200990202     D  BOLDOR                22     29  0
010300990202     D  socpas                30     32
010400990202     D KPJBA         E DS
010500990202     D soc001        E DS                  EXTNAME(xsoc001ds)
010600990202     D xsocds          DS          1000
010700990202     d dataiso         s               d   datfmt(*iso)
010800990202     d dataeur         s               d   datfmt(*eur)
010900000000     C*----------------------------------------------------*
011000000000     C     *ENTRY        PLIST                                                  *
011100000000     C                   PARM                    KPJBA                          *
011200000000     C     *DTAARA       DEFINE                  OFNDT2                         *
011300000000     C     *DTAARA       DEFINE                  MPHDT1                         *
011400000000     C     *DTAARA       DEFINE                  OCLBL2                         *
011500000000     C******************************************************
011600000000     C                   MOVEL     *BLANKS       BOLCLI                         *
011700000000     C                   Z-ADD     0             BOLNOR                         *
011800000000     C                   Z-ADD     0             BOLDEM                         *
011900000000     C                   Z-ADD     0             BOLDOR                         *
012000000000     C                   MOVEL     KPJBU         PARAM2                         *
012100990202     C*---------- RICERCA DITTA :
012200990202     C                   MOVEL     'SOC001'      TIPXSC
012300990202     C                   MOVEL     *blank        SOCXSC
012400990202     C                   EXSR      REPSOC
012500990202     C     RTNXSC        IFNE      '1'
012600990202     C                   MOVEL     XSOCDS        SOC001
012700990202     C                   MOVEL     xscrgs        desdit           20
012800990202     C                   MOVEL     xscrgs        DITRSC           35
012900990202     C                   MOVEL     xscvia        DITIND           35
013000990202     C                   movel     xsccap        DITCAP            5 0
013100990202     C                   MOVEL     xsccit        DITLOC           20
013200990202     C                   MOVEL     xscprv        DITPRV            2
013300990202     c                   end
013400990202     c                   time                    como14           14 0
013500990202     c                   move      como14        woggi             8 0
013501101021     c     *eur          movel     woggi         dataiso
013502101021     c                   movel     dataiso       DATEU             8 0
013600921104     C*----------------------------------------------------*
013700921104     C* CONTROLLO SE IL SISTEMA INFORMATIVO  E' RIFERITO A *
013800921104     C* GAITRA201 (BARTOLINI)                              *
013900921104     C     KNSIF         IFEQ      GAI(1)                                       *
014000971210     C     KNSIF         OREQ      GAI(3)                                       *
014100021029     C                   OPEN      FIVABWKF                                     *
014200921104     C                   Z-ADD     1             FLGAI             1 0
014300921104     C                   END                                                    *
014400000000     C******************************************************
014500990202     C*--- FILE tmmoc01L 'CODICE CLIENTE  N.ORDINE'        *
014600000000     C     KEYCLI        KLIST                                                  *
014700000000     C                   KFLD                    BOLCLI                         *
014800000000     C                   KFLD                    BOLNOR                         *
014900990202     C*--- FILE SALDI MAGAZZINO M.P. "tmsmp01L"            *
015000000000     C     KEYSAL        KLIST                                                  *
015100990202     C                   KFLD                    ANNDTA4           4 0          *
015200000000     C                   KFLD                    MOCMAG                         *
015300000000     C                   KFLD                    MOCMAT                         *
015400000000     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F" -------------*
015500000000     C     KEYANT        KLIST                                                  *
015600000000     C                   KFLD                    TABGRU                         *
015700000000     C                   KFLD                    TABCOD                         *
015800990202     C*--- FILE ANAG. MAT. PRIME "tmamp01L"                *
015900000000     C     KEYMAT        KLIST                                                  *
016000000000     C                   KFLD                    MOCMAT                         *
016100000000     C*--- CHIAVE PER RICERCA TIPO ORDINI 'ANTAB00F'-------*
016200000000     C     KEYTAB        KLIST                                                  *
016300000000     C                   KFLD                    GRU1              3            *
016400000000     C*--- CHIAVE PER RICERCA SPEDIZIONIERE/LUOGO          *
016500000000     C     KEYSPE        KLIST                                                  *
016600970801     C                   KFLD                    SPEFLS                         *
016700000000     C                   KFLD                    SPECLI                         *
016800000000     C                   KFLD                    CODSPE            3            *
016900000000     C*--- RICERCA RAG. SOC.CLIENTE 'CNACO00F'             *
017000990202     C     KEYcln        KLIST                                                  *
017100990202     C                   KFLD                    socpas                         *
017200990202     C                   KFLD                    bolcli                         *
017300000000     C*----------------------------------------------------*
017400000000     C*RIBALTAMENTO IN AAMMGG LA DATA DELL'ORDINE
017500000000     C*E DELLA DATA DI EMISSIONE
017600990202     c                   if        boldor > 0
017700990202     c                   move      boldor        dataeur
017800990202     c                   move      dataeur       dataiso
017900990202     c                   move      dataiso       datdor            8 0
018000990202     C                   end
018100990202     c                   if        boldem > 0
018200990202     c                   move      boldem        dataeur
018300990202     c                   move      dataeur       dataiso
018400990202     c                   move      dataiso       datdem            8 0
018500990202     C                   end
018600000000     C*RICERCA CAUSALE OPERATIVA
018700000000     C                   EXSR      RICCAU
018800000000     C*CARICAMENTO TIPOLOGIE ORDINI
018900000000     C                   EXSR      CARVIS
019000000000     C*POSIZIONAMENTO PER CLIENTE N.ORDINE
019100990202     C     KEYCLI        SETLL     tmmoc000                               04
019200000000     C  N04              GOTO      FINE
019300000000     C*LETTURA
019400000000     C     SU            TAG
019500990202     C     KEYCLI        READE     tmmoc000                               05
019600000000     C   05
019700000000     CAN 22              EXSR      STACOM
019800000000     C   05
019900000000     CAN 22              EXCEPT    FONDO
020000000000     C   05              GOTO      FINE
020100000000     C*RIBALTAMENTO IN GGMMAA DATA RIF.VS.ORD.
020200990202     c                   if        mocdrf > 0
020300990202     c                   move      mocdrf        dataiso
020400990202     c                   move      dataiso       dataeur
020500990202     c                   move      dataeur       datdrf            8 0
020600990202     c                   end
020700000000     C*
020800000000     C     MOCDOR        IFNE      DATDOR
020900000000     C                   GOTO      SU
021000000000     C                   END
021100000000     C     MOCTRG        CABEQ     '1'           LA
021200000000     C     MOCTRG        CABEQ     '6'           STAR
021300000000     C                   GOTO      SU
021400000000     C*----------------
021500000000     C     LA            TAG
021600000000     C     MOCTRG        IFEQ      '1'
021700000000     C     MOCFAS        CABNE     'AS'          SU
021800000000     C                   EXSR      LEGGI
021900000000     C                   END
022000000000     C     STAR          TAG
022100000000     C     MOCTRG        IFEQ      '6'
022200000000     C                   EXSR      MEMCOM
022300000000     C                   END
022400000000     C                   GOTO      SU
022500000000     C     FINE          TAG
022600000000     C                   SETON                                        LR
022700000000     C******************************************************
022800000000     C     RICCAU        BEGSR
022900000000     C**------------------------------
023000000000     C** RICERCA  CAUSALE OPERATIVA
023100000000     C**------------------------------
023200000000     C                   MOVE      '257'         TABGRU
023300000000     C                   MOVE      *ZEROS        TABCOD
023400000000     C                   MOVE      '1'           TABCOD
023500000000     C     KEYANT        CHAIN     ANTAB00F                           H1
023600000000     C   H1              GOTO      FINE
023700000000     C                   MOVEL     TABDES        DSBL1
023800000000     C*---------------
023900000000     C                   MOVE      '152'         TABGRU
024000000000     C                   MOVE      *ZEROS        TABCOD
024100000000     C                   MOVEL     'MP'          CINQUE            5
024200000000     C                   MOVE      TABCAU        CINQUE            5
024300000000     C                   MOVE      CINQUE        TABCOD
024400000000     C     KEYANT        CHAIN     ANTAB00F                           H1
024500000000     C   H1              GOTO      FINE
024600000000     C                   MOVEL     TABDES        TB152
024700000000     C                   MOVEA     AZE           AZ(1)
024800000000     C                   MOVEA     AZU           AZ(6)
024900000000     C                   MOVEA     AZG           AZ(11)
025000000000     C                   ENDSR
025100000000     C******************************************************
025200000000     C     CARVIS        BEGSR
025300000000     C**----------------------------------------
025400000000     C** CARICAMENTO TIPOLOGIE ORDINI CON FLAG
025500000000     C** SCARICO MAGAZZINO = N
025600000000     C**----------------------------------------
025700000000     C                   MOVEL     '162'         GRU1
025800000000     C     KEYTAB        SETLL     ANTAB000                               07
025900000000     C  N07              GOTO      ENDVIS
026000000000     C                   DO        *HIVAL
026100000000     C     KEYTAB        READE     ANTAB000                               06
026200000000     C   06              GOTO      ENDVIS
026300000000     C                   MOVEL     TABDES        DSTAB
026400000000     C     TABTOR        IFEQ      'N'
026500000000     C                   ADD       1             K                 3 0
026600000000     C                   MOVE      TABCOD        TOR(K)
026700000000     C                   MOVEL     DESTOR        DTOR(K)
026800000000     C                   END
026900000000     C                   END
027000000000     C     ENDVIS        ENDSR
027100000000     C*****************************************************
027200000000     C     PRENUM        BEGSR
027300000000     C**---------------------------------------------
027400000000     C** PRELEVO DALLE DATE AREE OCLBL2/ MPHDT1 I
027500000000     C** VALORI INIZIALI INCREMENTO I NUMERI
027600000000     C** ED ESEGUO L'AGGIORNAMENTO
027700000000     C**---------------------------------------------
027800000000     C                   Z-ADD     0             BOLNUO
027900000000     C                   Z-ADD     0             MOVNUO
028000000000     C*          -------------
028100000000     C     STARX         TAG
028200000000     C*          -------------
028300000000     C*DATA AREA NUMERO BOLLE
028400000000     C     *LOCK         IN        OCLBL2                               46
028500000000     C   46              GOTO      STARX
028600000000     C                   Z-ADD     NUMBOL        BOLNUO            6 0
028700000000     C                   ADD       1             BOLNUO
028800000000     C     BOLNUO        IFEQ      0
028900000000     C                   ADD       1             BOLNUO
029000000000     C                   END
029100000000     C                   Z-ADD     BOLNUO        NUMBOL
029200000000     C                   Z-ADD     DATDEM        DATABL
029300000000     C                   OUT       OCLBL2
029400000000     C*          -------------
029500000000     C     START         TAG
029600000000     C*          -------------
029700000000     C*DATA AREA NUMERO MOVIMENTI
029800000000     C     *LOCK         IN        MPHDT1                               46
029900000000     C   46              GOTO      START
030000990202     c                   z-add     anndta        anndta4
030100990202     c                   if        anndta4 > 40
030200990202     c                   add       1900          anndta4
030300990202     c                   else
030400990202     c                   add       2000          anndta4
030500990202     c                   end
030600000000     C                   Z-ADD     NUMMOV        MOVNUO            5 0
030700000000     C                   ADD       1             MOVNUO
030800000000     C     MOVNUO        IFEQ      0
030900000000     C                   ADD       1             MOVNUO
031000000000     C                   END
031100000000     C                   Z-ADD     MOVNUO        NUMMOV
031200000000     C                   Z-ADD     BOLDEM        DATDTA
031300000000     C                   OUT       MPHDT1
031400000000     C                   ENDSR
031500000000     C******************************************************
031600000000     C     RICANA        BEGSR
031700000000     C**----------------------------------------
031800000000     C** RICERCA DATI ANAGRAFICI DEL CLIENTE
031900000000     C**----------------------------------------
032000961029     C                   SETOFF                                         7374
032100990202     C     KEYcln        CHAIN     ancln01l                           20
032200990202     C     clnsogg       CHAIN     ansog01l                           20
032300990202     C     clnsogg       CHAIN     anind01l                           21
032400000000     C                   MOVEL     *BLANKS       RAG
032500000000     C                   MOVEL     *BLANKS       RAG1
032600000000     C                   MOVEL     *BLANKS       VIA
032700990202     C                   move      *zeros        CAP1
032800000000     C                   MOVEL     *BLANKS       CIT
032900000000     C                   MOVEL     *BLANKS       PRV
033000000000     C                   MOVEL     *BLANKS       RAGLUO
033100000000     C                   MOVEL     *BLANKS       RAGLU1
033200000000     C                   MOVEL     *BLANKS       INDLUO
033300000000     C                   MOVEL     *BLANKS       LOCLUO
033400000000     C                   MOVEL     *BLANKS       PROLUO
033500000000     C                   MOVEL     *ZEROS        CAPLUO
033600990202     C  N20              MOVEL     sogdes        RAG              22
033700990202     C  N20              MOVE      sogdes        RAG1             22
033800990202     C  N21              MOVEL     INDindriz     VIA              30
033900990202     C  N21              movel     INDCAP        CAP1              5
034000990202     C  N21              MOVEL     INDlocalit    CIT              25
034100990202     C  N21              MOVEL     INDPRov       PRV               2
034200990202     C* LUOGO DI SPEDIZIONE UGUALE A CLIENTE
034300990202     C  N20              MOVEL     sogdes        RAGLUO           22
034400990202     C  N20              MOVE      sogdes        RAGLU1           22
034500990202     C  N21              MOVEL     INDindriz     INDLUO           30
034600990202     C  N21              MOVEL     INDCAP        CAPLUO            5
034700990202     C  N21              MOVEL     INDlocalit    LOCLUO           20
034800990202     C  N21              MOVEL     INDPRov       PROLUO            2
034900000000     C     MOCLUO        IFNE      *BLANKS
035000970801     C                   MOVEL     'L'           SPEFLS
035100000000     C                   MOVEL     MOCCLI        SPECLI
035200000000     C                   MOVEL     MOCLUO        CODSPE
035300000000     C     KEYSPE        CHAIN     ANSPE3                             23
035400000000     C  N23              DO
035500000000     C                   MOVEL     *BLANKS       RAGLUO
035600000000     C                   MOVEL     *BLANKS       RAGLU1
035700970801     C                   CLEAR                   CAPLUO
035800000000     C                   MOVEL     *BLANKS       LOCLUO
035900000000     C                   MOVEL     *BLANKS       PROLUO
036000000000     C*
036100000000     C                   MOVEL     SPERAG        RAGLUO
036200000000     C                   MOVE      SPERAG        RAGLU1
036300000000     C                   MOVEL     SPEIND        INDLUO
036400970801     C                   MOVEL     SPECAP        CAPLUO
036500000000     C                   MOVEL     SPELOC        LOCLUO
036600000000     C                   MOVEL     SPEPRO        PROLUO
036700000000     C                   END
036800000000     C                   END
036900000000     C** IMPOSTA LUOGO SE ESISTE SULL'ORDINE
037000000000     C*RICERCA DEL CODICE IMBALLO SULLA TABELLA '008'
037100000000     C                   MOVE      '008'         TABGRU
037200000000     C                   MOVE      *ZEROS        TABCOD
037300000000     C                   MOVE      MOCIMB        TABCOD
037400000000     C     KEYANT        CHAIN     ANTAB00F                           47
037500000000     C   47              MOVEL     *BLANKS       DESIMB
037600000000     C  N47              MOVEL     TABDES        DESIMB           26
037700000000     C*RICERCA DEL CODICE RESA SULLA TABELLA '019'
037800000000     C                   MOVE      '019'         TABGRU
037900000000     C                   MOVE      *ZEROS        TABCOD
038000000000     C                   MOVE      MOCRES        TABCOD
038100000000     C     KEYANT        CHAIN     ANTAB00F                           47
038200000000     C   47              MOVEL     *BLANKS       DSPOR
038300000000     C  N47              MOVEL     TABDES        DSPOR
038400000000     C*RICERCA CAUSALE DEL TRASPORTO
038500000000     C                   MOVE      '707'         TABGRU
038600000000     C                   MOVEL     *BLANKS       TABCOD
038700000000     C                   MOVE      MOCTRO        TABCOD
038800000000     C     KEYANT        CHAIN     ANTAB00F                           47
038900000000     C   47              MOVEL     *BLANKS       DESTRO
039000000000     C  N47              MOVEL     TABDES        DESTRO           30
039100000000     C*RICERCA DEL SPEDIZIONIERE
039200970801     C                   MOVEL     'S'           SPEFLS
039300000000     C                   MOVEL     9999999       SPECLI
039400000000     C                   MOVEL     MOCSPE        CODSPE
039500000000     C     KEYSPE        CHAIN     ANSPE1                             24
039600000000     C   24              DO
039700000000     C                   MOVEL     *BLANKS       DESSPE
039800000000     C                   MOVEL     *BLANKS       INDSPE
039900000000     C                   MOVEL     *BLANKS       LOCSPE
040000000000     C                   END
040100000000     C  N24              DO
040200000000     C                   MOVEL     SPERAG        DESSPE           20
040300000000     C                   MOVEL     SPEIND        INDSPE           20
040400000000     C                   MOVEL     SPELOC        LOCSPE           15
040500990202     C     MOCCLI        CHAIN     tmclp000                           70
040600890612     C  N70              DO
040700890718     C*AZIENDA
040800890612     C     CLPSTM        IFEQ      '52'
040900980112     C     CLPSTM        OREQ      '57'
041000961029     C                   SETON                                        73
041100890612     C                   END
041200890718     C*FILIALE
041300890718     C     CLPSTM        IFEQ      '53'
041400980112     C     CLPSTM        OREQ      '56'
041500890718     C                   SETON                                        73
041600890718     C                   END
041700890718     C*AZIENDA
041800890718     C     CLPSTM        IFEQ      '54'
041900890718     C                   SETON                                        74
042000890718     C                   END
042100890612     C                   END
042200000000     C                   END
042300000000     C                   ENDSR
042400000000     C******************************************************
042500000000     C     MEMCOM        BEGSR
042600000000     C**----------------------------------------
042700000000     C** MEMORIZZAZIONE COMMENTI
042800000000     C**----------------------------------------
042900000000     C                   Z-ADD     0             A
043000000000     C                   DO        10            A                 3 0
043100000000     C     COM(A)        IFEQ      *BLANKS
043200000000     C                   MOVEL     MOCDMT        COM(A)
043300000000     C                   GOTO      ENDCOM
043400000000     C                   END
043500000000     C                   END
043600000000     C     ENDCOM        ENDSR
043700000000     C******************************************************
043800000000     C     STACOM        BEGSR
043900000000     C**----------------------------------------
044000000000     C** STAMPA COMMENTI
044100000000     C**----------------------------------------
044200000000     C                   DO        10            A
044300000000     C     COM(A)        IFNE      *BLANKS
044400000000     C                   EXCEPT    SCOM
044500000000     C                   END
044600000000     C                   END
044700000000     C                   MOVEA     *BLANKS       COM
044800000000     C                   ENDSR
044900000000     C******************************************************
045000000000     C     CALPRZ        BEGSR
045100000000     C**----------------------------
045200000000     C** CALCOLO DEL PREZZO SCONTATO
045300000000     C**----------------------------
045400000000     C                   Z-ADD     MOCPRZ        PRENET           11 2
045500000000     C     PRENET        MULT      MOCSC1        SCONTO           11 2
045600000000     C     SCONTO        DIV       100           SCONTO
045700000000     C     PRENET        SUB       SCONTO        PRENET
045800000000     C     PRENET        MULT      MOCSC2        SCONTO
045900000000     C     SCONTO        DIV       100           SCONTO
046000000000     C     PRENET        SUB       SCONTO        PRENET
046100000000     C     PRENET        MULT      MOCSC3        SCONTO
046200000000     C     SCONTO        DIV       100           SCONTO
046300000000     C     PRENET        SUB       SCONTO        PRENET
046400000000     C                   ENDSR
046500000000     C******************************************************
046600000000     C     CREMOV        BEGSR
046700000000     C**---------------------------------------
046800000000     C** CREAZIONE DEI MOVIVENTI DI MAGAZZINO
046900000000     C**---------------------------------------
047000000000     C     STARTV        TAG
047100000000     C*INCREMENTO E AGGIORNO DATA AREA OFNDT2
047200000000     C*NUMERATORE CODICE INDIVIDUALE DI RIGA
047300000000     C     *LOCK         IN        OFNDT2                               46
047400000000     C   46              GOTO      STARTV
047500000000     C                   Z-ADD     NUMCRG        NUMMAG            7 0
047600000000     C                   ADD       1             NUMMAG
047700000000     C     NUMMAG        IFEQ      0
047800000000     C                   ADD       1             NUMMAG
047900000000     C                   END
048000000000     C                   Z-ADD     NUMMAG        NUMCRG
048100000000     C                   OUT       OFNDT2
048200000000     C*----------
048300000000     C                   MOVEL     *BLANKS       MMPATT
048400000000     C                   Z-ADD     MOCUTE        MMPUTE
048500000000     C                   Z-ADD     DATDEM        MMPDMN
048600000000     C                   MOVEL     MOCMAG        MMPMAG
048700990202     C                   MOVE      ANNDTA4       MMPAAC
048800000000     C                   Z-ADD     DATDEM        MMPDMV
048900000000     C                   Z-ADD     MOVNUO        MMPNMV
049000000000     C     SETUP         TAG
049100990202     C     KEYMAT        CHAIN     tmamp01L                           0725
049200000000     C   25              GOTO      SETUP
049300000000     C  N07              DO
049400000000     C     AZ(7)         IFEQ      '+'
049500990202     C     DATDEM        IFGT      amptar
049600990202     C                   Z-ADD     DATDEM        ampTAR
049700990202     C                   UPDATE    tmamp000
049800000000     C                   END
049900000000     C                   END
050000000000     C                   END
050100000000     C** ULTIMA USCITA PER VENDITA
050200000000     C                   Z-ADD     DATDEM        MMPDDC
050300000000     C                   Z-ADD     BOLNUO        MMPNDC
050400000000     C                   MOVEL     TABCAU        MMPCAU
050500000000     C                   MOVEA     AZ            MMPOPC
050600000000     C                   MOVEL     MOCCDC        MMPCDC
050700000000     C                   MOVEL     MOCMAT        MMPMAT
050800000000     C                   MOVEL     MOCUMQ        MMPUMA
050900000000     C                   Z-ADD     MOCQTA        MMPQTA
051000000000     C                   Z-ADD     PRENET        MMPPRZ
051100000000     C                   MOVEL     MOCMAG        MMPMCP
051200000000     C                   MOVE      '153'         TABGRU
051300000000     C                   MOVEL     MOCMAG        TABCOD
051400000000     C     KEYANT        CHAIN     ANTAB00F                           58
051500000000     C  N58              DO
051600000000     C                   MOVEL     TABDES        DSMAG
051700000000     C                   MOVEL     TIG153        MMPFGQ
051800000000     C                   MOVE      TIG153        MMPFGV
051900000000     C                   MOVE      TIM153        MMPFGT
052000000000     C                   END
052100000000     C                   Z-ADD     NUMMAG        MMPCRG
052200000000     C                   MOVEL     *BLANKS       MMPFLG
052300990202     C                   WRITE     tmmmp000                             48
052400000000     C                   ENDSR
052500000000     C*-----------------------------------------------------
052600000000     C     AGGSAL        BEGSR
052700000000     C**---------------------------------------
052800000000     C** CALCOLO LA DISPONIBILITA' A MAGAZZINO
052900000000     C**---------------------------------------
053000000000     C     SULA          TAG
053100990202     C     KEYSAL        CHAIN     tmsmp000                           5226
053200000000     C   26              GOTO      SULA
053300000000     C     SMPQIM        SUB       MOCQTA        SMPQIM
053400000000     C*--------- AGGIORNAMENTO ENTRATA --------------------*
053500000000     C     1             DO        5             X                 2 0          *
053600000000     C     AZE(X)        COMP      '+'                                    92    *
053700000000     C     AZE(X)        COMP      '-'                                    91    *
053800000000     C   92EN(X)         ADD       MOCQTA        EN(X)                          *
053900000000     C   91EN(X)         SUB       MOCQTA        EN(X)                          *
054000000000     C                   END                                                    *
054100000000     C*--------- AGGIORNAMENTO USCITA ---------------------*
054200000000     C     1             DO        5             X                              *
054300000000     C     AZU(X)        COMP      '+'                                    92    *
054400000000     C     AZU(X)        COMP      '-'                                    91    *
054500000000     C   92US(X)         ADD       MOCQTA        US(X)                          *
054600000000     C   91US(X)         SUB       MOCQTA        US(X)                          *
054700000000     C                   END                                                    *
054800000000     C     AZG(5)        COMP      '+'                                    92    *
054900000000     C     AZG(5)        COMP      '-'                                    91    *
055000000000     C   92SMPQGF        ADD       MOCQTA        SMPQGF
055100000000     C   91SMPQGF        SUB       MOCQTA        SMPQGF
055200990202     C                   UPDATE    tmsmp000
055300000000     C                   ENDSR
055400000000     C******************************************************
055500000000     C     AGGORD        BEGSR
055600000000     C**-------------------------------------------
055700990202     C** AGGIORNAMENTO RIGHE DI tmmoc00F DA AS A FA
055800000000     C** AGGIORNAMENTO DEL N.BOLLA , DATA BOLLA
055900000000     C** E DEL LEGAME ORDINI/MAGAZZ.
056000000000     C**-------------------------------------------
056100000000     C                   MOVEL     'FA'          MOCFAS
056200000000     C                   Z-ADD     NUMBOL        MOCNBL
056300000000     C                   Z-ADD     DATDEM        MOCDBL
056400000000     C                   Z-ADD     NUMMAG        MOCCRG
056500000000     C                   Z-ADD     NUMBOL        MOCNFT
056600000000     C                   MOVEL     9             MOCNFT
056700000000     C** NUMERO FATTURA = NUMERO BOLLA CON 9 DAVANTI
056800000000     C                   Z-ADD     DATDEM        MOCDFT
056900000000     C** DATA FATTURA = DATA BOLLA
057000990202     C                   UPDATE    tmmoc000
057100000000     C                   ENDSR
057200000000     C******************************************************
057300000000     C     LEGGI         BEGSR
057400000000     C**----------------------------------------
057500000000     C** LETTURA RIGHE
057600000000     C**----------------------------------------
057700000000     C     MOCCLI        IFNE      PRECLI
057800000000     C                   EXSR      RICANA
057900000000     C*PRELEVO E AGGIORNO I VALORI DALLE DATE AREE
058000000000     C                   EXSR      PRENUM
058100921104     C* SE FLGAI UGUALE A 1 (QUINDI SETRAS) E CAP DIVERSO DA 40138
058200921104     C* SCRIVO IL RECORD DELL'ARCHIVIO SPEDIZIONE
058300921104     C*
058400921104     C     FLGAI         IFEQ      1
058500020111     C*    CAPLUO        ANDNE     '40138'
058600921104     C                   EXSR      CARFBL
058700921104     C                   END
058800890609     C                   OPEN      BOLLEINT
058900000000     C                   SETON                                        22
059000900718     C     MOCNRF        IFGT      0
059100900718     C                   SETON                                        25
059200900718     C                   END
059300000000     C                   EXCEPT    TESTA
059400000000     C                   EXCEPT    TESTA1
059500900718     C                   SETOFF                                       0625
059600000000     C                   Z-ADD     1             A                 3 0
059700000000     C     MOCTOR        LOOKUP    TOR(A)                                 06
059800000000     C   06              EXCEPT    RIGCOV
059900990202     C                   MOVEL     MOCCLI        PRECLI            8
060000000000     C                   END
060100000000     C   OF              DO
060200000000     C                   EXCEPT    SEGUE
060300000000     C                   EXCEPT    FONDO
060400000000     C                   EXCEPT    TESTA
060500000000     C                   END
060600000000     C                   EXCEPT    DETAIL
060700000000     C*CALCOLO PREZZO
060800000000     C                   EXSR      CALPRZ
060900000000     C*CREAZIONE MOVIMENTI
061000000000     C                   EXSR      CREMOV
061100000000     C*AGGIORNAMENTO SALDI
061200000000     C                   EXSR      AGGSAL
061300000000     C*AGGIORNAMENTO RIGHE
061400000000     C                   EXSR      AGGORD
061500000000     C                   ENDSR
061600921104     C*-------------------------------------------------------------------
061700921104     C     CARFBL        BEGSR
061800021029     C                   CLEAR                   FIVAB000
061900921104     C*
061901101021     c                   movel     dateu         VABAAS
061902101021     c                   move      dateu         VABmgs
062000921104     C* CODICE BOLLA PARTENZA
062100960108     C                   MOVE      'A '          VABCBO
062200921104     C* RAGIONE SOCIALE DESTINATARIO
062300960108     C                   MOVEL     RAGLUO        VABRSD
062400921104     C* INDIRIZZO DESTINATARIO
062500960108     C                   MOVEL     INDLUO        VABIND
062600921104     C* CAP DESTINATARIO
062700960108     C                   MOVEL     CAPLUO        VABCAD
062800921104     C* LOCALITA' DESTINATARIO
062900960108     C                   MOVEL     LOCLUO        VABLOD
063000921104     C* PROVINCIA DESTINATARIO
063100960108     C                   MOVEL     PROLUO        VABPRD
063200921104     C* TIPO SPEDIZIONE
063300980519     C                   MOVE      'C'           VABTSP
063400921104     C* NATURA MERCE
063500960108     C                   MOVE      GAI(2)        VABNAS
063600921104     C* RIFERIMENTO MITTENTE NUMERICO
063700960108     C                   Z-ADD     NUMBOL        VABRMN
063800921104     C* FLAG FERMO DEPOSITO
063900960108     C                   MOVE      'S'           VABFFD
064000980519     C* FORZO LNA E ZONA 00 SIA PER DESTINATARIO SDI CHE PER DESTIN.BAR
064100980112     C                   MOVE      MOCCLI        W0030             3 0
064200990202     C                   MOVEL     MOCCLI        W0040             5 0
064300990202     C     W0040         IFEQ      00465
064400980112     C     W0030         CHAIN     AZORG01L                           30
064500980112     C     *IN30         IFEQ      *OFF
064600980112     C                   MOVE      ORGFIL        VABLNA
064700101014     c* Se piena la filiale invio materialesede, imposto quella linea di arrivo
064800101014     c                   movel     orgde8        og148
064900101014     c                   if        §ogfiue>*zeros
065000101014     c                   movel     §ogfiue       vablna
065100101014     c                   endif
065200101014     c
065300980112     C                   MOVE      00            VABZNC
065400980519     C     ORGDIT        IFEQ      'SDI'
065500980519     C                   MOVE      'D'           VABTSP
065600980112     C                   END
065700980519     C                   END
065800980112     C                   END
065900921104     C* SCRIVO IL RECORD
065901101021     c*   solo se linea di arrivo non è 046 - SEDE
065902101021     c                   if        vablna<>46
066000021029     C                   WRITE     FIVAB000
066001101021     c                   endif
066100921104     C*
066200921104     C                   ENDSR
066300990202     C*----------------------------------------------------*
066400990202     C* Reperimento dati società
066500990202     C*----------------------------------------------------*
066600990202     C     REPSOC        BEGSR
066700990202     C*
066800990202     C                   CALLB     'XSOC'
066900990202     C                   PARM                    TIPXSC            6
067000990202     C                   PARM      socpas        SOCXSC            3
067100990202     C                   PARM                    CDSXSC            9 0
067200990202     C                   PARM                    MODXSC            3
067300990202     C                   PARM      *blanks       RTNXSC            1
067400990202     C                   PARM                    XSOCDS
067500990202     C                   PARM                    KPJBA
067600990202     C*
067700990202     C                   ENDSR
067800000000     O*--------------------------------------------------------------*
067900000000     O*     TESTATA                                                  *
068000000000     O*--------------------------------------------------------------*
068100890609     OBOLLEINT  E            TESTA            01
068200890609     O                       SK1                132
068300890609     O          E            TESTA            02
068400890609     O                       SK1                132
068500890609     O                       DITRSC              37
068600890609     O          E            TESTA            03
068700890609     O                       SK1                132
068800890609     O                       DITIND              37
068900890609     O                                           51 'BOLLA N.'
069000890609     O                       NUMBOL        Z     58
069100890612     O                                           63 'del'
069200990202     O                       BOLDEM              74 '0  /  /    '
069300890609     O          E            TESTA            04
069400890609     O                       SK1                132
069500890609     O                       DITCAP               7
069600890609     O                       DITLOC              28
069700890609     O                       DITPRV              31
069800890609     O          E            TESTA            05
069900890609     O                       SK1                132
070000890609     O          E            TESTA            06
070100890609     O                       SK2                132
070200890609     O          E            TESTA            07
070300890609     O                                           24 'Destinatario e luogo di'
070400890609     O                                           38 'destinazione'
070500890609     O          E            TESTA            08
070600890609     O                       SK2                132
070700890609     O          E            TESTA            09
070800000000     O                       MOCCLI               8
070900961029     O               73                          28 'Spett.le P.O. di'
071000961029     O               74                          28 'Spett.le        '
071100890612     O                       RAGLUO              53
071200890612     O                       RAGLU1              77
071300921112     O*
071400921112     O          E            TESTA       1
071500921112     O                       INDLUO              59
071600921112     O          E            TESTA       1
071700921112     O                       CAPLUO              34
071800921112     O                       LOCLUO              55
071900921112     O                       PROLUO              59
072000921112     O*
072100890609     O          E            TESTA            14
072200890609     O                       SK2                132
072300890609     O          E            TESTA            15
072400890609     O                                            8 'Vettore'
072500890609     O          E            TESTA            16
072600890609     O                       DESSPE              21
072700890609     O                       INDSPE              42
072800890609     O                       LOCSPE              58
072900890609     O          E            TESTA            17
073000890609     O                       SK2                132
073100890609     O          E            TESTA            18
073200890609     O                                           10 'Materiale'
073300890609     O                                           41 'Descrizione'
073400890612     O                                           61 'Quantità'
073500890609     O          E            TESTA            19
073600890609     O                       SK2                132
073700000000     O*--------------------------------------------------------------*
073800000000     O*     TESTATA DI DETTAGLIO                                     *
073900000000     O*--------------------------------------------------------------*
074000000000     O          E            TESTA1      1
074100000000     O                                           30 'NS.ORDINE N.'
074200000000     O                       MOCNOR        Z     36
074300000000     O                                           40 'DEL'
074400990202     O                       BOLDOR              51 '0  /  /    '
074500990202     O               25                          67 'RIF.VS.ORDINE'
074600990202     O               25      MOCNRF        Z     73
074700990202     O               25      DATDRF              84 '  /  /    '
074800000000     O*--------------------------------------------------------------*
074900000000     O*     DETTAGLIO                                                *
075000000000     O*--------------------------------------------------------------*
075100000000     O          E            DETAIL      1
075200000000     O                       MOCMAT              14
075300000000     O                       MOCDMT              48
075400000000     O                       MOCQTA        2     63
075500900718     O                       MOCCOM              82
075600000000     O*----------------------------------------------------------------
075700000000     O          E            SEGUE
075800000000     O                                           83 '%'
075900000000     O*----------------------------------------------------------------
076000000000     O*--------------------------------------------------------------*
076100000000     O*     FONDO                                                    *
076200000000     O*--------------------------------------------------------------*
076300890609     O          E            FONDO            56
076400890609     O                       SK2                132
076500890609     O          E            FONDO            58
076600890720     O                       SK5                132
076700890609     O                                            6 'COLLI'
076800890609     O                                           27 'PESO'
076900890720     O          E            FONDO            59
077000890720     O                       SK6                132
077100890612     O          E            FONDO            60
077200890612     O                       SK3                132
077300890612     O          E            FONDO            62
077400890612     O                       SK4                132
077500000000     O*--------------------------------------------------------------*
077600000000     O*     STAMPA RIGA DI COMMENTO                                  *
077700000000     O*--------------------------------------------------------------*
077800000000     O          E            SCOM        1
077900000000     O                       COM(A)              48
078000000000     O          E            SCOM        0  0
078100000000     O                       COM(A)              48
078200000000     O*--------------------------------------------------------------*
078300000000     O*     STAMPA RIGA DI PROVA O IN CONTO VISIONE                  *
078400000000     O*--------------------------------------------------------------*
078500000000     O          E    06      RIGCOV      1
078600000000     O                       DTOR(A)             43
078700000000     O          E    06      RIGCOV      0  0
078800000000     O                       DTOR(A)             43
078900000000     O*--------------------------------------------------------------*
079000971210**   SK1
079100890609                                         !                        SK01
079200890609                                                                  SK01
079300890609**   SK2
079400890612------------------------------------------------------------------SK02
079500890609-----------------                                                 SK02
079600890612**   SK3
079700890612  NOTE -----------------------------------------------------------SK03
079800890612-----------------                                                 SK03
079900890612**   SK4
080000890612       -----------------------------------------------------------SK04
080100890612-----------------                                                 SK04
080200890720**   SK5
080300890720                                       Merci Esenti Emissione BollSK05
080400890720a Accompagnamento                                                 SK05
080500890720**   SK6
080600890720                                       Merci ai Sensi D.P.R. 6/10/SK06
08070089072078 N.627 Art.4/8                                                  SK06
080800921104** GAI TABELLA CONTROLLO SISTEMA INFORMATIVO
080900921104GAITRA201
081000921104VEDI BAM
081100990603GAITRAPSP
