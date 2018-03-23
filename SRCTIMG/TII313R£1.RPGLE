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
004600000000     D DSMAG         E DS                  EXTNAME(ANTAB153)
004700000000     D*------------- TABELLA MAGAZZINI :
004800000000     D TB152         E DS                  EXTNAME(ANTAB152)
004900000000     D*------------- TABELLA CAUSALI :
005000000000     D  AZE                   30     34
005100000000     D                                     DIM(5)                               *ENTRATE
005200000000     D  AZU                   39     43
005300000000     D                                     DIM(5)                               *USCITE
005400000000     D  AZG                   48     52
005500000000     D                                     DIM(5)                               *GESTIONALI
005600990202     D DSSAL         E DS                  EXTNAME(tmsmp00F)
005700000000     D*------------- FILE SALDI:
005800990211     D  EN                    86    115P 2
005900000000     D                                     DIM(5)
006000990211     D  US                   116    145P 2
006100000000     D                                     DIM(5)
006200000000     D OFNDT2          DS           512
006300000000     D*------------- NUMERAT.CODICE INDIVIDUALE RIGA
006400000000     D  NUMCRG                 1      7  0
006500000000     D  FILDT2                 8    256
006600000000     D  FILDT3               257    512
006700000000     D MPHDT1          DS
006800000000     D*--- DATA AREA N.MOVIMENTO MAGAZZINO :
006900000000     D  NUMMOV                 1      5  0
007000000000     D  FILL1                  6      8
007100000000     D  DATDTA                 9     14  0
007200000000     D  FILL2                 15     17
007300000000     D  ANNDTA                18     19  0
007400000000     D  FIL03                 20     50
007500000000     D  DGIO                  51     56  0
007600000000     D  FIL04                 57    100
007700000000     D  FILL5                101    300
007800000000     D  GIOTOL               301    302  0
007900000000     D  FILL6                303    512
008000000000     D OCLBL2          DS
008100000000     D*--- DATA AREA N.BOLLA /DATA BOLLA :
008200000000     D  NUMBOL                 1      6  0
008300000000     D  DATABL                 7     12  0
008400000000     D DSBL1           DS
008500000000     D*--- DS TABELLA BL1
008600000000     D  TABCAU                 1      3
008700000000     D DSTAB           DS
008800000000     D*DESCRIZIONE TIPO ORDINE/FLAG VISIONE
008900000000     D  DESTOR                 1     25
009000000000     D  TABTOR                35     35
009100000000     D DSPOR           DS
009200000000     D*DESCRIZIONE DEL TRASPORTO + FLAG
009300000000     D  DESRES                 1     26
009400000000     D  T1                    41     41
009500000000     D  T2                    42     42
009600000000     D  T3                    43     43
009700000000     D PARAM2          DS
009800990202     D  BOLCLI                 1      8
009900990202     D  BOLNOR                 9     13  0
010000990202     D  BOLDEM                14     21  0
010100990202     D  BOLDOR                22     29  0
010200990202     D  socpas                30     32
010300990202     D KPJBA         E DS
010400990202     D soc001        E DS                  EXTNAME(xsoc001ds)
010500990202     D xsocds          DS          1000
010600990202     d dataiso         s               d   datfmt(*iso)
010700990202     d dataeur         s               d   datfmt(*eur)
010800000000     C*----------------------------------------------------*
010900000000     C     *ENTRY        PLIST                                                  *
011000000000     C                   PARM                    KPJBA                          *
011100000000     C     *DTAARA       DEFINE                  OFNDT2                         *
011200000000     C     *DTAARA       DEFINE                  MPHDT1                         *
011300000000     C     *DTAARA       DEFINE                  OCLBL2                         *
011400000000     C******************************************************
011500000000     C                   MOVEL     *BLANKS       BOLCLI                         *
011600000000     C                   Z-ADD     0             BOLNOR                         *
011700000000     C                   Z-ADD     0             BOLDEM                         *
011800000000     C                   Z-ADD     0             BOLDOR                         *
011900000000     C                   MOVEL     KPJBU         PARAM2                         *
012000990202     C*---------- RICERCA DITTA :
012100990202     C                   MOVEL     'SOC001'      TIPXSC
012200990202     C                   MOVEL     *blank        SOCXSC
012300990202     C                   EXSR      REPSOC
012400990202     C     RTNXSC        IFNE      '1'
012500990202     C                   MOVEL     XSOCDS        SOC001
012600990202     C                   MOVEL     xscrgs        desdit           20
012700990202     C                   MOVEL     xscrgs        DITRSC           35
012800990202     C                   MOVEL     xscvia        DITIND           35
012900990202     C                   movel     xsccap        DITCAP            5 0
013000990202     C                   MOVEL     xsccit        DITLOC           20
013100990202     C                   MOVEL     xscprv        DITPRV            2
013200990202     c                   end
013300990202     c                   time                    como14           14 0
013400990202     c                   move      como14        woggi             8 0
013500921104     C*----------------------------------------------------*
013600921104     C* CONTROLLO SE IL SISTEMA INFORMATIVO  E' RIFERITO A *
013700921104     C* GAITRA201 (BARTOLINI)                              *
013800921104     C     KNSIF         IFEQ      GAI(1)                                       *
013900971210     C     KNSIF         OREQ      GAI(3)                                       *
014000021029     C                   OPEN      FIVABWKF                                     *
014100921104     C                   Z-ADD     1             FLGAI             1 0
014200921104     C                   END                                                    *
014300000000     C******************************************************
014400990202     C*--- FILE tmmoc01L 'CODICE CLIENTE  N.ORDINE'        *
014500000000     C     KEYCLI        KLIST                                                  *
014600000000     C                   KFLD                    BOLCLI                         *
014700000000     C                   KFLD                    BOLNOR                         *
014800990202     C*--- FILE SALDI MAGAZZINO M.P. "tmsmp01L"            *
014900000000     C     KEYSAL        KLIST                                                  *
015000990202     C                   KFLD                    ANNDTA4           4 0          *
015100000000     C                   KFLD                    MOCMAG                         *
015200000000     C                   KFLD                    MOCMAT                         *
015300000000     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F" -------------*
015400000000     C     KEYANT        KLIST                                                  *
015500000000     C                   KFLD                    TABGRU                         *
015600000000     C                   KFLD                    TABCOD                         *
015700990202     C*--- FILE ANAG. MAT. PRIME "tmamp01L"                *
015800000000     C     KEYMAT        KLIST                                                  *
015900000000     C                   KFLD                    MOCMAT                         *
016000000000     C*--- CHIAVE PER RICERCA TIPO ORDINI 'ANTAB00F'-------*
016100000000     C     KEYTAB        KLIST                                                  *
016200000000     C                   KFLD                    GRU1              3            *
016300000000     C*--- CHIAVE PER RICERCA SPEDIZIONIERE/LUOGO          *
016400000000     C     KEYSPE        KLIST                                                  *
016500970801     C                   KFLD                    SPEFLS                         *
016600000000     C                   KFLD                    SPECLI                         *
016700000000     C                   KFLD                    CODSPE            3            *
016800000000     C*--- RICERCA RAG. SOC.CLIENTE 'CNACO00F'             *
016900990202     C     KEYcln        KLIST                                                  *
017000990202     C                   KFLD                    socpas                         *
017100990202     C                   KFLD                    bolcli                         *
017200000000     C*----------------------------------------------------*
017300000000     C*RIBALTAMENTO IN AAMMGG LA DATA DELL'ORDINE
017400000000     C*E DELLA DATA DI EMISSIONE
017500990202     c                   if        boldor > 0
017600990202     c                   move      boldor        dataeur
017700990202     c                   move      dataeur       dataiso
017800990202     c                   move      dataiso       datdor            8 0
017900990202     C                   end
018000990202     c                   if        boldem > 0
018100990202     c                   move      boldem        dataeur
018200990202     c                   move      dataeur       dataiso
018300990202     c                   move      dataiso       datdem            8 0
018400990202     C                   end
018500000000     C*RICERCA CAUSALE OPERATIVA
018600000000     C                   EXSR      RICCAU
018700000000     C*CARICAMENTO TIPOLOGIE ORDINI
018800000000     C                   EXSR      CARVIS
018900000000     C*POSIZIONAMENTO PER CLIENTE N.ORDINE
019000990202     C     KEYCLI        SETLL     tmmoc000                               04
019100000000     C  N04              GOTO      FINE
019200000000     C*LETTURA
019300000000     C     SU            TAG
019400990202     C     KEYCLI        READE     tmmoc000                               05
019500000000     C   05
019600000000     CAN 22              EXSR      STACOM
019700000000     C   05
019800000000     CAN 22              EXCEPT    FONDO
019900000000     C   05              GOTO      FINE
020000000000     C*RIBALTAMENTO IN GGMMAA DATA RIF.VS.ORD.
020100990202     c                   if        mocdrf > 0
020200990202     c                   move      mocdrf        dataiso
020300990202     c                   move      dataiso       dataeur
020400990202     c                   move      dataeur       datdrf            8 0
020500990202     c                   end
020600000000     C*
020700000000     C     MOCDOR        IFNE      DATDOR
020800000000     C                   GOTO      SU
020900000000     C                   END
021000000000     C     MOCTRG        CABEQ     '1'           LA
021100000000     C     MOCTRG        CABEQ     '6'           STAR
021200000000     C                   GOTO      SU
021300000000     C*----------------
021400000000     C     LA            TAG
021500000000     C     MOCTRG        IFEQ      '1'
021600000000     C     MOCFAS        CABNE     'AS'          SU
021700000000     C                   EXSR      LEGGI
021800000000     C                   END
021900000000     C     STAR          TAG
022000000000     C     MOCTRG        IFEQ      '6'
022100000000     C                   EXSR      MEMCOM
022200000000     C                   END
022300000000     C                   GOTO      SU
022400000000     C     FINE          TAG
022500000000     C                   SETON                                        LR
022600000000     C******************************************************
022700000000     C     RICCAU        BEGSR
022800000000     C**------------------------------
022900000000     C** RICERCA  CAUSALE OPERATIVA
023000000000     C**------------------------------
023100000000     C                   MOVE      '257'         TABGRU
023200000000     C                   MOVE      *ZEROS        TABCOD
023300000000     C                   MOVE      '1'           TABCOD
023400000000     C     KEYANT        CHAIN     ANTAB00F                           H1
023500000000     C   H1              GOTO      FINE
023600000000     C                   MOVEL     TABDES        DSBL1
023700000000     C*---------------
023800000000     C                   MOVE      '152'         TABGRU
023900000000     C                   MOVE      *ZEROS        TABCOD
024000000000     C                   MOVEL     'MP'          CINQUE            5
024100000000     C                   MOVE      TABCAU        CINQUE            5
024200000000     C                   MOVE      CINQUE        TABCOD
024300000000     C     KEYANT        CHAIN     ANTAB00F                           H1
024400000000     C   H1              GOTO      FINE
024500000000     C                   MOVEL     TABDES        TB152
024600000000     C                   MOVEA     AZE           AZ(1)
024700000000     C                   MOVEA     AZU           AZ(6)
024800000000     C                   MOVEA     AZG           AZ(11)
024900000000     C                   ENDSR
025000000000     C******************************************************
025100000000     C     CARVIS        BEGSR
025200000000     C**----------------------------------------
025300000000     C** CARICAMENTO TIPOLOGIE ORDINI CON FLAG
025400000000     C** SCARICO MAGAZZINO = N
025500000000     C**----------------------------------------
025600000000     C                   MOVEL     '162'         GRU1
025700000000     C     KEYTAB        SETLL     ANTAB000                               07
025800000000     C  N07              GOTO      ENDVIS
025900000000     C                   DO        *HIVAL
026000000000     C     KEYTAB        READE     ANTAB000                               06
026100000000     C   06              GOTO      ENDVIS
026200000000     C                   MOVEL     TABDES        DSTAB
026300000000     C     TABTOR        IFEQ      'N'
026400000000     C                   ADD       1             K                 3 0
026500000000     C                   MOVE      TABCOD        TOR(K)
026600000000     C                   MOVEL     DESTOR        DTOR(K)
026700000000     C                   END
026800000000     C                   END
026900000000     C     ENDVIS        ENDSR
027000000000     C*****************************************************
027100000000     C     PRENUM        BEGSR
027200000000     C**---------------------------------------------
027300000000     C** PRELEVO DALLE DATE AREE OCLBL2/ MPHDT1 I
027400000000     C** VALORI INIZIALI INCREMENTO I NUMERI
027500000000     C** ED ESEGUO L'AGGIORNAMENTO
027600000000     C**---------------------------------------------
027700000000     C                   Z-ADD     0             BOLNUO
027800000000     C                   Z-ADD     0             MOVNUO
027900000000     C*          -------------
028000000000     C     STARX         TAG
028100000000     C*          -------------
028200000000     C*DATA AREA NUMERO BOLLE
028300000000     C     *LOCK         IN        OCLBL2                               46
028400000000     C   46              GOTO      STARX
028500000000     C                   Z-ADD     NUMBOL        BOLNUO            6 0
028600000000     C                   ADD       1             BOLNUO
028700000000     C     BOLNUO        IFEQ      0
028800000000     C                   ADD       1             BOLNUO
028900000000     C                   END
029000000000     C                   Z-ADD     BOLNUO        NUMBOL
029100000000     C                   Z-ADD     DATDEM        DATABL
029200000000     C                   OUT       OCLBL2
029300000000     C*          -------------
029400000000     C     START         TAG
029500000000     C*          -------------
029600000000     C*DATA AREA NUMERO MOVIMENTI
029700000000     C     *LOCK         IN        MPHDT1                               46
029800000000     C   46              GOTO      START
029900990202     c                   z-add     anndta        anndta4
030000990202     c                   if        anndta4 > 40
030100990202     c                   add       1900          anndta4
030200990202     c                   else
030300990202     c                   add       2000          anndta4
030400990202     c                   end
030500000000     C                   Z-ADD     NUMMOV        MOVNUO            5 0
030600000000     C                   ADD       1             MOVNUO
030700000000     C     MOVNUO        IFEQ      0
030800000000     C                   ADD       1             MOVNUO
030900000000     C                   END
031000000000     C                   Z-ADD     MOVNUO        NUMMOV
031100000000     C                   Z-ADD     BOLDEM        DATDTA
031200000000     C                   OUT       MPHDT1
031300000000     C                   ENDSR
031400000000     C******************************************************
031500000000     C     RICANA        BEGSR
031600000000     C**----------------------------------------
031700000000     C** RICERCA DATI ANAGRAFICI DEL CLIENTE
031800000000     C**----------------------------------------
031900961029     C                   SETOFF                                         7374
032000990202     C     KEYcln        CHAIN     ancln01l                           20
032100990202     C     clnsogg       CHAIN     ansog01l                           20
032200990202     C     clnsogg       CHAIN     anind01l                           21
032300000000     C                   MOVEL     *BLANKS       RAG
032400000000     C                   MOVEL     *BLANKS       RAG1
032500000000     C                   MOVEL     *BLANKS       VIA
032600990202     C                   move      *zeros        CAP1
032700000000     C                   MOVEL     *BLANKS       CIT
032800000000     C                   MOVEL     *BLANKS       PRV
032900000000     C                   MOVEL     *BLANKS       RAGLUO
033000000000     C                   MOVEL     *BLANKS       RAGLU1
033100000000     C                   MOVEL     *BLANKS       INDLUO
033200000000     C                   MOVEL     *BLANKS       LOCLUO
033300000000     C                   MOVEL     *BLANKS       PROLUO
033400000000     C                   MOVEL     *ZEROS        CAPLUO
033500990202     C  N20              MOVEL     sogdes        RAG              22
033600990202     C  N20              MOVE      sogdes        RAG1             22
033700990202     C  N21              MOVEL     INDindriz     VIA              30
033800990202     C  N21              movel     INDCAP        CAP1              5
033900990202     C  N21              MOVEL     INDlocalit    CIT              25
034000990202     C  N21              MOVEL     INDPRov       PRV               2
034100990202     C* LUOGO DI SPEDIZIONE UGUALE A CLIENTE
034200990202     C  N20              MOVEL     sogdes        RAGLUO           22
034300990202     C  N20              MOVE      sogdes        RAGLU1           22
034400990202     C  N21              MOVEL     INDindriz     INDLUO           30
034500990202     C  N21              MOVEL     INDCAP        CAPLUO            5
034600990202     C  N21              MOVEL     INDlocalit    LOCLUO           20
034700990202     C  N21              MOVEL     INDPRov       PROLUO            2
034800000000     C     MOCLUO        IFNE      *BLANKS
034900970801     C                   MOVEL     'L'           SPEFLS
035000000000     C                   MOVEL     MOCCLI        SPECLI
035100000000     C                   MOVEL     MOCLUO        CODSPE
035200000000     C     KEYSPE        CHAIN     ANSPE3                             23
035300000000     C  N23              DO
035400000000     C                   MOVEL     *BLANKS       RAGLUO
035500000000     C                   MOVEL     *BLANKS       RAGLU1
035600970801     C                   CLEAR                   CAPLUO
035700000000     C                   MOVEL     *BLANKS       LOCLUO
035800000000     C                   MOVEL     *BLANKS       PROLUO
035900000000     C*
036000000000     C                   MOVEL     SPERAG        RAGLUO
036100000000     C                   MOVE      SPERAG        RAGLU1
036200000000     C                   MOVEL     SPEIND        INDLUO
036300970801     C                   MOVEL     SPECAP        CAPLUO
036400000000     C                   MOVEL     SPELOC        LOCLUO
036500000000     C                   MOVEL     SPEPRO        PROLUO
036600000000     C                   END
036700000000     C                   END
036800000000     C** IMPOSTA LUOGO SE ESISTE SULL'ORDINE
036900000000     C*RICERCA DEL CODICE IMBALLO SULLA TABELLA '008'
037000000000     C                   MOVE      '008'         TABGRU
037100000000     C                   MOVE      *ZEROS        TABCOD
037200000000     C                   MOVE      MOCIMB        TABCOD
037300000000     C     KEYANT        CHAIN     ANTAB00F                           47
037400000000     C   47              MOVEL     *BLANKS       DESIMB
037500000000     C  N47              MOVEL     TABDES        DESIMB           26
037600000000     C*RICERCA DEL CODICE RESA SULLA TABELLA '019'
037700000000     C                   MOVE      '019'         TABGRU
037800000000     C                   MOVE      *ZEROS        TABCOD
037900000000     C                   MOVE      MOCRES        TABCOD
038000000000     C     KEYANT        CHAIN     ANTAB00F                           47
038100000000     C   47              MOVEL     *BLANKS       DSPOR
038200000000     C  N47              MOVEL     TABDES        DSPOR
038300000000     C*RICERCA CAUSALE DEL TRASPORTO
038400000000     C                   MOVE      '707'         TABGRU
038500000000     C                   MOVEL     *BLANKS       TABCOD
038600000000     C                   MOVE      MOCTRO        TABCOD
038700000000     C     KEYANT        CHAIN     ANTAB00F                           47
038800000000     C   47              MOVEL     *BLANKS       DESTRO
038900000000     C  N47              MOVEL     TABDES        DESTRO           30
039000000000     C*RICERCA DEL SPEDIZIONIERE
039100970801     C                   MOVEL     'S'           SPEFLS
039200000000     C                   MOVEL     9999999       SPECLI
039300000000     C                   MOVEL     MOCSPE        CODSPE
039400000000     C     KEYSPE        CHAIN     ANSPE1                             24
039500000000     C   24              DO
039600000000     C                   MOVEL     *BLANKS       DESSPE
039700000000     C                   MOVEL     *BLANKS       INDSPE
039800000000     C                   MOVEL     *BLANKS       LOCSPE
039900000000     C                   END
040000000000     C  N24              DO
040100000000     C                   MOVEL     SPERAG        DESSPE           20
040200000000     C                   MOVEL     SPEIND        INDSPE           20
040300000000     C                   MOVEL     SPELOC        LOCSPE           15
040400990202     C     MOCCLI        CHAIN     tmclp000                           70
040500890612     C  N70              DO
040600890718     C*AZIENDA
040700890612     C     CLPSTM        IFEQ      '52'
040800980112     C     CLPSTM        OREQ      '57'
040900961029     C                   SETON                                        73
041000890612     C                   END
041100890718     C*FILIALE
041200890718     C     CLPSTM        IFEQ      '53'
041300980112     C     CLPSTM        OREQ      '56'
041400890718     C                   SETON                                        73
041500890718     C                   END
041600890718     C*AZIENDA
041700890718     C     CLPSTM        IFEQ      '54'
041800890718     C                   SETON                                        74
041900890718     C                   END
042000890612     C                   END
042100000000     C                   END
042200000000     C                   ENDSR
042300000000     C******************************************************
042400000000     C     MEMCOM        BEGSR
042500000000     C**----------------------------------------
042600000000     C** MEMORIZZAZIONE COMMENTI
042700000000     C**----------------------------------------
042800000000     C                   Z-ADD     0             A
042900000000     C                   DO        10            A                 3 0
043000000000     C     COM(A)        IFEQ      *BLANKS
043100000000     C                   MOVEL     MOCDMT        COM(A)
043200000000     C                   GOTO      ENDCOM
043300000000     C                   END
043400000000     C                   END
043500000000     C     ENDCOM        ENDSR
043600000000     C******************************************************
043700000000     C     STACOM        BEGSR
043800000000     C**----------------------------------------
043900000000     C** STAMPA COMMENTI
044000000000     C**----------------------------------------
044100000000     C                   DO        10            A
044200000000     C     COM(A)        IFNE      *BLANKS
044300000000     C                   EXCEPT    SCOM
044400000000     C                   END
044500000000     C                   END
044600000000     C                   MOVEA     *BLANKS       COM
044700000000     C                   ENDSR
044800000000     C******************************************************
044900000000     C     CALPRZ        BEGSR
045000000000     C**----------------------------
045100000000     C** CALCOLO DEL PREZZO SCONTATO
045200000000     C**----------------------------
045300000000     C                   Z-ADD     MOCPRZ        PRENET           11 2
045400000000     C     PRENET        MULT      MOCSC1        SCONTO           11 2
045500000000     C     SCONTO        DIV       100           SCONTO
045600000000     C     PRENET        SUB       SCONTO        PRENET
045700000000     C     PRENET        MULT      MOCSC2        SCONTO
045800000000     C     SCONTO        DIV       100           SCONTO
045900000000     C     PRENET        SUB       SCONTO        PRENET
046000000000     C     PRENET        MULT      MOCSC3        SCONTO
046100000000     C     SCONTO        DIV       100           SCONTO
046200000000     C     PRENET        SUB       SCONTO        PRENET
046300000000     C                   ENDSR
046400000000     C******************************************************
046500000000     C     CREMOV        BEGSR
046600000000     C**---------------------------------------
046700000000     C** CREAZIONE DEI MOVIVENTI DI MAGAZZINO
046800000000     C**---------------------------------------
046900000000     C     STARTV        TAG
047000000000     C*INCREMENTO E AGGIORNO DATA AREA OFNDT2
047100000000     C*NUMERATORE CODICE INDIVIDUALE DI RIGA
047200000000     C     *LOCK         IN        OFNDT2                               46
047300000000     C   46              GOTO      STARTV
047400000000     C                   Z-ADD     NUMCRG        NUMMAG            7 0
047500000000     C                   ADD       1             NUMMAG
047600000000     C     NUMMAG        IFEQ      0
047700000000     C                   ADD       1             NUMMAG
047800000000     C                   END
047900000000     C                   Z-ADD     NUMMAG        NUMCRG
048000000000     C                   OUT       OFNDT2
048100000000     C*----------
048200000000     C                   MOVEL     *BLANKS       MMPATT
048300000000     C                   Z-ADD     MOCUTE        MMPUTE
048400000000     C                   Z-ADD     DATDEM        MMPDMN
048500000000     C                   MOVEL     MOCMAG        MMPMAG
048600990202     C                   MOVE      ANNDTA4       MMPAAC
048700000000     C                   Z-ADD     DATDEM        MMPDMV
048800000000     C                   Z-ADD     MOVNUO        MMPNMV
048900000000     C     SETUP         TAG
049000990202     C     KEYMAT        CHAIN     tmamp01L                           0725
049100000000     C   25              GOTO      SETUP
049200000000     C  N07              DO
049300000000     C     AZ(7)         IFEQ      '+'
049400990202     C     DATDEM        IFGT      amptar
049500990202     C                   Z-ADD     DATDEM        ampTAR
049600990202     C                   UPDATE    tmamp000
049700000000     C                   END
049800000000     C                   END
049900000000     C                   END
050000000000     C** ULTIMA USCITA PER VENDITA
050100000000     C                   Z-ADD     DATDEM        MMPDDC
050200000000     C                   Z-ADD     BOLNUO        MMPNDC
050300000000     C                   MOVEL     TABCAU        MMPCAU
050400000000     C                   MOVEA     AZ            MMPOPC
050500000000     C                   MOVEL     MOCCDC        MMPCDC
050600000000     C                   MOVEL     MOCMAT        MMPMAT
050700000000     C                   MOVEL     MOCUMQ        MMPUMA
050800000000     C                   Z-ADD     MOCQTA        MMPQTA
050900000000     C                   Z-ADD     PRENET        MMPPRZ
051000000000     C                   MOVEL     MOCMAG        MMPMCP
051100000000     C                   MOVE      '153'         TABGRU
051200000000     C                   MOVEL     MOCMAG        TABCOD
051300000000     C     KEYANT        CHAIN     ANTAB00F                           58
051400000000     C  N58              DO
051500000000     C                   MOVEL     TABDES        DSMAG
051600000000     C                   MOVEL     TIG153        MMPFGQ
051700000000     C                   MOVE      TIG153        MMPFGV
051800000000     C                   MOVE      TIM153        MMPFGT
051900000000     C                   END
052000000000     C                   Z-ADD     NUMMAG        MMPCRG
052100000000     C                   MOVEL     *BLANKS       MMPFLG
052200990202     C                   WRITE     tmmmp000                             48
052300000000     C                   ENDSR
052400000000     C*-----------------------------------------------------
052500000000     C     AGGSAL        BEGSR
052600000000     C**---------------------------------------
052700000000     C** CALCOLO LA DISPONIBILITA' A MAGAZZINO
052800000000     C**---------------------------------------
052900000000     C     SULA          TAG
053000990202     C     KEYSAL        CHAIN     tmsmp000                           5226
053100000000     C   26              GOTO      SULA
053200000000     C     SMPQIM        SUB       MOCQTA        SMPQIM
053300000000     C*--------- AGGIORNAMENTO ENTRATA --------------------*
053400000000     C     1             DO        5             X                 2 0          *
053500000000     C     AZE(X)        COMP      '+'                                    92    *
053600000000     C     AZE(X)        COMP      '-'                                    91    *
053700000000     C   92EN(X)         ADD       MOCQTA        EN(X)                          *
053800000000     C   91EN(X)         SUB       MOCQTA        EN(X)                          *
053900000000     C                   END                                                    *
054000000000     C*--------- AGGIORNAMENTO USCITA ---------------------*
054100000000     C     1             DO        5             X                              *
054200000000     C     AZU(X)        COMP      '+'                                    92    *
054300000000     C     AZU(X)        COMP      '-'                                    91    *
054400000000     C   92US(X)         ADD       MOCQTA        US(X)                          *
054500000000     C   91US(X)         SUB       MOCQTA        US(X)                          *
054600000000     C                   END                                                    *
054700000000     C     AZG(5)        COMP      '+'                                    92    *
054800000000     C     AZG(5)        COMP      '-'                                    91    *
054900000000     C   92SMPQGF        ADD       MOCQTA        SMPQGF
055000000000     C   91SMPQGF        SUB       MOCQTA        SMPQGF
055100990202     C                   UPDATE    tmsmp000
055200000000     C                   ENDSR
055300000000     C******************************************************
055400000000     C     AGGORD        BEGSR
055500000000     C**-------------------------------------------
055600990202     C** AGGIORNAMENTO RIGHE DI tmmoc00F DA AS A FA
055700000000     C** AGGIORNAMENTO DEL N.BOLLA , DATA BOLLA
055800000000     C** E DEL LEGAME ORDINI/MAGAZZ.
055900000000     C**-------------------------------------------
056000000000     C                   MOVEL     'FA'          MOCFAS
056100000000     C                   Z-ADD     NUMBOL        MOCNBL
056200000000     C                   Z-ADD     DATDEM        MOCDBL
056300000000     C                   Z-ADD     NUMMAG        MOCCRG
056400000000     C                   Z-ADD     NUMBOL        MOCNFT
056500000000     C                   MOVEL     9             MOCNFT
056600000000     C** NUMERO FATTURA = NUMERO BOLLA CON 9 DAVANTI
056700000000     C                   Z-ADD     DATDEM        MOCDFT
056800000000     C** DATA FATTURA = DATA BOLLA
056900990202     C                   UPDATE    tmmoc000
057000000000     C                   ENDSR
057100000000     C******************************************************
057200000000     C     LEGGI         BEGSR
057300000000     C**----------------------------------------
057400000000     C** LETTURA RIGHE
057500000000     C**----------------------------------------
057600000000     C     MOCCLI        IFNE      PRECLI
057700000000     C                   EXSR      RICANA
057800000000     C*PRELEVO E AGGIORNO I VALORI DALLE DATE AREE
057900000000     C                   EXSR      PRENUM
058000921104     C* SE FLGAI UGUALE A 1 (QUINDI SETRAS) E CAP DIVERSO DA 40138
058100921104     C* SCRIVO IL RECORD DELL'ARCHIVIO SPEDIZIONE
058200921104     C*
058300921104     C     FLGAI         IFEQ      1
058400020111     C*    CAPLUO        ANDNE     '40138'
058500921104     C                   EXSR      CARFBL
058600921104     C                   END
058700890609     C                   OPEN      BOLLEINT
058800000000     C                   SETON                                        22
058900900718     C     MOCNRF        IFGT      0
059000900718     C                   SETON                                        25
059100900718     C                   END
059200000000     C                   EXCEPT    TESTA
059300000000     C                   EXCEPT    TESTA1
059400900718     C                   SETOFF                                       0625
059500000000     C                   Z-ADD     1             A                 3 0
059600000000     C     MOCTOR        LOOKUP    TOR(A)                                 06
059700000000     C   06              EXCEPT    RIGCOV
059800990202     C                   MOVEL     MOCCLI        PRECLI            8
059900000000     C                   END
060000000000     C   OF              DO
060100000000     C                   EXCEPT    SEGUE
060200000000     C                   EXCEPT    FONDO
060300000000     C                   EXCEPT    TESTA
060400000000     C                   END
060500000000     C                   EXCEPT    DETAIL
060600000000     C*CALCOLO PREZZO
060700000000     C                   EXSR      CALPRZ
060800000000     C*CREAZIONE MOVIMENTI
060900000000     C                   EXSR      CREMOV
061000000000     C*AGGIORNAMENTO SALDI
061100000000     C                   EXSR      AGGSAL
061200000000     C*AGGIORNAMENTO RIGHE
061300000000     C                   EXSR      AGGORD
061400000000     C                   ENDSR
061500921104     C*-------------------------------------------------------------------
061600921104     C     CARFBL        BEGSR
061700021029     C                   CLEAR                   FIVAB000
061800921104     C*
061900921104     C* CODICE BOLLA PARTENZA
062000960108     C                   MOVE      'A '          VABCBO
062100921104     C* RAGIONE SOCIALE DESTINATARIO
062200960108     C                   MOVEL     RAGLUO        VABRSD
062300921104     C* INDIRIZZO DESTINATARIO
062400960108     C                   MOVEL     INDLUO        VABIND
062500921104     C* CAP DESTINATARIO
062600960108     C                   MOVEL     CAPLUO        VABCAD
062700921104     C* LOCALITA' DESTINATARIO
062800960108     C                   MOVEL     LOCLUO        VABLOD
062900921104     C* PROVINCIA DESTINATARIO
063000960108     C                   MOVEL     PROLUO        VABPRD
063100921104     C* TIPO SPEDIZIONE
063200980519     C                   MOVE      'C'           VABTSP
063300921104     C* NATURA MERCE
063400960108     C                   MOVE      GAI(2)        VABNAS
063500921104     C* RIFERIMENTO MITTENTE NUMERICO
063600960108     C                   Z-ADD     NUMBOL        VABRMN
063700921104     C* FLAG FERMO DEPOSITO
063800960108     C                   MOVE      'S'           VABFFD
063900980519     C* FORZO LNA E ZONA 00 SIA PER DESTINATARIO SDI CHE PER DESTIN.BAR
064000980112     C                   MOVE      MOCCLI        W0030             3 0
064100990202     C                   MOVEL     MOCCLI        W0040             5 0
064200990202     C     W0040         IFEQ      00465
064300980112     C     W0030         CHAIN     AZORG01L                           30
064400980112     C     *IN30         IFEQ      *OFF
064500980112     C                   MOVE      ORGFIL        VABLNA
064600980112     C                   MOVE      00            VABZNC
064700980519     C     ORGDIT        IFEQ      'SDI'
064800980519     C                   MOVE      'D'           VABTSP
064900980112     C                   END
065000980519     C                   END
065100980112     C                   END
065200921104     C* SCRIVO IL RECORD
065300021029     C                   WRITE     FIVAB000
065400921104     C*
065500921104     C                   ENDSR
065600990202     C*----------------------------------------------------*
065700990202     C* Reperimento dati società
065800990202     C*----------------------------------------------------*
065900990202     C     REPSOC        BEGSR
066000990202     C*
066100990202     C                   CALLB     'XSOC'
066200990202     C                   PARM                    TIPXSC            6
066300990202     C                   PARM      socpas        SOCXSC            3
066400990202     C                   PARM                    CDSXSC            9 0
066500990202     C                   PARM                    MODXSC            3
066600990202     C                   PARM      *blanks       RTNXSC            1
066700990202     C                   PARM                    XSOCDS
066800990202     C                   PARM                    KPJBA
066900990202     C*
067000990202     C                   ENDSR
067100000000     O*--------------------------------------------------------------*
067200000000     O*     TESTATA                                                  *
067300000000     O*--------------------------------------------------------------*
067400890609     OBOLLEINT  E            TESTA            01
067500890609     O                       SK1                132
067600890609     O          E            TESTA            02
067700890609     O                       SK1                132
067800890609     O                       DITRSC              37
067900890609     O          E            TESTA            03
068000890609     O                       SK1                132
068100890609     O                       DITIND              37
068200890609     O                                           51 'BOLLA N.'
068300890609     O                       NUMBOL        Z     58
068400890612     O                                           63 'del'
068500990202     O                       BOLDEM              74 '0  /  /    '
068600890609     O          E            TESTA            04
068700890609     O                       SK1                132
068800890609     O                       DITCAP               7
068900890609     O                       DITLOC              28
069000890609     O                       DITPRV              31
069100890609     O          E            TESTA            05
069200890609     O                       SK1                132
069300890609     O          E            TESTA            06
069400890609     O                       SK2                132
069500890609     O          E            TESTA            07
069600890609     O                                           24 'Destinatario e luogo di'
069700890609     O                                           38 'destinazione'
069800890609     O          E            TESTA            08
069900890609     O                       SK2                132
070000890609     O          E            TESTA            09
070100000000     O                       MOCCLI               8
070200961029     O               73                          28 'Spett.le P.O. di'
070300961029     O               74                          28 'Spett.le        '
070400890612     O                       RAGLUO              53
070500890612     O                       RAGLU1              77
070600921112     O*
070700921112     O          E            TESTA       1
070800921112     O                       INDLUO              59
070900921112     O          E            TESTA       1
071000921112     O                       CAPLUO              34
071100921112     O                       LOCLUO              55
071200921112     O                       PROLUO              59
071300921112     O*
071400890609     O          E            TESTA            14
071500890609     O                       SK2                132
071600890609     O          E            TESTA            15
071700890609     O                                            8 'Vettore'
071800890609     O          E            TESTA            16
071900890609     O                       DESSPE              21
072000890609     O                       INDSPE              42
072100890609     O                       LOCSPE              58
072200890609     O          E            TESTA            17
072300890609     O                       SK2                132
072400890609     O          E            TESTA            18
072500890609     O                                           10 'Materiale'
072600890609     O                                           41 'Descrizione'
072700890612     O                                           61 'Quantità'
072800890609     O          E            TESTA            19
072900890609     O                       SK2                132
073000000000     O*--------------------------------------------------------------*
073100000000     O*     TESTATA DI DETTAGLIO                                     *
073200000000     O*--------------------------------------------------------------*
073300000000     O          E            TESTA1      1
073400000000     O                                           30 'NS.ORDINE N.'
073500000000     O                       MOCNOR        Z     36
073600000000     O                                           40 'DEL'
073700990202     O                       BOLDOR              51 '0  /  /    '
073800990202     O               25                          67 'RIF.VS.ORDINE'
073900990202     O               25      MOCNRF        Z     73
074000990202     O               25      DATDRF              84 '  /  /    '
074100000000     O*--------------------------------------------------------------*
074200000000     O*     DETTAGLIO                                                *
074300000000     O*--------------------------------------------------------------*
074400000000     O          E            DETAIL      1
074500000000     O                       MOCMAT              14
074600000000     O                       MOCDMT              48
074700000000     O                       MOCQTA        2     63
074800900718     O                       MOCCOM              82
074900000000     O*----------------------------------------------------------------
075000000000     O          E            SEGUE
075100000000     O                                           83 '%'
075200000000     O*----------------------------------------------------------------
075300000000     O*--------------------------------------------------------------*
075400000000     O*     FONDO                                                    *
075500000000     O*--------------------------------------------------------------*
075600890609     O          E            FONDO            56
075700890609     O                       SK2                132
075800890609     O          E            FONDO            58
075900890720     O                       SK5                132
076000890609     O                                            6 'COLLI'
076100890609     O                                           27 'PESO'
076200890720     O          E            FONDO            59
076300890720     O                       SK6                132
076400890612     O          E            FONDO            60
076500890612     O                       SK3                132
076600890612     O          E            FONDO            62
076700890612     O                       SK4                132
076800000000     O*--------------------------------------------------------------*
076900000000     O*     STAMPA RIGA DI COMMENTO                                  *
077000000000     O*--------------------------------------------------------------*
077100000000     O          E            SCOM        1
077200000000     O                       COM(A)              48
077300000000     O          E            SCOM        0  0
077400000000     O                       COM(A)              48
077500000000     O*--------------------------------------------------------------*
077600000000     O*     STAMPA RIGA DI PROVA O IN CONTO VISIONE                  *
077700000000     O*--------------------------------------------------------------*
077800000000     O          E    06      RIGCOV      1
077900000000     O                       DTOR(A)             43
078000000000     O          E    06      RIGCOV      0  0
078100000000     O                       DTOR(A)             43
078200000000     O*--------------------------------------------------------------*
078300971210**   SK1
078400890609                                         !                        SK01
078500890609                                                                  SK01
078600890609**   SK2
078700890612------------------------------------------------------------------SK02
078800890609-----------------                                                 SK02
078900890612**   SK3
079000890612  NOTE -----------------------------------------------------------SK03
079100890612-----------------                                                 SK03
079200890612**   SK4
079300890612       -----------------------------------------------------------SK04
079400890612-----------------                                                 SK04
079500890720**   SK5
079600890720                                       Merci Esenti Emissione BollSK05
079700890720a Accompagnamento                                                 SK05
079800890720**   SK6
079900890720                                       Merci ai Sensi D.P.R. 6/10/SK06
08000089072078 N.627 Art.4/8                                                  SK06
080100921104** GAI TABELLA CONTROLLO SISTEMA INFORMATIVO
080200921104GAITRA201
080300921104VEDI BAM
080400990603GAITRAPSP
