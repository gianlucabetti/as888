000100990127     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990127     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H* OCO308R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500000000     H*             - STAMPA BOLLE DI PRELIEVO -                     *
000600000000     H*             ----------------------------                     *
000700000000     H*                                                      AZ.O308 *
000800990127     H*         FILE : tmmoc06L - CNACO00F - CNIND00F -              *
000900970801     H*                ANTAB00F - FNSPE03L - FNSPE01L -              *
001000000000     H*--------------------------------------------------------------*
001100990127     Ftmmoc06L  IF   E           K DISK
001200990127     Fancln01l  IF   E           K DISK
001300990127     Fansog01l  IF   E           K DISK
001400990127     Fanind01l  IF   E           K DISK
001500870915     FANTAB00F  IF   E           K DISK
001600990127     Ftmamp01L  IF   E           K DISK
001700870731     F*--------- LUOGHI DI DESTINAZIONE -----------------------------*
001800970801     FFNSPE03L  IF   E           K DISK
001900970801     F                                     RENAME(FNSPE000:ANSPE3)
002000870915     F*
002100870915     F*--------- SPEDIZIONIERI --------------------------------------*
002200970801     FFNSPE01L  IF   E           K DISK
002300970801     F                                     RENAME(FNSPE000:ANSPE1)
002400000000     FOCPRTSTD  O    F  132        PRINTER
002500000000     D*--------------------------------------------------------------*
002600000000     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 1
002700000000     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 2
002800000000     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 3
002900000000     D SK4             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 4
003000000000     D SK5             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 5
003100870918     D TOR             S              2    DIM(10)                              *TIPO ORDINE
003200870918     D DTOR            S             25    DIM(10)                              *DESCRIZIONE
003300870918     D COM             S             30    DIM(10)                              *COMMENTO
003400000000     D*--------------------------------------------------------------*
003500000000     D PARAM2          DS
003600990127     D  VIDCLI                 1      8
003700990127     D  VIDNOR                 9     13  0
003800990127     d  socpas                14     16
003900870915     D ORDDS           DS
004000871117     D* CLIENTE/LUOGO/ SPEDIZIONIERE/ RESA/CAUSALE TRASPORTO
004100990127     D  MOCCLI                 1      8
004200990127     D  MOCLUO                 9     11
004300990127     D  MOCSPE                12     14
004400990127     D  MOCRES                15     17
004500990127     D  MOCTRO                18     18
004600870915     D PREDS           DS
004700871117     D* CLIENTE/LUOGO/ SPEDIZIONIERE/ RESA /CAUS.TRASP. PRECEDENTI
004800990127     D  PRECLI                 1      8
004900990127     D  PRELUO                 9     11
005000990127     D  PRESPE                12     14
005100990127     D  PRERES                15     17
005200990127     D  PRETRO                18     18
005300870918     D DSTAB           DS
005400870918     D*DESCRIZIONE TIPO ORDINE /FLAG VISIONE
005500870918     D  DESTOR                 1     25
005600870918     D  TABTOR                35     35
005700000000     D*
005800000000     D KPJBA         E DS
005900000000     I*---- PASSAGGIO PARAMETRI
006000990127     D soc001        E DS                  EXTNAME(xsoc001ds)
006100990127     D xsocds          DS          1000
006200990127     d dataiso         s               d   datfmt(*iso)
006300990127     d dataeur         s               d   datfmt(*eur)
006400000000     C*----------------------------------------------------*
006500000000     C     *ENTRY        PLIST                                                  *
006600000000     C                   PARM                    KPJBA                          *
006700000000     C******************************************************
006800990127     C*--- FILE tmmoc06L 'CODICE CLIENTE'                  *
006900870918     C     KEYCLI        KLIST                                                  *
007000870918     C                   KFLD                    VIDCLI                         *
007100870702     C*--- FILE CNACO00F 'UTENTE CAPO CONTO CODICE CLIENTE *
007200871222     C     KMAT          KLIST                                                  *
007300871222     C                   KFLD                    PREMAT                         *
007400990127     C     KEYcln        KLIST                                                  *
007500990127     C                   KFLD                    socpas                         *
007600990127     C                   KFLD                    KSC               8            *
007700870731     C*--- CHIAVE PER RICERCA LUOGO                        *
007800870731     C     KEYSPE        KLIST                                                  *
007900970801     C                   KFLD                    SPEFLS                         *
008000870731     C                   KFLD                    SPECLI                         *
008100870731     C                   KFLD                    CODSPE            3            *
008200870915     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F" -------------*
008300870915     C     KEYANT        KLIST                                                  *
008400870918     C                   KFLD                    GRU               3            *
008500870918     C                   KFLD                    COD               7            *
008600870918     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F" -------------*
008700870918     C     KEYTAB        KLIST                                                  *
008800870918     C                   KFLD                    GRU1              3            *
008900870702     C******************************************************
009000870702     C*AZZERAMENTI
009100870702     C                   Z-ADD     0             PAGE              3 0          *
009200870702     C                   Z-ADD     50            PAGE1             3 0          *
009300870716     C                   Z-ADD     0             TOTQTA           11 2          *
009400870702     C                   MOVEL     *BLANKS       VIDCLI                         *
009500870702     C                   Z-ADD     0             VIDNOR                         *
009600870915     C                   MOVEL     *BLANKS       ORDDS                          *
009700870915     C                   MOVEL     *BLANKS       PREDS                          *
009800870930     C                   MOVEL     *BLANKS       PREMAT           12            *
009900990127     C*
010000990127     C                   MOVEL     KPJBU         PARAM2                         *
010100990127     C*---------- RICERCA DITTA :
010200990127     C                   MOVEL     'SOC001'      TIPXSC
010300990127     C                   MOVEL     *blank        SOCXSC
010400990127     C                   EXSR      REPSOC
010500990127     C     RTNXSC        IFNE      '1'
010600990127     C                   MOVEL     XSOCDS        SOC001
010700990127     C                   MOVEL     xscrgs        desdit           20
010800990127     c                   end
010900990128     c                   time                    como14           14 0
011000990128     c                   move      como14        woggi             8 0
011100870918     C                   EXSR      CARVIS                                       *
011200870702     C                   EXSR      LEGGI                                        *
011300870702     C     FINE          TAG                                                    *
011400870702     C                   SETON                                        LR        *
011500870918     C******************************************************
011600870918     C     CARVIS        BEGSR
011700870918     C**----------------------------------------
011800870918     C** CARICAMENTO TIPI ORDINI CON FLAG SCARICO
011900870918     C** MAGAZZINO = N
012000870918     C**----------------------------------------
012100870918     C                   MOVEL     '162'         GRU1
012200870918     C     KEYTAB        SETLL     ANTAB000                               07
012300870918     C  N07              GOTO      ENDVIS
012400870918     C                   DO        *HIVAL
012500870918     C     KEYTAB        READE     ANTAB000                               06
012600870918     C   06              GOTO      ENDVIS
012700870918     C                   MOVEL     TABDES        DSTAB
012800870918     C     TABTOR        IFEQ      'N'
012900870918     C                   ADD       1             K                 3 0
013000870918     C                   MOVE      TABCOD        TOR(K)
013100870918     C                   MOVEL     DESTOR        DTOR(K)
013200870918     C                   END
013300870918     C                   END
013400870918     C     ENDVIS        ENDSR
013500870702     C******************************************************
013600870702     C     LEGGI         BEGSR
013700870918     C**----------------------------------------
013800870918     C** LETTURA
013900870918     C**----------------------------------------
014000990127     C     VIDCLI        IFNE      '99999999'
014100990127     C     KEYCLI        SETLL     tmmoc000                               04
014200870716     C  N04              GOTO      END1
014300870716     C                   END
014400870716     C*-------------------
014500871123     C     SU            TAG
014600990127     C     VIDCLI        IFEQ      '99999999'
014700990127     C                   READ      tmmoc000                               05
014800870918     C   05              EXSR      TOT
014900870918     C   05              EXSR      STACOM
015000870716     C   05              GOTO      END1
015100870716     C                   ELSE
015200990127     C     KEYCLI        READE     tmmoc000                               05
015300870918     C   05              EXSR      TOT
015400870918     C   05              EXSR      STACOM
015500870716     C   05              GOTO      END1
015600870716     C                   END
015700870716     C*-------------------
015800870716     C     VIDNOR        IFNE      99999
015900870716     C     MOCNOR        CABNE     VIDNOR        SU
016000870716     C                   END
016100870921     C     MOCTRG        CABEQ     '1'           LA
016200880719     C     MOCTRG        CABEQ     '8'           STAR
016300870921     C                   GOTO      SU
016400870921     C     LA            TAG
016500870716     C*-------------------
016600870702     C     MOCTRG        IFEQ      '1'
016700880719     C     MOCFAS        CABNE     'AS'          SU
016800870925     C                   EXSR      STAMP1
016900870702     C                   EXSR      STAMP3
017000870703     C                   GOTO      SU
017100870702     C                   END
017200880719     C     STAR          TAG
017300870702     C     MOCTRG        IFEQ      '8'
017400870918     C                   EXSR      MEMCOM
017500870702     C                   END
017600870702     C                   GOTO      SU
017700870716     C     END1          ENDSR
017800870921     C******************************************************
017900870921     C     STAMP1        BEGSR
018000870921     C**----------------------------------------
018100870921     C** CONTROLLO ROTTURE
018200870921     C**----------------------------------------
018300870921     C  N99              MOVEL     ORDDS         PREDS
018400870921     C  N99              MOVEL     MOCMAT        PREMAT
018500870921     C  N99              SETON                                            99
018600870921     C     MOCMAT        IFNE      PREMAT
018700870921     C                   EXSR      TOT
018800870921     C                   MOVEL     MOCMAT        PREMAT
018900870921     C                   END
019000870921     C     ORDDS         IFNE      PREDS
019100870921     C                   EXSR      TOT
019200870921     C                   EXSR      STACOM
019300870921     C                   Z-ADD     50            PAGE1
019400870921     C                   EXSR      RUT1
019500870921     C                   MOVEL     ORDDS         PREDS
019600870921     C                   MOVEL     MOCMAT        PREMAT
019700870921     C                   END
019800870921     C                   ENDSR
019900870702     C******************************************************
020000870702     C     STAMP3        BEGSR
020100870918     C**----------------------------------------
020200870921     C** STAMPA DETTAGLIO
020300870918     C**----------------------------------------
020400870702     C                   ADD       MOCQTA        TOTQTA
020500990127     c                   if        mocdor > 0
020600990127     c                   move      mocdor        dataiso
020700990127     c                   move      dataiso       dataeur
020800990127     c                   move      dataeur       dataor            8 0
020900990127     c                   end
021000990127     c                   if        mocdcp > 0
021100990127     c                   move      mocdcp        dataiso
021200990127     c                   move      dataiso       dataeur
021300990127     c                   move      dataeur       datcon            8 0
021400990127     c                   end
021500870918     C*RIGA IN CONTO VISIONE O PROVA DA NON SCARICARE DAL
021600870918     C*MAGAZZINO
021700870918     C                   SETOFF                                       06
021800870918     C                   Z-ADD     1             A                 3 0
021900870918     C     MOCTOR        LOOKUP    TOR(A)                                 06
022000870702     C                   EXSR      RUTSTA
022100870702     C                   ENDSR
022200870702     C***------------------------------------------------***
022300870702     C**--------- RUTINE EMISSIONE STAMPA ---------------***
022400870702     C***------------------------------------------------***
022500870702     CSR   RUTSTA        BEGSR                                                  *
022600870702     C     PAGE1         IFGE      50                                           *
022700870710     C                   MOVEL     MOCCLI        KSC                            *
022800990127     C     KEYcln        CHAIN     ancln01l                           35        *
022900990127     C     clnsogg       CHAIN     ansog01l                           35        *
023000871117     C   35              MOVEL     *BLANKS       RAG                            *
023100990127     C  N35              MOVEL     sogdes        RAG              30            *
023200990127     C     clnsogg       CHAIN     anind01l                           46        *
023300870731     C*RICERCA DEL LUOGO DI DESTINAZIONE SE DIVERSO DALL'ANAGRAFICO
023400870731     C     MOCLUO        COMP      *BLANKS                                10
023500870731     C  N10              DO
023600970801     C                   MOVEL     'L'           SPEFLS
023700990127     C                   MOVE      MOCCLI        SPECLI
023800870731     C                   MOVEL     MOCLUO        CODSPE
023900870915     C     KEYSPE        CHAIN     ANSPE3                             23
024000871117     C   23              MOVEL     *BLANKS       RAGSOC
024100871117     C   23              MOVEL     *BLANKS       INLUO
024200871117     C   23              MOVEL     *BLANKS       LOCLUO
024300970801     C   23              CLEAR                   CAPLUO
024400871117     C   23              MOVEL     *BLANKS       PROLUO
024500871117     C  N23              DO
024600870918     C                   MOVEL     SPERAG        RAGSOC
024700870918     C                   MOVEL     SPEIND        INLUO
024800870918     C                   MOVEL     SPELOC        LOCLUO
024900970801     C                   MOVEL     SPECAP        CAPLUO
025000870918     C                   MOVEL     SPEPRO        PROLUO
025100871117     C                   END
025200870731     C                   END
025300870915     C*RICERCA DEL SPEDIZIONIERE
025400870915     C     MOCSPE        COMP      *BLANKS                                11
025500870915     C  N11              DO
025600970801     C                   MOVEL     'S'           SPEFLS
025700870915     C                   MOVEL     9999999       SPECLI
025800870915     C                   MOVEL     MOCSPE        CODSPE
025900870915     C     KEYSPE        CHAIN     ANSPE1                             24
026000871117     C   24              MOVEL     *BLANKS       DESSPE
026100870915     C  N24              MOVEL     SPERAG        DESSPE           24
026200870915     C                   END
026300870915     C*RICERCA DEL CODICE RESA SULLA TABELLA '019'
026400870918     C                   MOVE      '019'         GRU
026500870918     C                   MOVE      *ZEROS        COD
026600870918     C                   MOVE      MOCRES        COD
026700870915     C     KEYANT        CHAIN     ANTAB00F                           47
026800871117     C   47              MOVEL     *BLANKS       DESRES
026900871117     C  N47              MOVEL     TABDES        DESRES           26
027000871117     C*RICERCA CAUSALE DEL TRASPORTO
027100871123     C                   MOVE      '707'         GRU
027200871123     C                   MOVEL     *BLANKS       COD
027300871123     C                   MOVE      MOCTRO        COD
027400871123     C     KEYANT        CHAIN     ANTAB00F                           48
027500871123     C   48              MOVEL     *BLANKS       DESTRO
027600871123     C  N48              MOVEL     TABDES        DESTRO           30
027700870702     C                   EXCEPT    TESTA                                        *
027800870702     C                   Z-ADD     0             PAGE1                          *
027900870702     C                   END                                                    *
028000870702     C                   EXCEPT    DETAIL                                       *
028100870702     C                   ENDSR                                                  *
028200870710     C***------------------------------------------------***
028300870731     CSR   RUT1          BEGSR                                                  *
028400870731     C     PAGE1         IFGE      50                                           *
028500870731     C                   MOVEL     MOCCLI        KSC                            *
028600990127     C     KEYcln        CHAIN     ancln01l                           35        *
028700990127     C     clnsogg       CHAIN     ansog01l                           35        *
028800871117     C   35              MOVEL     *BLANKS       RAG                            *
028900990127     C  N35              MOVEL     sogdes        RAG              30            *
029000990127     C     clnsogg       CHAIN     anind01l                           46        *
029100870731     C*RICERCA DEL LUOGO DI DESTINAZIONE SE DIVERSO DALL'ANAGRAFICO
029200870731     C     MOCLUO        COMP      *BLANKS                                10
029300870731     C  N10              DO
029400970801     C                   MOVEL     'L'           SPEFLS
029500870731     C                   MOVEL     MOCCLI        SPECLI
029600870731     C                   MOVEL     MOCLUO        CODSPE
029700870915     C     KEYSPE        CHAIN     ANSPE3                             23
029800871117     C   23              MOVEL     *BLANKS       RAGSOC
029900871117     C   23              MOVEL     *BLANKS       INLUO
030000871117     C   23              MOVEL     *BLANKS       LOCLUO
030100970801     C   23              CLEAR                   CAPLUO
030200871117     C   23              MOVEL     *BLANKS       PROLUO
030300871117     C  N23              DO
030400970819     C                   MOVEL     SPERAG        RAGSOC           35
030500970819     C                   MOVEL     SPEIND        INLUO            35
030600970819     C                   MOVEL     SPELOC        LOCLUO           35
030700970801     C                   MOVEL     SPECAP        CAPLUO            9
030800870918     C                   MOVEL     SPEPRO        PROLUO            2
030900871117     C                   END
031000870731     C                   END
031100870915     C*RICERCA DEL SPEDIZIONIERE
031200870915     C     MOCSPE        COMP      *BLANKS                                11
031300870915     C  N11              DO
031400970801     C                   MOVEL     'S'           SPEFLS
031500870915     C                   MOVEL     9999999       SPECLI
031600990127     C                   MOVEl     MOCSPE        CODSPE
031700870915     C     KEYSPE        CHAIN     ANSPE1                             24
031800871117     C   24              MOVEL     *BLANKS       DESSPE
031900870915     C  N24              MOVEL     SPERAG        DESSPE           24
032000870915     C                   END
032100870915     C*RICERCA DEL CODICE RESA SULLA TABELLA '019'
032200870918     C                   MOVE      '019'         GRU
032300870918     C                   MOVE      *ZEROS        COD
032400870918     C                   MOVE      MOCRES        COD
032500870915     C     KEYANT        CHAIN     ANTAB00F                           47
032600871117     C   47              MOVEL     *BLANKS       DESRES
032700871117     C  N47              MOVEL     TABDES        DESRES
032800871117     C*RICERCA CAUSALE DEL TRASPORTO
032900871123     C                   MOVE      '707'         GRU
033000871123     C                   MOVEL     *BLANKS       COD
033100871123     C                   MOVE      MOCTRO        COD
033200871123     C     KEYANT        CHAIN     ANTAB00F                           48
033300871123     C   48              MOVEL     *BLANKS       DESTRO
033400871123     C  N48              MOVEL     TABDES        DESTRO
033500870731     C                   EXCEPT    TESTA                                        *
033600870731     C                   Z-ADD     0             PAGE1                          *
033700870731     C                   END                                                    *
033800870731     C                   ENDSR
033900870918     C******************************************************
034000870918     C     MEMCOM        BEGSR
034100870918     C**----------------------------------------
034200870918     C** MEMORIZZAZIONE COMMENTI
034300870918     C**----------------------------------------
034400870918     C                   Z-ADD     0             A
034500870918     C                   DO        10            A                 3 0
034600870918     C     COM(A)        IFEQ      *BLANKS
034700870918     C                   MOVEL     MOCDMT        COM(A)
034800870918     C                   GOTO      ENDCOM
034900870918     C                   END
035000870918     C                   END
035100870918     C     ENDCOM        ENDSR
035200870918     C******************************************************
035300870918     C     TOT           BEGSR
035400870918     C**----------------------------------------
035500870918     C** STAMPA TOTALI
035600870918     C**----------------------------------------
035700990127     C     KMAT          CHAIN     tmamp01L                           12
035800871222     C   12              MOVEL     *BLANKS       AMPUBI
035900871222     C** UBICAZIONE
036000870918     C     TOTQTA        IFGT      0
036100870918     C                   EXCEPT    TOTALE
036200870918     C                   ADD       TOTQTA        GENQTA           15 2
036300870918     C                   END
036400870918     C                   Z-ADD     0             TOTQTA
036500870918     C                   ENDSR
036600870918     C******************************************************
036700870918     C     STACOM        BEGSR
036800870918     C**----------------------------------------
036900870918     C** STAMPA COMMENTI
037000870918     C**----------------------------------------
037100870918     C     GENQTA        IFGT      0
037200870918     C                   DO        10            A
037300870918     C     COM(A)        IFNE      *BLANKS
037400870918     C                   EXCEPT    SCOM
037500870918     C                   END
037600870918     C                   END
037700870918     C                   END
037800870918     C                   Z-ADD     0             GENQTA
037900870918     C                   MOVEA     *BLANKS       COM
038000870918     C                   ENDSR
038100990127     C*----------------------------------------------------*
038200990127     C* Reperimento dati società
038300990127     C*----------------------------------------------------*
038400990127     C     REPSOC        BEGSR
038500990127     C*
038600990127     C                   CALLB     'XSOC'
038700990127     C                   PARM                    TIPXSC            6
038800990127     C                   PARM      socpas        SOCXSC            3
038900990127     C                   PARM                    CDSXSC            9 0
039000990127     C                   PARM                    MODXSC            3
039100990127     C                   PARM      *blanks       RTNXSC            1
039200990127     C                   PARM                    XSOCDS
039300990127     C                   PARM                    KPJBA
039400990127     C*
039500990127     C                   ENDSR
039600870702     O*--------------------------------------------------------------*
039700870702     O*     TESTATA                                                  *
039800870702     O*--------------------------------------------------------------*
039900870702     OOCPRTSTD  E            TESTA            01
040000870702     O                       SK1                132
040100870702     O                       DESDIT              20
040200990128     O                       woggi              118 '  /  /    '
040300870702     O                       PAGE          Z    132
040400870702     O          E            TESTA       1
040500870702     O                       SK2                132
040600870702     O          E            TESTA       1
040700870702     O                       SK3                132
040800870702     O                       KNMUS              132
040900870702     O          E            TESTA       2
041000870918     O                       MOCCLI               8
041100870918     O                       RAG                 40
041200871123     O              N10                          73 'Luogo di Destinazione:'
041300970819     O              N10      RAGSOC             115
041400870918     O          E            TESTA       1
041500990127     O                       INDindriz           45
041600970819     O              N10      INLUO              115
041700870918     O          E            TESTA       1
041800990127     O                       INDCAP              11
041900990127     O                       INDlocalit          46
042000990127     O                       INDPRov             49
042100970801     O              N10      CAPLUO              83
042200970819     O              N10      LOCLUO             115
042300970819     O              N10      PROLUO             118
042400871123     O          E            TESTA       2
042500871123     O                                           15 'Resa:'
042600871117     O                       DESRES              42
042700910723     O                                           85 'SPEDIZIONE APPRONTATA DA'
042800910723     O                                         +  1 '------------------------'
042900910723     O                                              '---------------'
043000871123     O          E            TESTA       1
043100871123     O                                           24 'Spedizioniere:'
043200870918     O                       DESSPE              49
043300871123     O                                           83 'Causale del Trasporto:'
043400871117     O                       DESTRO             115
043500870918     O          E            TESTA       2  1
043600870702     O                       SK4                132
043700870731     O*--------------------------------------------------------------*
043800870731     O*     DETTAGLIO                                                *
043900870731     O*--------------------------------------------------------------*
044000870918     O          E            DETAIL      1
044100870702     O                       PAGE1              132
044200870702     O                       SK5                132
044300870930     O                       MOCMAT              13
044400870930     O                       MOCDMT              44
044500870702     O                       MOCQTA        2     63
044600870702     O                       MOCNOR        Z     89
044700990127     O                       DATCON             104 '  /  /    '
044800870918     O               06      DTOR(A)            131
044900870702     O*--------------------------------------------------------------*
045000870702     O*     TOTALE  QUANTITA'                                        *
045100870702     O*--------------------------------------------------------------*
045200870918     O          E            TOTALE      0  0
045300870702     O                       TOTQTA        2     79
045400871222     O                       AMPUBI             116
045500870918     O          E            TOTALE      0  0
045600870710     O                       TOTQTA        2     79
045700870702     O*--------------------------------------------------------------*
045800870702     O*     RIGA DI COMMENTO                                         *
045900870702     O*--------------------------------------------------------------*
046000870918     O          E            SCOM        1
046100870703     O                       SK5                132
046200870918     O                       COM(A)              43
046300870918     O          E            SCOM        0  0
046400870918     O                       COM(A)              43
046500871119**   SK1
046600870702*-DESDIT-----------****-------------------------------------------SK01
046700870702-------------------------------------*** il XX/XX/XX      Pag. XXXSK01
046800870702**   SK2
046900870702                            ** Gest. Ordini Clienti : STAMPA BOLLESK02
047000990317 DI PRELIEVO  **                                PGM.:   TII310R   SK02
047100870702**   SK3
047200870702                    ***-------------------------------------------SK03
047300870702-------------------------------------***        Utente  XXXXXXXXXXSK03
047400870702**   SK4
047500870915*---------------A R T I C O L O-------------*-Q.TA'DA PRELEVARE-*-SK04
047600880601---TOTALE-----*-N.ORDINE-*-DATA CONS.*-- UBICAZIONE --*           SK04
047700870702**   SK5
047800871119                                                                  SK05
047900871119                                                                  SK05
