000100090127     H DFTACTGRP(*NO) BNDDIR('PRNPGM')
000200990324     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300891128     H* OCO319R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500000000     H*                    - FATTURAZIONE -                          *
000600000000     H*                    -----------------                         *
000700000000     H*         FILE : OCMOC14L - ANCLP01L - CNACO00F -              *
000800951219     H*                CNIND00F - TABEL00F                           *
000900990326     H*                ANTAB00F -          -
001000000000     H*--------------------------------------------------------------*
001100990324     FtmMOC14L  UF   E           K DISK
001200990331     FtmCLP01L  IF   E           K DISK
001300990329     Ftmamp01l  IF   E           K DISK
001400990325     Fancln01l  IF   E           K DISK
001500990325     Fanind01l  IF   E           K DISK
001600990325     Fansog01l  IF   E           K DISK
001700990329     Fybacl01l  IF   E           K DISK
001800990326     Fandic01l  iF   E           K DISK
001900000000     FTABEL00F  IF   E           K DISK
002000000000     FANTAB00F  IF   E           K DISK
002100000000     FFAWEPPRT  O    F  132        PRINTER OFLIND(*INOF)
002200000000     FOCPRTSTD  O    F  132        PRINTER USROPN
002300000000     F*
002400090127     Dndbhm00f       E DS
002500090127     Dndbhi00f       E DS
002600090127     Dndbha00f       E DS
002700990331     D AIV             S              8    DIM(10)                              ALIQUOTA IVA
002800990326     D IIV             S             14  5 DIM(10)                              IMPONIBILE
002900020103     D iivw            S             14  2 DIM(10)                              IMPONIBILE
003000990326     D IV              S             14  5 DIM(10)                              IVA  X AL.IVA
003100020103     D ivw             S             14  2 DIM(10)                              IVA  X AL.IVA
003200000000     D DIV             S             15    DIM(10)                              DESCRIZ.ESEZ.IVA
003300990401     D CRI             S             30    DIM(100)                             C/C E S/C CONTROP
003400990326     D IRI             S             14  5 DIM(100)                             IMPONIBILI RICAVI
003500020103     D iriw            S             14  2 DIM(100)                             IMPONIBILI RICAVI
003600000000     D TOR             S              2    DIM(10)                              *TIPO ORDINE
003700990401     D SC              S             80    DIM(10) CTDATA PERRCD(1)
003800000000     D PARAM2          DS
003900000000     D*CAMPI VIDEO : N.FATTURA, DATA FATTURA,TIPO FATTURAZIONE,
004000000000     D*DATA BOLLA
004100990324     D  VIDNFT                 1      7  0
004200990324     D  VIDDFT                 8     15  0
004300990324     D  VIDTIP                16     19
004400990324     D  VIDDBL                20     27  0
004500990324     d  wsocieta              28     30
004600000000     D*SCOMPOSIZIONE DEL CAMPO IVA :COD.IVA, LIBRO , REGISTRO
004700000000     D*TIPO, ALIQUOTA IVA.
004800990331     D IVDS            DS
004900990331     D  IVaese                 1      3
005000990331     D  IVaalq                 4      8  2
005100000000     D PAGDS           DS
005200000000     D*DESCRIZIONE PAGAMENTO E FLAG: 0 = RIMESSA DIRETTA
005300000000     D*                              1 = RICEVUTA BANCARIA
005400000000     D  DESCPG                 1     30
005500000000     D  TIPCDP                31     31
005600000000     D RCDS            DS
005700000000     D*CLIENTE /CONDIZIONI DI PAGAMENTO
005800990324     D  MOCCLI                 1      8
005900990324     D  MOCCPG                 9     11
006000000000     D RCPRE           DS
006100000000     D*CLIENTE /CONDIZIONE DI PAGAMENTO PRECEDENTI
006200990324     D  PRECLI                 1      8
006300990324     D  PRECPG                 9     11
006400000000     D BLDS            DS
006500000000     D*NUMERO BOLLA/DATA BOLLA
006600000000     D  MOCNBL                 1      7  0
006700990324     D  MOCDBL                 8     15  0
006800000000     D PREBL           DS
006900000000     D*N.BOLLA /DATA BOLLA PRECEDENTE
007000000000     D  PRENBL                 1      7  0
007100990324     D  PREDBL                 8     15  0
007200000000     D RESDS           DS
007300000000     D*DESCRIZIONE RESA /IMPORTO RESA
007400000000     D  DESRES                 1     30
007500000316     D  IMPRES                31     45s 5
007600000000     D*DESCRIZIONE/  C/C IVA/  S/C IVA/ C/C CONTROPARTITA
007700000000     D* S/C CONTROPARTITA
007800990329     D dscp          e DS                  OCCURS(30)
007900990329      *
008000000000     D IMBDS           DS
008100000000     D*DESCRIZIONE IMBALLO/ IMPORTO IMBALLO
008200000000     D  DESIMB                 1     30
008300000316     D  IMPIMB                31     45s 5
008400000000     D ORDS            DS
008500000000     D*NUMERO ORDINE/DATA ORDINE/N.RIF.VS.ORDINE/
008600000000     D*DATA RIF.VS.ORDINE
008700000000     D  MOCNOR                 1      5  0
008800990324     D  MOCDOR                 6     13  0
008900990324     D  MOCNRF                14     20  0
009000990324     D  MOCDRF                21     28  0
009100000000     D PREOR           DS
009200000000     D*NUMERO ORDINE/DATA ORDINE/N.RIF.VS.ORDINE/
009300000000     D*DATA RIF.VS.ORDINE PRECEDENTI
009400000000     D  PRENOR                 1      5  0
009500990324     D  PREDOR                 6     13  0
009600990324     D  PRENRF                14     20  0
009700990324     D  PREDRF                21     28  0
009800000000     D NPCDS           DS
009900000000     D*NUMERO PROTOCOLLO
010000000000     D  MOCNPC                 1      9  0
010100990324     D  MOCDPC                10     17  0
010200990324     D  MOCNPN                18     26  0
010300000000     D NPCPRE          DS
010400000000     D*NUMERO PROTOCOLLO PRECEDENTE
010500000000     D  PRENPC                 1      9  0
010600990324     D  PREDPC                10     17  0
010700990324     D  PRENPN                18     26  0
010800000000     D SPEDS           DS
010900000000     D*SPESE RESA /SPESE IMBALLO
011000000000     D  MOCRES                 1      3
011100000000     D  MOCIMB                 4      6
011200000000     D SPEPRE          DS
011300000000     D*SPESE RESA /SPESE IMBALLO PRECEDENTI
011400000000     D  PRERES                 1      3
011500000000     D  PREIMB                 4      6
011600000000     D DSTAB           DS
011700000000     D*DESCRIZIONE TIPO ORDINE /FLAG VISIONE
011800000000     D  DESTOR                 1     25
011900000000     D  TABTOR                39     39
012000000000     D*
012100000000     D KPJBA         E DS
012200951219     D*-------------
012300951219     D* DS PAGAMENTO BOLLO EFFETTI
012400951219     D*-------------
012500951219     D* DS DATI VARI CONTABILITA'
012600951219     D*-------------
012700951219     D DSQC1         E DS
012800951219     D DSQC2         E DS
012900951219     D*-------------
013000951219     D* DS DATI VARI TASSAZIONE
013100951219     D*-------------
013200000211     D***** DSQT1         E DS
013300000211     D DSBS02        E DS                  EXTNAME(TIBS02DS)
013400000211     D dgei          E DS
013500000000     D*---- PASSAGGIO PARAMETRI
013600000000     D  LIBSYS                99    107
013700000000     D  MSGERR               453    502
013800000000     D CPADS           DS
013900990401     D  CPACC                         6
014000990401     D  CPASC                         8
014100990401     D  CPACDC                        8
014200990401     D  CPAvoc                        8
014300990324     d dataiso         s               d   datfmt(*iso)
014400990324     d dataeur         s               d   datfmt(*eur)
014500990602     d decdds          ds
014600990602     d decds                         14  5
014700990602     d  int                           9  0 overlay(decds:1)
014800990602     d  dec                           1  0 dim(5) overlay(decds:10)
014900990324     D soc001        e ds                  extname(xsoc001ds)
015000990324     D xsocds          DS          1000
015100990324     D  XNMDAG         S               d   datfmt(*iso)
015200990324     d  XNMDAT         S               d   datfmt(*iso)
015300990325     D ndc071ds      E DS
015400010622     D ndc270ds      E DS
015500990331     D tb155         e DS                  extname(antab155)
015600030228     D WrkSIn          S              5  2
015700000000     C*----------------------------------------------------*
015800000000     C     *ENTRY        PLIST                                                  *
015900000000     C                   PARM                    KPJBA                          *
016000990324     C     PNMR          PLIST
016100990324     C                   PARM                    XNMRIC            1
016200990324     C                   PARM                    XNMSOC
016300990324     C                   PARM                    XNMUNI
016400990324     C                   PARM                    XNMCTB
016500990324     C                   PARM                    XNMKEY
016600990324     C                   PARM                    XNMSER
016700990324     C                   PARM                    XNMCMT            1
016800990324     C                   PARM                    XNMDAT
016900990324     C                   PARM                    XNMIVA            1
017000990324     C                   PARM                    XNMREG            1
017100990324     C                   PARM                    XNMERR            1
017200990324     C                   PARM                    XNMNUM            9 0
017300990324     C                   PARM                    XNMAUT            1
017400990324     C                   PARM                    XNMDAG                         opzionale
017500990330     C*PULIZIA DS
017600990330     C                   MOVEL     *BLANKS       CPADS
017700990330     C                   Z-ADD     66            CONERR            3 0
017800990330     C                   MOVEL     *ZEROS        NPCPRE
017900990330     C                   MOVEL     *ZEROS        NPCDS
018000990331     C                   Z-ADD     0             IVAaLQ
018100990330     C                   MOVEL     *BLANKS       RCDS
018200990330     C                   MOVEL     *BLANKS       RCPRE
018300990330     C                   MOVEL     *ZEROS        BLDS
018400990330     C                   MOVEL     *ZEROS        PREBL
018500990330     C                   MOVEL     *ZEROS        ORDS
018600990330     C                   MOVEL     *ZEROS        PREOR
018700990330     C                   MOVEL     *BLANKS       DESRES
018800990330     C                   Z-ADD     0             IMPRES
018900990330     C                   MOVEL     *BLANKS       DESIMB
019000990330     C                   Z-ADD     0             IMPIMB
019100990330     C                   MOVEL     *BLANKS       PAGDS
019200990330     C                   Z-ADD     0             VIDNFT
019300990330     C                   Z-ADD     0             VIDDFT
019400990330     C                   Z-ADD     0             VIDDBL
019500990330     C                   MOVEL     *BLANKS       DESTOR
019600990330     C                   MOVEL     *BLANKS       TABTOR
019700990330     C                   MOVEL     *BLANKS       VIDTIP
019800990330     C                   MOVEL     *BLANKS       SPEDS
019900990330     C                   MOVEL     *BLANKS       SPEPRE
020000990330     C                   MOVEL     KPJBU         PARAM2
020100990324     C*---------- RICERCA DITTA :
020200990324     C                   MOVEL     'SOC001'      TIPXSC
020300990324     C                   MOVEL     *blank        SOCXSC
020400990324     C                   EXSR      REPSOC
020500990324     C     RTNXSC        IFNE      '1'
020600990324     C                   MOVEL     XSOCDS        SOC001
020700990324     C                   MOVEL     xscrgs        rsut             20
020800990324     C                   MOVEL     xscrgs        rsc              30
020900990324     c                   end
021000990324     C*----------                                          *
021100990326     C                   MOVEL     xscvia        IND              30
021200990326     C                   movel     xscCAP        CAP               5
021300990326     C                   MOVEL     xsccit        LOC              10
021400990326     C                   MOVEL     xscprv        PRV               2
021500990326     C                   MOVEL     xsccdf        CFI              16
021600990326     C                   MOVEL     xscatt        TRI               9
021700990326     C                   MOVEL     xsccct        COM               9
021800990326     C*                  Z-ADD     SIMCSO        CSO              10 0
021900990324     c                   time                    como14           14 0
022000990324     c                   move      como14        woggi             8 0
022100000000     C******************************************************
022200000000RIC  C*-- RICERCA PARAMETRI VARI :                         *
022300000000     C******************************************************
022400000000     C*--- RICERCA DESCRIZIONE COND.DI PAGAMENTO 'TABEL00F'*
022500000000     C     KEYTAB        KLIST                                                  *
022600000000     C                   KFLD                    CODUT                          *
022700000000     C                   KFLD                    COD               2            *
022800000000     C                   KFLD                    KEY               8            *
022900990324     c                   z-add     1             codut             1 0
023000990326     C*--- RICERCA RAS.SOCIALE CLIENTE
023100990324     C     KEYcln        KLIST                                                  *
023200990324     C                   KFLD                    wsocieta                       *
023300990324     C                   KFLD                    KSC               8            *
023400990324     C     Keyind        KLIST
023500990324     C                   KFLD                    sogsogg
023600990324     C                   KFLD                    tpin
023700990324     C                   KFLD                    cdin
023800990324     c                   move      *blank        tpin
023900990324     c                   move      *blank        cdin
024000990329     C     Keybac        klist
024100990329     C                   kfld                    wsocieta
024200990329     C                   kfld                    clnkcc
024300990329     C                   kfld                    clnksc
024400990326     C     *LIKE         DEFINE    indcdind      cdin
024500990326     C     *LIKE         DEFINE    indtpind      tpin
024600990326     C     Kandic        klist
024700990326     C                   kfld                    wsocieta
024800990326     C                   kfld                    wcbacli           1
024900990326     C                   kfld                    precli
025000990326     c                   move      'C'           wcbacli
025100000000     C*--- RICERCA DESCR. IMBALLO 'ANTAB00F'               *
025200000000     C     KEYANT        KLIST                                                  *
025300000000     C                   KFLD                    GRU               3            *
025400000000     C                   KFLD                    KEYT              7            *
025500000000     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F" -------------*
025600000000     C     KEYTOR        KLIST                                                  *
025700000000     C                   KFLD                    GRU1              3            *
025800000000     C*----------------------------------------------------*
025900000000     C*CONTI CONTABILITA' DS MULTIPLA
026000000000     C                   EXSR      CARCC
026100000000     C*CARICAMENTO TIPO ORDINI CON FLAG3 =N
026200000000     C                   EXSR      CARVIS                                       *
026300000000     C******************************************************
026400000000     C*PRELIEVO IL 1° NUMERO DI FATTURA DALLA DATA AREA
026500951219     C     KTABEL        KLIST
026600951219     C                   KFLD                    CODUT
026700951219     C                   KFLD                    CODTAB            2
026800951219     C                   KFLD                    CODCOD            8
026900951219     C*
027000951219     C* CARICAMENTO DATI VARI CONTABILITA'
027100951219     C                   EXSR      CARQC
027200951219     C*
027300951219     C* CARICAMENTO DATI VARI TASSAZIONE
027400000211     C*******            EXSR      CARQT
027500000211     C* CARICAMENTO importi bollo da tabella GEI
027600000211     c                   exsr      cargei
027700000211     c*
027800951219     C*
027900891227     C** RICERCA NUMERI FATTURA
028000990326     C                   move      §QCFI3        wserie            4
028100990326     C                   movel     §QCRIV        wserie
028200000000     C******************************************************
028300000000     C                   MOVEL     KNSIF         SYS               3 0
028400990326     C                   MOVEL     xsciva        PIVD             16
028500891128     C*
028600000000     C*ROVESCIO DATA FATTURA IN AAMMGG
028700000000     C*ROVESCIO DATA BOLLA A VIDEO IN AAMMGG
028800990324     C                   Z-ADD     VIDDFT        DATAFT            8 0
028900990324     C                   move      VIDDFT        DATAeur
029000990324     C                   move      dataeur       dataiso
029100990324     C                   move      DATAiso       DATFAT            8 0
029200990324     C                   move      VIDDBL        DATAeur
029300990324     C                   move      dataeur       dataiso
029400990324     C                   move      dataiso       VIDBOL            8 0
029500090127     c                   move      '1'           xnmric
029600090127     c                   move      datfat        xnmdat
029700090127     C*                  exsr      repnum
029800090127     C                   Z-ADD     xnmnum        FATNUM            7 0
029900090127     C                   move      xnmdat        FATDAT            8 0
030000990401      *
030100000000     C     SU            TAG
030200990324     C                   READ      tmMOC000                               02
030300000000     C   02              GOTO      FINE
030400000000     C*ESCUSIONE DELLE RIGHE CON TIPO RIGA DIVERSO DA 1
030500000000     C*ESCLUSIONE DELLE RIGHE DIVERSE DA 'BB'
030600000000     C*ESCLUSIONE DELLE RIGHE CON DATA BOLLA MAGGIORE DI
030700000000     C*QUELLA RICHIESTA A VIDEO
030800000000     C*ESCLUSIONE DELLE RIGHE CON TIPO ORDINE CON FLAG3 =N
030900000000     C     MOCTRG        CABNE     '1'           SU
031000000000     C                   SETOFF                                       06
031100000000     C                   Z-ADD     1             A
031200000000     C     MOCTOR        LOOKUP    TOR(A)                                 06
031300000000     C   06              GOTO      SU
031400000000     C     MOCFAS        CABNE     'BB'          SU
031500000000     C     MOCDBL        CABGT     VIDBOL        SU
031600000000     C  N99              MOVEL     NPCDS         NPCPRE
031700000000     C  N99              MOVEL     SPEDS         SPEPRE
031800000000     C*AL CAMBIO DI CLIENTE/PAGAMENTO INCREMENTO DEL N. DI
031900000000     C*FATTURA, AGGANCIO DELLA RAGIONE SOCIALE PER STAMPA-
032000000000     C*RE  LA TESTATA E AZZERAMENTO DI TUTTI I CAMPI DI COMODO
032100000000     C*SOLO DAL 2 GIRO STAMPO I TOTALI
032200000000     C     RCDS          IFNE      RCPRE
032300000000     C   99              XFOOT     IIV           FINIIV
032400000000     C   99FINIIV        CABEQ     0             STARTX
032500000000     C   99              EXSR      STAORP
032600000000     C   99              EXSR      STACLP
032700000000     C   99              EXSR      CONTR
032800000000     C   99IMPCOM        CABEQ     0             STARTX
032900000000     C   99              EXSR      TOTALE
033000000000     C                   SETON                                        99
033100000000     C                   EXSR      AZZER
033200000000     C     STARTX        TAG
033300000000     C                   EXSR      CONCLI
033400000000     C   03              GOTO      SU
033500000000     C   16              GOTO      SU
033600000000     C                   EXSR      RAGSOC
033700000000     C   20              GOTO      SU
033800000000     C   22              GOTO      SU
033900000000     C                   Z-ADD     0             PAGE              3 0
034000000000     C                   EXCEPT    TESTA
034100000000     C                   MOVEL     RCDS          RCPRE
034200000000     C                   MOVEL     SPEDS         SPEPRE
034300000000     C                   MOVEL     *ZEROS        PREBL
034400000000     C                   MOVEL     *ZEROS        PREOR
034500000000     C                   END
034600000000     C*AL CAMBIO BOLLA  /DATA BOLLA  STAMPA DELL'INTESTAZIONE
034700000000     C*ROVESCIO DATA BOLLA IN GGMMAA
034800000000     C     BLDS          IFNE      PREBL
034900000000     C                   EXSR      STAORP
035000000000     C                   ADD       1             CONBOL            3 0
035100000000     C                   ADD       1             CONTBL            3 0
035200000000     C   OF              EXCEPT    SEGUE
035300000000     C   OF              EXCEPT    TESTA
035400990330     c                   if        mocdbl > 0
035500990324     C                   move      MOCDBL        DATAIso
035600990324     C                   move      dataiso       dataeur
035700990324     C                   move      DATAeur       DATBOL            8 0
035800990330     c                   else
035900990330     c                   z-add     0             datbol
036000990330     c                   end
036100000000     C                   EXCEPT    BOLLA
036200000000     C                   MOVEL     BLDS          PREBL
036300000000     C                   MOVEL     *ZEROS        PREOR
036400000000     C                   END
036500000000     C*AL CAMBIO ORDINE/DATA ORDINE/VS.RIFERIM./DATA VS.RIF.
036600000000     C*STAMPA DELL'INTESTAZIONE
036700000000     C*ROVESCIO DATE ORDINI IN GGMMAA
036800000000     C                   SETOFF                                       18
036900000000     C     ORDS          IFNE      PREOR
037000000000     C     CONBOL        IFEQ      0
037100000000     C                   EXSR      STAORP
037200000000     C                   END
037300000000     C                   Z-ADD     0             CONBOL
037400000000     C                   Z-ADD     0             CONIES
037500000000     C     MOCNRF        IFEQ      0
037600000000     C                   SETON                                        18
037700000000     C                   END
037800000000     C   OF              EXCEPT    SEGUE
037900000000     C   OF              EXCEPT    TESTA
038000990330     c                   if        mocdor > 0
038100990324     C                   move      MOCDOR        DATAIso
038200990324     C                   move      dataiso       dataeur
038300990324     C                   move      dataeur       DATORD            8 0
038400990330     c                   else
038500990330     c                   z-add     0             datord
038600990330     c                   end
038700000000     C*
038800990330     c                   if        mocdrf > 0
038900990324     C                   move      MOCDRF        DATAiso
039000990324     C                   move      dataiso       dataeur
039100990324     C                   move      DATAeur       DATRIF            8 0
039200990330     c                   else
039300990330     c                   z-add     0             datrif
039400990330     c                   end
039500000000     C*
039600000000     C                   EXCEPT    ORDINI
039700000000     C                   MOVEL     ORDS          PREOR
039800000000     C                   MOVEL     NPCDS         NPCPRE
039900000000     C                   END
040000000000     C*
040100000000     C                   EXSR      CALPRE
040200990330      * reperimento conti da tab 155
040300990330     c                   exsr      tab155
040400000000     C                   EXSR      IVA
040500000000     C                   EXSR      CPART
040600000000     C   OF              EXCEPT    SEGUE
040700000000     C   OF              EXCEPT    TESTA
040800990401     C                   Z-ADD     MOCPRZ        PRZUNI           14 5
040900000000     C                   MOVEL     MOCDMT        DESMAT           20
041000000000     C                   EXCEPT    DETAIL
041100000000     C                   EXSR      AGGORD
041200000000     C                   GOTO      SU
041300000000     C     FINE          TAG
041400000000     C                   XFOOT     IIV           FINIIV           15 0
041500000000     C     FINIIV        IFGT      0
041600000000     C                   EXSR      STAORP
041700000000     C                   EXSR      STACLP
041800000000     C                   EXSR      TOTALE
041900000000     C                   END
042000000000     C** STAMPO TOTALI ULTIMA FATTURA SOLO SE ESISTE IMPONIBILE
042100090127     C*                  EXSR      AGGDTA
042200090127     c*                  exsr      primanota
042300990407     c                   movel(p)  param2        kpjbu
042400000000     C                   SETON                                        LR
042500000000     C******************************************************
042600000000     C     CARCC         BEGSR
042700000000     C**----------------------------------------
042800000000     C** CARICAMENTO DS MULTIPLA  CONTI CONTABILITA
042900000000     C**----------------------------------------
043000000000     C** PULIZIA DS
043100000000     C                   Z-ADD     0             A
043200000000     C                   DO        30            A
043300990325     C     A             OCCUR     dscp
043400990325     C                   clear                   dscp
043500000000     C                   END
043600000000     C** ACCESSO TABEL X CODICE
043700000000     C                   MOVEL     'CP'          COD1
043800000000     C     KTAB          KLIST
043900000000     C                   KFLD                    CODUT
044000000000     C                   KFLD                    COD1              2
044100000000     C*
044200000000     C     KTAB          SETLL     TABEL                                  07
044300000000     C  N07              SETON                                          H7
044400000000     C   H7              GOTO      FINE
044500000000     C                   Z-ADD     0             K
044600000000     C                   Z-ADD     0             A
044700000000     C                   DO        *HIVAL        A
044800000000     C     KTAB          READE     TABEL                                  07
044900000000     C   07              GOTO      ENDCC
045000000000     C     TBLKEY        IFNE      *BLANKS
045100000000     C                   MOVE      TBLKEY        K
045200990325     C     K             OCCUR     dscp
045300990325     C                   MOVEL     TBLUNI        dscp
045400000000     C                   END
045500000000     C                   END
045600000000     C     ENDCC         TAG
045700000000     C                   ENDSR
045800000000     C******************************************************
045900000000     C     CARVIS        BEGSR
046000000000     C**----------------------------------------
046100000000     C** CARICAMENTO TIPI ORDINI CON FLAG3 = N
046200000000     C**----------------------------------------
046300000000     C                   MOVEL     '162'         GRU1
046400000000     C     KEYTOR        SETLL     ANTAB000                               08
046500000000     C  N08              GOTO      ENDVIS
046600000000     C                   DO        *HIVAL
046700000000     C     KEYTOR        READE     ANTAB000                               09
046800000000     C   09              GOTO      ENDVIS
046900000000     C                   MOVEL     TABDES        DSTAB
047000000000     C     TABTOR        IFEQ      'N'
047100000000     C                   ADD       1             K                 3 0
047200000000     C                   MOVE      TABCOD        TOR(K)
047300000000     C                   END
047400000000     C                   END
047500000000     C     ENDVIS        ENDSR
047600000000     C******************************************************
047700000000     C     STAORP        BEGSR
047800000000     C**---------------------------
047900000000     C*STAMPA PROTOCOLLO ORDINE
048000000000     C**---------------------------
048100000000     C     PRENPC        IFGT      0
048200000000     C                   XFOOT     IIV           COMIIV           15 0
048300000000     C     COMIIV        CABEQ     0             NOSTA
048400990330     c                   if        predpc > 0
048500990324     C                   move      PREDPC        DATAIso
048600990324     C                   move      dataiso       dataeur
048700990324     C                   move      DATAeur       DATPRO            8 0
048800990330     c                   else
048900990330     c                   z-add     0             datpro
049000990330     c                   end
049100000000     C                   EXCEPT    PROORD
049200000000     C                   ELSE
049300000000     C                   ADD       1             CONORD            3 0
049400000000     C                   END
049500000000     C     NOSTA         ENDSR
049600000000     C******************************************************
049700000000     C     STACLP        BEGSR
049800000000     C**---------------------------
049900000000     C*STAMPA PROTOCOLLO CLIENTE
050000000000     C**---------------------------
050100000000     C     CONORD        IFGT      0
050200000000     C     CONIES        IFGT      0
050300990326     c                   move      fatdat        dataiso
050400990326     c     kandic        setll     andic01l
050500990326     c                   do        *hival
050600990326     c     kandic        reade     andic01l                               88
050700990326     c   88              leave
050800990326     c                   if        dataiso >= dicdtindic and
050900990326     c                             dicdtrevoc > *loval  and
051000990326     c                             dataiso <= dicdtrevoc or
051100990326     c                             dataiso >= dicdtindic and
051200990326     c                             dicdtrevoc = *loval  and
051300990326     c                             dataiso <= dicdtfidic
051400990326     c                   move      dicdtcli      dataiso
051500990326     C                   move      dataiso       dataeur
051600990326     C                   move      DATAeur       DATCLI            8 0
051700000000     C                   EXCEPT    PROFAT
051800000000     C                   Z-ADD     0             CONORD
051900990329     C                   END
052000000000     C                   END
052100000000     C                   END
052200000000     C                   END
052300000000     C                   ENDSR
052400000000     C******************************************************
052500000000     C     CONTR         BEGSR
052600000000     C**---------------------------
052700000000     C*CONTROLLO SCHIERA IMPONIBILI
052800000000     C**---------------------------
052900990325     C                   XFOOT     IIV           IMPCOM           15 5
053000000000     C                   ENDSR
053100000000     C******************************************************
053200000000     C     AZZER         BEGSR
053300000000     C**-----------------------------------
053400000000     C*AZZERAMENTI DI TUTTI I CAMPI DI COMODO
053500000000     C**----------------------------------------
053600000000     C                   MOVEL     *BLANKS       AIV
053700000000     C                   Z-ADD     0             IIV
053800000000     C                   Z-ADD     0             IV
053900000000     C                   MOVEL     *BLANKS       CRI
054000000000     C                   MOVEL     *BLANKS       DIV
054100000000     C                   Z-ADD     0             IRI
054200000000     C                   Z-ADD     0             IMPRES
054300000000     C                   Z-ADD     0             IMPIMB
054400000000     C                   Z-ADD     0             IMPBOL
054500000000     C                   Z-ADD     0             IMPESE
054600000000     C                   Z-ADD     0             TOTBOL
054700000000     C                   Z-ADD     0             TOTGEN
054800000000     C                   Z-ADD     0             TOTIV
054900000000     C                   Z-ADD     0             TOTIIV
055000000000     C                   Z-ADD     0             NETTO
055100000000     C                   Z-ADD     0             COMIIV
055200000000     C                   Z-ADD     0             PERIVA
055300000000     C                   Z-ADD     0             TOTIMB
055400000000     C                   Z-ADD     0             TOTRES
055500000000     C                   Z-ADD     0             CONTBL
055600000000     C                   ENDSR
055700000000     C******************************************************
055800000000     C     CONCLI        BEGSR
055900000000     C**-----------------------------------
056000000000     C*ESCLUSIONE DELLE RIGHE CHE NON HANNO ANAGRIFICA E
056100000000     C*CODICE FATTURA MAGGIORE DI QUELLO RICHIESTO A VIDEO
056200000000     C**----------------------------------
056300000000     C                   SETOFF                                       1603
056400990324     C     MOCCLI        CHAIN     tmCLP000                           03
056500000000     C   03              DO
056600000000     C                   EXSR      ERRORE
056700990324     C     MOCCLI        SETGT     tmMOC000
056800000000     C                   GOTO      ENDCLI
056900000000     C                   END
057000000000     C     CLPCDF        IFGT      VIDTIP
057100990324     C     MOCCLI        SETGT     tmMOC000
057200000000     C                   SETON                                        16
057300000000     C                   END
057400000000     C     ENDCLI        TAG
057500000000     C                   ENDSR
057600000000     C******************************************************
057700000000     C     RAGSOC        BEGSR
057800000000     C**----------------------------------------
057900000000     C*RICERCA DI TUTTI I DATI DEL CLIENTE
058000000000     C*ESCLUSIONE DELLA RIGA SE MANCA ANAGRAFICA
058100000000     C**----------------------------------------
058200000000     C                   SETOFF                                       222015
058300000000     C                   MOVEL     MOCCLI        KSC
058400990324     C     KEYcln        CHAIN     ancln01l                           20
058500990324     c                   if        clndtfirpc > *loval
058600990324     c                   seton                                        20
058700990324     c                   endif
058800990324     C  n20clnsogg       CHAIN     ansog01l                           20
058900990324     C  N20Keyind        CHAIN     anind01l                           20
059000990329     C  N20Keybac        CHAIN     ybacl01l                           20
059100000000     C   20              DO
059200000000     C                   EXSR      ERRORE
059300990324     C     MOCCLI        SETGT     tmMOC000
059400000000     C                   GOTO      ENDRAG
059500000000     C                   END
059600030228
059700030228     ** Converto le spese incasso da 3 a 2 decimali.
059800030228     C                   EVAL(H)   WrkSIn = AclSIn / 1000
059900030228
060000990324     C                   MOVEL     *BLANKS       RAG1             22
060100990324     C                   MOVEL     *BLANKS       RAG2             22
060200000000     C                   MOVEL     *BLANKS       VIA
060300990329     C                   move      *blank        CAP
060400000000     C                   MOVEL     *BLANKS       CIT
060500000000     C                   MOVEL     *BLANKS       PRV
060600990329     C                   MOVEL     *BLANKS       BNA              30
060700990324     C                   MOVEL     sogdes        RAG1
060800990324     C                   MOVE      sogdes        RAG2
060900990324     C                   MOVEL     INDindriz     VIA              30
061000990324     C                   movel     INDCAP        CAP               5
061100990324     C                   MOVEL     INDlocalit    CIT              25
061200990324     C                   MOVEL     INDPRov       PRV               2
061300990324     C                   MOVEL     sogpartiva    PIVA             16
061400990329     C*                  MOVEL     INDBNA        BNA
061500000000     C*
061600000000     C                   MOVEL     'FA'          COD
061700990401     C                   MOVEL     MOCCPG        CPG               4
061800990401     C                   MOVE      indlingua     CPG
061900000000     C                   MOVEL     *BLANKS       KEY
062000000000     C                   MOVEL     CPG           KEY
062100000000     C     KEYTAB        CHAIN     TABEL                              22
062200000000     C   22              DO
062300000000     C                   EXSR      ERRORE
062400990324     C     MOCCLI        SETGT     tmMOC000
062500000000     C                   GOTO      ENDRAG
062600000000     C                   END
062700000000     C  N22              MOVEL     TBLUNI        PAGDS
062800000000     C     TIPCDP        COMP      '1'                                    15
062900891227     C     TIPCDP        COMP      '8'                                    51
063000000000     C                   ADD       1             FATNUM
063100000000     C     ENDRAG        ENDSR
063200000000     C******************************************************
063300000000     C     IVA           BEGSR
063400000000     C**------------------------------------------------
063500000000     C*CARICAMENTO IMPONIBILI DELLE RIGHE PER ALIQUOTA O
063600000000     C*ESENZIONE IVA
063700000000     C**------------------------------------------------
063800110914     c*******            movel     ese155v       mociva
063900110914     c*******            move      ali155v       mociva            8
064000111011     c******* per eseguire le sommatorie totali generali occorre MOCIVA carico
064100111011     c******* gli attribuisco (1) per farlo funzionare
064200110914     c                   clear                   mociva            8
064300111011     c                   movel     '   00100'    mociva
064400000000     C                   DO        10            A                 3 0
064500990330     C     AIV(A)        IFEQ      mOCIVA
064600990330     C                   ADD       PREIMP        IIV(A)
064700990330     C                   GOTO      FUORI
064800990330     C                   ELSE
064900990330     C     AIV(A)        IFEQ      *BLANKS
065000990330     C                   MOVEL     MOCIVA        AIV(A)
065100990330     C                   Z-ADD     PREIMP        IIV(A)
065200990330     C                   GOTO      FUORI
065300990330     C                   END
065400990330     C                   END
065500000000     C                   END
065600000000     C     FUORI         TAG
065700000000     C                   MOVEL     AIV(A)        IVDS
065800110914     C**** ese155v       COMP      '   '                                  19
065900110914     C                   seton                                            19
066000000000     C  N19              ADD       1             CONIES            3 0
066100000000     C                   ENDSR
066200000000     C******************************************************
066300000000     C     CPART         BEGSR
066400000000     C**-----------------------------------
066500000000     C*CARICAMENTO IMPONIBILI DELLE RIGHE PER CONTROPARTITA
066600000000     C*E CENTRO DI COSTO
066700000000     C*IL CAMPO "CONTOC" CONTIENE I SEGUENTI CAMPI:
066800000000     C*C/C CONTROPARTITA/S/C CONTROPARTITA/CENTRO DI COSTO
066900000000     C**----------------------------------
067000000000     C                   Z-ADD     0             A
067100000000     C                   DO        100           A                 3 0
067200110914     c                   clear                   contoven         14
067300110914     c*******            movel     kcc155v       contoven         14
067400110914     c*******            move      ksc155v       contoven
067500990401     C                   MOVEL     CONTOven      CONTOC22         22
067600990401     C                   MOVE      MOCCDC        CONTOC22
067700990401     C                   MOVEl     CONTOC22      contoc           30
067800110914     C*******            MOVE      voc155v       CONTOC
067900000000     C*--------------------
068000000000     C     CRI(A)        IFEQ      CONTOC
068100000000     C                   ADD       PREIMP        IRI(A)
068200000000     C                   GOTO      GIU
068300000000     C                   ELSE
068400000000     C     CRI(A)        IFEQ      *BLANKS
068500000000     C                   MOVEL     CONTOC        CRI(A)
068600000000     C                   Z-ADD     PREIMP        IRI(A)
068700000000     C                   GOTO      GIU
068800000000     C                   END
068900000000     C                   END
069000000000     C                   END
069100000000     C     GIU           TAG
069200000000     C                   ENDSR
069300000000     C******************************************************
069400000000     C     CALPRE        BEGSR
069500000000     C**-----------------------------------
069600000000     C*CALCOLO IL PREZZO SCONTATO DELLA QUANTITA' ORDINATA
069700000000     C*E ARROTONDO ALLA LIRA SUPERIORE
069800000000     C**----------------------------------
069900000000     C                   Z-ADD     0             PREIMP
070000990324     C                   Z-ADD     0             PRENET           14 5
070100000000     C                   Z-ADD     MOCSC1        SCONT1            2 0
070200000000     C                   Z-ADD     MOCSC2        SCONT2            2 0
070300000000     C                   Z-ADD     MOCSC3        SCONT3            2 0
070400000000     C                   Z-ADD     MOCPRZ        PRENET
070500990324     C     PRENET        MULT      MOCSC1        SCONTO           14 5
070600000000     C     SCONTO        DIV       100           SCONTO
070700000000     C     PRENET        SUB       SCONTO        PRENET
070800000000     C     PRENET        MULT      MOCSC2        SCONTO
070900000000     C     SCONTO        DIV       100           SCONTO
071000000000     C     PRENET        SUB       SCONTO        PRENET
071100000000     C     PRENET        MULT      MOCSC3        SCONTO
071200000000     C     SCONTO        DIV       100           SCONTO
071300000000     C     PRENET        SUB       SCONTO        PRENET
071400990324     C     MOCQTA        MULT      PRENET        PREIMP           14 5
071500000000     C                   ENDSR
071600000000     C******************************************************
071700000000     C     AGGORD        BEGSR
071800000000     C**-----------------------------------
071900000000     C* AGGIORNAMENTO RIGHE DA 'BB' A 'FA'
072000000000     C**----------------------------------
072100000000     C                   MOVEL     'FA'          MOCFAS
072200000000     C                   Z-ADD     DATFAT        MOCDFT
072300090127     C*                  Z-ADD     FATNUM        MOCNFT
072400090127     C                   Z-ADD     999999999     MOCNFT
072500990324     C                   UPDATE    tmMOC000
072600000000     C                   ENDSR
072700000000     C******************************************************
072800000000     C     TOTALE        BEGSR
072900000000     C**-----------------------------------
073000000000     C*CALCOLO IVA ARROTONDAMENTO SEMPRE IN ECCESSO
073100000000     C*MEMORIZZO LA DESCRIZIONE IVA SE E' ESENTE NEI
073200000000     C*PIANO DEI CONTI
073300000000     C**----------------------------------
073400000000     C                   Z-ADD     0             NETTO
073500000000     C                   Z-ADD     0             PERIVA
073600000000     C                   Z-ADD     0             SCCAS
073700000000     C                   SETOFF                                         28
073800000000     C*------------
073900990324     C                   XFOOT     IIV           NETTO            14 5
074000000000     C                   EXSR      VARIE
074100000000     C                   Z-ADD     0             K
074200000000     C*------------
074300000000     C*CALCOLO SCONTO
074400990324     C                   Z-ADD     0             TOTCAS           17 5
074500990329     C     clnscopag1    IFGT      0
074600000000     C                   SETON                                        28
074700000000     C                   DO        10            K
074800000000     C     IIV(K)        IFGT      0
074900990329     C     IIV(K)        MULT      clnscopag1    SCCAS            17 5
075000000000     C     SCCAS         DIV       100           SCCAS
075100990325     C                   Z-ADD     SCCAS         CAS              17 5
075200000000     C     IIV(K)        SUB       CAS           IIV(K)
075300000000     C                   ADD       CAS           TOTCAS
075400000000     C                   END
075500000000     C                   END
075600000000     C                   SUB       TOTCAS        IRI(1)
075700000000     C     TOTCAS        MULT      -1            TOTCAS
075800000000     C                   END
075900000000     C*CALCOLO IVA
076000000000     C                   DO        10            K
076100000000     C     AIV(K)        IFNE      *BLANKS
076200000000     C                   MOVEL     AIV(K)        IVDS
076300000000     C*
076400990331     C     IVaese        IFEQ      '   '
076500990331     C     IIV(K)        MULT      ivaalq        PERIVA           17 5
076600000000     C     PERIVA        IFGT      0
076700030204     C     PERIVA        DIV       100           IV(k)
076800030204     C*    PERIVA        DIV       100           IVnodec          10 0
076900030204     C*                  z-add     ivnodec       IV(K)
077000000000     C                   END
077100000000     C                   END
077200000000     C                   END
077300000000     C                   END
077400000000     C                   EXSR      STATOT
077500000000     C                   ENDSR
077600000000     C******************************************************
077700000000     C     VARIE         BEGSR
077800000000     C**----------------------------------------
077900000000     C** MEMORIZZAZIONE DELLE SPESE VARIE AGGIUNTIVE
078000000000     C** NELLE SCHIERE DELL'IVA E DELLE CONTROPARTITE
078100000000     C** CAMPO "CAMIVA" CONTIENE C/C IVA E S/CONTO IVA
078200000000     C**----------------------------------------
078300000000     C                   Z-ADD     0             A
078400000000     C                   DO        10            A
078500000000     C     AIV(A)        IFNE      *BLANKS
078600000000     C                   MOVEL     AIV(A)        IVDS
078700990331     C     ivaese        COMP      '   '                                  27
078800000000     C                   END
078900000000     C                   END
079000000000     C*SPESE INCASSO
079100030228     C     WrkSIn        IFGT      0
079200000000     C                   Z-ADD     0             A
079300000000     C  N27              DO        10            A
079400030228     C                   ADD       WrkSIn        IIV(A)
079500000000     C                   GOTO      VIAXX
079600000000     C                   END
079700000000     C                   MOVEL     *BLANKS       CAMIVA
079800990325     C     3             OCCUR     dscp
079900990331     C                   MOVEL     §cpese        CAMIVA            8
080000990331     C                   MOVE      §cpali        CAMIVA
080100000000     C   27              DO        10            A
080200000000     C     AIV(A)        IFEQ      CAMIVA
080300030228     C                   ADD       WrkSIn        IIV(A)
080400000000     C                   GOTO      VIAXX
080500000000     C                   ELSE
080600000000     C     AIV(A)        IFEQ      *BLANKS
080700000000     C                   MOVEL     CAMIVA        AIV(A)
080800030228     C                   Z-ADD     WrkSIn        IIV(A)
080900000000     C                   GOTO      VIAXX
081000000000     C                   END
081100000000     C                   END
081200000000     C                   END
081300000000     C     VIAXX         TAG
081400000000     C                   Z-ADD     0             A
081500000000     C  N27              DO        100           A
081600030228     C                   ADD       WrkSIn        IRI(A)
081700000000     C                   GOTO      LA
081800000000     C                   END
081900990401     C                   MOVE      *BLANKS       CPADS
082000990401     C                   MOVE      §cpkcc        CPACC
082100990329     C                   MOVE      §cpksc        CPASC
082200990401     C                   MOVE      §cpcdc        CPACDC
082300990401     C                   MOVE      §cpvoc        CPAvoc
082400000000     C   27              DO        100           A
082500000000     C     CRI(A)        IFEQ      CPADS
082600030228     C                   ADD       WrkSIn        IRI(A)
082700000000     C                   GOTO      LA
082800000000     C                   ELSE
082900000000     C     CRI(A)        IFEQ      *BLANKS
083000000000     C                   MOVEL     CPADS         CRI(A)
083100030228     C                   Z-ADD     WrkSIn        IRI(A)
083200000000     C                   GOTO      LA
083300000000     C                   END
083400000000     C                   END
083500000000     C                   END
083600000000     C                   END
083700000000     C     LA            TAG
083800000000     C*-----------------
083900000000     C*SPESE IMBALLO
084000000000     C     PREIMB        IFNE      *BLANKS
084100000000     C                   MOVEL     *BLANKS       CAMIVA
084200000000     C                   MOVEL     '008'         GRU
084300000000     C                   MOVEL     *ZEROS        KEYT
084400000000     C                   MOVE      PREIMB        KEYT
084500000000     C     KEYANT        CHAIN     ANTAB000                           17
084600000000     C  N17              MOVEL     TABDES        IMBDS
084700000000     C     IMPIMB        IFGT      0
084800990324     C     IMPIMB        MULT      CONTBL        TOTIMB           15 5
084900000000     C                   Z-ADD     0             A
085000000000     C  N27              DO        10            A
085100000000     C                   ADD       TOTIMB        IIV(A)
085200000000     C                   GOTO      STA
085300000000     C                   END
085400990325     C     1             OCCUR     dscp
085500990331     C                   MOVEL     §cpese        CAMIVA
085600990331     C                   MOVE      §cpali        CAMIVA
085700000000     C   27              DO        10            A
085800000000     C     AIV(A)        IFEQ      CAMIVA
085900000000     C                   ADD       TOTIMB        IIV(A)
086000000000     C                   GOTO      STA
086100000000     C                   ELSE
086200000000     C     AIV(A)        IFEQ      *BLANKS
086300000000     C                   MOVEL     CAMIVA        AIV(A)
086400000000     C                   Z-ADD     TOTIMB        IIV(A)
086500000000     C                   GOTO      STA
086600000000     C                   END
086700000000     C                   END
086800000000     C                   END
086900000000     C     STA           TAG
087000000000     C                   Z-ADD     0             A
087100000000     C  N27              DO        100           A
087200000000     C                   ADD       TOTIMB        IRI(A)
087300000000     C                   GOTO      STA1
087400000000     C                   END
087500000000     C                   MOVEL     *BLANKS       CPADS
087600990329     C                   MOVEL     §cpkcc        CPACC
087700990329     C                   MOVE      §cpksc        CPASC
087800990329     C                   MOVEL     §cpcdc        CPACDC
087900990401     C                   MOVEL     §cpvoc        CPAvoc
088000000000     C   27              DO        100           A
088100000000     C     CRI(A)        IFEQ      CPADS
088200000000     C                   ADD       TOTIMB        IRI(A)
088300000000     C                   GOTO      STA1
088400000000     C                   ELSE
088500000000     C     CRI(A)        IFEQ      *BLANKS
088600000000     C                   MOVEL     CPADS         CRI(A)
088700000000     C                   Z-ADD     TOTIMB        IRI(A)
088800000000     C                   GOTO      STA1
088900000000     C                   END
089000000000     C                   END
089100000000     C                   END
089200000000     C                   END
089300000000     C     STA1          TAG
089400000000     C                   END
089500000000     C*--------------
089600000000     C*SPESE DI RESA O DI TRASPORTO
089700000000     C     PRERES        IFNE      *BLANKS
089800000000     C                   MOVEL     *BLANKS       CAMIVA
089900000000     C                   MOVEL     '019'         GRU
090000000000     C                   MOVEL     *ZEROS        KEYT
090100000000     C                   MOVE      PRERES        KEYT
090200000000     C     KEYANT        CHAIN     ANTAB000                           17
090300000000     C  N17              MOVEL     TABDES        RESDS
090400000000     C     IMPRES        IFGT      0
090500990324     C     IMPRES        MULT      CONTBL        TOTRES           15 5
090600000000     C                   Z-ADD     0             A
090700000000     C  N27              DO        10            A
090800000000     C                   ADD       TOTRES        IIV(A)
090900000000     C                   GOTO      STA2
091000000000     C                   END
091100990325     C     2             OCCUR     dscp
091200990331     C                   MOVEL     §cpese        CAMIVA
091300990331     C                   MOVE      §cpali        CAMIVA
091400000000     C   27              DO        10            A
091500000000     C     AIV(A)        IFEQ      CAMIVA
091600000000     C                   ADD       TOTRES        IIV(A)
091700000000     C                   GOTO      STA2
091800000000     C                   ELSE
091900000000     C     AIV(A)        IFEQ      *BLANKS
092000000000     C                   MOVEL     CAMIVA        AIV(A)
092100000000     C                   Z-ADD     TOTRES        IIV(A)
092200000000     C                   GOTO      STA2
092300000000     C                   END
092400000000     C                   END
092500000000     C                   END
092600000000     C     STA2          TAG
092700000000     C                   Z-ADD     0             A
092800000000     C  N27              DO        100           A
092900000000     C                   ADD       TOTRES        IRI(A)
093000000000     C                   GOTO      STA3
093100000000     C                   END
093200000000     C                   MOVEL     *BLANKS       CPADS
093300990329     C                   MOVEL     §cpkcc        CPACC
093400990329     C                   MOVE      §cpksc        CPASC
093500990329     C                   MOVEL     §cpcdc        CPACDC
093600990401     C                   MOVEL     §cpvoc        CPAvoc
093700000000     C   27              DO        100           A
093800000000     C     CRI(A)        IFEQ      CPADS
093900000000     C                   ADD       TOTRES        IRI(A)
094000000000     C                   GOTO      STA3
094100000000     C                   ELSE
094200000000     C     CRI(A)        IFEQ      *BLANKS
094300000000     C                   MOVEL     CPADS         CRI(A)
094400000000     C                   Z-ADD     TOTRES        IRI(A)
094500000000     C                   GOTO      STA3
094600000000     C                   END
094700000000     C                   END
094800000000     C                   END
094900000000     C                   END
095000000000     C     STA3          TAG
095100000000     C                   END
095200000000     C*CALCOLO IMPONIBILE DEGLI IMPORTI ESENTI
095300000000     C                   Z-ADD     0             K
095400990324     C                   Z-ADD     0             IMPESE           17 5
095500000000     C                   DO        10            K
095600000000     C     AIV(K)        IFNE      *BLANKS
095700000000     C                   MOVEL     AIV(K)        IVDS
095800990331     C     ivaese        IFne      '   '
095900000000     C                   ADD       IIV(K)        IMPESE
096000000000     C                   END
096100000000     C                   END
096200000000     C                   END
096300000000     C*CALCOLO BOLLO SULL'IMPONIBILE DEGLI IMPORTI ESENTI
096400000000     C                   Z-ADD     0             IMPBOL
096500000000     C     IMPESE        CABEQ     0             STAR
096600000211     C     IMPESE        IFGE      §geIMB
096700000211     C                   Z-ADD     §geIBL        IMPBOL           17 5
096800000000     C                   END
096900000000     C*BOLLO ESENTE
097000000000     C     IMPBOL        IFGT      0
097100000000     C                   Z-ADD     0             A
097200990325     C     4             OCCUR     dscp
097300000000     C                   MOVEL     *BLANKS       CPADS
097400990329     C                   MOVEL     §cpkcc        CPACC
097500990329     C                   MOVE      §cpksc        CPASC
097600990329     C                   MOVEL     §cpcdc        CPACDC
097700990401     C                   MOVEL     §cpvoc        CPAvoc
097800000000     C                   DO        100           A
097900000000     C     CRI(A)        IFEQ      CPADS
098000000000     C                   ADD       IMPBOL        IRI(A)
098100000000     C                   GOTO      STAR
098200000000     C                   ELSE
098300000000     C     CRI(A)        IFEQ      *BLANKS
098400000000     C                   MOVEL     CPADS         CRI(A)
098500000000     C                   Z-ADD     IMPBOL        IRI(A)
098600000000     C                   GOTO      STAR
098700000000     C                   END
098800000000     C                   END
098900000000     C                   END
099000000000     C                   END
099100000000     C     STAR          TAG
099200000000     C** PER IL BOLLO MEMORIZZO IN QUESTO MOMENTO SOLO
099300000000     C** L'IMPORTO NELLA SCHIERA DELLE CONTROPARTITTE
099400000000     C                   ENDSR
099500000000     C******************************************************
099600000000     C     STATOT        BEGSR
099700000000     C**-----------------------------------
099800000000     C** CALCOLO E STAMPA TOTALE FATTURA
099900000000     C**----------------------------------
100000000000     C   15              DO
100100990329     C     clnspeboll    IFEQ      'S'
100200000000     C** CALCOLO BOLLO SU R.B.
100300000000     C                   END
100400000000     C                   END
100500990324     C                   Z-ADD     0             TOTBOL           17 5
100600990329     C*    IMPBOL        ADD       IMPBRB        TOTBOL
100700990329     C                   ADD       IMPBol        TOTBOL
100800000000     C** TOTALE BOLLI R.B. + ESENZIONE IVA
100900000000     C   28              EXCEPT    STAMSC
101000000000     C                   EXCEPT    FONDOX
101100990326     C                   XFOOT     IV            TOTIV            15 5
101200990326     C                   XFOOT     IIV           TOTIIV           15 5
101300990324     C     TOTIV         ADD       TOTIIV        TOTGEN           17 5
101400000000     C                   ADD       TOTBOL        TOTGEN
101500000000     C** TOTALE FATTURA
101600000000     C                   Z-ADD     0             K
101700000000     C                   Z-ADD     0             K2
101800000000     C                   DO        10            K
101900000000     C                   SETOFF                                       50
102000000000     C     AIV(K)        IFNE      *BLANKS
102100000000     C                   MOVEL     AIV(K)        IVDS
102200990331     C     IVaese        COMP      '   '                              5050
102300000000     C     K             IFEQ      5
102400000000     C                   EXCEPT    TESTA
102500000000     C                   END
102600000000     C     K             IFEQ      9
102700000000     C                   EXCEPT    TESTA
102800000000     C                   END
102900990602      *arrotondamento per divisa ITL o EUR imponibile/imposta
103000990602     c                   z-add     iiv(k)        decds
103100990602     c                   exsr      arrot
103200990602     c                   z-add     decds         iivs             11 2
103300990602      *
103400990602     c                   z-add     iv(k)         decds
103500990602     c                   exsr      arrot
103600990602     c                   z-add     decds         ivs              11 2
103700990602      *
103800000000     C                   EXCEPT    FONDO
103900000000     C                   ADD       1             K2                2 0
104000000000     C                   END
104100000000     C                   END
104200990602      *arrotondamento per divisa ITL o EUR totale /bolli
104300990602     c                   z-add     totgen        decds
104400990602     c                   exsr      arrot
104500990602     c                   z-add     decds         totgens          11 2
104600990602     c                   z-add     totbol        decds
104700990602     c                   exsr      arrot
104800990602     c                   z-add     decds         totbols          11 2
104900090127     C*                  EXCEPT    TGEN
105000000000     C     TOTBOL        IFGT      0
105100000000     C     K2            IFEQ      4
105200000000     C                   EXCEPT    TBOL1
105300000000     C                   GOTO      LT4
105400000000     C                   END
105500000000     C     K2            IFEQ      8
105600000000     C                   EXCEPT    TBOL1
105700000000     C                   GOTO      LT4
105800000000     C                   END
105900090127     C*                  EXCEPT    TBOL
106000000000     C     LT4           TAG
106100000000     C                   END
106200000000     C*
106300000000     C** SE CI SONO BOLLI STAMPO ESENZIONE
106400000000     C     TOTGEN        IFGT      0
106500090127     C*                  EXSR      CREPN
106600000000     C                   END
106700000000     C                   ENDSR
106800000000     C******************************************************
106900000000     C*STAMPA DEGLI ERRORI
107000000000     C     ERRORE        BEGSR
107100000000     C  N88              OPEN      OCPRTSTD
107200000000     C  N88              SETON                                        88
107300000000     C     CONERR        IFGE      60
107400000000     C                   Z-ADD     0             PAGE1
107500000000     C                   EXCEPT    ERR
107600000000     C                   Z-ADD     1             CONERR
107700000000     C                   END
107800000000     C                   EXCEPT    ERR2
107900000000     C                   ADD       1             CONERR
108000000000     C                   ENDSR
108100900207     C************************************************************
108200900207     C*AGGIORNAMENTO DATA AREA  SE HA AGG.TO ALMENO UNA FATTURA
108300900207     C************************************************************
108400000000     C     AGGDTA        BEGSR
108500990329     c                   move      '5'           xnmric
108600990329     C                   MOVEl     §QCRIV        wserie                         REG.IVA VEND.
108700990329     C                   MOVE      §QCFI3        wserie                         LIBRO IVA VEND.
108800990329     c                   move      datfat        xnmdat
108900990329     c                   move      datfat        xnmdag
109000990329     C                   Z-ADD     FATNUM        xnmnum
109100990329     c                   exsr      repnum
109200000000     C                   ENDSR
109300000000     C*----------------------------------------------------*
109400000000     C     CREPN         BEGSR
109500990326     C*----------------------------------------------------*
109600000000     C** INPUTAZIONE CONTABILE
109700990326     C                   Z-ADD     0             riga              9 0
109800990326     C                   Z-ADD     0             rigai             9 0
109900090127     c*                  clear                   ndbhm000
110000990326
110100990407     C                   MOVEL     §QCfgn        bhmcaustes
110200990326     C                   Z-ADD     FATNUM        bhmsubnum
110300990326     c                   movel     knraz         bhmrifint
110400990326     c                   move      wsocieta      bhmsocieta
110500990326     c                   move      'CG'          bhmctb
110600990326     c                   move      'CG'          bhmclasse
110700990326     c                   move      'D'           bhmtpregz
110800990326     c                   add       1             riga
110900990326     c                   z-add     riga          bhmsubrig
111000990326     C                   movel     '2'           bhmserreg
111100990326     C                   move      §qcFI3        bhmserreg
111200990326     C                   MOVEL     rag1          bhmdesbrev
111300010622     c                   move      mocval        bhmdivisa
111400990326     c                   movel     '000151'      bhmkcc
111500990326     C                   MOVEL     PRECLI        bhmksc
111600990401     C                   MOVEl     '046'         bhmunita
111700990326     C                   move      DATFAT        bhmdtreg
111800990326     C                   move      DATFAT        bhmdtdoc
111900990326     C                   move      DATFAT        bhmdtpar
112000990326     C                   Z-ADD     fatnum        bhmnrreg
112100990326     C                   Z-ADD     fatnum        bhmnrdoc
112200990326     C                   Z-ADD     fatnum        bhmnrpar
112300011214     c                   z-add     totgens       bhmimporto
112400011214     c                   z-add     totgens       bhmimportd
112500990326     C                   MOVEL     'D'           bhmsegno
112600990326     c                   movel     cpg           bhmcondpag
112700090127     C*                  write     ndbhm000
112800000000     C** SCRITTURA RECORD GENERALE
112900990324     C** TIPO Rekord *C*
113000020103     c                   clear                   iivw
113100020103     c                   clear                   ivw
113200020103     c                   eval(h)   iivw = iiv
113300020103     c                   eval(h)   ivw  = iv
113400000000     C                   DO        10            A
113500090127     c*                  clear                   ndbhi000
113600000000     C     AIV(A)        IFNE      *BLANKS
113700990326     C                   MOVE      AIV(A)        ivds
113800990331     C                   z-add     ivaalq        bhiali
113900990331     C                   MOVE      ivaese        bhirifiva
114000020103     C                   Z-ADD     IIVw(A)       bhiimpond
114100020103     C                   Z-ADD     IIVw(A)       bhiimpon
114200020103     C                   Z-ADD     IVw(A)        bhiimporto
114300020103     C                   Z-ADD     IVw(A)        bhiimportd
114400000000     C** CLIENTE CODIFICATO
114500990326     c                   movel     knraz         bhirifint
114600990326     c                   z-add     fatnum        bhisubnum
114700990326     c                   movel     wsocieta      bhisocieta
114800990326     c                   add       1             rigai
114900990326     c                   z-add     rigai         bhisubrig
115000990326     c                   move      'A'           bhisegno
115100990326     C                   move      §qcfi3        bhilibro
115200990326     C                   move      '2'           bhitpreg
115300090127     C*                  WRITE     ndbhi000
115400000000     C                   END
115500000000     C                   END
115600000000     C** IVA
115700000000     C     TOTBOL        IFGT      0
115800011214     C                   Z-ADD     TOTBOLs       bhiimpon
115900011214     C                   Z-ADD     TOTBOLs       bhiimpond
116000990326     C                   Z-ADD     0             bhiimporto
116100990326     C                   Z-ADD     0             bhiimportd
116200990326     C                   ADD       1             rigai
116300990326     c                   z-add     rigai         bhisubrig
116400990326     C                   z-add     0             bhiali
116500090127     C*                  WRITE     ndbhi000
116600000000     C                   END
116700000000     C** BOLLI
116800000000     C** SCRITTURA IVA
116900000000     C** UN RECORD OGNI CODICE IVA
117000000000     C** UN RECORD PER BOLLI
117100000000     C**
117200000000     C** TIPO RECORD  *D*
117300000000     C** AZZERAMENTI DS
117400020103     c                   clear                   iriw
117500020103     c                   eval(h)   iriw = iri
117600000000     C                   DO        100           A
117700900206     C     CRI(A)        IFGT      *ZERO
117800900206     C                   MOVEL     CRI(A)        CPADS
117900990326     C                   move      CPACC         bhmkcc
118000990326     C                   move      CPASC         bhmksc
118100990401     C                   movel     CPAvoc        bhmvoce
118200020103     C                   Z-ADD     IRIw(A)       bhmimporto
118300020103     C                   Z-ADD     IRIw(A)       bhmimportd
118400990326     C                   Z-ADD     Fatnum        bhmnrpar
118500990326     C                   move      DATFAT        bhmdtpar
118600990326     C                   add       1             riga
118700990326     c                   z-add     riga          bhmsubrig
118800990326     c                   move      'A'           bhmsegno
118900090127     C*                  WRITE     ndbhm000
119000990326      *
119100990326     c                   move      bhmdtcoan     bhadtcoan
119200990326     c                   move      bhmrifint     bharifint
119300990326     c                   move      bhmsubnum     bhasubnum
119400990326     c                   move      bhmsubrig     bhasubrig
119500990326     c                   move      bhmvoce       bhavoce
119600990326     c                   move      'D1'          bhadimen
119700990406     C                   movel     CPACDC        bhaentit1
119800990326     c                   move      wsocieta      bhasocieta
119900020103     C                   Z-ADD     Iriw(A)       bhaimporto
120000020103     C                   Z-ADD     Iriw(A)       bhaimportd
120100990326     c                   move      'A'           bhasegno
120200990326     c                   add       1             bhasubsubr
120300090127     c*                  write     ndbha000
120400000000     C                   END
120500891227     C                   END
120600000000     C** SCRITTURA RECORD CONTROPARTITE
120700000000     C** UN RECORD PER CONTROPARTITA/CDC -SCHIERA CRI
120800000000     C                   ENDSR
120900951219     C*------------------------------------------------------------------------*
121000951219     C* CARQC -  CARICAMENTO DATI VARI CONTABILITA'
121100951219     C*------------------------------------------------------------------------*
121200951219     C     CARQC         BEGSR
121300951219     C* DATI VARI 1
121400951219     C                   CLEAR                   DSQC1
121500951219     C                   Z-ADD     1             CODUT
121600951219     C                   MOVEL     'QC'          CODTAB
121700951219     C                   MOVEL(P)  '1'           CODCOD
121800951219     C     KTABEL        CHAIN     TABEL00F                           55
121900951219IF  1C     *IN55         IFEQ      *OFF
122000951219     C                   MOVEL     TBLUNI        DSQC1
122100951219E   1C                   ENDIF
122200951219     C* DATI VARI 2
122300951219     C                   CLEAR                   DSQC2
122400951219     C                   MOVEL     'QC'          CODTAB
122500951219     C                   MOVEL(P)  '2'           CODCOD
122600951219     C     KTABEL        CHAIN     TABEL00F                           55
122700951219IF  1C     *IN55         IFEQ      *OFF
122800951219     C                   MOVEL     TBLUNI        DSQC2
122900951219E   1C                   ENDIF
123000951219     C*
123100951219     C                   ENDSR
123200951219     C*------------------------------------------------------------------------*
123300951219     C* CARQT -  CARICAMENTO DATI VARI TASSAZIONE
123400951219     C*------------------------------------------------------------------------*
123500000211     C****     CARQT         BEGSR
123600951219     C* DATI VARI 1
123700000211     C****                   CLEAR                   DSQT1
123800000211     C****                   Z-ADD     1             CODUT
123900000211     C****                   MOVEL     'QT'          CODTAB
124000000211     C****                   MOVEL(P)  '1'           CODCOD
124100000211     C****     KTABEL        CHAIN     TABEL00F                           55
124200000211IF  1C****     *IN55         IFEQ      *OFF
124300000211     C****                   MOVEL     TBLUNI        DSQT1
124400000211E   1C****                   ENDIF
124500951219     C*
124600000211     C****                   ENDSR
124700000211     C****************************************************
124800000211     C** CARICAMENTO IMPORTI STANDARD MONETA
124900000211     C****************************************************
125000000211     C     CARGEI        BEGSR
125100000211      *
125200000211     C                   CLEAR                   DSBS02
125300000211      *
125400000211     C                   MOVEL     'L'           T02TLA
125500000211     C                   MOVEL     'C'           T02MOD
125600000211     C                   MOVEL     KNSIF         T02SIF
125700000211     C                   MOVEL     'GEI'         T02COD
125800000211     C                   MOVEL     XSCDIV        T02KE1
125900000211      *
126000000211     C                   CALL      'TIBS02R'
126100000211     C                   PARM                    KPJBA
126200000211     C                   PARM                    DSBS02
126300000211      *
126400000211     C     T02ERR        IFEQ      *BLANKS
126500000211     C                   MOVEL     T02UNI        DGEI
126600000211     C                   ENDIF
126700000211      *
126800000211     C                   ENDSR
126900990325      *----------------------------------------------------
127000990325     C     primanota     BEGSR
127100990325     C*----------------------------------------------------
127200990325     C*
127300010622      *se la divisa che sto utilizzando per contabilizzare è diversa dalla
127400010622      * moneta di conto richiamo adeguatore per sistemazione importi nella
127500010622      *divisa corretta
127600010622     c                   if        mocval <> xscdiv
127700010622     c                   exsr      adeguatore
127800010622     c                   endif
127900990325     C* contabilizzazione batch
128000990325     c                   reset                   ndc071ds
128100990325     C                   MOVE      wsocieta      SOCieta071
128200990325     C                   MOVE      KNRAZ         rifint071
128300990325     c                   move      *loval        dtregda071
128400990325     c                   move      *hival        dtrega071
128500990325     c                   move      *loval        subnumd071
128600990325     c                   move      *hival        subnuma071
128700000316     C                   MOVE      *off          solcont071
128800990325    >C                   MOVE      *off          interat071
128900990325     C                   move      *off          err071
129000990325     c                   move      *off          control071
129100990325     C                   MOVEL     NDC071DS      kpjbu
129200990325     C*
129300990325     C                   CALL      'NDC071C'
129400990325     C                   parm                    kpjba
129500990325     C                   movel     kpjbu         ndc071ds
129600990325     C*
129700990325     C                   ENDSR
129800010622      *----------------------------------------------------
129900010622     C     adeguatore    BEGSR
130000010622     C*----------------------------------------------------
130100010622     C*
130200010622     C* adeguatore parametri
130300010622     c                   reset                   ndc270ds
130400010622     C                   MOVE      wsocieta      SOCieta270
130500010622     C                   MOVE      KNRAZ         rifint270
130600010622     c                   move      *loval        dtregda270
130700010622     c                   move      *hival        dtrega270
130800010622     c                   move      *loval        subnumd270
130900010622     c                   move      *hival        subnuma270
131000010622     C                   move      *off          err270
131100010622     c                   move      '3'           ricalc270
131200010622     c                   move      '1'           cmt270
131300010622     C                   MOVEL     NDC270DS      kpjbu
131400010622     C*
131500010622     C                   CALL      'NDC270C'
131600010622     C                   parm                    kpjba
131700010622     C                   movel     kpjbu         ndc270ds
131800010622     C*
131900010622     C                   ENDSR
132000990324     C*----------------------------------------------------*
132100990324     C* Reperisco numeratore
132200990324     C*----------------------------------------------------*
132300990324     c     repnum        begsr
132400990324     C                   MOVE      wsocieta      XNMSOC            3
132500990324     C                   MOVE      *BLANKS       XNMUNI            8
132600990324     C                   MOVE      'CG'          XNMCTB            2
132700990324     C                   MOVE      *BLANKS       XNMKEY            8
132800990324     C                   MOVE      WSERIE        XNMSER            4
132900990324     C                   MOVE      *OFF          XNMCMT            1
133000990324     C                   MOVE      *On           XNMIVA            1
133100990324     C                   MOVE      *OFF          XNMREG            1
133200990324     C                   MOVE      '0'           XNMerr            1
133300990324     C*
133400990324     C                   CALLB     'XNMR'        PNMR
133500990324     C*
133600990324     C     xnmerr        ifne      '0'
133700990324     C                   seton                                        h5
133800990324     C                   end
133900990324      *
134000990324     C                   endsr
134100990329     C*----------------------------------------------------*
134200990329     C* tabella 155
134300990329     C*----------------------------------------------------*
134400990329     c     tab155        begsr
134500990330     c                   clear                   tb155
134600990329     c     mocmat        chain     tmamp01l                           66
134700990329     c  n66              do
134800990329     C                   MOVEL     '155'         GRU
134900990329     C                   MOVEL     *ZEROS        KEYT
135000990329     C                   MOVE      ampclm        KEYT
135100990329     C     KEYANT        CHAIN     ANTAB000                           66
135200990329     C  N66              MOVEL     TABDES        tb155
135300990329     c                   enddo
135400990329     C                   endsr
135500990602     c*------------------------------------------------
135600990602     c     arrot         begsr
135700990602     c*------------------------------------------------
135800990602     c                   if        xscdiv = 'ITL' and dec(1) >= 5
135900990602     c                   add       1             int
136000990602     c                   movea     *zeros        dec
136100990602     c                   end
136200990602     c                   if        xscdiv = 'EUR' and dec(3) >= 5
136300990602     c                   add       1             dec(2)
136400990602     c                   movea     *zeros        dec(3)
136500990602     c                   end
136600990602     c                   endsr
136700990602     c*------------------------------------------------
136800990324     C*----------------------------------------------------*
136900990324     C* Reperimento dati società
137000990324     C*----------------------------------------------------*
137100990324     C     REPSOC        BEGSR
137200990324     C*
137300990324     C                   CALLB     'XSOC'
137400990324     C                   PARM                    TIPXSC            6
137500990324     C                   PARM      wsocieta      SOCXSC            3
137600990324     C                   PARM                    CDSXSC            9 0
137700990324     C                   PARM                    MODXSC            3
137800990324     C                   PARM      *blanks       RTNXSC            1
137900990324     C                   PARM                    XSOCDS
138000990324     C                   PARM                    KPJBA
138100990324     C*
138200990324     C                   ENDSR
138300000000     O*--------------------------------------------------------------*
138400000000     O*     TESTATA                                                  *
138500000000     O*--------------------------------------------------------------*
138600000000     O*--------------FATTURE------------------------------------------*
138700000000     OFAWEPPRT  E            TESTA            01
138800990401     O                       SC(1)               80
138900000000     O          E            TESTA            02
139000000000     O                       RSC               +  1
139100990326     O                       IND               +  1
139200000000     O                       LOC               +  1
139300000000     O          E            TESTA            03
139400000000     O                                         +  1 'Codice Fiscale'
139500990326     O                       CFI               +  1
139600000000     O                                         +  5 'Partita IVA'
139700000000     O                       PIVD              +  1
139800000000     O          E            TESTA            04
139900960604     O                                         +  1 'Regis.Imprese'
140000000000     O                       TRI               +  1
140100960604     O                                         +  1 'R.E.A.'
140200990326     O                       COM               +  1
140300910702     O                                         +  1 'Cap.Soc.'
140400990326     O*                      CSO           2   +  1
140500000000     O          E            TESTA            05
140600990401     O                       SC(1)               80
140700000000     O          E            TESTA            08
140800000000     O                       MOCCLI              40
140900000000     O                       RAG1                70
141000000000     O          E            TESTA            09
141100000000     O                       RAG2                70
141200000000     O          E            TESTA            11
141300000000     O                       VIA               + 46
141400000000     O          E            TESTA            12
141500000000     O                       CAP               + 46
141600000000     O                       CIT               +  1
141700000000     O                       PRV               +  1
141800090130     O          E            TESTA            15
141900090130     O                       SC(2)               80
142000090127     O*                      FATNUM        Z     41
142100090127     O*                                          46 'del'
142200090127     O*                      DATAFT              55 '  /  /    '
142300090127     O*         E            TESTA            17
142400090127     O*                      SC(3)               80
142500090127     O*                      MOCCPG              21
142600090127     O*                      DESCPG              52
142700090127     O*         E            TESTA            18
142800090127     O*                      BNA                 34
142900000000     O          E            TESTA            22 23
143000990401     O                       SC(4)               80
143100000000     O*--------------------------------------------------------------*
143200000000     O*     TESTATA DI CAMBIO BOLLA                                  *
143300000000     O*--------------------------------------------------------------*
143400000000     O          E            BOLLA       1
143500000000     O                                           32 'Bolla di Consegna N.'
143600000000     O                       MOCNBL        Z     39
143700000000     O                                           43 'del'
143800990401     O                       DATBOL              54 '  /  /    '
143900000000     O*RIBATTITURA
144000000000     O          E            BOLLA       0  0
144100000000     O                                           32 'Bolla di Consegna N.'
144200000000     O                       MOCNBL        Z     39
144300000000     O                                           43 'del'
144400990401     O                       DATBOL              54 '  /  /    '
144500000000     O*--------------------------------------------------------------*
144600000000     O*     TESTATA DI CAMBIO ORDINE                                 *
144700000000     O*--------------------------------------------------------------*
144800000000     O          E            ORDINI      1
144900990401     O                                           22 'Ns.ordine N.'
145000990401     O                       MOCNOR        Z     27
145100990401     O                                           31 'del'
145200990401     O                       DATORD              42 '  /  /    '
145300990401     O              N18                          58 'Rif.Vs.ordine N.'
145400990401     O              N18      MOCNRF        Z     66
145500990401     O              N18                          70 'del'
145600990326     O              N18      DATRIF              80 '  /  /    '
145700000000     O*--------------------------------------------------------------*
145800000000     O*     DETTAGLIO                                                *
145900000000     O*--------------------------------------------------------------*
146000000000     O          E            DETAIL      1
146100990401     O                       PREIMP              78 '       0 ,     '
146200990401     O                       PRZUNI              60 '       0 ,     '
146300990401     O                       MOCQTA              44 '     0 ,  '
146400890707     O                       DESMAT              33
146500890707     O                       MOCMAT              12
146600000000     O*--------------------------------------------------------------*
146700000000     O*     FONDO                                                    *
146800000000     O*--------------------------------------------------------------*
146900010305     O          E            FONDOX           60
147000990602     O                       SC(5)               80
147100000000     O          E            FONDO       1
147200090127     O*             N50      IVs           3     35
147300090127     O*             N50      IVaalq        2     20
147400990602     O                       IIVs          2     14
147500000000     O*--------------------------------------------------------------*
147600000000     O*     SCONTO CASSA                                             *
147700000000     O*--------------------------------------------------------------*
147800010305     O          E            STAMSC           59
147900000000     O                                           24 'Sconto Cassa'
148000000000     O                       TOTCAS        M     81
148100990329     O                       clnscopag1    3     56
148200000000     O*--------------------------------------------------------------*
148300000000     O*     TOTALI GENERALI                                          *
148400000000     O*--------------------------------------------------------------*
148500000000     O          E            TGEN        0
148600990602     O                       TOTGENs       2     75
148700990602     O                       TOTBOLs       2     59
148800010627     O                       mocval              80
148900000000     O*--------------------------------------------------------------*
149000000000     O          E            TBOL        1
149100000000     O                                           69 'BOLLI: ES.ART.15'
149200000000     O          E            TBOL1       0
149300000000     O                                         +  1 'BOLLI: ES.ART.15'
149400000000     O*--------------------------------------------------------------*
149500000000     O          E            SEGUE            64
149600000000     O                                           83 'SEGUE'
149700000000     O*--------------------------------------------------------------*
149800000000     O*     PROTOCOLLO ORDINE                                        *
149900000000     O          E            PROORD      1
150000000000     O                                           30 '* Vs. Protocollo N.'
150100000000     O                       PRENPC        Z     39
150200000000     O                                           43 'del'
150300990326     O                       DATPRO              52 '  /  /    '
150400000000     O                                           71 'Ns. Protocollo N.'
150500000000     O                       PRENPN        Z     81
150600000000     O*
150700000000     O*--------------------------------------------------------------*
150800000000     O*     PROTOCOLLO  CLIENTE                                      *
150900000000     O*--------------------------------------------------------------*
151000000000     O          E            PROFAT      2
151100890707     O                                           29 '** Vs. Protocollo N.'
151200990326     O                       DICNRCLI      Z     38
151300890707     O                                           42 'del'
151400990326     O                       DATCLI              51 '  /  /    '
151500890707     O                                           70 'Ns. Protocollo N.'
151600990329     O                       dicnrinter    Z     80
151700000000     O*--------------------------------------------------------------*
151800000000     O*     ERRORI                                                   *
151900000000     O*--------------------------------------------------------------*
152000951219     OOCPRTSTD  E            ERR              03
152100000000     O                       RSUT                20
152200000000     O                       UDATE              118 '  /  /  '
152300000000     O                       PAGE1         Z    132
152400000000     O                                           70 'CONTROLLO FATTURAZIONE'
152500000000     O                                           74 'DEL'
152600990326     O                       DATAFT              83 '  /  /    '
152700000000     O          E            ERR2        1
152800000000     O                                           17 'CODICE CLIENTE:'
152900000000     O                       MOCCLI              26
153000000000     O               22                          44 'CODICE PAGAMENTO'
153100000000     O               22      MOCCPG              49
153200000000     O                     03                    75 'ANAGRAFICA MANCANTE'
153300000000     O                     03                    89 'O INESISTENTE'
153400000000     O                     20                    75 'ANAGRAFICA MANCANTE'
153500000000     O                     20                    89 'O INESISTENTE'
153600000000     O                     22                    46 'CONDIZIONI DI PAGAMENTO'
153700000000     O                     22                    91 'INESISTENTI'
153800890707**
153900000000----------------------------------------------------------------------
154000090130    DOCUMENTO AD USO INTERNO
154100000000    Pagamento:
154200990401Materiale----Descrizione-----------Quantita'------Prezzo----------Importo---
154300090127---Imponibile---------------------------------------------------------------
