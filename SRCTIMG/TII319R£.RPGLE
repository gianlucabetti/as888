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
063800990331     c                   movel     ese155v       mociva
063900990331     c                   move      ali155v       mociva            8
064000000000     C                   DO        10            A                 3 0
064100990330     C     AIV(A)        IFEQ      mOCIVA
064200990330     C                   ADD       PREIMP        IIV(A)
064300990330     C                   GOTO      FUORI
064400990330     C                   ELSE
064500990330     C     AIV(A)        IFEQ      *BLANKS
064600990330     C                   MOVEL     MOCIVA        AIV(A)
064700990330     C                   Z-ADD     PREIMP        IIV(A)
064800990330     C                   GOTO      FUORI
064900990330     C                   END
065000990330     C                   END
065100000000     C                   END
065200000000     C     FUORI         TAG
065300000000     C                   MOVEL     AIV(A)        IVDS
065400990331     C     ese155v       COMP      '   '                                  19
065500000000     C  N19              ADD       1             CONIES            3 0
065600000000     C                   ENDSR
065700000000     C******************************************************
065800000000     C     CPART         BEGSR
065900000000     C**-----------------------------------
066000000000     C*CARICAMENTO IMPONIBILI DELLE RIGHE PER CONTROPARTITA
066100000000     C*E CENTRO DI COSTO
066200000000     C*IL CAMPO "CONTOC" CONTIENE I SEGUENTI CAMPI:
066300000000     C*C/C CONTROPARTITA/S/C CONTROPARTITA/CENTRO DI COSTO
066400000000     C**----------------------------------
066500000000     C                   Z-ADD     0             A
066600000000     C                   DO        100           A                 3 0
066700990331     c                   movel     kcc155v       contoven         14
066800990331     c                   move      ksc155v       contoven
066900990401     C                   MOVEL     CONTOven      CONTOC22         22
067000990401     C                   MOVE      MOCCDC        CONTOC22
067100990401     C                   MOVEl     CONTOC22      contoc           30
067200990401     C                   MOVE      voc155v       CONTOC
067300000000     C*--------------------
067400000000     C     CRI(A)        IFEQ      CONTOC
067500000000     C                   ADD       PREIMP        IRI(A)
067600000000     C                   GOTO      GIU
067700000000     C                   ELSE
067800000000     C     CRI(A)        IFEQ      *BLANKS
067900000000     C                   MOVEL     CONTOC        CRI(A)
068000000000     C                   Z-ADD     PREIMP        IRI(A)
068100000000     C                   GOTO      GIU
068200000000     C                   END
068300000000     C                   END
068400000000     C                   END
068500000000     C     GIU           TAG
068600000000     C                   ENDSR
068700000000     C******************************************************
068800000000     C     CALPRE        BEGSR
068900000000     C**-----------------------------------
069000000000     C*CALCOLO IL PREZZO SCONTATO DELLA QUANTITA' ORDINATA
069100000000     C*E ARROTONDO ALLA LIRA SUPERIORE
069200000000     C**----------------------------------
069300000000     C                   Z-ADD     0             PREIMP
069400990324     C                   Z-ADD     0             PRENET           14 5
069500000000     C                   Z-ADD     MOCSC1        SCONT1            2 0
069600000000     C                   Z-ADD     MOCSC2        SCONT2            2 0
069700000000     C                   Z-ADD     MOCSC3        SCONT3            2 0
069800000000     C                   Z-ADD     MOCPRZ        PRENET
069900990324     C     PRENET        MULT      MOCSC1        SCONTO           14 5
070000000000     C     SCONTO        DIV       100           SCONTO
070100000000     C     PRENET        SUB       SCONTO        PRENET
070200000000     C     PRENET        MULT      MOCSC2        SCONTO
070300000000     C     SCONTO        DIV       100           SCONTO
070400000000     C     PRENET        SUB       SCONTO        PRENET
070500000000     C     PRENET        MULT      MOCSC3        SCONTO
070600000000     C     SCONTO        DIV       100           SCONTO
070700000000     C     PRENET        SUB       SCONTO        PRENET
070800990324     C     MOCQTA        MULT      PRENET        PREIMP           14 5
070900000000     C                   ENDSR
071000000000     C******************************************************
071100000000     C     AGGORD        BEGSR
071200000000     C**-----------------------------------
071300000000     C* AGGIORNAMENTO RIGHE DA 'BB' A 'FA'
071400000000     C**----------------------------------
071500000000     C                   MOVEL     'FA'          MOCFAS
071600000000     C                   Z-ADD     DATFAT        MOCDFT
071700090127     C*                  Z-ADD     FATNUM        MOCNFT
071800090127     C                   Z-ADD     999999999     MOCNFT
071900990324     C                   UPDATE    tmMOC000
072000000000     C                   ENDSR
072100000000     C******************************************************
072200000000     C     TOTALE        BEGSR
072300000000     C**-----------------------------------
072400000000     C*CALCOLO IVA ARROTONDAMENTO SEMPRE IN ECCESSO
072500000000     C*MEMORIZZO LA DESCRIZIONE IVA SE E' ESENTE NEI
072600000000     C*PIANO DEI CONTI
072700000000     C**----------------------------------
072800000000     C                   Z-ADD     0             NETTO
072900000000     C                   Z-ADD     0             PERIVA
073000000000     C                   Z-ADD     0             SCCAS
073100000000     C                   SETOFF                                         28
073200000000     C*------------
073300990324     C                   XFOOT     IIV           NETTO            14 5
073400000000     C                   EXSR      VARIE
073500000000     C                   Z-ADD     0             K
073600000000     C*------------
073700000000     C*CALCOLO SCONTO
073800990324     C                   Z-ADD     0             TOTCAS           17 5
073900990329     C     clnscopag1    IFGT      0
074000000000     C                   SETON                                        28
074100000000     C                   DO        10            K
074200000000     C     IIV(K)        IFGT      0
074300990329     C     IIV(K)        MULT      clnscopag1    SCCAS            17 5
074400000000     C     SCCAS         DIV       100           SCCAS
074500990325     C                   Z-ADD     SCCAS         CAS              17 5
074600000000     C     IIV(K)        SUB       CAS           IIV(K)
074700000000     C                   ADD       CAS           TOTCAS
074800000000     C                   END
074900000000     C                   END
075000000000     C                   SUB       TOTCAS        IRI(1)
075100000000     C     TOTCAS        MULT      -1            TOTCAS
075200000000     C                   END
075300000000     C*CALCOLO IVA
075400000000     C                   DO        10            K
075500000000     C     AIV(K)        IFNE      *BLANKS
075600000000     C                   MOVEL     AIV(K)        IVDS
075700000000     C*
075800990331     C     IVaese        IFEQ      '   '
075900990331     C     IIV(K)        MULT      ivaalq        PERIVA           17 5
076000000000     C     PERIVA        IFGT      0
076100030204     C     PERIVA        DIV       100           IV(k)
076200030204     C*    PERIVA        DIV       100           IVnodec          10 0
076300030204     C*                  z-add     ivnodec       IV(K)
076400000000     C                   END
076500000000     C                   END
076600000000     C                   END
076700000000     C                   END
076800000000     C                   EXSR      STATOT
076900000000     C                   ENDSR
077000000000     C******************************************************
077100000000     C     VARIE         BEGSR
077200000000     C**----------------------------------------
077300000000     C** MEMORIZZAZIONE DELLE SPESE VARIE AGGIUNTIVE
077400000000     C** NELLE SCHIERE DELL'IVA E DELLE CONTROPARTITE
077500000000     C** CAMPO "CAMIVA" CONTIENE C/C IVA E S/CONTO IVA
077600000000     C**----------------------------------------
077700000000     C                   Z-ADD     0             A
077800000000     C                   DO        10            A
077900000000     C     AIV(A)        IFNE      *BLANKS
078000000000     C                   MOVEL     AIV(A)        IVDS
078100990331     C     ivaese        COMP      '   '                                  27
078200000000     C                   END
078300000000     C                   END
078400000000     C*SPESE INCASSO
078500030228     C     WrkSIn        IFGT      0
078600000000     C                   Z-ADD     0             A
078700000000     C  N27              DO        10            A
078800030228     C                   ADD       WrkSIn        IIV(A)
078900000000     C                   GOTO      VIAXX
079000000000     C                   END
079100000000     C                   MOVEL     *BLANKS       CAMIVA
079200990325     C     3             OCCUR     dscp
079300990331     C                   MOVEL     §cpese        CAMIVA            8
079400990331     C                   MOVE      §cpali        CAMIVA
079500000000     C   27              DO        10            A
079600000000     C     AIV(A)        IFEQ      CAMIVA
079700030228     C                   ADD       WrkSIn        IIV(A)
079800000000     C                   GOTO      VIAXX
079900000000     C                   ELSE
080000000000     C     AIV(A)        IFEQ      *BLANKS
080100000000     C                   MOVEL     CAMIVA        AIV(A)
080200030228     C                   Z-ADD     WrkSIn        IIV(A)
080300000000     C                   GOTO      VIAXX
080400000000     C                   END
080500000000     C                   END
080600000000     C                   END
080700000000     C     VIAXX         TAG
080800000000     C                   Z-ADD     0             A
080900000000     C  N27              DO        100           A
081000030228     C                   ADD       WrkSIn        IRI(A)
081100000000     C                   GOTO      LA
081200000000     C                   END
081300990401     C                   MOVE      *BLANKS       CPADS
081400990401     C                   MOVE      §cpkcc        CPACC
081500990329     C                   MOVE      §cpksc        CPASC
081600990401     C                   MOVE      §cpcdc        CPACDC
081700990401     C                   MOVE      §cpvoc        CPAvoc
081800000000     C   27              DO        100           A
081900000000     C     CRI(A)        IFEQ      CPADS
082000030228     C                   ADD       WrkSIn        IRI(A)
082100000000     C                   GOTO      LA
082200000000     C                   ELSE
082300000000     C     CRI(A)        IFEQ      *BLANKS
082400000000     C                   MOVEL     CPADS         CRI(A)
082500030228     C                   Z-ADD     WrkSIn        IRI(A)
082600000000     C                   GOTO      LA
082700000000     C                   END
082800000000     C                   END
082900000000     C                   END
083000000000     C                   END
083100000000     C     LA            TAG
083200000000     C*-----------------
083300000000     C*SPESE IMBALLO
083400000000     C     PREIMB        IFNE      *BLANKS
083500000000     C                   MOVEL     *BLANKS       CAMIVA
083600000000     C                   MOVEL     '008'         GRU
083700000000     C                   MOVEL     *ZEROS        KEYT
083800000000     C                   MOVE      PREIMB        KEYT
083900000000     C     KEYANT        CHAIN     ANTAB000                           17
084000000000     C  N17              MOVEL     TABDES        IMBDS
084100000000     C     IMPIMB        IFGT      0
084200990324     C     IMPIMB        MULT      CONTBL        TOTIMB           15 5
084300000000     C                   Z-ADD     0             A
084400000000     C  N27              DO        10            A
084500000000     C                   ADD       TOTIMB        IIV(A)
084600000000     C                   GOTO      STA
084700000000     C                   END
084800990325     C     1             OCCUR     dscp
084900990331     C                   MOVEL     §cpese        CAMIVA
085000990331     C                   MOVE      §cpali        CAMIVA
085100000000     C   27              DO        10            A
085200000000     C     AIV(A)        IFEQ      CAMIVA
085300000000     C                   ADD       TOTIMB        IIV(A)
085400000000     C                   GOTO      STA
085500000000     C                   ELSE
085600000000     C     AIV(A)        IFEQ      *BLANKS
085700000000     C                   MOVEL     CAMIVA        AIV(A)
085800000000     C                   Z-ADD     TOTIMB        IIV(A)
085900000000     C                   GOTO      STA
086000000000     C                   END
086100000000     C                   END
086200000000     C                   END
086300000000     C     STA           TAG
086400000000     C                   Z-ADD     0             A
086500000000     C  N27              DO        100           A
086600000000     C                   ADD       TOTIMB        IRI(A)
086700000000     C                   GOTO      STA1
086800000000     C                   END
086900000000     C                   MOVEL     *BLANKS       CPADS
087000990329     C                   MOVEL     §cpkcc        CPACC
087100990329     C                   MOVE      §cpksc        CPASC
087200990329     C                   MOVEL     §cpcdc        CPACDC
087300990401     C                   MOVEL     §cpvoc        CPAvoc
087400000000     C   27              DO        100           A
087500000000     C     CRI(A)        IFEQ      CPADS
087600000000     C                   ADD       TOTIMB        IRI(A)
087700000000     C                   GOTO      STA1
087800000000     C                   ELSE
087900000000     C     CRI(A)        IFEQ      *BLANKS
088000000000     C                   MOVEL     CPADS         CRI(A)
088100000000     C                   Z-ADD     TOTIMB        IRI(A)
088200000000     C                   GOTO      STA1
088300000000     C                   END
088400000000     C                   END
088500000000     C                   END
088600000000     C                   END
088700000000     C     STA1          TAG
088800000000     C                   END
088900000000     C*--------------
089000000000     C*SPESE DI RESA O DI TRASPORTO
089100000000     C     PRERES        IFNE      *BLANKS
089200000000     C                   MOVEL     *BLANKS       CAMIVA
089300000000     C                   MOVEL     '019'         GRU
089400000000     C                   MOVEL     *ZEROS        KEYT
089500000000     C                   MOVE      PRERES        KEYT
089600000000     C     KEYANT        CHAIN     ANTAB000                           17
089700000000     C  N17              MOVEL     TABDES        RESDS
089800000000     C     IMPRES        IFGT      0
089900990324     C     IMPRES        MULT      CONTBL        TOTRES           15 5
090000000000     C                   Z-ADD     0             A
090100000000     C  N27              DO        10            A
090200000000     C                   ADD       TOTRES        IIV(A)
090300000000     C                   GOTO      STA2
090400000000     C                   END
090500990325     C     2             OCCUR     dscp
090600990331     C                   MOVEL     §cpese        CAMIVA
090700990331     C                   MOVE      §cpali        CAMIVA
090800000000     C   27              DO        10            A
090900000000     C     AIV(A)        IFEQ      CAMIVA
091000000000     C                   ADD       TOTRES        IIV(A)
091100000000     C                   GOTO      STA2
091200000000     C                   ELSE
091300000000     C     AIV(A)        IFEQ      *BLANKS
091400000000     C                   MOVEL     CAMIVA        AIV(A)
091500000000     C                   Z-ADD     TOTRES        IIV(A)
091600000000     C                   GOTO      STA2
091700000000     C                   END
091800000000     C                   END
091900000000     C                   END
092000000000     C     STA2          TAG
092100000000     C                   Z-ADD     0             A
092200000000     C  N27              DO        100           A
092300000000     C                   ADD       TOTRES        IRI(A)
092400000000     C                   GOTO      STA3
092500000000     C                   END
092600000000     C                   MOVEL     *BLANKS       CPADS
092700990329     C                   MOVEL     §cpkcc        CPACC
092800990329     C                   MOVE      §cpksc        CPASC
092900990329     C                   MOVEL     §cpcdc        CPACDC
093000990401     C                   MOVEL     §cpvoc        CPAvoc
093100000000     C   27              DO        100           A
093200000000     C     CRI(A)        IFEQ      CPADS
093300000000     C                   ADD       TOTRES        IRI(A)
093400000000     C                   GOTO      STA3
093500000000     C                   ELSE
093600000000     C     CRI(A)        IFEQ      *BLANKS
093700000000     C                   MOVEL     CPADS         CRI(A)
093800000000     C                   Z-ADD     TOTRES        IRI(A)
093900000000     C                   GOTO      STA3
094000000000     C                   END
094100000000     C                   END
094200000000     C                   END
094300000000     C                   END
094400000000     C     STA3          TAG
094500000000     C                   END
094600000000     C*CALCOLO IMPONIBILE DEGLI IMPORTI ESENTI
094700000000     C                   Z-ADD     0             K
094800990324     C                   Z-ADD     0             IMPESE           17 5
094900000000     C                   DO        10            K
095000000000     C     AIV(K)        IFNE      *BLANKS
095100000000     C                   MOVEL     AIV(K)        IVDS
095200990331     C     ivaese        IFne      '   '
095300000000     C                   ADD       IIV(K)        IMPESE
095400000000     C                   END
095500000000     C                   END
095600000000     C                   END
095700000000     C*CALCOLO BOLLO SULL'IMPONIBILE DEGLI IMPORTI ESENTI
095800000000     C                   Z-ADD     0             IMPBOL
095900000000     C     IMPESE        CABEQ     0             STAR
096000000211     C     IMPESE        IFGE      §geIMB
096100000211     C                   Z-ADD     §geIBL        IMPBOL           17 5
096200000000     C                   END
096300000000     C*BOLLO ESENTE
096400000000     C     IMPBOL        IFGT      0
096500000000     C                   Z-ADD     0             A
096600990325     C     4             OCCUR     dscp
096700000000     C                   MOVEL     *BLANKS       CPADS
096800990329     C                   MOVEL     §cpkcc        CPACC
096900990329     C                   MOVE      §cpksc        CPASC
097000990329     C                   MOVEL     §cpcdc        CPACDC
097100990401     C                   MOVEL     §cpvoc        CPAvoc
097200000000     C                   DO        100           A
097300000000     C     CRI(A)        IFEQ      CPADS
097400000000     C                   ADD       IMPBOL        IRI(A)
097500000000     C                   GOTO      STAR
097600000000     C                   ELSE
097700000000     C     CRI(A)        IFEQ      *BLANKS
097800000000     C                   MOVEL     CPADS         CRI(A)
097900000000     C                   Z-ADD     IMPBOL        IRI(A)
098000000000     C                   GOTO      STAR
098100000000     C                   END
098200000000     C                   END
098300000000     C                   END
098400000000     C                   END
098500000000     C     STAR          TAG
098600000000     C** PER IL BOLLO MEMORIZZO IN QUESTO MOMENTO SOLO
098700000000     C** L'IMPORTO NELLA SCHIERA DELLE CONTROPARTITTE
098800000000     C                   ENDSR
098900000000     C******************************************************
099000000000     C     STATOT        BEGSR
099100000000     C**-----------------------------------
099200000000     C** CALCOLO E STAMPA TOTALE FATTURA
099300000000     C**----------------------------------
099400000000     C   15              DO
099500990329     C     clnspeboll    IFEQ      'S'
099600000000     C** CALCOLO BOLLO SU R.B.
099700000000     C                   END
099800000000     C                   END
099900990324     C                   Z-ADD     0             TOTBOL           17 5
100000990329     C*    IMPBOL        ADD       IMPBRB        TOTBOL
100100990329     C                   ADD       IMPBol        TOTBOL
100200000000     C** TOTALE BOLLI R.B. + ESENZIONE IVA
100300000000     C   28              EXCEPT    STAMSC
100400000000     C                   EXCEPT    FONDOX
100500990326     C                   XFOOT     IV            TOTIV            15 5
100600990326     C                   XFOOT     IIV           TOTIIV           15 5
100700990324     C     TOTIV         ADD       TOTIIV        TOTGEN           17 5
100800000000     C                   ADD       TOTBOL        TOTGEN
100900000000     C** TOTALE FATTURA
101000000000     C                   Z-ADD     0             K
101100000000     C                   Z-ADD     0             K2
101200000000     C                   DO        10            K
101300000000     C                   SETOFF                                       50
101400000000     C     AIV(K)        IFNE      *BLANKS
101500000000     C                   MOVEL     AIV(K)        IVDS
101600990331     C     IVaese        COMP      '   '                              5050
101700000000     C     K             IFEQ      5
101800000000     C                   EXCEPT    TESTA
101900000000     C                   END
102000000000     C     K             IFEQ      9
102100000000     C                   EXCEPT    TESTA
102200000000     C                   END
102300990602      *arrotondamento per divisa ITL o EUR imponibile/imposta
102400990602     c                   z-add     iiv(k)        decds
102500990602     c                   exsr      arrot
102600990602     c                   z-add     decds         iivs             11 2
102700990602      *
102800990602     c                   z-add     iv(k)         decds
102900990602     c                   exsr      arrot
103000990602     c                   z-add     decds         ivs              11 2
103100990602      *
103200000000     C                   EXCEPT    FONDO
103300000000     C                   ADD       1             K2                2 0
103400000000     C                   END
103500000000     C                   END
103600990602      *arrotondamento per divisa ITL o EUR totale /bolli
103700990602     c                   z-add     totgen        decds
103800990602     c                   exsr      arrot
103900990602     c                   z-add     decds         totgens          11 2
104000990602     c                   z-add     totbol        decds
104100990602     c                   exsr      arrot
104200990602     c                   z-add     decds         totbols          11 2
104300090127     C*                  EXCEPT    TGEN
104400000000     C     TOTBOL        IFGT      0
104500000000     C     K2            IFEQ      4
104600000000     C                   EXCEPT    TBOL1
104700000000     C                   GOTO      LT4
104800000000     C                   END
104900000000     C     K2            IFEQ      8
105000000000     C                   EXCEPT    TBOL1
105100000000     C                   GOTO      LT4
105200000000     C                   END
105300090127     C*                  EXCEPT    TBOL
105400000000     C     LT4           TAG
105500000000     C                   END
105600000000     C*
105700000000     C** SE CI SONO BOLLI STAMPO ESENZIONE
105800000000     C     TOTGEN        IFGT      0
105900090127     C*                  EXSR      CREPN
106000000000     C                   END
106100000000     C                   ENDSR
106200000000     C******************************************************
106300000000     C*STAMPA DEGLI ERRORI
106400000000     C     ERRORE        BEGSR
106500000000     C  N88              OPEN      OCPRTSTD
106600000000     C  N88              SETON                                        88
106700000000     C     CONERR        IFGE      60
106800000000     C                   Z-ADD     0             PAGE1
106900000000     C                   EXCEPT    ERR
107000000000     C                   Z-ADD     1             CONERR
107100000000     C                   END
107200000000     C                   EXCEPT    ERR2
107300000000     C                   ADD       1             CONERR
107400000000     C                   ENDSR
107500900207     C************************************************************
107600900207     C*AGGIORNAMENTO DATA AREA  SE HA AGG.TO ALMENO UNA FATTURA
107700900207     C************************************************************
107800000000     C     AGGDTA        BEGSR
107900990329     c                   move      '5'           xnmric
108000990329     C                   MOVEl     §QCRIV        wserie                         REG.IVA VEND.
108100990329     C                   MOVE      §QCFI3        wserie                         LIBRO IVA VEND.
108200990329     c                   move      datfat        xnmdat
108300990329     c                   move      datfat        xnmdag
108400990329     C                   Z-ADD     FATNUM        xnmnum
108500990329     c                   exsr      repnum
108600000000     C                   ENDSR
108700000000     C*----------------------------------------------------*
108800000000     C     CREPN         BEGSR
108900990326     C*----------------------------------------------------*
109000000000     C** INPUTAZIONE CONTABILE
109100990326     C                   Z-ADD     0             riga              9 0
109200990326     C                   Z-ADD     0             rigai             9 0
109300090127     c*                  clear                   ndbhm000
109400990326
109500990407     C                   MOVEL     §QCfgn        bhmcaustes
109600990326     C                   Z-ADD     FATNUM        bhmsubnum
109700990326     c                   movel     knraz         bhmrifint
109800990326     c                   move      wsocieta      bhmsocieta
109900990326     c                   move      'CG'          bhmctb
110000990326     c                   move      'CG'          bhmclasse
110100990326     c                   move      'D'           bhmtpregz
110200990326     c                   add       1             riga
110300990326     c                   z-add     riga          bhmsubrig
110400990326     C                   movel     '2'           bhmserreg
110500990326     C                   move      §qcFI3        bhmserreg
110600990326     C                   MOVEL     rag1          bhmdesbrev
110700010622     c                   move      mocval        bhmdivisa
110800990326     c                   movel     '000151'      bhmkcc
110900990326     C                   MOVEL     PRECLI        bhmksc
111000990401     C                   MOVEl     '046'         bhmunita
111100990326     C                   move      DATFAT        bhmdtreg
111200990326     C                   move      DATFAT        bhmdtdoc
111300990326     C                   move      DATFAT        bhmdtpar
111400990326     C                   Z-ADD     fatnum        bhmnrreg
111500990326     C                   Z-ADD     fatnum        bhmnrdoc
111600990326     C                   Z-ADD     fatnum        bhmnrpar
111700011214     c                   z-add     totgens       bhmimporto
111800011214     c                   z-add     totgens       bhmimportd
111900990326     C                   MOVEL     'D'           bhmsegno
112000990326     c                   movel     cpg           bhmcondpag
112100090127     C*                  write     ndbhm000
112200000000     C** SCRITTURA RECORD GENERALE
112300990324     C** TIPO Rekord *C*
112400020103     c                   clear                   iivw
112500020103     c                   clear                   ivw
112600020103     c                   eval(h)   iivw = iiv
112700020103     c                   eval(h)   ivw  = iv
112800000000     C                   DO        10            A
112900090127     c*                  clear                   ndbhi000
113000000000     C     AIV(A)        IFNE      *BLANKS
113100990326     C                   MOVE      AIV(A)        ivds
113200990331     C                   z-add     ivaalq        bhiali
113300990331     C                   MOVE      ivaese        bhirifiva
113400020103     C                   Z-ADD     IIVw(A)       bhiimpond
113500020103     C                   Z-ADD     IIVw(A)       bhiimpon
113600020103     C                   Z-ADD     IVw(A)        bhiimporto
113700020103     C                   Z-ADD     IVw(A)        bhiimportd
113800000000     C** CLIENTE CODIFICATO
113900990326     c                   movel     knraz         bhirifint
114000990326     c                   z-add     fatnum        bhisubnum
114100990326     c                   movel     wsocieta      bhisocieta
114200990326     c                   add       1             rigai
114300990326     c                   z-add     rigai         bhisubrig
114400990326     c                   move      'A'           bhisegno
114500990326     C                   move      §qcfi3        bhilibro
114600990326     C                   move      '2'           bhitpreg
114700090127     C*                  WRITE     ndbhi000
114800000000     C                   END
114900000000     C                   END
115000000000     C** IVA
115100000000     C     TOTBOL        IFGT      0
115200011214     C                   Z-ADD     TOTBOLs       bhiimpon
115300011214     C                   Z-ADD     TOTBOLs       bhiimpond
115400990326     C                   Z-ADD     0             bhiimporto
115500990326     C                   Z-ADD     0             bhiimportd
115600990326     C                   ADD       1             rigai
115700990326     c                   z-add     rigai         bhisubrig
115800990326     C                   z-add     0             bhiali
115900090127     C*                  WRITE     ndbhi000
116000000000     C                   END
116100000000     C** BOLLI
116200000000     C** SCRITTURA IVA
116300000000     C** UN RECORD OGNI CODICE IVA
116400000000     C** UN RECORD PER BOLLI
116500000000     C**
116600000000     C** TIPO RECORD  *D*
116700000000     C** AZZERAMENTI DS
116800020103     c                   clear                   iriw
116900020103     c                   eval(h)   iriw = iri
117000000000     C                   DO        100           A
117100900206     C     CRI(A)        IFGT      *ZERO
117200900206     C                   MOVEL     CRI(A)        CPADS
117300990326     C                   move      CPACC         bhmkcc
117400990326     C                   move      CPASC         bhmksc
117500990401     C                   movel     CPAvoc        bhmvoce
117600020103     C                   Z-ADD     IRIw(A)       bhmimporto
117700020103     C                   Z-ADD     IRIw(A)       bhmimportd
117800990326     C                   Z-ADD     Fatnum        bhmnrpar
117900990326     C                   move      DATFAT        bhmdtpar
118000990326     C                   add       1             riga
118100990326     c                   z-add     riga          bhmsubrig
118200990326     c                   move      'A'           bhmsegno
118300090127     C*                  WRITE     ndbhm000
118400990326      *
118500990326     c                   move      bhmdtcoan     bhadtcoan
118600990326     c                   move      bhmrifint     bharifint
118700990326     c                   move      bhmsubnum     bhasubnum
118800990326     c                   move      bhmsubrig     bhasubrig
118900990326     c                   move      bhmvoce       bhavoce
119000990326     c                   move      'D1'          bhadimen
119100990406     C                   movel     CPACDC        bhaentit1
119200990326     c                   move      wsocieta      bhasocieta
119300020103     C                   Z-ADD     Iriw(A)       bhaimporto
119400020103     C                   Z-ADD     Iriw(A)       bhaimportd
119500990326     c                   move      'A'           bhasegno
119600990326     c                   add       1             bhasubsubr
119700090127     c*                  write     ndbha000
119800000000     C                   END
119900891227     C                   END
120000000000     C** SCRITTURA RECORD CONTROPARTITE
120100000000     C** UN RECORD PER CONTROPARTITA/CDC -SCHIERA CRI
120200000000     C                   ENDSR
120300951219     C*------------------------------------------------------------------------*
120400951219     C* CARQC -  CARICAMENTO DATI VARI CONTABILITA'
120500951219     C*------------------------------------------------------------------------*
120600951219     C     CARQC         BEGSR
120700951219     C* DATI VARI 1
120800951219     C                   CLEAR                   DSQC1
120900951219     C                   Z-ADD     1             CODUT
121000951219     C                   MOVEL     'QC'          CODTAB
121100951219     C                   MOVEL(P)  '1'           CODCOD
121200951219     C     KTABEL        CHAIN     TABEL00F                           55
121300951219IF  1C     *IN55         IFEQ      *OFF
121400951219     C                   MOVEL     TBLUNI        DSQC1
121500951219E   1C                   ENDIF
121600951219     C* DATI VARI 2
121700951219     C                   CLEAR                   DSQC2
121800951219     C                   MOVEL     'QC'          CODTAB
121900951219     C                   MOVEL(P)  '2'           CODCOD
122000951219     C     KTABEL        CHAIN     TABEL00F                           55
122100951219IF  1C     *IN55         IFEQ      *OFF
122200951219     C                   MOVEL     TBLUNI        DSQC2
122300951219E   1C                   ENDIF
122400951219     C*
122500951219     C                   ENDSR
122600951219     C*------------------------------------------------------------------------*
122700951219     C* CARQT -  CARICAMENTO DATI VARI TASSAZIONE
122800951219     C*------------------------------------------------------------------------*
122900000211     C****     CARQT         BEGSR
123000951219     C* DATI VARI 1
123100000211     C****                   CLEAR                   DSQT1
123200000211     C****                   Z-ADD     1             CODUT
123300000211     C****                   MOVEL     'QT'          CODTAB
123400000211     C****                   MOVEL(P)  '1'           CODCOD
123500000211     C****     KTABEL        CHAIN     TABEL00F                           55
123600000211IF  1C****     *IN55         IFEQ      *OFF
123700000211     C****                   MOVEL     TBLUNI        DSQT1
123800000211E   1C****                   ENDIF
123900951219     C*
124000000211     C****                   ENDSR
124100000211     C****************************************************
124200000211     C** CARICAMENTO IMPORTI STANDARD MONETA
124300000211     C****************************************************
124400000211     C     CARGEI        BEGSR
124500000211      *
124600000211     C                   CLEAR                   DSBS02
124700000211      *
124800000211     C                   MOVEL     'L'           T02TLA
124900000211     C                   MOVEL     'C'           T02MOD
125000000211     C                   MOVEL     KNSIF         T02SIF
125100000211     C                   MOVEL     'GEI'         T02COD
125200000211     C                   MOVEL     XSCDIV        T02KE1
125300000211      *
125400000211     C                   CALL      'TIBS02R'
125500000211     C                   PARM                    KPJBA
125600000211     C                   PARM                    DSBS02
125700000211      *
125800000211     C     T02ERR        IFEQ      *BLANKS
125900000211     C                   MOVEL     T02UNI        DGEI
126000000211     C                   ENDIF
126100000211      *
126200000211     C                   ENDSR
126300990325      *----------------------------------------------------
126400990325     C     primanota     BEGSR
126500990325     C*----------------------------------------------------
126600990325     C*
126700010622      *se la divisa che sto utilizzando per contabilizzare è diversa dalla
126800010622      * moneta di conto richiamo adeguatore per sistemazione importi nella
126900010622      *divisa corretta
127000010622     c                   if        mocval <> xscdiv
127100010622     c                   exsr      adeguatore
127200010622     c                   endif
127300990325     C* contabilizzazione batch
127400990325     c                   reset                   ndc071ds
127500990325     C                   MOVE      wsocieta      SOCieta071
127600990325     C                   MOVE      KNRAZ         rifint071
127700990325     c                   move      *loval        dtregda071
127800990325     c                   move      *hival        dtrega071
127900990325     c                   move      *loval        subnumd071
128000990325     c                   move      *hival        subnuma071
128100000316     C                   MOVE      *off          solcont071
128200990325    >C                   MOVE      *off          interat071
128300990325     C                   move      *off          err071
128400990325     c                   move      *off          control071
128500990325     C                   MOVEL     NDC071DS      kpjbu
128600990325     C*
128700990325     C                   CALL      'NDC071C'
128800990325     C                   parm                    kpjba
128900990325     C                   movel     kpjbu         ndc071ds
129000990325     C*
129100990325     C                   ENDSR
129200010622      *----------------------------------------------------
129300010622     C     adeguatore    BEGSR
129400010622     C*----------------------------------------------------
129500010622     C*
129600010622     C* adeguatore parametri
129700010622     c                   reset                   ndc270ds
129800010622     C                   MOVE      wsocieta      SOCieta270
129900010622     C                   MOVE      KNRAZ         rifint270
130000010622     c                   move      *loval        dtregda270
130100010622     c                   move      *hival        dtrega270
130200010622     c                   move      *loval        subnumd270
130300010622     c                   move      *hival        subnuma270
130400010622     C                   move      *off          err270
130500010622     c                   move      '3'           ricalc270
130600010622     c                   move      '1'           cmt270
130700010622     C                   MOVEL     NDC270DS      kpjbu
130800010622     C*
130900010622     C                   CALL      'NDC270C'
131000010622     C                   parm                    kpjba
131100010622     C                   movel     kpjbu         ndc270ds
131200010622     C*
131300010622     C                   ENDSR
131400990324     C*----------------------------------------------------*
131500990324     C* Reperisco numeratore
131600990324     C*----------------------------------------------------*
131700990324     c     repnum        begsr
131800990324     C                   MOVE      wsocieta      XNMSOC            3
131900990324     C                   MOVE      *BLANKS       XNMUNI            8
132000990324     C                   MOVE      'CG'          XNMCTB            2
132100990324     C                   MOVE      *BLANKS       XNMKEY            8
132200990324     C                   MOVE      WSERIE        XNMSER            4
132300990324     C                   MOVE      *OFF          XNMCMT            1
132400990324     C                   MOVE      *On           XNMIVA            1
132500990324     C                   MOVE      *OFF          XNMREG            1
132600990324     C                   MOVE      '0'           XNMerr            1
132700990324     C*
132800990324     C                   CALLB     'XNMR'        PNMR
132900990324     C*
133000990324     C     xnmerr        ifne      '0'
133100990324     C                   seton                                        h5
133200990324     C                   end
133300990324      *
133400990324     C                   endsr
133500990329     C*----------------------------------------------------*
133600990329     C* tabella 155
133700990329     C*----------------------------------------------------*
133800990329     c     tab155        begsr
133900990330     c                   clear                   tb155
134000990329     c     mocmat        chain     tmamp01l                           66
134100990329     c  n66              do
134200990329     C                   MOVEL     '155'         GRU
134300990329     C                   MOVEL     *ZEROS        KEYT
134400990329     C                   MOVE      ampclm        KEYT
134500990329     C     KEYANT        CHAIN     ANTAB000                           66
134600990329     C  N66              MOVEL     TABDES        tb155
134700990329     c                   enddo
134800990329     C                   endsr
134900990602     c*------------------------------------------------
135000990602     c     arrot         begsr
135100990602     c*------------------------------------------------
135200990602     c                   if        xscdiv = 'ITL' and dec(1) >= 5
135300990602     c                   add       1             int
135400990602     c                   movea     *zeros        dec
135500990602     c                   end
135600990602     c                   if        xscdiv = 'EUR' and dec(3) >= 5
135700990602     c                   add       1             dec(2)
135800990602     c                   movea     *zeros        dec(3)
135900990602     c                   end
136000990602     c                   endsr
136100990602     c*------------------------------------------------
136200990324     C*----------------------------------------------------*
136300990324     C* Reperimento dati società
136400990324     C*----------------------------------------------------*
136500990324     C     REPSOC        BEGSR
136600990324     C*
136700990324     C                   CALLB     'XSOC'
136800990324     C                   PARM                    TIPXSC            6
136900990324     C                   PARM      wsocieta      SOCXSC            3
137000990324     C                   PARM                    CDSXSC            9 0
137100990324     C                   PARM                    MODXSC            3
137200990324     C                   PARM      *blanks       RTNXSC            1
137300990324     C                   PARM                    XSOCDS
137400990324     C                   PARM                    KPJBA
137500990324     C*
137600990324     C                   ENDSR
137700000000     O*--------------------------------------------------------------*
137800000000     O*     TESTATA                                                  *
137900000000     O*--------------------------------------------------------------*
138000000000     O*--------------FATTURE------------------------------------------*
138100000000     OFAWEPPRT  E            TESTA            01
138200990401     O                       SC(1)               80
138300000000     O          E            TESTA            02
138400000000     O                       RSC               +  1
138500990326     O                       IND               +  1
138600000000     O                       LOC               +  1
138700000000     O          E            TESTA            03
138800000000     O                                         +  1 'Codice Fiscale'
138900990326     O                       CFI               +  1
139000000000     O                                         +  5 'Partita IVA'
139100000000     O                       PIVD              +  1
139200000000     O          E            TESTA            04
139300960604     O                                         +  1 'Regis.Imprese'
139400000000     O                       TRI               +  1
139500960604     O                                         +  1 'R.E.A.'
139600990326     O                       COM               +  1
139700910702     O                                         +  1 'Cap.Soc.'
139800990326     O*                      CSO           2   +  1
139900000000     O          E            TESTA            05
140000990401     O                       SC(1)               80
140100000000     O          E            TESTA            08
140200000000     O                       MOCCLI              40
140300000000     O                       RAG1                70
140400000000     O          E            TESTA            09
140500000000     O                       RAG2                70
140600000000     O          E            TESTA            11
140700000000     O                       VIA               + 46
140800000000     O          E            TESTA            12
140900000000     O                       CAP               + 46
141000000000     O                       CIT               +  1
141100000000     O                       PRV               +  1
141200090130     O          E            TESTA            15
141300090130     O                       SC(2)               80
141400090127     O*                      FATNUM        Z     41
141500090127     O*                                          46 'del'
141600090127     O*                      DATAFT              55 '  /  /    '
141700090127     O*         E            TESTA            17
141800090127     O*                      SC(3)               80
141900090127     O*                      MOCCPG              21
142000090127     O*                      DESCPG              52
142100090127     O*         E            TESTA            18
142200090127     O*                      BNA                 34
142300000000     O          E            TESTA            22 23
142400990401     O                       SC(4)               80
142500000000     O*--------------------------------------------------------------*
142600000000     O*     TESTATA DI CAMBIO BOLLA                                  *
142700000000     O*--------------------------------------------------------------*
142800000000     O          E            BOLLA       1
142900000000     O                                           32 'Bolla di Consegna N.'
143000000000     O                       MOCNBL        Z     39
143100000000     O                                           43 'del'
143200990401     O                       DATBOL              54 '  /  /    '
143300000000     O*RIBATTITURA
143400000000     O          E            BOLLA       0  0
143500000000     O                                           32 'Bolla di Consegna N.'
143600000000     O                       MOCNBL        Z     39
143700000000     O                                           43 'del'
143800990401     O                       DATBOL              54 '  /  /    '
143900000000     O*--------------------------------------------------------------*
144000000000     O*     TESTATA DI CAMBIO ORDINE                                 *
144100000000     O*--------------------------------------------------------------*
144200000000     O          E            ORDINI      1
144300990401     O                                           22 'Ns.ordine N.'
144400990401     O                       MOCNOR        Z     27
144500990401     O                                           31 'del'
144600990401     O                       DATORD              42 '  /  /    '
144700990401     O              N18                          58 'Rif.Vs.ordine N.'
144800990401     O              N18      MOCNRF        Z     66
144900990401     O              N18                          70 'del'
145000990326     O              N18      DATRIF              80 '  /  /    '
145100000000     O*--------------------------------------------------------------*
145200000000     O*     DETTAGLIO                                                *
145300000000     O*--------------------------------------------------------------*
145400000000     O          E            DETAIL      1
145500990401     O                       PREIMP              78 '       0 ,     '
145600990401     O                       PRZUNI              60 '       0 ,     '
145700990401     O                       MOCQTA              44 '     0 ,  '
145800890707     O                       DESMAT              33
145900890707     O                       MOCMAT              12
146000000000     O*--------------------------------------------------------------*
146100000000     O*     FONDO                                                    *
146200000000     O*--------------------------------------------------------------*
146300010305     O          E            FONDOX           60
146400990602     O                       SC(5)               80
146500000000     O          E            FONDO       1
146600090127     O*             N50      IVs           3     35
146700090127     O*             N50      IVaalq        2     20
146800990602     O                       IIVs          2     14
146900000000     O*--------------------------------------------------------------*
147000000000     O*     SCONTO CASSA                                             *
147100000000     O*--------------------------------------------------------------*
147200010305     O          E            STAMSC           59
147300000000     O                                           24 'Sconto Cassa'
147400000000     O                       TOTCAS        M     81
147500990329     O                       clnscopag1    3     56
147600000000     O*--------------------------------------------------------------*
147700000000     O*     TOTALI GENERALI                                          *
147800000000     O*--------------------------------------------------------------*
147900000000     O          E            TGEN        0
148000990602     O                       TOTGENs       2     75
148100990602     O                       TOTBOLs       2     59
148200010627     O                       mocval              80
148300000000     O*--------------------------------------------------------------*
148400000000     O          E            TBOL        1
148500000000     O                                           69 'BOLLI: ES.ART.15'
148600000000     O          E            TBOL1       0
148700000000     O                                         +  1 'BOLLI: ES.ART.15'
148800000000     O*--------------------------------------------------------------*
148900000000     O          E            SEGUE            64
149000000000     O                                           83 'SEGUE'
149100000000     O*--------------------------------------------------------------*
149200000000     O*     PROTOCOLLO ORDINE                                        *
149300000000     O          E            PROORD      1
149400000000     O                                           30 '* Vs. Protocollo N.'
149500000000     O                       PRENPC        Z     39
149600000000     O                                           43 'del'
149700990326     O                       DATPRO              52 '  /  /    '
149800000000     O                                           71 'Ns. Protocollo N.'
149900000000     O                       PRENPN        Z     81
150000000000     O*
150100000000     O*--------------------------------------------------------------*
150200000000     O*     PROTOCOLLO  CLIENTE                                      *
150300000000     O*--------------------------------------------------------------*
150400000000     O          E            PROFAT      2
150500890707     O                                           29 '** Vs. Protocollo N.'
150600990326     O                       DICNRCLI      Z     38
150700890707     O                                           42 'del'
150800990326     O                       DATCLI              51 '  /  /    '
150900890707     O                                           70 'Ns. Protocollo N.'
151000990329     O                       dicnrinter    Z     80
151100000000     O*--------------------------------------------------------------*
151200000000     O*     ERRORI                                                   *
151300000000     O*--------------------------------------------------------------*
151400951219     OOCPRTSTD  E            ERR              03
151500000000     O                       RSUT                20
151600000000     O                       UDATE              118 '  /  /  '
151700000000     O                       PAGE1         Z    132
151800000000     O                                           70 'CONTROLLO FATTURAZIONE'
151900000000     O                                           74 'DEL'
152000990326     O                       DATAFT              83 '  /  /    '
152100000000     O          E            ERR2        1
152200000000     O                                           17 'CODICE CLIENTE:'
152300000000     O                       MOCCLI              26
152400000000     O               22                          44 'CODICE PAGAMENTO'
152500000000     O               22      MOCCPG              49
152600000000     O                     03                    75 'ANAGRAFICA MANCANTE'
152700000000     O                     03                    89 'O INESISTENTE'
152800000000     O                     20                    75 'ANAGRAFICA MANCANTE'
152900000000     O                     20                    89 'O INESISTENTE'
153000000000     O                     22                    46 'CONDIZIONI DI PAGAMENTO'
153100000000     O                     22                    91 'INESISTENTI'
153200890707**
153300000000----------------------------------------------------------------------
153400090130    DOCUMENTO AD USO INTERNO
153500000000    Pagamento:
153600990401Materiale----Descrizione-----------Quantita'------Prezzo----------Importo---
153700090127---Imponibile---------------------------------------------------------------
