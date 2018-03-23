000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300941026      * TNSC94R                                                      *
000400910916      *       *-----------------------------------*                  *
000500910916      *        INTERROGAZIONE STORICO CONTRASSEGNI                   *
000600910916      *       *-----------------------------------*                  *
000700910910      *
000800910521      * 12 -VIDEO PRECEDENTE                                         *
000900910910      * 15 -NESSUN RECORD DA CARICARE O FINE RECORD DA CARICARE      *
001000910910      * 16 -FINE RECORD VARIATI NEL SUBFILE                          *
001100910910      * 17 -INDICATORE DI COMODO                                     *
001200910910      * 30/36- ERRORI NEI CAMPI DELLA 1a VIDEATA                     *
001300910910      * 38 -ATTIVA MESSAGGIO DI PAGAMENTO BLOCCATO X QUEL CLIENTE    *
001400910910      * 40 -NON ATTIVA CARICAMENTO DI ALTRE SPEDIZIONI CON = NUM. DOC*
001500910910      *     SE NUMERO DOCUMENTO = 0                                  *
001600910910      * 50 -INDICATORE DI COMODO                                     *
001700910910      * 70 -ATTIVA DISATTIVA PULIZIA SUBFILE                         *
001800910917      * KL -FINE INTERROGAZIONE                                      *
001900900606?     *--------------------------------------------------------------*
002000941026     FTNSC94D   CF   E             WORKSTN
002100941026     F                                     SFILE(SC94S01:NRR)
002200941026     FTNCSV00F  IF   E             DISK
002300941026     FTNCSV01L  IF   E           K DISK
002400941026     F                                     RENAME(TNCSV000:TNCSV1)
002500910916     F                                     INFDS(DSINF1)
002600941026     FTNCSA01L  IF   E           K DISK
002700980127     F***CNACO00FIF  E           K        DISK
002800980127     F***CNIND00FIF  E           K        DISK
002900910909     FCNABI00F  IF   E           K DISK
003000941026     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003100910906     FTABEL00F  IF   E           K DISK
003200941026?     *--------------------------------------------------------------*
003300980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003400980127     D CNACO         E DS                  EXTNAME(CNACO00F)
003500980127     D CNIND         E DS                  EXTNAME(CNIND00F)
003600980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003700980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003800120223     d tnsc30ds      E DS
003900910708     D CNCR80        E DS
004000900514     D KPJBA         E DS
004100910916     D DSINF1          DS
004200910916     D  BLVNR1               397    400B 0
004300900516      *
004400910829     D  DSNRD                  1     10
004500910829     D  NRD                    1      7  0
004600910829     D  FIL                    8     10  0
004700900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004800000000     D  TCU                  398    697
004900000000     D                                     DIM(50)
005000000000     D  KCU                  698    847P 0
005100000000     D                                     DIM(50)
005200000000     D                                     PACKEVEN
005300900606      *
005400900606     D TCUDS           DS
005500900606     D  F3                     3      3
005600900606     D  F4                     4      4
005700900606     D  F56                    5      6
005800900515      *
005900941026     D WLBDA8          DS
006000941026     D  G02DAT                 1      8  0
006100941026     D  G02INV                 9     16  0
006200941026     D  G02ERR                17     17
006300941026     D  G02TGI                18     22  0
006400910521      *
006500950711     D TNSC94        E DS                  EXTNAME(TNSC94DS)
006600941026?     *--------------------------------------------------------------*
006700941026     IAZORG01L  AA
006800941026     I                             P    4    5 0ORGFIL
006900941026     I                                 14   33  ORGDES
007000941026?    C*--------------------------------------------------------------*
007100910916     C                   EXSR      PULIZ
007200941026     C     KSPE          SETLL     TNCSV1                             15
007300910916     C  N15              EXSR      CARSUB
007400910916     C     NRR           COMP      0                                      15
007500120223     C  N15*inkl         DOWEQ     '0'
007600910917     C     *IN12         ANDEQ     '0'
007700941026     C                   WRITE     SC94Z01
007800941026     C                   EXFMT     SC94C01
007900910917     C                   SETOFF                                       30
008000910919     C  NKL*IN30         DOWEQ     '0'
008100941026     C                   READC     SC94S01                                30
008200941026     C  N30VSEL          IFEQ      '1'
008300910917     C                   EXSR      VISUAL
008400941026     C                   EXFMT     SC94D02
008500120223     C* interroga multiassegni
008600120223     c                   if        *inki
008700120223     c                   clear                   tnsc30ds
008800120223     c                   eval      sc30CMTI = 'N'
008900120223     c                   eval      sc30AMBI = 'V'
009000120223     c                   eval      sc30TPCI = 'V'
009100120223     c                   eval      sc30AASI = raas
009200120223     c                   eval      sc30LNPI = rlnp
009300120223     c                   eval      sc30NRSI = rnrs
009400120223     c                   eval      sc30NSPI = rnsp
009500120224     c                   eval      sc30dtvI = hdtv
009600120224     c                   eval      sc30orvI = horv
009700120223     c                   movel(p)  tnsc30ds      kpjbu
009800120223     c                   call      'TNSC30R'
009900120223     c                   parm                    kpjba
010000120223     c                   end
010100910902     C                   END
010200910917     C                   END
010300910917     C                   END
010400950711     C                   Z-ADD     NRR           D94LNP
010500950711     C                   MOVEL     TNSC94        KPJBU
010600980127     C*
010700980127     C                   CLEAR                   TIBS69
010800980127     C                   MOVEL     'C'           I69TLA
010900980127     C                   CALL      'TIBS69R'
011000980127     C                   PARM                    TIBS69
011100980127     C                   PARM                    CNACO
011200980127     C                   PARM                    CNIND
011300980127     C                   PARM                    CNCLP
011400980127     C                   PARM                    FNCLS
011500980127     C*
011600910521     C                   SETON                                        LR
011700900515      *-----------------------------------------------------***********
011800910916      * PULIZIA SUBFILE                                     *  PULIZ  *
011900900515      *-----------------------------------------------------***********
012000900515     C     PULIZ         BEGSR
012100900524      *
012200910903     C                   SETOFF                                       70
012300941026     C                   WRITE     SC94C01
012400910903     C                   SETON                                        70
012500910917     C                   Z-ADD     0             NRR               3 0
012600910916     C*
012700900514     C                   ENDSR
012800941026      *-----------------------------------------------------***********
012900941026      * CARICAMENTO DEL SUBFILE                             *  PULIZ  *
013000941026      *-----------------------------------------------------*
013100941026      *   CARICAMENTO SUBFILE                               *
013200941026      *-----------------------------------------------------*
013300910916     C     CARSUB        BEGSR
013400910917     C                   SETOFF                                       25
013500910916     C     *IN15         DOWEQ     '0'
013600910916     C*
013700941026     C     KSPE          READE     TNCSV1                                 15
013800910916     C  N15              DO
013900910916     C*
014000910917     C* DAL 1° RECORD CARICA ANCHE I DATI SPEDIZIONE
014100910917     C  N25              DO
014200910917     C                   Z-ADD     CSVLNP        VLNP
014300910917     C* DECODIFICA
014400941026     C                   Z-ADD     VLNP          ORGFIL
014500941026     C                   MOVEL     *BLANKS       ORGDES
014600941026     C     ORGFIL        CHAIN     AZORG01L                           20
014700941026     C                   MOVEL     ORGDES        DESLNP
014800910917     C*
014900910917     C                   Z-ADD     CSVLNA        VLNA
015000910917     C* DECODIFICA
015100941026     C                   Z-ADD     VLNA          ORGFIL
015200941026     C                   MOVEL     *BLANKS       ORGDES
015300941026     C     ORGFIL        CHAIN     AZORG01L                           20
015400941026     C                   MOVEL     ORGDES        DESLNA
015500910917     C*
015600910917     C                   Z-ADD     CSVAAS        VAAS
015700910917     C                   Z-ADD     CSVNRS        VNRS
015800910917     C                   Z-ADD     CSVNSP        VNSP
015900941026     C                   MOVE      CSVMGS        G02INV
016000941026     C                   MOVEL     CSVAAS        G02INV
016100941026     C                   Z-ADD     0             G02DAT
016200941026     C                   MOVEL     '3'           G02ERR
016300941026     C                   CALL      'XSRDA8'
016400941026     C                   PARM                    WLBDA8
016500941026     C                   Z-ADD     G02DAT        VDSP
016600910917     C*
016700910917     C                   SETON                                        25
016800910917     C                   END
016900910917     C*
017000910916     C                   MOVE      CSVFEV        VFEV
017100910916     C                   MOVEL     CSVCAV        VCAV
017200941026     C                   Z-ADD     CSVDTV        G02INV
017300941026     C                   Z-ADD     0             G02DAT
017400941026     C                   MOVEL     '3'           G02ERR
017500941026     C                   CALL      'XSRDA8'
017600941026     C                   PARM                    WLBDA8
017700941026     C                   Z-ADD     G02DAT        VDTV
017800910916     C                   Z-ADD     CSVORV        VORV
017900910916     C                   MOVEL     CSVFGC        VFGC
018000910916     C                   Z-ADD     CSVNGC        VNGC
018100941026     C                   Z-ADD     CSVDGC        G02INV
018200941026     C                   Z-ADD     0             G02DAT
018300941026     C                   MOVEL     '3'           G02ERR
018400941026     C                   CALL      'XSRDA8'
018500941026     C                   PARM                    WLBDA8
018600941026     C                   Z-ADD     G02DAT        VDGC
018700941026     C                   Z-ADD     CSVCAS        VIMP
018800941026     C                   MOVEL     CSVVCA        VVCA
018900910828     C* NR° DI RECORD RELATIVO FISICO
019000910916     C                   Z-ADD     BLVNR1        VNREC
019100910828     C                   ADD       1             NRR
019200941026     C                   WRITE     SC94S01
019300910916     C*
019400910916     C                   END
019500910916     C                   END
019600910828     C                   ENDSR
019700941026      *-----------------------------------------------------*
019800941026      *   CARICAMENTO SUBFILE                               *
019900941026      *-----------------------------------------------------*
020000910828     C     VISUAL        BEGSR
020100941026     C     VNREC         CHAIN     TNCSV00F                           17
020200910917     C* STATO DEL C/A
020300910917     C                   Z-ADD     CSVSTA        RSTA
020400910917     C                   MOVEL     '4S'          CODTAB
020500910917     C                   MOVE      RSTA          KEYTAB
020600910917     C     KTAB          CHAIN     TABEL                              50
020700910917     C  N50              MOVEL     TBLUNI        DESSTA
020800910917     C   50              MOVEL     *BLANKS       DESSTA
020900910917     C* FILIALE VARIAZIONE
021000910917     C                   MOVEL     CSVFEV        RFEV
021100910917     C* CAUSALE VARIAZIONE
021200910917     C                   MOVEL     CSVCAV        RCAV
021300910917     C* DATA VARIAZIONE
021400941026     C                   Z-ADD     CSVDTV        G02INV
021500941026     C                   Z-ADD     0             G02DAT
021600941026     C                   MOVEL     '3'           G02ERR
021700941026     C                   CALL      'XSRDA8'
021800941026     C                   PARM                    WLBDA8
021900941026     C                   Z-ADD     G02DAT        RDTV
022000910917     C* LINEA DI PARTENZA + DECODIFICA
022100120224     C                   Z-ADD     CSVorv        horv
022200120224     C                   Z-ADD     CSVdtv        hdtv
022300120223     C                   Z-ADD     CSVAAS        RAAS
022400120223     C                   Z-ADD     CSVLNP        RLNP
022500941026     C                   Z-ADD     CSVLNP        ORGFIL
022600941026     C                   MOVEL     *BLANKS       ORGDES
022700941026     C     ORGFIL        CHAIN     AZORG01L                           50
022800941026     C                   MOVEL     ORGDES        DESLNP
022900910917     C* NR° SERIE + SPEDIZIONE
023000910917     C                   Z-ADD     CSVNRS        RNRS
023100910917     C                   Z-ADD     CSVNSP        RNSP
023200910917     C* CODICE CLIENTE
023300910917     C                   Z-ADD     CSVKSC        RKSC
023400910917     C* DATA SPEDIZIONE
023500941026     C                   MOVE      CSVMGS        G02INV
023600941026     C                   MOVEL     CSVAAS        G02INV
023700941026     C                   Z-ADD     0             G02DAT
023800941026     C                   MOVEL     '3'           G02ERR
023900941026     C                   CALL      'XSRDA8'
024000941026     C                   PARM                    WLBDA8
024100941026     C                   Z-ADD     G02DAT        RDSP
024200910917     C* DATA DISTINTA
024300941026     C                   Z-ADD     CSVDDC        G02INV
024400941026     C                   Z-ADD     0             G02DAT
024500941026     C                   MOVEL     '3'           G02ERR
024600941026     C                   CALL      'XSRDA8'
024700941026     C                   PARM                    WLBDA8
024800941026     C                   Z-ADD     G02DAT        RDDC
024900910917     C* LINEA ARRIVO + DECODIFICA
025000910917     C                   Z-ADD     CSVLNA        RLNA
025100941026     C                   Z-ADD     CSVLNA        ORGFIL
025200941026     C                   MOVEL     *BLANKS       ORGDES
025300941026     C     ORGFIL        CHAIN     AZORG01L                           50
025400941026     C                   MOVEL     ORGDES        DESLNA
025500910917     C* NUMERO DISTINTA
025600910917     C                   Z-ADD     CSVNDT        RNDT
025700910917     C* RIFERIMENTO MITTENTE
025800910917     C                   Z-ADD     CSVRMN        RRMN
025900941026     C                   MOVEL     CSVRMA        RRMA
026000910917     C* DATA ARRIVO DISTINTA
026100941026     C                   Z-ADD     CSVDTA        G02INV
026200941026     C                   Z-ADD     0             G02DAT
026300941026     C                   MOVEL     '3'           G02ERR
026400941026     C                   CALL      'XSRDA8'
026500941026     C                   PARM                    WLBDA8
026600941026     C                   Z-ADD     G02DAT        RDTA
026700910917     C* NR° ASSEGNO = RIFERIM. INCASSO
026800910917     C                   MOVEL     CSVNRA        RNRA
026900120223     c                   if        %subst(csvnra: 10: 1) <> ' '
027000120223     c                             and csvabi = 0
027100120223     c                   seton                                        09
027200120223     c                   else
027300120223     c                   setoff                                       09
027400120223     c                   end
027500910917     C* DATA ASSEGNO
027600941026     C                   Z-ADD     CSVDTE        G02INV
027700941026     C                   Z-ADD     0             G02DAT
027800941026     C                   MOVEL     '3'           G02ERR
027900941026     C                   CALL      'XSRDA8'
028000941026     C                   PARM                    WLBDA8
028100941026     C                   Z-ADD     G02DAT        RDTE
028200910917     C* TIPO INTESTAZIONE ASSEGNO
028300910917     C                   MOVEL     CSVTPI        RTPI
028400941026     C                   MOVEL     CSVTPA        RTPA
028500910917     C* CODICE INTESTAZIONE ASS.
028600910917     C                   Z-ADD     CSVCDI        RCDI
028700910917     C* ABI / CAB / EVENTUALE DECODIFICA BANCA
028800910917     C                   Z-ADD     CSVABI        RABI
028900910917     C                   Z-ADD     CSVCAB        RCAB
029000941026     C                   MOVEL     *BLANKS       ABIIST
029100910917     C     KABI          CHAIN     CNABI000                           75
029200941026     C                   MOVEL     ABIIST        RDES
029300941026     C* DATA REGISTRAZIONE INCASSO
029400941026     C                   Z-ADD     CSVDRI        G02INV
029500941026     C                   Z-ADD     0             G02DAT
029600941026     C                   MOVEL     '3'           G02ERR
029700941026     C                   CALL      'XSRDA8'
029800941026     C                   PARM                    WLBDA8
029900941026     C                   Z-ADD     G02DAT        RDRI
030000941026     C* NUMERO REG.INCASSO/CAMBIO
030100941026     C                   Z-ADD     CSVNRI        RNRI
030200941026     C                   Z-ADD     CSVCMB        RCMB
030300910917     C* INTESTAZIONE ASSEGNO / MITTENTE
030400910917     C* SE COD. INTESTAZ. ASSEGNO <> COD. CLIENTE LO DECODIFICO
030500910917     C     RCDI          IFNE      RKSC
030600980127     C***        KCCC      CHAINCNACO000             50
030700980127     C***        KCCC      CHAINCNIND000             50
030800980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
030900980127     C                   CLEAR                   TIBS69
031000980127     C                   CLEAR                   CNACO
031100980127     C                   CLEAR                   CNIND
031200980127     C                   CLEAR                   CNCLP
031300980127     C                   CLEAR                   FNCLS
031400980127     C                   MOVEL     ' '           I69TLA
031500980127     C                   MOVEL     KNSIF         I69SIF
031600980127     C                   Z-ADD     RCDI          I69KAC
031700980127     C                   Z-ADD     RCDI          I69KIN
031800980127     C                   CALL      'TIBS69R'
031900980127     C                   PARM                    TIBS69
032000980127     C                   PARM                    CNACO
032100980127     C                   PARM                    CNIND
032200980127     C                   PARM                    CNCLP
032300980127     C                   PARM                    FNCLS
032400980127     C     O69ERR        IFEQ      *BLANK
032500910917     C                   MOVEL     ACORAG        RINT
032600910917     C                   MOVEL     INDVIA        VIAINT
032700910917     C                   MOVEL     INDCIT        CITINT
032800941028     C                   MOVEL     *BLANKS       CAPINT
032900941028     C     INDCAP        IFNE      *ZEROS
033000941028     C                   MOVEL     INDCAP        CAPINT
033100941028     C                   END
033200941026     C                   MOVEL     INDSTA        NAZINT
033300941026     C                   MOVEL     INDPRV        PROINT
033400941026     C     INDCAE        IFNE      *ZEROS
033500941026     C     INDCAE        ANDNE     *BLANKS
033600941026     C                   MOVEL     INDCAE        CAPINT
033700941026     C                   END
033800910917     C                   END
033900980127     C                   END
034000910917     C* SE CLIENTE VARIO LO DECODIFICO DA BLCSA
034100910917     C                   MOVE      RKSC          APP               4
034200910917     C     APP           IFEQ      '8888'
034300910917     C     APP           OREQ      '9999'
034400941026     C     KBLCAS        CHAIN     TNCSA000                           50
034500910917     C                   MOVEL     CSARSD        DESMIO
034600910917     C                   MOVEL     CSAVID        INDMIO
034700941026     C                   MOVEL     CSACAD        CAPMIO
034800941026     C                   MOVEL     CSACID        LOCMIO
034900941026     C                   MOVEL     CSANZM        NAZMIO
035000941026     C                   MOVEL     CSAPRM        PROMIO
035100910917     C                   ELSE
035200980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
035300980127     C                   CLEAR                   TIBS69
035400980127     C                   CLEAR                   CNACO
035500980127     C                   CLEAR                   CNIND
035600980127     C                   CLEAR                   CNCLP
035700980127     C                   CLEAR                   FNCLS
035800980127     C                   MOVEL     ' '           I69TLA
035900980127     C                   MOVEL     KNSIF         I69SIF
036000980127     C                   Z-ADD     RKSC          I69KAC
036100980127     C                   Z-ADD     RKSC          I69KIN
036200980127     C                   CALL      'TIBS69R'
036300980127     C                   PARM                    TIBS69
036400980127     C                   PARM                    CNACO
036500980127     C                   PARM                    CNIND
036600980127     C                   PARM                    CNCLP
036700980127     C                   PARM                    FNCLS
036800980127     C     O69ERR        IFEQ      *BLANK
036900980127     C***        KCCC      CHAINCNACO000             50
037000980127     C***        KCCC      CHAINCNIND000             50
037100910917     C                   MOVEL     ACORAG        DESMIO
037200910917     C                   MOVEL     INDVIA        INDMIO
037300910917     C                   MOVEL     INDCIT        LOCMIO
037400941028     C                   MOVEL     *BLANKS       CAPMIO
037500941028     C     INDCAP        IFNE      *ZEROS
037600941028     C                   MOVEL     INDCAP        CAPMIO
037700941028     C                   END
037800941026     C                   MOVEL     INDSTA        NAZMIO
037900941026     C                   MOVEL     INDPRV        PROMIO
038000941026     C     INDCAE        IFNE      *ZEROS
038100941026     C     INDCAE        ANDNE     *BLANKS
038200941026     C                   MOVEL     INDCAE        CAPMIO
038300941026     C                   END
038400980127     C                   END
038500910917     C                   END
038600910917     C* DESTINATARIO
038700910917     C                   MOVEL     CSVRSD        RRSD
038800910917     C                   MOVEL     CSVPAR        RPAR
038900910917     C                   MOVEL     CSVPRO        RPRO
039000910917     C* NR° DOCUMENTO PAGAMENTO
039100910917     C                   Z-ADD     CSVNDP        RNDP
039200910917     C* BANCA PAGAMENTO - DECODIFICA SOLO SE COD. <> 0
039300910917     C                   MOVEL     *BLANKS       RBAN
039400910917     C                   Z-ADD     CSVBNA        RBNA
039500910917     C     CSVBNA        IFNE      0
039600980127     C                   CLEAR                   TIBS69
039700980127     C                   CLEAR                   CNACO
039800980127     C                   CLEAR                   CNIND
039900980127     C                   CLEAR                   CNCLP
040000980127     C                   CLEAR                   FNCLS
040100980127     C                   MOVEL     ' '           I69TLA
040200980127     C                   MOVEL     KNSIF         I69SIF
040300980127     C                   Z-ADD     KBNA          I69KCC
040400980127     C                   Z-ADD     CSVBNA        I69KAC
040500980127     C                   MOVEL     *BLANKS       ACORAG
040600980127     C                   CALL      'TIBS69R'
040700980127     C                   PARM                    TIBS69
040800980127     C                   PARM                    CNACO
040900980127     C                   PARM                    CNIND
041000980127     C                   PARM                    CNCLP
041100980127     C                   PARM                    FNCLS
041200980127     C     O69ERR        IFEQ      *BLANK
041300980127     C***        KBAN1     CHAINCNACO000             50
041400980127     C                   MOVEL     ACORAG        RBAN
041500910917     C                   END
041600980127     C                   END
041700910917     C* DATA PAGAMENTO
041800941026     C                   Z-ADD     CSVDDP        G02INV
041900941026     C                   Z-ADD     0             G02DAT
042000941026     C                   MOVEL     '3'           G02ERR
042100941026     C                   CALL      'XSRDA8'
042200941026     C                   PARM                    WLBDA8
042300941026     C                   Z-ADD     G02DAT        RDDP
042400941026     C* DATA REGISTRAZIONE PAGAMENTO
042500941026     C                   Z-ADD     CSVDRG        G02INV
042600941026     C                   Z-ADD     0             G02DAT
042700941026     C                   MOVEL     '3'           G02ERR
042800941026     C                   CALL      'XSRDA8'
042900941026     C                   PARM                    WLBDA8
043000941026     C                   Z-ADD     G02DAT        RDRG
043100941026     C                   Z-ADD     CSVNRG        RNRG
043200941026     C                   Z-ADD     CSVCMP        RCMP
043300910917     C* TIPO PAGAMENTO EFFETTUATO
043400910917     C                   MOVEL     CSVFPC        RFPC
043500910917     C* DATI EVENTUALE GIACENZA
043600910917     C                   Z-ADD     CSVNGC        RNGC
043700941026     C                   Z-ADD     CSVDGC        G02INV
043800941026     C                   Z-ADD     0             G02DAT
043900941026     C                   MOVEL     '3'           G02ERR
044000941026     C                   CALL      'XSRDA8'
044100941026     C                   PARM                    WLBDA8
044200941026     C                   Z-ADD     G02DAT        RDGC
044300910917     C* RAGGRUPPAMENTO CONTABILE
044400910917     C                   Z-ADD     CSVRGF        RRGF
044500910917     C* TIPO BOLLA
044600910917     C                   MOVEL     CSVTBL        RTBL
044700910917     C* IMPORTO C/A
044800941026     C                   Z-ADD     CSVCAS        RCAS
044900941028     C                   MOVEL     CSVVCA        RVCA
045000941028     C                   MOVEL     CSVGCA        RGCA
045100941028     C*
045200941028     C     PROMIO        COMP      *BLANKS                            10
045300941028     C     NAZMIO        COMP      *BLANKS                            11
045400941028     C     PROINT        COMP      *BLANKS                            14
045500941028     C     NAZINT        COMP      *BLANKS                            13
045600910906     C*
045700910902     C                   ENDSR
045800941026      *---------------------------------------------------------------*
045900941026      *  OPERAZIONI INIZIALI
046000941026      *---------------------------------------------------------------*
046100941026     C     *INZSR        BEGSR
046200941026     C*
046300941026     C     *ENTRY        PLIST
046400941026     C                   PARM                    KPJBA
046500950711     C                   MOVEL     KPJBU         TNSC94
046600941026      *
046700941026      * CHIAVI
046800941026     C     KTAB          KLIST
046900941026     C                   KFLD                    CODUT
047000941026     C                   KFLD                    CODTAB            2
047100941026     C                   KFLD                    KEYTAB            8
047200941026      *
047300941026     C     KSPE          KLIST
047400950711     C                   KFLD                    D94AAS
047500950711     C                   KFLD                    D94LNP
047600950711     C                   KFLD                    D94NRS
047700950711     C                   KFLD                    D94NSP
047800941026     C*
047900941026     C     KBLCAS        KLIST
048000941026     C                   KFLD                    RAAS
048100941026     C                   KFLD                    RLNP
048200941026     C                   KFLD                    RNRS
048300941026     C                   KFLD                    RNSP
048400941026     C*
048500941026     C     KBAN1         KLIST
048600941026     C                   KFLD                    CODUT
048700941026     C                   KFLD                    KBNA
048800941026     C                   KFLD                    RBNA
048900941026     C*
049000941026     C     KABI          KLIST
049100941026     C                   KFLD                    CSVABI
049200941026     C                   KFLD                    CSVCAB
049300941026      *---------------------------------------------------------------*
049400980127     C                   Z-ADD     1             CODUT             1 0
049500941026     C                   CALL      'X§PARUT'
049600941026     C                   PARM                    UT§DSE
049700941026     C*          CODUT     CABEQ-1        FINE
049800941026     C                   MOVEL     RAGUT         RSUT             20
049900941026     C                   MOVEL     REC80         CNCR80
050000941026     C                   MOVEL     KNMUS         §SEI              6
050100941026     C                   CLEAR                   WLBDA8
050200941026      *
050300941026      *  RICERCA CAPOCONTI
050400941026     C                   DO        50            X                 2 0
050500941026     C                   MOVE      TCU(X)        TCUDS
050600980127     C     F56           IFEQ      'CG'
050700941026     C     F4            IFEQ      '6'
050800941026     C                   Z-ADD     KCU(X)        KBNA              4 0
050900941026     C                   END
051000941026     C                   END
051100941026     C*
051200941026     C                   END
051300941026     C*
051400941026     C                   ENDSR
