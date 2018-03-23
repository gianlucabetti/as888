000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300010109      * TNSC97R                                                      *
000400910918      *            *-------------------------*                       *
000500910918      *             MANUTENZIONE CONTRASSEGNI                        *
000600020304      *                SOLO OMESSI STATO 8
000700910918      *            *-------------------------*                       *
000800010313      *           CHIUSURA  CON PAGAMENTO FITTIZIO                   *
000900900606?     *--------------------------------------------------------------*
001000010109     FTNSC97D   CF   E             WORKSTN
001100941025     FTNCSB03L  UF   E           K DISK
001200941025     FTNCSV00F  O    E             DISK
001300010109     FTNCSA01L  IF   E           K DISK
001400910909     FCNABI00F  IF   E           K DISK
001500941025     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001600910906     FTABEL00F  IF   E           K DISK
001700941025?     *--------------------------------------------------------------*
001800971219     D ERR             S             70    DIM(27) CTDATA PERRCD(1)
001900900515      *
002000120216     D tnsc30ds      E DS
002100120216     D CNCR80        E DS
002200900514     D KPJBA         E DS
002300910828     D  LIBSYS                92    100
002400910828     D  MSGERR               453    502
002500941025     D CSBDS         E DS                  EXTNAME(TNCSB00F)
002600941025     D               E DS                  EXTNAME(TNCSV00F)
002700941028     D  CSVDS                 16    229
002800900516      *
002900900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003000000000     D  TCU                  398    697
003100000000     D                                     DIM(50)
003200000000     D  KCU                  698    847P 0
003300000000     D                                     DIM(50)
003400000000     D                                     PACKEVEN
003500900606      *
003600900606     D TCUDS           DS
003700900606     D  F3                     3      3
003800900606     D  F4                     4      4
003900900606     D  F56                    5      6
004000900515      *
004100941025     D WLBDA8          DS
004200941025     D  G02DAT                 1      8  0
004300941025     D  G02INV                 9     16  0
004400941025     D  G02ERR                17     17
004500941025     D  G02TGI                18     22  0
004600971218     D*
004700010308     D*FTRUL15    E DSTRUL15DS
004800971218     D TIBS69        E DS                  EXTNAME(TIBS69DS)
004900971218     D CNACO         E DS                  EXTNAME(CNACO00F)
005000971218     D CNIND         E DS                  EXTNAME(CNIND00F)
005100971218     D CNCLP         E DS                  EXTNAME(CNCLP00F)
005200971218     D FNCLS         E DS                  EXTNAME(FNCLS00F)
005300910521      *
005400910918     D PAS01           DS
005500910918     D  FLAG                   1      1
005600960923     D DS4S          E DS
005700011019     D*DSCV          E DS
005800950711     D                SDS
005900950711     D  NOMPGM                 1     10
006000941025     I*--------------------------------------------------------------*
006100941025     IAZORG01L  AA
006200941025     I                             P    4    5 0ORGFIL
006300941025     I                                 14   33  ORGDES
006400910830      *---------------------------------------------------------------*
006500910917     C*
006600910916     C                   EXSR      PULIZ
006700910918     C     *INKC         DOWEQ     '0'
006800010109     C                   EXFMT     SC97D01
006900940413     C                   SETOFF                                       50
007000940413     C                   MOVEL     *BLANKS       $MSG
007100910930     C  NKC              DO
007200941026     C*  CONTROLLO SE L'ANNO E'NEL FORMATO 2 CHR E SE SI LO CONVERTO
007300950711     C                   MOVEL     V1CAAS        WAA2              2 0
007400941026     C     WAA2          IFEQ      0
007500950711     C     V1CAAS        IFGT      60
007600950711     C                   ADD       1900          V1CAAS
007700941026     C                   ELSE
007800950711     C                   ADD       2000          V1CAAS
007900941026     C                   END
008000941026     C                   END
008100941026     C*
008200950727     C     KSPE          CHAIN     TNCSB000                           5060
008300940413     C   50              MOVEL     ERR(1)        $MSG
008400971218     C     *IN50         IFEQ      *OFF
008500980225     C     *IN60         ANDEQ     *OFF
008600971218     C                   ENDIF
008700950727     C  N50*IN60         IFEQ      '1'
008800010109     C                   MOVEL     ERR(02)       $MSG
008900950727     C                   SETON                                        50
009000971218     C                   END
009100020304     C  N50CSBSTA        IFNE      8
009200010109     C                   MOVEL     ERR(03)       $MSG
009300010109     C                   SETON                                        50
009400010109     C                   END
009500010112     C  N50CSBDDP        IFNE      0
009600010112     C                   MOVEL     ERR(04)       $MSG
009700010112     C                   SETON                                        50
009800010112     C                   END
009900940413     C  N50              EXSR      VISUAL
010000120216     c* abilito comando per multiassegno
010100120220     c                   if        %subst(csbnra: 10: 1)=' ' or csbtpi <> 'M'
010200120220     c                   setoff                                       44
010300120216     c                   else
010400120216     c                   seton                                        44
010500120216     c                   end
010600010109     C*
010700910930     C     *INKL         DOWEQ     '0'
010800910919     C     *INKF         ANDEQ     '0'
010900910919     C     *IN50         ANDEQ     '0'
011000950711     C     *IN28         OREQ      '1'
011100010109     C                   EXFMT     SC97D02
011200120216     c* interroga multiassegni
011300120216     c                   if        *inki
011400120216     c                   clear                   tnsc30ds
011500120216     c                   eval      sc30CMTI = 'N'
011600120216     c                   eval      sc30AMBI = 'S'
011700120216     c                   eval      sc30TPCI = 'V'
011800120216     c                   eval      sc30AASI = v1caas
011900120216     c                   eval      sc30LNPI = v1clnp
012000120216     c                   eval      sc30NRSI = v1cnrs
012100120216     c                   eval      sc30NSPI = v1cnsp
012200120216     c                   movel(p)  tnsc30ds      kpjbu
012300120216     c                   call      'TNSC30R'
012400120216     c                   parm                    kpjba
012500120216     c                   end
012600010109     C  NKL
012700010109     CAN KF              EXSR      AGGIO
012800950711     C   KL              SETOFF                                       28
012900910902     C                   END
013000910917     C                   END
013100910917     C                   END
013200971218     C*
013300971218     C* RICHIAMO PGM X CHIUSURA FILES
013400971218     C                   CLEAR                   TIBS69
013500971218     C                   MOVEL     'C'           I69TLA
013600971218     C                   CALL      'TIBS69R'
013700971218     C                   PARM                    TIBS69
013800971218     C                   PARM                    CNACO
013900971218     C                   PARM                    CNIND
014000971218     C                   PARM                    CNCLP
014100971218     C                   PARM                    FNCLS
014200910521     C                   SETON                                        LR
014300910521     C*
014400900515      *-----------------------------------------------------***********
014500910918      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
014600900515      *-----------------------------------------------------***********
014700910918     C     PULIZ         BEGSR
014800950711     C                   Z-ADD     0             V1CLNP
014900950711     C                   Z-ADD     ANNO          V1CAAS
015000950711     C                   Z-ADD     0             V1CNRS
015100950711     C                   Z-ADD     0             V1CNSP
015200910918     C                   ENDSR
015300010109     C*----------------------------------------------------------
015400910828     C     VISUAL        BEGSR
015500010109     C*----------------------------------------------------------
015600010110     C                   SETOFF                                       28
015700971219     C                   CLEAR                   V1DINT
015800971219     C                   CLEAR                   V1CVII
015900971219     C                   CLEAR                   V1CCIT
016000971219     C                   CLEAR                   V1CCAI
016100971219     C                   CLEAR                   V1CNZI
016200971219     C                   CLEAR                   V1CPRI
016300910917     C* STATO DEL C/A
016400950710     C                   Z-ADD     CSBSTA        V1CSTA
016500910917     C                   MOVEL     '4S'          CODTAB
016600941108     C                   MOVEL     *BLANK        KEYTAB
016700950710     C                   MOVE      V1CSTA        KEYTAB
016800960923     C                   MOVEL     *BLANKS       WMOD
016900910930     C     KTAB          CHAIN     TABEL                              70
017000950710     C  N70              MOVEL     TBLUNI        V1DSTA
017100960923     C  N70              MOVEL     TBLUNI        DS4S
017200960923     C  N70              MOVEL     §4SMOD        WMOD              1
017300950710     C   70              MOVEL     *BLANKS       V1DSTA
017400910917     C* LINEA DI PARTENZA + DECODIFICA
017500950710     C                   Z-ADD     CSBLNP        V1CLNP
017600950710     C     V1CLNP        CHAIN     AZORG01L                           70
017700950710     C  N70              MOVEL     ORGDES        V1DLNP
017800950710     C   70              MOVEL     *BLANKS       V1DLNP
017900910917     C* NR° SERIE + SPEDIZIONE
018000950710     C                   Z-ADD     CSBNRS        V1CNRS
018100950710     C                   Z-ADD     CSBNSP        V1CNSP
018200910917     C* CODICE CLIENTE
018300950710     C                   Z-ADD     CSBKSC        V1CKSC
018400910917     C* DATA SPEDIZIONE
018500941028     C                   CLEAR                   WLBDA8
018600941025     C                   MOVEL     CSBAAS        G02INV
018700941028     C                   MOVE      CSBMGS        G02INV
018800941025     C                   Z-ADD     0             G02DAT
018900941025     C                   MOVEL     '3'           G02ERR
019000941025     C                   CALL      'XSRDA8'
019100941025     C                   PARM                    WLBDA8
019200950710     C                   Z-ADD     G02DAT        V1CDSP
019300910917     C* DATA DISTINTA
019400941025     C                   Z-ADD     CSBDDC        G02INV
019500941025     C                   Z-ADD     0             G02DAT
019600941025     C                   MOVEL     '3'           G02ERR
019700941025     C                   CALL      'XSRDA8'
019800941025     C                   PARM                    WLBDA8
019900950710     C                   Z-ADD     G02DAT        V1CDDC
020000910917     C* LINEA ARRIVO + DECODIFICA
020100950710     C                   Z-ADD     CSBLNA        V1CLNA
020200941025     C                   Z-ADD     CSBLNA        ORGFIL
020300941025     C     ORGFIL        CHAIN     AZORG01L                           70
020400950710     C  N70              MOVEL     ORGDES        V1DLNA
020500950710     C   70              MOVEL     *BLANKS       V1DLNA
020600910917     C* NUMERO DISTINTA
020700950710     C                   Z-ADD     CSBNDT        V1CNDT
020800910917     C* RIFERIMENTO MITTENTE
020900950710     C                   Z-ADD     CSBRMN        V1CRMN
021000950710     C                   MOVEL     CSBRMA        V1CRMA
021100910917     C* DATA ARRIVO DISTINTA
021200941025     C                   Z-ADD     CSBDTA        G02INV
021300941025     C                   Z-ADD     0             G02DAT
021400941025     C                   MOVEL     '3'           G02ERR
021500941025     C                   CALL      'XSRDA8'
021600941025     C                   PARM                    WLBDA8
021700950710     C                   Z-ADD     G02DAT        V1CDTA
021800910917     C* NR° ASSEGNO = RIFERIM. INCASSO
021900950710     C                   MOVEL     CSBNRA        V1CNRA
022000910917     C* DATA ASSEGNO
022100941025     C                   Z-ADD     CSBDTE        G02INV
022200941025     C                   Z-ADD     0             G02DAT
022300941025     C                   MOVEL     '3'           G02ERR
022400941025     C                   CALL      'XSRDA8'
022500941025     C                   PARM                    WLBDA8
022600950710     C                   Z-ADD     G02DAT        V1CDTE
022700910917     C* TIPO INTESTAZIONE ASSEGNO
022800950710     C                   MOVEL     CSBTPI        V1CTPI
022900950710     C                   MOVEL     CSBTPA        V1CTPA
023000950711     C                   MOVEL     CSBFUS        V1CFUS
023100950711     C                   MOVEL     CSBMAS        V1CMAS
023200910917     C* CODICE INTESTAZIONE ASS.
023300950710     C                   Z-ADD     CSBCDI        V1CCDI
023400910917     C* ABI / CAB / EVENTUALE DECODIFICA BANCA
023500950710     C                   Z-ADD     CSBABI        V1CABI
023600950710     C                   Z-ADD     CSBCAB        V1CCAB
023700910917     C     KABI          CHAIN     CNABI000                           75
023800950710     C  N75              MOVEL     ABIIST        V1CDES
023900950710     C   75              MOVEL     *BLANKS       V1CDES
024000941025     C* DATA REGISTRAZIONE INCASSO
024100941025     C                   Z-ADD     CSBDRI        G02INV
024200941025     C                   Z-ADD     0             G02DAT
024300941026     C                   MOVEL     '3'           G02ERR
024400941025     C                   CALL      'XSRDA8'
024500941025     C                   PARM                    WLBDA8
024600950710     C                   Z-ADD     G02DAT        V1CDRI
024700941025     C* NUMERO REG.INCASSO/CAMBIO
024800950710     C                   Z-ADD     CSBNRI        V1CNRI
024900950710     C                   Z-ADD     CSBCMB        V1CCMB
025000910917     C* INTESTAZIONE ASSEGNO / MITTENTE
025100971218     C                   CLEAR                   TNCSA000
025200971218     C* SE COD. INTESTAZ. ASSEGNO <> COD. CLIENTE DECODIFICO  CDI
025300971218  B1 C     V1CCDI        IFNE      V1CKSC
025400971218      *  SE CDI è = 8888 / 9999  PRENDO I DATI DA TNCSA
025500971219      *  (CASO POCO PROBABILE IN QUANTO IL CODICE INTESTAZ. NON DOVREBBE
025600971219      *   ESSERE=8888 O 9999 MENTRE IL KSC è UN CODIFICATO)
025700971218     C                   MOVE      V1CCDI        §8899             4 0
025800971218  B2 C     §8899         IFEQ      8888
025900971218     C     §8899         OREQ      9999
026000971218     C     KBLCAS        CHAIN     TNCSA000                           70
026100971218  B3 C     *IN70         IFEQ      *OFF
026200971219     C                   SETON                                        71
026300971218     C                   MOVEL     CSARSD        V1DINT
026400971218     C                   MOVEL     CSAVID        V1CVII
026500971218     C                   MOVEL     CSACAD        V1CCAI
026600971218     C                   MOVEL     CSACID        V1CCIT
026700971218     C                   MOVEL     CSANZM        V1CNZI
026800971218     C                   MOVEL     CSAPRM        V1CPRI
026900971218  E3 C                   ENDIF
027000971218  X2 C                   ELSE
027100971218      * ALTRIMENTI è UN CODIFICATO
027200971218      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE INTESTAZ. ASSEGNO
027300971218     C                   CLEAR                   TIBS69
027400971218     C                   CLEAR                   CNACO
027500971218     C                   CLEAR                   CNIND
027600971218     C                   CLEAR                   CNCLP
027700971218     C                   CLEAR                   FNCLS
027800971218     C                   MOVEL     ' '           I69TLA
027900971218     C                   MOVEL     KNSIF         I69SIF
028000971218     C                   Z-ADD     V1CCDI        I69KAC
028100971219     C                   Z-ADD     V1CCDI        I69KIN
028200971218     C                   CALL      'TIBS69R'
028300971218     C                   PARM                    TIBS69
028400971218     C                   PARM                    CNACO
028500971218     C                   PARM                    CNIND
028600971218     C                   PARM                    CNCLP
028700971218     C                   PARM                    FNCLS
028800971218  B3 C     O69ERR        IFEQ      *BLANK
028900971218     C                   MOVEL     ACORAG        V1DINT
029000971218     C                   MOVEL     INDVIA        V1CVII
029100971218     C                   MOVEL     *BLANKS       V1CCAI
029200971218  B4 C     INDCAP        IFNE      *ZEROS
029300971218     C                   MOVEL     INDCAP        V1CCAI
029400971218  E4 C                   END
029500971218     C                   MOVEL     INDSTA        V1CNZI
029600971218     C                   MOVEL     INDPRV        V1CPRI
029700971218  B4 C     INDCAE        IFNE      *ZEROS
029800971218     C     INDCAE        ANDNE     *BLANKS
029900971218     C                   MOVEL     INDCAE        V1CCAI
030000971218  E4 C                   END
030100971218     C                   MOVEL     INDCIT        V1CCIT
030200971218  E3 C                   ENDIF
030300971218  E2 C                   ENDIF
030400971218  E1 C                   ENDIF
030500971218     C* IN OGNI CASO DECODIFICO KSC
030600971218      *  SE KSC è = 8888 / 9999  PRENDO I DATI DA TNCSA
030700971218     C                   MOVE      V1CKSC        §8899
030800971218  B1 C     §8899         IFEQ      8888
030900971218     C     §8899         OREQ      9999
031000971126     C     KBLCAS        CHAIN     TNCSA000                           70
031100971218  B2 C     *IN70         IFEQ      *OFF
031200971219     C                   SETON                                        71
031300950710     C                   MOVEL     CSARSD        V1DMIO
031400950710     C                   MOVEL     CSAVID        V1CVIM
031500950710     C                   MOVEL     CSACAD        V1CCAM
031600950710     C                   MOVEL     CSACID        V1CLOM
031700950710     C                   MOVEL     CSANZM        V1CNZM
031800950710     C                   MOVEL     CSAPRM        V1CPRM
031900971218  E2 C                   ENDIF
032000971218  X1 C                   ELSE
032100971218      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
032200971218     C                   CLEAR                   TIBS69
032300971218     C                   CLEAR                   CNACO
032400971218     C                   CLEAR                   CNIND
032500971218     C                   CLEAR                   CNCLP
032600971218     C                   CLEAR                   FNCLS
032700971218     C                   MOVEL     ' '           I69TLA
032800971218     C                   MOVEL     KNSIF         I69SIF
032900971218     C                   Z-ADD     V1CKSC        I69KAC
033000971219     C                   Z-ADD     V1CKSC        I69KIN
033100971218     C                   CALL      'TIBS69R'
033200971218     C                   PARM                    TIBS69
033300971218     C                   PARM                    CNACO
033400971218     C                   PARM                    CNIND
033500971218     C                   PARM                    CNCLP
033600971218     C                   PARM                    FNCLS
033700971218  B2 C     O69ERR        IFEQ      *BLANK
033800950710     C                   MOVEL     ACORAG        V1DMIO
033900950710     C                   MOVEL     INDVIA        V1CVIM
034000950710     C                   MOVEL     *BLANKS       V1CCAM
034100971218  B3 C     INDCAP        IFNE      *ZEROS
034200950710     C                   MOVEL     INDCAP        V1CCAM
034300971218  E3 C                   END
034400950710     C                   MOVEL     INDSTA        V1CNZM
034500950710     C                   MOVEL     INDPRV        V1CPRM
034600971218  B3 C     INDCAE        IFNE      *ZEROS
034700941025     C     INDCAE        ANDNE     *BLANKS
034800950710     C                   MOVEL     INDCAE        V1CCAM
034900971218  E3 C                   END
035000950710     C                   MOVEL     INDCIT        V1CLOM
035100971218  E2 C                   END
035200971218  E1 C                   ENDIF
035300910917     C* DESTINATARIO
035400950710     C                   MOVEL     CSBRSD        V1CRSD
035500950710     C                   MOVEL     CSBPAR        V1CPAR
035600950710     C                   MOVEL     CSBPRO        V1CPRO
035700910917     C* NR° DOCUMENTO PAGAMENTO
035800950710     C                   Z-ADD     CSBNDP        V1CNDP
035900910917     C* BANCA PAGAMENTO - DECODIFICA SOLO SE COD. <> 0
036000950710     C                   MOVEL     *BLANKS       V1DBNA
036100950710     C                   Z-ADD     CSBBNA        V1CBNA
036200980127     C     CSBBNA        IFNE      0
036300980127     C                   CLEAR                   TIBS69
036400980127     C                   CLEAR                   CNACO
036500980127     C                   CLEAR                   CNIND
036600980127     C                   CLEAR                   CNCLP
036700980127     C                   CLEAR                   FNCLS
036800980127     C                   MOVEL     ' '           I69TLA
036900980127     C                   MOVEL     KNSIF         I69SIF
037000980127     C                   Z-ADD     KBNA          I69KCC
037100980127     C                   Z-ADD     CSBBNA        I69KAC
037200980127     C                   MOVEL     *BLANKS       ACORAG
037300980127     C                   CALL      'TIBS69R'
037400980127     C                   PARM                    TIBS69
037500980127     C                   PARM                    CNACO
037600980127     C                   PARM                    CNIND
037700980127     C                   PARM                    CNCLP
037800980127     C                   PARM                    FNCLS
037900980127     C     O69ERR        IFEQ      *BLANK
038000980127     C                   MOVEL     ACORAG        V1DBNA
038100910917     C                   END
038200980127     C                   END
038300910917     C* DATA PAGAMENTO
038400931203     C     CSBDDP        COMP      0                                      76
038500941025     C                   Z-ADD     CSBDDP        G02INV
038600941025     C                   Z-ADD     0             G02DAT
038700941026     C                   MOVEL     '3'           G02ERR
038800941025     C                   CALL      'XSRDA8'
038900941025     C                   PARM                    WLBDA8
039000950710     C                   Z-ADD     G02DAT        V1CDDP
039100941025     C* DATA REGISTRAZIONE PAGAMENTO
039200941025     C                   Z-ADD     CSBDRG        G02INV
039300941025     C                   Z-ADD     0             G02DAT
039400941026     C                   MOVEL     '3'           G02ERR
039500941025     C                   CALL      'XSRDA8'
039600941025     C                   PARM                    WLBDA8
039700950710     C                   Z-ADD     G02DAT        V1CDRG
039800950710     C                   Z-ADD     CSBNRG        V1CNRG
039900950710     C                   Z-ADD     CSBCMP        V1CCMP
040000910917     C* TIPO PAGAMENTO EFFETTUATO
040100950710     C                   MOVEL     CSBFPC        V1CFPC
040200910917     C* DATI EVENTUALE GIACENZA
040300950710     C                   Z-ADD     CSBNGC        V1CNGC
040400941025     C                   Z-ADD     CSBDGC        G02INV
040500941025     C                   Z-ADD     0             G02DAT
040600941026     C                   MOVEL     '3'           G02ERR
040700941025     C                   CALL      'XSRDA8'
040800941025     C                   PARM                    WLBDA8
040900950710     C                   Z-ADD     G02DAT        V1CDGC
041000910917     C* RAGGRUPPAMENTO CONTABILE
041100950710     C                   Z-ADD     CSBRGF        V1CRGF
041200910917     C* TIPO BOLLA
041300950710     C                   MOVEL     CSBTBL        V1CTBL
041400910917     C* IMPORTO C/A
041500950710     C                   Z-ADD     CSBCAS        V1CCAS
041600950710     C                   MOVEL     CSBVCA        V1CVCA
041700941028     C* FLAG PARTICOLARITA'
041800950710     C                   MOVEL     CSBGCA        V1CGCA
041900910920     C*
042000910902     C                   ENDSR
042100940413     C*-------------------------------------------------------------------*
042200910918     C     AGGIO         BEGSR
042300010109     C*-------------------------------------------------------------------*
042400010110     C                   SETOFF                                       28
042500940413     C                   MOVEL     *BLANKS       $MSG
042600931203     C*
042700950711     C  N28
042800950711     CAN KF              DO
042900910919     C* SE O.K. SCRIVE SITUAZIONE PRECEDENTE NELLO STORICO E AGGIORNA
043000971008     C* MUOVO I CAMPI DI TNCSB  IN BLCSV
043100971008     C                   MOVEL     CSBDS         CSVDS
043200971008      * Imposto nuovi valori
043300010112     C     CSBUDA        IFEQ      0
043400010110     C                   Z-ADD     DATEU         CSBUDA
043500010112     C                   ENDIF
043600010112     C     CSBDTA        IFEQ      0
043700010110     C                   Z-ADD     DATEU         CSBDTA
043800010112     C                   ENDIF
043900010112     C     CSBDDC        IFEQ      0
044000010110     C                   Z-ADD     DATEU         CSBDDC
044100010112     C                   ENDIF
044200010112     C     CSBNDT        IFEQ      0
044300010109     C                   Z-ADD     7777777       CSBNDT
044400010112     C                   ENDIF
044500010112     C     CSBNRI        IFEQ      0
044600010109     C                   Z-ADD     0             CSBNRI
044700010112     C                   ENDIF
044800010112     C     CSBDRI        IFEQ      0
044900010109     C                   Z-ADD     0             CSBDRI
045000010112     C                   ENDIF
045100010109     C                   Z-ADD     9999999       CSBBNA
045200010109     C                   Z-ADD     7777777       CSBNDP
045300010109     C                   Z-ADD     DATEU         CSBDDP
045400010109     C                   Z-ADD     0             CSBNRG
045500010109     C                   Z-ADD     0             CSBDRG
045600010109     C                   MOVEL     *BLANK        CSBFTR
045700010109     C                   Z-ADD     0             CSBDTR
045800010109     C                   Z-ADD     0             CSBSTA
045900010112     C     CSBMAS        IFEQ      *BLANK
046000010109     C                   MOVEL     'S'           CSBMAS
046100010112     C                   END
046200010109     C     CSBDS         IFNE      CSVDS
046300010109     C                   EXSR      STORY
046400010109     C                   END
046500910918     C                   EXCEPT    AGG
046600971009     C                   END
046700971008     C*
046800010109     C                   ENDSR
046900010109     C*----------------------------------------------------------------
047000910919     C     STORY         BEGSR
047100010109     C*----------------------------------------------------------------
047200910919     C* IMPOSTO I CAMPI NON COMUNI
047300950710     C     V1CSTA        IFEQ      9
047400910919     C                   MOVE      'ANCA'        CSVCAV
047500910919     C                   ELSE
047600050922     C                   MOVE      'PRES'        CSVCAV
047700910919     C                   END
047800010109     C                   Z-ADD     DATEU         CSVDTV
047900910919     C                   TIME                    CSVORV
048000910919     C                   Z-ADD     046           CSVFEV
048100941025     C                   WRITE     TNCSV000
048200910919     C                   ENDSR
048300910930     C*
048400941025      *-----------------------------------------------------*
048500941025      * OPERAZIONI INIZIALI                                 *
048600941025      *-----------------------------------------------------*
048700940413     C     *INZSR        BEGSR
048800940413      *
048900940413      * CHIAVI
049000940413     C     KTAB          KLIST
049100940413     C                   KFLD                    CODUT
049200940413     C                   KFLD                    CODTAB            2
049300940413     C                   KFLD                    KEYTAB            8
049400940413      *
049500940413     C     KSPE          KLIST
049600950711     C                   KFLD                    V1CAAS
049700950711     C                   KFLD                    V1CLNP
049800950711     C                   KFLD                    V1CNRS
049900950711     C                   KFLD                    V1CNSP
050000940413     C*
050100940413     C     KBLCAS        KLIST
050200950710     C                   KFLD                    V1CAAS
050300950710     C                   KFLD                    V1CLNP
050400950710     C                   KFLD                    V1CNRS
050500950710     C                   KFLD                    V1CNSP
050600940413     C*
050700940413     C     KBAN1         KLIST
050800940413     C                   KFLD                    CODUT
050900940413     C                   KFLD                    KBNA
051000950710     C                   KFLD                    V1CBNA
051100940413     C*
051200940413     C     KABI          KLIST
051300940413     C                   KFLD                    CSBABI
051400940413     C                   KFLD                    CSBCAB
051500940413      *---------------------------------------------------------------*
051600940413     C     *ENTRY        PLIST
051700940413     C                   PARM                    KPJBA
051800940413     C                   MOVEL     KPJBU         PAS01
051900940413      *
052000940413     C                   Z-ADD     1             CODUT
052100940413     C                   CALL      'X§PARUT'
052200940413     C                   PARM                    UT§DSE
052300940413     C*          CODUT     CABEQ-1        FINE
052400940413     C                   MOVEL     RAGUT         RSUT             20
052500940413     C                   MOVEL     REC80         CNCR80
052600940413     C                   MOVEL     KNMUS         §SEI              6
052700940413      *
052800940413      *  RICERCA CAPOCONTI
052900940413     C                   DO        50            X                 2 0
053000940413     C                   MOVE      TCU(X)        TCUDS
053100940413     C     F56           IFEQ      'CG'
053200940413     C     F4            IFEQ      '6'
053300940413     C                   Z-ADD     KCU(X)        KBNA              4 0
053400940413     C                   END
053500940413     C                   END
053600940413     C*
053700940413     C                   END
053800940413     C*
053900941103     C                   TIME                    WHHDAT           14 0
054000941103     C                   MOVE      WHHDAT        ANNO              4 0
054100010109     C                   MOVE      WHHDAT        G02DAT
054200010109     C                   Z-ADD     0             G02INV
054300010109     C                   CALL      'XSRDA8'
054400010109     C                   PARM                    WLBDA8
054500010109     C                   Z-ADD     G02INV        DATEU             8 0
054600940413     C                   ENDSR
054700900606      *----------------------------------------------------------------
054800941025     OTNCSB000  E            AGG
054900010109     O                       CSBUDA
055000010109     O                       CSBDTA
055100010109     O                       CSBDDC
055200010109     O                       CSBNDT
055300010109     O                       CSBNRI
055400010109     O                       CSBDRI
055500010109     O                       CSBBNA
055600010109     O                       CSBNDP
055700010109     O                       CSBDDP
055800010109     O                       CSBNRG
055900010109     O                       CSBDRG
056000010109     O                       CSBFTR
056100010109     O                       CSBDTR
056200010109     O                       CSBSTA
056300010109     O                       CSBMAS
056400010112** ERR
056500940413Numero di spedizione errato                                           01
056600010109C/Ass. non disponibile per effettuare variazione. Attendi 5' e ritenta02
056700020304STATO C/ASS NON CORRETTO : è ammesso solo lo stato  8=omesso          03
056800010112Contrassegno già pagato : non utilizzabile quetsa funzione            04
