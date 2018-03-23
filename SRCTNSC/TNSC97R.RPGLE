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
002000910708     D CNCR80        E DS
002100900514     D KPJBA         E DS
002200910828     D  LIBSYS                92    100
002300910828     D  MSGERR               453    502
002400941025     D CSBDS         E DS                  EXTNAME(TNCSB00F)
002500941025     D               E DS                  EXTNAME(TNCSV00F)
002600941028     D  CSVDS                 16    229
002700900516      *
002800900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002900000000     D  TCU                  398    697
003000000000     D                                     DIM(50)
003100000000     D  KCU                  698    847P 0
003200000000     D                                     DIM(50)
003300000000     D                                     PACKEVEN
003400900606      *
003500900606     D TCUDS           DS
003600900606     D  F3                     3      3
003700900606     D  F4                     4      4
003800900606     D  F56                    5      6
003900900515      *
004000941025     D WLBDA8          DS
004100941025     D  G02DAT                 1      8  0
004200941025     D  G02INV                 9     16  0
004300941025     D  G02ERR                17     17
004400941025     D  G02TGI                18     22  0
004500971218     D*
004600010308     D*FTRUL15    E DSTRUL15DS
004700971218     D TIBS69        E DS                  EXTNAME(TIBS69DS)
004800971218     D CNACO         E DS                  EXTNAME(CNACO00F)
004900971218     D CNIND         E DS                  EXTNAME(CNIND00F)
005000971218     D CNCLP         E DS                  EXTNAME(CNCLP00F)
005100971218     D FNCLS         E DS                  EXTNAME(FNCLS00F)
005200910521      *
005300910918     D PAS01           DS
005400910918     D  FLAG                   1      1
005500960923     D DS4S          E DS
005600011019     D*DSCV          E DS
005700950711     D                SDS
005800950711     D  NOMPGM                 1     10
005900941025     I*--------------------------------------------------------------*
006000941025     IAZORG01L  AA
006100941025     I                             P    4    5 0ORGFIL
006200941025     I                                 14   33  ORGDES
006300910830      *---------------------------------------------------------------*
006400910917     C*
006500910916     C                   EXSR      PULIZ
006600910918     C     *INKC         DOWEQ     '0'
006700010109     C                   EXFMT     SC97D01
006800940413     C                   SETOFF                                       50
006900940413     C                   MOVEL     *BLANKS       $MSG
007000910930     C  NKC              DO
007100941026     C*  CONTROLLO SE L'ANNO E'NEL FORMATO 2 CHR E SE SI LO CONVERTO
007200950711     C                   MOVEL     V1CAAS        WAA2              2 0
007300941026     C     WAA2          IFEQ      0
007400950711     C     V1CAAS        IFGT      60
007500950711     C                   ADD       1900          V1CAAS
007600941026     C                   ELSE
007700950711     C                   ADD       2000          V1CAAS
007800941026     C                   END
007900941026     C                   END
008000941026     C*
008100950727     C     KSPE          CHAIN     TNCSB000                           5060
008200940413     C   50              MOVEL     ERR(1)        $MSG
008300971218     C     *IN50         IFEQ      *OFF
008400980225     C     *IN60         ANDEQ     *OFF
008500971218     C                   ENDIF
008600950727     C  N50*IN60         IFEQ      '1'
008700010109     C                   MOVEL     ERR(02)       $MSG
008800950727     C                   SETON                                        50
008900971218     C                   END
009000020304     C  N50CSBSTA        IFNE      8
009100010109     C                   MOVEL     ERR(03)       $MSG
009200010109     C                   SETON                                        50
009300010109     C                   END
009400010112     C  N50CSBDDP        IFNE      0
009500010112     C                   MOVEL     ERR(04)       $MSG
009600010112     C                   SETON                                        50
009700010112     C                   END
009800940413     C  N50              EXSR      VISUAL
009900010109     C*
010000910930     C     *INKL         DOWEQ     '0'
010100910919     C     *INKF         ANDEQ     '0'
010200910919     C     *IN50         ANDEQ     '0'
010300950711     C     *IN28         OREQ      '1'
010400010109     C                   EXFMT     SC97D02
010500010109     C  NKL
010600010109     CAN KF              EXSR      AGGIO
010700950711     C   KL              SETOFF                                       28
010800910902     C                   END
010900910917     C                   END
011000910917     C                   END
011100971218     C*
011200971218     C* RICHIAMO PGM X CHIUSURA FILES
011300971218     C                   CLEAR                   TIBS69
011400971218     C                   MOVEL     'C'           I69TLA
011500971218     C                   CALL      'TIBS69R'
011600971218     C                   PARM                    TIBS69
011700971218     C                   PARM                    CNACO
011800971218     C                   PARM                    CNIND
011900971218     C                   PARM                    CNCLP
012000971218     C                   PARM                    FNCLS
012100910521     C                   SETON                                        LR
012200910521     C*
012300900515      *-----------------------------------------------------***********
012400910918      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
012500900515      *-----------------------------------------------------***********
012600910918     C     PULIZ         BEGSR
012700950711     C                   Z-ADD     0             V1CLNP
012800950711     C                   Z-ADD     ANNO          V1CAAS
012900950711     C                   Z-ADD     0             V1CNRS
013000950711     C                   Z-ADD     0             V1CNSP
013100910918     C                   ENDSR
013200010109     C*----------------------------------------------------------
013300910828     C     VISUAL        BEGSR
013400010109     C*----------------------------------------------------------
013500010110     C                   SETOFF                                       28
013600971219     C                   CLEAR                   V1DINT
013700971219     C                   CLEAR                   V1CVII
013800971219     C                   CLEAR                   V1CCIT
013900971219     C                   CLEAR                   V1CCAI
014000971219     C                   CLEAR                   V1CNZI
014100971219     C                   CLEAR                   V1CPRI
014200910917     C* STATO DEL C/A
014300950710     C                   Z-ADD     CSBSTA        V1CSTA
014400910917     C                   MOVEL     '4S'          CODTAB
014500941108     C                   MOVEL     *BLANK        KEYTAB
014600950710     C                   MOVE      V1CSTA        KEYTAB
014700960923     C                   MOVEL     *BLANKS       WMOD
014800910930     C     KTAB          CHAIN     TABEL                              70
014900950710     C  N70              MOVEL     TBLUNI        V1DSTA
015000960923     C  N70              MOVEL     TBLUNI        DS4S
015100960923     C  N70              MOVEL     §4SMOD        WMOD              1
015200950710     C   70              MOVEL     *BLANKS       V1DSTA
015300910917     C* LINEA DI PARTENZA + DECODIFICA
015400950710     C                   Z-ADD     CSBLNP        V1CLNP
015500950710     C     V1CLNP        CHAIN     AZORG01L                           70
015600950710     C  N70              MOVEL     ORGDES        V1DLNP
015700950710     C   70              MOVEL     *BLANKS       V1DLNP
015800910917     C* NR° SERIE + SPEDIZIONE
015900950710     C                   Z-ADD     CSBNRS        V1CNRS
016000950710     C                   Z-ADD     CSBNSP        V1CNSP
016100910917     C* CODICE CLIENTE
016200950710     C                   Z-ADD     CSBKSC        V1CKSC
016300910917     C* DATA SPEDIZIONE
016400941028     C                   CLEAR                   WLBDA8
016500941025     C                   MOVEL     CSBAAS        G02INV
016600941028     C                   MOVE      CSBMGS        G02INV
016700941025     C                   Z-ADD     0             G02DAT
016800941025     C                   MOVEL     '3'           G02ERR
016900941025     C                   CALL      'XSRDA8'
017000941025     C                   PARM                    WLBDA8
017100950710     C                   Z-ADD     G02DAT        V1CDSP
017200910917     C* DATA DISTINTA
017300941025     C                   Z-ADD     CSBDDC        G02INV
017400941025     C                   Z-ADD     0             G02DAT
017500941025     C                   MOVEL     '3'           G02ERR
017600941025     C                   CALL      'XSRDA8'
017700941025     C                   PARM                    WLBDA8
017800950710     C                   Z-ADD     G02DAT        V1CDDC
017900910917     C* LINEA ARRIVO + DECODIFICA
018000950710     C                   Z-ADD     CSBLNA        V1CLNA
018100941025     C                   Z-ADD     CSBLNA        ORGFIL
018200941025     C     ORGFIL        CHAIN     AZORG01L                           70
018300950710     C  N70              MOVEL     ORGDES        V1DLNA
018400950710     C   70              MOVEL     *BLANKS       V1DLNA
018500910917     C* NUMERO DISTINTA
018600950710     C                   Z-ADD     CSBNDT        V1CNDT
018700910917     C* RIFERIMENTO MITTENTE
018800950710     C                   Z-ADD     CSBRMN        V1CRMN
018900950710     C                   MOVEL     CSBRMA        V1CRMA
019000910917     C* DATA ARRIVO DISTINTA
019100941025     C                   Z-ADD     CSBDTA        G02INV
019200941025     C                   Z-ADD     0             G02DAT
019300941025     C                   MOVEL     '3'           G02ERR
019400941025     C                   CALL      'XSRDA8'
019500941025     C                   PARM                    WLBDA8
019600950710     C                   Z-ADD     G02DAT        V1CDTA
019700910917     C* NR° ASSEGNO = RIFERIM. INCASSO
019800950710     C                   MOVEL     CSBNRA        V1CNRA
019900910917     C* DATA ASSEGNO
020000941025     C                   Z-ADD     CSBDTE        G02INV
020100941025     C                   Z-ADD     0             G02DAT
020200941025     C                   MOVEL     '3'           G02ERR
020300941025     C                   CALL      'XSRDA8'
020400941025     C                   PARM                    WLBDA8
020500950710     C                   Z-ADD     G02DAT        V1CDTE
020600910917     C* TIPO INTESTAZIONE ASSEGNO
020700950710     C                   MOVEL     CSBTPI        V1CTPI
020800950710     C                   MOVEL     CSBTPA        V1CTPA
020900950711     C                   MOVEL     CSBFUS        V1CFUS
021000950711     C                   MOVEL     CSBMAS        V1CMAS
021100910917     C* CODICE INTESTAZIONE ASS.
021200950710     C                   Z-ADD     CSBCDI        V1CCDI
021300910917     C* ABI / CAB / EVENTUALE DECODIFICA BANCA
021400950710     C                   Z-ADD     CSBABI        V1CABI
021500950710     C                   Z-ADD     CSBCAB        V1CCAB
021600910917     C     KABI          CHAIN     CNABI000                           75
021700950710     C  N75              MOVEL     ABIIST        V1CDES
021800950710     C   75              MOVEL     *BLANKS       V1CDES
021900941025     C* DATA REGISTRAZIONE INCASSO
022000941025     C                   Z-ADD     CSBDRI        G02INV
022100941025     C                   Z-ADD     0             G02DAT
022200941026     C                   MOVEL     '3'           G02ERR
022300941025     C                   CALL      'XSRDA8'
022400941025     C                   PARM                    WLBDA8
022500950710     C                   Z-ADD     G02DAT        V1CDRI
022600941025     C* NUMERO REG.INCASSO/CAMBIO
022700950710     C                   Z-ADD     CSBNRI        V1CNRI
022800950710     C                   Z-ADD     CSBCMB        V1CCMB
022900910917     C* INTESTAZIONE ASSEGNO / MITTENTE
023000971218     C                   CLEAR                   TNCSA000
023100971218     C* SE COD. INTESTAZ. ASSEGNO <> COD. CLIENTE DECODIFICO  CDI
023200971218  B1 C     V1CCDI        IFNE      V1CKSC
023300971218      *  SE CDI è = 8888 / 9999  PRENDO I DATI DA TNCSA
023400971219      *  (CASO POCO PROBABILE IN QUANTO IL CODICE INTESTAZ. NON DOVREBBE
023500971219      *   ESSERE=8888 O 9999 MENTRE IL KSC è UN CODIFICATO)
023600971218     C                   MOVE      V1CCDI        §8899             4 0
023700971218  B2 C     §8899         IFEQ      8888
023800971218     C     §8899         OREQ      9999
023900971218     C     KBLCAS        CHAIN     TNCSA000                           70
024000971218  B3 C     *IN70         IFEQ      *OFF
024100971219     C                   SETON                                        71
024200971218     C                   MOVEL     CSARSD        V1DINT
024300971218     C                   MOVEL     CSAVID        V1CVII
024400971218     C                   MOVEL     CSACAD        V1CCAI
024500971218     C                   MOVEL     CSACID        V1CCIT
024600971218     C                   MOVEL     CSANZM        V1CNZI
024700971218     C                   MOVEL     CSAPRM        V1CPRI
024800971218  E3 C                   ENDIF
024900971218  X2 C                   ELSE
025000971218      * ALTRIMENTI è UN CODIFICATO
025100971218      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE INTESTAZ. ASSEGNO
025200971218     C                   CLEAR                   TIBS69
025300971218     C                   CLEAR                   CNACO
025400971218     C                   CLEAR                   CNIND
025500971218     C                   CLEAR                   CNCLP
025600971218     C                   CLEAR                   FNCLS
025700971218     C                   MOVEL     ' '           I69TLA
025800971218     C                   MOVEL     KNSIF         I69SIF
025900971218     C                   Z-ADD     V1CCDI        I69KAC
026000971219     C                   Z-ADD     V1CCDI        I69KIN
026100971218     C                   CALL      'TIBS69R'
026200971218     C                   PARM                    TIBS69
026300971218     C                   PARM                    CNACO
026400971218     C                   PARM                    CNIND
026500971218     C                   PARM                    CNCLP
026600971218     C                   PARM                    FNCLS
026700971218  B3 C     O69ERR        IFEQ      *BLANK
026800971218     C                   MOVEL     ACORAG        V1DINT
026900971218     C                   MOVEL     INDVIA        V1CVII
027000971218     C                   MOVEL     *BLANKS       V1CCAI
027100971218  B4 C     INDCAP        IFNE      *ZEROS
027200971218     C                   MOVEL     INDCAP        V1CCAI
027300971218  E4 C                   END
027400971218     C                   MOVEL     INDSTA        V1CNZI
027500971218     C                   MOVEL     INDPRV        V1CPRI
027600971218  B4 C     INDCAE        IFNE      *ZEROS
027700971218     C     INDCAE        ANDNE     *BLANKS
027800971218     C                   MOVEL     INDCAE        V1CCAI
027900971218  E4 C                   END
028000971218     C                   MOVEL     INDCIT        V1CCIT
028100971218  E3 C                   ENDIF
028200971218  E2 C                   ENDIF
028300971218  E1 C                   ENDIF
028400971218     C* IN OGNI CASO DECODIFICO KSC
028500971218      *  SE KSC è = 8888 / 9999  PRENDO I DATI DA TNCSA
028600971218     C                   MOVE      V1CKSC        §8899
028700971218  B1 C     §8899         IFEQ      8888
028800971218     C     §8899         OREQ      9999
028900971126     C     KBLCAS        CHAIN     TNCSA000                           70
029000971218  B2 C     *IN70         IFEQ      *OFF
029100971219     C                   SETON                                        71
029200950710     C                   MOVEL     CSARSD        V1DMIO
029300950710     C                   MOVEL     CSAVID        V1CVIM
029400950710     C                   MOVEL     CSACAD        V1CCAM
029500950710     C                   MOVEL     CSACID        V1CLOM
029600950710     C                   MOVEL     CSANZM        V1CNZM
029700950710     C                   MOVEL     CSAPRM        V1CPRM
029800971218  E2 C                   ENDIF
029900971218  X1 C                   ELSE
030000971218      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
030100971218     C                   CLEAR                   TIBS69
030200971218     C                   CLEAR                   CNACO
030300971218     C                   CLEAR                   CNIND
030400971218     C                   CLEAR                   CNCLP
030500971218     C                   CLEAR                   FNCLS
030600971218     C                   MOVEL     ' '           I69TLA
030700971218     C                   MOVEL     KNSIF         I69SIF
030800971218     C                   Z-ADD     V1CKSC        I69KAC
030900971219     C                   Z-ADD     V1CKSC        I69KIN
031000971218     C                   CALL      'TIBS69R'
031100971218     C                   PARM                    TIBS69
031200971218     C                   PARM                    CNACO
031300971218     C                   PARM                    CNIND
031400971218     C                   PARM                    CNCLP
031500971218     C                   PARM                    FNCLS
031600971218  B2 C     O69ERR        IFEQ      *BLANK
031700950710     C                   MOVEL     ACORAG        V1DMIO
031800950710     C                   MOVEL     INDVIA        V1CVIM
031900950710     C                   MOVEL     *BLANKS       V1CCAM
032000971218  B3 C     INDCAP        IFNE      *ZEROS
032100950710     C                   MOVEL     INDCAP        V1CCAM
032200971218  E3 C                   END
032300950710     C                   MOVEL     INDSTA        V1CNZM
032400950710     C                   MOVEL     INDPRV        V1CPRM
032500971218  B3 C     INDCAE        IFNE      *ZEROS
032600941025     C     INDCAE        ANDNE     *BLANKS
032700950710     C                   MOVEL     INDCAE        V1CCAM
032800971218  E3 C                   END
032900950710     C                   MOVEL     INDCIT        V1CLOM
033000971218  E2 C                   END
033100971218  E1 C                   ENDIF
033200910917     C* DESTINATARIO
033300950710     C                   MOVEL     CSBRSD        V1CRSD
033400950710     C                   MOVEL     CSBPAR        V1CPAR
033500950710     C                   MOVEL     CSBPRO        V1CPRO
033600910917     C* NR° DOCUMENTO PAGAMENTO
033700950710     C                   Z-ADD     CSBNDP        V1CNDP
033800910917     C* BANCA PAGAMENTO - DECODIFICA SOLO SE COD. <> 0
033900950710     C                   MOVEL     *BLANKS       V1DBNA
034000950710     C                   Z-ADD     CSBBNA        V1CBNA
034100980127     C     CSBBNA        IFNE      0
034200980127     C                   CLEAR                   TIBS69
034300980127     C                   CLEAR                   CNACO
034400980127     C                   CLEAR                   CNIND
034500980127     C                   CLEAR                   CNCLP
034600980127     C                   CLEAR                   FNCLS
034700980127     C                   MOVEL     ' '           I69TLA
034800980127     C                   MOVEL     KNSIF         I69SIF
034900980127     C                   Z-ADD     KBNA          I69KCC
035000980127     C                   Z-ADD     CSBBNA        I69KAC
035100980127     C                   MOVEL     *BLANKS       ACORAG
035200980127     C                   CALL      'TIBS69R'
035300980127     C                   PARM                    TIBS69
035400980127     C                   PARM                    CNACO
035500980127     C                   PARM                    CNIND
035600980127     C                   PARM                    CNCLP
035700980127     C                   PARM                    FNCLS
035800980127     C     O69ERR        IFEQ      *BLANK
035900980127     C                   MOVEL     ACORAG        V1DBNA
036000910917     C                   END
036100980127     C                   END
036200910917     C* DATA PAGAMENTO
036300931203     C     CSBDDP        COMP      0                                      76
036400941025     C                   Z-ADD     CSBDDP        G02INV
036500941025     C                   Z-ADD     0             G02DAT
036600941026     C                   MOVEL     '3'           G02ERR
036700941025     C                   CALL      'XSRDA8'
036800941025     C                   PARM                    WLBDA8
036900950710     C                   Z-ADD     G02DAT        V1CDDP
037000941025     C* DATA REGISTRAZIONE PAGAMENTO
037100941025     C                   Z-ADD     CSBDRG        G02INV
037200941025     C                   Z-ADD     0             G02DAT
037300941026     C                   MOVEL     '3'           G02ERR
037400941025     C                   CALL      'XSRDA8'
037500941025     C                   PARM                    WLBDA8
037600950710     C                   Z-ADD     G02DAT        V1CDRG
037700950710     C                   Z-ADD     CSBNRG        V1CNRG
037800950710     C                   Z-ADD     CSBCMP        V1CCMP
037900910917     C* TIPO PAGAMENTO EFFETTUATO
038000950710     C                   MOVEL     CSBFPC        V1CFPC
038100910917     C* DATI EVENTUALE GIACENZA
038200950710     C                   Z-ADD     CSBNGC        V1CNGC
038300941025     C                   Z-ADD     CSBDGC        G02INV
038400941025     C                   Z-ADD     0             G02DAT
038500941026     C                   MOVEL     '3'           G02ERR
038600941025     C                   CALL      'XSRDA8'
038700941025     C                   PARM                    WLBDA8
038800950710     C                   Z-ADD     G02DAT        V1CDGC
038900910917     C* RAGGRUPPAMENTO CONTABILE
039000950710     C                   Z-ADD     CSBRGF        V1CRGF
039100910917     C* TIPO BOLLA
039200950710     C                   MOVEL     CSBTBL        V1CTBL
039300910917     C* IMPORTO C/A
039400950710     C                   Z-ADD     CSBCAS        V1CCAS
039500950710     C                   MOVEL     CSBVCA        V1CVCA
039600941028     C* FLAG PARTICOLARITA'
039700950710     C                   MOVEL     CSBGCA        V1CGCA
039800910920     C*
039900910902     C                   ENDSR
040000940413     C*-------------------------------------------------------------------*
040100910918     C     AGGIO         BEGSR
040200010109     C*-------------------------------------------------------------------*
040300010110     C                   SETOFF                                       28
040400940413     C                   MOVEL     *BLANKS       $MSG
040500931203     C*
040600950711     C  N28
040700950711     CAN KF              DO
040800910919     C* SE O.K. SCRIVE SITUAZIONE PRECEDENTE NELLO STORICO E AGGIORNA
040900971008     C* MUOVO I CAMPI DI TNCSB  IN BLCSV
041000971008     C                   MOVEL     CSBDS         CSVDS
041100971008      * Imposto nuovi valori
041200010112     C     CSBUDA        IFEQ      0
041300010110     C                   Z-ADD     DATEU         CSBUDA
041400010112     C                   ENDIF
041500010112     C     CSBDTA        IFEQ      0
041600010110     C                   Z-ADD     DATEU         CSBDTA
041700010112     C                   ENDIF
041800010112     C     CSBDDC        IFEQ      0
041900010110     C                   Z-ADD     DATEU         CSBDDC
042000010112     C                   ENDIF
042100010112     C     CSBNDT        IFEQ      0
042200010109     C                   Z-ADD     7777777       CSBNDT
042300010112     C                   ENDIF
042400010112     C     CSBNRI        IFEQ      0
042500010109     C                   Z-ADD     0             CSBNRI
042600010112     C                   ENDIF
042700010112     C     CSBDRI        IFEQ      0
042800010109     C                   Z-ADD     0             CSBDRI
042900010112     C                   ENDIF
043000010109     C                   Z-ADD     9999999       CSBBNA
043100010109     C                   Z-ADD     7777777       CSBNDP
043200010109     C                   Z-ADD     DATEU         CSBDDP
043300010109     C                   Z-ADD     0             CSBNRG
043400010109     C                   Z-ADD     0             CSBDRG
043500010109     C                   MOVEL     *BLANK        CSBFTR
043600010109     C                   Z-ADD     0             CSBDTR
043700010109     C                   Z-ADD     0             CSBSTA
043800010112     C     CSBMAS        IFEQ      *BLANK
043900010109     C                   MOVEL     'S'           CSBMAS
044000010112     C                   END
044100010109     C     CSBDS         IFNE      CSVDS
044200010109     C                   EXSR      STORY
044300010109     C                   END
044400910918     C                   EXCEPT    AGG
044500971009     C                   END
044600971008     C*
044700010109     C                   ENDSR
044800010109     C*----------------------------------------------------------------
044900910919     C     STORY         BEGSR
045000010109     C*----------------------------------------------------------------
045100910919     C* IMPOSTO I CAMPI NON COMUNI
045200950710     C     V1CSTA        IFEQ      9
045300910919     C                   MOVE      'ANCA'        CSVCAV
045400910919     C                   ELSE
045500050922     C                   MOVE      'PRES'        CSVCAV
045600910919     C                   END
045700010109     C                   Z-ADD     DATEU         CSVDTV
045800910919     C                   TIME                    CSVORV
045900910919     C                   Z-ADD     046           CSVFEV
046000941025     C                   WRITE     TNCSV000
046100910919     C                   ENDSR
046200910930     C*
046300941025      *-----------------------------------------------------*
046400941025      * OPERAZIONI INIZIALI                                 *
046500941025      *-----------------------------------------------------*
046600940413     C     *INZSR        BEGSR
046700940413      *
046800940413      * CHIAVI
046900940413     C     KTAB          KLIST
047000940413     C                   KFLD                    CODUT
047100940413     C                   KFLD                    CODTAB            2
047200940413     C                   KFLD                    KEYTAB            8
047300940413      *
047400940413     C     KSPE          KLIST
047500950711     C                   KFLD                    V1CAAS
047600950711     C                   KFLD                    V1CLNP
047700950711     C                   KFLD                    V1CNRS
047800950711     C                   KFLD                    V1CNSP
047900940413     C*
048000940413     C     KBLCAS        KLIST
048100950710     C                   KFLD                    V1CAAS
048200950710     C                   KFLD                    V1CLNP
048300950710     C                   KFLD                    V1CNRS
048400950710     C                   KFLD                    V1CNSP
048500940413     C*
048600940413     C     KBAN1         KLIST
048700940413     C                   KFLD                    CODUT
048800940413     C                   KFLD                    KBNA
048900950710     C                   KFLD                    V1CBNA
049000940413     C*
049100940413     C     KABI          KLIST
049200940413     C                   KFLD                    CSBABI
049300940413     C                   KFLD                    CSBCAB
049400940413      *---------------------------------------------------------------*
049500940413     C     *ENTRY        PLIST
049600940413     C                   PARM                    KPJBA
049700940413     C                   MOVEL     KPJBU         PAS01
049800940413      *
049900940413     C                   Z-ADD     1             CODUT
050000940413     C                   CALL      'X§PARUT'
050100940413     C                   PARM                    UT§DSE
050200940413     C*          CODUT     CABEQ-1        FINE
050300940413     C                   MOVEL     RAGUT         RSUT             20
050400940413     C                   MOVEL     REC80         CNCR80
050500940413     C                   MOVEL     KNMUS         §SEI              6
050600940413      *
050700940413      *  RICERCA CAPOCONTI
050800940413     C                   DO        50            X                 2 0
050900940413     C                   MOVE      TCU(X)        TCUDS
051000940413     C     F56           IFEQ      'CG'
051100940413     C     F4            IFEQ      '6'
051200940413     C                   Z-ADD     KCU(X)        KBNA              4 0
051300940413     C                   END
051400940413     C                   END
051500940413     C*
051600940413     C                   END
051700940413     C*
051800941103     C                   TIME                    WHHDAT           14 0
051900941103     C                   MOVE      WHHDAT        ANNO              4 0
052000010109     C                   MOVE      WHHDAT        G02DAT
052100010109     C                   Z-ADD     0             G02INV
052200010109     C                   CALL      'XSRDA8'
052300010109     C                   PARM                    WLBDA8
052400010109     C                   Z-ADD     G02INV        DATEU             8 0
052500940413     C                   ENDSR
052600900606      *----------------------------------------------------------------
052700941025     OTNCSB000  E            AGG
052800010109     O                       CSBUDA
052900010109     O                       CSBDTA
053000010109     O                       CSBDDC
053100010109     O                       CSBNDT
053200010109     O                       CSBNRI
053300010109     O                       CSBDRI
053400010109     O                       CSBBNA
053500010109     O                       CSBNDP
053600010109     O                       CSBDDP
053700010109     O                       CSBNRG
053800010109     O                       CSBDRG
053900010109     O                       CSBFTR
054000010109     O                       CSBDTR
054100010109     O                       CSBSTA
054200010109     O                       CSBMAS
054300010112** ERR
054400940413Numero di spedizione errato                                           01
054500010109C/Ass. non disponibile per effettuare variazione. Attendi 5' e ritenta02
054600020304STATO C/ASS NON CORRETTO : è ammesso solo lo stato  8=omesso          03
054700010112Contrassegno già pagato : non utilizzabile quetsa funzione            04
