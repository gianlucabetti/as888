000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300941025      * TNSC95R                                                      *
000400910918      *            *-------------------------*                       *
000500910918      *             MANUTENZIONE CONTRASSEGNI                        *
000600910918      *            *-------------------------*                       *
000700911104      *                                                              *
000800910918      * 36- ERRORI NEI CAMPI DELLA 1a VIDEATA                        *
000900910918      * 51 -INDICATORE DI COMODO                                     *
001000971219      * 52 -SI PUO VARIARE SOLO RAGGR.TO CTB IL RESTO DA GEST. BOLLE * OBS
001100971219      * 52 -PROTEGGO ALCUNI CAMPI                                    *
001200940124      * 53 -CODICE ABI-CAB ERRATO                                    *
001300940124      * 54 -IMMETTERE NUMERO ASSEGNO                                 *
001400940124      * 55 -NUMERO ASSEGNO ERRATO                                    *
001500950711      * 40-45 ERRORI NEI CAMPI VARIATI                               *
001600910930      * 70 -INDICATORE DI COMODO                                     *
001700941025      * 76 -CONT ASSEGNO NON ANCORA PAGATO                           *
001800971219      * 77 -SPROTEGGE  IMPORTO-RAGGRUPPAMENTO                        *
001900910918      * 78 -SPROTEGGE STATO E CODICE INTESTAZIONE                    *
002000950711      * 28 -ERRORE GENERICO                                          *
002100910918      * KC -FINE LAVORO                                              *
002200910918      * KL -FINE VARIAZIONE                                          *
002300900606?     *--------------------------------------------------------------*
002400950711     FTNSC43D   CF   E             WORKSTN
002500950711     FTNSC95D   CF   E             WORKSTN
002600941025     FTNCSB03L  UF   E           K DISK
002700941025     FTNCSV00F  O    E             DISK
002800971008     FTNCSA01L  UF   E           K DISK
002900910909     FCNABI00F  IF   E           K DISK
003000941025     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003100910906     FTABEL00F  IF   E           K DISK
003200011019     FTnTbe01L  IF   E           K DISK
003300011112     FTiTas30C  IF   E           K DISK
003301100219     Ffiar531c  IF   E           K DISK
003400941025?     *--------------------------------------------------------------*
003500971219     D ERR             S             70    DIM(27) CTDATA PERRCD(1)
003600900515      *
003700910708     D CNCR80        E DS
003800900514     D KPJBA         E DS
003900910828     D  LIBSYS                92    100
004000910828     D  MSGERR               453    502
004100941025     D CSBDS         E DS                  EXTNAME(TNCSB00F)
004200941025     D               E DS                  EXTNAME(TNCSV00F)
004300941028     D  CSVDS                 16    229
004400900516      *
004500900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004600000000     D  TCU                  398    697
004700000000     D                                     DIM(50)
004800000000     D  KCU                  698    847P 0
004900000000     D                                     DIM(50)
005000000000     D                                     PACKEVEN
005100900606      *
005200900606     D TCUDS           DS
005300900606     D  F3                     3      3
005400900606     D  F4                     4      4
005500900606     D  F56                    5      6
005600900515      *
005700941025     D WLBDA8          DS
005800941025     D  G02DAT                 1      8  0
005900941025     D  G02INV                 9     16  0
006000941025     D  G02ERR                17     17
006100941025     D  G02TGI                18     22  0
006200971218     D*
006300971218     D TIBS69        E DS                  EXTNAME(TIBS69DS)
006400971218     D CNACO         E DS                  EXTNAME(CNACO00F)
006500971218     D CNIND         E DS                  EXTNAME(CNIND00F)
006600971218     D CNCLP         E DS                  EXTNAME(CNCLP00F)
006700971218     D FNCLS         E DS                  EXTNAME(FNCLS00F)
006701100219     D dar5gen       E DS
006800910521      *
006900910918     D PAS01           DS
007000910918     D  FLAG                   1      1
007100960923     D DS4S          E DS
007200960923     D DSCV          E DS
007300011019     D DGEI          E DS
007400011019     D                                     INZ
007500011019     D DGED          E DS
007600011019     D                                     INZ
007700011019     D YEurCoDS      E DS
007800011019     D                                     INZ
007900011112     D Trul21DS      E DS
008000011112     D                                     INZ
008100950711     D                SDS
008200950711     D  NOMPGM                 1     10
008300941025     I*--------------------------------------------------------------*
008400941025     IAZORG01L  AA
008500941025     I                             P    4    5 0ORGFIL
008600941025     I                                 14   33  ORGDES
008700910830      *---------------------------------------------------------------*
008800910917     C*
008900910916     C                   EXSR      PULIZ
009000910918     C     *INKC         DOWEQ     '0'
009100971219     C                   SETON                                        777952
009200911018     C     FLAG          COMP      *BLANKS                                78
009300950711     C                   EXFMT     SC95D01
009400940413     C                   SETOFF                                       50
009500940413     C                   MOVEL     *BLANKS       $MSG
009600910930     C  NKC              DO
009700941026     C*  CONTROLLO SE L'ANNO E'NEL FORMATO 2 CHR E SE SI LO CONVERTO
009800950711     C                   MOVEL     V1CAAS        WAA2              2 0
009900941026     C     WAA2          IFEQ      0
010000950711     C     V1CAAS        IFGT      60
010100950711     C                   ADD       1900          V1CAAS
010200941026     C                   ELSE
010300950711     C                   ADD       2000          V1CAAS
010400941026     C                   END
010500941026     C                   END
010600941026     C*
010700950727     C     KSPE          CHAIN     TNCSB000                           5060
010800940413     C   50              MOVEL     ERR(1)        $MSG
010900971218     C     *IN50         IFEQ      *OFF
011000980225     C     *IN60         ANDEQ     *OFF
011100971218     C                   ENDIF
011200950727     C  N50*IN60         IFEQ      '1'
011300950727     C                   MOVEL     ERR(21)       $MSG
011400950727     C                   SETON                                        50
011500971218     C                   END
011600940413     C  N50              EXSR      VISUAL
011700910930     C     *INKL         DOWEQ     '0'
011800910919     C     *INKF         ANDEQ     '0'
011900910919     C     *IN50         ANDEQ     '0'
012000950711     C     *IN28         OREQ      '1'
012100950711     C                   EXFMT     SC43D01
012200910930     C  NKL              EXSR      AGGIO
012300950711     C   KL              SETOFF                                       28
012400910902     C                   END
012500910917     C                   END
012600910917     C                   END
012700971218     C*
012800971218     C* RICHIAMO PGM X CHIUSURA FILES
012900971218     C                   CLEAR                   TIBS69
013000971218     C                   MOVEL     'C'           I69TLA
013100971218     C                   CALL      'TIBS69R'
013200971218     C                   PARM                    TIBS69
013300971218     C                   PARM                    CNACO
013400971218     C                   PARM                    CNIND
013500971218     C                   PARM                    CNCLP
013600971218     C                   PARM                    FNCLS
013700910521     C                   SETON                                        LR
013800910521     C*
013900900515      *-----------------------------------------------------***********
014000910918      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
014100900515      *-----------------------------------------------------***********
014200910918     C     PULIZ         BEGSR
014300950711     C                   Z-ADD     0             V1CLNP
014400950711     C                   Z-ADD     ANNO          V1CAAS
014500950711     C                   Z-ADD     0             V1CNRS
014600950711     C                   Z-ADD     0             V1CNSP
014700910918     C                   ENDSR
014800910918     C*
014900910828     C     VISUAL        BEGSR
015000950711     C                   SETOFF                                       284041
015100950711     C                   SETOFF                                       424344
015200950711     C                   SETOFF                                       454647
015300980302     C                   SETOFF                                       4871
015400971219     C                   CLEAR                   V1DINT
015500971219     C                   CLEAR                   V1CVII
015600971219     C                   CLEAR                   V1CCIT
015700971219     C                   CLEAR                   V1CCAI
015800971219     C                   CLEAR                   V1CNZI
015900971219     C                   CLEAR                   V1CPRI
016000910917     C* STATO DEL C/A
016100950710     C                   Z-ADD     CSBSTA        V1CSTA
016200910917     C                   MOVEL     '4S'          CODTAB
016300941108     C                   MOVEL     *BLANK        KEYTAB
016400950710     C                   MOVE      V1CSTA        KEYTAB
016500960923     C                   MOVEL     *BLANKS       WMOD
016600910930     C     KTAB          CHAIN     TABEL                              70
016700950710     C  N70              MOVEL     TBLUNI        V1DSTA
016800960923     C  N70              MOVEL     TBLUNI        DS4S
016900960923     C  N70              MOVEL     §4SMOD        WMOD              1
017000950710     C   70              MOVEL     *BLANKS       V1DSTA
017100910917     C* LINEA DI PARTENZA + DECODIFICA
017200950710     C                   Z-ADD     CSBLNP        V1CLNP
017300950710     C     V1CLNP        CHAIN     AZORG01L                           70
017400950710     C  N70              MOVEL     ORGDES        V1DLNP
017500950710     C   70              MOVEL     *BLANKS       V1DLNP
017600910917     C* NR° SERIE + SPEDIZIONE
017700950710     C                   Z-ADD     CSBNRS        V1CNRS
017800950710     C                   Z-ADD     CSBNSP        V1CNSP
017900910917     C* CODICE CLIENTE
018000950710     C                   Z-ADD     CSBKSC        V1CKSC
018100910917     C* DATA SPEDIZIONE
018200941028     C                   CLEAR                   WLBDA8
018300941025     C                   MOVEL     CSBAAS        G02INV
018400941028     C                   MOVE      CSBMGS        G02INV
018500941025     C                   Z-ADD     0             G02DAT
018600941025     C                   MOVEL     '3'           G02ERR
018700941025     C                   CALL      'XSRDA8'
018800941025     C                   PARM                    WLBDA8
018900950710     C                   Z-ADD     G02DAT        V1CDSP
019000910917     C* DATA DISTINTA
019100941025     C                   Z-ADD     CSBDDC        G02INV
019200941025     C                   Z-ADD     0             G02DAT
019300941025     C                   MOVEL     '3'           G02ERR
019400941025     C                   CALL      'XSRDA8'
019500941025     C                   PARM                    WLBDA8
019600950710     C                   Z-ADD     G02DAT        V1CDDC
019700910917     C* LINEA ARRIVO + DECODIFICA
019800950710     C                   Z-ADD     CSBLNA        V1CLNA
019900941025     C                   Z-ADD     CSBLNA        ORGFIL
020000941025     C     ORGFIL        CHAIN     AZORG01L                           70
020100950710     C  N70              MOVEL     ORGDES        V1DLNA
020200950710     C   70              MOVEL     *BLANKS       V1DLNA
020300910917     C* NUMERO DISTINTA
020400950710     C                   Z-ADD     CSBNDT        V1CNDT
020500910917     C* RIFERIMENTO MITTENTE
020600950710     C                   Z-ADD     CSBRMN        V1CRMN
020700950710     C                   MOVEL     CSBRMA        V1CRMA
020800910917     C* DATA ARRIVO DISTINTA
020900941025     C                   Z-ADD     CSBDTA        G02INV
021000941025     C                   Z-ADD     0             G02DAT
021100941025     C                   MOVEL     '3'           G02ERR
021200941025     C                   CALL      'XSRDA8'
021300941025     C                   PARM                    WLBDA8
021400950710     C                   Z-ADD     G02DAT        V1CDTA
021500910917     C* NR° ASSEGNO = RIFERIM. INCASSO
021600950710     C                   MOVEL     CSBNRA        V1CNRA
021700910917     C* DATA ASSEGNO
021800941025     C                   Z-ADD     CSBDTE        G02INV
021900941025     C                   Z-ADD     0             G02DAT
022000941025     C                   MOVEL     '3'           G02ERR
022100941025     C                   CALL      'XSRDA8'
022200941025     C                   PARM                    WLBDA8
022300950710     C                   Z-ADD     G02DAT        V1CDTE
022400910917     C* TIPO INTESTAZIONE ASSEGNO
022500950710     C                   MOVEL     CSBTPI        V1CTPI
022600950710     C                   MOVEL     CSBTPA        V1CTPA
022700950711     C                   MOVEL     CSBFUS        V1CFUS
022800950711     C                   MOVEL     CSBMAS        V1CMAS
022801100219     C* verifico se è stato variato il tipo incasso bollettato
022802100224     c                   setoff                                       8889
022803100224     c                   clear                   v1ctici
022804100224     c                   clear                   v1cticmb
022805100224     c                   if        v1caas > 2009
022806100224     c                   seton                                        88
022807100219     c     kar5          chain     fiar531c
022808100219     c                   if        %found(fiar531c)
022809100219     c                   movel     ar5uni        dar5gen
022810100224     c                   movel     §AR5TICI      v1ctici
022811100219     c                   if        §AR5FLGMB = 'S'
022812100224     c                   seton                                        89
022813100224     c                   movel     §AR5TICMB     v1cticmb
022814100219     c                   end
022815100219     c                   end
022816100224     c                   end
022900910917     C* CODICE INTESTAZIONE ASS.
023000950710     C                   Z-ADD     CSBCDI        V1CCDI
023100910917     C* ABI / CAB / EVENTUALE DECODIFICA BANCA
023200950710     C                   Z-ADD     CSBABI        V1CABI
023300950710     C                   Z-ADD     CSBCAB        V1CCAB
023400910917     C     KABI          CHAIN     CNABI000                           75
023500950710     C  N75              MOVEL     ABIIST        V1CDES
023600950710     C   75              MOVEL     *BLANKS       V1CDES
023700941025     C* DATA REGISTRAZIONE INCASSO
023800941025     C                   Z-ADD     CSBDRI        G02INV
023900941025     C                   Z-ADD     0             G02DAT
024000941026     C                   MOVEL     '3'           G02ERR
024100941025     C                   CALL      'XSRDA8'
024200941025     C                   PARM                    WLBDA8
024300950710     C                   Z-ADD     G02DAT        V1CDRI
024400941025     C* NUMERO REG.INCASSO/CAMBIO
024500950710     C                   Z-ADD     CSBNRI        V1CNRI
024600011019     C**                 Z-ADD     CSBCMB        V1CCMB
024700910917     C* INTESTAZIONE ASSEGNO / MITTENTE
024800971218     C                   CLEAR                   TNCSA000
024900971218     C* SE COD. INTESTAZ. ASSEGNO <> COD. CLIENTE DECODIFICO  CDI
025000971218  B1 C     V1CCDI        IFNE      V1CKSC
025100971218      *  SE CDI è = 8888 / 9999  PRENDO I DATI DA TNCSA
025200971219      *  (CASO POCO PROBABILE IN QUANTO IL CODICE INTESTAZ. NON DOVREBBE
025300971219      *   ESSERE=8888 O 9999 MENTRE IL KSC è UN CODIFICATO)
025400971218     C                   MOVE      V1CCDI        §8899             4 0
025500971218  B2 C     §8899         IFEQ      8888
025600971218     C     §8899         OREQ      9999
025700971218     C     KBLCAS        CHAIN     TNCSA000                           70
025800971218  B3 C     *IN70         IFEQ      *OFF
025900971219     C                   SETON                                        71
026000971218     C                   MOVEL     CSARSD        V1DINT
026100971218     C                   MOVEL     CSAVID        V1CVII
026200971218     C                   MOVEL     CSACAD        V1CCAI
026300971218     C                   MOVEL     CSACID        V1CCIT
026400971218     C                   MOVEL     CSANZM        V1CNZI
026500971218     C                   MOVEL     CSAPRM        V1CPRI
026600971218  E3 C                   ENDIF
026700971218  X2 C                   ELSE
026800971218      * ALTRIMENTI è UN CODIFICATO
026900971218      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE INTESTAZ. ASSEGNO
027000971218     C                   CLEAR                   TIBS69
027100971218     C                   CLEAR                   CNACO
027200971218     C                   CLEAR                   CNIND
027300971218     C                   CLEAR                   CNCLP
027400971218     C                   CLEAR                   FNCLS
027500971218     C                   MOVEL     ' '           I69TLA
027600971218     C                   MOVEL     KNSIF         I69SIF
027700971218     C                   Z-ADD     V1CCDI        I69KAC
027800971219     C                   Z-ADD     V1CCDI        I69KIN
027900971218     C                   CALL      'TIBS69R'
028000971218     C                   PARM                    TIBS69
028100971218     C                   PARM                    CNACO
028200971218     C                   PARM                    CNIND
028300971218     C                   PARM                    CNCLP
028400971218     C                   PARM                    FNCLS
028500971218  B3 C     O69ERR        IFEQ      *BLANK
028600971218     C                   MOVEL     ACORAG        V1DINT
028700971218     C                   MOVEL     INDVIA        V1CVII
028800971218     C                   MOVEL     *BLANKS       V1CCAI
028900971218  B4 C     INDCAP        IFNE      *ZEROS
029000971218     C                   MOVEL     INDCAP        V1CCAI
029100971218  E4 C                   END
029200971218     C                   MOVEL     INDSTA        V1CNZI
029300971218     C                   MOVEL     INDPRV        V1CPRI
029400971218  B4 C     INDCAE        IFNE      *ZEROS
029500971218     C     INDCAE        ANDNE     *BLANKS
029600971218     C                   MOVEL     INDCAE        V1CCAI
029700971218  E4 C                   END
029800971218     C                   MOVEL     INDCIT        V1CCIT
029900971218  E3 C                   ENDIF
030000971218  E2 C                   ENDIF
030100971218  E1 C                   ENDIF
030200971218     C* IN OGNI CASO DECODIFICO KSC
030300971218      *  SE KSC è = 8888 / 9999  PRENDO I DATI DA TNCSA
030400971218     C                   MOVE      V1CKSC        §8899
030500971218  B1 C     §8899         IFEQ      8888
030600971218     C     §8899         OREQ      9999
030700971126     C     KBLCAS        CHAIN     TNCSA000                           70
030800971218  B2 C     *IN70         IFEQ      *OFF
030900971219     C                   SETON                                        71
031000950710     C                   MOVEL     CSARSD        V1DMIO
031100950710     C                   MOVEL     CSAVID        V1CVIM
031200950710     C                   MOVEL     CSACAD        V1CCAM
031300950710     C                   MOVEL     CSACID        V1CLOM
031400950710     C                   MOVEL     CSANZM        V1CNZM
031500950710     C                   MOVEL     CSAPRM        V1CPRM
031600971218  E2 C                   ENDIF
031700971218  X1 C                   ELSE
031800971218      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
031900971218     C                   CLEAR                   TIBS69
032000971218     C                   CLEAR                   CNACO
032100971218     C                   CLEAR                   CNIND
032200971218     C                   CLEAR                   CNCLP
032300971218     C                   CLEAR                   FNCLS
032400971218     C                   MOVEL     ' '           I69TLA
032500971218     C                   MOVEL     KNSIF         I69SIF
032600971218     C                   Z-ADD     V1CKSC        I69KAC
032700971219     C                   Z-ADD     V1CKSC        I69KIN
032800971218     C                   CALL      'TIBS69R'
032900971218     C                   PARM                    TIBS69
033000971218     C                   PARM                    CNACO
033100971218     C                   PARM                    CNIND
033200971218     C                   PARM                    CNCLP
033300971218     C                   PARM                    FNCLS
033400971218  B2 C     O69ERR        IFEQ      *BLANK
033500950710     C                   MOVEL     ACORAG        V1DMIO
033600950710     C                   MOVEL     INDVIA        V1CVIM
033700950710     C                   MOVEL     *BLANKS       V1CCAM
033800971218  B3 C     INDCAP        IFNE      *ZEROS
033900950710     C                   MOVEL     INDCAP        V1CCAM
034000971218  E3 C                   END
034100950710     C                   MOVEL     INDSTA        V1CNZM
034200950710     C                   MOVEL     INDPRV        V1CPRM
034300971218  B3 C     INDCAE        IFNE      *ZEROS
034400941025     C     INDCAE        ANDNE     *BLANKS
034500950710     C                   MOVEL     INDCAE        V1CCAM
034600971218  E3 C                   END
034700950710     C                   MOVEL     INDCIT        V1CLOM
034800971218  E2 C                   END
034900971218  E1 C                   ENDIF
035000910917     C* DESTINATARIO
035100950710     C                   MOVEL     CSBRSD        V1CRSD
035200950710     C                   MOVEL     CSBPAR        V1CPAR
035300950710     C                   MOVEL     CSBPRO        V1CPRO
035400910917     C* NR° DOCUMENTO PAGAMENTO
035500950710     C                   Z-ADD     CSBNDP        V1CNDP
035600910917     C* BANCA PAGAMENTO - DECODIFICA SOLO SE COD. <> 0
035700950710     C                   MOVEL     *BLANKS       V1DBNA
035800950710     C                   Z-ADD     CSBBNA        V1CBNA
035900980127     C     CSBBNA        IFNE      0
036000980127     C                   CLEAR                   TIBS69
036100980127     C                   CLEAR                   CNACO
036200980127     C                   CLEAR                   CNIND
036300980127     C                   CLEAR                   CNCLP
036400980127     C                   CLEAR                   FNCLS
036500980127     C                   MOVEL     ' '           I69TLA
036600980127     C                   MOVEL     KNSIF         I69SIF
036700980127     C                   Z-ADD     KBNA          I69KCC
036800980127     C                   Z-ADD     CSBBNA        I69KAC
036900980127     C                   MOVEL     *BLANKS       ACORAG
037000980127     C                   CALL      'TIBS69R'
037100980127     C                   PARM                    TIBS69
037200980127     C                   PARM                    CNACO
037300980127     C                   PARM                    CNIND
037400980127     C                   PARM                    CNCLP
037500980127     C                   PARM                    FNCLS
037600980127     C     O69ERR        IFEQ      *BLANK
037700980127     C***        KBAN1     CHAINCNACO000             70
037800980127     C                   MOVEL     ACORAG        V1DBNA
037900910917     C                   END
038000980127     C                   END
038100910917     C* DATA PAGAMENTO
038200931203     C     CSBDDP        COMP      0                                      76
038300941025     C                   Z-ADD     CSBDDP        G02INV
038400941025     C                   Z-ADD     0             G02DAT
038500941026     C                   MOVEL     '3'           G02ERR
038600941025     C                   CALL      'XSRDA8'
038700941025     C                   PARM                    WLBDA8
038800950710     C                   Z-ADD     G02DAT        V1CDDP
038900941025     C* DATA REGISTRAZIONE PAGAMENTO
039000941025     C                   Z-ADD     CSBDRG        G02INV
039100941025     C                   Z-ADD     0             G02DAT
039200941026     C                   MOVEL     '3'           G02ERR
039300941025     C                   CALL      'XSRDA8'
039400941025     C                   PARM                    WLBDA8
039500950710     C                   Z-ADD     G02DAT        V1CDRG
039600950710     C                   Z-ADD     CSBNRG        V1CNRG
039700011019     C**                 Z-ADD     CSBCMP        V1CCMP
039800910917     C* TIPO PAGAMENTO EFFETTUATO
039900950710     C                   MOVEL     CSBFPC        V1CFPC
040000910917     C* DATI EVENTUALE GIACENZA
040100950710     C                   Z-ADD     CSBNGC        V1CNGC
040200941025     C                   Z-ADD     CSBDGC        G02INV
040300941025     C                   Z-ADD     0             G02DAT
040400941026     C                   MOVEL     '3'           G02ERR
040500941025     C                   CALL      'XSRDA8'
040600941025     C                   PARM                    WLBDA8
040700950710     C                   Z-ADD     G02DAT        V1CDGC
040800910917     C* RAGGRUPPAMENTO CONTABILE
040900950710     C                   Z-ADD     CSBRGF        V1CRGF
041000910917     C* TIPO BOLLA
041100950710     C                   MOVEL     CSBTBL        V1CTBL
041200910917     C* IMPORTO C/A
041300950710     C                   Z-ADD     CSBCAS        V1CCAS
041400950710     C                   MOVEL     CSBVCA        V1CVCA
041500941028     C* FLAG PARTICOLARITA'
041600950710     C                   MOVEL     CSBGCA        V1CGCA
041700910920     C* SE C/A GIA PAGATO  PROTEGGO TUTTI I CAMP
041800950710     C     V1CDDP        IFNE      0
041900910920     C                   SETOFF                                       7778
042000910920     C                   END
042100941028     C*
042200950710     C     V1CPRM        COMP      *BLANKS                            10
042300950710     C     V1CNZM        COMP      *BLANKS                            11
042400950710     C     V1CPRI        COMP      *BLANKS                            14
042500950710     C     V1CNZI        COMP      *BLANKS                            13
042600941028     C*
042700910920     C*
042800910902     C                   ENDSR
042900940413     C*-------------------------------------------------------------------*
043000910918     C     AGGIO         BEGSR
043100950711     C                   SETOFF                                       284041
043200950711     C                   SETOFF                                       424344
043300950711     C                   SETOFF                                       454647
043400950711     C                   SETOFF                                       4849
043500940413     C                   MOVEL     *BLANKS       $MSG
043600960326     C                   Z-ADD     0             DATASS            8 0
043700910918     C* CONTROLLO SE ESISTE LO STATO
043800910918     C                   MOVEL     '4S'          CODTAB
043900941108     C                   MOVEL     *BLANK        KEYTAB
044000950710     C                   MOVE      V1CSTA        KEYTAB
044100940413     C                   MOVEL     *BLANKS       TBLUNI
044200950711     C     KTAB          CHAIN     TABEL                              28
044300950711     C     *IN28         IFEQ      '1'
044400950711     C                   SETON                                        40
044500960923     C                   MOVEL     ERR(2)        $MSG
044600940413     C                   GOTO      FINAGG
044700940413     C                   END
044800960923     C                   MOVEL     TBLUNI        DS4S
044900960923     C     CSBSTA        IFNE      V1CSTA
045000960923     C     WMOD          IFNE      'S'
045100960923     C                   SETON                                        4028
045200960923     C                   MOVEL     CSBSTA        V1CSTA
045300960923     C                   MOVEL     ERR(23)       $MSG
045400960923     C                   GOTO      FINAGG
045500960923     C                   END
045600960923     C     §4SMOD        IFNE      'S'
045700960923     C                   SETON                                        4028
045800960923     C                   MOVEL     ERR(22)       $MSG
045900960923     C                   GOTO      FINAGG
046000960923     C                   END
046100960923     C                   END
046200960923     C                   MOVEL     §4SDES        V1DSTA
046300910920     C* SE SI VUOLE ANNULLARE IL C/A NON DEVE ESSERE INCASSATO
046400950710     C     V1CSTA        IFEQ      9
046500950710     C     V1CDTA        ANDNE     0
046600010112     C     V1CSTA        OREQ      3
046700010115     C     V1CDDC        ANDNE     0
046800950711     C                   SETON                                        4028
046900940413     C                   MOVEL     ERR(3)        $MSG
047000940413     C                   GOTO      FINAGG
047100910920     C                   END
047200911216     C* OPPURE DEVE ESSERE GIA TASSATA ALTRIMENTI BISOGNA AGIRE DA
047300911216     C* GESTIONE BOLLE
047400971219      * NON SERVE PIù IN QUANTO IL C/ASS NON è PIù SU TNTAS
047500971219     C*****      V1CSTA    IFEQ 9
047600971219     C****       *IN52     ANDEQ'1'
047700971219     C****                 SETON                     2840
047800971219     C****                 MOVELERR,4     $MSG
047900971219     C****                 GOTO FINAGG
048000971219     C****                 END
048100910918     C* CONTROLLA LA LINEA DI ARRIVO
048200950710     C                   Z-ADD     V1CLNA        ORGFIL
048300950711     C     ORGFIL        CHAIN     AZORG01L                           41
048400950711     C     *IN41         IFEQ      '1'
048500950711     C                   SETON                                        28
048600940413     C                   MOVEL     ERR(5)        $MSG
048700940413     C                   GOTO      FINAGG
048800940413     C                   END
048900950710     C                   MOVEL     ORGDES        V1DLNA
049000910918     C* CONTROLLA CODICE INTESTAZIONE CLIENTE
049100950710     C                   MOVE      V1CCDI        APP               4
049200971219      * NON SI PUò DIGITARE COD.INTESTAZ. = 8888/9999
049300910918     C     APP           IFEQ      '8888'
049400910918     C     APP           OREQ      '9999'
049500950710     C     CSBCDI        IFNE      V1CCDI
049600950711     C                   SETON                                        42
049700911001     C                   END
049800910918     C                   ELSE
049900971218     C****                 Z-ADDV1CCDI    KSC
050000971218     C****       KCCC      CHAINCNACO000             42
050100971218      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE
050200971218     C                   CLEAR                   TIBS69
050300971218     C                   CLEAR                   CNACO
050400971218     C                   CLEAR                   CNIND
050500971218     C                   CLEAR                   CNCLP
050600971218     C                   CLEAR                   FNCLS
050700971218     C                   MOVEL     ' '           I69TLA
050800971218     C                   MOVEL     KNSIF         I69SIF
050900971218     C                   Z-ADD     V1CCDI        I69KAC
051000971219     C                   Z-ADD     V1CCDI        I69KIN
051100971218     C                   CALL      'TIBS69R'
051200971218     C                   PARM                    TIBS69
051300971218     C                   PARM                    CNACO
051400971218     C                   PARM                    CNIND
051500971218     C                   PARM                    CNCLP
051600971218     C                   PARM                    FNCLS
051700971218     C     O69ERR        IFNE      *BLANK
051800971218     C                   SETON                                        42
051900971218     C                   ENDIF
052000910918     C                   END
052100950711     C  N42V1CCDI        COMP      0                                      42
052200950711     C     *IN42         IFEQ      '1'
052300950711     C                   SETON                                        28
052400940413     C                   MOVEL     ERR(6)        $MSG
052500940413     C                   GOTO      FINAGG
052600940413     C                   END
052700971218      * CONTROLLO CONGRUENZA ramo aziendale delle PRIME TRE CIFRE DEL COD.CLI
052800971218      *           DI INTESTAZ. PAGAMENTO CON LNP SE FRANCO O LNA SE ASSEGN.
052900971218     C                   MOVEL     V1CCDI        §LNCLI            3 0
053000971219      * SE CAMBIATO IL COD. INTESTAZ. LO DECODIFICO
053100971219     C     V1CCDI        IFNE      CSBCDI
053200971219     C                   MOVEL     ACORAG        V1DINT
053300971219     C                   MOVEL     INDVIA        V1CVII
053400971219     C                   MOVEL     *BLANKS       V1CCAI
053500971219     C     INDCAP        IFNE      *ZEROS
053600971219     C                   MOVEL     INDCAP        V1CCAI
053700971219     C                   END
053800971219     C                   MOVEL     INDSTA        V1CNZI
053900971219     C                   MOVEL     INDPRV        V1CPRI
054000971219     C     INDCAE        IFNE      *ZEROS
054100971219     C     INDCAE        ANDNE     *BLANKS
054200971219     C                   MOVEL     INDCAE        V1CCAI
054300971219     C                   END
054400971219     C                   MOVEL     INDCIT        V1CCIT
054500971219     C                   ENDIF
054600971219      * SE è STATO REIMPOSTATO = A KSC PULISCO I DATI COD. INTEST.
054700971219     C     V1CCDI        IFEQ      CSBKSC
054800971219     C                   CLEAR                   V1DINT
054900971219     C                   CLEAR                   V1CVII
055000971219     C                   CLEAR                   V1CCIT
055100971219     C                   CLEAR                   V1CCAI
055200971219     C                   CLEAR                   V1CNZI
055300971219     C                   CLEAR                   V1CPRI
055400971219     C                   ENDIF
055500910918     C* CONTROLLA RAGGRUPPAMENTO CONTABILITA'
055600950710     C                   Z-ADD     V1CRGF        ORGFIL
055700950711     C     ORGFIL        CHAIN     AZORG01L                           43
055800950711     C     *IN43         IFEQ      '1'
055900950711     C                   SETON                                        28
056000940413     C                   MOVEL     ERR(7)        $MSG
056100940413     C                   GOTO      FINAGG
056200940413     C                   END
056300910918     C* CONTROLLA IMPORTO ASSEGNO CHE NON SIA 0
056400950711     C     V1CCAS        IFEQ      0
056500950711     C                   SETON                                        2844
056600940413     C                   MOVEL     ERR(8)        $MSG
056700940413     C                   GOTO      FINAGG
056800940413     C                   END
056900941025     C* CONTROLLO LA DIVISA
057000941025     C                   CLEAR                   DSCV
057100011129     C                   MOVE      V1CCAS        DEC               2
057200950710     C     V1CVCA        IFNE      *BLANKS
057300960604     C*  Controllo interrogazione codicd valuta
057400960604     C     '?'           SCAN      V1CVCA                                 95
057500960604     C     *IN95         IFEQ      '1'
057600960604     C                   Z-ADD     1             §KUT              1 0
057700960604     C                   MOVEL     'CV'          §COD              2
057800960604     C                   MOVE      *BLANKS       V1CVCA
057900960604     C                   CALL      'X§TABER'
058000960604     C                   PARM                    §KUT
058100960604     C                   PARM                    §COD
058200960604     C                   PARM                    §KEY              8
058300960604     C                   PARM                    §DES             30
058400960604     C                   MOVEL     §KEY          V1CVCA
058500960604     C                   END
058600960604     C*
058700941025     C                   MOVEL     'CV'          CODTAB
058800941108     C                   MOVEL     *BLANK        KEYTAB
058900950710     C                   MOVEL     V1CVCA        KEYTAB
059000950711     C     KTAB          CHAIN     TABEL                              49
059100950711     C     *IN49         IFEQ      '1'
059200941025     C     TBLFLG        ORNE      ' '
059300950711     C                   SETON                                        2849
059400941025     C                   MOVEL     ERR(17)       $MSG
059500941025     C                   GOTO      FINAGG
059600941025     C                   END
059700951023     C                   MOVEL     TBLUNI        DSCV
059800011019     C**   §CVITA        IFNE      'I'
059900011019     C**   V1CCMB        ANDNE     0
060000011019     C**                 Z-ADD     §CVCMB        V1CCMB
060100011019     C**                 Z-ADD     §CVCMB        WCMB              9 3
060200011019     C**                 ELSE
060300011019     C**                 Z-ADD     0             V1CCMB
060400011019     C**                 Z-ADD     1             WCMB
060500011019     C**                 END
060600941025     C*
060700941025     C     DEC           IFNE      *ZEROS
060800941025     C     §CVFDC        ANDNE     'S'
060900950711     C                   SETON                                        494428
061000941025     C                   MOVEL     ERR(18)       $MSG
061100941025     C                   END
061200011112     ** Vecchio Controllo importo massimo contrassegno in m.c.
061300011019     C**   V1CCAS        MULT      WCMB          WTOTCA           20 0
061400011113     C**                 CLEAR                   YEurCoDS
061500011113     C**                 EVAL      YecDvI = V1CVca
061600011113     C**                 EVAL      YecImI = V1CCas
061700011113     C**                 EVAL      YecDvO = §GEDCn
061800011113     C**                 CALL      'YEURCO'
061900011113     C**                 PARM                    YEurCoDS
062000011019     C*
062100011113     C**                 EVAL      TbeCod = 'GEI'
062200011113     C**                 EVAL      TbeKe1 = §GEDCn
062300011113     C**                 CLEAR                   TbeKe2
062400011113     C**   K03Tbe01      CHAIN     TnTbe01L
062500011113     C**                 IF        %FOUND AND TbeAtb = *BLANK
062600011113     C**                 EVAL      DGEI = TbeUni
062700011019     C**   WTOTCA        IFGT      LIMCA
062800011113     C**                 IF        YecImO > §GEMCA
062900011113     C**                 SETON                                        494428
063000011019     C**                 MOVEL     ERR(24)       $MSG
063100011113     C**                 EVAL      $Msg = %TRIMR(Err(24)) + ' ' +
063200011113     C**                           %TRIM(%EDITC(§GEMCA:'2')) + ' ' + §GEDCn
063300011113     C**                 GOTO      FINAGG
063400011113     C**                 END
063500011113     C**                 ELSE
063600011113     C**                 CLEAR                   DGEI
063700011113     C**                 ENDIF
063800011114     ** Nuovo Controllo importo minimo e massimo contrassegno.
063900011113     C                   EXSR      ChkCasMax
064000011113     C                   IF        O21Fx2 <> *BLANK
064100011114     C                             OR
064200011114     C                             O21FMn <> *BLANK
064300011113     C                   SETON                                        494428
064400011114     C                   EVAL      $Msg =
064500011114     C                             'Immettere un importo compreso fra '
064600011114     C                             + %TRIM(%EDITC(O21LMn:'2')) + ' ' + §GEDMc
064700011114     C                             + ' e '
064800011114     C                             + %TRIM(%EDITC(O21Lx2:'2')) + ' ' + §GEDMc
064900011114     C                             + '.'
065000011113     C                   GOTO      FinAgg
065100011113     C                   ENDIF
065200941025     C*
065300941025     C                   ELSE
065400941025     C*
065500950711     C                   SETON                                        494428
065600941025     C                   MOVEL     ERR(19)       $MSG
065700971008     C                   GOTO      FINAGG
065800971008     C* Se divisa a blanks azzero il cambio
065900011019     C**                 Z-ADD     0             V1CCMB
066000011019     C**                 Z-ADD     0             V1CCMP
066100941025     C                   END
066200941025     C* CONTROLLO ATTENDIBILITA' DIVISA CAMBIO
066300011019     C**   V1CVCA        IFEQ      *BLANKS
066400011019     C**   §CVITA        OREQ      'I'
066500011019     C**   V1CCMB        IFNE      0
066600011019     C**   V1CCMP        ORNE      0
066700011019     C**                 SETON                                        4928
066800011019     C**                 MOVEL     ERR(20)       $MSG
066900011019     C**                 GOTO      FINAGG
067000011019     C**                 END
067100011019     C**                 END
067200940413     C* CONTROLLO DATI ASSEGNO
067300950710     C                   MOVEL     *BLANKS       V1CDES
067400950710     C     V1CTPA        IFEQ      ' '
067500950710     C     V1CNRA        IFNE      *ZEROS
067600950710     C     V1CNRA        ANDNE     *BLANKS
067700950711     C                   SETON                                        454428
067800940413     C                   MOVEL     ERR(9)        $MSG
067900940413     C                   GOTO      FINAGG
068000940413     C                   END
068100950710     C     V1CABI        IFNE      *ZEROS
068200950710     C     V1CCAB        ORNE      *ZEROS
068300950711     C                   SETON                                        474628
068400940413     C                   MOVEL     ERR(10)       $MSG
068500940413     C                   GOTO      FINAGG
068600940413     C                   END
068700950710     C     V1CDTE        IFNE      *ZEROS
068800950711     C                   SETON                                        484628
068900940413     C                   MOVEL     ERR(11)       $MSG
069000940413     C                   GOTO      FINAGG
069100940413     C                   END
069200940413     C*
069300940413     C                   ELSE
069400950710     C     V1CNRA        IFEQ      *ZEROS
069500950710     C     V1CNRA        OREQ      *BLANKS
069600950711     C                   SETON                                        454628
069700940413     C                   MOVEL     ERR(12)       $MSG
069800940413     C                   GOTO      FINAGG
069900940413     C                   END
070000950710     C     V1CABI        IFEQ      *ZEROS
070100950710     C     V1CCAB        ANDEQ     *ZEROS
070200950711     C                   SETON                                        474628
070300940413     C                   MOVEL     ERR(13)       $MSG
070400940413     C                   GOTO      FINAGG
070500940413     C                   END
070600940413     C* ABI / CAB / EVENTUALE DECODIFICA BANCA
070700950710     C                   Z-ADD     V1CABI        CSBABI
070800950710     C                   Z-ADD     V1CCAB        CSBCAB
070900950711     C     KABI          CHAIN     CNABI000                           28
071000950711     C     *IN28         IFEQ      '1'
071100950711     C                   SETON                                        4728
071200940413     C                   MOVEL     ERR(14)       $MSG
071300940413     C                   GOTO      FINAGG
071400940413     C                   END
071500950710     C                   MOVEL     ABIIST        V1CDES
071600950710     C     V1CDTE        IFEQ      *ZEROS
071700950711     C                   SETON                                        484628
071800940413     C                   MOVEL     ERR(15)       $MSG
071900940413     C                   GOTO      FINAGG
072000940413     C                   END
072100940412     C*  Controllo la validità della data
072200950710     C                   Z-ADD     V1CDTE        G02DAT
072300940412     C                   MOVEL     *BLANKS       G02ERR
072400941025     C                   CALL      'XSRDA8'
072500941025     C                   PARM                    WLBDA8
072600940412     C     G02ERR        IFEQ      '1'
072700950711     C                   SETON                                        2848
072800940413     C                   MOVEL     ERR(16)       $MSG
072900940413     C                   GOTO      FINAGG
073000940412     C                   ELSE
073100940412     C                   Z-ADD     G02INV        DATASS
073200950710     C                   Z-ADD     G02DAT        V1CDTE
073300940412     C                   END
073400940413     C*
073500940124     C                   END
073600971219      * SE TPI = M , DEVONO ESSERCI I RIFERIM. DELL'ASSEGNO
073700971219     C     V1CTPI        IFEQ      'M'
073800971219     C     V1CNRA        IFEQ      *BLANKS
073900971219     C                   SETON                                        2845
074000971219     C                   MOVEL     ERR(12)       $MSG
074100971219     C                   ENDIF
074200971219     C     V1CDTE        IFEQ      *ZEROS
074300971219     C                   SETON                                        2848
074400971219     C                   MOVEL     ERR(15)       $MSG
074500971219     C                   ENDIF
074600971219     C     V1CTPA        IFEQ      *BLANK
074700971219     C                   SETON                                        2846
074800971219     C                   MOVEL     ERR(26)       $MSG
074900971219     C                   ENDIF
075000971219     C   28              GOTO      FINAGG
075100971219     C                   ENDIF
075200971219     C     V1CNDT        IFEQ      *ZEROS
075300971219     C     V1CNRA        IFNE      *BLANKS
075400971219     C     V1CDTE        ORNE      *ZEROS
075500971219     C                   SETON                                        2845
075600971219     C                   MOVEL     ERR(27)       $MSG
075700971219     C                   ENDIF
075800971219     C   28              GOTO      FINAGG
075900971219     C                   ENDIF
076000931203     C*
076100950711     C  N28
076200950711     CAN KF              DO
076300910919     C* SE O.K. SCRIVE SITUAZIONE PRECEDENTE NELLO STORICO E AGGIORNA
076400971008     C* MUOVO I CAMPI DI TNCSB  IN BLCSV
076500971008     C                   MOVEL     CSBDS         CSVDS
076600971008      * Imposto nuovi valori
076700011019     C**                 Z-ADD     V1CCMB        CSBCMB
076800951023     C                   Z-ADD     V1CABI        CSBABI
076900950710     C                   Z-ADD     V1CCAB        CSBCAB
077000950710     C                   Z-ADD     V1CLNA        CSBLNA
077100950710     C                   Z-ADD     V1CCAS        CSBCAS
077200950710     C                   MOVEL     V1CVCA        CSBVCA
077300950710     C                   Z-ADD     V1CCDI        CSBCDI
077400950710     C                   Z-ADD     V1CRGF        CSBRGF
077500950710     C                   Z-ADD     V1CSTA        CSBSTA
077600940412     C                   Z-ADD     DATASS        CSBDTE
077700950710     C                   MOVEL     V1CTPA        CSBTPA
077800950710     C                   MOVEL     V1CNRA        CSBNRA
077900971008     C     CSBDS         IFNE      CSVDS
078000971008     C                   EXSR      STORY
078100971008     C                   END
078200960923     C*  Se devo trasmettere la modifica dello stato in filiale
078300960923     C*  azzero dati trasmissione
078400960923     C     §4SFIL        IFEQ      'S'
078500960923     C                   Z-ADD     0             CSBDTR
078600960923     C                   MOVEL     *BLANKS       CSBFTR
078700960923     C                   END
078800910918     C                   EXCEPT    AGG
078900971008      * Scrivo storico se sono stati modificati i valori
079000971008     C* Aggiorno TNCSA
079100971009     C     *IN71         IFEQ      '1'
079200971008     C                   MOVEL     V1DMIO        CSARSD
079300971008     C                   MOVEL     V1CVIM        CSAVID
079400971008     C                   MOVEL     V1CCAM        CSACAD
079500971008     C                   MOVEL     V1CLOM        CSACID
079600971008     C                   MOVEL     V1CNZM        CSANZM
079700971008     C                   MOVEL     V1CPRM        CSAPRM
079800971008     C                   UPDATE    TNCSA000
079900971008     C                   END
080000971009     C                   END
080100971008     C*
080200940413     C     FINAGG        ENDSR
080300910918     C*
080400910919     C     STORY         BEGSR
080500910919     C* IMPOSTO I CAMPI NON COMUNI
080600950710     C     V1CSTA        IFEQ      9
080700910919     C                   MOVE      'ANCA'        CSVCAV
080800910919     C                   ELSE
080900910919     C                   MOVE      'VACA'        CSVCAV
081000910919     C                   END
081100941025     C                   TIME                    WHHDAT           14 0
081200941103     C                   MOVE      WHHDAT        ANNO              4 0
081300941025     C                   MOVE      WHHDAT        G02DAT
081400941025     C                   Z-ADD     0             G02INV
081500941025     C                   CALL      'XSRDA8'
081600941025     C                   PARM                    WLBDA8
081700941025     C                   Z-ADD     G02INV        CSVDTV
081800910919     C                   TIME                    CSVORV
081900910919     C                   Z-ADD     046           CSVFEV
082000941025     C                   WRITE     TNCSV000
082100910919     C                   ENDSR
082200910930     C*
082300941025      *-----------------------------------------------------*
082400941025      * OPERAZIONI INIZIALI                                 *
082500941025      *-----------------------------------------------------*
082600940413     C     *INZSR        BEGSR
082700940413      *
082800940413      * CHIAVI
082900940413     C     KTAB          KLIST
083000940413     C                   KFLD                    CODUT
083100940413     C                   KFLD                    CODTAB            2
083200940413     C                   KFLD                    KEYTAB            8
083300011019      *
083400011019     C     K03Tbe01      KLIST
083500011019     C                   KFLD                    TbeCod
083600011019     C                   KFLD                    TbeKe1
083700011019     C                   KFLD                    TbeKe2
083800940413      *
083900940413     C     KSPE          KLIST
084000950711     C                   KFLD                    V1CAAS
084100950711     C                   KFLD                    V1CLNP
084200950711     C                   KFLD                    V1CNRS
084300950711     C                   KFLD                    V1CNSP
084400940413     C*
084500940413     C     KBLCAS        KLIST
084600950710     C                   KFLD                    V1CAAS
084700950710     C                   KFLD                    V1CLNP
084800950710     C                   KFLD                    V1CNRS
084900950710     C                   KFLD                    V1CNSP
085000940413     C*
085100940413     C     KBAN1         KLIST
085200940413     C                   KFLD                    CODUT
085300940413     C                   KFLD                    KBNA
085400950710     C                   KFLD                    V1CBNA
085500940413     C*
085600940413     C     KABI          KLIST
085700940413     C                   KFLD                    CSBABI
085800940413     C                   KFLD                    CSBCAB
085900011112     C*
086000011112     C     K05Tas30      KLIST
086100011112     C                   KFLD                    TasAaS
086200011112     C                   KFLD                    TasLnP
086300011112     C                   KFLD                    TasNrS
086400011112     C                   KFLD                    TasNSp
086500011112     C                   KFLD                    TasTBl
086501100219     C     Kar5          KLIST
086502100219     C                   KFLD                    V1CAAS
086503100219     C                   KFLD                    V1CLNP
086504100219     C                   KFLD                    V1CNRS
086505100219     C                   KFLD                    V1CNSP
086506100219     C                   KFLD                    ar5trd
086507100219     c                   eval      ar5trd = 'GEN'
086600940413      *---------------------------------------------------------------*
086700940413     C     *ENTRY        PLIST
086800940413     C                   PARM                    KPJBA
086900940413     C                   MOVEL     KPJBU         PAS01
087000940413      *
087100940413     C                   Z-ADD     1             CODUT
087200940413     C                   CALL      'X§PARUT'
087300940413     C                   PARM                    UT§DSE
087400940413     C*          CODUT     CABEQ-1        FINE
087500940413     C                   MOVEL     RAGUT         RSUT             20
087600940413     C                   MOVEL     REC80         CNCR80
087700940413     C                   MOVEL     KNMUS         §SEI              6
087800940413      *
087900940413      *  RICERCA CAPOCONTI
088000940413     C                   DO        50            X                 2 0
088100940413     C                   MOVE      TCU(X)        TCUDS
088200940413     C     F56           IFEQ      'CG'
088300940413     C     F4            IFEQ      '6'
088400940413     C                   Z-ADD     KCU(X)        KBNA              4 0
088500940413     C                   END
088600940413     C                   END
088700940413     C*
088800940413     C                   END
088900940413     C*
089000941103     C                   TIME                    WHHDAT           14 0
089100941103     C                   MOVE      WHHDAT        ANNO              4 0
089200971008     c***
089300971008      * Carico dalla tabella CV divisa Italia l'importo MAX di un
089400971008      *  C/Ass. in divisa
089500011019     C**                 MOVEL     'CV'          CODTAB
089600011019     C**                 MOVEL     *BLANKS       KEYTAB
089700011019     C**   KTAB          CHAIN     TABEL                              30
089800011019     C**   *IN30         IFEQ      *OFF
089900011019     C**                 MOVEL     TBLUNI        DSCV
090000011019     C**                 MOVEL     §CVLIM        LIMCA            10 0
090100011019     C**                 ELSE
090200011019     C**                 Z-ADD     20000000      LIMCA
090300011019     C**                 ENDIF
090400011019     C*
090500011019     C                   EVAL      TbeCod = 'GED'
090600011019     C                   EVAL      TbeKe1 = '1'
090700011019     C                   CLEAR                   TbeKe2
090800011019     C     K03Tbe01      CHAIN     TnTbe01L
090900011019     C                   IF        %FOUND AND TbeAtb = *BLANK
091000011019     C                   EVAL      DGED = TbeUni
091100011019     C                   ENDIF
091200011019     C*
091300940413     C                   ENDSR
091400011112
091500011112     ** Controllo importo massimo contrassegno.
091600011113     C     ChkCasMax     BEGSR
091700011112     C*
091800011112     C                   EVAL      TasAaS = CsbAaS
091900011112     C                   EVAL      TasLnP = CsbLnP
092000011112     C                   EVAL      TasNrS = CsbNrS
092100011112     C                   EVAL      TasNSp = CsbNSp
092200011112     C                   EVAL      TasTBl = CsbTBl
092300011112     C     K05Tas30      CHAIN     TiTas30C
092400011112     C*
092500011112     C                   CLEAR                   Trul21DS
092600011112     C                   EVAL      I21CBo = TasCBo
092700011112     C                   EVAL      I21TSp = TasTsp
092800011112     C                   EVAL      I21LnP = TasLnP
092900011112     C                   EVAL      I21NzM = TasNzM
093000011112     C                   EVAL      I21LnA = TasLnA
093100011112     C                   EVAL      I21NzD = TasNzD
093200011112     C                   EVAL      I21Ksc = TasKsc
093300011112     C                   MOVE      TasCTr        I21CTr
093400011113     C                   EVAL      I21TiC = V1CTpA + V1CTpI
093500011112     C                   EVAL      I21Imp = V1CCas
093600011112     C                   EVAL      I21Div = V1CVCa
093700011112     C                   EVAL      I21Ute = KNmUs
093800011112     C                   EVAL      I21Pgm = NomPgm
093900011112     C                   CALL      'TRUL21R'
094000011112     C                   PARM                    Trul21DS
094100011112     C*
094200011112     C                   ENDSR
094300011019
094400900606      *----------------------------------------------------------------
094500941025     OTNCSB000  E            AGG
094600910918     O                       CSBLNA
094700941025     O                       CSBCAS
094800941025     O                       CSBVCA
094900910918     O                       CSBCDI
095000910918     O                       CSBRGF
095100910918     O                       CSBSTA
095200931203     O                       CSBABI
095300940121     O                       CSBCAB
095400940121     O                       CSBNRA
095500940412     O                       CSBDTE
095600940413     O                       CSBTPA
095700011019     O**                     CSBCMB
095800960923     O                       CSBDTR
095900960923     O                       CSBFTR
096000971219** ERR
096100940413Numero di spedizione errato                                           01
096200940413Stato Contrassegno non esistente in tabella !!                        02
096300010112Stato C/ass non utilizzabile : è già incassato  !!                    03
096400940413Spedizione non tassata modificabile solo per il raggruppamento        04
096500940413Linea di arrivo errata                                                05
096600940413Codice cliente non valido                                             06
096700961119Punto operativo raggruppamento inesistente                            07
096800940413L'importo del Contrassegno non può essere 0                           08
096900940413Impossibile immettere numero assegno                                  09
097000940413ABI-CAB non validi per questo tipo di incasso                         10
097100940413Data assegno non valida per questo tipo di incasso                    11
097200940413Immettere numero Assegno                                              12
097300940413ABI-CAB Obbligatori per questo tipo di incasso                        13
097400940413ABI-CAB Errati                                                        14
097500940413Data Assegno obbligatoria per questo tipo di incasso                  15
097600940413Data Assegno errata                                                   16
097700941025Codice divisa inesistente                                             17
097800941025Per la divisa indicata non sono ammessi importi con decimali          18
097900010615Divisa obbligatoria !!!                                               19
098000941025Se immesso cambio obbligatoria divisa estera                          20
098100950727C/Ass. non disponibile per effettuare variazione. Attendi 5' e ritenta21
098200960923Stato C/Assegno non utilizzabilie da manutenzione                     22
098300960923Stato C/Assegno iniziale non modificabile                             23
098400011019Importo superiore al limite di                                        24
098500971218Filiale cod. cl. non è congruente con ramo aziendale della LNP o LNA  25
098600971219Immettere il Tipo Assegno                                             26
098700971219C/Assegno non incassato , non immettere riferim. assegno              27
