000100950711     H DECEDIT('0,') DATEDIT(*DMY.)
000200950711?     *--------------------------------------------------------------*
000300950711      * TRSC43R                                                      *
000400950711      *          *-----------------------------*                     *
000500950711      *            INTERROGAZIONE CONTRASSEGNI                       *
000600950711      *          *-----------------------------*                     *
000700950711      *                                                              *
000800950711?     *--------------------------------------------------------------*
000900950711     FTNSC43D   CF   E             WORKSTN
001000950711     FTNCSB03L  IF   E           K DISK
001100100219     Ffiar531c  IF   E           K DISK
001200950711     FTNCSA01L  IF   E           K DISK
001300950711     FCNABI00F  IF   E           K DISK
001400950711     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001500950711     FTABEL00F  IF   E           K DISK
001600160419     Fyhbos15l  IF   E           K DISK
001700950711?     *--------------------------------------------------------------*
001800950711     D C4S             S              1  0 DIM(50)
001900950711     D D4S             S             20    DIM(50)
002000150709     D ERR             S             70    DIM(2) CTDATA PERRCD(1)
002100120210     D tnsc30ds      E DS
002200150709     D tnsb50ds      E DS
002300120209     D CNCR80        E DS
002400950711     D KPJBA         E DS
002500950711     D  LIBSYS                92    100
002600950711     D  MSGERR               453    502
002700971219     D TNSC40        E DS                  EXTNAME(TNSC40DS)
002800971219     D TNSC44        E DS                  EXTNAME(TNSC44DS)
002900950711     D TNSC94        E DS                  EXTNAME(TNSC94DS)
003000971222     D TNSC96        E DS                  EXTNAME(TNSC96DS)
003100100219     D dar5gen       E DS
003200971219     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003300971219     D CNACO         E DS                  EXTNAME(CNACO00F)
003400971219     D CNIND         E DS                  EXTNAME(CNIND00F)
003500971219     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003600971219     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003700950711     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003800950711     D  TCU                  398    697
003900950711     D                                     DIM(50)
004000950711     D  KCU                  698    847P 0
004100950711     D                                     DIM(50)
004200950711     D                                     PACKEVEN
004300950711      *
004400950711     D TCUDS           DS
004500950711     D  F34                    3      4
004600950711     D  F56                    5      6
004700950711      *
004800950711     D WLBDA8          DS
004900950711     D  G02DAT                 1      8  0
005000950711     D  G02INV                 9     16  0
005100950711     D  G02ERR                17     17
005200950711     D  G02TGI                18     22  0
005300950711     D                SDS
005400950711     D  NOMPGM                 1     10
005500950711     I*--------------------------------------------------------------*
005600950711     IAZORG01L  AA
005700950711     I                             P    4    5 0ORGFIL
005800950711     I                                 14   33  ORGDES
005900950711     C*-------------------------------------------------------*
006000950711     C*  CICLO PRINCIPALE
006100950711     C*-------------------------------------------------------*
006200950711     C                   EXSR      CNTSPE
006300950711     C*  Loop gestione videata
006400950711     C     WFINE         DOWNE     'S'
006500950711     C     WTPVID        CASEQ     '1'           GESD01
006600950711     C     WTPVID        CASEQ     '2'           GESD02
006700950711     C                   END
006800010228     C                   END
006900971219     C*
007000971219     C* RICHIAMO PGM X CHIUSURA FILES
007100971219     C                   CLEAR                   TIBS69
007200971219     C                   MOVEL     'C'           I69TLA
007300971219     C                   CALL      'TIBS69R'
007400971219     C                   PARM                    TIBS69
007500971219     C                   PARM                    CNACO
007600971219     C                   PARM                    CNIND
007700971219     C                   PARM                    CNCLP
007800971219     C                   PARM                    FNCLS
007900950711     C*
008000000224     C                   MOVEL     TNSC40        KPJBU
008100950711     C                   SETON                                        LR
008200950711     C*-------------------------------------------------------*
008300950711     C*  Controllo se esiste spedizione
008400950711     C*-------------------------------------------------------*
008500950711     C     CNTSPE        BEGSR
008600950711     C*
008700950711     C                   MOVEL     '2'           WTPVID            1
008800950711     C                   Z-ADD     D40AAS        KAAS
008900950711     C                   Z-ADD     D40LNP        KLNP
009000950711     C                   Z-ADD     D40NRS        KNRS
009100950711     C                   Z-ADD     D40NSP        KNSP
009200950711     C     KCSB          CHAIN     TNCSB03L                           31
009300971219     C     *IN31         IFEQ      '0'
009400950711     C                   MOVEL     D40NAS        WNRA             10
009500010228     C*                    MOVELCSBINC    SAVINC 75
009600010228     C*                    MOVELCSBPAG    SAVPAG 46
009700950711     C*----------------------------------------------------*
009800950711     C     D40ABI        IFEQ      *ZEROS
009900950711     C     D40ABI        OREQ      CSBABI
010000950711     C     D40CAB        ANDNE     *ZEROS
010100950711     C     D40CAB        ANDEQ     CSBCAB
010200950711     C     D40ABI        OREQ      CSBABI
010300980122     C     WNRA          ANDGT     *ZEROS
010400950711     C     WNRA          ANDEQ     CSBNRA
010500980122     C     D40ABI        OREQ      CSBABI
010600980122     C     D40CAB        ANDEQ     *ZEROS
010700980122     C     WNRA          ANDLE     *ZEROS
010800990611     C* Controllo se C/Assegno Infragruppo
010900971222     C* devo richiamare il pgm per riconoscere i dati infragruppo
011000971222     C                   CLEAR                   TNSC96
011100971223     C                   MOVEL     'L'           D96TLA
011200971222     C                   MOVEL     KNSIF         D96NSI
011300010301     C                   Z-ADD     CSBAAS        D96AAS
011400010301     C                   Z-ADD     CSBLNP        D96LNP
011500010301     C                   Z-ADD     CSBNRS        D96NRS
011600010301     C                   Z-ADD     CSBNSP        D96NSP
011700971222     C* Richiamo pgm
011800971222     C                   CALL      'TNSC96R'
011900990611     C                   PARM                    KPJBA
012000990611     C                   PARM                    TNSC96
012100950711     C*----------------------------------------------------*
012200950711     C* Selezione per stato C/Assegno
012300950711     C     D40TCR        IFEQ      ' '
012400950711     C     D40TCR        OREQ      'D'
012500010228     C     CSBDDC        ANDEQ     0
012600950711     C     D40TCR        OREQ      'G'
012700010228     C     CSBSTA        ANDEQ     2
012800950711     C     D40TCR        OREQ      'A'
012900950711     C     CSBSTA        ANDEQ     9
013000960911     C     D40TCR        OREQ      'T'
013100960911     C     CSBSTA        ANDEQ     6
013200960911     C     D40TCR        OREQ      'R'
013300960911     C     CSBSTA        ANDEQ     7
013400960911     C     D40TCR        OREQ      'O'
013500960911     C     CSBSTA        ANDEQ     8
013600950711     C     D40TCR        OREQ      'S'
013700950711     C     CSBSTA        ANDEQ     5
013800010112     C     D40TCR        OREQ      '3'
013900010112     C     CSBSTA        ANDEQ     3
014000950711     C     D40TCR        OREQ      'I'
014100950711     C     CSBDDC        ANDGT     0
014200950711     C     CSBDDP        ANDEQ     0
014300950711     C     D40TCR        OREQ      'P'
014400950711     C     CSBDDP        ANDGT     0
014500950711     C                   EXSR      INZD01
014600950711     C                   END
014700950711     C                   END
014800950711     C*----------------------------------------------------*
014900950711     C                   END
015000950711     C*----------------------------------------------------*
015100950711     C                   ENDSR
015200950711     C*-------------------------------------------------------*
015300950711     C*  Inizializzazione videata
015400950711     C*-------------------------------------------------------*
015500950711     C     INZD01        BEGSR
015600950711     C*
015700950711     C                   MOVEL     '1'           WTPVID
015800971219      *
015900971231     C     D96TRV        IFEQ      'S'
016000971219     C                   SETON                                        08
016100971231     C                   ENDIF
016200950711     C*
016300950711     C                   MOVEL     CSBSTA        V1CSTA
016400950711     C                   MOVEL     *BLANKS       V1DSTA
016500950711     C*  Decodifico stato C/Ass.
016600950711     C                   Z-ADD     1             X
016700950711     C     CSBSTA        LOOKUP    C4S(X)                                 32
016800950711     C     *IN32         IFEQ      '1'
016900950711     C                   MOVEL     D4S(X)        V1DSTA
017000950711     C                   END
017100950711     C* Trasmesso / da trasmettere / ricevuto solo se pagato
017200950711     C                   SETOFF                                       85
017300950711     C                   MOVEL     *BLANKS       V1CTRA
017400950711     C     CSBNDP        IFNE      0
017500950711     C                   SETON                                        85
017600950711     C     CSBFTR        IFEQ      *BLANKS
017700950711     C                   MOVEL     'DA TRASM'    V1CTRA
017800950711     C                   MOVE      'ETTERE'      V1CTRA
017900950711     C                   END
018000950711     C     CSBFTR        IFEQ      'R'
018100950711     C                   MOVEL     'RICEVUTO'    V1CTRA
018200950711     C                   END
018300950711     C     CSBFTR        IFEQ      'T'
018400950711     C                   MOVEL     'TRASMESS'    V1CTRA
018500950711     C                   MOVE      'O     '      V1CTRA
018600950711     C                   END
018700950711     C                   END
018800950711     C* Linea di partenza + decodifica
018900950711     C                   Z-ADD     CSBLNP        V1CLNP
019000950711     C                   MOVEL     *BLANKS       ORGDES
019100950711     C                   Z-ADD     CSBLNP        ORGFIL
019200950711     C     ORGFIL        CHAIN     AZORG01L                           31
019300950711     C                   MOVEL     ORGDES        V1DLNP
019400950711     C* Nr° serie + Spedizione
019500950711     C                   Z-ADD     CSBNRS        V1CNRS
019600950711     C                   Z-ADD     CSBNSP        V1CNSP
019700950711     C* Codice cliente
019800950711     C                   Z-ADD     CSBKSC        V1CKSC
019900950711     C* data spedizione
020000950711     C                   MOVEL     CSBAAS        G02INV
020100950711     C                   MOVE      CSBMGS        G02INV
020200950711     C                   Z-ADD     0             G02DAT
020300950711     C                   MOVEL     '3'           G02ERR
020400950711     C                   CALL      'XSRDA8'
020500950711     C                   PARM                    WLBDA8
020600950711     C                   Z-ADD     G02DAT        V1CDSP
020700950711     C* data documento
020800950711     C                   Z-ADD     CSBDDC        G02INV
020900950711     C                   Z-ADD     0             G02DAT
021000950711     C                   MOVEL     '3'           G02ERR
021100950711     C                   CALL      'XSRDA8'
021200950711     C                   PARM                    WLBDA8
021300950711     C                   Z-ADD     G02DAT        V1CDDC
021400950711     C* Linea arrivo + decodifica
021500950711     C                   Z-ADD     CSBLNA        V1CLNA
021600950711     C                   Z-ADD     CSBLNA        ORGFIL
021700950711     C                   MOVEL     *BLANKS       ORGDES
021800950711     C     ORGFIL        CHAIN     AZORG01L                           50
021900950711     C                   MOVEL     ORGDES        V1DLNA
022000950711     C* Riferim. mittente
022100950711     C                   Z-ADD     CSBRMN        V1CRMN
022200950711     C                   MOVEL     CSBRMA        V1CRMA
022300950711     C* Distinta
022400950711     C                   Z-ADD     CSBNDT        V1CNDT
022500950711     C                   Z-ADD     CSBDTA        G02INV
022600950711     C                   Z-ADD     0             G02DAT
022700950711     C                   MOVEL     '3'           G02ERR
022800950711     C                   CALL      'XSRDA8'
022900950711     C                   PARM                    WLBDA8
023000950711     C                   Z-ADD     G02DAT        V1CDTA
023100950711     C* Assegno
023200950711     C                   MOVEL     CSBNRA        V1CNRA
023300950711     C                   Z-ADD     CSBDTE        G02INV
023400950711     C                   Z-ADD     0             G02DAT
023500950711     C                   MOVEL     '3'           G02ERR
023600950711     C                   CALL      'XSRDA8'
023700950711     C                   PARM                    WLBDA8
023800950711     C                   Z-ADD     G02DAT        V1CDTE
023900950711     C                   MOVEL     CSBTPI        V1CTPI
024000950711     C                   MOVEL     CSBTPA        V1CTPA
024100950711     C                   MOVEL     CSBFUS        V1CFUS
024200950711     C                   MOVEL     CSBMAS        V1CMAS
024300120209     c* abilito comando per multiassegno
024400120220     c                   if        %subst(csbnra: 10: 1)=' ' or csbtpi <> 'M'
024500120209     c                   setoff                                       09
024600120209     c                   else
024700120209     c                   seton                                        09
024800120209     c                   end
024900100219     C* verifico se è stato variato il tipo incasso bollettato
025000100224     c                   setoff                                       8889
025100100224     c                   clear                   v1cticmb
025200100224     c                   clear                   v1ctici
025300100224     c                   if        kaas > 2009
025400100224     c                   seton                                        88
025500100219     c     kar5          chain     fiar531c
025600100219     c                   if        %found(fiar531c)
025700100219     c                   movel     ar5uni        dar5gen
025800100224     c                   movel     §AR5TICI      v1ctici
025900100219     c                   if        §AR5FLGMB = 'S'
026000100224     c                   seton                                        89
026100100224     c                   movel     §AR5TICMB     v1cticmb
026200100219     c                   end
026300100219     c                   end
026400100224     c                   end
026500100219     C* Registrazione incasso
026600950711     C                   Z-ADD     CSBDRI        G02INV
026700950711     C                   Z-ADD     0             G02DAT
026800950711     C                   MOVEL     '3'           G02ERR
026900950711     C                   CALL      'XSRDA8'
027000950711     C                   PARM                    WLBDA8
027100950711     C                   Z-ADD     G02DAT        V1CDRI
027200950711     C                   Z-ADD     CSBNRI        V1CNRI
027300950711     C* Codice Intestazione Assegno
027400950711     C                   Z-ADD     CSBCDI        V1CCDI
027500950711     C* ABI / CAB / Eventuale decodifica Banca
027600950711     C                   Z-ADD     CSBABI        V1CABI
027700950711     C                   Z-ADD     CSBCAB        V1CCAB
027800950711     C                   Z-ADD     CSBABI        KABI
027900950711     C                   Z-ADD     CSBCAB        KCAB
028000950711     C                   MOVEL     *BLANKS       ABIIST
028100950711     C     KABI1         CHAIN     CNABI000                           31
028200950711     C                   MOVEL     ABIIST        V1CDES
028300950711     C* Se cliente vario lo decodifico da blcsa
028400950711     C                   MOVEL     *BLANKS       ACOOPZ
028500950711     C                   MOVE      V1CKSC        APP               4
028600950711     C     APP           IFEQ      '8888'
028700950711     C     APP           OREQ      '9999'
028800950711     C                   MOVEL     *BLANKS       CSARSD
028900950711     C                   MOVEL     *BLANKS       CSAVID
029000950711     C                   MOVEL     *BLANKS       CSACAD
029100950711     C                   MOVEL     *BLANKS       CSACID
029200950711     C                   MOVEL     *BLANKS       CSAPRM
029300950711     C                   MOVEL     *BLANKS       CSANZM
029400950711     C     KCSA          CHAIN     TNCSA01L                           31
029500950711     C                   MOVEL     CSARSD        V1DMIO
029600950711     C                   MOVEL     CSAVID        V1CVIM
029700950711     C                   MOVEL     CSACAD        V1CCAM
029800950711     C                   MOVEL     CSACID        V1CLOM
029900950711     C                   MOVEL     CSAPRM        V1CPRM
030000950711     C                   MOVEL     CSANZM        V1CNZM
030100950711     C                   ELSE
030200971219      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
030300971219     C                   CLEAR                   TIBS69
030400971219     C                   CLEAR                   CNACO
030500971219     C                   CLEAR                   CNIND
030600971219     C                   CLEAR                   CNCLP
030700971219     C                   CLEAR                   FNCLS
030800971219     C                   MOVEL     ' '           I69TLA
030900971219     C                   MOVEL     KNSIF         I69SIF
031000971219     C                   Z-ADD     V1CKSC        I69KAC
031100971219     C                   Z-ADD     V1CKSC        I69KIN
031200971219     C                   CALL      'TIBS69R'
031300971219     C                   PARM                    TIBS69
031400971219     C                   PARM                    CNACO
031500971219     C                   PARM                    CNIND
031600971219     C                   PARM                    CNCLP
031700971219     C                   PARM                    FNCLS
031800971219     C     O69ERR        IFEQ      *BLANK
031900971219     C                   MOVEL     ACORAG        V1DMIO
032000971219     C                   MOVEL     INDVIA        V1CVIM
032100971219     C                   MOVEL     *BLANKS       V1CCAM
032200971219     C     INDCAP        IFNE      *ZEROS
032300971219     C                   MOVEL     INDCAP        V1CCAM
032400971219     C                   END
032500971219     C                   MOVEL     INDSTA        V1CNZM
032600971219     C                   MOVEL     INDPRV        V1CPRM
032700971219     C     INDCAE        IFNE      *ZEROS
032800971219     C     INDCAE        ANDNE     *BLANKS
032900971219     C                   MOVEL     INDCAE        V1CCAM
033000971219     C                   END
033100971219     C                   MOVEL     INDCIT        V1CLOM
033200971219     C                   END
033300971219     C                   END
033400950711     C* Intestazione Assegno / mittente
033500950711     C* se cod. intestaz. assegno <> Cod. cliente lo decodifico
033600950711     C     V1CCDI        IFNE      V1CKSC
033700971219     C                   CLEAR                   TIBS69
033800971219     C                   CLEAR                   CNACO
033900971219     C                   CLEAR                   CNIND
034000971219     C                   CLEAR                   CNCLP
034100971219     C                   CLEAR                   FNCLS
034200971219     C                   MOVEL     ' '           I69TLA
034300971219     C                   MOVEL     KNSIF         I69SIF
034400971219     C                   Z-ADD     V1CCDI        I69KAC
034500971219     C                   Z-ADD     V1CCDI        I69KIN
034600971219     C                   CALL      'TIBS69R'
034700971219     C                   PARM                    TIBS69
034800971219     C                   PARM                    CNACO
034900971219     C                   PARM                    CNIND
035000971219     C                   PARM                    CNCLP
035100971219     C                   PARM                    FNCLS
035200971219     C     O69ERR        IFEQ      *BLANK
035300971219     C                   MOVEL     ACORAG        V1DINT
035400971219     C                   MOVEL     INDVIA        V1CVII
035500971219     C                   MOVEL     *BLANKS       V1CCAI
035600971219     C     INDCAP        IFNE      *ZEROS
035700971219     C                   MOVEL     INDCAP        V1CCAI
035800971219     C                   END
035900971219     C                   MOVEL     INDSTA        V1CNZI
036000971219     C                   MOVEL     INDPRV        V1CPRI
036100971219     C     INDCAE        IFNE      *ZEROS
036200971219     C     INDCAE        ANDNE     *BLANKS
036300971219     C                   MOVEL     INDCAE        V1CCAI
036400971219     C                   END
036500971219     C                   MOVEL     INDCIT        V1CCIT
036600971219     C                   MOVEL     ACOOPZ        BLOC              1
036700971219     C                   ENDIF
036800971219     C                   ENDIF
036900950711     C* Destinatario
037000950711     C                   MOVEL     CSBRSD        V1CRSD
037100950711     C                   MOVEL     CSBPAR        V1CPAR
037200950711     C                   MOVEL     CSBPRO        V1CPRO
037300950711     C* Documento pagamento
037400950711     C                   Z-ADD     CSBNDP        V1CNDP
037500950711     C                   Z-ADD     CSBDDP        G02INV
037600950711     C                   Z-ADD     0             G02DAT
037700950711     C                   MOVEL     '3'           G02ERR
037800950711     C                   CALL      'XSRDA8'
037900950711     C                   PARM                    WLBDA8
038000950711     C                   Z-ADD     G02DAT        V1CDDP
038100950711     C                   MOVEL     CSBFPC        V1CFPC
038200980127     C                   Z-ADD     CSBBNA        V1CBNA
038300980127      * CHIAMO TIBS69 PER DECODIFICA BANCA DI PAGMAENTO
038400980127     C     CSBBNA        IFNE      0
038500980127     C                   CLEAR                   TIBS69
038600980127     C                   CLEAR                   CNACO
038700980127     C                   CLEAR                   CNIND
038800980127     C                   CLEAR                   CNCLP
038900980127     C                   CLEAR                   FNCLS
039000980127     C                   MOVEL     ' '           I69TLA
039100980127     C                   MOVEL     KNSIF         I69SIF
039200980127     C                   Z-ADD     KCB           I69KCC
039300980127     C                   Z-ADD     CSBBNA        I69KAC
039400980127     C                   MOVEL     *BLANKS       ACORAG
039500980127     C                   CALL      'TIBS69R'
039600980127     C                   PARM                    TIBS69
039700980127     C                   PARM                    CNACO
039800980127     C                   PARM                    CNIND
039900980127     C                   PARM                    CNCLP
040000980127     C                   PARM                    FNCLS
040100980127     C     O69ERR        IFEQ      *BLANK
040200980127     C                   MOVEL     ACORAG        V1DBNA
040300950711     C                   END
040400980127     C                   END
040500950711     C* Registrazione pagamento
040600950711     C                   Z-ADD     CSBDRG        G02INV
040700950711     C                   Z-ADD     0             G02DAT
040800950711     C                   MOVEL     '3'           G02ERR
040900950711     C                   CALL      'XSRDA8'
041000950711     C                   PARM                    WLBDA8
041100950711     C                   Z-ADD     G02DAT        V1CDRG
041200950711     C                   Z-ADD     CSBNRG        V1CNRG
041300950711     C                   Z-ADD     CSBCMP        V1CCMP
041400950711     C* Eventuale giacenza
041500950711     C                   Z-ADD     CSBNGC        V1CNGC
041600950711     C                   Z-ADD     CSBDGC        G02INV
041700950711     C                   Z-ADD     0             G02DAT
041800950711     C                   MOVEL     '3'           G02ERR
041900950711     C                   CALL      'XSRDA8'
042000950711     C                   PARM                    WLBDA8
042100950711     C                   Z-ADD     G02DAT        V1CDGC
042200950711     C* Raggruppamento contabilità
042300950711     C                   Z-ADD     CSBRGF        V1CRGF
042400950711     C* Tipo bolla
042500950711     C                   MOVEL     CSBTBL        V1CTBL
042600950711     C* Importo C/A
042700950711     C                   Z-ADD     CSBCAS        V1CCAS
042800950711     C                   MOVEL     CSBVCA        V1CVCA
042900950711     C                   MOVEL     CSBGCA        V1CGCA
043000950711     C* Se bloccati i pagamenti C/Ass del cliente
043100950711     C     BLOC          COMP      'B'                                    38
043200950711     C* Anno di spediaione (CAMPO HIDDEN)
043300950711     C                   Z-ADD     CSBAAS        V1CAAS
043400950711     C*
043500950711     C     V1CPRM        COMP      *BLANKS                            10
043600950711     C     V1CNZM        COMP      *BLANKS                            11
043700950711     C     V1CPRI        COMP      *BLANKS                            14
043800950711     C     V1CNZI        COMP      *BLANKS                            13
043900160419     C*reperisce beneficiario
044000160420     C                   clear                   V1ckfsd
044100160420     C                   clear                   V1ckfs
044200160420     c     kbos          chain     yhbos15l
044300160420     c                   if        %found(yhbos15l)
044400160420     c                   move      bonkfs        v1ckfs
044500160420     C                   mOVEL     bonRAG        V1ckfsd
044600160420     c                   endif
044700160419      *
044800950711     C                   ENDSR
044900950711     C*----------------------------------------------------*
045000950711     C*  Routine gestione operazioni videata spedizione    *
045100950711     C*----------------------------------------------------*
045200950711     C     GESD01        BEGSR
045300950711     C*-                                                   *
045400950711     C                   EXFMT     SC43D01
045500950711     C* Ritorno                                            *
045600950711     C     *INKL         IFEQ      '1'
045700950711     C                   MOVEL     'S'           WFINE
045800950711     C                   MOVE      12            D40CMD
045900950711     C                   GOTO      FINVD1
046000950711     C                   END
046100950711     C* Interrogazione Storico                             *
046200950711     C     *INKD         IFEQ      '1'
046300950711     C                   Z-ADD     V1CLNP        D94LNP
046400950711     C                   Z-ADD     V1CAAS        D94AAS
046500950711     C                   Z-ADD     V1CNRS        D94NRS
046600950711     C                   Z-ADD     V1CNSP        D94NSP
046700950711     C                   MOVEL     TNSC94        KPJBU
046800950711     C                   CALL      'TNSC94R'
046900950711     C                   PARM                    KPJBA
047000950711     C                   MOVEL     KPJBU         TNSC94
047100950711     C     D94LNP        IFEQ      0
047200950711     C                   SETON                                        28
047300950711     C                   MOVEL     ERR(1)        $MSG
047400950711     C                   END
047500950711     C                   GOTO      FINVD1
047600950711     C                   END
047700150709     C* Interrogazione bolle                               *
047800150709     C     *INKg         IFEQ      '1'
047900150709     c                   clear                   TNSB50ds
048000160412     c                   move      'C05'         I50OP0
048100150709     c                   z-add     V1CLNP        d50LNP
048200150709     c                   z-add     V1CAAS        d50AAS
048300150709     c                   z-add     V1CNRS        d50NRS
048400150709     c                   z-add     V1CNSP        d50NSP
048500150709     c                   eval      kpjbu  = TNSB50ds
048600150709     c                   call      'TNSB50R'
048700150709     c                   parm                    kpjba
048800150709     c                   eval      TNSB50ds = kpjbu
048900150709     c                   if        O50err = '1'
049000150709     C                   SETON                                        28
049100150709     C                   MOVEL     ERR(2)        $MSG
049200150709     C                   GOTO      FINVD1
049300160412     c                   else
049400160412     c                   if        o50f12='1'
049500160412     c                   goto      finvd1
049600160412     c                   endif
049700150709     c                   endif
049800150709     c                   endif
049900971219     C* Interrogazione DATI INFRAGRUPPO                    *
050000971219     C     *INKH         IFEQ      '1'
050100971219     C                   Z-ADD     V1CLNP        D44LNP
050200971219     C                   Z-ADD     V1CAAS        D44AAS
050300971219     C                   Z-ADD     V1CNRS        D44NRS
050400971219     C                   Z-ADD     V1CNSP        D44NSP
050500971219     C                   MOVEL     TNSC44        KPJBU
050600971219     C                   CALL      'TNSC44R'
050700971219     C                   PARM                    KPJBA
050800971219     C                   MOVEL     KPJBU         TNSC94
050900971219     C                   GOTO      FINVD1
051000971219     C                   END
051100120209     c* interroga multiassegni
051200120209     c                   if        *inki
051300120210     c                   clear                   tnsc30ds
051400120210     c                   eval      sc30CMTI = 'N'
051500120210     c                   eval      sc30AMBI = 'S'
051600120210     c                   eval      sc30TPCI = 'V'
051700120210     c                   eval      sc30AASI = v1caas
051800120210     c                   eval      sc30LNPI = v1clnp
051900120210     c                   eval      sc30NRSI = v1cnrs
052000120210     c                   eval      sc30NSPI = v1cnsp
052100120210     c                   movel(p)  tnsc30ds      kpjbu
052200120210     c                   call      'TNSC30R'
052300120209     c                   parm                    kpjba
052400120521     C                   GOTO      FINVD1
052500120209     c                   end
052600950711     C*                                                    *
052700950711     C                   MOVEL     'S'           WFINE
052800950711     C*                                                    *
052900950711     C     FINVD1        ENDSR
053000950711     C*----------------------------------------------------*
053100950711     C*  Routine gestione operazioni seconda videata       *
053200950711     C*----------------------------------------------------*
053300950711     C     GESD02        BEGSR
053400950711     C*-                                                   *
053500950711     C                   EXFMT     SC43D02
053600950711     C*                                                    *
053700950711     C* Fine Programma                                     *
053800950711     C                   MOVEL     'S'           WFINE
053900950711     C*-                                                   *
054000950711     C                   ENDSR
054100950711      *---------------------------------------------------------------*
054200950711     C     *INZSR        BEGSR
054300950711      *---------------------------------------------------------------*
054400950711     C     *ENTRY        PLIST
054500950711     C                   PARM                    KPJBA
054600950711     C                   MOVEL     KPJBU         TNSC40
054700950711     C*
054800950711     C                   Z-ADD     1             CODUT
054900950711     C                   CALL      'X§PARUT'
055000950711     C                   PARM                    UT§DSE
055100950711     C                   MOVEL     RAGUT         RSUT             20
055200950711     C                   MOVEL     REC80         CNCR80
055300950711     C                   MOVEL     KNMUS         §SEI              6
055400950711     C*
055500950711     C                   Z-ADD     1             X                 3 0
055600950711     C                   Z-ADD     0             KCB
055700950711      *  Ricerco capoconto clienti
055800950711     C     X             DOWLE     50
055900950711     C     KCB           ANDEQ     0
056000950711     C                   MOVE      TCU(X)        TCUDS
056100950711     C     F56           IFEQ      'CG'
056200950711     C     F34           IFEQ      '06'
056300950711     C                   Z-ADD     KCU(X)        KCB               4 0
056400950711     C                   END
056500950711     C                   END
056600950711     C                   ADD       1             X
056700950711     C                   END
056800950711     C*----------------------------------------------------*
056900950711     C*  Definizione CHIAVI
057000950711     C     KTAB          KLIST
057100950711     C                   KFLD                    KKUT
057200950711     C                   KFLD                    KCOD
057300950711     C     KCSA          KLIST
057400950711     C                   KFLD                    KAAS
057500950711     C                   KFLD                    KLNP
057600950711     C                   KFLD                    KNRS
057700950711     C                   KFLD                    KNSP
057800950711     C     KCSB          KLIST
057900950711     C                   KFLD                    KAAS
058000950711     C                   KFLD                    KLNP
058100950711     C                   KFLD                    KNRS
058200950711     C                   KFLD                    KNSP
058300160419     C     Kbos          KLIST
058400160419     c                   kfld                    wsoc              3
058500160419     c                   kfld                    wkcc              6
058600160419     C                   KFLD                    csbLNP
058700160419     C                   KFLD                    csbAAS
058800160419     C                   KFLD                    csbNRS
058900160419     C                   KFLD                    csbNSP
059000160419     c                   move      '201'         wsoc              3
059100160419     c                   move      '000465'      wkcc              6
059200950711     C     KABI1         KLIST
059300950711     C                   KFLD                    KABI
059400950711     C                   KFLD                    KCAB
059500950711     C                   Z-ADD     1             KKUT
059600100219     C     Kar5          KLIST
059700100219     C                   KFLD                    KAAS
059800100219     C                   KFLD                    KLNP
059900100219     C                   KFLD                    KNRS
060000100219     C                   KFLD                    KNSP
060100100219     C                   KFLD                    ar5trd
060200100219     c                   eval      ar5trd = 'GEN'
060300950711     C*----------------------------------------------------*
060400950711     C*  Definizione variabili
060500950711     C     *LIKE         DEFINE    CSBAAS        KAAS
060600950711     C     *LIKE         DEFINE    CSBLNP        KLNP
060700950711     C     *LIKE         DEFINE    CSBNRS        KNRS
060800950711     C     *LIKE         DEFINE    CSBNSP        KNSP
060900980127     C     *LIKE         DEFINE    TBLKUT        KKUT
061000950711     C     *LIKE         DEFINE    TBLCOD        KCOD
061100950711     C     *LIKE         DEFINE    ABIABI        KABI
061200950711     C     *LIKE         DEFINE    ABICAB        KCAB
061300950711     C*----------------------------------------------------*
061400950711     C* Carico tabella DECODIFICA STATI
061500950711     C                   Z-ADD     0             X
061600950711     C                   MOVEL     '4S'          KCOD
061700950711     C     KTAB          CHAIN     TABEL                              31
061800950711     C     *IN31         DOWEQ     '0'
061900950711     C     X             ANDLT     50
062000950711     C     TBLFLG        IFEQ      ' '
062100950711     C                   ADD       1             X
062200950724     C                   MOVE      TBLKEY        C4S(X)
062300950711     C                   MOVEL     TBLUNI        D4S(X)
062400950711     C                   END
062500950711     C     KTAB          READE     TABEL                                  31
062600950711     C                   END
062700950711     C*
062800950711     C                   MOVEL     'N'           WFINE             1
062900950711     C                   CLEAR                   WLBDA8
063000950711     C                   CLEAR                   TNSC94
063100950711     C*----------------------------------------------------*
063200950711     C                   ENDSR
063300950711**
063400950711Non esistono variazioni su questo contrassegno
063500150709Bolla non trovata in interrogazione
