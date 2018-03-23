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
001600950711?     *--------------------------------------------------------------*
001700950711     D C4S             S              1  0 DIM(50)
001800950711     D D4S             S             20    DIM(50)
001900150709     D ERR             S             70    DIM(2) CTDATA PERRCD(1)
002000120210     D tnsc30ds      E DS
002100150709     D tnsb50ds      E DS
002200120209     D CNCR80        E DS
002300950711     D KPJBA         E DS
002400950711     D  LIBSYS                92    100
002500950711     D  MSGERR               453    502
002600971219     D TNSC40        E DS                  EXTNAME(TNSC40DS)
002700971219     D TNSC44        E DS                  EXTNAME(TNSC44DS)
002800950711     D TNSC94        E DS                  EXTNAME(TNSC94DS)
002900971222     D TNSC96        E DS                  EXTNAME(TNSC96DS)
003000100219     D dar5gen       E DS
003100971219     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003200971219     D CNACO         E DS                  EXTNAME(CNACO00F)
003300971219     D CNIND         E DS                  EXTNAME(CNIND00F)
003400971219     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003500971219     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003600950711     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003700950711     D  TCU                  398    697
003800950711     D                                     DIM(50)
003900950711     D  KCU                  698    847P 0
004000950711     D                                     DIM(50)
004100950711     D                                     PACKEVEN
004200950711      *
004300950711     D TCUDS           DS
004400950711     D  F34                    3      4
004500950711     D  F56                    5      6
004600950711      *
004700950711     D WLBDA8          DS
004800950711     D  G02DAT                 1      8  0
004900950711     D  G02INV                 9     16  0
005000950711     D  G02ERR                17     17
005100950711     D  G02TGI                18     22  0
005200950711     D                SDS
005300950711     D  NOMPGM                 1     10
005400950711     I*--------------------------------------------------------------*
005500950711     IAZORG01L  AA
005600950711     I                             P    4    5 0ORGFIL
005700950711     I                                 14   33  ORGDES
005800950711     C*-------------------------------------------------------*
005900950711     C*  CICLO PRINCIPALE
006000950711     C*-------------------------------------------------------*
006100950711     C                   EXSR      CNTSPE
006200950711     C*  Loop gestione videata
006300950711     C     WFINE         DOWNE     'S'
006400950711     C     WTPVID        CASEQ     '1'           GESD01
006500950711     C     WTPVID        CASEQ     '2'           GESD02
006600950711     C                   END
006700010228     C                   END
006800971219     C*
006900971219     C* RICHIAMO PGM X CHIUSURA FILES
007000971219     C                   CLEAR                   TIBS69
007100971219     C                   MOVEL     'C'           I69TLA
007200971219     C                   CALL      'TIBS69R'
007300971219     C                   PARM                    TIBS69
007400971219     C                   PARM                    CNACO
007500971219     C                   PARM                    CNIND
007600971219     C                   PARM                    CNCLP
007700971219     C                   PARM                    FNCLS
007800950711     C*
007900000224     C                   MOVEL     TNSC40        KPJBU
008000950711     C                   SETON                                        LR
008100950711     C*-------------------------------------------------------*
008200950711     C*  Controllo se esiste spedizione
008300950711     C*-------------------------------------------------------*
008400950711     C     CNTSPE        BEGSR
008500950711     C*
008600950711     C                   MOVEL     '2'           WTPVID            1
008700950711     C                   Z-ADD     D40AAS        KAAS
008800950711     C                   Z-ADD     D40LNP        KLNP
008900950711     C                   Z-ADD     D40NRS        KNRS
009000950711     C                   Z-ADD     D40NSP        KNSP
009100950711     C     KCSB          CHAIN     TNCSB03L                           31
009200971219     C     *IN31         IFEQ      '0'
009300950711     C                   MOVEL     D40NAS        WNRA             10
009400010228     C*                    MOVELCSBINC    SAVINC 75
009500010228     C*                    MOVELCSBPAG    SAVPAG 46
009600950711     C*----------------------------------------------------*
009700950711     C     D40ABI        IFEQ      *ZEROS
009800950711     C     D40ABI        OREQ      CSBABI
009900950711     C     D40CAB        ANDNE     *ZEROS
010000950711     C     D40CAB        ANDEQ     CSBCAB
010100950711     C     D40ABI        OREQ      CSBABI
010200980122     C     WNRA          ANDGT     *ZEROS
010300950711     C     WNRA          ANDEQ     CSBNRA
010400980122     C     D40ABI        OREQ      CSBABI
010500980122     C     D40CAB        ANDEQ     *ZEROS
010600980122     C     WNRA          ANDLE     *ZEROS
010700990611     C* Controllo se C/Assegno Infragruppo
010800971222     C* devo richiamare il pgm per riconoscere i dati infragruppo
010900971222     C                   CLEAR                   TNSC96
011000971223     C                   MOVEL     'L'           D96TLA
011100971222     C                   MOVEL     KNSIF         D96NSI
011200010301     C                   Z-ADD     CSBAAS        D96AAS
011300010301     C                   Z-ADD     CSBLNP        D96LNP
011400010301     C                   Z-ADD     CSBNRS        D96NRS
011500010301     C                   Z-ADD     CSBNSP        D96NSP
011600971222     C* Richiamo pgm
011700971222     C                   CALL      'TNSC96R'
011800990611     C                   PARM                    KPJBA
011900990611     C                   PARM                    TNSC96
012000950711     C*----------------------------------------------------*
012100950711     C* Selezione per stato C/Assegno
012200950711     C     D40TCR        IFEQ      ' '
012300950711     C     D40TCR        OREQ      'D'
012400010228     C     CSBDDC        ANDEQ     0
012500950711     C     D40TCR        OREQ      'G'
012600010228     C     CSBSTA        ANDEQ     2
012700950711     C     D40TCR        OREQ      'A'
012800950711     C     CSBSTA        ANDEQ     9
012900960911     C     D40TCR        OREQ      'T'
013000960911     C     CSBSTA        ANDEQ     6
013100960911     C     D40TCR        OREQ      'R'
013200960911     C     CSBSTA        ANDEQ     7
013300960911     C     D40TCR        OREQ      'O'
013400960911     C     CSBSTA        ANDEQ     8
013500950711     C     D40TCR        OREQ      'S'
013600950711     C     CSBSTA        ANDEQ     5
013700010112     C     D40TCR        OREQ      '3'
013800010112     C     CSBSTA        ANDEQ     3
013900950711     C     D40TCR        OREQ      'I'
014000950711     C     CSBDDC        ANDGT     0
014100950711     C     CSBDDP        ANDEQ     0
014200950711     C     D40TCR        OREQ      'P'
014300950711     C     CSBDDP        ANDGT     0
014400950711     C                   EXSR      INZD01
014500950711     C                   END
014600950711     C                   END
014700950711     C*----------------------------------------------------*
014800950711     C                   END
014900950711     C*----------------------------------------------------*
015000950711     C                   ENDSR
015100950711     C*-------------------------------------------------------*
015200950711     C*  Inizializzazione videata
015300950711     C*-------------------------------------------------------*
015400950711     C     INZD01        BEGSR
015500950711     C*
015600950711     C                   MOVEL     '1'           WTPVID
015700971219      *
015800971231     C     D96TRV        IFEQ      'S'
015900971219     C                   SETON                                        08
016000971231     C                   ENDIF
016100950711     C*
016200950711     C                   MOVEL     CSBSTA        V1CSTA
016300950711     C                   MOVEL     *BLANKS       V1DSTA
016400950711     C*  Decodifico stato C/Ass.
016500950711     C                   Z-ADD     1             X
016600950711     C     CSBSTA        LOOKUP    C4S(X)                                 32
016700950711     C     *IN32         IFEQ      '1'
016800950711     C                   MOVEL     D4S(X)        V1DSTA
016900950711     C                   END
017000950711     C* Trasmesso / da trasmettere / ricevuto solo se pagato
017100950711     C                   SETOFF                                       85
017200950711     C                   MOVEL     *BLANKS       V1CTRA
017300950711     C     CSBNDP        IFNE      0
017400950711     C                   SETON                                        85
017500950711     C     CSBFTR        IFEQ      *BLANKS
017600950711     C                   MOVEL     'DA TRASM'    V1CTRA
017700950711     C                   MOVE      'ETTERE'      V1CTRA
017800950711     C                   END
017900950711     C     CSBFTR        IFEQ      'R'
018000950711     C                   MOVEL     'RICEVUTO'    V1CTRA
018100950711     C                   END
018200950711     C     CSBFTR        IFEQ      'T'
018300950711     C                   MOVEL     'TRASMESS'    V1CTRA
018400950711     C                   MOVE      'O     '      V1CTRA
018500950711     C                   END
018600950711     C                   END
018700950711     C* Linea di partenza + decodifica
018800950711     C                   Z-ADD     CSBLNP        V1CLNP
018900950711     C                   MOVEL     *BLANKS       ORGDES
019000950711     C                   Z-ADD     CSBLNP        ORGFIL
019100950711     C     ORGFIL        CHAIN     AZORG01L                           31
019200950711     C                   MOVEL     ORGDES        V1DLNP
019300950711     C* Nr° serie + Spedizione
019400950711     C                   Z-ADD     CSBNRS        V1CNRS
019500950711     C                   Z-ADD     CSBNSP        V1CNSP
019600950711     C* Codice cliente
019700950711     C                   Z-ADD     CSBKSC        V1CKSC
019800950711     C* data spedizione
019900950711     C                   MOVEL     CSBAAS        G02INV
020000950711     C                   MOVE      CSBMGS        G02INV
020100950711     C                   Z-ADD     0             G02DAT
020200950711     C                   MOVEL     '3'           G02ERR
020300950711     C                   CALL      'XSRDA8'
020400950711     C                   PARM                    WLBDA8
020500950711     C                   Z-ADD     G02DAT        V1CDSP
020600950711     C* data documento
020700950711     C                   Z-ADD     CSBDDC        G02INV
020800950711     C                   Z-ADD     0             G02DAT
020900950711     C                   MOVEL     '3'           G02ERR
021000950711     C                   CALL      'XSRDA8'
021100950711     C                   PARM                    WLBDA8
021200950711     C                   Z-ADD     G02DAT        V1CDDC
021300950711     C* Linea arrivo + decodifica
021400950711     C                   Z-ADD     CSBLNA        V1CLNA
021500950711     C                   Z-ADD     CSBLNA        ORGFIL
021600950711     C                   MOVEL     *BLANKS       ORGDES
021700950711     C     ORGFIL        CHAIN     AZORG01L                           50
021800950711     C                   MOVEL     ORGDES        V1DLNA
021900950711     C* Riferim. mittente
022000950711     C                   Z-ADD     CSBRMN        V1CRMN
022100950711     C                   MOVEL     CSBRMA        V1CRMA
022200950711     C* Distinta
022300950711     C                   Z-ADD     CSBNDT        V1CNDT
022400950711     C                   Z-ADD     CSBDTA        G02INV
022500950711     C                   Z-ADD     0             G02DAT
022600950711     C                   MOVEL     '3'           G02ERR
022700950711     C                   CALL      'XSRDA8'
022800950711     C                   PARM                    WLBDA8
022900950711     C                   Z-ADD     G02DAT        V1CDTA
023000950711     C* Assegno
023100950711     C                   MOVEL     CSBNRA        V1CNRA
023200950711     C                   Z-ADD     CSBDTE        G02INV
023300950711     C                   Z-ADD     0             G02DAT
023400950711     C                   MOVEL     '3'           G02ERR
023500950711     C                   CALL      'XSRDA8'
023600950711     C                   PARM                    WLBDA8
023700950711     C                   Z-ADD     G02DAT        V1CDTE
023800950711     C                   MOVEL     CSBTPI        V1CTPI
023900950711     C                   MOVEL     CSBTPA        V1CTPA
024000950711     C                   MOVEL     CSBFUS        V1CFUS
024100950711     C                   MOVEL     CSBMAS        V1CMAS
024200120209     c* abilito comando per multiassegno
024300120220     c                   if        %subst(csbnra: 10: 1)=' ' or csbtpi <> 'M'
024400120209     c                   setoff                                       09
024500120209     c                   else
024600120209     c                   seton                                        09
024700120209     c                   end
024800100219     C* verifico se è stato variato il tipo incasso bollettato
024900100224     c                   setoff                                       8889
025000100224     c                   clear                   v1cticmb
025100100224     c                   clear                   v1ctici
025200100224     c                   if        kaas > 2009
025300100224     c                   seton                                        88
025400100219     c     kar5          chain     fiar531c
025500100219     c                   if        %found(fiar531c)
025600100219     c                   movel     ar5uni        dar5gen
025700100224     c                   movel     §AR5TICI      v1ctici
025800100219     c                   if        §AR5FLGMB = 'S'
025900100224     c                   seton                                        89
026000100224     c                   movel     §AR5TICMB     v1cticmb
026100100219     c                   end
026200100219     c                   end
026300100224     c                   end
026400100219     C* Registrazione incasso
026500950711     C                   Z-ADD     CSBDRI        G02INV
026600950711     C                   Z-ADD     0             G02DAT
026700950711     C                   MOVEL     '3'           G02ERR
026800950711     C                   CALL      'XSRDA8'
026900950711     C                   PARM                    WLBDA8
027000950711     C                   Z-ADD     G02DAT        V1CDRI
027100950711     C                   Z-ADD     CSBNRI        V1CNRI
027200950711     C* Codice Intestazione Assegno
027300950711     C                   Z-ADD     CSBCDI        V1CCDI
027400950711     C* ABI / CAB / Eventuale decodifica Banca
027500950711     C                   Z-ADD     CSBABI        V1CABI
027600950711     C                   Z-ADD     CSBCAB        V1CCAB
027700950711     C                   Z-ADD     CSBABI        KABI
027800950711     C                   Z-ADD     CSBCAB        KCAB
027900950711     C                   MOVEL     *BLANKS       ABIIST
028000950711     C     KABI1         CHAIN     CNABI000                           31
028100950711     C                   MOVEL     ABIIST        V1CDES
028200950711     C* Se cliente vario lo decodifico da blcsa
028300950711     C                   MOVEL     *BLANKS       ACOOPZ
028400950711     C                   MOVE      V1CKSC        APP               4
028500950711     C     APP           IFEQ      '8888'
028600950711     C     APP           OREQ      '9999'
028700950711     C                   MOVEL     *BLANKS       CSARSD
028800950711     C                   MOVEL     *BLANKS       CSAVID
028900950711     C                   MOVEL     *BLANKS       CSACAD
029000950711     C                   MOVEL     *BLANKS       CSACID
029100950711     C                   MOVEL     *BLANKS       CSAPRM
029200950711     C                   MOVEL     *BLANKS       CSANZM
029300950711     C     KCSA          CHAIN     TNCSA01L                           31
029400950711     C                   MOVEL     CSARSD        V1DMIO
029500950711     C                   MOVEL     CSAVID        V1CVIM
029600950711     C                   MOVEL     CSACAD        V1CCAM
029700950711     C                   MOVEL     CSACID        V1CLOM
029800950711     C                   MOVEL     CSAPRM        V1CPRM
029900950711     C                   MOVEL     CSANZM        V1CNZM
030000950711     C                   ELSE
030100971219      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
030200971219     C                   CLEAR                   TIBS69
030300971219     C                   CLEAR                   CNACO
030400971219     C                   CLEAR                   CNIND
030500971219     C                   CLEAR                   CNCLP
030600971219     C                   CLEAR                   FNCLS
030700971219     C                   MOVEL     ' '           I69TLA
030800971219     C                   MOVEL     KNSIF         I69SIF
030900971219     C                   Z-ADD     V1CKSC        I69KAC
031000971219     C                   Z-ADD     V1CKSC        I69KIN
031100971219     C                   CALL      'TIBS69R'
031200971219     C                   PARM                    TIBS69
031300971219     C                   PARM                    CNACO
031400971219     C                   PARM                    CNIND
031500971219     C                   PARM                    CNCLP
031600971219     C                   PARM                    FNCLS
031700971219     C     O69ERR        IFEQ      *BLANK
031800971219     C                   MOVEL     ACORAG        V1DMIO
031900971219     C                   MOVEL     INDVIA        V1CVIM
032000971219     C                   MOVEL     *BLANKS       V1CCAM
032100971219     C     INDCAP        IFNE      *ZEROS
032200971219     C                   MOVEL     INDCAP        V1CCAM
032300971219     C                   END
032400971219     C                   MOVEL     INDSTA        V1CNZM
032500971219     C                   MOVEL     INDPRV        V1CPRM
032600971219     C     INDCAE        IFNE      *ZEROS
032700971219     C     INDCAE        ANDNE     *BLANKS
032800971219     C                   MOVEL     INDCAE        V1CCAM
032900971219     C                   END
033000971219     C                   MOVEL     INDCIT        V1CLOM
033100971219     C                   END
033200971219     C                   END
033300950711     C* Intestazione Assegno / mittente
033400950711     C* se cod. intestaz. assegno <> Cod. cliente lo decodifico
033500950711     C     V1CCDI        IFNE      V1CKSC
033600971219     C                   CLEAR                   TIBS69
033700971219     C                   CLEAR                   CNACO
033800971219     C                   CLEAR                   CNIND
033900971219     C                   CLEAR                   CNCLP
034000971219     C                   CLEAR                   FNCLS
034100971219     C                   MOVEL     ' '           I69TLA
034200971219     C                   MOVEL     KNSIF         I69SIF
034300971219     C                   Z-ADD     V1CCDI        I69KAC
034400971219     C                   Z-ADD     V1CCDI        I69KIN
034500971219     C                   CALL      'TIBS69R'
034600971219     C                   PARM                    TIBS69
034700971219     C                   PARM                    CNACO
034800971219     C                   PARM                    CNIND
034900971219     C                   PARM                    CNCLP
035000971219     C                   PARM                    FNCLS
035100971219     C     O69ERR        IFEQ      *BLANK
035200971219     C                   MOVEL     ACORAG        V1DINT
035300971219     C                   MOVEL     INDVIA        V1CVII
035400971219     C                   MOVEL     *BLANKS       V1CCAI
035500971219     C     INDCAP        IFNE      *ZEROS
035600971219     C                   MOVEL     INDCAP        V1CCAI
035700971219     C                   END
035800971219     C                   MOVEL     INDSTA        V1CNZI
035900971219     C                   MOVEL     INDPRV        V1CPRI
036000971219     C     INDCAE        IFNE      *ZEROS
036100971219     C     INDCAE        ANDNE     *BLANKS
036200971219     C                   MOVEL     INDCAE        V1CCAI
036300971219     C                   END
036400971219     C                   MOVEL     INDCIT        V1CCIT
036500971219     C                   MOVEL     ACOOPZ        BLOC              1
036600971219     C                   ENDIF
036700971219     C                   ENDIF
036800950711     C* Destinatario
036900950711     C                   MOVEL     CSBRSD        V1CRSD
037000950711     C                   MOVEL     CSBPAR        V1CPAR
037100950711     C                   MOVEL     CSBPRO        V1CPRO
037200950711     C* Documento pagamento
037300950711     C                   Z-ADD     CSBNDP        V1CNDP
037400950711     C                   Z-ADD     CSBDDP        G02INV
037500950711     C                   Z-ADD     0             G02DAT
037600950711     C                   MOVEL     '3'           G02ERR
037700950711     C                   CALL      'XSRDA8'
037800950711     C                   PARM                    WLBDA8
037900950711     C                   Z-ADD     G02DAT        V1CDDP
038000950711     C                   MOVEL     CSBFPC        V1CFPC
038100980127     C                   Z-ADD     CSBBNA        V1CBNA
038200980127      * CHIAMO TIBS69 PER DECODIFICA BANCA DI PAGMAENTO
038300980127     C     CSBBNA        IFNE      0
038400980127     C                   CLEAR                   TIBS69
038500980127     C                   CLEAR                   CNACO
038600980127     C                   CLEAR                   CNIND
038700980127     C                   CLEAR                   CNCLP
038800980127     C                   CLEAR                   FNCLS
038900980127     C                   MOVEL     ' '           I69TLA
039000980127     C                   MOVEL     KNSIF         I69SIF
039100980127     C                   Z-ADD     KCB           I69KCC
039200980127     C                   Z-ADD     CSBBNA        I69KAC
039300980127     C                   MOVEL     *BLANKS       ACORAG
039400980127     C                   CALL      'TIBS69R'
039500980127     C                   PARM                    TIBS69
039600980127     C                   PARM                    CNACO
039700980127     C                   PARM                    CNIND
039800980127     C                   PARM                    CNCLP
039900980127     C                   PARM                    FNCLS
040000980127     C     O69ERR        IFEQ      *BLANK
040100980127     C                   MOVEL     ACORAG        V1DBNA
040200950711     C                   END
040300980127     C                   END
040400950711     C* Registrazione pagamento
040500950711     C                   Z-ADD     CSBDRG        G02INV
040600950711     C                   Z-ADD     0             G02DAT
040700950711     C                   MOVEL     '3'           G02ERR
040800950711     C                   CALL      'XSRDA8'
040900950711     C                   PARM                    WLBDA8
041000950711     C                   Z-ADD     G02DAT        V1CDRG
041100950711     C                   Z-ADD     CSBNRG        V1CNRG
041200950711     C                   Z-ADD     CSBCMP        V1CCMP
041300950711     C* Eventuale giacenza
041400950711     C                   Z-ADD     CSBNGC        V1CNGC
041500950711     C                   Z-ADD     CSBDGC        G02INV
041600950711     C                   Z-ADD     0             G02DAT
041700950711     C                   MOVEL     '3'           G02ERR
041800950711     C                   CALL      'XSRDA8'
041900950711     C                   PARM                    WLBDA8
042000950711     C                   Z-ADD     G02DAT        V1CDGC
042100950711     C* Raggruppamento contabilità
042200950711     C                   Z-ADD     CSBRGF        V1CRGF
042300950711     C* Tipo bolla
042400950711     C                   MOVEL     CSBTBL        V1CTBL
042500950711     C* Importo C/A
042600950711     C                   Z-ADD     CSBCAS        V1CCAS
042700950711     C                   MOVEL     CSBVCA        V1CVCA
042800950711     C                   MOVEL     CSBGCA        V1CGCA
042900950711     C* Se bloccati i pagamenti C/Ass del cliente
043000950711     C     BLOC          COMP      'B'                                    38
043100950711     C* Anno di spediaione (CAMPO HIDDEN)
043200950711     C                   Z-ADD     CSBAAS        V1CAAS
043300950711     C*
043400950711     C     V1CPRM        COMP      *BLANKS                            10
043500950711     C     V1CNZM        COMP      *BLANKS                            11
043600950711     C     V1CPRI        COMP      *BLANKS                            14
043700950711     C     V1CNZI        COMP      *BLANKS                            13
043800950711     C*
043900950711     C                   ENDSR
044000950711     C*----------------------------------------------------*
044100950711     C*  Routine gestione operazioni videata spedizione    *
044200950711     C*----------------------------------------------------*
044300950711     C     GESD01        BEGSR
044400950711     C*-                                                   *
044500950711     C                   EXFMT     SC43D01
044600950711     C* Ritorno                                            *
044700950711     C     *INKL         IFEQ      '1'
044800950711     C                   MOVEL     'S'           WFINE
044900950711     C                   MOVE      12            D40CMD
045000950711     C                   GOTO      FINVD1
045100950711     C                   END
045200950711     C* Interrogazione Storico                             *
045300950711     C     *INKD         IFEQ      '1'
045400950711     C                   Z-ADD     V1CLNP        D94LNP
045500950711     C                   Z-ADD     V1CAAS        D94AAS
045600950711     C                   Z-ADD     V1CNRS        D94NRS
045700950711     C                   Z-ADD     V1CNSP        D94NSP
045800950711     C                   MOVEL     TNSC94        KPJBU
045900950711     C                   CALL      'TNSC94R'
046000950711     C                   PARM                    KPJBA
046100950711     C                   MOVEL     KPJBU         TNSC94
046200950711     C     D94LNP        IFEQ      0
046300950711     C                   SETON                                        28
046400950711     C                   MOVEL     ERR(1)        $MSG
046500950711     C                   END
046600950711     C                   GOTO      FINVD1
046700950711     C                   END
046800150709     C* Interrogazione bolle                               *
046900150709     C     *INKg         IFEQ      '1'
047000150709     c                   clear                   TNSB50ds
047100160412     c                   move      'C05'         I50OP0
047200150709     c                   z-add     V1CLNP        d50LNP
047300150709     c                   z-add     V1CAAS        d50AAS
047400150709     c                   z-add     V1CNRS        d50NRS
047500150709     c                   z-add     V1CNSP        d50NSP
047600150709     c                   eval      kpjbu  = TNSB50ds
047700150709     c                   call      'TNSB50R'
047800150709     c                   parm                    kpjba
047900150709     c                   eval      TNSB50ds = kpjbu
048000150709     c                   if        O50err = '1'
048100150709     C                   SETON                                        28
048200150709     C                   MOVEL     ERR(2)        $MSG
048300150709     C                   GOTO      FINVD1
048400160412     c                   else
048500160412     c                   if        o50f12='1'
048600160412     c                   goto      finvd1
048700160412     c                   endif
048800150709     c                   endif
048900150709     c                   endif
049000971219     C* Interrogazione DATI INFRAGRUPPO                    *
049100971219     C     *INKH         IFEQ      '1'
049200971219     C                   Z-ADD     V1CLNP        D44LNP
049300971219     C                   Z-ADD     V1CAAS        D44AAS
049400971219     C                   Z-ADD     V1CNRS        D44NRS
049500971219     C                   Z-ADD     V1CNSP        D44NSP
049600971219     C                   MOVEL     TNSC44        KPJBU
049700971219     C                   CALL      'TNSC44R'
049800971219     C                   PARM                    KPJBA
049900971219     C                   MOVEL     KPJBU         TNSC94
050000971219     C                   GOTO      FINVD1
050100971219     C                   END
050200120209     c* interroga multiassegni
050300120209     c                   if        *inki
050400120210     c                   clear                   tnsc30ds
050500120210     c                   eval      sc30CMTI = 'N'
050600120210     c                   eval      sc30AMBI = 'S'
050700120210     c                   eval      sc30TPCI = 'V'
050800120210     c                   eval      sc30AASI = v1caas
050900120210     c                   eval      sc30LNPI = v1clnp
051000120210     c                   eval      sc30NRSI = v1cnrs
051100120210     c                   eval      sc30NSPI = v1cnsp
051200120210     c                   movel(p)  tnsc30ds      kpjbu
051300120210     c                   call      'TNSC30R'
051400120209     c                   parm                    kpjba
051500120521     C                   GOTO      FINVD1
051600120209     c                   end
051700950711     C*                                                    *
051800950711     C                   MOVEL     'S'           WFINE
051900950711     C*                                                    *
052000950711     C     FINVD1        ENDSR
052100950711     C*----------------------------------------------------*
052200950711     C*  Routine gestione operazioni seconda videata       *
052300950711     C*----------------------------------------------------*
052400950711     C     GESD02        BEGSR
052500950711     C*-                                                   *
052600950711     C                   EXFMT     SC43D02
052700950711     C*                                                    *
052800950711     C* Fine Programma                                     *
052900950711     C                   MOVEL     'S'           WFINE
053000950711     C*-                                                   *
053100950711     C                   ENDSR
053200950711      *---------------------------------------------------------------*
053300950711     C     *INZSR        BEGSR
053400950711      *---------------------------------------------------------------*
053500950711     C     *ENTRY        PLIST
053600950711     C                   PARM                    KPJBA
053700950711     C                   MOVEL     KPJBU         TNSC40
053800950711     C*
053900950711     C                   Z-ADD     1             CODUT
054000950711     C                   CALL      'X§PARUT'
054100950711     C                   PARM                    UT§DSE
054200950711     C                   MOVEL     RAGUT         RSUT             20
054300950711     C                   MOVEL     REC80         CNCR80
054400950711     C                   MOVEL     KNMUS         §SEI              6
054500950711     C*
054600950711     C                   Z-ADD     1             X                 3 0
054700950711     C                   Z-ADD     0             KCB
054800950711      *  Ricerco capoconto clienti
054900950711     C     X             DOWLE     50
055000950711     C     KCB           ANDEQ     0
055100950711     C                   MOVE      TCU(X)        TCUDS
055200950711     C     F56           IFEQ      'CG'
055300950711     C     F34           IFEQ      '06'
055400950711     C                   Z-ADD     KCU(X)        KCB               4 0
055500950711     C                   END
055600950711     C                   END
055700950711     C                   ADD       1             X
055800950711     C                   END
055900950711     C*----------------------------------------------------*
056000950711     C*  Definizione CHIAVI
056100950711     C     KTAB          KLIST
056200950711     C                   KFLD                    KKUT
056300950711     C                   KFLD                    KCOD
056400950711     C     KCSA          KLIST
056500950711     C                   KFLD                    KAAS
056600950711     C                   KFLD                    KLNP
056700950711     C                   KFLD                    KNRS
056800950711     C                   KFLD                    KNSP
056900950711     C     KCSB          KLIST
057000950711     C                   KFLD                    KAAS
057100950711     C                   KFLD                    KLNP
057200950711     C                   KFLD                    KNRS
057300950711     C                   KFLD                    KNSP
057400950711     C     KABI1         KLIST
057500950711     C                   KFLD                    KABI
057600950711     C                   KFLD                    KCAB
057700950711     C                   Z-ADD     1             KKUT
057800100219     C     Kar5          KLIST
057900100219     C                   KFLD                    KAAS
058000100219     C                   KFLD                    KLNP
058100100219     C                   KFLD                    KNRS
058200100219     C                   KFLD                    KNSP
058300100219     C                   KFLD                    ar5trd
058400100219     c                   eval      ar5trd = 'GEN'
058500950711     C*----------------------------------------------------*
058600950711     C*  Definizione variabili
058700950711     C     *LIKE         DEFINE    CSBAAS        KAAS
058800950711     C     *LIKE         DEFINE    CSBLNP        KLNP
058900950711     C     *LIKE         DEFINE    CSBNRS        KNRS
059000950711     C     *LIKE         DEFINE    CSBNSP        KNSP
059100980127     C     *LIKE         DEFINE    TBLKUT        KKUT
059200950711     C     *LIKE         DEFINE    TBLCOD        KCOD
059300950711     C     *LIKE         DEFINE    ABIABI        KABI
059400950711     C     *LIKE         DEFINE    ABICAB        KCAB
059500950711     C*----------------------------------------------------*
059600950711     C* Carico tabella DECODIFICA STATI
059700950711     C                   Z-ADD     0             X
059800950711     C                   MOVEL     '4S'          KCOD
059900950711     C     KTAB          CHAIN     TABEL                              31
060000950711     C     *IN31         DOWEQ     '0'
060100950711     C     X             ANDLT     50
060200950711     C     TBLFLG        IFEQ      ' '
060300950711     C                   ADD       1             X
060400950724     C                   MOVE      TBLKEY        C4S(X)
060500950711     C                   MOVEL     TBLUNI        D4S(X)
060600950711     C                   END
060700950711     C     KTAB          READE     TABEL                                  31
060800950711     C                   END
060900950711     C*
061000950711     C                   MOVEL     'N'           WFINE             1
061100950711     C                   CLEAR                   WLBDA8
061200950711     C                   CLEAR                   TNSC94
061300950711     C*----------------------------------------------------*
061400950711     C                   ENDSR
061500950711**
061600950711Non esistono variazioni su questo contrassegno
061700150709Bolla non trovata in interrogazione
