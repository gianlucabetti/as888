000100990510      *------------------------------------------------------------------------*
000200000412      *  MODIFICA C.A. IN FASE 900 CON CAUSALE 07 PAG. PRATICA VETTORE         *
000300020129      *                          E CON CAUSALE 15 PAG. PRATICA IN FRANCHIGIA   *
000400000317      *  TIPO ENTRATA 9
000500990428      *------------------------------------------------------------------------*
000600000317      *  MODIFICA C.A. IN FASE TRA LA 600 E A 660                              *
000700000317      *  TIPO ENTRATA 6
000800000317      *------------------------------------------------------------------------*
000900990426
001000980803      ****************************************************************
001100980803      *  FASI C.A. FISSATE A PROGRAMMA  (ricercare con 0 davanti)
001200980803      ****************************************************************
001300000315      * 900 = CHIUSURA C.A.                                 WFCHI
001400000317      * 600 = RICEZIONE DOCUMENTI                           WFRDO
001500011022      * 690 = INVIO PRATICHE CONSULDANNI                    WFIPC
001600990428      ****************************************************************
001700000315      ****************************************************************
001800000315      *  CAUSALE CHIUSURA CA FISSATA A PROGRAMMA
001900000315      ****************************************************************
002000000317      *  07 = PAGAMENTO PRATICA ASSICURATIVA                WCCPA
002100000317      *  05 = PAGAMENTO LIQUIDAZIONE TRANSATTIVA            WCCLT
002200020129      *  15 = PAGAMENTO PRATICA IN FRANCHIGIA               WCCFR
002300000315      ****************************************************************
002400990426
002500990426      ****************************************************************
002600990426      *  RIEPILOGO INDICATORI
002700990426      ***************************************************************
002800990427      * 01 - VISUALIZZA numero evento
002900990427      * 02 - VISUALIZZA numero pratica assicurativa
003000131011      * 04 - VISUALIZZA se pratica in getsione ad A.I.G.
003100000317      *** TIPO GESTIONE
003200000317      * 10 - MODIFICA C.A. in pagamento
003300000320      * 15 - C.A. Liquidazione transattiva
003400000320      * 17 - C.A. Pratica assicurativa
003500020129      * 18 - C.A. Pratica in franchigia
003600000317      * 20 - MODIFICA C.A. in ricezione documenti
003700990427      ***
003800990427      * 28 - ERRORE GENERICO DSPF
003900990427      * 29 - RIEMISSIONE FORMATO VIDEO
004000990427      * 30 - COMODO
004100990427      * 31 - COMODO
004200990427      * 32 - COMODO
004300990426      ***  Video 1
004400990427      * 36 - ERRORE numero C.A.
004500990427      * 37 -
004600990427      * 38 -
004700990427      * 39 -
004800990428      ***  Video 2
004900000315      * 40 - ERRORE forzatura pagamento CA
005000000315      * 41 - ERRORE beneficiario alternativo
005100000315      * 42 - ERRORE importo trattenuto
005200000315      * 43 - ERRORE data importo trattenuto errato
005300000315      * 44 -
005400000315      * 45 -
005500990430      * 46 -
005600990430      * 47 -
005700990427      * 48 -
005800990427      * 49 -
005900990427      * 50 -
006000000317      ***  Video 3
006100000317      * 60 - ERRORE ritorno indietro di fase
006200000317      * 61 - ERRORE importo richiesto
006300000317      * 62 - ERRORE numero documento
006400000317      * 63 - ERRORE data documento
006500000317      * 64 -
006600000317      * 65 -
006700000317      * 66 -
006800000317      * 67 -
006900000317      * 68 -
007000000317      * 69 -
007100000317      * 70 -
007200990426      ***************************************************************
007300990426
007400990426     H DECEDIT('0,') DATEDIT(*DMY.)
007500990426      * ---------------------------------------------------------
007600000315     fFIDN59D   CF   E             WORKSTN
007700990426     fTABEL00F  IF   E           K DISK
007800990426     fFNDCT01L  UF A E           K DISK
007900990426     fFNDCS01L  UF A E           K DISK
008000990426     fFNDET01L  IF   E           K DISK
008100990426     fFNDCF01L  IF A E           k DISK
008200990426     fFNDCL01L  UF A E           k DISK
008300000412     fFNDKA01L  UF   E           k DISK
008400000412     fAZORG01L  IF   E           K DISK
008500990426      * sede
008600000315     fTITAS30C  IF   E           K DISK
008700000315     fTNTMD01L  IF   E           K DISK
008800990426      * ---------------------------------------------------------
008900010910     D MSG             S             78    DIM(24) CTDATA PERRCD(1)             MSG VIDEO
009000050719     D NT              S             35    DIM(12) CTDATA PERRCD(1)             NOTE AUTOMATICHE
009100000404     D NOTE            S             35    DIM(10)                              Gest.note automatich
009200990503      * ---------------------------------------------------------
009300990503     D FIDN00DS      E DS
009400990503     D FIDN05DS      E DS
009500990426     D FIDN10DS      E DS
009600990426     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
009700990426     D TIBS02DS      E DS
009800990428     d TIBS69DS      E DS
009900990426     d DS_cnaco      E DS                  extname(CNACO00F)
010000990426     d DS_cnind      E DS                  extname(CNIND00F)
010100990426     d DS_cnclp      E DS                  extname(CNCLP00F)
010200990426     d DS_fncls      E DS                  extname(FNCLS00F)
010300990426     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
010400990426     d  tcu                  398    697    DIM(50)
010500990426     d  kcu                  698    847P 0 DIM(50)  PACKEVEN
010600990426     D CNCR80        E DS
010700990426     D KPJBA         E DS
010800010910     D DGEDDN        E DS
010900990426     D DTAD          E DS
011000990426     D DSTB          E DS
011100990426     D DSCV          E DS
011200000412     D DCCH          E DS
011300990426     D DDCT01        E DS
011400020129     d ddcf01        e ds
011500010907     D YEURCODS      E DS                  inz
011600010907     D   YECTAR      E                     inz('H')
011700011017     D XDECDS        E DS                  inz
011800011017     D   OCDESI      E                     inz(*OFF)
011900990426
012000990428     d                 ds
012100990428     d  DS_numca               1     14
012200990426     d  dctaac                 1      4  0
012300990426     d  dctfil                 5      7  0
012400990426     d  dctnca                 8     14  0
012500990426
012600990426     D WLBDAT          DS                  INZ
012700990426     D  G02DAT                 1      8  0
012800990426     D  G02INV                 9     16  0
012900990426     D  G02ERR                17     17
013000990426     D  G02TGI                18     22  0
013100990426
013200990426     d TCUDS           DS
013300990426     d  f1                     1      1
013400990426     d  f3                     3      3
013500990426     d  f2                     2      2
013600990426     d  f4                     4      4
013700990426     d  f56                    5      6
013800990426
013900990426     D                SDS
014000990426     D  VTCPGM                 1     10
014100990426
014200000317     D TIT10           C                   CONST('*   MODIFICA C.A. IN PAGAMENT-
014300000315     D                                     O   *')
014400000317     D TIT20           C                   CONST('MODIFICA  C.A. RICEZIONE DOCU-
014500000317     D                                     MENTI')
014600990426
014700990427     d Datasped        S               D   DATFMT(*eur)
014800000316     D Wdataeur        S               D   DATFMT(*eur)
014900000316     D Wdatadmy        S               D   DATFMT(*dmy)
015000990503     D Dataca          S                   LIKE(I10dta)
015100990503     D Wlnp            S                   LIKE(ARBlnp)
015200990430     D Wv2cksc         s                   LIKE(V2Cksc)
015300000320     D Wv2cipt         s                   LIKE(V2Cipt)
015400000320     D Wv2cdpt         s                   LIKE(V2Cdpt)
015500000321     D W_v2cdpt        s                   LIKE(V2Cdpt)
015600000412     D W_v3cddc        s                   LIKE(V3Cddc)
015700000412     D Wv3cipr         s                   LIKE(V3Cipr)
015800000412     D Wv3cddc         s                   LIKE(V3Cddc)
015900000412     D Wv3cndc         s                   LIKE(V3Cndc)
016000000412     D Wtrc            S                   LIKE(DKAtrc)
016100990426     D Kcod            S                   LIKE(TBLcod)
016200990426     D Kkey            S                   LIKE(TBLkey)
016300990426     D §key            S                   LIKE(TBLkey)
016400990426     D wvlt            s                   LIKE(DCTvpv)
016500990426     D W§dctport       S                   LIKE(§DCTport)
016600000412     D prgnot          s                   LIKE(DCSpno)
016700990426
016800000315     d WFCHI           s                   LIKE(DCTfca)   inz(900)
016900000317     d WFRDO           s                   LIKE(DCTfca)   inz(600)
017000011022     d WFIPC           s                   LIKE(DCTfca)   inz(690)
017100000317     d WCCPA           s                   LIKE(DCTcch)   inz('07')
017200000317     d WCCLT           s                   LIKE(DCTcch)   inz('05')
017300020129     d wccfr           s                   like(dctcch)   inz('15')
017400990426
017500000317     d entrata         s              1
017600990503     d Wtpb            s              1
017700990504     d Wbenalt         s              1
017800990426     D §des            S             30
017900990426     D wtibs69r        s              1
018000010910     D w002a           s              2
018100990426     D w0033           s              3  3
018200990426     D w0040           s              4  0
018300990426     D w0133           s             13  3
018400000412     D old123          s             12  3
018500000412     D new123          s             12  3
018600990426     D w0140           s             14  0
018700990426     D XX              s              2  0
018800000404     D JJ              s              3  0
018900990426     D wdtgio          s              8  0
019000990430     D wdatasp         s              8  0
019100990426     D dateu           s              8  0
019200000414     D WCLPSCF         s              9
019300990426      * ---------------------------------------------------------
019400990426
019500990809     ITITAS000
019600990426     I              TASAAS                      ARBAAS
019700990426     I              TASLNP                      ARBLNP
019800990426     I              TASNRS                      ARBNRS
019900990426     I              TASNSP                      ARBNSP
020000990426     I              TASMGS                      ARBMGS
020100990426     I              TASNCL                      ARBNCL
020200990426     I              TASKSC                      ARBKSC
020300990426     I              TASCCM                      ARBCCM
020400990809     I              TASPKf                      ARBPKF
020500990426     I              TASFVF                      ARBFVF
020600990426     I              TASVLF                      ARBVLF
020700990426     I              TASLNA                      ARBLNA
020800990426     I              TASIAS                      ARBIAS
020900990426     I              TASVAS                      ARBVAS
021000990426     I              TASTSP                      ARBTSP
021100990426     I              TASCTR                      ARBCTR
021200990426     I              TASTFP                      ARBTFP
021300990426     I              TASTFA                      ARBTFA
021400990426     I              TASLL1                      WDDT
021500990426     I              TASCCA                      ARBCCA
021600990426
021700990809     ITITAS010
021800990426     I              TASAAS                      ARBAAS
021900990426     I              TASLNP                      ARBLNP
022000990426     I              TASNRS                      ARBNRS
022100990426     I              TASNSP                      ARBNSP
022200990426     I              TASMGS                      ARBMGS
022300990426     I              TASNCL                      ARBNCL
022400990426     I              TASKSC                      ARBKSC
022500990426     I              TASCCM                      ARBCCM
022600990809     I              TASPKf                      ARBPKF
022700990426     I              TASFVF                      ARBFVF
022800990426     I              TASVLF                      ARBVLF
022900990426     I              TASLNA                      ARBLNA
023000990426     I              TASIAS                      ARBIAS
023100990426     I              TASVAS                      ARBVAS
023200990426     I              TASTSP                      ARBTSP
023300990426     I              TASCTR                      ARBCTR
023400990426     I              TASTFP                      ARBTFP
023500990426     I              TASTFA                      ARBTFA
023600990426     I              TASLL1                      WDDT
023700990426     I              TASCCA                      ARBCCA
023800990426
023900990809     ITITASP00
024000990426     I              TASAAS                      ARBAAS
024100990426     I              TASLNP                      ARBLNP
024200990426     I              TASNRS                      ARBNRS
024300990426     I              TASNSP                      ARBNSP
024400990426     I              TASMGS                      ARBMGS
024500990426     I              TASNCL                      ARBNCL
024600990426     I              TASKSC                      ARBKSC
024700990426     I              TASCCM                      ARBCCM
024800990809     I              TASPKf                      ARBPKF
024900990426     I              TASFVF                      ARBFVF
025000990426     I              TASVLF                      ARBVLF
025100990426     I              TASLNA                      ARBLNA
025200990426     I              TASIAS                      ARBIAS
025300990426     I              TASVAS                      ARBVAS
025400990426     I              TASTSP                      ARBTSP
025500990426     I              TASCTR                      ARBCTR
025600990426     I              TASTFP                      ARBTFP
025700990426     I              TASTFA                      ARBTFA
025800990426     I              TASLL1                      WDDT
025900990426     I              TASCCA                      ARBCCA
026000990426
026100990426     ITNTMD000
026200990426     I              TMDAAS                      ARBAAS
026300990426     I              TMDLNP                      ARBLNP
026400990426     I              TMDNRS                      ARBNRS
026500990426     I              TMDNSP                      ARBNSP
026600990426     I              TMDRSM                      ARBRSM
026700990426     I              TMDINM                      ARBINM
026800990426     I              TMDCAM                      ARBCAM
026900990426     I              TMDLOM                      ARBLOM
027000990426     I              TMDPRM                      ARBPRM
027100990426     I              TMDNZM                      ARBNZM
027200990426     I              TMDRSD                      ARBRSD
027300990426     I              TMDIND                      ARBIND
027400990426     I              TMDCAD                      ARBCAD
027500990426     I              TMDLOD                      ARBLOD
027600990426     I              TMDPRD                      ARBPRD
027700990426     I              TMDNZD                      ARBNZD
027800990426      * ---------------------------------------------------------
027900990427
028000990427      *            G E S T I O N E   V I D E O  1
028100990427     C     EMD01         TAG
028200990427      *
028300000315     C                   WRITE     FI59T01
028400000315     C                   WRITE     FI59Z01
028500000315     C                   EXFMT     FI59D01
028600990427      *
028700990427     C                   CLEAR                   V1CMSG
028800990427     c                   eval      *in29 = *off
028900990427     c                   eval      *in28 = *off
029000000315     c                   eval      *in36 = *off
029100990427
029200990428      * F3  = FINE
029300990427     C                   IF        *INKC = *ON
029400990427     C                   goto      FINE
029500990427     C                   ENDIF
029600990427
029700990427
029800990427     c                   EXSR      CTRD01
029900990427
030000990427     C                   IF        *IN28 = *ON  or  *IN29 = *ON
030100990427     C                   goto      EMD01
030200990427     C                   ENDIF
030300990426
030400990428      * Riempio testata  video 2
030500990426     C                   EXSR      RIETES
030600990426      *
030700000317      * Riempio dettaglio video 2  per modifica ca in pagamento
030800000317     C   10              EXSR      RIEDET2
030900000317      *
031000000317      * Riempio dettaglio video 3  per modifica ca in gestione documenti
031100000317     C   20              EXSR      RIEDET3
031200990426
031300000317      *            G E S T I O N E   V I D E O  2/3
031400990427     C     EMD02         TAG
031500990426      *
031600000317     C                   WRITE     FI59T02
031700000317     C                   WRITE     FI59Z02
031800000317      * video modifica pagamento
031900000317     c                   if        *in10
032000000317     C                   EXFMT     FI59D02
032100000317     c                   endif
032200000317      * video modifica gestione documenti
032300000412     c                   if        *in20
032400000317     C                   EXFMT     FI59D03
032500000317     c                   endif
032600990426      *
032700990427     C                   CLEAR                   V2CMSG
032800990426     c                   eval      *in29 = *off
032900990426     c                   eval      *in28 = *off
033000000317     c     40            DO        70            XX
033100990428     c                   eval      *IN(XX) = *OFF
033200990428     c                   ENDDO
033300990426
033400990426      * F3 = FINE
033500990426     C                   IF        *INKC = *ON
033600990426     C                   goto      FINE
033700990426     C                   ENDIF
033800990426
033900990426      * F12 = RITORNO
034000990426     C                   IF        *INKL = *ON
034100000317     c   10              clear                   fi59d02
034200000317     c   20              clear                   fi59d03
034300990428     C                   goto      EMD01
034400990426     C                   ENDIF
034500990426
034600990503      * F7 = VISUALIZZA C.A.
034700990503     C                   IF        *INKG = *ON
034800990503     c                   exsr      SUB_F7
034900990503     C                   goto      EMD02
035000990503     c                   ENDIF
035100990503
035200990426      * F18 = NOTE
035300990428     C                   IF        *INKS = *ON
035400990426     c                   exsr      SUB_F18
035500990428     C                   goto      EMD02
035600990426     c                   ENDIF
035700990426
035800990428      * Controlli video 2
035900000317     C   10              EXSR      CTRD02
036000000317      * Controlli video 3
036100000317     C   20              EXSR      CTRD03
036200990426      *
036300990426     C                   if        *IN28 = *ON  or  *IN29 = *ON
036400990428     C                   goto      EMD02
036500990426     C                   endif
036600990426
036700990503     C                   IF        *INKF = *OFF
036800990430     c                   goto      EMD02
036900990430     C                   ENDIF
037000131011      * se gestione ricezione documenti e la CA è affidata ad A.I.G. emetto una videata di richiesta
037100131011      * di pulizia del flag di affidamento pratica ad A.I.G.
037200050719     C                   if        *in20 and *in04
037300050719     c                   exsr      ges_wind
037400050719     c                   endif
037500990430      * Aggiorno files
037600000317     c   10              exsr      AGGIORNA2
037700000317      *
037800000317     c   20              exsr      AGGIORNA3
037900990430
038000990426      *------------------------
038100990426     C     FINE          TAG
038200990426      *------------------------
038300990426
038400990426      * Chiudo pgm di servizo
038500990426     c                   clear                   FIDN10DS
038600990426     c                   movel     'C'           i10tla
038700990426     c                   movel     FIDN10DS      kpjbu
038800990426     c                   call      'FIDN10R'
038900990426     c                   parm                    kpjba
039000990426      *
039100990426     C                   IF        Wtibs69r  = 'S'
039200990428     C                   clear                   TIBS69DS
039300990426     C                   EVAL      I69TLA  = 'C'
039400990426     C                   CALL      'TIBS69R'
039500990426     C                   PARM                    TIBS69DS
039600990426     C                   ENDIF
039700990426      *
039800990426     c                   clear                   tibs02ds
039900990426     c                   movel     'C'           t02tla
040000990426     c                   call      'TIBS02R'
040100990426     c                   parm                    kpjba
040200990426     c                   parm                    tibs02ds
040300990426      *
040400990426     C                   EVAL      *INLR = *ON
040500990427      **********************************************************************
040600990427      *  CONTROLLI VIDEATA 1
040700990427      **********************************************************************
040800990427     C     CTRD01        BEGSR
040900990427
041000990427      * SISTEMO ANNO DI DUE CIFRE
041100990427     c                   IF        V1Caac > *zeros  and  V1Caac < 100
041200990427     C                   if        V1Caac > 60
041300990427     C                   ADD       1900          V1Caac
041400990427     C                   else
041500990427     C                   ADD       2000          V1caac
041600990427     C                   endif
041700990427     c                   ENDIF
041800990427
041900990427      * CONTROLLO SE SELEZIONATI DATI VALIDI
042000990427     c                   IF        V1Cfil = *zeros  or  V1Cnca = *zeros
042100990427     c                                              or  V1Caac = *zeros
042200990427     c                   movel     msg(1)        V1Cmsg
042300990427     c                   eval      *in36 = *on
042400990427     c                   eval      *in28 = *on
042500990427     c                   ENDIF
042600990427     c   28              goto      ECTRD01
042700990427
042800990428      * CONTROLLO SE SELEZIONATA C.A. VALIDA
042900990430     c     kca           CHAIN     fndct000                           30
043000990427      *
043100990427     c                   SELECT
043200990427      *
043300990427     c                   WHEN      *IN30 = *ON
043400990427     c                   movel     msg(2)        V1Cmsg
043500990427      *
043600990427     c                   WHEN      DCTatb = 'A'
043700990427     c                   movel     msg(3)        V1Cmsg
043800000317      * c.a. non è chiusa   richiesta modifica pagamento
043900000317     c                   WHEN      DCTfca <> WFchi  and *in10
044000990427     c                   movel     msg(4)        V1Cmsg
044100990427      *
044200000317      * c.a. chiusa   richiesta modifica fase 600-660
044300000317     c                   WHEN      DCTfca < WFrdo   and *in20
044400000317     c                   movel     msg(5)        V1Cmsg
044500000317     c                   WHEN      DCTfca > WFipc   and *in20
044600000317     c                   movel     msg(5)        V1Cmsg
044700000317      *
044800990427     c                   ENDSL
044900990427      *
045000990427     c                   IF        V1Cmsg <> *blanks
045100990427     c                   eval      *in36 = *on
045200990427     c                   eval      *in28 = *on
045300990427     c                   goto      ECTRD01
045400990427     c                   ENDIF
045500000316      *
045600000316     c                   movel     dctflo        DDCT01
045700131011      * se flag A.I.G. accendo lo 04
045800050719     c                   eval      *in04= (§dctaffi = 'A')
045900050719
046000990427
046100990428      * AGGANCIO SPEDIZIONE
046200990430     c                   eval       Wtpb = 'S'
046300990427     c     kbol01        chain(N)  tntmd000                           30
046400990809     c     kbol01        chain     TITAS30c                           30
046500990427      *  se tipo bolla non è valido cerco altro rcd
046600990427     C                   movel     'TB'          kCOD
046700990427     C                   movel(P)  TAStbl        kKEY
046800990427     C     ktab          chain(N)  TABEL                              31
046900990427     C                   movel     TBLUNI        DSTB
047000990427     c                   if        §TBrbl = 'R'
047100990809     c     kbol01        reade     TITAS30c                               30
047200990427     c                   endif
047300990427      *
047400990428     c                   IF          *IN30 = *off
047500990430     c                   z-add     arbmgs        WDATASP
047600990430     c                   movel     arbaas        WDATASP
047700990430     c     *iso          move      Wdatasp       datasped
047800990428     c                   ELSE
047900990428     c                   movel     msg(6)        V1Cmsg
048000990428     c                   eval      *in36 = *on
048100990428     c                   eval      *in28 = *on
048200990428     c                   goto      ECTRD01
048300990428     c                   ENDIF
048400990428
048500990428      * CONTROLLO SE C.A. ABILITATA ALLE MODIFICHE
048600000317      *
048700000317      * modifica ca in pagamento
048800990428     c                   SELECT
048900000317     c                   WHEN      not *in10
049000020129     c                   WHEN      DCTcch <> WCCpa and DCTcch <> WCClt and
049100061123     c                             dctcch <> wccfr and DCTCCH <> '17' and
049200061123     c                             dctcch <> '18'
049300000315     c                   movel     msg(7)        V1Cmsg
049400990428     c                   ENDSL
049500990428      *
049600990428     c                   IF        V1Cmsg <> *blanks
049700990428     c                   eval      *in36 = *on
049800990428     c                   eval      *in28 = *on
049900990428     c                   goto      ECTRD01
050000990428     c                   ENDIF
050100990427
050200990427     c     ECTRD01       ENDSR
050300990426      ****************************************************************
050400990427      *  CARICAMENTO DATI DI TESTATA
050500990426      ****************************************************************
050600990426     C     RIETES        BEGSR
050700990426
050800990426     c                   movel     §DCTlnpc      Wlnp
050900990427      *
051000990427     c                   z-add     V1Cfil        vtcfil
051100990427     c                   z-add     V1Cnca        vtcnca
051200990503     c                   z-add     DCTmgc        dataca
051300990503     c                   movel     DCTaac        dataca
051400000414     c     *iso          move      DATACA        WDATAEUR
051500000414     c     *EUR          movel     WDATAEUR      VTCdc1
051600990426
051700990426     c                   move      datasped      VTCdsp
051800990427     C                   move      DCTlnp        VTClnp
051900990427     C                   move      DCTnrs        VTCnrs
052000990427     C                   move      DCTnsp        VTCnsp
052100990426
052200990427     C                   move      DCTlna        VTClna
052300990427     C     DCTlna        chain(N)  AZORG                              31
052400990426     C  N31              If        ORGDE5 <> *BLANKS
052500990426     C                   movel     ORGDE5        VTDlna
052600990426     C                   Else
052700990426     C                   movel     ORGDES        VTDlna
052800990426     C                   Endif
052900990426
053000990426      *  Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con quello C.A. tranne se la
053100990426      *  sped. è "Mamma di cambio porto", in questo caso inverto il porto C.A.
053200990426     c                   SELECT
053300110503     c***                WHEN      ARBcca <> '9'
053400110503     c                   WHEN      §DCTcca <> '9'
053500990426     c                   movel     §DCTport      W§DCTport
053600990426     c                   WHEN      §DCTport = 'A'
053700990426     c                   movel     'F'           W§DCTport
053800990426     c                   WHEN      §DCTport = 'F'
053900990426     c                   movel     'A'           W§DCTport
054000990426     c                   ENDSL
054100990426
054200990426      * Codici mittente/destinario
054300990426     c                   IF         w§DCTport = 'A'
054400990426     c                   move      arbccm        VTCksm
054500000316      *
054600990426     c                   movel     arbksc        VTCksd
054700990427      *
054800990426     c                   ELSE
054900990426     c                   move      arbksc        VTCksm
055000990426     c                   clear                   VTCksd
055100990426     c                   ENDIF
055200990426
055300990426      * dati bolla
055400990426     c                   IF        §DCTport = 'A'
055500990426     c                   movel(P)  'ASSEGNATO'   VTDcbo
055600990426     c                   ELSE
055700990426     c                   movel(P)  'FRANCO'      VTDcbo
055800990426     c                   ENDIF
055900990427      *
056000990426     c                   movel     arbrsm        VTCrsm
056100990426     c                   move      arbcam        VTCcam
056200990426     c                   movel     arblom        VTClom
056300990426     c                   movel     arbprm        VTCprm
056400990426     c                   movel     arbnzm        VTCnam
056500990426     c                   movel     arbrsd        VTCrsd
056600990426     c                   move      arbcad        VTCcad
056700990426     c                   movel     arblod        VTClod
056800990426     c                   movel     arbprd        VTCprd
056900990426     c                   movel     arbnzd        VTCnad
057000990426     c                   z-add     arbncl        VTCncl
057100990426     c                   z-add     arbpkf        VTCpkf
057200990426     c                   movel     arbfvf        VTCfvf
057300990426     c                   z-add     arbvlf        VTCvlf
057400990427
057500990427      * IMPORTO DA ASSICURARE
057600000316      *      solo se non impostato dalla fatturazione
057700011114     c                   if        ARBias <> *zeros and ARBvas = *blanks
057800011114     c                   movel     'ITL'         ARBvas
057900011114     c                   endif
058000000316     c                   IF        TASFCM <> 'F'
058100990427     C                   z-add     ARBias        VTCias
058200990427     C                   movel     ARBvas        VTCvas
058300000316     c                   END
058400990426
058500000317     c
058600000316      * C.A. chiusa
058700000317     c                   if        *in10
058800000317      *
058900000316     c     *iso          move      DCTdch        Wdataeur
059000000316     c     *eur          movel     Wdataeur      VTCdch
059100000316     c* decodifico se c'è la causale di chiusura
059200000316     c                   eval      VTCcch = dctcch
059300000316      *
059400000316     c                   clear                   TIBS02DS
059500000316     C                   MOVEL     'C'           t02mod
059600000316     C                   MOVEL     knsif         t02sif
059700000316     C                   MOVEL     'CCH'         t02cod
059800000316     C                   MOVEL(P)  dctcch        t02ke1
059900000316      *
060000000316     C                   CALL      'TIBS02R'
060100000316     C                   PARM                    KPJBA
060200000316     C                   PARM                    TIBS02DS
060300000316      *
060400000316     C                   if        t02err = *BLANKS
060500000316     C                   MOVEL     T02UNI        Dcch
060600000316     C                   MOVEL     §CCHdesc      VTDcch
060700000316     C                   endif
060800000317      *
060900000317     c                   endif
061000000317     c
061100000317      * FASE C.A.
061200000317     c                   if        *in20
061300000317      *
061400000317     C                   z-add     DCTfca        VTCfca
061500000317     c                   CLEAR                   FIDN05DS
061600000317     c                   movel     'C'           I05MOD
061700000317     c                   z-add     VTCfca        I05fca
061800000317     C                   CALL      'FIDN05R'
061900000317     c                   PARM                    KPJBA
062000000317     c                   PARM                    FIDN05DS
062100000317     c                   IF        O05err <> 'E'
062200000317     c                   movel     O05REC        DS_FNDFA
062300000317     c                   movel     DFAdes        VTDfca
062400000317     c                   ENDIF
062500000317      *
062600000317     c                   endif
062700990426
062800990426      * TIPO ANOMALIA
062900990427     c                   clear                   DTAD
063000990427      *
063100990427     c                   clear                   TIBS02DS
063200990427     C                   MOVEL     'C'           t02mod
063300990427     C                   MOVEL     knsif         t02sif
063400990427     C                   MOVEL     'TAD'         t02cod
063500990427     C                   MOVEL(P)  DCTtad        t02ke1
063600990427      *
063700990427     C                   CALL      'TIBS02R'
063800990427     C                   PARM                    KPJBA
063900990427     C                   PARM                    TIBS02DS
064000990427      *
064100990427     C                   if        t02err = *BLANKS
064200990427     C                   MOVEL     T02UNI        DTAD
064300990504     C                   MOVEL     DCTtad        VTCtad
064400990427     C                   MOVEL     §TADdesc      VTDtad
064500990427     C                   endif
064600990427
064700990427      * EVENTO
064800990427     c                   IF        DCTnev > *zeros
064900990427     C                   z-add     DCTnev        VTCnev
065000990427     C                   z-add     DCTaae        VTCaae
065100990427     C     KDET          chain     FNDET000                           31
065200990427     C                   IF        *IN31 = *off  and  DETatb <> 'A'
065300051014     c                   z-add     DETmge        VTCdev
065400990427     C                   movel     DCTaae        VTCdev
065500051014     c                   clear                   WLBdat
065600051014     c                   eval      G02inv = (DCTaae * 10000) + DETmge
065700051014     C                   eval      G02err = '3'
065800051014     C                   CALL      'XSRDA8'
065900051014     C                   PARM                    WLBDAT
066000051014     c                   z-add     G02dat        VTCdev
066100990427     C                   ENDIF
066200990427     C                   ENDIF
066300990427
066400990427      * NUMERO PRATICA ASSICURATIVA
066500990427     c                   IF        DCTprn > *zeros
066600990427     C                   movel     DCTdit        VTCdit
066700990427     C                   z-add     DCTprn        VTCprn
066800990427     C                   z-add     DCTpra        VTCpra
066900990427     C                   ENDIF
067000990426
067100990427      *          V I S I B I L I T A'   C A M P I
067200990427
067300990427     c                   eval      *IN01 = (VTCnev > *zeros)
067400990427     c                   eval      *IN02 = (VTCprn > *zeros)
067500990426
067600990426     C                   ENDSR
067700000320      ****************************************************************
067800000320      *  CARICAMENTO DATI DI DETTAGLIO  videata modifica pagamento
067900000320      ****************************************************************
068000000320     C     RIEDET2       BEGSR
068100000320
068200000320      * Verifico se Pratica Assicurativa o Liquidazione Transattiva
068300020129      * o Pratica in franchigia
068400000320     C                   eval      *in17 = (dctcch = WCcpa)                     * causale 07 PA
068500000320     C                   eval      *in15 = (dctcch = WCclt)                     * causale 05 LT
068600020129     C                   eval      *in18 = (dctcch = wccfr)                     * causale 15 FR
068700061123     C                   eval      *in17 = (dctcch = '17')                      * causale 17 PA
068800061123     C                   eval      *in17 = (dctcch = '18')                      * causale 18 PA
068900000320      *
069000020129    1c                   if        *in17 or *in18
069100000320      * BENEFICIARIO ALTERNATIVO
069200010907    2c                   IF        DCTKSC > *ZEROS
069300000320     c                   z-add     dctksc        V2cksc
069400000320     c                   z-add     dctksc        WV2cksc
069500000320     c                   move      V2cksc        w0040
069600000320      *   se codificato decodifico da p.d.c.
069700010907    3C                   IF        w0040 <> 8888
069800000320     c                   clear                   ACOrag
069900000320     C                   clear                   TIBS69DS
070000000320     c                   z-add     DCTksc        I69kac
070100000320     c                   exsr      CTR_anagra
070200000320     c  N30              movel     ACOrag        V2Dksc
070300000320      *
070400000320     c                   else
070500000320      *   se non codificato decodifico da anagrafica clienti c.a.
070600000320     c                   movel     'S'           Wbenalt
070700000320     c                   clear                   FNDKA000
070800000320     c                   movel     'C'           Wtrc
070900000320     c     KDKA          CHAIN(N)  FNDKA000                           30
071000000320     c                   movel     dkarag        V2dksc
071100010907   -3c                   ENDIF
071200010907   -2c                   ENDIF
071300000320      * visualizzo  importo da liquidare
071400000320     c     kca           CHAIN     FNDCL000                           30
071500010907    2c                   IF        *IN30 = *OFF   and DCLatb = ' '
071600000320      *
071700000320      * visualizzo  importo da liquidare
071800010907    3c                   IF          DCLipl > *zeros
071900000320     C                   z-add     DCLipl        W0133
072000000320     C                   movel     DCLvpl        WVLT
072100010912     C***                   exsr      ctrvlt
072200010912    4C***                   If        *IN28 = *OFF  and  *IN29 = *OFF
072300000320      *     Calcolo controvalore
072400010907     c***                   IF        DCLvpl = *blanks
072500010910    5c                   IF        DCLvpl = §gedDBA
072600000320     c                   z-add     DCLipl        V2Cipl
072700000320     c                   ELSE
072800010907     c***     §CVcmb        mult      DCLipl        V2Cipl
072900010907     C                   exsr      srCMB
073000010911     C                   z-add     W0133         V2Cipl
073100010907   -5c                   ENDIF
073200010907     c***                  clear                   V2Cvpv                           Valuta
073300010911     c                   movel     §gedDBA       V2Cvpv                           Valuta
073400000320      *
073500010912   -4c***                   ENDIF
073600010907     c                   else
073700010907     c                   clear                   V2Cipl
073800010907     c                   clear                   V2Cvpv
073900010907   -3c                   ENDIF
074000010911      * visualizzo  divisa/valuta dell'importo trattenuto (I/O)
074100010911      * (sempre, anche se importo = 0)
074200010911     c                   movel     §gedDBA       V2Cvlt                           Valuta
074300000320      * visualizzo  importo trattenuto
074400010907    3c                   IF          DCLipt > *zeros
074500000320     C                   z-add     DCLipt        W0133
074600000320     C                   movel     DCLvpt        WVLT
074700010912     C***                   exsr      ctrvlt
074800010912    4C***                   If        *IN28 = *OFF  and  *IN29 = *OFF
074900000320      *     Calcolo controvalore
075000010907     c***                   IF        DCLvpt = *blanks
075100010910    5c                   IF        DCLvpt = §gedDBA
075200000320     c                   z-add     DCLipt        V2Cipt
075300000320     c                   ELSE
075400010907     c***     §CVcmb        mult      DCLipt        V2Cipt
075500010907     C                   exsr      srCMB
075600010911     C                   z-add     W0133         V2Cipt
075700010907   -5c                   ENDIF
075800000320     c                   z-add     v2cipt        WV2Cipt
075900010907     c***                   clear                   V2Cvlt                           Valuta
076000000320      * data importo trattenuto
076100010907    5c                   if        DCLdpt > *zeros
076200000320     c     *iso          move      DCLdpt        Wdataeur
076300000320     c     *eur          movel     Wdataeur      V2Cdpt
076400000320     c                   eval      Wv2cdpt = V2Cdpt
076500010907   -5c                   endif
076600000320      *
076700010912    4c***                   ENDIF
076800010907     c                   else
076900010907     c                   clear                   V2Cipt
077000010907   -3c                   ENDIF
077100000320      *
077200010907   -2c                   ENDIF                                                  * FNDCL
077300010907   -1c                   ENDIF                                                  * in17
077400000320      *
077500000320     c                   ENDSR
077600000320      *
077700000320      ****************************************************************
077800000320      *  CARICAMENTO DATI DI DETTAGLIO  videata modifica gestione documenti
077900000320      ****************************************************************
078000000320     C     RIEDET3       BEGSR
078100000320
078200000320      * Verifico la fase precedente all'attuale
078300000320      *
078400000320      * mi posiziono con fase uguale e leggo indietro
078500000412     c     kca           setgt     fndcf01l
078600000320     c                   do        *hival
078700000412     c     kca           readpe    fndcf01l
078800000320      *
078900000320     c                   if        not %found
079000000320     c                   leave
079100000320     c                   endif
079200000320      *
079300000320      * verifico se fase precedente è minore dell'attuale
079400000320     c                   if        dcffca < dctfca
079500000320     c                   eval      v3cpfa = dcffca
079600000320      * decodifico
079700000320     c                   CLEAR                   FIDN05DS
079800000320     c                   movel     'C'           I05MOD
079900000320     c                   z-add     V3cpfa        I05fca
080000000320     C                   CALL      'FIDN05R'
080100000320     c                   PARM                    KPJBA
080200000320     c                   PARM                    FIDN05DS
080300000320     c                   IF        O05err <> 'E'
080400000320     c                   movel     O05REC        DS_FNDFA
080500000320     c                   movel     DFAdes        V3Dpfa
080600000320     c                   ENDIF
080700000320     c*
080800000412     c                   leave
080900000412     c*
081000000320     c                   endif
081100000320      *
081200000320     c                   enddo
081300000320      * visualizzo  importo massimo risarcibile
081400000320     c                   IF          DCTipv > *zeros
081500000412     C                   z-add     DCTipv        W0133
081600000412     C                   movel     DCTvpv        WVLT
081700010912     C***                   exsr      ctrvlt
081800010912     C***                   If        *IN28 = *OFF  and  *IN29 = *OFF
081900000320      *     Calcolo controvalore
082000010907     c***                   IF        DCTvpv = *blanks
082100010910     c                   IF        DCTvpv = §gedDBA
082200000320     c                   z-add     DCTipv        V3Cipv
082300000320     c                   ELSE
082400010907     c***     §CVcmb        mult      DCTipv        V3Cipv
082500010907     C                   exsr      srCMB
082600010911     C                   z-add     W0133         V3Cipv
082700000320     c                   ENDIF
082800010907     c***                   clear                   V3Cvpv                           Valuta
082900010911     c                   movel     §gedDBA       V3Cvpv                           Valuta
083000010912     c***                   ENDIF
083100010907     c                   else
083200010907     C                   clear                   DCTipv
083300010907     C                   clear                   DCTvpv
083400000320     c                   ENDIF
083500000320      * visualizzo  importo da liquidare
083600000320     c     kca           CHAIN     FNDCL000                           30
083700000320     c                   IF        *IN30 = *OFF   and DCLatb = ' '
083800000320      * visualizzo  importo trattenuto
083900000320     c                   IF          DCLipt > *zeros
084000000320     C                   z-add     DCLipt        W0133
084100000320     C                   movel     DCLvpt        WVLT
084200010912     C***                   exsr      ctrvlt
084300010912     C***                   If        *IN28 = *OFF  and  *IN29 = *OFF
084400000320      *     Calcolo controvalore
084500010907     c***                   IF        DCLvpt = *blanks
084600010910     c                   IF        DCLvpt = §gedDBA
084700000320     c                   z-add     DCLipt        V3Cipt
084800000320     c                   ELSE
084900010907     c***     §CVcmb        mult      DCLipt        V3Cipt
085000010907     C                   exsr      srCMB
085100010911     C                   z-add     W0133         V3Cipt
085200000320     c                   ENDIF
085300010907     c***                   clear                   V3Cvpt                           Valuta
085400010911     c                   movel     §gedDBA       V3Cvpt
085500000320      * data importo trattenuto
085600000320     c                   if        DCLdpt > *zeros
085700000320     c     *iso          move      DCLdpt        Wdataeur
085800000320     c     *eur          movel     Wdataeur      V3Cdpt
085900000320     c                   endif
086000000320      *
086100010912     c***                   ENDIF
086200010907     c                   else
086300010907     C                   clear                   DCLipt
086400010907     C                   clear                   DCLvpt
086500000320     c                   ENDIF
086600010911      * visualizzo  divisa/valuta dell'importo richiesto (I/O)
086700010911      * (sempre, anche se importo = 0)
086800010911     c                   movel     §gedDBA       V3Cvpr                           Valuta
086900000320      * visualizzo  importo richiesto
087000000320     c                   IF          DCLipr > *zeros
087100000320     C                   z-add     DCLipr        W0133
087200000320     C                   movel     DCLvpr        WVLT
087300000320     C                   exsr      ctrvlt
087400000320     C                   If        *IN28 = *OFF  and  *IN29 = *OFF
087500000320      *     Calcolo controvalore
087600010907     c***                   IF        DCLvpr = *blanks
087700010910     c                   IF        DCLvpr = §gedDBA
087800000320     c                   z-add     DCLipr        V3Cipr
087900000320     c                   ELSE
088000010907     c***     §CVcmb        mult      DCLipr        V3Cipr
088100010907     C                   exsr      srCMB
088200010911     C                   z-add     W0133         V3Cipr
088300000320     c                   ENDIF
088400010907     c***                   clear                   V3Cvpr                           Valuta
088500000412     c                   eval      Wv3cipr = v3cipr
088600000320      * data importo trattenuto
088700000320     c                   if        DCLddc > *zeros
088800000320     c     *iso          move      DCLddc        Wdataeur
088900000320     c     *eur          movel     Wdataeur      V3Cddc
089000000412     c                   eval      Wv3cddc = v3cddc
089100000320     c                   endif
089200000320      *
089300000320     c                   eval      v3cndc = DCLndc
089400000412     c                   eval      Wv3cndc = v3cndc
089500000320      *
089600000320     c                   ENDIF
089700010907     c                   else
089800010907     C                   clear                   DCLipr
089900010907     C                   clear                   DCLvpr
090000000320     c                   ENDIF
090100000320      *
090200000320     c                   ENDIF                                                  * FNDCL
090300000320      *
090400000320     c                   ENDSR
090500000320      *
090600990503      ****************************************************************
090700990503      * F7 = VISUALIZZA CA
090800990503      ****************************************************************
090900990503     C     SUB_F7        BEGSR
091000990503      *
091100990503     C                   clear                   FIDN00DS
091200990503     c                   z-add     DCTaac        I00aac
091300990503     c                   z-add     DCTfil        I00fil
091400990503     c                   z-add     DCTnca        I00nca
091500990503     c                   z-add     DCTaas        I00aas
091600990503     c                   z-add     DCTlnp        I00lnp
091700990503     c                   z-add     DCTnrs        I00nrs
091800990503     c                   z-add     DCTnsp        I00nsp
091900990503     c                   z-add     DCTfca        I00fca
092000990503     c                   z-add     DCTgfc        I00fgs
092100990503     c                   movel     Wtpb          I00tpb
092200990503      *
092300990503     C                   movel(P)  FIDN00DS      KPJBU
092400990503     c                   call      'FIDN01R'
092500990503     c                   parm                    kpjba
092600990503      *
092700990503     C                   ENDSR
092800990426      ****************************************************************
092900990426      * F18 = GESTIONE NOTE
093000990426      ****************************************************************
093100990426     C     SUB_F18       BEGSR
093200990426      *
093300990426     C                   clear                   FIDN10DS
093400990427     c                   z-add     dataca        i10dta
093500990426     c                   movel     'N'           i10trc
093600990426     c                   movel     'C'           i10tpd
093700990426     c                   movel     'V'           i10flm
093800990426     c                   movel     dctfca        i10nks
093900990426     c                   movel     DS_numca      i10nkt
094000990426      *
094100990426     c                   movel(P)  FIDN10DS      kpjbu
094200990426     C                   CALL      'FIDN10R'
094300990426     C                   PARM                    kpjba
094400990426      *
094500990426     c                   ENDSR
094600990428      **********************************************************************
094700990428      *  CONTROLLI VIDEATA 2
094800990428      **********************************************************************
094900990428     C     CTRD02        BEGSR
095000000321
095100000321     c                   clear                   v2cmsg
095200990428
095300000320      * controlli per pagamento liquidazione transattiva
095400000320     c                   IF        *in15
095500000320     c                   endif
095600000320      * controlli per pagamento Pratica assicurativa
095700020129      * e per Pratica in Franchigia
095800020129     c                   if        *in17  or  *in18
095900000320      * se richiesto forzatura pagamento e inserito importo trattenuto errore
096000000321     c                   if        v2cfpp = 'SI' and ((v2cksc <> wv2cksc) or
096100000321     c                             (v2cipt <> wv2cipt) or (v2cdpt <> wv2cdpt))
096200000321     c                   movel     msg(10)       V2cmsg
096300000321     c                   endif
096400990428      *
096500990428     c                   IF        V2Cmsg <> *blanks
096600990428     c                   eval      *in28 = *on
096700990428     c                   eval      *in40 = *on
096800990428     c                   goto      ectrd02
096900990428     c                   ENDIF
097000010510      * se non richiesta forzatura i pagamento eseguo altri controlli
097100010510      *
097200010510     c                   if        v2cfpp =  '  '
097300010510     c
097400990428
097500990428      * BENEFICIARIO
097600990428
097700990430      *
097800990430      *    se codificato deve essere su P.d.C. e se ho imp. trattenuto deve essere di fatturazione
097900000321     c                   if        V2Cksc <> *zeros
098000990430     c                   exsr      CTR_benalt
098100990430      *
098200990430     c                   ENDIF
098300990430      *
098400990430     c                   If        V2Cmsg <> *blanks
098500990430     c                   eval      *in28 = *on
098600000321     c                   eval      *in41 = *on
098700990430     C                   endif
098800990430      *
098900990430     c                   if        *in28 = *on  OR  *in29 = *on
099000990430     c                   goto      ectrd02
099100990430     C                   endif
099200990428
099300990430
099400000321      * IMPORTO TRATTENUTO
099500000321      *      se esiste importo trattenuto deve esistere anche il beneficiario
099600000321      *      se esiste importo trattenuto inserire anche la data
099700990430     c                   SELECT
099800990430      *
099900000321     c                   WHEN      V2Cksc = *zeros  and  V2Cipt > *zeros
100000000321     c                   movel     msg(13)       V2Cmsg
100100990430      *
100200000321     c                   WHEN      V2Cdpt = *zeros and V2Cipt > *zeros
100300000321     c                   movel     msg(14)       V2Cmsg
100400000321      *
100500000321     c                   WHEN      V2Cksc > *zeros  and  V2Cipt = *zeros
100600000321     c                   movel     msg(15)       V2Cmsg
100700000321      *
100800000321     c                   WHEN      V2Cdpt > *zeros and V2Cipt = *zeros
100900000321     c                   movel     msg(16)       V2Cmsg
101000000321      *
101100000321     c                   OTHER
101200000321      *     Controllo la Valuta
101300000321     C                   movel     V2Cvlt        WVLT
101400000321     C                   z-add     V2Cipt        W0133
101500000321     C                   exsr      ctrvlt
101600000321
101700990430      *
101800990430     c                   ENDSL
101900990430      *
102000990430     c                   IF        V2Cmsg <> *blanks
102100990430     c                   eval      *in28 = *on
102200000321     c                   eval      *in42 = *on
102300990430     C                   ENDIF
102400990430     c                   IF        *in28 = *on  OR  *in29 = *on
102500990430     c                   goto      ectrd02
102600990430     C                   ENDIF
102700000321
102800000321      * DATA IMPORTO TRATTENUTO
102900000321      *      deve essere corretta
103000000321     c                   if        v2cdpt > *zeros
103100000321     c                   clear                   wlbdat
103200000321     C                   z-add     v2cdpt        G02DAT
103300000321     C                   CALL      'XSRDA8'
103400000321     C                   PARM                    WLBDAT
103500000321     C                   IF        G02ERR = '1'
103600000321     c                   eval      *in43 = *ON
103700000321     c                   eval      *in28 = *ON
103800000321     C                   movel     MSG(17)       V2CMSG
103900000321     C                   ELSE
104000000321      *
104100000321     C                   move      G02INV        W_V2Cdpt
104200000321     C                   z-add     G02DAT        V2cdpt
104300000321     c                   endif
104400000321     c                   endif
104500010510      *
104600010510     c                   ENDIF
104700000321      *
104800000321     c                   ENDIF
104900990430
105000990430     c     ECTRD02       ENDSR
105100000321      **********************************************************************
105200000321      *  CONTROLLI VIDEATA 3
105300000321      **********************************************************************
105400000321     C     CTRD03        BEGSR
105500000321
105600000321     c                   clear                   v2cmsg
105700000321
105800000321      * se ritorno alla fase precedente  e inserito importo richiesto  errore
105900000412     c                   if        v3cfpp = 'SI' and ((v3cipr <> wv3cipr) or
106000000321     c                             (v3cndc <> wv3cndc) or (v3cddc <> wv3cddc))
106100000321     c                   movel     msg(18)       V2cmsg
106200000321     c                   endif
106300000321      *
106400000725     c                   IF        V2Cmsg <> *blanks
106500000321     c                   eval      *in28 = *on
106600000321     c                   eval      *in60 = *on
106700000321     c                   goto      ectrd03
106800000321     c                   ENDIF
106900000725      * se ritorno alla fase precedente  e non ci sono errori
107000000725     c                   if        v3cfpp = 'SI'
107100000725     c                   goto      ectrd03
107200000725     c                   ENDIF
107300000321
107400000321      * IMPORTO RICHIESTO
107500000321      *      se esiste importo RICHIESTO  deve esistere anche il documento
107600000321     c                   SELECT
107700000725      *
107800000321     c                   WHEN      V3Cipr > *zeros  and ((v3cndc = *zeros) or
107900000321     c                             (v3cddc = *zeros))
108000000321     c                   movel     msg(19)       V2Cmsg
108100000321      *
108200000321     c                   WHEN      V3Cipr = *zeros  and ((v3cndc <> *zeros) or
108300000321     c                             (v3cddc <> *zeros))
108400000321     c                   movel     msg(20)       V2Cmsg
108500000321      *
108600000321     c                   OTHER
108700000321      *     Controllo la Valuta
108800000321     C                   movel     V3Cvpr        WVLT
108900000321     C                   z-add     V3Cipr        W0133
109000000321     C                   exsr      ctrvlt
109100000321
109200000321      *
109300000321     c                   ENDSL
109400000321      *
109500000321     c                   IF        V2Cmsg <> *blanks
109600000321     c                   eval      *in28 = *on
109700000321     c                   eval      *in61 = *on
109800000321     C                   ENDIF
109900000321     c                   IF        *in28 = *on  OR  *in29 = *on
110000000321     c                   goto      ectrd03
110100000321     C                   ENDIF
110200000321
110300000321      * NUMERO DOCUMENTO
110400000321      *      deve essere maggiore di zero
110500000321     c                   if        v3cndc = *zeros
110600000321     C                   movel     MSG(17)       V2CMSG
110700000321     c                   eval      *in28 = *on
110800000321     c                   eval      *in62 = *on
110900000321     c                   goto      ectrd03
111000000321     C                   endif
111100000321
111200000321      * DATA DOCUMENTO
111300000321      *      deve essere corretta
111400000321     c                   if        v3cddc > *zeros
111500000321     c                   clear                   wlbdat
111600000321     C                   z-add     v3cddc        G02DAT
111700000321     C                   CALL      'XSRDA8'
111800000321     C                   PARM                    WLBDAT
111900000321     C                   IF        G02ERR = '1'
112000000321     c                   eval      *in63 = *ON
112100000321     c                   eval      *in28 = *ON
112200000321     C                   movel     MSG(17)       V2CMSG
112300000321     C                   ELSE
112400000321      *
112500000321     C                   move      G02INV        W_V3Cddc
112600000321     C                   z-add     G02DAT        V3cddc
112700000321     c                   endif
112800000412      *
112900000412     c                   endif
113000000321
113100000321     c     ECTRD03       ENDSR
113200990430      **************************************************************************
113300990430      * CONTROLLO IL BENEFICIARIO ALTERNATIVO
113400990430      **************************************************************************
113500990430     C     CTR_benalt    BEGSR
113600990430      *
113700990430      *    Aggancio anagrafico
113800990430     c                   clear                   ACOflg
113900990430     C                   clear                   TIBS69DS
114000990430     c                   z-add     V2Cksc        I69kac
114100000414     c                   z-add     V2Cksc        I69kcp
114200990430     c                   exsr      CTR_anagra
114300990430      *
114400990430     c                   SELECT
114500990430      *       non trovato o annullato errore
114600990430     c                   WHEN        *in30 = *on  or  ACOflg = '*'
114700000321     c                   movel     msg(11)       v2cmsg
114800990430      *
114900000321     c                   WHEN        v2cipt = *zeros
115000990430     c                   eval      *in29 = (V2Cksc <> WV2Cksc)
115100990430     c                   movel     ACOrag        V2Dksc
115200990503     c                   z-add     V2Cksc        WV2Cksc
115300990430      *
115400990430     c                   OTHER
115500990430      *    Controllo che sia il codice di fatturazione del beneficiario
115600990430     C                   clear                   TIBS69DS
115700000412     c                   If          DCTptr = 'M'
115800990430     c                   z-add     VTCksm        I69kcp
115900990430     c                   Else
116000990430     c                   z-add     VTCksd        I69kcp
116100990430     c                   Endif
116200990430     c                   movel     I69kcp        W0040
116300990430      *
116400990430     c                   If        W0040 <> 9999  AND  W0040 <> 8888
116500990430     c                   exsr      CTR_anagra
116600990430     c                   If        *in30 = *on  or  ACOflg = '*'
116700990430     c                                          or  V2Cksc <> CLPscf
116800000414     c                   movel     msg(12)       v2cmsg
116900000414     c                   eval      WCLPSCF = '(' + (%editc(CLPSCF:'Z')) + ')'
117000000414     C                   MOVE      WCLPSCF       V2CMSG
117100990430     c                   Else
117200990430     c                   eval      *in29 = (V2Cksc <> WV2Cksc)
117300990430     c                   movel     ACOrag        V2Dksc
117400990503     c                   z-add     V2Cksc        WV2Cksc
117500990430     c                   Endif
117600990430     c                   Endif
117700990430      *
117800990430     c                   ENDSL
117900990430      *
118000990430     C                   ENDSR
118100990426      *****************************************************************
118200990426      * CONTROLLO DIVISA
118300990426      *****************************************************************
118400990426     C     CTRVLT        BEGSR
118500990426      *
118600990426     C                   MOVEL     'CV'          KCOD
118700990426      * RICERCA
118800990426     C     '?'           SCAN      WVLT                                   30
118900990426      *
119000990426     C                   if        *IN30 = *ON
119100990426      *
119200990426     C                   CALL      'X§TABER'
119300990426     C                   PARM                    CODUT
119400990426     C                   PARM                    KCOD
119500990426     C                   PARM                    §KEY
119600990426     C                   PARM                    §DES
119700990426      *
119800990426     C                   MOVEL     §KEY          WVLT
119900990426     C                   eval      *in29 = *ON
120000990426     C                   GOTO      ENDVLT
120100990426     C                   endif
120200990426      *
120300990426      * CONTROLLO DIVISA
120400990426     C                   MOVEL(P)  WVLT          KKEY
120500990426     C     KTAB          CHAIN     TABEL00f                           30
120600990426      * ERRORE
120700990426     C                   if        *IN30 = *ON    OR
120800990426     C                              ( *IN30 = *OFF  and  TBLFLG <> *blanks)
120900000412     C                   MOVEL     msg(22)       V2CMSG
121000990426     C                   eval      *IN28 = *ON
121100990426     C                   GOTO      ENDVLT
121200990426     C                   endif
121300990426      *
121400990426     C                   MOVEL     TBLUNI        DSCV
121500990426      *
121600990426      * VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
121700990426     C                   IF        §CVFDC <> 'S'
121800990426     C                   MOVE      W0133         W0033
121900990426     C                   if        W0033 <> 0
122000000412     C                   movel     msg(23)       V2CMSG
122100990426     C                   eval      *IN28 = *ON
122200990426     C                   GOTO      ENDVLT
122300990426     C                   endif
122400010907     C                   else
122500010907      * arrotondo al decimale richiesto
122600010910     C     W0133         casNE     0             srCTRDEC
122700010907     C                   ENDCS
122800010910     C                   IF        *in28
122900010910     C                   MOVEL     MSG(24)       V2CMSG
123000010910     c                   move      §cvDEC        w002a
123100010914     c                   eval      %subst(V2CMSG:29:2) = w002a
123200010910     C                   GOTO      ENDVLT
123300010910     C                   ENDIF
123400990426     C                   ENDIF
123500990426      *
123600990426     C     ENDVLT        ENDSR
123700010907
123800010907      *****************************************************************
123900010910      * VERIFICA NUMERO DECIMALI IMPOSTATI
124000010907      *****************************************************************
124100010910     C     srCTRDEC      BEGSR
124200010907      *
124300011017     C                   reset                   XDECDS
124400010907      *
124500011017     C                   z-add     W0133         ICDnum
124600011017     C                   z-add     §cvDEC        ICDnrd
124700010907      *
124800011017     C                   call      'XDEC'
124900011017     C                   parm                    XDECDS
125000010907      *
125100011017     C                   IF        OCDesi  <> *off
125200010910     C                   eval      *IN28 = *ON
125300010907     C                   ENDIF
125400010907      *
125500010907     C                   ENDSR
125600010907
125700010907      *****************************************************************
125800010907      * CONVERSIONE IMPORTI IN MONETA CORRENTE
125900010907      *****************************************************************
126000010907     C     srCMB         BEGSR
126100010907      *
126200010907     C                   reset                   YEURCODS
126300010907      *
126400010907     C                   movel     WVLT          yecDVI
126500010907     C                   z-add     W0133         yecIMI
126600010910     C                   movel     §gedDBA       yecDVO
126700010907      *
126800010907     C                   call      'YEURCO'
126900010907     C                   parm                    YEURCODS
127000010907      *
127100010907     C                   IF        yecESI  = ' '
127200010907     C                   z-add     yecIMO        W0133
127300010907     C                   ENDIF
127400010907      *
127500010907     C                   ENDSR
127600050719      ****************************************************************
127700050719      *  GESTIONE FINESTRA SULL'AFFIDAMENTO PRATICHE
127800050719      ****************************************************************
127900050719     C     GES_WIND      BEGSR
128000050719
128100050719     c                   eval      w01aff = 'NO'
128200050719
128300050719
128400050719     c                   do        *hival
128500050719      *
128600050719     c                   exfmt     fi59w01
128700050719      * per cmd 6 aggiorno fndct
128800050719     c                   if        *inkf
128900131011      * se affidata ad A.I.G. valorizzo il dctflo
129000050719     c                   if        w01aff = 'SI'
129100050719     c                   eval      §dctaffi = ' '
129200050720     c                   movel     ddct01        dctflo
129300050719      * scrivo le note di affidamento
129400050719     c                   EVAL      NOTE(1) = NT(12)
129500050719     C                   EXSR      WRTDCS
129600050719     c                   endif
129700050719     c                   leave
129800050719     c                   endif
129900050719
130000050719     c                   enddo
130100050719
130200050719     C                   ENDSR
130300000321      *****************************************************************
130400000321      *   CONFERMA DATI C. A.  IN PAGAMENTO
130500000321      *****************************************************************
130600000321     C     AGGIORNA2     BEGSR
130700000321
130800000404      * pulisco i campi relativi alle note
130900000404     c                   clear                   JJ
131000000404     c                   clear                   NOTE
131100000321     C* Preparo i dati da scrivere nelle note e piloto le scritture sui files
131200000404      *
131300000404      * LIQUIDAZIONE TRANSATTIVA
131400000321      *
131500000321     C                   if        *in15 and v2cfpp = 'SI'
131600000404      * Scrivo le note
131700000404     c                   EVAL      NOTE(1) = NT(1)
131800000404     c                   EVAL      NOTE(2) = NT(2)
131900000404     C                   EXSR      WRTDCS
132000000404      * Valorizzo i campi da aggiornare in fndct e fndcl
132100000404     c                   eval      §dctpaga = 'P'
132200000404     c                   eval      dctflo = ddct01
132300000410     c                   clear                   dctft1
132400050221     c**!!!              clear                   dctdt1
132500000410     c                   clear                   dctft2
132600050221     c**!!!              clear                   dctdt2
132700000411     c                   update    fndct000
132800000404      *
132900000412      * aggancio fndcl
133000000412     c     kca           CHAIN     FNDCL000                           30
133100000404     c                   eval      dclfic = 'C'
133200000410     c                   clear                   dclftr
133300000410     c                   clear                   dcldtr
133400000411     c                   update    fndcl000
133500000404      *
133600000404     c                   endif
133700000404      *
133800020129      * PRATICA ASSICURATIVA o PRATICA IN FRANCHIGIA
133900000404      *
134000000407      * forzatura pagamento
134100020129     C**!!!              if        *in17 and v2cfpp = 'SI'
134200020129     C                   if        (*in17 or *in18 ) and v2cfpp = 'SI'
134300000404      * Scrivo le note
134400020129     c   17              EVAL      NOTE(1) = NT(3)
134500020129     c   17              EVAL      NOTE(2) = NT(4)
134600020129     c   18              EVAL      NOTE(1) = NT(10)
134700020129     c   18              EVAL      NOTE(2) = NT(11)
134800000404     C                   EXSR      WRTDCS
134900000404      * Valorizzo i campi da aggiornare in fndct e fndcl
135000000404     c                   eval      §dctpaga = 'P'
135100000404     c                   eval      dctflo = ddct01
135200000410     c                   clear                   dctft1
135300050221     c**!!!              clear                   dctdt1
135400000410     c                   clear                   dctft2
135500050221     c**!!!              clear                   dctdt2
135600000411     c                   update    fndct000
135700000404      *
135800000404     c                   eval      dclfic = 'C'
135900000410     c                   clear                   dclftr
136000000410     c                   clear                   dcldtr
136100000411     c                   update    fndcl000
136200000404      *
136300000404     c                   endif
136400000411      * modifica importo trattenuto
136500020129     C**!!!              if        *in17 and v2cfpp <> 'SI' and
136600020129     C                   if        (*in17 or *in18) and v2cfpp <> 'SI' and
136700000411     c                             ((Dctksc <> v2cksc) or (dclipt <> v2cipt))
136800000411     c*
136900000410      * Scrivo le note solo se è stato
137000000411     c                   EVAL      NOTE(1) = NT(8)
137100000411     c                   eval      %subst(NOTE(1):33:3) = dclvpt
137200000410      * preparo gli importi nel campo note
137300000412     c                   z-add     dclipt        old123
137400000412     c                   z-add     v2cipt        new123
137500000412     c                   eval      note(2) = %editc(old123:'3') + ' a ' + v2cvlt
137600000412     c                             + ' '  +  %editc(new123:'3')
137700000410      *
137800000407     C                   EXSR      WRTDCS
137900000411     c*
138000000410      * Valorizzo i campi da aggiornare in fndct e fndcl ed annullo se esisteva un beneficiario
138100000410      * alternativo diverso
138200000410      * verifico se è cambiato il beneficiario
138300000410     c                   if        dctksc <> v2cksc
138400000410      * aggancio fndka per annullarlo
138500000410     c     KDKA          CHAIN     FNDKA000                           30
138600000410      *
138700000410     c                   if        not *in30
138800000410     c                   eval      dkaatb = 'A'
138900000410     c                   clear                   dkaftr
139000000410     c                   clear                   dkadtr
139100000410     c                   update    fndka000
139200000410     c                   endif
139300000410
139400000410     c                   move      v2cksc        dctksc
139500000410     c                   clear                   dctft1
139600050221     c**!!!              clear                   dctdt1
139700000410     c                   clear                   dctft2
139800050221     c**!!!              clear                   dctdt2
139900000411     c                   update    fndct000
140000000410      *
140100000410     c                   endif
140200000410      *
140300000411     c                   eval      dclipt =v2cipt
140400010907     c                   if        DCLipt <> 0
140500000412     c                   eval      dclvpt =v2cvlt
140600010907     c                   else
140700010907     c                   clear                   dclvpt
140800010907     c                   ENDIF
140900000411     c                   eval      dcldpt = W_V2cdpt
141000000411     c                   clear                   dclftr
141100000411     c                   clear                   dcldtr
141200000411     c                   update    fndcl000
141300000410      *
141400000410     c                   endif
141500000404      *
141600000404     c                   ENDSR
141700000321      *
141800000411      *****************************************************************
141900000411      *   CONFERMA DATI C. A.  Ritorno fase precedente
142000000411      *****************************************************************
142100000411     C     AGGIORNA3     BEGSR
142200000411
142300000411      * pulisco i campi relativi alle note
142400000411     c                   clear                   JJ
142500000411     c                   clear                   NOTE
142600000411     C* Preparo i dati da scrivere nelle note e piloto le scritture sui files
142700000411      *
142800000411      * RITORNO FASE PRECEDENTE
142900000411      *
143000000411     C                   if        v3cfpp = 'SI'
143100000411      * Scrivo le note
143200000411     c                   EVAL      NOTE(1) = NT(5)
143300000411     C                   EXSR      WRTDCS
143400000411      * Valorizzo i campi da aggiornare in fndct e fndcl
143500000411      *
143600000411      * ricerca fase successiva
143700000411     c                   EXSR      SUB_FASU
143800000411     c                   eval      dctfca = v3cpfa
143900000411     c                   clear                   dctft1
144000050221     c**!!!              clear                   dctdt1
144100000411     c                   clear                   dctft2
144200050221     c**!!!              clear                   dctdt2
144300000411     c                   update    fndct000
144400000411      *
144500000411      * scrivo la fase
144600000411      *
144700000411     c                   exsr      WRTDCF
144800000411      *
144900000411     c                   endif
145000000411      *
145100000411      * MODIFICATO IMPORTO RICHIESTO
145200000411      *
145300000411     C                   if        v3cfpp <> 'SI' and ((v3cipr <> dclipr) or
145400000411     c                             (v3cndc <> dclndc) or (W_v3cddc <> dclddc))
145500000411      * Scrivo le note
145600000411     c                   EVAL      NOTE(1) = NT(6)
145700000411     c                   eval      %subst(NOTE(1):33:3) = dclvpr
145800000411      * preparo gli importi nel campo note
145900000412     c                   z-add     dclipr        old123
146000000412     c                   z-add     v3cipr        new123
146100000412     c                   eval      note(2) = %editc(old123:'3') + ' a ' + v3cvpr
146200000412     c                             + ' '  +  %editc(new123:'3')
146300000411      *
146400000411     C                   EXSR      WRTDCS
146500000411      * Valorizzo i campi da aggiornare in fndcl
146600000411      *
146700000411     c                   eval      dclddc = W_v3cddc
146800000411     c                   eval      dclndc = v3cndc
146900000411     c                   eval      dclipr = v3cipr
147000010907     c                   if        dclipr <> 0
147100010907     c                   eval      dclvpr = v3cvpr
147200010907     c                   else
147300010907     c                   clear                   dclvpr
147400010907     c                   endif
147500000411     c                   clear                   dclftr
147600000411     c                   clear                   dcldtr
147700000411     c                   update    fndcl000
147800000411      *
147900000411     c                   endif
148000000411      *
148100000411     c                   ENDSR
148200000411      *****************************************************************
148300000411      * RICAVO FASE SUCCESSIVA
148400000411      *****************************************************************
148500000411     C     SUB_FASU      BEGSR
148600000411      *
148700000411     c                   CLEAR                   FIDN05DS
148800000411     c                   movel     'F'           I05MOD
148900000411     c                   z-add     v3cpfa        I05fca
149000000411     c                   movel     dctfpr        I05fpr
149100000411     c                   movel     dctptr        I05tpc
149200000411     c                   movel     'O'           I05ffs
149300000411     c                   movel     ' '           I05fpe
149400000411     c                   z-add     DATEU         i05dta
149500000411     c                   move      dcttad        i05tad
149600000411      * Valorizzo numero CA
149700000411     c                   z-add     DCTaac        i05aac
149800000411     c                   z-add     DCTfil        i05fil
149900000411     c                   z-add     DCTnca        i05nca
150000000411
150100000411     C                   CALL      'FIDN05R'
150200000411     c                   PARM                    KPJBA
150300000411     c                   PARM                    FIDN05DS
150400000411      *
150500000411     c                   movel     O05REC        DS_FNDFA
150600000411      *
150700000411     c                   SELECT
150800000411     c                   WHEN      DFAgfs = 'P'
150900000412     c                   z-add     Wlnp          dctgfc
151000000411     c                   WHEN      DFAgfs = 'S'
151100000411     c                   z-add     46            dctgfc
151200000411     c                   WHEN      DFAgfs = 'A'
151300000411     c                   z-add     DCTlna        dctgfc
151400000411     c                   OTHER
151500000411     c                   z-add     DCTfil        dctgfc
151600000411     c                   ENDSL
151700000411      *
151800000411     c                   ENDSR
151900990426      *****************************************************************
152000990426      *  SCRITTURA FASI C.A.
152100990426      *****************************************************************
152200990426     c     WRTDCF        BEGSR
152300990426      *
152400990426     c                   clear                   fndcf000
152500990426      *
152600990430     c                   z-add     DCTaac        DCFaac
152700990430     c                   z-add     DCTfil        DCFfil
152800990430     c                   z-add     DCTnca        DCFnca
152900000411     c                   movel     DCTptr        DCFptr
153000000411     c                   z-add     DCTfca        DCFfca
153100990426      *
153200990426     c                   z-add     dateu         DCFdfc
153300990426     c                   movel     w0140         DCFhfc
153400000411     c                   if        simfel = 0
153500000411     c                   z-add     46            DCFfev
153600000411     c                   else
153700000411     c                   z-add     simfel        DCFfev
153800000411     c                   endif
153900990426     c                   movel     knmus         DCFpru
154000020129     c**!!!              clear                   DCFlet
154100990426     c                   clear                   DCFftr
154200990426     c                   clear                   DCFdtr
154300020129
154400020129      * Imposto il flag di pratica assicurativa in Franchigia
154500020129     c                   eval      §dcfpfra = §dctpfra
154600020129     c                   movel     ddcf01        dcflet
154700990426      *
154800990426     c                   write     fndcf000
154900990426      *
155000000412     c                   endsr
155100000321      *****************************************************************
155200000321      *  SCRIVO NOTE DELLE MODIFICHE EFFETTUATE
155300000321      *****************************************************************
155400000321     C     WRTDCS        BEGSR
155500000321      *
155600000321     c* cerco l'ultimo progressivo note della fase in corso
155700000321     c* scrittura nelle note dei dati relativi alla CA da mantenere
155800000321     c                   movel     dctfca        dcsnks
155900000321     c                   eval      dcstrc='N'
156000000321     c                   eval      dcspno=0
156100000321     c*
156200000321     c* scrittura note preparazione dei campi strandard
156300000321     c                   eval      dcsatb=' '
156400000321     c                   eval      dcstpd='C'
156500000321     c                   eval      dcsnkt=ds_numca
156600000321     c                   eval      dcsstd=' '
156700000321     c                   eval      dcsdim=dateu
156800000412      * ORA
156900000412     C                   TIME                    W0140
157000000412      *
157100000321     c                   movel     w0140         dcshim
157200000321     c*
157300000321     c* cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
157400000321     c     keydcs        setgt     fndcs01l
157500000321     c     keydcsp       readpe    fndcs01l                               93
157600000321     c*
157700000321     c                   move      knmus         dcspru
157800000321     c                   movel     simfel        dcspos
157900000321     c                   clear                   dcsstn
158000000321     c                   clear                   dcsft1
158100000321     c                   clear                   dcsdt1
158200000404     c* scrivo  righe di note per ogni modifica effettuata
158300000404     c                   z-add     0             jj
158400000404     c                   do        *hival
158500000404      *
158600000404     c                   add       1             JJ
158700000404      *
158800000404     c                   if        jj > 10
158900000404     c                   leave
159000000404     c                   endif
159100000404      *
159200000404     c                   if        note(jj) = *blanks
159300000404     c                   leave
159400000404     c                   endif
159500000321     c*
159600000404      * Preparo la prima riga di note a secondo della  situazione
159700000321     c                   add       1             dcspno
159800000321     c                   clear                   dcsnot
159900000404      *
160000000404     c                   movel     note(jj)      dcsnot
160100000404      *
160200000321     c                   write     fndcs000
160300000404      *
160400000404     c                   enddo
160500000321      *
160600000321     C                   ENDSR
160700990426      *****************************************************************
160800990426      *  AGGANCIO ANAGRAFICHE
160900990426      *****************************************************************
161000990426     C     CTR_ANAGRA    BEGSR
161100990428      *
161200990426     C                   CALL      'TIBS69R'
161300990426     C                   PARM                    tibs69DS
161400990426     C                   PARM                    DS_cnaco
161500990426     C                   PARM                    DS_cnind
161600990426     C                   PARM                    DS_cnclp
161700990426     C                   PARM                    DS_fncls
161800990426      *
161900990426     C                   eval      *IN30 = (O69ERR =  '1')
162000990426      *
162100990426     C                   eval      wtibs69r = 'S'
162200990426      *
162300990426     C                   ENDSR
162400990426      *****************************************************************
162500990426      *  ROUTINE INIZIALE
162600990426      *****************************************************************
162700990426     C     *INZSR        BEGSR
162800990426
162900990426     C     *ENTRY        PLIST
163000990426     C                   PARM                    KPJBA
163100000317     C                   MOVEL     KPJBU         ENTRATA
163200000317      * controllo che tipo di modifica si vuol fare
163300000412     C                   eval      *in10 = (entrata = '9')                      * C.A. in pagamento
163400000412     C                   eval      *in20 = (entrata = '6')                      * C.A. in ricez.doc.
163500990426
163600000317     c   10              movel     TIT10         VTCtit
163700000317     c   20              movel     TIT20         VTCtit
163800990503
163900990426      * Dati società
164000990426     C                   Z-ADD     1             CODUT
164100990426     C                   CALL      'X§PARUT'
164200990426     C                   PARM                    UT§DSE
164300990426      *
164400990426     C                   MOVEL     RAGUT         RSUT
164500990426     C                   MOVEL     REC80         CNCR80
164600990426      *
164700990426      *--- RICERCA CAPOCONTI
164800990426     C                   DO        50            XX
164900990426     C                   MOVE      TCU(XX)       TCUDS
165000990426     C     f56           CABNE     'CG'          END1
165100990426     C     f4            COMP      '1'                                    21
165200990426     C     f4            COMP      '2'                                    22
165300990426     C     f4            COMP      '3'                                    23
165400990426     C     f4            COMP      '6'                                    27
165500990426     C** 1 CLIENTI   21
165600990426     C** 2 FORNITORI 22
165700990426     C** 3 AGENTI    23
165800990426     C     f3            comp      '0'                                242425
165900990426     C     f3            comp      'I'                                    26
166000990426     C** 0 ITALIA   25
166100990426     C** 1 ESTERO   24
166200990426     ** I CAPO CONTO IVA
166300990426     c                   if        *in21 = *on
166400990426     C   24              Z-ADD     KCU(XX)       KCE               4 0
166500990426     C   25              Z-ADD     KCU(XX)       KCI               4 0
166600990426     c                   end
166700990426     c                   if        *in22 = *on
166800990426     C   24              Z-ADD     KCU(XX)       KFE               4 0
166900990426     C   25              Z-ADD     KCU(XX)       KFI               4 0
167000990426     c                   end
167100990426     c                   if        *in23 = *on
167200990426     C   24              Z-ADD     KCU(XX)       KAE               4 0
167300990426     C   25              Z-ADD     KCU(XX)       KAI               4 0
167400990426     c                   end
167500990426     C   26              Z-ADD     KCU(XX)       KIVA              4 0
167600990426     C   27              Z-ADD     KCU(XX)       KBNA              4 0
167700990426     C     end1          tag
167800990426     C                   end
167900990426     C                   SETOFF                                       212223
168000990426     C                   SETOFF                                       242526
168100990426     C                   SETOFF                                       27
168200000412
168300990426      * ORA
168400990426     C                   TIME                    W0140
168500990426      * UDATE IN GGMMAAAA
168600990426     C                   MOVE      W0140         WDTGIO
168700990426      * UDATE IN AAAAMMGG
168800990426     C                   Z-ADD     WDTGIO        G02DAT
168900990426     C                   MOVEL     *BLANK        G02ERR
169000990426     C                   CALL      'XSRDA8'
169100990426     C                   PARM                    WLBDAT
169200990426     C                   MOVEL     G02INV        DATEU
169300990503     C                   MOVEL     G02INV        V1Caac
169400990426
169500010907      * recupero la moneta corrente
169600010907     C                   CLEAR                   TIBS02DS
169700010907     C                   MOVEL     'C'           T02MOD
169800010907     C                   MOVEL     KNSIF         T02SIF
169900010907     C                   MOVEL     'GED'         T02COD
170000010907     C                   MOVEL     'DANNI'       T02KE1
170100010907     C                   CALL      'TIBS02R'
170200010907     C                   PARM                    KPJBA
170300010907     C                   PARM                    TIBS02DS
170400010907      *
170500010910     C                   MOVEL     T02UNI        DGEDDN
170600990426
170700990426     C     KTAB          KLIST
170800990426     C                   KFLD                    CODUT
170900990426     C                   KFLD                    KCOD
171000990426     C                   KFLD                    KKEY
171100990426      *
171200990426     c     kbol01        klist
171300990428     c                   kfld                    DCTaas
171400990428     c                   kfld                    DCTlnp
171500990428     c                   kfld                    DCTnrs
171600990428     c                   kfld                    DCTnsp
171700990426      *
171800990426     C     KDET          KLIST
171900990427     C                   KFLD                    DCTaae
172000990427     C                   KFLD                    DCTnev
172100990426      *
172200000321     c     keydcs        KLIST
172300000321     c                   KFLD                    dcstpd
172400000321     c                   KFLD                    dcsnkt
172500000321     c                   KFLD                    dcsstd
172600000321     c                   KFLD                    dcsdim
172700000321     c                   KFLD                    dcshim
172800000321     c                   KFLD                    dcsnks
172900000321     c                   KFLD                    dcstrc
173000000321     c                   KFLD                    prgnot
173100000321     c                   eval      prgnot= 999
173200000321     c* di fndcs01l - parziale
173300000321     c*
173400000321     c     keydcsp       KLIST
173500000321     c                   KFLD                    dcstpd
173600000321     c                   KFLD                    dcsnkt
173700000321     c                   KFLD                    dcsstd
173800000321     c                   KFLD                    dcsdim
173900000321     c                   KFLD                    dcshim
174000000321     c                   KFLD                    dcsnks
174100000321     c                   KFLD                    dcstrc
174200000321      *
174300990426     c     kca           klist
174400990503     c                   kfld                    v1caac
174500990503     c                   kfld                    v1cfil
174600990503     c                   kfld                    v1cnca
174700000320      *
174800000320     c     KDKA          klist
174900000320     c                   kfld                    dctaac
175000000320     c                   kfld                    dctfil
175100000320     c                   kfld                    dctnca
175200000320     c                   kfld                    Wtrc
175300000320      *
175400990426     C                   ENDSR
175500990426      *------------------------------------------------------------
175600990426** MSG  L78                                                                  *
175700990427Immesso numero C.A. non valido                                                 1
175800990427La Comunicazione Anomalia selezionata è inesistente                            2
175900990427La Comunicazione Anomalia selezionata è annullata                              3
176000000315La Comunicazione Anomalia selezionata non è chiusa                             4
176100000317La Comunicazione Anomalia selezionata non è in fase gestione documenti         5
176200000317Non trovata spedizione relativa alla Comunicazione Anomalia                    6
176300000317La Comunicazione Anomalia non è chiusa con una causale di pagamento            7
176400120724LIBERO - ex ERRORE Non è possibile modificare C.A. perchè è in fase di pagamento           8
176500120724LIBERO - ex ERRORE Non è possibile modificare C.A. perchè pagamento a carico Consuldanni   9
176600000321ERRORE Modificare i dati dell'importo trattenuto o forzare il pagamento        10
176700000321Codice errato o annullato                                                      11
176800000414Beneficiario Alternativo non è il cod.fatturazione del Beneficiario            12
176900000321Se inserito importo trattenuto inserire anche il beneficiario                  13
177000000321Se inserito importo trattenuto inserire anche la data                          14
177100000321Se non inserito importo trattenuto non inserire neppure il beneficiario        15
177200000321Se non inserito importo trattenuto non inserire neppure la data                16
177300000321Data errata                                                                    17
177400000321ERRORE Modificare i dati dell'importo richiesto o tornare indietro di fase     18
177500000321Se inserito importo richiesto inserire anche data e numero documento           19
177600000321Se non inserito importo richiesto non inserire  data e numero documento        20
177700010911LIBERO - ex Se immessa la divisa immettere anche l'importo o eliminare entrambi            21 33
177800000412Divisa errata                                                                  22 34
177900000412Non ammessi importi con decimali per la divisa indicata                        23 35
178000010910Ammessi importi con massimo __ decimali per la divisa indicata                 24
178100000321** NT Note automatiche in caso di modifica LUNG. 35
178200000321Forzato Pagamento C.A. Liquidazione    1
178300000321Transattiva                            2
178400000321Forzato Pagamento C.A. Pratica         3
178500000321Assicurativa                           4
178600000321Riportata indietro di fase             5
178700000321Modificato importo richiesto da xxx    6
178800000321xxxxxxxxx,xxx a xxx xxxxxxxxx,xxx      7
178900000321Modificato impor. trattenuto da xxx    8
179000000321xxxxxxxxx,xxx a xxx xxxxxxxxx,xxx      9
179100020129Forzato Pagamento C.A. Pratica         10
179200020129in Franchigia                          11
179300131011Tolto affidamento pratica ad A.I.G.    12
