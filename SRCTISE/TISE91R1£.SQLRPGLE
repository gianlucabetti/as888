000100000000     H*------------------------------------------------------------------------*
000200080916     H*    Esegue la somma del WADELPCE sul WADELPCT (Italia+Estero)           *
000300080916     H*      Legge il File dell'Estero e lo somma su quello del totale.        *
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900081027     FWADELPCE  uF   E             DISK    prefix(INE:3) usropn
001000081027     F                                     rename(WADELPC0:wadelE)
001100080916      *
001200081027     FWADELPCT  uF a E             DISK    rename(WADELPC0:wadelT) usropn
001300080916      *
001400081027     FWADELPCF  uF   E             DISK    prefix(INF:3) usropn
001500081027     F                                     rename(WADELPC0:wadelF)
001600081027      *
001700091209     FWADELPCN  uF   E             DISK    prefix(INN:3) usropn
001800091209     F                                     rename(WADEL00N:wadelN)
001900091209      *
002000081027     F*------------------------------------------------------------------------*
002100081027     FAZORG01L  IF   E           K DISK
002200081027     FTABEL00F  IF   E           K DISK
002300081027     D*------------------------------------------------------------------------*
002400081027     D* SCHIERE
002500081027     D*------------------------------------------------------------------------*
002600081027     D*------------------
002700081027     D* ORGANIGRAMMA
002800081027     D*------------------
002900081027     D FIL             S              3  0 DIM(500)                             *FILIALI
003000081027     D DFIL            S             20    DIM(500)
003100081027     D ARE             S              3  0 DIM(500)                             *AREE
003200081027     D DARE            S             20    DIM(500)
003300081027     D DIV             S              1    DIM(500)                             *DIVISIONI
003400081027     D DDIV            S             20    DIM(500)
003500081027     D*------------------
003600080916      * Record Nr. per agganciare il file di totale come file Fisico
003700080916     D RRN_PCT         s              9  0
003800080916      *
003900000000     D*------------------
004000000000     D* ARCHITETTURA
004100000000     D*------------------
004200000000     D KPJBA         E DS
004300080916      *
004400000000     D*------------------
004500000000     D* PARAMETRI DI LANCIO
004600000000     D*------------------
004700000000     D PARAM           DS
004800000000     D  PARDAT                 1      6  0
004900000000     D  PARAAI                 1      4  0
005000000000     D  PARMMI                 5      6  0
005100000000     D  PARDA2                 7     12  0
005200000000     D  PARAAF                 7     10  0
005300000000     D  PARMMF                11     12  0
005400000000     D  PARDCK                13     13
005500000000     D  PARDCC                14     14
005600000000     D  PARDCF                15     15
005700000000     D  PARFIC                16     18  0
005800000000     D  PARARC                19     21  0
005900000000     D  PARDIC                22     22
006000000000     D  PARDFF                23     23
006100000000     D  PARCOF                24     30  0
006200000000     D  PARDIF                31     31
006300000000     D  PARSTA                32     32
006400000824     D  PARSCE                33     33
006500000000     D  PARTSP                34     34
006600000000     D  PARIOE                35     35
006700000000     D  PARCLV                36     36
006800000000     D  PARDPT                37     42  0
006900000000     D  PARDP2                43     48  0
007000000000     D  PARSPD                49     52  1
007100000000     D  PARPDD                53     56  1
007200000000     D  PARPDA                57     60  1
007300000000     D  PARPOS                61     61
007400000825     D  PARCRE                62     62
007500010618     D  PARCL1                63     63
007600010618     D  PARCL2                64     64
007700010618     D  PARSIN                65     65
007800020404     D  PARCL3                66     66
007900020404     D  PARCL4                67     67
008000020404     D  PARNTW                68     68
008100030923     D  PARDFD                69     72  1
008200030923     D  PARDFA                73     76  1
008300030923     D  PARDFDP               77     80  1
008400030923     D  PARDFAP               81     84  1
008500030923     D  PARFPC                85     85
008600030923     D  PARWPC                86     86
008700030923     D  PARWCP                87     87
008800080916      *
008900000000     C*------------------------------------------------------------------------*
009000000000     C* MAIN LINE
009100000000     C*------------------------------------------------------------------------*
009200080916     C     *ENTRY        PLIST
009300080916     C                   PARM                    KPJBA
009400080916      *
009500080916     C                   MOVEL     KPJBU         PARAM
009600081027     C*
009700081027     C*       carica tabelle Area e Divisione x decodifica
009800081027     c                   exsr      cartab
009900081027     C*
010000081027     c                   open      WADELpcE
010100081027     c                   open      WADELpcT
010200081027     C*
010300080916     C* ciclo completo di lettura dell'Estero da sommare
010400080916     C                   read      WADELpcE
010500000000     C*
010600000000     C* CICLO FINO A FINE FILE DI LAVORO
010700080916DO  1C                   DOW       not %EoF(WADELpcE)
010800000000     C*
010900080916     C* Aggancia il record di Totale se già presente x sommare l'Estero
011000080916     C*  altrimenti scrive il record
011100080916     c                   exsr      Somma_File
011200081027     C*
011300081027     c                   movel     inEauC        CoDFiliale        3 0
011400081027     c                   exsr      Dec_Area
011500081027     C                   eval      inEarAU  = CodAREa
011600081027     C                   eval      inEarAUD = DesAREa
011700081027      *
011800081027      * Aggiorna l'Area dell'agente Unificante
011900081027     c                   update    wadelE
012000000000     C* FINE ANNO
012100080916     C                   read      WADELpcE
012200000000E   1C                   ENDDO
012300081027     C*
012400081027     c                   close     WADELpcE
012500081027     c                   close     WADELpcT
012600081027     C*
012700081027     C* Deve impostare l'area/descrizione area dell'agente unificante sui 3 files.
012800081027     c                   exsr      AREA_AgenUnif
012900081027      *
013000081027     C                   SETON                                        LR
013100020116     C* =========================================================================
013200080916     C*  Esegue il Dettaglio: va in somma al file Italia
013300080916     C* =========================================================================
013400080916     C     Somma_File    BEGSR
013500000000     C*
013600080916     C*  Pulisce il Nr.Rel.Record
013700080916     c                   reset                   rrn_PCT
013800080916     c*
013900080916      *?     Legge come una CHAIN...il fisico dei TOTALI     ?
014000080916     C/EXEC SQL
014100080916     C+ SELECT rrn(WADELpcT) INTO :RRN_PCT FROM wadelPCt WHERE WAPCLI =
014200081027     C+ :inECLI
014300080916     C/END-EXEC
014400080916     c                   if        SQLcod < 0  or SQLcod = 100
014500080916     C*
014600080916     C*  Non trovato il Cliente quindi scrive.
014700080916     c                   exsr      Write_rec
014800080916     C*
014900080916     c                   else
015000080916     C*
015100080916     C*  Trovato il Cliente quindi aggiunge.
015200080916     c                   exsr      Update_rec
015300080916     C*
015400080916     c                   end
015500080916     C*
015600080916     C                   ENDSR
015700080916     C* =========================================================================
015800080916     C*  Scrive il record poichè non c'era il cliente
015900080916     C* =========================================================================
016000080916     C     Write_rec     BEGSR
016100080916      *
016200081027     c                   eval      WAPAAD     = inEAAD
016300081027     c                   eval      WAPMMD     = inEMMD
016400081027     c                   eval      WAPAAA     = inEAAA
016500081027     c                   eval      WAPMMA     = inEMMA
016600081027     c                   eval      WAPIOE     = inEIOE
016700081027     c                   eval      WAPDIV     = inEDIV
016800081027     c                   eval      WAPDID     = inEDID
016900081027     c                   eval      WAPARE     = inEARE
017000081027     c                   eval      WAPARD     = inEARD
017100081027     c                   eval      WAPFIL     = inEFIL
017200081027     c                   eval      WAPFID     = inEFID
017300081027     c                   eval      WAPCLV     = inECLV
017400081027     c                   eval      WAPCLI     = inECLI
017500081027     c                   eval      WAPCLD     = inECLD
017600081027     c                   eval      WAPITC     = inEITC
017700081027     c                   eval      WAPCLN     = inECLN
017800081027     c                   eval      WAPAGE     = inEAGE
017900081027     c                   eval      WAPAGD     = inEAGD
018000081027     c                   eval      WAPSAP     = inESAP
018100081027     c                   eval      WAPkgP     = inEkgP
018200081027     c                   eval      WAPFAP     = inEFAP
018300081027     c                   eval      WAPDAP     = inEDAP
018400081027     c                   eval      WAPSAC     = inESAC
018500081027     c                   eval      WAPkgC     = inEkgC
018600081027     c                   eval      WAPFAC     = inEFAC
018700081027     c                   eval      WAPDAC     = inEDAC
018800081027     c                   eval      WAPBLO     = inEBLO
018900081027     c                   eval      WAPNS_01AP = inENS_01AP
019000081027     c                   eval      WAPkg_01AP = inEkg_01AP
019100081027     c                   eval      WAPRR_01AP = inERR_01AP
019200081027     c                   eval      WAPRP_01AP = inERP_01AP
019300081027     c                   eval      WAPNS_02AP = inENS_02AP
019400081027     c                   eval      WAPkg_02AP = inEkg_02AP
019500081027     c                   eval      WAPRR_02AP = inERR_02AP
019600081027     c                   eval      WAPRP_02AP = inERP_02AP
019700081027     c                   eval      WAPNS_03AP = inENS_03AP
019800081027     c                   eval      WAPkg_03AP = inEkg_03AP
019900081027     c                   eval      WAPRR_03AP = inERR_03AP
020000081027     c                   eval      WAPRP_03AP = inERP_03AP
020100081027     c                   eval      WAPNS_04AP = inENS_04AP
020200081027     c                   eval      WAPkg_04AP = inEkg_04AP
020300081027     c                   eval      WAPRR_04AP = inERR_04AP
020400081027     c                   eval      WAPRP_04AP = inERP_04AP
020500081027     c                   eval      WAPNS_05AP = inENS_05AP
020600081027     c                   eval      WAPkg_05AP = inEkg_05AP
020700081027     c                   eval      WAPRR_05AP = inERR_05AP
020800081027     c                   eval      WAPRP_05AP = inERP_05AP
020900081027     c                   eval      WAPNS_06AP = inENS_06AP
021000081027     c                   eval      WAPkg_06AP = inEkg_06AP
021100081027     c                   eval      WAPRR_06AP = inERR_06AP
021200081027     c                   eval      WAPRP_06AP = inERP_06AP
021300081027     c                   eval      WAPNS_07AP = inENS_07AP
021400081027     c                   eval      WAPkg_07AP = inEkg_07AP
021500081027     c                   eval      WAPRR_07AP = inERR_07AP
021600081027     c                   eval      WAPRP_07AP = inERP_07AP
021700081027     c                   eval      WAPNS_08AP = inENS_08AP
021800081027     c                   eval      WAPkg_08AP = inEkg_08AP
021900081027     c                   eval      WAPRR_08AP = inERR_08AP
022000081027     c                   eval      WAPRP_08AP = inERP_08AP
022100081027     c                   eval      WAPNS_09AP = inENS_09AP
022200081027     c                   eval      WAPkg_09AP = inEkg_09AP
022300081027     c                   eval      WAPRR_09AP = inERR_09AP
022400081027     c                   eval      WAPRP_09AP = inERP_09AP
022500081027     c                   eval      WAPNS_10AP = inENS_10AP
022600081027     c                   eval      WAPkg_10AP = inEkg_10AP
022700081027     c                   eval      WAPRR_10AP = inERR_10AP
022800081027     c                   eval      WAPRP_10AP = inERP_10AP
022900081027     c                   eval      WAPNS_11AP = inENS_11AP
023000081027     c                   eval      WAPkg_11AP = inEkg_11AP
023100081027     c                   eval      WAPRR_11AP = inERR_11AP
023200081027     c                   eval      WAPRP_11AP = inERP_11AP
023300081027     c                   eval      WAPNS_12AP = inENS_12AP
023400081027     c                   eval      WAPkg_12AP = inEkg_12AP
023500081027     c                   eval      WAPRR_12AP = inERR_12AP
023600081027     c                   eval      WAPRP_12AP = inERP_12AP
023700081027     c                   eval      WAPNS_01AC = inENS_01AC
023800081027     c                   eval      WAPkg_01AC = inEkg_01AC
023900081027     c                   eval      WAPRR_01AC = inERR_01AC
024000081027     c                   eval      WAPRP_01AC = inERP_01AC
024100081027     c                   eval      WAPNS_02AC = inENS_02AC
024200081027     c                   eval      WAPkg_02AC = inEkg_02AC
024300081027     c                   eval      WAPRR_02AC = inERR_02AC
024400081027     c                   eval      WAPRP_02AC = inERP_02AC
024500081027     c                   eval      WAPNS_03AC = inENS_03AC
024600081027     c                   eval      WAPkg_03AC = inEkg_03AC
024700081027     c                   eval      WAPRR_03AC = inERR_03AC
024800081027     c                   eval      WAPRP_03AC = inERP_03AC
024900081027     c                   eval      WAPNS_04AC = inENS_04AC
025000081027     c                   eval      WAPkg_04AC = inEkg_04AC
025100081027     c                   eval      WAPRR_04AC = inERR_04AC
025200081027     c                   eval      WAPRP_04AC = inERP_04AC
025300081027     c                   eval      WAPNS_05AC = inENS_05AC
025400081027     c                   eval      WAPkg_05AC = inEkg_05AC
025500081027     c                   eval      WAPRR_05AC = inERR_05AC
025600081027     c                   eval      WAPRP_05AC = inERP_05AC
025700081027     c                   eval      WAPNS_06AC = inENS_06AC
025800081027     c                   eval      WAPkg_06AC = inEkg_06AC
025900081027     c                   eval      WAPRR_06AC = inERR_06AC
026000081027     c                   eval      WAPRP_06AC = inERP_06AC
026100081027     c                   eval      WAPNS_07AC = inENS_07AC
026200081027     c                   eval      WAPkg_07AC = inEkg_07AC
026300081027     c                   eval      WAPRR_07AC = inERR_07AC
026400081027     c                   eval      WAPRP_07AC = inERP_07AC
026500081027     c                   eval      WAPNS_08AC = inENS_08AC
026600081027     c                   eval      WAPkg_08AC = inEkg_08AC
026700081027     c                   eval      WAPRR_08AC = inERR_08AC
026800081027     c                   eval      WAPRP_08AC = inERP_08AC
026900081027     c                   eval      WAPNS_09AC = inENS_09AC
027000081027     c                   eval      WAPkg_09AC = inEkg_09AC
027100081027     c                   eval      WAPRR_09AC = inERR_09AC
027200081027     c                   eval      WAPRP_09AC = inERP_09AC
027300081027     c                   eval      WAPNS_10AC = inENS_10AC
027400081027     c                   eval      WAPkg_10AC = inEkg_10AC
027500081027     c                   eval      WAPRR_10AC = inERR_10AC
027600081027     c                   eval      WAPRP_10AC = inERP_10AC
027700081027     c                   eval      WAPNS_11AC = inENS_11AC
027800081027     c                   eval      WAPkg_11AC = inEkg_11AC
027900081027     c                   eval      WAPRR_11AC = inERR_11AC
028000081027     c                   eval      WAPRP_11AC = inERP_11AC
028100081027     c                   eval      WAPNS_12AC = inENS_12AC
028200081027     c                   eval      WAPkg_12AC = inEkg_12AC
028300081027     c                   eval      WAPRR_12AC = inERR_12AC
028400081027     c                   eval      WAPRP_12AC = inERP_12AC
028500081027     c                   eval      WAPIND     = inEIND
028600081027     c                   eval      WAPCAP     = inECAP
028700081027     c                   eval      WAPLOC     = inELOC
028800081027     c                   eval      WAPPRV     = inEPRV
028900081027     c                   eval      WAPNAZ     = inENAZ
029000081027     c                   eval      WAPRESP    = inERESP
029100081027     c                   eval      WAPEMAR    = inEEMAR
029200081027     c                   eval      WAPCDF     = inECDF
029300081027     c                   eval      WAPIVA     = inEIVA
029400081027     c                   eval      WAPTEL     = inETEL
029500081027     c                   eval      WAPNEW_MES = inENEW_MES
029600081013     C*
029700081013      * ?Campi aggiunti: AGE Unificante e Descrizione
029800081027     c                   eval      WAPaud     = inEAUD                          *Descr.Unificante
029900081027     c                   eval      WAPauc     = inEAUC                          *Cod.  Unificante
030000080916      *
030100081027     c                   write     wadelT
030200080916      *
030300080916     C                   ENDSR
030400080916     C* =========================================================================
030500080916     C*  Aggiorna record con la somma x il cliente
030600080916     C* =========================================================================
030700080916     C     Update_rec    BEGSR
030800080916      *
030900080916     c     rrn_PCT       chain     wadelPCT
031000080916      *
031100080916     c                   if        %Found(wadelPCT)
031200080916      *
031300080916      * Blank in quanto somma "I" + "E"
031400080916     c                   eval      WAPIOE     = *blank
031500080916      *
031600081027     c                   eval      WAPSAP     = inESAP      + WAPSAP
031700081027     c                   eval      WAPkgP     = inEkgP      + WAPkgP
031800081027     c                   eval      WAPFAP     = inEFAP      + WAPFAP
031900081027     c                   eval      WAPSAC     = inESAC      + WAPSAC
032000081027     c                   eval      WAPkgC     = inEkgC      + WAPkgC
032100081027     c                   eval      WAPFAC     = inEFAC      + WAPFAC
032200080916      *
032300081027     c                   eval      WAPNS_01AP = inENS_01AP  + WAPNS_01AP
032400081027     c                   eval      WAPkg_01AP = inEkg_01AP  + WAPkg_01AP
032500081027     c                   eval      WAPRR_01AP = inERR_01AP  + WAPRR_01AP
032600081027     c                   eval      WAPRP_01AP = inERP_01AP  + WAPRP_01AP
032700081027     c                   eval      WAPNS_02AP = inENS_02AP  + WAPNS_02AP
032800081027     c                   eval      WAPkg_02AP = inEkg_02AP  + WAPkg_02AP
032900081027     c                   eval      WAPRR_02AP = inERR_02AP  + WAPRR_02AP
033000081027     c                   eval      WAPRP_02AP = inERP_02AP  + WAPRP_02AP
033100081027     c                   eval      WAPNS_03AP = inENS_03AP  + WAPNS_03AP
033200081027     c                   eval      WAPkg_03AP = inEkg_03AP  + WAPkg_03AP
033300081027     c                   eval      WAPRR_03AP = inERR_03AP  + WAPRR_03AP
033400081027     c                   eval      WAPRP_03AP = inERP_03AP  + WAPRP_03AP
033500081027     c                   eval      WAPNS_04AP = inENS_04AP  + WAPNS_04AP
033600081027     c                   eval      WAPkg_04AP = inEkg_04AP  + WAPkg_04AP
033700081027     c                   eval      WAPRR_04AP = inERR_04AP  + WAPRR_04AP
033800081027     c                   eval      WAPRP_04AP = inERP_04AP  + WAPRP_04AP
033900081027     c                   eval      WAPNS_05AP = inENS_05AP  + WAPNS_05AP
034000081027     c                   eval      WAPkg_05AP = inEkg_05AP  + WAPkg_05AP
034100081027     c                   eval      WAPRR_05AP = inERR_05AP  + WAPRR_05AP
034200081027     c                   eval      WAPRP_05AP = inERP_05AP  + WAPRP_05AP
034300081027     c                   eval      WAPNS_06AP = inENS_06AP  + WAPNS_06AP
034400081027     c                   eval      WAPkg_06AP = inEkg_06AP  + WAPkg_06AP
034500081027     c                   eval      WAPRR_06AP = inERR_06AP  + WAPRR_06AP
034600081027     c                   eval      WAPRP_06AP = inERP_06AP  + WAPRP_06AP
034700081027     c                   eval      WAPNS_07AP = inENS_07AP  + WAPNS_07AP
034800081027     c                   eval      WAPkg_07AP = inEkg_07AP  + WAPkg_07AP
034900081027     c                   eval      WAPRR_07AP = inERR_07AP  + WAPRR_07AP
035000081027     c                   eval      WAPRP_07AP = inERP_07AP  + WAPRP_07AP
035100081027     c                   eval      WAPNS_08AP = inENS_08AP  + WAPNS_08AP
035200081027     c                   eval      WAPkg_08AP = inEkg_08AP  + WAPkg_08AP
035300081027     c                   eval      WAPRR_08AP = inERR_08AP  + WAPRR_08AP
035400081027     c                   eval      WAPRP_08AP = inERP_08AP  + WAPRP_08AP
035500081027     c                   eval      WAPNS_09AP = inENS_09AP  + WAPNS_09AP
035600081027     c                   eval      WAPkg_09AP = inEkg_09AP  + WAPkg_09AP
035700081027     c                   eval      WAPRR_09AP = inERR_09AP  + WAPRR_09AP
035800081027     c                   eval      WAPRP_09AP = inERP_09AP  + WAPRP_09AP
035900081027     c                   eval      WAPNS_10AP = inENS_10AP  + WAPNS_10AP
036000081027     c                   eval      WAPkg_10AP = inEkg_10AP  + WAPkg_10AP
036100081027     c                   eval      WAPRR_10AP = inERR_10AP  + WAPRR_10AP
036200081027     c                   eval      WAPRP_10AP = inERP_10AP  + WAPRP_10AP
036300081027     c                   eval      WAPNS_11AP = inENS_11AP  + WAPNS_11AP
036400081027     c                   eval      WAPkg_11AP = inEkg_11AP  + WAPkg_11AP
036500081027     c                   eval      WAPRR_11AP = inERR_11AP  + WAPRR_11AP
036600081027     c                   eval      WAPRP_11AP = inERP_11AP  + WAPRP_11AP
036700081027     c                   eval      WAPNS_12AP = inENS_12AP  + WAPNS_12AP
036800081027     c                   eval      WAPkg_12AP = inEkg_12AP  + WAPkg_12AP
036900081027     c                   eval      WAPRR_12AP = inERR_12AP  + WAPRR_12AP
037000081027     c                   eval      WAPRP_12AP = inERP_12AP  + WAPRP_12AP
037100081027     c                   eval      WAPNS_01AC = inENS_01AC  + WAPNS_01AC
037200081027     c                   eval      WAPkg_01AC = inEkg_01AC  + WAPkg_01AC
037300081027     c                   eval      WAPRR_01AC = inERR_01AC  + WAPRR_01AC
037400081027     c                   eval      WAPRP_01AC = inERP_01AC  + WAPRP_01AC
037500081027     c                   eval      WAPNS_02AC = inENS_02AC  + WAPNS_02AC
037600081027     c                   eval      WAPkg_02AC = inEkg_02AC  + WAPkg_02AC
037700081027     c                   eval      WAPRR_02AC = inERR_02AC  + WAPRR_02AC
037800081027     c                   eval      WAPRP_02AC = inERP_02AC  + WAPRP_02AC
037900081027     c                   eval      WAPNS_03AC = inENS_03AC  + WAPNS_03AC
038000081027     c                   eval      WAPkg_03AC = inEkg_03AC  + WAPkg_03AC
038100081027     c                   eval      WAPRR_03AC = inERR_03AC  + WAPRR_03AC
038200081027     c                   eval      WAPRP_03AC = inERP_03AC  + WAPRP_03AC
038300081027     c                   eval      WAPNS_04AC = inENS_04AC  + WAPNS_04AC
038400081027     c                   eval      WAPkg_04AC = inEkg_04AC  + WAPkg_04AC
038500081027     c                   eval      WAPRR_04AC = inERR_04AC  + WAPRR_04AC
038600081027     c                   eval      WAPRP_04AC = inERP_04AC  + WAPRP_04AC
038700081027     c                   eval      WAPNS_05AC = inENS_05AC  + WAPNS_05AC
038800081027     c                   eval      WAPkg_05AC = inEkg_05AC  + WAPkg_05AC
038900081027     c                   eval      WAPRR_05AC = inERR_05AC  + WAPRR_05AC
039000081027     c                   eval      WAPRP_05AC = inERP_05AC  + WAPRP_05AC
039100081027     c                   eval      WAPNS_06AC = inENS_06AC  + WAPNS_06AC
039200081027     c                   eval      WAPkg_06AC = inEkg_06AC  + WAPkg_06AC
039300081027     c                   eval      WAPRR_06AC = inERR_06AC  + WAPRR_06AC
039400081027     c                   eval      WAPRP_06AC = inERP_06AC  + WAPRP_06AC
039500081027     c                   eval      WAPNS_07AC = inENS_07AC  + WAPNS_07AC
039600081027     c                   eval      WAPkg_07AC = inEkg_07AC  + WAPkg_07AC
039700081027     c                   eval      WAPRR_07AC = inERR_07AC  + WAPRR_07AC
039800081027     c                   eval      WAPRP_07AC = inERP_07AC  + WAPRP_07AC
039900081027     c                   eval      WAPNS_08AC = inENS_08AC  + WAPNS_08AC
040000081027     c                   eval      WAPkg_08AC = inEkg_08AC  + WAPkg_08AC
040100081027     c                   eval      WAPRR_08AC = inERR_08AC  + WAPRR_08AC
040200081027     c                   eval      WAPRP_08AC = inERP_08AC  + WAPRP_08AC
040300081027     c                   eval      WAPNS_09AC = inENS_09AC  + WAPNS_09AC
040400081027     c                   eval      WAPkg_09AC = inEkg_09AC  + WAPkg_09AC
040500081027     c                   eval      WAPRR_09AC = inERR_09AC  + WAPRR_09AC
040600081027     c                   eval      WAPRP_09AC = inERP_09AC  + WAPRP_09AC
040700081027     c                   eval      WAPNS_10AC = inENS_10AC  + WAPNS_10AC
040800081027     c                   eval      WAPkg_10AC = inEkg_10AC  + WAPkg_10AC
040900081027     c                   eval      WAPRR_10AC = inERR_10AC  + WAPRR_10AC
041000081027     c                   eval      WAPRP_10AC = inERP_10AC  + WAPRP_10AC
041100081027     c                   eval      WAPNS_11AC = inENS_11AC  + WAPNS_11AC
041200081027     c                   eval      WAPkg_11AC = inEkg_11AC  + WAPkg_11AC
041300081027     c                   eval      WAPRR_11AC = inERR_11AC  + WAPRR_11AC
041400081027     c                   eval      WAPRP_11AC = inERP_11AC  + WAPRP_11AC
041500081027     c                   eval      WAPNS_12AC = inENS_12AC  + WAPNS_12AC
041600081027     c                   eval      WAPkg_12AC = inEkg_12AC  + WAPkg_12AC
041700081027     c                   eval      WAPRR_12AC = inERR_12AC  + WAPRR_12AC
041800081027     c                   eval      WAPRP_12AC = inERP_12AC  + WAPRP_12AC
041900080916      *
042000080916      *  Ricalcolo del delta
042100080916     c                   exsr      New_Delta
042200080916      *
042300081114      *    Deve controllare qual'è il mese in cui è stato considerato Nuovo
042400081114      *     fra Italia / Estero .
042500081119     c                   if        (inENEW_MES > WAPNEW_MES and wapNEW_MES = 0)
042600081119     c                              or
042700081119     c                             (inENEW_MES < WAPNEW_MES and inENEW_MES > 0)
042800081114     c                   eval      WAPNEW_MES = inENEW_MES
042900081114     c                   end
043000081114      *
043100081027     c                   update    wadelT
043200080916     c                   end
043300080916      *
043400080916     C                   ENDSR
043500080916     C* =========================================================================
043600080916      *   deve ricalcolare il Delta per la somma su Anno precedente e in Corso
043700080916     C* =========================================================================
043800080916     C     New_Delta     BEgSR
043900080916     C*
044000080916     C* DELTA 12 MESI
044100080916     C                   clear                   WRRW             17 5          *RICAVO REALE
044200080916     C                   clear                   WRPW             17 5          *RICAVO PRESUNTO
044300080916     C                   clear                   WDPW              4 1          *DELTA PERCENTUALE
044400080916     c                   eval      WRRW = + WAPRR_01AP
044500080916     c                                    + WAPRR_02AP
044600080916     c                                    + WAPRR_03AP
044700080916     c                                    + WAPRR_04AP
044800080916     c                                    + WAPRR_05AP
044900080916     c                                    + WAPRR_06AP
045000080916     c                                    + WAPRR_07AP
045100080916     c                                    + WAPRR_08AP
045200080916     c                                    + WAPRR_09AP
045300080916     c                                    + WAPRR_10AP
045400080916     c                                    + WAPRR_11AP
045500080916     c                                    + WAPRR_12AP
045600080916      *
045700080916     c                   eval      WRPW = + WAPRP_01AP
045800080916     c                                    + WAPRP_02AP
045900080916     c                                    + WAPRP_03AP
046000080916     c                                    + WAPRP_04AP
046100080916     c                                    + WAPRP_05AP
046200080916     c                                    + WAPRP_06AP
046300080916     c                                    + WAPRP_07AP
046400080916     c                                    + WAPRP_08AP
046500080916     c                                    + WAPRP_09AP
046600080916     c                                    + WAPRP_10AP
046700080916     c                                    + WAPRP_11AP
046800080916     c                                    + WAPRP_12AP
046900080916     C                   EXSR      CALDEL
047000080916     C                   Z-ADD     WDPW          WAPDAP                         *SCHIERA DELTA
047100080916      *
047200080916     C                   clear                   WRRW             17 5          *RICAVO REALE
047300080916     C                   clear                   WRPW             17 5          *RICAVO PRESUNTO
047400080916     C                   clear                   WDPW              4 1          *DELTA PERCENTUALE
047500080916     c                   eval      WRRW = + WAPRR_01Ac
047600080916     c                                    + WAPRR_02Ac
047700080916     c                                    + WAPRR_03Ac
047800080916     c                                    + WAPRR_04Ac
047900080916     c                                    + WAPRR_05Ac
048000080916     c                                    + WAPRR_06Ac
048100080916     c                                    + WAPRR_07Ac
048200080916     c                                    + WAPRR_08Ac
048300080916     c                                    + WAPRR_09Ac
048400080916     c                                    + WAPRR_10Ac
048500080916     c                                    + WAPRR_11Ac
048600080916     c                                    + WAPRR_12Ac
048700080916      *
048800080916     c                   eval      WRPW = + WAPRP_01Ac
048900080916     c                                    + WAPRP_02Ac
049000080916     c                                    + WAPRP_03Ac
049100080916     c                                    + WAPRP_04Ac
049200080916     c                                    + WAPRP_05Ac
049300080916     c                                    + WAPRP_06Ac
049400080916     c                                    + WAPRP_07Ac
049500080916     c                                    + WAPRP_08Ac
049600080916     c                                    + WAPRP_09Ac
049700080916     c                                    + WAPRP_10Ac
049800080916     c                                    + WAPRP_11Ac
049900080916     c                                    + WAPRP_12Ac
050000000000     C                   EXSR      CALDEL
050100080916     C                   Z-ADD     WDPW          WAPDAC                         *SCHIERA DELTA
050200000000     C*
050300080916     C                   ENDSR
050400000000     C*------------------------------------------------------------------------*
050500000000     C* CALDEL - CALCOLA IL DELTA
050600081027     C*          INPUT  --> WRRW: RICAVO REALE
050700000000     C*                     WRPW: RICAVO PRESUNTO
050800000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
050900000000     C*------------------------------------------------------------------------*
051000000000     C     CALDEL        BEGSR
051100000000     C*--------------------
051200000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
051300000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
051400000000     C*--------------------
051500000000     C*
051600000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
051700020116     C     WRRW          SUB       WRPW          GAP              17 5          (REALE - PRESUNTO)
051800000000     C     GAP           MULT      100           GAP                            (GAP x 100)
051900000000     C*
052000000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
052100000000IF  1C     WRRW          IFEQ      *ZEROS
052200000000     C                   Z-ADD     1             WRRW
052300000000E   1C                   ENDIF
052400000000     C*
052500000000     C     GAP           DIV       WRRW          GAP
052600000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
052700000000     C*
052800000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
052900000000     C     WDPW          COMP      0                                    28
053000000000     C* POSITIVO
053100000000IF  1C     *IN28         IFEQ      *OFF
053200000000IF  2C     GAP           IFGE      1000
053300000000     C                   Z-ADD     +999,9        WDPW
053400000000E   2C                   ENDIF
053500000000     C* NEGATIVO
053600000000X   1C                   ELSE
053700000000IF  2C     GAP           IFLE      -1000
053800000000     C                   Z-ADD     -999,9        WDPW
053900000000E   2C                   ENDIF
054000000000E   1C                   ENDIF
054100000000     C*
054200000000     C                   ENDSR
054300000000     C*------------------------------------------------------------------------*
054400081027     C*  ALLA FINE DEVE AGGIORNARE L'AREA DELL'AGENTE UNIFICIANTE SUI 3 ARCHIVI
054500081027     C*------------------------------------------------------------------------*
054600081027     C     AREA_AgenUnif BEGSR
054700081027     C*
054800081027     c                   open      WADELpcT
054900081027     C* ciclo completo di lettura
055000081027     C                   read      WADELpcT
055100081027     C*
055200081027     C* CICLO FINO A FINE FILE DI LAVORO
055300081027DO  1C                   DOW       not %EoF(WADELpcT)
055400081027     C*
055500081027     c                   movel     waPauC        CoDFiliale        3 0
055600081027     c                   exsr      Dec_Area
055700081027     C                   eval      waParAU  = CodAREa
055800081027     C                   eval      waParAUD = DesAREa
055900081027      *
056000081027      * Aggiorna l'Area dell'agente Unificante
056100081027     c                   update    wadelT
056200081027     C* FINE ANNO
056300081027     C                   read      WADELpcT
056400081027E   1C                   ENDDO
056500081027     C*
056600081027     c                   close     WADELpcT
056700081027     C*----------------
056800081027     c                   open      WADELpcF
056900081027     C* ciclo completo di lettura
057000081027     C                   read      WADELpcF
057100081027     C*
057200081027     C* CICLO FINO A FINE FILE DI LAVORO
057300081027DO  1C                   DOW       not %EoF(WADELpcF)
057400081027     C*
057500081027     c                   movel     inFauC        CoDFiliale        3 0
057600081027     c                   exsr      Dec_Area
057700081027     C                   eval      inFarAU  = CodAREa
057800081027     C                   eval      inFarAUD = DesAREa
057900081027      *
058000081027      * Aggiorna l'Area dell'agente Unificante
058100081027     c                   update    wadelF
058200081027     C* FINE ANNO
058300081027     C                   read      WADELpcF
058400081027E   1C                   ENDDO
058500081027     C*
058600081027     c                   close     WADELpcF
058700081027     C*
058800091209     C*----------------
058900091209     c                   open      WADELpcN
059000091209     C* ciclo completo di lettura
059100091209     C                   read      WADELpcN
059200091209     C*
059300091209     C* CICLO FINO A FINE FILE DI LAVORO
059400091209DO  1C                   DOW       not %EoF(WADELpcN)
059500091209     C*
059600091209     c                   movel     inNauC        CoDFiliale        3 0
059700091209     c                   exsr      Dec_Area
059800091209     C                   eval      inNarAU  = CodAREa
059900091209     C                   eval      inNarAUD = DesAREa
060000091209      *
060100091209      * Aggiorna l'Area dell'agente Unificante
060200091209     c                   update    wadelN
060300091209     C* FINE ANNO
060400091209     C                   read      WADELpcN
060500091209E   1C                   ENDDO
060600091209     C*
060700091209     c                   close     WADELpcN
060800091209     C*
060900081027     C                   ENDSR
061000081027     C*------------------------------------------------------------------------*
061100081027     C* CARTAB - CARICA I DATI IN TABELLE
061200081027     C*------------------------------------------------------------------------*
061300081027     C     CARTAB        BEGSR
061400081027     C*
061500081027     C* CHIAVE LETTURA AZORG01L - COMPLETA
061600081027     C     KEYORG        KLIST
061700081027     C                   KFLD                    KORFIL                         *FILIALE
061800081027     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
061900081027     C*
062000081027     C* CHIAVE LETTURA TABEL00F - COMPLETA
062100081027     C     KEYTAB        KLIST
062200081027     C                   KFLD                    KTBKUT                         *CODICE UTENTE
062300081027     C                   KFLD                    KTBCOD                         *CODICE TABELLA
062400081027     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
062500081027     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
062600081027     C     *LIKE         DEFINE    TBLCOD        KTBCOD
062700081027     C     *LIKE         DEFINE    TBLKEY        KTBKEY
062800081027     C*
062900081027     C                   Z-ADD     *ZEROS        F                 5 0          *PER ORGANIGRAMMA
063000081027     C*--------------------
063100081027     C* ORGANIGRAMMA
063200081027     C*--------------------
063300081027     C                   Z-ADD     *ZEROS        F
063400081027     C                   Z-ADD     *ZEROS        KORFIL
063500081027      *
063600081027     C     KEYORG        SETLL     AZORG01L
063700081027     C                   READ      AZORG01L
063800081027      *
063900081027DO  1C                   DoW       not %EoF(AZORG01L)
064000081027IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
064100081027IF  3C     ORGFAG        IFNE      'V'                                          *FILIALE/AGENZIA
064200081027     C                   ADD       1             F
064300081027     C                   Z-ADD     ORGFIL        FIL(F)
064400081027     C                   MOVEL     ORGDES        DFIL(F)
064500081027     C                   Z-ADD     1             KTBKUT
064600081027     C                   MOVEL     '05'          KTBCOD
064700081027     C                   MOVEL     *BLANKS       KTBKEY
064800081027     C                   MOVEL     ORGCAR        KTBKEY
064900081027     C     KEYTAB        CHAIN     TABEL00F
065000081027IF  5C                   If        %Found(TABEL00F)
065100081027     C                   Z-ADD     ORGCAR        ARE(F)
065200081027     C                   MOVEL     TBLUNI        DARE(F)
065300081027E   5C                   ENDIF
065400081027     C                   Z-ADD     1             KTBKUT
065500081027     C                   MOVEL     '17'          KTBCOD
065600081027     C                   MOVEL     *BLANKS       KTBKEY
065700081027     C                   MOVEL     ORGFL3        KTBKEY
065800081027     C     KEYTAB        CHAIN     TABEL00F
065900081027IF  5C                   If        %Found(TABEL00F)
066000081027     C                   MOVEL     ORGFL3        DIV(F)
066100081027     C                   MOVEL     TBLUNI        DDIV(F)
066200081027E   5C                   ENDIF
066300081027E   3C                   ENDIF
066400081027E   2C                   ENDIF
066500081027     C*
066600081027     C                   READ      AZORG01L
066700081027E   1C                   ENDDO
066800081027     C*--------------------
066900081027     C                   ENDSR
067000081027     C*------------------------------------------------------------------------*
067100081027     C*? DECARE - DECODIFICA CODICE AREA
067200081027     C*          INPUT  -- DEPARE: CODICE AREA
067300081027     C*          OUTPUT -- DESARE: DESCRIZIONE AREA
067400081027     C*------------------------------------------------------------------------*
067500081027     C     Dec_Area      BEGSR
067600081027     C*
067700081027     C                   Z-ADD     1             f
067800081027     C     CodFiliale    LOOKUP    FIL(f)                                 99
067900081027IF  1C     *IN99         IFEQ      *OFF
068000081027     C                   MOVEL     *zeros        CodAREa           3 0
068100081027     C                   MOVEL     *ALL'*'       DesAREa          25
068200081027X   1C                   ELSE
068300081027     C                   MOVEL     ARE(f)        CodAREa
068400081027     C                   MOVEL     DARE(f)       DesAREa
068500081027E   1C                   ENDIF
068600081027     C*
068700081027     C                   ENDSR
068800081027     C*------------------------------------------------------------------------*
