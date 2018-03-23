000100000000     H*------------------------------------------------------------------------*
000200100202     H*    Esegue la somma del WADEAPCE sul WADEAPCT (Italia+Estero)           *
000300080916     H*      Legge il File dell'Estero e lo somma su quello del totale.        *
000400100203     H*      x CLIENTE / TIPO SERVIZIO.                                        *
000500000000     H*------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700000000     F*------------------------------------------------------------------------*
000800000000     F* DATA BASE
000900000000     F*------------------------------------------------------------------------*
001000100204     FWFDEAV1L  uF   E           K DISK
001100100204     F*------------------------------------------------------------------------*
001200100202     FWADEAPCE  uF   E             DISK    prefix(INE:3) usropn
001300100202     F                                     rename(WADEaPC0:wadeaE)
001400080916      *
001500100202     FWADEAPCT  uF a E             DISK    rename(WADEaPC0:wadeaT) usropn
001600080916      *
001700100216     FWADEAPCI  uF   E             DISK    prefix(INI:3) usropn
001800100216     F                                     rename(WADEaPC0:wadeaI)
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
008800100202     D  PARWfdpv              88     88
008900100202      * Stat. Clienti Persi (Trimestrale x il momento)                          lancio trimestre(T)
009000100202     D  PARCLPtmp             89     89                                         M/B/T/Q/S
009100100202     D  PARStampa             90     90                                         *blk/N
009200100202     D  PARStaComm            91     91                                         *blk/N
009300080916      *
009400000000     C*------------------------------------------------------------------------*
009500000000     C* MAIN LINE
009600000000     C*------------------------------------------------------------------------*
009700080916     C     *ENTRY        PLIST
009800080916     C                   PARM                    KPJBA
009900080916      *
010000080916     C                   MOVEL     KPJBU         PARAM
010100081027     C*
010200081027     C*       carica tabelle Area e Divisione x decodifica
010300081027     c                   exsr      cartab
010400081027     C*
010500100202     c                   open      WADEapcE
010600100202     c                   open      WADEapcT
010700081027     C*
010800080916     C* ciclo completo di lettura dell'Estero da sommare
010900100202     C                   read      WADEapcE
011000000000     C*
011100000000     C* CICLO FINO A FINE FILE DI LAVORO
011200100202DO  1C                   DOW       not %EoF(WADEapcE)
011300000000     C*
011400080916     C* Aggancia il record di Totale se già presente x sommare l'Estero
011500080916     C*  altrimenti scrive il record
011600080916     c                   exsr      Somma_File
011700081027     C*
011800081027     c                   movel     inEauC        CoDFiliale        3 0
011900081027     c                   exsr      Dec_Area
012000081027     C                   eval      inEarAU  = CodAREa
012100081027     C                   eval      inEarAUD = DesAREa
012200081027      *
012300081027      * Aggiorna l'Area dell'agente Unificante
012400100202     c                   update    wadeaE
012500000000     C* FINE ANNO
012600100202     C                   read      WADEapcE
012700000000E   1C                   ENDDO
012800081027     C*
012900100202     c                   close     WADEapcE
013000100202     c                   close     WADEapcT
013100100204     C*
013200081027     C* Deve impostare l'area/descrizione area dell'agente unificante sui 3 files.
013300100216     c                   exsr      AREA_VARIE
013400081027      *
013500081027     C                   SETON                                        LR
013600020116     C* =========================================================================
013700080916     C*  Esegue il Dettaglio: va in somma al file Italia
013800080916     C* =========================================================================
013900080916     C     Somma_File    BEGSR
014000000000     C*
014100080916     C*  Pulisce il Nr.Rel.Record
014200080916     c                   reset                   rrn_PCT
014300080916     c*
014400080916      *?     Legge come una CHAIN...il fisico dei TOTALI     ?
014500100203      *?     x CLIENTE e TIPO SERVIZIO                       ?
014600100203      *?   inoltre aggiunge i campi delle VARIE "e" o "h"    ?
014700100203      *?   sempre a totale senza il NETWORK.                 ?
014800080916     C/EXEC SQL
014900100202     C+ SELECT rrn(WADEapcT) INTO :RRN_PCT FROM wadeaPCt WHERE WADCLI =
015000100203     C+ :inECLI  and WADTSP = :inETSP
015100080916     C/END-EXEC
015200080916     c                   if        SQLcod < 0  or SQLcod = 100
015300080916     C*
015400080916     C*  Non trovato il Cliente quindi scrive.
015500080916     c                   exsr      Write_rec
015600080916     C*
015700080916     c                   else
015800080916     C*
015900080916     C*  Trovato il Cliente quindi aggiunge.
016000080916     c                   exsr      Update_rec
016100080916     C*
016200080916     c                   end
016300080916     C*
016400080916     C                   ENDSR
016500080916     C* =========================================================================
016600080916     C*  Scrive il record poichè non c'era il cliente
016700080916     C* =========================================================================
016800080916     C     Write_rec     BEGSR
016900080916      *
017000100202     c                   eval      WAdAAD     = inEAAD
017100100202     c                   eval      WAdMMD     = inEMMD
017200100202     c                   eval      WAdAAA     = inEAAA
017300100202     c                   eval      WAdMMA     = inEMMA
017400100202     c                   eval      WAdIOE     = inEIOE
017500100202     c                   eval      WAdDIV     = inEDIV
017600100202     c                   eval      WAdDID     = inEDID
017700100202     c                   eval      WAdARE     = inEARE
017800100202     c                   eval      WAdARD     = inEARD
017900100202     c                   eval      WAdFIL     = inEFIL
018000100202     c                   eval      WAdFID     = inEFID
018100100202     c                   eval      WAdCLV     = inECLV
018200100202     c                   eval      WAdCLI     = inECLI
018300100202     c                   eval      WAdCLD     = inECLD
018400100202     c                   eval      WAdITC     = inEITC
018500100202     c                   eval      WAdCLN     = inECLN
018600100202     c                   eval      WAdAGE     = inEAGE
018700100202     c                   eval      WAdAGD     = inEAGD
018800100202     c                   eval      WAdSAP     = inESAP
018900100202     c                   eval      WAdkgP     = inEkgP
019000100202     c                   eval      WAdFAP     = inEFAP
019100100202     c                   eval      WAdPAP     = inEPAP
019200100202     c                   eval      WAdDAP     = inEDAP
019300100202     c                   eval      WAdSAC     = inESAC
019400100202     c                   eval      WAdkgC     = inEkgC
019500100202     c                   eval      WAdFAC     = inEFAC
019600100202     c                   eval      WAdPAC     = inEPAC
019700100202     c                   eval      WAdDAC     = inEDAC
019800100202     c                   eval      WAdBLO     = inEBLO
019900100202     c                   eval      WAdNS_01AP = inENS_01AP
020000100202     c                   eval      WAdkg_01AP = inEkg_01AP
020100100202     c                   eval      WAdRR_01AP = inERR_01AP
020200100202     c                   eval      WAdRP_01AP = inERP_01AP
020300100202     c                   eval      WAdNS_02AP = inENS_02AP
020400100202     c                   eval      WAdkg_02AP = inEkg_02AP
020500100202     c                   eval      WAdRR_02AP = inERR_02AP
020600100202     c                   eval      WAdRP_02AP = inERP_02AP
020700100202     c                   eval      WAdNS_03AP = inENS_03AP
020800100202     c                   eval      WAdkg_03AP = inEkg_03AP
020900100202     c                   eval      WAdRR_03AP = inERR_03AP
021000100202     c                   eval      WAdRP_03AP = inERP_03AP
021100100202     c                   eval      WAdNS_04AP = inENS_04AP
021200100202     c                   eval      WAdkg_04AP = inEkg_04AP
021300100202     c                   eval      WAdRR_04AP = inERR_04AP
021400100202     c                   eval      WAdRP_04AP = inERP_04AP
021500100202     c                   eval      WAdNS_05AP = inENS_05AP
021600100202     c                   eval      WAdkg_05AP = inEkg_05AP
021700100202     c                   eval      WAdRR_05AP = inERR_05AP
021800100202     c                   eval      WAdRP_05AP = inERP_05AP
021900100202     c                   eval      WAdNS_06AP = inENS_06AP
022000100202     c                   eval      WAdkg_06AP = inEkg_06AP
022100100202     c                   eval      WAdRR_06AP = inERR_06AP
022200100202     c                   eval      WAdRP_06AP = inERP_06AP
022300100202     c                   eval      WAdNS_07AP = inENS_07AP
022400100202     c                   eval      WAdkg_07AP = inEkg_07AP
022500100202     c                   eval      WAdRR_07AP = inERR_07AP
022600100202     c                   eval      WAdRP_07AP = inERP_07AP
022700100202     c                   eval      WAdNS_08AP = inENS_08AP
022800100202     c                   eval      WAdkg_08AP = inEkg_08AP
022900100202     c                   eval      WAdRR_08AP = inERR_08AP
023000100202     c                   eval      WAdRP_08AP = inERP_08AP
023100100202     c                   eval      WAdNS_09AP = inENS_09AP
023200100202     c                   eval      WAdkg_09AP = inEkg_09AP
023300100202     c                   eval      WAdRR_09AP = inERR_09AP
023400100202     c                   eval      WAdRP_09AP = inERP_09AP
023500100202     c                   eval      WAdNS_10AP = inENS_10AP
023600100202     c                   eval      WAdkg_10AP = inEkg_10AP
023700100202     c                   eval      WAdRR_10AP = inERR_10AP
023800100202     c                   eval      WAdRP_10AP = inERP_10AP
023900100202     c                   eval      WAdNS_11AP = inENS_11AP
024000100202     c                   eval      WAdkg_11AP = inEkg_11AP
024100100202     c                   eval      WAdRR_11AP = inERR_11AP
024200100202     c                   eval      WAdRP_11AP = inERP_11AP
024300100202     c                   eval      WAdNS_12AP = inENS_12AP
024400100202     c                   eval      WAdkg_12AP = inEkg_12AP
024500100202     c                   eval      WAdRR_12AP = inERR_12AP
024600100202     c                   eval      WAdRP_12AP = inERP_12AP
024700100202     c                   eval      WAdNS_01AC = inENS_01AC
024800100202     c                   eval      WAdkg_01AC = inEkg_01AC
024900100202     c                   eval      WAdRR_01AC = inERR_01AC
025000100202     c                   eval      WAdRP_01AC = inERP_01AC
025100100202     c                   eval      WAdNS_02AC = inENS_02AC
025200100202     c                   eval      WAdkg_02AC = inEkg_02AC
025300100202     c                   eval      WAdRR_02AC = inERR_02AC
025400100202     c                   eval      WAdRP_02AC = inERP_02AC
025500100202     c                   eval      WAdNS_03AC = inENS_03AC
025600100202     c                   eval      WAdkg_03AC = inEkg_03AC
025700100202     c                   eval      WAdRR_03AC = inERR_03AC
025800100202     c                   eval      WAdRP_03AC = inERP_03AC
025900100202     c                   eval      WAdNS_04AC = inENS_04AC
026000100202     c                   eval      WAdkg_04AC = inEkg_04AC
026100100202     c                   eval      WAdRR_04AC = inERR_04AC
026200100202     c                   eval      WAdRP_04AC = inERP_04AC
026300100202     c                   eval      WAdNS_05AC = inENS_05AC
026400100202     c                   eval      WAdkg_05AC = inEkg_05AC
026500100202     c                   eval      WAdRR_05AC = inERR_05AC
026600100202     c                   eval      WAdRP_05AC = inERP_05AC
026700100202     c                   eval      WAdNS_06AC = inENS_06AC
026800100202     c                   eval      WAdkg_06AC = inEkg_06AC
026900100202     c                   eval      WAdRR_06AC = inERR_06AC
027000100202     c                   eval      WAdRP_06AC = inERP_06AC
027100100202     c                   eval      WAdNS_07AC = inENS_07AC
027200100202     c                   eval      WAdkg_07AC = inEkg_07AC
027300100202     c                   eval      WAdRR_07AC = inERR_07AC
027400100202     c                   eval      WAdRP_07AC = inERP_07AC
027500100202     c                   eval      WAdNS_08AC = inENS_08AC
027600100202     c                   eval      WAdkg_08AC = inEkg_08AC
027700100202     c                   eval      WAdRR_08AC = inERR_08AC
027800100202     c                   eval      WAdRP_08AC = inERP_08AC
027900100202     c                   eval      WAdNS_09AC = inENS_09AC
028000100202     c                   eval      WAdkg_09AC = inEkg_09AC
028100100202     c                   eval      WAdRR_09AC = inERR_09AC
028200100202     c                   eval      WAdRP_09AC = inERP_09AC
028300100202     c                   eval      WAdNS_10AC = inENS_10AC
028400100202     c                   eval      WAdkg_10AC = inEkg_10AC
028500100202     c                   eval      WAdRR_10AC = inERR_10AC
028600100202     c                   eval      WAdRP_10AC = inERP_10AC
028700100202     c                   eval      WAdNS_11AC = inENS_11AC
028800100202     c                   eval      WAdkg_11AC = inEkg_11AC
028900100202     c                   eval      WAdRR_11AC = inERR_11AC
029000100202     c                   eval      WAdRP_11AC = inERP_11AC
029100100202     c                   eval      WAdNS_12AC = inENS_12AC
029200100202     c                   eval      WAdkg_12AC = inEkg_12AC
029300100202     c                   eval      WAdRR_12AC = inERR_12AC
029400100202     c                   eval      WAdRP_12AC = inERP_12AC
029500100202     c                   eval      WAdIND     = inEIND
029600100202     c                   eval      WAdCAP     = inECAP
029700100202     c                   eval      WAdLOC     = inELOC
029800100202     c                   eval      WAdPRV     = inEPRV
029900100202     c                   eval      WAdNAZ     = inENAZ
030000100202     c                   eval      WAdRESP    = inERESP
030100100202     c                   eval      WAdEMAR    = inEEMAR
030200100202     c                   eval      WAdCDF     = inECDF
030300100202     c                   eval      WAdIVA     = inEIVA
030400100202     c                   eval      WAdTEL     = inETEL
030500100202     c                   eval      WAdNEW_MES = inENEW_MES
030600100203     C*
030700081013      * ?Campi aggiunti: AGE Unificante e Descrizione
030800100202     c                   eval      WAdaud     = inEAUD                          *Descr.Unificante
030900100202     c                   eval      WAdauc     = inEAUC                          *Cod.  Unificante
031000080916      *
031100100202      * ?Campi aggiunti: Network e Tipo Servizio
031200100216     c                   eval      WADNTW     = '   '                           *Network (NON DEFINITO a TOTALE)
031300100202     c                   eval      WADTSP     = inETSP                          *Tipo Servizio
031400100202      *
031500100202     c                   write     wadeaT
031600080916      *
031700080916     C                   ENDSR
031800080916     C* =========================================================================
031900080916     C*  Aggiorna record con la somma x il cliente
032000080916     C* =========================================================================
032100080916     C     Update_rec    BEGSR
032200080916      *
032300100202     c     rrn_PCT       chain     wadeaPCT
032400080916      *
032500100202     c                   if        %Found(wadeaPCT)
032600080916      *
032700080916      * Blank in quanto somma "I" + "E"
032800100202     c                   eval      WAdIOE     = *blank
032900080916      *
033000100202     c                   eval      WAdSAP     = inESAP      + WAdSAP
033100100202     c                   eval      WAdkgP     = inEkgP      + WAdkgP
033200100202     c                   eval      WAdFAP     = inEFAP      + WAdFAP
033300100202     c                   eval      WAdPAP     = inEPAP      + WAdPAP
033400100202     c                   eval      WAdSAC     = inESAC      + WAdSAC
033500100202     c                   eval      WAdkgC     = inEkgC      + WAdkgC
033600100202     c                   eval      WAdFAC     = inEFAC      + WAdFAC
033700100202     c                   eval      WAdPAC     = inEPAC      + WAdPAC
033800080916      *
033900100202     c                   eval      WAdNS_01AP = inENS_01AP  + WAdNS_01AP
034000100202     c                   eval      WAdkg_01AP = inEkg_01AP  + WAdkg_01AP
034100100202     c                   eval      WAdRR_01AP = inERR_01AP  + WAdRR_01AP
034200100202     c                   eval      WAdRP_01AP = inERP_01AP  + WAdRP_01AP
034300100202     c                   eval      WAdNS_02AP = inENS_02AP  + WAdNS_02AP
034400100202     c                   eval      WAdkg_02AP = inEkg_02AP  + WAdkg_02AP
034500100202     c                   eval      WAdRR_02AP = inERR_02AP  + WAdRR_02AP
034600100202     c                   eval      WAdRP_02AP = inERP_02AP  + WAdRP_02AP
034700100202     c                   eval      WAdNS_03AP = inENS_03AP  + WAdNS_03AP
034800100202     c                   eval      WAdkg_03AP = inEkg_03AP  + WAdkg_03AP
034900100202     c                   eval      WAdRR_03AP = inERR_03AP  + WAdRR_03AP
035000100202     c                   eval      WAdRP_03AP = inERP_03AP  + WAdRP_03AP
035100100202     c                   eval      WAdNS_04AP = inENS_04AP  + WAdNS_04AP
035200100202     c                   eval      WAdkg_04AP = inEkg_04AP  + WAdkg_04AP
035300100202     c                   eval      WAdRR_04AP = inERR_04AP  + WAdRR_04AP
035400100202     c                   eval      WAdRP_04AP = inERP_04AP  + WAdRP_04AP
035500100202     c                   eval      WAdNS_05AP = inENS_05AP  + WAdNS_05AP
035600100202     c                   eval      WAdkg_05AP = inEkg_05AP  + WAdkg_05AP
035700100202     c                   eval      WAdRR_05AP = inERR_05AP  + WAdRR_05AP
035800100202     c                   eval      WAdRP_05AP = inERP_05AP  + WAdRP_05AP
035900100202     c                   eval      WAdNS_06AP = inENS_06AP  + WAdNS_06AP
036000100202     c                   eval      WAdkg_06AP = inEkg_06AP  + WAdkg_06AP
036100100202     c                   eval      WAdRR_06AP = inERR_06AP  + WAdRR_06AP
036200100202     c                   eval      WAdRP_06AP = inERP_06AP  + WAdRP_06AP
036300100202     c                   eval      WAdNS_07AP = inENS_07AP  + WAdNS_07AP
036400100202     c                   eval      WAdkg_07AP = inEkg_07AP  + WAdkg_07AP
036500100202     c                   eval      WAdRR_07AP = inERR_07AP  + WAdRR_07AP
036600100202     c                   eval      WAdRP_07AP = inERP_07AP  + WAdRP_07AP
036700100202     c                   eval      WAdNS_08AP = inENS_08AP  + WAdNS_08AP
036800100202     c                   eval      WAdkg_08AP = inEkg_08AP  + WAdkg_08AP
036900100202     c                   eval      WAdRR_08AP = inERR_08AP  + WAdRR_08AP
037000100202     c                   eval      WAdRP_08AP = inERP_08AP  + WAdRP_08AP
037100100202     c                   eval      WAdNS_09AP = inENS_09AP  + WAdNS_09AP
037200100202     c                   eval      WAdkg_09AP = inEkg_09AP  + WAdkg_09AP
037300100202     c                   eval      WAdRR_09AP = inERR_09AP  + WAdRR_09AP
037400100202     c                   eval      WAdRP_09AP = inERP_09AP  + WAdRP_09AP
037500100202     c                   eval      WAdNS_10AP = inENS_10AP  + WAdNS_10AP
037600100202     c                   eval      WAdkg_10AP = inEkg_10AP  + WAdkg_10AP
037700100202     c                   eval      WAdRR_10AP = inERR_10AP  + WAdRR_10AP
037800100202     c                   eval      WAdRP_10AP = inERP_10AP  + WAdRP_10AP
037900100202     c                   eval      WAdNS_11AP = inENS_11AP  + WAdNS_11AP
038000100202     c                   eval      WAdkg_11AP = inEkg_11AP  + WAdkg_11AP
038100100202     c                   eval      WAdRR_11AP = inERR_11AP  + WAdRR_11AP
038200100202     c                   eval      WAdRP_11AP = inERP_11AP  + WAdRP_11AP
038300100202     c                   eval      WAdNS_12AP = inENS_12AP  + WAdNS_12AP
038400100202     c                   eval      WAdkg_12AP = inEkg_12AP  + WAdkg_12AP
038500100202     c                   eval      WAdRR_12AP = inERR_12AP  + WAdRR_12AP
038600100202     c                   eval      WAdRP_12AP = inERP_12AP  + WAdRP_12AP
038700100202     c                   eval      WAdNS_01AC = inENS_01AC  + WAdNS_01AC
038800100202     c                   eval      WAdkg_01AC = inEkg_01AC  + WAdkg_01AC
038900100202     c                   eval      WAdRR_01AC = inERR_01AC  + WAdRR_01AC
039000100202     c                   eval      WAdRP_01AC = inERP_01AC  + WAdRP_01AC
039100100202     c                   eval      WAdNS_02AC = inENS_02AC  + WAdNS_02AC
039200100202     c                   eval      WAdkg_02AC = inEkg_02AC  + WAdkg_02AC
039300100202     c                   eval      WAdRR_02AC = inERR_02AC  + WAdRR_02AC
039400100202     c                   eval      WAdRP_02AC = inERP_02AC  + WAdRP_02AC
039500100202     c                   eval      WAdNS_03AC = inENS_03AC  + WAdNS_03AC
039600100202     c                   eval      WAdkg_03AC = inEkg_03AC  + WAdkg_03AC
039700100202     c                   eval      WAdRR_03AC = inERR_03AC  + WAdRR_03AC
039800100202     c                   eval      WAdRP_03AC = inERP_03AC  + WAdRP_03AC
039900100202     c                   eval      WAdNS_04AC = inENS_04AC  + WAdNS_04AC
040000100202     c                   eval      WAdkg_04AC = inEkg_04AC  + WAdkg_04AC
040100100202     c                   eval      WAdRR_04AC = inERR_04AC  + WAdRR_04AC
040200100202     c                   eval      WAdRP_04AC = inERP_04AC  + WAdRP_04AC
040300100202     c                   eval      WAdNS_05AC = inENS_05AC  + WAdNS_05AC
040400100202     c                   eval      WAdkg_05AC = inEkg_05AC  + WAdkg_05AC
040500100202     c                   eval      WAdRR_05AC = inERR_05AC  + WAdRR_05AC
040600100202     c                   eval      WAdRP_05AC = inERP_05AC  + WAdRP_05AC
040700100202     c                   eval      WAdNS_06AC = inENS_06AC  + WAdNS_06AC
040800100202     c                   eval      WAdkg_06AC = inEkg_06AC  + WAdkg_06AC
040900100202     c                   eval      WAdRR_06AC = inERR_06AC  + WAdRR_06AC
041000100202     c                   eval      WAdRP_06AC = inERP_06AC  + WAdRP_06AC
041100100202     c                   eval      WAdNS_07AC = inENS_07AC  + WAdNS_07AC
041200100202     c                   eval      WAdkg_07AC = inEkg_07AC  + WAdkg_07AC
041300100202     c                   eval      WAdRR_07AC = inERR_07AC  + WAdRR_07AC
041400100202     c                   eval      WAdRP_07AC = inERP_07AC  + WAdRP_07AC
041500100202     c                   eval      WAdNS_08AC = inENS_08AC  + WAdNS_08AC
041600100202     c                   eval      WAdkg_08AC = inEkg_08AC  + WAdkg_08AC
041700100202     c                   eval      WAdRR_08AC = inERR_08AC  + WAdRR_08AC
041800100202     c                   eval      WAdRP_08AC = inERP_08AC  + WAdRP_08AC
041900100202     c                   eval      WAdNS_09AC = inENS_09AC  + WAdNS_09AC
042000100202     c                   eval      WAdkg_09AC = inEkg_09AC  + WAdkg_09AC
042100100202     c                   eval      WAdRR_09AC = inERR_09AC  + WAdRR_09AC
042200100202     c                   eval      WAdRP_09AC = inERP_09AC  + WAdRP_09AC
042300100202     c                   eval      WAdNS_10AC = inENS_10AC  + WAdNS_10AC
042400100202     c                   eval      WAdkg_10AC = inEkg_10AC  + WAdkg_10AC
042500100202     c                   eval      WAdRR_10AC = inERR_10AC  + WAdRR_10AC
042600100202     c                   eval      WAdRP_10AC = inERP_10AC  + WAdRP_10AC
042700100202     c                   eval      WAdNS_11AC = inENS_11AC  + WAdNS_11AC
042800100202     c                   eval      WAdkg_11AC = inEkg_11AC  + WAdkg_11AC
042900100202     c                   eval      WAdRR_11AC = inERR_11AC  + WAdRR_11AC
043000100202     c                   eval      WAdRP_11AC = inERP_11AC  + WAdRP_11AC
043100100202     c                   eval      WAdNS_12AC = inENS_12AC  + WAdNS_12AC
043200100202     c                   eval      WAdkg_12AC = inEkg_12AC  + WAdkg_12AC
043300100202     c                   eval      WAdRR_12AC = inERR_12AC  + WAdRR_12AC
043400100202     c                   eval      WAdRP_12AC = inERP_12AC  + WAdRP_12AC
043500080916      *
043600080916      *  Ricalcolo del delta
043700080916     c                   exsr      New_Delta
043800080916      *
043900081114      *    Deve controllare qual'è il mese in cui è stato considerato Nuovo
044000081114      *     fra Italia / Estero .
044100100202     c                   if        (inENEW_MES > WAdNEW_MES and wadNEW_MES = 0)
044200081119     c                              or
044300100202     c                             (inENEW_MES < WAdNEW_MES and inENEW_MES > 0)
044400100202     c                   eval      WAdNEW_MES = inENEW_MES
044500081114     c                   end
044600081114      *
044700100216     c                   eval      WADNTW     = '   '                           *Network (NON DEFINITO a TOTALE)
044800100202     c                   update    wadeaT
044900080916     c                   end
045000100204      *
045100100204     C                   ENDSR
045200100204     C* =========================================================================
045300100204      *   deve ricalcolare il Delta per la somma su Anno precedente e in Corso
045400100204     C* =========================================================================
045500100204     C     New_Delta     BEgSR
045600100204     C*
045700080916     C* DELTA 12 MESI
045800080916     C                   clear                   WRRW             17 5          *RICAVO REALE
045900080916     C                   clear                   WRPW             17 5          *RICAVO PRESUNTO
046000080916     C                   clear                   WDPW              4 1          *DELTA PERCENTUALE
046100100202     c                   eval      WRRW = + WAdRR_01AP
046200100202     c                                    + WAdRR_02AP
046300100202     c                                    + WAdRR_03AP
046400100202     c                                    + WAdRR_04AP
046500100202     c                                    + WAdRR_05AP
046600100202     c                                    + WAdRR_06AP
046700100202     c                                    + WAdRR_07AP
046800100202     c                                    + WAdRR_08AP
046900100202     c                                    + WAdRR_09AP
047000100202     c                                    + WAdRR_10AP
047100100202     c                                    + WAdRR_11AP
047200100202     c                                    + WAdRR_12AP
047300080916      *
047400100202     c                   eval      WRPW = + WAdRP_01AP
047500100202     c                                    + WAdRP_02AP
047600100202     c                                    + WAdRP_03AP
047700100202     c                                    + WAdRP_04AP
047800100202     c                                    + WAdRP_05AP
047900100202     c                                    + WAdRP_06AP
048000100202     c                                    + WAdRP_07AP
048100100202     c                                    + WAdRP_08AP
048200100202     c                                    + WAdRP_09AP
048300100202     c                                    + WAdRP_10AP
048400100202     c                                    + WAdRP_11AP
048500100202     c                                    + WAdRP_12AP
048600080916     C                   EXSR      CALDEL
048700100202     C                   Z-ADD     WDPW          WAdDAP                         *SCHIERA DELTA
048800080916      *
048900080916     C                   clear                   WRRW             17 5          *RICAVO REALE
049000080916     C                   clear                   WRPW             17 5          *RICAVO PRESUNTO
049100080916     C                   clear                   WDPW              4 1          *DELTA PERCENTUALE
049200100202     c                   eval      WRRW = + WAdRR_01Ac
049300100202     c                                    + WAdRR_02Ac
049400100202     c                                    + WAdRR_03Ac
049500100202     c                                    + WAdRR_04Ac
049600100202     c                                    + WAdRR_05Ac
049700100202     c                                    + WAdRR_06Ac
049800100202     c                                    + WAdRR_07Ac
049900100202     c                                    + WAdRR_08Ac
050000100202     c                                    + WAdRR_09Ac
050100100202     c                                    + WAdRR_10Ac
050200100202     c                                    + WAdRR_11Ac
050300100202     c                                    + WAdRR_12Ac
050400080916      *
050500100202     c                   eval      WRPW = + WAdRP_01Ac
050600100202     c                                    + WAdRP_02Ac
050700100202     c                                    + WAdRP_03Ac
050800100202     c                                    + WAdRP_04Ac
050900100202     c                                    + WAdRP_05Ac
051000100202     c                                    + WAdRP_06Ac
051100100202     c                                    + WAdRP_07Ac
051200100202     c                                    + WAdRP_08Ac
051300100202     c                                    + WAdRP_09Ac
051400100202     c                                    + WAdRP_10Ac
051500100202     c                                    + WAdRP_11Ac
051600100202     c                                    + WAdRP_12Ac
051700000000     C                   EXSR      CALDEL
051800100202     C                   Z-ADD     WDPW          WAdDAC                         *SCHIERA DELTA
051900000000     C*
052000080916     C                   ENDSR
052100000000     C*------------------------------------------------------------------------*
052200000000     C* CALDEL - CALCOLA IL DELTA
052300081027     C*          INPUT  --> WRRW: RICAVO REALE
052400000000     C*                     WRPW: RICAVO PRESUNTO
052500000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
052600000000     C*------------------------------------------------------------------------*
052700000000     C     CALDEL        BEGSR
052800000000     C*--------------------
052900000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
053000000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
053100000000     C*--------------------
053200000000     C*
053300000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
053400020116     C     WRRW          SUB       WRPW          GAP              17 5          (REALE - PRESUNTO)
053500000000     C     GAP           MULT      100           GAP                            (GAP x 100)
053600000000     C*
053700000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
053800000000IF  1C     WRRW          IFEQ      *ZEROS
053900000000     C                   Z-ADD     1             WRRW
054000000000E   1C                   ENDIF
054100000000     C*
054200000000     C     GAP           DIV       WRRW          GAP
054300000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
054400000000     C*
054500000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
054600000000     C     WDPW          COMP      0                                    28
054700000000     C* POSITIVO
054800000000IF  1C     *IN28         IFEQ      *OFF
054900000000IF  2C     GAP           IFGE      1000
055000000000     C                   Z-ADD     +999,9        WDPW
055100000000E   2C                   ENDIF
055200000000     C* NEGATIVO
055300000000X   1C                   ELSE
055400000000IF  2C     GAP           IFLE      -1000
055500000000     C                   Z-ADD     -999,9        WDPW
055600000000E   2C                   ENDIF
055700000000E   1C                   ENDIF
055800000000     C*
055900000000     C                   ENDSR
056000000000     C*------------------------------------------------------------------------*
056100081027     C*  ALLA FINE DEVE AGGIORNARE L'AREA DELL'AGENTE UNIFICIANTE SUI 3 ARCHIVI
056200081027     C*------------------------------------------------------------------------*
056300100216     C     AREA_VARIE    BEGSR
056400081027     C*
056500100216     C*  Non deve farlo per il File dell'Estero poichè lo aveva già fatto nel
056600100216     C*  ciclo principale di questo programma.
056700100216     C*   Quindi lo deve fare per il file di Totale e per il file Italia
056800100216     C*   l'aggiornamento dell'Area dell'Agente unificante.
056900100216     C*
057000100216     C*  Solo per il file di totale deve aggiornare i campi relativi alle VARIE 'e'/'h'
057100100216     C*  corrispondenti ai servizi PRIORITI e H1030
057200100216     C*  Il file di TOTALE non è suddiviso x NETWORK quindi nel campo verranno messi '***'
057300100216     C*
057400100202     c                   open      WADEapcT
057500081027     C* ciclo completo di lettura
057600100202     C                   read      WADEapcT
057700081027     C*
057800081027     C* CICLO FINO A FINE FILE DI LAVORO
057900100202DO  1C                   DOW       not %EoF(WADEapcT)
058000081027     C*
058100100202     c                   movel     waDauC        CoDFiliale        3 0
058200081027     c                   exsr      Dec_Area
058300100202     C                   eval      waDarAU  = CodAREa
058400100202     C                   eval      waDarAUD = DesAREa
058500100216      *
058600100216      * Aggiorna il network e i valori dei tipi servizi
058700100216     c                   eval      WADNTW     = '***'                           *Network (NON DEFINITO a TOTALE)
058800100216     C* solo x
058900100216     C*     Priority o H10:30
059000100216      *   aggiunge le Varie pre-selezionate sul WFDEAV0F solo le (e) e le (h)
059100100216     c                   if        WADTSP = 'E' or WADTSP = 'H'
059200100216E   1C                   Exsr      read_WFDEAV
059300100216E   1C                   END
059400081027      *
059500081027      * Aggiorna l'Area dell'agente Unificante
059600100202     c                   update    wadeaT
059700081027     C* FINE ANNO
059800100202     C                   read      WADEapcT
059900081027E   1C                   ENDDO
060000081027     C*
060100100202     c                   close     WADEapcT
060200100216     C*
060300100216     C*
060400081027     C*----------------
060500100216     c                   open      WADEapcI
060600081027     C* ciclo completo di lettura
060700100216     C                   read      WADEapcI
060800081027     C*
060900081027     C* CICLO FINO A FINE FILE DI LAVORO
061000100216DO  1C                   DOW       not %EoF(WADEapcI)
061100081027     C*
061200100216     c                   movel     inIauC        CoDFiliale        3 0
061300081027     c                   exsr      Dec_Area
061400100216     C                   eval      inIarAU  = CodAREa
061500100216     C                   eval      inIarAUD = DesAREa
061600081027      *
061700081027      * Aggiorna l'Area dell'agente Unificante
061800100216     c                   update    wadeaI
061900081027     C* FINE ANNO
062000100216     C                   read      WADEapcI
062100081027E   1C                   ENDDO
062200081027     C*
062300100216     c                   close     WADEapcI
062400100216     C*
062500091209     C*----------------
062600081027     C                   ENDSR
062700100216     C* =========================================================================
062800100216      *   Legge le varie e imposta i campi da aggiornare.
062900100216     C* =========================================================================
063000100216     C     read_WFDEAV   BEgSR
063100100216      *
063200100216      * ?Cicla sul file delle varie se il tipo servizio è (E) o (H)
063300100216      * ? e man mano che legge cancella il record letto.
063400100216     c                   clear                   TSERV
063500100216      *
063600100216     c                   if        WADtsp ='E'
063700100216     C                   MOVE      'e'           TSERV
063800100216     c                   elseIF    WADtsp ='H'
063900100216     C                   MOVE      'h'           TSERV
064000100216     c                   end
064100100216      *
064200100216      * ? solo se i tipi servizi me lo consentono  vado in somma
064300100216     c                   if        TSERV ='e' or TSERV ='h'
064400100216     c     k_deaV1       setll     wfDEAv1l
064500100216     c     k_deaV1       reade     wfDEAv1l
064600100216     C*
064700100216     c                   DOW       not %EoF(wfDEAv1l)
064800100216     C*  Anno in corso
064900100216     C                   IF        WFVANN =  WAdAAD
065000100216     C*
065100100216     c                   select
065200100216     C*
065300100216     C                   when      WFVmes =  1
065400100217     c                   eval      WAdVA_01SC = wfvNSP + WAdVA_01SC
065500100217     c                   eval      WAdVA_01VC = wfvRCP + WAdVA_01VC
065600100216     C                   when      WFVmes =  2
065700100217     c                   eval      WAdVA_02SC = wfvNSP + WAdVA_02SC
065800100217     c                   eval      WAdVA_02VC = wfvRCP + WAdVA_02VC
065900100216     C                   when      WFVmes =  3
066000100217     c                   eval      WAdVA_03SC = wfvNSP + WAdVA_03SC
066100100217     c                   eval      WAdVA_03VC = wfvRCP + WAdVA_03VC
066200100216     C                   when      WFVmes =  4
066300100217     c                   eval      WAdVA_04SC = wfvNSP + WAdVA_04SC
066400100217     c                   eval      WAdVA_04VC = wfvRCP + WAdVA_04VC
066500100216     C                   when      WFVmes =  5
066600100217     c                   eval      WAdVA_05SC = wfvNSP + WAdVA_05SC
066700100217     c                   eval      WAdVA_05VC = wfvRCP + WAdVA_05VC
066800100216     C                   when      WFVmes =  6
066900100217     c                   eval      WAdVA_06SC = wfvNSP + WAdVA_06SC
067000100217     c                   eval      WAdVA_06VC = wfvRCP + WAdVA_06VC
067100100216     C                   when      WFVmes =  7
067200100217     c                   eval      WAdVA_07SC = wfvNSP + WAdVA_07SC
067300100217     c                   eval      WAdVA_07VC = wfvRCP + WAdVA_07VC
067400100216     C                   when      WFVmes =  8
067500100217     c                   eval      WAdVA_08SC = wfvNSP + WAdVA_08SC
067600100217     c                   eval      WAdVA_08VC = wfvRCP + WAdVA_08VC
067700100217     C                   when      WFVmes =  9
067800100217     c                   eval      WAdVA_09SC = wfvNSP + WAdVA_09SC
067900100217     c                   eval      WAdVA_09VC = wfvRCP + WAdVA_09VC
068000100216     C                   when      WFVmes = 10
068100100217     c                   eval      WAdVA_10SC = wfvNSP + WAdVA_10SC
068200100217     c                   eval      WAdVA_10VC = wfvRCP + WAdVA_10VC
068300100216     C                   when      WFVmes = 11
068400100217     c                   eval      WAdVA_11SC = wfvNSP + WAdVA_11SC
068500100217     c                   eval      WAdVA_11VC = wfvRCP + WAdVA_11VC
068600100216     C                   when      WFVmes = 12
068700100217     c                   eval      WAdVA_12SC = wfvNSP + WAdVA_12SC
068800100217     c                   eval      WAdVA_12VC = wfvRCP + WAdVA_12VC
068900100216     c                   endSL
069000100216     C*
069100100216     C                   eval      WADVAT_SC  = wfvNSP  + WADVAT_SC
069200100216     C                   eval      WADVAT_VC  = wfvRCP  + WADVAT_VC
069300100216     C*  Anno precedente
069400100216     C                   ELSE
069500100216     C*
069600100216     c                   select
069700100216     C*
069800100216     C                   when      WFVmes =  1
069900100217     c                   eval      WAdVA_01SP = wfvNSP + WAdVA_01SP
070000100217     c                   eval      WAdVA_01VP = wfvRCP + WAdVA_01VP
070100100216     C                   when      WFVmes =  2
070200100217     c                   eval      WAdVA_02SP = wfvNSP + WAdVA_02SP
070300100217     c                   eval      WAdVA_02VP = wfvRCP + WAdVA_02VP
070400100216     C                   when      WFVmes =  3
070500100217     c                   eval      WAdVA_03SP = wfvNSP + WAdVA_03SP
070600100217     c                   eval      WAdVA_03VP = wfvRCP + WAdVA_03VP
070700100216     C                   when      WFVmes =  4
070800100217     c                   eval      WAdVA_04SP = wfvNSP + WAdVA_04SP
070900100217     c                   eval      WAdVA_04VP = wfvRCP + WAdVA_04VP
071000100216     C                   when      WFVmes =  5
071100100217     c                   eval      WAdVA_05SP = wfvNSP + WAdVA_05SP
071200100217     c                   eval      WAdVA_05VP = wfvRCP + WAdVA_05VP
071300100217     C                   when      WFVmes =  6
071400100217     c                   eval      WAdVA_06SP = wfvNSP + WAdVA_06SP
071500100217     c                   eval      WAdVA_06VP = wfvRCP + WAdVA_06VP
071600100216     C                   when      WFVmes =  7
071700100217     c                   eval      WAdVA_07SP = wfvNSP + WAdVA_07SP
071800100217     c                   eval      WAdVA_07VP = wfvRCP + WAdVA_07VP
071900100216     C                   when      WFVmes =  8
072000100217     c                   eval      WAdVA_08SP = wfvNSP + WAdVA_08SP
072100100217     c                   eval      WAdVA_08VP = wfvRCP + WAdVA_08VP
072200100216     C                   when      WFVmes =  9
072300100217     c                   eval      WAdVA_09SP = wfvNSP + WAdVA_09SP
072400100217     c                   eval      WAdVA_09VP = wfvRCP + WAdVA_09VP
072500100216     C                   when      WFVmes = 10
072600100217     c                   eval      WAdVA_10SP = wfvNSP + WAdVA_10SP
072700100217     c                   eval      WAdVA_10VP = wfvRCP + WAdVA_10VP
072800100216     C                   when      WFVmes = 11
072900100217     c                   eval      WAdVA_11SP = wfvNSP + WAdVA_11SP
073000100217     c                   eval      WAdVA_11VP = wfvRCP + WAdVA_11VP
073100100216     C                   when      WFVmes = 12
073200100217     c                   eval      WAdVA_12SP = wfvNSP + WAdVA_12SP
073300100217     c                   eval      WAdVA_12VP = wfvRCP + WAdVA_12VP
073400100216     c                   endSL
073500100216     C*
073600100216     C                   eval      WADVAT_SP  = wfvNSP + WADVAT_SP
073700100216     C                   eval      WADVAT_VP  = wfvRCP + WADVAT_VP
073800100216     C*
073900100216     C                   END
074000100216     C*
074100100216     C*  cancella il record già incasellato in modo da non riutilizzarlo
074200100216     C*   più in seguito in sommatorie errate
074300100216     c                   delete    WFDEAV00
074400100216     C*
074500100216     c     k_deaV1       reade     wfDEAv1l
074600100216     C                   ENDDO
074700100216     C*
074800100216     C                   ENDif
074900100216     C*
075000100216     C                   ENDSR
075100081027     C*------------------------------------------------------------------------*
075200081027     C* CARTAB - CARICA I DATI IN TABELLE
075300081027     C*------------------------------------------------------------------------*
075400081027     C     CARTAB        BEGSR
075500081027     C*
075600081027     C* CHIAVE LETTURA AZORG01L - COMPLETA
075700081027     C     KEYORG        KLIST
075800081027     C                   KFLD                    KORFIL                         *FILIALE
075900081027     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
076000081027     C*
076100081027     C* CHIAVE LETTURA TABEL00F - COMPLETA
076200081027     C     KEYTAB        KLIST
076300081027     C                   KFLD                    KTBKUT                         *CODICE UTENTE
076400081027     C                   KFLD                    KTBCOD                         *CODICE TABELLA
076500081027     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
076600100204      ***
076700100204     c     k_deaV1       KLIST
076800100204     C                   KFLD                    TSERV             1
076900100204     C                   KFLD                    WAdCLI
077000100204      ***
077100081027     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
077200081027     C     *LIKE         DEFINE    TBLCOD        KTBCOD
077300081027     C     *LIKE         DEFINE    TBLKEY        KTBKEY
077400081027     C*
077500081027     C                   Z-ADD     *ZEROS        F                 5 0          *PER ORGANIGRAMMA
077600081027     C*--------------------
077700081027     C* ORGANIGRAMMA
077800081027     C*--------------------
077900081027     C                   Z-ADD     *ZEROS        F
078000081027     C                   Z-ADD     *ZEROS        KORFIL
078100081027      *
078200081027     C     KEYORG        SETLL     AZORG01L
078300081027     C                   READ      AZORG01L
078400081027      *
078500081027DO  1C                   DoW       not %EoF(AZORG01L)
078600081027IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
078700081027IF  3C     ORGFAG        IFNE      'V'                                          *FILIALE/AGENZIA
078800081027     C                   ADD       1             F
078900081027     C                   Z-ADD     ORGFIL        FIL(F)
079000081027     C                   MOVEL     ORGDES        DFIL(F)
079100081027     C                   Z-ADD     1             KTBKUT
079200081027     C                   MOVEL     '05'          KTBCOD
079300081027     C                   MOVEL     *BLANKS       KTBKEY
079400081027     C                   MOVEL     ORGCAR        KTBKEY
079500081027     C     KEYTAB        CHAIN     TABEL00F
079600081027IF  5C                   If        %Found(TABEL00F)
079700081027     C                   Z-ADD     ORGCAR        ARE(F)
079800081027     C                   MOVEL     TBLUNI        DARE(F)
079900081027E   5C                   ENDIF
080000081027     C                   Z-ADD     1             KTBKUT
080100081027     C                   MOVEL     '17'          KTBCOD
080200081027     C                   MOVEL     *BLANKS       KTBKEY
080300081027     C                   MOVEL     ORGFL3        KTBKEY
080400081027     C     KEYTAB        CHAIN     TABEL00F
080500081027IF  5C                   If        %Found(TABEL00F)
080600081027     C                   MOVEL     ORGFL3        DIV(F)
080700081027     C                   MOVEL     TBLUNI        DDIV(F)
080800081027E   5C                   ENDIF
080900081027E   3C                   ENDIF
081000081027E   2C                   ENDIF
081100081027     C*
081200081027     C                   READ      AZORG01L
081300081027E   1C                   ENDDO
081400081027     C*--------------------
081500081027     C                   ENDSR
081600081027     C*------------------------------------------------------------------------*
081700081027     C*? DECARE - DECODIFICA CODICE AREA
081800081027     C*          INPUT  -- DEPARE: CODICE AREA
081900081027     C*          OUTPUT -- DESARE: DESCRIZIONE AREA
082000081027     C*------------------------------------------------------------------------*
082100081027     C     Dec_Area      BEGSR
082200081027     C*
082300081027     C                   Z-ADD     1             f
082400081027     C     CodFiliale    LOOKUP    FIL(f)                                 99
082500081027IF  1C     *IN99         IFEQ      *OFF
082600081027     C                   MOVEL     *zeros        CodAREa           3 0
082700081027     C                   MOVEL     *ALL'*'       DesAREa          25
082800081027X   1C                   ELSE
082900081027     C                   MOVEL     ARE(f)        CodAREa
083000081027     C                   MOVEL     DARE(f)       DesAREa
083100081027E   1C                   ENDIF
083200081027     C*
083300081027     C                   ENDSR
083400081027     C*------------------------------------------------------------------------*
