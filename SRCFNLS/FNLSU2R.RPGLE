000100040603     H DECEDIT('0,') DATEDIT(*yMd.)
000200130524      * FNLSU2R *----------------------------------------------------*
000300970424      *                 - CHIUSURA FOGLIO VIAGGIO
000400900515      *--------------------------------------------------------------*
000500980210     FAZORG01L  IF   E           K DISK
000600980210     FTABEL00F  IF   E           K DISK
000700960123     FFNAGB01L  O  A E           K DISK    USROPN
000800950109     FFNFGV01L  UF   E           K DISK
000900000428     FFNFGW01L  IF   E           K DISK
001000060714     FFNFV401L  iF A E           K DISK
001100020916     FFNFVV02L  IF   E           K DISK    USROPN
001200020916     F                                     RENAME(FNFVV000:FNFVV002)
001300001120     FFNFVV01L  IF   E           K DISK    USROPN
001400151118     FFNANM01L  iF   E           K DISK    USROPN
001500151118     F                                     INFDS(FNANM1)
001600990517     FFNANM02L  UF A E           K DISK    USROPN
001700990517     F                                     RENAME(FNANM000:FNANM)
001800990517     F                                     INFDS(FNANM2)
001900021021     FFNANM20L  IF   E           K DISK    USROPN
002000021021     F                                     RENAME(FNANM000:FNANM20)
002100021021     F                                     INFDS(FNAN20)
002200960123     FFNFVD01L  UF A E           K DISK    USROPN
002300950109     FFNFGV46F  O  A E           K DISK
002400950109     F                                     RENAME(FNFGV000:FNFGV046)
002500960123     FFNCLS01L  IF   E           K DISK    USROPN
002600940414      *
002700950109      * SPUNTE
002800140618     FFNBRV05L  UF A E           K DISK    USROPN
002900140618     F                                     RENAME(FNBRV000:FNBRV05)
003000070123     FFNBRV02L  UF   E           K DISK
003100070123     F                                     RENAME(FNBRV000:FNBRV02)
003200070123     FFNBRV01L  IF   E           K DISK
003300070123     F                                     RENAME(FNBRV000:FNBRV01)
003400110907     FFNBRV07L  IF   E           K DISK    USROPN
003500070123     F                                     RENAME(FNBRV000:FNBRV07)
003600130527     FFNBRV09L  IF   E           K DISK    USROPN
003700130527     F                                     RENAME(FNBRV000:FNBRV09)
003800980206     F* SPUNTE: STORICO SPUNTE ERRATE
003900070123     FFNBRVE1L  IF A E           K DISK
004000070123     F                                     RENAME(FNBRV000:FNBRVE)
004100910905      *
004200930127      * BOLLE PARTENZA
004300130527     FFNBLP77L  UF   E           K DISK
004400960123     FFNBLP01L  UF   E           K DISK    USROPN
004500950109     F                                     RENAME(FNBLP000:FNBLP001)
004600060220     FFiar401L  IF   E           K DISK    USROPN
004700960123     FFNBLT01L  UF   E           K DISK    USROPN
004800960123     FFNBLT27L  UF   E           K DISK    USROPN
004900950109     F                                     RENAME(FNBLT000:FNBLT027)
005000021021     FFNBLT38L  IF   E           K DISK    USROPN
005100021021     F                                     RENAME(FNBLT000:FNBLT038)
005200010309     FFNORM01L  UF   E           K DISK    USROPN
005300070711     ffnorg01l  uf   e           k disk    usropn
005400010309     FFNORF00F  O  A E           K DISK    USROPN
005500051207     FFNORv00F  O  A E           K DISK    USROPN
005600040609     fFiar501l  uf   e           k Disk    Usropn
005700910910      *
005800910910      * FILE BOLLE DI TRANSITO
005900020605     FFNBTP11L  UF   E           K DISK    USROPN
006000020606     FFNBTT11L  IF   E           K DISK    USROPN
006100020606     FFNBTT37L  UF   E           K DISK    USROPN
006200020606     F                                     RENAME(FNBTT000:FNBTT037)
006300020605     FFNBTT38L  IF   E           K DISK    USROPN
006400020605     F                                     RENAME(FNBTT000:FNBTT038)
006500020412      * BOLLE ARRIVI
006600020412     FFNART27L  IF   E           K DISK
006700910905      *
006800910905      * BOLLE DI APPOGGIO
006900070423     FFNBLP46L  O  A E           K DISK    USROPN
007000070423     F                                     RENAME(FNBLP000:FNBLP046)
007100991022     F**
007200910924      * TRASMISSIONE ALL'ARRIVO
007300950109     FFNBLPE0T  O  A E           K DISK    USROPN
007400000111     FFISPP00T  O  A E           K DISK    USROPN
007500951116     FFNFVA01T  O  A E           K DISK    USROPN
007600950113     F                                     RENAME(FNFVA000:FNFVA0TT)
007700000428     FFNFWA00T  O  A E           K DISK    USROPN
007800910905      *
007900950109      * TRASMISSIONE AL TRANSITO
008000950109     FFNBLPT0T  O  A E           K DISK    USROPN
008100930127      *
008200931020      * TASSAZIONE PADRONCINI
008300011127     FTNTBE01L  IF   E           K DISK
008400011121     Ffiftt01L  UF A E           K DISK    USROPN
008500020326     Ffiftd04L  IF A E           K DISK    USROPN
008600020416     Ffiftd01L  IF   E           K DISK    USROPN
008700020416     F                                     RENAME(fiftd000:Fiftd001)
008800970930     FFNSTPR0F  O  A E             DISK    USROPN
008900060516      * distinta
009000120607     ffidst01l  if   e           k disk
009100150603      * clienti/filiali con sospensione bolle
009200150603     ffibsp01l  if   e           k disk
009300150603     ffibsp02l  if   e           k disk    rename(fibsp000:fibsp02)
009400150610     ffibsp03l  if   e           k disk    rename(fibsp000:fibsp03)
009500150703     FCNACO00F  IF   E           K DISK
009600170327     f* legami bolla
009700170327     ffnlbl01l  if   e           k disk
009800150703
009900150703     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
010000931020      *
010100020916
010200170619     d $MaxElem        c                   const(6500)
010300080109     D S5F             S              8    DIM($MaxElem)
010400950118      * PER FNFGV00F
010500000428     D LFV             S              3    DIM(301)
010600950118     D* PER FNFVA00F
010700950109     D*
010800911004     D LP2             S              3  0 DIM(99)
010900000428     D FEL             S              3  0 DIM(151)
011000000428     D FET             S              3  0 DIM(151)
011100160816     D PDR             S              7  0 DIM(9500)
011200160816     D ETM             S              1    DIM(9500)
011300160816     D ETP             S              1    DIM(9500)
011400160816     d Scm             s              1    Dim(9500)
011500160816     d Scp             s              1    Dim(9500)
011600130524     d* Flag se deve inviare o meno il fogio locale, posizione legata
011700050203     d*  a  FFV
011800050203     D WINVLOC         S              1    DIM(301)
011900950118     D* PER FNFVD00F
012000950118     D* CARICO I COD TRATTAMENTO MERCE CHE HANNO §1BFG5 = "N"
012100950118     D CTM             S              2    DIM(99)
012200950118     D* CARICO I COD PISTOLA CREATI IN AUTOMATICO
012300050524     D NPS             S              2  0 DIM(101)
012400970818     D* CARICO I COD PISTOLA FASULLI DA REIMPOSTARE
012500050524     D NPF             S              2  0 DIM(101)
012600060710     D* record di FV4 da trasmettere in sede
012700130226     D FV4S            S              1    DIM(50)
012800130226     d* tfp abilitati a far partire colli di altro tfp
012900130226     D xcotfp          S              3    DIM(85)
013000151127     d* tabella clienti da borderizare se passato codice
013100171107     d ds_ccmb         ds
013200171109     D ccmb                           7    DIM(301)
013300130226     d
013400931202     D* SCHIERE PER RICERCA CAPOCONTI
013500000321     D*
013600021014     D CMD1            S             48    DIM(1) CTDATA PERRCD(1)
013700021014     D CM19            S             48    DIM(1) CTDATA PERRCD(1)
013800021014     D CM32            S             48    DIM(1) CTDATA PERRCD(1)
013900021014     D CM37            S             48    DIM(1) CTDATA PERRCD(1)
014000021014     D CM38            S             48    DIM(1) CTDATA PERRCD(1)
014100021014     D CM41            S             42    DIM(1) CTDATA PERRCD(1)
014200021014     D CM47            S             42    DIM(1) CTDATA PERRCD(1)
014300021014     D CM48            S             42    DIM(1) CTDATA PERRCD(1)
014400021014     D CM49            S             42    DIM(1) CTDATA PERRCD(1)
014500021014     D CM51            S             42    DIM(1) CTDATA PERRCD(1)
014600920324     D ERR             S             66    DIM(4) CTDATA PERRCD(1)
014700151022     D MSG             S             70    DIM(6) CTDATA PERRCD(1)
014800000619     D SPT             S              3  0 DIM(100)
014900060714     D CMjob           S             42    DIM(1) CTDATA PERRCD(1)
015000950109     D***
015100950109     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
015200950109     D***
015300950109     D WLBDAT          DS                  INZ
015400950109     D  G02DAT                 1      8  0
015500950109     D  G02INV                 9     16  0
015600950109     D  G02ERR                17     17
015700950109     D  G02TGI                18     22  0
015800940527     D*
015900950110     D                 DS
016000950110     D  FVDTS1                 1      1
016100950110     D  FVDTS2                 2      2
016200950110     D  FVDTS3                 3      3
016300950110     D  FVDTS4                 4      4
016400950110     D  FVDTS5                 5      5
016500950110     D  FVDTS6                 6      6
016600950110     D  TSP                    1      6
016700950110     D                                     DIM(6)
016800950110     D                 DS
016900950110     D  FVDVL1                 1      9  4
017000950110     D  FVDVL2                10     18  4
017100950110     D  FVDVL3                19     27  4
017200950110     D  FVDVL4                28     36  4
017300950110     D  FVDVL5                37     45  4
017400950110     D  FVDVL6                46     54  4
017500950110     D  VOL                    1     54  4
017600950110     D                                     DIM(6)
017700150610
017800150610     d                 ds
017900150610     d bsplin
018000150610     d sbsp                           3  0 dim(30) overlay(bsplin)
018100150610
018200910911     D KPJBA         E DS
018300930127     D  LIBSYS                92    101
018400150603     D fnlsu3ds        DS
018500150603     D  ilsu3flg               1      1
018600150603     D  ilsu3aas               2      5
018700150603     D  ilsu3lnp               6      8
018800150603     D  ilsu3nrs               9     10
018900150603     D  ilsu3nsp              11     17
019000150603     D  ilsu3DSP_B            18     25
019100150603     D  ilsu3DSP_S            26     33
019200150603     D  ilsu3nos              34     34
019300150603     d
019400150603     d  ilsu3tcr              35     35
019500150603     d  ilsu3dcr              36     43
019600150603     d  ilsu3hcr              44     47
019700150603     D  olsu3err             100    101
019800150603     D  olsu3boagg           102    110
019900150603     D  olsu3bonagg          111    116
020000150703     D  olsu3msg             117    193
020100150703     D  olsu3sosp            194    194
020200170419     D  Ilsu3NOAGG           201    201
020300180110     D  Ilsu3Fcmt            202    202
020400180110     D  Ilsu3Flo             203    212
020500021010     D* DS PER TIBS55R - RICERCA S.I. IN CUI SI E'
020600021010     D DSBS55        E DS                  EXTNAME(TIBS55DS)
020700011127     d dgedcn        e ds
020800010309     D DSBL4M        E DS
020900040609     D Dar5gen       E DS
021000000619     D DANM01        E DS
021100000619     D  §ANZNC       E                     EXTFLD(§ANMZNC)
021200911018     D DS7C          E DS
021300940414     D DS7J          E DS
021400911018     D DS3A          E DS
021500910911     D DS1B          E DS
021600951120     D DSTV          E DS
021700010622     D DSFV4V        E DS
021800121119     D Ddgd          E DS                  INZ
021900060710     D DS3B          E DS
022000150703     D DS3I          E DS
022100150703     D og140         E DS
022200900515      *
022300950118     D* LINEE DI ARRIVO  PER FNFGV00F
022400910201     D                 DS
022500921202     D  FGVLNA                 1      3  0
022600921202     D  FGVFFV                 4    243
022700921202     D  FGVFF2               244    450
022800000428     D  FGWFF3               451    690
022900000428     D  FGWFF4               691    900
023000000428     D  VUOTO                901    903
023100000428     D  FFV                    1    903
023200000428     D                                     DIM(301)
023300900522      *
023400950118     D* FILIALI SCARICO  PER FNFVG00F
023500910201     D                 DS
023600921202     D  LNA2                   1      3  0
023700921202     D  FGVFLP                 4    243
023800921202     D  FGVFL2               244    450
023900000428     D  FGWFL3               451    690
024000000428     D  FGWFL4               691    900
024100000428     D  VUOTO2               901    903
024200000428     D  FLP                    1    903
024300000428     D                                     DIM(301)
024400950118      *
024500930127     D                 DS
024600000731     D  EMME                   1      1
024700021014     d  Num31                  2      4  0
024800021014     D  NUM3                   5      7  0
024900930127     D  FISMBR                 1     10
025000960503     D                 DS
025100960503     D  BLPAAS                 1      4  0
025200960503     D  BLPMGS                 5      8  0
025300960503     D  BLPDSP                 1      8  0
025400970307     D                 DS
025500970307     D  WRKAAS                 1      4  0
025600970307     D  WRKLNP                 5      7  0
025700970307     D  WRKNRS                 8      9  0
025800970307     D  WRKNSP                10     16  0
025900970307     D  WRKSPE                 1     16  0
026000970307     D  WRKTBO                17     17
026100970307     D  WRKKEY                 1     17
026200970423     D******
026300961007      * PASSAGGIO A CALCOLO VOLUMI CML              -FNLSB7R-
026400961007     D PARAM1          DS
026500010213     D  PA1NUM                17     17  0 INZ
026600961007     D  PA1FEL               158    160  0
026700970423     D******
026800970423      * NUMERO DI FOGLIO VIAGGIO PASSATO
026900130524     D FNLSU2DS        DS
027000940414     D* CATEGORIA FOGLIO I.M.P.
027100940414     D  NPG1                   1      1  0
027200940414     D* NUMERO    FOGLIO I.M.P.
027300940414     D  NFV1                   2      4P 0
027400940415     D* DATA      FOGLIO I.M.P.
027500950915     D  DFV1                   5      9P 0
027600950113     D*
027700960209     D  PA2AGS                10     10
027800940505     D  FGV2                  11     15  0
027900991022     D* PASSATO DA CLP FNLSA2C
028000040531     D* ESAZI                 16     16
028100991022     D  NUM2                  17     17  0
028200960126     D* PA2FLG = "M" ---> RICHIAMATO DALLA GESTIONE F.VIAGGIO
028300960126     D  PA2FLG                19     19
028400930127     D  L1                    78    137P 0
028500910909     D                                     DIM(30)
028600930127     D  RSUT                 138    157
028700930127     D  SIMFEL               158    160  0
028800950109     D* FILIALE GESTIONE FOGLIO I.M.P.
028900950109     D  FGS1                 164    166  0
029000170613     D* FILIALE ELABORATORE SIMFEL HA CML - non serve più
029100970423     D  PARCML               167    167
029200170613     d* AGP non più usato
029300010910     D  pa2agp               168    168
029400040121     D  PA2spv               169    169
029500040121     D  PA2spp               170    170
029600130527     D  PA2CCM               171    177
029700130530     D  PA2barcode           178    178
029800970423     D******
029900950109     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
030000931209     D PARAM3          DS
030100931209     D  PA3PDR                 1      7  0
030200931209     D  PA3TSR                 8      8
030300931209     D  PA3NDC                 9     15  0
030400931209     D  PA3FCT                16     16
030500931209     D  PA3FVD                17     17
030600931209     D  PA3FVT                18     18
030700950115     D  PA3DDC                19     26  0
030800041122     D  PA3sml               256    256
030900070423     D PARAMa3         DS
031000070423     D  PA3NFV                 1      5
031100070423     D* pa3tfp deve contenere il terminal di partenza per poter elaborare e
031200070423     d* trasmettere le bolle per terminal di partenza
031300070423     d* se non passato un valore > '000' il programma elabora e trasmette
031400070423     d* TUTTE le bolle presenti nel file fnblp46f (come faceva prima)
031500070423     D  PA3tfp                 6      8
031600111123     D  PA3num                 9      9    inz('0')
031700111123     d
031800920324     D* PER ESPLOSIONE DATI IN RICEZIONE
031900911001     D PARAM5          DS
032000950123     D  PARDTP                 1      8  0
032100950919     D  PARNFV                 9     13  0
032200960806     D  PARDEF                14     14
032300971106     D* FLAG PASSATO DAL FNLS93R PER DIRE SE FILE ALLOCATI
032400971106     D  WLCK                  15     15
032500000621     D  PARFEL               158    160  0
032600021015     D  LEGFIL               237    246    INZ('FXXX999999')
032700021015     d  MbrFil4              247    250    INZ('MXXX')
032800021015     d  MbrFil3              251    253    Inz('YYY')
032900021015     d  MbrFil               247    256
033000940415     D*
033100940415     D* PER PRENDERE FASE ANOMALIA                  -TRUL11R-
033200940415     D PARAM9          DS
033300950113     D  PA9TLA                 1      1    INZ('L')
033400950113     D  PA9NPG                 2      2    INZ('3')
033500950113     D  PA9SPG                 3      3    INZ('P')
033600940415     D  FASE                   4      4    INZ
033700040617
033800040617      * passaggio dati alla chiusura orm abbinati a bolle in sospensione
033900040617     d Param6          ds
034000040617     d  pa6fgs                        3  0
034100040617     d  pa6nfv                        5  0
034200940415     D*
034300931020     D                 DS
034400950113     D  BLPPDR                 1      7  0
034500950109     D  BLPDRT                 8     15  0
034600950113     D  BLPNRT                16     16  0
034700950113     D  BLPFPP                17     17
034800950113     D  COMBLP                 1     17
034900950115     D                 DS
035000950115     D  WCPR                   1      7  0 INZ
035100950115     D  WDRT                   8     15  0 INZ
035200950115     D  WNRT                  16     16  0 INZ
035300950115     D  WFPP                  17     17    INZ
035400950115     D  COMSAV                 1     17
035500940414     D                 DS
035600950109     D  BLTAAS                 1      4  0
035700950109     D  BLTLNP                 5      7  0
035800950109     D  BLTNRS                 8      9  0
035900950109     D  BLTNSP                10     16  0
036000970307     D  BLTSPE                 1     16  0
036100970307     D  BLTTBO                17     17
036200970307     D  BLTKEY                 1     17
036300950109     D                 DS
036400950109     D  BTTAAS                 1      4  0
036500950109     D  BTTLNP                 5      7  0
036600950109     D  BTTNRS                 8      9  0
036700950109     D  BTTNSP                10     16  0
036800970307     D  BTTSPE                 1     16  0
036900970307     D  BTTTBO                17     17
037000970307     D  BTTKEY                 1     17
037100931202     D*
037200970424     D* DS ESTERNA PER CHIUSURA ANOMALIE
037300960416     D DSLR33        E DS                  EXTNAME(FNLR33DS)
037400970424     D* DS PER REPERIMENTO DATI FOGLIO
037500970424     D DSLV53        E DS                  EXTNAME(FNLV53DS)
037600970516     D* DS PER DSLV55R - PASSAGGIO PARAMETRI
037700970516     D DSLV55        E DS                  EXTNAME(FNLV55DS)
037800031110     D* DS PER DSLV54R - PASSAGGIO PARAMETRI
037900031110     D FNLV54DS      E DS
038000070221     D* DS PER FNLS45R - PASSAGGIO PARAMETRI
038100070221     D FNLS45DS      E DS
038200130610     d
038300151118     D FNANM1          DS
038400151118     D  ANMNR1               397    400B 0
038500990517     D FNANM2          DS
038600960416     D  ANMNR2               397    400B 0
038700021021     D FNAN20          DS
038800960705     D  ANMNR0               397    400B 0
038900080109      *
039000080109     D DSE5F           DS            87    OCCURS($MaxElem)
039100960701     D  E5F                    1     87
039200960701     D                                     DIM(29)
039300070123     D DSBRV         E DS                  EXTNAME(FNBRV00F)
039400130527     D DSbrvw5       E DS                  EXTNAME(fnbrv00f)
039500130530     D                                     prefix(W5:3)
039600021016
039700070711     d dfar          e ds
039800021016     d Tibs02Ds      e ds
039900050524     d
040000050524     d* DS x controllo riempimento schiere
040100050518     d trul0sds      e ds
040200130530
040300130530     d Trul06ds      e ds
040400130530     d  L6                     1     90  0 dim(30)
040500170613      * - Parametri x Controllo profilo utenti
040600170613     d TIBS34ds      e ds                  inz
040700170613      * - Ds di riferimento al file esterno AZUTE00F
040800170613     d AZUTEds       e ds                  extname(AZUTE00F)
040900170613      * - Ds per dati organigramma
041000170613     d dDatiUte      e ds                  prefix(d_)
041100130530
041200050524     d
041300021016      * Ds date statistiche
041400021016     d dSdf          e ds
041500171107
041600171107     d dsuni           ds
041700171107     d  s_uni                         7    dim(36)
041800171107     d k_ke1           s                   like(tbeke1)
041900171107     d k_ke2           s                   like(tbeke2)
042000171107     D bh              S              4  0
042100171107     D by              S              4  0
042200030107
042300021014     D VAR             DS             8
042400021014     d  Varx1                  2      4
042500021014     D  VARX                   5      7
042600031112     D  N_varx                 5      7  0
042700021014     D VAR2            DS             9
042800021014     d  Var2x1                 2      4
042900021014     D  VAR2X                  5      7
043000030902      *
043100030902     d Qcmd            s            150    inz
043200150703     d Qcmd1           s            512    inz
043300030902     d Qlen            s             15  5 inz(150)
043400030902      *
043500040422     d wcrtspu         s              1
043600040802     d wfgsspu         s              3  0
043700130527     d IX              s              4  0
043800031110
043900130627     D WNFV60          S                   LIKE(BRVNFV)
044000130531     D kfgv            S                   LIKE(fgvnfv)
044100130530     D WNFV5           S                   LIKE(FVDNFV) inz
044200130530     D KNPG            S                   LIKE(BRVNPG) inz
044300130527     D WVVDFV          S                   LIKE(D53DFV) inz
044400031110     d s_o54tfp        s                   like(o54tfp)
044500031112     d s_o54tfa        s                   like(o54tfa)
044600050204     d FLPTFA          s                   like(o54tfa)
044700040218     d wtfa            s                   like(o54tfa)
044800040310     d ktrd            s                   like(ar5trd)
044900050203     d kfgs            s                   Like(FvvFgs)
045000050203     d xx              s              2  0
045100050203     d yy              s              3  0
045200171107     d*zz              s              3  0
045300130226     d Indx            s              3  0
045400150703     d Indy            s              3  0
045500150703     D Writerig        S              4  0 inz
045600130226     d SAVfnrs         s              5  0 inz
045700130226     d Wflsnrs         s              5  0
045800050203     d winvialocale    s              1
045900130226     d wTFP90          s              1
046000060516     d kdstfgs         s                   like(dstfgs)
046100060516     d kdstnfv         s                   like(dstnfv)
046200120607     d kdstnpg         s                   like(dstnpg)
046300060714     d
046400060714     d worap           s               z
046500060714     d worad           s               z
046600060714     d minuti          s              5  0
046700070219     D wtimez          S               z
046800070219     D dataiso         S               d   datfmt(*iso)
046900070219     D oraiso          S               t   timfmt(*iso)
047000150703     D data_eur        S               d   datfmt(*eur)
047100150603     D w_SiBsp         S              1
047200150703     D w_bsptip        S              1
047300150610     D w_FilBsp        S              1
047400151215     D w_FilBsps       S              1
047500150605     d sav_clibo       s                   like(blpccm)
047600150605     d w_clibo         s                   like(blpccm)
047700150605     d sav_lnp         s                   like(blplnp)
047800150703     d O_testo         s            132    inz
047900171124     d w1v             s              1
048000150703
048100150703      // - Comando di override al PrtF
048200150703     d C_CmdOvrPrtF    c                   const('OVRPRTF +
048300150703     d                                           file(PRTEMAIL) +
048400150703     d                                           pagesize(66 132) +
048500150703     d                                           lpi(6) cpi(10) +
048600150703     d                                           ovrscope(*actgrpdfn) +
048700150703     d                                           ')
048800150703     d C_CmdDltOvr     c                   const('DLTOVR +
048900150703     d                                            file(PRTEMAIL) +
049000150703     d                                            lvl(*actgrpdfn)')
049100150703      // - Tabella "MRA" = Bart-Mailing - Danni
049200150703     d dMRAdan       e ds                  inz
049300150703      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
049400150703     d TRTCM1ds      e ds                  inz
049500150703      //   ·§CM1mitt = Indirizzo e-mail del mittente
049600150703     d   §CM1mitt    e                     inz(' ed@brt.it')
049700150703      //   ·§CM1dst  = Indirizzo e-mail del destinatario
049800150703     d   §CM1dst     e                     inz('elisa.sanfelici@brt.it')
049900150703      //   ·§CM1tips = Tipo lettera via e-mail:
050000150703      //    "LET" = testo allegato in corpo con logo
050100150703      //            (richiede righe libere iniziali per il logo)
050200150703      //    "COI" = testo integrato senza logo senza BRT SPA iniziale
050300150703      //            (non consente né UNDERLINE né HIGHLIGHT)
050400150703     d   §CM1tips    e                     inz('COI')
050500150703      //   ·§CM1po   = Filiale
050600150703     d   §CM1po      e                     inz('046')
050700150703      //   ·§CM1var  = Oggetto e-mail
050800150703     d   §CM1var     e                     inz('*OBJM*+
050900150703     d                                     Manutenzione data spedizione+
051000150703     d                                      bolle ')
051100150703      //   ·§CM1sts  = Stato
051200150703     d   §CM1sts     e                     inz(*off)
051300150703      //   ·§CM1sts  = Id processo
051400150703     d   §CM1idp     e                     inz('2')
051500150703     D*
051600150703     D s_BSPl          S              3  0 DIM(599)
051700150703     D s_BSPlBO        S              7  0 DIM(599)
051800150703     D s_BSPlD         S              8  0 DIM(599)
051900150703     d
052000150703     D s_BSPk          S              7  0 DIM(799)
052100150703     D s_BSPkBO        S              7  0 DIM(799)
052200150703     D s_BSPkD         S              8  0 DIM(799)
052300150703
052400150703      // - Esecuzione comando di sistema
052500150703     d qCmdExc         pr                  extpgm('QCMDEXC')
052600150703     d  Qcmd1                       512    const  options(*varsize)
052700150703     d  Qlen                         15  5 const
052800070219
052900070219     D                SDS
053000070219     D  NOMPGM                 1     10
053100150703     d   JobUser             254    263
053200050203      *
053300900515      *
053400900601      *---------------------------------------------------------------*
053500900601      * INDICATORI USATI
053600930129      *---------------------------------------------------------------*
053700991026      * 01    = TRASMETTO
053800910911      * 04    = DEVO GENERARE AUTOSPUTA
053900931102      * 05    = LOKUP NELLA SCHIERA DEI PADRONCINI
054000910910      * 06    = LINEA DI ARRIVO CHE TRANSITA
054100920427      * 08    = INVIO ALLA LINEA ARRIVO CON SCARICO IL SUO TERMINAL
054200921202      * 09    = ESISTONO BOLLE SUL FOGLIO VIAGGIO DA INVIARE
054300920526      * 15    = CHAIN SU FV NON RIUSCITA: RISOTTOMETTO AZIONE
054400931027      * 16    = CHAIN IN TABEL PER CONTROLLARE SE IL TIPO BOLLA E' DA
054500931027      *         ESCLUDERE
054600920703      * 17    = LINEA ARRIVO E/O SCARICO >< DA TERMINAL: VEDO SE ECCEZI
054700950110      * 18    = CHAIN SU FNFVD000
054800900608      * 19/21 =DI COMODO
054900950109      * 22    = LETTURA FNBLP00F
055000991026      * 26    = LETTURA FNBLT00F
055100920324      * 28    = CI SONO ERRORI IN SCRITTURA FILE STORICO
055200970507      * 29/37 = DI COMODO
055300950915      * 40    = ELABORO LE BOLLE  DELL'AREA
055400950915      * 41    = ELABORO LE BOLLE  TRANSITO
055500940414      * 42    = ELABORO LE ANOMALIE DISGUIDO
055600950915      * 43    = ELABORO LE SPUNTE ENTRATA PER FILIALE CON CML
055700170613      * 44    =
055800950915      * 50    = MEMBRI FILE GIA' ALLOCATI: IMPOSSIBILE ESEGURE CHIUSURA
055900080228      * 90    = errore di scrittura file o lookup
056000080228      * 91    = LETTURA BOLLE DA LETTURA SPUNTA ENTRATA E SPUNTE PART.
056100960503      * 92    = ESISTE ALMENO UNA SPUNTA LOCALE
056200930127      * 93/99 = OPERAZIONI DA ESEGUIRSI NON LA PRIMA VOLTA
056300930708     C*---------------------------------------------------------------*
056400960202     C**
056500150703
056600150703     c                   clear                   s_bspk
056700150703     c                   clear                   s_bspkBO
056800150703     c                   clear                   s_bspkD
056900150703     c                   clear                   s_bspl
057000150703     c                   clear                   s_bsplBO
057100150703     c                   clear                   s_bsplD
057200150703
057300000619    1C     FGV2          IFNE      99999
057400020807     C     K02FGV01      CHAIN(N)  FNFGV01L                           19
057500020807     C     K02FGV01      CHAIN     FNFGW01L                           10
057600000428     C     *IN10         IFEQ      *ON
057700000428     C     FGWATB        ORNE      ' '
057800000428     C                   MOVEL     'N'           WFGW              1
057900000428     C                   CLEAR                   FGWFF3
058000000428     C                   CLEAR                   FGWFF4
058100000428     C                   CLEAR                   FGWFL3
058200000428     C                   CLEAR                   FGWFL4
058300000619     C                   CLEAR                   FGWFGS
058400000428     C                   ELSE
058500000428     C                   MOVEL     'S'           WFGW              1
058600000428     C                   ENDIF
058700000731     C                   Z-ADD     FGWFGS        WFGSFV
058800000731     C     WFGSFV        IFEQ      0
058900000731     C                   Z-ADD     FGVLNP        WFGSFV
059000040802     c*
059100040802     c                   eval      wfgsspu=wfgsfv
059200000619     C                   ENDIF
059300040802     c
059400130227     C                   MOVEL     'N'           WFVPT             1
059500000428     C*
059600960212     C* MEMORIZZO SE IL FV E' DA TRASMETTERE IN SEDE O MENO
059700960212     C                   MOVEL     'TV'          COD
059800960212     C                   MOVEL(P)  FGVTTR        KEY
059900960212     C     KTAB          CHAIN     TABEL                              36
060000960212    3C     *IN36         IFEQ      *OFF
060100960212     C     TBLFLG        ANDEQ     ' '
060200960212     C                   MOVEL     TBLUNI        DSTV
060300960212     C                   ELSE
060400960212     C                   MOVEL     'S'           §TVUTC
060500960806     C                   CLEAR                   §TVDEF
060600960212     C                   ENDIF
060700971106     C*
060800000619     C* PARDTP E' USATO NELLA PARAM5 E NELLA CREAZIONE SPOOL DI BACKUP
060900971106     C                   MOVEL     FGVDFV        PARDTP
061000000619     C                   MOVEL     §TVDEF        PARDEF
061100000619   X1C                   ELSE
061200130524     c* L'elaborazione dei fogli 99999 utilizzata ma utilizzata per trasmett
061300061108     c* ere le bolle di recupero di fine anno prima della fatturazione
061400061108     C                   MOVEL     'N'           WFVPT
061500000619     C                   Z-ADD     DATEU         PARDTP
061600070301     C                   Z-ADD     DATEU         fgvdfv
061700000619     C                   CLEAR                   PARDEF
061800000619    1C                   ENDIF
061900971106     C                   MOVEL     FGV2          PARNFV
062000000621     C                   MOVE      SIMFEL        PARFEL
062100060714     c* Prendo ora di apertura pgm
062200060714     C                   time                    worap
062300070827     c
062400130530     c* Carico codici autotrasportatori  + L6
062500070827     c                   EXSR      CarPAD
062600920526     C*
062700960123     C* R I C H I A M A T O   D A   G E S T I O N E   F.V I A G G I O
062800960123    1C     PA2FLG        IFEQ      'M'
062900960123     C*
063000130530     c
063100130530     C* ALLOCO MEMBRI DI TRASMISSIONE PER IL FOGLIO VIAGGIO
063200960123     C                   EXSR      ALLOCO
063300960202     C**
063400960123     C* ELABORO FOGLI VIAGGIO PARTENZE
063500021014     C                   MOVE      ')'           VAR               8
063600000725     C* MEMBRI DIVERSI A SECONDA CHE SIA FV POSTE OPPURE NO VISTO
063700000725     C* CHE ELABORAZIONI PER LE POSTE DIURANO SEMPRE MOLTO ABBIAMO DE-
063800000725     C* CISO DI FARLE GIRARE SU ALTRA CODA.
063900000725     C                   MOVEL     'M'           VAR
064000000731     C                   MOVEL     'M'           EMME
064100021014     C                   Z-ADD     48            LUNG             15 5
064200960202     C                   MOVEL     FGVLNA        LNA2
064300960202     C*
064400960202     C  N50              EXSR      FVIAGG
064500960123     C* CHIUSURA FINALE FILES
064600000428     C   89              CLOSE     FNFWA00T
064700000428     C   94              CLOSE     FNFVA01T
064800960123     C*
064900960123   X1C                   ELSE
065000960123     C* R I C H I A M A T O   D A   C H I U S U R A   F.V I A G G I O
065100960123     C*
065200000619     C* IGNORO SE STO CHIUDENDO FV 99999
065300000619   2AC     FGV2          IFNE      99999
065400960123     C*
065500921202     C*  19 ON - FOGLIO INESISTENTE
065600921202     C*
065700921202     C*          SE FOGLIO GIA' CHIUSO ESCO
065800921202     C  N19FGVFCF        IFNE      ' '
065900921202     C                   SETON                                        19
066000921202     C                   ELSE
066100921202     C                   MOVEL     FGVLNA        LNA2
066200921202     C                   END
066300920526     C**
066400940505     C   19              GOTO      FINE
066500950109     C* DATA CON CUI AGGIORNO LE BOLLE
066600950109     C                   Z-ADD     FGVDFV        WDTP
066700130527     c*
066800130527     c* Memorizzo anno e mese foglio
066900130705     c                   z-add     fgvdfv        w0040             4 0
067000130705     c                   movel     w0040         kmmm              2 0
067100130527     c                   movel     fgvdfv        kaaa4             4 0
067200900705     C*
067300920324     C* ALLOCO MEMBRI DI TRASMISSIONE PER IL FOGLIO VIAGGIO
067400130530    2C     FGVTTR        IFNE      'I'
067500130530     C     §TVDEF        ANDNE     'S'
067600130530     c                   seton                                        09
067700130530     c                   endif
067800130530     c
067900920324     C                   EXSR      ALLOCO
068000900911     C* ELABORAZIONE BOLLE
068100950915     C  N50              EXSR      ELAB
068200130530     c
068300000619  X2AC                   ELSE
068400000619     C* ELABORO  B O L L E    P A R T E N Z E  CON F.V. 99999
068500000619     C*  (SE CE N'E')
068600021014     C                   Z-ADD     48            LUNG
068700021014     C                   MOVE      ')'           VAR
068800021014     C                   MOVE      '))'          VAR2              9
068900000725     C                   MOVEL     'M'           VAR
069000000725     C                   MOVEL     'M'           VAR2
069100000731     C                   MOVEL     'M'           EMME
069200000621     C                   Z-ADD     1             WNPG
069300000619     C                   Z-ADD     99999         COMNFV            5 0
069400130527     C     K02BLP77_9    setll     FNBLP000                               22
069500000619     C   22              EXSR      TRAS99
069600000619     C*
069700000619     C* CHIUSURA FINALE FILE DELLE BOLLE
069800000619     C     *IN99         IFEQ      *ON
069900000619     C                   CLOSE     FNBLPE0T
070000000619     C                   ENDIF
070100000619   2AC                   ENDIF
070200920522     C*
070300960123    1C                   ENDIF
070400960202     C** AGGIORNO FOGLIO VIAGGIO
070500000619     C     FGV2          IFNE      99999
070600000619     C     *IN50         ANDEQ     *OFF
070700020807     C     K02FGV01      CHAIN     FNFGV01L                           1915
070800960202     C     *IN15         IFEQ      *OFF
070900960202     C     *IN19         ANDEQ     *OFF
071000960202     C                   MOVEL     'S'           FGVFCF
071100960202     C                   MOVEL     'T'           FGVFTR
071200960202     C                   MOVEL     DATEU         FGVDTR
071300960202     C                   UPDATE    FNFGV000
071400960202     C                   ENDIF
071500960202     C                   ENDIF
071600960123     C*
071700940217     C* LANCIO PROGRAMMI
071800930930     C                   MOVEL     *BLANKS       WLCK
071900971106     C                   MOVEL     PARAM5        KPJBU
072000930929     C*
072100930929     C* LANCIO PGM CHE PREPARA FILE DA INVIARE
072200950123     C                   CALL      'FNLS93R'
072300950123     C                   PARM                    KPJBA
072400971106     C                   MOVEL     KPJBU         PARAM5
072500930930     C*
072600930930     C* SE FILE ALLOCATI E NO TRASMISSIONE LI RISOTTOMETTO
072700931126     C     WLCK          IFNE      ' '
072800950109     C                   MOVEL     'LS93'        KCOAZ
072900931203     C                   CALL      'BCH10'
073000931203     C                   PARM                    KPJBA
073100931126     C                   ENDIF
073200931214     C*
073300021014     c                   Move      FgvLnp        MbrFil3
073400021015     c                   Movel     MbrFil4       LEGFIL
073500021015     c                   Movel     'F'           Legfil
073600931214     C                   MOVE      PARAM5        KPJBU
073700950919     C*
073800950919     C*
073900950919     C* CI SONO SOLO SPUNTE --> SOTTOMETTO SOLO IL PGM DELLE SPUNTE
074000960806     C*  SOLO SE NON E' UN FOGLIO DI DEFLUENZA
074100950919     C     *IN92         IFEQ      *ON
074200971106     C     *IN97         ANDEQ     *OFF
074300960806     C     §TVDEF        ANDNE     'S'
074400950919     C                   MOVEL     'LR15'        KCOAZ
074500950919     C                   CALL      'BCH10'
074600950919     C                   PARM                    KPJBA
074700950919     C                   ENDIF
074800040617      * sottometto la chiusura degli orm abbinati alle bolle in sospensione
074900040617      * se non è un foglio di defluenza
075000061108     c                   If        §tvdef <> 'S'  and fgv2<>99999
075100040617     c                   Clear                   Param6
075200040617     c                   Eval      pa6fgs = simfel
075300040617     c                   Eval      pa6nfv = fgv2
075400040617     c                   Eval      kpjbu = param6
075500040617     c                   Eval      kcoaz = 'OR19'
075600040617     c                   Call      'BCH10'
075700040617     c                   Parm                    kpjba
075800040617     c                   EndIf
075900150703     c* invio email per bolle in sospensione da oltre xx giorni
076000150703     c                   if        s_bspk(1)>0 or s_bspl(1)>0
076100150703     c                   exsr      INV_email
076200150703     c                   endif
076300950919     C*
076400130524     C                   MOVEL     FNLSU2DS      KPJBU
076500940415     C*
076600000000     C     FINE          TAG
076700960124     C                   MOVEL     'C'           SA2TLA
076800960124     C                   CALL      'FNLSA2R3'
076900960124     C                   PARM                    SA2TLA
077000960124     C                   PARM                    SA2TRC
077100960124     C                   PARM                    SA2DTS
077200970124     C                   PARM                    WWLNA             3 0
077300970424     C*
077400960124     C                   CALL      'FNLSA2R4'
077500960124     C                   PARM                    SA2TLA            1
077600960124     C                   PARM                    SA2LNP
077700960124     C                   PARM                    SA2NFV
077800960124     C                   PARM                    SA2RTP
077900970424     C*
078000960416     C                   MOVEL     'C'           D33TLA
078100960416     C                   CALL      'FNLR33R'
078200960416     C                   PARM                    DSLR33
078300970424     C*
078400970424     C                   CLEAR                   DSLV53
078500970424     C                   MOVEL     'C'           D53TLA
078600970424     C                   CALL      'FNLV53R'
078700970424     C                   PARM                    DSLV53
078800970509     C*
078900970509     C     WFC44         IFEQ      'S'
079000070221     C*                  MOVEL     'C'           FLG
079100070221     C                   MOVEL     'C'           dls45flg
079200970509     C*
079300070221     C                   CALL      'FNLS45R'
079400070221     c                   parm                    fnls45ds
079500070222     c                   parm                    dsbrv
079600970509     C                   ENDIF
079700970516     C*
079800970516     C                   MOVEL     'C'           D55TLA
079900970516     C                   CALL      'FNLV55R'
080000031113     C                   PARM                    DSLV55
080100031113     c*
080200031113     C                   MOVEL     'C'           I54TLA
080300031113     C                   CALL      'FNLV54R'
080400031113     C                   PARM                    fnlv54ds
080500900529      *
080600060714     c                   time                    worad
080700060714     c     worad         subdur    worap         minuti:*MN
080800060714     c
080900111019     c* Se lavoro dura più di 5 minuti creo joblog ma non invio messaggio
081000111019     c*  a sede. Nel caso ci siano delle cose strane si prova a cercare se c'e'
081100070827    3c                   if        minuti>5
081200060714     C                   Z-ADD     42            LUNG             15 5
081300060714     C                   MOVEA     CMjob(1)      COMMAN
081400060714     C                   CALL      'QCMDEXC'
081500060714     C                   PARM                    COMMAN
081600060714     C                   PARM                    LUNG
081700060714     c                   endif
081800060714     c
081900000000     C                   SETON                                        LR
082000960123     C*---------------------------------------------------------------
082100960123     C* APERTURA FILE
082200960123     C*---------------------------------------------------------------
082300960123     C     OPENF         BEGSR
082400960123     C                   OPEN      FNAGB01L
082500990517     C                   OPEN      FNANM02L
082600151118     C                   OPEN      FNANM01L
082700960123     C                   OPEN      FNFVD01L
082800960123     C                   OPEN      FNCLS01L
082900140618     C                   OPEN      FNBRV05L
083000110907     C                   OPEN      FNBRV07L
083100130527     C                   OPEN      FNBRV09L
083200960123     C                   OPEN      FNBLP01L
083300060220     C                   OPEN      Fiar401L
083400960123     C                   OPEN      FNBLT01L
083500960123     C                   OPEN      FNBLT27L
083600020605     C                   OPEN      FNBTP11L
083700020606     C                   OPEN      FNBTT11L
083800020606     C                   OPEN      FNBTT37L
083900070423     C                   OPEN      FNBLP46L
084000010309     C                   OPEN      FNORM01L
084100070711     c                   open      fnorg01l
084200010309     C                   OPEN      FNORF00F
084300051207     C                   OPEN      FNORV00F
084400030107     c                   Open      Fiar501l
084500960123     C                   ENDSR
084600900601      *---------------------------------------------------------------
084700900531      * ELABORO TRASFERIMENTO
084800900601      *---------------------------------------------------------------
084900900531     C     ELAB          BEGSR
085000021014     C                   Z-ADD     48            LUNG
085100021014     C                   MOVE      ')'           VAR
085200000725     C                   MOVEL     'M'           VAR
085300000731     C                   MOVEL     'M'           EMME
085400950206     C                   Z-ADD     1             WNPG
085500921202     C*
085600931001     C* 09 ON - CI SONO BOLLE DA CHIUDERE
085700910906     C   09              DO
085800931001     C*
085900931001     C* ELABORO  B O L L E    P A R T E N Z E
086000900601     C                   EXSR      TRASFE
086100931001     C*
086200931021     C* CHIUSURA FINALE FILE TASSAZIONE PADRONCINI
086300011121     C                   CLOSE     fiftt01L
086400011121     C                   CLOSE     fiftd04L
086500020416     C                   CLOSE     fiftd01L
086600970930     C                   CLOSE     FNSTPR0F
086700931021     C*
086800931001     C* ELABORO  B O L L E    P A R T E N Z E  CON F.V. 99999
086900931001     C*  (SE CE N'E')
087000130531     C                   Z-ADD     99999         comNFV            5 0
087100130527     C     K02BLP77_9    setll     FNBLP000                               22
087200950323     C   22              EXSR      TRAS99
087300931001     C*
087400931001     C* CHIUSURA FINALE FILE DELLE BOLLE
087500931001     C     *IN99         IFEQ      *ON
087600950109     C                   CLOSE     FNBLPE0T
087700931001     C                   ENDIF
087800910906     C                   END
087900130530     c
088000130530     C* NON FACCIO L'AUTOGENERAZIONE SPUNTE SE :
088100130530     C*  - FV CON TIPO TRAINO INOLTRO
088200130530     C*  - FV DI DEFLUENZA
088300130530    2C     FGVTTR        IFNE      'I'
088400130530     C     §TVDEF        ANDNE     'S'
088500130530     C*
088600130530     C* NFV1 > 0   ====>  C H I U S U R A    D A    I. M. P.
088700130530    3C     NFV1          IFGT      0
088800130530     C                   EXSR      ABBIN
088900130530     C*
089000130530     C     K02FGV01      CHAIN(N)  FNFGV01L                           19
089100130530   X3C                   ELSE
089200130530     C* SE CHIUSURA NON DA IMP VERIFICO COMUNQUE PER LE SPUNTE PARTENZA
089300130530     C*  ESISTENTI IL PESO E/O VOLUME
089400130530     C                   EXSR      SPUPAR
089500130530    3C                   ENDIF
089600130530     C*
089700130530    2C                   ENDIF
089800931001     C*
089900931001     C* ELABORO  S P U N T E    P A R T E N Z E
090000910909     C                   EXSR      SPUNTE
090100931001     C*
090200931021     C* ELABORO  F O G L I    V I A G G I O    P A R T E N Z E
090300911004     C                   EXSR      FVIAGG
090400931001     C*
090500931001     C* CHIUSURA FINALE FILE
090600931001     C     *IN98         IFEQ      *ON
090700950109     C                   CLOSE     FNBLPT0T
090800931001     C                   ENDIF
090900931001     C     *IN96         IFEQ      *ON
091000000111     C                   CLOSE     FISPP00T
091100931001     C                   ENDIF
091200931001     C     *IN94         IFEQ      *ON
091300951116     C                   CLOSE     FNFVA01T
091400931001     C                   ENDIF
091500000428     C     *IN89         IFEQ      *ON
091600000428     C                   CLOSE     FNFWA00T
091700000428     C                   ENDIF
091800911001     C**
091900900531     C                   ENDSR
092000900601     C**-----------------------------------------------*
092100900601     C** ESEGUE CALL QCAEXEC SUL MEMBRO DELLE BOLLE    *
092200900601     C**-----------------------------------------------*
092300900601     C     CAEX          BEGSR
092400900601     C** FILIALE DI COMPETENZA PER APERTURA MEMBRO
092500910905     C** CHIUSURA FILE SOLO DA SECONDO GIRO IN POI
092600931001     C     *IN99         IFEQ      *ON
092700950109     C                   CLOSE     FNBLPE0T
092800931001     C                   ELSE
092900931001     C                   SETON                                        99
093000931001     C                   ENDIF
093100910905     C** OVRDBF
093200950109     C                   MOVE      VAR           CMD1
093300910926     C                   MOVEL     *BLANKS       COMMAN           80
093400900601     C                   MOVEA     CMD1(1)       COMMAN
093500900601     C                   CALL      'QCMDEXC'
093600900601     C                   PARM                    COMMAN
093700900601     C                   PARM                    LUNG
093800910905     C*
093900910905     C* APERTURA FILE
094000950109     C                   OPEN      FNBLPE0T
094100900601     C                   ENDSR
094200910905     C**-----------------------------------------------*
094300910905     C** ESEGUE CALL QCAEXEC SUL MEMBRO DELLE BOLLE TRANSITO
094400910905     C**-----------------------------------------------*
094500910905     C     CAEX08        BEGSR
094600910905     C** FILIALE DI COMPETENZA PER APERTURA MEMBRO
094700910905     C** CHIUSURA FILE SOLO DA SECONDO GIRO IN POI
094800950109     C   98              CLOSE     FNBLPT0T
094900910905     C  N98              SETON                                        98
095000910905     C** OVRDBF
095100950109     C                   MOVE      VAR           CM19
095200910905     C                   MOVEL     *BLANKS       COMMAN
095300910905     C                   MOVEA     CM19(1)       COMMAN
095400910905     C                   CALL      'QCMDEXC'
095500910905     C                   PARM                    COMMAN
095600910905     C                   PARM                    LUNG
095700910905     C*
095800910905     C* APERTURA FILE
095900950109     C                   OPEN      FNBLPT0T
096000910905     C                   ENDSR
096100910909     C**-----------------------------------------------*
096200910909     C** ESEGUE CALL QCAEXEC SUL MEMBRO SPEDIZIONE BOLLE
096300910909     C**-----------------------------------------------*
096400910909     C     CAEXSP        BEGSR
096500910909     C* CHIUDO MEMBRO
096600000111     C   96              CLOSE     FISPP00T
096700920608     C  N96              SETON                                        96
096800910909     C*
096900910909     C** OVRDBF
097000000111     C                   MOVE      VAR           CM32
097100910909     C                   MOVEL     *BLANKS       COMMAN
097200920608     C                   MOVEA     CM32(1)       COMMAN
097300910909     C                   CALL      'QCMDEXC'
097400910909     C                   PARM                    COMMAN
097500910909     C                   PARM                    LUNG
097600910909     C*
097700910909     C* APERTURA FILE
097800000111     C                   OPEN      FISPP00T
097900910909     C*
098000910909     C                   ENDSR
098100910910     C**-----------------------------------------------*
098200910910     C** ESEGUE CALL QCAEXEC SUL MEMBRO FOGLI VIAGGIO
098300910910     C**-----------------------------------------------*
098400910910     C     CAEXFV        BEGSR
098500910910     C*
098600960123     C** F N F V A 0 1 T
098700951116     C   94              CLOSE     FNFVA01T
098800921202     C  N94              SETON                                        94
098900910910     C*
099000910910     C** OVRDBF
099100951116     C                   MOVE      VAR           CM38
099200921202     C                   MOVEL     *BLANKS       COMMAN
099300921202     C                   MOVEA     CM38(1)       COMMAN
099400910910     C                   CALL      'QCMDEXC'
099500910910     C                   PARM                    COMMAN
099600910910     C                   PARM                    LUNG
099700910910     C*
099800910910     C* APERTURA FILE
099900951116     C                   OPEN      FNFVA01T
100000000428     C**
100100000428     C** F N F W A 0 0 T
100200000428     C   89              CLOSE     FNFWA00T
100300000428     C  N89              SETON                                        89
100400000428     C*
100500000428     C** OVRDBF
100600000428     C                   MOVE      VAR           CM37
100700000428     C                   MOVEL     *BLANKS       COMMAN
100800000428     C                   MOVEA     CM37(1)       COMMAN
100900000428     C                   CALL      'QCMDEXC'
101000000428     C                   PARM                    COMMAN
101100000428     C                   PARM                    LUNG
101200000428     C*
101300000428     C* APERTURA FILE
101400000428     C                   OPEN      FNFWA00T
101500060707     c
101600910910     C                   ENDSR
101700900601     C**-----------------------------------------------*
101800900601     C** TRASFERISCO BOLLE NELLO STORICO E IN FILE TRASMISSIONE
101900900601     C**-----------------------------------------------*
102000000320     C     TRASFE        BEGSR
102100950323     C                   CLEAR                   SAVCBO
102200130530     C                   CLEAR                   wBORtran
102300130527     C                   CLEAR                   wFLP              3 0
102400910906     C                   Z-ADD     0             SAVLNA            3 0
102500150605     c                   clear                   sav_clibo
102600150605     c                   clear                   sav_lnp
102700150608     c                   clear                   w_SiBsp
102800950109     C                   Z-ADD     1             WNPG
102900130531     C                   Z-ADD     1             I                 3 0
103000130531     c                   clear                   kfgv
103100931027     C*
103200011121     C                   OPEN      fiftt01L
103300011121     C                   OPEN      fiftd04L
103400020416     C                   OPEN      fiftd01L
103500970930     C                   OPEN      FNSTPR0F
103600931021     C*
103700130531     C* lettura bolle per linea di arrivo con numero foglio = zero
103800130531    0C     FFV(I)        DOWGT     *ZEROS
103900130531     C                   MOVEL     FFV(I)        SAVFFV
104000130531     C*
104100130531     C     KBLP77        setll     FNBLP000
104200130531     C     KBLP77        reade     FNBLP000                               22
104300910905     C*
104400130531    1C     *IN22         DOWEQ     '0'
104500950116     C* ELABORO DOLO LE BOLLE NON ANCORA TRASMESSE
104600950116     C     BLPDT2        IFEQ      0
104700950115     C* CONTROLLO IL TIPO BOLLA
104800950115     C                   EXSR      CTABEL
104900910909     C**
105000910909     C** L I N E A   A R R I V O   C A M B I A T A
105100130531    2C     SAVLNA        IFNE      BLPLNA
105200130702     C                   SETOFF                                       01
105300920505     C*
105400130530     c* In caso di parzializz. cod cliente devo borderizare solo
105500130530     c*  linea di arrivo = SIMFEL + L6
105600151126     c                   z-add     1             Indx              3 0
105700151126     c**                 if        pa2ccm>*zeros
105800151126     c**                 eval      Indx=%lookup(blplna:l6)
105900151126     c**                 if        indx=0
106000151126     C**   Kblp77        setgt     FNBLP000
106100151126     c**                 endif
106200151126     c**                 endif
106300130530     c
106400130530     C                   CLEAR                   dslv55
106500140403     c                   z-add     blptfp        d55lnp
106600130527     C                   MOVEL     BLPLNA        d55LIN
106700130527     C                   MOVEL     FGVDFV        d55DRF
106800130527     C                   CALL      'FNLV55R'
106900130530     C                   PARM                    dslv55
107000130527     c
107100130527    1C     d55err        IFEQ      *BLANKS
107200130527     c                   z-add     d55tfp        s_o54tfp
107300130527     c                   z-add     d55tfa        s_o54tfa
107400040630     c* se lna è un secondo livello in arrivo potrebbe succedere che il
107500040630     c* p.o. di scarico sia = alla lna (f.v. diretto ad un secondo liv.)
107600040630     c* (In questo caso devo inviare la bolla al ter-part della lna.
107700040630     c* Infatti anche in fviagg e spunte i dati vengono trasmessi al ter-par
107800040630     c* del p.o. di scarico (in qesto caso p.o. di scarico=lna)
107900040630     c* Se non si facesse in questo modo le bolle andrebbero via in un membr
108000040630     c* diverso rispetto alle spunte e al f.viaggio e in più il pgm fnls93
108100040630     c* non trasmetterebbe il membro delle bolle giusto)
108200130527    2c     blplna        ifne      d55tfa
108300040630     c                   clear                   wlnadir           1
108400040630     C                   Z-ADD     1             B                 3 0
108500040630     C                   MOVEL     blplna        COMPRO            3
108600040630     C     COMPRO        LOOKUP    FFV(B)                                 20
108700040630     C* NON TROVATA --> METTO LINEA ARRIVO STESSA
108800070913     c                   if        (*in20 and flp(B)=compro) or not *in20
108900040630     c                   movel     'D'           wlnadir
109000040630     c                   endif
109100130527    2c                   endif
109200040630     c*
109300130527     c* se secondo livello in arrivo
109400031219     c* invio la bolla al terpar del ter-arr della lna
109500130527    2c     blplna        ifne      d55tfa
109600040630     c     wlnadir       andeq     *blanks
109700130530     c                   clear                   dslv55
109800130527     C                   MOVEL     'P'           d55TPT
109900130527     C                   MOVEL     s_o54tfa      d55LIN
110000130527     C                   MOVEL     FGVDFV        d55DRF
110100130527     C                   CALL      'FNLV55R'
110200130530     C                   PARM                    dslv55
110300130527    3c     d55err        ifeq      *blanks
110400031219     c                   seton                                        01
110500130527     c                   movel     d55tfp        varx
110600031219    3c                   endif
110700130527     c
110800031219   x2c                   else
110900130527     c* se primo liv. in arrivo  o 2°liv.in arr. Diretto
111000031219     c* invio al terminal di partenza di lna
111100031219     c                   seton                                        01
111200130527     c                   movel     s_o54tfp      varx
111300031219    2c                   endif
111400031112    1c                   endif
111500031110     c
111600130527    1c     *in01         ifeq      *on
111700971104     C                   EXSR      CAEX
111800130527     c
111900130527     C* SE foglio locale accendo indicatore 97
112000130527     c     fgvlnp        ifeq      s_o54tfa
112100971106     C                   SETON                                        97
112200971106     C                   END
112300130527    1C                   END
112400130527     c*
112500130527     c* Controllo se c'e' transito
112600130527     C                   exsr      CtrFLP
112700000321     C*
112800130702     C* CONTROLLO SE C'E' TRANSITO
112900971104     C*
113000130530     c                   if        wBORtran='S'
113100130703     C                   MOVEL     wflp          VAR2X
113200130703     C                   MOVEL     wflp          VARX
113300000428     C                   EXSR      TRANSI
113400000428     C                   EXSR      CAEX08
113500130527     C                   ENDif
113600910909     C*
113700910909     C                   MOVEL     BLPLNA        SAVLNA
113800130531    2C                   ENDif
113900150603    2c                   if        blpccm>0
114000150603     c                   eval      w_clibo=blpccm
114100150603     c                   else
114200150603     c                   eval      w_clibo=blpksc
114300150603    2c                   endif
114400150603     c* Lettura file FIBSP a cambio di lnp o codice cliente
114500150603     c* Solo se non è chiusura Barcode
114600150603     c                   if        pa2barcode=' ' and
114700151215     c                             w_filbsps='S'  and
114800150603     c                             (blplnp<>sav_lnp or w_clibo<>sav_clibo)
114900150603
115000150603     c                   exsr      sr_fibsp
115100150603
115200150603     c                   eval      sav_lnp=blplnp
115300150603     c                   eval      sav_clibo=w_clibo
115400150603     c                   endif
115500130527     c
115600130527     c* Eseguo routine di borderizzazione e/o messa in sospensione
115700130527     c*  automatica
115800130531   2ac                   if        Indx>0
115900130527     c                   exsr      Borderiz
116000130527     c
116100130527     c                   if        wescludi='S'
116200150605     c                   unlock    fnblp77l
116300150603     c*
116400150603     c* richiamo pgm per mettere in sospensione la bolla
116500150603     c* di cliente con colli che partono da diverse filiali se non presenti
116600150603     c* spunte
116700170327     c*****              if        w_SiBsp='S'  and w_Sosp='S'
116800170327     c                   if        w_Sosp='S'
116900150603     c                   clear                   fnlsu3ds
117000150605     c                   eval      ilsu3aas=%editc(blpaas:'X')
117100150605     c                   eval      ilsu3lnp=%editc(blplnp:'X')
117200150605     c                   eval      ilsu3nrs=%editc(blpnrs:'X')
117300150605     c                   eval      ilsu3nsp=%editc(blpnsp:'X')
117400150610     c                   eval      ilsu3dsp_B=%editc(fgvdfv:'X')
117500150603     c                   eval      ilsu3nos='S'
117600170509     c                   eval      ilsu3flg='R'
117700150603     c                   movel     fnlsu3ds      kpjbu
117800150603     c                   call      'FNLSU3R'
117900150603     c                   parm                    kpjbA
118000150703     c                   movel     kpjbu         fnlsu3ds
118100150703     c* Se Bolla in sospensione oltre xx giorni memorizzo in schiera per invio
118200150703     c* mail
118300150703     c                   if        olsu3err=*blanks  and
118400150703     c                             olsu3sosp='1'
118500170608     c*                  exsr      sr_MemoEmail
118600150703     c                   endif
118700150603     c                   endif
118800130610     c
118900130531   2bc                   else
119000130527     c*
119100130527     c* Memorizzo in bolla fil transito se c'e'
119200130530     c                   if        wBORtran='S'
119300130527     c                   movel     wflp          blpflp
119400130527     c                   else
119500130527     c                   clear                   blpflp
119600130527     c                   endif
119700130527     c                   movel     s_o54tfa      blptfa
119800130527     c                   movel     s_o54tfa      blpfea
119900130527     C                   Z-ADD     FGV2          BLPNFV
120000130527     C                   Z-ADD     fgvdfv        BLPDBR
120100910905     C**
120200950109     C** DETTAGLIO SEGNACOLLI             F N B L T 0 0 F
120300950109     C**
120400950118     C*  SE IN BOLLA CTM= AUTOSPUNTA LA GENERO --> 04 ON
120500910911     C                   SETOFF                                       04
120600130527     C                   CLEAR                   WBLP01            1
120700950118     C     BLPCTM        LOOKUP    CTM                                    04
120800910911     C*
120900950109     C     KBLP2         SETLL     FNBLT000
121000950109     C     KBLP2         READE     FNBLT000                               26
121100910905     C*
121200910905     C     *IN26         DOWEQ     '0'
121300130527     C                   CLEAR                   WBRV1             1
121400130527     C                   CLEAR                   WBRV5             1
121500130527     C                   CLEAR                   dsbrvw5
121600130527     C*
121700911119     C* SE AUTOSPUNTA LA CREO
121800950109     C     *IN04         IFEQ      *ON
121900140618     C     KBLT2         SETLL     FNBRV05                                19
122000950109     C  N19              EXSR      CRTSPU
122100950109     C                   ENDIF
122200130527     c*
122300130527     c* Controllo le anomalie 110 105 se sono da chiudere
122400130527     c                   exsr      Contr_anom
122500060220     c
122600950109     C* FLAGGO LA TRASMISSIONE
122700950109     C                   MOVEL     'T'           BLTFTR
122800950109     C                   Z-ADD     DATEU         BLTDTR
122900910905     C*
123000950109     C                   UPDATE    FNBLT000
123100130527     C*
123200130527     C* SE TROVATA SPUNTA AGGIORNO LA DATA ENTRATA
123300130527     C     WBRV5         IFEQ      'S'
123400130701     c                   if        wblp01=' '
123500130527     C                   update    FNBLP000
123600130527     C                   MOVEL     'S'           WBLP01
123700130701     c                   endif
123800130527     c*
123900130527     c                   eval      dsbrv =dsbrvw5
124000130527     C                   EXSR      call44
124100130527     C                   ENDIF
124200910905     C*
124300950109     C     KBLP2         READE     FNBLT000                               26
124400910905     C                   END
124500130527     c
124600130527     c                   if        wblp01='S'
124700130527     c     kblp2         chain     fnblp01l
124800130911
124900130911     c* se ho trovato una spunta partenza scrivo AGB x aggiornare le date
125000130911     C* SCRIVO IL FILE FNAGB01L PER AGGIORNARE DATE
125100130911     C                   MOVEL     'P'           AGBTBO
125200130911     C                   MOVEL     BLPAAS        AGBAAS
125300130911     C                   MOVEL     BLPLNP        AGBLNP
125400130911     C                   MOVEL     BLPNRS        AGBNRS
125500130911     C                   MOVEL     BLPNSP        AGBNSP
125600130911     C                   MOVEL     'DT'          AGBAGB
125700130911     C                   WRITE     FNAGB000                             39
125800130527     c                   endif
125900950109     C*
126000060220     C* FLAGGO FIAR500F
126100950109     C                   EXSR      WRITEF
126200910905     C**
126300950109     C**  BOLLA                           F N B L P 0 0 F
126400910905     C**
126500060220     C* IL FLAG DDT LO METTO IN BL2SV1
126600060220     C                   MOVEL     BLPFST        BL2SV1
126700060220     C* IL FLAG sigla operatore video lo metto in bl2cei
126800000619     C                   MOVE      BLPSOP        BL2CEI
126900060220     c
127000950109     C                   Z-ADD     DATEU         BLPDT3
127100921026     C                   MOVEL     'S'           BLPFT3
127200950109     C                   Z-ADD     DATEU         BLPDT2
127300950109     C                   MOVEL     'T'           BLPFT2
127400950109     C                   Z-ADD     DATEU         BLPDT1
127500950109     C                   MOVEL     'T'           BLPFT1
127600911119     C*
127700911119     C* IN QUELLO PER SEDE E FIL.ARR SE ASSEGNATO 9999 METTO LA L.ARR
127800950109     C                   Z-ADD     BLPKSC        W0070             7 0
127900911119     C                   MOVE      BLPKSC        COMKSC            4 0
128000911119     C     COMKSC        IFEQ      9999
128100911119     C                   MOVEL     BLPLNA        BLPKSC
128200911119     C                   END
128300911119     C*
128400950109     C   01              WRITE     FNBLPE00
128500950113     C* SOLO PER TRANSITO: IMPOSTO IL VOLUME BOLLETTATO CON QUELLO
128600950113     C*  DA FATTURARE
128700130702     C     wbortran      IFEQ      'S'
128800950113     C                   Z-ADD     BLPVLB        W0053             5 3
128900950113     C                   MOVEL     BLPFVB        W001A             1
129000950113     C*
129100950113     C                   Z-ADD     BLPVLF        BLPVLB
129200950113     C                   MOVEL     BLPFVF        BLPFVB
129300950113     C*
129400950113     C                   WRITE     FNBLPT00
129500950113     C*
129600950113     C                   Z-ADD     W0053         BLPVLB
129700950113     C                   MOVEL     W001A         BLPFVB
129800021127     c
129900950113     C                   ENDIF
130000950412     C* MANDO IN SEDE SOLO E' PREVISTA LA TRASMISISONE DAL CODICE BOLLA
130100950412     C     §3ABSD        IFEQ      ' '
130200070423     C                   WRITE     FNBLP046                             90
130300950412     C                   ELSE
130400950412     C                   MOVEL     'N'           BLPFT2
130500950412     C                   ENDIF
130600911119     C*
130700950109     C                   Z-ADD     W0070         BLPKSC
130800000320     C**
130900130527     c                   if        wblp01='S'
131000130527     C                   UPDATE    FNBLP001
131100130527     c                   else
131200950109     C                   UPDATE    FNBLP000
131300130527     c                   endif
131400010309     c*
131500010309     c* AGGIORNO E SFLAGGO ARCHIVI O.R.M.
131600060220     c* Dentro a questa routine devo chainare fIAR4 tipo record "M"
131700010309     c                   EXSR      AGGORM
131800931020     C*
131900011121     C* CREO FILE TASSAZIONE PADRONCINI fiftt00F E fiftd00F
132000931027     C                   EXSR      CRTTAS
132100151118     c* chiudo anomalia 20 se lnp/cliente con sospensione bolle in
132200151118     c* chiusura f.v.p.
132300151118     c                   if        w_sibsp='S'
132400151118     C                   MOVEL     020           comcaa
132500151118     c     kanm01        chain     fnanm01l
132600151118     c                   if        %found(fnanm01l)
132700151118     C                   CLEAR                   DSLR33
132800151118     C                   MOVE      WDTP          D33DCH
132900151118     C                   MOVEL     '1'           D33FSC
133000151118     C                   MOVEL     '  '          D33CCH
133100151118     C                   MOVE      ANMNR1        D33NRR
133200151118     C                   CALL      'FNLR33R'
133300151118     C                   PARM                    DSLR33
133400151118     c                   endif
133500151118     c                   endif
133600950116     C                   ENDIF
133700130530     c
133800130531   2aC                   endif
133900130531   2bC                   endif
134000910906     C*
134100130531     C     Kblp77        reade     FNBLP000                               22
134200130531    1C                   ENDDO
134300130531     c
134400130531     c                   add       1             I
134500130531     c                   enddo
134600900601     C*
134700011121     C* CREO fiftt00F PER ULTIMA LETTURA
134800931203     C                   EXSR      CRTFTT
134900931203     C*
135000900601     C                   ENDSR
135100130527     C**************************************************************************
135200130527     C*    CONTROLLO ESISTENZA FILIALE DI TRANSITO
135300130527     C**************************************************************************
135400130527     C     CTRFLP        BEGSR
135500130530     c                   clear                   wBORtran          1
135600130527     C*
135700130527     C* CONTROLLO SE C'E' TRANSITO
135800141029     C******             SETOFF                                       05
135900130527     C*
136000130531     C     flp(I)        IFgt      *zeros
136100130531     c                   movel     flp(I)        WFLP              3 0
136200130527     c
136300130527     C* C'E' TRANSITO SE :
136400130531     C* 1) FLP NON E' = ALLA LINEA DI ARRIVO
136500130531     C* 2) FLP NON E' = AL TERMINAL DI ARRIVO DELLA LINEA ARRIVO
136600130527     c
136700130531     C     savffv        IFNE      wFLP
136800130531     C     s_o54tfa      ANDNE     wFLP
136900130530     C                   eval      WBORtran='S'
137000130527     C                   END
137100130527     C                   END
137200130527     C*
137300130527     C                   ENDSR
137400130527     C**-------------------------------------------------------*
137500130527     C* Borderizzazione bolle e messa in sospensione automatica
137600130527     C**-------------------------------------------------------*
137700130527     C     Borderiz      BEGSR
137800130527     c                   clear                   wescludi          1
137900130610     c                   clear                   uncollo
138000150603     c                   clear                   w_sosp            1
138100130527     c
138200130527     C* SPEDIZIONE GIA' BORDERIZZATA               - PER BORDERO'
138300130527    1C     BLPDBR        IFNE      0
138400130527     c                   eval      WEscludi='S'
138500130530     c                   leavesr
138600130527    1C                   END
138700130527     c
138800130527     C* DATA SPEDIZIONE < = DATA BORDERO' IMMESSA  - PER BORDERO'
138900130530     c                   if        pa2barcode=' ' and
139000130530     c                             blpdsp>fgvdfv
139100130527     c                   eval      WEscludi='S'
139200130530     c                   leavesr
139300130527    1C                   END
139400130527     c
139500130527     c* Se codice cliente mittente non è quello immessa. escludo
139600130527     c                   clear                   alfaccm
139700130527    1c                   if        pa2ccm>*zeros
139800130527     c
139900130527    2c                   if        blpccm>0
140000130527     c                   movel     blpccm        alfaccm           7
140100130527     c                   else
140200130527     c                   movel     blpksc        alfaccm
140300130527    2c                   endif
140400130527     c
140500151127    2c***                if        alfaccm<>pa2ccm
140600151127     c                   if        %lookup(alfaccm:ccmb)=0
140700130527     c                   eval      WEscludi='S'
140800130530     c                   leavesr
140900130527    2C                   endif
141000130527    1C                   endif
141100130527     c
141200130527      * SE MANCA IL COD PADRONCINO LA BOLLA È UN PREPAGATO NON CONFERMATO.
141300130527      *  SE ANNO/MESE BOLLA < DI ANNO/MESE DEL BORDERO' FORZO IL CODICE
141400130527      *  AUTOTRASP. = 999, LA DATA RITIRO = DATA BOLLA, IL CODICE
141500130527      *  PRESTAZIONE = 'P' E BORDERIZZO.
141600130527      *  IN CASO CONTRARIO NON BORDERIZZO.
141700130527    1C     BLPPDR        IFEQ      0
141800130530     C                   MOVEL     BLPMGS        WMMSp
141900130530    2C                   If        BLPAAS < kaaa4 or WMMSp< kmmm
142000150930     c                             or (fgvdfv>= §DGDCPD and fgvdfv<= §DGDCPA)
142100130527     C                   Z-ADD     999           BLPPDR
142200130527     C                   movel     blplnp        BLPPDR
142300130527     C                   Z-ADD     BLPMGS        BLPDRT
142400130527     C                   MOVEL     BLPAAS        BLPDRT
142500130527     c
142600130527    3C     BLPFPP        IFEQ      *BLANKS
142700130527     C                   MOVEL     'P'           BLPFPP
142800130527    3C                   ENDIF
142900130527      *
143000130527   x2C                   ELSE
143100130527     c                   eval      WEscludi='S'
143200130530     c                   leavesr
143300130527    2C                   endif
143400130527     c*
143500130527   x1c                   else
143600130530     c
143700130530     c* Per borderizzazione BARCODE --> controllo se esiste spunta o anom 010
143800170502    2c******             if        pa2barcode= 'S'  or W_siBsp='S'
143900170502    2c                   if        pa2barcode= 'S'
144000130530     c                   exsr      CTRNSC
144100130530     c
144200130627    3c                   if        uncollo=' '
144300130530     c                   eval      WEscludi='S'
144400150603     c                   if        w_SiBsp='S'
144500150603     c                   eval      W_sosp  ='S'
144600150603     c                   endif
144700130530     c                   leavesr
144800130610    3C                   endif
144900130627    2C                   endif
145000170327     c
145100170502     c* Controllo analogo anche se per il cliente o la lnp è prevista sospension
145200170502     c* su FIBSP (cliente con colli in partenza da più filiali='S')
145300170502     c*  escluse le bolle legate figlie
145400170502    2c                   if        w_sibsp= 'S'
145500170502     c     kblp2         chain     fnlbl01l
145600170502    3c                   if        not %found(fnlbl01l)
145700170502     c                   exsr      CTRNSC
145800170502     c
145900170502    4c                   if        uncollo=' '
146000170502     c                   eval      WEscludi='S'
146100170502    5c                   if        w_SiBsp='S'
146200170502     c                   eval      W_sosp  ='S'
146300170502    5c                   endif
146400170502     c                   leavesr
146500170502    4C                   endif
146600170502    3C                   endif
146700170502    2C                   endif
146800170502
146900170327     c* Se bolla legata "figlia" senza spunte, il giorno della bollettazione
147000170327     c*  la sposto in automatico in sospensione
147100170421     c* Escludo lo bolle lnp 101
147200170327     c                   if        blpdsp=blpdim and blpnrs=0 and blpdse=0
147300170421     c                             and blpdpc=0 and blpdam=0 and blplnp<>101
147400170327     c     kblp2         chain     fnlbl01l
147500170327     c                   if        %found(fnlbl01l)
147600170413     c* visto che ci sono casi "strani" , eseguo anche il controllo
147700170413     c*   dei colli e del manca record
147800170413     c                   exsr      ctrnsc
147900170413
148000170413    3c                   if        uncollo=' '
148100170327     c                   eval      WEscludi='S'
148200170327     c                   eval      W_sosp  ='S'
148300170327     c                   endif
148400170413     c
148500170413     c                   endif
148600170327     c                   endif
148700130610     c*
148800130530    1c                   endif
148900130527     c
149000130527     C                   ENDSR
149100130530     C**************************************************************************
149200130530     C*    CONTROLLO ESISTENZA SPUNTE PER LA BOLLA - borderizz.barcode
149300130530     C**************************************************************************
149400130530     C     CTRNSC        BEGSR
149500130530     C*
149600130530     C                   SETOFF                                       10
149700130530     c                   clear                   uncollo           1
149800130530     C* CONTROLLO SE CTM NON PREVEDE LA SPUNTA DEI SEGNACOLLI:
149900130530     C*  A BORDERO' LO STESSO (FLAG 5 DEL CTM)
150000170413     c* questo vale sempre: anche per la sospensione automatica
150100170413     c*  per sicurezza lo faccio anche se non dovrebbe mai accadere
150200170413     c***                if        pa2barcode= 'S'
150300130530     C     BLPCTM        LOOKUP    CTM                                    10
150400130530     c                   if        *in10
150500130530     C                   eval      uncollo='S'
150600130530     C                   leavesr
150700130530     C                   endif
150800170413     c***                endif
150900151118
151000151118     c                   if        w_sibsp='S'
151100151118     C                   MOVEL     020           comcaa
151200151118     c     kanm01        chain     fnanm01l
151300151118     c                   if        %found(fnanm01l) and anmdch=0
151400151118     C                   eval      uncollo='S'
151500151118     C                   leavesr
151600151118     c                   endif
151700151118     c                   endif
151800130530     c
151900130530     C* SI SPUNTA: CONTROLLO SE ALMENO UN SEGNACOLLO SPUNTATO C'E'
152000130530     C     KBLp2         SETLL     FNBLT01L
152100130530     C     KBLp2         READE(n)  FNBLT01L
152200130530     C*
152300130530    1C                   DOW       not %eof(fnblt01l)
152400130610     c
152500130530     C* LA FILIALE IN GESTIONE PER BORDERO' E' SIMFEL
152600150603     c                   if        pa2barcode='S'
152700130530     C                   Z-ADD     1             KNPG
152800130530     C                   Z-ADD     SIMFEL        KFGS
152900130530     C                   Z-ADD     fgv2          wnfv60
153000130530     c* BARCODE
153100130530     C     KBRVbor       SETLL     FNBRV01L                               31
153200150603     c                   else
153300150603     C     KBRV7         SETLL     FNBRV07L                               31
153400150603     c                   endif
153500130530    3c                   if        *in31
153600130530     C                   SETON                                        10
153700130530     C                   eval      uncollo='S'
153800130530     c                   leavesr
153900130530    3c                   endif
154000130530     c
154100130530     c* non trovata spunta ma presente manca record bolla in arrivo ancora
154200130530     c* aperta --> borderizzo lo stesso
154300130530    3c                   if        *in31=*off
154400130607     C                   MOVEL     010           comcaa
154500130530     c     kanm1         setll     fnanm02l
154600130530    4c                   if        %equal(fnanm02l)
154700130607     C                   eval      uncollo='S'
154800130530     c                   seton                                        10
154900130530     c                   leavesr
155000130530    4c                   endif
155100130530    3c                   endif
155200130530     c
155300130607     C     kblp2         READE(n)  FNBLT000
155400130530    1C                   ENDdo
155500130530     C*
155600130530     C                   ENDSR
155700130527     C**************************************************************************
155800130527     C*    CONTROLLO anomalia 110 105 da chiudere
155900130527     C**************************************************************************
156000130527     C     Contr_Anom    BEGSR
156100130527     C* ANOMALIA--> "NO BOLLA BORDERIZZATA" CHIUDO
156200130607     C                   MOVEL     110           comcaa
156300130527     C     KANM1         CHAIN(N)  FNANM02L                           32
156400130527     C     *IN32         IFEQ      *OFF
156500130607     C                   EXSR      call_lr33
156600130527     C                   END
156700130527     C*
156800130527     C* ANOMALIA --> "MANCA BOLLA"          CHIUDO
156900130607     C                   MOVEL     105           comcaa
157000130527     C     KANM1         CHAIN(N)  FNANM02L                           32
157100130527     C**
157200130527     C* SE C'E' GIA' CHIUSA COME ANNULLATA --> COME SE NON CI FOSSE
157300130527     C* (P.ES SE LA 105 ERA STATA ANNULLATA MA SUCCESSIVAMENTE LA
157400130527     C* SPEDIZIONE VIENE BOLLETTATA. IN QUESTO CASO IL PGM DEVE)
157500130527     C* AGGIORNARE BLTNFV)
157600130527    1C     *IN32         IFEQ      *OFF
157700130527     C     ANMDCH        ANDEQ     0
157800130527     C*
157900130527     C     *IN32         OREQ      *OFF
158000130527     C     ANMDCH        ANDGT     0
158100130527     C     ANMCCH        ANDNE     'AN'
158200130527     C**
158300130527    2C     ANMDCH        IFEQ      0
158400130527     C                   CLEAR                   WSPE
158500130527     C*
158600130527     C* CERCO LA SPUNTA ENTRATA PER AGGIORNARE LA DATA ENTRATA
158700130527     C                   MOVEL     '5'           KNPG
158800130527     C                   MOVE      *HIVAL        WVVDFV
158900130527     C     KBRV1         SETLL     FNBRV09L                           32
159000130527     C     KBRV1         READE     FNBRV09L                               32
159100130527     C*
159200130527     C     *IN32         DOWEQ     *OFF
159300130530     C                   CLEAR                   DSLV53
159400130527     C                   MOVEL     'V'           D53TFO
159500130527     c                   exsr      data_spunta
159600130527     c
159700130527     C     KBRV1         READE     FNBRV09L                               32
159800130527     C                   ENDDO
159900130527     C*
160000130527     C                   MOVEL     '1'           KNPG
160100130527     C     KBRV1         CHAIN     FNBRV09L                           32
160200130527     C     *IN32         DOWEQ     *OFF
160300130527     C                   MOVEL     'S'           WBRV1             1
160400130527     C* SE ESISTE UNA SPUNTA PARTENZA CON DATA < DELLA DATA PRIMA SPUN-
160500130527     C* TA ENTRATA --> MEMORIZZO I DATI DELLA SPUNTA PARTENZA
160600130530     C                   CLEAR                   DSLV53
160700130527     C                   MOVEL     'G'           D53TFO
160800130527     c                   exsr      data_Spunta
160900130527
161000130527     C     KBRV1         READE     FNBRV09L                               32
161100130527     C                   ENDDO
161200130527     C*
161300130527     C* TROVATA SPUNTA PARTENZA
161400130527    3C     WBRV1         IFEQ      'S'
161500130527     C* CHIAMO IL PGM DI AGGIORNAMENTO BOLLE PER SCRIVERE LA DATA ENTRA
161600130527     C* TA
161700130527     C* SOLO SE FV CHIUSO AGGIORNO LA BOLLA
161800130527     c* Es 27 05 13 : non lo faccio sempre se c'e' anomalia 105 significa
161900130527     c*  che la spunta è stata già elaborata dalla chiusura FGV
162000130527     c*  quindi il collo/bolla devono essere aggioranti a prescindere
162100130527    4C**** D53ERR        IFEQ      *BLANKS
162200130527     C**** D53FCF        ANDEQ     'S'
162300130527     C*
162400130527     C                   Z-ADD     BRVNPS        BLTNPP
162500130527     C                   Z-ADD     D53NFV        BLTNFV
162600130527     C                   Z-ADD     D53DFV        BLTDFV
162700130527     C*
162800130527     C* AGGIORNO LA DATA PARTENZA PRIMO COLLO SULLA BOLLA
162900130527    5C     BLPDPC        IFEQ      0
163000130527     C                   Z-ADD     BLTDFV        BLPDPC
163100130527     C* SOMMO LA SPEDIZIONE DI UN COLLO "EX MANCA BOLLA"
163200130527     C                   Z-ADD     1             WSPE
163300130527    5C                   END
163400130527     C*
163500130527     C* CALCOLO PESO E VOLUME UNITARI DI UN COLLO "EX MANCA BOLLA"
163600130527     C                   EXSR      TOTFVD
163700130527     C*
163800130527     C* AGGIORNO FNSTA PER LA RIELABORAZIONE DELLE STATISTICHE
163900130527     c                   If        BltDfv <= §SdfParU
164000130527     C                   Z-ADD     BLTDFV        SA2DTS
164100130527     C                   MOVE      'P'           SA2TRC
164200130527     C                   CALL      'FNLSA2R3'
164300130527     C                   PARM                    SA2TLA
164400130527     C                   PARM                    SA2TRC
164500130527     C                   PARM                    SA2DTS
164600130527     C                   PARM                    WWLNA             3 0
164700130527     C                   MOVEL     'S'           WLSA2             1
164800130527    5C                   END
164900130527     C*
165000130527     c                   If        BltDfv <= §SdfTerU
165100130527     C                   MOVE      BLTDFV        SA2DTS
165200130527     C                   MOVE      'T'           SA2TRC
165300130527     C                   CALL      'FNLSA2R3'
165400130527     C                   PARM                    SA2TLA
165500130527     C                   PARM                    SA2TRC
165600130527     C                   PARM                    SA2DTS
165700130527     C                   PARM                    WWLNA             3 0
165800130527     C                   MOVEL     'S'           WLSA2             1
165900130527    5C                   END
166000130527     C*
166100130527     C* SE FOGLIO APERTO NON AGGIORNO LA BOLLA E MEMORIZZO IL FOGLIO
166200130527     C*  SU CUI STO BORDERIZZANDO (non dovrebbe più accadere)
166300130527   X4C****               ELSE
166400130527     C*
166500130527     C****               Z-ADD     VIDNFV        BLTNFV
166600130527    4C****               ENDIF
166700130527     C*
166800130527    3C                   ENDIF
166900130527     C* SE TROVATA SPUNTA CLEARO DATA ENTRATA E VOLUME CML PRIMA
167000130527     C* AFFINCHE' L'FNLS45R AGGIORNI QUESTI CAMPI IN BASE ALLA PRIMA
167100130527     C* SPUNTA PASSATA SUL SISTEMA
167200130527    4C     WBRV5         IFEQ      'S'
167300130527     C                   CLEAR                   BLTDSE
167400130527    5C     W5VUC         IFGT      *ZEROS
167500130527     C                   CLEAR                   BLTVUC
167600130527    5C                   END
167700130527    4C                   END
167800130527     C*
167900130607     C                   EXSR      call_lr33
168000130527    2C                   END
168100130527     C                   ELSE
168200130527   X1C*
168300130527     C                   Z-ADD     FGV2          BLTNFV
168400130527    1C                   ENDIF
168500130527     C*
168600130527     c
168700130527     C                   ENDSR
168800130527      ****************************************************************
168900130527      * Prendo data spunta
169000130527      ****************************************************************
169100130527     c     Data_spunta   BEGSR
169200130527     C* DETERMINO DATA FOGLIO
169300130527     C                   MOVE      BRVNPG        D53NPG
169400130527     C                   MOVE      BRVNFV        D53NFV
169500130527     C                   MOVE      BRVFGS        D53FGS
169600130527     C                   MOVE      SIMFEL        D53FEL
169700130527     C                   CALL      'FNLV53R'
169800130530     C                   PARM                    DSLV53
169900130527     C                   MOVE      '1'           WLV53             1
170000130527     C*
170100130527     C     D53ERR        IFEQ      *BLANKS
170200130527     C     D53DFV        iflt      WVVDFV
170300130527     c* se è presente il peso e volume nella spunta meno recente
170400130527     c*  li devo mantenere per il richiamo a fnls45r
170500130527     c
170600130527     c                   if        brvvuc=0  and w5vuc>0
170700130527     c                   eval      brvvuc=w5vuc
170800180201     c                   eval      brvtap=w5tap
170900130527     c                   endif
171000130527     c                   if        brvpuc=0  and w5puc>0
171100130527     c                   eval      brvpuc=w5puc
171200180201     c                   eval      brvtap=w5tap
171300130527     c                   endif
171400130527     c                   if        brvcan=' ' and w5can<>' '
171500130527     c                   eval      brvcan=w5can
171600130527     c                   endif
171700130527     c
171800130527     C                   MOVEL     'S'           WBRV5             1
171900130527     c                   movel     dsbrv         dsbrvw5
172000130527     C                   MOVE      D53DFV        WVVDFV
172100130527     c* se c'e' il peso e volume --> lo passo per il calcolo
172200130527     c                   else
172300130527     c                   if        w5vuc=0  and brvvuc>0
172400130527     c                   eval      w5vuc=brvvuc
172500180201     c                   eval      w5tap=brvtap
172600130527     c                   endif
172700130527     c                   if        w5puc=0  and brvpuc>0
172800130527     c                   eval      w5puc=brvpuc
172900180201     c                   eval      w5tap=brvtap
173000130527     c                   endif
173100130527     c                   if        w5can=' ' and brvcan<>' '
173200130527     c                   eval      w5can=brvcan
173300130527     c                   endif
173400130527     c
173500130527     C                   END
173600130527     C                   END
173700130527     c                   ENDSR
173800130527     C**************************************************************************
173900130527     C*    CALCOLO PESO E VOLUME UNITARI
174000130527     C**************************************************************************
174100130527     C     TOTFVD        BEGSR
174200130530     c                   exsr      CALUNI
174300130527     C*
174400130527     C                   Z-ADD     BRVNFV        WNFV5
174500130603     C                   MOVEL     '1'           WTRC
174600130530     C     KFVDbor       CHAIN     FNFVD000                           34
174700130527     C   34              CLEAR                   FNFVD000
174800130527     C***  COLLI ***
174900130527     C                   ADD       1             FVDCLA
175000130530     c                   clear                   wtran
175100130527     C*
175200130527     C***  SPEDIZIONI  ***
175300130527     C     WSPE          IFGT      0
175400130530     C                   z-add     WSPE          wspen
175500130527     C                   ENDIF
175600130530     c
175700130530     c                   exsr      RIEFVD
175800130527     C*
175900130527     C* ABBLENCO FLAG E DATA TRASMISSIONE PER INVIO IN SEDE
176000130527     C                   MOVEL     ' '           FVDFTR
176100130527     C                   Z-ADD     0             FVDDTR
176200130527     C*
176300130527     C*
176400130527     C     *IN34         IFEQ      *ON
176500130527     C                   Z-ADD     BRVNFV        FVDNFV
176600130527     C                   MOVEL     '1'           FVDTRC
176700130527     C                   MOVEL     SIMFEL        FVDLNP
176800130527     C                   MOVEL     BLPLNA        FVDLNA
176900130527     C                   WRITE     FNFVD000
177000130527     C                   ELSE
177100130527     C*
177200130527     C                   UPDATE    FNFVD000
177300130527     C                   ENDIF
177400130527     C*
177500130527     C                   ENDSR
177600950109     C**-----------------------------------------------*
177700950109     C* SCRITTURA DI ALCUNI FILE DELLA BOLLA -----------
177800950109     C**-----------------------------------------------*
177900950109     C     WRITEF        BEGSR
178000060220     c
178100030107      *  Estensione Testata Bolla    F I A R 5 0 0 F
178200030107     c     Kblp2         Setll     Fiar501l
178300030203Do  1c                   Do        *Hival
178400030107     c     Kblp2         Reade     Fiar501l
178500030107      * Fine File esco dal ciclo
178600030107     c                   If        %Eof(Fiar501l)
178700030107     c                   Leave
178800030107     c                   EndIf
178900030110      * Controllo se già inviato
179000030110     c                   If        Ar5Dt1 <> *all'9' and Ar5Dt1 > 0
179100030110     c                   Iter
179200030110     c                   EndIf
179300030107      * Flaggo per la trasmissione anche se il record non viene trasmesso
179400030107     c                   Eval      Ar5Ft1 = 'T'
179500030107     c                   Z-Add     Dateu         Ar5Dt1
179600030107     c                   Update    Fiar5000
179700030203    1c                   EndDo
179800030107
179900950109     C                   ENDSR
180000900608     C**-----------------------------------------------*
180100950109     C** TRASFERISCO BOLLE  - 99999 -
180200900608     C**-----------------------------------------------*
180300000619     C     TRAS99        BEGSR
180400030107
180500030107     c                   Open      Fiftt01l
180600910906     C*
180700130702     C                   SETOFF                                       01
180800920221     C                   Z-ADD     0             SAVLNA
180900130527     C     K02BLP77_9    reade     FNBLP000                               22
181000900608     C*
181100950323    1C     *IN22         DOWEQ     '0'
181200010109      *
181300010109      * SE MANCA IL COD PADRONCINO LA BOLLA È UN PREPAGATO NON CONFERMATO.
181400010111      *  SE ANNO/MESE BOLLA < DI ANNO/MESE DEL JOB      CONFERMO FORZANDO
181500010109      *  COD.AUTOTRASP. = 999, LA DATA RITIRO = DATA BOLLA E IL CODICE
181600010109      *  PRESTAZIONE = 'P'. IN CASO CONTRARIO NON CONFERMO.
181700010109     C                   IF        BLPPDR = *zeros
181800010109     C                   movel     BLPMGS        WMMSp             2 0
181900010111     C                   movel     DATEU         WAAJb             4 0
182000010111     C                   move      dateu         W0040             4 0
182100010111     C                   movel     W0040         WMMJb             2 0
182200010109      *
182300010111     C                   If        BLPAAS < WAAJb or WMMSp < WMMJb
182400150930     c                             or (fgvdfv>= §DGDCPD and fgvdfv<= §DGDCPA)
182500010109     C                   z-add     999           BLPPDR
182600010109     C                   z-add     BLPMGS        BLPDRT
182700010109     C                   movel     BLPAAS        BLPDRT
182800010109     C                   If        BLPFPP = *BLANKS
182900010109     C                   eval      BLPFPP = 'P'
183000010109     C                   ENDIF
183100010109     C                   ENDIF
183200010109      *
183300010109     C                   ENDIF
183400010109      *
183500010109      *  PER I PREPAGATI, GIA' CONFERMATI
183600010109   1AC     BLPPDR        IFGT      0
183700990915     C*
183800950115     C* ELABORO SOLO LE BOLLE NON ANCORA TRASMESSE
183900991028    2C     BLPDT2        IFEQ      0
184000991028     C     BLPDT1        OREQ      0
184100920221     C*
184200940107     C* CHAIN CON CODICE BOLLA
184300940107     C                   EXSR      CTABEL
184400030107      * Aggiorno Fiftt con il numero di ritiri annullati
184500030107      * bolle "FY"
184600030107     c                   If        BlpCbo = 'FY'
184700030107     c                   Exsr      CrtFtt1
184800030107     c                   EndIf
184900000619     C*
185000920221     C* INVIO ANCHE PER FILIALE
185100950412    3C     §3ABFI        IFEQ      ' '
185200960411     C* SE E' "£" INVIO SOLO SE LNP<>LNA
185300960411     C     BLPCBO        OREQ      '£ '
185400960411     C     BLPLNP        ANDNE     BLPLNA
185500920221     C*
185600920221     C** L I N E A   A R R I V O   C A M B I A T A
185700950323    4C     SAVLNA        IFNE      BLPLNA
185800031112     C**                 CLEAR                   WLOCAL
185900971105     C                   SETOFF                                       01  50
186000950109     C*
186100130531     C                   CLEAR                   dslv55
186200140403     c                   z-add     blptfp        d55lnp
186300130531     C                   Z-ADD     BLPLNA        d55LIN
186400130531     C                   Z-ADD     PARDTP        d55DRF
186500130531     C                   CALL      'FNLV55R'
186600130531     c                   PARM                    dslv55
186700130531    5C     d55err        IFEQ      *BLANKS
186800130531     c                   z-add     d55tfp        s_o54tfp
186900130531     c                   z-add     d55tfa        s_o54tfa
187000130531     c* se secondo livello in arrivo
187100031219     c* invio la bolla al terpar del ter-arr della lna
187200130531    6c     blplna        ifne      d55tfa
187300130531     c                   clear                   dslv55
187400130531     C                   MOVEL     'P'           d55TPT
187500130531     C                   MOVEL     s_o54tfa      d55LIN
187600130531     C                   MOVEL     pardtp        d55DRF
187700130531     C                   CALL      'FNLV55R'
187800130531     C                   PARM                    dslv55
187900130531    7c     d55err        ifeq      *blanks
188000031219     c                   seton                                        01
188100130531     c                   movel     d55tfp        varx
188200031219    7c                   endif
188300031219   x6c                   else
188400031219     c* se primo liv. in arrivo o ogas diversi
188500031219     c* invio al terminal di partenza di lna
188600031219     c                   seton                                        01
188700031219     c                   movel     s_o54tfp      varx
188800031219    6c                   endif
188900031112    5c                   endif
189000031112     C     N_varx        LOOKUP    FEL                                    32
189100950323    5C     *IN32         IFEQ      *OFF
189200031112     C                   MOVEL     N_varx        VAR2X
189300031112     C                   MOVEL     N_varx        NUM3
189400950115     C                   EXSR      LCKBLP
189500031112     C                   Z-ADD     1             IY                3 0
189600031112     C     000           LOOKUP    FEL(IY)                                32
189700031112     C   32              Z-ADD     N_varx        FEL(IY)
189800950323    5C                   ENDIF
189900950115     C*
190000031112     c   50              setoff                                       01
190100950115     C*
190200031112     c     *in01         ifeq      *on
190300950109     C                   EXSR      CAEX
190400971106     C* SE BOLLA LOCALE MEMORIZZO CHE CE N'è ALMENO UNA
190500130531     c     simfel        ifeq      s_o54tfa
190600971106     C                   SETON                                        97
190700031112     C**                 MOVEL     'S'           WLOCAL
190800971106     C                   END
190900950323    5C                   ENDIF
191000950115     C*
191100950109     C                   MOVEL     BLPLNA        SAVLNA
191200950323    4C                   ENDIF
191300920221     C*
191400920427     C* SE E' DA TRASMETTERE SOLO IN SEDE NON APRO MEMBRI
191500950323   X3C                   ELSE
191600950109     C*
191700950109     C* IMPOSTO I FLAG DI TRASMISSIONE
191800950109     C                   MOVEL     'N'           BLPFT1
191900950109     C                   Z-ADD     DATEU         BLPDT1
192000940415     C*
192100940415     C* SE CODICE BOLLA FRANCO RECUPERO MUOVO DATA BORDERO' IN DCM/DAM
192200060303     c* stessa cosa per cod. bolla "M"-Prepagato annullato
192300950323    4C     BLPCBO        IFEQ      'B '
192400060303    4C     BLPCBO        oreQ      'M '
192500030728      * prima però imposto la data borderò uguale alla data foglio viaggio
192600030728     c                   Z-Add     FgvDfv        BlpDbr
192700920615     C                   MOVEL     BLPDBR        BLPDCM
192800920615     C                   MOVEL     BLPDBR        BLPDAM
192900950323    4C                   ENDIF
193000950323    3C                   ENDIF
193100950115     C*
193200950115     C* SOLO SE HO ALLOCATO I MEMBRI
193300950915    3C     *IN50         IFEQ      *OFF
193400911119     C*
193500060220     C* FLAGGO FIAR500F: LO FACCIO A PREDCINDERE DALLA TRASMISSIONE DELLA BO
193600060220     C                   EXSR      WRITEF
193700060220     C
193800991029     C* IN QUELLO PER ARRIVO SE 9999 METTO LA LN ARRIVO
193900950109     C                   Z-ADD     BLPKSC        W0070
194000950915     C                   MOVE      BLPKSC        COMKSC            4 0
194100950323    4C     COMKSC        IFEQ      9999
194200911119     C                   MOVEL     BLPLNA        BLPKSC
194300950323    4C                   END
194400950323     C*
194500991028     C     BLPDT3        IFEQ      0
194600991028     C                   Z-ADD     DATEU         BLPDT3
194700991028     C                   MOVEL     'S'           BLPFT3
194800991028     C                   ENDIF
194900991028     C*
195000991028    4C     BLPDT2        IFEQ      0
195100950109     C                   Z-ADD     DATEU         BLPDT2
195200950109     C*
195300991028    5C     §3ABSD        IFEQ      ' '
195400950109     C                   MOVEL     'T'           BLPFT2
195500070423     C                   WRITE     FNBLP046                             90
195600950109     C                   ELSE
195700950109     C                   MOVEL     'N'           BLPFT2
195800991028    5C                   ENDIF
195900991028    4C                   ENDIF
196000920221     C*
196100991028    4C     BLPDT1        IFEQ      0
196200991028    5C     §3ABFI        IFEQ      ' '
196300960411     C     BLPCBO        OREQ      '£ '
196400960411     C     BLPLNP        ANDNE     BLPLNA
196500991029     C*
196600060220     C* IL FLAG DDT LO METTO IN BL2SV1
196700060220     C                   MOVEL     BLPFST        BL2SV1
196800060220     C* IL FLAG sigla operatore video lo metto in bl2cei
196900000619     C                   MOVE      BLPSOP        BL2CEI
197000991029     C* IMPOSTO I FALG DI TRASMISSIONE
197100991029     C                   MOVEL     'T'           BLPFT1
197200991029     C                   Z-ADD     DATEU         BLPDT1
197300991029     C**
197400950109     C   01              WRITE     FNBLPE00
197500991028    5C                   ENDIF
197600991028    4C                   ENDIF
197700900608     C*
197800950109     C                   MOVEL     W0070         BLPKSC
197900950323     C* IMPOSTO IL FOGLIO A 99998 --> PER DIRE CHE L'HO GIA ELABORATA
198000950323     C                   Z-ADD     99998         BLPNFV
198100950109     C                   UPDATE    FNBLP000
198200950323    3C                   ENDIF
198300950323     C*
198400950323     C* SE BOLLA GIA' TRASMESSA --> IMPOSTO SOLO IL FOGLIO A 99998
198500950323   X2C                   ELSE
198600950323     C                   Z-ADD     99998         BLPNFV
198700950323     C                   UPDATE    FNBLP000
198800950323    2C                   ENDIF
198900990915   1AC                   ENDIF
199000910906     C*
199100130527     C     K02BLP77_9    reade     FNBLP000                               22
199200950323    1C                   ENDDO
199300900608     C*
199400900608     C                   ENDSR
199500931022     C*
199600011121     C*--- SCRIVO FILE TASSAZIONE  fiftt00F/fiftd00F -----------------*
199700931019     C     CRTTAS        BEGSR
199800931203     C***
199900931203     C*  C R E A Z I O N E    F L F T T 0 0 F
200000931203     C***
200100011121     C* CONTROLLO SE ESISTE IL FILE fiftt OGNI VOLTA CHE CAMBIANO IL
200200931203     C*   CODICE PADRONCINO E LA DATA RITIRO
200300931203     C*
200400931203    1C     COMBLP        IFNE      COMSAV
200500931203     C*
200600931203     C                   EXSR      CRTFTT
200700931203     C*
200800931203     C* CONTROLLO IL PADRONCINO OGNI VOLTA CHE CAMBIA
200900950113     C     WCPR          IFNE      BLPPDR
201000931203     C* IL COD.PADRONCINO DELLA BOLLA DEVE ESISTERE NELLA SCHIERA PDR
201100040826     C                   Z-ADD     1             W                 4 0
201200140620     C                   SETOFF                                       05
201300950113     C     BLPPDR        LOOKUP    PDR(W)                                 05
201400950113     C                   Z-ADD     BLPPDR        COMPDR
201500931203     C                   ENDIF
201600931203     C*
201700950113     C                   MOVEL     COMBLP        COMSAV
201800931203    1C                   ENDIF
201900970930     C*
202000971027    1C     *IN05         IFEQ      *ON
202100931027     C* CONTROLLO SE IL TIPO BOLLA E' DA ESCLUDERE
202200931027     C                   EXSR      CTAB
202300931027     C*
202400011121     C* ALF1 = '1' ===> CREO I FILE fiftt00F E fiftd00F  SE PDR NON DIP
202500971027     C*                 CREO COMUNQUE FILE FNSTPR0F PER TRASMISSIONE
202600971027     C*                 STOP. PER I NON DIPENDENTI, IL CALCOLO STOP IN
202700971027     C*                 QUESTO MOMENTO E' SOLO PREVENTIVO. VERRA' RI-
202800971027     C*                 FATTO AL MOMENTO DELLA CONFERMA CONTEGGI
202900020528     c*                 28/05/02:Creo dati per conteggi anche per i dipenden
203000020528     c*                          ti
203100971027    2C     ALF1          IFEQ      '1'
203200931203     C* NUMERO SPEDIZIONI TOTALE
203300020528     C*  05              ADD       1             COMSET
203400020528     C                   ADD       1             COMSET
203500931202     C*
203600931019     C* IMPOSTO CAMPI CHIAVE
203700971027    3C     BLPFPP        IFEQ      'M'
203800950113     C                   Z-ADD     1000          COMNDC
203900971027   X3C                   ELSE
204000950113     C                   Z-ADD     5000          COMNDC
204100971027    3C                   ENDIF
204200950113     C                   ADD       BLPNRT        COMNDC
204300950113     C                   MOVEL     BLPRSM        COMRSC
204400950114     C                   MOVEL     BLPDRT        COMDDC
204500931019     C*
204600971027    3C     COMTB1        IFEQ      'F'
204700931202     C                   MOVEL     BLPKSC        CO2KSC
204800971027   X3C                   ELSE
204900971027    4C     BLPCCM        IFNE      0
205000931202     C                   MOVEL     BLPCCM        CO2KSC
205100971027   X4C                   ELSE
205200931202     C                   MOVEL     BLPKSC        CO2KSC
205300950801     C                   MOVE      CO2KSC        W0040             4 0
205400971027    5C     W0040         IFEQ      9999
205500950801     C                   MOVE      8888          CO2KSC
205600971027    5C                   END
205700971027    4C                   ENDIF
205800971027    3C                   ENDIF
205900931202     C*
206000931019     C*
206100931202     C***
206200970930     C*  C R E A Z I O N E    F N F T D 0 0 F
206300931202     C***
206400020325     c                   clear                   wstp
206500020325     C* ESCLUDENDO FRANCHI E ASSEGNATI VARI,
206600020325     C*   CHAINO FNCLS00F PER PRENDERE IL NUMERO DEGLI STOP
206700020325     C                   Z-ADD     CO2KSC        NUM4              4 0
206800020325    5C     NUM4          IFNE      *ALL'8'
206900020325     C     NUM4          ANDNE     *ALL'9'
207000020325     C     CO2KSC        CHAIN     FNCLS000                           33
207100020325     C  N33              z-add     CLSSTP        wstp
207200020325    5C                   ENDIF
207300970930     C*
207400020325     c     wstp          ifeq      *zeros
207500011121     C     KFTD          CHAIN     fiftd000                           32
207600020325     c  n32              z-add     ftdstp        wstp
207700020326     c                   endif
207800020326     c     wstp          ifeq      *zeros
207900020416     c* Se il numero stop da attribuire è ancora = a 0 devo cercare
208000020416     c* nella distinta il primo numero disponibile
208100020416     C     KFTD1         setgt     fiftd001
208200020416     C     KFTD1         readpe    fiftd001
208300020416     c                   if        not %eof
208400020416     c                   eval      wstp = (ftdstp+1)
208500020416     c     wstp          iflt      1000
208600020416     c                   z-add     1000          wstp
208700020416     c                   endif
208800020417   x6c                   else
208900020417     c                   z-add     1000          wstp
209000020416     c                   endif
209100020325     c                   endif
209200020325     C                   CLEAR                   fiftd000
209300931108     C*
209400020325     c                   z-add     blpaas        ftdaas
209500020325     c                   z-add     blplnp        ftdlnp
209600020325     c                   z-add     blpnrs        ftdnrs
209700020325     c                   z-add     blpnsp        ftdnsp
209800931123     C                   MOVEL     BLPCAM        FTDCAP
209900020325     C                   z-add     1             FTDSET
210000931108     C                   MOVEL     'N'           FTDFGC
210100020325     c                   z-add     wstp          ftdstp
210200931108     C* FLAG ANTEPORTO
210300030902     C** IMPOSTO SEMPRE ANCHE SE C'ERA GIA' IL FLAG ANTEPORTO
210400931108     C                   MOVEL     BLPFAP        FTDFIN
210500931108     C*
210600021108     C* VOLUME : - SE TOTALE   ("T"), SOMMO BLPVLc
210700021108     C*          - SE PARZIALE ("Z"), SOMMO BLPVLc (SE MAGGIORE)
210800021108     C*                               ALTRIMENTI SOMMO BLPVLF
210900050112     C     BLPNCR        IFEQ      BLPNCL
211000950113     C                   Z-ADD     BLPVLC        WVOL
211100940321     C*
211200971027   X4C                   ELSE
211300971027    5C     BLPVLC        IFGE      BLPVLF
211400950113     C                   Z-ADD     BLPVLC        WVOL
211500971027   X5C                   ELSE
211600950113     C                   Z-ADD     BLPVLF        WVOL
211700971027    5C                   ENDIF
211800971027    4C                   ENDIF
211900021108     C* peso   : - SE TOTALE  , SOMMO BLPpkc
212000021108     C*          - SE PARZIALE, SOMMO BLPPKC (SE MAGGIORE)
212100021108     C*                               ALTRIMENTI SOMMO BLPPKF
212200050112     C     BLPNCp        ifeq      BLPNCL
212300021108     C                   Z-ADD     BLPpkC        Wpes
212400021108     C*
212500021108   X4C                   ELSE
212600021108    5C     BLPpkC        IFGE      BLPpkF
212700021108     C                   Z-ADD     BLPpkC        Wpes
212800021108   X5C                   ELSE
212900021108     C                   Z-ADD     BLPpkF        Wpes
213000021108    5C                   ENDIF
213100021108    4C                   ENDIF
213200940321     C*
213300931202     C*
213400931202     C                   Z-ADD     BLPNCL        FTDNCL
213500021108     C                   Z-ADD     WPES          FTDPKL
213600950113     C                   Z-ADD     WVOL          FTDVLU
213700950113     C* CALCOLO I COLLI PER CUI APPLICARE ETICHETTATURA: ESCLUDO
213800050112     C*  LE BOLLE CON SERIE
213900971027    5C     BLPNRS        IFEQ      0
214000950113     C                   Z-ADD     BLPNCL        FTDNCE
214100971027    5C                   ENDIF
214200931019     C*
214300931108     C* IMPOSTO CAMPI CHIAVE
214400931019     C                   Z-ADD     COMPDR        FTDPDR
214500020325     C                   movel     COMPDR        FTDfgs
214600931019     C                   MOVEL     COMTSR        FTDTSR
214700931019     C                   Z-ADD     COMNDC        FTDNDC
214800950115     C                   Z-ADD     COMDDC        FTDDDC
214900950113     C                   MOVEL     COMRSC        FTDRSC
215000931202     C                   MOVEL     CO2KSC        FTDKSC
215100931019     C*
215200011121     C                   WRITE     fiftd000
215300931019     C*
215400970930     C***
215500970930     C*  C R E A Z I O N E    F N S T P R 0 F
215600970930     C***
215700970930     C                   Z-ADD     BLPAAS        STRAAS
215800970930     C                   Z-ADD     BLPLNP        STRLNP
215900970930     C                   Z-ADD     BLPNRS        STRNRS
216000970930     C                   Z-ADD     BLPNSP        STRNSP
216100970930     C                   Z-ADD     COMPDR        STRPDR
216200970930     C                   Z-ADD     COMDDC        STRDDC
216300970930     C                   Z-ADD     COMNDC        STRNDC
216400970930     C* NUMERO STOP
216500970930     C                   Z-ADD     CO2KSC        NUM4              4 0
216600970930     C                   CLEAR                   STRSTP
216700000615     C* imposto numero stop = 99999 se merce portata a magazzino
216800970930     C     BLPFDN        IFEQ      'S'
216900000615     C                   Z-ADD     99999         STRSTP
217000970930     C                   ELSE
217100970930     C* chaino FNCLS00F per prendere l'eventuale numero stop
217200970930     C     NUM4          IFNE      *ALL'8'
217300970930     C     NUM4          ANDNE     *ALL'9'
217400970930     C     CO2KSC        CHAIN     FNCLS000                           33
217500001020     C  N33              Z-ADD     CLSSTP        STRSTP
217600970930     C                   ENDIF
217700970930     C                   ENDIF
217800970930     C                   MOVEL     CO2KSC        STRKSC
217900970930     C                   MOVEL     COMRSC        STRRSC
218000970930    3C                   SELECT
218100970930     C     §3ABSD        WHENEQ    *BLANKS
218200970930     C     §3ABFI        ANDEQ     *BLANKS
218300970930     C                   MOVEL     'E'           STRINV
218400970930     C     §3ABSD        WHENEQ    *BLANKS
218500970930     C                   MOVEL     'S'           STRINV
218600970930     C     §3ABFI        WHENEQ    *BLANKS
218700970930     C                   MOVEL     'F'           STRINV
218800970930    3C                   ENDSL
218900970930     C                   WRITE     FNSTPR00
219000931019     C*
219100931019     C                   ENDIF
219200970930     C                   ENDIF
219300931019     C                   ENDSR
219400931019     C*
219500011121     C*--- CREAZIONE fiftt00F ----------------------------------------*
219600931203     C     CRTFTT        BEGSR
219700931203     C*
219800931203    2C     COMSET        IFGT      0
219900950113     C     WCPR          ANDGT     0
220000931203     C                   Z-ADD     WCPR          WPDR
220100950113     C     WFPP          IFEQ      'M'
220200950113     C                   Z-ADD     1000          WNDC
220300950113     C                   ELSE
220400950113     C                   Z-ADD     5000          WNDC
220500950113     C                   ENDIF
220600950113     C                   ADD       WNRT          WNDC
220700931203     C*
220800011121     C     KFTT          CHAIN     fiftt000                           32
220900931209     C* 32 OFF - AGGIORNAMENTO
221000931203    3C     *IN32         IFEQ      *OFF
221100931203     C*
221200931203     C                   ADD       COMSET        FTTSET
221300931203     C                   CLEAR                   COMSET
221400931209     C*
221500011121     C                   UPDATE    fiftt000
221600931209     C*
221700931210     C* SOLO SE GIA' VALORIZZATA
221800931210     C     FTTFVL        IFEQ      'V'
221900931209     C* RICHIAMO PGM DI SVALORIZZAZIONE DISTINTA PADRONCINO
222000931209     C                   Z-ADD     WPDR          PA3PDR
222100931209     C                   MOVEL     COMTSR        PA3TSR
222200931209     C                   Z-ADD     WNDC          PA3NDC
222300950115     C                   Z-ADD     WDRT          PA3DDC
222400931209     C                   MOVEL     'N'           PA3FCT
222500931209     C                   MOVEL     'N'           PA3FVD
222600931209     C                   MOVEL     'N'           PA3FVT
222700041122     C                   MOVEL     ' '           PA3sml
222800931209     C                   MOVEL     PARAM3        KPJBU
222900011121     C                   CALL      'FICNE2R'
223000931209     C                   PARM                    KPJBA
223100931210     C                   ENDIF
223200931203     C*
223300931203   X3C                   ELSE
223400931209     C* 32 ON  - IMMISSIONE
223500011121     C                   CLEAR                   fiftt000
223600931209     C*
223700931203     C                   ADD       COMSET        FTTSET
223800931203     C                   CLEAR                   COMSET
223900931203     C                   Z-ADD     WPDR          FTTPDR
224000020325     C                   movel     WPDR          FTTfgs
224100931203     C                   MOVEL     COMTSR        FTTTSR
224200950113     C                   Z-ADD     WNDC          FTTNDC
224300931203     C                   Z-ADD     WDRT          FTTDDC
224400950113     C                   MOVEL     WFPP          FTTFPP
224500950113     C     WFPP          IFEQ      'M'
224600950113     C                   MOVEL     ETM(W)        FTTPEP
224700030107     c                   Movel     Scm(w)        FttLvl
224800950113     C                   ELSE
224900950113     C                   MOVEL     ETP(W)        FTTPEP
225000030107     c                   Movel     Scp(w)        FttLvl
225100950113    2C                   ENDIF
225200931203     C*
225300011127      * In base alla data del documento aggancio la tab.GEDCN per
225400011127      * impostare la divisa della valorizzazione dei conteggi.
225500011127     c                   if        fttddc <= §geddtfv
225600011127     c                   movel     §geditl       fttdiv
225700011127     c                   else
225800011127     c                   movel     §gedeur       fttdiv
225900011127     c                   end
226000011127      *
226100011121     C                   WRITE     fiftt000
226200931203    3C                   ENDIF
226300931203    2C                   ENDIF
226400931203     C*
226500931203     C                   ENDSR
226600931203     C*
226700030107      *---------------------------------------------------------------*
226800030107      * Creazione Fiftt00f x ritiri annullati - bolle "FY"
226900030107      *---------------------------------------------------------------*
227000030107     c     CrtFtt1       BegSr
227100030107
227200030107     c                   Z-Add     1             w
227300030107     c     BlpPdr        Lookup    Pdr(w)                                 30
227400030107
227500030107     c                   Movel     BlpPdr        Wpdr
227600030107     c                   If        BlpFpp = 'M'
227700030107     c                   Z-Add     1000          Wndc
227800030107     c                   Else
227900030107     c                   Z-Add     5000          Wndc
228000030107     c                   EndIf
228100030107     c                   Add       BlpNrt        Wndc
228200030107     c                   Movel     BlpDrt        Wdrt
228300030107     c     Kftt          Chain     Fiftt01l
228400030107      * Aggiornamento
228500030107If  1c                   If        %Found(Fiftt01l)
228600030107     c                   Add       1             FttTbs
228700030107     c                   Update    Fiftt000
228800030107
228900030107      * Solo se già valorizzata
229000030107     c                   If        FttFvl = 'V'
229100030107      * Richiamo pgm di svalorizzazione distinta autotrasportatore
229200030107     c                   Z-Add     Wpdr          Pa3Pdr
229300030107     c                   Movel     ComTsr        Pa3Tsr
229400030107     c                   Z-Add     Wndc          Pa3Ndc
229500030107     c                   Z-Add     Wdrt          Pa3Ddc
229600030107     c                   Movel     'N'           Pa3Fct
229700030107     c                   Movel     'N'           Pa3Fvd
229800030107     c                   Movel     'N'           Pa3Fvt
229900041122     C                   MOVEL     ' '           PA3sml
230000030107     c                   Movel     Param3        Kpjbu
230100030107     c                   Call      'FICNE2R'
230200030107     c                   Parm                    Kpjba
230300030107     c                   EndIf
230400030107
230500030107   x1c                   Else
230600030107      * Immissione
230700030107     c                   Clear                   Fiftt000
230800030107
230900030107     c                   Z-Add     1             FttTbs
231000030107     c                   Z-Add     Wpdr          FttPdr
231100030107     c                   Movel     Wpdr          FttFgs
231200030107     c                   Movel     ComTsr        FttTsr
231300030107     c                   Z-Add     Wndc          FttNdc
231400030107     c                   Z-Add     Wdrt          FttDdc
231500030107     c                   Movel     BlpFpp        FttFpp
231600030107     c                   If        BlpFpp = 'M'
231700030107     c                   Movel     Etm(w)        FttPep
231800030107     c                   Movel     Scm(w)        FttLvl
231900030107     c                   Else
232000030107     c                   Movel     Etp(w)        FttPep
232100030107     c                   Movel     Scp(w)        FttLvl
232200030107     c                   EndIf
232300030107      * In base alla data del documento aggancio la tab.GEDCN per
232400030107      * impostare la divisa della valorizzazione dei conteggi.
232500030107     c                   If        FttDdc <= §geddtfv
232600030107     c                   Movel     §geditl       FttDiv
232700030107     c                   Else
232800030107     c                   Movel     §gedeur       fttDiv
232900030107     c                   EndIf
233000030107
233100030107     c                   Write     Fiftt000
233200030107    1c                   EndIf
233300030107
233400030107     c                   EndSr
233500030107
233600931027     C*--- CHAIN TABEL -----------------------------------------------*
233700931027     C     CTAB          BEGSR
233800931028     C* EFFETTUO LA CHAIN IN TABEL SOLO SE CAMBIANO IL TIPO BOLLA E LA
233900931028     C*   FILIALE DI APPARTENENZA DEL PADRONCINO
234000950113     C                   MOVEL     WCPR          COMAPP
234100950113     C                   MOVE      BLPCBO        COMAPP            4
234200931124     C*
234300931028     C     SAVAPP        IFNE      COMAPP
234400931124     C                   MOVEL     ' '           ALF1              1
234500931027     C*
234600931028     C                   MOVEL     '8C'          COD
234700931108     C                   MOVEL     'R'           COMKEY            2
234800950109     C                   MOVE      BLPCBO        COMKEY
234900931027     C                   MOVEL     *BLANKS       KEY
235000931027     C                   MOVEL     COMKEY        KEY
235100950113     C                   MOVEL     WCPR          W0030             3 0
235200950113     C                   MOVE      W0030         KEY
235300931027     C*
235400931027     C     KTAB          CHAIN     TABEL                              16
235500931124     C* SE NON ESISTE OPPURE ESISTE MA E' ANNULLATO, RIPROVO CON LA
235600931124     C*   FILIALE 000 = TUTTE
235700931027     C     *IN16         IFEQ      *ON
235800931027     C     *IN16         OREQ      *OFF
235900931027     C     TBLFLG        ANDNE     ' '
236000931124     C*
236100931027     C                   MOVE      '000'         KEY
236200931027     C     KTAB          CHAIN     TABEL                              16
236300931124     C*
236400931124     C     *IN16         IFEQ      *ON
236500931124     C     *IN16         OREQ      *OFF
236600931124     C     TBLFLG        ANDNE     ' '
236700931124     C                   MOVEL     '1'           ALF1
236800931124     C                   ENDIF
236900931124     C*
237000931027     C                   ENDIF
237100931027     C*
237200931028     C                   MOVEL     COMAPP        SAVAPP            4
237300931028     C                   ENDIF
237400931027     C                   ENDSR
237500910909     C**-----------------------------------------------*
237600000111     C** RIEMPO CAMPI DEL FILE FISPP00T
237700910909     C**-----------------------------------------------*
237800910909     C     RIESPP        BEGSR
237900000111     C                   Z-ADD     BRVPES        SPPPES
238000950109     C                   MOVEL     BRVLNP        SPPLNP
238100950109     C                   MOVEL     BRVLNA        SPPLNA
238200950109     C                   MOVEL     BRVNRS        SPPNRS
238300950109     C                   MOVEL     BRVNSC        SPPNSC
238400950109     C                   MOVEL     BRVZNC        SPPZNC
238500950109     C                   MOVEL     BRVNPS        SPPNPS
238600950109     C                   MOVEL     BRVFGS        SPPFGS
238700950109     C                   MOVEL     BRVNFV        SPPNFV
238800940527     C                   MOVEL     WDTP          SPPDFV
238900950109     C                   Z-ADD     1             SPPNPG
239000950113     C                   MOVEL     BRVCAN        SPPCAN
239100950118     C                   MOVEL     BRVVUC        SPPVUC
239200990906     C                   MOVEL     BRVPUC        SPPPUC
239300961203     C     §TVDEF        IFEQ      'S'
239400961203     C                   MOVEL     'F'           SPPATR
239500961203     C                   ENDIF
239600000111     C                   Z-ADD     BRVDFS        SPPDFS
239700070123     C                   movel     BRVHMS        SPPHMS
239800000111     C                   Z-ADD     BRVDCS        SPPDCS
239900070123     C                   movel     BRVHCS        SPPHCS
240000000320     C                   Z-ADD     WDTR          SPPDTR
240100050707     C                   TIME                    W0140            14 0
240200000320     C                   MOVEL     W0140         SPPHTR
240300000111     C                   Z-ADD     BRVFLE        SPPFLE
240400910909     C                   ENDSR
240500910910     C**-----------------------------------------------*
240600000428     C** RIEMPO CAMPI DEL FILE FNFVA01T-FNFWA00T
240700910910     C**-----------------------------------------------*
240800910910     C     RIEFVA        BEGSR
240900950123     C                   MOVEL     1             FVANPG
241000950109     C                   MOVEL     FGVLNP        FVALNP
241100910910     C                   MOVEL     FGVNFV        FVANFV
241200910910     C                   MOVEL     FGVDFV        FVADFV
241300910910     C                   MOVEL     FGVLNA        FVALNA
241400921202     C                   MOVEL     FGVFFV        FVAFFV
241500921202     C                   MOVEL     FGVFF2        FVAFF2
241600921202     C                   MOVEL     FGVFLP        FVAFLP
241700921202     C                   MOVEL     FGVFL2        FVAFL2
241800951116     C                   MOVEL     FGVTRN        FVATRN
241900951120     C                   MOVEL     FGVTTR        FVATTR
242000951116     C                   MOVEL     FGVFRQ        FVATFM
242100951116     C                   MOVEL     FGVTRM        FVATRM
242200951116     C                   MOVEL     FGVTRR        FVATRR
242300951116     C                   MOVEL     FGVPDR        FVAPDR
242400951116     C                   MOVEL     FGVDPD        FVADPD
242500951116     C                   MOVEL     FGVCCN        FVACCN
242600130429     C                   MOVEL     FGVpmb        FVADCN
242700960123     C                   MOVEL     FGVDTP        FVADRP
242800960123     C                   MOVEL     FGVHMP        FVAHRP
242900960124     C* CALL PGM PER PRENDERE LA CAUSALE RITARDO
243000960124     C                   MOVEL     FGVLNP        SA2LNP
243100960124     C                   MOVEL     FGVNFV        SA2NFV
243200960124     C                   CLEAR                   SA2TLA
243300960124     C                   CLEAR                   SA2RTP
243400960124     C                   CALL      'FNLSA2R4'
243500960124     C                   PARM                    SA2TLA            1
243600960124     C                   PARM                    SA2LNP
243700960124     C                   PARM                    SA2NFV
243800960124     C                   PARM                    SA2RTP
243900960123     C*
244000960124     C                   MOVEL     SA2RTP        FVARTP
244100000428     C*
244200000428     C* RIEMPI FILE DI APPOGGIO FWA
244300000428     C     WFGW          IFEQ      'S'
244400000428     C                   CLEAR                   FNFWA000
244500000428     C                   MOVEL     FGWATB        FWAATB
244600000428     C                   MOVEL     FGWNFV        FWANFV
244700000428     C                   MOVEL     FGWLNP        FWALNP
244800000428     C                   MOVEL     FGWFGS        FWAFGS
244900000428     C                   MOVEL     FGWIPT        FWAIPT
245000000428     C                   MOVEL     FGWVIP        FWAVIP
245100000428     C                   MOVEL     FGWIES        FWAIES
245200000428     C                   MOVEL     FGWVES        FWAVES
245300000428     C                   MOVEL     FGWFF3        FWAFF3
245400000428     C                   MOVEL     FGWFF4        FWAFF4
245500000428     C                   MOVEL     FGWFL3        FWAFL3
245600000428     C                   MOVEL     FGWFL4        FWAFL4
245700000428     C                   ENDIF
245800960123     C*
245900960123     C* CONTROLLO SE IN ARRIVO OCCORRERA' SBINARE IL F.VIAGGIO PARTENZA
246000960123     C*   DAL FOGLIO ARRIVI
246100960123    1C     PA2FLG        IFEQ      'M'
246200960123     C                   CLEAR                   FVASBN
246300960123   X1C                   ELSE
246400960123     C                   MOVEL     'S'           FVASBN
246500960123    1C                   ENDIF
246600960123     C*
246700910910     C                   ENDSR
246800060710     C**-----------------------------------------------*
246900060710     C** SCrittura dati mancanti di FNFV4 in fnfv400F per arrivo
247000060710     C**-----------------------------------------------*
247100060710     c     SCRIVIFV4     begsr
247200060710     c
247300060710     C* SCRITTURA TIPO AUTOMEZZO E CATEGORIA TRAINO
247400060710     c* direttamente nel file fisico senza passare dal file fnfv400t
247500060710     c* verifico se esiste già
247600060710     C                   MOVEL     'P'           FV4EPA
247700060710     C                   MOVEL     'V'           FV4TRC
247800060710     C                   Z-ADD     FGVLNP        FV4LNP
247900060710     C                   Z-ADD     FGVNFV        FV4NFV
248000060710     C                   Z-ADD     *ZEROS        FV4LAI
248100060710     c     kfv4e         setll     fnfv401l                               23
248200060710    1c                   if        not *in23
248300060710     C                   CLEAR                   DSFV4V
248400060710     C                   MOVEL     *BLANKS       FV4ATB
248500060710     C                   MOVEL     FGVTMZ        §V4TMZ
248600060710     C                   MOVE      FGVSNP        §V4TTR
248700060710     C                   MOVEL     DSFV4V        FV4NOT
248800060710     C                   movel     'N'           FV4ft1
248900060710     C                   Z-ADD     DATEU         FV4dt1
249000060710     c* Per la sede verifico se da trasmettere
249100060710     c     fv4trc        lookup    fv4s                                   23
249200060710    2c                   if        *in23
249300060710     C                   clear                   FV4ft2
249400060710     C                   clear                   FV4dt2
249500060710   x2c                   else
249600060710     C                   movel     'N'           FV4ft2
249700060710     C                   Z-ADD     DATEU         FV4dt2
249800060710    2c                   endif
249900060710     C                   WRITE     FNFV4000
250000060710    1C                   ENDIF
250100060710     c                   ENDSR
250200970221     C**-----------------------------------------------*
250300970221     C* VEDO SE RIELABORARE LE STATISTICHE
250400970221     C**-----------------------------------------------*
250500970221     C     FNSTA         BEGSR
250600001027     C                   CLEAR                   WWLNA
250700001027     C**
250800021016     c                   If        WdtPar <= §SdfParU
250900970221     C                   MOVE      WDTPAR        SA2DTS            8 0
251000970221     C                   MOVE      'P'           SA2TRC            1
251100970221     C                   CALL      'FNLSA2R3'
251200970221     C                   PARM                    SA2TLA
251300970221     C                   PARM                    SA2TRC
251400970221     C                   PARM                    SA2DTS
251500970221     C                   PARM                    WWLNA             3 0
251600970221     C                   END
251700001027     C*
251800021016     c                   If        WdtPar <= §SdfTerU
251900970221     C                   MOVE      WDTPAR        SA2DTS
252000970221     C                   MOVE      'T'           SA2TRC
252100970221     C                   CALL      'FNLSA2R3'
252200970221     C                   PARM                    SA2TLA
252300970221     C                   PARM                    SA2TRC
252400970221     C                   PARM                    SA2DTS
252500970221     C                   PARM                    WWLNA             3 0
252600001027     C                   END
252700970221     C                   ENDSR
252800910909     C**-----------------------------------------------*
252900910909     C** TRASMISSIONE SPUNTE
253000910909     C**-----------------------------------------------*
253100910909     C     SPUNTE        BEGSR
253200960124     C                   CLEAR                   SA2TLA
253300980210     C                   SETOFF                                       0106
253400920505     C                   SETOFF                                       08
253500910909     C                   Z-ADD     0             SAVLNA            3 0
253600920703     C                   Z-ADD     0             SAVLNP            3 0
253700950323     C                   CLEAR                   SAVCBO
253800950203     C                   CLEAR                   SAVSPE
253900960708     C                   CLEAR                   WLAST
254000970314     C                   CLEAR                   WRKSPE
254100150730     c                   eval      wnpg=1
254200000320     C* PRENDO DATA/ORA SISTEMA PER L'AGGIORNAMENTO DI DATA E ORA
254300000320     C* TRASMISSIONE SPUNTE
254400000320     C                   TIME                    W0140            14 0
254500000320     C                   CLEAR                   WLBDAT
254600000320     C                   MOVE      W0140         G02DAT
254700000320     C                   CALL      'XSRDA8'
254800000320     C                   PARM                    WLBDAT
254900000320     C                   Z-ADD     G02INV        WDTR              8 0
255000931021     C*
255100910909     C* LETTURA PER FOGLIO VIAGGIO
255200070123     C     KBRV          SETLL     FNBRV02
255300070123     C     KBRV          READE     FNBRV02                                22
255400910909     C*
255500950215    1C     *IN22         DOWEQ     '0'
255600950215    2C     BRVFTR        IFEQ      *BLANKS
255700931220     C                   CLEAR                   WSPEN
255800931220     C                   CLEAR                   WSPET
255900950215     C                   CLEAR                   WAGG
256000980326     C                   CLEAR                   NCLU
256100960603     C**
256200960603     C* DETERMINO LNP DELLA BOLLA SE COLLO CON SERIE IN CONFERMA DA
256300960603     C* ALTRI AS
256400960604     C                   MOVE      'S'           WALCF
256500960604     C                   MOVEL     BRVLNP        KLNP
256600960604     C                   MOVEL     BRVLNA        KLNA
256700960604     C                   MOVEL     BRVNRS        KNRS
256800960604     C                   MOVEL     BRVNSC        KNSC
256900960603     C                   EXSR      RICLNP
257000910909     C**
257100910909     C** L I N E A   A R R I V O   C A M B I A T A
257200950215    3C     SAVLNA        IFNE      BRVLNA
257300920427     C* SCRIVO PER LIN ARRIVO CON SCARICO IL TERMINAL SPUNTE " " E "T"
257400920427     C   08
257500910912     C* SCRIVO LINEA DI ARRIVO SPUNTE "A" SE C'E' TRANSITO
257600980210    4COR 06SAVLNA        IFNE      0
257700910912     C                   EXSR      WTRLNA
257800950215    4C                   END
257900050203     c* mi salvo in una schiera parallela alle linee del foglio viaggio, se
258000050203     x*  locale, se presenti spunte transito
258100050204     c                   if        fgvlnp=flptfa  and savlna>0
258200050204     C                   Z-ADD     1             B
258300050204     C                   MOVEL     savLNA        COMPRO
258400050203     C     COMPRO        LOOKUP    FFV(B)                                 20
258500050203     C   20              MOVEL     winvialocale  winvloc(B)
258600050203     c                   endif
258700050203     c                   clear                   winvialocale
258800930712     C*
258900980210     C                   SETOFF                                       060801
259000930127     C                   Z-ADD     1             B                 3 0
259100950109     C                   MOVEL     BRVLNA        COMPRO            3
259200910912     C     COMPRO        LOOKUP    FFV(B)                                 20
259300910912     C*     TROVATA FIL SCARICO
259400930127     C   20              MOVEL     FLP(B)        COMLIN            3 0
259500910912     C* NON TROVATA --> METTO LINEA ARRIVO STESSA
259600950109     C  N20              MOVEL     BRVLNA        COMLIN
259700931022     C*
259800970811     C* F I L I A L E   S C A R I C O
259900051031     C                   CLEAR                   dslv55
260000051031     C****               MOVEL     'P'           d55TPT
260100051031     c                   z-add     fgvlnp        d55lnp
260200051031     C                   Z-ADD     COMLIN        d55LIN
260300051031     C                   Z-ADD     FGVDFV        d55DRF
260400051031     C                   CALL      'FNLV55R'
260500051031     C                   PARM                    dslv55
260600051031     C                   MOVEL     d55TFP        SAVFEL            3 0
260700051031     C                   MOVEL     d55TFP        VARX
260800051031     C                   MOVEL     d55TFP        VAR2X
260900051031     C                   Z-ADD     d55TFA        FLPTFA
261000950109     C*
261100021125     C                   SETON                                        01
261200910912     C* APERTURA MEMBRI
261300970811     C                   EXSR      CAEXSP
261400050204    4C***                END
261500920703     C*
261600980210     C* Reperisco terminal di arrivo della linea di arrivo
261700971105     C* Reperisco anche il terminal di partenza
261800130531     C                   CLEAR                   dslv55
261900130531     C                   MOVEL     ' '           d55TPT
262000130531     C                   Z-ADD     BRVLNA        d55LIN
262100130531     C                   Z-ADD     FGVLNP        d55LNP
262200130531     C                   Z-ADD     WDTP          d55DRF
262300130531     C                   CALL      'FNLV55R'
262400130531     C                   PARM                    dslv55
262500130531     C                   Z-ADD     d55TFA        LNATFA
262600130531     C                   Z-ADD     d55TFP        LNAFEL
262700051031     c
262800051031     c* Reperisco anche il terminal di arrivo della spunta letta
262900051031     C                   CLEAR                   dslv55
263000051031     C                   MOVEL     ' '           d55TPT
263100051031     C                   Z-ADD     BRVLNA        d55LIN
263200051031     C                   Z-ADD     wlnp          d55LNP
263300051031     C* Se fv poste gli passo il p.o. poste
263400051031     C     WFGW          IFEQ      'S'
263500051031     C     FGWFGS        ANDGT     *ZEROS
263600051031     C                   Z-ADD     FGWFGS        d55LNP
263700051031     C                   ENDIF
263800051031     C                   Z-ADD     WDTP          d55DRF
263900051031     C                   CALL      'FNLV55R'
264000051031     C                   PARM                    dslv55
264100051031     C                   Z-ADD     d55TFA        spuTFA            3 0
264200051031     c
264300031218     c* Determino se collo locale per sottomissione lr15
264400031218     c* Una volta acceso in92 non si deve rispegnere perchè basta che
264500031218     c* ce ne sia una per sottomettere
264600031218    1c     *in92         ifeq      *off
264700051031    3c     fgvlnp        ifeq      lnatfa
264800031218     c                   seton                                        92
264900031218    3c                   endif
265000031218    1c                   endif
265100980210     C**
265200980210     C* SE SCARICO >< DA LINEA DI ARRIVO
265300980210    4C     COMLIN        IFNE      BRVLNA
265400940107     C*
265500980210     C* 06  ON - T R A N S I T O
265600940107     C* SE SCARICO >< DA TERMINAL DI ARRIVO LINEA DI ARRIVO
265700980210    6C     COMLIN        IFNE      LNATFA
265800980210     C                   SETON                                        06
265900930312     C* APRO MEMBRI DEL TRANSITO
266000980210     C                   EXSR      TRANSI
266100980210     C                   EXSR      CAEX08
266200920703     C*
266300950215    6C                   END
266400950215    4C                   END
266500910909     C*
266600950109     C                   MOVEL     BRVLNA        SAVLNA
266700051031     C                   MOVEL     wlnp          SAVLNp
266800051031   x3c                   else
266900051031     c* se non cambiata la linea di arrivo vedo se cambiata la
267000051031     c*  linea di partenza spunte per la ricerca del tfa della spunta
267100051031    4c                   if        wlnp<>savlnp
267200051031     c* Reperisco anche il terminal di arrivo della spunta letta
267300051031     C                   CLEAR                   dslv55
267400051031     C                   MOVEL     ' '           d55TPT
267500051031     C                   Z-ADD     BRVLNA        d55LIN
267600051031     C                   Z-ADD     wlnp          d55LNP
267700051031     C* Se fv poste gli passo il p.o. poste
267800051031    5C     WFGW          IFEQ      'S'
267900051031     C     FGWFGS        ANDGT     *ZEROS
268000051031     C                   Z-ADD     FGWFGS        d55LNP
268100051031    5C                   ENDIF
268200051031     C                   Z-ADD     WDTP          d55DRF
268300051031     C                   CALL      'FNLV55R'
268400051031     C                   PARM                    dslv55
268500051031     C                   Z-ADD     d55TFA        spuTFA            3 0
268600051031     c                   z-add     wlnp          savlnp
268700051031    4C                   ENDIF
268800051031    3C                   ENDIF
268900950515     C*
269000960603     C                   CLEAR                   WTRAN
269100960603     C                   SETOFF                                       19
269200910912     C*
269300950515     C*  19 ON  - NON E' SPUNTA DA TRANSITO
269400950515     C*  19 OFF -     E' SPUNTA DA TRANSITO
269500960531     C     WLNP          LOOKUP    L1                                     19
269600980401     C**
269700980401     C* IGNORO OGNI AGGIORNAMENTO SE SI TRATTA DI SPUNTA DEFLUENZA
269800980401     C*  PERCHE' E' A TUTTI GLI EFFETTI UNA SPUNTA ARRIVI
269900980401   2AC     §TVDEF        IFNE      'S'
270000911018     C*
270100911106     C** T R A N S I T O
270200930203     C*
270300970221    3C     *IN19         IFEQ      *OFF
270400970828     C     WTAB5F        ANDEQ     ' '
270500950113     C*
270600911106     C**    PRENDO BOLLE E BLT DAL FILE DI TRANSITO
270700960603     C*     Non ripeto la chain se l'ho già fatta durante la ricerca
270800960603     C*     della lnp bolla
270900960603     C                   SELECT
271000960611     C     WOKBT         WHENEQ    *BLANKS
271100020606     C     KBtt37        CHAIN     FNBTT37L                           20
271200960611     C     WOKBT         WHENEQ    'S'
271300960603     C                   SETOFF                                       20
271400960603     C                   ENDSL
271500921014     C*
271600950515     C* CHIUDO ANOMALIA : "DISGUIDO"
271700970221     C*
271800970221   3AC     *IN20         IFEQ      *ON
271900970221     C                   Z-ADD     200           COMCAA
272000970221     C                   EXSR      CHIUAN
272100050927     c                   clear                   noinvia           1
272200130527     c* Per foglio locale non invio la spunta come disguido
272300050927     c*  (si tratta di colli di defluenza sparato per errore sul foglio
272400050927     c*   locale ma non si tratta nè di vera defluenza, nè di disguido)
272500050927     c                   if        wesisteanm=' '
272600050927     c                   eval      noinvia='S'
272700050927     c                   endif
272800970827     C* SOLO SE HO TROVATO L'ANOMALIA
272900970827     C  N21              Z-ADD     1             NCLU
273000921014     C**
273100970221  X3AC                   ELSE
273200970221     C**
273300950515     C                   MOVEL     'S'           WTRAN             1
273400921014     C*
273500950515     C* CHIUDO ANOMALIA : "BOLLA TRANSITO"
273600921014     C                   Z-ADD     125           COMCAA
273700921014     C                   EXSR      CHIUAN
273800921014     C*
273900950621    4C     BTTDUT        IFNE      0
274000950621     C                   MOVEL     'N'           WAGG              1
274100950621    4C                   ENDIF
274200950621     C*
274300950113     C                   MOVEL     FGVNFV        BTTNFV
274400970818     C* SE ERA PISTOLA FASULLA IMPOSTO 87
274500970827     C     BTTPUT        IFGT      0
274600970818     C     BTTPUT        LOOKUP    NPF                                    39
274700970827     C                   ELSE
274800970827     C                   SETOFF                                       39
274900970827     C                   ENDIF
275000970827     C**
275100970818     C     *IN39         IFEQ      *ON
275200950621     C                   MOVE      87            BTTPUT
275300950621     C                   ELSE
275400950113     C                   MOVEL     BRVNPS        BTTPUT
275500950621     C                   END
275600970827     C*
275700950706     C* Se le statistiche partito e a terra sono già state elaborate
275800950705     C* scrivo o aggiorno file FNSTA00F per rielaborazione statistiche
275900970221     C     BTTDUT        IFGT      *ZEROS
276000970221     C                   Z-ADD     BTTDUT        WDTPAR
276100970221     C                   EXSR      FNSTA
276200970221     C                   ENDIF
276300970221     C*
276400091027     c                   Z-ADD     WDTP          WDTPAR
276500970221     C                   EXSR      FNSTA
276600970221     C**
276700950113     C                   MOVEL     WDTP          BTTDUT
276800980407     C                   Z-ADD     1             NCLU
276900910927     C*
277000020606     C                   UPDATE    FNBTT037
277100950130     C*
277200950130     C* C A M B I A T A   S P E D I Z I O N E --> RICHAINO LA BOLLA
277300950215    4C     BTTSPE        IFNE      SAVSPE
277400970307     C*
277500970307     C* IMPOSTO LA SPEDIZIONE PERCHE' A CAMBIO ULTIMO COLLO SCRIVO AGB
277600970307     C     WRKSPE        IFEQ      0
277700970307     C                   Z-ADD     BTTSPE        WRKSPE
277800970307     C                   MOVEL     'T'           WRKTBO
277900970307     C                   ENDIF
278000950130     C*
278100020605     C     KBTT3         CHAIN     FNBTP11L                           21
278200930127     C*
278300091027     c* BTPDT3  contiene il terminal a cui trasmetto
278400020606     c                   z-add     btpdt3        savdt3            3 0
278500020606     c                   z-add     comlin        btpdt3
278600091027     c
278700950109     C                   MOVEL     'T'           BTPFT1
278800950113     C                   Z-ADD     DATEU         BTPDT1
278900950109     C                   MOVEL     'N'           BTPFT2
279000950113     C                   Z-ADD     DATEU         BTPDT2
279100950109     C                   MOVEL     'N'           BTPFT3
279200940110     C*
279300940110     C* CALCOLO PESO E VOLUME UNITARIO DEL COLLO
279400950110     C                   EXSR      CALUNT
279500931022     C*
279600911011     C* AGGIORNO DATA PARTENZA DAL TRANSITO
279700950215    5C     BTPDUT        IFEQ      0
279800931220     C* MUOVO 1 NEL CAMPO DELLE SPEDIZIONI
279900931220     C                   Z-ADD     1             WSPET
280000950109     C                   MOVEL     WDTP          BTPDUT
280100950621     C                   ELSE
280200950621     C                   MOVEL     'T'           AGBTBO
280300950621     C                   MOVE      BTPAAS        AGBAAS
280400950621     C                   MOVE      BTPLNP        AGBLNP
280500950621     C                   MOVE      BTPNRS        AGBNRS
280600950621     C                   MOVE      BTPNSP        AGBNSP
280700950621     C                   MOVEL     'DT'          AGBAGB
280800950630     C                   WRITE     FNAGB000                             39
280900950215    5C                   END
281000950109     C*
281100950109     C                   UPDATE    FNBTP000
281200950130     C*
281300950130     C                   MOVEL     BTTSPE        SAVSPE
281400910927     C*
281500091027     C* SE TRANSITO PER TRANSITO : INVIO DI NUOVO BOLLA
281600020606    5C   06SAVdt3        IFNE      COMLIN
281700031219     c* Se p.o. di scarico è anche il ter-part.della lnp bolla non devo
281800031219     c* ritrasmettere
281900130531     c                   clear                   dslv55
282000130531     c                   movel     'P'           d55tpt
282100130531     c                   z-add     wlnp          d55lin
282200130531     c                   z-add     wdtp          d55drf
282300130531     c                   call      'FNLV55R'
282400130531     c                   parm                    dslv55
282500130531     c     comlin        ifne      d55tfp
282600950109     C                   EXSR      WTRBTP
282700130531     C                   endif
282800950808    5C                   END
282900950808    4C                   ENDIF
283000970307     C**
283100970307     C* A CAMBIO ULTIMO COLLO SCRIVO
283200970307     C                   MOVEL     'T'           BTTTBO
283300970307     C     BTTKEY        IFNE      WRKKEY
283400970307     C* SCRIVO IL FILE FNAGB01L PER AGGIORNARE DATA ULTIMO COLLO
283500970307     C                   MOVEL     WRKTBO        AGBTBO
283600970307     C                   MOVEL     WRKAAS        AGBAAS
283700970307     C                   MOVEL     WRKLNP        AGBLNP
283800970307     C                   MOVEL     WRKNRS        AGBNRS
283900970307     C                   MOVEL     WRKNSP        AGBNSP
284000970307     C                   MOVEL     'DT'          AGBAGB
284100970307     C                   WRITE     FNAGB000                             39
284200970307     C**
284300970307     C                   MOVEL     BTTKEY        WRKKEY
284400970307     C                   ENDIF
284500970221   3AC                   ENDIF
284600970221    3C                   END
284700910912     C**
284800991022     C** L N P
284900910927     C**
285000960604    3C     *IN19         IFEQ      *ON
285100970827     C     WTAB5F        OREQ      'S'
285200960603     C*     Non ripeto la chain se l'ho già fatta durante la ricerca
285300960603     C*     della lnp bolla
285400960603     C                   SELECT
285500960611     C     WOKBL         WHENEQ    *BLANKS
285600960603     C     KBRV4         CHAIN     FNBLT27L                           20
285700960611     C     WOKBL         WHENEQ    'S'
285800960603     C                   SETOFF                                       20
285900960603     C                   ENDSL
286000911108     C*
286100921014     C* CREO   ANOMALIA : "MANCA BOLLA"
286200991022     C***
286300991022   3AC     *IN20         IFEQ      *ON
286400911108     C                   Z-ADD     105           COMCAA
286500990517     C     KANM3         SETLL     FNANM                                  21
286600911108     C  N21              EXSR      CRTANM
286700970221     C*
286800991022     C* NON SOMMO I COLLI  MANCA REC BOLLA
286900970221     C*  PERCHE' LO FARO' AL MOMENTO DELLA
287000970221     C*  BORDERIZZAZIONE DELLA BOLLA, SE E' UN MANCA REC BOLLA VERO
287100970221     C*  PER IL PROSPETTO NOLI (LE STATISTICHE HANNO UN LORO GIRO
287200970827  X3AC                   ELSE
287300991022     C* SE ESISTE NELL'ALTRO S.I. DO IL DISGUIDO E BASTA
287400040531   3BC**   WALTSI        IFEQ      'S'
287500040531     C**                 MOVEL     'D'           FLG
287600040531     C**                 EXSR      CALL44
287700040531  X3BC**                 ELSE
287800911108     C*
287900950515     C                   CLEAR                   WTRAN
288000110907     c
288100110907     c                   setoff                                       37
288200921014     C*
288300950515     C* CREO   ANOMALIA : "MANCA RECORD BOLLA BORDERIZZATA"
288400040625     C* NON LO FACCIO SE bolla poste
288500110907    4c                   if        bltnfv=0 or brvnps =88 or
288600110907     c                             brvnps=90
288700040603     c* Verifico se bolla con merce di valore da memorizzare nella
288800040603     c*  anomalia
288900040603     c     kar5          chain(N)  fiar501l
289000040603     c                   if        %found(fiar501l)
289100040603     c                   movel     ar5uni        dar5gen
289200040603     c                   else
289300040603     c                   clear                   dar5gen
289400040603     c                   endif
289500110907
289600110907    5C     BLTNFV        IFEQ      0
289700110907     C                   Z-ADD     110           COMCAA
289800040603     C     KANM3         SETLL     FNANM                                  21
289900911108     C  N21              EXSR      CRTANM
290000110907    5C                   END
290100110907     c*
290200110907     C* CREO   ANOMALIA : "COLLO MAI AFFIDATO"
290300110907    5c                   if        brvnps =88 or brvnps=90
290400110907     C* PER SICUREZZA VERIFICO CHE NON SIA PRESENTE UNA QUALSIASI SPUNTA
290500110907     C* IN ALTRA FILIALE
290600110907     c                   clear                   EsSpunta          1
290700110907     C                   MOVEL     DSBRV         DSBRVS
290800110907     C     KBRV4         setll     FNbrv07l
290900110907     C     KBRV4         reade     FNbrv07l
291000110907     c                   dow       not %eof(fnbrv07l)
291100110907     c* Escludo le pistole completamente fasulle
291200110907     c                   if        brvnps<>88 and brvnps<>90
291300110907     c                   eval      EsSpunta='S'
291400110907     c                   leave
291500110907     c                   endif
291600110907     c
291700110907     C     KBRV4         reade     FNbrv07l
291800110907     c                   enddo
291900110907     c
292000110907     C* Reimposto campi di fnbrv con dati salvati
292100110907     C                   MOVEL     DSBRVS        DSBRV
292200110907     C*
292300110908    6c                   if        EsSpunta=' '
292400110907     C                   Z-ADD     115           COMCAA
292500110907     C     KANM3         SETLL     FNANM                                  21
292600110907     C  N21              EXSR      CRTANM
292700110907    6c                   endif
292800110907    5c                   endif
292900110907     c
293000110907    4c                   endif
293100911108     C*
293200921014     C* CHIUDO ANOMALIA : "BOLLA PARTENZA"
293300921014     C                   Z-ADD     120           COMCAA
293400020412     C                   EXSR      CHIUAN
293500911108     C*
293600911108     C*
293700950621    4C     BLTDFV        IFNE      0
293800950621     C                   MOVEL     'N'           WAGG
293900950621    4C                   ENDIF
294000950621     C*
294100930520     C                   MOVEL     FGVNFV        BLTNFV
294200970221     C*
294300950706     C* Se le statistiche partito e a terra isono già state elaborate
294400950705     C* scrivo o aggiorno file FNSTA00F per rielaborazione statistiche
294500970221     C     BLTDFV        IFGT      *ZEROS
294600970221     C                   Z-ADD     BLTDFV        WDTPAR
294700970221     C                   EXSR      FNSTA
294800970221     C                   ENDIF
294900970221     C**
295000950707     C                   Z-ADD     WDTP          WDTPAR
295100970221     C                   EXSR      FNSTA
295200970221     C*
295300950706     C                   MOVEL     WDTP          BLTDFV
295400970627     C* PULISCO IL CAMPO CHE MI SEGNALA UNA EVENTUALE MANCANZA DI COLLO
295500970627     C* IMPORT DATOMI DAL PARTNER
295600970627     C                   CLEAR                   BLTFL1
295700970818     C* SE ERA PISTOLA FASULLA IMPOSTO 87
295800970827     C     BLTNPP        IFGT      0
295900970818     C     BLTNPP        LOOKUP    NPF                                    39
296000970827     C                   ELSE
296100970827     C                   SETOFF                                       39
296200970827     C                   ENDIF
296300970827     C**
296400970818     C     *IN39         IFEQ      *ON
296500950621     C                   MOVE      87            BLTNPP
296600950621     C                   ELSE
296700950110     C                   MOVEL     BRVNPS        BLTNPP
296800950623     C                   END
296900910927     C                   MOVEL     COMLIN        BLTFLP
297000980407     C                   Z-ADD     1             NCLU
297100910927     C*
297200950110     C                   UPDATE    FNBLT027
297300950130     C*
297400950130     C* C A M B I A T A   S P E D I Z I O N E --> RICHAINO LA BOLLA
297500950215    4C     BLTSPE        IFNE      SAVSPE
297600970307     C*
297700970307     C* IMPOSTO LA SPEDIZIONE PERCHE' A CAMBIO ULTIMO COLLO SCRIVO AGB
297800970307     C     WRKSPE        IFEQ      0
297900970307     C                   Z-ADD     BLTSPE        WRKSPE
298000970307     C                   MOVEL     'P'           WRKTBO
298100970307     C                   ENDIF
298200950130     C*
298300151022     C     KBLT3         CHAIN     FNBLP01L
298400940107     C*
298500940107     C* CALCOLO PESO E VOLUME UNITARIO DEL COLLO
298600940107     C                   EXSR      CALUNI
298700931022     C*
298800910927     C* SE E' IL PRIMO COLLO CHE PARTE METTO DATA FV
298900950215    5C     BLPDPC        IFEQ      0
299000931220     C* MUOVO 1 NEL CAMPO DELLE SPEDIZIONI
299100931220     C                   Z-ADD     1             WSPEN
299200950124     C                   MOVEL     WDTP          BLPDPC
299300950215    5C                   END
299400910930     C* FIL TRANSITO
299500930127     C                   MOVEL     BLPFLP        SAVFLP            3 0
299600910930     C                   MOVEL     COMLIN        BLPFLP
299700910930     C* AGGIORNO
299800151022     c                   if        %found(fnblp01l)
299900950110     C                   UPDATE    FNBLP001
300000151022     c                   else
300100151022     c                   exsr      sr_errore
300200151022     c                   endif
300300910927     C*
300400910930     C** LNP PER TRANSITO
300500910927     C**   SE NON CORRISPONDE A TRANSITO DELLA BOLLA SPEDISCO LA BOLLA
300600920505     C**   PERCHE' POSSO MANDARE VIA LA BOLLA PER UNA CERTA LINEA E
300700930707     C**   IL GIORNO DOPO I COLLI PER UN'ALTRA LINEA (RIMANDO BOLLA)
300800980210    5C   06SAVFLP        IFNE      0
300900920512     C     SAVFLP        ANDNE     COMLIN
301000930312     C     SAVFLP        OREQ      0
301100930312     C     BLPLNA        ANDNE     COMLIN
301200091027     C
301300091027     c* Imposto il numero foglio viaggio  di chiusura
301400091027     c                   eval      blpnfv=fgv2
301500950110     C                   WRITE     FNBLPT00
301600080423     C
301700040310    5c                   ENDIF
301800950130     C*
301900950130     C*
302000950130     C                   MOVEL     BLTSPE        SAVSPE
302100950215    4C                   ENDIF
302200970307     C*
302300970307     C* A CAMBIO ULTIMO COLLO SCRIVO
302400970307     C                   MOVEL     'P'           BLTTBO
302500970307     C     BLTKEY        IFNE      WRKKEY
302600970307     C* SCRIVO IL FILE FNAGB01L PER AGGIORNARE DATA ULTIMO COLLO
302700970307     C                   MOVEL     WRKTBO        AGBTBO
302800970307     C                   MOVEL     WRKAAS        AGBAAS
302900970307     C                   MOVEL     WRKLNP        AGBLNP
303000970307     C                   MOVEL     WRKNRS        AGBNRS
303100970307     C                   MOVEL     WRKNSP        AGBNSP
303200970307     C                   MOVEL     'DT'          AGBAGB
303300970307     C                   WRITE     FNAGB000                             39
303400970307     C**
303500970307     C                   MOVEL     BLTKEY        WRKKEY
303600970307     C                   ENDIF
303700950130     C*
303800040531   3BC**                 ENDIF
303900991022   3AC                   ENDIF
304000960403    3C                   ENDIF
304100980326   2AC                   ENDIF
304200910927     C**
304300910912     C** I N V I O   L A   S P U N T A
304400910927     C**
304500950516    3C   01              DO
304600000111     C                   CLEAR                   FISPP000
304700950215     C                   CLEAR                   SPPATR
304800950215     C                   CLEAR                   SPPFLP
304900910912     C* SE TRANSITO PER LNA '--> METTO FLAG "T"
305000970814     C*  SE E' LINEA/SERIE COMFERMABILE DA SIMFEL NON CREO IL DISGUIDO
305100950515     C     *IN19         IFEQ      *OFF
305200970814     C     WTAB5F        ANDEQ     ' '
305300040401     c* Se spunta che invio al p.o. di transito memorizzo il p.o. di scarico
305400040401     c   06              z-add     comlin        sppflp
305500950515     C  N06              MOVEL     'T'           SPPATR
305600971105     C  N06              MOVEL     FGVLNP        SPPFLP
305700940322     C* SE E' UN COLLO IN DISGUIDO METTO "D"
305800950515     C     WTRAN         IFEQ      ' '
305900050927     c* non si tratta di disguido se foglio locale e non esiste l'anom.200
306000050927     c                   if        fgvlnp<>flptfa or
306100050927     c                             (fgvlnp=flptfa and noinvia=' ')
306200950515     C                   MOVEL     'D'           SPPATR
306300971105     C                   MOVEL     FGVLNP        SPPFLP
306400050927     C                   endif
306500050927     c
306600050927     C                   ENDIF
306700020607     c                   else
306800020607     c* Se spunta che invio al p.o. di transito memorizzo il p.o. di scarico
306900020607     c   06              z-add     comlin        sppflp
307000950515     C                   ENDIF
307100991025     C* SE BOLLA IN ALTRO S.I. INVIO LA SPUNTA COME "D"
307200040531     C**   WALTSI        IFEQ      'S'
307300040531     C**                 MOVEL     'D'           SPPATR
307400040531     C**                 MOVEL     FGVLNP        SPPFLP
307500040531     C**                 ENDIF
307600910912     C*
307700910912     C                   EXSR      RIESPP
307800910910     C*
307900050204     c* Se sul foglio locale ho memorizzato spunte transito, devo
308000050204     c*  inviare il foglio per l'abbinamento
308100050204     c                   if        FGVLNP<>flpTFA Or
308200050204     c                             (fgvlnp=flptfa and sppatr<>' ' and
308300051031     c                              sppatr<>'F' and not *in06 and
308400051031     c                              sputfa<>flptfa)
308500050204     c                   if        fgvlnp=flptfa
308600050204     C                   MOVEL     'S'           winvialocale
308700050204     c                   endif
308800050204     c
308900000111     C                   WRITE     FISPP000
309000950215    3C                   END
309100050203    3C                   END
309200931022     C*
309300950110     C* AGGIORNO FNFVD00F
309400950215     C     WAGG          IFEQ      ' '
309500931022     C                   EXSR      AGGFVD
309600950215     C                   ENDIF
309700931022     C*
309800911107     C* AGGIORNO SPUNTA
309900950110     C                   MOVEL     'T'           BRVFTR
310000000320     C                   Z-ADD     WDTR          BRVDTR
310100050707     c                   time                    w0140
310200000320     C                   MOVEL     W0140         BRVHTR
310300070123     C                   UPDATE    FNBRV02
310400980728     C                   ELSE
310500070123     C* SE NE LEGGO UNA CON CODICE ANOMALIA E LA DEVO SCRIVERE IN FNBRV
310600980728     C*  SOLO SE SI TRATTA DI UN FOGLIO DI DEFLUENZA
310700980728     C     BRVCAN        IFEQ      'E'
310800070123     C     kbrve         setll     fNbrve                                 99
310900070123     C  n99              WRITE     FNBRVE
311000980728     C                   ENDIF
311100950215    2C                   END
311200070123     C     KBRV          READE     FNBRV02                                22
311300910910     C*
311400950215    1C                   END
311500910910     C*
311600920427     C* SCRIVO PER LIN ARRIVO CON SCARICO IL TERMINAL SPUNTE " " E "T"
311700920427     C   08
311800910912     C* SCRIVO PER ULTIMA LINEA ARRIVO
311900980210     COR 06SAVLNA        IFNE      0
312000960708     C                   MOVE      'S'           WLAST             1
312100910912     C                   EXSR      WTRLNA
312200910910     C                   END
312300050204     c* mi salvo in una schiera parallela alle linee del foglio viaggio, se
312400050204     x*  locale, se presenti spunte transito
312500050204     c                   if        fgvlnp=flptfa  and savlna>0
312600050204     C                   Z-ADD     1             B
312700050204     C                   MOVEL     savLNA        COMPRO
312800050204     C     COMPRO        LOOKUP    FFV(B)                                 20
312900050204     C   20              MOVEL     winvialocale  winvloc(B)
313000050204     c                   endif
313100970307     C**
313200970307     C* SCRIVO L'ULTIMO FNAGB
313300970307     C     WRKSPE        IFGT      0
313400970307     C                   MOVEL     WRKTBO        AGBTBO
313500970307     C                   MOVEL     WRKAAS        AGBAAS
313600970307     C                   MOVEL     WRKLNP        AGBLNP
313700970307     C                   MOVEL     WRKNRS        AGBNRS
313800970307     C                   MOVEL     WRKNSP        AGBNSP
313900970307     C                   MOVEL     'DT'          AGBAGB
314000970307     C                   WRITE     FNAGB000                             39
314100970307     C                   ENDIF
314200910909     C*
314300910909     C                   ENDSR
314400940107     C*
314500950110     C* CALCOLO VOLUME E PESO UNITARI FNBTT--------------------------*
314600950110     C     CALUNT        BEGSR
314700940107     C* COLLI / PESO / VOLUME UNITARI
314800940107     C                   Z-ADD     1             NCLU
314900030902     c*
315000050112     C     BtPNCR        IFEQ      BtPNCL
315100021111     C                   Z-ADD     BtPVLC        Wfvdvol
315200021111     C*
315300021111   X4C                   ELSE
315400021111    5C     BtPVLC        IFGE      BtPVLb
315500021111     C                   Z-ADD     BtPVLC        Wfvdvol
315600021111   X5C                   ELSE
315700021111     C                   Z-ADD     BtPVLb        Wfvdvol
315800021111    5C                   ENDIF
315900021111    4C                   ENDIF
316000021111     c
316100021111     C     wfvdvol       DIV(H)    BtPNCL        VOLU
316200030902     c*
316300050112     C     BtPNCp        ifeq      BtPNCL
316400021111     C                   Z-ADD     BtPpkC        Wfvdpes
316500021111     C*
316600021111   X4C                   ELSE
316700021111    5C     BtPpkC        IFGE      BtPpkb
316800021111     C                   Z-ADD     BtPpkC        Wfvdpes
316900021111   X5C                   ELSE
317000021111     C                   Z-ADD     BtPpkb        Wfvdpes
317100021111    5C                   ENDIF
317200021111    4C                   ENDIF
317300030902     c*
317400021111     C     wfvdpes       DIV(H)    BtPNCL        PKGU
317500940107     C*
317600940107     C                   ENDSR
317700950110     C*
317800950110     C* CALCOLO VOLUME E PESO UNITARI FNBLT--------------------------*
317900950110     C     CALUNI        BEGSR
318000950110     C* COLLI / PESO / VOLUME UNITARI
318100950110     C                   Z-ADD     1             NCLU
318200021111     c
318300050112     C     BLPNCR        ifeq      BLPNCL
318400021111     C                   Z-ADD     BLPVLC        Wfvdvol
318500021111     C*
318600021111   X4C                   ELSE
318700021111    5C     BLPVLC        IFGE      BLPVLF
318800021111     C                   Z-ADD     BLPVLC        Wfvdvol
318900021111   X5C                   ELSE
319000021111     C                   Z-ADD     BLPVLF        Wfvdvol
319100021111    5C                   ENDIF
319200021111    4C                   ENDIF
319300030902     c*
319400021111     C     wfvdvol       DIV(H)    BLPNCL        VOLU
319500021111     c
319600050112     C     BLPNCp        ifeq      BLPNCL
319700021111     C                   Z-ADD     BLPpkC        Wfvdpes
319800021111     C*
319900021111   X4C                   ELSE
320000021111    5C     BLPpkC        IFGE      BLPpkF
320100021111     C                   Z-ADD     BLPpkC        Wfvdpes
320200021111   X5C                   ELSE
320300021111     C                   Z-ADD     BLPpkF        Wfvdpes
320400021111    5C                   ENDIF
320500021111    4C                   ENDIF
320600030902     c*
320700021111     C     wfvdpes       DIV(H)    BLPNCL        PKGU
320800950110     C*
320900950110     C                   ENDSR
321000930708     C*
321100950110     C*--- AGGIORNO CAMPI DI FNFVD -----------------------------------*
321200931022     C     AGGFVD        BEGSR
321300931022     C*
321400980210    1C     NCLU          IFGT      0
321500950110     C* ESCLUSE LE BOLLE RECUPERO: MA DAL MOMENTO CHE NON HANNO COLLI.
321600950110     C*  NON MI PREOCCUPO
321700970221     C* ESCLUDO IL CODICE SOCIALE 2 ===> SDI  SE TROVATA BOLLA
321800980210    2C     *IN19         IFEQ      *ON
321900970221     C     *IN20         ANDEQ     *OFF
322000070223     C***  BLPSCL        ANDNE     '2'
322100970221     C* ELABORO SE NON TROVATA BOLLA
322200970221     C     *IN19         OREQ      *ON
322300970221     C     *IN20         ANDEQ     *ON
322400980210     C* ELABORO SE E' COLLO DISGUIDO O TRANSITO
322500950110     C     *IN19         OREQ      *OFF
322600970221     C*
322700950110     C* TIPO RECORD 1= BOLLE DELL'AREA    2=BOLLE TRANSITO
322800950515     C                   MOVEL     '2'           WTRC
322900950515     C*
323000980210    3C     *IN19         IFEQ      *ON
323100950515     C                   MOVEL     '1'           WTRC
323200980210     C                   ELSE
323300980210     C     WLNP          CHAIN     AZORG01L                           18
323400980210    4C     ORGDIT        IFEQ      'SDI'
323500980210     C                   MOVEL     '3'           WTRC
323600980210    4C                   ENDIF
323700980210    3C                   ENDIF
323800931214     C*
323900950110     C     KFVD2         CHAIN     FNFVD000                           18
324000950110     C   18              CLEAR                   FNFVD000
324100950110     C*
324200950110     C* COLLI
324300950110     C                   ADD       NCLU          FVDCLA
324400931026     C*
324500950515     C* SE TROVATA LA BOLLA CALCOLO PER BENE IL PARTITO
324600980210    3C     WTRAN         IFEQ      'S'
324700970221     C     *IN19         OREQ      *ON
324800970221     C     *IN20         ANDEQ     *OFF
324900950515     C                   EXSR      RIEFVD
325000980210    3C                   ENDIF
325100931022     C*
325200980210    3C     *IN18         IFEQ      *ON
325300931022     C* SOLO PER IMMISSIONE
325400931022     C                   Z-ADD     FGVLNP        FVDLNP
325500931022     C                   Z-ADD     FGV2          FVDNFV
325600931022     C                   Z-ADD     SAVLNA        FVDLNA
325700931022     C                   Z-ADD     COMLIN        FVDFLP
325800950110     C                   MOVEL     WTRC          FVDTRC
325900931022     C*
326000950110     C                   WRITE     FNFVD000
326100931022     C*
326200931022     C                   ELSE
326300931022     C*
326400931022     C                   MOVEL     ' '           FVDFTR
326500931022     C                   Z-ADD     0             FVDDTR
326600931022     C*
326700950110     C                   UPDATE    FNFVD000
326800980210    3C                   ENDIF
326900931022     C*
327000980210    2C                   ENDIF
327100980210    1C                   ENDIF
327200931022     C                   ENDSR
327300931022     C*
327400950110     C*--- RIEMPO CAMPI DI FNFVD -------------------------------------*
327500931022     C     RIEFVD        BEGSR
327600931022     C***
327700931022     C***  19 OFF  -   T  R  A  N  S  I  T  I
327800931022     C***
327900950515     C     WTRAN         IFEQ      'S'
328000930708     C* PESO
328100950113     C     BTPPKB        IFGT      3500
328200030213      * Peso medio
328300030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
328400950123     C                   ADD       PKGU          FVDKOA
328500930708     C                   ELSE
328600030213      * Peso medio
328700030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
328800950123     C                   ADD       PKGU          FVDKFA
328900930708     C                   ENDIF
329000950110     C* VOLUME
329100950110     C                   Z-ADD     1             Z                 3 0
329200950110     C     BTPTSP        LOOKUP    TSP(Z)                                 31
329300950110     C*
329400950110     C* NON TROVATO TIPO SERVIZIO --> LO IMPOSTO IN SCHIERA
329500950110     C     *IN31         IFEQ      *OFF
329600950110     C                   Z-ADD     1             Z
329700950110     C     ' '           LOOKUP    TSP(Z)                                 31
329800950110     C   31              MOVEL     BTPTSP        TSP(Z)
329900950110     C  N31              Z-ADD     1             Z
330000950110     C                   ENDIF
330100950110     C*
330200030213      * Volume medio
330300030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
330400950110     C                   ADD       VOLU          VOL(Z)
330500930708     C*
330600950110     C                   ADD       WSPET         FVDSNA
330700931022     C*
330800931022     C                   ELSE
330900931022     C***
331000970221     C***     A  R  E  A
331100931022     C***
331200930709     C*
331300930708     C* PESO
331400130530     C**   BLPPKF        IFGT      3500
331500130530     c                   if        wfvdpes>3500
331600030213      * Peso medio
331700030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
331800931022     C                   ADD       PKGU          FVDKOA
331900931022     C                   ELSE
332000030213      * Peso medio
332100030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
332200931022     C                   ADD       PKGU          FVDKFA
332300930708     C                   ENDIF
332400930708     C*
332500950110     C* VOLUME
332600950110     C                   Z-ADD     1             Z                 3 0
332700950110     C     BLPTSP        LOOKUP    TSP(Z)                                 31
332800950110     C*
332900950110     C* NON TROVATO TIPO SERVIZIO --> LO IMPOSTO IN SCHIERA
333000950110     C     *IN31         IFEQ      *OFF
333100950110     C                   Z-ADD     1             Z
333200950110     C     ' '           LOOKUP    TSP(Z)                                 31
333300950110     C   31              MOVEL     BLPTSP        TSP(Z)
333400950110     C  N31              Z-ADD     1             Z
333500950110     C                   ENDIF
333600950110     C*
333700030213      * Volume medio
333800030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
333900950110     C                   ADD       VOLU          VOL(Z)
334000950110     C                   ADD       WSPEN         FVDSNA
334100931022     C*
334200931022     C                   ENDIF
334300930708     C                   ENDSR
334400930708     C*
334500930709     C*--- CHAIN TABEL -----------------------------------------------*
334600930709     C     CTABEL        BEGSR
334700931021     C* EFFETTUO LA CHAIN IN TABEL SOLO SE CAMBIA IL TIPO BOLLA
334800950109     C     SAVCBO        IFNE      BLPCBO
334900930709     C                   MOVEL     '3A'          COD
335000930709     C                   MOVEL     *BLANKS       KEY
335100950109     C                   MOVEL     BLPCBO        KEY
335200930709     C     KTAB          CHAIN     TABEL                              31
335300931202     C     *IN31         IFEQ      *OFF
335400931202     C                   MOVEL     TBLUNI        DS3A
335500931202     C                   MOVEL     §3ATB1        COMTB1            1
335600931202     C                   ELSE
335700931202     C                   MOVE      ' '           §3ARBL
335800950413     C                   MOVEL     ' '           §3ABSD
335900950413     C                   MOVEL     ' '           §3ABFI
336000931202     C                   ENDIF
336100931202     C*
336200950109     C                   MOVEL     BLPCBO        SAVCBO
336300930709     C                   ENDIF
336400930709     C                   ENDSR
336500950113     C*
336600950113     C* RITRASMETTO IL RECORD TRANSITO ------------------------------*
336700950113     C     WTRBTP        BEGSR
336800950113     C                   MOVEL     BTPAAS        BLPAAS
336900950113     C                   MOVEL     BTPLNP        BLPLNP
337000950113     C                   MOVEL     BTPNRS        BLPNRS
337100950113     C                   MOVEL     BTPNSP        BLPNSP
337200950113     C                   MOVEL     BTPMGS        BLPMGS
337300950113     C                   MOVEL     BTPCBO        BLPCBO
337400950113     C                   MOVEL     BTPFNS        BLPFNS
337500950113     C                   MOVEL     BTPLNA        BLPLNA
337600950113     C                   MOVEL     BTPZNC        BLPZNC
337700950113     C                   MOVEL     BTPRSM        BLPRSM
337800950113     C                   MOVEL     BTPRSD        BLPRSD
337900950113     C                   MOVEL     BTPTSP        BLPTSP
338000950113     C                   MOVEL     BTPNCL        BLPNCL
338100950113     C                   MOVEL     BTPPKB        BLPPKB
338200950113     C                   MOVEL     BTPFVB        BLPFVB
338300950113     C                   MOVEL     BTPVLB        BLPVLB
338400950113     C                   MOVEL     BTPFLS        BLPFLS
338500950113     C                   MOVEL     BTPNCD        BLPNCD
338600950113     C                   MOVEL     BTPNCA        BLPNCA
338700950113     C                   MOVEL     BTPXCO        BLPXCO
338800950113     C                   MOVEL     BTPDBR        BLPDBR
338900021127     C                   MOVEL     BTPFLE        BLPFLE
339000950113     C                   MOVEL     BTPTFP        BLPTFP
339100950113     C                   MOVEL     BTPFEA        BLPFEA
339200950113     C                   MOVEL     BTPTFA        BLPTFA
339300091027     c* nei campi BLPNFV e BLPFLE imposto il foglio di partenza dei
339400091027     c*  colli per poter leggere queste bolle nel pgm della statistica
339500091027     c*    MC in arrivo
339600091027     c                   eval      blpnfv=fgv2
339700091027     c                   eval      blpfle=fgvlnp
339800091027     c
339900950113     C                   WRITE     FNBLPT00
340000950113     C                   ENDSR
340100911004     C**-----------------------------------------------*
340200911004     C** TRASFERISCO FOGLI VIAGGIO
340300911004     C**-----------------------------------------------*
340400911004     C     FVIAGG        BEGSR
340500150730     C                   Z-ADD     1             wnpg
340600150730     C                   Z-ADD     1             B
340700911004     C                   Z-ADD     0             X
340800930929     C                   CLEAR                   LP2
340900911106     C                   EXSR      RIEFVA
341000020403     c                   time                    w0060             6 0
341100911004     C*
341200940620    1C     FLP(B)        DOWGT     *ZEROS
341300920928     C*
341400960212     C* CONTROLLO SE LA SUA LINEA DI ARRIVO HA SPUNTE O BOLLE
341500960202     C                   MOVE      FFV(B)        BRVLNA
341600070123     C     KBRC2         SETLL     FNBRV02                                30
341700130530     C  N30k03BLP77      setll     FNBLP77L                               30
341800960402     C   30              MOVEL     'S'           WBOL              1
341900960402     C  N30              MOVEL     ' '           WBOL              1
342000920928     C*
342100960212     C* PER I FV NON ABBINATI DA NON TRASMETTERE IN SEDE, INVIO SOLO
342200960212     C*  SE HO BOLLE O SPUNTE
342300960402    2C     WBOL          IFEQ      'S'
342400960212     C     FGVTRN        ORGT      0
342500960212     C     §TVUTC        OREQ      'S'
342600040329     c* escludo fogli viaggio poste locali che non sono da trasmettere
342700040329     c* in arrivo
342800040329   2bc     wfvpt         ifne      'S'
342900040329     c     fgvttr        orne      'L'
343000020403     c*
343100020403     c                   add       1             w0060             6 0
343200960212     C*
343300911004     C                   MOVEL     FLP(B)        COMLIN
343400040329     C                   CLEAR                   fnlv54ds
343500040317     C****               MOVEL     'P'           D55TPT
343600040329     c                   z-add     fgvlnp        i54lnp
343700040329     C* Se fv poste gli passo il p.o. poste
343800040329     C     WFGW          IFEQ      'S'
343900040329     C     FGWFGS        ANDGT     *ZEROS
344000040329     C                   Z-ADD     FGWFGS        i54LNP
344100040329     C                   ENDIF
344200040329     C                   MOVEL     COMLIN        i54LIN
344300040329     C                   MOVEL     FGVDFV        i54drf
344400040329     C                   CALL      'FNLV54R'
344500040329     C                   PARM                    fnlv54ds
344600040329    3C     o54ERR        IFEQ      *BLANKS
344700920928     C*
344800911004     C* SE NON LO HO GIA' INVIATO INVIO
344900921202     C     COMLIN        LOOKUP    LP2                                    20
345000940620    4C  N20              DO
345100040826     C                   ADD       1             X                 4 0
345200921202     C                   MOVEL     COMLIN        LP2(X)
345300921202     C*
345400921202     C* TRASMETTO SE NON E' LOCALE
345500040329    5C     o54tfp        IFNE      FGVLNP
345600040329     c     §tvdef        andne     'S'
345700040329     c     fgvlnp        orne      o54tfa
345800911004     C*
345900040329     C                   MOVEL     o54TFP        VARX
346000921202     C                   EXSR      CAEXFV
346100020403     c*
346200911004     C*
346300960126     C* TRASMETTO SOLO SE FV APERTO O SFLAGGATO(DA TRASMETTERE)
346400960126    6C     FGVFCF        IFEQ      ' '
346500960126     C     FGVFTR        OREQ      ' '
346600950113     C                   MOVEL     ' '           FVAATR
346700921202     C                   MOVEL     COMLIN        FVALAI
346800950113     C                   WRITE     FNFVA0TT
346900000428    7C     WFGW          IFEQ      'S'
347000000428     C                   MOVEL     COMLIN        FWALAI
347100000428     C                   WRITE     FNFWA000
347200000428    7C                   ENDIF
347300060710     c* Scrittura del file FNFV4
347400060710     c                   EXSR      SCRIVIFV4
347500020807    6C                   ENDIF
347600010622      *
347700940620    5C                   ENDIF
347800940620    4C                   ENDDO
347900940620    3C                   ENDIF
348000920427     C*
348100940620     C* R I T R A S M E T T O   F O G L I O   X   T R A N S I T I  SE:
348200940620     C*
348300971105     C* 1) SE NON E' UN FOGLIO LOCALE (TFP DELLO SCARICO=FGVLNP)
348400940620     C*    E LA LINEA ARRIVO E' DIVERSA DALLO SCARICO
348500960402     C*    E SOLO SE HA BOLLE O SPUNTE
348600050927     C*    oppure locale con spunte transito
348700020807    3C     FFV(B)        IFNE      FLP(B)
348800960402     C     WBOL          ANDEQ     'S'
348900050927     c     winvloc(B)    oreq      'S'
349000921202     C*
349100920427     C                   MOVEL     FFV(B)        COMLNA
349200970516     C* Richiamo pgm per determinare terminal di arrivo e partenza
349300970703     C* della lna
349400031110     C                   CLEAR                   fnlv54ds
349500031112     C                   MOVE      COMLNA        i54LIN
349600031112     C                   MOVE      FGVLNP        i54LNP
349700000620     C* Se fv poste gli passo il p.o. poste
349800000620     C     WFGW          IFEQ      'S'
349900000620     C     FGWFGS        ANDGT     *ZEROS
350000031112     C                   Z-ADD     FGWFGS        i54LNP
350100000620     C                   ENDIF
350200031112     C                   MOVE      FGVDFV        i54DRF
350300031110     C                   CALL      'FNLV54R'
350400031110     C                   PARM                    fnlv54ds
350500921202     C*
350600940620     C*
350700050203     C* 2) SE TFP DELLA LINEA ARRIVO NON E' LOCALE
350800031110    4C     O54TFP        IFNE      FGVLNP
350900040317     c     fgvlnp        orne      o54tfa
351000050203     c
351100050203     c* 3) Trasmetto anche il foglio locale se presenti delle spunte
351200050203     c*    transito
351300050203     c     winvloc(B)    oreq      'S'
351400940620     C*
351500130531     c* Trasmettere
351600130531     c*  sempre al terminal di arrivo, tranne quando ci sono  spunte
351700050203     c*  transito su un foglio locale (vale solo per le  linee di arriv
351800050203     c*  un po' terminal arrvioe un po' 2 livello in arrivo)
351900980126     C*
352000050203     C**   WINV          IFEQ      'T'
352100050203     c                   if        winvloc(B)=' '
352200031110     C                   MOVEL     o54TFA        WLNA
352300980126     C                   ELSE
352400980126     C                   MOVEL     COMLNA        WLNA
352500980126     C                   ENDIF
352600921118     C*
352700050203     C* 3) SE NON L'HO GIA'INVIATO
352800980126     C     WLNA          LOOKUP    LP2                                    20
352900940620    6C  N20              DO
353000921118     C                   ADD       1             X
353100980126     C                   MOVEL     WLNA          LP2(X)
353200921118     C*
353300050203     c* Devo trasmettere il fogio nel membro del terPaart del TerARR
353400050203     c*  dell alna, tranne se foglio locale con invio spunte transito
353500050203     c     winvloc(B)    ifeq      ' '
353600040217     C     O54TFP        andne     O54TFA
353700040218     c                   move      o54tfa        wtfa
353800130716     C*****              CLEAR                   dslv55
353900130716     c*****              movel     'P'           d55tpt
354000130716     C*****              MOVE      wtfa          d55LIN
354100130716     C*****              MOVE      FGVDFV        d55DRF
354200130716     C*****              CALL      'FNLV55R'
354300130716     C*****              PARM                    dslv55
354400130716     C                   CLEAR                   fnlv54ds
354500130716     c                   movel     'P'           i54tpt
354600130716     C                   MOVE      wtfa          i54LIN
354700130716     C                   MOVE      FGVDFV        i54DRF
354800130716     C                   CALL      'FNLV54R'
354900130716     C                   PARM                    fnlv54ds
355000040217     c                   endif
355100040217     c*
355200130716     C****               MOVEL     d55TFP        VARX
355300130716     C                   MOVEL     o54TFP        VARX
355400920427     C                   EXSR      CAEXFV
355500921202     C*
355600960126     C* TRASMETTO SOLO SE FV APERTO O SFLAGGATO(CIOE' DA TRASMETTERE)
355700960126    7C     FGVFCF        IFEQ      ' '
355800960126     C     FGVFTR        OREQ      ' '
355900950113     C                   MOVEL     'T'           FVAATR
356000980126     C                   MOVEL     WLNA          FVALAI
356100950113     C                   WRITE     FNFVA0TT
356200020807    8C     WFGW          IFEQ      'S'
356300000428     C                   MOVEL     WLNA          FWALAI
356400000428     C                   WRITE     FNFWA000
356500020807    8C                   ENDIF
356600060710     c
356700060710     c* non mi serve più scrivere fnfv4 in quanto ho già scritto prima
356800060710     c*  il file fisico, unico, che trasmette eventualente anche
356900060710     c*  in sede (una sola volta quindi basta)
357000060710     c*********          EXSR      SCRIVIFV4
357100060710    7C                   ENDIF
357200010622      *
357300940620    6C                   ENDDO
357400940620    4C                   ENDIF
357500040329    3c                   ENDIF
357600040329   2bc                   endif
357700960212    2C                   ENDIF
357800911004     C*
357900911004     C                   ADD       1             B
358000940620    1C                   ENDDO
358100911004     C*
358200951120     C* SCRIVO FILE DI APPOGGIO DA INVIARE IN SEDE SE:
358300951120     C*   - IL FOGLIO NON E' ABBINATO
358400951120     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' VUOTO
358500951120     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO ED E'
358600951120     C*     DA UTILIZZARE NEI CONTROLLI DI SEDE (§TVUTS = "S")
358700951120     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO MA NON
358800951120     C*     C'E' IL RECORD IN TABELLA
358900950113     C                   MOVEL     ' '           FVAATR
359000951120     C*
359100960212    1C     FGVTRN        IFEQ      0
359200960212    2C     FGVTTR        OREQ      ' '
359300960212    3C     §TVUTC        OREQ      'S'
359400950113     C                   WRITE     FNFGV046
359500951120    1C                   ENDIF
359600930709     C*
359700911004     C                   ENDSR
359800910910     C**-----------------------------------------------*
359900950516     C** SCARICO SPUNTE PER L'AS DELLA LNA
360000910910     C**-----------------------------------------------*
360100910912     C     WTRLNA        BEGSR
360200910910     C*
360300050204     c* Secondo me questo IF si può eliminare ..... (ES)
360400040317    4C     lnafel        IFne      FGVLNP
360500040317     c     fgvlnp        orne      lnatfa
360600960604     C*
360700960806     C                   MOVEL     'N'           WALCF
360800930520     C*
360900040217     c* se transito e ter-arr della lna e lna sono sullo stesso ogas
361000040217     c* trasmetto a ter-part del ter-arr di lna
361100040217     c     *in06         ifeq      *off
361200000725     C                   MOVEL     LNAFEL        VARX
361300040217     c                   else
361400130531     C                   CLEAR                   dslv55
361500130531     C                   MOVEL     'P'           d55TPT
361600130531     C                   z-add     lnatfa        d55LIN
361700130531     C                   Z-ADD     FGVDFV        d55DRF
361800130531     c                   call      'FNLV55R'
361900130531     c                   parm                    dslv55
362000130531     C                   MOVEL     d55tfp        VARX
362100040217     c                   endif
362200910910     C                   EXSR      CAEXSP
362300910910     C*
362400950113     C                   MOVEL     BRVLNA        COMLNA            3 0
362500950113     C                   MOVEL     SAVLNA        BRVLNA
362600070123     C     KBRC2         SETLL     FNBRV02
362700070123     C     KBRC2         READE(N)  FNBRV02                                29
362800910910     C*
362900960205    2C     *IN29         DOWEQ     '0'
363000000111     C                   CLEAR                   FISPP000
363100910910     C*
363200960604     C                   MOVEL     BRVLNP        KLNP
363300960604     C                   MOVEL     BRVLNA        KLNA
363400960604     C                   MOVEL     BRVNRS        KNRS
363500960604     C                   MOVEL     BRVNSC        KNSC
363600960604     C*
363700960604     C                   EXSR      RICLNP
363800960604     C*
363900960604     C     WLNP          LOOKUP    L1                                     20
364000940322     C*
364100940322     C* SPUNTA PER TERMINAL
364200960205    3C     *IN08         IFEQ      *ON
364300940322     C* SPUNTA CHE TRANSITO
364400960205    4C     *IN20         IFEQ      *OFF
364500970814     C     WTAB5F        ANDEQ     ' '
364600950113     C*
364700960604     C                   SELECT
364800960611     C     WOKBT         WHENEQ    *BLANKS
364900020606     C     KBtt37        SETLL     FNBTT37L                               34
365000960611     C     WOKBT         WHENEQ    'S'
365100960604     C                   SETON                                        34
365200960604     C                   ENDSL
365300971105     C                   MOVEL     FGVLNP        SPPFLP
365400950515     C* 34 ON - USCITA DA TRANSITO
365500960205    5C     *IN34         IFEQ      *ON
365600950515     C                   MOVEL     'T'           SPPATR
365700960205   X5C                   ELSE
365800990819     C* DEFLUENZA
365900990819     C* Se filiale gestione spunta è anche il terminal di arrivo
366000990819     C* non devo trasmettere la spunta come disguido ma come defluenza
366100990819     C* (N.B.: caso di Padova che ha spuntato colli con lnp 007 ed lna
366200990819     C* 059 sul foglio viaggio locale anzichè sulla defluenza)
366300990819     C     BRVFGS        IFEQ      LNATFA
366400990819     C                   MOVEL     'F'           SPPATR
366500990819     C                   ELSE
366600940322     C* DISGUIDO
366700960604     C                   MOVEL     'D'           SPPATR
366800990819     C                   ENDIF
366900960205    5C                   ENDIF
367000950515     C*
367100960205   X4C                   ELSE
367200960205     C                   MOVEL     ' '           SPPATR
367300950515     C                   CLEAR                   SPPFLP
367400960205    4C                   ENDIF
367500940322     C*
367600940322     C                   EXSR      RIESPP
367700000111     C                   WRITE     FISPP000
367800960205    3C                   ENDIF
367900940322     C*
368000940322     C* SPUNTA ATTESA TRANSITO
368100980210    3C     *IN06         IFEQ      *ON
368200950515     C*
368300960205    4C     *IN20         IFEQ      *ON
368400970814     C     WTAB5F        OREQ      'S'
368500940322     C                   MOVEL     'A'           SPPATR
368600950215     C                   MOVEL     COMLIN        SPPFLP
368700940322     C                   EXSR      RIESPP
368800000111     C                   WRITE     FISPP000
368900960205    4C                   ENDIF
369000960205    3C                   ENDIF
369100910910     C*
369200070123     C     KBRC2         READE(N)  FNBRV02                                29
369300960205    2C                   END
369400910910     C*
369500940322     C* MI RIPOSIZIONO SULLA NUOVA LINEA ARRIVO LETTA
369600970814    2C     WLAST         IFEQ      *BLANKS
369700950113     C                   MOVEL     COMLNA        BRVLNA
369800070123     C     KBRC2         SETLL     FNBRV02
369900070123     C     KBRC2         READE     FNBRV02                                22
370000960708     C*
370100970814    3C     *IN22         IFEQ      *OFF
370200960708     C                   MOVE      'S'           WALCF
370300960708     C                   MOVEL     BRVLNP        KLNP
370400960708     C                   MOVEL     BRVLNA        KLNA
370500960708     C                   MOVEL     BRVNRS        KNRS
370600960708     C                   MOVEL     BRVNSC        KNSC
370700960708     C                   EXSR      RICLNP
370800970814    3C                   END
370900970814    2C                   END
371000021127    1C                   END
371100910910     C*
371200910910     C                   ENDSR
371300910911     C**-----------------------------------------------*
371400910911     C** CREO SPUNTE PER AUTOSPUNTA
371500910911     C**-----------------------------------------------*
371600910911     C     CRTSPU        BEGSR
371700140618     C                   CLEAR                   FNBRV05
371800950109     C                   MOVEL     BLPFLS        BRVLNP
371900950109     C                   MOVEL     BLPLNA        BRVLNA
372000950109     C                   MOVEL     BLPNRS        BRVNRS
372100950109     C                   MOVEL     BLTNSC        BRVNSC
372200950109     C                   MOVEL     BLPZNC        BRVZNC
372300950109     C                   MOVEL     '90'          BRVNPS
372400950109     C                   Z-ADD     WNPG          BRVNPG
372500000111     C                   Z-ADD     FGV2          BRVNFV
372600000111     C                   Z-ADD     FGVLNP        BRVPES
372700971105     C                   MOVEL     FGVLNP        BRVFGS
372800971107     C                   MOVEL     FGVLNP        BRVFLE
372900950109     C                   Z-ADD     BLTVUC        BRVVUC
373000990906     C                   Z-ADD     BLTPUC        BRVPUC
373100950109     C                   MOVEL     BLTCAN        BRVCAN
373200070219     C***                TIME                    W0140            14 0
373300070219     C***                MOVEL     W0140         BRVHMS
373400070219     C***                CLEAR                   WLBDAT
373500070219     C***                MOVE      W0140         G02DAT
373600070219     C***                CALL      'XSRDA8'
373700070219     C***                PARM                    WLBDAT
373800070219     C***                Z-ADD     G02INV        BRVDFS
373900070219     c                   time                    wtimez
374000070219     c* millisecondi
374100070219     c                   extrct    wtimez:*ms    millisec          6 0
374200070219     c                   movel     millisec      brvmis
374300070219     c* data immissione
374400070219     c                   movel     wtimez        dataiso
374500070219     c                   movel     dataiso       brvdfs
374600070219     c* ora immissione
374700070219     c                   move      wtimez        oraiso
374800070219     c                   movel     oraiso        brvhms
374900070219     c                   movel     'A'           brvtap
375000070219     c                   eval      brvpru='§'+ nompgm
375100910911     C*
375200140618     C                   WRITE     FNBRV05
375300910911     C                   ENDSR
375400910911     C**-----------------------------------------------*
375500910911     C** CREO RECORD ANOMALIA
375600910911     C**-----------------------------------------------*
375700910911     C     CRTANM        BEGSR
375800921022     C                   MOVEL     *BLANKS       ANMATB
375900960514     C                   MOVE      BRVFGS        ANMCDU
376000921022     C                   Z-ADD     0             ANMAAS
376100990517     C                   Z-ADD     0             ANMDUE
376200990517     C                   Z-ADD     0             ANMNPG
376300990517     C                   Z-ADD     0             ANMFGS
376400990518     C                   Z-ADD     0             ANMFL3
376500040531     C                   Z-ADD     999           ANMFL4
376600990518     C                   Z-ADD     0             ANMDTR
376700990518     C                   MOVE      *BLANK        ANMFLO
376800040603     c* data ora di creazione
376900050707     c***                z-add     *date         wdata             6 0
377000050707     c***                time                    wora              6 0
377100050707     c
377200050707     C                   TIME                    W0140            14 0
377300050707     C                   CLEAR                   WLBDAT
377400050707     C                   MOVE      W0140         G02DAT
377500050707     C                   CALL      'XSRDA8'
377600050707     C                   PARM                    WLBDAT
377700050707     C                   Z-ADD     G02INV        Wdata             6 0
377800050707     c                   movel     w0140         wora4             4 0
377900040603     c                   movel     wdata         anmflo
378000040603     c                   move      wora4         anmflo
378100040531     c
378200040531     C**   COMCAA        IFEQ      105
378300040531     c* la zona la appoggio ora in anmfl4
378400040531     C**                 CLEAR                   DANM01
378500040531     C**                 MOVE      BRVZNC        §ANZNC
378600040531     C**                 MOVEL     DANM01        ANMFLO
378700040531     C**                 ENDIF
378800040531     c                   z-add     brvznc        anmfl4
378900040531     c
379000990518     C                   MOVE      *BLANK        ANMFT3
379100040603     c
379200110909     C                   MOVE      *BLANK        ANMFT4
379300110909     C                   Z-ADD     0             ANMLNP
379400110909     C                   Z-ADD     0             ANMNSP
379500110909     c
379600110909     c* imposto dati bolle per anomalie 110 e 115
379700110909     c* flag di valore
379800110909     c                   if        comcaa=110  or comcaa=115
379900040603     c                   movel     §ar5bva       anmft4
380000110909     c                   eval      anmaas=bltaas
380100110909     c                   eval      anmlnp=bltlnp
380200110909     c                   eval      anmnsp=bltnsp
380300040603     c                   endif
380400110909
380500921022     C                   MOVEL     '1'           ANMFSA
380600950327     C                   MOVE      WDTP          ANMDAO
380700921022     C                   MOVEL     *BLANKS       ANMCCH
380800971107     C                   MOVEL     FGVLNP        ANMFLE
380900921022     C                   MOVEL     *BLANKS       ANMFT1
381000921022     C                   Z-ADD     0             ANMFL1
381100921022     C                   MOVEL     *BLANKS       ANMFT2
381200921022     C                   Z-ADD     0             ANMFL2
381300921022     C*
381400950113     C                   MOVEL     BRVLNP        ANMFLS
381500950113     C                   MOVEL     BRVLNA        ANMLNA
381600950113     C                   MOVEL     BRVNRS        ANMNRS
381700950113     C                   MOVEL     BRVNSC        ANMSCN
381800950113     C                   MOVEL     BRVNPS        ANMNPS
381900950113     C                   Z-ADD     BRVNFV        ANMNFV
382000911018     C                   Z-ADD     COMCAA        ANMCAA
382100911018     C                   MOVEL     '7C'          COD
382200911018     C                   MOVEL     *BLANKS       KEY
382300911018     C                   MOVEL     COMCAA        KEY
382400911018     C     KTAB          CHAIN     TABEL                              21
382500911018     C  N21              MOVEL     TBLUNI        DS7C
382600911018     C   21              MOVEL     *BLANKS       DS7C
382700911018     C*
382800911018     C                   MOVEL     §7CTAN        ANMTAN
382900911018     C                   MOVEL     §7CSAN        ANMSAN
383000911018     C                   MOVEL     §7CAIE        ANMAIE
383100911018     C                   Z-ADD     §7CLID        ANMLID
383200910911     C                   Z-ADD     0             ANMDCH
383300911011     C                   MOVEL     *BLANKS       ANMFSC
383400911018     C     §7CCHA        IFEQ      'S'
383500950327     C                   MOVE      WDTP          ANMDCH
383600911018     C                   MOVEL     '1'           ANMFSC
383700911018     C                   END
383800910911     C*
383900990517     C                   WRITE     FNANM
384000910911     C                   ENDSR
384100910911     C**-----------------------------------------------*
384200910911     C** CHIUSURA ANOMALIE MANCA BOLLA
384300910911     C**-----------------------------------------------*
384400910911     C     CHIUAN        BEGSR
384500050927     C                   CLEAR                   WESISTEANM        1
384600990517     C     KANM3         CHAIN(N)  FNANM                              21
384700960416     C     *IN21         IFEQ      *OFF
384800130607     c                   exsr      call_lr33
384900050927     C                   eval      wesisteanm='S'
385000050927     C                   END
385100921014     C*
385200910911     C                   ENDSR
385300130607     C**-----------------------------------------------*
385400130607     C** chiamata a FNLR33R
385500130607     C**-----------------------------------------------*
385600130607     C     call_lr33     BEGSR
385700130607     C                   CLEAR                   DSLR33
385800130607     C                   MOVE      WDTP          D33DCH
385900130607     C                   MOVEL     '1'           D33FSC
386000130607     C                   MOVEL     '  '          D33CCH
386100130607     c                   if        comcaa=110  or comcaa=105
386200130607     C                   MOVEL     'BO'          D33CCH
386300130607     c                   endif
386400130607     C                   MOVE      ANMNR2        D33NRR
386500130607     C                   CALL      'FNLR33R'
386600130607     C                   PARM                    DSLR33
386700130607     c                   ENDSR
386800920324     C**-----------------------------------------------*
386900920324     C** ALLOCO MEMBRI DI TRASMISSIONE
387000920324     C**-----------------------------------------------*
387100920324     C     ALLOCO        BEGSR
387200950109     C* CARICO FILIALI ELABORATORE LINE FV
387300920324     C                   EXSR      RIEMPI
387400920324     C* ALLOCO MEMBRI DELLE LINEE DI ARRIVO
387500920324     C                   EXSR      LCKMBR
387600920324     C* ALLOCO MEMBRI LINEE DI TRANSITO
387700950915     C  N50              EXSR      LCKTRN
387800920526     C* NON RIESCO AL ALLOCARE: RISOTTOMETTO 3 VOLTE
387900950915     C   50              EXSR      ERROR
388000920324     C*
388100920324     C                   ENDSR
388200920324     C**-----------------------------------------------*
388300971105     C** CARICO TERMINAL DI PARTENZA DELLE LINEE DEL FV
388400920324     C**-----------------------------------------------*
388500920324     C     RIEMPI        BEGSR
388600920324     C*
388700921202     C                   Z-ADD     1             K                 3 0
388800920324     C                   Z-ADD     1             I                 3 0
388900920324     C                   Z-ADD     1             Y                 3 0
389000920324     C*
389100921202     C     FFV(I)        DOWGT     *ZEROS
389200920324     C                   MOVEL     FFV(I)        SAVFFV            3 0
389300971104     C*
389400971104     C                   CLEAR                   DSLV55
389500971104     C                   MOVEL     'P'           D55TPT
389600971104     C                   Z-ADD     SAVFFV        D55LIN
389700971105     C                   Z-ADD     FGVDFV        D55DRF
389800971104     C                   CALL      'FNLV55R'
389900971104     C                   PARM                    DSLV55
390000971104     C     D55TFP        LOOKUP    FEL                                    30
390100971104     C  N30              Z-ADD     D55TFP        FEL(K)
390200920324     C  N30              ADD       1             K
390300920324     C* FILIALI DI TRANSITO
390400920324     C     FLP(I)        IFGT      *ZEROS
390500920324     C                   MOVEL     FLP(I)        SAVFLP            3 0
390600920324     C* C'E' TRANSITO SE NON E' LA FIL DI ARRIVO
390700920324     C*                         LA SUA FILIALE ELABORATORE
390800930312     C*   SE E' IL SUO TERMINAL ALLOCO LO STESSO
390900920324     C     SAVFFV        IFNE      SAVFLP
391000960325     C*
391100971105     C     D55TFP        ANDNE     SAVFLP
391200930312     C*
391300971105     C                   CLEAR                   DSLV55
391400971105     C                   MOVEL     'P'           D55TPT
391500971105     C                   Z-ADD     SAVFLP        D55LIN
391600971105     C                   Z-ADD     FGVDFV        D55DRF
391700971105     C                   CALL      'FNLV55R'
391800971105     C                   PARM                    DSLV55
391900950215     C*
392000971105     C* SE IL TERMINAL DI PARTENZA NON E' FGVLNP
392100971105     C     D55TFP        IFNE      FGVLNP
392200971105     C     D55TFP        LOOKUP    FET                                    30
392300971105     C  N30              Z-ADD     D55TFP        FET(Y)
392400920324     C  N30              ADD       1             Y
392500920324     C                   END
392600950215     C                   END
392700920324     C                   END
392800921202     C*
392900921202     C                   ADD       1             I
393000920324     C                   END
393100050524     c
393200050524     c* Controllo riempimento schiera
393300050524     c                   clear                   trul0sds
393400050524     c                   eval      i0sski='FEL'
393500050524     c                   eval      i0sele=%elem(fel)
393600050524     c                   eval      i0spie=k-1
393700050524     c                   eval      i0sfile='TerPar LNA'
393800050524     c                   eval      i0ssif=knsif
393900130718     c                   eval      i0spgm='FNLSU2R'
394000050524     c                   movel     trul0sds      kpjbu
394100050524     c                   call      'TRUL0SR'
394200050524     c                   parm                    kpjba
394300050524     c
394400050524     c* Controllo riempimento schiera
394500050524     c                   clear                   trul0sds
394600050524     c                   eval      i0sski='FET'
394700050524     c                   eval      i0sele=%elem(fet)
394800050524     c                   eval      i0spie=y
394900050524     c                   eval      i0sfile='TerPar FLP'
395000050524     c                   eval      i0ssif=knsif
395100130718     c                   eval      i0spgm='FNLSU2R'
395200050524     c                   movel     trul0sds      kpjbu
395300050524     c                   call      'TRUL0SR'
395400050524     c                   parm                    kpjba
395500920324     C                   ENDSR
395600920324     C**-----------------------------------------------*
395700920324     C** ESEGUE CALL QCAEXEC SUL MEMBRO DELLE BOLLE    *
395800920324     C**-----------------------------------------------*
395900920324     C     CRTMBR        BEGSR
396000930127     C*
396100930127     C                   CALL      'TRUL50C'
396200930127     C                   PARM                    FISICO           10
396300930127     C                   PARM                    FISMBR           10
396400000428     C                   PARM                    O50PLA
396500930127     C                   PARM                    LOGICO           10
396600930127     C                   PARM                    LOGMBR           10
396700930127     C                   PARM                    ULFLG             1
396800930127     C* RITENTO L'ALLOCAZIONE
396900950915     C                   CALL      'QCMDEXC'                            50
397000930127     C                   PARM                    COMMAN
397100930127     C                   PARM                    LUNG
397200930127     C*
397300920324     C                   ENDSR
397400920324     C**-----------------------------------------------*
397500920324     C** ALLOCAZIONE MEMBRI
397600920324     C**-----------------------------------------------*
397700920324     C     LCKMBR        BEGSR
397800920324     C                   Z-ADD     1             K
397900950915     C                   SETOFF                                       50
398000021014     C                   MOVE      '))'          VAR2
398100000725     C                   MOVEL     'M'           VAR2
398200000731     C                   MOVEL     'M'           EMME
398300021014     C                   Z-ADD     42            LUNG
398400920324     C*
398500920324     C     FEL(K)        DOWNE     0
398600950109     C* ALLOCO MEMBRO FNBLP
398700000725     C                   MOVEL     FEL(K)        VAR2X
398800930127     C                   MOVEL     FEL(K)        NUM3
398900920324     C**
399000920324     C** SE NON CI SONO BOLLE DA INVIARE E' INUTILE ALLOCARE
399100950115     C* ALLOCAZIONE FILE DELLE BOLLE
399200950115     C   09              EXSR      LCKBLP
399300920324     C**
399400971105     C* ALLOCO SOLO SE IL MEMBRO DA SCRIVERE NON è LOCALE COME TERMINAL
399500971105     C* DI PARTENZA
399600040331    1C  N50              DO
399700960124     C* SE VENGO DA MANUTENZIONE FV NON DEVO ALLOCARE IL FILE SPUNTE
399800960124    2C     PA2FLG        IFNE      'M'
399900960124     C**
400000000111     C                   MOVE      VAR2          CM47
400100920324     C                   MOVEL     *BLANKS       COMMAN           80
400200921202     C                   MOVEA     CM47(1)       COMMAN
400300950915     C                   CALL      'QCMDEXC'                            50
400400920324     C                   PARM                    COMMAN
400500920324     C                   PARM                    LUNG
400600930127     C*
400700930127     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
400800930127     C*  SE MANCA E RITENTO
400900960124    3C     *IN50         IFEQ      '1'
401000000111     C                   MOVEL     'FISPP00T'    FISICO
401100930127     C                   EXSR      CRTMBR
401200960124    3C                   END
401300960124    2C                   ENDIF
401400920324     C**
401500950915    2C  N50              DO
401600951116     C                   MOVE      VAR2          CM49
401700920324     C                   MOVEL     *BLANKS       COMMAN           80
401800921202     C                   MOVEA     CM49(1)       COMMAN
401900950915     C                   CALL      'QCMDEXC'                            50
402000920324     C                   PARM                    COMMAN
402100920324     C                   PARM                    LUNG
402200930127     C*
402300930127     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
402400930127     C*  SE MANCA E RITENTO
402500950915    3C     *IN50         IFEQ      '1'
402600951116     C                   MOVEL     'FNFVA01T'    FISICO
402700930127     C                   EXSR      CRTMBR
402800950115    3C                   END
402900960123     C*
403000960123    3C  N50              DO
403100000428     C                   MOVE      VAR2          CM48
403200000428     C                   MOVEL     *BLANKS       COMMAN           80
403300000428     C                   MOVEA     CM48(1)       COMMAN
403400000428     C                   CALL      'QCMDEXC'                            50
403500000428     C                   PARM                    COMMAN
403600000428     C                   PARM                    LUNG
403700000428     C*
403800000428     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
403900000428     C*  SE MANCA E RITENTO
404000000428    5C     *IN50         IFEQ      '1'
404100000428     C                   MOVEL     'FNFWA00T'    FISICO
404200000428     C                   EXSR      CRTMBR
404300000428    5C                   END
404400960123    3C                   END
404500960123     C*
404600950115    2C                   END
404700950115    1C                   END
404800950115     C**
404900950915     C  N50              ADD       1             K                 3 0
405000950915     C   50              Z-ADD     149           K
405100920324     C                   END
405200920324     C*
405300920324     C                   ENDSR
405400950115     C*
405500950115     C*--------------------------------------
405600950115     C* ALLOCAZIONE FILE DELLE BOLLE
405700950115     C*--------------------------------------
405800950115     C     LCKBLP        BEGSR
405900950115     C*
406000971104     C                   MOVE      VAR2          CM41
406100950115     C                   MOVEL     *BLANKS       COMMAN           80
406200971104     C                   MOVEA     CM41(1)       COMMAN
406300950915     C                   CALL      'QCMDEXC'                            50
406400950115     C                   PARM                    COMMAN
406500950115     C                   PARM                    LUNG
406600950115     C*
406700950115     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
406800030107     C*  SE MANCA E RITENTO
406900950915    1C     *IN50         IFEQ      '1'
407000971104     C                   MOVEL     'FNBLPE0T'    FISICO
407100950115     C                   EXSR      CRTMBR
407200950115    1C                   END
407300950115     C*
407400950115     C                   ENDSR
407500920324     C**-----------------------------------------------*
407600920324     C** ALLOCAZIONE MEMBRI  TRASMISSIONE
407700920324     C**-----------------------------------------------*
407800920324     C     LCKTRN        BEGSR
407900920324     C                   Z-ADD     1             K
408000950915     C                   SETOFF                                       50
408100920324     C     FET(K)        DOWNE     0
408200930310     C*
408300000725     C                   MOVEL     FET(K)        VAR2X
408400930310     C*
408500930310     C                   EXSR      TRANSI
408600930310     C*
408700950915     C  N50              ADD       1             K                 3 0
408800950915     C   50              Z-ADD     149           K
408900920324     C                   END
409000920324     C*
409100920324     C                   ENDSR
409200930310     C**-----------------------------------------------*
409300080423     C** ALLOCAZIONE FILE DEL TRANSITO
409400930310     C**-----------------------------------------------*
409500930310     C     TRANSI        BEGSR
409600000725     C                   MOVEL     VAR2X         NUM3
409700950109     C                   MOVE      VAR2          CM51
409800930310     C                   MOVEL     *BLANKS       COMMAN           80
409900930310     C                   MOVEA     CM51(1)       COMMAN
410000950915     C                   CALL      'QCMDEXC'                            50
410100930310     C                   PARM                    COMMAN
410200930310     C                   PARM                    LUNG
410300930310     C*
410400930310     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
410500930310     C*  SE MANCA E RITENTO
410600950915    1C     *IN50         IFEQ      '1'
410700950109     C                   MOVEL     'FNBLPT0T'    FISICO
410800930310     C                   EXSR      CRTMBR
410900950215    1C                   END
411000930310     C*
411100950915    1C  N50              DO
411200000111     C                   MOVE      VAR2          CM47
411300950215     C                   MOVEL     *BLANKS       COMMAN           80
411400950215     C                   MOVEA     CM47(1)       COMMAN
411500950915     C                   CALL      'QCMDEXC'                            50
411600950215     C                   PARM                    COMMAN
411700950215     C                   PARM                    LUNG
411800950215     C*
411900950215     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
412000950215     C*  SE MANCA E RITENTO
412100950915    3C     *IN50         IFEQ      '1'
412200000111     C                   MOVEL     'FISPP00T'    FISICO
412300950215     C                   EXSR      CRTMBR
412400950215    3C                   END
412500950215     C**
412600950915    3C  N50              DO
412700951116     C                   MOVE      VAR2          CM49
412800950215     C                   MOVEL     *BLANKS       COMMAN           80
412900950215     C                   MOVEA     CM49(1)       COMMAN
413000950915     C                   CALL      'QCMDEXC'                            50
413100950215     C                   PARM                    COMMAN
413200950215     C                   PARM                    LUNG
413300950215     C*
413400950215     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
413500950215     C*  SE MANCA E RITENTO
413600950915    4C     *IN50         IFEQ      '1'
413700951116     C                   MOVEL     'FNFVA01T'    FISICO
413800950215     C                   EXSR      CRTMBR
413900950215    4C                   END
414000950215     C**
414100960123    4C  N50              DO
414200000428     C**
414300000428     C                   MOVE      VAR2          CM48
414400000428     C                   MOVEL     *BLANKS       COMMAN           80
414500000428     C                   MOVEA     CM48(1)       COMMAN
414600000428     C                   CALL      'QCMDEXC'                            50
414700000428     C                   PARM                    COMMAN
414800000428     C                   PARM                    LUNG
414900000428     C*
415000000428     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
415100000428     C*  SE MANCA E RITENTO
415200000428    6C     *IN50         IFEQ      '1'
415300000428     C                   MOVEL     'FNFWA00T'    FISICO
415400000428     C                   EXSR      CRTMBR
415500000428    6C                   END
415600960123    4C                   END
415700960123     C*
415800950215    3C                   END
415900950215    1C                   END
416000930310     C                   ENDSR
416100920526     C*
416200920526     C* RISOTTOMETTO AZIONE PERCHE' NON RIESCO AD ALLOCARE -----------*
416300920526     C     ERROR         BEGSR
416400111019     C     NUM2          IFEQ      2
416500960129     C*
416600960129     C* SE VENGO DA MANUTENZIONE FV NON DEVO STAMPARE
416700111026     C***  PA2FLG        IFNE      'M'
416800111026     C***                OPEN      SYSPRT
416900111026     C***                EXCEPT    TESERR
417000111026     C***                EXCEPT    DETERR
417100111026     C***                CLOSE     SYSPRT
417200111026     C***                ENDIF
417300960129     C*
417400111019     C* SE NON RIESCO AD ALLOCARE INVIO mail a sede
417500111019     C                   movel     *blanks       wrkEml          253
417600111019     C                   movel     *blanks       wrkMsg         5000
417700111019     C                   movel     *blanks       wrkOgg           44
417800111019     c
417900111019     C                   MOVEL     MSG(1)        wrkogg
418000111019     C                   MOVEL(P)  MSG(2)        wrkmsg
418100111019     c                   eval      %subst(wrkmsg:41:3)=%editc(fgvlnp:'X')
418200111019     c                   eval      %subst(wrkmsg:45:6)=%editc(fgvnfv:'2')
418300120302     c                   eval      wrkeml='ced@brt.it'
418400111019     c
418500111019     C                   call(e)   'TIS701C'
418600111019     C                   parm                    wrkEml
418700111019     C                   parm                    wrkOgg
418800111019     C                   parm                    wrkMsg
418900920526     C*
419000920526     C                   ELSE
419100920526     C*
419200920526     C* RISOTTOMETTO L'AZIONE
419300920526     C                   ADD       1             NUM2
419400130524     C                   MOVEL     FNLSU2DS      KPJBU
419500130718     C                   MOVEL     'LSU2'        KCOAZ
419600920526     C                   CALL      'BCH10'
419700920526     C                   PARM                    KPJBA
419800920526     C* E RISOTTOMETTO AZIONE DI INVIO FILE
419900950130     C                   MOVEL     'LSA3'        KCOAZ
420000070423     c                   clear                   parama3
420100070423     c                   move      simfel        pa3tfp
420200070423     c                   movel(p)  parama3       kpjbu
420300931203     C                   CALL      'BCH10'
420400931203     C                   PARM                    KPJBA
420500920526     C                   END
420600920526     C                   ENDSR
420700930127     C**-----------------------------------------------*
420800000111     C** RIEMPO CAMPI DEL FILE FISPP00T
420900930127     C**-----------------------------------------------*
421000930127     C     INVSAI        BEGSR
421100950113     C                   MOVEL     '1'           SPPNPG
421200950113     C                   MOVEL     BRVLNP        SPPLNP
421300950113     C                   MOVEL     BRVLNA        SPPLNA
421400950113     C                   MOVEL     BRVNRS        SPPNRS
421500950113     C                   MOVEL     BRVNSC        SPPNSC
421600950113     C                   MOVEL     BRVNPS        SPPNPS
421700950113     C                   MOVEL     WDTP          SPPDFV
421800950113     C                   MOVEL     'T'           SPPFLG
421900000111     C                   WRITE     FISPP000
422000930127     C                   ENDSR
422100991019     C*
422200040311     C*--- ELABORAZIONE SPUNTE PARTENZA ------------------------------*
422300991019     C     SPUPAR        BEGSR
422400991019     C**
422500991019     C**   VEDO SE DEVO IMPOSTARVI PESO E/O VOLUME
422600000620     C                   CLEAR                   SAVSPE
422700130227     C                   SETOFF                                       45
422800991019     C*
422900040121     c* Invece che testare se ka cml/vdl, testo il flag se in sto terminal
423000040121     c*   si usa il peso  e/o volume sulle spunte
423100040609     c     pa2spv        ifeq      'S'
423200040609     c     pa2spp        oreq      'S'
423300991019     C* LETTO FV ENTRATA, LEGGO LE SUE SPUNTE
423400991019     C                   MOVEL     1             KVVNPG
423500000111     C                   Z-ADD     FGV2          KVVNFV
423600991019     C                   MOVEL     SIMFEL        KVVFGS
423700070123     C     KBRV32        SETLL     FNBRV02
423800070123     C     KBRV32        READE     FNBRV02                                32
423900991019     C*
424000991019    2C     *IN32         DOWEQ     *OFF
424100991019     C* ESCLUDO LE SPUNTE GIA' TRASMESSE
424200991019    3C     BRVFTR        IFEQ      ' '
424300000620     C*
424400040121    4c                   if        pa2spv='S' and brvvuc=0  or
424500040121     c                             pa2spp='S' and brvpuc=0
424600040121     c
424700991019     C                   MOVEL     DSBRV         SVBRVE
424800991019     C* IMPOSTO LA KEY PER CHAIN
424900991019     C                   Z-ADD     BRVVUC        WRVVUC
425000991019     C                   Z-ADD     BRVPUC        WRVPUC
425100991019     C                   EXSR      IMPKEY
425200991019     C*
425300070123     C     KBRV6         SETLL     FNBRV07L
425400070123     C     KBRV6         READE     FNBRV07L                               34
425500991019    5C     *IN34         DOWEQ     *OFF
425600991019     C**
425700120604     C* VEDO SE SPUNTA CON VOLUME o peso
425800991019    6C     WRVVUC        IFEQ      0
425900991019     C     BRVVUC        ANDGT     0
426000991019     C                   Z-ADD     BRVVUC        WRVVUC
426100991019    6C                   ENDIF
426200991019     C* VEDO SE SPUNTA CON PESO CML
426300991019    6C     WRVPUC        IFEQ      0
426400991019     C     BRVPUC        ANDGT     0
426500991019     C                   Z-ADD     BRVPUC        WRVPUC
426600991019    6C                   ENDIF
426700991019     C*
426800070123     C     KBRV6         READE     FNBRV07L                               34
426900991019    5C                   ENDDO
427000991019     C*
427100991019     C                   MOVEL     SVBRVE        DSBRV
427200991019     C                   Z-ADD     WRVVUC        BRVVUC
427300991019     C                   Z-ADD     WRVPUC        BRVPUC
427400991019     C* AGGIORNO SPUNTA PARTENZA
427500070123     C                   UPDATE    FNBRV02
427600991019    4C                   ENDIF
427700991019    3C                   ENDIF
427800991019     C*
427900070123     C     KBRV32        READE     FNBRV02                                32
428000991019    2C                   ENDDO
428100040609    1C                   ENDIF
428200991019     C**
428300991019     C                   ENDSR
428400940414     C*
428500940414     C*--- ELABORAZIONE ABBINAMENTO ----------------------------------*
428600040625     c     ABBIN         BEGSR
428700940418     C* APERTURA FILES
428800021021     C                   OPEN      FNANM20L
428900021021     C                   OPEN      FNBLT38L
429000020605     C                   OPEN      FNBTT38L
429100020916     C                   OPEN      FNFVV02L
429200001120     C                   OPEN      FNFVV01L
429300041217     c                   clear                   wcalLsB7          1
429400940418     C*
429500940415     C* PRENDO FASE ANOMALIA
429600940415     C                   CALL      'TRUL11R'
429700940415     C                   PARM                    PARAM9
429800940414     C*
429900940414     C                   CLEAR                   LFV
430000940414     C                   MOVEA     FFV           LFV
430100940414     C                   Z-ADD     1             I
430200940414     C*
430300170613     C***     PA2AGS        IFEQ      'S'
430400170613     C***                   SETON                                        44
430500170613     C***                   END
430600990917     C**
430700990917     C** SPUNTE DEL FOGLIO VIAGGIO PARTENZA
430800991019     C**   VEDO SE DEVO IMPOSTARVI PESO E/O VOLUME
430900991019     C                   EXSR      SPUPAR
431000000620     C*
431100000620     C                   CLEAR                   SAVSPE
431200940414     C*
431300940414     C* LEGGO TUTTE LE LINEE
431400950113    1C     LFV(I)        DOWGT     *ZEROS
431500940414     C                   MOVEL     LFV(I)        COMFIL
431600021021     c*
431700021021     c* per ogni linea di arrivo loopo sulle linee di partenza della L1
431800021021     c                   z-add     1             IL
431900021021    2c                   dow       l1(IL)>0
432000021021     c
432100021021     c                   eval      wlnp=l1(IL)
432200990917     C**
432300990917     C** BOLLE  PARTENZA
432400990917     C**
432500950915     C                   SETON                                        40
432600021021     C     KBLT          SETLL     FNBLT038
432700021021     C     KBLT          READE     FNBLT038                               32
432800940414     C*
432900021021    4C     *IN32         DOWEQ     '0'
433000000619     C*
433100970506     C* VEDO SE DEVO AGGIORNARE LA DATA DI ENTRATA
433200970506     C                   EXSR      AGGENT
433300970506     C**
433400950109     C* SE COLLI NON ANCORA PARTITI CERCO SOLO QUELLI BORDERIZZATI SUL
433500990303     C*  PER IL RESTO LI PRENDO TUTTI(DEVONO AVERE IL NUMERO FOGLIO
433600990303     C*  PIENO:ATTENZIONE ALLA "C" DELL'ESTERO)
433700000714     C*
433800130530    5C**** BLTNFV        IFEQ      FGV2
433900130530     C**** BLTFTR        ANDEQ     *BLANKS
434000990303     C*
434100130530     C     BLTFTR        ifeq      'C'
434200990303     C     BLTNFV        ANDGT     0
434300990303     C*
434400990303     C     BLTFTR        ORNE      *BLANKS
434500990303     C     BLTFTR        ANDNE     'C'
434600151126     c* Se parzializzo per KSC solo bolle di questo FGV
434700151126     c                   if        pa2ccm<=*zeros or bltnfv=fgv2
434800940414     C                   EXSR      CRTBRC
434900151126     c                   endif
435000151126     c
435100021021    5C                   ENDIF
435200000714     C**
435300021021     C     KBLT          READE     FNBLT038                               32
435400021021    4C                   ENDDO
435500021021     c
435600021021     c                   eval      IL=il+1
435700021021    2c                   enddo
435800021021     c
435900950915     C                   SETOFF                                       40
436000940414     C**
436100021025     C** BOLLE TRANSITO
436200940414     C**
436300151126     c                   if        pa2ccm<=*zeros
436400151126     C                   SETON                                        41
436500020605     C     KBtt38        SETLL     FNBTT38L
436600020605     C     KBtt38        READE     FNBTT38L                               32
436700940414     C*
436800950113    2C     *IN32         DOWEQ     '0'
436900080228     c* ignoro transito se bttlnp fa parte della £1 (può succedere x le
437000080228     c* trazioni ridotte quando alcuni 2 livello cambiano terminal di part)
437100080228     c     bttlnp        lookup    l1                                     90
437200080228     c                   if        not *in90
437300970506     C* VEDO SE DEVO AGGIORNARE LA DATA DI ENTRATA
437400970506     C                   EXSR      AGGENT
437500040422     c* se presente anomalia 200 dell'area non autogenero la spunta
437600040422     c* partenza perchè significa che il collo si trova presso un
437700040422     c* secondo livello in partenza di Simfel e quindi non posso
437800040422     c* dare per scontato che riparta
437900040422     c                   eval      wcrtspu='S'
438000040422     c                   z-add     200           comcaa
438100040503     c     kanm02        chain(N)  fnanm02l
438200040422     c                   if        %found(fnanm02l)
438300040422     c                             and anmdch = 0
438400040422     c     anmfle        lookup    l1                                     32
438500040422     c   32              eval      wcrtspu='N'
438600040422     c                   endif
438700970506     C**
438800040422     c     wcrtspu       ifeq      'S'
438900950109     C                   EXSR      CRTBRC
439000040422     c                   endif
439100080228
439200080228     c                   endif
439300940414     C*
439400020605     C     KBtt38        READE     FNBTT38L                               32
439500950113    2C                   ENDDO
439600940414     C                   SETOFF                                       41
439700151126     c                   endif
439800041217     c
439900041217     c* Eseguo call al pgm di calcolo volume e pesi
440000041217     c                   if        wcalLSB7='S'
440100041217     C                   Z-ADD     FGVLNP        PA1FEL
440200041217     C                   MOVEL     PARAM1        KPJBU
440300041217     C                   CALL      'FNLSB7R'
440400041217     C                   PARM                    KPJBA
440500041217     c
440600041217     c                   clear                   wcalLSB7
440700041217     c                   endif
440800940414     C**
440900970828     C** ANOMALIE DI DISGUIDO
441000940414     C**
441100000619     C     WFVPT         IFEQ      'N'
441200151126     c     pa2ccm        andle     *zeros
441300021021     C                   SETON                                        42
441400040115     c* Devo pulire questo campo perchè altrimenti potrebbe rimanere sporco
441500040115     c* e sbagliare la pistola in autogenerazione spunte partenza
441600040115     c                   clear                   wlocal
441700021021     c* loop per anmfle = a tutti i p.o. della £1
441800021021     C                   Z-ADD     1             IL                3 0
441900021021   1ac                   dow       l1(IL)>0
442000021021     c
442100021025     c                   eval      wlnp=l1(il)
442200940414     C                   Z-ADD     200           COMCAA
442300021021     C     KANM          SETLL     FNANM20
442400021021     C     KANM          READE     FNANM20                                32
442500940414     C*
442600950113    2C     *IN32         DOWEQ     '0'
442700940414     C* SE NON E' GIA' CHIUSA
442800950113    3C     ANMDCH        IFEQ      0
442900990517     C     WDTP          ANDGE     ANMDAO
443000960705     C*
443100020412     C                   CLEAR                   WCRT
443200960705     C* SE TROVO BOLLA PARTENZA CHIUDO ANOMALIA E NON AUTOGENERO SPUNTA
443300960705     C* (SE BOLLA DA ME CONFERMABILE E CONFERMABILE ANCHE DA ALTRI AS)
443400970828     C**
443500970828     C* ATTUALMENTE NON CREIAMO PIU' ANOMALIE DI DISGUIDO SE COLLO DA
443600970828     C*  ME CONFERMABILE ANCHE SE NON C'E' LA BOLLA PARTENZA PER CUI
443700970828     C*  NON HO PIU' BISOGNO DI CERCE LE ANOMALIE DISGUIDO
443800020412     c* 12/04/02: Ripristinata la routine srdisg in quanto se la 5f
443900020412     c* è stata inserita dopo il caricamento spunte il disguido può
444000020412     c* essere stato creato se al momento dello scarico spunte la
444100020412     c* bolla non era ancora stata confermata
444200020412    4C     anmnrs        ifgt      *zeros
444300020412     C                   exsr      srdisg
444400020412    4C                   endif
444500040406     c* disattivo autogenerazione spunte da anomalie di disguido: si voglion
444600040406     c* far partire i colli disguidati solo se spuntati in entrata o in par-
444700040406     c* tenza
444800040406     c                   eval      wcrt='N'
444900960705     C*
445000020412   3AC     wcrt          ifeq      ' '
445100070123     C     KANM5         CHAIN     FNBRV07                            31
445200950118    4C     *IN31         IFEQ      *OFF
445300950109     C* MI SALVO I CAMPI CHE MI SERVONO PER L'AGGIORNAMENTO
445400950109     C                   Z-ADD     BRVZNC        WRVZNC
445500950109     C                   Z-ADD     BRVVUC        WRVVUC
445600990906     C                   Z-ADD     BRVPUC        WRVPUC
445700980206     C     BRVCAN        IFNE      'E'
445800950113     C                   MOVEL     BRVCAN        WRVCAN
445900980206     C                   END
446000970506     C* IMPOSTO LA KEY PER CHAIN
446100970506     C                   EXSR      IMPKEY
446200970506     C* CREO SPUNTA PARTENZA
446300950109     C                   EXSR      CRTBRC
446400950113    4C                   END
446500020412   3AC                   endif
446600950113    3C                   END
446700940414     C*
446800021021     C     KANM          READE     FNANM20                                32
446900950113    2C                   ENDDO
447000021021     c                   eval      il=il+1
447100021021   1aC                   ENDDO
447200940414     C                   SETOFF                                       42
447300040624    1C                   ENDIF
447400940414     C**
447500960209     C** LEGGO LE SPUNTE ENTRATA PER CREARE EVENTUALI
447600950915     C** SPUNTE PARTENZA NON ANCORA CREATE
447700170613     C**  06/2017 non esiste più il caso di solo spunta 88 per cui questa loop
447800170613     C**          va sempre fatto
447900950915     C**
448000170613     C**     *IN44         IFEQ      *ON
448100170613     c**     pa2ccm        andle     *zeros
448200170613     c*
448300170613     c     pa2ccm        ifle      *zeros
448400170613     c
448500151126     c
448600950915     C                   SETON                                        43
448700001121     C*
448800950915     C                   Z-ADD     5             WNPG5
448900950915     C                   Z-ADD     DFV1          WDFV
449000020916      * ciclo all'interno della £1
449100020917do   c                   do        30            xx
449200020916     c                   If        L1(xx) = *Zeros
449300020916     c                   Leave
449400020916     c                   EndIf
449500020917     c     L1(xx)        Lookup    Spt                                    37
449600040802if   c     *IN37         IFEQ      *ON
449700040802     C     WFVPT         ANDEQ     'S'
449800040802     C*
449900040802     C     *IN37         OREQ      *OFF
450000040802     C     WFVPT         ANDEQ     'N'
450100040802     c*
450200040802if   c     *IN37         orEQ      *ON
450300040802     C     WFVPT         ANDEQ     'N'
450400040802     C
450500020916     c                   Move      L1(xx)        kfgs
450600020916     C     KFVV          SETLL     FNFVV02L
450700020916     C     KFVV          READE     FNFVV02L                               32
450800950915     C*
450900950915     C     *IN32         DOWEQ     *OFF
451000950915     C* LETTO FV ENTRATA, LEGGO LE SUE SPUNTE
451100970423     C                   MOVEL     FVVNPG        KVVNPG
451200000111     C                   Z-ADD     FVVNFV        KVVNFV
451300970423     C                   MOVEL     FVVFGS        KVVFGS
451400070123     C     KBRV33        SETLL     FNBRV02
451500070123     C     KBRV33        READE(N)  FNBRV02                                32
451600950915     C*
451700950915    2C     *IN32         DOWEQ     *OFF
451800960521     C* ESCLUDO LE SPUNTE CHE HANNO FLAG TRASMISSIONE 'N'
451900960521     C     BRVFTR        IFNE      'N'
452000960521     C     BRVDTR        ANDNE     *HIVAL
452100040422     c* se collo spuntato da un secondo livello in partenza non
452200040422     c* autogenero spunta partenza se presente anomalia 200 aperta
452300040422     c* creata da p.o. della £1 perchè significa che il collo è in
452400040422     c* disguido e non può ripartire se non spuntato in entrata o in
452500040422     c* partenza dal terminal di partenza
452600040422     c                   eval      wcrtspu='S'
452700040422     c     brvfgs        ifne      simfel
452800040422     c                   z-add     200           comcaa
452900040503     c     kanm3         chain(N)  fnanm02l
453000040422     c                   if        %found(fnanm02l)
453100040422     c                             and anmdch=0
453200040422     c     anmfle        lookup    l1                                     32
453300040422     c   32              eval      wcrtspu='N'
453400040422     c                   endif
453500040422     c                   endif
453600040422     c     wcrtspu       ifeq      'S'
453700950915     C                   Z-ADD     BRVZNC        WRVZNC
453800950915     C                   Z-ADD     BRVVUC        WRVVUC
453900990906     C                   Z-ADD     BRVPUC        WRVPUC
454000950915     C                   MOVEL     BRVCAN        WRVCAN
454100950915     C                   MOVEL     BRVNPS        WRVNPS
454200970506     C* IMPOSTO LA KEY PER CHAIN
454300970506     C                   EXSR      IMPKEY
454400970506     C* CREO SPUNTA PARTENZA
454500950915     C                   EXSR      CRTBRC
454600040422     c                   endif
454700960521     C                   ENDIF
454800950915     C*
454900070123     C     KBRV33        READE(N)  FNBRV02                                32
455000950915     C                   ENDDO
455100950915     C*
455200020916     C     KFVV          READE     FNFVV02L                               32
455300950915     C                   ENDDO
455400030902      *
455500020917if x c                   Else
455600020917     c                   Iter
455700020917if e c                   EndIf
455800030902      *
455900020917do e c                   EndDo
456000950915     C*
456100950915     C                   SETOFF                                       43
456200950915     C                   ENDIF
456300950915     C**
456400950915     C**CERCO SPUNTE IMP
456500950915     C**
456600960604     C                   MOVE      'N'           WALCF             1
456700960604     C*
456800070123     C     KBRV3         SETLL     FNBRV02
456900070123     C     KBRV3         READE(N)  FNBRV02                                32
457000940414     C*
457100950113    2C     *IN32         DOWEQ     *OFF
457200950109     C                   MOVEL     BRVLNP        KLNP
457300950109     C                   MOVEL     BRVLNA        KLNA
457400950109     C                   MOVEL     BRVNRS        KNRS
457500950113     C                   MOVEL     BRVNSC        KNSC
457600950109     C                   Z-ADD     1             WNPG
457700940414     C*
457800140618     C     KBRV5         SETLL     FNBRV05
457900140618     C     KBRV5         READE     FNBRV05                                33
458000940414     C*
458100940414     C* SEGNACOLLO TROVATO: ELIMINO SE NON PARTITO
458200000619    3C     *IN33         DOWEQ     *OFF
458300940414     C*
458400950113    4C     BRVDTR        IFEQ      0
458500980728     C* CANCELLO ANCHE SE FLAGGGATA CON "L" PERCHE' NON E' TRASMESSA MA
458600980728     C*  NON UTILIZZABILE
458700980728     C     BRVFTR        OREQ      'L'
458800140618     C                   DELETE    FNBRV05
458900950113   X4C                   ELSE
459000140618     C                   UNLOCK    FNBRV05L
459100961212    4C                   ENDIF
459200140618     C     KBRV5         READE     FNBRV05                                33
459300000619    3C                   ENDDO
459400940414     C*
459500070123     C     KBRV3         READE(N)  FNBRV02                                32
459600950113    2C                   ENDDO
459700940414     C**
459800940414     C                   ADD       1             I
459900950113    1C                   ENDDO
460000940414     C*
460100940418     C* CHIUSURA FILES
460200021021     C                   CLOSE     FNANM20L
460300021021     C                   CLOSE     FNBLT38L
460400020605     C                   CLOSE     FNBTT38L
460500020917     C                   CLOSE     FNFVV02L
460600001120     C                   CLOSE     FNFVV01L
460700940418     C*
460800940414     C                   ENDSR
460900970506     C*
461000970506     C* AGGIORNO LA DATA ENTRATA/DATA ENTRATA TRANSITO ---------------*
461100970506     C     AGGENT        BEGSR
461200970506     C* IMPOSTO LA KEY
461300970506     C                   EXSR      IMPKEY
461400041217     c                   clear                   Wdse0             1
461500041217     c
461600041217    1c                   if        *in40 and bltdse=0
461700041217     c                   eval      Wdse0='S'
461800041217    1c                   endif
461900970506     C**
462000970506     C* S P U N T A   P A R T E N Z A
462100970506     C**
462200970506     C* SE NON C'E' SPUNTA ENTRATA CONTROLLO SE NON L'HO GIA' SPARATO
462300970506     C* CERCO CON CATEGORIA 5
462400001117     C*
462500041217     C* Per bolla no poste: cerco spunta se DSE=0
462600041217     C* per bolla    poste: cerco spunta se DSE=0 prima dell'imi
462700041217     c* per       entrambe: cerco spunta se peso o volume=0
462800041217     c*                     di bolla con serie con utilizzo in spunta
462900041217     c*                     del peso o volume (da azorg)
463000041217    1c                   if        *in40
463100041217   1ac                   if        wdse0='S' or
463200041217     c                             (bltnrs>0 and bltvuc=0 and pa2spv='S') or
463300041217     c                             (bltnrs>0 and bltpuc=0 and pa2spp='S')
463400001121     C*
463500970506     C                   Z-ADD     5             WNPG
463600140618     C     KBRV55        CHAIN(N)  FNBRV05                            31
463700990917    2C                   SELECT
463800001123     C* Se fv PT do per scontato che p.o. poste è senza macchinone
463900001123     c* Quindi, in ricerca spunte entrata faccio solo una chain e non
464000001123     c* un ciclo di lettura
464100990917     C     *IN31         WHENEQ    *OFF
464200040121     c     pa2spv        andeq     ' '
464300040121     c     pa2spp        andeq     ' '
464400990917     C                   EXSR      AGGEN2
464500001117     C* N.B.: SE CML ESEGUE CICLO LETTURA DI TUTTE LE SPUNTE ENTRATA PE
464600001117     C*       R AVERE LA CERTEZZA DI MEMORIZZARE IL PESO E IL VOLUME
464700990917     C     *IN31         WHENEQ    *OFF
464800040121     c     pa2spv        andne     ' '
464900040121     c     *in31         oreq      *off
465000040121     c     pa2spp        andne     ' '
465100990917    3C     *IN31         DOWEQ     *OFF
465200990917     C                   EXSR      AGGEN2
465300140618     C     KBRV55        READE(N)  FNBRV05                                31
465400990917    3C                   ENDDO
465500990917     C*
465600990917   X2C                   OTHER
465700041217     c* La cat 1 o qualsiasi spunta  le cerco solo per dse=0
465800041217   2ac                   if        wdse0='S'
465900990917     C*
466000970506     C* SE NON C'E' CERCO CON CATEGORIA 1
466100970506     C                   Z-ADD     1             WNPG
466200140618     C     KBRV5         CHAIN(N)  FNBRV05                            31
466300990917    3C     *IN31         IFEQ      *OFF
466400990917     C                   EXSR      AGGEN2
466500990917   X3C                   ELSE
466600970506     C*
466700970506     C* ALTRIMENTI CERCO IN UNA QUALSIASI ALTRA CATEGORIA
466800070123     C     KBRV6         CHAIN     FNBRV07                            31
466900990917     C  N31              EXSR      AGGEN2
467000990917    3C                   ENDIF
467100041217   2ac                   ENDIF
467200041217    2C                   ENDSL
467300970506     C**
467400041217     C* SE C'ERA VOLUME CML memorizzo per eseguire a fine il calcolo
467500990917    2C     BLTVUC        IFGT      0
467600990917     C     BLTPUC        ORGT      0
467700041217     c                   eval      wcalLSB7='S'
467800990917    2C                   ENDIF
467900041217   1aC                   ENDIF
468000041217    1C                   ENDIF
468100970506     C**
468200970506     C* S P U N T A   T R A N S I T O
468300970506     C**
468400970506     C* SE NON C'E' DATA ENTRATA AL TRANSITO CONTROLLO SE L'HO SPARATO
468500970506    1C   41BTTDET        IFEQ      0
468600970506     C*  CERCO IN UNA QUALSIASI CATEGORIA
468700070123     C     KBRV6         CHAIN     FNBRV07                            31
468800990917    2C     *IN31         DOWEQ     *OFF
468900990917     C                   EXSR      AGGEN2
469000990917     C**
469100990917     C* SE HO CML VADO AVANTI A LEGGERE
469200040121     c     pa2spv        ifne      *blanks
469300040121     c     pa2spp        orne      *blanks
469400070123     C     KBRV6         READE     FNBRV07                                31
469500990917     C                   ELSE
469600990917     C                   SETON                                        31
469700990917     C                   ENDIF
469800990917    2C                   ENDDO
469900990917     C**
470000990917    2C     BTTVUC        IFGT      0
470100990917     C     BTTPUC        ORGT      0
470200041217     c                   eval      wcalLSB7='S'
470300990917    2C                   ENDIF
470400990917     C*
470500970506    1C                   END
470600970506     C                   ENDSR
470700990917     C*
470800990917     C* CALL A PGM FNLS44R PER AGGIORNAMENTO -------------------------*
470900990917     C     AGGEN2        BEGSR
471000990917     C*
471100990917     C* SE NON E' PISTOLA CREATA IN AUTOMATICO, AGGIORNO BOLLA
471200990917     C     BRVNPS        IFGT      0
471300990917     C     BRVNPS        LOOKUP    NPS                                    31
471400990917     C                   ELSE
471500990917     C                   SETOFF                                       31
471600990917     C                   ENDIF
471700990917     C**
471800990917    3C     *IN31         IFEQ      *OFF
471900990917     C*
472000990917     C                   EXSR      CALL44
472100000731     C     *IN40         IFEQ      *ON
472200000731     C* CHAINO FNBLT PER REPERIRE LA DATA ENTRATA AGGIORNATA
472300021021     C* ANCHE SE SONO NEL CICLO DI LETTURA FNBLT38L NON MI PREOCCUPO
472400000731     C* CHE SI SPORCHINO I CAMPI PERCHE' CON QUESTA CHAIN VADO A
472500000731     C* RILEGGERMI LO STESSO RECORD
472600040319     c* stessa cosa per fnbtt
472700000731     C     KBLT1         CHAIN(N)  FNBLT01L                           31
472800040319     c                   else
472900040319     c     *in41         ifeq      *on
473000040319     C     KBTT1         CHAIN     FNBTT11L                           31
473100040319     c                   endif
473200000731     C                   ENDIF
473300990917     C                   ENDIF
473400990917     C                   ENDSR
473500940414     C*
473600970828     C* CREO SPUNTE PARTENZA --------------------------------------*
473700940414     C     CRTBRC        BEGSR
473800940414     C                   SETOFF                                       33
473900950412     C                   CLEAR                   WDATU
474000970828     C                   CLEAR                   UPDBRV
474100940414     C*
474200950109     C                   Z-ADD     1             WNPG
474300020807    1C     *IN43         IFEQ      *OFF
474400970423     C* CHAIN PER NO SPUNTE ENTRATA
474500140618     C     KBRV5         CHAIN     FNBRV05                            31
474600961220     C*
474700970423    1C                   ELSE
474800040624     c
474900970423     C* CERCO SE C'E' LA BOLLA PARTENZA E NON E' BORDERIZZATA NON CREO
475000040624     C*  LA SPUNTA PARTENZA, se lnp in £1
475100960506     C     KBRV4         CHAIN(N)  FNBLT27L                           91
475200040624     c                   if        not *in91
475300040624     c     bltlnp        lookup    l1                                     37
475400040624     c                   if        not *in37
475500040624     c                   seton                                        91
475600040624     c                   endif
475700040624     c                   endif
475800040624     c
475900960506    2C     *IN91         IFEQ      *OFF
476000970507     C* CHAINO LA BOLLA
476100970507     C                   EXSR      CHABOL
476200020807     C* NON TROVATA--> COME  SE NON CI FOSSE
476300970507     C     WBOL          IFEQ      'N'
476400970507     C                   SETON                                        91
476500970507     C                   ENDIF
476600970506     C**
476700970506    3C  N91BLPFT1        IFEQ      ' '
476800961216     C     BLPDBR        ANDEQ     0
476900960503     C                   SETON                                        3133
477000960506   X3C                   ELSE
477100961220     C*
477200961220     C                   SETON                                        91
477300961220     C* SE SI TRATTA DI UNA BOLLA BORDERIZZATA SU FV ANCORA DA CHIUDERE
477400961220     C*  NON CREO SPUNTA PARTENZA
477500961220    4C     BLPFT1        IFEQ      ' '
477600961220     C     BLPNFV        ANDGT     0
477700961220     C     BLPNFV        ANDNE     FGV2
477800970424     C                   CLEAR                   DSLV53
477900970424     C                   MOVEL     BLPNFV        D53NFV
478000970424     C                   MOVEL     'G'           D53TFO
478100971107     C                   MOVEL     FGVLNP        D53FEL
478200020919     C                   Z-ADD     BLPTFP        D53FGS
478300970424     C                   CALL      'FNLV53R'
478400970424     C                   PARM                    DSLV53
478500970424    5C     D53FCF        IFEQ      ' '
478600961220     C                   SETON                                        3133
478700961220     C                   SETOFF                                       91
478800961220    5C                   ENDIF
478900961220    4C                   ENDIF
479000960506    3C                   ENDIF
479100970506     C*
479200970506   X2C                   ELSE
479300160622     C* NON TROVATA BOLLA PARTENZA
479400160622     c*  oppure bolla non del terminal di partenza:
479500160622     c*  se bolla arrivo non creo spunta partenza
479600160622     c*  devo tener conto anche se il secondo liv in partenza è un
479700160622     c*  terminal di arrivo (anche con 2°liv)
479800970506     C                   SETOFF                                       3435
479900160622     c* solo se non ho trovato fnblt rifaccio lokkup in £1
480000160622     c                   if        not %found(fnblt27l)
480100970506     C     KLNP          LOOKUP    L1                                     34
480200160622     c                   endif
480300130530     C*
480400031113     c     *in34         ifeq      *off
480500130530     c                   clear                   dslv55
480600130530     c                   movel     'A'           d55tpt
480700160622     c                   if        not %found(fnblt27l)
480800130530     c                   z-add     klnp          d55lnp
480900160622     c                   else
481000160622     c                   z-add     bltlnp        d55lnp
481100160622     c                   endif
481200130530     c                   z-add     klna          d55lin
481300130530     c                   z-add     wdfv          d55drf
481400130530     c                   call      'FNLV55R'
481500130530     c                   parm                    dslv55
481600130530     c     d55tfa        ifeq      fgvlnp
481700160622     c     d55tfa        oreq      brvpes
481800031113     c                   seton                                        3133
481900031113     c                   setoff                                       91
482000031113     c                   endif
482100031113     c                   endif
482200970506     C*
482300970506    2C                   ENDIF
482400960503     C*
482500140618     C   91KBRV5         CHAIN     FNBRV05                            31
482600961220    1C                   ENDIF
482700940414      *
482800970423      * 31 ON -NON ESISTE SPUNTA PARTENZA --> CREO SPUNTA (33OFF)
482900970423      * 31 OFF-    ESISTE SPUNTA PARTENZA :
483000970423     C**
483100950208    0C     *IN31         DOWEQ     *OFF
483200970828     C   42
483300970828     COR 43WRVVUC        IFGT      0
483400970828     C     BRVVUC        ANDEQ     0
483500971114     C                   Z-ADD     WRVVUC        BRVVUC
483600971114     C                   MOVEL     'S'           UPDBRV
483700970828     C                   ENDIF
483800990906     C   42
483900990906     COR 43WRVPUC        IFGT      0
484000990906     C     BRVPUC        ANDEQ     0
484100990906     C                   Z-ADD     WRVPUC        BRVPUC
484200990906     C                   MOVEL     'S'           UPDBRV
484300990906     C                   ENDIF
484400970423      * - BRVDTR =0 -->
484500950208    1C     BRVDTR        IFEQ      0
484600940414      *
484700970423      * FOGLIO <> DA QUELLO IN CHIUSURA
484800950109    2C     BRVNFV        IFNE      FGV2
484900940414      *
485000970424     C                   CLEAR                   DSLV53
485100000111     C                   Z-ADD     BRVNFV        D53NFV
485200970424     C                   MOVEL     'G'           D53TFO
485300020919     C                   Z-ADD     BRVFGS        D53FGS
485400970424     C                   CALL      'FNLV53R'
485500970424     C                   PARM                    DSLV53
485600940414      *
485700970423      * ESISTE FOGLIO --> SE E' APERTO NON ANNULLATO NON RICREO(33ON)
485800970424   3AC     D53ERR        IFEQ      ' '
485900970424    3C     D53FCF        IFEQ      ' '
486000970424     C     D53ATB        ANDEQ     ' '
486100970909     C     D53DEF        ANDEQ     ' '
486200940414     C                   SETON                                        33
486300950412     C                   ELSE
486400950412     C*
486500970424    4C     D53DFV        IFEQ      WDTP
486600950915     C* SE NON STO LEGGENDO SPUNTE ENTRATA CHAINO IN UPDAT
486700970828     C*  PERCHE' DEVO RIAGGIORNARLA CON I DATI DI QESTO FV
486800950915    5C     *IN43         IFEQ      *OFF
486900950412     C                   MOVEL     'S'           WDATU
487000950418     C                   MOVEL     BRVNFV        WSANFV
487100950418     C                   MOVEL     BRVFGS        WSAFGS
487200950915     C*
487300950915     C                   ELSE
487400950915     C* SE DA SPUNTA ENTRATA NON RICREO LA SPUNTA SE E' DI OGGI
487500950915     C                   SETON                                        33
487600950915    5C                   ENDIF
487700950915    4C                   END
487800950412    3C                   END
487900970424   3AC                   END
488000940414      *
488100940414      * NON ESISTE FOGLIO
488200970423      * O FOGLIO CHIUSO E ANNULLATO --> RICREO SPUNTA (VADO IN UPDATE)
488300940506      *                                                     33OFF
488400940506      *
488500950412   X2C                   ELSE
488600940506      *                        FOGLIO =  A QUELLO IN CHIUSURA: NON RICR
488700940506      *                                                     33ON
488800940414     C                   SETON                                        33
488900940414    2C                   END
489000950412     C*
489100950412   X1C                   ELSE
489200970423      *
489300970423      * - BRVDTR><0 --> SPUNTA GIA'TRASMESSA MA NON AGGIORNATA BOLLA:
489400970423      *                 SFLAGGO SEMPLICEMENTE LA SPUNTA E BASTA
489500971114    2C     BRVNFV        IFEQ      FGV2
489600971114     C  N43              CLEAR                   BRVDTR
489700000111     C  N43              CLEAR                   BRVHTR
489800971114     C  N43              CLEAR                   BRVFTR
489900950412     C*
490000971114     C  N43              MOVEL     'S'           UPDBRV            1
490100950412     C                   SETON                                        33
490200950412     C*
490300950915   X2C                   ELSE
490400970424      *
490500970424     C                   CLEAR                   DSLV53
490600000111     C                   Z-ADD     BRVNFV        D53NFV
490700970424     C                   MOVEL     'G'           D53TFO
490800020919     C                   Z-ADD     BrvFGS        D53FGS
490900970424     C                   CALL      'FNLV53R'
491000970424     C                   PARM                    DSLV53
491100950412     C* SE IL FOGLIO DELLA SPUNTA TROVATA NON HA LA STESSA DATA DEL FV
491200950412     C*  CHE STO CHIUDENDO--> TENGO ENTRAMBE LE SPUNTE, ALTRIMENTI NO
491300970424    3C     D53DFV        IFEQ      WDTP
491400970424    3C     D53ERR        ANDEQ     ' '
491500950915     C* SE NON STO LEGGENDO SPUNTE ENTRATA CHAINO IN UPDAT
491600950915    4C     *IN43         IFEQ      *OFF
491700950412     C                   MOVEL     'S'           WDATU             1
491800950418     C                   MOVEL     BRVNFV        WSANFV
491900950418     C                   MOVEL     BRVFGS        WSAFGS
492000950915     C*
492100950915   X4C                   ELSE
492200950915     C* SE DA SPUNTA ENTRATA NON RICREO LA SPUNTA PARTENZA DI OGGI
492300980316     C* (SOLO SE SPUNTA NON 'L')
492400980316     C     BRVFTR        IFNE      'L'
492500950915     C                   SETON                                        33
492600980316     C                   ELSE
492700980316     C                   MOVEL     'S'           WDATU             1
492800980316     C                   MOVEL     BRVNFV        WSANFV
492900980316     C                   MOVEL     BRVFGS        WSAFGS
493000980316     C                   ENDIF
493100950915    4C                   ENDIF
493200950915    3C                   ENDIF
493300950412     C*
493400950412    2C                   ENDIF
493500950412    1C                   ENDIF
493600950208     C*
493700950208     C* SE LA SPUNTA NON E' QUELLA BUONA VEDO SE CE N'E' UN'ALTRA
493800950412    1C     *IN33         IFEQ      *OFF
493900140618     C     KBRV5         READE     FNBRV05                                31
494000950412   X1C                   ELSE
494100950208     C* SE TROVATA SPUNTA BUONA --> ESCO
494200950208     C                   SETON                                        31
494300991019     C*
494400991019     C* CONTROLLO PESO E VOLUME DA BLT E BTT PERCHE'DEVONO ESSERE SEMPR
494500991019     C*  POSSIBILMENTE, QUESTI AD AGGIORNARE LE SPUNTE DA TRASMETTERE
494600991019     C*  IN ARRIVO
494700991019     C   40BLTVUC        IFGT      0
494800991019     C     BLTVUC        ANDNE     BRVVUC
494900991019     C                   Z-ADD     BLTVUC        BRVVUC
495000991019     C                   MOVEL     'S'           UPDBRV
495100991019     C                   ENDIF
495200991019     C   40BLTPUC        IFGT      0
495300991019     C     BLTPUC        ANDNE     BRVPUC
495400991019     C                   Z-ADD     BLTPUC        BRVPUC
495500991019     C                   MOVEL     'S'           UPDBRV
495600991019     C                   ENDIF
495700991019     C**
495800991019     C   41BTTVUC        IFGT      0
495900991019     C     BTTVUC        ANDNE     BRVVUC
496000991019     C                   Z-ADD     BTTVUC        BRVVUC
496100991019     C                   MOVEL     'S'           UPDBRV
496200991019     C                   ENDIF
496300991019     C   41BTTPUC        IFGT      0
496400991019     C     BTTPUC        ANDNE     BRVPUC
496500991019     C                   Z-ADD     BTTPUC        BRVPUC
496600991019     C                   MOVEL     'S'           UPDBRV
496700991019     C                   ENDIF
496800990917     C*
496900970828    3C     UPDBRV        IFEQ      'S'
497000140618     C                   UPDATE    FNBRV05
497100970828     C                   ELSE
497200140618     C                   UNLOCK    FNBRV05L
497300970828    3C                   ENDIF
497400991019     C**
497500991019    1C                   ENDIF
497600950208    0C                   ENDDO
497700970423     C**
497800970423     C* CONTROLLO SE COLLO SPUNTATO O MENO IN DATA ODIERNA
497900970423     C*  PER CREARE SPUNTE PISTOLE 98 96
498000970828    1C     *IN33         IFEQ      *OFF
498100970828     C* ENTRO ANCHE PER LEGGERE LE SPUNTE SE DEVO CERCARE IL VOLUME
498200970423     C                   CLEAR                   WNPS
498300170621     C                   CLEAR                   spufgs
498400070123     C     KBRV6         SETGT     FNBRV07L
498500070123     C     KBRV6         READPE    FNBRV07L                               34
498600970828    2C     *IN34         DOWEQ     *OFF
498700970828     C**
498800970828     C* VEDO SE SPUNTA CON VOLUME CML
498900970828     C   42
499000970828     COR 43WRVVUC        IFEQ      0
499100970828    3C     BRVVUC        ANDGT     0
499200970828     C                   Z-ADD     BRVVUC        WRVVUC
499300970828    3C                   ENDIF
499400000428     C* VEDO SE SPUNTA CON PESO CML
499500990906     C   42
499600990906     COR 43WRVPUC        IFEQ      0
499700990906    3C     BRVPUC        ANDGT     0
499800990906     C                   Z-ADD     BRVPUC        WRVPUC
499900990906    3C                   ENDIF
500000970828     C**
500100970423     C* ESCLUDO LE CATEGORIE 1
500200970828    3C     BRVNPG        IFNE      1
500300980116     C* AD ECCEZIONE DELLE SPUNTE PARTENZA SU FV NON CONTENENTE BRVLNA
500400980116     C* (BRVFTR='L')
500500980116     C     BRVFTR        OREQ      'L'
500600970424     C* CALL AL PGM PER REPERIRE LA DATA FOGLIO
500700970424     C                   CLEAR                   DSLV53
500800970424     C                   MOVEL     BRVNPG        D53NPG
500900000111     C                   Z-ADD     BRVNFV        D53NFV
501000970424     C                   MOVEL     BRVFGS        D53FGS
501100970424     C                   CALL      'FNLV53R'
501200970424     C                   PARM                    DSLV53
501300970424     C*
501400970828    4C     D53DFV        IFGE      WDTP
501500170621     c* Memeorizzo solo se non ho già trovato la spunta del terminal
501600170621     c                   if        spufgs<>simfel
501700170613     c* memorizzo se spunta fatta dal terminal o dal secondo liv
501800970423     C                   MOVEL     98            WNPS
501900970528     C                   MOVEL     BRVNPS        SPUNPS
502000170613     C                   movel     brvfgs        spufgs
502100170613     c* Se spunta terminal partenza --> posso uscire dal ciclo di lettura
502200170613     c                   if        brvfgs=simfel
502300970423     C                   SETON                                        34
502400170613     C                   endif
502500170621     c
502600170621     c                   else
502700170621     c                   seton                                        34
502800170621     C                   endif
502900170613     C
503000970828   X4C                   ELSE
503100970828    5C     WNPS          IFNE      98
503200970423     C                   MOVEL     96            WNPS
503300970528     C                   MOVEL     BRVNPS        SPUNPS
503400170613     C                   movel     brvfgs        spufgs
503500970828    5C                   ENDIF
503600970828    4C                   ENDIF
503700970828    3C                   ENDIF
503800970423     C*
503900970828    3C     *IN34         IFEQ      *OFF
504000040121     c     pa2spv        oreq      'S'
504100970828     C     WRVVUC        ANDEQ     0
504200970828     C     *IN40         ANDEQ     *OFF
504300970828     C     *IN41         ANDEQ     *OFF
504400990907      *PESO
504500040121     c     pa2spp        oreq      'S'
504600990907     C     WRVPUC        ANDEQ     0
504700990907     C     *IN40         ANDEQ     *OFF
504800990907     C     *IN41         ANDEQ     *OFF
504900070123     C     KBRV6         READPE    FNBRV07L                               34
505000970828    3C                   ENDIF
505100970828    2C                   ENDDO
505200970423     C*
505300991019    1C                   ENDIF
505400950208     C*
505500980206     C                   CLEAR                   WBRVE
505600950208     C* SE HO TROVATO UNA SPUNTA LA RICHAINO
505700970828    0C     WDATU         IFEQ      'S'
505800970828     C     *IN33         ANDEQ     *OFF
505900070123     C     KBRV2         CHAIN     FNBRV02                            31
506000980206     C* MEMORIZZO SPUNTA PARTENZA ERRATA IN CAMPI DEL FILE STORICO
506100980206     C* SPUNTE PARTENZA ERRATE
506200980206     C  N31BRVFTR        IFEQ      'L'
506300980206     C                   MOVE      'S'           WBRVE             1
506400980209     C                   MOVEL     DSBRV         SVBRVE
506500980206     C                   END
506600970828    0C                   ENDIF
506700950109     C*
506800950109     C* SOLO PER I TRANSITI: AUTOGENERO SPUNTA PER BOLLE TRANSITO
506900950109     C*                      SOLO SE RELATIVO FOGLIO ABBINATO
507000950412    0C   41BTTDET        IFEQ      0
507100950127     C* COME PRIMA COSA VERIFICO SE PER CASO LA SPUNTA ESISTE
507200950127     C* MA NON HA AGGIORNATO IL RECORD
507300950109     C                   EXSR      ABBBTT
507400950412    1C     WABBIN        IFNE      'S'
507500950118     C                   SETON                                        33
507600950412    1C                   ENDIF
507700950412    0C                   ENDIF
507800940512     C*
507900950412    0C  N33              DO
508000000714     C**
508100070123     C* SCARICO SPUNTA ERRATA IN FNBRVE
508200980206     C     WBRVE         IFEQ      'S'
508300980209     C* CONTROLLO CHE IL COLLO NON SIA IN IMP
508400070123     C     KBRV01        SETLL     FNBRV01L                               37
508500010611     c     *in37         ifeq      *off
508600070219     c     kbrve         setll     fnbrve                                 99
508700010611     C  n99              MOVEL     SVBRVE        DSBRV
508800070123     C  N99              WRITE     FNBRVE
508900010611     c                   endif
509000980206     C                   END
509100140618     C                   CLEAR                   FNBRV05
509200950109     C*
509300950118     C                   MOVEL     FGVLNP        BRVFGS
509400000111     C                   Z-ADD     FGVLNP        BRVPES
509500000111     C                   Z-ADD     FGV2          BRVNFV
509600950118     C                   Z-ADD     1             BRVNPG
509700971107     C                   Z-ADD     FGVLNP        BRVFLE
509800070220     c                   time                    wtimez
509900070220     c* millisecondi
510000070220     c                   extrct    wtimez:*ms    millisec          6 0
510100070220     c                   movel     millisec      brvmis
510200070220     c* data immissione
510300070220     c                   movel     wtimez        dataiso
510400070220     c                   movel     dataiso       brvdfs
510500070220     c* ora immissione
510600070220     c                   move      wtimez        oraiso
510700070220     c                   movel     oraiso        brvhms
510800070220     c                   movel     'A'           brvtap
510900070220     c                   eval      brvpru='§'+ nompgm
511000080328     c                   clear                   brvtsu
511100950109     C*
511200950915     C   40              EXSR      CRTBLT
511300950109     C*
511400950109     C   41              EXSR      CRTBTT
511500950109     C*
511600950113     C   42              EXSR      CRTSAN
511700940414     C*
511800950915     C   43              EXSR      CRTSPE
511900970423     C**
512000970423     C** VEDO SE IMPOSTARE PISTOLA 98 O 96
512100970506     C**  SOLO SE NON SI TRATTA DI UNA PISTOLA 90 CHE VINCE
512200970507     C**  ESCLUDO SEMPRE I LOCALI PER I QUALI LA PISTOLA 98 96 NON ESIS
512300970506     C**
512400970507    1C     BRVNPS        IFNE      90
512500970507     C*
512600970507    2C     WNPS          IFEQ      96
512700970507    3C     WLOCAL        IFNE      'S'
512800970423     C                   MOVEL     WNPS          BRVNPS
512900970528     C* PER BOLLE TRANSITO SE L'UNICA SPUNTA PRESENTE E' UNA 89 CREO
513000970528     C*  UNA 89
513100971027     C* ANCHE PER DISGUIDO
513200971027     C   42
513300971027     COR 41SPUNPS        IFEQ      89
513400970528     C                   MOVEL     SPUNPS        BRVNPS
513500970528     C                   ENDIF
513600970507     C                   ELSE
513700970507     C* LA 96 LOCALE DIVENTA --> 90
513800970507     C                   MOVEL     90            BRVNPS
513900970507    3C                   ENDIF
514000970507     C**
514100970507   X2C                   ELSE
514200970423     C**
514300170613     C* LA PISTOLA 98   per chi ha il flag §ogagp = blank
514400170613     C* dal 06/2017     vale per collo spuntato nel giorno SOLO dal secondo liv
514500170613     C*                 tranne in caso di SEMPRE autogenerare da qualsiasi spunt
514600170613     C*
514700970507    3C     WNPS          IFEQ      98
514800170613     C     WLOCAL        ANDNE     'S'
514900170613     c     spufgs        andne     simfel
515000170613     c     pa2ags        andne     'S'
515100170613     C***     PARCML        ANDEQ     ' '
515200170613     C***     WLOCAL        ANDNE     'S'
515300170613     C***     pa2agp        andeq     *blanks
515400970423     C                   MOVEL     WNPS          BRVNPS
515500970528     C* PER BOLLE TRANSITO SE L'UNICA SPUNTA PRESENTE E' UNA 89 CREO
515600971027     C*  UNA 89 ANCHE PER I DISGUIDI
515700971027     C   42
515800971027     COR 41SPUNPS        IFEQ      89
515900970528     C                   MOVEL     SPUNPS        BRVNPS
516000970528     C                   ENDIF
516100970507    3C                   ENDIF
516200970507    2C                   ENDIF
516300040406     C*
516400970507    1C                   ENDIF
516500950915     C*
516600140618     C   31              WRITE     FNBRV05
516700070123     C  N31              UPDATE    FNBRV02
516800950203     C*
516900950203     C* SE AUTOGENERATA SPUNTA PARTENZA: AGGIORNO BOLLA TRANSITO
517000970507    1C     *IN41         IFEQ      *ON
517100950203     C     BRVNPS        ANDEQ     89
517200970423     C                   EXSR      CALL44
517300970507    1C                   END
517400950412    0C                   END
517500940414     C*
517600940414     C                   ENDSR
517700950109     C*
517800950109     C*--- VEDO SE COLLO TRANSITO DICHIARATO PARTITO E ABBINATO-------*
517900950109     C     ABBBTT        BEGSR
518000950109     C*
518100950118     C* CHAIN SULLE SPUNTE PER VEDERE SE C'E' LA SPUNTA PARTENZA DA
518200950118     C*  ALTRA FILIALE --> SE NON C'E' NON CREO SPUNTA
518300950118     C*
518400950118     C* IN QUESTO CASO NON IMPORTA CHE TESTI SE LA SPUNTA NON E' DI
518500950118     C* SIMFEL PERCHE' SE CI FOSSE AVREI AGGIORNATO LA DATA ENTRATA
518600950118     C* E QUINDI NON SAREI DENTRO A QUESTA ROUTIME
518700950109     C                   Z-ADD     1             WNPG
518800950109     C                   CLEAR                   WABBIN
518900140618     C     KBTT          CHAIN(N)  FNBRV05                            34
519000970828     C**
519100950109    1C     *IN34         DOWEQ     *OFF
519200970828     C                   CLEAR                   DSLV53
519300020617     C                   MOVEL     'T'           D53TFO
519400070123     C****               Z-ADD     FGVLNP        D53FEL
519500000111     C                   Z-ADD     BRVNFV        D53NFV
519600970828     C                   MOVEL     BRVNPG        D53NPG
519700970828     C                   MOVEL     BRVFGS        D53FGS
519800070123     C****               MOVEL     BRVFLE        D53FLF
519900970828     C                   MOVEL     BRVLNA        D53LNA
520000970828     C                   MOVEL     'A'           D53ABB
520100020617     c                   z-add     fgvlnp        d53lai
520200970828     C                   CALL      'FNLV53R'
520300970828     C                   PARM                    DSLV53
520400970828    2C     D53ERR        IFEQ      ' '
520500970828     C     D53NFA        ANDGT     0
520600950113     C                   MOVEL     'S'           WABBIN            1
520700970828     C                   SETON                                        34
520800970828     C                   ELSE
520900950109     C*
521000140618     C     KBTT          READE(N)  FNBRV05                                34
521100970828    2C                   ENDIF
521200950113    1C                   ENDDO
521300950109     C*
521400950109     C                   ENDSR
521500970507     C*
521600970507     C* CHAINO BOLLA PARTENZA O BOLLA TRANSITO -----------------------*
521700040628     c     CHABOL        BEGSR
521800970507     C**
521900970507     C* CHAINO BOLLA PARTENZA
522000970507     C**
522100000620     C   45
522200000620     COR 43
522300970507    1COR 40BLTSPE        IFNE      SAVSPE
522400970507     C                   MOVEL     'S'           WBOL              1
522500970507     C                   CLEAR                   WLOCAL
522600970507     C     KBLT3         CHAIN(N)  FNBLP01L                           37
522700970507     C                   MOVEL     BLTSPE        SAVSPE           16 0
522800970507     C**
522900970507    2C     *IN37         IFEQ      *ON
523000970507     C                   MOVEL     'N'           WBOL
523100970507     C                   MOVEL     'S'           §1BFG5
523200970507   X2C                   ELSE
523300970507     C*
523400970507     C* CONTROLLO SE E' BOLLA LOCALE
523500970507    3C     BLPTFP        IFEQ      BLPTFA
523600970714     C     BLPLNP        OREQ      BLPLNA
523700970507     C                   MOVEL     'S'           WLOCAL            1
523800970507    3C                   ENDIF
523900970507     C*
524000970507     C* COD TRATTAMENTO MERCE CHE GENERA IN AUTOMATICO LE SPUNTE PARTEN
524100970507     C     BLPCTM        LOOKUP    CTM                                    37
524200970507     C*     TROVATO= IMPOSTO §1BFG5 = N
524300970507     C   37              MOVEL     'N'           §1BFG5
524400970507     C* NON TROVATO= IMPOSTO §1BFG5 = S
524500970507     C  N37              MOVEL     'S'           §1BFG5
524600970507    2C                   ENDIF
524700040624    1C                   ENDIF
524800040624     C**
524900970507     C* CHAIN BOLLA TRANSITO
525000970507     C**
525100970507     C   41BTTSPE        IFNE      SAVSPE
525200970507     C                   CLEAR                   WLOCAL
525300020605     C     KBTT3         CHAIN(N)  FNBTP11L                           37
525400970507     C                   MOVEL     BTTSPE        SAVSPE
525500970507     C                   ENDIF
525600970507     C                   ENDSR
525700950109     C*
525800950109     C* CREO SPUNTA DA FNBLT -----------------------------------------*
525900950109     C     CRTBLT        BEGSR
526000130226     c                   eval      wtfp90='N'
526100950109     C                   MOVEL     BLTFLS        BRVLNP
526200950109     C                   MOVEL     BLTLNA        BRVLNA
526300950109     C                   MOVEL     BLTNRS        BRVNRS
526400950113     C                   MOVEL     BLTNSC        BRVNSC
526500970507     C* CHAINO BOLLA
526600970507     C                   EXSR      CHABOL
526700950109     C*
526800970828     C                   MOVEL     BLPZNC        BRVZNC
526900950118     C                   MOVEL     BLTCAN        BRVCAN
527000970828     C                   MOVEL     BLTVUC        BRVVUC
527100990907     C                   MOVEL     BLTPUC        BRVPUC
527200950109     C*
527300960209     C* CONTROLLO SE ESISTE SPUNTA ENTRATA
527400170613    1C***44BLTDSE        IFEQ      0
527500170613    1C     BLTDSE        IFEQ      0
527600950109     C                   SETON                                        33
527700950109     C                   ELSE
527800950109     C                   SETOFF                                       33
527900950113    1C                   END
528000950109     C*
528100170613     C** SE NON PREVISTA GENERAZ.DA SPU.ENTRATA--> SEMPRE 88 O 90
528200170613     C**N44              SETON                                        33
528300950109     C*
528400950109     C*     ESISTE SPUNTA ENTRATA --> STESSO NUMERO PISTOLA
528500950113    1C     *IN33         IFEQ      '0'
528600950109     C                   MOVEL     BLTNPE        BRVNPS
528700950113   X1C                   ELSE
528800950109     C*
528900950109     C* NON ESISTE SPUNTA ENTRATA --> PISTOLA AUTOGENERATA 88 O 90
529000950109     C                   MOVEL     '88'          BRVNPS
529100130226     c* controllo se bolla con colli che partono da altra filiale
529200130226     c*  devo impostare pistola 90
529300130226     c                   if        blpxco='W'
529400130226     c                   exsr      Contr_XCO
529500130227     c
529600130227     c                   if        wtfp90='S'
529700130227     C                   MOVEL     90            BRVNPS
529800130226     c                   endif
529900130227     c                   endif
530000970507     C*
530100970423    2C     §1BFG5        IFEQ      'N'
530200970507     C     WLOCAL        OREQ      'S'
530300970507     C                   MOVEL     90            BRVNPS
530400970507    2C                   ENDIF
530500970507     C*
530600970423    1C                   END
530700950109     C*
530800950109     C                   ENDSR
530900970507     C*
531000130226     C* Controllo se collo partito da altro TFP ----------------------*
531100130226     C     Contr_XCO     BEGSR
531200130226     c
531300130227     C                   MOVEL     DSBRV         DSBRVS
531400130226     c                   eval      Wflsnrs=(blpfls*100)+bltnrs
531500130226
531600130226    1c                   if        wflsnrs<>savfnrs
531700130226     c                   clear                   xcotfp
531800130226     C                   MOVEL     'XCO'         kcod1
531900130226     C                   eval      kke1='ALTRITFP'
532000130226     C                   eval      kke2=%editc(wflsnrs:'X')
532100130226     c     ktbe2         chain     tntbe01l
532200130226    2c                   if        %found(tntbe01l) AND tbeatb=' '
532300130226     C                   MOVEA     TBeUNI        xcotfp
532400130226    2c                   endif
532500130226     c
532600130226     c                   eval      savfnrs=wflsnrs
532700130226    1c                   endif
532800130226     c
532900130226     c                   z-add     5             wnpg
533000130226    1c                   do        2
533100140618     c     kblt_xco      setll     fnbrv05l
533200140618     c     kblt_xco      reade     fnbrv05l
533300130226     c
533400140618    2c                   dow       not %eof(fnbrv05l)
533500130226    3c                   if        brvfle<>fgvlnp
533600130226     c* Verifico se la filiale che spunta è prevista
533700130226     c                   movel     brvfgs        alfafgs           3
533800130226     c                   eval      Indx=%lookup(alfafgs:xcotfp)
533900130226     c* Trovata --> memerizzo per impsostare pistola 90
534000130226    4c                   if        Indx>0
534100130226     c                   eval      Wtfp90='S'
534200130226     c                   leave
534300130226    4c                   endif
534400130226    3c                   endif
534500130226     c
534600140618     c     kblt_xco      reade     fnbrv05l
534700130226    2c                   enddo
534800130226     c
534900130226    2c                   if        wtfp90='S'
535000130226     c                   leave
535100130226     c                   else
535200130226     c                   eval      wnpg=1
535300130226    2c                   endif
535400130226     c
535500130226    1c                   enddo
535600130226     c
535700130227     C                   MOVEL     DSBRVS        DSBRV
535800130226     C                   ENDSR
535900130226     C*
536000130226     C* CREO SPUNTA DA FNBTT -----------------------------------------*
536100130226     C     CRTBTT        BEGSR
536200950109     C                   MOVEL     BTTFLS        BRVLNP
536300950109     C                   MOVEL     BTTLNA        BRVLNA
536400950109     C                   MOVEL     BTTNRS        BRVNRS
536500950113     C                   MOVEL     BTTNSC        BRVNSC
536600970507     C* CHAINO BOLLA
536700970507     C                   EXSR      CHABOL
536800950109     C*
536900950109     C                   MOVEL     BTPZNC        BRVZNC
537000990906     C                   MOVEL     BTTVUC        BRVVUC
537100990907     C                   MOVEL     BTTPUC        BRVPUC
537200950109     C                   MOVEL     BTTCAN        BRVCAN
537300950109     C* SE E' PISTOLA AUTOGENERATA SENZA SPUNTA IN ENTRATA:
537400950109     C* --> PISTOLA 89
537500950109     C     BTTDET        IFEQ      0
537600950109     C                   MOVEL     '89'          BRVNPS
537700950109     C                   ELSE
537800960209     C* GENERAZIONE SPUNTE 88
537900170613     C*  06/2017 SEMPRE spunta 88 non esiste più
538000170613     C**     *IN44         IFEQ      *OFF
538100170613     C**                   MOVEL     '88'          BRVNPS
538200170613     C**                   ELSE
538300960209     C* GENERAZIONE SPUNTE DA SPU.ENTRATA
538400950109     C                   MOVEL     BTTPET        BRVNPS
538500170613     C**                   ENDIF
538600950109     C                   ENDIF
538700950109     C*
538800950109     C                   ENDSR
538900950109     C*
539000950109     C* CREO SPUNTA DA ANOMALIE --------------------------------------*
539100950113     C     CRTSAN        BEGSR
539200950109     C                   MOVEL     ANMFLS        BRVLNP
539300950109     C                   MOVEL     ANMLNA        BRVLNA
539400950109     C                   MOVEL     ANMNRS        BRVNRS
539500950109     C                   MOVEL     ANMSCN        BRVNSC
539600950109     C                   MOVEL     WRVZNC        BRVZNC
539700970828     C                   MOVEL     WRVVUC        BRVVUC
539800990907     C                   MOVEL     WRVPUC        BRVPUC
539900950109     C                   MOVEL     WRVCAN        BRVCAN
540000960209     C* GENERAZIONE SPUNTE 88
540100170613     C*  06/2017 SEMPRE sopunta 88 non esiste più
540200170613     C**     *IN44         IFEQ      *OFF
540300170613     C**                   MOVEL     '88'          BRVNPS
540400170613     C**                   ELSE
540500960209     C* GENERAZIONE DA SPUNTE ENTRATA
540600950109     C                   MOVEL     ANMNPS        BRVNPS
540700170613     C**                   ENDIF
540800950109     C*
540900950109     C                   ENDSR
541000950915     C*
541100950915     C* CREO SPUNTA DA SPUNTE ENTRATA --------------------------------*
541200950915     C     CRTSPE        BEGSR
541300950915     C                   MOVEL     KLNP          BRVLNP
541400950915     C                   MOVEL     KLNA          BRVLNA
541500950915     C                   MOVEL     KNRS          BRVNRS
541600950915     C                   MOVEL     KNSC          BRVNSC
541700950915     C                   MOVEL     WRVZNC        BRVZNC
541800950915     C                   MOVEL     WRVVUC        BRVVUC
541900990907     C                   MOVEL     WRVPUC        BRVPUC
542000950915     C                   MOVEL     WRVCAN        BRVCAN
542100950915     C                   MOVEL     WRVNPS        BRVNPS
542200950915     C*
542300950915     C                   ENDSR
542400960531     C*
542500960705     C* CHIUDO ANMOALIE DI DISGUIDO SE ESISTE BOLLA PARTENZA ---------*
542600020412     C     SRDISG        BEGSR
542700070918     C**** kblt27        setll     FNBLT27L                               36
542800070918    2C**** *in36         IFEQ      *ON
542900070918     c
543000070918     C     kblt27        chain(N)  FNBLT27L
543100070918     c                   if        %found(fnblt27l)
543200070918     c* Se bolla trovata e del terminal di partenza
543300070918     c*  chiudo le anomalie 200 55 56
543400070918     c     bltlnp        lookup    l1                                     36
543500070918     c                   if        *in36
543600020412     C**TROVATA BOLLA PARTENZA: CHIUDO ANOMALIE 200 55 e 56
543700020412     C                   MOVE      'N'           WCRT              1
543800020412     C                   CLEAR                   DSLR33
543900020412     C                   MOVE      ANMDAO        D33DCH
544000020412     C                   MOVEL     'AN'          D33CCH
544100020412     C                   MOVE      ANMNR0        D33NRR
544200020412     C                   CALL      'FNLR33R'
544300020412     C                   PARM                    DSLR33
544400970828     C**CHIUDO ANCHE ANOMALIA 55
544500020412     C                   Z-ADD     55            COMCAA
544600020412     C     kanm31        CHAIN(N)  FNANM                              21
544700020412     C     *in21         IFEQ      *OFF
544800020412     C                   CLEAR                   DSLR33
544900020412     C                   MOVE      ANMDAO        D33DCH
545000020412     C                   MOVEL     'M'           D33FSC
545100020412     C                   MOVEL     'AN'          D33CCH
545200020412     C                   MOVEL     'S'           D33MAC
545300020412     C                   MOVE      ANMNR2        D33NRR
545400020412     C                   CALL      'FNLR33R'
545500020412     C                   PARM                    DSLR33
545600020412     C                   END
545700020412     C**CHIUDO ANCHE ANOMALIA 56
545800020412     C                   Z-ADD     56            COMCAA
545900020412     C     kanm31        CHAIN(N)  FNANM                              21
546000020412     C     *in21         IFEQ      *OFF
546100020412     C                   CLEAR                   DSLR33
546200020412     C                   MOVE      ANMDAO        D33DCH
546300020412     C                   MOVEL     'M'           D33FSC
546400020412     C                   MOVEL     'AN'          D33CCH
546500020412     C                   MOVEL     'S'           D33MAC
546600020412     C                   MOVE      ANMNR2        D33NRR
546700020412     C                   CALL      'FNLR33R'
546800020412     C                   PARM                    DSLR33
546900020412     C                   END
547000020412     C                   Z-ADD     200           COMCAA
547100020412    2C                   END
547200070918    2C                   END
547300070918     c
547400020412     C                   ENDSR
547500020411     C*--- RICERCO LNP BOLLA SE COLLO con serie > 0  -----------------*
547600960531     C     RICLNP        BEGSR
547700960531     C*
547800960611     C                   CLEAR                   WOKBL
547900960611     C                   CLEAR                   WOKBT
548000960603     C                   MOVE      BRVLNP        WLNP
548100970811     C                   CLEAR                   WTAB5F
548200960531     C*
548300020411    1C     BRVNRS        IFGT      *ZEROS
548400150610     c     w_FilBsp      oreq      'S'
548500020412     c* CERCO LA BOLLA
548600020412    2C                   SELECT
548700020411     C     WALCF         WHENEQ    'N'
548800020411     C     KBRV4         CHAIN(N)  FNBLT27L                           36
548900020411     c     *in36         ifeq      *off
549000020411     C                   MOVE      BLTLNP        WLNP
549100020411     C                   MOVE      'S'           WOKBL             1
549200020411     C                   CLEAR                   WTAB5F
549300020411     c                   else
549400020606     C     KBtt37        CHAIN(N)  FNBTT37L                           36
549500020411     c     *in36         ifeq      *off
549600020411     C                   MOVE      BTTLNP        WLNP
549700020411     C                   MOVE      'S'           WOKBT             1
549800020411     C                   CLEAR                   WTAB5F
549900020411     c                   endif
550000020411     c                   endif
550100020411     C     WALCF         WHENEQ    'S'
550200020411     C     KBRV4         CHAIN     FNBLT27L                           36
550300020411     c     *in36         ifeq      *off
550400020411     C                   MOVE      BLTLNP        WLNP
550500020411     C                   MOVE      'S'           WOKBL             1
550600020411     C                   CLEAR                   WTAB5F
550700020411     c                   else
550800020606     C     KBtt37        CHAIN     FNBTT37L                           36
550900020411     c     *in36         ifeq      *off
551000020411     C                   MOVE      BTTLNP        WLNP
551100020411     C                   MOVE      'S'           WOKBT             1
551200020411     C                   CLEAR                   WTAB5F
551300020411     c                   endif
551400020411     c                   endif
551500020412    2C                   ENDSL
551600020412     c* Se non trovata bolla part. nè bolla transito GUARDO SE ESISTE LA
551700020412     c* BOLLA ARRIVI (in un ottica di As unico infatti se la spunta letta
551800020411     c* è un disguido potrei trovare la bolla in arrivo se sull'As in cui
551900020411     c* il collo è stato disguidato risiedono anche i dati del p.o. di
552000020411     c* arrivo del collo)
552100020412    2c     *in36         ifeq      *on
552200020411     c     kbrv4         chain     fnart27l                           36
552300020412    3c     *in36         ifeq      *off
552400020411     C                   MOVE      artlnp        WLNP
552500020411     C                   CLEAR                   WTAB5F
552600020412   x3c                   else
552700020411     c* BOLLA NON TROVATA IN NESSUN ARCHIVIO:
552800020412     c* SPUNTA PARTENZA NON DI DISGUIDO: verifico se è presente
552900020411     c                   exsr      cntspa
553000020412    4c     w105          ifeq      *blanks
553100020412     c* VADO SU TABELLA 5F se non ho trovato nemmeno la spunta partenza
553200020412     C                   movel     fgvlnp        W008A             8
553300020412     C                   move      brvnrs        W005A             5
553400020412     C                   movel     brvlnp        w005a
553500020412     C                   move      w005a         w008a
553600070215     C                   z-add     1             ii                4 0
553700020412     C     w008a         lookup    s5f(ii)                                31
553800020412     C**
553900020412     C* SE NON TROVATO CERCO COME SERIE = 99
554000020412    5C     *in31         ifeq      *off
554100020412     C                   move      99            W008A
554200020412     C                   z-add     1             ii
554300020412     C     w008a         lookup    s5f(ii)                                31
554400020412    5C                   endif
554500020412     C**
554600020412    5C     *in31         ifeq      *on
554700020412     C                   MOVEL     'S'           WTAB5F            1
554800020412     c     ii            occur     dse5f
554900020412     c                   move      e5f(1)        wlnp
555000020412    5c                   endif
555100020411     c*
555200020412    4c                   endif
555300020412    3c                   endif
555400020412    2c                   endif
555500020412    1c                   endif
555600971028     C                   ENDSR
555700971028     C*
555800970827     C*--- CONTROLLO SE C'E' UNA SPUNTA PARTENZA ---------------------*
555900970827     C     CNTSPA        BEGSR
556000970827     C                   MOVE      *BLANKS       W105              1
556100070123     C* Mi salvo dati record di fnbrv prima di leggere
556200070123     C                   MOVEL     DSBRV         DSBRVS
556300140618     C     KBRV03        SETLL     FNBRV05L
556400140618     C     KBRV03        READE(N)  FNBRV05L                               21
556500971028    1C     *IN21         DOWEQ     *OFF
556600960830     C     W105          ANDEQ     *BLANKS
556700971105    2C     BRVFGS        IFNE      FGVLNP
556800970827     C     BRVATR        ANDEQ     ' '
556900960830     C                   MOVE      '1'           W105
557000970827     C                   MOVEL     BRVFGS        WLNP
557100970827     C                   CLEAR                   WTAB5F
557200140618     C     KBRV03        READE(N)  FNBRV05L                               21
557300971028   X2C                   ELSE
557400140618     C     KBRV03        READE(N)  FNBRV05L                               21
557500971028    2C                   ENDIF
557600971028    1C                   ENDDO
557700070123     C* Reimposto campi di fnbrv con dati salvati
557800960830     C                   MOVEL     DSBRVS        DSBRV
557900960830     C                   ENDSR
558000970423     C*
558100130527     C* RICHIAMO FNLS45R PER AGGIORNAMENTO DATE SU BOLLE ------------*
558200970423     C     CALL44        BEGSR
558300070221     c                   clear                   fnls45ds
558400070221     c                   movel     'S'           dls45ric
558500070221     c                   move      brvpes        dls45pes
558600070221     c                   call      'FNLS45R'
558700070221     c                   parm                    fnls45ds
558800070222     c                   parm                    dsbrv
558900970423     C*
559000970509     C                   MOVEL     'S'           WFC44             1
559100970423     C                   ENDSR
559200970423     C*
559300970423     C* IMPOSTO LA KEY PER CHAIN -------------------------------------*
559400970423     C     IMPKEY        BEGSR
559500970423     C                   SELECT
559600970423     C     *IN40         WHENEQ    *ON
559700970423     C                   MOVEL     BLTFLS        KLNP
559800970423     C                   MOVEL     BLTLNA        KLNA
559900970423     C                   MOVEL     BLTNRS        KNRS
560000970423     C                   MOVEL     BLTNSC        KNSC
560100970423     C*
560200970423     C     *IN42         WHENEQ    *ON
560300970423     C                   MOVEL     ANMFLS        KLNP
560400970423     C                   MOVEL     ANMLNA        KLNA
560500970423     C                   MOVEL     ANMNRS        KNRS
560600970423     C                   MOVEL     ANMSCN        KNSC
560700970423     C*
560800970423     C     *IN41         WHENEQ    *ON
560900970423     C                   MOVEL     BTTFLS        KLNP
561000970423     C                   MOVEL     BTTLNA        KLNA
561100970423     C                   MOVEL     BTTNRS        KNRS
561200970423     C                   MOVEL     BTTNSC        KNSC
561300990917     C                   OTHER
561400970423     C                   MOVEL     BRVLNP        KLNP
561500970423     C                   MOVEL     BRVLNA        KLNA
561600970423     C                   MOVEL     BRVNRS        KNRS
561700970423     C                   MOVEL     BRVNSC        KNSC
561800970423     C                   ENDSL
561900970423     C                   ENDSR
562000010309     C*
562100010309     C* Aggiorno e sflaggo file O.R.M. -------------------------------*
562200010309     C     AGGORM        BEGSR
562300010309     C                   MOVEL     'M'           WTRC4
562400060220     C     KAR4          CHAIN     FIAR401L                           20
562500051207    1C     *IN20         IFEQ      *OFF
562600060220     C                   MOVEL     AR4NOT        DSBL4M
562700010309     C     KORM          CHAIN     FNORM01L                           20
562800060220     c* Se il p.o. di ritiro non è uguale alla linea di partenza,
562900051207     c*  aggiorno in questo momento
563000060220    2c                   if        not *in20 and ormpor<>AR4lnp
563100051207     c                   exsr      sr_Storico
563200051207     c* Scrivo fase 200
563300051207     c                   z-add     200           ormfao
563400051207     C                   Z-ADD     FGVDFV        ORMDFO
563500051207     C                   Z-ADD     235858        ORMOFO
563600051207     c                   exsr      rieorf
563700051207     c                   write     fnorf000
563800051207     c
563900060220     c                   z-add     AR4lnp        ormpor
564000070711      * se ho aggiornato il p.o. ritiro dell'ORM devo aggiornare anche FNORG
564100070711     c     korm          chain     fnorg01l
564200070711if  1c                   if        %found(fnorg01l)
564300070711      * cambio il p.o. ritiro
564400070711     c                   eval      orgpor = ormpor
564500070711     c                   update    fnorg000
564600070711    2c                   endif
564700051207    2c                   endif
564800051207     c
564900051207    2c     *IN20         IFEQ      *OFF
565000010312     c     ormfao        andne     900
565100010309     c                   z-add     900           ormfao
565200010309     C                   Z-ADD     FGVDFV        ORMDFO
565300010405     C                   Z-ADD     235959        ORMOFO
565400040909     C                   z-add     dateu         ORMDTT
565500010309     c                   exsr      rieorf
565600070711      * imposto ormeti
565700070711     c                   clear                   dfar
565800070711     c                   clear                   tibs02ds
565900070711     c                   eval      t02mod = 'C'
566000070711     c                   eval      t02sif = knsif
566100070711     c                   eval      t02cod = 'FAR'
566200070711     c                   movel(p)  ormfao        t02ke1
566300070711     c                   call      'TIBS02R'
566400070711     c                   parm                    kpjba
566500070711     c                   parm                    tibs02ds
566600070711     c                   eval      dfar = t02uni
566700070711     c                   eval      ormeti = d§farass
566800010309     c                   write     fnorf000
566900051207    2C                   ENDIF
567000051207    c
567100051207     C  n20              UPDATE    FNORM000
567200051207    1C                   ENDIF
567300010309     C                   ENDSR
567400010309     C*
567500010309     C* Riempio campi record fase O.R.M. -------------------------------*
567600010309     C     RIEORF        BEGSR
567700010309     C                   clear                   fnorf000
567800010309     C                   eval      ORFpoe = ormpoe
567900010309     C                   eval      ORFnsr = ormnsr
568000010309     C                   eval      ORFnor = ormnor
568100010309     C                   eval      ORFnrv = ormnrv
568200010309     C                   eval      ORFpog = ormpor
568300010309     C                   eval      ORFdae = ormdfo
568400010309     C                   eval      ORFore = ormofo
568500010309     C                   eval      ORFfar = ormfao
568600040909     C                   eval      ORFpue = knmus
568700040909     c                   If        OrmNdc > *Zeros
568800060516      * devo trovare il p.o. gestione distinta giusto
568900120607     c                   move      ormnpg        kdstnpg
569000120607     c                   move      ormndc        kdstnfv
569100060516      * prima provo con il p.o. ritiro
569200060516     c                   eval      kdstfgs = ormpor
569300120607     c     kdst          chain     fidst01l
569400060516      * se non lo trovo cerco se il p.o. ritiro è gestito
569500120607     c                   if        not %found(fidst01l)
569600060516     c                   clear                   dslv55
569700060516     c                   eval      d55tpt = '6'
569800060516     c                   move      ormpor        d55lin
569900060516     c                   eval      d55drf = ormddc
570000060516     c                   call      'FNLV55R'
570100060516     c                   parm                    dslv55
570200060516     c                   if        d55err = *blanks and d55tfa <> d55lin
570300060516     c                   move      d55tfa        kdstfgs
570400120607     c     kdst          chain     fidst01l
570500060516      * se non trovo neanche con il p.o. che gestisce metto il p.o. ritiro
570600060516      * teoricamente non dovrebbe succedere
570700120607     c                   if        not %found(fidst01l)
570800060516     c                   eval      kdstfgs = ormpor
570900060516     c                   endif
571000060516     c                   endif
571100060516     c                   endif
571200060516     c**!!!              Eval      OrfFgs = OrmPor
571300060516     c                   eval      orffgs = kdstfgs
571400040909     c                   Eval      OrfNdc = OrmNdc
571500040909     c                   Eval      OrfDdc = OrmDdc
571600040909     c                   EndIf
571700010309
571800010309     c                   ENDSR
571900051207     C*
572000051207     C* Scrivo lo storico orm ------------------------------------------*
572100051207     C     SR_Storico    BEGSR
572200051207     C                   clear                   fnorv000
572300051207     C                   eval      ORVpoe = ORMpoe
572400051207     C                   eval      ORVnsr = ORMnsr
572500051207     C                   eval      ORVnor = ORMnor
572600051207     C                   eval      ORVnrv = ORMnrv
572700051207     C                   eval      ORVtor = ORMtor
572800051207     C                   eval      ORVcor = ORMcor
572900051207     C                   eval      ORVrso = ORMrso
573000051207     C                   eval      ORVino = ORMino
573100051207     C                   eval      ORVcao = ORMcao
573200051207     C                   eval      ORVloo = ORMloo
573300051207     C                   eval      ORVpro = ORMpro
573400051207     C                   eval      ORVnao = ORMnao
573500051207     C                   eval      ORVcra = ORMcra
573600051207     C                   eval      ORVrsr = ORMrsr
573700051207     C                   eval      ORVinr = ORMinr
573800051207     C                   eval      ORVcar = ORMcar
573900051207     C                   eval      ORVlor = ORMlor
574000051207     C                   eval      ORVprr = ORMprr
574100051207     C                   eval      ORVnar = ORMnar
574200051207     C                   eval      ORVrer = ORMrer
574300051207     C                   eval      ORVter = ORMter
574400051207     C                   eval      ORVcrc = ORMcrc
574500051207     C                   eval      ORVrsc = ORMrsc
574600051207     C                   eval      ORVinc = ORMinc
574700051207     C                   eval      ORVloc = ORMloc
574800051207     C                   eval      ORVcac = ORMcac
574900051207     C                   eval      ORVprc = ORMprc
575000051207     C                   eval      ORVnac = ORMnac
575100051207     C                   eval      ORVffd = ORMffd
575200051207     C                   eval      ORVdar = ORMdar
575300051207     C                   eval      ORVorr = ORMorr
575400051207     C                   eval      ORVnam = ORMnam
575500051207     C                   eval      ORVncl = ORMncl
575600051207     C                   eval      ORVpkg = ORMpkg
575700051207     C                   eval      ORVvlm = ORMvlm
575800051207     C                   eval      ORVbnc = ORMbnc
575900051207     C                   eval      ORVblc = ORMblc
576000051207     C                   eval      ORVatt = ORMatt
576100051207     C                   eval      ORVmtc = ORMmtc
576200051207     C                   eval      ORVpag = ORMpag
576300051207     C                   eval      ORVksc = ORMksc
576400051207     C                   eval      ORVctr = ORMctr
576500051207     C                   eval      ORVddt = ORMddt
576600051207     C                   eval      ORVpor = ORMpor
576700051207     C                   eval      ORVzor = ORMzor
576800051207     C                   eval      ORVpoc = ORMpoc
576900051207     C                   eval      ORVno1 = ORMno1
577000051207     C                   eval      ORVno2 = ORMno2
577100051207     C                   eval      ORVcst = ORMcst
577200051207     C                   eval      ORVvcs = ORMvcs
577300051207     C                   eval      ORVfcs = ORMfcs
577400051207     C                   eval      ORVdfo = ORMdfo
577500051207     C                   eval      ORVofo = ORMofo
577600051207     C                   eval      ORVfao = ORMfao
577700051207     C                   eval      ORVsto = ORMsto
577800051207     C                   eval      ORVdst = ORMdst
577900051207     C                   eval      ORVnpg = ORMnpg
578000051207     C                   eval      ORVndc = ORMndc
578100051207     C                   eval      ORVddc = ORMddc
578200051207     C                   eval      ORVstp = ORMstp
578300051207     C                   eval      ORVrfa = ORMrfa
578400051207     C                   eval      ORVsto = ORMsto
578500070711     c                   eval      orveti = ormeti
578600051207     C                   eval      ORVspi = ORMspi
578700051207     C                   eval      ORVflo = ORMflo
578800051207     C                   movel     knmus         ORVutv
578900051207     C                   TIME                    W0140            14 0
579000051207     C                   MOVEL     W0140         orvorv
579100051207     C                   CLEAR                   WLBDAT
579200051207     C                   MOVE      W0140         G02DAT
579300051207     C                   CALL      'XSRDA8'
579400051207     C                   PARM                    WLBDAT
579500051207     C                   Z-ADD     G02INV        orvdtv
579600051207     C                   Z-ADD     G02INV        orvdtt
579700051207     C                   write     fnorv000
579800051207
579900051207     c                   ENDSR
580000950109     C*---------------------------------------------------------------*
580100950109     C*    OPERAZIONI INIZIALI                                        *
580200950109     C*---------------------------------------------------------------*
580300950109     C     *INZSR        BEGSR
580400950109     C*
580500950109     C     *ENTRY        PLIST
580600950109     C                   PARM                    KPJBA
580700130524     C                   MOVEL     KPJBU         FNLSU2DS
580800021014
580900021014     c                   Movel     Simfel        Varx1
581000021014     c                   Movel     Simfel        Var2x1
581100021014     c                   Movel     Simfel        Num31
581200021015     c                   Move      Simfel        Mbrfil4
581300021014
581400000111     C* SPOSTO SUBITO NUMERI FOGLIO IN CAMPI LUNGHI 6,0
581500000111     C                   Z-ADD     FGV2          FGV260
581600000111     C                   Z-ADD     NFV1          NFV160
581700950109     C*
581800950109     C                   Z-ADD     1             CODUT             1 0
581900170613     c     *dtaara       define    §azute        azuteds
582000170613     c     *dtaara       define    §datiute      ddatiute
582100170613      *
582200170613     c                   in(E)     *dtaara
582300170613     c                   IF        %ERROR or RSUT = *blanks
582400170613     c                   clear                   Tibs34Ds
582500170613     c                   call      'TIBS34R'
582600170613     c                   parm                    Tibs34Ds
582700170613     c                   in        *dtaara
582800170613     c                   ENDIF
582900170613      *
583000000428     C** S.I. IN CUI SONO
583100021010     C                   CLEAR                   DSBS55
583200000428     C                   MOVEL     KNSIF         I50PSI
583300000428     C                   MOVEL     'L'           I50TLA
583400021010     C                   CALL      'TIBS55R'
583500021010     C                   PARM                    DSBS55
583600950109     C**
583700950109     C*    CHIAVI                                                     *
583800950109     C**
583900950109     C     KBLP2         KLIST
584000950109     C                   KFLD                    BLPAAS
584100950109     C                   KFLD                    BLPLNP
584200950109     C                   KFLD                    BLPNRS
584300950109     C                   KFLD                    BLPNSP
584400060220     C     KAR4          KLIST
584500010309     C                   KFLD                    BLPAAS
584600010309     C                   KFLD                    BLPLNP
584700010309     C                   KFLD                    BLPNRS
584800010309     C                   KFLD                    BLPNSP
584900010309     C                   KFLD                    WTRC4
585000950516     C     KBTP2         KLIST
585100020606     c                   kfld                    btpflp
585200950516     C                   KFLD                    BTPAAS
585300950516     C                   KFLD                    BTPLNP
585400950516     C                   KFLD                    BTPNRS
585500950516     C                   KFLD                    BTPNSP
585600950109     C     KBLT3         KLIST
585700950109     C                   KFLD                    BLTAAS
585800950109     C                   KFLD                    BLTLNP
585900950109     C                   KFLD                    BLTNRS
586000950109     C                   KFLD                    BLTNSP
586100950109     C     KBTT3         KLIST
586200020605     c                   kfld                    fgvlnp
586300950109     C                   KFLD                    BTTAAS
586400950109     C                   KFLD                    BTTLNP
586500950109     C                   KFLD                    BTTNRS
586600950109     C                   KFLD                    BTTNSP
586700950109     C     KTAB          KLIST
586800950109     C                   KFLD                    CODUT
586900950109     C                   KFLD                    COD               2
587000950109     C                   KFLD                    KEY               8
587100950109     C     KTAB2         KLIST
587200950109     C                   KFLD                    CODUT
587300950109     C                   KFLD                    COD
587400950109     C     KBLT2         KLIST
587500950109     C                   KFLD                    WNPG
587600950109     C                   KFLD                    BLTFLS
587700950109     C                   KFLD                    BLTLNA
587800950109     C                   KFLD                    BLTNRS
587900950113     C                   KFLD                    BLTNSC
588000971107     C                   KFLD                    FGVLNP
588100130226     C     KBLT_xco      KLIST
588200130226     C                   KFLD                    WNPG
588300130226     C                   KFLD                    BLTFLS
588400130226     C                   KFLD                    BLTLNA
588500130226     C                   KFLD                    BLTNRS
588600130226     C                   KFLD                    BLTNSC
588700950109     C     KBTT          KLIST
588800950109     C                   KFLD                    WNPG
588900950109     C                   KFLD                    BTTFLS
589000950109     C                   KFLD                    BTTLNA
589100950109     C                   KFLD                    BTTNRS
589200950113     C                   KFLD                    BTTNSC
589300950915     C     KFVV          KLIST
589400950915     C                   KFLD                    WNPG5
589500950915     C                   KFLD                    WDFV
589600020916     C                   KFLD                    Kfgs
589700000728     C     KFVV1         KLIST
589800000728     C                   KFLD                    BRVNPG
589900001123     C                   KFLD                    WNFV05
590000000728     C                   KFLD                    BRVFGS
590100950109     C     KBRC2         KLIST
590200950109     C                   KFLD                    WNPG
590300000111     C                   KFLD                    FGV260
590400971107     C                   KFLD                    FGVLNP
590500950109     C                   KFLD                    BRVLNA
590600130527     C     K03BLP77      klist
590700020807     C                   kfld                    SIMfel
590800960212     C                   KFLD                    FGV2
590900960212     C                   KFLD                    BRVLNA
591000130531     C     Kblp77        KLIST
591100130531     C                   KFLD                    simfel
591200130531     C                   KFLD                    kfgv
591300130531     C                   KFLD                    savffv
591400130527     C     K02BLP77_9    KLIST
591500020807     C                   KFLD                    SIMfel
591600020807     C                   KFLD                    COMnfv
591700950109     C     KFVD2         KLIST
591800950109     C                   KFLD                    FGVLNP
591900950109     C                   KFLD                    FGV2
592000950109     C                   KFLD                    BRVLNA
592100950110     C                   KFLD                    WTRC
592200130530     C     KFVDbor       KLIST
592300130530     C                   KFLD                    SIMFEL
592400130530     C                   KFLD                    WNFV5
592500130530     C                   KFLD                    BLPLNA
592600130530     C                   KFLD                    WTRC
592700060707     C     KFV4e         KLIST
592800060707     C                   KFLD                    FV4LNP
592900060707     C                   KFLD                    FV4NFV
593000060707     C                   KFLD                    FV4EPA
593100060707     C                   KFLD                    FV4LAI
593200060707     C                   KFLD                    FV4TRC
593300011127     C*
593400011127     C     KTBE          KLIST
593500011127     C                   KFLD                    KCOD1
593600011127     C                   KFLD                    KKE1
593700130226     C     KTBE2         KLIST
593800130226     C                   KFLD                    KCOD1
593900130226     C                   KFLD                    KKE1
594000130226     C                   KFLD                    KKE2
594100130226     c
594200011127     C     *LIKE         DEFINE    TBECOD        KCOD1
594300011127     C     *LIKE         DEFINE    TBEKE1        KKE1
594400130226     C     *LIKE         DEFINE    TBEKE2        KKE2
594500130226     c
594600950109     C     KFTT          KLIST
594700950109     C                   KFLD                    WPDR
594800950109     C                   KFLD                    COMTSR
594900950109     C                   KFLD                    WNDC
595000950113     C                   KFLD                    WDRT
595100950109     C     KFTD          KLIST
595200950109     C                   KFLD                    COMPDR
595300950109     C                   KFLD                    COMTSR
595400950109     C                   KFLD                    COMNDC
595500950114     C                   KFLD                    COMDDC
595600950109     C                   KFLD                    CO2KSC
595700950113     C                   KFLD                    COMRSC
595800020416     C     KFTD1         KLIST
595900020416     C                   KFLD                    COMPDR
596000020416     C                   KFLD                    COMTSR
596100020416     C                   KFLD                    COMNDC
596200020416     C                   KFLD                    COMDDC
596300950109     C     KBLT          KLIST
596400021021     C                   KFLD                    wlnp              3 0
596500021021     C                   KFLD                    DFV               8 0
596600021021     C                   KFLD                    COMFIL            3 0
596700000731     C     KBLT1         KLIST
596800000731     C                   KFLD                    BLTAAS
596900000731     C                   KFLD                    BLTLNP
597000000731     C                   KFLD                    BLTNRS
597100000731     C                   KFLD                    BLTNSP
597200000731     C                   KFLD                    BLTNSC
597300000714     C     KBLTL         KLIST
597400000714     C                   KFLD                    DFV
597500000714     C                   KFLD                    COMFIL
597600000714     C                   KFLD                    BLTLNP
597700960704     C     KBLT27        KLIST
597800960704     C                   KFLD                    ANMFLS
597900960704     C                   KFLD                    ANMLNA
598000960704     C                   KFLD                    ANMNRS
598100960704     C                   KFLD                    ANMSCN
598200950109     C     KANM          KLIST
598300021021     C                   KFLD                    wlnp
598400021021     C                   KFLD                    COMCAA
598500950109     C                   KFLD                    COMFIL
598600040422     C     KANM02        KLIST
598700040422     C                   KFLD                    bttFLS
598800040422     C                   KFLD                    bttLNA
598900040422     C                   KFLD                    bttNRS
599000040422     C                   KFLD                    bttnsc
599100040422     C                   KFLD                    COMCAA            3 0
599200950418     C     KANM3         KLIST
599300950418     C                   KFLD                    BRVLNP
599400950418     C                   KFLD                    BRVLNA
599500950418     C                   KFLD                    BRVNRS
599600950418     C                   KFLD                    BRVNSC
599700950418     C                   KFLD                    COMCAA            3 0
599800020412     C     KANM31        KLIST
599900020412     C                   KFLD                    ANMFLS
600000020412     C                   KFLD                    ANMLNA
600100020412     C                   KFLD                    ANMNRS
600200020412     C                   KFLD                    ANMSCN
600300020412     C                   KFLD                    COMCAA            3 0
600400130527     C     KANM1         KLIST
600500130527     C                   KFLD                    BLTFLS
600600130527     C                   KFLD                    BLTLNA
600700130527     C                   KFLD                    BLTNRS
600800130527     C                   KFLD                    BLTNSC
600900130607     C                   KFLD                    comcaa
601000151118     C     KANM01        KLIST
601100151118     C                   KFLD                    BLpaas
601200151118     C                   KFLD                    BLplnp
601300151118     C                   KFLD                    BLpNRS
601400151118     C                   KFLD                    BLpNSp
601500151118     C                   KFLD                    comcaa
601600950109     C     KANM5         KLIST
601700950109     C                   KFLD                    ANMFLS
601800950109     C                   KFLD                    ANMLNA
601900950109     C                   KFLD                    ANMNRS
602000950109     C                   KFLD                    ANMSCN
602100971107     C                   KFLD                    FGVLNP
602200950109     C     KBRV          KLIST
602300950109     C                   KFLD                    WNPG
602400000111     C                   KFLD                    FGV260
602500971107     C                   KFLD                    FGVLNP
602600130527     C     KBRV1         KLIST
602700130527     C                   KFLD                    KNPG
602800130527     C                   KFLD                    BLTFLS
602900130527     C                   KFLD                    BLTLNA
603000130527     C                   KFLD                    BLTNRS
603100130527     C                   KFLD                    BLTNSC
603200130527     C                   KFLD                    anmfle
603300130530     C     KBRVbor       KLIST
603400130530     C                   KFLD                    KNPG
603500130530     C                   KFLD                    WNFV60
603600130530     C                   KFLD                    KFGS
603700130530     C                   KFLD                    BLPFLS
603800130530     C                   KFLD                    blplna
603900130530     C                   KFLD                    BLPNRS
604000130530     C                   KFLD                    BLTNSC
604100950418     C     KBRV2         KLIST
604200950418     C                   KFLD                    WNPG
604300950418     C                   KFLD                    WSANFV
604400950418     C                   KFLD                    WSAFGS
604500950418     C                   KFLD                    KLNA
604600950418     C                   KFLD                    KLNP
604700950418     C                   KFLD                    KNRS
604800950418     C                   KFLD                    KNSC
604900980209     C     KBRV01        KLIST
605000980209     C                   KFLD                    NPG1
605100000111     C                   KFLD                    NFV160
605200980209     C                   KFLD                    FGS1
605300980209     C                   KFLD                    KLNP
605400980209     C                   KFLD                    KLNA
605500980209     C                   KFLD                    KNRS
605600980209     C                   KFLD                    KNSC
605700960830     C     KBRV03        KLIST
605800960830     C                   KFLD                    WNPG
605900960830     C                   KFLD                    BRVLNP
606000960830     C                   KFLD                    BRVLNA
606100960830     C                   KFLD                    BRVNRS
606200960830     C                   KFLD                    BRVNSC
606300950109     C     KBRV3         KLIST
606400950109     C                   KFLD                    NPG1
606500000111     C                   KFLD                    NFV160
606600950109     C                   KFLD                    FGS1
606700950109     C                   KFLD                    COMFIL
606800950915     C     KBRV33        KLIST
606900970423     C                   KFLD                    KVVNPG
607000970423     C                   KFLD                    KVVNFV
607100970423     C                   KFLD                    KVVFGS
607200950915     C                   KFLD                    COMFIL
607300990917     C     KBRV32        KLIST
607400990917     C                   KFLD                    KVVNPG
607500990917     C                   KFLD                    KVVNFV
607600990917     C                   KFLD                    KVVFGS
607700950109     C     KBRV4         KLIST
607800950109     C                   KFLD                    KLNP
607900950109     C                   KFLD                    KLNA
608000950109     C                   KFLD                    KNRS
608100950109     C                   KFLD                    KNSC
608200950118     C     KBRV5         KLIST
608300950109     C                   KFLD                    WNPG
608400950109     C                   KFLD                    KLNP
608500950109     C                   KFLD                    KLNA
608600950109     C                   KFLD                    KNRS
608700950109     C                   KFLD                    KNSC
608800971107     C                   KFLD                    FGVLNP
608900000731     C     KBRV55        KLIST
609000000731     C                   KFLD                    WNPG
609100000731     C                   KFLD                    KLNP
609200000731     C                   KFLD                    KLNA
609300000731     C                   KFLD                    KNRS
609400000731     C                   KFLD                    KNSC
609500040802     C                   KFLD                    WFGSspu
609600950118     C     KBRV6         KLIST
609700950118     C                   KFLD                    KLNP
609800950118     C                   KFLD                    KLNA
609900950118     C                   KFLD                    KNRS
610000950118     C                   KFLD                    KNSC
610100971107     C                   KFLD                    FGVLNP
610200150603     C     KBRV7         KLIST
610300150603     C                   KFLD                    BLTFLS
610400150603     C                   KFLD                    BLTLNA
610500150603     C                   KFLD                    BLTNRS
610600150603     C                   KFLD                    BLTNSC
610700150924     C*****              KFLD                    FGVLNP
610800020807     c     K02FGV01      klist
610900020807     c                   kfld                    fgv2
611000020807     c                   kfld                    SIMfel
611100010309     c*
611200010309     c     korm          klist
611300010309     c                   kfld                    §b4poe
611400010309     c                   kfld                    §b4nsr
611500010309     c                   kfld                    §b4nor
611600010309     c                   kfld                    §b4nrv
611700010611     c     kbrve         klist
611800010611     c                   kfld                    brvnpg
611900010611     c                   kfld                    brvnfv
612000010611     c                   kfld                    brvfgs
612100010611     c                   kfld                    brvlnp
612200010611     c                   kfld                    brvlna
612300010611     c                   kfld                    brvnrs
612400010611     c                   kfld                    brvnsc
612500020605     C     KBtt38        KLIST
612600020605     c                   kfld                    fgvlnp
612700020605     C                   KFLD                    DFV               8 0
612800020605     C                   KFLD                    COMFIL            3 0
612900020606     C     KBtt37        KLIST
613000020606     c                   kfld                    fgvlnp
613100020606     C                   KFLD                    KLNP
613200020606     C                   KFLD                    KLNA
613300020606     C                   KFLD                    KNRS
613400020606     C                   KFLD                    KNSC
613500040319     C     KBTT1         KLIST
613600040319     c                   kfld                    BTTFLP
613700040319     C                   KFLD                    BTTAAS
613800040319     C                   KFLD                    BTTLNP
613900040319     C                   KFLD                    BTTNRS
614000040319     C                   KFLD                    BTTNSP
614100040319     C                   KFLD                    BTTNSC
614200040310     c     Kar5          klist
614300040311     c                   kfld                    bltaas
614400040311     C                   KFLD                    bltlnp
614500040311     C                   KFLD                    bltnrs
614600040311     C                   KFLD                    bltnsp
614700040310     C                   KFLD                    ktrd
614800040310     c                   movel     'GEN'         ktrd
614900060516
615000060516     c     kdst          klist
615100120607     c                   kfld                    kdstnpg
615200120607     c                   kfld                    kdstnfv
615300060516     c                   kfld                    kdstfgs
615400060516
615500950118     C***
615600950118     C*  CODICI TRATTAMENTO MERCE CON §1BFG5 = "N"
615700950118     C***
615800960202     C* NON CARICO SE RICHIAMATO DA MANUTENZIONE
615900960202     C     PA2FLG        IFNE      'M'
616000960202     C* APERTURA FILE
616100960202     C                   EXSR      OPENF
616200130530     c
616300960202     C**
616400950118     C                   Z-ADD     1             X
616500950118     C                   MOVEL     '1B'          COD
616600950118     C     KTAB2         CHAIN     TABEL00F                           31
616700950118     C*
616800950118     C     *IN31         DOWEQ     *OFF
616900950118     C                   MOVEL     TBLUNI        DS1B
617000950118     C     §1BFG5        IFEQ      'N'
617100950118     C                   MOVEL     TBLKEY        CTM(X)
617200950118     C                   ADD       1             X
617300950118     C                   ENDIF
617400950118     C     KTAB2         READE     TABEL00F                               31
617500950118     C                   ENDDO
617600050524     c
617700050524     c* Controllo riempimento schiera
617800050524     c                   clear                   trul0sds
617900050524     c                   eval      i0sski='CTM'
618000050524     c                   eval      i0sele=%elem(ctm)
618100050524     c                   eval      i0spie=x
618200050524     c                   eval      i0scod='  1B '
618300050524     c                   eval      i0sfile='TABEL00F  '
618400050524     c                   eval      i0ssif=knsif
618500130718     c                   eval      i0spgm='FNLSU2R'
618600050524     c                   movel     trul0sds      kpjbu
618700050524     c                   call      'TRUL0SR'
618800050524     c                   parm                    kpjba
618900960531     C***
619000960531     C** TABELLA 5F - FILIALI CONFERMA BOLLE DA DISK
619100960531     C***
619200960531     C                   Z-ADD     0             X
619300960531     C                   MOVEL     '5F'          COD
619400960531     C     KTAB2         SETLL     TABEL
619500960531     C     KTAB2         READE     TABEL                                  31
619600960531     C*
619700960531    1C     *IN31         DOWEQ     '0'
619800960531    2C     TBLFLG        IFEQ      ' '
619900050524     c* elaboro soltanto i record con le prima 3 cifre <>'   '
620000050524     c*  perchè nella 5F ci sono 2 diversi tipi record: quello con i '   '
620100050524     c*   davanti qui non sono usati
620200050524     c                   movel     tblkey        w003a             3
620300050524     c                   if        w003a<>'   '
620400960531     C                   ADD       1             X
620500960531     C                   MOVEL     TBLKEY        S5F(X)
620600960531     C     X             OCCUR     DSE5F
620700960701     C                   MOVEA     TBLUNI        E5F
620800080109      * - Carica comunque il n° max di elementi previsti nella schiera.
620900080109      *   Se superato il limite massimo di riempimento, verrà inviato
621000080109      *   un messaggio di avvertimento - vedi chiamata al pgm TRUL0SR:
621100080109      *   verificherà lui la percentuale di riempimento...).
621200080109     C     X             IFGE      $MaxElem
621300960531     C                   SETON                                        31
621400960531     C                   ENDIF
621500050524     C                   ENDIF
621600960531    2C                   ENDIF
621700960531     C*
621800960531     C  N31KTAB2         READE     TABEL                                  31
621900960531    1C                   ENDDO
622000050524     c**
622100050524     c                   clear                   trul0sds
622200050524     c                   eval      i0sski='S5F'
622300050524     c                   eval      i0sele=%elem(s5f)
622400050524     c                   eval      i0spie=x
622500050524     c                   eval      i0scod='  5F '
622600050524     c                   eval      i0sfile='TABEL00F  '
622700050524     c                   eval      i0ssif=knsif
622800130718     c                   eval      i0spgm='FNLSU2R'
622900050524     c                   movel     trul0sds      kpjbu
623000050524     c                   call      'TRUL0SR'
623100050524     c                   parm                    kpjba
623200950118     C***
623300950118     C*  CODICI PISTOLA CON PISTOLA AUTOMATICA
623400950118     C***
623500950118     C                   Z-ADD     1             X
623600970818     C                   Z-ADD     1             Y                 3 0
623700950118     C                   MOVEL     '7J'          COD
623800950118     C     KTAB2         CHAIN     TABEL00F                           31
623900950118     C*
624000950118     C     *IN31         DOWEQ     *OFF
624100950118     C                   MOVEL     TBLUNI        DS7J
624200950118     C     §7JRPS        IFEQ      'A'
624300950118     C                   MOVEL     TBLKEY        NPS(X)
624400950118     C                   ADD       1             X
624500950118     C                   ENDIF
624600970818     C     §7JFSL        IFEQ      'F'
624700970818     C                   MOVEL     TBLKEY        NPF(Y)
624800970818     C                   ADD       1             Y
624900970818     C                   ENDIF
625000950118     C     KTAB2         READE     TABEL00F                               31
625100950118     C                   ENDDO
625200011127     C*
625300011127     C*  tabelle GEDCN x reperimento divisa conteggi padroncini/tariffe
625400011127     C                   movel     'GED'         kcod1
625500011127     C                   movel     *blanks       kke1
625600011127     C                   movel     'CN'          kke1
625700011127     C     ktbe          chain     tntbe01l                           31
625800011127     C                   clear                   dgedcn
625900011127     C  N31              movel     tbeuni        dgedcn
626000130227     c
626100121119     C*  Tabella DGD    x reperimento data dal/al per conferma autoamti
626200041216     c*                 prepagati
626300041217     c                   Clear                   Tibs02Ds
626400121119     c                   Clear                   ddgd
626500041217     c                   Eval      T02Mod = 'C'
626600041217     c                   Eval      T02Sif = Knsif
626700121119     c                   Eval      T02Cod = 'DGD'
626800121119     c                   Movel(p)  '1'           T02Ke1
626900041217     c                   Call      'TIBS02R'
627000041217     c                   Parm                    Kpjba
627100041217     c                   Parm                    Tibs02Ds
627200041217     c                   If        T02Err = *Blanks
627300121119     c                   Movel     T02Uni        ddgd
627400041217     c                   EndIf
627500000619     C*
627600060710     C*  tipi record di FV4 da trasmettere in sede
627700060710     C***
627800060710     C                   Z-ADD     1             X
627900060710     C                   MOVEL     '3B'          COD
628000060710     C     KTAB2         CHAIN     TABEL00F                           31
628100060710     C*
628200060710     C     *IN31         DOWEQ     *OFF
628300060710     c                   if        tblflg=' '
628400060710     C                   MOVEL     TBLUNI        DS3B
628500060710     C     §3btrs        IFEQ      'S'
628600060710     C                   MOVEL     TBLKEY        fv4S(X)
628700060710     C                   ADD       1             X
628800060710     C                   ENDIF
628900060710     C                   ENDIF
629000060710     C     KTAB2         READE     TABEL00F                               31
629100060710     C                   ENDDO
629200960202     C**
629300960202     C                   ENDIF
629400950109     C*
629500950109     C* SPENGO GLI INDICATORI USATI PER LA RICERCA CAPOCONTI
629600950109     C                   SETOFF                                       212223
629700950109     C                   SETOFF                                       242526
629800950515     C                   SETOFF                                       273132
629900950109     C*
630000950109     C* DEFINIZIONE CAMPI
630100970423     C     *LIKE         DEFINE    BRVNPS        WNPS
630200970528     C     *LIKE         DEFINE    BRVNPS        SPUNPS
630300170613     C     *LIKE         DEFINE    BRVFGS        SPUFGS
630400970423     C     *LIKE         DEFINE    FVVNPG        KVVNPG
630500000111     C     *LIKE         DEFINE    BRVNFV        KVVNFV
630600001121     C     *LIKE         DEFINE    BRVNFV        SAVNFV
630700001123     C     *LIKE         DEFINE    fvvnfv        wnfv05
630800970423     C     *LIKE         DEFINE    FVVFGS        KVVFGS
630900970423     C**
631000970423     C     *LIKE         DEFINE    FVDCLA        NCLU
631100950109     C     *LIKE         DEFINE    FVDKFA        PKGU
631200950113     C     *LIKE         DEFINE    FVDVL1        VOLU
631300950113     C     *LIKE         DEFINE    FVDSNA        WSPEN
631400130530     C     *LIKE         DEFINE    FVDSNA        WSPE
631500950113     C     *LIKE         DEFINE    FVDSNA        WSPET
631600950110     C     *LIKE         DEFINE    FVDTRC        WTRC
631700950109     C*
631800950109     C     *LIKE         DEFINE    FTTPDR        COMPDR
631900950109     C     *LIKE         DEFINE    FTTPDR        WPDR
632000950109     C     *LIKE         DEFINE    FTTTSR        COMTSR
632100950109     C     *LIKE         DEFINE    FTTNDC        COMNDC
632200950109     C     *LIKE         DEFINE    FTTNDC        WNDC
632300950109     C     *LIKE         DEFINE    FTTSET        COMSET
632400020326     C     *LIKE         DEFINE    FTDstp        wstp
632500950109     C     *LIKE         DEFINE    FTDKSC        CO2KSC
632600950113     C     *LIKE         DEFINE    FTDRSC        COMRSC
632700021111     C     *LIKE         DEFINE    FTDVLU        WFVDVOL
632800021111     C     *LIKE         DEFINE    FTDVLU        WVOL
632900021108     C     *LIKE         DEFINE    FTDpkl        Wpes
633000021111     C     *LIKE         DEFINE    FTDpkl        WFVDPES
633100950114     C     *LIKE         DEFINE    FTDDDC        COMDDC
633200950109     C*
633300960124     C     *LIKE         DEFINE    FGVLNP        SA2LNP
633400960124     C     *LIKE         DEFINE    FGVNFV        SA2NFV
633500000731     C     *LIKE         DEFINE    FGVLNP        WFGSFV
633600960124     C     *LIKE         DEFINE    FVARTP        SA2RTP
633700960124     C     *LIKE         DEFINE    FGVDFV        WDTP
633800950915     C     *LIKE         DEFINE    FGVDFV        WDFV
633900950113     C     *LIKE         DEFINE    BRVNPG        WNPG
634000950915     C     *LIKE         DEFINE    BRVNPG        WNPG5
634100950109     C     *LIKE         DEFINE    BRVZNC        WRVZNC
634200950109     C     *LIKE         DEFINE    BRVVUC        WRVVUC
634300990906     C     *LIKE         DEFINE    BRVPUC        WRVPUC
634400950109     C     *LIKE         DEFINE    BRVCAN        WRVCAN
634500950915     C     *LIKE         DEFINE    BRVNPS        WRVNPS
634600950109     C     *LIKE         DEFINE    BRVLNP        KLNP
634700950109     C     *LIKE         DEFINE    BRVLNA        KLNA
634800950109     C     *LIKE         DEFINE    BRVNRS        KNRS
634900950109     C     *LIKE         DEFINE    BRVNSC        KNSC
635000950418     C     *LIKE         DEFINE    BRVFGS        WSAFGS
635100950418     C     *LIKE         DEFINE    BRVNFV        WSANFV
635200000111     C     *LIKE         DEFINE    BRVNFV        FGV260
635300000111     C     *LIKE         DEFINE    BRVNFV        NFV160
635400960604     C     *LIKE         DEFINE    BRVLNP        WLNP
635500960905     C     *LIKE         DEFINE    BRVNPG        NPGP
635600960905     C     *LIKE         DEFINE    BRVNFV        NFVP
635700960905     C     *LIKE         DEFINE    BRVLNP        LNPP
635800960905     C     *LIKE         DEFINE    BRVLNA        LNAP
635900960905     C     *LIKE         DEFINE    BRVNRS        NRSP
636000960905     C     *LIKE         DEFINE    BRVNSC        NSCP
636100960905     C     *LIKE         DEFINE    BRVNPS        NPSP
636200960905     C     *LIKE         DEFINE    BRVVUC        VUCP
636300990623     C     *LIKE         DEFINE    BRVPUC        PUCP
636400950109     C     *LIKE         DEFINE    BLPCBO        SAVCBO
636500950623     C*
636600980210     C     *LIKE         DEFINE    D55TFA        LNATFA
636700971107     C     *LIKE         DEFINE    D55TFP        LNAFEL
636800950707     C     *LIKE         DEFINE    BTTDUT        WDTPAR
636900970703     C     *LIKE         DEFINE    D55LIN        WLIN
637000060220     C     *LIKE         DEFINE    AR4TRC        WTRC4
637100980126     C*
637200980126     C     *LIKE         DEFINE    BRVLNA        WLNA
637300980209     C     *LIKE         DEFINE    DSBRV         SVBRVE
637400070123     C     *LIKE         DEFINE    DSBRV         dsbrvs
637500990623     C                   Z-ADD     0             COMDFS            8 0
637600990623     C                   Z-ADD     0             COMHMS            4 0
637700950621     C*
637800150609      * verifico se presenti simfel o suoi secondi livello
637900150609      * nel file della sospensione automatica per colli di clienti
638000150609      * che partono da più filiali
638100150609     c                   eval      w_FilBsp=*blanks
638200151215     c                   eval      w_FilBsps=*blanks
638300151215     c                   clear                   w_SiBsp
638400151215     c                   clear                   w_bsptip
638500150610     c     'S'           setll     fibsp03l
638600150610     c     'S'           reade     fibsp03l
638700151215     c                   dow       not %eof(fibsp03l) and w_filbsps=*blanks
638800150609     c                   z-add     1             Ix
638900150609     c                   dow       Ix<=%elem(sbsp) and sbsp(Ix)>0
639000151215     c                             and w_FilBsps=*blanks
639100150609     c                   if        %lookup(sbsp(Ix):L1)>0
639200150609     c                   eval      w_FilBsp='S'
639300151215     c                   if        bspaut=*blanks
639400151215     c                   eval      w_FilBsps='S'
639500151215     c                   endif
639600150609     c                   endif
639700150609     c                   add       1             Ix
639800150609     c                   enddo
639900150610     c     'S'           reade     fibsp03l
640000150610     c                   enddo
640100150703
640200150703     c                   Clear                   tibs02ds
640300150703     c                   Clear                   dmradan
640400150703     c                   Movel     'C'           T02Mod
640500150703     c                   Movel     KNSIF         T02Sif
640600150703     c                   Movel     'MRA'         T02Cod
640700150703     c                   Movel     'FNLSU3R'     T02Ke1
640800150703     c                   Call      'TIBS02R'
640900150703     c                   Parm                    Kpjba
641000150703     c                   Parm                    tibs02ds
641100150703    2c                   If        T02Err = *Blanks
641200150703     c                   Movel     T02Uni        Dmradan
641300150703    2c                   endif
641400150703     C***
641500150703     C* CARICO DATI VARIABILI PARTENZE
641600150703     C***
641700150703     C                   MOVEL     '3I'          COD
641800150703     C                   MOVEL     '1       '    KEY
641900150703     C     KTAB          CHAIN     TABEL00F
642000150703     c                   if        %found(tabel00f)
642100150703     C                   MOVEL     TBLUNI        DS3I
642200150703     c                   else
642300150703     C                   CLEAR                   DS3I
642400150703     c                   endif
642500150609
642600021016      * Recupero le date delle statistiche
642700021016     c                   Clear                   Tibs02Ds
642800021016     c                   Clear                   dSdf
642900021016     c                   Eval      T02Mod = 'C'
643000021016     c                   Eval      T02Sif = Knsif
643100021016     c                   Eval      T02Cod = 'SDF'
643200021016     c                   Movel(p)  Simfel        T02Ke1
643300021016     c                   Call      'TIBS02R'
643400021016     c                   Parm                    Kpjba
643500021016     c                   Parm                    Tibs02Ds
643600021016     c                   If        T02Err = *Blanks
643700021016     c                   Movel     T02Uni        dSdf
643800021016     c                   EndIf
643900950109     C*
644000950109     C                   SETON                                        OFOE
644100950109     C* GIRO UDATE
644200950113     C                   TIME                    W0140            14 0
644300050707     C***                MOVEL     W0140         WTIME             6 0
644400950109     C                   MOVE      W0140         WDTGIO            8 0
644500950109     C                   Z-ADD     WDTGIO        G02DAT
644600950109     C                   MOVEL     *BLANK        G02ERR
644700950109     C                   CALL      'XSRDA8'
644800950109     C                   PARM                    WLBDAT
644900950109     C                   MOVEL     G02INV        DATEU             8 0
645000950109     C*
645100950109     C                   MOVEL     'R'           COMTSR
645200950109     C                   Z-ADD     0             DFV
645300950113     C                   CLEAR                   SAVSPE
645400970124     C* CAMPO CHE PASSO VUOTO AL PGM FNLSA2R3
645500970124     C                   CLEAR                   WWLNA
645600030107
645700950109     C                   ENDSR
645800070827     C**********************************************************************
645900070827     C*  P A D R O N C I N I
646000070827     C**********************************************************************
646100070827     C* CARICO I CODICI PADRONCINI PER I QUALI E' NECESSARIO CREARE I
646200070827     C*   FILE fiftt00F E fiftd00F
646300070827     C* CARICO ANCHE QUELLI CON MERCE PORTATA A MAGAZZINO
646400070827     C* CARICO ANCHE I PADRONCINI DIPENDENTI
646500070827     c     CarPAD        BEGSR
646600070827     c                   movel     fgvdfv        apddfv
646700070827     c
646800070827     C                   Z-ADD     1             X
646900070827     C                   Z-ADD     1             Y
647000070827     C                   CALL      'FNLSA2R2'
647100070827     C                   PARM                    APDPDR            7 0
647200070827     C                   PARM                    APDETM            1
647300070827     C                   PARM                    APDETP            1
647400070827     C                   PARM                    APDPDD            1
647500070827     c                   Parm                    ApdScm            1
647600070827     c                   Parm                    ApdScp            1
647700070827     C                   PARM                    WFINE             1
647800070827     C                   PARM                    simfel            3 0
647900070827     C                   PARM                    apddfv            8 0
648000070827     C*
648100070827     C     WFINE         DOWNE     'S'
648200140620     C**   APDPDD        IFEQ      *BLANKS
648300070827     C                   MOVEL     APDPDR        PDR(X)
648400070827     C                   MOVEL     APDETM        ETM(X)
648500070827     C                   MOVEL     APDETP        ETP(X)
648600070827     c                   Movel     ApdScm        Scm(x)
648700070827     c                   Movel     ApdScp        Scp(x)
648800070827     C                   ADD       1             X
648900140620     C**                 ELSE
649000140620     C**                 MOVEL     APDPDR        PDD(Y)
649100140620     C**                 ADD       1             Y
649200140620     C**                 END
649300070827     C*
649400070827     C                   CALL      'FNLSA2R2'
649500070827     C                   PARM                    APDPDR
649600070827     C                   PARM                    APDETM
649700070827     C                   PARM                    APDETP
649800070827     C                   PARM                    APDPDD
649900070827     c                   Parm                    ApdScm            1
650000070827     c                   Parm                    ApdScp            1
650100070827     C                   PARM                    WFINE             1
650200070827     C                   PARM                    simfel            3 0
650300070827     C                   PARM                    apddfv            8 0
650400070827     C                   ENDDO
650500070827     c
650600070827     c* Controllo riempimento schiera
650700070827     c                   clear                   trul0sds
650800070827     c                   eval      i0sski='PDR/ETM/ETP/...'
650900070827     c                   eval      i0sele=%elem(pdr)
651000070827     c                   eval      i0spie=x-1
651100070827     c                   eval      i0sfile='FNAPD00F  '
651200070827     c                   eval      i0ssif=knsif
651300130718     c                   eval      i0spgm='FNLSU2R'
651400070827     c                   movel     trul0sds      kpjbu
651500070827     c                   call      'TRUL0SR'
651600070827     c                   parm                    kpjba
651700070827     c
651800140620     c**                 clear                   trul0sds
651900140620     c**                 eval      i0sski='PDD'
652000140620     c**                 eval      i0sele=%elem(pdD)
652100140620     c**                 eval      i0spie=y-1
652200140620     c**                 eval      i0sfile='FNAPD00F  '
652300140620     c**                 eval      i0ssif=knsif
652400140620     c**                 eval      i0spgm='FNLSU2R'
652500140620     c**                 movel     trul0sds      kpjbu
652600140620     c**                 call      'TRUL0SR'
652700140620     c**                 parm                    kpjba
652800130530     c
652900130530     c* in caso di parzializzazione per cod cliente
653000130530     c* Carico la £6 della linea partenza FGV per vedere se ha clienti abilita
653100130530     c*  ti alla borderizzazione
653200130530     c                   clear                   L6
653300130530     c                   if        pa2ccm>*zeros
653400130530     c                   clear                   trul06ds
653500130530     c                   eval      d06cod = '£6'
653600130530     c                   movel     simfel        d06key
653700130530     c                   move      'S'           d06key
653800130530     c                   movel     fgvdfv        d06drf
653900130530     c                   movel     'L'           d06tla
654000130530     c                   movel     trul06ds      kpjbu
654100130530     c                   call      'TRUL06R'
654200130530     c                   parm                    kpjba
654300130530     c                   movel     kpjbu         trul06ds
654400151127
654500151127     c* Carico eventuale tabella con i codici collegati da borderizzare
654600171107     C*                  MOVEL     'CLF'         kcod1
654700171107     c*                  z-add     1             zz
654800171107     C*                  eval      kke1=pa2ccm
654900171107     c*    ktbe          setll     tntbe01l
655000171107     c*    ktbe          reade     tntbe01l
655100171107    2c***                dow       not %eof(tntbe01l)
655200171107     c***                if        tbeatb=' '
655300171107     c***                movel     tbeke2        ccmb(zz)
655400171107     c***                add       1             zz
655500171107     c***                endif
655600171107     c***  ktbe          reade     tntbe01l
655700171107     c***                enddo
655800171124        clear w1v;
655900171124        clear bh ;
656000171107        k_Ke1 = %editc(simfel:'X');
656100171107        %subst(k_Ke2:1:7) = pa2ccm;
656200171107        setll ('CLH':k_ke1:k_ke2) tntbe01l;
656300171107        reade ('CLH':k_ke1) tntbe01l;
656400171107        dow not %eof(tntbe01l) and bh<=%elem(ccmb);
656500171107           if %subst(tbeke2:1:7)<>%subst(k_ke2:1:7);
656600171107               leave ;
656700171107           endif;
656800171124        // non carico se annullato
656900171124          if tbeatb=*blanks;
657000171124        // nel primo elemento di schiera metto sempre il codice ricevuto
657100171124           if w1v=*blanks;
657200171124             ccmb(1)=pa2ccm;
657300171124             bh=2;
657400171124             by=%elem(s_uni);
657500171124             w1v='1';
657600171124           endif ;
657700171107           dsuni=tbeuni;
657800171107        // se il numero di elementi che sto per aggiungere dovesse superare la
657900171107        // capacità della schiera
658000171107        // calcolo il numero effettivo di elementi che posso ancora aggiungere
658100171107           if bh+%elem(s_uni)>%elem(ccmb);
658200171107              by=%elem(ccmb)+1-bh;
658300171107           endif  ;
658400171107           %subarr(ccmb:bh:by)=s_uni;
658500171107           bh+=%elem(s_uni);
658600171124          endif;
658700171107           reade ('CLH':k_ke1) tntbe01l;
658800171107        enddo;
658900171107        // contro quanti elementi riempiti per passarlo al pgm che controlla
659000171107        // il riempimento di schiera
659100171107           bh=%checkr(' ':ds_ccmb);
659200171107           by=bh/%size(pa2ccm);
659300171107        // Controllo il riempimento di schiera
659400171107     c                   clear                   trul0sds
659500171107     c                   eval      i0sski='CLH'
659600171107     c                   eval      i0sele=%elem(ccmb)
659700171107     c                   eval      i0spie=by
659800171109     c                   eval      i0scod=' CLH '
659900171107     c                   eval      i0sfile='TNTBE00F  '
660000171107     c                   eval      i0ssif=knsif
660100171107     c                   eval      i0spgm='FNLSU2R'
660200171107     c                   movel     trul0sds      kpjbu
660300171107     c                   call      'TRUL0SR'
660400171107     c                   parm                    kpjba
660500151127     c
660600130530     c                   endif
660700130530     c
660800070827     c                   ENDSR
660900150603     C**********************************************************************
661000150603     C*  FIBSP00F - lnp/clienti con sospensione bolle
661100150603     C**********************************************************************
661200150603     c     sr_fibsp      begsr
661300170327     c                   clear                   w_SiBsp
661400150703     c                   clear                   w_bsptip
661500150603     c     w_clibo       chain     fibsp02l
661600151214     c                   if        %found(fibsp02l) and bspfpa='S'
661700151214     c                                              and bspaut=*blanks
661800151215     c                             and %lookup(blplnp:sbsp)>0
661900150603     C                   eval      w_SiBsp='S'
662000150703     c                   eval      w_bsptip='K'
662100150603     c                   else
662200150603     c     blplnp        chain     fibsp01l
662300150603     c                   if        %found(fibsp01l) and bspfpa='S'
662400151214     c                                              and bspaut=*blanks
662500150603     C                   eval      w_SiBsp='S'
662600150703     c                   eval      w_bsptip='L'
662700150603     c                   endif
662800150603     c                   endif
662900150603     c                   endsr
663000150703     C**********************************************************************
663100150703     C*  Memorizzazinoe clienti/lnp con bolle in sosp. oltre xx giorni
663200150703     C**********************************************************************
663300150703     c     sr_MemoEmail  begsr
663400150703    1c                   select
663500150703     c* SOSPENSIONE per CLIENTE
663600150703   WHc                   when      w_bsptip='K'
663700150703     c                   eval      indy=%lookup(sav_clibo:s_bspk)
663800150703    2c                   if        indy=0
663900150703     c                   eval      indy=%lookup(0:s_bspk)
664000150703     c* memorizzo cod cliente
664100150703     c                   eval      s_bspk(indy)=sav_clibo
664200150703    2c                   endif
664300150703     c* memorizzo numero di spedizioni in sosp oltre xx giorni
664400150703     c                   add       1             s_bspkbo(indy)
664500150703     c* memorizzo la data più bassa di immissione
664600150703    2c                   if        s_BSPkD(indy)=0 or
664700150703     c                             blpdim>s_BSPkD(indy)
664800150703     c                   eval      s_BSPkD(indy)=blpdim
664900150703    2c                   endif
665000150703     c* SOSPENSIONE per LINEA DI PARTENZA
665100150703   WHc                   when      w_bsptip='L'
665200150703     c                   eval      indy=%lookup(sav_lnp:s_bspL)
665300150703    2c                   if        indy=0
665400150703     c                   eval      indy=%lookup(0:s_bspL)
665500150703     c* memorizzo cod cliente
665600150703     c                   eval      s_bspL(indy)=sav_lnp
665700150703    2c                   endif
665800150703     c* memorizzo numero di spedizioni in sosp oltre xx giorni
665900150703     c                   add       1             s_bspLbo(indy)
666000150703     c* memorizzo la data più bassa di immissione
666100150703    2c                   if        s_BSPlD(indy)=0 or
666200150703     c                             blpdim>s_BSPlD(indy)
666300150703     c                   eval      s_BSPlD(indy)=blpdim
666400150703    2c                   endif
666500150703    1c                   endsl
666600150703     c
666700150703     c                   endsr
666800150703     c*-------------------------------------------------------------------
666900150703     c     INV_email     BEGSR
667000150703     c
667100150703     c                   clear                   writerig
667200150703     c                   if        not %open(prtemail)
667300150703     C                   EXSR      SR_OPENPRTF
667400150703     C                   endif
667500150703     c
667600150703     c                   exsr      Routend
667700150703     c
667800150703     c                   ENDSR
667900151022       //--------------------------------------------------------------
668000151022       //?  Errore per bolla non trovata
668100151022       //--------------------------------------------------------------
668200151022     c     Sr_errore     BEGSR
668300151022
668400151022     C                   movel     *blanks       wrkEml          253
668500151022     C                   movel     *blanks       wrkEmlCc        253
668600151022     C                   movel     *blanks       wrkMsg         5000
668700151022     C                   movel     *blanks       wrkOgg           44
668800151022     C                   movel     *blanks       wrkesito          1
668900151022     c
669000151022     C                   MOVEL     MSG(5)        wrkogg
669100151022     C                   MOVEL(P)  MSG(6)        wrkmsg
669200151022     c                   eval      %subst(wrkmsg:43:16)=
669300151022     c                             %editc(bltspe:'X')
669400151022     c                   eval      wrkemlcc ='ced@brt.it'
669500151022     c                   eval      wrkeml='elisa.sanfelici@brt.it'
669600151022     c
669700151022     C                   call(e)   'TIS701C1'
669800151022     C                   parm                    wrkEml
669900151022     C                   parm                    wrkEmlCc
670000151022     C                   parm                    wrkOgg
670100151022     C                   parm                    wrkMsg
670200151022     C                   parm                    wrkesito
670300151022
670400151022     c                   ENDSR
670500150703      /free
670600150703       //--------------------------------------------------------------
670700150703       //?Override al file di stampa per impostarvi i dati per
670800150703       //?  l'invio via e-mail   +   stampa inizio e-mail
670900150703       //--------------------------------------------------------------
671000150703       BEGSR sr_OpenPrtF;
671100150703
671200150703         // Override al file di stampa per impostarvi i dati per
671300150703         // l'invio via e-mail
671400150703           exsr sr_Override;
671500150703
671600150703
671700150703           indy = 1 ;
671800150703
671900150703          dow     s_BSPl(indy)>0  ;
672000150703
672100150703          if      s_BSPlBO(indy)>0 ;
672200150703          dataiso=%date(s_BSPlD(indy)) ;
672300150703          data_eur=dataiso  ;
672400150703
672500150703           clear O_testo;
672600150703           chain  s_BSPl(indy)   azorg01l  ;
672700150703           if not %found(azorg01l)  ;
672800150703           eval orgdes=*all'?'  ;
672900150703           endif  ;
673000150703           %subst(O_testo:6:20) = %editc(s_BSPl(indy):'X')+'-'+orgdes  ;
673100150703           %subst(O_testo:46:10) =%char(data_eur)  ;
673200150703           %subst(O_testo:64:9 ) =%editc(s_BSPlBO(indy):'2')  ;
673300150703           except PrtDet;
673400150703           writerig=writerig+1  ;
673500150703           endif  ;
673600150703
673700150703           // se ne ho scritte 50 di righe devo inviare
673800150703           if writerig>=50  ;
673900150703             exsr      Routend  ;
674000150703             clear writerig                  ;
674100150703
674200150703             exsr sr_Override;
674300150703
674400150703           endif  ;
674500150703
674600150703           indy=indy+1  ;
674700150703           enddo    ;
674800150703
674900150703          // Stesso ciclo sui cod clienti
675000150703          indy = 1      ;
675100160208          dow     s_BSPk(indy)>0  ;
675200150703
675300150703          if      s_BSPkBO(indy)>0 ;
675400150703          dataiso=%date(s_BSPkD(indy)) ;
675500150703          data_eur=dataiso  ;
675600150703
675700150703           clear O_testo;
675800170613           chain  (1:d_dutkci:s_BSPk(indy)) cnaco00f  ;
675900150703           if not %found(cnaco00f)  ;
676000150703           eval acorag=*all'?'  ;
676100150703           endif  ;
676200150703           %subst(O_testo:6:20) = %editc(s_BSPk(indy):'X')+'-'+acorag  ;
676300150703           %subst(O_testo:46:10) =%char(data_eur)  ;
676400150703           %subst(O_testo:64:9 ) =%editc(s_BSPkBO(indy):'2')  ;
676500150703           except PrtDet;
676600150703           writerig=writerig+1  ;
676700150703           endif  ;
676800150703
676900150703           // se ne ho scritte 50 di righe devo inviare
677000150703           if writerig>=50  ;
677100150703             exsr      Routend  ;
677200150703             clear writerig                  ;
677300150703
677400150703             exsr sr_Override;
677500150703
677600150703           endif  ;
677700150703
677800150703           indy= indy+1  ;
677900150703           enddo    ;
678000150703
678100150703             exsr      Routend  ;
678200150703       ENDSR;
678300150703
678400150703       //--------------------------------------------------------------
678500150703       //?Override al file di stampa per impostarvi i dati per
678600150703       //?  l'invio via e-mail   +   stampa inizio e-mail
678700150703       //--------------------------------------------------------------
678800150703       BEGSR sr_Override;
678900150703
679000150703         reset TRTCM1ds;
679100150703
679200150703         IF  §MRAdreg <> *blank;
679300150703           §CM1mitt = %trim(§MRAdmitt);
679400150703           §CM1dst  = %trim(§MRAddest);
679500150703
679600150703         // Valorizzo i campi della e-m@ail
679700150703          chain  simfel   azorg01l ;
679800150703
679900150703          if   %found(azorg01l) ;
680000150703           og140=orgde0  ;
680100150703           if §ogapo>*zeros  ;
680200150703           §cm1dst=%trim(§cm1dst) +'; CED'+ §ogapo +'@brt.it;';
680300150703           else  ;
680400150703           §cm1dst=%trim(§cm1dst) +'; CED'+%editc(orgf70:'X')+'@brt.it;';
680500150703          endif  ;
680600150703          endif  ;
680700150703
680800150703           §CM1tips = §MRAdreg;
680900150703           §CM1po   = %editc(simfel:'X');
681000150703           §CM1var  = '*OBJM*' + 'SOSPENSIONE AUTOMATICA bolle      '   ;
681100150703           §CM1idp  = §MRAdidpro;
681200160208           Qcmd1 = C_CmdOvrPrtF
681300150703                + ' outq(' + %trim(§MRAdoutqi) + ')'
681400150703                + ' usrdfndta(''' + TRTCM1ds + ''')';
681500150703         ELSE;
681600160208           Qcmd1 = C_CmdOvrPrtF;
681700150703         ENDIF;
681800150703
681900150703         callp(e) qCmdExc(Qcmd1 : %size(Qcmd1));
682000150703
682100150703           open PrtEMAIL;
682200150703
682300150703         // Stampa una testata se NON è richiesta la e-mail
682400150703           IF  §MRAdreg =  *blank;
682500150703             O_testo = JobUser + ' - ' + nompgm
682600150703                     + ' - ' + %editc( *date : 'Y' )
682700150703                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
682800150703             except PrtDet;
682900150703             clear O_testo;
683000150703             except PrtDet;
683100150703             except PrtDet;
683200150703           ENDIF;
683300150703
683400150703         // Stampa testo iniziale
683500150703           O_testo = 'E'' stata eseguita la Sospensione Automatica +
683600150703                      di bolle immesse da almeno xx giorni.' ;
683700150703           %subst(O_testo:72:2) = (%editc(§3iggm: '4'))     ;
683800150703           except PrtDet;
683900150703           O_testo = 'Verificare col mittente o procedere alla lor+
684000150703                      o chiusura come "Merce MAI Affidata".' ;
684100150703           except PrtDet;
684200150703
684300150703         // Stampa una riga vuota
684400150703           clear O_testo;
684500150703           except PrtDet;
684600150703
684700150703         // Stampa intestazione
684800150703           O_testo = ' Bolle spostate in sospensione di          Data imm+
684900150703                     issione    Bolle da verificare       ';
685000150703           except PrtDet;
685100150703           O_testo = 'Codice Cliente o Linea di Partenza          più vec+
685200150703                     chia                                 ';
685300150703         //%subst(o_testo:90:10)= knmus     ;
685400150703           except PrtDet;
685500150703
685600150703         // Stampa una riga vuota
685700150703           clear O_testo;
685800150703           except PrtDet;
685900150703       ENDSR;
686000150703
686100150703       //--------------------------------------------------------------
686200150703       //?Operazioni finali.
686300150703       //--------------------------------------------------------------
686400150703       BEGSR RoutEnd;
686500150703
686600150703         if %open(PrtEMAIL);
686700150703
686800150703         //?Chiusura dello spool?
686900150703           clear O_testo;
687000150703           except PrtDet;
687100150703           O_testo = '***   Fine Lista   ***';
687200150703           except PrtDet;
687300150703
687400150703           close PrtEMAIL;
687500150703
687600150703         //?Eliminazione overflow?
687700160208           Qcmd1 = C_CmdDltOvr;
687800150703           callp(e) qCmdExc(Qcmd1 : %size(Qcmd1));
687900150703
688000150703         endif;
688100150703
688200150703       ENDSR;
688300150703
688400150703      /end-free
688500150703     o* ---------------------------------------------------------------
688600150703     o* ?Spool di stampa (per e-mail).
688700150703     o* ---------------------------------------------------------------
688800150703     oPrtEMAIL  e            PRTdet      1
688900150703     o                       O_testo
689000030902
689100030902      *---------------------------------------------------------------*
689200030902
689300960201**         CMD1
689400021014OVRDBF FILE(FNBLPE0T)               MBR(Mxxx000)
689500960202**         CM19
689600021014OVRDBF FILE(FNBLPT0T)               MBR(Mxxx000)
689700960202**         CM32
689800021014OVRDBF FILE(FISPP00T)               MBR(Mxxx000)
689900000428**         CM37
690000021014OVRDBF FILE(FNFWA00T)               MBR(Mxxx000)
690100000428**         CM38
690200021014OVRDBF FILE(FNFVA01T)               MBR(Mxxx000)
690300920324**         CM41
690400021014ALCOBJ OBJ((FNBLPE0T *FILE *EXCL Mxxx000))
690500920324**         CM47
690600021014ALCOBJ OBJ((FISPP00T *FILE *EXCL Mxxx000))
690700000428**         CM48
690800021014ALCOBJ OBJ((FNFWA00T *FILE *EXCL Mxxx000))
690900000428**         CM49
691000021014ALCOBJ OBJ((FNFVA01T *FILE *EXCL Mxxx000))
691100920324**         CM51
691200021014ALCOBJ OBJ((FNBLPT0T *FILE *EXCL Mxxx000))
691300920324**  ERRORI
691400920324XXXXXXXXXXXXXXXXXXXX/XXX       ** ELENCO FOGLI VIAGGIO NON ELABORA
691500920324TI PERCHE' MEMBRI GIA' ALLOCATI **
691600920324 FOGLIO VIAGGIO
691700920324           ** TELEFONARE CON URGENZA ALL'UFFICIO C.E.D BOLOGNA  **
691800960129** MSG
691900960129ERRORE TRASMISSIONE FOGLI VIAGGIO PARTENZE
692000111019A FILIALE: NON TRASMESSO Foglio Viaggio XXX-XX.XXX.
692100070827CREATO JOBLOG per lavoro durato più di 5 minuti
692200060714                         FOGLIO VIAGGIO XXX-XXXXX.
692300151022ERRORE DI BOLLA DISALLINEATA FNLSU2R
692400151022Presente fnBLT00f senza relativo fnBLP -->
692500060714**
692600060714DSPJOBLOG OUTPUT(*PRINT)
