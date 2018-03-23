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
010200160129     d $MaxElem        c                   const(4500)
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
013100151127     D ccmb            S              7    DIM(301)
013200130226     d
013300931202     D* SCHIERE PER RICERCA CAPOCONTI
013400000321     D*
013500021014     D CMD1            S             48    DIM(1) CTDATA PERRCD(1)
013600021014     D CM19            S             48    DIM(1) CTDATA PERRCD(1)
013700021014     D CM32            S             48    DIM(1) CTDATA PERRCD(1)
013800021014     D CM37            S             48    DIM(1) CTDATA PERRCD(1)
013900021014     D CM38            S             48    DIM(1) CTDATA PERRCD(1)
014000021014     D CM41            S             42    DIM(1) CTDATA PERRCD(1)
014100021014     D CM47            S             42    DIM(1) CTDATA PERRCD(1)
014200021014     D CM48            S             42    DIM(1) CTDATA PERRCD(1)
014300021014     D CM49            S             42    DIM(1) CTDATA PERRCD(1)
014400021014     D CM51            S             42    DIM(1) CTDATA PERRCD(1)
014500920324     D ERR             S             66    DIM(4) CTDATA PERRCD(1)
014600151022     D MSG             S             70    DIM(6) CTDATA PERRCD(1)
014700000619     D SPT             S              3  0 DIM(100)
014800060714     D CMjob           S             42    DIM(1) CTDATA PERRCD(1)
014900950109     D***
015000950109     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
015100950109     D***
015200950109     D WLBDAT          DS                  INZ
015300950109     D  G02DAT                 1      8  0
015400950109     D  G02INV                 9     16  0
015500950109     D  G02ERR                17     17
015600950109     D  G02TGI                18     22  0
015700940527     D*
015800950110     D                 DS
015900950110     D  FVDTS1                 1      1
016000950110     D  FVDTS2                 2      2
016100950110     D  FVDTS3                 3      3
016200950110     D  FVDTS4                 4      4
016300950110     D  FVDTS5                 5      5
016400950110     D  FVDTS6                 6      6
016500950110     D  TSP                    1      6
016600950110     D                                     DIM(6)
016700950110     D                 DS
016800950110     D  FVDVL1                 1      9  4
016900950110     D  FVDVL2                10     18  4
017000950110     D  FVDVL3                19     27  4
017100950110     D  FVDVL4                28     36  4
017200950110     D  FVDVL5                37     45  4
017300950110     D  FVDVL6                46     54  4
017400950110     D  VOL                    1     54  4
017500950110     D                                     DIM(6)
017600150610
017700150610     d                 ds
017800150610     d bsplin
017900150610     d sbsp                           3  0 dim(30) overlay(bsplin)
018000150610
018100910911     D KPJBA         E DS
018200930127     D  LIBSYS                92    101
018300150603     D fnlsu3ds        DS
018400150603     D  ilsu3flg               1      1
018500150603     D  ilsu3aas               2      5
018600150603     D  ilsu3lnp               6      8
018700150603     D  ilsu3nrs               9     10
018800150603     D  ilsu3nsp              11     17
018900150603     D  ilsu3DSP_B            18     25
019000150603     D  ilsu3DSP_S            26     33
019100150603     D  ilsu3nos              34     34
019200150603     d
019300150603     d  ilsu3tcr              35     35
019400150603     d  ilsu3dcr              36     43
019500150603     d  ilsu3hcr              44     47
019600150603     D  olsu3err             100    101
019700150603     D  olsu3boagg           102    110
019800150603     D  olsu3bonagg          111    116
019900150703     D  olsu3msg             117    193
020000150703     D  olsu3sosp            194    194
020100170419     D  Ilsu3NOAGG           201    201
020200021010     D* DS PER TIBS55R - RICERCA S.I. IN CUI SI E'
020300021010     D DSBS55        E DS                  EXTNAME(TIBS55DS)
020400011127     d dgedcn        e ds
020500010309     D DSBL4M        E DS
020600040609     D Dar5gen       E DS
020700000619     D DANM01        E DS
020800000619     D  §ANZNC       E                     EXTFLD(§ANMZNC)
020900911018     D DS7C          E DS
021000940414     D DS7J          E DS
021100911018     D DS3A          E DS
021200910911     D DS1B          E DS
021300951120     D DSTV          E DS
021400010622     D DSFV4V        E DS
021500121119     D Ddgd          E DS                  INZ
021600060710     D DS3B          E DS
021700150703     D DS3I          E DS
021800150703     D og140         E DS
021900900515      *
022000950118     D* LINEE DI ARRIVO  PER FNFGV00F
022100910201     D                 DS
022200921202     D  FGVLNA                 1      3  0
022300921202     D  FGVFFV                 4    243
022400921202     D  FGVFF2               244    450
022500000428     D  FGWFF3               451    690
022600000428     D  FGWFF4               691    900
022700000428     D  VUOTO                901    903
022800000428     D  FFV                    1    903
022900000428     D                                     DIM(301)
023000900522      *
023100950118     D* FILIALI SCARICO  PER FNFVG00F
023200910201     D                 DS
023300921202     D  LNA2                   1      3  0
023400921202     D  FGVFLP                 4    243
023500921202     D  FGVFL2               244    450
023600000428     D  FGWFL3               451    690
023700000428     D  FGWFL4               691    900
023800000428     D  VUOTO2               901    903
023900000428     D  FLP                    1    903
024000000428     D                                     DIM(301)
024100950118      *
024200930127     D                 DS
024300000731     D  EMME                   1      1
024400021014     d  Num31                  2      4  0
024500021014     D  NUM3                   5      7  0
024600930127     D  FISMBR                 1     10
024700960503     D                 DS
024800960503     D  BLPAAS                 1      4  0
024900960503     D  BLPMGS                 5      8  0
025000960503     D  BLPDSP                 1      8  0
025100970307     D                 DS
025200970307     D  WRKAAS                 1      4  0
025300970307     D  WRKLNP                 5      7  0
025400970307     D  WRKNRS                 8      9  0
025500970307     D  WRKNSP                10     16  0
025600970307     D  WRKSPE                 1     16  0
025700970307     D  WRKTBO                17     17
025800970307     D  WRKKEY                 1     17
025900970423     D******
026000961007      * PASSAGGIO A CALCOLO VOLUMI CML              -FNLSB7R-
026100961007     D PARAM1          DS
026200010213     D  PA1NUM                17     17  0 INZ
026300961007     D  PA1FEL               158    160  0
026400970423     D******
026500970423      * NUMERO DI FOGLIO VIAGGIO PASSATO
026600130524     D FNLSU2DS        DS
026700940414     D* CATEGORIA FOGLIO I.M.P.
026800940414     D  NPG1                   1      1  0
026900940414     D* NUMERO    FOGLIO I.M.P.
027000940414     D  NFV1                   2      4P 0
027100940415     D* DATA      FOGLIO I.M.P.
027200950915     D  DFV1                   5      9P 0
027300950113     D*
027400960209     D  PA2AGS                10     10
027500940505     D  FGV2                  11     15  0
027600991022     D* PASSATO DA CLP FNLSA2C
027700040531     D* ESAZI                 16     16
027800991022     D  NUM2                  17     17  0
027900960126     D* PA2FLG = "M" ---> RICHIAMATO DALLA GESTIONE F.VIAGGIO
028000960126     D  PA2FLG                19     19
028100930127     D  L1                    78    137P 0
028200910909     D                                     DIM(30)
028300930127     D  RSUT                 138    157
028400930127     D  SIMFEL               158    160  0
028500950109     D* FILIALE GESTIONE FOGLIO I.M.P.
028600950109     D  FGS1                 164    166  0
028700970828     D* FILIALE ELABORATORE SIMFEL HA CML
028800970423     D  PARCML               167    167
028900010910     D  pa2agp               168    168
029000040121     D  PA2spv               169    169
029100040121     D  PA2spp               170    170
029200130527     D  PA2CCM               171    177
029300130530     D  PA2barcode           178    178
029400970423     D******
029500950109     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
029600931209     D PARAM3          DS
029700931209     D  PA3PDR                 1      7  0
029800931209     D  PA3TSR                 8      8
029900931209     D  PA3NDC                 9     15  0
030000931209     D  PA3FCT                16     16
030100931209     D  PA3FVD                17     17
030200931209     D  PA3FVT                18     18
030300950115     D  PA3DDC                19     26  0
030400041122     D  PA3sml               256    256
030500070423     D PARAMa3         DS
030600070423     D  PA3NFV                 1      5
030700070423     D* pa3tfp deve contenere il terminal di partenza per poter elaborare e
030800070423     d* trasmettere le bolle per terminal di partenza
030900070423     d* se non passato un valore > '000' il programma elabora e trasmette
031000070423     d* TUTTE le bolle presenti nel file fnblp46f (come faceva prima)
031100070423     D  PA3tfp                 6      8
031200111123     D  PA3num                 9      9    inz('0')
031300111123     d
031400920324     D* PER ESPLOSIONE DATI IN RICEZIONE
031500911001     D PARAM5          DS
031600950123     D  PARDTP                 1      8  0
031700950919     D  PARNFV                 9     13  0
031800960806     D  PARDEF                14     14
031900971106     D* FLAG PASSATO DAL FNLS93R PER DIRE SE FILE ALLOCATI
032000971106     D  WLCK                  15     15
032100000621     D  PARFEL               158    160  0
032200021015     D  LEGFIL               237    246    INZ('FXXX999999')
032300021015     d  MbrFil4              247    250    INZ('MXXX')
032400021015     d  MbrFil3              251    253    Inz('YYY')
032500021015     d  MbrFil               247    256
032600940415     D*
032700940415     D* PER PRENDERE FASE ANOMALIA                  -TRUL11R-
032800940415     D PARAM9          DS
032900950113     D  PA9TLA                 1      1    INZ('L')
033000950113     D  PA9NPG                 2      2    INZ('3')
033100950113     D  PA9SPG                 3      3    INZ('P')
033200940415     D  FASE                   4      4    INZ
033300040617
033400040617      * passaggio dati alla chiusura orm abbinati a bolle in sospensione
033500040617     d Param6          ds
033600040617     d  pa6fgs                        3  0
033700040617     d  pa6nfv                        5  0
033800940415     D*
033900931202     D TCUDS           DS
034000931202     D  F1                     1      1
034100931202     D  F3                     3      3
034200931202     D  F2                     2      2
034300931202     D  F4                     4      4
034400931202     D  F56                    5      6
034500931020     D                 DS
034600950113     D  BLPPDR                 1      7  0
034700950109     D  BLPDRT                 8     15  0
034800950113     D  BLPNRT                16     16  0
034900950113     D  BLPFPP                17     17
035000950113     D  COMBLP                 1     17
035100950115     D                 DS
035200950115     D  WCPR                   1      7  0 INZ
035300950115     D  WDRT                   8     15  0 INZ
035400950115     D  WNRT                  16     16  0 INZ
035500950115     D  WFPP                  17     17    INZ
035600950115     D  COMSAV                 1     17
035700940414     D                 DS
035800950109     D  BLTAAS                 1      4  0
035900950109     D  BLTLNP                 5      7  0
036000950109     D  BLTNRS                 8      9  0
036100950109     D  BLTNSP                10     16  0
036200970307     D  BLTSPE                 1     16  0
036300970307     D  BLTTBO                17     17
036400970307     D  BLTKEY                 1     17
036500950109     D                 DS
036600950109     D  BTTAAS                 1      4  0
036700950109     D  BTTLNP                 5      7  0
036800950109     D  BTTNRS                 8      9  0
036900950109     D  BTTNSP                10     16  0
037000970307     D  BTTSPE                 1     16  0
037100970307     D  BTTTBO                17     17
037200970307     D  BTTKEY                 1     17
037300931202     D*
037400931202     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
037500931202     D  TCU                  398    697
037600931202     D                                     DIM(50)
037700931202     D  KCU                  698    847P 0
037800931202     D                                     DIM(50)
037900931202     D                                     PACKEVEN
038000970424     D* DS ESTERNA PER CHIUSURA ANOMALIE
038100960416     D DSLR33        E DS                  EXTNAME(FNLR33DS)
038200970424     D* DS PER REPERIMENTO DATI FOGLIO
038300970424     D DSLV53        E DS                  EXTNAME(FNLV53DS)
038400970516     D* DS PER DSLV55R - PASSAGGIO PARAMETRI
038500970516     D DSLV55        E DS                  EXTNAME(FNLV55DS)
038600031110     D* DS PER DSLV54R - PASSAGGIO PARAMETRI
038700031110     D FNLV54DS      E DS
038800070221     D* DS PER FNLS45R - PASSAGGIO PARAMETRI
038900070221     D FNLS45DS      E DS
039000130610     d
039100151118     D FNANM1          DS
039200151118     D  ANMNR1               397    400B 0
039300990517     D FNANM2          DS
039400960416     D  ANMNR2               397    400B 0
039500021021     D FNAN20          DS
039600960705     D  ANMNR0               397    400B 0
039700080109      *
039800080109     D DSE5F           DS            87    OCCURS($MaxElem)
039900960701     D  E5F                    1     87
040000960701     D                                     DIM(29)
040100070123     D DSBRV         E DS                  EXTNAME(FNBRV00F)
040200130527     D DSbrvw5       E DS                  EXTNAME(fnbrv00f)
040300130530     D                                     prefix(W5:3)
040400021016
040500070711     d dfar          e ds
040600021016     d Tibs02Ds      e ds
040700050524     d
040800050524     d* DS x controllo riempimento schiere
040900050518     d trul0sds      e ds
041000130530
041100130530     d Trul06ds      e ds
041200130530     d  L6                     1     90  0 dim(30)
041300130530
041400050524     d
041500021016      * Ds date statistiche
041600021016     d dSdf          e ds
041700030107
041800021014     D VAR             DS             8
041900021014     d  Varx1                  2      4
042000021014     D  VARX                   5      7
042100031112     D  N_varx                 5      7  0
042200021014     D VAR2            DS             9
042300021014     d  Var2x1                 2      4
042400021014     D  VAR2X                  5      7
042500030902      *
042600030902     d Qcmd            s            150    inz
042700150703     d Qcmd1           s            512    inz
042800030902     d Qlen            s             15  5 inz(150)
042900030902      *
043000040422     d wcrtspu         s              1
043100040802     d wfgsspu         s              3  0
043200130527     d IX              s              4  0
043300031110
043400130627     D WNFV60          S                   LIKE(BRVNFV)
043500130531     D kfgv            S                   LIKE(fgvnfv)
043600130530     D WNFV5           S                   LIKE(FVDNFV) inz
043700130530     D KNPG            S                   LIKE(BRVNPG) inz
043800130527     D WVVDFV          S                   LIKE(D53DFV) inz
043900031110     d s_o54tfp        s                   like(o54tfp)
044000031112     d s_o54tfa        s                   like(o54tfa)
044100050204     d FLPTFA          s                   like(o54tfa)
044200040218     d wtfa            s                   like(o54tfa)
044300040310     d ktrd            s                   like(ar5trd)
044400050203     d kfgs            s                   Like(FvvFgs)
044500050203     d xx              s              2  0
044600050203     d yy              s              3  0
044700151127     d zz              s              3  0
044800130226     d Indx            s              3  0
044900150703     d Indy            s              3  0
045000150703     D Writerig        S              4  0 inz
045100130226     d SAVfnrs         s              5  0 inz
045200130226     d Wflsnrs         s              5  0
045300050203     d winvialocale    s              1
045400130226     d wTFP90          s              1
045500060516     d kdstfgs         s                   like(dstfgs)
045600060516     d kdstnfv         s                   like(dstnfv)
045700120607     d kdstnpg         s                   like(dstnpg)
045800060714     d
045900060714     d worap           s               z
046000060714     d worad           s               z
046100060714     d minuti          s              5  0
046200070219     D wtimez          S               z
046300070219     D dataiso         S               d   datfmt(*iso)
046400070219     D oraiso          S               t   timfmt(*iso)
046500150703     D data_eur        S               d   datfmt(*eur)
046600150603     D w_SiBsp         S              1
046700150703     D w_bsptip        S              1
046800150610     D w_FilBsp        S              1
046900151215     D w_FilBsps       S              1
047000150605     d sav_clibo       s                   like(blpccm)
047100150605     d w_clibo         s                   like(blpccm)
047200150605     d sav_lnp         s                   like(blplnp)
047300150703     d O_testo         s            132    inz
047400150703
047500150703      // - Comando di override al PrtF
047600150703     d C_CmdOvrPrtF    c                   const('OVRPRTF +
047700150703     d                                           file(PRTEMAIL) +
047800150703     d                                           pagesize(66 132) +
047900150703     d                                           lpi(6) cpi(10) +
048000150703     d                                           ovrscope(*actgrpdfn) +
048100150703     d                                           ')
048200150703     d C_CmdDltOvr     c                   const('DLTOVR +
048300150703     d                                            file(PRTEMAIL) +
048400150703     d                                            lvl(*actgrpdfn)')
048500150703      // - Tabella "MRA" = Bart-Mailing - Danni
048600150703     d dMRAdan       e ds                  inz
048700150703      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
048800150703     d TRTCM1ds      e ds                  inz
048900150703      //   ·§CM1mitt = Indirizzo e-mail del mittente
049000150703     d   §CM1mitt    e                     inz(' ed@brt.it')
049100150703      //   ·§CM1dst  = Indirizzo e-mail del destinatario
049200150703     d   §CM1dst     e                     inz('elisa.sanfelici@brt.it')
049300150703      //   ·§CM1tips = Tipo lettera via e-mail:
049400150703      //    "LET" = testo allegato in corpo con logo
049500150703      //            (richiede righe libere iniziali per il logo)
049600150703      //    "COI" = testo integrato senza logo senza BRT SPA iniziale
049700150703      //            (non consente né UNDERLINE né HIGHLIGHT)
049800150703     d   §CM1tips    e                     inz('COI')
049900150703      //   ·§CM1po   = Filiale
050000150703     d   §CM1po      e                     inz('046')
050100150703      //   ·§CM1var  = Oggetto e-mail
050200150703     d   §CM1var     e                     inz('*OBJM*+
050300150703     d                                     Manutenzione data spedizione+
050400150703     d                                      bolle ')
050500150703      //   ·§CM1sts  = Stato
050600150703     d   §CM1sts     e                     inz(*off)
050700150703      //   ·§CM1sts  = Id processo
050800150703     d   §CM1idp     e                     inz('2')
050900150703     D*
051000150703     D s_BSPl          S              3  0 DIM(599)
051100150703     D s_BSPlBO        S              7  0 DIM(599)
051200150703     D s_BSPlD         S              8  0 DIM(599)
051300150703     d
051400150703     D s_BSPk          S              7  0 DIM(799)
051500150703     D s_BSPkBO        S              7  0 DIM(799)
051600150703     D s_BSPkD         S              8  0 DIM(799)
051700150703
051800150703      // - Esecuzione comando di sistema
051900150703     d qCmdExc         pr                  extpgm('QCMDEXC')
052000150703     d  Qcmd1                       512    const  options(*varsize)
052100150703     d  Qlen                         15  5 const
052200070219
052300070219     D                SDS
052400070219     D  NOMPGM                 1     10
052500150703     d   JobUser             254    263
052600050203      *
052700900515      *
052800900601      *---------------------------------------------------------------*
052900900601      * INDICATORI USATI
053000930129      *---------------------------------------------------------------*
053100991026      * 01    = TRASMETTO
053200910911      * 04    = DEVO GENERARE AUTOSPUTA
053300931102      * 05    = LOKUP NELLA SCHIERA DEI PADRONCINI
053400910910      * 06    = LINEA DI ARRIVO CHE TRANSITA
053500920427      * 08    = INVIO ALLA LINEA ARRIVO CON SCARICO IL SUO TERMINAL
053600921202      * 09    = ESISTONO BOLLE SUL FOGLIO VIAGGIO DA INVIARE
053700920526      * 15    = CHAIN SU FV NON RIUSCITA: RISOTTOMETTO AZIONE
053800931027      * 16    = CHAIN IN TABEL PER CONTROLLARE SE IL TIPO BOLLA E' DA
053900931027      *         ESCLUDERE
054000920703      * 17    = LINEA ARRIVO E/O SCARICO >< DA TERMINAL: VEDO SE ECCEZI
054100950110      * 18    = CHAIN SU FNFVD000
054200900608      * 19/21 =DI COMODO
054300950109      * 22    = LETTURA FNBLP00F
054400991026      * 26    = LETTURA FNBLT00F
054500920324      * 28    = CI SONO ERRORI IN SCRITTURA FILE STORICO
054600970507      * 29/37 = DI COMODO
054700950915      * 40    = ELABORO LE BOLLE  DELL'AREA
054800950915      * 41    = ELABORO LE BOLLE  TRANSITO
054900940414      * 42    = ELABORO LE ANOMALIE DISGUIDO
055000950915      * 43    = ELABORO LE SPUNTE ENTRATA PER FILIALE CON CML
055100960209      * 44    = SIMFEL GENERA LE SPUNTE DA SPUNTA ENTRATA(COME CML)
055200950915      * 50    = MEMBRI FILE GIA' ALLOCATI: IMPOSSIBILE ESEGURE CHIUSURA
055300080228      * 90    = errore di scrittura file o lookup
055400080228      * 91    = LETTURA BOLLE DA LETTURA SPUNTA ENTRATA E SPUNTE PART.
055500960503      * 92    = ESISTE ALMENO UNA SPUNTA LOCALE
055600930127      * 93/99 = OPERAZIONI DA ESEGUIRSI NON LA PRIMA VOLTA
055700930708     C*---------------------------------------------------------------*
055800960202     C**
055900150703
056000150703     c                   clear                   s_bspk
056100150703     c                   clear                   s_bspkBO
056200150703     c                   clear                   s_bspkD
056300150703     c                   clear                   s_bspl
056400150703     c                   clear                   s_bsplBO
056500150703     c                   clear                   s_bsplD
056600150703
056700000619    1C     FGV2          IFNE      99999
056800020807     C     K02FGV01      CHAIN(N)  FNFGV01L                           19
056900020807     C     K02FGV01      CHAIN     FNFGW01L                           10
057000000428     C     *IN10         IFEQ      *ON
057100000428     C     FGWATB        ORNE      ' '
057200000428     C                   MOVEL     'N'           WFGW              1
057300000428     C                   CLEAR                   FGWFF3
057400000428     C                   CLEAR                   FGWFF4
057500000428     C                   CLEAR                   FGWFL3
057600000428     C                   CLEAR                   FGWFL4
057700000619     C                   CLEAR                   FGWFGS
057800000428     C                   ELSE
057900000428     C                   MOVEL     'S'           WFGW              1
058000000428     C                   ENDIF
058100000731     C                   Z-ADD     FGWFGS        WFGSFV
058200000731     C     WFGSFV        IFEQ      0
058300000731     C                   Z-ADD     FGVLNP        WFGSFV
058400040802     c*
058500040802     c                   eval      wfgsspu=wfgsfv
058600000619     C                   ENDIF
058700040802     c
058800130227     C                   MOVEL     'N'           WFVPT             1
058900000428     C*
059000960212     C* MEMORIZZO SE IL FV E' DA TRASMETTERE IN SEDE O MENO
059100960212     C                   MOVEL     'TV'          COD
059200960212     C                   MOVEL(P)  FGVTTR        KEY
059300960212     C     KTAB          CHAIN     TABEL                              36
059400960212    3C     *IN36         IFEQ      *OFF
059500960212     C     TBLFLG        ANDEQ     ' '
059600960212     C                   MOVEL     TBLUNI        DSTV
059700960212     C                   ELSE
059800960212     C                   MOVEL     'S'           §TVUTC
059900960806     C                   CLEAR                   §TVDEF
060000960212     C                   ENDIF
060100971106     C*
060200000619     C* PARDTP E' USATO NELLA PARAM5 E NELLA CREAZIONE SPOOL DI BACKUP
060300971106     C                   MOVEL     FGVDFV        PARDTP
060400000619     C                   MOVEL     §TVDEF        PARDEF
060500000619   X1C                   ELSE
060600130524     c* L'elaborazione dei fogli 99999 utilizzata ma utilizzata per trasmett
060700061108     c* ere le bolle di recupero di fine anno prima della fatturazione
060800061108     C                   MOVEL     'N'           WFVPT
060900000619     C                   Z-ADD     DATEU         PARDTP
061000070301     C                   Z-ADD     DATEU         fgvdfv
061100000619     C                   CLEAR                   PARDEF
061200000619    1C                   ENDIF
061300971106     C                   MOVEL     FGV2          PARNFV
061400000621     C                   MOVE      SIMFEL        PARFEL
061500060714     c* Prendo ora di apertura pgm
061600060714     C                   time                    worap
061700070827     c
061800130530     c* Carico codici autotrasportatori  + L6
061900070827     c                   EXSR      CarPAD
062000920526     C*
062100960123     C* R I C H I A M A T O   D A   G E S T I O N E   F.V I A G G I O
062200960123    1C     PA2FLG        IFEQ      'M'
062300960123     C*
062400130530     c
062500130530     C* ALLOCO MEMBRI DI TRASMISSIONE PER IL FOGLIO VIAGGIO
062600960123     C                   EXSR      ALLOCO
062700960202     C**
062800960123     C* ELABORO FOGLI VIAGGIO PARTENZE
062900021014     C                   MOVE      ')'           VAR               8
063000000725     C* MEMBRI DIVERSI A SECONDA CHE SIA FV POSTE OPPURE NO VISTO
063100000725     C* CHE ELABORAZIONI PER LE POSTE DIURANO SEMPRE MOLTO ABBIAMO DE-
063200000725     C* CISO DI FARLE GIRARE SU ALTRA CODA.
063300000725     C                   MOVEL     'M'           VAR
063400000731     C                   MOVEL     'M'           EMME
063500021014     C                   Z-ADD     48            LUNG             15 5
063600960202     C                   MOVEL     FGVLNA        LNA2
063700960202     C*
063800960202     C  N50              EXSR      FVIAGG
063900960123     C* CHIUSURA FINALE FILES
064000000428     C   89              CLOSE     FNFWA00T
064100000428     C   94              CLOSE     FNFVA01T
064200960123     C*
064300960123   X1C                   ELSE
064400960123     C* R I C H I A M A T O   D A   C H I U S U R A   F.V I A G G I O
064500960123     C*
064600000619     C* IGNORO SE STO CHIUDENDO FV 99999
064700000619   2AC     FGV2          IFNE      99999
064800960123     C*
064900921202     C*  19 ON - FOGLIO INESISTENTE
065000921202     C*
065100921202     C*          SE FOGLIO GIA' CHIUSO ESCO
065200921202     C  N19FGVFCF        IFNE      ' '
065300921202     C                   SETON                                        19
065400921202     C                   ELSE
065500921202     C                   MOVEL     FGVLNA        LNA2
065600921202     C                   END
065700920526     C**
065800940505     C   19              GOTO      FINE
065900950109     C* DATA CON CUI AGGIORNO LE BOLLE
066000950109     C                   Z-ADD     FGVDFV        WDTP
066100130527     c*
066200130527     c* Memorizzo anno e mese foglio
066300130705     c                   z-add     fgvdfv        w0040             4 0
066400130705     c                   movel     w0040         kmmm              2 0
066500130527     c                   movel     fgvdfv        kaaa4             4 0
066600900705     C*
066700920324     C* ALLOCO MEMBRI DI TRASMISSIONE PER IL FOGLIO VIAGGIO
066800130530    2C     FGVTTR        IFNE      'I'
066900130530     C     §TVDEF        ANDNE     'S'
067000130530     c                   seton                                        09
067100130530     c                   endif
067200130530     c
067300920324     C                   EXSR      ALLOCO
067400900911     C* ELABORAZIONE BOLLE
067500950915     C  N50              EXSR      ELAB
067600130530     c
067700000619  X2AC                   ELSE
067800000619     C* ELABORO  B O L L E    P A R T E N Z E  CON F.V. 99999
067900000619     C*  (SE CE N'E')
068000021014     C                   Z-ADD     48            LUNG
068100021014     C                   MOVE      ')'           VAR
068200021014     C                   MOVE      '))'          VAR2              9
068300000725     C                   MOVEL     'M'           VAR
068400000725     C                   MOVEL     'M'           VAR2
068500000731     C                   MOVEL     'M'           EMME
068600000621     C                   Z-ADD     1             WNPG
068700000619     C                   Z-ADD     99999         COMNFV            5 0
068800130527     C     K02BLP77_9    setll     FNBLP000                               22
068900000619     C   22              EXSR      TRAS99
069000000619     C*
069100000619     C* CHIUSURA FINALE FILE DELLE BOLLE
069200000619     C     *IN99         IFEQ      *ON
069300000619     C                   CLOSE     FNBLPE0T
069400000619     C                   ENDIF
069500000619   2AC                   ENDIF
069600920522     C*
069700960123    1C                   ENDIF
069800960202     C** AGGIORNO FOGLIO VIAGGIO
069900000619     C     FGV2          IFNE      99999
070000000619     C     *IN50         ANDEQ     *OFF
070100020807     C     K02FGV01      CHAIN     FNFGV01L                           1915
070200960202     C     *IN15         IFEQ      *OFF
070300960202     C     *IN19         ANDEQ     *OFF
070400960202     C                   MOVEL     'S'           FGVFCF
070500960202     C                   MOVEL     'T'           FGVFTR
070600960202     C                   MOVEL     DATEU         FGVDTR
070700960202     C                   UPDATE    FNFGV000
070800960202     C                   ENDIF
070900960202     C                   ENDIF
071000960123     C*
071100940217     C* LANCIO PROGRAMMI
071200930930     C                   MOVEL     *BLANKS       WLCK
071300971106     C                   MOVEL     PARAM5        KPJBU
071400930929     C*
071500930929     C* LANCIO PGM CHE PREPARA FILE DA INVIARE
071600950123     C                   CALL      'FNLS93R'
071700950123     C                   PARM                    KPJBA
071800971106     C                   MOVEL     KPJBU         PARAM5
071900930930     C*
072000930930     C* SE FILE ALLOCATI E NO TRASMISSIONE LI RISOTTOMETTO
072100931126     C     WLCK          IFNE      ' '
072200950109     C                   MOVEL     'LS93'        KCOAZ
072300931203     C                   CALL      'BCH10'
072400931203     C                   PARM                    KPJBA
072500931126     C                   ENDIF
072600931214     C*
072700021014     c                   Move      FgvLnp        MbrFil3
072800021015     c                   Movel     MbrFil4       LEGFIL
072900021015     c                   Movel     'F'           Legfil
073000931214     C                   MOVE      PARAM5        KPJBU
073100950919     C*
073200950919     C*
073300950919     C* CI SONO SOLO SPUNTE --> SOTTOMETTO SOLO IL PGM DELLE SPUNTE
073400960806     C*  SOLO SE NON E' UN FOGLIO DI DEFLUENZA
073500950919     C     *IN92         IFEQ      *ON
073600971106     C     *IN97         ANDEQ     *OFF
073700960806     C     §TVDEF        ANDNE     'S'
073800950919     C                   MOVEL     'LR15'        KCOAZ
073900950919     C                   CALL      'BCH10'
074000950919     C                   PARM                    KPJBA
074100950919     C                   ENDIF
074200040617      * sottometto la chiusura degli orm abbinati alle bolle in sospensione
074300040617      * se non è un foglio di defluenza
074400061108     c                   If        §tvdef <> 'S'  and fgv2<>99999
074500040617     c                   Clear                   Param6
074600040617     c                   Eval      pa6fgs = simfel
074700040617     c                   Eval      pa6nfv = fgv2
074800040617     c                   Eval      kpjbu = param6
074900040617     c                   Eval      kcoaz = 'OR19'
075000040617     c                   Call      'BCH10'
075100040617     c                   Parm                    kpjba
075200040617     c                   EndIf
075300150703     c* invio email per bolle in sospensione da oltre xx giorni
075400150703     c                   if        s_bspk(1)>0 or s_bspl(1)>0
075500150703     c                   exsr      INV_email
075600150703     c                   endif
075700950919     C*
075800130524     C                   MOVEL     FNLSU2DS      KPJBU
075900940415     C*
076000000000     C     FINE          TAG
076100960124     C                   MOVEL     'C'           SA2TLA
076200960124     C                   CALL      'FNLSA2R3'
076300960124     C                   PARM                    SA2TLA
076400960124     C                   PARM                    SA2TRC
076500960124     C                   PARM                    SA2DTS
076600970124     C                   PARM                    WWLNA             3 0
076700970424     C*
076800960124     C                   CALL      'FNLSA2R4'
076900960124     C                   PARM                    SA2TLA            1
077000960124     C                   PARM                    SA2LNP
077100960124     C                   PARM                    SA2NFV
077200960124     C                   PARM                    SA2RTP
077300970424     C*
077400960416     C                   MOVEL     'C'           D33TLA
077500960416     C                   CALL      'FNLR33R'
077600960416     C                   PARM                    DSLR33
077700970424     C*
077800970424     C                   CLEAR                   DSLV53
077900970424     C                   MOVEL     'C'           D53TLA
078000970424     C                   CALL      'FNLV53R'
078100970424     C                   PARM                    DSLV53
078200970509     C*
078300970509     C     WFC44         IFEQ      'S'
078400070221     C*                  MOVEL     'C'           FLG
078500070221     C                   MOVEL     'C'           dls45flg
078600970509     C*
078700070221     C                   CALL      'FNLS45R'
078800070221     c                   parm                    fnls45ds
078900070222     c                   parm                    dsbrv
079000970509     C                   ENDIF
079100970516     C*
079200970516     C                   MOVEL     'C'           D55TLA
079300970516     C                   CALL      'FNLV55R'
079400031113     C                   PARM                    DSLV55
079500031113     c*
079600031113     C                   MOVEL     'C'           I54TLA
079700031113     C                   CALL      'FNLV54R'
079800031113     C                   PARM                    fnlv54ds
079900900529      *
080000060714     c                   time                    worad
080100060714     c     worad         subdur    worap         minuti:*MN
080200060714     c
080300111019     c* Se lavoro dura più di 5 minuti creo joblog ma non invio messaggio
080400111019     c*  a sede. Nel caso ci siano delle cose strane si prova a cercare se c'e'
080500070827    3c                   if        minuti>5
080600060714     C                   Z-ADD     42            LUNG             15 5
080700060714     C                   MOVEA     CMjob(1)      COMMAN
080800060714     C                   CALL      'QCMDEXC'
080900060714     C                   PARM                    COMMAN
081000060714     C                   PARM                    LUNG
081100060714     c                   endif
081200060714     c
081300000000     C                   SETON                                        LR
081400960123     C*---------------------------------------------------------------
081500960123     C* APERTURA FILE
081600960123     C*---------------------------------------------------------------
081700960123     C     OPENF         BEGSR
081800960123     C                   OPEN      FNAGB01L
081900990517     C                   OPEN      FNANM02L
082000151118     C                   OPEN      FNANM01L
082100960123     C                   OPEN      FNFVD01L
082200960123     C                   OPEN      FNCLS01L
082300140618     C                   OPEN      FNBRV05L
082400110907     C                   OPEN      FNBRV07L
082500130527     C                   OPEN      FNBRV09L
082600960123     C                   OPEN      FNBLP01L
082700060220     C                   OPEN      Fiar401L
082800960123     C                   OPEN      FNBLT01L
082900960123     C                   OPEN      FNBLT27L
083000020605     C                   OPEN      FNBTP11L
083100020606     C                   OPEN      FNBTT11L
083200020606     C                   OPEN      FNBTT37L
083300070423     C                   OPEN      FNBLP46L
083400010309     C                   OPEN      FNORM01L
083500070711     c                   open      fnorg01l
083600010309     C                   OPEN      FNORF00F
083700051207     C                   OPEN      FNORV00F
083800030107     c                   Open      Fiar501l
083900960123     C                   ENDSR
084000900601      *---------------------------------------------------------------
084100900531      * ELABORO TRASFERIMENTO
084200900601      *---------------------------------------------------------------
084300900531     C     ELAB          BEGSR
084400021014     C                   Z-ADD     48            LUNG
084500021014     C                   MOVE      ')'           VAR
084600000725     C                   MOVEL     'M'           VAR
084700000731     C                   MOVEL     'M'           EMME
084800950206     C                   Z-ADD     1             WNPG
084900921202     C*
085000931001     C* 09 ON - CI SONO BOLLE DA CHIUDERE
085100910906     C   09              DO
085200931001     C*
085300931001     C* ELABORO  B O L L E    P A R T E N Z E
085400900601     C                   EXSR      TRASFE
085500931001     C*
085600931021     C* CHIUSURA FINALE FILE TASSAZIONE PADRONCINI
085700011121     C                   CLOSE     fiftt01L
085800011121     C                   CLOSE     fiftd04L
085900020416     C                   CLOSE     fiftd01L
086000970930     C                   CLOSE     FNSTPR0F
086100931021     C*
086200931001     C* ELABORO  B O L L E    P A R T E N Z E  CON F.V. 99999
086300931001     C*  (SE CE N'E')
086400130531     C                   Z-ADD     99999         comNFV            5 0
086500130527     C     K02BLP77_9    setll     FNBLP000                               22
086600950323     C   22              EXSR      TRAS99
086700931001     C*
086800931001     C* CHIUSURA FINALE FILE DELLE BOLLE
086900931001     C     *IN99         IFEQ      *ON
087000950109     C                   CLOSE     FNBLPE0T
087100931001     C                   ENDIF
087200910906     C                   END
087300130530     c
087400130530     C* NON FACCIO L'AUTOGENERAZIONE SPUNTE SE :
087500130530     C*  - FV CON TIPO TRAINO INOLTRO
087600130530     C*  - FV DI DEFLUENZA
087700130530    2C     FGVTTR        IFNE      'I'
087800130530     C     §TVDEF        ANDNE     'S'
087900130530     C*
088000130530     C* NFV1 > 0   ====>  C H I U S U R A    D A    I. M. P.
088100130530    3C     NFV1          IFGT      0
088200130530     C                   EXSR      ABBIN
088300130530     C*
088400130530     C     K02FGV01      CHAIN(N)  FNFGV01L                           19
088500130530   X3C                   ELSE
088600130530     C* SE CHIUSURA NON DA IMP VERIFICO COMUNQUE PER LE SPUNTE PARTENZA
088700130530     C*  ESISTENTI IL PESO E/O VOLUME
088800130530     C                   EXSR      SPUPAR
088900130530    3C                   ENDIF
089000130530     C*
089100130530    2C                   ENDIF
089200931001     C*
089300931001     C* ELABORO  S P U N T E    P A R T E N Z E
089400910909     C                   EXSR      SPUNTE
089500931001     C*
089600931021     C* ELABORO  F O G L I    V I A G G I O    P A R T E N Z E
089700911004     C                   EXSR      FVIAGG
089800931001     C*
089900931001     C* CHIUSURA FINALE FILE
090000931001     C     *IN98         IFEQ      *ON
090100950109     C                   CLOSE     FNBLPT0T
090200931001     C                   ENDIF
090300931001     C     *IN96         IFEQ      *ON
090400000111     C                   CLOSE     FISPP00T
090500931001     C                   ENDIF
090600931001     C     *IN94         IFEQ      *ON
090700951116     C                   CLOSE     FNFVA01T
090800931001     C                   ENDIF
090900000428     C     *IN89         IFEQ      *ON
091000000428     C                   CLOSE     FNFWA00T
091100000428     C                   ENDIF
091200911001     C**
091300900531     C                   ENDSR
091400900601     C**-----------------------------------------------*
091500900601     C** ESEGUE CALL QCAEXEC SUL MEMBRO DELLE BOLLE    *
091600900601     C**-----------------------------------------------*
091700900601     C     CAEX          BEGSR
091800900601     C** FILIALE DI COMPETENZA PER APERTURA MEMBRO
091900910905     C** CHIUSURA FILE SOLO DA SECONDO GIRO IN POI
092000931001     C     *IN99         IFEQ      *ON
092100950109     C                   CLOSE     FNBLPE0T
092200931001     C                   ELSE
092300931001     C                   SETON                                        99
092400931001     C                   ENDIF
092500910905     C** OVRDBF
092600950109     C                   MOVE      VAR           CMD1
092700910926     C                   MOVEL     *BLANKS       COMMAN           80
092800900601     C                   MOVEA     CMD1(1)       COMMAN
092900900601     C                   CALL      'QCMDEXC'
093000900601     C                   PARM                    COMMAN
093100900601     C                   PARM                    LUNG
093200910905     C*
093300910905     C* APERTURA FILE
093400950109     C                   OPEN      FNBLPE0T
093500900601     C                   ENDSR
093600910905     C**-----------------------------------------------*
093700910905     C** ESEGUE CALL QCAEXEC SUL MEMBRO DELLE BOLLE TRANSITO
093800910905     C**-----------------------------------------------*
093900910905     C     CAEX08        BEGSR
094000910905     C** FILIALE DI COMPETENZA PER APERTURA MEMBRO
094100910905     C** CHIUSURA FILE SOLO DA SECONDO GIRO IN POI
094200950109     C   98              CLOSE     FNBLPT0T
094300910905     C  N98              SETON                                        98
094400910905     C** OVRDBF
094500950109     C                   MOVE      VAR           CM19
094600910905     C                   MOVEL     *BLANKS       COMMAN
094700910905     C                   MOVEA     CM19(1)       COMMAN
094800910905     C                   CALL      'QCMDEXC'
094900910905     C                   PARM                    COMMAN
095000910905     C                   PARM                    LUNG
095100910905     C*
095200910905     C* APERTURA FILE
095300950109     C                   OPEN      FNBLPT0T
095400910905     C                   ENDSR
095500910909     C**-----------------------------------------------*
095600910909     C** ESEGUE CALL QCAEXEC SUL MEMBRO SPEDIZIONE BOLLE
095700910909     C**-----------------------------------------------*
095800910909     C     CAEXSP        BEGSR
095900910909     C* CHIUDO MEMBRO
096000000111     C   96              CLOSE     FISPP00T
096100920608     C  N96              SETON                                        96
096200910909     C*
096300910909     C** OVRDBF
096400000111     C                   MOVE      VAR           CM32
096500910909     C                   MOVEL     *BLANKS       COMMAN
096600920608     C                   MOVEA     CM32(1)       COMMAN
096700910909     C                   CALL      'QCMDEXC'
096800910909     C                   PARM                    COMMAN
096900910909     C                   PARM                    LUNG
097000910909     C*
097100910909     C* APERTURA FILE
097200000111     C                   OPEN      FISPP00T
097300910909     C*
097400910909     C                   ENDSR
097500910910     C**-----------------------------------------------*
097600910910     C** ESEGUE CALL QCAEXEC SUL MEMBRO FOGLI VIAGGIO
097700910910     C**-----------------------------------------------*
097800910910     C     CAEXFV        BEGSR
097900910910     C*
098000960123     C** F N F V A 0 1 T
098100951116     C   94              CLOSE     FNFVA01T
098200921202     C  N94              SETON                                        94
098300910910     C*
098400910910     C** OVRDBF
098500951116     C                   MOVE      VAR           CM38
098600921202     C                   MOVEL     *BLANKS       COMMAN
098700921202     C                   MOVEA     CM38(1)       COMMAN
098800910910     C                   CALL      'QCMDEXC'
098900910910     C                   PARM                    COMMAN
099000910910     C                   PARM                    LUNG
099100910910     C*
099200910910     C* APERTURA FILE
099300951116     C                   OPEN      FNFVA01T
099400000428     C**
099500000428     C** F N F W A 0 0 T
099600000428     C   89              CLOSE     FNFWA00T
099700000428     C  N89              SETON                                        89
099800000428     C*
099900000428     C** OVRDBF
100000000428     C                   MOVE      VAR           CM37
100100000428     C                   MOVEL     *BLANKS       COMMAN
100200000428     C                   MOVEA     CM37(1)       COMMAN
100300000428     C                   CALL      'QCMDEXC'
100400000428     C                   PARM                    COMMAN
100500000428     C                   PARM                    LUNG
100600000428     C*
100700000428     C* APERTURA FILE
100800000428     C                   OPEN      FNFWA00T
100900060707     c
101000910910     C                   ENDSR
101100900601     C**-----------------------------------------------*
101200900601     C** TRASFERISCO BOLLE NELLO STORICO E IN FILE TRASMISSIONE
101300900601     C**-----------------------------------------------*
101400000320     C     TRASFE        BEGSR
101500950323     C                   CLEAR                   SAVCBO
101600130530     C                   CLEAR                   wBORtran
101700130527     C                   CLEAR                   wFLP              3 0
101800910906     C                   Z-ADD     0             SAVLNA            3 0
101900150605     c                   clear                   sav_clibo
102000150605     c                   clear                   sav_lnp
102100150608     c                   clear                   w_SiBsp
102200950109     C                   Z-ADD     1             WNPG
102300130531     C                   Z-ADD     1             I                 3 0
102400130531     c                   clear                   kfgv
102500931027     C*
102600011121     C                   OPEN      fiftt01L
102700011121     C                   OPEN      fiftd04L
102800020416     C                   OPEN      fiftd01L
102900970930     C                   OPEN      FNSTPR0F
103000931021     C*
103100130531     C* lettura bolle per linea di arrivo con numero foglio = zero
103200130531    0C     FFV(I)        DOWGT     *ZEROS
103300130531     C                   MOVEL     FFV(I)        SAVFFV
103400130531     C*
103500130531     C     KBLP77        setll     FNBLP000
103600130531     C     KBLP77        reade     FNBLP000                               22
103700910905     C*
103800130531    1C     *IN22         DOWEQ     '0'
103900950116     C* ELABORO DOLO LE BOLLE NON ANCORA TRASMESSE
104000950116     C     BLPDT2        IFEQ      0
104100950115     C* CONTROLLO IL TIPO BOLLA
104200950115     C                   EXSR      CTABEL
104300910909     C**
104400910909     C** L I N E A   A R R I V O   C A M B I A T A
104500130531    2C     SAVLNA        IFNE      BLPLNA
104600130702     C                   SETOFF                                       01
104700920505     C*
104800130530     c* In caso di parzializz. cod cliente devo borderizare solo
104900130530     c*  linea di arrivo = SIMFEL + L6
105000151126     c                   z-add     1             Indx              3 0
105100151126     c**                 if        pa2ccm>*zeros
105200151126     c**                 eval      Indx=%lookup(blplna:l6)
105300151126     c**                 if        indx=0
105400151126     C**   Kblp77        setgt     FNBLP000
105500151126     c**                 endif
105600151126     c**                 endif
105700130530     c
105800130530     C                   CLEAR                   dslv55
105900140403     c                   z-add     blptfp        d55lnp
106000130527     C                   MOVEL     BLPLNA        d55LIN
106100130527     C                   MOVEL     FGVDFV        d55DRF
106200130527     C                   CALL      'FNLV55R'
106300130530     C                   PARM                    dslv55
106400130527     c
106500130527    1C     d55err        IFEQ      *BLANKS
106600130527     c                   z-add     d55tfp        s_o54tfp
106700130527     c                   z-add     d55tfa        s_o54tfa
106800040630     c* se lna è un secondo livello in arrivo potrebbe succedere che il
106900040630     c* p.o. di scarico sia = alla lna (f.v. diretto ad un secondo liv.)
107000040630     c* (In questo caso devo inviare la bolla al ter-part della lna.
107100040630     c* Infatti anche in fviagg e spunte i dati vengono trasmessi al ter-par
107200040630     c* del p.o. di scarico (in qesto caso p.o. di scarico=lna)
107300040630     c* Se non si facesse in questo modo le bolle andrebbero via in un membr
107400040630     c* diverso rispetto alle spunte e al f.viaggio e in più il pgm fnls93
107500040630     c* non trasmetterebbe il membro delle bolle giusto)
107600130527    2c     blplna        ifne      d55tfa
107700040630     c                   clear                   wlnadir           1
107800040630     C                   Z-ADD     1             B                 3 0
107900040630     C                   MOVEL     blplna        COMPRO            3
108000040630     C     COMPRO        LOOKUP    FFV(B)                                 20
108100040630     C* NON TROVATA --> METTO LINEA ARRIVO STESSA
108200070913     c                   if        (*in20 and flp(B)=compro) or not *in20
108300040630     c                   movel     'D'           wlnadir
108400040630     c                   endif
108500130527    2c                   endif
108600040630     c*
108700130527     c* se secondo livello in arrivo
108800031219     c* invio la bolla al terpar del ter-arr della lna
108900130527    2c     blplna        ifne      d55tfa
109000040630     c     wlnadir       andeq     *blanks
109100130530     c                   clear                   dslv55
109200130527     C                   MOVEL     'P'           d55TPT
109300130527     C                   MOVEL     s_o54tfa      d55LIN
109400130527     C                   MOVEL     FGVDFV        d55DRF
109500130527     C                   CALL      'FNLV55R'
109600130530     C                   PARM                    dslv55
109700130527    3c     d55err        ifeq      *blanks
109800031219     c                   seton                                        01
109900130527     c                   movel     d55tfp        varx
110000031219    3c                   endif
110100130527     c
110200031219   x2c                   else
110300130527     c* se primo liv. in arrivo  o 2°liv.in arr. Diretto
110400031219     c* invio al terminal di partenza di lna
110500031219     c                   seton                                        01
110600130527     c                   movel     s_o54tfp      varx
110700031219    2c                   endif
110800031112    1c                   endif
110900031110     c
111000130527    1c     *in01         ifeq      *on
111100971104     C                   EXSR      CAEX
111200130527     c
111300130527     C* SE foglio locale accendo indicatore 97
111400130527     c     fgvlnp        ifeq      s_o54tfa
111500971106     C                   SETON                                        97
111600971106     C                   END
111700130527    1C                   END
111800130527     c*
111900130527     c* Controllo se c'e' transito
112000130527     C                   exsr      CtrFLP
112100000321     C*
112200130702     C* CONTROLLO SE C'E' TRANSITO
112300971104     C*
112400130530     c                   if        wBORtran='S'
112500130703     C                   MOVEL     wflp          VAR2X
112600130703     C                   MOVEL     wflp          VARX
112700000428     C                   EXSR      TRANSI
112800000428     C                   EXSR      CAEX08
112900130527     C                   ENDif
113000910909     C*
113100910909     C                   MOVEL     BLPLNA        SAVLNA
113200130531    2C                   ENDif
113300150603    2c                   if        blpccm>0
113400150603     c                   eval      w_clibo=blpccm
113500150603     c                   else
113600150603     c                   eval      w_clibo=blpksc
113700150603    2c                   endif
113800150603     c* Lettura file FIBSP a cambio di lnp o codice cliente
113900150603     c* Solo se non è chiusura Barcode
114000150603     c                   if        pa2barcode=' ' and
114100151215     c                             w_filbsps='S'  and
114200150603     c                             (blplnp<>sav_lnp or w_clibo<>sav_clibo)
114300150603
114400150603     c                   exsr      sr_fibsp
114500150603
114600150603     c                   eval      sav_lnp=blplnp
114700150603     c                   eval      sav_clibo=w_clibo
114800150603     c                   endif
114900130527     c
115000130527     c* Eseguo routine di borderizzazione e/o messa in sospensione
115100130527     c*  automatica
115200130531   2ac                   if        Indx>0
115300130527     c                   exsr      Borderiz
115400130527     c
115500130527     c                   if        wescludi='S'
115600150605     c                   unlock    fnblp77l
115700150603     c*
115800150603     c* richiamo pgm per mettere in sospensione la bolla
115900150603     c* di cliente con colli che partono da diverse filiali se non presenti
116000150603     c* spunte
116100170327     c*****              if        w_SiBsp='S'  and w_Sosp='S'
116200170327     c                   if        w_Sosp='S'
116300150603     c                   clear                   fnlsu3ds
116400150605     c                   eval      ilsu3aas=%editc(blpaas:'X')
116500150605     c                   eval      ilsu3lnp=%editc(blplnp:'X')
116600150605     c                   eval      ilsu3nrs=%editc(blpnrs:'X')
116700150605     c                   eval      ilsu3nsp=%editc(blpnsp:'X')
116800150610     c                   eval      ilsu3dsp_B=%editc(fgvdfv:'X')
116900150603     c                   eval      ilsu3nos='S'
117000150603     c                   movel     fnlsu3ds      kpjbu
117100150603     c                   call      'FNLSU3R'
117200150603     c                   parm                    kpjbA
117300150703     c                   movel     kpjbu         fnlsu3ds
117400150703     c* Se Bolla in sospensione oltre xx giorni memorizzo in schiera per invio
117500150703     c* mail
117600150703     c                   if        olsu3err=*blanks  and
117700150703     c                             olsu3sosp='1'
117800150703     c                   exsr      sr_MemoEmail
117900150703     c                   endif
118000150603     c                   endif
118100130610     c
118200130531   2bc                   else
118300130527     c*
118400130527     c* Memorizzo in bolla fil transito se c'e'
118500130530     c                   if        wBORtran='S'
118600130527     c                   movel     wflp          blpflp
118700130527     c                   else
118800130527     c                   clear                   blpflp
118900130527     c                   endif
119000130527     c                   movel     s_o54tfa      blptfa
119100130527     c                   movel     s_o54tfa      blpfea
119200130527     C                   Z-ADD     FGV2          BLPNFV
119300130527     C                   Z-ADD     fgvdfv        BLPDBR
119400910905     C**
119500950109     C** DETTAGLIO SEGNACOLLI             F N B L T 0 0 F
119600950109     C**
119700950118     C*  SE IN BOLLA CTM= AUTOSPUNTA LA GENERO --> 04 ON
119800910911     C                   SETOFF                                       04
119900130527     C                   CLEAR                   WBLP01            1
120000950118     C     BLPCTM        LOOKUP    CTM                                    04
120100910911     C*
120200950109     C     KBLP2         SETLL     FNBLT000
120300950109     C     KBLP2         READE     FNBLT000                               26
120400910905     C*
120500910905     C     *IN26         DOWEQ     '0'
120600130527     C                   CLEAR                   WBRV1             1
120700130527     C                   CLEAR                   WBRV5             1
120800130527     C                   CLEAR                   dsbrvw5
120900130527     C*
121000911119     C* SE AUTOSPUNTA LA CREO
121100950109     C     *IN04         IFEQ      *ON
121200140618     C     KBLT2         SETLL     FNBRV05                                19
121300950109     C  N19              EXSR      CRTSPU
121400950109     C                   ENDIF
121500130527     c*
121600130527     c* Controllo le anomalie 110 105 se sono da chiudere
121700130527     c                   exsr      Contr_anom
121800060220     c
121900950109     C* FLAGGO LA TRASMISSIONE
122000950109     C                   MOVEL     'T'           BLTFTR
122100950109     C                   Z-ADD     DATEU         BLTDTR
122200910905     C*
122300950109     C                   UPDATE    FNBLT000
122400130527     C*
122500130527     C* SE TROVATA SPUNTA AGGIORNO LA DATA ENTRATA
122600130527     C     WBRV5         IFEQ      'S'
122700130701     c                   if        wblp01=' '
122800130527     C                   update    FNBLP000
122900130527     C                   MOVEL     'S'           WBLP01
123000130701     c                   endif
123100130527     c*
123200130527     c                   eval      dsbrv =dsbrvw5
123300130527     C                   EXSR      call44
123400130527     C                   ENDIF
123500910905     C*
123600950109     C     KBLP2         READE     FNBLT000                               26
123700910905     C                   END
123800130527     c
123900130527     c                   if        wblp01='S'
124000130527     c     kblp2         chain     fnblp01l
124100130911
124200130911     c* se ho trovato una spunta partenza scrivo AGB x aggiornare le date
124300130911     C* SCRIVO IL FILE FNAGB01L PER AGGIORNARE DATE
124400130911     C                   MOVEL     'P'           AGBTBO
124500130911     C                   MOVEL     BLPAAS        AGBAAS
124600130911     C                   MOVEL     BLPLNP        AGBLNP
124700130911     C                   MOVEL     BLPNRS        AGBNRS
124800130911     C                   MOVEL     BLPNSP        AGBNSP
124900130911     C                   MOVEL     'DT'          AGBAGB
125000130911     C                   WRITE     FNAGB000                             39
125100130527     c                   endif
125200950109     C*
125300060220     C* FLAGGO FIAR500F
125400950109     C                   EXSR      WRITEF
125500910905     C**
125600950109     C**  BOLLA                           F N B L P 0 0 F
125700910905     C**
125800060220     C* IL FLAG DDT LO METTO IN BL2SV1
125900060220     C                   MOVEL     BLPFST        BL2SV1
126000060220     C* IL FLAG sigla operatore video lo metto in bl2cei
126100000619     C                   MOVE      BLPSOP        BL2CEI
126200060220     c
126300950109     C                   Z-ADD     DATEU         BLPDT3
126400921026     C                   MOVEL     'S'           BLPFT3
126500950109     C                   Z-ADD     DATEU         BLPDT2
126600950109     C                   MOVEL     'T'           BLPFT2
126700950109     C                   Z-ADD     DATEU         BLPDT1
126800950109     C                   MOVEL     'T'           BLPFT1
126900911119     C*
127000911119     C* IN QUELLO PER SEDE E FIL.ARR SE ASSEGNATO 9999 METTO LA L.ARR
127100950109     C                   Z-ADD     BLPKSC        W0070             7 0
127200911119     C                   MOVE      BLPKSC        COMKSC            4 0
127300911119     C     COMKSC        IFEQ      9999
127400911119     C                   MOVEL     BLPLNA        BLPKSC
127500911119     C                   END
127600911119     C*
127700950109     C   01              WRITE     FNBLPE00
127800950113     C* SOLO PER TRANSITO: IMPOSTO IL VOLUME BOLLETTATO CON QUELLO
127900950113     C*  DA FATTURARE
128000130702     C     wbortran      IFEQ      'S'
128100950113     C                   Z-ADD     BLPVLB        W0053             5 3
128200950113     C                   MOVEL     BLPFVB        W001A             1
128300950113     C*
128400950113     C                   Z-ADD     BLPVLF        BLPVLB
128500950113     C                   MOVEL     BLPFVF        BLPFVB
128600950113     C*
128700950113     C                   WRITE     FNBLPT00
128800950113     C*
128900950113     C                   Z-ADD     W0053         BLPVLB
129000950113     C                   MOVEL     W001A         BLPFVB
129100021127     c
129200950113     C                   ENDIF
129300950412     C* MANDO IN SEDE SOLO E' PREVISTA LA TRASMISISONE DAL CODICE BOLLA
129400950412     C     §3ABSD        IFEQ      ' '
129500070423     C                   WRITE     FNBLP046                             90
129600950412     C                   ELSE
129700950412     C                   MOVEL     'N'           BLPFT2
129800950412     C                   ENDIF
129900911119     C*
130000950109     C                   Z-ADD     W0070         BLPKSC
130100000320     C**
130200130527     c                   if        wblp01='S'
130300130527     C                   UPDATE    FNBLP001
130400130527     c                   else
130500950109     C                   UPDATE    FNBLP000
130600130527     c                   endif
130700010309     c*
130800010309     c* AGGIORNO E SFLAGGO ARCHIVI O.R.M.
130900060220     c* Dentro a questa routine devo chainare fIAR4 tipo record "M"
131000010309     c                   EXSR      AGGORM
131100931020     C*
131200011121     C* CREO FILE TASSAZIONE PADRONCINI fiftt00F E fiftd00F
131300931027     C                   EXSR      CRTTAS
131400151118     c* chiudo anomalia 20 se lnp/cliente con sospensione bolle in
131500151118     c* chiusura f.v.p.
131600151118     c                   if        w_sibsp='S'
131700151118     C                   MOVEL     020           comcaa
131800151118     c     kanm01        chain     fnanm01l
131900151118     c                   if        %found(fnanm01l)
132000151118     C                   CLEAR                   DSLR33
132100151118     C                   MOVE      WDTP          D33DCH
132200151118     C                   MOVEL     '1'           D33FSC
132300151118     C                   MOVEL     '  '          D33CCH
132400151118     C                   MOVE      ANMNR1        D33NRR
132500151118     C                   CALL      'FNLR33R'
132600151118     C                   PARM                    DSLR33
132700151118     c                   endif
132800151118     c                   endif
132900950116     C                   ENDIF
133000130530     c
133100130531   2aC                   endif
133200130531   2bC                   endif
133300910906     C*
133400130531     C     Kblp77        reade     FNBLP000                               22
133500130531    1C                   ENDDO
133600130531     c
133700130531     c                   add       1             I
133800130531     c                   enddo
133900900601     C*
134000011121     C* CREO fiftt00F PER ULTIMA LETTURA
134100931203     C                   EXSR      CRTFTT
134200931203     C*
134300900601     C                   ENDSR
134400130527     C**************************************************************************
134500130527     C*    CONTROLLO ESISTENZA FILIALE DI TRANSITO
134600130527     C**************************************************************************
134700130527     C     CTRFLP        BEGSR
134800130530     c                   clear                   wBORtran          1
134900130527     C*
135000130527     C* CONTROLLO SE C'E' TRANSITO
135100141029     C******             SETOFF                                       05
135200130527     C*
135300130531     C     flp(I)        IFgt      *zeros
135400130531     c                   movel     flp(I)        WFLP              3 0
135500130527     c
135600130527     C* C'E' TRANSITO SE :
135700130531     C* 1) FLP NON E' = ALLA LINEA DI ARRIVO
135800130531     C* 2) FLP NON E' = AL TERMINAL DI ARRIVO DELLA LINEA ARRIVO
135900130527     c
136000130531     C     savffv        IFNE      wFLP
136100130531     C     s_o54tfa      ANDNE     wFLP
136200130530     C                   eval      WBORtran='S'
136300130527     C                   END
136400130527     C                   END
136500130527     C*
136600130527     C                   ENDSR
136700130527     C**-------------------------------------------------------*
136800130527     C* Borderizzazione bolle e messa in sospensione automatica
136900130527     C**-------------------------------------------------------*
137000130527     C     Borderiz      BEGSR
137100130527     c                   clear                   wescludi          1
137200130610     c                   clear                   uncollo
137300150603     c                   clear                   w_sosp            1
137400130527     c
137500130527     C* SPEDIZIONE GIA' BORDERIZZATA               - PER BORDERO'
137600130527    1C     BLPDBR        IFNE      0
137700130527     c                   eval      WEscludi='S'
137800130530     c                   leavesr
137900130527    1C                   END
138000130527     c
138100130527     C* DATA SPEDIZIONE < = DATA BORDERO' IMMESSA  - PER BORDERO'
138200130530     c                   if        pa2barcode=' ' and
138300130530     c                             blpdsp>fgvdfv
138400130527     c                   eval      WEscludi='S'
138500130530     c                   leavesr
138600130527    1C                   END
138700130527     c
138800130527     c* Se codice cliente mittente non è quello immessa. escludo
138900130527     c                   clear                   alfaccm
139000130527    1c                   if        pa2ccm>*zeros
139100130527     c
139200130527    2c                   if        blpccm>0
139300130527     c                   movel     blpccm        alfaccm           7
139400130527     c                   else
139500130527     c                   movel     blpksc        alfaccm
139600130527    2c                   endif
139700130527     c
139800151127    2c***                if        alfaccm<>pa2ccm
139900151127     c                   if        %lookup(alfaccm:ccmb)=0
140000130527     c                   eval      WEscludi='S'
140100130530     c                   leavesr
140200130527    2C                   endif
140300130527    1C                   endif
140400130527     c
140500130527      * SE MANCA IL COD PADRONCINO LA BOLLA È UN PREPAGATO NON CONFERMATO.
140600130527      *  SE ANNO/MESE BOLLA < DI ANNO/MESE DEL BORDERO' FORZO IL CODICE
140700130527      *  AUTOTRASP. = 999, LA DATA RITIRO = DATA BOLLA, IL CODICE
140800130527      *  PRESTAZIONE = 'P' E BORDERIZZO.
140900130527      *  IN CASO CONTRARIO NON BORDERIZZO.
141000130527    1C     BLPPDR        IFEQ      0
141100130530     C                   MOVEL     BLPMGS        WMMSp
141200130530    2C                   If        BLPAAS < kaaa4 or WMMSp< kmmm
141300150930     c                             or (fgvdfv>= §DGDCPD and fgvdfv<= §DGDCPA)
141400130527     C                   Z-ADD     999           BLPPDR
141500130527     C                   movel     blplnp        BLPPDR
141600130527     C                   Z-ADD     BLPMGS        BLPDRT
141700130527     C                   MOVEL     BLPAAS        BLPDRT
141800130527     c
141900130527    3C     BLPFPP        IFEQ      *BLANKS
142000130527     C                   MOVEL     'P'           BLPFPP
142100130527    3C                   ENDIF
142200130527      *
142300130527   x2C                   ELSE
142400130527     c                   eval      WEscludi='S'
142500130530     c                   leavesr
142600130527    2C                   endif
142700130527     c*
142800130527   x1c                   else
142900130530     c
143000130530     c* Per borderizzazione BARCODE --> controllo se esiste spunta o anom 010
143100150603     c* Controllo analogo anche se per il cliente o la lnp è prevista sospension
143200150603     c* su FIBSP (cliente con colli in partenza da più filiali='S')
143300150603    2c                   if        pa2barcode= 'S'  or W_siBsp='S'
143400130530     c                   exsr      CTRNSC
143500130530     c
143600130627    3c                   if        uncollo=' '
143700130530     c                   eval      WEscludi='S'
143800150603     c                   if        w_SiBsp='S'
143900150603     c                   eval      W_sosp  ='S'
144000150603     c                   endif
144100130530     c                   leavesr
144200130610    3C                   endif
144300130627    2C                   endif
144400170327     c
144500170327     c* Se bolla legata "figlia" senza spunte, il giorno della bollettazione
144600170327     c*  la sposto in automatico in sospensione
144700170421     c* Escludo lo bolle lnp 101
144800170327     c                   if        blpdsp=blpdim and blpnrs=0 and blpdse=0
144900170421     c                             and blpdpc=0 and blpdam=0 and blplnp<>101
145000170327     c     kblp2         chain     fnlbl01l
145100170327     c                   if        %found(fnlbl01l)
145200170413     c* visto che ci sono casi "strani" , eseguo anche il controllo
145300170413     c*   dei colli e del manca record
145400170413     c                   exsr      ctrnsc
145500170413
145600170413    3c                   if        uncollo=' '
145700170327     c                   eval      WEscludi='S'
145800170327     c                   eval      W_sosp  ='S'
145900170327     c                   endif
146000170413     c
146100170413     c                   endif
146200170327     c                   endif
146300130610     c*
146400130530    1c                   endif
146500130527     c
146600130527     C                   ENDSR
146700130530     C**************************************************************************
146800130530     C*    CONTROLLO ESISTENZA SPUNTE PER LA BOLLA - borderizz.barcode
146900130530     C**************************************************************************
147000130530     C     CTRNSC        BEGSR
147100130530     C*
147200130530     C                   SETOFF                                       10
147300130530     c                   clear                   uncollo           1
147400130530     C* CONTROLLO SE CTM NON PREVEDE LA SPUNTA DEI SEGNACOLLI:
147500130530     C*  A BORDERO' LO STESSO (FLAG 5 DEL CTM)
147600170413     c* questo vale sempre: anche per la sospensione automatica
147700170413     c*  per sicurezza lo faccio anche se non dovrebbe mai accadere
147800170413     c***                if        pa2barcode= 'S'
147900130530     C     BLPCTM        LOOKUP    CTM                                    10
148000130530     c                   if        *in10
148100130530     C                   eval      uncollo='S'
148200130530     C                   leavesr
148300130530     C                   endif
148400170413     c***                endif
148500151118
148600151118     c                   if        w_sibsp='S'
148700151118     C                   MOVEL     020           comcaa
148800151118     c     kanm01        chain     fnanm01l
148900151118     c                   if        %found(fnanm01l) and anmdch=0
149000151118     C                   eval      uncollo='S'
149100151118     C                   leavesr
149200151118     c                   endif
149300151118     c                   endif
149400130530     c
149500130530     C* SI SPUNTA: CONTROLLO SE ALMENO UN SEGNACOLLO SPUNTATO C'E'
149600130530     C     KBLp2         SETLL     FNBLT01L
149700130530     C     KBLp2         READE(n)  FNBLT01L
149800130530     C*
149900130530    1C                   DOW       not %eof(fnblt01l)
150000130610     c
150100130530     C* LA FILIALE IN GESTIONE PER BORDERO' E' SIMFEL
150200150603     c                   if        pa2barcode='S'
150300130530     C                   Z-ADD     1             KNPG
150400130530     C                   Z-ADD     SIMFEL        KFGS
150500130530     C                   Z-ADD     fgv2          wnfv60
150600130530     c* BARCODE
150700130530     C     KBRVbor       SETLL     FNBRV01L                               31
150800150603     c                   else
150900150603     C     KBRV7         SETLL     FNBRV07L                               31
151000150603     c                   endif
151100130530    3c                   if        *in31
151200130530     C                   SETON                                        10
151300130530     C                   eval      uncollo='S'
151400130530     c                   leavesr
151500130530    3c                   endif
151600130530     c
151700130530     c* non trovata spunta ma presente manca record bolla in arrivo ancora
151800130530     c* aperta --> borderizzo lo stesso
151900130530    3c                   if        *in31=*off
152000130607     C                   MOVEL     010           comcaa
152100130530     c     kanm1         setll     fnanm02l
152200130530    4c                   if        %equal(fnanm02l)
152300130607     C                   eval      uncollo='S'
152400130530     c                   seton                                        10
152500130530     c                   leavesr
152600130530    4c                   endif
152700130530    3c                   endif
152800130530     c
152900130607     C     kblp2         READE(n)  FNBLT000
153000130530    1C                   ENDdo
153100130530     C*
153200130530     C                   ENDSR
153300130527     C**************************************************************************
153400130527     C*    CONTROLLO anomalia 110 105 da chiudere
153500130527     C**************************************************************************
153600130527     C     Contr_Anom    BEGSR
153700130527     C* ANOMALIA--> "NO BOLLA BORDERIZZATA" CHIUDO
153800130607     C                   MOVEL     110           comcaa
153900130527     C     KANM1         CHAIN(N)  FNANM02L                           32
154000130527     C     *IN32         IFEQ      *OFF
154100130607     C                   EXSR      call_lr33
154200130527     C                   END
154300130527     C*
154400130527     C* ANOMALIA --> "MANCA BOLLA"          CHIUDO
154500130607     C                   MOVEL     105           comcaa
154600130527     C     KANM1         CHAIN(N)  FNANM02L                           32
154700130527     C**
154800130527     C* SE C'E' GIA' CHIUSA COME ANNULLATA --> COME SE NON CI FOSSE
154900130527     C* (P.ES SE LA 105 ERA STATA ANNULLATA MA SUCCESSIVAMENTE LA
155000130527     C* SPEDIZIONE VIENE BOLLETTATA. IN QUESTO CASO IL PGM DEVE)
155100130527     C* AGGIORNARE BLTNFV)
155200130527    1C     *IN32         IFEQ      *OFF
155300130527     C     ANMDCH        ANDEQ     0
155400130527     C*
155500130527     C     *IN32         OREQ      *OFF
155600130527     C     ANMDCH        ANDGT     0
155700130527     C     ANMCCH        ANDNE     'AN'
155800130527     C**
155900130527    2C     ANMDCH        IFEQ      0
156000130527     C                   CLEAR                   WSPE
156100130527     C*
156200130527     C* CERCO LA SPUNTA ENTRATA PER AGGIORNARE LA DATA ENTRATA
156300130527     C                   MOVEL     '5'           KNPG
156400130527     C                   MOVE      *HIVAL        WVVDFV
156500130527     C     KBRV1         SETLL     FNBRV09L                           32
156600130527     C     KBRV1         READE     FNBRV09L                               32
156700130527     C*
156800130527     C     *IN32         DOWEQ     *OFF
156900130530     C                   CLEAR                   DSLV53
157000130527     C                   MOVEL     'V'           D53TFO
157100130527     c                   exsr      data_spunta
157200130527     c
157300130527     C     KBRV1         READE     FNBRV09L                               32
157400130527     C                   ENDDO
157500130527     C*
157600130527     C                   MOVEL     '1'           KNPG
157700130527     C     KBRV1         CHAIN     FNBRV09L                           32
157800130527     C     *IN32         DOWEQ     *OFF
157900130527     C                   MOVEL     'S'           WBRV1             1
158000130527     C* SE ESISTE UNA SPUNTA PARTENZA CON DATA < DELLA DATA PRIMA SPUN-
158100130527     C* TA ENTRATA --> MEMORIZZO I DATI DELLA SPUNTA PARTENZA
158200130530     C                   CLEAR                   DSLV53
158300130527     C                   MOVEL     'G'           D53TFO
158400130527     c                   exsr      data_Spunta
158500130527
158600130527     C     KBRV1         READE     FNBRV09L                               32
158700130527     C                   ENDDO
158800130527     C*
158900130527     C* TROVATA SPUNTA PARTENZA
159000130527    3C     WBRV1         IFEQ      'S'
159100130527     C* CHIAMO IL PGM DI AGGIORNAMENTO BOLLE PER SCRIVERE LA DATA ENTRA
159200130527     C* TA
159300130527     C* SOLO SE FV CHIUSO AGGIORNO LA BOLLA
159400130527     c* Es 27 05 13 : non lo faccio sempre se c'e' anomalia 105 significa
159500130527     c*  che la spunta è stata già elaborata dalla chiusura FGV
159600130527     c*  quindi il collo/bolla devono essere aggioranti a prescindere
159700130527    4C**** D53ERR        IFEQ      *BLANKS
159800130527     C**** D53FCF        ANDEQ     'S'
159900130527     C*
160000130527     C                   Z-ADD     BRVNPS        BLTNPP
160100130527     C                   Z-ADD     D53NFV        BLTNFV
160200130527     C                   Z-ADD     D53DFV        BLTDFV
160300130527     C*
160400130527     C* AGGIORNO LA DATA PARTENZA PRIMO COLLO SULLA BOLLA
160500130527    5C     BLPDPC        IFEQ      0
160600130527     C                   Z-ADD     BLTDFV        BLPDPC
160700130527     C* SOMMO LA SPEDIZIONE DI UN COLLO "EX MANCA BOLLA"
160800130527     C                   Z-ADD     1             WSPE
160900130527    5C                   END
161000130527     C*
161100130527     C* CALCOLO PESO E VOLUME UNITARI DI UN COLLO "EX MANCA BOLLA"
161200130527     C                   EXSR      TOTFVD
161300130527     C*
161400130527     C* AGGIORNO FNSTA PER LA RIELABORAZIONE DELLE STATISTICHE
161500130527     c                   If        BltDfv <= §SdfParU
161600130527     C                   Z-ADD     BLTDFV        SA2DTS
161700130527     C                   MOVE      'P'           SA2TRC
161800130527     C                   CALL      'FNLSA2R3'
161900130527     C                   PARM                    SA2TLA
162000130527     C                   PARM                    SA2TRC
162100130527     C                   PARM                    SA2DTS
162200130527     C                   PARM                    WWLNA             3 0
162300130527     C                   MOVEL     'S'           WLSA2             1
162400130527    5C                   END
162500130527     C*
162600130527     c                   If        BltDfv <= §SdfTerU
162700130527     C                   MOVE      BLTDFV        SA2DTS
162800130527     C                   MOVE      'T'           SA2TRC
162900130527     C                   CALL      'FNLSA2R3'
163000130527     C                   PARM                    SA2TLA
163100130527     C                   PARM                    SA2TRC
163200130527     C                   PARM                    SA2DTS
163300130527     C                   PARM                    WWLNA             3 0
163400130527     C                   MOVEL     'S'           WLSA2             1
163500130527    5C                   END
163600130527     C*
163700130527     C* SE FOGLIO APERTO NON AGGIORNO LA BOLLA E MEMORIZZO IL FOGLIO
163800130527     C*  SU CUI STO BORDERIZZANDO (non dovrebbe più accadere)
163900130527   X4C****               ELSE
164000130527     C*
164100130527     C****               Z-ADD     VIDNFV        BLTNFV
164200130527    4C****               ENDIF
164300130527     C*
164400130527    3C                   ENDIF
164500130527     C* SE TROVATA SPUNTA CLEARO DATA ENTRATA E VOLUME CML PRIMA
164600130527     C* AFFINCHE' L'FNLS45R AGGIORNI QUESTI CAMPI IN BASE ALLA PRIMA
164700130527     C* SPUNTA PASSATA SUL SISTEMA
164800130527    4C     WBRV5         IFEQ      'S'
164900130527     C                   CLEAR                   BLTDSE
165000130527    5C     W5VUC         IFGT      *ZEROS
165100130527     C                   CLEAR                   BLTVUC
165200130527    5C                   END
165300130527    4C                   END
165400130527     C*
165500130607     C                   EXSR      call_lr33
165600130527    2C                   END
165700130527     C                   ELSE
165800130527   X1C*
165900130527     C                   Z-ADD     FGV2          BLTNFV
166000130527    1C                   ENDIF
166100130527     C*
166200130527     c
166300130527     C                   ENDSR
166400130527      ****************************************************************
166500130527      * Prendo data spunta
166600130527      ****************************************************************
166700130527     c     Data_spunta   BEGSR
166800130527     C* DETERMINO DATA FOGLIO
166900130527     C                   MOVE      BRVNPG        D53NPG
167000130527     C                   MOVE      BRVNFV        D53NFV
167100130527     C                   MOVE      BRVFGS        D53FGS
167200130527     C                   MOVE      SIMFEL        D53FEL
167300130527     C                   CALL      'FNLV53R'
167400130530     C                   PARM                    DSLV53
167500130527     C                   MOVE      '1'           WLV53             1
167600130527     C*
167700130527     C     D53ERR        IFEQ      *BLANKS
167800130527     C     D53DFV        iflt      WVVDFV
167900130527     c* se è presente il peso e volume nella spunta meno recente
168000130527     c*  li devo mantenere per il richiamo a fnls45r
168100130527     c
168200130527     c                   if        brvvuc=0  and w5vuc>0
168300130527     c                   eval      brvvuc=w5vuc
168400130527     c                   endif
168500130527     c                   if        brvpuc=0  and w5puc>0
168600130527     c                   eval      brvpuc=w5puc
168700130527     c                   endif
168800130527     c                   if        brvcan=' ' and w5can<>' '
168900130527     c                   eval      brvcan=w5can
169000130527     c                   endif
169100130527     c
169200130527     C                   MOVEL     'S'           WBRV5             1
169300130527     c                   movel     dsbrv         dsbrvw5
169400130527     C                   MOVE      D53DFV        WVVDFV
169500130527     c* se c'e' il peso e volume --> lo passo per il calcolo
169600130527     c                   else
169700130527     c                   if        w5vuc=0  and brvvuc>0
169800130527     c                   eval      w5vuc=brvvuc
169900130527     c                   endif
170000130527     c                   if        w5puc=0  and brvpuc>0
170100130527     c                   eval      w5puc=brvpuc
170200130527     c                   endif
170300130527     c                   if        w5can=' ' and brvcan<>' '
170400130527     c                   eval      w5can=brvcan
170500130527     c                   endif
170600130527     c
170700130527     C                   END
170800130527     C                   END
170900130527     c                   ENDSR
171000130527     C**************************************************************************
171100130527     C*    CALCOLO PESO E VOLUME UNITARI
171200130527     C**************************************************************************
171300130527     C     TOTFVD        BEGSR
171400130530     c                   exsr      CALUNI
171500130527     C*
171600130527     C                   Z-ADD     BRVNFV        WNFV5
171700130603     C                   MOVEL     '1'           WTRC
171800130530     C     KFVDbor       CHAIN     FNFVD000                           34
171900130527     C   34              CLEAR                   FNFVD000
172000130527     C***  COLLI ***
172100130527     C                   ADD       1             FVDCLA
172200130530     c                   clear                   wtran
172300130527     C*
172400130527     C***  SPEDIZIONI  ***
172500130527     C     WSPE          IFGT      0
172600130530     C                   z-add     WSPE          wspen
172700130527     C                   ENDIF
172800130530     c
172900130530     c                   exsr      RIEFVD
173000130527     C*
173100130527     C* ABBLENCO FLAG E DATA TRASMISSIONE PER INVIO IN SEDE
173200130527     C                   MOVEL     ' '           FVDFTR
173300130527     C                   Z-ADD     0             FVDDTR
173400130527     C*
173500130527     C*
173600130527     C     *IN34         IFEQ      *ON
173700130527     C                   Z-ADD     BRVNFV        FVDNFV
173800130527     C                   MOVEL     '1'           FVDTRC
173900130527     C                   MOVEL     SIMFEL        FVDLNP
174000130527     C                   MOVEL     BLPLNA        FVDLNA
174100130527     C                   WRITE     FNFVD000
174200130527     C                   ELSE
174300130527     C*
174400130527     C                   UPDATE    FNFVD000
174500130527     C                   ENDIF
174600130527     C*
174700130527     C                   ENDSR
174800950109     C**-----------------------------------------------*
174900950109     C* SCRITTURA DI ALCUNI FILE DELLA BOLLA -----------
175000950109     C**-----------------------------------------------*
175100950109     C     WRITEF        BEGSR
175200060220     c
175300030107      *  Estensione Testata Bolla    F I A R 5 0 0 F
175400030107     c     Kblp2         Setll     Fiar501l
175500030203Do  1c                   Do        *Hival
175600030107     c     Kblp2         Reade     Fiar501l
175700030107      * Fine File esco dal ciclo
175800030107     c                   If        %Eof(Fiar501l)
175900030107     c                   Leave
176000030107     c                   EndIf
176100030110      * Controllo se già inviato
176200030110     c                   If        Ar5Dt1 <> *all'9' and Ar5Dt1 > 0
176300030110     c                   Iter
176400030110     c                   EndIf
176500030107      * Flaggo per la trasmissione anche se il record non viene trasmesso
176600030107     c                   Eval      Ar5Ft1 = 'T'
176700030107     c                   Z-Add     Dateu         Ar5Dt1
176800030107     c                   Update    Fiar5000
176900030203    1c                   EndDo
177000030107
177100950109     C                   ENDSR
177200900608     C**-----------------------------------------------*
177300950109     C** TRASFERISCO BOLLE  - 99999 -
177400900608     C**-----------------------------------------------*
177500000619     C     TRAS99        BEGSR
177600030107
177700030107     c                   Open      Fiftt01l
177800910906     C*
177900130702     C                   SETOFF                                       01
178000920221     C                   Z-ADD     0             SAVLNA
178100130527     C     K02BLP77_9    reade     FNBLP000                               22
178200900608     C*
178300950323    1C     *IN22         DOWEQ     '0'
178400010109      *
178500010109      * SE MANCA IL COD PADRONCINO LA BOLLA È UN PREPAGATO NON CONFERMATO.
178600010111      *  SE ANNO/MESE BOLLA < DI ANNO/MESE DEL JOB      CONFERMO FORZANDO
178700010109      *  COD.AUTOTRASP. = 999, LA DATA RITIRO = DATA BOLLA E IL CODICE
178800010109      *  PRESTAZIONE = 'P'. IN CASO CONTRARIO NON CONFERMO.
178900010109     C                   IF        BLPPDR = *zeros
179000010109     C                   movel     BLPMGS        WMMSp             2 0
179100010111     C                   movel     DATEU         WAAJb             4 0
179200010111     C                   move      dateu         W0040             4 0
179300010111     C                   movel     W0040         WMMJb             2 0
179400010109      *
179500010111     C                   If        BLPAAS < WAAJb or WMMSp < WMMJb
179600150930     c                             or (fgvdfv>= §DGDCPD and fgvdfv<= §DGDCPA)
179700010109     C                   z-add     999           BLPPDR
179800010109     C                   z-add     BLPMGS        BLPDRT
179900010109     C                   movel     BLPAAS        BLPDRT
180000010109     C                   If        BLPFPP = *BLANKS
180100010109     C                   eval      BLPFPP = 'P'
180200010109     C                   ENDIF
180300010109     C                   ENDIF
180400010109      *
180500010109     C                   ENDIF
180600010109      *
180700010109      *  PER I PREPAGATI, GIA' CONFERMATI
180800010109   1AC     BLPPDR        IFGT      0
180900990915     C*
181000950115     C* ELABORO SOLO LE BOLLE NON ANCORA TRASMESSE
181100991028    2C     BLPDT2        IFEQ      0
181200991028     C     BLPDT1        OREQ      0
181300920221     C*
181400940107     C* CHAIN CON CODICE BOLLA
181500940107     C                   EXSR      CTABEL
181600030107      * Aggiorno Fiftt con il numero di ritiri annullati
181700030107      * bolle "FY"
181800030107     c                   If        BlpCbo = 'FY'
181900030107     c                   Exsr      CrtFtt1
182000030107     c                   EndIf
182100000619     C*
182200920221     C* INVIO ANCHE PER FILIALE
182300950412    3C     §3ABFI        IFEQ      ' '
182400960411     C* SE E' "£" INVIO SOLO SE LNP<>LNA
182500960411     C     BLPCBO        OREQ      '£ '
182600960411     C     BLPLNP        ANDNE     BLPLNA
182700920221     C*
182800920221     C** L I N E A   A R R I V O   C A M B I A T A
182900950323    4C     SAVLNA        IFNE      BLPLNA
183000031112     C**                 CLEAR                   WLOCAL
183100971105     C                   SETOFF                                       01  50
183200950109     C*
183300130531     C                   CLEAR                   dslv55
183400140403     c                   z-add     blptfp        d55lnp
183500130531     C                   Z-ADD     BLPLNA        d55LIN
183600130531     C                   Z-ADD     PARDTP        d55DRF
183700130531     C                   CALL      'FNLV55R'
183800130531     c                   PARM                    dslv55
183900130531    5C     d55err        IFEQ      *BLANKS
184000130531     c                   z-add     d55tfp        s_o54tfp
184100130531     c                   z-add     d55tfa        s_o54tfa
184200130531     c* se secondo livello in arrivo
184300031219     c* invio la bolla al terpar del ter-arr della lna
184400130531    6c     blplna        ifne      d55tfa
184500130531     c                   clear                   dslv55
184600130531     C                   MOVEL     'P'           d55TPT
184700130531     C                   MOVEL     s_o54tfa      d55LIN
184800130531     C                   MOVEL     pardtp        d55DRF
184900130531     C                   CALL      'FNLV55R'
185000130531     C                   PARM                    dslv55
185100130531    7c     d55err        ifeq      *blanks
185200031219     c                   seton                                        01
185300130531     c                   movel     d55tfp        varx
185400031219    7c                   endif
185500031219   x6c                   else
185600031219     c* se primo liv. in arrivo o ogas diversi
185700031219     c* invio al terminal di partenza di lna
185800031219     c                   seton                                        01
185900031219     c                   movel     s_o54tfp      varx
186000031219    6c                   endif
186100031112    5c                   endif
186200031112     C     N_varx        LOOKUP    FEL                                    32
186300950323    5C     *IN32         IFEQ      *OFF
186400031112     C                   MOVEL     N_varx        VAR2X
186500031112     C                   MOVEL     N_varx        NUM3
186600950115     C                   EXSR      LCKBLP
186700031112     C                   Z-ADD     1             IY                3 0
186800031112     C     000           LOOKUP    FEL(IY)                                32
186900031112     C   32              Z-ADD     N_varx        FEL(IY)
187000950323    5C                   ENDIF
187100950115     C*
187200031112     c   50              setoff                                       01
187300950115     C*
187400031112     c     *in01         ifeq      *on
187500950109     C                   EXSR      CAEX
187600971106     C* SE BOLLA LOCALE MEMORIZZO CHE CE N'è ALMENO UNA
187700130531     c     simfel        ifeq      s_o54tfa
187800971106     C                   SETON                                        97
187900031112     C**                 MOVEL     'S'           WLOCAL
188000971106     C                   END
188100950323    5C                   ENDIF
188200950115     C*
188300950109     C                   MOVEL     BLPLNA        SAVLNA
188400950323    4C                   ENDIF
188500920221     C*
188600920427     C* SE E' DA TRASMETTERE SOLO IN SEDE NON APRO MEMBRI
188700950323   X3C                   ELSE
188800950109     C*
188900950109     C* IMPOSTO I FLAG DI TRASMISSIONE
189000950109     C                   MOVEL     'N'           BLPFT1
189100950109     C                   Z-ADD     DATEU         BLPDT1
189200940415     C*
189300940415     C* SE CODICE BOLLA FRANCO RECUPERO MUOVO DATA BORDERO' IN DCM/DAM
189400060303     c* stessa cosa per cod. bolla "M"-Prepagato annullato
189500950323    4C     BLPCBO        IFEQ      'B '
189600060303    4C     BLPCBO        oreQ      'M '
189700030728      * prima però imposto la data borderò uguale alla data foglio viaggio
189800030728     c                   Z-Add     FgvDfv        BlpDbr
189900920615     C                   MOVEL     BLPDBR        BLPDCM
190000920615     C                   MOVEL     BLPDBR        BLPDAM
190100950323    4C                   ENDIF
190200950323    3C                   ENDIF
190300950115     C*
190400950115     C* SOLO SE HO ALLOCATO I MEMBRI
190500950915    3C     *IN50         IFEQ      *OFF
190600911119     C*
190700060220     C* FLAGGO FIAR500F: LO FACCIO A PREDCINDERE DALLA TRASMISSIONE DELLA BO
190800060220     C                   EXSR      WRITEF
190900060220     C
191000991029     C* IN QUELLO PER ARRIVO SE 9999 METTO LA LN ARRIVO
191100950109     C                   Z-ADD     BLPKSC        W0070
191200950915     C                   MOVE      BLPKSC        COMKSC            4 0
191300950323    4C     COMKSC        IFEQ      9999
191400911119     C                   MOVEL     BLPLNA        BLPKSC
191500950323    4C                   END
191600950323     C*
191700991028     C     BLPDT3        IFEQ      0
191800991028     C                   Z-ADD     DATEU         BLPDT3
191900991028     C                   MOVEL     'S'           BLPFT3
192000991028     C                   ENDIF
192100991028     C*
192200991028    4C     BLPDT2        IFEQ      0
192300950109     C                   Z-ADD     DATEU         BLPDT2
192400950109     C*
192500991028    5C     §3ABSD        IFEQ      ' '
192600950109     C                   MOVEL     'T'           BLPFT2
192700070423     C                   WRITE     FNBLP046                             90
192800950109     C                   ELSE
192900950109     C                   MOVEL     'N'           BLPFT2
193000991028    5C                   ENDIF
193100991028    4C                   ENDIF
193200920221     C*
193300991028    4C     BLPDT1        IFEQ      0
193400991028    5C     §3ABFI        IFEQ      ' '
193500960411     C     BLPCBO        OREQ      '£ '
193600960411     C     BLPLNP        ANDNE     BLPLNA
193700991029     C*
193800060220     C* IL FLAG DDT LO METTO IN BL2SV1
193900060220     C                   MOVEL     BLPFST        BL2SV1
194000060220     C* IL FLAG sigla operatore video lo metto in bl2cei
194100000619     C                   MOVE      BLPSOP        BL2CEI
194200991029     C* IMPOSTO I FALG DI TRASMISSIONE
194300991029     C                   MOVEL     'T'           BLPFT1
194400991029     C                   Z-ADD     DATEU         BLPDT1
194500991029     C**
194600950109     C   01              WRITE     FNBLPE00
194700991028    5C                   ENDIF
194800991028    4C                   ENDIF
194900900608     C*
195000950109     C                   MOVEL     W0070         BLPKSC
195100950323     C* IMPOSTO IL FOGLIO A 99998 --> PER DIRE CHE L'HO GIA ELABORATA
195200950323     C                   Z-ADD     99998         BLPNFV
195300950109     C                   UPDATE    FNBLP000
195400950323    3C                   ENDIF
195500950323     C*
195600950323     C* SE BOLLA GIA' TRASMESSA --> IMPOSTO SOLO IL FOGLIO A 99998
195700950323   X2C                   ELSE
195800950323     C                   Z-ADD     99998         BLPNFV
195900950323     C                   UPDATE    FNBLP000
196000950323    2C                   ENDIF
196100990915   1AC                   ENDIF
196200910906     C*
196300130527     C     K02BLP77_9    reade     FNBLP000                               22
196400950323    1C                   ENDDO
196500900608     C*
196600900608     C                   ENDSR
196700931022     C*
196800011121     C*--- SCRIVO FILE TASSAZIONE  fiftt00F/fiftd00F -----------------*
196900931019     C     CRTTAS        BEGSR
197000931203     C***
197100931203     C*  C R E A Z I O N E    F L F T T 0 0 F
197200931203     C***
197300011121     C* CONTROLLO SE ESISTE IL FILE fiftt OGNI VOLTA CHE CAMBIANO IL
197400931203     C*   CODICE PADRONCINO E LA DATA RITIRO
197500931203     C*
197600931203    1C     COMBLP        IFNE      COMSAV
197700931203     C*
197800931203     C                   EXSR      CRTFTT
197900931203     C*
198000931203     C* CONTROLLO IL PADRONCINO OGNI VOLTA CHE CAMBIA
198100950113     C     WCPR          IFNE      BLPPDR
198200931203     C* IL COD.PADRONCINO DELLA BOLLA DEVE ESISTERE NELLA SCHIERA PDR
198300040826     C                   Z-ADD     1             W                 4 0
198400140620     C                   SETOFF                                       05
198500950113     C     BLPPDR        LOOKUP    PDR(W)                                 05
198600950113     C                   Z-ADD     BLPPDR        COMPDR
198700931203     C                   ENDIF
198800931203     C*
198900950113     C                   MOVEL     COMBLP        COMSAV
199000931203    1C                   ENDIF
199100970930     C*
199200971027    1C     *IN05         IFEQ      *ON
199300931027     C* CONTROLLO SE IL TIPO BOLLA E' DA ESCLUDERE
199400931027     C                   EXSR      CTAB
199500931027     C*
199600011121     C* ALF1 = '1' ===> CREO I FILE fiftt00F E fiftd00F  SE PDR NON DIP
199700971027     C*                 CREO COMUNQUE FILE FNSTPR0F PER TRASMISSIONE
199800971027     C*                 STOP. PER I NON DIPENDENTI, IL CALCOLO STOP IN
199900971027     C*                 QUESTO MOMENTO E' SOLO PREVENTIVO. VERRA' RI-
200000971027     C*                 FATTO AL MOMENTO DELLA CONFERMA CONTEGGI
200100020528     c*                 28/05/02:Creo dati per conteggi anche per i dipenden
200200020528     c*                          ti
200300971027    2C     ALF1          IFEQ      '1'
200400931203     C* NUMERO SPEDIZIONI TOTALE
200500020528     C*  05              ADD       1             COMSET
200600020528     C                   ADD       1             COMSET
200700931202     C*
200800931019     C* IMPOSTO CAMPI CHIAVE
200900971027    3C     BLPFPP        IFEQ      'M'
201000950113     C                   Z-ADD     1000          COMNDC
201100971027   X3C                   ELSE
201200950113     C                   Z-ADD     5000          COMNDC
201300971027    3C                   ENDIF
201400950113     C                   ADD       BLPNRT        COMNDC
201500950113     C                   MOVEL     BLPRSM        COMRSC
201600950114     C                   MOVEL     BLPDRT        COMDDC
201700931019     C*
201800971027    3C     COMTB1        IFEQ      'F'
201900931202     C                   MOVEL     BLPKSC        CO2KSC
202000971027   X3C                   ELSE
202100971027    4C     BLPCCM        IFNE      0
202200931202     C                   MOVEL     BLPCCM        CO2KSC
202300971027   X4C                   ELSE
202400931202     C                   MOVEL     BLPKSC        CO2KSC
202500950801     C                   MOVE      CO2KSC        W0040             4 0
202600971027    5C     W0040         IFEQ      9999
202700950801     C                   MOVE      8888          CO2KSC
202800971027    5C                   END
202900971027    4C                   ENDIF
203000971027    3C                   ENDIF
203100931202     C*
203200931019     C*
203300931202     C***
203400970930     C*  C R E A Z I O N E    F N F T D 0 0 F
203500931202     C***
203600020325     c                   clear                   wstp
203700020325     C* ESCLUDENDO FRANCHI E ASSEGNATI VARI,
203800020325     C*   CHAINO FNCLS00F PER PRENDERE IL NUMERO DEGLI STOP
203900020325     C                   Z-ADD     CO2KSC        NUM4              4 0
204000020325    5C     NUM4          IFNE      *ALL'8'
204100020325     C     NUM4          ANDNE     *ALL'9'
204200020325     C     CO2KSC        CHAIN     FNCLS000                           33
204300020325     C  N33              z-add     CLSSTP        wstp
204400020325    5C                   ENDIF
204500970930     C*
204600020325     c     wstp          ifeq      *zeros
204700011121     C     KFTD          CHAIN     fiftd000                           32
204800020325     c  n32              z-add     ftdstp        wstp
204900020326     c                   endif
205000020326     c     wstp          ifeq      *zeros
205100020416     c* Se il numero stop da attribuire è ancora = a 0 devo cercare
205200020416     c* nella distinta il primo numero disponibile
205300020416     C     KFTD1         setgt     fiftd001
205400020416     C     KFTD1         readpe    fiftd001
205500020416     c                   if        not %eof
205600020416     c                   eval      wstp = (ftdstp+1)
205700020416     c     wstp          iflt      1000
205800020416     c                   z-add     1000          wstp
205900020416     c                   endif
206000020417   x6c                   else
206100020417     c                   z-add     1000          wstp
206200020416     c                   endif
206300020325     c                   endif
206400020325     C                   CLEAR                   fiftd000
206500931108     C*
206600020325     c                   z-add     blpaas        ftdaas
206700020325     c                   z-add     blplnp        ftdlnp
206800020325     c                   z-add     blpnrs        ftdnrs
206900020325     c                   z-add     blpnsp        ftdnsp
207000931123     C                   MOVEL     BLPCAM        FTDCAP
207100020325     C                   z-add     1             FTDSET
207200931108     C                   MOVEL     'N'           FTDFGC
207300020325     c                   z-add     wstp          ftdstp
207400931108     C* FLAG ANTEPORTO
207500030902     C** IMPOSTO SEMPRE ANCHE SE C'ERA GIA' IL FLAG ANTEPORTO
207600931108     C                   MOVEL     BLPFAP        FTDFIN
207700931108     C*
207800021108     C* VOLUME : - SE TOTALE   ("T"), SOMMO BLPVLc
207900021108     C*          - SE PARZIALE ("Z"), SOMMO BLPVLc (SE MAGGIORE)
208000021108     C*                               ALTRIMENTI SOMMO BLPVLF
208100050112     C     BLPNCR        IFEQ      BLPNCL
208200950113     C                   Z-ADD     BLPVLC        WVOL
208300940321     C*
208400971027   X4C                   ELSE
208500971027    5C     BLPVLC        IFGE      BLPVLF
208600950113     C                   Z-ADD     BLPVLC        WVOL
208700971027   X5C                   ELSE
208800950113     C                   Z-ADD     BLPVLF        WVOL
208900971027    5C                   ENDIF
209000971027    4C                   ENDIF
209100021108     C* peso   : - SE TOTALE  , SOMMO BLPpkc
209200021108     C*          - SE PARZIALE, SOMMO BLPPKC (SE MAGGIORE)
209300021108     C*                               ALTRIMENTI SOMMO BLPPKF
209400050112     C     BLPNCp        ifeq      BLPNCL
209500021108     C                   Z-ADD     BLPpkC        Wpes
209600021108     C*
209700021108   X4C                   ELSE
209800021108    5C     BLPpkC        IFGE      BLPpkF
209900021108     C                   Z-ADD     BLPpkC        Wpes
210000021108   X5C                   ELSE
210100021108     C                   Z-ADD     BLPpkF        Wpes
210200021108    5C                   ENDIF
210300021108    4C                   ENDIF
210400940321     C*
210500931202     C*
210600931202     C                   Z-ADD     BLPNCL        FTDNCL
210700021108     C                   Z-ADD     WPES          FTDPKL
210800950113     C                   Z-ADD     WVOL          FTDVLU
210900950113     C* CALCOLO I COLLI PER CUI APPLICARE ETICHETTATURA: ESCLUDO
211000050112     C*  LE BOLLE CON SERIE
211100971027    5C     BLPNRS        IFEQ      0
211200950113     C                   Z-ADD     BLPNCL        FTDNCE
211300971027    5C                   ENDIF
211400931019     C*
211500931108     C* IMPOSTO CAMPI CHIAVE
211600931019     C                   Z-ADD     COMPDR        FTDPDR
211700020325     C                   movel     COMPDR        FTDfgs
211800931019     C                   MOVEL     COMTSR        FTDTSR
211900931019     C                   Z-ADD     COMNDC        FTDNDC
212000950115     C                   Z-ADD     COMDDC        FTDDDC
212100950113     C                   MOVEL     COMRSC        FTDRSC
212200931202     C                   MOVEL     CO2KSC        FTDKSC
212300931019     C*
212400011121     C                   WRITE     fiftd000
212500931019     C*
212600970930     C***
212700970930     C*  C R E A Z I O N E    F N S T P R 0 F
212800970930     C***
212900970930     C                   Z-ADD     BLPAAS        STRAAS
213000970930     C                   Z-ADD     BLPLNP        STRLNP
213100970930     C                   Z-ADD     BLPNRS        STRNRS
213200970930     C                   Z-ADD     BLPNSP        STRNSP
213300970930     C                   Z-ADD     COMPDR        STRPDR
213400970930     C                   Z-ADD     COMDDC        STRDDC
213500970930     C                   Z-ADD     COMNDC        STRNDC
213600970930     C* NUMERO STOP
213700970930     C                   Z-ADD     CO2KSC        NUM4              4 0
213800970930     C                   CLEAR                   STRSTP
213900000615     C* imposto numero stop = 99999 se merce portata a magazzino
214000970930     C     BLPFDN        IFEQ      'S'
214100000615     C                   Z-ADD     99999         STRSTP
214200970930     C                   ELSE
214300970930     C* chaino FNCLS00F per prendere l'eventuale numero stop
214400970930     C     NUM4          IFNE      *ALL'8'
214500970930     C     NUM4          ANDNE     *ALL'9'
214600970930     C     CO2KSC        CHAIN     FNCLS000                           33
214700001020     C  N33              Z-ADD     CLSSTP        STRSTP
214800970930     C                   ENDIF
214900970930     C                   ENDIF
215000970930     C                   MOVEL     CO2KSC        STRKSC
215100970930     C                   MOVEL     COMRSC        STRRSC
215200970930    3C                   SELECT
215300970930     C     §3ABSD        WHENEQ    *BLANKS
215400970930     C     §3ABFI        ANDEQ     *BLANKS
215500970930     C                   MOVEL     'E'           STRINV
215600970930     C     §3ABSD        WHENEQ    *BLANKS
215700970930     C                   MOVEL     'S'           STRINV
215800970930     C     §3ABFI        WHENEQ    *BLANKS
215900970930     C                   MOVEL     'F'           STRINV
216000970930    3C                   ENDSL
216100970930     C                   WRITE     FNSTPR00
216200931019     C*
216300931019     C                   ENDIF
216400970930     C                   ENDIF
216500931019     C                   ENDSR
216600931019     C*
216700011121     C*--- CREAZIONE fiftt00F ----------------------------------------*
216800931203     C     CRTFTT        BEGSR
216900931203     C*
217000931203    2C     COMSET        IFGT      0
217100950113     C     WCPR          ANDGT     0
217200931203     C                   Z-ADD     WCPR          WPDR
217300950113     C     WFPP          IFEQ      'M'
217400950113     C                   Z-ADD     1000          WNDC
217500950113     C                   ELSE
217600950113     C                   Z-ADD     5000          WNDC
217700950113     C                   ENDIF
217800950113     C                   ADD       WNRT          WNDC
217900931203     C*
218000011121     C     KFTT          CHAIN     fiftt000                           32
218100931209     C* 32 OFF - AGGIORNAMENTO
218200931203    3C     *IN32         IFEQ      *OFF
218300931203     C*
218400931203     C                   ADD       COMSET        FTTSET
218500931203     C                   CLEAR                   COMSET
218600931209     C*
218700011121     C                   UPDATE    fiftt000
218800931209     C*
218900931210     C* SOLO SE GIA' VALORIZZATA
219000931210     C     FTTFVL        IFEQ      'V'
219100931209     C* RICHIAMO PGM DI SVALORIZZAZIONE DISTINTA PADRONCINO
219200931209     C                   Z-ADD     WPDR          PA3PDR
219300931209     C                   MOVEL     COMTSR        PA3TSR
219400931209     C                   Z-ADD     WNDC          PA3NDC
219500950115     C                   Z-ADD     WDRT          PA3DDC
219600931209     C                   MOVEL     'N'           PA3FCT
219700931209     C                   MOVEL     'N'           PA3FVD
219800931209     C                   MOVEL     'N'           PA3FVT
219900041122     C                   MOVEL     ' '           PA3sml
220000931209     C                   MOVEL     PARAM3        KPJBU
220100011121     C                   CALL      'FICNE2R'
220200931209     C                   PARM                    KPJBA
220300931210     C                   ENDIF
220400931203     C*
220500931203   X3C                   ELSE
220600931209     C* 32 ON  - IMMISSIONE
220700011121     C                   CLEAR                   fiftt000
220800931209     C*
220900931203     C                   ADD       COMSET        FTTSET
221000931203     C                   CLEAR                   COMSET
221100931203     C                   Z-ADD     WPDR          FTTPDR
221200020325     C                   movel     WPDR          FTTfgs
221300931203     C                   MOVEL     COMTSR        FTTTSR
221400950113     C                   Z-ADD     WNDC          FTTNDC
221500931203     C                   Z-ADD     WDRT          FTTDDC
221600950113     C                   MOVEL     WFPP          FTTFPP
221700950113     C     WFPP          IFEQ      'M'
221800950113     C                   MOVEL     ETM(W)        FTTPEP
221900030107     c                   Movel     Scm(w)        FttLvl
222000950113     C                   ELSE
222100950113     C                   MOVEL     ETP(W)        FTTPEP
222200030107     c                   Movel     Scp(w)        FttLvl
222300950113    2C                   ENDIF
222400931203     C*
222500011127      * In base alla data del documento aggancio la tab.GEDCN per
222600011127      * impostare la divisa della valorizzazione dei conteggi.
222700011127     c                   if        fttddc <= §geddtfv
222800011127     c                   movel     §geditl       fttdiv
222900011127     c                   else
223000011127     c                   movel     §gedeur       fttdiv
223100011127     c                   end
223200011127      *
223300011121     C                   WRITE     fiftt000
223400931203    3C                   ENDIF
223500931203    2C                   ENDIF
223600931203     C*
223700931203     C                   ENDSR
223800931203     C*
223900030107      *---------------------------------------------------------------*
224000030107      * Creazione Fiftt00f x ritiri annullati - bolle "FY"
224100030107      *---------------------------------------------------------------*
224200030107     c     CrtFtt1       BegSr
224300030107
224400030107     c                   Z-Add     1             w
224500030107     c     BlpPdr        Lookup    Pdr(w)                                 30
224600030107
224700030107     c                   Movel     BlpPdr        Wpdr
224800030107     c                   If        BlpFpp = 'M'
224900030107     c                   Z-Add     1000          Wndc
225000030107     c                   Else
225100030107     c                   Z-Add     5000          Wndc
225200030107     c                   EndIf
225300030107     c                   Add       BlpNrt        Wndc
225400030107     c                   Movel     BlpDrt        Wdrt
225500030107     c     Kftt          Chain     Fiftt01l
225600030107      * Aggiornamento
225700030107If  1c                   If        %Found(Fiftt01l)
225800030107     c                   Add       1             FttTbs
225900030107     c                   Update    Fiftt000
226000030107
226100030107      * Solo se già valorizzata
226200030107     c                   If        FttFvl = 'V'
226300030107      * Richiamo pgm di svalorizzazione distinta autotrasportatore
226400030107     c                   Z-Add     Wpdr          Pa3Pdr
226500030107     c                   Movel     ComTsr        Pa3Tsr
226600030107     c                   Z-Add     Wndc          Pa3Ndc
226700030107     c                   Z-Add     Wdrt          Pa3Ddc
226800030107     c                   Movel     'N'           Pa3Fct
226900030107     c                   Movel     'N'           Pa3Fvd
227000030107     c                   Movel     'N'           Pa3Fvt
227100041122     C                   MOVEL     ' '           PA3sml
227200030107     c                   Movel     Param3        Kpjbu
227300030107     c                   Call      'FICNE2R'
227400030107     c                   Parm                    Kpjba
227500030107     c                   EndIf
227600030107
227700030107   x1c                   Else
227800030107      * Immissione
227900030107     c                   Clear                   Fiftt000
228000030107
228100030107     c                   Z-Add     1             FttTbs
228200030107     c                   Z-Add     Wpdr          FttPdr
228300030107     c                   Movel     Wpdr          FttFgs
228400030107     c                   Movel     ComTsr        FttTsr
228500030107     c                   Z-Add     Wndc          FttNdc
228600030107     c                   Z-Add     Wdrt          FttDdc
228700030107     c                   Movel     BlpFpp        FttFpp
228800030107     c                   If        BlpFpp = 'M'
228900030107     c                   Movel     Etm(w)        FttPep
229000030107     c                   Movel     Scm(w)        FttLvl
229100030107     c                   Else
229200030107     c                   Movel     Etp(w)        FttPep
229300030107     c                   Movel     Scp(w)        FttLvl
229400030107     c                   EndIf
229500030107      * In base alla data del documento aggancio la tab.GEDCN per
229600030107      * impostare la divisa della valorizzazione dei conteggi.
229700030107     c                   If        FttDdc <= §geddtfv
229800030107     c                   Movel     §geditl       FttDiv
229900030107     c                   Else
230000030107     c                   Movel     §gedeur       fttDiv
230100030107     c                   EndIf
230200030107
230300030107     c                   Write     Fiftt000
230400030107    1c                   EndIf
230500030107
230600030107     c                   EndSr
230700030107
230800931027     C*--- CHAIN TABEL -----------------------------------------------*
230900931027     C     CTAB          BEGSR
231000931028     C* EFFETTUO LA CHAIN IN TABEL SOLO SE CAMBIANO IL TIPO BOLLA E LA
231100931028     C*   FILIALE DI APPARTENENZA DEL PADRONCINO
231200950113     C                   MOVEL     WCPR          COMAPP
231300950113     C                   MOVE      BLPCBO        COMAPP            4
231400931124     C*
231500931028     C     SAVAPP        IFNE      COMAPP
231600931124     C                   MOVEL     ' '           ALF1              1
231700931027     C*
231800931028     C                   MOVEL     '8C'          COD
231900931108     C                   MOVEL     'R'           COMKEY            2
232000950109     C                   MOVE      BLPCBO        COMKEY
232100931027     C                   MOVEL     *BLANKS       KEY
232200931027     C                   MOVEL     COMKEY        KEY
232300950113     C                   MOVEL     WCPR          W0030             3 0
232400950113     C                   MOVE      W0030         KEY
232500931027     C*
232600931027     C     KTAB          CHAIN     TABEL                              16
232700931124     C* SE NON ESISTE OPPURE ESISTE MA E' ANNULLATO, RIPROVO CON LA
232800931124     C*   FILIALE 000 = TUTTE
232900931027     C     *IN16         IFEQ      *ON
233000931027     C     *IN16         OREQ      *OFF
233100931027     C     TBLFLG        ANDNE     ' '
233200931124     C*
233300931027     C                   MOVE      '000'         KEY
233400931027     C     KTAB          CHAIN     TABEL                              16
233500931124     C*
233600931124     C     *IN16         IFEQ      *ON
233700931124     C     *IN16         OREQ      *OFF
233800931124     C     TBLFLG        ANDNE     ' '
233900931124     C                   MOVEL     '1'           ALF1
234000931124     C                   ENDIF
234100931124     C*
234200931027     C                   ENDIF
234300931027     C*
234400931028     C                   MOVEL     COMAPP        SAVAPP            4
234500931028     C                   ENDIF
234600931027     C                   ENDSR
234700910909     C**-----------------------------------------------*
234800000111     C** RIEMPO CAMPI DEL FILE FISPP00T
234900910909     C**-----------------------------------------------*
235000910909     C     RIESPP        BEGSR
235100000111     C                   Z-ADD     BRVPES        SPPPES
235200950109     C                   MOVEL     BRVLNP        SPPLNP
235300950109     C                   MOVEL     BRVLNA        SPPLNA
235400950109     C                   MOVEL     BRVNRS        SPPNRS
235500950109     C                   MOVEL     BRVNSC        SPPNSC
235600950109     C                   MOVEL     BRVZNC        SPPZNC
235700950109     C                   MOVEL     BRVNPS        SPPNPS
235800950109     C                   MOVEL     BRVFGS        SPPFGS
235900950109     C                   MOVEL     BRVNFV        SPPNFV
236000940527     C                   MOVEL     WDTP          SPPDFV
236100950109     C                   Z-ADD     1             SPPNPG
236200950113     C                   MOVEL     BRVCAN        SPPCAN
236300950118     C                   MOVEL     BRVVUC        SPPVUC
236400990906     C                   MOVEL     BRVPUC        SPPPUC
236500961203     C     §TVDEF        IFEQ      'S'
236600961203     C                   MOVEL     'F'           SPPATR
236700961203     C                   ENDIF
236800000111     C                   Z-ADD     BRVDFS        SPPDFS
236900070123     C                   movel     BRVHMS        SPPHMS
237000000111     C                   Z-ADD     BRVDCS        SPPDCS
237100070123     C                   movel     BRVHCS        SPPHCS
237200000320     C                   Z-ADD     WDTR          SPPDTR
237300050707     C                   TIME                    W0140            14 0
237400000320     C                   MOVEL     W0140         SPPHTR
237500000111     C                   Z-ADD     BRVFLE        SPPFLE
237600910909     C                   ENDSR
237700910910     C**-----------------------------------------------*
237800000428     C** RIEMPO CAMPI DEL FILE FNFVA01T-FNFWA00T
237900910910     C**-----------------------------------------------*
238000910910     C     RIEFVA        BEGSR
238100950123     C                   MOVEL     1             FVANPG
238200950109     C                   MOVEL     FGVLNP        FVALNP
238300910910     C                   MOVEL     FGVNFV        FVANFV
238400910910     C                   MOVEL     FGVDFV        FVADFV
238500910910     C                   MOVEL     FGVLNA        FVALNA
238600921202     C                   MOVEL     FGVFFV        FVAFFV
238700921202     C                   MOVEL     FGVFF2        FVAFF2
238800921202     C                   MOVEL     FGVFLP        FVAFLP
238900921202     C                   MOVEL     FGVFL2        FVAFL2
239000951116     C                   MOVEL     FGVTRN        FVATRN
239100951120     C                   MOVEL     FGVTTR        FVATTR
239200951116     C                   MOVEL     FGVFRQ        FVATFM
239300951116     C                   MOVEL     FGVTRM        FVATRM
239400951116     C                   MOVEL     FGVTRR        FVATRR
239500951116     C                   MOVEL     FGVPDR        FVAPDR
239600951116     C                   MOVEL     FGVDPD        FVADPD
239700951116     C                   MOVEL     FGVCCN        FVACCN
239800130429     C                   MOVEL     FGVpmb        FVADCN
239900960123     C                   MOVEL     FGVDTP        FVADRP
240000960123     C                   MOVEL     FGVHMP        FVAHRP
240100960124     C* CALL PGM PER PRENDERE LA CAUSALE RITARDO
240200960124     C                   MOVEL     FGVLNP        SA2LNP
240300960124     C                   MOVEL     FGVNFV        SA2NFV
240400960124     C                   CLEAR                   SA2TLA
240500960124     C                   CLEAR                   SA2RTP
240600960124     C                   CALL      'FNLSA2R4'
240700960124     C                   PARM                    SA2TLA            1
240800960124     C                   PARM                    SA2LNP
240900960124     C                   PARM                    SA2NFV
241000960124     C                   PARM                    SA2RTP
241100960123     C*
241200960124     C                   MOVEL     SA2RTP        FVARTP
241300000428     C*
241400000428     C* RIEMPI FILE DI APPOGGIO FWA
241500000428     C     WFGW          IFEQ      'S'
241600000428     C                   CLEAR                   FNFWA000
241700000428     C                   MOVEL     FGWATB        FWAATB
241800000428     C                   MOVEL     FGWNFV        FWANFV
241900000428     C                   MOVEL     FGWLNP        FWALNP
242000000428     C                   MOVEL     FGWFGS        FWAFGS
242100000428     C                   MOVEL     FGWIPT        FWAIPT
242200000428     C                   MOVEL     FGWVIP        FWAVIP
242300000428     C                   MOVEL     FGWIES        FWAIES
242400000428     C                   MOVEL     FGWVES        FWAVES
242500000428     C                   MOVEL     FGWFF3        FWAFF3
242600000428     C                   MOVEL     FGWFF4        FWAFF4
242700000428     C                   MOVEL     FGWFL3        FWAFL3
242800000428     C                   MOVEL     FGWFL4        FWAFL4
242900000428     C                   ENDIF
243000960123     C*
243100960123     C* CONTROLLO SE IN ARRIVO OCCORRERA' SBINARE IL F.VIAGGIO PARTENZA
243200960123     C*   DAL FOGLIO ARRIVI
243300960123    1C     PA2FLG        IFEQ      'M'
243400960123     C                   CLEAR                   FVASBN
243500960123   X1C                   ELSE
243600960123     C                   MOVEL     'S'           FVASBN
243700960123    1C                   ENDIF
243800960123     C*
243900910910     C                   ENDSR
244000060710     C**-----------------------------------------------*
244100060710     C** SCrittura dati mancanti di FNFV4 in fnfv400F per arrivo
244200060710     C**-----------------------------------------------*
244300060710     c     SCRIVIFV4     begsr
244400060710     c
244500060710     C* SCRITTURA TIPO AUTOMEZZO E CATEGORIA TRAINO
244600060710     c* direttamente nel file fisico senza passare dal file fnfv400t
244700060710     c* verifico se esiste già
244800060710     C                   MOVEL     'P'           FV4EPA
244900060710     C                   MOVEL     'V'           FV4TRC
245000060710     C                   Z-ADD     FGVLNP        FV4LNP
245100060710     C                   Z-ADD     FGVNFV        FV4NFV
245200060710     C                   Z-ADD     *ZEROS        FV4LAI
245300060710     c     kfv4e         setll     fnfv401l                               23
245400060710    1c                   if        not *in23
245500060710     C                   CLEAR                   DSFV4V
245600060710     C                   MOVEL     *BLANKS       FV4ATB
245700060710     C                   MOVEL     FGVTMZ        §V4TMZ
245800060710     C                   MOVE      FGVSNP        §V4TTR
245900060710     C                   MOVEL     DSFV4V        FV4NOT
246000060710     C                   movel     'N'           FV4ft1
246100060710     C                   Z-ADD     DATEU         FV4dt1
246200060710     c* Per la sede verifico se da trasmettere
246300060710     c     fv4trc        lookup    fv4s                                   23
246400060710    2c                   if        *in23
246500060710     C                   clear                   FV4ft2
246600060710     C                   clear                   FV4dt2
246700060710   x2c                   else
246800060710     C                   movel     'N'           FV4ft2
246900060710     C                   Z-ADD     DATEU         FV4dt2
247000060710    2c                   endif
247100060710     C                   WRITE     FNFV4000
247200060710    1C                   ENDIF
247300060710     c                   ENDSR
247400970221     C**-----------------------------------------------*
247500970221     C* VEDO SE RIELABORARE LE STATISTICHE
247600970221     C**-----------------------------------------------*
247700970221     C     FNSTA         BEGSR
247800001027     C                   CLEAR                   WWLNA
247900001027     C**
248000021016     c                   If        WdtPar <= §SdfParU
248100970221     C                   MOVE      WDTPAR        SA2DTS            8 0
248200970221     C                   MOVE      'P'           SA2TRC            1
248300970221     C                   CALL      'FNLSA2R3'
248400970221     C                   PARM                    SA2TLA
248500970221     C                   PARM                    SA2TRC
248600970221     C                   PARM                    SA2DTS
248700970221     C                   PARM                    WWLNA             3 0
248800970221     C                   END
248900001027     C*
249000021016     c                   If        WdtPar <= §SdfTerU
249100970221     C                   MOVE      WDTPAR        SA2DTS
249200970221     C                   MOVE      'T'           SA2TRC
249300970221     C                   CALL      'FNLSA2R3'
249400970221     C                   PARM                    SA2TLA
249500970221     C                   PARM                    SA2TRC
249600970221     C                   PARM                    SA2DTS
249700970221     C                   PARM                    WWLNA             3 0
249800001027     C                   END
249900970221     C                   ENDSR
250000910909     C**-----------------------------------------------*
250100910909     C** TRASMISSIONE SPUNTE
250200910909     C**-----------------------------------------------*
250300910909     C     SPUNTE        BEGSR
250400960124     C                   CLEAR                   SA2TLA
250500980210     C                   SETOFF                                       0106
250600920505     C                   SETOFF                                       08
250700910909     C                   Z-ADD     0             SAVLNA            3 0
250800920703     C                   Z-ADD     0             SAVLNP            3 0
250900950323     C                   CLEAR                   SAVCBO
251000950203     C                   CLEAR                   SAVSPE
251100960708     C                   CLEAR                   WLAST
251200970314     C                   CLEAR                   WRKSPE
251300150730     c                   eval      wnpg=1
251400000320     C* PRENDO DATA/ORA SISTEMA PER L'AGGIORNAMENTO DI DATA E ORA
251500000320     C* TRASMISSIONE SPUNTE
251600000320     C                   TIME                    W0140            14 0
251700000320     C                   CLEAR                   WLBDAT
251800000320     C                   MOVE      W0140         G02DAT
251900000320     C                   CALL      'XSRDA8'
252000000320     C                   PARM                    WLBDAT
252100000320     C                   Z-ADD     G02INV        WDTR              8 0
252200931021     C*
252300910909     C* LETTURA PER FOGLIO VIAGGIO
252400070123     C     KBRV          SETLL     FNBRV02
252500070123     C     KBRV          READE     FNBRV02                                22
252600910909     C*
252700950215    1C     *IN22         DOWEQ     '0'
252800950215    2C     BRVFTR        IFEQ      *BLANKS
252900931220     C                   CLEAR                   WSPEN
253000931220     C                   CLEAR                   WSPET
253100950215     C                   CLEAR                   WAGG
253200980326     C                   CLEAR                   NCLU
253300960603     C**
253400960603     C* DETERMINO LNP DELLA BOLLA SE COLLO CON SERIE IN CONFERMA DA
253500960603     C* ALTRI AS
253600960604     C                   MOVE      'S'           WALCF
253700960604     C                   MOVEL     BRVLNP        KLNP
253800960604     C                   MOVEL     BRVLNA        KLNA
253900960604     C                   MOVEL     BRVNRS        KNRS
254000960604     C                   MOVEL     BRVNSC        KNSC
254100960603     C                   EXSR      RICLNP
254200910909     C**
254300910909     C** L I N E A   A R R I V O   C A M B I A T A
254400950215    3C     SAVLNA        IFNE      BRVLNA
254500920427     C* SCRIVO PER LIN ARRIVO CON SCARICO IL TERMINAL SPUNTE " " E "T"
254600920427     C   08
254700910912     C* SCRIVO LINEA DI ARRIVO SPUNTE "A" SE C'E' TRANSITO
254800980210    4COR 06SAVLNA        IFNE      0
254900910912     C                   EXSR      WTRLNA
255000950215    4C                   END
255100050203     c* mi salvo in una schiera parallela alle linee del foglio viaggio, se
255200050203     x*  locale, se presenti spunte transito
255300050204     c                   if        fgvlnp=flptfa  and savlna>0
255400050204     C                   Z-ADD     1             B
255500050204     C                   MOVEL     savLNA        COMPRO
255600050203     C     COMPRO        LOOKUP    FFV(B)                                 20
255700050203     C   20              MOVEL     winvialocale  winvloc(B)
255800050203     c                   endif
255900050203     c                   clear                   winvialocale
256000930712     C*
256100980210     C                   SETOFF                                       060801
256200930127     C                   Z-ADD     1             B                 3 0
256300950109     C                   MOVEL     BRVLNA        COMPRO            3
256400910912     C     COMPRO        LOOKUP    FFV(B)                                 20
256500910912     C*     TROVATA FIL SCARICO
256600930127     C   20              MOVEL     FLP(B)        COMLIN            3 0
256700910912     C* NON TROVATA --> METTO LINEA ARRIVO STESSA
256800950109     C  N20              MOVEL     BRVLNA        COMLIN
256900931022     C*
257000970811     C* F I L I A L E   S C A R I C O
257100051031     C                   CLEAR                   dslv55
257200051031     C****               MOVEL     'P'           d55TPT
257300051031     c                   z-add     fgvlnp        d55lnp
257400051031     C                   Z-ADD     COMLIN        d55LIN
257500051031     C                   Z-ADD     FGVDFV        d55DRF
257600051031     C                   CALL      'FNLV55R'
257700051031     C                   PARM                    dslv55
257800051031     C                   MOVEL     d55TFP        SAVFEL            3 0
257900051031     C                   MOVEL     d55TFP        VARX
258000051031     C                   MOVEL     d55TFP        VAR2X
258100051031     C                   Z-ADD     d55TFA        FLPTFA
258200950109     C*
258300021125     C                   SETON                                        01
258400910912     C* APERTURA MEMBRI
258500970811     C                   EXSR      CAEXSP
258600050204    4C***                END
258700920703     C*
258800980210     C* Reperisco terminal di arrivo della linea di arrivo
258900971105     C* Reperisco anche il terminal di partenza
259000130531     C                   CLEAR                   dslv55
259100130531     C                   MOVEL     ' '           d55TPT
259200130531     C                   Z-ADD     BRVLNA        d55LIN
259300130531     C                   Z-ADD     FGVLNP        d55LNP
259400130531     C                   Z-ADD     WDTP          d55DRF
259500130531     C                   CALL      'FNLV55R'
259600130531     C                   PARM                    dslv55
259700130531     C                   Z-ADD     d55TFA        LNATFA
259800130531     C                   Z-ADD     d55TFP        LNAFEL
259900051031     c
260000051031     c* Reperisco anche il terminal di arrivo della spunta letta
260100051031     C                   CLEAR                   dslv55
260200051031     C                   MOVEL     ' '           d55TPT
260300051031     C                   Z-ADD     BRVLNA        d55LIN
260400051031     C                   Z-ADD     wlnp          d55LNP
260500051031     C* Se fv poste gli passo il p.o. poste
260600051031     C     WFGW          IFEQ      'S'
260700051031     C     FGWFGS        ANDGT     *ZEROS
260800051031     C                   Z-ADD     FGWFGS        d55LNP
260900051031     C                   ENDIF
261000051031     C                   Z-ADD     WDTP          d55DRF
261100051031     C                   CALL      'FNLV55R'
261200051031     C                   PARM                    dslv55
261300051031     C                   Z-ADD     d55TFA        spuTFA            3 0
261400051031     c
261500031218     c* Determino se collo locale per sottomissione lr15
261600031218     c* Una volta acceso in92 non si deve rispegnere perchè basta che
261700031218     c* ce ne sia una per sottomettere
261800031218    1c     *in92         ifeq      *off
261900051031    3c     fgvlnp        ifeq      lnatfa
262000031218     c                   seton                                        92
262100031218    3c                   endif
262200031218    1c                   endif
262300980210     C**
262400980210     C* SE SCARICO >< DA LINEA DI ARRIVO
262500980210    4C     COMLIN        IFNE      BRVLNA
262600940107     C*
262700980210     C* 06  ON - T R A N S I T O
262800940107     C* SE SCARICO >< DA TERMINAL DI ARRIVO LINEA DI ARRIVO
262900980210    6C     COMLIN        IFNE      LNATFA
263000980210     C                   SETON                                        06
263100930312     C* APRO MEMBRI DEL TRANSITO
263200980210     C                   EXSR      TRANSI
263300980210     C                   EXSR      CAEX08
263400920703     C*
263500950215    6C                   END
263600950215    4C                   END
263700910909     C*
263800950109     C                   MOVEL     BRVLNA        SAVLNA
263900051031     C                   MOVEL     wlnp          SAVLNp
264000051031   x3c                   else
264100051031     c* se non cambiata la linea di arrivo vedo se cambiata la
264200051031     c*  linea di partenza spunte per la ricerca del tfa della spunta
264300051031    4c                   if        wlnp<>savlnp
264400051031     c* Reperisco anche il terminal di arrivo della spunta letta
264500051031     C                   CLEAR                   dslv55
264600051031     C                   MOVEL     ' '           d55TPT
264700051031     C                   Z-ADD     BRVLNA        d55LIN
264800051031     C                   Z-ADD     wlnp          d55LNP
264900051031     C* Se fv poste gli passo il p.o. poste
265000051031    5C     WFGW          IFEQ      'S'
265100051031     C     FGWFGS        ANDGT     *ZEROS
265200051031     C                   Z-ADD     FGWFGS        d55LNP
265300051031    5C                   ENDIF
265400051031     C                   Z-ADD     WDTP          d55DRF
265500051031     C                   CALL      'FNLV55R'
265600051031     C                   PARM                    dslv55
265700051031     C                   Z-ADD     d55TFA        spuTFA            3 0
265800051031     c                   z-add     wlnp          savlnp
265900051031    4C                   ENDIF
266000051031    3C                   ENDIF
266100950515     C*
266200960603     C                   CLEAR                   WTRAN
266300960603     C                   SETOFF                                       19
266400910912     C*
266500950515     C*  19 ON  - NON E' SPUNTA DA TRANSITO
266600950515     C*  19 OFF -     E' SPUNTA DA TRANSITO
266700960531     C     WLNP          LOOKUP    L1                                     19
266800980401     C**
266900980401     C* IGNORO OGNI AGGIORNAMENTO SE SI TRATTA DI SPUNTA DEFLUENZA
267000980401     C*  PERCHE' E' A TUTTI GLI EFFETTI UNA SPUNTA ARRIVI
267100980401   2AC     §TVDEF        IFNE      'S'
267200911018     C*
267300911106     C** T R A N S I T O
267400930203     C*
267500970221    3C     *IN19         IFEQ      *OFF
267600970828     C     WTAB5F        ANDEQ     ' '
267700950113     C*
267800911106     C**    PRENDO BOLLE E BLT DAL FILE DI TRANSITO
267900960603     C*     Non ripeto la chain se l'ho già fatta durante la ricerca
268000960603     C*     della lnp bolla
268100960603     C                   SELECT
268200960611     C     WOKBT         WHENEQ    *BLANKS
268300020606     C     KBtt37        CHAIN     FNBTT37L                           20
268400960611     C     WOKBT         WHENEQ    'S'
268500960603     C                   SETOFF                                       20
268600960603     C                   ENDSL
268700921014     C*
268800950515     C* CHIUDO ANOMALIA : "DISGUIDO"
268900970221     C*
269000970221   3AC     *IN20         IFEQ      *ON
269100970221     C                   Z-ADD     200           COMCAA
269200970221     C                   EXSR      CHIUAN
269300050927     c                   clear                   noinvia           1
269400130527     c* Per foglio locale non invio la spunta come disguido
269500050927     c*  (si tratta di colli di defluenza sparato per errore sul foglio
269600050927     c*   locale ma non si tratta nè di vera defluenza, nè di disguido)
269700050927     c                   if        wesisteanm=' '
269800050927     c                   eval      noinvia='S'
269900050927     c                   endif
270000970827     C* SOLO SE HO TROVATO L'ANOMALIA
270100970827     C  N21              Z-ADD     1             NCLU
270200921014     C**
270300970221  X3AC                   ELSE
270400970221     C**
270500950515     C                   MOVEL     'S'           WTRAN             1
270600921014     C*
270700950515     C* CHIUDO ANOMALIA : "BOLLA TRANSITO"
270800921014     C                   Z-ADD     125           COMCAA
270900921014     C                   EXSR      CHIUAN
271000921014     C*
271100950621    4C     BTTDUT        IFNE      0
271200950621     C                   MOVEL     'N'           WAGG              1
271300950621    4C                   ENDIF
271400950621     C*
271500950113     C                   MOVEL     FGVNFV        BTTNFV
271600970818     C* SE ERA PISTOLA FASULLA IMPOSTO 87
271700970827     C     BTTPUT        IFGT      0
271800970818     C     BTTPUT        LOOKUP    NPF                                    39
271900970827     C                   ELSE
272000970827     C                   SETOFF                                       39
272100970827     C                   ENDIF
272200970827     C**
272300970818     C     *IN39         IFEQ      *ON
272400950621     C                   MOVE      87            BTTPUT
272500950621     C                   ELSE
272600950113     C                   MOVEL     BRVNPS        BTTPUT
272700950621     C                   END
272800970827     C*
272900950706     C* Se le statistiche partito e a terra sono già state elaborate
273000950705     C* scrivo o aggiorno file FNSTA00F per rielaborazione statistiche
273100970221     C     BTTDUT        IFGT      *ZEROS
273200970221     C                   Z-ADD     BTTDUT        WDTPAR
273300970221     C                   EXSR      FNSTA
273400970221     C                   ENDIF
273500970221     C*
273600091027     c                   Z-ADD     WDTP          WDTPAR
273700970221     C                   EXSR      FNSTA
273800970221     C**
273900950113     C                   MOVEL     WDTP          BTTDUT
274000980407     C                   Z-ADD     1             NCLU
274100910927     C*
274200020606     C                   UPDATE    FNBTT037
274300950130     C*
274400950130     C* C A M B I A T A   S P E D I Z I O N E --> RICHAINO LA BOLLA
274500950215    4C     BTTSPE        IFNE      SAVSPE
274600970307     C*
274700970307     C* IMPOSTO LA SPEDIZIONE PERCHE' A CAMBIO ULTIMO COLLO SCRIVO AGB
274800970307     C     WRKSPE        IFEQ      0
274900970307     C                   Z-ADD     BTTSPE        WRKSPE
275000970307     C                   MOVEL     'T'           WRKTBO
275100970307     C                   ENDIF
275200950130     C*
275300020605     C     KBTT3         CHAIN     FNBTP11L                           21
275400930127     C*
275500091027     c* BTPDT3  contiene il terminal a cui trasmetto
275600020606     c                   z-add     btpdt3        savdt3            3 0
275700020606     c                   z-add     comlin        btpdt3
275800091027     c
275900950109     C                   MOVEL     'T'           BTPFT1
276000950113     C                   Z-ADD     DATEU         BTPDT1
276100950109     C                   MOVEL     'N'           BTPFT2
276200950113     C                   Z-ADD     DATEU         BTPDT2
276300950109     C                   MOVEL     'N'           BTPFT3
276400940110     C*
276500940110     C* CALCOLO PESO E VOLUME UNITARIO DEL COLLO
276600950110     C                   EXSR      CALUNT
276700931022     C*
276800911011     C* AGGIORNO DATA PARTENZA DAL TRANSITO
276900950215    5C     BTPDUT        IFEQ      0
277000931220     C* MUOVO 1 NEL CAMPO DELLE SPEDIZIONI
277100931220     C                   Z-ADD     1             WSPET
277200950109     C                   MOVEL     WDTP          BTPDUT
277300950621     C                   ELSE
277400950621     C                   MOVEL     'T'           AGBTBO
277500950621     C                   MOVE      BTPAAS        AGBAAS
277600950621     C                   MOVE      BTPLNP        AGBLNP
277700950621     C                   MOVE      BTPNRS        AGBNRS
277800950621     C                   MOVE      BTPNSP        AGBNSP
277900950621     C                   MOVEL     'DT'          AGBAGB
278000950630     C                   WRITE     FNAGB000                             39
278100950215    5C                   END
278200950109     C*
278300950109     C                   UPDATE    FNBTP000
278400950130     C*
278500950130     C                   MOVEL     BTTSPE        SAVSPE
278600910927     C*
278700091027     C* SE TRANSITO PER TRANSITO : INVIO DI NUOVO BOLLA
278800020606    5C   06SAVdt3        IFNE      COMLIN
278900031219     c* Se p.o. di scarico è anche il ter-part.della lnp bolla non devo
279000031219     c* ritrasmettere
279100130531     c                   clear                   dslv55
279200130531     c                   movel     'P'           d55tpt
279300130531     c                   z-add     wlnp          d55lin
279400130531     c                   z-add     wdtp          d55drf
279500130531     c                   call      'FNLV55R'
279600130531     c                   parm                    dslv55
279700130531     c     comlin        ifne      d55tfp
279800950109     C                   EXSR      WTRBTP
279900130531     C                   endif
280000950808    5C                   END
280100950808    4C                   ENDIF
280200970307     C**
280300970307     C* A CAMBIO ULTIMO COLLO SCRIVO
280400970307     C                   MOVEL     'T'           BTTTBO
280500970307     C     BTTKEY        IFNE      WRKKEY
280600970307     C* SCRIVO IL FILE FNAGB01L PER AGGIORNARE DATA ULTIMO COLLO
280700970307     C                   MOVEL     WRKTBO        AGBTBO
280800970307     C                   MOVEL     WRKAAS        AGBAAS
280900970307     C                   MOVEL     WRKLNP        AGBLNP
281000970307     C                   MOVEL     WRKNRS        AGBNRS
281100970307     C                   MOVEL     WRKNSP        AGBNSP
281200970307     C                   MOVEL     'DT'          AGBAGB
281300970307     C                   WRITE     FNAGB000                             39
281400970307     C**
281500970307     C                   MOVEL     BTTKEY        WRKKEY
281600970307     C                   ENDIF
281700970221   3AC                   ENDIF
281800970221    3C                   END
281900910912     C**
282000991022     C** L N P
282100910927     C**
282200960604    3C     *IN19         IFEQ      *ON
282300970827     C     WTAB5F        OREQ      'S'
282400960603     C*     Non ripeto la chain se l'ho già fatta durante la ricerca
282500960603     C*     della lnp bolla
282600960603     C                   SELECT
282700960611     C     WOKBL         WHENEQ    *BLANKS
282800960603     C     KBRV4         CHAIN     FNBLT27L                           20
282900960611     C     WOKBL         WHENEQ    'S'
283000960603     C                   SETOFF                                       20
283100960603     C                   ENDSL
283200911108     C*
283300921014     C* CREO   ANOMALIA : "MANCA BOLLA"
283400991022     C***
283500991022   3AC     *IN20         IFEQ      *ON
283600911108     C                   Z-ADD     105           COMCAA
283700990517     C     KANM3         SETLL     FNANM                                  21
283800911108     C  N21              EXSR      CRTANM
283900970221     C*
284000991022     C* NON SOMMO I COLLI  MANCA REC BOLLA
284100970221     C*  PERCHE' LO FARO' AL MOMENTO DELLA
284200970221     C*  BORDERIZZAZIONE DELLA BOLLA, SE E' UN MANCA REC BOLLA VERO
284300970221     C*  PER IL PROSPETTO NOLI (LE STATISTICHE HANNO UN LORO GIRO
284400970827  X3AC                   ELSE
284500991022     C* SE ESISTE NELL'ALTRO S.I. DO IL DISGUIDO E BASTA
284600040531   3BC**   WALTSI        IFEQ      'S'
284700040531     C**                 MOVEL     'D'           FLG
284800040531     C**                 EXSR      CALL44
284900040531  X3BC**                 ELSE
285000911108     C*
285100950515     C                   CLEAR                   WTRAN
285200110907     c
285300110907     c                   setoff                                       37
285400921014     C*
285500950515     C* CREO   ANOMALIA : "MANCA RECORD BOLLA BORDERIZZATA"
285600040625     C* NON LO FACCIO SE bolla poste
285700110907    4c                   if        bltnfv=0 or brvnps =88 or
285800110907     c                             brvnps=90
285900040603     c* Verifico se bolla con merce di valore da memorizzare nella
286000040603     c*  anomalia
286100040603     c     kar5          chain(N)  fiar501l
286200040603     c                   if        %found(fiar501l)
286300040603     c                   movel     ar5uni        dar5gen
286400040603     c                   else
286500040603     c                   clear                   dar5gen
286600040603     c                   endif
286700110907
286800110907    5C     BLTNFV        IFEQ      0
286900110907     C                   Z-ADD     110           COMCAA
287000040603     C     KANM3         SETLL     FNANM                                  21
287100911108     C  N21              EXSR      CRTANM
287200110907    5C                   END
287300110907     c*
287400110907     C* CREO   ANOMALIA : "COLLO MAI AFFIDATO"
287500110907    5c                   if        brvnps =88 or brvnps=90
287600110907     C* PER SICUREZZA VERIFICO CHE NON SIA PRESENTE UNA QUALSIASI SPUNTA
287700110907     C* IN ALTRA FILIALE
287800110907     c                   clear                   EsSpunta          1
287900110907     C                   MOVEL     DSBRV         DSBRVS
288000110907     C     KBRV4         setll     FNbrv07l
288100110907     C     KBRV4         reade     FNbrv07l
288200110907     c                   dow       not %eof(fnbrv07l)
288300110907     c* Escludo le pistole completamente fasulle
288400110907     c                   if        brvnps<>88 and brvnps<>90
288500110907     c                   eval      EsSpunta='S'
288600110907     c                   leave
288700110907     c                   endif
288800110907     c
288900110907     C     KBRV4         reade     FNbrv07l
289000110907     c                   enddo
289100110907     c
289200110907     C* Reimposto campi di fnbrv con dati salvati
289300110907     C                   MOVEL     DSBRVS        DSBRV
289400110907     C*
289500110908    6c                   if        EsSpunta=' '
289600110907     C                   Z-ADD     115           COMCAA
289700110907     C     KANM3         SETLL     FNANM                                  21
289800110907     C  N21              EXSR      CRTANM
289900110907    6c                   endif
290000110907    5c                   endif
290100110907     c
290200110907    4c                   endif
290300911108     C*
290400921014     C* CHIUDO ANOMALIA : "BOLLA PARTENZA"
290500921014     C                   Z-ADD     120           COMCAA
290600020412     C                   EXSR      CHIUAN
290700911108     C*
290800911108     C*
290900950621    4C     BLTDFV        IFNE      0
291000950621     C                   MOVEL     'N'           WAGG
291100950621    4C                   ENDIF
291200950621     C*
291300930520     C                   MOVEL     FGVNFV        BLTNFV
291400970221     C*
291500950706     C* Se le statistiche partito e a terra isono già state elaborate
291600950705     C* scrivo o aggiorno file FNSTA00F per rielaborazione statistiche
291700970221     C     BLTDFV        IFGT      *ZEROS
291800970221     C                   Z-ADD     BLTDFV        WDTPAR
291900970221     C                   EXSR      FNSTA
292000970221     C                   ENDIF
292100970221     C**
292200950707     C                   Z-ADD     WDTP          WDTPAR
292300970221     C                   EXSR      FNSTA
292400970221     C*
292500950706     C                   MOVEL     WDTP          BLTDFV
292600970627     C* PULISCO IL CAMPO CHE MI SEGNALA UNA EVENTUALE MANCANZA DI COLLO
292700970627     C* IMPORT DATOMI DAL PARTNER
292800970627     C                   CLEAR                   BLTFL1
292900970818     C* SE ERA PISTOLA FASULLA IMPOSTO 87
293000970827     C     BLTNPP        IFGT      0
293100970818     C     BLTNPP        LOOKUP    NPF                                    39
293200970827     C                   ELSE
293300970827     C                   SETOFF                                       39
293400970827     C                   ENDIF
293500970827     C**
293600970818     C     *IN39         IFEQ      *ON
293700950621     C                   MOVE      87            BLTNPP
293800950621     C                   ELSE
293900950110     C                   MOVEL     BRVNPS        BLTNPP
294000950623     C                   END
294100910927     C                   MOVEL     COMLIN        BLTFLP
294200980407     C                   Z-ADD     1             NCLU
294300910927     C*
294400950110     C                   UPDATE    FNBLT027
294500950130     C*
294600950130     C* C A M B I A T A   S P E D I Z I O N E --> RICHAINO LA BOLLA
294700950215    4C     BLTSPE        IFNE      SAVSPE
294800970307     C*
294900970307     C* IMPOSTO LA SPEDIZIONE PERCHE' A CAMBIO ULTIMO COLLO SCRIVO AGB
295000970307     C     WRKSPE        IFEQ      0
295100970307     C                   Z-ADD     BLTSPE        WRKSPE
295200970307     C                   MOVEL     'P'           WRKTBO
295300970307     C                   ENDIF
295400950130     C*
295500151022     C     KBLT3         CHAIN     FNBLP01L
295600940107     C*
295700940107     C* CALCOLO PESO E VOLUME UNITARIO DEL COLLO
295800940107     C                   EXSR      CALUNI
295900931022     C*
296000910927     C* SE E' IL PRIMO COLLO CHE PARTE METTO DATA FV
296100950215    5C     BLPDPC        IFEQ      0
296200931220     C* MUOVO 1 NEL CAMPO DELLE SPEDIZIONI
296300931220     C                   Z-ADD     1             WSPEN
296400950124     C                   MOVEL     WDTP          BLPDPC
296500950215    5C                   END
296600910930     C* FIL TRANSITO
296700930127     C                   MOVEL     BLPFLP        SAVFLP            3 0
296800910930     C                   MOVEL     COMLIN        BLPFLP
296900910930     C* AGGIORNO
297000151022     c                   if        %found(fnblp01l)
297100950110     C                   UPDATE    FNBLP001
297200151022     c                   else
297300151022     c                   exsr      sr_errore
297400151022     c                   endif
297500910927     C*
297600910930     C** LNP PER TRANSITO
297700910927     C**   SE NON CORRISPONDE A TRANSITO DELLA BOLLA SPEDISCO LA BOLLA
297800920505     C**   PERCHE' POSSO MANDARE VIA LA BOLLA PER UNA CERTA LINEA E
297900930707     C**   IL GIORNO DOPO I COLLI PER UN'ALTRA LINEA (RIMANDO BOLLA)
298000980210    5C   06SAVFLP        IFNE      0
298100920512     C     SAVFLP        ANDNE     COMLIN
298200930312     C     SAVFLP        OREQ      0
298300930312     C     BLPLNA        ANDNE     COMLIN
298400091027     C
298500091027     c* Imposto il numero foglio viaggio  di chiusura
298600091027     c                   eval      blpnfv=fgv2
298700950110     C                   WRITE     FNBLPT00
298800080423     C
298900040310    5c                   ENDIF
299000950130     C*
299100950130     C*
299200950130     C                   MOVEL     BLTSPE        SAVSPE
299300950215    4C                   ENDIF
299400970307     C*
299500970307     C* A CAMBIO ULTIMO COLLO SCRIVO
299600970307     C                   MOVEL     'P'           BLTTBO
299700970307     C     BLTKEY        IFNE      WRKKEY
299800970307     C* SCRIVO IL FILE FNAGB01L PER AGGIORNARE DATA ULTIMO COLLO
299900970307     C                   MOVEL     WRKTBO        AGBTBO
300000970307     C                   MOVEL     WRKAAS        AGBAAS
300100970307     C                   MOVEL     WRKLNP        AGBLNP
300200970307     C                   MOVEL     WRKNRS        AGBNRS
300300970307     C                   MOVEL     WRKNSP        AGBNSP
300400970307     C                   MOVEL     'DT'          AGBAGB
300500970307     C                   WRITE     FNAGB000                             39
300600970307     C**
300700970307     C                   MOVEL     BLTKEY        WRKKEY
300800970307     C                   ENDIF
300900950130     C*
301000040531   3BC**                 ENDIF
301100991022   3AC                   ENDIF
301200960403    3C                   ENDIF
301300980326   2AC                   ENDIF
301400910927     C**
301500910912     C** I N V I O   L A   S P U N T A
301600910927     C**
301700950516    3C   01              DO
301800000111     C                   CLEAR                   FISPP000
301900950215     C                   CLEAR                   SPPATR
302000950215     C                   CLEAR                   SPPFLP
302100910912     C* SE TRANSITO PER LNA '--> METTO FLAG "T"
302200970814     C*  SE E' LINEA/SERIE COMFERMABILE DA SIMFEL NON CREO IL DISGUIDO
302300950515     C     *IN19         IFEQ      *OFF
302400970814     C     WTAB5F        ANDEQ     ' '
302500040401     c* Se spunta che invio al p.o. di transito memorizzo il p.o. di scarico
302600040401     c   06              z-add     comlin        sppflp
302700950515     C  N06              MOVEL     'T'           SPPATR
302800971105     C  N06              MOVEL     FGVLNP        SPPFLP
302900940322     C* SE E' UN COLLO IN DISGUIDO METTO "D"
303000950515     C     WTRAN         IFEQ      ' '
303100050927     c* non si tratta di disguido se foglio locale e non esiste l'anom.200
303200050927     c                   if        fgvlnp<>flptfa or
303300050927     c                             (fgvlnp=flptfa and noinvia=' ')
303400950515     C                   MOVEL     'D'           SPPATR
303500971105     C                   MOVEL     FGVLNP        SPPFLP
303600050927     C                   endif
303700050927     c
303800050927     C                   ENDIF
303900020607     c                   else
304000020607     c* Se spunta che invio al p.o. di transito memorizzo il p.o. di scarico
304100020607     c   06              z-add     comlin        sppflp
304200950515     C                   ENDIF
304300991025     C* SE BOLLA IN ALTRO S.I. INVIO LA SPUNTA COME "D"
304400040531     C**   WALTSI        IFEQ      'S'
304500040531     C**                 MOVEL     'D'           SPPATR
304600040531     C**                 MOVEL     FGVLNP        SPPFLP
304700040531     C**                 ENDIF
304800910912     C*
304900910912     C                   EXSR      RIESPP
305000910910     C*
305100050204     c* Se sul foglio locale ho memorizzato spunte transito, devo
305200050204     c*  inviare il foglio per l'abbinamento
305300050204     c                   if        FGVLNP<>flpTFA Or
305400050204     c                             (fgvlnp=flptfa and sppatr<>' ' and
305500051031     c                              sppatr<>'F' and not *in06 and
305600051031     c                              sputfa<>flptfa)
305700050204     c                   if        fgvlnp=flptfa
305800050204     C                   MOVEL     'S'           winvialocale
305900050204     c                   endif
306000050204     c
306100000111     C                   WRITE     FISPP000
306200950215    3C                   END
306300050203    3C                   END
306400931022     C*
306500950110     C* AGGIORNO FNFVD00F
306600950215     C     WAGG          IFEQ      ' '
306700931022     C                   EXSR      AGGFVD
306800950215     C                   ENDIF
306900931022     C*
307000911107     C* AGGIORNO SPUNTA
307100950110     C                   MOVEL     'T'           BRVFTR
307200000320     C                   Z-ADD     WDTR          BRVDTR
307300050707     c                   time                    w0140
307400000320     C                   MOVEL     W0140         BRVHTR
307500070123     C                   UPDATE    FNBRV02
307600980728     C                   ELSE
307700070123     C* SE NE LEGGO UNA CON CODICE ANOMALIA E LA DEVO SCRIVERE IN FNBRV
307800980728     C*  SOLO SE SI TRATTA DI UN FOGLIO DI DEFLUENZA
307900980728     C     BRVCAN        IFEQ      'E'
308000070123     C     kbrve         setll     fNbrve                                 99
308100070123     C  n99              WRITE     FNBRVE
308200980728     C                   ENDIF
308300950215    2C                   END
308400070123     C     KBRV          READE     FNBRV02                                22
308500910910     C*
308600950215    1C                   END
308700910910     C*
308800920427     C* SCRIVO PER LIN ARRIVO CON SCARICO IL TERMINAL SPUNTE " " E "T"
308900920427     C   08
309000910912     C* SCRIVO PER ULTIMA LINEA ARRIVO
309100980210     COR 06SAVLNA        IFNE      0
309200960708     C                   MOVE      'S'           WLAST             1
309300910912     C                   EXSR      WTRLNA
309400910910     C                   END
309500050204     c* mi salvo in una schiera parallela alle linee del foglio viaggio, se
309600050204     x*  locale, se presenti spunte transito
309700050204     c                   if        fgvlnp=flptfa  and savlna>0
309800050204     C                   Z-ADD     1             B
309900050204     C                   MOVEL     savLNA        COMPRO
310000050204     C     COMPRO        LOOKUP    FFV(B)                                 20
310100050204     C   20              MOVEL     winvialocale  winvloc(B)
310200050204     c                   endif
310300970307     C**
310400970307     C* SCRIVO L'ULTIMO FNAGB
310500970307     C     WRKSPE        IFGT      0
310600970307     C                   MOVEL     WRKTBO        AGBTBO
310700970307     C                   MOVEL     WRKAAS        AGBAAS
310800970307     C                   MOVEL     WRKLNP        AGBLNP
310900970307     C                   MOVEL     WRKNRS        AGBNRS
311000970307     C                   MOVEL     WRKNSP        AGBNSP
311100970307     C                   MOVEL     'DT'          AGBAGB
311200970307     C                   WRITE     FNAGB000                             39
311300970307     C                   ENDIF
311400910909     C*
311500910909     C                   ENDSR
311600940107     C*
311700950110     C* CALCOLO VOLUME E PESO UNITARI FNBTT--------------------------*
311800950110     C     CALUNT        BEGSR
311900940107     C* COLLI / PESO / VOLUME UNITARI
312000940107     C                   Z-ADD     1             NCLU
312100030902     c*
312200050112     C     BtPNCR        IFEQ      BtPNCL
312300021111     C                   Z-ADD     BtPVLC        Wfvdvol
312400021111     C*
312500021111   X4C                   ELSE
312600021111    5C     BtPVLC        IFGE      BtPVLb
312700021111     C                   Z-ADD     BtPVLC        Wfvdvol
312800021111   X5C                   ELSE
312900021111     C                   Z-ADD     BtPVLb        Wfvdvol
313000021111    5C                   ENDIF
313100021111    4C                   ENDIF
313200021111     c
313300021111     C     wfvdvol       DIV(H)    BtPNCL        VOLU
313400030902     c*
313500050112     C     BtPNCp        ifeq      BtPNCL
313600021111     C                   Z-ADD     BtPpkC        Wfvdpes
313700021111     C*
313800021111   X4C                   ELSE
313900021111    5C     BtPpkC        IFGE      BtPpkb
314000021111     C                   Z-ADD     BtPpkC        Wfvdpes
314100021111   X5C                   ELSE
314200021111     C                   Z-ADD     BtPpkb        Wfvdpes
314300021111    5C                   ENDIF
314400021111    4C                   ENDIF
314500030902     c*
314600021111     C     wfvdpes       DIV(H)    BtPNCL        PKGU
314700940107     C*
314800940107     C                   ENDSR
314900950110     C*
315000950110     C* CALCOLO VOLUME E PESO UNITARI FNBLT--------------------------*
315100950110     C     CALUNI        BEGSR
315200950110     C* COLLI / PESO / VOLUME UNITARI
315300950110     C                   Z-ADD     1             NCLU
315400021111     c
315500050112     C     BLPNCR        ifeq      BLPNCL
315600021111     C                   Z-ADD     BLPVLC        Wfvdvol
315700021111     C*
315800021111   X4C                   ELSE
315900021111    5C     BLPVLC        IFGE      BLPVLF
316000021111     C                   Z-ADD     BLPVLC        Wfvdvol
316100021111   X5C                   ELSE
316200021111     C                   Z-ADD     BLPVLF        Wfvdvol
316300021111    5C                   ENDIF
316400021111    4C                   ENDIF
316500030902     c*
316600021111     C     wfvdvol       DIV(H)    BLPNCL        VOLU
316700021111     c
316800050112     C     BLPNCp        ifeq      BLPNCL
316900021111     C                   Z-ADD     BLPpkC        Wfvdpes
317000021111     C*
317100021111   X4C                   ELSE
317200021111    5C     BLPpkC        IFGE      BLPpkF
317300021111     C                   Z-ADD     BLPpkC        Wfvdpes
317400021111   X5C                   ELSE
317500021111     C                   Z-ADD     BLPpkF        Wfvdpes
317600021111    5C                   ENDIF
317700021111    4C                   ENDIF
317800030902     c*
317900021111     C     wfvdpes       DIV(H)    BLPNCL        PKGU
318000950110     C*
318100950110     C                   ENDSR
318200930708     C*
318300950110     C*--- AGGIORNO CAMPI DI FNFVD -----------------------------------*
318400931022     C     AGGFVD        BEGSR
318500931022     C*
318600980210    1C     NCLU          IFGT      0
318700950110     C* ESCLUSE LE BOLLE RECUPERO: MA DAL MOMENTO CHE NON HANNO COLLI.
318800950110     C*  NON MI PREOCCUPO
318900970221     C* ESCLUDO IL CODICE SOCIALE 2 ===> SDI  SE TROVATA BOLLA
319000980210    2C     *IN19         IFEQ      *ON
319100970221     C     *IN20         ANDEQ     *OFF
319200070223     C***  BLPSCL        ANDNE     '2'
319300970221     C* ELABORO SE NON TROVATA BOLLA
319400970221     C     *IN19         OREQ      *ON
319500970221     C     *IN20         ANDEQ     *ON
319600980210     C* ELABORO SE E' COLLO DISGUIDO O TRANSITO
319700950110     C     *IN19         OREQ      *OFF
319800970221     C*
319900950110     C* TIPO RECORD 1= BOLLE DELL'AREA    2=BOLLE TRANSITO
320000950515     C                   MOVEL     '2'           WTRC
320100950515     C*
320200980210    3C     *IN19         IFEQ      *ON
320300950515     C                   MOVEL     '1'           WTRC
320400980210     C                   ELSE
320500980210     C     WLNP          CHAIN     AZORG01L                           18
320600980210    4C     ORGDIT        IFEQ      'SDI'
320700980210     C                   MOVEL     '3'           WTRC
320800980210    4C                   ENDIF
320900980210    3C                   ENDIF
321000931214     C*
321100950110     C     KFVD2         CHAIN     FNFVD000                           18
321200950110     C   18              CLEAR                   FNFVD000
321300950110     C*
321400950110     C* COLLI
321500950110     C                   ADD       NCLU          FVDCLA
321600931026     C*
321700950515     C* SE TROVATA LA BOLLA CALCOLO PER BENE IL PARTITO
321800980210    3C     WTRAN         IFEQ      'S'
321900970221     C     *IN19         OREQ      *ON
322000970221     C     *IN20         ANDEQ     *OFF
322100950515     C                   EXSR      RIEFVD
322200980210    3C                   ENDIF
322300931022     C*
322400980210    3C     *IN18         IFEQ      *ON
322500931022     C* SOLO PER IMMISSIONE
322600931022     C                   Z-ADD     FGVLNP        FVDLNP
322700931022     C                   Z-ADD     FGV2          FVDNFV
322800931022     C                   Z-ADD     SAVLNA        FVDLNA
322900931022     C                   Z-ADD     COMLIN        FVDFLP
323000950110     C                   MOVEL     WTRC          FVDTRC
323100931022     C*
323200950110     C                   WRITE     FNFVD000
323300931022     C*
323400931022     C                   ELSE
323500931022     C*
323600931022     C                   MOVEL     ' '           FVDFTR
323700931022     C                   Z-ADD     0             FVDDTR
323800931022     C*
323900950110     C                   UPDATE    FNFVD000
324000980210    3C                   ENDIF
324100931022     C*
324200980210    2C                   ENDIF
324300980210    1C                   ENDIF
324400931022     C                   ENDSR
324500931022     C*
324600950110     C*--- RIEMPO CAMPI DI FNFVD -------------------------------------*
324700931022     C     RIEFVD        BEGSR
324800931022     C***
324900931022     C***  19 OFF  -   T  R  A  N  S  I  T  I
325000931022     C***
325100950515     C     WTRAN         IFEQ      'S'
325200930708     C* PESO
325300950113     C     BTPPKB        IFGT      3500
325400030213      * Peso medio
325500030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
325600950123     C                   ADD       PKGU          FVDKOA
325700930708     C                   ELSE
325800030213      * Peso medio
325900030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
326000950123     C                   ADD       PKGU          FVDKFA
326100930708     C                   ENDIF
326200950110     C* VOLUME
326300950110     C                   Z-ADD     1             Z                 3 0
326400950110     C     BTPTSP        LOOKUP    TSP(Z)                                 31
326500950110     C*
326600950110     C* NON TROVATO TIPO SERVIZIO --> LO IMPOSTO IN SCHIERA
326700950110     C     *IN31         IFEQ      *OFF
326800950110     C                   Z-ADD     1             Z
326900950110     C     ' '           LOOKUP    TSP(Z)                                 31
327000950110     C   31              MOVEL     BTPTSP        TSP(Z)
327100950110     C  N31              Z-ADD     1             Z
327200950110     C                   ENDIF
327300950110     C*
327400030213      * Volume medio
327500030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
327600950110     C                   ADD       VOLU          VOL(Z)
327700930708     C*
327800950110     C                   ADD       WSPET         FVDSNA
327900931022     C*
328000931022     C                   ELSE
328100931022     C***
328200970221     C***     A  R  E  A
328300931022     C***
328400930709     C*
328500930708     C* PESO
328600130530     C**   BLPPKF        IFGT      3500
328700130530     c                   if        wfvdpes>3500
328800030213      * Peso medio
328900030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
329000931022     C                   ADD       PKGU          FVDKOA
329100931022     C                   ELSE
329200030213      * Peso medio
329300030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
329400931022     C                   ADD       PKGU          FVDKFA
329500930708     C                   ENDIF
329600930708     C*
329700950110     C* VOLUME
329800950110     C                   Z-ADD     1             Z                 3 0
329900950110     C     BLPTSP        LOOKUP    TSP(Z)                                 31
330000950110     C*
330100950110     C* NON TROVATO TIPO SERVIZIO --> LO IMPOSTO IN SCHIERA
330200950110     C     *IN31         IFEQ      *OFF
330300950110     C                   Z-ADD     1             Z
330400950110     C     ' '           LOOKUP    TSP(Z)                                 31
330500950110     C   31              MOVEL     BLPTSP        TSP(Z)
330600950110     C  N31              Z-ADD     1             Z
330700950110     C                   ENDIF
330800950110     C*
330900030213      * Volume medio
331000030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
331100950110     C                   ADD       VOLU          VOL(Z)
331200950110     C                   ADD       WSPEN         FVDSNA
331300931022     C*
331400931022     C                   ENDIF
331500930708     C                   ENDSR
331600930708     C*
331700930709     C*--- CHAIN TABEL -----------------------------------------------*
331800930709     C     CTABEL        BEGSR
331900931021     C* EFFETTUO LA CHAIN IN TABEL SOLO SE CAMBIA IL TIPO BOLLA
332000950109     C     SAVCBO        IFNE      BLPCBO
332100930709     C                   MOVEL     '3A'          COD
332200930709     C                   MOVEL     *BLANKS       KEY
332300950109     C                   MOVEL     BLPCBO        KEY
332400930709     C     KTAB          CHAIN     TABEL                              31
332500931202     C     *IN31         IFEQ      *OFF
332600931202     C                   MOVEL     TBLUNI        DS3A
332700931202     C                   MOVEL     §3ATB1        COMTB1            1
332800931202     C                   ELSE
332900931202     C                   MOVE      ' '           §3ARBL
333000950413     C                   MOVEL     ' '           §3ABSD
333100950413     C                   MOVEL     ' '           §3ABFI
333200931202     C                   ENDIF
333300931202     C*
333400950109     C                   MOVEL     BLPCBO        SAVCBO
333500930709     C                   ENDIF
333600930709     C                   ENDSR
333700950113     C*
333800950113     C* RITRASMETTO IL RECORD TRANSITO ------------------------------*
333900950113     C     WTRBTP        BEGSR
334000950113     C                   MOVEL     BTPAAS        BLPAAS
334100950113     C                   MOVEL     BTPLNP        BLPLNP
334200950113     C                   MOVEL     BTPNRS        BLPNRS
334300950113     C                   MOVEL     BTPNSP        BLPNSP
334400950113     C                   MOVEL     BTPMGS        BLPMGS
334500950113     C                   MOVEL     BTPCBO        BLPCBO
334600950113     C                   MOVEL     BTPFNS        BLPFNS
334700950113     C                   MOVEL     BTPLNA        BLPLNA
334800950113     C                   MOVEL     BTPZNC        BLPZNC
334900950113     C                   MOVEL     BTPRSM        BLPRSM
335000950113     C                   MOVEL     BTPRSD        BLPRSD
335100950113     C                   MOVEL     BTPTSP        BLPTSP
335200950113     C                   MOVEL     BTPNCL        BLPNCL
335300950113     C                   MOVEL     BTPPKB        BLPPKB
335400950113     C                   MOVEL     BTPFVB        BLPFVB
335500950113     C                   MOVEL     BTPVLB        BLPVLB
335600950113     C                   MOVEL     BTPFLS        BLPFLS
335700950113     C                   MOVEL     BTPNCD        BLPNCD
335800950113     C                   MOVEL     BTPNCA        BLPNCA
335900950113     C                   MOVEL     BTPXCO        BLPXCO
336000950113     C                   MOVEL     BTPDBR        BLPDBR
336100021127     C                   MOVEL     BTPFLE        BLPFLE
336200950113     C                   MOVEL     BTPTFP        BLPTFP
336300950113     C                   MOVEL     BTPFEA        BLPFEA
336400950113     C                   MOVEL     BTPTFA        BLPTFA
336500091027     c* nei campi BLPNFV e BLPFLE imposto il foglio di partenza dei
336600091027     c*  colli per poter leggere queste bolle nel pgm della statistica
336700091027     c*    MC in arrivo
336800091027     c                   eval      blpnfv=fgv2
336900091027     c                   eval      blpfle=fgvlnp
337000091027     c
337100950113     C                   WRITE     FNBLPT00
337200950113     C                   ENDSR
337300911004     C**-----------------------------------------------*
337400911004     C** TRASFERISCO FOGLI VIAGGIO
337500911004     C**-----------------------------------------------*
337600911004     C     FVIAGG        BEGSR
337700150730     C                   Z-ADD     1             wnpg
337800150730     C                   Z-ADD     1             B
337900911004     C                   Z-ADD     0             X
338000930929     C                   CLEAR                   LP2
338100911106     C                   EXSR      RIEFVA
338200020403     c                   time                    w0060             6 0
338300911004     C*
338400940620    1C     FLP(B)        DOWGT     *ZEROS
338500920928     C*
338600960212     C* CONTROLLO SE LA SUA LINEA DI ARRIVO HA SPUNTE O BOLLE
338700960202     C                   MOVE      FFV(B)        BRVLNA
338800070123     C     KBRC2         SETLL     FNBRV02                                30
338900130530     C  N30k03BLP77      setll     FNBLP77L                               30
339000960402     C   30              MOVEL     'S'           WBOL              1
339100960402     C  N30              MOVEL     ' '           WBOL              1
339200920928     C*
339300960212     C* PER I FV NON ABBINATI DA NON TRASMETTERE IN SEDE, INVIO SOLO
339400960212     C*  SE HO BOLLE O SPUNTE
339500960402    2C     WBOL          IFEQ      'S'
339600960212     C     FGVTRN        ORGT      0
339700960212     C     §TVUTC        OREQ      'S'
339800040329     c* escludo fogli viaggio poste locali che non sono da trasmettere
339900040329     c* in arrivo
340000040329   2bc     wfvpt         ifne      'S'
340100040329     c     fgvttr        orne      'L'
340200020403     c*
340300020403     c                   add       1             w0060             6 0
340400960212     C*
340500911004     C                   MOVEL     FLP(B)        COMLIN
340600040329     C                   CLEAR                   fnlv54ds
340700040317     C****               MOVEL     'P'           D55TPT
340800040329     c                   z-add     fgvlnp        i54lnp
340900040329     C* Se fv poste gli passo il p.o. poste
341000040329     C     WFGW          IFEQ      'S'
341100040329     C     FGWFGS        ANDGT     *ZEROS
341200040329     C                   Z-ADD     FGWFGS        i54LNP
341300040329     C                   ENDIF
341400040329     C                   MOVEL     COMLIN        i54LIN
341500040329     C                   MOVEL     FGVDFV        i54drf
341600040329     C                   CALL      'FNLV54R'
341700040329     C                   PARM                    fnlv54ds
341800040329    3C     o54ERR        IFEQ      *BLANKS
341900920928     C*
342000911004     C* SE NON LO HO GIA' INVIATO INVIO
342100921202     C     COMLIN        LOOKUP    LP2                                    20
342200940620    4C  N20              DO
342300040826     C                   ADD       1             X                 4 0
342400921202     C                   MOVEL     COMLIN        LP2(X)
342500921202     C*
342600921202     C* TRASMETTO SE NON E' LOCALE
342700040329    5C     o54tfp        IFNE      FGVLNP
342800040329     c     §tvdef        andne     'S'
342900040329     c     fgvlnp        orne      o54tfa
343000911004     C*
343100040329     C                   MOVEL     o54TFP        VARX
343200921202     C                   EXSR      CAEXFV
343300020403     c*
343400911004     C*
343500960126     C* TRASMETTO SOLO SE FV APERTO O SFLAGGATO(DA TRASMETTERE)
343600960126    6C     FGVFCF        IFEQ      ' '
343700960126     C     FGVFTR        OREQ      ' '
343800950113     C                   MOVEL     ' '           FVAATR
343900921202     C                   MOVEL     COMLIN        FVALAI
344000950113     C                   WRITE     FNFVA0TT
344100000428    7C     WFGW          IFEQ      'S'
344200000428     C                   MOVEL     COMLIN        FWALAI
344300000428     C                   WRITE     FNFWA000
344400000428    7C                   ENDIF
344500060710     c* Scrittura del file FNFV4
344600060710     c                   EXSR      SCRIVIFV4
344700020807    6C                   ENDIF
344800010622      *
344900940620    5C                   ENDIF
345000940620    4C                   ENDDO
345100940620    3C                   ENDIF
345200920427     C*
345300940620     C* R I T R A S M E T T O   F O G L I O   X   T R A N S I T I  SE:
345400940620     C*
345500971105     C* 1) SE NON E' UN FOGLIO LOCALE (TFP DELLO SCARICO=FGVLNP)
345600940620     C*    E LA LINEA ARRIVO E' DIVERSA DALLO SCARICO
345700960402     C*    E SOLO SE HA BOLLE O SPUNTE
345800050927     C*    oppure locale con spunte transito
345900020807    3C     FFV(B)        IFNE      FLP(B)
346000960402     C     WBOL          ANDEQ     'S'
346100050927     c     winvloc(B)    oreq      'S'
346200921202     C*
346300920427     C                   MOVEL     FFV(B)        COMLNA
346400970516     C* Richiamo pgm per determinare terminal di arrivo e partenza
346500970703     C* della lna
346600031110     C                   CLEAR                   fnlv54ds
346700031112     C                   MOVE      COMLNA        i54LIN
346800031112     C                   MOVE      FGVLNP        i54LNP
346900000620     C* Se fv poste gli passo il p.o. poste
347000000620     C     WFGW          IFEQ      'S'
347100000620     C     FGWFGS        ANDGT     *ZEROS
347200031112     C                   Z-ADD     FGWFGS        i54LNP
347300000620     C                   ENDIF
347400031112     C                   MOVE      FGVDFV        i54DRF
347500031110     C                   CALL      'FNLV54R'
347600031110     C                   PARM                    fnlv54ds
347700921202     C*
347800940620     C*
347900050203     C* 2) SE TFP DELLA LINEA ARRIVO NON E' LOCALE
348000031110    4C     O54TFP        IFNE      FGVLNP
348100040317     c     fgvlnp        orne      o54tfa
348200050203     c
348300050203     c* 3) Trasmetto anche il foglio locale se presenti delle spunte
348400050203     c*    transito
348500050203     c     winvloc(B)    oreq      'S'
348600940620     C*
348700130531     c* Trasmettere
348800130531     c*  sempre al terminal di arrivo, tranne quando ci sono  spunte
348900050203     c*  transito su un foglio locale (vale solo per le  linee di arriv
349000050203     c*  un po' terminal arrvioe un po' 2 livello in arrivo)
349100980126     C*
349200050203     C**   WINV          IFEQ      'T'
349300050203     c                   if        winvloc(B)=' '
349400031110     C                   MOVEL     o54TFA        WLNA
349500980126     C                   ELSE
349600980126     C                   MOVEL     COMLNA        WLNA
349700980126     C                   ENDIF
349800921118     C*
349900050203     C* 3) SE NON L'HO GIA'INVIATO
350000980126     C     WLNA          LOOKUP    LP2                                    20
350100940620    6C  N20              DO
350200921118     C                   ADD       1             X
350300980126     C                   MOVEL     WLNA          LP2(X)
350400921118     C*
350500050203     c* Devo trasmettere il fogio nel membro del terPaart del TerARR
350600050203     c*  dell alna, tranne se foglio locale con invio spunte transito
350700050203     c     winvloc(B)    ifeq      ' '
350800040217     C     O54TFP        andne     O54TFA
350900040218     c                   move      o54tfa        wtfa
351000130716     C*****              CLEAR                   dslv55
351100130716     c*****              movel     'P'           d55tpt
351200130716     C*****              MOVE      wtfa          d55LIN
351300130716     C*****              MOVE      FGVDFV        d55DRF
351400130716     C*****              CALL      'FNLV55R'
351500130716     C*****              PARM                    dslv55
351600130716     C                   CLEAR                   fnlv54ds
351700130716     c                   movel     'P'           i54tpt
351800130716     C                   MOVE      wtfa          i54LIN
351900130716     C                   MOVE      FGVDFV        i54DRF
352000130716     C                   CALL      'FNLV54R'
352100130716     C                   PARM                    fnlv54ds
352200040217     c                   endif
352300040217     c*
352400130716     C****               MOVEL     d55TFP        VARX
352500130716     C                   MOVEL     o54TFP        VARX
352600920427     C                   EXSR      CAEXFV
352700921202     C*
352800960126     C* TRASMETTO SOLO SE FV APERTO O SFLAGGATO(CIOE' DA TRASMETTERE)
352900960126    7C     FGVFCF        IFEQ      ' '
353000960126     C     FGVFTR        OREQ      ' '
353100950113     C                   MOVEL     'T'           FVAATR
353200980126     C                   MOVEL     WLNA          FVALAI
353300950113     C                   WRITE     FNFVA0TT
353400020807    8C     WFGW          IFEQ      'S'
353500000428     C                   MOVEL     WLNA          FWALAI
353600000428     C                   WRITE     FNFWA000
353700020807    8C                   ENDIF
353800060710     c
353900060710     c* non mi serve più scrivere fnfv4 in quanto ho già scritto prima
354000060710     c*  il file fisico, unico, che trasmette eventualente anche
354100060710     c*  in sede (una sola volta quindi basta)
354200060710     c*********          EXSR      SCRIVIFV4
354300060710    7C                   ENDIF
354400010622      *
354500940620    6C                   ENDDO
354600940620    4C                   ENDIF
354700040329    3c                   ENDIF
354800040329   2bc                   endif
354900960212    2C                   ENDIF
355000911004     C*
355100911004     C                   ADD       1             B
355200940620    1C                   ENDDO
355300911004     C*
355400951120     C* SCRIVO FILE DI APPOGGIO DA INVIARE IN SEDE SE:
355500951120     C*   - IL FOGLIO NON E' ABBINATO
355600951120     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' VUOTO
355700951120     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO ED E'
355800951120     C*     DA UTILIZZARE NEI CONTROLLI DI SEDE (§TVUTS = "S")
355900951120     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO MA NON
356000951120     C*     C'E' IL RECORD IN TABELLA
356100950113     C                   MOVEL     ' '           FVAATR
356200951120     C*
356300960212    1C     FGVTRN        IFEQ      0
356400960212    2C     FGVTTR        OREQ      ' '
356500960212    3C     §TVUTC        OREQ      'S'
356600950113     C                   WRITE     FNFGV046
356700951120    1C                   ENDIF
356800930709     C*
356900911004     C                   ENDSR
357000910910     C**-----------------------------------------------*
357100950516     C** SCARICO SPUNTE PER L'AS DELLA LNA
357200910910     C**-----------------------------------------------*
357300910912     C     WTRLNA        BEGSR
357400910910     C*
357500050204     c* Secondo me questo IF si può eliminare ..... (ES)
357600040317    4C     lnafel        IFne      FGVLNP
357700040317     c     fgvlnp        orne      lnatfa
357800960604     C*
357900960806     C                   MOVEL     'N'           WALCF
358000930520     C*
358100040217     c* se transito e ter-arr della lna e lna sono sullo stesso ogas
358200040217     c* trasmetto a ter-part del ter-arr di lna
358300040217     c     *in06         ifeq      *off
358400000725     C                   MOVEL     LNAFEL        VARX
358500040217     c                   else
358600130531     C                   CLEAR                   dslv55
358700130531     C                   MOVEL     'P'           d55TPT
358800130531     C                   z-add     lnatfa        d55LIN
358900130531     C                   Z-ADD     FGVDFV        d55DRF
359000130531     c                   call      'FNLV55R'
359100130531     c                   parm                    dslv55
359200130531     C                   MOVEL     d55tfp        VARX
359300040217     c                   endif
359400910910     C                   EXSR      CAEXSP
359500910910     C*
359600950113     C                   MOVEL     BRVLNA        COMLNA            3 0
359700950113     C                   MOVEL     SAVLNA        BRVLNA
359800070123     C     KBRC2         SETLL     FNBRV02
359900070123     C     KBRC2         READE(N)  FNBRV02                                29
360000910910     C*
360100960205    2C     *IN29         DOWEQ     '0'
360200000111     C                   CLEAR                   FISPP000
360300910910     C*
360400960604     C                   MOVEL     BRVLNP        KLNP
360500960604     C                   MOVEL     BRVLNA        KLNA
360600960604     C                   MOVEL     BRVNRS        KNRS
360700960604     C                   MOVEL     BRVNSC        KNSC
360800960604     C*
360900960604     C                   EXSR      RICLNP
361000960604     C*
361100960604     C     WLNP          LOOKUP    L1                                     20
361200940322     C*
361300940322     C* SPUNTA PER TERMINAL
361400960205    3C     *IN08         IFEQ      *ON
361500940322     C* SPUNTA CHE TRANSITO
361600960205    4C     *IN20         IFEQ      *OFF
361700970814     C     WTAB5F        ANDEQ     ' '
361800950113     C*
361900960604     C                   SELECT
362000960611     C     WOKBT         WHENEQ    *BLANKS
362100020606     C     KBtt37        SETLL     FNBTT37L                               34
362200960611     C     WOKBT         WHENEQ    'S'
362300960604     C                   SETON                                        34
362400960604     C                   ENDSL
362500971105     C                   MOVEL     FGVLNP        SPPFLP
362600950515     C* 34 ON - USCITA DA TRANSITO
362700960205    5C     *IN34         IFEQ      *ON
362800950515     C                   MOVEL     'T'           SPPATR
362900960205   X5C                   ELSE
363000990819     C* DEFLUENZA
363100990819     C* Se filiale gestione spunta è anche il terminal di arrivo
363200990819     C* non devo trasmettere la spunta come disguido ma come defluenza
363300990819     C* (N.B.: caso di Padova che ha spuntato colli con lnp 007 ed lna
363400990819     C* 059 sul foglio viaggio locale anzichè sulla defluenza)
363500990819     C     BRVFGS        IFEQ      LNATFA
363600990819     C                   MOVEL     'F'           SPPATR
363700990819     C                   ELSE
363800940322     C* DISGUIDO
363900960604     C                   MOVEL     'D'           SPPATR
364000990819     C                   ENDIF
364100960205    5C                   ENDIF
364200950515     C*
364300960205   X4C                   ELSE
364400960205     C                   MOVEL     ' '           SPPATR
364500950515     C                   CLEAR                   SPPFLP
364600960205    4C                   ENDIF
364700940322     C*
364800940322     C                   EXSR      RIESPP
364900000111     C                   WRITE     FISPP000
365000960205    3C                   ENDIF
365100940322     C*
365200940322     C* SPUNTA ATTESA TRANSITO
365300980210    3C     *IN06         IFEQ      *ON
365400950515     C*
365500960205    4C     *IN20         IFEQ      *ON
365600970814     C     WTAB5F        OREQ      'S'
365700940322     C                   MOVEL     'A'           SPPATR
365800950215     C                   MOVEL     COMLIN        SPPFLP
365900940322     C                   EXSR      RIESPP
366000000111     C                   WRITE     FISPP000
366100960205    4C                   ENDIF
366200960205    3C                   ENDIF
366300910910     C*
366400070123     C     KBRC2         READE(N)  FNBRV02                                29
366500960205    2C                   END
366600910910     C*
366700940322     C* MI RIPOSIZIONO SULLA NUOVA LINEA ARRIVO LETTA
366800970814    2C     WLAST         IFEQ      *BLANKS
366900950113     C                   MOVEL     COMLNA        BRVLNA
367000070123     C     KBRC2         SETLL     FNBRV02
367100070123     C     KBRC2         READE     FNBRV02                                22
367200960708     C*
367300970814    3C     *IN22         IFEQ      *OFF
367400960708     C                   MOVE      'S'           WALCF
367500960708     C                   MOVEL     BRVLNP        KLNP
367600960708     C                   MOVEL     BRVLNA        KLNA
367700960708     C                   MOVEL     BRVNRS        KNRS
367800960708     C                   MOVEL     BRVNSC        KNSC
367900960708     C                   EXSR      RICLNP
368000970814    3C                   END
368100970814    2C                   END
368200021127    1C                   END
368300910910     C*
368400910910     C                   ENDSR
368500910911     C**-----------------------------------------------*
368600910911     C** CREO SPUNTE PER AUTOSPUNTA
368700910911     C**-----------------------------------------------*
368800910911     C     CRTSPU        BEGSR
368900140618     C                   CLEAR                   FNBRV05
369000950109     C                   MOVEL     BLPFLS        BRVLNP
369100950109     C                   MOVEL     BLPLNA        BRVLNA
369200950109     C                   MOVEL     BLPNRS        BRVNRS
369300950109     C                   MOVEL     BLTNSC        BRVNSC
369400950109     C                   MOVEL     BLPZNC        BRVZNC
369500950109     C                   MOVEL     '90'          BRVNPS
369600950109     C                   Z-ADD     WNPG          BRVNPG
369700000111     C                   Z-ADD     FGV2          BRVNFV
369800000111     C                   Z-ADD     FGVLNP        BRVPES
369900971105     C                   MOVEL     FGVLNP        BRVFGS
370000971107     C                   MOVEL     FGVLNP        BRVFLE
370100950109     C                   Z-ADD     BLTVUC        BRVVUC
370200990906     C                   Z-ADD     BLTPUC        BRVPUC
370300950109     C                   MOVEL     BLTCAN        BRVCAN
370400070219     C***                TIME                    W0140            14 0
370500070219     C***                MOVEL     W0140         BRVHMS
370600070219     C***                CLEAR                   WLBDAT
370700070219     C***                MOVE      W0140         G02DAT
370800070219     C***                CALL      'XSRDA8'
370900070219     C***                PARM                    WLBDAT
371000070219     C***                Z-ADD     G02INV        BRVDFS
371100070219     c                   time                    wtimez
371200070219     c* millisecondi
371300070219     c                   extrct    wtimez:*ms    millisec          6 0
371400070219     c                   movel     millisec      brvmis
371500070219     c* data immissione
371600070219     c                   movel     wtimez        dataiso
371700070219     c                   movel     dataiso       brvdfs
371800070219     c* ora immissione
371900070219     c                   move      wtimez        oraiso
372000070219     c                   movel     oraiso        brvhms
372100070219     c                   movel     'A'           brvtap
372200070219     c                   eval      brvpru='§'+ nompgm
372300910911     C*
372400140618     C                   WRITE     FNBRV05
372500910911     C                   ENDSR
372600910911     C**-----------------------------------------------*
372700910911     C** CREO RECORD ANOMALIA
372800910911     C**-----------------------------------------------*
372900910911     C     CRTANM        BEGSR
373000921022     C                   MOVEL     *BLANKS       ANMATB
373100960514     C                   MOVE      BRVFGS        ANMCDU
373200921022     C                   Z-ADD     0             ANMAAS
373300990517     C                   Z-ADD     0             ANMDUE
373400990517     C                   Z-ADD     0             ANMNPG
373500990517     C                   Z-ADD     0             ANMFGS
373600990518     C                   Z-ADD     0             ANMFL3
373700040531     C                   Z-ADD     999           ANMFL4
373800990518     C                   Z-ADD     0             ANMDTR
373900990518     C                   MOVE      *BLANK        ANMFLO
374000040603     c* data ora di creazione
374100050707     c***                z-add     *date         wdata             6 0
374200050707     c***                time                    wora              6 0
374300050707     c
374400050707     C                   TIME                    W0140            14 0
374500050707     C                   CLEAR                   WLBDAT
374600050707     C                   MOVE      W0140         G02DAT
374700050707     C                   CALL      'XSRDA8'
374800050707     C                   PARM                    WLBDAT
374900050707     C                   Z-ADD     G02INV        Wdata             6 0
375000050707     c                   movel     w0140         wora4             4 0
375100040603     c                   movel     wdata         anmflo
375200040603     c                   move      wora4         anmflo
375300040531     c
375400040531     C**   COMCAA        IFEQ      105
375500040531     c* la zona la appoggio ora in anmfl4
375600040531     C**                 CLEAR                   DANM01
375700040531     C**                 MOVE      BRVZNC        §ANZNC
375800040531     C**                 MOVEL     DANM01        ANMFLO
375900040531     C**                 ENDIF
376000040531     c                   z-add     brvznc        anmfl4
376100040531     c
376200990518     C                   MOVE      *BLANK        ANMFT3
376300040603     c
376400110909     C                   MOVE      *BLANK        ANMFT4
376500110909     C                   Z-ADD     0             ANMLNP
376600110909     C                   Z-ADD     0             ANMNSP
376700110909     c
376800110909     c* imposto dati bolle per anomalie 110 e 115
376900110909     c* flag di valore
377000110909     c                   if        comcaa=110  or comcaa=115
377100040603     c                   movel     §ar5bva       anmft4
377200110909     c                   eval      anmaas=bltaas
377300110909     c                   eval      anmlnp=bltlnp
377400110909     c                   eval      anmnsp=bltnsp
377500040603     c                   endif
377600110909
377700921022     C                   MOVEL     '1'           ANMFSA
377800950327     C                   MOVE      WDTP          ANMDAO
377900921022     C                   MOVEL     *BLANKS       ANMCCH
378000971107     C                   MOVEL     FGVLNP        ANMFLE
378100921022     C                   MOVEL     *BLANKS       ANMFT1
378200921022     C                   Z-ADD     0             ANMFL1
378300921022     C                   MOVEL     *BLANKS       ANMFT2
378400921022     C                   Z-ADD     0             ANMFL2
378500921022     C*
378600950113     C                   MOVEL     BRVLNP        ANMFLS
378700950113     C                   MOVEL     BRVLNA        ANMLNA
378800950113     C                   MOVEL     BRVNRS        ANMNRS
378900950113     C                   MOVEL     BRVNSC        ANMSCN
379000950113     C                   MOVEL     BRVNPS        ANMNPS
379100950113     C                   Z-ADD     BRVNFV        ANMNFV
379200911018     C                   Z-ADD     COMCAA        ANMCAA
379300911018     C                   MOVEL     '7C'          COD
379400911018     C                   MOVEL     *BLANKS       KEY
379500911018     C                   MOVEL     COMCAA        KEY
379600911018     C     KTAB          CHAIN     TABEL                              21
379700911018     C  N21              MOVEL     TBLUNI        DS7C
379800911018     C   21              MOVEL     *BLANKS       DS7C
379900911018     C*
380000911018     C                   MOVEL     §7CTAN        ANMTAN
380100911018     C                   MOVEL     §7CSAN        ANMSAN
380200911018     C                   MOVEL     §7CAIE        ANMAIE
380300911018     C                   Z-ADD     §7CLID        ANMLID
380400910911     C                   Z-ADD     0             ANMDCH
380500911011     C                   MOVEL     *BLANKS       ANMFSC
380600911018     C     §7CCHA        IFEQ      'S'
380700950327     C                   MOVE      WDTP          ANMDCH
380800911018     C                   MOVEL     '1'           ANMFSC
380900911018     C                   END
381000910911     C*
381100990517     C                   WRITE     FNANM
381200910911     C                   ENDSR
381300910911     C**-----------------------------------------------*
381400910911     C** CHIUSURA ANOMALIE MANCA BOLLA
381500910911     C**-----------------------------------------------*
381600910911     C     CHIUAN        BEGSR
381700050927     C                   CLEAR                   WESISTEANM        1
381800990517     C     KANM3         CHAIN(N)  FNANM                              21
381900960416     C     *IN21         IFEQ      *OFF
382000130607     c                   exsr      call_lr33
382100050927     C                   eval      wesisteanm='S'
382200050927     C                   END
382300921014     C*
382400910911     C                   ENDSR
382500130607     C**-----------------------------------------------*
382600130607     C** chiamata a FNLR33R
382700130607     C**-----------------------------------------------*
382800130607     C     call_lr33     BEGSR
382900130607     C                   CLEAR                   DSLR33
383000130607     C                   MOVE      WDTP          D33DCH
383100130607     C                   MOVEL     '1'           D33FSC
383200130607     C                   MOVEL     '  '          D33CCH
383300130607     c                   if        comcaa=110  or comcaa=105
383400130607     C                   MOVEL     'BO'          D33CCH
383500130607     c                   endif
383600130607     C                   MOVE      ANMNR2        D33NRR
383700130607     C                   CALL      'FNLR33R'
383800130607     C                   PARM                    DSLR33
383900130607     c                   ENDSR
384000920324     C**-----------------------------------------------*
384100920324     C** ALLOCO MEMBRI DI TRASMISSIONE
384200920324     C**-----------------------------------------------*
384300920324     C     ALLOCO        BEGSR
384400950109     C* CARICO FILIALI ELABORATORE LINE FV
384500920324     C                   EXSR      RIEMPI
384600920324     C* ALLOCO MEMBRI DELLE LINEE DI ARRIVO
384700920324     C                   EXSR      LCKMBR
384800920324     C* ALLOCO MEMBRI LINEE DI TRANSITO
384900950915     C  N50              EXSR      LCKTRN
385000920526     C* NON RIESCO AL ALLOCARE: RISOTTOMETTO 3 VOLTE
385100950915     C   50              EXSR      ERROR
385200920324     C*
385300920324     C                   ENDSR
385400920324     C**-----------------------------------------------*
385500971105     C** CARICO TERMINAL DI PARTENZA DELLE LINEE DEL FV
385600920324     C**-----------------------------------------------*
385700920324     C     RIEMPI        BEGSR
385800920324     C*
385900921202     C                   Z-ADD     1             K                 3 0
386000920324     C                   Z-ADD     1             I                 3 0
386100920324     C                   Z-ADD     1             Y                 3 0
386200920324     C*
386300921202     C     FFV(I)        DOWGT     *ZEROS
386400920324     C                   MOVEL     FFV(I)        SAVFFV            3 0
386500971104     C*
386600971104     C                   CLEAR                   DSLV55
386700971104     C                   MOVEL     'P'           D55TPT
386800971104     C                   Z-ADD     SAVFFV        D55LIN
386900971105     C                   Z-ADD     FGVDFV        D55DRF
387000971104     C                   CALL      'FNLV55R'
387100971104     C                   PARM                    DSLV55
387200971104     C     D55TFP        LOOKUP    FEL                                    30
387300971104     C  N30              Z-ADD     D55TFP        FEL(K)
387400920324     C  N30              ADD       1             K
387500920324     C* FILIALI DI TRANSITO
387600920324     C     FLP(I)        IFGT      *ZEROS
387700920324     C                   MOVEL     FLP(I)        SAVFLP            3 0
387800920324     C* C'E' TRANSITO SE NON E' LA FIL DI ARRIVO
387900920324     C*                         LA SUA FILIALE ELABORATORE
388000930312     C*   SE E' IL SUO TERMINAL ALLOCO LO STESSO
388100920324     C     SAVFFV        IFNE      SAVFLP
388200960325     C*
388300971105     C     D55TFP        ANDNE     SAVFLP
388400930312     C*
388500971105     C                   CLEAR                   DSLV55
388600971105     C                   MOVEL     'P'           D55TPT
388700971105     C                   Z-ADD     SAVFLP        D55LIN
388800971105     C                   Z-ADD     FGVDFV        D55DRF
388900971105     C                   CALL      'FNLV55R'
389000971105     C                   PARM                    DSLV55
389100950215     C*
389200971105     C* SE IL TERMINAL DI PARTENZA NON E' FGVLNP
389300971105     C     D55TFP        IFNE      FGVLNP
389400971105     C     D55TFP        LOOKUP    FET                                    30
389500971105     C  N30              Z-ADD     D55TFP        FET(Y)
389600920324     C  N30              ADD       1             Y
389700920324     C                   END
389800950215     C                   END
389900920324     C                   END
390000921202     C*
390100921202     C                   ADD       1             I
390200920324     C                   END
390300050524     c
390400050524     c* Controllo riempimento schiera
390500050524     c                   clear                   trul0sds
390600050524     c                   eval      i0sski='FEL'
390700050524     c                   eval      i0sele=%elem(fel)
390800050524     c                   eval      i0spie=k-1
390900050524     c                   eval      i0sfile='TerPar LNA'
391000050524     c                   eval      i0ssif=knsif
391100130718     c                   eval      i0spgm='FNLSU2R'
391200050524     c                   movel     trul0sds      kpjbu
391300050524     c                   call      'TRUL0SR'
391400050524     c                   parm                    kpjba
391500050524     c
391600050524     c* Controllo riempimento schiera
391700050524     c                   clear                   trul0sds
391800050524     c                   eval      i0sski='FET'
391900050524     c                   eval      i0sele=%elem(fet)
392000050524     c                   eval      i0spie=y
392100050524     c                   eval      i0sfile='TerPar FLP'
392200050524     c                   eval      i0ssif=knsif
392300130718     c                   eval      i0spgm='FNLSU2R'
392400050524     c                   movel     trul0sds      kpjbu
392500050524     c                   call      'TRUL0SR'
392600050524     c                   parm                    kpjba
392700920324     C                   ENDSR
392800920324     C**-----------------------------------------------*
392900920324     C** ESEGUE CALL QCAEXEC SUL MEMBRO DELLE BOLLE    *
393000920324     C**-----------------------------------------------*
393100920324     C     CRTMBR        BEGSR
393200930127     C*
393300930127     C                   CALL      'TRUL50C'
393400930127     C                   PARM                    FISICO           10
393500930127     C                   PARM                    FISMBR           10
393600000428     C                   PARM                    O50PLA
393700930127     C                   PARM                    LOGICO           10
393800930127     C                   PARM                    LOGMBR           10
393900930127     C                   PARM                    ULFLG             1
394000930127     C* RITENTO L'ALLOCAZIONE
394100950915     C                   CALL      'QCMDEXC'                            50
394200930127     C                   PARM                    COMMAN
394300930127     C                   PARM                    LUNG
394400930127     C*
394500920324     C                   ENDSR
394600920324     C**-----------------------------------------------*
394700920324     C** ALLOCAZIONE MEMBRI
394800920324     C**-----------------------------------------------*
394900920324     C     LCKMBR        BEGSR
395000920324     C                   Z-ADD     1             K
395100950915     C                   SETOFF                                       50
395200021014     C                   MOVE      '))'          VAR2
395300000725     C                   MOVEL     'M'           VAR2
395400000731     C                   MOVEL     'M'           EMME
395500021014     C                   Z-ADD     42            LUNG
395600920324     C*
395700920324     C     FEL(K)        DOWNE     0
395800950109     C* ALLOCO MEMBRO FNBLP
395900000725     C                   MOVEL     FEL(K)        VAR2X
396000930127     C                   MOVEL     FEL(K)        NUM3
396100920324     C**
396200920324     C** SE NON CI SONO BOLLE DA INVIARE E' INUTILE ALLOCARE
396300950115     C* ALLOCAZIONE FILE DELLE BOLLE
396400950115     C   09              EXSR      LCKBLP
396500920324     C**
396600971105     C* ALLOCO SOLO SE IL MEMBRO DA SCRIVERE NON è LOCALE COME TERMINAL
396700971105     C* DI PARTENZA
396800040331    1C  N50              DO
396900960124     C* SE VENGO DA MANUTENZIONE FV NON DEVO ALLOCARE IL FILE SPUNTE
397000960124    2C     PA2FLG        IFNE      'M'
397100960124     C**
397200000111     C                   MOVE      VAR2          CM47
397300920324     C                   MOVEL     *BLANKS       COMMAN           80
397400921202     C                   MOVEA     CM47(1)       COMMAN
397500950915     C                   CALL      'QCMDEXC'                            50
397600920324     C                   PARM                    COMMAN
397700920324     C                   PARM                    LUNG
397800930127     C*
397900930127     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
398000930127     C*  SE MANCA E RITENTO
398100960124    3C     *IN50         IFEQ      '1'
398200000111     C                   MOVEL     'FISPP00T'    FISICO
398300930127     C                   EXSR      CRTMBR
398400960124    3C                   END
398500960124    2C                   ENDIF
398600920324     C**
398700950915    2C  N50              DO
398800951116     C                   MOVE      VAR2          CM49
398900920324     C                   MOVEL     *BLANKS       COMMAN           80
399000921202     C                   MOVEA     CM49(1)       COMMAN
399100950915     C                   CALL      'QCMDEXC'                            50
399200920324     C                   PARM                    COMMAN
399300920324     C                   PARM                    LUNG
399400930127     C*
399500930127     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
399600930127     C*  SE MANCA E RITENTO
399700950915    3C     *IN50         IFEQ      '1'
399800951116     C                   MOVEL     'FNFVA01T'    FISICO
399900930127     C                   EXSR      CRTMBR
400000950115    3C                   END
400100960123     C*
400200960123    3C  N50              DO
400300000428     C                   MOVE      VAR2          CM48
400400000428     C                   MOVEL     *BLANKS       COMMAN           80
400500000428     C                   MOVEA     CM48(1)       COMMAN
400600000428     C                   CALL      'QCMDEXC'                            50
400700000428     C                   PARM                    COMMAN
400800000428     C                   PARM                    LUNG
400900000428     C*
401000000428     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
401100000428     C*  SE MANCA E RITENTO
401200000428    5C     *IN50         IFEQ      '1'
401300000428     C                   MOVEL     'FNFWA00T'    FISICO
401400000428     C                   EXSR      CRTMBR
401500000428    5C                   END
401600960123    3C                   END
401700960123     C*
401800950115    2C                   END
401900950115    1C                   END
402000950115     C**
402100950915     C  N50              ADD       1             K                 3 0
402200950915     C   50              Z-ADD     149           K
402300920324     C                   END
402400920324     C*
402500920324     C                   ENDSR
402600950115     C*
402700950115     C*--------------------------------------
402800950115     C* ALLOCAZIONE FILE DELLE BOLLE
402900950115     C*--------------------------------------
403000950115     C     LCKBLP        BEGSR
403100950115     C*
403200971104     C                   MOVE      VAR2          CM41
403300950115     C                   MOVEL     *BLANKS       COMMAN           80
403400971104     C                   MOVEA     CM41(1)       COMMAN
403500950915     C                   CALL      'QCMDEXC'                            50
403600950115     C                   PARM                    COMMAN
403700950115     C                   PARM                    LUNG
403800950115     C*
403900950115     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
404000030107     C*  SE MANCA E RITENTO
404100950915    1C     *IN50         IFEQ      '1'
404200971104     C                   MOVEL     'FNBLPE0T'    FISICO
404300950115     C                   EXSR      CRTMBR
404400950115    1C                   END
404500950115     C*
404600950115     C                   ENDSR
404700920324     C**-----------------------------------------------*
404800920324     C** ALLOCAZIONE MEMBRI  TRASMISSIONE
404900920324     C**-----------------------------------------------*
405000920324     C     LCKTRN        BEGSR
405100920324     C                   Z-ADD     1             K
405200950915     C                   SETOFF                                       50
405300920324     C     FET(K)        DOWNE     0
405400930310     C*
405500000725     C                   MOVEL     FET(K)        VAR2X
405600930310     C*
405700930310     C                   EXSR      TRANSI
405800930310     C*
405900950915     C  N50              ADD       1             K                 3 0
406000950915     C   50              Z-ADD     149           K
406100920324     C                   END
406200920324     C*
406300920324     C                   ENDSR
406400930310     C**-----------------------------------------------*
406500080423     C** ALLOCAZIONE FILE DEL TRANSITO
406600930310     C**-----------------------------------------------*
406700930310     C     TRANSI        BEGSR
406800000725     C                   MOVEL     VAR2X         NUM3
406900950109     C                   MOVE      VAR2          CM51
407000930310     C                   MOVEL     *BLANKS       COMMAN           80
407100930310     C                   MOVEA     CM51(1)       COMMAN
407200950915     C                   CALL      'QCMDEXC'                            50
407300930310     C                   PARM                    COMMAN
407400930310     C                   PARM                    LUNG
407500930310     C*
407600930310     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
407700930310     C*  SE MANCA E RITENTO
407800950915    1C     *IN50         IFEQ      '1'
407900950109     C                   MOVEL     'FNBLPT0T'    FISICO
408000930310     C                   EXSR      CRTMBR
408100950215    1C                   END
408200930310     C*
408300950915    1C  N50              DO
408400000111     C                   MOVE      VAR2          CM47
408500950215     C                   MOVEL     *BLANKS       COMMAN           80
408600950215     C                   MOVEA     CM47(1)       COMMAN
408700950915     C                   CALL      'QCMDEXC'                            50
408800950215     C                   PARM                    COMMAN
408900950215     C                   PARM                    LUNG
409000950215     C*
409100950215     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
409200950215     C*  SE MANCA E RITENTO
409300950915    3C     *IN50         IFEQ      '1'
409400000111     C                   MOVEL     'FISPP00T'    FISICO
409500950215     C                   EXSR      CRTMBR
409600950215    3C                   END
409700950215     C**
409800950915    3C  N50              DO
409900951116     C                   MOVE      VAR2          CM49
410000950215     C                   MOVEL     *BLANKS       COMMAN           80
410100950215     C                   MOVEA     CM49(1)       COMMAN
410200950915     C                   CALL      'QCMDEXC'                            50
410300950215     C                   PARM                    COMMAN
410400950215     C                   PARM                    LUNG
410500950215     C*
410600950215     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
410700950215     C*  SE MANCA E RITENTO
410800950915    4C     *IN50         IFEQ      '1'
410900951116     C                   MOVEL     'FNFVA01T'    FISICO
411000950215     C                   EXSR      CRTMBR
411100950215    4C                   END
411200950215     C**
411300960123    4C  N50              DO
411400000428     C**
411500000428     C                   MOVE      VAR2          CM48
411600000428     C                   MOVEL     *BLANKS       COMMAN           80
411700000428     C                   MOVEA     CM48(1)       COMMAN
411800000428     C                   CALL      'QCMDEXC'                            50
411900000428     C                   PARM                    COMMAN
412000000428     C                   PARM                    LUNG
412100000428     C*
412200000428     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
412300000428     C*  SE MANCA E RITENTO
412400000428    6C     *IN50         IFEQ      '1'
412500000428     C                   MOVEL     'FNFWA00T'    FISICO
412600000428     C                   EXSR      CRTMBR
412700000428    6C                   END
412800960123    4C                   END
412900960123     C*
413000950215    3C                   END
413100950215    1C                   END
413200930310     C                   ENDSR
413300920526     C*
413400920526     C* RISOTTOMETTO AZIONE PERCHE' NON RIESCO AD ALLOCARE -----------*
413500920526     C     ERROR         BEGSR
413600111019     C     NUM2          IFEQ      2
413700960129     C*
413800960129     C* SE VENGO DA MANUTENZIONE FV NON DEVO STAMPARE
413900111026     C***  PA2FLG        IFNE      'M'
414000111026     C***                OPEN      SYSPRT
414100111026     C***                EXCEPT    TESERR
414200111026     C***                EXCEPT    DETERR
414300111026     C***                CLOSE     SYSPRT
414400111026     C***                ENDIF
414500960129     C*
414600111019     C* SE NON RIESCO AD ALLOCARE INVIO mail a sede
414700111019     C                   movel     *blanks       wrkEml          253
414800111019     C                   movel     *blanks       wrkMsg         5000
414900111019     C                   movel     *blanks       wrkOgg           44
415000111019     c
415100111019     C                   MOVEL     MSG(1)        wrkogg
415200111019     C                   MOVEL(P)  MSG(2)        wrkmsg
415300111019     c                   eval      %subst(wrkmsg:41:3)=%editc(fgvlnp:'X')
415400111019     c                   eval      %subst(wrkmsg:45:6)=%editc(fgvnfv:'2')
415500120302     c                   eval      wrkeml='ced@brt.it'
415600111019     c
415700111019     C                   call(e)   'TIS701C'
415800111019     C                   parm                    wrkEml
415900111019     C                   parm                    wrkOgg
416000111019     C                   parm                    wrkMsg
416100920526     C*
416200920526     C                   ELSE
416300920526     C*
416400920526     C* RISOTTOMETTO L'AZIONE
416500920526     C                   ADD       1             NUM2
416600130524     C                   MOVEL     FNLSU2DS      KPJBU
416700130718     C                   MOVEL     'LSU2'        KCOAZ
416800920526     C                   CALL      'BCH10'
416900920526     C                   PARM                    KPJBA
417000920526     C* E RISOTTOMETTO AZIONE DI INVIO FILE
417100950130     C                   MOVEL     'LSA3'        KCOAZ
417200070423     c                   clear                   parama3
417300070423     c                   move      simfel        pa3tfp
417400070423     c                   movel(p)  parama3       kpjbu
417500931203     C                   CALL      'BCH10'
417600931203     C                   PARM                    KPJBA
417700920526     C                   END
417800920526     C                   ENDSR
417900930127     C**-----------------------------------------------*
418000000111     C** RIEMPO CAMPI DEL FILE FISPP00T
418100930127     C**-----------------------------------------------*
418200930127     C     INVSAI        BEGSR
418300950113     C                   MOVEL     '1'           SPPNPG
418400950113     C                   MOVEL     BRVLNP        SPPLNP
418500950113     C                   MOVEL     BRVLNA        SPPLNA
418600950113     C                   MOVEL     BRVNRS        SPPNRS
418700950113     C                   MOVEL     BRVNSC        SPPNSC
418800950113     C                   MOVEL     BRVNPS        SPPNPS
418900950113     C                   MOVEL     WDTP          SPPDFV
419000950113     C                   MOVEL     'T'           SPPFLG
419100000111     C                   WRITE     FISPP000
419200930127     C                   ENDSR
419300991019     C*
419400040311     C*--- ELABORAZIONE SPUNTE PARTENZA ------------------------------*
419500991019     C     SPUPAR        BEGSR
419600991019     C**
419700991019     C**   VEDO SE DEVO IMPOSTARVI PESO E/O VOLUME
419800000620     C                   CLEAR                   SAVSPE
419900130227     C                   SETOFF                                       45
420000991019     C*
420100040121     c* Invece che testare se ka cml/vdl, testo il flag se in sto terminal
420200040121     c*   si usa il peso  e/o volume sulle spunte
420300040609     c     pa2spv        ifeq      'S'
420400040609     c     pa2spp        oreq      'S'
420500991019     C* LETTO FV ENTRATA, LEGGO LE SUE SPUNTE
420600991019     C                   MOVEL     1             KVVNPG
420700000111     C                   Z-ADD     FGV2          KVVNFV
420800991019     C                   MOVEL     SIMFEL        KVVFGS
420900070123     C     KBRV32        SETLL     FNBRV02
421000070123     C     KBRV32        READE     FNBRV02                                32
421100991019     C*
421200991019    2C     *IN32         DOWEQ     *OFF
421300991019     C* ESCLUDO LE SPUNTE GIA' TRASMESSE
421400991019    3C     BRVFTR        IFEQ      ' '
421500000620     C*
421600040121    4c                   if        pa2spv='S' and brvvuc=0  or
421700040121     c                             pa2spp='S' and brvpuc=0
421800040121     c
421900991019     C                   MOVEL     DSBRV         SVBRVE
422000991019     C* IMPOSTO LA KEY PER CHAIN
422100991019     C                   Z-ADD     BRVVUC        WRVVUC
422200991019     C                   Z-ADD     BRVPUC        WRVPUC
422300991019     C                   EXSR      IMPKEY
422400991019     C*
422500070123     C     KBRV6         SETLL     FNBRV07L
422600070123     C     KBRV6         READE     FNBRV07L                               34
422700991019    5C     *IN34         DOWEQ     *OFF
422800991019     C**
422900120604     C* VEDO SE SPUNTA CON VOLUME o peso
423000991019    6C     WRVVUC        IFEQ      0
423100991019     C     BRVVUC        ANDGT     0
423200991019     C                   Z-ADD     BRVVUC        WRVVUC
423300991019    6C                   ENDIF
423400991019     C* VEDO SE SPUNTA CON PESO CML
423500991019    6C     WRVPUC        IFEQ      0
423600991019     C     BRVPUC        ANDGT     0
423700991019     C                   Z-ADD     BRVPUC        WRVPUC
423800991019    6C                   ENDIF
423900991019     C*
424000070123     C     KBRV6         READE     FNBRV07L                               34
424100991019    5C                   ENDDO
424200991019     C*
424300991019     C                   MOVEL     SVBRVE        DSBRV
424400991019     C                   Z-ADD     WRVVUC        BRVVUC
424500991019     C                   Z-ADD     WRVPUC        BRVPUC
424600991019     C* AGGIORNO SPUNTA PARTENZA
424700070123     C                   UPDATE    FNBRV02
424800991019    4C                   ENDIF
424900991019    3C                   ENDIF
425000991019     C*
425100070123     C     KBRV32        READE     FNBRV02                                32
425200991019    2C                   ENDDO
425300040609    1C                   ENDIF
425400991019     C**
425500991019     C                   ENDSR
425600940414     C*
425700940414     C*--- ELABORAZIONE ABBINAMENTO ----------------------------------*
425800040625     c     ABBIN         BEGSR
425900940418     C* APERTURA FILES
426000021021     C                   OPEN      FNANM20L
426100021021     C                   OPEN      FNBLT38L
426200020605     C                   OPEN      FNBTT38L
426300020916     C                   OPEN      FNFVV02L
426400001120     C                   OPEN      FNFVV01L
426500041217     c                   clear                   wcalLsB7          1
426600940418     C*
426700940415     C* PRENDO FASE ANOMALIA
426800940415     C                   CALL      'TRUL11R'
426900940415     C                   PARM                    PARAM9
427000940414     C*
427100940414     C                   CLEAR                   LFV
427200940414     C                   MOVEA     FFV           LFV
427300940414     C                   Z-ADD     1             I
427400940414     C*
427500960209     C     PA2AGS        IFEQ      'S'
427600940414     C                   SETON                                        44
427700940414     C                   END
427800990917     C**
427900990917     C** SPUNTE DEL FOGLIO VIAGGIO PARTENZA
428000991019     C**   VEDO SE DEVO IMPOSTARVI PESO E/O VOLUME
428100991019     C                   EXSR      SPUPAR
428200000620     C*
428300000620     C                   CLEAR                   SAVSPE
428400940414     C*
428500940414     C* LEGGO TUTTE LE LINEE
428600950113    1C     LFV(I)        DOWGT     *ZEROS
428700940414     C                   MOVEL     LFV(I)        COMFIL
428800021021     c*
428900021021     c* per ogni linea di arrivo loopo sulle linee di partenza della L1
429000021021     c                   z-add     1             IL
429100021021    2c                   dow       l1(IL)>0
429200021021     c
429300021021     c                   eval      wlnp=l1(IL)
429400990917     C**
429500990917     C** BOLLE  PARTENZA
429600990917     C**
429700950915     C                   SETON                                        40
429800021021     C     KBLT          SETLL     FNBLT038
429900021021     C     KBLT          READE     FNBLT038                               32
430000940414     C*
430100021021    4C     *IN32         DOWEQ     '0'
430200000619     C*
430300970506     C* VEDO SE DEVO AGGIORNARE LA DATA DI ENTRATA
430400970506     C                   EXSR      AGGENT
430500970506     C**
430600950109     C* SE COLLI NON ANCORA PARTITI CERCO SOLO QUELLI BORDERIZZATI SUL
430700990303     C*  PER IL RESTO LI PRENDO TUTTI(DEVONO AVERE IL NUMERO FOGLIO
430800990303     C*  PIENO:ATTENZIONE ALLA "C" DELL'ESTERO)
430900000714     C*
431000130530    5C**** BLTNFV        IFEQ      FGV2
431100130530     C**** BLTFTR        ANDEQ     *BLANKS
431200990303     C*
431300130530     C     BLTFTR        ifeq      'C'
431400990303     C     BLTNFV        ANDGT     0
431500990303     C*
431600990303     C     BLTFTR        ORNE      *BLANKS
431700990303     C     BLTFTR        ANDNE     'C'
431800151126     c* Se parzializzo per KSC solo bolle di questo FGV
431900151126     c                   if        pa2ccm<=*zeros or bltnfv=fgv2
432000940414     C                   EXSR      CRTBRC
432100151126     c                   endif
432200151126     c
432300021021    5C                   ENDIF
432400000714     C**
432500021021     C     KBLT          READE     FNBLT038                               32
432600021021    4C                   ENDDO
432700021021     c
432800021021     c                   eval      IL=il+1
432900021021    2c                   enddo
433000021021     c
433100950915     C                   SETOFF                                       40
433200940414     C**
433300021025     C** BOLLE TRANSITO
433400940414     C**
433500151126     c                   if        pa2ccm<=*zeros
433600151126     C                   SETON                                        41
433700020605     C     KBtt38        SETLL     FNBTT38L
433800020605     C     KBtt38        READE     FNBTT38L                               32
433900940414     C*
434000950113    2C     *IN32         DOWEQ     '0'
434100080228     c* ignoro transito se bttlnp fa parte della £1 (può succedere x le
434200080228     c* trazioni ridotte quando alcuni 2 livello cambiano terminal di part)
434300080228     c     bttlnp        lookup    l1                                     90
434400080228     c                   if        not *in90
434500970506     C* VEDO SE DEVO AGGIORNARE LA DATA DI ENTRATA
434600970506     C                   EXSR      AGGENT
434700040422     c* se presente anomalia 200 dell'area non autogenero la spunta
434800040422     c* partenza perchè significa che il collo si trova presso un
434900040422     c* secondo livello in partenza di Simfel e quindi non posso
435000040422     c* dare per scontato che riparta
435100040422     c                   eval      wcrtspu='S'
435200040422     c                   z-add     200           comcaa
435300040503     c     kanm02        chain(N)  fnanm02l
435400040422     c                   if        %found(fnanm02l)
435500040422     c                             and anmdch = 0
435600040422     c     anmfle        lookup    l1                                     32
435700040422     c   32              eval      wcrtspu='N'
435800040422     c                   endif
435900970506     C**
436000040422     c     wcrtspu       ifeq      'S'
436100950109     C                   EXSR      CRTBRC
436200040422     c                   endif
436300080228
436400080228     c                   endif
436500940414     C*
436600020605     C     KBtt38        READE     FNBTT38L                               32
436700950113    2C                   ENDDO
436800940414     C                   SETOFF                                       41
436900151126     c                   endif
437000041217     c
437100041217     c* Eseguo call al pgm di calcolo volume e pesi
437200041217     c                   if        wcalLSB7='S'
437300041217     C                   Z-ADD     FGVLNP        PA1FEL
437400041217     C                   MOVEL     PARAM1        KPJBU
437500041217     C                   CALL      'FNLSB7R'
437600041217     C                   PARM                    KPJBA
437700041217     c
437800041217     c                   clear                   wcalLSB7
437900041217     c                   endif
438000940414     C**
438100970828     C** ANOMALIE DI DISGUIDO
438200940414     C**
438300000619     C     WFVPT         IFEQ      'N'
438400151126     c     pa2ccm        andle     *zeros
438500021021     C                   SETON                                        42
438600040115     c* Devo pulire questo campo perchè altrimenti potrebbe rimanere sporco
438700040115     c* e sbagliare la pistola in autogenerazione spunte partenza
438800040115     c                   clear                   wlocal
438900021021     c* loop per anmfle = a tutti i p.o. della £1
439000021021     C                   Z-ADD     1             IL                3 0
439100021021   1ac                   dow       l1(IL)>0
439200021021     c
439300021025     c                   eval      wlnp=l1(il)
439400940414     C                   Z-ADD     200           COMCAA
439500021021     C     KANM          SETLL     FNANM20
439600021021     C     KANM          READE     FNANM20                                32
439700940414     C*
439800950113    2C     *IN32         DOWEQ     '0'
439900940414     C* SE NON E' GIA' CHIUSA
440000950113    3C     ANMDCH        IFEQ      0
440100990517     C     WDTP          ANDGE     ANMDAO
440200960705     C*
440300020412     C                   CLEAR                   WCRT
440400960705     C* SE TROVO BOLLA PARTENZA CHIUDO ANOMALIA E NON AUTOGENERO SPUNTA
440500960705     C* (SE BOLLA DA ME CONFERMABILE E CONFERMABILE ANCHE DA ALTRI AS)
440600970828     C**
440700970828     C* ATTUALMENTE NON CREIAMO PIU' ANOMALIE DI DISGUIDO SE COLLO DA
440800970828     C*  ME CONFERMABILE ANCHE SE NON C'E' LA BOLLA PARTENZA PER CUI
440900970828     C*  NON HO PIU' BISOGNO DI CERCE LE ANOMALIE DISGUIDO
441000020412     c* 12/04/02: Ripristinata la routine srdisg in quanto se la 5f
441100020412     c* è stata inserita dopo il caricamento spunte il disguido può
441200020412     c* essere stato creato se al momento dello scarico spunte la
441300020412     c* bolla non era ancora stata confermata
441400020412    4C     anmnrs        ifgt      *zeros
441500020412     C                   exsr      srdisg
441600020412    4C                   endif
441700040406     c* disattivo autogenerazione spunte da anomalie di disguido: si voglion
441800040406     c* far partire i colli disguidati solo se spuntati in entrata o in par-
441900040406     c* tenza
442000040406     c                   eval      wcrt='N'
442100960705     C*
442200020412   3AC     wcrt          ifeq      ' '
442300070123     C     KANM5         CHAIN     FNBRV07                            31
442400950118    4C     *IN31         IFEQ      *OFF
442500950109     C* MI SALVO I CAMPI CHE MI SERVONO PER L'AGGIORNAMENTO
442600950109     C                   Z-ADD     BRVZNC        WRVZNC
442700950109     C                   Z-ADD     BRVVUC        WRVVUC
442800990906     C                   Z-ADD     BRVPUC        WRVPUC
442900980206     C     BRVCAN        IFNE      'E'
443000950113     C                   MOVEL     BRVCAN        WRVCAN
443100980206     C                   END
443200970506     C* IMPOSTO LA KEY PER CHAIN
443300970506     C                   EXSR      IMPKEY
443400970506     C* CREO SPUNTA PARTENZA
443500950109     C                   EXSR      CRTBRC
443600950113    4C                   END
443700020412   3AC                   endif
443800950113    3C                   END
443900940414     C*
444000021021     C     KANM          READE     FNANM20                                32
444100950113    2C                   ENDDO
444200021021     c                   eval      il=il+1
444300021021   1aC                   ENDDO
444400940414     C                   SETOFF                                       42
444500040624    1C                   ENDIF
444600940414     C**
444700960209     C** LEGGO LE SPUNTE ENTRATA PER CREARE EVENTUALI
444800950915     C** SPUNTE PARTENZA NON ANCORA CREATE
444900950915     C**
445000950915     C     *IN44         IFEQ      *ON
445100151126     c     pa2ccm        andle     *zeros
445200151126     c
445300950915     C                   SETON                                        43
445400001121     C*
445500950915     C                   Z-ADD     5             WNPG5
445600950915     C                   Z-ADD     DFV1          WDFV
445700020916      * ciclo all'interno della £1
445800020917do   c                   do        30            xx
445900020916     c                   If        L1(xx) = *Zeros
446000020916     c                   Leave
446100020916     c                   EndIf
446200020917     c     L1(xx)        Lookup    Spt                                    37
446300040802if   c     *IN37         IFEQ      *ON
446400040802     C     WFVPT         ANDEQ     'S'
446500040802     C*
446600040802     C     *IN37         OREQ      *OFF
446700040802     C     WFVPT         ANDEQ     'N'
446800040802     c*
446900040802if   c     *IN37         orEQ      *ON
447000040802     C     WFVPT         ANDEQ     'N'
447100040802     C
447200020916     c                   Move      L1(xx)        kfgs
447300020916     C     KFVV          SETLL     FNFVV02L
447400020916     C     KFVV          READE     FNFVV02L                               32
447500950915     C*
447600950915     C     *IN32         DOWEQ     *OFF
447700950915     C* LETTO FV ENTRATA, LEGGO LE SUE SPUNTE
447800970423     C                   MOVEL     FVVNPG        KVVNPG
447900000111     C                   Z-ADD     FVVNFV        KVVNFV
448000970423     C                   MOVEL     FVVFGS        KVVFGS
448100070123     C     KBRV33        SETLL     FNBRV02
448200070123     C     KBRV33        READE(N)  FNBRV02                                32
448300950915     C*
448400950915    2C     *IN32         DOWEQ     *OFF
448500960521     C* ESCLUDO LE SPUNTE CHE HANNO FLAG TRASMISSIONE 'N'
448600960521     C     BRVFTR        IFNE      'N'
448700960521     C     BRVDTR        ANDNE     *HIVAL
448800040422     c* se collo spuntato da un secondo livello in partenza non
448900040422     c* autogenero spunta partenza se presente anomalia 200 aperta
449000040422     c* creata da p.o. della £1 perchè significa che il collo è in
449100040422     c* disguido e non può ripartire se non spuntato in entrata o in
449200040422     c* partenza dal terminal di partenza
449300040422     c                   eval      wcrtspu='S'
449400040422     c     brvfgs        ifne      simfel
449500040422     c                   z-add     200           comcaa
449600040503     c     kanm3         chain(N)  fnanm02l
449700040422     c                   if        %found(fnanm02l)
449800040422     c                             and anmdch=0
449900040422     c     anmfle        lookup    l1                                     32
450000040422     c   32              eval      wcrtspu='N'
450100040422     c                   endif
450200040422     c                   endif
450300040422     c     wcrtspu       ifeq      'S'
450400950915     C                   Z-ADD     BRVZNC        WRVZNC
450500950915     C                   Z-ADD     BRVVUC        WRVVUC
450600990906     C                   Z-ADD     BRVPUC        WRVPUC
450700950915     C                   MOVEL     BRVCAN        WRVCAN
450800950915     C                   MOVEL     BRVNPS        WRVNPS
450900970506     C* IMPOSTO LA KEY PER CHAIN
451000970506     C                   EXSR      IMPKEY
451100970506     C* CREO SPUNTA PARTENZA
451200950915     C                   EXSR      CRTBRC
451300040422     c                   endif
451400960521     C                   ENDIF
451500950915     C*
451600070123     C     KBRV33        READE(N)  FNBRV02                                32
451700950915     C                   ENDDO
451800950915     C*
451900020916     C     KFVV          READE     FNFVV02L                               32
452000950915     C                   ENDDO
452100030902      *
452200020917if x c                   Else
452300020917     c                   Iter
452400020917if e c                   EndIf
452500030902      *
452600020917do e c                   EndDo
452700950915     C*
452800950915     C                   SETOFF                                       43
452900950915     C                   ENDIF
453000950915     C**
453100950915     C**CERCO SPUNTE IMP
453200950915     C**
453300960604     C                   MOVE      'N'           WALCF             1
453400960604     C*
453500070123     C     KBRV3         SETLL     FNBRV02
453600070123     C     KBRV3         READE(N)  FNBRV02                                32
453700940414     C*
453800950113    2C     *IN32         DOWEQ     *OFF
453900950109     C                   MOVEL     BRVLNP        KLNP
454000950109     C                   MOVEL     BRVLNA        KLNA
454100950109     C                   MOVEL     BRVNRS        KNRS
454200950113     C                   MOVEL     BRVNSC        KNSC
454300950109     C                   Z-ADD     1             WNPG
454400940414     C*
454500140618     C     KBRV5         SETLL     FNBRV05
454600140618     C     KBRV5         READE     FNBRV05                                33
454700940414     C*
454800940414     C* SEGNACOLLO TROVATO: ELIMINO SE NON PARTITO
454900000619    3C     *IN33         DOWEQ     *OFF
455000940414     C*
455100950113    4C     BRVDTR        IFEQ      0
455200980728     C* CANCELLO ANCHE SE FLAGGGATA CON "L" PERCHE' NON E' TRASMESSA MA
455300980728     C*  NON UTILIZZABILE
455400980728     C     BRVFTR        OREQ      'L'
455500140618     C                   DELETE    FNBRV05
455600950113   X4C                   ELSE
455700140618     C                   UNLOCK    FNBRV05L
455800961212    4C                   ENDIF
455900140618     C     KBRV5         READE     FNBRV05                                33
456000000619    3C                   ENDDO
456100940414     C*
456200070123     C     KBRV3         READE(N)  FNBRV02                                32
456300950113    2C                   ENDDO
456400940414     C**
456500940414     C                   ADD       1             I
456600950113    1C                   ENDDO
456700940414     C*
456800940418     C* CHIUSURA FILES
456900021021     C                   CLOSE     FNANM20L
457000021021     C                   CLOSE     FNBLT38L
457100020605     C                   CLOSE     FNBTT38L
457200020917     C                   CLOSE     FNFVV02L
457300001120     C                   CLOSE     FNFVV01L
457400940418     C*
457500940414     C                   ENDSR
457600970506     C*
457700970506     C* AGGIORNO LA DATA ENTRATA/DATA ENTRATA TRANSITO ---------------*
457800970506     C     AGGENT        BEGSR
457900970506     C* IMPOSTO LA KEY
458000970506     C                   EXSR      IMPKEY
458100041217     c                   clear                   Wdse0             1
458200041217     c
458300041217    1c                   if        *in40 and bltdse=0
458400041217     c                   eval      Wdse0='S'
458500041217    1c                   endif
458600970506     C**
458700970506     C* S P U N T A   P A R T E N Z A
458800970506     C**
458900970506     C* SE NON C'E' SPUNTA ENTRATA CONTROLLO SE NON L'HO GIA' SPARATO
459000970506     C* CERCO CON CATEGORIA 5
459100001117     C*
459200041217     C* Per bolla no poste: cerco spunta se DSE=0
459300041217     C* per bolla    poste: cerco spunta se DSE=0 prima dell'imi
459400041217     c* per       entrambe: cerco spunta se peso o volume=0
459500041217     c*                     di bolla con serie con utilizzo in spunta
459600041217     c*                     del peso o volume (da azorg)
459700041217    1c                   if        *in40
459800041217   1ac                   if        wdse0='S' or
459900041217     c                             (bltnrs>0 and bltvuc=0 and pa2spv='S') or
460000041217     c                             (bltnrs>0 and bltpuc=0 and pa2spp='S')
460100001121     C*
460200970506     C                   Z-ADD     5             WNPG
460300140618     C     KBRV55        CHAIN(N)  FNBRV05                            31
460400990917    2C                   SELECT
460500001123     C* Se fv PT do per scontato che p.o. poste è senza macchinone
460600001123     c* Quindi, in ricerca spunte entrata faccio solo una chain e non
460700001123     c* un ciclo di lettura
460800990917     C     *IN31         WHENEQ    *OFF
460900040121     c     pa2spv        andeq     ' '
461000040121     c     pa2spp        andeq     ' '
461100990917     C                   EXSR      AGGEN2
461200001117     C* N.B.: SE CML ESEGUE CICLO LETTURA DI TUTTE LE SPUNTE ENTRATA PE
461300001117     C*       R AVERE LA CERTEZZA DI MEMORIZZARE IL PESO E IL VOLUME
461400990917     C     *IN31         WHENEQ    *OFF
461500040121     c     pa2spv        andne     ' '
461600040121     c     *in31         oreq      *off
461700040121     c     pa2spp        andne     ' '
461800990917    3C     *IN31         DOWEQ     *OFF
461900990917     C                   EXSR      AGGEN2
462000140618     C     KBRV55        READE(N)  FNBRV05                                31
462100990917    3C                   ENDDO
462200990917     C*
462300990917   X2C                   OTHER
462400041217     c* La cat 1 o qualsiasi spunta  le cerco solo per dse=0
462500041217   2ac                   if        wdse0='S'
462600990917     C*
462700970506     C* SE NON C'E' CERCO CON CATEGORIA 1
462800970506     C                   Z-ADD     1             WNPG
462900140618     C     KBRV5         CHAIN(N)  FNBRV05                            31
463000990917    3C     *IN31         IFEQ      *OFF
463100990917     C                   EXSR      AGGEN2
463200990917   X3C                   ELSE
463300970506     C*
463400970506     C* ALTRIMENTI CERCO IN UNA QUALSIASI ALTRA CATEGORIA
463500070123     C     KBRV6         CHAIN     FNBRV07                            31
463600990917     C  N31              EXSR      AGGEN2
463700990917    3C                   ENDIF
463800041217   2ac                   ENDIF
463900041217    2C                   ENDSL
464000970506     C**
464100041217     C* SE C'ERA VOLUME CML memorizzo per eseguire a fine il calcolo
464200990917    2C     BLTVUC        IFGT      0
464300990917     C     BLTPUC        ORGT      0
464400041217     c                   eval      wcalLSB7='S'
464500990917    2C                   ENDIF
464600041217   1aC                   ENDIF
464700041217    1C                   ENDIF
464800970506     C**
464900970506     C* S P U N T A   T R A N S I T O
465000970506     C**
465100970506     C* SE NON C'E' DATA ENTRATA AL TRANSITO CONTROLLO SE L'HO SPARATO
465200970506    1C   41BTTDET        IFEQ      0
465300970506     C*  CERCO IN UNA QUALSIASI CATEGORIA
465400070123     C     KBRV6         CHAIN     FNBRV07                            31
465500990917    2C     *IN31         DOWEQ     *OFF
465600990917     C                   EXSR      AGGEN2
465700990917     C**
465800990917     C* SE HO CML VADO AVANTI A LEGGERE
465900040121     c     pa2spv        ifne      *blanks
466000040121     c     pa2spp        orne      *blanks
466100070123     C     KBRV6         READE     FNBRV07                                31
466200990917     C                   ELSE
466300990917     C                   SETON                                        31
466400990917     C                   ENDIF
466500990917    2C                   ENDDO
466600990917     C**
466700990917    2C     BTTVUC        IFGT      0
466800990917     C     BTTPUC        ORGT      0
466900041217     c                   eval      wcalLSB7='S'
467000990917    2C                   ENDIF
467100990917     C*
467200970506    1C                   END
467300970506     C                   ENDSR
467400990917     C*
467500990917     C* CALL A PGM FNLS44R PER AGGIORNAMENTO -------------------------*
467600990917     C     AGGEN2        BEGSR
467700990917     C*
467800990917     C* SE NON E' PISTOLA CREATA IN AUTOMATICO, AGGIORNO BOLLA
467900990917     C     BRVNPS        IFGT      0
468000990917     C     BRVNPS        LOOKUP    NPS                                    31
468100990917     C                   ELSE
468200990917     C                   SETOFF                                       31
468300990917     C                   ENDIF
468400990917     C**
468500990917    3C     *IN31         IFEQ      *OFF
468600990917     C*
468700990917     C                   EXSR      CALL44
468800000731     C     *IN40         IFEQ      *ON
468900000731     C* CHAINO FNBLT PER REPERIRE LA DATA ENTRATA AGGIORNATA
469000021021     C* ANCHE SE SONO NEL CICLO DI LETTURA FNBLT38L NON MI PREOCCUPO
469100000731     C* CHE SI SPORCHINO I CAMPI PERCHE' CON QUESTA CHAIN VADO A
469200000731     C* RILEGGERMI LO STESSO RECORD
469300040319     c* stessa cosa per fnbtt
469400000731     C     KBLT1         CHAIN(N)  FNBLT01L                           31
469500040319     c                   else
469600040319     c     *in41         ifeq      *on
469700040319     C     KBTT1         CHAIN     FNBTT11L                           31
469800040319     c                   endif
469900000731     C                   ENDIF
470000990917     C                   ENDIF
470100990917     C                   ENDSR
470200940414     C*
470300970828     C* CREO SPUNTE PARTENZA --------------------------------------*
470400940414     C     CRTBRC        BEGSR
470500940414     C                   SETOFF                                       33
470600950412     C                   CLEAR                   WDATU
470700970828     C                   CLEAR                   UPDBRV
470800940414     C*
470900950109     C                   Z-ADD     1             WNPG
471000020807    1C     *IN43         IFEQ      *OFF
471100970423     C* CHAIN PER NO SPUNTE ENTRATA
471200140618     C     KBRV5         CHAIN     FNBRV05                            31
471300961220     C*
471400970423    1C                   ELSE
471500040624     c
471600970423     C* CERCO SE C'E' LA BOLLA PARTENZA E NON E' BORDERIZZATA NON CREO
471700040624     C*  LA SPUNTA PARTENZA, se lnp in £1
471800960506     C     KBRV4         CHAIN(N)  FNBLT27L                           91
471900040624     c                   if        not *in91
472000040624     c     bltlnp        lookup    l1                                     37
472100040624     c                   if        not *in37
472200040624     c                   seton                                        91
472300040624     c                   endif
472400040624     c                   endif
472500040624     c
472600960506    2C     *IN91         IFEQ      *OFF
472700970507     C* CHAINO LA BOLLA
472800970507     C                   EXSR      CHABOL
472900020807     C* NON TROVATA--> COME  SE NON CI FOSSE
473000970507     C     WBOL          IFEQ      'N'
473100970507     C                   SETON                                        91
473200970507     C                   ENDIF
473300970506     C**
473400970506    3C  N91BLPFT1        IFEQ      ' '
473500961216     C     BLPDBR        ANDEQ     0
473600960503     C                   SETON                                        3133
473700960506   X3C                   ELSE
473800961220     C*
473900961220     C                   SETON                                        91
474000961220     C* SE SI TRATTA DI UNA BOLLA BORDERIZZATA SU FV ANCORA DA CHIUDERE
474100961220     C*  NON CREO SPUNTA PARTENZA
474200961220    4C     BLPFT1        IFEQ      ' '
474300961220     C     BLPNFV        ANDGT     0
474400961220     C     BLPNFV        ANDNE     FGV2
474500970424     C                   CLEAR                   DSLV53
474600970424     C                   MOVEL     BLPNFV        D53NFV
474700970424     C                   MOVEL     'G'           D53TFO
474800971107     C                   MOVEL     FGVLNP        D53FEL
474900020919     C                   Z-ADD     BLPTFP        D53FGS
475000970424     C                   CALL      'FNLV53R'
475100970424     C                   PARM                    DSLV53
475200970424    5C     D53FCF        IFEQ      ' '
475300961220     C                   SETON                                        3133
475400961220     C                   SETOFF                                       91
475500961220    5C                   ENDIF
475600961220    4C                   ENDIF
475700960506    3C                   ENDIF
475800970506     C*
475900970506   X2C                   ELSE
476000160622     C* NON TROVATA BOLLA PARTENZA
476100160622     c*  oppure bolla non del terminal di partenza:
476200160622     c*  se bolla arrivo non creo spunta partenza
476300160622     c*  devo tener conto anche se il secondo liv in partenza è un
476400160622     c*  terminal di arrivo (anche con 2°liv)
476500970506     C                   SETOFF                                       3435
476600160622     c* solo se non ho trovato fnblt rifaccio lokkup in £1
476700160622     c                   if        not %found(fnblt27l)
476800970506     C     KLNP          LOOKUP    L1                                     34
476900160622     c                   endif
477000130530     C*
477100031113     c     *in34         ifeq      *off
477200130530     c                   clear                   dslv55
477300130530     c                   movel     'A'           d55tpt
477400160622     c                   if        not %found(fnblt27l)
477500130530     c                   z-add     klnp          d55lnp
477600160622     c                   else
477700160622     c                   z-add     bltlnp        d55lnp
477800160622     c                   endif
477900130530     c                   z-add     klna          d55lin
478000130530     c                   z-add     wdfv          d55drf
478100130530     c                   call      'FNLV55R'
478200130530     c                   parm                    dslv55
478300130530     c     d55tfa        ifeq      fgvlnp
478400160622     c     d55tfa        oreq      brvpes
478500031113     c                   seton                                        3133
478600031113     c                   setoff                                       91
478700031113     c                   endif
478800031113     c                   endif
478900970506     C*
479000970506    2C                   ENDIF
479100960503     C*
479200140618     C   91KBRV5         CHAIN     FNBRV05                            31
479300961220    1C                   ENDIF
479400940414      *
479500970423      * 31 ON -NON ESISTE SPUNTA PARTENZA --> CREO SPUNTA (33OFF)
479600970423      * 31 OFF-    ESISTE SPUNTA PARTENZA :
479700970423     C**
479800950208    0C     *IN31         DOWEQ     *OFF
479900970828     C   42
480000970828     COR 43WRVVUC        IFGT      0
480100970828     C     BRVVUC        ANDEQ     0
480200971114     C                   Z-ADD     WRVVUC        BRVVUC
480300971114     C                   MOVEL     'S'           UPDBRV
480400970828     C                   ENDIF
480500990906     C   42
480600990906     COR 43WRVPUC        IFGT      0
480700990906     C     BRVPUC        ANDEQ     0
480800990906     C                   Z-ADD     WRVPUC        BRVPUC
480900990906     C                   MOVEL     'S'           UPDBRV
481000990906     C                   ENDIF
481100970423      * - BRVDTR =0 -->
481200950208    1C     BRVDTR        IFEQ      0
481300940414      *
481400970423      * FOGLIO <> DA QUELLO IN CHIUSURA
481500950109    2C     BRVNFV        IFNE      FGV2
481600940414      *
481700970424     C                   CLEAR                   DSLV53
481800000111     C                   Z-ADD     BRVNFV        D53NFV
481900970424     C                   MOVEL     'G'           D53TFO
482000020919     C                   Z-ADD     BRVFGS        D53FGS
482100970424     C                   CALL      'FNLV53R'
482200970424     C                   PARM                    DSLV53
482300940414      *
482400970423      * ESISTE FOGLIO --> SE E' APERTO NON ANNULLATO NON RICREO(33ON)
482500970424   3AC     D53ERR        IFEQ      ' '
482600970424    3C     D53FCF        IFEQ      ' '
482700970424     C     D53ATB        ANDEQ     ' '
482800970909     C     D53DEF        ANDEQ     ' '
482900940414     C                   SETON                                        33
483000950412     C                   ELSE
483100950412     C*
483200970424    4C     D53DFV        IFEQ      WDTP
483300950915     C* SE NON STO LEGGENDO SPUNTE ENTRATA CHAINO IN UPDAT
483400970828     C*  PERCHE' DEVO RIAGGIORNARLA CON I DATI DI QESTO FV
483500950915    5C     *IN43         IFEQ      *OFF
483600950412     C                   MOVEL     'S'           WDATU
483700950418     C                   MOVEL     BRVNFV        WSANFV
483800950418     C                   MOVEL     BRVFGS        WSAFGS
483900950915     C*
484000950915     C                   ELSE
484100950915     C* SE DA SPUNTA ENTRATA NON RICREO LA SPUNTA SE E' DI OGGI
484200950915     C                   SETON                                        33
484300950915    5C                   ENDIF
484400950915    4C                   END
484500950412    3C                   END
484600970424   3AC                   END
484700940414      *
484800940414      * NON ESISTE FOGLIO
484900970423      * O FOGLIO CHIUSO E ANNULLATO --> RICREO SPUNTA (VADO IN UPDATE)
485000940506      *                                                     33OFF
485100940506      *
485200950412   X2C                   ELSE
485300940506      *                        FOGLIO =  A QUELLO IN CHIUSURA: NON RICR
485400940506      *                                                     33ON
485500940414     C                   SETON                                        33
485600940414    2C                   END
485700950412     C*
485800950412   X1C                   ELSE
485900970423      *
486000970423      * - BRVDTR><0 --> SPUNTA GIA'TRASMESSA MA NON AGGIORNATA BOLLA:
486100970423      *                 SFLAGGO SEMPLICEMENTE LA SPUNTA E BASTA
486200971114    2C     BRVNFV        IFEQ      FGV2
486300971114     C  N43              CLEAR                   BRVDTR
486400000111     C  N43              CLEAR                   BRVHTR
486500971114     C  N43              CLEAR                   BRVFTR
486600950412     C*
486700971114     C  N43              MOVEL     'S'           UPDBRV            1
486800950412     C                   SETON                                        33
486900950412     C*
487000950915   X2C                   ELSE
487100970424      *
487200970424     C                   CLEAR                   DSLV53
487300000111     C                   Z-ADD     BRVNFV        D53NFV
487400970424     C                   MOVEL     'G'           D53TFO
487500020919     C                   Z-ADD     BrvFGS        D53FGS
487600970424     C                   CALL      'FNLV53R'
487700970424     C                   PARM                    DSLV53
487800950412     C* SE IL FOGLIO DELLA SPUNTA TROVATA NON HA LA STESSA DATA DEL FV
487900950412     C*  CHE STO CHIUDENDO--> TENGO ENTRAMBE LE SPUNTE, ALTRIMENTI NO
488000970424    3C     D53DFV        IFEQ      WDTP
488100970424    3C     D53ERR        ANDEQ     ' '
488200950915     C* SE NON STO LEGGENDO SPUNTE ENTRATA CHAINO IN UPDAT
488300950915    4C     *IN43         IFEQ      *OFF
488400950412     C                   MOVEL     'S'           WDATU             1
488500950418     C                   MOVEL     BRVNFV        WSANFV
488600950418     C                   MOVEL     BRVFGS        WSAFGS
488700950915     C*
488800950915   X4C                   ELSE
488900950915     C* SE DA SPUNTA ENTRATA NON RICREO LA SPUNTA PARTENZA DI OGGI
489000980316     C* (SOLO SE SPUNTA NON 'L')
489100980316     C     BRVFTR        IFNE      'L'
489200950915     C                   SETON                                        33
489300980316     C                   ELSE
489400980316     C                   MOVEL     'S'           WDATU             1
489500980316     C                   MOVEL     BRVNFV        WSANFV
489600980316     C                   MOVEL     BRVFGS        WSAFGS
489700980316     C                   ENDIF
489800950915    4C                   ENDIF
489900950915    3C                   ENDIF
490000950412     C*
490100950412    2C                   ENDIF
490200950412    1C                   ENDIF
490300950208     C*
490400950208     C* SE LA SPUNTA NON E' QUELLA BUONA VEDO SE CE N'E' UN'ALTRA
490500950412    1C     *IN33         IFEQ      *OFF
490600140618     C     KBRV5         READE     FNBRV05                                31
490700950412   X1C                   ELSE
490800950208     C* SE TROVATA SPUNTA BUONA --> ESCO
490900950208     C                   SETON                                        31
491000991019     C*
491100991019     C* CONTROLLO PESO E VOLUME DA BLT E BTT PERCHE'DEVONO ESSERE SEMPR
491200991019     C*  POSSIBILMENTE, QUESTI AD AGGIORNARE LE SPUNTE DA TRASMETTERE
491300991019     C*  IN ARRIVO
491400991019     C   40BLTVUC        IFGT      0
491500991019     C     BLTVUC        ANDNE     BRVVUC
491600991019     C                   Z-ADD     BLTVUC        BRVVUC
491700991019     C                   MOVEL     'S'           UPDBRV
491800991019     C                   ENDIF
491900991019     C   40BLTPUC        IFGT      0
492000991019     C     BLTPUC        ANDNE     BRVPUC
492100991019     C                   Z-ADD     BLTPUC        BRVPUC
492200991019     C                   MOVEL     'S'           UPDBRV
492300991019     C                   ENDIF
492400991019     C**
492500991019     C   41BTTVUC        IFGT      0
492600991019     C     BTTVUC        ANDNE     BRVVUC
492700991019     C                   Z-ADD     BTTVUC        BRVVUC
492800991019     C                   MOVEL     'S'           UPDBRV
492900991019     C                   ENDIF
493000991019     C   41BTTPUC        IFGT      0
493100991019     C     BTTPUC        ANDNE     BRVPUC
493200991019     C                   Z-ADD     BTTPUC        BRVPUC
493300991019     C                   MOVEL     'S'           UPDBRV
493400991019     C                   ENDIF
493500990917     C*
493600970828    3C     UPDBRV        IFEQ      'S'
493700140618     C                   UPDATE    FNBRV05
493800970828     C                   ELSE
493900140618     C                   UNLOCK    FNBRV05L
494000970828    3C                   ENDIF
494100991019     C**
494200991019    1C                   ENDIF
494300950208    0C                   ENDDO
494400970423     C**
494500970423     C* CONTROLLO SE COLLO SPUNTATO O MENO IN DATA ODIERNA
494600970423     C*  PER CREARE SPUNTE PISTOLE 98 96
494700970828    1C     *IN33         IFEQ      *OFF
494800970828     C* ENTRO ANCHE PER LEGGERE LE SPUNTE SE DEVO CERCARE IL VOLUME
494900970423     C                   CLEAR                   WNPS
495000070123     C     KBRV6         SETGT     FNBRV07L
495100070123     C     KBRV6         READPE    FNBRV07L                               34
495200970828    2C     *IN34         DOWEQ     *OFF
495300970828     C**
495400970828     C* VEDO SE SPUNTA CON VOLUME CML
495500970828     C   42
495600970828     COR 43WRVVUC        IFEQ      0
495700970828    3C     BRVVUC        ANDGT     0
495800970828     C                   Z-ADD     BRVVUC        WRVVUC
495900970828    3C                   ENDIF
496000000428     C* VEDO SE SPUNTA CON PESO CML
496100990906     C   42
496200990906     COR 43WRVPUC        IFEQ      0
496300990906    3C     BRVPUC        ANDGT     0
496400990906     C                   Z-ADD     BRVPUC        WRVPUC
496500990906    3C                   ENDIF
496600970828     C**
496700970423     C* ESCLUDO LE CATEGORIE 1
496800970828    3C     BRVNPG        IFNE      1
496900980116     C* AD ECCEZIONE DELLE SPUNTE PARTENZA SU FV NON CONTENENTE BRVLNA
497000980116     C* (BRVFTR='L')
497100980116     C     BRVFTR        OREQ      'L'
497200970424     C* CALL AL PGM PER REPERIRE LA DATA FOGLIO
497300970424     C                   CLEAR                   DSLV53
497400970424     C                   MOVEL     BRVNPG        D53NPG
497500000111     C                   Z-ADD     BRVNFV        D53NFV
497600970424     C                   MOVEL     BRVFGS        D53FGS
497700970424     C                   CALL      'FNLV53R'
497800970424     C                   PARM                    DSLV53
497900970424     C*
498000970828    4C     D53DFV        IFGE      WDTP
498100970423     C                   MOVEL     98            WNPS
498200970528     C                   MOVEL     BRVNPS        SPUNPS
498300970423     C                   SETON                                        34
498400970828   X4C                   ELSE
498500970828    5C     WNPS          IFNE      98
498600970423     C                   MOVEL     96            WNPS
498700970528     C                   MOVEL     BRVNPS        SPUNPS
498800970828    5C                   ENDIF
498900970828    4C                   ENDIF
499000970828    3C                   ENDIF
499100970423     C*
499200970828    3C     *IN34         IFEQ      *OFF
499300040121     C**   PARCML        ORNE      ' '
499400040121     c     pa2spv        oreq      'S'
499500970828     C     WRVVUC        ANDEQ     0
499600970828     C     *IN40         ANDEQ     *OFF
499700970828     C     *IN41         ANDEQ     *OFF
499800990907      *PESO
499900040121     C**   PARCML        OREQ      'P'
500000040121     c     pa2spp        oreq      'S'
500100990907     C     WRVPUC        ANDEQ     0
500200990907     C     *IN40         ANDEQ     *OFF
500300990907     C     *IN41         ANDEQ     *OFF
500400070123     C     KBRV6         READPE    FNBRV07L                               34
500500970828    3C                   ENDIF
500600970828    2C                   ENDDO
500700970423     C*
500800991019    1C                   ENDIF
500900950208     C*
501000980206     C                   CLEAR                   WBRVE
501100950208     C* SE HO TROVATO UNA SPUNTA LA RICHAINO
501200970828    0C     WDATU         IFEQ      'S'
501300970828     C     *IN33         ANDEQ     *OFF
501400070123     C     KBRV2         CHAIN     FNBRV02                            31
501500980206     C* MEMORIZZO SPUNTA PARTENZA ERRATA IN CAMPI DEL FILE STORICO
501600980206     C* SPUNTE PARTENZA ERRATE
501700980206     C  N31BRVFTR        IFEQ      'L'
501800980206     C                   MOVE      'S'           WBRVE             1
501900980209     C                   MOVEL     DSBRV         SVBRVE
502000980206     C                   END
502100970828    0C                   ENDIF
502200950109     C*
502300950109     C* SOLO PER I TRANSITI: AUTOGENERO SPUNTA PER BOLLE TRANSITO
502400950109     C*                      SOLO SE RELATIVO FOGLIO ABBINATO
502500950412    0C   41BTTDET        IFEQ      0
502600950127     C* COME PRIMA COSA VERIFICO SE PER CASO LA SPUNTA ESISTE
502700950127     C* MA NON HA AGGIORNATO IL RECORD
502800950109     C                   EXSR      ABBBTT
502900950412    1C     WABBIN        IFNE      'S'
503000950118     C                   SETON                                        33
503100950412    1C                   ENDIF
503200950412    0C                   ENDIF
503300940512     C*
503400950412    0C  N33              DO
503500000714     C**
503600070123     C* SCARICO SPUNTA ERRATA IN FNBRVE
503700980206     C     WBRVE         IFEQ      'S'
503800980209     C* CONTROLLO CHE IL COLLO NON SIA IN IMP
503900070123     C     KBRV01        SETLL     FNBRV01L                               37
504000010611     c     *in37         ifeq      *off
504100070219     c     kbrve         setll     fnbrve                                 99
504200010611     C  n99              MOVEL     SVBRVE        DSBRV
504300070123     C  N99              WRITE     FNBRVE
504400010611     c                   endif
504500980206     C                   END
504600140618     C                   CLEAR                   FNBRV05
504700950109     C*
504800950118     C                   MOVEL     FGVLNP        BRVFGS
504900000111     C                   Z-ADD     FGVLNP        BRVPES
505000000111     C                   Z-ADD     FGV2          BRVNFV
505100950118     C                   Z-ADD     1             BRVNPG
505200971107     C                   Z-ADD     FGVLNP        BRVFLE
505300070220     c                   time                    wtimez
505400070220     c* millisecondi
505500070220     c                   extrct    wtimez:*ms    millisec          6 0
505600070220     c                   movel     millisec      brvmis
505700070220     c* data immissione
505800070220     c                   movel     wtimez        dataiso
505900070220     c                   movel     dataiso       brvdfs
506000070220     c* ora immissione
506100070220     c                   move      wtimez        oraiso
506200070220     c                   movel     oraiso        brvhms
506300070220     c                   movel     'A'           brvtap
506400070220     c                   eval      brvpru='§'+ nompgm
506500080328     c                   clear                   brvtsu
506600950109     C*
506700950915     C   40              EXSR      CRTBLT
506800950109     C*
506900950109     C   41              EXSR      CRTBTT
507000950109     C*
507100950113     C   42              EXSR      CRTSAN
507200940414     C*
507300950915     C   43              EXSR      CRTSPE
507400970423     C**
507500970423     C** VEDO SE IMPOSTARE PISTOLA 98 O 96
507600970506     C**  SOLO SE NON SI TRATTA DI UNA PISTOLA 90 CHE VINCE
507700970507     C**  ESCLUDO SEMPRE I LOCALI PER I QUALI LA PISTOLA 98 96 NON ESIS
507800970506     C**
507900970507    1C     BRVNPS        IFNE      90
508000970507     C*
508100970507    2C     WNPS          IFEQ      96
508200970507    3C     WLOCAL        IFNE      'S'
508300970423     C                   MOVEL     WNPS          BRVNPS
508400970528     C* PER BOLLE TRANSITO SE L'UNICA SPUNTA PRESENTE E' UNA 89 CREO
508500970528     C*  UNA 89
508600971027     C* ANCHE PER DISGUIDO
508700971027     C   42
508800971027     COR 41SPUNPS        IFEQ      89
508900970528     C                   MOVEL     SPUNPS        BRVNPS
509000970528     C                   ENDIF
509100970507     C                   ELSE
509200970507     C* LA 96 LOCALE DIVENTA --> 90
509300970507     C                   MOVEL     90            BRVNPS
509400970507    3C                   ENDIF
509500970507     C**
509600970507   X2C                   ELSE
509700970423     C**
509800970423     C* LA PISTOLA 98 E' SOLO PER CHI NON HA IL CML
509900010910     c* (e per chi ha il flag §ogagp = blank)
510000970507    3C     WNPS          IFEQ      98
510100970423     C     PARCML        ANDEQ     ' '
510200970507     C     WLOCAL        ANDNE     'S'
510300010910     C     pa2agp        andeq     *blanks
510400970423     C                   MOVEL     WNPS          BRVNPS
510500970528     C* PER BOLLE TRANSITO SE L'UNICA SPUNTA PRESENTE E' UNA 89 CREO
510600971027     C*  UNA 89 ANCHE PER I DISGUIDI
510700971027     C   42
510800971027     COR 41SPUNPS        IFEQ      89
510900970528     C                   MOVEL     SPUNPS        BRVNPS
511000970528     C                   ENDIF
511100970507    3C                   ENDIF
511200970507    2C                   ENDIF
511300040406     C*
511400970507    1C                   ENDIF
511500950915     C*
511600140618     C   31              WRITE     FNBRV05
511700070123     C  N31              UPDATE    FNBRV02
511800950203     C*
511900950203     C* SE AUTOGENERATA SPUNTA PARTENZA: AGGIORNO BOLLA TRANSITO
512000970507    1C     *IN41         IFEQ      *ON
512100950203     C     BRVNPS        ANDEQ     89
512200970423     C                   EXSR      CALL44
512300970507    1C                   END
512400950412    0C                   END
512500940414     C*
512600940414     C                   ENDSR
512700950109     C*
512800950109     C*--- VEDO SE COLLO TRANSITO DICHIARATO PARTITO E ABBINATO-------*
512900950109     C     ABBBTT        BEGSR
513000950109     C*
513100950118     C* CHAIN SULLE SPUNTE PER VEDERE SE C'E' LA SPUNTA PARTENZA DA
513200950118     C*  ALTRA FILIALE --> SE NON C'E' NON CREO SPUNTA
513300950118     C*
513400950118     C* IN QUESTO CASO NON IMPORTA CHE TESTI SE LA SPUNTA NON E' DI
513500950118     C* SIMFEL PERCHE' SE CI FOSSE AVREI AGGIORNATO LA DATA ENTRATA
513600950118     C* E QUINDI NON SAREI DENTRO A QUESTA ROUTIME
513700950109     C                   Z-ADD     1             WNPG
513800950109     C                   CLEAR                   WABBIN
513900140618     C     KBTT          CHAIN(N)  FNBRV05                            34
514000970828     C**
514100950109    1C     *IN34         DOWEQ     *OFF
514200970828     C                   CLEAR                   DSLV53
514300020617     C                   MOVEL     'T'           D53TFO
514400070123     C****               Z-ADD     FGVLNP        D53FEL
514500000111     C                   Z-ADD     BRVNFV        D53NFV
514600970828     C                   MOVEL     BRVNPG        D53NPG
514700970828     C                   MOVEL     BRVFGS        D53FGS
514800070123     C****               MOVEL     BRVFLE        D53FLF
514900970828     C                   MOVEL     BRVLNA        D53LNA
515000970828     C                   MOVEL     'A'           D53ABB
515100020617     c                   z-add     fgvlnp        d53lai
515200970828     C                   CALL      'FNLV53R'
515300970828     C                   PARM                    DSLV53
515400970828    2C     D53ERR        IFEQ      ' '
515500970828     C     D53NFA        ANDGT     0
515600950113     C                   MOVEL     'S'           WABBIN            1
515700970828     C                   SETON                                        34
515800970828     C                   ELSE
515900950109     C*
516000140618     C     KBTT          READE(N)  FNBRV05                                34
516100970828    2C                   ENDIF
516200950113    1C                   ENDDO
516300950109     C*
516400950109     C                   ENDSR
516500970507     C*
516600970507     C* CHAINO BOLLA PARTENZA O BOLLA TRANSITO -----------------------*
516700040628     c     CHABOL        BEGSR
516800970507     C**
516900970507     C* CHAINO BOLLA PARTENZA
517000970507     C**
517100000620     C   45
517200000620     COR 43
517300970507    1COR 40BLTSPE        IFNE      SAVSPE
517400970507     C                   MOVEL     'S'           WBOL              1
517500970507     C                   CLEAR                   WLOCAL
517600970507     C     KBLT3         CHAIN(N)  FNBLP01L                           37
517700970507     C                   MOVEL     BLTSPE        SAVSPE           16 0
517800970507     C**
517900970507    2C     *IN37         IFEQ      *ON
518000970507     C                   MOVEL     'N'           WBOL
518100970507     C                   MOVEL     'S'           §1BFG5
518200970507   X2C                   ELSE
518300970507     C*
518400970507     C* CONTROLLO SE E' BOLLA LOCALE
518500970507    3C     BLPTFP        IFEQ      BLPTFA
518600970714     C     BLPLNP        OREQ      BLPLNA
518700970507     C                   MOVEL     'S'           WLOCAL            1
518800970507    3C                   ENDIF
518900970507     C*
519000970507     C* COD TRATTAMENTO MERCE CHE GENERA IN AUTOMATICO LE SPUNTE PARTEN
519100970507     C     BLPCTM        LOOKUP    CTM                                    37
519200970507     C*     TROVATO= IMPOSTO §1BFG5 = N
519300970507     C   37              MOVEL     'N'           §1BFG5
519400970507     C* NON TROVATO= IMPOSTO §1BFG5 = S
519500970507     C  N37              MOVEL     'S'           §1BFG5
519600970507    2C                   ENDIF
519700040624    1C                   ENDIF
519800040624     C**
519900970507     C* CHAIN BOLLA TRANSITO
520000970507     C**
520100970507     C   41BTTSPE        IFNE      SAVSPE
520200970507     C                   CLEAR                   WLOCAL
520300020605     C     KBTT3         CHAIN(N)  FNBTP11L                           37
520400970507     C                   MOVEL     BTTSPE        SAVSPE
520500970507     C                   ENDIF
520600970507     C                   ENDSR
520700950109     C*
520800950109     C* CREO SPUNTA DA FNBLT -----------------------------------------*
520900950109     C     CRTBLT        BEGSR
521000130226     c                   eval      wtfp90='N'
521100950109     C                   MOVEL     BLTFLS        BRVLNP
521200950109     C                   MOVEL     BLTLNA        BRVLNA
521300950109     C                   MOVEL     BLTNRS        BRVNRS
521400950113     C                   MOVEL     BLTNSC        BRVNSC
521500970507     C* CHAINO BOLLA
521600970507     C                   EXSR      CHABOL
521700950109     C*
521800970828     C                   MOVEL     BLPZNC        BRVZNC
521900950118     C                   MOVEL     BLTCAN        BRVCAN
522000970828     C                   MOVEL     BLTVUC        BRVVUC
522100990907     C                   MOVEL     BLTPUC        BRVPUC
522200950109     C*
522300960209     C* CONTROLLO SE ESISTE SPUNTA ENTRATA
522400950113    1C   44BLTDSE        IFEQ      0
522500950109     C                   SETON                                        33
522600950109     C                   ELSE
522700950109     C                   SETOFF                                       33
522800950113    1C                   END
522900950109     C*
523000960209     C* SE NON PREVISTA GENERAZ.DA SPU.ENTRATA--> SEMPRE 88 O 90
523100950109     C  N44              SETON                                        33
523200950109     C*
523300950109     C*     ESISTE SPUNTA ENTRATA --> STESSO NUMERO PISTOLA
523400950113    1C     *IN33         IFEQ      '0'
523500950109     C                   MOVEL     BLTNPE        BRVNPS
523600950113   X1C                   ELSE
523700950109     C*
523800950109     C* NON ESISTE SPUNTA ENTRATA --> PISTOLA AUTOGENERATA 88 O 90
523900950109     C                   MOVEL     '88'          BRVNPS
524000130226     c* controllo se bolla con colli che partono da altra filiale
524100130226     c*  devo impostare pistola 90
524200130226     c                   if        blpxco='W'
524300130226     c                   exsr      Contr_XCO
524400130227     c
524500130227     c                   if        wtfp90='S'
524600130227     C                   MOVEL     90            BRVNPS
524700130226     c                   endif
524800130227     c                   endif
524900970507     C*
525000970423    2C     §1BFG5        IFEQ      'N'
525100970507     C     WLOCAL        OREQ      'S'
525200970507     C                   MOVEL     90            BRVNPS
525300970507    2C                   ENDIF
525400970507     C*
525500970423    1C                   END
525600950109     C*
525700950109     C                   ENDSR
525800970507     C*
525900130226     C* Controllo se collo partito da altro TFP ----------------------*
526000130226     C     Contr_XCO     BEGSR
526100130226     c
526200130227     C                   MOVEL     DSBRV         DSBRVS
526300130226     c                   eval      Wflsnrs=(blpfls*100)+bltnrs
526400130226
526500130226    1c                   if        wflsnrs<>savfnrs
526600130226     c                   clear                   xcotfp
526700130226     C                   MOVEL     'XCO'         kcod1
526800130226     C                   eval      kke1='ALTRITFP'
526900130226     C                   eval      kke2=%editc(wflsnrs:'X')
527000130226     c     ktbe2         chain     tntbe01l
527100130226    2c                   if        %found(tntbe01l) AND tbeatb=' '
527200130226     C                   MOVEA     TBeUNI        xcotfp
527300130226    2c                   endif
527400130226     c
527500130226     c                   eval      savfnrs=wflsnrs
527600130226    1c                   endif
527700130226     c
527800130226     c                   z-add     5             wnpg
527900130226    1c                   do        2
528000140618     c     kblt_xco      setll     fnbrv05l
528100140618     c     kblt_xco      reade     fnbrv05l
528200130226     c
528300140618    2c                   dow       not %eof(fnbrv05l)
528400130226    3c                   if        brvfle<>fgvlnp
528500130226     c* Verifico se la filiale che spunta è prevista
528600130226     c                   movel     brvfgs        alfafgs           3
528700130226     c                   eval      Indx=%lookup(alfafgs:xcotfp)
528800130226     c* Trovata --> memerizzo per impsostare pistola 90
528900130226    4c                   if        Indx>0
529000130226     c                   eval      Wtfp90='S'
529100130226     c                   leave
529200130226    4c                   endif
529300130226    3c                   endif
529400130226     c
529500140618     c     kblt_xco      reade     fnbrv05l
529600130226    2c                   enddo
529700130226     c
529800130226    2c                   if        wtfp90='S'
529900130226     c                   leave
530000130226     c                   else
530100130226     c                   eval      wnpg=1
530200130226    2c                   endif
530300130226     c
530400130226    1c                   enddo
530500130226     c
530600130227     C                   MOVEL     DSBRVS        DSBRV
530700130226     C                   ENDSR
530800130226     C*
530900130226     C* CREO SPUNTA DA FNBTT -----------------------------------------*
531000130226     C     CRTBTT        BEGSR
531100950109     C                   MOVEL     BTTFLS        BRVLNP
531200950109     C                   MOVEL     BTTLNA        BRVLNA
531300950109     C                   MOVEL     BTTNRS        BRVNRS
531400950113     C                   MOVEL     BTTNSC        BRVNSC
531500970507     C* CHAINO BOLLA
531600970507     C                   EXSR      CHABOL
531700950109     C*
531800950109     C                   MOVEL     BTPZNC        BRVZNC
531900990906     C                   MOVEL     BTTVUC        BRVVUC
532000990907     C                   MOVEL     BTTPUC        BRVPUC
532100950109     C                   MOVEL     BTTCAN        BRVCAN
532200950109     C* SE E' PISTOLA AUTOGENERATA SENZA SPUNTA IN ENTRATA:
532300950109     C* --> PISTOLA 89
532400950109     C     BTTDET        IFEQ      0
532500950109     C                   MOVEL     '89'          BRVNPS
532600950109     C                   ELSE
532700960209     C* GENERAZIONE SPUNTE 88
532800950109     C     *IN44         IFEQ      *OFF
532900950109     C                   MOVEL     '88'          BRVNPS
533000950109     C                   ELSE
533100960209     C* GENERAZIONE SPUNTE DA SPU.ENTRATA
533200950109     C                   MOVEL     BTTPET        BRVNPS
533300950109     C                   ENDIF
533400950109     C                   ENDIF
533500950109     C*
533600950109     C                   ENDSR
533700950109     C*
533800950109     C* CREO SPUNTA DA ANOMALIE --------------------------------------*
533900950113     C     CRTSAN        BEGSR
534000950109     C                   MOVEL     ANMFLS        BRVLNP
534100950109     C                   MOVEL     ANMLNA        BRVLNA
534200950109     C                   MOVEL     ANMNRS        BRVNRS
534300950109     C                   MOVEL     ANMSCN        BRVNSC
534400950109     C                   MOVEL     WRVZNC        BRVZNC
534500970828     C                   MOVEL     WRVVUC        BRVVUC
534600990907     C                   MOVEL     WRVPUC        BRVPUC
534700950109     C                   MOVEL     WRVCAN        BRVCAN
534800960209     C* GENERAZIONE SPUNTE 88
534900950109     C     *IN44         IFEQ      *OFF
535000950109     C                   MOVEL     '88'          BRVNPS
535100950109     C                   ELSE
535200960209     C* GENERAZIONE DA SPUNTE ENTRATA
535300950109     C                   MOVEL     ANMNPS        BRVNPS
535400950109     C                   ENDIF
535500950109     C*
535600950109     C                   ENDSR
535700950915     C*
535800950915     C* CREO SPUNTA DA SPUNTE ENTRATA --------------------------------*
535900950915     C     CRTSPE        BEGSR
536000950915     C                   MOVEL     KLNP          BRVLNP
536100950915     C                   MOVEL     KLNA          BRVLNA
536200950915     C                   MOVEL     KNRS          BRVNRS
536300950915     C                   MOVEL     KNSC          BRVNSC
536400950915     C                   MOVEL     WRVZNC        BRVZNC
536500950915     C                   MOVEL     WRVVUC        BRVVUC
536600990907     C                   MOVEL     WRVPUC        BRVPUC
536700950915     C                   MOVEL     WRVCAN        BRVCAN
536800950915     C                   MOVEL     WRVNPS        BRVNPS
536900950915     C*
537000950915     C                   ENDSR
537100960531     C*
537200960705     C* CHIUDO ANMOALIE DI DISGUIDO SE ESISTE BOLLA PARTENZA ---------*
537300020412     C     SRDISG        BEGSR
537400070918     C**** kblt27        setll     FNBLT27L                               36
537500070918    2C**** *in36         IFEQ      *ON
537600070918     c
537700070918     C     kblt27        chain(N)  FNBLT27L
537800070918     c                   if        %found(fnblt27l)
537900070918     c* Se bolla trovata e del terminal di partenza
538000070918     c*  chiudo le anomalie 200 55 56
538100070918     c     bltlnp        lookup    l1                                     36
538200070918     c                   if        *in36
538300020412     C**TROVATA BOLLA PARTENZA: CHIUDO ANOMALIE 200 55 e 56
538400020412     C                   MOVE      'N'           WCRT              1
538500020412     C                   CLEAR                   DSLR33
538600020412     C                   MOVE      ANMDAO        D33DCH
538700020412     C                   MOVEL     'AN'          D33CCH
538800020412     C                   MOVE      ANMNR0        D33NRR
538900020412     C                   CALL      'FNLR33R'
539000020412     C                   PARM                    DSLR33
539100970828     C**CHIUDO ANCHE ANOMALIA 55
539200020412     C                   Z-ADD     55            COMCAA
539300020412     C     kanm31        CHAIN(N)  FNANM                              21
539400020412     C     *in21         IFEQ      *OFF
539500020412     C                   CLEAR                   DSLR33
539600020412     C                   MOVE      ANMDAO        D33DCH
539700020412     C                   MOVEL     'M'           D33FSC
539800020412     C                   MOVEL     'AN'          D33CCH
539900020412     C                   MOVEL     'S'           D33MAC
540000020412     C                   MOVE      ANMNR2        D33NRR
540100020412     C                   CALL      'FNLR33R'
540200020412     C                   PARM                    DSLR33
540300020412     C                   END
540400020412     C**CHIUDO ANCHE ANOMALIA 56
540500020412     C                   Z-ADD     56            COMCAA
540600020412     C     kanm31        CHAIN(N)  FNANM                              21
540700020412     C     *in21         IFEQ      *OFF
540800020412     C                   CLEAR                   DSLR33
540900020412     C                   MOVE      ANMDAO        D33DCH
541000020412     C                   MOVEL     'M'           D33FSC
541100020412     C                   MOVEL     'AN'          D33CCH
541200020412     C                   MOVEL     'S'           D33MAC
541300020412     C                   MOVE      ANMNR2        D33NRR
541400020412     C                   CALL      'FNLR33R'
541500020412     C                   PARM                    DSLR33
541600020412     C                   END
541700020412     C                   Z-ADD     200           COMCAA
541800020412    2C                   END
541900070918    2C                   END
542000070918     c
542100020412     C                   ENDSR
542200020411     C*--- RICERCO LNP BOLLA SE COLLO con serie > 0  -----------------*
542300960531     C     RICLNP        BEGSR
542400960531     C*
542500960611     C                   CLEAR                   WOKBL
542600960611     C                   CLEAR                   WOKBT
542700960603     C                   MOVE      BRVLNP        WLNP
542800970811     C                   CLEAR                   WTAB5F
542900960531     C*
543000020411    1C     BRVNRS        IFGT      *ZEROS
543100150610     c     w_FilBsp      oreq      'S'
543200020412     c* CERCO LA BOLLA
543300020412    2C                   SELECT
543400020411     C     WALCF         WHENEQ    'N'
543500020411     C     KBRV4         CHAIN(N)  FNBLT27L                           36
543600020411     c     *in36         ifeq      *off
543700020411     C                   MOVE      BLTLNP        WLNP
543800020411     C                   MOVE      'S'           WOKBL             1
543900020411     C                   CLEAR                   WTAB5F
544000020411     c                   else
544100020606     C     KBtt37        CHAIN(N)  FNBTT37L                           36
544200020411     c     *in36         ifeq      *off
544300020411     C                   MOVE      BTTLNP        WLNP
544400020411     C                   MOVE      'S'           WOKBT             1
544500020411     C                   CLEAR                   WTAB5F
544600020411     c                   endif
544700020411     c                   endif
544800020411     C     WALCF         WHENEQ    'S'
544900020411     C     KBRV4         CHAIN     FNBLT27L                           36
545000020411     c     *in36         ifeq      *off
545100020411     C                   MOVE      BLTLNP        WLNP
545200020411     C                   MOVE      'S'           WOKBL             1
545300020411     C                   CLEAR                   WTAB5F
545400020411     c                   else
545500020606     C     KBtt37        CHAIN     FNBTT37L                           36
545600020411     c     *in36         ifeq      *off
545700020411     C                   MOVE      BTTLNP        WLNP
545800020411     C                   MOVE      'S'           WOKBT             1
545900020411     C                   CLEAR                   WTAB5F
546000020411     c                   endif
546100020411     c                   endif
546200020412    2C                   ENDSL
546300020412     c* Se non trovata bolla part. nè bolla transito GUARDO SE ESISTE LA
546400020412     c* BOLLA ARRIVI (in un ottica di As unico infatti se la spunta letta
546500020411     c* è un disguido potrei trovare la bolla in arrivo se sull'As in cui
546600020411     c* il collo è stato disguidato risiedono anche i dati del p.o. di
546700020411     c* arrivo del collo)
546800020412    2c     *in36         ifeq      *on
546900020411     c     kbrv4         chain     fnart27l                           36
547000020412    3c     *in36         ifeq      *off
547100020411     C                   MOVE      artlnp        WLNP
547200020411     C                   CLEAR                   WTAB5F
547300020412   x3c                   else
547400020411     c* BOLLA NON TROVATA IN NESSUN ARCHIVIO:
547500020412     c* SPUNTA PARTENZA NON DI DISGUIDO: verifico se è presente
547600020411     c                   exsr      cntspa
547700020412    4c     w105          ifeq      *blanks
547800020412     c* VADO SU TABELLA 5F se non ho trovato nemmeno la spunta partenza
547900020412     C                   movel     fgvlnp        W008A             8
548000020412     C                   move      brvnrs        W005A             5
548100020412     C                   movel     brvlnp        w005a
548200020412     C                   move      w005a         w008a
548300070215     C                   z-add     1             ii                4 0
548400020412     C     w008a         lookup    s5f(ii)                                31
548500020412     C**
548600020412     C* SE NON TROVATO CERCO COME SERIE = 99
548700020412    5C     *in31         ifeq      *off
548800020412     C                   move      99            W008A
548900020412     C                   z-add     1             ii
549000020412     C     w008a         lookup    s5f(ii)                                31
549100020412    5C                   endif
549200020412     C**
549300020412    5C     *in31         ifeq      *on
549400020412     C                   MOVEL     'S'           WTAB5F            1
549500020412     c     ii            occur     dse5f
549600020412     c                   move      e5f(1)        wlnp
549700020412    5c                   endif
549800020411     c*
549900020412    4c                   endif
550000020412    3c                   endif
550100020412    2c                   endif
550200020412    1c                   endif
550300971028     C                   ENDSR
550400971028     C*
550500970827     C*--- CONTROLLO SE C'E' UNA SPUNTA PARTENZA ---------------------*
550600970827     C     CNTSPA        BEGSR
550700970827     C                   MOVE      *BLANKS       W105              1
550800070123     C* Mi salvo dati record di fnbrv prima di leggere
550900070123     C                   MOVEL     DSBRV         DSBRVS
551000140618     C     KBRV03        SETLL     FNBRV05L
551100140618     C     KBRV03        READE(N)  FNBRV05L                               21
551200971028    1C     *IN21         DOWEQ     *OFF
551300960830     C     W105          ANDEQ     *BLANKS
551400971105    2C     BRVFGS        IFNE      FGVLNP
551500970827     C     BRVATR        ANDEQ     ' '
551600960830     C                   MOVE      '1'           W105
551700970827     C                   MOVEL     BRVFGS        WLNP
551800970827     C                   CLEAR                   WTAB5F
551900140618     C     KBRV03        READE(N)  FNBRV05L                               21
552000971028   X2C                   ELSE
552100140618     C     KBRV03        READE(N)  FNBRV05L                               21
552200971028    2C                   ENDIF
552300971028    1C                   ENDDO
552400070123     C* Reimposto campi di fnbrv con dati salvati
552500960830     C                   MOVEL     DSBRVS        DSBRV
552600960830     C                   ENDSR
552700970423     C*
552800130527     C* RICHIAMO FNLS45R PER AGGIORNAMENTO DATE SU BOLLE ------------*
552900970423     C     CALL44        BEGSR
553000070221     c                   clear                   fnls45ds
553100070221     c                   movel     'S'           dls45ric
553200070221     c                   move      brvpes        dls45pes
553300070221     c                   call      'FNLS45R'
553400070221     c                   parm                    fnls45ds
553500070222     c                   parm                    dsbrv
553600970423     C*
553700970509     C                   MOVEL     'S'           WFC44             1
553800970423     C                   ENDSR
553900970423     C*
554000970423     C* IMPOSTO LA KEY PER CHAIN -------------------------------------*
554100970423     C     IMPKEY        BEGSR
554200970423     C                   SELECT
554300970423     C     *IN40         WHENEQ    *ON
554400970423     C                   MOVEL     BLTFLS        KLNP
554500970423     C                   MOVEL     BLTLNA        KLNA
554600970423     C                   MOVEL     BLTNRS        KNRS
554700970423     C                   MOVEL     BLTNSC        KNSC
554800970423     C*
554900970423     C     *IN42         WHENEQ    *ON
555000970423     C                   MOVEL     ANMFLS        KLNP
555100970423     C                   MOVEL     ANMLNA        KLNA
555200970423     C                   MOVEL     ANMNRS        KNRS
555300970423     C                   MOVEL     ANMSCN        KNSC
555400970423     C*
555500970423     C     *IN41         WHENEQ    *ON
555600970423     C                   MOVEL     BTTFLS        KLNP
555700970423     C                   MOVEL     BTTLNA        KLNA
555800970423     C                   MOVEL     BTTNRS        KNRS
555900970423     C                   MOVEL     BTTNSC        KNSC
556000990917     C                   OTHER
556100970423     C                   MOVEL     BRVLNP        KLNP
556200970423     C                   MOVEL     BRVLNA        KLNA
556300970423     C                   MOVEL     BRVNRS        KNRS
556400970423     C                   MOVEL     BRVNSC        KNSC
556500970423     C                   ENDSL
556600970423     C                   ENDSR
556700010309     C*
556800010309     C* Aggiorno e sflaggo file O.R.M. -------------------------------*
556900010309     C     AGGORM        BEGSR
557000010309     C                   MOVEL     'M'           WTRC4
557100060220     C     KAR4          CHAIN     FIAR401L                           20
557200051207    1C     *IN20         IFEQ      *OFF
557300060220     C                   MOVEL     AR4NOT        DSBL4M
557400010309     C     KORM          CHAIN     FNORM01L                           20
557500060220     c* Se il p.o. di ritiro non è uguale alla linea di partenza,
557600051207     c*  aggiorno in questo momento
557700060220    2c                   if        not *in20 and ormpor<>AR4lnp
557800051207     c                   exsr      sr_Storico
557900051207     c* Scrivo fase 200
558000051207     c                   z-add     200           ormfao
558100051207     C                   Z-ADD     FGVDFV        ORMDFO
558200051207     C                   Z-ADD     235858        ORMOFO
558300051207     c                   exsr      rieorf
558400051207     c                   write     fnorf000
558500051207     c
558600060220     c                   z-add     AR4lnp        ormpor
558700070711      * se ho aggiornato il p.o. ritiro dell'ORM devo aggiornare anche FNORG
558800070711     c     korm          chain     fnorg01l
558900070711if  1c                   if        %found(fnorg01l)
559000070711      * cambio il p.o. ritiro
559100070711     c                   eval      orgpor = ormpor
559200070711     c                   update    fnorg000
559300070711    2c                   endif
559400051207    2c                   endif
559500051207     c
559600051207    2c     *IN20         IFEQ      *OFF
559700010312     c     ormfao        andne     900
559800010309     c                   z-add     900           ormfao
559900010309     C                   Z-ADD     FGVDFV        ORMDFO
560000010405     C                   Z-ADD     235959        ORMOFO
560100040909     C                   z-add     dateu         ORMDTT
560200010309     c                   exsr      rieorf
560300070711      * imposto ormeti
560400070711     c                   clear                   dfar
560500070711     c                   clear                   tibs02ds
560600070711     c                   eval      t02mod = 'C'
560700070711     c                   eval      t02sif = knsif
560800070711     c                   eval      t02cod = 'FAR'
560900070711     c                   movel(p)  ormfao        t02ke1
561000070711     c                   call      'TIBS02R'
561100070711     c                   parm                    kpjba
561200070711     c                   parm                    tibs02ds
561300070711     c                   eval      dfar = t02uni
561400070711     c                   eval      ormeti = d§farass
561500010309     c                   write     fnorf000
561600051207    2C                   ENDIF
561700051207    c
561800051207     C  n20              UPDATE    FNORM000
561900051207    1C                   ENDIF
562000010309     C                   ENDSR
562100010309     C*
562200010309     C* Riempio campi record fase O.R.M. -------------------------------*
562300010309     C     RIEORF        BEGSR
562400010309     C                   clear                   fnorf000
562500010309     C                   eval      ORFpoe = ormpoe
562600010309     C                   eval      ORFnsr = ormnsr
562700010309     C                   eval      ORFnor = ormnor
562800010309     C                   eval      ORFnrv = ormnrv
562900010309     C                   eval      ORFpog = ormpor
563000010309     C                   eval      ORFdae = ormdfo
563100010309     C                   eval      ORFore = ormofo
563200010309     C                   eval      ORFfar = ormfao
563300040909     C                   eval      ORFpue = knmus
563400040909     c                   If        OrmNdc > *Zeros
563500060516      * devo trovare il p.o. gestione distinta giusto
563600120607     c                   move      ormnpg        kdstnpg
563700120607     c                   move      ormndc        kdstnfv
563800060516      * prima provo con il p.o. ritiro
563900060516     c                   eval      kdstfgs = ormpor
564000120607     c     kdst          chain     fidst01l
564100060516      * se non lo trovo cerco se il p.o. ritiro è gestito
564200120607     c                   if        not %found(fidst01l)
564300060516     c                   clear                   dslv55
564400060516     c                   eval      d55tpt = '6'
564500060516     c                   move      ormpor        d55lin
564600060516     c                   eval      d55drf = ormddc
564700060516     c                   call      'FNLV55R'
564800060516     c                   parm                    dslv55
564900060516     c                   if        d55err = *blanks and d55tfa <> d55lin
565000060516     c                   move      d55tfa        kdstfgs
565100120607     c     kdst          chain     fidst01l
565200060516      * se non trovo neanche con il p.o. che gestisce metto il p.o. ritiro
565300060516      * teoricamente non dovrebbe succedere
565400120607     c                   if        not %found(fidst01l)
565500060516     c                   eval      kdstfgs = ormpor
565600060516     c                   endif
565700060516     c                   endif
565800060516     c                   endif
565900060516     c**!!!              Eval      OrfFgs = OrmPor
566000060516     c                   eval      orffgs = kdstfgs
566100040909     c                   Eval      OrfNdc = OrmNdc
566200040909     c                   Eval      OrfDdc = OrmDdc
566300040909     c                   EndIf
566400010309
566500010309     c                   ENDSR
566600051207     C*
566700051207     C* Scrivo lo storico orm ------------------------------------------*
566800051207     C     SR_Storico    BEGSR
566900051207     C                   clear                   fnorv000
567000051207     C                   eval      ORVpoe = ORMpoe
567100051207     C                   eval      ORVnsr = ORMnsr
567200051207     C                   eval      ORVnor = ORMnor
567300051207     C                   eval      ORVnrv = ORMnrv
567400051207     C                   eval      ORVtor = ORMtor
567500051207     C                   eval      ORVcor = ORMcor
567600051207     C                   eval      ORVrso = ORMrso
567700051207     C                   eval      ORVino = ORMino
567800051207     C                   eval      ORVcao = ORMcao
567900051207     C                   eval      ORVloo = ORMloo
568000051207     C                   eval      ORVpro = ORMpro
568100051207     C                   eval      ORVnao = ORMnao
568200051207     C                   eval      ORVcra = ORMcra
568300051207     C                   eval      ORVrsr = ORMrsr
568400051207     C                   eval      ORVinr = ORMinr
568500051207     C                   eval      ORVcar = ORMcar
568600051207     C                   eval      ORVlor = ORMlor
568700051207     C                   eval      ORVprr = ORMprr
568800051207     C                   eval      ORVnar = ORMnar
568900051207     C                   eval      ORVrer = ORMrer
569000051207     C                   eval      ORVter = ORMter
569100051207     C                   eval      ORVcrc = ORMcrc
569200051207     C                   eval      ORVrsc = ORMrsc
569300051207     C                   eval      ORVinc = ORMinc
569400051207     C                   eval      ORVloc = ORMloc
569500051207     C                   eval      ORVcac = ORMcac
569600051207     C                   eval      ORVprc = ORMprc
569700051207     C                   eval      ORVnac = ORMnac
569800051207     C                   eval      ORVffd = ORMffd
569900051207     C                   eval      ORVdar = ORMdar
570000051207     C                   eval      ORVorr = ORMorr
570100051207     C                   eval      ORVnam = ORMnam
570200051207     C                   eval      ORVncl = ORMncl
570300051207     C                   eval      ORVpkg = ORMpkg
570400051207     C                   eval      ORVvlm = ORMvlm
570500051207     C                   eval      ORVbnc = ORMbnc
570600051207     C                   eval      ORVblc = ORMblc
570700051207     C                   eval      ORVatt = ORMatt
570800051207     C                   eval      ORVmtc = ORMmtc
570900051207     C                   eval      ORVpag = ORMpag
571000051207     C                   eval      ORVksc = ORMksc
571100051207     C                   eval      ORVctr = ORMctr
571200051207     C                   eval      ORVddt = ORMddt
571300051207     C                   eval      ORVpor = ORMpor
571400051207     C                   eval      ORVzor = ORMzor
571500051207     C                   eval      ORVpoc = ORMpoc
571600051207     C                   eval      ORVno1 = ORMno1
571700051207     C                   eval      ORVno2 = ORMno2
571800051207     C                   eval      ORVcst = ORMcst
571900051207     C                   eval      ORVvcs = ORMvcs
572000051207     C                   eval      ORVfcs = ORMfcs
572100051207     C                   eval      ORVdfo = ORMdfo
572200051207     C                   eval      ORVofo = ORMofo
572300051207     C                   eval      ORVfao = ORMfao
572400051207     C                   eval      ORVsto = ORMsto
572500051207     C                   eval      ORVdst = ORMdst
572600051207     C                   eval      ORVnpg = ORMnpg
572700051207     C                   eval      ORVndc = ORMndc
572800051207     C                   eval      ORVddc = ORMddc
572900051207     C                   eval      ORVstp = ORMstp
573000051207     C                   eval      ORVrfa = ORMrfa
573100051207     C                   eval      ORVsto = ORMsto
573200070711     c                   eval      orveti = ormeti
573300051207     C                   eval      ORVspi = ORMspi
573400051207     C                   eval      ORVflo = ORMflo
573500051207     C                   movel     knmus         ORVutv
573600051207     C                   TIME                    W0140            14 0
573700051207     C                   MOVEL     W0140         orvorv
573800051207     C                   CLEAR                   WLBDAT
573900051207     C                   MOVE      W0140         G02DAT
574000051207     C                   CALL      'XSRDA8'
574100051207     C                   PARM                    WLBDAT
574200051207     C                   Z-ADD     G02INV        orvdtv
574300051207     C                   Z-ADD     G02INV        orvdtt
574400051207     C                   write     fnorv000
574500051207
574600051207     c                   ENDSR
574700950109     C*---------------------------------------------------------------*
574800950109     C*    OPERAZIONI INIZIALI                                        *
574900950109     C*---------------------------------------------------------------*
575000950109     C     *INZSR        BEGSR
575100950109     C*
575200950109     C     *ENTRY        PLIST
575300950109     C                   PARM                    KPJBA
575400130524     C                   MOVEL     KPJBU         FNLSU2DS
575500021014
575600021014     c                   Movel     Simfel        Varx1
575700021014     c                   Movel     Simfel        Var2x1
575800021014     c                   Movel     Simfel        Num31
575900021015     c                   Move      Simfel        Mbrfil4
576000021014
576100000111     C* SPOSTO SUBITO NUMERI FOGLIO IN CAMPI LUNGHI 6,0
576200000111     C                   Z-ADD     FGV2          FGV260
576300000111     C                   Z-ADD     NFV1          NFV160
576400950109     C*
576500950109     C                   Z-ADD     1             CODUT             1 0
576600950109     C                   CALL      'X§PARUT'
576700950109     C                   PARM                    UT§DSE
576800000428     C** S.I. IN CUI SONO
576900021010     C                   CLEAR                   DSBS55
577000000428     C                   MOVEL     KNSIF         I50PSI
577100000428     C                   MOVEL     'L'           I50TLA
577200021010     C                   CALL      'TIBS55R'
577300021010     C                   PARM                    DSBS55
577400950109     C**
577500950109     C*    CHIAVI                                                     *
577600950109     C**
577700950109     C     KBLP2         KLIST
577800950109     C                   KFLD                    BLPAAS
577900950109     C                   KFLD                    BLPLNP
578000950109     C                   KFLD                    BLPNRS
578100950109     C                   KFLD                    BLPNSP
578200060220     C     KAR4          KLIST
578300010309     C                   KFLD                    BLPAAS
578400010309     C                   KFLD                    BLPLNP
578500010309     C                   KFLD                    BLPNRS
578600010309     C                   KFLD                    BLPNSP
578700010309     C                   KFLD                    WTRC4
578800950516     C     KBTP2         KLIST
578900020606     c                   kfld                    btpflp
579000950516     C                   KFLD                    BTPAAS
579100950516     C                   KFLD                    BTPLNP
579200950516     C                   KFLD                    BTPNRS
579300950516     C                   KFLD                    BTPNSP
579400950109     C     KBLT3         KLIST
579500950109     C                   KFLD                    BLTAAS
579600950109     C                   KFLD                    BLTLNP
579700950109     C                   KFLD                    BLTNRS
579800950109     C                   KFLD                    BLTNSP
579900950109     C     KBTT3         KLIST
580000020605     c                   kfld                    fgvlnp
580100950109     C                   KFLD                    BTTAAS
580200950109     C                   KFLD                    BTTLNP
580300950109     C                   KFLD                    BTTNRS
580400950109     C                   KFLD                    BTTNSP
580500950109     C     KTAB          KLIST
580600950109     C                   KFLD                    CODUT
580700950109     C                   KFLD                    COD               2
580800950109     C                   KFLD                    KEY               8
580900950109     C     KTAB2         KLIST
581000950109     C                   KFLD                    CODUT
581100950109     C                   KFLD                    COD
581200950109     C     KBLT2         KLIST
581300950109     C                   KFLD                    WNPG
581400950109     C                   KFLD                    BLTFLS
581500950109     C                   KFLD                    BLTLNA
581600950109     C                   KFLD                    BLTNRS
581700950113     C                   KFLD                    BLTNSC
581800971107     C                   KFLD                    FGVLNP
581900130226     C     KBLT_xco      KLIST
582000130226     C                   KFLD                    WNPG
582100130226     C                   KFLD                    BLTFLS
582200130226     C                   KFLD                    BLTLNA
582300130226     C                   KFLD                    BLTNRS
582400130226     C                   KFLD                    BLTNSC
582500950109     C     KBTT          KLIST
582600950109     C                   KFLD                    WNPG
582700950109     C                   KFLD                    BTTFLS
582800950109     C                   KFLD                    BTTLNA
582900950109     C                   KFLD                    BTTNRS
583000950113     C                   KFLD                    BTTNSC
583100950915     C     KFVV          KLIST
583200950915     C                   KFLD                    WNPG5
583300950915     C                   KFLD                    WDFV
583400020916     C                   KFLD                    Kfgs
583500000728     C     KFVV1         KLIST
583600000728     C                   KFLD                    BRVNPG
583700001123     C                   KFLD                    WNFV05
583800000728     C                   KFLD                    BRVFGS
583900950109     C     KBRC2         KLIST
584000950109     C                   KFLD                    WNPG
584100000111     C                   KFLD                    FGV260
584200971107     C                   KFLD                    FGVLNP
584300950109     C                   KFLD                    BRVLNA
584400130527     C     K03BLP77      klist
584500020807     C                   kfld                    SIMfel
584600960212     C                   KFLD                    FGV2
584700960212     C                   KFLD                    BRVLNA
584800130531     C     Kblp77        KLIST
584900130531     C                   KFLD                    simfel
585000130531     C                   KFLD                    kfgv
585100130531     C                   KFLD                    savffv
585200130527     C     K02BLP77_9    KLIST
585300020807     C                   KFLD                    SIMfel
585400020807     C                   KFLD                    COMnfv
585500950109     C     KFVD2         KLIST
585600950109     C                   KFLD                    FGVLNP
585700950109     C                   KFLD                    FGV2
585800950109     C                   KFLD                    BRVLNA
585900950110     C                   KFLD                    WTRC
586000130530     C     KFVDbor       KLIST
586100130530     C                   KFLD                    SIMFEL
586200130530     C                   KFLD                    WNFV5
586300130530     C                   KFLD                    BLPLNA
586400130530     C                   KFLD                    WTRC
586500060707     C     KFV4e         KLIST
586600060707     C                   KFLD                    FV4LNP
586700060707     C                   KFLD                    FV4NFV
586800060707     C                   KFLD                    FV4EPA
586900060707     C                   KFLD                    FV4LAI
587000060707     C                   KFLD                    FV4TRC
587100011127     C*
587200011127     C     KTBE          KLIST
587300011127     C                   KFLD                    KCOD1
587400011127     C                   KFLD                    KKE1
587500130226     C     KTBE2         KLIST
587600130226     C                   KFLD                    KCOD1
587700130226     C                   KFLD                    KKE1
587800130226     C                   KFLD                    KKE2
587900130226     c
588000011127     C     *LIKE         DEFINE    TBECOD        KCOD1
588100011127     C     *LIKE         DEFINE    TBEKE1        KKE1
588200130226     C     *LIKE         DEFINE    TBEKE2        KKE2
588300130226     c
588400950109     C     KFTT          KLIST
588500950109     C                   KFLD                    WPDR
588600950109     C                   KFLD                    COMTSR
588700950109     C                   KFLD                    WNDC
588800950113     C                   KFLD                    WDRT
588900950109     C     KFTD          KLIST
589000950109     C                   KFLD                    COMPDR
589100950109     C                   KFLD                    COMTSR
589200950109     C                   KFLD                    COMNDC
589300950114     C                   KFLD                    COMDDC
589400950109     C                   KFLD                    CO2KSC
589500950113     C                   KFLD                    COMRSC
589600020416     C     KFTD1         KLIST
589700020416     C                   KFLD                    COMPDR
589800020416     C                   KFLD                    COMTSR
589900020416     C                   KFLD                    COMNDC
590000020416     C                   KFLD                    COMDDC
590100950109     C     KBLT          KLIST
590200021021     C                   KFLD                    wlnp              3 0
590300021021     C                   KFLD                    DFV               8 0
590400021021     C                   KFLD                    COMFIL            3 0
590500000731     C     KBLT1         KLIST
590600000731     C                   KFLD                    BLTAAS
590700000731     C                   KFLD                    BLTLNP
590800000731     C                   KFLD                    BLTNRS
590900000731     C                   KFLD                    BLTNSP
591000000731     C                   KFLD                    BLTNSC
591100000714     C     KBLTL         KLIST
591200000714     C                   KFLD                    DFV
591300000714     C                   KFLD                    COMFIL
591400000714     C                   KFLD                    BLTLNP
591500960704     C     KBLT27        KLIST
591600960704     C                   KFLD                    ANMFLS
591700960704     C                   KFLD                    ANMLNA
591800960704     C                   KFLD                    ANMNRS
591900960704     C                   KFLD                    ANMSCN
592000950109     C     KANM          KLIST
592100021021     C                   KFLD                    wlnp
592200021021     C                   KFLD                    COMCAA
592300950109     C                   KFLD                    COMFIL
592400040422     C     KANM02        KLIST
592500040422     C                   KFLD                    bttFLS
592600040422     C                   KFLD                    bttLNA
592700040422     C                   KFLD                    bttNRS
592800040422     C                   KFLD                    bttnsc
592900040422     C                   KFLD                    COMCAA            3 0
593000950418     C     KANM3         KLIST
593100950418     C                   KFLD                    BRVLNP
593200950418     C                   KFLD                    BRVLNA
593300950418     C                   KFLD                    BRVNRS
593400950418     C                   KFLD                    BRVNSC
593500950418     C                   KFLD                    COMCAA            3 0
593600020412     C     KANM31        KLIST
593700020412     C                   KFLD                    ANMFLS
593800020412     C                   KFLD                    ANMLNA
593900020412     C                   KFLD                    ANMNRS
594000020412     C                   KFLD                    ANMSCN
594100020412     C                   KFLD                    COMCAA            3 0
594200130527     C     KANM1         KLIST
594300130527     C                   KFLD                    BLTFLS
594400130527     C                   KFLD                    BLTLNA
594500130527     C                   KFLD                    BLTNRS
594600130527     C                   KFLD                    BLTNSC
594700130607     C                   KFLD                    comcaa
594800151118     C     KANM01        KLIST
594900151118     C                   KFLD                    BLpaas
595000151118     C                   KFLD                    BLplnp
595100151118     C                   KFLD                    BLpNRS
595200151118     C                   KFLD                    BLpNSp
595300151118     C                   KFLD                    comcaa
595400950109     C     KANM5         KLIST
595500950109     C                   KFLD                    ANMFLS
595600950109     C                   KFLD                    ANMLNA
595700950109     C                   KFLD                    ANMNRS
595800950109     C                   KFLD                    ANMSCN
595900971107     C                   KFLD                    FGVLNP
596000950109     C     KBRV          KLIST
596100950109     C                   KFLD                    WNPG
596200000111     C                   KFLD                    FGV260
596300971107     C                   KFLD                    FGVLNP
596400130527     C     KBRV1         KLIST
596500130527     C                   KFLD                    KNPG
596600130527     C                   KFLD                    BLTFLS
596700130527     C                   KFLD                    BLTLNA
596800130527     C                   KFLD                    BLTNRS
596900130527     C                   KFLD                    BLTNSC
597000130527     C                   KFLD                    anmfle
597100130530     C     KBRVbor       KLIST
597200130530     C                   KFLD                    KNPG
597300130530     C                   KFLD                    WNFV60
597400130530     C                   KFLD                    KFGS
597500130530     C                   KFLD                    BLPFLS
597600130530     C                   KFLD                    blplna
597700130530     C                   KFLD                    BLPNRS
597800130530     C                   KFLD                    BLTNSC
597900950418     C     KBRV2         KLIST
598000950418     C                   KFLD                    WNPG
598100950418     C                   KFLD                    WSANFV
598200950418     C                   KFLD                    WSAFGS
598300950418     C                   KFLD                    KLNA
598400950418     C                   KFLD                    KLNP
598500950418     C                   KFLD                    KNRS
598600950418     C                   KFLD                    KNSC
598700980209     C     KBRV01        KLIST
598800980209     C                   KFLD                    NPG1
598900000111     C                   KFLD                    NFV160
599000980209     C                   KFLD                    FGS1
599100980209     C                   KFLD                    KLNP
599200980209     C                   KFLD                    KLNA
599300980209     C                   KFLD                    KNRS
599400980209     C                   KFLD                    KNSC
599500960830     C     KBRV03        KLIST
599600960830     C                   KFLD                    WNPG
599700960830     C                   KFLD                    BRVLNP
599800960830     C                   KFLD                    BRVLNA
599900960830     C                   KFLD                    BRVNRS
600000960830     C                   KFLD                    BRVNSC
600100950109     C     KBRV3         KLIST
600200950109     C                   KFLD                    NPG1
600300000111     C                   KFLD                    NFV160
600400950109     C                   KFLD                    FGS1
600500950109     C                   KFLD                    COMFIL
600600950915     C     KBRV33        KLIST
600700970423     C                   KFLD                    KVVNPG
600800970423     C                   KFLD                    KVVNFV
600900970423     C                   KFLD                    KVVFGS
601000950915     C                   KFLD                    COMFIL
601100990917     C     KBRV32        KLIST
601200990917     C                   KFLD                    KVVNPG
601300990917     C                   KFLD                    KVVNFV
601400990917     C                   KFLD                    KVVFGS
601500950109     C     KBRV4         KLIST
601600950109     C                   KFLD                    KLNP
601700950109     C                   KFLD                    KLNA
601800950109     C                   KFLD                    KNRS
601900950109     C                   KFLD                    KNSC
602000950118     C     KBRV5         KLIST
602100950109     C                   KFLD                    WNPG
602200950109     C                   KFLD                    KLNP
602300950109     C                   KFLD                    KLNA
602400950109     C                   KFLD                    KNRS
602500950109     C                   KFLD                    KNSC
602600971107     C                   KFLD                    FGVLNP
602700000731     C     KBRV55        KLIST
602800000731     C                   KFLD                    WNPG
602900000731     C                   KFLD                    KLNP
603000000731     C                   KFLD                    KLNA
603100000731     C                   KFLD                    KNRS
603200000731     C                   KFLD                    KNSC
603300040802     C                   KFLD                    WFGSspu
603400950118     C     KBRV6         KLIST
603500950118     C                   KFLD                    KLNP
603600950118     C                   KFLD                    KLNA
603700950118     C                   KFLD                    KNRS
603800950118     C                   KFLD                    KNSC
603900971107     C                   KFLD                    FGVLNP
604000150603     C     KBRV7         KLIST
604100150603     C                   KFLD                    BLTFLS
604200150603     C                   KFLD                    BLTLNA
604300150603     C                   KFLD                    BLTNRS
604400150603     C                   KFLD                    BLTNSC
604500150924     C*****              KFLD                    FGVLNP
604600020807     c     K02FGV01      klist
604700020807     c                   kfld                    fgv2
604800020807     c                   kfld                    SIMfel
604900010309     c*
605000010309     c     korm          klist
605100010309     c                   kfld                    §b4poe
605200010309     c                   kfld                    §b4nsr
605300010309     c                   kfld                    §b4nor
605400010309     c                   kfld                    §b4nrv
605500010611     c     kbrve         klist
605600010611     c                   kfld                    brvnpg
605700010611     c                   kfld                    brvnfv
605800010611     c                   kfld                    brvfgs
605900010611     c                   kfld                    brvlnp
606000010611     c                   kfld                    brvlna
606100010611     c                   kfld                    brvnrs
606200010611     c                   kfld                    brvnsc
606300020605     C     KBtt38        KLIST
606400020605     c                   kfld                    fgvlnp
606500020605     C                   KFLD                    DFV               8 0
606600020605     C                   KFLD                    COMFIL            3 0
606700020606     C     KBtt37        KLIST
606800020606     c                   kfld                    fgvlnp
606900020606     C                   KFLD                    KLNP
607000020606     C                   KFLD                    KLNA
607100020606     C                   KFLD                    KNRS
607200020606     C                   KFLD                    KNSC
607300040319     C     KBTT1         KLIST
607400040319     c                   kfld                    BTTFLP
607500040319     C                   KFLD                    BTTAAS
607600040319     C                   KFLD                    BTTLNP
607700040319     C                   KFLD                    BTTNRS
607800040319     C                   KFLD                    BTTNSP
607900040319     C                   KFLD                    BTTNSC
608000040310     c     Kar5          klist
608100040311     c                   kfld                    bltaas
608200040311     C                   KFLD                    bltlnp
608300040311     C                   KFLD                    bltnrs
608400040311     C                   KFLD                    bltnsp
608500040310     C                   KFLD                    ktrd
608600040310     c                   movel     'GEN'         ktrd
608700060516
608800060516     c     kdst          klist
608900120607     c                   kfld                    kdstnpg
609000120607     c                   kfld                    kdstnfv
609100060516     c                   kfld                    kdstfgs
609200060516
609300950118     C***
609400950118     C*  CODICI TRATTAMENTO MERCE CON §1BFG5 = "N"
609500950118     C***
609600960202     C* NON CARICO SE RICHIAMATO DA MANUTENZIONE
609700960202     C     PA2FLG        IFNE      'M'
609800960202     C* APERTURA FILE
609900960202     C                   EXSR      OPENF
610000130530     c
610100960202     C**
610200950118     C                   Z-ADD     1             X
610300950118     C                   MOVEL     '1B'          COD
610400950118     C     KTAB2         CHAIN     TABEL00F                           31
610500950118     C*
610600950118     C     *IN31         DOWEQ     *OFF
610700950118     C                   MOVEL     TBLUNI        DS1B
610800950118     C     §1BFG5        IFEQ      'N'
610900950118     C                   MOVEL     TBLKEY        CTM(X)
611000950118     C                   ADD       1             X
611100950118     C                   ENDIF
611200950118     C     KTAB2         READE     TABEL00F                               31
611300950118     C                   ENDDO
611400050524     c
611500050524     c* Controllo riempimento schiera
611600050524     c                   clear                   trul0sds
611700050524     c                   eval      i0sski='CTM'
611800050524     c                   eval      i0sele=%elem(ctm)
611900050524     c                   eval      i0spie=x
612000050524     c                   eval      i0scod='  1B '
612100050524     c                   eval      i0sfile='TABEL00F  '
612200050524     c                   eval      i0ssif=knsif
612300130718     c                   eval      i0spgm='FNLSU2R'
612400050524     c                   movel     trul0sds      kpjbu
612500050524     c                   call      'TRUL0SR'
612600050524     c                   parm                    kpjba
612700960531     C***
612800960531     C** TABELLA 5F - FILIALI CONFERMA BOLLE DA DISK
612900960531     C***
613000960531     C                   Z-ADD     0             X
613100960531     C                   MOVEL     '5F'          COD
613200960531     C     KTAB2         SETLL     TABEL
613300960531     C     KTAB2         READE     TABEL                                  31
613400960531     C*
613500960531    1C     *IN31         DOWEQ     '0'
613600960531    2C     TBLFLG        IFEQ      ' '
613700050524     c* elaboro soltanto i record con le prima 3 cifre <>'   '
613800050524     c*  perchè nella 5F ci sono 2 diversi tipi record: quello con i '   '
613900050524     c*   davanti qui non sono usati
614000050524     c                   movel     tblkey        w003a             3
614100050524     c                   if        w003a<>'   '
614200960531     C                   ADD       1             X
614300960531     C                   MOVEL     TBLKEY        S5F(X)
614400960531     C     X             OCCUR     DSE5F
614500960701     C                   MOVEA     TBLUNI        E5F
614600080109      * - Carica comunque il n° max di elementi previsti nella schiera.
614700080109      *   Se superato il limite massimo di riempimento, verrà inviato
614800080109      *   un messaggio di avvertimento - vedi chiamata al pgm TRUL0SR:
614900080109      *   verificherà lui la percentuale di riempimento...).
615000080109     C     X             IFGE      $MaxElem
615100960531     C                   SETON                                        31
615200960531     C                   ENDIF
615300050524     C                   ENDIF
615400960531    2C                   ENDIF
615500960531     C*
615600960531     C  N31KTAB2         READE     TABEL                                  31
615700960531    1C                   ENDDO
615800050524     c**
615900050524     c                   clear                   trul0sds
616000050524     c                   eval      i0sski='S5F'
616100050524     c                   eval      i0sele=%elem(s5f)
616200050524     c                   eval      i0spie=x
616300050524     c                   eval      i0scod='  5F '
616400050524     c                   eval      i0sfile='TABEL00F  '
616500050524     c                   eval      i0ssif=knsif
616600130718     c                   eval      i0spgm='FNLSU2R'
616700050524     c                   movel     trul0sds      kpjbu
616800050524     c                   call      'TRUL0SR'
616900050524     c                   parm                    kpjba
617000950118     C***
617100950118     C*  CODICI PISTOLA CON PISTOLA AUTOMATICA
617200950118     C***
617300950118     C                   Z-ADD     1             X
617400970818     C                   Z-ADD     1             Y                 3 0
617500950118     C                   MOVEL     '7J'          COD
617600950118     C     KTAB2         CHAIN     TABEL00F                           31
617700950118     C*
617800950118     C     *IN31         DOWEQ     *OFF
617900950118     C                   MOVEL     TBLUNI        DS7J
618000950118     C     §7JRPS        IFEQ      'A'
618100950118     C                   MOVEL     TBLKEY        NPS(X)
618200950118     C                   ADD       1             X
618300950118     C                   ENDIF
618400970818     C     §7JFSL        IFEQ      'F'
618500970818     C                   MOVEL     TBLKEY        NPF(Y)
618600970818     C                   ADD       1             Y
618700970818     C                   ENDIF
618800950118     C     KTAB2         READE     TABEL00F                               31
618900950118     C                   ENDDO
619000011127     C*
619100011127     C*  tabelle GEDCN x reperimento divisa conteggi padroncini/tariffe
619200011127     C                   movel     'GED'         kcod1
619300011127     C                   movel     *blanks       kke1
619400011127     C                   movel     'CN'          kke1
619500011127     C     ktbe          chain     tntbe01l                           31
619600011127     C                   clear                   dgedcn
619700011127     C  N31              movel     tbeuni        dgedcn
619800130227     c
619900121119     C*  Tabella DGD    x reperimento data dal/al per conferma autoamti
620000041216     c*                 prepagati
620100041217     c                   Clear                   Tibs02Ds
620200121119     c                   Clear                   ddgd
620300041217     c                   Eval      T02Mod = 'C'
620400041217     c                   Eval      T02Sif = Knsif
620500121119     c                   Eval      T02Cod = 'DGD'
620600121119     c                   Movel(p)  '1'           T02Ke1
620700041217     c                   Call      'TIBS02R'
620800041217     c                   Parm                    Kpjba
620900041217     c                   Parm                    Tibs02Ds
621000041217     c                   If        T02Err = *Blanks
621100121119     c                   Movel     T02Uni        ddgd
621200041217     c                   EndIf
621300000619     C*
621400060710     C*  tipi record di FV4 da trasmettere in sede
621500060710     C***
621600060710     C                   Z-ADD     1             X
621700060710     C                   MOVEL     '3B'          COD
621800060710     C     KTAB2         CHAIN     TABEL00F                           31
621900060710     C*
622000060710     C     *IN31         DOWEQ     *OFF
622100060710     c                   if        tblflg=' '
622200060710     C                   MOVEL     TBLUNI        DS3B
622300060710     C     §3btrs        IFEQ      'S'
622400060710     C                   MOVEL     TBLKEY        fv4S(X)
622500060710     C                   ADD       1             X
622600060710     C                   ENDIF
622700060710     C                   ENDIF
622800060710     C     KTAB2         READE     TABEL00F                               31
622900060710     C                   ENDDO
623000950109     C***
623100950109     C*  R I C E R C A   C A P O C O N T I
623200950109     C***
623300040826     C                   DO        50            X                 4 0
623400950109     C                   MOVE      TCU(X)        TCUDS
623500950109     C     F56           CABNE     'CG'          END1
623600950109     C     F4            COMP      '1'                                    21
623700950109     C     F4            COMP      '2'                                    22
623800950109     C     F4            COMP      '3'                                    23
623900950109     C     F4            COMP      '6'                                    27
624000950109     C** 1 CLIENTI   21
624100950109     C** 2 FORNITORI 22
624200950109     C** 3 AGENTI    23
624300950109     C     F3            COMP      '0'                                242425
624400950109     C     F3            COMP      'I'                                    26
624500950109     C** 0 ITALIA   25
624600950109     C** 1 ESTERO   24
624700950109     ** I CAPO CONTO IVA
624800950109     C   21
624900950109     CAN 24              Z-ADD     KCU(X)        KCE               4 0
625000950109     C   21
625100950109     CAN 25              Z-ADD     KCU(X)        KCI               4 0
625200950109     C   22
625300950109     CAN 24              Z-ADD     KCU(X)        KFE               4 0
625400950109     C   22
625500950109     CAN 25              Z-ADD     KCU(X)        KFI               4 0
625600950109     C   23
625700950109     CAN 24              Z-ADD     KCU(X)        KAE               4 0
625800950109     C   23
625900950109     CAN 25              Z-ADD     KCU(X)        KAI               4 0
626000950109     C   26              Z-ADD     KCU(X)        KIVA              4 0
626100950109     C   27              Z-ADD     KCU(X)        KBNA              4 0
626200950109     C     END1          TAG
626300950109     C                   END
626400960202     C**
626500960202     C                   ENDIF
626600950109     C*
626700950109     C* SPENGO GLI INDICATORI USATI PER LA RICERCA CAPOCONTI
626800950109     C                   SETOFF                                       212223
626900950109     C                   SETOFF                                       242526
627000950515     C                   SETOFF                                       273132
627100950109     C*
627200950109     C* DEFINIZIONE CAMPI
627300970423     C     *LIKE         DEFINE    BRVNPS        WNPS
627400970528     C     *LIKE         DEFINE    BRVNPS        SPUNPS
627500970423     C     *LIKE         DEFINE    FVVNPG        KVVNPG
627600000111     C     *LIKE         DEFINE    BRVNFV        KVVNFV
627700001121     C     *LIKE         DEFINE    BRVNFV        SAVNFV
627800001123     C     *LIKE         DEFINE    fvvnfv        wnfv05
627900970423     C     *LIKE         DEFINE    FVVFGS        KVVFGS
628000970423     C**
628100970423     C     *LIKE         DEFINE    FVDCLA        NCLU
628200950109     C     *LIKE         DEFINE    FVDKFA        PKGU
628300950113     C     *LIKE         DEFINE    FVDVL1        VOLU
628400950113     C     *LIKE         DEFINE    FVDSNA        WSPEN
628500130530     C     *LIKE         DEFINE    FVDSNA        WSPE
628600950113     C     *LIKE         DEFINE    FVDSNA        WSPET
628700950110     C     *LIKE         DEFINE    FVDTRC        WTRC
628800950109     C*
628900950109     C     *LIKE         DEFINE    FTTPDR        COMPDR
629000950109     C     *LIKE         DEFINE    FTTPDR        WPDR
629100950109     C     *LIKE         DEFINE    FTTTSR        COMTSR
629200950109     C     *LIKE         DEFINE    FTTNDC        COMNDC
629300950109     C     *LIKE         DEFINE    FTTNDC        WNDC
629400950109     C     *LIKE         DEFINE    FTTSET        COMSET
629500020326     C     *LIKE         DEFINE    FTDstp        wstp
629600950109     C     *LIKE         DEFINE    FTDKSC        CO2KSC
629700950113     C     *LIKE         DEFINE    FTDRSC        COMRSC
629800021111     C     *LIKE         DEFINE    FTDVLU        WFVDVOL
629900021111     C     *LIKE         DEFINE    FTDVLU        WVOL
630000021108     C     *LIKE         DEFINE    FTDpkl        Wpes
630100021111     C     *LIKE         DEFINE    FTDpkl        WFVDPES
630200950114     C     *LIKE         DEFINE    FTDDDC        COMDDC
630300950109     C*
630400960124     C     *LIKE         DEFINE    FGVLNP        SA2LNP
630500960124     C     *LIKE         DEFINE    FGVNFV        SA2NFV
630600000731     C     *LIKE         DEFINE    FGVLNP        WFGSFV
630700960124     C     *LIKE         DEFINE    FVARTP        SA2RTP
630800960124     C     *LIKE         DEFINE    FGVDFV        WDTP
630900950915     C     *LIKE         DEFINE    FGVDFV        WDFV
631000950113     C     *LIKE         DEFINE    BRVNPG        WNPG
631100950915     C     *LIKE         DEFINE    BRVNPG        WNPG5
631200950109     C     *LIKE         DEFINE    BRVZNC        WRVZNC
631300950109     C     *LIKE         DEFINE    BRVVUC        WRVVUC
631400990906     C     *LIKE         DEFINE    BRVPUC        WRVPUC
631500950109     C     *LIKE         DEFINE    BRVCAN        WRVCAN
631600950915     C     *LIKE         DEFINE    BRVNPS        WRVNPS
631700950109     C     *LIKE         DEFINE    BRVLNP        KLNP
631800950109     C     *LIKE         DEFINE    BRVLNA        KLNA
631900950109     C     *LIKE         DEFINE    BRVNRS        KNRS
632000950109     C     *LIKE         DEFINE    BRVNSC        KNSC
632100950418     C     *LIKE         DEFINE    BRVFGS        WSAFGS
632200950418     C     *LIKE         DEFINE    BRVNFV        WSANFV
632300000111     C     *LIKE         DEFINE    BRVNFV        FGV260
632400000111     C     *LIKE         DEFINE    BRVNFV        NFV160
632500960604     C     *LIKE         DEFINE    BRVLNP        WLNP
632600960905     C     *LIKE         DEFINE    BRVNPG        NPGP
632700960905     C     *LIKE         DEFINE    BRVNFV        NFVP
632800960905     C     *LIKE         DEFINE    BRVLNP        LNPP
632900960905     C     *LIKE         DEFINE    BRVLNA        LNAP
633000960905     C     *LIKE         DEFINE    BRVNRS        NRSP
633100960905     C     *LIKE         DEFINE    BRVNSC        NSCP
633200960905     C     *LIKE         DEFINE    BRVNPS        NPSP
633300960905     C     *LIKE         DEFINE    BRVVUC        VUCP
633400990623     C     *LIKE         DEFINE    BRVPUC        PUCP
633500950109     C     *LIKE         DEFINE    BLPCBO        SAVCBO
633600950623     C*
633700980210     C     *LIKE         DEFINE    D55TFA        LNATFA
633800971107     C     *LIKE         DEFINE    D55TFP        LNAFEL
633900950707     C     *LIKE         DEFINE    BTTDUT        WDTPAR
634000970703     C     *LIKE         DEFINE    D55LIN        WLIN
634100060220     C     *LIKE         DEFINE    AR4TRC        WTRC4
634200980126     C*
634300980126     C     *LIKE         DEFINE    BRVLNA        WLNA
634400980209     C     *LIKE         DEFINE    DSBRV         SVBRVE
634500070123     C     *LIKE         DEFINE    DSBRV         dsbrvs
634600990623     C                   Z-ADD     0             COMDFS            8 0
634700990623     C                   Z-ADD     0             COMHMS            4 0
634800950621     C*
634900150609      * verifico se presenti simfel o suoi secondi livello
635000150609      * nel file della sospensione automatica per colli di clienti
635100150609      * che partono da più filiali
635200150609     c                   eval      w_FilBsp=*blanks
635300151215     c                   eval      w_FilBsps=*blanks
635400151215     c                   clear                   w_SiBsp
635500151215     c                   clear                   w_bsptip
635600150610     c     'S'           setll     fibsp03l
635700150610     c     'S'           reade     fibsp03l
635800151215     c                   dow       not %eof(fibsp03l) and w_filbsps=*blanks
635900150609     c                   z-add     1             Ix
636000150609     c                   dow       Ix<=%elem(sbsp) and sbsp(Ix)>0
636100151215     c                             and w_FilBsps=*blanks
636200150609     c                   if        %lookup(sbsp(Ix):L1)>0
636300150609     c                   eval      w_FilBsp='S'
636400151215     c                   if        bspaut=*blanks
636500151215     c                   eval      w_FilBsps='S'
636600151215     c                   endif
636700150609     c                   endif
636800150609     c                   add       1             Ix
636900150609     c                   enddo
637000150610     c     'S'           reade     fibsp03l
637100150610     c                   enddo
637200150703
637300150703     c                   Clear                   tibs02ds
637400150703     c                   Clear                   dmradan
637500150703     c                   Movel     'C'           T02Mod
637600150703     c                   Movel     KNSIF         T02Sif
637700150703     c                   Movel     'MRA'         T02Cod
637800150703     c                   Movel     'FNLSU3R'     T02Ke1
637900150703     c                   Call      'TIBS02R'
638000150703     c                   Parm                    Kpjba
638100150703     c                   Parm                    tibs02ds
638200150703    2c                   If        T02Err = *Blanks
638300150703     c                   Movel     T02Uni        Dmradan
638400150703    2c                   endif
638500150703     C***
638600150703     C* CARICO DATI VARIABILI PARTENZE
638700150703     C***
638800150703     C                   MOVEL     '3I'          COD
638900150703     C                   MOVEL     '1       '    KEY
639000150703     C     KTAB          CHAIN     TABEL00F
639100150703     c                   if        %found(tabel00f)
639200150703     C                   MOVEL     TBLUNI        DS3I
639300150703     c                   else
639400150703     C                   CLEAR                   DS3I
639500150703     c                   endif
639600150609
639700021016      * Recupero le date delle statistiche
639800021016     c                   Clear                   Tibs02Ds
639900021016     c                   Clear                   dSdf
640000021016     c                   Eval      T02Mod = 'C'
640100021016     c                   Eval      T02Sif = Knsif
640200021016     c                   Eval      T02Cod = 'SDF'
640300021016     c                   Movel(p)  Simfel        T02Ke1
640400021016     c                   Call      'TIBS02R'
640500021016     c                   Parm                    Kpjba
640600021016     c                   Parm                    Tibs02Ds
640700021016     c                   If        T02Err = *Blanks
640800021016     c                   Movel     T02Uni        dSdf
640900021016     c                   EndIf
641000950109     C*
641100950109     C                   SETON                                        OFOE
641200950109     C* GIRO UDATE
641300950113     C                   TIME                    W0140            14 0
641400050707     C***                MOVEL     W0140         WTIME             6 0
641500950109     C                   MOVE      W0140         WDTGIO            8 0
641600950109     C                   Z-ADD     WDTGIO        G02DAT
641700950109     C                   MOVEL     *BLANK        G02ERR
641800950109     C                   CALL      'XSRDA8'
641900950109     C                   PARM                    WLBDAT
642000950109     C                   MOVEL     G02INV        DATEU             8 0
642100950109     C*
642200950109     C                   MOVEL     'R'           COMTSR
642300950109     C                   Z-ADD     0             DFV
642400950113     C                   CLEAR                   SAVSPE
642500970124     C* CAMPO CHE PASSO VUOTO AL PGM FNLSA2R3
642600970124     C                   CLEAR                   WWLNA
642700030107
642800950109     C                   ENDSR
642900070827     C**********************************************************************
643000070827     C*  P A D R O N C I N I
643100070827     C**********************************************************************
643200070827     C* CARICO I CODICI PADRONCINI PER I QUALI E' NECESSARIO CREARE I
643300070827     C*   FILE fiftt00F E fiftd00F
643400070827     C* CARICO ANCHE QUELLI CON MERCE PORTATA A MAGAZZINO
643500070827     C* CARICO ANCHE I PADRONCINI DIPENDENTI
643600070827     c     CarPAD        BEGSR
643700070827     c                   movel     fgvdfv        apddfv
643800070827     c
643900070827     C                   Z-ADD     1             X
644000070827     C                   Z-ADD     1             Y
644100070827     C                   CALL      'FNLSA2R2'
644200070827     C                   PARM                    APDPDR            7 0
644300070827     C                   PARM                    APDETM            1
644400070827     C                   PARM                    APDETP            1
644500070827     C                   PARM                    APDPDD            1
644600070827     c                   Parm                    ApdScm            1
644700070827     c                   Parm                    ApdScp            1
644800070827     C                   PARM                    WFINE             1
644900070827     C                   PARM                    simfel            3 0
645000070827     C                   PARM                    apddfv            8 0
645100070827     C*
645200070827     C     WFINE         DOWNE     'S'
645300140620     C**   APDPDD        IFEQ      *BLANKS
645400070827     C                   MOVEL     APDPDR        PDR(X)
645500070827     C                   MOVEL     APDETM        ETM(X)
645600070827     C                   MOVEL     APDETP        ETP(X)
645700070827     c                   Movel     ApdScm        Scm(x)
645800070827     c                   Movel     ApdScp        Scp(x)
645900070827     C                   ADD       1             X
646000140620     C**                 ELSE
646100140620     C**                 MOVEL     APDPDR        PDD(Y)
646200140620     C**                 ADD       1             Y
646300140620     C**                 END
646400070827     C*
646500070827     C                   CALL      'FNLSA2R2'
646600070827     C                   PARM                    APDPDR
646700070827     C                   PARM                    APDETM
646800070827     C                   PARM                    APDETP
646900070827     C                   PARM                    APDPDD
647000070827     c                   Parm                    ApdScm            1
647100070827     c                   Parm                    ApdScp            1
647200070827     C                   PARM                    WFINE             1
647300070827     C                   PARM                    simfel            3 0
647400070827     C                   PARM                    apddfv            8 0
647500070827     C                   ENDDO
647600070827     c
647700070827     c* Controllo riempimento schiera
647800070827     c                   clear                   trul0sds
647900070827     c                   eval      i0sski='PDR/ETM/ETP/...'
648000070827     c                   eval      i0sele=%elem(pdr)
648100070827     c                   eval      i0spie=x-1
648200070827     c                   eval      i0sfile='FNAPD00F  '
648300070827     c                   eval      i0ssif=knsif
648400130718     c                   eval      i0spgm='FNLSU2R'
648500070827     c                   movel     trul0sds      kpjbu
648600070827     c                   call      'TRUL0SR'
648700070827     c                   parm                    kpjba
648800070827     c
648900140620     c**                 clear                   trul0sds
649000140620     c**                 eval      i0sski='PDD'
649100140620     c**                 eval      i0sele=%elem(pdD)
649200140620     c**                 eval      i0spie=y-1
649300140620     c**                 eval      i0sfile='FNAPD00F  '
649400140620     c**                 eval      i0ssif=knsif
649500140620     c**                 eval      i0spgm='FNLSU2R'
649600140620     c**                 movel     trul0sds      kpjbu
649700140620     c**                 call      'TRUL0SR'
649800140620     c**                 parm                    kpjba
649900130530     c
650000130530     c* in caso di parzializzazione per cod cliente
650100130530     c* Carico la £6 della linea partenza FGV per vedere se ha clienti abilita
650200130530     c*  ti alla borderizzazione
650300130530     c                   clear                   L6
650400130530     c                   if        pa2ccm>*zeros
650500130530     c                   clear                   trul06ds
650600130530     c                   eval      d06cod = '£6'
650700130530     c                   movel     simfel        d06key
650800130530     c                   move      'S'           d06key
650900130530     c                   movel     fgvdfv        d06drf
651000130530     c                   movel     'L'           d06tla
651100130530     c                   movel     trul06ds      kpjbu
651200130530     c                   call      'TRUL06R'
651300130530     c                   parm                    kpjba
651400130530     c                   movel     kpjbu         trul06ds
651500151127
651600151127     c* Carico eventuale tabella con i codici collegati da borderizzare
651700151127     C                   MOVEL     'CLF'         kcod1
651800151127     c                   z-add     1             zz
651900151127     C                   eval      kke1=pa2ccm
652000151127     c     ktbe          setll     tntbe01l
652100151127     c     ktbe          reade     tntbe01l
652200151127    2c                   dow       not %eof(tntbe01l)
652300151127     c                   if        tbeatb=' '
652400151127     c                   movel     tbeke2        ccmb(zz)
652500151127     c                   add       1             zz
652600151127     c                   endif
652700151127     c     ktbe          reade     tntbe01l
652800151127     c                   enddo
652900151127     c
653000130530     c                   endif
653100130530     c
653200070827     c                   ENDSR
653300150603     C**********************************************************************
653400150603     C*  FIBSP00F - lnp/clienti con sospensione bolle
653500150603     C**********************************************************************
653600150603     c     sr_fibsp      begsr
653700170327     c                   clear                   w_SiBsp
653800150703     c                   clear                   w_bsptip
653900150603     c     w_clibo       chain     fibsp02l
654000151214     c                   if        %found(fibsp02l) and bspfpa='S'
654100151214     c                                              and bspaut=*blanks
654200151215     c                             and %lookup(blplnp:sbsp)>0
654300150603     C                   eval      w_SiBsp='S'
654400150703     c                   eval      w_bsptip='K'
654500150603     c                   else
654600150603     c     blplnp        chain     fibsp01l
654700150603     c                   if        %found(fibsp01l) and bspfpa='S'
654800151214     c                                              and bspaut=*blanks
654900150603     C                   eval      w_SiBsp='S'
655000150703     c                   eval      w_bsptip='L'
655100150603     c                   endif
655200150603     c                   endif
655300150603     c                   endsr
655400150703     C**********************************************************************
655500150703     C*  Memorizzazinoe clienti/lnp con bolle in sosp. oltre xx giorni
655600150703     C**********************************************************************
655700150703     c     sr_MemoEmail  begsr
655800150703    1c                   select
655900150703     c* SOSPENSIONE per CLIENTE
656000150703   WHc                   when      w_bsptip='K'
656100150703     c                   eval      indy=%lookup(sav_clibo:s_bspk)
656200150703    2c                   if        indy=0
656300150703     c                   eval      indy=%lookup(0:s_bspk)
656400150703     c* memorizzo cod cliente
656500150703     c                   eval      s_bspk(indy)=sav_clibo
656600150703    2c                   endif
656700150703     c* memorizzo numero di spedizioni in sosp oltre xx giorni
656800150703     c                   add       1             s_bspkbo(indy)
656900150703     c* memorizzo la data più bassa di immissione
657000150703    2c                   if        s_BSPkD(indy)=0 or
657100150703     c                             blpdim>s_BSPkD(indy)
657200150703     c                   eval      s_BSPkD(indy)=blpdim
657300150703    2c                   endif
657400150703     c* SOSPENSIONE per LINEA DI PARTENZA
657500150703   WHc                   when      w_bsptip='L'
657600150703     c                   eval      indy=%lookup(sav_lnp:s_bspL)
657700150703    2c                   if        indy=0
657800150703     c                   eval      indy=%lookup(0:s_bspL)
657900150703     c* memorizzo cod cliente
658000150703     c                   eval      s_bspL(indy)=sav_lnp
658100150703    2c                   endif
658200150703     c* memorizzo numero di spedizioni in sosp oltre xx giorni
658300150703     c                   add       1             s_bspLbo(indy)
658400150703     c* memorizzo la data più bassa di immissione
658500150703    2c                   if        s_BSPlD(indy)=0 or
658600150703     c                             blpdim>s_BSPlD(indy)
658700150703     c                   eval      s_BSPlD(indy)=blpdim
658800150703    2c                   endif
658900150703    1c                   endsl
659000150703     c
659100150703     c                   endsr
659200150703     c*-------------------------------------------------------------------
659300150703     c     INV_email     BEGSR
659400150703     c
659500150703     c                   clear                   writerig
659600150703     c                   if        not %open(prtemail)
659700150703     C                   EXSR      SR_OPENPRTF
659800150703     C                   endif
659900150703     c
660000150703     c                   exsr      Routend
660100150703     c
660200150703     c                   ENDSR
660300151022       //--------------------------------------------------------------
660400151022       //?  Errore per bolla non trovata
660500151022       //--------------------------------------------------------------
660600151022     c     Sr_errore     BEGSR
660700151022
660800151022     C                   movel     *blanks       wrkEml          253
660900151022     C                   movel     *blanks       wrkEmlCc        253
661000151022     C                   movel     *blanks       wrkMsg         5000
661100151022     C                   movel     *blanks       wrkOgg           44
661200151022     C                   movel     *blanks       wrkesito          1
661300151022     c
661400151022     C                   MOVEL     MSG(5)        wrkogg
661500151022     C                   MOVEL(P)  MSG(6)        wrkmsg
661600151022     c                   eval      %subst(wrkmsg:43:16)=
661700151022     c                             %editc(bltspe:'X')
661800151022     c                   eval      wrkemlcc ='ced@brt.it'
661900151022     c                   eval      wrkeml='elisa.sanfelici@brt.it'
662000151022     c
662100151022     C                   call(e)   'TIS701C1'
662200151022     C                   parm                    wrkEml
662300151022     C                   parm                    wrkEmlCc
662400151022     C                   parm                    wrkOgg
662500151022     C                   parm                    wrkMsg
662600151022     C                   parm                    wrkesito
662700151022
662800151022     c                   ENDSR
662900150703      /free
663000150703       //--------------------------------------------------------------
663100150703       //?Override al file di stampa per impostarvi i dati per
663200150703       //?  l'invio via e-mail   +   stampa inizio e-mail
663300150703       //--------------------------------------------------------------
663400150703       BEGSR sr_OpenPrtF;
663500150703
663600150703         // Override al file di stampa per impostarvi i dati per
663700150703         // l'invio via e-mail
663800150703           exsr sr_Override;
663900150703
664000150703
664100150703           indy = 1 ;
664200150703
664300150703          dow     s_BSPl(indy)>0  ;
664400150703
664500150703          if      s_BSPlBO(indy)>0 ;
664600150703          dataiso=%date(s_BSPlD(indy)) ;
664700150703          data_eur=dataiso  ;
664800150703
664900150703           clear O_testo;
665000150703           chain  s_BSPl(indy)   azorg01l  ;
665100150703           if not %found(azorg01l)  ;
665200150703           eval orgdes=*all'?'  ;
665300150703           endif  ;
665400150703           %subst(O_testo:6:20) = %editc(s_BSPl(indy):'X')+'-'+orgdes  ;
665500150703           %subst(O_testo:46:10) =%char(data_eur)  ;
665600150703           %subst(O_testo:64:9 ) =%editc(s_BSPlBO(indy):'2')  ;
665700150703           except PrtDet;
665800150703           writerig=writerig+1  ;
665900150703           endif  ;
666000150703
666100150703           // se ne ho scritte 50 di righe devo inviare
666200150703           if writerig>=50  ;
666300150703             exsr      Routend  ;
666400150703             clear writerig                  ;
666500150703
666600150703             exsr sr_Override;
666700150703
666800150703           endif  ;
666900150703
667000150703           indy=indy+1  ;
667100150703           enddo    ;
667200150703
667300150703          // Stesso ciclo sui cod clienti
667400150703          indy = 1      ;
667500160208          dow     s_BSPk(indy)>0  ;
667600150703
667700150703          if      s_BSPkBO(indy)>0 ;
667800150703          dataiso=%date(s_BSPkD(indy)) ;
667900150703          data_eur=dataiso  ;
668000150703
668100150703           clear O_testo;
668200150703           chain  (1:kci:s_BSPk(indy)) cnaco00f  ;
668300150703           if not %found(cnaco00f)  ;
668400150703           eval acorag=*all'?'  ;
668500150703           endif  ;
668600150703           %subst(O_testo:6:20) = %editc(s_BSPk(indy):'X')+'-'+acorag  ;
668700150703           %subst(O_testo:46:10) =%char(data_eur)  ;
668800150703           %subst(O_testo:64:9 ) =%editc(s_BSPkBO(indy):'2')  ;
668900150703           except PrtDet;
669000150703           writerig=writerig+1  ;
669100150703           endif  ;
669200150703
669300150703           // se ne ho scritte 50 di righe devo inviare
669400150703           if writerig>=50  ;
669500150703             exsr      Routend  ;
669600150703             clear writerig                  ;
669700150703
669800150703             exsr sr_Override;
669900150703
670000150703           endif  ;
670100150703
670200150703           indy= indy+1  ;
670300150703           enddo    ;
670400150703
670500150703             exsr      Routend  ;
670600150703       ENDSR;
670700150703
670800150703       //--------------------------------------------------------------
670900150703       //?Override al file di stampa per impostarvi i dati per
671000150703       //?  l'invio via e-mail   +   stampa inizio e-mail
671100150703       //--------------------------------------------------------------
671200150703       BEGSR sr_Override;
671300150703
671400150703         reset TRTCM1ds;
671500150703
671600150703         IF  §MRAdreg <> *blank;
671700150703           §CM1mitt = %trim(§MRAdmitt);
671800150703           §CM1dst  = %trim(§MRAddest);
671900150703
672000150703         // Valorizzo i campi della e-m@ail
672100150703          chain  simfel   azorg01l ;
672200150703
672300150703          if   %found(azorg01l) ;
672400150703           og140=orgde0  ;
672500150703           if §ogapo>*zeros  ;
672600150703           §cm1dst=%trim(§cm1dst) +'; CED'+ §ogapo +'@brt.it;';
672700150703           else  ;
672800150703           §cm1dst=%trim(§cm1dst) +'; CED'+%editc(orgf70:'X')+'@brt.it;';
672900150703          endif  ;
673000150703          endif  ;
673100150703
673200150703           §CM1tips = §MRAdreg;
673300150703           §CM1po   = %editc(simfel:'X');
673400150703           §CM1var  = '*OBJM*' + 'SOSPENSIONE AUTOMATICA bolle      '   ;
673500150703           §CM1idp  = §MRAdidpro;
673600160208           Qcmd1 = C_CmdOvrPrtF
673700150703                + ' outq(' + %trim(§MRAdoutqi) + ')'
673800150703                + ' usrdfndta(''' + TRTCM1ds + ''')';
673900150703         ELSE;
674000160208           Qcmd1 = C_CmdOvrPrtF;
674100150703         ENDIF;
674200150703
674300150703         callp(e) qCmdExc(Qcmd1 : %size(Qcmd1));
674400150703
674500150703           open PrtEMAIL;
674600150703
674700150703         // Stampa una testata se NON è richiesta la e-mail
674800150703           IF  §MRAdreg =  *blank;
674900150703             O_testo = JobUser + ' - ' + nompgm
675000150703                     + ' - ' + %editc( *date : 'Y' )
675100150703                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
675200150703             except PrtDet;
675300150703             clear O_testo;
675400150703             except PrtDet;
675500150703             except PrtDet;
675600150703           ENDIF;
675700150703
675800150703         // Stampa testo iniziale
675900150703           O_testo = 'E'' stata eseguita la Sospensione Automatica +
676000150703                      di bolle immesse da almeno xx giorni.' ;
676100150703           %subst(O_testo:72:2) = (%editc(§3iggm: '4'))     ;
676200150703           except PrtDet;
676300150703           O_testo = 'Verificare col mittente o procedere alla lor+
676400150703                      o chiusura come "Merce MAI Affidata".' ;
676500150703           except PrtDet;
676600150703
676700150703         // Stampa una riga vuota
676800150703           clear O_testo;
676900150703           except PrtDet;
677000150703
677100150703         // Stampa intestazione
677200150703           O_testo = ' Bolle spostate in sospensione di          Data imm+
677300150703                     issione    Bolle da verificare       ';
677400150703           except PrtDet;
677500150703           O_testo = 'Codice Cliente o Linea di Partenza          più vec+
677600150703                     chia                                 ';
677700150703         //%subst(o_testo:90:10)= knmus     ;
677800150703           except PrtDet;
677900150703
678000150703         // Stampa una riga vuota
678100150703           clear O_testo;
678200150703           except PrtDet;
678300150703       ENDSR;
678400150703
678500150703       //--------------------------------------------------------------
678600150703       //?Operazioni finali.
678700150703       //--------------------------------------------------------------
678800150703       BEGSR RoutEnd;
678900150703
679000150703         if %open(PrtEMAIL);
679100150703
679200150703         //?Chiusura dello spool?
679300150703           clear O_testo;
679400150703           except PrtDet;
679500150703           O_testo = '***   Fine Lista   ***';
679600150703           except PrtDet;
679700150703
679800150703           close PrtEMAIL;
679900150703
680000150703         //?Eliminazione overflow?
680100160208           Qcmd1 = C_CmdDltOvr;
680200150703           callp(e) qCmdExc(Qcmd1 : %size(Qcmd1));
680300150703
680400150703         endif;
680500150703
680600150703       ENDSR;
680700150703
680800150703      /end-free
680900150703     o* ---------------------------------------------------------------
681000150703     o* ?Spool di stampa (per e-mail).
681100150703     o* ---------------------------------------------------------------
681200150703     oPrtEMAIL  e            PRTdet      1
681300150703     o                       O_testo
681400030902
681500030902      *---------------------------------------------------------------*
681600030902
681700960201**         CMD1
681800021014OVRDBF FILE(FNBLPE0T)               MBR(Mxxx000)
681900960202**         CM19
682000021014OVRDBF FILE(FNBLPT0T)               MBR(Mxxx000)
682100960202**         CM32
682200021014OVRDBF FILE(FISPP00T)               MBR(Mxxx000)
682300000428**         CM37
682400021014OVRDBF FILE(FNFWA00T)               MBR(Mxxx000)
682500000428**         CM38
682600021014OVRDBF FILE(FNFVA01T)               MBR(Mxxx000)
682700920324**         CM41
682800021014ALCOBJ OBJ((FNBLPE0T *FILE *EXCL Mxxx000))
682900920324**         CM47
683000021014ALCOBJ OBJ((FISPP00T *FILE *EXCL Mxxx000))
683100000428**         CM48
683200021014ALCOBJ OBJ((FNFWA00T *FILE *EXCL Mxxx000))
683300000428**         CM49
683400021014ALCOBJ OBJ((FNFVA01T *FILE *EXCL Mxxx000))
683500920324**         CM51
683600021014ALCOBJ OBJ((FNBLPT0T *FILE *EXCL Mxxx000))
683700920324**  ERRORI
683800920324XXXXXXXXXXXXXXXXXXXX/XXX       ** ELENCO FOGLI VIAGGIO NON ELABORA
683900920324TI PERCHE' MEMBRI GIA' ALLOCATI **
684000920324 FOGLIO VIAGGIO
684100920324           ** TELEFONARE CON URGENZA ALL'UFFICIO C.E.D BOLOGNA  **
684200960129** MSG
684300960129ERRORE TRASMISSIONE FOGLI VIAGGIO PARTENZE
684400111019A FILIALE: NON TRASMESSO Foglio Viaggio XXX-XX.XXX.
684500070827CREATO JOBLOG per lavoro durato più di 5 minuti
684600060714                         FOGLIO VIAGGIO XXX-XXXXX.
684700151022ERRORE DI BOLLA DISALLINEATA FNLSU2R
684800151022Presente fnBLT00f senza relativo fnBLP -->
684900060714**
685000060714DSPJOBLOG OUTPUT(*PRINT)
