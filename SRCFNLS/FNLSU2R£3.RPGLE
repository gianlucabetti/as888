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
117000170509     c                   eval      ilsu3flg='R'
117100150603     c                   movel     fnlsu3ds      kpjbu
117200150603     c                   call      'FNLSU3R'
117300150603     c                   parm                    kpjbA
117400150703     c                   movel     kpjbu         fnlsu3ds
117500150703     c* Se Bolla in sospensione oltre xx giorni memorizzo in schiera per invio
117600150703     c* mail
117700150703     c                   if        olsu3err=*blanks  and
117800150703     c                             olsu3sosp='1'
117900170608     c*                  exsr      sr_MemoEmail
118000150703     c                   endif
118100150603     c                   endif
118200130610     c
118300130531   2bc                   else
118400130527     c*
118500130527     c* Memorizzo in bolla fil transito se c'e'
118600130530     c                   if        wBORtran='S'
118700130527     c                   movel     wflp          blpflp
118800130527     c                   else
118900130527     c                   clear                   blpflp
119000130527     c                   endif
119100130527     c                   movel     s_o54tfa      blptfa
119200130527     c                   movel     s_o54tfa      blpfea
119300130527     C                   Z-ADD     FGV2          BLPNFV
119400130527     C                   Z-ADD     fgvdfv        BLPDBR
119500910905     C**
119600950109     C** DETTAGLIO SEGNACOLLI             F N B L T 0 0 F
119700950109     C**
119800950118     C*  SE IN BOLLA CTM= AUTOSPUNTA LA GENERO --> 04 ON
119900910911     C                   SETOFF                                       04
120000130527     C                   CLEAR                   WBLP01            1
120100950118     C     BLPCTM        LOOKUP    CTM                                    04
120200910911     C*
120300950109     C     KBLP2         SETLL     FNBLT000
120400950109     C     KBLP2         READE     FNBLT000                               26
120500910905     C*
120600910905     C     *IN26         DOWEQ     '0'
120700130527     C                   CLEAR                   WBRV1             1
120800130527     C                   CLEAR                   WBRV5             1
120900130527     C                   CLEAR                   dsbrvw5
121000130527     C*
121100911119     C* SE AUTOSPUNTA LA CREO
121200950109     C     *IN04         IFEQ      *ON
121300140618     C     KBLT2         SETLL     FNBRV05                                19
121400950109     C  N19              EXSR      CRTSPU
121500950109     C                   ENDIF
121600130527     c*
121700130527     c* Controllo le anomalie 110 105 se sono da chiudere
121800130527     c                   exsr      Contr_anom
121900060220     c
122000950109     C* FLAGGO LA TRASMISSIONE
122100950109     C                   MOVEL     'T'           BLTFTR
122200950109     C                   Z-ADD     DATEU         BLTDTR
122300910905     C*
122400950109     C                   UPDATE    FNBLT000
122500130527     C*
122600130527     C* SE TROVATA SPUNTA AGGIORNO LA DATA ENTRATA
122700130527     C     WBRV5         IFEQ      'S'
122800130701     c                   if        wblp01=' '
122900130527     C                   update    FNBLP000
123000130527     C                   MOVEL     'S'           WBLP01
123100130701     c                   endif
123200130527     c*
123300130527     c                   eval      dsbrv =dsbrvw5
123400130527     C                   EXSR      call44
123500130527     C                   ENDIF
123600910905     C*
123700950109     C     KBLP2         READE     FNBLT000                               26
123800910905     C                   END
123900130527     c
124000130527     c                   if        wblp01='S'
124100130527     c     kblp2         chain     fnblp01l
124200130911
124300130911     c* se ho trovato una spunta partenza scrivo AGB x aggiornare le date
124400130911     C* SCRIVO IL FILE FNAGB01L PER AGGIORNARE DATE
124500130911     C                   MOVEL     'P'           AGBTBO
124600130911     C                   MOVEL     BLPAAS        AGBAAS
124700130911     C                   MOVEL     BLPLNP        AGBLNP
124800130911     C                   MOVEL     BLPNRS        AGBNRS
124900130911     C                   MOVEL     BLPNSP        AGBNSP
125000130911     C                   MOVEL     'DT'          AGBAGB
125100130911     C                   WRITE     FNAGB000                             39
125200130527     c                   endif
125300950109     C*
125400060220     C* FLAGGO FIAR500F
125500950109     C                   EXSR      WRITEF
125600910905     C**
125700950109     C**  BOLLA                           F N B L P 0 0 F
125800910905     C**
125900060220     C* IL FLAG DDT LO METTO IN BL2SV1
126000060220     C                   MOVEL     BLPFST        BL2SV1
126100060220     C* IL FLAG sigla operatore video lo metto in bl2cei
126200000619     C                   MOVE      BLPSOP        BL2CEI
126300060220     c
126400950109     C                   Z-ADD     DATEU         BLPDT3
126500921026     C                   MOVEL     'S'           BLPFT3
126600950109     C                   Z-ADD     DATEU         BLPDT2
126700950109     C                   MOVEL     'T'           BLPFT2
126800950109     C                   Z-ADD     DATEU         BLPDT1
126900950109     C                   MOVEL     'T'           BLPFT1
127000911119     C*
127100911119     C* IN QUELLO PER SEDE E FIL.ARR SE ASSEGNATO 9999 METTO LA L.ARR
127200950109     C                   Z-ADD     BLPKSC        W0070             7 0
127300911119     C                   MOVE      BLPKSC        COMKSC            4 0
127400911119     C     COMKSC        IFEQ      9999
127500911119     C                   MOVEL     BLPLNA        BLPKSC
127600911119     C                   END
127700911119     C*
127800950109     C   01              WRITE     FNBLPE00
127900950113     C* SOLO PER TRANSITO: IMPOSTO IL VOLUME BOLLETTATO CON QUELLO
128000950113     C*  DA FATTURARE
128100130702     C     wbortran      IFEQ      'S'
128200950113     C                   Z-ADD     BLPVLB        W0053             5 3
128300950113     C                   MOVEL     BLPFVB        W001A             1
128400950113     C*
128500950113     C                   Z-ADD     BLPVLF        BLPVLB
128600950113     C                   MOVEL     BLPFVF        BLPFVB
128700950113     C*
128800950113     C                   WRITE     FNBLPT00
128900950113     C*
129000950113     C                   Z-ADD     W0053         BLPVLB
129100950113     C                   MOVEL     W001A         BLPFVB
129200021127     c
129300950113     C                   ENDIF
129400950412     C* MANDO IN SEDE SOLO E' PREVISTA LA TRASMISISONE DAL CODICE BOLLA
129500950412     C     §3ABSD        IFEQ      ' '
129600070423     C                   WRITE     FNBLP046                             90
129700950412     C                   ELSE
129800950412     C                   MOVEL     'N'           BLPFT2
129900950412     C                   ENDIF
130000911119     C*
130100950109     C                   Z-ADD     W0070         BLPKSC
130200000320     C**
130300130527     c                   if        wblp01='S'
130400130527     C                   UPDATE    FNBLP001
130500130527     c                   else
130600950109     C                   UPDATE    FNBLP000
130700130527     c                   endif
130800010309     c*
130900010309     c* AGGIORNO E SFLAGGO ARCHIVI O.R.M.
131000060220     c* Dentro a questa routine devo chainare fIAR4 tipo record "M"
131100010309     c                   EXSR      AGGORM
131200931020     C*
131300011121     C* CREO FILE TASSAZIONE PADRONCINI fiftt00F E fiftd00F
131400931027     C                   EXSR      CRTTAS
131500151118     c* chiudo anomalia 20 se lnp/cliente con sospensione bolle in
131600151118     c* chiusura f.v.p.
131700151118     c                   if        w_sibsp='S'
131800151118     C                   MOVEL     020           comcaa
131900151118     c     kanm01        chain     fnanm01l
132000151118     c                   if        %found(fnanm01l)
132100151118     C                   CLEAR                   DSLR33
132200151118     C                   MOVE      WDTP          D33DCH
132300151118     C                   MOVEL     '1'           D33FSC
132400151118     C                   MOVEL     '  '          D33CCH
132500151118     C                   MOVE      ANMNR1        D33NRR
132600151118     C                   CALL      'FNLR33R'
132700151118     C                   PARM                    DSLR33
132800151118     c                   endif
132900151118     c                   endif
133000950116     C                   ENDIF
133100130530     c
133200130531   2aC                   endif
133300130531   2bC                   endif
133400910906     C*
133500130531     C     Kblp77        reade     FNBLP000                               22
133600130531    1C                   ENDDO
133700130531     c
133800130531     c                   add       1             I
133900130531     c                   enddo
134000900601     C*
134100011121     C* CREO fiftt00F PER ULTIMA LETTURA
134200931203     C                   EXSR      CRTFTT
134300931203     C*
134400900601     C                   ENDSR
134500130527     C**************************************************************************
134600130527     C*    CONTROLLO ESISTENZA FILIALE DI TRANSITO
134700130527     C**************************************************************************
134800130527     C     CTRFLP        BEGSR
134900130530     c                   clear                   wBORtran          1
135000130527     C*
135100130527     C* CONTROLLO SE C'E' TRANSITO
135200141029     C******             SETOFF                                       05
135300130527     C*
135400130531     C     flp(I)        IFgt      *zeros
135500130531     c                   movel     flp(I)        WFLP              3 0
135600130527     c
135700130527     C* C'E' TRANSITO SE :
135800130531     C* 1) FLP NON E' = ALLA LINEA DI ARRIVO
135900130531     C* 2) FLP NON E' = AL TERMINAL DI ARRIVO DELLA LINEA ARRIVO
136000130527     c
136100130531     C     savffv        IFNE      wFLP
136200130531     C     s_o54tfa      ANDNE     wFLP
136300130530     C                   eval      WBORtran='S'
136400130527     C                   END
136500130527     C                   END
136600130527     C*
136700130527     C                   ENDSR
136800130527     C**-------------------------------------------------------*
136900130527     C* Borderizzazione bolle e messa in sospensione automatica
137000130527     C**-------------------------------------------------------*
137100130527     C     Borderiz      BEGSR
137200130527     c                   clear                   wescludi          1
137300130610     c                   clear                   uncollo
137400150603     c                   clear                   w_sosp            1
137500130527     c
137600130527     C* SPEDIZIONE GIA' BORDERIZZATA               - PER BORDERO'
137700130527    1C     BLPDBR        IFNE      0
137800130527     c                   eval      WEscludi='S'
137900130530     c                   leavesr
138000130527    1C                   END
138100130527     c
138200130527     C* DATA SPEDIZIONE < = DATA BORDERO' IMMESSA  - PER BORDERO'
138300130530     c                   if        pa2barcode=' ' and
138400130530     c                             blpdsp>fgvdfv
138500130527     c                   eval      WEscludi='S'
138600130530     c                   leavesr
138700130527    1C                   END
138800130527     c
138900130527     c* Se codice cliente mittente non è quello immessa. escludo
139000130527     c                   clear                   alfaccm
139100130527    1c                   if        pa2ccm>*zeros
139200130527     c
139300130527    2c                   if        blpccm>0
139400130527     c                   movel     blpccm        alfaccm           7
139500130527     c                   else
139600130527     c                   movel     blpksc        alfaccm
139700130527    2c                   endif
139800130527     c
139900151127    2c***                if        alfaccm<>pa2ccm
140000151127     c                   if        %lookup(alfaccm:ccmb)=0
140100130527     c                   eval      WEscludi='S'
140200130530     c                   leavesr
140300130527    2C                   endif
140400130527    1C                   endif
140500130527     c
140600130527      * SE MANCA IL COD PADRONCINO LA BOLLA È UN PREPAGATO NON CONFERMATO.
140700130527      *  SE ANNO/MESE BOLLA < DI ANNO/MESE DEL BORDERO' FORZO IL CODICE
140800130527      *  AUTOTRASP. = 999, LA DATA RITIRO = DATA BOLLA, IL CODICE
140900130527      *  PRESTAZIONE = 'P' E BORDERIZZO.
141000130527      *  IN CASO CONTRARIO NON BORDERIZZO.
141100130527    1C     BLPPDR        IFEQ      0
141200130530     C                   MOVEL     BLPMGS        WMMSp
141300130530    2C                   If        BLPAAS < kaaa4 or WMMSp< kmmm
141400150930     c                             or (fgvdfv>= §DGDCPD and fgvdfv<= §DGDCPA)
141500130527     C                   Z-ADD     999           BLPPDR
141600130527     C                   movel     blplnp        BLPPDR
141700130527     C                   Z-ADD     BLPMGS        BLPDRT
141800130527     C                   MOVEL     BLPAAS        BLPDRT
141900130527     c
142000130527    3C     BLPFPP        IFEQ      *BLANKS
142100130527     C                   MOVEL     'P'           BLPFPP
142200130527    3C                   ENDIF
142300130527      *
142400130527   x2C                   ELSE
142500130527     c                   eval      WEscludi='S'
142600130530     c                   leavesr
142700130527    2C                   endif
142800130527     c*
142900130527   x1c                   else
143000130530     c
143100130530     c* Per borderizzazione BARCODE --> controllo se esiste spunta o anom 010
143200170502    2c******             if        pa2barcode= 'S'  or W_siBsp='S'
143300170502    2c                   if        pa2barcode= 'S'
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
144500170502     c* Controllo analogo anche se per il cliente o la lnp è prevista sospension
144600170502     c* su FIBSP (cliente con colli in partenza da più filiali='S')
144700170502     c*  escluse le bolle legate figlie
144800170502    2c                   if        w_sibsp= 'S'
144900170502     c     kblp2         chain     fnlbl01l
145000170502    3c                   if        not %found(fnlbl01l)
145100170502     c                   exsr      CTRNSC
145200170502     c
145300170502    4c                   if        uncollo=' '
145400170502     c                   eval      WEscludi='S'
145500170502    5c                   if        w_SiBsp='S'
145600170502     c                   eval      W_sosp  ='S'
145700170502    5c                   endif
145800170502     c                   leavesr
145900170502    4C                   endif
146000170502    3C                   endif
146100170502    2C                   endif
146200170502
146300170327     c* Se bolla legata "figlia" senza spunte, il giorno della bollettazione
146400170327     c*  la sposto in automatico in sospensione
146500170421     c* Escludo lo bolle lnp 101
146600170327     c                   if        blpdsp=blpdim and blpnrs=0 and blpdse=0
146700170421     c                             and blpdpc=0 and blpdam=0 and blplnp<>101
146800170327     c     kblp2         chain     fnlbl01l
146900170327     c                   if        %found(fnlbl01l)
147000170413     c* visto che ci sono casi "strani" , eseguo anche il controllo
147100170413     c*   dei colli e del manca record
147200170413     c                   exsr      ctrnsc
147300170413
147400170413    3c                   if        uncollo=' '
147500170327     c                   eval      WEscludi='S'
147600170327     c                   eval      W_sosp  ='S'
147700170327     c                   endif
147800170413     c
147900170413     c                   endif
148000170327     c                   endif
148100130610     c*
148200130530    1c                   endif
148300130527     c
148400130527     C                   ENDSR
148500130530     C**************************************************************************
148600130530     C*    CONTROLLO ESISTENZA SPUNTE PER LA BOLLA - borderizz.barcode
148700130530     C**************************************************************************
148800130530     C     CTRNSC        BEGSR
148900130530     C*
149000130530     C                   SETOFF                                       10
149100130530     c                   clear                   uncollo           1
149200130530     C* CONTROLLO SE CTM NON PREVEDE LA SPUNTA DEI SEGNACOLLI:
149300130530     C*  A BORDERO' LO STESSO (FLAG 5 DEL CTM)
149400170413     c* questo vale sempre: anche per la sospensione automatica
149500170413     c*  per sicurezza lo faccio anche se non dovrebbe mai accadere
149600170413     c***                if        pa2barcode= 'S'
149700130530     C     BLPCTM        LOOKUP    CTM                                    10
149800130530     c                   if        *in10
149900130530     C                   eval      uncollo='S'
150000130530     C                   leavesr
150100130530     C                   endif
150200170413     c***                endif
150300151118
150400151118     c                   if        w_sibsp='S'
150500151118     C                   MOVEL     020           comcaa
150600151118     c     kanm01        chain     fnanm01l
150700151118     c                   if        %found(fnanm01l) and anmdch=0
150800151118     C                   eval      uncollo='S'
150900151118     C                   leavesr
151000151118     c                   endif
151100151118     c                   endif
151200130530     c
151300130530     C* SI SPUNTA: CONTROLLO SE ALMENO UN SEGNACOLLO SPUNTATO C'E'
151400130530     C     KBLp2         SETLL     FNBLT01L
151500130530     C     KBLp2         READE(n)  FNBLT01L
151600130530     C*
151700130530    1C                   DOW       not %eof(fnblt01l)
151800130610     c
151900130530     C* LA FILIALE IN GESTIONE PER BORDERO' E' SIMFEL
152000150603     c                   if        pa2barcode='S'
152100130530     C                   Z-ADD     1             KNPG
152200130530     C                   Z-ADD     SIMFEL        KFGS
152300130530     C                   Z-ADD     fgv2          wnfv60
152400130530     c* BARCODE
152500130530     C     KBRVbor       SETLL     FNBRV01L                               31
152600150603     c                   else
152700150603     C     KBRV7         SETLL     FNBRV07L                               31
152800150603     c                   endif
152900130530    3c                   if        *in31
153000130530     C                   SETON                                        10
153100130530     C                   eval      uncollo='S'
153200130530     c                   leavesr
153300130530    3c                   endif
153400130530     c
153500130530     c* non trovata spunta ma presente manca record bolla in arrivo ancora
153600130530     c* aperta --> borderizzo lo stesso
153700130530    3c                   if        *in31=*off
153800130607     C                   MOVEL     010           comcaa
153900130530     c     kanm1         setll     fnanm02l
154000130530    4c                   if        %equal(fnanm02l)
154100130607     C                   eval      uncollo='S'
154200130530     c                   seton                                        10
154300130530     c                   leavesr
154400130530    4c                   endif
154500130530    3c                   endif
154600130530     c
154700130607     C     kblp2         READE(n)  FNBLT000
154800130530    1C                   ENDdo
154900130530     C*
155000130530     C                   ENDSR
155100130527     C**************************************************************************
155200130527     C*    CONTROLLO anomalia 110 105 da chiudere
155300130527     C**************************************************************************
155400130527     C     Contr_Anom    BEGSR
155500130527     C* ANOMALIA--> "NO BOLLA BORDERIZZATA" CHIUDO
155600130607     C                   MOVEL     110           comcaa
155700130527     C     KANM1         CHAIN(N)  FNANM02L                           32
155800130527     C     *IN32         IFEQ      *OFF
155900130607     C                   EXSR      call_lr33
156000130527     C                   END
156100130527     C*
156200130527     C* ANOMALIA --> "MANCA BOLLA"          CHIUDO
156300130607     C                   MOVEL     105           comcaa
156400130527     C     KANM1         CHAIN(N)  FNANM02L                           32
156500130527     C**
156600130527     C* SE C'E' GIA' CHIUSA COME ANNULLATA --> COME SE NON CI FOSSE
156700130527     C* (P.ES SE LA 105 ERA STATA ANNULLATA MA SUCCESSIVAMENTE LA
156800130527     C* SPEDIZIONE VIENE BOLLETTATA. IN QUESTO CASO IL PGM DEVE)
156900130527     C* AGGIORNARE BLTNFV)
157000130527    1C     *IN32         IFEQ      *OFF
157100130527     C     ANMDCH        ANDEQ     0
157200130527     C*
157300130527     C     *IN32         OREQ      *OFF
157400130527     C     ANMDCH        ANDGT     0
157500130527     C     ANMCCH        ANDNE     'AN'
157600130527     C**
157700130527    2C     ANMDCH        IFEQ      0
157800130527     C                   CLEAR                   WSPE
157900130527     C*
158000130527     C* CERCO LA SPUNTA ENTRATA PER AGGIORNARE LA DATA ENTRATA
158100130527     C                   MOVEL     '5'           KNPG
158200130527     C                   MOVE      *HIVAL        WVVDFV
158300130527     C     KBRV1         SETLL     FNBRV09L                           32
158400130527     C     KBRV1         READE     FNBRV09L                               32
158500130527     C*
158600130527     C     *IN32         DOWEQ     *OFF
158700130530     C                   CLEAR                   DSLV53
158800130527     C                   MOVEL     'V'           D53TFO
158900130527     c                   exsr      data_spunta
159000130527     c
159100130527     C     KBRV1         READE     FNBRV09L                               32
159200130527     C                   ENDDO
159300130527     C*
159400130527     C                   MOVEL     '1'           KNPG
159500130527     C     KBRV1         CHAIN     FNBRV09L                           32
159600130527     C     *IN32         DOWEQ     *OFF
159700130527     C                   MOVEL     'S'           WBRV1             1
159800130527     C* SE ESISTE UNA SPUNTA PARTENZA CON DATA < DELLA DATA PRIMA SPUN-
159900130527     C* TA ENTRATA --> MEMORIZZO I DATI DELLA SPUNTA PARTENZA
160000130530     C                   CLEAR                   DSLV53
160100130527     C                   MOVEL     'G'           D53TFO
160200130527     c                   exsr      data_Spunta
160300130527
160400130527     C     KBRV1         READE     FNBRV09L                               32
160500130527     C                   ENDDO
160600130527     C*
160700130527     C* TROVATA SPUNTA PARTENZA
160800130527    3C     WBRV1         IFEQ      'S'
160900130527     C* CHIAMO IL PGM DI AGGIORNAMENTO BOLLE PER SCRIVERE LA DATA ENTRA
161000130527     C* TA
161100130527     C* SOLO SE FV CHIUSO AGGIORNO LA BOLLA
161200130527     c* Es 27 05 13 : non lo faccio sempre se c'e' anomalia 105 significa
161300130527     c*  che la spunta è stata già elaborata dalla chiusura FGV
161400130527     c*  quindi il collo/bolla devono essere aggioranti a prescindere
161500130527    4C**** D53ERR        IFEQ      *BLANKS
161600130527     C**** D53FCF        ANDEQ     'S'
161700130527     C*
161800130527     C                   Z-ADD     BRVNPS        BLTNPP
161900130527     C                   Z-ADD     D53NFV        BLTNFV
162000130527     C                   Z-ADD     D53DFV        BLTDFV
162100130527     C*
162200130527     C* AGGIORNO LA DATA PARTENZA PRIMO COLLO SULLA BOLLA
162300130527    5C     BLPDPC        IFEQ      0
162400130527     C                   Z-ADD     BLTDFV        BLPDPC
162500130527     C* SOMMO LA SPEDIZIONE DI UN COLLO "EX MANCA BOLLA"
162600130527     C                   Z-ADD     1             WSPE
162700130527    5C                   END
162800130527     C*
162900130527     C* CALCOLO PESO E VOLUME UNITARI DI UN COLLO "EX MANCA BOLLA"
163000130527     C                   EXSR      TOTFVD
163100130527     C*
163200130527     C* AGGIORNO FNSTA PER LA RIELABORAZIONE DELLE STATISTICHE
163300130527     c                   If        BltDfv <= §SdfParU
163400130527     C                   Z-ADD     BLTDFV        SA2DTS
163500130527     C                   MOVE      'P'           SA2TRC
163600130527     C                   CALL      'FNLSA2R3'
163700130527     C                   PARM                    SA2TLA
163800130527     C                   PARM                    SA2TRC
163900130527     C                   PARM                    SA2DTS
164000130527     C                   PARM                    WWLNA             3 0
164100130527     C                   MOVEL     'S'           WLSA2             1
164200130527    5C                   END
164300130527     C*
164400130527     c                   If        BltDfv <= §SdfTerU
164500130527     C                   MOVE      BLTDFV        SA2DTS
164600130527     C                   MOVE      'T'           SA2TRC
164700130527     C                   CALL      'FNLSA2R3'
164800130527     C                   PARM                    SA2TLA
164900130527     C                   PARM                    SA2TRC
165000130527     C                   PARM                    SA2DTS
165100130527     C                   PARM                    WWLNA             3 0
165200130527     C                   MOVEL     'S'           WLSA2             1
165300130527    5C                   END
165400130527     C*
165500130527     C* SE FOGLIO APERTO NON AGGIORNO LA BOLLA E MEMORIZZO IL FOGLIO
165600130527     C*  SU CUI STO BORDERIZZANDO (non dovrebbe più accadere)
165700130527   X4C****               ELSE
165800130527     C*
165900130527     C****               Z-ADD     VIDNFV        BLTNFV
166000130527    4C****               ENDIF
166100130527     C*
166200130527    3C                   ENDIF
166300130527     C* SE TROVATA SPUNTA CLEARO DATA ENTRATA E VOLUME CML PRIMA
166400130527     C* AFFINCHE' L'FNLS45R AGGIORNI QUESTI CAMPI IN BASE ALLA PRIMA
166500130527     C* SPUNTA PASSATA SUL SISTEMA
166600130527    4C     WBRV5         IFEQ      'S'
166700130527     C                   CLEAR                   BLTDSE
166800130527    5C     W5VUC         IFGT      *ZEROS
166900130527     C                   CLEAR                   BLTVUC
167000130527    5C                   END
167100130527    4C                   END
167200130527     C*
167300130607     C                   EXSR      call_lr33
167400130527    2C                   END
167500130527     C                   ELSE
167600130527   X1C*
167700130527     C                   Z-ADD     FGV2          BLTNFV
167800130527    1C                   ENDIF
167900130527     C*
168000130527     c
168100130527     C                   ENDSR
168200130527      ****************************************************************
168300130527      * Prendo data spunta
168400130527      ****************************************************************
168500130527     c     Data_spunta   BEGSR
168600130527     C* DETERMINO DATA FOGLIO
168700130527     C                   MOVE      BRVNPG        D53NPG
168800130527     C                   MOVE      BRVNFV        D53NFV
168900130527     C                   MOVE      BRVFGS        D53FGS
169000130527     C                   MOVE      SIMFEL        D53FEL
169100130527     C                   CALL      'FNLV53R'
169200130530     C                   PARM                    DSLV53
169300130527     C                   MOVE      '1'           WLV53             1
169400130527     C*
169500130527     C     D53ERR        IFEQ      *BLANKS
169600130527     C     D53DFV        iflt      WVVDFV
169700130527     c* se è presente il peso e volume nella spunta meno recente
169800130527     c*  li devo mantenere per il richiamo a fnls45r
169900130527     c
170000130527     c                   if        brvvuc=0  and w5vuc>0
170100130527     c                   eval      brvvuc=w5vuc
170200130527     c                   endif
170300130527     c                   if        brvpuc=0  and w5puc>0
170400130527     c                   eval      brvpuc=w5puc
170500130527     c                   endif
170600130527     c                   if        brvcan=' ' and w5can<>' '
170700130527     c                   eval      brvcan=w5can
170800130527     c                   endif
170900130527     c
171000130527     C                   MOVEL     'S'           WBRV5             1
171100130527     c                   movel     dsbrv         dsbrvw5
171200130527     C                   MOVE      D53DFV        WVVDFV
171300130527     c* se c'e' il peso e volume --> lo passo per il calcolo
171400130527     c                   else
171500130527     c                   if        w5vuc=0  and brvvuc>0
171600130527     c                   eval      w5vuc=brvvuc
171700130527     c                   endif
171800130527     c                   if        w5puc=0  and brvpuc>0
171900130527     c                   eval      w5puc=brvpuc
172000130527     c                   endif
172100130527     c                   if        w5can=' ' and brvcan<>' '
172200130527     c                   eval      w5can=brvcan
172300130527     c                   endif
172400130527     c
172500130527     C                   END
172600130527     C                   END
172700130527     c                   ENDSR
172800130527     C**************************************************************************
172900130527     C*    CALCOLO PESO E VOLUME UNITARI
173000130527     C**************************************************************************
173100130527     C     TOTFVD        BEGSR
173200130530     c                   exsr      CALUNI
173300130527     C*
173400130527     C                   Z-ADD     BRVNFV        WNFV5
173500130603     C                   MOVEL     '1'           WTRC
173600130530     C     KFVDbor       CHAIN     FNFVD000                           34
173700130527     C   34              CLEAR                   FNFVD000
173800130527     C***  COLLI ***
173900130527     C                   ADD       1             FVDCLA
174000130530     c                   clear                   wtran
174100130527     C*
174200130527     C***  SPEDIZIONI  ***
174300130527     C     WSPE          IFGT      0
174400130530     C                   z-add     WSPE          wspen
174500130527     C                   ENDIF
174600130530     c
174700130530     c                   exsr      RIEFVD
174800130527     C*
174900130527     C* ABBLENCO FLAG E DATA TRASMISSIONE PER INVIO IN SEDE
175000130527     C                   MOVEL     ' '           FVDFTR
175100130527     C                   Z-ADD     0             FVDDTR
175200130527     C*
175300130527     C*
175400130527     C     *IN34         IFEQ      *ON
175500130527     C                   Z-ADD     BRVNFV        FVDNFV
175600130527     C                   MOVEL     '1'           FVDTRC
175700130527     C                   MOVEL     SIMFEL        FVDLNP
175800130527     C                   MOVEL     BLPLNA        FVDLNA
175900130527     C                   WRITE     FNFVD000
176000130527     C                   ELSE
176100130527     C*
176200130527     C                   UPDATE    FNFVD000
176300130527     C                   ENDIF
176400130527     C*
176500130527     C                   ENDSR
176600950109     C**-----------------------------------------------*
176700950109     C* SCRITTURA DI ALCUNI FILE DELLA BOLLA -----------
176800950109     C**-----------------------------------------------*
176900950109     C     WRITEF        BEGSR
177000060220     c
177100030107      *  Estensione Testata Bolla    F I A R 5 0 0 F
177200030107     c     Kblp2         Setll     Fiar501l
177300030203Do  1c                   Do        *Hival
177400030107     c     Kblp2         Reade     Fiar501l
177500030107      * Fine File esco dal ciclo
177600030107     c                   If        %Eof(Fiar501l)
177700030107     c                   Leave
177800030107     c                   EndIf
177900030110      * Controllo se già inviato
178000030110     c                   If        Ar5Dt1 <> *all'9' and Ar5Dt1 > 0
178100030110     c                   Iter
178200030110     c                   EndIf
178300030107      * Flaggo per la trasmissione anche se il record non viene trasmesso
178400030107     c                   Eval      Ar5Ft1 = 'T'
178500030107     c                   Z-Add     Dateu         Ar5Dt1
178600030107     c                   Update    Fiar5000
178700030203    1c                   EndDo
178800030107
178900950109     C                   ENDSR
179000900608     C**-----------------------------------------------*
179100950109     C** TRASFERISCO BOLLE  - 99999 -
179200900608     C**-----------------------------------------------*
179300000619     C     TRAS99        BEGSR
179400030107
179500030107     c                   Open      Fiftt01l
179600910906     C*
179700130702     C                   SETOFF                                       01
179800920221     C                   Z-ADD     0             SAVLNA
179900130527     C     K02BLP77_9    reade     FNBLP000                               22
180000900608     C*
180100950323    1C     *IN22         DOWEQ     '0'
180200010109      *
180300010109      * SE MANCA IL COD PADRONCINO LA BOLLA È UN PREPAGATO NON CONFERMATO.
180400010111      *  SE ANNO/MESE BOLLA < DI ANNO/MESE DEL JOB      CONFERMO FORZANDO
180500010109      *  COD.AUTOTRASP. = 999, LA DATA RITIRO = DATA BOLLA E IL CODICE
180600010109      *  PRESTAZIONE = 'P'. IN CASO CONTRARIO NON CONFERMO.
180700010109     C                   IF        BLPPDR = *zeros
180800010109     C                   movel     BLPMGS        WMMSp             2 0
180900010111     C                   movel     DATEU         WAAJb             4 0
181000010111     C                   move      dateu         W0040             4 0
181100010111     C                   movel     W0040         WMMJb             2 0
181200010109      *
181300010111     C                   If        BLPAAS < WAAJb or WMMSp < WMMJb
181400150930     c                             or (fgvdfv>= §DGDCPD and fgvdfv<= §DGDCPA)
181500010109     C                   z-add     999           BLPPDR
181600010109     C                   z-add     BLPMGS        BLPDRT
181700010109     C                   movel     BLPAAS        BLPDRT
181800010109     C                   If        BLPFPP = *BLANKS
181900010109     C                   eval      BLPFPP = 'P'
182000010109     C                   ENDIF
182100010109     C                   ENDIF
182200010109      *
182300010109     C                   ENDIF
182400010109      *
182500010109      *  PER I PREPAGATI, GIA' CONFERMATI
182600010109   1AC     BLPPDR        IFGT      0
182700990915     C*
182800950115     C* ELABORO SOLO LE BOLLE NON ANCORA TRASMESSE
182900991028    2C     BLPDT2        IFEQ      0
183000991028     C     BLPDT1        OREQ      0
183100920221     C*
183200940107     C* CHAIN CON CODICE BOLLA
183300940107     C                   EXSR      CTABEL
183400030107      * Aggiorno Fiftt con il numero di ritiri annullati
183500030107      * bolle "FY"
183600030107     c                   If        BlpCbo = 'FY'
183700030107     c                   Exsr      CrtFtt1
183800030107     c                   EndIf
183900000619     C*
184000920221     C* INVIO ANCHE PER FILIALE
184100950412    3C     §3ABFI        IFEQ      ' '
184200960411     C* SE E' "£" INVIO SOLO SE LNP<>LNA
184300960411     C     BLPCBO        OREQ      '£ '
184400960411     C     BLPLNP        ANDNE     BLPLNA
184500920221     C*
184600920221     C** L I N E A   A R R I V O   C A M B I A T A
184700950323    4C     SAVLNA        IFNE      BLPLNA
184800031112     C**                 CLEAR                   WLOCAL
184900971105     C                   SETOFF                                       01  50
185000950109     C*
185100130531     C                   CLEAR                   dslv55
185200140403     c                   z-add     blptfp        d55lnp
185300130531     C                   Z-ADD     BLPLNA        d55LIN
185400130531     C                   Z-ADD     PARDTP        d55DRF
185500130531     C                   CALL      'FNLV55R'
185600130531     c                   PARM                    dslv55
185700130531    5C     d55err        IFEQ      *BLANKS
185800130531     c                   z-add     d55tfp        s_o54tfp
185900130531     c                   z-add     d55tfa        s_o54tfa
186000130531     c* se secondo livello in arrivo
186100031219     c* invio la bolla al terpar del ter-arr della lna
186200130531    6c     blplna        ifne      d55tfa
186300130531     c                   clear                   dslv55
186400130531     C                   MOVEL     'P'           d55TPT
186500130531     C                   MOVEL     s_o54tfa      d55LIN
186600130531     C                   MOVEL     pardtp        d55DRF
186700130531     C                   CALL      'FNLV55R'
186800130531     C                   PARM                    dslv55
186900130531    7c     d55err        ifeq      *blanks
187000031219     c                   seton                                        01
187100130531     c                   movel     d55tfp        varx
187200031219    7c                   endif
187300031219   x6c                   else
187400031219     c* se primo liv. in arrivo o ogas diversi
187500031219     c* invio al terminal di partenza di lna
187600031219     c                   seton                                        01
187700031219     c                   movel     s_o54tfp      varx
187800031219    6c                   endif
187900031112    5c                   endif
188000031112     C     N_varx        LOOKUP    FEL                                    32
188100950323    5C     *IN32         IFEQ      *OFF
188200031112     C                   MOVEL     N_varx        VAR2X
188300031112     C                   MOVEL     N_varx        NUM3
188400950115     C                   EXSR      LCKBLP
188500031112     C                   Z-ADD     1             IY                3 0
188600031112     C     000           LOOKUP    FEL(IY)                                32
188700031112     C   32              Z-ADD     N_varx        FEL(IY)
188800950323    5C                   ENDIF
188900950115     C*
189000031112     c   50              setoff                                       01
189100950115     C*
189200031112     c     *in01         ifeq      *on
189300950109     C                   EXSR      CAEX
189400971106     C* SE BOLLA LOCALE MEMORIZZO CHE CE N'è ALMENO UNA
189500130531     c     simfel        ifeq      s_o54tfa
189600971106     C                   SETON                                        97
189700031112     C**                 MOVEL     'S'           WLOCAL
189800971106     C                   END
189900950323    5C                   ENDIF
190000950115     C*
190100950109     C                   MOVEL     BLPLNA        SAVLNA
190200950323    4C                   ENDIF
190300920221     C*
190400920427     C* SE E' DA TRASMETTERE SOLO IN SEDE NON APRO MEMBRI
190500950323   X3C                   ELSE
190600950109     C*
190700950109     C* IMPOSTO I FLAG DI TRASMISSIONE
190800950109     C                   MOVEL     'N'           BLPFT1
190900950109     C                   Z-ADD     DATEU         BLPDT1
191000940415     C*
191100940415     C* SE CODICE BOLLA FRANCO RECUPERO MUOVO DATA BORDERO' IN DCM/DAM
191200060303     c* stessa cosa per cod. bolla "M"-Prepagato annullato
191300950323    4C     BLPCBO        IFEQ      'B '
191400060303    4C     BLPCBO        oreQ      'M '
191500030728      * prima però imposto la data borderò uguale alla data foglio viaggio
191600030728     c                   Z-Add     FgvDfv        BlpDbr
191700920615     C                   MOVEL     BLPDBR        BLPDCM
191800920615     C                   MOVEL     BLPDBR        BLPDAM
191900950323    4C                   ENDIF
192000950323    3C                   ENDIF
192100950115     C*
192200950115     C* SOLO SE HO ALLOCATO I MEMBRI
192300950915    3C     *IN50         IFEQ      *OFF
192400911119     C*
192500060220     C* FLAGGO FIAR500F: LO FACCIO A PREDCINDERE DALLA TRASMISSIONE DELLA BO
192600060220     C                   EXSR      WRITEF
192700060220     C
192800991029     C* IN QUELLO PER ARRIVO SE 9999 METTO LA LN ARRIVO
192900950109     C                   Z-ADD     BLPKSC        W0070
193000950915     C                   MOVE      BLPKSC        COMKSC            4 0
193100950323    4C     COMKSC        IFEQ      9999
193200911119     C                   MOVEL     BLPLNA        BLPKSC
193300950323    4C                   END
193400950323     C*
193500991028     C     BLPDT3        IFEQ      0
193600991028     C                   Z-ADD     DATEU         BLPDT3
193700991028     C                   MOVEL     'S'           BLPFT3
193800991028     C                   ENDIF
193900991028     C*
194000991028    4C     BLPDT2        IFEQ      0
194100950109     C                   Z-ADD     DATEU         BLPDT2
194200950109     C*
194300991028    5C     §3ABSD        IFEQ      ' '
194400950109     C                   MOVEL     'T'           BLPFT2
194500070423     C                   WRITE     FNBLP046                             90
194600950109     C                   ELSE
194700950109     C                   MOVEL     'N'           BLPFT2
194800991028    5C                   ENDIF
194900991028    4C                   ENDIF
195000920221     C*
195100991028    4C     BLPDT1        IFEQ      0
195200991028    5C     §3ABFI        IFEQ      ' '
195300960411     C     BLPCBO        OREQ      '£ '
195400960411     C     BLPLNP        ANDNE     BLPLNA
195500991029     C*
195600060220     C* IL FLAG DDT LO METTO IN BL2SV1
195700060220     C                   MOVEL     BLPFST        BL2SV1
195800060220     C* IL FLAG sigla operatore video lo metto in bl2cei
195900000619     C                   MOVE      BLPSOP        BL2CEI
196000991029     C* IMPOSTO I FALG DI TRASMISSIONE
196100991029     C                   MOVEL     'T'           BLPFT1
196200991029     C                   Z-ADD     DATEU         BLPDT1
196300991029     C**
196400950109     C   01              WRITE     FNBLPE00
196500991028    5C                   ENDIF
196600991028    4C                   ENDIF
196700900608     C*
196800950109     C                   MOVEL     W0070         BLPKSC
196900950323     C* IMPOSTO IL FOGLIO A 99998 --> PER DIRE CHE L'HO GIA ELABORATA
197000950323     C                   Z-ADD     99998         BLPNFV
197100950109     C                   UPDATE    FNBLP000
197200950323    3C                   ENDIF
197300950323     C*
197400950323     C* SE BOLLA GIA' TRASMESSA --> IMPOSTO SOLO IL FOGLIO A 99998
197500950323   X2C                   ELSE
197600950323     C                   Z-ADD     99998         BLPNFV
197700950323     C                   UPDATE    FNBLP000
197800950323    2C                   ENDIF
197900990915   1AC                   ENDIF
198000910906     C*
198100130527     C     K02BLP77_9    reade     FNBLP000                               22
198200950323    1C                   ENDDO
198300900608     C*
198400900608     C                   ENDSR
198500931022     C*
198600011121     C*--- SCRIVO FILE TASSAZIONE  fiftt00F/fiftd00F -----------------*
198700931019     C     CRTTAS        BEGSR
198800931203     C***
198900931203     C*  C R E A Z I O N E    F L F T T 0 0 F
199000931203     C***
199100011121     C* CONTROLLO SE ESISTE IL FILE fiftt OGNI VOLTA CHE CAMBIANO IL
199200931203     C*   CODICE PADRONCINO E LA DATA RITIRO
199300931203     C*
199400931203    1C     COMBLP        IFNE      COMSAV
199500931203     C*
199600931203     C                   EXSR      CRTFTT
199700931203     C*
199800931203     C* CONTROLLO IL PADRONCINO OGNI VOLTA CHE CAMBIA
199900950113     C     WCPR          IFNE      BLPPDR
200000931203     C* IL COD.PADRONCINO DELLA BOLLA DEVE ESISTERE NELLA SCHIERA PDR
200100040826     C                   Z-ADD     1             W                 4 0
200200140620     C                   SETOFF                                       05
200300950113     C     BLPPDR        LOOKUP    PDR(W)                                 05
200400950113     C                   Z-ADD     BLPPDR        COMPDR
200500931203     C                   ENDIF
200600931203     C*
200700950113     C                   MOVEL     COMBLP        COMSAV
200800931203    1C                   ENDIF
200900970930     C*
201000971027    1C     *IN05         IFEQ      *ON
201100931027     C* CONTROLLO SE IL TIPO BOLLA E' DA ESCLUDERE
201200931027     C                   EXSR      CTAB
201300931027     C*
201400011121     C* ALF1 = '1' ===> CREO I FILE fiftt00F E fiftd00F  SE PDR NON DIP
201500971027     C*                 CREO COMUNQUE FILE FNSTPR0F PER TRASMISSIONE
201600971027     C*                 STOP. PER I NON DIPENDENTI, IL CALCOLO STOP IN
201700971027     C*                 QUESTO MOMENTO E' SOLO PREVENTIVO. VERRA' RI-
201800971027     C*                 FATTO AL MOMENTO DELLA CONFERMA CONTEGGI
201900020528     c*                 28/05/02:Creo dati per conteggi anche per i dipenden
202000020528     c*                          ti
202100971027    2C     ALF1          IFEQ      '1'
202200931203     C* NUMERO SPEDIZIONI TOTALE
202300020528     C*  05              ADD       1             COMSET
202400020528     C                   ADD       1             COMSET
202500931202     C*
202600931019     C* IMPOSTO CAMPI CHIAVE
202700971027    3C     BLPFPP        IFEQ      'M'
202800950113     C                   Z-ADD     1000          COMNDC
202900971027   X3C                   ELSE
203000950113     C                   Z-ADD     5000          COMNDC
203100971027    3C                   ENDIF
203200950113     C                   ADD       BLPNRT        COMNDC
203300950113     C                   MOVEL     BLPRSM        COMRSC
203400950114     C                   MOVEL     BLPDRT        COMDDC
203500931019     C*
203600971027    3C     COMTB1        IFEQ      'F'
203700931202     C                   MOVEL     BLPKSC        CO2KSC
203800971027   X3C                   ELSE
203900971027    4C     BLPCCM        IFNE      0
204000931202     C                   MOVEL     BLPCCM        CO2KSC
204100971027   X4C                   ELSE
204200931202     C                   MOVEL     BLPKSC        CO2KSC
204300950801     C                   MOVE      CO2KSC        W0040             4 0
204400971027    5C     W0040         IFEQ      9999
204500950801     C                   MOVE      8888          CO2KSC
204600971027    5C                   END
204700971027    4C                   ENDIF
204800971027    3C                   ENDIF
204900931202     C*
205000931019     C*
205100931202     C***
205200970930     C*  C R E A Z I O N E    F N F T D 0 0 F
205300931202     C***
205400020325     c                   clear                   wstp
205500020325     C* ESCLUDENDO FRANCHI E ASSEGNATI VARI,
205600020325     C*   CHAINO FNCLS00F PER PRENDERE IL NUMERO DEGLI STOP
205700020325     C                   Z-ADD     CO2KSC        NUM4              4 0
205800020325    5C     NUM4          IFNE      *ALL'8'
205900020325     C     NUM4          ANDNE     *ALL'9'
206000020325     C     CO2KSC        CHAIN     FNCLS000                           33
206100020325     C  N33              z-add     CLSSTP        wstp
206200020325    5C                   ENDIF
206300970930     C*
206400020325     c     wstp          ifeq      *zeros
206500011121     C     KFTD          CHAIN     fiftd000                           32
206600020325     c  n32              z-add     ftdstp        wstp
206700020326     c                   endif
206800020326     c     wstp          ifeq      *zeros
206900020416     c* Se il numero stop da attribuire è ancora = a 0 devo cercare
207000020416     c* nella distinta il primo numero disponibile
207100020416     C     KFTD1         setgt     fiftd001
207200020416     C     KFTD1         readpe    fiftd001
207300020416     c                   if        not %eof
207400020416     c                   eval      wstp = (ftdstp+1)
207500020416     c     wstp          iflt      1000
207600020416     c                   z-add     1000          wstp
207700020416     c                   endif
207800020417   x6c                   else
207900020417     c                   z-add     1000          wstp
208000020416     c                   endif
208100020325     c                   endif
208200020325     C                   CLEAR                   fiftd000
208300931108     C*
208400020325     c                   z-add     blpaas        ftdaas
208500020325     c                   z-add     blplnp        ftdlnp
208600020325     c                   z-add     blpnrs        ftdnrs
208700020325     c                   z-add     blpnsp        ftdnsp
208800931123     C                   MOVEL     BLPCAM        FTDCAP
208900020325     C                   z-add     1             FTDSET
209000931108     C                   MOVEL     'N'           FTDFGC
209100020325     c                   z-add     wstp          ftdstp
209200931108     C* FLAG ANTEPORTO
209300030902     C** IMPOSTO SEMPRE ANCHE SE C'ERA GIA' IL FLAG ANTEPORTO
209400931108     C                   MOVEL     BLPFAP        FTDFIN
209500931108     C*
209600021108     C* VOLUME : - SE TOTALE   ("T"), SOMMO BLPVLc
209700021108     C*          - SE PARZIALE ("Z"), SOMMO BLPVLc (SE MAGGIORE)
209800021108     C*                               ALTRIMENTI SOMMO BLPVLF
209900050112     C     BLPNCR        IFEQ      BLPNCL
210000950113     C                   Z-ADD     BLPVLC        WVOL
210100940321     C*
210200971027   X4C                   ELSE
210300971027    5C     BLPVLC        IFGE      BLPVLF
210400950113     C                   Z-ADD     BLPVLC        WVOL
210500971027   X5C                   ELSE
210600950113     C                   Z-ADD     BLPVLF        WVOL
210700971027    5C                   ENDIF
210800971027    4C                   ENDIF
210900021108     C* peso   : - SE TOTALE  , SOMMO BLPpkc
211000021108     C*          - SE PARZIALE, SOMMO BLPPKC (SE MAGGIORE)
211100021108     C*                               ALTRIMENTI SOMMO BLPPKF
211200050112     C     BLPNCp        ifeq      BLPNCL
211300021108     C                   Z-ADD     BLPpkC        Wpes
211400021108     C*
211500021108   X4C                   ELSE
211600021108    5C     BLPpkC        IFGE      BLPpkF
211700021108     C                   Z-ADD     BLPpkC        Wpes
211800021108   X5C                   ELSE
211900021108     C                   Z-ADD     BLPpkF        Wpes
212000021108    5C                   ENDIF
212100021108    4C                   ENDIF
212200940321     C*
212300931202     C*
212400931202     C                   Z-ADD     BLPNCL        FTDNCL
212500021108     C                   Z-ADD     WPES          FTDPKL
212600950113     C                   Z-ADD     WVOL          FTDVLU
212700950113     C* CALCOLO I COLLI PER CUI APPLICARE ETICHETTATURA: ESCLUDO
212800050112     C*  LE BOLLE CON SERIE
212900971027    5C     BLPNRS        IFEQ      0
213000950113     C                   Z-ADD     BLPNCL        FTDNCE
213100971027    5C                   ENDIF
213200931019     C*
213300931108     C* IMPOSTO CAMPI CHIAVE
213400931019     C                   Z-ADD     COMPDR        FTDPDR
213500020325     C                   movel     COMPDR        FTDfgs
213600931019     C                   MOVEL     COMTSR        FTDTSR
213700931019     C                   Z-ADD     COMNDC        FTDNDC
213800950115     C                   Z-ADD     COMDDC        FTDDDC
213900950113     C                   MOVEL     COMRSC        FTDRSC
214000931202     C                   MOVEL     CO2KSC        FTDKSC
214100931019     C*
214200011121     C                   WRITE     fiftd000
214300931019     C*
214400970930     C***
214500970930     C*  C R E A Z I O N E    F N S T P R 0 F
214600970930     C***
214700970930     C                   Z-ADD     BLPAAS        STRAAS
214800970930     C                   Z-ADD     BLPLNP        STRLNP
214900970930     C                   Z-ADD     BLPNRS        STRNRS
215000970930     C                   Z-ADD     BLPNSP        STRNSP
215100970930     C                   Z-ADD     COMPDR        STRPDR
215200970930     C                   Z-ADD     COMDDC        STRDDC
215300970930     C                   Z-ADD     COMNDC        STRNDC
215400970930     C* NUMERO STOP
215500970930     C                   Z-ADD     CO2KSC        NUM4              4 0
215600970930     C                   CLEAR                   STRSTP
215700000615     C* imposto numero stop = 99999 se merce portata a magazzino
215800970930     C     BLPFDN        IFEQ      'S'
215900000615     C                   Z-ADD     99999         STRSTP
216000970930     C                   ELSE
216100970930     C* chaino FNCLS00F per prendere l'eventuale numero stop
216200970930     C     NUM4          IFNE      *ALL'8'
216300970930     C     NUM4          ANDNE     *ALL'9'
216400970930     C     CO2KSC        CHAIN     FNCLS000                           33
216500001020     C  N33              Z-ADD     CLSSTP        STRSTP
216600970930     C                   ENDIF
216700970930     C                   ENDIF
216800970930     C                   MOVEL     CO2KSC        STRKSC
216900970930     C                   MOVEL     COMRSC        STRRSC
217000970930    3C                   SELECT
217100970930     C     §3ABSD        WHENEQ    *BLANKS
217200970930     C     §3ABFI        ANDEQ     *BLANKS
217300970930     C                   MOVEL     'E'           STRINV
217400970930     C     §3ABSD        WHENEQ    *BLANKS
217500970930     C                   MOVEL     'S'           STRINV
217600970930     C     §3ABFI        WHENEQ    *BLANKS
217700970930     C                   MOVEL     'F'           STRINV
217800970930    3C                   ENDSL
217900970930     C                   WRITE     FNSTPR00
218000931019     C*
218100931019     C                   ENDIF
218200970930     C                   ENDIF
218300931019     C                   ENDSR
218400931019     C*
218500011121     C*--- CREAZIONE fiftt00F ----------------------------------------*
218600931203     C     CRTFTT        BEGSR
218700931203     C*
218800931203    2C     COMSET        IFGT      0
218900950113     C     WCPR          ANDGT     0
219000931203     C                   Z-ADD     WCPR          WPDR
219100950113     C     WFPP          IFEQ      'M'
219200950113     C                   Z-ADD     1000          WNDC
219300950113     C                   ELSE
219400950113     C                   Z-ADD     5000          WNDC
219500950113     C                   ENDIF
219600950113     C                   ADD       WNRT          WNDC
219700931203     C*
219800011121     C     KFTT          CHAIN     fiftt000                           32
219900931209     C* 32 OFF - AGGIORNAMENTO
220000931203    3C     *IN32         IFEQ      *OFF
220100931203     C*
220200931203     C                   ADD       COMSET        FTTSET
220300931203     C                   CLEAR                   COMSET
220400931209     C*
220500011121     C                   UPDATE    fiftt000
220600931209     C*
220700931210     C* SOLO SE GIA' VALORIZZATA
220800931210     C     FTTFVL        IFEQ      'V'
220900931209     C* RICHIAMO PGM DI SVALORIZZAZIONE DISTINTA PADRONCINO
221000931209     C                   Z-ADD     WPDR          PA3PDR
221100931209     C                   MOVEL     COMTSR        PA3TSR
221200931209     C                   Z-ADD     WNDC          PA3NDC
221300950115     C                   Z-ADD     WDRT          PA3DDC
221400931209     C                   MOVEL     'N'           PA3FCT
221500931209     C                   MOVEL     'N'           PA3FVD
221600931209     C                   MOVEL     'N'           PA3FVT
221700041122     C                   MOVEL     ' '           PA3sml
221800931209     C                   MOVEL     PARAM3        KPJBU
221900011121     C                   CALL      'FICNE2R'
222000931209     C                   PARM                    KPJBA
222100931210     C                   ENDIF
222200931203     C*
222300931203   X3C                   ELSE
222400931209     C* 32 ON  - IMMISSIONE
222500011121     C                   CLEAR                   fiftt000
222600931209     C*
222700931203     C                   ADD       COMSET        FTTSET
222800931203     C                   CLEAR                   COMSET
222900931203     C                   Z-ADD     WPDR          FTTPDR
223000020325     C                   movel     WPDR          FTTfgs
223100931203     C                   MOVEL     COMTSR        FTTTSR
223200950113     C                   Z-ADD     WNDC          FTTNDC
223300931203     C                   Z-ADD     WDRT          FTTDDC
223400950113     C                   MOVEL     WFPP          FTTFPP
223500950113     C     WFPP          IFEQ      'M'
223600950113     C                   MOVEL     ETM(W)        FTTPEP
223700030107     c                   Movel     Scm(w)        FttLvl
223800950113     C                   ELSE
223900950113     C                   MOVEL     ETP(W)        FTTPEP
224000030107     c                   Movel     Scp(w)        FttLvl
224100950113    2C                   ENDIF
224200931203     C*
224300011127      * In base alla data del documento aggancio la tab.GEDCN per
224400011127      * impostare la divisa della valorizzazione dei conteggi.
224500011127     c                   if        fttddc <= §geddtfv
224600011127     c                   movel     §geditl       fttdiv
224700011127     c                   else
224800011127     c                   movel     §gedeur       fttdiv
224900011127     c                   end
225000011127      *
225100011121     C                   WRITE     fiftt000
225200931203    3C                   ENDIF
225300931203    2C                   ENDIF
225400931203     C*
225500931203     C                   ENDSR
225600931203     C*
225700030107      *---------------------------------------------------------------*
225800030107      * Creazione Fiftt00f x ritiri annullati - bolle "FY"
225900030107      *---------------------------------------------------------------*
226000030107     c     CrtFtt1       BegSr
226100030107
226200030107     c                   Z-Add     1             w
226300030107     c     BlpPdr        Lookup    Pdr(w)                                 30
226400030107
226500030107     c                   Movel     BlpPdr        Wpdr
226600030107     c                   If        BlpFpp = 'M'
226700030107     c                   Z-Add     1000          Wndc
226800030107     c                   Else
226900030107     c                   Z-Add     5000          Wndc
227000030107     c                   EndIf
227100030107     c                   Add       BlpNrt        Wndc
227200030107     c                   Movel     BlpDrt        Wdrt
227300030107     c     Kftt          Chain     Fiftt01l
227400030107      * Aggiornamento
227500030107If  1c                   If        %Found(Fiftt01l)
227600030107     c                   Add       1             FttTbs
227700030107     c                   Update    Fiftt000
227800030107
227900030107      * Solo se già valorizzata
228000030107     c                   If        FttFvl = 'V'
228100030107      * Richiamo pgm di svalorizzazione distinta autotrasportatore
228200030107     c                   Z-Add     Wpdr          Pa3Pdr
228300030107     c                   Movel     ComTsr        Pa3Tsr
228400030107     c                   Z-Add     Wndc          Pa3Ndc
228500030107     c                   Z-Add     Wdrt          Pa3Ddc
228600030107     c                   Movel     'N'           Pa3Fct
228700030107     c                   Movel     'N'           Pa3Fvd
228800030107     c                   Movel     'N'           Pa3Fvt
228900041122     C                   MOVEL     ' '           PA3sml
229000030107     c                   Movel     Param3        Kpjbu
229100030107     c                   Call      'FICNE2R'
229200030107     c                   Parm                    Kpjba
229300030107     c                   EndIf
229400030107
229500030107   x1c                   Else
229600030107      * Immissione
229700030107     c                   Clear                   Fiftt000
229800030107
229900030107     c                   Z-Add     1             FttTbs
230000030107     c                   Z-Add     Wpdr          FttPdr
230100030107     c                   Movel     Wpdr          FttFgs
230200030107     c                   Movel     ComTsr        FttTsr
230300030107     c                   Z-Add     Wndc          FttNdc
230400030107     c                   Z-Add     Wdrt          FttDdc
230500030107     c                   Movel     BlpFpp        FttFpp
230600030107     c                   If        BlpFpp = 'M'
230700030107     c                   Movel     Etm(w)        FttPep
230800030107     c                   Movel     Scm(w)        FttLvl
230900030107     c                   Else
231000030107     c                   Movel     Etp(w)        FttPep
231100030107     c                   Movel     Scp(w)        FttLvl
231200030107     c                   EndIf
231300030107      * In base alla data del documento aggancio la tab.GEDCN per
231400030107      * impostare la divisa della valorizzazione dei conteggi.
231500030107     c                   If        FttDdc <= §geddtfv
231600030107     c                   Movel     §geditl       FttDiv
231700030107     c                   Else
231800030107     c                   Movel     §gedeur       fttDiv
231900030107     c                   EndIf
232000030107
232100030107     c                   Write     Fiftt000
232200030107    1c                   EndIf
232300030107
232400030107     c                   EndSr
232500030107
232600931027     C*--- CHAIN TABEL -----------------------------------------------*
232700931027     C     CTAB          BEGSR
232800931028     C* EFFETTUO LA CHAIN IN TABEL SOLO SE CAMBIANO IL TIPO BOLLA E LA
232900931028     C*   FILIALE DI APPARTENENZA DEL PADRONCINO
233000950113     C                   MOVEL     WCPR          COMAPP
233100950113     C                   MOVE      BLPCBO        COMAPP            4
233200931124     C*
233300931028     C     SAVAPP        IFNE      COMAPP
233400931124     C                   MOVEL     ' '           ALF1              1
233500931027     C*
233600931028     C                   MOVEL     '8C'          COD
233700931108     C                   MOVEL     'R'           COMKEY            2
233800950109     C                   MOVE      BLPCBO        COMKEY
233900931027     C                   MOVEL     *BLANKS       KEY
234000931027     C                   MOVEL     COMKEY        KEY
234100950113     C                   MOVEL     WCPR          W0030             3 0
234200950113     C                   MOVE      W0030         KEY
234300931027     C*
234400931027     C     KTAB          CHAIN     TABEL                              16
234500931124     C* SE NON ESISTE OPPURE ESISTE MA E' ANNULLATO, RIPROVO CON LA
234600931124     C*   FILIALE 000 = TUTTE
234700931027     C     *IN16         IFEQ      *ON
234800931027     C     *IN16         OREQ      *OFF
234900931027     C     TBLFLG        ANDNE     ' '
235000931124     C*
235100931027     C                   MOVE      '000'         KEY
235200931027     C     KTAB          CHAIN     TABEL                              16
235300931124     C*
235400931124     C     *IN16         IFEQ      *ON
235500931124     C     *IN16         OREQ      *OFF
235600931124     C     TBLFLG        ANDNE     ' '
235700931124     C                   MOVEL     '1'           ALF1
235800931124     C                   ENDIF
235900931124     C*
236000931027     C                   ENDIF
236100931027     C*
236200931028     C                   MOVEL     COMAPP        SAVAPP            4
236300931028     C                   ENDIF
236400931027     C                   ENDSR
236500910909     C**-----------------------------------------------*
236600000111     C** RIEMPO CAMPI DEL FILE FISPP00T
236700910909     C**-----------------------------------------------*
236800910909     C     RIESPP        BEGSR
236900000111     C                   Z-ADD     BRVPES        SPPPES
237000950109     C                   MOVEL     BRVLNP        SPPLNP
237100950109     C                   MOVEL     BRVLNA        SPPLNA
237200950109     C                   MOVEL     BRVNRS        SPPNRS
237300950109     C                   MOVEL     BRVNSC        SPPNSC
237400950109     C                   MOVEL     BRVZNC        SPPZNC
237500950109     C                   MOVEL     BRVNPS        SPPNPS
237600950109     C                   MOVEL     BRVFGS        SPPFGS
237700950109     C                   MOVEL     BRVNFV        SPPNFV
237800940527     C                   MOVEL     WDTP          SPPDFV
237900950109     C                   Z-ADD     1             SPPNPG
238000950113     C                   MOVEL     BRVCAN        SPPCAN
238100950118     C                   MOVEL     BRVVUC        SPPVUC
238200990906     C                   MOVEL     BRVPUC        SPPPUC
238300961203     C     §TVDEF        IFEQ      'S'
238400961203     C                   MOVEL     'F'           SPPATR
238500961203     C                   ENDIF
238600000111     C                   Z-ADD     BRVDFS        SPPDFS
238700070123     C                   movel     BRVHMS        SPPHMS
238800000111     C                   Z-ADD     BRVDCS        SPPDCS
238900070123     C                   movel     BRVHCS        SPPHCS
239000000320     C                   Z-ADD     WDTR          SPPDTR
239100050707     C                   TIME                    W0140            14 0
239200000320     C                   MOVEL     W0140         SPPHTR
239300000111     C                   Z-ADD     BRVFLE        SPPFLE
239400910909     C                   ENDSR
239500910910     C**-----------------------------------------------*
239600000428     C** RIEMPO CAMPI DEL FILE FNFVA01T-FNFWA00T
239700910910     C**-----------------------------------------------*
239800910910     C     RIEFVA        BEGSR
239900950123     C                   MOVEL     1             FVANPG
240000950109     C                   MOVEL     FGVLNP        FVALNP
240100910910     C                   MOVEL     FGVNFV        FVANFV
240200910910     C                   MOVEL     FGVDFV        FVADFV
240300910910     C                   MOVEL     FGVLNA        FVALNA
240400921202     C                   MOVEL     FGVFFV        FVAFFV
240500921202     C                   MOVEL     FGVFF2        FVAFF2
240600921202     C                   MOVEL     FGVFLP        FVAFLP
240700921202     C                   MOVEL     FGVFL2        FVAFL2
240800951116     C                   MOVEL     FGVTRN        FVATRN
240900951120     C                   MOVEL     FGVTTR        FVATTR
241000951116     C                   MOVEL     FGVFRQ        FVATFM
241100951116     C                   MOVEL     FGVTRM        FVATRM
241200951116     C                   MOVEL     FGVTRR        FVATRR
241300951116     C                   MOVEL     FGVPDR        FVAPDR
241400951116     C                   MOVEL     FGVDPD        FVADPD
241500951116     C                   MOVEL     FGVCCN        FVACCN
241600130429     C                   MOVEL     FGVpmb        FVADCN
241700960123     C                   MOVEL     FGVDTP        FVADRP
241800960123     C                   MOVEL     FGVHMP        FVAHRP
241900960124     C* CALL PGM PER PRENDERE LA CAUSALE RITARDO
242000960124     C                   MOVEL     FGVLNP        SA2LNP
242100960124     C                   MOVEL     FGVNFV        SA2NFV
242200960124     C                   CLEAR                   SA2TLA
242300960124     C                   CLEAR                   SA2RTP
242400960124     C                   CALL      'FNLSA2R4'
242500960124     C                   PARM                    SA2TLA            1
242600960124     C                   PARM                    SA2LNP
242700960124     C                   PARM                    SA2NFV
242800960124     C                   PARM                    SA2RTP
242900960123     C*
243000960124     C                   MOVEL     SA2RTP        FVARTP
243100000428     C*
243200000428     C* RIEMPI FILE DI APPOGGIO FWA
243300000428     C     WFGW          IFEQ      'S'
243400000428     C                   CLEAR                   FNFWA000
243500000428     C                   MOVEL     FGWATB        FWAATB
243600000428     C                   MOVEL     FGWNFV        FWANFV
243700000428     C                   MOVEL     FGWLNP        FWALNP
243800000428     C                   MOVEL     FGWFGS        FWAFGS
243900000428     C                   MOVEL     FGWIPT        FWAIPT
244000000428     C                   MOVEL     FGWVIP        FWAVIP
244100000428     C                   MOVEL     FGWIES        FWAIES
244200000428     C                   MOVEL     FGWVES        FWAVES
244300000428     C                   MOVEL     FGWFF3        FWAFF3
244400000428     C                   MOVEL     FGWFF4        FWAFF4
244500000428     C                   MOVEL     FGWFL3        FWAFL3
244600000428     C                   MOVEL     FGWFL4        FWAFL4
244700000428     C                   ENDIF
244800960123     C*
244900960123     C* CONTROLLO SE IN ARRIVO OCCORRERA' SBINARE IL F.VIAGGIO PARTENZA
245000960123     C*   DAL FOGLIO ARRIVI
245100960123    1C     PA2FLG        IFEQ      'M'
245200960123     C                   CLEAR                   FVASBN
245300960123   X1C                   ELSE
245400960123     C                   MOVEL     'S'           FVASBN
245500960123    1C                   ENDIF
245600960123     C*
245700910910     C                   ENDSR
245800060710     C**-----------------------------------------------*
245900060710     C** SCrittura dati mancanti di FNFV4 in fnfv400F per arrivo
246000060710     C**-----------------------------------------------*
246100060710     c     SCRIVIFV4     begsr
246200060710     c
246300060710     C* SCRITTURA TIPO AUTOMEZZO E CATEGORIA TRAINO
246400060710     c* direttamente nel file fisico senza passare dal file fnfv400t
246500060710     c* verifico se esiste già
246600060710     C                   MOVEL     'P'           FV4EPA
246700060710     C                   MOVEL     'V'           FV4TRC
246800060710     C                   Z-ADD     FGVLNP        FV4LNP
246900060710     C                   Z-ADD     FGVNFV        FV4NFV
247000060710     C                   Z-ADD     *ZEROS        FV4LAI
247100060710     c     kfv4e         setll     fnfv401l                               23
247200060710    1c                   if        not *in23
247300060710     C                   CLEAR                   DSFV4V
247400060710     C                   MOVEL     *BLANKS       FV4ATB
247500060710     C                   MOVEL     FGVTMZ        §V4TMZ
247600060710     C                   MOVE      FGVSNP        §V4TTR
247700060710     C                   MOVEL     DSFV4V        FV4NOT
247800060710     C                   movel     'N'           FV4ft1
247900060710     C                   Z-ADD     DATEU         FV4dt1
248000060710     c* Per la sede verifico se da trasmettere
248100060710     c     fv4trc        lookup    fv4s                                   23
248200060710    2c                   if        *in23
248300060710     C                   clear                   FV4ft2
248400060710     C                   clear                   FV4dt2
248500060710   x2c                   else
248600060710     C                   movel     'N'           FV4ft2
248700060710     C                   Z-ADD     DATEU         FV4dt2
248800060710    2c                   endif
248900060710     C                   WRITE     FNFV4000
249000060710    1C                   ENDIF
249100060710     c                   ENDSR
249200970221     C**-----------------------------------------------*
249300970221     C* VEDO SE RIELABORARE LE STATISTICHE
249400970221     C**-----------------------------------------------*
249500970221     C     FNSTA         BEGSR
249600001027     C                   CLEAR                   WWLNA
249700001027     C**
249800021016     c                   If        WdtPar <= §SdfParU
249900970221     C                   MOVE      WDTPAR        SA2DTS            8 0
250000970221     C                   MOVE      'P'           SA2TRC            1
250100970221     C                   CALL      'FNLSA2R3'
250200970221     C                   PARM                    SA2TLA
250300970221     C                   PARM                    SA2TRC
250400970221     C                   PARM                    SA2DTS
250500970221     C                   PARM                    WWLNA             3 0
250600970221     C                   END
250700001027     C*
250800021016     c                   If        WdtPar <= §SdfTerU
250900970221     C                   MOVE      WDTPAR        SA2DTS
251000970221     C                   MOVE      'T'           SA2TRC
251100970221     C                   CALL      'FNLSA2R3'
251200970221     C                   PARM                    SA2TLA
251300970221     C                   PARM                    SA2TRC
251400970221     C                   PARM                    SA2DTS
251500970221     C                   PARM                    WWLNA             3 0
251600001027     C                   END
251700970221     C                   ENDSR
251800910909     C**-----------------------------------------------*
251900910909     C** TRASMISSIONE SPUNTE
252000910909     C**-----------------------------------------------*
252100910909     C     SPUNTE        BEGSR
252200960124     C                   CLEAR                   SA2TLA
252300980210     C                   SETOFF                                       0106
252400920505     C                   SETOFF                                       08
252500910909     C                   Z-ADD     0             SAVLNA            3 0
252600920703     C                   Z-ADD     0             SAVLNP            3 0
252700950323     C                   CLEAR                   SAVCBO
252800950203     C                   CLEAR                   SAVSPE
252900960708     C                   CLEAR                   WLAST
253000970314     C                   CLEAR                   WRKSPE
253100150730     c                   eval      wnpg=1
253200000320     C* PRENDO DATA/ORA SISTEMA PER L'AGGIORNAMENTO DI DATA E ORA
253300000320     C* TRASMISSIONE SPUNTE
253400000320     C                   TIME                    W0140            14 0
253500000320     C                   CLEAR                   WLBDAT
253600000320     C                   MOVE      W0140         G02DAT
253700000320     C                   CALL      'XSRDA8'
253800000320     C                   PARM                    WLBDAT
253900000320     C                   Z-ADD     G02INV        WDTR              8 0
254000931021     C*
254100910909     C* LETTURA PER FOGLIO VIAGGIO
254200070123     C     KBRV          SETLL     FNBRV02
254300070123     C     KBRV          READE     FNBRV02                                22
254400910909     C*
254500950215    1C     *IN22         DOWEQ     '0'
254600950215    2C     BRVFTR        IFEQ      *BLANKS
254700931220     C                   CLEAR                   WSPEN
254800931220     C                   CLEAR                   WSPET
254900950215     C                   CLEAR                   WAGG
255000980326     C                   CLEAR                   NCLU
255100960603     C**
255200960603     C* DETERMINO LNP DELLA BOLLA SE COLLO CON SERIE IN CONFERMA DA
255300960603     C* ALTRI AS
255400960604     C                   MOVE      'S'           WALCF
255500960604     C                   MOVEL     BRVLNP        KLNP
255600960604     C                   MOVEL     BRVLNA        KLNA
255700960604     C                   MOVEL     BRVNRS        KNRS
255800960604     C                   MOVEL     BRVNSC        KNSC
255900960603     C                   EXSR      RICLNP
256000910909     C**
256100910909     C** L I N E A   A R R I V O   C A M B I A T A
256200950215    3C     SAVLNA        IFNE      BRVLNA
256300920427     C* SCRIVO PER LIN ARRIVO CON SCARICO IL TERMINAL SPUNTE " " E "T"
256400920427     C   08
256500910912     C* SCRIVO LINEA DI ARRIVO SPUNTE "A" SE C'E' TRANSITO
256600980210    4COR 06SAVLNA        IFNE      0
256700910912     C                   EXSR      WTRLNA
256800950215    4C                   END
256900050203     c* mi salvo in una schiera parallela alle linee del foglio viaggio, se
257000050203     x*  locale, se presenti spunte transito
257100050204     c                   if        fgvlnp=flptfa  and savlna>0
257200050204     C                   Z-ADD     1             B
257300050204     C                   MOVEL     savLNA        COMPRO
257400050203     C     COMPRO        LOOKUP    FFV(B)                                 20
257500050203     C   20              MOVEL     winvialocale  winvloc(B)
257600050203     c                   endif
257700050203     c                   clear                   winvialocale
257800930712     C*
257900980210     C                   SETOFF                                       060801
258000930127     C                   Z-ADD     1             B                 3 0
258100950109     C                   MOVEL     BRVLNA        COMPRO            3
258200910912     C     COMPRO        LOOKUP    FFV(B)                                 20
258300910912     C*     TROVATA FIL SCARICO
258400930127     C   20              MOVEL     FLP(B)        COMLIN            3 0
258500910912     C* NON TROVATA --> METTO LINEA ARRIVO STESSA
258600950109     C  N20              MOVEL     BRVLNA        COMLIN
258700931022     C*
258800970811     C* F I L I A L E   S C A R I C O
258900051031     C                   CLEAR                   dslv55
259000051031     C****               MOVEL     'P'           d55TPT
259100051031     c                   z-add     fgvlnp        d55lnp
259200051031     C                   Z-ADD     COMLIN        d55LIN
259300051031     C                   Z-ADD     FGVDFV        d55DRF
259400051031     C                   CALL      'FNLV55R'
259500051031     C                   PARM                    dslv55
259600051031     C                   MOVEL     d55TFP        SAVFEL            3 0
259700051031     C                   MOVEL     d55TFP        VARX
259800051031     C                   MOVEL     d55TFP        VAR2X
259900051031     C                   Z-ADD     d55TFA        FLPTFA
260000950109     C*
260100021125     C                   SETON                                        01
260200910912     C* APERTURA MEMBRI
260300970811     C                   EXSR      CAEXSP
260400050204    4C***                END
260500920703     C*
260600980210     C* Reperisco terminal di arrivo della linea di arrivo
260700971105     C* Reperisco anche il terminal di partenza
260800130531     C                   CLEAR                   dslv55
260900130531     C                   MOVEL     ' '           d55TPT
261000130531     C                   Z-ADD     BRVLNA        d55LIN
261100130531     C                   Z-ADD     FGVLNP        d55LNP
261200130531     C                   Z-ADD     WDTP          d55DRF
261300130531     C                   CALL      'FNLV55R'
261400130531     C                   PARM                    dslv55
261500130531     C                   Z-ADD     d55TFA        LNATFA
261600130531     C                   Z-ADD     d55TFP        LNAFEL
261700051031     c
261800051031     c* Reperisco anche il terminal di arrivo della spunta letta
261900051031     C                   CLEAR                   dslv55
262000051031     C                   MOVEL     ' '           d55TPT
262100051031     C                   Z-ADD     BRVLNA        d55LIN
262200051031     C                   Z-ADD     wlnp          d55LNP
262300051031     C* Se fv poste gli passo il p.o. poste
262400051031     C     WFGW          IFEQ      'S'
262500051031     C     FGWFGS        ANDGT     *ZEROS
262600051031     C                   Z-ADD     FGWFGS        d55LNP
262700051031     C                   ENDIF
262800051031     C                   Z-ADD     WDTP          d55DRF
262900051031     C                   CALL      'FNLV55R'
263000051031     C                   PARM                    dslv55
263100051031     C                   Z-ADD     d55TFA        spuTFA            3 0
263200051031     c
263300031218     c* Determino se collo locale per sottomissione lr15
263400031218     c* Una volta acceso in92 non si deve rispegnere perchè basta che
263500031218     c* ce ne sia una per sottomettere
263600031218    1c     *in92         ifeq      *off
263700051031    3c     fgvlnp        ifeq      lnatfa
263800031218     c                   seton                                        92
263900031218    3c                   endif
264000031218    1c                   endif
264100980210     C**
264200980210     C* SE SCARICO >< DA LINEA DI ARRIVO
264300980210    4C     COMLIN        IFNE      BRVLNA
264400940107     C*
264500980210     C* 06  ON - T R A N S I T O
264600940107     C* SE SCARICO >< DA TERMINAL DI ARRIVO LINEA DI ARRIVO
264700980210    6C     COMLIN        IFNE      LNATFA
264800980210     C                   SETON                                        06
264900930312     C* APRO MEMBRI DEL TRANSITO
265000980210     C                   EXSR      TRANSI
265100980210     C                   EXSR      CAEX08
265200920703     C*
265300950215    6C                   END
265400950215    4C                   END
265500910909     C*
265600950109     C                   MOVEL     BRVLNA        SAVLNA
265700051031     C                   MOVEL     wlnp          SAVLNp
265800051031   x3c                   else
265900051031     c* se non cambiata la linea di arrivo vedo se cambiata la
266000051031     c*  linea di partenza spunte per la ricerca del tfa della spunta
266100051031    4c                   if        wlnp<>savlnp
266200051031     c* Reperisco anche il terminal di arrivo della spunta letta
266300051031     C                   CLEAR                   dslv55
266400051031     C                   MOVEL     ' '           d55TPT
266500051031     C                   Z-ADD     BRVLNA        d55LIN
266600051031     C                   Z-ADD     wlnp          d55LNP
266700051031     C* Se fv poste gli passo il p.o. poste
266800051031    5C     WFGW          IFEQ      'S'
266900051031     C     FGWFGS        ANDGT     *ZEROS
267000051031     C                   Z-ADD     FGWFGS        d55LNP
267100051031    5C                   ENDIF
267200051031     C                   Z-ADD     WDTP          d55DRF
267300051031     C                   CALL      'FNLV55R'
267400051031     C                   PARM                    dslv55
267500051031     C                   Z-ADD     d55TFA        spuTFA            3 0
267600051031     c                   z-add     wlnp          savlnp
267700051031    4C                   ENDIF
267800051031    3C                   ENDIF
267900950515     C*
268000960603     C                   CLEAR                   WTRAN
268100960603     C                   SETOFF                                       19
268200910912     C*
268300950515     C*  19 ON  - NON E' SPUNTA DA TRANSITO
268400950515     C*  19 OFF -     E' SPUNTA DA TRANSITO
268500960531     C     WLNP          LOOKUP    L1                                     19
268600980401     C**
268700980401     C* IGNORO OGNI AGGIORNAMENTO SE SI TRATTA DI SPUNTA DEFLUENZA
268800980401     C*  PERCHE' E' A TUTTI GLI EFFETTI UNA SPUNTA ARRIVI
268900980401   2AC     §TVDEF        IFNE      'S'
269000911018     C*
269100911106     C** T R A N S I T O
269200930203     C*
269300970221    3C     *IN19         IFEQ      *OFF
269400970828     C     WTAB5F        ANDEQ     ' '
269500950113     C*
269600911106     C**    PRENDO BOLLE E BLT DAL FILE DI TRANSITO
269700960603     C*     Non ripeto la chain se l'ho già fatta durante la ricerca
269800960603     C*     della lnp bolla
269900960603     C                   SELECT
270000960611     C     WOKBT         WHENEQ    *BLANKS
270100020606     C     KBtt37        CHAIN     FNBTT37L                           20
270200960611     C     WOKBT         WHENEQ    'S'
270300960603     C                   SETOFF                                       20
270400960603     C                   ENDSL
270500921014     C*
270600950515     C* CHIUDO ANOMALIA : "DISGUIDO"
270700970221     C*
270800970221   3AC     *IN20         IFEQ      *ON
270900970221     C                   Z-ADD     200           COMCAA
271000970221     C                   EXSR      CHIUAN
271100050927     c                   clear                   noinvia           1
271200130527     c* Per foglio locale non invio la spunta come disguido
271300050927     c*  (si tratta di colli di defluenza sparato per errore sul foglio
271400050927     c*   locale ma non si tratta nè di vera defluenza, nè di disguido)
271500050927     c                   if        wesisteanm=' '
271600050927     c                   eval      noinvia='S'
271700050927     c                   endif
271800970827     C* SOLO SE HO TROVATO L'ANOMALIA
271900970827     C  N21              Z-ADD     1             NCLU
272000921014     C**
272100970221  X3AC                   ELSE
272200970221     C**
272300950515     C                   MOVEL     'S'           WTRAN             1
272400921014     C*
272500950515     C* CHIUDO ANOMALIA : "BOLLA TRANSITO"
272600921014     C                   Z-ADD     125           COMCAA
272700921014     C                   EXSR      CHIUAN
272800921014     C*
272900950621    4C     BTTDUT        IFNE      0
273000950621     C                   MOVEL     'N'           WAGG              1
273100950621    4C                   ENDIF
273200950621     C*
273300950113     C                   MOVEL     FGVNFV        BTTNFV
273400970818     C* SE ERA PISTOLA FASULLA IMPOSTO 87
273500970827     C     BTTPUT        IFGT      0
273600970818     C     BTTPUT        LOOKUP    NPF                                    39
273700970827     C                   ELSE
273800970827     C                   SETOFF                                       39
273900970827     C                   ENDIF
274000970827     C**
274100970818     C     *IN39         IFEQ      *ON
274200950621     C                   MOVE      87            BTTPUT
274300950621     C                   ELSE
274400950113     C                   MOVEL     BRVNPS        BTTPUT
274500950621     C                   END
274600970827     C*
274700950706     C* Se le statistiche partito e a terra sono già state elaborate
274800950705     C* scrivo o aggiorno file FNSTA00F per rielaborazione statistiche
274900970221     C     BTTDUT        IFGT      *ZEROS
275000970221     C                   Z-ADD     BTTDUT        WDTPAR
275100970221     C                   EXSR      FNSTA
275200970221     C                   ENDIF
275300970221     C*
275400091027     c                   Z-ADD     WDTP          WDTPAR
275500970221     C                   EXSR      FNSTA
275600970221     C**
275700950113     C                   MOVEL     WDTP          BTTDUT
275800980407     C                   Z-ADD     1             NCLU
275900910927     C*
276000020606     C                   UPDATE    FNBTT037
276100950130     C*
276200950130     C* C A M B I A T A   S P E D I Z I O N E --> RICHAINO LA BOLLA
276300950215    4C     BTTSPE        IFNE      SAVSPE
276400970307     C*
276500970307     C* IMPOSTO LA SPEDIZIONE PERCHE' A CAMBIO ULTIMO COLLO SCRIVO AGB
276600970307     C     WRKSPE        IFEQ      0
276700970307     C                   Z-ADD     BTTSPE        WRKSPE
276800970307     C                   MOVEL     'T'           WRKTBO
276900970307     C                   ENDIF
277000950130     C*
277100020605     C     KBTT3         CHAIN     FNBTP11L                           21
277200930127     C*
277300091027     c* BTPDT3  contiene il terminal a cui trasmetto
277400020606     c                   z-add     btpdt3        savdt3            3 0
277500020606     c                   z-add     comlin        btpdt3
277600091027     c
277700950109     C                   MOVEL     'T'           BTPFT1
277800950113     C                   Z-ADD     DATEU         BTPDT1
277900950109     C                   MOVEL     'N'           BTPFT2
278000950113     C                   Z-ADD     DATEU         BTPDT2
278100950109     C                   MOVEL     'N'           BTPFT3
278200940110     C*
278300940110     C* CALCOLO PESO E VOLUME UNITARIO DEL COLLO
278400950110     C                   EXSR      CALUNT
278500931022     C*
278600911011     C* AGGIORNO DATA PARTENZA DAL TRANSITO
278700950215    5C     BTPDUT        IFEQ      0
278800931220     C* MUOVO 1 NEL CAMPO DELLE SPEDIZIONI
278900931220     C                   Z-ADD     1             WSPET
279000950109     C                   MOVEL     WDTP          BTPDUT
279100950621     C                   ELSE
279200950621     C                   MOVEL     'T'           AGBTBO
279300950621     C                   MOVE      BTPAAS        AGBAAS
279400950621     C                   MOVE      BTPLNP        AGBLNP
279500950621     C                   MOVE      BTPNRS        AGBNRS
279600950621     C                   MOVE      BTPNSP        AGBNSP
279700950621     C                   MOVEL     'DT'          AGBAGB
279800950630     C                   WRITE     FNAGB000                             39
279900950215    5C                   END
280000950109     C*
280100950109     C                   UPDATE    FNBTP000
280200950130     C*
280300950130     C                   MOVEL     BTTSPE        SAVSPE
280400910927     C*
280500091027     C* SE TRANSITO PER TRANSITO : INVIO DI NUOVO BOLLA
280600020606    5C   06SAVdt3        IFNE      COMLIN
280700031219     c* Se p.o. di scarico è anche il ter-part.della lnp bolla non devo
280800031219     c* ritrasmettere
280900130531     c                   clear                   dslv55
281000130531     c                   movel     'P'           d55tpt
281100130531     c                   z-add     wlnp          d55lin
281200130531     c                   z-add     wdtp          d55drf
281300130531     c                   call      'FNLV55R'
281400130531     c                   parm                    dslv55
281500130531     c     comlin        ifne      d55tfp
281600950109     C                   EXSR      WTRBTP
281700130531     C                   endif
281800950808    5C                   END
281900950808    4C                   ENDIF
282000970307     C**
282100970307     C* A CAMBIO ULTIMO COLLO SCRIVO
282200970307     C                   MOVEL     'T'           BTTTBO
282300970307     C     BTTKEY        IFNE      WRKKEY
282400970307     C* SCRIVO IL FILE FNAGB01L PER AGGIORNARE DATA ULTIMO COLLO
282500970307     C                   MOVEL     WRKTBO        AGBTBO
282600970307     C                   MOVEL     WRKAAS        AGBAAS
282700970307     C                   MOVEL     WRKLNP        AGBLNP
282800970307     C                   MOVEL     WRKNRS        AGBNRS
282900970307     C                   MOVEL     WRKNSP        AGBNSP
283000970307     C                   MOVEL     'DT'          AGBAGB
283100970307     C                   WRITE     FNAGB000                             39
283200970307     C**
283300970307     C                   MOVEL     BTTKEY        WRKKEY
283400970307     C                   ENDIF
283500970221   3AC                   ENDIF
283600970221    3C                   END
283700910912     C**
283800991022     C** L N P
283900910927     C**
284000960604    3C     *IN19         IFEQ      *ON
284100970827     C     WTAB5F        OREQ      'S'
284200960603     C*     Non ripeto la chain se l'ho già fatta durante la ricerca
284300960603     C*     della lnp bolla
284400960603     C                   SELECT
284500960611     C     WOKBL         WHENEQ    *BLANKS
284600960603     C     KBRV4         CHAIN     FNBLT27L                           20
284700960611     C     WOKBL         WHENEQ    'S'
284800960603     C                   SETOFF                                       20
284900960603     C                   ENDSL
285000911108     C*
285100921014     C* CREO   ANOMALIA : "MANCA BOLLA"
285200991022     C***
285300991022   3AC     *IN20         IFEQ      *ON
285400911108     C                   Z-ADD     105           COMCAA
285500990517     C     KANM3         SETLL     FNANM                                  21
285600911108     C  N21              EXSR      CRTANM
285700970221     C*
285800991022     C* NON SOMMO I COLLI  MANCA REC BOLLA
285900970221     C*  PERCHE' LO FARO' AL MOMENTO DELLA
286000970221     C*  BORDERIZZAZIONE DELLA BOLLA, SE E' UN MANCA REC BOLLA VERO
286100970221     C*  PER IL PROSPETTO NOLI (LE STATISTICHE HANNO UN LORO GIRO
286200970827  X3AC                   ELSE
286300991022     C* SE ESISTE NELL'ALTRO S.I. DO IL DISGUIDO E BASTA
286400040531   3BC**   WALTSI        IFEQ      'S'
286500040531     C**                 MOVEL     'D'           FLG
286600040531     C**                 EXSR      CALL44
286700040531  X3BC**                 ELSE
286800911108     C*
286900950515     C                   CLEAR                   WTRAN
287000110907     c
287100110907     c                   setoff                                       37
287200921014     C*
287300950515     C* CREO   ANOMALIA : "MANCA RECORD BOLLA BORDERIZZATA"
287400040625     C* NON LO FACCIO SE bolla poste
287500110907    4c                   if        bltnfv=0 or brvnps =88 or
287600110907     c                             brvnps=90
287700040603     c* Verifico se bolla con merce di valore da memorizzare nella
287800040603     c*  anomalia
287900040603     c     kar5          chain(N)  fiar501l
288000040603     c                   if        %found(fiar501l)
288100040603     c                   movel     ar5uni        dar5gen
288200040603     c                   else
288300040603     c                   clear                   dar5gen
288400040603     c                   endif
288500110907
288600110907    5C     BLTNFV        IFEQ      0
288700110907     C                   Z-ADD     110           COMCAA
288800040603     C     KANM3         SETLL     FNANM                                  21
288900911108     C  N21              EXSR      CRTANM
289000110907    5C                   END
289100110907     c*
289200110907     C* CREO   ANOMALIA : "COLLO MAI AFFIDATO"
289300110907    5c                   if        brvnps =88 or brvnps=90
289400110907     C* PER SICUREZZA VERIFICO CHE NON SIA PRESENTE UNA QUALSIASI SPUNTA
289500110907     C* IN ALTRA FILIALE
289600110907     c                   clear                   EsSpunta          1
289700110907     C                   MOVEL     DSBRV         DSBRVS
289800110907     C     KBRV4         setll     FNbrv07l
289900110907     C     KBRV4         reade     FNbrv07l
290000110907     c                   dow       not %eof(fnbrv07l)
290100110907     c* Escludo le pistole completamente fasulle
290200110907     c                   if        brvnps<>88 and brvnps<>90
290300110907     c                   eval      EsSpunta='S'
290400110907     c                   leave
290500110907     c                   endif
290600110907     c
290700110907     C     KBRV4         reade     FNbrv07l
290800110907     c                   enddo
290900110907     c
291000110907     C* Reimposto campi di fnbrv con dati salvati
291100110907     C                   MOVEL     DSBRVS        DSBRV
291200110907     C*
291300110908    6c                   if        EsSpunta=' '
291400110907     C                   Z-ADD     115           COMCAA
291500110907     C     KANM3         SETLL     FNANM                                  21
291600110907     C  N21              EXSR      CRTANM
291700110907    6c                   endif
291800110907    5c                   endif
291900110907     c
292000110907    4c                   endif
292100911108     C*
292200921014     C* CHIUDO ANOMALIA : "BOLLA PARTENZA"
292300921014     C                   Z-ADD     120           COMCAA
292400020412     C                   EXSR      CHIUAN
292500911108     C*
292600911108     C*
292700950621    4C     BLTDFV        IFNE      0
292800950621     C                   MOVEL     'N'           WAGG
292900950621    4C                   ENDIF
293000950621     C*
293100930520     C                   MOVEL     FGVNFV        BLTNFV
293200970221     C*
293300950706     C* Se le statistiche partito e a terra isono già state elaborate
293400950705     C* scrivo o aggiorno file FNSTA00F per rielaborazione statistiche
293500970221     C     BLTDFV        IFGT      *ZEROS
293600970221     C                   Z-ADD     BLTDFV        WDTPAR
293700970221     C                   EXSR      FNSTA
293800970221     C                   ENDIF
293900970221     C**
294000950707     C                   Z-ADD     WDTP          WDTPAR
294100970221     C                   EXSR      FNSTA
294200970221     C*
294300950706     C                   MOVEL     WDTP          BLTDFV
294400970627     C* PULISCO IL CAMPO CHE MI SEGNALA UNA EVENTUALE MANCANZA DI COLLO
294500970627     C* IMPORT DATOMI DAL PARTNER
294600970627     C                   CLEAR                   BLTFL1
294700970818     C* SE ERA PISTOLA FASULLA IMPOSTO 87
294800970827     C     BLTNPP        IFGT      0
294900970818     C     BLTNPP        LOOKUP    NPF                                    39
295000970827     C                   ELSE
295100970827     C                   SETOFF                                       39
295200970827     C                   ENDIF
295300970827     C**
295400970818     C     *IN39         IFEQ      *ON
295500950621     C                   MOVE      87            BLTNPP
295600950621     C                   ELSE
295700950110     C                   MOVEL     BRVNPS        BLTNPP
295800950623     C                   END
295900910927     C                   MOVEL     COMLIN        BLTFLP
296000980407     C                   Z-ADD     1             NCLU
296100910927     C*
296200950110     C                   UPDATE    FNBLT027
296300950130     C*
296400950130     C* C A M B I A T A   S P E D I Z I O N E --> RICHAINO LA BOLLA
296500950215    4C     BLTSPE        IFNE      SAVSPE
296600970307     C*
296700970307     C* IMPOSTO LA SPEDIZIONE PERCHE' A CAMBIO ULTIMO COLLO SCRIVO AGB
296800970307     C     WRKSPE        IFEQ      0
296900970307     C                   Z-ADD     BLTSPE        WRKSPE
297000970307     C                   MOVEL     'P'           WRKTBO
297100970307     C                   ENDIF
297200950130     C*
297300151022     C     KBLT3         CHAIN     FNBLP01L
297400940107     C*
297500940107     C* CALCOLO PESO E VOLUME UNITARIO DEL COLLO
297600940107     C                   EXSR      CALUNI
297700931022     C*
297800910927     C* SE E' IL PRIMO COLLO CHE PARTE METTO DATA FV
297900950215    5C     BLPDPC        IFEQ      0
298000931220     C* MUOVO 1 NEL CAMPO DELLE SPEDIZIONI
298100931220     C                   Z-ADD     1             WSPEN
298200950124     C                   MOVEL     WDTP          BLPDPC
298300950215    5C                   END
298400910930     C* FIL TRANSITO
298500930127     C                   MOVEL     BLPFLP        SAVFLP            3 0
298600910930     C                   MOVEL     COMLIN        BLPFLP
298700910930     C* AGGIORNO
298800151022     c                   if        %found(fnblp01l)
298900950110     C                   UPDATE    FNBLP001
299000151022     c                   else
299100151022     c                   exsr      sr_errore
299200151022     c                   endif
299300910927     C*
299400910930     C** LNP PER TRANSITO
299500910927     C**   SE NON CORRISPONDE A TRANSITO DELLA BOLLA SPEDISCO LA BOLLA
299600920505     C**   PERCHE' POSSO MANDARE VIA LA BOLLA PER UNA CERTA LINEA E
299700930707     C**   IL GIORNO DOPO I COLLI PER UN'ALTRA LINEA (RIMANDO BOLLA)
299800980210    5C   06SAVFLP        IFNE      0
299900920512     C     SAVFLP        ANDNE     COMLIN
300000930312     C     SAVFLP        OREQ      0
300100930312     C     BLPLNA        ANDNE     COMLIN
300200091027     C
300300091027     c* Imposto il numero foglio viaggio  di chiusura
300400091027     c                   eval      blpnfv=fgv2
300500950110     C                   WRITE     FNBLPT00
300600080423     C
300700040310    5c                   ENDIF
300800950130     C*
300900950130     C*
301000950130     C                   MOVEL     BLTSPE        SAVSPE
301100950215    4C                   ENDIF
301200970307     C*
301300970307     C* A CAMBIO ULTIMO COLLO SCRIVO
301400970307     C                   MOVEL     'P'           BLTTBO
301500970307     C     BLTKEY        IFNE      WRKKEY
301600970307     C* SCRIVO IL FILE FNAGB01L PER AGGIORNARE DATA ULTIMO COLLO
301700970307     C                   MOVEL     WRKTBO        AGBTBO
301800970307     C                   MOVEL     WRKAAS        AGBAAS
301900970307     C                   MOVEL     WRKLNP        AGBLNP
302000970307     C                   MOVEL     WRKNRS        AGBNRS
302100970307     C                   MOVEL     WRKNSP        AGBNSP
302200970307     C                   MOVEL     'DT'          AGBAGB
302300970307     C                   WRITE     FNAGB000                             39
302400970307     C**
302500970307     C                   MOVEL     BLTKEY        WRKKEY
302600970307     C                   ENDIF
302700950130     C*
302800040531   3BC**                 ENDIF
302900991022   3AC                   ENDIF
303000960403    3C                   ENDIF
303100980326   2AC                   ENDIF
303200910927     C**
303300910912     C** I N V I O   L A   S P U N T A
303400910927     C**
303500950516    3C   01              DO
303600000111     C                   CLEAR                   FISPP000
303700950215     C                   CLEAR                   SPPATR
303800950215     C                   CLEAR                   SPPFLP
303900910912     C* SE TRANSITO PER LNA '--> METTO FLAG "T"
304000970814     C*  SE E' LINEA/SERIE COMFERMABILE DA SIMFEL NON CREO IL DISGUIDO
304100950515     C     *IN19         IFEQ      *OFF
304200970814     C     WTAB5F        ANDEQ     ' '
304300040401     c* Se spunta che invio al p.o. di transito memorizzo il p.o. di scarico
304400040401     c   06              z-add     comlin        sppflp
304500950515     C  N06              MOVEL     'T'           SPPATR
304600971105     C  N06              MOVEL     FGVLNP        SPPFLP
304700940322     C* SE E' UN COLLO IN DISGUIDO METTO "D"
304800950515     C     WTRAN         IFEQ      ' '
304900050927     c* non si tratta di disguido se foglio locale e non esiste l'anom.200
305000050927     c                   if        fgvlnp<>flptfa or
305100050927     c                             (fgvlnp=flptfa and noinvia=' ')
305200950515     C                   MOVEL     'D'           SPPATR
305300971105     C                   MOVEL     FGVLNP        SPPFLP
305400050927     C                   endif
305500050927     c
305600050927     C                   ENDIF
305700020607     c                   else
305800020607     c* Se spunta che invio al p.o. di transito memorizzo il p.o. di scarico
305900020607     c   06              z-add     comlin        sppflp
306000950515     C                   ENDIF
306100991025     C* SE BOLLA IN ALTRO S.I. INVIO LA SPUNTA COME "D"
306200040531     C**   WALTSI        IFEQ      'S'
306300040531     C**                 MOVEL     'D'           SPPATR
306400040531     C**                 MOVEL     FGVLNP        SPPFLP
306500040531     C**                 ENDIF
306600910912     C*
306700910912     C                   EXSR      RIESPP
306800910910     C*
306900050204     c* Se sul foglio locale ho memorizzato spunte transito, devo
307000050204     c*  inviare il foglio per l'abbinamento
307100050204     c                   if        FGVLNP<>flpTFA Or
307200050204     c                             (fgvlnp=flptfa and sppatr<>' ' and
307300051031     c                              sppatr<>'F' and not *in06 and
307400051031     c                              sputfa<>flptfa)
307500050204     c                   if        fgvlnp=flptfa
307600050204     C                   MOVEL     'S'           winvialocale
307700050204     c                   endif
307800050204     c
307900000111     C                   WRITE     FISPP000
308000950215    3C                   END
308100050203    3C                   END
308200931022     C*
308300950110     C* AGGIORNO FNFVD00F
308400950215     C     WAGG          IFEQ      ' '
308500931022     C                   EXSR      AGGFVD
308600950215     C                   ENDIF
308700931022     C*
308800911107     C* AGGIORNO SPUNTA
308900950110     C                   MOVEL     'T'           BRVFTR
309000000320     C                   Z-ADD     WDTR          BRVDTR
309100050707     c                   time                    w0140
309200000320     C                   MOVEL     W0140         BRVHTR
309300070123     C                   UPDATE    FNBRV02
309400980728     C                   ELSE
309500070123     C* SE NE LEGGO UNA CON CODICE ANOMALIA E LA DEVO SCRIVERE IN FNBRV
309600980728     C*  SOLO SE SI TRATTA DI UN FOGLIO DI DEFLUENZA
309700980728     C     BRVCAN        IFEQ      'E'
309800070123     C     kbrve         setll     fNbrve                                 99
309900070123     C  n99              WRITE     FNBRVE
310000980728     C                   ENDIF
310100950215    2C                   END
310200070123     C     KBRV          READE     FNBRV02                                22
310300910910     C*
310400950215    1C                   END
310500910910     C*
310600920427     C* SCRIVO PER LIN ARRIVO CON SCARICO IL TERMINAL SPUNTE " " E "T"
310700920427     C   08
310800910912     C* SCRIVO PER ULTIMA LINEA ARRIVO
310900980210     COR 06SAVLNA        IFNE      0
311000960708     C                   MOVE      'S'           WLAST             1
311100910912     C                   EXSR      WTRLNA
311200910910     C                   END
311300050204     c* mi salvo in una schiera parallela alle linee del foglio viaggio, se
311400050204     x*  locale, se presenti spunte transito
311500050204     c                   if        fgvlnp=flptfa  and savlna>0
311600050204     C                   Z-ADD     1             B
311700050204     C                   MOVEL     savLNA        COMPRO
311800050204     C     COMPRO        LOOKUP    FFV(B)                                 20
311900050204     C   20              MOVEL     winvialocale  winvloc(B)
312000050204     c                   endif
312100970307     C**
312200970307     C* SCRIVO L'ULTIMO FNAGB
312300970307     C     WRKSPE        IFGT      0
312400970307     C                   MOVEL     WRKTBO        AGBTBO
312500970307     C                   MOVEL     WRKAAS        AGBAAS
312600970307     C                   MOVEL     WRKLNP        AGBLNP
312700970307     C                   MOVEL     WRKNRS        AGBNRS
312800970307     C                   MOVEL     WRKNSP        AGBNSP
312900970307     C                   MOVEL     'DT'          AGBAGB
313000970307     C                   WRITE     FNAGB000                             39
313100970307     C                   ENDIF
313200910909     C*
313300910909     C                   ENDSR
313400940107     C*
313500950110     C* CALCOLO VOLUME E PESO UNITARI FNBTT--------------------------*
313600950110     C     CALUNT        BEGSR
313700940107     C* COLLI / PESO / VOLUME UNITARI
313800940107     C                   Z-ADD     1             NCLU
313900030902     c*
314000050112     C     BtPNCR        IFEQ      BtPNCL
314100021111     C                   Z-ADD     BtPVLC        Wfvdvol
314200021111     C*
314300021111   X4C                   ELSE
314400021111    5C     BtPVLC        IFGE      BtPVLb
314500021111     C                   Z-ADD     BtPVLC        Wfvdvol
314600021111   X5C                   ELSE
314700021111     C                   Z-ADD     BtPVLb        Wfvdvol
314800021111    5C                   ENDIF
314900021111    4C                   ENDIF
315000021111     c
315100021111     C     wfvdvol       DIV(H)    BtPNCL        VOLU
315200030902     c*
315300050112     C     BtPNCp        ifeq      BtPNCL
315400021111     C                   Z-ADD     BtPpkC        Wfvdpes
315500021111     C*
315600021111   X4C                   ELSE
315700021111    5C     BtPpkC        IFGE      BtPpkb
315800021111     C                   Z-ADD     BtPpkC        Wfvdpes
315900021111   X5C                   ELSE
316000021111     C                   Z-ADD     BtPpkb        Wfvdpes
316100021111    5C                   ENDIF
316200021111    4C                   ENDIF
316300030902     c*
316400021111     C     wfvdpes       DIV(H)    BtPNCL        PKGU
316500940107     C*
316600940107     C                   ENDSR
316700950110     C*
316800950110     C* CALCOLO VOLUME E PESO UNITARI FNBLT--------------------------*
316900950110     C     CALUNI        BEGSR
317000950110     C* COLLI / PESO / VOLUME UNITARI
317100950110     C                   Z-ADD     1             NCLU
317200021111     c
317300050112     C     BLPNCR        ifeq      BLPNCL
317400021111     C                   Z-ADD     BLPVLC        Wfvdvol
317500021111     C*
317600021111   X4C                   ELSE
317700021111    5C     BLPVLC        IFGE      BLPVLF
317800021111     C                   Z-ADD     BLPVLC        Wfvdvol
317900021111   X5C                   ELSE
318000021111     C                   Z-ADD     BLPVLF        Wfvdvol
318100021111    5C                   ENDIF
318200021111    4C                   ENDIF
318300030902     c*
318400021111     C     wfvdvol       DIV(H)    BLPNCL        VOLU
318500021111     c
318600050112     C     BLPNCp        ifeq      BLPNCL
318700021111     C                   Z-ADD     BLPpkC        Wfvdpes
318800021111     C*
318900021111   X4C                   ELSE
319000021111    5C     BLPpkC        IFGE      BLPpkF
319100021111     C                   Z-ADD     BLPpkC        Wfvdpes
319200021111   X5C                   ELSE
319300021111     C                   Z-ADD     BLPpkF        Wfvdpes
319400021111    5C                   ENDIF
319500021111    4C                   ENDIF
319600030902     c*
319700021111     C     wfvdpes       DIV(H)    BLPNCL        PKGU
319800950110     C*
319900950110     C                   ENDSR
320000930708     C*
320100950110     C*--- AGGIORNO CAMPI DI FNFVD -----------------------------------*
320200931022     C     AGGFVD        BEGSR
320300931022     C*
320400980210    1C     NCLU          IFGT      0
320500950110     C* ESCLUSE LE BOLLE RECUPERO: MA DAL MOMENTO CHE NON HANNO COLLI.
320600950110     C*  NON MI PREOCCUPO
320700970221     C* ESCLUDO IL CODICE SOCIALE 2 ===> SDI  SE TROVATA BOLLA
320800980210    2C     *IN19         IFEQ      *ON
320900970221     C     *IN20         ANDEQ     *OFF
321000070223     C***  BLPSCL        ANDNE     '2'
321100970221     C* ELABORO SE NON TROVATA BOLLA
321200970221     C     *IN19         OREQ      *ON
321300970221     C     *IN20         ANDEQ     *ON
321400980210     C* ELABORO SE E' COLLO DISGUIDO O TRANSITO
321500950110     C     *IN19         OREQ      *OFF
321600970221     C*
321700950110     C* TIPO RECORD 1= BOLLE DELL'AREA    2=BOLLE TRANSITO
321800950515     C                   MOVEL     '2'           WTRC
321900950515     C*
322000980210    3C     *IN19         IFEQ      *ON
322100950515     C                   MOVEL     '1'           WTRC
322200980210     C                   ELSE
322300980210     C     WLNP          CHAIN     AZORG01L                           18
322400980210    4C     ORGDIT        IFEQ      'SDI'
322500980210     C                   MOVEL     '3'           WTRC
322600980210    4C                   ENDIF
322700980210    3C                   ENDIF
322800931214     C*
322900950110     C     KFVD2         CHAIN     FNFVD000                           18
323000950110     C   18              CLEAR                   FNFVD000
323100950110     C*
323200950110     C* COLLI
323300950110     C                   ADD       NCLU          FVDCLA
323400931026     C*
323500950515     C* SE TROVATA LA BOLLA CALCOLO PER BENE IL PARTITO
323600980210    3C     WTRAN         IFEQ      'S'
323700970221     C     *IN19         OREQ      *ON
323800970221     C     *IN20         ANDEQ     *OFF
323900950515     C                   EXSR      RIEFVD
324000980210    3C                   ENDIF
324100931022     C*
324200980210    3C     *IN18         IFEQ      *ON
324300931022     C* SOLO PER IMMISSIONE
324400931022     C                   Z-ADD     FGVLNP        FVDLNP
324500931022     C                   Z-ADD     FGV2          FVDNFV
324600931022     C                   Z-ADD     SAVLNA        FVDLNA
324700931022     C                   Z-ADD     COMLIN        FVDFLP
324800950110     C                   MOVEL     WTRC          FVDTRC
324900931022     C*
325000950110     C                   WRITE     FNFVD000
325100931022     C*
325200931022     C                   ELSE
325300931022     C*
325400931022     C                   MOVEL     ' '           FVDFTR
325500931022     C                   Z-ADD     0             FVDDTR
325600931022     C*
325700950110     C                   UPDATE    FNFVD000
325800980210    3C                   ENDIF
325900931022     C*
326000980210    2C                   ENDIF
326100980210    1C                   ENDIF
326200931022     C                   ENDSR
326300931022     C*
326400950110     C*--- RIEMPO CAMPI DI FNFVD -------------------------------------*
326500931022     C     RIEFVD        BEGSR
326600931022     C***
326700931022     C***  19 OFF  -   T  R  A  N  S  I  T  I
326800931022     C***
326900950515     C     WTRAN         IFEQ      'S'
327000930708     C* PESO
327100950113     C     BTPPKB        IFGT      3500
327200030213      * Peso medio
327300030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
327400950123     C                   ADD       PKGU          FVDKOA
327500930708     C                   ELSE
327600030213      * Peso medio
327700030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
327800950123     C                   ADD       PKGU          FVDKFA
327900930708     C                   ENDIF
328000950110     C* VOLUME
328100950110     C                   Z-ADD     1             Z                 3 0
328200950110     C     BTPTSP        LOOKUP    TSP(Z)                                 31
328300950110     C*
328400950110     C* NON TROVATO TIPO SERVIZIO --> LO IMPOSTO IN SCHIERA
328500950110     C     *IN31         IFEQ      *OFF
328600950110     C                   Z-ADD     1             Z
328700950110     C     ' '           LOOKUP    TSP(Z)                                 31
328800950110     C   31              MOVEL     BTPTSP        TSP(Z)
328900950110     C  N31              Z-ADD     1             Z
329000950110     C                   ENDIF
329100950110     C*
329200030213      * Volume medio
329300030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
329400950110     C                   ADD       VOLU          VOL(Z)
329500930708     C*
329600950110     C                   ADD       WSPET         FVDSNA
329700931022     C*
329800931022     C                   ELSE
329900931022     C***
330000970221     C***     A  R  E  A
330100931022     C***
330200930709     C*
330300930708     C* PESO
330400130530     C**   BLPPKF        IFGT      3500
330500130530     c                   if        wfvdpes>3500
330600030213      * Peso medio
330700030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
330800931022     C                   ADD       PKGU          FVDKOA
330900931022     C                   ELSE
331000030213      * Peso medio
331100030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
331200931022     C                   ADD       PKGU          FVDKFA
331300930708     C                   ENDIF
331400930708     C*
331500950110     C* VOLUME
331600950110     C                   Z-ADD     1             Z                 3 0
331700950110     C     BLPTSP        LOOKUP    TSP(Z)                                 31
331800950110     C*
331900950110     C* NON TROVATO TIPO SERVIZIO --> LO IMPOSTO IN SCHIERA
332000950110     C     *IN31         IFEQ      *OFF
332100950110     C                   Z-ADD     1             Z
332200950110     C     ' '           LOOKUP    TSP(Z)                                 31
332300950110     C   31              MOVEL     BLPTSP        TSP(Z)
332400950110     C  N31              Z-ADD     1             Z
332500950110     C                   ENDIF
332600950110     C*
332700030213      * Volume medio
332800030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
332900950110     C                   ADD       VOLU          VOL(Z)
333000950110     C                   ADD       WSPEN         FVDSNA
333100931022     C*
333200931022     C                   ENDIF
333300930708     C                   ENDSR
333400930708     C*
333500930709     C*--- CHAIN TABEL -----------------------------------------------*
333600930709     C     CTABEL        BEGSR
333700931021     C* EFFETTUO LA CHAIN IN TABEL SOLO SE CAMBIA IL TIPO BOLLA
333800950109     C     SAVCBO        IFNE      BLPCBO
333900930709     C                   MOVEL     '3A'          COD
334000930709     C                   MOVEL     *BLANKS       KEY
334100950109     C                   MOVEL     BLPCBO        KEY
334200930709     C     KTAB          CHAIN     TABEL                              31
334300931202     C     *IN31         IFEQ      *OFF
334400931202     C                   MOVEL     TBLUNI        DS3A
334500931202     C                   MOVEL     §3ATB1        COMTB1            1
334600931202     C                   ELSE
334700931202     C                   MOVE      ' '           §3ARBL
334800950413     C                   MOVEL     ' '           §3ABSD
334900950413     C                   MOVEL     ' '           §3ABFI
335000931202     C                   ENDIF
335100931202     C*
335200950109     C                   MOVEL     BLPCBO        SAVCBO
335300930709     C                   ENDIF
335400930709     C                   ENDSR
335500950113     C*
335600950113     C* RITRASMETTO IL RECORD TRANSITO ------------------------------*
335700950113     C     WTRBTP        BEGSR
335800950113     C                   MOVEL     BTPAAS        BLPAAS
335900950113     C                   MOVEL     BTPLNP        BLPLNP
336000950113     C                   MOVEL     BTPNRS        BLPNRS
336100950113     C                   MOVEL     BTPNSP        BLPNSP
336200950113     C                   MOVEL     BTPMGS        BLPMGS
336300950113     C                   MOVEL     BTPCBO        BLPCBO
336400950113     C                   MOVEL     BTPFNS        BLPFNS
336500950113     C                   MOVEL     BTPLNA        BLPLNA
336600950113     C                   MOVEL     BTPZNC        BLPZNC
336700950113     C                   MOVEL     BTPRSM        BLPRSM
336800950113     C                   MOVEL     BTPRSD        BLPRSD
336900950113     C                   MOVEL     BTPTSP        BLPTSP
337000950113     C                   MOVEL     BTPNCL        BLPNCL
337100950113     C                   MOVEL     BTPPKB        BLPPKB
337200950113     C                   MOVEL     BTPFVB        BLPFVB
337300950113     C                   MOVEL     BTPVLB        BLPVLB
337400950113     C                   MOVEL     BTPFLS        BLPFLS
337500950113     C                   MOVEL     BTPNCD        BLPNCD
337600950113     C                   MOVEL     BTPNCA        BLPNCA
337700950113     C                   MOVEL     BTPXCO        BLPXCO
337800950113     C                   MOVEL     BTPDBR        BLPDBR
337900021127     C                   MOVEL     BTPFLE        BLPFLE
338000950113     C                   MOVEL     BTPTFP        BLPTFP
338100950113     C                   MOVEL     BTPFEA        BLPFEA
338200950113     C                   MOVEL     BTPTFA        BLPTFA
338300091027     c* nei campi BLPNFV e BLPFLE imposto il foglio di partenza dei
338400091027     c*  colli per poter leggere queste bolle nel pgm della statistica
338500091027     c*    MC in arrivo
338600091027     c                   eval      blpnfv=fgv2
338700091027     c                   eval      blpfle=fgvlnp
338800091027     c
338900950113     C                   WRITE     FNBLPT00
339000950113     C                   ENDSR
339100911004     C**-----------------------------------------------*
339200911004     C** TRASFERISCO FOGLI VIAGGIO
339300911004     C**-----------------------------------------------*
339400911004     C     FVIAGG        BEGSR
339500150730     C                   Z-ADD     1             wnpg
339600150730     C                   Z-ADD     1             B
339700911004     C                   Z-ADD     0             X
339800930929     C                   CLEAR                   LP2
339900911106     C                   EXSR      RIEFVA
340000020403     c                   time                    w0060             6 0
340100911004     C*
340200940620    1C     FLP(B)        DOWGT     *ZEROS
340300920928     C*
340400960212     C* CONTROLLO SE LA SUA LINEA DI ARRIVO HA SPUNTE O BOLLE
340500960202     C                   MOVE      FFV(B)        BRVLNA
340600070123     C     KBRC2         SETLL     FNBRV02                                30
340700130530     C  N30k03BLP77      setll     FNBLP77L                               30
340800960402     C   30              MOVEL     'S'           WBOL              1
340900960402     C  N30              MOVEL     ' '           WBOL              1
341000920928     C*
341100960212     C* PER I FV NON ABBINATI DA NON TRASMETTERE IN SEDE, INVIO SOLO
341200960212     C*  SE HO BOLLE O SPUNTE
341300960402    2C     WBOL          IFEQ      'S'
341400960212     C     FGVTRN        ORGT      0
341500960212     C     §TVUTC        OREQ      'S'
341600040329     c* escludo fogli viaggio poste locali che non sono da trasmettere
341700040329     c* in arrivo
341800040329   2bc     wfvpt         ifne      'S'
341900040329     c     fgvttr        orne      'L'
342000020403     c*
342100020403     c                   add       1             w0060             6 0
342200960212     C*
342300911004     C                   MOVEL     FLP(B)        COMLIN
342400040329     C                   CLEAR                   fnlv54ds
342500040317     C****               MOVEL     'P'           D55TPT
342600040329     c                   z-add     fgvlnp        i54lnp
342700040329     C* Se fv poste gli passo il p.o. poste
342800040329     C     WFGW          IFEQ      'S'
342900040329     C     FGWFGS        ANDGT     *ZEROS
343000040329     C                   Z-ADD     FGWFGS        i54LNP
343100040329     C                   ENDIF
343200040329     C                   MOVEL     COMLIN        i54LIN
343300040329     C                   MOVEL     FGVDFV        i54drf
343400040329     C                   CALL      'FNLV54R'
343500040329     C                   PARM                    fnlv54ds
343600040329    3C     o54ERR        IFEQ      *BLANKS
343700920928     C*
343800911004     C* SE NON LO HO GIA' INVIATO INVIO
343900921202     C     COMLIN        LOOKUP    LP2                                    20
344000940620    4C  N20              DO
344100040826     C                   ADD       1             X                 4 0
344200921202     C                   MOVEL     COMLIN        LP2(X)
344300921202     C*
344400921202     C* TRASMETTO SE NON E' LOCALE
344500040329    5C     o54tfp        IFNE      FGVLNP
344600040329     c     §tvdef        andne     'S'
344700040329     c     fgvlnp        orne      o54tfa
344800911004     C*
344900040329     C                   MOVEL     o54TFP        VARX
345000921202     C                   EXSR      CAEXFV
345100020403     c*
345200911004     C*
345300960126     C* TRASMETTO SOLO SE FV APERTO O SFLAGGATO(DA TRASMETTERE)
345400960126    6C     FGVFCF        IFEQ      ' '
345500960126     C     FGVFTR        OREQ      ' '
345600950113     C                   MOVEL     ' '           FVAATR
345700921202     C                   MOVEL     COMLIN        FVALAI
345800950113     C                   WRITE     FNFVA0TT
345900000428    7C     WFGW          IFEQ      'S'
346000000428     C                   MOVEL     COMLIN        FWALAI
346100000428     C                   WRITE     FNFWA000
346200000428    7C                   ENDIF
346300060710     c* Scrittura del file FNFV4
346400060710     c                   EXSR      SCRIVIFV4
346500020807    6C                   ENDIF
346600010622      *
346700940620    5C                   ENDIF
346800940620    4C                   ENDDO
346900940620    3C                   ENDIF
347000920427     C*
347100940620     C* R I T R A S M E T T O   F O G L I O   X   T R A N S I T I  SE:
347200940620     C*
347300971105     C* 1) SE NON E' UN FOGLIO LOCALE (TFP DELLO SCARICO=FGVLNP)
347400940620     C*    E LA LINEA ARRIVO E' DIVERSA DALLO SCARICO
347500960402     C*    E SOLO SE HA BOLLE O SPUNTE
347600050927     C*    oppure locale con spunte transito
347700020807    3C     FFV(B)        IFNE      FLP(B)
347800960402     C     WBOL          ANDEQ     'S'
347900050927     c     winvloc(B)    oreq      'S'
348000921202     C*
348100920427     C                   MOVEL     FFV(B)        COMLNA
348200970516     C* Richiamo pgm per determinare terminal di arrivo e partenza
348300970703     C* della lna
348400031110     C                   CLEAR                   fnlv54ds
348500031112     C                   MOVE      COMLNA        i54LIN
348600031112     C                   MOVE      FGVLNP        i54LNP
348700000620     C* Se fv poste gli passo il p.o. poste
348800000620     C     WFGW          IFEQ      'S'
348900000620     C     FGWFGS        ANDGT     *ZEROS
349000031112     C                   Z-ADD     FGWFGS        i54LNP
349100000620     C                   ENDIF
349200031112     C                   MOVE      FGVDFV        i54DRF
349300031110     C                   CALL      'FNLV54R'
349400031110     C                   PARM                    fnlv54ds
349500921202     C*
349600940620     C*
349700050203     C* 2) SE TFP DELLA LINEA ARRIVO NON E' LOCALE
349800031110    4C     O54TFP        IFNE      FGVLNP
349900040317     c     fgvlnp        orne      o54tfa
350000050203     c
350100050203     c* 3) Trasmetto anche il foglio locale se presenti delle spunte
350200050203     c*    transito
350300050203     c     winvloc(B)    oreq      'S'
350400940620     C*
350500130531     c* Trasmettere
350600130531     c*  sempre al terminal di arrivo, tranne quando ci sono  spunte
350700050203     c*  transito su un foglio locale (vale solo per le  linee di arriv
350800050203     c*  un po' terminal arrvioe un po' 2 livello in arrivo)
350900980126     C*
351000050203     C**   WINV          IFEQ      'T'
351100050203     c                   if        winvloc(B)=' '
351200031110     C                   MOVEL     o54TFA        WLNA
351300980126     C                   ELSE
351400980126     C                   MOVEL     COMLNA        WLNA
351500980126     C                   ENDIF
351600921118     C*
351700050203     C* 3) SE NON L'HO GIA'INVIATO
351800980126     C     WLNA          LOOKUP    LP2                                    20
351900940620    6C  N20              DO
352000921118     C                   ADD       1             X
352100980126     C                   MOVEL     WLNA          LP2(X)
352200921118     C*
352300050203     c* Devo trasmettere il fogio nel membro del terPaart del TerARR
352400050203     c*  dell alna, tranne se foglio locale con invio spunte transito
352500050203     c     winvloc(B)    ifeq      ' '
352600040217     C     O54TFP        andne     O54TFA
352700040218     c                   move      o54tfa        wtfa
352800130716     C*****              CLEAR                   dslv55
352900130716     c*****              movel     'P'           d55tpt
353000130716     C*****              MOVE      wtfa          d55LIN
353100130716     C*****              MOVE      FGVDFV        d55DRF
353200130716     C*****              CALL      'FNLV55R'
353300130716     C*****              PARM                    dslv55
353400130716     C                   CLEAR                   fnlv54ds
353500130716     c                   movel     'P'           i54tpt
353600130716     C                   MOVE      wtfa          i54LIN
353700130716     C                   MOVE      FGVDFV        i54DRF
353800130716     C                   CALL      'FNLV54R'
353900130716     C                   PARM                    fnlv54ds
354000040217     c                   endif
354100040217     c*
354200130716     C****               MOVEL     d55TFP        VARX
354300130716     C                   MOVEL     o54TFP        VARX
354400920427     C                   EXSR      CAEXFV
354500921202     C*
354600960126     C* TRASMETTO SOLO SE FV APERTO O SFLAGGATO(CIOE' DA TRASMETTERE)
354700960126    7C     FGVFCF        IFEQ      ' '
354800960126     C     FGVFTR        OREQ      ' '
354900950113     C                   MOVEL     'T'           FVAATR
355000980126     C                   MOVEL     WLNA          FVALAI
355100950113     C                   WRITE     FNFVA0TT
355200020807    8C     WFGW          IFEQ      'S'
355300000428     C                   MOVEL     WLNA          FWALAI
355400000428     C                   WRITE     FNFWA000
355500020807    8C                   ENDIF
355600060710     c
355700060710     c* non mi serve più scrivere fnfv4 in quanto ho già scritto prima
355800060710     c*  il file fisico, unico, che trasmette eventualente anche
355900060710     c*  in sede (una sola volta quindi basta)
356000060710     c*********          EXSR      SCRIVIFV4
356100060710    7C                   ENDIF
356200010622      *
356300940620    6C                   ENDDO
356400940620    4C                   ENDIF
356500040329    3c                   ENDIF
356600040329   2bc                   endif
356700960212    2C                   ENDIF
356800911004     C*
356900911004     C                   ADD       1             B
357000940620    1C                   ENDDO
357100911004     C*
357200951120     C* SCRIVO FILE DI APPOGGIO DA INVIARE IN SEDE SE:
357300951120     C*   - IL FOGLIO NON E' ABBINATO
357400951120     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' VUOTO
357500951120     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO ED E'
357600951120     C*     DA UTILIZZARE NEI CONTROLLI DI SEDE (§TVUTS = "S")
357700951120     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO MA NON
357800951120     C*     C'E' IL RECORD IN TABELLA
357900950113     C                   MOVEL     ' '           FVAATR
358000951120     C*
358100960212    1C     FGVTRN        IFEQ      0
358200960212    2C     FGVTTR        OREQ      ' '
358300960212    3C     §TVUTC        OREQ      'S'
358400950113     C                   WRITE     FNFGV046
358500951120    1C                   ENDIF
358600930709     C*
358700911004     C                   ENDSR
358800910910     C**-----------------------------------------------*
358900950516     C** SCARICO SPUNTE PER L'AS DELLA LNA
359000910910     C**-----------------------------------------------*
359100910912     C     WTRLNA        BEGSR
359200910910     C*
359300050204     c* Secondo me questo IF si può eliminare ..... (ES)
359400040317    4C     lnafel        IFne      FGVLNP
359500040317     c     fgvlnp        orne      lnatfa
359600960604     C*
359700960806     C                   MOVEL     'N'           WALCF
359800930520     C*
359900040217     c* se transito e ter-arr della lna e lna sono sullo stesso ogas
360000040217     c* trasmetto a ter-part del ter-arr di lna
360100040217     c     *in06         ifeq      *off
360200000725     C                   MOVEL     LNAFEL        VARX
360300040217     c                   else
360400130531     C                   CLEAR                   dslv55
360500130531     C                   MOVEL     'P'           d55TPT
360600130531     C                   z-add     lnatfa        d55LIN
360700130531     C                   Z-ADD     FGVDFV        d55DRF
360800130531     c                   call      'FNLV55R'
360900130531     c                   parm                    dslv55
361000130531     C                   MOVEL     d55tfp        VARX
361100040217     c                   endif
361200910910     C                   EXSR      CAEXSP
361300910910     C*
361400950113     C                   MOVEL     BRVLNA        COMLNA            3 0
361500950113     C                   MOVEL     SAVLNA        BRVLNA
361600070123     C     KBRC2         SETLL     FNBRV02
361700070123     C     KBRC2         READE(N)  FNBRV02                                29
361800910910     C*
361900960205    2C     *IN29         DOWEQ     '0'
362000000111     C                   CLEAR                   FISPP000
362100910910     C*
362200960604     C                   MOVEL     BRVLNP        KLNP
362300960604     C                   MOVEL     BRVLNA        KLNA
362400960604     C                   MOVEL     BRVNRS        KNRS
362500960604     C                   MOVEL     BRVNSC        KNSC
362600960604     C*
362700960604     C                   EXSR      RICLNP
362800960604     C*
362900960604     C     WLNP          LOOKUP    L1                                     20
363000940322     C*
363100940322     C* SPUNTA PER TERMINAL
363200960205    3C     *IN08         IFEQ      *ON
363300940322     C* SPUNTA CHE TRANSITO
363400960205    4C     *IN20         IFEQ      *OFF
363500970814     C     WTAB5F        ANDEQ     ' '
363600950113     C*
363700960604     C                   SELECT
363800960611     C     WOKBT         WHENEQ    *BLANKS
363900020606     C     KBtt37        SETLL     FNBTT37L                               34
364000960611     C     WOKBT         WHENEQ    'S'
364100960604     C                   SETON                                        34
364200960604     C                   ENDSL
364300971105     C                   MOVEL     FGVLNP        SPPFLP
364400950515     C* 34 ON - USCITA DA TRANSITO
364500960205    5C     *IN34         IFEQ      *ON
364600950515     C                   MOVEL     'T'           SPPATR
364700960205   X5C                   ELSE
364800990819     C* DEFLUENZA
364900990819     C* Se filiale gestione spunta è anche il terminal di arrivo
365000990819     C* non devo trasmettere la spunta come disguido ma come defluenza
365100990819     C* (N.B.: caso di Padova che ha spuntato colli con lnp 007 ed lna
365200990819     C* 059 sul foglio viaggio locale anzichè sulla defluenza)
365300990819     C     BRVFGS        IFEQ      LNATFA
365400990819     C                   MOVEL     'F'           SPPATR
365500990819     C                   ELSE
365600940322     C* DISGUIDO
365700960604     C                   MOVEL     'D'           SPPATR
365800990819     C                   ENDIF
365900960205    5C                   ENDIF
366000950515     C*
366100960205   X4C                   ELSE
366200960205     C                   MOVEL     ' '           SPPATR
366300950515     C                   CLEAR                   SPPFLP
366400960205    4C                   ENDIF
366500940322     C*
366600940322     C                   EXSR      RIESPP
366700000111     C                   WRITE     FISPP000
366800960205    3C                   ENDIF
366900940322     C*
367000940322     C* SPUNTA ATTESA TRANSITO
367100980210    3C     *IN06         IFEQ      *ON
367200950515     C*
367300960205    4C     *IN20         IFEQ      *ON
367400970814     C     WTAB5F        OREQ      'S'
367500940322     C                   MOVEL     'A'           SPPATR
367600950215     C                   MOVEL     COMLIN        SPPFLP
367700940322     C                   EXSR      RIESPP
367800000111     C                   WRITE     FISPP000
367900960205    4C                   ENDIF
368000960205    3C                   ENDIF
368100910910     C*
368200070123     C     KBRC2         READE(N)  FNBRV02                                29
368300960205    2C                   END
368400910910     C*
368500940322     C* MI RIPOSIZIONO SULLA NUOVA LINEA ARRIVO LETTA
368600970814    2C     WLAST         IFEQ      *BLANKS
368700950113     C                   MOVEL     COMLNA        BRVLNA
368800070123     C     KBRC2         SETLL     FNBRV02
368900070123     C     KBRC2         READE     FNBRV02                                22
369000960708     C*
369100970814    3C     *IN22         IFEQ      *OFF
369200960708     C                   MOVE      'S'           WALCF
369300960708     C                   MOVEL     BRVLNP        KLNP
369400960708     C                   MOVEL     BRVLNA        KLNA
369500960708     C                   MOVEL     BRVNRS        KNRS
369600960708     C                   MOVEL     BRVNSC        KNSC
369700960708     C                   EXSR      RICLNP
369800970814    3C                   END
369900970814    2C                   END
370000021127    1C                   END
370100910910     C*
370200910910     C                   ENDSR
370300910911     C**-----------------------------------------------*
370400910911     C** CREO SPUNTE PER AUTOSPUNTA
370500910911     C**-----------------------------------------------*
370600910911     C     CRTSPU        BEGSR
370700140618     C                   CLEAR                   FNBRV05
370800950109     C                   MOVEL     BLPFLS        BRVLNP
370900950109     C                   MOVEL     BLPLNA        BRVLNA
371000950109     C                   MOVEL     BLPNRS        BRVNRS
371100950109     C                   MOVEL     BLTNSC        BRVNSC
371200950109     C                   MOVEL     BLPZNC        BRVZNC
371300950109     C                   MOVEL     '90'          BRVNPS
371400950109     C                   Z-ADD     WNPG          BRVNPG
371500000111     C                   Z-ADD     FGV2          BRVNFV
371600000111     C                   Z-ADD     FGVLNP        BRVPES
371700971105     C                   MOVEL     FGVLNP        BRVFGS
371800971107     C                   MOVEL     FGVLNP        BRVFLE
371900950109     C                   Z-ADD     BLTVUC        BRVVUC
372000990906     C                   Z-ADD     BLTPUC        BRVPUC
372100950109     C                   MOVEL     BLTCAN        BRVCAN
372200070219     C***                TIME                    W0140            14 0
372300070219     C***                MOVEL     W0140         BRVHMS
372400070219     C***                CLEAR                   WLBDAT
372500070219     C***                MOVE      W0140         G02DAT
372600070219     C***                CALL      'XSRDA8'
372700070219     C***                PARM                    WLBDAT
372800070219     C***                Z-ADD     G02INV        BRVDFS
372900070219     c                   time                    wtimez
373000070219     c* millisecondi
373100070219     c                   extrct    wtimez:*ms    millisec          6 0
373200070219     c                   movel     millisec      brvmis
373300070219     c* data immissione
373400070219     c                   movel     wtimez        dataiso
373500070219     c                   movel     dataiso       brvdfs
373600070219     c* ora immissione
373700070219     c                   move      wtimez        oraiso
373800070219     c                   movel     oraiso        brvhms
373900070219     c                   movel     'A'           brvtap
374000070219     c                   eval      brvpru='§'+ nompgm
374100910911     C*
374200140618     C                   WRITE     FNBRV05
374300910911     C                   ENDSR
374400910911     C**-----------------------------------------------*
374500910911     C** CREO RECORD ANOMALIA
374600910911     C**-----------------------------------------------*
374700910911     C     CRTANM        BEGSR
374800921022     C                   MOVEL     *BLANKS       ANMATB
374900960514     C                   MOVE      BRVFGS        ANMCDU
375000921022     C                   Z-ADD     0             ANMAAS
375100990517     C                   Z-ADD     0             ANMDUE
375200990517     C                   Z-ADD     0             ANMNPG
375300990517     C                   Z-ADD     0             ANMFGS
375400990518     C                   Z-ADD     0             ANMFL3
375500040531     C                   Z-ADD     999           ANMFL4
375600990518     C                   Z-ADD     0             ANMDTR
375700990518     C                   MOVE      *BLANK        ANMFLO
375800040603     c* data ora di creazione
375900050707     c***                z-add     *date         wdata             6 0
376000050707     c***                time                    wora              6 0
376100050707     c
376200050707     C                   TIME                    W0140            14 0
376300050707     C                   CLEAR                   WLBDAT
376400050707     C                   MOVE      W0140         G02DAT
376500050707     C                   CALL      'XSRDA8'
376600050707     C                   PARM                    WLBDAT
376700050707     C                   Z-ADD     G02INV        Wdata             6 0
376800050707     c                   movel     w0140         wora4             4 0
376900040603     c                   movel     wdata         anmflo
377000040603     c                   move      wora4         anmflo
377100040531     c
377200040531     C**   COMCAA        IFEQ      105
377300040531     c* la zona la appoggio ora in anmfl4
377400040531     C**                 CLEAR                   DANM01
377500040531     C**                 MOVE      BRVZNC        §ANZNC
377600040531     C**                 MOVEL     DANM01        ANMFLO
377700040531     C**                 ENDIF
377800040531     c                   z-add     brvznc        anmfl4
377900040531     c
378000990518     C                   MOVE      *BLANK        ANMFT3
378100040603     c
378200110909     C                   MOVE      *BLANK        ANMFT4
378300110909     C                   Z-ADD     0             ANMLNP
378400110909     C                   Z-ADD     0             ANMNSP
378500110909     c
378600110909     c* imposto dati bolle per anomalie 110 e 115
378700110909     c* flag di valore
378800110909     c                   if        comcaa=110  or comcaa=115
378900040603     c                   movel     §ar5bva       anmft4
379000110909     c                   eval      anmaas=bltaas
379100110909     c                   eval      anmlnp=bltlnp
379200110909     c                   eval      anmnsp=bltnsp
379300040603     c                   endif
379400110909
379500921022     C                   MOVEL     '1'           ANMFSA
379600950327     C                   MOVE      WDTP          ANMDAO
379700921022     C                   MOVEL     *BLANKS       ANMCCH
379800971107     C                   MOVEL     FGVLNP        ANMFLE
379900921022     C                   MOVEL     *BLANKS       ANMFT1
380000921022     C                   Z-ADD     0             ANMFL1
380100921022     C                   MOVEL     *BLANKS       ANMFT2
380200921022     C                   Z-ADD     0             ANMFL2
380300921022     C*
380400950113     C                   MOVEL     BRVLNP        ANMFLS
380500950113     C                   MOVEL     BRVLNA        ANMLNA
380600950113     C                   MOVEL     BRVNRS        ANMNRS
380700950113     C                   MOVEL     BRVNSC        ANMSCN
380800950113     C                   MOVEL     BRVNPS        ANMNPS
380900950113     C                   Z-ADD     BRVNFV        ANMNFV
381000911018     C                   Z-ADD     COMCAA        ANMCAA
381100911018     C                   MOVEL     '7C'          COD
381200911018     C                   MOVEL     *BLANKS       KEY
381300911018     C                   MOVEL     COMCAA        KEY
381400911018     C     KTAB          CHAIN     TABEL                              21
381500911018     C  N21              MOVEL     TBLUNI        DS7C
381600911018     C   21              MOVEL     *BLANKS       DS7C
381700911018     C*
381800911018     C                   MOVEL     §7CTAN        ANMTAN
381900911018     C                   MOVEL     §7CSAN        ANMSAN
382000911018     C                   MOVEL     §7CAIE        ANMAIE
382100911018     C                   Z-ADD     §7CLID        ANMLID
382200910911     C                   Z-ADD     0             ANMDCH
382300911011     C                   MOVEL     *BLANKS       ANMFSC
382400911018     C     §7CCHA        IFEQ      'S'
382500950327     C                   MOVE      WDTP          ANMDCH
382600911018     C                   MOVEL     '1'           ANMFSC
382700911018     C                   END
382800910911     C*
382900990517     C                   WRITE     FNANM
383000910911     C                   ENDSR
383100910911     C**-----------------------------------------------*
383200910911     C** CHIUSURA ANOMALIE MANCA BOLLA
383300910911     C**-----------------------------------------------*
383400910911     C     CHIUAN        BEGSR
383500050927     C                   CLEAR                   WESISTEANM        1
383600990517     C     KANM3         CHAIN(N)  FNANM                              21
383700960416     C     *IN21         IFEQ      *OFF
383800130607     c                   exsr      call_lr33
383900050927     C                   eval      wesisteanm='S'
384000050927     C                   END
384100921014     C*
384200910911     C                   ENDSR
384300130607     C**-----------------------------------------------*
384400130607     C** chiamata a FNLR33R
384500130607     C**-----------------------------------------------*
384600130607     C     call_lr33     BEGSR
384700130607     C                   CLEAR                   DSLR33
384800130607     C                   MOVE      WDTP          D33DCH
384900130607     C                   MOVEL     '1'           D33FSC
385000130607     C                   MOVEL     '  '          D33CCH
385100130607     c                   if        comcaa=110  or comcaa=105
385200130607     C                   MOVEL     'BO'          D33CCH
385300130607     c                   endif
385400130607     C                   MOVE      ANMNR2        D33NRR
385500130607     C                   CALL      'FNLR33R'
385600130607     C                   PARM                    DSLR33
385700130607     c                   ENDSR
385800920324     C**-----------------------------------------------*
385900920324     C** ALLOCO MEMBRI DI TRASMISSIONE
386000920324     C**-----------------------------------------------*
386100920324     C     ALLOCO        BEGSR
386200950109     C* CARICO FILIALI ELABORATORE LINE FV
386300920324     C                   EXSR      RIEMPI
386400920324     C* ALLOCO MEMBRI DELLE LINEE DI ARRIVO
386500920324     C                   EXSR      LCKMBR
386600920324     C* ALLOCO MEMBRI LINEE DI TRANSITO
386700950915     C  N50              EXSR      LCKTRN
386800920526     C* NON RIESCO AL ALLOCARE: RISOTTOMETTO 3 VOLTE
386900950915     C   50              EXSR      ERROR
387000920324     C*
387100920324     C                   ENDSR
387200920324     C**-----------------------------------------------*
387300971105     C** CARICO TERMINAL DI PARTENZA DELLE LINEE DEL FV
387400920324     C**-----------------------------------------------*
387500920324     C     RIEMPI        BEGSR
387600920324     C*
387700921202     C                   Z-ADD     1             K                 3 0
387800920324     C                   Z-ADD     1             I                 3 0
387900920324     C                   Z-ADD     1             Y                 3 0
388000920324     C*
388100921202     C     FFV(I)        DOWGT     *ZEROS
388200920324     C                   MOVEL     FFV(I)        SAVFFV            3 0
388300971104     C*
388400971104     C                   CLEAR                   DSLV55
388500971104     C                   MOVEL     'P'           D55TPT
388600971104     C                   Z-ADD     SAVFFV        D55LIN
388700971105     C                   Z-ADD     FGVDFV        D55DRF
388800971104     C                   CALL      'FNLV55R'
388900971104     C                   PARM                    DSLV55
389000971104     C     D55TFP        LOOKUP    FEL                                    30
389100971104     C  N30              Z-ADD     D55TFP        FEL(K)
389200920324     C  N30              ADD       1             K
389300920324     C* FILIALI DI TRANSITO
389400920324     C     FLP(I)        IFGT      *ZEROS
389500920324     C                   MOVEL     FLP(I)        SAVFLP            3 0
389600920324     C* C'E' TRANSITO SE NON E' LA FIL DI ARRIVO
389700920324     C*                         LA SUA FILIALE ELABORATORE
389800930312     C*   SE E' IL SUO TERMINAL ALLOCO LO STESSO
389900920324     C     SAVFFV        IFNE      SAVFLP
390000960325     C*
390100971105     C     D55TFP        ANDNE     SAVFLP
390200930312     C*
390300971105     C                   CLEAR                   DSLV55
390400971105     C                   MOVEL     'P'           D55TPT
390500971105     C                   Z-ADD     SAVFLP        D55LIN
390600971105     C                   Z-ADD     FGVDFV        D55DRF
390700971105     C                   CALL      'FNLV55R'
390800971105     C                   PARM                    DSLV55
390900950215     C*
391000971105     C* SE IL TERMINAL DI PARTENZA NON E' FGVLNP
391100971105     C     D55TFP        IFNE      FGVLNP
391200971105     C     D55TFP        LOOKUP    FET                                    30
391300971105     C  N30              Z-ADD     D55TFP        FET(Y)
391400920324     C  N30              ADD       1             Y
391500920324     C                   END
391600950215     C                   END
391700920324     C                   END
391800921202     C*
391900921202     C                   ADD       1             I
392000920324     C                   END
392100050524     c
392200050524     c* Controllo riempimento schiera
392300050524     c                   clear                   trul0sds
392400050524     c                   eval      i0sski='FEL'
392500050524     c                   eval      i0sele=%elem(fel)
392600050524     c                   eval      i0spie=k-1
392700050524     c                   eval      i0sfile='TerPar LNA'
392800050524     c                   eval      i0ssif=knsif
392900130718     c                   eval      i0spgm='FNLSU2R'
393000050524     c                   movel     trul0sds      kpjbu
393100050524     c                   call      'TRUL0SR'
393200050524     c                   parm                    kpjba
393300050524     c
393400050524     c* Controllo riempimento schiera
393500050524     c                   clear                   trul0sds
393600050524     c                   eval      i0sski='FET'
393700050524     c                   eval      i0sele=%elem(fet)
393800050524     c                   eval      i0spie=y
393900050524     c                   eval      i0sfile='TerPar FLP'
394000050524     c                   eval      i0ssif=knsif
394100130718     c                   eval      i0spgm='FNLSU2R'
394200050524     c                   movel     trul0sds      kpjbu
394300050524     c                   call      'TRUL0SR'
394400050524     c                   parm                    kpjba
394500920324     C                   ENDSR
394600920324     C**-----------------------------------------------*
394700920324     C** ESEGUE CALL QCAEXEC SUL MEMBRO DELLE BOLLE    *
394800920324     C**-----------------------------------------------*
394900920324     C     CRTMBR        BEGSR
395000930127     C*
395100930127     C                   CALL      'TRUL50C'
395200930127     C                   PARM                    FISICO           10
395300930127     C                   PARM                    FISMBR           10
395400000428     C                   PARM                    O50PLA
395500930127     C                   PARM                    LOGICO           10
395600930127     C                   PARM                    LOGMBR           10
395700930127     C                   PARM                    ULFLG             1
395800930127     C* RITENTO L'ALLOCAZIONE
395900950915     C                   CALL      'QCMDEXC'                            50
396000930127     C                   PARM                    COMMAN
396100930127     C                   PARM                    LUNG
396200930127     C*
396300920324     C                   ENDSR
396400920324     C**-----------------------------------------------*
396500920324     C** ALLOCAZIONE MEMBRI
396600920324     C**-----------------------------------------------*
396700920324     C     LCKMBR        BEGSR
396800920324     C                   Z-ADD     1             K
396900950915     C                   SETOFF                                       50
397000021014     C                   MOVE      '))'          VAR2
397100000725     C                   MOVEL     'M'           VAR2
397200000731     C                   MOVEL     'M'           EMME
397300021014     C                   Z-ADD     42            LUNG
397400920324     C*
397500920324     C     FEL(K)        DOWNE     0
397600950109     C* ALLOCO MEMBRO FNBLP
397700000725     C                   MOVEL     FEL(K)        VAR2X
397800930127     C                   MOVEL     FEL(K)        NUM3
397900920324     C**
398000920324     C** SE NON CI SONO BOLLE DA INVIARE E' INUTILE ALLOCARE
398100950115     C* ALLOCAZIONE FILE DELLE BOLLE
398200950115     C   09              EXSR      LCKBLP
398300920324     C**
398400971105     C* ALLOCO SOLO SE IL MEMBRO DA SCRIVERE NON è LOCALE COME TERMINAL
398500971105     C* DI PARTENZA
398600040331    1C  N50              DO
398700960124     C* SE VENGO DA MANUTENZIONE FV NON DEVO ALLOCARE IL FILE SPUNTE
398800960124    2C     PA2FLG        IFNE      'M'
398900960124     C**
399000000111     C                   MOVE      VAR2          CM47
399100920324     C                   MOVEL     *BLANKS       COMMAN           80
399200921202     C                   MOVEA     CM47(1)       COMMAN
399300950915     C                   CALL      'QCMDEXC'                            50
399400920324     C                   PARM                    COMMAN
399500920324     C                   PARM                    LUNG
399600930127     C*
399700930127     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
399800930127     C*  SE MANCA E RITENTO
399900960124    3C     *IN50         IFEQ      '1'
400000000111     C                   MOVEL     'FISPP00T'    FISICO
400100930127     C                   EXSR      CRTMBR
400200960124    3C                   END
400300960124    2C                   ENDIF
400400920324     C**
400500950915    2C  N50              DO
400600951116     C                   MOVE      VAR2          CM49
400700920324     C                   MOVEL     *BLANKS       COMMAN           80
400800921202     C                   MOVEA     CM49(1)       COMMAN
400900950915     C                   CALL      'QCMDEXC'                            50
401000920324     C                   PARM                    COMMAN
401100920324     C                   PARM                    LUNG
401200930127     C*
401300930127     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
401400930127     C*  SE MANCA E RITENTO
401500950915    3C     *IN50         IFEQ      '1'
401600951116     C                   MOVEL     'FNFVA01T'    FISICO
401700930127     C                   EXSR      CRTMBR
401800950115    3C                   END
401900960123     C*
402000960123    3C  N50              DO
402100000428     C                   MOVE      VAR2          CM48
402200000428     C                   MOVEL     *BLANKS       COMMAN           80
402300000428     C                   MOVEA     CM48(1)       COMMAN
402400000428     C                   CALL      'QCMDEXC'                            50
402500000428     C                   PARM                    COMMAN
402600000428     C                   PARM                    LUNG
402700000428     C*
402800000428     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
402900000428     C*  SE MANCA E RITENTO
403000000428    5C     *IN50         IFEQ      '1'
403100000428     C                   MOVEL     'FNFWA00T'    FISICO
403200000428     C                   EXSR      CRTMBR
403300000428    5C                   END
403400960123    3C                   END
403500960123     C*
403600950115    2C                   END
403700950115    1C                   END
403800950115     C**
403900950915     C  N50              ADD       1             K                 3 0
404000950915     C   50              Z-ADD     149           K
404100920324     C                   END
404200920324     C*
404300920324     C                   ENDSR
404400950115     C*
404500950115     C*--------------------------------------
404600950115     C* ALLOCAZIONE FILE DELLE BOLLE
404700950115     C*--------------------------------------
404800950115     C     LCKBLP        BEGSR
404900950115     C*
405000971104     C                   MOVE      VAR2          CM41
405100950115     C                   MOVEL     *BLANKS       COMMAN           80
405200971104     C                   MOVEA     CM41(1)       COMMAN
405300950915     C                   CALL      'QCMDEXC'                            50
405400950115     C                   PARM                    COMMAN
405500950115     C                   PARM                    LUNG
405600950115     C*
405700950115     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
405800030107     C*  SE MANCA E RITENTO
405900950915    1C     *IN50         IFEQ      '1'
406000971104     C                   MOVEL     'FNBLPE0T'    FISICO
406100950115     C                   EXSR      CRTMBR
406200950115    1C                   END
406300950115     C*
406400950115     C                   ENDSR
406500920324     C**-----------------------------------------------*
406600920324     C** ALLOCAZIONE MEMBRI  TRASMISSIONE
406700920324     C**-----------------------------------------------*
406800920324     C     LCKTRN        BEGSR
406900920324     C                   Z-ADD     1             K
407000950915     C                   SETOFF                                       50
407100920324     C     FET(K)        DOWNE     0
407200930310     C*
407300000725     C                   MOVEL     FET(K)        VAR2X
407400930310     C*
407500930310     C                   EXSR      TRANSI
407600930310     C*
407700950915     C  N50              ADD       1             K                 3 0
407800950915     C   50              Z-ADD     149           K
407900920324     C                   END
408000920324     C*
408100920324     C                   ENDSR
408200930310     C**-----------------------------------------------*
408300080423     C** ALLOCAZIONE FILE DEL TRANSITO
408400930310     C**-----------------------------------------------*
408500930310     C     TRANSI        BEGSR
408600000725     C                   MOVEL     VAR2X         NUM3
408700950109     C                   MOVE      VAR2          CM51
408800930310     C                   MOVEL     *BLANKS       COMMAN           80
408900930310     C                   MOVEA     CM51(1)       COMMAN
409000950915     C                   CALL      'QCMDEXC'                            50
409100930310     C                   PARM                    COMMAN
409200930310     C                   PARM                    LUNG
409300930310     C*
409400930310     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
409500930310     C*  SE MANCA E RITENTO
409600950915    1C     *IN50         IFEQ      '1'
409700950109     C                   MOVEL     'FNBLPT0T'    FISICO
409800930310     C                   EXSR      CRTMBR
409900950215    1C                   END
410000930310     C*
410100950915    1C  N50              DO
410200000111     C                   MOVE      VAR2          CM47
410300950215     C                   MOVEL     *BLANKS       COMMAN           80
410400950215     C                   MOVEA     CM47(1)       COMMAN
410500950915     C                   CALL      'QCMDEXC'                            50
410600950215     C                   PARM                    COMMAN
410700950215     C                   PARM                    LUNG
410800950215     C*
410900950215     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
411000950215     C*  SE MANCA E RITENTO
411100950915    3C     *IN50         IFEQ      '1'
411200000111     C                   MOVEL     'FISPP00T'    FISICO
411300950215     C                   EXSR      CRTMBR
411400950215    3C                   END
411500950215     C**
411600950915    3C  N50              DO
411700951116     C                   MOVE      VAR2          CM49
411800950215     C                   MOVEL     *BLANKS       COMMAN           80
411900950215     C                   MOVEA     CM49(1)       COMMAN
412000950915     C                   CALL      'QCMDEXC'                            50
412100950215     C                   PARM                    COMMAN
412200950215     C                   PARM                    LUNG
412300950215     C*
412400950215     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
412500950215     C*  SE MANCA E RITENTO
412600950915    4C     *IN50         IFEQ      '1'
412700951116     C                   MOVEL     'FNFVA01T'    FISICO
412800950215     C                   EXSR      CRTMBR
412900950215    4C                   END
413000950215     C**
413100960123    4C  N50              DO
413200000428     C**
413300000428     C                   MOVE      VAR2          CM48
413400000428     C                   MOVEL     *BLANKS       COMMAN           80
413500000428     C                   MOVEA     CM48(1)       COMMAN
413600000428     C                   CALL      'QCMDEXC'                            50
413700000428     C                   PARM                    COMMAN
413800000428     C                   PARM                    LUNG
413900000428     C*
414000000428     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
414100000428     C*  SE MANCA E RITENTO
414200000428    6C     *IN50         IFEQ      '1'
414300000428     C                   MOVEL     'FNFWA00T'    FISICO
414400000428     C                   EXSR      CRTMBR
414500000428    6C                   END
414600960123    4C                   END
414700960123     C*
414800950215    3C                   END
414900950215    1C                   END
415000930310     C                   ENDSR
415100920526     C*
415200920526     C* RISOTTOMETTO AZIONE PERCHE' NON RIESCO AD ALLOCARE -----------*
415300920526     C     ERROR         BEGSR
415400111019     C     NUM2          IFEQ      2
415500960129     C*
415600960129     C* SE VENGO DA MANUTENZIONE FV NON DEVO STAMPARE
415700111026     C***  PA2FLG        IFNE      'M'
415800111026     C***                OPEN      SYSPRT
415900111026     C***                EXCEPT    TESERR
416000111026     C***                EXCEPT    DETERR
416100111026     C***                CLOSE     SYSPRT
416200111026     C***                ENDIF
416300960129     C*
416400111019     C* SE NON RIESCO AD ALLOCARE INVIO mail a sede
416500111019     C                   movel     *blanks       wrkEml          253
416600111019     C                   movel     *blanks       wrkMsg         5000
416700111019     C                   movel     *blanks       wrkOgg           44
416800111019     c
416900111019     C                   MOVEL     MSG(1)        wrkogg
417000111019     C                   MOVEL(P)  MSG(2)        wrkmsg
417100111019     c                   eval      %subst(wrkmsg:41:3)=%editc(fgvlnp:'X')
417200111019     c                   eval      %subst(wrkmsg:45:6)=%editc(fgvnfv:'2')
417300120302     c                   eval      wrkeml='ced@brt.it'
417400111019     c
417500111019     C                   call(e)   'TIS701C'
417600111019     C                   parm                    wrkEml
417700111019     C                   parm                    wrkOgg
417800111019     C                   parm                    wrkMsg
417900920526     C*
418000920526     C                   ELSE
418100920526     C*
418200920526     C* RISOTTOMETTO L'AZIONE
418300920526     C                   ADD       1             NUM2
418400130524     C                   MOVEL     FNLSU2DS      KPJBU
418500130718     C                   MOVEL     'LSU2'        KCOAZ
418600920526     C                   CALL      'BCH10'
418700920526     C                   PARM                    KPJBA
418800920526     C* E RISOTTOMETTO AZIONE DI INVIO FILE
418900950130     C                   MOVEL     'LSA3'        KCOAZ
419000070423     c                   clear                   parama3
419100070423     c                   move      simfel        pa3tfp
419200070423     c                   movel(p)  parama3       kpjbu
419300931203     C                   CALL      'BCH10'
419400931203     C                   PARM                    KPJBA
419500920526     C                   END
419600920526     C                   ENDSR
419700930127     C**-----------------------------------------------*
419800000111     C** RIEMPO CAMPI DEL FILE FISPP00T
419900930127     C**-----------------------------------------------*
420000930127     C     INVSAI        BEGSR
420100950113     C                   MOVEL     '1'           SPPNPG
420200950113     C                   MOVEL     BRVLNP        SPPLNP
420300950113     C                   MOVEL     BRVLNA        SPPLNA
420400950113     C                   MOVEL     BRVNRS        SPPNRS
420500950113     C                   MOVEL     BRVNSC        SPPNSC
420600950113     C                   MOVEL     BRVNPS        SPPNPS
420700950113     C                   MOVEL     WDTP          SPPDFV
420800950113     C                   MOVEL     'T'           SPPFLG
420900000111     C                   WRITE     FISPP000
421000930127     C                   ENDSR
421100991019     C*
421200040311     C*--- ELABORAZIONE SPUNTE PARTENZA ------------------------------*
421300991019     C     SPUPAR        BEGSR
421400991019     C**
421500991019     C**   VEDO SE DEVO IMPOSTARVI PESO E/O VOLUME
421600000620     C                   CLEAR                   SAVSPE
421700130227     C                   SETOFF                                       45
421800991019     C*
421900040121     c* Invece che testare se ka cml/vdl, testo il flag se in sto terminal
422000040121     c*   si usa il peso  e/o volume sulle spunte
422100040609     c     pa2spv        ifeq      'S'
422200040609     c     pa2spp        oreq      'S'
422300991019     C* LETTO FV ENTRATA, LEGGO LE SUE SPUNTE
422400991019     C                   MOVEL     1             KVVNPG
422500000111     C                   Z-ADD     FGV2          KVVNFV
422600991019     C                   MOVEL     SIMFEL        KVVFGS
422700070123     C     KBRV32        SETLL     FNBRV02
422800070123     C     KBRV32        READE     FNBRV02                                32
422900991019     C*
423000991019    2C     *IN32         DOWEQ     *OFF
423100991019     C* ESCLUDO LE SPUNTE GIA' TRASMESSE
423200991019    3C     BRVFTR        IFEQ      ' '
423300000620     C*
423400040121    4c                   if        pa2spv='S' and brvvuc=0  or
423500040121     c                             pa2spp='S' and brvpuc=0
423600040121     c
423700991019     C                   MOVEL     DSBRV         SVBRVE
423800991019     C* IMPOSTO LA KEY PER CHAIN
423900991019     C                   Z-ADD     BRVVUC        WRVVUC
424000991019     C                   Z-ADD     BRVPUC        WRVPUC
424100991019     C                   EXSR      IMPKEY
424200991019     C*
424300070123     C     KBRV6         SETLL     FNBRV07L
424400070123     C     KBRV6         READE     FNBRV07L                               34
424500991019    5C     *IN34         DOWEQ     *OFF
424600991019     C**
424700120604     C* VEDO SE SPUNTA CON VOLUME o peso
424800991019    6C     WRVVUC        IFEQ      0
424900991019     C     BRVVUC        ANDGT     0
425000991019     C                   Z-ADD     BRVVUC        WRVVUC
425100991019    6C                   ENDIF
425200991019     C* VEDO SE SPUNTA CON PESO CML
425300991019    6C     WRVPUC        IFEQ      0
425400991019     C     BRVPUC        ANDGT     0
425500991019     C                   Z-ADD     BRVPUC        WRVPUC
425600991019    6C                   ENDIF
425700991019     C*
425800070123     C     KBRV6         READE     FNBRV07L                               34
425900991019    5C                   ENDDO
426000991019     C*
426100991019     C                   MOVEL     SVBRVE        DSBRV
426200991019     C                   Z-ADD     WRVVUC        BRVVUC
426300991019     C                   Z-ADD     WRVPUC        BRVPUC
426400991019     C* AGGIORNO SPUNTA PARTENZA
426500070123     C                   UPDATE    FNBRV02
426600991019    4C                   ENDIF
426700991019    3C                   ENDIF
426800991019     C*
426900070123     C     KBRV32        READE     FNBRV02                                32
427000991019    2C                   ENDDO
427100040609    1C                   ENDIF
427200991019     C**
427300991019     C                   ENDSR
427400940414     C*
427500940414     C*--- ELABORAZIONE ABBINAMENTO ----------------------------------*
427600040625     c     ABBIN         BEGSR
427700940418     C* APERTURA FILES
427800021021     C                   OPEN      FNANM20L
427900021021     C                   OPEN      FNBLT38L
428000020605     C                   OPEN      FNBTT38L
428100020916     C                   OPEN      FNFVV02L
428200001120     C                   OPEN      FNFVV01L
428300041217     c                   clear                   wcalLsB7          1
428400940418     C*
428500940415     C* PRENDO FASE ANOMALIA
428600940415     C                   CALL      'TRUL11R'
428700940415     C                   PARM                    PARAM9
428800940414     C*
428900940414     C                   CLEAR                   LFV
429000940414     C                   MOVEA     FFV           LFV
429100940414     C                   Z-ADD     1             I
429200940414     C*
429300960209     C     PA2AGS        IFEQ      'S'
429400940414     C                   SETON                                        44
429500940414     C                   END
429600990917     C**
429700990917     C** SPUNTE DEL FOGLIO VIAGGIO PARTENZA
429800991019     C**   VEDO SE DEVO IMPOSTARVI PESO E/O VOLUME
429900991019     C                   EXSR      SPUPAR
430000000620     C*
430100000620     C                   CLEAR                   SAVSPE
430200940414     C*
430300940414     C* LEGGO TUTTE LE LINEE
430400950113    1C     LFV(I)        DOWGT     *ZEROS
430500940414     C                   MOVEL     LFV(I)        COMFIL
430600021021     c*
430700021021     c* per ogni linea di arrivo loopo sulle linee di partenza della L1
430800021021     c                   z-add     1             IL
430900021021    2c                   dow       l1(IL)>0
431000021021     c
431100021021     c                   eval      wlnp=l1(IL)
431200990917     C**
431300990917     C** BOLLE  PARTENZA
431400990917     C**
431500950915     C                   SETON                                        40
431600021021     C     KBLT          SETLL     FNBLT038
431700021021     C     KBLT          READE     FNBLT038                               32
431800940414     C*
431900021021    4C     *IN32         DOWEQ     '0'
432000000619     C*
432100970506     C* VEDO SE DEVO AGGIORNARE LA DATA DI ENTRATA
432200970506     C                   EXSR      AGGENT
432300970506     C**
432400950109     C* SE COLLI NON ANCORA PARTITI CERCO SOLO QUELLI BORDERIZZATI SUL
432500990303     C*  PER IL RESTO LI PRENDO TUTTI(DEVONO AVERE IL NUMERO FOGLIO
432600990303     C*  PIENO:ATTENZIONE ALLA "C" DELL'ESTERO)
432700000714     C*
432800130530    5C**** BLTNFV        IFEQ      FGV2
432900130530     C**** BLTFTR        ANDEQ     *BLANKS
433000990303     C*
433100130530     C     BLTFTR        ifeq      'C'
433200990303     C     BLTNFV        ANDGT     0
433300990303     C*
433400990303     C     BLTFTR        ORNE      *BLANKS
433500990303     C     BLTFTR        ANDNE     'C'
433600151126     c* Se parzializzo per KSC solo bolle di questo FGV
433700151126     c                   if        pa2ccm<=*zeros or bltnfv=fgv2
433800940414     C                   EXSR      CRTBRC
433900151126     c                   endif
434000151126     c
434100021021    5C                   ENDIF
434200000714     C**
434300021021     C     KBLT          READE     FNBLT038                               32
434400021021    4C                   ENDDO
434500021021     c
434600021021     c                   eval      IL=il+1
434700021021    2c                   enddo
434800021021     c
434900950915     C                   SETOFF                                       40
435000940414     C**
435100021025     C** BOLLE TRANSITO
435200940414     C**
435300151126     c                   if        pa2ccm<=*zeros
435400151126     C                   SETON                                        41
435500020605     C     KBtt38        SETLL     FNBTT38L
435600020605     C     KBtt38        READE     FNBTT38L                               32
435700940414     C*
435800950113    2C     *IN32         DOWEQ     '0'
435900080228     c* ignoro transito se bttlnp fa parte della £1 (può succedere x le
436000080228     c* trazioni ridotte quando alcuni 2 livello cambiano terminal di part)
436100080228     c     bttlnp        lookup    l1                                     90
436200080228     c                   if        not *in90
436300970506     C* VEDO SE DEVO AGGIORNARE LA DATA DI ENTRATA
436400970506     C                   EXSR      AGGENT
436500040422     c* se presente anomalia 200 dell'area non autogenero la spunta
436600040422     c* partenza perchè significa che il collo si trova presso un
436700040422     c* secondo livello in partenza di Simfel e quindi non posso
436800040422     c* dare per scontato che riparta
436900040422     c                   eval      wcrtspu='S'
437000040422     c                   z-add     200           comcaa
437100040503     c     kanm02        chain(N)  fnanm02l
437200040422     c                   if        %found(fnanm02l)
437300040422     c                             and anmdch = 0
437400040422     c     anmfle        lookup    l1                                     32
437500040422     c   32              eval      wcrtspu='N'
437600040422     c                   endif
437700970506     C**
437800040422     c     wcrtspu       ifeq      'S'
437900950109     C                   EXSR      CRTBRC
438000040422     c                   endif
438100080228
438200080228     c                   endif
438300940414     C*
438400020605     C     KBtt38        READE     FNBTT38L                               32
438500950113    2C                   ENDDO
438600940414     C                   SETOFF                                       41
438700151126     c                   endif
438800041217     c
438900041217     c* Eseguo call al pgm di calcolo volume e pesi
439000041217     c                   if        wcalLSB7='S'
439100041217     C                   Z-ADD     FGVLNP        PA1FEL
439200041217     C                   MOVEL     PARAM1        KPJBU
439300041217     C                   CALL      'FNLSB7R'
439400041217     C                   PARM                    KPJBA
439500041217     c
439600041217     c                   clear                   wcalLSB7
439700041217     c                   endif
439800940414     C**
439900970828     C** ANOMALIE DI DISGUIDO
440000940414     C**
440100000619     C     WFVPT         IFEQ      'N'
440200151126     c     pa2ccm        andle     *zeros
440300021021     C                   SETON                                        42
440400040115     c* Devo pulire questo campo perchè altrimenti potrebbe rimanere sporco
440500040115     c* e sbagliare la pistola in autogenerazione spunte partenza
440600040115     c                   clear                   wlocal
440700021021     c* loop per anmfle = a tutti i p.o. della £1
440800021021     C                   Z-ADD     1             IL                3 0
440900021021   1ac                   dow       l1(IL)>0
441000021021     c
441100021025     c                   eval      wlnp=l1(il)
441200940414     C                   Z-ADD     200           COMCAA
441300021021     C     KANM          SETLL     FNANM20
441400021021     C     KANM          READE     FNANM20                                32
441500940414     C*
441600950113    2C     *IN32         DOWEQ     '0'
441700940414     C* SE NON E' GIA' CHIUSA
441800950113    3C     ANMDCH        IFEQ      0
441900990517     C     WDTP          ANDGE     ANMDAO
442000960705     C*
442100020412     C                   CLEAR                   WCRT
442200960705     C* SE TROVO BOLLA PARTENZA CHIUDO ANOMALIA E NON AUTOGENERO SPUNTA
442300960705     C* (SE BOLLA DA ME CONFERMABILE E CONFERMABILE ANCHE DA ALTRI AS)
442400970828     C**
442500970828     C* ATTUALMENTE NON CREIAMO PIU' ANOMALIE DI DISGUIDO SE COLLO DA
442600970828     C*  ME CONFERMABILE ANCHE SE NON C'E' LA BOLLA PARTENZA PER CUI
442700970828     C*  NON HO PIU' BISOGNO DI CERCE LE ANOMALIE DISGUIDO
442800020412     c* 12/04/02: Ripristinata la routine srdisg in quanto se la 5f
442900020412     c* è stata inserita dopo il caricamento spunte il disguido può
443000020412     c* essere stato creato se al momento dello scarico spunte la
443100020412     c* bolla non era ancora stata confermata
443200020412    4C     anmnrs        ifgt      *zeros
443300020412     C                   exsr      srdisg
443400020412    4C                   endif
443500040406     c* disattivo autogenerazione spunte da anomalie di disguido: si voglion
443600040406     c* far partire i colli disguidati solo se spuntati in entrata o in par-
443700040406     c* tenza
443800040406     c                   eval      wcrt='N'
443900960705     C*
444000020412   3AC     wcrt          ifeq      ' '
444100070123     C     KANM5         CHAIN     FNBRV07                            31
444200950118    4C     *IN31         IFEQ      *OFF
444300950109     C* MI SALVO I CAMPI CHE MI SERVONO PER L'AGGIORNAMENTO
444400950109     C                   Z-ADD     BRVZNC        WRVZNC
444500950109     C                   Z-ADD     BRVVUC        WRVVUC
444600990906     C                   Z-ADD     BRVPUC        WRVPUC
444700980206     C     BRVCAN        IFNE      'E'
444800950113     C                   MOVEL     BRVCAN        WRVCAN
444900980206     C                   END
445000970506     C* IMPOSTO LA KEY PER CHAIN
445100970506     C                   EXSR      IMPKEY
445200970506     C* CREO SPUNTA PARTENZA
445300950109     C                   EXSR      CRTBRC
445400950113    4C                   END
445500020412   3AC                   endif
445600950113    3C                   END
445700940414     C*
445800021021     C     KANM          READE     FNANM20                                32
445900950113    2C                   ENDDO
446000021021     c                   eval      il=il+1
446100021021   1aC                   ENDDO
446200940414     C                   SETOFF                                       42
446300040624    1C                   ENDIF
446400940414     C**
446500960209     C** LEGGO LE SPUNTE ENTRATA PER CREARE EVENTUALI
446600950915     C** SPUNTE PARTENZA NON ANCORA CREATE
446700950915     C**
446800950915     C     *IN44         IFEQ      *ON
446900151126     c     pa2ccm        andle     *zeros
447000151126     c
447100950915     C                   SETON                                        43
447200001121     C*
447300950915     C                   Z-ADD     5             WNPG5
447400950915     C                   Z-ADD     DFV1          WDFV
447500020916      * ciclo all'interno della £1
447600020917do   c                   do        30            xx
447700020916     c                   If        L1(xx) = *Zeros
447800020916     c                   Leave
447900020916     c                   EndIf
448000020917     c     L1(xx)        Lookup    Spt                                    37
448100040802if   c     *IN37         IFEQ      *ON
448200040802     C     WFVPT         ANDEQ     'S'
448300040802     C*
448400040802     C     *IN37         OREQ      *OFF
448500040802     C     WFVPT         ANDEQ     'N'
448600040802     c*
448700040802if   c     *IN37         orEQ      *ON
448800040802     C     WFVPT         ANDEQ     'N'
448900040802     C
449000020916     c                   Move      L1(xx)        kfgs
449100020916     C     KFVV          SETLL     FNFVV02L
449200020916     C     KFVV          READE     FNFVV02L                               32
449300950915     C*
449400950915     C     *IN32         DOWEQ     *OFF
449500950915     C* LETTO FV ENTRATA, LEGGO LE SUE SPUNTE
449600970423     C                   MOVEL     FVVNPG        KVVNPG
449700000111     C                   Z-ADD     FVVNFV        KVVNFV
449800970423     C                   MOVEL     FVVFGS        KVVFGS
449900070123     C     KBRV33        SETLL     FNBRV02
450000070123     C     KBRV33        READE(N)  FNBRV02                                32
450100950915     C*
450200950915    2C     *IN32         DOWEQ     *OFF
450300960521     C* ESCLUDO LE SPUNTE CHE HANNO FLAG TRASMISSIONE 'N'
450400960521     C     BRVFTR        IFNE      'N'
450500960521     C     BRVDTR        ANDNE     *HIVAL
450600040422     c* se collo spuntato da un secondo livello in partenza non
450700040422     c* autogenero spunta partenza se presente anomalia 200 aperta
450800040422     c* creata da p.o. della £1 perchè significa che il collo è in
450900040422     c* disguido e non può ripartire se non spuntato in entrata o in
451000040422     c* partenza dal terminal di partenza
451100040422     c                   eval      wcrtspu='S'
451200040422     c     brvfgs        ifne      simfel
451300040422     c                   z-add     200           comcaa
451400040503     c     kanm3         chain(N)  fnanm02l
451500040422     c                   if        %found(fnanm02l)
451600040422     c                             and anmdch=0
451700040422     c     anmfle        lookup    l1                                     32
451800040422     c   32              eval      wcrtspu='N'
451900040422     c                   endif
452000040422     c                   endif
452100040422     c     wcrtspu       ifeq      'S'
452200950915     C                   Z-ADD     BRVZNC        WRVZNC
452300950915     C                   Z-ADD     BRVVUC        WRVVUC
452400990906     C                   Z-ADD     BRVPUC        WRVPUC
452500950915     C                   MOVEL     BRVCAN        WRVCAN
452600950915     C                   MOVEL     BRVNPS        WRVNPS
452700970506     C* IMPOSTO LA KEY PER CHAIN
452800970506     C                   EXSR      IMPKEY
452900970506     C* CREO SPUNTA PARTENZA
453000950915     C                   EXSR      CRTBRC
453100040422     c                   endif
453200960521     C                   ENDIF
453300950915     C*
453400070123     C     KBRV33        READE(N)  FNBRV02                                32
453500950915     C                   ENDDO
453600950915     C*
453700020916     C     KFVV          READE     FNFVV02L                               32
453800950915     C                   ENDDO
453900030902      *
454000020917if x c                   Else
454100020917     c                   Iter
454200020917if e c                   EndIf
454300030902      *
454400020917do e c                   EndDo
454500950915     C*
454600950915     C                   SETOFF                                       43
454700950915     C                   ENDIF
454800950915     C**
454900950915     C**CERCO SPUNTE IMP
455000950915     C**
455100960604     C                   MOVE      'N'           WALCF             1
455200960604     C*
455300070123     C     KBRV3         SETLL     FNBRV02
455400070123     C     KBRV3         READE(N)  FNBRV02                                32
455500940414     C*
455600950113    2C     *IN32         DOWEQ     *OFF
455700950109     C                   MOVEL     BRVLNP        KLNP
455800950109     C                   MOVEL     BRVLNA        KLNA
455900950109     C                   MOVEL     BRVNRS        KNRS
456000950113     C                   MOVEL     BRVNSC        KNSC
456100950109     C                   Z-ADD     1             WNPG
456200940414     C*
456300140618     C     KBRV5         SETLL     FNBRV05
456400140618     C     KBRV5         READE     FNBRV05                                33
456500940414     C*
456600940414     C* SEGNACOLLO TROVATO: ELIMINO SE NON PARTITO
456700000619    3C     *IN33         DOWEQ     *OFF
456800940414     C*
456900950113    4C     BRVDTR        IFEQ      0
457000980728     C* CANCELLO ANCHE SE FLAGGGATA CON "L" PERCHE' NON E' TRASMESSA MA
457100980728     C*  NON UTILIZZABILE
457200980728     C     BRVFTR        OREQ      'L'
457300140618     C                   DELETE    FNBRV05
457400950113   X4C                   ELSE
457500140618     C                   UNLOCK    FNBRV05L
457600961212    4C                   ENDIF
457700140618     C     KBRV5         READE     FNBRV05                                33
457800000619    3C                   ENDDO
457900940414     C*
458000070123     C     KBRV3         READE(N)  FNBRV02                                32
458100950113    2C                   ENDDO
458200940414     C**
458300940414     C                   ADD       1             I
458400950113    1C                   ENDDO
458500940414     C*
458600940418     C* CHIUSURA FILES
458700021021     C                   CLOSE     FNANM20L
458800021021     C                   CLOSE     FNBLT38L
458900020605     C                   CLOSE     FNBTT38L
459000020917     C                   CLOSE     FNFVV02L
459100001120     C                   CLOSE     FNFVV01L
459200940418     C*
459300940414     C                   ENDSR
459400970506     C*
459500970506     C* AGGIORNO LA DATA ENTRATA/DATA ENTRATA TRANSITO ---------------*
459600970506     C     AGGENT        BEGSR
459700970506     C* IMPOSTO LA KEY
459800970506     C                   EXSR      IMPKEY
459900041217     c                   clear                   Wdse0             1
460000041217     c
460100041217    1c                   if        *in40 and bltdse=0
460200041217     c                   eval      Wdse0='S'
460300041217    1c                   endif
460400970506     C**
460500970506     C* S P U N T A   P A R T E N Z A
460600970506     C**
460700970506     C* SE NON C'E' SPUNTA ENTRATA CONTROLLO SE NON L'HO GIA' SPARATO
460800970506     C* CERCO CON CATEGORIA 5
460900001117     C*
461000041217     C* Per bolla no poste: cerco spunta se DSE=0
461100041217     C* per bolla    poste: cerco spunta se DSE=0 prima dell'imi
461200041217     c* per       entrambe: cerco spunta se peso o volume=0
461300041217     c*                     di bolla con serie con utilizzo in spunta
461400041217     c*                     del peso o volume (da azorg)
461500041217    1c                   if        *in40
461600041217   1ac                   if        wdse0='S' or
461700041217     c                             (bltnrs>0 and bltvuc=0 and pa2spv='S') or
461800041217     c                             (bltnrs>0 and bltpuc=0 and pa2spp='S')
461900001121     C*
462000970506     C                   Z-ADD     5             WNPG
462100140618     C     KBRV55        CHAIN(N)  FNBRV05                            31
462200990917    2C                   SELECT
462300001123     C* Se fv PT do per scontato che p.o. poste è senza macchinone
462400001123     c* Quindi, in ricerca spunte entrata faccio solo una chain e non
462500001123     c* un ciclo di lettura
462600990917     C     *IN31         WHENEQ    *OFF
462700040121     c     pa2spv        andeq     ' '
462800040121     c     pa2spp        andeq     ' '
462900990917     C                   EXSR      AGGEN2
463000001117     C* N.B.: SE CML ESEGUE CICLO LETTURA DI TUTTE LE SPUNTE ENTRATA PE
463100001117     C*       R AVERE LA CERTEZZA DI MEMORIZZARE IL PESO E IL VOLUME
463200990917     C     *IN31         WHENEQ    *OFF
463300040121     c     pa2spv        andne     ' '
463400040121     c     *in31         oreq      *off
463500040121     c     pa2spp        andne     ' '
463600990917    3C     *IN31         DOWEQ     *OFF
463700990917     C                   EXSR      AGGEN2
463800140618     C     KBRV55        READE(N)  FNBRV05                                31
463900990917    3C                   ENDDO
464000990917     C*
464100990917   X2C                   OTHER
464200041217     c* La cat 1 o qualsiasi spunta  le cerco solo per dse=0
464300041217   2ac                   if        wdse0='S'
464400990917     C*
464500970506     C* SE NON C'E' CERCO CON CATEGORIA 1
464600970506     C                   Z-ADD     1             WNPG
464700140618     C     KBRV5         CHAIN(N)  FNBRV05                            31
464800990917    3C     *IN31         IFEQ      *OFF
464900990917     C                   EXSR      AGGEN2
465000990917   X3C                   ELSE
465100970506     C*
465200970506     C* ALTRIMENTI CERCO IN UNA QUALSIASI ALTRA CATEGORIA
465300070123     C     KBRV6         CHAIN     FNBRV07                            31
465400990917     C  N31              EXSR      AGGEN2
465500990917    3C                   ENDIF
465600041217   2ac                   ENDIF
465700041217    2C                   ENDSL
465800970506     C**
465900041217     C* SE C'ERA VOLUME CML memorizzo per eseguire a fine il calcolo
466000990917    2C     BLTVUC        IFGT      0
466100990917     C     BLTPUC        ORGT      0
466200041217     c                   eval      wcalLSB7='S'
466300990917    2C                   ENDIF
466400041217   1aC                   ENDIF
466500041217    1C                   ENDIF
466600970506     C**
466700970506     C* S P U N T A   T R A N S I T O
466800970506     C**
466900970506     C* SE NON C'E' DATA ENTRATA AL TRANSITO CONTROLLO SE L'HO SPARATO
467000970506    1C   41BTTDET        IFEQ      0
467100970506     C*  CERCO IN UNA QUALSIASI CATEGORIA
467200070123     C     KBRV6         CHAIN     FNBRV07                            31
467300990917    2C     *IN31         DOWEQ     *OFF
467400990917     C                   EXSR      AGGEN2
467500990917     C**
467600990917     C* SE HO CML VADO AVANTI A LEGGERE
467700040121     c     pa2spv        ifne      *blanks
467800040121     c     pa2spp        orne      *blanks
467900070123     C     KBRV6         READE     FNBRV07                                31
468000990917     C                   ELSE
468100990917     C                   SETON                                        31
468200990917     C                   ENDIF
468300990917    2C                   ENDDO
468400990917     C**
468500990917    2C     BTTVUC        IFGT      0
468600990917     C     BTTPUC        ORGT      0
468700041217     c                   eval      wcalLSB7='S'
468800990917    2C                   ENDIF
468900990917     C*
469000970506    1C                   END
469100970506     C                   ENDSR
469200990917     C*
469300990917     C* CALL A PGM FNLS44R PER AGGIORNAMENTO -------------------------*
469400990917     C     AGGEN2        BEGSR
469500990917     C*
469600990917     C* SE NON E' PISTOLA CREATA IN AUTOMATICO, AGGIORNO BOLLA
469700990917     C     BRVNPS        IFGT      0
469800990917     C     BRVNPS        LOOKUP    NPS                                    31
469900990917     C                   ELSE
470000990917     C                   SETOFF                                       31
470100990917     C                   ENDIF
470200990917     C**
470300990917    3C     *IN31         IFEQ      *OFF
470400990917     C*
470500990917     C                   EXSR      CALL44
470600000731     C     *IN40         IFEQ      *ON
470700000731     C* CHAINO FNBLT PER REPERIRE LA DATA ENTRATA AGGIORNATA
470800021021     C* ANCHE SE SONO NEL CICLO DI LETTURA FNBLT38L NON MI PREOCCUPO
470900000731     C* CHE SI SPORCHINO I CAMPI PERCHE' CON QUESTA CHAIN VADO A
471000000731     C* RILEGGERMI LO STESSO RECORD
471100040319     c* stessa cosa per fnbtt
471200000731     C     KBLT1         CHAIN(N)  FNBLT01L                           31
471300040319     c                   else
471400040319     c     *in41         ifeq      *on
471500040319     C     KBTT1         CHAIN     FNBTT11L                           31
471600040319     c                   endif
471700000731     C                   ENDIF
471800990917     C                   ENDIF
471900990917     C                   ENDSR
472000940414     C*
472100970828     C* CREO SPUNTE PARTENZA --------------------------------------*
472200940414     C     CRTBRC        BEGSR
472300940414     C                   SETOFF                                       33
472400950412     C                   CLEAR                   WDATU
472500970828     C                   CLEAR                   UPDBRV
472600940414     C*
472700950109     C                   Z-ADD     1             WNPG
472800020807    1C     *IN43         IFEQ      *OFF
472900970423     C* CHAIN PER NO SPUNTE ENTRATA
473000140618     C     KBRV5         CHAIN     FNBRV05                            31
473100961220     C*
473200970423    1C                   ELSE
473300040624     c
473400970423     C* CERCO SE C'E' LA BOLLA PARTENZA E NON E' BORDERIZZATA NON CREO
473500040624     C*  LA SPUNTA PARTENZA, se lnp in £1
473600960506     C     KBRV4         CHAIN(N)  FNBLT27L                           91
473700040624     c                   if        not *in91
473800040624     c     bltlnp        lookup    l1                                     37
473900040624     c                   if        not *in37
474000040624     c                   seton                                        91
474100040624     c                   endif
474200040624     c                   endif
474300040624     c
474400960506    2C     *IN91         IFEQ      *OFF
474500970507     C* CHAINO LA BOLLA
474600970507     C                   EXSR      CHABOL
474700020807     C* NON TROVATA--> COME  SE NON CI FOSSE
474800970507     C     WBOL          IFEQ      'N'
474900970507     C                   SETON                                        91
475000970507     C                   ENDIF
475100970506     C**
475200970506    3C  N91BLPFT1        IFEQ      ' '
475300961216     C     BLPDBR        ANDEQ     0
475400960503     C                   SETON                                        3133
475500960506   X3C                   ELSE
475600961220     C*
475700961220     C                   SETON                                        91
475800961220     C* SE SI TRATTA DI UNA BOLLA BORDERIZZATA SU FV ANCORA DA CHIUDERE
475900961220     C*  NON CREO SPUNTA PARTENZA
476000961220    4C     BLPFT1        IFEQ      ' '
476100961220     C     BLPNFV        ANDGT     0
476200961220     C     BLPNFV        ANDNE     FGV2
476300970424     C                   CLEAR                   DSLV53
476400970424     C                   MOVEL     BLPNFV        D53NFV
476500970424     C                   MOVEL     'G'           D53TFO
476600971107     C                   MOVEL     FGVLNP        D53FEL
476700020919     C                   Z-ADD     BLPTFP        D53FGS
476800970424     C                   CALL      'FNLV53R'
476900970424     C                   PARM                    DSLV53
477000970424    5C     D53FCF        IFEQ      ' '
477100961220     C                   SETON                                        3133
477200961220     C                   SETOFF                                       91
477300961220    5C                   ENDIF
477400961220    4C                   ENDIF
477500960506    3C                   ENDIF
477600970506     C*
477700970506   X2C                   ELSE
477800160622     C* NON TROVATA BOLLA PARTENZA
477900160622     c*  oppure bolla non del terminal di partenza:
478000160622     c*  se bolla arrivo non creo spunta partenza
478100160622     c*  devo tener conto anche se il secondo liv in partenza è un
478200160622     c*  terminal di arrivo (anche con 2°liv)
478300970506     C                   SETOFF                                       3435
478400160622     c* solo se non ho trovato fnblt rifaccio lokkup in £1
478500160622     c                   if        not %found(fnblt27l)
478600970506     C     KLNP          LOOKUP    L1                                     34
478700160622     c                   endif
478800130530     C*
478900031113     c     *in34         ifeq      *off
479000130530     c                   clear                   dslv55
479100130530     c                   movel     'A'           d55tpt
479200160622     c                   if        not %found(fnblt27l)
479300130530     c                   z-add     klnp          d55lnp
479400160622     c                   else
479500160622     c                   z-add     bltlnp        d55lnp
479600160622     c                   endif
479700130530     c                   z-add     klna          d55lin
479800130530     c                   z-add     wdfv          d55drf
479900130530     c                   call      'FNLV55R'
480000130530     c                   parm                    dslv55
480100130530     c     d55tfa        ifeq      fgvlnp
480200160622     c     d55tfa        oreq      brvpes
480300031113     c                   seton                                        3133
480400031113     c                   setoff                                       91
480500031113     c                   endif
480600031113     c                   endif
480700970506     C*
480800970506    2C                   ENDIF
480900960503     C*
481000140618     C   91KBRV5         CHAIN     FNBRV05                            31
481100961220    1C                   ENDIF
481200940414      *
481300970423      * 31 ON -NON ESISTE SPUNTA PARTENZA --> CREO SPUNTA (33OFF)
481400970423      * 31 OFF-    ESISTE SPUNTA PARTENZA :
481500970423     C**
481600950208    0C     *IN31         DOWEQ     *OFF
481700970828     C   42
481800970828     COR 43WRVVUC        IFGT      0
481900970828     C     BRVVUC        ANDEQ     0
482000971114     C                   Z-ADD     WRVVUC        BRVVUC
482100971114     C                   MOVEL     'S'           UPDBRV
482200970828     C                   ENDIF
482300990906     C   42
482400990906     COR 43WRVPUC        IFGT      0
482500990906     C     BRVPUC        ANDEQ     0
482600990906     C                   Z-ADD     WRVPUC        BRVPUC
482700990906     C                   MOVEL     'S'           UPDBRV
482800990906     C                   ENDIF
482900970423      * - BRVDTR =0 -->
483000950208    1C     BRVDTR        IFEQ      0
483100940414      *
483200970423      * FOGLIO <> DA QUELLO IN CHIUSURA
483300950109    2C     BRVNFV        IFNE      FGV2
483400940414      *
483500970424     C                   CLEAR                   DSLV53
483600000111     C                   Z-ADD     BRVNFV        D53NFV
483700970424     C                   MOVEL     'G'           D53TFO
483800020919     C                   Z-ADD     BRVFGS        D53FGS
483900970424     C                   CALL      'FNLV53R'
484000970424     C                   PARM                    DSLV53
484100940414      *
484200970423      * ESISTE FOGLIO --> SE E' APERTO NON ANNULLATO NON RICREO(33ON)
484300970424   3AC     D53ERR        IFEQ      ' '
484400970424    3C     D53FCF        IFEQ      ' '
484500970424     C     D53ATB        ANDEQ     ' '
484600970909     C     D53DEF        ANDEQ     ' '
484700940414     C                   SETON                                        33
484800950412     C                   ELSE
484900950412     C*
485000970424    4C     D53DFV        IFEQ      WDTP
485100950915     C* SE NON STO LEGGENDO SPUNTE ENTRATA CHAINO IN UPDAT
485200970828     C*  PERCHE' DEVO RIAGGIORNARLA CON I DATI DI QESTO FV
485300950915    5C     *IN43         IFEQ      *OFF
485400950412     C                   MOVEL     'S'           WDATU
485500950418     C                   MOVEL     BRVNFV        WSANFV
485600950418     C                   MOVEL     BRVFGS        WSAFGS
485700950915     C*
485800950915     C                   ELSE
485900950915     C* SE DA SPUNTA ENTRATA NON RICREO LA SPUNTA SE E' DI OGGI
486000950915     C                   SETON                                        33
486100950915    5C                   ENDIF
486200950915    4C                   END
486300950412    3C                   END
486400970424   3AC                   END
486500940414      *
486600940414      * NON ESISTE FOGLIO
486700970423      * O FOGLIO CHIUSO E ANNULLATO --> RICREO SPUNTA (VADO IN UPDATE)
486800940506      *                                                     33OFF
486900940506      *
487000950412   X2C                   ELSE
487100940506      *                        FOGLIO =  A QUELLO IN CHIUSURA: NON RICR
487200940506      *                                                     33ON
487300940414     C                   SETON                                        33
487400940414    2C                   END
487500950412     C*
487600950412   X1C                   ELSE
487700970423      *
487800970423      * - BRVDTR><0 --> SPUNTA GIA'TRASMESSA MA NON AGGIORNATA BOLLA:
487900970423      *                 SFLAGGO SEMPLICEMENTE LA SPUNTA E BASTA
488000971114    2C     BRVNFV        IFEQ      FGV2
488100971114     C  N43              CLEAR                   BRVDTR
488200000111     C  N43              CLEAR                   BRVHTR
488300971114     C  N43              CLEAR                   BRVFTR
488400950412     C*
488500971114     C  N43              MOVEL     'S'           UPDBRV            1
488600950412     C                   SETON                                        33
488700950412     C*
488800950915   X2C                   ELSE
488900970424      *
489000970424     C                   CLEAR                   DSLV53
489100000111     C                   Z-ADD     BRVNFV        D53NFV
489200970424     C                   MOVEL     'G'           D53TFO
489300020919     C                   Z-ADD     BrvFGS        D53FGS
489400970424     C                   CALL      'FNLV53R'
489500970424     C                   PARM                    DSLV53
489600950412     C* SE IL FOGLIO DELLA SPUNTA TROVATA NON HA LA STESSA DATA DEL FV
489700950412     C*  CHE STO CHIUDENDO--> TENGO ENTRAMBE LE SPUNTE, ALTRIMENTI NO
489800970424    3C     D53DFV        IFEQ      WDTP
489900970424    3C     D53ERR        ANDEQ     ' '
490000950915     C* SE NON STO LEGGENDO SPUNTE ENTRATA CHAINO IN UPDAT
490100950915    4C     *IN43         IFEQ      *OFF
490200950412     C                   MOVEL     'S'           WDATU             1
490300950418     C                   MOVEL     BRVNFV        WSANFV
490400950418     C                   MOVEL     BRVFGS        WSAFGS
490500950915     C*
490600950915   X4C                   ELSE
490700950915     C* SE DA SPUNTA ENTRATA NON RICREO LA SPUNTA PARTENZA DI OGGI
490800980316     C* (SOLO SE SPUNTA NON 'L')
490900980316     C     BRVFTR        IFNE      'L'
491000950915     C                   SETON                                        33
491100980316     C                   ELSE
491200980316     C                   MOVEL     'S'           WDATU             1
491300980316     C                   MOVEL     BRVNFV        WSANFV
491400980316     C                   MOVEL     BRVFGS        WSAFGS
491500980316     C                   ENDIF
491600950915    4C                   ENDIF
491700950915    3C                   ENDIF
491800950412     C*
491900950412    2C                   ENDIF
492000950412    1C                   ENDIF
492100950208     C*
492200950208     C* SE LA SPUNTA NON E' QUELLA BUONA VEDO SE CE N'E' UN'ALTRA
492300950412    1C     *IN33         IFEQ      *OFF
492400140618     C     KBRV5         READE     FNBRV05                                31
492500950412   X1C                   ELSE
492600950208     C* SE TROVATA SPUNTA BUONA --> ESCO
492700950208     C                   SETON                                        31
492800991019     C*
492900991019     C* CONTROLLO PESO E VOLUME DA BLT E BTT PERCHE'DEVONO ESSERE SEMPR
493000991019     C*  POSSIBILMENTE, QUESTI AD AGGIORNARE LE SPUNTE DA TRASMETTERE
493100991019     C*  IN ARRIVO
493200991019     C   40BLTVUC        IFGT      0
493300991019     C     BLTVUC        ANDNE     BRVVUC
493400991019     C                   Z-ADD     BLTVUC        BRVVUC
493500991019     C                   MOVEL     'S'           UPDBRV
493600991019     C                   ENDIF
493700991019     C   40BLTPUC        IFGT      0
493800991019     C     BLTPUC        ANDNE     BRVPUC
493900991019     C                   Z-ADD     BLTPUC        BRVPUC
494000991019     C                   MOVEL     'S'           UPDBRV
494100991019     C                   ENDIF
494200991019     C**
494300991019     C   41BTTVUC        IFGT      0
494400991019     C     BTTVUC        ANDNE     BRVVUC
494500991019     C                   Z-ADD     BTTVUC        BRVVUC
494600991019     C                   MOVEL     'S'           UPDBRV
494700991019     C                   ENDIF
494800991019     C   41BTTPUC        IFGT      0
494900991019     C     BTTPUC        ANDNE     BRVPUC
495000991019     C                   Z-ADD     BTTPUC        BRVPUC
495100991019     C                   MOVEL     'S'           UPDBRV
495200991019     C                   ENDIF
495300990917     C*
495400970828    3C     UPDBRV        IFEQ      'S'
495500140618     C                   UPDATE    FNBRV05
495600970828     C                   ELSE
495700140618     C                   UNLOCK    FNBRV05L
495800970828    3C                   ENDIF
495900991019     C**
496000991019    1C                   ENDIF
496100950208    0C                   ENDDO
496200970423     C**
496300970423     C* CONTROLLO SE COLLO SPUNTATO O MENO IN DATA ODIERNA
496400970423     C*  PER CREARE SPUNTE PISTOLE 98 96
496500970828    1C     *IN33         IFEQ      *OFF
496600970828     C* ENTRO ANCHE PER LEGGERE LE SPUNTE SE DEVO CERCARE IL VOLUME
496700970423     C                   CLEAR                   WNPS
496800070123     C     KBRV6         SETGT     FNBRV07L
496900070123     C     KBRV6         READPE    FNBRV07L                               34
497000970828    2C     *IN34         DOWEQ     *OFF
497100970828     C**
497200970828     C* VEDO SE SPUNTA CON VOLUME CML
497300970828     C   42
497400970828     COR 43WRVVUC        IFEQ      0
497500970828    3C     BRVVUC        ANDGT     0
497600970828     C                   Z-ADD     BRVVUC        WRVVUC
497700970828    3C                   ENDIF
497800000428     C* VEDO SE SPUNTA CON PESO CML
497900990906     C   42
498000990906     COR 43WRVPUC        IFEQ      0
498100990906    3C     BRVPUC        ANDGT     0
498200990906     C                   Z-ADD     BRVPUC        WRVPUC
498300990906    3C                   ENDIF
498400970828     C**
498500970423     C* ESCLUDO LE CATEGORIE 1
498600970828    3C     BRVNPG        IFNE      1
498700980116     C* AD ECCEZIONE DELLE SPUNTE PARTENZA SU FV NON CONTENENTE BRVLNA
498800980116     C* (BRVFTR='L')
498900980116     C     BRVFTR        OREQ      'L'
499000970424     C* CALL AL PGM PER REPERIRE LA DATA FOGLIO
499100970424     C                   CLEAR                   DSLV53
499200970424     C                   MOVEL     BRVNPG        D53NPG
499300000111     C                   Z-ADD     BRVNFV        D53NFV
499400970424     C                   MOVEL     BRVFGS        D53FGS
499500970424     C                   CALL      'FNLV53R'
499600970424     C                   PARM                    DSLV53
499700970424     C*
499800970828    4C     D53DFV        IFGE      WDTP
499900970423     C                   MOVEL     98            WNPS
500000970528     C                   MOVEL     BRVNPS        SPUNPS
500100970423     C                   SETON                                        34
500200970828   X4C                   ELSE
500300970828    5C     WNPS          IFNE      98
500400970423     C                   MOVEL     96            WNPS
500500970528     C                   MOVEL     BRVNPS        SPUNPS
500600970828    5C                   ENDIF
500700970828    4C                   ENDIF
500800970828    3C                   ENDIF
500900970423     C*
501000970828    3C     *IN34         IFEQ      *OFF
501100040121     C**   PARCML        ORNE      ' '
501200040121     c     pa2spv        oreq      'S'
501300970828     C     WRVVUC        ANDEQ     0
501400970828     C     *IN40         ANDEQ     *OFF
501500970828     C     *IN41         ANDEQ     *OFF
501600990907      *PESO
501700040121     C**   PARCML        OREQ      'P'
501800040121     c     pa2spp        oreq      'S'
501900990907     C     WRVPUC        ANDEQ     0
502000990907     C     *IN40         ANDEQ     *OFF
502100990907     C     *IN41         ANDEQ     *OFF
502200070123     C     KBRV6         READPE    FNBRV07L                               34
502300970828    3C                   ENDIF
502400970828    2C                   ENDDO
502500970423     C*
502600991019    1C                   ENDIF
502700950208     C*
502800980206     C                   CLEAR                   WBRVE
502900950208     C* SE HO TROVATO UNA SPUNTA LA RICHAINO
503000970828    0C     WDATU         IFEQ      'S'
503100970828     C     *IN33         ANDEQ     *OFF
503200070123     C     KBRV2         CHAIN     FNBRV02                            31
503300980206     C* MEMORIZZO SPUNTA PARTENZA ERRATA IN CAMPI DEL FILE STORICO
503400980206     C* SPUNTE PARTENZA ERRATE
503500980206     C  N31BRVFTR        IFEQ      'L'
503600980206     C                   MOVE      'S'           WBRVE             1
503700980209     C                   MOVEL     DSBRV         SVBRVE
503800980206     C                   END
503900970828    0C                   ENDIF
504000950109     C*
504100950109     C* SOLO PER I TRANSITI: AUTOGENERO SPUNTA PER BOLLE TRANSITO
504200950109     C*                      SOLO SE RELATIVO FOGLIO ABBINATO
504300950412    0C   41BTTDET        IFEQ      0
504400950127     C* COME PRIMA COSA VERIFICO SE PER CASO LA SPUNTA ESISTE
504500950127     C* MA NON HA AGGIORNATO IL RECORD
504600950109     C                   EXSR      ABBBTT
504700950412    1C     WABBIN        IFNE      'S'
504800950118     C                   SETON                                        33
504900950412    1C                   ENDIF
505000950412    0C                   ENDIF
505100940512     C*
505200950412    0C  N33              DO
505300000714     C**
505400070123     C* SCARICO SPUNTA ERRATA IN FNBRVE
505500980206     C     WBRVE         IFEQ      'S'
505600980209     C* CONTROLLO CHE IL COLLO NON SIA IN IMP
505700070123     C     KBRV01        SETLL     FNBRV01L                               37
505800010611     c     *in37         ifeq      *off
505900070219     c     kbrve         setll     fnbrve                                 99
506000010611     C  n99              MOVEL     SVBRVE        DSBRV
506100070123     C  N99              WRITE     FNBRVE
506200010611     c                   endif
506300980206     C                   END
506400140618     C                   CLEAR                   FNBRV05
506500950109     C*
506600950118     C                   MOVEL     FGVLNP        BRVFGS
506700000111     C                   Z-ADD     FGVLNP        BRVPES
506800000111     C                   Z-ADD     FGV2          BRVNFV
506900950118     C                   Z-ADD     1             BRVNPG
507000971107     C                   Z-ADD     FGVLNP        BRVFLE
507100070220     c                   time                    wtimez
507200070220     c* millisecondi
507300070220     c                   extrct    wtimez:*ms    millisec          6 0
507400070220     c                   movel     millisec      brvmis
507500070220     c* data immissione
507600070220     c                   movel     wtimez        dataiso
507700070220     c                   movel     dataiso       brvdfs
507800070220     c* ora immissione
507900070220     c                   move      wtimez        oraiso
508000070220     c                   movel     oraiso        brvhms
508100070220     c                   movel     'A'           brvtap
508200070220     c                   eval      brvpru='§'+ nompgm
508300080328     c                   clear                   brvtsu
508400950109     C*
508500950915     C   40              EXSR      CRTBLT
508600950109     C*
508700950109     C   41              EXSR      CRTBTT
508800950109     C*
508900950113     C   42              EXSR      CRTSAN
509000940414     C*
509100950915     C   43              EXSR      CRTSPE
509200970423     C**
509300970423     C** VEDO SE IMPOSTARE PISTOLA 98 O 96
509400970506     C**  SOLO SE NON SI TRATTA DI UNA PISTOLA 90 CHE VINCE
509500970507     C**  ESCLUDO SEMPRE I LOCALI PER I QUALI LA PISTOLA 98 96 NON ESIS
509600970506     C**
509700970507    1C     BRVNPS        IFNE      90
509800970507     C*
509900970507    2C     WNPS          IFEQ      96
510000970507    3C     WLOCAL        IFNE      'S'
510100970423     C                   MOVEL     WNPS          BRVNPS
510200970528     C* PER BOLLE TRANSITO SE L'UNICA SPUNTA PRESENTE E' UNA 89 CREO
510300970528     C*  UNA 89
510400971027     C* ANCHE PER DISGUIDO
510500971027     C   42
510600971027     COR 41SPUNPS        IFEQ      89
510700970528     C                   MOVEL     SPUNPS        BRVNPS
510800970528     C                   ENDIF
510900970507     C                   ELSE
511000970507     C* LA 96 LOCALE DIVENTA --> 90
511100970507     C                   MOVEL     90            BRVNPS
511200970507    3C                   ENDIF
511300970507     C**
511400970507   X2C                   ELSE
511500970423     C**
511600970423     C* LA PISTOLA 98 E' SOLO PER CHI NON HA IL CML
511700010910     c* (e per chi ha il flag §ogagp = blank)
511800970507    3C     WNPS          IFEQ      98
511900970423     C     PARCML        ANDEQ     ' '
512000970507     C     WLOCAL        ANDNE     'S'
512100010910     C     pa2agp        andeq     *blanks
512200970423     C                   MOVEL     WNPS          BRVNPS
512300970528     C* PER BOLLE TRANSITO SE L'UNICA SPUNTA PRESENTE E' UNA 89 CREO
512400971027     C*  UNA 89 ANCHE PER I DISGUIDI
512500971027     C   42
512600971027     COR 41SPUNPS        IFEQ      89
512700970528     C                   MOVEL     SPUNPS        BRVNPS
512800970528     C                   ENDIF
512900970507    3C                   ENDIF
513000970507    2C                   ENDIF
513100040406     C*
513200970507    1C                   ENDIF
513300950915     C*
513400140618     C   31              WRITE     FNBRV05
513500070123     C  N31              UPDATE    FNBRV02
513600950203     C*
513700950203     C* SE AUTOGENERATA SPUNTA PARTENZA: AGGIORNO BOLLA TRANSITO
513800970507    1C     *IN41         IFEQ      *ON
513900950203     C     BRVNPS        ANDEQ     89
514000970423     C                   EXSR      CALL44
514100970507    1C                   END
514200950412    0C                   END
514300940414     C*
514400940414     C                   ENDSR
514500950109     C*
514600950109     C*--- VEDO SE COLLO TRANSITO DICHIARATO PARTITO E ABBINATO-------*
514700950109     C     ABBBTT        BEGSR
514800950109     C*
514900950118     C* CHAIN SULLE SPUNTE PER VEDERE SE C'E' LA SPUNTA PARTENZA DA
515000950118     C*  ALTRA FILIALE --> SE NON C'E' NON CREO SPUNTA
515100950118     C*
515200950118     C* IN QUESTO CASO NON IMPORTA CHE TESTI SE LA SPUNTA NON E' DI
515300950118     C* SIMFEL PERCHE' SE CI FOSSE AVREI AGGIORNATO LA DATA ENTRATA
515400950118     C* E QUINDI NON SAREI DENTRO A QUESTA ROUTIME
515500950109     C                   Z-ADD     1             WNPG
515600950109     C                   CLEAR                   WABBIN
515700140618     C     KBTT          CHAIN(N)  FNBRV05                            34
515800970828     C**
515900950109    1C     *IN34         DOWEQ     *OFF
516000970828     C                   CLEAR                   DSLV53
516100020617     C                   MOVEL     'T'           D53TFO
516200070123     C****               Z-ADD     FGVLNP        D53FEL
516300000111     C                   Z-ADD     BRVNFV        D53NFV
516400970828     C                   MOVEL     BRVNPG        D53NPG
516500970828     C                   MOVEL     BRVFGS        D53FGS
516600070123     C****               MOVEL     BRVFLE        D53FLF
516700970828     C                   MOVEL     BRVLNA        D53LNA
516800970828     C                   MOVEL     'A'           D53ABB
516900020617     c                   z-add     fgvlnp        d53lai
517000970828     C                   CALL      'FNLV53R'
517100970828     C                   PARM                    DSLV53
517200970828    2C     D53ERR        IFEQ      ' '
517300970828     C     D53NFA        ANDGT     0
517400950113     C                   MOVEL     'S'           WABBIN            1
517500970828     C                   SETON                                        34
517600970828     C                   ELSE
517700950109     C*
517800140618     C     KBTT          READE(N)  FNBRV05                                34
517900970828    2C                   ENDIF
518000950113    1C                   ENDDO
518100950109     C*
518200950109     C                   ENDSR
518300970507     C*
518400970507     C* CHAINO BOLLA PARTENZA O BOLLA TRANSITO -----------------------*
518500040628     c     CHABOL        BEGSR
518600970507     C**
518700970507     C* CHAINO BOLLA PARTENZA
518800970507     C**
518900000620     C   45
519000000620     COR 43
519100970507    1COR 40BLTSPE        IFNE      SAVSPE
519200970507     C                   MOVEL     'S'           WBOL              1
519300970507     C                   CLEAR                   WLOCAL
519400970507     C     KBLT3         CHAIN(N)  FNBLP01L                           37
519500970507     C                   MOVEL     BLTSPE        SAVSPE           16 0
519600970507     C**
519700970507    2C     *IN37         IFEQ      *ON
519800970507     C                   MOVEL     'N'           WBOL
519900970507     C                   MOVEL     'S'           §1BFG5
520000970507   X2C                   ELSE
520100970507     C*
520200970507     C* CONTROLLO SE E' BOLLA LOCALE
520300970507    3C     BLPTFP        IFEQ      BLPTFA
520400970714     C     BLPLNP        OREQ      BLPLNA
520500970507     C                   MOVEL     'S'           WLOCAL            1
520600970507    3C                   ENDIF
520700970507     C*
520800970507     C* COD TRATTAMENTO MERCE CHE GENERA IN AUTOMATICO LE SPUNTE PARTEN
520900970507     C     BLPCTM        LOOKUP    CTM                                    37
521000970507     C*     TROVATO= IMPOSTO §1BFG5 = N
521100970507     C   37              MOVEL     'N'           §1BFG5
521200970507     C* NON TROVATO= IMPOSTO §1BFG5 = S
521300970507     C  N37              MOVEL     'S'           §1BFG5
521400970507    2C                   ENDIF
521500040624    1C                   ENDIF
521600040624     C**
521700970507     C* CHAIN BOLLA TRANSITO
521800970507     C**
521900970507     C   41BTTSPE        IFNE      SAVSPE
522000970507     C                   CLEAR                   WLOCAL
522100020605     C     KBTT3         CHAIN(N)  FNBTP11L                           37
522200970507     C                   MOVEL     BTTSPE        SAVSPE
522300970507     C                   ENDIF
522400970507     C                   ENDSR
522500950109     C*
522600950109     C* CREO SPUNTA DA FNBLT -----------------------------------------*
522700950109     C     CRTBLT        BEGSR
522800130226     c                   eval      wtfp90='N'
522900950109     C                   MOVEL     BLTFLS        BRVLNP
523000950109     C                   MOVEL     BLTLNA        BRVLNA
523100950109     C                   MOVEL     BLTNRS        BRVNRS
523200950113     C                   MOVEL     BLTNSC        BRVNSC
523300970507     C* CHAINO BOLLA
523400970507     C                   EXSR      CHABOL
523500950109     C*
523600970828     C                   MOVEL     BLPZNC        BRVZNC
523700950118     C                   MOVEL     BLTCAN        BRVCAN
523800970828     C                   MOVEL     BLTVUC        BRVVUC
523900990907     C                   MOVEL     BLTPUC        BRVPUC
524000950109     C*
524100960209     C* CONTROLLO SE ESISTE SPUNTA ENTRATA
524200950113    1C   44BLTDSE        IFEQ      0
524300950109     C                   SETON                                        33
524400950109     C                   ELSE
524500950109     C                   SETOFF                                       33
524600950113    1C                   END
524700950109     C*
524800960209     C* SE NON PREVISTA GENERAZ.DA SPU.ENTRATA--> SEMPRE 88 O 90
524900950109     C  N44              SETON                                        33
525000950109     C*
525100950109     C*     ESISTE SPUNTA ENTRATA --> STESSO NUMERO PISTOLA
525200950113    1C     *IN33         IFEQ      '0'
525300950109     C                   MOVEL     BLTNPE        BRVNPS
525400950113   X1C                   ELSE
525500950109     C*
525600950109     C* NON ESISTE SPUNTA ENTRATA --> PISTOLA AUTOGENERATA 88 O 90
525700950109     C                   MOVEL     '88'          BRVNPS
525800130226     c* controllo se bolla con colli che partono da altra filiale
525900130226     c*  devo impostare pistola 90
526000130226     c                   if        blpxco='W'
526100130226     c                   exsr      Contr_XCO
526200130227     c
526300130227     c                   if        wtfp90='S'
526400130227     C                   MOVEL     90            BRVNPS
526500130226     c                   endif
526600130227     c                   endif
526700970507     C*
526800970423    2C     §1BFG5        IFEQ      'N'
526900970507     C     WLOCAL        OREQ      'S'
527000970507     C                   MOVEL     90            BRVNPS
527100970507    2C                   ENDIF
527200970507     C*
527300970423    1C                   END
527400950109     C*
527500950109     C                   ENDSR
527600970507     C*
527700130226     C* Controllo se collo partito da altro TFP ----------------------*
527800130226     C     Contr_XCO     BEGSR
527900130226     c
528000130227     C                   MOVEL     DSBRV         DSBRVS
528100130226     c                   eval      Wflsnrs=(blpfls*100)+bltnrs
528200130226
528300130226    1c                   if        wflsnrs<>savfnrs
528400130226     c                   clear                   xcotfp
528500130226     C                   MOVEL     'XCO'         kcod1
528600130226     C                   eval      kke1='ALTRITFP'
528700130226     C                   eval      kke2=%editc(wflsnrs:'X')
528800130226     c     ktbe2         chain     tntbe01l
528900130226    2c                   if        %found(tntbe01l) AND tbeatb=' '
529000130226     C                   MOVEA     TBeUNI        xcotfp
529100130226    2c                   endif
529200130226     c
529300130226     c                   eval      savfnrs=wflsnrs
529400130226    1c                   endif
529500130226     c
529600130226     c                   z-add     5             wnpg
529700130226    1c                   do        2
529800140618     c     kblt_xco      setll     fnbrv05l
529900140618     c     kblt_xco      reade     fnbrv05l
530000130226     c
530100140618    2c                   dow       not %eof(fnbrv05l)
530200130226    3c                   if        brvfle<>fgvlnp
530300130226     c* Verifico se la filiale che spunta è prevista
530400130226     c                   movel     brvfgs        alfafgs           3
530500130226     c                   eval      Indx=%lookup(alfafgs:xcotfp)
530600130226     c* Trovata --> memerizzo per impsostare pistola 90
530700130226    4c                   if        Indx>0
530800130226     c                   eval      Wtfp90='S'
530900130226     c                   leave
531000130226    4c                   endif
531100130226    3c                   endif
531200130226     c
531300140618     c     kblt_xco      reade     fnbrv05l
531400130226    2c                   enddo
531500130226     c
531600130226    2c                   if        wtfp90='S'
531700130226     c                   leave
531800130226     c                   else
531900130226     c                   eval      wnpg=1
532000130226    2c                   endif
532100130226     c
532200130226    1c                   enddo
532300130226     c
532400130227     C                   MOVEL     DSBRVS        DSBRV
532500130226     C                   ENDSR
532600130226     C*
532700130226     C* CREO SPUNTA DA FNBTT -----------------------------------------*
532800130226     C     CRTBTT        BEGSR
532900950109     C                   MOVEL     BTTFLS        BRVLNP
533000950109     C                   MOVEL     BTTLNA        BRVLNA
533100950109     C                   MOVEL     BTTNRS        BRVNRS
533200950113     C                   MOVEL     BTTNSC        BRVNSC
533300970507     C* CHAINO BOLLA
533400970507     C                   EXSR      CHABOL
533500950109     C*
533600950109     C                   MOVEL     BTPZNC        BRVZNC
533700990906     C                   MOVEL     BTTVUC        BRVVUC
533800990907     C                   MOVEL     BTTPUC        BRVPUC
533900950109     C                   MOVEL     BTTCAN        BRVCAN
534000950109     C* SE E' PISTOLA AUTOGENERATA SENZA SPUNTA IN ENTRATA:
534100950109     C* --> PISTOLA 89
534200950109     C     BTTDET        IFEQ      0
534300950109     C                   MOVEL     '89'          BRVNPS
534400950109     C                   ELSE
534500960209     C* GENERAZIONE SPUNTE 88
534600950109     C     *IN44         IFEQ      *OFF
534700950109     C                   MOVEL     '88'          BRVNPS
534800950109     C                   ELSE
534900960209     C* GENERAZIONE SPUNTE DA SPU.ENTRATA
535000950109     C                   MOVEL     BTTPET        BRVNPS
535100950109     C                   ENDIF
535200950109     C                   ENDIF
535300950109     C*
535400950109     C                   ENDSR
535500950109     C*
535600950109     C* CREO SPUNTA DA ANOMALIE --------------------------------------*
535700950113     C     CRTSAN        BEGSR
535800950109     C                   MOVEL     ANMFLS        BRVLNP
535900950109     C                   MOVEL     ANMLNA        BRVLNA
536000950109     C                   MOVEL     ANMNRS        BRVNRS
536100950109     C                   MOVEL     ANMSCN        BRVNSC
536200950109     C                   MOVEL     WRVZNC        BRVZNC
536300970828     C                   MOVEL     WRVVUC        BRVVUC
536400990907     C                   MOVEL     WRVPUC        BRVPUC
536500950109     C                   MOVEL     WRVCAN        BRVCAN
536600960209     C* GENERAZIONE SPUNTE 88
536700950109     C     *IN44         IFEQ      *OFF
536800950109     C                   MOVEL     '88'          BRVNPS
536900950109     C                   ELSE
537000960209     C* GENERAZIONE DA SPUNTE ENTRATA
537100950109     C                   MOVEL     ANMNPS        BRVNPS
537200950109     C                   ENDIF
537300950109     C*
537400950109     C                   ENDSR
537500950915     C*
537600950915     C* CREO SPUNTA DA SPUNTE ENTRATA --------------------------------*
537700950915     C     CRTSPE        BEGSR
537800950915     C                   MOVEL     KLNP          BRVLNP
537900950915     C                   MOVEL     KLNA          BRVLNA
538000950915     C                   MOVEL     KNRS          BRVNRS
538100950915     C                   MOVEL     KNSC          BRVNSC
538200950915     C                   MOVEL     WRVZNC        BRVZNC
538300950915     C                   MOVEL     WRVVUC        BRVVUC
538400990907     C                   MOVEL     WRVPUC        BRVPUC
538500950915     C                   MOVEL     WRVCAN        BRVCAN
538600950915     C                   MOVEL     WRVNPS        BRVNPS
538700950915     C*
538800950915     C                   ENDSR
538900960531     C*
539000960705     C* CHIUDO ANMOALIE DI DISGUIDO SE ESISTE BOLLA PARTENZA ---------*
539100020412     C     SRDISG        BEGSR
539200070918     C**** kblt27        setll     FNBLT27L                               36
539300070918    2C**** *in36         IFEQ      *ON
539400070918     c
539500070918     C     kblt27        chain(N)  FNBLT27L
539600070918     c                   if        %found(fnblt27l)
539700070918     c* Se bolla trovata e del terminal di partenza
539800070918     c*  chiudo le anomalie 200 55 56
539900070918     c     bltlnp        lookup    l1                                     36
540000070918     c                   if        *in36
540100020412     C**TROVATA BOLLA PARTENZA: CHIUDO ANOMALIE 200 55 e 56
540200020412     C                   MOVE      'N'           WCRT              1
540300020412     C                   CLEAR                   DSLR33
540400020412     C                   MOVE      ANMDAO        D33DCH
540500020412     C                   MOVEL     'AN'          D33CCH
540600020412     C                   MOVE      ANMNR0        D33NRR
540700020412     C                   CALL      'FNLR33R'
540800020412     C                   PARM                    DSLR33
540900970828     C**CHIUDO ANCHE ANOMALIA 55
541000020412     C                   Z-ADD     55            COMCAA
541100020412     C     kanm31        CHAIN(N)  FNANM                              21
541200020412     C     *in21         IFEQ      *OFF
541300020412     C                   CLEAR                   DSLR33
541400020412     C                   MOVE      ANMDAO        D33DCH
541500020412     C                   MOVEL     'M'           D33FSC
541600020412     C                   MOVEL     'AN'          D33CCH
541700020412     C                   MOVEL     'S'           D33MAC
541800020412     C                   MOVE      ANMNR2        D33NRR
541900020412     C                   CALL      'FNLR33R'
542000020412     C                   PARM                    DSLR33
542100020412     C                   END
542200020412     C**CHIUDO ANCHE ANOMALIA 56
542300020412     C                   Z-ADD     56            COMCAA
542400020412     C     kanm31        CHAIN(N)  FNANM                              21
542500020412     C     *in21         IFEQ      *OFF
542600020412     C                   CLEAR                   DSLR33
542700020412     C                   MOVE      ANMDAO        D33DCH
542800020412     C                   MOVEL     'M'           D33FSC
542900020412     C                   MOVEL     'AN'          D33CCH
543000020412     C                   MOVEL     'S'           D33MAC
543100020412     C                   MOVE      ANMNR2        D33NRR
543200020412     C                   CALL      'FNLR33R'
543300020412     C                   PARM                    DSLR33
543400020412     C                   END
543500020412     C                   Z-ADD     200           COMCAA
543600020412    2C                   END
543700070918    2C                   END
543800070918     c
543900020412     C                   ENDSR
544000020411     C*--- RICERCO LNP BOLLA SE COLLO con serie > 0  -----------------*
544100960531     C     RICLNP        BEGSR
544200960531     C*
544300960611     C                   CLEAR                   WOKBL
544400960611     C                   CLEAR                   WOKBT
544500960603     C                   MOVE      BRVLNP        WLNP
544600970811     C                   CLEAR                   WTAB5F
544700960531     C*
544800020411    1C     BRVNRS        IFGT      *ZEROS
544900150610     c     w_FilBsp      oreq      'S'
545000020412     c* CERCO LA BOLLA
545100020412    2C                   SELECT
545200020411     C     WALCF         WHENEQ    'N'
545300020411     C     KBRV4         CHAIN(N)  FNBLT27L                           36
545400020411     c     *in36         ifeq      *off
545500020411     C                   MOVE      BLTLNP        WLNP
545600020411     C                   MOVE      'S'           WOKBL             1
545700020411     C                   CLEAR                   WTAB5F
545800020411     c                   else
545900020606     C     KBtt37        CHAIN(N)  FNBTT37L                           36
546000020411     c     *in36         ifeq      *off
546100020411     C                   MOVE      BTTLNP        WLNP
546200020411     C                   MOVE      'S'           WOKBT             1
546300020411     C                   CLEAR                   WTAB5F
546400020411     c                   endif
546500020411     c                   endif
546600020411     C     WALCF         WHENEQ    'S'
546700020411     C     KBRV4         CHAIN     FNBLT27L                           36
546800020411     c     *in36         ifeq      *off
546900020411     C                   MOVE      BLTLNP        WLNP
547000020411     C                   MOVE      'S'           WOKBL             1
547100020411     C                   CLEAR                   WTAB5F
547200020411     c                   else
547300020606     C     KBtt37        CHAIN     FNBTT37L                           36
547400020411     c     *in36         ifeq      *off
547500020411     C                   MOVE      BTTLNP        WLNP
547600020411     C                   MOVE      'S'           WOKBT             1
547700020411     C                   CLEAR                   WTAB5F
547800020411     c                   endif
547900020411     c                   endif
548000020412    2C                   ENDSL
548100020412     c* Se non trovata bolla part. nè bolla transito GUARDO SE ESISTE LA
548200020412     c* BOLLA ARRIVI (in un ottica di As unico infatti se la spunta letta
548300020411     c* è un disguido potrei trovare la bolla in arrivo se sull'As in cui
548400020411     c* il collo è stato disguidato risiedono anche i dati del p.o. di
548500020411     c* arrivo del collo)
548600020412    2c     *in36         ifeq      *on
548700020411     c     kbrv4         chain     fnart27l                           36
548800020412    3c     *in36         ifeq      *off
548900020411     C                   MOVE      artlnp        WLNP
549000020411     C                   CLEAR                   WTAB5F
549100020412   x3c                   else
549200020411     c* BOLLA NON TROVATA IN NESSUN ARCHIVIO:
549300020412     c* SPUNTA PARTENZA NON DI DISGUIDO: verifico se è presente
549400020411     c                   exsr      cntspa
549500020412    4c     w105          ifeq      *blanks
549600020412     c* VADO SU TABELLA 5F se non ho trovato nemmeno la spunta partenza
549700020412     C                   movel     fgvlnp        W008A             8
549800020412     C                   move      brvnrs        W005A             5
549900020412     C                   movel     brvlnp        w005a
550000020412     C                   move      w005a         w008a
550100070215     C                   z-add     1             ii                4 0
550200020412     C     w008a         lookup    s5f(ii)                                31
550300020412     C**
550400020412     C* SE NON TROVATO CERCO COME SERIE = 99
550500020412    5C     *in31         ifeq      *off
550600020412     C                   move      99            W008A
550700020412     C                   z-add     1             ii
550800020412     C     w008a         lookup    s5f(ii)                                31
550900020412    5C                   endif
551000020412     C**
551100020412    5C     *in31         ifeq      *on
551200020412     C                   MOVEL     'S'           WTAB5F            1
551300020412     c     ii            occur     dse5f
551400020412     c                   move      e5f(1)        wlnp
551500020412    5c                   endif
551600020411     c*
551700020412    4c                   endif
551800020412    3c                   endif
551900020412    2c                   endif
552000020412    1c                   endif
552100971028     C                   ENDSR
552200971028     C*
552300970827     C*--- CONTROLLO SE C'E' UNA SPUNTA PARTENZA ---------------------*
552400970827     C     CNTSPA        BEGSR
552500970827     C                   MOVE      *BLANKS       W105              1
552600070123     C* Mi salvo dati record di fnbrv prima di leggere
552700070123     C                   MOVEL     DSBRV         DSBRVS
552800140618     C     KBRV03        SETLL     FNBRV05L
552900140618     C     KBRV03        READE(N)  FNBRV05L                               21
553000971028    1C     *IN21         DOWEQ     *OFF
553100960830     C     W105          ANDEQ     *BLANKS
553200971105    2C     BRVFGS        IFNE      FGVLNP
553300970827     C     BRVATR        ANDEQ     ' '
553400960830     C                   MOVE      '1'           W105
553500970827     C                   MOVEL     BRVFGS        WLNP
553600970827     C                   CLEAR                   WTAB5F
553700140618     C     KBRV03        READE(N)  FNBRV05L                               21
553800971028   X2C                   ELSE
553900140618     C     KBRV03        READE(N)  FNBRV05L                               21
554000971028    2C                   ENDIF
554100971028    1C                   ENDDO
554200070123     C* Reimposto campi di fnbrv con dati salvati
554300960830     C                   MOVEL     DSBRVS        DSBRV
554400960830     C                   ENDSR
554500970423     C*
554600130527     C* RICHIAMO FNLS45R PER AGGIORNAMENTO DATE SU BOLLE ------------*
554700970423     C     CALL44        BEGSR
554800070221     c                   clear                   fnls45ds
554900070221     c                   movel     'S'           dls45ric
555000070221     c                   move      brvpes        dls45pes
555100070221     c                   call      'FNLS45R'
555200070221     c                   parm                    fnls45ds
555300070222     c                   parm                    dsbrv
555400970423     C*
555500970509     C                   MOVEL     'S'           WFC44             1
555600970423     C                   ENDSR
555700970423     C*
555800970423     C* IMPOSTO LA KEY PER CHAIN -------------------------------------*
555900970423     C     IMPKEY        BEGSR
556000970423     C                   SELECT
556100970423     C     *IN40         WHENEQ    *ON
556200970423     C                   MOVEL     BLTFLS        KLNP
556300970423     C                   MOVEL     BLTLNA        KLNA
556400970423     C                   MOVEL     BLTNRS        KNRS
556500970423     C                   MOVEL     BLTNSC        KNSC
556600970423     C*
556700970423     C     *IN42         WHENEQ    *ON
556800970423     C                   MOVEL     ANMFLS        KLNP
556900970423     C                   MOVEL     ANMLNA        KLNA
557000970423     C                   MOVEL     ANMNRS        KNRS
557100970423     C                   MOVEL     ANMSCN        KNSC
557200970423     C*
557300970423     C     *IN41         WHENEQ    *ON
557400970423     C                   MOVEL     BTTFLS        KLNP
557500970423     C                   MOVEL     BTTLNA        KLNA
557600970423     C                   MOVEL     BTTNRS        KNRS
557700970423     C                   MOVEL     BTTNSC        KNSC
557800990917     C                   OTHER
557900970423     C                   MOVEL     BRVLNP        KLNP
558000970423     C                   MOVEL     BRVLNA        KLNA
558100970423     C                   MOVEL     BRVNRS        KNRS
558200970423     C                   MOVEL     BRVNSC        KNSC
558300970423     C                   ENDSL
558400970423     C                   ENDSR
558500010309     C*
558600010309     C* Aggiorno e sflaggo file O.R.M. -------------------------------*
558700010309     C     AGGORM        BEGSR
558800010309     C                   MOVEL     'M'           WTRC4
558900060220     C     KAR4          CHAIN     FIAR401L                           20
559000051207    1C     *IN20         IFEQ      *OFF
559100060220     C                   MOVEL     AR4NOT        DSBL4M
559200010309     C     KORM          CHAIN     FNORM01L                           20
559300060220     c* Se il p.o. di ritiro non è uguale alla linea di partenza,
559400051207     c*  aggiorno in questo momento
559500060220    2c                   if        not *in20 and ormpor<>AR4lnp
559600051207     c                   exsr      sr_Storico
559700051207     c* Scrivo fase 200
559800051207     c                   z-add     200           ormfao
559900051207     C                   Z-ADD     FGVDFV        ORMDFO
560000051207     C                   Z-ADD     235858        ORMOFO
560100051207     c                   exsr      rieorf
560200051207     c                   write     fnorf000
560300051207     c
560400060220     c                   z-add     AR4lnp        ormpor
560500070711      * se ho aggiornato il p.o. ritiro dell'ORM devo aggiornare anche FNORG
560600070711     c     korm          chain     fnorg01l
560700070711if  1c                   if        %found(fnorg01l)
560800070711      * cambio il p.o. ritiro
560900070711     c                   eval      orgpor = ormpor
561000070711     c                   update    fnorg000
561100070711    2c                   endif
561200051207    2c                   endif
561300051207     c
561400051207    2c     *IN20         IFEQ      *OFF
561500010312     c     ormfao        andne     900
561600010309     c                   z-add     900           ormfao
561700010309     C                   Z-ADD     FGVDFV        ORMDFO
561800010405     C                   Z-ADD     235959        ORMOFO
561900040909     C                   z-add     dateu         ORMDTT
562000010309     c                   exsr      rieorf
562100070711      * imposto ormeti
562200070711     c                   clear                   dfar
562300070711     c                   clear                   tibs02ds
562400070711     c                   eval      t02mod = 'C'
562500070711     c                   eval      t02sif = knsif
562600070711     c                   eval      t02cod = 'FAR'
562700070711     c                   movel(p)  ormfao        t02ke1
562800070711     c                   call      'TIBS02R'
562900070711     c                   parm                    kpjba
563000070711     c                   parm                    tibs02ds
563100070711     c                   eval      dfar = t02uni
563200070711     c                   eval      ormeti = d§farass
563300010309     c                   write     fnorf000
563400051207    2C                   ENDIF
563500051207    c
563600051207     C  n20              UPDATE    FNORM000
563700051207    1C                   ENDIF
563800010309     C                   ENDSR
563900010309     C*
564000010309     C* Riempio campi record fase O.R.M. -------------------------------*
564100010309     C     RIEORF        BEGSR
564200010309     C                   clear                   fnorf000
564300010309     C                   eval      ORFpoe = ormpoe
564400010309     C                   eval      ORFnsr = ormnsr
564500010309     C                   eval      ORFnor = ormnor
564600010309     C                   eval      ORFnrv = ormnrv
564700010309     C                   eval      ORFpog = ormpor
564800010309     C                   eval      ORFdae = ormdfo
564900010309     C                   eval      ORFore = ormofo
565000010309     C                   eval      ORFfar = ormfao
565100040909     C                   eval      ORFpue = knmus
565200040909     c                   If        OrmNdc > *Zeros
565300060516      * devo trovare il p.o. gestione distinta giusto
565400120607     c                   move      ormnpg        kdstnpg
565500120607     c                   move      ormndc        kdstnfv
565600060516      * prima provo con il p.o. ritiro
565700060516     c                   eval      kdstfgs = ormpor
565800120607     c     kdst          chain     fidst01l
565900060516      * se non lo trovo cerco se il p.o. ritiro è gestito
566000120607     c                   if        not %found(fidst01l)
566100060516     c                   clear                   dslv55
566200060516     c                   eval      d55tpt = '6'
566300060516     c                   move      ormpor        d55lin
566400060516     c                   eval      d55drf = ormddc
566500060516     c                   call      'FNLV55R'
566600060516     c                   parm                    dslv55
566700060516     c                   if        d55err = *blanks and d55tfa <> d55lin
566800060516     c                   move      d55tfa        kdstfgs
566900120607     c     kdst          chain     fidst01l
567000060516      * se non trovo neanche con il p.o. che gestisce metto il p.o. ritiro
567100060516      * teoricamente non dovrebbe succedere
567200120607     c                   if        not %found(fidst01l)
567300060516     c                   eval      kdstfgs = ormpor
567400060516     c                   endif
567500060516     c                   endif
567600060516     c                   endif
567700060516     c**!!!              Eval      OrfFgs = OrmPor
567800060516     c                   eval      orffgs = kdstfgs
567900040909     c                   Eval      OrfNdc = OrmNdc
568000040909     c                   Eval      OrfDdc = OrmDdc
568100040909     c                   EndIf
568200010309
568300010309     c                   ENDSR
568400051207     C*
568500051207     C* Scrivo lo storico orm ------------------------------------------*
568600051207     C     SR_Storico    BEGSR
568700051207     C                   clear                   fnorv000
568800051207     C                   eval      ORVpoe = ORMpoe
568900051207     C                   eval      ORVnsr = ORMnsr
569000051207     C                   eval      ORVnor = ORMnor
569100051207     C                   eval      ORVnrv = ORMnrv
569200051207     C                   eval      ORVtor = ORMtor
569300051207     C                   eval      ORVcor = ORMcor
569400051207     C                   eval      ORVrso = ORMrso
569500051207     C                   eval      ORVino = ORMino
569600051207     C                   eval      ORVcao = ORMcao
569700051207     C                   eval      ORVloo = ORMloo
569800051207     C                   eval      ORVpro = ORMpro
569900051207     C                   eval      ORVnao = ORMnao
570000051207     C                   eval      ORVcra = ORMcra
570100051207     C                   eval      ORVrsr = ORMrsr
570200051207     C                   eval      ORVinr = ORMinr
570300051207     C                   eval      ORVcar = ORMcar
570400051207     C                   eval      ORVlor = ORMlor
570500051207     C                   eval      ORVprr = ORMprr
570600051207     C                   eval      ORVnar = ORMnar
570700051207     C                   eval      ORVrer = ORMrer
570800051207     C                   eval      ORVter = ORMter
570900051207     C                   eval      ORVcrc = ORMcrc
571000051207     C                   eval      ORVrsc = ORMrsc
571100051207     C                   eval      ORVinc = ORMinc
571200051207     C                   eval      ORVloc = ORMloc
571300051207     C                   eval      ORVcac = ORMcac
571400051207     C                   eval      ORVprc = ORMprc
571500051207     C                   eval      ORVnac = ORMnac
571600051207     C                   eval      ORVffd = ORMffd
571700051207     C                   eval      ORVdar = ORMdar
571800051207     C                   eval      ORVorr = ORMorr
571900051207     C                   eval      ORVnam = ORMnam
572000051207     C                   eval      ORVncl = ORMncl
572100051207     C                   eval      ORVpkg = ORMpkg
572200051207     C                   eval      ORVvlm = ORMvlm
572300051207     C                   eval      ORVbnc = ORMbnc
572400051207     C                   eval      ORVblc = ORMblc
572500051207     C                   eval      ORVatt = ORMatt
572600051207     C                   eval      ORVmtc = ORMmtc
572700051207     C                   eval      ORVpag = ORMpag
572800051207     C                   eval      ORVksc = ORMksc
572900051207     C                   eval      ORVctr = ORMctr
573000051207     C                   eval      ORVddt = ORMddt
573100051207     C                   eval      ORVpor = ORMpor
573200051207     C                   eval      ORVzor = ORMzor
573300051207     C                   eval      ORVpoc = ORMpoc
573400051207     C                   eval      ORVno1 = ORMno1
573500051207     C                   eval      ORVno2 = ORMno2
573600051207     C                   eval      ORVcst = ORMcst
573700051207     C                   eval      ORVvcs = ORMvcs
573800051207     C                   eval      ORVfcs = ORMfcs
573900051207     C                   eval      ORVdfo = ORMdfo
574000051207     C                   eval      ORVofo = ORMofo
574100051207     C                   eval      ORVfao = ORMfao
574200051207     C                   eval      ORVsto = ORMsto
574300051207     C                   eval      ORVdst = ORMdst
574400051207     C                   eval      ORVnpg = ORMnpg
574500051207     C                   eval      ORVndc = ORMndc
574600051207     C                   eval      ORVddc = ORMddc
574700051207     C                   eval      ORVstp = ORMstp
574800051207     C                   eval      ORVrfa = ORMrfa
574900051207     C                   eval      ORVsto = ORMsto
575000070711     c                   eval      orveti = ormeti
575100051207     C                   eval      ORVspi = ORMspi
575200051207     C                   eval      ORVflo = ORMflo
575300051207     C                   movel     knmus         ORVutv
575400051207     C                   TIME                    W0140            14 0
575500051207     C                   MOVEL     W0140         orvorv
575600051207     C                   CLEAR                   WLBDAT
575700051207     C                   MOVE      W0140         G02DAT
575800051207     C                   CALL      'XSRDA8'
575900051207     C                   PARM                    WLBDAT
576000051207     C                   Z-ADD     G02INV        orvdtv
576100051207     C                   Z-ADD     G02INV        orvdtt
576200051207     C                   write     fnorv000
576300051207
576400051207     c                   ENDSR
576500950109     C*---------------------------------------------------------------*
576600950109     C*    OPERAZIONI INIZIALI                                        *
576700950109     C*---------------------------------------------------------------*
576800950109     C     *INZSR        BEGSR
576900950109     C*
577000950109     C     *ENTRY        PLIST
577100950109     C                   PARM                    KPJBA
577200130524     C                   MOVEL     KPJBU         FNLSU2DS
577300021014
577400021014     c                   Movel     Simfel        Varx1
577500021014     c                   Movel     Simfel        Var2x1
577600021014     c                   Movel     Simfel        Num31
577700021015     c                   Move      Simfel        Mbrfil4
577800021014
577900000111     C* SPOSTO SUBITO NUMERI FOGLIO IN CAMPI LUNGHI 6,0
578000000111     C                   Z-ADD     FGV2          FGV260
578100000111     C                   Z-ADD     NFV1          NFV160
578200950109     C*
578300950109     C                   Z-ADD     1             CODUT             1 0
578400950109     C                   CALL      'X§PARUT'
578500950109     C                   PARM                    UT§DSE
578600000428     C** S.I. IN CUI SONO
578700021010     C                   CLEAR                   DSBS55
578800000428     C                   MOVEL     KNSIF         I50PSI
578900000428     C                   MOVEL     'L'           I50TLA
579000021010     C                   CALL      'TIBS55R'
579100021010     C                   PARM                    DSBS55
579200950109     C**
579300950109     C*    CHIAVI                                                     *
579400950109     C**
579500950109     C     KBLP2         KLIST
579600950109     C                   KFLD                    BLPAAS
579700950109     C                   KFLD                    BLPLNP
579800950109     C                   KFLD                    BLPNRS
579900950109     C                   KFLD                    BLPNSP
580000060220     C     KAR4          KLIST
580100010309     C                   KFLD                    BLPAAS
580200010309     C                   KFLD                    BLPLNP
580300010309     C                   KFLD                    BLPNRS
580400010309     C                   KFLD                    BLPNSP
580500010309     C                   KFLD                    WTRC4
580600950516     C     KBTP2         KLIST
580700020606     c                   kfld                    btpflp
580800950516     C                   KFLD                    BTPAAS
580900950516     C                   KFLD                    BTPLNP
581000950516     C                   KFLD                    BTPNRS
581100950516     C                   KFLD                    BTPNSP
581200950109     C     KBLT3         KLIST
581300950109     C                   KFLD                    BLTAAS
581400950109     C                   KFLD                    BLTLNP
581500950109     C                   KFLD                    BLTNRS
581600950109     C                   KFLD                    BLTNSP
581700950109     C     KBTT3         KLIST
581800020605     c                   kfld                    fgvlnp
581900950109     C                   KFLD                    BTTAAS
582000950109     C                   KFLD                    BTTLNP
582100950109     C                   KFLD                    BTTNRS
582200950109     C                   KFLD                    BTTNSP
582300950109     C     KTAB          KLIST
582400950109     C                   KFLD                    CODUT
582500950109     C                   KFLD                    COD               2
582600950109     C                   KFLD                    KEY               8
582700950109     C     KTAB2         KLIST
582800950109     C                   KFLD                    CODUT
582900950109     C                   KFLD                    COD
583000950109     C     KBLT2         KLIST
583100950109     C                   KFLD                    WNPG
583200950109     C                   KFLD                    BLTFLS
583300950109     C                   KFLD                    BLTLNA
583400950109     C                   KFLD                    BLTNRS
583500950113     C                   KFLD                    BLTNSC
583600971107     C                   KFLD                    FGVLNP
583700130226     C     KBLT_xco      KLIST
583800130226     C                   KFLD                    WNPG
583900130226     C                   KFLD                    BLTFLS
584000130226     C                   KFLD                    BLTLNA
584100130226     C                   KFLD                    BLTNRS
584200130226     C                   KFLD                    BLTNSC
584300950109     C     KBTT          KLIST
584400950109     C                   KFLD                    WNPG
584500950109     C                   KFLD                    BTTFLS
584600950109     C                   KFLD                    BTTLNA
584700950109     C                   KFLD                    BTTNRS
584800950113     C                   KFLD                    BTTNSC
584900950915     C     KFVV          KLIST
585000950915     C                   KFLD                    WNPG5
585100950915     C                   KFLD                    WDFV
585200020916     C                   KFLD                    Kfgs
585300000728     C     KFVV1         KLIST
585400000728     C                   KFLD                    BRVNPG
585500001123     C                   KFLD                    WNFV05
585600000728     C                   KFLD                    BRVFGS
585700950109     C     KBRC2         KLIST
585800950109     C                   KFLD                    WNPG
585900000111     C                   KFLD                    FGV260
586000971107     C                   KFLD                    FGVLNP
586100950109     C                   KFLD                    BRVLNA
586200130527     C     K03BLP77      klist
586300020807     C                   kfld                    SIMfel
586400960212     C                   KFLD                    FGV2
586500960212     C                   KFLD                    BRVLNA
586600130531     C     Kblp77        KLIST
586700130531     C                   KFLD                    simfel
586800130531     C                   KFLD                    kfgv
586900130531     C                   KFLD                    savffv
587000130527     C     K02BLP77_9    KLIST
587100020807     C                   KFLD                    SIMfel
587200020807     C                   KFLD                    COMnfv
587300950109     C     KFVD2         KLIST
587400950109     C                   KFLD                    FGVLNP
587500950109     C                   KFLD                    FGV2
587600950109     C                   KFLD                    BRVLNA
587700950110     C                   KFLD                    WTRC
587800130530     C     KFVDbor       KLIST
587900130530     C                   KFLD                    SIMFEL
588000130530     C                   KFLD                    WNFV5
588100130530     C                   KFLD                    BLPLNA
588200130530     C                   KFLD                    WTRC
588300060707     C     KFV4e         KLIST
588400060707     C                   KFLD                    FV4LNP
588500060707     C                   KFLD                    FV4NFV
588600060707     C                   KFLD                    FV4EPA
588700060707     C                   KFLD                    FV4LAI
588800060707     C                   KFLD                    FV4TRC
588900011127     C*
589000011127     C     KTBE          KLIST
589100011127     C                   KFLD                    KCOD1
589200011127     C                   KFLD                    KKE1
589300130226     C     KTBE2         KLIST
589400130226     C                   KFLD                    KCOD1
589500130226     C                   KFLD                    KKE1
589600130226     C                   KFLD                    KKE2
589700130226     c
589800011127     C     *LIKE         DEFINE    TBECOD        KCOD1
589900011127     C     *LIKE         DEFINE    TBEKE1        KKE1
590000130226     C     *LIKE         DEFINE    TBEKE2        KKE2
590100130226     c
590200950109     C     KFTT          KLIST
590300950109     C                   KFLD                    WPDR
590400950109     C                   KFLD                    COMTSR
590500950109     C                   KFLD                    WNDC
590600950113     C                   KFLD                    WDRT
590700950109     C     KFTD          KLIST
590800950109     C                   KFLD                    COMPDR
590900950109     C                   KFLD                    COMTSR
591000950109     C                   KFLD                    COMNDC
591100950114     C                   KFLD                    COMDDC
591200950109     C                   KFLD                    CO2KSC
591300950113     C                   KFLD                    COMRSC
591400020416     C     KFTD1         KLIST
591500020416     C                   KFLD                    COMPDR
591600020416     C                   KFLD                    COMTSR
591700020416     C                   KFLD                    COMNDC
591800020416     C                   KFLD                    COMDDC
591900950109     C     KBLT          KLIST
592000021021     C                   KFLD                    wlnp              3 0
592100021021     C                   KFLD                    DFV               8 0
592200021021     C                   KFLD                    COMFIL            3 0
592300000731     C     KBLT1         KLIST
592400000731     C                   KFLD                    BLTAAS
592500000731     C                   KFLD                    BLTLNP
592600000731     C                   KFLD                    BLTNRS
592700000731     C                   KFLD                    BLTNSP
592800000731     C                   KFLD                    BLTNSC
592900000714     C     KBLTL         KLIST
593000000714     C                   KFLD                    DFV
593100000714     C                   KFLD                    COMFIL
593200000714     C                   KFLD                    BLTLNP
593300960704     C     KBLT27        KLIST
593400960704     C                   KFLD                    ANMFLS
593500960704     C                   KFLD                    ANMLNA
593600960704     C                   KFLD                    ANMNRS
593700960704     C                   KFLD                    ANMSCN
593800950109     C     KANM          KLIST
593900021021     C                   KFLD                    wlnp
594000021021     C                   KFLD                    COMCAA
594100950109     C                   KFLD                    COMFIL
594200040422     C     KANM02        KLIST
594300040422     C                   KFLD                    bttFLS
594400040422     C                   KFLD                    bttLNA
594500040422     C                   KFLD                    bttNRS
594600040422     C                   KFLD                    bttnsc
594700040422     C                   KFLD                    COMCAA            3 0
594800950418     C     KANM3         KLIST
594900950418     C                   KFLD                    BRVLNP
595000950418     C                   KFLD                    BRVLNA
595100950418     C                   KFLD                    BRVNRS
595200950418     C                   KFLD                    BRVNSC
595300950418     C                   KFLD                    COMCAA            3 0
595400020412     C     KANM31        KLIST
595500020412     C                   KFLD                    ANMFLS
595600020412     C                   KFLD                    ANMLNA
595700020412     C                   KFLD                    ANMNRS
595800020412     C                   KFLD                    ANMSCN
595900020412     C                   KFLD                    COMCAA            3 0
596000130527     C     KANM1         KLIST
596100130527     C                   KFLD                    BLTFLS
596200130527     C                   KFLD                    BLTLNA
596300130527     C                   KFLD                    BLTNRS
596400130527     C                   KFLD                    BLTNSC
596500130607     C                   KFLD                    comcaa
596600151118     C     KANM01        KLIST
596700151118     C                   KFLD                    BLpaas
596800151118     C                   KFLD                    BLplnp
596900151118     C                   KFLD                    BLpNRS
597000151118     C                   KFLD                    BLpNSp
597100151118     C                   KFLD                    comcaa
597200950109     C     KANM5         KLIST
597300950109     C                   KFLD                    ANMFLS
597400950109     C                   KFLD                    ANMLNA
597500950109     C                   KFLD                    ANMNRS
597600950109     C                   KFLD                    ANMSCN
597700971107     C                   KFLD                    FGVLNP
597800950109     C     KBRV          KLIST
597900950109     C                   KFLD                    WNPG
598000000111     C                   KFLD                    FGV260
598100971107     C                   KFLD                    FGVLNP
598200130527     C     KBRV1         KLIST
598300130527     C                   KFLD                    KNPG
598400130527     C                   KFLD                    BLTFLS
598500130527     C                   KFLD                    BLTLNA
598600130527     C                   KFLD                    BLTNRS
598700130527     C                   KFLD                    BLTNSC
598800130527     C                   KFLD                    anmfle
598900130530     C     KBRVbor       KLIST
599000130530     C                   KFLD                    KNPG
599100130530     C                   KFLD                    WNFV60
599200130530     C                   KFLD                    KFGS
599300130530     C                   KFLD                    BLPFLS
599400130530     C                   KFLD                    blplna
599500130530     C                   KFLD                    BLPNRS
599600130530     C                   KFLD                    BLTNSC
599700950418     C     KBRV2         KLIST
599800950418     C                   KFLD                    WNPG
599900950418     C                   KFLD                    WSANFV
600000950418     C                   KFLD                    WSAFGS
600100950418     C                   KFLD                    KLNA
600200950418     C                   KFLD                    KLNP
600300950418     C                   KFLD                    KNRS
600400950418     C                   KFLD                    KNSC
600500980209     C     KBRV01        KLIST
600600980209     C                   KFLD                    NPG1
600700000111     C                   KFLD                    NFV160
600800980209     C                   KFLD                    FGS1
600900980209     C                   KFLD                    KLNP
601000980209     C                   KFLD                    KLNA
601100980209     C                   KFLD                    KNRS
601200980209     C                   KFLD                    KNSC
601300960830     C     KBRV03        KLIST
601400960830     C                   KFLD                    WNPG
601500960830     C                   KFLD                    BRVLNP
601600960830     C                   KFLD                    BRVLNA
601700960830     C                   KFLD                    BRVNRS
601800960830     C                   KFLD                    BRVNSC
601900950109     C     KBRV3         KLIST
602000950109     C                   KFLD                    NPG1
602100000111     C                   KFLD                    NFV160
602200950109     C                   KFLD                    FGS1
602300950109     C                   KFLD                    COMFIL
602400950915     C     KBRV33        KLIST
602500970423     C                   KFLD                    KVVNPG
602600970423     C                   KFLD                    KVVNFV
602700970423     C                   KFLD                    KVVFGS
602800950915     C                   KFLD                    COMFIL
602900990917     C     KBRV32        KLIST
603000990917     C                   KFLD                    KVVNPG
603100990917     C                   KFLD                    KVVNFV
603200990917     C                   KFLD                    KVVFGS
603300950109     C     KBRV4         KLIST
603400950109     C                   KFLD                    KLNP
603500950109     C                   KFLD                    KLNA
603600950109     C                   KFLD                    KNRS
603700950109     C                   KFLD                    KNSC
603800950118     C     KBRV5         KLIST
603900950109     C                   KFLD                    WNPG
604000950109     C                   KFLD                    KLNP
604100950109     C                   KFLD                    KLNA
604200950109     C                   KFLD                    KNRS
604300950109     C                   KFLD                    KNSC
604400971107     C                   KFLD                    FGVLNP
604500000731     C     KBRV55        KLIST
604600000731     C                   KFLD                    WNPG
604700000731     C                   KFLD                    KLNP
604800000731     C                   KFLD                    KLNA
604900000731     C                   KFLD                    KNRS
605000000731     C                   KFLD                    KNSC
605100040802     C                   KFLD                    WFGSspu
605200950118     C     KBRV6         KLIST
605300950118     C                   KFLD                    KLNP
605400950118     C                   KFLD                    KLNA
605500950118     C                   KFLD                    KNRS
605600950118     C                   KFLD                    KNSC
605700971107     C                   KFLD                    FGVLNP
605800150603     C     KBRV7         KLIST
605900150603     C                   KFLD                    BLTFLS
606000150603     C                   KFLD                    BLTLNA
606100150603     C                   KFLD                    BLTNRS
606200150603     C                   KFLD                    BLTNSC
606300150924     C*****              KFLD                    FGVLNP
606400020807     c     K02FGV01      klist
606500020807     c                   kfld                    fgv2
606600020807     c                   kfld                    SIMfel
606700010309     c*
606800010309     c     korm          klist
606900010309     c                   kfld                    §b4poe
607000010309     c                   kfld                    §b4nsr
607100010309     c                   kfld                    §b4nor
607200010309     c                   kfld                    §b4nrv
607300010611     c     kbrve         klist
607400010611     c                   kfld                    brvnpg
607500010611     c                   kfld                    brvnfv
607600010611     c                   kfld                    brvfgs
607700010611     c                   kfld                    brvlnp
607800010611     c                   kfld                    brvlna
607900010611     c                   kfld                    brvnrs
608000010611     c                   kfld                    brvnsc
608100020605     C     KBtt38        KLIST
608200020605     c                   kfld                    fgvlnp
608300020605     C                   KFLD                    DFV               8 0
608400020605     C                   KFLD                    COMFIL            3 0
608500020606     C     KBtt37        KLIST
608600020606     c                   kfld                    fgvlnp
608700020606     C                   KFLD                    KLNP
608800020606     C                   KFLD                    KLNA
608900020606     C                   KFLD                    KNRS
609000020606     C                   KFLD                    KNSC
609100040319     C     KBTT1         KLIST
609200040319     c                   kfld                    BTTFLP
609300040319     C                   KFLD                    BTTAAS
609400040319     C                   KFLD                    BTTLNP
609500040319     C                   KFLD                    BTTNRS
609600040319     C                   KFLD                    BTTNSP
609700040319     C                   KFLD                    BTTNSC
609800040310     c     Kar5          klist
609900040311     c                   kfld                    bltaas
610000040311     C                   KFLD                    bltlnp
610100040311     C                   KFLD                    bltnrs
610200040311     C                   KFLD                    bltnsp
610300040310     C                   KFLD                    ktrd
610400040310     c                   movel     'GEN'         ktrd
610500060516
610600060516     c     kdst          klist
610700120607     c                   kfld                    kdstnpg
610800120607     c                   kfld                    kdstnfv
610900060516     c                   kfld                    kdstfgs
611000060516
611100950118     C***
611200950118     C*  CODICI TRATTAMENTO MERCE CON §1BFG5 = "N"
611300950118     C***
611400960202     C* NON CARICO SE RICHIAMATO DA MANUTENZIONE
611500960202     C     PA2FLG        IFNE      'M'
611600960202     C* APERTURA FILE
611700960202     C                   EXSR      OPENF
611800130530     c
611900960202     C**
612000950118     C                   Z-ADD     1             X
612100950118     C                   MOVEL     '1B'          COD
612200950118     C     KTAB2         CHAIN     TABEL00F                           31
612300950118     C*
612400950118     C     *IN31         DOWEQ     *OFF
612500950118     C                   MOVEL     TBLUNI        DS1B
612600950118     C     §1BFG5        IFEQ      'N'
612700950118     C                   MOVEL     TBLKEY        CTM(X)
612800950118     C                   ADD       1             X
612900950118     C                   ENDIF
613000950118     C     KTAB2         READE     TABEL00F                               31
613100950118     C                   ENDDO
613200050524     c
613300050524     c* Controllo riempimento schiera
613400050524     c                   clear                   trul0sds
613500050524     c                   eval      i0sski='CTM'
613600050524     c                   eval      i0sele=%elem(ctm)
613700050524     c                   eval      i0spie=x
613800050524     c                   eval      i0scod='  1B '
613900050524     c                   eval      i0sfile='TABEL00F  '
614000050524     c                   eval      i0ssif=knsif
614100130718     c                   eval      i0spgm='FNLSU2R'
614200050524     c                   movel     trul0sds      kpjbu
614300050524     c                   call      'TRUL0SR'
614400050524     c                   parm                    kpjba
614500960531     C***
614600960531     C** TABELLA 5F - FILIALI CONFERMA BOLLE DA DISK
614700960531     C***
614800960531     C                   Z-ADD     0             X
614900960531     C                   MOVEL     '5F'          COD
615000960531     C     KTAB2         SETLL     TABEL
615100960531     C     KTAB2         READE     TABEL                                  31
615200960531     C*
615300960531    1C     *IN31         DOWEQ     '0'
615400960531    2C     TBLFLG        IFEQ      ' '
615500050524     c* elaboro soltanto i record con le prima 3 cifre <>'   '
615600050524     c*  perchè nella 5F ci sono 2 diversi tipi record: quello con i '   '
615700050524     c*   davanti qui non sono usati
615800050524     c                   movel     tblkey        w003a             3
615900050524     c                   if        w003a<>'   '
616000960531     C                   ADD       1             X
616100960531     C                   MOVEL     TBLKEY        S5F(X)
616200960531     C     X             OCCUR     DSE5F
616300960701     C                   MOVEA     TBLUNI        E5F
616400080109      * - Carica comunque il n° max di elementi previsti nella schiera.
616500080109      *   Se superato il limite massimo di riempimento, verrà inviato
616600080109      *   un messaggio di avvertimento - vedi chiamata al pgm TRUL0SR:
616700080109      *   verificherà lui la percentuale di riempimento...).
616800080109     C     X             IFGE      $MaxElem
616900960531     C                   SETON                                        31
617000960531     C                   ENDIF
617100050524     C                   ENDIF
617200960531    2C                   ENDIF
617300960531     C*
617400960531     C  N31KTAB2         READE     TABEL                                  31
617500960531    1C                   ENDDO
617600050524     c**
617700050524     c                   clear                   trul0sds
617800050524     c                   eval      i0sski='S5F'
617900050524     c                   eval      i0sele=%elem(s5f)
618000050524     c                   eval      i0spie=x
618100050524     c                   eval      i0scod='  5F '
618200050524     c                   eval      i0sfile='TABEL00F  '
618300050524     c                   eval      i0ssif=knsif
618400130718     c                   eval      i0spgm='FNLSU2R'
618500050524     c                   movel     trul0sds      kpjbu
618600050524     c                   call      'TRUL0SR'
618700050524     c                   parm                    kpjba
618800950118     C***
618900950118     C*  CODICI PISTOLA CON PISTOLA AUTOMATICA
619000950118     C***
619100950118     C                   Z-ADD     1             X
619200970818     C                   Z-ADD     1             Y                 3 0
619300950118     C                   MOVEL     '7J'          COD
619400950118     C     KTAB2         CHAIN     TABEL00F                           31
619500950118     C*
619600950118     C     *IN31         DOWEQ     *OFF
619700950118     C                   MOVEL     TBLUNI        DS7J
619800950118     C     §7JRPS        IFEQ      'A'
619900950118     C                   MOVEL     TBLKEY        NPS(X)
620000950118     C                   ADD       1             X
620100950118     C                   ENDIF
620200970818     C     §7JFSL        IFEQ      'F'
620300970818     C                   MOVEL     TBLKEY        NPF(Y)
620400970818     C                   ADD       1             Y
620500970818     C                   ENDIF
620600950118     C     KTAB2         READE     TABEL00F                               31
620700950118     C                   ENDDO
620800011127     C*
620900011127     C*  tabelle GEDCN x reperimento divisa conteggi padroncini/tariffe
621000011127     C                   movel     'GED'         kcod1
621100011127     C                   movel     *blanks       kke1
621200011127     C                   movel     'CN'          kke1
621300011127     C     ktbe          chain     tntbe01l                           31
621400011127     C                   clear                   dgedcn
621500011127     C  N31              movel     tbeuni        dgedcn
621600130227     c
621700121119     C*  Tabella DGD    x reperimento data dal/al per conferma autoamti
621800041216     c*                 prepagati
621900041217     c                   Clear                   Tibs02Ds
622000121119     c                   Clear                   ddgd
622100041217     c                   Eval      T02Mod = 'C'
622200041217     c                   Eval      T02Sif = Knsif
622300121119     c                   Eval      T02Cod = 'DGD'
622400121119     c                   Movel(p)  '1'           T02Ke1
622500041217     c                   Call      'TIBS02R'
622600041217     c                   Parm                    Kpjba
622700041217     c                   Parm                    Tibs02Ds
622800041217     c                   If        T02Err = *Blanks
622900121119     c                   Movel     T02Uni        ddgd
623000041217     c                   EndIf
623100000619     C*
623200060710     C*  tipi record di FV4 da trasmettere in sede
623300060710     C***
623400060710     C                   Z-ADD     1             X
623500060710     C                   MOVEL     '3B'          COD
623600060710     C     KTAB2         CHAIN     TABEL00F                           31
623700060710     C*
623800060710     C     *IN31         DOWEQ     *OFF
623900060710     c                   if        tblflg=' '
624000060710     C                   MOVEL     TBLUNI        DS3B
624100060710     C     §3btrs        IFEQ      'S'
624200060710     C                   MOVEL     TBLKEY        fv4S(X)
624300060710     C                   ADD       1             X
624400060710     C                   ENDIF
624500060710     C                   ENDIF
624600060710     C     KTAB2         READE     TABEL00F                               31
624700060710     C                   ENDDO
624800950109     C***
624900950109     C*  R I C E R C A   C A P O C O N T I
625000950109     C***
625100040826     C                   DO        50            X                 4 0
625200950109     C                   MOVE      TCU(X)        TCUDS
625300950109     C     F56           CABNE     'CG'          END1
625400950109     C     F4            COMP      '1'                                    21
625500950109     C     F4            COMP      '2'                                    22
625600950109     C     F4            COMP      '3'                                    23
625700950109     C     F4            COMP      '6'                                    27
625800950109     C** 1 CLIENTI   21
625900950109     C** 2 FORNITORI 22
626000950109     C** 3 AGENTI    23
626100950109     C     F3            COMP      '0'                                242425
626200950109     C     F3            COMP      'I'                                    26
626300950109     C** 0 ITALIA   25
626400950109     C** 1 ESTERO   24
626500950109     ** I CAPO CONTO IVA
626600950109     C   21
626700950109     CAN 24              Z-ADD     KCU(X)        KCE               4 0
626800950109     C   21
626900950109     CAN 25              Z-ADD     KCU(X)        KCI               4 0
627000950109     C   22
627100950109     CAN 24              Z-ADD     KCU(X)        KFE               4 0
627200950109     C   22
627300950109     CAN 25              Z-ADD     KCU(X)        KFI               4 0
627400950109     C   23
627500950109     CAN 24              Z-ADD     KCU(X)        KAE               4 0
627600950109     C   23
627700950109     CAN 25              Z-ADD     KCU(X)        KAI               4 0
627800950109     C   26              Z-ADD     KCU(X)        KIVA              4 0
627900950109     C   27              Z-ADD     KCU(X)        KBNA              4 0
628000950109     C     END1          TAG
628100950109     C                   END
628200960202     C**
628300960202     C                   ENDIF
628400950109     C*
628500950109     C* SPENGO GLI INDICATORI USATI PER LA RICERCA CAPOCONTI
628600950109     C                   SETOFF                                       212223
628700950109     C                   SETOFF                                       242526
628800950515     C                   SETOFF                                       273132
628900950109     C*
629000950109     C* DEFINIZIONE CAMPI
629100970423     C     *LIKE         DEFINE    BRVNPS        WNPS
629200970528     C     *LIKE         DEFINE    BRVNPS        SPUNPS
629300970423     C     *LIKE         DEFINE    FVVNPG        KVVNPG
629400000111     C     *LIKE         DEFINE    BRVNFV        KVVNFV
629500001121     C     *LIKE         DEFINE    BRVNFV        SAVNFV
629600001123     C     *LIKE         DEFINE    fvvnfv        wnfv05
629700970423     C     *LIKE         DEFINE    FVVFGS        KVVFGS
629800970423     C**
629900970423     C     *LIKE         DEFINE    FVDCLA        NCLU
630000950109     C     *LIKE         DEFINE    FVDKFA        PKGU
630100950113     C     *LIKE         DEFINE    FVDVL1        VOLU
630200950113     C     *LIKE         DEFINE    FVDSNA        WSPEN
630300130530     C     *LIKE         DEFINE    FVDSNA        WSPE
630400950113     C     *LIKE         DEFINE    FVDSNA        WSPET
630500950110     C     *LIKE         DEFINE    FVDTRC        WTRC
630600950109     C*
630700950109     C     *LIKE         DEFINE    FTTPDR        COMPDR
630800950109     C     *LIKE         DEFINE    FTTPDR        WPDR
630900950109     C     *LIKE         DEFINE    FTTTSR        COMTSR
631000950109     C     *LIKE         DEFINE    FTTNDC        COMNDC
631100950109     C     *LIKE         DEFINE    FTTNDC        WNDC
631200950109     C     *LIKE         DEFINE    FTTSET        COMSET
631300020326     C     *LIKE         DEFINE    FTDstp        wstp
631400950109     C     *LIKE         DEFINE    FTDKSC        CO2KSC
631500950113     C     *LIKE         DEFINE    FTDRSC        COMRSC
631600021111     C     *LIKE         DEFINE    FTDVLU        WFVDVOL
631700021111     C     *LIKE         DEFINE    FTDVLU        WVOL
631800021108     C     *LIKE         DEFINE    FTDpkl        Wpes
631900021111     C     *LIKE         DEFINE    FTDpkl        WFVDPES
632000950114     C     *LIKE         DEFINE    FTDDDC        COMDDC
632100950109     C*
632200960124     C     *LIKE         DEFINE    FGVLNP        SA2LNP
632300960124     C     *LIKE         DEFINE    FGVNFV        SA2NFV
632400000731     C     *LIKE         DEFINE    FGVLNP        WFGSFV
632500960124     C     *LIKE         DEFINE    FVARTP        SA2RTP
632600960124     C     *LIKE         DEFINE    FGVDFV        WDTP
632700950915     C     *LIKE         DEFINE    FGVDFV        WDFV
632800950113     C     *LIKE         DEFINE    BRVNPG        WNPG
632900950915     C     *LIKE         DEFINE    BRVNPG        WNPG5
633000950109     C     *LIKE         DEFINE    BRVZNC        WRVZNC
633100950109     C     *LIKE         DEFINE    BRVVUC        WRVVUC
633200990906     C     *LIKE         DEFINE    BRVPUC        WRVPUC
633300950109     C     *LIKE         DEFINE    BRVCAN        WRVCAN
633400950915     C     *LIKE         DEFINE    BRVNPS        WRVNPS
633500950109     C     *LIKE         DEFINE    BRVLNP        KLNP
633600950109     C     *LIKE         DEFINE    BRVLNA        KLNA
633700950109     C     *LIKE         DEFINE    BRVNRS        KNRS
633800950109     C     *LIKE         DEFINE    BRVNSC        KNSC
633900950418     C     *LIKE         DEFINE    BRVFGS        WSAFGS
634000950418     C     *LIKE         DEFINE    BRVNFV        WSANFV
634100000111     C     *LIKE         DEFINE    BRVNFV        FGV260
634200000111     C     *LIKE         DEFINE    BRVNFV        NFV160
634300960604     C     *LIKE         DEFINE    BRVLNP        WLNP
634400960905     C     *LIKE         DEFINE    BRVNPG        NPGP
634500960905     C     *LIKE         DEFINE    BRVNFV        NFVP
634600960905     C     *LIKE         DEFINE    BRVLNP        LNPP
634700960905     C     *LIKE         DEFINE    BRVLNA        LNAP
634800960905     C     *LIKE         DEFINE    BRVNRS        NRSP
634900960905     C     *LIKE         DEFINE    BRVNSC        NSCP
635000960905     C     *LIKE         DEFINE    BRVNPS        NPSP
635100960905     C     *LIKE         DEFINE    BRVVUC        VUCP
635200990623     C     *LIKE         DEFINE    BRVPUC        PUCP
635300950109     C     *LIKE         DEFINE    BLPCBO        SAVCBO
635400950623     C*
635500980210     C     *LIKE         DEFINE    D55TFA        LNATFA
635600971107     C     *LIKE         DEFINE    D55TFP        LNAFEL
635700950707     C     *LIKE         DEFINE    BTTDUT        WDTPAR
635800970703     C     *LIKE         DEFINE    D55LIN        WLIN
635900060220     C     *LIKE         DEFINE    AR4TRC        WTRC4
636000980126     C*
636100980126     C     *LIKE         DEFINE    BRVLNA        WLNA
636200980209     C     *LIKE         DEFINE    DSBRV         SVBRVE
636300070123     C     *LIKE         DEFINE    DSBRV         dsbrvs
636400990623     C                   Z-ADD     0             COMDFS            8 0
636500990623     C                   Z-ADD     0             COMHMS            4 0
636600950621     C*
636700150609      * verifico se presenti simfel o suoi secondi livello
636800150609      * nel file della sospensione automatica per colli di clienti
636900150609      * che partono da più filiali
637000150609     c                   eval      w_FilBsp=*blanks
637100151215     c                   eval      w_FilBsps=*blanks
637200151215     c                   clear                   w_SiBsp
637300151215     c                   clear                   w_bsptip
637400150610     c     'S'           setll     fibsp03l
637500150610     c     'S'           reade     fibsp03l
637600151215     c                   dow       not %eof(fibsp03l) and w_filbsps=*blanks
637700150609     c                   z-add     1             Ix
637800150609     c                   dow       Ix<=%elem(sbsp) and sbsp(Ix)>0
637900151215     c                             and w_FilBsps=*blanks
638000150609     c                   if        %lookup(sbsp(Ix):L1)>0
638100150609     c                   eval      w_FilBsp='S'
638200151215     c                   if        bspaut=*blanks
638300151215     c                   eval      w_FilBsps='S'
638400151215     c                   endif
638500150609     c                   endif
638600150609     c                   add       1             Ix
638700150609     c                   enddo
638800150610     c     'S'           reade     fibsp03l
638900150610     c                   enddo
639000150703
639100150703     c                   Clear                   tibs02ds
639200150703     c                   Clear                   dmradan
639300150703     c                   Movel     'C'           T02Mod
639400150703     c                   Movel     KNSIF         T02Sif
639500150703     c                   Movel     'MRA'         T02Cod
639600150703     c                   Movel     'FNLSU3R'     T02Ke1
639700150703     c                   Call      'TIBS02R'
639800150703     c                   Parm                    Kpjba
639900150703     c                   Parm                    tibs02ds
640000150703    2c                   If        T02Err = *Blanks
640100150703     c                   Movel     T02Uni        Dmradan
640200150703    2c                   endif
640300150703     C***
640400150703     C* CARICO DATI VARIABILI PARTENZE
640500150703     C***
640600150703     C                   MOVEL     '3I'          COD
640700150703     C                   MOVEL     '1       '    KEY
640800150703     C     KTAB          CHAIN     TABEL00F
640900150703     c                   if        %found(tabel00f)
641000150703     C                   MOVEL     TBLUNI        DS3I
641100150703     c                   else
641200150703     C                   CLEAR                   DS3I
641300150703     c                   endif
641400150609
641500021016      * Recupero le date delle statistiche
641600021016     c                   Clear                   Tibs02Ds
641700021016     c                   Clear                   dSdf
641800021016     c                   Eval      T02Mod = 'C'
641900021016     c                   Eval      T02Sif = Knsif
642000021016     c                   Eval      T02Cod = 'SDF'
642100021016     c                   Movel(p)  Simfel        T02Ke1
642200021016     c                   Call      'TIBS02R'
642300021016     c                   Parm                    Kpjba
642400021016     c                   Parm                    Tibs02Ds
642500021016     c                   If        T02Err = *Blanks
642600021016     c                   Movel     T02Uni        dSdf
642700021016     c                   EndIf
642800950109     C*
642900950109     C                   SETON                                        OFOE
643000950109     C* GIRO UDATE
643100950113     C                   TIME                    W0140            14 0
643200050707     C***                MOVEL     W0140         WTIME             6 0
643300950109     C                   MOVE      W0140         WDTGIO            8 0
643400950109     C                   Z-ADD     WDTGIO        G02DAT
643500950109     C                   MOVEL     *BLANK        G02ERR
643600950109     C                   CALL      'XSRDA8'
643700950109     C                   PARM                    WLBDAT
643800950109     C                   MOVEL     G02INV        DATEU             8 0
643900950109     C*
644000950109     C                   MOVEL     'R'           COMTSR
644100950109     C                   Z-ADD     0             DFV
644200950113     C                   CLEAR                   SAVSPE
644300970124     C* CAMPO CHE PASSO VUOTO AL PGM FNLSA2R3
644400970124     C                   CLEAR                   WWLNA
644500030107
644600950109     C                   ENDSR
644700070827     C**********************************************************************
644800070827     C*  P A D R O N C I N I
644900070827     C**********************************************************************
645000070827     C* CARICO I CODICI PADRONCINI PER I QUALI E' NECESSARIO CREARE I
645100070827     C*   FILE fiftt00F E fiftd00F
645200070827     C* CARICO ANCHE QUELLI CON MERCE PORTATA A MAGAZZINO
645300070827     C* CARICO ANCHE I PADRONCINI DIPENDENTI
645400070827     c     CarPAD        BEGSR
645500070827     c                   movel     fgvdfv        apddfv
645600070827     c
645700070827     C                   Z-ADD     1             X
645800070827     C                   Z-ADD     1             Y
645900070827     C                   CALL      'FNLSA2R2'
646000070827     C                   PARM                    APDPDR            7 0
646100070827     C                   PARM                    APDETM            1
646200070827     C                   PARM                    APDETP            1
646300070827     C                   PARM                    APDPDD            1
646400070827     c                   Parm                    ApdScm            1
646500070827     c                   Parm                    ApdScp            1
646600070827     C                   PARM                    WFINE             1
646700070827     C                   PARM                    simfel            3 0
646800070827     C                   PARM                    apddfv            8 0
646900070827     C*
647000070827     C     WFINE         DOWNE     'S'
647100140620     C**   APDPDD        IFEQ      *BLANKS
647200070827     C                   MOVEL     APDPDR        PDR(X)
647300070827     C                   MOVEL     APDETM        ETM(X)
647400070827     C                   MOVEL     APDETP        ETP(X)
647500070827     c                   Movel     ApdScm        Scm(x)
647600070827     c                   Movel     ApdScp        Scp(x)
647700070827     C                   ADD       1             X
647800140620     C**                 ELSE
647900140620     C**                 MOVEL     APDPDR        PDD(Y)
648000140620     C**                 ADD       1             Y
648100140620     C**                 END
648200070827     C*
648300070827     C                   CALL      'FNLSA2R2'
648400070827     C                   PARM                    APDPDR
648500070827     C                   PARM                    APDETM
648600070827     C                   PARM                    APDETP
648700070827     C                   PARM                    APDPDD
648800070827     c                   Parm                    ApdScm            1
648900070827     c                   Parm                    ApdScp            1
649000070827     C                   PARM                    WFINE             1
649100070827     C                   PARM                    simfel            3 0
649200070827     C                   PARM                    apddfv            8 0
649300070827     C                   ENDDO
649400070827     c
649500070827     c* Controllo riempimento schiera
649600070827     c                   clear                   trul0sds
649700070827     c                   eval      i0sski='PDR/ETM/ETP/...'
649800070827     c                   eval      i0sele=%elem(pdr)
649900070827     c                   eval      i0spie=x-1
650000070827     c                   eval      i0sfile='FNAPD00F  '
650100070827     c                   eval      i0ssif=knsif
650200130718     c                   eval      i0spgm='FNLSU2R'
650300070827     c                   movel     trul0sds      kpjbu
650400070827     c                   call      'TRUL0SR'
650500070827     c                   parm                    kpjba
650600070827     c
650700140620     c**                 clear                   trul0sds
650800140620     c**                 eval      i0sski='PDD'
650900140620     c**                 eval      i0sele=%elem(pdD)
651000140620     c**                 eval      i0spie=y-1
651100140620     c**                 eval      i0sfile='FNAPD00F  '
651200140620     c**                 eval      i0ssif=knsif
651300140620     c**                 eval      i0spgm='FNLSU2R'
651400140620     c**                 movel     trul0sds      kpjbu
651500140620     c**                 call      'TRUL0SR'
651600140620     c**                 parm                    kpjba
651700130530     c
651800130530     c* in caso di parzializzazione per cod cliente
651900130530     c* Carico la £6 della linea partenza FGV per vedere se ha clienti abilita
652000130530     c*  ti alla borderizzazione
652100130530     c                   clear                   L6
652200130530     c                   if        pa2ccm>*zeros
652300130530     c                   clear                   trul06ds
652400130530     c                   eval      d06cod = '£6'
652500130530     c                   movel     simfel        d06key
652600130530     c                   move      'S'           d06key
652700130530     c                   movel     fgvdfv        d06drf
652800130530     c                   movel     'L'           d06tla
652900130530     c                   movel     trul06ds      kpjbu
653000130530     c                   call      'TRUL06R'
653100130530     c                   parm                    kpjba
653200130530     c                   movel     kpjbu         trul06ds
653300151127
653400151127     c* Carico eventuale tabella con i codici collegati da borderizzare
653500151127     C                   MOVEL     'CLF'         kcod1
653600151127     c                   z-add     1             zz
653700151127     C                   eval      kke1=pa2ccm
653800151127     c     ktbe          setll     tntbe01l
653900151127     c     ktbe          reade     tntbe01l
654000151127    2c                   dow       not %eof(tntbe01l)
654100151127     c                   if        tbeatb=' '
654200151127     c                   movel     tbeke2        ccmb(zz)
654300151127     c                   add       1             zz
654400151127     c                   endif
654500151127     c     ktbe          reade     tntbe01l
654600151127     c                   enddo
654700151127     c
654800130530     c                   endif
654900130530     c
655000070827     c                   ENDSR
655100150603     C**********************************************************************
655200150603     C*  FIBSP00F - lnp/clienti con sospensione bolle
655300150603     C**********************************************************************
655400150603     c     sr_fibsp      begsr
655500170327     c                   clear                   w_SiBsp
655600150703     c                   clear                   w_bsptip
655700150603     c     w_clibo       chain     fibsp02l
655800151214     c                   if        %found(fibsp02l) and bspfpa='S'
655900151214     c                                              and bspaut=*blanks
656000151215     c                             and %lookup(blplnp:sbsp)>0
656100150603     C                   eval      w_SiBsp='S'
656200150703     c                   eval      w_bsptip='K'
656300150603     c                   else
656400150603     c     blplnp        chain     fibsp01l
656500150603     c                   if        %found(fibsp01l) and bspfpa='S'
656600151214     c                                              and bspaut=*blanks
656700150603     C                   eval      w_SiBsp='S'
656800150703     c                   eval      w_bsptip='L'
656900150603     c                   endif
657000150603     c                   endif
657100150603     c                   endsr
657200150703     C**********************************************************************
657300150703     C*  Memorizzazinoe clienti/lnp con bolle in sosp. oltre xx giorni
657400150703     C**********************************************************************
657500150703     c     sr_MemoEmail  begsr
657600150703    1c                   select
657700150703     c* SOSPENSIONE per CLIENTE
657800150703   WHc                   when      w_bsptip='K'
657900150703     c                   eval      indy=%lookup(sav_clibo:s_bspk)
658000150703    2c                   if        indy=0
658100150703     c                   eval      indy=%lookup(0:s_bspk)
658200150703     c* memorizzo cod cliente
658300150703     c                   eval      s_bspk(indy)=sav_clibo
658400150703    2c                   endif
658500150703     c* memorizzo numero di spedizioni in sosp oltre xx giorni
658600150703     c                   add       1             s_bspkbo(indy)
658700150703     c* memorizzo la data più bassa di immissione
658800150703    2c                   if        s_BSPkD(indy)=0 or
658900150703     c                             blpdim>s_BSPkD(indy)
659000150703     c                   eval      s_BSPkD(indy)=blpdim
659100150703    2c                   endif
659200150703     c* SOSPENSIONE per LINEA DI PARTENZA
659300150703   WHc                   when      w_bsptip='L'
659400150703     c                   eval      indy=%lookup(sav_lnp:s_bspL)
659500150703    2c                   if        indy=0
659600150703     c                   eval      indy=%lookup(0:s_bspL)
659700150703     c* memorizzo cod cliente
659800150703     c                   eval      s_bspL(indy)=sav_lnp
659900150703    2c                   endif
660000150703     c* memorizzo numero di spedizioni in sosp oltre xx giorni
660100150703     c                   add       1             s_bspLbo(indy)
660200150703     c* memorizzo la data più bassa di immissione
660300150703    2c                   if        s_BSPlD(indy)=0 or
660400150703     c                             blpdim>s_BSPlD(indy)
660500150703     c                   eval      s_BSPlD(indy)=blpdim
660600150703    2c                   endif
660700150703    1c                   endsl
660800150703     c
660900150703     c                   endsr
661000150703     c*-------------------------------------------------------------------
661100150703     c     INV_email     BEGSR
661200150703     c
661300150703     c                   clear                   writerig
661400150703     c                   if        not %open(prtemail)
661500150703     C                   EXSR      SR_OPENPRTF
661600150703     C                   endif
661700150703     c
661800150703     c                   exsr      Routend
661900150703     c
662000150703     c                   ENDSR
662100151022       //--------------------------------------------------------------
662200151022       //?  Errore per bolla non trovata
662300151022       //--------------------------------------------------------------
662400151022     c     Sr_errore     BEGSR
662500151022
662600151022     C                   movel     *blanks       wrkEml          253
662700151022     C                   movel     *blanks       wrkEmlCc        253
662800151022     C                   movel     *blanks       wrkMsg         5000
662900151022     C                   movel     *blanks       wrkOgg           44
663000151022     C                   movel     *blanks       wrkesito          1
663100151022     c
663200151022     C                   MOVEL     MSG(5)        wrkogg
663300151022     C                   MOVEL(P)  MSG(6)        wrkmsg
663400151022     c                   eval      %subst(wrkmsg:43:16)=
663500151022     c                             %editc(bltspe:'X')
663600151022     c                   eval      wrkemlcc ='ced@brt.it'
663700151022     c                   eval      wrkeml='elisa.sanfelici@brt.it'
663800151022     c
663900151022     C                   call(e)   'TIS701C1'
664000151022     C                   parm                    wrkEml
664100151022     C                   parm                    wrkEmlCc
664200151022     C                   parm                    wrkOgg
664300151022     C                   parm                    wrkMsg
664400151022     C                   parm                    wrkesito
664500151022
664600151022     c                   ENDSR
664700150703      /free
664800150703       //--------------------------------------------------------------
664900150703       //?Override al file di stampa per impostarvi i dati per
665000150703       //?  l'invio via e-mail   +   stampa inizio e-mail
665100150703       //--------------------------------------------------------------
665200150703       BEGSR sr_OpenPrtF;
665300150703
665400150703         // Override al file di stampa per impostarvi i dati per
665500150703         // l'invio via e-mail
665600150703           exsr sr_Override;
665700150703
665800150703
665900150703           indy = 1 ;
666000150703
666100150703          dow     s_BSPl(indy)>0  ;
666200150703
666300150703          if      s_BSPlBO(indy)>0 ;
666400150703          dataiso=%date(s_BSPlD(indy)) ;
666500150703          data_eur=dataiso  ;
666600150703
666700150703           clear O_testo;
666800150703           chain  s_BSPl(indy)   azorg01l  ;
666900150703           if not %found(azorg01l)  ;
667000150703           eval orgdes=*all'?'  ;
667100150703           endif  ;
667200150703           %subst(O_testo:6:20) = %editc(s_BSPl(indy):'X')+'-'+orgdes  ;
667300150703           %subst(O_testo:46:10) =%char(data_eur)  ;
667400150703           %subst(O_testo:64:9 ) =%editc(s_BSPlBO(indy):'2')  ;
667500150703           except PrtDet;
667600150703           writerig=writerig+1  ;
667700150703           endif  ;
667800150703
667900150703           // se ne ho scritte 50 di righe devo inviare
668000150703           if writerig>=50  ;
668100150703             exsr      Routend  ;
668200150703             clear writerig                  ;
668300150703
668400150703             exsr sr_Override;
668500150703
668600150703           endif  ;
668700150703
668800150703           indy=indy+1  ;
668900150703           enddo    ;
669000150703
669100150703          // Stesso ciclo sui cod clienti
669200150703          indy = 1      ;
669300160208          dow     s_BSPk(indy)>0  ;
669400150703
669500150703          if      s_BSPkBO(indy)>0 ;
669600150703          dataiso=%date(s_BSPkD(indy)) ;
669700150703          data_eur=dataiso  ;
669800150703
669900150703           clear O_testo;
670000150703           chain  (1:kci:s_BSPk(indy)) cnaco00f  ;
670100150703           if not %found(cnaco00f)  ;
670200150703           eval acorag=*all'?'  ;
670300150703           endif  ;
670400150703           %subst(O_testo:6:20) = %editc(s_BSPk(indy):'X')+'-'+acorag  ;
670500150703           %subst(O_testo:46:10) =%char(data_eur)  ;
670600150703           %subst(O_testo:64:9 ) =%editc(s_BSPkBO(indy):'2')  ;
670700150703           except PrtDet;
670800150703           writerig=writerig+1  ;
670900150703           endif  ;
671000150703
671100150703           // se ne ho scritte 50 di righe devo inviare
671200150703           if writerig>=50  ;
671300150703             exsr      Routend  ;
671400150703             clear writerig                  ;
671500150703
671600150703             exsr sr_Override;
671700150703
671800150703           endif  ;
671900150703
672000150703           indy= indy+1  ;
672100150703           enddo    ;
672200150703
672300150703             exsr      Routend  ;
672400150703       ENDSR;
672500150703
672600150703       //--------------------------------------------------------------
672700150703       //?Override al file di stampa per impostarvi i dati per
672800150703       //?  l'invio via e-mail   +   stampa inizio e-mail
672900150703       //--------------------------------------------------------------
673000150703       BEGSR sr_Override;
673100150703
673200150703         reset TRTCM1ds;
673300150703
673400150703         IF  §MRAdreg <> *blank;
673500150703           §CM1mitt = %trim(§MRAdmitt);
673600150703           §CM1dst  = %trim(§MRAddest);
673700150703
673800150703         // Valorizzo i campi della e-m@ail
673900150703          chain  simfel   azorg01l ;
674000150703
674100150703          if   %found(azorg01l) ;
674200150703           og140=orgde0  ;
674300150703           if §ogapo>*zeros  ;
674400150703           §cm1dst=%trim(§cm1dst) +'; CED'+ §ogapo +'@brt.it;';
674500150703           else  ;
674600150703           §cm1dst=%trim(§cm1dst) +'; CED'+%editc(orgf70:'X')+'@brt.it;';
674700150703          endif  ;
674800150703          endif  ;
674900150703
675000150703           §CM1tips = §MRAdreg;
675100150703           §CM1po   = %editc(simfel:'X');
675200150703           §CM1var  = '*OBJM*' + 'SOSPENSIONE AUTOMATICA bolle      '   ;
675300150703           §CM1idp  = §MRAdidpro;
675400160208           Qcmd1 = C_CmdOvrPrtF
675500150703                + ' outq(' + %trim(§MRAdoutqi) + ')'
675600150703                + ' usrdfndta(''' + TRTCM1ds + ''')';
675700150703         ELSE;
675800160208           Qcmd1 = C_CmdOvrPrtF;
675900150703         ENDIF;
676000150703
676100150703         callp(e) qCmdExc(Qcmd1 : %size(Qcmd1));
676200150703
676300150703           open PrtEMAIL;
676400150703
676500150703         // Stampa una testata se NON è richiesta la e-mail
676600150703           IF  §MRAdreg =  *blank;
676700150703             O_testo = JobUser + ' - ' + nompgm
676800150703                     + ' - ' + %editc( *date : 'Y' )
676900150703                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
677000150703             except PrtDet;
677100150703             clear O_testo;
677200150703             except PrtDet;
677300150703             except PrtDet;
677400150703           ENDIF;
677500150703
677600150703         // Stampa testo iniziale
677700150703           O_testo = 'E'' stata eseguita la Sospensione Automatica +
677800150703                      di bolle immesse da almeno xx giorni.' ;
677900150703           %subst(O_testo:72:2) = (%editc(§3iggm: '4'))     ;
678000150703           except PrtDet;
678100150703           O_testo = 'Verificare col mittente o procedere alla lor+
678200150703                      o chiusura come "Merce MAI Affidata".' ;
678300150703           except PrtDet;
678400150703
678500150703         // Stampa una riga vuota
678600150703           clear O_testo;
678700150703           except PrtDet;
678800150703
678900150703         // Stampa intestazione
679000150703           O_testo = ' Bolle spostate in sospensione di          Data imm+
679100150703                     issione    Bolle da verificare       ';
679200150703           except PrtDet;
679300150703           O_testo = 'Codice Cliente o Linea di Partenza          più vec+
679400150703                     chia                                 ';
679500150703         //%subst(o_testo:90:10)= knmus     ;
679600150703           except PrtDet;
679700150703
679800150703         // Stampa una riga vuota
679900150703           clear O_testo;
680000150703           except PrtDet;
680100150703       ENDSR;
680200150703
680300150703       //--------------------------------------------------------------
680400150703       //?Operazioni finali.
680500150703       //--------------------------------------------------------------
680600150703       BEGSR RoutEnd;
680700150703
680800150703         if %open(PrtEMAIL);
680900150703
681000150703         //?Chiusura dello spool?
681100150703           clear O_testo;
681200150703           except PrtDet;
681300150703           O_testo = '***   Fine Lista   ***';
681400150703           except PrtDet;
681500150703
681600150703           close PrtEMAIL;
681700150703
681800150703         //?Eliminazione overflow?
681900160208           Qcmd1 = C_CmdDltOvr;
682000150703           callp(e) qCmdExc(Qcmd1 : %size(Qcmd1));
682100150703
682200150703         endif;
682300150703
682400150703       ENDSR;
682500150703
682600150703      /end-free
682700150703     o* ---------------------------------------------------------------
682800150703     o* ?Spool di stampa (per e-mail).
682900150703     o* ---------------------------------------------------------------
683000150703     oPrtEMAIL  e            PRTdet      1
683100150703     o                       O_testo
683200030902
683300030902      *---------------------------------------------------------------*
683400030902
683500960201**         CMD1
683600021014OVRDBF FILE(FNBLPE0T)               MBR(Mxxx000)
683700960202**         CM19
683800021014OVRDBF FILE(FNBLPT0T)               MBR(Mxxx000)
683900960202**         CM32
684000021014OVRDBF FILE(FISPP00T)               MBR(Mxxx000)
684100000428**         CM37
684200021014OVRDBF FILE(FNFWA00T)               MBR(Mxxx000)
684300000428**         CM38
684400021014OVRDBF FILE(FNFVA01T)               MBR(Mxxx000)
684500920324**         CM41
684600021014ALCOBJ OBJ((FNBLPE0T *FILE *EXCL Mxxx000))
684700920324**         CM47
684800021014ALCOBJ OBJ((FISPP00T *FILE *EXCL Mxxx000))
684900000428**         CM48
685000021014ALCOBJ OBJ((FNFWA00T *FILE *EXCL Mxxx000))
685100000428**         CM49
685200021014ALCOBJ OBJ((FNFVA01T *FILE *EXCL Mxxx000))
685300920324**         CM51
685400021014ALCOBJ OBJ((FNBLPT0T *FILE *EXCL Mxxx000))
685500920324**  ERRORI
685600920324XXXXXXXXXXXXXXXXXXXX/XXX       ** ELENCO FOGLI VIAGGIO NON ELABORA
685700920324TI PERCHE' MEMBRI GIA' ALLOCATI **
685800920324 FOGLIO VIAGGIO
685900920324           ** TELEFONARE CON URGENZA ALL'UFFICIO C.E.D BOLOGNA  **
686000960129** MSG
686100960129ERRORE TRASMISSIONE FOGLI VIAGGIO PARTENZE
686200111019A FILIALE: NON TRASMESSO Foglio Viaggio XXX-XX.XXX.
686300070827CREATO JOBLOG per lavoro durato più di 5 minuti
686400060714                         FOGLIO VIAGGIO XXX-XXXXX.
686500151022ERRORE DI BOLLA DISALLINEATA FNLSU2R
686600151022Presente fnBLT00f senza relativo fnBLP -->
686700060714**
686800060714DSPJOBLOG OUTPUT(*PRINT)
