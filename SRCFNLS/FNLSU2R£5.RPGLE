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
028700170613     D* FILIALE ELABORATORE SIMFEL HA CML - non serve più
028800970423     D  PARCML               167    167
028900170613     d* AGP non più usato
029000010910     D  pa2agp               168    168
029100040121     D  PA2spv               169    169
029200040121     D  PA2spp               170    170
029300130527     D  PA2CCM               171    177
029400130530     D  PA2barcode           178    178
029500970423     D******
029600950109     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
029700931209     D PARAM3          DS
029800931209     D  PA3PDR                 1      7  0
029900931209     D  PA3TSR                 8      8
030000931209     D  PA3NDC                 9     15  0
030100931209     D  PA3FCT                16     16
030200931209     D  PA3FVD                17     17
030300931209     D  PA3FVT                18     18
030400950115     D  PA3DDC                19     26  0
030500041122     D  PA3sml               256    256
030600070423     D PARAMa3         DS
030700070423     D  PA3NFV                 1      5
030800070423     D* pa3tfp deve contenere il terminal di partenza per poter elaborare e
030900070423     d* trasmettere le bolle per terminal di partenza
031000070423     d* se non passato un valore > '000' il programma elabora e trasmette
031100070423     d* TUTTE le bolle presenti nel file fnblp46f (come faceva prima)
031200070423     D  PA3tfp                 6      8
031300111123     D  PA3num                 9      9    inz('0')
031400111123     d
031500920324     D* PER ESPLOSIONE DATI IN RICEZIONE
031600911001     D PARAM5          DS
031700950123     D  PARDTP                 1      8  0
031800950919     D  PARNFV                 9     13  0
031900960806     D  PARDEF                14     14
032000971106     D* FLAG PASSATO DAL FNLS93R PER DIRE SE FILE ALLOCATI
032100971106     D  WLCK                  15     15
032200000621     D  PARFEL               158    160  0
032300021015     D  LEGFIL               237    246    INZ('FXXX999999')
032400021015     d  MbrFil4              247    250    INZ('MXXX')
032500021015     d  MbrFil3              251    253    Inz('YYY')
032600021015     d  MbrFil               247    256
032700940415     D*
032800940415     D* PER PRENDERE FASE ANOMALIA                  -TRUL11R-
032900940415     D PARAM9          DS
033000950113     D  PA9TLA                 1      1    INZ('L')
033100950113     D  PA9NPG                 2      2    INZ('3')
033200950113     D  PA9SPG                 3      3    INZ('P')
033300940415     D  FASE                   4      4    INZ
033400040617
033500040617      * passaggio dati alla chiusura orm abbinati a bolle in sospensione
033600040617     d Param6          ds
033700040617     d  pa6fgs                        3  0
033800040617     d  pa6nfv                        5  0
033900940415     D*
034000931020     D                 DS
034100950113     D  BLPPDR                 1      7  0
034200950109     D  BLPDRT                 8     15  0
034300950113     D  BLPNRT                16     16  0
034400950113     D  BLPFPP                17     17
034500950113     D  COMBLP                 1     17
034600950115     D                 DS
034700950115     D  WCPR                   1      7  0 INZ
034800950115     D  WDRT                   8     15  0 INZ
034900950115     D  WNRT                  16     16  0 INZ
035000950115     D  WFPP                  17     17    INZ
035100950115     D  COMSAV                 1     17
035200940414     D                 DS
035300950109     D  BLTAAS                 1      4  0
035400950109     D  BLTLNP                 5      7  0
035500950109     D  BLTNRS                 8      9  0
035600950109     D  BLTNSP                10     16  0
035700970307     D  BLTSPE                 1     16  0
035800970307     D  BLTTBO                17     17
035900970307     D  BLTKEY                 1     17
036000950109     D                 DS
036100950109     D  BTTAAS                 1      4  0
036200950109     D  BTTLNP                 5      7  0
036300950109     D  BTTNRS                 8      9  0
036400950109     D  BTTNSP                10     16  0
036500970307     D  BTTSPE                 1     16  0
036600970307     D  BTTTBO                17     17
036700970307     D  BTTKEY                 1     17
036800931202     D*
036900970424     D* DS ESTERNA PER CHIUSURA ANOMALIE
037000960416     D DSLR33        E DS                  EXTNAME(FNLR33DS)
037100970424     D* DS PER REPERIMENTO DATI FOGLIO
037200970424     D DSLV53        E DS                  EXTNAME(FNLV53DS)
037300970516     D* DS PER DSLV55R - PASSAGGIO PARAMETRI
037400970516     D DSLV55        E DS                  EXTNAME(FNLV55DS)
037500031110     D* DS PER DSLV54R - PASSAGGIO PARAMETRI
037600031110     D FNLV54DS      E DS
037700070221     D* DS PER FNLS45R - PASSAGGIO PARAMETRI
037800070221     D FNLS45DS      E DS
037900130610     d
038000151118     D FNANM1          DS
038100151118     D  ANMNR1               397    400B 0
038200990517     D FNANM2          DS
038300960416     D  ANMNR2               397    400B 0
038400021021     D FNAN20          DS
038500960705     D  ANMNR0               397    400B 0
038600080109      *
038700080109     D DSE5F           DS            87    OCCURS($MaxElem)
038800960701     D  E5F                    1     87
038900960701     D                                     DIM(29)
039000070123     D DSBRV         E DS                  EXTNAME(FNBRV00F)
039100130527     D DSbrvw5       E DS                  EXTNAME(fnbrv00f)
039200130530     D                                     prefix(W5:3)
039300021016
039400070711     d dfar          e ds
039500021016     d Tibs02Ds      e ds
039600050524     d
039700050524     d* DS x controllo riempimento schiere
039800050518     d trul0sds      e ds
039900130530
040000130530     d Trul06ds      e ds
040100130530     d  L6                     1     90  0 dim(30)
040200170613      * - Parametri x Controllo profilo utenti
040300170613     d TIBS34ds      e ds                  inz
040400170613      * - Ds di riferimento al file esterno AZUTE00F
040500170613     d AZUTEds       e ds                  extname(AZUTE00F)
040600170613      * - Ds per dati organigramma
040700170613     d dDatiUte      e ds                  prefix(d_)
040800130530
040900050524     d
041000021016      * Ds date statistiche
041100021016     d dSdf          e ds
041200030107
041300021014     D VAR             DS             8
041400021014     d  Varx1                  2      4
041500021014     D  VARX                   5      7
041600031112     D  N_varx                 5      7  0
041700021014     D VAR2            DS             9
041800021014     d  Var2x1                 2      4
041900021014     D  VAR2X                  5      7
042000030902      *
042100030902     d Qcmd            s            150    inz
042200150703     d Qcmd1           s            512    inz
042300030902     d Qlen            s             15  5 inz(150)
042400030902      *
042500040422     d wcrtspu         s              1
042600040802     d wfgsspu         s              3  0
042700130527     d IX              s              4  0
042800031110
042900130627     D WNFV60          S                   LIKE(BRVNFV)
043000130531     D kfgv            S                   LIKE(fgvnfv)
043100130530     D WNFV5           S                   LIKE(FVDNFV) inz
043200130530     D KNPG            S                   LIKE(BRVNPG) inz
043300130527     D WVVDFV          S                   LIKE(D53DFV) inz
043400031110     d s_o54tfp        s                   like(o54tfp)
043500031112     d s_o54tfa        s                   like(o54tfa)
043600050204     d FLPTFA          s                   like(o54tfa)
043700040218     d wtfa            s                   like(o54tfa)
043800040310     d ktrd            s                   like(ar5trd)
043900050203     d kfgs            s                   Like(FvvFgs)
044000050203     d xx              s              2  0
044100050203     d yy              s              3  0
044200151127     d zz              s              3  0
044300130226     d Indx            s              3  0
044400150703     d Indy            s              3  0
044500150703     D Writerig        S              4  0 inz
044600130226     d SAVfnrs         s              5  0 inz
044700130226     d Wflsnrs         s              5  0
044800050203     d winvialocale    s              1
044900130226     d wTFP90          s              1
045000060516     d kdstfgs         s                   like(dstfgs)
045100060516     d kdstnfv         s                   like(dstnfv)
045200120607     d kdstnpg         s                   like(dstnpg)
045300060714     d
045400060714     d worap           s               z
045500060714     d worad           s               z
045600060714     d minuti          s              5  0
045700070219     D wtimez          S               z
045800070219     D dataiso         S               d   datfmt(*iso)
045900070219     D oraiso          S               t   timfmt(*iso)
046000150703     D data_eur        S               d   datfmt(*eur)
046100150603     D w_SiBsp         S              1
046200150703     D w_bsptip        S              1
046300150610     D w_FilBsp        S              1
046400151215     D w_FilBsps       S              1
046500150605     d sav_clibo       s                   like(blpccm)
046600150605     d w_clibo         s                   like(blpccm)
046700150605     d sav_lnp         s                   like(blplnp)
046800150703     d O_testo         s            132    inz
046900150703
047000150703      // - Comando di override al PrtF
047100150703     d C_CmdOvrPrtF    c                   const('OVRPRTF +
047200150703     d                                           file(PRTEMAIL) +
047300150703     d                                           pagesize(66 132) +
047400150703     d                                           lpi(6) cpi(10) +
047500150703     d                                           ovrscope(*actgrpdfn) +
047600150703     d                                           ')
047700150703     d C_CmdDltOvr     c                   const('DLTOVR +
047800150703     d                                            file(PRTEMAIL) +
047900150703     d                                            lvl(*actgrpdfn)')
048000150703      // - Tabella "MRA" = Bart-Mailing - Danni
048100150703     d dMRAdan       e ds                  inz
048200150703      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
048300150703     d TRTCM1ds      e ds                  inz
048400150703      //   ·§CM1mitt = Indirizzo e-mail del mittente
048500150703     d   §CM1mitt    e                     inz(' ed@brt.it')
048600150703      //   ·§CM1dst  = Indirizzo e-mail del destinatario
048700150703     d   §CM1dst     e                     inz('elisa.sanfelici@brt.it')
048800150703      //   ·§CM1tips = Tipo lettera via e-mail:
048900150703      //    "LET" = testo allegato in corpo con logo
049000150703      //            (richiede righe libere iniziali per il logo)
049100150703      //    "COI" = testo integrato senza logo senza BRT SPA iniziale
049200150703      //            (non consente né UNDERLINE né HIGHLIGHT)
049300150703     d   §CM1tips    e                     inz('COI')
049400150703      //   ·§CM1po   = Filiale
049500150703     d   §CM1po      e                     inz('046')
049600150703      //   ·§CM1var  = Oggetto e-mail
049700150703     d   §CM1var     e                     inz('*OBJM*+
049800150703     d                                     Manutenzione data spedizione+
049900150703     d                                      bolle ')
050000150703      //   ·§CM1sts  = Stato
050100150703     d   §CM1sts     e                     inz(*off)
050200150703      //   ·§CM1sts  = Id processo
050300150703     d   §CM1idp     e                     inz('2')
050400150703     D*
050500150703     D s_BSPl          S              3  0 DIM(599)
050600150703     D s_BSPlBO        S              7  0 DIM(599)
050700150703     D s_BSPlD         S              8  0 DIM(599)
050800150703     d
050900150703     D s_BSPk          S              7  0 DIM(799)
051000150703     D s_BSPkBO        S              7  0 DIM(799)
051100150703     D s_BSPkD         S              8  0 DIM(799)
051200150703
051300150703      // - Esecuzione comando di sistema
051400150703     d qCmdExc         pr                  extpgm('QCMDEXC')
051500150703     d  Qcmd1                       512    const  options(*varsize)
051600150703     d  Qlen                         15  5 const
051700070219
051800070219     D                SDS
051900070219     D  NOMPGM                 1     10
052000150703     d   JobUser             254    263
052100050203      *
052200900515      *
052300900601      *---------------------------------------------------------------*
052400900601      * INDICATORI USATI
052500930129      *---------------------------------------------------------------*
052600991026      * 01    = TRASMETTO
052700910911      * 04    = DEVO GENERARE AUTOSPUTA
052800931102      * 05    = LOKUP NELLA SCHIERA DEI PADRONCINI
052900910910      * 06    = LINEA DI ARRIVO CHE TRANSITA
053000920427      * 08    = INVIO ALLA LINEA ARRIVO CON SCARICO IL SUO TERMINAL
053100921202      * 09    = ESISTONO BOLLE SUL FOGLIO VIAGGIO DA INVIARE
053200920526      * 15    = CHAIN SU FV NON RIUSCITA: RISOTTOMETTO AZIONE
053300931027      * 16    = CHAIN IN TABEL PER CONTROLLARE SE IL TIPO BOLLA E' DA
053400931027      *         ESCLUDERE
053500920703      * 17    = LINEA ARRIVO E/O SCARICO >< DA TERMINAL: VEDO SE ECCEZI
053600950110      * 18    = CHAIN SU FNFVD000
053700900608      * 19/21 =DI COMODO
053800950109      * 22    = LETTURA FNBLP00F
053900991026      * 26    = LETTURA FNBLT00F
054000920324      * 28    = CI SONO ERRORI IN SCRITTURA FILE STORICO
054100970507      * 29/37 = DI COMODO
054200950915      * 40    = ELABORO LE BOLLE  DELL'AREA
054300950915      * 41    = ELABORO LE BOLLE  TRANSITO
054400940414      * 42    = ELABORO LE ANOMALIE DISGUIDO
054500950915      * 43    = ELABORO LE SPUNTE ENTRATA PER FILIALE CON CML
054600170613      * 44    =
054700950915      * 50    = MEMBRI FILE GIA' ALLOCATI: IMPOSSIBILE ESEGURE CHIUSURA
054800080228      * 90    = errore di scrittura file o lookup
054900080228      * 91    = LETTURA BOLLE DA LETTURA SPUNTA ENTRATA E SPUNTE PART.
055000960503      * 92    = ESISTE ALMENO UNA SPUNTA LOCALE
055100930127      * 93/99 = OPERAZIONI DA ESEGUIRSI NON LA PRIMA VOLTA
055200930708     C*---------------------------------------------------------------*
055300960202     C**
055400150703
055500150703     c                   clear                   s_bspk
055600150703     c                   clear                   s_bspkBO
055700150703     c                   clear                   s_bspkD
055800150703     c                   clear                   s_bspl
055900150703     c                   clear                   s_bsplBO
056000150703     c                   clear                   s_bsplD
056100150703
056200000619    1C     FGV2          IFNE      99999
056300020807     C     K02FGV01      CHAIN(N)  FNFGV01L                           19
056400020807     C     K02FGV01      CHAIN     FNFGW01L                           10
056500000428     C     *IN10         IFEQ      *ON
056600000428     C     FGWATB        ORNE      ' '
056700000428     C                   MOVEL     'N'           WFGW              1
056800000428     C                   CLEAR                   FGWFF3
056900000428     C                   CLEAR                   FGWFF4
057000000428     C                   CLEAR                   FGWFL3
057100000428     C                   CLEAR                   FGWFL4
057200000619     C                   CLEAR                   FGWFGS
057300000428     C                   ELSE
057400000428     C                   MOVEL     'S'           WFGW              1
057500000428     C                   ENDIF
057600000731     C                   Z-ADD     FGWFGS        WFGSFV
057700000731     C     WFGSFV        IFEQ      0
057800000731     C                   Z-ADD     FGVLNP        WFGSFV
057900040802     c*
058000040802     c                   eval      wfgsspu=wfgsfv
058100000619     C                   ENDIF
058200040802     c
058300130227     C                   MOVEL     'N'           WFVPT             1
058400000428     C*
058500960212     C* MEMORIZZO SE IL FV E' DA TRASMETTERE IN SEDE O MENO
058600960212     C                   MOVEL     'TV'          COD
058700960212     C                   MOVEL(P)  FGVTTR        KEY
058800960212     C     KTAB          CHAIN     TABEL                              36
058900960212    3C     *IN36         IFEQ      *OFF
059000960212     C     TBLFLG        ANDEQ     ' '
059100960212     C                   MOVEL     TBLUNI        DSTV
059200960212     C                   ELSE
059300960212     C                   MOVEL     'S'           §TVUTC
059400960806     C                   CLEAR                   §TVDEF
059500960212     C                   ENDIF
059600971106     C*
059700000619     C* PARDTP E' USATO NELLA PARAM5 E NELLA CREAZIONE SPOOL DI BACKUP
059800971106     C                   MOVEL     FGVDFV        PARDTP
059900000619     C                   MOVEL     §TVDEF        PARDEF
060000000619   X1C                   ELSE
060100130524     c* L'elaborazione dei fogli 99999 utilizzata ma utilizzata per trasmett
060200061108     c* ere le bolle di recupero di fine anno prima della fatturazione
060300061108     C                   MOVEL     'N'           WFVPT
060400000619     C                   Z-ADD     DATEU         PARDTP
060500070301     C                   Z-ADD     DATEU         fgvdfv
060600000619     C                   CLEAR                   PARDEF
060700000619    1C                   ENDIF
060800971106     C                   MOVEL     FGV2          PARNFV
060900000621     C                   MOVE      SIMFEL        PARFEL
061000060714     c* Prendo ora di apertura pgm
061100060714     C                   time                    worap
061200070827     c
061300130530     c* Carico codici autotrasportatori  + L6
061400070827     c                   EXSR      CarPAD
061500920526     C*
061600960123     C* R I C H I A M A T O   D A   G E S T I O N E   F.V I A G G I O
061700960123    1C     PA2FLG        IFEQ      'M'
061800960123     C*
061900130530     c
062000130530     C* ALLOCO MEMBRI DI TRASMISSIONE PER IL FOGLIO VIAGGIO
062100960123     C                   EXSR      ALLOCO
062200960202     C**
062300960123     C* ELABORO FOGLI VIAGGIO PARTENZE
062400021014     C                   MOVE      ')'           VAR               8
062500000725     C* MEMBRI DIVERSI A SECONDA CHE SIA FV POSTE OPPURE NO VISTO
062600000725     C* CHE ELABORAZIONI PER LE POSTE DIURANO SEMPRE MOLTO ABBIAMO DE-
062700000725     C* CISO DI FARLE GIRARE SU ALTRA CODA.
062800000725     C                   MOVEL     'M'           VAR
062900000731     C                   MOVEL     'M'           EMME
063000021014     C                   Z-ADD     48            LUNG             15 5
063100960202     C                   MOVEL     FGVLNA        LNA2
063200960202     C*
063300960202     C  N50              EXSR      FVIAGG
063400960123     C* CHIUSURA FINALE FILES
063500000428     C   89              CLOSE     FNFWA00T
063600000428     C   94              CLOSE     FNFVA01T
063700960123     C*
063800960123   X1C                   ELSE
063900960123     C* R I C H I A M A T O   D A   C H I U S U R A   F.V I A G G I O
064000960123     C*
064100000619     C* IGNORO SE STO CHIUDENDO FV 99999
064200000619   2AC     FGV2          IFNE      99999
064300960123     C*
064400921202     C*  19 ON - FOGLIO INESISTENTE
064500921202     C*
064600921202     C*          SE FOGLIO GIA' CHIUSO ESCO
064700921202     C  N19FGVFCF        IFNE      ' '
064800921202     C                   SETON                                        19
064900921202     C                   ELSE
065000921202     C                   MOVEL     FGVLNA        LNA2
065100921202     C                   END
065200920526     C**
065300940505     C   19              GOTO      FINE
065400950109     C* DATA CON CUI AGGIORNO LE BOLLE
065500950109     C                   Z-ADD     FGVDFV        WDTP
065600130527     c*
065700130527     c* Memorizzo anno e mese foglio
065800130705     c                   z-add     fgvdfv        w0040             4 0
065900130705     c                   movel     w0040         kmmm              2 0
066000130527     c                   movel     fgvdfv        kaaa4             4 0
066100900705     C*
066200920324     C* ALLOCO MEMBRI DI TRASMISSIONE PER IL FOGLIO VIAGGIO
066300130530    2C     FGVTTR        IFNE      'I'
066400130530     C     §TVDEF        ANDNE     'S'
066500130530     c                   seton                                        09
066600130530     c                   endif
066700130530     c
066800920324     C                   EXSR      ALLOCO
066900900911     C* ELABORAZIONE BOLLE
067000950915     C  N50              EXSR      ELAB
067100130530     c
067200000619  X2AC                   ELSE
067300000619     C* ELABORO  B O L L E    P A R T E N Z E  CON F.V. 99999
067400000619     C*  (SE CE N'E')
067500021014     C                   Z-ADD     48            LUNG
067600021014     C                   MOVE      ')'           VAR
067700021014     C                   MOVE      '))'          VAR2              9
067800000725     C                   MOVEL     'M'           VAR
067900000725     C                   MOVEL     'M'           VAR2
068000000731     C                   MOVEL     'M'           EMME
068100000621     C                   Z-ADD     1             WNPG
068200000619     C                   Z-ADD     99999         COMNFV            5 0
068300130527     C     K02BLP77_9    setll     FNBLP000                               22
068400000619     C   22              EXSR      TRAS99
068500000619     C*
068600000619     C* CHIUSURA FINALE FILE DELLE BOLLE
068700000619     C     *IN99         IFEQ      *ON
068800000619     C                   CLOSE     FNBLPE0T
068900000619     C                   ENDIF
069000000619   2AC                   ENDIF
069100920522     C*
069200960123    1C                   ENDIF
069300960202     C** AGGIORNO FOGLIO VIAGGIO
069400000619     C     FGV2          IFNE      99999
069500000619     C     *IN50         ANDEQ     *OFF
069600020807     C     K02FGV01      CHAIN     FNFGV01L                           1915
069700960202     C     *IN15         IFEQ      *OFF
069800960202     C     *IN19         ANDEQ     *OFF
069900960202     C                   MOVEL     'S'           FGVFCF
070000960202     C                   MOVEL     'T'           FGVFTR
070100960202     C                   MOVEL     DATEU         FGVDTR
070200960202     C                   UPDATE    FNFGV000
070300960202     C                   ENDIF
070400960202     C                   ENDIF
070500960123     C*
070600940217     C* LANCIO PROGRAMMI
070700930930     C                   MOVEL     *BLANKS       WLCK
070800971106     C                   MOVEL     PARAM5        KPJBU
070900930929     C*
071000930929     C* LANCIO PGM CHE PREPARA FILE DA INVIARE
071100950123     C                   CALL      'FNLS93R'
071200950123     C                   PARM                    KPJBA
071300971106     C                   MOVEL     KPJBU         PARAM5
071400930930     C*
071500930930     C* SE FILE ALLOCATI E NO TRASMISSIONE LI RISOTTOMETTO
071600931126     C     WLCK          IFNE      ' '
071700950109     C                   MOVEL     'LS93'        KCOAZ
071800931203     C                   CALL      'BCH10'
071900931203     C                   PARM                    KPJBA
072000931126     C                   ENDIF
072100931214     C*
072200021014     c                   Move      FgvLnp        MbrFil3
072300021015     c                   Movel     MbrFil4       LEGFIL
072400021015     c                   Movel     'F'           Legfil
072500931214     C                   MOVE      PARAM5        KPJBU
072600950919     C*
072700950919     C*
072800950919     C* CI SONO SOLO SPUNTE --> SOTTOMETTO SOLO IL PGM DELLE SPUNTE
072900960806     C*  SOLO SE NON E' UN FOGLIO DI DEFLUENZA
073000950919     C     *IN92         IFEQ      *ON
073100971106     C     *IN97         ANDEQ     *OFF
073200960806     C     §TVDEF        ANDNE     'S'
073300950919     C                   MOVEL     'LR15'        KCOAZ
073400950919     C                   CALL      'BCH10'
073500950919     C                   PARM                    KPJBA
073600950919     C                   ENDIF
073700040617      * sottometto la chiusura degli orm abbinati alle bolle in sospensione
073800040617      * se non è un foglio di defluenza
073900061108     c                   If        §tvdef <> 'S'  and fgv2<>99999
074000040617     c                   Clear                   Param6
074100040617     c                   Eval      pa6fgs = simfel
074200040617     c                   Eval      pa6nfv = fgv2
074300040617     c                   Eval      kpjbu = param6
074400040617     c                   Eval      kcoaz = 'OR19'
074500040617     c                   Call      'BCH10'
074600040617     c                   Parm                    kpjba
074700040617     c                   EndIf
074800150703     c* invio email per bolle in sospensione da oltre xx giorni
074900150703     c                   if        s_bspk(1)>0 or s_bspl(1)>0
075000150703     c                   exsr      INV_email
075100150703     c                   endif
075200950919     C*
075300130524     C                   MOVEL     FNLSU2DS      KPJBU
075400940415     C*
075500000000     C     FINE          TAG
075600960124     C                   MOVEL     'C'           SA2TLA
075700960124     C                   CALL      'FNLSA2R3'
075800960124     C                   PARM                    SA2TLA
075900960124     C                   PARM                    SA2TRC
076000960124     C                   PARM                    SA2DTS
076100970124     C                   PARM                    WWLNA             3 0
076200970424     C*
076300960124     C                   CALL      'FNLSA2R4'
076400960124     C                   PARM                    SA2TLA            1
076500960124     C                   PARM                    SA2LNP
076600960124     C                   PARM                    SA2NFV
076700960124     C                   PARM                    SA2RTP
076800970424     C*
076900960416     C                   MOVEL     'C'           D33TLA
077000960416     C                   CALL      'FNLR33R'
077100960416     C                   PARM                    DSLR33
077200970424     C*
077300970424     C                   CLEAR                   DSLV53
077400970424     C                   MOVEL     'C'           D53TLA
077500970424     C                   CALL      'FNLV53R'
077600970424     C                   PARM                    DSLV53
077700970509     C*
077800970509     C     WFC44         IFEQ      'S'
077900070221     C*                  MOVEL     'C'           FLG
078000070221     C                   MOVEL     'C'           dls45flg
078100970509     C*
078200070221     C                   CALL      'FNLS45R'
078300070221     c                   parm                    fnls45ds
078400070222     c                   parm                    dsbrv
078500970509     C                   ENDIF
078600970516     C*
078700970516     C                   MOVEL     'C'           D55TLA
078800970516     C                   CALL      'FNLV55R'
078900031113     C                   PARM                    DSLV55
079000031113     c*
079100031113     C                   MOVEL     'C'           I54TLA
079200031113     C                   CALL      'FNLV54R'
079300031113     C                   PARM                    fnlv54ds
079400900529      *
079500060714     c                   time                    worad
079600060714     c     worad         subdur    worap         minuti:*MN
079700060714     c
079800111019     c* Se lavoro dura più di 5 minuti creo joblog ma non invio messaggio
079900111019     c*  a sede. Nel caso ci siano delle cose strane si prova a cercare se c'e'
080000070827    3c                   if        minuti>5
080100060714     C                   Z-ADD     42            LUNG             15 5
080200060714     C                   MOVEA     CMjob(1)      COMMAN
080300060714     C                   CALL      'QCMDEXC'
080400060714     C                   PARM                    COMMAN
080500060714     C                   PARM                    LUNG
080600060714     c                   endif
080700060714     c
080800000000     C                   SETON                                        LR
080900960123     C*---------------------------------------------------------------
081000960123     C* APERTURA FILE
081100960123     C*---------------------------------------------------------------
081200960123     C     OPENF         BEGSR
081300960123     C                   OPEN      FNAGB01L
081400990517     C                   OPEN      FNANM02L
081500151118     C                   OPEN      FNANM01L
081600960123     C                   OPEN      FNFVD01L
081700960123     C                   OPEN      FNCLS01L
081800140618     C                   OPEN      FNBRV05L
081900110907     C                   OPEN      FNBRV07L
082000130527     C                   OPEN      FNBRV09L
082100960123     C                   OPEN      FNBLP01L
082200060220     C                   OPEN      Fiar401L
082300960123     C                   OPEN      FNBLT01L
082400960123     C                   OPEN      FNBLT27L
082500020605     C                   OPEN      FNBTP11L
082600020606     C                   OPEN      FNBTT11L
082700020606     C                   OPEN      FNBTT37L
082800070423     C                   OPEN      FNBLP46L
082900010309     C                   OPEN      FNORM01L
083000070711     c                   open      fnorg01l
083100010309     C                   OPEN      FNORF00F
083200051207     C                   OPEN      FNORV00F
083300030107     c                   Open      Fiar501l
083400960123     C                   ENDSR
083500900601      *---------------------------------------------------------------
083600900531      * ELABORO TRASFERIMENTO
083700900601      *---------------------------------------------------------------
083800900531     C     ELAB          BEGSR
083900021014     C                   Z-ADD     48            LUNG
084000021014     C                   MOVE      ')'           VAR
084100000725     C                   MOVEL     'M'           VAR
084200000731     C                   MOVEL     'M'           EMME
084300950206     C                   Z-ADD     1             WNPG
084400921202     C*
084500931001     C* 09 ON - CI SONO BOLLE DA CHIUDERE
084600910906     C   09              DO
084700931001     C*
084800931001     C* ELABORO  B O L L E    P A R T E N Z E
084900900601     C                   EXSR      TRASFE
085000931001     C*
085100931021     C* CHIUSURA FINALE FILE TASSAZIONE PADRONCINI
085200011121     C                   CLOSE     fiftt01L
085300011121     C                   CLOSE     fiftd04L
085400020416     C                   CLOSE     fiftd01L
085500970930     C                   CLOSE     FNSTPR0F
085600931021     C*
085700931001     C* ELABORO  B O L L E    P A R T E N Z E  CON F.V. 99999
085800931001     C*  (SE CE N'E')
085900130531     C                   Z-ADD     99999         comNFV            5 0
086000130527     C     K02BLP77_9    setll     FNBLP000                               22
086100950323     C   22              EXSR      TRAS99
086200931001     C*
086300931001     C* CHIUSURA FINALE FILE DELLE BOLLE
086400931001     C     *IN99         IFEQ      *ON
086500950109     C                   CLOSE     FNBLPE0T
086600931001     C                   ENDIF
086700910906     C                   END
086800130530     c
086900130530     C* NON FACCIO L'AUTOGENERAZIONE SPUNTE SE :
087000130530     C*  - FV CON TIPO TRAINO INOLTRO
087100130530     C*  - FV DI DEFLUENZA
087200130530    2C     FGVTTR        IFNE      'I'
087300130530     C     §TVDEF        ANDNE     'S'
087400130530     C*
087500130530     C* NFV1 > 0   ====>  C H I U S U R A    D A    I. M. P.
087600130530    3C     NFV1          IFGT      0
087700130530     C                   EXSR      ABBIN
087800130530     C*
087900130530     C     K02FGV01      CHAIN(N)  FNFGV01L                           19
088000130530   X3C                   ELSE
088100130530     C* SE CHIUSURA NON DA IMP VERIFICO COMUNQUE PER LE SPUNTE PARTENZA
088200130530     C*  ESISTENTI IL PESO E/O VOLUME
088300130530     C                   EXSR      SPUPAR
088400130530    3C                   ENDIF
088500130530     C*
088600130530    2C                   ENDIF
088700931001     C*
088800931001     C* ELABORO  S P U N T E    P A R T E N Z E
088900910909     C                   EXSR      SPUNTE
089000931001     C*
089100931021     C* ELABORO  F O G L I    V I A G G I O    P A R T E N Z E
089200911004     C                   EXSR      FVIAGG
089300931001     C*
089400931001     C* CHIUSURA FINALE FILE
089500931001     C     *IN98         IFEQ      *ON
089600950109     C                   CLOSE     FNBLPT0T
089700931001     C                   ENDIF
089800931001     C     *IN96         IFEQ      *ON
089900000111     C                   CLOSE     FISPP00T
090000931001     C                   ENDIF
090100931001     C     *IN94         IFEQ      *ON
090200951116     C                   CLOSE     FNFVA01T
090300931001     C                   ENDIF
090400000428     C     *IN89         IFEQ      *ON
090500000428     C                   CLOSE     FNFWA00T
090600000428     C                   ENDIF
090700911001     C**
090800900531     C                   ENDSR
090900900601     C**-----------------------------------------------*
091000900601     C** ESEGUE CALL QCAEXEC SUL MEMBRO DELLE BOLLE    *
091100900601     C**-----------------------------------------------*
091200900601     C     CAEX          BEGSR
091300900601     C** FILIALE DI COMPETENZA PER APERTURA MEMBRO
091400910905     C** CHIUSURA FILE SOLO DA SECONDO GIRO IN POI
091500931001     C     *IN99         IFEQ      *ON
091600950109     C                   CLOSE     FNBLPE0T
091700931001     C                   ELSE
091800931001     C                   SETON                                        99
091900931001     C                   ENDIF
092000910905     C** OVRDBF
092100950109     C                   MOVE      VAR           CMD1
092200910926     C                   MOVEL     *BLANKS       COMMAN           80
092300900601     C                   MOVEA     CMD1(1)       COMMAN
092400900601     C                   CALL      'QCMDEXC'
092500900601     C                   PARM                    COMMAN
092600900601     C                   PARM                    LUNG
092700910905     C*
092800910905     C* APERTURA FILE
092900950109     C                   OPEN      FNBLPE0T
093000900601     C                   ENDSR
093100910905     C**-----------------------------------------------*
093200910905     C** ESEGUE CALL QCAEXEC SUL MEMBRO DELLE BOLLE TRANSITO
093300910905     C**-----------------------------------------------*
093400910905     C     CAEX08        BEGSR
093500910905     C** FILIALE DI COMPETENZA PER APERTURA MEMBRO
093600910905     C** CHIUSURA FILE SOLO DA SECONDO GIRO IN POI
093700950109     C   98              CLOSE     FNBLPT0T
093800910905     C  N98              SETON                                        98
093900910905     C** OVRDBF
094000950109     C                   MOVE      VAR           CM19
094100910905     C                   MOVEL     *BLANKS       COMMAN
094200910905     C                   MOVEA     CM19(1)       COMMAN
094300910905     C                   CALL      'QCMDEXC'
094400910905     C                   PARM                    COMMAN
094500910905     C                   PARM                    LUNG
094600910905     C*
094700910905     C* APERTURA FILE
094800950109     C                   OPEN      FNBLPT0T
094900910905     C                   ENDSR
095000910909     C**-----------------------------------------------*
095100910909     C** ESEGUE CALL QCAEXEC SUL MEMBRO SPEDIZIONE BOLLE
095200910909     C**-----------------------------------------------*
095300910909     C     CAEXSP        BEGSR
095400910909     C* CHIUDO MEMBRO
095500000111     C   96              CLOSE     FISPP00T
095600920608     C  N96              SETON                                        96
095700910909     C*
095800910909     C** OVRDBF
095900000111     C                   MOVE      VAR           CM32
096000910909     C                   MOVEL     *BLANKS       COMMAN
096100920608     C                   MOVEA     CM32(1)       COMMAN
096200910909     C                   CALL      'QCMDEXC'
096300910909     C                   PARM                    COMMAN
096400910909     C                   PARM                    LUNG
096500910909     C*
096600910909     C* APERTURA FILE
096700000111     C                   OPEN      FISPP00T
096800910909     C*
096900910909     C                   ENDSR
097000910910     C**-----------------------------------------------*
097100910910     C** ESEGUE CALL QCAEXEC SUL MEMBRO FOGLI VIAGGIO
097200910910     C**-----------------------------------------------*
097300910910     C     CAEXFV        BEGSR
097400910910     C*
097500960123     C** F N F V A 0 1 T
097600951116     C   94              CLOSE     FNFVA01T
097700921202     C  N94              SETON                                        94
097800910910     C*
097900910910     C** OVRDBF
098000951116     C                   MOVE      VAR           CM38
098100921202     C                   MOVEL     *BLANKS       COMMAN
098200921202     C                   MOVEA     CM38(1)       COMMAN
098300910910     C                   CALL      'QCMDEXC'
098400910910     C                   PARM                    COMMAN
098500910910     C                   PARM                    LUNG
098600910910     C*
098700910910     C* APERTURA FILE
098800951116     C                   OPEN      FNFVA01T
098900000428     C**
099000000428     C** F N F W A 0 0 T
099100000428     C   89              CLOSE     FNFWA00T
099200000428     C  N89              SETON                                        89
099300000428     C*
099400000428     C** OVRDBF
099500000428     C                   MOVE      VAR           CM37
099600000428     C                   MOVEL     *BLANKS       COMMAN
099700000428     C                   MOVEA     CM37(1)       COMMAN
099800000428     C                   CALL      'QCMDEXC'
099900000428     C                   PARM                    COMMAN
100000000428     C                   PARM                    LUNG
100100000428     C*
100200000428     C* APERTURA FILE
100300000428     C                   OPEN      FNFWA00T
100400060707     c
100500910910     C                   ENDSR
100600900601     C**-----------------------------------------------*
100700900601     C** TRASFERISCO BOLLE NELLO STORICO E IN FILE TRASMISSIONE
100800900601     C**-----------------------------------------------*
100900000320     C     TRASFE        BEGSR
101000950323     C                   CLEAR                   SAVCBO
101100130530     C                   CLEAR                   wBORtran
101200130527     C                   CLEAR                   wFLP              3 0
101300910906     C                   Z-ADD     0             SAVLNA            3 0
101400150605     c                   clear                   sav_clibo
101500150605     c                   clear                   sav_lnp
101600150608     c                   clear                   w_SiBsp
101700950109     C                   Z-ADD     1             WNPG
101800130531     C                   Z-ADD     1             I                 3 0
101900130531     c                   clear                   kfgv
102000931027     C*
102100011121     C                   OPEN      fiftt01L
102200011121     C                   OPEN      fiftd04L
102300020416     C                   OPEN      fiftd01L
102400970930     C                   OPEN      FNSTPR0F
102500931021     C*
102600130531     C* lettura bolle per linea di arrivo con numero foglio = zero
102700130531    0C     FFV(I)        DOWGT     *ZEROS
102800130531     C                   MOVEL     FFV(I)        SAVFFV
102900130531     C*
103000130531     C     KBLP77        setll     FNBLP000
103100130531     C     KBLP77        reade     FNBLP000                               22
103200910905     C*
103300130531    1C     *IN22         DOWEQ     '0'
103400950116     C* ELABORO DOLO LE BOLLE NON ANCORA TRASMESSE
103500950116     C     BLPDT2        IFEQ      0
103600950115     C* CONTROLLO IL TIPO BOLLA
103700950115     C                   EXSR      CTABEL
103800910909     C**
103900910909     C** L I N E A   A R R I V O   C A M B I A T A
104000130531    2C     SAVLNA        IFNE      BLPLNA
104100130702     C                   SETOFF                                       01
104200920505     C*
104300130530     c* In caso di parzializz. cod cliente devo borderizare solo
104400130530     c*  linea di arrivo = SIMFEL + L6
104500151126     c                   z-add     1             Indx              3 0
104600151126     c**                 if        pa2ccm>*zeros
104700151126     c**                 eval      Indx=%lookup(blplna:l6)
104800151126     c**                 if        indx=0
104900151126     C**   Kblp77        setgt     FNBLP000
105000151126     c**                 endif
105100151126     c**                 endif
105200130530     c
105300130530     C                   CLEAR                   dslv55
105400140403     c                   z-add     blptfp        d55lnp
105500130527     C                   MOVEL     BLPLNA        d55LIN
105600130527     C                   MOVEL     FGVDFV        d55DRF
105700130527     C                   CALL      'FNLV55R'
105800130530     C                   PARM                    dslv55
105900130527     c
106000130527    1C     d55err        IFEQ      *BLANKS
106100130527     c                   z-add     d55tfp        s_o54tfp
106200130527     c                   z-add     d55tfa        s_o54tfa
106300040630     c* se lna è un secondo livello in arrivo potrebbe succedere che il
106400040630     c* p.o. di scarico sia = alla lna (f.v. diretto ad un secondo liv.)
106500040630     c* (In questo caso devo inviare la bolla al ter-part della lna.
106600040630     c* Infatti anche in fviagg e spunte i dati vengono trasmessi al ter-par
106700040630     c* del p.o. di scarico (in qesto caso p.o. di scarico=lna)
106800040630     c* Se non si facesse in questo modo le bolle andrebbero via in un membr
106900040630     c* diverso rispetto alle spunte e al f.viaggio e in più il pgm fnls93
107000040630     c* non trasmetterebbe il membro delle bolle giusto)
107100130527    2c     blplna        ifne      d55tfa
107200040630     c                   clear                   wlnadir           1
107300040630     C                   Z-ADD     1             B                 3 0
107400040630     C                   MOVEL     blplna        COMPRO            3
107500040630     C     COMPRO        LOOKUP    FFV(B)                                 20
107600040630     C* NON TROVATA --> METTO LINEA ARRIVO STESSA
107700070913     c                   if        (*in20 and flp(B)=compro) or not *in20
107800040630     c                   movel     'D'           wlnadir
107900040630     c                   endif
108000130527    2c                   endif
108100040630     c*
108200130527     c* se secondo livello in arrivo
108300031219     c* invio la bolla al terpar del ter-arr della lna
108400130527    2c     blplna        ifne      d55tfa
108500040630     c     wlnadir       andeq     *blanks
108600130530     c                   clear                   dslv55
108700130527     C                   MOVEL     'P'           d55TPT
108800130527     C                   MOVEL     s_o54tfa      d55LIN
108900130527     C                   MOVEL     FGVDFV        d55DRF
109000130527     C                   CALL      'FNLV55R'
109100130530     C                   PARM                    dslv55
109200130527    3c     d55err        ifeq      *blanks
109300031219     c                   seton                                        01
109400130527     c                   movel     d55tfp        varx
109500031219    3c                   endif
109600130527     c
109700031219   x2c                   else
109800130527     c* se primo liv. in arrivo  o 2°liv.in arr. Diretto
109900031219     c* invio al terminal di partenza di lna
110000031219     c                   seton                                        01
110100130527     c                   movel     s_o54tfp      varx
110200031219    2c                   endif
110300031112    1c                   endif
110400031110     c
110500130527    1c     *in01         ifeq      *on
110600971104     C                   EXSR      CAEX
110700130527     c
110800130527     C* SE foglio locale accendo indicatore 97
110900130527     c     fgvlnp        ifeq      s_o54tfa
111000971106     C                   SETON                                        97
111100971106     C                   END
111200130527    1C                   END
111300130527     c*
111400130527     c* Controllo se c'e' transito
111500130527     C                   exsr      CtrFLP
111600000321     C*
111700130702     C* CONTROLLO SE C'E' TRANSITO
111800971104     C*
111900130530     c                   if        wBORtran='S'
112000130703     C                   MOVEL     wflp          VAR2X
112100130703     C                   MOVEL     wflp          VARX
112200000428     C                   EXSR      TRANSI
112300000428     C                   EXSR      CAEX08
112400130527     C                   ENDif
112500910909     C*
112600910909     C                   MOVEL     BLPLNA        SAVLNA
112700130531    2C                   ENDif
112800150603    2c                   if        blpccm>0
112900150603     c                   eval      w_clibo=blpccm
113000150603     c                   else
113100150603     c                   eval      w_clibo=blpksc
113200150603    2c                   endif
113300150603     c* Lettura file FIBSP a cambio di lnp o codice cliente
113400150603     c* Solo se non è chiusura Barcode
113500150603     c                   if        pa2barcode=' ' and
113600151215     c                             w_filbsps='S'  and
113700150603     c                             (blplnp<>sav_lnp or w_clibo<>sav_clibo)
113800150603
113900150603     c                   exsr      sr_fibsp
114000150603
114100150603     c                   eval      sav_lnp=blplnp
114200150603     c                   eval      sav_clibo=w_clibo
114300150603     c                   endif
114400130527     c
114500130527     c* Eseguo routine di borderizzazione e/o messa in sospensione
114600130527     c*  automatica
114700130531   2ac                   if        Indx>0
114800130527     c                   exsr      Borderiz
114900130527     c
115000130527     c                   if        wescludi='S'
115100150605     c                   unlock    fnblp77l
115200150603     c*
115300150603     c* richiamo pgm per mettere in sospensione la bolla
115400150603     c* di cliente con colli che partono da diverse filiali se non presenti
115500150603     c* spunte
115600170327     c*****              if        w_SiBsp='S'  and w_Sosp='S'
115700170327     c                   if        w_Sosp='S'
115800150603     c                   clear                   fnlsu3ds
115900150605     c                   eval      ilsu3aas=%editc(blpaas:'X')
116000150605     c                   eval      ilsu3lnp=%editc(blplnp:'X')
116100150605     c                   eval      ilsu3nrs=%editc(blpnrs:'X')
116200150605     c                   eval      ilsu3nsp=%editc(blpnsp:'X')
116300150610     c                   eval      ilsu3dsp_B=%editc(fgvdfv:'X')
116400150603     c                   eval      ilsu3nos='S'
116500170509     c                   eval      ilsu3flg='R'
116600150603     c                   movel     fnlsu3ds      kpjbu
116700150603     c                   call      'FNLSU3R'
116800150603     c                   parm                    kpjbA
116900150703     c                   movel     kpjbu         fnlsu3ds
117000150703     c* Se Bolla in sospensione oltre xx giorni memorizzo in schiera per invio
117100150703     c* mail
117200150703     c                   if        olsu3err=*blanks  and
117300150703     c                             olsu3sosp='1'
117400170608     c*                  exsr      sr_MemoEmail
117500150703     c                   endif
117600150603     c                   endif
117700130610     c
117800130531   2bc                   else
117900130527     c*
118000130527     c* Memorizzo in bolla fil transito se c'e'
118100130530     c                   if        wBORtran='S'
118200130527     c                   movel     wflp          blpflp
118300130527     c                   else
118400130527     c                   clear                   blpflp
118500130527     c                   endif
118600130527     c                   movel     s_o54tfa      blptfa
118700130527     c                   movel     s_o54tfa      blpfea
118800130527     C                   Z-ADD     FGV2          BLPNFV
118900130527     C                   Z-ADD     fgvdfv        BLPDBR
119000910905     C**
119100950109     C** DETTAGLIO SEGNACOLLI             F N B L T 0 0 F
119200950109     C**
119300950118     C*  SE IN BOLLA CTM= AUTOSPUNTA LA GENERO --> 04 ON
119400910911     C                   SETOFF                                       04
119500130527     C                   CLEAR                   WBLP01            1
119600950118     C     BLPCTM        LOOKUP    CTM                                    04
119700910911     C*
119800950109     C     KBLP2         SETLL     FNBLT000
119900950109     C     KBLP2         READE     FNBLT000                               26
120000910905     C*
120100910905     C     *IN26         DOWEQ     '0'
120200130527     C                   CLEAR                   WBRV1             1
120300130527     C                   CLEAR                   WBRV5             1
120400130527     C                   CLEAR                   dsbrvw5
120500130527     C*
120600911119     C* SE AUTOSPUNTA LA CREO
120700950109     C     *IN04         IFEQ      *ON
120800140618     C     KBLT2         SETLL     FNBRV05                                19
120900950109     C  N19              EXSR      CRTSPU
121000950109     C                   ENDIF
121100130527     c*
121200130527     c* Controllo le anomalie 110 105 se sono da chiudere
121300130527     c                   exsr      Contr_anom
121400060220     c
121500950109     C* FLAGGO LA TRASMISSIONE
121600950109     C                   MOVEL     'T'           BLTFTR
121700950109     C                   Z-ADD     DATEU         BLTDTR
121800910905     C*
121900950109     C                   UPDATE    FNBLT000
122000130527     C*
122100130527     C* SE TROVATA SPUNTA AGGIORNO LA DATA ENTRATA
122200130527     C     WBRV5         IFEQ      'S'
122300130701     c                   if        wblp01=' '
122400130527     C                   update    FNBLP000
122500130527     C                   MOVEL     'S'           WBLP01
122600130701     c                   endif
122700130527     c*
122800130527     c                   eval      dsbrv =dsbrvw5
122900130527     C                   EXSR      call44
123000130527     C                   ENDIF
123100910905     C*
123200950109     C     KBLP2         READE     FNBLT000                               26
123300910905     C                   END
123400130527     c
123500130527     c                   if        wblp01='S'
123600130527     c     kblp2         chain     fnblp01l
123700130911
123800130911     c* se ho trovato una spunta partenza scrivo AGB x aggiornare le date
123900130911     C* SCRIVO IL FILE FNAGB01L PER AGGIORNARE DATE
124000130911     C                   MOVEL     'P'           AGBTBO
124100130911     C                   MOVEL     BLPAAS        AGBAAS
124200130911     C                   MOVEL     BLPLNP        AGBLNP
124300130911     C                   MOVEL     BLPNRS        AGBNRS
124400130911     C                   MOVEL     BLPNSP        AGBNSP
124500130911     C                   MOVEL     'DT'          AGBAGB
124600130911     C                   WRITE     FNAGB000                             39
124700130527     c                   endif
124800950109     C*
124900060220     C* FLAGGO FIAR500F
125000950109     C                   EXSR      WRITEF
125100910905     C**
125200950109     C**  BOLLA                           F N B L P 0 0 F
125300910905     C**
125400060220     C* IL FLAG DDT LO METTO IN BL2SV1
125500060220     C                   MOVEL     BLPFST        BL2SV1
125600060220     C* IL FLAG sigla operatore video lo metto in bl2cei
125700000619     C                   MOVE      BLPSOP        BL2CEI
125800060220     c
125900950109     C                   Z-ADD     DATEU         BLPDT3
126000921026     C                   MOVEL     'S'           BLPFT3
126100950109     C                   Z-ADD     DATEU         BLPDT2
126200950109     C                   MOVEL     'T'           BLPFT2
126300950109     C                   Z-ADD     DATEU         BLPDT1
126400950109     C                   MOVEL     'T'           BLPFT1
126500911119     C*
126600911119     C* IN QUELLO PER SEDE E FIL.ARR SE ASSEGNATO 9999 METTO LA L.ARR
126700950109     C                   Z-ADD     BLPKSC        W0070             7 0
126800911119     C                   MOVE      BLPKSC        COMKSC            4 0
126900911119     C     COMKSC        IFEQ      9999
127000911119     C                   MOVEL     BLPLNA        BLPKSC
127100911119     C                   END
127200911119     C*
127300950109     C   01              WRITE     FNBLPE00
127400950113     C* SOLO PER TRANSITO: IMPOSTO IL VOLUME BOLLETTATO CON QUELLO
127500950113     C*  DA FATTURARE
127600130702     C     wbortran      IFEQ      'S'
127700950113     C                   Z-ADD     BLPVLB        W0053             5 3
127800950113     C                   MOVEL     BLPFVB        W001A             1
127900950113     C*
128000950113     C                   Z-ADD     BLPVLF        BLPVLB
128100950113     C                   MOVEL     BLPFVF        BLPFVB
128200950113     C*
128300950113     C                   WRITE     FNBLPT00
128400950113     C*
128500950113     C                   Z-ADD     W0053         BLPVLB
128600950113     C                   MOVEL     W001A         BLPFVB
128700021127     c
128800950113     C                   ENDIF
128900950412     C* MANDO IN SEDE SOLO E' PREVISTA LA TRASMISISONE DAL CODICE BOLLA
129000950412     C     §3ABSD        IFEQ      ' '
129100070423     C                   WRITE     FNBLP046                             90
129200950412     C                   ELSE
129300950412     C                   MOVEL     'N'           BLPFT2
129400950412     C                   ENDIF
129500911119     C*
129600950109     C                   Z-ADD     W0070         BLPKSC
129700000320     C**
129800130527     c                   if        wblp01='S'
129900130527     C                   UPDATE    FNBLP001
130000130527     c                   else
130100950109     C                   UPDATE    FNBLP000
130200130527     c                   endif
130300010309     c*
130400010309     c* AGGIORNO E SFLAGGO ARCHIVI O.R.M.
130500060220     c* Dentro a questa routine devo chainare fIAR4 tipo record "M"
130600010309     c                   EXSR      AGGORM
130700931020     C*
130800011121     C* CREO FILE TASSAZIONE PADRONCINI fiftt00F E fiftd00F
130900931027     C                   EXSR      CRTTAS
131000151118     c* chiudo anomalia 20 se lnp/cliente con sospensione bolle in
131100151118     c* chiusura f.v.p.
131200151118     c                   if        w_sibsp='S'
131300151118     C                   MOVEL     020           comcaa
131400151118     c     kanm01        chain     fnanm01l
131500151118     c                   if        %found(fnanm01l)
131600151118     C                   CLEAR                   DSLR33
131700151118     C                   MOVE      WDTP          D33DCH
131800151118     C                   MOVEL     '1'           D33FSC
131900151118     C                   MOVEL     '  '          D33CCH
132000151118     C                   MOVE      ANMNR1        D33NRR
132100151118     C                   CALL      'FNLR33R'
132200151118     C                   PARM                    DSLR33
132300151118     c                   endif
132400151118     c                   endif
132500950116     C                   ENDIF
132600130530     c
132700130531   2aC                   endif
132800130531   2bC                   endif
132900910906     C*
133000130531     C     Kblp77        reade     FNBLP000                               22
133100130531    1C                   ENDDO
133200130531     c
133300130531     c                   add       1             I
133400130531     c                   enddo
133500900601     C*
133600011121     C* CREO fiftt00F PER ULTIMA LETTURA
133700931203     C                   EXSR      CRTFTT
133800931203     C*
133900900601     C                   ENDSR
134000130527     C**************************************************************************
134100130527     C*    CONTROLLO ESISTENZA FILIALE DI TRANSITO
134200130527     C**************************************************************************
134300130527     C     CTRFLP        BEGSR
134400130530     c                   clear                   wBORtran          1
134500130527     C*
134600130527     C* CONTROLLO SE C'E' TRANSITO
134700141029     C******             SETOFF                                       05
134800130527     C*
134900130531     C     flp(I)        IFgt      *zeros
135000130531     c                   movel     flp(I)        WFLP              3 0
135100130527     c
135200130527     C* C'E' TRANSITO SE :
135300130531     C* 1) FLP NON E' = ALLA LINEA DI ARRIVO
135400130531     C* 2) FLP NON E' = AL TERMINAL DI ARRIVO DELLA LINEA ARRIVO
135500130527     c
135600130531     C     savffv        IFNE      wFLP
135700130531     C     s_o54tfa      ANDNE     wFLP
135800130530     C                   eval      WBORtran='S'
135900130527     C                   END
136000130527     C                   END
136100130527     C*
136200130527     C                   ENDSR
136300130527     C**-------------------------------------------------------*
136400130527     C* Borderizzazione bolle e messa in sospensione automatica
136500130527     C**-------------------------------------------------------*
136600130527     C     Borderiz      BEGSR
136700130527     c                   clear                   wescludi          1
136800130610     c                   clear                   uncollo
136900150603     c                   clear                   w_sosp            1
137000130527     c
137100130527     C* SPEDIZIONE GIA' BORDERIZZATA               - PER BORDERO'
137200130527    1C     BLPDBR        IFNE      0
137300130527     c                   eval      WEscludi='S'
137400130530     c                   leavesr
137500130527    1C                   END
137600130527     c
137700130527     C* DATA SPEDIZIONE < = DATA BORDERO' IMMESSA  - PER BORDERO'
137800130530     c                   if        pa2barcode=' ' and
137900130530     c                             blpdsp>fgvdfv
138000130527     c                   eval      WEscludi='S'
138100130530     c                   leavesr
138200130527    1C                   END
138300130527     c
138400130527     c* Se codice cliente mittente non è quello immessa. escludo
138500130527     c                   clear                   alfaccm
138600130527    1c                   if        pa2ccm>*zeros
138700130527     c
138800130527    2c                   if        blpccm>0
138900130527     c                   movel     blpccm        alfaccm           7
139000130527     c                   else
139100130527     c                   movel     blpksc        alfaccm
139200130527    2c                   endif
139300130527     c
139400151127    2c***                if        alfaccm<>pa2ccm
139500151127     c                   if        %lookup(alfaccm:ccmb)=0
139600130527     c                   eval      WEscludi='S'
139700130530     c                   leavesr
139800130527    2C                   endif
139900130527    1C                   endif
140000130527     c
140100130527      * SE MANCA IL COD PADRONCINO LA BOLLA È UN PREPAGATO NON CONFERMATO.
140200130527      *  SE ANNO/MESE BOLLA < DI ANNO/MESE DEL BORDERO' FORZO IL CODICE
140300130527      *  AUTOTRASP. = 999, LA DATA RITIRO = DATA BOLLA, IL CODICE
140400130527      *  PRESTAZIONE = 'P' E BORDERIZZO.
140500130527      *  IN CASO CONTRARIO NON BORDERIZZO.
140600130527    1C     BLPPDR        IFEQ      0
140700130530     C                   MOVEL     BLPMGS        WMMSp
140800130530    2C                   If        BLPAAS < kaaa4 or WMMSp< kmmm
140900150930     c                             or (fgvdfv>= §DGDCPD and fgvdfv<= §DGDCPA)
141000130527     C                   Z-ADD     999           BLPPDR
141100130527     C                   movel     blplnp        BLPPDR
141200130527     C                   Z-ADD     BLPMGS        BLPDRT
141300130527     C                   MOVEL     BLPAAS        BLPDRT
141400130527     c
141500130527    3C     BLPFPP        IFEQ      *BLANKS
141600130527     C                   MOVEL     'P'           BLPFPP
141700130527    3C                   ENDIF
141800130527      *
141900130527   x2C                   ELSE
142000130527     c                   eval      WEscludi='S'
142100130530     c                   leavesr
142200130527    2C                   endif
142300130527     c*
142400130527   x1c                   else
142500130530     c
142600130530     c* Per borderizzazione BARCODE --> controllo se esiste spunta o anom 010
142700170502    2c******             if        pa2barcode= 'S'  or W_siBsp='S'
142800170502    2c                   if        pa2barcode= 'S'
142900130530     c                   exsr      CTRNSC
143000130530     c
143100130627    3c                   if        uncollo=' '
143200130530     c                   eval      WEscludi='S'
143300150603     c                   if        w_SiBsp='S'
143400150603     c                   eval      W_sosp  ='S'
143500150603     c                   endif
143600130530     c                   leavesr
143700130610    3C                   endif
143800130627    2C                   endif
143900170327     c
144000170502     c* Controllo analogo anche se per il cliente o la lnp è prevista sospension
144100170502     c* su FIBSP (cliente con colli in partenza da più filiali='S')
144200170502     c*  escluse le bolle legate figlie
144300170502    2c                   if        w_sibsp= 'S'
144400170502     c     kblp2         chain     fnlbl01l
144500170502    3c                   if        not %found(fnlbl01l)
144600170502     c                   exsr      CTRNSC
144700170502     c
144800170502    4c                   if        uncollo=' '
144900170502     c                   eval      WEscludi='S'
145000170502    5c                   if        w_SiBsp='S'
145100170502     c                   eval      W_sosp  ='S'
145200170502    5c                   endif
145300170502     c                   leavesr
145400170502    4C                   endif
145500170502    3C                   endif
145600170502    2C                   endif
145700170502
145800170327     c* Se bolla legata "figlia" senza spunte, il giorno della bollettazione
145900170327     c*  la sposto in automatico in sospensione
146000170421     c* Escludo lo bolle lnp 101
146100170327     c                   if        blpdsp=blpdim and blpnrs=0 and blpdse=0
146200170421     c                             and blpdpc=0 and blpdam=0 and blplnp<>101
146300170327     c     kblp2         chain     fnlbl01l
146400170327     c                   if        %found(fnlbl01l)
146500170413     c* visto che ci sono casi "strani" , eseguo anche il controllo
146600170413     c*   dei colli e del manca record
146700170413     c                   exsr      ctrnsc
146800170413
146900170413    3c                   if        uncollo=' '
147000170327     c                   eval      WEscludi='S'
147100170327     c                   eval      W_sosp  ='S'
147200170327     c                   endif
147300170413     c
147400170413     c                   endif
147500170327     c                   endif
147600130610     c*
147700130530    1c                   endif
147800130527     c
147900130527     C                   ENDSR
148000130530     C**************************************************************************
148100130530     C*    CONTROLLO ESISTENZA SPUNTE PER LA BOLLA - borderizz.barcode
148200130530     C**************************************************************************
148300130530     C     CTRNSC        BEGSR
148400130530     C*
148500130530     C                   SETOFF                                       10
148600130530     c                   clear                   uncollo           1
148700130530     C* CONTROLLO SE CTM NON PREVEDE LA SPUNTA DEI SEGNACOLLI:
148800130530     C*  A BORDERO' LO STESSO (FLAG 5 DEL CTM)
148900170413     c* questo vale sempre: anche per la sospensione automatica
149000170413     c*  per sicurezza lo faccio anche se non dovrebbe mai accadere
149100170413     c***                if        pa2barcode= 'S'
149200130530     C     BLPCTM        LOOKUP    CTM                                    10
149300130530     c                   if        *in10
149400130530     C                   eval      uncollo='S'
149500130530     C                   leavesr
149600130530     C                   endif
149700170413     c***                endif
149800151118
149900151118     c                   if        w_sibsp='S'
150000151118     C                   MOVEL     020           comcaa
150100151118     c     kanm01        chain     fnanm01l
150200151118     c                   if        %found(fnanm01l) and anmdch=0
150300151118     C                   eval      uncollo='S'
150400151118     C                   leavesr
150500151118     c                   endif
150600151118     c                   endif
150700130530     c
150800130530     C* SI SPUNTA: CONTROLLO SE ALMENO UN SEGNACOLLO SPUNTATO C'E'
150900130530     C     KBLp2         SETLL     FNBLT01L
151000130530     C     KBLp2         READE(n)  FNBLT01L
151100130530     C*
151200130530    1C                   DOW       not %eof(fnblt01l)
151300130610     c
151400130530     C* LA FILIALE IN GESTIONE PER BORDERO' E' SIMFEL
151500150603     c                   if        pa2barcode='S'
151600130530     C                   Z-ADD     1             KNPG
151700130530     C                   Z-ADD     SIMFEL        KFGS
151800130530     C                   Z-ADD     fgv2          wnfv60
151900130530     c* BARCODE
152000130530     C     KBRVbor       SETLL     FNBRV01L                               31
152100150603     c                   else
152200150603     C     KBRV7         SETLL     FNBRV07L                               31
152300150603     c                   endif
152400130530    3c                   if        *in31
152500130530     C                   SETON                                        10
152600130530     C                   eval      uncollo='S'
152700130530     c                   leavesr
152800130530    3c                   endif
152900130530     c
153000130530     c* non trovata spunta ma presente manca record bolla in arrivo ancora
153100130530     c* aperta --> borderizzo lo stesso
153200130530    3c                   if        *in31=*off
153300130607     C                   MOVEL     010           comcaa
153400130530     c     kanm1         setll     fnanm02l
153500130530    4c                   if        %equal(fnanm02l)
153600130607     C                   eval      uncollo='S'
153700130530     c                   seton                                        10
153800130530     c                   leavesr
153900130530    4c                   endif
154000130530    3c                   endif
154100130530     c
154200130607     C     kblp2         READE(n)  FNBLT000
154300130530    1C                   ENDdo
154400130530     C*
154500130530     C                   ENDSR
154600130527     C**************************************************************************
154700130527     C*    CONTROLLO anomalia 110 105 da chiudere
154800130527     C**************************************************************************
154900130527     C     Contr_Anom    BEGSR
155000130527     C* ANOMALIA--> "NO BOLLA BORDERIZZATA" CHIUDO
155100130607     C                   MOVEL     110           comcaa
155200130527     C     KANM1         CHAIN(N)  FNANM02L                           32
155300130527     C     *IN32         IFEQ      *OFF
155400130607     C                   EXSR      call_lr33
155500130527     C                   END
155600130527     C*
155700130527     C* ANOMALIA --> "MANCA BOLLA"          CHIUDO
155800130607     C                   MOVEL     105           comcaa
155900130527     C     KANM1         CHAIN(N)  FNANM02L                           32
156000130527     C**
156100130527     C* SE C'E' GIA' CHIUSA COME ANNULLATA --> COME SE NON CI FOSSE
156200130527     C* (P.ES SE LA 105 ERA STATA ANNULLATA MA SUCCESSIVAMENTE LA
156300130527     C* SPEDIZIONE VIENE BOLLETTATA. IN QUESTO CASO IL PGM DEVE)
156400130527     C* AGGIORNARE BLTNFV)
156500130527    1C     *IN32         IFEQ      *OFF
156600130527     C     ANMDCH        ANDEQ     0
156700130527     C*
156800130527     C     *IN32         OREQ      *OFF
156900130527     C     ANMDCH        ANDGT     0
157000130527     C     ANMCCH        ANDNE     'AN'
157100130527     C**
157200130527    2C     ANMDCH        IFEQ      0
157300130527     C                   CLEAR                   WSPE
157400130527     C*
157500130527     C* CERCO LA SPUNTA ENTRATA PER AGGIORNARE LA DATA ENTRATA
157600130527     C                   MOVEL     '5'           KNPG
157700130527     C                   MOVE      *HIVAL        WVVDFV
157800130527     C     KBRV1         SETLL     FNBRV09L                           32
157900130527     C     KBRV1         READE     FNBRV09L                               32
158000130527     C*
158100130527     C     *IN32         DOWEQ     *OFF
158200130530     C                   CLEAR                   DSLV53
158300130527     C                   MOVEL     'V'           D53TFO
158400130527     c                   exsr      data_spunta
158500130527     c
158600130527     C     KBRV1         READE     FNBRV09L                               32
158700130527     C                   ENDDO
158800130527     C*
158900130527     C                   MOVEL     '1'           KNPG
159000130527     C     KBRV1         CHAIN     FNBRV09L                           32
159100130527     C     *IN32         DOWEQ     *OFF
159200130527     C                   MOVEL     'S'           WBRV1             1
159300130527     C* SE ESISTE UNA SPUNTA PARTENZA CON DATA < DELLA DATA PRIMA SPUN-
159400130527     C* TA ENTRATA --> MEMORIZZO I DATI DELLA SPUNTA PARTENZA
159500130530     C                   CLEAR                   DSLV53
159600130527     C                   MOVEL     'G'           D53TFO
159700130527     c                   exsr      data_Spunta
159800130527
159900130527     C     KBRV1         READE     FNBRV09L                               32
160000130527     C                   ENDDO
160100130527     C*
160200130527     C* TROVATA SPUNTA PARTENZA
160300130527    3C     WBRV1         IFEQ      'S'
160400130527     C* CHIAMO IL PGM DI AGGIORNAMENTO BOLLE PER SCRIVERE LA DATA ENTRA
160500130527     C* TA
160600130527     C* SOLO SE FV CHIUSO AGGIORNO LA BOLLA
160700130527     c* Es 27 05 13 : non lo faccio sempre se c'e' anomalia 105 significa
160800130527     c*  che la spunta è stata già elaborata dalla chiusura FGV
160900130527     c*  quindi il collo/bolla devono essere aggioranti a prescindere
161000130527    4C**** D53ERR        IFEQ      *BLANKS
161100130527     C**** D53FCF        ANDEQ     'S'
161200130527     C*
161300130527     C                   Z-ADD     BRVNPS        BLTNPP
161400130527     C                   Z-ADD     D53NFV        BLTNFV
161500130527     C                   Z-ADD     D53DFV        BLTDFV
161600130527     C*
161700130527     C* AGGIORNO LA DATA PARTENZA PRIMO COLLO SULLA BOLLA
161800130527    5C     BLPDPC        IFEQ      0
161900130527     C                   Z-ADD     BLTDFV        BLPDPC
162000130527     C* SOMMO LA SPEDIZIONE DI UN COLLO "EX MANCA BOLLA"
162100130527     C                   Z-ADD     1             WSPE
162200130527    5C                   END
162300130527     C*
162400130527     C* CALCOLO PESO E VOLUME UNITARI DI UN COLLO "EX MANCA BOLLA"
162500130527     C                   EXSR      TOTFVD
162600130527     C*
162700130527     C* AGGIORNO FNSTA PER LA RIELABORAZIONE DELLE STATISTICHE
162800130527     c                   If        BltDfv <= §SdfParU
162900130527     C                   Z-ADD     BLTDFV        SA2DTS
163000130527     C                   MOVE      'P'           SA2TRC
163100130527     C                   CALL      'FNLSA2R3'
163200130527     C                   PARM                    SA2TLA
163300130527     C                   PARM                    SA2TRC
163400130527     C                   PARM                    SA2DTS
163500130527     C                   PARM                    WWLNA             3 0
163600130527     C                   MOVEL     'S'           WLSA2             1
163700130527    5C                   END
163800130527     C*
163900130527     c                   If        BltDfv <= §SdfTerU
164000130527     C                   MOVE      BLTDFV        SA2DTS
164100130527     C                   MOVE      'T'           SA2TRC
164200130527     C                   CALL      'FNLSA2R3'
164300130527     C                   PARM                    SA2TLA
164400130527     C                   PARM                    SA2TRC
164500130527     C                   PARM                    SA2DTS
164600130527     C                   PARM                    WWLNA             3 0
164700130527     C                   MOVEL     'S'           WLSA2             1
164800130527    5C                   END
164900130527     C*
165000130527     C* SE FOGLIO APERTO NON AGGIORNO LA BOLLA E MEMORIZZO IL FOGLIO
165100130527     C*  SU CUI STO BORDERIZZANDO (non dovrebbe più accadere)
165200130527   X4C****               ELSE
165300130527     C*
165400130527     C****               Z-ADD     VIDNFV        BLTNFV
165500130527    4C****               ENDIF
165600130527     C*
165700130527    3C                   ENDIF
165800130527     C* SE TROVATA SPUNTA CLEARO DATA ENTRATA E VOLUME CML PRIMA
165900130527     C* AFFINCHE' L'FNLS45R AGGIORNI QUESTI CAMPI IN BASE ALLA PRIMA
166000130527     C* SPUNTA PASSATA SUL SISTEMA
166100130527    4C     WBRV5         IFEQ      'S'
166200130527     C                   CLEAR                   BLTDSE
166300130527    5C     W5VUC         IFGT      *ZEROS
166400130527     C                   CLEAR                   BLTVUC
166500130527    5C                   END
166600130527    4C                   END
166700130527     C*
166800130607     C                   EXSR      call_lr33
166900130527    2C                   END
167000130527     C                   ELSE
167100130527   X1C*
167200130527     C                   Z-ADD     FGV2          BLTNFV
167300130527    1C                   ENDIF
167400130527     C*
167500130527     c
167600130527     C                   ENDSR
167700130527      ****************************************************************
167800130527      * Prendo data spunta
167900130527      ****************************************************************
168000130527     c     Data_spunta   BEGSR
168100130527     C* DETERMINO DATA FOGLIO
168200130527     C                   MOVE      BRVNPG        D53NPG
168300130527     C                   MOVE      BRVNFV        D53NFV
168400130527     C                   MOVE      BRVFGS        D53FGS
168500130527     C                   MOVE      SIMFEL        D53FEL
168600130527     C                   CALL      'FNLV53R'
168700130530     C                   PARM                    DSLV53
168800130527     C                   MOVE      '1'           WLV53             1
168900130527     C*
169000130527     C     D53ERR        IFEQ      *BLANKS
169100130527     C     D53DFV        iflt      WVVDFV
169200130527     c* se è presente il peso e volume nella spunta meno recente
169300130527     c*  li devo mantenere per il richiamo a fnls45r
169400130527     c
169500130527     c                   if        brvvuc=0  and w5vuc>0
169600130527     c                   eval      brvvuc=w5vuc
169700130527     c                   endif
169800130527     c                   if        brvpuc=0  and w5puc>0
169900130527     c                   eval      brvpuc=w5puc
170000130527     c                   endif
170100130527     c                   if        brvcan=' ' and w5can<>' '
170200130527     c                   eval      brvcan=w5can
170300130527     c                   endif
170400130527     c
170500130527     C                   MOVEL     'S'           WBRV5             1
170600130527     c                   movel     dsbrv         dsbrvw5
170700130527     C                   MOVE      D53DFV        WVVDFV
170800130527     c* se c'e' il peso e volume --> lo passo per il calcolo
170900130527     c                   else
171000130527     c                   if        w5vuc=0  and brvvuc>0
171100130527     c                   eval      w5vuc=brvvuc
171200130527     c                   endif
171300130527     c                   if        w5puc=0  and brvpuc>0
171400130527     c                   eval      w5puc=brvpuc
171500130527     c                   endif
171600130527     c                   if        w5can=' ' and brvcan<>' '
171700130527     c                   eval      w5can=brvcan
171800130527     c                   endif
171900130527     c
172000130527     C                   END
172100130527     C                   END
172200130527     c                   ENDSR
172300130527     C**************************************************************************
172400130527     C*    CALCOLO PESO E VOLUME UNITARI
172500130527     C**************************************************************************
172600130527     C     TOTFVD        BEGSR
172700130530     c                   exsr      CALUNI
172800130527     C*
172900130527     C                   Z-ADD     BRVNFV        WNFV5
173000130603     C                   MOVEL     '1'           WTRC
173100130530     C     KFVDbor       CHAIN     FNFVD000                           34
173200130527     C   34              CLEAR                   FNFVD000
173300130527     C***  COLLI ***
173400130527     C                   ADD       1             FVDCLA
173500130530     c                   clear                   wtran
173600130527     C*
173700130527     C***  SPEDIZIONI  ***
173800130527     C     WSPE          IFGT      0
173900130530     C                   z-add     WSPE          wspen
174000130527     C                   ENDIF
174100130530     c
174200130530     c                   exsr      RIEFVD
174300130527     C*
174400130527     C* ABBLENCO FLAG E DATA TRASMISSIONE PER INVIO IN SEDE
174500130527     C                   MOVEL     ' '           FVDFTR
174600130527     C                   Z-ADD     0             FVDDTR
174700130527     C*
174800130527     C*
174900130527     C     *IN34         IFEQ      *ON
175000130527     C                   Z-ADD     BRVNFV        FVDNFV
175100130527     C                   MOVEL     '1'           FVDTRC
175200130527     C                   MOVEL     SIMFEL        FVDLNP
175300130527     C                   MOVEL     BLPLNA        FVDLNA
175400130527     C                   WRITE     FNFVD000
175500130527     C                   ELSE
175600130527     C*
175700130527     C                   UPDATE    FNFVD000
175800130527     C                   ENDIF
175900130527     C*
176000130527     C                   ENDSR
176100950109     C**-----------------------------------------------*
176200950109     C* SCRITTURA DI ALCUNI FILE DELLA BOLLA -----------
176300950109     C**-----------------------------------------------*
176400950109     C     WRITEF        BEGSR
176500060220     c
176600030107      *  Estensione Testata Bolla    F I A R 5 0 0 F
176700030107     c     Kblp2         Setll     Fiar501l
176800030203Do  1c                   Do        *Hival
176900030107     c     Kblp2         Reade     Fiar501l
177000030107      * Fine File esco dal ciclo
177100030107     c                   If        %Eof(Fiar501l)
177200030107     c                   Leave
177300030107     c                   EndIf
177400030110      * Controllo se già inviato
177500030110     c                   If        Ar5Dt1 <> *all'9' and Ar5Dt1 > 0
177600030110     c                   Iter
177700030110     c                   EndIf
177800030107      * Flaggo per la trasmissione anche se il record non viene trasmesso
177900030107     c                   Eval      Ar5Ft1 = 'T'
178000030107     c                   Z-Add     Dateu         Ar5Dt1
178100030107     c                   Update    Fiar5000
178200030203    1c                   EndDo
178300030107
178400950109     C                   ENDSR
178500900608     C**-----------------------------------------------*
178600950109     C** TRASFERISCO BOLLE  - 99999 -
178700900608     C**-----------------------------------------------*
178800000619     C     TRAS99        BEGSR
178900030107
179000030107     c                   Open      Fiftt01l
179100910906     C*
179200130702     C                   SETOFF                                       01
179300920221     C                   Z-ADD     0             SAVLNA
179400130527     C     K02BLP77_9    reade     FNBLP000                               22
179500900608     C*
179600950323    1C     *IN22         DOWEQ     '0'
179700010109      *
179800010109      * SE MANCA IL COD PADRONCINO LA BOLLA È UN PREPAGATO NON CONFERMATO.
179900010111      *  SE ANNO/MESE BOLLA < DI ANNO/MESE DEL JOB      CONFERMO FORZANDO
180000010109      *  COD.AUTOTRASP. = 999, LA DATA RITIRO = DATA BOLLA E IL CODICE
180100010109      *  PRESTAZIONE = 'P'. IN CASO CONTRARIO NON CONFERMO.
180200010109     C                   IF        BLPPDR = *zeros
180300010109     C                   movel     BLPMGS        WMMSp             2 0
180400010111     C                   movel     DATEU         WAAJb             4 0
180500010111     C                   move      dateu         W0040             4 0
180600010111     C                   movel     W0040         WMMJb             2 0
180700010109      *
180800010111     C                   If        BLPAAS < WAAJb or WMMSp < WMMJb
180900150930     c                             or (fgvdfv>= §DGDCPD and fgvdfv<= §DGDCPA)
181000010109     C                   z-add     999           BLPPDR
181100010109     C                   z-add     BLPMGS        BLPDRT
181200010109     C                   movel     BLPAAS        BLPDRT
181300010109     C                   If        BLPFPP = *BLANKS
181400010109     C                   eval      BLPFPP = 'P'
181500010109     C                   ENDIF
181600010109     C                   ENDIF
181700010109      *
181800010109     C                   ENDIF
181900010109      *
182000010109      *  PER I PREPAGATI, GIA' CONFERMATI
182100010109   1AC     BLPPDR        IFGT      0
182200990915     C*
182300950115     C* ELABORO SOLO LE BOLLE NON ANCORA TRASMESSE
182400991028    2C     BLPDT2        IFEQ      0
182500991028     C     BLPDT1        OREQ      0
182600920221     C*
182700940107     C* CHAIN CON CODICE BOLLA
182800940107     C                   EXSR      CTABEL
182900030107      * Aggiorno Fiftt con il numero di ritiri annullati
183000030107      * bolle "FY"
183100030107     c                   If        BlpCbo = 'FY'
183200030107     c                   Exsr      CrtFtt1
183300030107     c                   EndIf
183400000619     C*
183500920221     C* INVIO ANCHE PER FILIALE
183600950412    3C     §3ABFI        IFEQ      ' '
183700960411     C* SE E' "£" INVIO SOLO SE LNP<>LNA
183800960411     C     BLPCBO        OREQ      '£ '
183900960411     C     BLPLNP        ANDNE     BLPLNA
184000920221     C*
184100920221     C** L I N E A   A R R I V O   C A M B I A T A
184200950323    4C     SAVLNA        IFNE      BLPLNA
184300031112     C**                 CLEAR                   WLOCAL
184400971105     C                   SETOFF                                       01  50
184500950109     C*
184600130531     C                   CLEAR                   dslv55
184700140403     c                   z-add     blptfp        d55lnp
184800130531     C                   Z-ADD     BLPLNA        d55LIN
184900130531     C                   Z-ADD     PARDTP        d55DRF
185000130531     C                   CALL      'FNLV55R'
185100130531     c                   PARM                    dslv55
185200130531    5C     d55err        IFEQ      *BLANKS
185300130531     c                   z-add     d55tfp        s_o54tfp
185400130531     c                   z-add     d55tfa        s_o54tfa
185500130531     c* se secondo livello in arrivo
185600031219     c* invio la bolla al terpar del ter-arr della lna
185700130531    6c     blplna        ifne      d55tfa
185800130531     c                   clear                   dslv55
185900130531     C                   MOVEL     'P'           d55TPT
186000130531     C                   MOVEL     s_o54tfa      d55LIN
186100130531     C                   MOVEL     pardtp        d55DRF
186200130531     C                   CALL      'FNLV55R'
186300130531     C                   PARM                    dslv55
186400130531    7c     d55err        ifeq      *blanks
186500031219     c                   seton                                        01
186600130531     c                   movel     d55tfp        varx
186700031219    7c                   endif
186800031219   x6c                   else
186900031219     c* se primo liv. in arrivo o ogas diversi
187000031219     c* invio al terminal di partenza di lna
187100031219     c                   seton                                        01
187200031219     c                   movel     s_o54tfp      varx
187300031219    6c                   endif
187400031112    5c                   endif
187500031112     C     N_varx        LOOKUP    FEL                                    32
187600950323    5C     *IN32         IFEQ      *OFF
187700031112     C                   MOVEL     N_varx        VAR2X
187800031112     C                   MOVEL     N_varx        NUM3
187900950115     C                   EXSR      LCKBLP
188000031112     C                   Z-ADD     1             IY                3 0
188100031112     C     000           LOOKUP    FEL(IY)                                32
188200031112     C   32              Z-ADD     N_varx        FEL(IY)
188300950323    5C                   ENDIF
188400950115     C*
188500031112     c   50              setoff                                       01
188600950115     C*
188700031112     c     *in01         ifeq      *on
188800950109     C                   EXSR      CAEX
188900971106     C* SE BOLLA LOCALE MEMORIZZO CHE CE N'è ALMENO UNA
189000130531     c     simfel        ifeq      s_o54tfa
189100971106     C                   SETON                                        97
189200031112     C**                 MOVEL     'S'           WLOCAL
189300971106     C                   END
189400950323    5C                   ENDIF
189500950115     C*
189600950109     C                   MOVEL     BLPLNA        SAVLNA
189700950323    4C                   ENDIF
189800920221     C*
189900920427     C* SE E' DA TRASMETTERE SOLO IN SEDE NON APRO MEMBRI
190000950323   X3C                   ELSE
190100950109     C*
190200950109     C* IMPOSTO I FLAG DI TRASMISSIONE
190300950109     C                   MOVEL     'N'           BLPFT1
190400950109     C                   Z-ADD     DATEU         BLPDT1
190500940415     C*
190600940415     C* SE CODICE BOLLA FRANCO RECUPERO MUOVO DATA BORDERO' IN DCM/DAM
190700060303     c* stessa cosa per cod. bolla "M"-Prepagato annullato
190800950323    4C     BLPCBO        IFEQ      'B '
190900060303    4C     BLPCBO        oreQ      'M '
191000030728      * prima però imposto la data borderò uguale alla data foglio viaggio
191100030728     c                   Z-Add     FgvDfv        BlpDbr
191200920615     C                   MOVEL     BLPDBR        BLPDCM
191300920615     C                   MOVEL     BLPDBR        BLPDAM
191400950323    4C                   ENDIF
191500950323    3C                   ENDIF
191600950115     C*
191700950115     C* SOLO SE HO ALLOCATO I MEMBRI
191800950915    3C     *IN50         IFEQ      *OFF
191900911119     C*
192000060220     C* FLAGGO FIAR500F: LO FACCIO A PREDCINDERE DALLA TRASMISSIONE DELLA BO
192100060220     C                   EXSR      WRITEF
192200060220     C
192300991029     C* IN QUELLO PER ARRIVO SE 9999 METTO LA LN ARRIVO
192400950109     C                   Z-ADD     BLPKSC        W0070
192500950915     C                   MOVE      BLPKSC        COMKSC            4 0
192600950323    4C     COMKSC        IFEQ      9999
192700911119     C                   MOVEL     BLPLNA        BLPKSC
192800950323    4C                   END
192900950323     C*
193000991028     C     BLPDT3        IFEQ      0
193100991028     C                   Z-ADD     DATEU         BLPDT3
193200991028     C                   MOVEL     'S'           BLPFT3
193300991028     C                   ENDIF
193400991028     C*
193500991028    4C     BLPDT2        IFEQ      0
193600950109     C                   Z-ADD     DATEU         BLPDT2
193700950109     C*
193800991028    5C     §3ABSD        IFEQ      ' '
193900950109     C                   MOVEL     'T'           BLPFT2
194000070423     C                   WRITE     FNBLP046                             90
194100950109     C                   ELSE
194200950109     C                   MOVEL     'N'           BLPFT2
194300991028    5C                   ENDIF
194400991028    4C                   ENDIF
194500920221     C*
194600991028    4C     BLPDT1        IFEQ      0
194700991028    5C     §3ABFI        IFEQ      ' '
194800960411     C     BLPCBO        OREQ      '£ '
194900960411     C     BLPLNP        ANDNE     BLPLNA
195000991029     C*
195100060220     C* IL FLAG DDT LO METTO IN BL2SV1
195200060220     C                   MOVEL     BLPFST        BL2SV1
195300060220     C* IL FLAG sigla operatore video lo metto in bl2cei
195400000619     C                   MOVE      BLPSOP        BL2CEI
195500991029     C* IMPOSTO I FALG DI TRASMISSIONE
195600991029     C                   MOVEL     'T'           BLPFT1
195700991029     C                   Z-ADD     DATEU         BLPDT1
195800991029     C**
195900950109     C   01              WRITE     FNBLPE00
196000991028    5C                   ENDIF
196100991028    4C                   ENDIF
196200900608     C*
196300950109     C                   MOVEL     W0070         BLPKSC
196400950323     C* IMPOSTO IL FOGLIO A 99998 --> PER DIRE CHE L'HO GIA ELABORATA
196500950323     C                   Z-ADD     99998         BLPNFV
196600950109     C                   UPDATE    FNBLP000
196700950323    3C                   ENDIF
196800950323     C*
196900950323     C* SE BOLLA GIA' TRASMESSA --> IMPOSTO SOLO IL FOGLIO A 99998
197000950323   X2C                   ELSE
197100950323     C                   Z-ADD     99998         BLPNFV
197200950323     C                   UPDATE    FNBLP000
197300950323    2C                   ENDIF
197400990915   1AC                   ENDIF
197500910906     C*
197600130527     C     K02BLP77_9    reade     FNBLP000                               22
197700950323    1C                   ENDDO
197800900608     C*
197900900608     C                   ENDSR
198000931022     C*
198100011121     C*--- SCRIVO FILE TASSAZIONE  fiftt00F/fiftd00F -----------------*
198200931019     C     CRTTAS        BEGSR
198300931203     C***
198400931203     C*  C R E A Z I O N E    F L F T T 0 0 F
198500931203     C***
198600011121     C* CONTROLLO SE ESISTE IL FILE fiftt OGNI VOLTA CHE CAMBIANO IL
198700931203     C*   CODICE PADRONCINO E LA DATA RITIRO
198800931203     C*
198900931203    1C     COMBLP        IFNE      COMSAV
199000931203     C*
199100931203     C                   EXSR      CRTFTT
199200931203     C*
199300931203     C* CONTROLLO IL PADRONCINO OGNI VOLTA CHE CAMBIA
199400950113     C     WCPR          IFNE      BLPPDR
199500931203     C* IL COD.PADRONCINO DELLA BOLLA DEVE ESISTERE NELLA SCHIERA PDR
199600040826     C                   Z-ADD     1             W                 4 0
199700140620     C                   SETOFF                                       05
199800950113     C     BLPPDR        LOOKUP    PDR(W)                                 05
199900950113     C                   Z-ADD     BLPPDR        COMPDR
200000931203     C                   ENDIF
200100931203     C*
200200950113     C                   MOVEL     COMBLP        COMSAV
200300931203    1C                   ENDIF
200400970930     C*
200500971027    1C     *IN05         IFEQ      *ON
200600931027     C* CONTROLLO SE IL TIPO BOLLA E' DA ESCLUDERE
200700931027     C                   EXSR      CTAB
200800931027     C*
200900011121     C* ALF1 = '1' ===> CREO I FILE fiftt00F E fiftd00F  SE PDR NON DIP
201000971027     C*                 CREO COMUNQUE FILE FNSTPR0F PER TRASMISSIONE
201100971027     C*                 STOP. PER I NON DIPENDENTI, IL CALCOLO STOP IN
201200971027     C*                 QUESTO MOMENTO E' SOLO PREVENTIVO. VERRA' RI-
201300971027     C*                 FATTO AL MOMENTO DELLA CONFERMA CONTEGGI
201400020528     c*                 28/05/02:Creo dati per conteggi anche per i dipenden
201500020528     c*                          ti
201600971027    2C     ALF1          IFEQ      '1'
201700931203     C* NUMERO SPEDIZIONI TOTALE
201800020528     C*  05              ADD       1             COMSET
201900020528     C                   ADD       1             COMSET
202000931202     C*
202100931019     C* IMPOSTO CAMPI CHIAVE
202200971027    3C     BLPFPP        IFEQ      'M'
202300950113     C                   Z-ADD     1000          COMNDC
202400971027   X3C                   ELSE
202500950113     C                   Z-ADD     5000          COMNDC
202600971027    3C                   ENDIF
202700950113     C                   ADD       BLPNRT        COMNDC
202800950113     C                   MOVEL     BLPRSM        COMRSC
202900950114     C                   MOVEL     BLPDRT        COMDDC
203000931019     C*
203100971027    3C     COMTB1        IFEQ      'F'
203200931202     C                   MOVEL     BLPKSC        CO2KSC
203300971027   X3C                   ELSE
203400971027    4C     BLPCCM        IFNE      0
203500931202     C                   MOVEL     BLPCCM        CO2KSC
203600971027   X4C                   ELSE
203700931202     C                   MOVEL     BLPKSC        CO2KSC
203800950801     C                   MOVE      CO2KSC        W0040             4 0
203900971027    5C     W0040         IFEQ      9999
204000950801     C                   MOVE      8888          CO2KSC
204100971027    5C                   END
204200971027    4C                   ENDIF
204300971027    3C                   ENDIF
204400931202     C*
204500931019     C*
204600931202     C***
204700970930     C*  C R E A Z I O N E    F N F T D 0 0 F
204800931202     C***
204900020325     c                   clear                   wstp
205000020325     C* ESCLUDENDO FRANCHI E ASSEGNATI VARI,
205100020325     C*   CHAINO FNCLS00F PER PRENDERE IL NUMERO DEGLI STOP
205200020325     C                   Z-ADD     CO2KSC        NUM4              4 0
205300020325    5C     NUM4          IFNE      *ALL'8'
205400020325     C     NUM4          ANDNE     *ALL'9'
205500020325     C     CO2KSC        CHAIN     FNCLS000                           33
205600020325     C  N33              z-add     CLSSTP        wstp
205700020325    5C                   ENDIF
205800970930     C*
205900020325     c     wstp          ifeq      *zeros
206000011121     C     KFTD          CHAIN     fiftd000                           32
206100020325     c  n32              z-add     ftdstp        wstp
206200020326     c                   endif
206300020326     c     wstp          ifeq      *zeros
206400020416     c* Se il numero stop da attribuire è ancora = a 0 devo cercare
206500020416     c* nella distinta il primo numero disponibile
206600020416     C     KFTD1         setgt     fiftd001
206700020416     C     KFTD1         readpe    fiftd001
206800020416     c                   if        not %eof
206900020416     c                   eval      wstp = (ftdstp+1)
207000020416     c     wstp          iflt      1000
207100020416     c                   z-add     1000          wstp
207200020416     c                   endif
207300020417   x6c                   else
207400020417     c                   z-add     1000          wstp
207500020416     c                   endif
207600020325     c                   endif
207700020325     C                   CLEAR                   fiftd000
207800931108     C*
207900020325     c                   z-add     blpaas        ftdaas
208000020325     c                   z-add     blplnp        ftdlnp
208100020325     c                   z-add     blpnrs        ftdnrs
208200020325     c                   z-add     blpnsp        ftdnsp
208300931123     C                   MOVEL     BLPCAM        FTDCAP
208400020325     C                   z-add     1             FTDSET
208500931108     C                   MOVEL     'N'           FTDFGC
208600020325     c                   z-add     wstp          ftdstp
208700931108     C* FLAG ANTEPORTO
208800030902     C** IMPOSTO SEMPRE ANCHE SE C'ERA GIA' IL FLAG ANTEPORTO
208900931108     C                   MOVEL     BLPFAP        FTDFIN
209000931108     C*
209100021108     C* VOLUME : - SE TOTALE   ("T"), SOMMO BLPVLc
209200021108     C*          - SE PARZIALE ("Z"), SOMMO BLPVLc (SE MAGGIORE)
209300021108     C*                               ALTRIMENTI SOMMO BLPVLF
209400050112     C     BLPNCR        IFEQ      BLPNCL
209500950113     C                   Z-ADD     BLPVLC        WVOL
209600940321     C*
209700971027   X4C                   ELSE
209800971027    5C     BLPVLC        IFGE      BLPVLF
209900950113     C                   Z-ADD     BLPVLC        WVOL
210000971027   X5C                   ELSE
210100950113     C                   Z-ADD     BLPVLF        WVOL
210200971027    5C                   ENDIF
210300971027    4C                   ENDIF
210400021108     C* peso   : - SE TOTALE  , SOMMO BLPpkc
210500021108     C*          - SE PARZIALE, SOMMO BLPPKC (SE MAGGIORE)
210600021108     C*                               ALTRIMENTI SOMMO BLPPKF
210700050112     C     BLPNCp        ifeq      BLPNCL
210800021108     C                   Z-ADD     BLPpkC        Wpes
210900021108     C*
211000021108   X4C                   ELSE
211100021108    5C     BLPpkC        IFGE      BLPpkF
211200021108     C                   Z-ADD     BLPpkC        Wpes
211300021108   X5C                   ELSE
211400021108     C                   Z-ADD     BLPpkF        Wpes
211500021108    5C                   ENDIF
211600021108    4C                   ENDIF
211700940321     C*
211800931202     C*
211900931202     C                   Z-ADD     BLPNCL        FTDNCL
212000021108     C                   Z-ADD     WPES          FTDPKL
212100950113     C                   Z-ADD     WVOL          FTDVLU
212200950113     C* CALCOLO I COLLI PER CUI APPLICARE ETICHETTATURA: ESCLUDO
212300050112     C*  LE BOLLE CON SERIE
212400971027    5C     BLPNRS        IFEQ      0
212500950113     C                   Z-ADD     BLPNCL        FTDNCE
212600971027    5C                   ENDIF
212700931019     C*
212800931108     C* IMPOSTO CAMPI CHIAVE
212900931019     C                   Z-ADD     COMPDR        FTDPDR
213000020325     C                   movel     COMPDR        FTDfgs
213100931019     C                   MOVEL     COMTSR        FTDTSR
213200931019     C                   Z-ADD     COMNDC        FTDNDC
213300950115     C                   Z-ADD     COMDDC        FTDDDC
213400950113     C                   MOVEL     COMRSC        FTDRSC
213500931202     C                   MOVEL     CO2KSC        FTDKSC
213600931019     C*
213700011121     C                   WRITE     fiftd000
213800931019     C*
213900970930     C***
214000970930     C*  C R E A Z I O N E    F N S T P R 0 F
214100970930     C***
214200970930     C                   Z-ADD     BLPAAS        STRAAS
214300970930     C                   Z-ADD     BLPLNP        STRLNP
214400970930     C                   Z-ADD     BLPNRS        STRNRS
214500970930     C                   Z-ADD     BLPNSP        STRNSP
214600970930     C                   Z-ADD     COMPDR        STRPDR
214700970930     C                   Z-ADD     COMDDC        STRDDC
214800970930     C                   Z-ADD     COMNDC        STRNDC
214900970930     C* NUMERO STOP
215000970930     C                   Z-ADD     CO2KSC        NUM4              4 0
215100970930     C                   CLEAR                   STRSTP
215200000615     C* imposto numero stop = 99999 se merce portata a magazzino
215300970930     C     BLPFDN        IFEQ      'S'
215400000615     C                   Z-ADD     99999         STRSTP
215500970930     C                   ELSE
215600970930     C* chaino FNCLS00F per prendere l'eventuale numero stop
215700970930     C     NUM4          IFNE      *ALL'8'
215800970930     C     NUM4          ANDNE     *ALL'9'
215900970930     C     CO2KSC        CHAIN     FNCLS000                           33
216000001020     C  N33              Z-ADD     CLSSTP        STRSTP
216100970930     C                   ENDIF
216200970930     C                   ENDIF
216300970930     C                   MOVEL     CO2KSC        STRKSC
216400970930     C                   MOVEL     COMRSC        STRRSC
216500970930    3C                   SELECT
216600970930     C     §3ABSD        WHENEQ    *BLANKS
216700970930     C     §3ABFI        ANDEQ     *BLANKS
216800970930     C                   MOVEL     'E'           STRINV
216900970930     C     §3ABSD        WHENEQ    *BLANKS
217000970930     C                   MOVEL     'S'           STRINV
217100970930     C     §3ABFI        WHENEQ    *BLANKS
217200970930     C                   MOVEL     'F'           STRINV
217300970930    3C                   ENDSL
217400970930     C                   WRITE     FNSTPR00
217500931019     C*
217600931019     C                   ENDIF
217700970930     C                   ENDIF
217800931019     C                   ENDSR
217900931019     C*
218000011121     C*--- CREAZIONE fiftt00F ----------------------------------------*
218100931203     C     CRTFTT        BEGSR
218200931203     C*
218300931203    2C     COMSET        IFGT      0
218400950113     C     WCPR          ANDGT     0
218500931203     C                   Z-ADD     WCPR          WPDR
218600950113     C     WFPP          IFEQ      'M'
218700950113     C                   Z-ADD     1000          WNDC
218800950113     C                   ELSE
218900950113     C                   Z-ADD     5000          WNDC
219000950113     C                   ENDIF
219100950113     C                   ADD       WNRT          WNDC
219200931203     C*
219300011121     C     KFTT          CHAIN     fiftt000                           32
219400931209     C* 32 OFF - AGGIORNAMENTO
219500931203    3C     *IN32         IFEQ      *OFF
219600931203     C*
219700931203     C                   ADD       COMSET        FTTSET
219800931203     C                   CLEAR                   COMSET
219900931209     C*
220000011121     C                   UPDATE    fiftt000
220100931209     C*
220200931210     C* SOLO SE GIA' VALORIZZATA
220300931210     C     FTTFVL        IFEQ      'V'
220400931209     C* RICHIAMO PGM DI SVALORIZZAZIONE DISTINTA PADRONCINO
220500931209     C                   Z-ADD     WPDR          PA3PDR
220600931209     C                   MOVEL     COMTSR        PA3TSR
220700931209     C                   Z-ADD     WNDC          PA3NDC
220800950115     C                   Z-ADD     WDRT          PA3DDC
220900931209     C                   MOVEL     'N'           PA3FCT
221000931209     C                   MOVEL     'N'           PA3FVD
221100931209     C                   MOVEL     'N'           PA3FVT
221200041122     C                   MOVEL     ' '           PA3sml
221300931209     C                   MOVEL     PARAM3        KPJBU
221400011121     C                   CALL      'FICNE2R'
221500931209     C                   PARM                    KPJBA
221600931210     C                   ENDIF
221700931203     C*
221800931203   X3C                   ELSE
221900931209     C* 32 ON  - IMMISSIONE
222000011121     C                   CLEAR                   fiftt000
222100931209     C*
222200931203     C                   ADD       COMSET        FTTSET
222300931203     C                   CLEAR                   COMSET
222400931203     C                   Z-ADD     WPDR          FTTPDR
222500020325     C                   movel     WPDR          FTTfgs
222600931203     C                   MOVEL     COMTSR        FTTTSR
222700950113     C                   Z-ADD     WNDC          FTTNDC
222800931203     C                   Z-ADD     WDRT          FTTDDC
222900950113     C                   MOVEL     WFPP          FTTFPP
223000950113     C     WFPP          IFEQ      'M'
223100950113     C                   MOVEL     ETM(W)        FTTPEP
223200030107     c                   Movel     Scm(w)        FttLvl
223300950113     C                   ELSE
223400950113     C                   MOVEL     ETP(W)        FTTPEP
223500030107     c                   Movel     Scp(w)        FttLvl
223600950113    2C                   ENDIF
223700931203     C*
223800011127      * In base alla data del documento aggancio la tab.GEDCN per
223900011127      * impostare la divisa della valorizzazione dei conteggi.
224000011127     c                   if        fttddc <= §geddtfv
224100011127     c                   movel     §geditl       fttdiv
224200011127     c                   else
224300011127     c                   movel     §gedeur       fttdiv
224400011127     c                   end
224500011127      *
224600011121     C                   WRITE     fiftt000
224700931203    3C                   ENDIF
224800931203    2C                   ENDIF
224900931203     C*
225000931203     C                   ENDSR
225100931203     C*
225200030107      *---------------------------------------------------------------*
225300030107      * Creazione Fiftt00f x ritiri annullati - bolle "FY"
225400030107      *---------------------------------------------------------------*
225500030107     c     CrtFtt1       BegSr
225600030107
225700030107     c                   Z-Add     1             w
225800030107     c     BlpPdr        Lookup    Pdr(w)                                 30
225900030107
226000030107     c                   Movel     BlpPdr        Wpdr
226100030107     c                   If        BlpFpp = 'M'
226200030107     c                   Z-Add     1000          Wndc
226300030107     c                   Else
226400030107     c                   Z-Add     5000          Wndc
226500030107     c                   EndIf
226600030107     c                   Add       BlpNrt        Wndc
226700030107     c                   Movel     BlpDrt        Wdrt
226800030107     c     Kftt          Chain     Fiftt01l
226900030107      * Aggiornamento
227000030107If  1c                   If        %Found(Fiftt01l)
227100030107     c                   Add       1             FttTbs
227200030107     c                   Update    Fiftt000
227300030107
227400030107      * Solo se già valorizzata
227500030107     c                   If        FttFvl = 'V'
227600030107      * Richiamo pgm di svalorizzazione distinta autotrasportatore
227700030107     c                   Z-Add     Wpdr          Pa3Pdr
227800030107     c                   Movel     ComTsr        Pa3Tsr
227900030107     c                   Z-Add     Wndc          Pa3Ndc
228000030107     c                   Z-Add     Wdrt          Pa3Ddc
228100030107     c                   Movel     'N'           Pa3Fct
228200030107     c                   Movel     'N'           Pa3Fvd
228300030107     c                   Movel     'N'           Pa3Fvt
228400041122     C                   MOVEL     ' '           PA3sml
228500030107     c                   Movel     Param3        Kpjbu
228600030107     c                   Call      'FICNE2R'
228700030107     c                   Parm                    Kpjba
228800030107     c                   EndIf
228900030107
229000030107   x1c                   Else
229100030107      * Immissione
229200030107     c                   Clear                   Fiftt000
229300030107
229400030107     c                   Z-Add     1             FttTbs
229500030107     c                   Z-Add     Wpdr          FttPdr
229600030107     c                   Movel     Wpdr          FttFgs
229700030107     c                   Movel     ComTsr        FttTsr
229800030107     c                   Z-Add     Wndc          FttNdc
229900030107     c                   Z-Add     Wdrt          FttDdc
230000030107     c                   Movel     BlpFpp        FttFpp
230100030107     c                   If        BlpFpp = 'M'
230200030107     c                   Movel     Etm(w)        FttPep
230300030107     c                   Movel     Scm(w)        FttLvl
230400030107     c                   Else
230500030107     c                   Movel     Etp(w)        FttPep
230600030107     c                   Movel     Scp(w)        FttLvl
230700030107     c                   EndIf
230800030107      * In base alla data del documento aggancio la tab.GEDCN per
230900030107      * impostare la divisa della valorizzazione dei conteggi.
231000030107     c                   If        FttDdc <= §geddtfv
231100030107     c                   Movel     §geditl       FttDiv
231200030107     c                   Else
231300030107     c                   Movel     §gedeur       fttDiv
231400030107     c                   EndIf
231500030107
231600030107     c                   Write     Fiftt000
231700030107    1c                   EndIf
231800030107
231900030107     c                   EndSr
232000030107
232100931027     C*--- CHAIN TABEL -----------------------------------------------*
232200931027     C     CTAB          BEGSR
232300931028     C* EFFETTUO LA CHAIN IN TABEL SOLO SE CAMBIANO IL TIPO BOLLA E LA
232400931028     C*   FILIALE DI APPARTENENZA DEL PADRONCINO
232500950113     C                   MOVEL     WCPR          COMAPP
232600950113     C                   MOVE      BLPCBO        COMAPP            4
232700931124     C*
232800931028     C     SAVAPP        IFNE      COMAPP
232900931124     C                   MOVEL     ' '           ALF1              1
233000931027     C*
233100931028     C                   MOVEL     '8C'          COD
233200931108     C                   MOVEL     'R'           COMKEY            2
233300950109     C                   MOVE      BLPCBO        COMKEY
233400931027     C                   MOVEL     *BLANKS       KEY
233500931027     C                   MOVEL     COMKEY        KEY
233600950113     C                   MOVEL     WCPR          W0030             3 0
233700950113     C                   MOVE      W0030         KEY
233800931027     C*
233900931027     C     KTAB          CHAIN     TABEL                              16
234000931124     C* SE NON ESISTE OPPURE ESISTE MA E' ANNULLATO, RIPROVO CON LA
234100931124     C*   FILIALE 000 = TUTTE
234200931027     C     *IN16         IFEQ      *ON
234300931027     C     *IN16         OREQ      *OFF
234400931027     C     TBLFLG        ANDNE     ' '
234500931124     C*
234600931027     C                   MOVE      '000'         KEY
234700931027     C     KTAB          CHAIN     TABEL                              16
234800931124     C*
234900931124     C     *IN16         IFEQ      *ON
235000931124     C     *IN16         OREQ      *OFF
235100931124     C     TBLFLG        ANDNE     ' '
235200931124     C                   MOVEL     '1'           ALF1
235300931124     C                   ENDIF
235400931124     C*
235500931027     C                   ENDIF
235600931027     C*
235700931028     C                   MOVEL     COMAPP        SAVAPP            4
235800931028     C                   ENDIF
235900931027     C                   ENDSR
236000910909     C**-----------------------------------------------*
236100000111     C** RIEMPO CAMPI DEL FILE FISPP00T
236200910909     C**-----------------------------------------------*
236300910909     C     RIESPP        BEGSR
236400000111     C                   Z-ADD     BRVPES        SPPPES
236500950109     C                   MOVEL     BRVLNP        SPPLNP
236600950109     C                   MOVEL     BRVLNA        SPPLNA
236700950109     C                   MOVEL     BRVNRS        SPPNRS
236800950109     C                   MOVEL     BRVNSC        SPPNSC
236900950109     C                   MOVEL     BRVZNC        SPPZNC
237000950109     C                   MOVEL     BRVNPS        SPPNPS
237100950109     C                   MOVEL     BRVFGS        SPPFGS
237200950109     C                   MOVEL     BRVNFV        SPPNFV
237300940527     C                   MOVEL     WDTP          SPPDFV
237400950109     C                   Z-ADD     1             SPPNPG
237500950113     C                   MOVEL     BRVCAN        SPPCAN
237600950118     C                   MOVEL     BRVVUC        SPPVUC
237700990906     C                   MOVEL     BRVPUC        SPPPUC
237800961203     C     §TVDEF        IFEQ      'S'
237900961203     C                   MOVEL     'F'           SPPATR
238000961203     C                   ENDIF
238100000111     C                   Z-ADD     BRVDFS        SPPDFS
238200070123     C                   movel     BRVHMS        SPPHMS
238300000111     C                   Z-ADD     BRVDCS        SPPDCS
238400070123     C                   movel     BRVHCS        SPPHCS
238500000320     C                   Z-ADD     WDTR          SPPDTR
238600050707     C                   TIME                    W0140            14 0
238700000320     C                   MOVEL     W0140         SPPHTR
238800000111     C                   Z-ADD     BRVFLE        SPPFLE
238900910909     C                   ENDSR
239000910910     C**-----------------------------------------------*
239100000428     C** RIEMPO CAMPI DEL FILE FNFVA01T-FNFWA00T
239200910910     C**-----------------------------------------------*
239300910910     C     RIEFVA        BEGSR
239400950123     C                   MOVEL     1             FVANPG
239500950109     C                   MOVEL     FGVLNP        FVALNP
239600910910     C                   MOVEL     FGVNFV        FVANFV
239700910910     C                   MOVEL     FGVDFV        FVADFV
239800910910     C                   MOVEL     FGVLNA        FVALNA
239900921202     C                   MOVEL     FGVFFV        FVAFFV
240000921202     C                   MOVEL     FGVFF2        FVAFF2
240100921202     C                   MOVEL     FGVFLP        FVAFLP
240200921202     C                   MOVEL     FGVFL2        FVAFL2
240300951116     C                   MOVEL     FGVTRN        FVATRN
240400951120     C                   MOVEL     FGVTTR        FVATTR
240500951116     C                   MOVEL     FGVFRQ        FVATFM
240600951116     C                   MOVEL     FGVTRM        FVATRM
240700951116     C                   MOVEL     FGVTRR        FVATRR
240800951116     C                   MOVEL     FGVPDR        FVAPDR
240900951116     C                   MOVEL     FGVDPD        FVADPD
241000951116     C                   MOVEL     FGVCCN        FVACCN
241100130429     C                   MOVEL     FGVpmb        FVADCN
241200960123     C                   MOVEL     FGVDTP        FVADRP
241300960123     C                   MOVEL     FGVHMP        FVAHRP
241400960124     C* CALL PGM PER PRENDERE LA CAUSALE RITARDO
241500960124     C                   MOVEL     FGVLNP        SA2LNP
241600960124     C                   MOVEL     FGVNFV        SA2NFV
241700960124     C                   CLEAR                   SA2TLA
241800960124     C                   CLEAR                   SA2RTP
241900960124     C                   CALL      'FNLSA2R4'
242000960124     C                   PARM                    SA2TLA            1
242100960124     C                   PARM                    SA2LNP
242200960124     C                   PARM                    SA2NFV
242300960124     C                   PARM                    SA2RTP
242400960123     C*
242500960124     C                   MOVEL     SA2RTP        FVARTP
242600000428     C*
242700000428     C* RIEMPI FILE DI APPOGGIO FWA
242800000428     C     WFGW          IFEQ      'S'
242900000428     C                   CLEAR                   FNFWA000
243000000428     C                   MOVEL     FGWATB        FWAATB
243100000428     C                   MOVEL     FGWNFV        FWANFV
243200000428     C                   MOVEL     FGWLNP        FWALNP
243300000428     C                   MOVEL     FGWFGS        FWAFGS
243400000428     C                   MOVEL     FGWIPT        FWAIPT
243500000428     C                   MOVEL     FGWVIP        FWAVIP
243600000428     C                   MOVEL     FGWIES        FWAIES
243700000428     C                   MOVEL     FGWVES        FWAVES
243800000428     C                   MOVEL     FGWFF3        FWAFF3
243900000428     C                   MOVEL     FGWFF4        FWAFF4
244000000428     C                   MOVEL     FGWFL3        FWAFL3
244100000428     C                   MOVEL     FGWFL4        FWAFL4
244200000428     C                   ENDIF
244300960123     C*
244400960123     C* CONTROLLO SE IN ARRIVO OCCORRERA' SBINARE IL F.VIAGGIO PARTENZA
244500960123     C*   DAL FOGLIO ARRIVI
244600960123    1C     PA2FLG        IFEQ      'M'
244700960123     C                   CLEAR                   FVASBN
244800960123   X1C                   ELSE
244900960123     C                   MOVEL     'S'           FVASBN
245000960123    1C                   ENDIF
245100960123     C*
245200910910     C                   ENDSR
245300060710     C**-----------------------------------------------*
245400060710     C** SCrittura dati mancanti di FNFV4 in fnfv400F per arrivo
245500060710     C**-----------------------------------------------*
245600060710     c     SCRIVIFV4     begsr
245700060710     c
245800060710     C* SCRITTURA TIPO AUTOMEZZO E CATEGORIA TRAINO
245900060710     c* direttamente nel file fisico senza passare dal file fnfv400t
246000060710     c* verifico se esiste già
246100060710     C                   MOVEL     'P'           FV4EPA
246200060710     C                   MOVEL     'V'           FV4TRC
246300060710     C                   Z-ADD     FGVLNP        FV4LNP
246400060710     C                   Z-ADD     FGVNFV        FV4NFV
246500060710     C                   Z-ADD     *ZEROS        FV4LAI
246600060710     c     kfv4e         setll     fnfv401l                               23
246700060710    1c                   if        not *in23
246800060710     C                   CLEAR                   DSFV4V
246900060710     C                   MOVEL     *BLANKS       FV4ATB
247000060710     C                   MOVEL     FGVTMZ        §V4TMZ
247100060710     C                   MOVE      FGVSNP        §V4TTR
247200060710     C                   MOVEL     DSFV4V        FV4NOT
247300060710     C                   movel     'N'           FV4ft1
247400060710     C                   Z-ADD     DATEU         FV4dt1
247500060710     c* Per la sede verifico se da trasmettere
247600060710     c     fv4trc        lookup    fv4s                                   23
247700060710    2c                   if        *in23
247800060710     C                   clear                   FV4ft2
247900060710     C                   clear                   FV4dt2
248000060710   x2c                   else
248100060710     C                   movel     'N'           FV4ft2
248200060710     C                   Z-ADD     DATEU         FV4dt2
248300060710    2c                   endif
248400060710     C                   WRITE     FNFV4000
248500060710    1C                   ENDIF
248600060710     c                   ENDSR
248700970221     C**-----------------------------------------------*
248800970221     C* VEDO SE RIELABORARE LE STATISTICHE
248900970221     C**-----------------------------------------------*
249000970221     C     FNSTA         BEGSR
249100001027     C                   CLEAR                   WWLNA
249200001027     C**
249300021016     c                   If        WdtPar <= §SdfParU
249400970221     C                   MOVE      WDTPAR        SA2DTS            8 0
249500970221     C                   MOVE      'P'           SA2TRC            1
249600970221     C                   CALL      'FNLSA2R3'
249700970221     C                   PARM                    SA2TLA
249800970221     C                   PARM                    SA2TRC
249900970221     C                   PARM                    SA2DTS
250000970221     C                   PARM                    WWLNA             3 0
250100970221     C                   END
250200001027     C*
250300021016     c                   If        WdtPar <= §SdfTerU
250400970221     C                   MOVE      WDTPAR        SA2DTS
250500970221     C                   MOVE      'T'           SA2TRC
250600970221     C                   CALL      'FNLSA2R3'
250700970221     C                   PARM                    SA2TLA
250800970221     C                   PARM                    SA2TRC
250900970221     C                   PARM                    SA2DTS
251000970221     C                   PARM                    WWLNA             3 0
251100001027     C                   END
251200970221     C                   ENDSR
251300910909     C**-----------------------------------------------*
251400910909     C** TRASMISSIONE SPUNTE
251500910909     C**-----------------------------------------------*
251600910909     C     SPUNTE        BEGSR
251700960124     C                   CLEAR                   SA2TLA
251800980210     C                   SETOFF                                       0106
251900920505     C                   SETOFF                                       08
252000910909     C                   Z-ADD     0             SAVLNA            3 0
252100920703     C                   Z-ADD     0             SAVLNP            3 0
252200950323     C                   CLEAR                   SAVCBO
252300950203     C                   CLEAR                   SAVSPE
252400960708     C                   CLEAR                   WLAST
252500970314     C                   CLEAR                   WRKSPE
252600150730     c                   eval      wnpg=1
252700000320     C* PRENDO DATA/ORA SISTEMA PER L'AGGIORNAMENTO DI DATA E ORA
252800000320     C* TRASMISSIONE SPUNTE
252900000320     C                   TIME                    W0140            14 0
253000000320     C                   CLEAR                   WLBDAT
253100000320     C                   MOVE      W0140         G02DAT
253200000320     C                   CALL      'XSRDA8'
253300000320     C                   PARM                    WLBDAT
253400000320     C                   Z-ADD     G02INV        WDTR              8 0
253500931021     C*
253600910909     C* LETTURA PER FOGLIO VIAGGIO
253700070123     C     KBRV          SETLL     FNBRV02
253800070123     C     KBRV          READE     FNBRV02                                22
253900910909     C*
254000950215    1C     *IN22         DOWEQ     '0'
254100950215    2C     BRVFTR        IFEQ      *BLANKS
254200931220     C                   CLEAR                   WSPEN
254300931220     C                   CLEAR                   WSPET
254400950215     C                   CLEAR                   WAGG
254500980326     C                   CLEAR                   NCLU
254600960603     C**
254700960603     C* DETERMINO LNP DELLA BOLLA SE COLLO CON SERIE IN CONFERMA DA
254800960603     C* ALTRI AS
254900960604     C                   MOVE      'S'           WALCF
255000960604     C                   MOVEL     BRVLNP        KLNP
255100960604     C                   MOVEL     BRVLNA        KLNA
255200960604     C                   MOVEL     BRVNRS        KNRS
255300960604     C                   MOVEL     BRVNSC        KNSC
255400960603     C                   EXSR      RICLNP
255500910909     C**
255600910909     C** L I N E A   A R R I V O   C A M B I A T A
255700950215    3C     SAVLNA        IFNE      BRVLNA
255800920427     C* SCRIVO PER LIN ARRIVO CON SCARICO IL TERMINAL SPUNTE " " E "T"
255900920427     C   08
256000910912     C* SCRIVO LINEA DI ARRIVO SPUNTE "A" SE C'E' TRANSITO
256100980210    4COR 06SAVLNA        IFNE      0
256200910912     C                   EXSR      WTRLNA
256300950215    4C                   END
256400050203     c* mi salvo in una schiera parallela alle linee del foglio viaggio, se
256500050203     x*  locale, se presenti spunte transito
256600050204     c                   if        fgvlnp=flptfa  and savlna>0
256700050204     C                   Z-ADD     1             B
256800050204     C                   MOVEL     savLNA        COMPRO
256900050203     C     COMPRO        LOOKUP    FFV(B)                                 20
257000050203     C   20              MOVEL     winvialocale  winvloc(B)
257100050203     c                   endif
257200050203     c                   clear                   winvialocale
257300930712     C*
257400980210     C                   SETOFF                                       060801
257500930127     C                   Z-ADD     1             B                 3 0
257600950109     C                   MOVEL     BRVLNA        COMPRO            3
257700910912     C     COMPRO        LOOKUP    FFV(B)                                 20
257800910912     C*     TROVATA FIL SCARICO
257900930127     C   20              MOVEL     FLP(B)        COMLIN            3 0
258000910912     C* NON TROVATA --> METTO LINEA ARRIVO STESSA
258100950109     C  N20              MOVEL     BRVLNA        COMLIN
258200931022     C*
258300970811     C* F I L I A L E   S C A R I C O
258400051031     C                   CLEAR                   dslv55
258500051031     C****               MOVEL     'P'           d55TPT
258600051031     c                   z-add     fgvlnp        d55lnp
258700051031     C                   Z-ADD     COMLIN        d55LIN
258800051031     C                   Z-ADD     FGVDFV        d55DRF
258900051031     C                   CALL      'FNLV55R'
259000051031     C                   PARM                    dslv55
259100051031     C                   MOVEL     d55TFP        SAVFEL            3 0
259200051031     C                   MOVEL     d55TFP        VARX
259300051031     C                   MOVEL     d55TFP        VAR2X
259400051031     C                   Z-ADD     d55TFA        FLPTFA
259500950109     C*
259600021125     C                   SETON                                        01
259700910912     C* APERTURA MEMBRI
259800970811     C                   EXSR      CAEXSP
259900050204    4C***                END
260000920703     C*
260100980210     C* Reperisco terminal di arrivo della linea di arrivo
260200971105     C* Reperisco anche il terminal di partenza
260300130531     C                   CLEAR                   dslv55
260400130531     C                   MOVEL     ' '           d55TPT
260500130531     C                   Z-ADD     BRVLNA        d55LIN
260600130531     C                   Z-ADD     FGVLNP        d55LNP
260700130531     C                   Z-ADD     WDTP          d55DRF
260800130531     C                   CALL      'FNLV55R'
260900130531     C                   PARM                    dslv55
261000130531     C                   Z-ADD     d55TFA        LNATFA
261100130531     C                   Z-ADD     d55TFP        LNAFEL
261200051031     c
261300051031     c* Reperisco anche il terminal di arrivo della spunta letta
261400051031     C                   CLEAR                   dslv55
261500051031     C                   MOVEL     ' '           d55TPT
261600051031     C                   Z-ADD     BRVLNA        d55LIN
261700051031     C                   Z-ADD     wlnp          d55LNP
261800051031     C* Se fv poste gli passo il p.o. poste
261900051031     C     WFGW          IFEQ      'S'
262000051031     C     FGWFGS        ANDGT     *ZEROS
262100051031     C                   Z-ADD     FGWFGS        d55LNP
262200051031     C                   ENDIF
262300051031     C                   Z-ADD     WDTP          d55DRF
262400051031     C                   CALL      'FNLV55R'
262500051031     C                   PARM                    dslv55
262600051031     C                   Z-ADD     d55TFA        spuTFA            3 0
262700051031     c
262800031218     c* Determino se collo locale per sottomissione lr15
262900031218     c* Una volta acceso in92 non si deve rispegnere perchè basta che
263000031218     c* ce ne sia una per sottomettere
263100031218    1c     *in92         ifeq      *off
263200051031    3c     fgvlnp        ifeq      lnatfa
263300031218     c                   seton                                        92
263400031218    3c                   endif
263500031218    1c                   endif
263600980210     C**
263700980210     C* SE SCARICO >< DA LINEA DI ARRIVO
263800980210    4C     COMLIN        IFNE      BRVLNA
263900940107     C*
264000980210     C* 06  ON - T R A N S I T O
264100940107     C* SE SCARICO >< DA TERMINAL DI ARRIVO LINEA DI ARRIVO
264200980210    6C     COMLIN        IFNE      LNATFA
264300980210     C                   SETON                                        06
264400930312     C* APRO MEMBRI DEL TRANSITO
264500980210     C                   EXSR      TRANSI
264600980210     C                   EXSR      CAEX08
264700920703     C*
264800950215    6C                   END
264900950215    4C                   END
265000910909     C*
265100950109     C                   MOVEL     BRVLNA        SAVLNA
265200051031     C                   MOVEL     wlnp          SAVLNp
265300051031   x3c                   else
265400051031     c* se non cambiata la linea di arrivo vedo se cambiata la
265500051031     c*  linea di partenza spunte per la ricerca del tfa della spunta
265600051031    4c                   if        wlnp<>savlnp
265700051031     c* Reperisco anche il terminal di arrivo della spunta letta
265800051031     C                   CLEAR                   dslv55
265900051031     C                   MOVEL     ' '           d55TPT
266000051031     C                   Z-ADD     BRVLNA        d55LIN
266100051031     C                   Z-ADD     wlnp          d55LNP
266200051031     C* Se fv poste gli passo il p.o. poste
266300051031    5C     WFGW          IFEQ      'S'
266400051031     C     FGWFGS        ANDGT     *ZEROS
266500051031     C                   Z-ADD     FGWFGS        d55LNP
266600051031    5C                   ENDIF
266700051031     C                   Z-ADD     WDTP          d55DRF
266800051031     C                   CALL      'FNLV55R'
266900051031     C                   PARM                    dslv55
267000051031     C                   Z-ADD     d55TFA        spuTFA            3 0
267100051031     c                   z-add     wlnp          savlnp
267200051031    4C                   ENDIF
267300051031    3C                   ENDIF
267400950515     C*
267500960603     C                   CLEAR                   WTRAN
267600960603     C                   SETOFF                                       19
267700910912     C*
267800950515     C*  19 ON  - NON E' SPUNTA DA TRANSITO
267900950515     C*  19 OFF -     E' SPUNTA DA TRANSITO
268000960531     C     WLNP          LOOKUP    L1                                     19
268100980401     C**
268200980401     C* IGNORO OGNI AGGIORNAMENTO SE SI TRATTA DI SPUNTA DEFLUENZA
268300980401     C*  PERCHE' E' A TUTTI GLI EFFETTI UNA SPUNTA ARRIVI
268400980401   2AC     §TVDEF        IFNE      'S'
268500911018     C*
268600911106     C** T R A N S I T O
268700930203     C*
268800970221    3C     *IN19         IFEQ      *OFF
268900970828     C     WTAB5F        ANDEQ     ' '
269000950113     C*
269100911106     C**    PRENDO BOLLE E BLT DAL FILE DI TRANSITO
269200960603     C*     Non ripeto la chain se l'ho già fatta durante la ricerca
269300960603     C*     della lnp bolla
269400960603     C                   SELECT
269500960611     C     WOKBT         WHENEQ    *BLANKS
269600020606     C     KBtt37        CHAIN     FNBTT37L                           20
269700960611     C     WOKBT         WHENEQ    'S'
269800960603     C                   SETOFF                                       20
269900960603     C                   ENDSL
270000921014     C*
270100950515     C* CHIUDO ANOMALIA : "DISGUIDO"
270200970221     C*
270300970221   3AC     *IN20         IFEQ      *ON
270400970221     C                   Z-ADD     200           COMCAA
270500970221     C                   EXSR      CHIUAN
270600050927     c                   clear                   noinvia           1
270700130527     c* Per foglio locale non invio la spunta come disguido
270800050927     c*  (si tratta di colli di defluenza sparato per errore sul foglio
270900050927     c*   locale ma non si tratta nè di vera defluenza, nè di disguido)
271000050927     c                   if        wesisteanm=' '
271100050927     c                   eval      noinvia='S'
271200050927     c                   endif
271300970827     C* SOLO SE HO TROVATO L'ANOMALIA
271400970827     C  N21              Z-ADD     1             NCLU
271500921014     C**
271600970221  X3AC                   ELSE
271700970221     C**
271800950515     C                   MOVEL     'S'           WTRAN             1
271900921014     C*
272000950515     C* CHIUDO ANOMALIA : "BOLLA TRANSITO"
272100921014     C                   Z-ADD     125           COMCAA
272200921014     C                   EXSR      CHIUAN
272300921014     C*
272400950621    4C     BTTDUT        IFNE      0
272500950621     C                   MOVEL     'N'           WAGG              1
272600950621    4C                   ENDIF
272700950621     C*
272800950113     C                   MOVEL     FGVNFV        BTTNFV
272900970818     C* SE ERA PISTOLA FASULLA IMPOSTO 87
273000970827     C     BTTPUT        IFGT      0
273100970818     C     BTTPUT        LOOKUP    NPF                                    39
273200970827     C                   ELSE
273300970827     C                   SETOFF                                       39
273400970827     C                   ENDIF
273500970827     C**
273600970818     C     *IN39         IFEQ      *ON
273700950621     C                   MOVE      87            BTTPUT
273800950621     C                   ELSE
273900950113     C                   MOVEL     BRVNPS        BTTPUT
274000950621     C                   END
274100970827     C*
274200950706     C* Se le statistiche partito e a terra sono già state elaborate
274300950705     C* scrivo o aggiorno file FNSTA00F per rielaborazione statistiche
274400970221     C     BTTDUT        IFGT      *ZEROS
274500970221     C                   Z-ADD     BTTDUT        WDTPAR
274600970221     C                   EXSR      FNSTA
274700970221     C                   ENDIF
274800970221     C*
274900091027     c                   Z-ADD     WDTP          WDTPAR
275000970221     C                   EXSR      FNSTA
275100970221     C**
275200950113     C                   MOVEL     WDTP          BTTDUT
275300980407     C                   Z-ADD     1             NCLU
275400910927     C*
275500020606     C                   UPDATE    FNBTT037
275600950130     C*
275700950130     C* C A M B I A T A   S P E D I Z I O N E --> RICHAINO LA BOLLA
275800950215    4C     BTTSPE        IFNE      SAVSPE
275900970307     C*
276000970307     C* IMPOSTO LA SPEDIZIONE PERCHE' A CAMBIO ULTIMO COLLO SCRIVO AGB
276100970307     C     WRKSPE        IFEQ      0
276200970307     C                   Z-ADD     BTTSPE        WRKSPE
276300970307     C                   MOVEL     'T'           WRKTBO
276400970307     C                   ENDIF
276500950130     C*
276600020605     C     KBTT3         CHAIN     FNBTP11L                           21
276700930127     C*
276800091027     c* BTPDT3  contiene il terminal a cui trasmetto
276900020606     c                   z-add     btpdt3        savdt3            3 0
277000020606     c                   z-add     comlin        btpdt3
277100091027     c
277200950109     C                   MOVEL     'T'           BTPFT1
277300950113     C                   Z-ADD     DATEU         BTPDT1
277400950109     C                   MOVEL     'N'           BTPFT2
277500950113     C                   Z-ADD     DATEU         BTPDT2
277600950109     C                   MOVEL     'N'           BTPFT3
277700940110     C*
277800940110     C* CALCOLO PESO E VOLUME UNITARIO DEL COLLO
277900950110     C                   EXSR      CALUNT
278000931022     C*
278100911011     C* AGGIORNO DATA PARTENZA DAL TRANSITO
278200950215    5C     BTPDUT        IFEQ      0
278300931220     C* MUOVO 1 NEL CAMPO DELLE SPEDIZIONI
278400931220     C                   Z-ADD     1             WSPET
278500950109     C                   MOVEL     WDTP          BTPDUT
278600950621     C                   ELSE
278700950621     C                   MOVEL     'T'           AGBTBO
278800950621     C                   MOVE      BTPAAS        AGBAAS
278900950621     C                   MOVE      BTPLNP        AGBLNP
279000950621     C                   MOVE      BTPNRS        AGBNRS
279100950621     C                   MOVE      BTPNSP        AGBNSP
279200950621     C                   MOVEL     'DT'          AGBAGB
279300950630     C                   WRITE     FNAGB000                             39
279400950215    5C                   END
279500950109     C*
279600950109     C                   UPDATE    FNBTP000
279700950130     C*
279800950130     C                   MOVEL     BTTSPE        SAVSPE
279900910927     C*
280000091027     C* SE TRANSITO PER TRANSITO : INVIO DI NUOVO BOLLA
280100020606    5C   06SAVdt3        IFNE      COMLIN
280200031219     c* Se p.o. di scarico è anche il ter-part.della lnp bolla non devo
280300031219     c* ritrasmettere
280400130531     c                   clear                   dslv55
280500130531     c                   movel     'P'           d55tpt
280600130531     c                   z-add     wlnp          d55lin
280700130531     c                   z-add     wdtp          d55drf
280800130531     c                   call      'FNLV55R'
280900130531     c                   parm                    dslv55
281000130531     c     comlin        ifne      d55tfp
281100950109     C                   EXSR      WTRBTP
281200130531     C                   endif
281300950808    5C                   END
281400950808    4C                   ENDIF
281500970307     C**
281600970307     C* A CAMBIO ULTIMO COLLO SCRIVO
281700970307     C                   MOVEL     'T'           BTTTBO
281800970307     C     BTTKEY        IFNE      WRKKEY
281900970307     C* SCRIVO IL FILE FNAGB01L PER AGGIORNARE DATA ULTIMO COLLO
282000970307     C                   MOVEL     WRKTBO        AGBTBO
282100970307     C                   MOVEL     WRKAAS        AGBAAS
282200970307     C                   MOVEL     WRKLNP        AGBLNP
282300970307     C                   MOVEL     WRKNRS        AGBNRS
282400970307     C                   MOVEL     WRKNSP        AGBNSP
282500970307     C                   MOVEL     'DT'          AGBAGB
282600970307     C                   WRITE     FNAGB000                             39
282700970307     C**
282800970307     C                   MOVEL     BTTKEY        WRKKEY
282900970307     C                   ENDIF
283000970221   3AC                   ENDIF
283100970221    3C                   END
283200910912     C**
283300991022     C** L N P
283400910927     C**
283500960604    3C     *IN19         IFEQ      *ON
283600970827     C     WTAB5F        OREQ      'S'
283700960603     C*     Non ripeto la chain se l'ho già fatta durante la ricerca
283800960603     C*     della lnp bolla
283900960603     C                   SELECT
284000960611     C     WOKBL         WHENEQ    *BLANKS
284100960603     C     KBRV4         CHAIN     FNBLT27L                           20
284200960611     C     WOKBL         WHENEQ    'S'
284300960603     C                   SETOFF                                       20
284400960603     C                   ENDSL
284500911108     C*
284600921014     C* CREO   ANOMALIA : "MANCA BOLLA"
284700991022     C***
284800991022   3AC     *IN20         IFEQ      *ON
284900911108     C                   Z-ADD     105           COMCAA
285000990517     C     KANM3         SETLL     FNANM                                  21
285100911108     C  N21              EXSR      CRTANM
285200970221     C*
285300991022     C* NON SOMMO I COLLI  MANCA REC BOLLA
285400970221     C*  PERCHE' LO FARO' AL MOMENTO DELLA
285500970221     C*  BORDERIZZAZIONE DELLA BOLLA, SE E' UN MANCA REC BOLLA VERO
285600970221     C*  PER IL PROSPETTO NOLI (LE STATISTICHE HANNO UN LORO GIRO
285700970827  X3AC                   ELSE
285800991022     C* SE ESISTE NELL'ALTRO S.I. DO IL DISGUIDO E BASTA
285900040531   3BC**   WALTSI        IFEQ      'S'
286000040531     C**                 MOVEL     'D'           FLG
286100040531     C**                 EXSR      CALL44
286200040531  X3BC**                 ELSE
286300911108     C*
286400950515     C                   CLEAR                   WTRAN
286500110907     c
286600110907     c                   setoff                                       37
286700921014     C*
286800950515     C* CREO   ANOMALIA : "MANCA RECORD BOLLA BORDERIZZATA"
286900040625     C* NON LO FACCIO SE bolla poste
287000110907    4c                   if        bltnfv=0 or brvnps =88 or
287100110907     c                             brvnps=90
287200040603     c* Verifico se bolla con merce di valore da memorizzare nella
287300040603     c*  anomalia
287400040603     c     kar5          chain(N)  fiar501l
287500040603     c                   if        %found(fiar501l)
287600040603     c                   movel     ar5uni        dar5gen
287700040603     c                   else
287800040603     c                   clear                   dar5gen
287900040603     c                   endif
288000110907
288100110907    5C     BLTNFV        IFEQ      0
288200110907     C                   Z-ADD     110           COMCAA
288300040603     C     KANM3         SETLL     FNANM                                  21
288400911108     C  N21              EXSR      CRTANM
288500110907    5C                   END
288600110907     c*
288700110907     C* CREO   ANOMALIA : "COLLO MAI AFFIDATO"
288800110907    5c                   if        brvnps =88 or brvnps=90
288900110907     C* PER SICUREZZA VERIFICO CHE NON SIA PRESENTE UNA QUALSIASI SPUNTA
289000110907     C* IN ALTRA FILIALE
289100110907     c                   clear                   EsSpunta          1
289200110907     C                   MOVEL     DSBRV         DSBRVS
289300110907     C     KBRV4         setll     FNbrv07l
289400110907     C     KBRV4         reade     FNbrv07l
289500110907     c                   dow       not %eof(fnbrv07l)
289600110907     c* Escludo le pistole completamente fasulle
289700110907     c                   if        brvnps<>88 and brvnps<>90
289800110907     c                   eval      EsSpunta='S'
289900110907     c                   leave
290000110907     c                   endif
290100110907     c
290200110907     C     KBRV4         reade     FNbrv07l
290300110907     c                   enddo
290400110907     c
290500110907     C* Reimposto campi di fnbrv con dati salvati
290600110907     C                   MOVEL     DSBRVS        DSBRV
290700110907     C*
290800110908    6c                   if        EsSpunta=' '
290900110907     C                   Z-ADD     115           COMCAA
291000110907     C     KANM3         SETLL     FNANM                                  21
291100110907     C  N21              EXSR      CRTANM
291200110907    6c                   endif
291300110907    5c                   endif
291400110907     c
291500110907    4c                   endif
291600911108     C*
291700921014     C* CHIUDO ANOMALIA : "BOLLA PARTENZA"
291800921014     C                   Z-ADD     120           COMCAA
291900020412     C                   EXSR      CHIUAN
292000911108     C*
292100911108     C*
292200950621    4C     BLTDFV        IFNE      0
292300950621     C                   MOVEL     'N'           WAGG
292400950621    4C                   ENDIF
292500950621     C*
292600930520     C                   MOVEL     FGVNFV        BLTNFV
292700970221     C*
292800950706     C* Se le statistiche partito e a terra isono già state elaborate
292900950705     C* scrivo o aggiorno file FNSTA00F per rielaborazione statistiche
293000970221     C     BLTDFV        IFGT      *ZEROS
293100970221     C                   Z-ADD     BLTDFV        WDTPAR
293200970221     C                   EXSR      FNSTA
293300970221     C                   ENDIF
293400970221     C**
293500950707     C                   Z-ADD     WDTP          WDTPAR
293600970221     C                   EXSR      FNSTA
293700970221     C*
293800950706     C                   MOVEL     WDTP          BLTDFV
293900970627     C* PULISCO IL CAMPO CHE MI SEGNALA UNA EVENTUALE MANCANZA DI COLLO
294000970627     C* IMPORT DATOMI DAL PARTNER
294100970627     C                   CLEAR                   BLTFL1
294200970818     C* SE ERA PISTOLA FASULLA IMPOSTO 87
294300970827     C     BLTNPP        IFGT      0
294400970818     C     BLTNPP        LOOKUP    NPF                                    39
294500970827     C                   ELSE
294600970827     C                   SETOFF                                       39
294700970827     C                   ENDIF
294800970827     C**
294900970818     C     *IN39         IFEQ      *ON
295000950621     C                   MOVE      87            BLTNPP
295100950621     C                   ELSE
295200950110     C                   MOVEL     BRVNPS        BLTNPP
295300950623     C                   END
295400910927     C                   MOVEL     COMLIN        BLTFLP
295500980407     C                   Z-ADD     1             NCLU
295600910927     C*
295700950110     C                   UPDATE    FNBLT027
295800950130     C*
295900950130     C* C A M B I A T A   S P E D I Z I O N E --> RICHAINO LA BOLLA
296000950215    4C     BLTSPE        IFNE      SAVSPE
296100970307     C*
296200970307     C* IMPOSTO LA SPEDIZIONE PERCHE' A CAMBIO ULTIMO COLLO SCRIVO AGB
296300970307     C     WRKSPE        IFEQ      0
296400970307     C                   Z-ADD     BLTSPE        WRKSPE
296500970307     C                   MOVEL     'P'           WRKTBO
296600970307     C                   ENDIF
296700950130     C*
296800151022     C     KBLT3         CHAIN     FNBLP01L
296900940107     C*
297000940107     C* CALCOLO PESO E VOLUME UNITARIO DEL COLLO
297100940107     C                   EXSR      CALUNI
297200931022     C*
297300910927     C* SE E' IL PRIMO COLLO CHE PARTE METTO DATA FV
297400950215    5C     BLPDPC        IFEQ      0
297500931220     C* MUOVO 1 NEL CAMPO DELLE SPEDIZIONI
297600931220     C                   Z-ADD     1             WSPEN
297700950124     C                   MOVEL     WDTP          BLPDPC
297800950215    5C                   END
297900910930     C* FIL TRANSITO
298000930127     C                   MOVEL     BLPFLP        SAVFLP            3 0
298100910930     C                   MOVEL     COMLIN        BLPFLP
298200910930     C* AGGIORNO
298300151022     c                   if        %found(fnblp01l)
298400950110     C                   UPDATE    FNBLP001
298500151022     c                   else
298600151022     c                   exsr      sr_errore
298700151022     c                   endif
298800910927     C*
298900910930     C** LNP PER TRANSITO
299000910927     C**   SE NON CORRISPONDE A TRANSITO DELLA BOLLA SPEDISCO LA BOLLA
299100920505     C**   PERCHE' POSSO MANDARE VIA LA BOLLA PER UNA CERTA LINEA E
299200930707     C**   IL GIORNO DOPO I COLLI PER UN'ALTRA LINEA (RIMANDO BOLLA)
299300980210    5C   06SAVFLP        IFNE      0
299400920512     C     SAVFLP        ANDNE     COMLIN
299500930312     C     SAVFLP        OREQ      0
299600930312     C     BLPLNA        ANDNE     COMLIN
299700091027     C
299800091027     c* Imposto il numero foglio viaggio  di chiusura
299900091027     c                   eval      blpnfv=fgv2
300000950110     C                   WRITE     FNBLPT00
300100080423     C
300200040310    5c                   ENDIF
300300950130     C*
300400950130     C*
300500950130     C                   MOVEL     BLTSPE        SAVSPE
300600950215    4C                   ENDIF
300700970307     C*
300800970307     C* A CAMBIO ULTIMO COLLO SCRIVO
300900970307     C                   MOVEL     'P'           BLTTBO
301000970307     C     BLTKEY        IFNE      WRKKEY
301100970307     C* SCRIVO IL FILE FNAGB01L PER AGGIORNARE DATA ULTIMO COLLO
301200970307     C                   MOVEL     WRKTBO        AGBTBO
301300970307     C                   MOVEL     WRKAAS        AGBAAS
301400970307     C                   MOVEL     WRKLNP        AGBLNP
301500970307     C                   MOVEL     WRKNRS        AGBNRS
301600970307     C                   MOVEL     WRKNSP        AGBNSP
301700970307     C                   MOVEL     'DT'          AGBAGB
301800970307     C                   WRITE     FNAGB000                             39
301900970307     C**
302000970307     C                   MOVEL     BLTKEY        WRKKEY
302100970307     C                   ENDIF
302200950130     C*
302300040531   3BC**                 ENDIF
302400991022   3AC                   ENDIF
302500960403    3C                   ENDIF
302600980326   2AC                   ENDIF
302700910927     C**
302800910912     C** I N V I O   L A   S P U N T A
302900910927     C**
303000950516    3C   01              DO
303100000111     C                   CLEAR                   FISPP000
303200950215     C                   CLEAR                   SPPATR
303300950215     C                   CLEAR                   SPPFLP
303400910912     C* SE TRANSITO PER LNA '--> METTO FLAG "T"
303500970814     C*  SE E' LINEA/SERIE COMFERMABILE DA SIMFEL NON CREO IL DISGUIDO
303600950515     C     *IN19         IFEQ      *OFF
303700970814     C     WTAB5F        ANDEQ     ' '
303800040401     c* Se spunta che invio al p.o. di transito memorizzo il p.o. di scarico
303900040401     c   06              z-add     comlin        sppflp
304000950515     C  N06              MOVEL     'T'           SPPATR
304100971105     C  N06              MOVEL     FGVLNP        SPPFLP
304200940322     C* SE E' UN COLLO IN DISGUIDO METTO "D"
304300950515     C     WTRAN         IFEQ      ' '
304400050927     c* non si tratta di disguido se foglio locale e non esiste l'anom.200
304500050927     c                   if        fgvlnp<>flptfa or
304600050927     c                             (fgvlnp=flptfa and noinvia=' ')
304700950515     C                   MOVEL     'D'           SPPATR
304800971105     C                   MOVEL     FGVLNP        SPPFLP
304900050927     C                   endif
305000050927     c
305100050927     C                   ENDIF
305200020607     c                   else
305300020607     c* Se spunta che invio al p.o. di transito memorizzo il p.o. di scarico
305400020607     c   06              z-add     comlin        sppflp
305500950515     C                   ENDIF
305600991025     C* SE BOLLA IN ALTRO S.I. INVIO LA SPUNTA COME "D"
305700040531     C**   WALTSI        IFEQ      'S'
305800040531     C**                 MOVEL     'D'           SPPATR
305900040531     C**                 MOVEL     FGVLNP        SPPFLP
306000040531     C**                 ENDIF
306100910912     C*
306200910912     C                   EXSR      RIESPP
306300910910     C*
306400050204     c* Se sul foglio locale ho memorizzato spunte transito, devo
306500050204     c*  inviare il foglio per l'abbinamento
306600050204     c                   if        FGVLNP<>flpTFA Or
306700050204     c                             (fgvlnp=flptfa and sppatr<>' ' and
306800051031     c                              sppatr<>'F' and not *in06 and
306900051031     c                              sputfa<>flptfa)
307000050204     c                   if        fgvlnp=flptfa
307100050204     C                   MOVEL     'S'           winvialocale
307200050204     c                   endif
307300050204     c
307400000111     C                   WRITE     FISPP000
307500950215    3C                   END
307600050203    3C                   END
307700931022     C*
307800950110     C* AGGIORNO FNFVD00F
307900950215     C     WAGG          IFEQ      ' '
308000931022     C                   EXSR      AGGFVD
308100950215     C                   ENDIF
308200931022     C*
308300911107     C* AGGIORNO SPUNTA
308400950110     C                   MOVEL     'T'           BRVFTR
308500000320     C                   Z-ADD     WDTR          BRVDTR
308600050707     c                   time                    w0140
308700000320     C                   MOVEL     W0140         BRVHTR
308800070123     C                   UPDATE    FNBRV02
308900980728     C                   ELSE
309000070123     C* SE NE LEGGO UNA CON CODICE ANOMALIA E LA DEVO SCRIVERE IN FNBRV
309100980728     C*  SOLO SE SI TRATTA DI UN FOGLIO DI DEFLUENZA
309200980728     C     BRVCAN        IFEQ      'E'
309300070123     C     kbrve         setll     fNbrve                                 99
309400070123     C  n99              WRITE     FNBRVE
309500980728     C                   ENDIF
309600950215    2C                   END
309700070123     C     KBRV          READE     FNBRV02                                22
309800910910     C*
309900950215    1C                   END
310000910910     C*
310100920427     C* SCRIVO PER LIN ARRIVO CON SCARICO IL TERMINAL SPUNTE " " E "T"
310200920427     C   08
310300910912     C* SCRIVO PER ULTIMA LINEA ARRIVO
310400980210     COR 06SAVLNA        IFNE      0
310500960708     C                   MOVE      'S'           WLAST             1
310600910912     C                   EXSR      WTRLNA
310700910910     C                   END
310800050204     c* mi salvo in una schiera parallela alle linee del foglio viaggio, se
310900050204     x*  locale, se presenti spunte transito
311000050204     c                   if        fgvlnp=flptfa  and savlna>0
311100050204     C                   Z-ADD     1             B
311200050204     C                   MOVEL     savLNA        COMPRO
311300050204     C     COMPRO        LOOKUP    FFV(B)                                 20
311400050204     C   20              MOVEL     winvialocale  winvloc(B)
311500050204     c                   endif
311600970307     C**
311700970307     C* SCRIVO L'ULTIMO FNAGB
311800970307     C     WRKSPE        IFGT      0
311900970307     C                   MOVEL     WRKTBO        AGBTBO
312000970307     C                   MOVEL     WRKAAS        AGBAAS
312100970307     C                   MOVEL     WRKLNP        AGBLNP
312200970307     C                   MOVEL     WRKNRS        AGBNRS
312300970307     C                   MOVEL     WRKNSP        AGBNSP
312400970307     C                   MOVEL     'DT'          AGBAGB
312500970307     C                   WRITE     FNAGB000                             39
312600970307     C                   ENDIF
312700910909     C*
312800910909     C                   ENDSR
312900940107     C*
313000950110     C* CALCOLO VOLUME E PESO UNITARI FNBTT--------------------------*
313100950110     C     CALUNT        BEGSR
313200940107     C* COLLI / PESO / VOLUME UNITARI
313300940107     C                   Z-ADD     1             NCLU
313400030902     c*
313500050112     C     BtPNCR        IFEQ      BtPNCL
313600021111     C                   Z-ADD     BtPVLC        Wfvdvol
313700021111     C*
313800021111   X4C                   ELSE
313900021111    5C     BtPVLC        IFGE      BtPVLb
314000021111     C                   Z-ADD     BtPVLC        Wfvdvol
314100021111   X5C                   ELSE
314200021111     C                   Z-ADD     BtPVLb        Wfvdvol
314300021111    5C                   ENDIF
314400021111    4C                   ENDIF
314500021111     c
314600021111     C     wfvdvol       DIV(H)    BtPNCL        VOLU
314700030902     c*
314800050112     C     BtPNCp        ifeq      BtPNCL
314900021111     C                   Z-ADD     BtPpkC        Wfvdpes
315000021111     C*
315100021111   X4C                   ELSE
315200021111    5C     BtPpkC        IFGE      BtPpkb
315300021111     C                   Z-ADD     BtPpkC        Wfvdpes
315400021111   X5C                   ELSE
315500021111     C                   Z-ADD     BtPpkb        Wfvdpes
315600021111    5C                   ENDIF
315700021111    4C                   ENDIF
315800030902     c*
315900021111     C     wfvdpes       DIV(H)    BtPNCL        PKGU
316000940107     C*
316100940107     C                   ENDSR
316200950110     C*
316300950110     C* CALCOLO VOLUME E PESO UNITARI FNBLT--------------------------*
316400950110     C     CALUNI        BEGSR
316500950110     C* COLLI / PESO / VOLUME UNITARI
316600950110     C                   Z-ADD     1             NCLU
316700021111     c
316800050112     C     BLPNCR        ifeq      BLPNCL
316900021111     C                   Z-ADD     BLPVLC        Wfvdvol
317000021111     C*
317100021111   X4C                   ELSE
317200021111    5C     BLPVLC        IFGE      BLPVLF
317300021111     C                   Z-ADD     BLPVLC        Wfvdvol
317400021111   X5C                   ELSE
317500021111     C                   Z-ADD     BLPVLF        Wfvdvol
317600021111    5C                   ENDIF
317700021111    4C                   ENDIF
317800030902     c*
317900021111     C     wfvdvol       DIV(H)    BLPNCL        VOLU
318000021111     c
318100050112     C     BLPNCp        ifeq      BLPNCL
318200021111     C                   Z-ADD     BLPpkC        Wfvdpes
318300021111     C*
318400021111   X4C                   ELSE
318500021111    5C     BLPpkC        IFGE      BLPpkF
318600021111     C                   Z-ADD     BLPpkC        Wfvdpes
318700021111   X5C                   ELSE
318800021111     C                   Z-ADD     BLPpkF        Wfvdpes
318900021111    5C                   ENDIF
319000021111    4C                   ENDIF
319100030902     c*
319200021111     C     wfvdpes       DIV(H)    BLPNCL        PKGU
319300950110     C*
319400950110     C                   ENDSR
319500930708     C*
319600950110     C*--- AGGIORNO CAMPI DI FNFVD -----------------------------------*
319700931022     C     AGGFVD        BEGSR
319800931022     C*
319900980210    1C     NCLU          IFGT      0
320000950110     C* ESCLUSE LE BOLLE RECUPERO: MA DAL MOMENTO CHE NON HANNO COLLI.
320100950110     C*  NON MI PREOCCUPO
320200970221     C* ESCLUDO IL CODICE SOCIALE 2 ===> SDI  SE TROVATA BOLLA
320300980210    2C     *IN19         IFEQ      *ON
320400970221     C     *IN20         ANDEQ     *OFF
320500070223     C***  BLPSCL        ANDNE     '2'
320600970221     C* ELABORO SE NON TROVATA BOLLA
320700970221     C     *IN19         OREQ      *ON
320800970221     C     *IN20         ANDEQ     *ON
320900980210     C* ELABORO SE E' COLLO DISGUIDO O TRANSITO
321000950110     C     *IN19         OREQ      *OFF
321100970221     C*
321200950110     C* TIPO RECORD 1= BOLLE DELL'AREA    2=BOLLE TRANSITO
321300950515     C                   MOVEL     '2'           WTRC
321400950515     C*
321500980210    3C     *IN19         IFEQ      *ON
321600950515     C                   MOVEL     '1'           WTRC
321700980210     C                   ELSE
321800980210     C     WLNP          CHAIN     AZORG01L                           18
321900980210    4C     ORGDIT        IFEQ      'SDI'
322000980210     C                   MOVEL     '3'           WTRC
322100980210    4C                   ENDIF
322200980210    3C                   ENDIF
322300931214     C*
322400950110     C     KFVD2         CHAIN     FNFVD000                           18
322500950110     C   18              CLEAR                   FNFVD000
322600950110     C*
322700950110     C* COLLI
322800950110     C                   ADD       NCLU          FVDCLA
322900931026     C*
323000950515     C* SE TROVATA LA BOLLA CALCOLO PER BENE IL PARTITO
323100980210    3C     WTRAN         IFEQ      'S'
323200970221     C     *IN19         OREQ      *ON
323300970221     C     *IN20         ANDEQ     *OFF
323400950515     C                   EXSR      RIEFVD
323500980210    3C                   ENDIF
323600931022     C*
323700980210    3C     *IN18         IFEQ      *ON
323800931022     C* SOLO PER IMMISSIONE
323900931022     C                   Z-ADD     FGVLNP        FVDLNP
324000931022     C                   Z-ADD     FGV2          FVDNFV
324100931022     C                   Z-ADD     SAVLNA        FVDLNA
324200931022     C                   Z-ADD     COMLIN        FVDFLP
324300950110     C                   MOVEL     WTRC          FVDTRC
324400931022     C*
324500950110     C                   WRITE     FNFVD000
324600931022     C*
324700931022     C                   ELSE
324800931022     C*
324900931022     C                   MOVEL     ' '           FVDFTR
325000931022     C                   Z-ADD     0             FVDDTR
325100931022     C*
325200950110     C                   UPDATE    FNFVD000
325300980210    3C                   ENDIF
325400931022     C*
325500980210    2C                   ENDIF
325600980210    1C                   ENDIF
325700931022     C                   ENDSR
325800931022     C*
325900950110     C*--- RIEMPO CAMPI DI FNFVD -------------------------------------*
326000931022     C     RIEFVD        BEGSR
326100931022     C***
326200931022     C***  19 OFF  -   T  R  A  N  S  I  T  I
326300931022     C***
326400950515     C     WTRAN         IFEQ      'S'
326500930708     C* PESO
326600950113     C     BTPPKB        IFGT      3500
326700030213      * Peso medio
326800030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
326900950123     C                   ADD       PKGU          FVDKOA
327000930708     C                   ELSE
327100030213      * Peso medio
327200030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
327300950123     C                   ADD       PKGU          FVDKFA
327400930708     C                   ENDIF
327500950110     C* VOLUME
327600950110     C                   Z-ADD     1             Z                 3 0
327700950110     C     BTPTSP        LOOKUP    TSP(Z)                                 31
327800950110     C*
327900950110     C* NON TROVATO TIPO SERVIZIO --> LO IMPOSTO IN SCHIERA
328000950110     C     *IN31         IFEQ      *OFF
328100950110     C                   Z-ADD     1             Z
328200950110     C     ' '           LOOKUP    TSP(Z)                                 31
328300950110     C   31              MOVEL     BTPTSP        TSP(Z)
328400950110     C  N31              Z-ADD     1             Z
328500950110     C                   ENDIF
328600950110     C*
328700030213      * Volume medio
328800030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
328900950110     C                   ADD       VOLU          VOL(Z)
329000930708     C*
329100950110     C                   ADD       WSPET         FVDSNA
329200931022     C*
329300931022     C                   ELSE
329400931022     C***
329500970221     C***     A  R  E  A
329600931022     C***
329700930709     C*
329800930708     C* PESO
329900130530     C**   BLPPKF        IFGT      3500
330000130530     c                   if        wfvdpes>3500
330100030213      * Peso medio
330200030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
330300931022     C                   ADD       PKGU          FVDKOA
330400931022     C                   ELSE
330500030213      * Peso medio
330600030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
330700931022     C                   ADD       PKGU          FVDKFA
330800930708     C                   ENDIF
330900930708     C*
331000950110     C* VOLUME
331100950110     C                   Z-ADD     1             Z                 3 0
331200950110     C     BLPTSP        LOOKUP    TSP(Z)                                 31
331300950110     C*
331400950110     C* NON TROVATO TIPO SERVIZIO --> LO IMPOSTO IN SCHIERA
331500950110     C     *IN31         IFEQ      *OFF
331600950110     C                   Z-ADD     1             Z
331700950110     C     ' '           LOOKUP    TSP(Z)                                 31
331800950110     C   31              MOVEL     BLPTSP        TSP(Z)
331900950110     C  N31              Z-ADD     1             Z
332000950110     C                   ENDIF
332100950110     C*
332200030213      * Volume medio
332300030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
332400950110     C                   ADD       VOLU          VOL(Z)
332500950110     C                   ADD       WSPEN         FVDSNA
332600931022     C*
332700931022     C                   ENDIF
332800930708     C                   ENDSR
332900930708     C*
333000930709     C*--- CHAIN TABEL -----------------------------------------------*
333100930709     C     CTABEL        BEGSR
333200931021     C* EFFETTUO LA CHAIN IN TABEL SOLO SE CAMBIA IL TIPO BOLLA
333300950109     C     SAVCBO        IFNE      BLPCBO
333400930709     C                   MOVEL     '3A'          COD
333500930709     C                   MOVEL     *BLANKS       KEY
333600950109     C                   MOVEL     BLPCBO        KEY
333700930709     C     KTAB          CHAIN     TABEL                              31
333800931202     C     *IN31         IFEQ      *OFF
333900931202     C                   MOVEL     TBLUNI        DS3A
334000931202     C                   MOVEL     §3ATB1        COMTB1            1
334100931202     C                   ELSE
334200931202     C                   MOVE      ' '           §3ARBL
334300950413     C                   MOVEL     ' '           §3ABSD
334400950413     C                   MOVEL     ' '           §3ABFI
334500931202     C                   ENDIF
334600931202     C*
334700950109     C                   MOVEL     BLPCBO        SAVCBO
334800930709     C                   ENDIF
334900930709     C                   ENDSR
335000950113     C*
335100950113     C* RITRASMETTO IL RECORD TRANSITO ------------------------------*
335200950113     C     WTRBTP        BEGSR
335300950113     C                   MOVEL     BTPAAS        BLPAAS
335400950113     C                   MOVEL     BTPLNP        BLPLNP
335500950113     C                   MOVEL     BTPNRS        BLPNRS
335600950113     C                   MOVEL     BTPNSP        BLPNSP
335700950113     C                   MOVEL     BTPMGS        BLPMGS
335800950113     C                   MOVEL     BTPCBO        BLPCBO
335900950113     C                   MOVEL     BTPFNS        BLPFNS
336000950113     C                   MOVEL     BTPLNA        BLPLNA
336100950113     C                   MOVEL     BTPZNC        BLPZNC
336200950113     C                   MOVEL     BTPRSM        BLPRSM
336300950113     C                   MOVEL     BTPRSD        BLPRSD
336400950113     C                   MOVEL     BTPTSP        BLPTSP
336500950113     C                   MOVEL     BTPNCL        BLPNCL
336600950113     C                   MOVEL     BTPPKB        BLPPKB
336700950113     C                   MOVEL     BTPFVB        BLPFVB
336800950113     C                   MOVEL     BTPVLB        BLPVLB
336900950113     C                   MOVEL     BTPFLS        BLPFLS
337000950113     C                   MOVEL     BTPNCD        BLPNCD
337100950113     C                   MOVEL     BTPNCA        BLPNCA
337200950113     C                   MOVEL     BTPXCO        BLPXCO
337300950113     C                   MOVEL     BTPDBR        BLPDBR
337400021127     C                   MOVEL     BTPFLE        BLPFLE
337500950113     C                   MOVEL     BTPTFP        BLPTFP
337600950113     C                   MOVEL     BTPFEA        BLPFEA
337700950113     C                   MOVEL     BTPTFA        BLPTFA
337800091027     c* nei campi BLPNFV e BLPFLE imposto il foglio di partenza dei
337900091027     c*  colli per poter leggere queste bolle nel pgm della statistica
338000091027     c*    MC in arrivo
338100091027     c                   eval      blpnfv=fgv2
338200091027     c                   eval      blpfle=fgvlnp
338300091027     c
338400950113     C                   WRITE     FNBLPT00
338500950113     C                   ENDSR
338600911004     C**-----------------------------------------------*
338700911004     C** TRASFERISCO FOGLI VIAGGIO
338800911004     C**-----------------------------------------------*
338900911004     C     FVIAGG        BEGSR
339000150730     C                   Z-ADD     1             wnpg
339100150730     C                   Z-ADD     1             B
339200911004     C                   Z-ADD     0             X
339300930929     C                   CLEAR                   LP2
339400911106     C                   EXSR      RIEFVA
339500020403     c                   time                    w0060             6 0
339600911004     C*
339700940620    1C     FLP(B)        DOWGT     *ZEROS
339800920928     C*
339900960212     C* CONTROLLO SE LA SUA LINEA DI ARRIVO HA SPUNTE O BOLLE
340000960202     C                   MOVE      FFV(B)        BRVLNA
340100070123     C     KBRC2         SETLL     FNBRV02                                30
340200130530     C  N30k03BLP77      setll     FNBLP77L                               30
340300960402     C   30              MOVEL     'S'           WBOL              1
340400960402     C  N30              MOVEL     ' '           WBOL              1
340500920928     C*
340600960212     C* PER I FV NON ABBINATI DA NON TRASMETTERE IN SEDE, INVIO SOLO
340700960212     C*  SE HO BOLLE O SPUNTE
340800960402    2C     WBOL          IFEQ      'S'
340900960212     C     FGVTRN        ORGT      0
341000960212     C     §TVUTC        OREQ      'S'
341100040329     c* escludo fogli viaggio poste locali che non sono da trasmettere
341200040329     c* in arrivo
341300040329   2bc     wfvpt         ifne      'S'
341400040329     c     fgvttr        orne      'L'
341500020403     c*
341600020403     c                   add       1             w0060             6 0
341700960212     C*
341800911004     C                   MOVEL     FLP(B)        COMLIN
341900040329     C                   CLEAR                   fnlv54ds
342000040317     C****               MOVEL     'P'           D55TPT
342100040329     c                   z-add     fgvlnp        i54lnp
342200040329     C* Se fv poste gli passo il p.o. poste
342300040329     C     WFGW          IFEQ      'S'
342400040329     C     FGWFGS        ANDGT     *ZEROS
342500040329     C                   Z-ADD     FGWFGS        i54LNP
342600040329     C                   ENDIF
342700040329     C                   MOVEL     COMLIN        i54LIN
342800040329     C                   MOVEL     FGVDFV        i54drf
342900040329     C                   CALL      'FNLV54R'
343000040329     C                   PARM                    fnlv54ds
343100040329    3C     o54ERR        IFEQ      *BLANKS
343200920928     C*
343300911004     C* SE NON LO HO GIA' INVIATO INVIO
343400921202     C     COMLIN        LOOKUP    LP2                                    20
343500940620    4C  N20              DO
343600040826     C                   ADD       1             X                 4 0
343700921202     C                   MOVEL     COMLIN        LP2(X)
343800921202     C*
343900921202     C* TRASMETTO SE NON E' LOCALE
344000040329    5C     o54tfp        IFNE      FGVLNP
344100040329     c     §tvdef        andne     'S'
344200040329     c     fgvlnp        orne      o54tfa
344300911004     C*
344400040329     C                   MOVEL     o54TFP        VARX
344500921202     C                   EXSR      CAEXFV
344600020403     c*
344700911004     C*
344800960126     C* TRASMETTO SOLO SE FV APERTO O SFLAGGATO(DA TRASMETTERE)
344900960126    6C     FGVFCF        IFEQ      ' '
345000960126     C     FGVFTR        OREQ      ' '
345100950113     C                   MOVEL     ' '           FVAATR
345200921202     C                   MOVEL     COMLIN        FVALAI
345300950113     C                   WRITE     FNFVA0TT
345400000428    7C     WFGW          IFEQ      'S'
345500000428     C                   MOVEL     COMLIN        FWALAI
345600000428     C                   WRITE     FNFWA000
345700000428    7C                   ENDIF
345800060710     c* Scrittura del file FNFV4
345900060710     c                   EXSR      SCRIVIFV4
346000020807    6C                   ENDIF
346100010622      *
346200940620    5C                   ENDIF
346300940620    4C                   ENDDO
346400940620    3C                   ENDIF
346500920427     C*
346600940620     C* R I T R A S M E T T O   F O G L I O   X   T R A N S I T I  SE:
346700940620     C*
346800971105     C* 1) SE NON E' UN FOGLIO LOCALE (TFP DELLO SCARICO=FGVLNP)
346900940620     C*    E LA LINEA ARRIVO E' DIVERSA DALLO SCARICO
347000960402     C*    E SOLO SE HA BOLLE O SPUNTE
347100050927     C*    oppure locale con spunte transito
347200020807    3C     FFV(B)        IFNE      FLP(B)
347300960402     C     WBOL          ANDEQ     'S'
347400050927     c     winvloc(B)    oreq      'S'
347500921202     C*
347600920427     C                   MOVEL     FFV(B)        COMLNA
347700970516     C* Richiamo pgm per determinare terminal di arrivo e partenza
347800970703     C* della lna
347900031110     C                   CLEAR                   fnlv54ds
348000031112     C                   MOVE      COMLNA        i54LIN
348100031112     C                   MOVE      FGVLNP        i54LNP
348200000620     C* Se fv poste gli passo il p.o. poste
348300000620     C     WFGW          IFEQ      'S'
348400000620     C     FGWFGS        ANDGT     *ZEROS
348500031112     C                   Z-ADD     FGWFGS        i54LNP
348600000620     C                   ENDIF
348700031112     C                   MOVE      FGVDFV        i54DRF
348800031110     C                   CALL      'FNLV54R'
348900031110     C                   PARM                    fnlv54ds
349000921202     C*
349100940620     C*
349200050203     C* 2) SE TFP DELLA LINEA ARRIVO NON E' LOCALE
349300031110    4C     O54TFP        IFNE      FGVLNP
349400040317     c     fgvlnp        orne      o54tfa
349500050203     c
349600050203     c* 3) Trasmetto anche il foglio locale se presenti delle spunte
349700050203     c*    transito
349800050203     c     winvloc(B)    oreq      'S'
349900940620     C*
350000130531     c* Trasmettere
350100130531     c*  sempre al terminal di arrivo, tranne quando ci sono  spunte
350200050203     c*  transito su un foglio locale (vale solo per le  linee di arriv
350300050203     c*  un po' terminal arrvioe un po' 2 livello in arrivo)
350400980126     C*
350500050203     C**   WINV          IFEQ      'T'
350600050203     c                   if        winvloc(B)=' '
350700031110     C                   MOVEL     o54TFA        WLNA
350800980126     C                   ELSE
350900980126     C                   MOVEL     COMLNA        WLNA
351000980126     C                   ENDIF
351100921118     C*
351200050203     C* 3) SE NON L'HO GIA'INVIATO
351300980126     C     WLNA          LOOKUP    LP2                                    20
351400940620    6C  N20              DO
351500921118     C                   ADD       1             X
351600980126     C                   MOVEL     WLNA          LP2(X)
351700921118     C*
351800050203     c* Devo trasmettere il fogio nel membro del terPaart del TerARR
351900050203     c*  dell alna, tranne se foglio locale con invio spunte transito
352000050203     c     winvloc(B)    ifeq      ' '
352100040217     C     O54TFP        andne     O54TFA
352200040218     c                   move      o54tfa        wtfa
352300130716     C*****              CLEAR                   dslv55
352400130716     c*****              movel     'P'           d55tpt
352500130716     C*****              MOVE      wtfa          d55LIN
352600130716     C*****              MOVE      FGVDFV        d55DRF
352700130716     C*****              CALL      'FNLV55R'
352800130716     C*****              PARM                    dslv55
352900130716     C                   CLEAR                   fnlv54ds
353000130716     c                   movel     'P'           i54tpt
353100130716     C                   MOVE      wtfa          i54LIN
353200130716     C                   MOVE      FGVDFV        i54DRF
353300130716     C                   CALL      'FNLV54R'
353400130716     C                   PARM                    fnlv54ds
353500040217     c                   endif
353600040217     c*
353700130716     C****               MOVEL     d55TFP        VARX
353800130716     C                   MOVEL     o54TFP        VARX
353900920427     C                   EXSR      CAEXFV
354000921202     C*
354100960126     C* TRASMETTO SOLO SE FV APERTO O SFLAGGATO(CIOE' DA TRASMETTERE)
354200960126    7C     FGVFCF        IFEQ      ' '
354300960126     C     FGVFTR        OREQ      ' '
354400950113     C                   MOVEL     'T'           FVAATR
354500980126     C                   MOVEL     WLNA          FVALAI
354600950113     C                   WRITE     FNFVA0TT
354700020807    8C     WFGW          IFEQ      'S'
354800000428     C                   MOVEL     WLNA          FWALAI
354900000428     C                   WRITE     FNFWA000
355000020807    8C                   ENDIF
355100060710     c
355200060710     c* non mi serve più scrivere fnfv4 in quanto ho già scritto prima
355300060710     c*  il file fisico, unico, che trasmette eventualente anche
355400060710     c*  in sede (una sola volta quindi basta)
355500060710     c*********          EXSR      SCRIVIFV4
355600060710    7C                   ENDIF
355700010622      *
355800940620    6C                   ENDDO
355900940620    4C                   ENDIF
356000040329    3c                   ENDIF
356100040329   2bc                   endif
356200960212    2C                   ENDIF
356300911004     C*
356400911004     C                   ADD       1             B
356500940620    1C                   ENDDO
356600911004     C*
356700951120     C* SCRIVO FILE DI APPOGGIO DA INVIARE IN SEDE SE:
356800951120     C*   - IL FOGLIO NON E' ABBINATO
356900951120     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' VUOTO
357000951120     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO ED E'
357100951120     C*     DA UTILIZZARE NEI CONTROLLI DI SEDE (§TVUTS = "S")
357200951120     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO MA NON
357300951120     C*     C'E' IL RECORD IN TABELLA
357400950113     C                   MOVEL     ' '           FVAATR
357500951120     C*
357600960212    1C     FGVTRN        IFEQ      0
357700960212    2C     FGVTTR        OREQ      ' '
357800960212    3C     §TVUTC        OREQ      'S'
357900950113     C                   WRITE     FNFGV046
358000951120    1C                   ENDIF
358100930709     C*
358200911004     C                   ENDSR
358300910910     C**-----------------------------------------------*
358400950516     C** SCARICO SPUNTE PER L'AS DELLA LNA
358500910910     C**-----------------------------------------------*
358600910912     C     WTRLNA        BEGSR
358700910910     C*
358800050204     c* Secondo me questo IF si può eliminare ..... (ES)
358900040317    4C     lnafel        IFne      FGVLNP
359000040317     c     fgvlnp        orne      lnatfa
359100960604     C*
359200960806     C                   MOVEL     'N'           WALCF
359300930520     C*
359400040217     c* se transito e ter-arr della lna e lna sono sullo stesso ogas
359500040217     c* trasmetto a ter-part del ter-arr di lna
359600040217     c     *in06         ifeq      *off
359700000725     C                   MOVEL     LNAFEL        VARX
359800040217     c                   else
359900130531     C                   CLEAR                   dslv55
360000130531     C                   MOVEL     'P'           d55TPT
360100130531     C                   z-add     lnatfa        d55LIN
360200130531     C                   Z-ADD     FGVDFV        d55DRF
360300130531     c                   call      'FNLV55R'
360400130531     c                   parm                    dslv55
360500130531     C                   MOVEL     d55tfp        VARX
360600040217     c                   endif
360700910910     C                   EXSR      CAEXSP
360800910910     C*
360900950113     C                   MOVEL     BRVLNA        COMLNA            3 0
361000950113     C                   MOVEL     SAVLNA        BRVLNA
361100070123     C     KBRC2         SETLL     FNBRV02
361200070123     C     KBRC2         READE(N)  FNBRV02                                29
361300910910     C*
361400960205    2C     *IN29         DOWEQ     '0'
361500000111     C                   CLEAR                   FISPP000
361600910910     C*
361700960604     C                   MOVEL     BRVLNP        KLNP
361800960604     C                   MOVEL     BRVLNA        KLNA
361900960604     C                   MOVEL     BRVNRS        KNRS
362000960604     C                   MOVEL     BRVNSC        KNSC
362100960604     C*
362200960604     C                   EXSR      RICLNP
362300960604     C*
362400960604     C     WLNP          LOOKUP    L1                                     20
362500940322     C*
362600940322     C* SPUNTA PER TERMINAL
362700960205    3C     *IN08         IFEQ      *ON
362800940322     C* SPUNTA CHE TRANSITO
362900960205    4C     *IN20         IFEQ      *OFF
363000970814     C     WTAB5F        ANDEQ     ' '
363100950113     C*
363200960604     C                   SELECT
363300960611     C     WOKBT         WHENEQ    *BLANKS
363400020606     C     KBtt37        SETLL     FNBTT37L                               34
363500960611     C     WOKBT         WHENEQ    'S'
363600960604     C                   SETON                                        34
363700960604     C                   ENDSL
363800971105     C                   MOVEL     FGVLNP        SPPFLP
363900950515     C* 34 ON - USCITA DA TRANSITO
364000960205    5C     *IN34         IFEQ      *ON
364100950515     C                   MOVEL     'T'           SPPATR
364200960205   X5C                   ELSE
364300990819     C* DEFLUENZA
364400990819     C* Se filiale gestione spunta è anche il terminal di arrivo
364500990819     C* non devo trasmettere la spunta come disguido ma come defluenza
364600990819     C* (N.B.: caso di Padova che ha spuntato colli con lnp 007 ed lna
364700990819     C* 059 sul foglio viaggio locale anzichè sulla defluenza)
364800990819     C     BRVFGS        IFEQ      LNATFA
364900990819     C                   MOVEL     'F'           SPPATR
365000990819     C                   ELSE
365100940322     C* DISGUIDO
365200960604     C                   MOVEL     'D'           SPPATR
365300990819     C                   ENDIF
365400960205    5C                   ENDIF
365500950515     C*
365600960205   X4C                   ELSE
365700960205     C                   MOVEL     ' '           SPPATR
365800950515     C                   CLEAR                   SPPFLP
365900960205    4C                   ENDIF
366000940322     C*
366100940322     C                   EXSR      RIESPP
366200000111     C                   WRITE     FISPP000
366300960205    3C                   ENDIF
366400940322     C*
366500940322     C* SPUNTA ATTESA TRANSITO
366600980210    3C     *IN06         IFEQ      *ON
366700950515     C*
366800960205    4C     *IN20         IFEQ      *ON
366900970814     C     WTAB5F        OREQ      'S'
367000940322     C                   MOVEL     'A'           SPPATR
367100950215     C                   MOVEL     COMLIN        SPPFLP
367200940322     C                   EXSR      RIESPP
367300000111     C                   WRITE     FISPP000
367400960205    4C                   ENDIF
367500960205    3C                   ENDIF
367600910910     C*
367700070123     C     KBRC2         READE(N)  FNBRV02                                29
367800960205    2C                   END
367900910910     C*
368000940322     C* MI RIPOSIZIONO SULLA NUOVA LINEA ARRIVO LETTA
368100970814    2C     WLAST         IFEQ      *BLANKS
368200950113     C                   MOVEL     COMLNA        BRVLNA
368300070123     C     KBRC2         SETLL     FNBRV02
368400070123     C     KBRC2         READE     FNBRV02                                22
368500960708     C*
368600970814    3C     *IN22         IFEQ      *OFF
368700960708     C                   MOVE      'S'           WALCF
368800960708     C                   MOVEL     BRVLNP        KLNP
368900960708     C                   MOVEL     BRVLNA        KLNA
369000960708     C                   MOVEL     BRVNRS        KNRS
369100960708     C                   MOVEL     BRVNSC        KNSC
369200960708     C                   EXSR      RICLNP
369300970814    3C                   END
369400970814    2C                   END
369500021127    1C                   END
369600910910     C*
369700910910     C                   ENDSR
369800910911     C**-----------------------------------------------*
369900910911     C** CREO SPUNTE PER AUTOSPUNTA
370000910911     C**-----------------------------------------------*
370100910911     C     CRTSPU        BEGSR
370200140618     C                   CLEAR                   FNBRV05
370300950109     C                   MOVEL     BLPFLS        BRVLNP
370400950109     C                   MOVEL     BLPLNA        BRVLNA
370500950109     C                   MOVEL     BLPNRS        BRVNRS
370600950109     C                   MOVEL     BLTNSC        BRVNSC
370700950109     C                   MOVEL     BLPZNC        BRVZNC
370800950109     C                   MOVEL     '90'          BRVNPS
370900950109     C                   Z-ADD     WNPG          BRVNPG
371000000111     C                   Z-ADD     FGV2          BRVNFV
371100000111     C                   Z-ADD     FGVLNP        BRVPES
371200971105     C                   MOVEL     FGVLNP        BRVFGS
371300971107     C                   MOVEL     FGVLNP        BRVFLE
371400950109     C                   Z-ADD     BLTVUC        BRVVUC
371500990906     C                   Z-ADD     BLTPUC        BRVPUC
371600950109     C                   MOVEL     BLTCAN        BRVCAN
371700070219     C***                TIME                    W0140            14 0
371800070219     C***                MOVEL     W0140         BRVHMS
371900070219     C***                CLEAR                   WLBDAT
372000070219     C***                MOVE      W0140         G02DAT
372100070219     C***                CALL      'XSRDA8'
372200070219     C***                PARM                    WLBDAT
372300070219     C***                Z-ADD     G02INV        BRVDFS
372400070219     c                   time                    wtimez
372500070219     c* millisecondi
372600070219     c                   extrct    wtimez:*ms    millisec          6 0
372700070219     c                   movel     millisec      brvmis
372800070219     c* data immissione
372900070219     c                   movel     wtimez        dataiso
373000070219     c                   movel     dataiso       brvdfs
373100070219     c* ora immissione
373200070219     c                   move      wtimez        oraiso
373300070219     c                   movel     oraiso        brvhms
373400070219     c                   movel     'A'           brvtap
373500070219     c                   eval      brvpru='§'+ nompgm
373600910911     C*
373700140618     C                   WRITE     FNBRV05
373800910911     C                   ENDSR
373900910911     C**-----------------------------------------------*
374000910911     C** CREO RECORD ANOMALIA
374100910911     C**-----------------------------------------------*
374200910911     C     CRTANM        BEGSR
374300921022     C                   MOVEL     *BLANKS       ANMATB
374400960514     C                   MOVE      BRVFGS        ANMCDU
374500921022     C                   Z-ADD     0             ANMAAS
374600990517     C                   Z-ADD     0             ANMDUE
374700990517     C                   Z-ADD     0             ANMNPG
374800990517     C                   Z-ADD     0             ANMFGS
374900990518     C                   Z-ADD     0             ANMFL3
375000040531     C                   Z-ADD     999           ANMFL4
375100990518     C                   Z-ADD     0             ANMDTR
375200990518     C                   MOVE      *BLANK        ANMFLO
375300040603     c* data ora di creazione
375400050707     c***                z-add     *date         wdata             6 0
375500050707     c***                time                    wora              6 0
375600050707     c
375700050707     C                   TIME                    W0140            14 0
375800050707     C                   CLEAR                   WLBDAT
375900050707     C                   MOVE      W0140         G02DAT
376000050707     C                   CALL      'XSRDA8'
376100050707     C                   PARM                    WLBDAT
376200050707     C                   Z-ADD     G02INV        Wdata             6 0
376300050707     c                   movel     w0140         wora4             4 0
376400040603     c                   movel     wdata         anmflo
376500040603     c                   move      wora4         anmflo
376600040531     c
376700040531     C**   COMCAA        IFEQ      105
376800040531     c* la zona la appoggio ora in anmfl4
376900040531     C**                 CLEAR                   DANM01
377000040531     C**                 MOVE      BRVZNC        §ANZNC
377100040531     C**                 MOVEL     DANM01        ANMFLO
377200040531     C**                 ENDIF
377300040531     c                   z-add     brvznc        anmfl4
377400040531     c
377500990518     C                   MOVE      *BLANK        ANMFT3
377600040603     c
377700110909     C                   MOVE      *BLANK        ANMFT4
377800110909     C                   Z-ADD     0             ANMLNP
377900110909     C                   Z-ADD     0             ANMNSP
378000110909     c
378100110909     c* imposto dati bolle per anomalie 110 e 115
378200110909     c* flag di valore
378300110909     c                   if        comcaa=110  or comcaa=115
378400040603     c                   movel     §ar5bva       anmft4
378500110909     c                   eval      anmaas=bltaas
378600110909     c                   eval      anmlnp=bltlnp
378700110909     c                   eval      anmnsp=bltnsp
378800040603     c                   endif
378900110909
379000921022     C                   MOVEL     '1'           ANMFSA
379100950327     C                   MOVE      WDTP          ANMDAO
379200921022     C                   MOVEL     *BLANKS       ANMCCH
379300971107     C                   MOVEL     FGVLNP        ANMFLE
379400921022     C                   MOVEL     *BLANKS       ANMFT1
379500921022     C                   Z-ADD     0             ANMFL1
379600921022     C                   MOVEL     *BLANKS       ANMFT2
379700921022     C                   Z-ADD     0             ANMFL2
379800921022     C*
379900950113     C                   MOVEL     BRVLNP        ANMFLS
380000950113     C                   MOVEL     BRVLNA        ANMLNA
380100950113     C                   MOVEL     BRVNRS        ANMNRS
380200950113     C                   MOVEL     BRVNSC        ANMSCN
380300950113     C                   MOVEL     BRVNPS        ANMNPS
380400950113     C                   Z-ADD     BRVNFV        ANMNFV
380500911018     C                   Z-ADD     COMCAA        ANMCAA
380600911018     C                   MOVEL     '7C'          COD
380700911018     C                   MOVEL     *BLANKS       KEY
380800911018     C                   MOVEL     COMCAA        KEY
380900911018     C     KTAB          CHAIN     TABEL                              21
381000911018     C  N21              MOVEL     TBLUNI        DS7C
381100911018     C   21              MOVEL     *BLANKS       DS7C
381200911018     C*
381300911018     C                   MOVEL     §7CTAN        ANMTAN
381400911018     C                   MOVEL     §7CSAN        ANMSAN
381500911018     C                   MOVEL     §7CAIE        ANMAIE
381600911018     C                   Z-ADD     §7CLID        ANMLID
381700910911     C                   Z-ADD     0             ANMDCH
381800911011     C                   MOVEL     *BLANKS       ANMFSC
381900911018     C     §7CCHA        IFEQ      'S'
382000950327     C                   MOVE      WDTP          ANMDCH
382100911018     C                   MOVEL     '1'           ANMFSC
382200911018     C                   END
382300910911     C*
382400990517     C                   WRITE     FNANM
382500910911     C                   ENDSR
382600910911     C**-----------------------------------------------*
382700910911     C** CHIUSURA ANOMALIE MANCA BOLLA
382800910911     C**-----------------------------------------------*
382900910911     C     CHIUAN        BEGSR
383000050927     C                   CLEAR                   WESISTEANM        1
383100990517     C     KANM3         CHAIN(N)  FNANM                              21
383200960416     C     *IN21         IFEQ      *OFF
383300130607     c                   exsr      call_lr33
383400050927     C                   eval      wesisteanm='S'
383500050927     C                   END
383600921014     C*
383700910911     C                   ENDSR
383800130607     C**-----------------------------------------------*
383900130607     C** chiamata a FNLR33R
384000130607     C**-----------------------------------------------*
384100130607     C     call_lr33     BEGSR
384200130607     C                   CLEAR                   DSLR33
384300130607     C                   MOVE      WDTP          D33DCH
384400130607     C                   MOVEL     '1'           D33FSC
384500130607     C                   MOVEL     '  '          D33CCH
384600130607     c                   if        comcaa=110  or comcaa=105
384700130607     C                   MOVEL     'BO'          D33CCH
384800130607     c                   endif
384900130607     C                   MOVE      ANMNR2        D33NRR
385000130607     C                   CALL      'FNLR33R'
385100130607     C                   PARM                    DSLR33
385200130607     c                   ENDSR
385300920324     C**-----------------------------------------------*
385400920324     C** ALLOCO MEMBRI DI TRASMISSIONE
385500920324     C**-----------------------------------------------*
385600920324     C     ALLOCO        BEGSR
385700950109     C* CARICO FILIALI ELABORATORE LINE FV
385800920324     C                   EXSR      RIEMPI
385900920324     C* ALLOCO MEMBRI DELLE LINEE DI ARRIVO
386000920324     C                   EXSR      LCKMBR
386100920324     C* ALLOCO MEMBRI LINEE DI TRANSITO
386200950915     C  N50              EXSR      LCKTRN
386300920526     C* NON RIESCO AL ALLOCARE: RISOTTOMETTO 3 VOLTE
386400950915     C   50              EXSR      ERROR
386500920324     C*
386600920324     C                   ENDSR
386700920324     C**-----------------------------------------------*
386800971105     C** CARICO TERMINAL DI PARTENZA DELLE LINEE DEL FV
386900920324     C**-----------------------------------------------*
387000920324     C     RIEMPI        BEGSR
387100920324     C*
387200921202     C                   Z-ADD     1             K                 3 0
387300920324     C                   Z-ADD     1             I                 3 0
387400920324     C                   Z-ADD     1             Y                 3 0
387500920324     C*
387600921202     C     FFV(I)        DOWGT     *ZEROS
387700920324     C                   MOVEL     FFV(I)        SAVFFV            3 0
387800971104     C*
387900971104     C                   CLEAR                   DSLV55
388000971104     C                   MOVEL     'P'           D55TPT
388100971104     C                   Z-ADD     SAVFFV        D55LIN
388200971105     C                   Z-ADD     FGVDFV        D55DRF
388300971104     C                   CALL      'FNLV55R'
388400971104     C                   PARM                    DSLV55
388500971104     C     D55TFP        LOOKUP    FEL                                    30
388600971104     C  N30              Z-ADD     D55TFP        FEL(K)
388700920324     C  N30              ADD       1             K
388800920324     C* FILIALI DI TRANSITO
388900920324     C     FLP(I)        IFGT      *ZEROS
389000920324     C                   MOVEL     FLP(I)        SAVFLP            3 0
389100920324     C* C'E' TRANSITO SE NON E' LA FIL DI ARRIVO
389200920324     C*                         LA SUA FILIALE ELABORATORE
389300930312     C*   SE E' IL SUO TERMINAL ALLOCO LO STESSO
389400920324     C     SAVFFV        IFNE      SAVFLP
389500960325     C*
389600971105     C     D55TFP        ANDNE     SAVFLP
389700930312     C*
389800971105     C                   CLEAR                   DSLV55
389900971105     C                   MOVEL     'P'           D55TPT
390000971105     C                   Z-ADD     SAVFLP        D55LIN
390100971105     C                   Z-ADD     FGVDFV        D55DRF
390200971105     C                   CALL      'FNLV55R'
390300971105     C                   PARM                    DSLV55
390400950215     C*
390500971105     C* SE IL TERMINAL DI PARTENZA NON E' FGVLNP
390600971105     C     D55TFP        IFNE      FGVLNP
390700971105     C     D55TFP        LOOKUP    FET                                    30
390800971105     C  N30              Z-ADD     D55TFP        FET(Y)
390900920324     C  N30              ADD       1             Y
391000920324     C                   END
391100950215     C                   END
391200920324     C                   END
391300921202     C*
391400921202     C                   ADD       1             I
391500920324     C                   END
391600050524     c
391700050524     c* Controllo riempimento schiera
391800050524     c                   clear                   trul0sds
391900050524     c                   eval      i0sski='FEL'
392000050524     c                   eval      i0sele=%elem(fel)
392100050524     c                   eval      i0spie=k-1
392200050524     c                   eval      i0sfile='TerPar LNA'
392300050524     c                   eval      i0ssif=knsif
392400130718     c                   eval      i0spgm='FNLSU2R'
392500050524     c                   movel     trul0sds      kpjbu
392600050524     c                   call      'TRUL0SR'
392700050524     c                   parm                    kpjba
392800050524     c
392900050524     c* Controllo riempimento schiera
393000050524     c                   clear                   trul0sds
393100050524     c                   eval      i0sski='FET'
393200050524     c                   eval      i0sele=%elem(fet)
393300050524     c                   eval      i0spie=y
393400050524     c                   eval      i0sfile='TerPar FLP'
393500050524     c                   eval      i0ssif=knsif
393600130718     c                   eval      i0spgm='FNLSU2R'
393700050524     c                   movel     trul0sds      kpjbu
393800050524     c                   call      'TRUL0SR'
393900050524     c                   parm                    kpjba
394000920324     C                   ENDSR
394100920324     C**-----------------------------------------------*
394200920324     C** ESEGUE CALL QCAEXEC SUL MEMBRO DELLE BOLLE    *
394300920324     C**-----------------------------------------------*
394400920324     C     CRTMBR        BEGSR
394500930127     C*
394600930127     C                   CALL      'TRUL50C'
394700930127     C                   PARM                    FISICO           10
394800930127     C                   PARM                    FISMBR           10
394900000428     C                   PARM                    O50PLA
395000930127     C                   PARM                    LOGICO           10
395100930127     C                   PARM                    LOGMBR           10
395200930127     C                   PARM                    ULFLG             1
395300930127     C* RITENTO L'ALLOCAZIONE
395400950915     C                   CALL      'QCMDEXC'                            50
395500930127     C                   PARM                    COMMAN
395600930127     C                   PARM                    LUNG
395700930127     C*
395800920324     C                   ENDSR
395900920324     C**-----------------------------------------------*
396000920324     C** ALLOCAZIONE MEMBRI
396100920324     C**-----------------------------------------------*
396200920324     C     LCKMBR        BEGSR
396300920324     C                   Z-ADD     1             K
396400950915     C                   SETOFF                                       50
396500021014     C                   MOVE      '))'          VAR2
396600000725     C                   MOVEL     'M'           VAR2
396700000731     C                   MOVEL     'M'           EMME
396800021014     C                   Z-ADD     42            LUNG
396900920324     C*
397000920324     C     FEL(K)        DOWNE     0
397100950109     C* ALLOCO MEMBRO FNBLP
397200000725     C                   MOVEL     FEL(K)        VAR2X
397300930127     C                   MOVEL     FEL(K)        NUM3
397400920324     C**
397500920324     C** SE NON CI SONO BOLLE DA INVIARE E' INUTILE ALLOCARE
397600950115     C* ALLOCAZIONE FILE DELLE BOLLE
397700950115     C   09              EXSR      LCKBLP
397800920324     C**
397900971105     C* ALLOCO SOLO SE IL MEMBRO DA SCRIVERE NON è LOCALE COME TERMINAL
398000971105     C* DI PARTENZA
398100040331    1C  N50              DO
398200960124     C* SE VENGO DA MANUTENZIONE FV NON DEVO ALLOCARE IL FILE SPUNTE
398300960124    2C     PA2FLG        IFNE      'M'
398400960124     C**
398500000111     C                   MOVE      VAR2          CM47
398600920324     C                   MOVEL     *BLANKS       COMMAN           80
398700921202     C                   MOVEA     CM47(1)       COMMAN
398800950915     C                   CALL      'QCMDEXC'                            50
398900920324     C                   PARM                    COMMAN
399000920324     C                   PARM                    LUNG
399100930127     C*
399200930127     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
399300930127     C*  SE MANCA E RITENTO
399400960124    3C     *IN50         IFEQ      '1'
399500000111     C                   MOVEL     'FISPP00T'    FISICO
399600930127     C                   EXSR      CRTMBR
399700960124    3C                   END
399800960124    2C                   ENDIF
399900920324     C**
400000950915    2C  N50              DO
400100951116     C                   MOVE      VAR2          CM49
400200920324     C                   MOVEL     *BLANKS       COMMAN           80
400300921202     C                   MOVEA     CM49(1)       COMMAN
400400950915     C                   CALL      'QCMDEXC'                            50
400500920324     C                   PARM                    COMMAN
400600920324     C                   PARM                    LUNG
400700930127     C*
400800930127     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
400900930127     C*  SE MANCA E RITENTO
401000950915    3C     *IN50         IFEQ      '1'
401100951116     C                   MOVEL     'FNFVA01T'    FISICO
401200930127     C                   EXSR      CRTMBR
401300950115    3C                   END
401400960123     C*
401500960123    3C  N50              DO
401600000428     C                   MOVE      VAR2          CM48
401700000428     C                   MOVEL     *BLANKS       COMMAN           80
401800000428     C                   MOVEA     CM48(1)       COMMAN
401900000428     C                   CALL      'QCMDEXC'                            50
402000000428     C                   PARM                    COMMAN
402100000428     C                   PARM                    LUNG
402200000428     C*
402300000428     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
402400000428     C*  SE MANCA E RITENTO
402500000428    5C     *IN50         IFEQ      '1'
402600000428     C                   MOVEL     'FNFWA00T'    FISICO
402700000428     C                   EXSR      CRTMBR
402800000428    5C                   END
402900960123    3C                   END
403000960123     C*
403100950115    2C                   END
403200950115    1C                   END
403300950115     C**
403400950915     C  N50              ADD       1             K                 3 0
403500950915     C   50              Z-ADD     149           K
403600920324     C                   END
403700920324     C*
403800920324     C                   ENDSR
403900950115     C*
404000950115     C*--------------------------------------
404100950115     C* ALLOCAZIONE FILE DELLE BOLLE
404200950115     C*--------------------------------------
404300950115     C     LCKBLP        BEGSR
404400950115     C*
404500971104     C                   MOVE      VAR2          CM41
404600950115     C                   MOVEL     *BLANKS       COMMAN           80
404700971104     C                   MOVEA     CM41(1)       COMMAN
404800950915     C                   CALL      'QCMDEXC'                            50
404900950115     C                   PARM                    COMMAN
405000950115     C                   PARM                    LUNG
405100950115     C*
405200950115     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
405300030107     C*  SE MANCA E RITENTO
405400950915    1C     *IN50         IFEQ      '1'
405500971104     C                   MOVEL     'FNBLPE0T'    FISICO
405600950115     C                   EXSR      CRTMBR
405700950115    1C                   END
405800950115     C*
405900950115     C                   ENDSR
406000920324     C**-----------------------------------------------*
406100920324     C** ALLOCAZIONE MEMBRI  TRASMISSIONE
406200920324     C**-----------------------------------------------*
406300920324     C     LCKTRN        BEGSR
406400920324     C                   Z-ADD     1             K
406500950915     C                   SETOFF                                       50
406600920324     C     FET(K)        DOWNE     0
406700930310     C*
406800000725     C                   MOVEL     FET(K)        VAR2X
406900930310     C*
407000930310     C                   EXSR      TRANSI
407100930310     C*
407200950915     C  N50              ADD       1             K                 3 0
407300950915     C   50              Z-ADD     149           K
407400920324     C                   END
407500920324     C*
407600920324     C                   ENDSR
407700930310     C**-----------------------------------------------*
407800080423     C** ALLOCAZIONE FILE DEL TRANSITO
407900930310     C**-----------------------------------------------*
408000930310     C     TRANSI        BEGSR
408100000725     C                   MOVEL     VAR2X         NUM3
408200950109     C                   MOVE      VAR2          CM51
408300930310     C                   MOVEL     *BLANKS       COMMAN           80
408400930310     C                   MOVEA     CM51(1)       COMMAN
408500950915     C                   CALL      'QCMDEXC'                            50
408600930310     C                   PARM                    COMMAN
408700930310     C                   PARM                    LUNG
408800930310     C*
408900930310     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
409000930310     C*  SE MANCA E RITENTO
409100950915    1C     *IN50         IFEQ      '1'
409200950109     C                   MOVEL     'FNBLPT0T'    FISICO
409300930310     C                   EXSR      CRTMBR
409400950215    1C                   END
409500930310     C*
409600950915    1C  N50              DO
409700000111     C                   MOVE      VAR2          CM47
409800950215     C                   MOVEL     *BLANKS       COMMAN           80
409900950215     C                   MOVEA     CM47(1)       COMMAN
410000950915     C                   CALL      'QCMDEXC'                            50
410100950215     C                   PARM                    COMMAN
410200950215     C                   PARM                    LUNG
410300950215     C*
410400950215     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
410500950215     C*  SE MANCA E RITENTO
410600950915    3C     *IN50         IFEQ      '1'
410700000111     C                   MOVEL     'FISPP00T'    FISICO
410800950215     C                   EXSR      CRTMBR
410900950215    3C                   END
411000950215     C**
411100950915    3C  N50              DO
411200951116     C                   MOVE      VAR2          CM49
411300950215     C                   MOVEL     *BLANKS       COMMAN           80
411400950215     C                   MOVEA     CM49(1)       COMMAN
411500950915     C                   CALL      'QCMDEXC'                            50
411600950215     C                   PARM                    COMMAN
411700950215     C                   PARM                    LUNG
411800950215     C*
411900950215     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
412000950215     C*  SE MANCA E RITENTO
412100950915    4C     *IN50         IFEQ      '1'
412200951116     C                   MOVEL     'FNFVA01T'    FISICO
412300950215     C                   EXSR      CRTMBR
412400950215    4C                   END
412500950215     C**
412600960123    4C  N50              DO
412700000428     C**
412800000428     C                   MOVE      VAR2          CM48
412900000428     C                   MOVEL     *BLANKS       COMMAN           80
413000000428     C                   MOVEA     CM48(1)       COMMAN
413100000428     C                   CALL      'QCMDEXC'                            50
413200000428     C                   PARM                    COMMAN
413300000428     C                   PARM                    LUNG
413400000428     C*
413500000428     C* SE NON RIUSCITA L'ALLOCAZIONE PROVO AD AGGIUNGERE IL MEMBRO
413600000428     C*  SE MANCA E RITENTO
413700000428    6C     *IN50         IFEQ      '1'
413800000428     C                   MOVEL     'FNFWA00T'    FISICO
413900000428     C                   EXSR      CRTMBR
414000000428    6C                   END
414100960123    4C                   END
414200960123     C*
414300950215    3C                   END
414400950215    1C                   END
414500930310     C                   ENDSR
414600920526     C*
414700920526     C* RISOTTOMETTO AZIONE PERCHE' NON RIESCO AD ALLOCARE -----------*
414800920526     C     ERROR         BEGSR
414900111019     C     NUM2          IFEQ      2
415000960129     C*
415100960129     C* SE VENGO DA MANUTENZIONE FV NON DEVO STAMPARE
415200111026     C***  PA2FLG        IFNE      'M'
415300111026     C***                OPEN      SYSPRT
415400111026     C***                EXCEPT    TESERR
415500111026     C***                EXCEPT    DETERR
415600111026     C***                CLOSE     SYSPRT
415700111026     C***                ENDIF
415800960129     C*
415900111019     C* SE NON RIESCO AD ALLOCARE INVIO mail a sede
416000111019     C                   movel     *blanks       wrkEml          253
416100111019     C                   movel     *blanks       wrkMsg         5000
416200111019     C                   movel     *blanks       wrkOgg           44
416300111019     c
416400111019     C                   MOVEL     MSG(1)        wrkogg
416500111019     C                   MOVEL(P)  MSG(2)        wrkmsg
416600111019     c                   eval      %subst(wrkmsg:41:3)=%editc(fgvlnp:'X')
416700111019     c                   eval      %subst(wrkmsg:45:6)=%editc(fgvnfv:'2')
416800120302     c                   eval      wrkeml='ced@brt.it'
416900111019     c
417000111019     C                   call(e)   'TIS701C'
417100111019     C                   parm                    wrkEml
417200111019     C                   parm                    wrkOgg
417300111019     C                   parm                    wrkMsg
417400920526     C*
417500920526     C                   ELSE
417600920526     C*
417700920526     C* RISOTTOMETTO L'AZIONE
417800920526     C                   ADD       1             NUM2
417900130524     C                   MOVEL     FNLSU2DS      KPJBU
418000130718     C                   MOVEL     'LSU2'        KCOAZ
418100920526     C                   CALL      'BCH10'
418200920526     C                   PARM                    KPJBA
418300920526     C* E RISOTTOMETTO AZIONE DI INVIO FILE
418400950130     C                   MOVEL     'LSA3'        KCOAZ
418500070423     c                   clear                   parama3
418600070423     c                   move      simfel        pa3tfp
418700070423     c                   movel(p)  parama3       kpjbu
418800931203     C                   CALL      'BCH10'
418900931203     C                   PARM                    KPJBA
419000920526     C                   END
419100920526     C                   ENDSR
419200930127     C**-----------------------------------------------*
419300000111     C** RIEMPO CAMPI DEL FILE FISPP00T
419400930127     C**-----------------------------------------------*
419500930127     C     INVSAI        BEGSR
419600950113     C                   MOVEL     '1'           SPPNPG
419700950113     C                   MOVEL     BRVLNP        SPPLNP
419800950113     C                   MOVEL     BRVLNA        SPPLNA
419900950113     C                   MOVEL     BRVNRS        SPPNRS
420000950113     C                   MOVEL     BRVNSC        SPPNSC
420100950113     C                   MOVEL     BRVNPS        SPPNPS
420200950113     C                   MOVEL     WDTP          SPPDFV
420300950113     C                   MOVEL     'T'           SPPFLG
420400000111     C                   WRITE     FISPP000
420500930127     C                   ENDSR
420600991019     C*
420700040311     C*--- ELABORAZIONE SPUNTE PARTENZA ------------------------------*
420800991019     C     SPUPAR        BEGSR
420900991019     C**
421000991019     C**   VEDO SE DEVO IMPOSTARVI PESO E/O VOLUME
421100000620     C                   CLEAR                   SAVSPE
421200130227     C                   SETOFF                                       45
421300991019     C*
421400040121     c* Invece che testare se ka cml/vdl, testo il flag se in sto terminal
421500040121     c*   si usa il peso  e/o volume sulle spunte
421600040609     c     pa2spv        ifeq      'S'
421700040609     c     pa2spp        oreq      'S'
421800991019     C* LETTO FV ENTRATA, LEGGO LE SUE SPUNTE
421900991019     C                   MOVEL     1             KVVNPG
422000000111     C                   Z-ADD     FGV2          KVVNFV
422100991019     C                   MOVEL     SIMFEL        KVVFGS
422200070123     C     KBRV32        SETLL     FNBRV02
422300070123     C     KBRV32        READE     FNBRV02                                32
422400991019     C*
422500991019    2C     *IN32         DOWEQ     *OFF
422600991019     C* ESCLUDO LE SPUNTE GIA' TRASMESSE
422700991019    3C     BRVFTR        IFEQ      ' '
422800000620     C*
422900040121    4c                   if        pa2spv='S' and brvvuc=0  or
423000040121     c                             pa2spp='S' and brvpuc=0
423100040121     c
423200991019     C                   MOVEL     DSBRV         SVBRVE
423300991019     C* IMPOSTO LA KEY PER CHAIN
423400991019     C                   Z-ADD     BRVVUC        WRVVUC
423500991019     C                   Z-ADD     BRVPUC        WRVPUC
423600991019     C                   EXSR      IMPKEY
423700991019     C*
423800070123     C     KBRV6         SETLL     FNBRV07L
423900070123     C     KBRV6         READE     FNBRV07L                               34
424000991019    5C     *IN34         DOWEQ     *OFF
424100991019     C**
424200120604     C* VEDO SE SPUNTA CON VOLUME o peso
424300991019    6C     WRVVUC        IFEQ      0
424400991019     C     BRVVUC        ANDGT     0
424500991019     C                   Z-ADD     BRVVUC        WRVVUC
424600991019    6C                   ENDIF
424700991019     C* VEDO SE SPUNTA CON PESO CML
424800991019    6C     WRVPUC        IFEQ      0
424900991019     C     BRVPUC        ANDGT     0
425000991019     C                   Z-ADD     BRVPUC        WRVPUC
425100991019    6C                   ENDIF
425200991019     C*
425300070123     C     KBRV6         READE     FNBRV07L                               34
425400991019    5C                   ENDDO
425500991019     C*
425600991019     C                   MOVEL     SVBRVE        DSBRV
425700991019     C                   Z-ADD     WRVVUC        BRVVUC
425800991019     C                   Z-ADD     WRVPUC        BRVPUC
425900991019     C* AGGIORNO SPUNTA PARTENZA
426000070123     C                   UPDATE    FNBRV02
426100991019    4C                   ENDIF
426200991019    3C                   ENDIF
426300991019     C*
426400070123     C     KBRV32        READE     FNBRV02                                32
426500991019    2C                   ENDDO
426600040609    1C                   ENDIF
426700991019     C**
426800991019     C                   ENDSR
426900940414     C*
427000940414     C*--- ELABORAZIONE ABBINAMENTO ----------------------------------*
427100040625     c     ABBIN         BEGSR
427200940418     C* APERTURA FILES
427300021021     C                   OPEN      FNANM20L
427400021021     C                   OPEN      FNBLT38L
427500020605     C                   OPEN      FNBTT38L
427600020916     C                   OPEN      FNFVV02L
427700001120     C                   OPEN      FNFVV01L
427800041217     c                   clear                   wcalLsB7          1
427900940418     C*
428000940415     C* PRENDO FASE ANOMALIA
428100940415     C                   CALL      'TRUL11R'
428200940415     C                   PARM                    PARAM9
428300940414     C*
428400940414     C                   CLEAR                   LFV
428500940414     C                   MOVEA     FFV           LFV
428600940414     C                   Z-ADD     1             I
428700940414     C*
428800170613     C***     PA2AGS        IFEQ      'S'
428900170613     C***                   SETON                                        44
429000170613     C***                   END
429100990917     C**
429200990917     C** SPUNTE DEL FOGLIO VIAGGIO PARTENZA
429300991019     C**   VEDO SE DEVO IMPOSTARVI PESO E/O VOLUME
429400991019     C                   EXSR      SPUPAR
429500000620     C*
429600000620     C                   CLEAR                   SAVSPE
429700940414     C*
429800940414     C* LEGGO TUTTE LE LINEE
429900950113    1C     LFV(I)        DOWGT     *ZEROS
430000940414     C                   MOVEL     LFV(I)        COMFIL
430100021021     c*
430200021021     c* per ogni linea di arrivo loopo sulle linee di partenza della L1
430300021021     c                   z-add     1             IL
430400021021    2c                   dow       l1(IL)>0
430500021021     c
430600021021     c                   eval      wlnp=l1(IL)
430700990917     C**
430800990917     C** BOLLE  PARTENZA
430900990917     C**
431000950915     C                   SETON                                        40
431100021021     C     KBLT          SETLL     FNBLT038
431200021021     C     KBLT          READE     FNBLT038                               32
431300940414     C*
431400021021    4C     *IN32         DOWEQ     '0'
431500000619     C*
431600970506     C* VEDO SE DEVO AGGIORNARE LA DATA DI ENTRATA
431700970506     C                   EXSR      AGGENT
431800970506     C**
431900950109     C* SE COLLI NON ANCORA PARTITI CERCO SOLO QUELLI BORDERIZZATI SUL
432000990303     C*  PER IL RESTO LI PRENDO TUTTI(DEVONO AVERE IL NUMERO FOGLIO
432100990303     C*  PIENO:ATTENZIONE ALLA "C" DELL'ESTERO)
432200000714     C*
432300130530    5C**** BLTNFV        IFEQ      FGV2
432400130530     C**** BLTFTR        ANDEQ     *BLANKS
432500990303     C*
432600130530     C     BLTFTR        ifeq      'C'
432700990303     C     BLTNFV        ANDGT     0
432800990303     C*
432900990303     C     BLTFTR        ORNE      *BLANKS
433000990303     C     BLTFTR        ANDNE     'C'
433100151126     c* Se parzializzo per KSC solo bolle di questo FGV
433200151126     c                   if        pa2ccm<=*zeros or bltnfv=fgv2
433300940414     C                   EXSR      CRTBRC
433400151126     c                   endif
433500151126     c
433600021021    5C                   ENDIF
433700000714     C**
433800021021     C     KBLT          READE     FNBLT038                               32
433900021021    4C                   ENDDO
434000021021     c
434100021021     c                   eval      IL=il+1
434200021021    2c                   enddo
434300021021     c
434400950915     C                   SETOFF                                       40
434500940414     C**
434600021025     C** BOLLE TRANSITO
434700940414     C**
434800151126     c                   if        pa2ccm<=*zeros
434900151126     C                   SETON                                        41
435000020605     C     KBtt38        SETLL     FNBTT38L
435100020605     C     KBtt38        READE     FNBTT38L                               32
435200940414     C*
435300950113    2C     *IN32         DOWEQ     '0'
435400080228     c* ignoro transito se bttlnp fa parte della £1 (può succedere x le
435500080228     c* trazioni ridotte quando alcuni 2 livello cambiano terminal di part)
435600080228     c     bttlnp        lookup    l1                                     90
435700080228     c                   if        not *in90
435800970506     C* VEDO SE DEVO AGGIORNARE LA DATA DI ENTRATA
435900970506     C                   EXSR      AGGENT
436000040422     c* se presente anomalia 200 dell'area non autogenero la spunta
436100040422     c* partenza perchè significa che il collo si trova presso un
436200040422     c* secondo livello in partenza di Simfel e quindi non posso
436300040422     c* dare per scontato che riparta
436400040422     c                   eval      wcrtspu='S'
436500040422     c                   z-add     200           comcaa
436600040503     c     kanm02        chain(N)  fnanm02l
436700040422     c                   if        %found(fnanm02l)
436800040422     c                             and anmdch = 0
436900040422     c     anmfle        lookup    l1                                     32
437000040422     c   32              eval      wcrtspu='N'
437100040422     c                   endif
437200970506     C**
437300040422     c     wcrtspu       ifeq      'S'
437400950109     C                   EXSR      CRTBRC
437500040422     c                   endif
437600080228
437700080228     c                   endif
437800940414     C*
437900020605     C     KBtt38        READE     FNBTT38L                               32
438000950113    2C                   ENDDO
438100940414     C                   SETOFF                                       41
438200151126     c                   endif
438300041217     c
438400041217     c* Eseguo call al pgm di calcolo volume e pesi
438500041217     c                   if        wcalLSB7='S'
438600041217     C                   Z-ADD     FGVLNP        PA1FEL
438700041217     C                   MOVEL     PARAM1        KPJBU
438800041217     C                   CALL      'FNLSB7R'
438900041217     C                   PARM                    KPJBA
439000041217     c
439100041217     c                   clear                   wcalLSB7
439200041217     c                   endif
439300940414     C**
439400970828     C** ANOMALIE DI DISGUIDO
439500940414     C**
439600000619     C     WFVPT         IFEQ      'N'
439700151126     c     pa2ccm        andle     *zeros
439800021021     C                   SETON                                        42
439900040115     c* Devo pulire questo campo perchè altrimenti potrebbe rimanere sporco
440000040115     c* e sbagliare la pistola in autogenerazione spunte partenza
440100040115     c                   clear                   wlocal
440200021021     c* loop per anmfle = a tutti i p.o. della £1
440300021021     C                   Z-ADD     1             IL                3 0
440400021021   1ac                   dow       l1(IL)>0
440500021021     c
440600021025     c                   eval      wlnp=l1(il)
440700940414     C                   Z-ADD     200           COMCAA
440800021021     C     KANM          SETLL     FNANM20
440900021021     C     KANM          READE     FNANM20                                32
441000940414     C*
441100950113    2C     *IN32         DOWEQ     '0'
441200940414     C* SE NON E' GIA' CHIUSA
441300950113    3C     ANMDCH        IFEQ      0
441400990517     C     WDTP          ANDGE     ANMDAO
441500960705     C*
441600020412     C                   CLEAR                   WCRT
441700960705     C* SE TROVO BOLLA PARTENZA CHIUDO ANOMALIA E NON AUTOGENERO SPUNTA
441800960705     C* (SE BOLLA DA ME CONFERMABILE E CONFERMABILE ANCHE DA ALTRI AS)
441900970828     C**
442000970828     C* ATTUALMENTE NON CREIAMO PIU' ANOMALIE DI DISGUIDO SE COLLO DA
442100970828     C*  ME CONFERMABILE ANCHE SE NON C'E' LA BOLLA PARTENZA PER CUI
442200970828     C*  NON HO PIU' BISOGNO DI CERCE LE ANOMALIE DISGUIDO
442300020412     c* 12/04/02: Ripristinata la routine srdisg in quanto se la 5f
442400020412     c* è stata inserita dopo il caricamento spunte il disguido può
442500020412     c* essere stato creato se al momento dello scarico spunte la
442600020412     c* bolla non era ancora stata confermata
442700020412    4C     anmnrs        ifgt      *zeros
442800020412     C                   exsr      srdisg
442900020412    4C                   endif
443000040406     c* disattivo autogenerazione spunte da anomalie di disguido: si voglion
443100040406     c* far partire i colli disguidati solo se spuntati in entrata o in par-
443200040406     c* tenza
443300040406     c                   eval      wcrt='N'
443400960705     C*
443500020412   3AC     wcrt          ifeq      ' '
443600070123     C     KANM5         CHAIN     FNBRV07                            31
443700950118    4C     *IN31         IFEQ      *OFF
443800950109     C* MI SALVO I CAMPI CHE MI SERVONO PER L'AGGIORNAMENTO
443900950109     C                   Z-ADD     BRVZNC        WRVZNC
444000950109     C                   Z-ADD     BRVVUC        WRVVUC
444100990906     C                   Z-ADD     BRVPUC        WRVPUC
444200980206     C     BRVCAN        IFNE      'E'
444300950113     C                   MOVEL     BRVCAN        WRVCAN
444400980206     C                   END
444500970506     C* IMPOSTO LA KEY PER CHAIN
444600970506     C                   EXSR      IMPKEY
444700970506     C* CREO SPUNTA PARTENZA
444800950109     C                   EXSR      CRTBRC
444900950113    4C                   END
445000020412   3AC                   endif
445100950113    3C                   END
445200940414     C*
445300021021     C     KANM          READE     FNANM20                                32
445400950113    2C                   ENDDO
445500021021     c                   eval      il=il+1
445600021021   1aC                   ENDDO
445700940414     C                   SETOFF                                       42
445800040624    1C                   ENDIF
445900940414     C**
446000960209     C** LEGGO LE SPUNTE ENTRATA PER CREARE EVENTUALI
446100950915     C** SPUNTE PARTENZA NON ANCORA CREATE
446200170613     C**  06/2017 non esiste più il caso di solo spunta 88 per cui questa loop
446300170613     C**          va sempre fatto
446400950915     C**
446500170613     C**     *IN44         IFEQ      *ON
446600170613     c**     pa2ccm        andle     *zeros
446700170613     c*
446800170613     c     pa2ccm        ifle      *zeros
446900170613     c
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
496800170621     C                   CLEAR                   spufgs
496900070123     C     KBRV6         SETGT     FNBRV07L
497000070123     C     KBRV6         READPE    FNBRV07L                               34
497100970828    2C     *IN34         DOWEQ     *OFF
497200970828     C**
497300970828     C* VEDO SE SPUNTA CON VOLUME CML
497400970828     C   42
497500970828     COR 43WRVVUC        IFEQ      0
497600970828    3C     BRVVUC        ANDGT     0
497700970828     C                   Z-ADD     BRVVUC        WRVVUC
497800970828    3C                   ENDIF
497900000428     C* VEDO SE SPUNTA CON PESO CML
498000990906     C   42
498100990906     COR 43WRVPUC        IFEQ      0
498200990906    3C     BRVPUC        ANDGT     0
498300990906     C                   Z-ADD     BRVPUC        WRVPUC
498400990906    3C                   ENDIF
498500970828     C**
498600970423     C* ESCLUDO LE CATEGORIE 1
498700970828    3C     BRVNPG        IFNE      1
498800980116     C* AD ECCEZIONE DELLE SPUNTE PARTENZA SU FV NON CONTENENTE BRVLNA
498900980116     C* (BRVFTR='L')
499000980116     C     BRVFTR        OREQ      'L'
499100970424     C* CALL AL PGM PER REPERIRE LA DATA FOGLIO
499200970424     C                   CLEAR                   DSLV53
499300970424     C                   MOVEL     BRVNPG        D53NPG
499400000111     C                   Z-ADD     BRVNFV        D53NFV
499500970424     C                   MOVEL     BRVFGS        D53FGS
499600970424     C                   CALL      'FNLV53R'
499700970424     C                   PARM                    DSLV53
499800970424     C*
499900970828    4C     D53DFV        IFGE      WDTP
500000170621     c* Memeorizzo solo se non ho già trovato la spunta del terminal
500100170621     c                   if        spufgs<>simfel
500200170613     c* memorizzo se spunta fatta dal terminal o dal secondo liv
500300970423     C                   MOVEL     98            WNPS
500400970528     C                   MOVEL     BRVNPS        SPUNPS
500500170613     C                   movel     brvfgs        spufgs
500600170613     c* Se spunta terminal partenza --> posso uscire dal ciclo di lettura
500700170613     c                   if        brvfgs=simfel
500800970423     C                   SETON                                        34
500900170613     C                   endif
501000170621     c
501100170621     c                   else
501200170621     c                   seton                                        34
501300170621     C                   endif
501400170613     C
501500970828   X4C                   ELSE
501600970828    5C     WNPS          IFNE      98
501700970423     C                   MOVEL     96            WNPS
501800970528     C                   MOVEL     BRVNPS        SPUNPS
501900170613     C                   movel     brvfgs        spufgs
502000970828    5C                   ENDIF
502100970828    4C                   ENDIF
502200970828    3C                   ENDIF
502300970423     C*
502400970828    3C     *IN34         IFEQ      *OFF
502500040121     c     pa2spv        oreq      'S'
502600970828     C     WRVVUC        ANDEQ     0
502700970828     C     *IN40         ANDEQ     *OFF
502800970828     C     *IN41         ANDEQ     *OFF
502900990907      *PESO
503000040121     c     pa2spp        oreq      'S'
503100990907     C     WRVPUC        ANDEQ     0
503200990907     C     *IN40         ANDEQ     *OFF
503300990907     C     *IN41         ANDEQ     *OFF
503400070123     C     KBRV6         READPE    FNBRV07L                               34
503500970828    3C                   ENDIF
503600970828    2C                   ENDDO
503700970423     C*
503800991019    1C                   ENDIF
503900950208     C*
504000980206     C                   CLEAR                   WBRVE
504100950208     C* SE HO TROVATO UNA SPUNTA LA RICHAINO
504200970828    0C     WDATU         IFEQ      'S'
504300970828     C     *IN33         ANDEQ     *OFF
504400070123     C     KBRV2         CHAIN     FNBRV02                            31
504500980206     C* MEMORIZZO SPUNTA PARTENZA ERRATA IN CAMPI DEL FILE STORICO
504600980206     C* SPUNTE PARTENZA ERRATE
504700980206     C  N31BRVFTR        IFEQ      'L'
504800980206     C                   MOVE      'S'           WBRVE             1
504900980209     C                   MOVEL     DSBRV         SVBRVE
505000980206     C                   END
505100970828    0C                   ENDIF
505200950109     C*
505300950109     C* SOLO PER I TRANSITI: AUTOGENERO SPUNTA PER BOLLE TRANSITO
505400950109     C*                      SOLO SE RELATIVO FOGLIO ABBINATO
505500950412    0C   41BTTDET        IFEQ      0
505600950127     C* COME PRIMA COSA VERIFICO SE PER CASO LA SPUNTA ESISTE
505700950127     C* MA NON HA AGGIORNATO IL RECORD
505800950109     C                   EXSR      ABBBTT
505900950412    1C     WABBIN        IFNE      'S'
506000950118     C                   SETON                                        33
506100950412    1C                   ENDIF
506200950412    0C                   ENDIF
506300940512     C*
506400950412    0C  N33              DO
506500000714     C**
506600070123     C* SCARICO SPUNTA ERRATA IN FNBRVE
506700980206     C     WBRVE         IFEQ      'S'
506800980209     C* CONTROLLO CHE IL COLLO NON SIA IN IMP
506900070123     C     KBRV01        SETLL     FNBRV01L                               37
507000010611     c     *in37         ifeq      *off
507100070219     c     kbrve         setll     fnbrve                                 99
507200010611     C  n99              MOVEL     SVBRVE        DSBRV
507300070123     C  N99              WRITE     FNBRVE
507400010611     c                   endif
507500980206     C                   END
507600140618     C                   CLEAR                   FNBRV05
507700950109     C*
507800950118     C                   MOVEL     FGVLNP        BRVFGS
507900000111     C                   Z-ADD     FGVLNP        BRVPES
508000000111     C                   Z-ADD     FGV2          BRVNFV
508100950118     C                   Z-ADD     1             BRVNPG
508200971107     C                   Z-ADD     FGVLNP        BRVFLE
508300070220     c                   time                    wtimez
508400070220     c* millisecondi
508500070220     c                   extrct    wtimez:*ms    millisec          6 0
508600070220     c                   movel     millisec      brvmis
508700070220     c* data immissione
508800070220     c                   movel     wtimez        dataiso
508900070220     c                   movel     dataiso       brvdfs
509000070220     c* ora immissione
509100070220     c                   move      wtimez        oraiso
509200070220     c                   movel     oraiso        brvhms
509300070220     c                   movel     'A'           brvtap
509400070220     c                   eval      brvpru='§'+ nompgm
509500080328     c                   clear                   brvtsu
509600950109     C*
509700950915     C   40              EXSR      CRTBLT
509800950109     C*
509900950109     C   41              EXSR      CRTBTT
510000950109     C*
510100950113     C   42              EXSR      CRTSAN
510200940414     C*
510300950915     C   43              EXSR      CRTSPE
510400970423     C**
510500970423     C** VEDO SE IMPOSTARE PISTOLA 98 O 96
510600970506     C**  SOLO SE NON SI TRATTA DI UNA PISTOLA 90 CHE VINCE
510700970507     C**  ESCLUDO SEMPRE I LOCALI PER I QUALI LA PISTOLA 98 96 NON ESIS
510800970506     C**
510900970507    1C     BRVNPS        IFNE      90
511000970507     C*
511100970507    2C     WNPS          IFEQ      96
511200970507    3C     WLOCAL        IFNE      'S'
511300970423     C                   MOVEL     WNPS          BRVNPS
511400970528     C* PER BOLLE TRANSITO SE L'UNICA SPUNTA PRESENTE E' UNA 89 CREO
511500970528     C*  UNA 89
511600971027     C* ANCHE PER DISGUIDO
511700971027     C   42
511800971027     COR 41SPUNPS        IFEQ      89
511900970528     C                   MOVEL     SPUNPS        BRVNPS
512000970528     C                   ENDIF
512100970507     C                   ELSE
512200970507     C* LA 96 LOCALE DIVENTA --> 90
512300970507     C                   MOVEL     90            BRVNPS
512400970507    3C                   ENDIF
512500970507     C**
512600970507   X2C                   ELSE
512700970423     C**
512800170613     C* LA PISTOLA 98   per chi ha il flag §ogagp = blank
512900170613     C* dal 06/2017     vale per collo spuntato nel giorno SOLO dal secondo liv
513000170613     C*                 tranne in caso di SEMPRE autogenerare da qualsiasi spunt
513100170613     C*
513200970507    3C     WNPS          IFEQ      98
513300170613     C     WLOCAL        ANDNE     'S'
513400170613     c     spufgs        andne     simfel
513500170613     c     pa2ags        andne     'S'
513600170613     C***     PARCML        ANDEQ     ' '
513700170613     C***     WLOCAL        ANDNE     'S'
513800170613     C***     pa2agp        andeq     *blanks
513900970423     C                   MOVEL     WNPS          BRVNPS
514000970528     C* PER BOLLE TRANSITO SE L'UNICA SPUNTA PRESENTE E' UNA 89 CREO
514100971027     C*  UNA 89 ANCHE PER I DISGUIDI
514200971027     C   42
514300971027     COR 41SPUNPS        IFEQ      89
514400970528     C                   MOVEL     SPUNPS        BRVNPS
514500970528     C                   ENDIF
514600970507    3C                   ENDIF
514700970507    2C                   ENDIF
514800040406     C*
514900970507    1C                   ENDIF
515000950915     C*
515100140618     C   31              WRITE     FNBRV05
515200070123     C  N31              UPDATE    FNBRV02
515300950203     C*
515400950203     C* SE AUTOGENERATA SPUNTA PARTENZA: AGGIORNO BOLLA TRANSITO
515500970507    1C     *IN41         IFEQ      *ON
515600950203     C     BRVNPS        ANDEQ     89
515700970423     C                   EXSR      CALL44
515800970507    1C                   END
515900950412    0C                   END
516000940414     C*
516100940414     C                   ENDSR
516200950109     C*
516300950109     C*--- VEDO SE COLLO TRANSITO DICHIARATO PARTITO E ABBINATO-------*
516400950109     C     ABBBTT        BEGSR
516500950109     C*
516600950118     C* CHAIN SULLE SPUNTE PER VEDERE SE C'E' LA SPUNTA PARTENZA DA
516700950118     C*  ALTRA FILIALE --> SE NON C'E' NON CREO SPUNTA
516800950118     C*
516900950118     C* IN QUESTO CASO NON IMPORTA CHE TESTI SE LA SPUNTA NON E' DI
517000950118     C* SIMFEL PERCHE' SE CI FOSSE AVREI AGGIORNATO LA DATA ENTRATA
517100950118     C* E QUINDI NON SAREI DENTRO A QUESTA ROUTIME
517200950109     C                   Z-ADD     1             WNPG
517300950109     C                   CLEAR                   WABBIN
517400140618     C     KBTT          CHAIN(N)  FNBRV05                            34
517500970828     C**
517600950109    1C     *IN34         DOWEQ     *OFF
517700970828     C                   CLEAR                   DSLV53
517800020617     C                   MOVEL     'T'           D53TFO
517900070123     C****               Z-ADD     FGVLNP        D53FEL
518000000111     C                   Z-ADD     BRVNFV        D53NFV
518100970828     C                   MOVEL     BRVNPG        D53NPG
518200970828     C                   MOVEL     BRVFGS        D53FGS
518300070123     C****               MOVEL     BRVFLE        D53FLF
518400970828     C                   MOVEL     BRVLNA        D53LNA
518500970828     C                   MOVEL     'A'           D53ABB
518600020617     c                   z-add     fgvlnp        d53lai
518700970828     C                   CALL      'FNLV53R'
518800970828     C                   PARM                    DSLV53
518900970828    2C     D53ERR        IFEQ      ' '
519000970828     C     D53NFA        ANDGT     0
519100950113     C                   MOVEL     'S'           WABBIN            1
519200970828     C                   SETON                                        34
519300970828     C                   ELSE
519400950109     C*
519500140618     C     KBTT          READE(N)  FNBRV05                                34
519600970828    2C                   ENDIF
519700950113    1C                   ENDDO
519800950109     C*
519900950109     C                   ENDSR
520000970507     C*
520100970507     C* CHAINO BOLLA PARTENZA O BOLLA TRANSITO -----------------------*
520200040628     c     CHABOL        BEGSR
520300970507     C**
520400970507     C* CHAINO BOLLA PARTENZA
520500970507     C**
520600000620     C   45
520700000620     COR 43
520800970507    1COR 40BLTSPE        IFNE      SAVSPE
520900970507     C                   MOVEL     'S'           WBOL              1
521000970507     C                   CLEAR                   WLOCAL
521100970507     C     KBLT3         CHAIN(N)  FNBLP01L                           37
521200970507     C                   MOVEL     BLTSPE        SAVSPE           16 0
521300970507     C**
521400970507    2C     *IN37         IFEQ      *ON
521500970507     C                   MOVEL     'N'           WBOL
521600970507     C                   MOVEL     'S'           §1BFG5
521700970507   X2C                   ELSE
521800970507     C*
521900970507     C* CONTROLLO SE E' BOLLA LOCALE
522000970507    3C     BLPTFP        IFEQ      BLPTFA
522100970714     C     BLPLNP        OREQ      BLPLNA
522200970507     C                   MOVEL     'S'           WLOCAL            1
522300970507    3C                   ENDIF
522400970507     C*
522500970507     C* COD TRATTAMENTO MERCE CHE GENERA IN AUTOMATICO LE SPUNTE PARTEN
522600970507     C     BLPCTM        LOOKUP    CTM                                    37
522700970507     C*     TROVATO= IMPOSTO §1BFG5 = N
522800970507     C   37              MOVEL     'N'           §1BFG5
522900970507     C* NON TROVATO= IMPOSTO §1BFG5 = S
523000970507     C  N37              MOVEL     'S'           §1BFG5
523100970507    2C                   ENDIF
523200040624    1C                   ENDIF
523300040624     C**
523400970507     C* CHAIN BOLLA TRANSITO
523500970507     C**
523600970507     C   41BTTSPE        IFNE      SAVSPE
523700970507     C                   CLEAR                   WLOCAL
523800020605     C     KBTT3         CHAIN(N)  FNBTP11L                           37
523900970507     C                   MOVEL     BTTSPE        SAVSPE
524000970507     C                   ENDIF
524100970507     C                   ENDSR
524200950109     C*
524300950109     C* CREO SPUNTA DA FNBLT -----------------------------------------*
524400950109     C     CRTBLT        BEGSR
524500130226     c                   eval      wtfp90='N'
524600950109     C                   MOVEL     BLTFLS        BRVLNP
524700950109     C                   MOVEL     BLTLNA        BRVLNA
524800950109     C                   MOVEL     BLTNRS        BRVNRS
524900950113     C                   MOVEL     BLTNSC        BRVNSC
525000970507     C* CHAINO BOLLA
525100970507     C                   EXSR      CHABOL
525200950109     C*
525300970828     C                   MOVEL     BLPZNC        BRVZNC
525400950118     C                   MOVEL     BLTCAN        BRVCAN
525500970828     C                   MOVEL     BLTVUC        BRVVUC
525600990907     C                   MOVEL     BLTPUC        BRVPUC
525700950109     C*
525800960209     C* CONTROLLO SE ESISTE SPUNTA ENTRATA
525900170613    1C***44BLTDSE        IFEQ      0
526000170613    1C     BLTDSE        IFEQ      0
526100950109     C                   SETON                                        33
526200950109     C                   ELSE
526300950109     C                   SETOFF                                       33
526400950113    1C                   END
526500950109     C*
526600170613     C** SE NON PREVISTA GENERAZ.DA SPU.ENTRATA--> SEMPRE 88 O 90
526700170613     C**N44              SETON                                        33
526800950109     C*
526900950109     C*     ESISTE SPUNTA ENTRATA --> STESSO NUMERO PISTOLA
527000950113    1C     *IN33         IFEQ      '0'
527100950109     C                   MOVEL     BLTNPE        BRVNPS
527200950113   X1C                   ELSE
527300950109     C*
527400950109     C* NON ESISTE SPUNTA ENTRATA --> PISTOLA AUTOGENERATA 88 O 90
527500950109     C                   MOVEL     '88'          BRVNPS
527600130226     c* controllo se bolla con colli che partono da altra filiale
527700130226     c*  devo impostare pistola 90
527800130226     c                   if        blpxco='W'
527900130226     c                   exsr      Contr_XCO
528000130227     c
528100130227     c                   if        wtfp90='S'
528200130227     C                   MOVEL     90            BRVNPS
528300130226     c                   endif
528400130227     c                   endif
528500970507     C*
528600970423    2C     §1BFG5        IFEQ      'N'
528700970507     C     WLOCAL        OREQ      'S'
528800970507     C                   MOVEL     90            BRVNPS
528900970507    2C                   ENDIF
529000970507     C*
529100970423    1C                   END
529200950109     C*
529300950109     C                   ENDSR
529400970507     C*
529500130226     C* Controllo se collo partito da altro TFP ----------------------*
529600130226     C     Contr_XCO     BEGSR
529700130226     c
529800130227     C                   MOVEL     DSBRV         DSBRVS
529900130226     c                   eval      Wflsnrs=(blpfls*100)+bltnrs
530000130226
530100130226    1c                   if        wflsnrs<>savfnrs
530200130226     c                   clear                   xcotfp
530300130226     C                   MOVEL     'XCO'         kcod1
530400130226     C                   eval      kke1='ALTRITFP'
530500130226     C                   eval      kke2=%editc(wflsnrs:'X')
530600130226     c     ktbe2         chain     tntbe01l
530700130226    2c                   if        %found(tntbe01l) AND tbeatb=' '
530800130226     C                   MOVEA     TBeUNI        xcotfp
530900130226    2c                   endif
531000130226     c
531100130226     c                   eval      savfnrs=wflsnrs
531200130226    1c                   endif
531300130226     c
531400130226     c                   z-add     5             wnpg
531500130226    1c                   do        2
531600140618     c     kblt_xco      setll     fnbrv05l
531700140618     c     kblt_xco      reade     fnbrv05l
531800130226     c
531900140618    2c                   dow       not %eof(fnbrv05l)
532000130226    3c                   if        brvfle<>fgvlnp
532100130226     c* Verifico se la filiale che spunta è prevista
532200130226     c                   movel     brvfgs        alfafgs           3
532300130226     c                   eval      Indx=%lookup(alfafgs:xcotfp)
532400130226     c* Trovata --> memerizzo per impsostare pistola 90
532500130226    4c                   if        Indx>0
532600130226     c                   eval      Wtfp90='S'
532700130226     c                   leave
532800130226    4c                   endif
532900130226    3c                   endif
533000130226     c
533100140618     c     kblt_xco      reade     fnbrv05l
533200130226    2c                   enddo
533300130226     c
533400130226    2c                   if        wtfp90='S'
533500130226     c                   leave
533600130226     c                   else
533700130226     c                   eval      wnpg=1
533800130226    2c                   endif
533900130226     c
534000130226    1c                   enddo
534100130226     c
534200130227     C                   MOVEL     DSBRVS        DSBRV
534300130226     C                   ENDSR
534400130226     C*
534500130226     C* CREO SPUNTA DA FNBTT -----------------------------------------*
534600130226     C     CRTBTT        BEGSR
534700950109     C                   MOVEL     BTTFLS        BRVLNP
534800950109     C                   MOVEL     BTTLNA        BRVLNA
534900950109     C                   MOVEL     BTTNRS        BRVNRS
535000950113     C                   MOVEL     BTTNSC        BRVNSC
535100970507     C* CHAINO BOLLA
535200970507     C                   EXSR      CHABOL
535300950109     C*
535400950109     C                   MOVEL     BTPZNC        BRVZNC
535500990906     C                   MOVEL     BTTVUC        BRVVUC
535600990907     C                   MOVEL     BTTPUC        BRVPUC
535700950109     C                   MOVEL     BTTCAN        BRVCAN
535800950109     C* SE E' PISTOLA AUTOGENERATA SENZA SPUNTA IN ENTRATA:
535900950109     C* --> PISTOLA 89
536000950109     C     BTTDET        IFEQ      0
536100950109     C                   MOVEL     '89'          BRVNPS
536200950109     C                   ELSE
536300960209     C* GENERAZIONE SPUNTE 88
536400170613     C*  06/2017 SEMPRE spunta 88 non esiste più
536500170613     C**     *IN44         IFEQ      *OFF
536600170613     C**                   MOVEL     '88'          BRVNPS
536700170613     C**                   ELSE
536800960209     C* GENERAZIONE SPUNTE DA SPU.ENTRATA
536900950109     C                   MOVEL     BTTPET        BRVNPS
537000170613     C**                   ENDIF
537100950109     C                   ENDIF
537200950109     C*
537300950109     C                   ENDSR
537400950109     C*
537500950109     C* CREO SPUNTA DA ANOMALIE --------------------------------------*
537600950113     C     CRTSAN        BEGSR
537700950109     C                   MOVEL     ANMFLS        BRVLNP
537800950109     C                   MOVEL     ANMLNA        BRVLNA
537900950109     C                   MOVEL     ANMNRS        BRVNRS
538000950109     C                   MOVEL     ANMSCN        BRVNSC
538100950109     C                   MOVEL     WRVZNC        BRVZNC
538200970828     C                   MOVEL     WRVVUC        BRVVUC
538300990907     C                   MOVEL     WRVPUC        BRVPUC
538400950109     C                   MOVEL     WRVCAN        BRVCAN
538500960209     C* GENERAZIONE SPUNTE 88
538600170613     C*  06/2017 SEMPRE sopunta 88 non esiste più
538700170613     C**     *IN44         IFEQ      *OFF
538800170613     C**                   MOVEL     '88'          BRVNPS
538900170613     C**                   ELSE
539000960209     C* GENERAZIONE DA SPUNTE ENTRATA
539100950109     C                   MOVEL     ANMNPS        BRVNPS
539200170613     C**                   ENDIF
539300950109     C*
539400950109     C                   ENDSR
539500950915     C*
539600950915     C* CREO SPUNTA DA SPUNTE ENTRATA --------------------------------*
539700950915     C     CRTSPE        BEGSR
539800950915     C                   MOVEL     KLNP          BRVLNP
539900950915     C                   MOVEL     KLNA          BRVLNA
540000950915     C                   MOVEL     KNRS          BRVNRS
540100950915     C                   MOVEL     KNSC          BRVNSC
540200950915     C                   MOVEL     WRVZNC        BRVZNC
540300950915     C                   MOVEL     WRVVUC        BRVVUC
540400990907     C                   MOVEL     WRVPUC        BRVPUC
540500950915     C                   MOVEL     WRVCAN        BRVCAN
540600950915     C                   MOVEL     WRVNPS        BRVNPS
540700950915     C*
540800950915     C                   ENDSR
540900960531     C*
541000960705     C* CHIUDO ANMOALIE DI DISGUIDO SE ESISTE BOLLA PARTENZA ---------*
541100020412     C     SRDISG        BEGSR
541200070918     C**** kblt27        setll     FNBLT27L                               36
541300070918    2C**** *in36         IFEQ      *ON
541400070918     c
541500070918     C     kblt27        chain(N)  FNBLT27L
541600070918     c                   if        %found(fnblt27l)
541700070918     c* Se bolla trovata e del terminal di partenza
541800070918     c*  chiudo le anomalie 200 55 56
541900070918     c     bltlnp        lookup    l1                                     36
542000070918     c                   if        *in36
542100020412     C**TROVATA BOLLA PARTENZA: CHIUDO ANOMALIE 200 55 e 56
542200020412     C                   MOVE      'N'           WCRT              1
542300020412     C                   CLEAR                   DSLR33
542400020412     C                   MOVE      ANMDAO        D33DCH
542500020412     C                   MOVEL     'AN'          D33CCH
542600020412     C                   MOVE      ANMNR0        D33NRR
542700020412     C                   CALL      'FNLR33R'
542800020412     C                   PARM                    DSLR33
542900970828     C**CHIUDO ANCHE ANOMALIA 55
543000020412     C                   Z-ADD     55            COMCAA
543100020412     C     kanm31        CHAIN(N)  FNANM                              21
543200020412     C     *in21         IFEQ      *OFF
543300020412     C                   CLEAR                   DSLR33
543400020412     C                   MOVE      ANMDAO        D33DCH
543500020412     C                   MOVEL     'M'           D33FSC
543600020412     C                   MOVEL     'AN'          D33CCH
543700020412     C                   MOVEL     'S'           D33MAC
543800020412     C                   MOVE      ANMNR2        D33NRR
543900020412     C                   CALL      'FNLR33R'
544000020412     C                   PARM                    DSLR33
544100020412     C                   END
544200020412     C**CHIUDO ANCHE ANOMALIA 56
544300020412     C                   Z-ADD     56            COMCAA
544400020412     C     kanm31        CHAIN(N)  FNANM                              21
544500020412     C     *in21         IFEQ      *OFF
544600020412     C                   CLEAR                   DSLR33
544700020412     C                   MOVE      ANMDAO        D33DCH
544800020412     C                   MOVEL     'M'           D33FSC
544900020412     C                   MOVEL     'AN'          D33CCH
545000020412     C                   MOVEL     'S'           D33MAC
545100020412     C                   MOVE      ANMNR2        D33NRR
545200020412     C                   CALL      'FNLR33R'
545300020412     C                   PARM                    DSLR33
545400020412     C                   END
545500020412     C                   Z-ADD     200           COMCAA
545600020412    2C                   END
545700070918    2C                   END
545800070918     c
545900020412     C                   ENDSR
546000020411     C*--- RICERCO LNP BOLLA SE COLLO con serie > 0  -----------------*
546100960531     C     RICLNP        BEGSR
546200960531     C*
546300960611     C                   CLEAR                   WOKBL
546400960611     C                   CLEAR                   WOKBT
546500960603     C                   MOVE      BRVLNP        WLNP
546600970811     C                   CLEAR                   WTAB5F
546700960531     C*
546800020411    1C     BRVNRS        IFGT      *ZEROS
546900150610     c     w_FilBsp      oreq      'S'
547000020412     c* CERCO LA BOLLA
547100020412    2C                   SELECT
547200020411     C     WALCF         WHENEQ    'N'
547300020411     C     KBRV4         CHAIN(N)  FNBLT27L                           36
547400020411     c     *in36         ifeq      *off
547500020411     C                   MOVE      BLTLNP        WLNP
547600020411     C                   MOVE      'S'           WOKBL             1
547700020411     C                   CLEAR                   WTAB5F
547800020411     c                   else
547900020606     C     KBtt37        CHAIN(N)  FNBTT37L                           36
548000020411     c     *in36         ifeq      *off
548100020411     C                   MOVE      BTTLNP        WLNP
548200020411     C                   MOVE      'S'           WOKBT             1
548300020411     C                   CLEAR                   WTAB5F
548400020411     c                   endif
548500020411     c                   endif
548600020411     C     WALCF         WHENEQ    'S'
548700020411     C     KBRV4         CHAIN     FNBLT27L                           36
548800020411     c     *in36         ifeq      *off
548900020411     C                   MOVE      BLTLNP        WLNP
549000020411     C                   MOVE      'S'           WOKBL             1
549100020411     C                   CLEAR                   WTAB5F
549200020411     c                   else
549300020606     C     KBtt37        CHAIN     FNBTT37L                           36
549400020411     c     *in36         ifeq      *off
549500020411     C                   MOVE      BTTLNP        WLNP
549600020411     C                   MOVE      'S'           WOKBT             1
549700020411     C                   CLEAR                   WTAB5F
549800020411     c                   endif
549900020411     c                   endif
550000020412    2C                   ENDSL
550100020412     c* Se non trovata bolla part. nè bolla transito GUARDO SE ESISTE LA
550200020412     c* BOLLA ARRIVI (in un ottica di As unico infatti se la spunta letta
550300020411     c* è un disguido potrei trovare la bolla in arrivo se sull'As in cui
550400020411     c* il collo è stato disguidato risiedono anche i dati del p.o. di
550500020411     c* arrivo del collo)
550600020412    2c     *in36         ifeq      *on
550700020411     c     kbrv4         chain     fnart27l                           36
550800020412    3c     *in36         ifeq      *off
550900020411     C                   MOVE      artlnp        WLNP
551000020411     C                   CLEAR                   WTAB5F
551100020412   x3c                   else
551200020411     c* BOLLA NON TROVATA IN NESSUN ARCHIVIO:
551300020412     c* SPUNTA PARTENZA NON DI DISGUIDO: verifico se è presente
551400020411     c                   exsr      cntspa
551500020412    4c     w105          ifeq      *blanks
551600020412     c* VADO SU TABELLA 5F se non ho trovato nemmeno la spunta partenza
551700020412     C                   movel     fgvlnp        W008A             8
551800020412     C                   move      brvnrs        W005A             5
551900020412     C                   movel     brvlnp        w005a
552000020412     C                   move      w005a         w008a
552100070215     C                   z-add     1             ii                4 0
552200020412     C     w008a         lookup    s5f(ii)                                31
552300020412     C**
552400020412     C* SE NON TROVATO CERCO COME SERIE = 99
552500020412    5C     *in31         ifeq      *off
552600020412     C                   move      99            W008A
552700020412     C                   z-add     1             ii
552800020412     C     w008a         lookup    s5f(ii)                                31
552900020412    5C                   endif
553000020412     C**
553100020412    5C     *in31         ifeq      *on
553200020412     C                   MOVEL     'S'           WTAB5F            1
553300020412     c     ii            occur     dse5f
553400020412     c                   move      e5f(1)        wlnp
553500020412    5c                   endif
553600020411     c*
553700020412    4c                   endif
553800020412    3c                   endif
553900020412    2c                   endif
554000020412    1c                   endif
554100971028     C                   ENDSR
554200971028     C*
554300970827     C*--- CONTROLLO SE C'E' UNA SPUNTA PARTENZA ---------------------*
554400970827     C     CNTSPA        BEGSR
554500970827     C                   MOVE      *BLANKS       W105              1
554600070123     C* Mi salvo dati record di fnbrv prima di leggere
554700070123     C                   MOVEL     DSBRV         DSBRVS
554800140618     C     KBRV03        SETLL     FNBRV05L
554900140618     C     KBRV03        READE(N)  FNBRV05L                               21
555000971028    1C     *IN21         DOWEQ     *OFF
555100960830     C     W105          ANDEQ     *BLANKS
555200971105    2C     BRVFGS        IFNE      FGVLNP
555300970827     C     BRVATR        ANDEQ     ' '
555400960830     C                   MOVE      '1'           W105
555500970827     C                   MOVEL     BRVFGS        WLNP
555600970827     C                   CLEAR                   WTAB5F
555700140618     C     KBRV03        READE(N)  FNBRV05L                               21
555800971028   X2C                   ELSE
555900140618     C     KBRV03        READE(N)  FNBRV05L                               21
556000971028    2C                   ENDIF
556100971028    1C                   ENDDO
556200070123     C* Reimposto campi di fnbrv con dati salvati
556300960830     C                   MOVEL     DSBRVS        DSBRV
556400960830     C                   ENDSR
556500970423     C*
556600130527     C* RICHIAMO FNLS45R PER AGGIORNAMENTO DATE SU BOLLE ------------*
556700970423     C     CALL44        BEGSR
556800070221     c                   clear                   fnls45ds
556900070221     c                   movel     'S'           dls45ric
557000070221     c                   move      brvpes        dls45pes
557100070221     c                   call      'FNLS45R'
557200070221     c                   parm                    fnls45ds
557300070222     c                   parm                    dsbrv
557400970423     C*
557500970509     C                   MOVEL     'S'           WFC44             1
557600970423     C                   ENDSR
557700970423     C*
557800970423     C* IMPOSTO LA KEY PER CHAIN -------------------------------------*
557900970423     C     IMPKEY        BEGSR
558000970423     C                   SELECT
558100970423     C     *IN40         WHENEQ    *ON
558200970423     C                   MOVEL     BLTFLS        KLNP
558300970423     C                   MOVEL     BLTLNA        KLNA
558400970423     C                   MOVEL     BLTNRS        KNRS
558500970423     C                   MOVEL     BLTNSC        KNSC
558600970423     C*
558700970423     C     *IN42         WHENEQ    *ON
558800970423     C                   MOVEL     ANMFLS        KLNP
558900970423     C                   MOVEL     ANMLNA        KLNA
559000970423     C                   MOVEL     ANMNRS        KNRS
559100970423     C                   MOVEL     ANMSCN        KNSC
559200970423     C*
559300970423     C     *IN41         WHENEQ    *ON
559400970423     C                   MOVEL     BTTFLS        KLNP
559500970423     C                   MOVEL     BTTLNA        KLNA
559600970423     C                   MOVEL     BTTNRS        KNRS
559700970423     C                   MOVEL     BTTNSC        KNSC
559800990917     C                   OTHER
559900970423     C                   MOVEL     BRVLNP        KLNP
560000970423     C                   MOVEL     BRVLNA        KLNA
560100970423     C                   MOVEL     BRVNRS        KNRS
560200970423     C                   MOVEL     BRVNSC        KNSC
560300970423     C                   ENDSL
560400970423     C                   ENDSR
560500010309     C*
560600010309     C* Aggiorno e sflaggo file O.R.M. -------------------------------*
560700010309     C     AGGORM        BEGSR
560800010309     C                   MOVEL     'M'           WTRC4
560900060220     C     KAR4          CHAIN     FIAR401L                           20
561000051207    1C     *IN20         IFEQ      *OFF
561100060220     C                   MOVEL     AR4NOT        DSBL4M
561200010309     C     KORM          CHAIN     FNORM01L                           20
561300060220     c* Se il p.o. di ritiro non è uguale alla linea di partenza,
561400051207     c*  aggiorno in questo momento
561500060220    2c                   if        not *in20 and ormpor<>AR4lnp
561600051207     c                   exsr      sr_Storico
561700051207     c* Scrivo fase 200
561800051207     c                   z-add     200           ormfao
561900051207     C                   Z-ADD     FGVDFV        ORMDFO
562000051207     C                   Z-ADD     235858        ORMOFO
562100051207     c                   exsr      rieorf
562200051207     c                   write     fnorf000
562300051207     c
562400060220     c                   z-add     AR4lnp        ormpor
562500070711      * se ho aggiornato il p.o. ritiro dell'ORM devo aggiornare anche FNORG
562600070711     c     korm          chain     fnorg01l
562700070711if  1c                   if        %found(fnorg01l)
562800070711      * cambio il p.o. ritiro
562900070711     c                   eval      orgpor = ormpor
563000070711     c                   update    fnorg000
563100070711    2c                   endif
563200051207    2c                   endif
563300051207     c
563400051207    2c     *IN20         IFEQ      *OFF
563500010312     c     ormfao        andne     900
563600010309     c                   z-add     900           ormfao
563700010309     C                   Z-ADD     FGVDFV        ORMDFO
563800010405     C                   Z-ADD     235959        ORMOFO
563900040909     C                   z-add     dateu         ORMDTT
564000010309     c                   exsr      rieorf
564100070711      * imposto ormeti
564200070711     c                   clear                   dfar
564300070711     c                   clear                   tibs02ds
564400070711     c                   eval      t02mod = 'C'
564500070711     c                   eval      t02sif = knsif
564600070711     c                   eval      t02cod = 'FAR'
564700070711     c                   movel(p)  ormfao        t02ke1
564800070711     c                   call      'TIBS02R'
564900070711     c                   parm                    kpjba
565000070711     c                   parm                    tibs02ds
565100070711     c                   eval      dfar = t02uni
565200070711     c                   eval      ormeti = d§farass
565300010309     c                   write     fnorf000
565400051207    2C                   ENDIF
565500051207    c
565600051207     C  n20              UPDATE    FNORM000
565700051207    1C                   ENDIF
565800010309     C                   ENDSR
565900010309     C*
566000010309     C* Riempio campi record fase O.R.M. -------------------------------*
566100010309     C     RIEORF        BEGSR
566200010309     C                   clear                   fnorf000
566300010309     C                   eval      ORFpoe = ormpoe
566400010309     C                   eval      ORFnsr = ormnsr
566500010309     C                   eval      ORFnor = ormnor
566600010309     C                   eval      ORFnrv = ormnrv
566700010309     C                   eval      ORFpog = ormpor
566800010309     C                   eval      ORFdae = ormdfo
566900010309     C                   eval      ORFore = ormofo
567000010309     C                   eval      ORFfar = ormfao
567100040909     C                   eval      ORFpue = knmus
567200040909     c                   If        OrmNdc > *Zeros
567300060516      * devo trovare il p.o. gestione distinta giusto
567400120607     c                   move      ormnpg        kdstnpg
567500120607     c                   move      ormndc        kdstnfv
567600060516      * prima provo con il p.o. ritiro
567700060516     c                   eval      kdstfgs = ormpor
567800120607     c     kdst          chain     fidst01l
567900060516      * se non lo trovo cerco se il p.o. ritiro è gestito
568000120607     c                   if        not %found(fidst01l)
568100060516     c                   clear                   dslv55
568200060516     c                   eval      d55tpt = '6'
568300060516     c                   move      ormpor        d55lin
568400060516     c                   eval      d55drf = ormddc
568500060516     c                   call      'FNLV55R'
568600060516     c                   parm                    dslv55
568700060516     c                   if        d55err = *blanks and d55tfa <> d55lin
568800060516     c                   move      d55tfa        kdstfgs
568900120607     c     kdst          chain     fidst01l
569000060516      * se non trovo neanche con il p.o. che gestisce metto il p.o. ritiro
569100060516      * teoricamente non dovrebbe succedere
569200120607     c                   if        not %found(fidst01l)
569300060516     c                   eval      kdstfgs = ormpor
569400060516     c                   endif
569500060516     c                   endif
569600060516     c                   endif
569700060516     c**!!!              Eval      OrfFgs = OrmPor
569800060516     c                   eval      orffgs = kdstfgs
569900040909     c                   Eval      OrfNdc = OrmNdc
570000040909     c                   Eval      OrfDdc = OrmDdc
570100040909     c                   EndIf
570200010309
570300010309     c                   ENDSR
570400051207     C*
570500051207     C* Scrivo lo storico orm ------------------------------------------*
570600051207     C     SR_Storico    BEGSR
570700051207     C                   clear                   fnorv000
570800051207     C                   eval      ORVpoe = ORMpoe
570900051207     C                   eval      ORVnsr = ORMnsr
571000051207     C                   eval      ORVnor = ORMnor
571100051207     C                   eval      ORVnrv = ORMnrv
571200051207     C                   eval      ORVtor = ORMtor
571300051207     C                   eval      ORVcor = ORMcor
571400051207     C                   eval      ORVrso = ORMrso
571500051207     C                   eval      ORVino = ORMino
571600051207     C                   eval      ORVcao = ORMcao
571700051207     C                   eval      ORVloo = ORMloo
571800051207     C                   eval      ORVpro = ORMpro
571900051207     C                   eval      ORVnao = ORMnao
572000051207     C                   eval      ORVcra = ORMcra
572100051207     C                   eval      ORVrsr = ORMrsr
572200051207     C                   eval      ORVinr = ORMinr
572300051207     C                   eval      ORVcar = ORMcar
572400051207     C                   eval      ORVlor = ORMlor
572500051207     C                   eval      ORVprr = ORMprr
572600051207     C                   eval      ORVnar = ORMnar
572700051207     C                   eval      ORVrer = ORMrer
572800051207     C                   eval      ORVter = ORMter
572900051207     C                   eval      ORVcrc = ORMcrc
573000051207     C                   eval      ORVrsc = ORMrsc
573100051207     C                   eval      ORVinc = ORMinc
573200051207     C                   eval      ORVloc = ORMloc
573300051207     C                   eval      ORVcac = ORMcac
573400051207     C                   eval      ORVprc = ORMprc
573500051207     C                   eval      ORVnac = ORMnac
573600051207     C                   eval      ORVffd = ORMffd
573700051207     C                   eval      ORVdar = ORMdar
573800051207     C                   eval      ORVorr = ORMorr
573900051207     C                   eval      ORVnam = ORMnam
574000051207     C                   eval      ORVncl = ORMncl
574100051207     C                   eval      ORVpkg = ORMpkg
574200051207     C                   eval      ORVvlm = ORMvlm
574300051207     C                   eval      ORVbnc = ORMbnc
574400051207     C                   eval      ORVblc = ORMblc
574500051207     C                   eval      ORVatt = ORMatt
574600051207     C                   eval      ORVmtc = ORMmtc
574700051207     C                   eval      ORVpag = ORMpag
574800051207     C                   eval      ORVksc = ORMksc
574900051207     C                   eval      ORVctr = ORMctr
575000051207     C                   eval      ORVddt = ORMddt
575100051207     C                   eval      ORVpor = ORMpor
575200051207     C                   eval      ORVzor = ORMzor
575300051207     C                   eval      ORVpoc = ORMpoc
575400051207     C                   eval      ORVno1 = ORMno1
575500051207     C                   eval      ORVno2 = ORMno2
575600051207     C                   eval      ORVcst = ORMcst
575700051207     C                   eval      ORVvcs = ORMvcs
575800051207     C                   eval      ORVfcs = ORMfcs
575900051207     C                   eval      ORVdfo = ORMdfo
576000051207     C                   eval      ORVofo = ORMofo
576100051207     C                   eval      ORVfao = ORMfao
576200051207     C                   eval      ORVsto = ORMsto
576300051207     C                   eval      ORVdst = ORMdst
576400051207     C                   eval      ORVnpg = ORMnpg
576500051207     C                   eval      ORVndc = ORMndc
576600051207     C                   eval      ORVddc = ORMddc
576700051207     C                   eval      ORVstp = ORMstp
576800051207     C                   eval      ORVrfa = ORMrfa
576900051207     C                   eval      ORVsto = ORMsto
577000070711     c                   eval      orveti = ormeti
577100051207     C                   eval      ORVspi = ORMspi
577200051207     C                   eval      ORVflo = ORMflo
577300051207     C                   movel     knmus         ORVutv
577400051207     C                   TIME                    W0140            14 0
577500051207     C                   MOVEL     W0140         orvorv
577600051207     C                   CLEAR                   WLBDAT
577700051207     C                   MOVE      W0140         G02DAT
577800051207     C                   CALL      'XSRDA8'
577900051207     C                   PARM                    WLBDAT
578000051207     C                   Z-ADD     G02INV        orvdtv
578100051207     C                   Z-ADD     G02INV        orvdtt
578200051207     C                   write     fnorv000
578300051207
578400051207     c                   ENDSR
578500950109     C*---------------------------------------------------------------*
578600950109     C*    OPERAZIONI INIZIALI                                        *
578700950109     C*---------------------------------------------------------------*
578800950109     C     *INZSR        BEGSR
578900950109     C*
579000950109     C     *ENTRY        PLIST
579100950109     C                   PARM                    KPJBA
579200130524     C                   MOVEL     KPJBU         FNLSU2DS
579300021014
579400021014     c                   Movel     Simfel        Varx1
579500021014     c                   Movel     Simfel        Var2x1
579600021014     c                   Movel     Simfel        Num31
579700021015     c                   Move      Simfel        Mbrfil4
579800021014
579900000111     C* SPOSTO SUBITO NUMERI FOGLIO IN CAMPI LUNGHI 6,0
580000000111     C                   Z-ADD     FGV2          FGV260
580100000111     C                   Z-ADD     NFV1          NFV160
580200950109     C*
580300950109     C                   Z-ADD     1             CODUT             1 0
580400170613     c     *dtaara       define    §azute        azuteds
580500170613     c     *dtaara       define    §datiute      ddatiute
580600170613      *
580700170613     c                   in(E)     *dtaara
580800170613     c                   IF        %ERROR or RSUT = *blanks
580900170613     c                   clear                   Tibs34Ds
581000170613     c                   call      'TIBS34R'
581100170613     c                   parm                    Tibs34Ds
581200170613     c                   in        *dtaara
581300170613     c                   ENDIF
581400170613      *
581500000428     C** S.I. IN CUI SONO
581600021010     C                   CLEAR                   DSBS55
581700000428     C                   MOVEL     KNSIF         I50PSI
581800000428     C                   MOVEL     'L'           I50TLA
581900021010     C                   CALL      'TIBS55R'
582000021010     C                   PARM                    DSBS55
582100950109     C**
582200950109     C*    CHIAVI                                                     *
582300950109     C**
582400950109     C     KBLP2         KLIST
582500950109     C                   KFLD                    BLPAAS
582600950109     C                   KFLD                    BLPLNP
582700950109     C                   KFLD                    BLPNRS
582800950109     C                   KFLD                    BLPNSP
582900060220     C     KAR4          KLIST
583000010309     C                   KFLD                    BLPAAS
583100010309     C                   KFLD                    BLPLNP
583200010309     C                   KFLD                    BLPNRS
583300010309     C                   KFLD                    BLPNSP
583400010309     C                   KFLD                    WTRC4
583500950516     C     KBTP2         KLIST
583600020606     c                   kfld                    btpflp
583700950516     C                   KFLD                    BTPAAS
583800950516     C                   KFLD                    BTPLNP
583900950516     C                   KFLD                    BTPNRS
584000950516     C                   KFLD                    BTPNSP
584100950109     C     KBLT3         KLIST
584200950109     C                   KFLD                    BLTAAS
584300950109     C                   KFLD                    BLTLNP
584400950109     C                   KFLD                    BLTNRS
584500950109     C                   KFLD                    BLTNSP
584600950109     C     KBTT3         KLIST
584700020605     c                   kfld                    fgvlnp
584800950109     C                   KFLD                    BTTAAS
584900950109     C                   KFLD                    BTTLNP
585000950109     C                   KFLD                    BTTNRS
585100950109     C                   KFLD                    BTTNSP
585200950109     C     KTAB          KLIST
585300950109     C                   KFLD                    CODUT
585400950109     C                   KFLD                    COD               2
585500950109     C                   KFLD                    KEY               8
585600950109     C     KTAB2         KLIST
585700950109     C                   KFLD                    CODUT
585800950109     C                   KFLD                    COD
585900950109     C     KBLT2         KLIST
586000950109     C                   KFLD                    WNPG
586100950109     C                   KFLD                    BLTFLS
586200950109     C                   KFLD                    BLTLNA
586300950109     C                   KFLD                    BLTNRS
586400950113     C                   KFLD                    BLTNSC
586500971107     C                   KFLD                    FGVLNP
586600130226     C     KBLT_xco      KLIST
586700130226     C                   KFLD                    WNPG
586800130226     C                   KFLD                    BLTFLS
586900130226     C                   KFLD                    BLTLNA
587000130226     C                   KFLD                    BLTNRS
587100130226     C                   KFLD                    BLTNSC
587200950109     C     KBTT          KLIST
587300950109     C                   KFLD                    WNPG
587400950109     C                   KFLD                    BTTFLS
587500950109     C                   KFLD                    BTTLNA
587600950109     C                   KFLD                    BTTNRS
587700950113     C                   KFLD                    BTTNSC
587800950915     C     KFVV          KLIST
587900950915     C                   KFLD                    WNPG5
588000950915     C                   KFLD                    WDFV
588100020916     C                   KFLD                    Kfgs
588200000728     C     KFVV1         KLIST
588300000728     C                   KFLD                    BRVNPG
588400001123     C                   KFLD                    WNFV05
588500000728     C                   KFLD                    BRVFGS
588600950109     C     KBRC2         KLIST
588700950109     C                   KFLD                    WNPG
588800000111     C                   KFLD                    FGV260
588900971107     C                   KFLD                    FGVLNP
589000950109     C                   KFLD                    BRVLNA
589100130527     C     K03BLP77      klist
589200020807     C                   kfld                    SIMfel
589300960212     C                   KFLD                    FGV2
589400960212     C                   KFLD                    BRVLNA
589500130531     C     Kblp77        KLIST
589600130531     C                   KFLD                    simfel
589700130531     C                   KFLD                    kfgv
589800130531     C                   KFLD                    savffv
589900130527     C     K02BLP77_9    KLIST
590000020807     C                   KFLD                    SIMfel
590100020807     C                   KFLD                    COMnfv
590200950109     C     KFVD2         KLIST
590300950109     C                   KFLD                    FGVLNP
590400950109     C                   KFLD                    FGV2
590500950109     C                   KFLD                    BRVLNA
590600950110     C                   KFLD                    WTRC
590700130530     C     KFVDbor       KLIST
590800130530     C                   KFLD                    SIMFEL
590900130530     C                   KFLD                    WNFV5
591000130530     C                   KFLD                    BLPLNA
591100130530     C                   KFLD                    WTRC
591200060707     C     KFV4e         KLIST
591300060707     C                   KFLD                    FV4LNP
591400060707     C                   KFLD                    FV4NFV
591500060707     C                   KFLD                    FV4EPA
591600060707     C                   KFLD                    FV4LAI
591700060707     C                   KFLD                    FV4TRC
591800011127     C*
591900011127     C     KTBE          KLIST
592000011127     C                   KFLD                    KCOD1
592100011127     C                   KFLD                    KKE1
592200130226     C     KTBE2         KLIST
592300130226     C                   KFLD                    KCOD1
592400130226     C                   KFLD                    KKE1
592500130226     C                   KFLD                    KKE2
592600130226     c
592700011127     C     *LIKE         DEFINE    TBECOD        KCOD1
592800011127     C     *LIKE         DEFINE    TBEKE1        KKE1
592900130226     C     *LIKE         DEFINE    TBEKE2        KKE2
593000130226     c
593100950109     C     KFTT          KLIST
593200950109     C                   KFLD                    WPDR
593300950109     C                   KFLD                    COMTSR
593400950109     C                   KFLD                    WNDC
593500950113     C                   KFLD                    WDRT
593600950109     C     KFTD          KLIST
593700950109     C                   KFLD                    COMPDR
593800950109     C                   KFLD                    COMTSR
593900950109     C                   KFLD                    COMNDC
594000950114     C                   KFLD                    COMDDC
594100950109     C                   KFLD                    CO2KSC
594200950113     C                   KFLD                    COMRSC
594300020416     C     KFTD1         KLIST
594400020416     C                   KFLD                    COMPDR
594500020416     C                   KFLD                    COMTSR
594600020416     C                   KFLD                    COMNDC
594700020416     C                   KFLD                    COMDDC
594800950109     C     KBLT          KLIST
594900021021     C                   KFLD                    wlnp              3 0
595000021021     C                   KFLD                    DFV               8 0
595100021021     C                   KFLD                    COMFIL            3 0
595200000731     C     KBLT1         KLIST
595300000731     C                   KFLD                    BLTAAS
595400000731     C                   KFLD                    BLTLNP
595500000731     C                   KFLD                    BLTNRS
595600000731     C                   KFLD                    BLTNSP
595700000731     C                   KFLD                    BLTNSC
595800000714     C     KBLTL         KLIST
595900000714     C                   KFLD                    DFV
596000000714     C                   KFLD                    COMFIL
596100000714     C                   KFLD                    BLTLNP
596200960704     C     KBLT27        KLIST
596300960704     C                   KFLD                    ANMFLS
596400960704     C                   KFLD                    ANMLNA
596500960704     C                   KFLD                    ANMNRS
596600960704     C                   KFLD                    ANMSCN
596700950109     C     KANM          KLIST
596800021021     C                   KFLD                    wlnp
596900021021     C                   KFLD                    COMCAA
597000950109     C                   KFLD                    COMFIL
597100040422     C     KANM02        KLIST
597200040422     C                   KFLD                    bttFLS
597300040422     C                   KFLD                    bttLNA
597400040422     C                   KFLD                    bttNRS
597500040422     C                   KFLD                    bttnsc
597600040422     C                   KFLD                    COMCAA            3 0
597700950418     C     KANM3         KLIST
597800950418     C                   KFLD                    BRVLNP
597900950418     C                   KFLD                    BRVLNA
598000950418     C                   KFLD                    BRVNRS
598100950418     C                   KFLD                    BRVNSC
598200950418     C                   KFLD                    COMCAA            3 0
598300020412     C     KANM31        KLIST
598400020412     C                   KFLD                    ANMFLS
598500020412     C                   KFLD                    ANMLNA
598600020412     C                   KFLD                    ANMNRS
598700020412     C                   KFLD                    ANMSCN
598800020412     C                   KFLD                    COMCAA            3 0
598900130527     C     KANM1         KLIST
599000130527     C                   KFLD                    BLTFLS
599100130527     C                   KFLD                    BLTLNA
599200130527     C                   KFLD                    BLTNRS
599300130527     C                   KFLD                    BLTNSC
599400130607     C                   KFLD                    comcaa
599500151118     C     KANM01        KLIST
599600151118     C                   KFLD                    BLpaas
599700151118     C                   KFLD                    BLplnp
599800151118     C                   KFLD                    BLpNRS
599900151118     C                   KFLD                    BLpNSp
600000151118     C                   KFLD                    comcaa
600100950109     C     KANM5         KLIST
600200950109     C                   KFLD                    ANMFLS
600300950109     C                   KFLD                    ANMLNA
600400950109     C                   KFLD                    ANMNRS
600500950109     C                   KFLD                    ANMSCN
600600971107     C                   KFLD                    FGVLNP
600700950109     C     KBRV          KLIST
600800950109     C                   KFLD                    WNPG
600900000111     C                   KFLD                    FGV260
601000971107     C                   KFLD                    FGVLNP
601100130527     C     KBRV1         KLIST
601200130527     C                   KFLD                    KNPG
601300130527     C                   KFLD                    BLTFLS
601400130527     C                   KFLD                    BLTLNA
601500130527     C                   KFLD                    BLTNRS
601600130527     C                   KFLD                    BLTNSC
601700130527     C                   KFLD                    anmfle
601800130530     C     KBRVbor       KLIST
601900130530     C                   KFLD                    KNPG
602000130530     C                   KFLD                    WNFV60
602100130530     C                   KFLD                    KFGS
602200130530     C                   KFLD                    BLPFLS
602300130530     C                   KFLD                    blplna
602400130530     C                   KFLD                    BLPNRS
602500130530     C                   KFLD                    BLTNSC
602600950418     C     KBRV2         KLIST
602700950418     C                   KFLD                    WNPG
602800950418     C                   KFLD                    WSANFV
602900950418     C                   KFLD                    WSAFGS
603000950418     C                   KFLD                    KLNA
603100950418     C                   KFLD                    KLNP
603200950418     C                   KFLD                    KNRS
603300950418     C                   KFLD                    KNSC
603400980209     C     KBRV01        KLIST
603500980209     C                   KFLD                    NPG1
603600000111     C                   KFLD                    NFV160
603700980209     C                   KFLD                    FGS1
603800980209     C                   KFLD                    KLNP
603900980209     C                   KFLD                    KLNA
604000980209     C                   KFLD                    KNRS
604100980209     C                   KFLD                    KNSC
604200960830     C     KBRV03        KLIST
604300960830     C                   KFLD                    WNPG
604400960830     C                   KFLD                    BRVLNP
604500960830     C                   KFLD                    BRVLNA
604600960830     C                   KFLD                    BRVNRS
604700960830     C                   KFLD                    BRVNSC
604800950109     C     KBRV3         KLIST
604900950109     C                   KFLD                    NPG1
605000000111     C                   KFLD                    NFV160
605100950109     C                   KFLD                    FGS1
605200950109     C                   KFLD                    COMFIL
605300950915     C     KBRV33        KLIST
605400970423     C                   KFLD                    KVVNPG
605500970423     C                   KFLD                    KVVNFV
605600970423     C                   KFLD                    KVVFGS
605700950915     C                   KFLD                    COMFIL
605800990917     C     KBRV32        KLIST
605900990917     C                   KFLD                    KVVNPG
606000990917     C                   KFLD                    KVVNFV
606100990917     C                   KFLD                    KVVFGS
606200950109     C     KBRV4         KLIST
606300950109     C                   KFLD                    KLNP
606400950109     C                   KFLD                    KLNA
606500950109     C                   KFLD                    KNRS
606600950109     C                   KFLD                    KNSC
606700950118     C     KBRV5         KLIST
606800950109     C                   KFLD                    WNPG
606900950109     C                   KFLD                    KLNP
607000950109     C                   KFLD                    KLNA
607100950109     C                   KFLD                    KNRS
607200950109     C                   KFLD                    KNSC
607300971107     C                   KFLD                    FGVLNP
607400000731     C     KBRV55        KLIST
607500000731     C                   KFLD                    WNPG
607600000731     C                   KFLD                    KLNP
607700000731     C                   KFLD                    KLNA
607800000731     C                   KFLD                    KNRS
607900000731     C                   KFLD                    KNSC
608000040802     C                   KFLD                    WFGSspu
608100950118     C     KBRV6         KLIST
608200950118     C                   KFLD                    KLNP
608300950118     C                   KFLD                    KLNA
608400950118     C                   KFLD                    KNRS
608500950118     C                   KFLD                    KNSC
608600971107     C                   KFLD                    FGVLNP
608700150603     C     KBRV7         KLIST
608800150603     C                   KFLD                    BLTFLS
608900150603     C                   KFLD                    BLTLNA
609000150603     C                   KFLD                    BLTNRS
609100150603     C                   KFLD                    BLTNSC
609200150924     C*****              KFLD                    FGVLNP
609300020807     c     K02FGV01      klist
609400020807     c                   kfld                    fgv2
609500020807     c                   kfld                    SIMfel
609600010309     c*
609700010309     c     korm          klist
609800010309     c                   kfld                    §b4poe
609900010309     c                   kfld                    §b4nsr
610000010309     c                   kfld                    §b4nor
610100010309     c                   kfld                    §b4nrv
610200010611     c     kbrve         klist
610300010611     c                   kfld                    brvnpg
610400010611     c                   kfld                    brvnfv
610500010611     c                   kfld                    brvfgs
610600010611     c                   kfld                    brvlnp
610700010611     c                   kfld                    brvlna
610800010611     c                   kfld                    brvnrs
610900010611     c                   kfld                    brvnsc
611000020605     C     KBtt38        KLIST
611100020605     c                   kfld                    fgvlnp
611200020605     C                   KFLD                    DFV               8 0
611300020605     C                   KFLD                    COMFIL            3 0
611400020606     C     KBtt37        KLIST
611500020606     c                   kfld                    fgvlnp
611600020606     C                   KFLD                    KLNP
611700020606     C                   KFLD                    KLNA
611800020606     C                   KFLD                    KNRS
611900020606     C                   KFLD                    KNSC
612000040319     C     KBTT1         KLIST
612100040319     c                   kfld                    BTTFLP
612200040319     C                   KFLD                    BTTAAS
612300040319     C                   KFLD                    BTTLNP
612400040319     C                   KFLD                    BTTNRS
612500040319     C                   KFLD                    BTTNSP
612600040319     C                   KFLD                    BTTNSC
612700040310     c     Kar5          klist
612800040311     c                   kfld                    bltaas
612900040311     C                   KFLD                    bltlnp
613000040311     C                   KFLD                    bltnrs
613100040311     C                   KFLD                    bltnsp
613200040310     C                   KFLD                    ktrd
613300040310     c                   movel     'GEN'         ktrd
613400060516
613500060516     c     kdst          klist
613600120607     c                   kfld                    kdstnpg
613700120607     c                   kfld                    kdstnfv
613800060516     c                   kfld                    kdstfgs
613900060516
614000950118     C***
614100950118     C*  CODICI TRATTAMENTO MERCE CON §1BFG5 = "N"
614200950118     C***
614300960202     C* NON CARICO SE RICHIAMATO DA MANUTENZIONE
614400960202     C     PA2FLG        IFNE      'M'
614500960202     C* APERTURA FILE
614600960202     C                   EXSR      OPENF
614700130530     c
614800960202     C**
614900950118     C                   Z-ADD     1             X
615000950118     C                   MOVEL     '1B'          COD
615100950118     C     KTAB2         CHAIN     TABEL00F                           31
615200950118     C*
615300950118     C     *IN31         DOWEQ     *OFF
615400950118     C                   MOVEL     TBLUNI        DS1B
615500950118     C     §1BFG5        IFEQ      'N'
615600950118     C                   MOVEL     TBLKEY        CTM(X)
615700950118     C                   ADD       1             X
615800950118     C                   ENDIF
615900950118     C     KTAB2         READE     TABEL00F                               31
616000950118     C                   ENDDO
616100050524     c
616200050524     c* Controllo riempimento schiera
616300050524     c                   clear                   trul0sds
616400050524     c                   eval      i0sski='CTM'
616500050524     c                   eval      i0sele=%elem(ctm)
616600050524     c                   eval      i0spie=x
616700050524     c                   eval      i0scod='  1B '
616800050524     c                   eval      i0sfile='TABEL00F  '
616900050524     c                   eval      i0ssif=knsif
617000130718     c                   eval      i0spgm='FNLSU2R'
617100050524     c                   movel     trul0sds      kpjbu
617200050524     c                   call      'TRUL0SR'
617300050524     c                   parm                    kpjba
617400960531     C***
617500960531     C** TABELLA 5F - FILIALI CONFERMA BOLLE DA DISK
617600960531     C***
617700960531     C                   Z-ADD     0             X
617800960531     C                   MOVEL     '5F'          COD
617900960531     C     KTAB2         SETLL     TABEL
618000960531     C     KTAB2         READE     TABEL                                  31
618100960531     C*
618200960531    1C     *IN31         DOWEQ     '0'
618300960531    2C     TBLFLG        IFEQ      ' '
618400050524     c* elaboro soltanto i record con le prima 3 cifre <>'   '
618500050524     c*  perchè nella 5F ci sono 2 diversi tipi record: quello con i '   '
618600050524     c*   davanti qui non sono usati
618700050524     c                   movel     tblkey        w003a             3
618800050524     c                   if        w003a<>'   '
618900960531     C                   ADD       1             X
619000960531     C                   MOVEL     TBLKEY        S5F(X)
619100960531     C     X             OCCUR     DSE5F
619200960701     C                   MOVEA     TBLUNI        E5F
619300080109      * - Carica comunque il n° max di elementi previsti nella schiera.
619400080109      *   Se superato il limite massimo di riempimento, verrà inviato
619500080109      *   un messaggio di avvertimento - vedi chiamata al pgm TRUL0SR:
619600080109      *   verificherà lui la percentuale di riempimento...).
619700080109     C     X             IFGE      $MaxElem
619800960531     C                   SETON                                        31
619900960531     C                   ENDIF
620000050524     C                   ENDIF
620100960531    2C                   ENDIF
620200960531     C*
620300960531     C  N31KTAB2         READE     TABEL                                  31
620400960531    1C                   ENDDO
620500050524     c**
620600050524     c                   clear                   trul0sds
620700050524     c                   eval      i0sski='S5F'
620800050524     c                   eval      i0sele=%elem(s5f)
620900050524     c                   eval      i0spie=x
621000050524     c                   eval      i0scod='  5F '
621100050524     c                   eval      i0sfile='TABEL00F  '
621200050524     c                   eval      i0ssif=knsif
621300130718     c                   eval      i0spgm='FNLSU2R'
621400050524     c                   movel     trul0sds      kpjbu
621500050524     c                   call      'TRUL0SR'
621600050524     c                   parm                    kpjba
621700950118     C***
621800950118     C*  CODICI PISTOLA CON PISTOLA AUTOMATICA
621900950118     C***
622000950118     C                   Z-ADD     1             X
622100970818     C                   Z-ADD     1             Y                 3 0
622200950118     C                   MOVEL     '7J'          COD
622300950118     C     KTAB2         CHAIN     TABEL00F                           31
622400950118     C*
622500950118     C     *IN31         DOWEQ     *OFF
622600950118     C                   MOVEL     TBLUNI        DS7J
622700950118     C     §7JRPS        IFEQ      'A'
622800950118     C                   MOVEL     TBLKEY        NPS(X)
622900950118     C                   ADD       1             X
623000950118     C                   ENDIF
623100970818     C     §7JFSL        IFEQ      'F'
623200970818     C                   MOVEL     TBLKEY        NPF(Y)
623300970818     C                   ADD       1             Y
623400970818     C                   ENDIF
623500950118     C     KTAB2         READE     TABEL00F                               31
623600950118     C                   ENDDO
623700011127     C*
623800011127     C*  tabelle GEDCN x reperimento divisa conteggi padroncini/tariffe
623900011127     C                   movel     'GED'         kcod1
624000011127     C                   movel     *blanks       kke1
624100011127     C                   movel     'CN'          kke1
624200011127     C     ktbe          chain     tntbe01l                           31
624300011127     C                   clear                   dgedcn
624400011127     C  N31              movel     tbeuni        dgedcn
624500130227     c
624600121119     C*  Tabella DGD    x reperimento data dal/al per conferma autoamti
624700041216     c*                 prepagati
624800041217     c                   Clear                   Tibs02Ds
624900121119     c                   Clear                   ddgd
625000041217     c                   Eval      T02Mod = 'C'
625100041217     c                   Eval      T02Sif = Knsif
625200121119     c                   Eval      T02Cod = 'DGD'
625300121119     c                   Movel(p)  '1'           T02Ke1
625400041217     c                   Call      'TIBS02R'
625500041217     c                   Parm                    Kpjba
625600041217     c                   Parm                    Tibs02Ds
625700041217     c                   If        T02Err = *Blanks
625800121119     c                   Movel     T02Uni        ddgd
625900041217     c                   EndIf
626000000619     C*
626100060710     C*  tipi record di FV4 da trasmettere in sede
626200060710     C***
626300060710     C                   Z-ADD     1             X
626400060710     C                   MOVEL     '3B'          COD
626500060710     C     KTAB2         CHAIN     TABEL00F                           31
626600060710     C*
626700060710     C     *IN31         DOWEQ     *OFF
626800060710     c                   if        tblflg=' '
626900060710     C                   MOVEL     TBLUNI        DS3B
627000060710     C     §3btrs        IFEQ      'S'
627100060710     C                   MOVEL     TBLKEY        fv4S(X)
627200060710     C                   ADD       1             X
627300060710     C                   ENDIF
627400060710     C                   ENDIF
627500060710     C     KTAB2         READE     TABEL00F                               31
627600060710     C                   ENDDO
627700960202     C**
627800960202     C                   ENDIF
627900950109     C*
628000950109     C* SPENGO GLI INDICATORI USATI PER LA RICERCA CAPOCONTI
628100950109     C                   SETOFF                                       212223
628200950109     C                   SETOFF                                       242526
628300950515     C                   SETOFF                                       273132
628400950109     C*
628500950109     C* DEFINIZIONE CAMPI
628600970423     C     *LIKE         DEFINE    BRVNPS        WNPS
628700970528     C     *LIKE         DEFINE    BRVNPS        SPUNPS
628800170613     C     *LIKE         DEFINE    BRVFGS        SPUFGS
628900970423     C     *LIKE         DEFINE    FVVNPG        KVVNPG
629000000111     C     *LIKE         DEFINE    BRVNFV        KVVNFV
629100001121     C     *LIKE         DEFINE    BRVNFV        SAVNFV
629200001123     C     *LIKE         DEFINE    fvvnfv        wnfv05
629300970423     C     *LIKE         DEFINE    FVVFGS        KVVFGS
629400970423     C**
629500970423     C     *LIKE         DEFINE    FVDCLA        NCLU
629600950109     C     *LIKE         DEFINE    FVDKFA        PKGU
629700950113     C     *LIKE         DEFINE    FVDVL1        VOLU
629800950113     C     *LIKE         DEFINE    FVDSNA        WSPEN
629900130530     C     *LIKE         DEFINE    FVDSNA        WSPE
630000950113     C     *LIKE         DEFINE    FVDSNA        WSPET
630100950110     C     *LIKE         DEFINE    FVDTRC        WTRC
630200950109     C*
630300950109     C     *LIKE         DEFINE    FTTPDR        COMPDR
630400950109     C     *LIKE         DEFINE    FTTPDR        WPDR
630500950109     C     *LIKE         DEFINE    FTTTSR        COMTSR
630600950109     C     *LIKE         DEFINE    FTTNDC        COMNDC
630700950109     C     *LIKE         DEFINE    FTTNDC        WNDC
630800950109     C     *LIKE         DEFINE    FTTSET        COMSET
630900020326     C     *LIKE         DEFINE    FTDstp        wstp
631000950109     C     *LIKE         DEFINE    FTDKSC        CO2KSC
631100950113     C     *LIKE         DEFINE    FTDRSC        COMRSC
631200021111     C     *LIKE         DEFINE    FTDVLU        WFVDVOL
631300021111     C     *LIKE         DEFINE    FTDVLU        WVOL
631400021108     C     *LIKE         DEFINE    FTDpkl        Wpes
631500021111     C     *LIKE         DEFINE    FTDpkl        WFVDPES
631600950114     C     *LIKE         DEFINE    FTDDDC        COMDDC
631700950109     C*
631800960124     C     *LIKE         DEFINE    FGVLNP        SA2LNP
631900960124     C     *LIKE         DEFINE    FGVNFV        SA2NFV
632000000731     C     *LIKE         DEFINE    FGVLNP        WFGSFV
632100960124     C     *LIKE         DEFINE    FVARTP        SA2RTP
632200960124     C     *LIKE         DEFINE    FGVDFV        WDTP
632300950915     C     *LIKE         DEFINE    FGVDFV        WDFV
632400950113     C     *LIKE         DEFINE    BRVNPG        WNPG
632500950915     C     *LIKE         DEFINE    BRVNPG        WNPG5
632600950109     C     *LIKE         DEFINE    BRVZNC        WRVZNC
632700950109     C     *LIKE         DEFINE    BRVVUC        WRVVUC
632800990906     C     *LIKE         DEFINE    BRVPUC        WRVPUC
632900950109     C     *LIKE         DEFINE    BRVCAN        WRVCAN
633000950915     C     *LIKE         DEFINE    BRVNPS        WRVNPS
633100950109     C     *LIKE         DEFINE    BRVLNP        KLNP
633200950109     C     *LIKE         DEFINE    BRVLNA        KLNA
633300950109     C     *LIKE         DEFINE    BRVNRS        KNRS
633400950109     C     *LIKE         DEFINE    BRVNSC        KNSC
633500950418     C     *LIKE         DEFINE    BRVFGS        WSAFGS
633600950418     C     *LIKE         DEFINE    BRVNFV        WSANFV
633700000111     C     *LIKE         DEFINE    BRVNFV        FGV260
633800000111     C     *LIKE         DEFINE    BRVNFV        NFV160
633900960604     C     *LIKE         DEFINE    BRVLNP        WLNP
634000960905     C     *LIKE         DEFINE    BRVNPG        NPGP
634100960905     C     *LIKE         DEFINE    BRVNFV        NFVP
634200960905     C     *LIKE         DEFINE    BRVLNP        LNPP
634300960905     C     *LIKE         DEFINE    BRVLNA        LNAP
634400960905     C     *LIKE         DEFINE    BRVNRS        NRSP
634500960905     C     *LIKE         DEFINE    BRVNSC        NSCP
634600960905     C     *LIKE         DEFINE    BRVNPS        NPSP
634700960905     C     *LIKE         DEFINE    BRVVUC        VUCP
634800990623     C     *LIKE         DEFINE    BRVPUC        PUCP
634900950109     C     *LIKE         DEFINE    BLPCBO        SAVCBO
635000950623     C*
635100980210     C     *LIKE         DEFINE    D55TFA        LNATFA
635200971107     C     *LIKE         DEFINE    D55TFP        LNAFEL
635300950707     C     *LIKE         DEFINE    BTTDUT        WDTPAR
635400970703     C     *LIKE         DEFINE    D55LIN        WLIN
635500060220     C     *LIKE         DEFINE    AR4TRC        WTRC4
635600980126     C*
635700980126     C     *LIKE         DEFINE    BRVLNA        WLNA
635800980209     C     *LIKE         DEFINE    DSBRV         SVBRVE
635900070123     C     *LIKE         DEFINE    DSBRV         dsbrvs
636000990623     C                   Z-ADD     0             COMDFS            8 0
636100990623     C                   Z-ADD     0             COMHMS            4 0
636200950621     C*
636300150609      * verifico se presenti simfel o suoi secondi livello
636400150609      * nel file della sospensione automatica per colli di clienti
636500150609      * che partono da più filiali
636600150609     c                   eval      w_FilBsp=*blanks
636700151215     c                   eval      w_FilBsps=*blanks
636800151215     c                   clear                   w_SiBsp
636900151215     c                   clear                   w_bsptip
637000150610     c     'S'           setll     fibsp03l
637100150610     c     'S'           reade     fibsp03l
637200151215     c                   dow       not %eof(fibsp03l) and w_filbsps=*blanks
637300150609     c                   z-add     1             Ix
637400150609     c                   dow       Ix<=%elem(sbsp) and sbsp(Ix)>0
637500151215     c                             and w_FilBsps=*blanks
637600150609     c                   if        %lookup(sbsp(Ix):L1)>0
637700150609     c                   eval      w_FilBsp='S'
637800151215     c                   if        bspaut=*blanks
637900151215     c                   eval      w_FilBsps='S'
638000151215     c                   endif
638100150609     c                   endif
638200150609     c                   add       1             Ix
638300150609     c                   enddo
638400150610     c     'S'           reade     fibsp03l
638500150610     c                   enddo
638600150703
638700150703     c                   Clear                   tibs02ds
638800150703     c                   Clear                   dmradan
638900150703     c                   Movel     'C'           T02Mod
639000150703     c                   Movel     KNSIF         T02Sif
639100150703     c                   Movel     'MRA'         T02Cod
639200150703     c                   Movel     'FNLSU3R'     T02Ke1
639300150703     c                   Call      'TIBS02R'
639400150703     c                   Parm                    Kpjba
639500150703     c                   Parm                    tibs02ds
639600150703    2c                   If        T02Err = *Blanks
639700150703     c                   Movel     T02Uni        Dmradan
639800150703    2c                   endif
639900150703     C***
640000150703     C* CARICO DATI VARIABILI PARTENZE
640100150703     C***
640200150703     C                   MOVEL     '3I'          COD
640300150703     C                   MOVEL     '1       '    KEY
640400150703     C     KTAB          CHAIN     TABEL00F
640500150703     c                   if        %found(tabel00f)
640600150703     C                   MOVEL     TBLUNI        DS3I
640700150703     c                   else
640800150703     C                   CLEAR                   DS3I
640900150703     c                   endif
641000150609
641100021016      * Recupero le date delle statistiche
641200021016     c                   Clear                   Tibs02Ds
641300021016     c                   Clear                   dSdf
641400021016     c                   Eval      T02Mod = 'C'
641500021016     c                   Eval      T02Sif = Knsif
641600021016     c                   Eval      T02Cod = 'SDF'
641700021016     c                   Movel(p)  Simfel        T02Ke1
641800021016     c                   Call      'TIBS02R'
641900021016     c                   Parm                    Kpjba
642000021016     c                   Parm                    Tibs02Ds
642100021016     c                   If        T02Err = *Blanks
642200021016     c                   Movel     T02Uni        dSdf
642300021016     c                   EndIf
642400950109     C*
642500950109     C                   SETON                                        OFOE
642600950109     C* GIRO UDATE
642700950113     C                   TIME                    W0140            14 0
642800050707     C***                MOVEL     W0140         WTIME             6 0
642900950109     C                   MOVE      W0140         WDTGIO            8 0
643000950109     C                   Z-ADD     WDTGIO        G02DAT
643100950109     C                   MOVEL     *BLANK        G02ERR
643200950109     C                   CALL      'XSRDA8'
643300950109     C                   PARM                    WLBDAT
643400950109     C                   MOVEL     G02INV        DATEU             8 0
643500950109     C*
643600950109     C                   MOVEL     'R'           COMTSR
643700950109     C                   Z-ADD     0             DFV
643800950113     C                   CLEAR                   SAVSPE
643900970124     C* CAMPO CHE PASSO VUOTO AL PGM FNLSA2R3
644000970124     C                   CLEAR                   WWLNA
644100030107
644200950109     C                   ENDSR
644300070827     C**********************************************************************
644400070827     C*  P A D R O N C I N I
644500070827     C**********************************************************************
644600070827     C* CARICO I CODICI PADRONCINI PER I QUALI E' NECESSARIO CREARE I
644700070827     C*   FILE fiftt00F E fiftd00F
644800070827     C* CARICO ANCHE QUELLI CON MERCE PORTATA A MAGAZZINO
644900070827     C* CARICO ANCHE I PADRONCINI DIPENDENTI
645000070827     c     CarPAD        BEGSR
645100070827     c                   movel     fgvdfv        apddfv
645200070827     c
645300070827     C                   Z-ADD     1             X
645400070827     C                   Z-ADD     1             Y
645500070827     C                   CALL      'FNLSA2R2'
645600070827     C                   PARM                    APDPDR            7 0
645700070827     C                   PARM                    APDETM            1
645800070827     C                   PARM                    APDETP            1
645900070827     C                   PARM                    APDPDD            1
646000070827     c                   Parm                    ApdScm            1
646100070827     c                   Parm                    ApdScp            1
646200070827     C                   PARM                    WFINE             1
646300070827     C                   PARM                    simfel            3 0
646400070827     C                   PARM                    apddfv            8 0
646500070827     C*
646600070827     C     WFINE         DOWNE     'S'
646700140620     C**   APDPDD        IFEQ      *BLANKS
646800070827     C                   MOVEL     APDPDR        PDR(X)
646900070827     C                   MOVEL     APDETM        ETM(X)
647000070827     C                   MOVEL     APDETP        ETP(X)
647100070827     c                   Movel     ApdScm        Scm(x)
647200070827     c                   Movel     ApdScp        Scp(x)
647300070827     C                   ADD       1             X
647400140620     C**                 ELSE
647500140620     C**                 MOVEL     APDPDR        PDD(Y)
647600140620     C**                 ADD       1             Y
647700140620     C**                 END
647800070827     C*
647900070827     C                   CALL      'FNLSA2R2'
648000070827     C                   PARM                    APDPDR
648100070827     C                   PARM                    APDETM
648200070827     C                   PARM                    APDETP
648300070827     C                   PARM                    APDPDD
648400070827     c                   Parm                    ApdScm            1
648500070827     c                   Parm                    ApdScp            1
648600070827     C                   PARM                    WFINE             1
648700070827     C                   PARM                    simfel            3 0
648800070827     C                   PARM                    apddfv            8 0
648900070827     C                   ENDDO
649000070827     c
649100070827     c* Controllo riempimento schiera
649200070827     c                   clear                   trul0sds
649300070827     c                   eval      i0sski='PDR/ETM/ETP/...'
649400070827     c                   eval      i0sele=%elem(pdr)
649500070827     c                   eval      i0spie=x-1
649600070827     c                   eval      i0sfile='FNAPD00F  '
649700070827     c                   eval      i0ssif=knsif
649800130718     c                   eval      i0spgm='FNLSU2R'
649900070827     c                   movel     trul0sds      kpjbu
650000070827     c                   call      'TRUL0SR'
650100070827     c                   parm                    kpjba
650200070827     c
650300140620     c**                 clear                   trul0sds
650400140620     c**                 eval      i0sski='PDD'
650500140620     c**                 eval      i0sele=%elem(pdD)
650600140620     c**                 eval      i0spie=y-1
650700140620     c**                 eval      i0sfile='FNAPD00F  '
650800140620     c**                 eval      i0ssif=knsif
650900140620     c**                 eval      i0spgm='FNLSU2R'
651000140620     c**                 movel     trul0sds      kpjbu
651100140620     c**                 call      'TRUL0SR'
651200140620     c**                 parm                    kpjba
651300130530     c
651400130530     c* in caso di parzializzazione per cod cliente
651500130530     c* Carico la £6 della linea partenza FGV per vedere se ha clienti abilita
651600130530     c*  ti alla borderizzazione
651700130530     c                   clear                   L6
651800130530     c                   if        pa2ccm>*zeros
651900130530     c                   clear                   trul06ds
652000130530     c                   eval      d06cod = '£6'
652100130530     c                   movel     simfel        d06key
652200130530     c                   move      'S'           d06key
652300130530     c                   movel     fgvdfv        d06drf
652400130530     c                   movel     'L'           d06tla
652500130530     c                   movel     trul06ds      kpjbu
652600130530     c                   call      'TRUL06R'
652700130530     c                   parm                    kpjba
652800130530     c                   movel     kpjbu         trul06ds
652900151127
653000151127     c* Carico eventuale tabella con i codici collegati da borderizzare
653100151127     C                   MOVEL     'CLF'         kcod1
653200151127     c                   z-add     1             zz
653300151127     C                   eval      kke1=pa2ccm
653400151127     c     ktbe          setll     tntbe01l
653500151127     c     ktbe          reade     tntbe01l
653600151127    2c                   dow       not %eof(tntbe01l)
653700151127     c                   if        tbeatb=' '
653800151127     c                   movel     tbeke2        ccmb(zz)
653900151127     c                   add       1             zz
654000151127     c                   endif
654100151127     c     ktbe          reade     tntbe01l
654200151127     c                   enddo
654300151127     c
654400130530     c                   endif
654500130530     c
654600070827     c                   ENDSR
654700150603     C**********************************************************************
654800150603     C*  FIBSP00F - lnp/clienti con sospensione bolle
654900150603     C**********************************************************************
655000150603     c     sr_fibsp      begsr
655100170327     c                   clear                   w_SiBsp
655200150703     c                   clear                   w_bsptip
655300150603     c     w_clibo       chain     fibsp02l
655400151214     c                   if        %found(fibsp02l) and bspfpa='S'
655500151214     c                                              and bspaut=*blanks
655600151215     c                             and %lookup(blplnp:sbsp)>0
655700150603     C                   eval      w_SiBsp='S'
655800150703     c                   eval      w_bsptip='K'
655900150603     c                   else
656000150603     c     blplnp        chain     fibsp01l
656100150603     c                   if        %found(fibsp01l) and bspfpa='S'
656200151214     c                                              and bspaut=*blanks
656300150603     C                   eval      w_SiBsp='S'
656400150703     c                   eval      w_bsptip='L'
656500150603     c                   endif
656600150603     c                   endif
656700150603     c                   endsr
656800150703     C**********************************************************************
656900150703     C*  Memorizzazinoe clienti/lnp con bolle in sosp. oltre xx giorni
657000150703     C**********************************************************************
657100150703     c     sr_MemoEmail  begsr
657200150703    1c                   select
657300150703     c* SOSPENSIONE per CLIENTE
657400150703   WHc                   when      w_bsptip='K'
657500150703     c                   eval      indy=%lookup(sav_clibo:s_bspk)
657600150703    2c                   if        indy=0
657700150703     c                   eval      indy=%lookup(0:s_bspk)
657800150703     c* memorizzo cod cliente
657900150703     c                   eval      s_bspk(indy)=sav_clibo
658000150703    2c                   endif
658100150703     c* memorizzo numero di spedizioni in sosp oltre xx giorni
658200150703     c                   add       1             s_bspkbo(indy)
658300150703     c* memorizzo la data più bassa di immissione
658400150703    2c                   if        s_BSPkD(indy)=0 or
658500150703     c                             blpdim>s_BSPkD(indy)
658600150703     c                   eval      s_BSPkD(indy)=blpdim
658700150703    2c                   endif
658800150703     c* SOSPENSIONE per LINEA DI PARTENZA
658900150703   WHc                   when      w_bsptip='L'
659000150703     c                   eval      indy=%lookup(sav_lnp:s_bspL)
659100150703    2c                   if        indy=0
659200150703     c                   eval      indy=%lookup(0:s_bspL)
659300150703     c* memorizzo cod cliente
659400150703     c                   eval      s_bspL(indy)=sav_lnp
659500150703    2c                   endif
659600150703     c* memorizzo numero di spedizioni in sosp oltre xx giorni
659700150703     c                   add       1             s_bspLbo(indy)
659800150703     c* memorizzo la data più bassa di immissione
659900150703    2c                   if        s_BSPlD(indy)=0 or
660000150703     c                             blpdim>s_BSPlD(indy)
660100150703     c                   eval      s_BSPlD(indy)=blpdim
660200150703    2c                   endif
660300150703    1c                   endsl
660400150703     c
660500150703     c                   endsr
660600150703     c*-------------------------------------------------------------------
660700150703     c     INV_email     BEGSR
660800150703     c
660900150703     c                   clear                   writerig
661000150703     c                   if        not %open(prtemail)
661100150703     C                   EXSR      SR_OPENPRTF
661200150703     C                   endif
661300150703     c
661400150703     c                   exsr      Routend
661500150703     c
661600150703     c                   ENDSR
661700151022       //--------------------------------------------------------------
661800151022       //?  Errore per bolla non trovata
661900151022       //--------------------------------------------------------------
662000151022     c     Sr_errore     BEGSR
662100151022
662200151022     C                   movel     *blanks       wrkEml          253
662300151022     C                   movel     *blanks       wrkEmlCc        253
662400151022     C                   movel     *blanks       wrkMsg         5000
662500151022     C                   movel     *blanks       wrkOgg           44
662600151022     C                   movel     *blanks       wrkesito          1
662700151022     c
662800151022     C                   MOVEL     MSG(5)        wrkogg
662900151022     C                   MOVEL(P)  MSG(6)        wrkmsg
663000151022     c                   eval      %subst(wrkmsg:43:16)=
663100151022     c                             %editc(bltspe:'X')
663200151022     c                   eval      wrkemlcc ='ced@brt.it'
663300151022     c                   eval      wrkeml='elisa.sanfelici@brt.it'
663400151022     c
663500151022     C                   call(e)   'TIS701C1'
663600151022     C                   parm                    wrkEml
663700151022     C                   parm                    wrkEmlCc
663800151022     C                   parm                    wrkOgg
663900151022     C                   parm                    wrkMsg
664000151022     C                   parm                    wrkesito
664100151022
664200151022     c                   ENDSR
664300150703      /free
664400150703       //--------------------------------------------------------------
664500150703       //?Override al file di stampa per impostarvi i dati per
664600150703       //?  l'invio via e-mail   +   stampa inizio e-mail
664700150703       //--------------------------------------------------------------
664800150703       BEGSR sr_OpenPrtF;
664900150703
665000150703         // Override al file di stampa per impostarvi i dati per
665100150703         // l'invio via e-mail
665200150703           exsr sr_Override;
665300150703
665400150703
665500150703           indy = 1 ;
665600150703
665700150703          dow     s_BSPl(indy)>0  ;
665800150703
665900150703          if      s_BSPlBO(indy)>0 ;
666000150703          dataiso=%date(s_BSPlD(indy)) ;
666100150703          data_eur=dataiso  ;
666200150703
666300150703           clear O_testo;
666400150703           chain  s_BSPl(indy)   azorg01l  ;
666500150703           if not %found(azorg01l)  ;
666600150703           eval orgdes=*all'?'  ;
666700150703           endif  ;
666800150703           %subst(O_testo:6:20) = %editc(s_BSPl(indy):'X')+'-'+orgdes  ;
666900150703           %subst(O_testo:46:10) =%char(data_eur)  ;
667000150703           %subst(O_testo:64:9 ) =%editc(s_BSPlBO(indy):'2')  ;
667100150703           except PrtDet;
667200150703           writerig=writerig+1  ;
667300150703           endif  ;
667400150703
667500150703           // se ne ho scritte 50 di righe devo inviare
667600150703           if writerig>=50  ;
667700150703             exsr      Routend  ;
667800150703             clear writerig                  ;
667900150703
668000150703             exsr sr_Override;
668100150703
668200150703           endif  ;
668300150703
668400150703           indy=indy+1  ;
668500150703           enddo    ;
668600150703
668700150703          // Stesso ciclo sui cod clienti
668800150703          indy = 1      ;
668900160208          dow     s_BSPk(indy)>0  ;
669000150703
669100150703          if      s_BSPkBO(indy)>0 ;
669200150703          dataiso=%date(s_BSPkD(indy)) ;
669300150703          data_eur=dataiso  ;
669400150703
669500150703           clear O_testo;
669600170613           chain  (1:d_dutkci:s_BSPk(indy)) cnaco00f  ;
669700150703           if not %found(cnaco00f)  ;
669800150703           eval acorag=*all'?'  ;
669900150703           endif  ;
670000150703           %subst(O_testo:6:20) = %editc(s_BSPk(indy):'X')+'-'+acorag  ;
670100150703           %subst(O_testo:46:10) =%char(data_eur)  ;
670200150703           %subst(O_testo:64:9 ) =%editc(s_BSPkBO(indy):'2')  ;
670300150703           except PrtDet;
670400150703           writerig=writerig+1  ;
670500150703           endif  ;
670600150703
670700150703           // se ne ho scritte 50 di righe devo inviare
670800150703           if writerig>=50  ;
670900150703             exsr      Routend  ;
671000150703             clear writerig                  ;
671100150703
671200150703             exsr sr_Override;
671300150703
671400150703           endif  ;
671500150703
671600150703           indy= indy+1  ;
671700150703           enddo    ;
671800150703
671900150703             exsr      Routend  ;
672000150703       ENDSR;
672100150703
672200150703       //--------------------------------------------------------------
672300150703       //?Override al file di stampa per impostarvi i dati per
672400150703       //?  l'invio via e-mail   +   stampa inizio e-mail
672500150703       //--------------------------------------------------------------
672600150703       BEGSR sr_Override;
672700150703
672800150703         reset TRTCM1ds;
672900150703
673000150703         IF  §MRAdreg <> *blank;
673100150703           §CM1mitt = %trim(§MRAdmitt);
673200150703           §CM1dst  = %trim(§MRAddest);
673300150703
673400150703         // Valorizzo i campi della e-m@ail
673500150703          chain  simfel   azorg01l ;
673600150703
673700150703          if   %found(azorg01l) ;
673800150703           og140=orgde0  ;
673900150703           if §ogapo>*zeros  ;
674000150703           §cm1dst=%trim(§cm1dst) +'; CED'+ §ogapo +'@brt.it;';
674100150703           else  ;
674200150703           §cm1dst=%trim(§cm1dst) +'; CED'+%editc(orgf70:'X')+'@brt.it;';
674300150703          endif  ;
674400150703          endif  ;
674500150703
674600150703           §CM1tips = §MRAdreg;
674700150703           §CM1po   = %editc(simfel:'X');
674800150703           §CM1var  = '*OBJM*' + 'SOSPENSIONE AUTOMATICA bolle      '   ;
674900150703           §CM1idp  = §MRAdidpro;
675000160208           Qcmd1 = C_CmdOvrPrtF
675100150703                + ' outq(' + %trim(§MRAdoutqi) + ')'
675200150703                + ' usrdfndta(''' + TRTCM1ds + ''')';
675300150703         ELSE;
675400160208           Qcmd1 = C_CmdOvrPrtF;
675500150703         ENDIF;
675600150703
675700150703         callp(e) qCmdExc(Qcmd1 : %size(Qcmd1));
675800150703
675900150703           open PrtEMAIL;
676000150703
676100150703         // Stampa una testata se NON è richiesta la e-mail
676200150703           IF  §MRAdreg =  *blank;
676300150703             O_testo = JobUser + ' - ' + nompgm
676400150703                     + ' - ' + %editc( *date : 'Y' )
676500150703                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
676600150703             except PrtDet;
676700150703             clear O_testo;
676800150703             except PrtDet;
676900150703             except PrtDet;
677000150703           ENDIF;
677100150703
677200150703         // Stampa testo iniziale
677300150703           O_testo = 'E'' stata eseguita la Sospensione Automatica +
677400150703                      di bolle immesse da almeno xx giorni.' ;
677500150703           %subst(O_testo:72:2) = (%editc(§3iggm: '4'))     ;
677600150703           except PrtDet;
677700150703           O_testo = 'Verificare col mittente o procedere alla lor+
677800150703                      o chiusura come "Merce MAI Affidata".' ;
677900150703           except PrtDet;
678000150703
678100150703         // Stampa una riga vuota
678200150703           clear O_testo;
678300150703           except PrtDet;
678400150703
678500150703         // Stampa intestazione
678600150703           O_testo = ' Bolle spostate in sospensione di          Data imm+
678700150703                     issione    Bolle da verificare       ';
678800150703           except PrtDet;
678900150703           O_testo = 'Codice Cliente o Linea di Partenza          più vec+
679000150703                     chia                                 ';
679100150703         //%subst(o_testo:90:10)= knmus     ;
679200150703           except PrtDet;
679300150703
679400150703         // Stampa una riga vuota
679500150703           clear O_testo;
679600150703           except PrtDet;
679700150703       ENDSR;
679800150703
679900150703       //--------------------------------------------------------------
680000150703       //?Operazioni finali.
680100150703       //--------------------------------------------------------------
680200150703       BEGSR RoutEnd;
680300150703
680400150703         if %open(PrtEMAIL);
680500150703
680600150703         //?Chiusura dello spool?
680700150703           clear O_testo;
680800150703           except PrtDet;
680900150703           O_testo = '***   Fine Lista   ***';
681000150703           except PrtDet;
681100150703
681200150703           close PrtEMAIL;
681300150703
681400150703         //?Eliminazione overflow?
681500160208           Qcmd1 = C_CmdDltOvr;
681600150703           callp(e) qCmdExc(Qcmd1 : %size(Qcmd1));
681700150703
681800150703         endif;
681900150703
682000150703       ENDSR;
682100150703
682200150703      /end-free
682300150703     o* ---------------------------------------------------------------
682400150703     o* ?Spool di stampa (per e-mail).
682500150703     o* ---------------------------------------------------------------
682600150703     oPrtEMAIL  e            PRTdet      1
682700150703     o                       O_testo
682800030902
682900030902      *---------------------------------------------------------------*
683000030902
683100960201**         CMD1
683200021014OVRDBF FILE(FNBLPE0T)               MBR(Mxxx000)
683300960202**         CM19
683400021014OVRDBF FILE(FNBLPT0T)               MBR(Mxxx000)
683500960202**         CM32
683600021014OVRDBF FILE(FISPP00T)               MBR(Mxxx000)
683700000428**         CM37
683800021014OVRDBF FILE(FNFWA00T)               MBR(Mxxx000)
683900000428**         CM38
684000021014OVRDBF FILE(FNFVA01T)               MBR(Mxxx000)
684100920324**         CM41
684200021014ALCOBJ OBJ((FNBLPE0T *FILE *EXCL Mxxx000))
684300920324**         CM47
684400021014ALCOBJ OBJ((FISPP00T *FILE *EXCL Mxxx000))
684500000428**         CM48
684600021014ALCOBJ OBJ((FNFWA00T *FILE *EXCL Mxxx000))
684700000428**         CM49
684800021014ALCOBJ OBJ((FNFVA01T *FILE *EXCL Mxxx000))
684900920324**         CM51
685000021014ALCOBJ OBJ((FNBLPT0T *FILE *EXCL Mxxx000))
685100920324**  ERRORI
685200920324XXXXXXXXXXXXXXXXXXXX/XXX       ** ELENCO FOGLI VIAGGIO NON ELABORA
685300920324TI PERCHE' MEMBRI GIA' ALLOCATI **
685400920324 FOGLIO VIAGGIO
685500920324           ** TELEFONARE CON URGENZA ALL'UFFICIO C.E.D BOLOGNA  **
685600960129** MSG
685700960129ERRORE TRASMISSIONE FOGLI VIAGGIO PARTENZE
685800111019A FILIALE: NON TRASMESSO Foglio Viaggio XXX-XX.XXX.
685900070827CREATO JOBLOG per lavoro durato più di 5 minuti
686000060714                         FOGLIO VIAGGIO XXX-XXXXX.
686100151022ERRORE DI BOLLA DISALLINEATA FNLSU2R
686200151022Presente fnBLT00f senza relativo fnBLP -->
686300060714**
686400060714DSPJOBLOG OUTPUT(*PRINT)
