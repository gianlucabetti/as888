000100040601     H DECEDIT('0,') DATEDIT(*yMd.)
000200070207     H* FNLS45R *-----------------------------------------------------*
000300111018     H*         - CARICO DATI DA terminalini a FNBRV
000400070212     h*         - nuovo file di caricamento dati spunta anche x PDA
000500930629     H*---------------------------------------------------------------*
000600070906     Ffibar00e  IF A E           K DISK
000700070906     F                                     RENAME(barco:barcoerr)
000800070906     FAZORG01L  IF   E           K DISK
000900000209     FFISGN03L  UF   E           K DISK
001000911014     FTABEL00F  IF   E           K DISK
001100130228     Ftntbe01L  IF   E           K DISK
001200000505     FFNFWA01L  IF   E           K DISK
001300000505     FFNFVA01L  IF   E           K DISK
001400960925     F                                     RENAME(FNFVA000:FNFVA001)
001500070201     FFNBRV07L  IF   E           K DISK    prefix(br7:3)
001600070131     F                                     RENAME(FNBRV000:FNBRV007)
001700140613     FFNBRV05L  UF   E           K DISK
001800140613     F                                     RENAME(FNBRV000:FNBRV005)
001900070131     FFNBRV01L  UF A E           K DISK
002000070201     FFNBRV09L  IF   E           K DISK    prefix(br9:3)
002100070131     F                                     RENAME(FNBRV000:FNBRV009)
002200070131     FFNBRVE2L  IF A E           K DISK
002300070131     F                                     RENAME(FNBRV000:FNBRVE)
002400070710     FFNBRVE1L  IF   E           K DISK
002500070710     F                                     RENAME(FNBRV000:FNBRVE1)
002600070212     FFIbar00F  UF   E           K DISK    USROPN  infds(FIBAR)
002700001114     FFNBLP01L  UF   E           K DISK
002800961118     FFNEVS02L  IF   E           K DISK    USROPN
002900050331     FTigcp21l  IF   E           K DISK
003000091209     FFIar501l  uF a E           K DISK
003100180115     FFIars01l  uF a E           K DISK
003200960520     FFNFGV01L  IF   E           K DISK
003300040908     FFNFGV02L  IF   E           K DISK
003400040908     F                                     RENAME(FNFgV000:FNFgV002)
003500000505     FFNFGW01L  IF   E           K DISK
003600021002     FFNFVV01L  IF   E           K DISK
003700070207     FFNFVV02L  IF   E           K DISK    prefix(FV2:3)
003800950927     F                                     RENAME(FNFVV000:FNFVV002)
003900990120     FFNDCT01L  IF   E           K DISK
004000990120     F                                     RENAME(FNDCT000:FNDCT001)
004100990126     FFNDCD02L  UF   E           K DISK
004200950927     FFNFVA02L  IF   E           K DISK
004300150622     FFIBSP01L  IF   E           K DISK
004400150622     FFIBSP02L  IF   E           K DISK    rename(fibsp000:fibsp02)
004500990518     FFNANM02L  UF A E           K DISK
004600990518     F                                     INFDS(FNANM2)
004700970217     FFNAGB01L  UF A E           K DISK
004800970205     FFNLBL02L  IF   E           K DISK
004900941206     F* BOLLE ARRIVI ----------------------
005000941206     FFNART27L  UF   E           K DISK
005100050223     FFNARB01L  uF   E           K DISK
005200941206     F* BOLLE TRANSITO --------------------
005300021203     FFNBTT37L  UF   E           K DISK
005400021203     FFNBTP11L  IF   E           K DISK
005500941206     F* BOLLE PARTENZA --------------------
005600941206     FFNBLT27L  UF   E           K DISK
005601180124     F* DETTAGLIO COLLI UNICO -------------
005602180124     FFiART27L  UF   E           K DISK    prefix(U_)
005700911014     D*
005800960523     D L6              S              3  0 DIM(30)
005900961212     D*
006000130614     D S5F             S              8    DIM(9000)
006100130614     D F5F             S             12    DIM(9000)
006200020218     D WPO             S              3    DIM(4)
006300020204     D CAN             S              1    DIM(200)
006400961212     D*
006500921229     D NPS             S              2  0 DIM(101)
006600921229     D RPS             S              1    DIM(101)
006700000210     D TPS             S              1    DIM(101)
006800000316     D OPS             S              3  0 DIM(101)
006900000316     D INV             S              1    DIM(101)
007000040503     D FSL             S              1    DIM(101)
007100961212     D*
007200941214     D CAT             S              1  0 DIM(20)
007300941214     D NST             S              1    DIM(20)
007400950927     D*
007500961122     D TTR             S              1    DIM(10)
007600961122     D*
007700950927     D PG2             S              5  0 DIM(20)
007800950927     D*
007900000505     D FFS             S              3    DIM(301)
008000970114     D*
008100040601     D CM5             S              1    DIM(15) CTDATA PERRCD(15)
008200980928     D*
008300111018     D** ERC             S              1  0 DIM(10)
008400111018     D** ERF             S              5    DIM(10)
008500111018     D** ERE             S              3  0 DIM(10)
008600111018     D** ERp             S              2    DIM(10)
008700981105     D K70             S              1    DIM(70)
008800981105     D MSG             S             70    DIM(3) CTDATA PERRCD(1)
008900001109     D ACG             S              3  0 DIM(50)
009000130228     d* tfp abilitati a far partire colli di altro tfp
009100130228     D xcotfp          S              3    DIM(85)
009200000516     D*-----------------------------
009300930701     D* DS PER CONFRONTO SPUNTE: BAR
009400930701     D*-----------------------------
009500930701     D                 DS
009600960524     D  BARCAN                 1      1
009700960524     D  BARLNP                 2      4  0
009800960524     D  BARLNA                 5      7  0
009900960524     D  BARNRS                 8      9  0
010000960524     D  BARNSC                10     16  0
010100960524     D  BARNPS                17     18  0
010200960524     D  BARVUC                19     25  6
010300960524     D  BARZNC                26     27  0
010400021002     D  BARFGS                28     30  0
010500960524     D  BARNPG                31     31  0
010600991223     D  BARNFV                32     37  0
010700991223     D  BARPUC                38     44  3
010800000223     D  BARDFS                45     52  0
010900070209     D  BARHMS                53     58  0
011000070209     D  BARmis                59     61  0
011100070822     D  BARimmis              45     61  0
011200070209     D  BARPRU                62     71
011300070209     D  BARTAP                72     72
011400960524     D* DS PER CONFRONTO SEGNACOLLO CHE STO CARICANTO
011500070209     D  BAR1                   1     72
011600961212     D* KEY PER CHAIN FOGLI VIAGGIO A ROTTURA
011700991223     D  BARKEY                28     37  0
011800930701     D*-----------------------------
011900930701     D* DS PER CONFRONTO SPUNTE: BRV
012000930701     D*-----------------------------
012100930701     D                 DS
012200960524     D  BRVCAN                 1      1
012300960524     D  BRVLNP                 2      4  0
012400960524     D  BRVLNA                 5      7  0
012500960524     D  BRVNRS                 8      9  0
012600960524     D  BRVNSC                10     16  0
012700960524     D  BRVNPS                17     18  0
012800960524     D  BRVVUC                19     25  6
012900960524     D  BRVZNC                26     27  0
013000021002     D  BRVFGS                28     30  0
013100960524     D  BRVNPG                31     31  0
013200991223     D  BRVNFV                32     37  0
013300991223     D  BRVPUC                38     44  3
013400000223     D  BRVDFS                45     52  0
013500070131     D  BRVHMS                53     58  0
013600070209     D  Brvmis                59     61  0
013700070822     D  Brvimmis              45     61  0
013800070209     D  BrvPRU                62     71
013900070209     D  BrvTAP                72     72
014000021014     D* DS PER CONFRONTO SEGNACOLLO CHE STO CARICANTO
014100070209     D  BRV                    1     72
014200960524     D* KEY PER CHAIN FOGLI VIAGGIO A ROTTURA
014300991223     D  BRVKEY                28     37  0
014400960527     D                 DS
014500021014     D  BR7FGS                 1      3  0
014600960527     D  BR7NPG                 4      4  0
014700991223     D  BR7NFV                 5     10  0
014800960527     D* KEY PER CHAIN FOGLI VIAGGIO A ROTTURA
014900991223     D  BR7KEY                 1     10  0
015000020205     D                 DS
015100040303     D  BR9FGS                 1      3  0
015200040303     D  BR9NPG                 4      4  0
015300040303     D  BR9NFV                 5     10  0
015400040303     D  BR9KEY                 1     10  0
015500941213     D WLBDAT          DS
015600941213     D  G02DAT                 1      8  0
015700941213     D  G02INV                 9     16  0
015800941213     D  G02ERR                17     17
015900941213     D  G02TGI                18     22  0
016000000223     D WGIDAT          DS
016100000223     D  GI8DAT                 1      8  0
016200000223     D  GI8INV                 9     16  0
016300000223     D  GI8TGI                17     21  0
016400950927     D*LINEE   DI ARRIVO
016500950927     D                 DS
016600950927     D  FVALNA                 1      3  0
016700950927     D  FVAFFV                 4    243
016800950927     D  FVAFF2               244    450
016900000505     D  FWAFF3               451    690
017000000505     D  FWAFF4               691    900
017100000505     D  VUOTO                901    903
017200000505     D  FFV                    1    903
017300000505     D                                     DIM(301)
017400950927     D*FILIALE DI SCARICO
017500950927     D                 DS
017600950927     D  LNA2                   1      3  0
017700950927     D  FVAFLP                 4    243
017800950927     D  FVAFL2               244    450
017900000505     D  FWAFL3               451    690
018000000505     D  FWAFL4               691    900
018100000505     D  VUOTO2               901    903
018200000505     D  FLP                    1    903
018300000505     D                                     DIM(301)
018400980115     D* LINEE DI ARRIVO  PER FNFGV00F
018500980115     D                 DS
018600980115     D  FGVLNA                 1      3  0
018700980115     D  FGVFFV                 4    243
018800980115     D  FGVFF2               244    450
018900000505     D  FGWFF3               451    690
019000000505     D  FGWFF4               691    900
019100000505     D  VUOTO3               901    903
019200000505     D  FFG                    1    903
019300000505     D                                     DIM(301)
019400980115      *
019500161007     D**               DS
019600161007     D**GCPAGC                 1      4  0
019700161007     D**GCPMGC                 5      8  0
019800161007     D**GCPDGC                 1      8  0
019900970217     D                 DS
020000980128     D  ARTAAS                 1      4  0
020100980128     D  ARTLNP                 5      7  0
020200980128     D  ARTNRS                 8      9  0
020300980128     D  ARTNSP                10     16  0
020400980128     D  ARTTBO                17     17
020500980128     D  ARTSPE                 1     17
020600970217     D                 DS
020700970217     D  BLTAAS                 1      4  0
020800970217     D  BLTLNP                 5      7  0
020900970217     D  BLTNRS                 8      9  0
021000970217     D  BLTNSP                10     16  0
021100970217     D  BLTTBO                17     17
021200970217     D  BLTSPE                 1     17
021300970218     D                 DS
021400970218     D  BTTAAS                 1      4  0
021500970218     D  BTTLNP                 5      7  0
021600970218     D  BTTNRS                 8      9  0
021700970218     D  BTTNSP                10     16  0
021800980120     D  BTTTBO                17     17
021900970218     D  BTTSPE                 1     17
022000970217     D                 DS
022100970217     D  SAVAAS                 1      4  0
022200970217     D  SAVLNP                 5      7  0
022300970217     D  SAVNRS                 8      9  0
022400970217     D  SAVNSP                10     16  0
022500970508     D  SAVBOL                 1     16  0 INZ
022600970508     D  SAVTBO                17     17    INZ
022700970217     D  SAVSPE                 1     17
022800980120     D                 DS
022900980120     D  WDAAS                  1      4  0
023000980120     D  WDLNP                  5      7  0
023100980120     D  WDNRS                  8      9  0
023200980120     D  WDNSP                 10     16  0
023300980120     D  WDSPE                  1     16
023400021014     D                 DS
023500021014     D  WCED                   1      6
023600021014     D  WTER                   7      9
023700021014     D  WVUOTO                10     10
023800021014     D  WPROF                  1     10
023900020201     D                 DS
024000020201     D  BLPAAS                 1      4  0
024100020201     D  BLPMGS                 5      8  0
024200020201     D  BLPDSP                 1      8  0
024300020201     D                 DS
024400020201     D  ARBAAS                 1      4  0
024500020201     D  ARBMGS                 5      8  0
024600020201     D  ARBDSP                 1      8  0
024700020201     D                 DS
024800020201     D  BTPAAS                 1      4  0
024900020201     D  BTPMGS                 5      8  0
025000020201     D  BTPDSP                 1      8  0
025100930615     D KPJBA         E DS
025200110103      *
025300110103      * - Parametri per richiamo al pgm di controllo profilo utenti
025400110103     d TIBS34ds      e ds
025500110103      * - Ds di riferimento al file esterno AzUte00F
025600110103     d AZUTEds       e ds                  ExtName(AzUte00F)
025700110103      * - Ds per dati organigramma
025800110103     d dDatiUte      e ds
025900110103      *
026000961122     D DSTV          E DS
026100990121     D DCCH            DS
026200990121     D  §CHCRA                51     51
026300990121     D DTAD            DS
026400990121     D  §TARGR                26     26
026500990126     D  §TARIT                82     82
026600961122     D DS7C          E DS
026700921020     D DS7N          E DS
026800040430     D DS7O          E DS
026900921229     D DS7J          E DS
027000970522     D DS7A2         E DS
027100030723     D DVPO          E DS
027200000928     D DS5A          E DS
027201180125     D DS3E          E DS
027300170505     D**** DS5APT        E DS
027400000614     D OG143         E DS
027500941229     D* DS PER TRUL06R - CARICAMENTO £X
027600941229     D DSUL06        E DS                  EXTNAME(TRUL06DS)
027700941229     D  LIN                    1     90  0
027800941229     D                                     DIM(30)
027900000705     D DSLV53        E DS                  EXTNAME(FNLV53DS)
028000020213     D DSLV55        E DS                  EXTNAME(FNLV55DS)
028100970814     D DSLR33        E DS                  EXTNAME(FNLR33DS)
028200021014     D DSBS55        E DS                  EXTNAME(TIBS55DS)
028300990120     D DSBS02        E DS                  EXTNAME(TIBS02DS)
028400161007     d FNLGI1DS      E DS
028500170125     d
028600170125     d TRULVPODS     e ds
028700170125     D  skFilOk               16    765    dim(250)
028701180124     D fic             s                   like(skFilOk) dim(250) inz
028800170125     d
028900070212     d
029000070212     d* DS passaggio dati
029100070207     D fnls45ds      E DS
029200160726     d
029300160726     D DAGBFLO       E DS
029400091209     D DAR5          E DS
029500091209     D DAR5GEN       E DS
029501180125     D DARSJ         E DS
029502180125     D DARTFLO       E DS
029600990518     D FNANM2          DS
029700960416     D  ANMNR2               397    400B 0
029800070212     D FIBAR           DS
029900030226     D  BA1_file              83     92
030000030226     D  BA1_libr              93    102
030100150622     d                 ds
030200150622     d bsplin
030300150622     d sbsp                           3  0 dim(30) overlay(bsplin)
030400040811
030500040811      * ds per il controllo della presenza di CA per la spedizione richiesta
030600040811     d FIDN12DS      e ds
030700040811     d  skKey                 26    305    dim(20)
030800040811     d  skAnn                306    325    dim(20)
030900040811     d  skDca                326    485  0 dim(20)
031000040811     d  skFca                486    545  0 dim(20)
031100040811     d  skDch                546    705  0 dim(20)
031200040811     d  skCch                706    745    dim(20)
031300040811     d*
031400070212     D DSRECBRV      E DS                  extname(fnbrv00f)  prefix(D_)
031500161007     D tigcp00ds     E DS                  extname(tigcp00f)
031600080317     D WTSU            s              1  0
031700160421     D wznc            s              2
031800140613     D kdel_npg        s                   like(brvnpg)
031900170509     D boldbr          s                   like(arbdbr)
032000130228     D KCOD            s                   like(tbecod)
032100130228     D Kke1            s                   like(tbeke1)
032200130228     D Kke2            s                   like(tbeke2)
032300130228     D SAWBRV          s                   like(brv)
032400080317     D WPES            s                   like(brvpes)
032500070208     D BPES            s                   like(brvpes)
032600070208     D BTFP            s                   like(brvFLE)
032700050322     D savesart        s                   like(wesart)
032800041105     D wprimotbo       s                   like(savtbo)
032900070221     D WSAVFLG         s                   like(dls45FLG)
033000140613     D wdfs            s                   like(brvdfs)
033100140616     D qd_nfv          s                   like(brvnfv)
033200140616     D qd_npg          s                   like(brvnpg)
033300140616     D qd_dfs          s                   like(brvdfs)
033400140616     D qd_hms          s                   like(brvhms)
033500140616     D bardcs          s                   like(brvdcs)
033600140616     D barhcs          s                   like(brvhcs)
033700140616     D barfcf          s                   like(fvvfcf)
033800140616     D brvfcf          s                   like(fvvfcf)
033900150622     d w_clibo         s                   Like(blpccm)
034000160726     d welasta_p       s              1
034100160726     d wBspTp          s              1
034200150622     d wBspspu         s              1
034300140618     d A_imm           s             14  0 inz
034400140618     d B_imm           s             14  0 inz
034500140618     d brv_imm         s             14  0 inz
034600140618     d brv_sca         s             14  0 inz
034700140618     d DatReale        s              8  0 inz
034800140618     d oraReale        s              4  0 inz
034900130228     d Indx            s              3  0
035000130228     d SAVfnrs         s              5  0 inz
035100130228     d Wflsnrs         s              5  0
035200041013     D WCONTRDFV       s              1    inz('N')
035300041105     D WAGBART         s              1    inz(' ')
035400070906     D WNPS            s              2    inz
035500080630     D WOK             s              1    inz
035600170505     D W138            s              1    inz
035700080630     D Secondi         s              9  0 inz
035800040621     D ktrd            s                   like(ar5trd)  inz('GEN')
035900180115     D karstr          s                   like(arstrc)  inz('J')
036000080630     D Timeold         s               t   timfmt(*iso)
036100080630     D Timenew         s               t   timfmt(*iso)
036101180123     d savcan          s                   like(barcan)
036102180126     d w_puc           s                   like(bltpuc)
036103180126     d w_fpc           s                   like(bltfpc)
036104180126     d w_vuc           s                   like(bltvuc)
036105180126     d w_fvc           s                   like(bltfvc)
036200980928     D* DEFINIZIONE COSTANTI
036300980928     D CUTI            C                   CONST('EDP*      ')
036400960423     C*---------------------------------------------------------------*
036500921014     C* RIEPILOGO INDICATORI
036600970115     C*---------------------------------------------------------------*
036700921014     C* 02    - RICHIAMATO
036800930914     C* 03    - POSSO RIABILITARE LA BOLLA
036900921021     C* 05    - MODIFICA DEL SOLO NUMERO DI FOGLIO
037000000505     C* 28/29 - DI COMODO
037100070212     C* 30    - LETTURA FIBAR00F
037200070131     C* 31    - CHAIN FNBRV
037300950927     C* 32    - LETTURA FILE FNBLT FNART FNARB FNBTT
037400950927     C* 33/34 - DI COMODO
037500921014     C* 37    - NON ESISTE BOLLA TRANSITO
037600070131     C* 38    - ERRORE SU WRITE FNBRV e su update
037700921105     C* 39    - RECORD ALLOCATO
037800921014     C* 40    - SE NON RECORD ALLOCATI O ALLOCATI PER 3 VOLTE FINE PGM
037900950926     C* 41    - COMODO
038000030528     C* 45    - Errore su write fnagb
038100040310     C* 46    - lookuip su schiera NTS
038200970204     C* 96    - ESISTE GIA' A MENO L'ANOMALIA CHE DEVO CREARE
038300970204     C* 97    - USATO PER LA CHIUSURA ANOMALIE CON 145 E 146
038400080325     C* 98    - FIBAR allocato --> esco dal pgm
038500080325     C* 99    - DI COMODO
038600921014     C*---------------------------------------------------------------*
038700911016     C     *ENTRY        PLIST
038800070212     C                   PARM                    fnls45ds
038900070208     C                   PARM                    dsrecbrv
039000941214     C*---------------------------------------------------------------*
039100070221     c* tipo di richiamo in base a dls45FLG
039200070208     C*---------------------------------------------------------------*
039300930521     C* FLG = '2' MODIFICATO SOLO NUMERO FOGLIO
039400160624     c
039500160624     c* Eliminato tipo richiamo "6" in quanto eliminate anomalie 60 e 61
039600070221     C* FLG = '6' CREO SOLO ANOMALIA 61 (EXSR CRTA60) DA SPU DISGUIDO
039700160624     c
039800991022     C* FLG = 'D' CREO ANOMALIA DISGUIDO PER COLLO DA ME CONFERMABILE
039900930521     C* FLG = ' ' INSERIMENTO NUOVA SPUNTA O VARIATA SOLO ANOMALIA
040000930521     C* FLG = 'C' CHIUDO PGM CON LR E BASTA
040100050418     C* FLG = 'T' aggiorno bolla transito x disguido
040200070221     c*
040300070221     c* Richiami attualmente usati: '2'  ' '  'T'  'C'
040400070208     C*---------------------------------------------------------------*
040500070208     c* Eaborazioni per chiusura pgm
040600070221    1C     dls45FLG      IFEQ      'C'
040700070208     c                   EXSR      ELACHIUDI
040800040811     c*
040900930521     C                   SETON                                        LR
041000930521     C*
041100930521   X1C                   ELSE
041200941229     C*
041300941229     C* IMPOSTAZIONI INIZIALI
041400941229    2C     UNO           IFEQ      ' '
041500941229     C                   EXSR      CARTAB
041600941229    2C                   END
041700070208     c
041800070208     c* Impostazione di BPES
041900070208     c                   if        dls45pes>*zeros
042000070208     c                   movel     dls45pes      BPES
042100070208     c                   endif
042200070208     c                   if        dls45tfp>*zeros
042300070208     c                   movel     dls45tfp      BTFP
042400070208     c                   endif
042500070208     c
042600070208     c                   EXSR      IMPO
042700930521     C*
042800930521     C                   SETOFF                                       05
042900941229     C                   Z-ADD     1             B                 3 0
043000930521     C****
043100930521     C**** P G M   R I C H I A M A T O
043200930521     C****
043300070208    2C                   if        dls45ric='S'
043400070208     C                   SETON                                        02
043500070208     c
043600070208     C* nel secndo parametro passato ci sono tutti i campi della spunta
043700070208     c*  con la quale effettuare gli aggiornamenti
043800070212     C                   Z-ADD     d_brvnpg      BARNPG
043900070212     C                   Z-ADD     D_brvnfv      BARNFV
044000070212     C                   Z-ADD     d_brvlnp      BARLNP
044100070212     C                   Z-ADD     d_brvlna      BARLNA
044200070212     C                   Z-ADD     d_brvnrs      BARNRS
044300070212     C                   Z-ADD     d_brvnsc      BARNSC
044400070212     C                   MOVEL     d_brvcan      BARCAN
044500070212     C                   Z-ADD     d_brvnps      BARNPS
044600070212     C                   Z-ADD     d_brvvuc      BARVUC
044700070212     C                   Z-ADD     d_brvpuc      BARPUC
044800070212     C                   Z-ADD     d_brvdfs      BARDFS
044900070212     C                   Z-ADD     d_brvhms      BARHMS
045000070212     C                   Z-ADD     d_brvmis      BARmis
045100070212     C                   movel     d_brvtap      BARtap
045200070212     C                   Z-ADD     d_brvznc      BARZNC
045300070212     c                   movel     d_brvpru      barpru
045400070212     c                   movel     d_brvfgs      barfgs
045500101130     c                   movel     d_brvdcs      brvdcs
045600101130     c                   movel     d_brvhcs      brvhcs
045700070208     c
045800960530     C                   CLEAR                   SAVMAC
045900960529     C**
046000070208     c** 05 on - modificato solo numero di foglio viaggio
046100070221    3C     dls45FLG      IFEQ      '2'
046200921229     C                   SETON                                        05
046300020213     C                   ENDIF
046400960524     C*
046500070209     C* CONTROLLO CHE ESISTA COD.ANOMALIA NELLA TABELLA ANOMALIE
046501180124     c                   clear                   savcan
046600020204     C     BARCAN        IFNE      ' '
046700020204     C     BARCAN        ANDNE     ' '
046800020204     C     BARCAN        LOOKUP    CAN                                    34
046801180124     c                   if        not *in34
046802180124     c                   eval      savcan=barcan
046803180124     c                   endif
046900020204     C  N34              CLEAR                   BARCAN
047000020204     C                   ENDIF
047100961213     C*
047200961213     C* LETTURA FOGLIO E IMPOSTAZIONE CAMPI DI COMOD
047300961213     C                   EXSR      BARFGV
047400960529     C**
047500960529     C* SE NON HO TROVATO IL FOGLIO ESCO IMPOSTANDO L'ERRORE
047600960529    3C     BARDFV        IFEQ      0
047700070906     C                   SETON                                        39
047800960529   X3C                   ELSE
047900991230     C     BARLNA        CHAIN     AZORG01L                           31
048000040303    4c                   if        *in31 or
048100040303     c                             orgfag<>'F' and orgfag<>'A'
048200040303     C                   CLEAR                   NTWBAR
048300040303   x4c                   else
048400040303     C                   MOVE      ORGDE3        OG143
048500040303     C                   MOVE      §OGNTW        NTWBAR
048600000523     C*
048700000523     C* SE DEVO CREARE ANOMALIA DI DISGUIDO, CHIAMO SOLTANTO CRTA52
048800070221    5C     dls45FLG      IFEQ      'D'
048900991022     C                   MOVEL     'D'           WTIBOL
049000000523   X5C                   ELSE
049100920424     C*
049200920424     C* SE TROVO RECORD ALLOCATI RITENTO 3 VOLTE
049300000523    6C     B             DOWLE     3
049400950707     C                   MOVE      'S'           OKAGG             1
049500911016     C                   EXSR      UPDARB
049600000523    7C     OKAGG         IFEQ      'N'
049700950707     C                   Z-ADD     4             B
049800000523   X7C                   ELSE
049900021112     C                   Z-ADD     4             B
050000000523    7C                   ENDIF
050100150824     c
050200000523    6C                   ENDDO
050300150824
050400150824     c* Colli che partono dapiù filiali: facico fare la write al
050500150824     c*  chiamante
050600150824     C                   IF        wbsptp<>' ' and wbspspu='S'
050700150824     c                             and wagpar=' '
050800150824     c                   eval      %subst(dls45flo:1:1)='B'
050900150824     c                   endif
051000150824     c
051100040303    5C                   ENDif
051200040303    4C                   ENDIF
051300960410     C*
051400960410     C* PER BOLLA ARRIVO NON LOCALE, VEDO SE CREARE ANOMALIA 50
051500001114     C*  COLLO ARRIVATO NON PARTITO
051600960529     C*  NON ELABORO SE COLLO LOCALE O SOLO DELLA LINEA DI PARTENZA
051700001115     C* (N.B. INFATTI, SE BOLLA ARRIVO LOCALE, NON C'E' TRASMISSIONE DI
051800001115     C*  F.V. E QUINDI QUESTA ROUTINE NON SAREBBE MAI IN GRADO DI
051900001115     C*  CREARE L'ANOMALIA 50  DAL MOMENTO CHE VA A CERCARE IL FOGLIO
052000001115     C*  QUESTO VALE ANCHE PER I COLLI 180 PER 89)
052100020218    5C  N05WTIBOL        IFNE      'P'
052200960530     C     WTIBOL        ANDNE     'S'
052300020214     C     WTIBOL        ANDNE     'R'
052400070221     C     dls45FLG      ANDNE     '6'
052500960410     C                   EXSR      CRTA50
052600000523    5C                   ENDIF
052700000523     C*
052800960530    3C                   ENDIF
052900911016     C                   GOTO      FINE
053000960529     C*
053100960529    2C                   ENDIF
053200930521     C****
053300930521     C**** P G M    N O N    R I C H I A M A T O
053400930521     C****
053500930521     C* APRO FILE
053600040601     c                   clear                   wconta            2 0
053700040601     c                   movel     'S'           wapri             1
053800070212     C                   OPEN(e)   FIBAR00F
053900040601     c* Attendo 5 secondi e riprovo max 9 volte poi esco con errore
054000040601     c                   dow       %error  and wconta<10
054100040601     C                   Z-ADD     15            LUNG
054200040601     C                   MOVEA(p)  CM5           COMMAN           80
054300040601     C                   CALL      'QCMDEXC'                            99
054400040601     C                   PARM                    COMMAN
054500040601     C                   PARM                    LUNG             15 5
054600040601     c
054700040601     c                   add       1             wconta
054800070212     C                   OPEN(e)   FIBAR00F
054900040601     c                   enddo
055000040601     c
055100040601     c* Se ancora errore vado a fine file
055200040601     c                   if        %error
055300040601     c                   eval      wapri='N'
055400040601     c                   goto      fine
055500040601     c                   endif
055600040601     c
055700920424     C                   SETON                                        40
055800941214     C                   Z-ADD     *ZERO         SA1KEY
055900960530     C                   CLEAR                   SAVMAC
056000070208     C                   MOVE      DLS45FLG2     FLGPRE            1
056100911014     C*
056200080325     C                   READ      FIBAR00F                             9830
056300911016     C*
056400930521    2C     *IN40         DOWEQ     '1'
056500080325    2C     *IN98         andeq     *off
056600920424     C                   SETOFF                                       40
056700930521    3C     *IN30         DOWEQ     '0'
056800080325    3C     *IN98         andeq     '0'
056900930602     C*
057000040419     C                   SETOFF                                       3839
057100930521    4C     BARLNP        IFGT      *ZERO
057200040419     C     BARLNA        ANDGT     *ZERO
057300960731     C     BARNSC        ANDGT     *ZERO
057400040224     C     BARNpg        ANDGT     *ZERO
057500070201     c* Per ora non si accettano fogli maggiori di 100.000
057600040601     C     BARNfv        ANDlT     100000
057700070212     c
057800070212     c* Ad ogni record imposto BPES, perchè per le spunte cat 1 di un
057900070212     c*  secondo livello, BPES diventa il terminal per motivi di
058000070212     c*  creazione della anoamlia 200, per cui potrebbe rimanare
058100070212     c*  sporco se dopo ci fossero spunte di altre categorie
058200070212     c                   movel     dls45pes      BPES
058300021015     C                   Z-ADD     BPES          BARFGS
058400070209     c
058500160511      * CLEARO PESO E VOLUME UNITARI SE SUPERIORI AL MAX PREVISTO
058600160511      * limiti per transpallet  (pistola 46)
058700160511      * LIMITI PER RILEVATORE PESO VOLUME PALLET (Pistola 48)
058800160511      * limite per VDL  ( tutte le altre pistole)
058900160511     c                   select
059000160511     c                   when      barnps= 48
059100030723     C     BARPUC        IFGT      §vpoMPP
059200030723     C                   CLEAR                   BARPUC
059300030723     C                   ENDIF
059400030723     C     BARVUC        IFGT      §vpoMVP
059500030723     C                   CLEAR                   BARVUC
059600030723     C                   ENDIF
059700160511     c
059800160511     c                   when      barnps= 46
059900160511     C     BARPUC        IFGT      §vpoMPT
060000160511     C                   CLEAR                   BARPUC
060100160511     C                   ENDIF
060200160511     c                   other
060300160511     C     BARPUC        IFGT      §vpoMPV
060400160511     C                   CLEAR                   BARPUC
060500160511     C                   ENDIF
060600160511     C     BARVUC        IFGT      §vpoMVV
060700160511     C                   CLEAR                   BARVUC
060800160511     C                   ENDIF
060900160511     C                   Endsl
061000160511     c
061100040805      * CLEARO PESO E VOLUME UNITARI SE INFERIORI AL MINIMO PREVISTO
061200040805      * I LIMITI VALGONO PER TUTTI I TIPI DI IMPIANTO
061300040805     C     BARPUC        IFLT      §vpoSPV
061400040805     C                   CLEAR                   BARPUC
061500040805     C                   ENDIF
061600040805     C     BARVUC        IFLT      §vpoSVV
061700040805     C                   CLEAR                   BARVUC
061800040805     C                   ENDIF
061900020204     C* CONTROLLO CHE ESISTA COD,ANOMALIA NELLA TABELLA ANOMALIE
061901180124     c                   clear                   savcan
062000020204     C     BARCAN        IFNE      ' '
062100020204     C     BARCAN        ANDNE     ' '
062200020204     C     BARCAN        LOOKUP    CAN                                    34
062201180124     c                   if        not *in34
062202180124     c                   eval      savcan=barcan
062203180124     c                   endif
062300020204     C  N34              CLEAR                   BARCAN
062400020204     C                   ENDIF
062500040419     c
062600070906     c* Non carico se LNP inesistente
062700040419     C     BARLNP        CHAIN     AZORG01L                           31
062800040419     c* Non carico se LNA inesistente
062900070906   4bC     *IN31         IFEQ      *Off
063000040419     c     orgfag        andeq     'A'
063100040419     C     *IN31         oreq      *Off
063200040419     c     orgfag        andeq     'F'
063300980212     C*
063400991230     C     BARLNA        CHAIN     AZORG01L                           31
063500040419     c* non carico se LNA inesistente
063600040303    5C     *IN31         IFEQ      *ON
063700040224     c     orgfag        orne      'A'
063800040224     c     orgfag        andne     'F'
063900020212     C                   CLEAR                   NTWBAR
064000040303   x5C                   ELSE
064100020212     C                   MOVEL     ORGDE3        OG143
064200020212     C                   MOVEL     §OGNTW        NTWBAR
064300070912     c
064400070912     C* LETTURA FOGLIO DELLA SPUNTA E IMPOSTAZIONE CAMPI DI COMOD
064500070912     C                   EXSR      BARFGV
064600070912     c
064700070912     c* Non carico se numero foglio inesistente
064800070912   5ac                   if        bardfv>0
064900140616     c                   clear                   amb_QD            1
065000140613     c                   clear                   A_imm
065100140613     c                   clear                   B_imm
065200140613     c*
065300140613     c* se IMA e spunta consegna PIu' RECENTE --> cancello senza caricare
065400140613     c                   if        barnpg=3 and barspg='A'
065500140616     c                   eval      kdel_npg=4
065600140613     c                   exsr      Cer_cons
065700140613     c                   endif
065800140616     c                   if        barnpg=4
065900140616     c                   eval      kdel_npg=3
066000140616     c                   exsr      Cer_cons
066100140616     c                   endif
066200140613
066300140616   5cc                   if        amb_QD<>'N'
066400040303     c**
066500040303     C* VERIFICO IL FLAG NELLA DS7N
066600040303     C                   Z-ADD     1             WA                3 0
066700040310     C     BARNPG        LOOKUP    CAT(WA)                                46
066800040310    6C     *IN46         IFEQ      *OFF
066900040303     C     NST(WA)       ORNE      '1'
067000040303     c* Se tengo tutte le spunte, verifico spunta doppia per lo stesso NFV
067100070131     C     KBAR1         CHAIN     FNBRV01L                           31
067200040303   x6c                   else
067300911016     C*
067400040303     C* spunta doppia per stessa categoria e p.o. gestione
067500140613     C     KBAR2         CHAIN     FNBRV05L                           31
067600040303    6c                   endif
067700941229     C*
067800040303     C* Aggiorno bolle e spunta se:
067900040303     C* - spunta nuova (31 on)
068000040303     C* - spunta già esistente ma dati del record diversi(BAR1<>BRV)
068100040303     C* - spunta già presente ma nel giro prec.trovati record allocati(B>1)
068200070212     c* - trovati errori  (BARERR=E)
068300070912   5bC     *IN31         IFEQ      *ON
068400941229     C     BAR1          ORNE      BRV
068500001114     C     B             ORGT      1
068600070212     C     BARERR        OReq      'E'
068700040303     c**
068800040303     C* OPERAZIONI PER SPUNTA GIA' PRESENTE e da tenerne una
068900040303    6C     *IN31         IFEQ      *OFF
069000040303     C     nst(WA)       ANDEQ     '1'
069100010608     C     BAR1          ANDNE     BRV
069200010608     C                   EXSR      SRBRVE
069300040303    6C                   ENDIF
069400070710     c**
069500070710     C* OPERAZIONI PER SPUNTA GIA' PRESENTE e da tenerne una
069600070710    6C     *IN31         IFEQ      *OFF
069700070710     C     nst(WA)       ANDne     '1'
069800070710     C     BAR1          ANDNE     BRV
069900070710     c* la precedente spunta non ha l'anomalia
070000070710     c     brvcan        andeq     ' '
070100070710     c* nell'attuale è stata inserita
070200070710     c     barcan        andne     ' '
070300070822     c*  Carico solo se data/ora Immissione spunta sono < della precedente
070400070822     c     barimmis      andgt     brvimmis
070500070710     C                   EXSR      SRBRVE
070600070710    6C                   ENDIF
070700040303     C* RIEMPI I CAMPI
070800040303     C                   EXSR      RIEMPI
070900040303     C**
071000040303     C**  A G G I O R N A M E N T I
071100961213     C                   MOVE      'S'           OKAGG             1
071200911112     C                   EXSR      UPDARB
071300911014     C**
071400920424     C**  AGGIORNO SPUNTA
071500950707     C*   Non l'aggiorno se spunta partenza manuale e di collo già
071600950707     C*   consegnato
071700040303    6C     OKAGG         IFEQ      'S'
071800040303     C*
071900960521     C* SE NO  LA DEVO TRASMETTERE LA FLAGGO
072000961213    7C     WNOTRA        IFEQ      'S'
072100960521     C                   MOVEL     'N'           BRVFTR
072200960521     C                   MOVEL     *HIVAL        BRVDTR
072300991223     C                   MOVEL     *HIVAL        BRVHTR
072400960521     C                   CLEAR                   WNOTRA
072500961213    7C                   ENDIF
072600980115     C* SE SPUNTA PARTENZA RELATIVA AD UN FOGLIO VIAGGIO SU CUI NON E'
072700980115     C* PIU' PRESENTE LA LNA DEL COLLO -> FLAGGO LA SPUNTA
072800980205     C* (RIENTRA IN QUESTO CASO ANCHE UNA SPUNTA CON COD.ANOMALIA "E"
072900980205     C* CIOE' UNA SPUNTA LA CUI LINEA NON ERA PRESENTE SUL FV GIA' NEL
073000980205     C* MOMENTO IN CUI E' STATA FATTA)
073100040303    7C     BARNPG        IFEQ      1
073200980115     C                   MOVE      BARLNA        W003A             3
073300980313     C     W003A         LOOKUP    FFS                                    33
073400980115     C  N33              MOVEL     'L'           BRVFTR
073500980115     C  N33              MOVEL     *HIVAL        BRVDTR
073600991223     C  N33              MOVEL     *HIVAL        BRVHTR
073700980210     C   33BRVCAN        IFEQ      'E'
073800980210     C                   CLEAR                   BRVCAN
073900980210     C                   END
074000041020    7C                   ENDIF
074100041020     c
074200041020     c* Imposto brvatr='D' per disguido
074300041020     c                   if        wtibol='D'
074400041020     c                   eval      brvatr='D'
074500041020     c                   else
074600041020     c                   clear                   brvatr
074700041020    7C                   ENDIF
074800040303     c**
074900070131     C* AGGIORNO LF FNBRV01L
075000040310    7C  N31*IN46         IFEQ      *OFF
075100040303     C     NST(WA)       ORNE      '1'
075200070131     C                   UPDATE    FNBRV000
075300040303     C                   ELSE
075400140613     C* AGGIORNO LF FNBRV05L
075500140613     C                   UPDATE    FNBRV005
075600040303    7C                   END
075700010607     C*
075800070131     C   31              WRITE     FNBRV000                             38
075900140613     c
076000140613     c* Se previsto cancello la spunta "doppia" tra 3 e 4
076100140616    7c                   if        amb_QD='S'
076200140616     c                   exsr      Canc_QD
076300140616    7c                   endif
076400150824     c**
076500150824     c*  COLLO SPUNTABILE DA PIÙ FILIALI
076600150824     c**
076700150824     c** elaboro per "K-cli" con BPES <> LNPD ( in qeusto caso ho pulito
076800150824     c*                                        AGGPAR)
076900150824     c*  oppure  per "L-linea" se TFPLNP <>TFPPES
077000150824     c* lo faccio dopo la write di BRV perchè fnlv52r deve leggere la spunta per
077100150824     c*  determinare la nuova lnp della bolla
077200150824     C                   IF        wbsptp<>' ' and wbspspu='S'
077300150824     c                             and wagpar=' '
077400150824     c                   exsr      DA_BSP
077500150824     c                   endif
077600140613     c
077700040303   X6C                   ELSE
077800040303     C*
077900040310    7C  N31*IN46         IFEQ      *OFF
078000040303     C     NST(WA)       ORNE      '1'
078100070131     C                   UNLOCK    FNBRV01L
078200040303     C                   ELSE
078300140613     C                   UNLOCK    FNBRV05L
078400040303    7C                   END
078500040303    6C                   END
078600950927     C* PER BOLLA ARRIVO NON LOCALE, VEDO SE CREARE ANOMALIA 50
078700950927     C*  COLLO ARRIVATO NON PARTITO
078800960207     C*  NON ELABORO SE COLLO LOCALE O SOLO DELLA LINEA DI PARTENZA
078900020214    6C  N05WTIBOL        IFEQ      'D'
079000020214     C     WTIBOL        OREQ      'A'
079100020214     C     WTIBOL        OREQ      'T'
079200020218     C     WTIBOL        OREQ      'L'
079300950927     C                   EXSR      CRTA50
079400950927    6C                   ENDIF
079500950927     C*
079600070912   5bC                   ELSE
079700040303     C*
079800040310    6C  N31*IN46         IFEQ      *OFF
079900040303     C     NST(WA)       ORNE      '1'
080000070131     C                   UNLOCK    FNBRV01L
080100040303     C                   ELSE
080200140613     C                   UNLOCK    FNBRV05L
080300040303    6C                   END
080400070912   5bC                   END
080500140613   5cC                   END
080600110103     c*
080700070912   5ac                   else
080800070912     c* Scrivo la spunta errata in fibar00e
080900070912     c                   movel     bpes          barpes
081000070912     c                   movel     wdate1        bardim
081001180126     c                   eval      barcan=savcan
081100070912     c                   write     barcoerr                             99
081200070912   5aC                   END
081300040303    5C                   END
081400070906   4bC                   END
081500040419    4C                   END
081600911112     C*
081700900619     C* CANCELLO RECORD LETTO
081800001114     C* Se spunta imi e non ho aggiornato blt perchè inesistente o se
081900021203     C* non ho aggiornato blp perchè record allocato la spunta non viene
082000001114     C* cancellata.
082100021107     c                   if        not *in38 and not *in39
082200021107     C                   DELETE    BARCO
082300021107     c                   else
082400021203     c* Se ci fosse errore ma passati 4 giorni dalla data di immissione
082500021203     c*  spunte, deleto lo stesso
082600021203     c                   if        brvdfs<=wpul050
082700021203     c                   DELETE    BARCO
082800021203     c                   else
082900070212     c                   eval      barerr='E'
082901180126     c                   eval      barcan=savcan
083000021107     c                   update    BARCO
083100021107     c                   endif
083200021203     c                   endif
083300900618     C*
083400080325     C                   READ      FIBAR00F                             9830
083500930521    3C                   END
083600021112     C** 40              ADD       1             B
083700021112     C   40              z-add     4             B
083800920424     C* DOPO 3 VOLTE ESCO
083900930521    3C   40B             IFGT      3
084000920424     C                   SETOFF                                       40
084100930521   X3C                   ELSE
084200070212     C                   CLOSE     FIBAR00F
084300070212     C                   OPEN      FIBAR00F
084400070212     C                   READ      FIBAR00F                               30
084500930521    3C                   END
084600930521    2C                   END
084700970224     C* ELABORO L'ULTIMO DATO MEMORIZZATO
084800970224     C                   EXSR      WRTAGB
084900911016     C*
085000911016     C     FINE          TAG
085100930521     C** 02 OFF - PROGRAMMA NON RICHIAMATO
085200930521     C**
085300950926     C** LANCIO PGM DI AGGIORNAMENTO BOLLE
085400961212    2C     *IN02         IFEQ      *OFF
085500970814     C**
085600970814     C                   CLEAR                   DSLV55
085700970814     C                   MOVEL     'C'           D55TLA
085800020213     C                   EXSR      FNLV55
085900000705     C**
086000000705     C                   CLEAR                   DSLV53
086100000705     C                   MOVEL     'C'           D53TLA
086200000705     C                   CALL      'FNLV53R'
086300000705     C                   PARM                    DSLV53
086400930521     C*
086500040601     c                   if        wapri='S'
086600070212     C                   CLOSE     FIBAR00F
086700040601     c                   endif
086800930521     C*
086900981105     C* SE HO ALEMNO UN FOGLIO SCRITTO--> TRASMETTO
087000111018     C***  ERF(1)        IFNE      *BLANKS
087100111018     C***                EXSR      TRASMI
087200111018     C***                ENDIF
087300040811     c*
087400040811     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
087500040811     c                   clear                   fidn12ds
087600040811     c                   eval      i12tla = 'C'
087700040811     c                   call      'FIDN12R'
087800040811     c                   parm                    fidn12ds
087900040811     c*
088000930521     C                   SETON                                        LR
088100930521     C*
088200930521   X2C                   ELSE
088300930521     C**
088400930521     C** 02 ON  - PROGRAMMA RICHIAMATO
088500930521     C**
088600921020     C* SE NON HO AGGIORNATO IMPOSTO LA "E" DI ERRORE
088700960521     C     *IN39         IFEQ      *ON
088800070221     C                   MOVEL     'E'           dls45FLG
088900960521     C                   ELSE
089000960521     C*
089100950926     C* SE AGGIORNATO FACCIO SOTTOMETTERE IL PGM DI AGGIORNAMENTO BOLLE
089200070221     C                   MOVEL     'A'           dls45FLG
089300950707     C* Se non ho aggiornato perchè spunta partenza manuale di collo
089400950707     C* consegnato imposto "N" di aggiornamento non effettuato
089500960521     C     OKAGG         IFEQ      'N'
089600070221     C                   MOVE      'N'           dls45FLG
089700950707     C                   END
089800960521     C                   ENDIF
089900070208     C                   MOVE      DLS45FLG2     FLGPRE            1
090000930521     C*
090100930521     C                   RETURN
090200930521    2C                   END
090300930521    1C                   END
090400941214     C**************************************************************************
090500941214     C* RILEVO IL FOGLIO VIAGGIO PARTENZE/ARRIVI
090600941214     C**************************************************************************
090700941214     C     RILFOG        BEGSR
090800941214     C*
090900941214     C                   Z-ADD     *ZERO         WDFV
091000941229     C                   CLEAR                   WSPG
091100021014     C                   Z-ADD     0             WFLE
091200961122     C                   CLEAR                   WTTR
091300980115     C                   CLEAR                   FFG
091400140613     C                   CLEAR                   WFCF
091500021024     c*
091600021024     c* Prendo il terminal di partenza del p.o. in gestione
091700070212    1c                   if        savfgs<>wfgs
091800021024     C                   CLEAR                   DSLV55
091900021024     C                   MOVEL     'P'           D55TPT
092000021024     C                   MOVEL     WFGS          D55LIN
092100021024     C                   MOVEL     WDATE1        D55DRF
092200021024     C                   CALL      'FNLV55R'
092300021024     C                   PARM                    DSLV55
092400021024     C**
092500070212    2C     D55ERR        IFne      ' '
092600021024     C                   MOVE      wfgs          tfpfgs
092700021024     c                   else
092800021024     C                   MOVE      d55tfp        tfpfgs
092900070212    2c                   endif
093000021024     c                   eval      savfgs=wfgs
093100070212    1c                   endif
093200941214     C*
093300941214     C* ARRIVI O PARTENZE ?
093400970114    1C     WNPG          IFEQ      1
093500941214     C*
093600021014     C     KFGV1         CHAIN     FNFGV01L                           99
093700000505     C*
093800070209    2C     *IN99         IFEQ      *OFF
093900021014     C     KFGV1         CHAIN     FNFGW01L                           28
094000070209    3C     *IN28         IFEQ      *ON
094100000505     C     FGWATB        ORNE      ' '
094200000505     C                   CLEAR                   FGWFF3
094300000505     C                   CLEAR                   FGWFF4
094400000505     C                   CLEAR                   FGWFL3
094500000505     C                   CLEAR                   FGWFL4
094600070209    3C                   ENDIF
094700980928     C                   Z-ADD     FGVDFV        WDFV
094800021024     C                   Z-ADD     FGVLNP        WFgs
094900021024     C                   Z-ADD     FGVLNP        WFLE
095000980928     C                   MOVEL     FGVTTR        WTTR
095100140613     C                   MOVEL     FGVFCF        WFCF
095200980928     C**
095300070209   X2C                   ELSE
095400960925     C     KFVA1         CHAIN     FNFVA01L                           99
095500970114    3C     *IN99         IFEQ      *OFF
095600970114     C                   Z-ADD     FVADFV        WDFV
095700021014     C                   Z-ADD     FVALNP        WFLE
095800970114     C                   MOVEL     FVATTR        WTTR
095900140613     C                   MOVEL     'S'           WFCF
096000111018   x3C**                 ELSE
096100110103     c*
096200070209     C* Se non trovato invio msg di errore
096300111018     C**                 EXSR      INVMSG
096400070212    3C                   ENDIF
096500070212    2C                   ENDIF
096600941214     C*
096700970114   X1C                   ELSE
096800941214     C*
096900021014     C* ACCEDO DIRETTAMENTE AL FOGLIO CON P.O. GESTIONE
097000021014     C* SE FOGLIO NON TROVATO CERCO CON SIMFEL- PUÒ ESSERE SOLO UN
097100070212     C*  FOGLIO IMP SOLO SE RICHIAMO PER DATI DI FIBAR!!
097200021014     C     KFVV1         CHAIN     FNFVV01L                           99
097300070209    2C     *IN99         IFEQ      *ON
097400021014     C     WNPG          ANDEQ     3
097500021014     C     RICBAR        ANDEQ     'S'
097600021014     C                   Z-ADD     SIMFEL        WFGS
097700021014     C     KFVV1         CHAIN     FNFVV01L                           99
097800021014     C  N99FVVSPG        IFNE      'P'
097900021014     C                   SETON                                        99
098000021014     C                   ENDIF
098100070209    2C                   ENDIF
098200021014     C*
098300070209    2C     *IN99         IFEQ      *OFF
098400980928     C                   Z-ADD     FVVDFV        WDFV
098500980928     C                   MOVEL     FVVSPG        WSPG
098600021014     C                   MOVEL     FVVFGS        WFGS
098700140613     C                   MOVEL     FVVFCF        WFCF
098800111018     C**                 ELSE
098900111018     C**                 EXSR      INVMSG
099000070209    2C                   ENDIF
099100941214     C*
099200970114    1C                   ENDIF
099300021014     C**
099400021024     C* SE WFLE=0 prendo TERMINAL DI PARTENZA di fgs A UDATE
099500070223     c*   di fatto si tratta del TFP di BPES poteva che può differire
099600070223     c*   dal FGS solo in caso di spunte ion procedura arrivo per p.o.
099700070223     c*   con terminal di partenza <> dal terminal arrivo
099800021014    1C     WFLE          IFEQ      0
099900021024     C                   MOVE      tfpfgs        WFLE
100000021014    1C                   END
100100941214     C*
100200941214     C                   ENDSR
100300020213     C**************************************************************************
100400020213     C*    IMPOSTAZIONI DI ALCUNI CAMPI DI PASSAGGIO SE VUOTI
100500020213     C**************************************************************************
100600020213     C     IMPO          BEGSR
100700150824     c                   eval      %subst(dls45flo:1:1)=' '
100800110103     c*
100900070208     C* BPES--> SE NON passato    E' SIMFEL
101000020213     C     BPES          IFEQ      0
101100020213     C                   Z-ADD     SIMFEL        BPES
101200070208     c                   if        btfp=0
101300070208     C                   Z-ADD     SIMFEL        BTFP
101400020213     C                   ENDIF
101500070208     C                   ENDIF
101600020213     C**
101700070208     C* BTFP--> SE NON passato ma c'è BPES, è terminal partenza di BPES
101800070208     c                   if        BTFP=0
101900020213     C                   CLEAR                   DSLV55
102000020213     C                   MOVEL     'P'           D55TPT
102100020213     C                   Z-ADD     BPES          D55LIN
102200020213     C                   Z-ADD     WDATE1        D55DRF
102300020213     C                   EXSR      FNLV55
102400110103     c*
102500020213     C                   Z-ADD     D55TFP        BTFP
102600020213     C                   ENDIF
102700110103     c*
102800070209     c* Se BPES non passato, reimposto anche il campo di input
102900070212     c                   if        DLS45PES<=*zeros
103000070209     c                   movel     bpes          dls45pes
103100070209     c                   endif
103200110103     c*
103300020213     C                   ENDSR
103400961213     C**************************************************************************
103500961213     C*    RIEMPI I CAMPI DEL RECORD DA SCRIVERE
103600961213     C**************************************************************************
103700961213     C     RIEMPI        BEGSR
103800110103     c*
103900080317     C* CONTROLLO SE E' PISTOLA MANUALE
104000080317     C                   EXSR      PISMAN
104100080317     c
104200961213     C* SE NON C'E' AGGIUNGO ALTRIMENTI AGGIORNO
104300080317    1C     WBRVE         IFEQ      'S'
104400070131     C* Se routine richiamata per scrittura di FNBRVe devo sempre clear
104500010608     c* are brvatr
104600010608     C                   CLEAR                   BRVATR
104700070201     C                   CLEAR                   BRVtsu
104800080317   x1C                   ELSE
104900961213     C   31              CLEAR                   BRVATR
105000070201     C   31              CLEAR                   BRVtsu
105100110103     c*
105200080317     c** Conteggio le spunte risparate + volte
105300080317     c** ignoro tutte le spunte manuali
105400100709     c** ignoro se spunta con errore (già caricata)
105500080422    1c                   if        not *in31  and (rps(y)<>'M' or
105600080422     c                             rps(y)='M' and tps(y)='C')
105700100709     c                             and barerr<>'E'
105800110103     c*
105900080317    2c                   if        nst(WA)<>'1'  or
106000080317     c* Se spunta unica  ma stesso numero foglio --> conto doppio
106100080317     c                             (nst(WA)= '1' and brvnfv=barnfv)
106200110103     c*
106300080630     c* Non considero doppia nemmeno se spunta cat 3 o 4 pistola 49
106400080630     c*  sparate entro i 2 sec.
106500080630     c                   eval      wok='S'
106600080630    3c                   if        (barnpg=3 or barnpg=4) and barnps=49
106700080630     c                             and brvnps=49 and brvdfs=bardfs
106800080630     c                   move      barhms        Timenew
106900080630     c                   move      brvhms        Timeold
107000080630     c     Timenew       subdur    Timeold       secondi:*s
107100080701     c                   mllzo     1             secondi
107200080630    4c                   if        secondi<=§vposecdop
107300080630     c                   eval      wok='N'
107400080630    4c                   endif
107500080630    3c                   endif
107600110103     c*
107700080630   2AC                   IF        WOK='S'
107800080317    3c                   if        brvtsu=' '
107900080317     c                   eval      brvtsu='1'
108000080317   x3c                   else
108100080317     c                   movel     brvtsu        wtsu              1 0
108200080317    4c                   if        wtsu<9
108300080317     c                   eval      wtsu=wtsu+1
108400080317     c                   movel     wtsu          brvtsu
108500080317    4c                   endif
108600110103     c*
108700080317    3c                   endif
108800080630   2Ac                   endif
108900110103     c*
109000080317   x2c                   else
109100080317     c* Prendo data foglio
109200080317    3C     BRvKEY        IFNE      SA1KEY
109300080317     C                   MOVEL     BRvKEY        SA1KEY
109400080317     C                   Z-ADD     BRvNPG        WNPG
109500080317     C                   Z-ADD     BRvNFV        WNFV
109600080317     C                   Z-ADD     BRvFGS        WFGS
109700080317     C                   movel     BRvnps        Wnps
109800080317     C                   EXSR      RILFOG
109900080317     c                   Z-ADD     WDFV          BRVDFV
110000140616     c                   movel     WFCF          BRVFCF
110100140616     c                   movel     Wspg          BRVspg
110200080317    3c                   endif
110300110103     c*
110400080317     c* Se spunta unica, foglio diverso ma stessa data -->
110500080317     c*                         mantengo TSU
110600080317     c*                                    data diversa --> lo pulisco
110700080317    3c                   if        bardfv<>brvdfv
110800080317     c                   clear                   brvtsu
110900080317    3c                   endif
111000110103     c*
111100080317    2c                   endif
111200080317    1c                   endif
111300110103     c*
111400080317    1C                   ENDIF
111500110103     c*
111600070201     C                   CLEAR                   BRVnvr
111700961213     C* FILIALE GESTIONE LA PRENDO DAL FOGLIO
111800961213     C                   CLEAR                   BRVDTR
111900961213     C                   CLEAR                   BRVFTR
112000991223     C                   CLEAR                   BRVHTR
112100961213     C                   Z-ADD     BARFGS        BRVFGS
112200961213     C                   Z-ADD     BARNPG        BRVNPG
112300990708     C                   Z-ADD     BARNFV        BRVNFV
112400040428     C* P.O. CHE HA EFFETTUATO LA SPUNTA (l'originale contenuto il WPES
112500040428     c*  e non più il bpes, per le spunte cat.1)
112600040428     C                   Z-ADD     WPES          BRVPES
112700961213     C                   MOVEL     BARLNP        BRVLNP
112800961213     C                   MOVEL     BARLNA        BRVLNA
112900961213     C                   MOVEL     BARNRS        BRVNRS
113000961213     C                   MOVEL     BARNSC        BRVNSC
113100961213     C                   Z-ADD     BARZNC        BRVZNC
113200961213     C**
113300961213     C* SE SONO IN IMMISSIONE LA AGGIORNO, IN VARIAZIONE SOLO
113400970829     C*  SE E' PISTOLA CON PRIORITA' < O UGUALE ALLA PRESENTE
113500961213     C*
113600961213     C* SE L'ANOMALIA GIA' ESISTE, NON LA TOLGO SE SU QUESTA SPUNTA
113700961213     C*  NON C'E'
113800040324    1C     *IN31         IFEQ      *ON
113900010608     C     WBRVE         OREQ      'S'
114000961213     C                   MOVEL     BARCAN        BRVCAN
114100961213     C                   MOVEL     BARNPS        BRVNPS
114200070212     C                   MOVEL     BARtap        BRVtap
114300040324     C                   Z-ADD     BARVUC        BRVVUC
114400040324     C                   Z-ADD     BARPUC        BRVPUC
114500040324   x1C                   ELSE
114600961213     C* CODICE ANOMALIA
114700961213     C     BARCAN        IFNE      ' '
114800980205     C     BRVCAN        OREQ      'E'
114900961213     C                   MOVEL     BARCAN        BRVCAN
115000961213     C                   ENDIF
115100110103     c*
115200970829     C* CERCO LA PRIORITA' DELLA PISTOLA GIA' PRESENTE
115300970829     C                   MOVEL     BRVNPS        ALF2
115400970829     C                   EXSR      RICRAG
115500970829     C* PISTOLA CON PRIORITA' < O = ALLA PRESENTE: E' + IMPORTANTE
115600970829     C     OPS(Y)        IFLE      OPS(Z)
115700961213     C                   MOVEL     BARNPS        BRVNPS
115800070212     C                   MOVEL     BARtap        BRVtap
115900961213     C                   ELSE
116000961213     C                   MOVEL     BRVNPS        BARNPS
116100070212     C                   MOVEL     BRVtap        BARtap
116200961213     C                   ENDIF
116300110103     c*
116400041011     c* Peso e volume prendo se pieno e se non c'e' già
116500041011     c                   if        barpuc>0
116600041011     c                   if        *in31 or (not *in31 and brvpuc=0)
116700040324     C                   Z-ADD     BARPUC        BRVPUC
116800040324     c                   endif
116900041011     c                   endif
117000040324     c                   if        barvuc>0
117100041011     c                   if        *in31 or (not *in31 and brvvuc=0)
117200040324     C                   Z-ADD     BARvUC        BRVvUC
117300040324     c                   endif
117400041011     c                   endif
117500110103     c*
117600040324    1C                   ENDIF
117700961213     C*
117800070212     c* Se la spunta era già stata caricata con errore (di solito
117900070212     c*  per la non creazione della anomalia 50) non aggiorno la data/ora
118000070212     c*  caricamento spunta
118100070212     c                   if        barerr<>'E' or *in31
118200961213     C                   Z-ADD     WDATE1        BRVDCS
118300070131     C                   time                    BRVHCS
118400070212     c                   endif
118500140613     c* impostazione data caricamento spunte
118600140613     c                   exsr      impoDFS
118700140613     c                   z-add     wdfs          brvdfs
118800140613     c
118900070131     c                   clear                   brvhms
119000070131     C                   movel     BARHMS        BRVHMS
119100070212     c                   z-add     barmis        brvmis
119200961213     C                   Z-ADD     BARFLE        BRVFLE
119300070212     C                   movel     BARpru        BRVpru
119400080314     c
119500080314     c
119600961213     C                   ENDSR
119700941206     C**************************************************************************
119800941206     C*    CARICO TABELLE
119900941206     C**************************************************************************
120000941229     C     CARTAB        BEGSR
120100930521     C*
120200930521     C* PARAMETRI UTENTE
120300110103     C                   Z-ADD     1             CODUT             1 0
120400110103      *
120500110103      * Reperimento dati utente/aziendali
120600110103     c                   exsr      sr_DatiJob
120700970320     C*
120800970320     C                   CLEAR                   SAVBOL
120900961212     C**
121000961212     C**  CARICO TABELLA 7J - CODICI PISTOLA
121100961122    1C     *IN02         IFEQ      '0'
121200921229     C                   MOVEL     '7J'          COD
121300990108     C                   Z-ADD     1             X                 4 0
121400921229     C     KTAB2         SETLL     TABEL
121500950927     C     KTAB2         READE     TABEL                                  99
121600921229     C*
121700961122    2C     *IN99         DOWEQ     *OFF
121800961122    3C     TBLFLG        IFEQ      ' '
121900921229     C                   MOVEL     TBLKEY        NPS(X)
122000921229     C                   MOVEL     TBLUNI        DS7J
122100921229     C                   MOVEL     §7JRPS        RPS(X)
122200000210     C                   MOVEL     §7JTPS        TPS(X)
122300000210     C                   MOVEL     §7JORD        OPS(X)
122400000316     C                   MOVEL     §7JINV        INV(X)
122500040503     C                   MOVEL     §7Jfsl        fsl(X)
122600921229     C                   ADD       1             X
122700961122    3C                   END
122800921229     C*
122900950927     C     KTAB2         READE     TABEL                                  99
123000961122    2C                   ENDDO
123100961122    1C                   ENDIF
123200961122     C**
123300961212     C** CARICO TABELLA  TV - TIPI TRAINO DEFLUENZA
123400961122     C                   MOVEL     'TV'          COD
123500990108     C                   Z-ADD     1             X
123600961122     C     KTAB2         SETLL     TABEL
123700961122     C     KTAB2         READE     TABEL                                  99
123800961122     C*
123900961122    2C     *IN99         DOWEQ     *OFF
124000961122    3C     TBLFLG        IFEQ      ' '
124100961122     C                   MOVEL     TBLUNI        DSTV
124200961122    4C     §TVDEF        IFEQ      'S'
124300961122     C                   MOVEL     TBLKEY        TTR(X)
124400961122     C                   ADD       1             X
124500961122    4C                   ENDIF
124600961122    3C                   ENDIF
124700961122     C*
124800961122     C     KTAB2         READE     TABEL                                  99
124900961122    2C                   ENDDO
125000961212     C**
125100961212     C* CARICO LA TABELLA 7N - DELLE CATEGORIE SPUNTE
125200941214     C                   MOVEL     '7N'          COD
125300941214     C                   Z-ADD     1             X
125400941214     C     KTAB2         SETLL     TABEL
125500950927     C     KTAB2         READE     TABEL                                  99
125600941214     C*
125700950927     C     *IN99         DOWEQ     '0'
125800941214     C     TBLFLG        IFEQ      ' '
125900941214     C                   MOVEL     TBLKEY        CAT(X)
126000941214     C                   MOVEL     TBLUNI        DS7N
126100941214     C                   MOVEL     §7NNST        NST(X)
126200941214     C                   ADD       1             X
126300941214     C                   ENDIF
126400941214     C*
126500950927     C     KTAB2         READE     TABEL                                  99
126600941214     C                   ENDDO
126700961212     C**
126800960702     C** CARICO TABELLA 5F - FILIALI CONFERMA BOLLE DA DISK
126900960702     C                   MOVEL     '5F'          COD
127000960702     C                   Z-ADD     0             X
127100960702     C     KTAB2         SETLL     TABEL
127200960702     C     KTAB2         READE     TABEL                                  99
127300960702     C*
127400960702    1C     *IN99         DOWEQ     '0'
127500960702    2C     TBLFLG        IFEQ      ' '
127600070201     C                   MOVEL     TBLKEY        W003A             3
127700070201     C     W003A         IFNE      '   '
127800960702     C                   ADD       1             X
127900960702     C                   MOVEL     TBLKEY        S5F(X)
128000970828     C* MEMORIZZO LA FIL CHE CONFERMA
128100970828     C                   MOVEL     TBLUNI        F5F(X)
128200110103     c*
128300130618     C     X             IFGE      9000
128400070201     C                   SETON                                        99
128500070201     C                   ENDIF
128600110103     c*
128700970828     C                   ENDIF
128800960702    2C                   ENDIF
128900960702     C*
129000960702     C  N99KTAB2         READE     TABEL                                  99
129100960702    1C                   ENDDO
129200961212     C**
129300020204     C** CARICO TABELLA 3E - CODICI ANOMALIA VALIDI SU SPUNTE
129400020204     C                   MOVEL     '3E'          COD
129500961212     C                   Z-ADD     0             X
129600961212     C     KTAB2         SETLL     TABEL
129700961212     C     KTAB2         READE     TABEL                                  99
129800961212     C*
129900961212    1C     *IN99         DOWEQ     *OFF
130000961212    2C     TBLFLG        IFEQ      ' '
130001180123     c* scarto le anomalie di segnalazione che non vengono memorizzate
130002180123     c* sulla spunta
130003180125     c                   eval      ds3e=tbluni
130004180125     c     §3efta        ifne      'S'
130100961212     C                   ADD       1             X
130200020204     C                   MOVEL     TBLKEY        CAN(X)
130300961212     C     X             IFGE      200
130400961212     C                   SETON                                        99
130500961212     C                   ENDIF
130501180125     c                   endif
130600961212    2C                   ENDIF
130700961212     C*
130800961212     C  N99KTAB2         READE     TABEL                                  99
130900961212    1C                   ENDDO
131000020502     C*****
131100020502     C* RILEVO DATA E ORA PER SCRIVERLO NELLE SPUNTE
131200020502     C                   TIME                    WTIME            14 0
131300020502     C                   MOVE      WTIME         WDATE             8 0
131400020502     C                   Z-ADD     WDATE         G02DAT
131500020502     C                   Z-ADD     *ZERO         G02INV
131600020502     C                   MOVEL     *BLANKS       G02ERR
131700020502     C                   CALL      'XSRDA8'
131800020502     C                   PARM                    WLBDAT
131900020502     C                   Z-ADD     G02INV        WDATE1            8 0
132000030725      *
132100030725      * CHAIN A VARIABILI PARTENZE x PRENDERE MAX PESO E VOLUME CARICABILI
132200030725     C                   EXSR      CERVPO
132300970522     C*
132400970522     C* CHAIN A VARIABILI ARRIVI PER VEDERE A QUALE LIVELLO SI CHIUDE
132500970522     C* L'IDD (LIV MAX.)
132600970522     C                   MOVEL     '7A'          COD
132700970522     C                   MOVEL(P)  '2'           KEY
132800970522     C     KTAB          CHAIN     TABEL                              99
132900970522     C  N99              MOVEL     TBLUNI        DS7A2
133000000927     C* CHAIN A GG PULIZIA PER CREAZIONE ANOMLIA 10 E 50
133100000927     C                   MOVEL     '5A'          COD
133200000927     C                   MOVEL(P)  '1'           KEY
133300000927     C     KTAB          CHAIN     TABEL                              99
133400000927     C  N99              MOVEL     TBLUNI        DS5A
133500000927     C*
133600170505     C***                MOVEL     '5A'          COD
133700170505     C***                MOVEL(P)  'PT'          KEY
133800170505     C**   KTAB          CHAIN     TABEL                              99
133900170505     C**N99              MOVEL     TBLUNI        DS5APT
134000000927     C*
134100000927     C* PREDO GG MINORI TRA PULIZIA ARB E PULIZIA ARB POSTE
134200170505     C***  §5AARB        IFLT      §5APT5
134300170505
134400000927     C                   Z-ADD     §5AARB        WGGPUL            3 0
134500170505
134600170505     C***                ELSE
134700170505     C***                Z-ADD     §5APT5        WGGPUL            3 0
134800170505     C***                ENDIF
134900000927     C**
135000000927      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA BOLLE ARRIVI
135100000927     C                   MOVE      *BLANKS       $DAT
135200000927     C                   MOVE      *BLANKS       $ERR
135300000927     C                   MOVEL     'FNLR84R'     $PGM
135400000927     C                   CALL      'TRUL49C'                            28
135500000927     C                   PARM                    $PGM             10
135600000927     C                   PARM                    $DAT              8
135700000927     C                   PARM                    $ERR              1
135800000927     C*
135900000928    3C     $ERR          IFEQ      '1'
136000000927     C     *IN28         OREQ      *ON
136100000928     C     $DAT          ORLE      *ZEROS
136200000927     C                   Z-ADD     WDATE1        G02INV
136300000927     C                   ELSE
136400000928     C                   MOVEL     $DAT          G02INV
136500000927    3C                   ENDIF
136600000927     C*
136700000927      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI
136800000927      * DI PULIZIA BOLLE ARRIVI
136900000927     C                   Z-ADD     *ZEROS        G02DAT
137000000927     C                   Z-ADD     *ZEROS        G02TGI
137100000927     C                   MOVEL     '3'           G02ERR
137200000927     C                   CALL      'XSRDA8'
137300000927     C                   PARM                    WLBDAT
137400000927      *
137500000927     C                   SUB       WGGPUL        G02TGI
137600000928     C                   Z-ADD     *ZEROS        GI8DAT
137700000928     C                   Z-ADD     *ZEROS        GI8INV
137800000928     C                   Z-ADD     G02TGI        GI8TGI
137900000927     C                   CALL      'XSRGI8'
138000000927     C                   PARM                    WGIDAT
138100000927      *
138200000928     C                   Z-ADD     GI8INV        PRIDAT
138300961212     C**
138400070209     c** udate - 4 --> sono i giorni che devo tenere le spunte dentro al
138500070212     c**               FIBAR se non riesco a creare l'anomalia collo
138600021203     c**               arrivato non part
138700021203     C                   Z-ADD     WDATE1        G02INV
138800021203     C                   Z-ADD     *ZEROS        G02DAT
138900021203     C                   Z-ADD     *ZEROS        G02TGI
139000021203     C                   MOVEL     '3'           G02ERR
139100021203     C                   CALL      'XSRDA8'
139200021203     C                   PARM                    WLBDAT
139300021203      *
139400021203     C                   SUB       4             G02TGI
139500021203     C                   Z-ADD     *ZEROS        GI8DAT
139600021203     C                   Z-ADD     *ZEROS        GI8INV
139700021203     C                   Z-ADD     G02TGI        GI8TGI
139800021203     C                   CALL      'XSRGI8'
139900021203     C                   PARM                    WGIDAT
140000021203     C                   Z-ADD     GI8INV        wpul050
140100961212     C**
140200021203     C**
140300930521     C                   MOVEL     '1'           UNO               1
140400930521     C                   MOVEL     'AA'          SAVNPS            2
140500960411     C*
140600960411     C                   MOVE      ' '           OPN9              1
140700961212     C*
140800021014     C                   CLEAR                   DSBS55
140900971113     C                   MOVEL     'L'           I50TLA
141000021014     C                   CALL      'TIBS55R'
141100021014     C                   PARM                    DSBS55
141200971113     C**
141300971113     C                   CLEAR                   KPJBA
141400021014     C                   MOVEL     'EDPCED'      WCED
141500021014     C                   MOVEL     SIMFEL        WTER
141600021014     C                   MOVEL     WPROF         KNMUS
141700971113     C                   MOVEL     O50PSI        KNSIF
141800970829     C                   MOVEL     'DSP01'       KNMTD
141900961212     C                   MOVEL     'N'           KSTJO
142000961212     C                   MOVEL     'B'           KTPAZ
142100961212     C                   MOVEL     'AZNAUTO'     KNMEB
142200961212     C                   MOVEL     'J'           KEXCN
142300961212     C                   MOVEL     'P5'          KCOJB
142400961212     C                   MOVEL     'N'           KCANC
142500000927     C*
142600000927     C                   CLEAR                   SA2DFV
142700001109     C**
142800001109     C** CARICO TABELLA 7C - ANOMALIE CREABILI DA CAT.SPUNTA GENERICA
142900001109     C                   MOVEL     '7C'          COD
143000001109     C                   Z-ADD     0             X
143100001109     C     KTAB2         SETLL     TABEL
143200001109     C     KTAB2         READE     TABEL                                  99
143300001109     C*
143400001109    1C     *IN99         DOWEQ     *OFF
143500001109    2C     TBLFLG        IFEQ      ' '
143600001109     C                   MOVEL     TBLUNI        DS7C
143700001109    3C     §7CCCG        IFEQ      'S'
143800001109     C                   ADD       1             X
143900001109     C                   MOVEL     TBLKEY        ACG(X)
144000001109    3C                   ENDIF
144100001109    2C                   ENDIF
144200001109     C*
144300001109     C     KTAB2         READE     TABEL                                  99
144400001109    1C                   ENDDO
144500110103      *
144600091209      * Aggancio tabella AR5 per record 'GEN'
144700091209     c                   Clear                   dAr5
144800091209     c                   Clear                   dsbs02
144900091209     c                   Eval      T02Mod = 'C'
145000091209     c                   Eval      T02Sif = knsif
145100091209     c                   Eval      T02Cod = 'AR5'
145200091209     c                   Movel(p)  'GEN'         T02Ke1
145300091209     c                   Call      'TIBS02R'
145400091209     c                   Parm                    Kpjba
145500091209     c                   Parm                    dsbs02
145600091209     c                   If        T02Err = *Blanks
145700091209     c                   Movel     T02Uni        dAr5
145800091209     c                   EndIf
145900170125     C*
146000170125     C* CARICO TABELLA DECOFIIMG PER le filiali con New IMA
146100170125     c                   clear                   AllNewIMA         1
146200170125     c                   clear                   trulvpods
146300170125     c                   eval      ivpoke1 = 'DECOFIIPG'
146400170125     c                   call      'TRULVPOR'
146500170125     c                   parm                    trulvpods
146600170125     c                   if         %lookup('999':skFilOk)>0
146700170125     C                   EVAL       AllNewIMA='S'
146800170125     c                   endif
146801180124     C* CARICO TABELLA DECOFIINC PER le filiali con gestione incompatiblil
146802180124     c                   clear                   AllINC            1
146803180124     c                   clear                   trulvpods
146804180124     c                   eval      ivpoke1 = 'DECOFIINC'
146805180124     c                   call      'TRULVPOR'
146806180124     c                   parm                    trulvpods
146807180124     c                   eval      fic = skfilok
146808180124     c                   if        %lookup('999':fic)>0
146809180124     c                   eval      AllINC = 'S'
146810180124     c                   endif
146900170125
147000911014     C                   ENDSR
147100941206     C**************************************************************************
147200941206     C* CREO ANOMALIA
147300941206     C**************************************************************************
147400911016     C     CRTANM        BEGSR
147500000320     C                   CLEAR                   WCCH
147600970521     C*
147700970521     C                   CLEAR                   WANM10
147800941206     C*
147900970204    1C     COMCAA        IFNE      645
148000990518     C     KANM          CHAIN     FNANM000                           96
148100970204   X1C                   ELSE
148200961205     C*
148300990518     C     KANM          CHAIN(N)  FNANM000                           96
148400961205     C* PER L'ANOMALIA 645 LEGGO FINTANTO CHE NON TROVO UNA ANOMALIA
148500961205     C*  CON DATA = ALLA DATA SPUNTA: SE C'E' NON CREO
148600970204    2C     *IN96         DOWEQ     *OFF
148700990518     C     ANMDAO        ANDNE     BARDFV
148800990518     C     KANM          READE(N)  FNANM000                               96
148900970204    2C                   ENDDO
149000961205     C*
149100970204    1C                   ENDIF
149200000811     C*
149300000811    1C     *IN96         IFEQ      *ON
149400000811     C*
149500000811     C     COMCAA        ORNE      645
149600000811     C     ANMAIE        ANDEQ     'I'
149700000811     C* PER ORA SOLO SULLE INTERNE RICREO ANOMLIA SE PRESENTE
149800970204     C*  - SE CHIUSE SEMPRE
149900040923     C*  - SE APERTA SOLO LA DATA E' < DELLA ESISTENTE
150000040923     c* Per la 200 la ricreo solo se e' chiusa senza guardare la data creaz
150100961205     C*
150200970204     C                   MOVEL     'S'           WWRITE
150300970204    2C     *IN96         IFEQ      *OFF
150400970204     C     ANMDCH        ANDEQ     0
150500970204     C     ANMDAO        ANDGE     BARDFV
150600041005    2C     *IN96         oreq      *OFF
150700041005     C     ANMDCH        andeq     0
150800040923     C     comcaa        ANDeq     200
150900970204     C                   MOVEL     ' '           WWRITE            1
151000970204    2C                   ENDIF
151100970204     C**
151200070215    2C     WWRITE        IFEQ      'S'
151300990518     C                   CLEAR                   FNANM000
151400950927     C                   EXSR      RIEANM
151500000614     C**
151600001109     C* CREO SOLO SE ANOMALIA :
151700001109     C* CREABILE DA QUALUNQUE CATEGORIA SPUNTA
151800070215    3C     §7CCCG        ifeq      'S'
151900001109     C* NON CREABILE DA CATEG. GENERICA E LA CATEGORIA NON È GENERICA
152000070215     C     BARSPG        orne      'I'
152100950927     C*
152200950927     C                   MOVE      COMLNP        ANMLNP
152300990518     C                   Z-ADD     COMAAS        ANMAAS
152400950927     C                   MOVE      COMNSP        ANMNSP
152500960524     C                   MOVE      BARDFV        ANMDAO
152600990518     C                   Z-ADD     BARNFV        ANMNFV
152700950927     C                   MOVEL     BARFGS        ANMCDU
152800040621     c* imposto se spedizione di valore
152900070215    4c                   if        anmnsp>0
153000091209     c     kar5          chain(n)  fiar501l
153100070215    5c                   if        %found(fiar501l)
153200040621     c                   movel     ar5uni        dar5gen
153300040621     c                   else
153400040621     c                   clear                   dar5gen
153500070215    5c                   endif
153600040621     c                   movel     §ar5bva       anmft4
153700070215    4c                   endif
153800110103     c*
153900040429     c* 22/4/4 --> al posto di anmfgs imposto BPES il p.o. che spara
154000040429     c*            che solo per le spunte partenza è sempre barfgs
154100040429     C                   MOVEL     bpes          ANMFLE
154200070215    4C     ANMCAA        IFEQ      10
154300150205    4C     ANMCAA        oreq      410
154400070215     C                   z-add     BARZNC        ANMfl4
154500070215    4C                   ENDIF
154600911112     C*
154700921020     C* SE ESTERNA SCRIVO FIL A CUI TRASMETTERE
154800070215    4C     §7CAIE        IFEQ      'E'
154900070215    5C     COMLNP        IFGT      0
155000960528     C                   MOVEL     COMLNP        ANMFL1
155100960528     C                   ELSE
155200960704     C                   MOVEL     LNPB          ANMFL1
155300070215    5C                   ENDIF
155400070215    4C                   ENDIF
155500911016     C*
155600970204     C* SE INTERNA AUTOCHIUSA E C'E' GIA' NON FACCIO NULLA
155700070215    4C     *IN96         IFEQ      *ON
155800970204     C     §7CCHA        ORNE      'S'
155900960430     C* SE LA KEY E' PER SEGNACOLLO ED IL NUMERO DI SEGNACOLLO E' 0
156000960430     C*  NON CREO L'ANOMALIA (ERRORE DA TERMINALINO)
156100070215    5C     §7CKEY        IFNE      'S'
156200960430     C     §7CKEY        OREQ      'S'
156300960430     C     ANMSCN        ANDGT     0
156400990922     C*
156500990922     C* LA DATA I APERTURA ANOMALIA CI DEVE ESSERE , ALTRIMENTI
156600990922     C*  E' MEGLIO NON CREARE L'ANOMALIA
156700070215    6C     ANMDAO        IFGT      0
156800990518     C   96              WRITE     FNANM000
156900990518     C  N96              UPDATE    FNANM000
157000070215    6C                   ENDIF
157100070215    5C                   ENDIF
157200961205     C*
157300970204     C* SE CREO ANOAMLIA 145 O 146 CHIUDO LE ANOMALIE INTERNE 140 E 130
157400070215    5C     ANMCAA        IFEQ      145
157500970204     C     ANMCAA        OREQ      146
157600970204     C                   Z-ADD     130           COMCAA
157700970204     C                   EXSR      CHIUAN
157800970204     C                   Z-ADD     140           COMCAA
157900970204     C                   EXSR      CHIUAN
158000070215    5C                   ENDIF
158100070215    4C                   ENDIF
158200970204     C*
158300070215    3C                   ENDIF
158400070215    2C                   ENDIF
158500970521   X1C                   ELSE
158600110103     c*
158700970521     C* OPERAZIONI PER ANOMALIA 10 GIA' ESISTENTE MA CHIUSA:
158800970521    2C     COMCAA        IFEQ      10
158900970522     C* Se anomalia 10 a liv massimo --> memorizzo per creaz.anom.410
159000970522    3C     ANMLID        IFEQ      §7ALCI
159100970521     C                   MOVE      '1'           WANM10            1
159200970522    3C                   END
159300970522     C* Se anomalia chiusa (non pratica IDD):
159400970522     C* 1) sflaggo e chiudo l'anomalia mettendo cch='PR' se è a liv max
159500970522    3C     ANMDCH        IFGT      *ZEROS
159600970522     C     ANMCCH        ANDNE     'PR'
159700970522     C*
159800970522    4C                   SELECT
159900970522     C     ANMLID        WHENEQ    §7ALCI
160000970522     C     ANMCCH        ANDEQ     'AN'
160100970522     C                   MOVEL     'PR'          ANMCCH
160200970522     C                   MOVEL     'D'           ANMFSC
160300970522     C                   CLEAR                   ANMFT1
160400970522     C                   CLEAR                   ANMFT2
160500970522     C     ANMLID        WHENLT    §7ALCI
160600970523     C                   CLEAR                   ANMCCH
160700970522     C                   CLEAR                   ANMDCH
160800970522     C                   CLEAR                   ANMFSC
160900970522    4C                   ENDSL
161000990518     C                   UPDATE    FNANM000
161100970522    3C                   ENDIF
161200970522    2C                   ENDIF
161300970204    1C                   ENDIF
161400970204     C*
161500990518     C                   UNLOCK    FNANM02L
161600911016     C                   ENDSR
161700960613     C**************************************************************************
161800960613     C*    CONTROLLO IL RAGGRUPPAMENTO DELLA PISTOLA
161900960613     C**************************************************************************
162000960613     C     PISMAN        BEGSR
162100960613     C*
162200961213     C                   Z-ADD     1             Y                 3 0
162300961213     C*
162400960613    1C     *IN02         IFEQ      *OFF
162500960613     C     BARNPS        LOOKUP    NPS(Y)                                 34
162600960613     C  N34              Z-ADD     1             Y
162700960613     C*
162800960613   X1C                   ELSE
162900960613     C*
163000960613     C                   MOVEL     '7J'          COD
163100960613     C                   MOVEL(P)  BARNPS        KEY
163200960613     C     KTAB          CHAIN     TABEL                              34
163300000316     C     *IN34         IFEQ      *OFF
163400000316     C                   MOVEL     TBLUNI        DS7J
163500000316     C                   MOVEL     §7JRPS        RPS(Y)
163600000316     C                   MOVEL     §7JORD        OPS(Y)
163700000316     C                   MOVEL     §7JTPS        TPS(Y)
163800000316     C                   MOVEL     §7JINV        INV(Y)
163900040503     C                   MOVEL     §7Jfsl        fsl(Y)
164000000316     C                   ELSE
164100000316     C                   MOVEL     'M'           RPS(Y)
164200000316     C                   MOVEL     999           OPS(Y)
164300000316     C                   MOVEL     'B'           TPS(Y)
164400000316     C                   MOVEL     ' '           INV(Y)
164500040503     C                   MOVEL     ' '           fsl(Y)
164600960613    1C                   END
164700000316    1C                   END
164800960613     C                   ENDSR
164900941206     C**************************************************************************
165000020213     C*    A G G I O R N O    R E C O R D    B O L L E
165100941206     C**************************************************************************
165200980212     C     UPDARB        BEGSR
165300960521     C                   CLEAR                   WNOTRA
165400961210     C                   CLEAR                   W60X
165500961211     C                   CLEAR                   WESAN
165600961210     C                   MOVEL     'S'           W620
165700961211     C                   MOVEL     'S'           WCREA
165800021106     C                   CLEAR                   WDCM
165900021106     C                   CLEAR                   WBOAR
166000021106     C                   CLEAR                   WATDCM
166100040430     C                   CLEAR                   WABCCA
166200040430     C                   CLEAR                   WABNDC
166300161007     C                   CLEAR                   gcpdgc            8 0
166400021106     C                   MOVEL     ' '           WGIAC             1
166500941206     C*
166600921229     C* TROVO TIPO E RAGGRUPPAMENTO PISTOLE DI QUELLA CARICATA
166700960613     C                   EXSR      PISMAN
166800921229     C*
166900960523     C* CARICO LA L6 DELLA FILIALE GESTIONE
167000020213     C     BPES          IFNE      SAVPES
167100020213     C                   MOVEL     BPES          SAVPES
167200961209     C                   EXSR      CARL6
167300960523     C                   ENDIF
167400921013     C*
167500041012     C                   SETOFF                                         0332
167600950922     C                   CLEAR                   COMLNP
167700950922     C                   CLEAR                   COMAAS
167800950922     C                   CLEAR                   COMNSP
167900950922     C                   CLEAR                   WSPLOC
168000950927     C                   CLEAR                   WTIBOL
168100970423     C                   CLEAR                   WWFVC
168200020124     C                   CLEAR                   LNPB
168300020124     C                   CLEAR                   WTAB5F
168400020131     C                   CLEAR                   WESART
168500020131     C                   CLEAR                   WESBLT
168600020131     C                   CLEAR                   WESBTT
168700130228     c                   clear                   bolxco
168800160421     c                   clear                   bolznc
168900170509     c                   clear                   boldbr
169000021022     c*
169100021125     C* TERMINAL partenza DI BPES
169200021125     C                   MOVEL     'P'           WTITER
169300021022     C                   MOVEL     BARDFV        WDTRIF
169400021022     C                   EXSR      TERPES
169500021022     C                   Z-ADD     D55TFP        PESTFP
169600021203     c*
169700040422     c* Se trovo la bolla transito x p.o. spunta --> aggiorno transito, senz
169800021203     c*  controllo i terminal (c'e' un problema a livello di eccezioni ter
169900021203     C     KBAR37        CHAIN     FNBTT37L                           3739
170000021203     C   39              GOTO      ENDUPD
170100021203     C  N37KBTT          CHAIN     FNBTP11L                           37
170200021203     C**
170300021203    1C     *IN37         IFEQ      *OFF
170400021203     C                   MOVEL     'S'           WESBTT            1
170500021203     C                   MOVEL     BTTLNP        LNPB
170600021203     C                   MOVEL     BTPDSP        DSPB
170700021203     C                   MOVEL     BTPDSP        WDTRIF
170800021203     C                   MOVEL     BTPTFP        LNPTFP
170900050603     C                   MOVEL     BTPTFa        bolTFa
171000170509     C                   MOVE      BTPdbr        boldbr
171100040621     C                   MOVE      BTTLNP        COMLNP
171200040621     C                   MOVE      BTTAAS        COMAAS
171300040621     C                   MOVE      BTTNSP        COMNSP
171400021203   X1C                   ELSE
171500021203     C                   MOVEL     'N'           WESBTT
171600020124     C**
171700020124     C** VEDO SE TROVO LA BOLLA IN BLT
171800020124     C     KBAR          CHAIN     FNBLT27L                           3239
171900020213     C   39              GOTO      ENDUPD
172000020213     C  N32KBLT          CHAIN(N)  FNBLP01L                           32
172100020124     C**
172200021203    2C     *IN32         IFEQ      *OFF
172300020131     C                   MOVEL     'S'           WESBLT            1
172400020124     C                   MOVEL     BLTLNP        LNPB
172500020201     C                   MOVEL     BLPTFP        LNPTFP
172600050603     C                   MOVEL     BLPTFa        bolTFa
172700020201     C                   MOVEL     BLPDSP        WDTRIF
172800020214     C                   MOVEL     BLPDSP        DSPB
172900130301     c                   movel     BLPxco        bolxco            1
173000160421     c                   movel     BLPznc        bolznc            2
173100170509     C                   MOVE      BLPdbr        boldbr
173200040621     C                   MOVE      BLTLNP        COMLNP
173300040621     C                   MOVE      BLTAAS        COMAAS
173400040621     C                   MOVE      BLTNSP        COMNSP
173500021203   X2C                   ELSE
173600020213     C                   MOVEL     'N'           WESBLT
173700020124     C** SE NON TROVO LA BOLLA PARTENZA CERCO NEGLI ARRIVI
173800020124     C     KBAR          CHAIN     FNART27L                           3239
173900020213     C   39              GOTO      ENDUPD
174000050223     C  N32KARB          CHAIN(N)  FNARB01L                           32
174100020124     C**
174200021203    3C     *IN32         IFEQ      *OFF
174300020201     C                   MOVEL     'S'           WESART            1
174400020124     C                   MOVEL     ARTLNP        LNPB
174500020214     C                   MOVEL     ARBDSP        DSPB
174600020201     C                   MOVEL     ARBDSP        WDTRIF
174700020201     C                   MOVEL     ARBTFP        LNPTFP
174800050603     C                   MOVEL     ARBTFa        boltfa
174900130301     c                   movel     ARBxco        bolxco            1
175000160421     c                   movel     ARBznc        bolznc
175100170509     C                   MOVE      ARBdbr        boldbr
175200040621     C                   MOVE      ARTLNP        COMLNP
175300040621     C                   MOVE      ARTAAS        COMAAS
175400040621     C                   MOVE      ARTNSP        COMNSP
175500021203   X3C                   ELSE
175600020213     C                   MOVEL     'N'           WESART
175700020131     C** IMPOSTO LNPB = BARLNP
175800020131     C                   MOVEL     BARLNP        LNPB
175900020214     C                   Z-ADD     BARDFV        DSPB
176000050603     c                   clear                   boltfa
176100020124     C**
176200020124     C** SE NON TROVATA MAI LA BOLLA, CERCO SE ESISTE LA EVENTUALE
176300020124     C*   SPUNTA PARTENZA E LA TABELLA 5F
176400020205    4C     BARNRS        IFGT      *ZEROS
176500020124     C                   EXSR      RICLNP
176600020213    4C                   ENDIF
176700021105     c* prendo terminal di partenza della linea partenza calcolata
176800070222     c                   if        bardfv=0
176900021105     C                   Z-ADD     BARDFV        WDTRIF
177000070222     c                   else
177100070222     c                   z-add     wdate1        wdtrif
177200070222     c                   endif
177300070222     c
177400021105     C                   EXSR      TERPAR
177500020213    3C                   ENDIF
177600020213    2C                   ENDIF
177700020213    1C                   ENDIF
177800020204     C**
177900020215     C** CONTROLLO TRA P.O. SPUNTA E LINEE SPUNTE CHE AGGIONAMENTI
178000020213     C**  POSSO FARE
178100021125     c*
178200050215     C* TERMINAL arrivo DI BPES
178300021125     C                   MOVEL     'A'           WTITER
178400021125     C                   MOVEL     BARDFV        WDTRIF
178500021125     C                   EXSR      TERPES
178600021125     C                   Z-ADD     D55TFA        PESTFA
178700021125     c
178800020204     C                   EXSR      CTRAGG
178900020213     C**
179000020213     C** A G G I O R N A M E N T O   B O L L E   P A R T E N Z A
179100020213    1C     WAGPAR        IFEQ      'S'
179200021203    2C     WESblt        IFEQ      ' '
179300021203     C     KBAR          CHAIN     FNBLT27L                           3239
179400021203     C   39              GOTO      ENDUPD
179500021203     C  N32              MOVEL     'S'           WESBLT            1
179600021203     C   32              MOVEL     'N'           WESBLT            1
179700021203    2C                   ENDIF
179800021203     c**
179900021203    2C     WESBLT        ifeq      'S'
180000150817     c* Aggiorno solo se la bolla NON parte da + filiali
180100150817     c* oppure sel la bolla per LINEA parte da + filiali ma è mia bolla
180200150817     c* oppure se chi spara è la linea partenza della bolla che parte da + fil
180300150622     c                   if        wbsptp=' ' or wbsptp='L'
180400150622     c                             or bpes=lnpb
180500020213     C                   EXSR      AGGBLT
180600150622     c                   else
180700150622     c                   clear                   wagPAR
180800150622     c                   endif
180900150622     c
181000021203    3C     WEND          IFEQ      '1'
181100020213     C                   GOTO      ENDUPD
181200021203    3C                   ENDIF
181300021203    2C                   ENDIF
181400020213     C*
181500021203    2C     WESBLT        ifeq      'N'
181600020204     C                   MOVEL     'N'           WBOPAR            1
181700020204     C                   MOVEL     'P'           WTIBOL            1
181800021203    2C                   ENDIF
181900021203    1C                   ENDIF
182000020213     C*
182100020213    1C     WAGPAR        IFNE      'S'
182200020213     C     WESBLT        ANDEQ     'S'
182300020213     C                   UNLOCK    FNBLT27L
182400020213    1C                   ENDIF
182500020213     C**
182600020213     C* A G G I O R N O   B O L L E   A R R I V I
182700020213     C* A PRESCINDERE DAL FATTO CHE TROVI O MENO LA BOLLA
182800020213    1C     WAGARR        IFEQ      'S'
182900020220     C* LEGGO LE BOLLE ARRIVI ANCHE PER FLAG=6.
183000020220     C*  SI TRATTA DI SPUNTA DI DISGUIDO PER CUI NON AGGIORNEREBBE
183100020220     C*  LA BOLLA ARRIVI MA MI SERVE PER ENTRARE IN CTRA60
183200070221    1C     dls45FLG      OREQ      '6'
183300020213     C* SE NON HO ANCORA CHAINATO IL DETTAGLIO COLLI LO FACCIO ORA
183400020213    2C     WESART        IFEQ      ' '
183500020213     C     KBAR          CHAIN     FNART27L                           3239
183600020218     C   39              GOTO      ENDUPD
183700020213     C  N32              MOVEL     'S'           WESART            1
183800020214     C   32              MOVEL     'N'           WESART            1
183900020213    2C                   ENDIF
184000020213     C*
184100020213     C                   EXSR      AGGART
184200020213    1C                   ENDIF
184300020213     C*
184400020213    1C     WAGARR        IFNE      'S'
184500020213     C     WESART        ANDEQ     'S'
184600020213     C                   UNLOCK    FNART27L
184700020213    1C                   ENDIF
184800040428     c*
184900040428     c*Chiudo eventuale anomalia di disguido presente sul collo, se
185000040428     c*  di area <> dal'anomalia
185100040503     C* SOLO SE SPUNTA DI PISTOLA REALE
185200040503     c                   if        fsl(y)<>'F'
185300040428     c                   eval      comcaa=200
185400040428     C*
185500040503     C     Kanm          chain(N)  FNANM000
185600040428     c                   if        %found(fnanm02l)
185700040428     C                   CLEAR                   DSLV55
185800040428     C                   MOVEL     'P'           D55TPT
185900040428     C                   Z-ADD     anmfle        D55LIN
186000040428     C                   Z-ADD     bardfv        D55DRF
186100040428     C                   EXSR      FNLV55
186200040428     c                   if        pestfp<>d55tfp  or
186300040428     c                             bpes=anmlna
186400040428     c                   exsr      chiuan
186500040428     c                   endif
186600040428     c                   endif
186700040503     c                   endif
186800921013     C**
186900960531     C** T R A N S I T I  /  D I S G U I D I   I N   A R E A   E  N O N
187000921013     C**
187100070221    0C     dls45FLG      IFNE      '6'
187200911112     C*
187300960522     C* NON E' UNA SPUNTA DEL TERMINAL DI ARRIVO
187400050603    1C     Usalnatfa     IFNE      BPES
187500021022     c     wesbtt        oreq      'S'
187600960522     C*
187700960522     C* SE NEMMENO I 2 TERMINAL DI ARRIVO CORRISPONDONO
187800960522     C* --> T R A N S I T O
187900050603    2C     Usalnatfa     IFNE      PESTFA
188000021022     c     wesbtt        oreq      'S'
188100960531     C**
188200020214     C** I TRANSITI SONO ANCORA DA SISTEMARE
188300020214     C**  QUESTE 2 SPECIFICHE SONO DA ELIMINARE: INFATTI LA BOLLA POTRA
188400020214     C**  ESSERE SIA IN BLT CHE IN BTT
188500020205    3C     WAGPAR        IFEQ      ' '
188600960703     C     WBOPAR        OREQ      'N'
188700960522     C**
188800040422     C     WESBTT        ifeq      'N'
188900960521     C**
189000960612     C* SE E' SPUNTA ENTRATA O PARTENZA E NON HO TROVATO NE' LA BOLLA
189100960521     C*  PARTENZA NE' LA BOLLA TRANSITO, PER SERIE =0
189200960521     C*  IMPOSTO NELLA SPUNTA FLAG TRASMISSIONE "N" E 99999999
189300960612     C*  PER NON AUTOGENERARE LA SPUNTA PARTENZA O NON TRASMETTERLA
189400001123     C* IDEM PER SPUNTA IMI (PER BOLLE POSTE AUTOGENERA ANCHE DA SPUNTE
189500001123     C* IMI)
189600960522    5C     BARNPG        IFEQ      5
189700020205     C     BARNPG        OREQ      1
189800001123     C     BARNPG        OREQ      3
189900001123     C     BARSPG        ANDEQ     'I'
190000020205    6C     BARNRS        IFEQ      0
190100020205     C     WAGPAR        ANDEQ     'S'
190200960521     C     WBOPAR        ANDEQ     'N'
190300960521     C                   MOVEL     'S'           WNOTRA            1
190400020205    6C                   ENDIF
190500960612    5C                   ENDIF
190600040120     c
190700911016     C*
190800110124     C* SE NON E' BOLLA LOCALE e nemmeno in £6 : DISGUIDO
190900150817     c*  non è disguido nemmeno se collo che parte da altra filiale
191000150817     c*  ( la bolla deve essere spostata di lnp)
191100020205    5C     WAGPAR        IFEQ      ' '
191200110124    5C     WAGarr        andeq     ' '
191300150817    5C     WBSPSPU       andeq     ' '
191400150817     c
191500020215     C     BARFLE        IFEQ      BTFP
191600960522     C                   Z-ADD     200           COMCAA
191700960404     C  N05              EXSR      CRTANM
191800020215     C                   ENDIF
191900020215     C* DEVO COMUNQUE DIRE CHE SI TRATTA DI UN DISGUIDO
192000960404     C                   MOVEL     'D'           WTIBOL
192100041012     c
192200041012     c* Aggiorno anche la bolla in  arrivo
192300041012     c                   EXSR      AGGARRIVO
192400960522    5C                   ENDIF
192500960522     C**
192600911016     C* 37 OFF - TROVATA AGGIORNO BOLLA TRANSITO
192700960522   X4C                   ELSE
19280002021418   C                   EXSR      AGGBTT
192900960522    4C                   ENDIF
193000040430     c*
193100040430     c* Eventuale spunta di disguido o transito, chiude fuori posto
193200040430     c*  di bolla partenza
193300040430     C                   Z-ADD     120           COMCAA
193400040430     C                   EXSR      CHIUAN
193500970204     C*
193600970204   X3C                   ELSE
193700970204     C* SE NON E' COLLO DELLA AREA DI ARRIVO E COLLO IN PARTENZA
193800970204     C*  VEDO SE CREARE L'ANOMALIA FUORI POSTO BOLLA IN PARTENZA
193900970204     C                   EXSR      ANM120
194000990120     C** VERIFICO LA PRESENZA DI UNA C.A. PER CREARE EVENTUALI ANOM
194100000523     C** IGNORO SE E' SPUNTA CAT 4-8 SOLO IN ARRIVO(IN PART.SEMPRE)
194200990121     C                   MOVEL     'S'           WCAT4             1
194300990126     C*     GESTISCO IL RITROVAMENTO COLLO
194400990126     C                   MOVEL     'S'           WRITRO            1
194500990126     C*     GESTISCO LA CREAZIONE ANOMALIE
194600990126     C                   MOVEL     'S'           WCREAN            1
194700990120     C                   EXSR      CTRDAN
194800990120     C**
194900990120     C** SE NON TROVATA CA DI COLLO
195000990120     C* VERIFICO SE LA SPEDIZIONE E' SU UNA CA CHIUSA --> CREO
195100990120    4C     WTRDAN        IFEQ      ' '
195200040811      *
195300040811      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
195400040811      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
195500040811      * legate alla spedizione che passo
195600040811     c                   clear                   fidn12ds
195700170505     c***                eval      i12aas = BLTAAS
195800170505     c***                eval      i12lnp = BLTLNP
195900170505     c***                eval      i12nrs = BLTNRS
196000170505     c***                eval      i12nsp = BLTNSP
196100170505     c* per evitare che i campi siamo sporchi, uso i salvati
196200170505     c                   eval      i12aas = comAAS
196300170505     c                   eval      i12lnp = comLNP
196400170505     c                   eval      i12nrs = barNRS
196500170505     c                   eval      i12nsp = comNSP
196600170505     c                   eval      i12fel = 'E'
196700040811     c*
196800040811     c                   call      'FIDN12R'
196900040811     c                   parm                    fidn12ds
197000040811     c*
197100040811     c                   setoff                                       33
197200040811     c*
197300040811     c* se non ci sono errori
197400040811     c                   if        o12err <> ' '
197500040811     c                   seton                                        33
197600040811     c                   else
197700040811     c* se numero di CA trovate maggiore di zero
197800040811     c                   if        o12nca = 0
197900040811     c                   seton                                        33
198000040811     c                   else
198100040811     c                   z-add     1             jj                2 0
198200040811     c                   dow       jj <= o12nca and *in33 = *off
198300040811     C     skDch(jj)     IFGT      0
198400040811     C*
198500040811     C* Valorizzo i campi DCT* utilizzati nella routine CRE151 con quelli
198600040811     C* ritornati dal *pgm d utilità FIDN12R
198700040811     C                   movel     skCch(jj)     DCTCCH
198800040811     C*
198900040811     C                   EXSR      CRE151
199000040811     C                   SETON                                        33
199100040811     C                   ENDIF
199200040811     c*
199300040811     c                   add       1             jj
199400040811     c                   enddo
199500040811     c*
199600040811     c                   seton                                        33
199700040811     c*
199800040811     c                   endif
199900040811     c                   endif
200000040811     C**
200100990120    4C                   ENDIF
200200990120     C**
200300960531    3C                   ENDIF
200400960522     C*
200500960531   X2C                   ELSE
200600970203     C*  D I S G U I D O   I N   A R E A
200700970203     C*  (SE NON E' UNA MIA BOLLA PARTENZA)
200800961021     C                   SETOFF                                       88
200900961122     C                   CLEAR                   WSTESS
201000961028     C*
201100020213    3C  N05BPES          IFNE      BARLNA
201200050930     c* al posto di BPES uso barfgs per le spunte IMP: mi fa più comodo
201300050930     c*  usare direttamente il terminal di partenza di chi spara perchè
201400050930     c*  l'IMP lo fa solo il terminal di partenza ma può essere usato
201500050930     c*  anche dai 2 livello
201600050930     C     barfgs        ANDNE     LNPB
201700960522     C     BARNPG        ANDGT     1
201800050930     C     BARNPG        ANDne     5
201900050930     c* Se spunta entrata escludo se di tratta di un aggiornamento
202000050930     c*  in partenza e non in arrivo (per cui il p.o. fa parte
202100050930     c*  dell'area di partenza
202200050930    3C     BPES          orne      BARLNA
202300050930     C     barfgs        ANDNE     LNPB
202400050930     C     BARNPG        ANDeq     5
202500050930     c     wagarr        andeq     'S'
202600050930     c
202700980727     C** CREO ANOMALIE SE COLLO SPUNTATO IN IMA ANCHE SE DA ME CONFERMA
202800020213    3C     BPES          ORNE      BARLNA
202900020213     C     BPES          ANDEQ     LNPB
203000980727     C     BARNPG        ANDEQ     3
203100980727     C     BARSPG        ANDEQ     'A'
203200960523     C*
203300020213     C* NON DEVE ESSERE PRESENTE NEMMENO IN L6 DEL P.O. SPUNTA
203400960523     C     BARLNA        LOOKUP    L6                                     88
203500980127     C**
203600960531    4C     *IN88         IFEQ      *OFF
203700961202     C                   Z-ADD     146           CAACRE
203800961202     C                   Z-ADD     145           CAACIU
203900961202     C                   EXSR      ANM14X
204000960531    4C                   ENDIF
204100960531    3C                   ENDIF
204200960522     C*
204300960523     C* ALTRIMENTI  LA CHIUDO
204400960531    3C     *IN88         IFEQ      *ON
204500960523     C     BARNPG        OREQ      1
204600020213     C     BPES          OREQ      BARLNA
204700961122     C*
204800000523     C* SE CHI SPARA E' LA FILIALE FINALE DI ARRIVO, ME LO MEMORIZZO
204900961122     C*  MI SERVE PER LA CHIUSURA
205000961122    4C     *IN88         IFEQ      *ON
205100020213     C     BPES          OREQ      BARLNA
205200961122     C                   MOVEL     'S'           WSTESS
205300961122    4C                   ENDIF
205400961122     C**
205500960522     C                   Z-ADD     146           COMCAA
205600960522     C                   EXSR      CHIUAN
205700961122     C*
205800960522     C                   Z-ADD     145           COMCAA
205900960522     C                   EXSR      CHIUAN
206000970204     C*
206100970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
206200970204     C                   EXSR      ANMARR
206300960531    3C                   ENDIF
206400961021     C*
206500970203     C* CREO E/O CHIUDO LE ANOMALIE CHE RIGUARDANO L'AREA DI ARRIVO
206600970203     C                   EXSR      ANMSEM
206700960531    2C                   ENDIF
206800911016     C*
206900960531   X1C                   ELSE
207000970203     C* T E R M I N A L   D I   A R R I V O
207100961122     C                   SETOFF                                       88
207200961122     C                   CLEAR                   WSTESS
207300970203     C*
207400970203     C* CREO E/O CHIUDO LE ANOMALIE CHE RIGUARDANO L'AREA DI ARRIVO
207500970203     C                   EXSR      ANMSEM
207600970204     C*
207700970204     C* CHIUDO EVENTUALE ANOMALIA BOLLA ARRIVI
207800970204    2C     BARNPG        IFEQ      2
207900970204     C                   Z-ADD     130           COMCAA
208000970204     C                   EXSR      CHIUAN
208100970204    2C                   ENDIF
208200940330     C*
208300940330     C* SONO TERMINAL DI ARRIVO-ANOMALIA:INOLTRARE COLLO ALL'ARRIVO
208400960531    2C     WTIBOL        IFNE      'A'
208500020218    2C     WTIBOL        ANDNE     'L'
208600960522     C                   MOVEL     'R'           WTIBOL
208700960531    2C                   ENDIF
208800960522     C*
208900960522     C* SOLO SE NON E' UN MIO COLLO
209000020213    2C  N05BPES          IFNE      BARLNA
209100091211     c
209200091211     C* CARICO LA L6 DELLA FILIALE GESTIONE
209300091211     C     BPES          IFNE      SAVPES
209400091211     C                   MOVEL     BPES          SAVPES
209500091211     C                   EXSR      CARL6
209600091211     C                   ENDIF
209700091211     c
209800960523     C* NON DEVE ESSERE PRESENTE NEMMENO IN L6
209900960531     C     BARLNA        LOOKUP    L6                                     88
210000960531    3C     *IN88         IFEQ      *OFF
210100960523     C*
210200960531    4C     BARNPG        IFGT      2
210300960522     C     BARNPG        ANDLT     5
210400000523     C     BARNPG        OREQ      8
210500961202     C                   Z-ADD     145           CAACRE
210600961202     C                   Z-ADD     146           CAACIU
210700961202     C                   EXSR      ANM14X
210800960522     C*
210900960531   X4C                   ELSE
211000961211     C* SE TROVO LA 145 E LA POSSO CHIUDERE CREO SE POSSO LA 620
211100961211     C*  SE NON TROVO LA 145 DEMANDO L'EVENTUALE CREAZIONE DELLA 620
211200961211     C*  ALLA CHIUSURA DELLA EVENTUALE 146, CHE POTREBBE ANCHE NON
211300961211     C*  ESSERCI. SIA CHE CI SIA DA CHIUDERE CHE NON CI SIA, A QUESTO
211400961211     C*  PUNTO LA 620 LA CREO
211500960522     C                   Z-ADD     145           COMCAA
211600961211     C* SOLO SE ESISTE L'ANOMALIA CREO LA 620
211700961211     C                   MOVEL     'S'           WESAN             1
211800961211     C*
211900960522     C                   EXSR      CHIUAN
212000961211     C                   CLEAR                   WESAN
212100961029     C* SE C'E' CHIUDO ANCHE IL DISGUIDO IN AREA
212200961029     C                   Z-ADD     146           COMCAA
212300961211     C*
212400961211     C   97              MOVEL     'S'           W620
212500961211     C  N97              MOVEL     'N'           W620
212600961029     C                   EXSR      CHIUAN
212700961211     C                   MOVEL     'S'           W620
212800960523    4C                   ENDIF
212900961021   X3C                   ELSE
213000961122     C*
213100020213     C* SE CHI SPARA E' LA FILIALE FINALE DI ARRIVO, ME LO MEMORIZZO
213200961122     C*  MI SERVE PER LA CHIUSURA
213300961122     C                   MOVEL     'S'           WSTESS            1
213400961021     C                   Z-ADD     146           COMCAA
213500961021     C                   EXSR      CHIUAN
213600970204     C*
213700970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
213800970204     C                   EXSR      ANMARR
213900960522    3C                   ENDIF
214000961122     C**
214100961021   X2C                   ELSE
214200970204     C* CHI SPARA E' LA FILIALE FINALE DI ARRIVO
214300961122     C                   MOVEL     'S'           WSTESS
214400961021     C                   Z-ADD     146           COMCAA
214500961021     C                   EXSR      CHIUAN
214600970204     C*
214700970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
214800970204     C                   EXSR      ANMARR
214900960523    2C                   ENDIF
215000911016     C*
215100950922    1C                   END
215200961213    0C                   ENDIF
215300021022     c
215400021203     c                   unlock    fnbtt37l
215500970203     C**
215600970203     C** SE FLAG = 2 MODIFICO ANOMALIE METTENDO NUOVO NUMERO DI FOGLIO
215700970203     C**
215800970203     C     *IN05         IFEQ      *ON
215900970203     C*
216000921021     C                   EXSR      RICFAS
216100921021     C*
216200990518     C     KBAR          SETLL     FNANM000
216300990518     C     KBAR          READE     FNANM000                               99
216400921021     C*
216500960423    2C     *IN99         DOWEQ     '0'
216600070208    3C     ANMNFV        IFEQ      DLS45NFVO
216700000516     C* SE NON C'E' SOTTO CATEGORIA DEVONO ESSERE UGUALI
216800000516    4C     BARSPG        IFEQ      ' '
216900921021     C     ANMFSA        ANDEQ     COMFAS
217000000516     C* SE     C'E' SOTTO CATEGORIA, DEVE ESSERE UGUALE AD UNA DELLE
217100000516     C*  TANTE
217200000516     C     ANMFSA        OREQ      WFAS1
217300000516     C     ANMFSA        OREQ      WFAS2
217400000516     C     ANMFSA        OREQ      WFAS3
217500000516     C     ANMFSA        OREQ      WFAS4
217600000516     C     ANMFSA        OREQ      WFAS5
217700921021     C                   Z-ADD     BARNFV        ANMNFV
217800960524     C                   Z-ADD     BARDFV        ANMDAO
217900000516     C                   MOVEL     COMFAS        ANMFSA
218000990518     C                   UPDATE    FNANM000
218100000516    4C                   ENDIF
218200000516    3C                   ENDIF
218300921021     C*
218400990518     C     KBAR          READE     FNANM000                               99
218500000516    2C                   ENDDO
218600920424     C*
218700920424     C                   Z-ADD     3             B
218800021107    1C                   END
218900920424     C     ENDUPD        TAG
219000000209     C*
219100110907    1C     *IN39         IFEQ      *ON
219200000209     C                   SETON                                        40
219300110907   x1C                   ELSE
219400000209     C*
219500000210     C* ELABORO SOLO SPUNTA DA MACCHINONE
219600110907     c*  non lo faccio se modifica di numero foglio da video
219700110907    2c  N05RPS(Y)        ifne      'A'
219800000209     C* AGGIORNO FILE  FISGN SE C'E'
219900000209     C     KBAR          CHAIN     FISGN03L                           33
220000110907    3C     *IN33         DOWEQ     *OFF
220100110907    4C     SGNST2        IFLT      9
220200000209     C                   ADD       1             SGNST2
220300001228     C                   CLEAR                   SGNT6A
220400001228     C                   CLEAR                   SGNT6B
220500001228     C                   CLEAR                   SGNT6C
220600001228     C                   CLEAR                   SGNT6D
220700001228     C                   CLEAR                   SGNT6E
220800001228     C                   CLEAR                   SGNT6F
220900080624     c                   eval      sgndatora = %char(%timestamp:*iso0)
221000000209     C                   UPDATE    FISGN000
221100110907    4C                   ENDIF
221200001020     C     KBAR          READE     FISGN03L                               33
221300110907    3C                   ENDDO
221400110907     c
221500110907     c* Una qualsiasi spunta REALE chiude eventuale anomalia interna
221600110907     c*  115 - COLLO MAI AFFIDATO
221700110929     C                   MOVEL     'AR'          SAVCCH
221800110907     C                   Z-ADD     115           COMCAA
221900110907     C                   EXSR      CHIUAN
222000110907     c
222100110907     c
222200110907    2C                   ENDIF
222300110907    1C                   ENDIF
222400941206     C*
222500020215     C                   ENDSR
222600961118     C**************************************************************************
222700961205     C* CREO ANOMALIA 600 PER L'EXPORT   COLLO A TERRA
222800961118     C**************************************************************************
222900961118     C     ANM600        BEGSR
223000961210     C*
223100961210     C* PER ESTERO CREO ANOMALIA 645 E 600
223200020212    7C     NTWBAR        IFEQ      'EEX'
223300060322    7C     NTWBAR        oreq      'DPD'
223400060322    7C     NTWBAR        oreq      'FED'
223500961210     C                   Z-ADD     645           COMCAA
223600961210     C                   EXSR      CRTANM
223700961210     C*
223800961210    8C     W60X          IFEQ      'S'
223900961209     C* SE ESISTE LA 620  LA DELETO
224000961118     C                   Z-ADD     620           COMCAA
224100001110     C     COMCAA        LOOKUP    ACG                                    34
224200001110     C     BARSPG        IFNE      'I'
224300001110     C     *IN34         OREQ      *ON
224400990518     C     KANM          DELETE    FNANM02L                           34
224500001110     C                   ENDIF
224600961118     C                   Z-ADD     600           COMCAA
224700961118     C                   EXSR      CRTANM
224800961210     C                   CLEAR                   W60X
224900961210    8C                   ENDIF
225000961210    7C                   ENDIF
225100961210     C                   ENDSR
225200960702     C**************************************************************************
225300960702     C* CERCO LNP BOLLA SE COLLO CON SERIE > 0
225400960702     C**************************************************************************
225500960702     C     RICLNP        BEGSR
225600970814     C                   SETOFF                                       33
225700960702     C*
225800020201     C* CONTROLLO SE COLLO CONFERMABILE DAL TERMINAL DEL P.O. EFFETTUA
225900020201     C*  SPUNTA
226000020201     C                   MOVEL     PESTFP        W008A             8
226100960702     C                   MOVE      BARNRS        W005A             5
226200960702     C                   MOVEL     BARLNP        W005A
226300960702     C                   MOVE      W005A         W008A
226400990108     C                   Z-ADD     1             YY                4 0
226500970828     C     W008A         LOOKUP    S5F(YY)                                33
226600020131     C**
226700020131     C* SE NON TROVATO CERCO COME SERIE = 99
226800020131     C     *IN33         IFEQ      *OFF
226900020131     C                   MOVE      99            W008A
227000020131     C                   Z-ADD     1             YY                4 0
227100020131     C     W008A         LOOKUP    S5F(YY)                                33
227200020131     C                   ENDIF
227300020131     C**
227400020131    1C     *IN33         IFEQ      *ON
227500020131     C                   MOVEL     'S'           WTAB5F            1
227600020131     C* CERCO SPUNTA PARTENZA SE C'E' PRENDO LA FGS DI QUEST'ULTIMA
227700020201     C*  E LA DATA DI RIFERIMENTO E' LA DATA DI QUESTA SPUNTA
227800960702     C                   Z-ADD     1             WNPG
227900070131     C     KBAR3         SETLL     FNBRV09L
228000070131     C     KBAR3         READE     FNBRV09L                               33
228100020131    2C     *IN33         DOWEQ     *OFF
228200040303    3C     BR9ATR        IFEQ      *BLANKS
228300040323     c* Escludo le defluenze e le spunte di simfel(potrebbe quindi essere co
228400040323     c*  lo in transito da me e quindi non partito da me)
228500040323    4C     BR9KEY        IFNE      SA1KEY
228600040323     C                   MOVEL     BR9KEY        SA1KEY
228700040323     C                   Z-ADD     BR9NPG        WNPG
228800040323     C                   Z-ADD     BR9NFV        WNFV
228900040323     C                   Z-ADD     BR9FGS        WFGS
229000070906     C                   movel     BR9nps        Wnps
229100040323     C                   EXSR      RILFOG
229200040323     c                   Z-ADD     WDFV          BRVDFV
229300140616     c                   movel     WFCF          BRVFCF
229400140616     c                   movel     Wspg          BRVspg
229500040323    5C     WTTR          IFNE      *BLANKS
229600040323     C     WTTR          LOOKUP    TTR                                    34
229700040323     C                   MOVE      *IN34         W9DEFL            1
229800040323     C                   ELSE
229900040323     C                   MOVE      '0'           W9DEFL            1
230000040323    5C                   ENDIF
230100040323    4C                   ENDIF
230200040323    4c                   if        w9defl='0' and br9fgs<>simfel
230300110103     c*
230400040303     C                   MOVE      BR9PES        LNPB
230500970814     C                   CLEAR                   WTAB5F
230600020214     C                   Z-ADD     BRVDFV        DSPB
230700020214     C                   Z-ADD     BRVDFV        WDTRIF
230800960702     C                   SETON                                        33
230900020201    4C                   END
231000020131    3C                   END
231100070131     C  N33KBAR3         READE     FNBRV09L                               33
231200020131    2C                   ENDDO
231300020204     C**
231400020131    2C     WTAB5F        IFEQ      'S'
231500020218     C* CERCO SE C'E' IL P.O. CHE HA SPUNTATO, ALTRIMENTI IL PRIMO
231600020218     C                   MOVEA     F5F(YY)       WPO
231700020218     C                   Z-ADD     1             ZZ
231800020218     C                   CLEAR                   WTROVA
231900020218    3C                   DO        4             ZZ                3 0
232000020218    4C     WPO(ZZ)       IFGT      *ZEROS
232100020218     C                   MOVEL     WPO(ZZ)       W0030             3 0
232200020218    5C     W0030         IFEQ      BPES
232300020218     C                   MOVEL     W0030         LNPB
232400020218     C                   MOVEL     'S'           WTROVA
232500020218    5C                   ENDIF
232600020218    4C                   ENDIF
232700020218    3C                   ENDDO
232800020218     C**
232900020218     C* SE NON C'E' P.O. = BPES PRENDO IL PRIMO
233000020218     C     WTROVA        IFEQ      ' '
233100020218     C                   MOVEL     WPO(1)        LNPB
233200020218     C                   ENDIF
233300020214     C                   MOVEL     BARDFV        DSPB
233400020214     C                   MOVEL     BARDFV        WDTRIF
233500020131    2C                   ENDIF
233600970814    1C                   ENDIF
233700960702     C                   ENDSR
233800941206     C**************************************************************************
233900941206     C* CERCO IL RAGGRUPPAMENTO PISTOLA
234000941206     C**************************************************************************
234100921229     C     RICRAG        BEGSR
234200921229     C*
234300930305     C     SAVNPS        IFNE      ALF2
234400921229     C*
234500950515     C* HO CARICATO TUTTA SCHIERA
234600921229     C     *IN02         IFEQ      '0'
234700970417     C                   Z-ADD     1             Z                 3 0
234800930305     C                   MOVEL     ALF2          NUM2              2 0
234900970417     C     NUM2          LOOKUP    NPS(Z)                                 34
235000970417     C  N34              Z-ADD     1             Z
235100921229     C*
235200921229     C                   ELSE
235300921229     C*
235400921229     C* NON HO CARICATO LA SCHIERA --> FACCIO CHAIN
235500921229     C                   MOVEL     '7J'          COD
235600921229     C                   MOVEL     *BLANKS       KEY
235700930305     C                   MOVEL     ALF2          KEY
235800921229     C     KTAB          CHAIN     TABEL                              34
235900001023     C                   Z-ADD     2             Z
236000000316     C     *IN34         IFEQ      *OFF
236100000316     C                   MOVEL     TBLUNI        DS7J
236200000316     C                   MOVEL     §7JRPS        RPS(Z)
236300000316     C                   MOVEL     §7JORD        OPS(Z)
236400000316     C                   MOVEL     §7JTPS        TPS(Z)
236500000316     C                   MOVEL     §7JINV        INV(Z)
236600040503     C                   MOVEL     §7Jfsl        fsl(Z)
236700000316     C                   ELSE
236800000316     C                   MOVEL     'M'           RPS(Z)
236900000316     C                   MOVEL     999           OPS(Z)
237000000316     C                   MOVEL     'B'           TPS(Z)
237100000316     C                   MOVEL     ' '           INV(Z)
237200040503     C                   MOVEL     ' '           fsl(Z)
237300000316    1C                   END
237400921229     C                   END
237500921229     C*
237600930305     C                   MOVEL     ALF2          SAVNPS
237700921229     C                   END
237800941206     C*
237900921229     C                   ENDSR
238000941206     C**************************************************************************
238100941206     C* CHIUDO ANOMALIA
238200941206     C**************************************************************************
238300911018     C     CHIUAN        BEGSR
238400961211     C                   SETOFF                                       97
238500941206     C*
238600990518     C     KANM          CHAIN(N)  FNANM000                           97
238700961121     C**
238800961211    1C     *IN97         IFEQ      *OFF
238900970117     C**
239000970117     C** ANOMALIA 145/146: SE DATA SPUNTA > DATA ANOAMLIA
239100970117     C*  DEVO CONTROLLARE CHE NON CI SIANO ALTRE SPUNTE
239200970117     C*  CHE MI IMPEDISCONO LA CHIUSURA
239300970117     C     ANMCAA        IFEQ      145
239400970117     C     ANMCAA        OREQ      146
239500041013     c* Imposto econtrdfv per anomalie 145 146 che la deve sempre cotrollare
239600041013     c                   eval      wcontrdfv='S'
239700990518     C     BARDFV        IFGT      ANMDAO
239800970117     C                   EXSR      CTRSPU
239900970117     C                   ENDIF
240000970117     C                   ENDIF
240100961121     C**
240200041013     c     Wcontrdfv     ifne      'S'
240300041013     C* PER CATEGORIA no arrivi e partenza CHIUDO SE DATA >=
240400961122     C     WSTESS        OREQ      'S'
240500990518     C     BARDFV        ANDEQ     ANMDAO
240600041013     C     BARNPG        ANDne     2
240700041013     C     BARNPG        ANDne     1
240800041013     C* PER LE CATEGORIE arrivi SOLO SE DATA > e la posso chiudere
240900990518     C     BARDFV        ORGT      ANMDAO
241000970117     C     WCHIU         ANDEQ     'S'
241100961211     C*
241200961211     C     WCREA         OREQ      'N'
241300020213     C     BARFLE        ANDNE     BTFP
241400961121     C**
241500960416     C                   CLEAR                   DSLR33
241600960524     C                   MOVE      BARDFV        D33DCH
241700960514    2C     BARKEY        IFNE      SAVFOG
241800911125     C                   EXSR      RICFAS
241900960514    2C                   END
242000960514     C*
242100960416     C                   MOVEL     COMFAS        D33FSC
242200960416     C                   MOVEL     SAVCCH        D33CCH
242300960530     C                   MOVEL     SAVMAC        D33MAC
242400960416     C                   MOVE      ANMNR2        D33NRR
242500960416     C*
242600990712     C                   CALL      'FNLR33R'
242700960416     C                   PARM                    DSLR33
242800961209     C*
242900961209     C* PER ANOMALIE 145 E 146 CREO SE E' DA CREARE L'ANOMALIA 620
243000020212     C*  AOLO SE NON SONO IN CREAZIONE DELL 600
243100020212    2C     NTWBAR        IFEQ      'EEX'
243200961210     C     W620          ANDEQ     'S'
243300060322    2C     NTWBAR        oreq      'DPD'
243400060322     C     W620          ANDEQ     'S'
243500060322    2C     NTWBAR        oreq      'FED'
243600060322     C     W620          ANDEQ     'S'
243700961210     C*
243800961210    3C     ANMCAA        IFEQ      145
243900961209     C     ANMCAA        OREQ      146
244000961210     C*
244100961210    4C     W60X          IFEQ      'S'
244200961210     C                   Z-ADD     600           COMCAA
244300001110     C     COMCAA        LOOKUP    ACG                                    34
244400001110    5C     BARSPG        IFNE      'I'
244500001110     C     *IN34         OREQ      *ON
244600990518     C     KANM          DELETE    FNANM02L                           34
244700001110    5C                   ENDIF
244800961209     C                   Z-ADD     620           COMCAA
244900961209     C                   EXSR      CRTANM
245000961209     C                   CLEAR                   W60X
245100961210    4C                   ENDIF
245200961209     C*
245300961209     C* VEDO SE CANCELLARE L'ANOMALIA 645
245400961209     C                   Z-ADD     645           COMCAA
245500001110     C     COMCAA        LOOKUP    ACG                                    34
245600001110   4AC     BARSPG        IFNE      'I'
245700001110     C     *IN34         OREQ      *ON
245800990518     C     KANM          CHAIN     FNANM02L                           34
245900961210    4C     *IN34         DOWEQ     *OFF
246000990518    5C     ANMDAO        IFEQ      BARDFV
246100990518     C                   DELETE    FNANM000
246200961209     C                   SETON                                        34
246300961209     C                   ELSE
246400990518     C     KANM          READE     FNANM02L                               34
246500961210    5C                   ENDIF
246600961210    4C                   ENDDO
246700001110   4AC                   ENDIF
246800961210    3C                   ENDIF
246900961209     C*
247000961210    2C                   ENDIF
247100960514     C*
247200960514     C* L'ANOMALIA 05 PUO' ESSERCI PIU' VOLTE
247300961209    2C     ANMCAA        IFEQ      005
247400980709     C                   SETOFF                                       34
247500960514    3C     *IN34         DOWEQ     *OFF
247600990518     C     KANM          READE(N)  FNANM000                               34
247700960514     C                   MOVE      ANMNR2        D33NRR
247800960514     C*
247900960514     C  N34              CALL      'FNLR33R'
248000960514     C                   PARM                    DSLR33
248100960514    3C                   ENDDO
248200911018     C*
248300960514    2C                   ENDIF
248400961121   1AC                   ENDIF
248500961209   X1C                   ELSE
248600961209     C*
248700961211     C* SOLO PER ANOMALIE 145 E 146
248800961210    2C     COMCAA        IFEQ      145
248900961210     C     COMCAA        OREQ      146
249000991230     C* SE DEVO CREARE EVENTUALE 620 PER LNA ESTERA
249100020212    3C     W60X          IFEQ      'S'
249200961210     C     W620          ANDEQ     'S'
249300060322     C* E LA DEVO CREARE SIA SE TROVO CHE SE NON TROVO L'ANOMALIA 145
249400060322     C*  O 146 DA CHIUDERE  (WESAN =' ')
249500060322     C     WESAN         ANDEQ     ' '
249600110103     c*
249700060322    4C     NTWBAR        ifeq      'EEX'
249800060322     C     NTWBAR        oreq      'DPD'
249900060322     C     NTWBAR        oreq      'EEX'
250000961209     C                   Z-ADD     620           COMCAA
250100961209     C                   EXSR      CRTANM
250200961209     C                   CLEAR                   W60X
250300060322    4C                   ENDIF
250400060322    3C                   ENDIF
250500961210    2C                   ENDIF
250600961210    1C                   ENDIF
250700110103     c*
250800041013     c                   eval      wcontrdfv='N'
250900911018     C*
251000110929     c                   clear                   savmac
251100110929     c                   clear                   savcch
251200911018     C                   ENDSR
251300941206     C**************************************************************************
251400941206     C*    RICERCA FASE
251500941206     C**************************************************************************
251600911125     C     RICFAS        BEGSR
251700941206     C*
251800921020     C                   MOVEL     '7N'          COD
251900921020     C                   MOVEL     *BLANKS       KEY
252000921020     C                   MOVEL     BARNPG        KEY
252100921105     C     KTAB          CHAIN     TABEL                              41
252200921105     C  N41              MOVEL     TBLUNI        DS7N
252300000516     C*
252400000516     C     BARSPG        IFNE      ' '
252500000516     C                   MOVEL     §7NFS1        WFAS1             1
252600000516     C                   MOVEL     §7NFS2        WFAS2             1
252700000516     C                   MOVEL     §7NFS3        WFAS3             1
252800000516     C                   MOVEL     §7NFS4        WFAS4             1
252900000516     C                   MOVEL     §7NFS5        WFAS5             1
253000000516     C                   ENDIF
253100921021     C*
253200960524     C     BARSPG        IFEQ      §7NSC1
253300921021     C                   MOVEL     §7NFS1        COMFAS            1
253400921021     C                   ELSE
253500960524     C     BARSPG        IFEQ      §7NSC2
253600921021     C                   MOVEL     §7NFS2        COMFAS
253700921021     C                   ELSE
253800960524     C     BARSPG        IFEQ      §7NSC3
253900921021     C                   MOVEL     §7NFS3        COMFAS
254000921021     C                   ELSE
254100960524     C     BARSPG        IFEQ      §7NSC4
254200921021     C                   MOVEL     §7NFS4        COMFAS
254300921021     C                   ELSE
254400921021     C                   MOVEL     §7NFS5        COMFAS
254500921021     C                   END
254600921021     C                   END
254700921021     C                   END
254800921021     C                   END
254900921020     C*
255000961217     C                   MOVEL     BARKEY        SAVFOG            8 0
255100941206     C*
255200911125     C                   ENDSR
255300950927     C**************************************************************************
255400950927     C*    CONTROLLO SE CREARE ANOMALIA 50: COLLO ARRIVATO NON PARTITO
255500130228     C*                      O ANOMALIA 55/56/57: DISGUIDO - ESTERNA
255600950927     C**************************************************************************
255700950927     C     CRTA50        BEGSR
255800000320     C                   CLEAR                   WCCH
255900020214     C* PER ANOMALIA 50 NON CREO SE ESISTE GIA' LA DATA DI PARTENZA
256000020214     C*  BOLLE TRANSITO
256100020214     C     WTIBOL        IFEQ      'T'
256200020214     C     COMDFV        ANDGT     0
256300020214     C                   GOTO      NOA50
256400020214     C                   ENDIF
256500020214     C**
256600020214     C     WTIBOL        IFEQ      'A'
256700020218     C     WTIBOL        OREQ      'L'
256800050215     c* Per l'anoamlia 50 basterebbe questo test senza leggere le spunte,
256900050215     c*  le spunte le devo leggere lo stesso per i manca record
257000020214     C     WESART        IFEQ      'S'
257100020214     C     COMDFV        ANDGT     0
257200020214     C                   GOTO      NOA50
257300020214     C                   ENDIF
257400020214     C* SE E' COLLO IN ARRIVO E BOLLA CONSEGNATA --> NON CREO
257500020214     C     WESART        IFEQ      'S'
257600020214     C     ARTDCM        ANDGT     0
257700020214     C                   GOTO      NOA50
257800020214     C                   ENDIF
257900020218     C* SE BOLLA LOCALE E NON C'E' LA BOLLA NON FACCIO NIENTE
258000020218     C     WTIBOL        IFEQ      'L'
258100020218     C     WBOPAR        ANDEQ     'N'
258200020218     C                   GOTO      NOA50
258300020218     C                   ENDIF
258400020214     C                   ENDIF
258500020214     C**
258600950927     C* VERIFICO SE ESISTE GIA'
258700040429     c* ora la esistenza è all'interno dello stesso p.o. e non in assoluto
258800950927    1C     WTIBOL        IFEQ      'D'
258900950927     C                   Z-ADD     55            COMCAA
259000950927     C                   ELSE
259100950927     C                   Z-ADD     50            COMCAA
259200950927    1C                   ENDIF
259300990409     C* SE LA CATEGORIA E' IMA IMG O CONS INVECE CHE 55 CREO LA 56
259400020214    1C     COMCAA        IFEQ      55
259500020214    2C     BARNPG        IFEQ      4
259600990412     C     BARNPG        OREQ      3
259700990409     C     BARSPG        ANDEQ     'A'
259800990412     C     BARNPG        OREQ      3
259900990409     C     BARSPG        ANDEQ     'G'
260000161021     C     BARNPG        OREQ      3
260100161021     C     BARSPG        ANDEQ     'Z'
260200990409     C                   Z-ADD     56            COMCAA
260300020214    2C                   ENDIF
260400020214    1C                   ENDIF
260500130228     c* se Anomalia 55 a collo che può partire da diverse filiali
260600130228     c*  e chi spunta è una di qeuste --> non creo 55 ma 57 Disguido AUTORIZZ
260700130228     c*  solo per ategorie di spunta partenza
260800130228     c                   if        comcaa=55  and
260900130228     c                             (bolxco='W' or
261000130228     c                             (wesbtt='N' and wesblt='N' and wesart='N'
261100130228     c                              and barnrs>0))
261200130228     c* se si tratta di spunta partenza --> creo la 57
261300130228     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
261400130228     c                             or barnpg=1
261500130228     c                   exsr      CtrXCO
261600130228     c                   if        Indx>0
261700130301     c                   z-add     57            comcaa
261800130228     c                   endif
261900130228     c                   endif
262000130228     c                   endif
262100130301     c
262200130301     c* In caso di anomalia 50 manca bolla --> verifico se non è da creare
262300130301     c*  per collo spuntabile in questo TFP e "locale"
262400130301     c                   if        comcaa=50 and
262500130301     c                             (bolxco='W' or
262600130301     c                             (wesbtt='N' and wesblt='N' and wesart='N'
262700130301     c                              and barnrs>0))
262800130301     c* se si tratta di spunta di partenza --> non creo la 50 ma la 57
262900130301     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
263000130301     c                             or barnpg=1
263100130301     c                   exsr      CtrXCO
263200130301     c                   if        Indx>0
263300130301     c                   z-add     57            comcaa
263400130301     c                   endif
263500130301     c                   endif
263600130301     c                   endif
263700971009     C*
263800130228     c* per anomalia 55 o 56 57 valida l'anomalia per p.o. spunta
263900040429     c* per anomalia 50:
264000040429     c* per bolle transito solo se spara il p.o. transito
264100040429     c* per bolle arrivi, se trovo anomalia nell'area di arrivo(e non
264200040429     c*  di un eventuale p.o. transito)
264300040429     c                   clear                   wtrova
264400040429     C     KANM          SETLL     FNANM000
264500040504     C     KANM          reade(N)  FNANM000                               34
264600040429    1c                   dow       not*in34 and wtrova=*blanks
264700040429    2c                   if        anmcaa<>50 or wtibol='T'
264800040429    3c                   if        anmfle=bpes
264900040429     c                   eval      wtrova='1'
265000040429   x3c                   else
265100040504     C     KANM          reade(N)  FNANM000                               34
265200040429    3c                   endif
265300110103     c*
265400040429   x2c                   else
265500040429     C                   CLEAR                   DSLV55
265600040429     C                   MOVEL     'A'           D55TPT
265700040429     C                   Z-ADD     anmfle        D55LIN
265800040429     C                   Z-ADD     lnpb          D55lnp
265900040429     C                   Z-ADD     bardfv        D55DRF
266000040429     C                   EXSR      FNLV55
266100040429    3c                   if        d55tfa=pestfa
266200040429     c                   eval      wtrova='1'
266300040429   x3c                   else
266400040504     C     KANM          reade(N)  FNANM000                               34
266500040429    3c                   endif
266600040429    2c                   endif
266700110103     c*
266800040429    1c                   enddo
266900990412     C*
267000990412     C* PER ANOMALIA 55 VERIFICO ANCHE CHE NON CI SIA LA 56
267100990412     C*  PERCHE' DOPO AVER SPARATO UN COLLO IN IMA PUO' ESSERE
267200990412     C*  SPARATO IN PARTENZA PER FARLO RIPARTIRE
267300040429    1C     *IN34         IFEQ      *On
267400990412     C     COMCAA        ANDEQ     55
267500990412     C                   Z-ADD     56            COMCAA
267600040429     C     KANM          SETLL     FNANM000
267700040504     C     KANM          reade(N)  FNANM000                               34
267800040429     c                   dow       not*in34 and wtrova=*blanks
267900040429     c                   if        anmfle=bpes
268000040429     c                   eval      wtrova='1'
268100040429     c                   else
268200040504     C     KANM          reade(N)  FNANM000                               34
268300040429     c                   endif
268400040429     c                   enddo
268500990412     C                   Z-ADD     55            COMCAA
268600990412    1C                   ENDIF
268700950927     C*
268800040429    1C     *IN34         IFEQ      *On
268900970828     C* NON CREO ANOMALIA 55 SE C'E' UNA SPUNTA PARTENZA DI ALTRO AS
269000970828     C*  PERCHE' NON SI TRATTA PIU' DI UN DISGUIDO MA O E' COLLO
269100990409     C*  IN TRANSITO DI CUI NON HO RICEVUTO LA BOLLA
269200990409     C*  O E' COLLO DISGUIDATO IN ALTRA FIL CHE PER ANDARE IN ARRIVO
269300970828     C*  DEVE PASSARE DA ALTRA FIL ANCORA
269400970828     C*  LA 200 VIENE INVECE CREATA LO STESSO PERCHE' UNA ANOMALIA
269500970828     C*  GESTIONALE CHE SERVE PER FAR RIPARTIRE AUTOMATICAMENTE IL COLL
269600970828     C*  ANCHE SE NON VIENE SPARATO NE' IN PARTENZA NE' IN ENTRATA
269700000316     C                   EXSR      CNOIDD
269800950927    1C                   ENDIF
269900020214     C     NOA50         ENDSR
270000000316     C**************************************************************************
270100000316     C* RIEMPO ALCUNI CAMPI ANOMALIA NO IDD
270200000316     C**************************************************************************
270300000316     C     CNOIDD        BEGSR
270400160624     C** ANOMALIE 50 -->
270500160725     C* VERIFICO SE ESISTE UNA SPUNTA PARTENZA PER QUEL COLLO
270600160624     C*  SE SI NON CREO ANOMALIA PER 50
270700130228     C*  ANOMALIE 55/56/57 --> CONSIDERO SOLO LA SPUNTA "D-DISGUIDO"
270800020214     C*                     PER NON CREARLE
270900000316     C                   Z-ADD     1             WNPG
271000000317     C                   CLEAR                   WTROVA
271100021121     C* CAMPI DI COMODO PER ANOM 50 per transito su stessa £1
271200021121     C                   CLEAR                   WFVALNP
271300021121     C                   CLEAR                   WFVANFV
271400021121     C                   CLEAR                   WFVADFV
271500110103     c*
271600070131     C     KBAR3         CHAIN     FNBRV09L                           99
271700020214    1C     *IN99         DOWEQ     *OFF
271800000317     C     WTROVA        ANDEQ     ' '
271900050215     c                   clear                   wrileggi          1
272000020214     C*****
272100020214     C* SOLO SE LA SPUNTA TROVATA E' DI DISGUIDO, ALTRIMENTI SE NORMALE
272200020214     C*  E' UNA SPUNTA PREVISTA, QUINDI DA IGNORARE
272300020214    2C     COMCAA        IFEQ      55
272400020214     C     COMCAA        OREQ      56
272500130228     C     COMCAA        OREQ      57
272600040303    3C     BR9FGS        IFNE      BTFP
272700040429     c* verifico tramite pgm fnlv53r se spunta di foglio diretto a me(BPES)
272800040429     c*  se è diretto a me non creo anomalia
272900040429     c*  Questo vale sia per colli in disguido che colli in transito
273000040429     c*  manca record bolla partenza
273100040430     c                   clear                   dslv53
273200040504     c                   movel     'T'           d53tfo
273300040429     C                   MOVEL     BR9NPG        D53NPG
273400040429     C                   Z-ADD     BR9NFV        D53NFV
273500040429     C                   MOVEL     BR9FGS        D53FGS
273600040429     C                   MOVEL     BR9FLE        D53FLF
273700040504     C                   MOVEL     'A'           D53ABB
273800040429     C                   MOVEL     br9lna        D53LNA
273900050603     C                   MOVEL     Usalnatfa     D53TFA
274000040504     C                   MOVEL     bpes          D53lai
274100040429     C                   CALL      'FNLV53R'
274200040429     C                   PARM                    DSLV53
274300040429     c* se non ci sono errori --> OK esco dal ciclo
274400040429    4c                   if        d53err=*blanks
274500000317     C                   MOVEL     '1'           WTROVA            1
274600050418   x4c                   else
274700050418     C                   eval      wrileggi='1'
274800040429    4c                   endif
274900020214   X3C                   ELSE
275000050215     C                   eval      wrileggi='1'
275100020214    3C                   ENDIF
275200020214   X2C                   ELSE
275300020214     C**
275400031204     c** bolla non locale: cerco spunta non mia
275500031204   2ac     WSPLOC        ifeq      *blanks
275600040303    3C     BR9FGS        IFNE      BTFP
275700050329     c* Deve essere una spunta trasmessa, altrimenti ignoro (scaricata
275800050329     c*  tardi)
275900050329     c     br9ftr        andne     *blanks
276000110103     c*
276100050222    4c                   if        comcaa=50
276200050215     c                   clear                   dslv53
276300050322     c                   if        wtibol='T'
276400050322     c                   movel     'T'           d53tfo
276500050322     c                   else
276600050215     c                   movel     'A'           d53tfo
276700050322     c                   endif
276800050215     C                   MOVEL     BR9NPG        D53NPG
276900050215     C                   Z-ADD     BR9NFV        D53NFV
277000050215     C                   MOVEL     BR9FGS        D53FGS
277100050215     C                   MOVEL     BR9FLE        D53FLF
277200050215     C                   MOVEL     'A'           D53ABB
277300050215     C                   MOVEL     br9lna        D53LNA
277400050603     C                   MOVEL     Usalnatfa     D53TFA
277500050215     C                   MOVEL     pestfa        D53lai
277600050215     C                   CALL      'FNLV53R'
277700050215     C                   PARM                    DSLV53
277800050215     c* se non ci sono errori --> OK esco dal ciclo
277900050222    5c                   if        d53err=*blanks
278000020214     C                   MOVEL     '1'           WTROVA            1
278100050222   X5C                   ELSE
278200050418     c* Se si tratta di errore di foglio non trovato e provengo da
278300050418     c*  aggiornamento bolla transito Ex disguido (richiamo da fnlr13r)
278400050418     c*  il foglio si potrebbe trovare in fnfva01r, er cui conttrollo da
278500050418     c*  FNFGV
278600070221    6c                   if        d53err='2' and dls45FLG='T'
278700050418     c                   movel     'G'           d53tfo
278800050418     C                   CALL      'FNLV53R'
278900050418     C                   PARM                    DSLV53
279000050418     c* se non ci sono errori --> OK esco dal ciclo
279100050418    7c                   if        d53err=*blanks
279200050418     C                   MOVEL     '1'           WTROVA            1
279300050418   x7c                   else
279400050418     C                   eval      wrileggi='1'
279500050418    7c                   endif
279600050418   x6c                   else
279700050418     C                   eval      wrileggi='1'
279800050418    6c                   endif
279900050222    5C                   ENDIF
280000110103     c*
280100050222     c* Per anom 60/61 se trovata spunta --> ok
280200160624   X4C**                 ELSE
280300160624     C**                 MOVEL     '1'           WTROVA            1
280400050222     c                   endif
280500110103     c*
280600020214   X3C                   ELSE
280700050215     C                   eval      wrileggi='1'
280800020214    3C                   ENDIF
280900110103     c*
281000031204   2ac                   else
281100031204     c** bolla     locale: cerco spunta mia
281200040303    3C     BR9FGS        IFeq      BTFP
281300031204     C                   MOVEL     '1'           WTROVA            1
281400031204   X3C                   ELSE
281500050215     C                   eval      wrileggi='1'
281600031204    3C                   ENDIF
281700031204   2aC                   ENDIF
281800031204    2C                   ENDIF
281900050215     c
282000050215     c                   if        wrileggi='1'
282100070131     C     KBAR3         READE     FNBRV09L                               99
282200050215     c                   endif
282300020214    1C                   ENDDO
282400160725     c*
282500160725     c* Se non trovata e si tratta di bolla locale: cerco spunta
282600160725     c*  entrata
282700160725     c                   if        wtrova=' ' and wsploc=*blanks
282800160725     C                   Z-ADD     5             WNPG
282900160725     C     KBAR3         chain     FNBRV09L
283000160725     c                   if        %found(fnbrv09l) and br9fgs=btfp
283100160725     C                   MOVEL     '1'           WTROVA            1
283200160725     c                   setoff                                       99
283300160725     c                   endif
283400160725     c                   endif
283500000316     C*
283600000316     C* 99 ON  - SPUNTA NON TROVATA --> CREO ANOMALIA PER 50/55/56
283700000316    0C     *IN99         IFEQ      *ON
283800000316     C     COMCAA        ANDNE     60
283900000614     C     COMCAA        ANDNE     61
284000000316     C* 99 OFF - SPUNTA     TROVATA --> CREO ANOMALIA PER 60
284100160624     C**   *IN99         OREQ      *OFF
284200160624     C**   COMCAA        ANDEQ     60
284300160624     C**   *IN99         OREQ      *OFF
284400160624     C**   COMCAA        ANDEQ     61
284500000316     C                   CLEAR                   FNANM000
284600170505     c***
284700000316     C* MEMORIZZO NUMERO SPEDIZIONE SU ANOMALIA SE C'E'
284800170505    1C***                SELECT
284900170505     C***  WESBTT        wheneq    'S'
285000170505     C***                Z-ADD     BTTAAS        ANMAAS
285100170505     C***                Z-ADD     BTTLNP        ANMLNP
285200170505     C***                Z-ADD     BTTNSP        ANMNSP
285300170505     C***  WESART        wheneq    'S'
285400170505     C***                Z-ADD     ARTAAS        ANMAAS
285500170505     C***                Z-ADD     ARTLNP        ANMLNP
285600170505     C***                Z-ADD     ARTNSP        ANMNSP
285700170505     C***  WESblt        wheneq    'S'
285800170505     C***                Z-ADD     bltAAS        ANMAAS
285900170505     C***                Z-ADD     bltLNP        ANMLNP
286000170505     C***                Z-ADD     bltNSP        ANMNSP
286100170505    1C***                ENDSL
286200170505
286300170505     c                   if        comnsp>0
286400170505     C                   Z-ADD     comAAS        ANMAAS
286500170505     C                   Z-ADD     comLNP        ANMLNP
286600170505     C                   Z-ADD     comNSP        ANMNSP
286700170505     c                   endif
286800040621     c* Se c'e' la spedizione verifico se spedizione di valore
286900110103     c*
287000040621     C* linea partenza segnacollo
287100000316     C                   MOVEL     BARLNP        ANMFLS
287200000316     C* PER ANOMALIA 50
287300000316    1C     COMCAA        IFEQ      50
287400000316     C                   Z-ADD     BARFGS        ANMFLE
287500040908     C* PER BOLLA LOCALE non c'e' abbinamento di foglio per cui deduco le
287600040908     c*  date damettere sulla anomalia
287700040908     c                   clear                   wnofva            1
287800040908     c                   if        wtibol='L'
287900040908     c                   eval      wnofva='S'
288000040908     c                   endif
288100170505     c* Per sicurezza, per essere sicura di non aver sporcato i campi di ARB
288200170505     c*  uso i campi salvati
288300170505     c****               if        wesart='S' and arbtfp=arbtfa
288400170505     c                   if        wesart='S' and lnptfp=boltfa
288500040908     c                   eval      wnofva='S'
288600040908     c                   endif
288700050603     c                   if        wesart='N' and lnptfp=Usalnatfa
288800040908     c                   eval      wnofva='S'
288900040908     c                   endif
289000040908     c
289100040908   1ac                   if        wnofva='S'
289200040908    2c                   if        wesart='S'
289300170509     C                   Z-ADD     bolDBR        SAVDAO
289400170509     C                   Z-ADD     bolDBR        ANMDAO
289500020218     C                   Z-ADD     99998         ANMNFV
289600020218     C                   Z-ADD     LNPTFP        ANMCDU
289700020218     C                   Z-ADD     LNPTFP        WFL1
289800020218     C                   MOVEL     'L'           WFLGT
289900040908     c*
290000040908   x2c                   else
290100040908     c* Se non c'e' bolla cerco il 1° foglio locale con data < data spunta
290200040908     c     kfgv2         setll     fnfgv02l
290300040908     c     lnptfp        readpe    fnfgv02l
290400040908    3c                   dow       not %eof(fnfgv02l)  and
290500040908     c                             fgvttr <>'L'
290600040908     c     lnptfp        readpe    fnfgv02l
290700040908    3c                   enddo
290800110103     c*
290900040908    3c                   if        not %eof(fnfgv02l)
291000040908     C                   Z-ADD     fgvdfv        SAVDAO
291100040908     C                   Z-ADD     fgvdfv        ANMDAO
291200040908     C                   Z-ADD     99998         ANMNFV
291300040908     C                   Z-ADD     LNPTFP        ANMCDU
291400040908     C                   Z-ADD     LNPTFP        WFL1
291500040908     C                   MOVEL     'L'           WFLGT
291600040908    3c                   endif
291700040908    2c                   endif
291800110103     c*
291900040908  x1aC                   ELSE
292000070208     C* SE TROVA LA BOLLA IN BLP C'E' SEMPRE LNP BOLLA
292100020214     C*  ALTRIMENTI LA 5F ALTRIMENTI BARLNP
292200020214     C                   MOVEL     LNPB          WLIN
292300020214     C* IMPOSTO LA DATA DI RIFERIMENTO PER LA RICERCA DEL TERMINAL
292400020214     C*  DI PARTENZA: LA PRIMA VOLTA E' LA DATA DELLA BOLLA SE C'E'
292500020214     C                   MOVE      DSPB          WDTRIF
292600000316     C*
292700000316     C* CERCO IL NUMERO DI FV SU CUI DOVREBBE ESSERE PARTITO IL COLLO
292800000316     C                   MOVE      *BLANKS       WFLGT             1
292900000316     C                   CLEAR                   WLINPR
293000000316     C                   CLEAR                   WVALNP
293100000316    2C     WLIN          DOWGT     0
293200000316     C                   EXSR      RICFV
293300000316     C* MEMORIZZO IL P.O. DI TRANSITO E LA LINEA DI PARTENZA DEL FV
293400000316     C     WLIN          IFGT      *ZEROS
293500000316     C                   Z-ADD     WLIN          WLINPR
293600000316     C                   Z-ADD     FVALNP        WVALNP
293700000316     C                   ENDIF
293800000316    2C                   ENDDO
293900000316    2C     ANMCDU        IFGT      0
294000000316     C                   MOVEL     ANMCDU        WFL1
294100000316   X2C                   ELSE
294200070208     C* SE TROVA LA BOLLA IN BLP C'E' SEMPRE LNP BOLLA
294300020214     C*  ALTRIMENTI LA 5F ALTRIMENTI BARLNP
294400000316     C                   MOVEL     LNPB          WFL1
294500000316    2C                   ENDIF
294600020218   1AC                   ENDIF
294700000316     C*
294800000316   X1C                   ELSE
294900160624     C* PER ANOMALIA 55/56/57/      (NO IDD LE ALTRE)
295000000316     C                   Z-ADD     BARDFV        ANMDAO
295100000316     C                   MOVEL     BARFGS        ANMFLE
295200000316     C                   MOVEL     LNPB          WFL1
295300000316     C                   Z-ADD     BARNFV        ANMNFV
295400000316     C                   MOVEL     BARFGS        ANMCDU
295500000316    1C                   ENDIF
295600000316     C* SCRIVO L'ANOMALIA
295700000316     C                   EXSR      RIEANM
295800040621     c*
295900040621     c                   if        anmnsp>0
296000091209     c     kar5          chain(n)  fiar501l
296100040621    3c                   if        %found(fiar501l)
296200040621     c                   movel     ar5uni        dar5gen
296300040621     c                   else
296400040621     c                   clear                   dar5gen
296500040621    3c                   endif
296600040621     c                   movel     §ar5bva       anmft4
296700040621     c                   endif
296800000614     C*
296900000316     C* ESTERNA
297000000614    1C     §7CAIE        IFEQ      'E'
297100000316     C                   MOVEL     WFL1          ANMFL1
297200000614    1C                   ENDIF
297300000316     C* SCRIVO L'ANOMALIA SOLO SE HO TROVATO IL FOGLIO PARTENZA PER"50"
297400000614    1C     ANMCAA        IFNE      50
297500000316     C     ANMCAA        OREQ      50
297600000316     C     ANMNFV        ANDGT     0
297700000316     C* SE LA KEY E' PER SEGNACOLLO ED IL NUMERO E' 0 NON CREO L'ANOMAL
297800000316    2C     §7CKEY        IFNE      'S'
297900000316     C     §7CKEY        OREQ      'S'
298000000316     C     ANMSCN        ANDGT     0
298100001110     C************************
298200001110     C* CREO ANOMALIA SOLO SE:
298300001110     C************************
298400070215     C* ANOMALIA CREABILE ANCHE DA CAT.SPUNTA GENERICA
298500070215     C*                                 (ANCHE DA SPUNTA IMI)
298600070215     C     §7CCCG        ifeq      'S'
298700070215     C* CATEGORIA SPUNTA NON GENERICA   (NON È UNA SPUNTA IMI)
298800070215     C     BARSPG        orne      'I'
298900000316     C                   WRITE     FNANM000
299000001110     C*
299100001110     C* CHIUDO SE POSSIBILE UNA EVENTUALE 55 GIA' CREATA E POI CREO
299200001110     C*  LA 56
299300001110    3C     COMCAA        IFEQ      56
299400001110     C                   Z-ADD     55            COMCAA
299500001110     C                   MOVEL     'S'           SAVMAC
299600001110     C                   EXSR      CHIUAN
299700001110     C                   CLEAR                   SAVMAC
299800001110    3C                   ENDIF
299900000614    3C                   ENDIF
300000000614     C*
300100000316     C* PER ANOMALIA 50 CREATA AGGIORNO ANCHE LA DATA DI PARTENZA
300200000316     C* O LA DATA DI USCITA DAL TRANSITO
300300000316     C     ANMCAA        IFEQ      50
300400000316     C                   EXSR      AGGPAR
300500000316     C                   ENDIF
300600160727     C     ANMCAA        IFEQ      55
300700160727     C     ANMCAA        oreq      56
300800160727     C                   EXSR      AGGPAR_d
300900160727     C                   ENDIF
301000000316     C*
301100000316    2C                   ENDIF
301200020222     C*
301300020222     C                   ELSE
301400020222     C* SE NON TROVATO FOGLIO ATTENDO CHE ARRIVI (CI DEVE ESSERE PRIMA
301500031204     C*  O POI) se non locale
301600060103     c* Solo se non richiamato
301700060103     c  N02wtibol        ifne      'L'
301800020222     C                   SETON                                        3940
301900031204     c                   endif
302000000614    1C                   ENDIF
302100000614    0C                   ENDIF
302200000316     C                   ENDSR
302300130228     C**************************************************************************
302400130228     C* controllo se bolla con colli chde partono da divderse filiali
302500130228     C**************************************************************************
302600130228     C     CtrXCO        BEGSR
302700130228     c                   eval      Wflsnrs=(BARLNP*100)+BARnrs
302800130228
302900130228    1c                   if        wflsnrs<>savfnrs
303000130228     c                   clear                   xcotfp
303100130228     C                   MOVEL     'XCO'         kcod
303200130228     C                   eval      kke1='ALTRITFP'
303300130228     C                   eval      kke2=%editc(wflsnrs:'X')
303400130228     c     ktbe2         chain     tntbe01l
303500130228    2c                   if        %found(tntbe01l) AND tbeatb=' '
303600130228     C                   MOVEA     TBeUNI        xcotfp
303700130228    2c                   endif
303800130228     c
303900130228     c                   eval      savfnrs=wflsnrs
304000130228    1c                   endif
304100130228    c
304200130228     C* SE la fil spunte è compresa --> creo la 57
304300130228     c                   movel     barfgs        alfafgs           3
304400130228     c                   eval      Indx=%lookup(alfafgs:xcotfp)
304500130228     c
304600130228     c*
304700130228     C                   ENDSR
304800950927     C**************************************************************************
304900950927     C* RICERCA FV SU CUI E' PARTITA LA SPUNTA
305000950927     C**************************************************************************
305100950927     C     RICFV         BEGSR
305200950927     C                   CLEAR                   WMODL
305300021121     C                   movel     'N'           Wnoabb2           1
305400021121     C                   movel     'N'           Wnoabb6           1
305500950927     C* PRENDO IL TERMINAL DI PARTENZA DELLA LINEA DI PARTENZA
305600970814     C                   CLEAR                   DSLV55
305700970814     C                   MOVEL     'P'           D55TPT
305800970814     C                   MOVEL     WLIN          D55LIN
305900020214     C                   MOVEL     WDTRIF        D55DRF
306000020213     C                   EXSR      FNLV55
306100970814     C                   MOVEL     D55TFP        WPTFP
306200110103     c*
306300100713     C* PRENDO IL TERMINAL DI arrivo generico della linea
306400100713     C                   CLEAR                   DSLV55
306500100713     C                   MOVEL     'A'           D55TPT
306600100713     C                   MOVEL     barlna        D55LIN
306700100713     C                   MOVEL     WDTRIF        D55DRF
306800100713     C                   EXSR      FNLV55
306900100713     C                   MOVEL     D55TFa        WGENlnaTFA
307000021121     c
307100021121     c* se WPTFP = simfel ed ho salvato i campi del foglio illuminato
307200021121     c*  trovato, li considero validi e non rieseguo la routine
307300021121     c                   if        wptfp=simfel  and wfvalnp>0
307400021121     C                   MOVEL     wFVALNP       ANMCDU
307500021121     C                   Z-ADD     wFVALNP       SAVFLP
307600021121     C                   Z-ADD     wFVANFV       ANMNFV
307700021121     C                   Z-ADD     wFVADFV       ANMDAO
307800021121     C                   Z-ADD     wFVADFV       SAVDAO
307900021121     C                   CLEAR                   WLIN
308000021121     C                   MOVEL     '1'           WMODL             1
308100021121     c                   else
308200950927     C*
308300950927     C* CONTROLLO I FV ABBINATI, SE E' FOGLIO ARRIVI
308400020214     C     BARNPG        IFEQ      2
308500950927     C                   Z-ADD     BARNPG        KNPG4
308600950927     C                   Z-ADD     BARNFV        KNFV4
308700020214     C                   Z-ADD     BARFGS        KFGS4
308800950927     C                   EXSR      REDFVA
308900020214     C                   ENDIF
309000950927     C*
309100950927     C* SE NON L'HO TROVATO CERCO UN ALTRO F.ARRIVI DEL GIORNO
309200950927     C     WMODL         IFEQ      ' '
309300021121     c                   eval      wdata=bardfv
309400950927     C                   EXSR      MEMFAR
309500021121     C* MEMORIZZO LA NUOVA DATA
309600021121     C                   MOVEL     BARDFV        SAVDFV
309700110103     C                   Z-ADD     1             I                 3 0
309800950927     C*
309900950927    2C     PG2(I)        DOWGT     0
310000950927     C     ANMNFV        ANDEQ     0
310100950927     C                   Z-ADD     2             KNPG4
310200950927     C                   Z-ADD     PG2(I)        KNFV4
310300020214     C                   Z-ADD     SAVTFA        KFGS4
310400950927     C                   EXSR      REDFVA
310500021121     c*
310600021121     c* Memorizzo se nessuno dei fogli 2 ha dei fogli partenza abbinati
310700021121     c                   if        wabbinati=' '
310800021121     c                   clear                   wnoabb2
310900021121     c                   endif
311000950927     C                   ADD       1             I
311100950927    2C                   ENDDO
311200950927    1C                   ENDIF
311300110103     c*
311400021121     c* Se non ho fogli abbianti e non si tratta di cat arrivi, ritento
311500021121     c*  la ricerca dei fogli arrivi
311600021121    1c     WMODL         IFEQ      ' '
311700110103    2c                   if        barnpg<> 2 and
311800110103     c                             wnoabb2 ='N' and
311900021121     c                             wsavdata=bardfv
312000021121     c* Data - 1
312100021121     C                   MOVEL     bardfv        G02INV
312200021121     C                   Z-ADD     *ZEROS        G02DAT
312300021121     C                   Z-ADD     *ZEROS        G02TGI
312400021121     C                   MOVEL     '3'           G02ERR
312500021121     C                   CALL      'XSRDA8'
312600021121     C                   PARM                    WLBDAT
312700021121      *
312800021121     C                   SUB       1             G02TGI
312900021121     C                   Z-ADD     *ZEROS        GI8DAT
313000021121     C                   Z-ADD     *ZEROS        GI8INV
313100021121     C                   Z-ADD     G02TGI        GI8TGI
313200021121     C                   CALL      'XSRGI8'
313300021121     C                   PARM                    WGIDAT
313400021121      *
313500021121     C                   Z-ADD     GI8INV        wdata
313600021121      *
313700021121     C                   EXSR      MEMFAR
313800021121     C* MEMORIZZO LA NUOVA DATA
313900021121     c                   if        wsavdata>0
314000021121     C                   MOVEL     wsavdata      SAVDFV
314100021121     c                   else
314200021121     C                   MOVEL     BARDFV        SAVDFV
314300021121     c                   endif
314400110103     c*
314500021121     C                   Z-ADD     1             I
314600021121     C*
314700021121    3C     PG2(I)        DOWGT     0
314800021121     C     ANMNFV        ANDEQ     0
314900021121     C                   Z-ADD     2             KNPG4
315000021121     C                   Z-ADD     PG2(I)        KNFV4
315100021121     C                   Z-ADD     SAVTFA        KFGS4
315200021121     C                   EXSR      REDFVA
315300021121     C                   ADD       1             I
315400021121    3C                   ENDDO
315500021121    2C                   ENDIF
315600021121    1c                   endif
315700110103     c*
315800100713     c* se il FV non ancora trovato e si dovrebbe trattare di bolla locale
315900100713     c*  tengo i dati del foglio illuiminato
316000100713     c     WMODL         ifeq      ' '
316100100713     c     wgenlnatfa    andeq     wlin
316200100713     C                   MOVEL     wFVALNP       ANMCDU
316300100713     C                   Z-ADD     wFVALNP       SAVFLP
316400100713     C                   Z-ADD     wFVANFV       ANMNFV
316500100713     C                   Z-ADD     wFVADFV       ANMDAO
316600100713     C                   Z-ADD     wFVADFV       SAVDAO
316700100713     C                   CLEAR                   WLIN
316800100713     C                   MOVEL     '1'           WMODL             1
316900100713     c                   endif
317000950927     C*
317100950927     C* SE IL FV ANCORA NON C'E' CERCO SE ANCORA DA ABBINARE
317200021121    1C     WMODL         IFEQ      ' '
317300950927     C                   CLEAR                   KNFV4
317400950927     C                   CLEAR                   KNPG4
317500020214     C                   MOVE      SAVTFA        KFGS4
317600000223     C* Determino BARDFV-4 gg. che è la data minima che deve avere un
317700000223     C* foglio ancora da abbinare. Se la data foglio è più bassa di
317800000223     C* tale data il foglio verrà scartato
317900000223     C                   MOVE      *ZEROS        DATMIN
318000000223     C                   CLEAR                   WLBDAT
318100000223     C                   CLEAR                   WGIDAT
318200000223     C                   Z-ADD     BARDFV        G02INV
318300000223     C                   MOVEL     '3'           G02ERR
318400000223     C                   CALL      'XSRDA8'
318500000223     C                   PARM                    WLBDAT
318600000223     C     G02TGI        SUB       4             GI8TGI
318700000223     C                   CALL      'XSRGI8'
318800000223     C                   PARM                    WGIDAT
318900000223     C                   Z-ADD     GI8INV        DATMIN
319000950927     C                   EXSR      REDFVA
319100950927     C* SE ANCORA NON HO TROVATO NIENTE --> ESCO DAL CICLO
319200950927    2C     WMODL         IFEQ      ' '
319300950927     C                   CLEAR                   WLIN
319400950927    2C                   ENDIF
319500950927    1C                   ENDIF
319600990827     C*
319700990827     C* PULISCO WLIN PER NON CONTINUARE NELLA RICERCA DEL FOGLIO
319800990827     C* SE LA LINEA DI ARRIVO RISULTA TRANSITARE
319900990827     C* DA UN P.O. CHE  E' LO STESSO DAL QUALE IL COLLO ERA PARTITO
320000990827     C* (PER ES.: 53/31 CHE TRANSITA DA 062 E 062 LO FA TRANSITARE DA
320100990827     C* 53 (CASO CHE SI VERIFICA DURANTE IL PASSAGGIO DALLE TRAZIONI
320200990827     C* RIDOTTE ALLE TRAZIONI NORMALI))
320300990827     C     WLINPR        IFGT      *ZEROS
320400990827     C     WLIN          ANDGT     *ZEROS
320500990827     C     WLIN          ANDEQ     WVALNP
320600990827     C                   CLEAR                   WLIN
320700990827     C                   ENDIF
320800021121     C                   ENDIF
320900950927     C*
321000950927     C                   ENDSR
321100950927     C**************************************************************************
321200950927     C* LETTURA DEI DEI FOGLI VIAGGIO PARTENZA
321300950927     C**************************************************************************
321400950927     C     REDFVA        BEGSR
321500950927     C                   CLEAR                   WMODL
321600021121     C                   CLEAR                   Wabbinati         1
321700950927     C     KFVA4         SETLL     FNFVA000
321800950927     C     KFVA4         READE     FNFVA000                               99
321900021121     c* se 99 on - nessun foglio abbinato --> memorizzo
322000021121     c                   if        *in99
322100021121     c                   eval      wabbinati='N'
322200021121     c                   endif
322300950927     C*
322400950927    1C     *IN99         DOWEQ     *OFF
322500950927    2C     FVALNP        IFEQ      WPTFP
322600950927     C* DATA FOGLIO DEVE ESSERE <= ALLA DATA DI ARRIVO
322700960524     C     FVADFV        ANDLE     BARDFV
322800000223     C     KNPG4         ANDGT     *ZEROS
322900000223     C* SE STO CERCANDO FOGLIO ANCORA DA ABBINARE SCARTO SE E' DI DATA
323000000223     C* INFERIORE ALLA DATA MINIMA (DATA ARRIVO COLLO MENO QUATTRO GG.)
323100000223     C     FVALNP        OREQ      WPTFP
323200000223     C     FVADFV        ANDLE     BARDFV
323300000223     C     FVADFV        ANDGE     DATMIN
323400000223     C     KNPG4         ANDEQ     *ZEROS
323500960207     C*
323600950927     C* SE SI TRATTA DI UN FOGLIO DIRETTO TUTTO A POSTO
323700100712     c* ok anche se locale
323800950927    3C     FVAATR        IFEQ      ' '
323900100712     c     fvattr        oreq      'L'
324000950927     C                   MOVEL     FVALNP        ANMCDU
324100020221     C                   Z-ADD     FVALNP        SAVFLP
324200990518     C                   Z-ADD     FVANFV        ANMNFV
324300950927     C                   Z-ADD     FVADFV        ANMDAO
324400980120     C                   Z-ADD     FVADFV        SAVDAO
324500950927     C                   CLEAR                   WLIN
324600950927     C                   MOVEL     '1'           WMODL             1
324700980120     C* SE IL PRIMO FV TROVATO E' UN DIRETTO LO MEMORIZZO PER SAPERE SE
324800980120     C* DOVRO' AGGIOR.LA DATA DI PARTENZA O LA DATA USCITA DAL TRANSITO
324900980120     C     WFLGT         IFEQ      *BLANKS
325000980120     C                   MOVE      'D'           WFLGT
325100980120     C                   END
325200950927     C                   SETON                                        99
325300960207     C*
325400950927   X3C                   ELSE
325500960207     C* SE E' FOGLIO ILLUMINATO
325600950927     C* DEVO VEDERE DA DOVE E' TRANSITO E MANDARE AL TRANSITO LA SEGNA
325700950927     C*  LAZIONE
325800000505     C     KFWA          CHAIN     FNFWA01L                           28
325900000505     C     *IN28         IFEQ      *ON
326000000505     C     FWAATB        ORNE      ' '
326100000505     C                   CLEAR                   FWAFF3
326200000505     C                   CLEAR                   FWAFF4
326300000505     C                   CLEAR                   FWAFL3
326400000505     C                   CLEAR                   FWAFL4
326500000505     C                   ENDIF
326600000505     C**
326700950927     C                   Z-ADD     FVALNA        LNA2
326800950927     C                   Z-ADD     1             X
326900951002     C                   MOVEL     BARLNA        W003A             3
327000950927     C     W003A         LOOKUP    FFV(X)                                 99
327100960207     C* NON E' IL MIO TERMINAL
327200960902     C* ESCLUDO I FOGLI LOCALI
327300021125     c* in relatà elaboro anche i fogli locali, altrimenti come faccio a
327400021125     c*  determinare un collo arrivo no part, del terminal per la lna?
327500021125     c*  ES. 43  x 132
327600960207    4C     *IN99         IFEQ      *ON
327700021125     C***  FVATTR        ANDNE     'L'
327800050603     C                   MOVEL     Usalnatfa     W003A             3
327900960522    5C     FLP(X)        IFNE      W003A
328000960207     C                   MOVEL     FLP(X)        WLIN
328100020214     C                   MOVE      FVADFV        WDTRIF
328200960207     C                   MOVEL     '1'           WMODL             1
328300980120     C* SE IL PRIMO FV TROVATO E' UN TRANSITO LO MEMORIZZO PER SAPERE
328400980120     C* SE DOVRò AGGIOR. DATA DI PARTENZA O DATA USCITA DAL TRANSITO
328500980120     C     WFLGT         IFEQ      *BLANKS
328600980120     C                   MOVE      'T'           WFLGT
328700021121     c* Memorizzo lo stesso i campi che mi servono perchè se poi di tratta
328800021121     c*  di transito "locale" cioè su stesso as (vedi 82 con 113)
328900021121     c*  devo comunque tenere i dati del foglio illuminato non essendoci
329000021121     c*  abbinamento del foglio locale su stessa £1
329100021121     c*  Il problema verrà a morire quando diventeranno dei 1 livello
329200021121     c*  come devono essere
329300021121     c*
329400021121     c                   z-add     FVALNP        WFVALNP
329500021121     c                   z-add     FVANFV        WFVANFV
329600021121     c                   z-add     FVADFV        WFVADFV
329700980120     C                   END
329800960207   X5C                   ELSE
329900960207     C* SE E' FOGLIO ILLUMINATO PER 2 LIVELLO IN ARRIVO TUTTO A POSTO
330000960207     C                   MOVEL     FVALNP        ANMCDU
330100990518     C                   Z-ADD     FVANFV        ANMNFV
330200960207     C                   Z-ADD     FVADFV        ANMDAO
330300980120     C                   Z-ADD     FVADFV        SAVDAO
330400020221     C                   Z-ADD     FVALNP        SAVFLP
330500960207     C                   CLEAR                   WLIN
330600960207     C                   MOVEL     '1'           WMODL             1
330700980120     C* SE IL PRIMO FV TROVATO E' UN DIRETTO LO MEMORIZZO PER SAPERE SE
330800980120     C* DOVRO' AGGIOR.LA DATA DI PARTENZA O LA DATA USCITA DAL TRANSITO
330900980120     C     WFLGT         IFEQ      *BLANKS
331000980120     C                   MOVE      'D'           WFLGT
331100980120     C                   END
331200960207    5C                   ENDIF
331300960207     C*
331400960207   X4C                   ELSE
331500960207     C     KFVA4         READE     FNFVA000                               99
331600960207    4C                   ENDIF
331700950927    3C                   ENDIF
331800950927     C*
331900950927   X2C                   ELSE
332000950927     C     KFVA4         READE     FNFVA000                               99
332100950927    2C                   ENDIF
332200950927    1C                   ENDDO
332300950927     C                   ENDSR
332400980119     C**************************************************************************
332500980119     C* AGGIORNO DATA PART. O DI USCITA DA TRANS. A FRONTE DI ANOM.50
332600980119     C**************************************************************************
332700980119     C     AGGPAR        BEGSR
332800980119     C     KBAR          CHAIN     FNART27L                           3239
332900980119     C   39              GOTO      ENDPAR
333000160722     c
333100160722    1c     *IN32         IFEQ      *OFF
333200160722     c* Spunta di bolla in arrivo ( anche locale)
333300160722    2C                   if        WTIBOL='A'  or WTIBOL='L'
333400160722
333500980120    3C                   SELECT
333600980120     C     WFLGT         WHENEQ    'D'
333700980120     C                   Z-ADD     SAVDAO        ARTDFV
333800041201     C                   Z-ADD     anmnfv        ARTnFV
333900980120     C                   Z-ADD     88            ARTNPP
334000041202     C                   movel     'S'           ARTABN
334100020218     C     WFLGT         WHENEQ    'L'
334200020218     C                   Z-ADD     SAVDAO        ARTDFV
334300041201     C                   Z-ADD     anmnfv        ARTnFV
334400020218     C                   Z-ADD     90            ARTNPP
334500041202     C                   movel     'S'           ARTABN
334600980120     C     WFLGT         WHENEQ    'T'
334700980120     C                   Z-ADD     SAVDAO        ARTDUT
334800980120     C                   Z-ADD     89            ARTPUT
334900020221     C                   Z-ADD     SAVFLP        ARTFLP
335000041202     C                   movel     'S'           ARTABN
335100980120    3C                   ENDSL
335200160722    2c                   endif
335300160722     c*
335400160722     c* spunta al transito: devo aggiornare anche  in FNART
335500160722     c*  la data di partenza se non c'è
335600160722    2c                   if        WTIBOL='T'
335700160722     c
335800160722    3c                   if        artdfv=0
335900160722     C                   Z-ADD     SAVDAO        ARTDFV
336000160722     C                   Z-ADD     anmnfv        ARTnFV
336100160722     C                   Z-ADD     88            ARTNPP
336200160722    3c                   endif
336300160722    2c                   endif
336400160720
336500980120     C                   UPDATE    FNART000
336600980120     C                   MOVEL     'A'           WTBO              1
336700980120     C                   MOVEL     ARTSPE        WDSPE
336800980120     C                   EXSR      WR1AGB
336900160722    1C                   endif
337000160720
337100160722     C                   if        WTIBOL='T'
337200021203     C     KBAR37        CHAIN     FNBTT37L                           3739
337300980119     C   39              GOTO      ENDPAR
337400161108    2C     *IN37         IFEQ      *OFF
337500980120     C                   Z-ADD     SAVDAO        BTTDFV
337600980120     C                   Z-ADD     88            BTTNPP
337700980120     C                   UPDATE    FNBTT000
337800980120     C                   MOVEL     'T'           WTBO              1
337900980120     C                   MOVEL     BTTSPE        WDSPE
338000980120     C                   EXSR      WR1AGB
338100980120    2C                   END
338200160722    1C                   endif
338300160720
338400160720     C*  AGGIORNO ANCHE LA BOLLA PARTENZA
338500020218    1C     WTIBOL        IFEQ      'L'
338600160720     C     WTIBOL        oreq      'A'
338700160721     C     WTIBOL        oreq      'T'
338800020218     C     KBAR          CHAIN     FNBLT27L                           3239
338900020218     C   39              GOTO      ENDPAR
339000020218    2C     *IN32         IFEQ      *OFF
339100160721
339200160721     c                   if        wtibol<>'T'
339300160721     c                   z-add     anmdch        bltdam
339400160721     c                   else
339500160721     c                   z-add     anmdch        bltdet
339600160721     c                   z-add     bpes          bltflp
339700160722     c                   clear                   bltdut
339800160721     c                   endif
339900160720
340000160721    3C                   IF        WFLGT<>'T' AND
340100160721     c                             (bltdfv=0 or bltdfv>savdao) or
340200160721     c                             wflgt= 'T' and bltdfv=0
340300160721     c
340400160720     C                   Z-ADD     SAVDAO        bltDFV
340500160726     c                   eval      welasta_p='S'
340600160720
340700160721    4c                   select
340800160720     C     BLTDSE        WHENEQ    0
340900160720     c                   if        wflgt='D'
341000160720     C                   MOVEL     88            BLTNPP
341100160721     c                   else
341200160720     C                   Z-ADD     90            bltNPP
341300160720     c                   endif
341400160720     C     BLTDSE        WHENGT    0
341500160720     C     BLTDSE        ANDLT     BLTDFV
341600160720     C                   MOVEL     96            BLTNPP
341700160720     C     BLTDSE        WHENGT    0
341800160720     C     BLTDSE        ANDGE     BLTDFV
341900160720     C                   MOVEL     98            BLTNPP
342000160721    4C                   ENDSL
342100160720
342200160721    3c                   endif
342300160720
342400160722    3C                   if        wflgt='T'  and WTIBOL<>'T' and
342500160722     c                             (bltdut=0 or bltdut>savdao)
342600160720     C                   Z-ADD     SAVDAO        bltDUT
342700160722     C                   Z-ADD     bpes          bltFLP
342800160721    3c                   endif
342900160720     c
343000020218     C                   UPDATE    FNBLT000
343100020218     C                   MOVEL     'P'           WTBO              1
343200020218     C                   MOVEL     BLTSPE        WDSPE
343300020218     C                   EXSR      WR1AGB
343400160721     c
343500160721     c* se c'è transito, aggiorno anche BTT
343600160721    3C     WFLGT         ifeq      'T'
343700160721     C     KBAR_t        CHAIN     FNBTT37L                           3239
343800160721    4C     *IN32         IFEQ      *OFF
343900160721     C     *IN39         andeq     *OFF
344000160721
344100160726     c                   eval      welasta_p='S'
344200160721     C                   Z-ADD     SAVDAO        bttDUT
344300160721     c                   z-add     anmdch        bttdam
344400160721     C                   SELECT
344500160721     C     BTTDET        WHENEQ    0
344600160721     C                   MOVEL     89            BTTPUT
344700160721     C     BTTDET        WHENGT    0
344800160721     C     BTTPET        ANDEQ     89
344900160721     C                   MOVEL     89            BTTPUT
345000160721     C     BTTDET        WHENGT    0
345100160721     C     BTTDET        ANDLT     BTTDUT
345200160721     C                   MOVEL     96            BTTPUT
345300160721     C     BTTDET        WHENGT    0
345400160721     C     BTTDET        ANDGE     BTTDUT
345500160721     C                   MOVEL     98            BTTPUT
345600160721     C                   ENDSL
345700160721     C*
345800160721     C     BTTDET        IFEQ      0
345900160721     C                   Z-ADD     ANMDAO        BTTDET
346000160721     C                   MOVEL     89            BTTPET
346100160721     C                   ENDIF
346200160721     C                   UPDATE    FNBTT000
346300160721     C                   MOVEL     'T'           WTBO              1
346400160721     C                   MOVEL     BLTSPE        WDSPE
346500160721     C                   EXSR      WR1AGB
346600160721    4C                   ENDif
346700160721    3C                   ENDif
346800160720
346900020218    2C                   END
347000020218    1C                   END
347100160721
347200980119     C     ENDPAR        TAG
347300160721
347400980119     C   39              SETON                                        40
347500160726     c
347600160726     c                   clear                   welasta_p
347700160726     c
347800980119     C                   ENDSR
347900160727     C**************************************************************************
348000160727     C* AGGIORNO DATA entrata al transito per DIsguido
348100160727     C**************************************************************************
348200160727     C     AGGPAR_d      BEGSR
348300160727     C     KBAR          CHAIN     FNBLT27L                           3239
348400160727    2C     *IN32         IFEQ      *OFF
348500160727    2C     *IN39         andeq     *OFF
348600160727     C                   Z-ADD     SAVDAO        bltDET
348700160727     c                   clear                   bltdut
348800160727     c                   movel     barfgs        bltflp
348900160727     c                   update    fnblt000
349000160727
349100160727     C                   MOVEL     'P'           WTBO              1
349200160727     C                   MOVEL     BLTSPE        WDSPE
349300160727     C                   EXSR      WR1AGB
349400160727     c                   endif
349500160727     c
349600160727     C                   ENDSR
349700950927     C**************************************************************************
349800950927     C* MEMORIZZO I FOGLI ARRIVI CETEGORIA 2 E 6 DEL GIORNO
349900950927     C**************************************************************************
350000950927     C     MEMFAR        BEGSR
350100021121     c                   clear                   wsavdata          8 0
350200021113     c
350300021113     c* se è collo di bolla transito
350400021113     c*  devo usare simfel, il suo p.o. di transito
350500021113     c                   if        wtibol<>'T'
350600050603     C                   MOVEL     usalnatfa     KTFA2
350700021113     c                   else
350800021113     C                   z-add     simfel        KTFA2
350900021113     c                   endif
351000021113     c
351100950927     C* RIMEMORIZZO I FV SOLO SE E' CAMBIATA LA  DATA
351200021121    1C     wdata         IFNE      SAVDFV
351300970113     C     KTFA2         ORNE      SAVTFA
351400021107     c
351500021121     c* Cerco fogli cat 2 e 6  se non trovati in data BARDFV, li cerco
351600021107     c*  in data bardfv - 1
351700021113     c* faccio max 8 tentativi (1 settimana  tenendo conto che ci possono
351800021113     c*  essere dei festivi)
351900950927     C                   CLEAR                   PG2
352000021113     c                   z-add     0             wgiorni           2 0
352100021113     c
352200021113    2c                   dow       wdata>0  and wgiorni<8
352300021113     c                   add       1             wgiorni
352400950927     C*
352500950927     C                   Z-ADD     2             WNPG
352600950927     C     KFVV          SETLL     FNFVV002
352700950927     C     KFVV          READE     FNFVV002                               33
352800950927     C                   Z-ADD     1             I
352900950927     C*
353000950927     C     *IN33         DOWEQ     *OFF
353100950927     C                   Z-ADD     FV2NFV        PG2(I)
353200950927     C                   ADD       1             I
353300950927     C     KFVV          READE     FNFVV002                               33
353400950927     C                   ENDDO
353500021107     c*
353600021107     c                   xfoot     pg2           wpg2             13 0
353700110103     c                   if        wpg2>0
353800021121     c                   eval      wsavdata=wdata
353900021107     c                   clear                   wdata
354000021107     c                   else
354100021107     c* Data - 1
354200021107     C                   MOVEL     wdata         G02INV
354300021107     C                   Z-ADD     *ZEROS        G02DAT
354400021107     C                   Z-ADD     *ZEROS        G02TGI
354500021107     C                   MOVEL     '3'           G02ERR
354600021107     C                   CALL      'XSRDA8'
354700021107     C                   PARM                    WLBDAT
354800021107      *
354900021107     C                   SUB       1             G02TGI
355000021107     C                   Z-ADD     *ZEROS        GI8DAT
355100021107     C                   Z-ADD     *ZEROS        GI8INV
355200021107     C                   Z-ADD     G02TGI        GI8TGI
355300021107     C                   CALL      'XSRGI8'
355400021107     C                   PARM                    WGIDAT
355500021107      *
355600021107     C                   Z-ADD     GI8INV        wdata
355700021107     c                   endif
355800021107     c                   enddo
355900021121     c*
356000970113     C                   MOVEL     KTFA2         SAVTFA
356100021107    1C                   ENDIF
356200950927     C                   ENDSR
356300950927     C**************************************************************************
356400950927     C* RIEMPO PARTE DEI CAMPI DELLA ANOMALIA
356500950927     C**************************************************************************
356600950927     C     RIEANM        BEGSR
356700950927     C                   MOVEL     BARLNP        ANMFLS
356800950927     C                   MOVEL     BARLNA        ANMLNA
356900950927     C                   MOVEL     BARNRS        ANMNRS
357000950927     C                   MOVEL     BARNSC        ANMSCN
357100950927     C                   MOVEL     BARNPS        ANMNPS
357200950927     C                   Z-ADD     COMCAA        ANMCAA
357300040621     c                   z-add     999           anmfl4
357400040621     c* imposto in anmflo data e ora creazione spunta
357500050707     C                   TIME                    TIMEanm          14 0
357600050707     c                   movel     timeanm       wora4             4 0
357700050707     C                   move      timeanm       G02DAT
357800050707     C                   Z-ADD     *ZERO         G02INV
357900050707     C                   MOVEL     *BLANKS       G02ERR
358000050707     C                   CALL      'XSRDA8'
358100050707     C                   PARM                    WLBDAT
358200050707     C                   move      G02INV        WDAT6             6 0
358300040621     c                   movel     wdat6         anmflo
358400040621     c                   move      wora4         anmflo
358500950927     C* FASE APERTURA
358600950927     C     BARKEY        IFNE      SAVFOG
358700950927     C                   EXSR      RICFAS
358800950927     C                   END
358900950927     C                   MOVEL     COMFAS        ANMFSA
359000950927     C* CODICE ANOMALIA
359100950927     C     COMCAA        IFNE      SAVCAA
359200950927     C                   MOVEL     '7C'          COD
359300950927     C                   MOVEL(P)  COMCAA        KEY
359400950927     C     KTAB          CHAIN     TABEL                              34
359500950927     C   34              CLEAR                   DS7C
359600950927     C  N34              MOVEL     TBLUNI        DS7C
359700950927     C                   MOVEL     COMCAA        SAVCAA            3 0
359800950927     C                   ENDIF
359900950927     C*
360000950927     C                   MOVEL     §7CTAN        ANMTAN
360100950927     C                   MOVEL     §7CAIE        ANMAIE
360200950927     C                   MOVEL     §7CSAN        ANMSAN
360300950927     C                   MOVEL     §7CLID        ANMLID
360400950927     C* AUTOCHIUSA
360500950927     C     §7CCHA        IFEQ      'S'
360600960524     C                   MOVE      BARDFV        ANMDCH
360700950927     C                   MOVEL     COMFAS        ANMFSC
360800000320     C                   MOVEL     WCCH          ANMCCH
360900950927     C                   END
361000950927     C                   ENDSR
361100960523     C**************************************************************************
361200960523     C* CARICO LA L6 DELLA FILIALE GESTIONE FOGLIO
361300960523     C**************************************************************************
361400960523     C     CARL6         BEGSR
361500960523     C                   CLEAR                   DSUL06
361600960523     C                   MOVE      '£6'          D06COD
361700020213     C                   MOVEL     SAVPES        D06KEY
361800960523     C                   MOVEL     DSUL06        KPJBU
361900960523     C*
362000960523     C                   CALL      'TRUL06R'
362100960523     C                   PARM                    KPJBA
362200960523     C                   MOVEL     KPJBU         DSUL06
362300960523     C                   MOVEA     LIN           L6
362400960523     C                   ENDSR
362500960523     C**************************************************************************
362600960523     C* SE ESISTE GIA' UNA SPUNTA DELLA LNA, NON CREO ANOMALIE 145-146
362700960523     C**************************************************************************
362800960523     C     CTRSPU        BEGSR
362900960523     C                   MOVEL     'S'           WCREA
363000970117     C*
363100970117     C                   MOVEL     'S'           WCHIU             1
363200970117     C*"WCHIU" MI SERVE PER DETERMINARE SE POSSO CHIUDERE UNA ANOMALIA
363300970117     C* 145/146 CHE HA DATA ANOMALIA < DELLA DATA DELLA SPUNTA CHE
363400970117     C*  STO ELABORANDO MA POTREBBE COMUNQUE ESSERCI UNA SPUNTA
363500970117     C*  CHE MI MANTIENE APERTA IN DATA > TALE ANOMALIA
363600970117     C*  1) SE E' PRESENTE UN'ALTRA SPUNTA CON DATA > NON CHIUDO
363700970117     C*  2) SE E' PRESENTE UNA SPUNTA CON DATA = CATEGORIA 3 4 5
363800020204     C*     NON CHIUDO SE LA SPUNTA CHE STO SCARICANDO E' CAT 1 2 6
363900970117     C*  3) SE E' PRESENTE UNA SPUNTA CON DATA = CATEGORIA 3 4 5
364000970117     C*     E E' UNA SPUNTA DELLA LNA SE LA SPUNTA CHE STO SCARICANDO
364100970117     C*     E' CATEGORIA 3 4 5
364200960531     C**
364300020204    1C     WAGARR        IFEQ      'S'
364400960531     C**
364500070131     C     KBRV7         CHAIN     FNBRV07L                           33
364600960531    2C     *IN33         DOWEQ     *OFF
364700960531    3C     BR7KEY        IFNE      SA1KEY
364800960527     C                   MOVEL     BR7KEY        SA1KEY
364900960527     C                   Z-ADD     BR7NPG        WNPG
365000960527     C                   Z-ADD     BR7NFV        WNFV
365100021014     C                   Z-ADD     BR7FGS        WFGS
365200070906     C                   movel     BR7nps        Wnps
365300021014     C                   CLEAR                   RICBAR
365400960523     C                   EXSR      RILFOG
365500960523     C                   Z-ADD     WDFV          BRVDFV
365600140616     C                   movel     WFCF          BRVFCF
365700140616     C                   movel     Wspg          BRVspg
365800960531    3C                   ENDIF
365900040224     c* Terminal di arrivo della spunta letta
366000040224     C                   CLEAR                   DSLV55
366100040224     C                   MOVEL     'A'           D55TPT
366200040224     C                   MOVEL     LNPB          D55LNP
366300040224     C                   MOVEL     brvdfv        D55DRF
366400040224     C                   MOVE      Br7pes        D55LIN
366500040224     C                   EXSR      FNLV55
366600050603     c                   if        d55tfa=Usalnatfa
366700960531     C**
366800961210    3C     BRVDFV        IFGT      BARDFV
366900960523     C                   MOVEL     'N'           WCREA             1
367000970117     C                   MOVEL     'N'           WCHIU             1
367100961209   X3C                   ELSE
367200961209     C*
367300961210    4C     BRVDFV        IFEQ      BARDFV
367400961210     C     BR7NPG        ANDGE     3
367500961210     C     BR7NPG        ANDLE     5
367600970117     C*
367700970117     C     BARNPG        IFEQ      1
367800970117     C     BARNPG        OREQ      2
367900970117     C                   MOVEL     'N'           WCHIU
368000970117     C                   ENDIF
368100961209     C*
368200020213    5C     BR7PES        IFEQ      BARLNA
368300961209     C                   MOVEL     'N'           WCREA             1
368400970117     C*
368500970117     C     BARNPG        IFNE      1
368600970117     C     BARNPG        ANDNE     2
368700970117     C                   MOVEL     'N'           WCHIU
368800970117     C                   ENDIF
368900970117     C*
369000961209   X5C                   ELSE
369100020214    6C     BR7PES        IFNE      SAVPES
369200020214     C                   MOVEL     BR7PES        SAVPES
369300961209     C                   EXSR      CARL6
369400961209    6C                   ENDIF
369500961209     C*
369600961209     C     BARLNA        LOOKUP    L6                                     34
369700961209     C   34              MOVEL     'N'           WCREA             1
369800970117     C*
369900970117    6C   34BARNPG        IFNE      1
370000970117     C     BARNPG        ANDNE     2
370100970117     C                   MOVEL     'N'           WCHIU
370200970117    6C                   ENDIF
370300970117     C*
370400961209    5C                   ENDIF
370500961209    4C                   ENDIF
370600961209    3C                   ENDIF
370700040224    3C                   ENDIF
370800961209     C*
370900961209    3C     WCREA         IFEQ      'N'
371000961209     C                   SETON                                        33
371100961210     C                   ELSE
371200070131     C     KBRV7         READE     FNBRV07L                               33
371300961209    3C                   ENDIF
371400040224     C**
371500960531    2C                   ENDDO
371600960531     C*
371700960531    1C                   ENDIF
371800960523     C*
371900960523     C                   ENDSR
372000961202     C**************************************************************************
372100961202     C* CREO ANOMALIA 145 O 146 E CHIUDO L'ALTRA
372200961202     C**************************************************************************
372300961202     C     ANM14X        BEGSR
372400961202     C* LA CREO SOLO SE NON ESISTE UNA SPUNTA DELLA LNA CON DATA=>
372500961210     C     WDCM          IFGT      0
372600961210     C     WDCM          ANDGE     BARDFV
372700961210     C                   MOVEL     'N'           WCREA
372800961210     C                   ELSE
372900961202     C                   EXSR      CTRSPU
373000961210     C                   ENDIF
373100961202     C*
373200961202    5C     WCREA         IFEQ      'S'
373300020213     C     BARFLE        ANDEQ     BTFP
373400020213     C     BARFLE        ORNE      BTFP
373500961202     C                   Z-ADD     CAACRE        COMCAA
373600961202     C                   EXSR      CRTANM
373700961202     C* SOLO PER EXPORT
373800961202     C*
373900961202     C     CAACRE        IFEQ      146
374000961202    6C     WTIBOL        ANDNE     'A'
374100961202     C                   MOVEL     'S'           WTIBOL
374200961202    6C                   ENDIF
374300961202    5C                   ENDIF
374400961202     C*
374500961202     C* SE SPUNTA DA ALTRO AS MA NON DOVEVO CREARE IL COLLO DA INOLTRAR
374600961202     C*  LA CHIUDO SUBITO. ANCHE SE IL COLLO E' CONSEGNATO SENZA SPUNTE
374700020213    5C     BARFLE        IFNE      BTFP
374800961202    6C     WCREA         IFNE      'S'
374900961202     C                   Z-ADD     CAACRE        COMCAA
375000961211     C* IN QUESTO CASO NON DEVO CREARE L'ANOMALIA 620 IN QUANTO
375100961211     C*  LA SPEDIZIONE E' GIA' STATA CONSEGNA A COMUNQUE E' PRESENTE
375200961211     C*  UNA SPUNTA CON DATA >.
375300961211     C                   MOVEL     'N'           W620
375400961202     C                   EXSR      CHIUAN
375500961211     C                   MOVEL     'S'           W620
375600961211     C*
375700961209   X6C                   ELSE
375800961210     C                   EXSR      ANM600
375900961202    6C                   ENDIF
376000961209     C*
376100961202   X5C                   ELSE
376200961210    6C     WCREA         IFEQ      'S'
376300961209     C* PER ESTERO CREO ANOMALIA 645 E 600
376400961209     C                   EXSR      ANM600
376500961210    6C                   ENDIF
376600961210    5C                   ENDIF
376700961202     C*
376800961210     C                   MOVEL     'N'           W620              1
376900961202     C                   Z-ADD     CAACIU        COMCAA
377000961202     C                   EXSR      CHIUAN
377100961210     C                   MOVEL     'S'           W620              1
377200961202     C                   ENDSR
377300961213     C**************************************************************************
377400961213     C* LETTURA FOGLIO DELLA SPUNTA CHE STO CARICANDO E IMPOSTAZIONE
377500961213     C*  CAMPI DI COMOD
377600961213     C**************************************************************************
377700961213     C     BARFGV        BEGSR
377800961213     C*
377900961213    3C     BARKEY        IFNE      SAVKEY
378000961213     C                   MOVEL     BARKEY        SAVKEY
378100961213     C                   Z-ADD     BARNPG        WNPG
378200961213     C                   Z-ADD     BARNFV        WNFV
378300021014     C                   Z-ADD     BARFGS        WFGS
378400070906     C                   movel     BARnps        Wnps
378500021014     C                   MOVEL     'S'           RICBAR            1
378600961213     C                   EXSR      RILFOG
378700980313     C                   MOVEA     FFG           FFS
378800961213     C                   Z-ADD     WDFV          BARDFV
378900961213     C                   MOVEL     WSPG          BARSPG
379000021025     C                   MOVEL     WFGS          wwwFGS
379100021023     C                   Z-ADD     WFLE          BARFLE
379200140616     C                   movel     WFCF          BARFCF
379300970521     C*
379400970521     C     WTTR          IFNE      *BLANKS
379500970520     C     WTTR          LOOKUP    TTR                                    34
379600970520     C                   MOVE      *IN34         WDEFL             1
379700970521     C                   ELSE
379800970521     C                   MOVE      '0'           WDEFL             1
379900970521     C                   ENDIF
380000970521     C*
380100961213    3C                   ENDIF
380200021025     C* IL P.O. GESTIONE LO DEVO SEMPRE IMPOSTARE PERCHÈ PUÒ CAMBIARE
380300021031     C                   MOVEL     WWWFGS        BARFGS
380400070209     c
380500070209     c* Se si tratta di spunta partenza, il BPES deve essere il P.O foglio
380600070209     c*  per problemi con eventuale creazione di anomalie 200
380700070209     c                   eval      wpes=bpes
380800070209     c                   if        barnpg=1
380900070209     c                   eval      bpes=barfgs
381000070209     c                   endif
381100961213     C                   ENDSR
381200970203     C**************************************************************************
381300970203     C* ANOMALIE CHE DEVO SEMPRE CREARE O SEMPRE CHIUDERE SE COLLO
381400970203     C*  SPARATO NELL'AREA DI ARRIVO (COME TERMINAL DI ARRIVO)
381500970203     C**************************************************************************
381600970203     C     ANMSEM        BEGSR
381700970203     C*
381800970203     C* COLLO/BOLLA ARRIVI PRESENTE
381900970204     C* SE CONSEGNATO IL COLLO CREO SEMPRE ANOMALIA --> COLLO CONSEGNAT
382000970204    1C  N05WBOAR         IFEQ      'S'
382100990120    2C     BARDFV        IFGT      WATDCM
382200970204     C     WATDCM        ANDGT     0
382300170328     c* oppure senza consegna ma con cons anomala "S"
382400170328     c     watdcm        oreq      0
382500170328     c     wabcca        andeq     'S'
382600170505     c
382700170505     c*
382800170505     c* se bolla con cons anomala legata, la bolla figlia
382900170505     c*  DEVE essere borderizzata per dare anomalia 138
383000170505     c*  per semplificare leggo della bolla figlia solo il 1° dettaglio colli
383100170505     c                   eval      w138='S'
383200170505    3c                   if        wabcca<>' '  and comnsp>0
383300170505     c     kbolla        chain     fnlbl02l
383400170505    4c                   if        %found(fnlbl02l)
383500170505     c     klbl          chain(n)  fnblp01l
383600170505    5c                   if        %found(fnblp01l) and
383700170505     c                              blpft2=' '
383800170505     c                   eval      w138='N'
383900170505    5c                   endif
384000170505    4c                   endif
384100170505    3c                   endif
384200170505     c
384300170505     c                   if        w138='S'
384400970203     C                   Z-ADD     138           COMCAA
384500970203     C                   EXSR      CRTANM
384600170505     c                   endif
384700170505     c
384800990120    2C                   ENDIF
384900170505     c
385000000705     C* SE COLLO CAT.8 SEGNALA SE LA RELATIVA DISTINTA 4 E'
385100000705     C*  GIA' CHIUSA CON ANOMALIA 139
385200040430     c                   if        watdcm>0  and barnpg=8
385300000705     C* SE NON E' CHIUSA VEDO SE IN FASE DI CHIUSURA: CHIAMO FNLV53R
385400000705     C                   CLEAR                   DSLV53
385500000705     C                   MOVEL     'V'           D53TFO
385600020213     C                   MOVEL     BTFP          D53FEL
385700000705     C                   Z-ADD     BARNFV        D53NFV
385800000705     C                   MOVEL     4             D53NPG
385900000705     C                   MOVEL     BARFGS        D53FGS
386000000705     C                   MOVEL     BARFLE        D53FLF
386100000705     C                   CALL      'FNLV53R'
386200000705     C                   PARM                    DSLV53
386300000705     C*
386400000705     C* FOGLIO CHIUSO (S) O IN FASE DI CHIUSURA (F)
386500000705    3C     D53FCF        IFEQ      'S'
386600000705     C     D53FCF        OREQ      'F'
386700040430     c* non creo se la relativa consegna anomala non lo prevede
386800040430    4c                   if        WABCCA<>*blanks and wabcca<>savcca
386900040430     C                   MOVEL     '7O'          COD
387000040430     C                   MOVEL(P)  wabcca        KEY
387100040430     C     KTAB          CHAIN     TABEL                              41
387200040430     C  N41              MOVEL     TBLUNI        DS7o
387300040430     c   41              clear                   §7o139
387400040430     c                   eval      savcca=wabcca
387500040430    4c                   endif
387600040430     c
387700040430    4c                   if        §7o139=*blanks or wabcca=*blanks
387800000705     C                   Z-ADD     139           COMCAA
387900000705     C                   EXSR      CRTANM
388000040430    4C                   ENDIF
388100040430    3C                   ENDIF
388200000705    2C                   ENDIF
388300000705     C*
388400990120     C* COLLO NON CONSEGNATO
388500990121    2C     WATDCM        IFEQ      0
388600170328     c     wabcca        andne     'S'
388700990120     C* VERIFICO SE DEVO CREARE ANOMALIA SU DANNI
388800000523     C*  NON LO CREO PER LA CATEGORIA 4 E 8
388900990121     C                   MOVEL     'N'           WCAT4             1
389000990126     C* NON GESTISCO IL RITROVAMENTO COLLO
389100990126     C                   MOVEL     'N'           WRITRO
389200990126     C*     GESTISCO LA CREAZIONE ANOMALIE
389300990126     C                   MOVEL     'S'           WCREAN
389400990120     C                   EXSR      CTRDAN
389500990120     C** SE NON TROVATA CA DI COLLO
389600990120     C* VERIFICO SE LA SPEDIZIONE E' SU UNA CA CHIUSA --> CREO
389700990120    3C     WTRDAN        IFEQ      ' '
389800040811      *
389900040811      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
390000040811      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
390100040811      * legate alla spedizione che passo
390200040811     c                   clear                   fidn12ds
390300170505     c***                eval      i12aas = ARTAAS
390400170505     c***                eval      i12lnp = ARTLNP
390500170505     c***                eval      i12nrs = ARTNRS
390600170505     c***                eval      i12nsp = ARTNSP
390700170505     c                   eval      i12aas = comAAS
390800170505     c                   eval      i12lnp = comLNP
390900170505     c                   eval      i12nrs = barNRS
391000170505     c                   eval      i12nsp = comNSP
391100170505     c                   eval      i12fel = 'E'
391200040811     c*
391300040811     c                   call      'FIDN12R'
391400040811     c                   parm                    fidn12ds
391500040811     c*
391600040811     c                   setoff                                       33
391700040811     c*
391800040811     c* se non ci sono errori
391900040811     c                   if        o12err <> ' '
392000040811     c                   seton                                        33
392100040811     c                   else
392200040811     c* se numero di CA trovate maggiore di zero
392300040811     c                   if        o12nca = 0
392400040811     c                   seton                                        33
392500040811     c                   else
392600040811     c                   z-add     1             jj                2 0
392700040811     c                   dow       jj <= o12nca and *in33 = *off
392800040811     C     skDch(jj)     IFGT      0
392900040811     C     BARNPG        IFNE      4
393000040811     C     BARNPG        ANDNE     8
393100040811     C*
393200040811     C* Valorizzo i campi DCT* utilizzati nella routine CRE151 con quelli
393300040811     C* ritornati dal *pgm d utilità FIDN12R
393400040811     C                   movel     skCch(jj)     DCTCCH
393500040811     C*
393600040811     C                   EXSR      CRE151
393700040811     C                   ELSE
393800040811     C* SE E' CAT 4 E 8 LA CHIUDO
393900040811     C                   Z-ADD     151           COMCAA
394000040811     C                   EXSR      CHIUAN
394100040811     C                   ENDIF
394200040811     C**
394300040811     C                   SETON                                        33
394400040811     C**
394500040811     C                   ENDIF
394600040811     c*
394700040811     c                   add       1             jj
394800040811     c                   enddo
394900040811     c*
395000040811     c                   seton                                        33
395100040811     c*
395200040811     c                   endif
395300040811     c                   endif
395400040811     C**
395500990120    3C                   ENDIF
395600990120     C**
395700990120    2C                   ENDIF
395800990120    1C                   ENDIF
395900021023     C*
396000970203     C* CHIUDO LE ANOMALIE DI COLLO MANCATE
396100020204    1C     WAGARR        IFEQ      'S'
396200970203     C* PER COLLO NON LOCALE
396300021023     C     WAGPAR        OREQ      ' '
396400970203     C* PER BOLLA NON TROVATA IN PARTENZA
396500970203     C     WBOPAR        OREQ      'N'
396600021023     C**
396700021023     C* NON AGGIORNO CON SPUNTA ENTRATA
396800021023     C* SE NON E' LOCALE O DISGUIDO IN AREA AGGIORNO SEMPRE
396900021023    2C     WSPLOC        IFEQ      ' '
397000021023     C     WSPLOC        OREQ      'D'
397100021023     C* SE E' LOCALE SOLO SE E' SPUNTA FATTA CON CAT.ARRIVI  DAL P.O.
397200021023     C*  LINEA ARRIVO O TERMINAL ARRIVO
397300021023     C     WSPLOC        OREQ      'A'
397400021023     C     BARNPG        ANDNE     5
397500021023     C     BARNPG        ANDNE     1
397600021023     C*
397700021023     C     WSPLOC        OREQ      'A'
397800021023     C     BARNPG        ANDEQ     1
397900021023     C     WDEFL         ANDEQ     '1'
398000021023     C*
398100021023     C     WSPLOC        OREQ      'T'
398200021023     C     BARNPG        ANDNE     5
398300021023     C     BARNPG        ANDNE     1
398400021023     C*
398500021023     C     WSPLOC        OREQ      'T'
398600021023     C     BARNPG        ANDEQ     1
398700021023     C     WDEFL         ANDEQ     '1'
398800021023     C*
398900970203     C*
399000970203     C* COLLO ARRIVATO MANCANTE --> SOLO SE SPARATO SUL SISTEMA
399100041018     c* L'anomalia 15 non la chiudo se altra spunta presente in cat.arrivi
399200041018     c*  con stessa data
399300041018     c                   eval      wcontrdfv='S'
399400041018     c                   eval      wstess='S'
399500041018     c                   eval      wchiu='S'
399600970203     C                   Z-ADD     15            COMCAA
399700970203     C                   MOVEL     'TR'          SAVCCH
399800970203     C                   EXSR      CHIUAN
399900970203     C*
400000970203     C* COLLO PARTITO NON ARRIVATO
400100970203     C                   MOVEL     'AR'          SAVCCH
400200970203     C                   Z-ADD     5             COMCAA
400300970203     C                   EXSR      CHIUAN
400400970203     C* MANCA COLLO MANCA BOLLA
400500970203     C                   MOVEL     'AR'          SAVCCH
400600970203     C                   Z-ADD     12            COMCAA
400700970203     C                   EXSR      CHIUAN
400800990521    2C                   ENDIF
400900990521    1C                   ENDIF
401000040430     c
401100040430     c* Chiudo fuori posto bolla partenza e fuori posto bolla transito
401200040430     C                   Z-ADD     120           COMCAA
401300040430     C                   EXSR      CHIUAN
401400040430     C                   Z-ADD     125           COMCAA
401500040430     C                   EXSR      CHIUAN
401600970203     C                   ENDSR
401700970204     C**************************************************************************
401800970204     C* ANOMALIA FUORI POSTO SEGNACOLLO BOLLA IN PARTENZA
401900970204     C**************************************************************************
402000970204     C     ANM120        BEGSR
402100970204     C                   Z-ADD     120           COMCAA
402200970204     C*
402300970204     C* VALGONO SOLTANTO LE SPUNTE SPARATE NELL'AREA DI PARTENZA
402400020213    1C  N05BARFLE        IFEQ      BTFP
402500970204     C* SPUNTA DEL TERMINAL DI PARTENZA
402600020213    2C     BARFGS        IFEQ      BTFP
402700970204     C* SE COLLO NON SPUNTATO NELLE CATEGORIE PARTENZA
402800970204    3C     BARNPG        IFNE      5
402900970204     C     BARNPG        ANDNE     1
403000970204     C     BARSPG        ANDNE     'P'
403100970204     C                   EXSR      CRTANM
403200970204   X3C                   ELSE
403300970204     C* SE COLLO     SPUNTATO NELLE CATEGORIE PARTENZA
403400970204     C                   EXSR      CHIUAN
403500970204    3C                   ENDIF
403600970204     C*
403700970204   X2C                   ELSE
403800970204     C* COLLO SPUNTATO DA UN 2 LIVELLO IN PARTENZA
403900970204     C* SE COLLO NON SPUNTATO NELLE CATEGORIE PARTENZA
404000970204    3C     BARNPG        IFNE      5
404100970204     C     BARSPG        ANDNE     'P'
404200970204     C                   EXSR      CRTANM
404300970204     C                   ELSE
404400970204     C* SE COLLO     SPUNTATO NELLE CATEGORIE PARTENZA
404500970204     C                   EXSR      CHIUAN
404600970204    3C                   ENDIF
404700970204    2C                   ENDIF
404800970204    1C                   ENDIF
404900970204     C                   ENDSR
405000970204     C**************************************************************************
405100970204     C* ANOMALIE CHE CREO SOLO SE HA SPARATO LA LINEA DI ARRIVO DEL COL
405200970204     C**************************************************************************
405300970204     C     ANMARR        BEGSR
405400970204     C*
405500970204     C* COLLO/BOLLA ARRIVI PRESENTE
405600970204    1C  N05WBOAR         IFEQ      'S'
405700990126     C                   MOVEL     'N'           WCAT4             1
405800990126     C*     GESTISCO IL RITROVAMENTO COLLO
405900990126     C                   MOVEL     'S'           WRITRO
406000020204     C* NON GESTISCO LA CREAZIONE ANOMALIE CHE GIA' FACCIO IN CHIUAN
406100990126     C                   MOVEL     'N'           WCREAN
406200990126     C                   EXSR      CTRDAN
406300970204     C*
406400970204    2C     WATDCM        IFEQ      0
406500170328     c* in caso di cons anomala "S" il collo potrebbe non avere la data di cons
406600170328     c     wabcca        andne     'S'
406700170328     c
406800970204     C     BARDFV        ORGT      WATDCM
406900170329     c     watdcm        andgt     0
407000970204     C*
407100970204    3C     WABNDC        IFNE      0
407200970204     C     WATDCM        ANDEQ     0
407300170328     c*
407400970204     C* BOLLA IN CONSEGNA
407500970204    4C     BARNPG        IFNE      4
407600000523    4C     BARNPG        ANDNE     8
407700970204     C* SE NO SPUNTA ARRIVI CREO ANOMALIA: BOLLA IN CONSEGNA FUORI POST
407800970204    5C     BARNPG        IFNE      2
407900970204     C                   Z-ADD     135           COMCAA
408000970204     C                   EXSR      CRTANM
408100970204    5C                   END
408200970204     C*
408300000607    4C                   ENDIF
408400000607     C**
408500000607    4C     BARNPG        IFEQ      4
408600970204     C*
408700970204     C*        ANOMALIA: "IN CONSEGNA SU ALTRA DISTINTA"
408800970204    5C     WABNDC        IFNE      BARNFV
408900970204     C                   Z-ADD     150           COMCAA
409000970204     C                   EXSR      CRTANM
409100000523     C*
409200970204   X5C                   ELSE
409300970204     C*
409400970204     C* CHIUDO ANOMALIA: "IN CONSEGNA SU ALTRA DISTINTA"
409500970204     C                   Z-ADD     150           COMCAA
409600970204     C                   MOVEL     '  '          SAVCCH            2
409700970204     C                   EXSR      CHIUAN
409800970204    5C                   ENDIF
409900970204    4C                   ENDIF
410000970204    3C                   ENDIF
410100970204     C*
410200970204     C* BOLLA ARRIVI FUORI POSTO
410300161201     c* In caso di wgiac = 'D' non devo dare il fuori posto
410400161201    3C     WGIAC         IFEQ      ' '
410500970204     C*
410600161201    4C     BARSPG        IFEQ      'G'
410700161201    4C     BARSPG        oreq      'Z'
410800970204     C                   Z-ADD     130           COMCAA
410900970204     C                   EXSR      CRTANM
411000970204    4C                   ENDIF
411100161021     c
411200970204     C* IN ENTRATA SOLO SE NON E' BOLLA LOCALE
411300970204    4C     BARNPG        IFEQ      5
411400970204     C     WTIBOL        ANDNE     'L'
411500970204     C     WABNDC        ANDEQ     0
411600970204     C                   Z-ADD     130           COMCAA
411700970204     C                   EXSR      CRTANM
411800970204    4C                   ENDIF
411900970204    3C                   ENDIF
412000970204     C*
412100970204     C* BOLLA GIACENTE FUORI POSTO
412200161201     c* per ora il fuori posto lo devo dare anche per le
412300161201     c*  giacenze a partare a mag giac. mentre con i 2gg in IMA
412400161201     c*  dovrò dare l'anomalia soltanto se già a IMG se spuntato in IMA
412500161201     c*  per le altre categorie errore sempre
412600970204    3C     BARSPG        IFNE      'G'
412700161021    3C     BARSPG        andne     'Z'
412800161201     C**** WGIAC         ANDEQ     'S'
412900161201     C     WGIAC         ANDne     ' '
413000970205     C     BARDFV        ANDGT     GCPDGC
413100170125     c* se WGIAC = "D" e filiale arrivo partita con procedura NEWIMA
413200170125     c*  non creao l'anomalia
413300170125    4c                   if        wgiac='D'  and allnewima<>'S'
413400170125     c                   movel     barlna        w003a
413500170125     c                   if         %lookup(w003a:skFilOk)>0
413600170125     c                   eval      allnewima='Y'
413700170125     c                   else
413800170125     c                   clear                   allnewima
413900170125     c                   endif
414000170125    4c                   endif
414100170125     c
414200170125    4c                   if        wgiac='S' or (barspg<>'A' and
414300170125     c                             barnpg<>2) or
414400170125     c                             (wgiac='D' and allnewima=' ')
414500970204     C                   Z-ADD     140           COMCAA
414600970204     C                   EXSR      CRTANM
414700170125    4c                   endif
414800170125     c
414900970204    3C                   ENDIF
415000970204     C*
415100970204    2C                   ENDIF
415200970204     C**
415300970204     C* C H I U D O
415400970204     C* BOLLA CONSEGNA
415500970204    2C     BARNPG        IFEQ      4
415600970204     C                   Z-ADD     135           COMCAA
415700970204     C                   EXSR      CHIUAN
415800970204    2C                   END
415900970204     C* BOLLA ARRIVI
416000970204    2C     BARNPG        IFEQ      2
416100970204     C     BARNPG        OREQ      4
416200000523     C     BARNPG        OREQ      8
416300970204     C     BARSPG        OREQ      'A'
416400970204     C                   Z-ADD     130           COMCAA
416500970204     C                   EXSR      CHIUAN
416600970204    2C                   END
416700970204     C* BOLLA GIACENZE
416800970204    2C     BARNPG        IFEQ      3
416900970204     C     BARSPG        ANDEQ     'G'
417000161028    2C     BARNPG        orEQ      3
417100161028     C     BARSPG        ANDEQ     'Z'
417200970204     C                   Z-ADD     140           COMCAA
417300970204     C                   EXSR      CHIUAN
417400970204    2C                   END
417500970204     C*
417600970204    1C                   ENDIF
417700970204     C                   ENDSR
417800970218     C**************************************************************************
417900970218     C* SCRITTURA DI FNAGB PER AGGIORNAMENTO  BOLLE
418000970218     C**************************************************************************
418100970217     C     WRTAGB        BEGSR
418200970217     C* LA PRIMA VOLTA NON SCRIVO
418300041105    1C     WAGGF         IFEQ      'S'
418400970217     C     KSAV          CHAIN     FNAGB01L                           34
418500970217     C   34              CLEAR                   FNAGB000
418600970218     C                   MOVEL     SAVTBO        AGBTBO
418700970217     C                   MOVEL     KAGB          AGBAGB
418800970217     C                   MOVEL     SAVAAS        AGBAAS
418900970217     C                   MOVEL     SAVLNP        AGBLNP
419000970217     C                   MOVEL     SAVNRS        AGBNRS
419100970217     C                   MOVEL     SAVNSP        AGBNSP
419200970217     C     WFBS          IFEQ      'S'
419300970217     C                   MOVEL     WFBS          AGBFBS
419400970217     C                   ENDIF
419500970217     C     WFVC          IFEQ      'S'
419600970217     C                   MOVEL     WFVC          AGBFVC
419700970217     C                   ENDIF
419800990728     C     WFPC          IFEQ      'S'
419900990728     C                   MOVEL     WFPC          AGBFPC
420000990728     C                   ENDIF
420100970217     C*
420200981124     C   34              WRITE     FNAGB000                             99
420300970217     C  N34              UPDATE    FNAGB000
420400041105     c
420500041105     c* Se previsto scrivo anche per l'altra bolla
420600041105     c                   eval      wprimotbo=savtbo
420700041105     c
420800041105     c                   if        wagbart='S'
420900041105     c                   eval      savtbo='A'
421000041105     c                   EXSR      ALTROAGB
421100041105     c                   endif
421200041105     c                   eval      savtbo=wprimotbo
421300160421     c
421400160421     c* Scrivo record per la zona
421500160421     c                   if        wznc<>*blanks
421600160421     C                   CLEAR                   FNAGB000
421700160421     C                   MOVEL     SAVTBO        AGBTBO
421800160421     C                   MOVEL     wznc          AGBAGB
421900160421     C                   MOVEL     SAVAAS        AGBAAS
422000160421     C                   MOVEL     SAVLNP        AGBLNP
422100160421     C                   MOVEL     SAVNRS        AGBNRS
422200160421     C                   MOVEL     SAVNSP        AGBNSP
422300160421     C                   WRITE     FNAGB000                             99
422400160421     c                   endif
422500041105     c
422600970217    1C                   ENDIF
422700970218     C*
422800970218     C                   CLEAR                   WFBS
422900970218     C                   CLEAR                   WFVC
423000990728     C                   CLEAR                   WFPC
423100160421     C                   CLEAR                   Wznc
423200041105     C                   CLEAR                   Wagbart
423300970217     C                   ENDSR
423400041105     C**************************************************************************
423500041105     c* scrivo fnagb per l'altro tipo bolla
423600041105     C**************************************************************************
423700041105     c     ALTROAGB      begsr
423800041105     C     KSAV          CHAIN     FNAGB01L                           34
423900041105     c                   eval      agbtbo=savtbo
424000041105     c                   if        *in34
424100041105     c                   clear                   agbfbs
424200041105     C                   WRITE     FNAGB000                             99
424300041105     c                   else
424400041105     c* Se record trovato aggiorno solo i campi del peso/volume
424500041105     C     WFVC          IFEQ      'S'
424600041105     C                   MOVEL     WFVC          AGBFVC
424700041105     C                   ENDIF
424800041105     C     WFPC          IFEQ      'S'
424900041105     C                   MOVEL     WFPC          AGBFPC
425000041105     C                   ENDIF
425100041105     c
425200041105     C                   UPDATE    FNAGB000
425300041105     c                   endif
425400041105     c                   endSR
425500980120     C**************************************************************************
425600980120     C* SCRITTURA DI FNAGB PER AGGIORNAMENTO  BOLLE
425700980120     C**************************************************************************
425800980120     C     WR1AGB        BEGSR
425900110103      *
426000030528     c                   setoff                                       45
426100110103      *
426200980120     C     KEAGB         CHAIN     FNAGB01L                           34
426300980120     C   34              CLEAR                   FNAGB000
426400980120     C                   MOVEL     WTBO          AGBTBO
426500980120     C                   MOVEL     KAGB          AGBAGB
426600980120     C                   MOVEL     WDAAS         AGBAAS
426700980120     C                   MOVEL     WDLNP         AGBLNP
426800980120     C                   MOVEL     WDNRS         AGBNRS
426900980120     C                   MOVEL     WDNSP         AGBNSP
427000160726     c*
427100160726     c* Se richiesto, passo flag di rielaborazione statistiche partenze
427200160726     c                   if        welasta_p='S'
427300160726     c                   clear                   dagbflo
427400160726     c                   eval      §agbdsf='S'
427500160726     c                   eval      agbflo=dagbflo
427600160726     c                   endif
427700980120     C*
427800030528     C   34              WRITE     FNAGB000                             45
427900980120     C  N34              UPDATE    FNAGB000
428000980120     C*
428100160726     c                   clear                   welasta_p
428200160726     C*
428300980120     C                   ENDSR
428400970609     C**************************************************************************
428500970609     C* SCRITTURA DI FNSTA PER RIELABORAZIONE STATISTICA ARRIVI
428600970609     C**************************************************************************
428700970609     C     WRTSTA        BEGSR
428800970609     C* LA PRIMA VOLTA NON SCRIVO
428900970609    1C     WARDAM        IFGT      0
429000970609     C* VECCHIA DATA
429100970609     C     WSVDAM        IFGT      0
429200970609     C                   CLEAR                   SA2TLA
429300070221     C     dls45FLG      IFEQ      'C'
429400970609     C                   MOVEL     'L'           SA2TLA
429500970609     C                   END
429600970609     C                   MOVEL     'A'           SA2TRC
429700970609     C                   Z-ADD     WSVDAM        SA2DTS
429800970609     C                   MOVE      *ZEROS        SA2LNA
429900970609     C                   CALL      'FNLSA2R3'
430000970609     C                   PARM                    SA2TLA            1
430100970609     C                   PARM                    SA2TRC            1
430200970609     C                   PARM                    SA2DTS            8 0
430300970609     C                   PARM                    SA2LNA            3 0
430400970609     C                   END
430500970609     C* NUOVA DATA
430600970609     C                   Z-ADD     WARDAM        SA2DTS
430700970624     C                   MOVEL     'A'           SA2TRC
430800970609     C                   CALL      'FNLSA2R3'
430900970609     C                   PARM                    SA2TLA            1
431000970609     C                   PARM                    SA2TRC            1
431100970609     C                   PARM                    SA2DTS            8 0
431200970609     C                   PARM                    SA2LNA            3 0
431300970609    1C                   ENDIF
431400970609     C*
431500970609     C                   ENDSR
431600980928     C**************************************************************************
431700980928     C* MEMORIZZO I FOGLI NON TROVATI DA INVIARE A SEDE CON MSG
431800980928     C**************************************************************************
431900111018     C**   INVMSG        BEGSR
432000980928     C*
432100111018     C**                 Z-ADD     1             EE                3 0
432200111018     C**                 SETON                                        95
432300111018     C**                 CLEAR                   WPRES
432400111018     C**                 MOVE      WNFV          WNFVA             5
432500111018    4C**   *IN95         DOWEQ     *ON
432600111018     C**   WPRES         ANDEQ     ' '
432700980928     C* IL FOGLIO CORRISPONDE: NON LO RIMEMORIZZO
432800111018     C**   WNFVA         LOOKUP    ERF(EE)                                95
432900111018    5C** 95WNPG          IFEQ      ERC(EE)
433000111018     C**   WFGS          ANDEQ     ERE(EE)
433100111018     C**   WNPS          ANDEQ     ERP(EE)
433200111018     C**                 MOVEL     'S'           WPRES             1
433300111018    5C**                 ENDIF
433400111018     C**                 ADD       1             EE
433500111018    4C**                 ENDDO
433600980928     C* SE NON E' PRESENTE RIMEMORIZZO
433700111018    4C**   WPRES         IFEQ      ' '
433800111018     C**                 Z-ADD     1             EE
433900111018     C**   '     '       LOOKUP    ERF(EE)                                95
434000111018     C** 95              MOVE      WNFVA         ERF(EE)
434100111018     C** 95              Z-ADD     WNPG          ERC(EE)
434200111018     C** 95              Z-ADD     WFGS          ERE(EE)
434300111018     C** 95              movel     Wnps          ERP(EE)
434400111018    5C**   EE            IFEQ      8
434500111018     C**                 EXSR      TRASMI
434600111018    5C**                 ENDIF
434700111018    4C**                 ENDIF
434800111018     C*+                 ENDSR
434900990120     C**************************************************************************
435000990120     C* SE DEVO CREO ANOMALIA 152 / 153
435100990120     C**************************************************************************
435200990120     C     CRT15X        BEGSR
435300990120     C** SE LA C.A. E' APERTA, VERIFICO SE AVARIA E RESO O DISTRUTTA
435400990120    1C     DCTDSD        IFEQ      'R'
435500990120     C     DCTDSD        OREQ      'D'
435600990126     C**
435700990126     C                   EXSR      CERTAD
435800990120     C* CONTROLLO SE TIPO ANOMALIA E' AVARIA
435900990120     C** CREO ANOMALIA 152 0 153
436000990120    2C     §TARGR        IFEQ      'V'
436100990120     C*
436200990120     C     DCTDSD        IFEQ      'R'
436300990120     C                   Z-ADD     152           COMCAA
436400990120     C                   ELSE
436500990120     C                   Z-ADD     153           COMCAA
436600990120     C                   ENDIF
436700990120     C**
436800990120     C                   EXSR      CRTANM
436900990120    2C                   ENDIF
437000990120    1C                   ENDIF
437100990120     C                   ENDSR
437200990120     C**************************************************************************
437300990120     C* CONTROLLO SE PRESENTE CA PER IL COLLO
437400990120     C**************************************************************************
437500990120     C     CTRDAN        BEGSR
437600990126     C*
437700990120     C                   CLEAR                   WTRDAN
437800990120     C     KBAR          CHAIN     FNDCD02L                           33
437900990126     C*
438000990120    1C     *IN33         IFEQ      *OFF
438100990120     C     DCDATB        ANDEQ     ' '
438200990120     C     KDCT          CHAIN     FNDCT01L                           33
438300050223      * se dctdt2 > 0 la c.a. non è valida x la filiale
438400050224     c                   If        Not *In33 and dctdt2 > 0
438500050224     c                   Eval      *In33 = *On
438600050224     c                   EndIf
438700050224     C
438800990120    2C     *IN33         IFEQ      *OFF
438900990120     C     DCTATB        ANDEQ     ' '
439000990126     C* TROVATA C.A. NON ANNULLATA
439100990120     C                   MOVEL     'S'           WTRDAN            1
439200990126     C**
439300990126     C* CONTROLLO SE DEVO IMPOSTARE FLAG COLLO RITROVATO
439400990126    3C     WRITRO        IFEQ      'S'
439500990126     C                   EXSR      CERTAD
439600990126    4C     §TARIT        IFEQ      'S'
439700990126     C     DCDDFV        ANDEQ     0
439800990126     C                   Z-ADD     BARDFV        DCDDFV
439900990126     C                   Z-ADD     BARFGS        DCDFGS
440000990126     C                   Z-ADD     BARNPS        DCDNPS
440100990126     C                   MOVEL     'S'           DCDFAR
440200990126     C                   UPDATE    FNDCD000
440300990126    4C                   ENDIF
440400990126    3C                   ENDIF
440500990126     C**
440600990126     C                   UNLOCK    FNDCD02L
440700990126     C**
440800990126     C** CONTROLLO SE DEVO CREARE LE ANOMALIE
440900990126    3C     WCREAN        IFEQ      'S'
441000990126     C**
441100990126     C* ANOMALIA 151 - COLLO DI CA CHIUSA
441200990126    4C     DCTDCH        IFGT      0
441300990121     C* IN ARRIVO NON LA CREO PER SPUNTA DI CATEGORIA 4
441400990126    5C     WCAT4         IFEQ      'N'
441500990121     C     BARNPG        ANDNE     4
441600000522     C     BARNPG        ANDNE     8
441700990121     C     WCAT4         OREQ      'S'
441800990120     C                   EXSR      CRE151
441900990126   X5C                   ELSE
442000990121     C                   Z-ADD     151           COMCAA
442100990121     C                   EXSR      CHIUAN
442200990126    5C                   ENDIF
442300990126   X4C                   ELSE
442400990120     C* ANOMALIE 152 O 153
442500990126    5C     DCDDCH        IFEQ      0
442600990120     C                   EXSR      CRT15X
442700990126    5C                   ENDIF
442800990126    4C                   ENDIF
442900990121    3C                   ENDIF
443000990120    2C                   ENDIF
443100990120     C**
443200990120   X1C                   ELSE
443300990120     C* ANNULLATO COME SE NON CI FOSSE
443400990120     C                   SETON                                        33
443500990120    1C                   ENDIF
443600990120     C                   ENDSR
443700990120     C**************************************************************************
443800990120     C* CREO ANOMALIA 151 SE LO PREVEDE LA CAUSALE CHIUSURA
443900990120     C**************************************************************************
444000990120     C     CRE151        BEGSR
444100990120     C     DCTCCH        IFNE      *BLANKS
444200990120     C                   CLEAR                   DSBS02
444300990120     C                   MOVEL     'C'           T02MOD
444400990120     C                   MOVEL     KNSIF         T02SIF
444500990121     C                   MOVEL     'CCH'         T02COD
444600990120     C                   MOVEL     DCTCCH        T02KE1
444700990120     C                   CALL      'TIBS02R'
444800990120     C                   PARM                    KPJBA
444900990120     C                   PARM                    DSBS02
445000990121    2C     T02ERR        IFEQ      ' '
445100990121     C                   MOVEL     T02UNI        DCCH
445200990121     C                   ELSE
445300990121     C                   MOVEL     'S'           §CHCRA
445400990121    2C                   ENDIF
445500990120     C                   ENDIF
445600990120     C**
445700990120     C* CREO
445800990120     C     DCTCCH        IFEQ      *BLANKS
445900990120     C     §CHCRA        OREQ      'S'
446000990120     C                   Z-ADD     151           COMCAA
446100990120     C                   EXSR      CRTANM
446200990120     C                   ENDIF
446300990120     C                   ENDSR
446400990126     C**************************************************************************
446500990126     C* CERCO TABELLA RECORD TIPO ANOMALIA TAD
446600990126     C**************************************************************************
446700990126     C     CERTAD        BEGSR
446800990126    1C     DCTTAD        IFNE      SAVTAD
446900990126     C                   CLEAR                   DSBS02
447000990126     C                   MOVEL     'C'           T02MOD
447100990126     C                   MOVEL     KNSIF         T02SIF
447200990126     C                   MOVEL     'TAD'         T02COD
447300990126     C                   MOVEL     DCTTAD        T02KE1
447400990126     C                   CALL      'TIBS02R'
447500990126     C                   PARM                    KPJBA
447600990126     C                   PARM                    DSBS02
447700990126     C**
447800990126    2C     T02ERR        IFEQ      ' '
447900990126     C                   MOVEL     T02UNI        DTAD
448000990126     C                   ELSE
448100990126     C                   MOVEL     'V'           §TARGR
448200990126     C                   MOVEL     'S'           §TARIT
448300990126    2C                   ENDIF
448400990126     C                   MOVEL     DCTTAD        SAVTAD
448500990126    1C                   ENDIF
448600990126     C                   ENDSR
448700030723     C**************************************************************************
448800030723     C* CERCO TABELLA VARIABILI PGM PARTENZE
448900030723     C**************************************************************************
449000030723     C     CERVPO        BEGSR
449100030723      *
449200030723      * CHAIN A VARIABILI PARTENZE PER PRENDERE MAX PESO E VOLUME CARICABILI
449300030723     C                   CLEAR                   DVPO
449400030723     C                   CLEAR                   DSBS02
449500030723     C                   MOVEL     'C'           T02MOD
449600030723     C                   MOVEL     KNSIF         T02SIF
449700030723     C                   MOVEL     'VPO'         T02COD
449800030723     C                   MOVEL     'VPO'         T02KE1
449900030723     C                   CALL      'TIBS02R'
450000030723     C                   PARM                    KPJBA
450100030723     C                   PARM                    DSBS02
450200030723     C**
450300030723    2C     T02ERR        IFEQ      ' '
450400030725     C                   MOVEL     T02UNI        DVPO
450500030723    2C                   ENDIF
450600110103      *
450700030723     C                   ENDSR
450800000927     C**************************************************************************
450900000927     C* CONTROLLO SE POSSO CREARE ANOMALIE DI MANCA RECOD SECONDO LA
451000000927     C*  ATA FOGLIO E I GIONRI DI PULIZIA
451100000927     C**************************************************************************
451200000927     C     CTRDAT        BEGSR
451300000927     C*
451400000927     C* A CAMBIO DATA RICONTROLLO
451500000927    1C     BARDFV        IFNE      SA2DFV
451600000927     C                   CLEAR                   WCRE10
451700000927     C*
451800000927     C* SE FOGLIO UGUALE A UDATE, VA SICURAMENTE BENE
451900000927    2C     BARDFV        IFEQ      WDATE1
452000000927     C                   MOVEL     'S'           WCRE10            1
452100000927   X2C                   ELSE
452200000927     C* CONTROLLO CON I GG DI PULIZIA
452300000927      * DATA FOGLIO MAGGIORE DATA ULTIMO UTILIZZO: POSSO CREARE ANOM
452400000927    3C     BARDFV        IFGE      PRIDAT
452500000927     C                   MOVEL     'S'           WCRE10            1
452600000927    3C                   ENDIF
452700000927    2C                   ENDIF
452800000927     C*
452900000927     C                   Z-ADD     BARDFV        SA2DFV
453000000927    1C                   ENDIF
453100130301     c*
453200130301     c* Se è da creare --> controllo se si tratta di fil che fa partire colli
453300130301     c*  di altro TFP e non la creo lo stesso
453400130301     c                   clear                   wlocalxco         1
453500130301     c
453600130301     c                   if        wcre10='S' and ( bolxco='W'  or
453700130301     c                             (wesbtt='N' and wesblt='N' and wesart='N'
453800130301     c                              and barnrs>0))
453900130301     c* se si tratta di spunta partenza --> creo la 57
454000130301     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
454100130301     c                             or barnpg=1
454200130301     c                   exsr      CtrXCO
454300130301     c                   if        Indx>0
454400130301     c                   eval      Wlocalxco='S'
454500130301     c                   endif
454600130301     c                   endif
454700130301     c                   endif
454800000927     C*
454900010607     C                   ENDSR
455000010607     C**************************************************************************
455100070131     C* MEMORIZZAZIONE SPUNTE DI CONSEGNA DOPPIE SU FNBRVE0F
455200010607     C**************************************************************************
455300010607     C     SRBRVE        BEGSR
455400070710     c* Se devo tenere una spuynta solo memeorizzo per categoria
455500070710     c*  e segnacollo
455600070710     c                   if        nst(WA)='1'
455700070710     c
455800070131     C     KBRVE2        SETLL     FNBRVE2L                               33
455900010607     C     *IN33         IFEQ      *OFF
456000010607     C* SPUNTA NON ESISTENTE: MEMORIZZO ANCHE LA VECCHIA
456100070131     C                   WRITE     FNBRVE
456200010607     C                   ENDIF
456300070131     C* Salvo campi di FNBRV che potrebbero rimanere invariati
456400070131     C* Li dovrò reimpostare dopo la scrittura di FNBRVe per poi
456500010608     C* fare i giusti aggiornamenti
456600010608     C                   MOVE      BRVATR        SAWATR
456700080317     C                   MOVE      BRVTSU        SAWTSU
456800080317     c                   movel     brv           sawbrv
456900010608     C* IN OGNI CASO MEMORIZZO LA SPUNTA NUOVA
457000010608     C                   MOVEL     'S'           WBRVE             1
457100010608     C                   EXSR      RIEMPI
457200010608     C                   CLEAR                   WBRVE
457300070131     C                   WRITE     FNBRVE
457400070131     C* Reimposto campi di FNBRV come da lettura iniziale
457500010608     C                   MOVE      SAWATR        BRVATR
457600080317     C                   MOVE      SAWtsu        BRVtsu
457700080317     C                   MOVE      SAWbrv        BRV
457800110103     c*
457900070710     c                   else
458000070710     c* Se devo memorizzare le spunte senza anomalia controllo per foglio
458100070710     C     KBRVE1        SETLL     FNBRVE1L                               33
458200070710     C     *IN33         IFEQ      *OFF
458300070710     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
458400070710     C                   WRITE     FNBRVE
458500070710     C                   ENDIF
458600070710     C                   ENDIF
458700070710     c*
458800010607     C                   ENDSR
458900020124     C**************************************************************************
459000020204     C* CERCO TERMINAL PARTENZA LNP COLLO
459100020124     C**************************************************************************
459200020204     C     TERPAR        BEGSR
459300020124     C                   CLEAR                   DSLV55
459400020124     C                   MOVEL     'P'           D55TPT
459500020124     C                   MOVEL     LNPB          D55LIN
459600020124     C                   MOVEL     BARDFV        D55DRF
459700020213     C                   EXSR      FNLV55
459800020204     C                   MOVE      D55TFP        LNPTFP
459900020204     C**
460000020204     C                   ENDSR
460100020204     C**************************************************************************
460200020204     C* CERCO TERMINAL ARRIVO LNA COLLO
460300020204     C**************************************************************************
460400020204     C     TERARR        BEGSR
460500020124     C**
460600020124     C* LINEA DI ARRIVO
460700020124     C     BARLNA        IFNE      SVVLNA
460800020124     C     LNPB          ORNE      SVVLNP
460900020204     C     BARDFV        ORNE      SVVDFV
461000020124     C*
461100020124     C* TEMINAL DI ARRIVO
461200020124     C                   CLEAR                   DSLV55
461300020124     C                   MOVEL     ' '           D55TPT
461400020124     C                   MOVEL     BARDFV        D55DRF
461500020124     C                   MOVEL     BARLNA        D55LIN
461600020124     C                   MOVEL     LNPB          D55LNP
461700020213     C                   EXSR      FNLV55
461800020124     C**
461900020213     C                   MOVEL     D55TFA        LNATFA
462000020124     C*
462100020124     C                   MOVEL     BARLNA        SVVLNA
462200020204     C                   MOVEL     LNPB          SVVLNP
462300020204     C                   MOVEL     BARDFV        SVVDFV
462400020124     C                   ENDIF
462500020124     C                   ENDSR
462600020124     C**************************************************************************
462700020124     C* AGGIORNO LE BOLLE PARTENZA
462800020124     C**************************************************************************
462900020124     C     AGGBLT        BEGSR
463000060223     C                   CLEAR                   WEND              1
463100020124     C* NON FACCIO AGGIORNAMENTI SE SPUNTA DI COLLO DISGUIDATO
463200070221    1C     dls45FLG      IFNE      '6'
463300020124     C*
463400020124     C                   MOVEL     'P'           WTIBOL            1
463500020124     C                   MOVEL     'S'           WBOPAR
463600020124     C*
463700020124     C* Se collo già consegnato e spunta partenza di pistola manuale
463800020124     C* ignoro la spunta
463900020124    3C     BARNPG        IFEQ      1
464000020124     C     BLTDCM        ANDNE     *ZEROS
464100020124     C     RPS(Y)        ANDEQ     'M'
464200020124     C                   MOVE      'N'           OKAGG
464300020201     C                   MOVEL     '1'           WEND
464400020204     C                   UNLOCK    FNBLT27L
464500020201     C                   GOTO      ENDBLT
464600020124    3C                   END
464700020204     C*  SE NON IMPOSTATO VOLUME E PESO DA PARTENZA
464800020204     C     WWFVC         IFEQ      ' '
464900020124     C                   MOVEL     'P'           WWFVC
465000020204     C                   ENDIF
465100020124     C*
465200020124     C                   MOVEL     'P'           BLTTBO
465300020124     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
465400020124     C* NE PRECEDENTE
465500020124     C     FLGPRE        IFNE      'A'
465600020124     C     BLTSPE        ANDNE     SAVSPE
465700020124     C     WAGGF         IFEQ      'S'
465800020124     C                   EXSR      WRTAGB
465900020124     C                   CLEAR                   WAGGF
466000020124     C                   ENDIF
466100020124     C                   MOVEL     BLTSPE        SAVSPE
466200020124     C                   ENDIF
466300020124     C*
466400020124    3C     BARCAN        IFNE      ' '
466500020124     C* NON MEMORIZZO ANOMALIA "E" SU DETTAGLIO COLLO PERCHE' LA NON
466600020124     C* INDICA UNA VERA E PROPRIA ANOMALIA MA SOLO UN ERRORE PER LINEA
466700020124     C* MANCANTE SUL FOGLIO AL MOMENTO DELLA SPUNTA DEL COLLO
466800020124     C     BARCAN        ANDNE     'E'
466900020124     C                   MOVEL     BARCAN        BLTCAN
467000020124     C                   MOVEL     'S'           WAGGF             1
467100020124    3C                   END
467200160421     c
467300160421     c* Se zona bolla <> zona spunta --> riaggiorno zona bolla
467400160422     c                   if        bolznc<>'  ' and
467500160429     c                             bolznc<>%editc(barznc:'X')  and
467600160503     c                             barnpg<>8
467700160503     c                   if        tps(Y)= 'L' or tps(y)='C'
467800160421     c                   movel     barznc        wznc
467900160421     C                   MOVEL     'S'           WAGGF             1
468000160421     c                   endif
468100160503     c                   endif
468200020124     C*
468300041105     c                   clear                   wagvolart         1
468400041105     c                   clear                   wagpesart         1
468401180124     c                   clear                   wagvolart_a       1
468402180124     c                   clear                   wagpesart_a       1
468500041105     c
468600020124    3C     BARVUC        IFGT      0
468700020124     C* AGGIORNO IL VOLUME CML SOLO SE IL VOLUME SUL COLLO:
468800020124     C* - E' = 0
468900020124     C* - E' DELL'ARRIVO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
469000020124     C**- E' DEL TRANSITO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
469100020124    4C     BLTVUC        IFEQ      0
469200020124     C     BLTVUC        ORNE      0
469300020214     C     BLTFVC        ANDNE     'P'
469400020124     C     WWFVC         ANDEQ     'P'
469500041013     C                   MOVEL     'S'           WFVC
469600020124     C                   Z-ADD     BARVUC        BLTVUC
469700041105     c                   eval      wagvolart='S'
469701180124     c                   eval      wagvolart_a='S'
469800020124     C                   MOVEL     'S'           WAGGF             1
469900020124     C                   MOVEL     WWFVC         BLTFVC
470000020124    4C                   END
470100020124     C*
470200020124    3C                   END
470300020124     C*STESSO GIRO DEL VOLUME ANCHE PER IL PESO
470400020124    3C     BARPUC        IFGT      0
470500020124     C* AGGIORNO IL VOLUME CML SOLO SE IL VOLUME SUL COLLO:
470600020124     C* - E' = 0
470700020124     C* - E' DELL'ARRIVO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
470800020124     C**- E' DEL TRANSITO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
470900020124    4C     BLTPUC        IFEQ      0
471000020124     C     BLTPUC        ORNE      0
471100020214     C     BLTFPC        ANDNE     'P'
471200020124     C     WWFVC         ANDEQ     'P'
471300041012     c                   movel     'S'           wfpc
471400020124     C                   Z-ADD     BARPUC        BLTPUC
471500041105     c                   eval      wagpesart='S'
471501180124     c                   eval      wagpesart_a='S'
471600020124     C                   MOVEL     'S'           WAGGF             1
471700020124     C                   MOVEL     WWFVC         BLTFPC
471800020124    4C                   END
471900020124     C*
472000020124    3C                   END
472100020124     C*
472200020124     C* SPUNTA LOCALE IN DISGUIDO --> AGGIORNO LA DATA ENTRATA TRANSITO
472300020124    3C     WSPLOC        IFEQ      'D'
472400020124     C                   MOVEL     BARDFV        BLTDET
472500020124     C                   MOVEL     BARFGS        BLTFLP
472600020124     C                   CLEAR                   BLTDUT
472700020124     C                   MOVEL     'S'           WAGGF             1
472800020124     C*
472900020124   X3C                   ELSE
473000020124     C*
473100020124     C* SE NON C'E' AGGIUNGO DATA E PISTOLA
473200020124     C* SOLO SE NON E' SPUNTA DISGUIDO IN AREA
473300020124    4C     BLTDSE        IFEQ      0
473400020124     C                   MOVEL     BARDFV        BLTDSE
473500020124     C                   MOVEL     BARNPS        BLTNPE
473600020124     C                   MOVEL     'S'           WAGGF             1
473700020124   X4C                   ELSE
473800020124     C*
473900020124     C* SE E' QUESTA LA SPUNTA CHE HA AGGIORNATO IL COLLO RIAGGIORNO
474000020124    5C   05BLTNPE        IFEQ      BARNPS
474100020124     C                   MOVEL     BARDFV        BLTDSE
474200020124     C                   MOVEL     'S'           WAGGF             1
474300020124    5C                   ENDIF
474400020124     C*
474500020124     C* RIAGGIRNO COLLO SE PRECEDENTE SPUNTA NON VALIDA E ATTUALE VALID
474600020124    5C  N05BLTNPE        IFNE      BARNPS
474700020124     C     RPS(Y)        ANDEQ     'L'
474800020124     C                   MOVEL     BLTNPE        ALF2
474900020124     C*
475000020124     C* CERCO RAGGRUPPAMENTO PISTOLA
475100020124     C                   EXSR      RICRAG
475200020124     C*
475300020124    6C     RPS(Z)        IFEQ      'M'
475400020124     C     RPS(Z)        OREQ      'A'
475500020124     C                   MOVEL     BARDFV        BLTDSE
475600020124     C                   MOVEL     BARNPS        BLTNPE
475700020124     C                   MOVEL     'S'           WAGGF             1
475800020124    6C                   END
475900020124    5C                   END
476000020124     C*
476100020124    4C                   END
476200020124    3C                   END
476300020124     C*
476400041105     C*  Se FLG3 = A non devo aggiornare fnblt perchè richiamo solo per
476500041105     c*  gli arrivi
476600070208    3C     dls45flg2     IFNE      'A'
476700020124     C                   UPDATE    FNBLT000
476701180124     c* se aggiornato peso o volume aggiorno peso/volume e TAP su FIART
476702180124     c* (mi serve per gestione incompatibili)
476703180124    4c                   if        wagvolart_a='S' or wagpesart_a='S'
476704180126     c                   eval      w_puc = bltpuc
476705180126     c                   eval      w_fpc = bltfpc
476706180126     c                   eval      w_vuc = bltvuc
476707180126     c                   eval      w_fvc = bltfvc
476708180124     c                   exsr      UpdFiart
476724180124    4c                   endif
476800041105     c* Se aggiornato peso e volume e collo partito, aggiorno anche ART
476900041105     c                   if        bltdfv>0 and
477000160421     c                             (wagvolart='S' or wagpesaRT='S')
477100041105     C     KBAR          CHAIN     FNART27L                           3434
477200041105     C                   IF        NOT *IN34
477300041105     c* VOLUME
477400041105     C                   if        wagvolart='S'
477500041105     c* Se lan fedex non aggiorno
477600041105     c                   if        artvuc=0 or ntwbar<>'FED'
477700041105     c                   eval      wagbart='S'
477800041105     c                   eval      artvuc=bltvuc
477900041105     c                   eval      artfvc=bltfvc
478000041105     c                   endif
478100041105     c                   endif
478200041105     C                   if        wagpesart='S'
478300041105     c                   eval      wagbart='S'
478400041105     c                   eval      artpuc=bltpuc
478500041105     c                   eval      artfpc=bltfpc
478600041105     c                   endif
478700160421     c
478800041105     c                   update    fnart000
478900041105     C                   ENDIF
479000041105     C                   ENDIF
479100041105     C
479200020124     C**
479300020124     C* SE RICHIAMATO AGGIORNO SUBITO LA BOLLA
479400110103    5c   02              if        wfpc='S' or wfvc='S'
479500160421     c                             or wznc<>*blanks
479600020124     C                   SETOFF                                       3489
479700020124     C     KBLT          CHAIN     FNBLP01L                           3489
479800041021     c                   if        not *in34 and not *in89
479900041012     c                   if        wfvc='S'
480000041012     C                   MOVEL     99999         BLPNCR
480100041012     c                   endif
480200041012     c                   if        wfpc='S'
480300041012     C                   MOVEL     99999         BLPNCp
480400041012     c                   endif
480500160421     c                   if        wznc<>*blanks
480600160421     c                   movel     wznc          blpznc
480700160421     c                   endif
480800041012     c
480900041012     C                   UPDATE    FNBLP000
481000050223     c
481100050223     c* Aggiorno subito anche la bolla arrivo
481200050224     c                   if        wagbart='S'
481300050223     C     KBLT          CHAIN     FNarb01L                           3489
481400050223     C                   MOVEL     'E'           arbfbs
481500160421     c                   if        wznc<>*blanks
481600160421     c                   movel     wznc          arbznc
481700160421     c                   endif
481800050224     c  n89
481900050224     cann34              update    fnarb000
482000050224     c                   clear                   wagbart
482100050224     c                   endif
482200041012     c
482300041012     c                   clear                   wfpc
482400041012     c                   clear                   wfvc
482500160421     c                   clear                   wznc
482600041012     c                   endif
482700020124    5C                   ENDIF
482800020124     C**
482900020124   X3C                   ELSE
483000020124     C                   UNLOCK    FNBLT27L
483100020124    3C                   ENDIF
483200020124     C*
483300020213     C*CHIUDO EVENTUALI ANOMALIE DI DISGUIDO,SE COLLO NON DEL TFP
483400020124     C     BARNRS        IFGT      *ZEROS
483500020214     C*
483600020213     C                   CLEAR                   DSLV55
483700020213     C                   MOVEL     'P'           D55TPT
483800020213     C                   Z-ADD     BARLNP        D55LIN
483900020214     C                   Z-ADD     DSPB          D55DRF
484000020213     C                   EXSR      FNLV55
484100020131     C**
484200020213     C     D55TFP        IFNE      LNPTFP
484300020124     C                   MOVEL     'S'           SAVMAC
484400020124     C                   Z-ADD     200           COMCAA
484500020124     C                   EXSR      CHIUAN
484600020124     C*
484700020124     C                   Z-ADD     55            COMCAA
484800020124     C                   EXSR      CHIUAN
484900020124     C*
485000020124     C                   Z-ADD     56            COMCAA
485100020124     C                   EXSR      CHIUAN
485200020131     C                   ENDIF
485300020124     C*
485400020124     C                   CLEAR                   SAVMAC
485500020124     C                   END
485600020124     C*
485700020124   X1C                   ELSE
485800020124     C                   UNLOCK    FNBLT27L
485900020124    1C                   ENDIF
486000020124     C**
486100020201     C     ENDBLT        ENDSR
486200020131     C**************************************************************************
486300020131     C* AGGIORNO LE BOLLE ARRIVO
486400020131     C**************************************************************************
486500020131     C     AGGART        BEGSR
486600020201     C                   CLEAR                   WEND
486700020131     C*
486800020131     C     WTIBOL        IFEQ      'P'
486900020131     C                   MOVEL     'L'           WTIBOL
487000020131     C                   ELSE
487100020131     C                   MOVEL     'A'           WTIBOL
487200020131     C                   ENDIF
487300020131     C*
487400020131     C* E S I S T E   B O L L A   A R R I V O
487500020131    2C     WESART        IFEQ      'S'
487600020131     C* SE FLG=6 SVINCOLO RECORD
487700070221     C     dls45FLG      IFEQ      '6'
487800020131     C                   UNLOCK    FNART27L
487900020131     C                   ENDIF
488000020131     C**
488100020131     C     BARNPS        IFEQ      89
488200020131     C                   UNLOCK    FNART27L
488300020131     C                   ELSE
488400020214     C     ARTFLP        IFEQ      0
488500020214     C                   Z-ADD     ARTDFV        COMDFV
488600020214     C                   ELSE
488700020214     C                   Z-ADD     ARTDUT        COMDFV
488800020214     C                   ENDIF
488900020131     C                   MOVE      ARTDCM        WATDCM
489000020131     C                   MOVE      ARTDAM        SAVDAM
489100020131     C                   MOVEL     'S'           WBOAR             1
489200020131     C*
489300020131     C* SE FLG=6 CREO ANOMALIA 61
489400160624     c* Anomalia 61 eliminata
489500160624     C***  dls45FLG      IFEQ      '6'
489600160624     C***                EXSR      CRTA60
489700160624     C***                MOVEL     '1'           WEND
489800160624     C***                GOTO      ENDART
489900160624     C***                ENDIF
490000020131     C*
490100020131     C     WWFVC         IFEQ      *BLANKS
490200020131     C                   MOVEL     'A'           WWFVC
490300020131     C                   END
490400020131     C*
490500020131     C* LA PRIMA VOLTA MEMORIZZO LA SPEDIZIONE
490600020131     C                   MOVEL     'A'           ARTTBO
490700020131     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
490800020131     C* NE PRECEDENTE
490900020131     C     FLGPRE        IFNE      'A'
491000020131     C     ARTSPE        ANDNE     SAVSPE
491100020131     C     WAGGF         IFEQ      'S'
491200020131     C                   EXSR      WRTAGB
491300020131     C                   CLEAR                   WAGGF
491400020131     C                   ENDIF
491500020131     C                   MOVEL     ARTSPE        SAVSPE
491600020131     C                   ENDIF
491700160422     c* zona bolla
491800160422     c                   if        bolznc<>'  '   and
491900160429     c                             bolznc<>%editc(barznc:'X') and
492000160503     c                             barnpg<>8
492100160503     c                   if        tps(Y)= 'L' or tps(y)='C'
492200160422     c                   movel     barznc        wznc
492300160422     C                   MOVEL     'S'           WAGGF             1
492400160422     c                   endif
492500160503     c                   endif
492600020131     C*
492700020131     C* CODICE ANOMALIA
492800041012     c                   exsr      AGGCAN
492900020131     C*
492901180124     c                   clear                   wagvolart_a       1
492902180124     c                   clear                   wagpesart_a       1
493000020131     C* VOLUME DA MACCHINA SMISTACOLLI
493100041012     C                   EXSR      AGGVOL
493200030604     C*
493300020131     C* PESO DA MACCHINA SMISTACOLLI
493400041012     C                   EXSR      AGGpes
493500110103     c*
493600110103     c*
493700091209     c* Se chi spara è la linea di arrivo del collo o il suo gestore
493800091209     c* --> aggiorno la data più bassa di arrivo 1° collo
493900091211     C* CARICO LA L6 DELLA FILIALE GESTIONE
494000091211     C     BPES          IFNE      SAVPES
494100091211     C                   MOVEL     BPES          SAVPES
494200091211     C                   EXSR      CARL6
494300091211     C                   ENDIF
494400110103      *
494500091211     C     BARLNA        LOOKUP    L6                                     88
494600150615     c*
494700150817     c* non aggiorno più data e ora arrivo alla filiale finale di arrivo
494800150817     c*  er l'estero perchè creeremo apposito campo per
494900150817     c*  indicare la data di arrivo all'hub/terminal di arrivo
495000150622     c***                if        not *in88 and  boltfa=BPES  and
495100150622     c***                          (ntwbar='EEX' or ntwbar='DPD' or
495200150622     c***                          ntwbar='FED')
495300150622     c***                seton                                        88
495400150622     c***                endif
495500110103      *
495600150817      * tolto test su data spedizione dal 2009/12 in poi
495700150817    3c                   if        *in88 or artlna=bpes
495800091211     c     kar52         chain     fiar501l
495900091209    4c                   if        %found(fiar501l)
496000091209     c                   movel     ar5uni        dar5gen
496100091209     c                   else
496200091209     c                   clear                   dar5gen
496300091209    4c                   endif
496400110103     c*
496500091209     c                   clear                   warrdt
496600091209     c                   clear                   warrhm
496700091209    4c                   if        §ar5arrdt>*zeros
496800091209     c                   movel     §ar5arrdt     warrdt            8 0
496900091209    5c                   if        §ar5arrhm>*zeros
497000091209     c                   movel     §ar5arrhm     warrhm            4 0
497100091209    5c                   endif
497200091209    4c                   endif
497300110103     c*
497400101129     c* Uso data e ora immissione se non superiore all'ora caricamento
497500150817     c                   if        (bardfs<brvdcs) or
497600150817     c                             (bardfs=brvdcs and barhms<brvhcs)
497700101130     c                   movel     bardfs        datReale
497800101130     c                   movel     barhms        oraReale
497900101129     c                   else
498000101129     c                   movel     brvdcs        datReale
498100101129     c                   movel     brvhcs        oraReale
498200101129     c                   endif
498300110103     c*
498400091211     c                   if         warrdt=0  or
498500101129    4c                             (warrdt>0 and warrdt>datReale) or
498600101129     c                             (warrdt=datReale and warrhm>oraReale)
498700091209     c                   exsr      AggioFIAR5
498800110103     c*
498900091209   x4c                   else
499000091209     c                   unlock    fiar501l
499100091209    4c                   endif
499200091209    3c                   endif
499300180124      **
499400180115      * Se chi spunta è il terminal di arrivo --> verifico se spunta incompatibi
499500091209      **
499600180125     c***                if        bpes=boltfa and (brvspg='J'  or
499601180125     c***                          (savcan='S' and
499602180125     c***                          (AllINC='S' or %lookup(boltfa:fic)>0)
499604180125     c***                           ))
499605180125     c***                exsr      sr_addinc
499800180125     c***                ENDIF
499900180115      *
500000020131    3C     ARTDAM        IFEQ      0
500100020131     C                   MOVEL     BARDFV        ARTDAM
500200020131     C                   MOVEL     'S'           WAGGF             1
500300020131     C                   MOVEL     BARNPS        ARTNAP
500400050509     c* Imposto flag ARTFL2= 'P' se catagoria di spunta è tipica processo
500500050509     c*  operativo partenza
500600050509     c                   if        barnpg=5 or (barnpg=1 and wdefl=' ') or
500700050509     c                             (barnpg=3  and barspg='P')
500800050509     c                   eval      artfl2='P'
500900050509     c                   else
501000050509     c                   eval      artfl2=' '
501100050509     c                   endif
501200020131     C*
501300020131     C                   MOVEL     ' '           ARTDAB
501400020131     C                   SETON                                        03
501500020131     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
501600160624     c* ANOMALIE 60 E 61 ELIMINATE
501700160624     C***                EXSR      CRTA60
501800020131     C*
501900020131   X3C                   ELSE
502000020131    4C   05ARTNAP        IFEQ      BARNPS
502100020131     C                   MOVEL     BARDFV        ARTDAM
502200020131     C                   MOVEL     'S'           WAGGF             1
502300020131    4C                   ENDIF
502400020131     C*
502500020131     C* 'D' DI DISABILITAZIONE COLLO :
502600020131     C*   SE RISPUNTATO LANCIO PGM DI ABILITAZIONE X RIABILITARE BOLLA
502700020131    4C  N05ARTDAB        IFEQ      'S'
502800020131     C                   SETON                                        03
502900020131     C                   MOVEL     ' '           ARTDAB
503000040324     C                   MOVEL     'S'           WAGGF             1
503100020131    4C                   ENDIF
503200020131     C*
503300020131    4C  N05ARTNAP        IFNE      BARNPS
503400020131     C     RPS(Y)        ANDEQ     'L'
503500020131     C                   MOVEL     ARTNAP        ALF2              2
503600020131     C*
503700020131     C* CERCO RAGGRUPPAMENTO PISTOLA
503800020131     C                   EXSR      RICRAG
503900020131     C*
504000020131    5C     RPS(Z)        IFEQ      'M'
504100020131     C     RPS(Z)        OREQ      'A'
504200020131     C                   MOVEL     'S'           WAGGF             1
504300020131     C                   MOVEL     BARDFV        ARTDAM
504400020213     C                   MOVEL     BARNPS        ARTNAP
504500050509     c* Imposto flag ARTFL2= 'P' se catagoria di spunta è tipica processo
504600050509     c*  operativo partenza
504700050509     c                   if        barnpg=5 or (barnpg=1 and wdefl=' ') or
504800050509     c                             (barnpg=3  and barspg='P')
504900050509     c                   eval      artfl2='P'
505000050509     c                   else
505100050509     c                   eval      artfl2=' '
505200050509     c                   endif
505300020131     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
505400160624     c* anomalie 60 e 61 eliminate
505500160624     C***                EXSR      CRTA60
505600020131     C*
505700020131    5C                   ENDIF
505800020131    4C                   ENDIF
505900020131     C*
506000020131    3C                   END
506100020131     C*
506200020131     C                   Z-ADD     ARTDCM        WDCM
506300020131     C**
506400070208    3C     dls45flg2     IFNE      'A'
506500020131     C                   UPDATE    FNART000
506501180124    4c                   if        wagvolart_a='S' or wagpesart_a='S'
506502180126     c                   eval      w_puc = artpuc
506503180126     c                   eval      w_fpc = artfpc
506504180126     c                   eval      w_vuc = artvuc
506505180126     c                   eval      w_fvc = artfvc
506506180124     c                   exsr      updfiart
506507180124    4c                   endif
506600020131     C                   ELSE
506700020131     C                   UNLOCK    FNART27L
506800020131    3C                   ENDIF
506801180125      * Se chi spunta è il terminal di arrivo --> verifico se spunta incompatibi
506802180125      **
506803180125     c                   if        bpes=boltfa and barnpg=2 and
506804180126     c                             (barspg='J'  or
506805180125     c                             (savcan='S' and
506806180125     c                             (AllINC='S' or
506807180125     c                              %lookup(%editc(boltfa:'X'):fic)>0
506808180125     c                              )))
506809180125     c                   exsr      sr_addinc
506810180125     c                   ENDIF
506900020131     C*
507000050223     C     KARB          CHAIN(N)  FNARB01L                           32
507100020131     C*
507200020131    3C     *IN32         IFEQ      *OFF
507300020131     C*
507400070208    4C     dls45flg2     IFNE      'A'
507500020131     C* SOLO SE PGM RICHIAMATO:
507600020131     C* SE VARIATA DATA DI ARRIVO SULLA BOLLA RIELABORO LA STATISTICA
507700020131    5C     *IN02         IFEQ      *ON
507800020131     C     ARTDAM        ANDNE     SAVDAM
507900020131    6C     ARTDAM        IFNE      WARDAM
508000020131     C     SAVDAM        ORNE      WSVDAM
508100020131     C                   EXSR      WRTSTA
508200020131     C                   MOVE      ARTDAM        WARDAM
508300020131     C                   MOVE      SAVDAM        WSVDAM
508400020131    6C                   END
508500020131    5C                   END
508600020131     C*
508700020131     C                   MOVEL     ARBNDC        WABNDC
508800040430     C                   MOVEL     ARBcca        WABcca
508900020131     C   03              MOVEL     'S'           WFBS
509000020131     C*
509100020131    4C                   ENDIF
509200020131     C**
509300020131     C** VEDO SE BOLLA GIACENTE
509400020131    4C     ARBFBC        IFEQ      'G'
509500050331     C     KGCA          CHAIN     Tigcp21L                           34
509600161007     c  n34              z-add     gcpmgc        gcpdgc
509700161007     c  n34              movel     gcpagc        gcpdgc
509800161007    5C  N34GCPFAS        IFLT      35
509900161007    5C     GCPFAS        oreq      36
510000161007     c* Verifico tramite driver se il collo dove essere in IMG
510100161007     c                   clear                   fnlgi1ds
510200161007     c                   eval      ilgi1aas=arbaas
510300161007     c                   eval      ilgi1lnp=arblnp
510400161007     c                   eval      ilgi1nrs=arbnrs
510500161007     c                   eval      ilgi1nsp=arbnsp
510600161007     c                   select
510700161007     c                   when      arbncp=arbncl
510800161007     c                   eval      ilgi1pkf=arbpkc
510900161007     c                   when      arbpkc>arbpkf
511000161007     c                   eval      ilgi1pkf=arbpkc
511100161007     c                   other
511200161007     c                   eval      ilgi1pkf=arbpkf
511300161007     c                   endsl
511400161007     c                   movel     fnlgi1ds      kpjbu
511500161007     c                   call      'FNLGI1R'
511600161007     c                   parm                    kpjba
511700161007     c                   parm                    tigcp00ds
511800161007     c                   movel     kpjbu         fnlgi1ds
511900161021     c* In questo caso la sped potrebbe essere sia in IMA che in IMG
512000170125     c*  quindi non devo dare alcun fuori posto con la nuova procedura
512100170125     c*  anche se ha Y (ovvero portato nel giorno in IMG:
512200170125     c*  potrebbe mancare un collo della sped spuntato succesivamente)
512300170125   5ac****               if        olgi1AMG='S'
512400170125     c
512500170125   5ac                   if        olgi1AMG='S' or olgi1AMG='Y'
512600161021     C                   MOVEL     'D'           WGIAC
512700161021  x5ac                   else
512800170125   5bc****               if        olgi1AMG='Y'  or olgi1inmg='S'
512900170125   5bc                   if        olgi1inmg='S'
513000161201     C                   MOVEL     'S'           WGIAC
513100161201   5bc                   endif
513200161201   5ac                   endif
513300161201     c
513400161201   5ac                   if        wgiac<>' '
513500020131     C* NON LA CONSIDERO GIACENTE SE IN QUESTO MOMENTO C'E' UNA BOLLA
513600020131     C*  FIGLIA IN CONSEGNA O TUTTE LE FIGLIE SONO BLOCCATE
513700020131     C                   MOVEL     'S'           WFIBLO            1
513800020131     C                   CLEAR                   WESFI
513900020131     C     KARB          SETLL     FNLBL02L
514000020131     C     KARB          READE     FNLBL02L                               29
514100020131     C**
514200020131    6C     *IN29         DOWEQ     *OFF
514300020131     C*
514400050223     C     KLBL          CHAIN(N)  FNARB01L                           28
514500020131    7C     *IN28         IFEQ      *OFF
514600020131     C                   MOVEL     'S'           WESFI             1
514700020131     C* FIGLIA IN CONSEGNA
514800020131     C* NON LA CONSDIERO SE E' CONSEGNATA
514900020131   7AC     ARBDCM        IFEQ      0
515000020131    8C     ARBNDC        IFNE      0
515100020131     C                   SETON                                        29
515200020131     C                   CLEAR                   WGIAC
515300020131   X8C                   ELSE
515400020131     C*
515500020131     C* SE NON E' NEMMENO BLOCCATA MEMORIZZO
515600020131    9C     ARBFBC        IFNE      'B'
515700020131     C                   MOVEL     'N'           WFIBLO
515800020131    9C                   ENDIF
515900020131    8C                   ENDIF
516000020131   7AC                   ENDIF
516100170125    7C                   ENDIF
516200020131     C*
516300020131     C  N29KARB          READE     FNLBL02L                               29
516400020131    6C                   ENDDO
516500020131     C*
516600020131     C* SE TUTTE LE FIGLIE SONO BLOCCATE --> ESCLUDO
516700161201    6C     WGIAC         IFNE      ' '
516800020131     C     WESFI         ANDEQ     'S'
516900020131     C     WFIBLO        ANDEQ     'S'
517000020131     C                   CLEAR                   WGIAC
517100020131    6C                   ENDIF
517200020131     C*
517300161021   5aC                   ENDIF
517400161007    5C                   ENDIF
517500020131    4C                   ENDIF
517600020131    3C                   ENDIF
517700020131     C*
517800020131     C** COLLO EXPORT: VEDO SE CREARE L'ANOMALIA COLLO INVIATO PARTNER
517900020131     C*  MA RISPARATO IN IMA O IN PARTENZA/ARRIVO
518000020131     C*  NON CREO ANOMALIA SE E' DATA =99999999 PERCHE' SI TRATTA DI
518100020131     C*  DIROTTAMENTO E QUINDI VALGONO I COLLI DELLA FIGLIA E NON
518200020131     C*  DELLA MAMMA
518300020212    3C     NTWBAR        IFEQ      'EEX'
518400060322    3C     NTWBAR        oreq      'DPD'
518500060322    3C     NTWBAR        oreq      'FED'
518600060322    4C     BARDFV        ifGT      ARTDTR
518700020131     C     ARTDTR        ANDGT     0
518800020131     C     ARTDTR        ANDNE     *HIVAL
518900020131     C** CREO ANOMALIA 600 O 620
519000020131     C                   MOVEL     'S'           W60X              1
519100060322    4C                   ENDIF
519200060322    3C                   ENDIF
519300020131    2C                   ENDIF
519400020131     C*
519500020131   X2C                   ELSE
519600020131     C*
519700070221   2AC     dls45FLG      IFNE      '6'
519800020131     C** CREO LE ANOMALIE DI MANCA BOLLA SOLO SE NON SONO PASSATI I
519900020131     C*  GG DIPULIZIA BOLLE ARRIVI (PER EVITARE MANCA RECORD ERRATI)
520000020131     C                   EXSR      CTRDAT
520100020131     C**
520200020131   2BC     WCRE10        IFEQ      'S'
520300130301     c     wlocalxco     andne     'S'
520400020131     C**
520500020131     C* ANOMALIA: "MANCA BOLLA" --> ANCHE PER SPEDIZIONE LOCALE SE
520600020131     C*  E' SPARATA NELLE CATEGORIE ARRIVI
520700020131     C*  IL FOGLIO DI DEFLUENZA E' COME UNA CATEGORIA ARRIVI
520800020131    3C  N05BARNPG        IFNE      5
520900020131     C     BARSPG        ANDNE     'P'
521000020131     C     BARSPG        ANDNE     'I'
521100020131     C     WTIBOL        ANDEQ     'L'
521200020131     C     WTIBOL        OREQ      'A'
521300020131     C* LA CATEGORIA 1 LA CONSIDERO SOLTANTO SE E' DEFLUENZA
521400020131     C*
521500020131    4C     BARNPG        IFNE      1
521600020131     C     WTIBOL        OREQ      'A'
521700020131     C     WDEFL         OREQ      '1'
521800020131     C                   Z-ADD     10            COMCAA
521900020131     C                   EXSR      CRTANM
522000020131     C* Se l'anomalia 10 esiste già a livello max. creo anom. 410 per
522100020131     C* segnalare il persistere del manca record
522200020131     C     WANM10        IFEQ      '1'
522300020131     C                   Z-ADD     410           COMCAA
522400020131     C                   EXSR      CRTANM
522500020131     C                   END
522600020131    4C                   ENDIF
522700020131    3C                   ENDIF
522800020131     C*
522900020131     C* SE LNA ESTERA VERIFICO SE C'E' EVENTO 2 PER CREARE ANOM.600/620
523000020212    3C     NTWBAR        IFEQ      'EEX'
523100060322    3C     NTWBAR        oreq      'DPD'
523200060322    3C     NTWBAR        oreq      'FED'
523300020131     C*
523400020131     C* SE LNA ESTERA APRO FILE FNEVS02L
523500020131    4C     WAPER         IFEQ      ' '
523600020131     C                   OPEN      FNEVS02L
523700020131     C                   MOVEL     '1'           WAPER             1
523800020131    4C                   ENDIF
523900020131     C*
524000020131     C                   MOVEL     'E2 '         KCEV
524100020131     C     KEVS          SETGT     FNEVS02L
524200020131     C     KEVS          READPE    FNEVS02L                               34
524300020131    4C     *IN34         IFEQ      *OFF
524400020131     C     EVSDEV        ANDLT     BARDFV
524500020131     C* CREO ANOMALIA 600 E 620
524600020131     C                   MOVEL     'S'           W60X
524700020131    4C                   ENDIF
524800020131    3C                   ENDIF
524900020131   2BC                   ENDIF
525000020131   2AC                   ENDIF
525100020131    2C                   ENDIF
525200020131     C**
525300020201     C     ENDART        ENDSR
525400020213     C**************************************************************************
525500020213     C* AGGIORNO LE BOLLE TRANSITO
525600020213     C**************************************************************************
525700020213     C     AGGBTT        BEGSR
525800020213     C                   MOVEL     'T'           WTIBOL
525900020214     C* MEMORIZZO CHE C'E' BOLLA TRANSITO, MI SERVE PER CRTA50
526000020214     C                   MOVE      BTTDFV        COMDFV
526100020213     C* LA PRIMA VOLTA MEMORIZZO LA SPEDIZIONE
526200020213     C                   MOVEL     'T'           BTTTBO
526300020213     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
526400020213     C* NE PRECEDENTE
526500020213     C     BTTSPE        IFNE      SAVSPE
526600020213     C     WAGGF         IFEQ      'S'
526700020213     C                   EXSR      WRTAGB
526800020213     C                   CLEAR                   WAGGF
526900020213     C                   ENDIF
527000020213     C                   MOVEL     BTTSPE        SAVSPE
527100020213     C                   ENDIF
527200020213     C*
527300020213    5C     BARCAN        IFNE      ' '
527400020213     C     BARCAN        ANDNE     'E'
527500020213     C                   MOVEL     BARCAN        BTTCAN
527600020213     C                   MOVEL     'S'           WAGGF             1
527700020213    5C                   END
527800020213     C*
527900020213    5C     BARVUC        IFGT      0
528000020213     C*
528100020213    6C     BTTVUC        IFEQ      0
528200041013     c                   movel     'S'           WFVC
528300020213     C                   Z-ADD     BARVUC        BTTVUC
528400020213     C                   MOVEL     'S'           WAGGF             1
528500020213     C                   MOVEL     'T'           BTTFVC
528600020213    6C                   END
528700020213     C*
528800020213    5C                   END
528900020213     C*PESO DA CML
529000020213    5C     BARPUC        IFGT      0
529100020213     C*
529200020213    6C     BTTPUC        IFEQ      0
529300041013     c                   movel     'S'           WFPC
529400020213     C                   Z-ADD     BARPUC        BTTPUC
529500020213     C                   MOVEL     'S'           WAGGF             1
529600020213     C                   MOVEL     'T'           BTTFPC
529700020213    6C                   END
529800020213     C*
529900020213    5C                   END
530000020213     C*
530100020213    5C     BTTDET        IFEQ      0
530200020213     C                   MOVEL     'S'           WAGGF             1
530300020213     C                   MOVEL     BARDFV        BTTDET
530400020213     C                   MOVEL     BARNPS        BTTPET
530500020213     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
530600020213     C*  SOLO SE NON E' PISTOLA FASULLA
530700160624     C**                 MOVEL     'TT'          WCCH
530800160624     C**   OPS(Y)        IFLT      040
530900160624     C**                 EXSR      CRTA60
531000160624     C**                 CLEAR                   WCCH
531100160624     C**                 ENDIF
531200020213     C*
531300020213   X5C                   ELSE
531400020213     C*
531500020213    6C   05BTTPET        IFEQ      BARNPS
531600020213     C                   MOVEL     'S'           WAGGF             1
531700020213     C                   MOVEL     BARDFV        BTTDET
531800020213    6C                   END
531900020213     C*
532000020213    6C  N05BTTPET        IFNE      BARNPS
532100020213     C     RPS(Y)        ANDEQ     'L'
532200020213     C                   MOVEL     BTTPET        ALF2
532300020213     C*
532400020213     C* CERCO RAGGRUPPAMENTO PISTOLA
532500020213     C                   EXSR      RICRAG
532600020213     C*
532700020213    7C     RPS(Z)        IFEQ      'M'
532800020213     C     RPS(Z)        OREQ      'A'
532900020213     C                   MOVEL     BARDFV        BTTDET
533000020213     C                   MOVEL     'S'           WAGGF             1
533100020213     C                   MOVEL     BARNPS        BTTPET
533200160624     c
533300020213     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
533400160624     c* Anomalia 61 eliminara
533500160624     C**                 MOVEL     'TT'          WCCH
533600160624     C**                 EXSR      CRTA60
533700160624     C**                 CLEAR                   WCCH
533800020213    7C                   END
533900020213    6C                   END
534000020213     C*
534100020213    5C                   END
534200020213     C*
534300020213     C                   UPDATE    FNBTT000
534400020213     C*
534500020213     C* SE INVENTARIO O IN CONSEGNA ANOMALIA: FUORI POSTO DEL TRANSITO
534600020213    5C  N05BARNPG        IFEQ      3
534700020213     C     BARSPG        ANDNE     'P'
534800020213     C     BARNPG        OREQ      4
534900020213     C     BARNPG        OREQ      8
535000020213     C                   Z-ADD     125           COMCAA
535100020213     C                   EXSR      CRTANM
535200020213    5C                   END
535300040430     c* Se il collo è in transito, posso chiudere i fuori posto in partenza
535400040430     C                   Z-ADD     120           COMCAA
535500040430     C                   EXSR      chiuan
535600110103     c*
535700040422     c*Chiudo eventuale anomalia di disguido presente sul collo, se
535800040422     c*  generate da un 2 livello dell'area di partenza
535900040503     C* SOLO SE SPUNTA DI PISTOLA REALE
536000040503     c                   if        fsl(y)<>'F'
536100040428     c                   eval      comcaa=200
536200040422     C*
536300040503     C     Kanm          chain(N)  FNANM000
536400040428     c                   if        %found(fnanm02l)
536500040428     C                   CLEAR                   DSLV55
536600040428     C                   MOVEL     'P'           D55TPT
536700040428     C                   Z-ADD     anmfle        D55LIN
536800040428     C                   Z-ADD     bardfv        D55DRF
536900040428     C                   EXSR      FNLV55
537000040428     c                   if        bpes=d55tfp
537100040428     c                   exsr      chiuan
537200040428     c                   endif
537300040428     c                   endif
537400040503     c                   endif
537500041012     c*
537600041012     c* Aggiorno anche la bolla arrivo con i dati del transito
537700041012     c                   EXSR      AGGARRIVO
537800110103     c*
537900020213     C                   ENDSR
538000020201     C**************************************************************************
538100020201     C* CONTROLLO COSA POSSO AGGIORNARE IN BASE A CHI SONO
538200020201     C**************************************************************************
538300020201     C     CTRAGG        BEGSR
538400020204     C                   CLEAR                   WAGPAR
538500020204     C                   CLEAR                   WAGARR
538600150622     C                   CLEAR                   Wbsptp
538700150622     C                   CLEAR                   Wbspspu
538800020213     C**
538900020213     C** CERCO TERMINAL DELLA LINEA DI ARRIVO
539000020213     C                   EXSR      TERARR
539100050603     c                   eval      Usalnatfa=lnatfa
539200020204     C*
539300070208     c*         Non considero se c'e'bolla transito
539400070208     c     wesbtt        ifne      'S'
539500150622     C* SE NON C'È BOLLA TRANSITO VERIFICO SE COLLI CHE PARTONO DA
539600150817     C*  più filiali se ho trovato la bolla in partenza non partita
539700150817     c*  e con data di entrata = 0
539800170224     c                   if        wesblt='S' and blpdse=0
539900170224     c                   if        Blpdt1=0  or blpft1='N' and blpft2='N'
540000150818     c* se si tratta di una spunta tipica partenza
540100150818     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
540200150818     c                             or barnpg=1
540300150622     c                   exsr      sr_fibsp
540400150622     c                   if        wbsptp<>' '      and
540500150622     c                             %lookup(bpes:sbsp)>0
540600150622     c                   eval      wbspspu='S'
540700150818     C                   MOVEL     'P'           WTIBOL            1
540800150622     c                   endif
540900170224     c                   endif
541000170224
541100150622     c                   endif
541200150818     c                   endif
541300020204     C*
541400020201     C*  1) P.O. EFFETTUA SPUNTA = LNP SPUNTA (DA BOLLA)
541500020204    2C     BPES          IFEQ      LNPB
541600020201     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.PARTENZA DELLA LNP SPUNTA
541700020201     C*     IN BASE ALLA DATA SPEDIZIONE SE TROVATA LA BOLLA
541800020204     C     BPES          OREQ      LNPTFP
541900020204     C                   MOVEL     'S'           WAGPAR            1
542000020204   X2C                   ELSE
542100020204     C*  3) TER.PARTENZA P.O. EFFETTUA SPUNTA (IN BASE ALLA DATA SPED
542200020204     C*     SE TROVATA LA BOLLA) = TER.PARTENZA LNP SPUNTA SEMPRE IN
542300020204     C*     BASE ALLA DATA SPEDIZIONE SE TROVATA BOLLA
542400021125     C                   MOVEL     'P'           WTITER            1
542500020201     C                   EXSR      TERPES
542600020215    3C     D55TFP        IFEQ      LNPTFP
542700020204     C                   MOVEL     'S'           WAGPAR            1
542800020204    3C                   ENDIF
542900020204    2C                   ENDIF
543000020201    1C                   ENDIF
543100020204     C**
543200020204     C** NON AGGIORNO BOLLE ARRIVO SE SPUNTA DELL'AREA PARTENZA,
543300020204     C*   SPUNTA TIPICA PARTENZA E P.O. SPUNTA = LNA SPUNTA O TFA SPU
543400021125     c* controllo solo se spunta tipica partenza: l'arrivo non mi
543500021125     c*  interessa. non so perchè lo avevo preso in considerazione!!
543600150817     c
543700150817     c* Non aggiorno nemmeno se bolla da "trasferire"
543800020204    1C     WAGPAR        IFEQ      'S'
543900150817     c     wbspspu       oreq      'S'
544000150817     c
544100021125     C**   BPES          ifeq      lnpb
544200021125    2C**   BPES          OREQ      BARLNA
544300021125     C**   BPES          OREQ      LNATFA
544400021125     C**   pestfa        OREQ      LNATFA
544500020204     C* PARTENZA NON DEFLUENZA
544600020204    3C     BARNPG        IFEQ      1
544700020204     C     WDEFL         ANDEQ     '0'
544800020204     C* ENTRATA
544900020204     C     BARNPG        OREQ      5
545000020204     C* I.M.P.
545100020204     C     BARNPG        OREQ      3
545200020204     C     BARSPG        ANDEQ     'P'
545300020204     C* I.M.I.
545400020204     C     BARNPG        OREQ      3
545500020218     C     BARSPG        ANDEQ     'I'
545600020204     C                   GOTO      NOAGAR
545700020204    3C                   ENDIF
545800021125    2C**                 ENDIF
545900020204    1C                   ENDIF
546000020204     C**
546100021022     c     wesbtt        ifne      'S'
546200020204     C* AGGIORNO BOLLE ARRIVO SE :
546300020204     C*  1) P.O. EFFETTUA SPUNTA = LNA SPUNTA
546400020205    1C                   SELECT
546500020204     C     BPES          WHENEQ    BARLNA
546600020204     C                   MOVEL     'S'           WAGARR            1
546700020205    2C     WAGPAR        IFEQ      'S'
546800020204     C                   MOVEL     'A'           WWFVC
546900020204     C                   MOVEL     'A'           WSPLOC            1
547000020205    2C                   ENDIF
547100020204     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.ARRIVO   DELLA LNA SPUNTA
547200020204     C*     IN BASE ALLA DATA SPUNTA
547300020204     C     BPES          WHENEQ    LNATFA
547400050603     C     BPES          oreq      bolTFA
547500020204     C                   MOVEL     'S'           WAGARR            1
547600020205    2C     WAGPAR        IFEQ      'S'
547700020204     C                   MOVEL     'A'           WWFVC
547800020204     C                   MOVEL     'T'           WSPLOC            1
547900020205    2C                   ENDIF
548000050603     c* imposto il terminal di arrivo =BPES
548100050603     c                   eval      Usalnatfa=BPES
548200020204   X1C                   OTHER
548300020204     C*  3) TER.ARRIVO P.O. EFFETTUA SPUNTA
548400020204     C*     = TER.ARRIVO LNA SPUNTA IN BASE ALLA DATA SPUNTA
548500020204    2C     PESTFA        IFEQ      LNATFA
548600050603    2C     PESTFA        oreq      bolTFA
548700020204     C                   MOVEL     'S'           WAGARR            1
548800020204     C                   MOVEL     'D'           WSPLOC
548900020204     C                   MOVEL     'A'           WWFVC
549000050603     c* imposto il terminal di arrivo =ter arrivo di BPES
549100050603     c                   eval      Usalnatfa=pestfa
549200110117   x2c                   else
549300110117     c
549400110117     c*  4) Se nessuna è vera ma chi spara è la capostipite della lna-£6
549500110117     c*     anche se è una incongruenza la considero valida ai fini dell'arrivo
549600110117     C* CARICO LA L6 DELLA FILIALE GESTIONE
549700110117    3C     BPES          IFNE      SAVPES
549800110117     C                   MOVEL     BPES          SAVPES
549900110117     C                   EXSR      CARL6
550000110117    3C                   ENDIF
550100110117    c
550200110117     C     BARLNA        LOOKUP    L6                                     34
550300110117    3c                   if        *in34
550400110117     C                   MOVEL     'S'           WAGARR            1
550500110117    4C     WAGPAR        IFEQ      'S'
550600110117     C                   MOVEL     'A'           WWFVC
550700110117     C                   MOVEL     'A'           WSPLOC            1
550800110117    4C                   ENDIF
550900110117    3C                   ENDIF
551000110117     c
551100020205    2C                   ENDIF
551200020205    1C                   ENDSL
551300021022     c*
551400021022    2C                   endif
551500020204     C**
551600070208     C     NOAGAR        ENDSR
551700150622     C**************************************************************************
551800150622     C*  vedo se lnp o ksc confermabile da più linee
551900150622     C**************************************************************************
552000150622     C     sr_fibsp      BEGSR
552100150622     c                   eval      WBspTp=*blanks
552200150622    2c                   if        blpccm>0
552300150622     c                   eval      w_clibo=blpccm
552400150622     c                   else
552500150622     c                   eval      w_clibo=blpksc
552600150622    2c                   endif
552700150622     c     w_clibo       chain     fibsp02l
552800150622     c                   if        %found(fibsp02l) and bspfpa='S'
552900150622     c                   eval      wBspTp='K'
553000150622     c****               if        not %found(fibsp02l) or bspfpa<>'S'
553100150622     c                   else
553200150817     c     LNPB          chain     fibsp01l
553300150622     c                   if        %found(fibsp01l) and bspfpa='S'
553400150622     c                   eval      wBspTp='L'
553500150622     c                   endif
553600150622     c                   endif
553700150622     c                   endsr
553800020201     C**************************************************************************
553900020201     C* TERMINAL DI PARTENZA E ARRIVO DEL P.O. EFFETTUA SPUNTA
554000020201     C**************************************************************************
554100020205     C     TERPES        BEGSR
554200020201     C* IMPOSTO TERMINAL PARTENZA E ARRIVO
554300020201     C                   CLEAR                   DSLV55
554400020205     C                   MOVEL     WTITER        D55TPT
554500020205     C                   MOVEL     LNPB          D55LNP
554600020205     C                   MOVEL     WDTRIF        D55DRF
554700020205     C                   MOVE      BPES          D55LIN
554800020205     C                   EXSR      FNLV55
554900020205     C                   ENDSR
555000020205     C**************************************************************************
555100020205     C* CALL A PGM FNLV55R
555200020205     C**************************************************************************
555300020205     C     FNLV55        BEGSR
555400020201     C                   CALL      'FNLV55R'
555500020201     C                   PARM                    DSLV55
555600020201     C**
555700020205     C     D55ERR        IFNE      ' '
555800020205     C                   MOVE      D55LIN        D55TFA
555900020205     C                   MOVE      D55LIN        D55TFP
556000020201     C                   END
556100020201     C                   ENDSR
556200041012     C**************************************************************************
556300041012     C* AGGIORNO CODICE ANOMALIA
556400041012     C**************************************************************************
556500041012     c     AGGCAN        BEGSR
556600041012    3C     BARCAN        IFNE      ' '
556700041012     C     BARCAN        ANDNE     'E'
556800070508     c     barcan        andne     artcan
556900041012     C**
557000070508     c* Aggiorno la testata e quindi scrivo AGB solo se artcan
557100070508     c*  è vuoto prima di questo aggiornamento
557200070508     c                   if        artcan=' '
557300041012     C                   MOVEL     'S'           WAGGF             1
557400070508     c                   endif
557500070508     C                   MOVEL     BARCAN        ARTCAN
557600041012     c                   endif
557700041012     c                   endsr
557800041012     C**************************************************************************
557900041012     C* AGGIORNO volume cml
558000041012     C**************************************************************************
558100041105     C     aGGVOL        BEGSR
558200041012    3C                   SELECT
558300041012    3C                   when      BARVUC = *zeros
558400041012      * NON SOSTITUISCO IL VOLUME ARRIVO SE LNA FEDEX
558500041012    3C                   when      ARTFVC = 'A' and ARTVUC > *zeros
558600041012    3C                                          and NTWBAR = 'FED'
558700041012      * IL VOLUME ARRIVO O TRANSITO LI SOSTITUISCO SE QUELLO DELLA
558800041012      * SPUNTA E' RILEVATO DALLA PARTENZA
558900041012    3C                   when       ARTVUC = *zeros  or
559000041012    3C                             (ARTVUC > *zeros and ARTFVC <> 'P'
559100041012     C                                              and WWFVC  =  'P')   or
559200041012    3C                             (ARTVUC > *zeros and ARTFVC =  'A'
559300041012     C                                              and WWFVC  =  'T')
559400041012     c                   movel     'S'           wfvc
559500041012     C                   MOVEL     'S'           WAGGF             1
559600041012     C                   Z-ADD     BARVUC        ARTVUC
559700041012     C                   MOVEL     WWFVC         ARTFVC
559701180124     c                   eval      wagvolart_a='S'
559800041012    3c                   ENDSL
559900041012     c                   ENDSR
560000041012     C**************************************************************************
560100041012     C* AGGIORNO Peso   cml
560200041012     C**************************************************************************
560300041012     c     AGGPES        BEGSR
560400041012    3C     BARPUC        IFGT      0
560500041012    4C     ARTPUC        IFEQ      0
560600041012     C* IL PESO ARRIVO O TRANSITO LI SOSTITUISCO SE QUELLO DELLA
560700041012     C* SPUNTA E' RILEVATO DALLA PARTENZA
560800041012     C     ARTPUC        ORNE      0
560900041012     C     WWFVC         ANDEQ     'P'
561000041012     C     ARTFPC        ANDNE     'P'
561100041012     C     ARTPUC        ORNE      0
561200041012     C     WWFVC         ANDEQ     'T'
561300041012     C     ARTFPC        ANDEQ     'A'
561400041012     c                   movel     'S'           wfpc
561500041012     C                   MOVEL     'S'           WAGGF             1
561600041012     C                   Z-ADD     BARPUC        ARTPUC
561700041012     C                   MOVEL     WWFVC         ARTFPC
561701180124     c                   eval      wagpesart_a='S'
561800041012    4C                   END
561900041012     C*
562000041012    3C                   END
562100041012     c                   ENDSR
562200091209     C**************************************************************************
562300091209     c*    Aggiorno fiar5 o lo scrivo se inesistente
562400091209     C**************************************************************************
562500091209     c     AggioFIAR5    BEGSR
562600091209     c
562700091209     c                   if        %found(fiar501l)
562800101129     c                   movel     datReale      §ar5arrdt
562900101129     c                   movel     oraReale      §ar5arrhm
563000091209     c                   movel(p)  dar5gen       ar5uni
563100091209      * se devo trasmettere alla sede sfleggo per la trasmissione
563200091209     c                   If        §Ar5Trse = 'S'
563300091209     c                   Clear                   Ar5Ft2
563400091209     c                   EndIf
563500091209     c                   update    fiar5000
563600091209      * Scrivo Fiar5
563700091209   x2c                   Else
563800091209     c                   Clear                   Fiar5000
563900091209     c                   Eval      Ar5Aas = ArtAas
564000091209     c                   Eval      Ar5Lnp = ArtLnp
564100091209     c                   Eval      Ar5Nrs = ArtNrs
564200091209     c                   Eval      Ar5Nsp = ArtNsp
564300091209     c                   Eval      Ar5Trd = 'GEN'
564400091209     c                   Move      WDATE1        Ar5Dac
564500091209     c                   time                    Ar5Orc
564600091209     c                   Clear                   Dar5Gen
564700101129     c                   movel     datReale      §ar5arrdt
564800101129     c                   movel     oraReale      §ar5arrhm
564900091209     c                   Movel(p)  DAr5Gen       Ar5Uni
565000091209     c                   Write     Fiar5000
565100091209    2c                   EndIf
565200091209     c                   ENDSR
565300041012     C**************************************************************************
565400041012     C* AGGIORNO anche la bolla di arrivo
565500041012     C**************************************************************************
565600041012     c     AGGARRIVO     BEGSR
565700041012     c* Aggiorno anche bolla  arrivi con dati del transito
565800130228     c                   clear                   aggioart          1
565900041012     C     KBAR          CHAIN     FNART27L
566000130228    1c                   if        %found(fnart27l)
566100041012     C                   MOVEL     'A'           ARTTBO
566200130228     c                   eval      Aggioart='S'
566300041012     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
566400041012     C* NE PRECEDENTE
566500041012     C     ARTSPE        IFNE      SAVSPE
566600041012     C     WAGGF         IFEQ      'S'
566700041012     C                   EXSR      WRTAGB
566800041012     C                   CLEAR                   WAGGF
566900041012     C                   ENDIF
567000041012     C                   MOVEL     ARTSPE        SAVSPE
567100041012     C                   ENDIF
567200110103     c*
567300041012     c* Codice anomalia: tengo sempre l'ultima anomalia
567400041012     C                   EXSR      AGGCAN
567500041012     C
567600041012     C* AGGIORNO DATA ENTRATA AL TRANSITO
567700041012     C                   Z-ADD     barDFV        ARTDET
567800041012     C                   MOVEL     barNPS        ARTPET
567900041012     C                   MOVEL     'S'           WAGGF             1
568000041012     C     barFGS        IFNE      ARTFLP
568100041012     C* FILIALE IN GESTIONE <> FILIALE TRANSITO
568200041012     C                   MOVEL     barFGS        ARTFLP
568300041012     C                   MOVEL     *ZEROS        ARTDUT
568400041012     C                   MOVEL     *ZEROS        ARTPUT
568500041012     C                   END
568600041012     c* peso e volume
568700041012     C     WWFVC         IFEQ      *BLANKS
568800041012     C                   MOVEL     'T'           WWFVC
568900041012     C                   END
569000041012     c                   EXSR      AGGVOL
569100041012     c                   EXSR      AGGpes
569200041012     c                   update    fnart000
569300130228     c
569400041013     c* Se trovata bolla, eseguo solo per disguido la CRTA60
569500160624     c* anomalie 60 e 61 eliminate
569600050322     c                   eval      savesart=wesart
569700041013     c                   movel     'S'           wesart
569800160624     c**                 if        wtibol='D'
569900160624     c**                 eval      wsavflg=dls45FLG
570000160624     c**                 eval      dls45FLG='6'
570100160624     c**                 exsr      CRTA60
570200160624     c**                 eval      dls45FLG=wsavflg
570300160624     c**                 endif
570400130228     c
570500130228    1c                   endif
570600041013     c* Chiudo una eventuale anomalia 15 aperta controllando le date anomali
570700041013     c*  e spunta
570800041013     c                   z-add     15            comcaa
570900041013     c                   eval      wcontrdfv='S'
571000041013     c                   eval      wstess='S'
571100041013     c                   eval      wchiu='S'
571200041013     c                   exsr      chiuan
571300130228     c
571400130228    1c                   if        Aggioart='S'
571500130228     c                   eval      wesart=savesart
571600130228     c                   endif
571700041013     c*
571800041012     c                   ENDSR
571900070208     C**************************************************************************
572000070208     C* Routine iniziale
572100070208     C**************************************************************************
572200070208     c     *inzsr        BEGSR
572300070208     C     KBAR          KLIST
572400070208     C                   KFLD                    BARLNP
572500070208     C                   KFLD                    BARLNA
572600070208     C                   KFLD                    BARNRS
572700070208     C                   KFLD                    BARNSC
572800170505     C     Kbolla        KLIST
572900170505     C                   KFLD                    comaas
573000170505     C                   KFLD                    comlnp
573100170505     C                   KFLD                    BARNRS
573200170505     C                   KFLD                    comnsp
573300070208     C     KBAR37        KLIST
573400070208     C                   KFLD                    bpes
573500070208     C                   KFLD                    BARLNP
573600070208     C                   KFLD                    BARLNA
573700070208     C                   KFLD                    BARNRS
573800070208     C                   KFLD                    BARNSC
573900160721     C     KBAR_t        KLIST
574000160721     C                   KFLD                    savflp
574100160721     C                   KFLD                    BARLNP
574200160721     C                   KFLD                    BARLNA
574300160721     C                   KFLD                    BARNRS
574400160721     C                   KFLD                    BARNSC
574500070208     C     KBAR1         KLIST
574600070208     C                   KFLD                    BARNPG
574700070208     C                   KFLD                    BARNfv
574800070208     C                   KFLD                    BARfgs
574900070208     C                   KFLD                    BARLNP
575000070208     C                   KFLD                    BARLNA
575100070208     C                   KFLD                    BARNRS
575200070208     C                   KFLD                    BARNSC
575300140616     C     KBAR_qd4      KLIST
575400140616     C                   KFLD                    kdel_npg
575500140616     C                   KFLD                    BARLNP
575600140616     C                   KFLD                    BARLNA
575700140616     C                   KFLD                    BARNRS
575800140616     C                   KFLD                    BARNSC
575900140616     C                   KFLD                    BARfgs
576000140616     C                   KFLD                    qd_dfs
576100140616     C                   KFLD                    qd_hms
576200070208     C     KBAR2         KLIST
576300070208     C                   KFLD                    BARNPG
576400070208     C                   KFLD                    BARLNP
576500070208     C                   KFLD                    BARLNA
576600070208     C                   KFLD                    BARNRS
576700070208     C                   KFLD                    BARNSC
576800070208     C                   KFLD                    BARfgs
576900140613     C     KBAR2_d       KLIST
577000140613     C                   KFLD                    kdel_npg
577100140613     C                   KFLD                    BARLNP
577200140613     C                   KFLD                    BARLNA
577300140613     C                   KFLD                    BARNRS
577400140613     C                   KFLD                    BARNSC
577500140613     C                   KFLD                    BARfgs
577600140616     C                   KFLD                    wdfs
577700070208     C     KBAR3         KLIST
577800070208     C                   KFLD                    WNPG
577900070208     C                   KFLD                    BARLNP
578000070208     C                   KFLD                    BARLNA
578100070208     C                   KFLD                    BARNRS
578200070208     C                   KFLD                    BARNSC
578300070208     C     KBRV7         KLIST
578400070208     C                   KFLD                    BARLNP
578500070208     C                   KFLD                    BARLNA
578600070208     C                   KFLD                    BARNRS
578700070208     C                   KFLD                    BARNSC
578800070208     C     KARB          KLIST
578900070208     C                   KFLD                    ARTAAS
579000070208     C                   KFLD                    ARTLNP
579100070208     C                   KFLD                    ARTNRS
579200070208     C                   KFLD                    ARTNSP
579300070208     C     KGCA          KLIST
579400070208     C                   KFLD                    ARTAAS
579500070208     C                   KFLD                    ARTLNP
579600070208     C                   KFLD                    ARTNRS
579700070208     C                   KFLD                    ARTNSP
579800070208     C                   KFLD                    KFRG
579900070208     C     KSAV          KLIST
580000070208     C                   KFLD                    SAVTBO
580100070208     C                   KFLD                    SAVAAS
580200070208     C                   KFLD                    SAVLNP
580300070208     C                   KFLD                    SAVNRS
580400070208     C                   KFLD                    SAVNSP
580500070208     C                   KFLD                    KAGB
580600070208     C     KEAGB         KLIST
580700070208     C                   KFLD                    WTBO
580800070208     C                   KFLD                    WDAAS
580900070208     C                   KFLD                    WDLNP
581000070208     C                   KFLD                    WDNRS
581100070208     C                   KFLD                    WDNSP
581200070208     C                   KFLD                    KAGB
581300070208     C     KBLT          KLIST
581400070208     C                   KFLD                    BLTAAS
581500070208     C                   KFLD                    BLTLNP
581600070208     C                   KFLD                    BLTNRS
581700070208     C                   KFLD                    BLTNSP
581800070208     C     KBTT          KLIST
581900070208     C                   KFLD                    BTTflp
582000070208     C                   KFLD                    BTTAAS
582100070208     C                   KFLD                    BTTLNP
582200070208     C                   KFLD                    BTTNRS
582300070208     C                   KFLD                    BTTNSP
582400070208     C     KANM          KLIST
582500070208     C                   KFLD                    BARLNP
582600070208     C                   KFLD                    BARLNA
582700070208     C                   KFLD                    BARNRS
582800070208     C                   KFLD                    BARNSC
582900070208     C                   KFLD                    COMCAA            3 0
583000070208     C     KEVS          KLIST
583100070208     C                   KFLD                    BARLNP
583200070208     C                   KFLD                    BARLNA
583300070208     C                   KFLD                    BARNRS
583400070208     C                   KFLD                    BARNSC
583500070208     C                   KFLD                    KCEV
583600070208     C     KFVV1         KLIST
583700070208     C                   KFLD                    WNPG
583800070208     C                   KFLD                    WNFV
583900070208     C                   KFLD                    WFGS
584000070208     C     KFVV          KLIST
584100070208     C                   KFLD                    WNPG
584200070208     C                   KFLD                    wdata
584300070208     C                   KFLD                    KTFA2
584400070208     C     KFVA1         KLIST
584500070208     C                   KFLD                    WFGS
584600070208     C                   KFLD                    WNFV
584700070208     C     KFGV1         KLIST
584800070208     C                   KFLD                    WNFV
584900070208     C                   KFLD                    tfpfgs
585000070208     c     KFGV2         KLIST
585100070208     C                   KFLD                    lnptfp
585200070208     C                   KFLD                    bardfv
585300070208     C     KFVA4         KLIST
585400070208     C                   KFLD                    KNPG4
585500070208     C                   KFLD                    KNFV4
585600070208     C                   KFLD                    KFGS4
585700070208     C     KFWA          KLIST
585800070208     C                   KFLD                    FVALNP
585900070208     C                   KFLD                    FVANFV
586000070208     C                   KFLD                    FVALAI
586100070208     C     KTAB          KLIST
586200070208     C                   KFLD                    CODUT
586300070208     C                   KFLD                    COD
586400070208     C                   KFLD                    KEY
586500070208     C     KTAB2         KLIST
586600070208     C                   KFLD                    CODUT
586700070208     C                   KFLD                    COD
586800130228     C     KTBE2         KLIST
586900130228     C                   KFLD                    KCOD
587000130228     C                   KFLD                    KKE1
587100130228     C                   KFLD                    KKE2
587200070208     C*
587300070208     C     KLBL          KLIST
587400070208     C                   KFLD                    LBLAAN
587500070208     C                   KFLD                    LBLLPN
587600070208     C                   KFLD                    LBLNRN
587700070208     C                   KFLD                    LBLNSN
587800070208     C     KDCT          KLIST
587900070208     C                   KFLD                    DCDAAC
588000070208     C                   KFLD                    DCDFIL
588100070208     C                   KFLD                    DCDNCA
588200070208     C     KBRVE2        KLIST
588300070208     C                   KFLD                    BRVNPG
588400070208     C                   KFLD                    BRVLNP
588500070208     C                   KFLD                    BRVLNA
588600070208     C                   KFLD                    BRVNRS
588700070208     C                   KFLD                    BRVNSC
588800070710     C     KBRVE1        KLIST
588900070710     C                   KFLD                    BRVNPG
589000070710     C                   KFLD                    BRVNfv
589100070710     C                   KFLD                    BRVfgs
589200070710     C                   KFLD                    BRVLNP
589300070710     C                   KFLD                    BRVLNA
589400070710     C                   KFLD                    BRVNRS
589500070710     C                   KFLD                    BRVNSC
589600070208     C     Kar5          KLIST
589700070208     C                   KFLD                    anmaas
589800070208     C                   KFLD                    anmLNp
589900070208     C                   KFLD                    anmNRS
590000070208     C                   KFLD                    anmnsp
590100070208     C                   KFLD                    Ktrd
590200091211     C     Kar52         KLIST
590300091211     C                   KFLD                    artaas
590400091211     C                   KFLD                    artLNp
590500091211     C                   KFLD                    artNRS
590600091211     C                   KFLD                    artnsp
590700091211     C                   KFLD                    Ktrd
590800180115     C     Kars          KLIST
590900180115     C                   KFLD                    artaas
591000180115     C                   KFLD                    artLNp
591100180115     C                   KFLD                    artNRS
591200180115     C                   KFLD                    artnsp
591300180115     C                   KFLD                    Karstr
591400070208     C*
591500070208     C     *LIKE         DEFINE    FVAdfv        Wpul050
591600070208     C     *LIKE         DEFINE    FVALNP        WFVALNP
591700070208     C     *LIKE         DEFINE    FVANFV        WFVANFV
591800070208     C     *LIKE         DEFINE    FVADFV        WFVADFV
591900070208     C     *LIKE         DEFINE    bardfv        Wdata
592000070208     C     *LIKE         DEFINE    wfgs          WWWfgs
592100070208     C     *LIKE         DEFINE    wfgs          tfpfgs
592200070208     C     *LIKE         DEFINE    wfgs          savfgs
592300070208     c                   clear                   savfgs
592400070208     C     *LIKE         DEFINE    FVVFLE        WFLE
592500070208     C     *LIKE         DEFINE    ARTFLP        SAVFLP
592600070208     C     *LIKE         DEFINE    BARDFV        DSPB
592700070208     C     *LIKE         DEFINE    BARDFV        COMDFV
592800070208     C     *LIKE         DEFINE    BARDFV        SVVDFV
592900070208     C     *LIKE         DEFINE    BLPTFP        LNPTFP
5930000702080    C     *LIKE         DEFINE    BARDFV        WDTRIF
593100070208     C     *LIKE         DEFINE    BARDFV        PRIDAT
593200070208     C     *LIKE         DEFINE    BARDFV        SA2DFV
593300070208     C     *LIKE         DEFINE    ANMCCH        WCCH
593400070208     C     *LIKE         DEFINE    GCPFRG        KFRG
593500070208     C                   MOVEL     '0'           KFRG
593600070208     C     *LIKE         DEFINE    AGBAGB        KAGB
593700070208     C     *LIKE         DEFINE    AGBFBS        WFBS
593800070208     C     *LIKE         DEFINE    AGBFVC        WFVC
593900070208     C     *LIKE         DEFINE    AGBFPC        WFPC
594000070208     C     *LIKE         DEFINE    ARBNDC        WABNDC
594100070208     C     *LIKE         DEFINE    ARTDCM        WATDCM
594200070208     C     *LIKE         DEFINE    ARbcca        WABcca
594300070208     C     *LIKE         DEFINE    ARbcca        savcca
594400070208     C     *LIKE         DEFINE    BARLNA        SVVLNA
594500070208     C     *LIKE         DEFINE    BARLNP        SVVLNP
594600070208     C     *LIKE         DEFINE    ARTDCM        WDCM
594700070208     C     *LIKE         DEFINE    ANMCAA        CAACRE
594800070208     C     *LIKE         DEFINE    ANMCAA        CAACIU
594900070208     C     *LIKE         DEFINE    FGVTTR        WTTR
595000140613     C     *LIKE         DEFINE    FGVFCF        WFCF
595100070208     C     *LIKE         DEFINE    EVSCEV        KCEV
595200070208     C     *LIKE         DEFINE    D33MAC        SAVMAC
595300070208     C     *LIKE         DEFINE    BRVFLE        BARFLE
595400070208     C     *LIKE         DEFINE    BRVFGS        SAVPES
595500070208     C     *LIKE         DEFINE    BRVFGS        KFGS4
595600070208     C     *LIKE         DEFINE    TBLCOD        COD
595700070208     C     *LIKE         DEFINE    TBLKEY        KEY
595800070208     C     *LIKE         DEFINE    BARLNA        LNATFA
595900070208     C     *LIKE         DEFINE    BARLNA        Usalnatfa
596000100713     C     *LIKE         DEFINE    BARLNA        WgenLNAtfa
596100070208     C     *LIKE         DEFINE    blptfa        bolTFA
596200070208     C     *LIKE         DEFINE    BARLNA        PESTFA
596300070208     C     *LIKE         DEFINE    BARLNA        PESTFP
596400070208     C     *LIKE         DEFINE    BARLNP        LNPB
596500070208     C     *LIKE         DEFINE    FVVFGS        WFGS
596600070208     C     *LIKE         DEFINE    FVVSPG        WSPG
596700070208     C     *LIKE         DEFINE    FVVNPG        WNPG
596800070208     C     *LIKE         DEFINE    FVVNPG        KNPG4
596900070208     C     *LIKE         DEFINE    FVVNFV        WNFV
597000070208     C     *LIKE         DEFINE    FVVNFV        KNFV4
597100070208     C     *LIKE         DEFINE    FVVFLE        WLIN
597200070208     C     *LIKE         DEFINE    FVVFLE        WLINPR
597300070208     C     *LIKE         DEFINE    FVVSPG        BARSPG
597400140616     C     *LIKE         DEFINE    FVVSPG        brvSPG
597500070208     C     *LIKE         DEFINE    FVVDFV        WDFV
597600070208     C     *LIKE         DEFINE    FVVDFV        SAVDFV
597700070208     C     *LIKE         DEFINE    FVVDFV        BARDFV
597800070208     C     *LIKE         DEFINE    FVVDFV        BRVDFV
597900070208     C     *LIKE         DEFINE    BARKEY        SAVKEY
598000070208     C     *LIKE         DEFINE    BRVKEY        SA1KEY
598100070208     C     *LIKE         DEFINE    ARBLNP        COMLNP
598200070208     C     *LIKE         DEFINE    ARBAAS        COMAAS
598300070208     C     *LIKE         DEFINE    ARBNSP        COMNSP
598400070208     C     *LIKE         DEFINE    ARBTFP        WPTFP
598500070208     C     *LIKE         DEFINE    ANMFL1        WFL1
598600070208     C     *LIKE         DEFINE    D55TFA        KTFA2
598700070208     C     *LIKE         DEFINE    D55TFA        SAVTFA
598800070208     C     *LIKE         DEFINE    ARTFVC        WWFVC
598900070208     C     *LIKE         DEFINE    ARTDAM        SAVDAM
599000070208     C     *LIKE         DEFINE    ARTDAM        WARDAM
599100070208     C     *LIKE         DEFINE    ARTDAM        WSVDAM
599200070208     C     *LIKE         DEFINE    FVADFV        SAVDAO
599300070208     C     *LIKE         DEFINE    FVALNP        WVALNP
599400070208     C     *LIKE         DEFINE    DCTTAD        SAVTAD
599500070208     C                   CLEAR                   SAVTAD
599600070208     C     *LIKE         DEFINE    §OGNTW        NTWBAR
599700070208     C     *LIKE         DEFINE    GI8INV        DATMIN
599800070208     C     *LIKE         DEFINE    BRVATR        SAWATR
599900080317     C     *LIKE         DEFINE    BRVTSU        SAWtsu
600000070208     C*
600100070208     C                   MOVEL     'DT'          KAGB
600200070208     c                   ENDSR
600300140613     C**************************************************************************
600400140613     C* Cerca spunta 4
600500140613     C**************************************************************************
600600140613     c     Cer_cons      BEGSR
600700140616     c* imposto data immiss.spunta della spunta che sto caricando
600800140616     c                   exsr      ImpoDFS
600900140616     c*
601000140616     C     KBAR2_d       setll     FNBRV05L
601100140616     C     KBAR2_d       reade(n)  FNBRV05L
601200140616    1c                   dow       not %eof(fnbrv05l)
601300140613     c* controllo data e ora immissione
601400140613     c                   clear                   brv_imm
601500140613     c                   clear                   brv_sca
601600140613     c                   z-add     brvhms        brv_imm
601700140613     c                   movel     brvdfs        brv_imm
601800140613     c                   z-add     brvhcs        brv_sca
601900140613     c                   movel     brvdcs        brv_sca
602000140613     c                   exsr      sceglidata
602100140613     c                   z-add     a_imm         b_imm
602200140613     c
602300140613     c* a parità di data devo tenere la più recente
602400140613     c                   clear                   brv_imm
602500140613     c                   clear                   brv_sca
602600140613     c                   z-add     barhms        brv_imm
602700140613     c                   movel     wDFS          brv_imm
602800140613     C                   Z-ADD     WDATE1        barDCS
602900140613     C                   time                    barHCS
603000140613     c                   z-add     barhcs        brv_sca
603100140613     c                   movel     bardcs        brv_sca
603200140613     c                   exsr      sceglidata
603300140613     c*
603400140613     c                   select
603500140616     c* quella trovata + recente --> non carico la spunta
603600140613    2c                   when      b_imm > A_IMM  and brvdfs=wdfs
603700140613     c
603800140613     c* Se anche la data foglio è = non carico la spunta ricevuta
603900140613    3C     BRvKEY        IFNE      SA1KEY
604000140613     C                   MOVEL     BRvKEY        SA1KEY
604100140613     C                   Z-ADD     BRvNPG        WNPG
604200140613     C                   Z-ADD     BRvNFV        WNFV
604300140613     C                   Z-ADD     BRvFGS        WFGS
604400140613     C                   movel     BRvnps        Wnps
604500140613     C                   EXSR      RILFOG
604600140613     c                   Z-ADD     WDFV          BRVDFV
604700140616     c                   movel     WFCF          BRVFCF
604800140616     c                   movel     Wspg          BRVspg
604900140613    3c                   endif
605000140613     c
605100140616     c                   if        barnpg=3 and
605200140616    3c                             brvdfv=bardfv
605300140616     c                   eval      amb_QD ='N'
605400140616     c                   endif
605500140616     c                   if        barnpg=4 and brvspg='A' and
605600140616    3c                             brvdfv=bardfv
605700140616     c                   eval      amb_QD ='N'
605800140616     c                   endif
605900140613     c
606000140616     c                   if        amb_qd='N'
606100140613     c* memorizzo la spunta nuova negli errori
606200140613     C                   MOVEL     'S'           WBRVE             1
606300140613     C                   EXSR      RIEMPI
606400140613     C                   CLEAR                   WBRVE
606500140613     C                   WRITE     FNBRVE
606600140616     c                   leave
606700140613    3c                   endif
606800140616     c
606900140616     c* quella di carico + recente --> cancello quella già presente
607000140613    2c                   when      A_imm > B_IMM  and brvdfs=wdfs
607100140613     c* Se anche la data foglio è = non carico la spunta ricevuta
607200140613    3C     BRvKEY        IFNE      SA1KEY
607300140613     C                   MOVEL     BRvKEY        SA1KEY
607400140613     C                   Z-ADD     BRvNPG        WNPG
607500140613     C                   Z-ADD     BRvNFV        WNFV
607600140613     C                   Z-ADD     BRvFGS        WFGS
607700140613     C                   movel     BRvnps        Wnps
607800140613     C                   EXSR      RILFOG
607900140613     c                   Z-ADD     WDFV          BRVDFV
608000140616     c                   movel     WFCF          BRVFCF
608100140616     c                   movel     Wspg          BRVspg
608200140613    3c                   endif
608300140613     c
608400140616     c                   if        barnpg=3 and
608500140616    3c                             brvdfv=bardfv and brvfcf=' '
608600140616     c                   eval      amb_QD ='S'
608700140613     c* mi slavo data e ora imm spunta 4 per la delete
608800140616     c                   eval      qd_npg=4
608900140616     c                   eval      qd_dfs=brvdfs
609000140616     c                   eval      qd_hms=brvhms
609100140616     c                   leave
609200140613     c                   endif
609300140616     c                   if        barnpg=4 and  brvspg='A' and
609400140616    3c                             brvdfv=bardfv and brvfcf=' '
609500140616     c                   eval      amb_QD ='S'
609600140616     c* mi slavo data e ora imm spunta 4 per la delete
609700140616     c                   eval      qd_npg=3
609800140616     c                   eval      qd_dfs=brvdfs
609900140616     c                   eval      qd_hms=brvhms
610000140616     c                   eval      qd_nfv=brvnfv
610100140616     c                   leave
610200140616     c                   endif
610300140613     c
610400140613    2c                   endsl
610500140616     c
610600140616     C     KBAR2_d       reade(n)  FNBRV05L
610700140616    1c                   enddo
610800140613     c
610900140613     c                   ENDSR
611000140613     C**************************************************************************
611100140613     C* Scelta data tra immisisone e caicamento
611200140613     C**************************************************************************
611300140613     c     Sceglidata    BEGSR
611400140613     c                   if         brv_imm>brv_sca and
611500140616     c                              %subst(%editc(brv_sca:'X'):1:8)>*zeros
611600140613     c                   eval       A_imm=brv_sca
611700140613     c                   else
611800140613     c                   eval       A_imm=brv_imm
611900140613     c                   endif
612000140613     c                   ENDSR
612100140613     C**************************************************************************
612200140613     C*    impostazione data immissione spunta
612300140613     C**************************************************************************
612400140613     c     ImpoDFS       BEGSR
612500140613     c* Se data immissione > di oggi, imposto la minore tra oggi e data fogl
612600140613     c                   if        BARDFS<=wdate1
612700140613     C                   Z-ADD     BARDFS        wDFS
612800140613     c                   else
612900140613     c                   if        wdate1<=bardfv
613000140613     c                   z-add     wdate1        wDFS
613100140613     c                   else
613200140613     c                   z-add     bardfv        wDFS
613300140613     c                   endif
613400140613     c                   endif
613500140613     c                   ENDSR
613600140616     C**************************************************************************
613700140616     C*    cancellazione spunta meno recente IMA o cons
613800140616     C**************************************************************************
613900140616     c     Canc_QD       BEGsr
614000140616     c                   eval      kdel_npg=qd_npg
614100140616    1c                   if        kdel_npg=4
614200140616     c     kbar_qd4      chain     fnbrv05l                             38
614300140616     c  n38              if        %found(fnbrv05l)
614400140616     C     KBRVE1        SETLL     FNBRVE1L                               33
614500140616    2C     *IN33         IFEQ      *OFF
614600140616     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
614700140616     C                   WRITE     FNBRVE
614800140616    2C                   ENDIF
614900140616     c
615000140616     c                   delete    fnbrv005
615100141029     c                   endif
615200140616     c
615300140616   x1c                   else
615400140616     c     kbar_qd4      setll     fnbrv05l
615500140616     c     kbar_qd4      reade     fnbrv05l
615600140616    2c                   dow       not %eof(fnbrv05l) and brvnfv<>qd_nfv
615700140616     c     kbar_qd4      reade     fnbrv05l
615800140616    2c                   enddo
615900140616    2c                   if        not %eof(fnbrv05l)
616000140616     C     KBRVE1        SETLL     FNBRVE1L                               33
616100140616    3C     *IN33         IFEQ      *OFF
616200140616     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
616300140616     C                   WRITE     FNBRVE
616400140616    3C                   ENDIF
616500140616     c                   delete    fnbrv005
616600140616    2c                   endif
616700140616     c
616800140616    1c                   endif
616900140616     c                   ENDSR
617000150622     C**************************************************************************
617100150622     C*    Aggiornamenti da tabella BSP
617200150622     C**************************************************************************
617300150622     c     DA_BSP        BEGsr
617400150622     c* Scrivo FNAGB per aggiornare la bolla
617500150622     C                   CLEAR                   FNAGB000
617600150622     C                   MOVEL     'P'           AGBTBO
617700150622     C                   MOVEL     'CB'          AGBAGB
617800150622     C                   MOVEL     blpAAS        AGBAAS
617900150622     C                   MOVEL     blpLNP        AGBLNP
618000150622     C                   MOVEL     blpNRS        AGBNRS
618100150622     C                   MOVEL     blpNSP        AGBNSP
618200150622     C                   WRITE     FNAGB000                             99
618300150622     c                   ENDSR
618400070208     C**************************************************************************
618500070208     C* Elaborazioni per chiusura pgm
618600070208     C**************************************************************************
618700070208     c     ELACHIUDI     BEGSR
618800070208     C* ELABORO L'ULTIMO DATO MEMORIZZATO
618900070208     C                   EXSR      WRTAGB
619000070208     c
619100070208     C                   CLEAR                   DSLV55
619200070208     C                   MOVEL     'C'           D55TLA
619300070208     C                   EXSR      FNLV55
619400070208     C* SCRITTURA ULTIMO RECORD IN FNSTA
619500070208     C   02              EXSR      WRTSTA
619600070208     C**
619700070208     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
619800070208     c                   clear                   fidn12ds
619900070208     c                   eval      i12tla = 'C'
620000070208     c                   call      'FIDN12R'
620100070208     c                   parm                    fidn12ds
620200161007     c
620300161007     c                   clear                   fnlgi1ds
620400161007     c                   eval      ilgi1tla = 'C'
620500161007     c                   movel     fnlgi1ds      kpjbu
620600161007     c                   call      'FNLGI1R'
620700161007     c                   parm                    kpjba
620800070208     c                   ENDSR
620900110103
621000110103      *---------------------------------------------------------------*
621100110103      *?Reperimento dati del job (Utente/Operativi)                  ?*
621200110103      *---------------------------------------------------------------*
621300110103     c     sr_DatiJob    BEGSR
621400110103      *
621500110103     c     *dtaara       define    §azute        AZUTEds
621600110103     c     *dtaara       define    §datiute      dDatiUte
621700110103      *
621800110103     c                   in(E)     *dtaara
621900110103     c                   IF        %Error or RSUT = *blanks
622000110103     c                   clear                   TIBS34ds
622100110103     c                   call      'TIBS34R'
622200110103     c                   parm                    TIBS34ds
622300110103     c                   in        *dtaara
622400110103     c                   ENDIF
622500110103      *
622600110103     c                   ENDSR
622601180124      *---------------------------------------------------------------*
622602180124      *?Aggiunta segnalazione di incompatibile                       ?*
622603180124      *---------------------------------------------------------------*
622604180124     c     sr_addinc     BEGSR
622605180125     c* se presente spunta vdl non si tratta di incompatibile
622606180125          chain(N) (barlnp:barlna:barnrs:barnsc) fiart27l;
622607180125          if %found(fiart27l);
622608180125             dartflo=U_artflo ;
622609180125           if §flotap<>'V' ;
622610180124             setll (barlnp:barlna:barnrs:barnsc:'J') fiars01l  ;
622611180124             if not %equal(fiars01l);
622612180124                clear fiars000;
622613180124                ARSFLS=barlnp;
622614180124                ARSLNA=barlna;
622615180124                ARSNRS=barnrs;
622616180124                ARSNSC=barnsc;
622617180124                ARSTRC='J'   ;
622618180124
622619180124                clear darsj;
622620180124                §ARSJPEST=bpes;
622621180124                §ARSJNPGT=barnpg;
622622180124                §ARSJDFVT=bardfv;
622623180126                §ARSJNPST=%editc(barnps:'X');
622624180125                select;
622625180129                when barspg='J';
622626180125                 §ARSJTIPO='J' ;
622627180125                other;
622628180125                 §ARSJTIPO='S' ;
622629180125                endsl;
622630180124                ARSNOT=darsj;
622631180124
622632180124                ARSDUV=wdate1     ;
622633180124                write fiars000   ;
622634180124         // Scrivo AGB per aggiornare bolla in sede
622635180124     C                   CLEAR                   FNAGB000
622636180124     C                   MOVEL     'A'           AGBTBO
622637180124     C                   MOVEL     'JA'          AGBAGB
622638180124     C                   MOVEL     artAAS        AGBAAS
622639180124     C                   MOVEL     artLNP        AGBLNP
622640180124     C                   MOVEL     artNRS        AGBNRS
622641180124     C                   MOVEL     artNSP        AGBNSP
622642180124     C                   WRITE     FNAGB000                             99
622643180124             endif;
622644180125          endif;
622645180125         endif;
622646180124     c                   ENDSR
622647180124      *---------------------------------------------------------------*
622648180124      *?Cancellazione segnalazione di incompatibile a fronte di VDL  ?*
622649180124      *---------------------------------------------------------------*
622650180124     c     sr_anninc     BEGSR
622651180124      *
622652180124           chain (barlnp:barlna:barnrs:barnsc:'J') fiars01l  ;
622653180124           if %found(fiars01l);
622654180124              delete fiars000;
622655180124         // Scrivo AGB per aggiornare bolla in sede
622656180124     C                   CLEAR                   FNAGB000
622657180124     C                   MOVEL     'A'           AGBTBO
622658180124     C                   MOVEL     'JD'          AGBAGB
622659180124     C                   MOVEL     artAAS        AGBAAS
622660180124     C                   MOVEL     artLNP        AGBLNP
622661180124     C                   MOVEL     artNRS        AGBNRS
622662180124     C                   MOVEL     artNSP        AGBNSP
622663180124     C                   WRITE     FNAGB000                             99
622664180124           endif;
622665180124      *
622666180124     c                   ENDSR
622667180124      *---------------------------------------------------------------*
622668180124      * Aggiornamento Volume, peso vdl e tipo apparato su FIART      ?*
622669180124      *---------------------------------------------------------------*
622670180124     c     updFiart      BEGSR
622671180124     C     KBAR          CHAIN     FiART27L                           3434
622672180124    5c                   if        not *in34
622673180124    6c                   if        wagpesart_a = 'S'
622674180126     c                   eval      U_artpuc=w_puc
622675180126     c                   eval      U_artfpc=w_fpc
622676180124    6c                   endif
622677180124    6c                   if        wagvolart_a='S'
622678180126     c                   eval      U_artvuc=w_vuc
622679180126     c                   eval      U_artfvc=w_fvc
622680180124    6c                   endif
622681180124     c                   eval      dartflo=U_artflo
622682180124     c* se TAP fiart vuoro --> aggiorno sempre col tap della spunta
622683180124    6c                   if        §flotap=*blanks
622684180124     c                   eval      §flotap=bartap
622685180124   x6c                   else
622686180124     c* se TAP fiart pieno --> aggiorno solo se tap della spunta "V"
622687180124    7c                   if        bartap='V'
622688180124     c                   eval      §flotap=bartap
622689180124    7c                   endif
622690180124    6c                   endif
622691180124     c                   eval      U_artflo=dartflo
622692180124     c                   update    fiart000
622693180124    5c                   endif
622694180125     c* se passata spunta VDL devo annullare eventuale segnalazione di incompati
622695180125     c* bile
622696180125     c                   if        §flotap='V'
622697180125     c                   exsr      sr_anninc
622698180125     c                   endif
622699180124     c                   endsr
622700040601**         CM5
622800040601DLYJOB DLY(5)
622900980928**         MSG
623000070906CARICA SPUNTE:Fogli Inesitenti (FGS NPG NFV-NPS) FNLS45R
623100070906XXX X XXXXX-xx;  XXX X XXXXX-xx;  XXX X XXXXX-xx;  XXX X XXXXX-xx;
623200070906           -  ;             -  ;             -  ;             -  .
