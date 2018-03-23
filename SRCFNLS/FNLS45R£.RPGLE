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
005700180124     F* DETTAGLIO COLLI UNICO -------------
005800180124     FFiART27L  UF   E           K DISK    prefix(U_)
005900911014     D*
006000960523     D L6              S              3  0 DIM(30)
006100961212     D*
006200130614     D S5F             S              8    DIM(9000)
006300130614     D F5F             S             12    DIM(9000)
006400020218     D WPO             S              3    DIM(4)
006500020204     D CAN             S              1    DIM(200)
006600961212     D*
006700921229     D NPS             S              2  0 DIM(101)
006800921229     D RPS             S              1    DIM(101)
006900000210     D TPS             S              1    DIM(101)
007000000316     D OPS             S              3  0 DIM(101)
007100000316     D INV             S              1    DIM(101)
007200040503     D FSL             S              1    DIM(101)
007300961212     D*
007400941214     D CAT             S              1  0 DIM(20)
007500941214     D NST             S              1    DIM(20)
007600950927     D*
007700961122     D TTR             S              1    DIM(10)
007800961122     D*
007900950927     D PG2             S              5  0 DIM(20)
008000950927     D*
008100000505     D FFS             S              3    DIM(301)
008200970114     D*
008300040601     D CM5             S              1    DIM(15) CTDATA PERRCD(15)
008400980928     D*
008500111018     D** ERC             S              1  0 DIM(10)
008600111018     D** ERF             S              5    DIM(10)
008700111018     D** ERE             S              3  0 DIM(10)
008800111018     D** ERp             S              2    DIM(10)
008900981105     D K70             S              1    DIM(70)
009000981105     D MSG             S             70    DIM(3) CTDATA PERRCD(1)
009100001109     D ACG             S              3  0 DIM(50)
009200130228     d* tfp abilitati a far partire colli di altro tfp
009300130228     D xcotfp          S              3    DIM(85)
009301180215     D*lnaf            S              3  0 DIM(5) CTDATA PERRCD(5)
009400000516     D*-----------------------------
009500930701     D* DS PER CONFRONTO SPUNTE: BAR
009600930701     D*-----------------------------
009700930701     D                 DS
009800960524     D  BARCAN                 1      1
009900960524     D  BARLNP                 2      4  0
010000960524     D  BARLNA                 5      7  0
010100960524     D  BARNRS                 8      9  0
010200960524     D  BARNSC                10     16  0
010300960524     D  BARNPS                17     18  0
010400960524     D  BARVUC                19     25  6
010500960524     D  BARZNC                26     27  0
010600021002     D  BARFGS                28     30  0
010700960524     D  BARNPG                31     31  0
010800991223     D  BARNFV                32     37  0
010900991223     D  BARPUC                38     44  3
011000000223     D  BARDFS                45     52  0
011100070209     D  BARHMS                53     58  0
011200070209     D  BARmis                59     61  0
011300070822     D  BARimmis              45     61  0
011400070209     D  BARPRU                62     71
011500070209     D  BARTAP                72     72
011600960524     D* DS PER CONFRONTO SEGNACOLLO CHE STO CARICANTO
011700070209     D  BAR1                   1     72
011800961212     D* KEY PER CHAIN FOGLI VIAGGIO A ROTTURA
011900991223     D  BARKEY                28     37  0
012000930701     D*-----------------------------
012100930701     D* DS PER CONFRONTO SPUNTE: BRV
012200930701     D*-----------------------------
012300930701     D                 DS
012400960524     D  BRVCAN                 1      1
012500960524     D  BRVLNP                 2      4  0
012600960524     D  BRVLNA                 5      7  0
012700960524     D  BRVNRS                 8      9  0
012800960524     D  BRVNSC                10     16  0
012900960524     D  BRVNPS                17     18  0
013000960524     D  BRVVUC                19     25  6
013100960524     D  BRVZNC                26     27  0
013200021002     D  BRVFGS                28     30  0
013300960524     D  BRVNPG                31     31  0
013400991223     D  BRVNFV                32     37  0
013500991223     D  BRVPUC                38     44  3
013600000223     D  BRVDFS                45     52  0
013700070131     D  BRVHMS                53     58  0
013800070209     D  Brvmis                59     61  0
013900070822     D  Brvimmis              45     61  0
014000070209     D  BrvPRU                62     71
014100070209     D  BrvTAP                72     72
014200021014     D* DS PER CONFRONTO SEGNACOLLO CHE STO CARICANTO
014300070209     D  BRV                    1     72
014400960524     D* KEY PER CHAIN FOGLI VIAGGIO A ROTTURA
014500991223     D  BRVKEY                28     37  0
014600960527     D                 DS
014700021014     D  BR7FGS                 1      3  0
014800960527     D  BR7NPG                 4      4  0
014900991223     D  BR7NFV                 5     10  0
015000960527     D* KEY PER CHAIN FOGLI VIAGGIO A ROTTURA
015100991223     D  BR7KEY                 1     10  0
015200020205     D                 DS
015300040303     D  BR9FGS                 1      3  0
015400040303     D  BR9NPG                 4      4  0
015500040303     D  BR9NFV                 5     10  0
015600040303     D  BR9KEY                 1     10  0
015700941213     D WLBDAT          DS
015800941213     D  G02DAT                 1      8  0
015900941213     D  G02INV                 9     16  0
016000941213     D  G02ERR                17     17
016100941213     D  G02TGI                18     22  0
016200000223     D WGIDAT          DS
016300000223     D  GI8DAT                 1      8  0
016400000223     D  GI8INV                 9     16  0
016500000223     D  GI8TGI                17     21  0
016600950927     D*LINEE   DI ARRIVO
016700950927     D                 DS
016800950927     D  FVALNA                 1      3  0
016900950927     D  FVAFFV                 4    243
017000950927     D  FVAFF2               244    450
017100000505     D  FWAFF3               451    690
017200000505     D  FWAFF4               691    900
017300000505     D  VUOTO                901    903
017400000505     D  FFV                    1    903
017500000505     D                                     DIM(301)
017600950927     D*FILIALE DI SCARICO
017700950927     D                 DS
017800950927     D  LNA2                   1      3  0
017900950927     D  FVAFLP                 4    243
018000950927     D  FVAFL2               244    450
018100000505     D  FWAFL3               451    690
018200000505     D  FWAFL4               691    900
018300000505     D  VUOTO2               901    903
018400000505     D  FLP                    1    903
018500000505     D                                     DIM(301)
018600980115     D* LINEE DI ARRIVO  PER FNFGV00F
018700980115     D                 DS
018800980115     D  FGVLNA                 1      3  0
018900980115     D  FGVFFV                 4    243
019000980115     D  FGVFF2               244    450
019100000505     D  FGWFF3               451    690
019200000505     D  FGWFF4               691    900
019300000505     D  VUOTO3               901    903
019400000505     D  FFG                    1    903
019500000505     D                                     DIM(301)
019600980115      *
019700161007     D**               DS
019800161007     D**GCPAGC                 1      4  0
019900161007     D**GCPMGC                 5      8  0
020000161007     D**GCPDGC                 1      8  0
020100970217     D                 DS
020200980128     D  ARTAAS                 1      4  0
020300980128     D  ARTLNP                 5      7  0
020400980128     D  ARTNRS                 8      9  0
020500980128     D  ARTNSP                10     16  0
020600980128     D  ARTTBO                17     17
020700980128     D  ARTSPE                 1     17
020800970217     D                 DS
020900970217     D  BLTAAS                 1      4  0
021000970217     D  BLTLNP                 5      7  0
021100970217     D  BLTNRS                 8      9  0
021200970217     D  BLTNSP                10     16  0
021300970217     D  BLTTBO                17     17
021400970217     D  BLTSPE                 1     17
021500970218     D                 DS
021600970218     D  BTTAAS                 1      4  0
021700970218     D  BTTLNP                 5      7  0
021800970218     D  BTTNRS                 8      9  0
021900970218     D  BTTNSP                10     16  0
022000980120     D  BTTTBO                17     17
022100970218     D  BTTSPE                 1     17
022200970217     D                 DS
022300970217     D  SAVAAS                 1      4  0
022400970217     D  SAVLNP                 5      7  0
022500970217     D  SAVNRS                 8      9  0
022600970217     D  SAVNSP                10     16  0
022700970508     D  SAVBOL                 1     16  0 INZ
022800970508     D  SAVTBO                17     17    INZ
022900970217     D  SAVSPE                 1     17
023000980120     D                 DS
023100980120     D  WDAAS                  1      4  0
023200980120     D  WDLNP                  5      7  0
023300980120     D  WDNRS                  8      9  0
023400980120     D  WDNSP                 10     16  0
023500980120     D  WDSPE                  1     16
023600021014     D                 DS
023700021014     D  WCED                   1      6
023800021014     D  WTER                   7      9
023900021014     D  WVUOTO                10     10
024000021014     D  WPROF                  1     10
024100020201     D                 DS
024200020201     D  BLPAAS                 1      4  0
024300020201     D  BLPMGS                 5      8  0
024400020201     D  BLPDSP                 1      8  0
024500020201     D                 DS
024600020201     D  ARBAAS                 1      4  0
024700020201     D  ARBMGS                 5      8  0
024800020201     D  ARBDSP                 1      8  0
024900020201     D                 DS
025000020201     D  BTPAAS                 1      4  0
025100020201     D  BTPMGS                 5      8  0
025200020201     D  BTPDSP                 1      8  0
025300930615     D KPJBA         E DS
025400110103      *
025500110103      * - Parametri per richiamo al pgm di controllo profilo utenti
025600110103     d TIBS34ds      e ds
025700110103      * - Ds di riferimento al file esterno AzUte00F
025800110103     d AZUTEds       e ds                  ExtName(AzUte00F)
025900110103      * - Ds per dati organigramma
026000110103     d dDatiUte      e ds
026100110103      *
026200961122     D DSTV          E DS
026300990121     D DCCH            DS
026400990121     D  §CHCRA                51     51
026500990121     D DTAD            DS
026600990121     D  §TARGR                26     26
026700990126     D  §TARIT                82     82
026800961122     D DS7C          E DS
026900921020     D DS7N          E DS
027000040430     D DS7O          E DS
027100921229     D DS7J          E DS
027200970522     D DS7A2         E DS
027300030723     D DVPO          E DS
027400000928     D DS5A          E DS
027500180125     D DS3E          E DS
027600170505     D**** DS5APT        E DS
027700000614     D OG143         E DS
027800941229     D* DS PER TRUL06R - CARICAMENTO £X
027900941229     D DSUL06        E DS                  EXTNAME(TRUL06DS)
028000941229     D  LIN                    1     90  0
028100941229     D                                     DIM(30)
028200000705     D DSLV53        E DS                  EXTNAME(FNLV53DS)
028300020213     D DSLV55        E DS                  EXTNAME(FNLV55DS)
028400970814     D DSLR33        E DS                  EXTNAME(FNLR33DS)
028500021014     D DSBS55        E DS                  EXTNAME(TIBS55DS)
028600990120     D DSBS02        E DS                  EXTNAME(TIBS02DS)
028700161007     d FNLGI1DS      E DS
028800170125     d
028900170125     d TRULVPODS     e ds
029000170125     D  skFilOk               16    765    dim(250)
029100180124     D fic             s                   like(skFilOk) dim(250) inz
029200170125     d
029300070212     d
029400070212     d* DS passaggio dati
029500070207     D fnls45ds      E DS
029600160726     d
029700160726     D DAGBFLO       E DS
029800091209     D DAR5          E DS
029900091209     D DAR5GEN       E DS
030000180125     D DARSJ         E DS
030100180125     D DARTFLO       E DS
030200990518     D FNANM2          DS
030300960416     D  ANMNR2               397    400B 0
030400070212     D FIBAR           DS
030500030226     D  BA1_file              83     92
030600030226     D  BA1_libr              93    102
030700150622     d                 ds
030800150622     d bsplin
030900150622     d sbsp                           3  0 dim(30) overlay(bsplin)
031000040811
031001180215     d setf_ds         ds
031003180215     d setf                           3    dim(30) overlay(setf_ds)
031100040811      * ds per il controllo della presenza di CA per la spedizione richiesta
031200040811     d FIDN12DS      e ds
031300040811     d  skKey                 26    305    dim(20)
031400040811     d  skAnn                306    325    dim(20)
031500040811     d  skDca                326    485  0 dim(20)
031600040811     d  skFca                486    545  0 dim(20)
031700040811     d  skDch                546    705  0 dim(20)
031800040811     d  skCch                706    745    dim(20)
031900040811     d*
032000070212     D DSRECBRV      E DS                  extname(fnbrv00f)  prefix(D_)
032100161007     D tigcp00ds     E DS                  extname(tigcp00f)
032200080317     D WTSU            s              1  0
032300160421     D wznc            s              2
032400140613     D kdel_npg        s                   like(brvnpg)
032500170509     D boldbr          s                   like(arbdbr)
032600130228     D KCOD            s                   like(tbecod)
032700130228     D Kke1            s                   like(tbeke1)
032800130228     D Kke2            s                   like(tbeke2)
032900130228     D SAWBRV          s                   like(brv)
033000080317     D WPES            s                   like(brvpes)
033100070208     D BPES            s                   like(brvpes)
033200070208     D BTFP            s                   like(brvFLE)
033300050322     D savesart        s                   like(wesart)
033400041105     D wprimotbo       s                   like(savtbo)
033500070221     D WSAVFLG         s                   like(dls45FLG)
033600140613     D wdfs            s                   like(brvdfs)
033700140616     D qd_nfv          s                   like(brvnfv)
033800140616     D qd_npg          s                   like(brvnpg)
033900140616     D qd_dfs          s                   like(brvdfs)
034000140616     D qd_hms          s                   like(brvhms)
034100140616     D bardcs          s                   like(brvdcs)
034200140616     D barhcs          s                   like(brvhcs)
034300140616     D barfcf          s                   like(fvvfcf)
034400140616     D brvfcf          s                   like(fvvfcf)
034500150622     d w_clibo         s                   Like(blpccm)
034501180216     d wdarr           s                   like(artdam)
034600160726     d welasta_p       s              1
034700160726     d wBspTp          s              1
034800150622     d wBspspu         s              1
034900140618     d A_imm           s             14  0 inz
035000140618     d B_imm           s             14  0 inz
035100140618     d brv_imm         s             14  0 inz
035200140618     d brv_sca         s             14  0 inz
035300140618     d DatReale        s              8  0 inz
035400140618     d oraReale        s              4  0 inz
035500130228     d Indx            s              3  0
035600130228     d SAVfnrs         s              5  0 inz
035700130228     d Wflsnrs         s              5  0
035800041013     D WCONTRDFV       s              1    inz('N')
035900041105     D WAGBART         s              1    inz(' ')
036000070906     D WNPS            s              2    inz
036100080630     D WOK             s              1    inz
036200170505     D W138            s              1    inz
036300080630     D Secondi         s              9  0 inz
036400040621     D ktrd            s                   like(ar5trd)  inz('GEN')
036500180115     D karstr          s                   like(arstrc)  inz('J')
036600080630     D Timeold         s               t   timfmt(*iso)
036700080630     D Timenew         s               t   timfmt(*iso)
036800180123     d savcan          s                   like(barcan)
036900180126     d w_puc           s                   like(bltpuc)
037000180126     d w_fpc           s                   like(bltfpc)
037100180126     d w_vuc           s                   like(bltvuc)
037200180126     d w_fvc           s                   like(bltfvc)
037201180206     d waggRB          s              1
037202180215     d w_key1          s                   like(tbeke1)
037300980928     D* DEFINIZIONE COSTANTI
037400980928     D CUTI            C                   CONST('EDP*      ')
037500960423     C*---------------------------------------------------------------*
037600921014     C* RIEPILOGO INDICATORI
037700970115     C*---------------------------------------------------------------*
037800921014     C* 02    - RICHIAMATO
037900930914     C* 03    - POSSO RIABILITARE LA BOLLA
038000921021     C* 05    - MODIFICA DEL SOLO NUMERO DI FOGLIO
038100000505     C* 28/29 - DI COMODO
038200070212     C* 30    - LETTURA FIBAR00F
038300070131     C* 31    - CHAIN FNBRV
038400950927     C* 32    - LETTURA FILE FNBLT FNART FNARB FNBTT
038500950927     C* 33/34 - DI COMODO
038600921014     C* 37    - NON ESISTE BOLLA TRANSITO
038700070131     C* 38    - ERRORE SU WRITE FNBRV e su update
038800921105     C* 39    - RECORD ALLOCATO
038900921014     C* 40    - SE NON RECORD ALLOCATI O ALLOCATI PER 3 VOLTE FINE PGM
039000950926     C* 41    - COMODO
039100030528     C* 45    - Errore su write fnagb
039200040310     C* 46    - lookuip su schiera NTS
039300970204     C* 96    - ESISTE GIA' A MENO L'ANOMALIA CHE DEVO CREARE
039400970204     C* 97    - USATO PER LA CHIUSURA ANOMALIE CON 145 E 146
039500080325     C* 98    - FIBAR allocato --> esco dal pgm
039600080325     C* 99    - DI COMODO
039700921014     C*---------------------------------------------------------------*
039800911016     C     *ENTRY        PLIST
039900070212     C                   PARM                    fnls45ds
040000070208     C                   PARM                    dsrecbrv
040100941214     C*---------------------------------------------------------------*
040200070221     c* tipo di richiamo in base a dls45FLG
040300070208     C*---------------------------------------------------------------*
040400930521     C* FLG = '2' MODIFICATO SOLO NUMERO FOGLIO
040500160624     c
040600160624     c* Eliminato tipo richiamo "6" in quanto eliminate anomalie 60 e 61
040700070221     C* FLG = '6' CREO SOLO ANOMALIA 61 (EXSR CRTA60) DA SPU DISGUIDO
040800160624     c
040900991022     C* FLG = 'D' CREO ANOMALIA DISGUIDO PER COLLO DA ME CONFERMABILE
041000930521     C* FLG = ' ' INSERIMENTO NUOVA SPUNTA O VARIATA SOLO ANOMALIA
041100930521     C* FLG = 'C' CHIUDO PGM CON LR E BASTA
041200050418     C* FLG = 'T' aggiorno bolla transito x disguido
041300070221     c*
041400070221     c* Richiami attualmente usati: '2'  ' '  'T'  'C'
041500070208     C*---------------------------------------------------------------*
041600070208     c* Eaborazioni per chiusura pgm
041700070221    1C     dls45FLG      IFEQ      'C'
041800070208     c                   EXSR      ELACHIUDI
041900040811     c*
042000930521     C                   SETON                                        LR
042100930521     C*
042200930521   X1C                   ELSE
042300941229     C*
042400941229     C* IMPOSTAZIONI INIZIALI
042500941229    2C     UNO           IFEQ      ' '
042600941229     C                   EXSR      CARTAB
042700941229    2C                   END
042800070208     c
042900070208     c* Impostazione di BPES
043000070208     c                   if        dls45pes>*zeros
043100070208     c                   movel     dls45pes      BPES
043200070208     c                   endif
043300070208     c                   if        dls45tfp>*zeros
043400070208     c                   movel     dls45tfp      BTFP
043500070208     c                   endif
043600070208     c
043700070208     c                   EXSR      IMPO
043800930521     C*
043900930521     C                   SETOFF                                       05
044000941229     C                   Z-ADD     1             B                 3 0
044100930521     C****
044200930521     C**** P G M   R I C H I A M A T O
044300930521     C****
044400070208    2C                   if        dls45ric='S'
044500070208     C                   SETON                                        02
044600070208     c
044700070208     C* nel secndo parametro passato ci sono tutti i campi della spunta
044800070208     c*  con la quale effettuare gli aggiornamenti
044900070212     C                   Z-ADD     d_brvnpg      BARNPG
045000070212     C                   Z-ADD     D_brvnfv      BARNFV
045100070212     C                   Z-ADD     d_brvlnp      BARLNP
045200070212     C                   Z-ADD     d_brvlna      BARLNA
045300070212     C                   Z-ADD     d_brvnrs      BARNRS
045400070212     C                   Z-ADD     d_brvnsc      BARNSC
045500070212     C                   MOVEL     d_brvcan      BARCAN
045600070212     C                   Z-ADD     d_brvnps      BARNPS
045700070212     C                   Z-ADD     d_brvvuc      BARVUC
045800070212     C                   Z-ADD     d_brvpuc      BARPUC
045900070212     C                   Z-ADD     d_brvdfs      BARDFS
046000070212     C                   Z-ADD     d_brvhms      BARHMS
046100070212     C                   Z-ADD     d_brvmis      BARmis
046200070212     C                   movel     d_brvtap      BARtap
046300070212     C                   Z-ADD     d_brvznc      BARZNC
046400070212     c                   movel     d_brvpru      barpru
046500070212     c                   movel     d_brvfgs      barfgs
046600101130     c                   movel     d_brvdcs      brvdcs
046700101130     c                   movel     d_brvhcs      brvhcs
046800070208     c
046900960530     C                   CLEAR                   SAVMAC
047000960529     C**
047100070208     c** 05 on - modificato solo numero di foglio viaggio
047200070221    3C     dls45FLG      IFEQ      '2'
047300921229     C                   SETON                                        05
047400020213     C                   ENDIF
047500960524     C*
047600070209     C* CONTROLLO CHE ESISTA COD.ANOMALIA NELLA TABELLA ANOMALIE
047700180124     c                   clear                   savcan
047800020204     C     BARCAN        IFNE      ' '
047900020204     C     BARCAN        ANDNE     ' '
048000020204     C     BARCAN        LOOKUP    CAN                                    34
048100180124     c                   if        not *in34
048200180124     c                   eval      savcan=barcan
048300180124     c                   endif
048400020204     C  N34              CLEAR                   BARCAN
048500020204     C                   ENDIF
048600961213     C*
048700961213     C* LETTURA FOGLIO E IMPOSTAZIONE CAMPI DI COMOD
048800961213     C                   EXSR      BARFGV
048900960529     C**
049000960529     C* SE NON HO TROVATO IL FOGLIO ESCO IMPOSTANDO L'ERRORE
049100960529    3C     BARDFV        IFEQ      0
049200070906     C                   SETON                                        39
049300960529   X3C                   ELSE
049400991230     C     BARLNA        CHAIN     AZORG01L                           31
049500040303    4c                   if        *in31 or
049600040303     c                             orgfag<>'F' and orgfag<>'A'
049700040303     C                   CLEAR                   NTWBAR
049800040303   x4c                   else
049900040303     C                   MOVE      ORGDE3        OG143
050000040303     C                   MOVE      §OGNTW        NTWBAR
050100000523     C*
050200000523     C* SE DEVO CREARE ANOMALIA DI DISGUIDO, CHIAMO SOLTANTO CRTA52
050300070221    5C     dls45FLG      IFEQ      'D'
050400991022     C                   MOVEL     'D'           WTIBOL
050500000523   X5C                   ELSE
050600920424     C*
050700920424     C* SE TROVO RECORD ALLOCATI RITENTO 3 VOLTE
050800000523    6C     B             DOWLE     3
050900950707     C                   MOVE      'S'           OKAGG             1
051000911016     C                   EXSR      UPDARB
051100000523    7C     OKAGG         IFEQ      'N'
051200950707     C                   Z-ADD     4             B
051300000523   X7C                   ELSE
051400021112     C                   Z-ADD     4             B
051500000523    7C                   ENDIF
051600150824     c
051700000523    6C                   ENDDO
051800150824
051900150824     c* Colli che partono dapiù filiali: facico fare la write al
052000150824     c*  chiamante
052100150824     C                   IF        wbsptp<>' ' and wbspspu='S'
052200150824     c                             and wagpar=' '
052300150824     c                   eval      %subst(dls45flo:1:1)='B'
052400150824     c                   endif
052500150824     c
052600040303    5C                   ENDif
052700040303    4C                   ENDIF
052800960410     C*
052900960410     C* PER BOLLA ARRIVO NON LOCALE, VEDO SE CREARE ANOMALIA 50
053000001114     C*  COLLO ARRIVATO NON PARTITO
053100960529     C*  NON ELABORO SE COLLO LOCALE O SOLO DELLA LINEA DI PARTENZA
053200001115     C* (N.B. INFATTI, SE BOLLA ARRIVO LOCALE, NON C'E' TRASMISSIONE DI
053300001115     C*  F.V. E QUINDI QUESTA ROUTINE NON SAREBBE MAI IN GRADO DI
053400001115     C*  CREARE L'ANOMALIA 50  DAL MOMENTO CHE VA A CERCARE IL FOGLIO
053500001115     C*  QUESTO VALE ANCHE PER I COLLI 180 PER 89)
053600020218    5C  N05WTIBOL        IFNE      'P'
053700960530     C     WTIBOL        ANDNE     'S'
053800020214     C     WTIBOL        ANDNE     'R'
053900070221     C     dls45FLG      ANDNE     '6'
054000960410     C                   EXSR      CRTA50
054100000523    5C                   ENDIF
054200000523     C*
054300960530    3C                   ENDIF
054400911016     C                   GOTO      FINE
054500960529     C*
054600960529    2C                   ENDIF
054700930521     C****
054800930521     C**** P G M    N O N    R I C H I A M A T O
054900930521     C****
055000930521     C* APRO FILE
055100040601     c                   clear                   wconta            2 0
055200040601     c                   movel     'S'           wapri             1
055300070212     C                   OPEN(e)   FIBAR00F
055400040601     c* Attendo 5 secondi e riprovo max 9 volte poi esco con errore
055500040601     c                   dow       %error  and wconta<10
055600040601     C                   Z-ADD     15            LUNG
055700040601     C                   MOVEA(p)  CM5           COMMAN           80
055800040601     C                   CALL      'QCMDEXC'                            99
055900040601     C                   PARM                    COMMAN
056000040601     C                   PARM                    LUNG             15 5
056100040601     c
056200040601     c                   add       1             wconta
056300070212     C                   OPEN(e)   FIBAR00F
056400040601     c                   enddo
056500040601     c
056600040601     c* Se ancora errore vado a fine file
056700040601     c                   if        %error
056800040601     c                   eval      wapri='N'
056900040601     c                   goto      fine
057000040601     c                   endif
057100040601     c
057200920424     C                   SETON                                        40
057300941214     C                   Z-ADD     *ZERO         SA1KEY
057400960530     C                   CLEAR                   SAVMAC
057500070208     C                   MOVE      DLS45FLG2     FLGPRE            1
057600911014     C*
057700080325     C                   READ      FIBAR00F                             9830
057800911016     C*
057900930521    2C     *IN40         DOWEQ     '1'
058000080325    2C     *IN98         andeq     *off
058100920424     C                   SETOFF                                       40
058200930521    3C     *IN30         DOWEQ     '0'
058300080325    3C     *IN98         andeq     '0'
058400930602     C*
058500040419     C                   SETOFF                                       3839
058600930521    4C     BARLNP        IFGT      *ZERO
058700040419     C     BARLNA        ANDGT     *ZERO
058800960731     C     BARNSC        ANDGT     *ZERO
058900040224     C     BARNpg        ANDGT     *ZERO
059000070201     c* Per ora non si accettano fogli maggiori di 100.000
059100040601     C     BARNfv        ANDlT     100000
059200070212     c
059300070212     c* Ad ogni record imposto BPES, perchè per le spunte cat 1 di un
059400070212     c*  secondo livello, BPES diventa il terminal per motivi di
059500070212     c*  creazione della anoamlia 200, per cui potrebbe rimanare
059600070212     c*  sporco se dopo ci fossero spunte di altre categorie
059700070212     c                   movel     dls45pes      BPES
059800021015     C                   Z-ADD     BPES          BARFGS
059900070209     c
060000160511      * CLEARO PESO E VOLUME UNITARI SE SUPERIORI AL MAX PREVISTO
060100160511      * limiti per transpallet  (pistola 46)
060200160511      * LIMITI PER RILEVATORE PESO VOLUME PALLET (Pistola 48)
060300160511      * limite per VDL  ( tutte le altre pistole)
060400160511     c                   select
060500160511     c                   when      barnps= 48
060600030723     C     BARPUC        IFGT      §vpoMPP
060700030723     C                   CLEAR                   BARPUC
060800030723     C                   ENDIF
060900030723     C     BARVUC        IFGT      §vpoMVP
061000030723     C                   CLEAR                   BARVUC
061100030723     C                   ENDIF
061200160511     c
061300160511     c                   when      barnps= 46
061400160511     C     BARPUC        IFGT      §vpoMPT
061500160511     C                   CLEAR                   BARPUC
061600160511     C                   ENDIF
061700160511     c                   other
061800160511     C     BARPUC        IFGT      §vpoMPV
061900160511     C                   CLEAR                   BARPUC
062000160511     C                   ENDIF
062100160511     C     BARVUC        IFGT      §vpoMVV
062200160511     C                   CLEAR                   BARVUC
062300160511     C                   ENDIF
062400160511     C                   Endsl
062500160511     c
062600040805      * CLEARO PESO E VOLUME UNITARI SE INFERIORI AL MINIMO PREVISTO
062700040805      * I LIMITI VALGONO PER TUTTI I TIPI DI IMPIANTO
062800040805     C     BARPUC        IFLT      §vpoSPV
062900040805     C                   CLEAR                   BARPUC
063000040805     C                   ENDIF
063100040805     C     BARVUC        IFLT      §vpoSVV
063200040805     C                   CLEAR                   BARVUC
063300040805     C                   ENDIF
063400020204     C* CONTROLLO CHE ESISTA COD,ANOMALIA NELLA TABELLA ANOMALIE
063500180124     c                   clear                   savcan
063600020204     C     BARCAN        IFNE      ' '
063700020204     C     BARCAN        ANDNE     ' '
063800020204     C     BARCAN        LOOKUP    CAN                                    34
063900180124     c                   if        not *in34
064000180124     c                   eval      savcan=barcan
064100180124     c                   endif
064200020204     C  N34              CLEAR                   BARCAN
064300020204     C                   ENDIF
064400040419     c
064500070906     c* Non carico se LNP inesistente
064600040419     C     BARLNP        CHAIN     AZORG01L                           31
064700040419     c* Non carico se LNA inesistente
064800070906   4bC     *IN31         IFEQ      *Off
064900040419     c     orgfag        andeq     'A'
065000040419     C     *IN31         oreq      *Off
065100040419     c     orgfag        andeq     'F'
065200980212     C*
065300991230     C     BARLNA        CHAIN     AZORG01L                           31
065400040419     c* non carico se LNA inesistente
065500040303    5C     *IN31         IFEQ      *ON
065600040224     c     orgfag        orne      'A'
065700040224     c     orgfag        andne     'F'
065800020212     C                   CLEAR                   NTWBAR
065900040303   x5C                   ELSE
066000020212     C                   MOVEL     ORGDE3        OG143
066100020212     C                   MOVEL     §OGNTW        NTWBAR
066200070912     c
066300070912     C* LETTURA FOGLIO DELLA SPUNTA E IMPOSTAZIONE CAMPI DI COMOD
066400070912     C                   EXSR      BARFGV
066500070912     c
066600070912     c* Non carico se numero foglio inesistente
066700070912   5ac                   if        bardfv>0
066800140616     c                   clear                   amb_QD            1
066900140613     c                   clear                   A_imm
067000140613     c                   clear                   B_imm
067100140613     c*
067200140613     c* se IMA e spunta consegna PIu' RECENTE --> cancello senza caricare
067300140613     c                   if        barnpg=3 and barspg='A'
067400140616     c                   eval      kdel_npg=4
067500140613     c                   exsr      Cer_cons
067600140613     c                   endif
067700140616     c                   if        barnpg=4
067800140616     c                   eval      kdel_npg=3
067900140616     c                   exsr      Cer_cons
068000140616     c                   endif
068100140613
068200140616   5cc                   if        amb_QD<>'N'
068300040303     c**
068400040303     C* VERIFICO IL FLAG NELLA DS7N
068500040303     C                   Z-ADD     1             WA                3 0
068600040310     C     BARNPG        LOOKUP    CAT(WA)                                46
068700040310    6C     *IN46         IFEQ      *OFF
068800040303     C     NST(WA)       ORNE      '1'
068900040303     c* Se tengo tutte le spunte, verifico spunta doppia per lo stesso NFV
069000070131     C     KBAR1         CHAIN     FNBRV01L                           31
069100040303   x6c                   else
069200911016     C*
069300040303     C* spunta doppia per stessa categoria e p.o. gestione
069400140613     C     KBAR2         CHAIN     FNBRV05L                           31
069500040303    6c                   endif
069600941229     C*
069700040303     C* Aggiorno bolle e spunta se:
069800040303     C* - spunta nuova (31 on)
069900040303     C* - spunta già esistente ma dati del record diversi(BAR1<>BRV)
070000040303     C* - spunta già presente ma nel giro prec.trovati record allocati(B>1)
070100070212     c* - trovati errori  (BARERR=E)
070200070912   5bC     *IN31         IFEQ      *ON
070300941229     C     BAR1          ORNE      BRV
070400001114     C     B             ORGT      1
070500070212     C     BARERR        OReq      'E'
070600040303     c**
070700040303     C* OPERAZIONI PER SPUNTA GIA' PRESENTE e da tenerne una
070800040303    6C     *IN31         IFEQ      *OFF
070900040303     C     nst(WA)       ANDEQ     '1'
071000010608     C     BAR1          ANDNE     BRV
071100010608     C                   EXSR      SRBRVE
071200040303    6C                   ENDIF
071300070710     c**
071400070710     C* OPERAZIONI PER SPUNTA GIA' PRESENTE e da tenerne una
071500070710    6C     *IN31         IFEQ      *OFF
071600070710     C     nst(WA)       ANDne     '1'
071700070710     C     BAR1          ANDNE     BRV
071800070710     c* la precedente spunta non ha l'anomalia
071900070710     c     brvcan        andeq     ' '
072000070710     c* nell'attuale è stata inserita
072100070710     c     barcan        andne     ' '
072200070822     c*  Carico solo se data/ora Immissione spunta sono < della precedente
072300070822     c     barimmis      andgt     brvimmis
072400070710     C                   EXSR      SRBRVE
072500070710    6C                   ENDIF
072600040303     C* RIEMPI I CAMPI
072700040303     C                   EXSR      RIEMPI
072800040303     C**
072900040303     C**  A G G I O R N A M E N T I
073000961213     C                   MOVE      'S'           OKAGG             1
073100911112     C                   EXSR      UPDARB
073200911014     C**
073300920424     C**  AGGIORNO SPUNTA
073400950707     C*   Non l'aggiorno se spunta partenza manuale e di collo già
073500950707     C*   consegnato
073600040303    6C     OKAGG         IFEQ      'S'
073700040303     C*
073800960521     C* SE NO  LA DEVO TRASMETTERE LA FLAGGO
073900961213    7C     WNOTRA        IFEQ      'S'
074000960521     C                   MOVEL     'N'           BRVFTR
074100960521     C                   MOVEL     *HIVAL        BRVDTR
074200991223     C                   MOVEL     *HIVAL        BRVHTR
074300960521     C                   CLEAR                   WNOTRA
074400961213    7C                   ENDIF
074500980115     C* SE SPUNTA PARTENZA RELATIVA AD UN FOGLIO VIAGGIO SU CUI NON E'
074600980115     C* PIU' PRESENTE LA LNA DEL COLLO -> FLAGGO LA SPUNTA
074700980205     C* (RIENTRA IN QUESTO CASO ANCHE UNA SPUNTA CON COD.ANOMALIA "E"
074800980205     C* CIOE' UNA SPUNTA LA CUI LINEA NON ERA PRESENTE SUL FV GIA' NEL
074900980205     C* MOMENTO IN CUI E' STATA FATTA)
075000040303    7C     BARNPG        IFEQ      1
075100980115     C                   MOVE      BARLNA        W003A             3
075200980313     C     W003A         LOOKUP    FFS                                    33
075300980115     C  N33              MOVEL     'L'           BRVFTR
075400980115     C  N33              MOVEL     *HIVAL        BRVDTR
075500991223     C  N33              MOVEL     *HIVAL        BRVHTR
075600980210     C   33BRVCAN        IFEQ      'E'
075700980210     C                   CLEAR                   BRVCAN
075800980210     C                   END
075900041020    7C                   ENDIF
076000041020     c
076100041020     c* Imposto brvatr='D' per disguido
076200041020     c                   if        wtibol='D'
076300041020     c                   eval      brvatr='D'
076400041020     c                   else
076500041020     c                   clear                   brvatr
076600041020    7C                   ENDIF
076700040303     c**
076800070131     C* AGGIORNO LF FNBRV01L
076900040310    7C  N31*IN46         IFEQ      *OFF
077000040303     C     NST(WA)       ORNE      '1'
077100070131     C                   UPDATE    FNBRV000
077200040303     C                   ELSE
077300140613     C* AGGIORNO LF FNBRV05L
077400140613     C                   UPDATE    FNBRV005
077500040303    7C                   END
077600010607     C*
077700070131     C   31              WRITE     FNBRV000                             38
077800140613     c
077900140613     c* Se previsto cancello la spunta "doppia" tra 3 e 4
078000140616    7c                   if        amb_QD='S'
078100140616     c                   exsr      Canc_QD
078200140616    7c                   endif
078300150824     c**
078400150824     c*  COLLO SPUNTABILE DA PIÙ FILIALI
078500150824     c**
078600150824     c** elaboro per "K-cli" con BPES <> LNPD ( in qeusto caso ho pulito
078700150824     c*                                        AGGPAR)
078800150824     c*  oppure  per "L-linea" se TFPLNP <>TFPPES
078900150824     c* lo faccio dopo la write di BRV perchè fnlv52r deve leggere la spunta per
079000150824     c*  determinare la nuova lnp della bolla
079100150824     C                   IF        wbsptp<>' ' and wbspspu='S'
079200150824     c                             and wagpar=' '
079300150824     c                   exsr      DA_BSP
079400150824     c                   endif
079500140613     c
079600040303   X6C                   ELSE
079700040303     C*
079800040310    7C  N31*IN46         IFEQ      *OFF
079900040303     C     NST(WA)       ORNE      '1'
080000070131     C                   UNLOCK    FNBRV01L
080100040303     C                   ELSE
080200140613     C                   UNLOCK    FNBRV05L
080300040303    7C                   END
080400040303    6C                   END
080500950927     C* PER BOLLA ARRIVO NON LOCALE, VEDO SE CREARE ANOMALIA 50
080600950927     C*  COLLO ARRIVATO NON PARTITO
080700960207     C*  NON ELABORO SE COLLO LOCALE O SOLO DELLA LINEA DI PARTENZA
080800020214    6C  N05WTIBOL        IFEQ      'D'
080900020214     C     WTIBOL        OREQ      'A'
081000020214     C     WTIBOL        OREQ      'T'
081100020218     C     WTIBOL        OREQ      'L'
081200950927     C                   EXSR      CRTA50
081300950927    6C                   ENDIF
081400950927     C*
081500070912   5bC                   ELSE
081600040303     C*
081700040310    6C  N31*IN46         IFEQ      *OFF
081800040303     C     NST(WA)       ORNE      '1'
081900070131     C                   UNLOCK    FNBRV01L
082000040303     C                   ELSE
082100140613     C                   UNLOCK    FNBRV05L
082200040303    6C                   END
082300070912   5bC                   END
082400140613   5cC                   END
082500110103     c*
082600070912   5ac                   else
082700070912     c* Scrivo la spunta errata in fibar00e
082800070912     c                   movel     bpes          barpes
082900070912     c                   movel     wdate1        bardim
083000180126     c                   eval      barcan=savcan
083100070912     c                   write     barcoerr                             99
083200070912   5aC                   END
083300040303    5C                   END
083400070906   4bC                   END
083500040419    4C                   END
083600911112     C*
083700900619     C* CANCELLO RECORD LETTO
083800001114     C* Se spunta imi e non ho aggiornato blt perchè inesistente o se
083900021203     C* non ho aggiornato blp perchè record allocato la spunta non viene
084000001114     C* cancellata.
084100021107     c                   if        not *in38 and not *in39
084200021107     C                   DELETE    BARCO
084300021107     c                   else
084400021203     c* Se ci fosse errore ma passati 4 giorni dalla data di immissione
084500021203     c*  spunte, deleto lo stesso
084600021203     c                   if        brvdfs<=wpul050
084700021203     c                   DELETE    BARCO
084800021203     c                   else
084900070212     c                   eval      barerr='E'
085000180126     c                   eval      barcan=savcan
085100021107     c                   update    BARCO
085200021107     c                   endif
085300021203     c                   endif
085400900618     C*
085500080325     C                   READ      FIBAR00F                             9830
085600930521    3C                   END
085700021112     C** 40              ADD       1             B
085800021112     C   40              z-add     4             B
085900920424     C* DOPO 3 VOLTE ESCO
086000930521    3C   40B             IFGT      3
086100920424     C                   SETOFF                                       40
086200930521   X3C                   ELSE
086300070212     C                   CLOSE     FIBAR00F
086400070212     C                   OPEN      FIBAR00F
086500070212     C                   READ      FIBAR00F                               30
086600930521    3C                   END
086700930521    2C                   END
086800970224     C* ELABORO L'ULTIMO DATO MEMORIZZATO
086900970224     C                   EXSR      WRTAGB
087000911016     C*
087100911016     C     FINE          TAG
087200930521     C** 02 OFF - PROGRAMMA NON RICHIAMATO
087300930521     C**
087400950926     C** LANCIO PGM DI AGGIORNAMENTO BOLLE
087500961212    2C     *IN02         IFEQ      *OFF
087600970814     C**
087700970814     C                   CLEAR                   DSLV55
087800970814     C                   MOVEL     'C'           D55TLA
087900020213     C                   EXSR      FNLV55
088000000705     C**
088100000705     C                   CLEAR                   DSLV53
088200000705     C                   MOVEL     'C'           D53TLA
088300000705     C                   CALL      'FNLV53R'
088400000705     C                   PARM                    DSLV53
088500930521     C*
088600040601     c                   if        wapri='S'
088700070212     C                   CLOSE     FIBAR00F
088800040601     c                   endif
088900930521     C*
089000981105     C* SE HO ALEMNO UN FOGLIO SCRITTO--> TRASMETTO
089100111018     C***  ERF(1)        IFNE      *BLANKS
089200111018     C***                EXSR      TRASMI
089300111018     C***                ENDIF
089400040811     c*
089500040811     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
089600040811     c                   clear                   fidn12ds
089700040811     c                   eval      i12tla = 'C'
089800040811     c                   call      'FIDN12R'
089900040811     c                   parm                    fidn12ds
090000040811     c*
090100930521     C                   SETON                                        LR
090200930521     C*
090300930521   X2C                   ELSE
090400930521     C**
090500930521     C** 02 ON  - PROGRAMMA RICHIAMATO
090600930521     C**
090700921020     C* SE NON HO AGGIORNATO IMPOSTO LA "E" DI ERRORE
090800960521     C     *IN39         IFEQ      *ON
090900070221     C                   MOVEL     'E'           dls45FLG
091000960521     C                   ELSE
091100960521     C*
091200950926     C* SE AGGIORNATO FACCIO SOTTOMETTERE IL PGM DI AGGIORNAMENTO BOLLE
091300070221     C                   MOVEL     'A'           dls45FLG
091400950707     C* Se non ho aggiornato perchè spunta partenza manuale di collo
091500950707     C* consegnato imposto "N" di aggiornamento non effettuato
091600960521     C     OKAGG         IFEQ      'N'
091700070221     C                   MOVE      'N'           dls45FLG
091800950707     C                   END
091900960521     C                   ENDIF
092000070208     C                   MOVE      DLS45FLG2     FLGPRE            1
092100930521     C*
092200930521     C                   RETURN
092300930521    2C                   END
092400930521    1C                   END
092500941214     C**************************************************************************
092600941214     C* RILEVO IL FOGLIO VIAGGIO PARTENZE/ARRIVI
092700941214     C**************************************************************************
092800941214     C     RILFOG        BEGSR
092900941214     C*
093000941214     C                   Z-ADD     *ZERO         WDFV
093100941229     C                   CLEAR                   WSPG
093200021014     C                   Z-ADD     0             WFLE
093300961122     C                   CLEAR                   WTTR
093400980115     C                   CLEAR                   FFG
093500140613     C                   CLEAR                   WFCF
093600021024     c*
093700021024     c* Prendo il terminal di partenza del p.o. in gestione
093800070212    1c                   if        savfgs<>wfgs
093900021024     C                   CLEAR                   DSLV55
094000021024     C                   MOVEL     'P'           D55TPT
094100021024     C                   MOVEL     WFGS          D55LIN
094200021024     C                   MOVEL     WDATE1        D55DRF
094300021024     C                   CALL      'FNLV55R'
094400021024     C                   PARM                    DSLV55
094500021024     C**
094600070212    2C     D55ERR        IFne      ' '
094700021024     C                   MOVE      wfgs          tfpfgs
094800021024     c                   else
094900021024     C                   MOVE      d55tfp        tfpfgs
095000070212    2c                   endif
095100021024     c                   eval      savfgs=wfgs
095200070212    1c                   endif
095300941214     C*
095400941214     C* ARRIVI O PARTENZE ?
095500970114    1C     WNPG          IFEQ      1
095600941214     C*
095700021014     C     KFGV1         CHAIN     FNFGV01L                           99
095800000505     C*
095900070209    2C     *IN99         IFEQ      *OFF
096000021014     C     KFGV1         CHAIN     FNFGW01L                           28
096100070209    3C     *IN28         IFEQ      *ON
096200000505     C     FGWATB        ORNE      ' '
096300000505     C                   CLEAR                   FGWFF3
096400000505     C                   CLEAR                   FGWFF4
096500000505     C                   CLEAR                   FGWFL3
096600000505     C                   CLEAR                   FGWFL4
096700070209    3C                   ENDIF
096800980928     C                   Z-ADD     FGVDFV        WDFV
096900021024     C                   Z-ADD     FGVLNP        WFgs
097000021024     C                   Z-ADD     FGVLNP        WFLE
097100980928     C                   MOVEL     FGVTTR        WTTR
097200140613     C                   MOVEL     FGVFCF        WFCF
097300980928     C**
097400070209   X2C                   ELSE
097500960925     C     KFVA1         CHAIN     FNFVA01L                           99
097600970114    3C     *IN99         IFEQ      *OFF
097700970114     C                   Z-ADD     FVADFV        WDFV
097800021014     C                   Z-ADD     FVALNP        WFLE
097900970114     C                   MOVEL     FVATTR        WTTR
098000140613     C                   MOVEL     'S'           WFCF
098100111018   x3C**                 ELSE
098200110103     c*
098300070209     C* Se non trovato invio msg di errore
098400111018     C**                 EXSR      INVMSG
098500070212    3C                   ENDIF
098600070212    2C                   ENDIF
098700941214     C*
098800970114   X1C                   ELSE
098900941214     C*
099000021014     C* ACCEDO DIRETTAMENTE AL FOGLIO CON P.O. GESTIONE
099100021014     C* SE FOGLIO NON TROVATO CERCO CON SIMFEL- PUÒ ESSERE SOLO UN
099200070212     C*  FOGLIO IMP SOLO SE RICHIAMO PER DATI DI FIBAR!!
099300021014     C     KFVV1         CHAIN     FNFVV01L                           99
099400070209    2C     *IN99         IFEQ      *ON
099500021014     C     WNPG          ANDEQ     3
099600021014     C     RICBAR        ANDEQ     'S'
099700021014     C                   Z-ADD     SIMFEL        WFGS
099800021014     C     KFVV1         CHAIN     FNFVV01L                           99
099900021014     C  N99FVVSPG        IFNE      'P'
100000021014     C                   SETON                                        99
100100021014     C                   ENDIF
100200070209    2C                   ENDIF
100300021014     C*
100400070209    2C     *IN99         IFEQ      *OFF
100500980928     C                   Z-ADD     FVVDFV        WDFV
100600980928     C                   MOVEL     FVVSPG        WSPG
100700021014     C                   MOVEL     FVVFGS        WFGS
100800140613     C                   MOVEL     FVVFCF        WFCF
100900111018     C**                 ELSE
101000111018     C**                 EXSR      INVMSG
101100070209    2C                   ENDIF
101200941214     C*
101300970114    1C                   ENDIF
101400021014     C**
101500021024     C* SE WFLE=0 prendo TERMINAL DI PARTENZA di fgs A UDATE
101600070223     c*   di fatto si tratta del TFP di BPES poteva che può differire
101700070223     c*   dal FGS solo in caso di spunte ion procedura arrivo per p.o.
101800070223     c*   con terminal di partenza <> dal terminal arrivo
101900021014    1C     WFLE          IFEQ      0
102000021024     C                   MOVE      tfpfgs        WFLE
102100021014    1C                   END
102200941214     C*
102300941214     C                   ENDSR
102400020213     C**************************************************************************
102500020213     C*    IMPOSTAZIONI DI ALCUNI CAMPI DI PASSAGGIO SE VUOTI
102600020213     C**************************************************************************
102700020213     C     IMPO          BEGSR
102800150824     c                   eval      %subst(dls45flo:1:1)=' '
102900110103     c*
103000070208     C* BPES--> SE NON passato    E' SIMFEL
103100020213     C     BPES          IFEQ      0
103200020213     C                   Z-ADD     SIMFEL        BPES
103300070208     c                   if        btfp=0
103400070208     C                   Z-ADD     SIMFEL        BTFP
103500020213     C                   ENDIF
103600070208     C                   ENDIF
103700020213     C**
103800070208     C* BTFP--> SE NON passato ma c'è BPES, è terminal partenza di BPES
103900070208     c                   if        BTFP=0
104000020213     C                   CLEAR                   DSLV55
104100020213     C                   MOVEL     'P'           D55TPT
104200020213     C                   Z-ADD     BPES          D55LIN
104300020213     C                   Z-ADD     WDATE1        D55DRF
104400020213     C                   EXSR      FNLV55
104500110103     c*
104600020213     C                   Z-ADD     D55TFP        BTFP
104700020213     C                   ENDIF
104800110103     c*
104900070209     c* Se BPES non passato, reimposto anche il campo di input
105000070212     c                   if        DLS45PES<=*zeros
105100070209     c                   movel     bpes          dls45pes
105200070209     c                   endif
105300110103     c*
105400020213     C                   ENDSR
105500961213     C**************************************************************************
105600961213     C*    RIEMPI I CAMPI DEL RECORD DA SCRIVERE
105700961213     C**************************************************************************
105800961213     C     RIEMPI        BEGSR
105900110103     c*
106000080317     C* CONTROLLO SE E' PISTOLA MANUALE
106100080317     C                   EXSR      PISMAN
106200080317     c
106300961213     C* SE NON C'E' AGGIUNGO ALTRIMENTI AGGIORNO
106400080317    1C     WBRVE         IFEQ      'S'
106500070131     C* Se routine richiamata per scrittura di FNBRVe devo sempre clear
106600010608     c* are brvatr
106700010608     C                   CLEAR                   BRVATR
106800070201     C                   CLEAR                   BRVtsu
106900080317   x1C                   ELSE
107000961213     C   31              CLEAR                   BRVATR
107100070201     C   31              CLEAR                   BRVtsu
107200110103     c*
107300080317     c** Conteggio le spunte risparate + volte
107400080317     c** ignoro tutte le spunte manuali
107500100709     c** ignoro se spunta con errore (già caricata)
107600080422    1c                   if        not *in31  and (rps(y)<>'M' or
107700080422     c                             rps(y)='M' and tps(y)='C')
107800100709     c                             and barerr<>'E'
107900110103     c*
108000080317    2c                   if        nst(WA)<>'1'  or
108100080317     c* Se spunta unica  ma stesso numero foglio --> conto doppio
108200080317     c                             (nst(WA)= '1' and brvnfv=barnfv)
108300110103     c*
108400080630     c* Non considero doppia nemmeno se spunta cat 3 o 4 pistola 49
108500080630     c*  sparate entro i 2 sec.
108600080630     c                   eval      wok='S'
108700080630    3c                   if        (barnpg=3 or barnpg=4) and barnps=49
108800080630     c                             and brvnps=49 and brvdfs=bardfs
108900080630     c                   move      barhms        Timenew
109000080630     c                   move      brvhms        Timeold
109100080630     c     Timenew       subdur    Timeold       secondi:*s
109200080701     c                   mllzo     1             secondi
109300080630    4c                   if        secondi<=§vposecdop
109400080630     c                   eval      wok='N'
109500080630    4c                   endif
109600080630    3c                   endif
109700110103     c*
109800080630   2AC                   IF        WOK='S'
109900080317    3c                   if        brvtsu=' '
110000080317     c                   eval      brvtsu='1'
110100080317   x3c                   else
110200080317     c                   movel     brvtsu        wtsu              1 0
110300080317    4c                   if        wtsu<9
110400080317     c                   eval      wtsu=wtsu+1
110500080317     c                   movel     wtsu          brvtsu
110600080317    4c                   endif
110700110103     c*
110800080317    3c                   endif
110900080630   2Ac                   endif
111000110103     c*
111100080317   x2c                   else
111200080317     c* Prendo data foglio
111300080317    3C     BRvKEY        IFNE      SA1KEY
111400080317     C                   MOVEL     BRvKEY        SA1KEY
111500080317     C                   Z-ADD     BRvNPG        WNPG
111600080317     C                   Z-ADD     BRvNFV        WNFV
111700080317     C                   Z-ADD     BRvFGS        WFGS
111800080317     C                   movel     BRvnps        Wnps
111900080317     C                   EXSR      RILFOG
112000080317     c                   Z-ADD     WDFV          BRVDFV
112100140616     c                   movel     WFCF          BRVFCF
112200140616     c                   movel     Wspg          BRVspg
112300080317    3c                   endif
112400110103     c*
112500080317     c* Se spunta unica, foglio diverso ma stessa data -->
112600080317     c*                         mantengo TSU
112700080317     c*                                    data diversa --> lo pulisco
112800080317    3c                   if        bardfv<>brvdfv
112900080317     c                   clear                   brvtsu
113000080317    3c                   endif
113100110103     c*
113200080317    2c                   endif
113300080317    1c                   endif
113400110103     c*
113500080317    1C                   ENDIF
113600110103     c*
113700070201     C                   CLEAR                   BRVnvr
113800961213     C* FILIALE GESTIONE LA PRENDO DAL FOGLIO
113900961213     C                   CLEAR                   BRVDTR
114000961213     C                   CLEAR                   BRVFTR
114100991223     C                   CLEAR                   BRVHTR
114200961213     C                   Z-ADD     BARFGS        BRVFGS
114300961213     C                   Z-ADD     BARNPG        BRVNPG
114400990708     C                   Z-ADD     BARNFV        BRVNFV
114500040428     C* P.O. CHE HA EFFETTUATO LA SPUNTA (l'originale contenuto il WPES
114600040428     c*  e non più il bpes, per le spunte cat.1)
114700040428     C                   Z-ADD     WPES          BRVPES
114800961213     C                   MOVEL     BARLNP        BRVLNP
114900961213     C                   MOVEL     BARLNA        BRVLNA
115000961213     C                   MOVEL     BARNRS        BRVNRS
115100961213     C                   MOVEL     BARNSC        BRVNSC
115200961213     C                   Z-ADD     BARZNC        BRVZNC
115300961213     C**
115400961213     C* SE SONO IN IMMISSIONE LA AGGIORNO, IN VARIAZIONE SOLO
115500970829     C*  SE E' PISTOLA CON PRIORITA' < O UGUALE ALLA PRESENTE
115600961213     C*
115700961213     C* SE L'ANOMALIA GIA' ESISTE, NON LA TOLGO SE SU QUESTA SPUNTA
115800961213     C*  NON C'E'
115900040324    1C     *IN31         IFEQ      *ON
116000010608     C     WBRVE         OREQ      'S'
116100961213     C                   MOVEL     BARCAN        BRVCAN
116200961213     C                   MOVEL     BARNPS        BRVNPS
116300070212     C                   MOVEL     BARtap        BRVtap
116400040324     C                   Z-ADD     BARVUC        BRVVUC
116500040324     C                   Z-ADD     BARPUC        BRVPUC
116600040324   x1C                   ELSE
116700961213     C* CODICE ANOMALIA
116800961213     C     BARCAN        IFNE      ' '
116900980205     C     BRVCAN        OREQ      'E'
117000961213     C                   MOVEL     BARCAN        BRVCAN
117100961213     C                   ENDIF
117200110103     c*
117300970829     C* CERCO LA PRIORITA' DELLA PISTOLA GIA' PRESENTE
117400970829     C                   MOVEL     BRVNPS        ALF2
117500970829     C                   EXSR      RICRAG
117600970829     C* PISTOLA CON PRIORITA' < O = ALLA PRESENTE: E' + IMPORTANTE
117700970829     C     OPS(Y)        IFLE      OPS(Z)
117800961213     C                   MOVEL     BARNPS        BRVNPS
117900070212     C                   MOVEL     BARtap        BRVtap
118000961213     C                   ELSE
118100961213     C                   MOVEL     BRVNPS        BARNPS
118200070212     C                   MOVEL     BRVtap        BARtap
118300961213     C                   ENDIF
118400110103     c*
118500041011     c* Peso e volume prendo se pieno e se non c'e' già
118600041011     c                   if        barpuc>0
118700041011     c                   if        *in31 or (not *in31 and brvpuc=0)
118800040324     C                   Z-ADD     BARPUC        BRVPUC
118900040324     c                   endif
119000041011     c                   endif
119100040324     c                   if        barvuc>0
119200041011     c                   if        *in31 or (not *in31 and brvvuc=0)
119300040324     C                   Z-ADD     BARvUC        BRVvUC
119400040324     c                   endif
119500041011     c                   endif
119600110103     c*
119700040324    1C                   ENDIF
119800961213     C*
119900070212     c* Se la spunta era già stata caricata con errore (di solito
120000070212     c*  per la non creazione della anomalia 50) non aggiorno la data/ora
120100070212     c*  caricamento spunta
120200070212     c                   if        barerr<>'E' or *in31
120300961213     C                   Z-ADD     WDATE1        BRVDCS
120400070131     C                   time                    BRVHCS
120500070212     c                   endif
120600140613     c* impostazione data caricamento spunte
120700140613     c                   exsr      impoDFS
120800140613     c                   z-add     wdfs          brvdfs
120900140613     c
121000070131     c                   clear                   brvhms
121100070131     C                   movel     BARHMS        BRVHMS
121200070212     c                   z-add     barmis        brvmis
121300961213     C                   Z-ADD     BARFLE        BRVFLE
121400070212     C                   movel     BARpru        BRVpru
121500080314     c
121600080314     c
121700961213     C                   ENDSR
121800941206     C**************************************************************************
121900941206     C*    CARICO TABELLE
122000941206     C**************************************************************************
122100941229     C     CARTAB        BEGSR
122200930521     C*
122300930521     C* PARAMETRI UTENTE
122400110103     C                   Z-ADD     1             CODUT             1 0
122500110103      *
122600110103      * Reperimento dati utente/aziendali
122700110103     c                   exsr      sr_DatiJob
122800970320     C*
122900970320     C                   CLEAR                   SAVBOL
123000961212     C**
123100961212     C**  CARICO TABELLA 7J - CODICI PISTOLA
123200961122    1C     *IN02         IFEQ      '0'
123300921229     C                   MOVEL     '7J'          COD
123400990108     C                   Z-ADD     1             X                 4 0
123500921229     C     KTAB2         SETLL     TABEL
123600950927     C     KTAB2         READE     TABEL                                  99
123700921229     C*
123800961122    2C     *IN99         DOWEQ     *OFF
123900961122    3C     TBLFLG        IFEQ      ' '
124000921229     C                   MOVEL     TBLKEY        NPS(X)
124100921229     C                   MOVEL     TBLUNI        DS7J
124200921229     C                   MOVEL     §7JRPS        RPS(X)
124300000210     C                   MOVEL     §7JTPS        TPS(X)
124400000210     C                   MOVEL     §7JORD        OPS(X)
124500000316     C                   MOVEL     §7JINV        INV(X)
124600040503     C                   MOVEL     §7Jfsl        fsl(X)
124700921229     C                   ADD       1             X
124800961122    3C                   END
124900921229     C*
125000950927     C     KTAB2         READE     TABEL                                  99
125100961122    2C                   ENDDO
125200961122    1C                   ENDIF
125300961122     C**
125400961212     C** CARICO TABELLA  TV - TIPI TRAINO DEFLUENZA
125500961122     C                   MOVEL     'TV'          COD
125600990108     C                   Z-ADD     1             X
125700961122     C     KTAB2         SETLL     TABEL
125800961122     C     KTAB2         READE     TABEL                                  99
125900961122     C*
126000961122    2C     *IN99         DOWEQ     *OFF
126100961122    3C     TBLFLG        IFEQ      ' '
126200961122     C                   MOVEL     TBLUNI        DSTV
126300961122    4C     §TVDEF        IFEQ      'S'
126400961122     C                   MOVEL     TBLKEY        TTR(X)
126500961122     C                   ADD       1             X
126600961122    4C                   ENDIF
126700961122    3C                   ENDIF
126800961122     C*
126900961122     C     KTAB2         READE     TABEL                                  99
127000961122    2C                   ENDDO
127100961212     C**
127200961212     C* CARICO LA TABELLA 7N - DELLE CATEGORIE SPUNTE
127300941214     C                   MOVEL     '7N'          COD
127400941214     C                   Z-ADD     1             X
127500941214     C     KTAB2         SETLL     TABEL
127600950927     C     KTAB2         READE     TABEL                                  99
127700941214     C*
127800950927     C     *IN99         DOWEQ     '0'
127900941214     C     TBLFLG        IFEQ      ' '
128000941214     C                   MOVEL     TBLKEY        CAT(X)
128100941214     C                   MOVEL     TBLUNI        DS7N
128200941214     C                   MOVEL     §7NNST        NST(X)
128300941214     C                   ADD       1             X
128400941214     C                   ENDIF
128500941214     C*
128600950927     C     KTAB2         READE     TABEL                                  99
128700941214     C                   ENDDO
128800961212     C**
128900960702     C** CARICO TABELLA 5F - FILIALI CONFERMA BOLLE DA DISK
129000960702     C                   MOVEL     '5F'          COD
129100960702     C                   Z-ADD     0             X
129200960702     C     KTAB2         SETLL     TABEL
129300960702     C     KTAB2         READE     TABEL                                  99
129400960702     C*
129500960702    1C     *IN99         DOWEQ     '0'
129600960702    2C     TBLFLG        IFEQ      ' '
129700070201     C                   MOVEL     TBLKEY        W003A             3
129800070201     C     W003A         IFNE      '   '
129900960702     C                   ADD       1             X
130000960702     C                   MOVEL     TBLKEY        S5F(X)
130100970828     C* MEMORIZZO LA FIL CHE CONFERMA
130200970828     C                   MOVEL     TBLUNI        F5F(X)
130300110103     c*
130400130618     C     X             IFGE      9000
130500070201     C                   SETON                                        99
130600070201     C                   ENDIF
130700110103     c*
130800970828     C                   ENDIF
130900960702    2C                   ENDIF
131000960702     C*
131100960702     C  N99KTAB2         READE     TABEL                                  99
131200960702    1C                   ENDDO
131300961212     C**
131400020204     C** CARICO TABELLA 3E - CODICI ANOMALIA VALIDI SU SPUNTE
131500020204     C                   MOVEL     '3E'          COD
131600961212     C                   Z-ADD     0             X
131700961212     C     KTAB2         SETLL     TABEL
131800961212     C     KTAB2         READE     TABEL                                  99
131900961212     C*
132000961212    1C     *IN99         DOWEQ     *OFF
132100961212    2C     TBLFLG        IFEQ      ' '
132200180123     c* scarto le anomalie di segnalazione che non vengono memorizzate
132300180123     c* sulla spunta
132400180125     c                   eval      ds3e=tbluni
132500180125     c     §3efta        ifne      'S'
132600961212     C                   ADD       1             X
132700020204     C                   MOVEL     TBLKEY        CAN(X)
132800961212     C     X             IFGE      200
132900961212     C                   SETON                                        99
133000961212     C                   ENDIF
133100180125     c                   endif
133200961212    2C                   ENDIF
133300961212     C*
133400961212     C  N99KTAB2         READE     TABEL                                  99
133500961212    1C                   ENDDO
133600020502     C*****
133700020502     C* RILEVO DATA E ORA PER SCRIVERLO NELLE SPUNTE
133800020502     C                   TIME                    WTIME            14 0
133900020502     C                   MOVE      WTIME         WDATE             8 0
134000020502     C                   Z-ADD     WDATE         G02DAT
134100020502     C                   Z-ADD     *ZERO         G02INV
134200020502     C                   MOVEL     *BLANKS       G02ERR
134300020502     C                   CALL      'XSRDA8'
134400020502     C                   PARM                    WLBDAT
134500020502     C                   Z-ADD     G02INV        WDATE1            8 0
134600030725      *
134700030725      * CHAIN A VARIABILI PARTENZE x PRENDERE MAX PESO E VOLUME CARICABILI
134800030725     C                   EXSR      CERVPO
134900970522     C*
135000970522     C* CHAIN A VARIABILI ARRIVI PER VEDERE A QUALE LIVELLO SI CHIUDE
135100970522     C* L'IDD (LIV MAX.)
135200970522     C                   MOVEL     '7A'          COD
135300970522     C                   MOVEL(P)  '2'           KEY
135400970522     C     KTAB          CHAIN     TABEL                              99
135500970522     C  N99              MOVEL     TBLUNI        DS7A2
135600000927     C* CHAIN A GG PULIZIA PER CREAZIONE ANOMLIA 10 E 50
135700000927     C                   MOVEL     '5A'          COD
135800000927     C                   MOVEL(P)  '1'           KEY
135900000927     C     KTAB          CHAIN     TABEL                              99
136000000927     C  N99              MOVEL     TBLUNI        DS5A
136100000927     C*
136200170505     C***                MOVEL     '5A'          COD
136300170505     C***                MOVEL(P)  'PT'          KEY
136400170505     C**   KTAB          CHAIN     TABEL                              99
136500170505     C**N99              MOVEL     TBLUNI        DS5APT
136600000927     C*
136700000927     C* PREDO GG MINORI TRA PULIZIA ARB E PULIZIA ARB POSTE
136800170505     C***  §5AARB        IFLT      §5APT5
136900170505
137000000927     C                   Z-ADD     §5AARB        WGGPUL            3 0
137100170505
137200170505     C***                ELSE
137300170505     C***                Z-ADD     §5APT5        WGGPUL            3 0
137400170505     C***                ENDIF
137500000927     C**
137600000927      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA BOLLE ARRIVI
137700000927     C                   MOVE      *BLANKS       $DAT
137800000927     C                   MOVE      *BLANKS       $ERR
137900000927     C                   MOVEL     'FNLR84R'     $PGM
138000000927     C                   CALL      'TRUL49C'                            28
138100000927     C                   PARM                    $PGM             10
138200000927     C                   PARM                    $DAT              8
138300000927     C                   PARM                    $ERR              1
138400000927     C*
138500000928    3C     $ERR          IFEQ      '1'
138600000927     C     *IN28         OREQ      *ON
138700000928     C     $DAT          ORLE      *ZEROS
138800000927     C                   Z-ADD     WDATE1        G02INV
138900000927     C                   ELSE
139000000928     C                   MOVEL     $DAT          G02INV
139100000927    3C                   ENDIF
139200000927     C*
139300000927      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI
139400000927      * DI PULIZIA BOLLE ARRIVI
139500000927     C                   Z-ADD     *ZEROS        G02DAT
139600000927     C                   Z-ADD     *ZEROS        G02TGI
139700000927     C                   MOVEL     '3'           G02ERR
139800000927     C                   CALL      'XSRDA8'
139900000927     C                   PARM                    WLBDAT
140000000927      *
140100000927     C                   SUB       WGGPUL        G02TGI
140200000928     C                   Z-ADD     *ZEROS        GI8DAT
140300000928     C                   Z-ADD     *ZEROS        GI8INV
140400000928     C                   Z-ADD     G02TGI        GI8TGI
140500000927     C                   CALL      'XSRGI8'
140600000927     C                   PARM                    WGIDAT
140700000927      *
140800000928     C                   Z-ADD     GI8INV        PRIDAT
140900961212     C**
141000070209     c** udate - 4 --> sono i giorni che devo tenere le spunte dentro al
141100070212     c**               FIBAR se non riesco a creare l'anomalia collo
141200021203     c**               arrivato non part
141300021203     C                   Z-ADD     WDATE1        G02INV
141400021203     C                   Z-ADD     *ZEROS        G02DAT
141500021203     C                   Z-ADD     *ZEROS        G02TGI
141600021203     C                   MOVEL     '3'           G02ERR
141700021203     C                   CALL      'XSRDA8'
141800021203     C                   PARM                    WLBDAT
141900021203      *
142000021203     C                   SUB       4             G02TGI
142100021203     C                   Z-ADD     *ZEROS        GI8DAT
142200021203     C                   Z-ADD     *ZEROS        GI8INV
142300021203     C                   Z-ADD     G02TGI        GI8TGI
142400021203     C                   CALL      'XSRGI8'
142500021203     C                   PARM                    WGIDAT
142600021203     C                   Z-ADD     GI8INV        wpul050
142700961212     C**
142800021203     C**
142900930521     C                   MOVEL     '1'           UNO               1
143000930521     C                   MOVEL     'AA'          SAVNPS            2
143100960411     C*
143200960411     C                   MOVE      ' '           OPN9              1
143300961212     C*
143400021014     C                   CLEAR                   DSBS55
143500971113     C                   MOVEL     'L'           I50TLA
143600021014     C                   CALL      'TIBS55R'
143700021014     C                   PARM                    DSBS55
143800971113     C**
143900971113     C                   CLEAR                   KPJBA
144000021014     C                   MOVEL     'EDPCED'      WCED
144100021014     C                   MOVEL     SIMFEL        WTER
144200021014     C                   MOVEL     WPROF         KNMUS
144300971113     C                   MOVEL     O50PSI        KNSIF
144400970829     C                   MOVEL     'DSP01'       KNMTD
144500961212     C                   MOVEL     'N'           KSTJO
144600961212     C                   MOVEL     'B'           KTPAZ
144700961212     C                   MOVEL     'AZNAUTO'     KNMEB
144800961212     C                   MOVEL     'J'           KEXCN
144900961212     C                   MOVEL     'P5'          KCOJB
145000961212     C                   MOVEL     'N'           KCANC
145100000927     C*
145200000927     C                   CLEAR                   SA2DFV
145300001109     C**
145400001109     C** CARICO TABELLA 7C - ANOMALIE CREABILI DA CAT.SPUNTA GENERICA
145500001109     C                   MOVEL     '7C'          COD
145600001109     C                   Z-ADD     0             X
145700001109     C     KTAB2         SETLL     TABEL
145800001109     C     KTAB2         READE     TABEL                                  99
145900001109     C*
146000001109    1C     *IN99         DOWEQ     *OFF
146100001109    2C     TBLFLG        IFEQ      ' '
146200001109     C                   MOVEL     TBLUNI        DS7C
146300001109    3C     §7CCCG        IFEQ      'S'
146400001109     C                   ADD       1             X
146500001109     C                   MOVEL     TBLKEY        ACG(X)
146600001109    3C                   ENDIF
146700001109    2C                   ENDIF
146800001109     C*
146900001109     C     KTAB2         READE     TABEL                                  99
147000001109    1C                   ENDDO
147100110103      *
147200091209      * Aggancio tabella AR5 per record 'GEN'
147300091209     c                   Clear                   dAr5
147400091209     c                   Clear                   dsbs02
147500091209     c                   Eval      T02Mod = 'C'
147600091209     c                   Eval      T02Sif = knsif
147700091209     c                   Eval      T02Cod = 'AR5'
147800091209     c                   Movel(p)  'GEN'         T02Ke1
147900091209     c                   Call      'TIBS02R'
148000091209     c                   Parm                    Kpjba
148100091209     c                   Parm                    dsbs02
148200091209     c                   If        T02Err = *Blanks
148300091209     c                   Movel     T02Uni        dAr5
148400091209     c                   EndIf
148500170125     C*
148600170125     C* CARICO TABELLA DECOFIIMG PER le filiali con New IMA
148700170125     c                   clear                   AllNewIMA         1
148800170125     c                   clear                   trulvpods
148900170125     c                   eval      ivpoke1 = 'DECOFIIPG'
149000170125     c                   call      'TRULVPOR'
149100170125     c                   parm                    trulvpods
149200170125     c                   if         %lookup('999':skFilOk)>0
149300170125     C                   EVAL       AllNewIMA='S'
149400170125     c                   endif
149500180124     C* CARICO TABELLA DECOFIINC PER le filiali con gestione incompatiblil
149600180124     c                   clear                   AllINC            1
149700180124     c                   clear                   trulvpods
149800180124     c                   eval      ivpoke1 = 'DECOFIINC'
149900180124     c                   call      'TRULVPOR'
150000180124     c                   parm                    trulvpods
150100180124     c                   eval      fic = skfilok
150200180124     c                   if        %lookup('999':fic)>0
150300180124     c                   eval      AllINC = 'S'
150400180124     c                   endif
150500170125
150600911014     C                   ENDSR
150700941206     C**************************************************************************
150800941206     C* CREO ANOMALIA
150900941206     C**************************************************************************
151000911016     C     CRTANM        BEGSR
151100000320     C                   CLEAR                   WCCH
151200970521     C*
151300970521     C                   CLEAR                   WANM10
151400941206     C*
151500970204    1C     COMCAA        IFNE      645
151600990518     C     KANM          CHAIN     FNANM000                           96
151700970204   X1C                   ELSE
151800961205     C*
151900990518     C     KANM          CHAIN(N)  FNANM000                           96
152000961205     C* PER L'ANOMALIA 645 LEGGO FINTANTO CHE NON TROVO UNA ANOMALIA
152100961205     C*  CON DATA = ALLA DATA SPUNTA: SE C'E' NON CREO
152200970204    2C     *IN96         DOWEQ     *OFF
152300990518     C     ANMDAO        ANDNE     BARDFV
152400990518     C     KANM          READE(N)  FNANM000                               96
152500970204    2C                   ENDDO
152600961205     C*
152700970204    1C                   ENDIF
152800000811     C*
152900000811    1C     *IN96         IFEQ      *ON
153000000811     C*
153100000811     C     COMCAA        ORNE      645
153200000811     C     ANMAIE        ANDEQ     'I'
153300000811     C* PER ORA SOLO SULLE INTERNE RICREO ANOMLIA SE PRESENTE
153400970204     C*  - SE CHIUSE SEMPRE
153500040923     C*  - SE APERTA SOLO LA DATA E' < DELLA ESISTENTE
153600040923     c* Per la 200 la ricreo solo se e' chiusa senza guardare la data creaz
153700961205     C*
153800970204     C                   MOVEL     'S'           WWRITE
153900970204    2C     *IN96         IFEQ      *OFF
154000970204     C     ANMDCH        ANDEQ     0
154100970204     C     ANMDAO        ANDGE     BARDFV
154200041005    2C     *IN96         oreq      *OFF
154300041005     C     ANMDCH        andeq     0
154400040923     C     comcaa        ANDeq     200
154500970204     C                   MOVEL     ' '           WWRITE            1
154600970204    2C                   ENDIF
154700970204     C**
154800070215    2C     WWRITE        IFEQ      'S'
154900990518     C                   CLEAR                   FNANM000
155000950927     C                   EXSR      RIEANM
155100000614     C**
155200001109     C* CREO SOLO SE ANOMALIA :
155300001109     C* CREABILE DA QUALUNQUE CATEGORIA SPUNTA
155400070215    3C     §7CCCG        ifeq      'S'
155500001109     C* NON CREABILE DA CATEG. GENERICA E LA CATEGORIA NON È GENERICA
155600070215     C     BARSPG        orne      'I'
155700950927     C*
155800950927     C                   MOVE      COMLNP        ANMLNP
155900990518     C                   Z-ADD     COMAAS        ANMAAS
156000950927     C                   MOVE      COMNSP        ANMNSP
156100960524     C                   MOVE      BARDFV        ANMDAO
156200990518     C                   Z-ADD     BARNFV        ANMNFV
156300950927     C                   MOVEL     BARFGS        ANMCDU
156400040621     c* imposto se spedizione di valore
156500070215    4c                   if        anmnsp>0
156600091209     c     kar5          chain(n)  fiar501l
156700070215    5c                   if        %found(fiar501l)
156800040621     c                   movel     ar5uni        dar5gen
156900040621     c                   else
157000040621     c                   clear                   dar5gen
157100070215    5c                   endif
157200040621     c                   movel     §ar5bva       anmft4
157300070215    4c                   endif
157400110103     c*
157500040429     c* 22/4/4 --> al posto di anmfgs imposto BPES il p.o. che spara
157600040429     c*            che solo per le spunte partenza è sempre barfgs
157700040429     C                   MOVEL     bpes          ANMFLE
157800070215    4C     ANMCAA        IFEQ      10
157900150205    4C     ANMCAA        oreq      410
158000070215     C                   z-add     BARZNC        ANMfl4
158100070215    4C                   ENDIF
158200911112     C*
158300921020     C* SE ESTERNA SCRIVO FIL A CUI TRASMETTERE
158400070215    4C     §7CAIE        IFEQ      'E'
158500070215    5C     COMLNP        IFGT      0
158600960528     C                   MOVEL     COMLNP        ANMFL1
158700960528     C                   ELSE
158800960704     C                   MOVEL     LNPB          ANMFL1
158900070215    5C                   ENDIF
159000070215    4C                   ENDIF
159100911016     C*
159200970204     C* SE INTERNA AUTOCHIUSA E C'E' GIA' NON FACCIO NULLA
159300070215    4C     *IN96         IFEQ      *ON
159400970204     C     §7CCHA        ORNE      'S'
159500960430     C* SE LA KEY E' PER SEGNACOLLO ED IL NUMERO DI SEGNACOLLO E' 0
159600960430     C*  NON CREO L'ANOMALIA (ERRORE DA TERMINALINO)
159700070215    5C     §7CKEY        IFNE      'S'
159800960430     C     §7CKEY        OREQ      'S'
159900960430     C     ANMSCN        ANDGT     0
160000990922     C*
160100990922     C* LA DATA I APERTURA ANOMALIA CI DEVE ESSERE , ALTRIMENTI
160200990922     C*  E' MEGLIO NON CREARE L'ANOMALIA
160300070215    6C     ANMDAO        IFGT      0
160400990518     C   96              WRITE     FNANM000
160500990518     C  N96              UPDATE    FNANM000
160600070215    6C                   ENDIF
160700070215    5C                   ENDIF
160800961205     C*
160900970204     C* SE CREO ANOAMLIA 145 O 146 CHIUDO LE ANOMALIE INTERNE 140 E 130
161000070215    5C     ANMCAA        IFEQ      145
161100970204     C     ANMCAA        OREQ      146
161200970204     C                   Z-ADD     130           COMCAA
161300970204     C                   EXSR      CHIUAN
161400970204     C                   Z-ADD     140           COMCAA
161500970204     C                   EXSR      CHIUAN
161600070215    5C                   ENDIF
161700070215    4C                   ENDIF
161800970204     C*
161900070215    3C                   ENDIF
162000070215    2C                   ENDIF
162100970521   X1C                   ELSE
162200110103     c*
162300970521     C* OPERAZIONI PER ANOMALIA 10 GIA' ESISTENTE MA CHIUSA:
162400970521    2C     COMCAA        IFEQ      10
162500970522     C* Se anomalia 10 a liv massimo --> memorizzo per creaz.anom.410
162600970522    3C     ANMLID        IFEQ      §7ALCI
162700970521     C                   MOVE      '1'           WANM10            1
162800970522    3C                   END
162900970522     C* Se anomalia chiusa (non pratica IDD):
163000970522     C* 1) sflaggo e chiudo l'anomalia mettendo cch='PR' se è a liv max
163100970522    3C     ANMDCH        IFGT      *ZEROS
163200970522     C     ANMCCH        ANDNE     'PR'
163300970522     C*
163400970522    4C                   SELECT
163500970522     C     ANMLID        WHENEQ    §7ALCI
163600970522     C     ANMCCH        ANDEQ     'AN'
163700970522     C                   MOVEL     'PR'          ANMCCH
163800970522     C                   MOVEL     'D'           ANMFSC
163900970522     C                   CLEAR                   ANMFT1
164000970522     C                   CLEAR                   ANMFT2
164100970522     C     ANMLID        WHENLT    §7ALCI
164200970523     C                   CLEAR                   ANMCCH
164300970522     C                   CLEAR                   ANMDCH
164400970522     C                   CLEAR                   ANMFSC
164500970522    4C                   ENDSL
164600990518     C                   UPDATE    FNANM000
164700970522    3C                   ENDIF
164800970522    2C                   ENDIF
164900970204    1C                   ENDIF
165000970204     C*
165100990518     C                   UNLOCK    FNANM02L
165200911016     C                   ENDSR
165300960613     C**************************************************************************
165400960613     C*    CONTROLLO IL RAGGRUPPAMENTO DELLA PISTOLA
165500960613     C**************************************************************************
165600960613     C     PISMAN        BEGSR
165700960613     C*
165800961213     C                   Z-ADD     1             Y                 3 0
165900961213     C*
166000960613    1C     *IN02         IFEQ      *OFF
166100960613     C     BARNPS        LOOKUP    NPS(Y)                                 34
166200960613     C  N34              Z-ADD     1             Y
166300960613     C*
166400960613   X1C                   ELSE
166500960613     C*
166600960613     C                   MOVEL     '7J'          COD
166700960613     C                   MOVEL(P)  BARNPS        KEY
166800960613     C     KTAB          CHAIN     TABEL                              34
166900000316     C     *IN34         IFEQ      *OFF
167000000316     C                   MOVEL     TBLUNI        DS7J
167100000316     C                   MOVEL     §7JRPS        RPS(Y)
167200000316     C                   MOVEL     §7JORD        OPS(Y)
167300000316     C                   MOVEL     §7JTPS        TPS(Y)
167400000316     C                   MOVEL     §7JINV        INV(Y)
167500040503     C                   MOVEL     §7Jfsl        fsl(Y)
167600000316     C                   ELSE
167700000316     C                   MOVEL     'M'           RPS(Y)
167800000316     C                   MOVEL     999           OPS(Y)
167900000316     C                   MOVEL     'B'           TPS(Y)
168000000316     C                   MOVEL     ' '           INV(Y)
168100040503     C                   MOVEL     ' '           fsl(Y)
168200960613    1C                   END
168300000316    1C                   END
168400960613     C                   ENDSR
168500941206     C**************************************************************************
168600020213     C*    A G G I O R N O    R E C O R D    B O L L E
168700941206     C**************************************************************************
168800980212     C     UPDARB        BEGSR
168900960521     C                   CLEAR                   WNOTRA
169000961210     C                   CLEAR                   W60X
169100961211     C                   CLEAR                   WESAN
169200961210     C                   MOVEL     'S'           W620
169300961211     C                   MOVEL     'S'           WCREA
169400021106     C                   CLEAR                   WDCM
169500021106     C                   CLEAR                   WBOAR
169600021106     C                   CLEAR                   WATDCM
169700040430     C                   CLEAR                   WABCCA
169800040430     C                   CLEAR                   WABNDC
169900161007     C                   CLEAR                   gcpdgc            8 0
170000021106     C                   MOVEL     ' '           WGIAC             1
170100941206     C*
170200921229     C* TROVO TIPO E RAGGRUPPAMENTO PISTOLE DI QUELLA CARICATA
170300960613     C                   EXSR      PISMAN
170400921229     C*
170500960523     C* CARICO LA L6 DELLA FILIALE GESTIONE
170600020213     C     BPES          IFNE      SAVPES
170700020213     C                   MOVEL     BPES          SAVPES
170800961209     C                   EXSR      CARL6
170900960523     C                   ENDIF
171000921013     C*
171100041012     C                   SETOFF                                         0332
171200950922     C                   CLEAR                   COMLNP
171300950922     C                   CLEAR                   COMAAS
171400950922     C                   CLEAR                   COMNSP
171500950922     C                   CLEAR                   WSPLOC
171600950927     C                   CLEAR                   WTIBOL
171700970423     C                   CLEAR                   WWFVC
171800020124     C                   CLEAR                   LNPB
171900020124     C                   CLEAR                   WTAB5F
172000020131     C                   CLEAR                   WESART
172100020131     C                   CLEAR                   WESBLT
172200020131     C                   CLEAR                   WESBTT
172300130228     c                   clear                   bolxco
172400160421     c                   clear                   bolznc
172500170509     c                   clear                   boldbr
172600021022     c*
172700021125     C* TERMINAL partenza DI BPES
172800021125     C                   MOVEL     'P'           WTITER
172900021022     C                   MOVEL     BARDFV        WDTRIF
173000021022     C                   EXSR      TERPES
173100021022     C                   Z-ADD     D55TFP        PESTFP
173200021203     c*
173300040422     c* Se trovo la bolla transito x p.o. spunta --> aggiorno transito, senz
173400021203     c*  controllo i terminal (c'e' un problema a livello di eccezioni ter
173500021203     C     KBAR37        CHAIN     FNBTT37L                           3739
173600021203     C   39              GOTO      ENDUPD
173700021203     C  N37KBTT          CHAIN     FNBTP11L                           37
173800021203     C**
173900021203    1C     *IN37         IFEQ      *OFF
174000021203     C                   MOVEL     'S'           WESBTT            1
174100021203     C                   MOVEL     BTTLNP        LNPB
174200021203     C                   MOVEL     BTPDSP        DSPB
174300021203     C                   MOVEL     BTPDSP        WDTRIF
174400021203     C                   MOVEL     BTPTFP        LNPTFP
174500050603     C                   MOVEL     BTPTFa        bolTFa
174600170509     C                   MOVE      BTPdbr        boldbr
174700040621     C                   MOVE      BTTLNP        COMLNP
174800040621     C                   MOVE      BTTAAS        COMAAS
174900040621     C                   MOVE      BTTNSP        COMNSP
175000021203   X1C                   ELSE
175100021203     C                   MOVEL     'N'           WESBTT
175200020124     C**
175300020124     C** VEDO SE TROVO LA BOLLA IN BLT
175400020124     C     KBAR          CHAIN     FNBLT27L                           3239
175500020213     C   39              GOTO      ENDUPD
175600020213     C  N32KBLT          CHAIN(N)  FNBLP01L                           32
175700020124     C**
175800021203    2C     *IN32         IFEQ      *OFF
175900020131     C                   MOVEL     'S'           WESBLT            1
176000020124     C                   MOVEL     BLTLNP        LNPB
176100020201     C                   MOVEL     BLPTFP        LNPTFP
176200050603     C                   MOVEL     BLPTFa        bolTFa
176300020201     C                   MOVEL     BLPDSP        WDTRIF
176400020214     C                   MOVEL     BLPDSP        DSPB
176500130301     c                   movel     BLPxco        bolxco            1
176600160421     c                   movel     BLPznc        bolznc            2
176700170509     C                   MOVE      BLPdbr        boldbr
176800040621     C                   MOVE      BLTLNP        COMLNP
176900040621     C                   MOVE      BLTAAS        COMAAS
177000040621     C                   MOVE      BLTNSP        COMNSP
177100021203   X2C                   ELSE
177200020213     C                   MOVEL     'N'           WESBLT
177300020124     C** SE NON TROVO LA BOLLA PARTENZA CERCO NEGLI ARRIVI
177400020124     C     KBAR          CHAIN     FNART27L                           3239
177500020213     C   39              GOTO      ENDUPD
177600050223     C  N32KARB          CHAIN(N)  FNARB01L                           32
177700020124     C**
177800021203    3C     *IN32         IFEQ      *OFF
177900020201     C                   MOVEL     'S'           WESART            1
178000020124     C                   MOVEL     ARTLNP        LNPB
178100020214     C                   MOVEL     ARBDSP        DSPB
178200020201     C                   MOVEL     ARBDSP        WDTRIF
178300020201     C                   MOVEL     ARBTFP        LNPTFP
178400050603     C                   MOVEL     ARBTFa        boltfa
178500130301     c                   movel     ARBxco        bolxco            1
178600160421     c                   movel     ARBznc        bolznc
178700170509     C                   MOVE      ARBdbr        boldbr
178800040621     C                   MOVE      ARTLNP        COMLNP
178900040621     C                   MOVE      ARTAAS        COMAAS
179000040621     C                   MOVE      ARTNSP        COMNSP
179100021203   X3C                   ELSE
179200020213     C                   MOVEL     'N'           WESART
179300020131     C** IMPOSTO LNPB = BARLNP
179400020131     C                   MOVEL     BARLNP        LNPB
179500020214     C                   Z-ADD     BARDFV        DSPB
179600050603     c                   clear                   boltfa
179700020124     C**
179800020124     C** SE NON TROVATA MAI LA BOLLA, CERCO SE ESISTE LA EVENTUALE
179900020124     C*   SPUNTA PARTENZA E LA TABELLA 5F
180000020205    4C     BARNRS        IFGT      *ZEROS
180100020124     C                   EXSR      RICLNP
180200020213    4C                   ENDIF
180300021105     c* prendo terminal di partenza della linea partenza calcolata
180400070222     c                   if        bardfv=0
180500021105     C                   Z-ADD     BARDFV        WDTRIF
180600070222     c                   else
180700070222     c                   z-add     wdate1        wdtrif
180800070222     c                   endif
180900070222     c
181000021105     C                   EXSR      TERPAR
181100020213    3C                   ENDIF
181200020213    2C                   ENDIF
181300020213    1C                   ENDIF
181400020204     C**
181500020215     C** CONTROLLO TRA P.O. SPUNTA E LINEE SPUNTE CHE AGGIONAMENTI
181600020213     C**  POSSO FARE
181700021125     c*
181800050215     C* TERMINAL arrivo DI BPES
181900021125     C                   MOVEL     'A'           WTITER
182000021125     C                   MOVEL     BARDFV        WDTRIF
182100021125     C                   EXSR      TERPES
182200021125     C                   Z-ADD     D55TFA        PESTFA
182300021125     c
182400020204     C                   EXSR      CTRAGG
182500020213     C**
182600020213     C** A G G I O R N A M E N T O   B O L L E   P A R T E N Z A
182700020213    1C     WAGPAR        IFEQ      'S'
182800021203    2C     WESblt        IFEQ      ' '
182900021203     C     KBAR          CHAIN     FNBLT27L                           3239
183000021203     C   39              GOTO      ENDUPD
183100021203     C  N32              MOVEL     'S'           WESBLT            1
183200021203     C   32              MOVEL     'N'           WESBLT            1
183300021203    2C                   ENDIF
183400021203     c**
183500021203    2C     WESBLT        ifeq      'S'
183600150817     c* Aggiorno solo se la bolla NON parte da + filiali
183700150817     c* oppure sel la bolla per LINEA parte da + filiali ma è mia bolla
183800150817     c* oppure se chi spara è la linea partenza della bolla che parte da + fil
183900150622     c                   if        wbsptp=' ' or wbsptp='L'
184000150622     c                             or bpes=lnpb
184100020213     C                   EXSR      AGGBLT
184200150622     c                   else
184300150622     c                   clear                   wagPAR
184400150622     c                   endif
184500150622     c
184600021203    3C     WEND          IFEQ      '1'
184700020213     C                   GOTO      ENDUPD
184800021203    3C                   ENDIF
184900021203    2C                   ENDIF
185000020213     C*
185100021203    2C     WESBLT        ifeq      'N'
185200020204     C                   MOVEL     'N'           WBOPAR            1
185300020204     C                   MOVEL     'P'           WTIBOL            1
185400021203    2C                   ENDIF
185500021203    1C                   ENDIF
185600020213     C*
185700020213    1C     WAGPAR        IFNE      'S'
185800020213     C     WESBLT        ANDEQ     'S'
185900020213     C                   UNLOCK    FNBLT27L
186000020213    1C                   ENDIF
186100020213     C**
186200020213     C* A G G I O R N O   B O L L E   A R R I V I
186300020213     C* A PRESCINDERE DAL FATTO CHE TROVI O MENO LA BOLLA
186400020213    1C     WAGARR        IFEQ      'S'
186500020220     C* LEGGO LE BOLLE ARRIVI ANCHE PER FLAG=6.
186600020220     C*  SI TRATTA DI SPUNTA DI DISGUIDO PER CUI NON AGGIORNEREBBE
186700020220     C*  LA BOLLA ARRIVI MA MI SERVE PER ENTRARE IN CTRA60
186800070221    1C     dls45FLG      OREQ      '6'
186900020213     C* SE NON HO ANCORA CHAINATO IL DETTAGLIO COLLI LO FACCIO ORA
187000020213    2C     WESART        IFEQ      ' '
187100020213     C     KBAR          CHAIN     FNART27L                           3239
187200020218     C   39              GOTO      ENDUPD
187300020213     C  N32              MOVEL     'S'           WESART            1
187400020214     C   32              MOVEL     'N'           WESART            1
187500020213    2C                   ENDIF
187600020213     C*
187700020213     C                   EXSR      AGGART
187800020213    1C                   ENDIF
187900020213     C*
188000020213    1C     WAGARR        IFNE      'S'
188100020213     C     WESART        ANDEQ     'S'
188200020213     C                   UNLOCK    FNART27L
188300020213    1C                   ENDIF
188400040428     c*
188500040428     c*Chiudo eventuale anomalia di disguido presente sul collo, se
188600040428     c*  di area <> dal'anomalia
188700040503     C* SOLO SE SPUNTA DI PISTOLA REALE
188800040503     c                   if        fsl(y)<>'F'
188900040428     c                   eval      comcaa=200
189000040428     C*
189100040503     C     Kanm          chain(N)  FNANM000
189200040428     c                   if        %found(fnanm02l)
189300040428     C                   CLEAR                   DSLV55
189400040428     C                   MOVEL     'P'           D55TPT
189500040428     C                   Z-ADD     anmfle        D55LIN
189600040428     C                   Z-ADD     bardfv        D55DRF
189700040428     C                   EXSR      FNLV55
189800040428     c                   if        pestfp<>d55tfp  or
189900040428     c                             bpes=anmlna
190000040428     c                   exsr      chiuan
190100040428     c                   endif
190200040428     c                   endif
190300040503     c                   endif
190400921013     C**
190500960531     C** T R A N S I T I  /  D I S G U I D I   I N   A R E A   E  N O N
190600921013     C**
190700070221    0C     dls45FLG      IFNE      '6'
190800911112     C*
190900960522     C* NON E' UNA SPUNTA DEL TERMINAL DI ARRIVO
191000050603    1C     Usalnatfa     IFNE      BPES
191100021022     c     wesbtt        oreq      'S'
191200960522     C*
191300960522     C* SE NEMMENO I 2 TERMINAL DI ARRIVO CORRISPONDONO
191400960522     C* --> T R A N S I T O
191500050603    2C     Usalnatfa     IFNE      PESTFA
191600021022     c     wesbtt        oreq      'S'
191700960531     C**
191800020214     C** I TRANSITI SONO ANCORA DA SISTEMARE
191900020214     C**  QUESTE 2 SPECIFICHE SONO DA ELIMINARE: INFATTI LA BOLLA POTRA
192000020214     C**  ESSERE SIA IN BLT CHE IN BTT
192100020205    3C     WAGPAR        IFEQ      ' '
192200960703     C     WBOPAR        OREQ      'N'
192300960522     C**
192400040422     C     WESBTT        ifeq      'N'
192500960521     C**
192600960612     C* SE E' SPUNTA ENTRATA O PARTENZA E NON HO TROVATO NE' LA BOLLA
192700960521     C*  PARTENZA NE' LA BOLLA TRANSITO, PER SERIE =0
192800960521     C*  IMPOSTO NELLA SPUNTA FLAG TRASMISSIONE "N" E 99999999
192900960612     C*  PER NON AUTOGENERARE LA SPUNTA PARTENZA O NON TRASMETTERLA
193000001123     C* IDEM PER SPUNTA IMI (PER BOLLE POSTE AUTOGENERA ANCHE DA SPUNTE
193100001123     C* IMI)
193200960522    5C     BARNPG        IFEQ      5
193300020205     C     BARNPG        OREQ      1
193400001123     C     BARNPG        OREQ      3
193500001123     C     BARSPG        ANDEQ     'I'
193600020205    6C     BARNRS        IFEQ      0
193700020205     C     WAGPAR        ANDEQ     'S'
193800960521     C     WBOPAR        ANDEQ     'N'
193900960521     C                   MOVEL     'S'           WNOTRA            1
194000020205    6C                   ENDIF
194100960612    5C                   ENDIF
194200040120     c
194300911016     C*
194400110124     C* SE NON E' BOLLA LOCALE e nemmeno in £6 : DISGUIDO
194500150817     c*  non è disguido nemmeno se collo che parte da altra filiale
194600150817     c*  ( la bolla deve essere spostata di lnp)
194700020205    5C     WAGPAR        IFEQ      ' '
194800110124    5C     WAGarr        andeq     ' '
194900150817    5C     WBSPSPU       andeq     ' '
195000150817     c
195100020215     C     BARFLE        IFEQ      BTFP
195200960522     C                   Z-ADD     200           COMCAA
195300960404     C  N05              EXSR      CRTANM
195400020215     C                   ENDIF
195500020215     C* DEVO COMUNQUE DIRE CHE SI TRATTA DI UN DISGUIDO
195600960404     C                   MOVEL     'D'           WTIBOL
195700041012     c
195800041012     c* Aggiorno anche la bolla in  arrivo
195801180216     c                   eval      wdarr = 0
195900041012     c                   EXSR      AGGARRIVO
196000960522    5C                   ENDIF
196100960522     C**
196200911016     C* 37 OFF - TROVATA AGGIORNO BOLLA TRANSITO
196300960522   X4C                   ELSE
19640002021418   C                   EXSR      AGGBTT
196500960522    4C                   ENDIF
196600040430     c*
196700040430     c* Eventuale spunta di disguido o transito, chiude fuori posto
196800040430     c*  di bolla partenza
196900040430     C                   Z-ADD     120           COMCAA
197000040430     C                   EXSR      CHIUAN
197100970204     C*
197200970204   X3C                   ELSE
197300970204     C* SE NON E' COLLO DELLA AREA DI ARRIVO E COLLO IN PARTENZA
197400970204     C*  VEDO SE CREARE L'ANOMALIA FUORI POSTO BOLLA IN PARTENZA
197500970204     C                   EXSR      ANM120
197600990120     C** VERIFICO LA PRESENZA DI UNA C.A. PER CREARE EVENTUALI ANOM
197700000523     C** IGNORO SE E' SPUNTA CAT 4-8 SOLO IN ARRIVO(IN PART.SEMPRE)
197800990121     C                   MOVEL     'S'           WCAT4             1
197900990126     C*     GESTISCO IL RITROVAMENTO COLLO
198000990126     C                   MOVEL     'S'           WRITRO            1
198100990126     C*     GESTISCO LA CREAZIONE ANOMALIE
198200990126     C                   MOVEL     'S'           WCREAN            1
198300990120     C                   EXSR      CTRDAN
198400990120     C**
198500990120     C** SE NON TROVATA CA DI COLLO
198600990120     C* VERIFICO SE LA SPEDIZIONE E' SU UNA CA CHIUSA --> CREO
198700990120    4C     WTRDAN        IFEQ      ' '
198800040811      *
198900040811      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
199000040811      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
199100040811      * legate alla spedizione che passo
199200040811     c                   clear                   fidn12ds
199300170505     c***                eval      i12aas = BLTAAS
199400170505     c***                eval      i12lnp = BLTLNP
199500170505     c***                eval      i12nrs = BLTNRS
199600170505     c***                eval      i12nsp = BLTNSP
199700170505     c* per evitare che i campi siamo sporchi, uso i salvati
199800170505     c                   eval      i12aas = comAAS
199900170505     c                   eval      i12lnp = comLNP
200000170505     c                   eval      i12nrs = barNRS
200100170505     c                   eval      i12nsp = comNSP
200200170505     c                   eval      i12fel = 'E'
200300040811     c*
200400040811     c                   call      'FIDN12R'
200500040811     c                   parm                    fidn12ds
200600040811     c*
200700040811     c                   setoff                                       33
200800040811     c*
200900040811     c* se non ci sono errori
201000040811     c                   if        o12err <> ' '
201100040811     c                   seton                                        33
201200040811     c                   else
201300040811     c* se numero di CA trovate maggiore di zero
201400040811     c                   if        o12nca = 0
201500040811     c                   seton                                        33
201600040811     c                   else
201700040811     c                   z-add     1             jj                2 0
201800040811     c                   dow       jj <= o12nca and *in33 = *off
201900040811     C     skDch(jj)     IFGT      0
202000040811     C*
202100040811     C* Valorizzo i campi DCT* utilizzati nella routine CRE151 con quelli
202200040811     C* ritornati dal *pgm d utilità FIDN12R
202300040811     C                   movel     skCch(jj)     DCTCCH
202400040811     C*
202500040811     C                   EXSR      CRE151
202600040811     C                   SETON                                        33
202700040811     C                   ENDIF
202800040811     c*
202900040811     c                   add       1             jj
203000040811     c                   enddo
203100040811     c*
203200040811     c                   seton                                        33
203300040811     c*
203400040811     c                   endif
203500040811     c                   endif
203600040811     C**
203700990120    4C                   ENDIF
203800990120     C**
203900960531    3C                   ENDIF
204000960522     C*
204100960531   X2C                   ELSE
204200970203     C*  D I S G U I D O   I N   A R E A
204300970203     C*  (SE NON E' UNA MIA BOLLA PARTENZA)
204400961021     C                   SETOFF                                       88
204500961122     C                   CLEAR                   WSTESS
204600961028     C*
204700020213    3C  N05BPES          IFNE      BARLNA
204800050930     c* al posto di BPES uso barfgs per le spunte IMP: mi fa più comodo
204900050930     c*  usare direttamente il terminal di partenza di chi spara perchè
205000050930     c*  l'IMP lo fa solo il terminal di partenza ma può essere usato
205100050930     c*  anche dai 2 livello
205200050930     C     barfgs        ANDNE     LNPB
205300960522     C     BARNPG        ANDGT     1
205400050930     C     BARNPG        ANDne     5
205500050930     c* Se spunta entrata escludo se di tratta di un aggiornamento
205600050930     c*  in partenza e non in arrivo (per cui il p.o. fa parte
205700050930     c*  dell'area di partenza
205800050930    3C     BPES          orne      BARLNA
205900050930     C     barfgs        ANDNE     LNPB
206000050930     C     BARNPG        ANDeq     5
206100050930     c     wagarr        andeq     'S'
206200050930     c
206300980727     C** CREO ANOMALIE SE COLLO SPUNTATO IN IMA ANCHE SE DA ME CONFERMA
206400020213    3C     BPES          ORNE      BARLNA
206500020213     C     BPES          ANDEQ     LNPB
206600980727     C     BARNPG        ANDEQ     3
206700980727     C     BARSPG        ANDEQ     'A'
206800960523     C*
206900020213     C* NON DEVE ESSERE PRESENTE NEMMENO IN L6 DEL P.O. SPUNTA
207000960523     C     BARLNA        LOOKUP    L6                                     88
207100980127     C**
207200960531    4C     *IN88         IFEQ      *OFF
207300961202     C                   Z-ADD     146           CAACRE
207400961202     C                   Z-ADD     145           CAACIU
207500961202     C                   EXSR      ANM14X
207600960531    4C                   ENDIF
207700960531    3C                   ENDIF
207800960522     C*
207900960523     C* ALTRIMENTI  LA CHIUDO
208000960531    3C     *IN88         IFEQ      *ON
208100960523     C     BARNPG        OREQ      1
208200020213     C     BPES          OREQ      BARLNA
208300961122     C*
208400000523     C* SE CHI SPARA E' LA FILIALE FINALE DI ARRIVO, ME LO MEMORIZZO
208500961122     C*  MI SERVE PER LA CHIUSURA
208600961122    4C     *IN88         IFEQ      *ON
208700020213     C     BPES          OREQ      BARLNA
208800961122     C                   MOVEL     'S'           WSTESS
208900961122    4C                   ENDIF
209000961122     C**
209100960522     C                   Z-ADD     146           COMCAA
209200960522     C                   EXSR      CHIUAN
209300961122     C*
209400960522     C                   Z-ADD     145           COMCAA
209500960522     C                   EXSR      CHIUAN
209600970204     C*
209700970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
209800970204     C                   EXSR      ANMARR
209900960531    3C                   ENDIF
210000961021     C*
210100970203     C* CREO E/O CHIUDO LE ANOMALIE CHE RIGUARDANO L'AREA DI ARRIVO
210200970203     C                   EXSR      ANMSEM
210300960531    2C                   ENDIF
210400911016     C*
210500960531   X1C                   ELSE
210600970203     C* T E R M I N A L   D I   A R R I V O
210700961122     C                   SETOFF                                       88
210800961122     C                   CLEAR                   WSTESS
210900970203     C*
211000970203     C* CREO E/O CHIUDO LE ANOMALIE CHE RIGUARDANO L'AREA DI ARRIVO
211100970203     C                   EXSR      ANMSEM
211200970204     C*
211300970204     C* CHIUDO EVENTUALE ANOMALIA BOLLA ARRIVI
211400970204    2C     BARNPG        IFEQ      2
211500970204     C                   Z-ADD     130           COMCAA
211600970204     C                   EXSR      CHIUAN
211700970204    2C                   ENDIF
211800940330     C*
211900940330     C* SONO TERMINAL DI ARRIVO-ANOMALIA:INOLTRARE COLLO ALL'ARRIVO
212000960531    2C     WTIBOL        IFNE      'A'
212100020218    2C     WTIBOL        ANDNE     'L'
212200960522     C                   MOVEL     'R'           WTIBOL
212300960531    2C                   ENDIF
212400960522     C*
212500960522     C* SOLO SE NON E' UN MIO COLLO
212600020213    2C  N05BPES          IFNE      BARLNA
212700091211     c
212800091211     C* CARICO LA L6 DELLA FILIALE GESTIONE
212900091211     C     BPES          IFNE      SAVPES
213000091211     C                   MOVEL     BPES          SAVPES
213100091211     C                   EXSR      CARL6
213200091211     C                   ENDIF
213300091211     c
213400960523     C* NON DEVE ESSERE PRESENTE NEMMENO IN L6
213500960531     C     BARLNA        LOOKUP    L6                                     88
213600960531    3C     *IN88         IFEQ      *OFF
213700960523     C*
213800960531    4C     BARNPG        IFGT      2
213900960522     C     BARNPG        ANDLT     5
214000000523     C     BARNPG        OREQ      8
214100961202     C                   Z-ADD     145           CAACRE
214200961202     C                   Z-ADD     146           CAACIU
214300961202     C                   EXSR      ANM14X
214400960522     C*
214500960531   X4C                   ELSE
214600961211     C* SE TROVO LA 145 E LA POSSO CHIUDERE CREO SE POSSO LA 620
214700961211     C*  SE NON TROVO LA 145 DEMANDO L'EVENTUALE CREAZIONE DELLA 620
214800961211     C*  ALLA CHIUSURA DELLA EVENTUALE 146, CHE POTREBBE ANCHE NON
214900961211     C*  ESSERCI. SIA CHE CI SIA DA CHIUDERE CHE NON CI SIA, A QUESTO
215000961211     C*  PUNTO LA 620 LA CREO
215100960522     C                   Z-ADD     145           COMCAA
215200961211     C* SOLO SE ESISTE L'ANOMALIA CREO LA 620
215300961211     C                   MOVEL     'S'           WESAN             1
215400961211     C*
215500960522     C                   EXSR      CHIUAN
215600961211     C                   CLEAR                   WESAN
215700961029     C* SE C'E' CHIUDO ANCHE IL DISGUIDO IN AREA
215800961029     C                   Z-ADD     146           COMCAA
215900961211     C*
216000961211     C   97              MOVEL     'S'           W620
216100961211     C  N97              MOVEL     'N'           W620
216200961029     C                   EXSR      CHIUAN
216300961211     C                   MOVEL     'S'           W620
216400960523    4C                   ENDIF
216500961021   X3C                   ELSE
216600961122     C*
216700020213     C* SE CHI SPARA E' LA FILIALE FINALE DI ARRIVO, ME LO MEMORIZZO
216800961122     C*  MI SERVE PER LA CHIUSURA
216900961122     C                   MOVEL     'S'           WSTESS            1
217000961021     C                   Z-ADD     146           COMCAA
217100961021     C                   EXSR      CHIUAN
217200970204     C*
217300970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
217400970204     C                   EXSR      ANMARR
217500960522    3C                   ENDIF
217600961122     C**
217700961021   X2C                   ELSE
217800970204     C* CHI SPARA E' LA FILIALE FINALE DI ARRIVO
217900961122     C                   MOVEL     'S'           WSTESS
218000961021     C                   Z-ADD     146           COMCAA
218100961021     C                   EXSR      CHIUAN
218200970204     C*
218300970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
218400970204     C                   EXSR      ANMARR
218500960523    2C                   ENDIF
218600911016     C*
218700950922    1C                   END
218800961213    0C                   ENDIF
218900021022     c
219000021203     c                   unlock    fnbtt37l
219100970203     C**
219200970203     C** SE FLAG = 2 MODIFICO ANOMALIE METTENDO NUOVO NUMERO DI FOGLIO
219300970203     C**
219400970203     C     *IN05         IFEQ      *ON
219500970203     C*
219600921021     C                   EXSR      RICFAS
219700921021     C*
219800990518     C     KBAR          SETLL     FNANM000
219900990518     C     KBAR          READE     FNANM000                               99
220000921021     C*
220100960423    2C     *IN99         DOWEQ     '0'
220200070208    3C     ANMNFV        IFEQ      DLS45NFVO
220300000516     C* SE NON C'E' SOTTO CATEGORIA DEVONO ESSERE UGUALI
220400000516    4C     BARSPG        IFEQ      ' '
220500921021     C     ANMFSA        ANDEQ     COMFAS
220600000516     C* SE     C'E' SOTTO CATEGORIA, DEVE ESSERE UGUALE AD UNA DELLE
220700000516     C*  TANTE
220800000516     C     ANMFSA        OREQ      WFAS1
220900000516     C     ANMFSA        OREQ      WFAS2
221000000516     C     ANMFSA        OREQ      WFAS3
221100000516     C     ANMFSA        OREQ      WFAS4
221200000516     C     ANMFSA        OREQ      WFAS5
221300921021     C                   Z-ADD     BARNFV        ANMNFV
221400960524     C                   Z-ADD     BARDFV        ANMDAO
221500000516     C                   MOVEL     COMFAS        ANMFSA
221600990518     C                   UPDATE    FNANM000
221700000516    4C                   ENDIF
221800000516    3C                   ENDIF
221900921021     C*
222000990518     C     KBAR          READE     FNANM000                               99
222100000516    2C                   ENDDO
222200920424     C*
222300920424     C                   Z-ADD     3             B
222400021107    1C                   END
222500920424     C     ENDUPD        TAG
222600000209     C*
222700110907    1C     *IN39         IFEQ      *ON
222800000209     C                   SETON                                        40
222900110907   x1C                   ELSE
223000000209     C*
223100000210     C* ELABORO SOLO SPUNTA DA MACCHINONE
223200110907     c*  non lo faccio se modifica di numero foglio da video
223300110907    2c  N05RPS(Y)        ifne      'A'
223400000209     C* AGGIORNO FILE  FISGN SE C'E'
223500000209     C     KBAR          CHAIN     FISGN03L                           33
223600110907    3C     *IN33         DOWEQ     *OFF
223700110907    4C     SGNST2        IFLT      9
223800000209     C                   ADD       1             SGNST2
223900001228     C                   CLEAR                   SGNT6A
224000001228     C                   CLEAR                   SGNT6B
224100001228     C                   CLEAR                   SGNT6C
224200001228     C                   CLEAR                   SGNT6D
224300001228     C                   CLEAR                   SGNT6E
224400001228     C                   CLEAR                   SGNT6F
224500080624     c                   eval      sgndatora = %char(%timestamp:*iso0)
224600000209     C                   UPDATE    FISGN000
224700110907    4C                   ENDIF
224800001020     C     KBAR          READE     FISGN03L                               33
224900110907    3C                   ENDDO
225000110907     c
225100110907     c* Una qualsiasi spunta REALE chiude eventuale anomalia interna
225200110907     c*  115 - COLLO MAI AFFIDATO
225300110929     C                   MOVEL     'AR'          SAVCCH
225400110907     C                   Z-ADD     115           COMCAA
225500110907     C                   EXSR      CHIUAN
225600110907     c
225700110907     c
225800110907    2C                   ENDIF
225900110907    1C                   ENDIF
226000941206     C*
226100020215     C                   ENDSR
226200961118     C**************************************************************************
226300961205     C* CREO ANOMALIA 600 PER L'EXPORT   COLLO A TERRA
226400961118     C**************************************************************************
226500961118     C     ANM600        BEGSR
226600961210     C*
226700961210     C* PER ESTERO CREO ANOMALIA 645 E 600
226800020212    7C     NTWBAR        IFEQ      'EEX'
226900060322    7C     NTWBAR        oreq      'DPD'
227000060322    7C     NTWBAR        oreq      'FED'
227100961210     C                   Z-ADD     645           COMCAA
227200961210     C                   EXSR      CRTANM
227300961210     C*
227400961210    8C     W60X          IFEQ      'S'
227500961209     C* SE ESISTE LA 620  LA DELETO
227600961118     C                   Z-ADD     620           COMCAA
227700001110     C     COMCAA        LOOKUP    ACG                                    34
227800001110     C     BARSPG        IFNE      'I'
227900001110     C     *IN34         OREQ      *ON
228000990518     C     KANM          DELETE    FNANM02L                           34
228100001110     C                   ENDIF
228200961118     C                   Z-ADD     600           COMCAA
228300961118     C                   EXSR      CRTANM
228400961210     C                   CLEAR                   W60X
228500961210    8C                   ENDIF
228600961210    7C                   ENDIF
228700961210     C                   ENDSR
228800960702     C**************************************************************************
228900960702     C* CERCO LNP BOLLA SE COLLO CON SERIE > 0
229000960702     C**************************************************************************
229100960702     C     RICLNP        BEGSR
229200970814     C                   SETOFF                                       33
229300960702     C*
229400020201     C* CONTROLLO SE COLLO CONFERMABILE DAL TERMINAL DEL P.O. EFFETTUA
229500020201     C*  SPUNTA
229600020201     C                   MOVEL     PESTFP        W008A             8
229700960702     C                   MOVE      BARNRS        W005A             5
229800960702     C                   MOVEL     BARLNP        W005A
229900960702     C                   MOVE      W005A         W008A
230000990108     C                   Z-ADD     1             YY                4 0
230100970828     C     W008A         LOOKUP    S5F(YY)                                33
230200020131     C**
230300020131     C* SE NON TROVATO CERCO COME SERIE = 99
230400020131     C     *IN33         IFEQ      *OFF
230500020131     C                   MOVE      99            W008A
230600020131     C                   Z-ADD     1             YY                4 0
230700020131     C     W008A         LOOKUP    S5F(YY)                                33
230800020131     C                   ENDIF
230900020131     C**
231000020131    1C     *IN33         IFEQ      *ON
231100020131     C                   MOVEL     'S'           WTAB5F            1
231200020131     C* CERCO SPUNTA PARTENZA SE C'E' PRENDO LA FGS DI QUEST'ULTIMA
231300020201     C*  E LA DATA DI RIFERIMENTO E' LA DATA DI QUESTA SPUNTA
231400960702     C                   Z-ADD     1             WNPG
231500070131     C     KBAR3         SETLL     FNBRV09L
231600070131     C     KBAR3         READE     FNBRV09L                               33
231700020131    2C     *IN33         DOWEQ     *OFF
231800040303    3C     BR9ATR        IFEQ      *BLANKS
231900040323     c* Escludo le defluenze e le spunte di simfel(potrebbe quindi essere co
232000040323     c*  lo in transito da me e quindi non partito da me)
232100040323    4C     BR9KEY        IFNE      SA1KEY
232200040323     C                   MOVEL     BR9KEY        SA1KEY
232300040323     C                   Z-ADD     BR9NPG        WNPG
232400040323     C                   Z-ADD     BR9NFV        WNFV
232500040323     C                   Z-ADD     BR9FGS        WFGS
232600070906     C                   movel     BR9nps        Wnps
232700040323     C                   EXSR      RILFOG
232800040323     c                   Z-ADD     WDFV          BRVDFV
232900140616     c                   movel     WFCF          BRVFCF
233000140616     c                   movel     Wspg          BRVspg
233100040323    5C     WTTR          IFNE      *BLANKS
233200040323     C     WTTR          LOOKUP    TTR                                    34
233300040323     C                   MOVE      *IN34         W9DEFL            1
233400040323     C                   ELSE
233500040323     C                   MOVE      '0'           W9DEFL            1
233600040323    5C                   ENDIF
233700040323    4C                   ENDIF
233800040323    4c                   if        w9defl='0' and br9fgs<>simfel
233900110103     c*
234000040303     C                   MOVE      BR9PES        LNPB
234100970814     C                   CLEAR                   WTAB5F
234200020214     C                   Z-ADD     BRVDFV        DSPB
234300020214     C                   Z-ADD     BRVDFV        WDTRIF
234400960702     C                   SETON                                        33
234500020201    4C                   END
234600020131    3C                   END
234700070131     C  N33KBAR3         READE     FNBRV09L                               33
234800020131    2C                   ENDDO
234900020204     C**
235000020131    2C     WTAB5F        IFEQ      'S'
235100020218     C* CERCO SE C'E' IL P.O. CHE HA SPUNTATO, ALTRIMENTI IL PRIMO
235200020218     C                   MOVEA     F5F(YY)       WPO
235300020218     C                   Z-ADD     1             ZZ
235400020218     C                   CLEAR                   WTROVA
235500020218    3C                   DO        4             ZZ                3 0
235600020218    4C     WPO(ZZ)       IFGT      *ZEROS
235700020218     C                   MOVEL     WPO(ZZ)       W0030             3 0
235800020218    5C     W0030         IFEQ      BPES
235900020218     C                   MOVEL     W0030         LNPB
236000020218     C                   MOVEL     'S'           WTROVA
236100020218    5C                   ENDIF
236200020218    4C                   ENDIF
236300020218    3C                   ENDDO
236400020218     C**
236500020218     C* SE NON C'E' P.O. = BPES PRENDO IL PRIMO
236600020218     C     WTROVA        IFEQ      ' '
236700020218     C                   MOVEL     WPO(1)        LNPB
236800020218     C                   ENDIF
236900020214     C                   MOVEL     BARDFV        DSPB
237000020214     C                   MOVEL     BARDFV        WDTRIF
237100020131    2C                   ENDIF
237200970814    1C                   ENDIF
237300960702     C                   ENDSR
237400941206     C**************************************************************************
237500941206     C* CERCO IL RAGGRUPPAMENTO PISTOLA
237600941206     C**************************************************************************
237700921229     C     RICRAG        BEGSR
237800921229     C*
237900930305     C     SAVNPS        IFNE      ALF2
238000921229     C*
238100950515     C* HO CARICATO TUTTA SCHIERA
238200921229     C     *IN02         IFEQ      '0'
238300970417     C                   Z-ADD     1             Z                 3 0
238400930305     C                   MOVEL     ALF2          NUM2              2 0
238500970417     C     NUM2          LOOKUP    NPS(Z)                                 34
238600970417     C  N34              Z-ADD     1             Z
238700921229     C*
238800921229     C                   ELSE
238900921229     C*
239000921229     C* NON HO CARICATO LA SCHIERA --> FACCIO CHAIN
239100921229     C                   MOVEL     '7J'          COD
239200921229     C                   MOVEL     *BLANKS       KEY
239300930305     C                   MOVEL     ALF2          KEY
239400921229     C     KTAB          CHAIN     TABEL                              34
239500001023     C                   Z-ADD     2             Z
239600000316     C     *IN34         IFEQ      *OFF
239700000316     C                   MOVEL     TBLUNI        DS7J
239800000316     C                   MOVEL     §7JRPS        RPS(Z)
239900000316     C                   MOVEL     §7JORD        OPS(Z)
240000000316     C                   MOVEL     §7JTPS        TPS(Z)
240100000316     C                   MOVEL     §7JINV        INV(Z)
240200040503     C                   MOVEL     §7Jfsl        fsl(Z)
240300000316     C                   ELSE
240400000316     C                   MOVEL     'M'           RPS(Z)
240500000316     C                   MOVEL     999           OPS(Z)
240600000316     C                   MOVEL     'B'           TPS(Z)
240700000316     C                   MOVEL     ' '           INV(Z)
240800040503     C                   MOVEL     ' '           fsl(Z)
240900000316    1C                   END
241000921229     C                   END
241100921229     C*
241200930305     C                   MOVEL     ALF2          SAVNPS
241300921229     C                   END
241400941206     C*
241500921229     C                   ENDSR
241600941206     C**************************************************************************
241700941206     C* CHIUDO ANOMALIA
241800941206     C**************************************************************************
241900911018     C     CHIUAN        BEGSR
242000961211     C                   SETOFF                                       97
242100941206     C*
242200990518     C     KANM          CHAIN(N)  FNANM000                           97
242300961121     C**
242400961211    1C     *IN97         IFEQ      *OFF
242500970117     C**
242600970117     C** ANOMALIA 145/146: SE DATA SPUNTA > DATA ANOAMLIA
242700970117     C*  DEVO CONTROLLARE CHE NON CI SIANO ALTRE SPUNTE
242800970117     C*  CHE MI IMPEDISCONO LA CHIUSURA
242900970117     C     ANMCAA        IFEQ      145
243000970117     C     ANMCAA        OREQ      146
243100041013     c* Imposto econtrdfv per anomalie 145 146 che la deve sempre cotrollare
243200041013     c                   eval      wcontrdfv='S'
243300990518     C     BARDFV        IFGT      ANMDAO
243400970117     C                   EXSR      CTRSPU
243500970117     C                   ENDIF
243600970117     C                   ENDIF
243700961121     C**
243800041013     c     Wcontrdfv     ifne      'S'
243900041013     C* PER CATEGORIA no arrivi e partenza CHIUDO SE DATA >=
244000961122     C     WSTESS        OREQ      'S'
244100990518     C     BARDFV        ANDEQ     ANMDAO
244200041013     C     BARNPG        ANDne     2
244300041013     C     BARNPG        ANDne     1
244400041013     C* PER LE CATEGORIE arrivi SOLO SE DATA > e la posso chiudere
244500990518     C     BARDFV        ORGT      ANMDAO
244600970117     C     WCHIU         ANDEQ     'S'
244700961211     C*
244800961211     C     WCREA         OREQ      'N'
244900020213     C     BARFLE        ANDNE     BTFP
245000961121     C**
245100960416     C                   CLEAR                   DSLR33
245200960524     C                   MOVE      BARDFV        D33DCH
245300960514    2C     BARKEY        IFNE      SAVFOG
245400911125     C                   EXSR      RICFAS
245500960514    2C                   END
245600960514     C*
245700960416     C                   MOVEL     COMFAS        D33FSC
245800960416     C                   MOVEL     SAVCCH        D33CCH
245900960530     C                   MOVEL     SAVMAC        D33MAC
246000960416     C                   MOVE      ANMNR2        D33NRR
246100960416     C*
246200990712     C                   CALL      'FNLR33R'
246300960416     C                   PARM                    DSLR33
246400961209     C*
246500961209     C* PER ANOMALIE 145 E 146 CREO SE E' DA CREARE L'ANOMALIA 620
246600020212     C*  AOLO SE NON SONO IN CREAZIONE DELL 600
246700020212    2C     NTWBAR        IFEQ      'EEX'
246800961210     C     W620          ANDEQ     'S'
246900060322    2C     NTWBAR        oreq      'DPD'
247000060322     C     W620          ANDEQ     'S'
247100060322    2C     NTWBAR        oreq      'FED'
247200060322     C     W620          ANDEQ     'S'
247300961210     C*
247400961210    3C     ANMCAA        IFEQ      145
247500961209     C     ANMCAA        OREQ      146
247600961210     C*
247700961210    4C     W60X          IFEQ      'S'
247800961210     C                   Z-ADD     600           COMCAA
247900001110     C     COMCAA        LOOKUP    ACG                                    34
248000001110    5C     BARSPG        IFNE      'I'
248100001110     C     *IN34         OREQ      *ON
248200990518     C     KANM          DELETE    FNANM02L                           34
248300001110    5C                   ENDIF
248400961209     C                   Z-ADD     620           COMCAA
248500961209     C                   EXSR      CRTANM
248600961209     C                   CLEAR                   W60X
248700961210    4C                   ENDIF
248800961209     C*
248900961209     C* VEDO SE CANCELLARE L'ANOMALIA 645
249000961209     C                   Z-ADD     645           COMCAA
249100001110     C     COMCAA        LOOKUP    ACG                                    34
249200001110   4AC     BARSPG        IFNE      'I'
249300001110     C     *IN34         OREQ      *ON
249400990518     C     KANM          CHAIN     FNANM02L                           34
249500961210    4C     *IN34         DOWEQ     *OFF
249600990518    5C     ANMDAO        IFEQ      BARDFV
249700990518     C                   DELETE    FNANM000
249800961209     C                   SETON                                        34
249900961209     C                   ELSE
250000990518     C     KANM          READE     FNANM02L                               34
250100961210    5C                   ENDIF
250200961210    4C                   ENDDO
250300001110   4AC                   ENDIF
250400961210    3C                   ENDIF
250500961209     C*
250600961210    2C                   ENDIF
250700960514     C*
250800960514     C* L'ANOMALIA 05 PUO' ESSERCI PIU' VOLTE
250900961209    2C     ANMCAA        IFEQ      005
251000980709     C                   SETOFF                                       34
251100960514    3C     *IN34         DOWEQ     *OFF
251200990518     C     KANM          READE(N)  FNANM000                               34
251300960514     C                   MOVE      ANMNR2        D33NRR
251400960514     C*
251500960514     C  N34              CALL      'FNLR33R'
251600960514     C                   PARM                    DSLR33
251700960514    3C                   ENDDO
251800911018     C*
251900960514    2C                   ENDIF
252000961121   1AC                   ENDIF
252100961209   X1C                   ELSE
252200961209     C*
252300961211     C* SOLO PER ANOMALIE 145 E 146
252400961210    2C     COMCAA        IFEQ      145
252500961210     C     COMCAA        OREQ      146
252600991230     C* SE DEVO CREARE EVENTUALE 620 PER LNA ESTERA
252700020212    3C     W60X          IFEQ      'S'
252800961210     C     W620          ANDEQ     'S'
252900060322     C* E LA DEVO CREARE SIA SE TROVO CHE SE NON TROVO L'ANOMALIA 145
253000060322     C*  O 146 DA CHIUDERE  (WESAN =' ')
253100060322     C     WESAN         ANDEQ     ' '
253200110103     c*
253300060322    4C     NTWBAR        ifeq      'EEX'
253400060322     C     NTWBAR        oreq      'DPD'
253500060322     C     NTWBAR        oreq      'EEX'
253600961209     C                   Z-ADD     620           COMCAA
253700961209     C                   EXSR      CRTANM
253800961209     C                   CLEAR                   W60X
253900060322    4C                   ENDIF
254000060322    3C                   ENDIF
254100961210    2C                   ENDIF
254200961210    1C                   ENDIF
254300110103     c*
254400041013     c                   eval      wcontrdfv='N'
254500911018     C*
254600110929     c                   clear                   savmac
254700110929     c                   clear                   savcch
254800911018     C                   ENDSR
254900941206     C**************************************************************************
255000941206     C*    RICERCA FASE
255100941206     C**************************************************************************
255200911125     C     RICFAS        BEGSR
255300941206     C*
255400921020     C                   MOVEL     '7N'          COD
255500921020     C                   MOVEL     *BLANKS       KEY
255600921020     C                   MOVEL     BARNPG        KEY
255700921105     C     KTAB          CHAIN     TABEL                              41
255800921105     C  N41              MOVEL     TBLUNI        DS7N
255900000516     C*
256000000516     C     BARSPG        IFNE      ' '
256100000516     C                   MOVEL     §7NFS1        WFAS1             1
256200000516     C                   MOVEL     §7NFS2        WFAS2             1
256300000516     C                   MOVEL     §7NFS3        WFAS3             1
256400000516     C                   MOVEL     §7NFS4        WFAS4             1
256500000516     C                   MOVEL     §7NFS5        WFAS5             1
256600000516     C                   ENDIF
256700921021     C*
256800960524     C     BARSPG        IFEQ      §7NSC1
256900921021     C                   MOVEL     §7NFS1        COMFAS            1
257000921021     C                   ELSE
257100960524     C     BARSPG        IFEQ      §7NSC2
257200921021     C                   MOVEL     §7NFS2        COMFAS
257300921021     C                   ELSE
257400960524     C     BARSPG        IFEQ      §7NSC3
257500921021     C                   MOVEL     §7NFS3        COMFAS
257600921021     C                   ELSE
257700960524     C     BARSPG        IFEQ      §7NSC4
257800921021     C                   MOVEL     §7NFS4        COMFAS
257900921021     C                   ELSE
258000921021     C                   MOVEL     §7NFS5        COMFAS
258100921021     C                   END
258200921021     C                   END
258300921021     C                   END
258400921021     C                   END
258500921020     C*
258600961217     C                   MOVEL     BARKEY        SAVFOG            8 0
258700941206     C*
258800911125     C                   ENDSR
258900950927     C**************************************************************************
259000950927     C*    CONTROLLO SE CREARE ANOMALIA 50: COLLO ARRIVATO NON PARTITO
259100130228     C*                      O ANOMALIA 55/56/57: DISGUIDO - ESTERNA
259200950927     C**************************************************************************
259300950927     C     CRTA50        BEGSR
259400000320     C                   CLEAR                   WCCH
259500020214     C* PER ANOMALIA 50 NON CREO SE ESISTE GIA' LA DATA DI PARTENZA
259600020214     C*  BOLLE TRANSITO
259700020214     C     WTIBOL        IFEQ      'T'
259800020214     C     COMDFV        ANDGT     0
259900020214     C                   GOTO      NOA50
260000020214     C                   ENDIF
260100020214     C**
260200020214     C     WTIBOL        IFEQ      'A'
260300020218     C     WTIBOL        OREQ      'L'
260400050215     c* Per l'anoamlia 50 basterebbe questo test senza leggere le spunte,
260500050215     c*  le spunte le devo leggere lo stesso per i manca record
260600020214     C     WESART        IFEQ      'S'
260700020214     C     COMDFV        ANDGT     0
260800020214     C                   GOTO      NOA50
260900020214     C                   ENDIF
261000020214     C* SE E' COLLO IN ARRIVO E BOLLA CONSEGNATA --> NON CREO
261100020214     C     WESART        IFEQ      'S'
261200020214     C     ARTDCM        ANDGT     0
261300020214     C                   GOTO      NOA50
261400020214     C                   ENDIF
261500020218     C* SE BOLLA LOCALE E NON C'E' LA BOLLA NON FACCIO NIENTE
261600020218     C     WTIBOL        IFEQ      'L'
261700020218     C     WBOPAR        ANDEQ     'N'
261800020218     C                   GOTO      NOA50
261900020218     C                   ENDIF
262000020214     C                   ENDIF
262100020214     C**
262200950927     C* VERIFICO SE ESISTE GIA'
262300040429     c* ora la esistenza è all'interno dello stesso p.o. e non in assoluto
262400950927    1C     WTIBOL        IFEQ      'D'
262500950927     C                   Z-ADD     55            COMCAA
262600950927     C                   ELSE
262700950927     C                   Z-ADD     50            COMCAA
262800950927    1C                   ENDIF
262900990409     C* SE LA CATEGORIA E' IMA IMG O CONS INVECE CHE 55 CREO LA 56
263000020214    1C     COMCAA        IFEQ      55
263100020214    2C     BARNPG        IFEQ      4
263200990412     C     BARNPG        OREQ      3
263300990409     C     BARSPG        ANDEQ     'A'
263400990412     C     BARNPG        OREQ      3
263500990409     C     BARSPG        ANDEQ     'G'
263600161021     C     BARNPG        OREQ      3
263700161021     C     BARSPG        ANDEQ     'Z'
263800990409     C                   Z-ADD     56            COMCAA
263900020214    2C                   ENDIF
264000020214    1C                   ENDIF
264100130228     c* se Anomalia 55 a collo che può partire da diverse filiali
264200130228     c*  e chi spunta è una di qeuste --> non creo 55 ma 57 Disguido AUTORIZZ
264300130228     c*  solo per ategorie di spunta partenza
264400130228     c                   if        comcaa=55  and
264500130228     c                             (bolxco='W' or
264600130228     c                             (wesbtt='N' and wesblt='N' and wesart='N'
264700130228     c                              and barnrs>0))
264800130228     c* se si tratta di spunta partenza --> creo la 57
264900130228     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
265000130228     c                             or barnpg=1
265100130228     c                   exsr      CtrXCO
265200130228     c                   if        Indx>0
265300130301     c                   z-add     57            comcaa
265400130228     c                   endif
265500130228     c                   endif
265600130228     c                   endif
265700130301     c
265800130301     c* In caso di anomalia 50 manca bolla --> verifico se non è da creare
265900130301     c*  per collo spuntabile in questo TFP e "locale"
266000130301     c                   if        comcaa=50 and
266100130301     c                             (bolxco='W' or
266200130301     c                             (wesbtt='N' and wesblt='N' and wesart='N'
266300130301     c                              and barnrs>0))
266400130301     c* se si tratta di spunta di partenza --> non creo la 50 ma la 57
266500130301     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
266600130301     c                             or barnpg=1
266700130301     c                   exsr      CtrXCO
266800130301     c                   if        Indx>0
266900130301     c                   z-add     57            comcaa
267000130301     c                   endif
267100130301     c                   endif
267200130301     c                   endif
267300971009     C*
267400130228     c* per anomalia 55 o 56 57 valida l'anomalia per p.o. spunta
267500040429     c* per anomalia 50:
267600040429     c* per bolle transito solo se spara il p.o. transito
267700040429     c* per bolle arrivi, se trovo anomalia nell'area di arrivo(e non
267800040429     c*  di un eventuale p.o. transito)
267900040429     c                   clear                   wtrova
268000040429     C     KANM          SETLL     FNANM000
268100040504     C     KANM          reade(N)  FNANM000                               34
268200040429    1c                   dow       not*in34 and wtrova=*blanks
268300040429    2c                   if        anmcaa<>50 or wtibol='T'
268400040429    3c                   if        anmfle=bpes
268500040429     c                   eval      wtrova='1'
268600040429   x3c                   else
268700040504     C     KANM          reade(N)  FNANM000                               34
268800040429    3c                   endif
268900110103     c*
269000040429   x2c                   else
269100040429     C                   CLEAR                   DSLV55
269200040429     C                   MOVEL     'A'           D55TPT
269300040429     C                   Z-ADD     anmfle        D55LIN
269400040429     C                   Z-ADD     lnpb          D55lnp
269500040429     C                   Z-ADD     bardfv        D55DRF
269600040429     C                   EXSR      FNLV55
269700040429    3c                   if        d55tfa=pestfa
269800040429     c                   eval      wtrova='1'
269900040429   x3c                   else
270000040504     C     KANM          reade(N)  FNANM000                               34
270100040429    3c                   endif
270200040429    2c                   endif
270300110103     c*
270400040429    1c                   enddo
270500990412     C*
270600990412     C* PER ANOMALIA 55 VERIFICO ANCHE CHE NON CI SIA LA 56
270700990412     C*  PERCHE' DOPO AVER SPARATO UN COLLO IN IMA PUO' ESSERE
270800990412     C*  SPARATO IN PARTENZA PER FARLO RIPARTIRE
270900040429    1C     *IN34         IFEQ      *On
271000990412     C     COMCAA        ANDEQ     55
271100990412     C                   Z-ADD     56            COMCAA
271200040429     C     KANM          SETLL     FNANM000
271300040504     C     KANM          reade(N)  FNANM000                               34
271400040429     c                   dow       not*in34 and wtrova=*blanks
271500040429     c                   if        anmfle=bpes
271600040429     c                   eval      wtrova='1'
271700040429     c                   else
271800040504     C     KANM          reade(N)  FNANM000                               34
271900040429     c                   endif
272000040429     c                   enddo
272100990412     C                   Z-ADD     55            COMCAA
272200990412    1C                   ENDIF
272300950927     C*
272400040429    1C     *IN34         IFEQ      *On
272500970828     C* NON CREO ANOMALIA 55 SE C'E' UNA SPUNTA PARTENZA DI ALTRO AS
272600970828     C*  PERCHE' NON SI TRATTA PIU' DI UN DISGUIDO MA O E' COLLO
272700990409     C*  IN TRANSITO DI CUI NON HO RICEVUTO LA BOLLA
272800990409     C*  O E' COLLO DISGUIDATO IN ALTRA FIL CHE PER ANDARE IN ARRIVO
272900970828     C*  DEVE PASSARE DA ALTRA FIL ANCORA
273000970828     C*  LA 200 VIENE INVECE CREATA LO STESSO PERCHE' UNA ANOMALIA
273100970828     C*  GESTIONALE CHE SERVE PER FAR RIPARTIRE AUTOMATICAMENTE IL COLL
273200970828     C*  ANCHE SE NON VIENE SPARATO NE' IN PARTENZA NE' IN ENTRATA
273300000316     C                   EXSR      CNOIDD
273400950927    1C                   ENDIF
273500020214     C     NOA50         ENDSR
273600000316     C**************************************************************************
273700000316     C* RIEMPO ALCUNI CAMPI ANOMALIA NO IDD
273800000316     C**************************************************************************
273900000316     C     CNOIDD        BEGSR
274000160624     C** ANOMALIE 50 -->
274100160725     C* VERIFICO SE ESISTE UNA SPUNTA PARTENZA PER QUEL COLLO
274200160624     C*  SE SI NON CREO ANOMALIA PER 50
274300130228     C*  ANOMALIE 55/56/57 --> CONSIDERO SOLO LA SPUNTA "D-DISGUIDO"
274400020214     C*                     PER NON CREARLE
274500000316     C                   Z-ADD     1             WNPG
274600000317     C                   CLEAR                   WTROVA
274700021121     C* CAMPI DI COMODO PER ANOM 50 per transito su stessa £1
274800021121     C                   CLEAR                   WFVALNP
274900021121     C                   CLEAR                   WFVANFV
275000021121     C                   CLEAR                   WFVADFV
275100110103     c*
275200070131     C     KBAR3         CHAIN     FNBRV09L                           99
275300020214    1C     *IN99         DOWEQ     *OFF
275400000317     C     WTROVA        ANDEQ     ' '
275500050215     c                   clear                   wrileggi          1
275600020214     C*****
275700020214     C* SOLO SE LA SPUNTA TROVATA E' DI DISGUIDO, ALTRIMENTI SE NORMALE
275800020214     C*  E' UNA SPUNTA PREVISTA, QUINDI DA IGNORARE
275900020214    2C     COMCAA        IFEQ      55
276000020214     C     COMCAA        OREQ      56
276100130228     C     COMCAA        OREQ      57
276200040303    3C     BR9FGS        IFNE      BTFP
276300040429     c* verifico tramite pgm fnlv53r se spunta di foglio diretto a me(BPES)
276400040429     c*  se è diretto a me non creo anomalia
276500040429     c*  Questo vale sia per colli in disguido che colli in transito
276600040429     c*  manca record bolla partenza
276700040430     c                   clear                   dslv53
276800040504     c                   movel     'T'           d53tfo
276900040429     C                   MOVEL     BR9NPG        D53NPG
277000040429     C                   Z-ADD     BR9NFV        D53NFV
277100040429     C                   MOVEL     BR9FGS        D53FGS
277200040429     C                   MOVEL     BR9FLE        D53FLF
277300040504     C                   MOVEL     'A'           D53ABB
277400040429     C                   MOVEL     br9lna        D53LNA
277500050603     C                   MOVEL     Usalnatfa     D53TFA
277600040504     C                   MOVEL     bpes          D53lai
277700040429     C                   CALL      'FNLV53R'
277800040429     C                   PARM                    DSLV53
277900040429     c* se non ci sono errori --> OK esco dal ciclo
278000040429    4c                   if        d53err=*blanks
278100000317     C                   MOVEL     '1'           WTROVA            1
278200050418   x4c                   else
278300050418     C                   eval      wrileggi='1'
278400040429    4c                   endif
278500020214   X3C                   ELSE
278600050215     C                   eval      wrileggi='1'
278700020214    3C                   ENDIF
278800020214   X2C                   ELSE
278900020214     C**
279000031204     c** bolla non locale: cerco spunta non mia
279100031204   2ac     WSPLOC        ifeq      *blanks
279200040303    3C     BR9FGS        IFNE      BTFP
279300050329     c* Deve essere una spunta trasmessa, altrimenti ignoro (scaricata
279400050329     c*  tardi)
279500050329     c     br9ftr        andne     *blanks
279600110103     c*
279700050222    4c                   if        comcaa=50
279800050215     c                   clear                   dslv53
279900050322     c                   if        wtibol='T'
280000050322     c                   movel     'T'           d53tfo
280100050322     c                   else
280200050215     c                   movel     'A'           d53tfo
280300050322     c                   endif
280400050215     C                   MOVEL     BR9NPG        D53NPG
280500050215     C                   Z-ADD     BR9NFV        D53NFV
280600050215     C                   MOVEL     BR9FGS        D53FGS
280700050215     C                   MOVEL     BR9FLE        D53FLF
280800050215     C                   MOVEL     'A'           D53ABB
280900050215     C                   MOVEL     br9lna        D53LNA
281000050603     C                   MOVEL     Usalnatfa     D53TFA
281100050215     C                   MOVEL     pestfa        D53lai
281200050215     C                   CALL      'FNLV53R'
281300050215     C                   PARM                    DSLV53
281400050215     c* se non ci sono errori --> OK esco dal ciclo
281500050222    5c                   if        d53err=*blanks
281600020214     C                   MOVEL     '1'           WTROVA            1
281700050222   X5C                   ELSE
281800050418     c* Se si tratta di errore di foglio non trovato e provengo da
281900050418     c*  aggiornamento bolla transito Ex disguido (richiamo da fnlr13r)
282000050418     c*  il foglio si potrebbe trovare in fnfva01r, er cui conttrollo da
282100050418     c*  FNFGV
282200070221    6c                   if        d53err='2' and dls45FLG='T'
282300050418     c                   movel     'G'           d53tfo
282400050418     C                   CALL      'FNLV53R'
282500050418     C                   PARM                    DSLV53
282600050418     c* se non ci sono errori --> OK esco dal ciclo
282700050418    7c                   if        d53err=*blanks
282800050418     C                   MOVEL     '1'           WTROVA            1
282900050418   x7c                   else
283000050418     C                   eval      wrileggi='1'
283100050418    7c                   endif
283200050418   x6c                   else
283300050418     C                   eval      wrileggi='1'
283400050418    6c                   endif
283500050222    5C                   ENDIF
283600110103     c*
283700050222     c* Per anom 60/61 se trovata spunta --> ok
283800160624   X4C**                 ELSE
283900160624     C**                 MOVEL     '1'           WTROVA            1
284000050222     c                   endif
284100110103     c*
284200020214   X3C                   ELSE
284300050215     C                   eval      wrileggi='1'
284400020214    3C                   ENDIF
284500110103     c*
284600031204   2ac                   else
284700031204     c** bolla     locale: cerco spunta mia
284800040303    3C     BR9FGS        IFeq      BTFP
284900031204     C                   MOVEL     '1'           WTROVA            1
285000031204   X3C                   ELSE
285100050215     C                   eval      wrileggi='1'
285200031204    3C                   ENDIF
285300031204   2aC                   ENDIF
285400031204    2C                   ENDIF
285500050215     c
285600050215     c                   if        wrileggi='1'
285700070131     C     KBAR3         READE     FNBRV09L                               99
285800050215     c                   endif
285900020214    1C                   ENDDO
286000160725     c*
286100160725     c* Se non trovata e si tratta di bolla locale: cerco spunta
286200160725     c*  entrata
286300160725     c                   if        wtrova=' ' and wsploc=*blanks
286400160725     C                   Z-ADD     5             WNPG
286500160725     C     KBAR3         chain     FNBRV09L
286600160725     c                   if        %found(fnbrv09l) and br9fgs=btfp
286700160725     C                   MOVEL     '1'           WTROVA            1
286800160725     c                   setoff                                       99
286900160725     c                   endif
287000160725     c                   endif
287100000316     C*
287200000316     C* 99 ON  - SPUNTA NON TROVATA --> CREO ANOMALIA PER 50/55/56
287300000316    0C     *IN99         IFEQ      *ON
287400000316     C     COMCAA        ANDNE     60
287500000614     C     COMCAA        ANDNE     61
287600000316     C* 99 OFF - SPUNTA     TROVATA --> CREO ANOMALIA PER 60
287700160624     C**   *IN99         OREQ      *OFF
287800160624     C**   COMCAA        ANDEQ     60
287900160624     C**   *IN99         OREQ      *OFF
288000160624     C**   COMCAA        ANDEQ     61
288100000316     C                   CLEAR                   FNANM000
288200170505     c***
288300000316     C* MEMORIZZO NUMERO SPEDIZIONE SU ANOMALIA SE C'E'
288400170505    1C***                SELECT
288500170505     C***  WESBTT        wheneq    'S'
288600170505     C***                Z-ADD     BTTAAS        ANMAAS
288700170505     C***                Z-ADD     BTTLNP        ANMLNP
288800170505     C***                Z-ADD     BTTNSP        ANMNSP
288900170505     C***  WESART        wheneq    'S'
289000170505     C***                Z-ADD     ARTAAS        ANMAAS
289100170505     C***                Z-ADD     ARTLNP        ANMLNP
289200170505     C***                Z-ADD     ARTNSP        ANMNSP
289300170505     C***  WESblt        wheneq    'S'
289400170505     C***                Z-ADD     bltAAS        ANMAAS
289500170505     C***                Z-ADD     bltLNP        ANMLNP
289600170505     C***                Z-ADD     bltNSP        ANMNSP
289700170505    1C***                ENDSL
289800170505
289900170505     c                   if        comnsp>0
290000170505     C                   Z-ADD     comAAS        ANMAAS
290100170505     C                   Z-ADD     comLNP        ANMLNP
290200170505     C                   Z-ADD     comNSP        ANMNSP
290300170505     c                   endif
290400040621     c* Se c'e' la spedizione verifico se spedizione di valore
290500110103     c*
290600040621     C* linea partenza segnacollo
290700000316     C                   MOVEL     BARLNP        ANMFLS
290800000316     C* PER ANOMALIA 50
290900000316    1C     COMCAA        IFEQ      50
291000000316     C                   Z-ADD     BARFGS        ANMFLE
291100040908     C* PER BOLLA LOCALE non c'e' abbinamento di foglio per cui deduco le
291200040908     c*  date damettere sulla anomalia
291300040908     c                   clear                   wnofva            1
291400040908     c                   if        wtibol='L'
291500040908     c                   eval      wnofva='S'
291600040908     c                   endif
291700170505     c* Per sicurezza, per essere sicura di non aver sporcato i campi di ARB
291800170505     c*  uso i campi salvati
291900170505     c****               if        wesart='S' and arbtfp=arbtfa
292000170505     c                   if        wesart='S' and lnptfp=boltfa
292100040908     c                   eval      wnofva='S'
292200040908     c                   endif
292300050603     c                   if        wesart='N' and lnptfp=Usalnatfa
292400040908     c                   eval      wnofva='S'
292500040908     c                   endif
292600040908     c
292700040908   1ac                   if        wnofva='S'
292800040908    2c                   if        wesart='S'
292900170509     C                   Z-ADD     bolDBR        SAVDAO
293000170509     C                   Z-ADD     bolDBR        ANMDAO
293100020218     C                   Z-ADD     99998         ANMNFV
293200020218     C                   Z-ADD     LNPTFP        ANMCDU
293300020218     C                   Z-ADD     LNPTFP        WFL1
293400020218     C                   MOVEL     'L'           WFLGT
293500040908     c*
293600040908   x2c                   else
293700040908     c* Se non c'e' bolla cerco il 1° foglio locale con data < data spunta
293800040908     c     kfgv2         setll     fnfgv02l
293900040908     c     lnptfp        readpe    fnfgv02l
294000040908    3c                   dow       not %eof(fnfgv02l)  and
294100040908     c                             fgvttr <>'L'
294200040908     c     lnptfp        readpe    fnfgv02l
294300040908    3c                   enddo
294400110103     c*
294500040908    3c                   if        not %eof(fnfgv02l)
294600040908     C                   Z-ADD     fgvdfv        SAVDAO
294700040908     C                   Z-ADD     fgvdfv        ANMDAO
294800040908     C                   Z-ADD     99998         ANMNFV
294900040908     C                   Z-ADD     LNPTFP        ANMCDU
295000040908     C                   Z-ADD     LNPTFP        WFL1
295100040908     C                   MOVEL     'L'           WFLGT
295200040908    3c                   endif
295300040908    2c                   endif
295400110103     c*
295500040908  x1aC                   ELSE
295600070208     C* SE TROVA LA BOLLA IN BLP C'E' SEMPRE LNP BOLLA
295700020214     C*  ALTRIMENTI LA 5F ALTRIMENTI BARLNP
295800020214     C                   MOVEL     LNPB          WLIN
295900020214     C* IMPOSTO LA DATA DI RIFERIMENTO PER LA RICERCA DEL TERMINAL
296000020214     C*  DI PARTENZA: LA PRIMA VOLTA E' LA DATA DELLA BOLLA SE C'E'
296100020214     C                   MOVE      DSPB          WDTRIF
296200000316     C*
296300000316     C* CERCO IL NUMERO DI FV SU CUI DOVREBBE ESSERE PARTITO IL COLLO
296400000316     C                   MOVE      *BLANKS       WFLGT             1
296500000316     C                   CLEAR                   WLINPR
296600000316     C                   CLEAR                   WVALNP
296700000316    2C     WLIN          DOWGT     0
296800000316     C                   EXSR      RICFV
296900000316     C* MEMORIZZO IL P.O. DI TRANSITO E LA LINEA DI PARTENZA DEL FV
297000000316     C     WLIN          IFGT      *ZEROS
297100000316     C                   Z-ADD     WLIN          WLINPR
297200000316     C                   Z-ADD     FVALNP        WVALNP
297300000316     C                   ENDIF
297400000316    2C                   ENDDO
297500000316    2C     ANMCDU        IFGT      0
297600000316     C                   MOVEL     ANMCDU        WFL1
297700000316   X2C                   ELSE
297800070208     C* SE TROVA LA BOLLA IN BLP C'E' SEMPRE LNP BOLLA
297900020214     C*  ALTRIMENTI LA 5F ALTRIMENTI BARLNP
298000000316     C                   MOVEL     LNPB          WFL1
298100000316    2C                   ENDIF
298200020218   1AC                   ENDIF
298300000316     C*
298400000316   X1C                   ELSE
298500160624     C* PER ANOMALIA 55/56/57/      (NO IDD LE ALTRE)
298600000316     C                   Z-ADD     BARDFV        ANMDAO
298700000316     C                   MOVEL     BARFGS        ANMFLE
298800000316     C                   MOVEL     LNPB          WFL1
298900000316     C                   Z-ADD     BARNFV        ANMNFV
299000000316     C                   MOVEL     BARFGS        ANMCDU
299100000316    1C                   ENDIF
299200000316     C* SCRIVO L'ANOMALIA
299300000316     C                   EXSR      RIEANM
299400040621     c*
299500040621     c                   if        anmnsp>0
299600091209     c     kar5          chain(n)  fiar501l
299700040621    3c                   if        %found(fiar501l)
299800040621     c                   movel     ar5uni        dar5gen
299900040621     c                   else
300000040621     c                   clear                   dar5gen
300100040621    3c                   endif
300200040621     c                   movel     §ar5bva       anmft4
300300040621     c                   endif
300400000614     C*
300500000316     C* ESTERNA
300600000614    1C     §7CAIE        IFEQ      'E'
300700000316     C                   MOVEL     WFL1          ANMFL1
300800000614    1C                   ENDIF
300900000316     C* SCRIVO L'ANOMALIA SOLO SE HO TROVATO IL FOGLIO PARTENZA PER"50"
301000000614    1C     ANMCAA        IFNE      50
301100000316     C     ANMCAA        OREQ      50
301200000316     C     ANMNFV        ANDGT     0
301300000316     C* SE LA KEY E' PER SEGNACOLLO ED IL NUMERO E' 0 NON CREO L'ANOMAL
301400000316    2C     §7CKEY        IFNE      'S'
301500000316     C     §7CKEY        OREQ      'S'
301600000316     C     ANMSCN        ANDGT     0
301700001110     C************************
301800001110     C* CREO ANOMALIA SOLO SE:
301900001110     C************************
302000070215     C* ANOMALIA CREABILE ANCHE DA CAT.SPUNTA GENERICA
302100070215     C*                                 (ANCHE DA SPUNTA IMI)
302200070215     C     §7CCCG        ifeq      'S'
302300070215     C* CATEGORIA SPUNTA NON GENERICA   (NON È UNA SPUNTA IMI)
302400070215     C     BARSPG        orne      'I'
302500000316     C                   WRITE     FNANM000
302600001110     C*
302700001110     C* CHIUDO SE POSSIBILE UNA EVENTUALE 55 GIA' CREATA E POI CREO
302800001110     C*  LA 56
302900001110    3C     COMCAA        IFEQ      56
303000001110     C                   Z-ADD     55            COMCAA
303100001110     C                   MOVEL     'S'           SAVMAC
303200001110     C                   EXSR      CHIUAN
303300001110     C                   CLEAR                   SAVMAC
303400001110    3C                   ENDIF
303500000614    3C                   ENDIF
303600000614     C*
303700000316     C* PER ANOMALIA 50 CREATA AGGIORNO ANCHE LA DATA DI PARTENZA
303800000316     C* O LA DATA DI USCITA DAL TRANSITO
303900000316     C     ANMCAA        IFEQ      50
304000000316     C                   EXSR      AGGPAR
304100000316     C                   ENDIF
304200160727     C     ANMCAA        IFEQ      55
304300160727     C     ANMCAA        oreq      56
304400160727     C                   EXSR      AGGPAR_d
304500160727     C                   ENDIF
304600000316     C*
304700000316    2C                   ENDIF
304800020222     C*
304900020222     C                   ELSE
305000020222     C* SE NON TROVATO FOGLIO ATTENDO CHE ARRIVI (CI DEVE ESSERE PRIMA
305100031204     C*  O POI) se non locale
305200060103     c* Solo se non richiamato
305300060103     c  N02wtibol        ifne      'L'
305400020222     C                   SETON                                        3940
305500031204     c                   endif
305600000614    1C                   ENDIF
305700000614    0C                   ENDIF
305800000316     C                   ENDSR
305900130228     C**************************************************************************
306000130228     C* controllo se bolla con colli chde partono da divderse filiali
306100130228     C**************************************************************************
306200130228     C     CtrXCO        BEGSR
306300130228     c                   eval      Wflsnrs=(BARLNP*100)+BARnrs
306400130228
306500130228    1c                   if        wflsnrs<>savfnrs
306600130228     c                   clear                   xcotfp
306700130228     C                   MOVEL     'XCO'         kcod
306800130228     C                   eval      kke1='ALTRITFP'
306900130228     C                   eval      kke2=%editc(wflsnrs:'X')
307000130228     c     ktbe2         chain     tntbe01l
307100130228    2c                   if        %found(tntbe01l) AND tbeatb=' '
307200130228     C                   MOVEA     TBeUNI        xcotfp
307300130228    2c                   endif
307400130228     c
307500130228     c                   eval      savfnrs=wflsnrs
307600130228    1c                   endif
307700130228    c
307800130228     C* SE la fil spunte è compresa --> creo la 57
307900130228     c                   movel     barfgs        alfafgs           3
308000130228     c                   eval      Indx=%lookup(alfafgs:xcotfp)
308100130228     c
308200130228     c*
308300130228     C                   ENDSR
308400950927     C**************************************************************************
308500950927     C* RICERCA FV SU CUI E' PARTITA LA SPUNTA
308600950927     C**************************************************************************
308700950927     C     RICFV         BEGSR
308800950927     C                   CLEAR                   WMODL
308900021121     C                   movel     'N'           Wnoabb2           1
309000021121     C                   movel     'N'           Wnoabb6           1
309100950927     C* PRENDO IL TERMINAL DI PARTENZA DELLA LINEA DI PARTENZA
309200970814     C                   CLEAR                   DSLV55
309300970814     C                   MOVEL     'P'           D55TPT
309400970814     C                   MOVEL     WLIN          D55LIN
309500020214     C                   MOVEL     WDTRIF        D55DRF
309600020213     C                   EXSR      FNLV55
309700970814     C                   MOVEL     D55TFP        WPTFP
309800110103     c*
309900100713     C* PRENDO IL TERMINAL DI arrivo generico della linea
310000100713     C                   CLEAR                   DSLV55
310100100713     C                   MOVEL     'A'           D55TPT
310200100713     C                   MOVEL     barlna        D55LIN
310300100713     C                   MOVEL     WDTRIF        D55DRF
310400100713     C                   EXSR      FNLV55
310500100713     C                   MOVEL     D55TFa        WGENlnaTFA
310600021121     c
310700021121     c* se WPTFP = simfel ed ho salvato i campi del foglio illuminato
310800021121     c*  trovato, li considero validi e non rieseguo la routine
310900021121     c                   if        wptfp=simfel  and wfvalnp>0
311000021121     C                   MOVEL     wFVALNP       ANMCDU
311100021121     C                   Z-ADD     wFVALNP       SAVFLP
311200021121     C                   Z-ADD     wFVANFV       ANMNFV
311300021121     C                   Z-ADD     wFVADFV       ANMDAO
311400021121     C                   Z-ADD     wFVADFV       SAVDAO
311500021121     C                   CLEAR                   WLIN
311600021121     C                   MOVEL     '1'           WMODL             1
311700021121     c                   else
311800950927     C*
311900950927     C* CONTROLLO I FV ABBINATI, SE E' FOGLIO ARRIVI
312000020214     C     BARNPG        IFEQ      2
312100950927     C                   Z-ADD     BARNPG        KNPG4
312200950927     C                   Z-ADD     BARNFV        KNFV4
312300020214     C                   Z-ADD     BARFGS        KFGS4
312400950927     C                   EXSR      REDFVA
312500020214     C                   ENDIF
312600950927     C*
312700950927     C* SE NON L'HO TROVATO CERCO UN ALTRO F.ARRIVI DEL GIORNO
312800950927     C     WMODL         IFEQ      ' '
312900021121     c                   eval      wdata=bardfv
313000950927     C                   EXSR      MEMFAR
313100021121     C* MEMORIZZO LA NUOVA DATA
313200021121     C                   MOVEL     BARDFV        SAVDFV
313300110103     C                   Z-ADD     1             I                 3 0
313400950927     C*
313500950927    2C     PG2(I)        DOWGT     0
313600950927     C     ANMNFV        ANDEQ     0
313700950927     C                   Z-ADD     2             KNPG4
313800950927     C                   Z-ADD     PG2(I)        KNFV4
313900020214     C                   Z-ADD     SAVTFA        KFGS4
314000950927     C                   EXSR      REDFVA
314100021121     c*
314200021121     c* Memorizzo se nessuno dei fogli 2 ha dei fogli partenza abbinati
314300021121     c                   if        wabbinati=' '
314400021121     c                   clear                   wnoabb2
314500021121     c                   endif
314600950927     C                   ADD       1             I
314700950927    2C                   ENDDO
314800950927    1C                   ENDIF
314900110103     c*
315000021121     c* Se non ho fogli abbianti e non si tratta di cat arrivi, ritento
315100021121     c*  la ricerca dei fogli arrivi
315200021121    1c     WMODL         IFEQ      ' '
315300110103    2c                   if        barnpg<> 2 and
315400110103     c                             wnoabb2 ='N' and
315500021121     c                             wsavdata=bardfv
315600021121     c* Data - 1
315700021121     C                   MOVEL     bardfv        G02INV
315800021121     C                   Z-ADD     *ZEROS        G02DAT
315900021121     C                   Z-ADD     *ZEROS        G02TGI
316000021121     C                   MOVEL     '3'           G02ERR
316100021121     C                   CALL      'XSRDA8'
316200021121     C                   PARM                    WLBDAT
316300021121      *
316400021121     C                   SUB       1             G02TGI
316500021121     C                   Z-ADD     *ZEROS        GI8DAT
316600021121     C                   Z-ADD     *ZEROS        GI8INV
316700021121     C                   Z-ADD     G02TGI        GI8TGI
316800021121     C                   CALL      'XSRGI8'
316900021121     C                   PARM                    WGIDAT
317000021121      *
317100021121     C                   Z-ADD     GI8INV        wdata
317200021121      *
317300021121     C                   EXSR      MEMFAR
317400021121     C* MEMORIZZO LA NUOVA DATA
317500021121     c                   if        wsavdata>0
317600021121     C                   MOVEL     wsavdata      SAVDFV
317700021121     c                   else
317800021121     C                   MOVEL     BARDFV        SAVDFV
317900021121     c                   endif
318000110103     c*
318100021121     C                   Z-ADD     1             I
318200021121     C*
318300021121    3C     PG2(I)        DOWGT     0
318400021121     C     ANMNFV        ANDEQ     0
318500021121     C                   Z-ADD     2             KNPG4
318600021121     C                   Z-ADD     PG2(I)        KNFV4
318700021121     C                   Z-ADD     SAVTFA        KFGS4
318800021121     C                   EXSR      REDFVA
318900021121     C                   ADD       1             I
319000021121    3C                   ENDDO
319100021121    2C                   ENDIF
319200021121    1c                   endif
319300110103     c*
319400100713     c* se il FV non ancora trovato e si dovrebbe trattare di bolla locale
319500100713     c*  tengo i dati del foglio illuiminato
319600100713     c     WMODL         ifeq      ' '
319700100713     c     wgenlnatfa    andeq     wlin
319800100713     C                   MOVEL     wFVALNP       ANMCDU
319900100713     C                   Z-ADD     wFVALNP       SAVFLP
320000100713     C                   Z-ADD     wFVANFV       ANMNFV
320100100713     C                   Z-ADD     wFVADFV       ANMDAO
320200100713     C                   Z-ADD     wFVADFV       SAVDAO
320300100713     C                   CLEAR                   WLIN
320400100713     C                   MOVEL     '1'           WMODL             1
320500100713     c                   endif
320600950927     C*
320700950927     C* SE IL FV ANCORA NON C'E' CERCO SE ANCORA DA ABBINARE
320800021121    1C     WMODL         IFEQ      ' '
320900950927     C                   CLEAR                   KNFV4
321000950927     C                   CLEAR                   KNPG4
321100020214     C                   MOVE      SAVTFA        KFGS4
321200000223     C* Determino BARDFV-4 gg. che è la data minima che deve avere un
321300000223     C* foglio ancora da abbinare. Se la data foglio è più bassa di
321400000223     C* tale data il foglio verrà scartato
321500000223     C                   MOVE      *ZEROS        DATMIN
321600000223     C                   CLEAR                   WLBDAT
321700000223     C                   CLEAR                   WGIDAT
321800000223     C                   Z-ADD     BARDFV        G02INV
321900000223     C                   MOVEL     '3'           G02ERR
322000000223     C                   CALL      'XSRDA8'
322100000223     C                   PARM                    WLBDAT
322200000223     C     G02TGI        SUB       4             GI8TGI
322300000223     C                   CALL      'XSRGI8'
322400000223     C                   PARM                    WGIDAT
322500000223     C                   Z-ADD     GI8INV        DATMIN
322600950927     C                   EXSR      REDFVA
322700950927     C* SE ANCORA NON HO TROVATO NIENTE --> ESCO DAL CICLO
322800950927    2C     WMODL         IFEQ      ' '
322900950927     C                   CLEAR                   WLIN
323000950927    2C                   ENDIF
323100950927    1C                   ENDIF
323200990827     C*
323300990827     C* PULISCO WLIN PER NON CONTINUARE NELLA RICERCA DEL FOGLIO
323400990827     C* SE LA LINEA DI ARRIVO RISULTA TRANSITARE
323500990827     C* DA UN P.O. CHE  E' LO STESSO DAL QUALE IL COLLO ERA PARTITO
323600990827     C* (PER ES.: 53/31 CHE TRANSITA DA 062 E 062 LO FA TRANSITARE DA
323700990827     C* 53 (CASO CHE SI VERIFICA DURANTE IL PASSAGGIO DALLE TRAZIONI
323800990827     C* RIDOTTE ALLE TRAZIONI NORMALI))
323900990827     C     WLINPR        IFGT      *ZEROS
324000990827     C     WLIN          ANDGT     *ZEROS
324100990827     C     WLIN          ANDEQ     WVALNP
324200990827     C                   CLEAR                   WLIN
324300990827     C                   ENDIF
324400021121     C                   ENDIF
324500950927     C*
324600950927     C                   ENDSR
324700950927     C**************************************************************************
324800950927     C* LETTURA DEI DEI FOGLI VIAGGIO PARTENZA
324900950927     C**************************************************************************
325000950927     C     REDFVA        BEGSR
325100950927     C                   CLEAR                   WMODL
325200021121     C                   CLEAR                   Wabbinati         1
325300950927     C     KFVA4         SETLL     FNFVA000
325400950927     C     KFVA4         READE     FNFVA000                               99
325500021121     c* se 99 on - nessun foglio abbinato --> memorizzo
325600021121     c                   if        *in99
325700021121     c                   eval      wabbinati='N'
325800021121     c                   endif
325900950927     C*
326000950927    1C     *IN99         DOWEQ     *OFF
326100950927    2C     FVALNP        IFEQ      WPTFP
326200950927     C* DATA FOGLIO DEVE ESSERE <= ALLA DATA DI ARRIVO
326300960524     C     FVADFV        ANDLE     BARDFV
326400000223     C     KNPG4         ANDGT     *ZEROS
326500000223     C* SE STO CERCANDO FOGLIO ANCORA DA ABBINARE SCARTO SE E' DI DATA
326600000223     C* INFERIORE ALLA DATA MINIMA (DATA ARRIVO COLLO MENO QUATTRO GG.)
326700000223     C     FVALNP        OREQ      WPTFP
326800000223     C     FVADFV        ANDLE     BARDFV
326900000223     C     FVADFV        ANDGE     DATMIN
327000000223     C     KNPG4         ANDEQ     *ZEROS
327100960207     C*
327200950927     C* SE SI TRATTA DI UN FOGLIO DIRETTO TUTTO A POSTO
327300100712     c* ok anche se locale
327400950927    3C     FVAATR        IFEQ      ' '
327500100712     c     fvattr        oreq      'L'
327600950927     C                   MOVEL     FVALNP        ANMCDU
327700020221     C                   Z-ADD     FVALNP        SAVFLP
327800990518     C                   Z-ADD     FVANFV        ANMNFV
327900950927     C                   Z-ADD     FVADFV        ANMDAO
328000980120     C                   Z-ADD     FVADFV        SAVDAO
328100950927     C                   CLEAR                   WLIN
328200950927     C                   MOVEL     '1'           WMODL             1
328300980120     C* SE IL PRIMO FV TROVATO E' UN DIRETTO LO MEMORIZZO PER SAPERE SE
328400980120     C* DOVRO' AGGIOR.LA DATA DI PARTENZA O LA DATA USCITA DAL TRANSITO
328500980120     C     WFLGT         IFEQ      *BLANKS
328600980120     C                   MOVE      'D'           WFLGT
328700980120     C                   END
328800950927     C                   SETON                                        99
328900960207     C*
329000950927   X3C                   ELSE
329100960207     C* SE E' FOGLIO ILLUMINATO
329200950927     C* DEVO VEDERE DA DOVE E' TRANSITO E MANDARE AL TRANSITO LA SEGNA
329300950927     C*  LAZIONE
329400000505     C     KFWA          CHAIN     FNFWA01L                           28
329500000505     C     *IN28         IFEQ      *ON
329600000505     C     FWAATB        ORNE      ' '
329700000505     C                   CLEAR                   FWAFF3
329800000505     C                   CLEAR                   FWAFF4
329900000505     C                   CLEAR                   FWAFL3
330000000505     C                   CLEAR                   FWAFL4
330100000505     C                   ENDIF
330200000505     C**
330300950927     C                   Z-ADD     FVALNA        LNA2
330400950927     C                   Z-ADD     1             X
330500951002     C                   MOVEL     BARLNA        W003A             3
330600950927     C     W003A         LOOKUP    FFV(X)                                 99
330700960207     C* NON E' IL MIO TERMINAL
330800960902     C* ESCLUDO I FOGLI LOCALI
330900021125     c* in relatà elaboro anche i fogli locali, altrimenti come faccio a
331000021125     c*  determinare un collo arrivo no part, del terminal per la lna?
331100021125     c*  ES. 43  x 132
331200960207    4C     *IN99         IFEQ      *ON
331300021125     C***  FVATTR        ANDNE     'L'
331400050603     C                   MOVEL     Usalnatfa     W003A             3
331500960522    5C     FLP(X)        IFNE      W003A
331600960207     C                   MOVEL     FLP(X)        WLIN
331700020214     C                   MOVE      FVADFV        WDTRIF
331800960207     C                   MOVEL     '1'           WMODL             1
331900980120     C* SE IL PRIMO FV TROVATO E' UN TRANSITO LO MEMORIZZO PER SAPERE
332000980120     C* SE DOVRò AGGIOR. DATA DI PARTENZA O DATA USCITA DAL TRANSITO
332100980120     C     WFLGT         IFEQ      *BLANKS
332200980120     C                   MOVE      'T'           WFLGT
332300021121     c* Memorizzo lo stesso i campi che mi servono perchè se poi di tratta
332400021121     c*  di transito "locale" cioè su stesso as (vedi 82 con 113)
332500021121     c*  devo comunque tenere i dati del foglio illuminato non essendoci
332600021121     c*  abbinamento del foglio locale su stessa £1
332700021121     c*  Il problema verrà a morire quando diventeranno dei 1 livello
332800021121     c*  come devono essere
332900021121     c*
333000021121     c                   z-add     FVALNP        WFVALNP
333100021121     c                   z-add     FVANFV        WFVANFV
333200021121     c                   z-add     FVADFV        WFVADFV
333300980120     C                   END
333400960207   X5C                   ELSE
333500960207     C* SE E' FOGLIO ILLUMINATO PER 2 LIVELLO IN ARRIVO TUTTO A POSTO
333600960207     C                   MOVEL     FVALNP        ANMCDU
333700990518     C                   Z-ADD     FVANFV        ANMNFV
333800960207     C                   Z-ADD     FVADFV        ANMDAO
333900980120     C                   Z-ADD     FVADFV        SAVDAO
334000020221     C                   Z-ADD     FVALNP        SAVFLP
334100960207     C                   CLEAR                   WLIN
334200960207     C                   MOVEL     '1'           WMODL             1
334300980120     C* SE IL PRIMO FV TROVATO E' UN DIRETTO LO MEMORIZZO PER SAPERE SE
334400980120     C* DOVRO' AGGIOR.LA DATA DI PARTENZA O LA DATA USCITA DAL TRANSITO
334500980120     C     WFLGT         IFEQ      *BLANKS
334600980120     C                   MOVE      'D'           WFLGT
334700980120     C                   END
334800960207    5C                   ENDIF
334900960207     C*
335000960207   X4C                   ELSE
335100960207     C     KFVA4         READE     FNFVA000                               99
335200960207    4C                   ENDIF
335300950927    3C                   ENDIF
335400950927     C*
335500950927   X2C                   ELSE
335600950927     C     KFVA4         READE     FNFVA000                               99
335700950927    2C                   ENDIF
335800950927    1C                   ENDDO
335900950927     C                   ENDSR
336000980119     C**************************************************************************
336100980119     C* AGGIORNO DATA PART. O DI USCITA DA TRANS. A FRONTE DI ANOM.50
336200980119     C**************************************************************************
336300980119     C     AGGPAR        BEGSR
336400980119     C     KBAR          CHAIN     FNART27L                           3239
336500980119     C   39              GOTO      ENDPAR
336600160722     c
336700160722    1c     *IN32         IFEQ      *OFF
336800160722     c* Spunta di bolla in arrivo ( anche locale)
336900160722    2C                   if        WTIBOL='A'  or WTIBOL='L'
337000160722
337100980120    3C                   SELECT
337200980120     C     WFLGT         WHENEQ    'D'
337300980120     C                   Z-ADD     SAVDAO        ARTDFV
337400041201     C                   Z-ADD     anmnfv        ARTnFV
337500980120     C                   Z-ADD     88            ARTNPP
337600041202     C                   movel     'S'           ARTABN
337700020218     C     WFLGT         WHENEQ    'L'
337800020218     C                   Z-ADD     SAVDAO        ARTDFV
337900041201     C                   Z-ADD     anmnfv        ARTnFV
338000020218     C                   Z-ADD     90            ARTNPP
338100041202     C                   movel     'S'           ARTABN
338200980120     C     WFLGT         WHENEQ    'T'
338300980120     C                   Z-ADD     SAVDAO        ARTDUT
338400980120     C                   Z-ADD     89            ARTPUT
338500020221     C                   Z-ADD     SAVFLP        ARTFLP
338600041202     C                   movel     'S'           ARTABN
338700980120    3C                   ENDSL
338800160722    2c                   endif
338900160722     c*
339000160722     c* spunta al transito: devo aggiornare anche  in FNART
339100160722     c*  la data di partenza se non c'è
339200160722    2c                   if        WTIBOL='T'
339300160722     c
339400160722    3c                   if        artdfv=0
339500160722     C                   Z-ADD     SAVDAO        ARTDFV
339600160722     C                   Z-ADD     anmnfv        ARTnFV
339700160722     C                   Z-ADD     88            ARTNPP
339800160722    3c                   endif
339900160722    2c                   endif
340000160720
340100980120     C                   UPDATE    FNART000
340200980120     C                   MOVEL     'A'           WTBO              1
340300980120     C                   MOVEL     ARTSPE        WDSPE
340400980120     C                   EXSR      WR1AGB
340500160722    1C                   endif
340600160720
340700160722     C                   if        WTIBOL='T'
340800021203     C     KBAR37        CHAIN     FNBTT37L                           3739
340900980119     C   39              GOTO      ENDPAR
341000161108    2C     *IN37         IFEQ      *OFF
341100980120     C                   Z-ADD     SAVDAO        BTTDFV
341200980120     C                   Z-ADD     88            BTTNPP
341300980120     C                   UPDATE    FNBTT000
341400980120     C                   MOVEL     'T'           WTBO              1
341500980120     C                   MOVEL     BTTSPE        WDSPE
341600980120     C                   EXSR      WR1AGB
341700980120    2C                   END
341800160722    1C                   endif
341900160720
342000160720     C*  AGGIORNO ANCHE LA BOLLA PARTENZA
342100020218    1C     WTIBOL        IFEQ      'L'
342200160720     C     WTIBOL        oreq      'A'
342300160721     C     WTIBOL        oreq      'T'
342400020218     C     KBAR          CHAIN     FNBLT27L                           3239
342500020218     C   39              GOTO      ENDPAR
342600020218    2C     *IN32         IFEQ      *OFF
342700160721
342800160721     c                   if        wtibol<>'T'
342900160721     c                   z-add     anmdch        bltdam
343000160721     c                   else
343100160721     c                   z-add     anmdch        bltdet
343200160721     c                   z-add     bpes          bltflp
343300160722     c                   clear                   bltdut
343400160721     c                   endif
343500160720
343600160721    3C                   IF        WFLGT<>'T' AND
343700160721     c                             (bltdfv=0 or bltdfv>savdao) or
343800160721     c                             wflgt= 'T' and bltdfv=0
343900160721     c
344000160720     C                   Z-ADD     SAVDAO        bltDFV
344100160726     c                   eval      welasta_p='S'
344200160720
344300160721    4c                   select
344400160720     C     BLTDSE        WHENEQ    0
344500160720     c                   if        wflgt='D'
344600160720     C                   MOVEL     88            BLTNPP
344700160721     c                   else
344800160720     C                   Z-ADD     90            bltNPP
344900160720     c                   endif
345000160720     C     BLTDSE        WHENGT    0
345100160720     C     BLTDSE        ANDLT     BLTDFV
345200160720     C                   MOVEL     96            BLTNPP
345300160720     C     BLTDSE        WHENGT    0
345400160720     C     BLTDSE        ANDGE     BLTDFV
345500160720     C                   MOVEL     98            BLTNPP
345600160721    4C                   ENDSL
345700160720
345800160721    3c                   endif
345900160720
346000160722    3C                   if        wflgt='T'  and WTIBOL<>'T' and
346100160722     c                             (bltdut=0 or bltdut>savdao)
346200160720     C                   Z-ADD     SAVDAO        bltDUT
346300160722     C                   Z-ADD     bpes          bltFLP
346400160721    3c                   endif
346500160720     c
346600020218     C                   UPDATE    FNBLT000
346700020218     C                   MOVEL     'P'           WTBO              1
346800020218     C                   MOVEL     BLTSPE        WDSPE
346900020218     C                   EXSR      WR1AGB
347000160721     c
347100160721     c* se c'è transito, aggiorno anche BTT
347200160721    3C     WFLGT         ifeq      'T'
347300160721     C     KBAR_t        CHAIN     FNBTT37L                           3239
347400160721    4C     *IN32         IFEQ      *OFF
347500160721     C     *IN39         andeq     *OFF
347600160721
347700160726     c                   eval      welasta_p='S'
347800160721     C                   Z-ADD     SAVDAO        bttDUT
347900160721     c                   z-add     anmdch        bttdam
348000160721     C                   SELECT
348100160721     C     BTTDET        WHENEQ    0
348200160721     C                   MOVEL     89            BTTPUT
348300160721     C     BTTDET        WHENGT    0
348400160721     C     BTTPET        ANDEQ     89
348500160721     C                   MOVEL     89            BTTPUT
348600160721     C     BTTDET        WHENGT    0
348700160721     C     BTTDET        ANDLT     BTTDUT
348800160721     C                   MOVEL     96            BTTPUT
348900160721     C     BTTDET        WHENGT    0
349000160721     C     BTTDET        ANDGE     BTTDUT
349100160721     C                   MOVEL     98            BTTPUT
349200160721     C                   ENDSL
349300160721     C*
349400160721     C     BTTDET        IFEQ      0
349500160721     C                   Z-ADD     ANMDAO        BTTDET
349600160721     C                   MOVEL     89            BTTPET
349700160721     C                   ENDIF
349800160721     C                   UPDATE    FNBTT000
349900160721     C                   MOVEL     'T'           WTBO              1
350000160721     C                   MOVEL     BLTSPE        WDSPE
350100160721     C                   EXSR      WR1AGB
350200160721    4C                   ENDif
350300160721    3C                   ENDif
350400160720
350500020218    2C                   END
350600020218    1C                   END
350700160721
350800980119     C     ENDPAR        TAG
350900160721
351000980119     C   39              SETON                                        40
351100160726     c
351200160726     c                   clear                   welasta_p
351300160726     c
351400980119     C                   ENDSR
351500160727     C**************************************************************************
351600160727     C* AGGIORNO DATA entrata al transito per DIsguido
351700160727     C**************************************************************************
351800160727     C     AGGPAR_d      BEGSR
351900160727     C     KBAR          CHAIN     FNBLT27L                           3239
352000160727    2C     *IN32         IFEQ      *OFF
352100160727    2C     *IN39         andeq     *OFF
352200160727     C                   Z-ADD     SAVDAO        bltDET
352300160727     c                   clear                   bltdut
352400160727     c                   movel     barfgs        bltflp
352500160727     c                   update    fnblt000
352600160727
352700160727     C                   MOVEL     'P'           WTBO              1
352800160727     C                   MOVEL     BLTSPE        WDSPE
352900160727     C                   EXSR      WR1AGB
353000160727     c                   endif
353100160727     c
353200160727     C                   ENDSR
353300950927     C**************************************************************************
353400950927     C* MEMORIZZO I FOGLI ARRIVI CETEGORIA 2 E 6 DEL GIORNO
353500950927     C**************************************************************************
353600950927     C     MEMFAR        BEGSR
353700021121     c                   clear                   wsavdata          8 0
353800021113     c
353900021113     c* se è collo di bolla transito
354000021113     c*  devo usare simfel, il suo p.o. di transito
354100021113     c                   if        wtibol<>'T'
354200050603     C                   MOVEL     usalnatfa     KTFA2
354300021113     c                   else
354400021113     C                   z-add     simfel        KTFA2
354500021113     c                   endif
354600021113     c
354700950927     C* RIMEMORIZZO I FV SOLO SE E' CAMBIATA LA  DATA
354800021121    1C     wdata         IFNE      SAVDFV
354900970113     C     KTFA2         ORNE      SAVTFA
355000021107     c
355100021121     c* Cerco fogli cat 2 e 6  se non trovati in data BARDFV, li cerco
355200021107     c*  in data bardfv - 1
355300021113     c* faccio max 8 tentativi (1 settimana  tenendo conto che ci possono
355400021113     c*  essere dei festivi)
355500950927     C                   CLEAR                   PG2
355600021113     c                   z-add     0             wgiorni           2 0
355700021113     c
355800021113    2c                   dow       wdata>0  and wgiorni<8
355900021113     c                   add       1             wgiorni
356000950927     C*
356100950927     C                   Z-ADD     2             WNPG
356200950927     C     KFVV          SETLL     FNFVV002
356300950927     C     KFVV          READE     FNFVV002                               33
356400950927     C                   Z-ADD     1             I
356500950927     C*
356600950927     C     *IN33         DOWEQ     *OFF
356700950927     C                   Z-ADD     FV2NFV        PG2(I)
356800950927     C                   ADD       1             I
356900950927     C     KFVV          READE     FNFVV002                               33
357000950927     C                   ENDDO
357100021107     c*
357200021107     c                   xfoot     pg2           wpg2             13 0
357300110103     c                   if        wpg2>0
357400021121     c                   eval      wsavdata=wdata
357500021107     c                   clear                   wdata
357600021107     c                   else
357700021107     c* Data - 1
357800021107     C                   MOVEL     wdata         G02INV
357900021107     C                   Z-ADD     *ZEROS        G02DAT
358000021107     C                   Z-ADD     *ZEROS        G02TGI
358100021107     C                   MOVEL     '3'           G02ERR
358200021107     C                   CALL      'XSRDA8'
358300021107     C                   PARM                    WLBDAT
358400021107      *
358500021107     C                   SUB       1             G02TGI
358600021107     C                   Z-ADD     *ZEROS        GI8DAT
358700021107     C                   Z-ADD     *ZEROS        GI8INV
358800021107     C                   Z-ADD     G02TGI        GI8TGI
358900021107     C                   CALL      'XSRGI8'
359000021107     C                   PARM                    WGIDAT
359100021107      *
359200021107     C                   Z-ADD     GI8INV        wdata
359300021107     c                   endif
359400021107     c                   enddo
359500021121     c*
359600970113     C                   MOVEL     KTFA2         SAVTFA
359700021107    1C                   ENDIF
359800950927     C                   ENDSR
359900950927     C**************************************************************************
360000950927     C* RIEMPO PARTE DEI CAMPI DELLA ANOMALIA
360100950927     C**************************************************************************
360200950927     C     RIEANM        BEGSR
360300950927     C                   MOVEL     BARLNP        ANMFLS
360400950927     C                   MOVEL     BARLNA        ANMLNA
360500950927     C                   MOVEL     BARNRS        ANMNRS
360600950927     C                   MOVEL     BARNSC        ANMSCN
360700950927     C                   MOVEL     BARNPS        ANMNPS
360800950927     C                   Z-ADD     COMCAA        ANMCAA
360900040621     c                   z-add     999           anmfl4
361000040621     c* imposto in anmflo data e ora creazione spunta
361100050707     C                   TIME                    TIMEanm          14 0
361200050707     c                   movel     timeanm       wora4             4 0
361300050707     C                   move      timeanm       G02DAT
361400050707     C                   Z-ADD     *ZERO         G02INV
361500050707     C                   MOVEL     *BLANKS       G02ERR
361600050707     C                   CALL      'XSRDA8'
361700050707     C                   PARM                    WLBDAT
361800050707     C                   move      G02INV        WDAT6             6 0
361900040621     c                   movel     wdat6         anmflo
362000040621     c                   move      wora4         anmflo
362100950927     C* FASE APERTURA
362200950927     C     BARKEY        IFNE      SAVFOG
362300950927     C                   EXSR      RICFAS
362400950927     C                   END
362500950927     C                   MOVEL     COMFAS        ANMFSA
362600950927     C* CODICE ANOMALIA
362700950927     C     COMCAA        IFNE      SAVCAA
362800950927     C                   MOVEL     '7C'          COD
362900950927     C                   MOVEL(P)  COMCAA        KEY
363000950927     C     KTAB          CHAIN     TABEL                              34
363100950927     C   34              CLEAR                   DS7C
363200950927     C  N34              MOVEL     TBLUNI        DS7C
363300950927     C                   MOVEL     COMCAA        SAVCAA            3 0
363400950927     C                   ENDIF
363500950927     C*
363600950927     C                   MOVEL     §7CTAN        ANMTAN
363700950927     C                   MOVEL     §7CAIE        ANMAIE
363800950927     C                   MOVEL     §7CSAN        ANMSAN
363900950927     C                   MOVEL     §7CLID        ANMLID
364000950927     C* AUTOCHIUSA
364100950927     C     §7CCHA        IFEQ      'S'
364200960524     C                   MOVE      BARDFV        ANMDCH
364300950927     C                   MOVEL     COMFAS        ANMFSC
364400000320     C                   MOVEL     WCCH          ANMCCH
364500950927     C                   END
364600950927     C                   ENDSR
364700960523     C**************************************************************************
364800960523     C* CARICO LA L6 DELLA FILIALE GESTIONE FOGLIO
364900960523     C**************************************************************************
365000960523     C     CARL6         BEGSR
365100960523     C                   CLEAR                   DSUL06
365200960523     C                   MOVE      '£6'          D06COD
365300020213     C                   MOVEL     SAVPES        D06KEY
365400960523     C                   MOVEL     DSUL06        KPJBU
365500960523     C*
365600960523     C                   CALL      'TRUL06R'
365700960523     C                   PARM                    KPJBA
365800960523     C                   MOVEL     KPJBU         DSUL06
365900960523     C                   MOVEA     LIN           L6
366000960523     C                   ENDSR
366100960523     C**************************************************************************
366200960523     C* SE ESISTE GIA' UNA SPUNTA DELLA LNA, NON CREO ANOMALIE 145-146
366300960523     C**************************************************************************
366400960523     C     CTRSPU        BEGSR
366500960523     C                   MOVEL     'S'           WCREA
366600970117     C*
366700970117     C                   MOVEL     'S'           WCHIU             1
366800970117     C*"WCHIU" MI SERVE PER DETERMINARE SE POSSO CHIUDERE UNA ANOMALIA
366900970117     C* 145/146 CHE HA DATA ANOMALIA < DELLA DATA DELLA SPUNTA CHE
367000970117     C*  STO ELABORANDO MA POTREBBE COMUNQUE ESSERCI UNA SPUNTA
367100970117     C*  CHE MI MANTIENE APERTA IN DATA > TALE ANOMALIA
367200970117     C*  1) SE E' PRESENTE UN'ALTRA SPUNTA CON DATA > NON CHIUDO
367300970117     C*  2) SE E' PRESENTE UNA SPUNTA CON DATA = CATEGORIA 3 4 5
367400020204     C*     NON CHIUDO SE LA SPUNTA CHE STO SCARICANDO E' CAT 1 2 6
367500970117     C*  3) SE E' PRESENTE UNA SPUNTA CON DATA = CATEGORIA 3 4 5
367600970117     C*     E E' UNA SPUNTA DELLA LNA SE LA SPUNTA CHE STO SCARICANDO
367700970117     C*     E' CATEGORIA 3 4 5
367800960531     C**
367900020204    1C     WAGARR        IFEQ      'S'
368000960531     C**
368100070131     C     KBRV7         CHAIN     FNBRV07L                           33
368200960531    2C     *IN33         DOWEQ     *OFF
368300960531    3C     BR7KEY        IFNE      SA1KEY
368400960527     C                   MOVEL     BR7KEY        SA1KEY
368500960527     C                   Z-ADD     BR7NPG        WNPG
368600960527     C                   Z-ADD     BR7NFV        WNFV
368700021014     C                   Z-ADD     BR7FGS        WFGS
368800070906     C                   movel     BR7nps        Wnps
368900021014     C                   CLEAR                   RICBAR
369000960523     C                   EXSR      RILFOG
369100960523     C                   Z-ADD     WDFV          BRVDFV
369200140616     C                   movel     WFCF          BRVFCF
369300140616     C                   movel     Wspg          BRVspg
369400960531    3C                   ENDIF
369500040224     c* Terminal di arrivo della spunta letta
369600040224     C                   CLEAR                   DSLV55
369700040224     C                   MOVEL     'A'           D55TPT
369800040224     C                   MOVEL     LNPB          D55LNP
369900040224     C                   MOVEL     brvdfv        D55DRF
370000040224     C                   MOVE      Br7pes        D55LIN
370100040224     C                   EXSR      FNLV55
370200050603     c                   if        d55tfa=Usalnatfa
370300960531     C**
370400961210    3C     BRVDFV        IFGT      BARDFV
370500960523     C                   MOVEL     'N'           WCREA             1
370600970117     C                   MOVEL     'N'           WCHIU             1
370700961209   X3C                   ELSE
370800961209     C*
370900961210    4C     BRVDFV        IFEQ      BARDFV
371000961210     C     BR7NPG        ANDGE     3
371100961210     C     BR7NPG        ANDLE     5
371200970117     C*
371300970117     C     BARNPG        IFEQ      1
371400970117     C     BARNPG        OREQ      2
371500970117     C                   MOVEL     'N'           WCHIU
371600970117     C                   ENDIF
371700961209     C*
371800020213    5C     BR7PES        IFEQ      BARLNA
371900961209     C                   MOVEL     'N'           WCREA             1
372000970117     C*
372100970117     C     BARNPG        IFNE      1
372200970117     C     BARNPG        ANDNE     2
372300970117     C                   MOVEL     'N'           WCHIU
372400970117     C                   ENDIF
372500970117     C*
372600961209   X5C                   ELSE
372700020214    6C     BR7PES        IFNE      SAVPES
372800020214     C                   MOVEL     BR7PES        SAVPES
372900961209     C                   EXSR      CARL6
373000961209    6C                   ENDIF
373100961209     C*
373200961209     C     BARLNA        LOOKUP    L6                                     34
373300961209     C   34              MOVEL     'N'           WCREA             1
373400970117     C*
373500970117    6C   34BARNPG        IFNE      1
373600970117     C     BARNPG        ANDNE     2
373700970117     C                   MOVEL     'N'           WCHIU
373800970117    6C                   ENDIF
373900970117     C*
374000961209    5C                   ENDIF
374100961209    4C                   ENDIF
374200961209    3C                   ENDIF
374300040224    3C                   ENDIF
374400961209     C*
374500961209    3C     WCREA         IFEQ      'N'
374600961209     C                   SETON                                        33
374700961210     C                   ELSE
374800070131     C     KBRV7         READE     FNBRV07L                               33
374900961209    3C                   ENDIF
375000040224     C**
375100960531    2C                   ENDDO
375200960531     C*
375300960531    1C                   ENDIF
375400960523     C*
375500960523     C                   ENDSR
375600961202     C**************************************************************************
375700961202     C* CREO ANOMALIA 145 O 146 E CHIUDO L'ALTRA
375800961202     C**************************************************************************
375900961202     C     ANM14X        BEGSR
376000961202     C* LA CREO SOLO SE NON ESISTE UNA SPUNTA DELLA LNA CON DATA=>
376100961210     C     WDCM          IFGT      0
376200961210     C     WDCM          ANDGE     BARDFV
376300961210     C                   MOVEL     'N'           WCREA
376400961210     C                   ELSE
376500961202     C                   EXSR      CTRSPU
376600961210     C                   ENDIF
376700961202     C*
376800961202    5C     WCREA         IFEQ      'S'
376900020213     C     BARFLE        ANDEQ     BTFP
377000020213     C     BARFLE        ORNE      BTFP
377100961202     C                   Z-ADD     CAACRE        COMCAA
377200961202     C                   EXSR      CRTANM
377300961202     C* SOLO PER EXPORT
377400961202     C*
377500961202     C     CAACRE        IFEQ      146
377600961202    6C     WTIBOL        ANDNE     'A'
377700961202     C                   MOVEL     'S'           WTIBOL
377800961202    6C                   ENDIF
377900961202    5C                   ENDIF
378000961202     C*
378100961202     C* SE SPUNTA DA ALTRO AS MA NON DOVEVO CREARE IL COLLO DA INOLTRAR
378200961202     C*  LA CHIUDO SUBITO. ANCHE SE IL COLLO E' CONSEGNATO SENZA SPUNTE
378300020213    5C     BARFLE        IFNE      BTFP
378400961202    6C     WCREA         IFNE      'S'
378500961202     C                   Z-ADD     CAACRE        COMCAA
378600961211     C* IN QUESTO CASO NON DEVO CREARE L'ANOMALIA 620 IN QUANTO
378700961211     C*  LA SPEDIZIONE E' GIA' STATA CONSEGNA A COMUNQUE E' PRESENTE
378800961211     C*  UNA SPUNTA CON DATA >.
378900961211     C                   MOVEL     'N'           W620
379000961202     C                   EXSR      CHIUAN
379100961211     C                   MOVEL     'S'           W620
379200961211     C*
379300961209   X6C                   ELSE
379400961210     C                   EXSR      ANM600
379500961202    6C                   ENDIF
379600961209     C*
379700961202   X5C                   ELSE
379800961210    6C     WCREA         IFEQ      'S'
379900961209     C* PER ESTERO CREO ANOMALIA 645 E 600
380000961209     C                   EXSR      ANM600
380100961210    6C                   ENDIF
380200961210    5C                   ENDIF
380300961202     C*
380400961210     C                   MOVEL     'N'           W620              1
380500961202     C                   Z-ADD     CAACIU        COMCAA
380600961202     C                   EXSR      CHIUAN
380700961210     C                   MOVEL     'S'           W620              1
380800961202     C                   ENDSR
380900961213     C**************************************************************************
381000961213     C* LETTURA FOGLIO DELLA SPUNTA CHE STO CARICANDO E IMPOSTAZIONE
381100961213     C*  CAMPI DI COMOD
381200961213     C**************************************************************************
381300961213     C     BARFGV        BEGSR
381400961213     C*
381500961213    3C     BARKEY        IFNE      SAVKEY
381600961213     C                   MOVEL     BARKEY        SAVKEY
381700961213     C                   Z-ADD     BARNPG        WNPG
381800961213     C                   Z-ADD     BARNFV        WNFV
381900021014     C                   Z-ADD     BARFGS        WFGS
382000070906     C                   movel     BARnps        Wnps
382100021014     C                   MOVEL     'S'           RICBAR            1
382200961213     C                   EXSR      RILFOG
382300980313     C                   MOVEA     FFG           FFS
382400961213     C                   Z-ADD     WDFV          BARDFV
382500961213     C                   MOVEL     WSPG          BARSPG
382600021025     C                   MOVEL     WFGS          wwwFGS
382700021023     C                   Z-ADD     WFLE          BARFLE
382800140616     C                   movel     WFCF          BARFCF
382900970521     C*
383000970521     C     WTTR          IFNE      *BLANKS
383100970520     C     WTTR          LOOKUP    TTR                                    34
383200970520     C                   MOVE      *IN34         WDEFL             1
383300970521     C                   ELSE
383400970521     C                   MOVE      '0'           WDEFL             1
383500970521     C                   ENDIF
383600970521     C*
383700961213    3C                   ENDIF
383800021025     C* IL P.O. GESTIONE LO DEVO SEMPRE IMPOSTARE PERCHÈ PUÒ CAMBIARE
383900021031     C                   MOVEL     WWWFGS        BARFGS
384000070209     c
384100070209     c* Se si tratta di spunta partenza, il BPES deve essere il P.O foglio
384200070209     c*  per problemi con eventuale creazione di anomalie 200
384300070209     c                   eval      wpes=bpes
384400070209     c                   if        barnpg=1
384500070209     c                   eval      bpes=barfgs
384600070209     c                   endif
384700961213     C                   ENDSR
384800970203     C**************************************************************************
384900970203     C* ANOMALIE CHE DEVO SEMPRE CREARE O SEMPRE CHIUDERE SE COLLO
385000970203     C*  SPARATO NELL'AREA DI ARRIVO (COME TERMINAL DI ARRIVO)
385100970203     C**************************************************************************
385200970203     C     ANMSEM        BEGSR
385300970203     C*
385400970203     C* COLLO/BOLLA ARRIVI PRESENTE
385500970204     C* SE CONSEGNATO IL COLLO CREO SEMPRE ANOMALIA --> COLLO CONSEGNAT
385600970204    1C  N05WBOAR         IFEQ      'S'
385700990120    2C     BARDFV        IFGT      WATDCM
385800970204     C     WATDCM        ANDGT     0
385900170328     c* oppure senza consegna ma con cons anomala "S"
386000170328     c     watdcm        oreq      0
386100170328     c     wabcca        andeq     'S'
386200170505     c
386300170505     c*
386400170505     c* se bolla con cons anomala legata, la bolla figlia
386500170505     c*  DEVE essere borderizzata per dare anomalia 138
386600170505     c*  per semplificare leggo della bolla figlia solo il 1° dettaglio colli
386700170505     c                   eval      w138='S'
386800170505    3c                   if        wabcca<>' '  and comnsp>0
386900170505     c     kbolla        chain     fnlbl02l
387000170505    4c                   if        %found(fnlbl02l)
387100170505     c     klbl          chain(n)  fnblp01l
387200170505    5c                   if        %found(fnblp01l) and
387300170505     c                              blpft2=' '
387400170505     c                   eval      w138='N'
387500170505    5c                   endif
387600170505    4c                   endif
387700170505    3c                   endif
387800170505     c
387900170505     c                   if        w138='S'
388000970203     C                   Z-ADD     138           COMCAA
388100970203     C                   EXSR      CRTANM
388200170505     c                   endif
388300170505     c
388400990120    2C                   ENDIF
388500170505     c
388600000705     C* SE COLLO CAT.8 SEGNALA SE LA RELATIVA DISTINTA 4 E'
388700000705     C*  GIA' CHIUSA CON ANOMALIA 139
388800040430     c                   if        watdcm>0  and barnpg=8
388900000705     C* SE NON E' CHIUSA VEDO SE IN FASE DI CHIUSURA: CHIAMO FNLV53R
389000000705     C                   CLEAR                   DSLV53
389100000705     C                   MOVEL     'V'           D53TFO
389200020213     C                   MOVEL     BTFP          D53FEL
389300000705     C                   Z-ADD     BARNFV        D53NFV
389400000705     C                   MOVEL     4             D53NPG
389500000705     C                   MOVEL     BARFGS        D53FGS
389600000705     C                   MOVEL     BARFLE        D53FLF
389700000705     C                   CALL      'FNLV53R'
389800000705     C                   PARM                    DSLV53
389900000705     C*
390000000705     C* FOGLIO CHIUSO (S) O IN FASE DI CHIUSURA (F)
390100000705    3C     D53FCF        IFEQ      'S'
390200000705     C     D53FCF        OREQ      'F'
390300040430     c* non creo se la relativa consegna anomala non lo prevede
390400040430    4c                   if        WABCCA<>*blanks and wabcca<>savcca
390500040430     C                   MOVEL     '7O'          COD
390600040430     C                   MOVEL(P)  wabcca        KEY
390700040430     C     KTAB          CHAIN     TABEL                              41
390800040430     C  N41              MOVEL     TBLUNI        DS7o
390900040430     c   41              clear                   §7o139
391000040430     c                   eval      savcca=wabcca
391100040430    4c                   endif
391200040430     c
391300040430    4c                   if        §7o139=*blanks or wabcca=*blanks
391400000705     C                   Z-ADD     139           COMCAA
391500000705     C                   EXSR      CRTANM
391600040430    4C                   ENDIF
391700040430    3C                   ENDIF
391800000705    2C                   ENDIF
391900000705     C*
392000990120     C* COLLO NON CONSEGNATO
392100990121    2C     WATDCM        IFEQ      0
392200170328     c     wabcca        andne     'S'
392300990120     C* VERIFICO SE DEVO CREARE ANOMALIA SU DANNI
392400000523     C*  NON LO CREO PER LA CATEGORIA 4 E 8
392500990121     C                   MOVEL     'N'           WCAT4             1
392600990126     C* NON GESTISCO IL RITROVAMENTO COLLO
392700990126     C                   MOVEL     'N'           WRITRO
392800990126     C*     GESTISCO LA CREAZIONE ANOMALIE
392900990126     C                   MOVEL     'S'           WCREAN
393000990120     C                   EXSR      CTRDAN
393100990120     C** SE NON TROVATA CA DI COLLO
393200990120     C* VERIFICO SE LA SPEDIZIONE E' SU UNA CA CHIUSA --> CREO
393300990120    3C     WTRDAN        IFEQ      ' '
393400040811      *
393500040811      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
393600040811      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
393700040811      * legate alla spedizione che passo
393800040811     c                   clear                   fidn12ds
393900170505     c***                eval      i12aas = ARTAAS
394000170505     c***                eval      i12lnp = ARTLNP
394100170505     c***                eval      i12nrs = ARTNRS
394200170505     c***                eval      i12nsp = ARTNSP
394300170505     c                   eval      i12aas = comAAS
394400170505     c                   eval      i12lnp = comLNP
394500170505     c                   eval      i12nrs = barNRS
394600170505     c                   eval      i12nsp = comNSP
394700170505     c                   eval      i12fel = 'E'
394800040811     c*
394900040811     c                   call      'FIDN12R'
395000040811     c                   parm                    fidn12ds
395100040811     c*
395200040811     c                   setoff                                       33
395300040811     c*
395400040811     c* se non ci sono errori
395500040811     c                   if        o12err <> ' '
395600040811     c                   seton                                        33
395700040811     c                   else
395800040811     c* se numero di CA trovate maggiore di zero
395900040811     c                   if        o12nca = 0
396000040811     c                   seton                                        33
396100040811     c                   else
396200040811     c                   z-add     1             jj                2 0
396300040811     c                   dow       jj <= o12nca and *in33 = *off
396400040811     C     skDch(jj)     IFGT      0
396500040811     C     BARNPG        IFNE      4
396600040811     C     BARNPG        ANDNE     8
396700040811     C*
396800040811     C* Valorizzo i campi DCT* utilizzati nella routine CRE151 con quelli
396900040811     C* ritornati dal *pgm d utilità FIDN12R
397000040811     C                   movel     skCch(jj)     DCTCCH
397100040811     C*
397200040811     C                   EXSR      CRE151
397300040811     C                   ELSE
397400040811     C* SE E' CAT 4 E 8 LA CHIUDO
397500040811     C                   Z-ADD     151           COMCAA
397600040811     C                   EXSR      CHIUAN
397700040811     C                   ENDIF
397800040811     C**
397900040811     C                   SETON                                        33
398000040811     C**
398100040811     C                   ENDIF
398200040811     c*
398300040811     c                   add       1             jj
398400040811     c                   enddo
398500040811     c*
398600040811     c                   seton                                        33
398700040811     c*
398800040811     c                   endif
398900040811     c                   endif
399000040811     C**
399100990120    3C                   ENDIF
399200990120     C**
399300990120    2C                   ENDIF
399400990120    1C                   ENDIF
399500021023     C*
399600970203     C* CHIUDO LE ANOMALIE DI COLLO MANCATE
399700020204    1C     WAGARR        IFEQ      'S'
399800970203     C* PER COLLO NON LOCALE
399900021023     C     WAGPAR        OREQ      ' '
400000970203     C* PER BOLLA NON TROVATA IN PARTENZA
400100970203     C     WBOPAR        OREQ      'N'
400200021023     C**
400300021023     C* NON AGGIORNO CON SPUNTA ENTRATA
400400021023     C* SE NON E' LOCALE O DISGUIDO IN AREA AGGIORNO SEMPRE
400500021023    2C     WSPLOC        IFEQ      ' '
400600021023     C     WSPLOC        OREQ      'D'
400700021023     C* SE E' LOCALE SOLO SE E' SPUNTA FATTA CON CAT.ARRIVI  DAL P.O.
400800021023     C*  LINEA ARRIVO O TERMINAL ARRIVO
400900021023     C     WSPLOC        OREQ      'A'
401000021023     C     BARNPG        ANDNE     5
401100021023     C     BARNPG        ANDNE     1
401200021023     C*
401300021023     C     WSPLOC        OREQ      'A'
401400021023     C     BARNPG        ANDEQ     1
401500021023     C     WDEFL         ANDEQ     '1'
401600021023     C*
401700021023     C     WSPLOC        OREQ      'T'
401800021023     C     BARNPG        ANDNE     5
401900021023     C     BARNPG        ANDNE     1
402000021023     C*
402100021023     C     WSPLOC        OREQ      'T'
402200021023     C     BARNPG        ANDEQ     1
402300021023     C     WDEFL         ANDEQ     '1'
402400021023     C*
402500970203     C*
402600970203     C* COLLO ARRIVATO MANCANTE --> SOLO SE SPARATO SUL SISTEMA
402700041018     c* L'anomalia 15 non la chiudo se altra spunta presente in cat.arrivi
402800041018     c*  con stessa data
402900041018     c                   eval      wcontrdfv='S'
403000041018     c                   eval      wstess='S'
403100041018     c                   eval      wchiu='S'
403200970203     C                   Z-ADD     15            COMCAA
403300970203     C                   MOVEL     'TR'          SAVCCH
403400970203     C                   EXSR      CHIUAN
403500970203     C*
403600970203     C* COLLO PARTITO NON ARRIVATO
403700970203     C                   MOVEL     'AR'          SAVCCH
403800970203     C                   Z-ADD     5             COMCAA
403900970203     C                   EXSR      CHIUAN
404000970203     C* MANCA COLLO MANCA BOLLA
404100970203     C                   MOVEL     'AR'          SAVCCH
404200970203     C                   Z-ADD     12            COMCAA
404300970203     C                   EXSR      CHIUAN
404400990521    2C                   ENDIF
404500990521    1C                   ENDIF
404600040430     c
404700040430     c* Chiudo fuori posto bolla partenza e fuori posto bolla transito
404800040430     C                   Z-ADD     120           COMCAA
404900040430     C                   EXSR      CHIUAN
405000040430     C                   Z-ADD     125           COMCAA
405100040430     C                   EXSR      CHIUAN
405200970203     C                   ENDSR
405300970204     C**************************************************************************
405400970204     C* ANOMALIA FUORI POSTO SEGNACOLLO BOLLA IN PARTENZA
405500970204     C**************************************************************************
405600970204     C     ANM120        BEGSR
405700970204     C                   Z-ADD     120           COMCAA
405800970204     C*
405900970204     C* VALGONO SOLTANTO LE SPUNTE SPARATE NELL'AREA DI PARTENZA
406000020213    1C  N05BARFLE        IFEQ      BTFP
406100970204     C* SPUNTA DEL TERMINAL DI PARTENZA
406200020213    2C     BARFGS        IFEQ      BTFP
406300970204     C* SE COLLO NON SPUNTATO NELLE CATEGORIE PARTENZA
406400970204    3C     BARNPG        IFNE      5
406500970204     C     BARNPG        ANDNE     1
406600970204     C     BARSPG        ANDNE     'P'
406700970204     C                   EXSR      CRTANM
406800970204   X3C                   ELSE
406900970204     C* SE COLLO     SPUNTATO NELLE CATEGORIE PARTENZA
407000970204     C                   EXSR      CHIUAN
407100970204    3C                   ENDIF
407200970204     C*
407300970204   X2C                   ELSE
407400970204     C* COLLO SPUNTATO DA UN 2 LIVELLO IN PARTENZA
407500970204     C* SE COLLO NON SPUNTATO NELLE CATEGORIE PARTENZA
407600970204    3C     BARNPG        IFNE      5
407700970204     C     BARSPG        ANDNE     'P'
407800970204     C                   EXSR      CRTANM
407900970204     C                   ELSE
408000970204     C* SE COLLO     SPUNTATO NELLE CATEGORIE PARTENZA
408100970204     C                   EXSR      CHIUAN
408200970204    3C                   ENDIF
408300970204    2C                   ENDIF
408400970204    1C                   ENDIF
408500970204     C                   ENDSR
408600970204     C**************************************************************************
408700970204     C* ANOMALIE CHE CREO SOLO SE HA SPARATO LA LINEA DI ARRIVO DEL COL
408800970204     C**************************************************************************
408900970204     C     ANMARR        BEGSR
409000970204     C*
409100970204     C* COLLO/BOLLA ARRIVI PRESENTE
409200970204    1C  N05WBOAR         IFEQ      'S'
409300990126     C                   MOVEL     'N'           WCAT4             1
409400990126     C*     GESTISCO IL RITROVAMENTO COLLO
409500990126     C                   MOVEL     'S'           WRITRO
409600020204     C* NON GESTISCO LA CREAZIONE ANOMALIE CHE GIA' FACCIO IN CHIUAN
409700990126     C                   MOVEL     'N'           WCREAN
409800990126     C                   EXSR      CTRDAN
409900970204     C*
410000970204    2C     WATDCM        IFEQ      0
410100170328     c* in caso di cons anomala "S" il collo potrebbe non avere la data di cons
410200170328     c     wabcca        andne     'S'
410300170328     c
410400970204     C     BARDFV        ORGT      WATDCM
410500170329     c     watdcm        andgt     0
410600970204     C*
410700970204    3C     WABNDC        IFNE      0
410800970204     C     WATDCM        ANDEQ     0
410900170328     c*
411000970204     C* BOLLA IN CONSEGNA
411100970204    4C     BARNPG        IFNE      4
411200000523    4C     BARNPG        ANDNE     8
411300970204     C* SE NO SPUNTA ARRIVI CREO ANOMALIA: BOLLA IN CONSEGNA FUORI POST
411400970204    5C     BARNPG        IFNE      2
411500970204     C                   Z-ADD     135           COMCAA
411600970204     C                   EXSR      CRTANM
411700970204    5C                   END
411800970204     C*
411900000607    4C                   ENDIF
412000000607     C**
412100000607    4C     BARNPG        IFEQ      4
412200970204     C*
412300970204     C*        ANOMALIA: "IN CONSEGNA SU ALTRA DISTINTA"
412400970204    5C     WABNDC        IFNE      BARNFV
412500970204     C                   Z-ADD     150           COMCAA
412600970204     C                   EXSR      CRTANM
412700000523     C*
412800970204   X5C                   ELSE
412900970204     C*
413000970204     C* CHIUDO ANOMALIA: "IN CONSEGNA SU ALTRA DISTINTA"
413100970204     C                   Z-ADD     150           COMCAA
413200970204     C                   MOVEL     '  '          SAVCCH            2
413300970204     C                   EXSR      CHIUAN
413400970204    5C                   ENDIF
413500970204    4C                   ENDIF
413600970204    3C                   ENDIF
413700970204     C*
413800970204     C* BOLLA ARRIVI FUORI POSTO
413900161201     c* In caso di wgiac = 'D' non devo dare il fuori posto
414000161201    3C     WGIAC         IFEQ      ' '
414100970204     C*
414200161201    4C     BARSPG        IFEQ      'G'
414300161201    4C     BARSPG        oreq      'Z'
414400970204     C                   Z-ADD     130           COMCAA
414500970204     C                   EXSR      CRTANM
414600970204    4C                   ENDIF
414700161021     c
414800970204     C* IN ENTRATA SOLO SE NON E' BOLLA LOCALE
414900970204    4C     BARNPG        IFEQ      5
415000970204     C     WTIBOL        ANDNE     'L'
415100970204     C     WABNDC        ANDEQ     0
415200970204     C                   Z-ADD     130           COMCAA
415300970204     C                   EXSR      CRTANM
415400970204    4C                   ENDIF
415500970204    3C                   ENDIF
415600970204     C*
415700970204     C* BOLLA GIACENTE FUORI POSTO
415800161201     c* per ora il fuori posto lo devo dare anche per le
415900161201     c*  giacenze a partare a mag giac. mentre con i 2gg in IMA
416000161201     c*  dovrò dare l'anomalia soltanto se già a IMG se spuntato in IMA
416100161201     c*  per le altre categorie errore sempre
416200970204    3C     BARSPG        IFNE      'G'
416300161021    3C     BARSPG        andne     'Z'
416400161201     C**** WGIAC         ANDEQ     'S'
416500161201     C     WGIAC         ANDne     ' '
416600970205     C     BARDFV        ANDGT     GCPDGC
416700170125     c* se WGIAC = "D" e filiale arrivo partita con procedura NEWIMA
416800170125     c*  non creao l'anomalia
416900170125    4c                   if        wgiac='D'  and allnewima<>'S'
417000170125     c                   movel     barlna        w003a
417100170125     c                   if         %lookup(w003a:skFilOk)>0
417200170125     c                   eval      allnewima='Y'
417300170125     c                   else
417400170125     c                   clear                   allnewima
417500170125     c                   endif
417600170125    4c                   endif
417700170125     c
417800170125    4c                   if        wgiac='S' or (barspg<>'A' and
417900170125     c                             barnpg<>2) or
418000170125     c                             (wgiac='D' and allnewima=' ')
418100970204     C                   Z-ADD     140           COMCAA
418200970204     C                   EXSR      CRTANM
418300170125    4c                   endif
418400170125     c
418500970204    3C                   ENDIF
418600970204     C*
418700970204    2C                   ENDIF
418800970204     C**
418900970204     C* C H I U D O
419000970204     C* BOLLA CONSEGNA
419100970204    2C     BARNPG        IFEQ      4
419200970204     C                   Z-ADD     135           COMCAA
419300970204     C                   EXSR      CHIUAN
419400970204    2C                   END
419500970204     C* BOLLA ARRIVI
419600970204    2C     BARNPG        IFEQ      2
419700970204     C     BARNPG        OREQ      4
419800000523     C     BARNPG        OREQ      8
419900970204     C     BARSPG        OREQ      'A'
420000970204     C                   Z-ADD     130           COMCAA
420100970204     C                   EXSR      CHIUAN
420200970204    2C                   END
420300970204     C* BOLLA GIACENZE
420400970204    2C     BARNPG        IFEQ      3
420500970204     C     BARSPG        ANDEQ     'G'
420600161028    2C     BARNPG        orEQ      3
420700161028     C     BARSPG        ANDEQ     'Z'
420800970204     C                   Z-ADD     140           COMCAA
420900970204     C                   EXSR      CHIUAN
421000970204    2C                   END
421100970204     C*
421200970204    1C                   ENDIF
421300970204     C                   ENDSR
421400970218     C**************************************************************************
421500970218     C* SCRITTURA DI FNAGB PER AGGIORNAMENTO  BOLLE
421600970218     C**************************************************************************
421700970217     C     WRTAGB        BEGSR
421800970217     C* LA PRIMA VOLTA NON SCRIVO
421900041105    1C     WAGGF         IFEQ      'S'
422000180131     c     savnsp        andgt     0
422100180131     c
422200970217     C     KSAV          CHAIN     FNAGB01L                           34
422300970217     C   34              CLEAR                   FNAGB000
422400970218     C                   MOVEL     SAVTBO        AGBTBO
422500970217     C                   MOVEL     KAGB          AGBAGB
422600970217     C                   MOVEL     SAVAAS        AGBAAS
422700970217     C                   MOVEL     SAVLNP        AGBLNP
422800970217     C                   MOVEL     SAVNRS        AGBNRS
422900970217     C                   MOVEL     SAVNSP        AGBNSP
423000970217     C     WFBS          IFEQ      'S'
423100970217     C                   MOVEL     WFBS          AGBFBS
423200970217     C                   ENDIF
423300970217     C     WFVC          IFEQ      'S'
423400970217     C                   MOVEL     WFVC          AGBFVC
423500970217     C                   ENDIF
423600990728     C     WFPC          IFEQ      'S'
423700990728     C                   MOVEL     WFPC          AGBFPC
423800990728     C                   ENDIF
423900970217     C*
424000981124     C   34              WRITE     FNAGB000                             99
424100970217     C  N34              UPDATE    FNAGB000
424200041105     c
424300041105     c* Se previsto scrivo anche per l'altra bolla
424400041105     c                   eval      wprimotbo=savtbo
424500041105     c
424600041105     c                   if        wagbart='S'
424700041105     c                   eval      savtbo='A'
424800041105     c                   EXSR      ALTROAGB
424900041105     c                   endif
425000041105     c                   eval      savtbo=wprimotbo
425100160421     c
425200160421     c* Scrivo record per la zona
425300160421     c                   if        wznc<>*blanks
425400160421     C                   CLEAR                   FNAGB000
425500160421     C                   MOVEL     SAVTBO        AGBTBO
425600160421     C                   MOVEL     wznc          AGBAGB
425700160421     C                   MOVEL     SAVAAS        AGBAAS
425800160421     C                   MOVEL     SAVLNP        AGBLNP
425900160421     C                   MOVEL     SAVNRS        AGBNRS
426000160421     C                   MOVEL     SAVNSP        AGBNSP
426100160421     C                   WRITE     FNAGB000                             99
426200160421     c                   endif
426201180206     c* Scrivo agb per ripristino spedizione chiusa in partenza per merce mai af
426202180206     c                   if        waggRB='S'
426203180206     C                   CLEAR                   FNAGB000
426204180206     C                   MOVEL     'P'           AGBTBO
426205180206     C                   MOVEL     'RB'          AGBAGB
426206180206     C                   MOVEL     SAVAAS        AGBAAS
426207180206     C                   MOVEL     SAVLNP        AGBLNP
426208180206     C                   MOVEL     SAVNRS        AGBNRS
426209180206     C                   MOVEL     SAVNSP        AGBNSP
426210180206     C                   WRITE     FNAGB000                             99
426211180206     C                   clear                   waggRB
426214180206     c                   ENDIF
426300041105     c
426400970217    1C                   ENDIF
426500970218     C*
426600970218     C                   CLEAR                   WFBS
426700970218     C                   CLEAR                   WFVC
426800990728     C                   CLEAR                   WFPC
426900160421     C                   CLEAR                   Wznc
427000041105     C                   CLEAR                   Wagbart
427100970217     C                   ENDSR
427200041105     C**************************************************************************
427300041105     c* scrivo fnagb per l'altro tipo bolla
427400041105     C**************************************************************************
427500041105     c     ALTROAGB      begsr
427600041105     C     KSAV          CHAIN     FNAGB01L                           34
427700041105     c                   eval      agbtbo=savtbo
427800041105     c                   if        *in34
427900041105     c                   clear                   agbfbs
428000041105     C                   WRITE     FNAGB000                             99
428100041105     c                   else
428200041105     c* Se record trovato aggiorno solo i campi del peso/volume
428300041105     C     WFVC          IFEQ      'S'
428400041105     C                   MOVEL     WFVC          AGBFVC
428500041105     C                   ENDIF
428600041105     C     WFPC          IFEQ      'S'
428700041105     C                   MOVEL     WFPC          AGBFPC
428800041105     C                   ENDIF
428900041105     c
429000041105     C                   UPDATE    FNAGB000
429100041105     c                   endif
429200041105     c                   endSR
429300980120     C**************************************************************************
429400980120     C* SCRITTURA DI FNAGB PER AGGIORNAMENTO  BOLLE
429500980120     C**************************************************************************
429600980120     C     WR1AGB        BEGSR
429700110103      *
429800030528     c                   setoff                                       45
429900110103      *
430000980120     C     KEAGB         CHAIN     FNAGB01L                           34
430100980120     C   34              CLEAR                   FNAGB000
430200980120     C                   MOVEL     WTBO          AGBTBO
430300980120     C                   MOVEL     KAGB          AGBAGB
430400980120     C                   MOVEL     WDAAS         AGBAAS
430500980120     C                   MOVEL     WDLNP         AGBLNP
430600980120     C                   MOVEL     WDNRS         AGBNRS
430700980120     C                   MOVEL     WDNSP         AGBNSP
430800160726     c*
430900160726     c* Se richiesto, passo flag di rielaborazione statistiche partenze
431000160726     c                   if        welasta_p='S'
431100160726     c                   clear                   dagbflo
431200160726     c                   eval      §agbdsf='S'
431300160726     c                   eval      agbflo=dagbflo
431400160726     c                   endif
431500980120     C*
431600030528     C   34              WRITE     FNAGB000                             45
431700980120     C  N34              UPDATE    FNAGB000
431800980120     C*
431900160726     c                   clear                   welasta_p
432000160726     C*
432100980120     C                   ENDSR
432200970609     C**************************************************************************
432300970609     C* SCRITTURA DI FNSTA PER RIELABORAZIONE STATISTICA ARRIVI
432400970609     C**************************************************************************
432500970609     C     WRTSTA        BEGSR
432600970609     C* LA PRIMA VOLTA NON SCRIVO
432700970609    1C     WARDAM        IFGT      0
432800970609     C* VECCHIA DATA
432900970609     C     WSVDAM        IFGT      0
433000970609     C                   CLEAR                   SA2TLA
433100070221     C     dls45FLG      IFEQ      'C'
433200970609     C                   MOVEL     'L'           SA2TLA
433300970609     C                   END
433400970609     C                   MOVEL     'A'           SA2TRC
433500970609     C                   Z-ADD     WSVDAM        SA2DTS
433600970609     C                   MOVE      *ZEROS        SA2LNA
433700970609     C                   CALL      'FNLSA2R3'
433800970609     C                   PARM                    SA2TLA            1
433900970609     C                   PARM                    SA2TRC            1
434000970609     C                   PARM                    SA2DTS            8 0
434100970609     C                   PARM                    SA2LNA            3 0
434200970609     C                   END
434300970609     C* NUOVA DATA
434400970609     C                   Z-ADD     WARDAM        SA2DTS
434500970624     C                   MOVEL     'A'           SA2TRC
434600970609     C                   CALL      'FNLSA2R3'
434700970609     C                   PARM                    SA2TLA            1
434800970609     C                   PARM                    SA2TRC            1
434900970609     C                   PARM                    SA2DTS            8 0
435000970609     C                   PARM                    SA2LNA            3 0
435100970609    1C                   ENDIF
435200970609     C*
435300970609     C                   ENDSR
435400980928     C**************************************************************************
435500980928     C* MEMORIZZO I FOGLI NON TROVATI DA INVIARE A SEDE CON MSG
435600980928     C**************************************************************************
435700111018     C**   INVMSG        BEGSR
435800980928     C*
435900111018     C**                 Z-ADD     1             EE                3 0
436000111018     C**                 SETON                                        95
436100111018     C**                 CLEAR                   WPRES
436200111018     C**                 MOVE      WNFV          WNFVA             5
436300111018    4C**   *IN95         DOWEQ     *ON
436400111018     C**   WPRES         ANDEQ     ' '
436500980928     C* IL FOGLIO CORRISPONDE: NON LO RIMEMORIZZO
436600111018     C**   WNFVA         LOOKUP    ERF(EE)                                95
436700111018    5C** 95WNPG          IFEQ      ERC(EE)
436800111018     C**   WFGS          ANDEQ     ERE(EE)
436900111018     C**   WNPS          ANDEQ     ERP(EE)
437000111018     C**                 MOVEL     'S'           WPRES             1
437100111018    5C**                 ENDIF
437200111018     C**                 ADD       1             EE
437300111018    4C**                 ENDDO
437400980928     C* SE NON E' PRESENTE RIMEMORIZZO
437500111018    4C**   WPRES         IFEQ      ' '
437600111018     C**                 Z-ADD     1             EE
437700111018     C**   '     '       LOOKUP    ERF(EE)                                95
437800111018     C** 95              MOVE      WNFVA         ERF(EE)
437900111018     C** 95              Z-ADD     WNPG          ERC(EE)
438000111018     C** 95              Z-ADD     WFGS          ERE(EE)
438100111018     C** 95              movel     Wnps          ERP(EE)
438200111018    5C**   EE            IFEQ      8
438300111018     C**                 EXSR      TRASMI
438400111018    5C**                 ENDIF
438500111018    4C**                 ENDIF
438600111018     C*+                 ENDSR
438700990120     C**************************************************************************
438800990120     C* SE DEVO CREO ANOMALIA 152 / 153
438900990120     C**************************************************************************
439000990120     C     CRT15X        BEGSR
439100990120     C** SE LA C.A. E' APERTA, VERIFICO SE AVARIA E RESO O DISTRUTTA
439200990120    1C     DCTDSD        IFEQ      'R'
439300990120     C     DCTDSD        OREQ      'D'
439400990126     C**
439500990126     C                   EXSR      CERTAD
439600990120     C* CONTROLLO SE TIPO ANOMALIA E' AVARIA
439700990120     C** CREO ANOMALIA 152 0 153
439800990120    2C     §TARGR        IFEQ      'V'
439900990120     C*
440000990120     C     DCTDSD        IFEQ      'R'
440100990120     C                   Z-ADD     152           COMCAA
440200990120     C                   ELSE
440300990120     C                   Z-ADD     153           COMCAA
440400990120     C                   ENDIF
440500990120     C**
440600990120     C                   EXSR      CRTANM
440700990120    2C                   ENDIF
440800990120    1C                   ENDIF
440900990120     C                   ENDSR
441000990120     C**************************************************************************
441100990120     C* CONTROLLO SE PRESENTE CA PER IL COLLO
441200990120     C**************************************************************************
441300990120     C     CTRDAN        BEGSR
441400990126     C*
441500990120     C                   CLEAR                   WTRDAN
441600990120     C     KBAR          CHAIN     FNDCD02L                           33
441700990126     C*
441800990120    1C     *IN33         IFEQ      *OFF
441900990120     C     DCDATB        ANDEQ     ' '
442000990120     C     KDCT          CHAIN     FNDCT01L                           33
442100050223      * se dctdt2 > 0 la c.a. non è valida x la filiale
442200050224     c                   If        Not *In33 and dctdt2 > 0
442300050224     c                   Eval      *In33 = *On
442400050224     c                   EndIf
442500050224     C
442600990120    2C     *IN33         IFEQ      *OFF
442700990120     C     DCTATB        ANDEQ     ' '
442800990126     C* TROVATA C.A. NON ANNULLATA
442900990120     C                   MOVEL     'S'           WTRDAN            1
443000990126     C**
443100990126     C* CONTROLLO SE DEVO IMPOSTARE FLAG COLLO RITROVATO
443200990126    3C     WRITRO        IFEQ      'S'
443300990126     C                   EXSR      CERTAD
443400990126    4C     §TARIT        IFEQ      'S'
443500990126     C     DCDDFV        ANDEQ     0
443600990126     C                   Z-ADD     BARDFV        DCDDFV
443700990126     C                   Z-ADD     BARFGS        DCDFGS
443800990126     C                   Z-ADD     BARNPS        DCDNPS
443900990126     C                   MOVEL     'S'           DCDFAR
444000990126     C                   UPDATE    FNDCD000
444100990126    4C                   ENDIF
444200990126    3C                   ENDIF
444300990126     C**
444400990126     C                   UNLOCK    FNDCD02L
444500990126     C**
444600990126     C** CONTROLLO SE DEVO CREARE LE ANOMALIE
444700990126    3C     WCREAN        IFEQ      'S'
444800990126     C**
444900990126     C* ANOMALIA 151 - COLLO DI CA CHIUSA
445000990126    4C     DCTDCH        IFGT      0
445100990121     C* IN ARRIVO NON LA CREO PER SPUNTA DI CATEGORIA 4
445200990126    5C     WCAT4         IFEQ      'N'
445300990121     C     BARNPG        ANDNE     4
445400000522     C     BARNPG        ANDNE     8
445500990121     C     WCAT4         OREQ      'S'
445600990120     C                   EXSR      CRE151
445700990126   X5C                   ELSE
445800990121     C                   Z-ADD     151           COMCAA
445900990121     C                   EXSR      CHIUAN
446000990126    5C                   ENDIF
446100990126   X4C                   ELSE
446200990120     C* ANOMALIE 152 O 153
446300990126    5C     DCDDCH        IFEQ      0
446400990120     C                   EXSR      CRT15X
446500990126    5C                   ENDIF
446600990126    4C                   ENDIF
446700990121    3C                   ENDIF
446800990120    2C                   ENDIF
446900990120     C**
447000990120   X1C                   ELSE
447100990120     C* ANNULLATO COME SE NON CI FOSSE
447200990120     C                   SETON                                        33
447300990120    1C                   ENDIF
447400990120     C                   ENDSR
447500990120     C**************************************************************************
447600990120     C* CREO ANOMALIA 151 SE LO PREVEDE LA CAUSALE CHIUSURA
447700990120     C**************************************************************************
447800990120     C     CRE151        BEGSR
447900990120     C     DCTCCH        IFNE      *BLANKS
448000990120     C                   CLEAR                   DSBS02
448100990120     C                   MOVEL     'C'           T02MOD
448200990120     C                   MOVEL     KNSIF         T02SIF
448300990121     C                   MOVEL     'CCH'         T02COD
448400990120     C                   MOVEL     DCTCCH        T02KE1
448500990120     C                   CALL      'TIBS02R'
448600990120     C                   PARM                    KPJBA
448700990120     C                   PARM                    DSBS02
448800990121    2C     T02ERR        IFEQ      ' '
448900990121     C                   MOVEL     T02UNI        DCCH
449000990121     C                   ELSE
449100990121     C                   MOVEL     'S'           §CHCRA
449200990121    2C                   ENDIF
449300990120     C                   ENDIF
449400990120     C**
449500990120     C* CREO
449600990120     C     DCTCCH        IFEQ      *BLANKS
449700990120     C     §CHCRA        OREQ      'S'
449800990120     C                   Z-ADD     151           COMCAA
449900990120     C                   EXSR      CRTANM
450000990120     C                   ENDIF
450100990120     C                   ENDSR
450200990126     C**************************************************************************
450300990126     C* CERCO TABELLA RECORD TIPO ANOMALIA TAD
450400990126     C**************************************************************************
450500990126     C     CERTAD        BEGSR
450600990126    1C     DCTTAD        IFNE      SAVTAD
450700990126     C                   CLEAR                   DSBS02
450800990126     C                   MOVEL     'C'           T02MOD
450900990126     C                   MOVEL     KNSIF         T02SIF
451000990126     C                   MOVEL     'TAD'         T02COD
451100990126     C                   MOVEL     DCTTAD        T02KE1
451200990126     C                   CALL      'TIBS02R'
451300990126     C                   PARM                    KPJBA
451400990126     C                   PARM                    DSBS02
451500990126     C**
451600990126    2C     T02ERR        IFEQ      ' '
451700990126     C                   MOVEL     T02UNI        DTAD
451800990126     C                   ELSE
451900990126     C                   MOVEL     'V'           §TARGR
452000990126     C                   MOVEL     'S'           §TARIT
452100990126    2C                   ENDIF
452200990126     C                   MOVEL     DCTTAD        SAVTAD
452300990126    1C                   ENDIF
452400990126     C                   ENDSR
452500030723     C**************************************************************************
452600030723     C* CERCO TABELLA VARIABILI PGM PARTENZE
452700030723     C**************************************************************************
452800030723     C     CERVPO        BEGSR
452900030723      *
453000030723      * CHAIN A VARIABILI PARTENZE PER PRENDERE MAX PESO E VOLUME CARICABILI
453100030723     C                   CLEAR                   DVPO
453200030723     C                   CLEAR                   DSBS02
453300030723     C                   MOVEL     'C'           T02MOD
453400030723     C                   MOVEL     KNSIF         T02SIF
453500030723     C                   MOVEL     'VPO'         T02COD
453600030723     C                   MOVEL     'VPO'         T02KE1
453700030723     C                   CALL      'TIBS02R'
453800030723     C                   PARM                    KPJBA
453900030723     C                   PARM                    DSBS02
454000030723     C**
454100030723    2C     T02ERR        IFEQ      ' '
454200030725     C                   MOVEL     T02UNI        DVPO
454300030723    2C                   ENDIF
454400110103      *
454500030723     C                   ENDSR
454600000927     C**************************************************************************
454700000927     C* CONTROLLO SE POSSO CREARE ANOMALIE DI MANCA RECOD SECONDO LA
454800000927     C*  ATA FOGLIO E I GIONRI DI PULIZIA
454900000927     C**************************************************************************
455000000927     C     CTRDAT        BEGSR
455100000927     C*
455200000927     C* A CAMBIO DATA RICONTROLLO
455300000927    1C     BARDFV        IFNE      SA2DFV
455400000927     C                   CLEAR                   WCRE10
455500000927     C*
455600000927     C* SE FOGLIO UGUALE A UDATE, VA SICURAMENTE BENE
455700000927    2C     BARDFV        IFEQ      WDATE1
455800000927     C                   MOVEL     'S'           WCRE10            1
455900000927   X2C                   ELSE
456000000927     C* CONTROLLO CON I GG DI PULIZIA
456100000927      * DATA FOGLIO MAGGIORE DATA ULTIMO UTILIZZO: POSSO CREARE ANOM
456200000927    3C     BARDFV        IFGE      PRIDAT
456300000927     C                   MOVEL     'S'           WCRE10            1
456400000927    3C                   ENDIF
456500000927    2C                   ENDIF
456600000927     C*
456700000927     C                   Z-ADD     BARDFV        SA2DFV
456800000927    1C                   ENDIF
456900130301     c*
457000130301     c* Se è da creare --> controllo se si tratta di fil che fa partire colli
457100130301     c*  di altro TFP e non la creo lo stesso
457200130301     c                   clear                   wlocalxco         1
457300130301     c
457400130301     c                   if        wcre10='S' and ( bolxco='W'  or
457500130301     c                             (wesbtt='N' and wesblt='N' and wesart='N'
457600130301     c                              and barnrs>0))
457700130301     c* se si tratta di spunta partenza --> creo la 57
457800130301     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
457900130301     c                             or barnpg=1
458000130301     c                   exsr      CtrXCO
458100130301     c                   if        Indx>0
458200130301     c                   eval      Wlocalxco='S'
458300130301     c                   endif
458400130301     c                   endif
458500130301     c                   endif
458600000927     C*
458700010607     C                   ENDSR
458800010607     C**************************************************************************
458900070131     C* MEMORIZZAZIONE SPUNTE DI CONSEGNA DOPPIE SU FNBRVE0F
459000010607     C**************************************************************************
459100010607     C     SRBRVE        BEGSR
459200070710     c* Se devo tenere una spuynta solo memeorizzo per categoria
459300070710     c*  e segnacollo
459400070710     c                   if        nst(WA)='1'
459500070710     c
459600070131     C     KBRVE2        SETLL     FNBRVE2L                               33
459700010607     C     *IN33         IFEQ      *OFF
459800010607     C* SPUNTA NON ESISTENTE: MEMORIZZO ANCHE LA VECCHIA
459900070131     C                   WRITE     FNBRVE
460000010607     C                   ENDIF
460100070131     C* Salvo campi di FNBRV che potrebbero rimanere invariati
460200070131     C* Li dovrò reimpostare dopo la scrittura di FNBRVe per poi
460300010608     C* fare i giusti aggiornamenti
460400010608     C                   MOVE      BRVATR        SAWATR
460500080317     C                   MOVE      BRVTSU        SAWTSU
460600080317     c                   movel     brv           sawbrv
460700010608     C* IN OGNI CASO MEMORIZZO LA SPUNTA NUOVA
460800010608     C                   MOVEL     'S'           WBRVE             1
460900010608     C                   EXSR      RIEMPI
461000010608     C                   CLEAR                   WBRVE
461100070131     C                   WRITE     FNBRVE
461200070131     C* Reimposto campi di FNBRV come da lettura iniziale
461300010608     C                   MOVE      SAWATR        BRVATR
461400080317     C                   MOVE      SAWtsu        BRVtsu
461500080317     C                   MOVE      SAWbrv        BRV
461600110103     c*
461700070710     c                   else
461800070710     c* Se devo memorizzare le spunte senza anomalia controllo per foglio
461900070710     C     KBRVE1        SETLL     FNBRVE1L                               33
462000070710     C     *IN33         IFEQ      *OFF
462100070710     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
462200070710     C                   WRITE     FNBRVE
462300070710     C                   ENDIF
462400070710     C                   ENDIF
462500070710     c*
462600010607     C                   ENDSR
462700020124     C**************************************************************************
462800020204     C* CERCO TERMINAL PARTENZA LNP COLLO
462900020124     C**************************************************************************
463000020204     C     TERPAR        BEGSR
463100020124     C                   CLEAR                   DSLV55
463200020124     C                   MOVEL     'P'           D55TPT
463300020124     C                   MOVEL     LNPB          D55LIN
463400020124     C                   MOVEL     BARDFV        D55DRF
463500020213     C                   EXSR      FNLV55
463600020204     C                   MOVE      D55TFP        LNPTFP
463700020204     C**
463800020204     C                   ENDSR
463900020204     C**************************************************************************
464000020204     C* CERCO TERMINAL ARRIVO LNA COLLO
464100020204     C**************************************************************************
464200020204     C     TERARR        BEGSR
464300020124     C**
464400020124     C* LINEA DI ARRIVO
464500020124     C     BARLNA        IFNE      SVVLNA
464600020124     C     LNPB          ORNE      SVVLNP
464700020204     C     BARDFV        ORNE      SVVDFV
464800020124     C*
464900020124     C* TEMINAL DI ARRIVO
465000020124     C                   CLEAR                   DSLV55
465100020124     C                   MOVEL     ' '           D55TPT
465200020124     C                   MOVEL     BARDFV        D55DRF
465300020124     C                   MOVEL     BARLNA        D55LIN
465400020124     C                   MOVEL     LNPB          D55LNP
465500020213     C                   EXSR      FNLV55
465600020124     C**
465700020213     C                   MOVEL     D55TFA        LNATFA
465800020124     C*
465900020124     C                   MOVEL     BARLNA        SVVLNA
466000020204     C                   MOVEL     LNPB          SVVLNP
466100020204     C                   MOVEL     BARDFV        SVVDFV
466200020124     C                   ENDIF
466300020124     C                   ENDSR
466400020124     C**************************************************************************
466500020124     C* AGGIORNO LE BOLLE PARTENZA
466600020124     C**************************************************************************
466700020124     C     AGGBLT        BEGSR
466800060223     C                   CLEAR                   WEND              1
466900020124     C* NON FACCIO AGGIORNAMENTI SE SPUNTA DI COLLO DISGUIDATO
467000070221    1C     dls45FLG      IFNE      '6'
467100020124     C*
467200020124     C                   MOVEL     'P'           WTIBOL            1
467300020124     C                   MOVEL     'S'           WBOPAR
467400020124     C*
467500020124     C* Se collo già consegnato e spunta partenza di pistola manuale
467600020124     C* ignoro la spunta
467700020124    3C     BARNPG        IFEQ      1
467800020124     C     BLTDCM        ANDNE     *ZEROS
467900020124     C     RPS(Y)        ANDEQ     'M'
468000020124     C                   MOVE      'N'           OKAGG
468100020201     C                   MOVEL     '1'           WEND
468200020204     C                   UNLOCK    FNBLT27L
468300020201     C                   GOTO      ENDBLT
468400020124    3C                   END
468500020204     C*  SE NON IMPOSTATO VOLUME E PESO DA PARTENZA
468600020204     C     WWFVC         IFEQ      ' '
468700020124     C                   MOVEL     'P'           WWFVC
468800020204     C                   ENDIF
468900020124     C*
469000020124     C                   MOVEL     'P'           BLTTBO
469100020124     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
469200020124     C* NE PRECEDENTE
469300020124     C     FLGPRE        IFNE      'A'
469400020124     C     BLTSPE        ANDNE     SAVSPE
469500020124     C     WAGGF         IFEQ      'S'
469600020124     C                   EXSR      WRTAGB
469700020124     C                   CLEAR                   WAGGF
469800020124     C                   ENDIF
469900020124     C                   MOVEL     BLTSPE        SAVSPE
470000020124     C                   ENDIF
470100020124     C*
470200020124    3C     BARCAN        IFNE      ' '
470300020124     C* NON MEMORIZZO ANOMALIA "E" SU DETTAGLIO COLLO PERCHE' LA NON
470400020124     C* INDICA UNA VERA E PROPRIA ANOMALIA MA SOLO UN ERRORE PER LINEA
470500020124     C* MANCANTE SUL FOGLIO AL MOMENTO DELLA SPUNTA DEL COLLO
470600020124     C     BARCAN        ANDNE     'E'
470700020124     C                   MOVEL     BARCAN        BLTCAN
470800020124     C                   MOVEL     'S'           WAGGF             1
470900020124    3C                   END
471000160421     c
471100160421     c* Se zona bolla <> zona spunta --> riaggiorno zona bolla
471200160422     c                   if        bolznc<>'  ' and
471300160429     c                             bolznc<>%editc(barznc:'X')  and
471400160503     c                             barnpg<>8
471500160503     c                   if        tps(Y)= 'L' or tps(y)='C'
471600160421     c                   movel     barznc        wznc
471700160421     C                   MOVEL     'S'           WAGGF             1
471800160421     c                   endif
471900160503     c                   endif
472000020124     C*
472100180129     c                   exsr      Aggpesvol_p
472200020124     C*
472300020124     C* SPUNTA LOCALE IN DISGUIDO --> AGGIORNO LA DATA ENTRATA TRANSITO
472400020124    3C     WSPLOC        IFEQ      'D'
472500020124     C                   MOVEL     BARDFV        BLTDET
472600020124     C                   MOVEL     BARFGS        BLTFLP
472700020124     C                   CLEAR                   BLTDUT
472800020124     C                   MOVEL     'S'           WAGGF             1
472900020124     C*
473000020124   X3C                   ELSE
473100020124     C*
473200020124     C* SE NON C'E' AGGIUNGO DATA E PISTOLA
473300020124     C* SOLO SE NON E' SPUNTA DISGUIDO IN AREA
473400020124    4C     BLTDSE        IFEQ      0
473500020124     C                   MOVEL     BARDFV        BLTDSE
473600020124     C                   MOVEL     BARNPS        BLTNPE
473700020124     C                   MOVEL     'S'           WAGGF             1
473701180206     C* se spedizione chiusa per merce mai affidata memorizzo di scrivere fnagb
473702180206     c* ripristino. Non lo posso scrivere adesso agb per le spedizioni che vengo
473703180206     c* a cavallo di anno: in questo caso con RB rinumerererebbe la spedizione e
473704180206     c* record di agb per la stessa spedizione (scritti dopo) non verrebbero ela
473705180206     c* quanto scritti con la vecchia key spedizione
473706180206     c*
473707180206     C                   if        blpft1='N' and blpft2='N' and blpcca='7'
473708180206     C                   eval      waggRB='S'
473709180206     C                   ENDIF
473800020124   X4C                   ELSE
473900020124     C*
474000020124     C* SE E' QUESTA LA SPUNTA CHE HA AGGIORNATO IL COLLO RIAGGIORNO
474100020124    5C   05BLTNPE        IFEQ      BARNPS
474200020124     C                   MOVEL     BARDFV        BLTDSE
474300020124     C                   MOVEL     'S'           WAGGF             1
474400020124    5C                   ENDIF
474500020124     C*
474600020124     C* RIAGGIRNO COLLO SE PRECEDENTE SPUNTA NON VALIDA E ATTUALE VALID
474700020124    5C  N05BLTNPE        IFNE      BARNPS
474800020124     C     RPS(Y)        ANDEQ     'L'
474900020124     C                   MOVEL     BLTNPE        ALF2
475000020124     C*
475100020124     C* CERCO RAGGRUPPAMENTO PISTOLA
475200020124     C                   EXSR      RICRAG
475300020124     C*
475400020124    6C     RPS(Z)        IFEQ      'M'
475500020124     C     RPS(Z)        OREQ      'A'
475600020124     C                   MOVEL     BARDFV        BLTDSE
475700020124     C                   MOVEL     BARNPS        BLTNPE
475800020124     C                   MOVEL     'S'           WAGGF             1
475900020124    6C                   END
476000020124    5C                   END
476100020124     C*
476200020124    4C                   END
476300020124    3C                   END
476400020124     C*
476500041105     C*  Se FLG3 = A non devo aggiornare fnblt perchè richiamo solo per
476600041105     c*  gli arrivi
476700070208    3C     dls45flg2     IFNE      'A'
476800020124     C                   UPDATE    FNBLT000
476900180124     c* se aggiornato peso o volume aggiorno peso/volume e TAP su FIART
477000180124     c* (mi serve per gestione incompatibili)
477100180124    4c                   if        wagvolart_a='S' or wagpesart_a='S'
477200180126     c                   eval      w_puc = bltpuc
477300180126     c                   eval      w_fpc = bltfpc
477400180126     c                   eval      w_vuc = bltvuc
477500180126     c                   eval      w_fvc = bltfvc
477600180124     c                   exsr      UpdFiart
477700180124    4c                   endif
477800041105     c* Se aggiornato peso e volume e collo partito, aggiorno anche ART
477900041105     c                   if        bltdfv>0 and
478000160421     c                             (wagvolart='S' or wagpesaRT='S')
478100041105     C     KBAR          CHAIN     FNART27L                           3434
478200041105     C                   IF        NOT *IN34
478300041105     c* VOLUME
478400041105     C                   if        wagvolart='S'
478500041105     c* Se lan fedex non aggiorno
478600041105     c                   if        artvuc=0 or ntwbar<>'FED'
478700041105     c                   eval      wagbart='S'
478800041105     c                   eval      artvuc=bltvuc
478900041105     c                   eval      artfvc=bltfvc
479000041105     c                   endif
479100041105     c                   endif
479200041105     C                   if        wagpesart='S'
479300041105     c                   eval      wagbart='S'
479400041105     c                   eval      artpuc=bltpuc
479500041105     c                   eval      artfpc=bltfpc
479600041105     c                   endif
479700160421     c
479800041105     c                   update    fnart000
479900041105     C                   ENDIF
480000041105     C                   ENDIF
480100041105     C
480200020124     C**
480300020124     C* SE RICHIAMATO AGGIORNO SUBITO LA BOLLA
480400110103    5c   02              if        wfpc='S' or wfvc='S'
480500160421     c                             or wznc<>*blanks
480600020124     C                   SETOFF                                       3489
480700020124     C     KBLT          CHAIN     FNBLP01L                           3489
480800041021     c                   if        not *in34 and not *in89
480900041012     c                   if        wfvc='S'
481000041012     C                   MOVEL     99999         BLPNCR
481100041012     c                   endif
481200041012     c                   if        wfpc='S'
481300041012     C                   MOVEL     99999         BLPNCp
481400041012     c                   endif
481500160421     c                   if        wznc<>*blanks
481600160421     c                   movel     wznc          blpznc
481700160421     c                   endif
481800041012     c
481900041012     C                   UPDATE    FNBLP000
482000050223     c
482100050223     c* Aggiorno subito anche la bolla arrivo
482200050224     c                   if        wagbart='S'
482300050223     C     KBLT          CHAIN     FNarb01L                           3489
482400050223     C                   MOVEL     'E'           arbfbs
482500160421     c                   if        wznc<>*blanks
482600160421     c                   movel     wznc          arbznc
482700160421     c                   endif
482800050224     c  n89
482900050224     cann34              update    fnarb000
483000050224     c                   clear                   wagbart
483100050224     c                   endif
483200041012     c
483300041012     c                   clear                   wfpc
483400041012     c                   clear                   wfvc
483500160421     c                   clear                   wznc
483600041012     c                   endif
483700020124    5C                   ENDIF
483800020124     C**
483900020124   X3C                   ELSE
484000020124     C                   UNLOCK    FNBLT27L
484100020124    3C                   ENDIF
484200020124     C*
484300020213     C*CHIUDO EVENTUALI ANOMALIE DI DISGUIDO,SE COLLO NON DEL TFP
484400020124     C     BARNRS        IFGT      *ZEROS
484500020214     C*
484600020213     C                   CLEAR                   DSLV55
484700020213     C                   MOVEL     'P'           D55TPT
484800020213     C                   Z-ADD     BARLNP        D55LIN
484900020214     C                   Z-ADD     DSPB          D55DRF
485000020213     C                   EXSR      FNLV55
485100020131     C**
485200020213     C     D55TFP        IFNE      LNPTFP
485300020124     C                   MOVEL     'S'           SAVMAC
485400020124     C                   Z-ADD     200           COMCAA
485500020124     C                   EXSR      CHIUAN
485600020124     C*
485700020124     C                   Z-ADD     55            COMCAA
485800020124     C                   EXSR      CHIUAN
485900020124     C*
486000020124     C                   Z-ADD     56            COMCAA
486100020124     C                   EXSR      CHIUAN
486200020131     C                   ENDIF
486300020124     C*
486400020124     C                   CLEAR                   SAVMAC
486500020124     C                   END
486600020124     C*
486700020124   X1C                   ELSE
486800020124     C                   UNLOCK    FNBLT27L
486900020124    1C                   ENDIF
487000020124     C**
487100020201     C     ENDBLT        ENDSR
487200180129     C**************************************************************************
487300180129     c* Aggiorno peso e volume in partenza anche se la bolla
487400180129     c*  è solo in blt da spunta arrivi ma per il FIART
487500180129     C**************************************************************************
487600180129     c     Aggpesvol_p   BEGSR
487700180129     c                   clear                   wagvolart         1
487800180129     c                   clear                   wagpesart         1
487900180129     c                   clear                   wagvolart_a       1
488000180129     c                   clear                   wagpesart_a       1
488100180129     c
488200180129    3C     BARVUC        IFGT      0
488300180129     C* AGGIORNO IL VOLUME CML SOLO SE IL VOLUME SUL COLLO:
488400180129     C* - E' = 0
488500180129     C* - E' DELL'ARRIVO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
488600180129     C**- E' DEL TRANSITO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
488700180129    4C     BLTVUC        IFEQ      0
488800180129     C     BLTVUC        ORNE      0
488900180129     C     BLTFVC        ANDNE     'P'
489000180129     C     WWFVC         ANDEQ     'P'
489100180131     c* non li devo toccare qeuti due falg se aggiornamento
489200180131     c*  in caso dimnca rec arrivo e spunta arrivi
489300180131     c                   if        wriagb= ' '
489400180129     C                   MOVEL     'S'           WFVC
489500180131     C                   MOVEL     'S'           WAGGF             1
489600180129     c                   eval      wagvolart='S'
489700180131     c                   endif
489800180131
489900180131     C                   Z-ADD     BARVUC        BLTVUC
490000180129     c                   eval      wagvolart_a='S'
490100180129     C                   MOVEL     WWFVC         BLTFVC
490200180129    4C                   END
490300180129     C*
490400180129    3C                   END
490500180129     C*STESSO GIRO DEL VOLUME ANCHE PER IL PESO
490600180129    3C     BARPUC        IFGT      0
490700180129     C* AGGIORNO IL VOLUME CML SOLO SE IL VOLUME SUL COLLO:
490800180129     C* - E' = 0
490900180129     C* - E' DELL'ARRIVO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
491000180129     C**- E' DEL TRANSITO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
491100180129    4C     BLTPUC        IFEQ      0
491200180129     C     BLTPUC        ORNE      0
491300180129     C     BLTFPC        ANDNE     'P'
491400180129     C     WWFVC         ANDEQ     'P'
491500180131     c
491600180131     c                   if        wriagb= ' '
491700180129     c                   movel     'S'           wfpc
491800180131     C                   MOVEL     'S'           WAGGF             1
491900180129     c                   eval      wagpesart='S'
492000180131     c                   endif
492100180131     c
492200180131     C                   Z-ADD     BARPUC        BLTPUC
492300180129     c                   eval      wagpesart_a='S'
492400180129     C                   MOVEL     WWFVC         BLTFPC
492500180129    4C                   END
492600180129     C*
492700180129    3C                   END
492800180129     c                   ENDSR
492900020131     C**************************************************************************
493000020131     C* AGGIORNO LE BOLLE ARRIVO
493100020131     C**************************************************************************
493200020131     C     AGGART        BEGSR
493300020201     C                   CLEAR                   WEND
493400020131     C*
493500020131     C     WTIBOL        IFEQ      'P'
493600020131     C                   MOVEL     'L'           WTIBOL
493700020131     C                   ELSE
493800020131     C                   MOVEL     'A'           WTIBOL
493900020131     C                   ENDIF
494000180130     C* FLAG DI aggiornamento peso e volume che vale si se esiste la
494100180130     c*  bolla sia in caso di manca rec in arrivo maa bolla presente in
494200180130     c*  partenza
494300180130     C     WWFVC         IFEQ      *BLANKS
494400180130     C                   MOVEL     'A'           WWFVC
494500180130     C                   END
494600020131     C*
494700020131     C* E S I S T E   B O L L A   A R R I V O
494800020131    2C     WESART        IFEQ      'S'
494900020131     C* SE FLG=6 SVINCOLO RECORD
495000070221     C     dls45FLG      IFEQ      '6'
495100020131     C                   UNLOCK    FNART27L
495200020131     C                   ENDIF
495300020131     C**
495400020131     C     BARNPS        IFEQ      89
495500020131     C                   UNLOCK    FNART27L
495600020131     C                   ELSE
495700020214     C     ARTFLP        IFEQ      0
495800020214     C                   Z-ADD     ARTDFV        COMDFV
495900020214     C                   ELSE
496000020214     C                   Z-ADD     ARTDUT        COMDFV
496100020214     C                   ENDIF
496200020131     C                   MOVE      ARTDCM        WATDCM
496300020131     C                   MOVE      ARTDAM        SAVDAM
496400020131     C                   MOVEL     'S'           WBOAR             1
496500020131     C*
496600020131     C* SE FLG=6 CREO ANOMALIA 61
496700160624     c* Anomalia 61 eliminata
496800160624     C***  dls45FLG      IFEQ      '6'
496900160624     C***                EXSR      CRTA60
497000160624     C***                MOVEL     '1'           WEND
497100160624     C***                GOTO      ENDART
497200160624     C***                ENDIF
497300020131     C*
497400020131     C*
497500020131     C* LA PRIMA VOLTA MEMORIZZO LA SPEDIZIONE
497600020131     C                   MOVEL     'A'           ARTTBO
497700020131     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
497800020131     C* NE PRECEDENTE
497900020131     C     FLGPRE        IFNE      'A'
498000020131     C     ARTSPE        ANDNE     SAVSPE
498100020131     C     WAGGF         IFEQ      'S'
498200020131     C                   EXSR      WRTAGB
498300020131     C                   CLEAR                   WAGGF
498400020131     C                   ENDIF
498500020131     C                   MOVEL     ARTSPE        SAVSPE
498600020131     C                   ENDIF
498700160422     c* zona bolla
498800160422     c                   if        bolznc<>'  '   and
498900160429     c                             bolznc<>%editc(barznc:'X') and
499000160503     c                             barnpg<>8
499100160503     c                   if        tps(Y)= 'L' or tps(y)='C'
499200160422     c                   movel     barznc        wznc
499300160422     C                   MOVEL     'S'           WAGGF             1
499400160422     c                   endif
499500160503     c                   endif
499600020131     C*
499700020131     C* CODICE ANOMALIA
499800041012     c                   exsr      AGGCAN
499900020131     C*
500000180124     c                   clear                   wagvolart_a       1
500100180124     c                   clear                   wagpesart_a       1
500200020131     C* VOLUME DA MACCHINA SMISTACOLLI
500300041012     C                   EXSR      AGGVOL
500400030604     C*
500500020131     C* PESO DA MACCHINA SMISTACOLLI
500600041012     C                   EXSR      AGGpes
500700110103     c*
500800110103     c*
500900091209     c* Se chi spara è la linea di arrivo del collo o il suo gestore
501000091209     c* --> aggiorno la data più bassa di arrivo 1° collo
501100091211     C* CARICO LA L6 DELLA FILIALE GESTIONE
501200091211     C     BPES          IFNE      SAVPES
501300091211     C                   MOVEL     BPES          SAVPES
501400091211     C                   EXSR      CARL6
501500091211     C                   ENDIF
501600110103      *
501700091211     C     BARLNA        LOOKUP    L6                                     88
501800150615     c*
501900150817     c* non aggiorno più data e ora arrivo alla filiale finale di arrivo
502000150817     c*  er l'estero perchè creeremo apposito campo per
502100150817     c*  indicare la data di arrivo all'hub/terminal di arrivo
502200150622     c***                if        not *in88 and  boltfa=BPES  and
502300150622     c***                          (ntwbar='EEX' or ntwbar='DPD' or
502400150622     c***                          ntwbar='FED')
502500150622     c***                seton                                        88
502600150622     c***                endif
502700110103      *
502800150817      * tolto test su data spedizione dal 2009/12 in poi
502900150817    3c                   if        *in88 or artlna=bpes
503000091211     c     kar52         chain     fiar501l
503100091209    4c                   if        %found(fiar501l)
503200091209     c                   movel     ar5uni        dar5gen
503300091209     c                   else
503400091209     c                   clear                   dar5gen
503500091209    4c                   endif
503600110103     c*
503700091209     c                   clear                   warrdt
503800091209     c                   clear                   warrhm
503900091209    4c                   if        §ar5arrdt>*zeros
504000091209     c                   movel     §ar5arrdt     warrdt            8 0
504100091209    5c                   if        §ar5arrhm>*zeros
504200091209     c                   movel     §ar5arrhm     warrhm            4 0
504300091209    5c                   endif
504400091209    4c                   endif
504500110103     c*
504600101129     c* Uso data e ora immissione se non superiore all'ora caricamento
504700150817     c                   if        (bardfs<brvdcs) or
504800150817     c                             (bardfs=brvdcs and barhms<brvhcs)
504900101130     c                   movel     bardfs        datReale
505000101130     c                   movel     barhms        oraReale
505100101129     c                   else
505200101129     c                   movel     brvdcs        datReale
505300101129     c                   movel     brvhcs        oraReale
505400101129     c                   endif
505500110103     c*
505600091211     c                   if         warrdt=0  or
505700101129    4c                             (warrdt>0 and warrdt>datReale) or
505800101129     c                             (warrdt=datReale and warrhm>oraReale)
505900091209     c                   exsr      AggioFIAR5
506000110103     c*
506100091209   x4c                   else
506200091209     c                   unlock    fiar501l
506300091209    4c                   endif
506400091209    3c                   endif
506500180124      **
506600180115      * Se chi spunta è il terminal di arrivo --> verifico se spunta incompatibi
506700091209      **
506800180125     c***                if        bpes=boltfa and (brvspg='J'  or
506900180125     c***                          (savcan='S' and
507000180125     c***                          (AllINC='S' or %lookup(boltfa:fic)>0)
507100180125     c***                           ))
507200180125     c***                exsr      sr_addinc
507300180125     c***                ENDIF
507400180115      *
507500020131    3C     ARTDAM        IFEQ      0
507600020131     C                   MOVEL     BARDFV        ARTDAM
507700020131     C                   MOVEL     'S'           WAGGF             1
507800020131     C                   MOVEL     BARNPS        ARTNAP
507900050509     c* Imposto flag ARTFL2= 'P' se catagoria di spunta è tipica processo
508000050509     c*  operativo partenza
508100050509     c                   if        barnpg=5 or (barnpg=1 and wdefl=' ') or
508200050509     c                             (barnpg=3  and barspg='P')
508300050509     c                   eval      artfl2='P'
508400050509     c                   else
508500050509     c                   eval      artfl2=' '
508600050509     c                   endif
508700020131     C*
508800020131     C                   MOVEL     ' '           ARTDAB
508900020131     C                   SETON                                        03
509000020131     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
509100160624     c* ANOMALIE 60 E 61 ELIMINATE
509200160624     C***                EXSR      CRTA60
509300020131     C*
509400020131   X3C                   ELSE
509500020131    4C   05ARTNAP        IFEQ      BARNPS
509600020131     C                   MOVEL     BARDFV        ARTDAM
509700020131     C                   MOVEL     'S'           WAGGF             1
509800020131    4C                   ENDIF
509900020131     C*
510000020131     C* 'D' DI DISABILITAZIONE COLLO :
510100020131     C*   SE RISPUNTATO LANCIO PGM DI ABILITAZIONE X RIABILITARE BOLLA
510200020131    4C  N05ARTDAB        IFEQ      'S'
510300020131     C                   SETON                                        03
510400020131     C                   MOVEL     ' '           ARTDAB
510500040324     C                   MOVEL     'S'           WAGGF             1
510600020131    4C                   ENDIF
510700020131     C*
510800020131    4C  N05ARTNAP        IFNE      BARNPS
510900020131     C     RPS(Y)        ANDEQ     'L'
511000020131     C                   MOVEL     ARTNAP        ALF2              2
511100020131     C*
511200020131     C* CERCO RAGGRUPPAMENTO PISTOLA
511300020131     C                   EXSR      RICRAG
511400020131     C*
511500020131    5C     RPS(Z)        IFEQ      'M'
511600020131     C     RPS(Z)        OREQ      'A'
511700020131     C                   MOVEL     'S'           WAGGF             1
511800020131     C                   MOVEL     BARDFV        ARTDAM
511900020213     C                   MOVEL     BARNPS        ARTNAP
512000050509     c* Imposto flag ARTFL2= 'P' se catagoria di spunta è tipica processo
512100050509     c*  operativo partenza
512200050509     c                   if        barnpg=5 or (barnpg=1 and wdefl=' ') or
512300050509     c                             (barnpg=3  and barspg='P')
512400050509     c                   eval      artfl2='P'
512500050509     c                   else
512600050509     c                   eval      artfl2=' '
512700050509     c                   endif
512800020131     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
512900160624     c* anomalie 60 e 61 eliminate
513000160624     C***                EXSR      CRTA60
513100020131     C*
513200020131    5C                   ENDIF
513300020131    4C                   ENDIF
513400020131     C*
513500020131    3C                   END
513600020131     C*
513700020131     C                   Z-ADD     ARTDCM        WDCM
513800020131     C**
513900070208    3C     dls45flg2     IFNE      'A'
514000020131     C                   UPDATE    FNART000
514100180124    4c                   if        wagvolart_a='S' or wagpesart_a='S'
514200180126     c                   eval      w_puc = artpuc
514300180126     c                   eval      w_fpc = artfpc
514400180126     c                   eval      w_vuc = artvuc
514500180126     c                   eval      w_fvc = artfvc
514600180124     c                   exsr      updfiart
514700180124    4c                   endif
514800020131     C                   ELSE
514900020131     C                   UNLOCK    FNART27L
515000020131    3C                   ENDIF
515100180125      * Se chi spunta è il terminal di arrivo --> verifico se spunta incompatibi
515200180125      **
515300180125     c                   if        bpes=boltfa and barnpg=2 and
515400180126     c                             (barspg='J'  or
515500180125     c                             (savcan='S' and
515600180125     c                             (AllINC='S' or
515700180125     c                              %lookup(%editc(boltfa:'X'):fic)>0
515800180125     c                              )))
515801180216     c                   eval      wdarr = artdam
515900180125     c                   exsr      sr_addinc
516000180125     c                   ENDIF
516100020131     C*
516200050223     C     KARB          CHAIN(N)  FNARB01L                           32
516300020131     C*
516400020131    3C     *IN32         IFEQ      *OFF
516500020131     C*
516600070208    4C     dls45flg2     IFNE      'A'
516700020131     C* SOLO SE PGM RICHIAMATO:
516800020131     C* SE VARIATA DATA DI ARRIVO SULLA BOLLA RIELABORO LA STATISTICA
516900020131    5C     *IN02         IFEQ      *ON
517000020131     C     ARTDAM        ANDNE     SAVDAM
517100020131    6C     ARTDAM        IFNE      WARDAM
517200020131     C     SAVDAM        ORNE      WSVDAM
517300020131     C                   EXSR      WRTSTA
517400020131     C                   MOVE      ARTDAM        WARDAM
517500020131     C                   MOVE      SAVDAM        WSVDAM
517600020131    6C                   END
517700020131    5C                   END
517800020131     C*
517900020131     C                   MOVEL     ARBNDC        WABNDC
518000040430     C                   MOVEL     ARBcca        WABcca
518100020131     C   03              MOVEL     'S'           WFBS
518200020131     C*
518300020131    4C                   ENDIF
518400020131     C**
518500020131     C** VEDO SE BOLLA GIACENTE
518600020131    4C     ARBFBC        IFEQ      'G'
518700050331     C     KGCA          CHAIN     Tigcp21L                           34
518800161007     c  n34              z-add     gcpmgc        gcpdgc
518900161007     c  n34              movel     gcpagc        gcpdgc
519000161007    5C  N34GCPFAS        IFLT      35
519100161007    5C     GCPFAS        oreq      36
519200161007     c* Verifico tramite driver se il collo dove essere in IMG
519300161007     c                   clear                   fnlgi1ds
519400161007     c                   eval      ilgi1aas=arbaas
519500161007     c                   eval      ilgi1lnp=arblnp
519600161007     c                   eval      ilgi1nrs=arbnrs
519700161007     c                   eval      ilgi1nsp=arbnsp
519800161007     c                   select
519900161007     c                   when      arbncp=arbncl
520000161007     c                   eval      ilgi1pkf=arbpkc
520100161007     c                   when      arbpkc>arbpkf
520200161007     c                   eval      ilgi1pkf=arbpkc
520300161007     c                   other
520400161007     c                   eval      ilgi1pkf=arbpkf
520500161007     c                   endsl
520600161007     c                   movel     fnlgi1ds      kpjbu
520700161007     c                   call      'FNLGI1R'
520800161007     c                   parm                    kpjba
520900161007     c                   parm                    tigcp00ds
521000161007     c                   movel     kpjbu         fnlgi1ds
521100161021     c* In questo caso la sped potrebbe essere sia in IMA che in IMG
521200170125     c*  quindi non devo dare alcun fuori posto con la nuova procedura
521300170125     c*  anche se ha Y (ovvero portato nel giorno in IMG:
521400170125     c*  potrebbe mancare un collo della sped spuntato succesivamente)
521500170125   5ac****               if        olgi1AMG='S'
521600170125     c
521700170125   5ac                   if        olgi1AMG='S' or olgi1AMG='Y'
521800161021     C                   MOVEL     'D'           WGIAC
521900161021  x5ac                   else
522000170125   5bc****               if        olgi1AMG='Y'  or olgi1inmg='S'
522100170125   5bc                   if        olgi1inmg='S'
522200161201     C                   MOVEL     'S'           WGIAC
522300161201   5bc                   endif
522400161201   5ac                   endif
522500161201     c
522600161201   5ac                   if        wgiac<>' '
522700020131     C* NON LA CONSIDERO GIACENTE SE IN QUESTO MOMENTO C'E' UNA BOLLA
522800020131     C*  FIGLIA IN CONSEGNA O TUTTE LE FIGLIE SONO BLOCCATE
522900020131     C                   MOVEL     'S'           WFIBLO            1
523000020131     C                   CLEAR                   WESFI
523100020131     C     KARB          SETLL     FNLBL02L
523200020131     C     KARB          READE     FNLBL02L                               29
523300020131     C**
523400020131    6C     *IN29         DOWEQ     *OFF
523500020131     C*
523600050223     C     KLBL          CHAIN(N)  FNARB01L                           28
523700020131    7C     *IN28         IFEQ      *OFF
523800020131     C                   MOVEL     'S'           WESFI             1
523900020131     C* FIGLIA IN CONSEGNA
524000020131     C* NON LA CONSDIERO SE E' CONSEGNATA
524100020131   7AC     ARBDCM        IFEQ      0
524200020131    8C     ARBNDC        IFNE      0
524300020131     C                   SETON                                        29
524400020131     C                   CLEAR                   WGIAC
524500020131   X8C                   ELSE
524600020131     C*
524700020131     C* SE NON E' NEMMENO BLOCCATA MEMORIZZO
524800020131    9C     ARBFBC        IFNE      'B'
524900020131     C                   MOVEL     'N'           WFIBLO
525000020131    9C                   ENDIF
525100020131    8C                   ENDIF
525200020131   7AC                   ENDIF
525300170125    7C                   ENDIF
525400020131     C*
525500020131     C  N29KARB          READE     FNLBL02L                               29
525600020131    6C                   ENDDO
525700020131     C*
525800020131     C* SE TUTTE LE FIGLIE SONO BLOCCATE --> ESCLUDO
525900161201    6C     WGIAC         IFNE      ' '
526000020131     C     WESFI         ANDEQ     'S'
526100020131     C     WFIBLO        ANDEQ     'S'
526200020131     C                   CLEAR                   WGIAC
526300020131    6C                   ENDIF
526400020131     C*
526500161021   5aC                   ENDIF
526600161007    5C                   ENDIF
526700020131    4C                   ENDIF
526800020131    3C                   ENDIF
526900020131     C*
527000020131     C** COLLO EXPORT: VEDO SE CREARE L'ANOMALIA COLLO INVIATO PARTNER
527100020131     C*  MA RISPARATO IN IMA O IN PARTENZA/ARRIVO
527200020131     C*  NON CREO ANOMALIA SE E' DATA =99999999 PERCHE' SI TRATTA DI
527300020131     C*  DIROTTAMENTO E QUINDI VALGONO I COLLI DELLA FIGLIA E NON
527400020131     C*  DELLA MAMMA
527500020212    3C     NTWBAR        IFEQ      'EEX'
527600060322    3C     NTWBAR        oreq      'DPD'
527700060322    3C     NTWBAR        oreq      'FED'
527800060322    4C     BARDFV        ifGT      ARTDTR
527900020131     C     ARTDTR        ANDGT     0
528000020131     C     ARTDTR        ANDNE     *HIVAL
528100020131     C** CREO ANOMALIA 600 O 620
528200020131     C                   MOVEL     'S'           W60X              1
528300060322    4C                   ENDIF
528400060322    3C                   ENDIF
528500020131    2C                   ENDIF
528600020131     C*
528700020131   X2C                   ELSE
528800020131     C*
528900070221   2AC     dls45FLG      IFNE      '6'
529000180129     c* come prima cosa memorizzo il peso e volume per FIART, se
529100180130     c* si tratta di un manca record o comunque bolla non ancora in FNARB
529200180130     c*  se fosse già consegnato sarebbe sicuramente collo riciclato
529300180130     c*  e mi disinteresso (potrei perdere le chiusue per merce mai affidata
529400180130     c*  i cui colli vengono spuntati prima in arrivo )
529500180130    3c                   if        wesblt='S' and bltdcm=0
529600180129     c
529700180131     c                   eval      wriagb='N'
529800180129     c                   exsr      Aggpesvol_p
529900180131     c                   clear                   wriagb            1
530000180129     c
530100180129    4c                   if        wagvolart_a='S' or wagpesart_a='S'
530200180129     c                   eval      w_puc = bltpuc
530300180129     c                   eval      w_fpc = bltfpc
530400180129     c                   eval      w_vuc = bltvuc
530500180129     c                   eval      w_fvc = bltfvc
530600180129     c                   exsr      UpdFiart
530700180129     c                   clear                   WAGvolart
530800180129     c                   clear                   WAGpesart
530900180129    4c                   endif
531000180129     c
531100180129     c                   if        bpes=boltfa and barnpg=2 and
531200180129     c                             (barspg='J'  or
531300180129     c                             (savcan='S' and
531400180129     c                             (AllINC='S' or
531500180129     c                              %lookup(%editc(boltfa:'X'):fic)>0
531600180129     c                              )))
531601180216     c                   clear                   wdarr
531700180129     c                   exsr      sr_addinc
531800180129    4c                   ENDIF
531900180129    3c                   ENDIF
532000180129     c
532100020131     C** CREO LE ANOMALIE DI MANCA BOLLA SOLO SE NON SONO PASSATI I
532200020131     C*  GG DIPULIZIA BOLLE ARRIVI (PER EVITARE MANCA RECORD ERRATI)
532300020131     C                   EXSR      CTRDAT
532400020131     C**
532500020131   2BC     WCRE10        IFEQ      'S'
532600130301     c     wlocalxco     andne     'S'
532700020131     C**
532800020131     C* ANOMALIA: "MANCA BOLLA" --> ANCHE PER SPEDIZIONE LOCALE SE
532900020131     C*  E' SPARATA NELLE CATEGORIE ARRIVI
533000020131     C*  IL FOGLIO DI DEFLUENZA E' COME UNA CATEGORIA ARRIVI
533100020131    3C  N05BARNPG        IFNE      5
533200020131     C     BARSPG        ANDNE     'P'
533300020131     C     BARSPG        ANDNE     'I'
533400020131     C     WTIBOL        ANDEQ     'L'
533500020131     C     WTIBOL        OREQ      'A'
533600020131     C* LA CATEGORIA 1 LA CONSIDERO SOLTANTO SE E' DEFLUENZA
533700020131     C*
533800020131    4C     BARNPG        IFNE      1
533900020131     C     WTIBOL        OREQ      'A'
534000020131     C     WDEFL         OREQ      '1'
534100020131     C                   Z-ADD     10            COMCAA
534200020131     C                   EXSR      CRTANM
534300020131     C* Se l'anomalia 10 esiste già a livello max. creo anom. 410 per
534400020131     C* segnalare il persistere del manca record
534500020131     C     WANM10        IFEQ      '1'
534600020131     C                   Z-ADD     410           COMCAA
534700020131     C                   EXSR      CRTANM
534800020131     C                   END
534900020131    4C                   ENDIF
535000020131    3C                   ENDIF
535100020131     C*
535200020131     C* SE LNA ESTERA VERIFICO SE C'E' EVENTO 2 PER CREARE ANOM.600/620
535300020212    3C     NTWBAR        IFEQ      'EEX'
535400060322    3C     NTWBAR        oreq      'DPD'
535500060322    3C     NTWBAR        oreq      'FED'
535600020131     C*
535700020131     C* SE LNA ESTERA APRO FILE FNEVS02L
535800020131    4C     WAPER         IFEQ      ' '
535900020131     C                   OPEN      FNEVS02L
536000020131     C                   MOVEL     '1'           WAPER             1
536100020131    4C                   ENDIF
536200020131     C*
536300020131     C                   MOVEL     'E2 '         KCEV
536400020131     C     KEVS          SETGT     FNEVS02L
536500020131     C     KEVS          READPE    FNEVS02L                               34
536600020131    4C     *IN34         IFEQ      *OFF
536700020131     C     EVSDEV        ANDLT     BARDFV
536800020131     C* CREO ANOMALIA 600 E 620
536900020131     C                   MOVEL     'S'           W60X
537000020131    4C                   ENDIF
537100020131    3C                   ENDIF
537200020131   2BC                   ENDIF
537300180129     c
537400180129   2AC                   ENDIF
537500020131    2C                   ENDIF
537600020131     C**
537700020201     C     ENDART        ENDSR
537800020213     C**************************************************************************
537900020213     C* AGGIORNO LE BOLLE TRANSITO
538000020213     C**************************************************************************
538100020213     C     AGGBTT        BEGSR
538200020213     C                   MOVEL     'T'           WTIBOL
538300020214     C* MEMORIZZO CHE C'E' BOLLA TRANSITO, MI SERVE PER CRTA50
538400020214     C                   MOVE      BTTDFV        COMDFV
538500020213     C* LA PRIMA VOLTA MEMORIZZO LA SPEDIZIONE
538600020213     C                   MOVEL     'T'           BTTTBO
538700020213     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
538800020213     C* NE PRECEDENTE
538900020213     C     BTTSPE        IFNE      SAVSPE
539000020213     C     WAGGF         IFEQ      'S'
539100020213     C                   EXSR      WRTAGB
539200020213     C                   CLEAR                   WAGGF
539300020213     C                   ENDIF
539400020213     C                   MOVEL     BTTSPE        SAVSPE
539500020213     C                   ENDIF
539600020213     C*
539700020213    5C     BARCAN        IFNE      ' '
539800020213     C     BARCAN        ANDNE     'E'
539900020213     C                   MOVEL     BARCAN        BTTCAN
540000020213     C                   MOVEL     'S'           WAGGF             1
540100020213    5C                   END
540200020213     C*
540300020213    5C     BARVUC        IFGT      0
540400020213     C*
540500020213    6C     BTTVUC        IFEQ      0
540600041013     c                   movel     'S'           WFVC
540700020213     C                   Z-ADD     BARVUC        BTTVUC
540800020213     C                   MOVEL     'S'           WAGGF             1
540900020213     C                   MOVEL     'T'           BTTFVC
541000020213    6C                   END
541100020213     C*
541200020213    5C                   END
541300020213     C*PESO DA CML
541400020213    5C     BARPUC        IFGT      0
541500020213     C*
541600020213    6C     BTTPUC        IFEQ      0
541700041013     c                   movel     'S'           WFPC
541800020213     C                   Z-ADD     BARPUC        BTTPUC
541900020213     C                   MOVEL     'S'           WAGGF             1
542000020213     C                   MOVEL     'T'           BTTFPC
542100020213    6C                   END
542200020213     C*
542300020213    5C                   END
542400020213     C*
542500020213    5C     BTTDET        IFEQ      0
542600020213     C                   MOVEL     'S'           WAGGF             1
542700020213     C                   MOVEL     BARDFV        BTTDET
542800020213     C                   MOVEL     BARNPS        BTTPET
542900020213     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
543000020213     C*  SOLO SE NON E' PISTOLA FASULLA
543100160624     C**                 MOVEL     'TT'          WCCH
543200160624     C**   OPS(Y)        IFLT      040
543300160624     C**                 EXSR      CRTA60
543400160624     C**                 CLEAR                   WCCH
543500160624     C**                 ENDIF
543600020213     C*
543700020213   X5C                   ELSE
543800020213     C*
543900020213    6C   05BTTPET        IFEQ      BARNPS
544000020213     C                   MOVEL     'S'           WAGGF             1
544100020213     C                   MOVEL     BARDFV        BTTDET
544200020213    6C                   END
544300020213     C*
544400020213    6C  N05BTTPET        IFNE      BARNPS
544500020213     C     RPS(Y)        ANDEQ     'L'
544600020213     C                   MOVEL     BTTPET        ALF2
544700020213     C*
544800020213     C* CERCO RAGGRUPPAMENTO PISTOLA
544900020213     C                   EXSR      RICRAG
545000020213     C*
545100020213    7C     RPS(Z)        IFEQ      'M'
545200020213     C     RPS(Z)        OREQ      'A'
545300020213     C                   MOVEL     BARDFV        BTTDET
545400020213     C                   MOVEL     'S'           WAGGF             1
545500020213     C                   MOVEL     BARNPS        BTTPET
545600160624     c
545700020213     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
545800160624     c* Anomalia 61 eliminara
545900160624     C**                 MOVEL     'TT'          WCCH
546000160624     C**                 EXSR      CRTA60
546100160624     C**                 CLEAR                   WCCH
546200020213    7C                   END
546300020213    6C                   END
546400020213     C*
546500020213    5C                   END
546600020213     C*
546700020213     C                   UPDATE    FNBTT000
546800020213     C*
546900020213     C* SE INVENTARIO O IN CONSEGNA ANOMALIA: FUORI POSTO DEL TRANSITO
547000020213    5C  N05BARNPG        IFEQ      3
547100020213     C     BARSPG        ANDNE     'P'
547200020213     C     BARNPG        OREQ      4
547300020213     C     BARNPG        OREQ      8
547400020213     C                   Z-ADD     125           COMCAA
547500020213     C                   EXSR      CRTANM
547600020213    5C                   END
547700040430     c* Se il collo è in transito, posso chiudere i fuori posto in partenza
547800040430     C                   Z-ADD     120           COMCAA
547900040430     C                   EXSR      chiuan
548000110103     c*
548100040422     c*Chiudo eventuale anomalia di disguido presente sul collo, se
548200040422     c*  generate da un 2 livello dell'area di partenza
548300040503     C* SOLO SE SPUNTA DI PISTOLA REALE
548400040503     c                   if        fsl(y)<>'F'
548500040428     c                   eval      comcaa=200
548600040422     C*
548700040503     C     Kanm          chain(N)  FNANM000
548800040428     c                   if        %found(fnanm02l)
548900040428     C                   CLEAR                   DSLV55
549000040428     C                   MOVEL     'P'           D55TPT
549100040428     C                   Z-ADD     anmfle        D55LIN
549200040428     C                   Z-ADD     bardfv        D55DRF
549300040428     C                   EXSR      FNLV55
549400040428     c                   if        bpes=d55tfp
549500040428     c                   exsr      chiuan
549600040428     c                   endif
549700040428     c                   endif
549800040503     c                   endif
549900041012     c*
550000041012     c* Aggiorno anche la bolla arrivo con i dati del transito
550100041012     c                   EXSR      AGGARRIVO
550200110103     c*
550300020213     C                   ENDSR
550400020201     C**************************************************************************
550500020201     C* CONTROLLO COSA POSSO AGGIORNARE IN BASE A CHI SONO
550600020201     C**************************************************************************
550700020201     C     CTRAGG        BEGSR
550800020204     C                   CLEAR                   WAGPAR
550900020204     C                   CLEAR                   WAGARR
551000150622     C                   CLEAR                   Wbsptp
551100150622     C                   CLEAR                   Wbspspu
551200020213     C**
551300020213     C** CERCO TERMINAL DELLA LINEA DI ARRIVO
551400020213     C                   EXSR      TERARR
551500050603     c                   eval      Usalnatfa=lnatfa
551600020204     C*
551700070208     c*         Non considero se c'e'bolla transito
551800070208     c     wesbtt        ifne      'S'
551900150622     C* SE NON C'È BOLLA TRANSITO VERIFICO SE COLLI CHE PARTONO DA
552000150817     C*  più filiali se ho trovato la bolla in partenza non partita
552100150817     c*  e con data di entrata = 0
552200170224     c                   if        wesblt='S' and blpdse=0
552300170224     c                   if        Blpdt1=0  or blpft1='N' and blpft2='N'
552400150818     c* se si tratta di una spunta tipica partenza
552500150818     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
552600150818     c                             or barnpg=1
552700150622     c                   exsr      sr_fibsp
552800150622     c                   if        wbsptp<>' '      and
552900150622     c                             %lookup(bpes:sbsp)>0
553000150622     c                   eval      wbspspu='S'
553100150818     C                   MOVEL     'P'           WTIBOL            1
553200150622     c                   endif
553300170224     c                   endif
553400170224
553500150622     c                   endif
553600150818     c                   endif
553700020204     C*
553800020201     C*  1) P.O. EFFETTUA SPUNTA = LNP SPUNTA (DA BOLLA)
553900020204    2C     BPES          IFEQ      LNPB
554000020201     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.PARTENZA DELLA LNP SPUNTA
554100020201     C*     IN BASE ALLA DATA SPEDIZIONE SE TROVATA LA BOLLA
554200020204     C     BPES          OREQ      LNPTFP
554300020204     C                   MOVEL     'S'           WAGPAR            1
554400020204   X2C                   ELSE
554500020204     C*  3) TER.PARTENZA P.O. EFFETTUA SPUNTA (IN BASE ALLA DATA SPED
554600020204     C*     SE TROVATA LA BOLLA) = TER.PARTENZA LNP SPUNTA SEMPRE IN
554700020204     C*     BASE ALLA DATA SPEDIZIONE SE TROVATA BOLLA
554800021125     C                   MOVEL     'P'           WTITER            1
554900020201     C                   EXSR      TERPES
555000020215    3C     D55TFP        IFEQ      LNPTFP
555100020204     C                   MOVEL     'S'           WAGPAR            1
555200020204    3C                   ENDIF
555300020204    2C                   ENDIF
555400020201    1C                   ENDIF
555500020204     C**
555600020204     C** NON AGGIORNO BOLLE ARRIVO SE SPUNTA DELL'AREA PARTENZA,
555700020204     C*   SPUNTA TIPICA PARTENZA E P.O. SPUNTA = LNA SPUNTA O TFA SPU
555800021125     c* controllo solo se spunta tipica partenza: l'arrivo non mi
555900021125     c*  interessa. non so perchè lo avevo preso in considerazione!!
556000150817     c
556100150817     c* Non aggiorno nemmeno se bolla da "trasferire"
556200020204    1C     WAGPAR        IFEQ      'S'
556300150817     c     wbspspu       oreq      'S'
556400150817     c
556500021125     C**   BPES          ifeq      lnpb
556600021125    2C**   BPES          OREQ      BARLNA
556700021125     C**   BPES          OREQ      LNATFA
556800021125     C**   pestfa        OREQ      LNATFA
556900020204     C* PARTENZA NON DEFLUENZA
557000020204    3C     BARNPG        IFEQ      1
557100020204     C     WDEFL         ANDEQ     '0'
557200020204     C* ENTRATA
557300020204     C     BARNPG        OREQ      5
557400020204     C* I.M.P.
557500020204     C     BARNPG        OREQ      3
557600020204     C     BARSPG        ANDEQ     'P'
557700020204     C* I.M.I.
557800020204     C     BARNPG        OREQ      3
557900020218     C     BARSPG        ANDEQ     'I'
558000020204     C                   GOTO      NOAGAR
558100020204    3C                   ENDIF
558200021125    2C**                 ENDIF
558300020204    1C                   ENDIF
558400020204     C**
558500021022     c     wesbtt        ifne      'S'
558600020204     C* AGGIORNO BOLLE ARRIVO SE :
558700020204     C*  1) P.O. EFFETTUA SPUNTA = LNA SPUNTA
558800020205    1C                   SELECT
558900020204     C     BPES          WHENEQ    BARLNA
559000020204     C                   MOVEL     'S'           WAGARR            1
559100020205    2C     WAGPAR        IFEQ      'S'
559200020204     C                   MOVEL     'A'           WWFVC
559300020204     C                   MOVEL     'A'           WSPLOC            1
559400020205    2C                   ENDIF
559500020204     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.ARRIVO   DELLA LNA SPUNTA
559600020204     C*     IN BASE ALLA DATA SPUNTA
559700020204     C     BPES          WHENEQ    LNATFA
559800050603     C     BPES          oreq      bolTFA
559900020204     C                   MOVEL     'S'           WAGARR            1
560000020205    2C     WAGPAR        IFEQ      'S'
560100020204     C                   MOVEL     'A'           WWFVC
560200020204     C                   MOVEL     'T'           WSPLOC            1
560300020205    2C                   ENDIF
560400050603     c* imposto il terminal di arrivo =BPES
560500050603     c                   eval      Usalnatfa=BPES
560600020204   X1C                   OTHER
560700020204     C*  3) TER.ARRIVO P.O. EFFETTUA SPUNTA
560800020204     C*     = TER.ARRIVO LNA SPUNTA IN BASE ALLA DATA SPUNTA
560900020204    2C     PESTFA        IFEQ      LNATFA
561000050603    2C     PESTFA        oreq      bolTFA
561100020204     C                   MOVEL     'S'           WAGARR            1
561200020204     C                   MOVEL     'D'           WSPLOC
561300020204     C                   MOVEL     'A'           WWFVC
561400050603     c* imposto il terminal di arrivo =ter arrivo di BPES
561500050603     c                   eval      Usalnatfa=pestfa
561600110117   x2c                   else
561700110117     c
561800110117     c*  4) Se nessuna è vera ma chi spara è la capostipite della lna-£6
561900110117     c*     anche se è una incongruenza la considero valida ai fini dell'arrivo
562000110117     C* CARICO LA L6 DELLA FILIALE GESTIONE
562100110117    3C     BPES          IFNE      SAVPES
562200110117     C                   MOVEL     BPES          SAVPES
562300110117     C                   EXSR      CARL6
562400110117    3C                   ENDIF
562500110117    c
562600110117     C     BARLNA        LOOKUP    L6                                     34
562700110117    3c                   if        *in34
562800110117     C                   MOVEL     'S'           WAGARR            1
562900110117    4C     WAGPAR        IFEQ      'S'
563000110117     C                   MOVEL     'A'           WWFVC
563100110117     C                   MOVEL     'A'           WSPLOC            1
563200110117    4C                   ENDIF
563300110117    3C                   ENDIF
563400110117     c
563500020205    2C                   ENDIF
563600020205    1C                   ENDSL
563700021022     c*
563800021022    2C                   endif
563900020204     C**
564000070208     C     NOAGAR        ENDSR
564100150622     C**************************************************************************
564200150622     C*  vedo se lnp o ksc confermabile da più linee
564300150622     C**************************************************************************
564400150622     C     sr_fibsp      BEGSR
564500150622     c                   eval      WBspTp=*blanks
564600150622    2c                   if        blpccm>0
564700150622     c                   eval      w_clibo=blpccm
564800150622     c                   else
564900150622     c                   eval      w_clibo=blpksc
565000150622    2c                   endif
565100150622     c     w_clibo       chain     fibsp02l
565200150622     c                   if        %found(fibsp02l) and bspfpa='S'
565300150622     c                   eval      wBspTp='K'
565400150622     c****               if        not %found(fibsp02l) or bspfpa<>'S'
565500150622     c                   else
565600150817     c     LNPB          chain     fibsp01l
565700150622     c                   if        %found(fibsp01l) and bspfpa='S'
565800150622     c                   eval      wBspTp='L'
565900150622     c                   endif
566000150622     c                   endif
566100150622     c                   endsr
566200020201     C**************************************************************************
566300020201     C* TERMINAL DI PARTENZA E ARRIVO DEL P.O. EFFETTUA SPUNTA
566400020201     C**************************************************************************
566500020205     C     TERPES        BEGSR
566600020201     C* IMPOSTO TERMINAL PARTENZA E ARRIVO
566700020201     C                   CLEAR                   DSLV55
566800020205     C                   MOVEL     WTITER        D55TPT
566900020205     C                   MOVEL     LNPB          D55LNP
567000020205     C                   MOVEL     WDTRIF        D55DRF
567100020205     C                   MOVE      BPES          D55LIN
567200020205     C                   EXSR      FNLV55
567300020205     C                   ENDSR
567400020205     C**************************************************************************
567500020205     C* CALL A PGM FNLV55R
567600020205     C**************************************************************************
567700020205     C     FNLV55        BEGSR
567800020201     C                   CALL      'FNLV55R'
567900020201     C                   PARM                    DSLV55
568000020201     C**
568100020205     C     D55ERR        IFNE      ' '
568200020205     C                   MOVE      D55LIN        D55TFA
568300020205     C                   MOVE      D55LIN        D55TFP
568400020201     C                   END
568500020201     C                   ENDSR
568600041012     C**************************************************************************
568700041012     C* AGGIORNO CODICE ANOMALIA
568800041012     C**************************************************************************
568900041012     c     AGGCAN        BEGSR
569000041012    3C     BARCAN        IFNE      ' '
569100041012     C     BARCAN        ANDNE     'E'
569200070508     c     barcan        andne     artcan
569300041012     C**
569400070508     c* Aggiorno la testata e quindi scrivo AGB solo se artcan
569500070508     c*  è vuoto prima di questo aggiornamento
569600070508     c                   if        artcan=' '
569700041012     C                   MOVEL     'S'           WAGGF             1
569800070508     c                   endif
569900070508     C                   MOVEL     BARCAN        ARTCAN
570000041012     c                   endif
570100041012     c                   endsr
570200041012     C**************************************************************************
570300041012     C* AGGIORNO volume cml
570400041012     C**************************************************************************
570500041105     C     aGGVOL        BEGSR
570600041012    3C                   SELECT
570700041012    3C                   when      BARVUC = *zeros
570800041012      * NON SOSTITUISCO IL VOLUME ARRIVO SE LNA FEDEX
570900041012    3C                   when      ARTFVC = 'A' and ARTVUC > *zeros
571000041012    3C                                          and NTWBAR = 'FED'
571100041012      * IL VOLUME ARRIVO O TRANSITO LI SOSTITUISCO SE QUELLO DELLA
571200041012      * SPUNTA E' RILEVATO DALLA PARTENZA
571300041012    3C                   when       ARTVUC = *zeros  or
571400041012    3C                             (ARTVUC > *zeros and ARTFVC <> 'P'
571500041012     C                                              and WWFVC  =  'P')   or
571600041012    3C                             (ARTVUC > *zeros and ARTFVC =  'A'
571700041012     C                                              and WWFVC  =  'T')
571800041012     c                   movel     'S'           wfvc
571900041012     C                   MOVEL     'S'           WAGGF             1
572000041012     C                   Z-ADD     BARVUC        ARTVUC
572100041012     C                   MOVEL     WWFVC         ARTFVC
572200180124     c                   eval      wagvolart_a='S'
572300041012    3c                   ENDSL
572400041012     c                   ENDSR
572500041012     C**************************************************************************
572600041012     C* AGGIORNO Peso   cml
572700041012     C**************************************************************************
572800041012     c     AGGPES        BEGSR
572900041012    3C     BARPUC        IFGT      0
573000041012    4C     ARTPUC        IFEQ      0
573100041012     C* IL PESO ARRIVO O TRANSITO LI SOSTITUISCO SE QUELLO DELLA
573200041012     C* SPUNTA E' RILEVATO DALLA PARTENZA
573300041012     C     ARTPUC        ORNE      0
573400041012     C     WWFVC         ANDEQ     'P'
573500041012     C     ARTFPC        ANDNE     'P'
573600041012     C     ARTPUC        ORNE      0
573700041012     C     WWFVC         ANDEQ     'T'
573800041012     C     ARTFPC        ANDEQ     'A'
573900041012     c                   movel     'S'           wfpc
574000041012     C                   MOVEL     'S'           WAGGF             1
574100041012     C                   Z-ADD     BARPUC        ARTPUC
574200041012     C                   MOVEL     WWFVC         ARTFPC
574300180124     c                   eval      wagpesart_a='S'
574400041012    4C                   END
574500041012     C*
574600041012    3C                   END
574700041012     c                   ENDSR
574800091209     C**************************************************************************
574900091209     c*    Aggiorno fiar5 o lo scrivo se inesistente
575000091209     C**************************************************************************
575100091209     c     AggioFIAR5    BEGSR
575200091209     c
575300091209     c                   if        %found(fiar501l)
575400101129     c                   movel     datReale      §ar5arrdt
575500101129     c                   movel     oraReale      §ar5arrhm
575600091209     c                   movel(p)  dar5gen       ar5uni
575700091209      * se devo trasmettere alla sede sfleggo per la trasmissione
575800091209     c                   If        §Ar5Trse = 'S'
575900091209     c                   Clear                   Ar5Ft2
576000091209     c                   EndIf
576100091209     c                   update    fiar5000
576200091209      * Scrivo Fiar5
576300091209   x2c                   Else
576400091209     c                   Clear                   Fiar5000
576500091209     c                   Eval      Ar5Aas = ArtAas
576600091209     c                   Eval      Ar5Lnp = ArtLnp
576700091209     c                   Eval      Ar5Nrs = ArtNrs
576800091209     c                   Eval      Ar5Nsp = ArtNsp
576900091209     c                   Eval      Ar5Trd = 'GEN'
577000091209     c                   Move      WDATE1        Ar5Dac
577100091209     c                   time                    Ar5Orc
577200091209     c                   Clear                   Dar5Gen
577300101129     c                   movel     datReale      §ar5arrdt
577400101129     c                   movel     oraReale      §ar5arrhm
577500091209     c                   Movel(p)  DAr5Gen       Ar5Uni
577600091209     c                   Write     Fiar5000
577700091209    2c                   EndIf
577800091209     c                   ENDSR
577900041012     C**************************************************************************
578000041012     C* AGGIORNO anche la bolla di arrivo
578100041012     C**************************************************************************
578200041012     c     AGGARRIVO     BEGSR
578300041012     c* Aggiorno anche bolla  arrivi con dati del transito
578400130228     c                   clear                   aggioart          1
578500041012     C     KBAR          CHAIN     FNART27L
578600130228    1c                   if        %found(fnart27l)
578700041012     C                   MOVEL     'A'           ARTTBO
578800130228     c                   eval      Aggioart='S'
578900041012     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
579000041012     C* NE PRECEDENTE
579100041012     C     ARTSPE        IFNE      SAVSPE
579200041012     C     WAGGF         IFEQ      'S'
579300041012     C                   EXSR      WRTAGB
579400041012     C                   CLEAR                   WAGGF
579500041012     C                   ENDIF
579600041012     C                   MOVEL     ARTSPE        SAVSPE
579700041012     C                   ENDIF
579800110103     c*
579900041012     c* Codice anomalia: tengo sempre l'ultima anomalia
580000041012     C                   EXSR      AGGCAN
580100041012     C
580200041012     C* AGGIORNO DATA ENTRATA AL TRANSITO
580300041012     C                   Z-ADD     barDFV        ARTDET
580400041012     C                   MOVEL     barNPS        ARTPET
580500041012     C                   MOVEL     'S'           WAGGF             1
580600041012     C     barFGS        IFNE      ARTFLP
580700041012     C* FILIALE IN GESTIONE <> FILIALE TRANSITO
580800041012     C                   MOVEL     barFGS        ARTFLP
580900041012     C                   MOVEL     *ZEROS        ARTDUT
581000041012     C                   MOVEL     *ZEROS        ARTPUT
581100041012     C                   END
581200041012     c* peso e volume
581300041012     C     WWFVC         IFEQ      *BLANKS
581400041012     C                   MOVEL     'T'           WWFVC
581500041012     C                   END
581501180213     c                   clear                   wagvolart_a       1
581502180213     c                   clear                   wagpesart_a       1
581600041012     c                   EXSR      AGGVOL
581700041012     c                   EXSR      AGGpes
581800041012     c                   update    fnart000
581801180213    4c                   if        wagvolart_a='S' or wagpesart_a='S'
581802180213     c                   eval      w_puc = artpuc
581803180213     c                   eval      w_fpc = artfpc
581804180213     c                   eval      w_vuc = artvuc
581805180213     c                   eval      w_fvc = artfvc
581806180213     c                   exsr      updfiart
581807180213    4c                   endif
581808180213      * Aggiornamento per incompatibili per spedizioni che vanno in Sardegna
581809180213      * quando spunta Pisa
581810180216         wdarr = artdet;
581811180215         exsr sr_inc;
581825180215     c*                   if        bpes=99     and barnpg=2
581826180215     c*                             and %lookup(barlna:lnaf)>0 and
581827180215     c*                             (barspg='J'  or
581828180215     c*                             (savcan='S' and
581829180215     c*                             (AllINC='S' or
581830180215     c*                              %lookup(%editc(bpes:'X'):fic)>0
581831180215     c*                              )))
581832180215     c*                   exsr      sr_addinc
581833180215     c*                   ENDIF
581900130228     c
582000041013     c* Se trovata bolla, eseguo solo per disguido la CRTA60
582100160624     c* anomalie 60 e 61 eliminate
582200050322     c                   eval      savesart=wesart
582300041013     c                   movel     'S'           wesart
582400160624     c**                 if        wtibol='D'
582500160624     c**                 eval      wsavflg=dls45FLG
582600160624     c**                 eval      dls45FLG='6'
582700160624     c**                 exsr      CRTA60
582800160624     c**                 eval      dls45FLG=wsavflg
582900160624     c**                 endif
582901180214   x1c                   else
582902180214    3c                   if        wesblt='S' and bltdcm=0
582903180214     c
582904180214     c                   eval      wriagb='N'
582905180214     c                   exsr      Aggpesvol_p
582906180214     c                   clear                   wriagb            1
582907180214     c
582908180214    4c                   if        wagvolart_a='S' or wagpesart_a='S'
582909180214     c                   eval      w_puc = bltpuc
582910180214     c                   eval      w_fpc = bltfpc
582911180214     c                   eval      w_vuc = bltvuc
582912180214     c                   eval      w_fvc = bltfvc
582913180214     c                   exsr      UpdFiart
582914180214     c                   clear                   WAGvolart
582915180214     c                   clear                   WAGpesart
582916180214    4c                   endif
582917180215      * Aggiornamento per incompatibili per spedizioni che vanno in Sardegna
582918180215      * quando spunta Pisa
582919180216         wdarr=0 ;
582920180215         exsr sr_inc;
582930180214     c
582931180214    3c                   ENDIF
583000130228     c
583100130228    1c                   endif
583200041013     c* Chiudo una eventuale anomalia 15 aperta controllando le date anomali
583300041013     c*  e spunta
583400041013     c                   z-add     15            comcaa
583500041013     c                   eval      wcontrdfv='S'
583600041013     c                   eval      wstess='S'
583700041013     c                   eval      wchiu='S'
583800041013     c                   exsr      chiuan
583900130228     c
584000130228    1c                   if        Aggioart='S'
584100130228     c                   eval      wesart=savesart
584200130228     c                   endif
584300041013     c*
584400041012     c                   ENDSR
584402180215      *---------------------------------------------------------------*
584403180215      *?Verifica se colli incompatibili                                ?*
584404180215      *---------------------------------------------------------------*
584405180215     c     sr_inc        BEGSR
584406180215        if barnpg=2 and (barspg='J' or
584407180215                        (savcan='S' and
584408180215                        (ALLINC='S' or
584409180215                         %lookup(%editc(bpes:'X'):fic)>0
584410180215                         )))      ;
584411180215          w_key1=%editc(barlna:'X');
584412180215          chain ('ETF':w_key1) tntbe01l ;
584413180215          if %found (tntbe01l) ;
584414180215            setf_ds=tbeuni;
584415180215            if %lookup(%editc(bpes:'X'):setf)>0 ;
584416180215              exsr sr_addinc;
584417180215            endif;
584418180215          endif;
584419180215        ENDIF;
584421180215     c                   ENDSR
584500070208     C**************************************************************************
584600070208     C* Routine iniziale
584700070208     C**************************************************************************
584800070208     c     *inzsr        BEGSR
584900070208     C     KBAR          KLIST
585000070208     C                   KFLD                    BARLNP
585100070208     C                   KFLD                    BARLNA
585200070208     C                   KFLD                    BARNRS
585300070208     C                   KFLD                    BARNSC
585400170505     C     Kbolla        KLIST
585500170505     C                   KFLD                    comaas
585600170505     C                   KFLD                    comlnp
585700170505     C                   KFLD                    BARNRS
585800170505     C                   KFLD                    comnsp
585900070208     C     KBAR37        KLIST
586000070208     C                   KFLD                    bpes
586100070208     C                   KFLD                    BARLNP
586200070208     C                   KFLD                    BARLNA
586300070208     C                   KFLD                    BARNRS
586400070208     C                   KFLD                    BARNSC
586500160721     C     KBAR_t        KLIST
586600160721     C                   KFLD                    savflp
586700160721     C                   KFLD                    BARLNP
586800160721     C                   KFLD                    BARLNA
586900160721     C                   KFLD                    BARNRS
587000160721     C                   KFLD                    BARNSC
587100070208     C     KBAR1         KLIST
587200070208     C                   KFLD                    BARNPG
587300070208     C                   KFLD                    BARNfv
587400070208     C                   KFLD                    BARfgs
587500070208     C                   KFLD                    BARLNP
587600070208     C                   KFLD                    BARLNA
587700070208     C                   KFLD                    BARNRS
587800070208     C                   KFLD                    BARNSC
587900140616     C     KBAR_qd4      KLIST
588000140616     C                   KFLD                    kdel_npg
588100140616     C                   KFLD                    BARLNP
588200140616     C                   KFLD                    BARLNA
588300140616     C                   KFLD                    BARNRS
588400140616     C                   KFLD                    BARNSC
588500140616     C                   KFLD                    BARfgs
588600140616     C                   KFLD                    qd_dfs
588700140616     C                   KFLD                    qd_hms
588800070208     C     KBAR2         KLIST
588900070208     C                   KFLD                    BARNPG
589000070208     C                   KFLD                    BARLNP
589100070208     C                   KFLD                    BARLNA
589200070208     C                   KFLD                    BARNRS
589300070208     C                   KFLD                    BARNSC
589400070208     C                   KFLD                    BARfgs
589500140613     C     KBAR2_d       KLIST
589600140613     C                   KFLD                    kdel_npg
589700140613     C                   KFLD                    BARLNP
589800140613     C                   KFLD                    BARLNA
589900140613     C                   KFLD                    BARNRS
590000140613     C                   KFLD                    BARNSC
590100140613     C                   KFLD                    BARfgs
590200140616     C                   KFLD                    wdfs
590300070208     C     KBAR3         KLIST
590400070208     C                   KFLD                    WNPG
590500070208     C                   KFLD                    BARLNP
590600070208     C                   KFLD                    BARLNA
590700070208     C                   KFLD                    BARNRS
590800070208     C                   KFLD                    BARNSC
590900070208     C     KBRV7         KLIST
591000070208     C                   KFLD                    BARLNP
591100070208     C                   KFLD                    BARLNA
591200070208     C                   KFLD                    BARNRS
591300070208     C                   KFLD                    BARNSC
591400070208     C     KARB          KLIST
591500070208     C                   KFLD                    ARTAAS
591600070208     C                   KFLD                    ARTLNP
591700070208     C                   KFLD                    ARTNRS
591800070208     C                   KFLD                    ARTNSP
591900070208     C     KGCA          KLIST
592000070208     C                   KFLD                    ARTAAS
592100070208     C                   KFLD                    ARTLNP
592200070208     C                   KFLD                    ARTNRS
592300070208     C                   KFLD                    ARTNSP
592400070208     C                   KFLD                    KFRG
592500070208     C     KSAV          KLIST
592600070208     C                   KFLD                    SAVTBO
592700070208     C                   KFLD                    SAVAAS
592800070208     C                   KFLD                    SAVLNP
592900070208     C                   KFLD                    SAVNRS
593000070208     C                   KFLD                    SAVNSP
593100070208     C                   KFLD                    KAGB
593200070208     C     KEAGB         KLIST
593300070208     C                   KFLD                    WTBO
593400070208     C                   KFLD                    WDAAS
593500070208     C                   KFLD                    WDLNP
593600070208     C                   KFLD                    WDNRS
593700070208     C                   KFLD                    WDNSP
593800070208     C                   KFLD                    KAGB
593900070208     C     KBLT          KLIST
594000070208     C                   KFLD                    BLTAAS
594100070208     C                   KFLD                    BLTLNP
594200070208     C                   KFLD                    BLTNRS
594300070208     C                   KFLD                    BLTNSP
594400070208     C     KBTT          KLIST
594500070208     C                   KFLD                    BTTflp
594600070208     C                   KFLD                    BTTAAS
594700070208     C                   KFLD                    BTTLNP
594800070208     C                   KFLD                    BTTNRS
594900070208     C                   KFLD                    BTTNSP
595000070208     C     KANM          KLIST
595100070208     C                   KFLD                    BARLNP
595200070208     C                   KFLD                    BARLNA
595300070208     C                   KFLD                    BARNRS
595400070208     C                   KFLD                    BARNSC
595500070208     C                   KFLD                    COMCAA            3 0
595600070208     C     KEVS          KLIST
595700070208     C                   KFLD                    BARLNP
595800070208     C                   KFLD                    BARLNA
595900070208     C                   KFLD                    BARNRS
596000070208     C                   KFLD                    BARNSC
596100070208     C                   KFLD                    KCEV
596200070208     C     KFVV1         KLIST
596300070208     C                   KFLD                    WNPG
596400070208     C                   KFLD                    WNFV
596500070208     C                   KFLD                    WFGS
596600070208     C     KFVV          KLIST
596700070208     C                   KFLD                    WNPG
596800070208     C                   KFLD                    wdata
596900070208     C                   KFLD                    KTFA2
597000070208     C     KFVA1         KLIST
597100070208     C                   KFLD                    WFGS
597200070208     C                   KFLD                    WNFV
597300070208     C     KFGV1         KLIST
597400070208     C                   KFLD                    WNFV
597500070208     C                   KFLD                    tfpfgs
597600070208     c     KFGV2         KLIST
597700070208     C                   KFLD                    lnptfp
597800070208     C                   KFLD                    bardfv
597900070208     C     KFVA4         KLIST
598000070208     C                   KFLD                    KNPG4
598100070208     C                   KFLD                    KNFV4
598200070208     C                   KFLD                    KFGS4
598300070208     C     KFWA          KLIST
598400070208     C                   KFLD                    FVALNP
598500070208     C                   KFLD                    FVANFV
598600070208     C                   KFLD                    FVALAI
598700070208     C     KTAB          KLIST
598800070208     C                   KFLD                    CODUT
598900070208     C                   KFLD                    COD
599000070208     C                   KFLD                    KEY
599100070208     C     KTAB2         KLIST
599200070208     C                   KFLD                    CODUT
599300070208     C                   KFLD                    COD
599400130228     C     KTBE2         KLIST
599500130228     C                   KFLD                    KCOD
599600130228     C                   KFLD                    KKE1
599700130228     C                   KFLD                    KKE2
599800070208     C*
599900070208     C     KLBL          KLIST
600000070208     C                   KFLD                    LBLAAN
600100070208     C                   KFLD                    LBLLPN
600200070208     C                   KFLD                    LBLNRN
600300070208     C                   KFLD                    LBLNSN
600400070208     C     KDCT          KLIST
600500070208     C                   KFLD                    DCDAAC
600600070208     C                   KFLD                    DCDFIL
600700070208     C                   KFLD                    DCDNCA
600800070208     C     KBRVE2        KLIST
600900070208     C                   KFLD                    BRVNPG
601000070208     C                   KFLD                    BRVLNP
601100070208     C                   KFLD                    BRVLNA
601200070208     C                   KFLD                    BRVNRS
601300070208     C                   KFLD                    BRVNSC
601400070710     C     KBRVE1        KLIST
601500070710     C                   KFLD                    BRVNPG
601600070710     C                   KFLD                    BRVNfv
601700070710     C                   KFLD                    BRVfgs
601800070710     C                   KFLD                    BRVLNP
601900070710     C                   KFLD                    BRVLNA
602000070710     C                   KFLD                    BRVNRS
602100070710     C                   KFLD                    BRVNSC
602200070208     C     Kar5          KLIST
602300070208     C                   KFLD                    anmaas
602400070208     C                   KFLD                    anmLNp
602500070208     C                   KFLD                    anmNRS
602600070208     C                   KFLD                    anmnsp
602700070208     C                   KFLD                    Ktrd
602800091211     C     Kar52         KLIST
602900091211     C                   KFLD                    artaas
603000091211     C                   KFLD                    artLNp
603100091211     C                   KFLD                    artNRS
603200091211     C                   KFLD                    artnsp
603300091211     C                   KFLD                    Ktrd
603400180115     C     Kars          KLIST
603500180115     C                   KFLD                    artaas
603600180115     C                   KFLD                    artLNp
603700180115     C                   KFLD                    artNRS
603800180115     C                   KFLD                    artnsp
603900180115     C                   KFLD                    Karstr
604000070208     C*
604100070208     C     *LIKE         DEFINE    FVAdfv        Wpul050
604200070208     C     *LIKE         DEFINE    FVALNP        WFVALNP
604300070208     C     *LIKE         DEFINE    FVANFV        WFVANFV
604400070208     C     *LIKE         DEFINE    FVADFV        WFVADFV
604500070208     C     *LIKE         DEFINE    bardfv        Wdata
604600070208     C     *LIKE         DEFINE    wfgs          WWWfgs
604700070208     C     *LIKE         DEFINE    wfgs          tfpfgs
604800070208     C     *LIKE         DEFINE    wfgs          savfgs
604900070208     c                   clear                   savfgs
605000070208     C     *LIKE         DEFINE    FVVFLE        WFLE
605100070208     C     *LIKE         DEFINE    ARTFLP        SAVFLP
605200070208     C     *LIKE         DEFINE    BARDFV        DSPB
605300070208     C     *LIKE         DEFINE    BARDFV        COMDFV
605400070208     C     *LIKE         DEFINE    BARDFV        SVVDFV
605500070208     C     *LIKE         DEFINE    BLPTFP        LNPTFP
6056000702080    C     *LIKE         DEFINE    BARDFV        WDTRIF
605700070208     C     *LIKE         DEFINE    BARDFV        PRIDAT
605800070208     C     *LIKE         DEFINE    BARDFV        SA2DFV
605900070208     C     *LIKE         DEFINE    ANMCCH        WCCH
606000070208     C     *LIKE         DEFINE    GCPFRG        KFRG
606100070208     C                   MOVEL     '0'           KFRG
606200070208     C     *LIKE         DEFINE    AGBAGB        KAGB
606300070208     C     *LIKE         DEFINE    AGBFBS        WFBS
606400070208     C     *LIKE         DEFINE    AGBFVC        WFVC
606500070208     C     *LIKE         DEFINE    AGBFPC        WFPC
606600070208     C     *LIKE         DEFINE    ARBNDC        WABNDC
606700070208     C     *LIKE         DEFINE    ARTDCM        WATDCM
606800070208     C     *LIKE         DEFINE    ARbcca        WABcca
606900070208     C     *LIKE         DEFINE    ARbcca        savcca
607000070208     C     *LIKE         DEFINE    BARLNA        SVVLNA
607100070208     C     *LIKE         DEFINE    BARLNP        SVVLNP
607200070208     C     *LIKE         DEFINE    ARTDCM        WDCM
607300070208     C     *LIKE         DEFINE    ANMCAA        CAACRE
607400070208     C     *LIKE         DEFINE    ANMCAA        CAACIU
607500070208     C     *LIKE         DEFINE    FGVTTR        WTTR
607600140613     C     *LIKE         DEFINE    FGVFCF        WFCF
607700070208     C     *LIKE         DEFINE    EVSCEV        KCEV
607800070208     C     *LIKE         DEFINE    D33MAC        SAVMAC
607900070208     C     *LIKE         DEFINE    BRVFLE        BARFLE
608000070208     C     *LIKE         DEFINE    BRVFGS        SAVPES
608100070208     C     *LIKE         DEFINE    BRVFGS        KFGS4
608200070208     C     *LIKE         DEFINE    TBLCOD        COD
608300070208     C     *LIKE         DEFINE    TBLKEY        KEY
608400070208     C     *LIKE         DEFINE    BARLNA        LNATFA
608500070208     C     *LIKE         DEFINE    BARLNA        Usalnatfa
608600100713     C     *LIKE         DEFINE    BARLNA        WgenLNAtfa
608700070208     C     *LIKE         DEFINE    blptfa        bolTFA
608800070208     C     *LIKE         DEFINE    BARLNA        PESTFA
608900070208     C     *LIKE         DEFINE    BARLNA        PESTFP
609000070208     C     *LIKE         DEFINE    BARLNP        LNPB
609100070208     C     *LIKE         DEFINE    FVVFGS        WFGS
609200070208     C     *LIKE         DEFINE    FVVSPG        WSPG
609300070208     C     *LIKE         DEFINE    FVVNPG        WNPG
609400070208     C     *LIKE         DEFINE    FVVNPG        KNPG4
609500070208     C     *LIKE         DEFINE    FVVNFV        WNFV
609600070208     C     *LIKE         DEFINE    FVVNFV        KNFV4
609700070208     C     *LIKE         DEFINE    FVVFLE        WLIN
609800070208     C     *LIKE         DEFINE    FVVFLE        WLINPR
609900070208     C     *LIKE         DEFINE    FVVSPG        BARSPG
610000140616     C     *LIKE         DEFINE    FVVSPG        brvSPG
610100070208     C     *LIKE         DEFINE    FVVDFV        WDFV
610200070208     C     *LIKE         DEFINE    FVVDFV        SAVDFV
610300070208     C     *LIKE         DEFINE    FVVDFV        BARDFV
610400070208     C     *LIKE         DEFINE    FVVDFV        BRVDFV
610500070208     C     *LIKE         DEFINE    BARKEY        SAVKEY
610600070208     C     *LIKE         DEFINE    BRVKEY        SA1KEY
610700070208     C     *LIKE         DEFINE    ARBLNP        COMLNP
610800070208     C     *LIKE         DEFINE    ARBAAS        COMAAS
610900070208     C     *LIKE         DEFINE    ARBNSP        COMNSP
611000070208     C     *LIKE         DEFINE    ARBTFP        WPTFP
611100070208     C     *LIKE         DEFINE    ANMFL1        WFL1
611200070208     C     *LIKE         DEFINE    D55TFA        KTFA2
611300070208     C     *LIKE         DEFINE    D55TFA        SAVTFA
611400070208     C     *LIKE         DEFINE    ARTFVC        WWFVC
611500070208     C     *LIKE         DEFINE    ARTDAM        SAVDAM
611600070208     C     *LIKE         DEFINE    ARTDAM        WARDAM
611700070208     C     *LIKE         DEFINE    ARTDAM        WSVDAM
611800070208     C     *LIKE         DEFINE    FVADFV        SAVDAO
611900070208     C     *LIKE         DEFINE    FVALNP        WVALNP
612000070208     C     *LIKE         DEFINE    DCTTAD        SAVTAD
612100070208     C                   CLEAR                   SAVTAD
612200070208     C     *LIKE         DEFINE    §OGNTW        NTWBAR
612300070208     C     *LIKE         DEFINE    GI8INV        DATMIN
612400070208     C     *LIKE         DEFINE    BRVATR        SAWATR
612500080317     C     *LIKE         DEFINE    BRVTSU        SAWtsu
612600070208     C*
612700070208     C                   MOVEL     'DT'          KAGB
612800070208     c                   ENDSR
612900140613     C**************************************************************************
613000140613     C* Cerca spunta 4
613100140613     C**************************************************************************
613200140613     c     Cer_cons      BEGSR
613300140616     c* imposto data immiss.spunta della spunta che sto caricando
613400140616     c                   exsr      ImpoDFS
613500140616     c*
613600140616     C     KBAR2_d       setll     FNBRV05L
613700140616     C     KBAR2_d       reade(n)  FNBRV05L
613800140616    1c                   dow       not %eof(fnbrv05l)
613900140613     c* controllo data e ora immissione
614000140613     c                   clear                   brv_imm
614100140613     c                   clear                   brv_sca
614200140613     c                   z-add     brvhms        brv_imm
614300140613     c                   movel     brvdfs        brv_imm
614400140613     c                   z-add     brvhcs        brv_sca
614500140613     c                   movel     brvdcs        brv_sca
614600140613     c                   exsr      sceglidata
614700140613     c                   z-add     a_imm         b_imm
614800140613     c
614900140613     c* a parità di data devo tenere la più recente
615000140613     c                   clear                   brv_imm
615100140613     c                   clear                   brv_sca
615200140613     c                   z-add     barhms        brv_imm
615300140613     c                   movel     wDFS          brv_imm
615400140613     C                   Z-ADD     WDATE1        barDCS
615500140613     C                   time                    barHCS
615600140613     c                   z-add     barhcs        brv_sca
615700140613     c                   movel     bardcs        brv_sca
615800140613     c                   exsr      sceglidata
615900140613     c*
616000140613     c                   select
616100140616     c* quella trovata + recente --> non carico la spunta
616200140613    2c                   when      b_imm > A_IMM  and brvdfs=wdfs
616300140613     c
616400140613     c* Se anche la data foglio è = non carico la spunta ricevuta
616500140613    3C     BRvKEY        IFNE      SA1KEY
616600140613     C                   MOVEL     BRvKEY        SA1KEY
616700140613     C                   Z-ADD     BRvNPG        WNPG
616800140613     C                   Z-ADD     BRvNFV        WNFV
616900140613     C                   Z-ADD     BRvFGS        WFGS
617000140613     C                   movel     BRvnps        Wnps
617100140613     C                   EXSR      RILFOG
617200140613     c                   Z-ADD     WDFV          BRVDFV
617300140616     c                   movel     WFCF          BRVFCF
617400140616     c                   movel     Wspg          BRVspg
617500140613    3c                   endif
617600140613     c
617700140616     c                   if        barnpg=3 and
617800140616    3c                             brvdfv=bardfv
617900140616     c                   eval      amb_QD ='N'
618000140616     c                   endif
618100140616     c                   if        barnpg=4 and brvspg='A' and
618200140616    3c                             brvdfv=bardfv
618300140616     c                   eval      amb_QD ='N'
618400140616     c                   endif
618500140613     c
618600140616     c                   if        amb_qd='N'
618700140613     c* memorizzo la spunta nuova negli errori
618800140613     C                   MOVEL     'S'           WBRVE             1
618900140613     C                   EXSR      RIEMPI
619000140613     C                   CLEAR                   WBRVE
619100140613     C                   WRITE     FNBRVE
619200140616     c                   leave
619300140613    3c                   endif
619400140616     c
619500140616     c* quella di carico + recente --> cancello quella già presente
619600140613    2c                   when      A_imm > B_IMM  and brvdfs=wdfs
619700140613     c* Se anche la data foglio è = non carico la spunta ricevuta
619800140613    3C     BRvKEY        IFNE      SA1KEY
619900140613     C                   MOVEL     BRvKEY        SA1KEY
620000140613     C                   Z-ADD     BRvNPG        WNPG
620100140613     C                   Z-ADD     BRvNFV        WNFV
620200140613     C                   Z-ADD     BRvFGS        WFGS
620300140613     C                   movel     BRvnps        Wnps
620400140613     C                   EXSR      RILFOG
620500140613     c                   Z-ADD     WDFV          BRVDFV
620600140616     c                   movel     WFCF          BRVFCF
620700140616     c                   movel     Wspg          BRVspg
620800140613    3c                   endif
620900140613     c
621000140616     c                   if        barnpg=3 and
621100140616    3c                             brvdfv=bardfv and brvfcf=' '
621200140616     c                   eval      amb_QD ='S'
621300140613     c* mi slavo data e ora imm spunta 4 per la delete
621400140616     c                   eval      qd_npg=4
621500140616     c                   eval      qd_dfs=brvdfs
621600140616     c                   eval      qd_hms=brvhms
621700140616     c                   leave
621800140613     c                   endif
621900140616     c                   if        barnpg=4 and  brvspg='A' and
622000140616    3c                             brvdfv=bardfv and brvfcf=' '
622100140616     c                   eval      amb_QD ='S'
622200140616     c* mi slavo data e ora imm spunta 4 per la delete
622300140616     c                   eval      qd_npg=3
622400140616     c                   eval      qd_dfs=brvdfs
622500140616     c                   eval      qd_hms=brvhms
622600140616     c                   eval      qd_nfv=brvnfv
622700140616     c                   leave
622800140616     c                   endif
622900140613     c
623000140613    2c                   endsl
623100140616     c
623200140616     C     KBAR2_d       reade(n)  FNBRV05L
623300140616    1c                   enddo
623400140613     c
623500140613     c                   ENDSR
623600140613     C**************************************************************************
623700140613     C* Scelta data tra immisisone e caicamento
623800140613     C**************************************************************************
623900140613     c     Sceglidata    BEGSR
624000140613     c                   if         brv_imm>brv_sca and
624100140616     c                              %subst(%editc(brv_sca:'X'):1:8)>*zeros
624200140613     c                   eval       A_imm=brv_sca
624300140613     c                   else
624400140613     c                   eval       A_imm=brv_imm
624500140613     c                   endif
624600140613     c                   ENDSR
624700140613     C**************************************************************************
624800140613     C*    impostazione data immissione spunta
624900140613     C**************************************************************************
625000140613     c     ImpoDFS       BEGSR
625100140613     c* Se data immissione > di oggi, imposto la minore tra oggi e data fogl
625200140613     c                   if        BARDFS<=wdate1
625300140613     C                   Z-ADD     BARDFS        wDFS
625400140613     c                   else
625500140613     c                   if        wdate1<=bardfv
625600140613     c                   z-add     wdate1        wDFS
625700140613     c                   else
625800140613     c                   z-add     bardfv        wDFS
625900140613     c                   endif
626000140613     c                   endif
626100140613     c                   ENDSR
626200140616     C**************************************************************************
626300140616     C*    cancellazione spunta meno recente IMA o cons
626400140616     C**************************************************************************
626500140616     c     Canc_QD       BEGsr
626600140616     c                   eval      kdel_npg=qd_npg
626700140616    1c                   if        kdel_npg=4
626800140616     c     kbar_qd4      chain     fnbrv05l                             38
626900140616     c  n38              if        %found(fnbrv05l)
627000140616     C     KBRVE1        SETLL     FNBRVE1L                               33
627100140616    2C     *IN33         IFEQ      *OFF
627200140616     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
627300140616     C                   WRITE     FNBRVE
627400140616    2C                   ENDIF
627500140616     c
627600140616     c                   delete    fnbrv005
627700141029     c                   endif
627800140616     c
627900140616   x1c                   else
628000140616     c     kbar_qd4      setll     fnbrv05l
628100140616     c     kbar_qd4      reade     fnbrv05l
628200140616    2c                   dow       not %eof(fnbrv05l) and brvnfv<>qd_nfv
628300140616     c     kbar_qd4      reade     fnbrv05l
628400140616    2c                   enddo
628500140616    2c                   if        not %eof(fnbrv05l)
628600140616     C     KBRVE1        SETLL     FNBRVE1L                               33
628700140616    3C     *IN33         IFEQ      *OFF
628800140616     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
628900140616     C                   WRITE     FNBRVE
629000140616    3C                   ENDIF
629100140616     c                   delete    fnbrv005
629200140616    2c                   endif
629300140616     c
629400140616    1c                   endif
629500140616     c                   ENDSR
629600150622     C**************************************************************************
629700150622     C*    Aggiornamenti da tabella BSP
629800150622     C**************************************************************************
629900150622     c     DA_BSP        BEGsr
630000150622     c* Scrivo FNAGB per aggiornare la bolla
630100150622     C                   CLEAR                   FNAGB000
630200150622     C                   MOVEL     'P'           AGBTBO
630300150622     C                   MOVEL     'CB'          AGBAGB
630400150622     C                   MOVEL     blpAAS        AGBAAS
630500150622     C                   MOVEL     blpLNP        AGBLNP
630600150622     C                   MOVEL     blpNRS        AGBNRS
630700150622     C                   MOVEL     blpNSP        AGBNSP
630800150622     C                   WRITE     FNAGB000                             99
630900150622     c                   ENDSR
630901180206     C**************************************************************************
630903180206     C*    Ripristino bolla chiusa per merce mai affidata
630904180206     C**************************************************************************
630906180206     c     sr_agbRB      begsr
630907180206     c* Scrivo FNAGB per aggiornare la bolla
630908180206     C                   CLEAR                   FNAGB000
630909180206     C                   MOVEL     'P'           AGBTBO
630910180206     C                   MOVEL     'RB'          AGBAGB
630911180206     C                   MOVEL     blpAAS        AGBAAS
630912180206     C                   MOVEL     blpLNP        AGBLNP
630913180206     C                   MOVEL     blpNRS        AGBNRS
630914180206     C                   MOVEL     blpNSP        AGBNSP
630915180206     C                   WRITE     FNAGB000                             99
630916180206     c
630917180206     c                   ENDSR
631000070208     C**************************************************************************
631100070208     C* Elaborazioni per chiusura pgm
631200070208     C**************************************************************************
631300070208     c     ELACHIUDI     BEGSR
631400070208     C* ELABORO L'ULTIMO DATO MEMORIZZATO
631500070208     C                   EXSR      WRTAGB
631600070208     c
631700070208     C                   CLEAR                   DSLV55
631800070208     C                   MOVEL     'C'           D55TLA
631900070208     C                   EXSR      FNLV55
632000070208     C* SCRITTURA ULTIMO RECORD IN FNSTA
632100070208     C   02              EXSR      WRTSTA
632200070208     C**
632300070208     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
632400070208     c                   clear                   fidn12ds
632500070208     c                   eval      i12tla = 'C'
632600070208     c                   call      'FIDN12R'
632700070208     c                   parm                    fidn12ds
632800161007     c
632900161007     c                   clear                   fnlgi1ds
633000161007     c                   eval      ilgi1tla = 'C'
633100161007     c                   movel     fnlgi1ds      kpjbu
633200161007     c                   call      'FNLGI1R'
633300161007     c                   parm                    kpjba
633400070208     c                   ENDSR
633500110103
633600110103      *---------------------------------------------------------------*
633700110103      *?Reperimento dati del job (Utente/Operativi)                  ?*
633800110103      *---------------------------------------------------------------*
633900110103     c     sr_DatiJob    BEGSR
634000110103      *
634100110103     c     *dtaara       define    §azute        AZUTEds
634200110103     c     *dtaara       define    §datiute      dDatiUte
634300110103      *
634400110103     c                   in(E)     *dtaara
634500110103     c                   IF        %Error or RSUT = *blanks
634600110103     c                   clear                   TIBS34ds
634700110103     c                   call      'TIBS34R'
634800110103     c                   parm                    TIBS34ds
634900110103     c                   in        *dtaara
635000110103     c                   ENDIF
635100110103      *
635200110103     c                   ENDSR
635300180124      *---------------------------------------------------------------*
635400180124      *?Aggiunta segnalazione di incompatibile                       ?*
635500180124      *---------------------------------------------------------------*
635600180124     c     sr_addinc     BEGSR
635700180125     c* se presente spunta vdl non si tratta di incompatibile
635800180125          chain(N) (barlnp:barlna:barnrs:barnsc) fiart27l;
635900180125          if %found(fiart27l);
636000180125             dartflo=U_artflo ;
636100180125           if §flotap<>'V' ;
636200180124             setll (barlnp:barlna:barnrs:barnsc:'J') fiars01l  ;
636300180124             if not %equal(fiars01l);
636400180124                clear fiars000;
636500180124                ARSFLS=barlnp;
636600180124                ARSLNA=barlna;
636700180124                ARSNRS=barnrs;
636800180124                ARSNSC=barnsc;
636900180124                ARSTRC='J'   ;
637000180124
637100180124                clear darsj;
637200180124                §ARSJPEST=bpes;
637300180124                §ARSJNPGT=barnpg;
637400180124                §ARSJDFVT=bardfv;
637500180126                §ARSJNPST=%editc(barnps:'X');
637600180125                select;
637700180129                when barspg='J';
637800180125                 §ARSJTIPO='J' ;
637900180125                other;
638000180125                 §ARSJTIPO='S' ;
638100180125                endsl;
638200180124                ARSNOT=darsj;
638300180124
638400180124                ARSDUV=wdate1     ;
638500180124                write fiars000   ;
638501180216                // devo rielaborare la statistica arrivi se data > della data ar
638502180216                // al transito
638503180216                if wdarr> 0 and bardfv > wdarr;
638504180216     C                   CLEAR                   SA2TLA
638505180216     C     dls45FLG      IFEQ      'C'
638506180216     C                   MOVEL     'L'           SA2TLA
638507180216     C                   END
638508180216     C                   MOVEL     'A'           SA2TRC
638509180216     C                   Z-ADD     wdarr         SA2DTS
638510180216     C                   MOVE      *ZEROS        SA2LNA
638511180216     C                   CALL      'FNLSA2R3'
638512180216     C                   PARM                    SA2TLA            1
638513180216     C                   PARM                    SA2TRC            1
638514180216     C                   PARM                    SA2DTS            8 0
638515180216     C                   PARM                    SA2LNA            3 0
638516180216
638517180216                ENDIF;
638600180124         // Scrivo AGB per aggiornare bolla in sede
638700180124     C                   CLEAR                   FNAGB000
638800180124     C                   MOVEL     'A'           AGBTBO
638900180124     C                   MOVEL     'JA'          AGBAGB
639000180130     C                   MOVEL     u_artAAS      AGBAAS
639100180130     C                   MOVEL     u_artLNP      AGBLNP
639200180130     C                   MOVEL     u_artNRS      AGBNRS
639300180130     C                   MOVEL     u_artNSP      AGBNSP
639400180124     C                   WRITE     FNAGB000                             99
639500180124             endif;
639600180125          endif;
639700180125         endif;
639800180124     c                   ENDSR
639900180124      *---------------------------------------------------------------*
640000180124      *?Cancellazione segnalazione di incompatibile a fronte di VDL  ?*
640100180124      *---------------------------------------------------------------*
640200180124     c     sr_anninc     BEGSR
640300180124      *
640400180124           chain (barlnp:barlna:barnrs:barnsc:'J') fiars01l  ;
640500180124           if %found(fiars01l);
640600180124              delete fiars000;
640700180124         // Scrivo AGB per aggiornare bolla in sede
640800180124     C                   CLEAR                   FNAGB000
640900180124     C                   MOVEL     'A'           AGBTBO
641000180124     C                   MOVEL     'JD'          AGBAGB
641100180130     C                   MOVEL     u_artAAS      AGBAAS
641200180130     C                   MOVEL     u_artLNP      AGBLNP
641300180130     C                   MOVEL     u_artNRS      AGBNRS
641400180130     C                   MOVEL     u_artNSP      AGBNSP
641500180124     C                   WRITE     FNAGB000                             99
641600180124           endif;
641700180124      *
641800180124     c                   ENDSR
641900180124      *---------------------------------------------------------------*
642000180124      * Aggiornamento Volume, peso vdl e tipo apparato su FIART      ?*
642100180124      *---------------------------------------------------------------*
642200180124     c     updFiart      BEGSR
642300180130     c* Per ora se trovo allocato il record di FIART non aggiorno
642400180130     c*  il tipo apparato e per ora non vado a tiogliere eventuale
642500180130     c*  segnalazione di incompatibile
642600180124     C     KBAR          CHAIN     FiART27L                           3434
642700180124    5c                   if        not *in34
642800180124    6c                   if        wagpesart_a = 'S'
642900180126     c                   eval      U_artpuc=w_puc
643000180126     c                   eval      U_artfpc=w_fpc
643100180124    6c                   endif
643200180124    6c                   if        wagvolart_a='S'
643300180126     c                   eval      U_artvuc=w_vuc
643400180126     c                   eval      U_artfvc=w_fvc
643500180124    6c                   endif
643600180124     c                   eval      dartflo=U_artflo
643700180124     c* se TAP fiart vuoro --> aggiorno sempre col tap della spunta
643800180124    6c                   if        §flotap=*blanks
643900180124     c                   eval      §flotap=bartap
644000180124   x6c                   else
644100180124     c* se TAP fiart pieno --> aggiorno solo se tap della spunta "V"
644200180124    7c                   if        bartap='V'
644300180124     c                   eval      §flotap=bartap
644400180124    7c                   endif
644500180124    6c                   endif
644600180124     c                   eval      U_artflo=dartflo
644700180124     c                   update    fiart000
644800180130
644900180125     c* se passata spunta VDL devo annullare eventuale segnalazione di incompati
645000180125     c* bile
645100180125     c                   if        §flotap='V'
645200180125     c                   exsr      sr_anninc
645300180125     c                   endif
645400180130
645500180130    5c                   endif
645600180124     c                   endsr
645700040601**         CM5
645800040601DLYJOB DLY(5)
645900980928**         MSG
646000070906CARICA SPUNTE:Fogli Inesitenti (FGS NPG NFV-NPS) FNLS45R
646100070906XXX X XXXXX-xx;  XXX X XXXXX-xx;  XXX X XXXXX-xx;  XXX X XXXXX-xx;
646200070906           -  ;             -  ;             -  ;             -  .
