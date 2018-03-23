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
003100960520     FFNFGV01L  IF   E           K DISK
003200040908     FFNFGV02L  IF   E           K DISK
003300040908     F                                     RENAME(FNFgV000:FNFgV002)
003400000505     FFNFGW01L  IF   E           K DISK
003500021002     FFNFVV01L  IF   E           K DISK
003600070207     FFNFVV02L  IF   E           K DISK    prefix(FV2:3)
003700950927     F                                     RENAME(FNFVV000:FNFVV002)
003800990120     FFNDCT01L  IF   E           K DISK
003900990120     F                                     RENAME(FNDCT000:FNDCT001)
004000990126     FFNDCD02L  UF   E           K DISK
004100950927     FFNFVA02L  IF   E           K DISK
004200150622     FFIBSP01L  IF   E           K DISK
004300150622     FFIBSP02L  IF   E           K DISK    rename(fibsp000:fibsp02)
004400990518     FFNANM02L  UF A E           K DISK
004500990518     F                                     INFDS(FNANM2)
004600970217     FFNAGB01L  UF A E           K DISK
004700970205     FFNLBL02L  IF   E           K DISK
004800941206     F* BOLLE ARRIVI ----------------------
004900941206     FFNART27L  UF   E           K DISK
005000050223     FFNARB01L  uF   E           K DISK
005100941206     F* BOLLE TRANSITO --------------------
005200021203     FFNBTT37L  UF   E           K DISK
005300021203     FFNBTP11L  IF   E           K DISK
005400941206     F* BOLLE PARTENZA --------------------
005500941206     FFNBLT27L  UF   E           K DISK
005600170904     FFNBLT29L  UF   E           K DISK    rename(fnblt000:fnblt029)
005700170904     f                                     prefix(b29:3)
005800911014     D*
005900960523     D L6              S              3  0 DIM(30)
006000961212     D*
006100170905     D** S5F             S              8    DIM(9000)
006200170905     D** F5F             S             12    DIM(9000)
006300020218     D WPO             S              3    DIM(4)
006400020204     D CAN             S              1    DIM(200)
006500961212     D*
006600921229     D NPS             S              2  0 DIM(101)
006700921229     D RPS             S              1    DIM(101)
006800000210     D TPS             S              1    DIM(101)
006900000316     D OPS             S              3  0 DIM(101)
007000000316     D INV             S              1    DIM(101)
007100040503     D FSL             S              1    DIM(101)
007200961212     D*
007300941214     D CAT             S              1  0 DIM(20)
007400941214     D NST             S              1    DIM(20)
007500950927     D*
007600961122     D TTR             S              1    DIM(10)
007700961122     D*
007800950927     D PG2             S              5  0 DIM(20)
007900950927     D*
008000000505     D FFS             S              3    DIM(301)
008100970114     D*
008200040601     D CM5             S              1    DIM(15) CTDATA PERRCD(15)
008300980928     D*
008400111018     D** ERC             S              1  0 DIM(10)
008500111018     D** ERF             S              5    DIM(10)
008600111018     D** ERE             S              3  0 DIM(10)
008700111018     D** ERp             S              2    DIM(10)
008800981105     D K70             S              1    DIM(70)
008900981105     D MSG             S             70    DIM(3) CTDATA PERRCD(1)
009000001109     D ACG             S              3  0 DIM(50)
009100130228     d* tfp abilitati a far partire colli di altro tfp
009200130228     D xcotfp          S              3    DIM(85)
009300000516     D*-----------------------------
009400930701     D* DS PER CONFRONTO SPUNTE: BAR
009500930701     D*-----------------------------
009600930701     D                 DS
009700960524     D  BARCAN                 1      1
009800960524     D  BARLNP                 2      4  0
009900960524     D  BARLNA                 5      7  0
010000960524     D  BARNRS                 8      9  0
010100960524     D  BARNSC                10     16  0
010200960524     D  BARNPS                17     18  0
010300960524     D  BARVUC                19     25  6
010400960524     D  BARZNC                26     27  0
010500021002     D  BARFGS                28     30  0
010600960524     D  BARNPG                31     31  0
010700991223     D  BARNFV                32     37  0
010800991223     D  BARPUC                38     44  3
010900000223     D  BARDFS                45     52  0
011000070209     D  BARHMS                53     58  0
011100070209     D  BARmis                59     61  0
011200070822     D  BARimmis              45     61  0
011300070209     D  BARPRU                62     71
011400070209     D  BARTAP                72     72
011500960524     D* DS PER CONFRONTO SEGNACOLLO CHE STO CARICANTO
011600070209     D  BAR1                   1     72
011700961212     D* KEY PER CHAIN FOGLI VIAGGIO A ROTTURA
011800991223     D  BARKEY                28     37  0
011900930701     D*-----------------------------
012000930701     D* DS PER CONFRONTO SPUNTE: BRV
012100930701     D*-----------------------------
012200930701     D                 DS
012300960524     D  BRVCAN                 1      1
012400960524     D  BRVLNP                 2      4  0
012500960524     D  BRVLNA                 5      7  0
012600960524     D  BRVNRS                 8      9  0
012700960524     D  BRVNSC                10     16  0
012800960524     D  BRVNPS                17     18  0
012900960524     D  BRVVUC                19     25  6
013000960524     D  BRVZNC                26     27  0
013100021002     D  BRVFGS                28     30  0
013200960524     D  BRVNPG                31     31  0
013300991223     D  BRVNFV                32     37  0
013400991223     D  BRVPUC                38     44  3
013500000223     D  BRVDFS                45     52  0
013600070131     D  BRVHMS                53     58  0
013700070209     D  Brvmis                59     61  0
013800070822     D  Brvimmis              45     61  0
013900070209     D  BrvPRU                62     71
014000070209     D  BrvTAP                72     72
014100021014     D* DS PER CONFRONTO SEGNACOLLO CHE STO CARICANTO
014200070209     D  BRV                    1     72
014300960524     D* KEY PER CHAIN FOGLI VIAGGIO A ROTTURA
014400991223     D  BRVKEY                28     37  0
014500960527     D                 DS
014600021014     D  BR7FGS                 1      3  0
014700960527     D  BR7NPG                 4      4  0
014800991223     D  BR7NFV                 5     10  0
014900960527     D* KEY PER CHAIN FOGLI VIAGGIO A ROTTURA
015000991223     D  BR7KEY                 1     10  0
015100020205     D                 DS
015200040303     D  BR9FGS                 1      3  0
015300040303     D  BR9NPG                 4      4  0
015400040303     D  BR9NFV                 5     10  0
015500040303     D  BR9KEY                 1     10  0
015600941213     D WLBDAT          DS
015700941213     D  G02DAT                 1      8  0
015800941213     D  G02INV                 9     16  0
015900941213     D  G02ERR                17     17
016000941213     D  G02TGI                18     22  0
016100000223     D WGIDAT          DS
016200000223     D  GI8DAT                 1      8  0
016300000223     D  GI8INV                 9     16  0
016400000223     D  GI8TGI                17     21  0
016500950927     D*LINEE   DI ARRIVO
016600950927     D                 DS
016700950927     D  FVALNA                 1      3  0
016800950927     D  FVAFFV                 4    243
016900950927     D  FVAFF2               244    450
017000000505     D  FWAFF3               451    690
017100000505     D  FWAFF4               691    900
017200000505     D  VUOTO                901    903
017300000505     D  FFV                    1    903
017400000505     D                                     DIM(301)
017500950927     D*FILIALE DI SCARICO
017600950927     D                 DS
017700950927     D  LNA2                   1      3  0
017800950927     D  FVAFLP                 4    243
017900950927     D  FVAFL2               244    450
018000000505     D  FWAFL3               451    690
018100000505     D  FWAFL4               691    900
018200000505     D  VUOTO2               901    903
018300000505     D  FLP                    1    903
018400000505     D                                     DIM(301)
018500980115     D* LINEE DI ARRIVO  PER FNFGV00F
018600980115     D                 DS
018700980115     D  FGVLNA                 1      3  0
018800980115     D  FGVFFV                 4    243
018900980115     D  FGVFF2               244    450
019000000505     D  FGWFF3               451    690
019100000505     D  FGWFF4               691    900
019200000505     D  VUOTO3               901    903
019300000505     D  FFG                    1    903
019400000505     D                                     DIM(301)
019500980115      *
019600161007     D**               DS
019700161007     D**GCPAGC                 1      4  0
019800161007     D**GCPMGC                 5      8  0
019900161007     D**GCPDGC                 1      8  0
020000970217     D                 DS
020100980128     D  ARTAAS                 1      4  0
020200980128     D  ARTLNP                 5      7  0
020300980128     D  ARTNRS                 8      9  0
020400980128     D  ARTNSP                10     16  0
020500980128     D  ARTTBO                17     17
020600980128     D  ARTSPE                 1     17
020700970217     D                 DS
020800970217     D  BLTAAS                 1      4  0
020900970217     D  BLTLNP                 5      7  0
021000970217     D  BLTNRS                 8      9  0
021100970217     D  BLTNSP                10     16  0
021200970217     D  BLTTBO                17     17
021300970217     D  BLTSPE                 1     17
021400970218     D                 DS
021500970218     D  BTTAAS                 1      4  0
021600970218     D  BTTLNP                 5      7  0
021700970218     D  BTTNRS                 8      9  0
021800970218     D  BTTNSP                10     16  0
021900980120     D  BTTTBO                17     17
022000970218     D  BTTSPE                 1     17
022100970217     D                 DS
022200970217     D  SAVAAS                 1      4  0
022300970217     D  SAVLNP                 5      7  0
022400970217     D  SAVNRS                 8      9  0
022500970217     D  SAVNSP                10     16  0
022600970508     D  SAVBOL                 1     16  0 INZ
022700970508     D  SAVTBO                17     17    INZ
022800970217     D  SAVSPE                 1     17
022900980120     D                 DS
023000980120     D  WDAAS                  1      4  0
023100980120     D  WDLNP                  5      7  0
023200980120     D  WDNRS                  8      9  0
023300980120     D  WDNSP                 10     16  0
023400980120     D  WDSPE                  1     16
023500021014     D                 DS
023600021014     D  WCED                   1      6
023700021014     D  WTER                   7      9
023800021014     D  WVUOTO                10     10
023900021014     D  WPROF                  1     10
024000020201     D                 DS
024100020201     D  BLPAAS                 1      4  0
024200020201     D  BLPMGS                 5      8  0
024300020201     D  BLPDSP                 1      8  0
024400020201     D                 DS
024500020201     D  ARBAAS                 1      4  0
024600020201     D  ARBMGS                 5      8  0
024700020201     D  ARBDSP                 1      8  0
024800020201     D                 DS
024900020201     D  BTPAAS                 1      4  0
025000020201     D  BTPMGS                 5      8  0
025100020201     D  BTPDSP                 1      8  0
025200930615     D KPJBA         E DS
025300110103      *
025400110103      * - Parametri per richiamo al pgm di controllo profilo utenti
025500110103     d TIBS34ds      e ds
025600110103      * - Ds di riferimento al file esterno AzUte00F
025700110103     d AZUTEds       e ds                  ExtName(AzUte00F)
025800110103      * - Ds per dati organigramma
025900110103     d dDatiUte      e ds
026000110103      *
026100961122     D DSTV          E DS
026200990121     D DCCH            DS
026300990121     D  §CHCRA                51     51
026400990121     D DTAD            DS
026500990121     D  §TARGR                26     26
026600990126     D  §TARIT                82     82
026700961122     D DS7C          E DS
026800921020     D DS7N          E DS
026900040430     D DS7O          E DS
027000921229     D DS7J          E DS
027100970522     D DS7A2         E DS
027200030723     D DVPO          E DS
027300000928     D DS5A          E DS
027400170505     D**** DS5APT        E DS
027500000614     D OG143         E DS
027600941229     D* DS PER TRUL06R - CARICAMENTO £X
027700941229     D DSUL06        E DS                  EXTNAME(TRUL06DS)
027800941229     D  LIN                    1     90  0
027900941229     D                                     DIM(30)
028000000705     D DSLV53        E DS                  EXTNAME(FNLV53DS)
028100020213     D DSLV55        E DS                  EXTNAME(FNLV55DS)
028200970814     D DSLR33        E DS                  EXTNAME(FNLR33DS)
028300021014     D DSBS55        E DS                  EXTNAME(TIBS55DS)
028400990120     D DSBS02        E DS                  EXTNAME(TIBS02DS)
028500161007     d FNLGI1DS      E DS
028600170905     d trulnrsds     E DS
028700170905     d xgiolavds     E DS
028800170125     d
028900170905     d** TRULVPODS     e ds
029000170905     D**  skFilOk               16    765    dim(250)
029100170125     d
029200070212     d
029300070212     d* DS passaggio dati
029400070207     D fnls45ds      E DS
029500160726     d
029600160726     D DAGBFLO       E DS
029700091209     D DAR5          E DS
029800091209     D DAR5GEN       E DS
029900990518     D FNANM2          DS
030000960416     D  ANMNR2               397    400B 0
030100070212     D FIBAR           DS
030200030226     D  BA1_file              83     92
030300030226     D  BA1_libr              93    102
030400150622     d                 ds
030500150622     d bsplin
030600150622     d sbsp                           3  0 dim(30) overlay(bsplin)
030700040811
030800040811      * ds per il controllo della presenza di CA per la spedizione richiesta
030900040811     d FIDN12DS      e ds
031000040811     d  skKey                 26    305    dim(20)
031100040811     d  skAnn                306    325    dim(20)
031200040811     d  skDca                326    485  0 dim(20)
031300040811     d  skFca                486    545  0 dim(20)
031400040811     d  skDch                546    705  0 dim(20)
031500040811     d  skCch                706    745    dim(20)
031600040811     d*
031700070212     D DSRECBRV      E DS                  extname(fnbrv00f)  prefix(D_)
031800161007     D tigcp00ds     E DS                  extname(tigcp00f)
031900080317     D WTSU            s              1  0
032000160421     D wznc            s              2
032100140613     D kdel_npg        s                   like(brvnpg)
032200170509     D boldbr          s                   like(arbdbr)
032300130228     D KCOD            s                   like(tbecod)
032400130228     D Kke1            s                   like(tbeke1)
032500130228     D Kke2            s                   like(tbeke2)
032600130228     D SAWBRV          s                   like(brv)
032700080317     D WPES            s                   like(brvpes)
032800070208     D BPES            s                   like(brvpes)
032900070208     D BTFP            s                   like(brvFLE)
033000050322     D savesart        s                   like(wesart)
033100041105     D wprimotbo       s                   like(savtbo)
033200070221     D WSAVFLG         s                   like(dls45FLG)
033300140613     D wdfs            s                   like(brvdfs)
033400140616     D qd_nfv          s                   like(brvnfv)
033500140616     D qd_npg          s                   like(brvnpg)
033600140616     D qd_dfs          s                   like(brvdfs)
033700140616     D qd_hms          s                   like(brvhms)
033800140616     D bardcs          s                   like(brvdcs)
033900140616     D barhcs          s                   like(brvhcs)
034000140616     D barfcf          s                   like(fvvfcf)
034100140616     D brvfcf          s                   like(fvvfcf)
034200150622     d w_clibo         s                   Like(blpccm)
034300160726     d welasta_p       s              1
034400160726     d wBspTp          s              1
034500150622     d wBspspu         s              1
034600140618     d A_imm           s             14  0 inz
034700140618     d B_imm           s             14  0 inz
034800140618     d brv_imm         s             14  0 inz
034900140618     d brv_sca         s             14  0 inz
035000140618     d DatReale        s              8  0 inz
035100140618     d oraReale        s              4  0 inz
035200130228     d Indx            s              3  0
035300130228     d SAVfnrs         s              5  0 inz
035400130228     d Wflsnrs         s              5  0
035500041013     D WCONTRDFV       s              1    inz('N')
035600041105     D WAGBART         s              1    inz(' ')
035700070906     D WNPS            s              2    inz
035800080630     D WOK             s              1    inz
035900170505     D W138            s              1    inz
036000080630     D Secondi         s              9  0 inz
036100040621     D ktrd            s                   like(ar5trd)  inz('GEN')
036200080630     D Timeold         s               t   timfmt(*iso)
036300080630     D Timenew         s               t   timfmt(*iso)
036400980928     D* DEFINIZIONE COSTANTI
036500980928     D CUTI            C                   CONST('EDP*      ')
036600960423     C*---------------------------------------------------------------*
036700921014     C* RIEPILOGO INDICATORI
036800970115     C*---------------------------------------------------------------*
036900921014     C* 02    - RICHIAMATO
037000930914     C* 03    - POSSO RIABILITARE LA BOLLA
037100921021     C* 05    - MODIFICA DEL SOLO NUMERO DI FOGLIO
037200000505     C* 28/29 - DI COMODO
037300070212     C* 30    - LETTURA FIBAR00F
037400070131     C* 31    - CHAIN FNBRV
037500950927     C* 32    - LETTURA FILE FNBLT FNART FNARB FNBTT
037600950927     C* 33/34 - DI COMODO
037700921014     C* 37    - NON ESISTE BOLLA TRANSITO
037800070131     C* 38    - ERRORE SU WRITE FNBRV e su update
037900921105     C* 39    - RECORD ALLOCATO
038000921014     C* 40    - SE NON RECORD ALLOCATI O ALLOCATI PER 3 VOLTE FINE PGM
038100950926     C* 41    - COMODO
038200030528     C* 45    - Errore su write fnagb
038300040310     C* 46    - lookuip su schiera NTS
038400970204     C* 96    - ESISTE GIA' A MENO L'ANOMALIA CHE DEVO CREARE
038500970204     C* 97    - USATO PER LA CHIUSURA ANOMALIE CON 145 E 146
038600080325     C* 98    - FIBAR allocato --> esco dal pgm
038700080325     C* 99    - DI COMODO
038800921014     C*---------------------------------------------------------------*
038900911016     C     *ENTRY        PLIST
039000070212     C                   PARM                    fnls45ds
039100070208     C                   PARM                    dsrecbrv
039200941214     C*---------------------------------------------------------------*
039300070221     c* tipo di richiamo in base a dls45FLG
039400070208     C*---------------------------------------------------------------*
039500930521     C* FLG = '2' MODIFICATO SOLO NUMERO FOGLIO
039600160624     c
039700160624     c* Eliminato tipo richiamo "6" in quanto eliminate anomalie 60 e 61
039800070221     C* FLG = '6' CREO SOLO ANOMALIA 61 (EXSR CRTA60) DA SPU DISGUIDO
039900160624     c
040000991022     C* FLG = 'D' CREO ANOMALIA DISGUIDO PER COLLO DA ME CONFERMABILE
040100930521     C* FLG = ' ' INSERIMENTO NUOVA SPUNTA O VARIATA SOLO ANOMALIA
040200930521     C* FLG = 'C' CHIUDO PGM CON LR E BASTA
040300050418     C* FLG = 'T' aggiorno bolla transito x disguido
040400070221     c*
040500070221     c* Richiami attualmente usati: '2'  ' '  'T'  'C'
040600070208     C*---------------------------------------------------------------*
040700070208     c* Eaborazioni per chiusura pgm
040800070221    1C     dls45FLG      IFEQ      'C'
040900070208     c                   EXSR      ELACHIUDI
041000040811     c*
041100930521     C                   SETON                                        LR
041200930521     C*
041300930521   X1C                   ELSE
041400941229     C*
041500941229     C* IMPOSTAZIONI INIZIALI
041600941229    2C     UNO           IFEQ      ' '
041700941229     C                   EXSR      CARTAB
041800941229    2C                   END
041900070208     c
042000070208     c* Impostazione di BPES
042100070208     c                   if        dls45pes>*zeros
042200070208     c                   movel     dls45pes      BPES
042300070208     c                   endif
042400070208     c                   if        dls45tfp>*zeros
042500070208     c                   movel     dls45tfp      BTFP
042600070208     c                   endif
042700070208     c
042800070208     c                   EXSR      IMPO
042900930521     C*
043000930521     C                   SETOFF                                       05
043100941229     C                   Z-ADD     1             B                 3 0
043200930521     C****
043300930521     C**** P G M   R I C H I A M A T O
043400930521     C****
043500070208    2C                   if        dls45ric='S'
043600070208     C                   SETON                                        02
043700070208     c
043800070208     C* nel secndo parametro passato ci sono tutti i campi della spunta
043900070208     c*  con la quale effettuare gli aggiornamenti
044000070212     C                   Z-ADD     d_brvnpg      BARNPG
044100070212     C                   Z-ADD     D_brvnfv      BARNFV
044200070212     C                   Z-ADD     d_brvlnp      BARLNP
044300070212     C                   Z-ADD     d_brvlna      BARLNA
044400070212     C                   Z-ADD     d_brvnrs      BARNRS
044500070212     C                   Z-ADD     d_brvnsc      BARNSC
044600070212     C                   MOVEL     d_brvcan      BARCAN
044700070212     C                   Z-ADD     d_brvnps      BARNPS
044800070212     C                   Z-ADD     d_brvvuc      BARVUC
044900070212     C                   Z-ADD     d_brvpuc      BARPUC
045000070212     C                   Z-ADD     d_brvdfs      BARDFS
045100070212     C                   Z-ADD     d_brvhms      BARHMS
045200070212     C                   Z-ADD     d_brvmis      BARmis
045300070212     C                   movel     d_brvtap      BARtap
045400070212     C                   Z-ADD     d_brvznc      BARZNC
045500070212     c                   movel     d_brvpru      barpru
045600070212     c                   movel     d_brvfgs      barfgs
045700101130     c                   movel     d_brvdcs      brvdcs
045800101130     c                   movel     d_brvhcs      brvhcs
045900070208     c
046000960530     C                   CLEAR                   SAVMAC
046100960529     C**
046200070208     c** 05 on - modificato solo numero di foglio viaggio
046300070221    3C     dls45FLG      IFEQ      '2'
046400921229     C                   SETON                                        05
046500020213     C                   ENDIF
046600960524     C*
046700070209     C* CONTROLLO CHE ESISTA COD.ANOMALIA NELLA TABELLA ANOMALIE
046800020204     C     BARCAN        IFNE      ' '
046900020204     C     BARCAN        ANDNE     ' '
047000020204     C     BARCAN        LOOKUP    CAN                                    34
047100020204     C  N34              CLEAR                   BARCAN
047200020204     C                   ENDIF
047300961213     C*
047400961213     C* LETTURA FOGLIO E IMPOSTAZIONE CAMPI DI COMOD
047500961213     C                   EXSR      BARFGV
047600960529     C**
047700960529     C* SE NON HO TROVATO IL FOGLIO ESCO IMPOSTANDO L'ERRORE
047800960529    3C     BARDFV        IFEQ      0
047900070906     C                   SETON                                        39
048000960529   X3C                   ELSE
048100991230     C     BARLNA        CHAIN     AZORG01L                           31
048200040303    4c                   if        *in31 or
048300040303     c                             orgfag<>'F' and orgfag<>'A'
048400040303     C                   CLEAR                   NTWBAR
048500040303   x4c                   else
048600040303     C                   MOVE      ORGDE3        OG143
048700040303     C                   MOVE      §OGNTW        NTWBAR
048800000523     C*
048900000523     C* SE DEVO CREARE ANOMALIA DI DISGUIDO, CHIAMO SOLTANTO CRTA52
049000070221    5C     dls45FLG      IFEQ      'D'
049100991022     C                   MOVEL     'D'           WTIBOL
049200000523   X5C                   ELSE
049300920424     C*
049400920424     C* SE TROVO RECORD ALLOCATI RITENTO 3 VOLTE
049500000523    6C     B             DOWLE     3
049600950707     C                   MOVE      'S'           OKAGG             1
049700911016     C                   EXSR      UPDARB
049800000523    7C     OKAGG         IFEQ      'N'
049900950707     C                   Z-ADD     4             B
050000000523   X7C                   ELSE
050100021112     C                   Z-ADD     4             B
050200000523    7C                   ENDIF
050300150824     c
050400000523    6C                   ENDDO
050500150824
050600150824     c* Colli che partono dapiù filiali: facico fare la write al
050700150824     c*  chiamante
050800150824     C                   IF        wbsptp<>' ' and wbspspu='S'
050900150824     c                             and wagpar=' '
051000150824     c                   eval      %subst(dls45flo:1:1)='B'
051100150824     c                   endif
051200150824     c
051300040303    5C                   ENDif
051400040303    4C                   ENDIF
051500960410     C*
051600960410     C* PER BOLLA ARRIVO NON LOCALE, VEDO SE CREARE ANOMALIA 50
051700001114     C*  COLLO ARRIVATO NON PARTITO
051800960529     C*  NON ELABORO SE COLLO LOCALE O SOLO DELLA LINEA DI PARTENZA
051900001115     C* (N.B. INFATTI, SE BOLLA ARRIVO LOCALE, NON C'E' TRASMISSIONE DI
052000001115     C*  F.V. E QUINDI QUESTA ROUTINE NON SAREBBE MAI IN GRADO DI
052100001115     C*  CREARE L'ANOMALIA 50  DAL MOMENTO CHE VA A CERCARE IL FOGLIO
052200001115     C*  QUESTO VALE ANCHE PER I COLLI 180 PER 89)
052300020218    5C  N05WTIBOL        IFNE      'P'
052400960530     C     WTIBOL        ANDNE     'S'
052500020214     C     WTIBOL        ANDNE     'R'
052600070221     C     dls45FLG      ANDNE     '6'
052700960410     C                   EXSR      CRTA50
052800000523    5C                   ENDIF
052900000523     C*
053000960530    3C                   ENDIF
053100911016     C                   GOTO      FINE
053200960529     C*
053300960529    2C                   ENDIF
053400930521     C****
053500930521     C**** P G M    N O N    R I C H I A M A T O
053600930521     C****
053700930521     C* APRO FILE
053800040601     c                   clear                   wconta            2 0
053900040601     c                   movel     'S'           wapri             1
054000070212     C                   OPEN(e)   FIBAR00F
054100040601     c* Attendo 5 secondi e riprovo max 9 volte poi esco con errore
054200040601     c                   dow       %error  and wconta<10
054300040601     C                   Z-ADD     15            LUNG
054400040601     C                   MOVEA(p)  CM5           COMMAN           80
054500040601     C                   CALL      'QCMDEXC'                            99
054600040601     C                   PARM                    COMMAN
054700040601     C                   PARM                    LUNG             15 5
054800040601     c
054900040601     c                   add       1             wconta
055000070212     C                   OPEN(e)   FIBAR00F
055100040601     c                   enddo
055200040601     c
055300040601     c* Se ancora errore vado a fine file
055400040601     c                   if        %error
055500040601     c                   eval      wapri='N'
055600040601     c                   goto      fine
055700040601     c                   endif
055800040601     c
055900920424     C                   SETON                                        40
056000941214     C                   Z-ADD     *ZERO         SA1KEY
056100960530     C                   CLEAR                   SAVMAC
056200070208     C                   MOVE      DLS45FLG2     FLGPRE            1
056300911014     C*
056400080325     C                   READ      FIBAR00F                             9830
056500911016     C*
056600930521    2C     *IN40         DOWEQ     '1'
056700080325    2C     *IN98         andeq     *off
056800920424     C                   SETOFF                                       40
056900930521    3C     *IN30         DOWEQ     '0'
057000080325    3C     *IN98         andeq     '0'
057100930602     C*
057200040419     C                   SETOFF                                       3839
057300930521    4C     BARLNP        IFGT      *ZERO
057400040419     C     BARLNA        ANDGT     *ZERO
057500960731     C     BARNSC        ANDGT     *ZERO
057600040224     C     BARNpg        ANDGT     *ZERO
057700070201     c* Per ora non si accettano fogli maggiori di 100.000
057800040601     C     BARNfv        ANDlT     100000
057900070212     c
058000070212     c* Ad ogni record imposto BPES, perchè per le spunte cat 1 di un
058100070212     c*  secondo livello, BPES diventa il terminal per motivi di
058200070212     c*  creazione della anoamlia 200, per cui potrebbe rimanare
058300070212     c*  sporco se dopo ci fossero spunte di altre categorie
058400070212     c                   movel     dls45pes      BPES
058500021015     C                   Z-ADD     BPES          BARFGS
058600070209     c
058700160511      * CLEARO PESO E VOLUME UNITARI SE SUPERIORI AL MAX PREVISTO
058800160511      * limiti per transpallet  (pistola 46)
058900160511      * LIMITI PER RILEVATORE PESO VOLUME PALLET (Pistola 48)
059000160511      * limite per VDL  ( tutte le altre pistole)
059100160511     c                   select
059200160511     c                   when      barnps= 48
059300030723     C     BARPUC        IFGT      §vpoMPP
059400030723     C                   CLEAR                   BARPUC
059500030723     C                   ENDIF
059600030723     C     BARVUC        IFGT      §vpoMVP
059700030723     C                   CLEAR                   BARVUC
059800030723     C                   ENDIF
059900160511     c
060000160511     c                   when      barnps= 46
060100160511     C     BARPUC        IFGT      §vpoMPT
060200160511     C                   CLEAR                   BARPUC
060300160511     C                   ENDIF
060400160511     c                   other
060500160511     C     BARPUC        IFGT      §vpoMPV
060600160511     C                   CLEAR                   BARPUC
060700160511     C                   ENDIF
060800160511     C     BARVUC        IFGT      §vpoMVV
060900160511     C                   CLEAR                   BARVUC
061000160511     C                   ENDIF
061100160511     C                   Endsl
061200160511     c
061300040805      * CLEARO PESO E VOLUME UNITARI SE INFERIORI AL MINIMO PREVISTO
061400040805      * I LIMITI VALGONO PER TUTTI I TIPI DI IMPIANTO
061500040805     C     BARPUC        IFLT      §vpoSPV
061600040805     C                   CLEAR                   BARPUC
061700040805     C                   ENDIF
061800040805     C     BARVUC        IFLT      §vpoSVV
061900040805     C                   CLEAR                   BARVUC
062000040805     C                   ENDIF
062100020204     C* CONTROLLO CHE ESISTA COD,ANOMALIA NELLA TABELLA ANOMALIE
062200020204     C     BARCAN        IFNE      ' '
062300020204     C     BARCAN        ANDNE     ' '
062400020204     C     BARCAN        LOOKUP    CAN                                    34
062500020204     C  N34              CLEAR                   BARCAN
062600020204     C                   ENDIF
062700040419     c
062800070906     c* Non carico se LNP inesistente
062900040419     C     BARLNP        CHAIN     AZORG01L                           31
063000040419     c* Non carico se LNA inesistente
063100070906   4bC     *IN31         IFEQ      *Off
063200040419     c     orgfag        andeq     'A'
063300040419     C     *IN31         oreq      *Off
063400040419     c     orgfag        andeq     'F'
063500980212     C*
063600991230     C     BARLNA        CHAIN     AZORG01L                           31
063700040419     c* non carico se LNA inesistente
063800040303    5C     *IN31         IFEQ      *ON
063900040224     c     orgfag        orne      'A'
064000040224     c     orgfag        andne     'F'
064100020212     C                   CLEAR                   NTWBAR
064200040303   x5C                   ELSE
064300020212     C                   MOVEL     ORGDE3        OG143
064400020212     C                   MOVEL     §OGNTW        NTWBAR
064500070912     c
064600070912     C* LETTURA FOGLIO DELLA SPUNTA E IMPOSTAZIONE CAMPI DI COMOD
064700070912     C                   EXSR      BARFGV
064800070912     c
064900070912     c* Non carico se numero foglio inesistente
065000070912   5ac                   if        bardfv>0
065100140616     c                   clear                   amb_QD            1
065200140613     c                   clear                   A_imm
065300140613     c                   clear                   B_imm
065400140613     c*
065500140613     c* se IMA e spunta consegna PIu' RECENTE --> cancello senza caricare
065600140613     c                   if        barnpg=3 and barspg='A'
065700140616     c                   eval      kdel_npg=4
065800140613     c                   exsr      Cer_cons
065900140613     c                   endif
066000140616     c                   if        barnpg=4
066100140616     c                   eval      kdel_npg=3
066200140616     c                   exsr      Cer_cons
066300140616     c                   endif
066400140613
066500140616   5cc                   if        amb_QD<>'N'
066600040303     c**
066700040303     C* VERIFICO IL FLAG NELLA DS7N
066800040303     C                   Z-ADD     1             WA                3 0
066900040310     C     BARNPG        LOOKUP    CAT(WA)                                46
067000040310    6C     *IN46         IFEQ      *OFF
067100040303     C     NST(WA)       ORNE      '1'
067200040303     c* Se tengo tutte le spunte, verifico spunta doppia per lo stesso NFV
067300070131     C     KBAR1         CHAIN     FNBRV01L                           31
067400040303   x6c                   else
067500911016     C*
067600040303     C* spunta doppia per stessa categoria e p.o. gestione
067700140613     C     KBAR2         CHAIN     FNBRV05L                           31
067800040303    6c                   endif
067900941229     C*
068000040303     C* Aggiorno bolle e spunta se:
068100040303     C* - spunta nuova (31 on)
068200040303     C* - spunta già esistente ma dati del record diversi(BAR1<>BRV)
068300040303     C* - spunta già presente ma nel giro prec.trovati record allocati(B>1)
068400070212     c* - trovati errori  (BARERR=E)
068500070912   5bC     *IN31         IFEQ      *ON
068600941229     C     BAR1          ORNE      BRV
068700001114     C     B             ORGT      1
068800070212     C     BARERR        OReq      'E'
068900040303     c**
069000040303     C* OPERAZIONI PER SPUNTA GIA' PRESENTE e da tenerne una
069100040303    6C     *IN31         IFEQ      *OFF
069200040303     C     nst(WA)       ANDEQ     '1'
069300010608     C     BAR1          ANDNE     BRV
069400010608     C                   EXSR      SRBRVE
069500040303    6C                   ENDIF
069600070710     c**
069700070710     C* OPERAZIONI PER SPUNTA GIA' PRESENTE e da tenerne una
069800070710    6C     *IN31         IFEQ      *OFF
069900070710     C     nst(WA)       ANDne     '1'
070000070710     C     BAR1          ANDNE     BRV
070100070710     c* la precedente spunta non ha l'anomalia
070200070710     c     brvcan        andeq     ' '
070300070710     c* nell'attuale è stata inserita
070400070710     c     barcan        andne     ' '
070500070822     c*  Carico solo se data/ora Immissione spunta sono < della precedente
070600070822     c     barimmis      andgt     brvimmis
070700070710     C                   EXSR      SRBRVE
070800070710    6C                   ENDIF
070900040303     C* RIEMPI I CAMPI
071000040303     C                   EXSR      RIEMPI
071100040303     C**
071200040303     C**  A G G I O R N A M E N T I
071300961213     C                   MOVE      'S'           OKAGG             1
071400911112     C                   EXSR      UPDARB
071500911014     C**
071600920424     C**  AGGIORNO SPUNTA
071700950707     C*   Non l'aggiorno se spunta partenza manuale e di collo già
071800950707     C*   consegnato
071900040303    6C     OKAGG         IFEQ      'S'
072000040303     C*
072100960521     C* SE NO  LA DEVO TRASMETTERE LA FLAGGO
072200961213    7C     WNOTRA        IFEQ      'S'
072300960521     C                   MOVEL     'N'           BRVFTR
072400960521     C                   MOVEL     *HIVAL        BRVDTR
072500991223     C                   MOVEL     *HIVAL        BRVHTR
072600960521     C                   CLEAR                   WNOTRA
072700961213    7C                   ENDIF
072800980115     C* SE SPUNTA PARTENZA RELATIVA AD UN FOGLIO VIAGGIO SU CUI NON E'
072900980115     C* PIU' PRESENTE LA LNA DEL COLLO -> FLAGGO LA SPUNTA
073000980205     C* (RIENTRA IN QUESTO CASO ANCHE UNA SPUNTA CON COD.ANOMALIA "E"
073100980205     C* CIOE' UNA SPUNTA LA CUI LINEA NON ERA PRESENTE SUL FV GIA' NEL
073200980205     C* MOMENTO IN CUI E' STATA FATTA)
073300040303    7C     BARNPG        IFEQ      1
073400980115     C                   MOVE      BARLNA        W003A             3
073500980313     C     W003A         LOOKUP    FFS                                    33
073600980115     C  N33              MOVEL     'L'           BRVFTR
073700980115     C  N33              MOVEL     *HIVAL        BRVDTR
073800991223     C  N33              MOVEL     *HIVAL        BRVHTR
073900980210     C   33BRVCAN        IFEQ      'E'
074000980210     C                   CLEAR                   BRVCAN
074100980210     C                   END
074200041020    7C                   ENDIF
074300041020     c
074400041020     c* Imposto brvatr='D' per disguido
074500041020     c                   if        wtibol='D'
074600041020     c                   eval      brvatr='D'
074700041020     c                   else
074800041020     c                   clear                   brvatr
074900041020    7C                   ENDIF
075000040303     c**
075100070131     C* AGGIORNO LF FNBRV01L
075200040310    7C  N31*IN46         IFEQ      *OFF
075300040303     C     NST(WA)       ORNE      '1'
075400070131     C                   UPDATE    FNBRV000
075500040303     C                   ELSE
075600140613     C* AGGIORNO LF FNBRV05L
075700140613     C                   UPDATE    FNBRV005
075800040303    7C                   END
075900010607     C*
076000070131     C   31              WRITE     FNBRV000                             38
076100140613     c
076200140613     c* Se previsto cancello la spunta "doppia" tra 3 e 4
076300140616    7c                   if        amb_QD='S'
076400140616     c                   exsr      Canc_QD
076500140616    7c                   endif
076600150824     c**
076700150824     c*  COLLO SPUNTABILE DA PIÙ FILIALI
076800150824     c**
076900150824     c** elaboro per "K-cli" con BPES <> LNPD ( in qeusto caso ho pulito
077000150824     c*                                        AGGPAR)
077100150824     c*  oppure  per "L-linea" se TFPLNP <>TFPPES
077200150824     c* lo faccio dopo la write di BRV perchè fnlv52r deve leggere la spunta per
077300150824     c*  determinare la nuova lnp della bolla
077400150824     C                   IF        wbsptp<>' ' and wbspspu='S'
077500150824     c                             and wagpar=' '
077600150824     c                   exsr      DA_BSP
077700150824     c                   endif
077800140613     c
077900040303   X6C                   ELSE
078000040303     C*
078100040310    7C  N31*IN46         IFEQ      *OFF
078200040303     C     NST(WA)       ORNE      '1'
078300070131     C                   UNLOCK    FNBRV01L
078400040303     C                   ELSE
078500140613     C                   UNLOCK    FNBRV05L
078600040303    7C                   END
078700040303    6C                   END
078800950927     C* PER BOLLA ARRIVO NON LOCALE, VEDO SE CREARE ANOMALIA 50
078900950927     C*  COLLO ARRIVATO NON PARTITO
079000960207     C*  NON ELABORO SE COLLO LOCALE O SOLO DELLA LINEA DI PARTENZA
079100020214    6C  N05WTIBOL        IFEQ      'D'
079200020214     C     WTIBOL        OREQ      'A'
079300020214     C     WTIBOL        OREQ      'T'
079400020218     C     WTIBOL        OREQ      'L'
079500950927     C                   EXSR      CRTA50
079600950927    6C                   ENDIF
079700950927     C*
079800070912   5bC                   ELSE
079900040303     C*
080000040310    6C  N31*IN46         IFEQ      *OFF
080100040303     C     NST(WA)       ORNE      '1'
080200070131     C                   UNLOCK    FNBRV01L
080300040303     C                   ELSE
080400140613     C                   UNLOCK    FNBRV05L
080500040303    6C                   END
080600070912   5bC                   END
080700140613   5cC                   END
080800110103     c*
080900070912   5ac                   else
081000070912     c* Scrivo la spunta errata in fibar00e
081100070912     c                   movel     bpes          barpes
081200070912     c                   movel     wdate1        bardim
081300070912     c                   write     barcoerr                             99
081400070912   5aC                   END
081500040303    5C                   END
081600070906   4bC                   END
081700040419    4C                   END
081800911112     C*
081900900619     C* CANCELLO RECORD LETTO
082000001114     C* Se spunta imi e non ho aggiornato blt perchè inesistente o se
082100021203     C* non ho aggiornato blp perchè record allocato la spunta non viene
082200001114     C* cancellata.
082300021107     c                   if        not *in38 and not *in39
082400021107     C                   DELETE    BARCO
082500021107     c                   else
082600021203     c* Se ci fosse errore ma passati 4 giorni dalla data di immissione
082700021203     c*  spunte, deleto lo stesso
082800021203     c                   if        brvdfs<=wpul050
082900021203     c                   DELETE    BARCO
083000021203     c                   else
083100070212     c                   eval      barerr='E'
083200021107     c                   update    BARCO
083300021107     c                   endif
083400021203     c                   endif
083500900618     C*
083600080325     C                   READ      FIBAR00F                             9830
083700930521    3C                   END
083800021112     C** 40              ADD       1             B
083900021112     C   40              z-add     4             B
084000920424     C* DOPO 3 VOLTE ESCO
084100930521    3C   40B             IFGT      3
084200920424     C                   SETOFF                                       40
084300930521   X3C                   ELSE
084400070212     C                   CLOSE     FIBAR00F
084500070212     C                   OPEN      FIBAR00F
084600070212     C                   READ      FIBAR00F                               30
084700930521    3C                   END
084800930521    2C                   END
084900970224     C* ELABORO L'ULTIMO DATO MEMORIZZATO
085000970224     C                   EXSR      WRTAGB
085100911016     C*
085200911016     C     FINE          TAG
085300930521     C** 02 OFF - PROGRAMMA NON RICHIAMATO
085400930521     C**
085500950926     C** LANCIO PGM DI AGGIORNAMENTO BOLLE
085600961212    2C     *IN02         IFEQ      *OFF
085700970814     C**
085800970814     C                   CLEAR                   DSLV55
085900970814     C                   MOVEL     'C'           D55TLA
086000020213     C                   EXSR      FNLV55
086100000705     C**
086200000705     C                   CLEAR                   DSLV53
086300000705     C                   MOVEL     'C'           D53TLA
086400000705     C                   CALL      'FNLV53R'
086500000705     C                   PARM                    DSLV53
086600930521     C*
086700040601     c                   if        wapri='S'
086800070212     C                   CLOSE     FIBAR00F
086900040601     c                   endif
087000930521     C*
087100981105     C* SE HO ALEMNO UN FOGLIO SCRITTO--> TRASMETTO
087200111018     C***  ERF(1)        IFNE      *BLANKS
087300111018     C***                EXSR      TRASMI
087400111018     C***                ENDIF
087500040811     c*
087600040811     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
087700040811     c                   clear                   fidn12ds
087800040811     c                   eval      i12tla = 'C'
087900040811     c                   call      'FIDN12R'
088000040811     c                   parm                    fidn12ds
088100040811     c*
088200930521     C                   SETON                                        LR
088300930521     C*
088400930521   X2C                   ELSE
088500930521     C**
088600930521     C** 02 ON  - PROGRAMMA RICHIAMATO
088700930521     C**
088800921020     C* SE NON HO AGGIORNATO IMPOSTO LA "E" DI ERRORE
088900960521     C     *IN39         IFEQ      *ON
089000070221     C                   MOVEL     'E'           dls45FLG
089100960521     C                   ELSE
089200960521     C*
089300950926     C* SE AGGIORNATO FACCIO SOTTOMETTERE IL PGM DI AGGIORNAMENTO BOLLE
089400070221     C                   MOVEL     'A'           dls45FLG
089500950707     C* Se non ho aggiornato perchè spunta partenza manuale di collo
089600950707     C* consegnato imposto "N" di aggiornamento non effettuato
089700960521     C     OKAGG         IFEQ      'N'
089800070221     C                   MOVE      'N'           dls45FLG
089900950707     C                   END
090000960521     C                   ENDIF
090100070208     C                   MOVE      DLS45FLG2     FLGPRE            1
090200930521     C*
090300930521     C                   RETURN
090400930521    2C                   END
090500930521    1C                   END
090600941214     C**************************************************************************
090700941214     C* RILEVO IL FOGLIO VIAGGIO PARTENZE/ARRIVI
090800941214     C**************************************************************************
090900941214     C     RILFOG        BEGSR
091000941214     C*
091100941214     C                   Z-ADD     *ZERO         WDFV
091200941229     C                   CLEAR                   WSPG
091300021014     C                   Z-ADD     0             WFLE
091400961122     C                   CLEAR                   WTTR
091500980115     C                   CLEAR                   FFG
091600140613     C                   CLEAR                   WFCF
091700021024     c*
091800021024     c* Prendo il terminal di partenza del p.o. in gestione
091900070212    1c                   if        savfgs<>wfgs
092000021024     C                   CLEAR                   DSLV55
092100021024     C                   MOVEL     'P'           D55TPT
092200021024     C                   MOVEL     WFGS          D55LIN
092300021024     C                   MOVEL     WDATE1        D55DRF
092400021024     C                   CALL      'FNLV55R'
092500021024     C                   PARM                    DSLV55
092600021024     C**
092700070212    2C     D55ERR        IFne      ' '
092800021024     C                   MOVE      wfgs          tfpfgs
092900021024     c                   else
093000021024     C                   MOVE      d55tfp        tfpfgs
093100070212    2c                   endif
093200021024     c                   eval      savfgs=wfgs
093300070212    1c                   endif
093400941214     C*
093500941214     C* ARRIVI O PARTENZE ?
093600970114    1C     WNPG          IFEQ      1
093700941214     C*
093800021014     C     KFGV1         CHAIN     FNFGV01L                           99
093900000505     C*
094000070209    2C     *IN99         IFEQ      *OFF
094100021014     C     KFGV1         CHAIN     FNFGW01L                           28
094200070209    3C     *IN28         IFEQ      *ON
094300000505     C     FGWATB        ORNE      ' '
094400000505     C                   CLEAR                   FGWFF3
094500000505     C                   CLEAR                   FGWFF4
094600000505     C                   CLEAR                   FGWFL3
094700000505     C                   CLEAR                   FGWFL4
094800070209    3C                   ENDIF
094900980928     C                   Z-ADD     FGVDFV        WDFV
095000021024     C                   Z-ADD     FGVLNP        WFgs
095100021024     C                   Z-ADD     FGVLNP        WFLE
095200980928     C                   MOVEL     FGVTTR        WTTR
095300140613     C                   MOVEL     FGVFCF        WFCF
095400980928     C**
095500070209   X2C                   ELSE
095600960925     C     KFVA1         CHAIN     FNFVA01L                           99
095700970114    3C     *IN99         IFEQ      *OFF
095800970114     C                   Z-ADD     FVADFV        WDFV
095900021014     C                   Z-ADD     FVALNP        WFLE
096000970114     C                   MOVEL     FVATTR        WTTR
096100140613     C                   MOVEL     'S'           WFCF
096200111018   x3C**                 ELSE
096300110103     c*
096400070209     C* Se non trovato invio msg di errore
096500111018     C**                 EXSR      INVMSG
096600070212    3C                   ENDIF
096700070212    2C                   ENDIF
096800941214     C*
096900970114   X1C                   ELSE
097000941214     C*
097100021014     C* ACCEDO DIRETTAMENTE AL FOGLIO CON P.O. GESTIONE
097200021014     C* SE FOGLIO NON TROVATO CERCO CON SIMFEL- PUÒ ESSERE SOLO UN
097300070212     C*  FOGLIO IMP SOLO SE RICHIAMO PER DATI DI FIBAR!!
097400021014     C     KFVV1         CHAIN     FNFVV01L                           99
097500070209    2C     *IN99         IFEQ      *ON
097600021014     C     WNPG          ANDEQ     3
097700021014     C     RICBAR        ANDEQ     'S'
097800021014     C                   Z-ADD     SIMFEL        WFGS
097900021014     C     KFVV1         CHAIN     FNFVV01L                           99
098000021014     C  N99FVVSPG        IFNE      'P'
098100021014     C                   SETON                                        99
098200021014     C                   ENDIF
098300070209    2C                   ENDIF
098400021014     C*
098500070209    2C     *IN99         IFEQ      *OFF
098600980928     C                   Z-ADD     FVVDFV        WDFV
098700980928     C                   MOVEL     FVVSPG        WSPG
098800021014     C                   MOVEL     FVVFGS        WFGS
098900140613     C                   MOVEL     FVVFCF        WFCF
099000111018     C**                 ELSE
099100111018     C**                 EXSR      INVMSG
099200070209    2C                   ENDIF
099300941214     C*
099400970114    1C                   ENDIF
099500021014     C**
099600021024     C* SE WFLE=0 prendo TERMINAL DI PARTENZA di fgs A UDATE
099700070223     c*   di fatto si tratta del TFP di BPES poteva che può differire
099800070223     c*   dal FGS solo in caso di spunte ion procedura arrivo per p.o.
099900070223     c*   con terminal di partenza <> dal terminal arrivo
100000021014    1C     WFLE          IFEQ      0
100100021024     C                   MOVE      tfpfgs        WFLE
100200021014    1C                   END
100300941214     C*
100400941214     C                   ENDSR
100500020213     C**************************************************************************
100600020213     C*    IMPOSTAZIONI DI ALCUNI CAMPI DI PASSAGGIO SE VUOTI
100700020213     C**************************************************************************
100800020213     C     IMPO          BEGSR
100900150824     c                   eval      %subst(dls45flo:1:1)=' '
101000110103     c*
101100070208     C* BPES--> SE NON passato    E' SIMFEL
101200020213     C     BPES          IFEQ      0
101300020213     C                   Z-ADD     SIMFEL        BPES
101400070208     c                   if        btfp=0
101500070208     C                   Z-ADD     SIMFEL        BTFP
101600020213     C                   ENDIF
101700070208     C                   ENDIF
101800020213     C**
101900070208     C* BTFP--> SE NON passato ma c'è BPES, è terminal partenza di BPES
102000070208     c                   if        BTFP=0
102100020213     C                   CLEAR                   DSLV55
102200020213     C                   MOVEL     'P'           D55TPT
102300020213     C                   Z-ADD     BPES          D55LIN
102400020213     C                   Z-ADD     WDATE1        D55DRF
102500020213     C                   EXSR      FNLV55
102600110103     c*
102700020213     C                   Z-ADD     D55TFP        BTFP
102800020213     C                   ENDIF
102900110103     c*
103000070209     c* Se BPES non passato, reimposto anche il campo di input
103100070212     c                   if        DLS45PES<=*zeros
103200070209     c                   movel     bpes          dls45pes
103300070209     c                   endif
103400110103     c*
103500020213     C                   ENDSR
103600961213     C**************************************************************************
103700961213     C*    RIEMPI I CAMPI DEL RECORD DA SCRIVERE
103800961213     C**************************************************************************
103900961213     C     RIEMPI        BEGSR
104000110103     c*
104100080317     C* CONTROLLO SE E' PISTOLA MANUALE
104200080317     C                   EXSR      PISMAN
104300080317     c
104400961213     C* SE NON C'E' AGGIUNGO ALTRIMENTI AGGIORNO
104500080317    1C     WBRVE         IFEQ      'S'
104600070131     C* Se routine richiamata per scrittura di FNBRVe devo sempre clear
104700010608     c* are brvatr
104800010608     C                   CLEAR                   BRVATR
104900070201     C                   CLEAR                   BRVtsu
105000080317   x1C                   ELSE
105100961213     C   31              CLEAR                   BRVATR
105200070201     C   31              CLEAR                   BRVtsu
105300110103     c*
105400080317     c** Conteggio le spunte risparate + volte
105500080317     c** ignoro tutte le spunte manuali
105600100709     c** ignoro se spunta con errore (già caricata)
105700080422    1c                   if        not *in31  and (rps(y)<>'M' or
105800080422     c                             rps(y)='M' and tps(y)='C')
105900100709     c                             and barerr<>'E'
106000110103     c*
106100080317    2c                   if        nst(WA)<>'1'  or
106200080317     c* Se spunta unica  ma stesso numero foglio --> conto doppio
106300080317     c                             (nst(WA)= '1' and brvnfv=barnfv)
106400110103     c*
106500080630     c* Non considero doppia nemmeno se spunta cat 3 o 4 pistola 49
106600080630     c*  sparate entro i 2 sec.
106700080630     c                   eval      wok='S'
106800080630    3c                   if        (barnpg=3 or barnpg=4) and barnps=49
106900080630     c                             and brvnps=49 and brvdfs=bardfs
107000080630     c                   move      barhms        Timenew
107100080630     c                   move      brvhms        Timeold
107200080630     c     Timenew       subdur    Timeold       secondi:*s
107300080701     c                   mllzo     1             secondi
107400080630    4c                   if        secondi<=§vposecdop
107500080630     c                   eval      wok='N'
107600080630    4c                   endif
107700080630    3c                   endif
107800110103     c*
107900080630   2AC                   IF        WOK='S'
108000080317    3c                   if        brvtsu=' '
108100080317     c                   eval      brvtsu='1'
108200080317   x3c                   else
108300080317     c                   movel     brvtsu        wtsu              1 0
108400080317    4c                   if        wtsu<9
108500080317     c                   eval      wtsu=wtsu+1
108600080317     c                   movel     wtsu          brvtsu
108700080317    4c                   endif
108800110103     c*
108900080317    3c                   endif
109000080630   2Ac                   endif
109100110103     c*
109200080317   x2c                   else
109300080317     c* Prendo data foglio
109400080317    3C     BRvKEY        IFNE      SA1KEY
109500080317     C                   MOVEL     BRvKEY        SA1KEY
109600080317     C                   Z-ADD     BRvNPG        WNPG
109700080317     C                   Z-ADD     BRvNFV        WNFV
109800080317     C                   Z-ADD     BRvFGS        WFGS
109900080317     C                   movel     BRvnps        Wnps
110000080317     C                   EXSR      RILFOG
110100080317     c                   Z-ADD     WDFV          BRVDFV
110200140616     c                   movel     WFCF          BRVFCF
110300140616     c                   movel     Wspg          BRVspg
110400080317    3c                   endif
110500110103     c*
110600080317     c* Se spunta unica, foglio diverso ma stessa data -->
110700080317     c*                         mantengo TSU
110800080317     c*                                    data diversa --> lo pulisco
110900080317    3c                   if        bardfv<>brvdfv
111000080317     c                   clear                   brvtsu
111100080317    3c                   endif
111200110103     c*
111300080317    2c                   endif
111400080317    1c                   endif
111500110103     c*
111600080317    1C                   ENDIF
111700110103     c*
111800070201     C                   CLEAR                   BRVnvr
111900961213     C* FILIALE GESTIONE LA PRENDO DAL FOGLIO
112000961213     C                   CLEAR                   BRVDTR
112100961213     C                   CLEAR                   BRVFTR
112200991223     C                   CLEAR                   BRVHTR
112300961213     C                   Z-ADD     BARFGS        BRVFGS
112400961213     C                   Z-ADD     BARNPG        BRVNPG
112500990708     C                   Z-ADD     BARNFV        BRVNFV
112600040428     C* P.O. CHE HA EFFETTUATO LA SPUNTA (l'originale contenuto il WPES
112700040428     c*  e non più il bpes, per le spunte cat.1)
112800040428     C                   Z-ADD     WPES          BRVPES
112900961213     C                   MOVEL     BARLNP        BRVLNP
113000961213     C                   MOVEL     BARLNA        BRVLNA
113100961213     C                   MOVEL     BARNRS        BRVNRS
113200961213     C                   MOVEL     BARNSC        BRVNSC
113300961213     C                   Z-ADD     BARZNC        BRVZNC
113400961213     C**
113500961213     C* SE SONO IN IMMISSIONE LA AGGIORNO, IN VARIAZIONE SOLO
113600970829     C*  SE E' PISTOLA CON PRIORITA' < O UGUALE ALLA PRESENTE
113700961213     C*
113800961213     C* SE L'ANOMALIA GIA' ESISTE, NON LA TOLGO SE SU QUESTA SPUNTA
113900961213     C*  NON C'E'
114000040324    1C     *IN31         IFEQ      *ON
114100010608     C     WBRVE         OREQ      'S'
114200961213     C                   MOVEL     BARCAN        BRVCAN
114300961213     C                   MOVEL     BARNPS        BRVNPS
114400070212     C                   MOVEL     BARtap        BRVtap
114500040324     C                   Z-ADD     BARVUC        BRVVUC
114600040324     C                   Z-ADD     BARPUC        BRVPUC
114700040324   x1C                   ELSE
114800961213     C* CODICE ANOMALIA
114900961213     C     BARCAN        IFNE      ' '
115000980205     C     BRVCAN        OREQ      'E'
115100961213     C                   MOVEL     BARCAN        BRVCAN
115200961213     C                   ENDIF
115300110103     c*
115400970829     C* CERCO LA PRIORITA' DELLA PISTOLA GIA' PRESENTE
115500970829     C                   MOVEL     BRVNPS        ALF2
115600970829     C                   EXSR      RICRAG
115700970829     C* PISTOLA CON PRIORITA' < O = ALLA PRESENTE: E' + IMPORTANTE
115800970829     C     OPS(Y)        IFLE      OPS(Z)
115900961213     C                   MOVEL     BARNPS        BRVNPS
116000070212     C                   MOVEL     BARtap        BRVtap
116100961213     C                   ELSE
116200961213     C                   MOVEL     BRVNPS        BARNPS
116300070212     C                   MOVEL     BRVtap        BARtap
116400961213     C                   ENDIF
116500110103     c*
116600041011     c* Peso e volume prendo se pieno e se non c'e' già
116700041011     c                   if        barpuc>0
116800041011     c                   if        *in31 or (not *in31 and brvpuc=0)
116900040324     C                   Z-ADD     BARPUC        BRVPUC
117000040324     c                   endif
117100041011     c                   endif
117200040324     c                   if        barvuc>0
117300041011     c                   if        *in31 or (not *in31 and brvvuc=0)
117400040324     C                   Z-ADD     BARvUC        BRVvUC
117500040324     c                   endif
117600041011     c                   endif
117700110103     c*
117800040324    1C                   ENDIF
117900961213     C*
118000070212     c* Se la spunta era già stata caricata con errore (di solito
118100070212     c*  per la non creazione della anomalia 50) non aggiorno la data/ora
118200070212     c*  caricamento spunta
118300070212     c                   if        barerr<>'E' or *in31
118400961213     C                   Z-ADD     WDATE1        BRVDCS
118500070131     C                   time                    BRVHCS
118600070212     c                   endif
118700140613     c* impostazione data caricamento spunte
118800140613     c                   exsr      impoDFS
118900140613     c                   z-add     wdfs          brvdfs
119000140613     c
119100070131     c                   clear                   brvhms
119200070131     C                   movel     BARHMS        BRVHMS
119300070212     c                   z-add     barmis        brvmis
119400961213     C                   Z-ADD     BARFLE        BRVFLE
119500070212     C                   movel     BARpru        BRVpru
119600080314     c
119700080314     c
119800961213     C                   ENDSR
119900941206     C**************************************************************************
120000941206     C*    CARICO TABELLE
120100941206     C**************************************************************************
120200941229     C     CARTAB        BEGSR
120300930521     C*
120400930521     C* PARAMETRI UTENTE
120500110103     C                   Z-ADD     1             CODUT             1 0
120600110103      *
120700110103      * Reperimento dati utente/aziendali
120800110103     c                   exsr      sr_DatiJob
120900970320     C*
121000970320     C                   CLEAR                   SAVBOL
121100961212     C**
121200961212     C**  CARICO TABELLA 7J - CODICI PISTOLA
121300961122    1C     *IN02         IFEQ      '0'
121400921229     C                   MOVEL     '7J'          COD
121500990108     C                   Z-ADD     1             X                 4 0
121600921229     C     KTAB2         SETLL     TABEL
121700950927     C     KTAB2         READE     TABEL                                  99
121800921229     C*
121900961122    2C     *IN99         DOWEQ     *OFF
122000961122    3C     TBLFLG        IFEQ      ' '
122100921229     C                   MOVEL     TBLKEY        NPS(X)
122200921229     C                   MOVEL     TBLUNI        DS7J
122300921229     C                   MOVEL     §7JRPS        RPS(X)
122400000210     C                   MOVEL     §7JTPS        TPS(X)
122500000210     C                   MOVEL     §7JORD        OPS(X)
122600000316     C                   MOVEL     §7JINV        INV(X)
122700040503     C                   MOVEL     §7Jfsl        fsl(X)
122800921229     C                   ADD       1             X
122900961122    3C                   END
123000921229     C*
123100950927     C     KTAB2         READE     TABEL                                  99
123200961122    2C                   ENDDO
123300961122    1C                   ENDIF
123400961122     C**
123500961212     C** CARICO TABELLA  TV - TIPI TRAINO DEFLUENZA
123600961122     C                   MOVEL     'TV'          COD
123700990108     C                   Z-ADD     1             X
123800961122     C     KTAB2         SETLL     TABEL
123900961122     C     KTAB2         READE     TABEL                                  99
124000961122     C*
124100961122    2C     *IN99         DOWEQ     *OFF
124200961122    3C     TBLFLG        IFEQ      ' '
124300961122     C                   MOVEL     TBLUNI        DSTV
124400961122    4C     §TVDEF        IFEQ      'S'
124500961122     C                   MOVEL     TBLKEY        TTR(X)
124600961122     C                   ADD       1             X
124700961122    4C                   ENDIF
124800961122    3C                   ENDIF
124900961122     C*
125000961122     C     KTAB2         READE     TABEL                                  99
125100961122    2C                   ENDDO
125200961212     C**
125300961212     C* CARICO LA TABELLA 7N - DELLE CATEGORIE SPUNTE
125400941214     C                   MOVEL     '7N'          COD
125500941214     C                   Z-ADD     1             X
125600941214     C     KTAB2         SETLL     TABEL
125700950927     C     KTAB2         READE     TABEL                                  99
125800941214     C*
125900950927     C     *IN99         DOWEQ     '0'
126000941214     C     TBLFLG        IFEQ      ' '
126100941214     C                   MOVEL     TBLKEY        CAT(X)
126200941214     C                   MOVEL     TBLUNI        DS7N
126300941214     C                   MOVEL     §7NNST        NST(X)
126400941214     C                   ADD       1             X
126500941214     C                   ENDIF
126600941214     C*
126700950927     C     KTAB2         READE     TABEL                                  99
126800941214     C                   ENDDO
126900961212     C**
127000960702     C** CARICO TABELLA 5F - FILIALI CONFERMA BOLLE DA DISK
127100170901     C**                 MOVEL     '5F'          COD
127200170901     C**                 Z-ADD     0             X
127300170901     C**   KTAB2         SETLL     TABEL
127400170901     C**   KTAB2         READE     TABEL                                  99
127500960702     C*
127600170901    1C**   *IN99         DOWEQ     '0'
127700170901    2C**   TBLFLG        IFEQ      ' '
127800170901     C**                 MOVEL     TBLKEY        W003A             3
127900170901     C**   W003A         IFNE      '   '
128000170901     C**                 ADD       1             X
128100170901     C**                 MOVEL     TBLKEY        S5F(X)
128200970828     C* MEMORIZZO LA FIL CHE CONFERMA
128300170901     C**                 MOVEL     TBLUNI        F5F(X)
128400110103     c*
128500170901     C**   X             IFGE      9000
128600170901     C**                 SETON                                        99
128700170901     C**                 ENDIF
128800110103     c*
128900170901     C**                 ENDIF
129000170901    2C**                 ENDIF
129100960702     C*
129200170901     C**N99KTAB2         READE     TABEL                                  99
129300170901    1C**                 ENDDO
129400961212     C**
129500020204     C** CARICO TABELLA 3E - CODICI ANOMALIA VALIDI SU SPUNTE
129600020204     C                   MOVEL     '3E'          COD
129700961212     C                   Z-ADD     0             X
129800961212     C     KTAB2         SETLL     TABEL
129900961212     C     KTAB2         READE     TABEL                                  99
130000961212     C*
130100961212    1C     *IN99         DOWEQ     *OFF
130200961212    2C     TBLFLG        IFEQ      ' '
130300961212     C                   ADD       1             X
130400020204     C                   MOVEL     TBLKEY        CAN(X)
130500961212     C     X             IFGE      200
130600961212     C                   SETON                                        99
130700961212     C                   ENDIF
130800961212    2C                   ENDIF
130900961212     C*
131000961212     C  N99KTAB2         READE     TABEL                                  99
131100961212    1C                   ENDDO
131200020502     C*****
131300020502     C* RILEVO DATA E ORA PER SCRIVERLO NELLE SPUNTE
131400020502     C                   TIME                    WTIME            14 0
131500020502     C                   MOVE      WTIME         WDATE             8 0
131600020502     C                   Z-ADD     WDATE         G02DAT
131700020502     C                   Z-ADD     *ZERO         G02INV
131800020502     C                   MOVEL     *BLANKS       G02ERR
131900020502     C                   CALL      'XSRDA8'
132000020502     C                   PARM                    WLBDAT
132100020502     C                   Z-ADD     G02INV        WDATE1            8 0
132200170905
132300170905     c* sottraggo alla data del giorno 2 giorni lavorativi
132400170905     c                   clear                   xgiolavds
132500170905     c                   eval      IXGLDATA=wdate1
132600170905     c                   eval      IXGLPA  ='A'
132700170905     c                   eval      IXGLSUB ='S'
132800170905     c                   eval      IXGLGG  =2
132900170905     c                   eval      IXGLLAV ='S'
133000171023     c                   eval      IXGLfil =simfel
133100170905     c                   call      'XGIOLAV'
133200170905     c                   parm                    xgiolavds
133300170905     c                   if        OXGLDATA=0
133400170905     c                   eval      oxgldata=wdate1
133500170905     c                   endif
133600030725      *
133700030725      * CHAIN A VARIABILI PARTENZE x PRENDERE MAX PESO E VOLUME CARICABILI
133800030725     C                   EXSR      CERVPO
133900970522     C*
134000970522     C* CHAIN A VARIABILI ARRIVI PER VEDERE A QUALE LIVELLO SI CHIUDE
134100970522     C* L'IDD (LIV MAX.)
134200970522     C                   MOVEL     '7A'          COD
134300970522     C                   MOVEL(P)  '2'           KEY
134400970522     C     KTAB          CHAIN     TABEL                              99
134500970522     C  N99              MOVEL     TBLUNI        DS7A2
134600000927     C* CHAIN A GG PULIZIA PER CREAZIONE ANOMLIA 10 E 50
134700000927     C                   MOVEL     '5A'          COD
134800000927     C                   MOVEL(P)  '1'           KEY
134900000927     C     KTAB          CHAIN     TABEL                              99
135000000927     C  N99              MOVEL     TBLUNI        DS5A
135100000927     C*
135200170505     C***                MOVEL     '5A'          COD
135300170505     C***                MOVEL(P)  'PT'          KEY
135400170505     C**   KTAB          CHAIN     TABEL                              99
135500170505     C**N99              MOVEL     TBLUNI        DS5APT
135600000927     C*
135700000927     C* PREDO GG MINORI TRA PULIZIA ARB E PULIZIA ARB POSTE
135800170505     C***  §5AARB        IFLT      §5APT5
135900170505
136000000927     C                   Z-ADD     §5AARB        WGGPUL            3 0
136100170505
136200170505     C***                ELSE
136300170505     C***                Z-ADD     §5APT5        WGGPUL            3 0
136400170505     C***                ENDIF
136500000927     C**
136600000927      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA BOLLE ARRIVI
136700000927     C                   MOVE      *BLANKS       $DAT
136800000927     C                   MOVE      *BLANKS       $ERR
136900000927     C                   MOVEL     'FNLR84R'     $PGM
137000000927     C                   CALL      'TRUL49C'                            28
137100000927     C                   PARM                    $PGM             10
137200000927     C                   PARM                    $DAT              8
137300000927     C                   PARM                    $ERR              1
137400000927     C*
137500000928    3C     $ERR          IFEQ      '1'
137600000927     C     *IN28         OREQ      *ON
137700000928     C     $DAT          ORLE      *ZEROS
137800000927     C                   Z-ADD     WDATE1        G02INV
137900000927     C                   ELSE
138000000928     C                   MOVEL     $DAT          G02INV
138100000927    3C                   ENDIF
138200000927     C*
138300000927      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI
138400000927      * DI PULIZIA BOLLE ARRIVI
138500000927     C                   Z-ADD     *ZEROS        G02DAT
138600000927     C                   Z-ADD     *ZEROS        G02TGI
138700000927     C                   MOVEL     '3'           G02ERR
138800000927     C                   CALL      'XSRDA8'
138900000927     C                   PARM                    WLBDAT
139000000927      *
139100000927     C                   SUB       WGGPUL        G02TGI
139200000928     C                   Z-ADD     *ZEROS        GI8DAT
139300000928     C                   Z-ADD     *ZEROS        GI8INV
139400000928     C                   Z-ADD     G02TGI        GI8TGI
139500000927     C                   CALL      'XSRGI8'
139600000927     C                   PARM                    WGIDAT
139700000927      *
139800000928     C                   Z-ADD     GI8INV        PRIDAT
139900961212     C**
140000070209     c** udate - 4 --> sono i giorni che devo tenere le spunte dentro al
140100070212     c**               FIBAR se non riesco a creare l'anomalia collo
140200021203     c**               arrivato non part
140300021203     C                   Z-ADD     WDATE1        G02INV
140400021203     C                   Z-ADD     *ZEROS        G02DAT
140500021203     C                   Z-ADD     *ZEROS        G02TGI
140600021203     C                   MOVEL     '3'           G02ERR
140700021203     C                   CALL      'XSRDA8'
140800021203     C                   PARM                    WLBDAT
140900021203      *
141000021203     C                   SUB       4             G02TGI
141100021203     C                   Z-ADD     *ZEROS        GI8DAT
141200021203     C                   Z-ADD     *ZEROS        GI8INV
141300021203     C                   Z-ADD     G02TGI        GI8TGI
141400021203     C                   CALL      'XSRGI8'
141500021203     C                   PARM                    WGIDAT
141600021203     C                   Z-ADD     GI8INV        wpul050
141700961212     C**
141800021203     C**
141900930521     C                   MOVEL     '1'           UNO               1
142000930521     C                   MOVEL     'AA'          SAVNPS            2
142100960411     C*
142200960411     C                   MOVE      ' '           OPN9              1
142300961212     C*
142400021014     C                   CLEAR                   DSBS55
142500971113     C                   MOVEL     'L'           I50TLA
142600021014     C                   CALL      'TIBS55R'
142700021014     C                   PARM                    DSBS55
142800971113     C**
142900971113     C                   CLEAR                   KPJBA
143000021014     C                   MOVEL     'EDPCED'      WCED
143100021014     C                   MOVEL     SIMFEL        WTER
143200021014     C                   MOVEL     WPROF         KNMUS
143300971113     C                   MOVEL     O50PSI        KNSIF
143400970829     C                   MOVEL     'DSP01'       KNMTD
143500961212     C                   MOVEL     'N'           KSTJO
143600961212     C                   MOVEL     'B'           KTPAZ
143700961212     C                   MOVEL     'AZNAUTO'     KNMEB
143800961212     C                   MOVEL     'J'           KEXCN
143900961212     C                   MOVEL     'P5'          KCOJB
144000961212     C                   MOVEL     'N'           KCANC
144100000927     C*
144200000927     C                   CLEAR                   SA2DFV
144300001109     C**
144400001109     C** CARICO TABELLA 7C - ANOMALIE CREABILI DA CAT.SPUNTA GENERICA
144500001109     C                   MOVEL     '7C'          COD
144600001109     C                   Z-ADD     0             X
144700001109     C     KTAB2         SETLL     TABEL
144800001109     C     KTAB2         READE     TABEL                                  99
144900001109     C*
145000001109    1C     *IN99         DOWEQ     *OFF
145100001109    2C     TBLFLG        IFEQ      ' '
145200001109     C                   MOVEL     TBLUNI        DS7C
145300001109    3C     §7CCCG        IFEQ      'S'
145400001109     C                   ADD       1             X
145500001109     C                   MOVEL     TBLKEY        ACG(X)
145600001109    3C                   ENDIF
145700001109    2C                   ENDIF
145800001109     C*
145900001109     C     KTAB2         READE     TABEL                                  99
146000001109    1C                   ENDDO
146100110103      *
146200091209      * Aggancio tabella AR5 per record 'GEN'
146300091209     c                   Clear                   dAr5
146400091209     c                   Clear                   dsbs02
146500091209     c                   Eval      T02Mod = 'C'
146600091209     c                   Eval      T02Sif = knsif
146700091209     c                   Eval      T02Cod = 'AR5'
146800091209     c                   Movel(p)  'GEN'         T02Ke1
146900091209     c                   Call      'TIBS02R'
147000091209     c                   Parm                    Kpjba
147100091209     c                   Parm                    dsbs02
147200091209     c                   If        T02Err = *Blanks
147300091209     c                   Movel     T02Uni        dAr5
147400091209     c                   EndIf
147500170125     C*
147600170125     C* CARICO TABELLA DECOFIIMG PER le filiali con New IMA
147700170125     c                   clear                   AllNewIMA         1
147800170905     c**                 clear                   trulvpods
147900170905     c**                 eval      ivpoke1 = 'DECOFIIPG'
148000170905     c**                 call      'TRULVPOR'
148100170905     c**                 parm                    trulvpods
148200170905     c**                 if         %lookup('999':skFilOk)>0
148300170125     C                   EVAL       AllNewIMA='S'
148400170905     c**                 endif
148500170125
148600911014     C                   ENDSR
148700941206     C**************************************************************************
148800941206     C* CREO ANOMALIA
148900941206     C**************************************************************************
149000911016     C     CRTANM        BEGSR
149100000320     C                   CLEAR                   WCCH
149200970521     C*
149300970521     C                   CLEAR                   WANM10
149400941206     C*
149500970204    1C     COMCAA        IFNE      645
149600990518     C     KANM          CHAIN     FNANM000                           96
149700970204   X1C                   ELSE
149800961205     C*
149900990518     C     KANM          CHAIN(N)  FNANM000                           96
150000961205     C* PER L'ANOMALIA 645 LEGGO FINTANTO CHE NON TROVO UNA ANOMALIA
150100961205     C*  CON DATA = ALLA DATA SPUNTA: SE C'E' NON CREO
150200970204    2C     *IN96         DOWEQ     *OFF
150300990518     C     ANMDAO        ANDNE     BARDFV
150400990518     C     KANM          READE(N)  FNANM000                               96
150500970204    2C                   ENDDO
150600961205     C*
150700970204    1C                   ENDIF
150800000811     C*
150900000811    1C     *IN96         IFEQ      *ON
151000000811     C*
151100000811     C     COMCAA        ORNE      645
151200000811     C     ANMAIE        ANDEQ     'I'
151300000811     C* PER ORA SOLO SULLE INTERNE RICREO ANOMLIA SE PRESENTE
151400970204     C*  - SE CHIUSE SEMPRE
151500040923     C*  - SE APERTA SOLO LA DATA E' < DELLA ESISTENTE
151600040923     c* Per la 200 la ricreo solo se e' chiusa senza guardare la data creaz
151700961205     C*
151800970204     C                   MOVEL     'S'           WWRITE
151900970204    2C     *IN96         IFEQ      *OFF
152000970204     C     ANMDCH        ANDEQ     0
152100970204     C     ANMDAO        ANDGE     BARDFV
152200041005    2C     *IN96         oreq      *OFF
152300041005     C     ANMDCH        andeq     0
152400040923     C     comcaa        ANDeq     200
152500970204     C                   MOVEL     ' '           WWRITE            1
152600970204    2C                   ENDIF
152700970204     C**
152800070215    2C     WWRITE        IFEQ      'S'
152900990518     C                   CLEAR                   FNANM000
153000950927     C                   EXSR      RIEANM
153100000614     C**
153200001109     C* CREO SOLO SE ANOMALIA :
153300001109     C* CREABILE DA QUALUNQUE CATEGORIA SPUNTA
153400070215    3C     §7CCCG        ifeq      'S'
153500001109     C* NON CREABILE DA CATEG. GENERICA E LA CATEGORIA NON È GENERICA
153600070215     C     BARSPG        orne      'I'
153700950927     C*
153800950927     C                   MOVE      COMLNP        ANMLNP
153900990518     C                   Z-ADD     COMAAS        ANMAAS
154000950927     C                   MOVE      COMNSP        ANMNSP
154100960524     C                   MOVE      BARDFV        ANMDAO
154200990518     C                   Z-ADD     BARNFV        ANMNFV
154300950927     C                   MOVEL     BARFGS        ANMCDU
154400040621     c* imposto se spedizione di valore
154500070215    4c                   if        anmnsp>0
154600091209     c     kar5          chain(n)  fiar501l
154700070215    5c                   if        %found(fiar501l)
154800040621     c                   movel     ar5uni        dar5gen
154900040621     c                   else
155000040621     c                   clear                   dar5gen
155100070215    5c                   endif
155200040621     c                   movel     §ar5bva       anmft4
155300070215    4c                   endif
155400110103     c*
155500040429     c* 22/4/4 --> al posto di anmfgs imposto BPES il p.o. che spara
155600040429     c*            che solo per le spunte partenza è sempre barfgs
155700040429     C                   MOVEL     bpes          ANMFLE
155800070215    4C     ANMCAA        IFEQ      10
155900150205    4C     ANMCAA        oreq      410
156000070215     C                   z-add     BARZNC        ANMfl4
156100070215    4C                   ENDIF
156200911112     C*
156300921020     C* SE ESTERNA SCRIVO FIL A CUI TRASMETTERE
156400070215    4C     §7CAIE        IFEQ      'E'
156500070215    5C     COMLNP        IFGT      0
156600960528     C                   MOVEL     COMLNP        ANMFL1
156700960528     C                   ELSE
156800960704     C                   MOVEL     LNPB          ANMFL1
156900070215    5C                   ENDIF
157000070215    4C                   ENDIF
157100911016     C*
157200970204     C* SE INTERNA AUTOCHIUSA E C'E' GIA' NON FACCIO NULLA
157300070215    4C     *IN96         IFEQ      *ON
157400970204     C     §7CCHA        ORNE      'S'
157500960430     C* SE LA KEY E' PER SEGNACOLLO ED IL NUMERO DI SEGNACOLLO E' 0
157600960430     C*  NON CREO L'ANOMALIA (ERRORE DA TERMINALINO)
157700070215    5C     §7CKEY        IFNE      'S'
157800960430     C     §7CKEY        OREQ      'S'
157900960430     C     ANMSCN        ANDGT     0
158000990922     C*
158100990922     C* LA DATA I APERTURA ANOMALIA CI DEVE ESSERE , ALTRIMENTI
158200990922     C*  E' MEGLIO NON CREARE L'ANOMALIA
158300070215    6C     ANMDAO        IFGT      0
158400990518     C   96              WRITE     FNANM000
158500990518     C  N96              UPDATE    FNANM000
158600070215    6C                   ENDIF
158700070215    5C                   ENDIF
158800961205     C*
158900970204     C* SE CREO ANOAMLIA 145 O 146 CHIUDO LE ANOMALIE INTERNE 140 E 130
159000070215    5C     ANMCAA        IFEQ      145
159100970204     C     ANMCAA        OREQ      146
159200970204     C                   Z-ADD     130           COMCAA
159300970204     C                   EXSR      CHIUAN
159400970204     C                   Z-ADD     140           COMCAA
159500970204     C                   EXSR      CHIUAN
159600070215    5C                   ENDIF
159700070215    4C                   ENDIF
159800970204     C*
159900070215    3C                   ENDIF
160000070215    2C                   ENDIF
160100970521   X1C                   ELSE
160200110103     c*
160300970521     C* OPERAZIONI PER ANOMALIA 10 GIA' ESISTENTE MA CHIUSA:
160400970521    2C     COMCAA        IFEQ      10
160500970522     C* Se anomalia 10 a liv massimo --> memorizzo per creaz.anom.410
160600970522    3C     ANMLID        IFEQ      §7ALCI
160700970521     C                   MOVE      '1'           WANM10            1
160800970522    3C                   END
160900970522     C* Se anomalia chiusa (non pratica IDD):
161000970522     C* 1) sflaggo e chiudo l'anomalia mettendo cch='PR' se è a liv max
161100970522    3C     ANMDCH        IFGT      *ZEROS
161200970522     C     ANMCCH        ANDNE     'PR'
161300970522     C*
161400970522    4C                   SELECT
161500970522     C     ANMLID        WHENEQ    §7ALCI
161600970522     C     ANMCCH        ANDEQ     'AN'
161700970522     C                   MOVEL     'PR'          ANMCCH
161800970522     C                   MOVEL     'D'           ANMFSC
161900970522     C                   CLEAR                   ANMFT1
162000970522     C                   CLEAR                   ANMFT2
162100970522     C     ANMLID        WHENLT    §7ALCI
162200970523     C                   CLEAR                   ANMCCH
162300970522     C                   CLEAR                   ANMDCH
162400970522     C                   CLEAR                   ANMFSC
162500970522    4C                   ENDSL
162600990518     C                   UPDATE    FNANM000
162700970522    3C                   ENDIF
162800970522    2C                   ENDIF
162900970204    1C                   ENDIF
163000970204     C*
163100990518     C                   UNLOCK    FNANM02L
163200911016     C                   ENDSR
163300960613     C**************************************************************************
163400960613     C*    CONTROLLO IL RAGGRUPPAMENTO DELLA PISTOLA
163500960613     C**************************************************************************
163600960613     C     PISMAN        BEGSR
163700960613     C*
163800961213     C                   Z-ADD     1             Y                 3 0
163900961213     C*
164000960613    1C     *IN02         IFEQ      *OFF
164100960613     C     BARNPS        LOOKUP    NPS(Y)                                 34
164200960613     C  N34              Z-ADD     1             Y
164300960613     C*
164400960613   X1C                   ELSE
164500960613     C*
164600960613     C                   MOVEL     '7J'          COD
164700960613     C                   MOVEL(P)  BARNPS        KEY
164800960613     C     KTAB          CHAIN     TABEL                              34
164900000316     C     *IN34         IFEQ      *OFF
165000000316     C                   MOVEL     TBLUNI        DS7J
165100000316     C                   MOVEL     §7JRPS        RPS(Y)
165200000316     C                   MOVEL     §7JORD        OPS(Y)
165300000316     C                   MOVEL     §7JTPS        TPS(Y)
165400000316     C                   MOVEL     §7JINV        INV(Y)
165500040503     C                   MOVEL     §7Jfsl        fsl(Y)
165600000316     C                   ELSE
165700000316     C                   MOVEL     'M'           RPS(Y)
165800000316     C                   MOVEL     999           OPS(Y)
165900000316     C                   MOVEL     'B'           TPS(Y)
166000000316     C                   MOVEL     ' '           INV(Y)
166100040503     C                   MOVEL     ' '           fsl(Y)
166200960613    1C                   END
166300000316    1C                   END
166400960613     C                   ENDSR
166500941206     C**************************************************************************
166600020213     C*    A G G I O R N O    R E C O R D    B O L L E
166700941206     C**************************************************************************
166800980212     C     UPDARB        BEGSR
166900960521     C                   CLEAR                   WNOTRA
167000961210     C                   CLEAR                   W60X
167100961211     C                   CLEAR                   WESAN
167200961210     C                   MOVEL     'S'           W620
167300961211     C                   MOVEL     'S'           WCREA
167400021106     C                   CLEAR                   WDCM
167500021106     C                   CLEAR                   WBOAR
167600021106     C                   CLEAR                   WATDCM
167700040430     C                   CLEAR                   WABCCA
167800040430     C                   CLEAR                   WABNDC
167900161007     C                   CLEAR                   gcpdgc            8 0
168000021106     C                   MOVEL     ' '           WGIAC             1
168100941206     C*
168200921229     C* TROVO TIPO E RAGGRUPPAMENTO PISTOLE DI QUELLA CARICATA
168300960613     C                   EXSR      PISMAN
168400921229     C*
168500960523     C* CARICO LA L6 DELLA FILIALE GESTIONE
168600020213     C     BPES          IFNE      SAVPES
168700020213     C                   MOVEL     BPES          SAVPES
168800961209     C                   EXSR      CARL6
168900960523     C                   ENDIF
169000921013     C*
169100041012     C                   SETOFF                                         0332
169200950922     C                   CLEAR                   COMLNP
169300950922     C                   CLEAR                   COMAAS
169400950922     C                   CLEAR                   COMNSP
169500950922     C                   CLEAR                   WSPLOC
169600950927     C                   CLEAR                   WTIBOL
169700970423     C                   CLEAR                   WWFVC
169800020124     C                   CLEAR                   LNPB
169900020131     C                   CLEAR                   WESART
170000020131     C                   CLEAR                   WESBLT
170100020131     C                   CLEAR                   WESBTT
170200130228     c                   clear                   bolxco
170300160421     c                   clear                   bolznc
170400170509     c                   clear                   boldbr
170500021022     c*
170600021125     C* TERMINAL partenza DI BPES
170700021125     C                   MOVEL     'P'           WTITER
170800021022     C                   MOVEL     BARDFV        WDTRIF
170900021022     C                   EXSR      TERPES
171000021022     C                   Z-ADD     D55TFP        PESTFP
171100021203     c*
171200040422     c* Se trovo la bolla transito x p.o. spunta --> aggiorno transito, senz
171300021203     c*  controllo i terminal (c'e' un problema a livello di eccezioni ter
171400021203     C     KBAR37        CHAIN     FNBTT37L                           3739
171500021203     C   39              GOTO      ENDUPD
171600021203     C  N37KBTT          CHAIN     FNBTP11L                           37
171700021203     C**
171800021203    1C     *IN37         IFEQ      *OFF
171900021203     C                   MOVEL     'S'           WESBTT            1
172000021203     C                   MOVEL     BTTLNP        LNPB
172100021203     C                   MOVEL     BTPDSP        DSPB
172200021203     C                   MOVEL     BTPDSP        WDTRIF
172300021203     C                   MOVEL     BTPTFP        LNPTFP
172400050603     C                   MOVEL     BTPTFa        bolTFa
172500170509     C                   MOVE      BTPdbr        boldbr
172600040621     C                   MOVE      BTTLNP        COMLNP
172700040621     C                   MOVE      BTTAAS        COMAAS
172800040621     C                   MOVE      BTTNSP        COMNSP
172900021203   X1C                   ELSE
173000021203     C                   MOVEL     'N'           WESBTT
173100020124     C**
173200020124     C** VEDO SE TROVO LA BOLLA IN BLT
173300020124     C     KBAR          CHAIN     FNBLT27L                           3239
173400020213     C   39              GOTO      ENDUPD
173500020213     C  N32KBLT          CHAIN(N)  FNBLP01L                           32
173600020124     C**
173700021203    2C     *IN32         IFEQ      *OFF
173800020131     C                   MOVEL     'S'           WESBLT            1
173900020124     C                   MOVEL     BLTLNP        LNPB
174000020201     C                   MOVEL     BLPTFP        LNPTFP
174100050603     C                   MOVEL     BLPTFa        bolTFa
174200020201     C                   MOVEL     BLPDSP        WDTRIF
174300020214     C                   MOVEL     BLPDSP        DSPB
174400130301     c                   movel     BLPxco        bolxco            1
174500160421     c                   movel     BLPznc        bolznc            2
174600170509     C                   MOVE      BLPdbr        boldbr
174700040621     C                   MOVE      BLTLNP        COMLNP
174800040621     C                   MOVE      BLTAAS        COMAAS
174900040621     C                   MOVE      BLTNSP        COMNSP
175000021203   X2C                   ELSE
175100020213     C                   MOVEL     'N'           WESBLT
175200020124     C** SE NON TROVO LA BOLLA PARTENZA CERCO NEGLI ARRIVI
175300020124     C     KBAR          CHAIN     FNART27L                           3239
175400020213     C   39              GOTO      ENDUPD
175500050223     C  N32KARB          CHAIN(N)  FNARB01L                           32
175600020124     C**
175700021203    3C     *IN32         IFEQ      *OFF
175800020201     C                   MOVEL     'S'           WESART            1
175900020124     C                   MOVEL     ARTLNP        LNPB
176000020214     C                   MOVEL     ARBDSP        DSPB
176100020201     C                   MOVEL     ARBDSP        WDTRIF
176200020201     C                   MOVEL     ARBTFP        LNPTFP
176300050603     C                   MOVEL     ARBTFa        boltfa
176400130301     c                   movel     ARBxco        bolxco            1
176500160421     c                   movel     ARBznc        bolznc
176600170509     C                   MOVE      ARBdbr        boldbr
176700040621     C                   MOVE      ARTLNP        COMLNP
176800040621     C                   MOVE      ARTAAS        COMAAS
176900040621     C                   MOVE      ARTNSP        COMNSP
177000021203   X3C                   ELSE
177100020213     C                   MOVEL     'N'           WESART
177200020131     C** IMPOSTO LNPB = BARLNP
177300020131     C                   MOVEL     BARLNP        LNPB
177400020214     C                   Z-ADD     BARDFV        DSPB
177500050603     c                   clear                   boltfa
177600020124     C**
177700020124     C** SE NON TROVATA MAI LA BOLLA, CERCO SE ESISTE LA EVENTUALE
177800020124     C*   SPUNTA PARTENZA E LA TABELLA 5F
177900020205    4C     BARNRS        IFGT      *ZEROS
178000020124     C                   EXSR      RICLNP
178100020213    4C                   ENDIF
178200021105     c* prendo terminal di partenza della linea partenza calcolata
178300170901     c                   if        bardfv>0
178400021105     C                   Z-ADD     BARDFV        WDTRIF
178500070222     c                   else
178600070222     c                   z-add     wdate1        wdtrif
178700070222     c                   endif
178800070222     c
178900021105     C                   EXSR      TERPAR
179000020213    3C                   ENDIF
179100020213    2C                   ENDIF
179200020213    1C                   ENDIF
179300020204     C**
179400020215     C** CONTROLLO TRA P.O. SPUNTA E LINEE SPUNTE CHE AGGIONAMENTI
179500020213     C**  POSSO FARE
179600021125     c*
179700050215     C* TERMINAL arrivo DI BPES
179800021125     C                   MOVEL     'A'           WTITER
179900021125     C                   MOVEL     BARDFV        WDTRIF
180000021125     C                   EXSR      TERPES
180100021125     C                   Z-ADD     D55TFA        PESTFA
180200021125     c
180300020204     C                   EXSR      CTRAGG
180400020213     C**
180500020213     C** A G G I O R N A M E N T O   B O L L E   P A R T E N Z A
180600020213    1C     WAGPAR        IFEQ      'S'
180700021203    2C     WESblt        IFEQ      ' '
180800021203     C     KBAR          CHAIN     FNBLT27L                           3239
180900021203     C   39              GOTO      ENDUPD
181000021203     C  N32              MOVEL     'S'           WESBLT            1
181100021203     C   32              MOVEL     'N'           WESBLT            1
181200021203    2C                   ENDIF
181300021203     c**
181400021203    2C     WESBLT        ifeq      'S'
181500150817     c* Aggiorno solo se la bolla NON parte da + filiali
181600150817     c* oppure sel la bolla per LINEA parte da + filiali ma è mia bolla
181700150817     c* oppure se chi spara è la linea partenza della bolla che parte da + fil
181800150622     c                   if        wbsptp=' ' or wbsptp='L'
181900150622     c                             or bpes=lnpb
182000020213     C                   EXSR      AGGBLT
182100150622     c                   else
182200150622     c                   clear                   wagPAR
182300150622     c                   endif
182400150622     c
182500021203    3C     WEND          IFEQ      '1'
182600020213     C                   GOTO      ENDUPD
182700021203    3C                   ENDIF
182800021203    2C                   ENDIF
182900020213     C*
183000021203    2C     WESBLT        ifeq      'N'
183100020204     C                   MOVEL     'N'           WBOPAR            1
183200020204     C                   MOVEL     'P'           WTIBOL            1
183300021203    2C                   ENDIF
183400021203    1C                   ENDIF
183500020213     C*
183600020213    1C     WAGPAR        IFNE      'S'
183700020213     C     WESBLT        ANDEQ     'S'
183800020213     C                   UNLOCK    FNBLT27L
183900020213    1C                   ENDIF
184000020213     C**
184100020213     C* A G G I O R N O   B O L L E   A R R I V I
184200020213     C* A PRESCINDERE DAL FATTO CHE TROVI O MENO LA BOLLA
184300020213    1C     WAGARR        IFEQ      'S'
184400020220     C* LEGGO LE BOLLE ARRIVI ANCHE PER FLAG=6.
184500020220     C*  SI TRATTA DI SPUNTA DI DISGUIDO PER CUI NON AGGIORNEREBBE
184600020220     C*  LA BOLLA ARRIVI MA MI SERVE PER ENTRARE IN CTRA60
184700070221    1C     dls45FLG      OREQ      '6'
184800020213     C* SE NON HO ANCORA CHAINATO IL DETTAGLIO COLLI LO FACCIO ORA
184900020213    2C     WESART        IFEQ      ' '
185000020213     C     KBAR          CHAIN     FNART27L                           3239
185100020218     C   39              GOTO      ENDUPD
185200020213     C  N32              MOVEL     'S'           WESART            1
185300020214     C   32              MOVEL     'N'           WESART            1
185400020213    2C                   ENDIF
185500020213     C*
185600020213     C                   EXSR      AGGART
185700020213    1C                   ENDIF
185800020213     C*
185900020213    1C     WAGARR        IFNE      'S'
186000020213     C     WESART        ANDEQ     'S'
186100020213     C                   UNLOCK    FNART27L
186200020213    1C                   ENDIF
186300040428     c*
186400040428     c*Chiudo eventuale anomalia di disguido presente sul collo, se
186500040428     c*  di area <> dal'anomalia
186600040503     C* SOLO SE SPUNTA DI PISTOLA REALE
186700040503     c                   if        fsl(y)<>'F'
186800040428     c                   eval      comcaa=200
186900040428     C*
187000040503     C     Kanm          chain(N)  FNANM000
187100040428     c                   if        %found(fnanm02l)
187200040428     C                   CLEAR                   DSLV55
187300040428     C                   MOVEL     'P'           D55TPT
187400040428     C                   Z-ADD     anmfle        D55LIN
187500040428     C                   Z-ADD     bardfv        D55DRF
187600040428     C                   EXSR      FNLV55
187700040428     c                   if        pestfp<>d55tfp  or
187800040428     c                             bpes=anmlna
187900040428     c                   exsr      chiuan
188000040428     c                   endif
188100040428     c                   endif
188200040503     c                   endif
188300921013     C**
188400960531     C** T R A N S I T I  /  D I S G U I D I   I N   A R E A   E  N O N
188500921013     C**
188600070221    0C     dls45FLG      IFNE      '6'
188700911112     C*
188800960522     C* NON E' UNA SPUNTA DEL TERMINAL DI ARRIVO
188900050603    1C     Usalnatfa     IFNE      BPES
189000021022     c     wesbtt        oreq      'S'
189100960522     C*
189200960522     C* SE NEMMENO I 2 TERMINAL DI ARRIVO CORRISPONDONO
189300960522     C* --> T R A N S I T O
189400050603    2C     Usalnatfa     IFNE      PESTFA
189500021022     c     wesbtt        oreq      'S'
189600960531     C**
189700020214     C** I TRANSITI SONO ANCORA DA SISTEMARE
189800020214     C**  QUESTE 2 SPECIFICHE SONO DA ELIMINARE: INFATTI LA BOLLA POTRA
189900020214     C**  ESSERE SIA IN BLT CHE IN BTT
190000020205    3C     WAGPAR        IFEQ      ' '
190100960703     C     WBOPAR        OREQ      'N'
190200960522     C**
190300040422     C     WESBTT        ifeq      'N'
190400960521     C**
190500960612     C* SE E' SPUNTA ENTRATA O PARTENZA E NON HO TROVATO NE' LA BOLLA
190600960521     C*  PARTENZA NE' LA BOLLA TRANSITO, PER SERIE =0
190700960521     C*  IMPOSTO NELLA SPUNTA FLAG TRASMISSIONE "N" E 99999999
190800960612     C*  PER NON AUTOGENERARE LA SPUNTA PARTENZA O NON TRASMETTERLA
190900001123     C* IDEM PER SPUNTA IMI (PER BOLLE POSTE AUTOGENERA ANCHE DA SPUNTE
191000001123     C* IMI)
191100960522    5C     BARNPG        IFEQ      5
191200020205     C     BARNPG        OREQ      1
191300001123     C     BARNPG        OREQ      3
191400001123     C     BARSPG        ANDEQ     'I'
191500020205    6C     BARNRS        IFEQ      0
191600020205     C     WAGPAR        ANDEQ     'S'
191700960521     C     WBOPAR        ANDEQ     'N'
191800960521     C                   MOVEL     'S'           WNOTRA            1
191900020205    6C                   ENDIF
192000960612    5C                   ENDIF
192100040120     c
192200911016     C*
192300110124     C* SE NON E' BOLLA LOCALE e nemmeno in £6 : DISGUIDO
192400150817     c*  non è disguido nemmeno se collo che parte da altra filiale
192500150817     c*  ( la bolla deve essere spostata di lnp)
192600020205    5C     WAGPAR        IFEQ      ' '
192700110124    5C     WAGarr        andeq     ' '
192800150817    5C     WBSPSPU       andeq     ' '
192900150817     c
193000020215     C     BARFLE        IFEQ      BTFP
193100960522     C                   Z-ADD     200           COMCAA
193200960404     C  N05              EXSR      CRTANM
193300020215     C                   ENDIF
193400020215     C* DEVO COMUNQUE DIRE CHE SI TRATTA DI UN DISGUIDO
193500960404     C                   MOVEL     'D'           WTIBOL
193600041012     c
193700041012     c* Aggiorno anche la bolla in  arrivo
193800041012     c                   EXSR      AGGARRIVO
193900960522    5C                   ENDIF
194000960522     C**
194100911016     C* 37 OFF - TROVATA AGGIORNO BOLLA TRANSITO
194200960522   X4C                   ELSE
19430002021418   C                   EXSR      AGGBTT
194400960522    4C                   ENDIF
194500040430     c*
194600040430     c* Eventuale spunta di disguido o transito, chiude fuori posto
194700040430     c*  di bolla partenza
194800040430     C                   Z-ADD     120           COMCAA
194900040430     C                   EXSR      CHIUAN
195000970204     C*
195100970204   X3C                   ELSE
195200970204     C* SE NON E' COLLO DELLA AREA DI ARRIVO E COLLO IN PARTENZA
195300970204     C*  VEDO SE CREARE L'ANOMALIA FUORI POSTO BOLLA IN PARTENZA
195400970204     C                   EXSR      ANM120
195500990120     C** VERIFICO LA PRESENZA DI UNA C.A. PER CREARE EVENTUALI ANOM
195600000523     C** IGNORO SE E' SPUNTA CAT 4-8 SOLO IN ARRIVO(IN PART.SEMPRE)
195700990121     C                   MOVEL     'S'           WCAT4             1
195800990126     C*     GESTISCO IL RITROVAMENTO COLLO
195900990126     C                   MOVEL     'S'           WRITRO            1
196000990126     C*     GESTISCO LA CREAZIONE ANOMALIE
196100990126     C                   MOVEL     'S'           WCREAN            1
196200990120     C                   EXSR      CTRDAN
196300990120     C**
196400990120     C** SE NON TROVATA CA DI COLLO
196500990120     C* VERIFICO SE LA SPEDIZIONE E' SU UNA CA CHIUSA --> CREO
196600990120    4C     WTRDAN        IFEQ      ' '
196700040811      *
196800040811      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
196900040811      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
197000040811      * legate alla spedizione che passo
197100040811     c                   clear                   fidn12ds
197200170505     c***                eval      i12aas = BLTAAS
197300170505     c***                eval      i12lnp = BLTLNP
197400170505     c***                eval      i12nrs = BLTNRS
197500170505     c***                eval      i12nsp = BLTNSP
197600170505     c* per evitare che i campi siamo sporchi, uso i salvati
197700170505     c                   eval      i12aas = comAAS
197800170505     c                   eval      i12lnp = comLNP
197900170505     c                   eval      i12nrs = barNRS
198000170505     c                   eval      i12nsp = comNSP
198100170505     c                   eval      i12fel = 'E'
198200040811     c*
198300040811     c                   call      'FIDN12R'
198400040811     c                   parm                    fidn12ds
198500040811     c*
198600040811     c                   setoff                                       33
198700040811     c*
198800040811     c* se non ci sono errori
198900040811     c                   if        o12err <> ' '
199000040811     c                   seton                                        33
199100040811     c                   else
199200040811     c* se numero di CA trovate maggiore di zero
199300040811     c                   if        o12nca = 0
199400040811     c                   seton                                        33
199500040811     c                   else
199600040811     c                   z-add     1             jj                2 0
199700040811     c                   dow       jj <= o12nca and *in33 = *off
199800040811     C     skDch(jj)     IFGT      0
199900040811     C*
200000040811     C* Valorizzo i campi DCT* utilizzati nella routine CRE151 con quelli
200100040811     C* ritornati dal *pgm d utilità FIDN12R
200200040811     C                   movel     skCch(jj)     DCTCCH
200300040811     C*
200400040811     C                   EXSR      CRE151
200500040811     C                   SETON                                        33
200600040811     C                   ENDIF
200700040811     c*
200800040811     c                   add       1             jj
200900040811     c                   enddo
201000040811     c*
201100040811     c                   seton                                        33
201200040811     c*
201300040811     c                   endif
201400040811     c                   endif
201500040811     C**
201600990120    4C                   ENDIF
201700990120     C**
201800960531    3C                   ENDIF
201900960522     C*
202000960531   X2C                   ELSE
202100970203     C*  D I S G U I D O   I N   A R E A
202200970203     C*  (SE NON E' UNA MIA BOLLA PARTENZA)
202300961021     C                   SETOFF                                       88
202400961122     C                   CLEAR                   WSTESS
202500961028     C*
202600020213    3C  N05BPES          IFNE      BARLNA
202700050930     c* al posto di BPES uso barfgs per le spunte IMP: mi fa più comodo
202800050930     c*  usare direttamente il terminal di partenza di chi spara perchè
202900050930     c*  l'IMP lo fa solo il terminal di partenza ma può essere usato
203000050930     c*  anche dai 2 livello
203100050930     C     barfgs        ANDNE     LNPB
203200960522     C     BARNPG        ANDGT     1
203300050930     C     BARNPG        ANDne     5
203400050930     c* Se spunta entrata escludo se di tratta di un aggiornamento
203500050930     c*  in partenza e non in arrivo (per cui il p.o. fa parte
203600050930     c*  dell'area di partenza
203700050930    3C     BPES          orne      BARLNA
203800050930     C     barfgs        ANDNE     LNPB
203900050930     C     BARNPG        ANDeq     5
204000050930     c     wagarr        andeq     'S'
204100050930     c
204200980727     C** CREO ANOMALIE SE COLLO SPUNTATO IN IMA ANCHE SE DA ME CONFERMA
204300020213    3C     BPES          ORNE      BARLNA
204400020213     C     BPES          ANDEQ     LNPB
204500980727     C     BARNPG        ANDEQ     3
204600980727     C     BARSPG        ANDEQ     'A'
204700960523     C*
204800020213     C* NON DEVE ESSERE PRESENTE NEMMENO IN L6 DEL P.O. SPUNTA
204900960523     C     BARLNA        LOOKUP    L6                                     88
205000980127     C**
205100960531    4C     *IN88         IFEQ      *OFF
205200961202     C                   Z-ADD     146           CAACRE
205300961202     C                   Z-ADD     145           CAACIU
205400961202     C                   EXSR      ANM14X
205500960531    4C                   ENDIF
205600960531    3C                   ENDIF
205700960522     C*
205800960523     C* ALTRIMENTI  LA CHIUDO
205900960531    3C     *IN88         IFEQ      *ON
206000960523     C     BARNPG        OREQ      1
206100020213     C     BPES          OREQ      BARLNA
206200961122     C*
206300000523     C* SE CHI SPARA E' LA FILIALE FINALE DI ARRIVO, ME LO MEMORIZZO
206400961122     C*  MI SERVE PER LA CHIUSURA
206500961122    4C     *IN88         IFEQ      *ON
206600020213     C     BPES          OREQ      BARLNA
206700961122     C                   MOVEL     'S'           WSTESS
206800961122    4C                   ENDIF
206900961122     C**
207000960522     C                   Z-ADD     146           COMCAA
207100960522     C                   EXSR      CHIUAN
207200961122     C*
207300960522     C                   Z-ADD     145           COMCAA
207400960522     C                   EXSR      CHIUAN
207500970204     C*
207600970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
207700970204     C                   EXSR      ANMARR
207800960531    3C                   ENDIF
207900961021     C*
208000970203     C* CREO E/O CHIUDO LE ANOMALIE CHE RIGUARDANO L'AREA DI ARRIVO
208100970203     C                   EXSR      ANMSEM
208200960531    2C                   ENDIF
208300911016     C*
208400960531   X1C                   ELSE
208500970203     C* T E R M I N A L   D I   A R R I V O
208600961122     C                   SETOFF                                       88
208700961122     C                   CLEAR                   WSTESS
208800970203     C*
208900970203     C* CREO E/O CHIUDO LE ANOMALIE CHE RIGUARDANO L'AREA DI ARRIVO
209000970203     C                   EXSR      ANMSEM
209100970204     C*
209200970204     C* CHIUDO EVENTUALE ANOMALIA BOLLA ARRIVI
209300970204    2C     BARNPG        IFEQ      2
209400970204     C                   Z-ADD     130           COMCAA
209500970204     C                   EXSR      CHIUAN
209600970204    2C                   ENDIF
209700940330     C*
209800940330     C* SONO TERMINAL DI ARRIVO-ANOMALIA:INOLTRARE COLLO ALL'ARRIVO
209900960531    2C     WTIBOL        IFNE      'A'
210000020218    2C     WTIBOL        ANDNE     'L'
210100960522     C                   MOVEL     'R'           WTIBOL
210200960531    2C                   ENDIF
210300960522     C*
210400960522     C* SOLO SE NON E' UN MIO COLLO
210500020213    2C  N05BPES          IFNE      BARLNA
210600091211     c
210700091211     C* CARICO LA L6 DELLA FILIALE GESTIONE
210800091211     C     BPES          IFNE      SAVPES
210900091211     C                   MOVEL     BPES          SAVPES
211000091211     C                   EXSR      CARL6
211100091211     C                   ENDIF
211200091211     c
211300960523     C* NON DEVE ESSERE PRESENTE NEMMENO IN L6
211400960531     C     BARLNA        LOOKUP    L6                                     88
211500960531    3C     *IN88         IFEQ      *OFF
211600960523     C*
211700960531    4C     BARNPG        IFGT      2
211800960522     C     BARNPG        ANDLT     5
211900000523     C     BARNPG        OREQ      8
212000961202     C                   Z-ADD     145           CAACRE
212100961202     C                   Z-ADD     146           CAACIU
212200961202     C                   EXSR      ANM14X
212300960522     C*
212400960531   X4C                   ELSE
212500961211     C* SE TROVO LA 145 E LA POSSO CHIUDERE CREO SE POSSO LA 620
212600961211     C*  SE NON TROVO LA 145 DEMANDO L'EVENTUALE CREAZIONE DELLA 620
212700961211     C*  ALLA CHIUSURA DELLA EVENTUALE 146, CHE POTREBBE ANCHE NON
212800961211     C*  ESSERCI. SIA CHE CI SIA DA CHIUDERE CHE NON CI SIA, A QUESTO
212900961211     C*  PUNTO LA 620 LA CREO
213000960522     C                   Z-ADD     145           COMCAA
213100961211     C* SOLO SE ESISTE L'ANOMALIA CREO LA 620
213200961211     C                   MOVEL     'S'           WESAN             1
213300961211     C*
213400960522     C                   EXSR      CHIUAN
213500961211     C                   CLEAR                   WESAN
213600961029     C* SE C'E' CHIUDO ANCHE IL DISGUIDO IN AREA
213700961029     C                   Z-ADD     146           COMCAA
213800961211     C*
213900961211     C   97              MOVEL     'S'           W620
214000961211     C  N97              MOVEL     'N'           W620
214100961029     C                   EXSR      CHIUAN
214200961211     C                   MOVEL     'S'           W620
214300960523    4C                   ENDIF
214400961021   X3C                   ELSE
214500961122     C*
214600020213     C* SE CHI SPARA E' LA FILIALE FINALE DI ARRIVO, ME LO MEMORIZZO
214700961122     C*  MI SERVE PER LA CHIUSURA
214800961122     C                   MOVEL     'S'           WSTESS            1
214900961021     C                   Z-ADD     146           COMCAA
215000961021     C                   EXSR      CHIUAN
215100970204     C*
215200970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
215300970204     C                   EXSR      ANMARR
215400960522    3C                   ENDIF
215500961122     C**
215600961021   X2C                   ELSE
215700970204     C* CHI SPARA E' LA FILIALE FINALE DI ARRIVO
215800961122     C                   MOVEL     'S'           WSTESS
215900961021     C                   Z-ADD     146           COMCAA
216000961021     C                   EXSR      CHIUAN
216100970204     C*
216200970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
216300970204     C                   EXSR      ANMARR
216400960523    2C                   ENDIF
216500911016     C*
216600950922    1C                   END
216700961213    0C                   ENDIF
216800021022     c
216900021203     c                   unlock    fnbtt37l
217000970203     C**
217100970203     C** SE FLAG = 2 MODIFICO ANOMALIE METTENDO NUOVO NUMERO DI FOGLIO
217200970203     C**
217300970203     C     *IN05         IFEQ      *ON
217400970203     C*
217500921021     C                   EXSR      RICFAS
217600921021     C*
217700990518     C     KBAR          SETLL     FNANM000
217800990518     C     KBAR          READE     FNANM000                               99
217900921021     C*
218000960423    2C     *IN99         DOWEQ     '0'
218100070208    3C     ANMNFV        IFEQ      DLS45NFVO
218200000516     C* SE NON C'E' SOTTO CATEGORIA DEVONO ESSERE UGUALI
218300000516    4C     BARSPG        IFEQ      ' '
218400921021     C     ANMFSA        ANDEQ     COMFAS
218500000516     C* SE     C'E' SOTTO CATEGORIA, DEVE ESSERE UGUALE AD UNA DELLE
218600000516     C*  TANTE
218700000516     C     ANMFSA        OREQ      WFAS1
218800000516     C     ANMFSA        OREQ      WFAS2
218900000516     C     ANMFSA        OREQ      WFAS3
219000000516     C     ANMFSA        OREQ      WFAS4
219100000516     C     ANMFSA        OREQ      WFAS5
219200921021     C                   Z-ADD     BARNFV        ANMNFV
219300960524     C                   Z-ADD     BARDFV        ANMDAO
219400000516     C                   MOVEL     COMFAS        ANMFSA
219500990518     C                   UPDATE    FNANM000
219600000516    4C                   ENDIF
219700000516    3C                   ENDIF
219800921021     C*
219900990518     C     KBAR          READE     FNANM000                               99
220000000516    2C                   ENDDO
220100920424     C*
220200920424     C                   Z-ADD     3             B
220300021107    1C                   END
220400920424     C     ENDUPD        TAG
220500000209     C*
220600110907    1C     *IN39         IFEQ      *ON
220700000209     C                   SETON                                        40
220800110907   x1C                   ELSE
220900000209     C*
221000000210     C* ELABORO SOLO SPUNTA DA MACCHINONE
221100110907     c*  non lo faccio se modifica di numero foglio da video
221200110907    2c  N05RPS(Y)        ifne      'A'
221300000209     C* AGGIORNO FILE  FISGN SE C'E'
221400000209     C     KBAR          CHAIN     FISGN03L                           33
221500110907    3C     *IN33         DOWEQ     *OFF
221600110907    4C     SGNST2        IFLT      9
221700000209     C                   ADD       1             SGNST2
221800001228     C                   CLEAR                   SGNT6A
221900001228     C                   CLEAR                   SGNT6B
222000001228     C                   CLEAR                   SGNT6C
222100001228     C                   CLEAR                   SGNT6D
222200001228     C                   CLEAR                   SGNT6E
222300001228     C                   CLEAR                   SGNT6F
222400080624     c                   eval      sgndatora = %char(%timestamp:*iso0)
222500000209     C                   UPDATE    FISGN000
222600110907    4C                   ENDIF
222700001020     C     KBAR          READE     FISGN03L                               33
222800110907    3C                   ENDDO
222900110907     c
223000110907     c* Una qualsiasi spunta REALE chiude eventuale anomalia interna
223100110907     c*  115 - COLLO MAI AFFIDATO
223200110929     C                   MOVEL     'AR'          SAVCCH
223300110907     C                   Z-ADD     115           COMCAA
223400110907     C                   EXSR      CHIUAN
223500110907     c
223600110907     c
223700110907    2C                   ENDIF
223800110907    1C                   ENDIF
223900941206     C*
224000020215     C                   ENDSR
224100961118     C**************************************************************************
224200961205     C* CREO ANOMALIA 600 PER L'EXPORT   COLLO A TERRA
224300961118     C**************************************************************************
224400961118     C     ANM600        BEGSR
224500961210     C*
224600961210     C* PER ESTERO CREO ANOMALIA 645 E 600
224700020212    7C     NTWBAR        IFEQ      'EEX'
224800060322    7C     NTWBAR        oreq      'DPD'
224900060322    7C     NTWBAR        oreq      'FED'
225000961210     C                   Z-ADD     645           COMCAA
225100961210     C                   EXSR      CRTANM
225200961210     C*
225300961210    8C     W60X          IFEQ      'S'
225400961209     C* SE ESISTE LA 620  LA DELETO
225500961118     C                   Z-ADD     620           COMCAA
225600001110     C     COMCAA        LOOKUP    ACG                                    34
225700001110     C     BARSPG        IFNE      'I'
225800001110     C     *IN34         OREQ      *ON
225900990518     C     KANM          DELETE    FNANM02L                           34
226000001110     C                   ENDIF
226100961118     C                   Z-ADD     600           COMCAA
226200961118     C                   EXSR      CRTANM
226300961210     C                   CLEAR                   W60X
226400961210    8C                   ENDIF
226500961210    7C                   ENDIF
226600961210     C                   ENDSR
226700960702     C**************************************************************************
226800960702     C* CERCO LNP BOLLA SE COLLO CON SERIE > 0
226900960702     C**************************************************************************
227000960702     C     RICLNP        BEGSR
227100970814     C                   SETOFF                                       33
227200170901     c                   clear                   wtab5f            1
227300170905     c                   clear                   datapiu           8 0
227400170905     c                   clear                   datameno          8 0
227500170905     c                   clear                   lnpmeno           3 0
227600170905     c                   clear                   lnppiu            3 0
227700960702     C*
227800020201     C* CONTROLLO SE COLLO CONFERMABILE DAL TERMINAL DEL P.O. EFFETTUA
227900020201     C*  SPUNTA
228000170901     C**                 MOVEL     PESTFP        W008A             8
228100170901     C**                 MOVE      BARNRS        W005A             5
228200170901     C**                 MOVEL     BARLNP        W005A
228300170901     C**                 MOVE      W005A         W008A
228400170901     C**                 Z-ADD     1             YY                4 0
228500170901     C*+   W008A         LOOKUP    S5F(YY)                                33
228600020131     C**
228700020131     C* SE NON TROVATO CERCO COME SERIE = 99
228800170901     C**   *IN33         IFEQ      *OFF
228900170901     C**                 MOVE      99            W008A
229000170901     C**                 Z-ADD     1             YY                4 0
229100170901     C**   W008A         LOOKUP    S5F(YY)                                33
229200170901     C**                 ENDIF
229300020131     C**
229400170901    1C**   *IN33         IFEQ      *ON
229500170901     C**                 MOVEL     'S'           WTAB5F            1
229600020131     C* CERCO SPUNTA PARTENZA SE C'E' PRENDO LA FGS DI QUEST'ULTIMA
229700020201     C*  E LA DATA DI RIFERIMENTO E' LA DATA DI QUESTA SPUNTA
229800170901     C                   Z-ADD     1             WNPG
229900170904     C                   Z-ADD     1             conta             1 0
230000170904    1c                   dow       conta<=2 and wtab5f=' '
230100070131     C     KBAR3         SETLL     FNBRV09L
230200070131     C     KBAR3         READE     FNBRV09L                               33
230300020131    2C     *IN33         DOWEQ     *OFF
230400040303    3C     BR9ATR        IFEQ      *BLANKS
230500040323     c* Escludo le defluenze e le spunte di simfel(potrebbe quindi essere co
230600040323     c*  lo in transito da me e quindi non partito da me)
230700040323    4C     BR9KEY        IFNE      SA1KEY
230800040323     C                   MOVEL     BR9KEY        SA1KEY
230900040323     C                   Z-ADD     BR9NPG        WNPG
231000040323     C                   Z-ADD     BR9NFV        WNFV
231100040323     C                   Z-ADD     BR9FGS        WFGS
231200070906     C                   movel     BR9nps        Wnps
231300040323     C                   EXSR      RILFOG
231400040323     c                   Z-ADD     WDFV          BRVDFV
231500140616     c                   movel     WFCF          BRVFCF
231600140616     c                   movel     Wspg          BRVspg
231700040323    5C     WTTR          IFNE      *BLANKS
231800040323     C     WTTR          LOOKUP    TTR                                    34
231900040323     C                   MOVE      *IN34         W9DEFL            1
232000040323     C                   ELSE
232100040323     C                   MOVE      '0'           W9DEFL            1
232200040323    5C                   ENDIF
232300040323    4C                   ENDIF
232400040323    4c                   if        w9defl='0' and br9fgs<>simfel
232500110103     c*
232600040303     C                   MOVE      BR9PES        LNPB
232700170901     C                   eval      WTAB5F='N'
232800020214     C                   Z-ADD     BRVDFV        DSPB
232900020214     C                   Z-ADD     BRVDFV        WDTRIF
233000960702     C                   SETON                                        33
233100020201    4C                   END
233200020131    3C                   END
233300070131     C  N33KBAR3         READE     FNBRV09L                               33
233400020131    2C                   ENDDO
233500170904     c                   eval      wnpg=5
233600170904     c                   eval      conta=conta+1
233700170904    1c                   enddo
233800020204     C**
233900170901    2C**   WTAB5F        IFEQ      'S'
234000020218     C* CERCO SE C'E' IL P.O. CHE HA SPUNTATO, ALTRIMENTI IL PRIMO
234100170901     C**                 MOVEA     F5F(YY)       WPO
234200170901     C**                 Z-ADD     1             ZZ
234300170901     C**                 CLEAR                   WTROVA
234400170901    3C**                 DO        4             ZZ                3 0
234500170901    4C**   WPO(ZZ)       IFGT      *ZEROS
234600170901     C**                 MOVEL     WPO(ZZ)       W0030             3 0
234700170901    5C**   W0030         IFEQ      BPES
234800170901     C**                 MOVEL     W0030         LNPB
234900170901     C**                 MOVEL     'S'           WTROVA
235000170901    5C**                 ENDIF
235100170901    4C**                 ENDIF
235200170901    3C**                 ENDDO
235300020218     C**
235400020218     C* SE NON C'E' P.O. = BPES PRENDO IL PRIMO
235500170901     C**   WTROVA        IFEQ      ' '
235600170901     C**                 MOVEL     WPO(1)        LNPB
235700170901     C**                 ENDIF
235800170901     C**                 MOVEL     BARDFV        DSPB
235900170901     C**                 MOVEL     BARDFV        WDTRIF
236000170901    2C**                 ENDIF
236100170901    1C**                 ENDIF
236200170904    1c                   if        wtab5f=' '
236300170904    2c                   if        barnpg=5 or (barnpg=3 and barspg='P')
236400170901     c                             or barnpg=1
236500170901     c                   eval      lnpb=bpes
236600170901     C                   MOVEL     BARDFV        DSPB
236700170901     C                   MOVEL     BARDFV        WDTRIF
236800170904   x2C                   else
236900170904     C*se categoria arrivi devo vedere se trovo bolle con quella lnp/serie
237000170904     c     kblt29        setll     fnblt29l
237100170904     c     kblt29b       reade     fnblt29l
237200170904    3c                   if        not %eof(fnblt29l)
237300170905     c                   exsr      ctr_nsc
237400170905     c                   if        boldat>0
237500170905     c                   eval      datapiu=boldat
237600170905     c                   eval      lnppiu=bollnp
237700170905    3c                   endif
237800170905    3c                   endif
237900170904
238000170904     c     kblt29        setll     fnblt29l
238100170904     c     kblt29b       readpe    fnblt29l
238200170904    3c                   if        not %eof(fnblt29l)
238300170905     c                   exsr      ctr_nsc
238400170905     c                   if        boldat>0
238500170905     c                   eval      datameno=boldat
238600170905     c                   eval      lnpmeno=bollnp
238700170904    3c                   endif
238800170905    3c                   endif
238900170905     c
239000170905     c                   clear                   boldat
239100170905     c                   clear                   bollnp
239200170905    3c                   select
239300170905     c                   when      datapiu>datameno
239400170905     c                   eval      boldat=datapiu
239500170905     c                   eval      bollnp=lnppiu
239600170905     c                   when      datameno>datapiu
239700170905     c                   eval      boldat=datameno
239800170905     c                   eval      bollnp=lnpmeno
239900170905     c                   other
240000170905     c                   eval      boldat=datameno
240100170905     c                   eval      bollnp=lnpmeno
240200170905    3c                   endsl
240300170905     c                   if        boldat>0 and boldat>=oxgldata
240400170905     c                   eval      lnpb=bollnp
240500170905     C                   MOVEL     BARDFV        DSPB
240600170905     C                   MOVEL     BARDFV        WDTRIF
240700170905     c                   endif
240800170905     c
240900170904    2c                   endif
241000170904    1c                   endif
241100170901     c
241200960702     C                   ENDSR
241300170905     C**************************************************************************
241400170905     c* Elaboro il manca record cercando chi può averlo fatto partire
241500170905     C**************************************************************************
241600170905     c     CTR_nsc       BEGSR
241700170905     c                   clear                   boldat            8 0
241800170905     c                   clear                   bollnp            3 0
241900170905     c* controllo se la bolla appartiene a segnacollo in manca record
242000170905     c     kblp29        chain(n)  fnblp01l
242100170905    4c                   if        %found(fnblp01l)
242200170905     c                   clear                   trulnrsds
242300170905     c                   eval      INRSFLS =barlnp
242400170905     c                   eval      INRSnrs =barnrs
242500170905     c                   eval      INRSncd =barnsc
242600170905    5c                   if        blpccm>0
242700170905     c                   eval      INRSccm =blpccm
242800170905     c                   else
242900170905     c                   eval      INRSccm =blpksc
243000170905    5c                   endif
243100170905     c                   call      'TRULNRSR'
243200170905     c                   parm                    trulnrsds
243300170905
243400170905    5c                   if        ONRSERR  =' '
243500170905     c                   eval      bollnp=blplnp
243600170905    6c                   select
243700170905     c                   when      b29dse>0
243800170905     c                   eval      boldat=b29dse
243900170905     c                   when      b29dfv>0
244000170905     c                   eval      boldat=b29dfv
244100170905     c                   other
244200170905     c                   eval      boldat=wdate1
244300170905    6c                   endsl
244400170905    5c                   endif
244500170905    4c                   endif
244600170905     c                   ENDSR
244700941206     C**************************************************************************
244800941206     C* CERCO IL RAGGRUPPAMENTO PISTOLA
244900941206     C**************************************************************************
245000921229     C     RICRAG        BEGSR
245100921229     C*
245200930305     C     SAVNPS        IFNE      ALF2
245300921229     C*
245400950515     C* HO CARICATO TUTTA SCHIERA
245500921229     C     *IN02         IFEQ      '0'
245600970417     C                   Z-ADD     1             Z                 3 0
245700930305     C                   MOVEL     ALF2          NUM2              2 0
245800970417     C     NUM2          LOOKUP    NPS(Z)                                 34
245900970417     C  N34              Z-ADD     1             Z
246000921229     C*
246100921229     C                   ELSE
246200921229     C*
246300921229     C* NON HO CARICATO LA SCHIERA --> FACCIO CHAIN
246400921229     C                   MOVEL     '7J'          COD
246500921229     C                   MOVEL     *BLANKS       KEY
246600930305     C                   MOVEL     ALF2          KEY
246700921229     C     KTAB          CHAIN     TABEL                              34
246800001023     C                   Z-ADD     2             Z
246900000316     C     *IN34         IFEQ      *OFF
247000000316     C                   MOVEL     TBLUNI        DS7J
247100000316     C                   MOVEL     §7JRPS        RPS(Z)
247200000316     C                   MOVEL     §7JORD        OPS(Z)
247300000316     C                   MOVEL     §7JTPS        TPS(Z)
247400000316     C                   MOVEL     §7JINV        INV(Z)
247500040503     C                   MOVEL     §7Jfsl        fsl(Z)
247600000316     C                   ELSE
247700000316     C                   MOVEL     'M'           RPS(Z)
247800000316     C                   MOVEL     999           OPS(Z)
247900000316     C                   MOVEL     'B'           TPS(Z)
248000000316     C                   MOVEL     ' '           INV(Z)
248100040503     C                   MOVEL     ' '           fsl(Z)
248200000316    1C                   END
248300921229     C                   END
248400921229     C*
248500930305     C                   MOVEL     ALF2          SAVNPS
248600921229     C                   END
248700941206     C*
248800921229     C                   ENDSR
248900941206     C**************************************************************************
249000941206     C* CHIUDO ANOMALIA
249100941206     C**************************************************************************
249200911018     C     CHIUAN        BEGSR
249300961211     C                   SETOFF                                       97
249400941206     C*
249500990518     C     KANM          CHAIN(N)  FNANM000                           97
249600961121     C**
249700961211    1C     *IN97         IFEQ      *OFF
249800970117     C**
249900970117     C** ANOMALIA 145/146: SE DATA SPUNTA > DATA ANOAMLIA
250000970117     C*  DEVO CONTROLLARE CHE NON CI SIANO ALTRE SPUNTE
250100970117     C*  CHE MI IMPEDISCONO LA CHIUSURA
250200970117     C     ANMCAA        IFEQ      145
250300970117     C     ANMCAA        OREQ      146
250400041013     c* Imposto econtrdfv per anomalie 145 146 che la deve sempre cotrollare
250500041013     c                   eval      wcontrdfv='S'
250600990518     C     BARDFV        IFGT      ANMDAO
250700970117     C                   EXSR      CTRSPU
250800970117     C                   ENDIF
250900970117     C                   ENDIF
251000961121     C**
251100041013     c     Wcontrdfv     ifne      'S'
251200041013     C* PER CATEGORIA no arrivi e partenza CHIUDO SE DATA >=
251300961122     C     WSTESS        OREQ      'S'
251400990518     C     BARDFV        ANDEQ     ANMDAO
251500041013     C     BARNPG        ANDne     2
251600041013     C     BARNPG        ANDne     1
251700041013     C* PER LE CATEGORIE arrivi SOLO SE DATA > e la posso chiudere
251800990518     C     BARDFV        ORGT      ANMDAO
251900970117     C     WCHIU         ANDEQ     'S'
252000961211     C*
252100961211     C     WCREA         OREQ      'N'
252200020213     C     BARFLE        ANDNE     BTFP
252300961121     C**
252400960416     C                   CLEAR                   DSLR33
252500960524     C                   MOVE      BARDFV        D33DCH
252600960514    2C     BARKEY        IFNE      SAVFOG
252700911125     C                   EXSR      RICFAS
252800960514    2C                   END
252900960514     C*
253000960416     C                   MOVEL     COMFAS        D33FSC
253100960416     C                   MOVEL     SAVCCH        D33CCH
253200960530     C                   MOVEL     SAVMAC        D33MAC
253300960416     C                   MOVE      ANMNR2        D33NRR
253400960416     C*
253500990712     C                   CALL      'FNLR33R'
253600960416     C                   PARM                    DSLR33
253700961209     C*
253800961209     C* PER ANOMALIE 145 E 146 CREO SE E' DA CREARE L'ANOMALIA 620
253900020212     C*  AOLO SE NON SONO IN CREAZIONE DELL 600
254000020212    2C     NTWBAR        IFEQ      'EEX'
254100961210     C     W620          ANDEQ     'S'
254200060322    2C     NTWBAR        oreq      'DPD'
254300060322     C     W620          ANDEQ     'S'
254400060322    2C     NTWBAR        oreq      'FED'
254500060322     C     W620          ANDEQ     'S'
254600961210     C*
254700961210    3C     ANMCAA        IFEQ      145
254800961209     C     ANMCAA        OREQ      146
254900961210     C*
255000961210    4C     W60X          IFEQ      'S'
255100961210     C                   Z-ADD     600           COMCAA
255200001110     C     COMCAA        LOOKUP    ACG                                    34
255300001110    5C     BARSPG        IFNE      'I'
255400001110     C     *IN34         OREQ      *ON
255500990518     C     KANM          DELETE    FNANM02L                           34
255600001110    5C                   ENDIF
255700961209     C                   Z-ADD     620           COMCAA
255800961209     C                   EXSR      CRTANM
255900961209     C                   CLEAR                   W60X
256000961210    4C                   ENDIF
256100961209     C*
256200961209     C* VEDO SE CANCELLARE L'ANOMALIA 645
256300961209     C                   Z-ADD     645           COMCAA
256400001110     C     COMCAA        LOOKUP    ACG                                    34
256500001110   4AC     BARSPG        IFNE      'I'
256600001110     C     *IN34         OREQ      *ON
256700990518     C     KANM          CHAIN     FNANM02L                           34
256800961210    4C     *IN34         DOWEQ     *OFF
256900990518    5C     ANMDAO        IFEQ      BARDFV
257000990518     C                   DELETE    FNANM000
257100961209     C                   SETON                                        34
257200961209     C                   ELSE
257300990518     C     KANM          READE     FNANM02L                               34
257400961210    5C                   ENDIF
257500961210    4C                   ENDDO
257600001110   4AC                   ENDIF
257700961210    3C                   ENDIF
257800961209     C*
257900961210    2C                   ENDIF
258000960514     C*
258100960514     C* L'ANOMALIA 05 PUO' ESSERCI PIU' VOLTE
258200961209    2C     ANMCAA        IFEQ      005
258300980709     C                   SETOFF                                       34
258400960514    3C     *IN34         DOWEQ     *OFF
258500990518     C     KANM          READE(N)  FNANM000                               34
258600960514     C                   MOVE      ANMNR2        D33NRR
258700960514     C*
258800960514     C  N34              CALL      'FNLR33R'
258900960514     C                   PARM                    DSLR33
259000960514    3C                   ENDDO
259100911018     C*
259200960514    2C                   ENDIF
259300961121   1AC                   ENDIF
259400961209   X1C                   ELSE
259500961209     C*
259600961211     C* SOLO PER ANOMALIE 145 E 146
259700961210    2C     COMCAA        IFEQ      145
259800961210     C     COMCAA        OREQ      146
259900991230     C* SE DEVO CREARE EVENTUALE 620 PER LNA ESTERA
260000020212    3C     W60X          IFEQ      'S'
260100961210     C     W620          ANDEQ     'S'
260200060322     C* E LA DEVO CREARE SIA SE TROVO CHE SE NON TROVO L'ANOMALIA 145
260300060322     C*  O 146 DA CHIUDERE  (WESAN =' ')
260400060322     C     WESAN         ANDEQ     ' '
260500110103     c*
260600060322    4C     NTWBAR        ifeq      'EEX'
260700060322     C     NTWBAR        oreq      'DPD'
260800060322     C     NTWBAR        oreq      'EEX'
260900961209     C                   Z-ADD     620           COMCAA
261000961209     C                   EXSR      CRTANM
261100961209     C                   CLEAR                   W60X
261200060322    4C                   ENDIF
261300060322    3C                   ENDIF
261400961210    2C                   ENDIF
261500961210    1C                   ENDIF
261600110103     c*
261700041013     c                   eval      wcontrdfv='N'
261800911018     C*
261900110929     c                   clear                   savmac
262000110929     c                   clear                   savcch
262100911018     C                   ENDSR
262200941206     C**************************************************************************
262300941206     C*    RICERCA FASE
262400941206     C**************************************************************************
262500911125     C     RICFAS        BEGSR
262600941206     C*
262700921020     C                   MOVEL     '7N'          COD
262800921020     C                   MOVEL     *BLANKS       KEY
262900921020     C                   MOVEL     BARNPG        KEY
263000921105     C     KTAB          CHAIN     TABEL                              41
263100921105     C  N41              MOVEL     TBLUNI        DS7N
263200000516     C*
263300000516     C     BARSPG        IFNE      ' '
263400000516     C                   MOVEL     §7NFS1        WFAS1             1
263500000516     C                   MOVEL     §7NFS2        WFAS2             1
263600000516     C                   MOVEL     §7NFS3        WFAS3             1
263700000516     C                   MOVEL     §7NFS4        WFAS4             1
263800000516     C                   MOVEL     §7NFS5        WFAS5             1
263900000516     C                   ENDIF
264000921021     C*
264100960524     C     BARSPG        IFEQ      §7NSC1
264200921021     C                   MOVEL     §7NFS1        COMFAS            1
264300921021     C                   ELSE
264400960524     C     BARSPG        IFEQ      §7NSC2
264500921021     C                   MOVEL     §7NFS2        COMFAS
264600921021     C                   ELSE
264700960524     C     BARSPG        IFEQ      §7NSC3
264800921021     C                   MOVEL     §7NFS3        COMFAS
264900921021     C                   ELSE
265000960524     C     BARSPG        IFEQ      §7NSC4
265100921021     C                   MOVEL     §7NFS4        COMFAS
265200921021     C                   ELSE
265300921021     C                   MOVEL     §7NFS5        COMFAS
265400921021     C                   END
265500921021     C                   END
265600921021     C                   END
265700921021     C                   END
265800921020     C*
265900961217     C                   MOVEL     BARKEY        SAVFOG            8 0
266000941206     C*
266100911125     C                   ENDSR
266200950927     C**************************************************************************
266300950927     C*    CONTROLLO SE CREARE ANOMALIA 50: COLLO ARRIVATO NON PARTITO
266400130228     C*                      O ANOMALIA 55/56/57: DISGUIDO - ESTERNA
266500950927     C**************************************************************************
266600950927     C     CRTA50        BEGSR
266700000320     C                   CLEAR                   WCCH
266800020214     C* PER ANOMALIA 50 NON CREO SE ESISTE GIA' LA DATA DI PARTENZA
266900020214     C*  BOLLE TRANSITO
267000020214     C     WTIBOL        IFEQ      'T'
267100020214     C     COMDFV        ANDGT     0
267200020214     C                   GOTO      NOA50
267300020214     C                   ENDIF
267400020214     C**
267500020214     C     WTIBOL        IFEQ      'A'
267600020218     C     WTIBOL        OREQ      'L'
267700050215     c* Per l'anoamlia 50 basterebbe questo test senza leggere le spunte,
267800050215     c*  le spunte le devo leggere lo stesso per i manca record
267900020214     C     WESART        IFEQ      'S'
268000020214     C     COMDFV        ANDGT     0
268100020214     C                   GOTO      NOA50
268200020214     C                   ENDIF
268300020214     C* SE E' COLLO IN ARRIVO E BOLLA CONSEGNATA --> NON CREO
268400020214     C     WESART        IFEQ      'S'
268500020214     C     ARTDCM        ANDGT     0
268600020214     C                   GOTO      NOA50
268700020214     C                   ENDIF
268800020218     C* SE BOLLA LOCALE E NON C'E' LA BOLLA NON FACCIO NIENTE
268900020218     C     WTIBOL        IFEQ      'L'
269000020218     C     WBOPAR        ANDEQ     'N'
269100020218     C                   GOTO      NOA50
269200020218     C                   ENDIF
269300020214     C                   ENDIF
269400020214     C**
269500950927     C* VERIFICO SE ESISTE GIA'
269600040429     c* ora la esistenza è all'interno dello stesso p.o. e non in assoluto
269700950927    1C     WTIBOL        IFEQ      'D'
269800950927     C                   Z-ADD     55            COMCAA
269900950927     C                   ELSE
270000950927     C                   Z-ADD     50            COMCAA
270100950927    1C                   ENDIF
270200990409     C* SE LA CATEGORIA E' IMA IMG O CONS INVECE CHE 55 CREO LA 56
270300020214    1C     COMCAA        IFEQ      55
270400020214    2C     BARNPG        IFEQ      4
270500990412     C     BARNPG        OREQ      3
270600990409     C     BARSPG        ANDEQ     'A'
270700990412     C     BARNPG        OREQ      3
270800990409     C     BARSPG        ANDEQ     'G'
270900161021     C     BARNPG        OREQ      3
271000161021     C     BARSPG        ANDEQ     'Z'
271100990409     C                   Z-ADD     56            COMCAA
271200020214    2C                   ENDIF
271300020214    1C                   ENDIF
271400130228     c* se Anomalia 55 a collo che può partire da diverse filiali
271500130228     c*  e chi spunta è una di qeuste --> non creo 55 ma 57 Disguido AUTORIZZ
271600130228     c*  solo per ategorie di spunta partenza
271700130228     c                   if        comcaa=55  and
271800130228     c                             (bolxco='W' or
271900130228     c                             (wesbtt='N' and wesblt='N' and wesart='N'
272000130228     c                              and barnrs>0))
272100130228     c* se si tratta di spunta partenza --> creo la 57
272200130228     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
272300130228     c                             or barnpg=1
272400130228     c                   exsr      CtrXCO
272500130228     c                   if        Indx>0
272600130301     c                   z-add     57            comcaa
272700130228     c                   endif
272800130228     c                   endif
272900130228     c                   endif
273000130301     c
273100130301     c* In caso di anomalia 50 manca bolla --> verifico se non è da creare
273200130301     c*  per collo spuntabile in questo TFP e "locale"
273300130301     c                   if        comcaa=50 and
273400130301     c                             (bolxco='W' or
273500130301     c                             (wesbtt='N' and wesblt='N' and wesart='N'
273600130301     c                              and barnrs>0))
273700130301     c* se si tratta di spunta di partenza --> non creo la 50 ma la 57
273800130301     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
273900130301     c                             or barnpg=1
274000130301     c                   exsr      CtrXCO
274100130301     c                   if        Indx>0
274200130301     c                   z-add     57            comcaa
274300130301     c                   endif
274400130301     c                   endif
274500130301     c                   endif
274600971009     C*
274700130228     c* per anomalia 55 o 56 57 valida l'anomalia per p.o. spunta
274800040429     c* per anomalia 50:
274900040429     c* per bolle transito solo se spara il p.o. transito
275000040429     c* per bolle arrivi, se trovo anomalia nell'area di arrivo(e non
275100040429     c*  di un eventuale p.o. transito)
275200040429     c                   clear                   wtrova
275300040429     C     KANM          SETLL     FNANM000
275400040504     C     KANM          reade(N)  FNANM000                               34
275500040429    1c                   dow       not*in34 and wtrova=*blanks
275600040429    2c                   if        anmcaa<>50 or wtibol='T'
275700040429    3c                   if        anmfle=bpes
275800040429     c                   eval      wtrova='1'
275900040429   x3c                   else
276000040504     C     KANM          reade(N)  FNANM000                               34
276100040429    3c                   endif
276200110103     c*
276300040429   x2c                   else
276400040429     C                   CLEAR                   DSLV55
276500040429     C                   MOVEL     'A'           D55TPT
276600040429     C                   Z-ADD     anmfle        D55LIN
276700040429     C                   Z-ADD     lnpb          D55lnp
276800040429     C                   Z-ADD     bardfv        D55DRF
276900040429     C                   EXSR      FNLV55
277000040429    3c                   if        d55tfa=pestfa
277100040429     c                   eval      wtrova='1'
277200040429   x3c                   else
277300040504     C     KANM          reade(N)  FNANM000                               34
277400040429    3c                   endif
277500040429    2c                   endif
277600110103     c*
277700040429    1c                   enddo
277800990412     C*
277900990412     C* PER ANOMALIA 55 VERIFICO ANCHE CHE NON CI SIA LA 56
278000990412     C*  PERCHE' DOPO AVER SPARATO UN COLLO IN IMA PUO' ESSERE
278100990412     C*  SPARATO IN PARTENZA PER FARLO RIPARTIRE
278200040429    1C     *IN34         IFEQ      *On
278300990412     C     COMCAA        ANDEQ     55
278400990412     C                   Z-ADD     56            COMCAA
278500040429     C     KANM          SETLL     FNANM000
278600040504     C     KANM          reade(N)  FNANM000                               34
278700040429     c                   dow       not*in34 and wtrova=*blanks
278800040429     c                   if        anmfle=bpes
278900040429     c                   eval      wtrova='1'
279000040429     c                   else
279100040504     C     KANM          reade(N)  FNANM000                               34
279200040429     c                   endif
279300040429     c                   enddo
279400990412     C                   Z-ADD     55            COMCAA
279500990412    1C                   ENDIF
279600950927     C*
279700040429    1C     *IN34         IFEQ      *On
279800970828     C* NON CREO ANOMALIA 55 SE C'E' UNA SPUNTA PARTENZA DI ALTRO AS
279900970828     C*  PERCHE' NON SI TRATTA PIU' DI UN DISGUIDO MA O E' COLLO
280000990409     C*  IN TRANSITO DI CUI NON HO RICEVUTO LA BOLLA
280100990409     C*  O E' COLLO DISGUIDATO IN ALTRA FIL CHE PER ANDARE IN ARRIVO
280200970828     C*  DEVE PASSARE DA ALTRA FIL ANCORA
280300970828     C*  LA 200 VIENE INVECE CREATA LO STESSO PERCHE' UNA ANOMALIA
280400970828     C*  GESTIONALE CHE SERVE PER FAR RIPARTIRE AUTOMATICAMENTE IL COLL
280500970828     C*  ANCHE SE NON VIENE SPARATO NE' IN PARTENZA NE' IN ENTRATA
280600000316     C                   EXSR      CNOIDD
280700950927    1C                   ENDIF
280800020214     C     NOA50         ENDSR
280900000316     C**************************************************************************
281000000316     C* RIEMPO ALCUNI CAMPI ANOMALIA NO IDD
281100000316     C**************************************************************************
281200000316     C     CNOIDD        BEGSR
281300160624     C** ANOMALIE 50 -->
281400160725     C* VERIFICO SE ESISTE UNA SPUNTA PARTENZA PER QUEL COLLO
281500160624     C*  SE SI NON CREO ANOMALIA PER 50
281600130228     C*  ANOMALIE 55/56/57 --> CONSIDERO SOLO LA SPUNTA "D-DISGUIDO"
281700020214     C*                     PER NON CREARLE
281800000316     C                   Z-ADD     1             WNPG
281900000317     C                   CLEAR                   WTROVA
282000021121     C* CAMPI DI COMODO PER ANOM 50 per transito su stessa £1
282100021121     C                   CLEAR                   WFVALNP
282200021121     C                   CLEAR                   WFVANFV
282300021121     C                   CLEAR                   WFVADFV
282400110103     c*
282500070131     C     KBAR3         CHAIN     FNBRV09L                           99
282600020214    1C     *IN99         DOWEQ     *OFF
282700000317     C     WTROVA        ANDEQ     ' '
282800050215     c                   clear                   wrileggi          1
282900020214     C*****
283000020214     C* SOLO SE LA SPUNTA TROVATA E' DI DISGUIDO, ALTRIMENTI SE NORMALE
283100020214     C*  E' UNA SPUNTA PREVISTA, QUINDI DA IGNORARE
283200020214    2C     COMCAA        IFEQ      55
283300020214     C     COMCAA        OREQ      56
283400130228     C     COMCAA        OREQ      57
283500040303    3C     BR9FGS        IFNE      BTFP
283600040429     c* verifico tramite pgm fnlv53r se spunta di foglio diretto a me(BPES)
283700040429     c*  se è diretto a me non creo anomalia
283800040429     c*  Questo vale sia per colli in disguido che colli in transito
283900040429     c*  manca record bolla partenza
284000040430     c                   clear                   dslv53
284100040504     c                   movel     'T'           d53tfo
284200040429     C                   MOVEL     BR9NPG        D53NPG
284300040429     C                   Z-ADD     BR9NFV        D53NFV
284400040429     C                   MOVEL     BR9FGS        D53FGS
284500040429     C                   MOVEL     BR9FLE        D53FLF
284600040504     C                   MOVEL     'A'           D53ABB
284700040429     C                   MOVEL     br9lna        D53LNA
284800050603     C                   MOVEL     Usalnatfa     D53TFA
284900040504     C                   MOVEL     bpes          D53lai
285000040429     C                   CALL      'FNLV53R'
285100040429     C                   PARM                    DSLV53
285200040429     c* se non ci sono errori --> OK esco dal ciclo
285300040429    4c                   if        d53err=*blanks
285400000317     C                   MOVEL     '1'           WTROVA            1
285500050418   x4c                   else
285600050418     C                   eval      wrileggi='1'
285700040429    4c                   endif
285800020214   X3C                   ELSE
285900050215     C                   eval      wrileggi='1'
286000020214    3C                   ENDIF
286100020214   X2C                   ELSE
286200020214     C**
286300031204     c** bolla non locale: cerco spunta non mia
286400031204   2ac     WSPLOC        ifeq      *blanks
286500040303    3C     BR9FGS        IFNE      BTFP
286600050329     c* Deve essere una spunta trasmessa, altrimenti ignoro (scaricata
286700050329     c*  tardi)
286800050329     c     br9ftr        andne     *blanks
286900110103     c*
287000050222    4c                   if        comcaa=50
287100050215     c                   clear                   dslv53
287200050322     c                   if        wtibol='T'
287300050322     c                   movel     'T'           d53tfo
287400050322     c                   else
287500050215     c                   movel     'A'           d53tfo
287600050322     c                   endif
287700050215     C                   MOVEL     BR9NPG        D53NPG
287800050215     C                   Z-ADD     BR9NFV        D53NFV
287900050215     C                   MOVEL     BR9FGS        D53FGS
288000050215     C                   MOVEL     BR9FLE        D53FLF
288100050215     C                   MOVEL     'A'           D53ABB
288200050215     C                   MOVEL     br9lna        D53LNA
288300050603     C                   MOVEL     Usalnatfa     D53TFA
288400050215     C                   MOVEL     pestfa        D53lai
288500050215     C                   CALL      'FNLV53R'
288600050215     C                   PARM                    DSLV53
288700050215     c* se non ci sono errori --> OK esco dal ciclo
288800050222    5c                   if        d53err=*blanks
288900020214     C                   MOVEL     '1'           WTROVA            1
289000050222   X5C                   ELSE
289100050418     c* Se si tratta di errore di foglio non trovato e provengo da
289200050418     c*  aggiornamento bolla transito Ex disguido (richiamo da fnlr13r)
289300050418     c*  il foglio si potrebbe trovare in fnfva01r, er cui conttrollo da
289400050418     c*  FNFGV
289500070221    6c                   if        d53err='2' and dls45FLG='T'
289600050418     c                   movel     'G'           d53tfo
289700050418     C                   CALL      'FNLV53R'
289800050418     C                   PARM                    DSLV53
289900050418     c* se non ci sono errori --> OK esco dal ciclo
290000050418    7c                   if        d53err=*blanks
290100050418     C                   MOVEL     '1'           WTROVA            1
290200050418   x7c                   else
290300050418     C                   eval      wrileggi='1'
290400050418    7c                   endif
290500050418   x6c                   else
290600050418     C                   eval      wrileggi='1'
290700050418    6c                   endif
290800050222    5C                   ENDIF
290900110103     c*
291000050222     c* Per anom 60/61 se trovata spunta --> ok
291100160624   X4C**                 ELSE
291200160624     C**                 MOVEL     '1'           WTROVA            1
291300050222     c                   endif
291400110103     c*
291500020214   X3C                   ELSE
291600050215     C                   eval      wrileggi='1'
291700020214    3C                   ENDIF
291800110103     c*
291900031204   2ac                   else
292000031204     c** bolla     locale: cerco spunta mia
292100040303    3C     BR9FGS        IFeq      BTFP
292200031204     C                   MOVEL     '1'           WTROVA            1
292300031204   X3C                   ELSE
292400050215     C                   eval      wrileggi='1'
292500031204    3C                   ENDIF
292600031204   2aC                   ENDIF
292700031204    2C                   ENDIF
292800050215     c
292900050215     c                   if        wrileggi='1'
293000070131     C     KBAR3         READE     FNBRV09L                               99
293100050215     c                   endif
293200020214    1C                   ENDDO
293300160725     c*
293400160725     c* Se non trovata e si tratta di bolla locale: cerco spunta
293500160725     c*  entrata
293600160725     c                   if        wtrova=' ' and wsploc=*blanks
293700160725     C                   Z-ADD     5             WNPG
293800160725     C     KBAR3         chain     FNBRV09L
293900160725     c                   if        %found(fnbrv09l) and br9fgs=btfp
294000160725     C                   MOVEL     '1'           WTROVA            1
294100160725     c                   setoff                                       99
294200160725     c                   endif
294300160725     c                   endif
294400000316     C*
294500000316     C* 99 ON  - SPUNTA NON TROVATA --> CREO ANOMALIA PER 50/55/56
294600000316    0C     *IN99         IFEQ      *ON
294700000316     C     COMCAA        ANDNE     60
294800000614     C     COMCAA        ANDNE     61
294900000316     C* 99 OFF - SPUNTA     TROVATA --> CREO ANOMALIA PER 60
295000160624     C**   *IN99         OREQ      *OFF
295100160624     C**   COMCAA        ANDEQ     60
295200160624     C**   *IN99         OREQ      *OFF
295300160624     C**   COMCAA        ANDEQ     61
295400000316     C                   CLEAR                   FNANM000
295500170505     c***
295600000316     C* MEMORIZZO NUMERO SPEDIZIONE SU ANOMALIA SE C'E'
295700170505    1C***                SELECT
295800170505     C***  WESBTT        wheneq    'S'
295900170505     C***                Z-ADD     BTTAAS        ANMAAS
296000170505     C***                Z-ADD     BTTLNP        ANMLNP
296100170505     C***                Z-ADD     BTTNSP        ANMNSP
296200170505     C***  WESART        wheneq    'S'
296300170505     C***                Z-ADD     ARTAAS        ANMAAS
296400170505     C***                Z-ADD     ARTLNP        ANMLNP
296500170505     C***                Z-ADD     ARTNSP        ANMNSP
296600170505     C***  WESblt        wheneq    'S'
296700170505     C***                Z-ADD     bltAAS        ANMAAS
296800170505     C***                Z-ADD     bltLNP        ANMLNP
296900170505     C***                Z-ADD     bltNSP        ANMNSP
297000170505    1C***                ENDSL
297100170505
297200170505     c                   if        comnsp>0
297300170505     C                   Z-ADD     comAAS        ANMAAS
297400170505     C                   Z-ADD     comLNP        ANMLNP
297500170505     C                   Z-ADD     comNSP        ANMNSP
297600170505     c                   endif
297700040621     c* Se c'e' la spedizione verifico se spedizione di valore
297800110103     c*
297900040621     C* linea partenza segnacollo
298000000316     C                   MOVEL     BARLNP        ANMFLS
298100000316     C* PER ANOMALIA 50
298200000316    1C     COMCAA        IFEQ      50
298300000316     C                   Z-ADD     BARFGS        ANMFLE
298400040908     C* PER BOLLA LOCALE non c'e' abbinamento di foglio per cui deduco le
298500040908     c*  date damettere sulla anomalia
298600040908     c                   clear                   wnofva            1
298700040908     c                   if        wtibol='L'
298800040908     c                   eval      wnofva='S'
298900040908     c                   endif
299000170505     c* Per sicurezza, per essere sicura di non aver sporcato i campi di ARB
299100170505     c*  uso i campi salvati
299200170505     c****               if        wesart='S' and arbtfp=arbtfa
299300170505     c                   if        wesart='S' and lnptfp=boltfa
299400040908     c                   eval      wnofva='S'
299500040908     c                   endif
299600050603     c                   if        wesart='N' and lnptfp=Usalnatfa
299700040908     c                   eval      wnofva='S'
299800040908     c                   endif
299900040908     c
300000040908   1ac                   if        wnofva='S'
300100040908    2c                   if        wesart='S'
300200170509     C                   Z-ADD     bolDBR        SAVDAO
300300170509     C                   Z-ADD     bolDBR        ANMDAO
300400020218     C                   Z-ADD     99998         ANMNFV
300500020218     C                   Z-ADD     LNPTFP        ANMCDU
300600020218     C                   Z-ADD     LNPTFP        WFL1
300700020218     C                   MOVEL     'L'           WFLGT
300800040908     c*
300900040908   x2c                   else
301000040908     c* Se non c'e' bolla cerco il 1° foglio locale con data < data spunta
301100040908     c     kfgv2         setll     fnfgv02l
301200040908     c     lnptfp        readpe    fnfgv02l
301300040908    3c                   dow       not %eof(fnfgv02l)  and
301400040908     c                             fgvttr <>'L'
301500040908     c     lnptfp        readpe    fnfgv02l
301600040908    3c                   enddo
301700110103     c*
301800040908    3c                   if        not %eof(fnfgv02l)
301900040908     C                   Z-ADD     fgvdfv        SAVDAO
302000040908     C                   Z-ADD     fgvdfv        ANMDAO
302100040908     C                   Z-ADD     99998         ANMNFV
302200040908     C                   Z-ADD     LNPTFP        ANMCDU
302300040908     C                   Z-ADD     LNPTFP        WFL1
302400040908     C                   MOVEL     'L'           WFLGT
302500040908    3c                   endif
302600040908    2c                   endif
302700110103     c*
302800040908  x1aC                   ELSE
302900070208     C* SE TROVA LA BOLLA IN BLP C'E' SEMPRE LNP BOLLA
303000020214     C*  ALTRIMENTI LA 5F ALTRIMENTI BARLNP
303100020214     C                   MOVEL     LNPB          WLIN
303200020214     C* IMPOSTO LA DATA DI RIFERIMENTO PER LA RICERCA DEL TERMINAL
303300020214     C*  DI PARTENZA: LA PRIMA VOLTA E' LA DATA DELLA BOLLA SE C'E'
303400020214     C                   MOVE      DSPB          WDTRIF
303500000316     C*
303600000316     C* CERCO IL NUMERO DI FV SU CUI DOVREBBE ESSERE PARTITO IL COLLO
303700000316     C                   MOVE      *BLANKS       WFLGT             1
303800000316     C                   CLEAR                   WLINPR
303900000316     C                   CLEAR                   WVALNP
304000000316    2C     WLIN          DOWGT     0
304100000316     C                   EXSR      RICFV
304200000316     C* MEMORIZZO IL P.O. DI TRANSITO E LA LINEA DI PARTENZA DEL FV
304300000316     C     WLIN          IFGT      *ZEROS
304400000316     C                   Z-ADD     WLIN          WLINPR
304500000316     C                   Z-ADD     FVALNP        WVALNP
304600000316     C                   ENDIF
304700000316    2C                   ENDDO
304800000316    2C     ANMCDU        IFGT      0
304900000316     C                   MOVEL     ANMCDU        WFL1
305000000316   X2C                   ELSE
305100070208     C* SE TROVA LA BOLLA IN BLP C'E' SEMPRE LNP BOLLA
305200020214     C*  ALTRIMENTI LA 5F ALTRIMENTI BARLNP
305300000316     C                   MOVEL     LNPB          WFL1
305400000316    2C                   ENDIF
305500020218   1AC                   ENDIF
305600000316     C*
305700000316   X1C                   ELSE
305800160624     C* PER ANOMALIA 55/56/57/      (NO IDD LE ALTRE)
305900000316     C                   Z-ADD     BARDFV        ANMDAO
306000000316     C                   MOVEL     BARFGS        ANMFLE
306100000316     C                   MOVEL     LNPB          WFL1
306200000316     C                   Z-ADD     BARNFV        ANMNFV
306300000316     C                   MOVEL     BARFGS        ANMCDU
306400000316    1C                   ENDIF
306500000316     C* SCRIVO L'ANOMALIA
306600000316     C                   EXSR      RIEANM
306700040621     c*
306800040621     c                   if        anmnsp>0
306900091209     c     kar5          chain(n)  fiar501l
307000040621    3c                   if        %found(fiar501l)
307100040621     c                   movel     ar5uni        dar5gen
307200040621     c                   else
307300040621     c                   clear                   dar5gen
307400040621    3c                   endif
307500040621     c                   movel     §ar5bva       anmft4
307600040621     c                   endif
307700000614     C*
307800000316     C* ESTERNA
307900000614    1C     §7CAIE        IFEQ      'E'
308000000316     C                   MOVEL     WFL1          ANMFL1
308100000614    1C                   ENDIF
308200000316     C* SCRIVO L'ANOMALIA SOLO SE HO TROVATO IL FOGLIO PARTENZA PER"50"
308300000614    1C     ANMCAA        IFNE      50
308400000316     C     ANMCAA        OREQ      50
308500000316     C     ANMNFV        ANDGT     0
308600000316     C* SE LA KEY E' PER SEGNACOLLO ED IL NUMERO E' 0 NON CREO L'ANOMAL
308700000316    2C     §7CKEY        IFNE      'S'
308800000316     C     §7CKEY        OREQ      'S'
308900000316     C     ANMSCN        ANDGT     0
309000001110     C************************
309100001110     C* CREO ANOMALIA SOLO SE:
309200001110     C************************
309300070215     C* ANOMALIA CREABILE ANCHE DA CAT.SPUNTA GENERICA
309400070215     C*                                 (ANCHE DA SPUNTA IMI)
309500070215     C     §7CCCG        ifeq      'S'
309600070215     C* CATEGORIA SPUNTA NON GENERICA   (NON È UNA SPUNTA IMI)
309700070215     C     BARSPG        orne      'I'
309800000316     C                   WRITE     FNANM000
309900001110     C*
310000001110     C* CHIUDO SE POSSIBILE UNA EVENTUALE 55 GIA' CREATA E POI CREO
310100001110     C*  LA 56
310200001110    3C     COMCAA        IFEQ      56
310300001110     C                   Z-ADD     55            COMCAA
310400001110     C                   MOVEL     'S'           SAVMAC
310500001110     C                   EXSR      CHIUAN
310600001110     C                   CLEAR                   SAVMAC
310700001110    3C                   ENDIF
310800000614    3C                   ENDIF
310900000614     C*
311000000316     C* PER ANOMALIA 50 CREATA AGGIORNO ANCHE LA DATA DI PARTENZA
311100000316     C* O LA DATA DI USCITA DAL TRANSITO
311200000316     C     ANMCAA        IFEQ      50
311300000316     C                   EXSR      AGGPAR
311400000316     C                   ENDIF
311500160727     C     ANMCAA        IFEQ      55
311600160727     C     ANMCAA        oreq      56
311700160727     C                   EXSR      AGGPAR_d
311800160727     C                   ENDIF
311900000316     C*
312000000316    2C                   ENDIF
312100020222     C*
312200020222     C                   ELSE
312300020222     C* SE NON TROVATO FOGLIO ATTENDO CHE ARRIVI (CI DEVE ESSERE PRIMA
312400031204     C*  O POI) se non locale
312500060103     c* Solo se non richiamato
312600060103     c  N02wtibol        ifne      'L'
312700020222     C                   SETON                                        3940
312800031204     c                   endif
312900000614    1C                   ENDIF
313000000614    0C                   ENDIF
313100000316     C                   ENDSR
313200130228     C**************************************************************************
313300130228     C* controllo se bolla con colli chde partono da divderse filiali
313400130228     C**************************************************************************
313500130228     C     CtrXCO        BEGSR
313600130228     c                   eval      Wflsnrs=(BARLNP*100)+BARnrs
313700130228
313800130228    1c                   if        wflsnrs<>savfnrs
313900130228     c                   clear                   xcotfp
314000130228     C                   MOVEL     'XCO'         kcod
314100130228     C                   eval      kke1='ALTRITFP'
314200130228     C                   eval      kke2=%editc(wflsnrs:'X')
314300130228     c     ktbe2         chain     tntbe01l
314400130228    2c                   if        %found(tntbe01l) AND tbeatb=' '
314500130228     C                   MOVEA     TBeUNI        xcotfp
314600130228    2c                   endif
314700130228     c
314800130228     c                   eval      savfnrs=wflsnrs
314900130228    1c                   endif
315000130228    c
315100130228     C* SE la fil spunte è compresa --> creo la 57
315200130228     c                   movel     barfgs        alfafgs           3
315300130228     c                   eval      Indx=%lookup(alfafgs:xcotfp)
315400130228     c
315500130228     c*
315600130228     C                   ENDSR
315700950927     C**************************************************************************
315800950927     C* RICERCA FV SU CUI E' PARTITA LA SPUNTA
315900950927     C**************************************************************************
316000950927     C     RICFV         BEGSR
316100950927     C                   CLEAR                   WMODL
316200021121     C                   movel     'N'           Wnoabb2           1
316300021121     C                   movel     'N'           Wnoabb6           1
316400950927     C* PRENDO IL TERMINAL DI PARTENZA DELLA LINEA DI PARTENZA
316500970814     C                   CLEAR                   DSLV55
316600970814     C                   MOVEL     'P'           D55TPT
316700970814     C                   MOVEL     WLIN          D55LIN
316800020214     C                   MOVEL     WDTRIF        D55DRF
316900020213     C                   EXSR      FNLV55
317000970814     C                   MOVEL     D55TFP        WPTFP
317100110103     c*
317200100713     C* PRENDO IL TERMINAL DI arrivo generico della linea
317300100713     C                   CLEAR                   DSLV55
317400100713     C                   MOVEL     'A'           D55TPT
317500100713     C                   MOVEL     barlna        D55LIN
317600100713     C                   MOVEL     WDTRIF        D55DRF
317700100713     C                   EXSR      FNLV55
317800100713     C                   MOVEL     D55TFa        WGENlnaTFA
317900021121     c
318000021121     c* se WPTFP = simfel ed ho salvato i campi del foglio illuminato
318100021121     c*  trovato, li considero validi e non rieseguo la routine
318200021121     c                   if        wptfp=simfel  and wfvalnp>0
318300021121     C                   MOVEL     wFVALNP       ANMCDU
318400021121     C                   Z-ADD     wFVALNP       SAVFLP
318500021121     C                   Z-ADD     wFVANFV       ANMNFV
318600021121     C                   Z-ADD     wFVADFV       ANMDAO
318700021121     C                   Z-ADD     wFVADFV       SAVDAO
318800021121     C                   CLEAR                   WLIN
318900021121     C                   MOVEL     '1'           WMODL             1
319000021121     c                   else
319100950927     C*
319200950927     C* CONTROLLO I FV ABBINATI, SE E' FOGLIO ARRIVI
319300020214     C     BARNPG        IFEQ      2
319400950927     C                   Z-ADD     BARNPG        KNPG4
319500950927     C                   Z-ADD     BARNFV        KNFV4
319600020214     C                   Z-ADD     BARFGS        KFGS4
319700950927     C                   EXSR      REDFVA
319800020214     C                   ENDIF
319900950927     C*
320000950927     C* SE NON L'HO TROVATO CERCO UN ALTRO F.ARRIVI DEL GIORNO
320100950927     C     WMODL         IFEQ      ' '
320200021121     c                   eval      wdata=bardfv
320300950927     C                   EXSR      MEMFAR
320400021121     C* MEMORIZZO LA NUOVA DATA
320500021121     C                   MOVEL     BARDFV        SAVDFV
320600110103     C                   Z-ADD     1             I                 3 0
320700950927     C*
320800950927    2C     PG2(I)        DOWGT     0
320900950927     C     ANMNFV        ANDEQ     0
321000950927     C                   Z-ADD     2             KNPG4
321100950927     C                   Z-ADD     PG2(I)        KNFV4
321200020214     C                   Z-ADD     SAVTFA        KFGS4
321300950927     C                   EXSR      REDFVA
321400021121     c*
321500021121     c* Memorizzo se nessuno dei fogli 2 ha dei fogli partenza abbinati
321600021121     c                   if        wabbinati=' '
321700021121     c                   clear                   wnoabb2
321800021121     c                   endif
321900950927     C                   ADD       1             I
322000950927    2C                   ENDDO
322100950927    1C                   ENDIF
322200110103     c*
322300021121     c* Se non ho fogli abbianti e non si tratta di cat arrivi, ritento
322400021121     c*  la ricerca dei fogli arrivi
322500021121    1c     WMODL         IFEQ      ' '
322600110103    2c                   if        barnpg<> 2 and
322700110103     c                             wnoabb2 ='N' and
322800021121     c                             wsavdata=bardfv
322900021121     c* Data - 1
323000021121     C                   MOVEL     bardfv        G02INV
323100021121     C                   Z-ADD     *ZEROS        G02DAT
323200021121     C                   Z-ADD     *ZEROS        G02TGI
323300021121     C                   MOVEL     '3'           G02ERR
323400021121     C                   CALL      'XSRDA8'
323500021121     C                   PARM                    WLBDAT
323600021121      *
323700021121     C                   SUB       1             G02TGI
323800021121     C                   Z-ADD     *ZEROS        GI8DAT
323900021121     C                   Z-ADD     *ZEROS        GI8INV
324000021121     C                   Z-ADD     G02TGI        GI8TGI
324100021121     C                   CALL      'XSRGI8'
324200021121     C                   PARM                    WGIDAT
324300021121      *
324400021121     C                   Z-ADD     GI8INV        wdata
324500021121      *
324600021121     C                   EXSR      MEMFAR
324700021121     C* MEMORIZZO LA NUOVA DATA
324800021121     c                   if        wsavdata>0
324900021121     C                   MOVEL     wsavdata      SAVDFV
325000021121     c                   else
325100021121     C                   MOVEL     BARDFV        SAVDFV
325200021121     c                   endif
325300110103     c*
325400021121     C                   Z-ADD     1             I
325500021121     C*
325600021121    3C     PG2(I)        DOWGT     0
325700021121     C     ANMNFV        ANDEQ     0
325800021121     C                   Z-ADD     2             KNPG4
325900021121     C                   Z-ADD     PG2(I)        KNFV4
326000021121     C                   Z-ADD     SAVTFA        KFGS4
326100021121     C                   EXSR      REDFVA
326200021121     C                   ADD       1             I
326300021121    3C                   ENDDO
326400021121    2C                   ENDIF
326500021121    1c                   endif
326600110103     c*
326700100713     c* se il FV non ancora trovato e si dovrebbe trattare di bolla locale
326800100713     c*  tengo i dati del foglio illuiminato
326900100713     c     WMODL         ifeq      ' '
327000100713     c     wgenlnatfa    andeq     wlin
327100100713     C                   MOVEL     wFVALNP       ANMCDU
327200100713     C                   Z-ADD     wFVALNP       SAVFLP
327300100713     C                   Z-ADD     wFVANFV       ANMNFV
327400100713     C                   Z-ADD     wFVADFV       ANMDAO
327500100713     C                   Z-ADD     wFVADFV       SAVDAO
327600100713     C                   CLEAR                   WLIN
327700100713     C                   MOVEL     '1'           WMODL             1
327800100713     c                   endif
327900950927     C*
328000950927     C* SE IL FV ANCORA NON C'E' CERCO SE ANCORA DA ABBINARE
328100021121    1C     WMODL         IFEQ      ' '
328200950927     C                   CLEAR                   KNFV4
328300950927     C                   CLEAR                   KNPG4
328400020214     C                   MOVE      SAVTFA        KFGS4
328500000223     C* Determino BARDFV-4 gg. che è la data minima che deve avere un
328600000223     C* foglio ancora da abbinare. Se la data foglio è più bassa di
328700000223     C* tale data il foglio verrà scartato
328800000223     C                   MOVE      *ZEROS        DATMIN
328900000223     C                   CLEAR                   WLBDAT
329000000223     C                   CLEAR                   WGIDAT
329100000223     C                   Z-ADD     BARDFV        G02INV
329200000223     C                   MOVEL     '3'           G02ERR
329300000223     C                   CALL      'XSRDA8'
329400000223     C                   PARM                    WLBDAT
329500000223     C     G02TGI        SUB       4             GI8TGI
329600000223     C                   CALL      'XSRGI8'
329700000223     C                   PARM                    WGIDAT
329800000223     C                   Z-ADD     GI8INV        DATMIN
329900950927     C                   EXSR      REDFVA
330000950927     C* SE ANCORA NON HO TROVATO NIENTE --> ESCO DAL CICLO
330100950927    2C     WMODL         IFEQ      ' '
330200950927     C                   CLEAR                   WLIN
330300950927    2C                   ENDIF
330400950927    1C                   ENDIF
330500990827     C*
330600990827     C* PULISCO WLIN PER NON CONTINUARE NELLA RICERCA DEL FOGLIO
330700990827     C* SE LA LINEA DI ARRIVO RISULTA TRANSITARE
330800990827     C* DA UN P.O. CHE  E' LO STESSO DAL QUALE IL COLLO ERA PARTITO
330900990827     C* (PER ES.: 53/31 CHE TRANSITA DA 062 E 062 LO FA TRANSITARE DA
331000990827     C* 53 (CASO CHE SI VERIFICA DURANTE IL PASSAGGIO DALLE TRAZIONI
331100990827     C* RIDOTTE ALLE TRAZIONI NORMALI))
331200990827     C     WLINPR        IFGT      *ZEROS
331300990827     C     WLIN          ANDGT     *ZEROS
331400990827     C     WLIN          ANDEQ     WVALNP
331500990827     C                   CLEAR                   WLIN
331600990827     C                   ENDIF
331700021121     C                   ENDIF
331800950927     C*
331900950927     C                   ENDSR
332000950927     C**************************************************************************
332100950927     C* LETTURA DEI DEI FOGLI VIAGGIO PARTENZA
332200950927     C**************************************************************************
332300950927     C     REDFVA        BEGSR
332400950927     C                   CLEAR                   WMODL
332500021121     C                   CLEAR                   Wabbinati         1
332600950927     C     KFVA4         SETLL     FNFVA000
332700950927     C     KFVA4         READE     FNFVA000                               99
332800021121     c* se 99 on - nessun foglio abbinato --> memorizzo
332900021121     c                   if        *in99
333000021121     c                   eval      wabbinati='N'
333100021121     c                   endif
333200950927     C*
333300950927    1C     *IN99         DOWEQ     *OFF
333400950927    2C     FVALNP        IFEQ      WPTFP
333500950927     C* DATA FOGLIO DEVE ESSERE <= ALLA DATA DI ARRIVO
333600960524     C     FVADFV        ANDLE     BARDFV
333700000223     C     KNPG4         ANDGT     *ZEROS
333800000223     C* SE STO CERCANDO FOGLIO ANCORA DA ABBINARE SCARTO SE E' DI DATA
333900000223     C* INFERIORE ALLA DATA MINIMA (DATA ARRIVO COLLO MENO QUATTRO GG.)
334000000223     C     FVALNP        OREQ      WPTFP
334100000223     C     FVADFV        ANDLE     BARDFV
334200000223     C     FVADFV        ANDGE     DATMIN
334300000223     C     KNPG4         ANDEQ     *ZEROS
334400960207     C*
334500950927     C* SE SI TRATTA DI UN FOGLIO DIRETTO TUTTO A POSTO
334600100712     c* ok anche se locale
334700950927    3C     FVAATR        IFEQ      ' '
334800100712     c     fvattr        oreq      'L'
334900950927     C                   MOVEL     FVALNP        ANMCDU
335000020221     C                   Z-ADD     FVALNP        SAVFLP
335100990518     C                   Z-ADD     FVANFV        ANMNFV
335200950927     C                   Z-ADD     FVADFV        ANMDAO
335300980120     C                   Z-ADD     FVADFV        SAVDAO
335400950927     C                   CLEAR                   WLIN
335500950927     C                   MOVEL     '1'           WMODL             1
335600980120     C* SE IL PRIMO FV TROVATO E' UN DIRETTO LO MEMORIZZO PER SAPERE SE
335700980120     C* DOVRO' AGGIOR.LA DATA DI PARTENZA O LA DATA USCITA DAL TRANSITO
335800980120     C     WFLGT         IFEQ      *BLANKS
335900980120     C                   MOVE      'D'           WFLGT
336000980120     C                   END
336100950927     C                   SETON                                        99
336200960207     C*
336300950927   X3C                   ELSE
336400960207     C* SE E' FOGLIO ILLUMINATO
336500950927     C* DEVO VEDERE DA DOVE E' TRANSITO E MANDARE AL TRANSITO LA SEGNA
336600950927     C*  LAZIONE
336700000505     C     KFWA          CHAIN     FNFWA01L                           28
336800000505     C     *IN28         IFEQ      *ON
336900000505     C     FWAATB        ORNE      ' '
337000000505     C                   CLEAR                   FWAFF3
337100000505     C                   CLEAR                   FWAFF4
337200000505     C                   CLEAR                   FWAFL3
337300000505     C                   CLEAR                   FWAFL4
337400000505     C                   ENDIF
337500000505     C**
337600950927     C                   Z-ADD     FVALNA        LNA2
337700950927     C                   Z-ADD     1             X
337800951002     C                   MOVEL     BARLNA        W003A             3
337900950927     C     W003A         LOOKUP    FFV(X)                                 99
338000960207     C* NON E' IL MIO TERMINAL
338100960902     C* ESCLUDO I FOGLI LOCALI
338200021125     c* in relatà elaboro anche i fogli locali, altrimenti come faccio a
338300021125     c*  determinare un collo arrivo no part, del terminal per la lna?
338400021125     c*  ES. 43  x 132
338500960207    4C     *IN99         IFEQ      *ON
338600021125     C***  FVATTR        ANDNE     'L'
338700050603     C                   MOVEL     Usalnatfa     W003A             3
338800960522    5C     FLP(X)        IFNE      W003A
338900960207     C                   MOVEL     FLP(X)        WLIN
339000020214     C                   MOVE      FVADFV        WDTRIF
339100960207     C                   MOVEL     '1'           WMODL             1
339200980120     C* SE IL PRIMO FV TROVATO E' UN TRANSITO LO MEMORIZZO PER SAPERE
339300980120     C* SE DOVRò AGGIOR. DATA DI PARTENZA O DATA USCITA DAL TRANSITO
339400980120     C     WFLGT         IFEQ      *BLANKS
339500980120     C                   MOVE      'T'           WFLGT
339600021121     c* Memorizzo lo stesso i campi che mi servono perchè se poi di tratta
339700021121     c*  di transito "locale" cioè su stesso as (vedi 82 con 113)
339800021121     c*  devo comunque tenere i dati del foglio illuminato non essendoci
339900021121     c*  abbinamento del foglio locale su stessa £1
340000021121     c*  Il problema verrà a morire quando diventeranno dei 1 livello
340100021121     c*  come devono essere
340200021121     c*
340300021121     c                   z-add     FVALNP        WFVALNP
340400021121     c                   z-add     FVANFV        WFVANFV
340500021121     c                   z-add     FVADFV        WFVADFV
340600980120     C                   END
340700960207   X5C                   ELSE
340800960207     C* SE E' FOGLIO ILLUMINATO PER 2 LIVELLO IN ARRIVO TUTTO A POSTO
340900960207     C                   MOVEL     FVALNP        ANMCDU
341000990518     C                   Z-ADD     FVANFV        ANMNFV
341100960207     C                   Z-ADD     FVADFV        ANMDAO
341200980120     C                   Z-ADD     FVADFV        SAVDAO
341300020221     C                   Z-ADD     FVALNP        SAVFLP
341400960207     C                   CLEAR                   WLIN
341500960207     C                   MOVEL     '1'           WMODL             1
341600980120     C* SE IL PRIMO FV TROVATO E' UN DIRETTO LO MEMORIZZO PER SAPERE SE
341700980120     C* DOVRO' AGGIOR.LA DATA DI PARTENZA O LA DATA USCITA DAL TRANSITO
341800980120     C     WFLGT         IFEQ      *BLANKS
341900980120     C                   MOVE      'D'           WFLGT
342000980120     C                   END
342100960207    5C                   ENDIF
342200960207     C*
342300960207   X4C                   ELSE
342400960207     C     KFVA4         READE     FNFVA000                               99
342500960207    4C                   ENDIF
342600950927    3C                   ENDIF
342700950927     C*
342800950927   X2C                   ELSE
342900950927     C     KFVA4         READE     FNFVA000                               99
343000950927    2C                   ENDIF
343100950927    1C                   ENDDO
343200950927     C                   ENDSR
343300980119     C**************************************************************************
343400980119     C* AGGIORNO DATA PART. O DI USCITA DA TRANS. A FRONTE DI ANOM.50
343500980119     C**************************************************************************
343600980119     C     AGGPAR        BEGSR
343700980119     C     KBAR          CHAIN     FNART27L                           3239
343800980119     C   39              GOTO      ENDPAR
343900160722     c
344000160722    1c     *IN32         IFEQ      *OFF
344100160722     c* Spunta di bolla in arrivo ( anche locale)
344200160722    2C                   if        WTIBOL='A'  or WTIBOL='L'
344300160722
344400980120    3C                   SELECT
344500980120     C     WFLGT         WHENEQ    'D'
344600980120     C                   Z-ADD     SAVDAO        ARTDFV
344700041201     C                   Z-ADD     anmnfv        ARTnFV
344800980120     C                   Z-ADD     88            ARTNPP
344900041202     C                   movel     'S'           ARTABN
345000020218     C     WFLGT         WHENEQ    'L'
345100020218     C                   Z-ADD     SAVDAO        ARTDFV
345200041201     C                   Z-ADD     anmnfv        ARTnFV
345300020218     C                   Z-ADD     90            ARTNPP
345400041202     C                   movel     'S'           ARTABN
345500980120     C     WFLGT         WHENEQ    'T'
345600980120     C                   Z-ADD     SAVDAO        ARTDUT
345700980120     C                   Z-ADD     89            ARTPUT
345800020221     C                   Z-ADD     SAVFLP        ARTFLP
345900041202     C                   movel     'S'           ARTABN
346000980120    3C                   ENDSL
346100160722    2c                   endif
346200160722     c*
346300160722     c* spunta al transito: devo aggiornare anche  in FNART
346400160722     c*  la data di partenza se non c'è
346500160722    2c                   if        WTIBOL='T'
346600160722     c
346700160722    3c                   if        artdfv=0
346800160722     C                   Z-ADD     SAVDAO        ARTDFV
346900160722     C                   Z-ADD     anmnfv        ARTnFV
347000160722     C                   Z-ADD     88            ARTNPP
347100160722    3c                   endif
347200160722    2c                   endif
347300160720
347400980120     C                   UPDATE    FNART000
347500980120     C                   MOVEL     'A'           WTBO              1
347600980120     C                   MOVEL     ARTSPE        WDSPE
347700980120     C                   EXSR      WR1AGB
347800160722    1C                   endif
347900160720
348000160722     C                   if        WTIBOL='T'
348100021203     C     KBAR37        CHAIN     FNBTT37L                           3739
348200980119     C   39              GOTO      ENDPAR
348300161108    2C     *IN37         IFEQ      *OFF
348400980120     C                   Z-ADD     SAVDAO        BTTDFV
348500980120     C                   Z-ADD     88            BTTNPP
348600980120     C                   UPDATE    FNBTT000
348700980120     C                   MOVEL     'T'           WTBO              1
348800980120     C                   MOVEL     BTTSPE        WDSPE
348900980120     C                   EXSR      WR1AGB
349000980120    2C                   END
349100160722    1C                   endif
349200160720
349300160720     C*  AGGIORNO ANCHE LA BOLLA PARTENZA
349400020218    1C     WTIBOL        IFEQ      'L'
349500160720     C     WTIBOL        oreq      'A'
349600160721     C     WTIBOL        oreq      'T'
349700020218     C     KBAR          CHAIN     FNBLT27L                           3239
349800020218     C   39              GOTO      ENDPAR
349900020218    2C     *IN32         IFEQ      *OFF
350000160721
350100160721     c                   if        wtibol<>'T'
350200160721     c                   z-add     anmdch        bltdam
350300160721     c                   else
350400160721     c                   z-add     anmdch        bltdet
350500160721     c                   z-add     bpes          bltflp
350600160722     c                   clear                   bltdut
350700160721     c                   endif
350800160720
350900160721    3C                   IF        WFLGT<>'T' AND
351000160721     c                             (bltdfv=0 or bltdfv>savdao) or
351100160721     c                             wflgt= 'T' and bltdfv=0
351200160721     c
351300160720     C                   Z-ADD     SAVDAO        bltDFV
351400160726     c                   eval      welasta_p='S'
351500160720
351600160721    4c                   select
351700160720     C     BLTDSE        WHENEQ    0
351800160720     c                   if        wflgt='D'
351900160720     C                   MOVEL     88            BLTNPP
352000160721     c                   else
352100160720     C                   Z-ADD     90            bltNPP
352200160720     c                   endif
352300160720     C     BLTDSE        WHENGT    0
352400160720     C     BLTDSE        ANDLT     BLTDFV
352500160720     C                   MOVEL     96            BLTNPP
352600160720     C     BLTDSE        WHENGT    0
352700160720     C     BLTDSE        ANDGE     BLTDFV
352800160720     C                   MOVEL     98            BLTNPP
352900160721    4C                   ENDSL
353000160720
353100160721    3c                   endif
353200160720
353300160722    3C                   if        wflgt='T'  and WTIBOL<>'T' and
353400160722     c                             (bltdut=0 or bltdut>savdao)
353500160720     C                   Z-ADD     SAVDAO        bltDUT
353600160722     C                   Z-ADD     bpes          bltFLP
353700160721    3c                   endif
353800160720     c
353900020218     C                   UPDATE    FNBLT000
354000020218     C                   MOVEL     'P'           WTBO              1
354100020218     C                   MOVEL     BLTSPE        WDSPE
354200020218     C                   EXSR      WR1AGB
354300160721     c
354400160721     c* se c'è transito, aggiorno anche BTT
354500160721    3C     WFLGT         ifeq      'T'
354600160721     C     KBAR_t        CHAIN     FNBTT37L                           3239
354700160721    4C     *IN32         IFEQ      *OFF
354800160721     C     *IN39         andeq     *OFF
354900160721
355000160726     c                   eval      welasta_p='S'
355100160721     C                   Z-ADD     SAVDAO        bttDUT
355200160721     c                   z-add     anmdch        bttdam
355300160721     C                   SELECT
355400160721     C     BTTDET        WHENEQ    0
355500160721     C                   MOVEL     89            BTTPUT
355600160721     C     BTTDET        WHENGT    0
355700160721     C     BTTPET        ANDEQ     89
355800160721     C                   MOVEL     89            BTTPUT
355900160721     C     BTTDET        WHENGT    0
356000160721     C     BTTDET        ANDLT     BTTDUT
356100160721     C                   MOVEL     96            BTTPUT
356200160721     C     BTTDET        WHENGT    0
356300160721     C     BTTDET        ANDGE     BTTDUT
356400160721     C                   MOVEL     98            BTTPUT
356500160721     C                   ENDSL
356600160721     C*
356700160721     C     BTTDET        IFEQ      0
356800160721     C                   Z-ADD     ANMDAO        BTTDET
356900160721     C                   MOVEL     89            BTTPET
357000160721     C                   ENDIF
357100160721     C                   UPDATE    FNBTT000
357200160721     C                   MOVEL     'T'           WTBO              1
357300160721     C                   MOVEL     BLTSPE        WDSPE
357400160721     C                   EXSR      WR1AGB
357500160721    4C                   ENDif
357600160721    3C                   ENDif
357700160720
357800020218    2C                   END
357900020218    1C                   END
358000160721
358100980119     C     ENDPAR        TAG
358200160721
358300980119     C   39              SETON                                        40
358400160726     c
358500160726     c                   clear                   welasta_p
358600160726     c
358700980119     C                   ENDSR
358800160727     C**************************************************************************
358900160727     C* AGGIORNO DATA entrata al transito per DIsguido
359000160727     C**************************************************************************
359100160727     C     AGGPAR_d      BEGSR
359200160727     C     KBAR          CHAIN     FNBLT27L                           3239
359300160727    2C     *IN32         IFEQ      *OFF
359400160727    2C     *IN39         andeq     *OFF
359500160727     C                   Z-ADD     SAVDAO        bltDET
359600160727     c                   clear                   bltdut
359700160727     c                   movel     barfgs        bltflp
359800160727     c                   update    fnblt000
359900160727
360000160727     C                   MOVEL     'P'           WTBO              1
360100160727     C                   MOVEL     BLTSPE        WDSPE
360200160727     C                   EXSR      WR1AGB
360300160727     c                   endif
360400160727     c
360500160727     C                   ENDSR
360600950927     C**************************************************************************
360700950927     C* MEMORIZZO I FOGLI ARRIVI CETEGORIA 2 E 6 DEL GIORNO
360800950927     C**************************************************************************
360900950927     C     MEMFAR        BEGSR
361000021121     c                   clear                   wsavdata          8 0
361100021113     c
361200021113     c* se è collo di bolla transito
361300021113     c*  devo usare simfel, il suo p.o. di transito
361400021113     c                   if        wtibol<>'T'
361500050603     C                   MOVEL     usalnatfa     KTFA2
361600021113     c                   else
361700021113     C                   z-add     simfel        KTFA2
361800021113     c                   endif
361900021113     c
362000950927     C* RIMEMORIZZO I FV SOLO SE E' CAMBIATA LA  DATA
362100021121    1C     wdata         IFNE      SAVDFV
362200970113     C     KTFA2         ORNE      SAVTFA
362300021107     c
362400021121     c* Cerco fogli cat 2 e 6  se non trovati in data BARDFV, li cerco
362500021107     c*  in data bardfv - 1
362600021113     c* faccio max 8 tentativi (1 settimana  tenendo conto che ci possono
362700021113     c*  essere dei festivi)
362800950927     C                   CLEAR                   PG2
362900021113     c                   z-add     0             wgiorni           2 0
363000021113     c
363100021113    2c                   dow       wdata>0  and wgiorni<8
363200021113     c                   add       1             wgiorni
363300950927     C*
363400950927     C                   Z-ADD     2             WNPG
363500950927     C     KFVV          SETLL     FNFVV002
363600950927     C     KFVV          READE     FNFVV002                               33
363700950927     C                   Z-ADD     1             I
363800950927     C*
363900950927     C     *IN33         DOWEQ     *OFF
364000950927     C                   Z-ADD     FV2NFV        PG2(I)
364100950927     C                   ADD       1             I
364200950927     C     KFVV          READE     FNFVV002                               33
364300950927     C                   ENDDO
364400021107     c*
364500021107     c                   xfoot     pg2           wpg2             13 0
364600110103     c                   if        wpg2>0
364700021121     c                   eval      wsavdata=wdata
364800021107     c                   clear                   wdata
364900021107     c                   else
365000021107     c* Data - 1
365100021107     C                   MOVEL     wdata         G02INV
365200021107     C                   Z-ADD     *ZEROS        G02DAT
365300021107     C                   Z-ADD     *ZEROS        G02TGI
365400021107     C                   MOVEL     '3'           G02ERR
365500021107     C                   CALL      'XSRDA8'
365600021107     C                   PARM                    WLBDAT
365700021107      *
365800021107     C                   SUB       1             G02TGI
365900021107     C                   Z-ADD     *ZEROS        GI8DAT
366000021107     C                   Z-ADD     *ZEROS        GI8INV
366100021107     C                   Z-ADD     G02TGI        GI8TGI
366200021107     C                   CALL      'XSRGI8'
366300021107     C                   PARM                    WGIDAT
366400021107      *
366500021107     C                   Z-ADD     GI8INV        wdata
366600021107     c                   endif
366700021107     c                   enddo
366800021121     c*
366900970113     C                   MOVEL     KTFA2         SAVTFA
367000021107    1C                   ENDIF
367100950927     C                   ENDSR
367200950927     C**************************************************************************
367300950927     C* RIEMPO PARTE DEI CAMPI DELLA ANOMALIA
367400950927     C**************************************************************************
367500950927     C     RIEANM        BEGSR
367600950927     C                   MOVEL     BARLNP        ANMFLS
367700950927     C                   MOVEL     BARLNA        ANMLNA
367800950927     C                   MOVEL     BARNRS        ANMNRS
367900950927     C                   MOVEL     BARNSC        ANMSCN
368000950927     C                   MOVEL     BARNPS        ANMNPS
368100950927     C                   Z-ADD     COMCAA        ANMCAA
368200040621     c                   z-add     999           anmfl4
368300040621     c* imposto in anmflo data e ora creazione spunta
368400050707     C                   TIME                    TIMEanm          14 0
368500050707     c                   movel     timeanm       wora4             4 0
368600050707     C                   move      timeanm       G02DAT
368700050707     C                   Z-ADD     *ZERO         G02INV
368800050707     C                   MOVEL     *BLANKS       G02ERR
368900050707     C                   CALL      'XSRDA8'
369000050707     C                   PARM                    WLBDAT
369100050707     C                   move      G02INV        WDAT6             6 0
369200040621     c                   movel     wdat6         anmflo
369300040621     c                   move      wora4         anmflo
369400950927     C* FASE APERTURA
369500950927     C     BARKEY        IFNE      SAVFOG
369600950927     C                   EXSR      RICFAS
369700950927     C                   END
369800950927     C                   MOVEL     COMFAS        ANMFSA
369900950927     C* CODICE ANOMALIA
370000950927     C     COMCAA        IFNE      SAVCAA
370100950927     C                   MOVEL     '7C'          COD
370200950927     C                   MOVEL(P)  COMCAA        KEY
370300950927     C     KTAB          CHAIN     TABEL                              34
370400950927     C   34              CLEAR                   DS7C
370500950927     C  N34              MOVEL     TBLUNI        DS7C
370600950927     C                   MOVEL     COMCAA        SAVCAA            3 0
370700950927     C                   ENDIF
370800950927     C*
370900950927     C                   MOVEL     §7CTAN        ANMTAN
371000950927     C                   MOVEL     §7CAIE        ANMAIE
371100950927     C                   MOVEL     §7CSAN        ANMSAN
371200950927     C                   MOVEL     §7CLID        ANMLID
371300950927     C* AUTOCHIUSA
371400950927     C     §7CCHA        IFEQ      'S'
371500960524     C                   MOVE      BARDFV        ANMDCH
371600950927     C                   MOVEL     COMFAS        ANMFSC
371700000320     C                   MOVEL     WCCH          ANMCCH
371800950927     C                   END
371900950927     C                   ENDSR
372000960523     C**************************************************************************
372100960523     C* CARICO LA L6 DELLA FILIALE GESTIONE FOGLIO
372200960523     C**************************************************************************
372300960523     C     CARL6         BEGSR
372400960523     C                   CLEAR                   DSUL06
372500960523     C                   MOVE      '£6'          D06COD
372600020213     C                   MOVEL     SAVPES        D06KEY
372700960523     C                   MOVEL     DSUL06        KPJBU
372800960523     C*
372900960523     C                   CALL      'TRUL06R'
373000960523     C                   PARM                    KPJBA
373100960523     C                   MOVEL     KPJBU         DSUL06
373200960523     C                   MOVEA     LIN           L6
373300960523     C                   ENDSR
373400960523     C**************************************************************************
373500960523     C* SE ESISTE GIA' UNA SPUNTA DELLA LNA, NON CREO ANOMALIE 145-146
373600960523     C**************************************************************************
373700960523     C     CTRSPU        BEGSR
373800960523     C                   MOVEL     'S'           WCREA
373900970117     C*
374000970117     C                   MOVEL     'S'           WCHIU             1
374100970117     C*"WCHIU" MI SERVE PER DETERMINARE SE POSSO CHIUDERE UNA ANOMALIA
374200970117     C* 145/146 CHE HA DATA ANOMALIA < DELLA DATA DELLA SPUNTA CHE
374300970117     C*  STO ELABORANDO MA POTREBBE COMUNQUE ESSERCI UNA SPUNTA
374400970117     C*  CHE MI MANTIENE APERTA IN DATA > TALE ANOMALIA
374500970117     C*  1) SE E' PRESENTE UN'ALTRA SPUNTA CON DATA > NON CHIUDO
374600970117     C*  2) SE E' PRESENTE UNA SPUNTA CON DATA = CATEGORIA 3 4 5
374700020204     C*     NON CHIUDO SE LA SPUNTA CHE STO SCARICANDO E' CAT 1 2 6
374800970117     C*  3) SE E' PRESENTE UNA SPUNTA CON DATA = CATEGORIA 3 4 5
374900970117     C*     E E' UNA SPUNTA DELLA LNA SE LA SPUNTA CHE STO SCARICANDO
375000970117     C*     E' CATEGORIA 3 4 5
375100960531     C**
375200020204    1C     WAGARR        IFEQ      'S'
375300960531     C**
375400070131     C     KBRV7         CHAIN     FNBRV07L                           33
375500960531    2C     *IN33         DOWEQ     *OFF
375600960531    3C     BR7KEY        IFNE      SA1KEY
375700960527     C                   MOVEL     BR7KEY        SA1KEY
375800960527     C                   Z-ADD     BR7NPG        WNPG
375900960527     C                   Z-ADD     BR7NFV        WNFV
376000021014     C                   Z-ADD     BR7FGS        WFGS
376100070906     C                   movel     BR7nps        Wnps
376200021014     C                   CLEAR                   RICBAR
376300960523     C                   EXSR      RILFOG
376400960523     C                   Z-ADD     WDFV          BRVDFV
376500140616     C                   movel     WFCF          BRVFCF
376600140616     C                   movel     Wspg          BRVspg
376700960531    3C                   ENDIF
376800040224     c* Terminal di arrivo della spunta letta
376900040224     C                   CLEAR                   DSLV55
377000040224     C                   MOVEL     'A'           D55TPT
377100040224     C                   MOVEL     LNPB          D55LNP
377200040224     C                   MOVEL     brvdfv        D55DRF
377300040224     C                   MOVE      Br7pes        D55LIN
377400040224     C                   EXSR      FNLV55
377500050603     c                   if        d55tfa=Usalnatfa
377600960531     C**
377700961210    3C     BRVDFV        IFGT      BARDFV
377800960523     C                   MOVEL     'N'           WCREA             1
377900970117     C                   MOVEL     'N'           WCHIU             1
378000961209   X3C                   ELSE
378100961209     C*
378200961210    4C     BRVDFV        IFEQ      BARDFV
378300961210     C     BR7NPG        ANDGE     3
378400961210     C     BR7NPG        ANDLE     5
378500970117     C*
378600970117     C     BARNPG        IFEQ      1
378700970117     C     BARNPG        OREQ      2
378800970117     C                   MOVEL     'N'           WCHIU
378900970117     C                   ENDIF
379000961209     C*
379100020213    5C     BR7PES        IFEQ      BARLNA
379200961209     C                   MOVEL     'N'           WCREA             1
379300970117     C*
379400970117     C     BARNPG        IFNE      1
379500970117     C     BARNPG        ANDNE     2
379600970117     C                   MOVEL     'N'           WCHIU
379700970117     C                   ENDIF
379800970117     C*
379900961209   X5C                   ELSE
380000020214    6C     BR7PES        IFNE      SAVPES
380100020214     C                   MOVEL     BR7PES        SAVPES
380200961209     C                   EXSR      CARL6
380300961209    6C                   ENDIF
380400961209     C*
380500961209     C     BARLNA        LOOKUP    L6                                     34
380600961209     C   34              MOVEL     'N'           WCREA             1
380700970117     C*
380800970117    6C   34BARNPG        IFNE      1
380900970117     C     BARNPG        ANDNE     2
381000970117     C                   MOVEL     'N'           WCHIU
381100970117    6C                   ENDIF
381200970117     C*
381300961209    5C                   ENDIF
381400961209    4C                   ENDIF
381500961209    3C                   ENDIF
381600040224    3C                   ENDIF
381700961209     C*
381800961209    3C     WCREA         IFEQ      'N'
381900961209     C                   SETON                                        33
382000961210     C                   ELSE
382100070131     C     KBRV7         READE     FNBRV07L                               33
382200961209    3C                   ENDIF
382300040224     C**
382400960531    2C                   ENDDO
382500960531     C*
382600960531    1C                   ENDIF
382700960523     C*
382800960523     C                   ENDSR
382900961202     C**************************************************************************
383000961202     C* CREO ANOMALIA 145 O 146 E CHIUDO L'ALTRA
383100961202     C**************************************************************************
383200961202     C     ANM14X        BEGSR
383300961202     C* LA CREO SOLO SE NON ESISTE UNA SPUNTA DELLA LNA CON DATA=>
383400961210     C     WDCM          IFGT      0
383500961210     C     WDCM          ANDGE     BARDFV
383600961210     C                   MOVEL     'N'           WCREA
383700961210     C                   ELSE
383800961202     C                   EXSR      CTRSPU
383900961210     C                   ENDIF
384000961202     C*
384100961202    5C     WCREA         IFEQ      'S'
384200020213     C     BARFLE        ANDEQ     BTFP
384300020213     C     BARFLE        ORNE      BTFP
384400961202     C                   Z-ADD     CAACRE        COMCAA
384500961202     C                   EXSR      CRTANM
384600961202     C* SOLO PER EXPORT
384700961202     C*
384800961202     C     CAACRE        IFEQ      146
384900961202    6C     WTIBOL        ANDNE     'A'
385000961202     C                   MOVEL     'S'           WTIBOL
385100961202    6C                   ENDIF
385200961202    5C                   ENDIF
385300961202     C*
385400961202     C* SE SPUNTA DA ALTRO AS MA NON DOVEVO CREARE IL COLLO DA INOLTRAR
385500961202     C*  LA CHIUDO SUBITO. ANCHE SE IL COLLO E' CONSEGNATO SENZA SPUNTE
385600020213    5C     BARFLE        IFNE      BTFP
385700961202    6C     WCREA         IFNE      'S'
385800961202     C                   Z-ADD     CAACRE        COMCAA
385900961211     C* IN QUESTO CASO NON DEVO CREARE L'ANOMALIA 620 IN QUANTO
386000961211     C*  LA SPEDIZIONE E' GIA' STATA CONSEGNA A COMUNQUE E' PRESENTE
386100961211     C*  UNA SPUNTA CON DATA >.
386200961211     C                   MOVEL     'N'           W620
386300961202     C                   EXSR      CHIUAN
386400961211     C                   MOVEL     'S'           W620
386500961211     C*
386600961209   X6C                   ELSE
386700961210     C                   EXSR      ANM600
386800961202    6C                   ENDIF
386900961209     C*
387000961202   X5C                   ELSE
387100961210    6C     WCREA         IFEQ      'S'
387200961209     C* PER ESTERO CREO ANOMALIA 645 E 600
387300961209     C                   EXSR      ANM600
387400961210    6C                   ENDIF
387500961210    5C                   ENDIF
387600961202     C*
387700961210     C                   MOVEL     'N'           W620              1
387800961202     C                   Z-ADD     CAACIU        COMCAA
387900961202     C                   EXSR      CHIUAN
388000961210     C                   MOVEL     'S'           W620              1
388100961202     C                   ENDSR
388200961213     C**************************************************************************
388300961213     C* LETTURA FOGLIO DELLA SPUNTA CHE STO CARICANDO E IMPOSTAZIONE
388400961213     C*  CAMPI DI COMOD
388500961213     C**************************************************************************
388600961213     C     BARFGV        BEGSR
388700961213     C*
388800961213    3C     BARKEY        IFNE      SAVKEY
388900961213     C                   MOVEL     BARKEY        SAVKEY
389000961213     C                   Z-ADD     BARNPG        WNPG
389100961213     C                   Z-ADD     BARNFV        WNFV
389200021014     C                   Z-ADD     BARFGS        WFGS
389300070906     C                   movel     BARnps        Wnps
389400021014     C                   MOVEL     'S'           RICBAR            1
389500961213     C                   EXSR      RILFOG
389600980313     C                   MOVEA     FFG           FFS
389700961213     C                   Z-ADD     WDFV          BARDFV
389800961213     C                   MOVEL     WSPG          BARSPG
389900021025     C                   MOVEL     WFGS          wwwFGS
390000021023     C                   Z-ADD     WFLE          BARFLE
390100140616     C                   movel     WFCF          BARFCF
390200970521     C*
390300970521     C     WTTR          IFNE      *BLANKS
390400970520     C     WTTR          LOOKUP    TTR                                    34
390500970520     C                   MOVE      *IN34         WDEFL             1
390600970521     C                   ELSE
390700970521     C                   MOVE      '0'           WDEFL             1
390800970521     C                   ENDIF
390900970521     C*
391000961213    3C                   ENDIF
391100021025     C* IL P.O. GESTIONE LO DEVO SEMPRE IMPOSTARE PERCHÈ PUÒ CAMBIARE
391200021031     C                   MOVEL     WWWFGS        BARFGS
391300070209     c
391400070209     c* Se si tratta di spunta partenza, il BPES deve essere il P.O foglio
391500070209     c*  per problemi con eventuale creazione di anomalie 200
391600070209     c                   eval      wpes=bpes
391700070209     c                   if        barnpg=1
391800070209     c                   eval      bpes=barfgs
391900070209     c                   endif
392000961213     C                   ENDSR
392100970203     C**************************************************************************
392200970203     C* ANOMALIE CHE DEVO SEMPRE CREARE O SEMPRE CHIUDERE SE COLLO
392300970203     C*  SPARATO NELL'AREA DI ARRIVO (COME TERMINAL DI ARRIVO)
392400970203     C**************************************************************************
392500970203     C     ANMSEM        BEGSR
392600970203     C*
392700970203     C* COLLO/BOLLA ARRIVI PRESENTE
392800970204     C* SE CONSEGNATO IL COLLO CREO SEMPRE ANOMALIA --> COLLO CONSEGNAT
392900970204    1C  N05WBOAR         IFEQ      'S'
393000990120    2C     BARDFV        IFGT      WATDCM
393100970204     C     WATDCM        ANDGT     0
393200170328     c* oppure senza consegna ma con cons anomala "S"
393300170328     c     watdcm        oreq      0
393400170328     c     wabcca        andeq     'S'
393500170505     c
393600170505     c*
393700170505     c* se bolla con cons anomala legata, la bolla figlia
393800170505     c*  DEVE essere borderizzata per dare anomalia 138
393900170505     c*  per semplificare leggo della bolla figlia solo il 1° dettaglio colli
394000170505     c                   eval      w138='S'
394100170505    3c                   if        wabcca<>' '  and comnsp>0
394200170505     c     kbolla        chain     fnlbl02l
394300170505    4c                   if        %found(fnlbl02l)
394400170505     c     klbl          chain(n)  fnblp01l
394500170505    5c                   if        %found(fnblp01l) and
394600170505     c                              blpft2=' '
394700170505     c                   eval      w138='N'
394800170505    5c                   endif
394900170505    4c                   endif
395000170505    3c                   endif
395100170505     c
395200170505     c                   if        w138='S'
395300970203     C                   Z-ADD     138           COMCAA
395400970203     C                   EXSR      CRTANM
395500170505     c                   endif
395600170505     c
395700990120    2C                   ENDIF
395800170505     c
395900000705     C* SE COLLO CAT.8 SEGNALA SE LA RELATIVA DISTINTA 4 E'
396000000705     C*  GIA' CHIUSA CON ANOMALIA 139
396100040430     c                   if        watdcm>0  and barnpg=8
396200000705     C* SE NON E' CHIUSA VEDO SE IN FASE DI CHIUSURA: CHIAMO FNLV53R
396300000705     C                   CLEAR                   DSLV53
396400000705     C                   MOVEL     'V'           D53TFO
396500020213     C                   MOVEL     BTFP          D53FEL
396600000705     C                   Z-ADD     BARNFV        D53NFV
396700000705     C                   MOVEL     4             D53NPG
396800000705     C                   MOVEL     BARFGS        D53FGS
396900000705     C                   MOVEL     BARFLE        D53FLF
397000000705     C                   CALL      'FNLV53R'
397100000705     C                   PARM                    DSLV53
397200000705     C*
397300000705     C* FOGLIO CHIUSO (S) O IN FASE DI CHIUSURA (F)
397400000705    3C     D53FCF        IFEQ      'S'
397500000705     C     D53FCF        OREQ      'F'
397600040430     c* non creo se la relativa consegna anomala non lo prevede
397700040430    4c                   if        WABCCA<>*blanks and wabcca<>savcca
397800040430     C                   MOVEL     '7O'          COD
397900040430     C                   MOVEL(P)  wabcca        KEY
398000040430     C     KTAB          CHAIN     TABEL                              41
398100040430     C  N41              MOVEL     TBLUNI        DS7o
398200040430     c   41              clear                   §7o139
398300040430     c                   eval      savcca=wabcca
398400040430    4c                   endif
398500040430     c
398600040430    4c                   if        §7o139=*blanks or wabcca=*blanks
398700000705     C                   Z-ADD     139           COMCAA
398800000705     C                   EXSR      CRTANM
398900040430    4C                   ENDIF
399000040430    3C                   ENDIF
399100000705    2C                   ENDIF
399200000705     C*
399300990120     C* COLLO NON CONSEGNATO
399400990121    2C     WATDCM        IFEQ      0
399500170328     c     wabcca        andne     'S'
399600990120     C* VERIFICO SE DEVO CREARE ANOMALIA SU DANNI
399700000523     C*  NON LO CREO PER LA CATEGORIA 4 E 8
399800990121     C                   MOVEL     'N'           WCAT4             1
399900990126     C* NON GESTISCO IL RITROVAMENTO COLLO
400000990126     C                   MOVEL     'N'           WRITRO
400100990126     C*     GESTISCO LA CREAZIONE ANOMALIE
400200990126     C                   MOVEL     'S'           WCREAN
400300990120     C                   EXSR      CTRDAN
400400990120     C** SE NON TROVATA CA DI COLLO
400500990120     C* VERIFICO SE LA SPEDIZIONE E' SU UNA CA CHIUSA --> CREO
400600990120    3C     WTRDAN        IFEQ      ' '
400700040811      *
400800040811      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
400900040811      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
401000040811      * legate alla spedizione che passo
401100040811     c                   clear                   fidn12ds
401200170505     c***                eval      i12aas = ARTAAS
401300170505     c***                eval      i12lnp = ARTLNP
401400170505     c***                eval      i12nrs = ARTNRS
401500170505     c***                eval      i12nsp = ARTNSP
401600170505     c                   eval      i12aas = comAAS
401700170505     c                   eval      i12lnp = comLNP
401800170505     c                   eval      i12nrs = barNRS
401900170505     c                   eval      i12nsp = comNSP
402000170505     c                   eval      i12fel = 'E'
402100040811     c*
402200040811     c                   call      'FIDN12R'
402300040811     c                   parm                    fidn12ds
402400040811     c*
402500040811     c                   setoff                                       33
402600040811     c*
402700040811     c* se non ci sono errori
402800040811     c                   if        o12err <> ' '
402900040811     c                   seton                                        33
403000040811     c                   else
403100040811     c* se numero di CA trovate maggiore di zero
403200040811     c                   if        o12nca = 0
403300040811     c                   seton                                        33
403400040811     c                   else
403500040811     c                   z-add     1             jj                2 0
403600040811     c                   dow       jj <= o12nca and *in33 = *off
403700040811     C     skDch(jj)     IFGT      0
403800040811     C     BARNPG        IFNE      4
403900040811     C     BARNPG        ANDNE     8
404000040811     C*
404100040811     C* Valorizzo i campi DCT* utilizzati nella routine CRE151 con quelli
404200040811     C* ritornati dal *pgm d utilità FIDN12R
404300040811     C                   movel     skCch(jj)     DCTCCH
404400040811     C*
404500040811     C                   EXSR      CRE151
404600040811     C                   ELSE
404700040811     C* SE E' CAT 4 E 8 LA CHIUDO
404800040811     C                   Z-ADD     151           COMCAA
404900040811     C                   EXSR      CHIUAN
405000040811     C                   ENDIF
405100040811     C**
405200040811     C                   SETON                                        33
405300040811     C**
405400040811     C                   ENDIF
405500040811     c*
405600040811     c                   add       1             jj
405700040811     c                   enddo
405800040811     c*
405900040811     c                   seton                                        33
406000040811     c*
406100040811     c                   endif
406200040811     c                   endif
406300040811     C**
406400990120    3C                   ENDIF
406500990120     C**
406600990120    2C                   ENDIF
406700990120    1C                   ENDIF
406800021023     C*
406900970203     C* CHIUDO LE ANOMALIE DI COLLO MANCATE
407000020204    1C     WAGARR        IFEQ      'S'
407100970203     C* PER COLLO NON LOCALE
407200021023     C     WAGPAR        OREQ      ' '
407300970203     C* PER BOLLA NON TROVATA IN PARTENZA
407400970203     C     WBOPAR        OREQ      'N'
407500021023     C**
407600021023     C* NON AGGIORNO CON SPUNTA ENTRATA
407700021023     C* SE NON E' LOCALE O DISGUIDO IN AREA AGGIORNO SEMPRE
407800021023    2C     WSPLOC        IFEQ      ' '
407900021023     C     WSPLOC        OREQ      'D'
408000021023     C* SE E' LOCALE SOLO SE E' SPUNTA FATTA CON CAT.ARRIVI  DAL P.O.
408100021023     C*  LINEA ARRIVO O TERMINAL ARRIVO
408200021023     C     WSPLOC        OREQ      'A'
408300021023     C     BARNPG        ANDNE     5
408400021023     C     BARNPG        ANDNE     1
408500021023     C*
408600021023     C     WSPLOC        OREQ      'A'
408700021023     C     BARNPG        ANDEQ     1
408800021023     C     WDEFL         ANDEQ     '1'
408900021023     C*
409000021023     C     WSPLOC        OREQ      'T'
409100021023     C     BARNPG        ANDNE     5
409200021023     C     BARNPG        ANDNE     1
409300021023     C*
409400021023     C     WSPLOC        OREQ      'T'
409500021023     C     BARNPG        ANDEQ     1
409600021023     C     WDEFL         ANDEQ     '1'
409700021023     C*
409800970203     C*
409900970203     C* COLLO ARRIVATO MANCANTE --> SOLO SE SPARATO SUL SISTEMA
410000041018     c* L'anomalia 15 non la chiudo se altra spunta presente in cat.arrivi
410100041018     c*  con stessa data
410200041018     c                   eval      wcontrdfv='S'
410300041018     c                   eval      wstess='S'
410400041018     c                   eval      wchiu='S'
410500970203     C                   Z-ADD     15            COMCAA
410600970203     C                   MOVEL     'TR'          SAVCCH
410700970203     C                   EXSR      CHIUAN
410800970203     C*
410900970203     C* COLLO PARTITO NON ARRIVATO
411000970203     C                   MOVEL     'AR'          SAVCCH
411100970203     C                   Z-ADD     5             COMCAA
411200970203     C                   EXSR      CHIUAN
411300970203     C* MANCA COLLO MANCA BOLLA
411400970203     C                   MOVEL     'AR'          SAVCCH
411500970203     C                   Z-ADD     12            COMCAA
411600970203     C                   EXSR      CHIUAN
411700990521    2C                   ENDIF
411800990521    1C                   ENDIF
411900040430     c
412000040430     c* Chiudo fuori posto bolla partenza e fuori posto bolla transito
412100040430     C                   Z-ADD     120           COMCAA
412200040430     C                   EXSR      CHIUAN
412300040430     C                   Z-ADD     125           COMCAA
412400040430     C                   EXSR      CHIUAN
412500970203     C                   ENDSR
412600970204     C**************************************************************************
412700970204     C* ANOMALIA FUORI POSTO SEGNACOLLO BOLLA IN PARTENZA
412800970204     C**************************************************************************
412900970204     C     ANM120        BEGSR
413000970204     C                   Z-ADD     120           COMCAA
413100970204     C*
413200970204     C* VALGONO SOLTANTO LE SPUNTE SPARATE NELL'AREA DI PARTENZA
413300020213    1C  N05BARFLE        IFEQ      BTFP
413400970204     C* SPUNTA DEL TERMINAL DI PARTENZA
413500020213    2C     BARFGS        IFEQ      BTFP
413600970204     C* SE COLLO NON SPUNTATO NELLE CATEGORIE PARTENZA
413700970204    3C     BARNPG        IFNE      5
413800970204     C     BARNPG        ANDNE     1
413900970204     C     BARSPG        ANDNE     'P'
414000970204     C                   EXSR      CRTANM
414100970204   X3C                   ELSE
414200970204     C* SE COLLO     SPUNTATO NELLE CATEGORIE PARTENZA
414300970204     C                   EXSR      CHIUAN
414400970204    3C                   ENDIF
414500970204     C*
414600970204   X2C                   ELSE
414700970204     C* COLLO SPUNTATO DA UN 2 LIVELLO IN PARTENZA
414800970204     C* SE COLLO NON SPUNTATO NELLE CATEGORIE PARTENZA
414900970204    3C     BARNPG        IFNE      5
415000970204     C     BARSPG        ANDNE     'P'
415100970204     C                   EXSR      CRTANM
415200970204     C                   ELSE
415300970204     C* SE COLLO     SPUNTATO NELLE CATEGORIE PARTENZA
415400970204     C                   EXSR      CHIUAN
415500970204    3C                   ENDIF
415600970204    2C                   ENDIF
415700970204    1C                   ENDIF
415800970204     C                   ENDSR
415900970204     C**************************************************************************
416000970204     C* ANOMALIE CHE CREO SOLO SE HA SPARATO LA LINEA DI ARRIVO DEL COL
416100970204     C**************************************************************************
416200970204     C     ANMARR        BEGSR
416300970204     C*
416400970204     C* COLLO/BOLLA ARRIVI PRESENTE
416500970204    1C  N05WBOAR         IFEQ      'S'
416600990126     C                   MOVEL     'N'           WCAT4             1
416700990126     C*     GESTISCO IL RITROVAMENTO COLLO
416800990126     C                   MOVEL     'S'           WRITRO
416900020204     C* NON GESTISCO LA CREAZIONE ANOMALIE CHE GIA' FACCIO IN CHIUAN
417000990126     C                   MOVEL     'N'           WCREAN
417100990126     C                   EXSR      CTRDAN
417200970204     C*
417300970204    2C     WATDCM        IFEQ      0
417400170328     c* in caso di cons anomala "S" il collo potrebbe non avere la data di cons
417500170328     c     wabcca        andne     'S'
417600170328     c
417700970204     C     BARDFV        ORGT      WATDCM
417800170329     c     watdcm        andgt     0
417900970204     C*
418000970204    3C     WABNDC        IFNE      0
418100970204     C     WATDCM        ANDEQ     0
418200170328     c*
418300970204     C* BOLLA IN CONSEGNA
418400970204    4C     BARNPG        IFNE      4
418500000523    4C     BARNPG        ANDNE     8
418600970204     C* SE NO SPUNTA ARRIVI CREO ANOMALIA: BOLLA IN CONSEGNA FUORI POST
418700970204    5C     BARNPG        IFNE      2
418800970204     C                   Z-ADD     135           COMCAA
418900970204     C                   EXSR      CRTANM
419000970204    5C                   END
419100970204     C*
419200000607    4C                   ENDIF
419300000607     C**
419400000607    4C     BARNPG        IFEQ      4
419500970204     C*
419600970204     C*        ANOMALIA: "IN CONSEGNA SU ALTRA DISTINTA"
419700970204    5C     WABNDC        IFNE      BARNFV
419800970204     C                   Z-ADD     150           COMCAA
419900970204     C                   EXSR      CRTANM
420000000523     C*
420100970204   X5C                   ELSE
420200970204     C*
420300970204     C* CHIUDO ANOMALIA: "IN CONSEGNA SU ALTRA DISTINTA"
420400970204     C                   Z-ADD     150           COMCAA
420500970204     C                   MOVEL     '  '          SAVCCH            2
420600970204     C                   EXSR      CHIUAN
420700970204    5C                   ENDIF
420800970204    4C                   ENDIF
420900970204    3C                   ENDIF
421000970204     C*
421100970204     C* BOLLA ARRIVI FUORI POSTO
421200161201     c* In caso di wgiac = 'D' non devo dare il fuori posto
421300161201    3C     WGIAC         IFEQ      ' '
421400970204     C*
421500161201    4C     BARSPG        IFEQ      'G'
421600161201    4C     BARSPG        oreq      'Z'
421700970204     C                   Z-ADD     130           COMCAA
421800970204     C                   EXSR      CRTANM
421900970204    4C                   ENDIF
422000161021     c
422100970204     C* IN ENTRATA SOLO SE NON E' BOLLA LOCALE
422200970204    4C     BARNPG        IFEQ      5
422300970204     C     WTIBOL        ANDNE     'L'
422400970204     C     WABNDC        ANDEQ     0
422500970204     C                   Z-ADD     130           COMCAA
422600970204     C                   EXSR      CRTANM
422700970204    4C                   ENDIF
422800970204    3C                   ENDIF
422900970204     C*
423000970204     C* BOLLA GIACENTE FUORI POSTO
423100161201     c* per ora il fuori posto lo devo dare anche per le
423200161201     c*  giacenze a partare a mag giac. mentre con i 2gg in IMA
423300161201     c*  dovrò dare l'anomalia soltanto se già a IMG se spuntato in IMA
423400161201     c*  per le altre categorie errore sempre
423500970204    3C     BARSPG        IFNE      'G'
423600161021    3C     BARSPG        andne     'Z'
423700161201     C**** WGIAC         ANDEQ     'S'
423800161201     C     WGIAC         ANDne     ' '
423900970205     C     BARDFV        ANDGT     GCPDGC
424000170125     c* se WGIAC = "D" e filiale arrivo partita con procedura NEWIMA
424100170125     c*  non creao l'anomalia
424200170905    4c***                if        wgiac='D'  and allnewima<>'S'
424300170905     c***                movel     barlna        w003a
424400170905     c***                if         %lookup(w003a:skFilOk)>0
424500170905     c**+                eval      allnewima='Y'
424600170905     c***                else
424700170905     c***                clear                   allnewima
424800170905     c***                endif
424900170905    4c***                endif
425000170125     c
425100170125    4c                   if        wgiac='S' or (barspg<>'A' and
425200170125     c                             barnpg<>2) or
425300170125     c                             (wgiac='D' and allnewima=' ')
425400970204     C                   Z-ADD     140           COMCAA
425500970204     C                   EXSR      CRTANM
425600170125    4c                   endif
425700170125     c
425800970204    3C                   ENDIF
425900970204     C*
426000970204    2C                   ENDIF
426100970204     C**
426200970204     C* C H I U D O
426300970204     C* BOLLA CONSEGNA
426400970204    2C     BARNPG        IFEQ      4
426500970204     C                   Z-ADD     135           COMCAA
426600970204     C                   EXSR      CHIUAN
426700970204    2C                   END
426800970204     C* BOLLA ARRIVI
426900970204    2C     BARNPG        IFEQ      2
427000970204     C     BARNPG        OREQ      4
427100000523     C     BARNPG        OREQ      8
427200970204     C     BARSPG        OREQ      'A'
427300970204     C                   Z-ADD     130           COMCAA
427400970204     C                   EXSR      CHIUAN
427500970204    2C                   END
427600970204     C* BOLLA GIACENZE
427700970204    2C     BARNPG        IFEQ      3
427800970204     C     BARSPG        ANDEQ     'G'
427900161028    2C     BARNPG        orEQ      3
428000161028     C     BARSPG        ANDEQ     'Z'
428100970204     C                   Z-ADD     140           COMCAA
428200970204     C                   EXSR      CHIUAN
428300970204    2C                   END
428400970204     C*
428500970204    1C                   ENDIF
428600970204     C                   ENDSR
428700970218     C**************************************************************************
428800970218     C* SCRITTURA DI FNAGB PER AGGIORNAMENTO  BOLLE
428900970218     C**************************************************************************
429000970217     C     WRTAGB        BEGSR
429100970217     C* LA PRIMA VOLTA NON SCRIVO
429200041105    1C     WAGGF         IFEQ      'S'
429300970217     C     KSAV          CHAIN     FNAGB01L                           34
429400970217     C   34              CLEAR                   FNAGB000
429500970218     C                   MOVEL     SAVTBO        AGBTBO
429600970217     C                   MOVEL     KAGB          AGBAGB
429700970217     C                   MOVEL     SAVAAS        AGBAAS
429800970217     C                   MOVEL     SAVLNP        AGBLNP
429900970217     C                   MOVEL     SAVNRS        AGBNRS
430000970217     C                   MOVEL     SAVNSP        AGBNSP
430100970217     C     WFBS          IFEQ      'S'
430200970217     C                   MOVEL     WFBS          AGBFBS
430300970217     C                   ENDIF
430400970217     C     WFVC          IFEQ      'S'
430500970217     C                   MOVEL     WFVC          AGBFVC
430600970217     C                   ENDIF
430700990728     C     WFPC          IFEQ      'S'
430800990728     C                   MOVEL     WFPC          AGBFPC
430900990728     C                   ENDIF
431000970217     C*
431100981124     C   34              WRITE     FNAGB000                             99
431200970217     C  N34              UPDATE    FNAGB000
431300041105     c
431400041105     c* Se previsto scrivo anche per l'altra bolla
431500041105     c                   eval      wprimotbo=savtbo
431600041105     c
431700041105     c                   if        wagbart='S'
431800041105     c                   eval      savtbo='A'
431900041105     c                   EXSR      ALTROAGB
432000041105     c                   endif
432100041105     c                   eval      savtbo=wprimotbo
432200160421     c
432300160421     c* Scrivo record per la zona
432400160421     c                   if        wznc<>*blanks
432500160421     C                   CLEAR                   FNAGB000
432600160421     C                   MOVEL     SAVTBO        AGBTBO
432700160421     C                   MOVEL     wznc          AGBAGB
432800160421     C                   MOVEL     SAVAAS        AGBAAS
432900160421     C                   MOVEL     SAVLNP        AGBLNP
433000160421     C                   MOVEL     SAVNRS        AGBNRS
433100160421     C                   MOVEL     SAVNSP        AGBNSP
433200160421     C                   WRITE     FNAGB000                             99
433300160421     c                   endif
433400041105     c
433500970217    1C                   ENDIF
433600970218     C*
433700970218     C                   CLEAR                   WFBS
433800970218     C                   CLEAR                   WFVC
433900990728     C                   CLEAR                   WFPC
434000160421     C                   CLEAR                   Wznc
434100041105     C                   CLEAR                   Wagbart
434200970217     C                   ENDSR
434300041105     C**************************************************************************
434400041105     c* scrivo fnagb per l'altro tipo bolla
434500041105     C**************************************************************************
434600041105     c     ALTROAGB      begsr
434700041105     C     KSAV          CHAIN     FNAGB01L                           34
434800041105     c                   eval      agbtbo=savtbo
434900041105     c                   if        *in34
435000041105     c                   clear                   agbfbs
435100041105     C                   WRITE     FNAGB000                             99
435200041105     c                   else
435300041105     c* Se record trovato aggiorno solo i campi del peso/volume
435400041105     C     WFVC          IFEQ      'S'
435500041105     C                   MOVEL     WFVC          AGBFVC
435600041105     C                   ENDIF
435700041105     C     WFPC          IFEQ      'S'
435800041105     C                   MOVEL     WFPC          AGBFPC
435900041105     C                   ENDIF
436000041105     c
436100041105     C                   UPDATE    FNAGB000
436200041105     c                   endif
436300041105     c                   endSR
436400980120     C**************************************************************************
436500980120     C* SCRITTURA DI FNAGB PER AGGIORNAMENTO  BOLLE
436600980120     C**************************************************************************
436700980120     C     WR1AGB        BEGSR
436800110103      *
436900030528     c                   setoff                                       45
437000110103      *
437100980120     C     KEAGB         CHAIN     FNAGB01L                           34
437200980120     C   34              CLEAR                   FNAGB000
437300980120     C                   MOVEL     WTBO          AGBTBO
437400980120     C                   MOVEL     KAGB          AGBAGB
437500980120     C                   MOVEL     WDAAS         AGBAAS
437600980120     C                   MOVEL     WDLNP         AGBLNP
437700980120     C                   MOVEL     WDNRS         AGBNRS
437800980120     C                   MOVEL     WDNSP         AGBNSP
437900160726     c*
438000160726     c* Se richiesto, passo flag di rielaborazione statistiche partenze
438100160726     c                   if        welasta_p='S'
438200160726     c                   clear                   dagbflo
438300160726     c                   eval      §agbdsf='S'
438400160726     c                   eval      agbflo=dagbflo
438500160726     c                   endif
438600980120     C*
438700030528     C   34              WRITE     FNAGB000                             45
438800980120     C  N34              UPDATE    FNAGB000
438900980120     C*
439000160726     c                   clear                   welasta_p
439100160726     C*
439200980120     C                   ENDSR
439300970609     C**************************************************************************
439400970609     C* SCRITTURA DI FNSTA PER RIELABORAZIONE STATISTICA ARRIVI
439500970609     C**************************************************************************
439600970609     C     WRTSTA        BEGSR
439700970609     C* LA PRIMA VOLTA NON SCRIVO
439800970609    1C     WARDAM        IFGT      0
439900970609     C* VECCHIA DATA
440000970609     C     WSVDAM        IFGT      0
440100970609     C                   CLEAR                   SA2TLA
440200070221     C     dls45FLG      IFEQ      'C'
440300970609     C                   MOVEL     'L'           SA2TLA
440400970609     C                   END
440500970609     C                   MOVEL     'A'           SA2TRC
440600970609     C                   Z-ADD     WSVDAM        SA2DTS
440700970609     C                   MOVE      *ZEROS        SA2LNA
440800970609     C                   CALL      'FNLSA2R3'
440900970609     C                   PARM                    SA2TLA            1
441000970609     C                   PARM                    SA2TRC            1
441100970609     C                   PARM                    SA2DTS            8 0
441200970609     C                   PARM                    SA2LNA            3 0
441300970609     C                   END
441400970609     C* NUOVA DATA
441500970609     C                   Z-ADD     WARDAM        SA2DTS
441600970624     C                   MOVEL     'A'           SA2TRC
441700970609     C                   CALL      'FNLSA2R3'
441800970609     C                   PARM                    SA2TLA            1
441900970609     C                   PARM                    SA2TRC            1
442000970609     C                   PARM                    SA2DTS            8 0
442100970609     C                   PARM                    SA2LNA            3 0
442200970609    1C                   ENDIF
442300970609     C*
442400970609     C                   ENDSR
442500980928     C**************************************************************************
442600980928     C* MEMORIZZO I FOGLI NON TROVATI DA INVIARE A SEDE CON MSG
442700980928     C**************************************************************************
442800111018     C**   INVMSG        BEGSR
442900980928     C*
443000111018     C**                 Z-ADD     1             EE                3 0
443100111018     C**                 SETON                                        95
443200111018     C**                 CLEAR                   WPRES
443300111018     C**                 MOVE      WNFV          WNFVA             5
443400111018    4C**   *IN95         DOWEQ     *ON
443500111018     C**   WPRES         ANDEQ     ' '
443600980928     C* IL FOGLIO CORRISPONDE: NON LO RIMEMORIZZO
443700111018     C**   WNFVA         LOOKUP    ERF(EE)                                95
443800111018    5C** 95WNPG          IFEQ      ERC(EE)
443900111018     C**   WFGS          ANDEQ     ERE(EE)
444000111018     C**   WNPS          ANDEQ     ERP(EE)
444100111018     C**                 MOVEL     'S'           WPRES             1
444200111018    5C**                 ENDIF
444300111018     C**                 ADD       1             EE
444400111018    4C**                 ENDDO
444500980928     C* SE NON E' PRESENTE RIMEMORIZZO
444600111018    4C**   WPRES         IFEQ      ' '
444700111018     C**                 Z-ADD     1             EE
444800111018     C**   '     '       LOOKUP    ERF(EE)                                95
444900111018     C** 95              MOVE      WNFVA         ERF(EE)
445000111018     C** 95              Z-ADD     WNPG          ERC(EE)
445100111018     C** 95              Z-ADD     WFGS          ERE(EE)
445200111018     C** 95              movel     Wnps          ERP(EE)
445300111018    5C**   EE            IFEQ      8
445400111018     C**                 EXSR      TRASMI
445500111018    5C**                 ENDIF
445600111018    4C**                 ENDIF
445700111018     C*+                 ENDSR
445800990120     C**************************************************************************
445900990120     C* SE DEVO CREO ANOMALIA 152 / 153
446000990120     C**************************************************************************
446100990120     C     CRT15X        BEGSR
446200990120     C** SE LA C.A. E' APERTA, VERIFICO SE AVARIA E RESO O DISTRUTTA
446300990120    1C     DCTDSD        IFEQ      'R'
446400990120     C     DCTDSD        OREQ      'D'
446500990126     C**
446600990126     C                   EXSR      CERTAD
446700990120     C* CONTROLLO SE TIPO ANOMALIA E' AVARIA
446800990120     C** CREO ANOMALIA 152 0 153
446900990120    2C     §TARGR        IFEQ      'V'
447000990120     C*
447100990120     C     DCTDSD        IFEQ      'R'
447200990120     C                   Z-ADD     152           COMCAA
447300990120     C                   ELSE
447400990120     C                   Z-ADD     153           COMCAA
447500990120     C                   ENDIF
447600990120     C**
447700990120     C                   EXSR      CRTANM
447800990120    2C                   ENDIF
447900990120    1C                   ENDIF
448000990120     C                   ENDSR
448100990120     C**************************************************************************
448200990120     C* CONTROLLO SE PRESENTE CA PER IL COLLO
448300990120     C**************************************************************************
448400990120     C     CTRDAN        BEGSR
448500990126     C*
448600990120     C                   CLEAR                   WTRDAN
448700990120     C     KBAR          CHAIN     FNDCD02L                           33
448800990126     C*
448900990120    1C     *IN33         IFEQ      *OFF
449000990120     C     DCDATB        ANDEQ     ' '
449100990120     C     KDCT          CHAIN     FNDCT01L                           33
449200050223      * se dctdt2 > 0 la c.a. non è valida x la filiale
449300050224     c                   If        Not *In33 and dctdt2 > 0
449400050224     c                   Eval      *In33 = *On
449500050224     c                   EndIf
449600050224     C
449700990120    2C     *IN33         IFEQ      *OFF
449800990120     C     DCTATB        ANDEQ     ' '
449900990126     C* TROVATA C.A. NON ANNULLATA
450000990120     C                   MOVEL     'S'           WTRDAN            1
450100990126     C**
450200990126     C* CONTROLLO SE DEVO IMPOSTARE FLAG COLLO RITROVATO
450300990126    3C     WRITRO        IFEQ      'S'
450400990126     C                   EXSR      CERTAD
450500990126    4C     §TARIT        IFEQ      'S'
450600990126     C     DCDDFV        ANDEQ     0
450700990126     C                   Z-ADD     BARDFV        DCDDFV
450800990126     C                   Z-ADD     BARFGS        DCDFGS
450900990126     C                   Z-ADD     BARNPS        DCDNPS
451000990126     C                   MOVEL     'S'           DCDFAR
451100990126     C                   UPDATE    FNDCD000
451200990126    4C                   ENDIF
451300990126    3C                   ENDIF
451400990126     C**
451500990126     C                   UNLOCK    FNDCD02L
451600990126     C**
451700990126     C** CONTROLLO SE DEVO CREARE LE ANOMALIE
451800990126    3C     WCREAN        IFEQ      'S'
451900990126     C**
452000990126     C* ANOMALIA 151 - COLLO DI CA CHIUSA
452100990126    4C     DCTDCH        IFGT      0
452200990121     C* IN ARRIVO NON LA CREO PER SPUNTA DI CATEGORIA 4
452300990126    5C     WCAT4         IFEQ      'N'
452400990121     C     BARNPG        ANDNE     4
452500000522     C     BARNPG        ANDNE     8
452600990121     C     WCAT4         OREQ      'S'
452700990120     C                   EXSR      CRE151
452800990126   X5C                   ELSE
452900990121     C                   Z-ADD     151           COMCAA
453000990121     C                   EXSR      CHIUAN
453100990126    5C                   ENDIF
453200990126   X4C                   ELSE
453300990120     C* ANOMALIE 152 O 153
453400990126    5C     DCDDCH        IFEQ      0
453500990120     C                   EXSR      CRT15X
453600990126    5C                   ENDIF
453700990126    4C                   ENDIF
453800990121    3C                   ENDIF
453900990120    2C                   ENDIF
454000990120     C**
454100990120   X1C                   ELSE
454200990120     C* ANNULLATO COME SE NON CI FOSSE
454300990120     C                   SETON                                        33
454400990120    1C                   ENDIF
454500990120     C                   ENDSR
454600990120     C**************************************************************************
454700990120     C* CREO ANOMALIA 151 SE LO PREVEDE LA CAUSALE CHIUSURA
454800990120     C**************************************************************************
454900990120     C     CRE151        BEGSR
455000990120     C     DCTCCH        IFNE      *BLANKS
455100990120     C                   CLEAR                   DSBS02
455200990120     C                   MOVEL     'C'           T02MOD
455300990120     C                   MOVEL     KNSIF         T02SIF
455400990121     C                   MOVEL     'CCH'         T02COD
455500990120     C                   MOVEL     DCTCCH        T02KE1
455600990120     C                   CALL      'TIBS02R'
455700990120     C                   PARM                    KPJBA
455800990120     C                   PARM                    DSBS02
455900990121    2C     T02ERR        IFEQ      ' '
456000990121     C                   MOVEL     T02UNI        DCCH
456100990121     C                   ELSE
456200990121     C                   MOVEL     'S'           §CHCRA
456300990121    2C                   ENDIF
456400990120     C                   ENDIF
456500990120     C**
456600990120     C* CREO
456700990120     C     DCTCCH        IFEQ      *BLANKS
456800990120     C     §CHCRA        OREQ      'S'
456900990120     C                   Z-ADD     151           COMCAA
457000990120     C                   EXSR      CRTANM
457100990120     C                   ENDIF
457200990120     C                   ENDSR
457300990126     C**************************************************************************
457400990126     C* CERCO TABELLA RECORD TIPO ANOMALIA TAD
457500990126     C**************************************************************************
457600990126     C     CERTAD        BEGSR
457700990126    1C     DCTTAD        IFNE      SAVTAD
457800990126     C                   CLEAR                   DSBS02
457900990126     C                   MOVEL     'C'           T02MOD
458000990126     C                   MOVEL     KNSIF         T02SIF
458100990126     C                   MOVEL     'TAD'         T02COD
458200990126     C                   MOVEL     DCTTAD        T02KE1
458300990126     C                   CALL      'TIBS02R'
458400990126     C                   PARM                    KPJBA
458500990126     C                   PARM                    DSBS02
458600990126     C**
458700990126    2C     T02ERR        IFEQ      ' '
458800990126     C                   MOVEL     T02UNI        DTAD
458900990126     C                   ELSE
459000990126     C                   MOVEL     'V'           §TARGR
459100990126     C                   MOVEL     'S'           §TARIT
459200990126    2C                   ENDIF
459300990126     C                   MOVEL     DCTTAD        SAVTAD
459400990126    1C                   ENDIF
459500990126     C                   ENDSR
459600030723     C**************************************************************************
459700030723     C* CERCO TABELLA VARIABILI PGM PARTENZE
459800030723     C**************************************************************************
459900030723     C     CERVPO        BEGSR
460000030723      *
460100030723      * CHAIN A VARIABILI PARTENZE PER PRENDERE MAX PESO E VOLUME CARICABILI
460200030723     C                   CLEAR                   DVPO
460300030723     C                   CLEAR                   DSBS02
460400030723     C                   MOVEL     'C'           T02MOD
460500030723     C                   MOVEL     KNSIF         T02SIF
460600030723     C                   MOVEL     'VPO'         T02COD
460700030723     C                   MOVEL     'VPO'         T02KE1
460800030723     C                   CALL      'TIBS02R'
460900030723     C                   PARM                    KPJBA
461000030723     C                   PARM                    DSBS02
461100030723     C**
461200030723    2C     T02ERR        IFEQ      ' '
461300030725     C                   MOVEL     T02UNI        DVPO
461400030723    2C                   ENDIF
461500110103      *
461600030723     C                   ENDSR
461700000927     C**************************************************************************
461800000927     C* CONTROLLO SE POSSO CREARE ANOMALIE DI MANCA RECOD SECONDO LA
461900000927     C*  ATA FOGLIO E I GIONRI DI PULIZIA
462000000927     C**************************************************************************
462100000927     C     CTRDAT        BEGSR
462200000927     C*
462300000927     C* A CAMBIO DATA RICONTROLLO
462400000927    1C     BARDFV        IFNE      SA2DFV
462500000927     C                   CLEAR                   WCRE10
462600000927     C*
462700000927     C* SE FOGLIO UGUALE A UDATE, VA SICURAMENTE BENE
462800000927    2C     BARDFV        IFEQ      WDATE1
462900000927     C                   MOVEL     'S'           WCRE10            1
463000000927   X2C                   ELSE
463100000927     C* CONTROLLO CON I GG DI PULIZIA
463200000927      * DATA FOGLIO MAGGIORE DATA ULTIMO UTILIZZO: POSSO CREARE ANOM
463300000927    3C     BARDFV        IFGE      PRIDAT
463400000927     C                   MOVEL     'S'           WCRE10            1
463500000927    3C                   ENDIF
463600000927    2C                   ENDIF
463700000927     C*
463800000927     C                   Z-ADD     BARDFV        SA2DFV
463900000927    1C                   ENDIF
464000130301     c*
464100130301     c* Se è da creare --> controllo se si tratta di fil che fa partire colli
464200130301     c*  di altro TFP e non la creo lo stesso
464300130301     c                   clear                   wlocalxco         1
464400130301     c
464500130301     c                   if        wcre10='S' and ( bolxco='W'  or
464600130301     c                             (wesbtt='N' and wesblt='N' and wesart='N'
464700130301     c                              and barnrs>0))
464800130301     c* se si tratta di spunta partenza --> creo la 57
464900130301     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
465000130301     c                             or barnpg=1
465100130301     c                   exsr      CtrXCO
465200130301     c                   if        Indx>0
465300130301     c                   eval      Wlocalxco='S'
465400130301     c                   endif
465500130301     c                   endif
465600130301     c                   endif
465700000927     C*
465800010607     C                   ENDSR
465900010607     C**************************************************************************
466000070131     C* MEMORIZZAZIONE SPUNTE DI CONSEGNA DOPPIE SU FNBRVE0F
466100010607     C**************************************************************************
466200010607     C     SRBRVE        BEGSR
466300070710     c* Se devo tenere una spuynta solo memeorizzo per categoria
466400070710     c*  e segnacollo
466500070710     c                   if        nst(WA)='1'
466600070710     c
466700070131     C     KBRVE2        SETLL     FNBRVE2L                               33
466800010607     C     *IN33         IFEQ      *OFF
466900010607     C* SPUNTA NON ESISTENTE: MEMORIZZO ANCHE LA VECCHIA
467000070131     C                   WRITE     FNBRVE
467100010607     C                   ENDIF
467200070131     C* Salvo campi di FNBRV che potrebbero rimanere invariati
467300070131     C* Li dovrò reimpostare dopo la scrittura di FNBRVe per poi
467400010608     C* fare i giusti aggiornamenti
467500010608     C                   MOVE      BRVATR        SAWATR
467600080317     C                   MOVE      BRVTSU        SAWTSU
467700080317     c                   movel     brv           sawbrv
467800010608     C* IN OGNI CASO MEMORIZZO LA SPUNTA NUOVA
467900010608     C                   MOVEL     'S'           WBRVE             1
468000010608     C                   EXSR      RIEMPI
468100010608     C                   CLEAR                   WBRVE
468200070131     C                   WRITE     FNBRVE
468300070131     C* Reimposto campi di FNBRV come da lettura iniziale
468400010608     C                   MOVE      SAWATR        BRVATR
468500080317     C                   MOVE      SAWtsu        BRVtsu
468600080317     C                   MOVE      SAWbrv        BRV
468700110103     c*
468800070710     c                   else
468900070710     c* Se devo memorizzare le spunte senza anomalia controllo per foglio
469000070710     C     KBRVE1        SETLL     FNBRVE1L                               33
469100070710     C     *IN33         IFEQ      *OFF
469200070710     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
469300070710     C                   WRITE     FNBRVE
469400070710     C                   ENDIF
469500070710     C                   ENDIF
469600070710     c*
469700010607     C                   ENDSR
469800020124     C**************************************************************************
469900020204     C* CERCO TERMINAL PARTENZA LNP COLLO
470000020124     C**************************************************************************
470100020204     C     TERPAR        BEGSR
470200020124     C                   CLEAR                   DSLV55
470300020124     C                   MOVEL     'P'           D55TPT
470400020124     C                   MOVEL     LNPB          D55LIN
470500020124     C                   MOVEL     BARDFV        D55DRF
470600020213     C                   EXSR      FNLV55
470700020204     C                   MOVE      D55TFP        LNPTFP
470800020204     C**
470900020204     C                   ENDSR
471000020204     C**************************************************************************
471100020204     C* CERCO TERMINAL ARRIVO LNA COLLO
471200020204     C**************************************************************************
471300020204     C     TERARR        BEGSR
471400020124     C**
471500020124     C* LINEA DI ARRIVO
471600020124     C     BARLNA        IFNE      SVVLNA
471700020124     C     LNPB          ORNE      SVVLNP
471800020204     C     BARDFV        ORNE      SVVDFV
471900020124     C*
472000020124     C* TEMINAL DI ARRIVO
472100020124     C                   CLEAR                   DSLV55
472200020124     C                   MOVEL     ' '           D55TPT
472300020124     C                   MOVEL     BARDFV        D55DRF
472400020124     C                   MOVEL     BARLNA        D55LIN
472500020124     C                   MOVEL     LNPB          D55LNP
472600020213     C                   EXSR      FNLV55
472700020124     C**
472800020213     C                   MOVEL     D55TFA        LNATFA
472900020124     C*
473000020124     C                   MOVEL     BARLNA        SVVLNA
473100020204     C                   MOVEL     LNPB          SVVLNP
473200020204     C                   MOVEL     BARDFV        SVVDFV
473300020124     C                   ENDIF
473400020124     C                   ENDSR
473500020124     C**************************************************************************
473600020124     C* AGGIORNO LE BOLLE PARTENZA
473700020124     C**************************************************************************
473800020124     C     AGGBLT        BEGSR
473900060223     C                   CLEAR                   WEND              1
474000020124     C* NON FACCIO AGGIORNAMENTI SE SPUNTA DI COLLO DISGUIDATO
474100070221    1C     dls45FLG      IFNE      '6'
474200020124     C*
474300020124     C                   MOVEL     'P'           WTIBOL            1
474400020124     C                   MOVEL     'S'           WBOPAR
474500020124     C*
474600020124     C* Se collo già consegnato e spunta partenza di pistola manuale
474700020124     C* ignoro la spunta
474800020124    3C     BARNPG        IFEQ      1
474900020124     C     BLTDCM        ANDNE     *ZEROS
475000020124     C     RPS(Y)        ANDEQ     'M'
475100020124     C                   MOVE      'N'           OKAGG
475200020201     C                   MOVEL     '1'           WEND
475300020204     C                   UNLOCK    FNBLT27L
475400020201     C                   GOTO      ENDBLT
475500020124    3C                   END
475600020204     C*  SE NON IMPOSTATO VOLUME E PESO DA PARTENZA
475700020204     C     WWFVC         IFEQ      ' '
475800020124     C                   MOVEL     'P'           WWFVC
475900020204     C                   ENDIF
476000020124     C*
476100020124     C                   MOVEL     'P'           BLTTBO
476200020124     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
476300020124     C* NE PRECEDENTE
476400020124     C     FLGPRE        IFNE      'A'
476500020124     C     BLTSPE        ANDNE     SAVSPE
476600020124     C     WAGGF         IFEQ      'S'
476700020124     C                   EXSR      WRTAGB
476800020124     C                   CLEAR                   WAGGF
476900020124     C                   ENDIF
477000020124     C                   MOVEL     BLTSPE        SAVSPE
477100020124     C                   ENDIF
477200020124     C*
477300020124    3C     BARCAN        IFNE      ' '
477400020124     C* NON MEMORIZZO ANOMALIA "E" SU DETTAGLIO COLLO PERCHE' LA NON
477500020124     C* INDICA UNA VERA E PROPRIA ANOMALIA MA SOLO UN ERRORE PER LINEA
477600020124     C* MANCANTE SUL FOGLIO AL MOMENTO DELLA SPUNTA DEL COLLO
477700020124     C     BARCAN        ANDNE     'E'
477800020124     C                   MOVEL     BARCAN        BLTCAN
477900020124     C                   MOVEL     'S'           WAGGF             1
478000020124    3C                   END
478100160421     c
478200160421     c* Se zona bolla <> zona spunta --> riaggiorno zona bolla
478300160422     c                   if        bolznc<>'  ' and
478400160429     c                             bolznc<>%editc(barznc:'X')  and
478500160503     c                             barnpg<>8
478600160503     c                   if        tps(Y)= 'L' or tps(y)='C'
478700160421     c                   movel     barznc        wznc
478800160421     C                   MOVEL     'S'           WAGGF             1
478900160421     c                   endif
479000160503     c                   endif
479100020124     C*
479200041105     c                   clear                   wagvolart         1
479300041105     c                   clear                   wagpesart         1
479400041105     c
479500020124    3C     BARVUC        IFGT      0
479600020124     C* AGGIORNO IL VOLUME CML SOLO SE IL VOLUME SUL COLLO:
479700020124     C* - E' = 0
479800020124     C* - E' DELL'ARRIVO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
479900020124     C**- E' DEL TRANSITO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
480000020124    4C     BLTVUC        IFEQ      0
480100020124     C     BLTVUC        ORNE      0
480200020214     C     BLTFVC        ANDNE     'P'
480300020124     C     WWFVC         ANDEQ     'P'
480400041013     C                   MOVEL     'S'           WFVC
480500020124     C                   Z-ADD     BARVUC        BLTVUC
480600041105     c                   eval      wagvolart='S'
480700020124     C                   MOVEL     'S'           WAGGF             1
480800020124     C                   MOVEL     WWFVC         BLTFVC
480900020124    4C                   END
481000020124     C*
481100020124    3C                   END
481200020124     C*STESSO GIRO DEL VOLUME ANCHE PER IL PESO
481300020124    3C     BARPUC        IFGT      0
481400020124     C* AGGIORNO IL VOLUME CML SOLO SE IL VOLUME SUL COLLO:
481500020124     C* - E' = 0
481600020124     C* - E' DELL'ARRIVO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
481700020124     C**- E' DEL TRANSITO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
481800020124    4C     BLTPUC        IFEQ      0
481900020124     C     BLTPUC        ORNE      0
482000020214     C     BLTFPC        ANDNE     'P'
482100020124     C     WWFVC         ANDEQ     'P'
482200041012     c                   movel     'S'           wfpc
482300020124     C                   Z-ADD     BARPUC        BLTPUC
482400041105     c                   eval      wagpesart='S'
482500020124     C                   MOVEL     'S'           WAGGF             1
482600020124     C                   MOVEL     WWFVC         BLTFPC
482700020124    4C                   END
482800020124     C*
482900020124    3C                   END
483000020124     C*
483100020124     C* SPUNTA LOCALE IN DISGUIDO --> AGGIORNO LA DATA ENTRATA TRANSITO
483200020124    3C     WSPLOC        IFEQ      'D'
483300020124     C                   MOVEL     BARDFV        BLTDET
483400020124     C                   MOVEL     BARFGS        BLTFLP
483500020124     C                   CLEAR                   BLTDUT
483600020124     C                   MOVEL     'S'           WAGGF             1
483700020124     C*
483800020124   X3C                   ELSE
483900020124     C*
484000020124     C* SE NON C'E' AGGIUNGO DATA E PISTOLA
484100020124     C* SOLO SE NON E' SPUNTA DISGUIDO IN AREA
484200020124    4C     BLTDSE        IFEQ      0
484300020124     C                   MOVEL     BARDFV        BLTDSE
484400020124     C                   MOVEL     BARNPS        BLTNPE
484500020124     C                   MOVEL     'S'           WAGGF             1
484600020124   X4C                   ELSE
484700020124     C*
484800020124     C* SE E' QUESTA LA SPUNTA CHE HA AGGIORNATO IL COLLO RIAGGIORNO
484900020124    5C   05BLTNPE        IFEQ      BARNPS
485000020124     C                   MOVEL     BARDFV        BLTDSE
485100020124     C                   MOVEL     'S'           WAGGF             1
485200020124    5C                   ENDIF
485300020124     C*
485400020124     C* RIAGGIRNO COLLO SE PRECEDENTE SPUNTA NON VALIDA E ATTUALE VALID
485500020124    5C  N05BLTNPE        IFNE      BARNPS
485600020124     C     RPS(Y)        ANDEQ     'L'
485700020124     C                   MOVEL     BLTNPE        ALF2
485800020124     C*
485900020124     C* CERCO RAGGRUPPAMENTO PISTOLA
486000020124     C                   EXSR      RICRAG
486100020124     C*
486200020124    6C     RPS(Z)        IFEQ      'M'
486300020124     C     RPS(Z)        OREQ      'A'
486400020124     C                   MOVEL     BARDFV        BLTDSE
486500020124     C                   MOVEL     BARNPS        BLTNPE
486600020124     C                   MOVEL     'S'           WAGGF             1
486700020124    6C                   END
486800020124    5C                   END
486900020124     C*
487000020124    4C                   END
487100020124    3C                   END
487200020124     C*
487300041105     C*  Se FLG3 = A non devo aggiornare fnblt perchè richiamo solo per
487400041105     c*  gli arrivi
487500070208    3C     dls45flg2     IFNE      'A'
487600020124     C                   UPDATE    FNBLT000
487700041105     c* Se aggiornato peso e volume e collo partito, aggiorno anche ART
487800041105     c                   if        bltdfv>0 and
487900160421     c                             (wagvolart='S' or wagpesaRT='S')
488000041105     C     KBAR          CHAIN     FNART27L                           3434
488100041105     C                   IF        NOT *IN34
488200041105     c* VOLUME
488300041105     C                   if        wagvolart='S'
488400041105     c* Se lan fedex non aggiorno
488500041105     c                   if        artvuc=0 or ntwbar<>'FED'
488600041105     c                   eval      wagbart='S'
488700041105     c                   eval      artvuc=bltvuc
488800041105     c                   eval      artfvc=bltfvc
488900041105     c                   endif
489000041105     c                   endif
489100041105     C                   if        wagpesart='S'
489200041105     c                   eval      wagbart='S'
489300041105     c                   eval      artpuc=bltpuc
489400041105     c                   eval      artfpc=bltfpc
489500041105     c                   endif
489600160421     c
489700041105     c                   update    fnart000
489800041105     C                   ENDIF
489900041105     C                   ENDIF
490000041105     C
490100020124     C**
490200020124     C* SE RICHIAMATO AGGIORNO SUBITO LA BOLLA
490300110103    5c   02              if        wfpc='S' or wfvc='S'
490400160421     c                             or wznc<>*blanks
490500020124     C                   SETOFF                                       3489
490600020124     C     KBLT          CHAIN     FNBLP01L                           3489
490700041021     c                   if        not *in34 and not *in89
490800041012     c                   if        wfvc='S'
490900041012     C                   MOVEL     99999         BLPNCR
491000041012     c                   endif
491100041012     c                   if        wfpc='S'
491200041012     C                   MOVEL     99999         BLPNCp
491300041012     c                   endif
491400160421     c                   if        wznc<>*blanks
491500160421     c                   movel     wznc          blpznc
491600160421     c                   endif
491700041012     c
491800041012     C                   UPDATE    FNBLP000
491900050223     c
492000050223     c* Aggiorno subito anche la bolla arrivo
492100050224     c                   if        wagbart='S'
492200050223     C     KBLT          CHAIN     FNarb01L                           3489
492300050223     C                   MOVEL     'E'           arbfbs
492400160421     c                   if        wznc<>*blanks
492500160421     c                   movel     wznc          arbznc
492600160421     c                   endif
492700050224     c  n89
492800050224     cann34              update    fnarb000
492900050224     c                   clear                   wagbart
493000050224     c                   endif
493100041012     c
493200041012     c                   clear                   wfpc
493300041012     c                   clear                   wfvc
493400160421     c                   clear                   wznc
493500041012     c                   endif
493600020124    5C                   ENDIF
493700020124     C**
493800020124   X3C                   ELSE
493900020124     C                   UNLOCK    FNBLT27L
494000020124    3C                   ENDIF
494100020124     C*
494200020213     C*CHIUDO EVENTUALI ANOMALIE DI DISGUIDO,SE COLLO NON DEL TFP
494300020124     C     BARNRS        IFGT      *ZEROS
494400020214     C*
494500020213     C                   CLEAR                   DSLV55
494600020213     C                   MOVEL     'P'           D55TPT
494700020213     C                   Z-ADD     BARLNP        D55LIN
494800020214     C                   Z-ADD     DSPB          D55DRF
494900020213     C                   EXSR      FNLV55
495000020131     C**
495100020213     C     D55TFP        IFNE      LNPTFP
495200020124     C                   MOVEL     'S'           SAVMAC
495300020124     C                   Z-ADD     200           COMCAA
495400020124     C                   EXSR      CHIUAN
495500020124     C*
495600020124     C                   Z-ADD     55            COMCAA
495700020124     C                   EXSR      CHIUAN
495800020124     C*
495900020124     C                   Z-ADD     56            COMCAA
496000020124     C                   EXSR      CHIUAN
496100020131     C                   ENDIF
496200020124     C*
496300020124     C                   CLEAR                   SAVMAC
496400020124     C                   END
496500020124     C*
496600020124   X1C                   ELSE
496700020124     C                   UNLOCK    FNBLT27L
496800020124    1C                   ENDIF
496900020124     C**
497000020201     C     ENDBLT        ENDSR
497100020131     C**************************************************************************
497200020131     C* AGGIORNO LE BOLLE ARRIVO
497300020131     C**************************************************************************
497400020131     C     AGGART        BEGSR
497500020201     C                   CLEAR                   WEND
497600020131     C*
497700020131     C     WTIBOL        IFEQ      'P'
497800020131     C                   MOVEL     'L'           WTIBOL
497900020131     C                   ELSE
498000020131     C                   MOVEL     'A'           WTIBOL
498100020131     C                   ENDIF
498200020131     C*
498300020131     C* E S I S T E   B O L L A   A R R I V O
498400020131    2C     WESART        IFEQ      'S'
498500020131     C* SE FLG=6 SVINCOLO RECORD
498600070221     C     dls45FLG      IFEQ      '6'
498700020131     C                   UNLOCK    FNART27L
498800020131     C                   ENDIF
498900020131     C**
499000020131     C     BARNPS        IFEQ      89
499100020131     C                   UNLOCK    FNART27L
499200020131     C                   ELSE
499300020214     C     ARTFLP        IFEQ      0
499400020214     C                   Z-ADD     ARTDFV        COMDFV
499500020214     C                   ELSE
499600020214     C                   Z-ADD     ARTDUT        COMDFV
499700020214     C                   ENDIF
499800020131     C                   MOVE      ARTDCM        WATDCM
499900020131     C                   MOVE      ARTDAM        SAVDAM
500000020131     C                   MOVEL     'S'           WBOAR             1
500100020131     C*
500200020131     C* SE FLG=6 CREO ANOMALIA 61
500300160624     c* Anomalia 61 eliminata
500400160624     C***  dls45FLG      IFEQ      '6'
500500160624     C***                EXSR      CRTA60
500600160624     C***                MOVEL     '1'           WEND
500700160624     C***                GOTO      ENDART
500800160624     C***                ENDIF
500900020131     C*
501000020131     C     WWFVC         IFEQ      *BLANKS
501100020131     C                   MOVEL     'A'           WWFVC
501200020131     C                   END
501300020131     C*
501400020131     C* LA PRIMA VOLTA MEMORIZZO LA SPEDIZIONE
501500020131     C                   MOVEL     'A'           ARTTBO
501600020131     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
501700020131     C* NE PRECEDENTE
501800020131     C     FLGPRE        IFNE      'A'
501900020131     C     ARTSPE        ANDNE     SAVSPE
502000020131     C     WAGGF         IFEQ      'S'
502100020131     C                   EXSR      WRTAGB
502200020131     C                   CLEAR                   WAGGF
502300020131     C                   ENDIF
502400020131     C                   MOVEL     ARTSPE        SAVSPE
502500020131     C                   ENDIF
502600160422     c* zona bolla
502700160422     c                   if        bolznc<>'  '   and
502800160429     c                             bolznc<>%editc(barznc:'X') and
502900160503     c                             barnpg<>8
503000160503     c                   if        tps(Y)= 'L' or tps(y)='C'
503100160422     c                   movel     barznc        wznc
503200160422     C                   MOVEL     'S'           WAGGF             1
503300160422     c                   endif
503400160503     c                   endif
503500020131     C*
503600020131     C* CODICE ANOMALIA
503700041012     c                   exsr      AGGCAN
503800020131     C*
503900020131     C* VOLUME DA MACCHINA SMISTACOLLI
504000041012     C                   EXSR      AGGVOL
504100030604     C*
504200020131     C* PESO DA MACCHINA SMISTACOLLI
504300041012     C                   EXSR      AGGpes
504400110103     c*
504500110103     c*
504600091209     c* Se chi spara è la linea di arrivo del collo o il suo gestore
504700091209     c* --> aggiorno la data più bassa di arrivo 1° collo
504800091211     C* CARICO LA L6 DELLA FILIALE GESTIONE
504900091211     C     BPES          IFNE      SAVPES
505000091211     C                   MOVEL     BPES          SAVPES
505100091211     C                   EXSR      CARL6
505200091211     C                   ENDIF
505300110103      *
505400091211     C     BARLNA        LOOKUP    L6                                     88
505500150615     c*
505600150817     c* non aggiorno più data e ora arrivo alla filiale finale di arrivo
505700150817     c*  er l'estero perchè creeremo apposito campo per
505800150817     c*  indicare la data di arrivo all'hub/terminal di arrivo
505900150622     c***                if        not *in88 and  boltfa=BPES  and
506000150622     c***                          (ntwbar='EEX' or ntwbar='DPD' or
506100150622     c***                          ntwbar='FED')
506200150622     c***                seton                                        88
506300150622     c***                endif
506400110103      *
506500150817      * tolto test su data spedizione dal 2009/12 in poi
506600150817    3c                   if        *in88 or artlna=bpes
506700091211     c     kar52         chain     fiar501l
506800091209    4c                   if        %found(fiar501l)
506900091209     c                   movel     ar5uni        dar5gen
507000091209     c                   else
507100091209     c                   clear                   dar5gen
507200091209    4c                   endif
507300110103     c*
507400091209     c                   clear                   warrdt
507500091209     c                   clear                   warrhm
507600091209    4c                   if        §ar5arrdt>*zeros
507700091209     c                   movel     §ar5arrdt     warrdt            8 0
507800091209    5c                   if        §ar5arrhm>*zeros
507900091209     c                   movel     §ar5arrhm     warrhm            4 0
508000091209    5c                   endif
508100091209    4c                   endif
508200110103     c*
508300101129     c* Uso data e ora immissione se non superiore all'ora caricamento
508400150817     c                   if        (bardfs<brvdcs) or
508500150817     c                             (bardfs=brvdcs and barhms<brvhcs)
508600101130     c                   movel     bardfs        datReale
508700101130     c                   movel     barhms        oraReale
508800101129     c                   else
508900101129     c                   movel     brvdcs        datReale
509000101129     c                   movel     brvhcs        oraReale
509100101129     c                   endif
509200110103     c*
509300091211     c                   if         warrdt=0  or
509400101129    4c                             (warrdt>0 and warrdt>datReale) or
509500101129     c                             (warrdt=datReale and warrhm>oraReale)
509600091209     c                   exsr      AggioFIAR5
509700110103     c*
509800091209   x4c                   else
509900091209     c                   unlock    fiar501l
510000091209    4c                   endif
510100091209    3c                   endif
510200091209      **
510300020131    3C     ARTDAM        IFEQ      0
510400020131     C                   MOVEL     BARDFV        ARTDAM
510500020131     C                   MOVEL     'S'           WAGGF             1
510600020131     C                   MOVEL     BARNPS        ARTNAP
510700050509     c* Imposto flag ARTFL2= 'P' se catagoria di spunta è tipica processo
510800050509     c*  operativo partenza
510900050509     c                   if        barnpg=5 or (barnpg=1 and wdefl=' ') or
511000050509     c                             (barnpg=3  and barspg='P')
511100050509     c                   eval      artfl2='P'
511200050509     c                   else
511300050509     c                   eval      artfl2=' '
511400050509     c                   endif
511500020131     C*
511600020131     C                   MOVEL     ' '           ARTDAB
511700020131     C                   SETON                                        03
511800020131     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
511900160624     c* ANOMALIE 60 E 61 ELIMINATE
512000160624     C***                EXSR      CRTA60
512100020131     C*
512200020131   X3C                   ELSE
512300020131    4C   05ARTNAP        IFEQ      BARNPS
512400020131     C                   MOVEL     BARDFV        ARTDAM
512500020131     C                   MOVEL     'S'           WAGGF             1
512600020131    4C                   ENDIF
512700020131     C*
512800020131     C* 'D' DI DISABILITAZIONE COLLO :
512900020131     C*   SE RISPUNTATO LANCIO PGM DI ABILITAZIONE X RIABILITARE BOLLA
513000020131    4C  N05ARTDAB        IFEQ      'S'
513100020131     C                   SETON                                        03
513200020131     C                   MOVEL     ' '           ARTDAB
513300040324     C                   MOVEL     'S'           WAGGF             1
513400020131    4C                   ENDIF
513500020131     C*
513600020131    4C  N05ARTNAP        IFNE      BARNPS
513700020131     C     RPS(Y)        ANDEQ     'L'
513800020131     C                   MOVEL     ARTNAP        ALF2              2
513900020131     C*
514000020131     C* CERCO RAGGRUPPAMENTO PISTOLA
514100020131     C                   EXSR      RICRAG
514200020131     C*
514300020131    5C     RPS(Z)        IFEQ      'M'
514400020131     C     RPS(Z)        OREQ      'A'
514500020131     C                   MOVEL     'S'           WAGGF             1
514600020131     C                   MOVEL     BARDFV        ARTDAM
514700020213     C                   MOVEL     BARNPS        ARTNAP
514800050509     c* Imposto flag ARTFL2= 'P' se catagoria di spunta è tipica processo
514900050509     c*  operativo partenza
515000050509     c                   if        barnpg=5 or (barnpg=1 and wdefl=' ') or
515100050509     c                             (barnpg=3  and barspg='P')
515200050509     c                   eval      artfl2='P'
515300050509     c                   else
515400050509     c                   eval      artfl2=' '
515500050509     c                   endif
515600020131     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
515700160624     c* anomalie 60 e 61 eliminate
515800160624     C***                EXSR      CRTA60
515900020131     C*
516000020131    5C                   ENDIF
516100020131    4C                   ENDIF
516200020131     C*
516300020131    3C                   END
516400020131     C*
516500020131     C                   Z-ADD     ARTDCM        WDCM
516600020131     C**
516700070208    3C     dls45flg2     IFNE      'A'
516800020131     C                   UPDATE    FNART000
516900020131     C                   ELSE
517000020131     C                   UNLOCK    FNART27L
517100020131    3C                   ENDIF
517200020131     C*
517300050223     C     KARB          CHAIN(N)  FNARB01L                           32
517400020131     C*
517500020131    3C     *IN32         IFEQ      *OFF
517600020131     C*
517700070208    4C     dls45flg2     IFNE      'A'
517800020131     C* SOLO SE PGM RICHIAMATO:
517900020131     C* SE VARIATA DATA DI ARRIVO SULLA BOLLA RIELABORO LA STATISTICA
518000020131    5C     *IN02         IFEQ      *ON
518100020131     C     ARTDAM        ANDNE     SAVDAM
518200020131    6C     ARTDAM        IFNE      WARDAM
518300020131     C     SAVDAM        ORNE      WSVDAM
518400020131     C                   EXSR      WRTSTA
518500020131     C                   MOVE      ARTDAM        WARDAM
518600020131     C                   MOVE      SAVDAM        WSVDAM
518700020131    6C                   END
518800020131    5C                   END
518900020131     C*
519000020131     C                   MOVEL     ARBNDC        WABNDC
519100040430     C                   MOVEL     ARBcca        WABcca
519200020131     C   03              MOVEL     'S'           WFBS
519300020131     C*
519400020131    4C                   ENDIF
519500020131     C**
519600020131     C** VEDO SE BOLLA GIACENTE
519700020131    4C     ARBFBC        IFEQ      'G'
519800050331     C     KGCA          CHAIN     Tigcp21L                           34
519900161007     c  n34              z-add     gcpmgc        gcpdgc
520000161007     c  n34              movel     gcpagc        gcpdgc
520100161007    5C  N34GCPFAS        IFLT      35
520200161007    5C     GCPFAS        oreq      36
520300161007     c* Verifico tramite driver se il collo dove essere in IMG
520400161007     c                   clear                   fnlgi1ds
520500161007     c                   eval      ilgi1aas=arbaas
520600161007     c                   eval      ilgi1lnp=arblnp
520700161007     c                   eval      ilgi1nrs=arbnrs
520800161007     c                   eval      ilgi1nsp=arbnsp
520900161007     c                   select
521000161007     c                   when      arbncp=arbncl
521100161007     c                   eval      ilgi1pkf=arbpkc
521200161007     c                   when      arbpkc>arbpkf
521300161007     c                   eval      ilgi1pkf=arbpkc
521400161007     c                   other
521500161007     c                   eval      ilgi1pkf=arbpkf
521600161007     c                   endsl
521700161007     c                   movel     fnlgi1ds      kpjbu
521800161007     c                   call      'FNLGI1R'
521900161007     c                   parm                    kpjba
522000161007     c                   parm                    tigcp00ds
522100161007     c                   movel     kpjbu         fnlgi1ds
522200161021     c* In questo caso la sped potrebbe essere sia in IMA che in IMG
522300170125     c*  quindi non devo dare alcun fuori posto con la nuova procedura
522400170125     c*  anche se ha Y (ovvero portato nel giorno in IMG:
522500170125     c*  potrebbe mancare un collo della sped spuntato succesivamente)
522600170125   5ac****               if        olgi1AMG='S'
522700170125     c
522800170125   5ac                   if        olgi1AMG='S' or olgi1AMG='Y'
522900161021     C                   MOVEL     'D'           WGIAC
523000161021  x5ac                   else
523100170125   5bc****               if        olgi1AMG='Y'  or olgi1inmg='S'
523200170125   5bc                   if        olgi1inmg='S'
523300161201     C                   MOVEL     'S'           WGIAC
523400161201   5bc                   endif
523500161201   5ac                   endif
523600161201     c
523700161201   5ac                   if        wgiac<>' '
523800020131     C* NON LA CONSIDERO GIACENTE SE IN QUESTO MOMENTO C'E' UNA BOLLA
523900020131     C*  FIGLIA IN CONSEGNA O TUTTE LE FIGLIE SONO BLOCCATE
524000020131     C                   MOVEL     'S'           WFIBLO            1
524100020131     C                   CLEAR                   WESFI
524200020131     C     KARB          SETLL     FNLBL02L
524300020131     C     KARB          READE     FNLBL02L                               29
524400020131     C**
524500020131    6C     *IN29         DOWEQ     *OFF
524600020131     C*
524700050223     C     KLBL          CHAIN(N)  FNARB01L                           28
524800020131    7C     *IN28         IFEQ      *OFF
524900020131     C                   MOVEL     'S'           WESFI             1
525000020131     C* FIGLIA IN CONSEGNA
525100020131     C* NON LA CONSDIERO SE E' CONSEGNATA
525200020131   7AC     ARBDCM        IFEQ      0
525300020131    8C     ARBNDC        IFNE      0
525400020131     C                   SETON                                        29
525500020131     C                   CLEAR                   WGIAC
525600020131   X8C                   ELSE
525700020131     C*
525800020131     C* SE NON E' NEMMENO BLOCCATA MEMORIZZO
525900020131    9C     ARBFBC        IFNE      'B'
526000020131     C                   MOVEL     'N'           WFIBLO
526100020131    9C                   ENDIF
526200020131    8C                   ENDIF
526300020131   7AC                   ENDIF
526400170125    7C                   ENDIF
526500020131     C*
526600020131     C  N29KARB          READE     FNLBL02L                               29
526700020131    6C                   ENDDO
526800020131     C*
526900020131     C* SE TUTTE LE FIGLIE SONO BLOCCATE --> ESCLUDO
527000161201    6C     WGIAC         IFNE      ' '
527100020131     C     WESFI         ANDEQ     'S'
527200020131     C     WFIBLO        ANDEQ     'S'
527300020131     C                   CLEAR                   WGIAC
527400020131    6C                   ENDIF
527500020131     C*
527600161021   5aC                   ENDIF
527700161007    5C                   ENDIF
527800020131    4C                   ENDIF
527900020131    3C                   ENDIF
528000020131     C*
528100020131     C** COLLO EXPORT: VEDO SE CREARE L'ANOMALIA COLLO INVIATO PARTNER
528200020131     C*  MA RISPARATO IN IMA O IN PARTENZA/ARRIVO
528300020131     C*  NON CREO ANOMALIA SE E' DATA =99999999 PERCHE' SI TRATTA DI
528400020131     C*  DIROTTAMENTO E QUINDI VALGONO I COLLI DELLA FIGLIA E NON
528500020131     C*  DELLA MAMMA
528600020212    3C     NTWBAR        IFEQ      'EEX'
528700060322    3C     NTWBAR        oreq      'DPD'
528800060322    3C     NTWBAR        oreq      'FED'
528900060322    4C     BARDFV        ifGT      ARTDTR
529000020131     C     ARTDTR        ANDGT     0
529100020131     C     ARTDTR        ANDNE     *HIVAL
529200020131     C** CREO ANOMALIA 600 O 620
529300020131     C                   MOVEL     'S'           W60X              1
529400060322    4C                   ENDIF
529500060322    3C                   ENDIF
529600020131    2C                   ENDIF
529700020131     C*
529800020131   X2C                   ELSE
529900020131     C*
530000070221   2AC     dls45FLG      IFNE      '6'
530100020131     C** CREO LE ANOMALIE DI MANCA BOLLA SOLO SE NON SONO PASSATI I
530200020131     C*  GG DIPULIZIA BOLLE ARRIVI (PER EVITARE MANCA RECORD ERRATI)
530300020131     C                   EXSR      CTRDAT
530400020131     C**
530500020131   2BC     WCRE10        IFEQ      'S'
530600130301     c     wlocalxco     andne     'S'
530700020131     C**
530800020131     C* ANOMALIA: "MANCA BOLLA" --> ANCHE PER SPEDIZIONE LOCALE SE
530900020131     C*  E' SPARATA NELLE CATEGORIE ARRIVI
531000020131     C*  IL FOGLIO DI DEFLUENZA E' COME UNA CATEGORIA ARRIVI
531100020131    3C  N05BARNPG        IFNE      5
531200020131     C     BARSPG        ANDNE     'P'
531300020131     C     BARSPG        ANDNE     'I'
531400020131     C     WTIBOL        ANDEQ     'L'
531500020131     C     WTIBOL        OREQ      'A'
531600020131     C* LA CATEGORIA 1 LA CONSIDERO SOLTANTO SE E' DEFLUENZA
531700020131     C*
531800020131    4C     BARNPG        IFNE      1
531900020131     C     WTIBOL        OREQ      'A'
532000020131     C     WDEFL         OREQ      '1'
532100020131     C                   Z-ADD     10            COMCAA
532200020131     C                   EXSR      CRTANM
532300020131     C* Se l'anomalia 10 esiste già a livello max. creo anom. 410 per
532400020131     C* segnalare il persistere del manca record
532500020131     C     WANM10        IFEQ      '1'
532600020131     C                   Z-ADD     410           COMCAA
532700020131     C                   EXSR      CRTANM
532800020131     C                   END
532900020131    4C                   ENDIF
533000020131    3C                   ENDIF
533100020131     C*
533200020131     C* SE LNA ESTERA VERIFICO SE C'E' EVENTO 2 PER CREARE ANOM.600/620
533300020212    3C     NTWBAR        IFEQ      'EEX'
533400060322    3C     NTWBAR        oreq      'DPD'
533500060322    3C     NTWBAR        oreq      'FED'
533600020131     C*
533700020131     C* SE LNA ESTERA APRO FILE FNEVS02L
533800020131    4C     WAPER         IFEQ      ' '
533900020131     C                   OPEN      FNEVS02L
534000020131     C                   MOVEL     '1'           WAPER             1
534100020131    4C                   ENDIF
534200020131     C*
534300020131     C                   MOVEL     'E2 '         KCEV
534400020131     C     KEVS          SETGT     FNEVS02L
534500020131     C     KEVS          READPE    FNEVS02L                               34
534600020131    4C     *IN34         IFEQ      *OFF
534700020131     C     EVSDEV        ANDLT     BARDFV
534800020131     C* CREO ANOMALIA 600 E 620
534900020131     C                   MOVEL     'S'           W60X
535000020131    4C                   ENDIF
535100020131    3C                   ENDIF
535200020131   2BC                   ENDIF
535300020131   2AC                   ENDIF
535400020131    2C                   ENDIF
535500020131     C**
535600020201     C     ENDART        ENDSR
535700020213     C**************************************************************************
535800020213     C* AGGIORNO LE BOLLE TRANSITO
535900020213     C**************************************************************************
536000020213     C     AGGBTT        BEGSR
536100020213     C                   MOVEL     'T'           WTIBOL
536200020214     C* MEMORIZZO CHE C'E' BOLLA TRANSITO, MI SERVE PER CRTA50
536300020214     C                   MOVE      BTTDFV        COMDFV
536400020213     C* LA PRIMA VOLTA MEMORIZZO LA SPEDIZIONE
536500020213     C                   MOVEL     'T'           BTTTBO
536600020213     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
536700020213     C* NE PRECEDENTE
536800020213     C     BTTSPE        IFNE      SAVSPE
536900020213     C     WAGGF         IFEQ      'S'
537000020213     C                   EXSR      WRTAGB
537100020213     C                   CLEAR                   WAGGF
537200020213     C                   ENDIF
537300020213     C                   MOVEL     BTTSPE        SAVSPE
537400020213     C                   ENDIF
537500020213     C*
537600020213    5C     BARCAN        IFNE      ' '
537700020213     C     BARCAN        ANDNE     'E'
537800020213     C                   MOVEL     BARCAN        BTTCAN
537900020213     C                   MOVEL     'S'           WAGGF             1
538000020213    5C                   END
538100020213     C*
538200020213    5C     BARVUC        IFGT      0
538300020213     C*
538400020213    6C     BTTVUC        IFEQ      0
538500041013     c                   movel     'S'           WFVC
538600020213     C                   Z-ADD     BARVUC        BTTVUC
538700020213     C                   MOVEL     'S'           WAGGF             1
538800020213     C                   MOVEL     'T'           BTTFVC
538900020213    6C                   END
539000020213     C*
539100020213    5C                   END
539200020213     C*PESO DA CML
539300020213    5C     BARPUC        IFGT      0
539400020213     C*
539500020213    6C     BTTPUC        IFEQ      0
539600041013     c                   movel     'S'           WFPC
539700020213     C                   Z-ADD     BARPUC        BTTPUC
539800020213     C                   MOVEL     'S'           WAGGF             1
539900020213     C                   MOVEL     'T'           BTTFPC
540000020213    6C                   END
540100020213     C*
540200020213    5C                   END
540300020213     C*
540400020213    5C     BTTDET        IFEQ      0
540500020213     C                   MOVEL     'S'           WAGGF             1
540600020213     C                   MOVEL     BARDFV        BTTDET
540700020213     C                   MOVEL     BARNPS        BTTPET
540800020213     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
540900020213     C*  SOLO SE NON E' PISTOLA FASULLA
541000160624     C**                 MOVEL     'TT'          WCCH
541100160624     C**   OPS(Y)        IFLT      040
541200160624     C**                 EXSR      CRTA60
541300160624     C**                 CLEAR                   WCCH
541400160624     C**                 ENDIF
541500020213     C*
541600020213   X5C                   ELSE
541700020213     C*
541800020213    6C   05BTTPET        IFEQ      BARNPS
541900020213     C                   MOVEL     'S'           WAGGF             1
542000020213     C                   MOVEL     BARDFV        BTTDET
542100020213    6C                   END
542200020213     C*
542300020213    6C  N05BTTPET        IFNE      BARNPS
542400020213     C     RPS(Y)        ANDEQ     'L'
542500020213     C                   MOVEL     BTTPET        ALF2
542600020213     C*
542700020213     C* CERCO RAGGRUPPAMENTO PISTOLA
542800020213     C                   EXSR      RICRAG
542900020213     C*
543000020213    7C     RPS(Z)        IFEQ      'M'
543100020213     C     RPS(Z)        OREQ      'A'
543200020213     C                   MOVEL     BARDFV        BTTDET
543300020213     C                   MOVEL     'S'           WAGGF             1
543400020213     C                   MOVEL     BARNPS        BTTPET
543500160624     c
543600020213     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
543700160624     c* Anomalia 61 eliminara
543800160624     C**                 MOVEL     'TT'          WCCH
543900160624     C**                 EXSR      CRTA60
544000160624     C**                 CLEAR                   WCCH
544100020213    7C                   END
544200020213    6C                   END
544300020213     C*
544400020213    5C                   END
544500020213     C*
544600020213     C                   UPDATE    FNBTT000
544700020213     C*
544800020213     C* SE INVENTARIO O IN CONSEGNA ANOMALIA: FUORI POSTO DEL TRANSITO
544900020213    5C  N05BARNPG        IFEQ      3
545000020213     C     BARSPG        ANDNE     'P'
545100020213     C     BARNPG        OREQ      4
545200020213     C     BARNPG        OREQ      8
545300020213     C                   Z-ADD     125           COMCAA
545400020213     C                   EXSR      CRTANM
545500020213    5C                   END
545600040430     c* Se il collo è in transito, posso chiudere i fuori posto in partenza
545700040430     C                   Z-ADD     120           COMCAA
545800040430     C                   EXSR      chiuan
545900110103     c*
546000040422     c*Chiudo eventuale anomalia di disguido presente sul collo, se
546100040422     c*  generate da un 2 livello dell'area di partenza
546200040503     C* SOLO SE SPUNTA DI PISTOLA REALE
546300040503     c                   if        fsl(y)<>'F'
546400040428     c                   eval      comcaa=200
546500040422     C*
546600040503     C     Kanm          chain(N)  FNANM000
546700040428     c                   if        %found(fnanm02l)
546800040428     C                   CLEAR                   DSLV55
546900040428     C                   MOVEL     'P'           D55TPT
547000040428     C                   Z-ADD     anmfle        D55LIN
547100040428     C                   Z-ADD     bardfv        D55DRF
547200040428     C                   EXSR      FNLV55
547300040428     c                   if        bpes=d55tfp
547400040428     c                   exsr      chiuan
547500040428     c                   endif
547600040428     c                   endif
547700040503     c                   endif
547800041012     c*
547900041012     c* Aggiorno anche la bolla arrivo con i dati del transito
548000041012     c                   EXSR      AGGARRIVO
548100110103     c*
548200020213     C                   ENDSR
548300020201     C**************************************************************************
548400020201     C* CONTROLLO COSA POSSO AGGIORNARE IN BASE A CHI SONO
548500020201     C**************************************************************************
548600020201     C     CTRAGG        BEGSR
548700020204     C                   CLEAR                   WAGPAR
548800020204     C                   CLEAR                   WAGARR
548900150622     C                   CLEAR                   Wbsptp
549000150622     C                   CLEAR                   Wbspspu
549100020213     C**
549200020213     C** CERCO TERMINAL DELLA LINEA DI ARRIVO
549300020213     C                   EXSR      TERARR
549400050603     c                   eval      Usalnatfa=lnatfa
549500020204     C*
549600070208     c*         Non considero se c'e'bolla transito
549700070208     c     wesbtt        ifne      'S'
549800150622     C* SE NON C'È BOLLA TRANSITO VERIFICO SE COLLI CHE PARTONO DA
549900150817     C*  più filiali se ho trovato la bolla in partenza non partita
550000150817     c*  e con data di entrata = 0
550100170224     c                   if        wesblt='S' and blpdse=0
550200170224     c                   if        Blpdt1=0  or blpft1='N' and blpft2='N'
550300150818     c* se si tratta di una spunta tipica partenza
550400150818     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
550500150818     c                             or barnpg=1
550600150622     c                   exsr      sr_fibsp
550700150622     c                   if        wbsptp<>' '      and
550800150622     c                             %lookup(bpes:sbsp)>0
550900150622     c                   eval      wbspspu='S'
551000150818     C                   MOVEL     'P'           WTIBOL            1
551100150622     c                   endif
551200170224     c                   endif
551300170224
551400150622     c                   endif
551500150818     c                   endif
551600020204     C*
551700020201     C*  1) P.O. EFFETTUA SPUNTA = LNP SPUNTA (DA BOLLA)
551800020204    2C     BPES          IFEQ      LNPB
551900020201     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.PARTENZA DELLA LNP SPUNTA
552000020201     C*     IN BASE ALLA DATA SPEDIZIONE SE TROVATA LA BOLLA
552100020204     C     BPES          OREQ      LNPTFP
552200020204     C                   MOVEL     'S'           WAGPAR            1
552300020204   X2C                   ELSE
552400020204     C*  3) TER.PARTENZA P.O. EFFETTUA SPUNTA (IN BASE ALLA DATA SPED
552500020204     C*     SE TROVATA LA BOLLA) = TER.PARTENZA LNP SPUNTA SEMPRE IN
552600020204     C*     BASE ALLA DATA SPEDIZIONE SE TROVATA BOLLA
552700021125     C                   MOVEL     'P'           WTITER            1
552800020201     C                   EXSR      TERPES
552900020215    3C     D55TFP        IFEQ      LNPTFP
553000020204     C                   MOVEL     'S'           WAGPAR            1
553100020204    3C                   ENDIF
553200020204    2C                   ENDIF
553300020201    1C                   ENDIF
553400020204     C**
553500020204     C** NON AGGIORNO BOLLE ARRIVO SE SPUNTA DELL'AREA PARTENZA,
553600020204     C*   SPUNTA TIPICA PARTENZA E P.O. SPUNTA = LNA SPUNTA O TFA SPU
553700021125     c* controllo solo se spunta tipica partenza: l'arrivo non mi
553800021125     c*  interessa. non so perchè lo avevo preso in considerazione!!
553900150817     c
554000150817     c* Non aggiorno nemmeno se bolla da "trasferire"
554100020204    1C     WAGPAR        IFEQ      'S'
554200150817     c     wbspspu       oreq      'S'
554300150817     c
554400021125     C**   BPES          ifeq      lnpb
554500021125    2C**   BPES          OREQ      BARLNA
554600021125     C**   BPES          OREQ      LNATFA
554700021125     C**   pestfa        OREQ      LNATFA
554800020204     C* PARTENZA NON DEFLUENZA
554900020204    3C     BARNPG        IFEQ      1
555000020204     C     WDEFL         ANDEQ     '0'
555100020204     C* ENTRATA
555200020204     C     BARNPG        OREQ      5
555300020204     C* I.M.P.
555400020204     C     BARNPG        OREQ      3
555500020204     C     BARSPG        ANDEQ     'P'
555600020204     C* I.M.I.
555700020204     C     BARNPG        OREQ      3
555800020218     C     BARSPG        ANDEQ     'I'
555900020204     C                   GOTO      NOAGAR
556000020204    3C                   ENDIF
556100021125    2C**                 ENDIF
556200020204    1C                   ENDIF
556300020204     C**
556400021022     c     wesbtt        ifne      'S'
556500020204     C* AGGIORNO BOLLE ARRIVO SE :
556600020204     C*  1) P.O. EFFETTUA SPUNTA = LNA SPUNTA
556700020205    1C                   SELECT
556800020204     C     BPES          WHENEQ    BARLNA
556900020204     C                   MOVEL     'S'           WAGARR            1
557000020205    2C     WAGPAR        IFEQ      'S'
557100020204     C                   MOVEL     'A'           WWFVC
557200020204     C                   MOVEL     'A'           WSPLOC            1
557300020205    2C                   ENDIF
557400020204     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.ARRIVO   DELLA LNA SPUNTA
557500020204     C*     IN BASE ALLA DATA SPUNTA
557600020204     C     BPES          WHENEQ    LNATFA
557700050603     C     BPES          oreq      bolTFA
557800020204     C                   MOVEL     'S'           WAGARR            1
557900020205    2C     WAGPAR        IFEQ      'S'
558000020204     C                   MOVEL     'A'           WWFVC
558100020204     C                   MOVEL     'T'           WSPLOC            1
558200020205    2C                   ENDIF
558300050603     c* imposto il terminal di arrivo =BPES
558400050603     c                   eval      Usalnatfa=BPES
558500020204   X1C                   OTHER
558600020204     C*  3) TER.ARRIVO P.O. EFFETTUA SPUNTA
558700020204     C*     = TER.ARRIVO LNA SPUNTA IN BASE ALLA DATA SPUNTA
558800020204    2C     PESTFA        IFEQ      LNATFA
558900050603    2C     PESTFA        oreq      bolTFA
559000020204     C                   MOVEL     'S'           WAGARR            1
559100020204     C                   MOVEL     'D'           WSPLOC
559200020204     C                   MOVEL     'A'           WWFVC
559300050603     c* imposto il terminal di arrivo =ter arrivo di BPES
559400050603     c                   eval      Usalnatfa=pestfa
559500110117   x2c                   else
559600110117     c
559700110117     c*  4) Se nessuna è vera ma chi spara è la capostipite della lna-£6
559800110117     c*     anche se è una incongruenza la considero valida ai fini dell'arrivo
559900110117     C* CARICO LA L6 DELLA FILIALE GESTIONE
560000110117    3C     BPES          IFNE      SAVPES
560100110117     C                   MOVEL     BPES          SAVPES
560200110117     C                   EXSR      CARL6
560300110117    3C                   ENDIF
560400110117    c
560500110117     C     BARLNA        LOOKUP    L6                                     34
560600110117    3c                   if        *in34
560700110117     C                   MOVEL     'S'           WAGARR            1
560800110117    4C     WAGPAR        IFEQ      'S'
560900110117     C                   MOVEL     'A'           WWFVC
561000110117     C                   MOVEL     'A'           WSPLOC            1
561100110117    4C                   ENDIF
561200110117    3C                   ENDIF
561300110117     c
561400020205    2C                   ENDIF
561500020205    1C                   ENDSL
561600021022     c*
561700021022    2C                   endif
561800020204     C**
561900070208     C     NOAGAR        ENDSR
562000150622     C**************************************************************************
562100150622     C*  vedo se lnp o ksc confermabile da più linee
562200150622     C**************************************************************************
562300150622     C     sr_fibsp      BEGSR
562400150622     c                   eval      WBspTp=*blanks
562500150622    2c                   if        blpccm>0
562600150622     c                   eval      w_clibo=blpccm
562700150622     c                   else
562800150622     c                   eval      w_clibo=blpksc
562900150622    2c                   endif
563000150622     c     w_clibo       chain     fibsp02l
563100150622     c                   if        %found(fibsp02l) and bspfpa='S'
563200150622     c                   eval      wBspTp='K'
563300150622     c****               if        not %found(fibsp02l) or bspfpa<>'S'
563400150622     c                   else
563500150817     c     LNPB          chain     fibsp01l
563600150622     c                   if        %found(fibsp01l) and bspfpa='S'
563700150622     c                   eval      wBspTp='L'
563800150622     c                   endif
563900150622     c                   endif
564000150622     c                   endsr
564100020201     C**************************************************************************
564200020201     C* TERMINAL DI PARTENZA E ARRIVO DEL P.O. EFFETTUA SPUNTA
564300020201     C**************************************************************************
564400020205     C     TERPES        BEGSR
564500020201     C* IMPOSTO TERMINAL PARTENZA E ARRIVO
564600020201     C                   CLEAR                   DSLV55
564700020205     C                   MOVEL     WTITER        D55TPT
564800020205     C                   MOVEL     LNPB          D55LNP
564900020205     C                   MOVEL     WDTRIF        D55DRF
565000020205     C                   MOVE      BPES          D55LIN
565100020205     C                   EXSR      FNLV55
565200020205     C                   ENDSR
565300020205     C**************************************************************************
565400020205     C* CALL A PGM FNLV55R
565500020205     C**************************************************************************
565600020205     C     FNLV55        BEGSR
565700020201     C                   CALL      'FNLV55R'
565800020201     C                   PARM                    DSLV55
565900020201     C**
566000020205     C     D55ERR        IFNE      ' '
566100020205     C                   MOVE      D55LIN        D55TFA
566200020205     C                   MOVE      D55LIN        D55TFP
566300020201     C                   END
566400020201     C                   ENDSR
566500041012     C**************************************************************************
566600041012     C* AGGIORNO CODICE ANOMALIA
566700041012     C**************************************************************************
566800041012     c     AGGCAN        BEGSR
566900041012    3C     BARCAN        IFNE      ' '
567000041012     C     BARCAN        ANDNE     'E'
567100070508     c     barcan        andne     artcan
567200041012     C**
567300070508     c* Aggiorno la testata e quindi scrivo AGB solo se artcan
567400070508     c*  è vuoto prima di questo aggiornamento
567500070508     c                   if        artcan=' '
567600041012     C                   MOVEL     'S'           WAGGF             1
567700070508     c                   endif
567800070508     C                   MOVEL     BARCAN        ARTCAN
567900041012     c                   endif
568000041012     c                   endsr
568100041012     C**************************************************************************
568200041012     C* AGGIORNO volume cml
568300041012     C**************************************************************************
568400041105     C     aGGVOL        BEGSR
568500041012    3C                   SELECT
568600041012    3C                   when      BARVUC = *zeros
568700041012      * NON SOSTITUISCO IL VOLUME ARRIVO SE LNA FEDEX
568800041012    3C                   when      ARTFVC = 'A' and ARTVUC > *zeros
568900041012    3C                                          and NTWBAR = 'FED'
569000041012      * IL VOLUME ARRIVO O TRANSITO LI SOSTITUISCO SE QUELLO DELLA
569100041012      * SPUNTA E' RILEVATO DALLA PARTENZA
569200041012    3C                   when       ARTVUC = *zeros  or
569300041012    3C                             (ARTVUC > *zeros and ARTFVC <> 'P'
569400041012     C                                              and WWFVC  =  'P')   or
569500041012    3C                             (ARTVUC > *zeros and ARTFVC =  'A'
569600041012     C                                              and WWFVC  =  'T')
569700041012     c                   movel     'S'           wfvc
569800041012     C                   MOVEL     'S'           WAGGF             1
569900041012     C                   Z-ADD     BARVUC        ARTVUC
570000041012     C                   MOVEL     WWFVC         ARTFVC
570100041012    3c                   ENDSL
570200041012     c                   ENDSR
570300041012     C**************************************************************************
570400041012     C* AGGIORNO Peso   cml
570500041012     C**************************************************************************
570600041012     c     AGGPES        BEGSR
570700041012    3C     BARPUC        IFGT      0
570800041012    4C     ARTPUC        IFEQ      0
570900041012     C* IL PESO ARRIVO O TRANSITO LI SOSTITUISCO SE QUELLO DELLA
571000041012     C* SPUNTA E' RILEVATO DALLA PARTENZA
571100041012     C     ARTPUC        ORNE      0
571200041012     C     WWFVC         ANDEQ     'P'
571300041012     C     ARTFPC        ANDNE     'P'
571400041012     C     ARTPUC        ORNE      0
571500041012     C     WWFVC         ANDEQ     'T'
571600041012     C     ARTFPC        ANDEQ     'A'
571700041012     c                   movel     'S'           wfpc
571800041012     C                   MOVEL     'S'           WAGGF             1
571900041012     C                   Z-ADD     BARPUC        ARTPUC
572000041012     C                   MOVEL     WWFVC         ARTFPC
572100041012    4C                   END
572200041012     C*
572300041012    3C                   END
572400041012     c                   ENDSR
572500091209     C**************************************************************************
572600091209     c*    Aggiorno fiar5 o lo scrivo se inesistente
572700091209     C**************************************************************************
572800091209     c     AggioFIAR5    BEGSR
572900091209     c
573000091209     c                   if        %found(fiar501l)
573100101129     c                   movel     datReale      §ar5arrdt
573200101129     c                   movel     oraReale      §ar5arrhm
573300091209     c                   movel(p)  dar5gen       ar5uni
573400091209      * se devo trasmettere alla sede sfleggo per la trasmissione
573500091209     c                   If        §Ar5Trse = 'S'
573600091209     c                   Clear                   Ar5Ft2
573700091209     c                   EndIf
573800091209     c                   update    fiar5000
573900091209      * Scrivo Fiar5
574000091209   x2c                   Else
574100091209     c                   Clear                   Fiar5000
574200091209     c                   Eval      Ar5Aas = ArtAas
574300091209     c                   Eval      Ar5Lnp = ArtLnp
574400091209     c                   Eval      Ar5Nrs = ArtNrs
574500091209     c                   Eval      Ar5Nsp = ArtNsp
574600091209     c                   Eval      Ar5Trd = 'GEN'
574700091209     c                   Move      WDATE1        Ar5Dac
574800091209     c                   time                    Ar5Orc
574900091209     c                   Clear                   Dar5Gen
575000101129     c                   movel     datReale      §ar5arrdt
575100101129     c                   movel     oraReale      §ar5arrhm
575200091209     c                   Movel(p)  DAr5Gen       Ar5Uni
575300091209     c                   Write     Fiar5000
575400091209    2c                   EndIf
575500091209     c                   ENDSR
575600041012     C**************************************************************************
575700041012     C* AGGIORNO anche la bolla di arrivo
575800041012     C**************************************************************************
575900041012     c     AGGARRIVO     BEGSR
576000041012     c* Aggiorno anche bolla  arrivi con dati del transito
576100130228     c                   clear                   aggioart          1
576200041012     C     KBAR          CHAIN     FNART27L
576300130228    1c                   if        %found(fnart27l)
576400041012     C                   MOVEL     'A'           ARTTBO
576500130228     c                   eval      Aggioart='S'
576600041012     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
576700041012     C* NE PRECEDENTE
576800041012     C     ARTSPE        IFNE      SAVSPE
576900041012     C     WAGGF         IFEQ      'S'
577000041012     C                   EXSR      WRTAGB
577100041012     C                   CLEAR                   WAGGF
577200041012     C                   ENDIF
577300041012     C                   MOVEL     ARTSPE        SAVSPE
577400041012     C                   ENDIF
577500110103     c*
577600041012     c* Codice anomalia: tengo sempre l'ultima anomalia
577700041012     C                   EXSR      AGGCAN
577800041012     C
577900041012     C* AGGIORNO DATA ENTRATA AL TRANSITO
578000041012     C                   Z-ADD     barDFV        ARTDET
578100041012     C                   MOVEL     barNPS        ARTPET
578200041012     C                   MOVEL     'S'           WAGGF             1
578300041012     C     barFGS        IFNE      ARTFLP
578400041012     C* FILIALE IN GESTIONE <> FILIALE TRANSITO
578500041012     C                   MOVEL     barFGS        ARTFLP
578600041012     C                   MOVEL     *ZEROS        ARTDUT
578700041012     C                   MOVEL     *ZEROS        ARTPUT
578800041012     C                   END
578900041012     c* peso e volume
579000041012     C     WWFVC         IFEQ      *BLANKS
579100041012     C                   MOVEL     'T'           WWFVC
579200041012     C                   END
579300041012     c                   EXSR      AGGVOL
579400041012     c                   EXSR      AGGpes
579500041012     c                   update    fnart000
579600130228     c
579700041013     c* Se trovata bolla, eseguo solo per disguido la CRTA60
579800160624     c* anomalie 60 e 61 eliminate
579900050322     c                   eval      savesart=wesart
580000041013     c                   movel     'S'           wesart
580100160624     c**                 if        wtibol='D'
580200160624     c**                 eval      wsavflg=dls45FLG
580300160624     c**                 eval      dls45FLG='6'
580400160624     c**                 exsr      CRTA60
580500160624     c**                 eval      dls45FLG=wsavflg
580600160624     c**                 endif
580700130228     c
580800130228    1c                   endif
580900041013     c* Chiudo una eventuale anomalia 15 aperta controllando le date anomali
581000041013     c*  e spunta
581100041013     c                   z-add     15            comcaa
581200041013     c                   eval      wcontrdfv='S'
581300041013     c                   eval      wstess='S'
581400041013     c                   eval      wchiu='S'
581500041013     c                   exsr      chiuan
581600130228     c
581700130228    1c                   if        Aggioart='S'
581800130228     c                   eval      wesart=savesart
581900130228     c                   endif
582000041013     c*
582100041012     c                   ENDSR
582200070208     C**************************************************************************
582300070208     C* Routine iniziale
582400070208     C**************************************************************************
582500070208     c     *inzsr        BEGSR
582600070208     C     KBAR          KLIST
582700070208     C                   KFLD                    BARLNP
582800070208     C                   KFLD                    BARLNA
582900070208     C                   KFLD                    BARNRS
583000070208     C                   KFLD                    BARNSC
583100170505     C     Kbolla        KLIST
583200170505     C                   KFLD                    comaas
583300170505     C                   KFLD                    comlnp
583400170505     C                   KFLD                    BARNRS
583500170505     C                   KFLD                    comnsp
583600070208     C     KBAR37        KLIST
583700070208     C                   KFLD                    bpes
583800070208     C                   KFLD                    BARLNP
583900070208     C                   KFLD                    BARLNA
584000070208     C                   KFLD                    BARNRS
584100070208     C                   KFLD                    BARNSC
584200160721     C     KBAR_t        KLIST
584300160721     C                   KFLD                    savflp
584400160721     C                   KFLD                    BARLNP
584500160721     C                   KFLD                    BARLNA
584600160721     C                   KFLD                    BARNRS
584700160721     C                   KFLD                    BARNSC
584800070208     C     KBAR1         KLIST
584900070208     C                   KFLD                    BARNPG
585000070208     C                   KFLD                    BARNfv
585100070208     C                   KFLD                    BARfgs
585200070208     C                   KFLD                    BARLNP
585300070208     C                   KFLD                    BARLNA
585400070208     C                   KFLD                    BARNRS
585500070208     C                   KFLD                    BARNSC
585600140616     C     KBAR_qd4      KLIST
585700140616     C                   KFLD                    kdel_npg
585800140616     C                   KFLD                    BARLNP
585900140616     C                   KFLD                    BARLNA
586000140616     C                   KFLD                    BARNRS
586100140616     C                   KFLD                    BARNSC
586200140616     C                   KFLD                    BARfgs
586300140616     C                   KFLD                    qd_dfs
586400140616     C                   KFLD                    qd_hms
586500070208     C     KBAR2         KLIST
586600070208     C                   KFLD                    BARNPG
586700070208     C                   KFLD                    BARLNP
586800070208     C                   KFLD                    BARLNA
586900070208     C                   KFLD                    BARNRS
587000070208     C                   KFLD                    BARNSC
587100070208     C                   KFLD                    BARfgs
587200140613     C     KBAR2_d       KLIST
587300140613     C                   KFLD                    kdel_npg
587400140613     C                   KFLD                    BARLNP
587500140613     C                   KFLD                    BARLNA
587600140613     C                   KFLD                    BARNRS
587700140613     C                   KFLD                    BARNSC
587800140613     C                   KFLD                    BARfgs
587900140616     C                   KFLD                    wdfs
588000070208     C     KBAR3         KLIST
588100070208     C                   KFLD                    WNPG
588200070208     C                   KFLD                    BARLNP
588300070208     C                   KFLD                    BARLNA
588400070208     C                   KFLD                    BARNRS
588500070208     C                   KFLD                    BARNSC
588600070208     C     KBRV7         KLIST
588700070208     C                   KFLD                    BARLNP
588800070208     C                   KFLD                    BARLNA
588900070208     C                   KFLD                    BARNRS
589000070208     C                   KFLD                    BARNSC
589100070208     C     KARB          KLIST
589200070208     C                   KFLD                    ARTAAS
589300070208     C                   KFLD                    ARTLNP
589400070208     C                   KFLD                    ARTNRS
589500070208     C                   KFLD                    ARTNSP
589600070208     C     KGCA          KLIST
589700070208     C                   KFLD                    ARTAAS
589800070208     C                   KFLD                    ARTLNP
589900070208     C                   KFLD                    ARTNRS
590000070208     C                   KFLD                    ARTNSP
590100070208     C                   KFLD                    KFRG
590200070208     C     KSAV          KLIST
590300070208     C                   KFLD                    SAVTBO
590400070208     C                   KFLD                    SAVAAS
590500070208     C                   KFLD                    SAVLNP
590600070208     C                   KFLD                    SAVNRS
590700070208     C                   KFLD                    SAVNSP
590800070208     C                   KFLD                    KAGB
590900070208     C     KEAGB         KLIST
591000070208     C                   KFLD                    WTBO
591100070208     C                   KFLD                    WDAAS
591200070208     C                   KFLD                    WDLNP
591300070208     C                   KFLD                    WDNRS
591400070208     C                   KFLD                    WDNSP
591500070208     C                   KFLD                    KAGB
591600070208     C     KBLT          KLIST
591700070208     C                   KFLD                    BLTAAS
591800070208     C                   KFLD                    BLTLNP
591900070208     C                   KFLD                    BLTNRS
592000070208     C                   KFLD                    BLTNSP
592100170905     C     KBLp29        KLIST
592200170905     C                   KFLD                    B29AAS
592300170905     C                   KFLD                    B29LNP
592400170905     C                   KFLD                    B29NRS
592500170905     C                   KFLD                    B29NSP
592600170905     C     Kblt29        KLIST
592700170905     C                   KFLD                    BARLNP
592800170905     C                   KFLD                    BARNRS
592900170905     C                   KFLD                    BARNSC
593000170905     C                   KFLD                    BARLNA
593100170905     C     Kblt29b       KLIST
593200170905     C                   KFLD                    BARLNP
593300170905     C                   KFLD                    BARNRS
593400070208     C     KBTT          KLIST
593500070208     C                   KFLD                    BTTflp
593600070208     C                   KFLD                    BTTAAS
593700070208     C                   KFLD                    BTTLNP
593800070208     C                   KFLD                    BTTNRS
593900070208     C                   KFLD                    BTTNSP
594000070208     C     KANM          KLIST
594100070208     C                   KFLD                    BARLNP
594200070208     C                   KFLD                    BARLNA
594300070208     C                   KFLD                    BARNRS
594400070208     C                   KFLD                    BARNSC
594500070208     C                   KFLD                    COMCAA            3 0
594600070208     C     KEVS          KLIST
594700070208     C                   KFLD                    BARLNP
594800070208     C                   KFLD                    BARLNA
594900070208     C                   KFLD                    BARNRS
595000070208     C                   KFLD                    BARNSC
595100070208     C                   KFLD                    KCEV
595200070208     C     KFVV1         KLIST
595300070208     C                   KFLD                    WNPG
595400070208     C                   KFLD                    WNFV
595500070208     C                   KFLD                    WFGS
595600070208     C     KFVV          KLIST
595700070208     C                   KFLD                    WNPG
595800070208     C                   KFLD                    wdata
595900070208     C                   KFLD                    KTFA2
596000070208     C     KFVA1         KLIST
596100070208     C                   KFLD                    WFGS
596200070208     C                   KFLD                    WNFV
596300070208     C     KFGV1         KLIST
596400070208     C                   KFLD                    WNFV
596500070208     C                   KFLD                    tfpfgs
596600070208     c     KFGV2         KLIST
596700070208     C                   KFLD                    lnptfp
596800070208     C                   KFLD                    bardfv
596900070208     C     KFVA4         KLIST
597000070208     C                   KFLD                    KNPG4
597100070208     C                   KFLD                    KNFV4
597200070208     C                   KFLD                    KFGS4
597300070208     C     KFWA          KLIST
597400070208     C                   KFLD                    FVALNP
597500070208     C                   KFLD                    FVANFV
597600070208     C                   KFLD                    FVALAI
597700070208     C     KTAB          KLIST
597800070208     C                   KFLD                    CODUT
597900070208     C                   KFLD                    COD
598000070208     C                   KFLD                    KEY
598100070208     C     KTAB2         KLIST
598200070208     C                   KFLD                    CODUT
598300070208     C                   KFLD                    COD
598400130228     C     KTBE2         KLIST
598500130228     C                   KFLD                    KCOD
598600130228     C                   KFLD                    KKE1
598700130228     C                   KFLD                    KKE2
598800070208     C*
598900070208     C     KLBL          KLIST
599000070208     C                   KFLD                    LBLAAN
599100070208     C                   KFLD                    LBLLPN
599200070208     C                   KFLD                    LBLNRN
599300070208     C                   KFLD                    LBLNSN
599400070208     C     KDCT          KLIST
599500070208     C                   KFLD                    DCDAAC
599600070208     C                   KFLD                    DCDFIL
599700070208     C                   KFLD                    DCDNCA
599800070208     C     KBRVE2        KLIST
599900070208     C                   KFLD                    BRVNPG
600000070208     C                   KFLD                    BRVLNP
600100070208     C                   KFLD                    BRVLNA
600200070208     C                   KFLD                    BRVNRS
600300070208     C                   KFLD                    BRVNSC
600400070710     C     KBRVE1        KLIST
600500070710     C                   KFLD                    BRVNPG
600600070710     C                   KFLD                    BRVNfv
600700070710     C                   KFLD                    BRVfgs
600800070710     C                   KFLD                    BRVLNP
600900070710     C                   KFLD                    BRVLNA
601000070710     C                   KFLD                    BRVNRS
601100070710     C                   KFLD                    BRVNSC
601200070208     C     Kar5          KLIST
601300070208     C                   KFLD                    anmaas
601400070208     C                   KFLD                    anmLNp
601500070208     C                   KFLD                    anmNRS
601600070208     C                   KFLD                    anmnsp
601700070208     C                   KFLD                    Ktrd
601800091211     C     Kar52         KLIST
601900091211     C                   KFLD                    artaas
602000091211     C                   KFLD                    artLNp
602100091211     C                   KFLD                    artNRS
602200091211     C                   KFLD                    artnsp
602300091211     C                   KFLD                    Ktrd
602400070208     C*
602500070208     C     *LIKE         DEFINE    FVAdfv        Wpul050
602600070208     C     *LIKE         DEFINE    FVALNP        WFVALNP
602700070208     C     *LIKE         DEFINE    FVANFV        WFVANFV
602800070208     C     *LIKE         DEFINE    FVADFV        WFVADFV
602900070208     C     *LIKE         DEFINE    bardfv        Wdata
603000070208     C     *LIKE         DEFINE    wfgs          WWWfgs
603100070208     C     *LIKE         DEFINE    wfgs          tfpfgs
603200070208     C     *LIKE         DEFINE    wfgs          savfgs
603300070208     c                   clear                   savfgs
603400070208     C     *LIKE         DEFINE    FVVFLE        WFLE
603500070208     C     *LIKE         DEFINE    ARTFLP        SAVFLP
603600070208     C     *LIKE         DEFINE    BARDFV        DSPB
603700070208     C     *LIKE         DEFINE    BARDFV        COMDFV
603800070208     C     *LIKE         DEFINE    BARDFV        SVVDFV
603900070208     C     *LIKE         DEFINE    BLPTFP        LNPTFP
6040000702080    C     *LIKE         DEFINE    BARDFV        WDTRIF
604100070208     C     *LIKE         DEFINE    BARDFV        PRIDAT
604200070208     C     *LIKE         DEFINE    BARDFV        SA2DFV
604300070208     C     *LIKE         DEFINE    ANMCCH        WCCH
604400070208     C     *LIKE         DEFINE    GCPFRG        KFRG
604500070208     C                   MOVEL     '0'           KFRG
604600070208     C     *LIKE         DEFINE    AGBAGB        KAGB
604700070208     C     *LIKE         DEFINE    AGBFBS        WFBS
604800070208     C     *LIKE         DEFINE    AGBFVC        WFVC
604900070208     C     *LIKE         DEFINE    AGBFPC        WFPC
605000070208     C     *LIKE         DEFINE    ARBNDC        WABNDC
605100070208     C     *LIKE         DEFINE    ARTDCM        WATDCM
605200070208     C     *LIKE         DEFINE    ARbcca        WABcca
605300070208     C     *LIKE         DEFINE    ARbcca        savcca
605400070208     C     *LIKE         DEFINE    BARLNA        SVVLNA
605500070208     C     *LIKE         DEFINE    BARLNP        SVVLNP
605600070208     C     *LIKE         DEFINE    ARTDCM        WDCM
605700070208     C     *LIKE         DEFINE    ANMCAA        CAACRE
605800070208     C     *LIKE         DEFINE    ANMCAA        CAACIU
605900070208     C     *LIKE         DEFINE    FGVTTR        WTTR
606000140613     C     *LIKE         DEFINE    FGVFCF        WFCF
606100070208     C     *LIKE         DEFINE    EVSCEV        KCEV
606200070208     C     *LIKE         DEFINE    D33MAC        SAVMAC
606300070208     C     *LIKE         DEFINE    BRVFLE        BARFLE
606400070208     C     *LIKE         DEFINE    BRVFGS        SAVPES
606500070208     C     *LIKE         DEFINE    BRVFGS        KFGS4
606600070208     C     *LIKE         DEFINE    TBLCOD        COD
606700070208     C     *LIKE         DEFINE    TBLKEY        KEY
606800070208     C     *LIKE         DEFINE    BARLNA        LNATFA
606900070208     C     *LIKE         DEFINE    BARLNA        Usalnatfa
607000100713     C     *LIKE         DEFINE    BARLNA        WgenLNAtfa
607100070208     C     *LIKE         DEFINE    blptfa        bolTFA
607200070208     C     *LIKE         DEFINE    BARLNA        PESTFA
607300070208     C     *LIKE         DEFINE    BARLNA        PESTFP
607400070208     C     *LIKE         DEFINE    BARLNP        LNPB
607500070208     C     *LIKE         DEFINE    FVVFGS        WFGS
607600070208     C     *LIKE         DEFINE    FVVSPG        WSPG
607700070208     C     *LIKE         DEFINE    FVVNPG        WNPG
607800070208     C     *LIKE         DEFINE    FVVNPG        KNPG4
607900070208     C     *LIKE         DEFINE    FVVNFV        WNFV
608000070208     C     *LIKE         DEFINE    FVVNFV        KNFV4
608100070208     C     *LIKE         DEFINE    FVVFLE        WLIN
608200070208     C     *LIKE         DEFINE    FVVFLE        WLINPR
608300070208     C     *LIKE         DEFINE    FVVSPG        BARSPG
608400140616     C     *LIKE         DEFINE    FVVSPG        brvSPG
608500070208     C     *LIKE         DEFINE    FVVDFV        WDFV
608600070208     C     *LIKE         DEFINE    FVVDFV        SAVDFV
608700070208     C     *LIKE         DEFINE    FVVDFV        BARDFV
608800070208     C     *LIKE         DEFINE    FVVDFV        BRVDFV
608900070208     C     *LIKE         DEFINE    BARKEY        SAVKEY
609000070208     C     *LIKE         DEFINE    BRVKEY        SA1KEY
609100070208     C     *LIKE         DEFINE    ARBLNP        COMLNP
609200070208     C     *LIKE         DEFINE    ARBAAS        COMAAS
609300070208     C     *LIKE         DEFINE    ARBNSP        COMNSP
609400070208     C     *LIKE         DEFINE    ARBTFP        WPTFP
609500070208     C     *LIKE         DEFINE    ANMFL1        WFL1
609600070208     C     *LIKE         DEFINE    D55TFA        KTFA2
609700070208     C     *LIKE         DEFINE    D55TFA        SAVTFA
609800070208     C     *LIKE         DEFINE    ARTFVC        WWFVC
609900070208     C     *LIKE         DEFINE    ARTDAM        SAVDAM
610000070208     C     *LIKE         DEFINE    ARTDAM        WARDAM
610100070208     C     *LIKE         DEFINE    ARTDAM        WSVDAM
610200070208     C     *LIKE         DEFINE    FVADFV        SAVDAO
610300070208     C     *LIKE         DEFINE    FVALNP        WVALNP
610400070208     C     *LIKE         DEFINE    DCTTAD        SAVTAD
610500070208     C                   CLEAR                   SAVTAD
610600070208     C     *LIKE         DEFINE    §OGNTW        NTWBAR
610700070208     C     *LIKE         DEFINE    GI8INV        DATMIN
610800070208     C     *LIKE         DEFINE    BRVATR        SAWATR
610900080317     C     *LIKE         DEFINE    BRVTSU        SAWtsu
611000070208     C*
611100070208     C                   MOVEL     'DT'          KAGB
611200070208     c                   ENDSR
611300140613     C**************************************************************************
611400140613     C* Cerca spunta 4
611500140613     C**************************************************************************
611600140613     c     Cer_cons      BEGSR
611700140616     c* imposto data immiss.spunta della spunta che sto caricando
611800140616     c                   exsr      ImpoDFS
611900140616     c*
612000140616     C     KBAR2_d       setll     FNBRV05L
612100140616     C     KBAR2_d       reade(n)  FNBRV05L
612200140616    1c                   dow       not %eof(fnbrv05l)
612300140613     c* controllo data e ora immissione
612400140613     c                   clear                   brv_imm
612500140613     c                   clear                   brv_sca
612600140613     c                   z-add     brvhms        brv_imm
612700140613     c                   movel     brvdfs        brv_imm
612800140613     c                   z-add     brvhcs        brv_sca
612900140613     c                   movel     brvdcs        brv_sca
613000140613     c                   exsr      sceglidata
613100140613     c                   z-add     a_imm         b_imm
613200140613     c
613300140613     c* a parità di data devo tenere la più recente
613400140613     c                   clear                   brv_imm
613500140613     c                   clear                   brv_sca
613600140613     c                   z-add     barhms        brv_imm
613700140613     c                   movel     wDFS          brv_imm
613800140613     C                   Z-ADD     WDATE1        barDCS
613900140613     C                   time                    barHCS
614000140613     c                   z-add     barhcs        brv_sca
614100140613     c                   movel     bardcs        brv_sca
614200140613     c                   exsr      sceglidata
614300140613     c*
614400140613     c                   select
614500140616     c* quella trovata + recente --> non carico la spunta
614600140613    2c                   when      b_imm > A_IMM  and brvdfs=wdfs
614700140613     c
614800140613     c* Se anche la data foglio è = non carico la spunta ricevuta
614900140613    3C     BRvKEY        IFNE      SA1KEY
615000140613     C                   MOVEL     BRvKEY        SA1KEY
615100140613     C                   Z-ADD     BRvNPG        WNPG
615200140613     C                   Z-ADD     BRvNFV        WNFV
615300140613     C                   Z-ADD     BRvFGS        WFGS
615400140613     C                   movel     BRvnps        Wnps
615500140613     C                   EXSR      RILFOG
615600140613     c                   Z-ADD     WDFV          BRVDFV
615700140616     c                   movel     WFCF          BRVFCF
615800140616     c                   movel     Wspg          BRVspg
615900140613    3c                   endif
616000140613     c
616100140616     c                   if        barnpg=3 and
616200140616    3c                             brvdfv=bardfv
616300140616     c                   eval      amb_QD ='N'
616400140616     c                   endif
616500140616     c                   if        barnpg=4 and brvspg='A' and
616600140616    3c                             brvdfv=bardfv
616700140616     c                   eval      amb_QD ='N'
616800140616     c                   endif
616900140613     c
617000140616     c                   if        amb_qd='N'
617100140613     c* memorizzo la spunta nuova negli errori
617200140613     C                   MOVEL     'S'           WBRVE             1
617300140613     C                   EXSR      RIEMPI
617400140613     C                   CLEAR                   WBRVE
617500140613     C                   WRITE     FNBRVE
617600140616     c                   leave
617700140613    3c                   endif
617800140616     c
617900140616     c* quella di carico + recente --> cancello quella già presente
618000140613    2c                   when      A_imm > B_IMM  and brvdfs=wdfs
618100140613     c* Se anche la data foglio è = non carico la spunta ricevuta
618200140613    3C     BRvKEY        IFNE      SA1KEY
618300140613     C                   MOVEL     BRvKEY        SA1KEY
618400140613     C                   Z-ADD     BRvNPG        WNPG
618500140613     C                   Z-ADD     BRvNFV        WNFV
618600140613     C                   Z-ADD     BRvFGS        WFGS
618700140613     C                   movel     BRvnps        Wnps
618800140613     C                   EXSR      RILFOG
618900140613     c                   Z-ADD     WDFV          BRVDFV
619000140616     c                   movel     WFCF          BRVFCF
619100140616     c                   movel     Wspg          BRVspg
619200140613    3c                   endif
619300140613     c
619400140616     c                   if        barnpg=3 and
619500140616    3c                             brvdfv=bardfv and brvfcf=' '
619600140616     c                   eval      amb_QD ='S'
619700140613     c* mi slavo data e ora imm spunta 4 per la delete
619800140616     c                   eval      qd_npg=4
619900140616     c                   eval      qd_dfs=brvdfs
620000140616     c                   eval      qd_hms=brvhms
620100140616     c                   leave
620200140613     c                   endif
620300140616     c                   if        barnpg=4 and  brvspg='A' and
620400140616    3c                             brvdfv=bardfv and brvfcf=' '
620500140616     c                   eval      amb_QD ='S'
620600140616     c* mi slavo data e ora imm spunta 4 per la delete
620700140616     c                   eval      qd_npg=3
620800140616     c                   eval      qd_dfs=brvdfs
620900140616     c                   eval      qd_hms=brvhms
621000140616     c                   eval      qd_nfv=brvnfv
621100140616     c                   leave
621200140616     c                   endif
621300140613     c
621400140613    2c                   endsl
621500140616     c
621600140616     C     KBAR2_d       reade(n)  FNBRV05L
621700140616    1c                   enddo
621800140613     c
621900140613     c                   ENDSR
622000140613     C**************************************************************************
622100140613     C* Scelta data tra immisisone e caicamento
622200140613     C**************************************************************************
622300140613     c     Sceglidata    BEGSR
622400140613     c                   if         brv_imm>brv_sca and
622500140616     c                              %subst(%editc(brv_sca:'X'):1:8)>*zeros
622600140613     c                   eval       A_imm=brv_sca
622700140613     c                   else
622800140613     c                   eval       A_imm=brv_imm
622900140613     c                   endif
623000140613     c                   ENDSR
623100140613     C**************************************************************************
623200140613     C*    impostazione data immissione spunta
623300140613     C**************************************************************************
623400140613     c     ImpoDFS       BEGSR
623500140613     c* Se data immissione > di oggi, imposto la minore tra oggi e data fogl
623600140613     c                   if        BARDFS<=wdate1
623700140613     C                   Z-ADD     BARDFS        wDFS
623800140613     c                   else
623900140613     c                   if        wdate1<=bardfv
624000140613     c                   z-add     wdate1        wDFS
624100140613     c                   else
624200140613     c                   z-add     bardfv        wDFS
624300140613     c                   endif
624400140613     c                   endif
624500140613     c                   ENDSR
624600140616     C**************************************************************************
624700140616     C*    cancellazione spunta meno recente IMA o cons
624800140616     C**************************************************************************
624900140616     c     Canc_QD       BEGsr
625000140616     c                   eval      kdel_npg=qd_npg
625100140616    1c                   if        kdel_npg=4
625200140616     c     kbar_qd4      chain     fnbrv05l                             38
625300140616     c  n38              if        %found(fnbrv05l)
625400140616     C     KBRVE1        SETLL     FNBRVE1L                               33
625500140616    2C     *IN33         IFEQ      *OFF
625600140616     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
625700140616     C                   WRITE     FNBRVE
625800140616    2C                   ENDIF
625900140616     c
626000140616     c                   delete    fnbrv005
626100141029     c                   endif
626200140616     c
626300140616   x1c                   else
626400140616     c     kbar_qd4      setll     fnbrv05l
626500140616     c     kbar_qd4      reade     fnbrv05l
626600140616    2c                   dow       not %eof(fnbrv05l) and brvnfv<>qd_nfv
626700140616     c     kbar_qd4      reade     fnbrv05l
626800140616    2c                   enddo
626900140616    2c                   if        not %eof(fnbrv05l)
627000140616     C     KBRVE1        SETLL     FNBRVE1L                               33
627100140616    3C     *IN33         IFEQ      *OFF
627200140616     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
627300140616     C                   WRITE     FNBRVE
627400140616    3C                   ENDIF
627500140616     c                   delete    fnbrv005
627600140616    2c                   endif
627700140616     c
627800140616    1c                   endif
627900140616     c                   ENDSR
628000150622     C**************************************************************************
628100150622     C*    Aggiornamenti da tabella BSP
628200150622     C**************************************************************************
628300150622     c     DA_BSP        BEGsr
628400150622     c* Scrivo FNAGB per aggiornare la bolla
628500150622     C                   CLEAR                   FNAGB000
628600150622     C                   MOVEL     'P'           AGBTBO
628700150622     C                   MOVEL     'CB'          AGBAGB
628800150622     C                   MOVEL     blpAAS        AGBAAS
628900150622     C                   MOVEL     blpLNP        AGBLNP
629000150622     C                   MOVEL     blpNRS        AGBNRS
629100150622     C                   MOVEL     blpNSP        AGBNSP
629200150622     C                   WRITE     FNAGB000                             99
629300150622     c                   ENDSR
629400070208     C**************************************************************************
629500070208     C* Elaborazioni per chiusura pgm
629600070208     C**************************************************************************
629700070208     c     ELACHIUDI     BEGSR
629800070208     C* ELABORO L'ULTIMO DATO MEMORIZZATO
629900070208     C                   EXSR      WRTAGB
630000070208     c
630100070208     C                   CLEAR                   DSLV55
630200070208     C                   MOVEL     'C'           D55TLA
630300070208     C                   EXSR      FNLV55
630400070208     C* SCRITTURA ULTIMO RECORD IN FNSTA
630500070208     C   02              EXSR      WRTSTA
630600070208     C**
630700070208     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
630800070208     c                   clear                   fidn12ds
630900070208     c                   eval      i12tla = 'C'
631000070208     c                   call      'FIDN12R'
631100070208     c                   parm                    fidn12ds
631200161007     c
631300161007     c                   clear                   fnlgi1ds
631400161007     c                   eval      ilgi1tla = 'C'
631500161007     c                   movel     fnlgi1ds      kpjbu
631600161007     c                   call      'FNLGI1R'
631700161007     c                   parm                    kpjba
631800070208     c                   ENDSR
631900110103
632000110103      *---------------------------------------------------------------*
632100110103      *?Reperimento dati del job (Utente/Operativi)                  ?*
632200110103      *---------------------------------------------------------------*
632300110103     c     sr_DatiJob    BEGSR
632400110103      *
632500110103     c     *dtaara       define    §azute        AZUTEds
632600110103     c     *dtaara       define    §datiute      dDatiUte
632700110103      *
632800110103     c                   in(E)     *dtaara
632900110103     c                   IF        %Error or RSUT = *blanks
633000110103     c                   clear                   TIBS34ds
633100110103     c                   call      'TIBS34R'
633200110103     c                   parm                    TIBS34ds
633300110103     c                   in        *dtaara
633400110103     c                   ENDIF
633500110103      *
633600110103     c                   ENDSR
633700040601**         CM5
633800040601DLYJOB DLY(5)
633900980928**         MSG
634000070906CARICA SPUNTE:Fogli Inesitenti (FGS NPG NFV-NPS) FNLS45R
634100070906XXX X XXXXX-xx;  XXX X XXXXX-xx;  XXX X XXXXX-xx;  XXX X XXXXX-xx;
634200070906           -  ;             -  ;             -  ;             -  .
