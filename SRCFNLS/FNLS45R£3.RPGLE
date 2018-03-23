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
005600911014     D*
005700960523     D L6              S              3  0 DIM(30)
005800961212     D*
005900130614     D S5F             S              8    DIM(9000)
006000130614     D F5F             S             12    DIM(9000)
006100020218     D WPO             S              3    DIM(4)
006200020204     D CAN             S              1    DIM(200)
006300961212     D*
006400921229     D NPS             S              2  0 DIM(101)
006500921229     D RPS             S              1    DIM(101)
006600000210     D TPS             S              1    DIM(101)
006700000316     D OPS             S              3  0 DIM(101)
006800000316     D INV             S              1    DIM(101)
006900040503     D FSL             S              1    DIM(101)
007000961212     D*
007100941214     D CAT             S              1  0 DIM(20)
007200941214     D NST             S              1    DIM(20)
007300950927     D*
007400961122     D TTR             S              1    DIM(10)
007500961122     D*
007600950927     D PG2             S              5  0 DIM(20)
007700950927     D*
007800000505     D FFS             S              3    DIM(301)
007900970114     D*
008000040601     D CM5             S              1    DIM(15) CTDATA PERRCD(15)
008100980928     D*
008200111018     D** ERC             S              1  0 DIM(10)
008300111018     D** ERF             S              5    DIM(10)
008400111018     D** ERE             S              3  0 DIM(10)
008500111018     D** ERp             S              2    DIM(10)
008600981105     D K70             S              1    DIM(70)
008700981105     D MSG             S             70    DIM(3) CTDATA PERRCD(1)
008800001109     D ACG             S              3  0 DIM(50)
008900130228     d* tfp abilitati a far partire colli di altro tfp
009000130228     D xcotfp          S              3    DIM(85)
009100000516     D*-----------------------------
009200930701     D* DS PER CONFRONTO SPUNTE: BAR
009300930701     D*-----------------------------
009400930701     D                 DS
009500960524     D  BARCAN                 1      1
009600960524     D  BARLNP                 2      4  0
009700960524     D  BARLNA                 5      7  0
009800960524     D  BARNRS                 8      9  0
009900960524     D  BARNSC                10     16  0
010000960524     D  BARNPS                17     18  0
010100960524     D  BARVUC                19     25  6
010200960524     D  BARZNC                26     27  0
010300021002     D  BARFGS                28     30  0
010400960524     D  BARNPG                31     31  0
010500991223     D  BARNFV                32     37  0
010600991223     D  BARPUC                38     44  3
010700000223     D  BARDFS                45     52  0
010800070209     D  BARHMS                53     58  0
010900070209     D  BARmis                59     61  0
011000070822     D  BARimmis              45     61  0
011100070209     D  BARPRU                62     71
011200070209     D  BARTAP                72     72
011300960524     D* DS PER CONFRONTO SEGNACOLLO CHE STO CARICANTO
011400070209     D  BAR1                   1     72
011500961212     D* KEY PER CHAIN FOGLI VIAGGIO A ROTTURA
011600991223     D  BARKEY                28     37  0
011700930701     D*-----------------------------
011800930701     D* DS PER CONFRONTO SPUNTE: BRV
011900930701     D*-----------------------------
012000930701     D                 DS
012100960524     D  BRVCAN                 1      1
012200960524     D  BRVLNP                 2      4  0
012300960524     D  BRVLNA                 5      7  0
012400960524     D  BRVNRS                 8      9  0
012500960524     D  BRVNSC                10     16  0
012600960524     D  BRVNPS                17     18  0
012700960524     D  BRVVUC                19     25  6
012800960524     D  BRVZNC                26     27  0
012900021002     D  BRVFGS                28     30  0
013000960524     D  BRVNPG                31     31  0
013100991223     D  BRVNFV                32     37  0
013200991223     D  BRVPUC                38     44  3
013300000223     D  BRVDFS                45     52  0
013400070131     D  BRVHMS                53     58  0
013500070209     D  Brvmis                59     61  0
013600070822     D  Brvimmis              45     61  0
013700070209     D  BrvPRU                62     71
013800070209     D  BrvTAP                72     72
013900021014     D* DS PER CONFRONTO SEGNACOLLO CHE STO CARICANTO
014000070209     D  BRV                    1     72
014100960524     D* KEY PER CHAIN FOGLI VIAGGIO A ROTTURA
014200991223     D  BRVKEY                28     37  0
014300960527     D                 DS
014400021014     D  BR7FGS                 1      3  0
014500960527     D  BR7NPG                 4      4  0
014600991223     D  BR7NFV                 5     10  0
014700960527     D* KEY PER CHAIN FOGLI VIAGGIO A ROTTURA
014800991223     D  BR7KEY                 1     10  0
014900020205     D                 DS
015000040303     D  BR9FGS                 1      3  0
015100040303     D  BR9NPG                 4      4  0
015200040303     D  BR9NFV                 5     10  0
015300040303     D  BR9KEY                 1     10  0
015400941213     D WLBDAT          DS
015500941213     D  G02DAT                 1      8  0
015600941213     D  G02INV                 9     16  0
015700941213     D  G02ERR                17     17
015800941213     D  G02TGI                18     22  0
015900000223     D WGIDAT          DS
016000000223     D  GI8DAT                 1      8  0
016100000223     D  GI8INV                 9     16  0
016200000223     D  GI8TGI                17     21  0
016300950927     D*LINEE   DI ARRIVO
016400950927     D                 DS
016500950927     D  FVALNA                 1      3  0
016600950927     D  FVAFFV                 4    243
016700950927     D  FVAFF2               244    450
016800000505     D  FWAFF3               451    690
016900000505     D  FWAFF4               691    900
017000000505     D  VUOTO                901    903
017100000505     D  FFV                    1    903
017200000505     D                                     DIM(301)
017300950927     D*FILIALE DI SCARICO
017400950927     D                 DS
017500950927     D  LNA2                   1      3  0
017600950927     D  FVAFLP                 4    243
017700950927     D  FVAFL2               244    450
017800000505     D  FWAFL3               451    690
017900000505     D  FWAFL4               691    900
018000000505     D  VUOTO2               901    903
018100000505     D  FLP                    1    903
018200000505     D                                     DIM(301)
018300980115     D* LINEE DI ARRIVO  PER FNFGV00F
018400980115     D                 DS
018500980115     D  FGVLNA                 1      3  0
018600980115     D  FGVFFV                 4    243
018700980115     D  FGVFF2               244    450
018800000505     D  FGWFF3               451    690
018900000505     D  FGWFF4               691    900
019000000505     D  VUOTO3               901    903
019100000505     D  FFG                    1    903
019200000505     D                                     DIM(301)
019300980115      *
019400161007     D**               DS
019500161007     D**GCPAGC                 1      4  0
019600161007     D**GCPMGC                 5      8  0
019700161007     D**GCPDGC                 1      8  0
019800970217     D                 DS
019900980128     D  ARTAAS                 1      4  0
020000980128     D  ARTLNP                 5      7  0
020100980128     D  ARTNRS                 8      9  0
020200980128     D  ARTNSP                10     16  0
020300980128     D  ARTTBO                17     17
020400980128     D  ARTSPE                 1     17
020500970217     D                 DS
020600970217     D  BLTAAS                 1      4  0
020700970217     D  BLTLNP                 5      7  0
020800970217     D  BLTNRS                 8      9  0
020900970217     D  BLTNSP                10     16  0
021000970217     D  BLTTBO                17     17
021100970217     D  BLTSPE                 1     17
021200970218     D                 DS
021300970218     D  BTTAAS                 1      4  0
021400970218     D  BTTLNP                 5      7  0
021500970218     D  BTTNRS                 8      9  0
021600970218     D  BTTNSP                10     16  0
021700980120     D  BTTTBO                17     17
021800970218     D  BTTSPE                 1     17
021900970217     D                 DS
022000970217     D  SAVAAS                 1      4  0
022100970217     D  SAVLNP                 5      7  0
022200970217     D  SAVNRS                 8      9  0
022300970217     D  SAVNSP                10     16  0
022400970508     D  SAVBOL                 1     16  0 INZ
022500970508     D  SAVTBO                17     17    INZ
022600970217     D  SAVSPE                 1     17
022700980120     D                 DS
022800980120     D  WDAAS                  1      4  0
022900980120     D  WDLNP                  5      7  0
023000980120     D  WDNRS                  8      9  0
023100980120     D  WDNSP                 10     16  0
023200980120     D  WDSPE                  1     16
023300021014     D                 DS
023400021014     D  WCED                   1      6
023500021014     D  WTER                   7      9
023600021014     D  WVUOTO                10     10
023700021014     D  WPROF                  1     10
023800020201     D                 DS
023900020201     D  BLPAAS                 1      4  0
024000020201     D  BLPMGS                 5      8  0
024100020201     D  BLPDSP                 1      8  0
024200020201     D                 DS
024300020201     D  ARBAAS                 1      4  0
024400020201     D  ARBMGS                 5      8  0
024500020201     D  ARBDSP                 1      8  0
024600020201     D                 DS
024700020201     D  BTPAAS                 1      4  0
024800020201     D  BTPMGS                 5      8  0
024900020201     D  BTPDSP                 1      8  0
025000930615     D KPJBA         E DS
025100110103      *
025200110103      * - Parametri per richiamo al pgm di controllo profilo utenti
025300110103     d TIBS34ds      e ds
025400110103      * - Ds di riferimento al file esterno AzUte00F
025500110103     d AZUTEds       e ds                  ExtName(AzUte00F)
025600110103      * - Ds per dati organigramma
025700110103     d dDatiUte      e ds
025800110103      *
025900961122     D DSTV          E DS
026000990121     D DCCH            DS
026100990121     D  §CHCRA                51     51
026200990121     D DTAD            DS
026300990121     D  §TARGR                26     26
026400990126     D  §TARIT                82     82
026500961122     D DS7C          E DS
026600921020     D DS7N          E DS
026700040430     D DS7O          E DS
026800921229     D DS7J          E DS
026900970522     D DS7A2         E DS
027000030723     D DVPO          E DS
027100000928     D DS5A          E DS
027200000928     D DS5APT        E DS
027300000614     D OG143         E DS
027400941229     D* DS PER TRUL06R - CARICAMENTO £X
027500941229     D DSUL06        E DS                  EXTNAME(TRUL06DS)
027600941229     D  LIN                    1     90  0
027700941229     D                                     DIM(30)
027800000705     D DSLV53        E DS                  EXTNAME(FNLV53DS)
027900020213     D DSLV55        E DS                  EXTNAME(FNLV55DS)
028000970814     D DSLR33        E DS                  EXTNAME(FNLR33DS)
028100021014     D DSBS55        E DS                  EXTNAME(TIBS55DS)
028200990120     D DSBS02        E DS                  EXTNAME(TIBS02DS)
028300161007     d FNLGI1DS      E DS
028400170125     d
028500170125     d TRULVPODS     e ds
028600170125     D  skFilOk               16    765    dim(250)
028700170125     d
028800070212     d
028900070212     d* DS passaggio dati
029000070207     D fnls45ds      E DS
029100160726     d
029200160726     D DAGBFLO       E DS
029300091209     D DAR5          E DS
029400091209     D DAR5GEN       E DS
029500990518     D FNANM2          DS
029600960416     D  ANMNR2               397    400B 0
029700070212     D FIBAR           DS
029800030226     D  BA1_file              83     92
029900030226     D  BA1_libr              93    102
030000150622     d                 ds
030100150622     d bsplin
030200150622     d sbsp                           3  0 dim(30) overlay(bsplin)
030300040811
030400040811      * ds per il controllo della presenza di CA per la spedizione richiesta
030500040811     d FIDN12DS      e ds
030600040811     d  skKey                 26    305    dim(20)
030700040811     d  skAnn                306    325    dim(20)
030800040811     d  skDca                326    485  0 dim(20)
030900040811     d  skFca                486    545  0 dim(20)
031000040811     d  skDch                546    705  0 dim(20)
031100040811     d  skCch                706    745    dim(20)
031200040811     d*
031300070212     D DSRECBRV      E DS                  extname(fnbrv00f)  prefix(D_)
031400161007     D tigcp00ds     E DS                  extname(tigcp00f)
031500080317     D WTSU            s              1  0
031600160421     D wznc            s              2
031700140613     D kdel_npg        s                   like(brvnpg)
031800130228     D KCOD            s                   like(tbecod)
031900130228     D Kke1            s                   like(tbeke1)
032000130228     D Kke2            s                   like(tbeke2)
032100130228     D SAWBRV          s                   like(brv)
032200080317     D WPES            s                   like(brvpes)
032300070208     D BPES            s                   like(brvpes)
032400070208     D BTFP            s                   like(brvFLE)
032500050322     D savesart        s                   like(wesart)
032600041105     D wprimotbo       s                   like(savtbo)
032700070221     D WSAVFLG         s                   like(dls45FLG)
032800140613     D wdfs            s                   like(brvdfs)
032900140616     D qd_nfv          s                   like(brvnfv)
033000140616     D qd_npg          s                   like(brvnpg)
033100140616     D qd_dfs          s                   like(brvdfs)
033200140616     D qd_hms          s                   like(brvhms)
033300140616     D bardcs          s                   like(brvdcs)
033400140616     D barhcs          s                   like(brvhcs)
033500140616     D barfcf          s                   like(fvvfcf)
033600140616     D brvfcf          s                   like(fvvfcf)
033700150622     d w_clibo         s                   Like(blpccm)
033800160726     d welasta_p       s              1
033900160726     d wBspTp          s              1
034000150622     d wBspspu         s              1
034100140618     d A_imm           s             14  0 inz
034200140618     d B_imm           s             14  0 inz
034300140618     d brv_imm         s             14  0 inz
034400140618     d brv_sca         s             14  0 inz
034500140618     d DatReale        s              8  0 inz
034600140618     d oraReale        s              4  0 inz
034700130228     d Indx            s              3  0
034800130228     d SAVfnrs         s              5  0 inz
034900130228     d Wflsnrs         s              5  0
035000041013     D WCONTRDFV       s              1    inz('N')
035100041105     D WAGBART         s              1    inz(' ')
035200070906     D WNPS            s              2    inz
035300080630     D WOK             s              1    inz
035400080630     D Secondi         s              9  0 inz
035500040621     D ktrd            s                   like(ar5trd)  inz('GEN')
035600080630     D Timeold         s               t   timfmt(*iso)
035700080630     D Timenew         s               t   timfmt(*iso)
035800980928     D* DEFINIZIONE COSTANTI
035900980928     D CUTI            C                   CONST('EDP*      ')
036000960423     C*---------------------------------------------------------------*
036100921014     C* RIEPILOGO INDICATORI
036200970115     C*---------------------------------------------------------------*
036300921014     C* 02    - RICHIAMATO
036400930914     C* 03    - POSSO RIABILITARE LA BOLLA
036500921021     C* 05    - MODIFICA DEL SOLO NUMERO DI FOGLIO
036600000505     C* 28/29 - DI COMODO
036700070212     C* 30    - LETTURA FIBAR00F
036800070131     C* 31    - CHAIN FNBRV
036900950927     C* 32    - LETTURA FILE FNBLT FNART FNARB FNBTT
037000950927     C* 33/34 - DI COMODO
037100921014     C* 37    - NON ESISTE BOLLA TRANSITO
037200070131     C* 38    - ERRORE SU WRITE FNBRV e su update
037300921105     C* 39    - RECORD ALLOCATO
037400921014     C* 40    - SE NON RECORD ALLOCATI O ALLOCATI PER 3 VOLTE FINE PGM
037500950926     C* 41    - COMODO
037600030528     C* 45    - Errore su write fnagb
037700040310     C* 46    - lookuip su schiera NTS
037800970204     C* 96    - ESISTE GIA' A MENO L'ANOMALIA CHE DEVO CREARE
037900970204     C* 97    - USATO PER LA CHIUSURA ANOMALIE CON 145 E 146
038000080325     C* 98    - FIBAR allocato --> esco dal pgm
038100080325     C* 99    - DI COMODO
038200921014     C*---------------------------------------------------------------*
038300911016     C     *ENTRY        PLIST
038400070212     C                   PARM                    fnls45ds
038500070208     C                   PARM                    dsrecbrv
038600941214     C*---------------------------------------------------------------*
038700070221     c* tipo di richiamo in base a dls45FLG
038800070208     C*---------------------------------------------------------------*
038900930521     C* FLG = '2' MODIFICATO SOLO NUMERO FOGLIO
039000160624     c
039100160624     c* Eliminato tipo richiamo "6" in quanto eliminate anomalie 60 e 61
039200070221     C* FLG = '6' CREO SOLO ANOMALIA 61 (EXSR CRTA60) DA SPU DISGUIDO
039300160624     c
039400991022     C* FLG = 'D' CREO ANOMALIA DISGUIDO PER COLLO DA ME CONFERMABILE
039500930521     C* FLG = ' ' INSERIMENTO NUOVA SPUNTA O VARIATA SOLO ANOMALIA
039600930521     C* FLG = 'C' CHIUDO PGM CON LR E BASTA
039700050418     C* FLG = 'T' aggiorno bolla transito x disguido
039800070221     c*
039900070221     c* Richiami attualmente usati: '2'  ' '  'T'  'C'
040000070208     C*---------------------------------------------------------------*
040100070208     c* Eaborazioni per chiusura pgm
040200070221    1C     dls45FLG      IFEQ      'C'
040300070208     c                   EXSR      ELACHIUDI
040400040811     c*
040500930521     C                   SETON                                        LR
040600930521     C*
040700930521   X1C                   ELSE
040800941229     C*
040900941229     C* IMPOSTAZIONI INIZIALI
041000941229    2C     UNO           IFEQ      ' '
041100941229     C                   EXSR      CARTAB
041200941229    2C                   END
041300070208     c
041400070208     c* Impostazione di BPES
041500070208     c                   if        dls45pes>*zeros
041600070208     c                   movel     dls45pes      BPES
041700070208     c                   endif
041800070208     c                   if        dls45tfp>*zeros
041900070208     c                   movel     dls45tfp      BTFP
042000070208     c                   endif
042100070208     c
042200070208     c                   EXSR      IMPO
042300930521     C*
042400930521     C                   SETOFF                                       05
042500941229     C                   Z-ADD     1             B                 3 0
042600930521     C****
042700930521     C**** P G M   R I C H I A M A T O
042800930521     C****
042900070208    2C                   if        dls45ric='S'
043000070208     C                   SETON                                        02
043100070208     c
043200070208     C* nel secndo parametro passato ci sono tutti i campi della spunta
043300070208     c*  con la quale effettuare gli aggiornamenti
043400070212     C                   Z-ADD     d_brvnpg      BARNPG
043500070212     C                   Z-ADD     D_brvnfv      BARNFV
043600070212     C                   Z-ADD     d_brvlnp      BARLNP
043700070212     C                   Z-ADD     d_brvlna      BARLNA
043800070212     C                   Z-ADD     d_brvnrs      BARNRS
043900070212     C                   Z-ADD     d_brvnsc      BARNSC
044000070212     C                   MOVEL     d_brvcan      BARCAN
044100070212     C                   Z-ADD     d_brvnps      BARNPS
044200070212     C                   Z-ADD     d_brvvuc      BARVUC
044300070212     C                   Z-ADD     d_brvpuc      BARPUC
044400070212     C                   Z-ADD     d_brvdfs      BARDFS
044500070212     C                   Z-ADD     d_brvhms      BARHMS
044600070212     C                   Z-ADD     d_brvmis      BARmis
044700070212     C                   movel     d_brvtap      BARtap
044800070212     C                   Z-ADD     d_brvznc      BARZNC
044900070212     c                   movel     d_brvpru      barpru
045000070212     c                   movel     d_brvfgs      barfgs
045100101130     c                   movel     d_brvdcs      brvdcs
045200101130     c                   movel     d_brvhcs      brvhcs
045300070208     c
045400960530     C                   CLEAR                   SAVMAC
045500960529     C**
045600070208     c** 05 on - modificato solo numero di foglio viaggio
045700070221    3C     dls45FLG      IFEQ      '2'
045800921229     C                   SETON                                        05
045900020213     C                   ENDIF
046000960524     C*
046100070209     C* CONTROLLO CHE ESISTA COD.ANOMALIA NELLA TABELLA ANOMALIE
046200020204     C     BARCAN        IFNE      ' '
046300020204     C     BARCAN        ANDNE     ' '
046400020204     C     BARCAN        LOOKUP    CAN                                    34
046500020204     C  N34              CLEAR                   BARCAN
046600020204     C                   ENDIF
046700961213     C*
046800961213     C* LETTURA FOGLIO E IMPOSTAZIONE CAMPI DI COMOD
046900961213     C                   EXSR      BARFGV
047000960529     C**
047100960529     C* SE NON HO TROVATO IL FOGLIO ESCO IMPOSTANDO L'ERRORE
047200960529    3C     BARDFV        IFEQ      0
047300070906     C                   SETON                                        39
047400960529   X3C                   ELSE
047500991230     C     BARLNA        CHAIN     AZORG01L                           31
047600040303    4c                   if        *in31 or
047700040303     c                             orgfag<>'F' and orgfag<>'A'
047800040303     C                   CLEAR                   NTWBAR
047900040303   x4c                   else
048000040303     C                   MOVE      ORGDE3        OG143
048100040303     C                   MOVE      §OGNTW        NTWBAR
048200000523     C*
048300000523     C* SE DEVO CREARE ANOMALIA DI DISGUIDO, CHIAMO SOLTANTO CRTA52
048400070221    5C     dls45FLG      IFEQ      'D'
048500991022     C                   MOVEL     'D'           WTIBOL
048600000523   X5C                   ELSE
048700920424     C*
048800920424     C* SE TROVO RECORD ALLOCATI RITENTO 3 VOLTE
048900000523    6C     B             DOWLE     3
049000950707     C                   MOVE      'S'           OKAGG             1
049100911016     C                   EXSR      UPDARB
049200000523    7C     OKAGG         IFEQ      'N'
049300950707     C                   Z-ADD     4             B
049400000523   X7C                   ELSE
049500021112     C                   Z-ADD     4             B
049600000523    7C                   ENDIF
049700150824     c
049800000523    6C                   ENDDO
049900150824
050000150824     c* Colli che partono dapiù filiali: facico fare la write al
050100150824     c*  chiamante
050200150824     C                   IF        wbsptp<>' ' and wbspspu='S'
050300150824     c                             and wagpar=' '
050400150824     c                   eval      %subst(dls45flo:1:1)='B'
050500150824     c                   endif
050600150824     c
050700040303    5C                   ENDif
050800040303    4C                   ENDIF
050900960410     C*
051000960410     C* PER BOLLA ARRIVO NON LOCALE, VEDO SE CREARE ANOMALIA 50
051100001114     C*  COLLO ARRIVATO NON PARTITO
051200960529     C*  NON ELABORO SE COLLO LOCALE O SOLO DELLA LINEA DI PARTENZA
051300001115     C* (N.B. INFATTI, SE BOLLA ARRIVO LOCALE, NON C'E' TRASMISSIONE DI
051400001115     C*  F.V. E QUINDI QUESTA ROUTINE NON SAREBBE MAI IN GRADO DI
051500001115     C*  CREARE L'ANOMALIA 50  DAL MOMENTO CHE VA A CERCARE IL FOGLIO
051600001115     C*  QUESTO VALE ANCHE PER I COLLI 180 PER 89)
051700020218    5C  N05WTIBOL        IFNE      'P'
051800960530     C     WTIBOL        ANDNE     'S'
051900020214     C     WTIBOL        ANDNE     'R'
052000070221     C     dls45FLG      ANDNE     '6'
052100960410     C                   EXSR      CRTA50
052200000523    5C                   ENDIF
052300000523     C*
052400960530    3C                   ENDIF
052500911016     C                   GOTO      FINE
052600960529     C*
052700960529    2C                   ENDIF
052800930521     C****
052900930521     C**** P G M    N O N    R I C H I A M A T O
053000930521     C****
053100930521     C* APRO FILE
053200040601     c                   clear                   wconta            2 0
053300040601     c                   movel     'S'           wapri             1
053400070212     C                   OPEN(e)   FIBAR00F
053500040601     c* Attendo 5 secondi e riprovo max 9 volte poi esco con errore
053600040601     c                   dow       %error  and wconta<10
053700040601     C                   Z-ADD     15            LUNG
053800040601     C                   MOVEA(p)  CM5           COMMAN           80
053900040601     C                   CALL      'QCMDEXC'                            99
054000040601     C                   PARM                    COMMAN
054100040601     C                   PARM                    LUNG             15 5
054200040601     c
054300040601     c                   add       1             wconta
054400070212     C                   OPEN(e)   FIBAR00F
054500040601     c                   enddo
054600040601     c
054700040601     c* Se ancora errore vado a fine file
054800040601     c                   if        %error
054900040601     c                   eval      wapri='N'
055000040601     c                   goto      fine
055100040601     c                   endif
055200040601     c
055300920424     C                   SETON                                        40
055400941214     C                   Z-ADD     *ZERO         SA1KEY
055500960530     C                   CLEAR                   SAVMAC
055600070208     C                   MOVE      DLS45FLG2     FLGPRE            1
055700911014     C*
055800080325     C                   READ      FIBAR00F                             9830
055900911016     C*
056000930521    2C     *IN40         DOWEQ     '1'
056100080325    2C     *IN98         andeq     *off
056200920424     C                   SETOFF                                       40
056300930521    3C     *IN30         DOWEQ     '0'
056400080325    3C     *IN98         andeq     '0'
056500930602     C*
056600040419     C                   SETOFF                                       3839
056700930521    4C     BARLNP        IFGT      *ZERO
056800040419     C     BARLNA        ANDGT     *ZERO
056900960731     C     BARNSC        ANDGT     *ZERO
057000040224     C     BARNpg        ANDGT     *ZERO
057100070201     c* Per ora non si accettano fogli maggiori di 100.000
057200040601     C     BARNfv        ANDlT     100000
057300070212     c
057400070212     c* Ad ogni record imposto BPES, perchè per le spunte cat 1 di un
057500070212     c*  secondo livello, BPES diventa il terminal per motivi di
057600070212     c*  creazione della anoamlia 200, per cui potrebbe rimanare
057700070212     c*  sporco se dopo ci fossero spunte di altre categorie
057800070212     c                   movel     dls45pes      BPES
057900021015     C                   Z-ADD     BPES          BARFGS
058000070209     c
058100160511      * CLEARO PESO E VOLUME UNITARI SE SUPERIORI AL MAX PREVISTO
058200160511      * limiti per transpallet  (pistola 46)
058300160511      * LIMITI PER RILEVATORE PESO VOLUME PALLET (Pistola 48)
058400160511      * limite per VDL  ( tutte le altre pistole)
058500160511     c                   select
058600160511     c                   when      barnps= 48
058700030723     C     BARPUC        IFGT      §vpoMPP
058800030723     C                   CLEAR                   BARPUC
058900030723     C                   ENDIF
059000030723     C     BARVUC        IFGT      §vpoMVP
059100030723     C                   CLEAR                   BARVUC
059200030723     C                   ENDIF
059300160511     c
059400160511     c                   when      barnps= 46
059500160511     C     BARPUC        IFGT      §vpoMPT
059600160511     C                   CLEAR                   BARPUC
059700160511     C                   ENDIF
059800160511     c                   other
059900160511     C     BARPUC        IFGT      §vpoMPV
060000160511     C                   CLEAR                   BARPUC
060100160511     C                   ENDIF
060200160511     C     BARVUC        IFGT      §vpoMVV
060300160511     C                   CLEAR                   BARVUC
060400160511     C                   ENDIF
060500160511     C                   Endsl
060600160511     c
060700040805      * CLEARO PESO E VOLUME UNITARI SE INFERIORI AL MINIMO PREVISTO
060800040805      * I LIMITI VALGONO PER TUTTI I TIPI DI IMPIANTO
060900040805     C     BARPUC        IFLT      §vpoSPV
061000040805     C                   CLEAR                   BARPUC
061100040805     C                   ENDIF
061200040805     C     BARVUC        IFLT      §vpoSVV
061300040805     C                   CLEAR                   BARVUC
061400040805     C                   ENDIF
061500020204     C* CONTROLLO CHE ESISTA COD,ANOMALIA NELLA TABELLA ANOMALIE
061600020204     C     BARCAN        IFNE      ' '
061700020204     C     BARCAN        ANDNE     ' '
061800020204     C     BARCAN        LOOKUP    CAN                                    34
061900020204     C  N34              CLEAR                   BARCAN
062000020204     C                   ENDIF
062100040419     c
062200070906     c* Non carico se LNP inesistente
062300040419     C     BARLNP        CHAIN     AZORG01L                           31
062400040419     c* Non carico se LNA inesistente
062500070906   4bC     *IN31         IFEQ      *Off
062600040419     c     orgfag        andeq     'A'
062700040419     C     *IN31         oreq      *Off
062800040419     c     orgfag        andeq     'F'
062900980212     C*
063000991230     C     BARLNA        CHAIN     AZORG01L                           31
063100040419     c* non carico se LNA inesistente
063200040303    5C     *IN31         IFEQ      *ON
063300040224     c     orgfag        orne      'A'
063400040224     c     orgfag        andne     'F'
063500020212     C                   CLEAR                   NTWBAR
063600040303   x5C                   ELSE
063700020212     C                   MOVEL     ORGDE3        OG143
063800020212     C                   MOVEL     §OGNTW        NTWBAR
063900070912     c
064000070912     C* LETTURA FOGLIO DELLA SPUNTA E IMPOSTAZIONE CAMPI DI COMOD
064100070912     C                   EXSR      BARFGV
064200070912     c
064300070912     c* Non carico se numero foglio inesistente
064400070912   5ac                   if        bardfv>0
064500140616     c                   clear                   amb_QD            1
064600140613     c                   clear                   A_imm
064700140613     c                   clear                   B_imm
064800140613     c*
064900140613     c* se IMA e spunta consegna PIu' RECENTE --> cancello senza caricare
065000140613     c                   if        barnpg=3 and barspg='A'
065100140616     c                   eval      kdel_npg=4
065200140613     c                   exsr      Cer_cons
065300140613     c                   endif
065400140616     c                   if        barnpg=4
065500140616     c                   eval      kdel_npg=3
065600140616     c                   exsr      Cer_cons
065700140616     c                   endif
065800140613
065900140616   5cc                   if        amb_QD<>'N'
066000040303     c**
066100040303     C* VERIFICO IL FLAG NELLA DS7N
066200040303     C                   Z-ADD     1             WA                3 0
066300040310     C     BARNPG        LOOKUP    CAT(WA)                                46
066400040310    6C     *IN46         IFEQ      *OFF
066500040303     C     NST(WA)       ORNE      '1'
066600040303     c* Se tengo tutte le spunte, verifico spunta doppia per lo stesso NFV
066700070131     C     KBAR1         CHAIN     FNBRV01L                           31
066800040303   x6c                   else
066900911016     C*
067000040303     C* spunta doppia per stessa categoria e p.o. gestione
067100140613     C     KBAR2         CHAIN     FNBRV05L                           31
067200040303    6c                   endif
067300941229     C*
067400040303     C* Aggiorno bolle e spunta se:
067500040303     C* - spunta nuova (31 on)
067600040303     C* - spunta già esistente ma dati del record diversi(BAR1<>BRV)
067700040303     C* - spunta già presente ma nel giro prec.trovati record allocati(B>1)
067800070212     c* - trovati errori  (BARERR=E)
067900070912   5bC     *IN31         IFEQ      *ON
068000941229     C     BAR1          ORNE      BRV
068100001114     C     B             ORGT      1
068200070212     C     BARERR        OReq      'E'
068300040303     c**
068400040303     C* OPERAZIONI PER SPUNTA GIA' PRESENTE e da tenerne una
068500040303    6C     *IN31         IFEQ      *OFF
068600040303     C     nst(WA)       ANDEQ     '1'
068700010608     C     BAR1          ANDNE     BRV
068800010608     C                   EXSR      SRBRVE
068900040303    6C                   ENDIF
069000070710     c**
069100070710     C* OPERAZIONI PER SPUNTA GIA' PRESENTE e da tenerne una
069200070710    6C     *IN31         IFEQ      *OFF
069300070710     C     nst(WA)       ANDne     '1'
069400070710     C     BAR1          ANDNE     BRV
069500070710     c* la precedente spunta non ha l'anomalia
069600070710     c     brvcan        andeq     ' '
069700070710     c* nell'attuale è stata inserita
069800070710     c     barcan        andne     ' '
069900070822     c*  Carico solo se data/ora Immissione spunta sono < della precedente
070000070822     c     barimmis      andgt     brvimmis
070100070710     C                   EXSR      SRBRVE
070200070710    6C                   ENDIF
070300040303     C* RIEMPI I CAMPI
070400040303     C                   EXSR      RIEMPI
070500040303     C**
070600040303     C**  A G G I O R N A M E N T I
070700961213     C                   MOVE      'S'           OKAGG             1
070800911112     C                   EXSR      UPDARB
070900911014     C**
071000920424     C**  AGGIORNO SPUNTA
071100950707     C*   Non l'aggiorno se spunta partenza manuale e di collo già
071200950707     C*   consegnato
071300040303    6C     OKAGG         IFEQ      'S'
071400040303     C*
071500960521     C* SE NO  LA DEVO TRASMETTERE LA FLAGGO
071600961213    7C     WNOTRA        IFEQ      'S'
071700960521     C                   MOVEL     'N'           BRVFTR
071800960521     C                   MOVEL     *HIVAL        BRVDTR
071900991223     C                   MOVEL     *HIVAL        BRVHTR
072000960521     C                   CLEAR                   WNOTRA
072100961213    7C                   ENDIF
072200980115     C* SE SPUNTA PARTENZA RELATIVA AD UN FOGLIO VIAGGIO SU CUI NON E'
072300980115     C* PIU' PRESENTE LA LNA DEL COLLO -> FLAGGO LA SPUNTA
072400980205     C* (RIENTRA IN QUESTO CASO ANCHE UNA SPUNTA CON COD.ANOMALIA "E"
072500980205     C* CIOE' UNA SPUNTA LA CUI LINEA NON ERA PRESENTE SUL FV GIA' NEL
072600980205     C* MOMENTO IN CUI E' STATA FATTA)
072700040303    7C     BARNPG        IFEQ      1
072800980115     C                   MOVE      BARLNA        W003A             3
072900980313     C     W003A         LOOKUP    FFS                                    33
073000980115     C  N33              MOVEL     'L'           BRVFTR
073100980115     C  N33              MOVEL     *HIVAL        BRVDTR
073200991223     C  N33              MOVEL     *HIVAL        BRVHTR
073300980210     C   33BRVCAN        IFEQ      'E'
073400980210     C                   CLEAR                   BRVCAN
073500980210     C                   END
073600041020    7C                   ENDIF
073700041020     c
073800041020     c* Imposto brvatr='D' per disguido
073900041020     c                   if        wtibol='D'
074000041020     c                   eval      brvatr='D'
074100041020     c                   else
074200041020     c                   clear                   brvatr
074300041020    7C                   ENDIF
074400040303     c**
074500070131     C* AGGIORNO LF FNBRV01L
074600040310    7C  N31*IN46         IFEQ      *OFF
074700040303     C     NST(WA)       ORNE      '1'
074800070131     C                   UPDATE    FNBRV000
074900040303     C                   ELSE
075000140613     C* AGGIORNO LF FNBRV05L
075100140613     C                   UPDATE    FNBRV005
075200040303    7C                   END
075300010607     C*
075400070131     C   31              WRITE     FNBRV000                             38
075500140613     c
075600140613     c* Se previsto cancello la spunta "doppia" tra 3 e 4
075700140616    7c                   if        amb_QD='S'
075800140616     c                   exsr      Canc_QD
075900140616    7c                   endif
076000150824     c**
076100150824     c*  COLLO SPUNTABILE DA PIÙ FILIALI
076200150824     c**
076300150824     c** elaboro per "K-cli" con BPES <> LNPD ( in qeusto caso ho pulito
076400150824     c*                                        AGGPAR)
076500150824     c*  oppure  per "L-linea" se TFPLNP <>TFPPES
076600150824     c* lo faccio dopo la write di BRV perchè fnlv52r deve leggere la spunta per
076700150824     c*  determinare la nuova lnp della bolla
076800150824     C                   IF        wbsptp<>' ' and wbspspu='S'
076900150824     c                             and wagpar=' '
077000150824     c                   exsr      DA_BSP
077100150824     c                   endif
077200140613     c
077300040303   X6C                   ELSE
077400040303     C*
077500040310    7C  N31*IN46         IFEQ      *OFF
077600040303     C     NST(WA)       ORNE      '1'
077700070131     C                   UNLOCK    FNBRV01L
077800040303     C                   ELSE
077900140613     C                   UNLOCK    FNBRV05L
078000040303    7C                   END
078100040303    6C                   END
078200950927     C* PER BOLLA ARRIVO NON LOCALE, VEDO SE CREARE ANOMALIA 50
078300950927     C*  COLLO ARRIVATO NON PARTITO
078400960207     C*  NON ELABORO SE COLLO LOCALE O SOLO DELLA LINEA DI PARTENZA
078500020214    6C  N05WTIBOL        IFEQ      'D'
078600020214     C     WTIBOL        OREQ      'A'
078700020214     C     WTIBOL        OREQ      'T'
078800020218     C     WTIBOL        OREQ      'L'
078900950927     C                   EXSR      CRTA50
079000950927    6C                   ENDIF
079100950927     C*
079200070912   5bC                   ELSE
079300040303     C*
079400040310    6C  N31*IN46         IFEQ      *OFF
079500040303     C     NST(WA)       ORNE      '1'
079600070131     C                   UNLOCK    FNBRV01L
079700040303     C                   ELSE
079800140613     C                   UNLOCK    FNBRV05L
079900040303    6C                   END
080000070912   5bC                   END
080100140613   5cC                   END
080200110103     c*
080300070912   5ac                   else
080400070912     c* Scrivo la spunta errata in fibar00e
080500070912     c                   movel     bpes          barpes
080600070912     c                   movel     wdate1        bardim
080700070912     c                   write     barcoerr                             99
080800070912   5aC                   END
080900040303    5C                   END
081000070906   4bC                   END
081100040419    4C                   END
081200911112     C*
081300900619     C* CANCELLO RECORD LETTO
081400001114     C* Se spunta imi e non ho aggiornato blt perchè inesistente o se
081500021203     C* non ho aggiornato blp perchè record allocato la spunta non viene
081600001114     C* cancellata.
081700021107     c                   if        not *in38 and not *in39
081800021107     C                   DELETE    BARCO
081900021107     c                   else
082000021203     c* Se ci fosse errore ma passati 4 giorni dalla data di immissione
082100021203     c*  spunte, deleto lo stesso
082200021203     c                   if        brvdfs<=wpul050
082300021203     c                   DELETE    BARCO
082400021203     c                   else
082500070212     c                   eval      barerr='E'
082600021107     c                   update    BARCO
082700021107     c                   endif
082800021203     c                   endif
082900900618     C*
083000080325     C                   READ      FIBAR00F                             9830
083100930521    3C                   END
083200021112     C** 40              ADD       1             B
083300021112     C   40              z-add     4             B
083400920424     C* DOPO 3 VOLTE ESCO
083500930521    3C   40B             IFGT      3
083600920424     C                   SETOFF                                       40
083700930521   X3C                   ELSE
083800070212     C                   CLOSE     FIBAR00F
083900070212     C                   OPEN      FIBAR00F
084000070212     C                   READ      FIBAR00F                               30
084100930521    3C                   END
084200930521    2C                   END
084300970224     C* ELABORO L'ULTIMO DATO MEMORIZZATO
084400970224     C                   EXSR      WRTAGB
084500911016     C*
084600911016     C     FINE          TAG
084700930521     C** 02 OFF - PROGRAMMA NON RICHIAMATO
084800930521     C**
084900950926     C** LANCIO PGM DI AGGIORNAMENTO BOLLE
085000961212    2C     *IN02         IFEQ      *OFF
085100970814     C**
085200970814     C                   CLEAR                   DSLV55
085300970814     C                   MOVEL     'C'           D55TLA
085400020213     C                   EXSR      FNLV55
085500000705     C**
085600000705     C                   CLEAR                   DSLV53
085700000705     C                   MOVEL     'C'           D53TLA
085800000705     C                   CALL      'FNLV53R'
085900000705     C                   PARM                    DSLV53
086000930521     C*
086100040601     c                   if        wapri='S'
086200070212     C                   CLOSE     FIBAR00F
086300040601     c                   endif
086400930521     C*
086500981105     C* SE HO ALEMNO UN FOGLIO SCRITTO--> TRASMETTO
086600111018     C***  ERF(1)        IFNE      *BLANKS
086700111018     C***                EXSR      TRASMI
086800111018     C***                ENDIF
086900040811     c*
087000040811     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
087100040811     c                   clear                   fidn12ds
087200040811     c                   eval      i12tla = 'C'
087300040811     c                   call      'FIDN12R'
087400040811     c                   parm                    fidn12ds
087500040811     c*
087600930521     C                   SETON                                        LR
087700930521     C*
087800930521   X2C                   ELSE
087900930521     C**
088000930521     C** 02 ON  - PROGRAMMA RICHIAMATO
088100930521     C**
088200921020     C* SE NON HO AGGIORNATO IMPOSTO LA "E" DI ERRORE
088300960521     C     *IN39         IFEQ      *ON
088400070221     C                   MOVEL     'E'           dls45FLG
088500960521     C                   ELSE
088600960521     C*
088700950926     C* SE AGGIORNATO FACCIO SOTTOMETTERE IL PGM DI AGGIORNAMENTO BOLLE
088800070221     C                   MOVEL     'A'           dls45FLG
088900950707     C* Se non ho aggiornato perchè spunta partenza manuale di collo
089000950707     C* consegnato imposto "N" di aggiornamento non effettuato
089100960521     C     OKAGG         IFEQ      'N'
089200070221     C                   MOVE      'N'           dls45FLG
089300950707     C                   END
089400960521     C                   ENDIF
089500070208     C                   MOVE      DLS45FLG2     FLGPRE            1
089600930521     C*
089700930521     C                   RETURN
089800930521    2C                   END
089900930521    1C                   END
090000941214     C**************************************************************************
090100941214     C* RILEVO IL FOGLIO VIAGGIO PARTENZE/ARRIVI
090200941214     C**************************************************************************
090300941214     C     RILFOG        BEGSR
090400941214     C*
090500941214     C                   Z-ADD     *ZERO         WDFV
090600941229     C                   CLEAR                   WSPG
090700021014     C                   Z-ADD     0             WFLE
090800961122     C                   CLEAR                   WTTR
090900980115     C                   CLEAR                   FFG
091000140613     C                   CLEAR                   WFCF
091100021024     c*
091200021024     c* Prendo il terminal di partenza del p.o. in gestione
091300070212    1c                   if        savfgs<>wfgs
091400021024     C                   CLEAR                   DSLV55
091500021024     C                   MOVEL     'P'           D55TPT
091600021024     C                   MOVEL     WFGS          D55LIN
091700021024     C                   MOVEL     WDATE1        D55DRF
091800021024     C                   CALL      'FNLV55R'
091900021024     C                   PARM                    DSLV55
092000021024     C**
092100070212    2C     D55ERR        IFne      ' '
092200021024     C                   MOVE      wfgs          tfpfgs
092300021024     c                   else
092400021024     C                   MOVE      d55tfp        tfpfgs
092500070212    2c                   endif
092600021024     c                   eval      savfgs=wfgs
092700070212    1c                   endif
092800941214     C*
092900941214     C* ARRIVI O PARTENZE ?
093000970114    1C     WNPG          IFEQ      1
093100941214     C*
093200021014     C     KFGV1         CHAIN     FNFGV01L                           99
093300000505     C*
093400070209    2C     *IN99         IFEQ      *OFF
093500021014     C     KFGV1         CHAIN     FNFGW01L                           28
093600070209    3C     *IN28         IFEQ      *ON
093700000505     C     FGWATB        ORNE      ' '
093800000505     C                   CLEAR                   FGWFF3
093900000505     C                   CLEAR                   FGWFF4
094000000505     C                   CLEAR                   FGWFL3
094100000505     C                   CLEAR                   FGWFL4
094200070209    3C                   ENDIF
094300980928     C                   Z-ADD     FGVDFV        WDFV
094400021024     C                   Z-ADD     FGVLNP        WFgs
094500021024     C                   Z-ADD     FGVLNP        WFLE
094600980928     C                   MOVEL     FGVTTR        WTTR
094700140613     C                   MOVEL     FGVFCF        WFCF
094800980928     C**
094900070209   X2C                   ELSE
095000960925     C     KFVA1         CHAIN     FNFVA01L                           99
095100970114    3C     *IN99         IFEQ      *OFF
095200970114     C                   Z-ADD     FVADFV        WDFV
095300021014     C                   Z-ADD     FVALNP        WFLE
095400970114     C                   MOVEL     FVATTR        WTTR
095500140613     C                   MOVEL     'S'           WFCF
095600111018   x3C**                 ELSE
095700110103     c*
095800070209     C* Se non trovato invio msg di errore
095900111018     C**                 EXSR      INVMSG
096000070212    3C                   ENDIF
096100070212    2C                   ENDIF
096200941214     C*
096300970114   X1C                   ELSE
096400941214     C*
096500021014     C* ACCEDO DIRETTAMENTE AL FOGLIO CON P.O. GESTIONE
096600021014     C* SE FOGLIO NON TROVATO CERCO CON SIMFEL- PUÒ ESSERE SOLO UN
096700070212     C*  FOGLIO IMP SOLO SE RICHIAMO PER DATI DI FIBAR!!
096800021014     C     KFVV1         CHAIN     FNFVV01L                           99
096900070209    2C     *IN99         IFEQ      *ON
097000021014     C     WNPG          ANDEQ     3
097100021014     C     RICBAR        ANDEQ     'S'
097200021014     C                   Z-ADD     SIMFEL        WFGS
097300021014     C     KFVV1         CHAIN     FNFVV01L                           99
097400021014     C  N99FVVSPG        IFNE      'P'
097500021014     C                   SETON                                        99
097600021014     C                   ENDIF
097700070209    2C                   ENDIF
097800021014     C*
097900070209    2C     *IN99         IFEQ      *OFF
098000980928     C                   Z-ADD     FVVDFV        WDFV
098100980928     C                   MOVEL     FVVSPG        WSPG
098200021014     C                   MOVEL     FVVFGS        WFGS
098300140613     C                   MOVEL     FVVFCF        WFCF
098400111018     C**                 ELSE
098500111018     C**                 EXSR      INVMSG
098600070209    2C                   ENDIF
098700941214     C*
098800970114    1C                   ENDIF
098900021014     C**
099000021024     C* SE WFLE=0 prendo TERMINAL DI PARTENZA di fgs A UDATE
099100070223     c*   di fatto si tratta del TFP di BPES poteva che può differire
099200070223     c*   dal FGS solo in caso di spunte ion procedura arrivo per p.o.
099300070223     c*   con terminal di partenza <> dal terminal arrivo
099400021014    1C     WFLE          IFEQ      0
099500021024     C                   MOVE      tfpfgs        WFLE
099600021014    1C                   END
099700941214     C*
099800941214     C                   ENDSR
099900020213     C**************************************************************************
100000020213     C*    IMPOSTAZIONI DI ALCUNI CAMPI DI PASSAGGIO SE VUOTI
100100020213     C**************************************************************************
100200020213     C     IMPO          BEGSR
100300150824     c                   eval      %subst(dls45flo:1:1)=' '
100400110103     c*
100500070208     C* BPES--> SE NON passato    E' SIMFEL
100600020213     C     BPES          IFEQ      0
100700020213     C                   Z-ADD     SIMFEL        BPES
100800070208     c                   if        btfp=0
100900070208     C                   Z-ADD     SIMFEL        BTFP
101000020213     C                   ENDIF
101100070208     C                   ENDIF
101200020213     C**
101300070208     C* BTFP--> SE NON passato ma c'è BPES, è terminal partenza di BPES
101400070208     c                   if        BTFP=0
101500020213     C                   CLEAR                   DSLV55
101600020213     C                   MOVEL     'P'           D55TPT
101700020213     C                   Z-ADD     BPES          D55LIN
101800020213     C                   Z-ADD     WDATE1        D55DRF
101900020213     C                   EXSR      FNLV55
102000110103     c*
102100020213     C                   Z-ADD     D55TFP        BTFP
102200020213     C                   ENDIF
102300110103     c*
102400070209     c* Se BPES non passato, reimposto anche il campo di input
102500070212     c                   if        DLS45PES<=*zeros
102600070209     c                   movel     bpes          dls45pes
102700070209     c                   endif
102800110103     c*
102900020213     C                   ENDSR
103000961213     C**************************************************************************
103100961213     C*    RIEMPI I CAMPI DEL RECORD DA SCRIVERE
103200961213     C**************************************************************************
103300961213     C     RIEMPI        BEGSR
103400110103     c*
103500080317     C* CONTROLLO SE E' PISTOLA MANUALE
103600080317     C                   EXSR      PISMAN
103700080317     c
103800961213     C* SE NON C'E' AGGIUNGO ALTRIMENTI AGGIORNO
103900080317    1C     WBRVE         IFEQ      'S'
104000070131     C* Se routine richiamata per scrittura di FNBRVe devo sempre clear
104100010608     c* are brvatr
104200010608     C                   CLEAR                   BRVATR
104300070201     C                   CLEAR                   BRVtsu
104400080317   x1C                   ELSE
104500961213     C   31              CLEAR                   BRVATR
104600070201     C   31              CLEAR                   BRVtsu
104700110103     c*
104800080317     c** Conteggio le spunte risparate + volte
104900080317     c** ignoro tutte le spunte manuali
105000100709     c** ignoro se spunta con errore (già caricata)
105100080422    1c                   if        not *in31  and (rps(y)<>'M' or
105200080422     c                             rps(y)='M' and tps(y)='C')
105300100709     c                             and barerr<>'E'
105400110103     c*
105500080317    2c                   if        nst(WA)<>'1'  or
105600080317     c* Se spunta unica  ma stesso numero foglio --> conto doppio
105700080317     c                             (nst(WA)= '1' and brvnfv=barnfv)
105800110103     c*
105900080630     c* Non considero doppia nemmeno se spunta cat 3 o 4 pistola 49
106000080630     c*  sparate entro i 2 sec.
106100080630     c                   eval      wok='S'
106200080630    3c                   if        (barnpg=3 or barnpg=4) and barnps=49
106300080630     c                             and brvnps=49 and brvdfs=bardfs
106400080630     c                   move      barhms        Timenew
106500080630     c                   move      brvhms        Timeold
106600080630     c     Timenew       subdur    Timeold       secondi:*s
106700080701     c                   mllzo     1             secondi
106800080630    4c                   if        secondi<=§vposecdop
106900080630     c                   eval      wok='N'
107000080630    4c                   endif
107100080630    3c                   endif
107200110103     c*
107300080630   2AC                   IF        WOK='S'
107400080317    3c                   if        brvtsu=' '
107500080317     c                   eval      brvtsu='1'
107600080317   x3c                   else
107700080317     c                   movel     brvtsu        wtsu              1 0
107800080317    4c                   if        wtsu<9
107900080317     c                   eval      wtsu=wtsu+1
108000080317     c                   movel     wtsu          brvtsu
108100080317    4c                   endif
108200110103     c*
108300080317    3c                   endif
108400080630   2Ac                   endif
108500110103     c*
108600080317   x2c                   else
108700080317     c* Prendo data foglio
108800080317    3C     BRvKEY        IFNE      SA1KEY
108900080317     C                   MOVEL     BRvKEY        SA1KEY
109000080317     C                   Z-ADD     BRvNPG        WNPG
109100080317     C                   Z-ADD     BRvNFV        WNFV
109200080317     C                   Z-ADD     BRvFGS        WFGS
109300080317     C                   movel     BRvnps        Wnps
109400080317     C                   EXSR      RILFOG
109500080317     c                   Z-ADD     WDFV          BRVDFV
109600140616     c                   movel     WFCF          BRVFCF
109700140616     c                   movel     Wspg          BRVspg
109800080317    3c                   endif
109900110103     c*
110000080317     c* Se spunta unica, foglio diverso ma stessa data -->
110100080317     c*                         mantengo TSU
110200080317     c*                                    data diversa --> lo pulisco
110300080317    3c                   if        bardfv<>brvdfv
110400080317     c                   clear                   brvtsu
110500080317    3c                   endif
110600110103     c*
110700080317    2c                   endif
110800080317    1c                   endif
110900110103     c*
111000080317    1C                   ENDIF
111100110103     c*
111200070201     C                   CLEAR                   BRVnvr
111300961213     C* FILIALE GESTIONE LA PRENDO DAL FOGLIO
111400961213     C                   CLEAR                   BRVDTR
111500961213     C                   CLEAR                   BRVFTR
111600991223     C                   CLEAR                   BRVHTR
111700961213     C                   Z-ADD     BARFGS        BRVFGS
111800961213     C                   Z-ADD     BARNPG        BRVNPG
111900990708     C                   Z-ADD     BARNFV        BRVNFV
112000040428     C* P.O. CHE HA EFFETTUATO LA SPUNTA (l'originale contenuto il WPES
112100040428     c*  e non più il bpes, per le spunte cat.1)
112200040428     C                   Z-ADD     WPES          BRVPES
112300961213     C                   MOVEL     BARLNP        BRVLNP
112400961213     C                   MOVEL     BARLNA        BRVLNA
112500961213     C                   MOVEL     BARNRS        BRVNRS
112600961213     C                   MOVEL     BARNSC        BRVNSC
112700961213     C                   Z-ADD     BARZNC        BRVZNC
112800961213     C**
112900961213     C* SE SONO IN IMMISSIONE LA AGGIORNO, IN VARIAZIONE SOLO
113000970829     C*  SE E' PISTOLA CON PRIORITA' < O UGUALE ALLA PRESENTE
113100961213     C*
113200961213     C* SE L'ANOMALIA GIA' ESISTE, NON LA TOLGO SE SU QUESTA SPUNTA
113300961213     C*  NON C'E'
113400040324    1C     *IN31         IFEQ      *ON
113500010608     C     WBRVE         OREQ      'S'
113600961213     C                   MOVEL     BARCAN        BRVCAN
113700961213     C                   MOVEL     BARNPS        BRVNPS
113800070212     C                   MOVEL     BARtap        BRVtap
113900040324     C                   Z-ADD     BARVUC        BRVVUC
114000040324     C                   Z-ADD     BARPUC        BRVPUC
114100040324   x1C                   ELSE
114200961213     C* CODICE ANOMALIA
114300961213     C     BARCAN        IFNE      ' '
114400980205     C     BRVCAN        OREQ      'E'
114500961213     C                   MOVEL     BARCAN        BRVCAN
114600961213     C                   ENDIF
114700110103     c*
114800970829     C* CERCO LA PRIORITA' DELLA PISTOLA GIA' PRESENTE
114900970829     C                   MOVEL     BRVNPS        ALF2
115000970829     C                   EXSR      RICRAG
115100970829     C* PISTOLA CON PRIORITA' < O = ALLA PRESENTE: E' + IMPORTANTE
115200970829     C     OPS(Y)        IFLE      OPS(Z)
115300961213     C                   MOVEL     BARNPS        BRVNPS
115400070212     C                   MOVEL     BARtap        BRVtap
115500961213     C                   ELSE
115600961213     C                   MOVEL     BRVNPS        BARNPS
115700070212     C                   MOVEL     BRVtap        BARtap
115800961213     C                   ENDIF
115900110103     c*
116000041011     c* Peso e volume prendo se pieno e se non c'e' già
116100041011     c                   if        barpuc>0
116200041011     c                   if        *in31 or (not *in31 and brvpuc=0)
116300040324     C                   Z-ADD     BARPUC        BRVPUC
116400040324     c                   endif
116500041011     c                   endif
116600040324     c                   if        barvuc>0
116700041011     c                   if        *in31 or (not *in31 and brvvuc=0)
116800040324     C                   Z-ADD     BARvUC        BRVvUC
116900040324     c                   endif
117000041011     c                   endif
117100110103     c*
117200040324    1C                   ENDIF
117300961213     C*
117400070212     c* Se la spunta era già stata caricata con errore (di solito
117500070212     c*  per la non creazione della anomalia 50) non aggiorno la data/ora
117600070212     c*  caricamento spunta
117700070212     c                   if        barerr<>'E' or *in31
117800961213     C                   Z-ADD     WDATE1        BRVDCS
117900070131     C                   time                    BRVHCS
118000070212     c                   endif
118100140613     c* impostazione data caricamento spunte
118200140613     c                   exsr      impoDFS
118300140613     c                   z-add     wdfs          brvdfs
118400140613     c
118500070131     c                   clear                   brvhms
118600070131     C                   movel     BARHMS        BRVHMS
118700070212     c                   z-add     barmis        brvmis
118800961213     C                   Z-ADD     BARFLE        BRVFLE
118900070212     C                   movel     BARpru        BRVpru
119000080314     c
119100080314     c
119200961213     C                   ENDSR
119300941206     C**************************************************************************
119400941206     C*    CARICO TABELLE
119500941206     C**************************************************************************
119600941229     C     CARTAB        BEGSR
119700930521     C*
119800930521     C* PARAMETRI UTENTE
119900110103     C                   Z-ADD     1             CODUT             1 0
120000110103      *
120100110103      * Reperimento dati utente/aziendali
120200110103     c                   exsr      sr_DatiJob
120300970320     C*
120400970320     C                   CLEAR                   SAVBOL
120500961212     C**
120600961212     C**  CARICO TABELLA 7J - CODICI PISTOLA
120700961122    1C     *IN02         IFEQ      '0'
120800921229     C                   MOVEL     '7J'          COD
120900990108     C                   Z-ADD     1             X                 4 0
121000921229     C     KTAB2         SETLL     TABEL
121100950927     C     KTAB2         READE     TABEL                                  99
121200921229     C*
121300961122    2C     *IN99         DOWEQ     *OFF
121400961122    3C     TBLFLG        IFEQ      ' '
121500921229     C                   MOVEL     TBLKEY        NPS(X)
121600921229     C                   MOVEL     TBLUNI        DS7J
121700921229     C                   MOVEL     §7JRPS        RPS(X)
121800000210     C                   MOVEL     §7JTPS        TPS(X)
121900000210     C                   MOVEL     §7JORD        OPS(X)
122000000316     C                   MOVEL     §7JINV        INV(X)
122100040503     C                   MOVEL     §7Jfsl        fsl(X)
122200921229     C                   ADD       1             X
122300961122    3C                   END
122400921229     C*
122500950927     C     KTAB2         READE     TABEL                                  99
122600961122    2C                   ENDDO
122700961122    1C                   ENDIF
122800961122     C**
122900961212     C** CARICO TABELLA  TV - TIPI TRAINO DEFLUENZA
123000961122     C                   MOVEL     'TV'          COD
123100990108     C                   Z-ADD     1             X
123200961122     C     KTAB2         SETLL     TABEL
123300961122     C     KTAB2         READE     TABEL                                  99
123400961122     C*
123500961122    2C     *IN99         DOWEQ     *OFF
123600961122    3C     TBLFLG        IFEQ      ' '
123700961122     C                   MOVEL     TBLUNI        DSTV
123800961122    4C     §TVDEF        IFEQ      'S'
123900961122     C                   MOVEL     TBLKEY        TTR(X)
124000961122     C                   ADD       1             X
124100961122    4C                   ENDIF
124200961122    3C                   ENDIF
124300961122     C*
124400961122     C     KTAB2         READE     TABEL                                  99
124500961122    2C                   ENDDO
124600961212     C**
124700961212     C* CARICO LA TABELLA 7N - DELLE CATEGORIE SPUNTE
124800941214     C                   MOVEL     '7N'          COD
124900941214     C                   Z-ADD     1             X
125000941214     C     KTAB2         SETLL     TABEL
125100950927     C     KTAB2         READE     TABEL                                  99
125200941214     C*
125300950927     C     *IN99         DOWEQ     '0'
125400941214     C     TBLFLG        IFEQ      ' '
125500941214     C                   MOVEL     TBLKEY        CAT(X)
125600941214     C                   MOVEL     TBLUNI        DS7N
125700941214     C                   MOVEL     §7NNST        NST(X)
125800941214     C                   ADD       1             X
125900941214     C                   ENDIF
126000941214     C*
126100950927     C     KTAB2         READE     TABEL                                  99
126200941214     C                   ENDDO
126300961212     C**
126400960702     C** CARICO TABELLA 5F - FILIALI CONFERMA BOLLE DA DISK
126500960702     C                   MOVEL     '5F'          COD
126600960702     C                   Z-ADD     0             X
126700960702     C     KTAB2         SETLL     TABEL
126800960702     C     KTAB2         READE     TABEL                                  99
126900960702     C*
127000960702    1C     *IN99         DOWEQ     '0'
127100960702    2C     TBLFLG        IFEQ      ' '
127200070201     C                   MOVEL     TBLKEY        W003A             3
127300070201     C     W003A         IFNE      '   '
127400960702     C                   ADD       1             X
127500960702     C                   MOVEL     TBLKEY        S5F(X)
127600970828     C* MEMORIZZO LA FIL CHE CONFERMA
127700970828     C                   MOVEL     TBLUNI        F5F(X)
127800110103     c*
127900130618     C     X             IFGE      9000
128000070201     C                   SETON                                        99
128100070201     C                   ENDIF
128200110103     c*
128300970828     C                   ENDIF
128400960702    2C                   ENDIF
128500960702     C*
128600960702     C  N99KTAB2         READE     TABEL                                  99
128700960702    1C                   ENDDO
128800961212     C**
128900020204     C** CARICO TABELLA 3E - CODICI ANOMALIA VALIDI SU SPUNTE
129000020204     C                   MOVEL     '3E'          COD
129100961212     C                   Z-ADD     0             X
129200961212     C     KTAB2         SETLL     TABEL
129300961212     C     KTAB2         READE     TABEL                                  99
129400961212     C*
129500961212    1C     *IN99         DOWEQ     *OFF
129600961212    2C     TBLFLG        IFEQ      ' '
129700961212     C                   ADD       1             X
129800020204     C                   MOVEL     TBLKEY        CAN(X)
129900961212     C     X             IFGE      200
130000961212     C                   SETON                                        99
130100961212     C                   ENDIF
130200961212    2C                   ENDIF
130300961212     C*
130400961212     C  N99KTAB2         READE     TABEL                                  99
130500961212    1C                   ENDDO
130600020502     C*****
130700020502     C* RILEVO DATA E ORA PER SCRIVERLO NELLE SPUNTE
130800020502     C                   TIME                    WTIME            14 0
130900020502     C                   MOVE      WTIME         WDATE             8 0
131000020502     C                   Z-ADD     WDATE         G02DAT
131100020502     C                   Z-ADD     *ZERO         G02INV
131200020502     C                   MOVEL     *BLANKS       G02ERR
131300020502     C                   CALL      'XSRDA8'
131400020502     C                   PARM                    WLBDAT
131500020502     C                   Z-ADD     G02INV        WDATE1            8 0
131600030725      *
131700030725      * CHAIN A VARIABILI PARTENZE x PRENDERE MAX PESO E VOLUME CARICABILI
131800030725     C                   EXSR      CERVPO
131900970522     C*
132000970522     C* CHAIN A VARIABILI ARRIVI PER VEDERE A QUALE LIVELLO SI CHIUDE
132100970522     C* L'IDD (LIV MAX.)
132200970522     C                   MOVEL     '7A'          COD
132300970522     C                   MOVEL(P)  '2'           KEY
132400970522     C     KTAB          CHAIN     TABEL                              99
132500970522     C  N99              MOVEL     TBLUNI        DS7A2
132600000927     C* CHAIN A GG PULIZIA PER CREAZIONE ANOMLIA 10 E 50
132700000927     C                   MOVEL     '5A'          COD
132800000927     C                   MOVEL(P)  '1'           KEY
132900000927     C     KTAB          CHAIN     TABEL                              99
133000000927     C  N99              MOVEL     TBLUNI        DS5A
133100000927     C*
133200000927     C                   MOVEL     '5A'          COD
133300000927     C                   MOVEL(P)  'PT'          KEY
133400000927     C     KTAB          CHAIN     TABEL                              99
133500000927     C  N99              MOVEL     TBLUNI        DS5APT
133600000927     C*
133700000927     C* PREDO GG MINORI TRA PULIZIA ARB E PULIZIA ARB POSTE
133800000927     C     §5AARB        IFLT      §5APT5
133900000927     C                   Z-ADD     §5AARB        WGGPUL            3 0
134000000927     C                   ELSE
134100000927     C                   Z-ADD     §5APT5        WGGPUL            3 0
134200000927     C                   ENDIF
134300000927     C**
134400000927      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA BOLLE ARRIVI
134500000927     C                   MOVE      *BLANKS       $DAT
134600000927     C                   MOVE      *BLANKS       $ERR
134700000927     C                   MOVEL     'FNLR84R'     $PGM
134800000927     C                   CALL      'TRUL49C'                            28
134900000927     C                   PARM                    $PGM             10
135000000927     C                   PARM                    $DAT              8
135100000927     C                   PARM                    $ERR              1
135200000927     C*
135300000928    3C     $ERR          IFEQ      '1'
135400000927     C     *IN28         OREQ      *ON
135500000928     C     $DAT          ORLE      *ZEROS
135600000927     C                   Z-ADD     WDATE1        G02INV
135700000927     C                   ELSE
135800000928     C                   MOVEL     $DAT          G02INV
135900000927    3C                   ENDIF
136000000927     C*
136100000927      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI
136200000927      * DI PULIZIA BOLLE ARRIVI
136300000927     C                   Z-ADD     *ZEROS        G02DAT
136400000927     C                   Z-ADD     *ZEROS        G02TGI
136500000927     C                   MOVEL     '3'           G02ERR
136600000927     C                   CALL      'XSRDA8'
136700000927     C                   PARM                    WLBDAT
136800000927      *
136900000927     C                   SUB       WGGPUL        G02TGI
137000000928     C                   Z-ADD     *ZEROS        GI8DAT
137100000928     C                   Z-ADD     *ZEROS        GI8INV
137200000928     C                   Z-ADD     G02TGI        GI8TGI
137300000927     C                   CALL      'XSRGI8'
137400000927     C                   PARM                    WGIDAT
137500000927      *
137600000928     C                   Z-ADD     GI8INV        PRIDAT
137700961212     C**
137800070209     c** udate - 4 --> sono i giorni che devo tenere le spunte dentro al
137900070212     c**               FIBAR se non riesco a creare l'anomalia collo
138000021203     c**               arrivato non part
138100021203     C                   Z-ADD     WDATE1        G02INV
138200021203     C                   Z-ADD     *ZEROS        G02DAT
138300021203     C                   Z-ADD     *ZEROS        G02TGI
138400021203     C                   MOVEL     '3'           G02ERR
138500021203     C                   CALL      'XSRDA8'
138600021203     C                   PARM                    WLBDAT
138700021203      *
138800021203     C                   SUB       4             G02TGI
138900021203     C                   Z-ADD     *ZEROS        GI8DAT
139000021203     C                   Z-ADD     *ZEROS        GI8INV
139100021203     C                   Z-ADD     G02TGI        GI8TGI
139200021203     C                   CALL      'XSRGI8'
139300021203     C                   PARM                    WGIDAT
139400021203     C                   Z-ADD     GI8INV        wpul050
139500961212     C**
139600021203     C**
139700930521     C                   MOVEL     '1'           UNO               1
139800930521     C                   MOVEL     'AA'          SAVNPS            2
139900960411     C*
140000960411     C                   MOVE      ' '           OPN9              1
140100961212     C*
140200021014     C                   CLEAR                   DSBS55
140300971113     C                   MOVEL     'L'           I50TLA
140400021014     C                   CALL      'TIBS55R'
140500021014     C                   PARM                    DSBS55
140600971113     C**
140700971113     C                   CLEAR                   KPJBA
140800021014     C                   MOVEL     'EDPCED'      WCED
140900021014     C                   MOVEL     SIMFEL        WTER
141000021014     C                   MOVEL     WPROF         KNMUS
141100971113     C                   MOVEL     O50PSI        KNSIF
141200970829     C                   MOVEL     'DSP01'       KNMTD
141300961212     C                   MOVEL     'N'           KSTJO
141400961212     C                   MOVEL     'B'           KTPAZ
141500961212     C                   MOVEL     'AZNAUTO'     KNMEB
141600961212     C                   MOVEL     'J'           KEXCN
141700961212     C                   MOVEL     'P5'          KCOJB
141800961212     C                   MOVEL     'N'           KCANC
141900000927     C*
142000000927     C                   CLEAR                   SA2DFV
142100001109     C**
142200001109     C** CARICO TABELLA 7C - ANOMALIE CREABILI DA CAT.SPUNTA GENERICA
142300001109     C                   MOVEL     '7C'          COD
142400001109     C                   Z-ADD     0             X
142500001109     C     KTAB2         SETLL     TABEL
142600001109     C     KTAB2         READE     TABEL                                  99
142700001109     C*
142800001109    1C     *IN99         DOWEQ     *OFF
142900001109    2C     TBLFLG        IFEQ      ' '
143000001109     C                   MOVEL     TBLUNI        DS7C
143100001109    3C     §7CCCG        IFEQ      'S'
143200001109     C                   ADD       1             X
143300001109     C                   MOVEL     TBLKEY        ACG(X)
143400001109    3C                   ENDIF
143500001109    2C                   ENDIF
143600001109     C*
143700001109     C     KTAB2         READE     TABEL                                  99
143800001109    1C                   ENDDO
143900110103      *
144000091209      * Aggancio tabella AR5 per record 'GEN'
144100091209     c                   Clear                   dAr5
144200091209     c                   Clear                   dsbs02
144300091209     c                   Eval      T02Mod = 'C'
144400091209     c                   Eval      T02Sif = knsif
144500091209     c                   Eval      T02Cod = 'AR5'
144600091209     c                   Movel(p)  'GEN'         T02Ke1
144700091209     c                   Call      'TIBS02R'
144800091209     c                   Parm                    Kpjba
144900091209     c                   Parm                    dsbs02
145000091209     c                   If        T02Err = *Blanks
145100091209     c                   Movel     T02Uni        dAr5
145200091209     c                   EndIf
145300170125     C*
145400170125     C* CARICO TABELLA DECOFIIMG PER le filiali con New IMA
145500170125     c                   clear                   AllNewIMA         1
145600170125     c                   clear                   trulvpods
145700170125     c                   eval      ivpoke1 = 'DECOFIIPG'
145800170125     c                   call      'TRULVPOR'
145900170125     c                   parm                    trulvpods
146000170125     c                   if         %lookup('999':skFilOk)>0
146100170125     C                   EVAL       AllNewIMA='S'
146200170125     c                   endif
146300170125
146400911014     C                   ENDSR
146500941206     C**************************************************************************
146600941206     C* CREO ANOMALIA
146700941206     C**************************************************************************
146800911016     C     CRTANM        BEGSR
146900000320     C                   CLEAR                   WCCH
147000970521     C*
147100970521     C                   CLEAR                   WANM10
147200941206     C*
147300970204    1C     COMCAA        IFNE      645
147400990518     C     KANM          CHAIN     FNANM000                           96
147500970204   X1C                   ELSE
147600961205     C*
147700990518     C     KANM          CHAIN(N)  FNANM000                           96
147800961205     C* PER L'ANOMALIA 645 LEGGO FINTANTO CHE NON TROVO UNA ANOMALIA
147900961205     C*  CON DATA = ALLA DATA SPUNTA: SE C'E' NON CREO
148000970204    2C     *IN96         DOWEQ     *OFF
148100990518     C     ANMDAO        ANDNE     BARDFV
148200990518     C     KANM          READE(N)  FNANM000                               96
148300970204    2C                   ENDDO
148400961205     C*
148500970204    1C                   ENDIF
148600000811     C*
148700000811    1C     *IN96         IFEQ      *ON
148800000811     C*
148900000811     C     COMCAA        ORNE      645
149000000811     C     ANMAIE        ANDEQ     'I'
149100000811     C* PER ORA SOLO SULLE INTERNE RICREO ANOMLIA SE PRESENTE
149200970204     C*  - SE CHIUSE SEMPRE
149300040923     C*  - SE APERTA SOLO LA DATA E' < DELLA ESISTENTE
149400040923     c* Per la 200 la ricreo solo se e' chiusa senza guardare la data creaz
149500961205     C*
149600970204     C                   MOVEL     'S'           WWRITE
149700970204    2C     *IN96         IFEQ      *OFF
149800970204     C     ANMDCH        ANDEQ     0
149900970204     C     ANMDAO        ANDGE     BARDFV
150000041005    2C     *IN96         oreq      *OFF
150100041005     C     ANMDCH        andeq     0
150200040923     C     comcaa        ANDeq     200
150300970204     C                   MOVEL     ' '           WWRITE            1
150400970204    2C                   ENDIF
150500970204     C**
150600070215    2C     WWRITE        IFEQ      'S'
150700990518     C                   CLEAR                   FNANM000
150800950927     C                   EXSR      RIEANM
150900000614     C**
151000001109     C* CREO SOLO SE ANOMALIA :
151100001109     C* CREABILE DA QUALUNQUE CATEGORIA SPUNTA
151200070215    3C     §7CCCG        ifeq      'S'
151300001109     C* NON CREABILE DA CATEG. GENERICA E LA CATEGORIA NON È GENERICA
151400070215     C     BARSPG        orne      'I'
151500950927     C*
151600950927     C                   MOVE      COMLNP        ANMLNP
151700990518     C                   Z-ADD     COMAAS        ANMAAS
151800950927     C                   MOVE      COMNSP        ANMNSP
151900960524     C                   MOVE      BARDFV        ANMDAO
152000990518     C                   Z-ADD     BARNFV        ANMNFV
152100950927     C                   MOVEL     BARFGS        ANMCDU
152200040621     c* imposto se spedizione di valore
152300070215    4c                   if        anmnsp>0
152400091209     c     kar5          chain(n)  fiar501l
152500070215    5c                   if        %found(fiar501l)
152600040621     c                   movel     ar5uni        dar5gen
152700040621     c                   else
152800040621     c                   clear                   dar5gen
152900070215    5c                   endif
153000040621     c                   movel     §ar5bva       anmft4
153100070215    4c                   endif
153200110103     c*
153300040429     c* 22/4/4 --> al posto di anmfgs imposto BPES il p.o. che spara
153400040429     c*            che solo per le spunte partenza è sempre barfgs
153500040429     C                   MOVEL     bpes          ANMFLE
153600070215    4C     ANMCAA        IFEQ      10
153700150205    4C     ANMCAA        oreq      410
153800070215     C                   z-add     BARZNC        ANMfl4
153900070215    4C                   ENDIF
154000911112     C*
154100921020     C* SE ESTERNA SCRIVO FIL A CUI TRASMETTERE
154200070215    4C     §7CAIE        IFEQ      'E'
154300070215    5C     COMLNP        IFGT      0
154400960528     C                   MOVEL     COMLNP        ANMFL1
154500960528     C                   ELSE
154600960704     C                   MOVEL     LNPB          ANMFL1
154700070215    5C                   ENDIF
154800070215    4C                   ENDIF
154900911016     C*
155000970204     C* SE INTERNA AUTOCHIUSA E C'E' GIA' NON FACCIO NULLA
155100070215    4C     *IN96         IFEQ      *ON
155200970204     C     §7CCHA        ORNE      'S'
155300960430     C* SE LA KEY E' PER SEGNACOLLO ED IL NUMERO DI SEGNACOLLO E' 0
155400960430     C*  NON CREO L'ANOMALIA (ERRORE DA TERMINALINO)
155500070215    5C     §7CKEY        IFNE      'S'
155600960430     C     §7CKEY        OREQ      'S'
155700960430     C     ANMSCN        ANDGT     0
155800990922     C*
155900990922     C* LA DATA I APERTURA ANOMALIA CI DEVE ESSERE , ALTRIMENTI
156000990922     C*  E' MEGLIO NON CREARE L'ANOMALIA
156100070215    6C     ANMDAO        IFGT      0
156200990518     C   96              WRITE     FNANM000
156300990518     C  N96              UPDATE    FNANM000
156400070215    6C                   ENDIF
156500070215    5C                   ENDIF
156600961205     C*
156700970204     C* SE CREO ANOAMLIA 145 O 146 CHIUDO LE ANOMALIE INTERNE 140 E 130
156800070215    5C     ANMCAA        IFEQ      145
156900970204     C     ANMCAA        OREQ      146
157000970204     C                   Z-ADD     130           COMCAA
157100970204     C                   EXSR      CHIUAN
157200970204     C                   Z-ADD     140           COMCAA
157300970204     C                   EXSR      CHIUAN
157400070215    5C                   ENDIF
157500070215    4C                   ENDIF
157600970204     C*
157700070215    3C                   ENDIF
157800070215    2C                   ENDIF
157900970521   X1C                   ELSE
158000110103     c*
158100970521     C* OPERAZIONI PER ANOMALIA 10 GIA' ESISTENTE MA CHIUSA:
158200970521    2C     COMCAA        IFEQ      10
158300970522     C* Se anomalia 10 a liv massimo --> memorizzo per creaz.anom.410
158400970522    3C     ANMLID        IFEQ      §7ALCI
158500970521     C                   MOVE      '1'           WANM10            1
158600970522    3C                   END
158700970522     C* Se anomalia chiusa (non pratica IDD):
158800970522     C* 1) sflaggo e chiudo l'anomalia mettendo cch='PR' se è a liv max
158900970522    3C     ANMDCH        IFGT      *ZEROS
159000970522     C     ANMCCH        ANDNE     'PR'
159100970522     C*
159200970522    4C                   SELECT
159300970522     C     ANMLID        WHENEQ    §7ALCI
159400970522     C     ANMCCH        ANDEQ     'AN'
159500970522     C                   MOVEL     'PR'          ANMCCH
159600970522     C                   MOVEL     'D'           ANMFSC
159700970522     C                   CLEAR                   ANMFT1
159800970522     C                   CLEAR                   ANMFT2
159900970522     C     ANMLID        WHENLT    §7ALCI
160000970523     C                   CLEAR                   ANMCCH
160100970522     C                   CLEAR                   ANMDCH
160200970522     C                   CLEAR                   ANMFSC
160300970522    4C                   ENDSL
160400990518     C                   UPDATE    FNANM000
160500970522    3C                   ENDIF
160600970522    2C                   ENDIF
160700970204    1C                   ENDIF
160800970204     C*
160900990518     C                   UNLOCK    FNANM02L
161000911016     C                   ENDSR
161100960613     C**************************************************************************
161200960613     C*    CONTROLLO IL RAGGRUPPAMENTO DELLA PISTOLA
161300960613     C**************************************************************************
161400960613     C     PISMAN        BEGSR
161500960613     C*
161600961213     C                   Z-ADD     1             Y                 3 0
161700961213     C*
161800960613    1C     *IN02         IFEQ      *OFF
161900960613     C     BARNPS        LOOKUP    NPS(Y)                                 34
162000960613     C  N34              Z-ADD     1             Y
162100960613     C*
162200960613   X1C                   ELSE
162300960613     C*
162400960613     C                   MOVEL     '7J'          COD
162500960613     C                   MOVEL(P)  BARNPS        KEY
162600960613     C     KTAB          CHAIN     TABEL                              34
162700000316     C     *IN34         IFEQ      *OFF
162800000316     C                   MOVEL     TBLUNI        DS7J
162900000316     C                   MOVEL     §7JRPS        RPS(Y)
163000000316     C                   MOVEL     §7JORD        OPS(Y)
163100000316     C                   MOVEL     §7JTPS        TPS(Y)
163200000316     C                   MOVEL     §7JINV        INV(Y)
163300040503     C                   MOVEL     §7Jfsl        fsl(Y)
163400000316     C                   ELSE
163500000316     C                   MOVEL     'M'           RPS(Y)
163600000316     C                   MOVEL     999           OPS(Y)
163700000316     C                   MOVEL     'B'           TPS(Y)
163800000316     C                   MOVEL     ' '           INV(Y)
163900040503     C                   MOVEL     ' '           fsl(Y)
164000960613    1C                   END
164100000316    1C                   END
164200960613     C                   ENDSR
164300941206     C**************************************************************************
164400020213     C*    A G G I O R N O    R E C O R D    B O L L E
164500941206     C**************************************************************************
164600980212     C     UPDARB        BEGSR
164700960521     C                   CLEAR                   WNOTRA
164800961210     C                   CLEAR                   W60X
164900961211     C                   CLEAR                   WESAN
165000961210     C                   MOVEL     'S'           W620
165100961211     C                   MOVEL     'S'           WCREA
165200021106     C                   CLEAR                   WDCM
165300021106     C                   CLEAR                   WBOAR
165400021106     C                   CLEAR                   WATDCM
165500040430     C                   CLEAR                   WABCCA
165600040430     C                   CLEAR                   WABNDC
165700161007     C                   CLEAR                   gcpdgc            8 0
165800021106     C                   MOVEL     ' '           WGIAC             1
165900941206     C*
166000921229     C* TROVO TIPO E RAGGRUPPAMENTO PISTOLE DI QUELLA CARICATA
166100960613     C                   EXSR      PISMAN
166200921229     C*
166300960523     C* CARICO LA L6 DELLA FILIALE GESTIONE
166400020213     C     BPES          IFNE      SAVPES
166500020213     C                   MOVEL     BPES          SAVPES
166600961209     C                   EXSR      CARL6
166700960523     C                   ENDIF
166800921013     C*
166900041012     C                   SETOFF                                         0332
167000950922     C                   CLEAR                   COMLNP
167100950922     C                   CLEAR                   COMAAS
167200950922     C                   CLEAR                   COMNSP
167300950922     C                   CLEAR                   WSPLOC
167400950927     C                   CLEAR                   WTIBOL
167500970423     C                   CLEAR                   WWFVC
167600020124     C                   CLEAR                   LNPB
167700020124     C                   CLEAR                   WTAB5F
167800020131     C                   CLEAR                   WESART
167900020131     C                   CLEAR                   WESBLT
168000020131     C                   CLEAR                   WESBTT
168100130228     c                   clear                   bolxco
168200160421     c                   clear                   bolznc
168300021022     c*
168400021125     C* TERMINAL partenza DI BPES
168500021125     C                   MOVEL     'P'           WTITER
168600021022     C                   MOVEL     BARDFV        WDTRIF
168700021022     C                   EXSR      TERPES
168800021022     C                   Z-ADD     D55TFP        PESTFP
168900021203     c*
169000040422     c* Se trovo la bolla transito x p.o. spunta --> aggiorno transito, senz
169100021203     c*  controllo i terminal (c'e' un problema a livello di eccezioni ter
169200021203     C     KBAR37        CHAIN     FNBTT37L                           3739
169300021203     C   39              GOTO      ENDUPD
169400021203     C  N37KBTT          CHAIN     FNBTP11L                           37
169500021203     C**
169600021203    1C     *IN37         IFEQ      *OFF
169700021203     C                   MOVEL     'S'           WESBTT            1
169800021203     C                   MOVEL     BTTLNP        LNPB
169900021203     C                   MOVEL     BTPDSP        DSPB
170000021203     C                   MOVEL     BTPDSP        WDTRIF
170100021203     C                   MOVEL     BTPTFP        LNPTFP
170200050603     C                   MOVEL     BTPTFa        bolTFa
170300040621     C                   MOVE      BTTLNP        COMLNP
170400040621     C                   MOVE      BTTAAS        COMAAS
170500040621     C                   MOVE      BTTNSP        COMNSP
170600021203   X1C                   ELSE
170700021203     C                   MOVEL     'N'           WESBTT
170800020124     C**
170900020124     C** VEDO SE TROVO LA BOLLA IN BLT
171000020124     C     KBAR          CHAIN     FNBLT27L                           3239
171100020213     C   39              GOTO      ENDUPD
171200020213     C  N32KBLT          CHAIN(N)  FNBLP01L                           32
171300020124     C**
171400021203    2C     *IN32         IFEQ      *OFF
171500020131     C                   MOVEL     'S'           WESBLT            1
171600020124     C                   MOVEL     BLTLNP        LNPB
171700020201     C                   MOVEL     BLPTFP        LNPTFP
171800050603     C                   MOVEL     BLPTFa        bolTFa
171900020201     C                   MOVEL     BLPDSP        WDTRIF
172000020214     C                   MOVEL     BLPDSP        DSPB
172100130301     c                   movel     BLPxco        bolxco            1
172200160421     c                   movel     BLPznc        bolznc            2
172300040621     C                   MOVE      BLTLNP        COMLNP
172400040621     C                   MOVE      BLTAAS        COMAAS
172500040621     C                   MOVE      BLTNSP        COMNSP
172600021203   X2C                   ELSE
172700020213     C                   MOVEL     'N'           WESBLT
172800020124     C** SE NON TROVO LA BOLLA PARTENZA CERCO NEGLI ARRIVI
172900020124     C     KBAR          CHAIN     FNART27L                           3239
173000020213     C   39              GOTO      ENDUPD
173100050223     C  N32KARB          CHAIN(N)  FNARB01L                           32
173200020124     C**
173300021203    3C     *IN32         IFEQ      *OFF
173400020201     C                   MOVEL     'S'           WESART            1
173500020124     C                   MOVEL     ARTLNP        LNPB
173600020214     C                   MOVEL     ARBDSP        DSPB
173700020201     C                   MOVEL     ARBDSP        WDTRIF
173800020201     C                   MOVEL     ARBTFP        LNPTFP
173900050603     C                   MOVEL     ARBTFa        boltfa
174000130301     c                   movel     ARBxco        bolxco            1
174100160421     c                   movel     ARBznc        bolznc
174200040621     C                   MOVE      ARTLNP        COMLNP
174300040621     C                   MOVE      ARTAAS        COMAAS
174400040621     C                   MOVE      ARTNSP        COMNSP
174500021203   X3C                   ELSE
174600020213     C                   MOVEL     'N'           WESART
174700020131     C** IMPOSTO LNPB = BARLNP
174800020131     C                   MOVEL     BARLNP        LNPB
174900020214     C                   Z-ADD     BARDFV        DSPB
175000050603     c                   clear                   boltfa
175100020124     C**
175200020124     C** SE NON TROVATA MAI LA BOLLA, CERCO SE ESISTE LA EVENTUALE
175300020124     C*   SPUNTA PARTENZA E LA TABELLA 5F
175400020205    4C     BARNRS        IFGT      *ZEROS
175500020124     C                   EXSR      RICLNP
175600020213    4C                   ENDIF
175700021105     c* prendo terminal di partenza della linea partenza calcolata
175800070222     c                   if        bardfv=0
175900021105     C                   Z-ADD     BARDFV        WDTRIF
176000070222     c                   else
176100070222     c                   z-add     wdate1        wdtrif
176200070222     c                   endif
176300070222     c
176400021105     C                   EXSR      TERPAR
176500020213    3C                   ENDIF
176600020213    2C                   ENDIF
176700020213    1C                   ENDIF
176800020204     C**
176900020215     C** CONTROLLO TRA P.O. SPUNTA E LINEE SPUNTE CHE AGGIONAMENTI
177000020213     C**  POSSO FARE
177100021125     c*
177200050215     C* TERMINAL arrivo DI BPES
177300021125     C                   MOVEL     'A'           WTITER
177400021125     C                   MOVEL     BARDFV        WDTRIF
177500021125     C                   EXSR      TERPES
177600021125     C                   Z-ADD     D55TFA        PESTFA
177700021125     c
177800020204     C                   EXSR      CTRAGG
177900020213     C**
178000020213     C** A G G I O R N A M E N T O   B O L L E   P A R T E N Z A
178100020213    1C     WAGPAR        IFEQ      'S'
178200021203    2C     WESblt        IFEQ      ' '
178300021203     C     KBAR          CHAIN     FNBLT27L                           3239
178400021203     C   39              GOTO      ENDUPD
178500021203     C  N32              MOVEL     'S'           WESBLT            1
178600021203     C   32              MOVEL     'N'           WESBLT            1
178700021203    2C                   ENDIF
178800021203     c**
178900021203    2C     WESBLT        ifeq      'S'
179000150817     c* Aggiorno solo se la bolla NON parte da + filiali
179100150817     c* oppure sel la bolla per LINEA parte da + filiali ma è mia bolla
179200150817     c* oppure se chi spara è la linea partenza della bolla che parte da + fil
179300150622     c                   if        wbsptp=' ' or wbsptp='L'
179400150622     c                             or bpes=lnpb
179500020213     C                   EXSR      AGGBLT
179600150622     c                   else
179700150622     c                   clear                   wagPAR
179800150622     c                   endif
179900150622     c
180000021203    3C     WEND          IFEQ      '1'
180100020213     C                   GOTO      ENDUPD
180200021203    3C                   ENDIF
180300021203    2C                   ENDIF
180400020213     C*
180500021203    2C     WESBLT        ifeq      'N'
180600020204     C                   MOVEL     'N'           WBOPAR            1
180700020204     C                   MOVEL     'P'           WTIBOL            1
180800021203    2C                   ENDIF
180900021203    1C                   ENDIF
181000020213     C*
181100020213    1C     WAGPAR        IFNE      'S'
181200020213     C     WESBLT        ANDEQ     'S'
181300020213     C                   UNLOCK    FNBLT27L
181400020213    1C                   ENDIF
181500020213     C**
181600020213     C* A G G I O R N O   B O L L E   A R R I V I
181700020213     C* A PRESCINDERE DAL FATTO CHE TROVI O MENO LA BOLLA
181800020213    1C     WAGARR        IFEQ      'S'
181900020220     C* LEGGO LE BOLLE ARRIVI ANCHE PER FLAG=6.
182000020220     C*  SI TRATTA DI SPUNTA DI DISGUIDO PER CUI NON AGGIORNEREBBE
182100020220     C*  LA BOLLA ARRIVI MA MI SERVE PER ENTRARE IN CTRA60
182200070221    1C     dls45FLG      OREQ      '6'
182300020213     C* SE NON HO ANCORA CHAINATO IL DETTAGLIO COLLI LO FACCIO ORA
182400020213    2C     WESART        IFEQ      ' '
182500020213     C     KBAR          CHAIN     FNART27L                           3239
182600020218     C   39              GOTO      ENDUPD
182700020213     C  N32              MOVEL     'S'           WESART            1
182800020214     C   32              MOVEL     'N'           WESART            1
182900020213    2C                   ENDIF
183000020213     C*
183100020213     C                   EXSR      AGGART
183200020213    1C                   ENDIF
183300020213     C*
183400020213    1C     WAGARR        IFNE      'S'
183500020213     C     WESART        ANDEQ     'S'
183600020213     C                   UNLOCK    FNART27L
183700020213    1C                   ENDIF
183800040428     c*
183900040428     c*Chiudo eventuale anomalia di disguido presente sul collo, se
184000040428     c*  di area <> dal'anomalia
184100040503     C* SOLO SE SPUNTA DI PISTOLA REALE
184200040503     c                   if        fsl(y)<>'F'
184300040428     c                   eval      comcaa=200
184400040428     C*
184500040503     C     Kanm          chain(N)  FNANM000
184600040428     c                   if        %found(fnanm02l)
184700040428     C                   CLEAR                   DSLV55
184800040428     C                   MOVEL     'P'           D55TPT
184900040428     C                   Z-ADD     anmfle        D55LIN
185000040428     C                   Z-ADD     bardfv        D55DRF
185100040428     C                   EXSR      FNLV55
185200040428     c                   if        pestfp<>d55tfp  or
185300040428     c                             bpes=anmlna
185400040428     c                   exsr      chiuan
185500040428     c                   endif
185600040428     c                   endif
185700040503     c                   endif
185800921013     C**
185900960531     C** T R A N S I T I  /  D I S G U I D I   I N   A R E A   E  N O N
186000921013     C**
186100070221    0C     dls45FLG      IFNE      '6'
186200911112     C*
186300960522     C* NON E' UNA SPUNTA DEL TERMINAL DI ARRIVO
186400050603    1C     Usalnatfa     IFNE      BPES
186500021022     c     wesbtt        oreq      'S'
186600960522     C*
186700960522     C* SE NEMMENO I 2 TERMINAL DI ARRIVO CORRISPONDONO
186800960522     C* --> T R A N S I T O
186900050603    2C     Usalnatfa     IFNE      PESTFA
187000021022     c     wesbtt        oreq      'S'
187100960531     C**
187200020214     C** I TRANSITI SONO ANCORA DA SISTEMARE
187300020214     C**  QUESTE 2 SPECIFICHE SONO DA ELIMINARE: INFATTI LA BOLLA POTRA
187400020214     C**  ESSERE SIA IN BLT CHE IN BTT
187500020205    3C     WAGPAR        IFEQ      ' '
187600960703     C     WBOPAR        OREQ      'N'
187700960522     C**
187800040422     C     WESBTT        ifeq      'N'
187900960521     C**
188000960612     C* SE E' SPUNTA ENTRATA O PARTENZA E NON HO TROVATO NE' LA BOLLA
188100960521     C*  PARTENZA NE' LA BOLLA TRANSITO, PER SERIE =0
188200960521     C*  IMPOSTO NELLA SPUNTA FLAG TRASMISSIONE "N" E 99999999
188300960612     C*  PER NON AUTOGENERARE LA SPUNTA PARTENZA O NON TRASMETTERLA
188400001123     C* IDEM PER SPUNTA IMI (PER BOLLE POSTE AUTOGENERA ANCHE DA SPUNTE
188500001123     C* IMI)
188600960522    5C     BARNPG        IFEQ      5
188700020205     C     BARNPG        OREQ      1
188800001123     C     BARNPG        OREQ      3
188900001123     C     BARSPG        ANDEQ     'I'
189000020205    6C     BARNRS        IFEQ      0
189100020205     C     WAGPAR        ANDEQ     'S'
189200960521     C     WBOPAR        ANDEQ     'N'
189300960521     C                   MOVEL     'S'           WNOTRA            1
189400020205    6C                   ENDIF
189500960612    5C                   ENDIF
189600040120     c
189700911016     C*
189800110124     C* SE NON E' BOLLA LOCALE e nemmeno in £6 : DISGUIDO
189900150817     c*  non è disguido nemmeno se collo che parte da altra filiale
190000150817     c*  ( la bolla deve essere spostata di lnp)
190100020205    5C     WAGPAR        IFEQ      ' '
190200110124    5C     WAGarr        andeq     ' '
190300150817    5C     WBSPSPU       andeq     ' '
190400150817     c
190500020215     C     BARFLE        IFEQ      BTFP
190600960522     C                   Z-ADD     200           COMCAA
190700960404     C  N05              EXSR      CRTANM
190800020215     C                   ENDIF
190900020215     C* DEVO COMUNQUE DIRE CHE SI TRATTA DI UN DISGUIDO
191000960404     C                   MOVEL     'D'           WTIBOL
191100041012     c
191200041012     c* Aggiorno anche la bolla in  arrivo
191300041012     c                   EXSR      AGGARRIVO
191400960522    5C                   ENDIF
191500960522     C**
191600911016     C* 37 OFF - TROVATA AGGIORNO BOLLA TRANSITO
191700960522   X4C                   ELSE
19180002021418   C                   EXSR      AGGBTT
191900960522    4C                   ENDIF
192000040430     c*
192100040430     c* Eventuale spunta di disguido o transito, chiude fuori posto
192200040430     c*  di bolla partenza
192300040430     C                   Z-ADD     120           COMCAA
192400040430     C                   EXSR      CHIUAN
192500970204     C*
192600970204   X3C                   ELSE
192700970204     C* SE NON E' COLLO DELLA AREA DI ARRIVO E COLLO IN PARTENZA
192800970204     C*  VEDO SE CREARE L'ANOMALIA FUORI POSTO BOLLA IN PARTENZA
192900970204     C                   EXSR      ANM120
193000990120     C** VERIFICO LA PRESENZA DI UNA C.A. PER CREARE EVENTUALI ANOM
193100000523     C** IGNORO SE E' SPUNTA CAT 4-8 SOLO IN ARRIVO(IN PART.SEMPRE)
193200990121     C                   MOVEL     'S'           WCAT4             1
193300990126     C*     GESTISCO IL RITROVAMENTO COLLO
193400990126     C                   MOVEL     'S'           WRITRO            1
193500990126     C*     GESTISCO LA CREAZIONE ANOMALIE
193600990126     C                   MOVEL     'S'           WCREAN            1
193700990120     C                   EXSR      CTRDAN
193800990120     C**
193900990120     C** SE NON TROVATA CA DI COLLO
194000990120     C* VERIFICO SE LA SPEDIZIONE E' SU UNA CA CHIUSA --> CREO
194100990120    4C     WTRDAN        IFEQ      ' '
194200040811      *
194300040811      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
194400040811      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
194500040811      * legate alla spedizione che passo
194600040811     c                   clear                   fidn12ds
194700040811     c                   eval      i12aas = BLTAAS
194800040811     c                   eval      i12lnp = BLTLNP
194900040811     c                   eval      i12nrs = BLTNRS
195000040811     c                   eval      i12nsp = BLTNSP
195100040811     c                   eval      i12fel = 'E'
195200040811     c*
195300040811     c                   call      'FIDN12R'
195400040811     c                   parm                    fidn12ds
195500040811     c*
195600040811     c                   setoff                                       33
195700040811     c*
195800040811     c* se non ci sono errori
195900040811     c                   if        o12err <> ' '
196000040811     c                   seton                                        33
196100040811     c                   else
196200040811     c* se numero di CA trovate maggiore di zero
196300040811     c                   if        o12nca = 0
196400040811     c                   seton                                        33
196500040811     c                   else
196600040811     c                   z-add     1             jj                2 0
196700040811     c                   dow       jj <= o12nca and *in33 = *off
196800040811     C     skDch(jj)     IFGT      0
196900040811     C*
197000040811     C* Valorizzo i campi DCT* utilizzati nella routine CRE151 con quelli
197100040811     C* ritornati dal *pgm d utilità FIDN12R
197200040811     C                   movel     skCch(jj)     DCTCCH
197300040811     C*
197400040811     C                   EXSR      CRE151
197500040811     C                   SETON                                        33
197600040811     C                   ENDIF
197700040811     c*
197800040811     c                   add       1             jj
197900040811     c                   enddo
198000040811     c*
198100040811     c                   seton                                        33
198200040811     c*
198300040811     c                   endif
198400040811     c                   endif
198500040811     C**
198600990120    4C                   ENDIF
198700990120     C**
198800960531    3C                   ENDIF
198900960522     C*
199000960531   X2C                   ELSE
199100970203     C*  D I S G U I D O   I N   A R E A
199200970203     C*  (SE NON E' UNA MIA BOLLA PARTENZA)
199300961021     C                   SETOFF                                       88
199400961122     C                   CLEAR                   WSTESS
199500961028     C*
199600020213    3C  N05BPES          IFNE      BARLNA
199700050930     c* al posto di BPES uso barfgs per le spunte IMP: mi fa più comodo
199800050930     c*  usare direttamente il terminal di partenza di chi spara perchè
199900050930     c*  l'IMP lo fa solo il terminal di partenza ma può essere usato
200000050930     c*  anche dai 2 livello
200100050930     C     barfgs        ANDNE     LNPB
200200960522     C     BARNPG        ANDGT     1
200300050930     C     BARNPG        ANDne     5
200400050930     c* Se spunta entrata escludo se di tratta di un aggiornamento
200500050930     c*  in partenza e non in arrivo (per cui il p.o. fa parte
200600050930     c*  dell'area di partenza
200700050930    3C     BPES          orne      BARLNA
200800050930     C     barfgs        ANDNE     LNPB
200900050930     C     BARNPG        ANDeq     5
201000050930     c     wagarr        andeq     'S'
201100050930     c
201200980727     C** CREO ANOMALIE SE COLLO SPUNTATO IN IMA ANCHE SE DA ME CONFERMA
201300020213    3C     BPES          ORNE      BARLNA
201400020213     C     BPES          ANDEQ     LNPB
201500980727     C     BARNPG        ANDEQ     3
201600980727     C     BARSPG        ANDEQ     'A'
201700960523     C*
201800020213     C* NON DEVE ESSERE PRESENTE NEMMENO IN L6 DEL P.O. SPUNTA
201900960523     C     BARLNA        LOOKUP    L6                                     88
202000980127     C**
202100960531    4C     *IN88         IFEQ      *OFF
202200961202     C                   Z-ADD     146           CAACRE
202300961202     C                   Z-ADD     145           CAACIU
202400961202     C                   EXSR      ANM14X
202500960531    4C                   ENDIF
202600960531    3C                   ENDIF
202700960522     C*
202800960523     C* ALTRIMENTI  LA CHIUDO
202900960531    3C     *IN88         IFEQ      *ON
203000960523     C     BARNPG        OREQ      1
203100020213     C     BPES          OREQ      BARLNA
203200961122     C*
203300000523     C* SE CHI SPARA E' LA FILIALE FINALE DI ARRIVO, ME LO MEMORIZZO
203400961122     C*  MI SERVE PER LA CHIUSURA
203500961122    4C     *IN88         IFEQ      *ON
203600020213     C     BPES          OREQ      BARLNA
203700961122     C                   MOVEL     'S'           WSTESS
203800961122    4C                   ENDIF
203900961122     C**
204000960522     C                   Z-ADD     146           COMCAA
204100960522     C                   EXSR      CHIUAN
204200961122     C*
204300960522     C                   Z-ADD     145           COMCAA
204400960522     C                   EXSR      CHIUAN
204500970204     C*
204600970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
204700970204     C                   EXSR      ANMARR
204800960531    3C                   ENDIF
204900961021     C*
205000970203     C* CREO E/O CHIUDO LE ANOMALIE CHE RIGUARDANO L'AREA DI ARRIVO
205100970203     C                   EXSR      ANMSEM
205200960531    2C                   ENDIF
205300911016     C*
205400960531   X1C                   ELSE
205500970203     C* T E R M I N A L   D I   A R R I V O
205600961122     C                   SETOFF                                       88
205700961122     C                   CLEAR                   WSTESS
205800970203     C*
205900970203     C* CREO E/O CHIUDO LE ANOMALIE CHE RIGUARDANO L'AREA DI ARRIVO
206000970203     C                   EXSR      ANMSEM
206100970204     C*
206200970204     C* CHIUDO EVENTUALE ANOMALIA BOLLA ARRIVI
206300970204    2C     BARNPG        IFEQ      2
206400970204     C                   Z-ADD     130           COMCAA
206500970204     C                   EXSR      CHIUAN
206600970204    2C                   ENDIF
206700940330     C*
206800940330     C* SONO TERMINAL DI ARRIVO-ANOMALIA:INOLTRARE COLLO ALL'ARRIVO
206900960531    2C     WTIBOL        IFNE      'A'
207000020218    2C     WTIBOL        ANDNE     'L'
207100960522     C                   MOVEL     'R'           WTIBOL
207200960531    2C                   ENDIF
207300960522     C*
207400960522     C* SOLO SE NON E' UN MIO COLLO
207500020213    2C  N05BPES          IFNE      BARLNA
207600091211     c
207700091211     C* CARICO LA L6 DELLA FILIALE GESTIONE
207800091211     C     BPES          IFNE      SAVPES
207900091211     C                   MOVEL     BPES          SAVPES
208000091211     C                   EXSR      CARL6
208100091211     C                   ENDIF
208200091211     c
208300960523     C* NON DEVE ESSERE PRESENTE NEMMENO IN L6
208400960531     C     BARLNA        LOOKUP    L6                                     88
208500960531    3C     *IN88         IFEQ      *OFF
208600960523     C*
208700960531    4C     BARNPG        IFGT      2
208800960522     C     BARNPG        ANDLT     5
208900000523     C     BARNPG        OREQ      8
209000961202     C                   Z-ADD     145           CAACRE
209100961202     C                   Z-ADD     146           CAACIU
209200961202     C                   EXSR      ANM14X
209300960522     C*
209400960531   X4C                   ELSE
209500961211     C* SE TROVO LA 145 E LA POSSO CHIUDERE CREO SE POSSO LA 620
209600961211     C*  SE NON TROVO LA 145 DEMANDO L'EVENTUALE CREAZIONE DELLA 620
209700961211     C*  ALLA CHIUSURA DELLA EVENTUALE 146, CHE POTREBBE ANCHE NON
209800961211     C*  ESSERCI. SIA CHE CI SIA DA CHIUDERE CHE NON CI SIA, A QUESTO
209900961211     C*  PUNTO LA 620 LA CREO
210000960522     C                   Z-ADD     145           COMCAA
210100961211     C* SOLO SE ESISTE L'ANOMALIA CREO LA 620
210200961211     C                   MOVEL     'S'           WESAN             1
210300961211     C*
210400960522     C                   EXSR      CHIUAN
210500961211     C                   CLEAR                   WESAN
210600961029     C* SE C'E' CHIUDO ANCHE IL DISGUIDO IN AREA
210700961029     C                   Z-ADD     146           COMCAA
210800961211     C*
210900961211     C   97              MOVEL     'S'           W620
211000961211     C  N97              MOVEL     'N'           W620
211100961029     C                   EXSR      CHIUAN
211200961211     C                   MOVEL     'S'           W620
211300960523    4C                   ENDIF
211400961021   X3C                   ELSE
211500961122     C*
211600020213     C* SE CHI SPARA E' LA FILIALE FINALE DI ARRIVO, ME LO MEMORIZZO
211700961122     C*  MI SERVE PER LA CHIUSURA
211800961122     C                   MOVEL     'S'           WSTESS            1
211900961021     C                   Z-ADD     146           COMCAA
212000961021     C                   EXSR      CHIUAN
212100970204     C*
212200970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
212300970204     C                   EXSR      ANMARR
212400960522    3C                   ENDIF
212500961122     C**
212600961021   X2C                   ELSE
212700970204     C* CHI SPARA E' LA FILIALE FINALE DI ARRIVO
212800961122     C                   MOVEL     'S'           WSTESS
212900961021     C                   Z-ADD     146           COMCAA
213000961021     C                   EXSR      CHIUAN
213100970204     C*
213200970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
213300970204     C                   EXSR      ANMARR
213400960523    2C                   ENDIF
213500911016     C*
213600950922    1C                   END
213700961213    0C                   ENDIF
213800021022     c
213900021203     c                   unlock    fnbtt37l
214000970203     C**
214100970203     C** SE FLAG = 2 MODIFICO ANOMALIE METTENDO NUOVO NUMERO DI FOGLIO
214200970203     C**
214300970203     C     *IN05         IFEQ      *ON
214400970203     C*
214500921021     C                   EXSR      RICFAS
214600921021     C*
214700990518     C     KBAR          SETLL     FNANM000
214800990518     C     KBAR          READE     FNANM000                               99
214900921021     C*
215000960423    2C     *IN99         DOWEQ     '0'
215100070208    3C     ANMNFV        IFEQ      DLS45NFVO
215200000516     C* SE NON C'E' SOTTO CATEGORIA DEVONO ESSERE UGUALI
215300000516    4C     BARSPG        IFEQ      ' '
215400921021     C     ANMFSA        ANDEQ     COMFAS
215500000516     C* SE     C'E' SOTTO CATEGORIA, DEVE ESSERE UGUALE AD UNA DELLE
215600000516     C*  TANTE
215700000516     C     ANMFSA        OREQ      WFAS1
215800000516     C     ANMFSA        OREQ      WFAS2
215900000516     C     ANMFSA        OREQ      WFAS3
216000000516     C     ANMFSA        OREQ      WFAS4
216100000516     C     ANMFSA        OREQ      WFAS5
216200921021     C                   Z-ADD     BARNFV        ANMNFV
216300960524     C                   Z-ADD     BARDFV        ANMDAO
216400000516     C                   MOVEL     COMFAS        ANMFSA
216500990518     C                   UPDATE    FNANM000
216600000516    4C                   ENDIF
216700000516    3C                   ENDIF
216800921021     C*
216900990518     C     KBAR          READE     FNANM000                               99
217000000516    2C                   ENDDO
217100920424     C*
217200920424     C                   Z-ADD     3             B
217300021107    1C                   END
217400920424     C     ENDUPD        TAG
217500000209     C*
217600110907    1C     *IN39         IFEQ      *ON
217700000209     C                   SETON                                        40
217800110907   x1C                   ELSE
217900000209     C*
218000000210     C* ELABORO SOLO SPUNTA DA MACCHINONE
218100110907     c*  non lo faccio se modifica di numero foglio da video
218200110907    2c  N05RPS(Y)        ifne      'A'
218300000209     C* AGGIORNO FILE  FISGN SE C'E'
218400000209     C     KBAR          CHAIN     FISGN03L                           33
218500110907    3C     *IN33         DOWEQ     *OFF
218600110907    4C     SGNST2        IFLT      9
218700000209     C                   ADD       1             SGNST2
218800001228     C                   CLEAR                   SGNT6A
218900001228     C                   CLEAR                   SGNT6B
219000001228     C                   CLEAR                   SGNT6C
219100001228     C                   CLEAR                   SGNT6D
219200001228     C                   CLEAR                   SGNT6E
219300001228     C                   CLEAR                   SGNT6F
219400080624     c                   eval      sgndatora = %char(%timestamp:*iso0)
219500000209     C                   UPDATE    FISGN000
219600110907    4C                   ENDIF
219700001020     C     KBAR          READE     FISGN03L                               33
219800110907    3C                   ENDDO
219900110907     c
220000110907     c* Una qualsiasi spunta REALE chiude eventuale anomalia interna
220100110907     c*  115 - COLLO MAI AFFIDATO
220200110929     C                   MOVEL     'AR'          SAVCCH
220300110907     C                   Z-ADD     115           COMCAA
220400110907     C                   EXSR      CHIUAN
220500110907     c
220600110907     c
220700110907    2C                   ENDIF
220800110907    1C                   ENDIF
220900941206     C*
221000020215     C                   ENDSR
221100961118     C**************************************************************************
221200961205     C* CREO ANOMALIA 600 PER L'EXPORT   COLLO A TERRA
221300961118     C**************************************************************************
221400961118     C     ANM600        BEGSR
221500961210     C*
221600961210     C* PER ESTERO CREO ANOMALIA 645 E 600
221700020212    7C     NTWBAR        IFEQ      'EEX'
221800060322    7C     NTWBAR        oreq      'DPD'
221900060322    7C     NTWBAR        oreq      'FED'
222000961210     C                   Z-ADD     645           COMCAA
222100961210     C                   EXSR      CRTANM
222200961210     C*
222300961210    8C     W60X          IFEQ      'S'
222400961209     C* SE ESISTE LA 620  LA DELETO
222500961118     C                   Z-ADD     620           COMCAA
222600001110     C     COMCAA        LOOKUP    ACG                                    34
222700001110     C     BARSPG        IFNE      'I'
222800001110     C     *IN34         OREQ      *ON
222900990518     C     KANM          DELETE    FNANM02L                           34
223000001110     C                   ENDIF
223100961118     C                   Z-ADD     600           COMCAA
223200961118     C                   EXSR      CRTANM
223300961210     C                   CLEAR                   W60X
223400961210    8C                   ENDIF
223500961210    7C                   ENDIF
223600961210     C                   ENDSR
223700960702     C**************************************************************************
223800960702     C* CERCO LNP BOLLA SE COLLO CON SERIE > 0
223900960702     C**************************************************************************
224000960702     C     RICLNP        BEGSR
224100970814     C                   SETOFF                                       33
224200960702     C*
224300020201     C* CONTROLLO SE COLLO CONFERMABILE DAL TERMINAL DEL P.O. EFFETTUA
224400020201     C*  SPUNTA
224500020201     C                   MOVEL     PESTFP        W008A             8
224600960702     C                   MOVE      BARNRS        W005A             5
224700960702     C                   MOVEL     BARLNP        W005A
224800960702     C                   MOVE      W005A         W008A
224900990108     C                   Z-ADD     1             YY                4 0
225000970828     C     W008A         LOOKUP    S5F(YY)                                33
225100020131     C**
225200020131     C* SE NON TROVATO CERCO COME SERIE = 99
225300020131     C     *IN33         IFEQ      *OFF
225400020131     C                   MOVE      99            W008A
225500020131     C                   Z-ADD     1             YY                4 0
225600020131     C     W008A         LOOKUP    S5F(YY)                                33
225700020131     C                   ENDIF
225800020131     C**
225900020131    1C     *IN33         IFEQ      *ON
226000020131     C                   MOVEL     'S'           WTAB5F            1
226100020131     C* CERCO SPUNTA PARTENZA SE C'E' PRENDO LA FGS DI QUEST'ULTIMA
226200020201     C*  E LA DATA DI RIFERIMENTO E' LA DATA DI QUESTA SPUNTA
226300960702     C                   Z-ADD     1             WNPG
226400070131     C     KBAR3         SETLL     FNBRV09L
226500070131     C     KBAR3         READE     FNBRV09L                               33
226600020131    2C     *IN33         DOWEQ     *OFF
226700040303    3C     BR9ATR        IFEQ      *BLANKS
226800040323     c* Escludo le defluenze e le spunte di simfel(potrebbe quindi essere co
226900040323     c*  lo in transito da me e quindi non partito da me)
227000040323    4C     BR9KEY        IFNE      SA1KEY
227100040323     C                   MOVEL     BR9KEY        SA1KEY
227200040323     C                   Z-ADD     BR9NPG        WNPG
227300040323     C                   Z-ADD     BR9NFV        WNFV
227400040323     C                   Z-ADD     BR9FGS        WFGS
227500070906     C                   movel     BR9nps        Wnps
227600040323     C                   EXSR      RILFOG
227700040323     c                   Z-ADD     WDFV          BRVDFV
227800140616     c                   movel     WFCF          BRVFCF
227900140616     c                   movel     Wspg          BRVspg
228000040323    5C     WTTR          IFNE      *BLANKS
228100040323     C     WTTR          LOOKUP    TTR                                    34
228200040323     C                   MOVE      *IN34         W9DEFL            1
228300040323     C                   ELSE
228400040323     C                   MOVE      '0'           W9DEFL            1
228500040323    5C                   ENDIF
228600040323    4C                   ENDIF
228700040323    4c                   if        w9defl='0' and br9fgs<>simfel
228800110103     c*
228900040303     C                   MOVE      BR9PES        LNPB
229000970814     C                   CLEAR                   WTAB5F
229100020214     C                   Z-ADD     BRVDFV        DSPB
229200020214     C                   Z-ADD     BRVDFV        WDTRIF
229300960702     C                   SETON                                        33
229400020201    4C                   END
229500020131    3C                   END
229600070131     C  N33KBAR3         READE     FNBRV09L                               33
229700020131    2C                   ENDDO
229800020204     C**
229900020131    2C     WTAB5F        IFEQ      'S'
230000020218     C* CERCO SE C'E' IL P.O. CHE HA SPUNTATO, ALTRIMENTI IL PRIMO
230100020218     C                   MOVEA     F5F(YY)       WPO
230200020218     C                   Z-ADD     1             ZZ
230300020218     C                   CLEAR                   WTROVA
230400020218    3C                   DO        4             ZZ                3 0
230500020218    4C     WPO(ZZ)       IFGT      *ZEROS
230600020218     C                   MOVEL     WPO(ZZ)       W0030             3 0
230700020218    5C     W0030         IFEQ      BPES
230800020218     C                   MOVEL     W0030         LNPB
230900020218     C                   MOVEL     'S'           WTROVA
231000020218    5C                   ENDIF
231100020218    4C                   ENDIF
231200020218    3C                   ENDDO
231300020218     C**
231400020218     C* SE NON C'E' P.O. = BPES PRENDO IL PRIMO
231500020218     C     WTROVA        IFEQ      ' '
231600020218     C                   MOVEL     WPO(1)        LNPB
231700020218     C                   ENDIF
231800020214     C                   MOVEL     BARDFV        DSPB
231900020214     C                   MOVEL     BARDFV        WDTRIF
232000020131    2C                   ENDIF
232100970814    1C                   ENDIF
232200960702     C                   ENDSR
232300941206     C**************************************************************************
232400941206     C* CERCO IL RAGGRUPPAMENTO PISTOLA
232500941206     C**************************************************************************
232600921229     C     RICRAG        BEGSR
232700921229     C*
232800930305     C     SAVNPS        IFNE      ALF2
232900921229     C*
233000950515     C* HO CARICATO TUTTA SCHIERA
233100921229     C     *IN02         IFEQ      '0'
233200970417     C                   Z-ADD     1             Z                 3 0
233300930305     C                   MOVEL     ALF2          NUM2              2 0
233400970417     C     NUM2          LOOKUP    NPS(Z)                                 34
233500970417     C  N34              Z-ADD     1             Z
233600921229     C*
233700921229     C                   ELSE
233800921229     C*
233900921229     C* NON HO CARICATO LA SCHIERA --> FACCIO CHAIN
234000921229     C                   MOVEL     '7J'          COD
234100921229     C                   MOVEL     *BLANKS       KEY
234200930305     C                   MOVEL     ALF2          KEY
234300921229     C     KTAB          CHAIN     TABEL                              34
234400001023     C                   Z-ADD     2             Z
234500000316     C     *IN34         IFEQ      *OFF
234600000316     C                   MOVEL     TBLUNI        DS7J
234700000316     C                   MOVEL     §7JRPS        RPS(Z)
234800000316     C                   MOVEL     §7JORD        OPS(Z)
234900000316     C                   MOVEL     §7JTPS        TPS(Z)
235000000316     C                   MOVEL     §7JINV        INV(Z)
235100040503     C                   MOVEL     §7Jfsl        fsl(Z)
235200000316     C                   ELSE
235300000316     C                   MOVEL     'M'           RPS(Z)
235400000316     C                   MOVEL     999           OPS(Z)
235500000316     C                   MOVEL     'B'           TPS(Z)
235600000316     C                   MOVEL     ' '           INV(Z)
235700040503     C                   MOVEL     ' '           fsl(Z)
235800000316    1C                   END
235900921229     C                   END
236000921229     C*
236100930305     C                   MOVEL     ALF2          SAVNPS
236200921229     C                   END
236300941206     C*
236400921229     C                   ENDSR
236500941206     C**************************************************************************
236600941206     C* CHIUDO ANOMALIA
236700941206     C**************************************************************************
236800911018     C     CHIUAN        BEGSR
236900961211     C                   SETOFF                                       97
237000941206     C*
237100990518     C     KANM          CHAIN(N)  FNANM000                           97
237200961121     C**
237300961211    1C     *IN97         IFEQ      *OFF
237400970117     C**
237500970117     C** ANOMALIA 145/146: SE DATA SPUNTA > DATA ANOAMLIA
237600970117     C*  DEVO CONTROLLARE CHE NON CI SIANO ALTRE SPUNTE
237700970117     C*  CHE MI IMPEDISCONO LA CHIUSURA
237800970117     C     ANMCAA        IFEQ      145
237900970117     C     ANMCAA        OREQ      146
238000041013     c* Imposto econtrdfv per anomalie 145 146 che la deve sempre cotrollare
238100041013     c                   eval      wcontrdfv='S'
238200990518     C     BARDFV        IFGT      ANMDAO
238300970117     C                   EXSR      CTRSPU
238400970117     C                   ENDIF
238500970117     C                   ENDIF
238600961121     C**
238700041013     c     Wcontrdfv     ifne      'S'
238800041013     C* PER CATEGORIA no arrivi e partenza CHIUDO SE DATA >=
238900961122     C     WSTESS        OREQ      'S'
239000990518     C     BARDFV        ANDEQ     ANMDAO
239100041013     C     BARNPG        ANDne     2
239200041013     C     BARNPG        ANDne     1
239300041013     C* PER LE CATEGORIE arrivi SOLO SE DATA > e la posso chiudere
239400990518     C     BARDFV        ORGT      ANMDAO
239500970117     C     WCHIU         ANDEQ     'S'
239600961211     C*
239700961211     C     WCREA         OREQ      'N'
239800020213     C     BARFLE        ANDNE     BTFP
239900961121     C**
240000960416     C                   CLEAR                   DSLR33
240100960524     C                   MOVE      BARDFV        D33DCH
240200960514    2C     BARKEY        IFNE      SAVFOG
240300911125     C                   EXSR      RICFAS
240400960514    2C                   END
240500960514     C*
240600960416     C                   MOVEL     COMFAS        D33FSC
240700960416     C                   MOVEL     SAVCCH        D33CCH
240800960530     C                   MOVEL     SAVMAC        D33MAC
240900960416     C                   MOVE      ANMNR2        D33NRR
241000960416     C*
241100990712     C                   CALL      'FNLR33R'
241200960416     C                   PARM                    DSLR33
241300961209     C*
241400961209     C* PER ANOMALIE 145 E 146 CREO SE E' DA CREARE L'ANOMALIA 620
241500020212     C*  AOLO SE NON SONO IN CREAZIONE DELL 600
241600020212    2C     NTWBAR        IFEQ      'EEX'
241700961210     C     W620          ANDEQ     'S'
241800060322    2C     NTWBAR        oreq      'DPD'
241900060322     C     W620          ANDEQ     'S'
242000060322    2C     NTWBAR        oreq      'FED'
242100060322     C     W620          ANDEQ     'S'
242200961210     C*
242300961210    3C     ANMCAA        IFEQ      145
242400961209     C     ANMCAA        OREQ      146
242500961210     C*
242600961210    4C     W60X          IFEQ      'S'
242700961210     C                   Z-ADD     600           COMCAA
242800001110     C     COMCAA        LOOKUP    ACG                                    34
242900001110    5C     BARSPG        IFNE      'I'
243000001110     C     *IN34         OREQ      *ON
243100990518     C     KANM          DELETE    FNANM02L                           34
243200001110    5C                   ENDIF
243300961209     C                   Z-ADD     620           COMCAA
243400961209     C                   EXSR      CRTANM
243500961209     C                   CLEAR                   W60X
243600961210    4C                   ENDIF
243700961209     C*
243800961209     C* VEDO SE CANCELLARE L'ANOMALIA 645
243900961209     C                   Z-ADD     645           COMCAA
244000001110     C     COMCAA        LOOKUP    ACG                                    34
244100001110   4AC     BARSPG        IFNE      'I'
244200001110     C     *IN34         OREQ      *ON
244300990518     C     KANM          CHAIN     FNANM02L                           34
244400961210    4C     *IN34         DOWEQ     *OFF
244500990518    5C     ANMDAO        IFEQ      BARDFV
244600990518     C                   DELETE    FNANM000
244700961209     C                   SETON                                        34
244800961209     C                   ELSE
244900990518     C     KANM          READE     FNANM02L                               34
245000961210    5C                   ENDIF
245100961210    4C                   ENDDO
245200001110   4AC                   ENDIF
245300961210    3C                   ENDIF
245400961209     C*
245500961210    2C                   ENDIF
245600960514     C*
245700960514     C* L'ANOMALIA 05 PUO' ESSERCI PIU' VOLTE
245800961209    2C     ANMCAA        IFEQ      005
245900980709     C                   SETOFF                                       34
246000960514    3C     *IN34         DOWEQ     *OFF
246100990518     C     KANM          READE(N)  FNANM000                               34
246200960514     C                   MOVE      ANMNR2        D33NRR
246300960514     C*
246400960514     C  N34              CALL      'FNLR33R'
246500960514     C                   PARM                    DSLR33
246600960514    3C                   ENDDO
246700911018     C*
246800960514    2C                   ENDIF
246900961121   1AC                   ENDIF
247000961209   X1C                   ELSE
247100961209     C*
247200961211     C* SOLO PER ANOMALIE 145 E 146
247300961210    2C     COMCAA        IFEQ      145
247400961210     C     COMCAA        OREQ      146
247500991230     C* SE DEVO CREARE EVENTUALE 620 PER LNA ESTERA
247600020212    3C     W60X          IFEQ      'S'
247700961210     C     W620          ANDEQ     'S'
247800060322     C* E LA DEVO CREARE SIA SE TROVO CHE SE NON TROVO L'ANOMALIA 145
247900060322     C*  O 146 DA CHIUDERE  (WESAN =' ')
248000060322     C     WESAN         ANDEQ     ' '
248100110103     c*
248200060322    4C     NTWBAR        ifeq      'EEX'
248300060322     C     NTWBAR        oreq      'DPD'
248400060322     C     NTWBAR        oreq      'EEX'
248500961209     C                   Z-ADD     620           COMCAA
248600961209     C                   EXSR      CRTANM
248700961209     C                   CLEAR                   W60X
248800060322    4C                   ENDIF
248900060322    3C                   ENDIF
249000961210    2C                   ENDIF
249100961210    1C                   ENDIF
249200110103     c*
249300041013     c                   eval      wcontrdfv='N'
249400911018     C*
249500110929     c                   clear                   savmac
249600110929     c                   clear                   savcch
249700911018     C                   ENDSR
249800941206     C**************************************************************************
249900941206     C*    RICERCA FASE
250000941206     C**************************************************************************
250100911125     C     RICFAS        BEGSR
250200941206     C*
250300921020     C                   MOVEL     '7N'          COD
250400921020     C                   MOVEL     *BLANKS       KEY
250500921020     C                   MOVEL     BARNPG        KEY
250600921105     C     KTAB          CHAIN     TABEL                              41
250700921105     C  N41              MOVEL     TBLUNI        DS7N
250800000516     C*
250900000516     C     BARSPG        IFNE      ' '
251000000516     C                   MOVEL     §7NFS1        WFAS1             1
251100000516     C                   MOVEL     §7NFS2        WFAS2             1
251200000516     C                   MOVEL     §7NFS3        WFAS3             1
251300000516     C                   MOVEL     §7NFS4        WFAS4             1
251400000516     C                   MOVEL     §7NFS5        WFAS5             1
251500000516     C                   ENDIF
251600921021     C*
251700960524     C     BARSPG        IFEQ      §7NSC1
251800921021     C                   MOVEL     §7NFS1        COMFAS            1
251900921021     C                   ELSE
252000960524     C     BARSPG        IFEQ      §7NSC2
252100921021     C                   MOVEL     §7NFS2        COMFAS
252200921021     C                   ELSE
252300960524     C     BARSPG        IFEQ      §7NSC3
252400921021     C                   MOVEL     §7NFS3        COMFAS
252500921021     C                   ELSE
252600960524     C     BARSPG        IFEQ      §7NSC4
252700921021     C                   MOVEL     §7NFS4        COMFAS
252800921021     C                   ELSE
252900921021     C                   MOVEL     §7NFS5        COMFAS
253000921021     C                   END
253100921021     C                   END
253200921021     C                   END
253300921021     C                   END
253400921020     C*
253500961217     C                   MOVEL     BARKEY        SAVFOG            8 0
253600941206     C*
253700911125     C                   ENDSR
253800950927     C**************************************************************************
253900950927     C*    CONTROLLO SE CREARE ANOMALIA 50: COLLO ARRIVATO NON PARTITO
254000130228     C*                      O ANOMALIA 55/56/57: DISGUIDO - ESTERNA
254100950927     C**************************************************************************
254200950927     C     CRTA50        BEGSR
254300000320     C                   CLEAR                   WCCH
254400020214     C* PER ANOMALIA 50 NON CREO SE ESISTE GIA' LA DATA DI PARTENZA
254500020214     C*  BOLLE TRANSITO
254600020214     C     WTIBOL        IFEQ      'T'
254700020214     C     COMDFV        ANDGT     0
254800020214     C                   GOTO      NOA50
254900020214     C                   ENDIF
255000020214     C**
255100020214     C     WTIBOL        IFEQ      'A'
255200020218     C     WTIBOL        OREQ      'L'
255300050215     c* Per l'anoamlia 50 basterebbe questo test senza leggere le spunte,
255400050215     c*  le spunte le devo leggere lo stesso per i manca record
255500020214     C     WESART        IFEQ      'S'
255600020214     C     COMDFV        ANDGT     0
255700020214     C                   GOTO      NOA50
255800020214     C                   ENDIF
255900020214     C* SE E' COLLO IN ARRIVO E BOLLA CONSEGNATA --> NON CREO
256000020214     C     WESART        IFEQ      'S'
256100020214     C     ARTDCM        ANDGT     0
256200020214     C                   GOTO      NOA50
256300020214     C                   ENDIF
256400020218     C* SE BOLLA LOCALE E NON C'E' LA BOLLA NON FACCIO NIENTE
256500020218     C     WTIBOL        IFEQ      'L'
256600020218     C     WBOPAR        ANDEQ     'N'
256700020218     C                   GOTO      NOA50
256800020218     C                   ENDIF
256900020214     C                   ENDIF
257000020214     C**
257100950927     C* VERIFICO SE ESISTE GIA'
257200040429     c* ora la esistenza è all'interno dello stesso p.o. e non in assoluto
257300950927    1C     WTIBOL        IFEQ      'D'
257400950927     C                   Z-ADD     55            COMCAA
257500950927     C                   ELSE
257600950927     C                   Z-ADD     50            COMCAA
257700950927    1C                   ENDIF
257800990409     C* SE LA CATEGORIA E' IMA IMG O CONS INVECE CHE 55 CREO LA 56
257900020214    1C     COMCAA        IFEQ      55
258000020214    2C     BARNPG        IFEQ      4
258100990412     C     BARNPG        OREQ      3
258200990409     C     BARSPG        ANDEQ     'A'
258300990412     C     BARNPG        OREQ      3
258400990409     C     BARSPG        ANDEQ     'G'
258500161021     C     BARNPG        OREQ      3
258600161021     C     BARSPG        ANDEQ     'Z'
258700990409     C                   Z-ADD     56            COMCAA
258800020214    2C                   ENDIF
258900020214    1C                   ENDIF
259000130228     c* se Anomalia 55 a collo che può partire da diverse filiali
259100130228     c*  e chi spunta è una di qeuste --> non creo 55 ma 57 Disguido AUTORIZZ
259200130228     c*  solo per ategorie di spunta partenza
259300130228     c                   if        comcaa=55  and
259400130228     c                             (bolxco='W' or
259500130228     c                             (wesbtt='N' and wesblt='N' and wesart='N'
259600130228     c                              and barnrs>0))
259700130228     c* se si tratta di spunta partenza --> creo la 57
259800130228     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
259900130228     c                             or barnpg=1
260000130228     c                   exsr      CtrXCO
260100130228     c                   if        Indx>0
260200130301     c                   z-add     57            comcaa
260300130228     c                   endif
260400130228     c                   endif
260500130228     c                   endif
260600130301     c
260700130301     c* In caso di anomalia 50 manca bolla --> verifico se non è da creare
260800130301     c*  per collo spuntabile in questo TFP e "locale"
260900130301     c                   if        comcaa=50 and
261000130301     c                             (bolxco='W' or
261100130301     c                             (wesbtt='N' and wesblt='N' and wesart='N'
261200130301     c                              and barnrs>0))
261300130301     c* se si tratta di spunta di partenza --> non creo la 50 ma la 57
261400130301     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
261500130301     c                             or barnpg=1
261600130301     c                   exsr      CtrXCO
261700130301     c                   if        Indx>0
261800130301     c                   z-add     57            comcaa
261900130301     c                   endif
262000130301     c                   endif
262100130301     c                   endif
262200971009     C*
262300130228     c* per anomalia 55 o 56 57 valida l'anomalia per p.o. spunta
262400040429     c* per anomalia 50:
262500040429     c* per bolle transito solo se spara il p.o. transito
262600040429     c* per bolle arrivi, se trovo anomalia nell'area di arrivo(e non
262700040429     c*  di un eventuale p.o. transito)
262800040429     c                   clear                   wtrova
262900040429     C     KANM          SETLL     FNANM000
263000040504     C     KANM          reade(N)  FNANM000                               34
263100040429    1c                   dow       not*in34 and wtrova=*blanks
263200040429    2c                   if        anmcaa<>50 or wtibol='T'
263300040429    3c                   if        anmfle=bpes
263400040429     c                   eval      wtrova='1'
263500040429   x3c                   else
263600040504     C     KANM          reade(N)  FNANM000                               34
263700040429    3c                   endif
263800110103     c*
263900040429   x2c                   else
264000040429     C                   CLEAR                   DSLV55
264100040429     C                   MOVEL     'A'           D55TPT
264200040429     C                   Z-ADD     anmfle        D55LIN
264300040429     C                   Z-ADD     lnpb          D55lnp
264400040429     C                   Z-ADD     bardfv        D55DRF
264500040429     C                   EXSR      FNLV55
264600040429    3c                   if        d55tfa=pestfa
264700040429     c                   eval      wtrova='1'
264800040429   x3c                   else
264900040504     C     KANM          reade(N)  FNANM000                               34
265000040429    3c                   endif
265100040429    2c                   endif
265200110103     c*
265300040429    1c                   enddo
265400990412     C*
265500990412     C* PER ANOMALIA 55 VERIFICO ANCHE CHE NON CI SIA LA 56
265600990412     C*  PERCHE' DOPO AVER SPARATO UN COLLO IN IMA PUO' ESSERE
265700990412     C*  SPARATO IN PARTENZA PER FARLO RIPARTIRE
265800040429    1C     *IN34         IFEQ      *On
265900990412     C     COMCAA        ANDEQ     55
266000990412     C                   Z-ADD     56            COMCAA
266100040429     C     KANM          SETLL     FNANM000
266200040504     C     KANM          reade(N)  FNANM000                               34
266300040429     c                   dow       not*in34 and wtrova=*blanks
266400040429     c                   if        anmfle=bpes
266500040429     c                   eval      wtrova='1'
266600040429     c                   else
266700040504     C     KANM          reade(N)  FNANM000                               34
266800040429     c                   endif
266900040429     c                   enddo
267000990412     C                   Z-ADD     55            COMCAA
267100990412    1C                   ENDIF
267200950927     C*
267300040429    1C     *IN34         IFEQ      *On
267400970828     C* NON CREO ANOMALIA 55 SE C'E' UNA SPUNTA PARTENZA DI ALTRO AS
267500970828     C*  PERCHE' NON SI TRATTA PIU' DI UN DISGUIDO MA O E' COLLO
267600990409     C*  IN TRANSITO DI CUI NON HO RICEVUTO LA BOLLA
267700990409     C*  O E' COLLO DISGUIDATO IN ALTRA FIL CHE PER ANDARE IN ARRIVO
267800970828     C*  DEVE PASSARE DA ALTRA FIL ANCORA
267900970828     C*  LA 200 VIENE INVECE CREATA LO STESSO PERCHE' UNA ANOMALIA
268000970828     C*  GESTIONALE CHE SERVE PER FAR RIPARTIRE AUTOMATICAMENTE IL COLL
268100970828     C*  ANCHE SE NON VIENE SPARATO NE' IN PARTENZA NE' IN ENTRATA
268200000316     C                   EXSR      CNOIDD
268300950927    1C                   ENDIF
268400020214     C     NOA50         ENDSR
268500000316     C**************************************************************************
268600000316     C* CREA ANOMALIA COLLO ARRIVATO PARTITO CON PISTOLA FASULLA
268700000316     C**************************************************************************
268800160624     C***  CRTA60        BEGSR
268900000316     C* ESCLUDO SE PISTOLA ATOMATICA
269000160624    1C***  RPS(Y)        IFNE      'A'
269100160624     C***
269200000316     C* VEDO DALLA PISTOLA PARTENZA SE CREARE O MENO L'ANOMALIA
269300160624    2C***  WTIBOL        IFne      'T'
269400160624     C***                MOVEL     ARTNPP        ALF2
269500041013    2C**   WTIBOL        IFEQ      'T'
269600160624     c***                else
269700160624     C***                MOVEL     BTTNPP        ALF2
269800160624    2C***                ENDIF
269900000316     C*
270000160624     C***                EXSR      RICRAG
270100000316     C**
270200000316     C** DA INVIARE IN PARTENZA
270300160624    2C***  INV(Z)        IFEQ      'S'
270400000316     C**
270500160624    3C***  WTIBOL        IFne      'T'
270600000316     C* SOLO SE COLLO NON TRANSITATO O TRANSITATO CON PIST.FASULLA
270700160624    4C***  ARTDUT        IFGT      0
270800160624     C***                MOVEL     ARTPUT        ALF2
270900160624     C***                EXSR      RICRAG
271000160624    4C***                ENDIF
271100000316     C* SE NON C'E' TRANSITO O TRANSITO CON PISTOLA FASULLA, CONTR.
271200160624    4C***  ARTDUT        IFEQ      0
271300160624     C***  ARTDUT        ORGT      0
271400160624     C***  OPS(Z)        ANDGE     040
271500160624     C***                Z-ADD     60            COMCAA
271600000607     C* SE RICHIAMATO CON FLAG 6 CREO LA 61
271700160624     C***  dls45FLG      IFEQ      '6'
271800160624     C***                Z-ADD     61            COMCAA
271900160624     C***                ENDIF
272000041013     c* Se c'e' già anomalia non la ricreo
272100160624     c***  kanm          setll     fnanm000                               34
272200160624     c***                if        not *in34
272300160624     C***                EXSR      CNOIDD
272400160624    4C***                ENDIF
272500160624    4C***                ENDIF
272600000316     C*
272700160624   X3C***                ELSE
272800000316     C* NEGLI ALTRI CASI CREO SUBITO
272900160624     C***                Z-ADD     60            COMCAA
273000000607     C* SE RICHIAMATO CON FLAG 6 CREO LA 61
273100160624     C***  dls45FLG      IFEQ      '6'
273200160624     C***                Z-ADD     61            COMCAA
273300160624     C***                ENDIF
273400160624     c***  kanm          setll     fnanm000                               34
273500160624     c***                if        not *in34
273600160624     C***                EXSR      CNOIDD
273700160624    3C***                ENDIF
273800160624    3C***                ENDIF
273900160624     C***
274000160624    2C***                ENDIF
274100160624    1C***                ENDIF
274200160624     C***                ENDSR
274300000316     C**************************************************************************
274400000316     C* RIEMPO ALCUNI CAMPI ANOMALIA NO IDD
274500000316     C**************************************************************************
274600000316     C     CNOIDD        BEGSR
274700160624     C** ANOMALIE 50 -->
274800160725     C* VERIFICO SE ESISTE UNA SPUNTA PARTENZA PER QUEL COLLO
274900160624     C*  SE SI NON CREO ANOMALIA PER 50
275000130228     C*  ANOMALIE 55/56/57 --> CONSIDERO SOLO LA SPUNTA "D-DISGUIDO"
275100020214     C*                     PER NON CREARLE
275200000316     C                   Z-ADD     1             WNPG
275300000317     C                   CLEAR                   WTROVA
275400021121     C* CAMPI DI COMODO PER ANOM 50 per transito su stessa £1
275500021121     C                   CLEAR                   WFVALNP
275600021121     C                   CLEAR                   WFVANFV
275700021121     C                   CLEAR                   WFVADFV
275800110103     c*
275900070131     C     KBAR3         CHAIN     FNBRV09L                           99
276000020214    1C     *IN99         DOWEQ     *OFF
276100000317     C     WTROVA        ANDEQ     ' '
276200050215     c                   clear                   wrileggi          1
276300020214     C*****
276400020214     C* SOLO SE LA SPUNTA TROVATA E' DI DISGUIDO, ALTRIMENTI SE NORMALE
276500020214     C*  E' UNA SPUNTA PREVISTA, QUINDI DA IGNORARE
276600020214    2C     COMCAA        IFEQ      55
276700020214     C     COMCAA        OREQ      56
276800130228     C     COMCAA        OREQ      57
276900040303    3C     BR9FGS        IFNE      BTFP
277000040429     c* verifico tramite pgm fnlv53r se spunta di foglio diretto a me(BPES)
277100040429     c*  se è diretto a me non creo anomalia
277200040429     c*  Questo vale sia per colli in disguido che colli in transito
277300040429     c*  manca record bolla partenza
277400040430     c                   clear                   dslv53
277500040504     c                   movel     'T'           d53tfo
277600040429     C                   MOVEL     BR9NPG        D53NPG
277700040429     C                   Z-ADD     BR9NFV        D53NFV
277800040429     C                   MOVEL     BR9FGS        D53FGS
277900040429     C                   MOVEL     BR9FLE        D53FLF
278000040504     C                   MOVEL     'A'           D53ABB
278100040429     C                   MOVEL     br9lna        D53LNA
278200050603     C                   MOVEL     Usalnatfa     D53TFA
278300040504     C                   MOVEL     bpes          D53lai
278400040429     C                   CALL      'FNLV53R'
278500040429     C                   PARM                    DSLV53
278600040429     c* se non ci sono errori --> OK esco dal ciclo
278700040429    4c                   if        d53err=*blanks
278800000317     C                   MOVEL     '1'           WTROVA            1
278900050418   x4c                   else
279000050418     C                   eval      wrileggi='1'
279100040429    4c                   endif
279200020214   X3C                   ELSE
279300050215     C                   eval      wrileggi='1'
279400020214    3C                   ENDIF
279500020214   X2C                   ELSE
279600020214     C**
279700031204     c** bolla non locale: cerco spunta non mia
279800031204   2ac     WSPLOC        ifeq      *blanks
279900040303    3C     BR9FGS        IFNE      BTFP
280000050329     c* Deve essere una spunta trasmessa, altrimenti ignoro (scaricata
280100050329     c*  tardi)
280200050329     c     br9ftr        andne     *blanks
280300110103     c*
280400050222    4c                   if        comcaa=50
280500050215     c                   clear                   dslv53
280600050322     c                   if        wtibol='T'
280700050322     c                   movel     'T'           d53tfo
280800050322     c                   else
280900050215     c                   movel     'A'           d53tfo
281000050322     c                   endif
281100050215     C                   MOVEL     BR9NPG        D53NPG
281200050215     C                   Z-ADD     BR9NFV        D53NFV
281300050215     C                   MOVEL     BR9FGS        D53FGS
281400050215     C                   MOVEL     BR9FLE        D53FLF
281500050215     C                   MOVEL     'A'           D53ABB
281600050215     C                   MOVEL     br9lna        D53LNA
281700050603     C                   MOVEL     Usalnatfa     D53TFA
281800050215     C                   MOVEL     pestfa        D53lai
281900050215     C                   CALL      'FNLV53R'
282000050215     C                   PARM                    DSLV53
282100050215     c* se non ci sono errori --> OK esco dal ciclo
282200050222    5c                   if        d53err=*blanks
282300020214     C                   MOVEL     '1'           WTROVA            1
282400050222   X5C                   ELSE
282500050418     c* Se si tratta di errore di foglio non trovato e provengo da
282600050418     c*  aggiornamento bolla transito Ex disguido (richiamo da fnlr13r)
282700050418     c*  il foglio si potrebbe trovare in fnfva01r, er cui conttrollo da
282800050418     c*  FNFGV
282900070221    6c                   if        d53err='2' and dls45FLG='T'
283000050418     c                   movel     'G'           d53tfo
283100050418     C                   CALL      'FNLV53R'
283200050418     C                   PARM                    DSLV53
283300050418     c* se non ci sono errori --> OK esco dal ciclo
283400050418    7c                   if        d53err=*blanks
283500050418     C                   MOVEL     '1'           WTROVA            1
283600050418   x7c                   else
283700050418     C                   eval      wrileggi='1'
283800050418    7c                   endif
283900050418   x6c                   else
284000050418     C                   eval      wrileggi='1'
284100050418    6c                   endif
284200050222    5C                   ENDIF
284300110103     c*
284400050222     c* Per anom 60/61 se trovata spunta --> ok
284500160624   X4C**                 ELSE
284600160624     C**                 MOVEL     '1'           WTROVA            1
284700050222     c                   endif
284800110103     c*
284900020214   X3C                   ELSE
285000050215     C                   eval      wrileggi='1'
285100020214    3C                   ENDIF
285200110103     c*
285300031204   2ac                   else
285400031204     c** bolla     locale: cerco spunta mia
285500040303    3C     BR9FGS        IFeq      BTFP
285600031204     C                   MOVEL     '1'           WTROVA            1
285700031204   X3C                   ELSE
285800050215     C                   eval      wrileggi='1'
285900031204    3C                   ENDIF
286000031204   2aC                   ENDIF
286100031204    2C                   ENDIF
286200050215     c
286300050215     c                   if        wrileggi='1'
286400070131     C     KBAR3         READE     FNBRV09L                               99
286500050215     c                   endif
286600020214    1C                   ENDDO
286700160725     c*
286800160725     c* Se non trovata e si tratta di bolla locale: cerco spunta
286900160725     c*  entrata
287000160725     c                   if        wtrova=' ' and wsploc=*blanks
287100160725     C                   Z-ADD     5             WNPG
287200160725     C     KBAR3         chain     FNBRV09L
287300160725     c                   if        %found(fnbrv09l) and br9fgs=btfp
287400160725     C                   MOVEL     '1'           WTROVA            1
287500160725     c                   setoff                                       99
287600160725     c                   endif
287700160725     c                   endif
287800000316     C*
287900000316     C* 99 ON  - SPUNTA NON TROVATA --> CREO ANOMALIA PER 50/55/56
288000000316    0C     *IN99         IFEQ      *ON
288100000316     C     COMCAA        ANDNE     60
288200000614     C     COMCAA        ANDNE     61
288300000316     C* 99 OFF - SPUNTA     TROVATA --> CREO ANOMALIA PER 60
288400160624     C**   *IN99         OREQ      *OFF
288500160624     C**   COMCAA        ANDEQ     60
288600160624     C**   *IN99         OREQ      *OFF
288700160624     C**   COMCAA        ANDEQ     61
288800000316     C                   CLEAR                   FNANM000
288900040621     c*
289000000316     C* MEMORIZZO NUMERO SPEDIZIONE SU ANOMALIA SE C'E'
289100000316    1C                   SELECT
289200040504     C     WESBTT        wheneq    'S'
289300000316     C                   Z-ADD     BTTAAS        ANMAAS
289400000316     C                   Z-ADD     BTTLNP        ANMLNP
289500000316     C                   Z-ADD     BTTNSP        ANMNSP
289600040504     C     WESART        wheneq    'S'
289700000316     C                   Z-ADD     ARTAAS        ANMAAS
289800000316     C                   Z-ADD     ARTLNP        ANMLNP
289900000316     C                   Z-ADD     ARTNSP        ANMNSP
290000040504     C     WESblt        wheneq    'S'
290100040504     C                   Z-ADD     bltAAS        ANMAAS
290200040504     C                   Z-ADD     bltLNP        ANMLNP
290300040504     C                   Z-ADD     bltNSP        ANMNSP
290400000316    1C                   ENDSL
290500040621     c* Se c'e' la spedizione verifico se spedizione di valore
290600110103     c*
290700040621     C* linea partenza segnacollo
290800000316     C                   MOVEL     BARLNP        ANMFLS
290900000316     C* PER ANOMALIA 50
291000000316    1C     COMCAA        IFEQ      50
291100000316     C                   Z-ADD     BARFGS        ANMFLE
291200040908     C* PER BOLLA LOCALE non c'e' abbinamento di foglio per cui deduco le
291300040908     c*  date damettere sulla anomalia
291400040908     c                   clear                   wnofva            1
291500040908     c                   if        wtibol='L'
291600040908     c                   eval      wnofva='S'
291700040908     c                   endif
291800040908     c                   if        wesart='S' and arbtfp=arbtfa
291900040908     c                   eval      wnofva='S'
292000040908     c                   endif
292100050603     c                   if        wesart='N' and lnptfp=Usalnatfa
292200040908     c                   eval      wnofva='S'
292300040908     c                   endif
292400040908     c
292500040908   1ac                   if        wnofva='S'
292600040908    2c                   if        wesart='S'
292700020218     C                   Z-ADD     ARBDBR        SAVDAO
292800020218     C                   Z-ADD     ARBDBR        ANMDAO
292900020218     C                   Z-ADD     99998         ANMNFV
293000020218     C                   Z-ADD     LNPTFP        ANMCDU
293100020218     C                   Z-ADD     LNPTFP        WFL1
293200020218     C                   MOVEL     'L'           WFLGT
293300040908     c*
293400040908   x2c                   else
293500040908     c* Se non c'e' bolla cerco il 1° foglio locale con data < data spunta
293600040908     c     kfgv2         setll     fnfgv02l
293700040908     c     lnptfp        readpe    fnfgv02l
293800040908    3c                   dow       not %eof(fnfgv02l)  and
293900040908     c                             fgvttr <>'L'
294000040908     c     lnptfp        readpe    fnfgv02l
294100040908    3c                   enddo
294200110103     c*
294300040908    3c                   if        not %eof(fnfgv02l)
294400040908     C                   Z-ADD     fgvdfv        SAVDAO
294500040908     C                   Z-ADD     fgvdfv        ANMDAO
294600040908     C                   Z-ADD     99998         ANMNFV
294700040908     C                   Z-ADD     LNPTFP        ANMCDU
294800040908     C                   Z-ADD     LNPTFP        WFL1
294900040908     C                   MOVEL     'L'           WFLGT
295000040908    3c                   endif
295100040908    2c                   endif
295200110103     c*
295300040908  x1aC                   ELSE
295400070208     C* SE TROVA LA BOLLA IN BLP C'E' SEMPRE LNP BOLLA
295500020214     C*  ALTRIMENTI LA 5F ALTRIMENTI BARLNP
295600020214     C                   MOVEL     LNPB          WLIN
295700020214     C* IMPOSTO LA DATA DI RIFERIMENTO PER LA RICERCA DEL TERMINAL
295800020214     C*  DI PARTENZA: LA PRIMA VOLTA E' LA DATA DELLA BOLLA SE C'E'
295900020214     C                   MOVE      DSPB          WDTRIF
296000000316     C*
296100000316     C* CERCO IL NUMERO DI FV SU CUI DOVREBBE ESSERE PARTITO IL COLLO
296200000316     C                   MOVE      *BLANKS       WFLGT             1
296300000316     C                   CLEAR                   WLINPR
296400000316     C                   CLEAR                   WVALNP
296500000316    2C     WLIN          DOWGT     0
296600000316     C                   EXSR      RICFV
296700000316     C* MEMORIZZO IL P.O. DI TRANSITO E LA LINEA DI PARTENZA DEL FV
296800000316     C     WLIN          IFGT      *ZEROS
296900000316     C                   Z-ADD     WLIN          WLINPR
297000000316     C                   Z-ADD     FVALNP        WVALNP
297100000316     C                   ENDIF
297200000316    2C                   ENDDO
297300000316    2C     ANMCDU        IFGT      0
297400000316     C                   MOVEL     ANMCDU        WFL1
297500000316   X2C                   ELSE
297600070208     C* SE TROVA LA BOLLA IN BLP C'E' SEMPRE LNP BOLLA
297700020214     C*  ALTRIMENTI LA 5F ALTRIMENTI BARLNP
297800000316     C                   MOVEL     LNPB          WFL1
297900000316    2C                   ENDIF
298000020218   1AC                   ENDIF
298100000316     C*
298200000316   X1C                   ELSE
298300160624     C* PER ANOMALIA 55/56/57/      (NO IDD LE ALTRE)
298400000316     C                   Z-ADD     BARDFV        ANMDAO
298500000316     C                   MOVEL     BARFGS        ANMFLE
298600000316     C                   MOVEL     LNPB          WFL1
298700000316     C                   Z-ADD     BARNFV        ANMNFV
298800000316     C                   MOVEL     BARFGS        ANMCDU
298900000316    1C                   ENDIF
299000000316     C* SCRIVO L'ANOMALIA
299100000316     C                   EXSR      RIEANM
299200040621     c*
299300040621     c                   if        anmnsp>0
299400091209     c     kar5          chain(n)  fiar501l
299500040621    3c                   if        %found(fiar501l)
299600040621     c                   movel     ar5uni        dar5gen
299700040621     c                   else
299800040621     c                   clear                   dar5gen
299900040621    3c                   endif
300000040621     c                   movel     §ar5bva       anmft4
300100040621     c                   endif
300200000614     C*
300300000316     C* ESTERNA
300400000614    1C     §7CAIE        IFEQ      'E'
300500000316     C                   MOVEL     WFL1          ANMFL1
300600000614    1C                   ENDIF
300700000316     C* SCRIVO L'ANOMALIA SOLO SE HO TROVATO IL FOGLIO PARTENZA PER"50"
300800000614    1C     ANMCAA        IFNE      50
300900000316     C     ANMCAA        OREQ      50
301000000316     C     ANMNFV        ANDGT     0
301100000316     C* SE LA KEY E' PER SEGNACOLLO ED IL NUMERO E' 0 NON CREO L'ANOMAL
301200000316    2C     §7CKEY        IFNE      'S'
301300000316     C     §7CKEY        OREQ      'S'
301400000316     C     ANMSCN        ANDGT     0
301500001110     C************************
301600001110     C* CREO ANOMALIA SOLO SE:
301700001110     C************************
301800070215     C* ANOMALIA CREABILE ANCHE DA CAT.SPUNTA GENERICA
301900070215     C*                                 (ANCHE DA SPUNTA IMI)
302000070215     C     §7CCCG        ifeq      'S'
302100070215     C* CATEGORIA SPUNTA NON GENERICA   (NON È UNA SPUNTA IMI)
302200070215     C     BARSPG        orne      'I'
302300000316     C                   WRITE     FNANM000
302400001110     C*
302500001110     C* CHIUDO SE POSSIBILE UNA EVENTUALE 55 GIA' CREATA E POI CREO
302600001110     C*  LA 56
302700001110    3C     COMCAA        IFEQ      56
302800001110     C                   Z-ADD     55            COMCAA
302900001110     C                   MOVEL     'S'           SAVMAC
303000001110     C                   EXSR      CHIUAN
303100001110     C                   CLEAR                   SAVMAC
303200001110    3C                   ENDIF
303300000614    3C                   ENDIF
303400000614     C*
303500000316     C* PER ANOMALIA 50 CREATA AGGIORNO ANCHE LA DATA DI PARTENZA
303600000316     C* O LA DATA DI USCITA DAL TRANSITO
303700000316     C     ANMCAA        IFEQ      50
303800000316     C                   EXSR      AGGPAR
303900000316     C                   ENDIF
304000160727     C     ANMCAA        IFEQ      55
304100160727     C     ANMCAA        oreq      56
304200160727     C                   EXSR      AGGPAR_d
304300160727     C                   ENDIF
304400000316     C*
304500000316    2C                   ENDIF
304600020222     C*
304700020222     C                   ELSE
304800020222     C* SE NON TROVATO FOGLIO ATTENDO CHE ARRIVI (CI DEVE ESSERE PRIMA
304900031204     C*  O POI) se non locale
305000060103     c* Solo se non richiamato
305100060103     c  N02wtibol        ifne      'L'
305200020222     C                   SETON                                        3940
305300031204     c                   endif
305400000614    1C                   ENDIF
305500000614    0C                   ENDIF
305600000316     C                   ENDSR
305700130228     C**************************************************************************
305800130228     C* controllo se bolla con colli chde partono da divderse filiali
305900130228     C**************************************************************************
306000130228     C     CtrXCO        BEGSR
306100130228     c                   eval      Wflsnrs=(BARLNP*100)+BARnrs
306200130228
306300130228    1c                   if        wflsnrs<>savfnrs
306400130228     c                   clear                   xcotfp
306500130228     C                   MOVEL     'XCO'         kcod
306600130228     C                   eval      kke1='ALTRITFP'
306700130228     C                   eval      kke2=%editc(wflsnrs:'X')
306800130228     c     ktbe2         chain     tntbe01l
306900130228    2c                   if        %found(tntbe01l) AND tbeatb=' '
307000130228     C                   MOVEA     TBeUNI        xcotfp
307100130228    2c                   endif
307200130228     c
307300130228     c                   eval      savfnrs=wflsnrs
307400130228    1c                   endif
307500130228    c
307600130228     C* SE la fil spunte è compresa --> creo la 57
307700130228     c                   movel     barfgs        alfafgs           3
307800130228     c                   eval      Indx=%lookup(alfafgs:xcotfp)
307900130228     c
308000130228     c*
308100130228     C                   ENDSR
308200950927     C**************************************************************************
308300950927     C* RICERCA FV SU CUI E' PARTITA LA SPUNTA
308400950927     C**************************************************************************
308500950927     C     RICFV         BEGSR
308600950927     C                   CLEAR                   WMODL
308700021121     C                   movel     'N'           Wnoabb2           1
308800021121     C                   movel     'N'           Wnoabb6           1
308900950927     C* PRENDO IL TERMINAL DI PARTENZA DELLA LINEA DI PARTENZA
309000970814     C                   CLEAR                   DSLV55
309100970814     C                   MOVEL     'P'           D55TPT
309200970814     C                   MOVEL     WLIN          D55LIN
309300020214     C                   MOVEL     WDTRIF        D55DRF
309400020213     C                   EXSR      FNLV55
309500970814     C                   MOVEL     D55TFP        WPTFP
309600110103     c*
309700100713     C* PRENDO IL TERMINAL DI arrivo generico della linea
309800100713     C                   CLEAR                   DSLV55
309900100713     C                   MOVEL     'A'           D55TPT
310000100713     C                   MOVEL     barlna        D55LIN
310100100713     C                   MOVEL     WDTRIF        D55DRF
310200100713     C                   EXSR      FNLV55
310300100713     C                   MOVEL     D55TFa        WGENlnaTFA
310400021121     c
310500021121     c* se WPTFP = simfel ed ho salvato i campi del foglio illuminato
310600021121     c*  trovato, li considero validi e non rieseguo la routine
310700021121     c                   if        wptfp=simfel  and wfvalnp>0
310800021121     C                   MOVEL     wFVALNP       ANMCDU
310900021121     C                   Z-ADD     wFVALNP       SAVFLP
311000021121     C                   Z-ADD     wFVANFV       ANMNFV
311100021121     C                   Z-ADD     wFVADFV       ANMDAO
311200021121     C                   Z-ADD     wFVADFV       SAVDAO
311300021121     C                   CLEAR                   WLIN
311400021121     C                   MOVEL     '1'           WMODL             1
311500021121     c                   else
311600950927     C*
311700950927     C* CONTROLLO I FV ABBINATI, SE E' FOGLIO ARRIVI
311800020214     C     BARNPG        IFEQ      2
311900950927     C                   Z-ADD     BARNPG        KNPG4
312000950927     C                   Z-ADD     BARNFV        KNFV4
312100020214     C                   Z-ADD     BARFGS        KFGS4
312200950927     C                   EXSR      REDFVA
312300020214     C                   ENDIF
312400950927     C*
312500950927     C* SE NON L'HO TROVATO CERCO UN ALTRO F.ARRIVI DEL GIORNO
312600950927     C     WMODL         IFEQ      ' '
312700021121     c                   eval      wdata=bardfv
312800950927     C                   EXSR      MEMFAR
312900021121     C* MEMORIZZO LA NUOVA DATA
313000021121     C                   MOVEL     BARDFV        SAVDFV
313100110103     C                   Z-ADD     1             I                 3 0
313200950927     C*
313300950927    2C     PG2(I)        DOWGT     0
313400950927     C     ANMNFV        ANDEQ     0
313500950927     C                   Z-ADD     2             KNPG4
313600950927     C                   Z-ADD     PG2(I)        KNFV4
313700020214     C                   Z-ADD     SAVTFA        KFGS4
313800950927     C                   EXSR      REDFVA
313900021121     c*
314000021121     c* Memorizzo se nessuno dei fogli 2 ha dei fogli partenza abbinati
314100021121     c                   if        wabbinati=' '
314200021121     c                   clear                   wnoabb2
314300021121     c                   endif
314400950927     C                   ADD       1             I
314500950927    2C                   ENDDO
314600950927    1C                   ENDIF
314700110103     c*
314800021121     c* Se non ho fogli abbianti e non si tratta di cat arrivi, ritento
314900021121     c*  la ricerca dei fogli arrivi
315000021121    1c     WMODL         IFEQ      ' '
315100110103    2c                   if        barnpg<> 2 and
315200110103     c                             wnoabb2 ='N' and
315300021121     c                             wsavdata=bardfv
315400021121     c* Data - 1
315500021121     C                   MOVEL     bardfv        G02INV
315600021121     C                   Z-ADD     *ZEROS        G02DAT
315700021121     C                   Z-ADD     *ZEROS        G02TGI
315800021121     C                   MOVEL     '3'           G02ERR
315900021121     C                   CALL      'XSRDA8'
316000021121     C                   PARM                    WLBDAT
316100021121      *
316200021121     C                   SUB       1             G02TGI
316300021121     C                   Z-ADD     *ZEROS        GI8DAT
316400021121     C                   Z-ADD     *ZEROS        GI8INV
316500021121     C                   Z-ADD     G02TGI        GI8TGI
316600021121     C                   CALL      'XSRGI8'
316700021121     C                   PARM                    WGIDAT
316800021121      *
316900021121     C                   Z-ADD     GI8INV        wdata
317000021121      *
317100021121     C                   EXSR      MEMFAR
317200021121     C* MEMORIZZO LA NUOVA DATA
317300021121     c                   if        wsavdata>0
317400021121     C                   MOVEL     wsavdata      SAVDFV
317500021121     c                   else
317600021121     C                   MOVEL     BARDFV        SAVDFV
317700021121     c                   endif
317800110103     c*
317900021121     C                   Z-ADD     1             I
318000021121     C*
318100021121    3C     PG2(I)        DOWGT     0
318200021121     C     ANMNFV        ANDEQ     0
318300021121     C                   Z-ADD     2             KNPG4
318400021121     C                   Z-ADD     PG2(I)        KNFV4
318500021121     C                   Z-ADD     SAVTFA        KFGS4
318600021121     C                   EXSR      REDFVA
318700021121     C                   ADD       1             I
318800021121    3C                   ENDDO
318900021121    2C                   ENDIF
319000021121    1c                   endif
319100110103     c*
319200100713     c* se il FV non ancora trovato e si dovrebbe trattare di bolla locale
319300100713     c*  tengo i dati del foglio illuiminato
319400100713     c     WMODL         ifeq      ' '
319500100713     c     wgenlnatfa    andeq     wlin
319600100713     C                   MOVEL     wFVALNP       ANMCDU
319700100713     C                   Z-ADD     wFVALNP       SAVFLP
319800100713     C                   Z-ADD     wFVANFV       ANMNFV
319900100713     C                   Z-ADD     wFVADFV       ANMDAO
320000100713     C                   Z-ADD     wFVADFV       SAVDAO
320100100713     C                   CLEAR                   WLIN
320200100713     C                   MOVEL     '1'           WMODL             1
320300100713     c                   endif
320400950927     C*
320500950927     C* SE IL FV ANCORA NON C'E' CERCO SE ANCORA DA ABBINARE
320600021121    1C     WMODL         IFEQ      ' '
320700950927     C                   CLEAR                   KNFV4
320800950927     C                   CLEAR                   KNPG4
320900020214     C                   MOVE      SAVTFA        KFGS4
321000000223     C* Determino BARDFV-4 gg. che è la data minima che deve avere un
321100000223     C* foglio ancora da abbinare. Se la data foglio è più bassa di
321200000223     C* tale data il foglio verrà scartato
321300000223     C                   MOVE      *ZEROS        DATMIN
321400000223     C                   CLEAR                   WLBDAT
321500000223     C                   CLEAR                   WGIDAT
321600000223     C                   Z-ADD     BARDFV        G02INV
321700000223     C                   MOVEL     '3'           G02ERR
321800000223     C                   CALL      'XSRDA8'
321900000223     C                   PARM                    WLBDAT
322000000223     C     G02TGI        SUB       4             GI8TGI
322100000223     C                   CALL      'XSRGI8'
322200000223     C                   PARM                    WGIDAT
322300000223     C                   Z-ADD     GI8INV        DATMIN
322400950927     C                   EXSR      REDFVA
322500950927     C* SE ANCORA NON HO TROVATO NIENTE --> ESCO DAL CICLO
322600950927    2C     WMODL         IFEQ      ' '
322700950927     C                   CLEAR                   WLIN
322800950927    2C                   ENDIF
322900950927    1C                   ENDIF
323000990827     C*
323100990827     C* PULISCO WLIN PER NON CONTINUARE NELLA RICERCA DEL FOGLIO
323200990827     C* SE LA LINEA DI ARRIVO RISULTA TRANSITARE
323300990827     C* DA UN P.O. CHE  E' LO STESSO DAL QUALE IL COLLO ERA PARTITO
323400990827     C* (PER ES.: 53/31 CHE TRANSITA DA 062 E 062 LO FA TRANSITARE DA
323500990827     C* 53 (CASO CHE SI VERIFICA DURANTE IL PASSAGGIO DALLE TRAZIONI
323600990827     C* RIDOTTE ALLE TRAZIONI NORMALI))
323700990827     C     WLINPR        IFGT      *ZEROS
323800990827     C     WLIN          ANDGT     *ZEROS
323900990827     C     WLIN          ANDEQ     WVALNP
324000990827     C                   CLEAR                   WLIN
324100990827     C                   ENDIF
324200021121     C                   ENDIF
324300950927     C*
324400950927     C                   ENDSR
324500950927     C**************************************************************************
324600950927     C* LETTURA DEI DEI FOGLI VIAGGIO PARTENZA
324700950927     C**************************************************************************
324800950927     C     REDFVA        BEGSR
324900950927     C                   CLEAR                   WMODL
325000021121     C                   CLEAR                   Wabbinati         1
325100950927     C     KFVA4         SETLL     FNFVA000
325200950927     C     KFVA4         READE     FNFVA000                               99
325300021121     c* se 99 on - nessun foglio abbinato --> memorizzo
325400021121     c                   if        *in99
325500021121     c                   eval      wabbinati='N'
325600021121     c                   endif
325700950927     C*
325800950927    1C     *IN99         DOWEQ     *OFF
325900950927    2C     FVALNP        IFEQ      WPTFP
326000950927     C* DATA FOGLIO DEVE ESSERE <= ALLA DATA DI ARRIVO
326100960524     C     FVADFV        ANDLE     BARDFV
326200000223     C     KNPG4         ANDGT     *ZEROS
326300000223     C* SE STO CERCANDO FOGLIO ANCORA DA ABBINARE SCARTO SE E' DI DATA
326400000223     C* INFERIORE ALLA DATA MINIMA (DATA ARRIVO COLLO MENO QUATTRO GG.)
326500000223     C     FVALNP        OREQ      WPTFP
326600000223     C     FVADFV        ANDLE     BARDFV
326700000223     C     FVADFV        ANDGE     DATMIN
326800000223     C     KNPG4         ANDEQ     *ZEROS
326900960207     C*
327000950927     C* SE SI TRATTA DI UN FOGLIO DIRETTO TUTTO A POSTO
327100100712     c* ok anche se locale
327200950927    3C     FVAATR        IFEQ      ' '
327300100712     c     fvattr        oreq      'L'
327400950927     C                   MOVEL     FVALNP        ANMCDU
327500020221     C                   Z-ADD     FVALNP        SAVFLP
327600990518     C                   Z-ADD     FVANFV        ANMNFV
327700950927     C                   Z-ADD     FVADFV        ANMDAO
327800980120     C                   Z-ADD     FVADFV        SAVDAO
327900950927     C                   CLEAR                   WLIN
328000950927     C                   MOVEL     '1'           WMODL             1
328100980120     C* SE IL PRIMO FV TROVATO E' UN DIRETTO LO MEMORIZZO PER SAPERE SE
328200980120     C* DOVRO' AGGIOR.LA DATA DI PARTENZA O LA DATA USCITA DAL TRANSITO
328300980120     C     WFLGT         IFEQ      *BLANKS
328400980120     C                   MOVE      'D'           WFLGT
328500980120     C                   END
328600950927     C                   SETON                                        99
328700960207     C*
328800950927   X3C                   ELSE
328900960207     C* SE E' FOGLIO ILLUMINATO
329000950927     C* DEVO VEDERE DA DOVE E' TRANSITO E MANDARE AL TRANSITO LA SEGNA
329100950927     C*  LAZIONE
329200000505     C     KFWA          CHAIN     FNFWA01L                           28
329300000505     C     *IN28         IFEQ      *ON
329400000505     C     FWAATB        ORNE      ' '
329500000505     C                   CLEAR                   FWAFF3
329600000505     C                   CLEAR                   FWAFF4
329700000505     C                   CLEAR                   FWAFL3
329800000505     C                   CLEAR                   FWAFL4
329900000505     C                   ENDIF
330000000505     C**
330100950927     C                   Z-ADD     FVALNA        LNA2
330200950927     C                   Z-ADD     1             X
330300951002     C                   MOVEL     BARLNA        W003A             3
330400950927     C     W003A         LOOKUP    FFV(X)                                 99
330500960207     C* NON E' IL MIO TERMINAL
330600960902     C* ESCLUDO I FOGLI LOCALI
330700021125     c* in relatà elaboro anche i fogli locali, altrimenti come faccio a
330800021125     c*  determinare un collo arrivo no part, del terminal per la lna?
330900021125     c*  ES. 43  x 132
331000960207    4C     *IN99         IFEQ      *ON
331100021125     C***  FVATTR        ANDNE     'L'
331200050603     C                   MOVEL     Usalnatfa     W003A             3
331300960522    5C     FLP(X)        IFNE      W003A
331400960207     C                   MOVEL     FLP(X)        WLIN
331500020214     C                   MOVE      FVADFV        WDTRIF
331600960207     C                   MOVEL     '1'           WMODL             1
331700980120     C* SE IL PRIMO FV TROVATO E' UN TRANSITO LO MEMORIZZO PER SAPERE
331800980120     C* SE DOVRò AGGIOR. DATA DI PARTENZA O DATA USCITA DAL TRANSITO
331900980120     C     WFLGT         IFEQ      *BLANKS
332000980120     C                   MOVE      'T'           WFLGT
332100021121     c* Memorizzo lo stesso i campi che mi servono perchè se poi di tratta
332200021121     c*  di transito "locale" cioè su stesso as (vedi 82 con 113)
332300021121     c*  devo comunque tenere i dati del foglio illuminato non essendoci
332400021121     c*  abbinamento del foglio locale su stessa £1
332500021121     c*  Il problema verrà a morire quando diventeranno dei 1 livello
332600021121     c*  come devono essere
332700021121     c*
332800021121     c                   z-add     FVALNP        WFVALNP
332900021121     c                   z-add     FVANFV        WFVANFV
333000021121     c                   z-add     FVADFV        WFVADFV
333100980120     C                   END
333200960207   X5C                   ELSE
333300960207     C* SE E' FOGLIO ILLUMINATO PER 2 LIVELLO IN ARRIVO TUTTO A POSTO
333400960207     C                   MOVEL     FVALNP        ANMCDU
333500990518     C                   Z-ADD     FVANFV        ANMNFV
333600960207     C                   Z-ADD     FVADFV        ANMDAO
333700980120     C                   Z-ADD     FVADFV        SAVDAO
333800020221     C                   Z-ADD     FVALNP        SAVFLP
333900960207     C                   CLEAR                   WLIN
334000960207     C                   MOVEL     '1'           WMODL             1
334100980120     C* SE IL PRIMO FV TROVATO E' UN DIRETTO LO MEMORIZZO PER SAPERE SE
334200980120     C* DOVRO' AGGIOR.LA DATA DI PARTENZA O LA DATA USCITA DAL TRANSITO
334300980120     C     WFLGT         IFEQ      *BLANKS
334400980120     C                   MOVE      'D'           WFLGT
334500980120     C                   END
334600960207    5C                   ENDIF
334700960207     C*
334800960207   X4C                   ELSE
334900960207     C     KFVA4         READE     FNFVA000                               99
335000960207    4C                   ENDIF
335100950927    3C                   ENDIF
335200950927     C*
335300950927   X2C                   ELSE
335400950927     C     KFVA4         READE     FNFVA000                               99
335500950927    2C                   ENDIF
335600950927    1C                   ENDDO
335700950927     C                   ENDSR
335800980119     C**************************************************************************
335900980119     C* AGGIORNO DATA PART. O DI USCITA DA TRANS. A FRONTE DI ANOM.50
336000980119     C**************************************************************************
336100980119     C     AGGPAR        BEGSR
336200980119     C     KBAR          CHAIN     FNART27L                           3239
336300980119     C   39              GOTO      ENDPAR
336400160722     c
336500160722    1c     *IN32         IFEQ      *OFF
336600160722     c* Spunta di bolla in arrivo ( anche locale)
336700160722    2C                   if        WTIBOL='A'  or WTIBOL='L'
336800160722
336900980120    3C                   SELECT
337000980120     C     WFLGT         WHENEQ    'D'
337100980120     C                   Z-ADD     SAVDAO        ARTDFV
337200041201     C                   Z-ADD     anmnfv        ARTnFV
337300980120     C                   Z-ADD     88            ARTNPP
337400041202     C                   movel     'S'           ARTABN
337500020218     C     WFLGT         WHENEQ    'L'
337600020218     C                   Z-ADD     SAVDAO        ARTDFV
337700041201     C                   Z-ADD     anmnfv        ARTnFV
337800020218     C                   Z-ADD     90            ARTNPP
337900041202     C                   movel     'S'           ARTABN
338000980120     C     WFLGT         WHENEQ    'T'
338100980120     C                   Z-ADD     SAVDAO        ARTDUT
338200980120     C                   Z-ADD     89            ARTPUT
338300020221     C                   Z-ADD     SAVFLP        ARTFLP
338400041202     C                   movel     'S'           ARTABN
338500980120    3C                   ENDSL
338600160722    2c                   endif
338700160722     c*
338800160722     c* spunta al transito: devo aggiornare anche  in FNART
338900160722     c*  la data di partenza se non c'è
339000160722    2c                   if        WTIBOL='T'
339100160722     c
339200160722    3c                   if        artdfv=0
339300160722     C                   Z-ADD     SAVDAO        ARTDFV
339400160722     C                   Z-ADD     anmnfv        ARTnFV
339500160722     C                   Z-ADD     88            ARTNPP
339600160722    3c                   endif
339700160722    2c                   endif
339800160720
339900980120     C                   UPDATE    FNART000
340000980120     C                   MOVEL     'A'           WTBO              1
340100980120     C                   MOVEL     ARTSPE        WDSPE
340200980120     C                   EXSR      WR1AGB
340300160722    1C                   endif
340400160720
340500160722     C                   if        WTIBOL='T'
340600021203     C     KBAR37        CHAIN     FNBTT37L                           3739
340700980119     C   39              GOTO      ENDPAR
340800161108    2C     *IN37         IFEQ      *OFF
340900980120     C                   Z-ADD     SAVDAO        BTTDFV
341000980120     C                   Z-ADD     88            BTTNPP
341100980120     C                   UPDATE    FNBTT000
341200980120     C                   MOVEL     'T'           WTBO              1
341300980120     C                   MOVEL     BTTSPE        WDSPE
341400980120     C                   EXSR      WR1AGB
341500980120    2C                   END
341600160722    1C                   endif
341700160720
341800160720     C*  AGGIORNO ANCHE LA BOLLA PARTENZA
341900020218    1C     WTIBOL        IFEQ      'L'
342000160720     C     WTIBOL        oreq      'A'
342100160721     C     WTIBOL        oreq      'T'
342200020218     C     KBAR          CHAIN     FNBLT27L                           3239
342300020218     C   39              GOTO      ENDPAR
342400020218    2C     *IN32         IFEQ      *OFF
342500160721
342600160721     c                   if        wtibol<>'T'
342700160721     c                   z-add     anmdch        bltdam
342800160721     c                   else
342900160721     c                   z-add     anmdch        bltdet
343000160721     c                   z-add     bpes          bltflp
343100160722     c                   clear                   bltdut
343200160721     c                   endif
343300160720
343400160721    3C                   IF        WFLGT<>'T' AND
343500160721     c                             (bltdfv=0 or bltdfv>savdao) or
343600160721     c                             wflgt= 'T' and bltdfv=0
343700160721     c
343800160720     C                   Z-ADD     SAVDAO        bltDFV
343900160726     c                   eval      welasta_p='S'
344000160720
344100160721    4c                   select
344200160720     C     BLTDSE        WHENEQ    0
344300160720     c                   if        wflgt='D'
344400160720     C                   MOVEL     88            BLTNPP
344500160721     c                   else
344600160720     C                   Z-ADD     90            bltNPP
344700160720     c                   endif
344800160720     C     BLTDSE        WHENGT    0
344900160720     C     BLTDSE        ANDLT     BLTDFV
345000160720     C                   MOVEL     96            BLTNPP
345100160720     C     BLTDSE        WHENGT    0
345200160720     C     BLTDSE        ANDGE     BLTDFV
345300160720     C                   MOVEL     98            BLTNPP
345400160721    4C                   ENDSL
345500160720
345600160721    3c                   endif
345700160720
345800160722    3C                   if        wflgt='T'  and WTIBOL<>'T' and
345900160722     c                             (bltdut=0 or bltdut>savdao)
346000160720     C                   Z-ADD     SAVDAO        bltDUT
346100160722     C                   Z-ADD     bpes          bltFLP
346200160721    3c                   endif
346300160720     c
346400020218     C                   UPDATE    FNBLT000
346500020218     C                   MOVEL     'P'           WTBO              1
346600020218     C                   MOVEL     BLTSPE        WDSPE
346700020218     C                   EXSR      WR1AGB
346800160721     c
346900160721     c* se c'è transito, aggiorno anche BTT
347000160721    3C     WFLGT         ifeq      'T'
347100160721     C     KBAR_t        CHAIN     FNBTT37L                           3239
347200160721    4C     *IN32         IFEQ      *OFF
347300160721     C     *IN39         andeq     *OFF
347400160721
347500160726     c                   eval      welasta_p='S'
347600160721     C                   Z-ADD     SAVDAO        bttDUT
347700160721     c                   z-add     anmdch        bttdam
347800160721     C                   SELECT
347900160721     C     BTTDET        WHENEQ    0
348000160721     C                   MOVEL     89            BTTPUT
348100160721     C     BTTDET        WHENGT    0
348200160721     C     BTTPET        ANDEQ     89
348300160721     C                   MOVEL     89            BTTPUT
348400160721     C     BTTDET        WHENGT    0
348500160721     C     BTTDET        ANDLT     BTTDUT
348600160721     C                   MOVEL     96            BTTPUT
348700160721     C     BTTDET        WHENGT    0
348800160721     C     BTTDET        ANDGE     BTTDUT
348900160721     C                   MOVEL     98            BTTPUT
349000160721     C                   ENDSL
349100160721     C*
349200160721     C     BTTDET        IFEQ      0
349300160721     C                   Z-ADD     ANMDAO        BTTDET
349400160721     C                   MOVEL     89            BTTPET
349500160721     C                   ENDIF
349600160721     C                   UPDATE    FNBTT000
349700160721     C                   MOVEL     'T'           WTBO              1
349800160721     C                   MOVEL     BLTSPE        WDSPE
349900160721     C                   EXSR      WR1AGB
350000160721    4C                   ENDif
350100160721    3C                   ENDif
350200160720
350300020218    2C                   END
350400020218    1C                   END
350500160721
350600980119     C     ENDPAR        TAG
350700160721
350800980119     C   39              SETON                                        40
350900160726     c
351000160726     c                   clear                   welasta_p
351100160726     c
351200980119     C                   ENDSR
351300160727     C**************************************************************************
351400160727     C* AGGIORNO DATA entrata al transito per DIsguido
351500160727     C**************************************************************************
351600160727     C     AGGPAR_d      BEGSR
351700160727     C     KBAR          CHAIN     FNBLT27L                           3239
351800160727    2C     *IN32         IFEQ      *OFF
351900160727    2C     *IN39         andeq     *OFF
352000160727     C                   Z-ADD     SAVDAO        bltDET
352100160727     c                   clear                   bltdut
352200160727     c                   movel     barfgs        bltflp
352300160727     c                   update    fnblt000
352400160727
352500160727     C                   MOVEL     'P'           WTBO              1
352600160727     C                   MOVEL     BLTSPE        WDSPE
352700160727     C                   EXSR      WR1AGB
352800160727     c                   endif
352900160727     c
353000160727     C                   ENDSR
353100950927     C**************************************************************************
353200950927     C* MEMORIZZO I FOGLI ARRIVI CETEGORIA 2 E 6 DEL GIORNO
353300950927     C**************************************************************************
353400950927     C     MEMFAR        BEGSR
353500021121     c                   clear                   wsavdata          8 0
353600021113     c
353700021113     c* se è collo di bolla transito
353800021113     c*  devo usare simfel, il suo p.o. di transito
353900021113     c                   if        wtibol<>'T'
354000050603     C                   MOVEL     usalnatfa     KTFA2
354100021113     c                   else
354200021113     C                   z-add     simfel        KTFA2
354300021113     c                   endif
354400021113     c
354500950927     C* RIMEMORIZZO I FV SOLO SE E' CAMBIATA LA  DATA
354600021121    1C     wdata         IFNE      SAVDFV
354700970113     C     KTFA2         ORNE      SAVTFA
354800021107     c
354900021121     c* Cerco fogli cat 2 e 6  se non trovati in data BARDFV, li cerco
355000021107     c*  in data bardfv - 1
355100021113     c* faccio max 8 tentativi (1 settimana  tenendo conto che ci possono
355200021113     c*  essere dei festivi)
355300950927     C                   CLEAR                   PG2
355400021113     c                   z-add     0             wgiorni           2 0
355500021113     c
355600021113    2c                   dow       wdata>0  and wgiorni<8
355700021113     c                   add       1             wgiorni
355800950927     C*
355900950927     C                   Z-ADD     2             WNPG
356000950927     C     KFVV          SETLL     FNFVV002
356100950927     C     KFVV          READE     FNFVV002                               33
356200950927     C                   Z-ADD     1             I
356300950927     C*
356400950927     C     *IN33         DOWEQ     *OFF
356500950927     C                   Z-ADD     FV2NFV        PG2(I)
356600950927     C                   ADD       1             I
356700950927     C     KFVV          READE     FNFVV002                               33
356800950927     C                   ENDDO
356900021107     c*
357000021107     c                   xfoot     pg2           wpg2             13 0
357100110103     c                   if        wpg2>0
357200021121     c                   eval      wsavdata=wdata
357300021107     c                   clear                   wdata
357400021107     c                   else
357500021107     c* Data - 1
357600021107     C                   MOVEL     wdata         G02INV
357700021107     C                   Z-ADD     *ZEROS        G02DAT
357800021107     C                   Z-ADD     *ZEROS        G02TGI
357900021107     C                   MOVEL     '3'           G02ERR
358000021107     C                   CALL      'XSRDA8'
358100021107     C                   PARM                    WLBDAT
358200021107      *
358300021107     C                   SUB       1             G02TGI
358400021107     C                   Z-ADD     *ZEROS        GI8DAT
358500021107     C                   Z-ADD     *ZEROS        GI8INV
358600021107     C                   Z-ADD     G02TGI        GI8TGI
358700021107     C                   CALL      'XSRGI8'
358800021107     C                   PARM                    WGIDAT
358900021107      *
359000021107     C                   Z-ADD     GI8INV        wdata
359100021107     c                   endif
359200021107     c                   enddo
359300021121     c*
359400970113     C                   MOVEL     KTFA2         SAVTFA
359500021107    1C                   ENDIF
359600950927     C                   ENDSR
359700950927     C**************************************************************************
359800950927     C* RIEMPO PARTE DEI CAMPI DELLA ANOMALIA
359900950927     C**************************************************************************
360000950927     C     RIEANM        BEGSR
360100950927     C                   MOVEL     BARLNP        ANMFLS
360200950927     C                   MOVEL     BARLNA        ANMLNA
360300950927     C                   MOVEL     BARNRS        ANMNRS
360400950927     C                   MOVEL     BARNSC        ANMSCN
360500950927     C                   MOVEL     BARNPS        ANMNPS
360600950927     C                   Z-ADD     COMCAA        ANMCAA
360700040621     c                   z-add     999           anmfl4
360800040621     c* imposto in anmflo data e ora creazione spunta
360900050707     C                   TIME                    TIMEanm          14 0
361000050707     c                   movel     timeanm       wora4             4 0
361100050707     C                   move      timeanm       G02DAT
361200050707     C                   Z-ADD     *ZERO         G02INV
361300050707     C                   MOVEL     *BLANKS       G02ERR
361400050707     C                   CALL      'XSRDA8'
361500050707     C                   PARM                    WLBDAT
361600050707     C                   move      G02INV        WDAT6             6 0
361700040621     c                   movel     wdat6         anmflo
361800040621     c                   move      wora4         anmflo
361900950927     C* FASE APERTURA
362000950927     C     BARKEY        IFNE      SAVFOG
362100950927     C                   EXSR      RICFAS
362200950927     C                   END
362300950927     C                   MOVEL     COMFAS        ANMFSA
362400950927     C* CODICE ANOMALIA
362500950927     C     COMCAA        IFNE      SAVCAA
362600950927     C                   MOVEL     '7C'          COD
362700950927     C                   MOVEL(P)  COMCAA        KEY
362800950927     C     KTAB          CHAIN     TABEL                              34
362900950927     C   34              CLEAR                   DS7C
363000950927     C  N34              MOVEL     TBLUNI        DS7C
363100950927     C                   MOVEL     COMCAA        SAVCAA            3 0
363200950927     C                   ENDIF
363300950927     C*
363400950927     C                   MOVEL     §7CTAN        ANMTAN
363500950927     C                   MOVEL     §7CAIE        ANMAIE
363600950927     C                   MOVEL     §7CSAN        ANMSAN
363700950927     C                   MOVEL     §7CLID        ANMLID
363800950927     C* AUTOCHIUSA
363900950927     C     §7CCHA        IFEQ      'S'
364000960524     C                   MOVE      BARDFV        ANMDCH
364100950927     C                   MOVEL     COMFAS        ANMFSC
364200000320     C                   MOVEL     WCCH          ANMCCH
364300950927     C                   END
364400950927     C                   ENDSR
364500960523     C**************************************************************************
364600960523     C* CARICO LA L6 DELLA FILIALE GESTIONE FOGLIO
364700960523     C**************************************************************************
364800960523     C     CARL6         BEGSR
364900960523     C                   CLEAR                   DSUL06
365000960523     C                   MOVE      '£6'          D06COD
365100020213     C                   MOVEL     SAVPES        D06KEY
365200960523     C                   MOVEL     DSUL06        KPJBU
365300960523     C*
365400960523     C                   CALL      'TRUL06R'
365500960523     C                   PARM                    KPJBA
365600960523     C                   MOVEL     KPJBU         DSUL06
365700960523     C                   MOVEA     LIN           L6
365800960523     C                   ENDSR
365900960523     C**************************************************************************
366000960523     C* SE ESISTE GIA' UNA SPUNTA DELLA LNA, NON CREO ANOMALIE 145-146
366100960523     C**************************************************************************
366200960523     C     CTRSPU        BEGSR
366300960523     C                   MOVEL     'S'           WCREA
366400970117     C*
366500970117     C                   MOVEL     'S'           WCHIU             1
366600970117     C*"WCHIU" MI SERVE PER DETERMINARE SE POSSO CHIUDERE UNA ANOMALIA
366700970117     C* 145/146 CHE HA DATA ANOMALIA < DELLA DATA DELLA SPUNTA CHE
366800970117     C*  STO ELABORANDO MA POTREBBE COMUNQUE ESSERCI UNA SPUNTA
366900970117     C*  CHE MI MANTIENE APERTA IN DATA > TALE ANOMALIA
367000970117     C*  1) SE E' PRESENTE UN'ALTRA SPUNTA CON DATA > NON CHIUDO
367100970117     C*  2) SE E' PRESENTE UNA SPUNTA CON DATA = CATEGORIA 3 4 5
367200020204     C*     NON CHIUDO SE LA SPUNTA CHE STO SCARICANDO E' CAT 1 2 6
367300970117     C*  3) SE E' PRESENTE UNA SPUNTA CON DATA = CATEGORIA 3 4 5
367400970117     C*     E E' UNA SPUNTA DELLA LNA SE LA SPUNTA CHE STO SCARICANDO
367500970117     C*     E' CATEGORIA 3 4 5
367600960531     C**
367700020204    1C     WAGARR        IFEQ      'S'
367800960531     C**
367900070131     C     KBRV7         CHAIN     FNBRV07L                           33
368000960531    2C     *IN33         DOWEQ     *OFF
368100960531    3C     BR7KEY        IFNE      SA1KEY
368200960527     C                   MOVEL     BR7KEY        SA1KEY
368300960527     C                   Z-ADD     BR7NPG        WNPG
368400960527     C                   Z-ADD     BR7NFV        WNFV
368500021014     C                   Z-ADD     BR7FGS        WFGS
368600070906     C                   movel     BR7nps        Wnps
368700021014     C                   CLEAR                   RICBAR
368800960523     C                   EXSR      RILFOG
368900960523     C                   Z-ADD     WDFV          BRVDFV
369000140616     C                   movel     WFCF          BRVFCF
369100140616     C                   movel     Wspg          BRVspg
369200960531    3C                   ENDIF
369300040224     c* Terminal di arrivo della spunta letta
369400040224     C                   CLEAR                   DSLV55
369500040224     C                   MOVEL     'A'           D55TPT
369600040224     C                   MOVEL     LNPB          D55LNP
369700040224     C                   MOVEL     brvdfv        D55DRF
369800040224     C                   MOVE      Br7pes        D55LIN
369900040224     C                   EXSR      FNLV55
370000050603     c                   if        d55tfa=Usalnatfa
370100960531     C**
370200961210    3C     BRVDFV        IFGT      BARDFV
370300960523     C                   MOVEL     'N'           WCREA             1
370400970117     C                   MOVEL     'N'           WCHIU             1
370500961209   X3C                   ELSE
370600961209     C*
370700961210    4C     BRVDFV        IFEQ      BARDFV
370800961210     C     BR7NPG        ANDGE     3
370900961210     C     BR7NPG        ANDLE     5
371000970117     C*
371100970117     C     BARNPG        IFEQ      1
371200970117     C     BARNPG        OREQ      2
371300970117     C                   MOVEL     'N'           WCHIU
371400970117     C                   ENDIF
371500961209     C*
371600020213    5C     BR7PES        IFEQ      BARLNA
371700961209     C                   MOVEL     'N'           WCREA             1
371800970117     C*
371900970117     C     BARNPG        IFNE      1
372000970117     C     BARNPG        ANDNE     2
372100970117     C                   MOVEL     'N'           WCHIU
372200970117     C                   ENDIF
372300970117     C*
372400961209   X5C                   ELSE
372500020214    6C     BR7PES        IFNE      SAVPES
372600020214     C                   MOVEL     BR7PES        SAVPES
372700961209     C                   EXSR      CARL6
372800961209    6C                   ENDIF
372900961209     C*
373000961209     C     BARLNA        LOOKUP    L6                                     34
373100961209     C   34              MOVEL     'N'           WCREA             1
373200970117     C*
373300970117    6C   34BARNPG        IFNE      1
373400970117     C     BARNPG        ANDNE     2
373500970117     C                   MOVEL     'N'           WCHIU
373600970117    6C                   ENDIF
373700970117     C*
373800961209    5C                   ENDIF
373900961209    4C                   ENDIF
374000961209    3C                   ENDIF
374100040224    3C                   ENDIF
374200961209     C*
374300961209    3C     WCREA         IFEQ      'N'
374400961209     C                   SETON                                        33
374500961210     C                   ELSE
374600070131     C     KBRV7         READE     FNBRV07L                               33
374700961209    3C                   ENDIF
374800040224     C**
374900960531    2C                   ENDDO
375000960531     C*
375100960531    1C                   ENDIF
375200960523     C*
375300960523     C                   ENDSR
375400961202     C**************************************************************************
375500961202     C* CREO ANOMALIA 145 O 146 E CHIUDO L'ALTRA
375600961202     C**************************************************************************
375700961202     C     ANM14X        BEGSR
375800961202     C* LA CREO SOLO SE NON ESISTE UNA SPUNTA DELLA LNA CON DATA=>
375900961210     C     WDCM          IFGT      0
376000961210     C     WDCM          ANDGE     BARDFV
376100961210     C                   MOVEL     'N'           WCREA
376200961210     C                   ELSE
376300961202     C                   EXSR      CTRSPU
376400961210     C                   ENDIF
376500961202     C*
376600961202    5C     WCREA         IFEQ      'S'
376700020213     C     BARFLE        ANDEQ     BTFP
376800020213     C     BARFLE        ORNE      BTFP
376900961202     C                   Z-ADD     CAACRE        COMCAA
377000961202     C                   EXSR      CRTANM
377100961202     C* SOLO PER EXPORT
377200961202     C*
377300961202     C     CAACRE        IFEQ      146
377400961202    6C     WTIBOL        ANDNE     'A'
377500961202     C                   MOVEL     'S'           WTIBOL
377600961202    6C                   ENDIF
377700961202    5C                   ENDIF
377800961202     C*
377900961202     C* SE SPUNTA DA ALTRO AS MA NON DOVEVO CREARE IL COLLO DA INOLTRAR
378000961202     C*  LA CHIUDO SUBITO. ANCHE SE IL COLLO E' CONSEGNATO SENZA SPUNTE
378100020213    5C     BARFLE        IFNE      BTFP
378200961202    6C     WCREA         IFNE      'S'
378300961202     C                   Z-ADD     CAACRE        COMCAA
378400961211     C* IN QUESTO CASO NON DEVO CREARE L'ANOMALIA 620 IN QUANTO
378500961211     C*  LA SPEDIZIONE E' GIA' STATA CONSEGNA A COMUNQUE E' PRESENTE
378600961211     C*  UNA SPUNTA CON DATA >.
378700961211     C                   MOVEL     'N'           W620
378800961202     C                   EXSR      CHIUAN
378900961211     C                   MOVEL     'S'           W620
379000961211     C*
379100961209   X6C                   ELSE
379200961210     C                   EXSR      ANM600
379300961202    6C                   ENDIF
379400961209     C*
379500961202   X5C                   ELSE
379600961210    6C     WCREA         IFEQ      'S'
379700961209     C* PER ESTERO CREO ANOMALIA 645 E 600
379800961209     C                   EXSR      ANM600
379900961210    6C                   ENDIF
380000961210    5C                   ENDIF
380100961202     C*
380200961210     C                   MOVEL     'N'           W620              1
380300961202     C                   Z-ADD     CAACIU        COMCAA
380400961202     C                   EXSR      CHIUAN
380500961210     C                   MOVEL     'S'           W620              1
380600961202     C                   ENDSR
380700961213     C**************************************************************************
380800961213     C* LETTURA FOGLIO DELLA SPUNTA CHE STO CARICANDO E IMPOSTAZIONE
380900961213     C*  CAMPI DI COMOD
381000961213     C**************************************************************************
381100961213     C     BARFGV        BEGSR
381200961213     C*
381300961213    3C     BARKEY        IFNE      SAVKEY
381400961213     C                   MOVEL     BARKEY        SAVKEY
381500961213     C                   Z-ADD     BARNPG        WNPG
381600961213     C                   Z-ADD     BARNFV        WNFV
381700021014     C                   Z-ADD     BARFGS        WFGS
381800070906     C                   movel     BARnps        Wnps
381900021014     C                   MOVEL     'S'           RICBAR            1
382000961213     C                   EXSR      RILFOG
382100980313     C                   MOVEA     FFG           FFS
382200961213     C                   Z-ADD     WDFV          BARDFV
382300961213     C                   MOVEL     WSPG          BARSPG
382400021025     C                   MOVEL     WFGS          wwwFGS
382500021023     C                   Z-ADD     WFLE          BARFLE
382600140616     C                   movel     WFCF          BARFCF
382700970521     C*
382800970521     C     WTTR          IFNE      *BLANKS
382900970520     C     WTTR          LOOKUP    TTR                                    34
383000970520     C                   MOVE      *IN34         WDEFL             1
383100970521     C                   ELSE
383200970521     C                   MOVE      '0'           WDEFL             1
383300970521     C                   ENDIF
383400970521     C*
383500961213    3C                   ENDIF
383600021025     C* IL P.O. GESTIONE LO DEVO SEMPRE IMPOSTARE PERCHÈ PUÒ CAMBIARE
383700021031     C                   MOVEL     WWWFGS        BARFGS
383800070209     c
383900070209     c* Se si tratta di spunta partenza, il BPES deve essere il P.O foglio
384000070209     c*  per problemi con eventuale creazione di anomalie 200
384100070209     c                   eval      wpes=bpes
384200070209     c                   if        barnpg=1
384300070209     c                   eval      bpes=barfgs
384400070209     c                   endif
384500961213     C                   ENDSR
384600970203     C**************************************************************************
384700970203     C* ANOMALIE CHE DEVO SEMPRE CREARE O SEMPRE CHIUDERE SE COLLO
384800970203     C*  SPARATO NELL'AREA DI ARRIVO (COME TERMINAL DI ARRIVO)
384900970203     C**************************************************************************
385000970203     C     ANMSEM        BEGSR
385100970203     C*
385200970203     C* COLLO/BOLLA ARRIVI PRESENTE
385300970204     C* SE CONSEGNATO IL COLLO CREO SEMPRE ANOMALIA --> COLLO CONSEGNAT
385400970204    1C  N05WBOAR         IFEQ      'S'
385500990120    2C     BARDFV        IFGT      WATDCM
385600970204     C     WATDCM        ANDGT     0
385700970203     C                   Z-ADD     138           COMCAA
385800970203     C                   EXSR      CRTANM
385900990120    2C                   ENDIF
386000000705     C* SE COLLO CAT.8 SEGNALA SE LA RELATIVA DISTINTA 4 E'
386100000705     C*  GIA' CHIUSA CON ANOMALIA 139
386200040430     c                   if        watdcm>0  and barnpg=8
386300000705     C* SE NON E' CHIUSA VEDO SE IN FASE DI CHIUSURA: CHIAMO FNLV53R
386400000705     C                   CLEAR                   DSLV53
386500000705     C                   MOVEL     'V'           D53TFO
386600020213     C                   MOVEL     BTFP          D53FEL
386700000705     C                   Z-ADD     BARNFV        D53NFV
386800000705     C                   MOVEL     4             D53NPG
386900000705     C                   MOVEL     BARFGS        D53FGS
387000000705     C                   MOVEL     BARFLE        D53FLF
387100000705     C                   CALL      'FNLV53R'
387200000705     C                   PARM                    DSLV53
387300000705     C*
387400000705     C* FOGLIO CHIUSO (S) O IN FASE DI CHIUSURA (F)
387500000705    3C     D53FCF        IFEQ      'S'
387600000705     C     D53FCF        OREQ      'F'
387700040430     c* non creo se la relativa consegna anomala non lo prevede
387800040430    4c                   if        WABCCA<>*blanks and wabcca<>savcca
387900040430     C                   MOVEL     '7O'          COD
388000040430     C                   MOVEL(P)  wabcca        KEY
388100040430     C     KTAB          CHAIN     TABEL                              41
388200040430     C  N41              MOVEL     TBLUNI        DS7o
388300040430     c   41              clear                   §7o139
388400040430     c                   eval      savcca=wabcca
388500040430    4c                   endif
388600040430     c
388700040430    4c                   if        §7o139=*blanks or wabcca=*blanks
388800000705     C                   Z-ADD     139           COMCAA
388900000705     C                   EXSR      CRTANM
389000040430    4C                   ENDIF
389100040430    3C                   ENDIF
389200000705    2C                   ENDIF
389300000705     C*
389400990120     C* COLLO NON CONSEGNATO
389500990121    2C     WATDCM        IFEQ      0
389600990120     C* VERIFICO SE DEVO CREARE ANOMALIA SU DANNI
389700000523     C*  NON LO CREO PER LA CATEGORIA 4 E 8
389800990121     C                   MOVEL     'N'           WCAT4             1
389900990126     C* NON GESTISCO IL RITROVAMENTO COLLO
390000990126     C                   MOVEL     'N'           WRITRO
390100990126     C*     GESTISCO LA CREAZIONE ANOMALIE
390200990126     C                   MOVEL     'S'           WCREAN
390300990120     C                   EXSR      CTRDAN
390400990120     C** SE NON TROVATA CA DI COLLO
390500990120     C* VERIFICO SE LA SPEDIZIONE E' SU UNA CA CHIUSA --> CREO
390600990120    3C     WTRDAN        IFEQ      ' '
390700040811      *
390800040811      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
390900040811      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
391000040811      * legate alla spedizione che passo
391100040811     c                   clear                   fidn12ds
391200040811     c                   eval      i12aas = ARTAAS
391300040811     c                   eval      i12lnp = ARTLNP
391400040811     c                   eval      i12nrs = ARTNRS
391500040811     c                   eval      i12nsp = ARTNSP
391600040811     c                   eval      i12fel = 'E'
391700040811     c*
391800040811     c                   call      'FIDN12R'
391900040811     c                   parm                    fidn12ds
392000040811     c*
392100040811     c                   setoff                                       33
392200040811     c*
392300040811     c* se non ci sono errori
392400040811     c                   if        o12err <> ' '
392500040811     c                   seton                                        33
392600040811     c                   else
392700040811     c* se numero di CA trovate maggiore di zero
392800040811     c                   if        o12nca = 0
392900040811     c                   seton                                        33
393000040811     c                   else
393100040811     c                   z-add     1             jj                2 0
393200040811     c                   dow       jj <= o12nca and *in33 = *off
393300040811     C     skDch(jj)     IFGT      0
393400040811     C     BARNPG        IFNE      4
393500040811     C     BARNPG        ANDNE     8
393600040811     C*
393700040811     C* Valorizzo i campi DCT* utilizzati nella routine CRE151 con quelli
393800040811     C* ritornati dal *pgm d utilità FIDN12R
393900040811     C                   movel     skCch(jj)     DCTCCH
394000040811     C*
394100040811     C                   EXSR      CRE151
394200040811     C                   ELSE
394300040811     C* SE E' CAT 4 E 8 LA CHIUDO
394400040811     C                   Z-ADD     151           COMCAA
394500040811     C                   EXSR      CHIUAN
394600040811     C                   ENDIF
394700040811     C**
394800040811     C                   SETON                                        33
394900040811     C**
395000040811     C                   ENDIF
395100040811     c*
395200040811     c                   add       1             jj
395300040811     c                   enddo
395400040811     c*
395500040811     c                   seton                                        33
395600040811     c*
395700040811     c                   endif
395800040811     c                   endif
395900040811     C**
396000990120    3C                   ENDIF
396100990120     C**
396200990120    2C                   ENDIF
396300990120    1C                   ENDIF
396400021023     C*
396500970203     C* CHIUDO LE ANOMALIE DI COLLO MANCATE
396600020204    1C     WAGARR        IFEQ      'S'
396700970203     C* PER COLLO NON LOCALE
396800021023     C     WAGPAR        OREQ      ' '
396900970203     C* PER BOLLA NON TROVATA IN PARTENZA
397000970203     C     WBOPAR        OREQ      'N'
397100021023     C**
397200021023     C* NON AGGIORNO CON SPUNTA ENTRATA
397300021023     C* SE NON E' LOCALE O DISGUIDO IN AREA AGGIORNO SEMPRE
397400021023    2C     WSPLOC        IFEQ      ' '
397500021023     C     WSPLOC        OREQ      'D'
397600021023     C* SE E' LOCALE SOLO SE E' SPUNTA FATTA CON CAT.ARRIVI  DAL P.O.
397700021023     C*  LINEA ARRIVO O TERMINAL ARRIVO
397800021023     C     WSPLOC        OREQ      'A'
397900021023     C     BARNPG        ANDNE     5
398000021023     C     BARNPG        ANDNE     1
398100021023     C*
398200021023     C     WSPLOC        OREQ      'A'
398300021023     C     BARNPG        ANDEQ     1
398400021023     C     WDEFL         ANDEQ     '1'
398500021023     C*
398600021023     C     WSPLOC        OREQ      'T'
398700021023     C     BARNPG        ANDNE     5
398800021023     C     BARNPG        ANDNE     1
398900021023     C*
399000021023     C     WSPLOC        OREQ      'T'
399100021023     C     BARNPG        ANDEQ     1
399200021023     C     WDEFL         ANDEQ     '1'
399300021023     C*
399400970203     C*
399500970203     C* COLLO ARRIVATO MANCANTE --> SOLO SE SPARATO SUL SISTEMA
399600041018     c* L'anomalia 15 non la chiudo se altra spunta presente in cat.arrivi
399700041018     c*  con stessa data
399800041018     c                   eval      wcontrdfv='S'
399900041018     c                   eval      wstess='S'
400000041018     c                   eval      wchiu='S'
400100970203     C                   Z-ADD     15            COMCAA
400200970203     C                   MOVEL     'TR'          SAVCCH
400300970203     C                   EXSR      CHIUAN
400400970203     C*
400500970203     C* COLLO PARTITO NON ARRIVATO
400600970203     C                   MOVEL     'AR'          SAVCCH
400700970203     C                   Z-ADD     5             COMCAA
400800970203     C                   EXSR      CHIUAN
400900970203     C* MANCA COLLO MANCA BOLLA
401000970203     C                   MOVEL     'AR'          SAVCCH
401100970203     C                   Z-ADD     12            COMCAA
401200970203     C                   EXSR      CHIUAN
401300990521    2C                   ENDIF
401400990521    1C                   ENDIF
401500040430     c
401600040430     c* Chiudo fuori posto bolla partenza e fuori posto bolla transito
401700040430     C                   Z-ADD     120           COMCAA
401800040430     C                   EXSR      CHIUAN
401900040430     C                   Z-ADD     125           COMCAA
402000040430     C                   EXSR      CHIUAN
402100970203     C                   ENDSR
402200970204     C**************************************************************************
402300970204     C* ANOMALIA FUORI POSTO SEGNACOLLO BOLLA IN PARTENZA
402400970204     C**************************************************************************
402500970204     C     ANM120        BEGSR
402600970204     C                   Z-ADD     120           COMCAA
402700970204     C*
402800970204     C* VALGONO SOLTANTO LE SPUNTE SPARATE NELL'AREA DI PARTENZA
402900020213    1C  N05BARFLE        IFEQ      BTFP
403000970204     C* SPUNTA DEL TERMINAL DI PARTENZA
403100020213    2C     BARFGS        IFEQ      BTFP
403200970204     C* SE COLLO NON SPUNTATO NELLE CATEGORIE PARTENZA
403300970204    3C     BARNPG        IFNE      5
403400970204     C     BARNPG        ANDNE     1
403500970204     C     BARSPG        ANDNE     'P'
403600970204     C                   EXSR      CRTANM
403700970204   X3C                   ELSE
403800970204     C* SE COLLO     SPUNTATO NELLE CATEGORIE PARTENZA
403900970204     C                   EXSR      CHIUAN
404000970204    3C                   ENDIF
404100970204     C*
404200970204   X2C                   ELSE
404300970204     C* COLLO SPUNTATO DA UN 2 LIVELLO IN PARTENZA
404400970204     C* SE COLLO NON SPUNTATO NELLE CATEGORIE PARTENZA
404500970204    3C     BARNPG        IFNE      5
404600970204     C     BARSPG        ANDNE     'P'
404700970204     C                   EXSR      CRTANM
404800970204     C                   ELSE
404900970204     C* SE COLLO     SPUNTATO NELLE CATEGORIE PARTENZA
405000970204     C                   EXSR      CHIUAN
405100970204    3C                   ENDIF
405200970204    2C                   ENDIF
405300970204    1C                   ENDIF
405400970204     C                   ENDSR
405500970204     C**************************************************************************
405600970204     C* ANOMALIE CHE CREO SOLO SE HA SPARATO LA LINEA DI ARRIVO DEL COL
405700970204     C**************************************************************************
405800970204     C     ANMARR        BEGSR
405900970204     C*
406000970204     C* COLLO/BOLLA ARRIVI PRESENTE
406100970204    1C  N05WBOAR         IFEQ      'S'
406200990126     C                   MOVEL     'N'           WCAT4             1
406300990126     C*     GESTISCO IL RITROVAMENTO COLLO
406400990126     C                   MOVEL     'S'           WRITRO
406500020204     C* NON GESTISCO LA CREAZIONE ANOMALIE CHE GIA' FACCIO IN CHIUAN
406600990126     C                   MOVEL     'N'           WCREAN
406700990126     C                   EXSR      CTRDAN
406800970204     C*
406900970204    2C     WATDCM        IFEQ      0
407000970204     C     BARDFV        ORGT      WATDCM
407100970204     C*
407200970204    3C     WABNDC        IFNE      0
407300970204     C     WATDCM        ANDEQ     0
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
499300091209      **
499400020131    3C     ARTDAM        IFEQ      0
499500020131     C                   MOVEL     BARDFV        ARTDAM
499600020131     C                   MOVEL     'S'           WAGGF             1
499700020131     C                   MOVEL     BARNPS        ARTNAP
499800050509     c* Imposto flag ARTFL2= 'P' se catagoria di spunta è tipica processo
499900050509     c*  operativo partenza
500000050509     c                   if        barnpg=5 or (barnpg=1 and wdefl=' ') or
500100050509     c                             (barnpg=3  and barspg='P')
500200050509     c                   eval      artfl2='P'
500300050509     c                   else
500400050509     c                   eval      artfl2=' '
500500050509     c                   endif
500600020131     C*
500700020131     C                   MOVEL     ' '           ARTDAB
500800020131     C                   SETON                                        03
500900020131     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
501000160624     c* ANOMALIE 60 E 61 ELIMINATE
501100160624     C***                EXSR      CRTA60
501200020131     C*
501300020131   X3C                   ELSE
501400020131    4C   05ARTNAP        IFEQ      BARNPS
501500020131     C                   MOVEL     BARDFV        ARTDAM
501600020131     C                   MOVEL     'S'           WAGGF             1
501700020131    4C                   ENDIF
501800020131     C*
501900020131     C* 'D' DI DISABILITAZIONE COLLO :
502000020131     C*   SE RISPUNTATO LANCIO PGM DI ABILITAZIONE X RIABILITARE BOLLA
502100020131    4C  N05ARTDAB        IFEQ      'S'
502200020131     C                   SETON                                        03
502300020131     C                   MOVEL     ' '           ARTDAB
502400040324     C                   MOVEL     'S'           WAGGF             1
502500020131    4C                   ENDIF
502600020131     C*
502700020131    4C  N05ARTNAP        IFNE      BARNPS
502800020131     C     RPS(Y)        ANDEQ     'L'
502900020131     C                   MOVEL     ARTNAP        ALF2              2
503000020131     C*
503100020131     C* CERCO RAGGRUPPAMENTO PISTOLA
503200020131     C                   EXSR      RICRAG
503300020131     C*
503400020131    5C     RPS(Z)        IFEQ      'M'
503500020131     C     RPS(Z)        OREQ      'A'
503600020131     C                   MOVEL     'S'           WAGGF             1
503700020131     C                   MOVEL     BARDFV        ARTDAM
503800020213     C                   MOVEL     BARNPS        ARTNAP
503900050509     c* Imposto flag ARTFL2= 'P' se catagoria di spunta è tipica processo
504000050509     c*  operativo partenza
504100050509     c                   if        barnpg=5 or (barnpg=1 and wdefl=' ') or
504200050509     c                             (barnpg=3  and barspg='P')
504300050509     c                   eval      artfl2='P'
504400050509     c                   else
504500050509     c                   eval      artfl2=' '
504600050509     c                   endif
504700020131     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
504800160624     c* anomalie 60 e 61 eliminate
504900160624     C***                EXSR      CRTA60
505000020131     C*
505100020131    5C                   ENDIF
505200020131    4C                   ENDIF
505300020131     C*
505400020131    3C                   END
505500020131     C*
505600020131     C                   Z-ADD     ARTDCM        WDCM
505700020131     C**
505800070208    3C     dls45flg2     IFNE      'A'
505900020131     C                   UPDATE    FNART000
506000020131     C                   ELSE
506100020131     C                   UNLOCK    FNART27L
506200020131    3C                   ENDIF
506300020131     C*
506400050223     C     KARB          CHAIN(N)  FNARB01L                           32
506500020131     C*
506600020131    3C     *IN32         IFEQ      *OFF
506700020131     C*
506800070208    4C     dls45flg2     IFNE      'A'
506900020131     C* SOLO SE PGM RICHIAMATO:
507000020131     C* SE VARIATA DATA DI ARRIVO SULLA BOLLA RIELABORO LA STATISTICA
507100020131    5C     *IN02         IFEQ      *ON
507200020131     C     ARTDAM        ANDNE     SAVDAM
507300020131    6C     ARTDAM        IFNE      WARDAM
507400020131     C     SAVDAM        ORNE      WSVDAM
507500020131     C                   EXSR      WRTSTA
507600020131     C                   MOVE      ARTDAM        WARDAM
507700020131     C                   MOVE      SAVDAM        WSVDAM
507800020131    6C                   END
507900020131    5C                   END
508000020131     C*
508100020131     C                   MOVEL     ARBNDC        WABNDC
508200040430     C                   MOVEL     ARBcca        WABcca
508300020131     C   03              MOVEL     'S'           WFBS
508400020131     C*
508500020131    4C                   ENDIF
508600020131     C**
508700020131     C** VEDO SE BOLLA GIACENTE
508800020131    4C     ARBFBC        IFEQ      'G'
508900050331     C     KGCA          CHAIN     Tigcp21L                           34
509000161007     c  n34              z-add     gcpmgc        gcpdgc
509100161007     c  n34              movel     gcpagc        gcpdgc
509200161007    5C  N34GCPFAS        IFLT      35
509300161007    5C     GCPFAS        oreq      36
509400161007     c* Verifico tramite driver se il collo dove essere in IMG
509500161007     c                   clear                   fnlgi1ds
509600161007     c                   eval      ilgi1aas=arbaas
509700161007     c                   eval      ilgi1lnp=arblnp
509800161007     c                   eval      ilgi1nrs=arbnrs
509900161007     c                   eval      ilgi1nsp=arbnsp
510000161007     c                   select
510100161007     c                   when      arbncp=arbncl
510200161007     c                   eval      ilgi1pkf=arbpkc
510300161007     c                   when      arbpkc>arbpkf
510400161007     c                   eval      ilgi1pkf=arbpkc
510500161007     c                   other
510600161007     c                   eval      ilgi1pkf=arbpkf
510700161007     c                   endsl
510800161007     c                   movel     fnlgi1ds      kpjbu
510900161007     c                   call      'FNLGI1R'
511000161007     c                   parm                    kpjba
511100161007     c                   parm                    tigcp00ds
511200161007     c                   movel     kpjbu         fnlgi1ds
511300161021     c* In questo caso la sped potrebbe essere sia in IMA che in IMG
511400170125     c*  quindi non devo dare alcun fuori posto con la nuova procedura
511500170125     c*  anche se ha Y (ovvero portato nel giorno in IMG:
511600170125     c*  potrebbe mancare un collo della sped spuntato succesivamente)
511700170125   5ac****               if        olgi1AMG='S'
511800170125     c
511900170125   5ac                   if        olgi1AMG='S' or olgi1AMG='Y'
512000161021     C                   MOVEL     'D'           WGIAC
512100161021  x5ac                   else
512200170125   5bc****               if        olgi1AMG='Y'  or olgi1inmg='S'
512300170125   5bc                   if        olgi1inmg='S'
512400161201     C                   MOVEL     'S'           WGIAC
512500161201   5bc                   endif
512600161201   5ac                   endif
512700161201     c
512800161201   5ac                   if        wgiac<>' '
512900020131     C* NON LA CONSIDERO GIACENTE SE IN QUESTO MOMENTO C'E' UNA BOLLA
513000020131     C*  FIGLIA IN CONSEGNA O TUTTE LE FIGLIE SONO BLOCCATE
513100020131     C                   MOVEL     'S'           WFIBLO            1
513200020131     C                   CLEAR                   WESFI
513300020131     C     KARB          SETLL     FNLBL02L
513400020131     C     KARB          READE     FNLBL02L                               29
513500020131     C**
513600020131    6C     *IN29         DOWEQ     *OFF
513700020131     C*
513800050223     C     KLBL          CHAIN(N)  FNARB01L                           28
513900020131    7C     *IN28         IFEQ      *OFF
514000020131     C                   MOVEL     'S'           WESFI             1
514100020131     C* FIGLIA IN CONSEGNA
514200020131     C* NON LA CONSDIERO SE E' CONSEGNATA
514300020131   7AC     ARBDCM        IFEQ      0
514400020131    8C     ARBNDC        IFNE      0
514500020131     C                   SETON                                        29
514600020131     C                   CLEAR                   WGIAC
514700020131   X8C                   ELSE
514800020131     C*
514900020131     C* SE NON E' NEMMENO BLOCCATA MEMORIZZO
515000020131    9C     ARBFBC        IFNE      'B'
515100020131     C                   MOVEL     'N'           WFIBLO
515200020131    9C                   ENDIF
515300020131    8C                   ENDIF
515400020131   7AC                   ENDIF
515500170125    7C                   ENDIF
515600020131     C*
515700020131     C  N29KARB          READE     FNLBL02L                               29
515800020131    6C                   ENDDO
515900020131     C*
516000020131     C* SE TUTTE LE FIGLIE SONO BLOCCATE --> ESCLUDO
516100161201    6C     WGIAC         IFNE      ' '
516200020131     C     WESFI         ANDEQ     'S'
516300020131     C     WFIBLO        ANDEQ     'S'
516400020131     C                   CLEAR                   WGIAC
516500020131    6C                   ENDIF
516600020131     C*
516700161021   5aC                   ENDIF
516800161007    5C                   ENDIF
516900020131    4C                   ENDIF
517000020131    3C                   ENDIF
517100020131     C*
517200020131     C** COLLO EXPORT: VEDO SE CREARE L'ANOMALIA COLLO INVIATO PARTNER
517300020131     C*  MA RISPARATO IN IMA O IN PARTENZA/ARRIVO
517400020131     C*  NON CREO ANOMALIA SE E' DATA =99999999 PERCHE' SI TRATTA DI
517500020131     C*  DIROTTAMENTO E QUINDI VALGONO I COLLI DELLA FIGLIA E NON
517600020131     C*  DELLA MAMMA
517700020212    3C     NTWBAR        IFEQ      'EEX'
517800060322    3C     NTWBAR        oreq      'DPD'
517900060322    3C     NTWBAR        oreq      'FED'
518000060322    4C     BARDFV        ifGT      ARTDTR
518100020131     C     ARTDTR        ANDGT     0
518200020131     C     ARTDTR        ANDNE     *HIVAL
518300020131     C** CREO ANOMALIA 600 O 620
518400020131     C                   MOVEL     'S'           W60X              1
518500060322    4C                   ENDIF
518600060322    3C                   ENDIF
518700020131    2C                   ENDIF
518800020131     C*
518900020131   X2C                   ELSE
519000020131     C*
519100070221   2AC     dls45FLG      IFNE      '6'
519200020131     C** CREO LE ANOMALIE DI MANCA BOLLA SOLO SE NON SONO PASSATI I
519300020131     C*  GG DIPULIZIA BOLLE ARRIVI (PER EVITARE MANCA RECORD ERRATI)
519400020131     C                   EXSR      CTRDAT
519500020131     C**
519600020131   2BC     WCRE10        IFEQ      'S'
519700130301     c     wlocalxco     andne     'S'
519800020131     C**
519900020131     C* ANOMALIA: "MANCA BOLLA" --> ANCHE PER SPEDIZIONE LOCALE SE
520000020131     C*  E' SPARATA NELLE CATEGORIE ARRIVI
520100020131     C*  IL FOGLIO DI DEFLUENZA E' COME UNA CATEGORIA ARRIVI
520200020131    3C  N05BARNPG        IFNE      5
520300020131     C     BARSPG        ANDNE     'P'
520400020131     C     BARSPG        ANDNE     'I'
520500020131     C     WTIBOL        ANDEQ     'L'
520600020131     C     WTIBOL        OREQ      'A'
520700020131     C* LA CATEGORIA 1 LA CONSIDERO SOLTANTO SE E' DEFLUENZA
520800020131     C*
520900020131    4C     BARNPG        IFNE      1
521000020131     C     WTIBOL        OREQ      'A'
521100020131     C     WDEFL         OREQ      '1'
521200020131     C                   Z-ADD     10            COMCAA
521300020131     C                   EXSR      CRTANM
521400020131     C* Se l'anomalia 10 esiste già a livello max. creo anom. 410 per
521500020131     C* segnalare il persistere del manca record
521600020131     C     WANM10        IFEQ      '1'
521700020131     C                   Z-ADD     410           COMCAA
521800020131     C                   EXSR      CRTANM
521900020131     C                   END
522000020131    4C                   ENDIF
522100020131    3C                   ENDIF
522200020131     C*
522300020131     C* SE LNA ESTERA VERIFICO SE C'E' EVENTO 2 PER CREARE ANOM.600/620
522400020212    3C     NTWBAR        IFEQ      'EEX'
522500060322    3C     NTWBAR        oreq      'DPD'
522600060322    3C     NTWBAR        oreq      'FED'
522700020131     C*
522800020131     C* SE LNA ESTERA APRO FILE FNEVS02L
522900020131    4C     WAPER         IFEQ      ' '
523000020131     C                   OPEN      FNEVS02L
523100020131     C                   MOVEL     '1'           WAPER             1
523200020131    4C                   ENDIF
523300020131     C*
523400020131     C                   MOVEL     'E2 '         KCEV
523500020131     C     KEVS          SETGT     FNEVS02L
523600020131     C     KEVS          READPE    FNEVS02L                               34
523700020131    4C     *IN34         IFEQ      *OFF
523800020131     C     EVSDEV        ANDLT     BARDFV
523900020131     C* CREO ANOMALIA 600 E 620
524000020131     C                   MOVEL     'S'           W60X
524100020131    4C                   ENDIF
524200020131    3C                   ENDIF
524300020131   2BC                   ENDIF
524400020131   2AC                   ENDIF
524500020131    2C                   ENDIF
524600020131     C**
524700020201     C     ENDART        ENDSR
524800020213     C**************************************************************************
524900020213     C* AGGIORNO LE BOLLE TRANSITO
525000020213     C**************************************************************************
525100020213     C     AGGBTT        BEGSR
525200020213     C                   MOVEL     'T'           WTIBOL
525300020214     C* MEMORIZZO CHE C'E' BOLLA TRANSITO, MI SERVE PER CRTA50
525400020214     C                   MOVE      BTTDFV        COMDFV
525500020213     C* LA PRIMA VOLTA MEMORIZZO LA SPEDIZIONE
525600020213     C                   MOVEL     'T'           BTTTBO
525700020213     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
525800020213     C* NE PRECEDENTE
525900020213     C     BTTSPE        IFNE      SAVSPE
526000020213     C     WAGGF         IFEQ      'S'
526100020213     C                   EXSR      WRTAGB
526200020213     C                   CLEAR                   WAGGF
526300020213     C                   ENDIF
526400020213     C                   MOVEL     BTTSPE        SAVSPE
526500020213     C                   ENDIF
526600020213     C*
526700020213    5C     BARCAN        IFNE      ' '
526800020213     C     BARCAN        ANDNE     'E'
526900020213     C                   MOVEL     BARCAN        BTTCAN
527000020213     C                   MOVEL     'S'           WAGGF             1
527100020213    5C                   END
527200020213     C*
527300020213    5C     BARVUC        IFGT      0
527400020213     C*
527500020213    6C     BTTVUC        IFEQ      0
527600041013     c                   movel     'S'           WFVC
527700020213     C                   Z-ADD     BARVUC        BTTVUC
527800020213     C                   MOVEL     'S'           WAGGF             1
527900020213     C                   MOVEL     'T'           BTTFVC
528000020213    6C                   END
528100020213     C*
528200020213    5C                   END
528300020213     C*PESO DA CML
528400020213    5C     BARPUC        IFGT      0
528500020213     C*
528600020213    6C     BTTPUC        IFEQ      0
528700041013     c                   movel     'S'           WFPC
528800020213     C                   Z-ADD     BARPUC        BTTPUC
528900020213     C                   MOVEL     'S'           WAGGF             1
529000020213     C                   MOVEL     'T'           BTTFPC
529100020213    6C                   END
529200020213     C*
529300020213    5C                   END
529400020213     C*
529500020213    5C     BTTDET        IFEQ      0
529600020213     C                   MOVEL     'S'           WAGGF             1
529700020213     C                   MOVEL     BARDFV        BTTDET
529800020213     C                   MOVEL     BARNPS        BTTPET
529900020213     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
530000020213     C*  SOLO SE NON E' PISTOLA FASULLA
530100160624     C**                 MOVEL     'TT'          WCCH
530200160624     C**   OPS(Y)        IFLT      040
530300160624     C**                 EXSR      CRTA60
530400160624     C**                 CLEAR                   WCCH
530500160624     C**                 ENDIF
530600020213     C*
530700020213   X5C                   ELSE
530800020213     C*
530900020213    6C   05BTTPET        IFEQ      BARNPS
531000020213     C                   MOVEL     'S'           WAGGF             1
531100020213     C                   MOVEL     BARDFV        BTTDET
531200020213    6C                   END
531300020213     C*
531400020213    6C  N05BTTPET        IFNE      BARNPS
531500020213     C     RPS(Y)        ANDEQ     'L'
531600020213     C                   MOVEL     BTTPET        ALF2
531700020213     C*
531800020213     C* CERCO RAGGRUPPAMENTO PISTOLA
531900020213     C                   EXSR      RICRAG
532000020213     C*
532100020213    7C     RPS(Z)        IFEQ      'M'
532200020213     C     RPS(Z)        OREQ      'A'
532300020213     C                   MOVEL     BARDFV        BTTDET
532400020213     C                   MOVEL     'S'           WAGGF             1
532500020213     C                   MOVEL     BARNPS        BTTPET
532600160624     c
532700020213     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
532800160624     c* Anomalia 61 eliminara
532900160624     C**                 MOVEL     'TT'          WCCH
533000160624     C**                 EXSR      CRTA60
533100160624     C**                 CLEAR                   WCCH
533200020213    7C                   END
533300020213    6C                   END
533400020213     C*
533500020213    5C                   END
533600020213     C*
533700020213     C                   UPDATE    FNBTT000
533800020213     C*
533900020213     C* SE INVENTARIO O IN CONSEGNA ANOMALIA: FUORI POSTO DEL TRANSITO
534000020213    5C  N05BARNPG        IFEQ      3
534100020213     C     BARSPG        ANDNE     'P'
534200020213     C     BARNPG        OREQ      4
534300020213     C     BARNPG        OREQ      8
534400020213     C                   Z-ADD     125           COMCAA
534500020213     C                   EXSR      CRTANM
534600020213    5C                   END
534700040430     c* Se il collo è in transito, posso chiudere i fuori posto in partenza
534800040430     C                   Z-ADD     120           COMCAA
534900040430     C                   EXSR      chiuan
535000110103     c*
535100040422     c*Chiudo eventuale anomalia di disguido presente sul collo, se
535200040422     c*  generate da un 2 livello dell'area di partenza
535300040503     C* SOLO SE SPUNTA DI PISTOLA REALE
535400040503     c                   if        fsl(y)<>'F'
535500040428     c                   eval      comcaa=200
535600040422     C*
535700040503     C     Kanm          chain(N)  FNANM000
535800040428     c                   if        %found(fnanm02l)
535900040428     C                   CLEAR                   DSLV55
536000040428     C                   MOVEL     'P'           D55TPT
536100040428     C                   Z-ADD     anmfle        D55LIN
536200040428     C                   Z-ADD     bardfv        D55DRF
536300040428     C                   EXSR      FNLV55
536400040428     c                   if        bpes=d55tfp
536500040428     c                   exsr      chiuan
536600040428     c                   endif
536700040428     c                   endif
536800040503     c                   endif
536900041012     c*
537000041012     c* Aggiorno anche la bolla arrivo con i dati del transito
537100041012     c                   EXSR      AGGARRIVO
537200110103     c*
537300020213     C                   ENDSR
537400020201     C**************************************************************************
537500020201     C* CONTROLLO COSA POSSO AGGIORNARE IN BASE A CHI SONO
537600020201     C**************************************************************************
537700020201     C     CTRAGG        BEGSR
537800020204     C                   CLEAR                   WAGPAR
537900020204     C                   CLEAR                   WAGARR
538000150622     C                   CLEAR                   Wbsptp
538100150622     C                   CLEAR                   Wbspspu
538200020213     C**
538300020213     C** CERCO TERMINAL DELLA LINEA DI ARRIVO
538400020213     C                   EXSR      TERARR
538500050603     c                   eval      Usalnatfa=lnatfa
538600020204     C*
538700070208     c*         Non considero se c'e'bolla transito
538800070208     c     wesbtt        ifne      'S'
538900150622     C* SE NON C'È BOLLA TRANSITO VERIFICO SE COLLI CHE PARTONO DA
539000150817     C*  più filiali se ho trovato la bolla in partenza non partita
539100150817     c*  e con data di entrata = 0
539200150622     c                   if        wesblt='S' and Blpdt1=0
539300150622     C                             and blpdse=0
539400150818     c* se si tratta di una spunta tipica partenza
539500150818     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
539600150818     c                             or barnpg=1
539700150622     c                   exsr      sr_fibsp
539800150622     c                   if        wbsptp<>' '      and
539900150622     c                             %lookup(bpes:sbsp)>0
540000150622     c                   eval      wbspspu='S'
540100150818     C                   MOVEL     'P'           WTIBOL            1
540200150622     c                   endif
540300150622     c                   endif
540400150818     c                   endif
540500020204     C*
540600020201     C*  1) P.O. EFFETTUA SPUNTA = LNP SPUNTA (DA BOLLA)
540700020204    2C     BPES          IFEQ      LNPB
540800020201     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.PARTENZA DELLA LNP SPUNTA
540900020201     C*     IN BASE ALLA DATA SPEDIZIONE SE TROVATA LA BOLLA
541000020204     C     BPES          OREQ      LNPTFP
541100020204     C                   MOVEL     'S'           WAGPAR            1
541200020204   X2C                   ELSE
541300020204     C*  3) TER.PARTENZA P.O. EFFETTUA SPUNTA (IN BASE ALLA DATA SPED
541400020204     C*     SE TROVATA LA BOLLA) = TER.PARTENZA LNP SPUNTA SEMPRE IN
541500020204     C*     BASE ALLA DATA SPEDIZIONE SE TROVATA BOLLA
541600021125     C                   MOVEL     'P'           WTITER            1
541700020201     C                   EXSR      TERPES
541800020215    3C     D55TFP        IFEQ      LNPTFP
541900020204     C                   MOVEL     'S'           WAGPAR            1
542000020204    3C                   ENDIF
542100020204    2C                   ENDIF
542200020201    1C                   ENDIF
542300020204     C**
542400020204     C** NON AGGIORNO BOLLE ARRIVO SE SPUNTA DELL'AREA PARTENZA,
542500020204     C*   SPUNTA TIPICA PARTENZA E P.O. SPUNTA = LNA SPUNTA O TFA SPU
542600021125     c* controllo solo se spunta tipica partenza: l'arrivo non mi
542700021125     c*  interessa. non so perchè lo avevo preso in considerazione!!
542800150817     c
542900150817     c* Non aggiorno nemmeno se bolla da "trasferire"
543000020204    1C     WAGPAR        IFEQ      'S'
543100150817     c     wbspspu       oreq      'S'
543200150817     c
543300021125     C**   BPES          ifeq      lnpb
543400021125    2C**   BPES          OREQ      BARLNA
543500021125     C**   BPES          OREQ      LNATFA
543600021125     C**   pestfa        OREQ      LNATFA
543700020204     C* PARTENZA NON DEFLUENZA
543800020204    3C     BARNPG        IFEQ      1
543900020204     C     WDEFL         ANDEQ     '0'
544000020204     C* ENTRATA
544100020204     C     BARNPG        OREQ      5
544200020204     C* I.M.P.
544300020204     C     BARNPG        OREQ      3
544400020204     C     BARSPG        ANDEQ     'P'
544500020204     C* I.M.I.
544600020204     C     BARNPG        OREQ      3
544700020218     C     BARSPG        ANDEQ     'I'
544800020204     C                   GOTO      NOAGAR
544900020204    3C                   ENDIF
545000021125    2C**                 ENDIF
545100020204    1C                   ENDIF
545200020204     C**
545300021022     c     wesbtt        ifne      'S'
545400020204     C* AGGIORNO BOLLE ARRIVO SE :
545500020204     C*  1) P.O. EFFETTUA SPUNTA = LNA SPUNTA
545600020205    1C                   SELECT
545700020204     C     BPES          WHENEQ    BARLNA
545800020204     C                   MOVEL     'S'           WAGARR            1
545900020205    2C     WAGPAR        IFEQ      'S'
546000020204     C                   MOVEL     'A'           WWFVC
546100020204     C                   MOVEL     'A'           WSPLOC            1
546200020205    2C                   ENDIF
546300020204     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.ARRIVO   DELLA LNA SPUNTA
546400020204     C*     IN BASE ALLA DATA SPUNTA
546500020204     C     BPES          WHENEQ    LNATFA
546600050603     C     BPES          oreq      bolTFA
546700020204     C                   MOVEL     'S'           WAGARR            1
546800020205    2C     WAGPAR        IFEQ      'S'
546900020204     C                   MOVEL     'A'           WWFVC
547000020204     C                   MOVEL     'T'           WSPLOC            1
547100020205    2C                   ENDIF
547200050603     c* imposto il terminal di arrivo =BPES
547300050603     c                   eval      Usalnatfa=BPES
547400020204   X1C                   OTHER
547500020204     C*  3) TER.ARRIVO P.O. EFFETTUA SPUNTA
547600020204     C*     = TER.ARRIVO LNA SPUNTA IN BASE ALLA DATA SPUNTA
547700020204    2C     PESTFA        IFEQ      LNATFA
547800050603    2C     PESTFA        oreq      bolTFA
547900020204     C                   MOVEL     'S'           WAGARR            1
548000020204     C                   MOVEL     'D'           WSPLOC
548100020204     C                   MOVEL     'A'           WWFVC
548200050603     c* imposto il terminal di arrivo =ter arrivo di BPES
548300050603     c                   eval      Usalnatfa=pestfa
548400110117   x2c                   else
548500110117     c
548600110117     c*  4) Se nessuna è vera ma chi spara è la capostipite della lna-£6
548700110117     c*     anche se è una incongruenza la considero valida ai fini dell'arrivo
548800110117     C* CARICO LA L6 DELLA FILIALE GESTIONE
548900110117    3C     BPES          IFNE      SAVPES
549000110117     C                   MOVEL     BPES          SAVPES
549100110117     C                   EXSR      CARL6
549200110117    3C                   ENDIF
549300110117    c
549400110117     C     BARLNA        LOOKUP    L6                                     34
549500110117    3c                   if        *in34
549600110117     C                   MOVEL     'S'           WAGARR            1
549700110117    4C     WAGPAR        IFEQ      'S'
549800110117     C                   MOVEL     'A'           WWFVC
549900110117     C                   MOVEL     'A'           WSPLOC            1
550000110117    4C                   ENDIF
550100110117    3C                   ENDIF
550200110117     c
550300020205    2C                   ENDIF
550400020205    1C                   ENDSL
550500021022     c*
550600021022    2C                   endif
550700020204     C**
550800070208     C     NOAGAR        ENDSR
550900150622     C**************************************************************************
551000150622     C*  vedo se lnp o ksc confermabile da più linee
551100150622     C**************************************************************************
551200150622     C     sr_fibsp      BEGSR
551300150622     c                   eval      WBspTp=*blanks
551400150622    2c                   if        blpccm>0
551500150622     c                   eval      w_clibo=blpccm
551600150622     c                   else
551700150622     c                   eval      w_clibo=blpksc
551800150622    2c                   endif
551900150622     c     w_clibo       chain     fibsp02l
552000150622     c                   if        %found(fibsp02l) and bspfpa='S'
552100150622     c                   eval      wBspTp='K'
552200150622     c****               if        not %found(fibsp02l) or bspfpa<>'S'
552300150622     c                   else
552400150817     c     LNPB          chain     fibsp01l
552500150622     c                   if        %found(fibsp01l) and bspfpa='S'
552600150622     c                   eval      wBspTp='L'
552700150622     c                   endif
552800150622     c                   endif
552900150622     c                   endsr
553000020201     C**************************************************************************
553100020201     C* TERMINAL DI PARTENZA E ARRIVO DEL P.O. EFFETTUA SPUNTA
553200020201     C**************************************************************************
553300020205     C     TERPES        BEGSR
553400020201     C* IMPOSTO TERMINAL PARTENZA E ARRIVO
553500020201     C                   CLEAR                   DSLV55
553600020205     C                   MOVEL     WTITER        D55TPT
553700020205     C                   MOVEL     LNPB          D55LNP
553800020205     C                   MOVEL     WDTRIF        D55DRF
553900020205     C                   MOVE      BPES          D55LIN
554000020205     C                   EXSR      FNLV55
554100020205     C                   ENDSR
554200020205     C**************************************************************************
554300020205     C* CALL A PGM FNLV55R
554400020205     C**************************************************************************
554500020205     C     FNLV55        BEGSR
554600020201     C                   CALL      'FNLV55R'
554700020201     C                   PARM                    DSLV55
554800020201     C**
554900020205     C     D55ERR        IFNE      ' '
555000020205     C                   MOVE      D55LIN        D55TFA
555100020205     C                   MOVE      D55LIN        D55TFP
555200020201     C                   END
555300020201     C                   ENDSR
555400041012     C**************************************************************************
555500041012     C* AGGIORNO CODICE ANOMALIA
555600041012     C**************************************************************************
555700041012     c     AGGCAN        BEGSR
555800041012    3C     BARCAN        IFNE      ' '
555900041012     C     BARCAN        ANDNE     'E'
556000070508     c     barcan        andne     artcan
556100041012     C**
556200070508     c* Aggiorno la testata e quindi scrivo AGB solo se artcan
556300070508     c*  è vuoto prima di questo aggiornamento
556400070508     c                   if        artcan=' '
556500041012     C                   MOVEL     'S'           WAGGF             1
556600070508     c                   endif
556700070508     C                   MOVEL     BARCAN        ARTCAN
556800041012     c                   endif
556900041012     c                   endsr
557000041012     C**************************************************************************
557100041012     C* AGGIORNO volume cml
557200041012     C**************************************************************************
557300041105     C     aGGVOL        BEGSR
557400041012    3C                   SELECT
557500041012    3C                   when      BARVUC = *zeros
557600041012      * NON SOSTITUISCO IL VOLUME ARRIVO SE LNA FEDEX
557700041012    3C                   when      ARTFVC = 'A' and ARTVUC > *zeros
557800041012    3C                                          and NTWBAR = 'FED'
557900041012      * IL VOLUME ARRIVO O TRANSITO LI SOSTITUISCO SE QUELLO DELLA
558000041012      * SPUNTA E' RILEVATO DALLA PARTENZA
558100041012    3C                   when       ARTVUC = *zeros  or
558200041012    3C                             (ARTVUC > *zeros and ARTFVC <> 'P'
558300041012     C                                              and WWFVC  =  'P')   or
558400041012    3C                             (ARTVUC > *zeros and ARTFVC =  'A'
558500041012     C                                              and WWFVC  =  'T')
558600041012     c                   movel     'S'           wfvc
558700041012     C                   MOVEL     'S'           WAGGF             1
558800041012     C                   Z-ADD     BARVUC        ARTVUC
558900041012     C                   MOVEL     WWFVC         ARTFVC
559000041012    3c                   ENDSL
559100041012     c                   ENDSR
559200041012     C**************************************************************************
559300041012     C* AGGIORNO Peso   cml
559400041012     C**************************************************************************
559500041012     c     AGGPES        BEGSR
559600041012    3C     BARPUC        IFGT      0
559700041012    4C     ARTPUC        IFEQ      0
559800041012     C* IL PESO ARRIVO O TRANSITO LI SOSTITUISCO SE QUELLO DELLA
559900041012     C* SPUNTA E' RILEVATO DALLA PARTENZA
560000041012     C     ARTPUC        ORNE      0
560100041012     C     WWFVC         ANDEQ     'P'
560200041012     C     ARTFPC        ANDNE     'P'
560300041012     C     ARTPUC        ORNE      0
560400041012     C     WWFVC         ANDEQ     'T'
560500041012     C     ARTFPC        ANDEQ     'A'
560600041012     c                   movel     'S'           wfpc
560700041012     C                   MOVEL     'S'           WAGGF             1
560800041012     C                   Z-ADD     BARPUC        ARTPUC
560900041012     C                   MOVEL     WWFVC         ARTFPC
561000041012    4C                   END
561100041012     C*
561200041012    3C                   END
561300041012     c                   ENDSR
561400091209     C**************************************************************************
561500091209     c*    Aggiorno fiar5 o lo scrivo se inesistente
561600091209     C**************************************************************************
561700091209     c     AggioFIAR5    BEGSR
561800091209     c
561900091209     c                   if        %found(fiar501l)
562000101129     c                   movel     datReale      §ar5arrdt
562100101129     c                   movel     oraReale      §ar5arrhm
562200091209     c                   movel(p)  dar5gen       ar5uni
562300091209      * se devo trasmettere alla sede sfleggo per la trasmissione
562400091209     c                   If        §Ar5Trse = 'S'
562500091209     c                   Clear                   Ar5Ft2
562600091209     c                   EndIf
562700091209     c                   update    fiar5000
562800091209      * Scrivo Fiar5
562900091209   x2c                   Else
563000091209     c                   Clear                   Fiar5000
563100091209     c                   Eval      Ar5Aas = ArtAas
563200091209     c                   Eval      Ar5Lnp = ArtLnp
563300091209     c                   Eval      Ar5Nrs = ArtNrs
563400091209     c                   Eval      Ar5Nsp = ArtNsp
563500091209     c                   Eval      Ar5Trd = 'GEN'
563600091209     c                   Move      WDATE1        Ar5Dac
563700091209     c                   time                    Ar5Orc
563800091209     c                   Clear                   Dar5Gen
563900101129     c                   movel     datReale      §ar5arrdt
564000101129     c                   movel     oraReale      §ar5arrhm
564100091209     c                   Movel(p)  DAr5Gen       Ar5Uni
564200091209     c                   Write     Fiar5000
564300091209    2c                   EndIf
564400091209     c                   ENDSR
564500041012     C**************************************************************************
564600041012     C* AGGIORNO anche la bolla di arrivo
564700041012     C**************************************************************************
564800041012     c     AGGARRIVO     BEGSR
564900041012     c* Aggiorno anche bolla  arrivi con dati del transito
565000130228     c                   clear                   aggioart          1
565100041012     C     KBAR          CHAIN     FNART27L
565200130228    1c                   if        %found(fnart27l)
565300041012     C                   MOVEL     'A'           ARTTBO
565400130228     c                   eval      Aggioart='S'
565500041012     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
565600041012     C* NE PRECEDENTE
565700041012     C     ARTSPE        IFNE      SAVSPE
565800041012     C     WAGGF         IFEQ      'S'
565900041012     C                   EXSR      WRTAGB
566000041012     C                   CLEAR                   WAGGF
566100041012     C                   ENDIF
566200041012     C                   MOVEL     ARTSPE        SAVSPE
566300041012     C                   ENDIF
566400110103     c*
566500041012     c* Codice anomalia: tengo sempre l'ultima anomalia
566600041012     C                   EXSR      AGGCAN
566700041012     C
566800041012     C* AGGIORNO DATA ENTRATA AL TRANSITO
566900041012     C                   Z-ADD     barDFV        ARTDET
567000041012     C                   MOVEL     barNPS        ARTPET
567100041012     C                   MOVEL     'S'           WAGGF             1
567200041012     C     barFGS        IFNE      ARTFLP
567300041012     C* FILIALE IN GESTIONE <> FILIALE TRANSITO
567400041012     C                   MOVEL     barFGS        ARTFLP
567500041012     C                   MOVEL     *ZEROS        ARTDUT
567600041012     C                   MOVEL     *ZEROS        ARTPUT
567700041012     C                   END
567800041012     c* peso e volume
567900041012     C     WWFVC         IFEQ      *BLANKS
568000041012     C                   MOVEL     'T'           WWFVC
568100041012     C                   END
568200041012     c                   EXSR      AGGVOL
568300041012     c                   EXSR      AGGpes
568400041012     c                   update    fnart000
568500130228     c
568600041013     c* Se trovata bolla, eseguo solo per disguido la CRTA60
568700160624     c* anomalie 60 e 61 eliminate
568800050322     c                   eval      savesart=wesart
568900041013     c                   movel     'S'           wesart
569000160624     c**                 if        wtibol='D'
569100160624     c**                 eval      wsavflg=dls45FLG
569200160624     c**                 eval      dls45FLG='6'
569300160624     c**                 exsr      CRTA60
569400160624     c**                 eval      dls45FLG=wsavflg
569500160624     c**                 endif
569600130228     c
569700130228    1c                   endif
569800041013     c* Chiudo una eventuale anomalia 15 aperta controllando le date anomali
569900041013     c*  e spunta
570000041013     c                   z-add     15            comcaa
570100041013     c                   eval      wcontrdfv='S'
570200041013     c                   eval      wstess='S'
570300041013     c                   eval      wchiu='S'
570400041013     c                   exsr      chiuan
570500130228     c
570600130228    1c                   if        Aggioart='S'
570700130228     c                   eval      wesart=savesart
570800130228     c                   endif
570900041013     c*
571000041012     c                   ENDSR
571100070208     C**************************************************************************
571200070208     C* Routine iniziale
571300070208     C**************************************************************************
571400070208     c     *inzsr        BEGSR
571500070208     C     KBAR          KLIST
571600070208     C                   KFLD                    BARLNP
571700070208     C                   KFLD                    BARLNA
571800070208     C                   KFLD                    BARNRS
571900070208     C                   KFLD                    BARNSC
572000070208     C     KBAR37        KLIST
572100070208     C                   KFLD                    bpes
572200070208     C                   KFLD                    BARLNP
572300070208     C                   KFLD                    BARLNA
572400070208     C                   KFLD                    BARNRS
572500070208     C                   KFLD                    BARNSC
572600160721     C     KBAR_t        KLIST
572700160721     C                   KFLD                    savflp
572800160721     C                   KFLD                    BARLNP
572900160721     C                   KFLD                    BARLNA
573000160721     C                   KFLD                    BARNRS
573100160721     C                   KFLD                    BARNSC
573200070208     C     KBAR1         KLIST
573300070208     C                   KFLD                    BARNPG
573400070208     C                   KFLD                    BARNfv
573500070208     C                   KFLD                    BARfgs
573600070208     C                   KFLD                    BARLNP
573700070208     C                   KFLD                    BARLNA
573800070208     C                   KFLD                    BARNRS
573900070208     C                   KFLD                    BARNSC
574000140616     C     KBAR_qd4      KLIST
574100140616     C                   KFLD                    kdel_npg
574200140616     C                   KFLD                    BARLNP
574300140616     C                   KFLD                    BARLNA
574400140616     C                   KFLD                    BARNRS
574500140616     C                   KFLD                    BARNSC
574600140616     C                   KFLD                    BARfgs
574700140616     C                   KFLD                    qd_dfs
574800140616     C                   KFLD                    qd_hms
574900070208     C     KBAR2         KLIST
575000070208     C                   KFLD                    BARNPG
575100070208     C                   KFLD                    BARLNP
575200070208     C                   KFLD                    BARLNA
575300070208     C                   KFLD                    BARNRS
575400070208     C                   KFLD                    BARNSC
575500070208     C                   KFLD                    BARfgs
575600140613     C     KBAR2_d       KLIST
575700140613     C                   KFLD                    kdel_npg
575800140613     C                   KFLD                    BARLNP
575900140613     C                   KFLD                    BARLNA
576000140613     C                   KFLD                    BARNRS
576100140613     C                   KFLD                    BARNSC
576200140613     C                   KFLD                    BARfgs
576300140616     C                   KFLD                    wdfs
576400070208     C     KBAR3         KLIST
576500070208     C                   KFLD                    WNPG
576600070208     C                   KFLD                    BARLNP
576700070208     C                   KFLD                    BARLNA
576800070208     C                   KFLD                    BARNRS
576900070208     C                   KFLD                    BARNSC
577000070208     C     KBRV7         KLIST
577100070208     C                   KFLD                    BARLNP
577200070208     C                   KFLD                    BARLNA
577300070208     C                   KFLD                    BARNRS
577400070208     C                   KFLD                    BARNSC
577500070208     C     KARB          KLIST
577600070208     C                   KFLD                    ARTAAS
577700070208     C                   KFLD                    ARTLNP
577800070208     C                   KFLD                    ARTNRS
577900070208     C                   KFLD                    ARTNSP
578000070208     C     KGCA          KLIST
578100070208     C                   KFLD                    ARTAAS
578200070208     C                   KFLD                    ARTLNP
578300070208     C                   KFLD                    ARTNRS
578400070208     C                   KFLD                    ARTNSP
578500070208     C                   KFLD                    KFRG
578600070208     C     KSAV          KLIST
578700070208     C                   KFLD                    SAVTBO
578800070208     C                   KFLD                    SAVAAS
578900070208     C                   KFLD                    SAVLNP
579000070208     C                   KFLD                    SAVNRS
579100070208     C                   KFLD                    SAVNSP
579200070208     C                   KFLD                    KAGB
579300070208     C     KEAGB         KLIST
579400070208     C                   KFLD                    WTBO
579500070208     C                   KFLD                    WDAAS
579600070208     C                   KFLD                    WDLNP
579700070208     C                   KFLD                    WDNRS
579800070208     C                   KFLD                    WDNSP
579900070208     C                   KFLD                    KAGB
580000070208     C     KBLT          KLIST
580100070208     C                   KFLD                    BLTAAS
580200070208     C                   KFLD                    BLTLNP
580300070208     C                   KFLD                    BLTNRS
580400070208     C                   KFLD                    BLTNSP
580500070208     C     KBTT          KLIST
580600070208     C                   KFLD                    BTTflp
580700070208     C                   KFLD                    BTTAAS
580800070208     C                   KFLD                    BTTLNP
580900070208     C                   KFLD                    BTTNRS
581000070208     C                   KFLD                    BTTNSP
581100070208     C     KANM          KLIST
581200070208     C                   KFLD                    BARLNP
581300070208     C                   KFLD                    BARLNA
581400070208     C                   KFLD                    BARNRS
581500070208     C                   KFLD                    BARNSC
581600070208     C                   KFLD                    COMCAA            3 0
581700070208     C     KEVS          KLIST
581800070208     C                   KFLD                    BARLNP
581900070208     C                   KFLD                    BARLNA
582000070208     C                   KFLD                    BARNRS
582100070208     C                   KFLD                    BARNSC
582200070208     C                   KFLD                    KCEV
582300070208     C     KFVV1         KLIST
582400070208     C                   KFLD                    WNPG
582500070208     C                   KFLD                    WNFV
582600070208     C                   KFLD                    WFGS
582700070208     C     KFVV          KLIST
582800070208     C                   KFLD                    WNPG
582900070208     C                   KFLD                    wdata
583000070208     C                   KFLD                    KTFA2
583100070208     C     KFVA1         KLIST
583200070208     C                   KFLD                    WFGS
583300070208     C                   KFLD                    WNFV
583400070208     C     KFGV1         KLIST
583500070208     C                   KFLD                    WNFV
583600070208     C                   KFLD                    tfpfgs
583700070208     c     KFGV2         KLIST
583800070208     C                   KFLD                    lnptfp
583900070208     C                   KFLD                    bardfv
584000070208     C     KFVA4         KLIST
584100070208     C                   KFLD                    KNPG4
584200070208     C                   KFLD                    KNFV4
584300070208     C                   KFLD                    KFGS4
584400070208     C     KFWA          KLIST
584500070208     C                   KFLD                    FVALNP
584600070208     C                   KFLD                    FVANFV
584700070208     C                   KFLD                    FVALAI
584800070208     C     KTAB          KLIST
584900070208     C                   KFLD                    CODUT
585000070208     C                   KFLD                    COD
585100070208     C                   KFLD                    KEY
585200070208     C     KTAB2         KLIST
585300070208     C                   KFLD                    CODUT
585400070208     C                   KFLD                    COD
585500130228     C     KTBE2         KLIST
585600130228     C                   KFLD                    KCOD
585700130228     C                   KFLD                    KKE1
585800130228     C                   KFLD                    KKE2
585900070208     C*
586000070208     C     KLBL          KLIST
586100070208     C                   KFLD                    LBLAAN
586200070208     C                   KFLD                    LBLLPN
586300070208     C                   KFLD                    LBLNRN
586400070208     C                   KFLD                    LBLNSN
586500070208     C     KDCT          KLIST
586600070208     C                   KFLD                    DCDAAC
586700070208     C                   KFLD                    DCDFIL
586800070208     C                   KFLD                    DCDNCA
586900070208     C     KBRVE2        KLIST
587000070208     C                   KFLD                    BRVNPG
587100070208     C                   KFLD                    BRVLNP
587200070208     C                   KFLD                    BRVLNA
587300070208     C                   KFLD                    BRVNRS
587400070208     C                   KFLD                    BRVNSC
587500070710     C     KBRVE1        KLIST
587600070710     C                   KFLD                    BRVNPG
587700070710     C                   KFLD                    BRVNfv
587800070710     C                   KFLD                    BRVfgs
587900070710     C                   KFLD                    BRVLNP
588000070710     C                   KFLD                    BRVLNA
588100070710     C                   KFLD                    BRVNRS
588200070710     C                   KFLD                    BRVNSC
588300070208     C     Kar5          KLIST
588400070208     C                   KFLD                    anmaas
588500070208     C                   KFLD                    anmLNp
588600070208     C                   KFLD                    anmNRS
588700070208     C                   KFLD                    anmnsp
588800070208     C                   KFLD                    Ktrd
588900091211     C     Kar52         KLIST
589000091211     C                   KFLD                    artaas
589100091211     C                   KFLD                    artLNp
589200091211     C                   KFLD                    artNRS
589300091211     C                   KFLD                    artnsp
589400091211     C                   KFLD                    Ktrd
589500070208     C*
589600070208     C     *LIKE         DEFINE    FVAdfv        Wpul050
589700070208     C     *LIKE         DEFINE    FVALNP        WFVALNP
589800070208     C     *LIKE         DEFINE    FVANFV        WFVANFV
589900070208     C     *LIKE         DEFINE    FVADFV        WFVADFV
590000070208     C     *LIKE         DEFINE    bardfv        Wdata
590100070208     C     *LIKE         DEFINE    wfgs          WWWfgs
590200070208     C     *LIKE         DEFINE    wfgs          tfpfgs
590300070208     C     *LIKE         DEFINE    wfgs          savfgs
590400070208     c                   clear                   savfgs
590500070208     C     *LIKE         DEFINE    FVVFLE        WFLE
590600070208     C     *LIKE         DEFINE    ARTFLP        SAVFLP
590700070208     C     *LIKE         DEFINE    BARDFV        DSPB
590800070208     C     *LIKE         DEFINE    BARDFV        COMDFV
590900070208     C     *LIKE         DEFINE    BARDFV        SVVDFV
591000070208     C     *LIKE         DEFINE    BLPTFP        LNPTFP
5911000702080    C     *LIKE         DEFINE    BARDFV        WDTRIF
591200070208     C     *LIKE         DEFINE    BARDFV        PRIDAT
591300070208     C     *LIKE         DEFINE    BARDFV        SA2DFV
591400070208     C     *LIKE         DEFINE    ANMCCH        WCCH
591500070208     C     *LIKE         DEFINE    GCPFRG        KFRG
591600070208     C                   MOVEL     '0'           KFRG
591700070208     C     *LIKE         DEFINE    AGBAGB        KAGB
591800070208     C     *LIKE         DEFINE    AGBFBS        WFBS
591900070208     C     *LIKE         DEFINE    AGBFVC        WFVC
592000070208     C     *LIKE         DEFINE    AGBFPC        WFPC
592100070208     C     *LIKE         DEFINE    ARBNDC        WABNDC
592200070208     C     *LIKE         DEFINE    ARTDCM        WATDCM
592300070208     C     *LIKE         DEFINE    ARbcca        WABcca
592400070208     C     *LIKE         DEFINE    ARbcca        savcca
592500070208     C     *LIKE         DEFINE    BARLNA        SVVLNA
592600070208     C     *LIKE         DEFINE    BARLNP        SVVLNP
592700070208     C     *LIKE         DEFINE    ARTDCM        WDCM
592800070208     C     *LIKE         DEFINE    ANMCAA        CAACRE
592900070208     C     *LIKE         DEFINE    ANMCAA        CAACIU
593000070208     C     *LIKE         DEFINE    FGVTTR        WTTR
593100140613     C     *LIKE         DEFINE    FGVFCF        WFCF
593200070208     C     *LIKE         DEFINE    EVSCEV        KCEV
593300070208     C     *LIKE         DEFINE    D33MAC        SAVMAC
593400070208     C     *LIKE         DEFINE    BRVFLE        BARFLE
593500070208     C     *LIKE         DEFINE    BRVFGS        SAVPES
593600070208     C     *LIKE         DEFINE    BRVFGS        KFGS4
593700070208     C     *LIKE         DEFINE    TBLCOD        COD
593800070208     C     *LIKE         DEFINE    TBLKEY        KEY
593900070208     C     *LIKE         DEFINE    BARLNA        LNATFA
594000070208     C     *LIKE         DEFINE    BARLNA        Usalnatfa
594100100713     C     *LIKE         DEFINE    BARLNA        WgenLNAtfa
594200070208     C     *LIKE         DEFINE    blptfa        bolTFA
594300070208     C     *LIKE         DEFINE    BARLNA        PESTFA
594400070208     C     *LIKE         DEFINE    BARLNA        PESTFP
594500070208     C     *LIKE         DEFINE    BARLNP        LNPB
594600070208     C     *LIKE         DEFINE    FVVFGS        WFGS
594700070208     C     *LIKE         DEFINE    FVVSPG        WSPG
594800070208     C     *LIKE         DEFINE    FVVNPG        WNPG
594900070208     C     *LIKE         DEFINE    FVVNPG        KNPG4
595000070208     C     *LIKE         DEFINE    FVVNFV        WNFV
595100070208     C     *LIKE         DEFINE    FVVNFV        KNFV4
595200070208     C     *LIKE         DEFINE    FVVFLE        WLIN
595300070208     C     *LIKE         DEFINE    FVVFLE        WLINPR
595400070208     C     *LIKE         DEFINE    FVVSPG        BARSPG
595500140616     C     *LIKE         DEFINE    FVVSPG        brvSPG
595600070208     C     *LIKE         DEFINE    FVVDFV        WDFV
595700070208     C     *LIKE         DEFINE    FVVDFV        SAVDFV
595800070208     C     *LIKE         DEFINE    FVVDFV        BARDFV
595900070208     C     *LIKE         DEFINE    FVVDFV        BRVDFV
596000070208     C     *LIKE         DEFINE    BARKEY        SAVKEY
596100070208     C     *LIKE         DEFINE    BRVKEY        SA1KEY
596200070208     C     *LIKE         DEFINE    ARBLNP        COMLNP
596300070208     C     *LIKE         DEFINE    ARBAAS        COMAAS
596400070208     C     *LIKE         DEFINE    ARBNSP        COMNSP
596500070208     C     *LIKE         DEFINE    ARBTFP        WPTFP
596600070208     C     *LIKE         DEFINE    ANMFL1        WFL1
596700070208     C     *LIKE         DEFINE    D55TFA        KTFA2
596800070208     C     *LIKE         DEFINE    D55TFA        SAVTFA
596900070208     C     *LIKE         DEFINE    ARTFVC        WWFVC
597000070208     C     *LIKE         DEFINE    ARTDAM        SAVDAM
597100070208     C     *LIKE         DEFINE    ARTDAM        WARDAM
597200070208     C     *LIKE         DEFINE    ARTDAM        WSVDAM
597300070208     C     *LIKE         DEFINE    FVADFV        SAVDAO
597400070208     C     *LIKE         DEFINE    FVALNP        WVALNP
597500070208     C     *LIKE         DEFINE    DCTTAD        SAVTAD
597600070208     C                   CLEAR                   SAVTAD
597700070208     C     *LIKE         DEFINE    §OGNTW        NTWBAR
597800070208     C     *LIKE         DEFINE    GI8INV        DATMIN
597900070208     C     *LIKE         DEFINE    BRVATR        SAWATR
598000080317     C     *LIKE         DEFINE    BRVTSU        SAWtsu
598100070208     C*
598200070208     C                   MOVEL     'DT'          KAGB
598300070208     c                   ENDSR
598400140613     C**************************************************************************
598500140613     C* Cerca spunta 4
598600140613     C**************************************************************************
598700140613     c     Cer_cons      BEGSR
598800140616     c* imposto data immiss.spunta della spunta che sto caricando
598900140616     c                   exsr      ImpoDFS
599000140616     c*
599100140616     C     KBAR2_d       setll     FNBRV05L
599200140616     C     KBAR2_d       reade(n)  FNBRV05L
599300140616    1c                   dow       not %eof(fnbrv05l)
599400140613     c* controllo data e ora immissione
599500140613     c                   clear                   brv_imm
599600140613     c                   clear                   brv_sca
599700140613     c                   z-add     brvhms        brv_imm
599800140613     c                   movel     brvdfs        brv_imm
599900140613     c                   z-add     brvhcs        brv_sca
600000140613     c                   movel     brvdcs        brv_sca
600100140613     c                   exsr      sceglidata
600200140613     c                   z-add     a_imm         b_imm
600300140613     c
600400140613     c* a parità di data devo tenere la più recente
600500140613     c                   clear                   brv_imm
600600140613     c                   clear                   brv_sca
600700140613     c                   z-add     barhms        brv_imm
600800140613     c                   movel     wDFS          brv_imm
600900140613     C                   Z-ADD     WDATE1        barDCS
601000140613     C                   time                    barHCS
601100140613     c                   z-add     barhcs        brv_sca
601200140613     c                   movel     bardcs        brv_sca
601300140613     c                   exsr      sceglidata
601400140613     c*
601500140613     c                   select
601600140616     c* quella trovata + recente --> non carico la spunta
601700140613    2c                   when      b_imm > A_IMM  and brvdfs=wdfs
601800140613     c
601900140613     c* Se anche la data foglio è = non carico la spunta ricevuta
602000140613    3C     BRvKEY        IFNE      SA1KEY
602100140613     C                   MOVEL     BRvKEY        SA1KEY
602200140613     C                   Z-ADD     BRvNPG        WNPG
602300140613     C                   Z-ADD     BRvNFV        WNFV
602400140613     C                   Z-ADD     BRvFGS        WFGS
602500140613     C                   movel     BRvnps        Wnps
602600140613     C                   EXSR      RILFOG
602700140613     c                   Z-ADD     WDFV          BRVDFV
602800140616     c                   movel     WFCF          BRVFCF
602900140616     c                   movel     Wspg          BRVspg
603000140613    3c                   endif
603100140613     c
603200140616     c                   if        barnpg=3 and
603300140616    3c                             brvdfv=bardfv
603400140616     c                   eval      amb_QD ='N'
603500140616     c                   endif
603600140616     c                   if        barnpg=4 and brvspg='A' and
603700140616    3c                             brvdfv=bardfv
603800140616     c                   eval      amb_QD ='N'
603900140616     c                   endif
604000140613     c
604100140616     c                   if        amb_qd='N'
604200140613     c* memorizzo la spunta nuova negli errori
604300140613     C                   MOVEL     'S'           WBRVE             1
604400140613     C                   EXSR      RIEMPI
604500140613     C                   CLEAR                   WBRVE
604600140613     C                   WRITE     FNBRVE
604700140616     c                   leave
604800140613    3c                   endif
604900140616     c
605000140616     c* quella di carico + recente --> cancello quella già presente
605100140613    2c                   when      A_imm > B_IMM  and brvdfs=wdfs
605200140613     c* Se anche la data foglio è = non carico la spunta ricevuta
605300140613    3C     BRvKEY        IFNE      SA1KEY
605400140613     C                   MOVEL     BRvKEY        SA1KEY
605500140613     C                   Z-ADD     BRvNPG        WNPG
605600140613     C                   Z-ADD     BRvNFV        WNFV
605700140613     C                   Z-ADD     BRvFGS        WFGS
605800140613     C                   movel     BRvnps        Wnps
605900140613     C                   EXSR      RILFOG
606000140613     c                   Z-ADD     WDFV          BRVDFV
606100140616     c                   movel     WFCF          BRVFCF
606200140616     c                   movel     Wspg          BRVspg
606300140613    3c                   endif
606400140613     c
606500140616     c                   if        barnpg=3 and
606600140616    3c                             brvdfv=bardfv and brvfcf=' '
606700140616     c                   eval      amb_QD ='S'
606800140613     c* mi slavo data e ora imm spunta 4 per la delete
606900140616     c                   eval      qd_npg=4
607000140616     c                   eval      qd_dfs=brvdfs
607100140616     c                   eval      qd_hms=brvhms
607200140616     c                   leave
607300140613     c                   endif
607400140616     c                   if        barnpg=4 and  brvspg='A' and
607500140616    3c                             brvdfv=bardfv and brvfcf=' '
607600140616     c                   eval      amb_QD ='S'
607700140616     c* mi slavo data e ora imm spunta 4 per la delete
607800140616     c                   eval      qd_npg=3
607900140616     c                   eval      qd_dfs=brvdfs
608000140616     c                   eval      qd_hms=brvhms
608100140616     c                   eval      qd_nfv=brvnfv
608200140616     c                   leave
608300140616     c                   endif
608400140613     c
608500140613    2c                   endsl
608600140616     c
608700140616     C     KBAR2_d       reade(n)  FNBRV05L
608800140616    1c                   enddo
608900140613     c
609000140613     c                   ENDSR
609100140613     C**************************************************************************
609200140613     C* Scelta data tra immisisone e caicamento
609300140613     C**************************************************************************
609400140613     c     Sceglidata    BEGSR
609500140613     c                   if         brv_imm>brv_sca and
609600140616     c                              %subst(%editc(brv_sca:'X'):1:8)>*zeros
609700140613     c                   eval       A_imm=brv_sca
609800140613     c                   else
609900140613     c                   eval       A_imm=brv_imm
610000140613     c                   endif
610100140613     c                   ENDSR
610200140613     C**************************************************************************
610300140613     C*    impostazione data immissione spunta
610400140613     C**************************************************************************
610500140613     c     ImpoDFS       BEGSR
610600140613     c* Se data immissione > di oggi, imposto la minore tra oggi e data fogl
610700140613     c                   if        BARDFS<=wdate1
610800140613     C                   Z-ADD     BARDFS        wDFS
610900140613     c                   else
611000140613     c                   if        wdate1<=bardfv
611100140613     c                   z-add     wdate1        wDFS
611200140613     c                   else
611300140613     c                   z-add     bardfv        wDFS
611400140613     c                   endif
611500140613     c                   endif
611600140613     c                   ENDSR
611700140616     C**************************************************************************
611800140616     C*    cancellazione spunta meno recente IMA o cons
611900140616     C**************************************************************************
612000140616     c     Canc_QD       BEGsr
612100140616     c                   eval      kdel_npg=qd_npg
612200140616    1c                   if        kdel_npg=4
612300140616     c     kbar_qd4      chain     fnbrv05l                             38
612400140616     c  n38              if        %found(fnbrv05l)
612500140616     C     KBRVE1        SETLL     FNBRVE1L                               33
612600140616    2C     *IN33         IFEQ      *OFF
612700140616     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
612800140616     C                   WRITE     FNBRVE
612900140616    2C                   ENDIF
613000140616     c
613100140616     c                   delete    fnbrv005
613200141029     c                   endif
613300140616     c
613400140616   x1c                   else
613500140616     c     kbar_qd4      setll     fnbrv05l
613600140616     c     kbar_qd4      reade     fnbrv05l
613700140616    2c                   dow       not %eof(fnbrv05l) and brvnfv<>qd_nfv
613800140616     c     kbar_qd4      reade     fnbrv05l
613900140616    2c                   enddo
614000140616    2c                   if        not %eof(fnbrv05l)
614100140616     C     KBRVE1        SETLL     FNBRVE1L                               33
614200140616    3C     *IN33         IFEQ      *OFF
614300140616     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
614400140616     C                   WRITE     FNBRVE
614500140616    3C                   ENDIF
614600140616     c                   delete    fnbrv005
614700140616    2c                   endif
614800140616     c
614900140616    1c                   endif
615000140616     c                   ENDSR
615100150622     C**************************************************************************
615200150622     C*    Aggiornamenti da tabella BSP
615300150622     C**************************************************************************
615400150622     c     DA_BSP        BEGsr
615500150622     c* Scrivo FNAGB per aggiornare la bolla
615600150622     C                   CLEAR                   FNAGB000
615700150622     C                   MOVEL     'P'           AGBTBO
615800150622     C                   MOVEL     'CB'          AGBAGB
615900150622     C                   MOVEL     blpAAS        AGBAAS
616000150622     C                   MOVEL     blpLNP        AGBLNP
616100150622     C                   MOVEL     blpNRS        AGBNRS
616200150622     C                   MOVEL     blpNSP        AGBNSP
616300150622     C                   WRITE     FNAGB000                             99
616400150622     c                   ENDSR
616500070208     C**************************************************************************
616600070208     C* Elaborazioni per chiusura pgm
616700070208     C**************************************************************************
616800070208     c     ELACHIUDI     BEGSR
616900070208     C* ELABORO L'ULTIMO DATO MEMORIZZATO
617000070208     C                   EXSR      WRTAGB
617100070208     c
617200070208     C                   CLEAR                   DSLV55
617300070208     C                   MOVEL     'C'           D55TLA
617400070208     C                   EXSR      FNLV55
617500070208     C* SCRITTURA ULTIMO RECORD IN FNSTA
617600070208     C   02              EXSR      WRTSTA
617700070208     C**
617800070208     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
617900070208     c                   clear                   fidn12ds
618000070208     c                   eval      i12tla = 'C'
618100070208     c                   call      'FIDN12R'
618200070208     c                   parm                    fidn12ds
618300161007     c
618400161007     c                   clear                   fnlgi1ds
618500161007     c                   eval      ilgi1tla = 'C'
618600161007     c                   movel     fnlgi1ds      kpjbu
618700161007     c                   call      'FNLGI1R'
618800161007     c                   parm                    kpjba
618900070208     c                   ENDSR
619000110103
619100110103      *---------------------------------------------------------------*
619200110103      *?Reperimento dati del job (Utente/Operativi)                  ?*
619300110103      *---------------------------------------------------------------*
619400110103     c     sr_DatiJob    BEGSR
619500110103      *
619600110103     c     *dtaara       define    §azute        AZUTEds
619700110103     c     *dtaara       define    §datiute      dDatiUte
619800110103      *
619900110103     c                   in(E)     *dtaara
620000110103     c                   IF        %Error or RSUT = *blanks
620100110103     c                   clear                   TIBS34ds
620200110103     c                   call      'TIBS34R'
620300110103     c                   parm                    TIBS34ds
620400110103     c                   in        *dtaara
620500110103     c                   ENDIF
620600110103      *
620700110103     c                   ENDSR
620800040601**         CM5
620900040601DLYJOB DLY(5)
621000980928**         MSG
621100070906CARICA SPUNTE:Fogli Inesitenti (FGS NPG NFV-NPS) FNLS45R
621200070906XXX X XXXXX-xx;  XXX X XXXXX-xx;  XXX X XXXXX-xx;  XXX X XXXXX-xx;
621300070906           -  ;             -  ;             -  ;             -  .
