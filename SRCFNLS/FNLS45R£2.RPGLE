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
028400070212     d
028500070212     d* DS passaggio dati
028600070207     D fnls45ds      E DS
028700160726     d
028800160726     D DAGBFLO       E DS
028900091209     D DAR5          E DS
029000091209     D DAR5GEN       E DS
029100990518     D FNANM2          DS
029200960416     D  ANMNR2               397    400B 0
029300070212     D FIBAR           DS
029400030226     D  BA1_file              83     92
029500030226     D  BA1_libr              93    102
029600150622     d                 ds
029700150622     d bsplin
029800150622     d sbsp                           3  0 dim(30) overlay(bsplin)
029900040811
030000040811      * ds per il controllo della presenza di CA per la spedizione richiesta
030100040811     d FIDN12DS      e ds
030200040811     d  skKey                 26    305    dim(20)
030300040811     d  skAnn                306    325    dim(20)
030400040811     d  skDca                326    485  0 dim(20)
030500040811     d  skFca                486    545  0 dim(20)
030600040811     d  skDch                546    705  0 dim(20)
030700040811     d  skCch                706    745    dim(20)
030800040811     d*
030900070212     D DSRECBRV      E DS                  extname(fnbrv00f)  prefix(D_)
031000161007     D tigcp00ds     E DS                  extname(tigcp00f)
031100080317     D WTSU            s              1  0
031200160421     D wznc            s              2
031300140613     D kdel_npg        s                   like(brvnpg)
031400130228     D KCOD            s                   like(tbecod)
031500130228     D Kke1            s                   like(tbeke1)
031600130228     D Kke2            s                   like(tbeke2)
031700130228     D SAWBRV          s                   like(brv)
031800080317     D WPES            s                   like(brvpes)
031900070208     D BPES            s                   like(brvpes)
032000070208     D BTFP            s                   like(brvFLE)
032100050322     D savesart        s                   like(wesart)
032200041105     D wprimotbo       s                   like(savtbo)
032300070221     D WSAVFLG         s                   like(dls45FLG)
032400140613     D wdfs            s                   like(brvdfs)
032500140616     D qd_nfv          s                   like(brvnfv)
032600140616     D qd_npg          s                   like(brvnpg)
032700140616     D qd_dfs          s                   like(brvdfs)
032800140616     D qd_hms          s                   like(brvhms)
032900140616     D bardcs          s                   like(brvdcs)
033000140616     D barhcs          s                   like(brvhcs)
033100140616     D barfcf          s                   like(fvvfcf)
033200140616     D brvfcf          s                   like(fvvfcf)
033300150622     d w_clibo         s                   Like(blpccm)
033400160726     d welasta_p       s              1
033500160726     d wBspTp          s              1
033600150622     d wBspspu         s              1
033700140618     d A_imm           s             14  0 inz
033800140618     d B_imm           s             14  0 inz
033900140618     d brv_imm         s             14  0 inz
034000140618     d brv_sca         s             14  0 inz
034100140618     d DatReale        s              8  0 inz
034200140618     d oraReale        s              4  0 inz
034300130228     d Indx            s              3  0
034400130228     d SAVfnrs         s              5  0 inz
034500130228     d Wflsnrs         s              5  0
034600041013     D WCONTRDFV       s              1    inz('N')
034700041105     D WAGBART         s              1    inz(' ')
034800070906     D WNPS            s              2    inz
034900080630     D WOK             s              1    inz
035000080630     D Secondi         s              9  0 inz
035100040621     D ktrd            s                   like(ar5trd)  inz('GEN')
035200080630     D Timeold         s               t   timfmt(*iso)
035300080630     D Timenew         s               t   timfmt(*iso)
035400980928     D* DEFINIZIONE COSTANTI
035500980928     D CUTI            C                   CONST('EDP*      ')
035600960423     C*---------------------------------------------------------------*
035700921014     C* RIEPILOGO INDICATORI
035800970115     C*---------------------------------------------------------------*
035900921014     C* 02    - RICHIAMATO
036000930914     C* 03    - POSSO RIABILITARE LA BOLLA
036100921021     C* 05    - MODIFICA DEL SOLO NUMERO DI FOGLIO
036200000505     C* 28/29 - DI COMODO
036300070212     C* 30    - LETTURA FIBAR00F
036400070131     C* 31    - CHAIN FNBRV
036500950927     C* 32    - LETTURA FILE FNBLT FNART FNARB FNBTT
036600950927     C* 33/34 - DI COMODO
036700921014     C* 37    - NON ESISTE BOLLA TRANSITO
036800070131     C* 38    - ERRORE SU WRITE FNBRV e su update
036900921105     C* 39    - RECORD ALLOCATO
037000921014     C* 40    - SE NON RECORD ALLOCATI O ALLOCATI PER 3 VOLTE FINE PGM
037100950926     C* 41    - COMODO
037200030528     C* 45    - Errore su write fnagb
037300040310     C* 46    - lookuip su schiera NTS
037400970204     C* 96    - ESISTE GIA' A MENO L'ANOMALIA CHE DEVO CREARE
037500970204     C* 97    - USATO PER LA CHIUSURA ANOMALIE CON 145 E 146
037600080325     C* 98    - FIBAR allocato --> esco dal pgm
037700080325     C* 99    - DI COMODO
037800921014     C*---------------------------------------------------------------*
037900911016     C     *ENTRY        PLIST
038000070212     C                   PARM                    fnls45ds
038100070208     C                   PARM                    dsrecbrv
038200941214     C*---------------------------------------------------------------*
038300070221     c* tipo di richiamo in base a dls45FLG
038400070208     C*---------------------------------------------------------------*
038500930521     C* FLG = '2' MODIFICATO SOLO NUMERO FOGLIO
038600160624     c
038700160624     c* Eliminato tipo richiamo "6" in quanto eliminate anomalie 60 e 61
038800070221     C* FLG = '6' CREO SOLO ANOMALIA 61 (EXSR CRTA60) DA SPU DISGUIDO
038900160624     c
039000991022     C* FLG = 'D' CREO ANOMALIA DISGUIDO PER COLLO DA ME CONFERMABILE
039100930521     C* FLG = ' ' INSERIMENTO NUOVA SPUNTA O VARIATA SOLO ANOMALIA
039200930521     C* FLG = 'C' CHIUDO PGM CON LR E BASTA
039300050418     C* FLG = 'T' aggiorno bolla transito x disguido
039400070221     c*
039500070221     c* Richiami attualmente usati: '2'  ' '  'T'  'C'
039600070208     C*---------------------------------------------------------------*
039700070208     c* Eaborazioni per chiusura pgm
039800070221    1C     dls45FLG      IFEQ      'C'
039900070208     c                   EXSR      ELACHIUDI
040000040811     c*
040100930521     C                   SETON                                        LR
040200930521     C*
040300930521   X1C                   ELSE
040400941229     C*
040500941229     C* IMPOSTAZIONI INIZIALI
040600941229    2C     UNO           IFEQ      ' '
040700941229     C                   EXSR      CARTAB
040800941229    2C                   END
040900070208     c
041000070208     c* Impostazione di BPES
041100070208     c                   if        dls45pes>*zeros
041200070208     c                   movel     dls45pes      BPES
041300070208     c                   endif
041400070208     c                   if        dls45tfp>*zeros
041500070208     c                   movel     dls45tfp      BTFP
041600070208     c                   endif
041700070208     c
041800070208     c                   EXSR      IMPO
041900930521     C*
042000930521     C                   SETOFF                                       05
042100941229     C                   Z-ADD     1             B                 3 0
042200930521     C****
042300930521     C**** P G M   R I C H I A M A T O
042400930521     C****
042500070208    2C                   if        dls45ric='S'
042600070208     C                   SETON                                        02
042700070208     c
042800070208     C* nel secndo parametro passato ci sono tutti i campi della spunta
042900070208     c*  con la quale effettuare gli aggiornamenti
043000070212     C                   Z-ADD     d_brvnpg      BARNPG
043100070212     C                   Z-ADD     D_brvnfv      BARNFV
043200070212     C                   Z-ADD     d_brvlnp      BARLNP
043300070212     C                   Z-ADD     d_brvlna      BARLNA
043400070212     C                   Z-ADD     d_brvnrs      BARNRS
043500070212     C                   Z-ADD     d_brvnsc      BARNSC
043600070212     C                   MOVEL     d_brvcan      BARCAN
043700070212     C                   Z-ADD     d_brvnps      BARNPS
043800070212     C                   Z-ADD     d_brvvuc      BARVUC
043900070212     C                   Z-ADD     d_brvpuc      BARPUC
044000070212     C                   Z-ADD     d_brvdfs      BARDFS
044100070212     C                   Z-ADD     d_brvhms      BARHMS
044200070212     C                   Z-ADD     d_brvmis      BARmis
044300070212     C                   movel     d_brvtap      BARtap
044400070212     C                   Z-ADD     d_brvznc      BARZNC
044500070212     c                   movel     d_brvpru      barpru
044600070212     c                   movel     d_brvfgs      barfgs
044700101130     c                   movel     d_brvdcs      brvdcs
044800101130     c                   movel     d_brvhcs      brvhcs
044900070208     c
045000960530     C                   CLEAR                   SAVMAC
045100960529     C**
045200070208     c** 05 on - modificato solo numero di foglio viaggio
045300070221    3C     dls45FLG      IFEQ      '2'
045400921229     C                   SETON                                        05
045500020213     C                   ENDIF
045600960524     C*
045700070209     C* CONTROLLO CHE ESISTA COD.ANOMALIA NELLA TABELLA ANOMALIE
045800020204     C     BARCAN        IFNE      ' '
045900020204     C     BARCAN        ANDNE     ' '
046000020204     C     BARCAN        LOOKUP    CAN                                    34
046100020204     C  N34              CLEAR                   BARCAN
046200020204     C                   ENDIF
046300961213     C*
046400961213     C* LETTURA FOGLIO E IMPOSTAZIONE CAMPI DI COMOD
046500961213     C                   EXSR      BARFGV
046600960529     C**
046700960529     C* SE NON HO TROVATO IL FOGLIO ESCO IMPOSTANDO L'ERRORE
046800960529    3C     BARDFV        IFEQ      0
046900070906     C                   SETON                                        39
047000960529   X3C                   ELSE
047100991230     C     BARLNA        CHAIN     AZORG01L                           31
047200040303    4c                   if        *in31 or
047300040303     c                             orgfag<>'F' and orgfag<>'A'
047400040303     C                   CLEAR                   NTWBAR
047500040303   x4c                   else
047600040303     C                   MOVE      ORGDE3        OG143
047700040303     C                   MOVE      §OGNTW        NTWBAR
047800000523     C*
047900000523     C* SE DEVO CREARE ANOMALIA DI DISGUIDO, CHIAMO SOLTANTO CRTA52
048000070221    5C     dls45FLG      IFEQ      'D'
048100991022     C                   MOVEL     'D'           WTIBOL
048200000523   X5C                   ELSE
048300920424     C*
048400920424     C* SE TROVO RECORD ALLOCATI RITENTO 3 VOLTE
048500000523    6C     B             DOWLE     3
048600950707     C                   MOVE      'S'           OKAGG             1
048700911016     C                   EXSR      UPDARB
048800000523    7C     OKAGG         IFEQ      'N'
048900950707     C                   Z-ADD     4             B
049000000523   X7C                   ELSE
049100021112     C                   Z-ADD     4             B
049200000523    7C                   ENDIF
049300150824     c
049400000523    6C                   ENDDO
049500150824
049600150824     c* Colli che partono dapiù filiali: facico fare la write al
049700150824     c*  chiamante
049800150824     C                   IF        wbsptp<>' ' and wbspspu='S'
049900150824     c                             and wagpar=' '
050000150824     c                   eval      %subst(dls45flo:1:1)='B'
050100150824     c                   endif
050200150824     c
050300040303    5C                   ENDif
050400040303    4C                   ENDIF
050500960410     C*
050600960410     C* PER BOLLA ARRIVO NON LOCALE, VEDO SE CREARE ANOMALIA 50
050700001114     C*  COLLO ARRIVATO NON PARTITO
050800960529     C*  NON ELABORO SE COLLO LOCALE O SOLO DELLA LINEA DI PARTENZA
050900001115     C* (N.B. INFATTI, SE BOLLA ARRIVO LOCALE, NON C'E' TRASMISSIONE DI
051000001115     C*  F.V. E QUINDI QUESTA ROUTINE NON SAREBBE MAI IN GRADO DI
051100001115     C*  CREARE L'ANOMALIA 50  DAL MOMENTO CHE VA A CERCARE IL FOGLIO
051200001115     C*  QUESTO VALE ANCHE PER I COLLI 180 PER 89)
051300020218    5C  N05WTIBOL        IFNE      'P'
051400960530     C     WTIBOL        ANDNE     'S'
051500020214     C     WTIBOL        ANDNE     'R'
051600070221     C     dls45FLG      ANDNE     '6'
051700960410     C                   EXSR      CRTA50
051800000523    5C                   ENDIF
051900000523     C*
052000960530    3C                   ENDIF
052100911016     C                   GOTO      FINE
052200960529     C*
052300960529    2C                   ENDIF
052400930521     C****
052500930521     C**** P G M    N O N    R I C H I A M A T O
052600930521     C****
052700930521     C* APRO FILE
052800040601     c                   clear                   wconta            2 0
052900040601     c                   movel     'S'           wapri             1
053000070212     C                   OPEN(e)   FIBAR00F
053100040601     c* Attendo 5 secondi e riprovo max 9 volte poi esco con errore
053200040601     c                   dow       %error  and wconta<10
053300040601     C                   Z-ADD     15            LUNG
053400040601     C                   MOVEA(p)  CM5           COMMAN           80
053500040601     C                   CALL      'QCMDEXC'                            99
053600040601     C                   PARM                    COMMAN
053700040601     C                   PARM                    LUNG             15 5
053800040601     c
053900040601     c                   add       1             wconta
054000070212     C                   OPEN(e)   FIBAR00F
054100040601     c                   enddo
054200040601     c
054300040601     c* Se ancora errore vado a fine file
054400040601     c                   if        %error
054500040601     c                   eval      wapri='N'
054600040601     c                   goto      fine
054700040601     c                   endif
054800040601     c
054900920424     C                   SETON                                        40
055000941214     C                   Z-ADD     *ZERO         SA1KEY
055100960530     C                   CLEAR                   SAVMAC
055200070208     C                   MOVE      DLS45FLG2     FLGPRE            1
055300911014     C*
055400080325     C                   READ      FIBAR00F                             9830
055500911016     C*
055600930521    2C     *IN40         DOWEQ     '1'
055700080325    2C     *IN98         andeq     *off
055800920424     C                   SETOFF                                       40
055900930521    3C     *IN30         DOWEQ     '0'
056000080325    3C     *IN98         andeq     '0'
056100930602     C*
056200040419     C                   SETOFF                                       3839
056300930521    4C     BARLNP        IFGT      *ZERO
056400040419     C     BARLNA        ANDGT     *ZERO
056500960731     C     BARNSC        ANDGT     *ZERO
056600040224     C     BARNpg        ANDGT     *ZERO
056700070201     c* Per ora non si accettano fogli maggiori di 100.000
056800040601     C     BARNfv        ANDlT     100000
056900070212     c
057000070212     c* Ad ogni record imposto BPES, perchè per le spunte cat 1 di un
057100070212     c*  secondo livello, BPES diventa il terminal per motivi di
057200070212     c*  creazione della anoamlia 200, per cui potrebbe rimanare
057300070212     c*  sporco se dopo ci fossero spunte di altre categorie
057400070212     c                   movel     dls45pes      BPES
057500021015     C                   Z-ADD     BPES          BARFGS
057600070209     c
057700160511      * CLEARO PESO E VOLUME UNITARI SE SUPERIORI AL MAX PREVISTO
057800160511      * limiti per transpallet  (pistola 46)
057900160511      * LIMITI PER RILEVATORE PESO VOLUME PALLET (Pistola 48)
058000160511      * limite per VDL  ( tutte le altre pistole)
058100160511     c                   select
058200160511     c                   when      barnps= 48
058300030723     C     BARPUC        IFGT      §vpoMPP
058400030723     C                   CLEAR                   BARPUC
058500030723     C                   ENDIF
058600030723     C     BARVUC        IFGT      §vpoMVP
058700030723     C                   CLEAR                   BARVUC
058800030723     C                   ENDIF
058900160511     c
059000160511     c                   when      barnps= 46
059100160511     C     BARPUC        IFGT      §vpoMPT
059200160511     C                   CLEAR                   BARPUC
059300160511     C                   ENDIF
059400160511     c                   other
059500160511     C     BARPUC        IFGT      §vpoMPV
059600160511     C                   CLEAR                   BARPUC
059700160511     C                   ENDIF
059800160511     C     BARVUC        IFGT      §vpoMVV
059900160511     C                   CLEAR                   BARVUC
060000160511     C                   ENDIF
060100160511     C                   Endsl
060200160511     c
060300040805      * CLEARO PESO E VOLUME UNITARI SE INFERIORI AL MINIMO PREVISTO
060400040805      * I LIMITI VALGONO PER TUTTI I TIPI DI IMPIANTO
060500040805     C     BARPUC        IFLT      §vpoSPV
060600040805     C                   CLEAR                   BARPUC
060700040805     C                   ENDIF
060800040805     C     BARVUC        IFLT      §vpoSVV
060900040805     C                   CLEAR                   BARVUC
061000040805     C                   ENDIF
061100020204     C* CONTROLLO CHE ESISTA COD,ANOMALIA NELLA TABELLA ANOMALIE
061200020204     C     BARCAN        IFNE      ' '
061300020204     C     BARCAN        ANDNE     ' '
061400020204     C     BARCAN        LOOKUP    CAN                                    34
061500020204     C  N34              CLEAR                   BARCAN
061600020204     C                   ENDIF
061700040419     c
061800070906     c* Non carico se LNP inesistente
061900040419     C     BARLNP        CHAIN     AZORG01L                           31
062000040419     c* Non carico se LNA inesistente
062100070906   4bC     *IN31         IFEQ      *Off
062200040419     c     orgfag        andeq     'A'
062300040419     C     *IN31         oreq      *Off
062400040419     c     orgfag        andeq     'F'
062500980212     C*
062600991230     C     BARLNA        CHAIN     AZORG01L                           31
062700040419     c* non carico se LNA inesistente
062800040303    5C     *IN31         IFEQ      *ON
062900040224     c     orgfag        orne      'A'
063000040224     c     orgfag        andne     'F'
063100020212     C                   CLEAR                   NTWBAR
063200040303   x5C                   ELSE
063300020212     C                   MOVEL     ORGDE3        OG143
063400020212     C                   MOVEL     §OGNTW        NTWBAR
063500070912     c
063600070912     C* LETTURA FOGLIO DELLA SPUNTA E IMPOSTAZIONE CAMPI DI COMOD
063700070912     C                   EXSR      BARFGV
063800070912     c
063900070912     c* Non carico se numero foglio inesistente
064000070912   5ac                   if        bardfv>0
064100140616     c                   clear                   amb_QD            1
064200140613     c                   clear                   A_imm
064300140613     c                   clear                   B_imm
064400140613     c*
064500140613     c* se IMA e spunta consegna PIu' RECENTE --> cancello senza caricare
064600140613     c                   if        barnpg=3 and barspg='A'
064700140616     c                   eval      kdel_npg=4
064800140613     c                   exsr      Cer_cons
064900140613     c                   endif
065000140616     c                   if        barnpg=4
065100140616     c                   eval      kdel_npg=3
065200140616     c                   exsr      Cer_cons
065300140616     c                   endif
065400140613
065500140616   5cc                   if        amb_QD<>'N'
065600040303     c**
065700040303     C* VERIFICO IL FLAG NELLA DS7N
065800040303     C                   Z-ADD     1             WA                3 0
065900040310     C     BARNPG        LOOKUP    CAT(WA)                                46
066000040310    6C     *IN46         IFEQ      *OFF
066100040303     C     NST(WA)       ORNE      '1'
066200040303     c* Se tengo tutte le spunte, verifico spunta doppia per lo stesso NFV
066300070131     C     KBAR1         CHAIN     FNBRV01L                           31
066400040303   x6c                   else
066500911016     C*
066600040303     C* spunta doppia per stessa categoria e p.o. gestione
066700140613     C     KBAR2         CHAIN     FNBRV05L                           31
066800040303    6c                   endif
066900941229     C*
067000040303     C* Aggiorno bolle e spunta se:
067100040303     C* - spunta nuova (31 on)
067200040303     C* - spunta già esistente ma dati del record diversi(BAR1<>BRV)
067300040303     C* - spunta già presente ma nel giro prec.trovati record allocati(B>1)
067400070212     c* - trovati errori  (BARERR=E)
067500070912   5bC     *IN31         IFEQ      *ON
067600941229     C     BAR1          ORNE      BRV
067700001114     C     B             ORGT      1
067800070212     C     BARERR        OReq      'E'
067900040303     c**
068000040303     C* OPERAZIONI PER SPUNTA GIA' PRESENTE e da tenerne una
068100040303    6C     *IN31         IFEQ      *OFF
068200040303     C     nst(WA)       ANDEQ     '1'
068300010608     C     BAR1          ANDNE     BRV
068400010608     C                   EXSR      SRBRVE
068500040303    6C                   ENDIF
068600070710     c**
068700070710     C* OPERAZIONI PER SPUNTA GIA' PRESENTE e da tenerne una
068800070710    6C     *IN31         IFEQ      *OFF
068900070710     C     nst(WA)       ANDne     '1'
069000070710     C     BAR1          ANDNE     BRV
069100070710     c* la precedente spunta non ha l'anomalia
069200070710     c     brvcan        andeq     ' '
069300070710     c* nell'attuale è stata inserita
069400070710     c     barcan        andne     ' '
069500070822     c*  Carico solo se data/ora Immissione spunta sono < della precedente
069600070822     c     barimmis      andgt     brvimmis
069700070710     C                   EXSR      SRBRVE
069800070710    6C                   ENDIF
069900040303     C* RIEMPI I CAMPI
070000040303     C                   EXSR      RIEMPI
070100040303     C**
070200040303     C**  A G G I O R N A M E N T I
070300961213     C                   MOVE      'S'           OKAGG             1
070400911112     C                   EXSR      UPDARB
070500911014     C**
070600920424     C**  AGGIORNO SPUNTA
070700950707     C*   Non l'aggiorno se spunta partenza manuale e di collo già
070800950707     C*   consegnato
070900040303    6C     OKAGG         IFEQ      'S'
071000040303     C*
071100960521     C* SE NO  LA DEVO TRASMETTERE LA FLAGGO
071200961213    7C     WNOTRA        IFEQ      'S'
071300960521     C                   MOVEL     'N'           BRVFTR
071400960521     C                   MOVEL     *HIVAL        BRVDTR
071500991223     C                   MOVEL     *HIVAL        BRVHTR
071600960521     C                   CLEAR                   WNOTRA
071700961213    7C                   ENDIF
071800980115     C* SE SPUNTA PARTENZA RELATIVA AD UN FOGLIO VIAGGIO SU CUI NON E'
071900980115     C* PIU' PRESENTE LA LNA DEL COLLO -> FLAGGO LA SPUNTA
072000980205     C* (RIENTRA IN QUESTO CASO ANCHE UNA SPUNTA CON COD.ANOMALIA "E"
072100980205     C* CIOE' UNA SPUNTA LA CUI LINEA NON ERA PRESENTE SUL FV GIA' NEL
072200980205     C* MOMENTO IN CUI E' STATA FATTA)
072300040303    7C     BARNPG        IFEQ      1
072400980115     C                   MOVE      BARLNA        W003A             3
072500980313     C     W003A         LOOKUP    FFS                                    33
072600980115     C  N33              MOVEL     'L'           BRVFTR
072700980115     C  N33              MOVEL     *HIVAL        BRVDTR
072800991223     C  N33              MOVEL     *HIVAL        BRVHTR
072900980210     C   33BRVCAN        IFEQ      'E'
073000980210     C                   CLEAR                   BRVCAN
073100980210     C                   END
073200041020    7C                   ENDIF
073300041020     c
073400041020     c* Imposto brvatr='D' per disguido
073500041020     c                   if        wtibol='D'
073600041020     c                   eval      brvatr='D'
073700041020     c                   else
073800041020     c                   clear                   brvatr
073900041020    7C                   ENDIF
074000040303     c**
074100070131     C* AGGIORNO LF FNBRV01L
074200040310    7C  N31*IN46         IFEQ      *OFF
074300040303     C     NST(WA)       ORNE      '1'
074400070131     C                   UPDATE    FNBRV000
074500040303     C                   ELSE
074600140613     C* AGGIORNO LF FNBRV05L
074700140613     C                   UPDATE    FNBRV005
074800040303    7C                   END
074900010607     C*
075000070131     C   31              WRITE     FNBRV000                             38
075100140613     c
075200140613     c* Se previsto cancello la spunta "doppia" tra 3 e 4
075300140616    7c                   if        amb_QD='S'
075400140616     c                   exsr      Canc_QD
075500140616    7c                   endif
075600150824     c**
075700150824     c*  COLLO SPUNTABILE DA PIÙ FILIALI
075800150824     c**
075900150824     c** elaboro per "K-cli" con BPES <> LNPD ( in qeusto caso ho pulito
076000150824     c*                                        AGGPAR)
076100150824     c*  oppure  per "L-linea" se TFPLNP <>TFPPES
076200150824     c* lo faccio dopo la write di BRV perchè fnlv52r deve leggere la spunta per
076300150824     c*  determinare la nuova lnp della bolla
076400150824     C                   IF        wbsptp<>' ' and wbspspu='S'
076500150824     c                             and wagpar=' '
076600150824     c                   exsr      DA_BSP
076700150824     c                   endif
076800140613     c
076900040303   X6C                   ELSE
077000040303     C*
077100040310    7C  N31*IN46         IFEQ      *OFF
077200040303     C     NST(WA)       ORNE      '1'
077300070131     C                   UNLOCK    FNBRV01L
077400040303     C                   ELSE
077500140613     C                   UNLOCK    FNBRV05L
077600040303    7C                   END
077700040303    6C                   END
077800950927     C* PER BOLLA ARRIVO NON LOCALE, VEDO SE CREARE ANOMALIA 50
077900950927     C*  COLLO ARRIVATO NON PARTITO
078000960207     C*  NON ELABORO SE COLLO LOCALE O SOLO DELLA LINEA DI PARTENZA
078100020214    6C  N05WTIBOL        IFEQ      'D'
078200020214     C     WTIBOL        OREQ      'A'
078300020214     C     WTIBOL        OREQ      'T'
078400020218     C     WTIBOL        OREQ      'L'
078500950927     C                   EXSR      CRTA50
078600950927    6C                   ENDIF
078700950927     C*
078800070912   5bC                   ELSE
078900040303     C*
079000040310    6C  N31*IN46         IFEQ      *OFF
079100040303     C     NST(WA)       ORNE      '1'
079200070131     C                   UNLOCK    FNBRV01L
079300040303     C                   ELSE
079400140613     C                   UNLOCK    FNBRV05L
079500040303    6C                   END
079600070912   5bC                   END
079700140613   5cC                   END
079800110103     c*
079900070912   5ac                   else
080000070912     c* Scrivo la spunta errata in fibar00e
080100070912     c                   movel     bpes          barpes
080200070912     c                   movel     wdate1        bardim
080300070912     c                   write     barcoerr                             99
080400070912   5aC                   END
080500040303    5C                   END
080600070906   4bC                   END
080700040419    4C                   END
080800911112     C*
080900900619     C* CANCELLO RECORD LETTO
081000001114     C* Se spunta imi e non ho aggiornato blt perchè inesistente o se
081100021203     C* non ho aggiornato blp perchè record allocato la spunta non viene
081200001114     C* cancellata.
081300021107     c                   if        not *in38 and not *in39
081400021107     C                   DELETE    BARCO
081500021107     c                   else
081600021203     c* Se ci fosse errore ma passati 4 giorni dalla data di immissione
081700021203     c*  spunte, deleto lo stesso
081800021203     c                   if        brvdfs<=wpul050
081900021203     c                   DELETE    BARCO
082000021203     c                   else
082100070212     c                   eval      barerr='E'
082200021107     c                   update    BARCO
082300021107     c                   endif
082400021203     c                   endif
082500900618     C*
082600080325     C                   READ      FIBAR00F                             9830
082700930521    3C                   END
082800021112     C** 40              ADD       1             B
082900021112     C   40              z-add     4             B
083000920424     C* DOPO 3 VOLTE ESCO
083100930521    3C   40B             IFGT      3
083200920424     C                   SETOFF                                       40
083300930521   X3C                   ELSE
083400070212     C                   CLOSE     FIBAR00F
083500070212     C                   OPEN      FIBAR00F
083600070212     C                   READ      FIBAR00F                               30
083700930521    3C                   END
083800930521    2C                   END
083900970224     C* ELABORO L'ULTIMO DATO MEMORIZZATO
084000970224     C                   EXSR      WRTAGB
084100911016     C*
084200911016     C     FINE          TAG
084300930521     C** 02 OFF - PROGRAMMA NON RICHIAMATO
084400930521     C**
084500950926     C** LANCIO PGM DI AGGIORNAMENTO BOLLE
084600961212    2C     *IN02         IFEQ      *OFF
084700970814     C**
084800970814     C                   CLEAR                   DSLV55
084900970814     C                   MOVEL     'C'           D55TLA
085000020213     C                   EXSR      FNLV55
085100000705     C**
085200000705     C                   CLEAR                   DSLV53
085300000705     C                   MOVEL     'C'           D53TLA
085400000705     C                   CALL      'FNLV53R'
085500000705     C                   PARM                    DSLV53
085600930521     C*
085700040601     c                   if        wapri='S'
085800070212     C                   CLOSE     FIBAR00F
085900040601     c                   endif
086000930521     C*
086100981105     C* SE HO ALEMNO UN FOGLIO SCRITTO--> TRASMETTO
086200111018     C***  ERF(1)        IFNE      *BLANKS
086300111018     C***                EXSR      TRASMI
086400111018     C***                ENDIF
086500040811     c*
086600040811     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
086700040811     c                   clear                   fidn12ds
086800040811     c                   eval      i12tla = 'C'
086900040811     c                   call      'FIDN12R'
087000040811     c                   parm                    fidn12ds
087100040811     c*
087200930521     C                   SETON                                        LR
087300930521     C*
087400930521   X2C                   ELSE
087500930521     C**
087600930521     C** 02 ON  - PROGRAMMA RICHIAMATO
087700930521     C**
087800921020     C* SE NON HO AGGIORNATO IMPOSTO LA "E" DI ERRORE
087900960521     C     *IN39         IFEQ      *ON
088000070221     C                   MOVEL     'E'           dls45FLG
088100960521     C                   ELSE
088200960521     C*
088300950926     C* SE AGGIORNATO FACCIO SOTTOMETTERE IL PGM DI AGGIORNAMENTO BOLLE
088400070221     C                   MOVEL     'A'           dls45FLG
088500950707     C* Se non ho aggiornato perchè spunta partenza manuale di collo
088600950707     C* consegnato imposto "N" di aggiornamento non effettuato
088700960521     C     OKAGG         IFEQ      'N'
088800070221     C                   MOVE      'N'           dls45FLG
088900950707     C                   END
089000960521     C                   ENDIF
089100070208     C                   MOVE      DLS45FLG2     FLGPRE            1
089200930521     C*
089300930521     C                   RETURN
089400930521    2C                   END
089500930521    1C                   END
089600941214     C**************************************************************************
089700941214     C* RILEVO IL FOGLIO VIAGGIO PARTENZE/ARRIVI
089800941214     C**************************************************************************
089900941214     C     RILFOG        BEGSR
090000941214     C*
090100941214     C                   Z-ADD     *ZERO         WDFV
090200941229     C                   CLEAR                   WSPG
090300021014     C                   Z-ADD     0             WFLE
090400961122     C                   CLEAR                   WTTR
090500980115     C                   CLEAR                   FFG
090600140613     C                   CLEAR                   WFCF
090700021024     c*
090800021024     c* Prendo il terminal di partenza del p.o. in gestione
090900070212    1c                   if        savfgs<>wfgs
091000021024     C                   CLEAR                   DSLV55
091100021024     C                   MOVEL     'P'           D55TPT
091200021024     C                   MOVEL     WFGS          D55LIN
091300021024     C                   MOVEL     WDATE1        D55DRF
091400021024     C                   CALL      'FNLV55R'
091500021024     C                   PARM                    DSLV55
091600021024     C**
091700070212    2C     D55ERR        IFne      ' '
091800021024     C                   MOVE      wfgs          tfpfgs
091900021024     c                   else
092000021024     C                   MOVE      d55tfp        tfpfgs
092100070212    2c                   endif
092200021024     c                   eval      savfgs=wfgs
092300070212    1c                   endif
092400941214     C*
092500941214     C* ARRIVI O PARTENZE ?
092600970114    1C     WNPG          IFEQ      1
092700941214     C*
092800021014     C     KFGV1         CHAIN     FNFGV01L                           99
092900000505     C*
093000070209    2C     *IN99         IFEQ      *OFF
093100021014     C     KFGV1         CHAIN     FNFGW01L                           28
093200070209    3C     *IN28         IFEQ      *ON
093300000505     C     FGWATB        ORNE      ' '
093400000505     C                   CLEAR                   FGWFF3
093500000505     C                   CLEAR                   FGWFF4
093600000505     C                   CLEAR                   FGWFL3
093700000505     C                   CLEAR                   FGWFL4
093800070209    3C                   ENDIF
093900980928     C                   Z-ADD     FGVDFV        WDFV
094000021024     C                   Z-ADD     FGVLNP        WFgs
094100021024     C                   Z-ADD     FGVLNP        WFLE
094200980928     C                   MOVEL     FGVTTR        WTTR
094300140613     C                   MOVEL     FGVFCF        WFCF
094400980928     C**
094500070209   X2C                   ELSE
094600960925     C     KFVA1         CHAIN     FNFVA01L                           99
094700970114    3C     *IN99         IFEQ      *OFF
094800970114     C                   Z-ADD     FVADFV        WDFV
094900021014     C                   Z-ADD     FVALNP        WFLE
095000970114     C                   MOVEL     FVATTR        WTTR
095100140613     C                   MOVEL     'S'           WFCF
095200111018   x3C**                 ELSE
095300110103     c*
095400070209     C* Se non trovato invio msg di errore
095500111018     C**                 EXSR      INVMSG
095600070212    3C                   ENDIF
095700070212    2C                   ENDIF
095800941214     C*
095900970114   X1C                   ELSE
096000941214     C*
096100021014     C* ACCEDO DIRETTAMENTE AL FOGLIO CON P.O. GESTIONE
096200021014     C* SE FOGLIO NON TROVATO CERCO CON SIMFEL- PUÒ ESSERE SOLO UN
096300070212     C*  FOGLIO IMP SOLO SE RICHIAMO PER DATI DI FIBAR!!
096400021014     C     KFVV1         CHAIN     FNFVV01L                           99
096500070209    2C     *IN99         IFEQ      *ON
096600021014     C     WNPG          ANDEQ     3
096700021014     C     RICBAR        ANDEQ     'S'
096800021014     C                   Z-ADD     SIMFEL        WFGS
096900021014     C     KFVV1         CHAIN     FNFVV01L                           99
097000021014     C  N99FVVSPG        IFNE      'P'
097100021014     C                   SETON                                        99
097200021014     C                   ENDIF
097300070209    2C                   ENDIF
097400021014     C*
097500070209    2C     *IN99         IFEQ      *OFF
097600980928     C                   Z-ADD     FVVDFV        WDFV
097700980928     C                   MOVEL     FVVSPG        WSPG
097800021014     C                   MOVEL     FVVFGS        WFGS
097900140613     C                   MOVEL     FVVFCF        WFCF
098000111018     C**                 ELSE
098100111018     C**                 EXSR      INVMSG
098200070209    2C                   ENDIF
098300941214     C*
098400970114    1C                   ENDIF
098500021014     C**
098600021024     C* SE WFLE=0 prendo TERMINAL DI PARTENZA di fgs A UDATE
098700070223     c*   di fatto si tratta del TFP di BPES poteva che può differire
098800070223     c*   dal FGS solo in caso di spunte ion procedura arrivo per p.o.
098900070223     c*   con terminal di partenza <> dal terminal arrivo
099000021014    1C     WFLE          IFEQ      0
099100021024     C                   MOVE      tfpfgs        WFLE
099200021014    1C                   END
099300941214     C*
099400941214     C                   ENDSR
099500020213     C**************************************************************************
099600020213     C*    IMPOSTAZIONI DI ALCUNI CAMPI DI PASSAGGIO SE VUOTI
099700020213     C**************************************************************************
099800020213     C     IMPO          BEGSR
099900150824     c                   eval      %subst(dls45flo:1:1)=' '
100000110103     c*
100100070208     C* BPES--> SE NON passato    E' SIMFEL
100200020213     C     BPES          IFEQ      0
100300020213     C                   Z-ADD     SIMFEL        BPES
100400070208     c                   if        btfp=0
100500070208     C                   Z-ADD     SIMFEL        BTFP
100600020213     C                   ENDIF
100700070208     C                   ENDIF
100800020213     C**
100900070208     C* BTFP--> SE NON passato ma c'è BPES, è terminal partenza di BPES
101000070208     c                   if        BTFP=0
101100020213     C                   CLEAR                   DSLV55
101200020213     C                   MOVEL     'P'           D55TPT
101300020213     C                   Z-ADD     BPES          D55LIN
101400020213     C                   Z-ADD     WDATE1        D55DRF
101500020213     C                   EXSR      FNLV55
101600110103     c*
101700020213     C                   Z-ADD     D55TFP        BTFP
101800020213     C                   ENDIF
101900110103     c*
102000070209     c* Se BPES non passato, reimposto anche il campo di input
102100070212     c                   if        DLS45PES<=*zeros
102200070209     c                   movel     bpes          dls45pes
102300070209     c                   endif
102400110103     c*
102500020213     C                   ENDSR
102600961213     C**************************************************************************
102700961213     C*    RIEMPI I CAMPI DEL RECORD DA SCRIVERE
102800961213     C**************************************************************************
102900961213     C     RIEMPI        BEGSR
103000110103     c*
103100080317     C* CONTROLLO SE E' PISTOLA MANUALE
103200080317     C                   EXSR      PISMAN
103300080317     c
103400961213     C* SE NON C'E' AGGIUNGO ALTRIMENTI AGGIORNO
103500080317    1C     WBRVE         IFEQ      'S'
103600070131     C* Se routine richiamata per scrittura di FNBRVe devo sempre clear
103700010608     c* are brvatr
103800010608     C                   CLEAR                   BRVATR
103900070201     C                   CLEAR                   BRVtsu
104000080317   x1C                   ELSE
104100961213     C   31              CLEAR                   BRVATR
104200070201     C   31              CLEAR                   BRVtsu
104300110103     c*
104400080317     c** Conteggio le spunte risparate + volte
104500080317     c** ignoro tutte le spunte manuali
104600100709     c** ignoro se spunta con errore (già caricata)
104700080422    1c                   if        not *in31  and (rps(y)<>'M' or
104800080422     c                             rps(y)='M' and tps(y)='C')
104900100709     c                             and barerr<>'E'
105000110103     c*
105100080317    2c                   if        nst(WA)<>'1'  or
105200080317     c* Se spunta unica  ma stesso numero foglio --> conto doppio
105300080317     c                             (nst(WA)= '1' and brvnfv=barnfv)
105400110103     c*
105500080630     c* Non considero doppia nemmeno se spunta cat 3 o 4 pistola 49
105600080630     c*  sparate entro i 2 sec.
105700080630     c                   eval      wok='S'
105800080630    3c                   if        (barnpg=3 or barnpg=4) and barnps=49
105900080630     c                             and brvnps=49 and brvdfs=bardfs
106000080630     c                   move      barhms        Timenew
106100080630     c                   move      brvhms        Timeold
106200080630     c     Timenew       subdur    Timeold       secondi:*s
106300080701     c                   mllzo     1             secondi
106400080630    4c                   if        secondi<=§vposecdop
106500080630     c                   eval      wok='N'
106600080630    4c                   endif
106700080630    3c                   endif
106800110103     c*
106900080630   2AC                   IF        WOK='S'
107000080317    3c                   if        brvtsu=' '
107100080317     c                   eval      brvtsu='1'
107200080317   x3c                   else
107300080317     c                   movel     brvtsu        wtsu              1 0
107400080317    4c                   if        wtsu<9
107500080317     c                   eval      wtsu=wtsu+1
107600080317     c                   movel     wtsu          brvtsu
107700080317    4c                   endif
107800110103     c*
107900080317    3c                   endif
108000080630   2Ac                   endif
108100110103     c*
108200080317   x2c                   else
108300080317     c* Prendo data foglio
108400080317    3C     BRvKEY        IFNE      SA1KEY
108500080317     C                   MOVEL     BRvKEY        SA1KEY
108600080317     C                   Z-ADD     BRvNPG        WNPG
108700080317     C                   Z-ADD     BRvNFV        WNFV
108800080317     C                   Z-ADD     BRvFGS        WFGS
108900080317     C                   movel     BRvnps        Wnps
109000080317     C                   EXSR      RILFOG
109100080317     c                   Z-ADD     WDFV          BRVDFV
109200140616     c                   movel     WFCF          BRVFCF
109300140616     c                   movel     Wspg          BRVspg
109400080317    3c                   endif
109500110103     c*
109600080317     c* Se spunta unica, foglio diverso ma stessa data -->
109700080317     c*                         mantengo TSU
109800080317     c*                                    data diversa --> lo pulisco
109900080317    3c                   if        bardfv<>brvdfv
110000080317     c                   clear                   brvtsu
110100080317    3c                   endif
110200110103     c*
110300080317    2c                   endif
110400080317    1c                   endif
110500110103     c*
110600080317    1C                   ENDIF
110700110103     c*
110800070201     C                   CLEAR                   BRVnvr
110900961213     C* FILIALE GESTIONE LA PRENDO DAL FOGLIO
111000961213     C                   CLEAR                   BRVDTR
111100961213     C                   CLEAR                   BRVFTR
111200991223     C                   CLEAR                   BRVHTR
111300961213     C                   Z-ADD     BARFGS        BRVFGS
111400961213     C                   Z-ADD     BARNPG        BRVNPG
111500990708     C                   Z-ADD     BARNFV        BRVNFV
111600040428     C* P.O. CHE HA EFFETTUATO LA SPUNTA (l'originale contenuto il WPES
111700040428     c*  e non più il bpes, per le spunte cat.1)
111800040428     C                   Z-ADD     WPES          BRVPES
111900961213     C                   MOVEL     BARLNP        BRVLNP
112000961213     C                   MOVEL     BARLNA        BRVLNA
112100961213     C                   MOVEL     BARNRS        BRVNRS
112200961213     C                   MOVEL     BARNSC        BRVNSC
112300961213     C                   Z-ADD     BARZNC        BRVZNC
112400961213     C**
112500961213     C* SE SONO IN IMMISSIONE LA AGGIORNO, IN VARIAZIONE SOLO
112600970829     C*  SE E' PISTOLA CON PRIORITA' < O UGUALE ALLA PRESENTE
112700961213     C*
112800961213     C* SE L'ANOMALIA GIA' ESISTE, NON LA TOLGO SE SU QUESTA SPUNTA
112900961213     C*  NON C'E'
113000040324    1C     *IN31         IFEQ      *ON
113100010608     C     WBRVE         OREQ      'S'
113200961213     C                   MOVEL     BARCAN        BRVCAN
113300961213     C                   MOVEL     BARNPS        BRVNPS
113400070212     C                   MOVEL     BARtap        BRVtap
113500040324     C                   Z-ADD     BARVUC        BRVVUC
113600040324     C                   Z-ADD     BARPUC        BRVPUC
113700040324   x1C                   ELSE
113800961213     C* CODICE ANOMALIA
113900961213     C     BARCAN        IFNE      ' '
114000980205     C     BRVCAN        OREQ      'E'
114100961213     C                   MOVEL     BARCAN        BRVCAN
114200961213     C                   ENDIF
114300110103     c*
114400970829     C* CERCO LA PRIORITA' DELLA PISTOLA GIA' PRESENTE
114500970829     C                   MOVEL     BRVNPS        ALF2
114600970829     C                   EXSR      RICRAG
114700970829     C* PISTOLA CON PRIORITA' < O = ALLA PRESENTE: E' + IMPORTANTE
114800970829     C     OPS(Y)        IFLE      OPS(Z)
114900961213     C                   MOVEL     BARNPS        BRVNPS
115000070212     C                   MOVEL     BARtap        BRVtap
115100961213     C                   ELSE
115200961213     C                   MOVEL     BRVNPS        BARNPS
115300070212     C                   MOVEL     BRVtap        BARtap
115400961213     C                   ENDIF
115500110103     c*
115600041011     c* Peso e volume prendo se pieno e se non c'e' già
115700041011     c                   if        barpuc>0
115800041011     c                   if        *in31 or (not *in31 and brvpuc=0)
115900040324     C                   Z-ADD     BARPUC        BRVPUC
116000040324     c                   endif
116100041011     c                   endif
116200040324     c                   if        barvuc>0
116300041011     c                   if        *in31 or (not *in31 and brvvuc=0)
116400040324     C                   Z-ADD     BARvUC        BRVvUC
116500040324     c                   endif
116600041011     c                   endif
116700110103     c*
116800040324    1C                   ENDIF
116900961213     C*
117000070212     c* Se la spunta era già stata caricata con errore (di solito
117100070212     c*  per la non creazione della anomalia 50) non aggiorno la data/ora
117200070212     c*  caricamento spunta
117300070212     c                   if        barerr<>'E' or *in31
117400961213     C                   Z-ADD     WDATE1        BRVDCS
117500070131     C                   time                    BRVHCS
117600070212     c                   endif
117700140613     c* impostazione data caricamento spunte
117800140613     c                   exsr      impoDFS
117900140613     c                   z-add     wdfs          brvdfs
118000140613     c
118100070131     c                   clear                   brvhms
118200070131     C                   movel     BARHMS        BRVHMS
118300070212     c                   z-add     barmis        brvmis
118400961213     C                   Z-ADD     BARFLE        BRVFLE
118500070212     C                   movel     BARpru        BRVpru
118600080314     c
118700080314     c
118800961213     C                   ENDSR
118900941206     C**************************************************************************
119000941206     C*    CARICO TABELLE
119100941206     C**************************************************************************
119200941229     C     CARTAB        BEGSR
119300930521     C*
119400930521     C* PARAMETRI UTENTE
119500110103     C                   Z-ADD     1             CODUT             1 0
119600110103      *
119700110103      * Reperimento dati utente/aziendali
119800110103     c                   exsr      sr_DatiJob
119900970320     C*
120000970320     C                   CLEAR                   SAVBOL
120100961212     C**
120200961212     C**  CARICO TABELLA 7J - CODICI PISTOLA
120300961122    1C     *IN02         IFEQ      '0'
120400921229     C                   MOVEL     '7J'          COD
120500990108     C                   Z-ADD     1             X                 4 0
120600921229     C     KTAB2         SETLL     TABEL
120700950927     C     KTAB2         READE     TABEL                                  99
120800921229     C*
120900961122    2C     *IN99         DOWEQ     *OFF
121000961122    3C     TBLFLG        IFEQ      ' '
121100921229     C                   MOVEL     TBLKEY        NPS(X)
121200921229     C                   MOVEL     TBLUNI        DS7J
121300921229     C                   MOVEL     §7JRPS        RPS(X)
121400000210     C                   MOVEL     §7JTPS        TPS(X)
121500000210     C                   MOVEL     §7JORD        OPS(X)
121600000316     C                   MOVEL     §7JINV        INV(X)
121700040503     C                   MOVEL     §7Jfsl        fsl(X)
121800921229     C                   ADD       1             X
121900961122    3C                   END
122000921229     C*
122100950927     C     KTAB2         READE     TABEL                                  99
122200961122    2C                   ENDDO
122300961122    1C                   ENDIF
122400961122     C**
122500961212     C** CARICO TABELLA  TV - TIPI TRAINO DEFLUENZA
122600961122     C                   MOVEL     'TV'          COD
122700990108     C                   Z-ADD     1             X
122800961122     C     KTAB2         SETLL     TABEL
122900961122     C     KTAB2         READE     TABEL                                  99
123000961122     C*
123100961122    2C     *IN99         DOWEQ     *OFF
123200961122    3C     TBLFLG        IFEQ      ' '
123300961122     C                   MOVEL     TBLUNI        DSTV
123400961122    4C     §TVDEF        IFEQ      'S'
123500961122     C                   MOVEL     TBLKEY        TTR(X)
123600961122     C                   ADD       1             X
123700961122    4C                   ENDIF
123800961122    3C                   ENDIF
123900961122     C*
124000961122     C     KTAB2         READE     TABEL                                  99
124100961122    2C                   ENDDO
124200961212     C**
124300961212     C* CARICO LA TABELLA 7N - DELLE CATEGORIE SPUNTE
124400941214     C                   MOVEL     '7N'          COD
124500941214     C                   Z-ADD     1             X
124600941214     C     KTAB2         SETLL     TABEL
124700950927     C     KTAB2         READE     TABEL                                  99
124800941214     C*
124900950927     C     *IN99         DOWEQ     '0'
125000941214     C     TBLFLG        IFEQ      ' '
125100941214     C                   MOVEL     TBLKEY        CAT(X)
125200941214     C                   MOVEL     TBLUNI        DS7N
125300941214     C                   MOVEL     §7NNST        NST(X)
125400941214     C                   ADD       1             X
125500941214     C                   ENDIF
125600941214     C*
125700950927     C     KTAB2         READE     TABEL                                  99
125800941214     C                   ENDDO
125900961212     C**
126000960702     C** CARICO TABELLA 5F - FILIALI CONFERMA BOLLE DA DISK
126100960702     C                   MOVEL     '5F'          COD
126200960702     C                   Z-ADD     0             X
126300960702     C     KTAB2         SETLL     TABEL
126400960702     C     KTAB2         READE     TABEL                                  99
126500960702     C*
126600960702    1C     *IN99         DOWEQ     '0'
126700960702    2C     TBLFLG        IFEQ      ' '
126800070201     C                   MOVEL     TBLKEY        W003A             3
126900070201     C     W003A         IFNE      '   '
127000960702     C                   ADD       1             X
127100960702     C                   MOVEL     TBLKEY        S5F(X)
127200970828     C* MEMORIZZO LA FIL CHE CONFERMA
127300970828     C                   MOVEL     TBLUNI        F5F(X)
127400110103     c*
127500130618     C     X             IFGE      9000
127600070201     C                   SETON                                        99
127700070201     C                   ENDIF
127800110103     c*
127900970828     C                   ENDIF
128000960702    2C                   ENDIF
128100960702     C*
128200960702     C  N99KTAB2         READE     TABEL                                  99
128300960702    1C                   ENDDO
128400961212     C**
128500020204     C** CARICO TABELLA 3E - CODICI ANOMALIA VALIDI SU SPUNTE
128600020204     C                   MOVEL     '3E'          COD
128700961212     C                   Z-ADD     0             X
128800961212     C     KTAB2         SETLL     TABEL
128900961212     C     KTAB2         READE     TABEL                                  99
129000961212     C*
129100961212    1C     *IN99         DOWEQ     *OFF
129200961212    2C     TBLFLG        IFEQ      ' '
129300961212     C                   ADD       1             X
129400020204     C                   MOVEL     TBLKEY        CAN(X)
129500961212     C     X             IFGE      200
129600961212     C                   SETON                                        99
129700961212     C                   ENDIF
129800961212    2C                   ENDIF
129900961212     C*
130000961212     C  N99KTAB2         READE     TABEL                                  99
130100961212    1C                   ENDDO
130200020502     C*****
130300020502     C* RILEVO DATA E ORA PER SCRIVERLO NELLE SPUNTE
130400020502     C                   TIME                    WTIME            14 0
130500020502     C                   MOVE      WTIME         WDATE             8 0
130600020502     C                   Z-ADD     WDATE         G02DAT
130700020502     C                   Z-ADD     *ZERO         G02INV
130800020502     C                   MOVEL     *BLANKS       G02ERR
130900020502     C                   CALL      'XSRDA8'
131000020502     C                   PARM                    WLBDAT
131100020502     C                   Z-ADD     G02INV        WDATE1            8 0
131200030725      *
131300030725      * CHAIN A VARIABILI PARTENZE x PRENDERE MAX PESO E VOLUME CARICABILI
131400030725     C                   EXSR      CERVPO
131500970522     C*
131600970522     C* CHAIN A VARIABILI ARRIVI PER VEDERE A QUALE LIVELLO SI CHIUDE
131700970522     C* L'IDD (LIV MAX.)
131800970522     C                   MOVEL     '7A'          COD
131900970522     C                   MOVEL(P)  '2'           KEY
132000970522     C     KTAB          CHAIN     TABEL                              99
132100970522     C  N99              MOVEL     TBLUNI        DS7A2
132200000927     C* CHAIN A GG PULIZIA PER CREAZIONE ANOMLIA 10 E 50
132300000927     C                   MOVEL     '5A'          COD
132400000927     C                   MOVEL(P)  '1'           KEY
132500000927     C     KTAB          CHAIN     TABEL                              99
132600000927     C  N99              MOVEL     TBLUNI        DS5A
132700000927     C*
132800000927     C                   MOVEL     '5A'          COD
132900000927     C                   MOVEL(P)  'PT'          KEY
133000000927     C     KTAB          CHAIN     TABEL                              99
133100000927     C  N99              MOVEL     TBLUNI        DS5APT
133200000927     C*
133300000927     C* PREDO GG MINORI TRA PULIZIA ARB E PULIZIA ARB POSTE
133400000927     C     §5AARB        IFLT      §5APT5
133500000927     C                   Z-ADD     §5AARB        WGGPUL            3 0
133600000927     C                   ELSE
133700000927     C                   Z-ADD     §5APT5        WGGPUL            3 0
133800000927     C                   ENDIF
133900000927     C**
134000000927      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA BOLLE ARRIVI
134100000927     C                   MOVE      *BLANKS       $DAT
134200000927     C                   MOVE      *BLANKS       $ERR
134300000927     C                   MOVEL     'FNLR84R'     $PGM
134400000927     C                   CALL      'TRUL49C'                            28
134500000927     C                   PARM                    $PGM             10
134600000927     C                   PARM                    $DAT              8
134700000927     C                   PARM                    $ERR              1
134800000927     C*
134900000928    3C     $ERR          IFEQ      '1'
135000000927     C     *IN28         OREQ      *ON
135100000928     C     $DAT          ORLE      *ZEROS
135200000927     C                   Z-ADD     WDATE1        G02INV
135300000927     C                   ELSE
135400000928     C                   MOVEL     $DAT          G02INV
135500000927    3C                   ENDIF
135600000927     C*
135700000927      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI
135800000927      * DI PULIZIA BOLLE ARRIVI
135900000927     C                   Z-ADD     *ZEROS        G02DAT
136000000927     C                   Z-ADD     *ZEROS        G02TGI
136100000927     C                   MOVEL     '3'           G02ERR
136200000927     C                   CALL      'XSRDA8'
136300000927     C                   PARM                    WLBDAT
136400000927      *
136500000927     C                   SUB       WGGPUL        G02TGI
136600000928     C                   Z-ADD     *ZEROS        GI8DAT
136700000928     C                   Z-ADD     *ZEROS        GI8INV
136800000928     C                   Z-ADD     G02TGI        GI8TGI
136900000927     C                   CALL      'XSRGI8'
137000000927     C                   PARM                    WGIDAT
137100000927      *
137200000928     C                   Z-ADD     GI8INV        PRIDAT
137300961212     C**
137400070209     c** udate - 4 --> sono i giorni che devo tenere le spunte dentro al
137500070212     c**               FIBAR se non riesco a creare l'anomalia collo
137600021203     c**               arrivato non part
137700021203     C                   Z-ADD     WDATE1        G02INV
137800021203     C                   Z-ADD     *ZEROS        G02DAT
137900021203     C                   Z-ADD     *ZEROS        G02TGI
138000021203     C                   MOVEL     '3'           G02ERR
138100021203     C                   CALL      'XSRDA8'
138200021203     C                   PARM                    WLBDAT
138300021203      *
138400021203     C                   SUB       4             G02TGI
138500021203     C                   Z-ADD     *ZEROS        GI8DAT
138600021203     C                   Z-ADD     *ZEROS        GI8INV
138700021203     C                   Z-ADD     G02TGI        GI8TGI
138800021203     C                   CALL      'XSRGI8'
138900021203     C                   PARM                    WGIDAT
139000021203     C                   Z-ADD     GI8INV        wpul050
139100961212     C**
139200021203     C**
139300930521     C                   MOVEL     '1'           UNO               1
139400930521     C                   MOVEL     'AA'          SAVNPS            2
139500960411     C*
139600960411     C                   MOVE      ' '           OPN9              1
139700961212     C*
139800021014     C                   CLEAR                   DSBS55
139900971113     C                   MOVEL     'L'           I50TLA
140000021014     C                   CALL      'TIBS55R'
140100021014     C                   PARM                    DSBS55
140200971113     C**
140300971113     C                   CLEAR                   KPJBA
140400021014     C                   MOVEL     'EDPCED'      WCED
140500021014     C                   MOVEL     SIMFEL        WTER
140600021014     C                   MOVEL     WPROF         KNMUS
140700971113     C                   MOVEL     O50PSI        KNSIF
140800970829     C                   MOVEL     'DSP01'       KNMTD
140900961212     C                   MOVEL     'N'           KSTJO
141000961212     C                   MOVEL     'B'           KTPAZ
141100961212     C                   MOVEL     'AZNAUTO'     KNMEB
141200961212     C                   MOVEL     'J'           KEXCN
141300961212     C                   MOVEL     'P5'          KCOJB
141400961212     C                   MOVEL     'N'           KCANC
141500000927     C*
141600000927     C                   CLEAR                   SA2DFV
141700001109     C**
141800001109     C** CARICO TABELLA 7C - ANOMALIE CREABILI DA CAT.SPUNTA GENERICA
141900001109     C                   MOVEL     '7C'          COD
142000001109     C                   Z-ADD     0             X
142100001109     C     KTAB2         SETLL     TABEL
142200001109     C     KTAB2         READE     TABEL                                  99
142300001109     C*
142400001109    1C     *IN99         DOWEQ     *OFF
142500001109    2C     TBLFLG        IFEQ      ' '
142600001109     C                   MOVEL     TBLUNI        DS7C
142700001109    3C     §7CCCG        IFEQ      'S'
142800001109     C                   ADD       1             X
142900001109     C                   MOVEL     TBLKEY        ACG(X)
143000001109    3C                   ENDIF
143100001109    2C                   ENDIF
143200001109     C*
143300001109     C     KTAB2         READE     TABEL                                  99
143400001109    1C                   ENDDO
143500110103      *
143600091209      * Aggancio tabella AR5 per record 'GEN'
143700091209     c                   Clear                   dAr5
143800091209     c                   Clear                   dsbs02
143900091209     c                   Eval      T02Mod = 'C'
144000091209     c                   Eval      T02Sif = knsif
144100091209     c                   Eval      T02Cod = 'AR5'
144200091209     c                   Movel(p)  'GEN'         T02Ke1
144300091209     c                   Call      'TIBS02R'
144400091209     c                   Parm                    Kpjba
144500091209     c                   Parm                    dsbs02
144600091209     c                   If        T02Err = *Blanks
144700091209     c                   Movel     T02Uni        dAr5
144800091209     c                   EndIf
144900921229     C*
145000911014     C                   ENDSR
145100941206     C**************************************************************************
145200941206     C* CREO ANOMALIA
145300941206     C**************************************************************************
145400911016     C     CRTANM        BEGSR
145500000320     C                   CLEAR                   WCCH
145600970521     C*
145700970521     C                   CLEAR                   WANM10
145800941206     C*
145900970204    1C     COMCAA        IFNE      645
146000990518     C     KANM          CHAIN     FNANM000                           96
146100970204   X1C                   ELSE
146200961205     C*
146300990518     C     KANM          CHAIN(N)  FNANM000                           96
146400961205     C* PER L'ANOMALIA 645 LEGGO FINTANTO CHE NON TROVO UNA ANOMALIA
146500961205     C*  CON DATA = ALLA DATA SPUNTA: SE C'E' NON CREO
146600970204    2C     *IN96         DOWEQ     *OFF
146700990518     C     ANMDAO        ANDNE     BARDFV
146800990518     C     KANM          READE(N)  FNANM000                               96
146900970204    2C                   ENDDO
147000961205     C*
147100970204    1C                   ENDIF
147200000811     C*
147300000811    1C     *IN96         IFEQ      *ON
147400000811     C*
147500000811     C     COMCAA        ORNE      645
147600000811     C     ANMAIE        ANDEQ     'I'
147700000811     C* PER ORA SOLO SULLE INTERNE RICREO ANOMLIA SE PRESENTE
147800970204     C*  - SE CHIUSE SEMPRE
147900040923     C*  - SE APERTA SOLO LA DATA E' < DELLA ESISTENTE
148000040923     c* Per la 200 la ricreo solo se e' chiusa senza guardare la data creaz
148100961205     C*
148200970204     C                   MOVEL     'S'           WWRITE
148300970204    2C     *IN96         IFEQ      *OFF
148400970204     C     ANMDCH        ANDEQ     0
148500970204     C     ANMDAO        ANDGE     BARDFV
148600041005    2C     *IN96         oreq      *OFF
148700041005     C     ANMDCH        andeq     0
148800040923     C     comcaa        ANDeq     200
148900970204     C                   MOVEL     ' '           WWRITE            1
149000970204    2C                   ENDIF
149100970204     C**
149200070215    2C     WWRITE        IFEQ      'S'
149300990518     C                   CLEAR                   FNANM000
149400950927     C                   EXSR      RIEANM
149500000614     C**
149600001109     C* CREO SOLO SE ANOMALIA :
149700001109     C* CREABILE DA QUALUNQUE CATEGORIA SPUNTA
149800070215    3C     §7CCCG        ifeq      'S'
149900001109     C* NON CREABILE DA CATEG. GENERICA E LA CATEGORIA NON È GENERICA
150000070215     C     BARSPG        orne      'I'
150100950927     C*
150200950927     C                   MOVE      COMLNP        ANMLNP
150300990518     C                   Z-ADD     COMAAS        ANMAAS
150400950927     C                   MOVE      COMNSP        ANMNSP
150500960524     C                   MOVE      BARDFV        ANMDAO
150600990518     C                   Z-ADD     BARNFV        ANMNFV
150700950927     C                   MOVEL     BARFGS        ANMCDU
150800040621     c* imposto se spedizione di valore
150900070215    4c                   if        anmnsp>0
151000091209     c     kar5          chain(n)  fiar501l
151100070215    5c                   if        %found(fiar501l)
151200040621     c                   movel     ar5uni        dar5gen
151300040621     c                   else
151400040621     c                   clear                   dar5gen
151500070215    5c                   endif
151600040621     c                   movel     §ar5bva       anmft4
151700070215    4c                   endif
151800110103     c*
151900040429     c* 22/4/4 --> al posto di anmfgs imposto BPES il p.o. che spara
152000040429     c*            che solo per le spunte partenza è sempre barfgs
152100040429     C                   MOVEL     bpes          ANMFLE
152200070215    4C     ANMCAA        IFEQ      10
152300150205    4C     ANMCAA        oreq      410
152400070215     C                   z-add     BARZNC        ANMfl4
152500070215    4C                   ENDIF
152600911112     C*
152700921020     C* SE ESTERNA SCRIVO FIL A CUI TRASMETTERE
152800070215    4C     §7CAIE        IFEQ      'E'
152900070215    5C     COMLNP        IFGT      0
153000960528     C                   MOVEL     COMLNP        ANMFL1
153100960528     C                   ELSE
153200960704     C                   MOVEL     LNPB          ANMFL1
153300070215    5C                   ENDIF
153400070215    4C                   ENDIF
153500911016     C*
153600970204     C* SE INTERNA AUTOCHIUSA E C'E' GIA' NON FACCIO NULLA
153700070215    4C     *IN96         IFEQ      *ON
153800970204     C     §7CCHA        ORNE      'S'
153900960430     C* SE LA KEY E' PER SEGNACOLLO ED IL NUMERO DI SEGNACOLLO E' 0
154000960430     C*  NON CREO L'ANOMALIA (ERRORE DA TERMINALINO)
154100070215    5C     §7CKEY        IFNE      'S'
154200960430     C     §7CKEY        OREQ      'S'
154300960430     C     ANMSCN        ANDGT     0
154400990922     C*
154500990922     C* LA DATA I APERTURA ANOMALIA CI DEVE ESSERE , ALTRIMENTI
154600990922     C*  E' MEGLIO NON CREARE L'ANOMALIA
154700070215    6C     ANMDAO        IFGT      0
154800990518     C   96              WRITE     FNANM000
154900990518     C  N96              UPDATE    FNANM000
155000070215    6C                   ENDIF
155100070215    5C                   ENDIF
155200961205     C*
155300970204     C* SE CREO ANOAMLIA 145 O 146 CHIUDO LE ANOMALIE INTERNE 140 E 130
155400070215    5C     ANMCAA        IFEQ      145
155500970204     C     ANMCAA        OREQ      146
155600970204     C                   Z-ADD     130           COMCAA
155700970204     C                   EXSR      CHIUAN
155800970204     C                   Z-ADD     140           COMCAA
155900970204     C                   EXSR      CHIUAN
156000070215    5C                   ENDIF
156100070215    4C                   ENDIF
156200970204     C*
156300070215    3C                   ENDIF
156400070215    2C                   ENDIF
156500970521   X1C                   ELSE
156600110103     c*
156700970521     C* OPERAZIONI PER ANOMALIA 10 GIA' ESISTENTE MA CHIUSA:
156800970521    2C     COMCAA        IFEQ      10
156900970522     C* Se anomalia 10 a liv massimo --> memorizzo per creaz.anom.410
157000970522    3C     ANMLID        IFEQ      §7ALCI
157100970521     C                   MOVE      '1'           WANM10            1
157200970522    3C                   END
157300970522     C* Se anomalia chiusa (non pratica IDD):
157400970522     C* 1) sflaggo e chiudo l'anomalia mettendo cch='PR' se è a liv max
157500970522    3C     ANMDCH        IFGT      *ZEROS
157600970522     C     ANMCCH        ANDNE     'PR'
157700970522     C*
157800970522    4C                   SELECT
157900970522     C     ANMLID        WHENEQ    §7ALCI
158000970522     C     ANMCCH        ANDEQ     'AN'
158100970522     C                   MOVEL     'PR'          ANMCCH
158200970522     C                   MOVEL     'D'           ANMFSC
158300970522     C                   CLEAR                   ANMFT1
158400970522     C                   CLEAR                   ANMFT2
158500970522     C     ANMLID        WHENLT    §7ALCI
158600970523     C                   CLEAR                   ANMCCH
158700970522     C                   CLEAR                   ANMDCH
158800970522     C                   CLEAR                   ANMFSC
158900970522    4C                   ENDSL
159000990518     C                   UPDATE    FNANM000
159100970522    3C                   ENDIF
159200970522    2C                   ENDIF
159300970204    1C                   ENDIF
159400970204     C*
159500990518     C                   UNLOCK    FNANM02L
159600911016     C                   ENDSR
159700960613     C**************************************************************************
159800960613     C*    CONTROLLO IL RAGGRUPPAMENTO DELLA PISTOLA
159900960613     C**************************************************************************
160000960613     C     PISMAN        BEGSR
160100960613     C*
160200961213     C                   Z-ADD     1             Y                 3 0
160300961213     C*
160400960613    1C     *IN02         IFEQ      *OFF
160500960613     C     BARNPS        LOOKUP    NPS(Y)                                 34
160600960613     C  N34              Z-ADD     1             Y
160700960613     C*
160800960613   X1C                   ELSE
160900960613     C*
161000960613     C                   MOVEL     '7J'          COD
161100960613     C                   MOVEL(P)  BARNPS        KEY
161200960613     C     KTAB          CHAIN     TABEL                              34
161300000316     C     *IN34         IFEQ      *OFF
161400000316     C                   MOVEL     TBLUNI        DS7J
161500000316     C                   MOVEL     §7JRPS        RPS(Y)
161600000316     C                   MOVEL     §7JORD        OPS(Y)
161700000316     C                   MOVEL     §7JTPS        TPS(Y)
161800000316     C                   MOVEL     §7JINV        INV(Y)
161900040503     C                   MOVEL     §7Jfsl        fsl(Y)
162000000316     C                   ELSE
162100000316     C                   MOVEL     'M'           RPS(Y)
162200000316     C                   MOVEL     999           OPS(Y)
162300000316     C                   MOVEL     'B'           TPS(Y)
162400000316     C                   MOVEL     ' '           INV(Y)
162500040503     C                   MOVEL     ' '           fsl(Y)
162600960613    1C                   END
162700000316    1C                   END
162800960613     C                   ENDSR
162900941206     C**************************************************************************
163000020213     C*    A G G I O R N O    R E C O R D    B O L L E
163100941206     C**************************************************************************
163200980212     C     UPDARB        BEGSR
163300960521     C                   CLEAR                   WNOTRA
163400961210     C                   CLEAR                   W60X
163500961211     C                   CLEAR                   WESAN
163600961210     C                   MOVEL     'S'           W620
163700961211     C                   MOVEL     'S'           WCREA
163800021106     C                   CLEAR                   WDCM
163900021106     C                   CLEAR                   WBOAR
164000021106     C                   CLEAR                   WATDCM
164100040430     C                   CLEAR                   WABCCA
164200040430     C                   CLEAR                   WABNDC
164300161007     C                   CLEAR                   gcpdgc            8 0
164400021106     C                   MOVEL     ' '           WGIAC             1
164500941206     C*
164600921229     C* TROVO TIPO E RAGGRUPPAMENTO PISTOLE DI QUELLA CARICATA
164700960613     C                   EXSR      PISMAN
164800921229     C*
164900960523     C* CARICO LA L6 DELLA FILIALE GESTIONE
165000020213     C     BPES          IFNE      SAVPES
165100020213     C                   MOVEL     BPES          SAVPES
165200961209     C                   EXSR      CARL6
165300960523     C                   ENDIF
165400921013     C*
165500041012     C                   SETOFF                                         0332
165600950922     C                   CLEAR                   COMLNP
165700950922     C                   CLEAR                   COMAAS
165800950922     C                   CLEAR                   COMNSP
165900950922     C                   CLEAR                   WSPLOC
166000950927     C                   CLEAR                   WTIBOL
166100970423     C                   CLEAR                   WWFVC
166200020124     C                   CLEAR                   LNPB
166300020124     C                   CLEAR                   WTAB5F
166400020131     C                   CLEAR                   WESART
166500020131     C                   CLEAR                   WESBLT
166600020131     C                   CLEAR                   WESBTT
166700130228     c                   clear                   bolxco
166800160421     c                   clear                   bolznc
166900021022     c*
167000021125     C* TERMINAL partenza DI BPES
167100021125     C                   MOVEL     'P'           WTITER
167200021022     C                   MOVEL     BARDFV        WDTRIF
167300021022     C                   EXSR      TERPES
167400021022     C                   Z-ADD     D55TFP        PESTFP
167500021203     c*
167600040422     c* Se trovo la bolla transito x p.o. spunta --> aggiorno transito, senz
167700021203     c*  controllo i terminal (c'e' un problema a livello di eccezioni ter
167800021203     C     KBAR37        CHAIN     FNBTT37L                           3739
167900021203     C   39              GOTO      ENDUPD
168000021203     C  N37KBTT          CHAIN     FNBTP11L                           37
168100021203     C**
168200021203    1C     *IN37         IFEQ      *OFF
168300021203     C                   MOVEL     'S'           WESBTT            1
168400021203     C                   MOVEL     BTTLNP        LNPB
168500021203     C                   MOVEL     BTPDSP        DSPB
168600021203     C                   MOVEL     BTPDSP        WDTRIF
168700021203     C                   MOVEL     BTPTFP        LNPTFP
168800050603     C                   MOVEL     BTPTFa        bolTFa
168900040621     C                   MOVE      BTTLNP        COMLNP
169000040621     C                   MOVE      BTTAAS        COMAAS
169100040621     C                   MOVE      BTTNSP        COMNSP
169200021203   X1C                   ELSE
169300021203     C                   MOVEL     'N'           WESBTT
169400020124     C**
169500020124     C** VEDO SE TROVO LA BOLLA IN BLT
169600020124     C     KBAR          CHAIN     FNBLT27L                           3239
169700020213     C   39              GOTO      ENDUPD
169800020213     C  N32KBLT          CHAIN(N)  FNBLP01L                           32
169900020124     C**
170000021203    2C     *IN32         IFEQ      *OFF
170100020131     C                   MOVEL     'S'           WESBLT            1
170200020124     C                   MOVEL     BLTLNP        LNPB
170300020201     C                   MOVEL     BLPTFP        LNPTFP
170400050603     C                   MOVEL     BLPTFa        bolTFa
170500020201     C                   MOVEL     BLPDSP        WDTRIF
170600020214     C                   MOVEL     BLPDSP        DSPB
170700130301     c                   movel     BLPxco        bolxco            1
170800160421     c                   movel     BLPznc        bolznc            2
170900040621     C                   MOVE      BLTLNP        COMLNP
171000040621     C                   MOVE      BLTAAS        COMAAS
171100040621     C                   MOVE      BLTNSP        COMNSP
171200021203   X2C                   ELSE
171300020213     C                   MOVEL     'N'           WESBLT
171400020124     C** SE NON TROVO LA BOLLA PARTENZA CERCO NEGLI ARRIVI
171500020124     C     KBAR          CHAIN     FNART27L                           3239
171600020213     C   39              GOTO      ENDUPD
171700050223     C  N32KARB          CHAIN(N)  FNARB01L                           32
171800020124     C**
171900021203    3C     *IN32         IFEQ      *OFF
172000020201     C                   MOVEL     'S'           WESART            1
172100020124     C                   MOVEL     ARTLNP        LNPB
172200020214     C                   MOVEL     ARBDSP        DSPB
172300020201     C                   MOVEL     ARBDSP        WDTRIF
172400020201     C                   MOVEL     ARBTFP        LNPTFP
172500050603     C                   MOVEL     ARBTFa        boltfa
172600130301     c                   movel     ARBxco        bolxco            1
172700160421     c                   movel     ARBznc        bolznc
172800040621     C                   MOVE      ARTLNP        COMLNP
172900040621     C                   MOVE      ARTAAS        COMAAS
173000040621     C                   MOVE      ARTNSP        COMNSP
173100021203   X3C                   ELSE
173200020213     C                   MOVEL     'N'           WESART
173300020131     C** IMPOSTO LNPB = BARLNP
173400020131     C                   MOVEL     BARLNP        LNPB
173500020214     C                   Z-ADD     BARDFV        DSPB
173600050603     c                   clear                   boltfa
173700020124     C**
173800020124     C** SE NON TROVATA MAI LA BOLLA, CERCO SE ESISTE LA EVENTUALE
173900020124     C*   SPUNTA PARTENZA E LA TABELLA 5F
174000020205    4C     BARNRS        IFGT      *ZEROS
174100020124     C                   EXSR      RICLNP
174200020213    4C                   ENDIF
174300021105     c* prendo terminal di partenza della linea partenza calcolata
174400070222     c                   if        bardfv=0
174500021105     C                   Z-ADD     BARDFV        WDTRIF
174600070222     c                   else
174700070222     c                   z-add     wdate1        wdtrif
174800070222     c                   endif
174900070222     c
175000021105     C                   EXSR      TERPAR
175100020213    3C                   ENDIF
175200020213    2C                   ENDIF
175300020213    1C                   ENDIF
175400020204     C**
175500020215     C** CONTROLLO TRA P.O. SPUNTA E LINEE SPUNTE CHE AGGIONAMENTI
175600020213     C**  POSSO FARE
175700021125     c*
175800050215     C* TERMINAL arrivo DI BPES
175900021125     C                   MOVEL     'A'           WTITER
176000021125     C                   MOVEL     BARDFV        WDTRIF
176100021125     C                   EXSR      TERPES
176200021125     C                   Z-ADD     D55TFA        PESTFA
176300021125     c
176400020204     C                   EXSR      CTRAGG
176500020213     C**
176600020213     C** A G G I O R N A M E N T O   B O L L E   P A R T E N Z A
176700020213    1C     WAGPAR        IFEQ      'S'
176800021203    2C     WESblt        IFEQ      ' '
176900021203     C     KBAR          CHAIN     FNBLT27L                           3239
177000021203     C   39              GOTO      ENDUPD
177100021203     C  N32              MOVEL     'S'           WESBLT            1
177200021203     C   32              MOVEL     'N'           WESBLT            1
177300021203    2C                   ENDIF
177400021203     c**
177500021203    2C     WESBLT        ifeq      'S'
177600150817     c* Aggiorno solo se la bolla NON parte da + filiali
177700150817     c* oppure sel la bolla per LINEA parte da + filiali ma è mia bolla
177800150817     c* oppure se chi spara è la linea partenza della bolla che parte da + fil
177900150622     c                   if        wbsptp=' ' or wbsptp='L'
178000150622     c                             or bpes=lnpb
178100020213     C                   EXSR      AGGBLT
178200150622     c                   else
178300150622     c                   clear                   wagPAR
178400150622     c                   endif
178500150622     c
178600021203    3C     WEND          IFEQ      '1'
178700020213     C                   GOTO      ENDUPD
178800021203    3C                   ENDIF
178900021203    2C                   ENDIF
179000020213     C*
179100021203    2C     WESBLT        ifeq      'N'
179200020204     C                   MOVEL     'N'           WBOPAR            1
179300020204     C                   MOVEL     'P'           WTIBOL            1
179400021203    2C                   ENDIF
179500021203    1C                   ENDIF
179600020213     C*
179700020213    1C     WAGPAR        IFNE      'S'
179800020213     C     WESBLT        ANDEQ     'S'
179900020213     C                   UNLOCK    FNBLT27L
180000020213    1C                   ENDIF
180100020213     C**
180200020213     C* A G G I O R N O   B O L L E   A R R I V I
180300020213     C* A PRESCINDERE DAL FATTO CHE TROVI O MENO LA BOLLA
180400020213    1C     WAGARR        IFEQ      'S'
180500020220     C* LEGGO LE BOLLE ARRIVI ANCHE PER FLAG=6.
180600020220     C*  SI TRATTA DI SPUNTA DI DISGUIDO PER CUI NON AGGIORNEREBBE
180700020220     C*  LA BOLLA ARRIVI MA MI SERVE PER ENTRARE IN CTRA60
180800070221    1C     dls45FLG      OREQ      '6'
180900020213     C* SE NON HO ANCORA CHAINATO IL DETTAGLIO COLLI LO FACCIO ORA
181000020213    2C     WESART        IFEQ      ' '
181100020213     C     KBAR          CHAIN     FNART27L                           3239
181200020218     C   39              GOTO      ENDUPD
181300020213     C  N32              MOVEL     'S'           WESART            1
181400020214     C   32              MOVEL     'N'           WESART            1
181500020213    2C                   ENDIF
181600020213     C*
181700020213     C                   EXSR      AGGART
181800020213    1C                   ENDIF
181900020213     C*
182000020213    1C     WAGARR        IFNE      'S'
182100020213     C     WESART        ANDEQ     'S'
182200020213     C                   UNLOCK    FNART27L
182300020213    1C                   ENDIF
182400040428     c*
182500040428     c*Chiudo eventuale anomalia di disguido presente sul collo, se
182600040428     c*  di area <> dal'anomalia
182700040503     C* SOLO SE SPUNTA DI PISTOLA REALE
182800040503     c                   if        fsl(y)<>'F'
182900040428     c                   eval      comcaa=200
183000040428     C*
183100040503     C     Kanm          chain(N)  FNANM000
183200040428     c                   if        %found(fnanm02l)
183300040428     C                   CLEAR                   DSLV55
183400040428     C                   MOVEL     'P'           D55TPT
183500040428     C                   Z-ADD     anmfle        D55LIN
183600040428     C                   Z-ADD     bardfv        D55DRF
183700040428     C                   EXSR      FNLV55
183800040428     c                   if        pestfp<>d55tfp  or
183900040428     c                             bpes=anmlna
184000040428     c                   exsr      chiuan
184100040428     c                   endif
184200040428     c                   endif
184300040503     c                   endif
184400921013     C**
184500960531     C** T R A N S I T I  /  D I S G U I D I   I N   A R E A   E  N O N
184600921013     C**
184700070221    0C     dls45FLG      IFNE      '6'
184800911112     C*
184900960522     C* NON E' UNA SPUNTA DEL TERMINAL DI ARRIVO
185000050603    1C     Usalnatfa     IFNE      BPES
185100021022     c     wesbtt        oreq      'S'
185200960522     C*
185300960522     C* SE NEMMENO I 2 TERMINAL DI ARRIVO CORRISPONDONO
185400960522     C* --> T R A N S I T O
185500050603    2C     Usalnatfa     IFNE      PESTFA
185600021022     c     wesbtt        oreq      'S'
185700960531     C**
185800020214     C** I TRANSITI SONO ANCORA DA SISTEMARE
185900020214     C**  QUESTE 2 SPECIFICHE SONO DA ELIMINARE: INFATTI LA BOLLA POTRA
186000020214     C**  ESSERE SIA IN BLT CHE IN BTT
186100020205    3C     WAGPAR        IFEQ      ' '
186200960703     C     WBOPAR        OREQ      'N'
186300960522     C**
186400040422     C     WESBTT        ifeq      'N'
186500960521     C**
186600960612     C* SE E' SPUNTA ENTRATA O PARTENZA E NON HO TROVATO NE' LA BOLLA
186700960521     C*  PARTENZA NE' LA BOLLA TRANSITO, PER SERIE =0
186800960521     C*  IMPOSTO NELLA SPUNTA FLAG TRASMISSIONE "N" E 99999999
186900960612     C*  PER NON AUTOGENERARE LA SPUNTA PARTENZA O NON TRASMETTERLA
187000001123     C* IDEM PER SPUNTA IMI (PER BOLLE POSTE AUTOGENERA ANCHE DA SPUNTE
187100001123     C* IMI)
187200960522    5C     BARNPG        IFEQ      5
187300020205     C     BARNPG        OREQ      1
187400001123     C     BARNPG        OREQ      3
187500001123     C     BARSPG        ANDEQ     'I'
187600020205    6C     BARNRS        IFEQ      0
187700020205     C     WAGPAR        ANDEQ     'S'
187800960521     C     WBOPAR        ANDEQ     'N'
187900960521     C                   MOVEL     'S'           WNOTRA            1
188000020205    6C                   ENDIF
188100960612    5C                   ENDIF
188200040120     c
188300911016     C*
188400110124     C* SE NON E' BOLLA LOCALE e nemmeno in £6 : DISGUIDO
188500150817     c*  non è disguido nemmeno se collo che parte da altra filiale
188600150817     c*  ( la bolla deve essere spostata di lnp)
188700020205    5C     WAGPAR        IFEQ      ' '
188800110124    5C     WAGarr        andeq     ' '
188900150817    5C     WBSPSPU       andeq     ' '
189000150817     c
189100020215     C     BARFLE        IFEQ      BTFP
189200960522     C                   Z-ADD     200           COMCAA
189300960404     C  N05              EXSR      CRTANM
189400020215     C                   ENDIF
189500020215     C* DEVO COMUNQUE DIRE CHE SI TRATTA DI UN DISGUIDO
189600960404     C                   MOVEL     'D'           WTIBOL
189700041012     c
189800041012     c* Aggiorno anche la bolla in  arrivo
189900041012     c                   EXSR      AGGARRIVO
190000960522    5C                   ENDIF
190100960522     C**
190200911016     C* 37 OFF - TROVATA AGGIORNO BOLLA TRANSITO
190300960522   X4C                   ELSE
19040002021418   C                   EXSR      AGGBTT
190500960522    4C                   ENDIF
190600040430     c*
190700040430     c* Eventuale spunta di disguido o transito, chiude fuori posto
190800040430     c*  di bolla partenza
190900040430     C                   Z-ADD     120           COMCAA
191000040430     C                   EXSR      CHIUAN
191100970204     C*
191200970204   X3C                   ELSE
191300970204     C* SE NON E' COLLO DELLA AREA DI ARRIVO E COLLO IN PARTENZA
191400970204     C*  VEDO SE CREARE L'ANOMALIA FUORI POSTO BOLLA IN PARTENZA
191500970204     C                   EXSR      ANM120
191600990120     C** VERIFICO LA PRESENZA DI UNA C.A. PER CREARE EVENTUALI ANOM
191700000523     C** IGNORO SE E' SPUNTA CAT 4-8 SOLO IN ARRIVO(IN PART.SEMPRE)
191800990121     C                   MOVEL     'S'           WCAT4             1
191900990126     C*     GESTISCO IL RITROVAMENTO COLLO
192000990126     C                   MOVEL     'S'           WRITRO            1
192100990126     C*     GESTISCO LA CREAZIONE ANOMALIE
192200990126     C                   MOVEL     'S'           WCREAN            1
192300990120     C                   EXSR      CTRDAN
192400990120     C**
192500990120     C** SE NON TROVATA CA DI COLLO
192600990120     C* VERIFICO SE LA SPEDIZIONE E' SU UNA CA CHIUSA --> CREO
192700990120    4C     WTRDAN        IFEQ      ' '
192800040811      *
192900040811      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
193000040811      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
193100040811      * legate alla spedizione che passo
193200040811     c                   clear                   fidn12ds
193300040811     c                   eval      i12aas = BLTAAS
193400040811     c                   eval      i12lnp = BLTLNP
193500040811     c                   eval      i12nrs = BLTNRS
193600040811     c                   eval      i12nsp = BLTNSP
193700040811     c                   eval      i12fel = 'E'
193800040811     c*
193900040811     c                   call      'FIDN12R'
194000040811     c                   parm                    fidn12ds
194100040811     c*
194200040811     c                   setoff                                       33
194300040811     c*
194400040811     c* se non ci sono errori
194500040811     c                   if        o12err <> ' '
194600040811     c                   seton                                        33
194700040811     c                   else
194800040811     c* se numero di CA trovate maggiore di zero
194900040811     c                   if        o12nca = 0
195000040811     c                   seton                                        33
195100040811     c                   else
195200040811     c                   z-add     1             jj                2 0
195300040811     c                   dow       jj <= o12nca and *in33 = *off
195400040811     C     skDch(jj)     IFGT      0
195500040811     C*
195600040811     C* Valorizzo i campi DCT* utilizzati nella routine CRE151 con quelli
195700040811     C* ritornati dal *pgm d utilità FIDN12R
195800040811     C                   movel     skCch(jj)     DCTCCH
195900040811     C*
196000040811     C                   EXSR      CRE151
196100040811     C                   SETON                                        33
196200040811     C                   ENDIF
196300040811     c*
196400040811     c                   add       1             jj
196500040811     c                   enddo
196600040811     c*
196700040811     c                   seton                                        33
196800040811     c*
196900040811     c                   endif
197000040811     c                   endif
197100040811     C**
197200990120    4C                   ENDIF
197300990120     C**
197400960531    3C                   ENDIF
197500960522     C*
197600960531   X2C                   ELSE
197700970203     C*  D I S G U I D O   I N   A R E A
197800970203     C*  (SE NON E' UNA MIA BOLLA PARTENZA)
197900961021     C                   SETOFF                                       88
198000961122     C                   CLEAR                   WSTESS
198100961028     C*
198200020213    3C  N05BPES          IFNE      BARLNA
198300050930     c* al posto di BPES uso barfgs per le spunte IMP: mi fa più comodo
198400050930     c*  usare direttamente il terminal di partenza di chi spara perchè
198500050930     c*  l'IMP lo fa solo il terminal di partenza ma può essere usato
198600050930     c*  anche dai 2 livello
198700050930     C     barfgs        ANDNE     LNPB
198800960522     C     BARNPG        ANDGT     1
198900050930     C     BARNPG        ANDne     5
199000050930     c* Se spunta entrata escludo se di tratta di un aggiornamento
199100050930     c*  in partenza e non in arrivo (per cui il p.o. fa parte
199200050930     c*  dell'area di partenza
199300050930    3C     BPES          orne      BARLNA
199400050930     C     barfgs        ANDNE     LNPB
199500050930     C     BARNPG        ANDeq     5
199600050930     c     wagarr        andeq     'S'
199700050930     c
199800980727     C** CREO ANOMALIE SE COLLO SPUNTATO IN IMA ANCHE SE DA ME CONFERMA
199900020213    3C     BPES          ORNE      BARLNA
200000020213     C     BPES          ANDEQ     LNPB
200100980727     C     BARNPG        ANDEQ     3
200200980727     C     BARSPG        ANDEQ     'A'
200300960523     C*
200400020213     C* NON DEVE ESSERE PRESENTE NEMMENO IN L6 DEL P.O. SPUNTA
200500960523     C     BARLNA        LOOKUP    L6                                     88
200600980127     C**
200700960531    4C     *IN88         IFEQ      *OFF
200800961202     C                   Z-ADD     146           CAACRE
200900961202     C                   Z-ADD     145           CAACIU
201000961202     C                   EXSR      ANM14X
201100960531    4C                   ENDIF
201200960531    3C                   ENDIF
201300960522     C*
201400960523     C* ALTRIMENTI  LA CHIUDO
201500960531    3C     *IN88         IFEQ      *ON
201600960523     C     BARNPG        OREQ      1
201700020213     C     BPES          OREQ      BARLNA
201800961122     C*
201900000523     C* SE CHI SPARA E' LA FILIALE FINALE DI ARRIVO, ME LO MEMORIZZO
202000961122     C*  MI SERVE PER LA CHIUSURA
202100961122    4C     *IN88         IFEQ      *ON
202200020213     C     BPES          OREQ      BARLNA
202300961122     C                   MOVEL     'S'           WSTESS
202400961122    4C                   ENDIF
202500961122     C**
202600960522     C                   Z-ADD     146           COMCAA
202700960522     C                   EXSR      CHIUAN
202800961122     C*
202900960522     C                   Z-ADD     145           COMCAA
203000960522     C                   EXSR      CHIUAN
203100970204     C*
203200970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
203300970204     C                   EXSR      ANMARR
203400960531    3C                   ENDIF
203500961021     C*
203600970203     C* CREO E/O CHIUDO LE ANOMALIE CHE RIGUARDANO L'AREA DI ARRIVO
203700970203     C                   EXSR      ANMSEM
203800960531    2C                   ENDIF
203900911016     C*
204000960531   X1C                   ELSE
204100970203     C* T E R M I N A L   D I   A R R I V O
204200961122     C                   SETOFF                                       88
204300961122     C                   CLEAR                   WSTESS
204400970203     C*
204500970203     C* CREO E/O CHIUDO LE ANOMALIE CHE RIGUARDANO L'AREA DI ARRIVO
204600970203     C                   EXSR      ANMSEM
204700970204     C*
204800970204     C* CHIUDO EVENTUALE ANOMALIA BOLLA ARRIVI
204900970204    2C     BARNPG        IFEQ      2
205000970204     C                   Z-ADD     130           COMCAA
205100970204     C                   EXSR      CHIUAN
205200970204    2C                   ENDIF
205300940330     C*
205400940330     C* SONO TERMINAL DI ARRIVO-ANOMALIA:INOLTRARE COLLO ALL'ARRIVO
205500960531    2C     WTIBOL        IFNE      'A'
205600020218    2C     WTIBOL        ANDNE     'L'
205700960522     C                   MOVEL     'R'           WTIBOL
205800960531    2C                   ENDIF
205900960522     C*
206000960522     C* SOLO SE NON E' UN MIO COLLO
206100020213    2C  N05BPES          IFNE      BARLNA
206200091211     c
206300091211     C* CARICO LA L6 DELLA FILIALE GESTIONE
206400091211     C     BPES          IFNE      SAVPES
206500091211     C                   MOVEL     BPES          SAVPES
206600091211     C                   EXSR      CARL6
206700091211     C                   ENDIF
206800091211     c
206900960523     C* NON DEVE ESSERE PRESENTE NEMMENO IN L6
207000960531     C     BARLNA        LOOKUP    L6                                     88
207100960531    3C     *IN88         IFEQ      *OFF
207200960523     C*
207300960531    4C     BARNPG        IFGT      2
207400960522     C     BARNPG        ANDLT     5
207500000523     C     BARNPG        OREQ      8
207600961202     C                   Z-ADD     145           CAACRE
207700961202     C                   Z-ADD     146           CAACIU
207800961202     C                   EXSR      ANM14X
207900960522     C*
208000960531   X4C                   ELSE
208100961211     C* SE TROVO LA 145 E LA POSSO CHIUDERE CREO SE POSSO LA 620
208200961211     C*  SE NON TROVO LA 145 DEMANDO L'EVENTUALE CREAZIONE DELLA 620
208300961211     C*  ALLA CHIUSURA DELLA EVENTUALE 146, CHE POTREBBE ANCHE NON
208400961211     C*  ESSERCI. SIA CHE CI SIA DA CHIUDERE CHE NON CI SIA, A QUESTO
208500961211     C*  PUNTO LA 620 LA CREO
208600960522     C                   Z-ADD     145           COMCAA
208700961211     C* SOLO SE ESISTE L'ANOMALIA CREO LA 620
208800961211     C                   MOVEL     'S'           WESAN             1
208900961211     C*
209000960522     C                   EXSR      CHIUAN
209100961211     C                   CLEAR                   WESAN
209200961029     C* SE C'E' CHIUDO ANCHE IL DISGUIDO IN AREA
209300961029     C                   Z-ADD     146           COMCAA
209400961211     C*
209500961211     C   97              MOVEL     'S'           W620
209600961211     C  N97              MOVEL     'N'           W620
209700961029     C                   EXSR      CHIUAN
209800961211     C                   MOVEL     'S'           W620
209900960523    4C                   ENDIF
210000961021   X3C                   ELSE
210100961122     C*
210200020213     C* SE CHI SPARA E' LA FILIALE FINALE DI ARRIVO, ME LO MEMORIZZO
210300961122     C*  MI SERVE PER LA CHIUSURA
210400961122     C                   MOVEL     'S'           WSTESS            1
210500961021     C                   Z-ADD     146           COMCAA
210600961021     C                   EXSR      CHIUAN
210700970204     C*
210800970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
210900970204     C                   EXSR      ANMARR
211000960522    3C                   ENDIF
211100961122     C**
211200961021   X2C                   ELSE
211300970204     C* CHI SPARA E' LA FILIALE FINALE DI ARRIVO
211400961122     C                   MOVEL     'S'           WSTESS
211500961021     C                   Z-ADD     146           COMCAA
211600961021     C                   EXSR      CHIUAN
211700970204     C*
211800970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
211900970204     C                   EXSR      ANMARR
212000960523    2C                   ENDIF
212100911016     C*
212200950922    1C                   END
212300961213    0C                   ENDIF
212400021022     c
212500021203     c                   unlock    fnbtt37l
212600970203     C**
212700970203     C** SE FLAG = 2 MODIFICO ANOMALIE METTENDO NUOVO NUMERO DI FOGLIO
212800970203     C**
212900970203     C     *IN05         IFEQ      *ON
213000970203     C*
213100921021     C                   EXSR      RICFAS
213200921021     C*
213300990518     C     KBAR          SETLL     FNANM000
213400990518     C     KBAR          READE     FNANM000                               99
213500921021     C*
213600960423    2C     *IN99         DOWEQ     '0'
213700070208    3C     ANMNFV        IFEQ      DLS45NFVO
213800000516     C* SE NON C'E' SOTTO CATEGORIA DEVONO ESSERE UGUALI
213900000516    4C     BARSPG        IFEQ      ' '
214000921021     C     ANMFSA        ANDEQ     COMFAS
214100000516     C* SE     C'E' SOTTO CATEGORIA, DEVE ESSERE UGUALE AD UNA DELLE
214200000516     C*  TANTE
214300000516     C     ANMFSA        OREQ      WFAS1
214400000516     C     ANMFSA        OREQ      WFAS2
214500000516     C     ANMFSA        OREQ      WFAS3
214600000516     C     ANMFSA        OREQ      WFAS4
214700000516     C     ANMFSA        OREQ      WFAS5
214800921021     C                   Z-ADD     BARNFV        ANMNFV
214900960524     C                   Z-ADD     BARDFV        ANMDAO
215000000516     C                   MOVEL     COMFAS        ANMFSA
215100990518     C                   UPDATE    FNANM000
215200000516    4C                   ENDIF
215300000516    3C                   ENDIF
215400921021     C*
215500990518     C     KBAR          READE     FNANM000                               99
215600000516    2C                   ENDDO
215700920424     C*
215800920424     C                   Z-ADD     3             B
215900021107    1C                   END
216000920424     C     ENDUPD        TAG
216100000209     C*
216200110907    1C     *IN39         IFEQ      *ON
216300000209     C                   SETON                                        40
216400110907   x1C                   ELSE
216500000209     C*
216600000210     C* ELABORO SOLO SPUNTA DA MACCHINONE
216700110907     c*  non lo faccio se modifica di numero foglio da video
216800110907    2c  N05RPS(Y)        ifne      'A'
216900000209     C* AGGIORNO FILE  FISGN SE C'E'
217000000209     C     KBAR          CHAIN     FISGN03L                           33
217100110907    3C     *IN33         DOWEQ     *OFF
217200110907    4C     SGNST2        IFLT      9
217300000209     C                   ADD       1             SGNST2
217400001228     C                   CLEAR                   SGNT6A
217500001228     C                   CLEAR                   SGNT6B
217600001228     C                   CLEAR                   SGNT6C
217700001228     C                   CLEAR                   SGNT6D
217800001228     C                   CLEAR                   SGNT6E
217900001228     C                   CLEAR                   SGNT6F
218000080624     c                   eval      sgndatora = %char(%timestamp:*iso0)
218100000209     C                   UPDATE    FISGN000
218200110907    4C                   ENDIF
218300001020     C     KBAR          READE     FISGN03L                               33
218400110907    3C                   ENDDO
218500110907     c
218600110907     c* Una qualsiasi spunta REALE chiude eventuale anomalia interna
218700110907     c*  115 - COLLO MAI AFFIDATO
218800110929     C                   MOVEL     'AR'          SAVCCH
218900110907     C                   Z-ADD     115           COMCAA
219000110907     C                   EXSR      CHIUAN
219100110907     c
219200110907     c
219300110907    2C                   ENDIF
219400110907    1C                   ENDIF
219500941206     C*
219600020215     C                   ENDSR
219700961118     C**************************************************************************
219800961205     C* CREO ANOMALIA 600 PER L'EXPORT   COLLO A TERRA
219900961118     C**************************************************************************
220000961118     C     ANM600        BEGSR
220100961210     C*
220200961210     C* PER ESTERO CREO ANOMALIA 645 E 600
220300020212    7C     NTWBAR        IFEQ      'EEX'
220400060322    7C     NTWBAR        oreq      'DPD'
220500060322    7C     NTWBAR        oreq      'FED'
220600961210     C                   Z-ADD     645           COMCAA
220700961210     C                   EXSR      CRTANM
220800961210     C*
220900961210    8C     W60X          IFEQ      'S'
221000961209     C* SE ESISTE LA 620  LA DELETO
221100961118     C                   Z-ADD     620           COMCAA
221200001110     C     COMCAA        LOOKUP    ACG                                    34
221300001110     C     BARSPG        IFNE      'I'
221400001110     C     *IN34         OREQ      *ON
221500990518     C     KANM          DELETE    FNANM02L                           34
221600001110     C                   ENDIF
221700961118     C                   Z-ADD     600           COMCAA
221800961118     C                   EXSR      CRTANM
221900961210     C                   CLEAR                   W60X
222000961210    8C                   ENDIF
222100961210    7C                   ENDIF
222200961210     C                   ENDSR
222300960702     C**************************************************************************
222400960702     C* CERCO LNP BOLLA SE COLLO CON SERIE > 0
222500960702     C**************************************************************************
222600960702     C     RICLNP        BEGSR
222700970814     C                   SETOFF                                       33
222800960702     C*
222900020201     C* CONTROLLO SE COLLO CONFERMABILE DAL TERMINAL DEL P.O. EFFETTUA
223000020201     C*  SPUNTA
223100020201     C                   MOVEL     PESTFP        W008A             8
223200960702     C                   MOVE      BARNRS        W005A             5
223300960702     C                   MOVEL     BARLNP        W005A
223400960702     C                   MOVE      W005A         W008A
223500990108     C                   Z-ADD     1             YY                4 0
223600970828     C     W008A         LOOKUP    S5F(YY)                                33
223700020131     C**
223800020131     C* SE NON TROVATO CERCO COME SERIE = 99
223900020131     C     *IN33         IFEQ      *OFF
224000020131     C                   MOVE      99            W008A
224100020131     C                   Z-ADD     1             YY                4 0
224200020131     C     W008A         LOOKUP    S5F(YY)                                33
224300020131     C                   ENDIF
224400020131     C**
224500020131    1C     *IN33         IFEQ      *ON
224600020131     C                   MOVEL     'S'           WTAB5F            1
224700020131     C* CERCO SPUNTA PARTENZA SE C'E' PRENDO LA FGS DI QUEST'ULTIMA
224800020201     C*  E LA DATA DI RIFERIMENTO E' LA DATA DI QUESTA SPUNTA
224900960702     C                   Z-ADD     1             WNPG
225000070131     C     KBAR3         SETLL     FNBRV09L
225100070131     C     KBAR3         READE     FNBRV09L                               33
225200020131    2C     *IN33         DOWEQ     *OFF
225300040303    3C     BR9ATR        IFEQ      *BLANKS
225400040323     c* Escludo le defluenze e le spunte di simfel(potrebbe quindi essere co
225500040323     c*  lo in transito da me e quindi non partito da me)
225600040323    4C     BR9KEY        IFNE      SA1KEY
225700040323     C                   MOVEL     BR9KEY        SA1KEY
225800040323     C                   Z-ADD     BR9NPG        WNPG
225900040323     C                   Z-ADD     BR9NFV        WNFV
226000040323     C                   Z-ADD     BR9FGS        WFGS
226100070906     C                   movel     BR9nps        Wnps
226200040323     C                   EXSR      RILFOG
226300040323     c                   Z-ADD     WDFV          BRVDFV
226400140616     c                   movel     WFCF          BRVFCF
226500140616     c                   movel     Wspg          BRVspg
226600040323    5C     WTTR          IFNE      *BLANKS
226700040323     C     WTTR          LOOKUP    TTR                                    34
226800040323     C                   MOVE      *IN34         W9DEFL            1
226900040323     C                   ELSE
227000040323     C                   MOVE      '0'           W9DEFL            1
227100040323    5C                   ENDIF
227200040323    4C                   ENDIF
227300040323    4c                   if        w9defl='0' and br9fgs<>simfel
227400110103     c*
227500040303     C                   MOVE      BR9PES        LNPB
227600970814     C                   CLEAR                   WTAB5F
227700020214     C                   Z-ADD     BRVDFV        DSPB
227800020214     C                   Z-ADD     BRVDFV        WDTRIF
227900960702     C                   SETON                                        33
228000020201    4C                   END
228100020131    3C                   END
228200070131     C  N33KBAR3         READE     FNBRV09L                               33
228300020131    2C                   ENDDO
228400020204     C**
228500020131    2C     WTAB5F        IFEQ      'S'
228600020218     C* CERCO SE C'E' IL P.O. CHE HA SPUNTATO, ALTRIMENTI IL PRIMO
228700020218     C                   MOVEA     F5F(YY)       WPO
228800020218     C                   Z-ADD     1             ZZ
228900020218     C                   CLEAR                   WTROVA
229000020218    3C                   DO        4             ZZ                3 0
229100020218    4C     WPO(ZZ)       IFGT      *ZEROS
229200020218     C                   MOVEL     WPO(ZZ)       W0030             3 0
229300020218    5C     W0030         IFEQ      BPES
229400020218     C                   MOVEL     W0030         LNPB
229500020218     C                   MOVEL     'S'           WTROVA
229600020218    5C                   ENDIF
229700020218    4C                   ENDIF
229800020218    3C                   ENDDO
229900020218     C**
230000020218     C* SE NON C'E' P.O. = BPES PRENDO IL PRIMO
230100020218     C     WTROVA        IFEQ      ' '
230200020218     C                   MOVEL     WPO(1)        LNPB
230300020218     C                   ENDIF
230400020214     C                   MOVEL     BARDFV        DSPB
230500020214     C                   MOVEL     BARDFV        WDTRIF
230600020131    2C                   ENDIF
230700970814    1C                   ENDIF
230800960702     C                   ENDSR
230900941206     C**************************************************************************
231000941206     C* CERCO IL RAGGRUPPAMENTO PISTOLA
231100941206     C**************************************************************************
231200921229     C     RICRAG        BEGSR
231300921229     C*
231400930305     C     SAVNPS        IFNE      ALF2
231500921229     C*
231600950515     C* HO CARICATO TUTTA SCHIERA
231700921229     C     *IN02         IFEQ      '0'
231800970417     C                   Z-ADD     1             Z                 3 0
231900930305     C                   MOVEL     ALF2          NUM2              2 0
232000970417     C     NUM2          LOOKUP    NPS(Z)                                 34
232100970417     C  N34              Z-ADD     1             Z
232200921229     C*
232300921229     C                   ELSE
232400921229     C*
232500921229     C* NON HO CARICATO LA SCHIERA --> FACCIO CHAIN
232600921229     C                   MOVEL     '7J'          COD
232700921229     C                   MOVEL     *BLANKS       KEY
232800930305     C                   MOVEL     ALF2          KEY
232900921229     C     KTAB          CHAIN     TABEL                              34
233000001023     C                   Z-ADD     2             Z
233100000316     C     *IN34         IFEQ      *OFF
233200000316     C                   MOVEL     TBLUNI        DS7J
233300000316     C                   MOVEL     §7JRPS        RPS(Z)
233400000316     C                   MOVEL     §7JORD        OPS(Z)
233500000316     C                   MOVEL     §7JTPS        TPS(Z)
233600000316     C                   MOVEL     §7JINV        INV(Z)
233700040503     C                   MOVEL     §7Jfsl        fsl(Z)
233800000316     C                   ELSE
233900000316     C                   MOVEL     'M'           RPS(Z)
234000000316     C                   MOVEL     999           OPS(Z)
234100000316     C                   MOVEL     'B'           TPS(Z)
234200000316     C                   MOVEL     ' '           INV(Z)
234300040503     C                   MOVEL     ' '           fsl(Z)
234400000316    1C                   END
234500921229     C                   END
234600921229     C*
234700930305     C                   MOVEL     ALF2          SAVNPS
234800921229     C                   END
234900941206     C*
235000921229     C                   ENDSR
235100941206     C**************************************************************************
235200941206     C* CHIUDO ANOMALIA
235300941206     C**************************************************************************
235400911018     C     CHIUAN        BEGSR
235500961211     C                   SETOFF                                       97
235600941206     C*
235700990518     C     KANM          CHAIN(N)  FNANM000                           97
235800961121     C**
235900961211    1C     *IN97         IFEQ      *OFF
236000970117     C**
236100970117     C** ANOMALIA 145/146: SE DATA SPUNTA > DATA ANOAMLIA
236200970117     C*  DEVO CONTROLLARE CHE NON CI SIANO ALTRE SPUNTE
236300970117     C*  CHE MI IMPEDISCONO LA CHIUSURA
236400970117     C     ANMCAA        IFEQ      145
236500970117     C     ANMCAA        OREQ      146
236600041013     c* Imposto econtrdfv per anomalie 145 146 che la deve sempre cotrollare
236700041013     c                   eval      wcontrdfv='S'
236800990518     C     BARDFV        IFGT      ANMDAO
236900970117     C                   EXSR      CTRSPU
237000970117     C                   ENDIF
237100970117     C                   ENDIF
237200961121     C**
237300041013     c     Wcontrdfv     ifne      'S'
237400041013     C* PER CATEGORIA no arrivi e partenza CHIUDO SE DATA >=
237500961122     C     WSTESS        OREQ      'S'
237600990518     C     BARDFV        ANDEQ     ANMDAO
237700041013     C     BARNPG        ANDne     2
237800041013     C     BARNPG        ANDne     1
237900041013     C* PER LE CATEGORIE arrivi SOLO SE DATA > e la posso chiudere
238000990518     C     BARDFV        ORGT      ANMDAO
238100970117     C     WCHIU         ANDEQ     'S'
238200961211     C*
238300961211     C     WCREA         OREQ      'N'
238400020213     C     BARFLE        ANDNE     BTFP
238500961121     C**
238600960416     C                   CLEAR                   DSLR33
238700960524     C                   MOVE      BARDFV        D33DCH
238800960514    2C     BARKEY        IFNE      SAVFOG
238900911125     C                   EXSR      RICFAS
239000960514    2C                   END
239100960514     C*
239200960416     C                   MOVEL     COMFAS        D33FSC
239300960416     C                   MOVEL     SAVCCH        D33CCH
239400960530     C                   MOVEL     SAVMAC        D33MAC
239500960416     C                   MOVE      ANMNR2        D33NRR
239600960416     C*
239700990712     C                   CALL      'FNLR33R'
239800960416     C                   PARM                    DSLR33
239900961209     C*
240000961209     C* PER ANOMALIE 145 E 146 CREO SE E' DA CREARE L'ANOMALIA 620
240100020212     C*  AOLO SE NON SONO IN CREAZIONE DELL 600
240200020212    2C     NTWBAR        IFEQ      'EEX'
240300961210     C     W620          ANDEQ     'S'
240400060322    2C     NTWBAR        oreq      'DPD'
240500060322     C     W620          ANDEQ     'S'
240600060322    2C     NTWBAR        oreq      'FED'
240700060322     C     W620          ANDEQ     'S'
240800961210     C*
240900961210    3C     ANMCAA        IFEQ      145
241000961209     C     ANMCAA        OREQ      146
241100961210     C*
241200961210    4C     W60X          IFEQ      'S'
241300961210     C                   Z-ADD     600           COMCAA
241400001110     C     COMCAA        LOOKUP    ACG                                    34
241500001110    5C     BARSPG        IFNE      'I'
241600001110     C     *IN34         OREQ      *ON
241700990518     C     KANM          DELETE    FNANM02L                           34
241800001110    5C                   ENDIF
241900961209     C                   Z-ADD     620           COMCAA
242000961209     C                   EXSR      CRTANM
242100961209     C                   CLEAR                   W60X
242200961210    4C                   ENDIF
242300961209     C*
242400961209     C* VEDO SE CANCELLARE L'ANOMALIA 645
242500961209     C                   Z-ADD     645           COMCAA
242600001110     C     COMCAA        LOOKUP    ACG                                    34
242700001110   4AC     BARSPG        IFNE      'I'
242800001110     C     *IN34         OREQ      *ON
242900990518     C     KANM          CHAIN     FNANM02L                           34
243000961210    4C     *IN34         DOWEQ     *OFF
243100990518    5C     ANMDAO        IFEQ      BARDFV
243200990518     C                   DELETE    FNANM000
243300961209     C                   SETON                                        34
243400961209     C                   ELSE
243500990518     C     KANM          READE     FNANM02L                               34
243600961210    5C                   ENDIF
243700961210    4C                   ENDDO
243800001110   4AC                   ENDIF
243900961210    3C                   ENDIF
244000961209     C*
244100961210    2C                   ENDIF
244200960514     C*
244300960514     C* L'ANOMALIA 05 PUO' ESSERCI PIU' VOLTE
244400961209    2C     ANMCAA        IFEQ      005
244500980709     C                   SETOFF                                       34
244600960514    3C     *IN34         DOWEQ     *OFF
244700990518     C     KANM          READE(N)  FNANM000                               34
244800960514     C                   MOVE      ANMNR2        D33NRR
244900960514     C*
245000960514     C  N34              CALL      'FNLR33R'
245100960514     C                   PARM                    DSLR33
245200960514    3C                   ENDDO
245300911018     C*
245400960514    2C                   ENDIF
245500961121   1AC                   ENDIF
245600961209   X1C                   ELSE
245700961209     C*
245800961211     C* SOLO PER ANOMALIE 145 E 146
245900961210    2C     COMCAA        IFEQ      145
246000961210     C     COMCAA        OREQ      146
246100991230     C* SE DEVO CREARE EVENTUALE 620 PER LNA ESTERA
246200020212    3C     W60X          IFEQ      'S'
246300961210     C     W620          ANDEQ     'S'
246400060322     C* E LA DEVO CREARE SIA SE TROVO CHE SE NON TROVO L'ANOMALIA 145
246500060322     C*  O 146 DA CHIUDERE  (WESAN =' ')
246600060322     C     WESAN         ANDEQ     ' '
246700110103     c*
246800060322    4C     NTWBAR        ifeq      'EEX'
246900060322     C     NTWBAR        oreq      'DPD'
247000060322     C     NTWBAR        oreq      'EEX'
247100961209     C                   Z-ADD     620           COMCAA
247200961209     C                   EXSR      CRTANM
247300961209     C                   CLEAR                   W60X
247400060322    4C                   ENDIF
247500060322    3C                   ENDIF
247600961210    2C                   ENDIF
247700961210    1C                   ENDIF
247800110103     c*
247900041013     c                   eval      wcontrdfv='N'
248000911018     C*
248100110929     c                   clear                   savmac
248200110929     c                   clear                   savcch
248300911018     C                   ENDSR
248400941206     C**************************************************************************
248500941206     C*    RICERCA FASE
248600941206     C**************************************************************************
248700911125     C     RICFAS        BEGSR
248800941206     C*
248900921020     C                   MOVEL     '7N'          COD
249000921020     C                   MOVEL     *BLANKS       KEY
249100921020     C                   MOVEL     BARNPG        KEY
249200921105     C     KTAB          CHAIN     TABEL                              41
249300921105     C  N41              MOVEL     TBLUNI        DS7N
249400000516     C*
249500000516     C     BARSPG        IFNE      ' '
249600000516     C                   MOVEL     §7NFS1        WFAS1             1
249700000516     C                   MOVEL     §7NFS2        WFAS2             1
249800000516     C                   MOVEL     §7NFS3        WFAS3             1
249900000516     C                   MOVEL     §7NFS4        WFAS4             1
250000000516     C                   MOVEL     §7NFS5        WFAS5             1
250100000516     C                   ENDIF
250200921021     C*
250300960524     C     BARSPG        IFEQ      §7NSC1
250400921021     C                   MOVEL     §7NFS1        COMFAS            1
250500921021     C                   ELSE
250600960524     C     BARSPG        IFEQ      §7NSC2
250700921021     C                   MOVEL     §7NFS2        COMFAS
250800921021     C                   ELSE
250900960524     C     BARSPG        IFEQ      §7NSC3
251000921021     C                   MOVEL     §7NFS3        COMFAS
251100921021     C                   ELSE
251200960524     C     BARSPG        IFEQ      §7NSC4
251300921021     C                   MOVEL     §7NFS4        COMFAS
251400921021     C                   ELSE
251500921021     C                   MOVEL     §7NFS5        COMFAS
251600921021     C                   END
251700921021     C                   END
251800921021     C                   END
251900921021     C                   END
252000921020     C*
252100961217     C                   MOVEL     BARKEY        SAVFOG            8 0
252200941206     C*
252300911125     C                   ENDSR
252400950927     C**************************************************************************
252500950927     C*    CONTROLLO SE CREARE ANOMALIA 50: COLLO ARRIVATO NON PARTITO
252600130228     C*                      O ANOMALIA 55/56/57: DISGUIDO - ESTERNA
252700950927     C**************************************************************************
252800950927     C     CRTA50        BEGSR
252900000320     C                   CLEAR                   WCCH
253000020214     C* PER ANOMALIA 50 NON CREO SE ESISTE GIA' LA DATA DI PARTENZA
253100020214     C*  BOLLE TRANSITO
253200020214     C     WTIBOL        IFEQ      'T'
253300020214     C     COMDFV        ANDGT     0
253400020214     C                   GOTO      NOA50
253500020214     C                   ENDIF
253600020214     C**
253700020214     C     WTIBOL        IFEQ      'A'
253800020218     C     WTIBOL        OREQ      'L'
253900050215     c* Per l'anoamlia 50 basterebbe questo test senza leggere le spunte,
254000050215     c*  le spunte le devo leggere lo stesso per i manca record
254100020214     C     WESART        IFEQ      'S'
254200020214     C     COMDFV        ANDGT     0
254300020214     C                   GOTO      NOA50
254400020214     C                   ENDIF
254500020214     C* SE E' COLLO IN ARRIVO E BOLLA CONSEGNATA --> NON CREO
254600020214     C     WESART        IFEQ      'S'
254700020214     C     ARTDCM        ANDGT     0
254800020214     C                   GOTO      NOA50
254900020214     C                   ENDIF
255000020218     C* SE BOLLA LOCALE E NON C'E' LA BOLLA NON FACCIO NIENTE
255100020218     C     WTIBOL        IFEQ      'L'
255200020218     C     WBOPAR        ANDEQ     'N'
255300020218     C                   GOTO      NOA50
255400020218     C                   ENDIF
255500020214     C                   ENDIF
255600020214     C**
255700950927     C* VERIFICO SE ESISTE GIA'
255800040429     c* ora la esistenza è all'interno dello stesso p.o. e non in assoluto
255900950927    1C     WTIBOL        IFEQ      'D'
256000950927     C                   Z-ADD     55            COMCAA
256100950927     C                   ELSE
256200950927     C                   Z-ADD     50            COMCAA
256300950927    1C                   ENDIF
256400990409     C* SE LA CATEGORIA E' IMA IMG O CONS INVECE CHE 55 CREO LA 56
256500020214    1C     COMCAA        IFEQ      55
256600020214    2C     BARNPG        IFEQ      4
256700990412     C     BARNPG        OREQ      3
256800990409     C     BARSPG        ANDEQ     'A'
256900990412     C     BARNPG        OREQ      3
257000990409     C     BARSPG        ANDEQ     'G'
257100161021     C     BARNPG        OREQ      3
257200161021     C     BARSPG        ANDEQ     'Z'
257300990409     C                   Z-ADD     56            COMCAA
257400020214    2C                   ENDIF
257500020214    1C                   ENDIF
257600130228     c* se Anomalia 55 a collo che può partire da diverse filiali
257700130228     c*  e chi spunta è una di qeuste --> non creo 55 ma 57 Disguido AUTORIZZ
257800130228     c*  solo per ategorie di spunta partenza
257900130228     c                   if        comcaa=55  and
258000130228     c                             (bolxco='W' or
258100130228     c                             (wesbtt='N' and wesblt='N' and wesart='N'
258200130228     c                              and barnrs>0))
258300130228     c* se si tratta di spunta partenza --> creo la 57
258400130228     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
258500130228     c                             or barnpg=1
258600130228     c                   exsr      CtrXCO
258700130228     c                   if        Indx>0
258800130301     c                   z-add     57            comcaa
258900130228     c                   endif
259000130228     c                   endif
259100130228     c                   endif
259200130301     c
259300130301     c* In caso di anomalia 50 manca bolla --> verifico se non è da creare
259400130301     c*  per collo spuntabile in questo TFP e "locale"
259500130301     c                   if        comcaa=50 and
259600130301     c                             (bolxco='W' or
259700130301     c                             (wesbtt='N' and wesblt='N' and wesart='N'
259800130301     c                              and barnrs>0))
259900130301     c* se si tratta di spunta di partenza --> non creo la 50 ma la 57
260000130301     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
260100130301     c                             or barnpg=1
260200130301     c                   exsr      CtrXCO
260300130301     c                   if        Indx>0
260400130301     c                   z-add     57            comcaa
260500130301     c                   endif
260600130301     c                   endif
260700130301     c                   endif
260800971009     C*
260900130228     c* per anomalia 55 o 56 57 valida l'anomalia per p.o. spunta
261000040429     c* per anomalia 50:
261100040429     c* per bolle transito solo se spara il p.o. transito
261200040429     c* per bolle arrivi, se trovo anomalia nell'area di arrivo(e non
261300040429     c*  di un eventuale p.o. transito)
261400040429     c                   clear                   wtrova
261500040429     C     KANM          SETLL     FNANM000
261600040504     C     KANM          reade(N)  FNANM000                               34
261700040429    1c                   dow       not*in34 and wtrova=*blanks
261800040429    2c                   if        anmcaa<>50 or wtibol='T'
261900040429    3c                   if        anmfle=bpes
262000040429     c                   eval      wtrova='1'
262100040429   x3c                   else
262200040504     C     KANM          reade(N)  FNANM000                               34
262300040429    3c                   endif
262400110103     c*
262500040429   x2c                   else
262600040429     C                   CLEAR                   DSLV55
262700040429     C                   MOVEL     'A'           D55TPT
262800040429     C                   Z-ADD     anmfle        D55LIN
262900040429     C                   Z-ADD     lnpb          D55lnp
263000040429     C                   Z-ADD     bardfv        D55DRF
263100040429     C                   EXSR      FNLV55
263200040429    3c                   if        d55tfa=pestfa
263300040429     c                   eval      wtrova='1'
263400040429   x3c                   else
263500040504     C     KANM          reade(N)  FNANM000                               34
263600040429    3c                   endif
263700040429    2c                   endif
263800110103     c*
263900040429    1c                   enddo
264000990412     C*
264100990412     C* PER ANOMALIA 55 VERIFICO ANCHE CHE NON CI SIA LA 56
264200990412     C*  PERCHE' DOPO AVER SPARATO UN COLLO IN IMA PUO' ESSERE
264300990412     C*  SPARATO IN PARTENZA PER FARLO RIPARTIRE
264400040429    1C     *IN34         IFEQ      *On
264500990412     C     COMCAA        ANDEQ     55
264600990412     C                   Z-ADD     56            COMCAA
264700040429     C     KANM          SETLL     FNANM000
264800040504     C     KANM          reade(N)  FNANM000                               34
264900040429     c                   dow       not*in34 and wtrova=*blanks
265000040429     c                   if        anmfle=bpes
265100040429     c                   eval      wtrova='1'
265200040429     c                   else
265300040504     C     KANM          reade(N)  FNANM000                               34
265400040429     c                   endif
265500040429     c                   enddo
265600990412     C                   Z-ADD     55            COMCAA
265700990412    1C                   ENDIF
265800950927     C*
265900040429    1C     *IN34         IFEQ      *On
266000970828     C* NON CREO ANOMALIA 55 SE C'E' UNA SPUNTA PARTENZA DI ALTRO AS
266100970828     C*  PERCHE' NON SI TRATTA PIU' DI UN DISGUIDO MA O E' COLLO
266200990409     C*  IN TRANSITO DI CUI NON HO RICEVUTO LA BOLLA
266300990409     C*  O E' COLLO DISGUIDATO IN ALTRA FIL CHE PER ANDARE IN ARRIVO
266400970828     C*  DEVE PASSARE DA ALTRA FIL ANCORA
266500970828     C*  LA 200 VIENE INVECE CREATA LO STESSO PERCHE' UNA ANOMALIA
266600970828     C*  GESTIONALE CHE SERVE PER FAR RIPARTIRE AUTOMATICAMENTE IL COLL
266700970828     C*  ANCHE SE NON VIENE SPARATO NE' IN PARTENZA NE' IN ENTRATA
266800000316     C                   EXSR      CNOIDD
266900950927    1C                   ENDIF
267000020214     C     NOA50         ENDSR
267100000316     C**************************************************************************
267200000316     C* CREA ANOMALIA COLLO ARRIVATO PARTITO CON PISTOLA FASULLA
267300000316     C**************************************************************************
267400160624     C***  CRTA60        BEGSR
267500000316     C* ESCLUDO SE PISTOLA ATOMATICA
267600160624    1C***  RPS(Y)        IFNE      'A'
267700160624     C***
267800000316     C* VEDO DALLA PISTOLA PARTENZA SE CREARE O MENO L'ANOMALIA
267900160624    2C***  WTIBOL        IFne      'T'
268000160624     C***                MOVEL     ARTNPP        ALF2
268100041013    2C**   WTIBOL        IFEQ      'T'
268200160624     c***                else
268300160624     C***                MOVEL     BTTNPP        ALF2
268400160624    2C***                ENDIF
268500000316     C*
268600160624     C***                EXSR      RICRAG
268700000316     C**
268800000316     C** DA INVIARE IN PARTENZA
268900160624    2C***  INV(Z)        IFEQ      'S'
269000000316     C**
269100160624    3C***  WTIBOL        IFne      'T'
269200000316     C* SOLO SE COLLO NON TRANSITATO O TRANSITATO CON PIST.FASULLA
269300160624    4C***  ARTDUT        IFGT      0
269400160624     C***                MOVEL     ARTPUT        ALF2
269500160624     C***                EXSR      RICRAG
269600160624    4C***                ENDIF
269700000316     C* SE NON C'E' TRANSITO O TRANSITO CON PISTOLA FASULLA, CONTR.
269800160624    4C***  ARTDUT        IFEQ      0
269900160624     C***  ARTDUT        ORGT      0
270000160624     C***  OPS(Z)        ANDGE     040
270100160624     C***                Z-ADD     60            COMCAA
270200000607     C* SE RICHIAMATO CON FLAG 6 CREO LA 61
270300160624     C***  dls45FLG      IFEQ      '6'
270400160624     C***                Z-ADD     61            COMCAA
270500160624     C***                ENDIF
270600041013     c* Se c'e' già anomalia non la ricreo
270700160624     c***  kanm          setll     fnanm000                               34
270800160624     c***                if        not *in34
270900160624     C***                EXSR      CNOIDD
271000160624    4C***                ENDIF
271100160624    4C***                ENDIF
271200000316     C*
271300160624   X3C***                ELSE
271400000316     C* NEGLI ALTRI CASI CREO SUBITO
271500160624     C***                Z-ADD     60            COMCAA
271600000607     C* SE RICHIAMATO CON FLAG 6 CREO LA 61
271700160624     C***  dls45FLG      IFEQ      '6'
271800160624     C***                Z-ADD     61            COMCAA
271900160624     C***                ENDIF
272000160624     c***  kanm          setll     fnanm000                               34
272100160624     c***                if        not *in34
272200160624     C***                EXSR      CNOIDD
272300160624    3C***                ENDIF
272400160624    3C***                ENDIF
272500160624     C***
272600160624    2C***                ENDIF
272700160624    1C***                ENDIF
272800160624     C***                ENDSR
272900000316     C**************************************************************************
273000000316     C* RIEMPO ALCUNI CAMPI ANOMALIA NO IDD
273100000316     C**************************************************************************
273200000316     C     CNOIDD        BEGSR
273300160624     C** ANOMALIE 50 -->
273400160725     C* VERIFICO SE ESISTE UNA SPUNTA PARTENZA PER QUEL COLLO
273500160624     C*  SE SI NON CREO ANOMALIA PER 50
273600130228     C*  ANOMALIE 55/56/57 --> CONSIDERO SOLO LA SPUNTA "D-DISGUIDO"
273700020214     C*                     PER NON CREARLE
273800000316     C                   Z-ADD     1             WNPG
273900000317     C                   CLEAR                   WTROVA
274000021121     C* CAMPI DI COMODO PER ANOM 50 per transito su stessa £1
274100021121     C                   CLEAR                   WFVALNP
274200021121     C                   CLEAR                   WFVANFV
274300021121     C                   CLEAR                   WFVADFV
274400110103     c*
274500070131     C     KBAR3         CHAIN     FNBRV09L                           99
274600020214    1C     *IN99         DOWEQ     *OFF
274700000317     C     WTROVA        ANDEQ     ' '
274800050215     c                   clear                   wrileggi          1
274900020214     C*****
275000020214     C* SOLO SE LA SPUNTA TROVATA E' DI DISGUIDO, ALTRIMENTI SE NORMALE
275100020214     C*  E' UNA SPUNTA PREVISTA, QUINDI DA IGNORARE
275200020214    2C     COMCAA        IFEQ      55
275300020214     C     COMCAA        OREQ      56
275400130228     C     COMCAA        OREQ      57
275500040303    3C     BR9FGS        IFNE      BTFP
275600040429     c* verifico tramite pgm fnlv53r se spunta di foglio diretto a me(BPES)
275700040429     c*  se è diretto a me non creo anomalia
275800040429     c*  Questo vale sia per colli in disguido che colli in transito
275900040429     c*  manca record bolla partenza
276000040430     c                   clear                   dslv53
276100040504     c                   movel     'T'           d53tfo
276200040429     C                   MOVEL     BR9NPG        D53NPG
276300040429     C                   Z-ADD     BR9NFV        D53NFV
276400040429     C                   MOVEL     BR9FGS        D53FGS
276500040429     C                   MOVEL     BR9FLE        D53FLF
276600040504     C                   MOVEL     'A'           D53ABB
276700040429     C                   MOVEL     br9lna        D53LNA
276800050603     C                   MOVEL     Usalnatfa     D53TFA
276900040504     C                   MOVEL     bpes          D53lai
277000040429     C                   CALL      'FNLV53R'
277100040429     C                   PARM                    DSLV53
277200040429     c* se non ci sono errori --> OK esco dal ciclo
277300040429    4c                   if        d53err=*blanks
277400000317     C                   MOVEL     '1'           WTROVA            1
277500050418   x4c                   else
277600050418     C                   eval      wrileggi='1'
277700040429    4c                   endif
277800020214   X3C                   ELSE
277900050215     C                   eval      wrileggi='1'
278000020214    3C                   ENDIF
278100020214   X2C                   ELSE
278200020214     C**
278300031204     c** bolla non locale: cerco spunta non mia
278400031204   2ac     WSPLOC        ifeq      *blanks
278500040303    3C     BR9FGS        IFNE      BTFP
278600050329     c* Deve essere una spunta trasmessa, altrimenti ignoro (scaricata
278700050329     c*  tardi)
278800050329     c     br9ftr        andne     *blanks
278900110103     c*
279000050222    4c                   if        comcaa=50
279100050215     c                   clear                   dslv53
279200050322     c                   if        wtibol='T'
279300050322     c                   movel     'T'           d53tfo
279400050322     c                   else
279500050215     c                   movel     'A'           d53tfo
279600050322     c                   endif
279700050215     C                   MOVEL     BR9NPG        D53NPG
279800050215     C                   Z-ADD     BR9NFV        D53NFV
279900050215     C                   MOVEL     BR9FGS        D53FGS
280000050215     C                   MOVEL     BR9FLE        D53FLF
280100050215     C                   MOVEL     'A'           D53ABB
280200050215     C                   MOVEL     br9lna        D53LNA
280300050603     C                   MOVEL     Usalnatfa     D53TFA
280400050215     C                   MOVEL     pestfa        D53lai
280500050215     C                   CALL      'FNLV53R'
280600050215     C                   PARM                    DSLV53
280700050215     c* se non ci sono errori --> OK esco dal ciclo
280800050222    5c                   if        d53err=*blanks
280900020214     C                   MOVEL     '1'           WTROVA            1
281000050222   X5C                   ELSE
281100050418     c* Se si tratta di errore di foglio non trovato e provengo da
281200050418     c*  aggiornamento bolla transito Ex disguido (richiamo da fnlr13r)
281300050418     c*  il foglio si potrebbe trovare in fnfva01r, er cui conttrollo da
281400050418     c*  FNFGV
281500070221    6c                   if        d53err='2' and dls45FLG='T'
281600050418     c                   movel     'G'           d53tfo
281700050418     C                   CALL      'FNLV53R'
281800050418     C                   PARM                    DSLV53
281900050418     c* se non ci sono errori --> OK esco dal ciclo
282000050418    7c                   if        d53err=*blanks
282100050418     C                   MOVEL     '1'           WTROVA            1
282200050418   x7c                   else
282300050418     C                   eval      wrileggi='1'
282400050418    7c                   endif
282500050418   x6c                   else
282600050418     C                   eval      wrileggi='1'
282700050418    6c                   endif
282800050222    5C                   ENDIF
282900110103     c*
283000050222     c* Per anom 60/61 se trovata spunta --> ok
283100160624   X4C**                 ELSE
283200160624     C**                 MOVEL     '1'           WTROVA            1
283300050222     c                   endif
283400110103     c*
283500020214   X3C                   ELSE
283600050215     C                   eval      wrileggi='1'
283700020214    3C                   ENDIF
283800110103     c*
283900031204   2ac                   else
284000031204     c** bolla     locale: cerco spunta mia
284100040303    3C     BR9FGS        IFeq      BTFP
284200031204     C                   MOVEL     '1'           WTROVA            1
284300031204   X3C                   ELSE
284400050215     C                   eval      wrileggi='1'
284500031204    3C                   ENDIF
284600031204   2aC                   ENDIF
284700031204    2C                   ENDIF
284800050215     c
284900050215     c                   if        wrileggi='1'
285000070131     C     KBAR3         READE     FNBRV09L                               99
285100050215     c                   endif
285200020214    1C                   ENDDO
285300160725     c*
285400160725     c* Se non trovata e si tratta di bolla locale: cerco spunta
285500160725     c*  entrata
285600160725     c                   if        wtrova=' ' and wsploc=*blanks
285700160725     C                   Z-ADD     5             WNPG
285800160725     C     KBAR3         chain     FNBRV09L
285900160725     c                   if        %found(fnbrv09l) and br9fgs=btfp
286000160725     C                   MOVEL     '1'           WTROVA            1
286100160725     c                   setoff                                       99
286200160725     c                   endif
286300160725     c                   endif
286400000316     C*
286500000316     C* 99 ON  - SPUNTA NON TROVATA --> CREO ANOMALIA PER 50/55/56
286600000316    0C     *IN99         IFEQ      *ON
286700000316     C     COMCAA        ANDNE     60
286800000614     C     COMCAA        ANDNE     61
286900000316     C* 99 OFF - SPUNTA     TROVATA --> CREO ANOMALIA PER 60
287000160624     C**   *IN99         OREQ      *OFF
287100160624     C**   COMCAA        ANDEQ     60
287200160624     C**   *IN99         OREQ      *OFF
287300160624     C**   COMCAA        ANDEQ     61
287400000316     C                   CLEAR                   FNANM000
287500040621     c*
287600000316     C* MEMORIZZO NUMERO SPEDIZIONE SU ANOMALIA SE C'E'
287700000316    1C                   SELECT
287800040504     C     WESBTT        wheneq    'S'
287900000316     C                   Z-ADD     BTTAAS        ANMAAS
288000000316     C                   Z-ADD     BTTLNP        ANMLNP
288100000316     C                   Z-ADD     BTTNSP        ANMNSP
288200040504     C     WESART        wheneq    'S'
288300000316     C                   Z-ADD     ARTAAS        ANMAAS
288400000316     C                   Z-ADD     ARTLNP        ANMLNP
288500000316     C                   Z-ADD     ARTNSP        ANMNSP
288600040504     C     WESblt        wheneq    'S'
288700040504     C                   Z-ADD     bltAAS        ANMAAS
288800040504     C                   Z-ADD     bltLNP        ANMLNP
288900040504     C                   Z-ADD     bltNSP        ANMNSP
289000000316    1C                   ENDSL
289100040621     c* Se c'e' la spedizione verifico se spedizione di valore
289200110103     c*
289300040621     C* linea partenza segnacollo
289400000316     C                   MOVEL     BARLNP        ANMFLS
289500000316     C* PER ANOMALIA 50
289600000316    1C     COMCAA        IFEQ      50
289700000316     C                   Z-ADD     BARFGS        ANMFLE
289800040908     C* PER BOLLA LOCALE non c'e' abbinamento di foglio per cui deduco le
289900040908     c*  date damettere sulla anomalia
290000040908     c                   clear                   wnofva            1
290100040908     c                   if        wtibol='L'
290200040908     c                   eval      wnofva='S'
290300040908     c                   endif
290400040908     c                   if        wesart='S' and arbtfp=arbtfa
290500040908     c                   eval      wnofva='S'
290600040908     c                   endif
290700050603     c                   if        wesart='N' and lnptfp=Usalnatfa
290800040908     c                   eval      wnofva='S'
290900040908     c                   endif
291000040908     c
291100040908   1ac                   if        wnofva='S'
291200040908    2c                   if        wesart='S'
291300020218     C                   Z-ADD     ARBDBR        SAVDAO
291400020218     C                   Z-ADD     ARBDBR        ANMDAO
291500020218     C                   Z-ADD     99998         ANMNFV
291600020218     C                   Z-ADD     LNPTFP        ANMCDU
291700020218     C                   Z-ADD     LNPTFP        WFL1
291800020218     C                   MOVEL     'L'           WFLGT
291900040908     c*
292000040908   x2c                   else
292100040908     c* Se non c'e' bolla cerco il 1° foglio locale con data < data spunta
292200040908     c     kfgv2         setll     fnfgv02l
292300040908     c     lnptfp        readpe    fnfgv02l
292400040908    3c                   dow       not %eof(fnfgv02l)  and
292500040908     c                             fgvttr <>'L'
292600040908     c     lnptfp        readpe    fnfgv02l
292700040908    3c                   enddo
292800110103     c*
292900040908    3c                   if        not %eof(fnfgv02l)
293000040908     C                   Z-ADD     fgvdfv        SAVDAO
293100040908     C                   Z-ADD     fgvdfv        ANMDAO
293200040908     C                   Z-ADD     99998         ANMNFV
293300040908     C                   Z-ADD     LNPTFP        ANMCDU
293400040908     C                   Z-ADD     LNPTFP        WFL1
293500040908     C                   MOVEL     'L'           WFLGT
293600040908    3c                   endif
293700040908    2c                   endif
293800110103     c*
293900040908  x1aC                   ELSE
294000070208     C* SE TROVA LA BOLLA IN BLP C'E' SEMPRE LNP BOLLA
294100020214     C*  ALTRIMENTI LA 5F ALTRIMENTI BARLNP
294200020214     C                   MOVEL     LNPB          WLIN
294300020214     C* IMPOSTO LA DATA DI RIFERIMENTO PER LA RICERCA DEL TERMINAL
294400020214     C*  DI PARTENZA: LA PRIMA VOLTA E' LA DATA DELLA BOLLA SE C'E'
294500020214     C                   MOVE      DSPB          WDTRIF
294600000316     C*
294700000316     C* CERCO IL NUMERO DI FV SU CUI DOVREBBE ESSERE PARTITO IL COLLO
294800000316     C                   MOVE      *BLANKS       WFLGT             1
294900000316     C                   CLEAR                   WLINPR
295000000316     C                   CLEAR                   WVALNP
295100000316    2C     WLIN          DOWGT     0
295200000316     C                   EXSR      RICFV
295300000316     C* MEMORIZZO IL P.O. DI TRANSITO E LA LINEA DI PARTENZA DEL FV
295400000316     C     WLIN          IFGT      *ZEROS
295500000316     C                   Z-ADD     WLIN          WLINPR
295600000316     C                   Z-ADD     FVALNP        WVALNP
295700000316     C                   ENDIF
295800000316    2C                   ENDDO
295900000316    2C     ANMCDU        IFGT      0
296000000316     C                   MOVEL     ANMCDU        WFL1
296100000316   X2C                   ELSE
296200070208     C* SE TROVA LA BOLLA IN BLP C'E' SEMPRE LNP BOLLA
296300020214     C*  ALTRIMENTI LA 5F ALTRIMENTI BARLNP
296400000316     C                   MOVEL     LNPB          WFL1
296500000316    2C                   ENDIF
296600020218   1AC                   ENDIF
296700000316     C*
296800000316   X1C                   ELSE
296900160624     C* PER ANOMALIA 55/56/57/      (NO IDD LE ALTRE)
297000000316     C                   Z-ADD     BARDFV        ANMDAO
297100000316     C                   MOVEL     BARFGS        ANMFLE
297200000316     C                   MOVEL     LNPB          WFL1
297300000316     C                   Z-ADD     BARNFV        ANMNFV
297400000316     C                   MOVEL     BARFGS        ANMCDU
297500000316    1C                   ENDIF
297600000316     C* SCRIVO L'ANOMALIA
297700000316     C                   EXSR      RIEANM
297800040621     c*
297900040621     c                   if        anmnsp>0
298000091209     c     kar5          chain(n)  fiar501l
298100040621    3c                   if        %found(fiar501l)
298200040621     c                   movel     ar5uni        dar5gen
298300040621     c                   else
298400040621     c                   clear                   dar5gen
298500040621    3c                   endif
298600040621     c                   movel     §ar5bva       anmft4
298700040621     c                   endif
298800000614     C*
298900000316     C* ESTERNA
299000000614    1C     §7CAIE        IFEQ      'E'
299100000316     C                   MOVEL     WFL1          ANMFL1
299200000614    1C                   ENDIF
299300000316     C* SCRIVO L'ANOMALIA SOLO SE HO TROVATO IL FOGLIO PARTENZA PER"50"
299400000614    1C     ANMCAA        IFNE      50
299500000316     C     ANMCAA        OREQ      50
299600000316     C     ANMNFV        ANDGT     0
299700000316     C* SE LA KEY E' PER SEGNACOLLO ED IL NUMERO E' 0 NON CREO L'ANOMAL
299800000316    2C     §7CKEY        IFNE      'S'
299900000316     C     §7CKEY        OREQ      'S'
300000000316     C     ANMSCN        ANDGT     0
300100001110     C************************
300200001110     C* CREO ANOMALIA SOLO SE:
300300001110     C************************
300400070215     C* ANOMALIA CREABILE ANCHE DA CAT.SPUNTA GENERICA
300500070215     C*                                 (ANCHE DA SPUNTA IMI)
300600070215     C     §7CCCG        ifeq      'S'
300700070215     C* CATEGORIA SPUNTA NON GENERICA   (NON È UNA SPUNTA IMI)
300800070215     C     BARSPG        orne      'I'
300900000316     C                   WRITE     FNANM000
301000001110     C*
301100001110     C* CHIUDO SE POSSIBILE UNA EVENTUALE 55 GIA' CREATA E POI CREO
301200001110     C*  LA 56
301300001110    3C     COMCAA        IFEQ      56
301400001110     C                   Z-ADD     55            COMCAA
301500001110     C                   MOVEL     'S'           SAVMAC
301600001110     C                   EXSR      CHIUAN
301700001110     C                   CLEAR                   SAVMAC
301800001110    3C                   ENDIF
301900000614    3C                   ENDIF
302000000614     C*
302100000316     C* PER ANOMALIA 50 CREATA AGGIORNO ANCHE LA DATA DI PARTENZA
302200000316     C* O LA DATA DI USCITA DAL TRANSITO
302300000316     C     ANMCAA        IFEQ      50
302400000316     C                   EXSR      AGGPAR
302500000316     C                   ENDIF
302600160727     C     ANMCAA        IFEQ      55
302700160727     C     ANMCAA        oreq      56
302800160727     C                   EXSR      AGGPAR_d
302900160727     C                   ENDIF
303000000316     C*
303100000316    2C                   ENDIF
303200020222     C*
303300020222     C                   ELSE
303400020222     C* SE NON TROVATO FOGLIO ATTENDO CHE ARRIVI (CI DEVE ESSERE PRIMA
303500031204     C*  O POI) se non locale
303600060103     c* Solo se non richiamato
303700060103     c  N02wtibol        ifne      'L'
303800020222     C                   SETON                                        3940
303900031204     c                   endif
304000000614    1C                   ENDIF
304100000614    0C                   ENDIF
304200000316     C                   ENDSR
304300130228     C**************************************************************************
304400130228     C* controllo se bolla con colli chde partono da divderse filiali
304500130228     C**************************************************************************
304600130228     C     CtrXCO        BEGSR
304700130228     c                   eval      Wflsnrs=(BARLNP*100)+BARnrs
304800130228
304900130228    1c                   if        wflsnrs<>savfnrs
305000130228     c                   clear                   xcotfp
305100130228     C                   MOVEL     'XCO'         kcod
305200130228     C                   eval      kke1='ALTRITFP'
305300130228     C                   eval      kke2=%editc(wflsnrs:'X')
305400130228     c     ktbe2         chain     tntbe01l
305500130228    2c                   if        %found(tntbe01l) AND tbeatb=' '
305600130228     C                   MOVEA     TBeUNI        xcotfp
305700130228    2c                   endif
305800130228     c
305900130228     c                   eval      savfnrs=wflsnrs
306000130228    1c                   endif
306100130228    c
306200130228     C* SE la fil spunte è compresa --> creo la 57
306300130228     c                   movel     barfgs        alfafgs           3
306400130228     c                   eval      Indx=%lookup(alfafgs:xcotfp)
306500130228     c
306600130228     c*
306700130228     C                   ENDSR
306800950927     C**************************************************************************
306900950927     C* RICERCA FV SU CUI E' PARTITA LA SPUNTA
307000950927     C**************************************************************************
307100950927     C     RICFV         BEGSR
307200950927     C                   CLEAR                   WMODL
307300021121     C                   movel     'N'           Wnoabb2           1
307400021121     C                   movel     'N'           Wnoabb6           1
307500950927     C* PRENDO IL TERMINAL DI PARTENZA DELLA LINEA DI PARTENZA
307600970814     C                   CLEAR                   DSLV55
307700970814     C                   MOVEL     'P'           D55TPT
307800970814     C                   MOVEL     WLIN          D55LIN
307900020214     C                   MOVEL     WDTRIF        D55DRF
308000020213     C                   EXSR      FNLV55
308100970814     C                   MOVEL     D55TFP        WPTFP
308200110103     c*
308300100713     C* PRENDO IL TERMINAL DI arrivo generico della linea
308400100713     C                   CLEAR                   DSLV55
308500100713     C                   MOVEL     'A'           D55TPT
308600100713     C                   MOVEL     barlna        D55LIN
308700100713     C                   MOVEL     WDTRIF        D55DRF
308800100713     C                   EXSR      FNLV55
308900100713     C                   MOVEL     D55TFa        WGENlnaTFA
309000021121     c
309100021121     c* se WPTFP = simfel ed ho salvato i campi del foglio illuminato
309200021121     c*  trovato, li considero validi e non rieseguo la routine
309300021121     c                   if        wptfp=simfel  and wfvalnp>0
309400021121     C                   MOVEL     wFVALNP       ANMCDU
309500021121     C                   Z-ADD     wFVALNP       SAVFLP
309600021121     C                   Z-ADD     wFVANFV       ANMNFV
309700021121     C                   Z-ADD     wFVADFV       ANMDAO
309800021121     C                   Z-ADD     wFVADFV       SAVDAO
309900021121     C                   CLEAR                   WLIN
310000021121     C                   MOVEL     '1'           WMODL             1
310100021121     c                   else
310200950927     C*
310300950927     C* CONTROLLO I FV ABBINATI, SE E' FOGLIO ARRIVI
310400020214     C     BARNPG        IFEQ      2
310500950927     C                   Z-ADD     BARNPG        KNPG4
310600950927     C                   Z-ADD     BARNFV        KNFV4
310700020214     C                   Z-ADD     BARFGS        KFGS4
310800950927     C                   EXSR      REDFVA
310900020214     C                   ENDIF
311000950927     C*
311100950927     C* SE NON L'HO TROVATO CERCO UN ALTRO F.ARRIVI DEL GIORNO
311200950927     C     WMODL         IFEQ      ' '
311300021121     c                   eval      wdata=bardfv
311400950927     C                   EXSR      MEMFAR
311500021121     C* MEMORIZZO LA NUOVA DATA
311600021121     C                   MOVEL     BARDFV        SAVDFV
311700110103     C                   Z-ADD     1             I                 3 0
311800950927     C*
311900950927    2C     PG2(I)        DOWGT     0
312000950927     C     ANMNFV        ANDEQ     0
312100950927     C                   Z-ADD     2             KNPG4
312200950927     C                   Z-ADD     PG2(I)        KNFV4
312300020214     C                   Z-ADD     SAVTFA        KFGS4
312400950927     C                   EXSR      REDFVA
312500021121     c*
312600021121     c* Memorizzo se nessuno dei fogli 2 ha dei fogli partenza abbinati
312700021121     c                   if        wabbinati=' '
312800021121     c                   clear                   wnoabb2
312900021121     c                   endif
313000950927     C                   ADD       1             I
313100950927    2C                   ENDDO
313200950927    1C                   ENDIF
313300110103     c*
313400021121     c* Se non ho fogli abbianti e non si tratta di cat arrivi, ritento
313500021121     c*  la ricerca dei fogli arrivi
313600021121    1c     WMODL         IFEQ      ' '
313700110103    2c                   if        barnpg<> 2 and
313800110103     c                             wnoabb2 ='N' and
313900021121     c                             wsavdata=bardfv
314000021121     c* Data - 1
314100021121     C                   MOVEL     bardfv        G02INV
314200021121     C                   Z-ADD     *ZEROS        G02DAT
314300021121     C                   Z-ADD     *ZEROS        G02TGI
314400021121     C                   MOVEL     '3'           G02ERR
314500021121     C                   CALL      'XSRDA8'
314600021121     C                   PARM                    WLBDAT
314700021121      *
314800021121     C                   SUB       1             G02TGI
314900021121     C                   Z-ADD     *ZEROS        GI8DAT
315000021121     C                   Z-ADD     *ZEROS        GI8INV
315100021121     C                   Z-ADD     G02TGI        GI8TGI
315200021121     C                   CALL      'XSRGI8'
315300021121     C                   PARM                    WGIDAT
315400021121      *
315500021121     C                   Z-ADD     GI8INV        wdata
315600021121      *
315700021121     C                   EXSR      MEMFAR
315800021121     C* MEMORIZZO LA NUOVA DATA
315900021121     c                   if        wsavdata>0
316000021121     C                   MOVEL     wsavdata      SAVDFV
316100021121     c                   else
316200021121     C                   MOVEL     BARDFV        SAVDFV
316300021121     c                   endif
316400110103     c*
316500021121     C                   Z-ADD     1             I
316600021121     C*
316700021121    3C     PG2(I)        DOWGT     0
316800021121     C     ANMNFV        ANDEQ     0
316900021121     C                   Z-ADD     2             KNPG4
317000021121     C                   Z-ADD     PG2(I)        KNFV4
317100021121     C                   Z-ADD     SAVTFA        KFGS4
317200021121     C                   EXSR      REDFVA
317300021121     C                   ADD       1             I
317400021121    3C                   ENDDO
317500021121    2C                   ENDIF
317600021121    1c                   endif
317700110103     c*
317800100713     c* se il FV non ancora trovato e si dovrebbe trattare di bolla locale
317900100713     c*  tengo i dati del foglio illuiminato
318000100713     c     WMODL         ifeq      ' '
318100100713     c     wgenlnatfa    andeq     wlin
318200100713     C                   MOVEL     wFVALNP       ANMCDU
318300100713     C                   Z-ADD     wFVALNP       SAVFLP
318400100713     C                   Z-ADD     wFVANFV       ANMNFV
318500100713     C                   Z-ADD     wFVADFV       ANMDAO
318600100713     C                   Z-ADD     wFVADFV       SAVDAO
318700100713     C                   CLEAR                   WLIN
318800100713     C                   MOVEL     '1'           WMODL             1
318900100713     c                   endif
319000950927     C*
319100950927     C* SE IL FV ANCORA NON C'E' CERCO SE ANCORA DA ABBINARE
319200021121    1C     WMODL         IFEQ      ' '
319300950927     C                   CLEAR                   KNFV4
319400950927     C                   CLEAR                   KNPG4
319500020214     C                   MOVE      SAVTFA        KFGS4
319600000223     C* Determino BARDFV-4 gg. che è la data minima che deve avere un
319700000223     C* foglio ancora da abbinare. Se la data foglio è più bassa di
319800000223     C* tale data il foglio verrà scartato
319900000223     C                   MOVE      *ZEROS        DATMIN
320000000223     C                   CLEAR                   WLBDAT
320100000223     C                   CLEAR                   WGIDAT
320200000223     C                   Z-ADD     BARDFV        G02INV
320300000223     C                   MOVEL     '3'           G02ERR
320400000223     C                   CALL      'XSRDA8'
320500000223     C                   PARM                    WLBDAT
320600000223     C     G02TGI        SUB       4             GI8TGI
320700000223     C                   CALL      'XSRGI8'
320800000223     C                   PARM                    WGIDAT
320900000223     C                   Z-ADD     GI8INV        DATMIN
321000950927     C                   EXSR      REDFVA
321100950927     C* SE ANCORA NON HO TROVATO NIENTE --> ESCO DAL CICLO
321200950927    2C     WMODL         IFEQ      ' '
321300950927     C                   CLEAR                   WLIN
321400950927    2C                   ENDIF
321500950927    1C                   ENDIF
321600990827     C*
321700990827     C* PULISCO WLIN PER NON CONTINUARE NELLA RICERCA DEL FOGLIO
321800990827     C* SE LA LINEA DI ARRIVO RISULTA TRANSITARE
321900990827     C* DA UN P.O. CHE  E' LO STESSO DAL QUALE IL COLLO ERA PARTITO
322000990827     C* (PER ES.: 53/31 CHE TRANSITA DA 062 E 062 LO FA TRANSITARE DA
322100990827     C* 53 (CASO CHE SI VERIFICA DURANTE IL PASSAGGIO DALLE TRAZIONI
322200990827     C* RIDOTTE ALLE TRAZIONI NORMALI))
322300990827     C     WLINPR        IFGT      *ZEROS
322400990827     C     WLIN          ANDGT     *ZEROS
322500990827     C     WLIN          ANDEQ     WVALNP
322600990827     C                   CLEAR                   WLIN
322700990827     C                   ENDIF
322800021121     C                   ENDIF
322900950927     C*
323000950927     C                   ENDSR
323100950927     C**************************************************************************
323200950927     C* LETTURA DEI DEI FOGLI VIAGGIO PARTENZA
323300950927     C**************************************************************************
323400950927     C     REDFVA        BEGSR
323500950927     C                   CLEAR                   WMODL
323600021121     C                   CLEAR                   Wabbinati         1
323700950927     C     KFVA4         SETLL     FNFVA000
323800950927     C     KFVA4         READE     FNFVA000                               99
323900021121     c* se 99 on - nessun foglio abbinato --> memorizzo
324000021121     c                   if        *in99
324100021121     c                   eval      wabbinati='N'
324200021121     c                   endif
324300950927     C*
324400950927    1C     *IN99         DOWEQ     *OFF
324500950927    2C     FVALNP        IFEQ      WPTFP
324600950927     C* DATA FOGLIO DEVE ESSERE <= ALLA DATA DI ARRIVO
324700960524     C     FVADFV        ANDLE     BARDFV
324800000223     C     KNPG4         ANDGT     *ZEROS
324900000223     C* SE STO CERCANDO FOGLIO ANCORA DA ABBINARE SCARTO SE E' DI DATA
325000000223     C* INFERIORE ALLA DATA MINIMA (DATA ARRIVO COLLO MENO QUATTRO GG.)
325100000223     C     FVALNP        OREQ      WPTFP
325200000223     C     FVADFV        ANDLE     BARDFV
325300000223     C     FVADFV        ANDGE     DATMIN
325400000223     C     KNPG4         ANDEQ     *ZEROS
325500960207     C*
325600950927     C* SE SI TRATTA DI UN FOGLIO DIRETTO TUTTO A POSTO
325700100712     c* ok anche se locale
325800950927    3C     FVAATR        IFEQ      ' '
325900100712     c     fvattr        oreq      'L'
326000950927     C                   MOVEL     FVALNP        ANMCDU
326100020221     C                   Z-ADD     FVALNP        SAVFLP
326200990518     C                   Z-ADD     FVANFV        ANMNFV
326300950927     C                   Z-ADD     FVADFV        ANMDAO
326400980120     C                   Z-ADD     FVADFV        SAVDAO
326500950927     C                   CLEAR                   WLIN
326600950927     C                   MOVEL     '1'           WMODL             1
326700980120     C* SE IL PRIMO FV TROVATO E' UN DIRETTO LO MEMORIZZO PER SAPERE SE
326800980120     C* DOVRO' AGGIOR.LA DATA DI PARTENZA O LA DATA USCITA DAL TRANSITO
326900980120     C     WFLGT         IFEQ      *BLANKS
327000980120     C                   MOVE      'D'           WFLGT
327100980120     C                   END
327200950927     C                   SETON                                        99
327300960207     C*
327400950927   X3C                   ELSE
327500960207     C* SE E' FOGLIO ILLUMINATO
327600950927     C* DEVO VEDERE DA DOVE E' TRANSITO E MANDARE AL TRANSITO LA SEGNA
327700950927     C*  LAZIONE
327800000505     C     KFWA          CHAIN     FNFWA01L                           28
327900000505     C     *IN28         IFEQ      *ON
328000000505     C     FWAATB        ORNE      ' '
328100000505     C                   CLEAR                   FWAFF3
328200000505     C                   CLEAR                   FWAFF4
328300000505     C                   CLEAR                   FWAFL3
328400000505     C                   CLEAR                   FWAFL4
328500000505     C                   ENDIF
328600000505     C**
328700950927     C                   Z-ADD     FVALNA        LNA2
328800950927     C                   Z-ADD     1             X
328900951002     C                   MOVEL     BARLNA        W003A             3
329000950927     C     W003A         LOOKUP    FFV(X)                                 99
329100960207     C* NON E' IL MIO TERMINAL
329200960902     C* ESCLUDO I FOGLI LOCALI
329300021125     c* in relatà elaboro anche i fogli locali, altrimenti come faccio a
329400021125     c*  determinare un collo arrivo no part, del terminal per la lna?
329500021125     c*  ES. 43  x 132
329600960207    4C     *IN99         IFEQ      *ON
329700021125     C***  FVATTR        ANDNE     'L'
329800050603     C                   MOVEL     Usalnatfa     W003A             3
329900960522    5C     FLP(X)        IFNE      W003A
330000960207     C                   MOVEL     FLP(X)        WLIN
330100020214     C                   MOVE      FVADFV        WDTRIF
330200960207     C                   MOVEL     '1'           WMODL             1
330300980120     C* SE IL PRIMO FV TROVATO E' UN TRANSITO LO MEMORIZZO PER SAPERE
330400980120     C* SE DOVRò AGGIOR. DATA DI PARTENZA O DATA USCITA DAL TRANSITO
330500980120     C     WFLGT         IFEQ      *BLANKS
330600980120     C                   MOVE      'T'           WFLGT
330700021121     c* Memorizzo lo stesso i campi che mi servono perchè se poi di tratta
330800021121     c*  di transito "locale" cioè su stesso as (vedi 82 con 113)
330900021121     c*  devo comunque tenere i dati del foglio illuminato non essendoci
331000021121     c*  abbinamento del foglio locale su stessa £1
331100021121     c*  Il problema verrà a morire quando diventeranno dei 1 livello
331200021121     c*  come devono essere
331300021121     c*
331400021121     c                   z-add     FVALNP        WFVALNP
331500021121     c                   z-add     FVANFV        WFVANFV
331600021121     c                   z-add     FVADFV        WFVADFV
331700980120     C                   END
331800960207   X5C                   ELSE
331900960207     C* SE E' FOGLIO ILLUMINATO PER 2 LIVELLO IN ARRIVO TUTTO A POSTO
332000960207     C                   MOVEL     FVALNP        ANMCDU
332100990518     C                   Z-ADD     FVANFV        ANMNFV
332200960207     C                   Z-ADD     FVADFV        ANMDAO
332300980120     C                   Z-ADD     FVADFV        SAVDAO
332400020221     C                   Z-ADD     FVALNP        SAVFLP
332500960207     C                   CLEAR                   WLIN
332600960207     C                   MOVEL     '1'           WMODL             1
332700980120     C* SE IL PRIMO FV TROVATO E' UN DIRETTO LO MEMORIZZO PER SAPERE SE
332800980120     C* DOVRO' AGGIOR.LA DATA DI PARTENZA O LA DATA USCITA DAL TRANSITO
332900980120     C     WFLGT         IFEQ      *BLANKS
333000980120     C                   MOVE      'D'           WFLGT
333100980120     C                   END
333200960207    5C                   ENDIF
333300960207     C*
333400960207   X4C                   ELSE
333500960207     C     KFVA4         READE     FNFVA000                               99
333600960207    4C                   ENDIF
333700950927    3C                   ENDIF
333800950927     C*
333900950927   X2C                   ELSE
334000950927     C     KFVA4         READE     FNFVA000                               99
334100950927    2C                   ENDIF
334200950927    1C                   ENDDO
334300950927     C                   ENDSR
334400980119     C**************************************************************************
334500980119     C* AGGIORNO DATA PART. O DI USCITA DA TRANS. A FRONTE DI ANOM.50
334600980119     C**************************************************************************
334700980119     C     AGGPAR        BEGSR
334800980119     C     KBAR          CHAIN     FNART27L                           3239
334900980119     C   39              GOTO      ENDPAR
335000160722     c
335100160722    1c     *IN32         IFEQ      *OFF
335200160722     c* Spunta di bolla in arrivo ( anche locale)
335300160722    2C                   if        WTIBOL='A'  or WTIBOL='L'
335400160722
335500980120    3C                   SELECT
335600980120     C     WFLGT         WHENEQ    'D'
335700980120     C                   Z-ADD     SAVDAO        ARTDFV
335800041201     C                   Z-ADD     anmnfv        ARTnFV
335900980120     C                   Z-ADD     88            ARTNPP
336000041202     C                   movel     'S'           ARTABN
336100020218     C     WFLGT         WHENEQ    'L'
336200020218     C                   Z-ADD     SAVDAO        ARTDFV
336300041201     C                   Z-ADD     anmnfv        ARTnFV
336400020218     C                   Z-ADD     90            ARTNPP
336500041202     C                   movel     'S'           ARTABN
336600980120     C     WFLGT         WHENEQ    'T'
336700980120     C                   Z-ADD     SAVDAO        ARTDUT
336800980120     C                   Z-ADD     89            ARTPUT
336900020221     C                   Z-ADD     SAVFLP        ARTFLP
337000041202     C                   movel     'S'           ARTABN
337100980120    3C                   ENDSL
337200160722    2c                   endif
337300160722     c*
337400160722     c* spunta al transito: devo aggiornare anche  in FNART
337500160722     c*  la data di partenza se non c'è
337600160722    2c                   if        WTIBOL='T'
337700160722     c
337800160722    3c                   if        artdfv=0
337900160722     C                   Z-ADD     SAVDAO        ARTDFV
338000160722     C                   Z-ADD     anmnfv        ARTnFV
338100160722     C                   Z-ADD     88            ARTNPP
338200160722    3c                   endif
338300160722    2c                   endif
338400160720
338500980120     C                   UPDATE    FNART000
338600980120     C                   MOVEL     'A'           WTBO              1
338700980120     C                   MOVEL     ARTSPE        WDSPE
338800980120     C                   EXSR      WR1AGB
338900160722    1C                   endif
339000160720
339100160722     C                   if        WTIBOL='T'
339200021203     C     KBAR37        CHAIN     FNBTT37L                           3739
339300980119     C   39              GOTO      ENDPAR
339400161108    2C     *IN37         IFEQ      *OFF
339500980120     C                   Z-ADD     SAVDAO        BTTDFV
339600980120     C                   Z-ADD     88            BTTNPP
339700980120     C                   UPDATE    FNBTT000
339800980120     C                   MOVEL     'T'           WTBO              1
339900980120     C                   MOVEL     BTTSPE        WDSPE
340000980120     C                   EXSR      WR1AGB
340100980120    2C                   END
340200160722    1C                   endif
340300160720
340400160720     C*  AGGIORNO ANCHE LA BOLLA PARTENZA
340500020218    1C     WTIBOL        IFEQ      'L'
340600160720     C     WTIBOL        oreq      'A'
340700160721     C     WTIBOL        oreq      'T'
340800020218     C     KBAR          CHAIN     FNBLT27L                           3239
340900020218     C   39              GOTO      ENDPAR
341000020218    2C     *IN32         IFEQ      *OFF
341100160721
341200160721     c                   if        wtibol<>'T'
341300160721     c                   z-add     anmdch        bltdam
341400160721     c                   else
341500160721     c                   z-add     anmdch        bltdet
341600160721     c                   z-add     bpes          bltflp
341700160722     c                   clear                   bltdut
341800160721     c                   endif
341900160720
342000160721    3C                   IF        WFLGT<>'T' AND
342100160721     c                             (bltdfv=0 or bltdfv>savdao) or
342200160721     c                             wflgt= 'T' and bltdfv=0
342300160721     c
342400160720     C                   Z-ADD     SAVDAO        bltDFV
342500160726     c                   eval      welasta_p='S'
342600160720
342700160721    4c                   select
342800160720     C     BLTDSE        WHENEQ    0
342900160720     c                   if        wflgt='D'
343000160720     C                   MOVEL     88            BLTNPP
343100160721     c                   else
343200160720     C                   Z-ADD     90            bltNPP
343300160720     c                   endif
343400160720     C     BLTDSE        WHENGT    0
343500160720     C     BLTDSE        ANDLT     BLTDFV
343600160720     C                   MOVEL     96            BLTNPP
343700160720     C     BLTDSE        WHENGT    0
343800160720     C     BLTDSE        ANDGE     BLTDFV
343900160720     C                   MOVEL     98            BLTNPP
344000160721    4C                   ENDSL
344100160720
344200160721    3c                   endif
344300160720
344400160722    3C                   if        wflgt='T'  and WTIBOL<>'T' and
344500160722     c                             (bltdut=0 or bltdut>savdao)
344600160720     C                   Z-ADD     SAVDAO        bltDUT
344700160722     C                   Z-ADD     bpes          bltFLP
344800160721    3c                   endif
344900160720     c
345000020218     C                   UPDATE    FNBLT000
345100020218     C                   MOVEL     'P'           WTBO              1
345200020218     C                   MOVEL     BLTSPE        WDSPE
345300020218     C                   EXSR      WR1AGB
345400160721     c
345500160721     c* se c'è transito, aggiorno anche BTT
345600160721    3C     WFLGT         ifeq      'T'
345700160721     C     KBAR_t        CHAIN     FNBTT37L                           3239
345800160721    4C     *IN32         IFEQ      *OFF
345900160721     C     *IN39         andeq     *OFF
346000160721
346100160726     c                   eval      welasta_p='S'
346200160721     C                   Z-ADD     SAVDAO        bttDUT
346300160721     c                   z-add     anmdch        bttdam
346400160721     C                   SELECT
346500160721     C     BTTDET        WHENEQ    0
346600160721     C                   MOVEL     89            BTTPUT
346700160721     C     BTTDET        WHENGT    0
346800160721     C     BTTPET        ANDEQ     89
346900160721     C                   MOVEL     89            BTTPUT
347000160721     C     BTTDET        WHENGT    0
347100160721     C     BTTDET        ANDLT     BTTDUT
347200160721     C                   MOVEL     96            BTTPUT
347300160721     C     BTTDET        WHENGT    0
347400160721     C     BTTDET        ANDGE     BTTDUT
347500160721     C                   MOVEL     98            BTTPUT
347600160721     C                   ENDSL
347700160721     C*
347800160721     C     BTTDET        IFEQ      0
347900160721     C                   Z-ADD     ANMDAO        BTTDET
348000160721     C                   MOVEL     89            BTTPET
348100160721     C                   ENDIF
348200160721     C                   UPDATE    FNBTT000
348300160721     C                   MOVEL     'T'           WTBO              1
348400160721     C                   MOVEL     BLTSPE        WDSPE
348500160721     C                   EXSR      WR1AGB
348600160721    4C                   ENDif
348700160721    3C                   ENDif
348800160720
348900020218    2C                   END
349000020218    1C                   END
349100160721
349200980119     C     ENDPAR        TAG
349300160721
349400980119     C   39              SETON                                        40
349500160726     c
349600160726     c                   clear                   welasta_p
349700160726     c
349800980119     C                   ENDSR
349900160727     C**************************************************************************
350000160727     C* AGGIORNO DATA entrata al transito per DIsguido
350100160727     C**************************************************************************
350200160727     C     AGGPAR_d      BEGSR
350300160727     C     KBAR          CHAIN     FNBLT27L                           3239
350400160727    2C     *IN32         IFEQ      *OFF
350500160727    2C     *IN39         andeq     *OFF
350600160727     C                   Z-ADD     SAVDAO        bltDET
350700160727     c                   clear                   bltdut
350800160727     c                   movel     barfgs        bltflp
350900160727     c                   update    fnblt000
351000160727
351100160727     C                   MOVEL     'P'           WTBO              1
351200160727     C                   MOVEL     BLTSPE        WDSPE
351300160727     C                   EXSR      WR1AGB
351400160727     c                   endif
351500160727     c
351600160727     C                   ENDSR
351700950927     C**************************************************************************
351800950927     C* MEMORIZZO I FOGLI ARRIVI CETEGORIA 2 E 6 DEL GIORNO
351900950927     C**************************************************************************
352000950927     C     MEMFAR        BEGSR
352100021121     c                   clear                   wsavdata          8 0
352200021113     c
352300021113     c* se è collo di bolla transito
352400021113     c*  devo usare simfel, il suo p.o. di transito
352500021113     c                   if        wtibol<>'T'
352600050603     C                   MOVEL     usalnatfa     KTFA2
352700021113     c                   else
352800021113     C                   z-add     simfel        KTFA2
352900021113     c                   endif
353000021113     c
353100950927     C* RIMEMORIZZO I FV SOLO SE E' CAMBIATA LA  DATA
353200021121    1C     wdata         IFNE      SAVDFV
353300970113     C     KTFA2         ORNE      SAVTFA
353400021107     c
353500021121     c* Cerco fogli cat 2 e 6  se non trovati in data BARDFV, li cerco
353600021107     c*  in data bardfv - 1
353700021113     c* faccio max 8 tentativi (1 settimana  tenendo conto che ci possono
353800021113     c*  essere dei festivi)
353900950927     C                   CLEAR                   PG2
354000021113     c                   z-add     0             wgiorni           2 0
354100021113     c
354200021113    2c                   dow       wdata>0  and wgiorni<8
354300021113     c                   add       1             wgiorni
354400950927     C*
354500950927     C                   Z-ADD     2             WNPG
354600950927     C     KFVV          SETLL     FNFVV002
354700950927     C     KFVV          READE     FNFVV002                               33
354800950927     C                   Z-ADD     1             I
354900950927     C*
355000950927     C     *IN33         DOWEQ     *OFF
355100950927     C                   Z-ADD     FV2NFV        PG2(I)
355200950927     C                   ADD       1             I
355300950927     C     KFVV          READE     FNFVV002                               33
355400950927     C                   ENDDO
355500021107     c*
355600021107     c                   xfoot     pg2           wpg2             13 0
355700110103     c                   if        wpg2>0
355800021121     c                   eval      wsavdata=wdata
355900021107     c                   clear                   wdata
356000021107     c                   else
356100021107     c* Data - 1
356200021107     C                   MOVEL     wdata         G02INV
356300021107     C                   Z-ADD     *ZEROS        G02DAT
356400021107     C                   Z-ADD     *ZEROS        G02TGI
356500021107     C                   MOVEL     '3'           G02ERR
356600021107     C                   CALL      'XSRDA8'
356700021107     C                   PARM                    WLBDAT
356800021107      *
356900021107     C                   SUB       1             G02TGI
357000021107     C                   Z-ADD     *ZEROS        GI8DAT
357100021107     C                   Z-ADD     *ZEROS        GI8INV
357200021107     C                   Z-ADD     G02TGI        GI8TGI
357300021107     C                   CALL      'XSRGI8'
357400021107     C                   PARM                    WGIDAT
357500021107      *
357600021107     C                   Z-ADD     GI8INV        wdata
357700021107     c                   endif
357800021107     c                   enddo
357900021121     c*
358000970113     C                   MOVEL     KTFA2         SAVTFA
358100021107    1C                   ENDIF
358200950927     C                   ENDSR
358300950927     C**************************************************************************
358400950927     C* RIEMPO PARTE DEI CAMPI DELLA ANOMALIA
358500950927     C**************************************************************************
358600950927     C     RIEANM        BEGSR
358700950927     C                   MOVEL     BARLNP        ANMFLS
358800950927     C                   MOVEL     BARLNA        ANMLNA
358900950927     C                   MOVEL     BARNRS        ANMNRS
359000950927     C                   MOVEL     BARNSC        ANMSCN
359100950927     C                   MOVEL     BARNPS        ANMNPS
359200950927     C                   Z-ADD     COMCAA        ANMCAA
359300040621     c                   z-add     999           anmfl4
359400040621     c* imposto in anmflo data e ora creazione spunta
359500050707     C                   TIME                    TIMEanm          14 0
359600050707     c                   movel     timeanm       wora4             4 0
359700050707     C                   move      timeanm       G02DAT
359800050707     C                   Z-ADD     *ZERO         G02INV
359900050707     C                   MOVEL     *BLANKS       G02ERR
360000050707     C                   CALL      'XSRDA8'
360100050707     C                   PARM                    WLBDAT
360200050707     C                   move      G02INV        WDAT6             6 0
360300040621     c                   movel     wdat6         anmflo
360400040621     c                   move      wora4         anmflo
360500950927     C* FASE APERTURA
360600950927     C     BARKEY        IFNE      SAVFOG
360700950927     C                   EXSR      RICFAS
360800950927     C                   END
360900950927     C                   MOVEL     COMFAS        ANMFSA
361000950927     C* CODICE ANOMALIA
361100950927     C     COMCAA        IFNE      SAVCAA
361200950927     C                   MOVEL     '7C'          COD
361300950927     C                   MOVEL(P)  COMCAA        KEY
361400950927     C     KTAB          CHAIN     TABEL                              34
361500950927     C   34              CLEAR                   DS7C
361600950927     C  N34              MOVEL     TBLUNI        DS7C
361700950927     C                   MOVEL     COMCAA        SAVCAA            3 0
361800950927     C                   ENDIF
361900950927     C*
362000950927     C                   MOVEL     §7CTAN        ANMTAN
362100950927     C                   MOVEL     §7CAIE        ANMAIE
362200950927     C                   MOVEL     §7CSAN        ANMSAN
362300950927     C                   MOVEL     §7CLID        ANMLID
362400950927     C* AUTOCHIUSA
362500950927     C     §7CCHA        IFEQ      'S'
362600960524     C                   MOVE      BARDFV        ANMDCH
362700950927     C                   MOVEL     COMFAS        ANMFSC
362800000320     C                   MOVEL     WCCH          ANMCCH
362900950927     C                   END
363000950927     C                   ENDSR
363100960523     C**************************************************************************
363200960523     C* CARICO LA L6 DELLA FILIALE GESTIONE FOGLIO
363300960523     C**************************************************************************
363400960523     C     CARL6         BEGSR
363500960523     C                   CLEAR                   DSUL06
363600960523     C                   MOVE      '£6'          D06COD
363700020213     C                   MOVEL     SAVPES        D06KEY
363800960523     C                   MOVEL     DSUL06        KPJBU
363900960523     C*
364000960523     C                   CALL      'TRUL06R'
364100960523     C                   PARM                    KPJBA
364200960523     C                   MOVEL     KPJBU         DSUL06
364300960523     C                   MOVEA     LIN           L6
364400960523     C                   ENDSR
364500960523     C**************************************************************************
364600960523     C* SE ESISTE GIA' UNA SPUNTA DELLA LNA, NON CREO ANOMALIE 145-146
364700960523     C**************************************************************************
364800960523     C     CTRSPU        BEGSR
364900960523     C                   MOVEL     'S'           WCREA
365000970117     C*
365100970117     C                   MOVEL     'S'           WCHIU             1
365200970117     C*"WCHIU" MI SERVE PER DETERMINARE SE POSSO CHIUDERE UNA ANOMALIA
365300970117     C* 145/146 CHE HA DATA ANOMALIA < DELLA DATA DELLA SPUNTA CHE
365400970117     C*  STO ELABORANDO MA POTREBBE COMUNQUE ESSERCI UNA SPUNTA
365500970117     C*  CHE MI MANTIENE APERTA IN DATA > TALE ANOMALIA
365600970117     C*  1) SE E' PRESENTE UN'ALTRA SPUNTA CON DATA > NON CHIUDO
365700970117     C*  2) SE E' PRESENTE UNA SPUNTA CON DATA = CATEGORIA 3 4 5
365800020204     C*     NON CHIUDO SE LA SPUNTA CHE STO SCARICANDO E' CAT 1 2 6
365900970117     C*  3) SE E' PRESENTE UNA SPUNTA CON DATA = CATEGORIA 3 4 5
366000970117     C*     E E' UNA SPUNTA DELLA LNA SE LA SPUNTA CHE STO SCARICANDO
366100970117     C*     E' CATEGORIA 3 4 5
366200960531     C**
366300020204    1C     WAGARR        IFEQ      'S'
366400960531     C**
366500070131     C     KBRV7         CHAIN     FNBRV07L                           33
366600960531    2C     *IN33         DOWEQ     *OFF
366700960531    3C     BR7KEY        IFNE      SA1KEY
366800960527     C                   MOVEL     BR7KEY        SA1KEY
366900960527     C                   Z-ADD     BR7NPG        WNPG
367000960527     C                   Z-ADD     BR7NFV        WNFV
367100021014     C                   Z-ADD     BR7FGS        WFGS
367200070906     C                   movel     BR7nps        Wnps
367300021014     C                   CLEAR                   RICBAR
367400960523     C                   EXSR      RILFOG
367500960523     C                   Z-ADD     WDFV          BRVDFV
367600140616     C                   movel     WFCF          BRVFCF
367700140616     C                   movel     Wspg          BRVspg
367800960531    3C                   ENDIF
367900040224     c* Terminal di arrivo della spunta letta
368000040224     C                   CLEAR                   DSLV55
368100040224     C                   MOVEL     'A'           D55TPT
368200040224     C                   MOVEL     LNPB          D55LNP
368300040224     C                   MOVEL     brvdfv        D55DRF
368400040224     C                   MOVE      Br7pes        D55LIN
368500040224     C                   EXSR      FNLV55
368600050603     c                   if        d55tfa=Usalnatfa
368700960531     C**
368800961210    3C     BRVDFV        IFGT      BARDFV
368900960523     C                   MOVEL     'N'           WCREA             1
369000970117     C                   MOVEL     'N'           WCHIU             1
369100961209   X3C                   ELSE
369200961209     C*
369300961210    4C     BRVDFV        IFEQ      BARDFV
369400961210     C     BR7NPG        ANDGE     3
369500961210     C     BR7NPG        ANDLE     5
369600970117     C*
369700970117     C     BARNPG        IFEQ      1
369800970117     C     BARNPG        OREQ      2
369900970117     C                   MOVEL     'N'           WCHIU
370000970117     C                   ENDIF
370100961209     C*
370200020213    5C     BR7PES        IFEQ      BARLNA
370300961209     C                   MOVEL     'N'           WCREA             1
370400970117     C*
370500970117     C     BARNPG        IFNE      1
370600970117     C     BARNPG        ANDNE     2
370700970117     C                   MOVEL     'N'           WCHIU
370800970117     C                   ENDIF
370900970117     C*
371000961209   X5C                   ELSE
371100020214    6C     BR7PES        IFNE      SAVPES
371200020214     C                   MOVEL     BR7PES        SAVPES
371300961209     C                   EXSR      CARL6
371400961209    6C                   ENDIF
371500961209     C*
371600961209     C     BARLNA        LOOKUP    L6                                     34
371700961209     C   34              MOVEL     'N'           WCREA             1
371800970117     C*
371900970117    6C   34BARNPG        IFNE      1
372000970117     C     BARNPG        ANDNE     2
372100970117     C                   MOVEL     'N'           WCHIU
372200970117    6C                   ENDIF
372300970117     C*
372400961209    5C                   ENDIF
372500961209    4C                   ENDIF
372600961209    3C                   ENDIF
372700040224    3C                   ENDIF
372800961209     C*
372900961209    3C     WCREA         IFEQ      'N'
373000961209     C                   SETON                                        33
373100961210     C                   ELSE
373200070131     C     KBRV7         READE     FNBRV07L                               33
373300961209    3C                   ENDIF
373400040224     C**
373500960531    2C                   ENDDO
373600960531     C*
373700960531    1C                   ENDIF
373800960523     C*
373900960523     C                   ENDSR
374000961202     C**************************************************************************
374100961202     C* CREO ANOMALIA 145 O 146 E CHIUDO L'ALTRA
374200961202     C**************************************************************************
374300961202     C     ANM14X        BEGSR
374400961202     C* LA CREO SOLO SE NON ESISTE UNA SPUNTA DELLA LNA CON DATA=>
374500961210     C     WDCM          IFGT      0
374600961210     C     WDCM          ANDGE     BARDFV
374700961210     C                   MOVEL     'N'           WCREA
374800961210     C                   ELSE
374900961202     C                   EXSR      CTRSPU
375000961210     C                   ENDIF
375100961202     C*
375200961202    5C     WCREA         IFEQ      'S'
375300020213     C     BARFLE        ANDEQ     BTFP
375400020213     C     BARFLE        ORNE      BTFP
375500961202     C                   Z-ADD     CAACRE        COMCAA
375600961202     C                   EXSR      CRTANM
375700961202     C* SOLO PER EXPORT
375800961202     C*
375900961202     C     CAACRE        IFEQ      146
376000961202    6C     WTIBOL        ANDNE     'A'
376100961202     C                   MOVEL     'S'           WTIBOL
376200961202    6C                   ENDIF
376300961202    5C                   ENDIF
376400961202     C*
376500961202     C* SE SPUNTA DA ALTRO AS MA NON DOVEVO CREARE IL COLLO DA INOLTRAR
376600961202     C*  LA CHIUDO SUBITO. ANCHE SE IL COLLO E' CONSEGNATO SENZA SPUNTE
376700020213    5C     BARFLE        IFNE      BTFP
376800961202    6C     WCREA         IFNE      'S'
376900961202     C                   Z-ADD     CAACRE        COMCAA
377000961211     C* IN QUESTO CASO NON DEVO CREARE L'ANOMALIA 620 IN QUANTO
377100961211     C*  LA SPEDIZIONE E' GIA' STATA CONSEGNA A COMUNQUE E' PRESENTE
377200961211     C*  UNA SPUNTA CON DATA >.
377300961211     C                   MOVEL     'N'           W620
377400961202     C                   EXSR      CHIUAN
377500961211     C                   MOVEL     'S'           W620
377600961211     C*
377700961209   X6C                   ELSE
377800961210     C                   EXSR      ANM600
377900961202    6C                   ENDIF
378000961209     C*
378100961202   X5C                   ELSE
378200961210    6C     WCREA         IFEQ      'S'
378300961209     C* PER ESTERO CREO ANOMALIA 645 E 600
378400961209     C                   EXSR      ANM600
378500961210    6C                   ENDIF
378600961210    5C                   ENDIF
378700961202     C*
378800961210     C                   MOVEL     'N'           W620              1
378900961202     C                   Z-ADD     CAACIU        COMCAA
379000961202     C                   EXSR      CHIUAN
379100961210     C                   MOVEL     'S'           W620              1
379200961202     C                   ENDSR
379300961213     C**************************************************************************
379400961213     C* LETTURA FOGLIO DELLA SPUNTA CHE STO CARICANDO E IMPOSTAZIONE
379500961213     C*  CAMPI DI COMOD
379600961213     C**************************************************************************
379700961213     C     BARFGV        BEGSR
379800961213     C*
379900961213    3C     BARKEY        IFNE      SAVKEY
380000961213     C                   MOVEL     BARKEY        SAVKEY
380100961213     C                   Z-ADD     BARNPG        WNPG
380200961213     C                   Z-ADD     BARNFV        WNFV
380300021014     C                   Z-ADD     BARFGS        WFGS
380400070906     C                   movel     BARnps        Wnps
380500021014     C                   MOVEL     'S'           RICBAR            1
380600961213     C                   EXSR      RILFOG
380700980313     C                   MOVEA     FFG           FFS
380800961213     C                   Z-ADD     WDFV          BARDFV
380900961213     C                   MOVEL     WSPG          BARSPG
381000021025     C                   MOVEL     WFGS          wwwFGS
381100021023     C                   Z-ADD     WFLE          BARFLE
381200140616     C                   movel     WFCF          BARFCF
381300970521     C*
381400970521     C     WTTR          IFNE      *BLANKS
381500970520     C     WTTR          LOOKUP    TTR                                    34
381600970520     C                   MOVE      *IN34         WDEFL             1
381700970521     C                   ELSE
381800970521     C                   MOVE      '0'           WDEFL             1
381900970521     C                   ENDIF
382000970521     C*
382100961213    3C                   ENDIF
382200021025     C* IL P.O. GESTIONE LO DEVO SEMPRE IMPOSTARE PERCHÈ PUÒ CAMBIARE
382300021031     C                   MOVEL     WWWFGS        BARFGS
382400070209     c
382500070209     c* Se si tratta di spunta partenza, il BPES deve essere il P.O foglio
382600070209     c*  per problemi con eventuale creazione di anomalie 200
382700070209     c                   eval      wpes=bpes
382800070209     c                   if        barnpg=1
382900070209     c                   eval      bpes=barfgs
383000070209     c                   endif
383100961213     C                   ENDSR
383200970203     C**************************************************************************
383300970203     C* ANOMALIE CHE DEVO SEMPRE CREARE O SEMPRE CHIUDERE SE COLLO
383400970203     C*  SPARATO NELL'AREA DI ARRIVO (COME TERMINAL DI ARRIVO)
383500970203     C**************************************************************************
383600970203     C     ANMSEM        BEGSR
383700970203     C*
383800970203     C* COLLO/BOLLA ARRIVI PRESENTE
383900970204     C* SE CONSEGNATO IL COLLO CREO SEMPRE ANOMALIA --> COLLO CONSEGNAT
384000970204    1C  N05WBOAR         IFEQ      'S'
384100990120    2C     BARDFV        IFGT      WATDCM
384200970204     C     WATDCM        ANDGT     0
384300970203     C                   Z-ADD     138           COMCAA
384400970203     C                   EXSR      CRTANM
384500990120    2C                   ENDIF
384600000705     C* SE COLLO CAT.8 SEGNALA SE LA RELATIVA DISTINTA 4 E'
384700000705     C*  GIA' CHIUSA CON ANOMALIA 139
384800040430     c                   if        watdcm>0  and barnpg=8
384900000705     C* SE NON E' CHIUSA VEDO SE IN FASE DI CHIUSURA: CHIAMO FNLV53R
385000000705     C                   CLEAR                   DSLV53
385100000705     C                   MOVEL     'V'           D53TFO
385200020213     C                   MOVEL     BTFP          D53FEL
385300000705     C                   Z-ADD     BARNFV        D53NFV
385400000705     C                   MOVEL     4             D53NPG
385500000705     C                   MOVEL     BARFGS        D53FGS
385600000705     C                   MOVEL     BARFLE        D53FLF
385700000705     C                   CALL      'FNLV53R'
385800000705     C                   PARM                    DSLV53
385900000705     C*
386000000705     C* FOGLIO CHIUSO (S) O IN FASE DI CHIUSURA (F)
386100000705    3C     D53FCF        IFEQ      'S'
386200000705     C     D53FCF        OREQ      'F'
386300040430     c* non creo se la relativa consegna anomala non lo prevede
386400040430    4c                   if        WABCCA<>*blanks and wabcca<>savcca
386500040430     C                   MOVEL     '7O'          COD
386600040430     C                   MOVEL(P)  wabcca        KEY
386700040430     C     KTAB          CHAIN     TABEL                              41
386800040430     C  N41              MOVEL     TBLUNI        DS7o
386900040430     c   41              clear                   §7o139
387000040430     c                   eval      savcca=wabcca
387100040430    4c                   endif
387200040430     c
387300040430    4c                   if        §7o139=*blanks or wabcca=*blanks
387400000705     C                   Z-ADD     139           COMCAA
387500000705     C                   EXSR      CRTANM
387600040430    4C                   ENDIF
387700040430    3C                   ENDIF
387800000705    2C                   ENDIF
387900000705     C*
388000990120     C* COLLO NON CONSEGNATO
388100990121    2C     WATDCM        IFEQ      0
388200990120     C* VERIFICO SE DEVO CREARE ANOMALIA SU DANNI
388300000523     C*  NON LO CREO PER LA CATEGORIA 4 E 8
388400990121     C                   MOVEL     'N'           WCAT4             1
388500990126     C* NON GESTISCO IL RITROVAMENTO COLLO
388600990126     C                   MOVEL     'N'           WRITRO
388700990126     C*     GESTISCO LA CREAZIONE ANOMALIE
388800990126     C                   MOVEL     'S'           WCREAN
388900990120     C                   EXSR      CTRDAN
389000990120     C** SE NON TROVATA CA DI COLLO
389100990120     C* VERIFICO SE LA SPEDIZIONE E' SU UNA CA CHIUSA --> CREO
389200990120    3C     WTRDAN        IFEQ      ' '
389300040811      *
389400040811      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
389500040811      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
389600040811      * legate alla spedizione che passo
389700040811     c                   clear                   fidn12ds
389800040811     c                   eval      i12aas = ARTAAS
389900040811     c                   eval      i12lnp = ARTLNP
390000040811     c                   eval      i12nrs = ARTNRS
390100040811     c                   eval      i12nsp = ARTNSP
390200040811     c                   eval      i12fel = 'E'
390300040811     c*
390400040811     c                   call      'FIDN12R'
390500040811     c                   parm                    fidn12ds
390600040811     c*
390700040811     c                   setoff                                       33
390800040811     c*
390900040811     c* se non ci sono errori
391000040811     c                   if        o12err <> ' '
391100040811     c                   seton                                        33
391200040811     c                   else
391300040811     c* se numero di CA trovate maggiore di zero
391400040811     c                   if        o12nca = 0
391500040811     c                   seton                                        33
391600040811     c                   else
391700040811     c                   z-add     1             jj                2 0
391800040811     c                   dow       jj <= o12nca and *in33 = *off
391900040811     C     skDch(jj)     IFGT      0
392000040811     C     BARNPG        IFNE      4
392100040811     C     BARNPG        ANDNE     8
392200040811     C*
392300040811     C* Valorizzo i campi DCT* utilizzati nella routine CRE151 con quelli
392400040811     C* ritornati dal *pgm d utilità FIDN12R
392500040811     C                   movel     skCch(jj)     DCTCCH
392600040811     C*
392700040811     C                   EXSR      CRE151
392800040811     C                   ELSE
392900040811     C* SE E' CAT 4 E 8 LA CHIUDO
393000040811     C                   Z-ADD     151           COMCAA
393100040811     C                   EXSR      CHIUAN
393200040811     C                   ENDIF
393300040811     C**
393400040811     C                   SETON                                        33
393500040811     C**
393600040811     C                   ENDIF
393700040811     c*
393800040811     c                   add       1             jj
393900040811     c                   enddo
394000040811     c*
394100040811     c                   seton                                        33
394200040811     c*
394300040811     c                   endif
394400040811     c                   endif
394500040811     C**
394600990120    3C                   ENDIF
394700990120     C**
394800990120    2C                   ENDIF
394900990120    1C                   ENDIF
395000021023     C*
395100970203     C* CHIUDO LE ANOMALIE DI COLLO MANCATE
395200020204    1C     WAGARR        IFEQ      'S'
395300970203     C* PER COLLO NON LOCALE
395400021023     C     WAGPAR        OREQ      ' '
395500970203     C* PER BOLLA NON TROVATA IN PARTENZA
395600970203     C     WBOPAR        OREQ      'N'
395700021023     C**
395800021023     C* NON AGGIORNO CON SPUNTA ENTRATA
395900021023     C* SE NON E' LOCALE O DISGUIDO IN AREA AGGIORNO SEMPRE
396000021023    2C     WSPLOC        IFEQ      ' '
396100021023     C     WSPLOC        OREQ      'D'
396200021023     C* SE E' LOCALE SOLO SE E' SPUNTA FATTA CON CAT.ARRIVI  DAL P.O.
396300021023     C*  LINEA ARRIVO O TERMINAL ARRIVO
396400021023     C     WSPLOC        OREQ      'A'
396500021023     C     BARNPG        ANDNE     5
396600021023     C     BARNPG        ANDNE     1
396700021023     C*
396800021023     C     WSPLOC        OREQ      'A'
396900021023     C     BARNPG        ANDEQ     1
397000021023     C     WDEFL         ANDEQ     '1'
397100021023     C*
397200021023     C     WSPLOC        OREQ      'T'
397300021023     C     BARNPG        ANDNE     5
397400021023     C     BARNPG        ANDNE     1
397500021023     C*
397600021023     C     WSPLOC        OREQ      'T'
397700021023     C     BARNPG        ANDEQ     1
397800021023     C     WDEFL         ANDEQ     '1'
397900021023     C*
398000970203     C*
398100970203     C* COLLO ARRIVATO MANCANTE --> SOLO SE SPARATO SUL SISTEMA
398200041018     c* L'anomalia 15 non la chiudo se altra spunta presente in cat.arrivi
398300041018     c*  con stessa data
398400041018     c                   eval      wcontrdfv='S'
398500041018     c                   eval      wstess='S'
398600041018     c                   eval      wchiu='S'
398700970203     C                   Z-ADD     15            COMCAA
398800970203     C                   MOVEL     'TR'          SAVCCH
398900970203     C                   EXSR      CHIUAN
399000970203     C*
399100970203     C* COLLO PARTITO NON ARRIVATO
399200970203     C                   MOVEL     'AR'          SAVCCH
399300970203     C                   Z-ADD     5             COMCAA
399400970203     C                   EXSR      CHIUAN
399500970203     C* MANCA COLLO MANCA BOLLA
399600970203     C                   MOVEL     'AR'          SAVCCH
399700970203     C                   Z-ADD     12            COMCAA
399800970203     C                   EXSR      CHIUAN
399900990521    2C                   ENDIF
400000990521    1C                   ENDIF
400100040430     c
400200040430     c* Chiudo fuori posto bolla partenza e fuori posto bolla transito
400300040430     C                   Z-ADD     120           COMCAA
400400040430     C                   EXSR      CHIUAN
400500040430     C                   Z-ADD     125           COMCAA
400600040430     C                   EXSR      CHIUAN
400700970203     C                   ENDSR
400800970204     C**************************************************************************
400900970204     C* ANOMALIA FUORI POSTO SEGNACOLLO BOLLA IN PARTENZA
401000970204     C**************************************************************************
401100970204     C     ANM120        BEGSR
401200970204     C                   Z-ADD     120           COMCAA
401300970204     C*
401400970204     C* VALGONO SOLTANTO LE SPUNTE SPARATE NELL'AREA DI PARTENZA
401500020213    1C  N05BARFLE        IFEQ      BTFP
401600970204     C* SPUNTA DEL TERMINAL DI PARTENZA
401700020213    2C     BARFGS        IFEQ      BTFP
401800970204     C* SE COLLO NON SPUNTATO NELLE CATEGORIE PARTENZA
401900970204    3C     BARNPG        IFNE      5
402000970204     C     BARNPG        ANDNE     1
402100970204     C     BARSPG        ANDNE     'P'
402200970204     C                   EXSR      CRTANM
402300970204   X3C                   ELSE
402400970204     C* SE COLLO     SPUNTATO NELLE CATEGORIE PARTENZA
402500970204     C                   EXSR      CHIUAN
402600970204    3C                   ENDIF
402700970204     C*
402800970204   X2C                   ELSE
402900970204     C* COLLO SPUNTATO DA UN 2 LIVELLO IN PARTENZA
403000970204     C* SE COLLO NON SPUNTATO NELLE CATEGORIE PARTENZA
403100970204    3C     BARNPG        IFNE      5
403200970204     C     BARSPG        ANDNE     'P'
403300970204     C                   EXSR      CRTANM
403400970204     C                   ELSE
403500970204     C* SE COLLO     SPUNTATO NELLE CATEGORIE PARTENZA
403600970204     C                   EXSR      CHIUAN
403700970204    3C                   ENDIF
403800970204    2C                   ENDIF
403900970204    1C                   ENDIF
404000970204     C                   ENDSR
404100970204     C**************************************************************************
404200970204     C* ANOMALIE CHE CREO SOLO SE HA SPARATO LA LINEA DI ARRIVO DEL COL
404300970204     C**************************************************************************
404400970204     C     ANMARR        BEGSR
404500970204     C*
404600970204     C* COLLO/BOLLA ARRIVI PRESENTE
404700970204    1C  N05WBOAR         IFEQ      'S'
404800990126     C                   MOVEL     'N'           WCAT4             1
404900990126     C*     GESTISCO IL RITROVAMENTO COLLO
405000990126     C                   MOVEL     'S'           WRITRO
405100020204     C* NON GESTISCO LA CREAZIONE ANOMALIE CHE GIA' FACCIO IN CHIUAN
405200990126     C                   MOVEL     'N'           WCREAN
405300990126     C                   EXSR      CTRDAN
405400970204     C*
405500970204    2C     WATDCM        IFEQ      0
405600970204     C     BARDFV        ORGT      WATDCM
405700970204     C*
405800970204    3C     WABNDC        IFNE      0
405900970204     C     WATDCM        ANDEQ     0
406000970204     C* BOLLA IN CONSEGNA
406100970204    4C     BARNPG        IFNE      4
406200000523    4C     BARNPG        ANDNE     8
406300970204     C* SE NO SPUNTA ARRIVI CREO ANOMALIA: BOLLA IN CONSEGNA FUORI POST
406400970204    5C     BARNPG        IFNE      2
406500970204     C                   Z-ADD     135           COMCAA
406600970204     C                   EXSR      CRTANM
406700970204    5C                   END
406800970204     C*
406900000607    4C                   ENDIF
407000000607     C**
407100000607    4C     BARNPG        IFEQ      4
407200970204     C*
407300970204     C*        ANOMALIA: "IN CONSEGNA SU ALTRA DISTINTA"
407400970204    5C     WABNDC        IFNE      BARNFV
407500970204     C                   Z-ADD     150           COMCAA
407600970204     C                   EXSR      CRTANM
407700000523     C*
407800970204   X5C                   ELSE
407900970204     C*
408000970204     C* CHIUDO ANOMALIA: "IN CONSEGNA SU ALTRA DISTINTA"
408100970204     C                   Z-ADD     150           COMCAA
408200970204     C                   MOVEL     '  '          SAVCCH            2
408300970204     C                   EXSR      CHIUAN
408400970204    5C                   ENDIF
408500970204    4C                   ENDIF
408600970204    3C                   ENDIF
408700970204     C*
408800970204     C* BOLLA ARRIVI FUORI POSTO
408900161201     c* In caso di wgiac = 'D' non devo dare il fuori posto
409000161201    3C     WGIAC         IFEQ      ' '
409100970204     C*
409200161201    4C     BARSPG        IFEQ      'G'
409300161201    4C     BARSPG        oreq      'Z'
409400970204     C                   Z-ADD     130           COMCAA
409500970204     C                   EXSR      CRTANM
409600970204    4C                   ENDIF
409700161021     c
409800970204     C* IN ENTRATA SOLO SE NON E' BOLLA LOCALE
409900970204    4C     BARNPG        IFEQ      5
410000970204     C     WTIBOL        ANDNE     'L'
410100970204     C     WABNDC        ANDEQ     0
410200970204     C                   Z-ADD     130           COMCAA
410300970204     C                   EXSR      CRTANM
410400970204    4C                   ENDIF
410500970204    3C                   ENDIF
410600970204     C*
410700970204     C* BOLLA GIACENTE FUORI POSTO
410800161201     c* per ora il fuori posto lo devo dare anche per le
410900161201     c*  giacenze a partare a mag giac. mentre con i 2gg in IMA
411000161201     c*  dovrò dare l'anomalia soltanto se già a IMG se spuntato in IMA
411100161201     c*  per le altre categorie errore sempre
411200970204    3C     BARSPG        IFNE      'G'
411300161021    3C     BARSPG        andne     'Z'
411400161201     C**** WGIAC         ANDEQ     'S'
411500161201     C     WGIAC         ANDne     ' '
411600970205     C     BARDFV        ANDGT     GCPDGC
411700970204     C                   Z-ADD     140           COMCAA
411800970204     C                   EXSR      CRTANM
411900970204    3C                   ENDIF
412000970204     C*
412100970204    2C                   ENDIF
412200970204     C**
412300970204     C* C H I U D O
412400970204     C* BOLLA CONSEGNA
412500970204    2C     BARNPG        IFEQ      4
412600970204     C                   Z-ADD     135           COMCAA
412700970204     C                   EXSR      CHIUAN
412800970204    2C                   END
412900970204     C* BOLLA ARRIVI
413000970204    2C     BARNPG        IFEQ      2
413100970204     C     BARNPG        OREQ      4
413200000523     C     BARNPG        OREQ      8
413300970204     C     BARSPG        OREQ      'A'
413400970204     C                   Z-ADD     130           COMCAA
413500970204     C                   EXSR      CHIUAN
413600970204    2C                   END
413700970204     C* BOLLA GIACENZE
413800970204    2C     BARNPG        IFEQ      3
413900970204     C     BARSPG        ANDEQ     'G'
414000161028    2C     BARNPG        orEQ      3
414100161028     C     BARSPG        ANDEQ     'Z'
414200970204     C                   Z-ADD     140           COMCAA
414300970204     C                   EXSR      CHIUAN
414400970204    2C                   END
414500970204     C*
414600970204    1C                   ENDIF
414700970204     C                   ENDSR
414800970218     C**************************************************************************
414900970218     C* SCRITTURA DI FNAGB PER AGGIORNAMENTO  BOLLE
415000970218     C**************************************************************************
415100970217     C     WRTAGB        BEGSR
415200970217     C* LA PRIMA VOLTA NON SCRIVO
415300041105    1C     WAGGF         IFEQ      'S'
415400970217     C     KSAV          CHAIN     FNAGB01L                           34
415500970217     C   34              CLEAR                   FNAGB000
415600970218     C                   MOVEL     SAVTBO        AGBTBO
415700970217     C                   MOVEL     KAGB          AGBAGB
415800970217     C                   MOVEL     SAVAAS        AGBAAS
415900970217     C                   MOVEL     SAVLNP        AGBLNP
416000970217     C                   MOVEL     SAVNRS        AGBNRS
416100970217     C                   MOVEL     SAVNSP        AGBNSP
416200970217     C     WFBS          IFEQ      'S'
416300970217     C                   MOVEL     WFBS          AGBFBS
416400970217     C                   ENDIF
416500970217     C     WFVC          IFEQ      'S'
416600970217     C                   MOVEL     WFVC          AGBFVC
416700970217     C                   ENDIF
416800990728     C     WFPC          IFEQ      'S'
416900990728     C                   MOVEL     WFPC          AGBFPC
417000990728     C                   ENDIF
417100970217     C*
417200981124     C   34              WRITE     FNAGB000                             99
417300970217     C  N34              UPDATE    FNAGB000
417400041105     c
417500041105     c* Se previsto scrivo anche per l'altra bolla
417600041105     c                   eval      wprimotbo=savtbo
417700041105     c
417800041105     c                   if        wagbart='S'
417900041105     c                   eval      savtbo='A'
418000041105     c                   EXSR      ALTROAGB
418100041105     c                   endif
418200041105     c                   eval      savtbo=wprimotbo
418300160421     c
418400160421     c* Scrivo record per la zona
418500160421     c                   if        wznc<>*blanks
418600160421     C                   CLEAR                   FNAGB000
418700160421     C                   MOVEL     SAVTBO        AGBTBO
418800160421     C                   MOVEL     wznc          AGBAGB
418900160421     C                   MOVEL     SAVAAS        AGBAAS
419000160421     C                   MOVEL     SAVLNP        AGBLNP
419100160421     C                   MOVEL     SAVNRS        AGBNRS
419200160421     C                   MOVEL     SAVNSP        AGBNSP
419300160421     C                   WRITE     FNAGB000                             99
419400160421     c                   endif
419500041105     c
419600970217    1C                   ENDIF
419700970218     C*
419800970218     C                   CLEAR                   WFBS
419900970218     C                   CLEAR                   WFVC
420000990728     C                   CLEAR                   WFPC
420100160421     C                   CLEAR                   Wznc
420200041105     C                   CLEAR                   Wagbart
420300970217     C                   ENDSR
420400041105     C**************************************************************************
420500041105     c* scrivo fnagb per l'altro tipo bolla
420600041105     C**************************************************************************
420700041105     c     ALTROAGB      begsr
420800041105     C     KSAV          CHAIN     FNAGB01L                           34
420900041105     c                   eval      agbtbo=savtbo
421000041105     c                   if        *in34
421100041105     c                   clear                   agbfbs
421200041105     C                   WRITE     FNAGB000                             99
421300041105     c                   else
421400041105     c* Se record trovato aggiorno solo i campi del peso/volume
421500041105     C     WFVC          IFEQ      'S'
421600041105     C                   MOVEL     WFVC          AGBFVC
421700041105     C                   ENDIF
421800041105     C     WFPC          IFEQ      'S'
421900041105     C                   MOVEL     WFPC          AGBFPC
422000041105     C                   ENDIF
422100041105     c
422200041105     C                   UPDATE    FNAGB000
422300041105     c                   endif
422400041105     c                   endSR
422500980120     C**************************************************************************
422600980120     C* SCRITTURA DI FNAGB PER AGGIORNAMENTO  BOLLE
422700980120     C**************************************************************************
422800980120     C     WR1AGB        BEGSR
422900110103      *
423000030528     c                   setoff                                       45
423100110103      *
423200980120     C     KEAGB         CHAIN     FNAGB01L                           34
423300980120     C   34              CLEAR                   FNAGB000
423400980120     C                   MOVEL     WTBO          AGBTBO
423500980120     C                   MOVEL     KAGB          AGBAGB
423600980120     C                   MOVEL     WDAAS         AGBAAS
423700980120     C                   MOVEL     WDLNP         AGBLNP
423800980120     C                   MOVEL     WDNRS         AGBNRS
423900980120     C                   MOVEL     WDNSP         AGBNSP
424000160726     c*
424100160726     c* Se richiesto, passo flag di rielaborazione statistiche partenze
424200160726     c                   if        welasta_p='S'
424300160726     c                   clear                   dagbflo
424400160726     c                   eval      §agbdsf='S'
424500160726     c                   eval      agbflo=dagbflo
424600160726     c                   endif
424700980120     C*
424800030528     C   34              WRITE     FNAGB000                             45
424900980120     C  N34              UPDATE    FNAGB000
425000980120     C*
425100160726     c                   clear                   welasta_p
425200160726     C*
425300980120     C                   ENDSR
425400970609     C**************************************************************************
425500970609     C* SCRITTURA DI FNSTA PER RIELABORAZIONE STATISTICA ARRIVI
425600970609     C**************************************************************************
425700970609     C     WRTSTA        BEGSR
425800970609     C* LA PRIMA VOLTA NON SCRIVO
425900970609    1C     WARDAM        IFGT      0
426000970609     C* VECCHIA DATA
426100970609     C     WSVDAM        IFGT      0
426200970609     C                   CLEAR                   SA2TLA
426300070221     C     dls45FLG      IFEQ      'C'
426400970609     C                   MOVEL     'L'           SA2TLA
426500970609     C                   END
426600970609     C                   MOVEL     'A'           SA2TRC
426700970609     C                   Z-ADD     WSVDAM        SA2DTS
426800970609     C                   MOVE      *ZEROS        SA2LNA
426900970609     C                   CALL      'FNLSA2R3'
427000970609     C                   PARM                    SA2TLA            1
427100970609     C                   PARM                    SA2TRC            1
427200970609     C                   PARM                    SA2DTS            8 0
427300970609     C                   PARM                    SA2LNA            3 0
427400970609     C                   END
427500970609     C* NUOVA DATA
427600970609     C                   Z-ADD     WARDAM        SA2DTS
427700970624     C                   MOVEL     'A'           SA2TRC
427800970609     C                   CALL      'FNLSA2R3'
427900970609     C                   PARM                    SA2TLA            1
428000970609     C                   PARM                    SA2TRC            1
428100970609     C                   PARM                    SA2DTS            8 0
428200970609     C                   PARM                    SA2LNA            3 0
428300970609    1C                   ENDIF
428400970609     C*
428500970609     C                   ENDSR
428600980928     C**************************************************************************
428700980928     C* MEMORIZZO I FOGLI NON TROVATI DA INVIARE A SEDE CON MSG
428800980928     C**************************************************************************
428900111018     C**   INVMSG        BEGSR
429000980928     C*
429100111018     C**                 Z-ADD     1             EE                3 0
429200111018     C**                 SETON                                        95
429300111018     C**                 CLEAR                   WPRES
429400111018     C**                 MOVE      WNFV          WNFVA             5
429500111018    4C**   *IN95         DOWEQ     *ON
429600111018     C**   WPRES         ANDEQ     ' '
429700980928     C* IL FOGLIO CORRISPONDE: NON LO RIMEMORIZZO
429800111018     C**   WNFVA         LOOKUP    ERF(EE)                                95
429900111018    5C** 95WNPG          IFEQ      ERC(EE)
430000111018     C**   WFGS          ANDEQ     ERE(EE)
430100111018     C**   WNPS          ANDEQ     ERP(EE)
430200111018     C**                 MOVEL     'S'           WPRES             1
430300111018    5C**                 ENDIF
430400111018     C**                 ADD       1             EE
430500111018    4C**                 ENDDO
430600980928     C* SE NON E' PRESENTE RIMEMORIZZO
430700111018    4C**   WPRES         IFEQ      ' '
430800111018     C**                 Z-ADD     1             EE
430900111018     C**   '     '       LOOKUP    ERF(EE)                                95
431000111018     C** 95              MOVE      WNFVA         ERF(EE)
431100111018     C** 95              Z-ADD     WNPG          ERC(EE)
431200111018     C** 95              Z-ADD     WFGS          ERE(EE)
431300111018     C** 95              movel     Wnps          ERP(EE)
431400111018    5C**   EE            IFEQ      8
431500111018     C**                 EXSR      TRASMI
431600111018    5C**                 ENDIF
431700111018    4C**                 ENDIF
431800111018     C*+                 ENDSR
431900990120     C**************************************************************************
432000990120     C* SE DEVO CREO ANOMALIA 152 / 153
432100990120     C**************************************************************************
432200990120     C     CRT15X        BEGSR
432300990120     C** SE LA C.A. E' APERTA, VERIFICO SE AVARIA E RESO O DISTRUTTA
432400990120    1C     DCTDSD        IFEQ      'R'
432500990120     C     DCTDSD        OREQ      'D'
432600990126     C**
432700990126     C                   EXSR      CERTAD
432800990120     C* CONTROLLO SE TIPO ANOMALIA E' AVARIA
432900990120     C** CREO ANOMALIA 152 0 153
433000990120    2C     §TARGR        IFEQ      'V'
433100990120     C*
433200990120     C     DCTDSD        IFEQ      'R'
433300990120     C                   Z-ADD     152           COMCAA
433400990120     C                   ELSE
433500990120     C                   Z-ADD     153           COMCAA
433600990120     C                   ENDIF
433700990120     C**
433800990120     C                   EXSR      CRTANM
433900990120    2C                   ENDIF
434000990120    1C                   ENDIF
434100990120     C                   ENDSR
434200990120     C**************************************************************************
434300990120     C* CONTROLLO SE PRESENTE CA PER IL COLLO
434400990120     C**************************************************************************
434500990120     C     CTRDAN        BEGSR
434600990126     C*
434700990120     C                   CLEAR                   WTRDAN
434800990120     C     KBAR          CHAIN     FNDCD02L                           33
434900990126     C*
435000990120    1C     *IN33         IFEQ      *OFF
435100990120     C     DCDATB        ANDEQ     ' '
435200990120     C     KDCT          CHAIN     FNDCT01L                           33
435300050223      * se dctdt2 > 0 la c.a. non è valida x la filiale
435400050224     c                   If        Not *In33 and dctdt2 > 0
435500050224     c                   Eval      *In33 = *On
435600050224     c                   EndIf
435700050224     C
435800990120    2C     *IN33         IFEQ      *OFF
435900990120     C     DCTATB        ANDEQ     ' '
436000990126     C* TROVATA C.A. NON ANNULLATA
436100990120     C                   MOVEL     'S'           WTRDAN            1
436200990126     C**
436300990126     C* CONTROLLO SE DEVO IMPOSTARE FLAG COLLO RITROVATO
436400990126    3C     WRITRO        IFEQ      'S'
436500990126     C                   EXSR      CERTAD
436600990126    4C     §TARIT        IFEQ      'S'
436700990126     C     DCDDFV        ANDEQ     0
436800990126     C                   Z-ADD     BARDFV        DCDDFV
436900990126     C                   Z-ADD     BARFGS        DCDFGS
437000990126     C                   Z-ADD     BARNPS        DCDNPS
437100990126     C                   MOVEL     'S'           DCDFAR
437200990126     C                   UPDATE    FNDCD000
437300990126    4C                   ENDIF
437400990126    3C                   ENDIF
437500990126     C**
437600990126     C                   UNLOCK    FNDCD02L
437700990126     C**
437800990126     C** CONTROLLO SE DEVO CREARE LE ANOMALIE
437900990126    3C     WCREAN        IFEQ      'S'
438000990126     C**
438100990126     C* ANOMALIA 151 - COLLO DI CA CHIUSA
438200990126    4C     DCTDCH        IFGT      0
438300990121     C* IN ARRIVO NON LA CREO PER SPUNTA DI CATEGORIA 4
438400990126    5C     WCAT4         IFEQ      'N'
438500990121     C     BARNPG        ANDNE     4
438600000522     C     BARNPG        ANDNE     8
438700990121     C     WCAT4         OREQ      'S'
438800990120     C                   EXSR      CRE151
438900990126   X5C                   ELSE
439000990121     C                   Z-ADD     151           COMCAA
439100990121     C                   EXSR      CHIUAN
439200990126    5C                   ENDIF
439300990126   X4C                   ELSE
439400990120     C* ANOMALIE 152 O 153
439500990126    5C     DCDDCH        IFEQ      0
439600990120     C                   EXSR      CRT15X
439700990126    5C                   ENDIF
439800990126    4C                   ENDIF
439900990121    3C                   ENDIF
440000990120    2C                   ENDIF
440100990120     C**
440200990120   X1C                   ELSE
440300990120     C* ANNULLATO COME SE NON CI FOSSE
440400990120     C                   SETON                                        33
440500990120    1C                   ENDIF
440600990120     C                   ENDSR
440700990120     C**************************************************************************
440800990120     C* CREO ANOMALIA 151 SE LO PREVEDE LA CAUSALE CHIUSURA
440900990120     C**************************************************************************
441000990120     C     CRE151        BEGSR
441100990120     C     DCTCCH        IFNE      *BLANKS
441200990120     C                   CLEAR                   DSBS02
441300990120     C                   MOVEL     'C'           T02MOD
441400990120     C                   MOVEL     KNSIF         T02SIF
441500990121     C                   MOVEL     'CCH'         T02COD
441600990120     C                   MOVEL     DCTCCH        T02KE1
441700990120     C                   CALL      'TIBS02R'
441800990120     C                   PARM                    KPJBA
441900990120     C                   PARM                    DSBS02
442000990121    2C     T02ERR        IFEQ      ' '
442100990121     C                   MOVEL     T02UNI        DCCH
442200990121     C                   ELSE
442300990121     C                   MOVEL     'S'           §CHCRA
442400990121    2C                   ENDIF
442500990120     C                   ENDIF
442600990120     C**
442700990120     C* CREO
442800990120     C     DCTCCH        IFEQ      *BLANKS
442900990120     C     §CHCRA        OREQ      'S'
443000990120     C                   Z-ADD     151           COMCAA
443100990120     C                   EXSR      CRTANM
443200990120     C                   ENDIF
443300990120     C                   ENDSR
443400990126     C**************************************************************************
443500990126     C* CERCO TABELLA RECORD TIPO ANOMALIA TAD
443600990126     C**************************************************************************
443700990126     C     CERTAD        BEGSR
443800990126    1C     DCTTAD        IFNE      SAVTAD
443900990126     C                   CLEAR                   DSBS02
444000990126     C                   MOVEL     'C'           T02MOD
444100990126     C                   MOVEL     KNSIF         T02SIF
444200990126     C                   MOVEL     'TAD'         T02COD
444300990126     C                   MOVEL     DCTTAD        T02KE1
444400990126     C                   CALL      'TIBS02R'
444500990126     C                   PARM                    KPJBA
444600990126     C                   PARM                    DSBS02
444700990126     C**
444800990126    2C     T02ERR        IFEQ      ' '
444900990126     C                   MOVEL     T02UNI        DTAD
445000990126     C                   ELSE
445100990126     C                   MOVEL     'V'           §TARGR
445200990126     C                   MOVEL     'S'           §TARIT
445300990126    2C                   ENDIF
445400990126     C                   MOVEL     DCTTAD        SAVTAD
445500990126    1C                   ENDIF
445600990126     C                   ENDSR
445700030723     C**************************************************************************
445800030723     C* CERCO TABELLA VARIABILI PGM PARTENZE
445900030723     C**************************************************************************
446000030723     C     CERVPO        BEGSR
446100030723      *
446200030723      * CHAIN A VARIABILI PARTENZE PER PRENDERE MAX PESO E VOLUME CARICABILI
446300030723     C                   CLEAR                   DVPO
446400030723     C                   CLEAR                   DSBS02
446500030723     C                   MOVEL     'C'           T02MOD
446600030723     C                   MOVEL     KNSIF         T02SIF
446700030723     C                   MOVEL     'VPO'         T02COD
446800030723     C                   MOVEL     'VPO'         T02KE1
446900030723     C                   CALL      'TIBS02R'
447000030723     C                   PARM                    KPJBA
447100030723     C                   PARM                    DSBS02
447200030723     C**
447300030723    2C     T02ERR        IFEQ      ' '
447400030725     C                   MOVEL     T02UNI        DVPO
447500030723    2C                   ENDIF
447600110103      *
447700030723     C                   ENDSR
447800000927     C**************************************************************************
447900000927     C* CONTROLLO SE POSSO CREARE ANOMALIE DI MANCA RECOD SECONDO LA
448000000927     C*  ATA FOGLIO E I GIONRI DI PULIZIA
448100000927     C**************************************************************************
448200000927     C     CTRDAT        BEGSR
448300000927     C*
448400000927     C* A CAMBIO DATA RICONTROLLO
448500000927    1C     BARDFV        IFNE      SA2DFV
448600000927     C                   CLEAR                   WCRE10
448700000927     C*
448800000927     C* SE FOGLIO UGUALE A UDATE, VA SICURAMENTE BENE
448900000927    2C     BARDFV        IFEQ      WDATE1
449000000927     C                   MOVEL     'S'           WCRE10            1
449100000927   X2C                   ELSE
449200000927     C* CONTROLLO CON I GG DI PULIZIA
449300000927      * DATA FOGLIO MAGGIORE DATA ULTIMO UTILIZZO: POSSO CREARE ANOM
449400000927    3C     BARDFV        IFGE      PRIDAT
449500000927     C                   MOVEL     'S'           WCRE10            1
449600000927    3C                   ENDIF
449700000927    2C                   ENDIF
449800000927     C*
449900000927     C                   Z-ADD     BARDFV        SA2DFV
450000000927    1C                   ENDIF
450100130301     c*
450200130301     c* Se è da creare --> controllo se si tratta di fil che fa partire colli
450300130301     c*  di altro TFP e non la creo lo stesso
450400130301     c                   clear                   wlocalxco         1
450500130301     c
450600130301     c                   if        wcre10='S' and ( bolxco='W'  or
450700130301     c                             (wesbtt='N' and wesblt='N' and wesart='N'
450800130301     c                              and barnrs>0))
450900130301     c* se si tratta di spunta partenza --> creo la 57
451000130301     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
451100130301     c                             or barnpg=1
451200130301     c                   exsr      CtrXCO
451300130301     c                   if        Indx>0
451400130301     c                   eval      Wlocalxco='S'
451500130301     c                   endif
451600130301     c                   endif
451700130301     c                   endif
451800000927     C*
451900010607     C                   ENDSR
452000010607     C**************************************************************************
452100070131     C* MEMORIZZAZIONE SPUNTE DI CONSEGNA DOPPIE SU FNBRVE0F
452200010607     C**************************************************************************
452300010607     C     SRBRVE        BEGSR
452400070710     c* Se devo tenere una spuynta solo memeorizzo per categoria
452500070710     c*  e segnacollo
452600070710     c                   if        nst(WA)='1'
452700070710     c
452800070131     C     KBRVE2        SETLL     FNBRVE2L                               33
452900010607     C     *IN33         IFEQ      *OFF
453000010607     C* SPUNTA NON ESISTENTE: MEMORIZZO ANCHE LA VECCHIA
453100070131     C                   WRITE     FNBRVE
453200010607     C                   ENDIF
453300070131     C* Salvo campi di FNBRV che potrebbero rimanere invariati
453400070131     C* Li dovrò reimpostare dopo la scrittura di FNBRVe per poi
453500010608     C* fare i giusti aggiornamenti
453600010608     C                   MOVE      BRVATR        SAWATR
453700080317     C                   MOVE      BRVTSU        SAWTSU
453800080317     c                   movel     brv           sawbrv
453900010608     C* IN OGNI CASO MEMORIZZO LA SPUNTA NUOVA
454000010608     C                   MOVEL     'S'           WBRVE             1
454100010608     C                   EXSR      RIEMPI
454200010608     C                   CLEAR                   WBRVE
454300070131     C                   WRITE     FNBRVE
454400070131     C* Reimposto campi di FNBRV come da lettura iniziale
454500010608     C                   MOVE      SAWATR        BRVATR
454600080317     C                   MOVE      SAWtsu        BRVtsu
454700080317     C                   MOVE      SAWbrv        BRV
454800110103     c*
454900070710     c                   else
455000070710     c* Se devo memorizzare le spunte senza anomalia controllo per foglio
455100070710     C     KBRVE1        SETLL     FNBRVE1L                               33
455200070710     C     *IN33         IFEQ      *OFF
455300070710     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
455400070710     C                   WRITE     FNBRVE
455500070710     C                   ENDIF
455600070710     C                   ENDIF
455700070710     c*
455800010607     C                   ENDSR
455900020124     C**************************************************************************
456000020204     C* CERCO TERMINAL PARTENZA LNP COLLO
456100020124     C**************************************************************************
456200020204     C     TERPAR        BEGSR
456300020124     C                   CLEAR                   DSLV55
456400020124     C                   MOVEL     'P'           D55TPT
456500020124     C                   MOVEL     LNPB          D55LIN
456600020124     C                   MOVEL     BARDFV        D55DRF
456700020213     C                   EXSR      FNLV55
456800020204     C                   MOVE      D55TFP        LNPTFP
456900020204     C**
457000020204     C                   ENDSR
457100020204     C**************************************************************************
457200020204     C* CERCO TERMINAL ARRIVO LNA COLLO
457300020204     C**************************************************************************
457400020204     C     TERARR        BEGSR
457500020124     C**
457600020124     C* LINEA DI ARRIVO
457700020124     C     BARLNA        IFNE      SVVLNA
457800020124     C     LNPB          ORNE      SVVLNP
457900020204     C     BARDFV        ORNE      SVVDFV
458000020124     C*
458100020124     C* TEMINAL DI ARRIVO
458200020124     C                   CLEAR                   DSLV55
458300020124     C                   MOVEL     ' '           D55TPT
458400020124     C                   MOVEL     BARDFV        D55DRF
458500020124     C                   MOVEL     BARLNA        D55LIN
458600020124     C                   MOVEL     LNPB          D55LNP
458700020213     C                   EXSR      FNLV55
458800020124     C**
458900020213     C                   MOVEL     D55TFA        LNATFA
459000020124     C*
459100020124     C                   MOVEL     BARLNA        SVVLNA
459200020204     C                   MOVEL     LNPB          SVVLNP
459300020204     C                   MOVEL     BARDFV        SVVDFV
459400020124     C                   ENDIF
459500020124     C                   ENDSR
459600020124     C**************************************************************************
459700020124     C* AGGIORNO LE BOLLE PARTENZA
459800020124     C**************************************************************************
459900020124     C     AGGBLT        BEGSR
460000060223     C                   CLEAR                   WEND              1
460100020124     C* NON FACCIO AGGIORNAMENTI SE SPUNTA DI COLLO DISGUIDATO
460200070221    1C     dls45FLG      IFNE      '6'
460300020124     C*
460400020124     C                   MOVEL     'P'           WTIBOL            1
460500020124     C                   MOVEL     'S'           WBOPAR
460600020124     C*
460700020124     C* Se collo già consegnato e spunta partenza di pistola manuale
460800020124     C* ignoro la spunta
460900020124    3C     BARNPG        IFEQ      1
461000020124     C     BLTDCM        ANDNE     *ZEROS
461100020124     C     RPS(Y)        ANDEQ     'M'
461200020124     C                   MOVE      'N'           OKAGG
461300020201     C                   MOVEL     '1'           WEND
461400020204     C                   UNLOCK    FNBLT27L
461500020201     C                   GOTO      ENDBLT
461600020124    3C                   END
461700020204     C*  SE NON IMPOSTATO VOLUME E PESO DA PARTENZA
461800020204     C     WWFVC         IFEQ      ' '
461900020124     C                   MOVEL     'P'           WWFVC
462000020204     C                   ENDIF
462100020124     C*
462200020124     C                   MOVEL     'P'           BLTTBO
462300020124     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
462400020124     C* NE PRECEDENTE
462500020124     C     FLGPRE        IFNE      'A'
462600020124     C     BLTSPE        ANDNE     SAVSPE
462700020124     C     WAGGF         IFEQ      'S'
462800020124     C                   EXSR      WRTAGB
462900020124     C                   CLEAR                   WAGGF
463000020124     C                   ENDIF
463100020124     C                   MOVEL     BLTSPE        SAVSPE
463200020124     C                   ENDIF
463300020124     C*
463400020124    3C     BARCAN        IFNE      ' '
463500020124     C* NON MEMORIZZO ANOMALIA "E" SU DETTAGLIO COLLO PERCHE' LA NON
463600020124     C* INDICA UNA VERA E PROPRIA ANOMALIA MA SOLO UN ERRORE PER LINEA
463700020124     C* MANCANTE SUL FOGLIO AL MOMENTO DELLA SPUNTA DEL COLLO
463800020124     C     BARCAN        ANDNE     'E'
463900020124     C                   MOVEL     BARCAN        BLTCAN
464000020124     C                   MOVEL     'S'           WAGGF             1
464100020124    3C                   END
464200160421     c
464300160421     c* Se zona bolla <> zona spunta --> riaggiorno zona bolla
464400160422     c                   if        bolznc<>'  ' and
464500160429     c                             bolznc<>%editc(barznc:'X')  and
464600160503     c                             barnpg<>8
464700160503     c                   if        tps(Y)= 'L' or tps(y)='C'
464800160421     c                   movel     barznc        wznc
464900160421     C                   MOVEL     'S'           WAGGF             1
465000160421     c                   endif
465100160503     c                   endif
465200020124     C*
465300041105     c                   clear                   wagvolart         1
465400041105     c                   clear                   wagpesart         1
465500041105     c
465600020124    3C     BARVUC        IFGT      0
465700020124     C* AGGIORNO IL VOLUME CML SOLO SE IL VOLUME SUL COLLO:
465800020124     C* - E' = 0
465900020124     C* - E' DELL'ARRIVO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
466000020124     C**- E' DEL TRANSITO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
466100020124    4C     BLTVUC        IFEQ      0
466200020124     C     BLTVUC        ORNE      0
466300020214     C     BLTFVC        ANDNE     'P'
466400020124     C     WWFVC         ANDEQ     'P'
466500041013     C                   MOVEL     'S'           WFVC
466600020124     C                   Z-ADD     BARVUC        BLTVUC
466700041105     c                   eval      wagvolart='S'
466800020124     C                   MOVEL     'S'           WAGGF             1
466900020124     C                   MOVEL     WWFVC         BLTFVC
467000020124    4C                   END
467100020124     C*
467200020124    3C                   END
467300020124     C*STESSO GIRO DEL VOLUME ANCHE PER IL PESO
467400020124    3C     BARPUC        IFGT      0
467500020124     C* AGGIORNO IL VOLUME CML SOLO SE IL VOLUME SUL COLLO:
467600020124     C* - E' = 0
467700020124     C* - E' DELL'ARRIVO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
467800020124     C**- E' DEL TRANSITO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
467900020124    4C     BLTPUC        IFEQ      0
468000020124     C     BLTPUC        ORNE      0
468100020214     C     BLTFPC        ANDNE     'P'
468200020124     C     WWFVC         ANDEQ     'P'
468300041012     c                   movel     'S'           wfpc
468400020124     C                   Z-ADD     BARPUC        BLTPUC
468500041105     c                   eval      wagpesart='S'
468600020124     C                   MOVEL     'S'           WAGGF             1
468700020124     C                   MOVEL     WWFVC         BLTFPC
468800020124    4C                   END
468900020124     C*
469000020124    3C                   END
469100020124     C*
469200020124     C* SPUNTA LOCALE IN DISGUIDO --> AGGIORNO LA DATA ENTRATA TRANSITO
469300020124    3C     WSPLOC        IFEQ      'D'
469400020124     C                   MOVEL     BARDFV        BLTDET
469500020124     C                   MOVEL     BARFGS        BLTFLP
469600020124     C                   CLEAR                   BLTDUT
469700020124     C                   MOVEL     'S'           WAGGF             1
469800020124     C*
469900020124   X3C                   ELSE
470000020124     C*
470100020124     C* SE NON C'E' AGGIUNGO DATA E PISTOLA
470200020124     C* SOLO SE NON E' SPUNTA DISGUIDO IN AREA
470300020124    4C     BLTDSE        IFEQ      0
470400020124     C                   MOVEL     BARDFV        BLTDSE
470500020124     C                   MOVEL     BARNPS        BLTNPE
470600020124     C                   MOVEL     'S'           WAGGF             1
470700020124   X4C                   ELSE
470800020124     C*
470900020124     C* SE E' QUESTA LA SPUNTA CHE HA AGGIORNATO IL COLLO RIAGGIORNO
471000020124    5C   05BLTNPE        IFEQ      BARNPS
471100020124     C                   MOVEL     BARDFV        BLTDSE
471200020124     C                   MOVEL     'S'           WAGGF             1
471300020124    5C                   ENDIF
471400020124     C*
471500020124     C* RIAGGIRNO COLLO SE PRECEDENTE SPUNTA NON VALIDA E ATTUALE VALID
471600020124    5C  N05BLTNPE        IFNE      BARNPS
471700020124     C     RPS(Y)        ANDEQ     'L'
471800020124     C                   MOVEL     BLTNPE        ALF2
471900020124     C*
472000020124     C* CERCO RAGGRUPPAMENTO PISTOLA
472100020124     C                   EXSR      RICRAG
472200020124     C*
472300020124    6C     RPS(Z)        IFEQ      'M'
472400020124     C     RPS(Z)        OREQ      'A'
472500020124     C                   MOVEL     BARDFV        BLTDSE
472600020124     C                   MOVEL     BARNPS        BLTNPE
472700020124     C                   MOVEL     'S'           WAGGF             1
472800020124    6C                   END
472900020124    5C                   END
473000020124     C*
473100020124    4C                   END
473200020124    3C                   END
473300020124     C*
473400041105     C*  Se FLG3 = A non devo aggiornare fnblt perchè richiamo solo per
473500041105     c*  gli arrivi
473600070208    3C     dls45flg2     IFNE      'A'
473700020124     C                   UPDATE    FNBLT000
473800041105     c* Se aggiornato peso e volume e collo partito, aggiorno anche ART
473900041105     c                   if        bltdfv>0 and
474000160421     c                             (wagvolart='S' or wagpesaRT='S')
474100041105     C     KBAR          CHAIN     FNART27L                           3434
474200041105     C                   IF        NOT *IN34
474300041105     c* VOLUME
474400041105     C                   if        wagvolart='S'
474500041105     c* Se lan fedex non aggiorno
474600041105     c                   if        artvuc=0 or ntwbar<>'FED'
474700041105     c                   eval      wagbart='S'
474800041105     c                   eval      artvuc=bltvuc
474900041105     c                   eval      artfvc=bltfvc
475000041105     c                   endif
475100041105     c                   endif
475200041105     C                   if        wagpesart='S'
475300041105     c                   eval      wagbart='S'
475400041105     c                   eval      artpuc=bltpuc
475500041105     c                   eval      artfpc=bltfpc
475600041105     c                   endif
475700160421     c
475800041105     c                   update    fnart000
475900041105     C                   ENDIF
476000041105     C                   ENDIF
476100041105     C
476200020124     C**
476300020124     C* SE RICHIAMATO AGGIORNO SUBITO LA BOLLA
476400110103    5c   02              if        wfpc='S' or wfvc='S'
476500160421     c                             or wznc<>*blanks
476600020124     C                   SETOFF                                       3489
476700020124     C     KBLT          CHAIN     FNBLP01L                           3489
476800041021     c                   if        not *in34 and not *in89
476900041012     c                   if        wfvc='S'
477000041012     C                   MOVEL     99999         BLPNCR
477100041012     c                   endif
477200041012     c                   if        wfpc='S'
477300041012     C                   MOVEL     99999         BLPNCp
477400041012     c                   endif
477500160421     c                   if        wznc<>*blanks
477600160421     c                   movel     wznc          blpznc
477700160421     c                   endif
477800041012     c
477900041012     C                   UPDATE    FNBLP000
478000050223     c
478100050223     c* Aggiorno subito anche la bolla arrivo
478200050224     c                   if        wagbart='S'
478300050223     C     KBLT          CHAIN     FNarb01L                           3489
478400050223     C                   MOVEL     'E'           arbfbs
478500160421     c                   if        wznc<>*blanks
478600160421     c                   movel     wznc          arbznc
478700160421     c                   endif
478800050224     c  n89
478900050224     cann34              update    fnarb000
479000050224     c                   clear                   wagbart
479100050224     c                   endif
479200041012     c
479300041012     c                   clear                   wfpc
479400041012     c                   clear                   wfvc
479500160421     c                   clear                   wznc
479600041012     c                   endif
479700020124    5C                   ENDIF
479800020124     C**
479900020124   X3C                   ELSE
480000020124     C                   UNLOCK    FNBLT27L
480100020124    3C                   ENDIF
480200020124     C*
480300020213     C*CHIUDO EVENTUALI ANOMALIE DI DISGUIDO,SE COLLO NON DEL TFP
480400020124     C     BARNRS        IFGT      *ZEROS
480500020214     C*
480600020213     C                   CLEAR                   DSLV55
480700020213     C                   MOVEL     'P'           D55TPT
480800020213     C                   Z-ADD     BARLNP        D55LIN
480900020214     C                   Z-ADD     DSPB          D55DRF
481000020213     C                   EXSR      FNLV55
481100020131     C**
481200020213     C     D55TFP        IFNE      LNPTFP
481300020124     C                   MOVEL     'S'           SAVMAC
481400020124     C                   Z-ADD     200           COMCAA
481500020124     C                   EXSR      CHIUAN
481600020124     C*
481700020124     C                   Z-ADD     55            COMCAA
481800020124     C                   EXSR      CHIUAN
481900020124     C*
482000020124     C                   Z-ADD     56            COMCAA
482100020124     C                   EXSR      CHIUAN
482200020131     C                   ENDIF
482300020124     C*
482400020124     C                   CLEAR                   SAVMAC
482500020124     C                   END
482600020124     C*
482700020124   X1C                   ELSE
482800020124     C                   UNLOCK    FNBLT27L
482900020124    1C                   ENDIF
483000020124     C**
483100020201     C     ENDBLT        ENDSR
483200020131     C**************************************************************************
483300020131     C* AGGIORNO LE BOLLE ARRIVO
483400020131     C**************************************************************************
483500020131     C     AGGART        BEGSR
483600020201     C                   CLEAR                   WEND
483700020131     C*
483800020131     C     WTIBOL        IFEQ      'P'
483900020131     C                   MOVEL     'L'           WTIBOL
484000020131     C                   ELSE
484100020131     C                   MOVEL     'A'           WTIBOL
484200020131     C                   ENDIF
484300020131     C*
484400020131     C* E S I S T E   B O L L A   A R R I V O
484500020131    2C     WESART        IFEQ      'S'
484600020131     C* SE FLG=6 SVINCOLO RECORD
484700070221     C     dls45FLG      IFEQ      '6'
484800020131     C                   UNLOCK    FNART27L
484900020131     C                   ENDIF
485000020131     C**
485100020131     C     BARNPS        IFEQ      89
485200020131     C                   UNLOCK    FNART27L
485300020131     C                   ELSE
485400020214     C     ARTFLP        IFEQ      0
485500020214     C                   Z-ADD     ARTDFV        COMDFV
485600020214     C                   ELSE
485700020214     C                   Z-ADD     ARTDUT        COMDFV
485800020214     C                   ENDIF
485900020131     C                   MOVE      ARTDCM        WATDCM
486000020131     C                   MOVE      ARTDAM        SAVDAM
486100020131     C                   MOVEL     'S'           WBOAR             1
486200020131     C*
486300020131     C* SE FLG=6 CREO ANOMALIA 61
486400160624     c* Anomalia 61 eliminata
486500160624     C***  dls45FLG      IFEQ      '6'
486600160624     C***                EXSR      CRTA60
486700160624     C***                MOVEL     '1'           WEND
486800160624     C***                GOTO      ENDART
486900160624     C***                ENDIF
487000020131     C*
487100020131     C     WWFVC         IFEQ      *BLANKS
487200020131     C                   MOVEL     'A'           WWFVC
487300020131     C                   END
487400020131     C*
487500020131     C* LA PRIMA VOLTA MEMORIZZO LA SPEDIZIONE
487600020131     C                   MOVEL     'A'           ARTTBO
487700020131     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
487800020131     C* NE PRECEDENTE
487900020131     C     FLGPRE        IFNE      'A'
488000020131     C     ARTSPE        ANDNE     SAVSPE
488100020131     C     WAGGF         IFEQ      'S'
488200020131     C                   EXSR      WRTAGB
488300020131     C                   CLEAR                   WAGGF
488400020131     C                   ENDIF
488500020131     C                   MOVEL     ARTSPE        SAVSPE
488600020131     C                   ENDIF
488700160422     c* zona bolla
488800160422     c                   if        bolznc<>'  '   and
488900160429     c                             bolznc<>%editc(barznc:'X') and
489000160503     c                             barnpg<>8
489100160503     c                   if        tps(Y)= 'L' or tps(y)='C'
489200160422     c                   movel     barznc        wznc
489300160422     C                   MOVEL     'S'           WAGGF             1
489400160422     c                   endif
489500160503     c                   endif
489600020131     C*
489700020131     C* CODICE ANOMALIA
489800041012     c                   exsr      AGGCAN
489900020131     C*
490000020131     C* VOLUME DA MACCHINA SMISTACOLLI
490100041012     C                   EXSR      AGGVOL
490200030604     C*
490300020131     C* PESO DA MACCHINA SMISTACOLLI
490400041012     C                   EXSR      AGGpes
490500110103     c*
490600110103     c*
490700091209     c* Se chi spara è la linea di arrivo del collo o il suo gestore
490800091209     c* --> aggiorno la data più bassa di arrivo 1° collo
490900091211     C* CARICO LA L6 DELLA FILIALE GESTIONE
491000091211     C     BPES          IFNE      SAVPES
491100091211     C                   MOVEL     BPES          SAVPES
491200091211     C                   EXSR      CARL6
491300091211     C                   ENDIF
491400110103      *
491500091211     C     BARLNA        LOOKUP    L6                                     88
491600150615     c*
491700150817     c* non aggiorno più data e ora arrivo alla filiale finale di arrivo
491800150817     c*  er l'estero perchè creeremo apposito campo per
491900150817     c*  indicare la data di arrivo all'hub/terminal di arrivo
492000150622     c***                if        not *in88 and  boltfa=BPES  and
492100150622     c***                          (ntwbar='EEX' or ntwbar='DPD' or
492200150622     c***                          ntwbar='FED')
492300150622     c***                seton                                        88
492400150622     c***                endif
492500110103      *
492600150817      * tolto test su data spedizione dal 2009/12 in poi
492700150817    3c                   if        *in88 or artlna=bpes
492800091211     c     kar52         chain     fiar501l
492900091209    4c                   if        %found(fiar501l)
493000091209     c                   movel     ar5uni        dar5gen
493100091209     c                   else
493200091209     c                   clear                   dar5gen
493300091209    4c                   endif
493400110103     c*
493500091209     c                   clear                   warrdt
493600091209     c                   clear                   warrhm
493700091209    4c                   if        §ar5arrdt>*zeros
493800091209     c                   movel     §ar5arrdt     warrdt            8 0
493900091209    5c                   if        §ar5arrhm>*zeros
494000091209     c                   movel     §ar5arrhm     warrhm            4 0
494100091209    5c                   endif
494200091209    4c                   endif
494300110103     c*
494400101129     c* Uso data e ora immissione se non superiore all'ora caricamento
494500150817     c                   if        (bardfs<brvdcs) or
494600150817     c                             (bardfs=brvdcs and barhms<brvhcs)
494700101130     c                   movel     bardfs        datReale
494800101130     c                   movel     barhms        oraReale
494900101129     c                   else
495000101129     c                   movel     brvdcs        datReale
495100101129     c                   movel     brvhcs        oraReale
495200101129     c                   endif
495300110103     c*
495400091211     c                   if         warrdt=0  or
495500101129    4c                             (warrdt>0 and warrdt>datReale) or
495600101129     c                             (warrdt=datReale and warrhm>oraReale)
495700091209     c                   exsr      AggioFIAR5
495800110103     c*
495900091209   x4c                   else
496000091209     c                   unlock    fiar501l
496100091209    4c                   endif
496200091209    3c                   endif
496300091209      **
496400020131    3C     ARTDAM        IFEQ      0
496500020131     C                   MOVEL     BARDFV        ARTDAM
496600020131     C                   MOVEL     'S'           WAGGF             1
496700020131     C                   MOVEL     BARNPS        ARTNAP
496800050509     c* Imposto flag ARTFL2= 'P' se catagoria di spunta è tipica processo
496900050509     c*  operativo partenza
497000050509     c                   if        barnpg=5 or (barnpg=1 and wdefl=' ') or
497100050509     c                             (barnpg=3  and barspg='P')
497200050509     c                   eval      artfl2='P'
497300050509     c                   else
497400050509     c                   eval      artfl2=' '
497500050509     c                   endif
497600020131     C*
497700020131     C                   MOVEL     ' '           ARTDAB
497800020131     C                   SETON                                        03
497900020131     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
498000160624     c* ANOMALIE 60 E 61 ELIMINATE
498100160624     C***                EXSR      CRTA60
498200020131     C*
498300020131   X3C                   ELSE
498400020131    4C   05ARTNAP        IFEQ      BARNPS
498500020131     C                   MOVEL     BARDFV        ARTDAM
498600020131     C                   MOVEL     'S'           WAGGF             1
498700020131    4C                   ENDIF
498800020131     C*
498900020131     C* 'D' DI DISABILITAZIONE COLLO :
499000020131     C*   SE RISPUNTATO LANCIO PGM DI ABILITAZIONE X RIABILITARE BOLLA
499100020131    4C  N05ARTDAB        IFEQ      'S'
499200020131     C                   SETON                                        03
499300020131     C                   MOVEL     ' '           ARTDAB
499400040324     C                   MOVEL     'S'           WAGGF             1
499500020131    4C                   ENDIF
499600020131     C*
499700020131    4C  N05ARTNAP        IFNE      BARNPS
499800020131     C     RPS(Y)        ANDEQ     'L'
499900020131     C                   MOVEL     ARTNAP        ALF2              2
500000020131     C*
500100020131     C* CERCO RAGGRUPPAMENTO PISTOLA
500200020131     C                   EXSR      RICRAG
500300020131     C*
500400020131    5C     RPS(Z)        IFEQ      'M'
500500020131     C     RPS(Z)        OREQ      'A'
500600020131     C                   MOVEL     'S'           WAGGF             1
500700020131     C                   MOVEL     BARDFV        ARTDAM
500800020213     C                   MOVEL     BARNPS        ARTNAP
500900050509     c* Imposto flag ARTFL2= 'P' se catagoria di spunta è tipica processo
501000050509     c*  operativo partenza
501100050509     c                   if        barnpg=5 or (barnpg=1 and wdefl=' ') or
501200050509     c                             (barnpg=3  and barspg='P')
501300050509     c                   eval      artfl2='P'
501400050509     c                   else
501500050509     c                   eval      artfl2=' '
501600050509     c                   endif
501700020131     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
501800160624     c* anomalie 60 e 61 eliminate
501900160624     C***                EXSR      CRTA60
502000020131     C*
502100020131    5C                   ENDIF
502200020131    4C                   ENDIF
502300020131     C*
502400020131    3C                   END
502500020131     C*
502600020131     C                   Z-ADD     ARTDCM        WDCM
502700020131     C**
502800070208    3C     dls45flg2     IFNE      'A'
502900020131     C                   UPDATE    FNART000
503000020131     C                   ELSE
503100020131     C                   UNLOCK    FNART27L
503200020131    3C                   ENDIF
503300020131     C*
503400050223     C     KARB          CHAIN(N)  FNARB01L                           32
503500020131     C*
503600020131    3C     *IN32         IFEQ      *OFF
503700020131     C*
503800070208    4C     dls45flg2     IFNE      'A'
503900020131     C* SOLO SE PGM RICHIAMATO:
504000020131     C* SE VARIATA DATA DI ARRIVO SULLA BOLLA RIELABORO LA STATISTICA
504100020131    5C     *IN02         IFEQ      *ON
504200020131     C     ARTDAM        ANDNE     SAVDAM
504300020131    6C     ARTDAM        IFNE      WARDAM
504400020131     C     SAVDAM        ORNE      WSVDAM
504500020131     C                   EXSR      WRTSTA
504600020131     C                   MOVE      ARTDAM        WARDAM
504700020131     C                   MOVE      SAVDAM        WSVDAM
504800020131    6C                   END
504900020131    5C                   END
505000020131     C*
505100020131     C                   MOVEL     ARBNDC        WABNDC
505200040430     C                   MOVEL     ARBcca        WABcca
505300020131     C   03              MOVEL     'S'           WFBS
505400020131     C*
505500020131    4C                   ENDIF
505600020131     C**
505700020131     C** VEDO SE BOLLA GIACENTE
505800020131    4C     ARBFBC        IFEQ      'G'
505900050331     C     KGCA          CHAIN     Tigcp21L                           34
506000161007     c  n34              z-add     gcpmgc        gcpdgc
506100161007     c  n34              movel     gcpagc        gcpdgc
506200161007    5C  N34GCPFAS        IFLT      35
506300161007    5C     GCPFAS        oreq      36
506400161007     c* Verifico tramite driver se il collo dove essere in IMG
506500161007     c                   clear                   fnlgi1ds
506600161007     c                   eval      ilgi1aas=arbaas
506700161007     c                   eval      ilgi1lnp=arblnp
506800161007     c                   eval      ilgi1nrs=arbnrs
506900161007     c                   eval      ilgi1nsp=arbnsp
507000161007     c                   select
507100161007     c                   when      arbncp=arbncl
507200161007     c                   eval      ilgi1pkf=arbpkc
507300161007     c                   when      arbpkc>arbpkf
507400161007     c                   eval      ilgi1pkf=arbpkc
507500161007     c                   other
507600161007     c                   eval      ilgi1pkf=arbpkf
507700161007     c                   endsl
507800161007     c                   movel     fnlgi1ds      kpjbu
507900161007     c                   call      'FNLGI1R'
508000161007     c                   parm                    kpjba
508100161007     c                   parm                    tigcp00ds
508200161007     c                   movel     kpjbu         fnlgi1ds
508300161021     c* In questo caso la sped potrebbe essere sia in IMA che in IMG
508400161021     c*  quindi non devo dare alcun fuori posto
508500161021   5ac                   if        olgi1AMG='S'
508600161021     C                   MOVEL     'D'           WGIAC
508700161021  x5ac                   else
508800161021   5bc                   if        olgi1AMG='Y'  or olgi1inmg='S'
508900161201     C                   MOVEL     'S'           WGIAC
509000161201   5bc                   endif
509100161201   5ac                   endif
509200161201     c
509300161201   5ac                   if        wgiac<>' '
509400020131     C* NON LA CONSIDERO GIACENTE SE IN QUESTO MOMENTO C'E' UNA BOLLA
509500020131     C*  FIGLIA IN CONSEGNA O TUTTE LE FIGLIE SONO BLOCCATE
509600020131     C                   MOVEL     'S'           WFIBLO            1
509700020131     C                   CLEAR                   WESFI
509800020131     C     KARB          SETLL     FNLBL02L
509900020131     C     KARB          READE     FNLBL02L                               29
510000020131     C**
510100020131    6C     *IN29         DOWEQ     *OFF
510200020131     C*
510300050223     C     KLBL          CHAIN(N)  FNARB01L                           28
510400020131    7C     *IN28         IFEQ      *OFF
510500020131     C                   MOVEL     'S'           WESFI             1
510600020131     C* FIGLIA IN CONSEGNA
510700020131     C* NON LA CONSDIERO SE E' CONSEGNATA
510800020131   7AC     ARBDCM        IFEQ      0
510900020131    8C     ARBNDC        IFNE      0
511000020131     C                   SETON                                        29
511100020131     C                   CLEAR                   WGIAC
511200020131   X8C                   ELSE
511300020131     C*
511400020131     C* SE NON E' NEMMENO BLOCCATA MEMORIZZO
511500020131    9C     ARBFBC        IFNE      'B'
511600020131     C                   MOVEL     'N'           WFIBLO
511700020131    9C                   ENDIF
511800020131    8C                   ENDIF
511900020131   7AC                   ENDIF
512000020131    7C                   ENDIF
512100020131     C*
512200020131     C  N29KARB          READE     FNLBL02L                               29
512300020131    6C                   ENDDO
512400020131     C*
512500020131     C* SE TUTTE LE FIGLIE SONO BLOCCATE --> ESCLUDO
512600161201    6C     WGIAC         IFNE      ' '
512700020131     C     WESFI         ANDEQ     'S'
512800020131     C     WFIBLO        ANDEQ     'S'
512900020131     C                   CLEAR                   WGIAC
513000020131    6C                   ENDIF
513100020131     C*
513200161021   5aC                   ENDIF
513300161007    5C                   ENDIF
513400020131    4C                   ENDIF
513500020131    3C                   ENDIF
513600020131     C*
513700020131     C** COLLO EXPORT: VEDO SE CREARE L'ANOMALIA COLLO INVIATO PARTNER
513800020131     C*  MA RISPARATO IN IMA O IN PARTENZA/ARRIVO
513900020131     C*  NON CREO ANOMALIA SE E' DATA =99999999 PERCHE' SI TRATTA DI
514000020131     C*  DIROTTAMENTO E QUINDI VALGONO I COLLI DELLA FIGLIA E NON
514100020131     C*  DELLA MAMMA
514200020212    3C     NTWBAR        IFEQ      'EEX'
514300060322    3C     NTWBAR        oreq      'DPD'
514400060322    3C     NTWBAR        oreq      'FED'
514500060322    4C     BARDFV        ifGT      ARTDTR
514600020131     C     ARTDTR        ANDGT     0
514700020131     C     ARTDTR        ANDNE     *HIVAL
514800020131     C** CREO ANOMALIA 600 O 620
514900020131     C                   MOVEL     'S'           W60X              1
515000060322    4C                   ENDIF
515100060322    3C                   ENDIF
515200020131    2C                   ENDIF
515300020131     C*
515400020131   X2C                   ELSE
515500020131     C*
515600070221   2AC     dls45FLG      IFNE      '6'
515700020131     C** CREO LE ANOMALIE DI MANCA BOLLA SOLO SE NON SONO PASSATI I
515800020131     C*  GG DIPULIZIA BOLLE ARRIVI (PER EVITARE MANCA RECORD ERRATI)
515900020131     C                   EXSR      CTRDAT
516000020131     C**
516100020131   2BC     WCRE10        IFEQ      'S'
516200130301     c     wlocalxco     andne     'S'
516300020131     C**
516400020131     C* ANOMALIA: "MANCA BOLLA" --> ANCHE PER SPEDIZIONE LOCALE SE
516500020131     C*  E' SPARATA NELLE CATEGORIE ARRIVI
516600020131     C*  IL FOGLIO DI DEFLUENZA E' COME UNA CATEGORIA ARRIVI
516700020131    3C  N05BARNPG        IFNE      5
516800020131     C     BARSPG        ANDNE     'P'
516900020131     C     BARSPG        ANDNE     'I'
517000020131     C     WTIBOL        ANDEQ     'L'
517100020131     C     WTIBOL        OREQ      'A'
517200020131     C* LA CATEGORIA 1 LA CONSIDERO SOLTANTO SE E' DEFLUENZA
517300020131     C*
517400020131    4C     BARNPG        IFNE      1
517500020131     C     WTIBOL        OREQ      'A'
517600020131     C     WDEFL         OREQ      '1'
517700020131     C                   Z-ADD     10            COMCAA
517800020131     C                   EXSR      CRTANM
517900020131     C* Se l'anomalia 10 esiste già a livello max. creo anom. 410 per
518000020131     C* segnalare il persistere del manca record
518100020131     C     WANM10        IFEQ      '1'
518200020131     C                   Z-ADD     410           COMCAA
518300020131     C                   EXSR      CRTANM
518400020131     C                   END
518500020131    4C                   ENDIF
518600020131    3C                   ENDIF
518700020131     C*
518800020131     C* SE LNA ESTERA VERIFICO SE C'E' EVENTO 2 PER CREARE ANOM.600/620
518900020212    3C     NTWBAR        IFEQ      'EEX'
519000060322    3C     NTWBAR        oreq      'DPD'
519100060322    3C     NTWBAR        oreq      'FED'
519200020131     C*
519300020131     C* SE LNA ESTERA APRO FILE FNEVS02L
519400020131    4C     WAPER         IFEQ      ' '
519500020131     C                   OPEN      FNEVS02L
519600020131     C                   MOVEL     '1'           WAPER             1
519700020131    4C                   ENDIF
519800020131     C*
519900020131     C                   MOVEL     'E2 '         KCEV
520000020131     C     KEVS          SETGT     FNEVS02L
520100020131     C     KEVS          READPE    FNEVS02L                               34
520200020131    4C     *IN34         IFEQ      *OFF
520300020131     C     EVSDEV        ANDLT     BARDFV
520400020131     C* CREO ANOMALIA 600 E 620
520500020131     C                   MOVEL     'S'           W60X
520600020131    4C                   ENDIF
520700020131    3C                   ENDIF
520800020131   2BC                   ENDIF
520900020131   2AC                   ENDIF
521000020131    2C                   ENDIF
521100020131     C**
521200020201     C     ENDART        ENDSR
521300020213     C**************************************************************************
521400020213     C* AGGIORNO LE BOLLE TRANSITO
521500020213     C**************************************************************************
521600020213     C     AGGBTT        BEGSR
521700020213     C                   MOVEL     'T'           WTIBOL
521800020214     C* MEMORIZZO CHE C'E' BOLLA TRANSITO, MI SERVE PER CRTA50
521900020214     C                   MOVE      BTTDFV        COMDFV
522000020213     C* LA PRIMA VOLTA MEMORIZZO LA SPEDIZIONE
522100020213     C                   MOVEL     'T'           BTTTBO
522200020213     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
522300020213     C* NE PRECEDENTE
522400020213     C     BTTSPE        IFNE      SAVSPE
522500020213     C     WAGGF         IFEQ      'S'
522600020213     C                   EXSR      WRTAGB
522700020213     C                   CLEAR                   WAGGF
522800020213     C                   ENDIF
522900020213     C                   MOVEL     BTTSPE        SAVSPE
523000020213     C                   ENDIF
523100020213     C*
523200020213    5C     BARCAN        IFNE      ' '
523300020213     C     BARCAN        ANDNE     'E'
523400020213     C                   MOVEL     BARCAN        BTTCAN
523500020213     C                   MOVEL     'S'           WAGGF             1
523600020213    5C                   END
523700020213     C*
523800020213    5C     BARVUC        IFGT      0
523900020213     C*
524000020213    6C     BTTVUC        IFEQ      0
524100041013     c                   movel     'S'           WFVC
524200020213     C                   Z-ADD     BARVUC        BTTVUC
524300020213     C                   MOVEL     'S'           WAGGF             1
524400020213     C                   MOVEL     'T'           BTTFVC
524500020213    6C                   END
524600020213     C*
524700020213    5C                   END
524800020213     C*PESO DA CML
524900020213    5C     BARPUC        IFGT      0
525000020213     C*
525100020213    6C     BTTPUC        IFEQ      0
525200041013     c                   movel     'S'           WFPC
525300020213     C                   Z-ADD     BARPUC        BTTPUC
525400020213     C                   MOVEL     'S'           WAGGF             1
525500020213     C                   MOVEL     'T'           BTTFPC
525600020213    6C                   END
525700020213     C*
525800020213    5C                   END
525900020213     C*
526000020213    5C     BTTDET        IFEQ      0
526100020213     C                   MOVEL     'S'           WAGGF             1
526200020213     C                   MOVEL     BARDFV        BTTDET
526300020213     C                   MOVEL     BARNPS        BTTPET
526400020213     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
526500020213     C*  SOLO SE NON E' PISTOLA FASULLA
526600160624     C**                 MOVEL     'TT'          WCCH
526700160624     C**   OPS(Y)        IFLT      040
526800160624     C**                 EXSR      CRTA60
526900160624     C**                 CLEAR                   WCCH
527000160624     C**                 ENDIF
527100020213     C*
527200020213   X5C                   ELSE
527300020213     C*
527400020213    6C   05BTTPET        IFEQ      BARNPS
527500020213     C                   MOVEL     'S'           WAGGF             1
527600020213     C                   MOVEL     BARDFV        BTTDET
527700020213    6C                   END
527800020213     C*
527900020213    6C  N05BTTPET        IFNE      BARNPS
528000020213     C     RPS(Y)        ANDEQ     'L'
528100020213     C                   MOVEL     BTTPET        ALF2
528200020213     C*
528300020213     C* CERCO RAGGRUPPAMENTO PISTOLA
528400020213     C                   EXSR      RICRAG
528500020213     C*
528600020213    7C     RPS(Z)        IFEQ      'M'
528700020213     C     RPS(Z)        OREQ      'A'
528800020213     C                   MOVEL     BARDFV        BTTDET
528900020213     C                   MOVEL     'S'           WAGGF             1
529000020213     C                   MOVEL     BARNPS        BTTPET
529100160624     c
529200020213     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
529300160624     c* Anomalia 61 eliminara
529400160624     C**                 MOVEL     'TT'          WCCH
529500160624     C**                 EXSR      CRTA60
529600160624     C**                 CLEAR                   WCCH
529700020213    7C                   END
529800020213    6C                   END
529900020213     C*
530000020213    5C                   END
530100020213     C*
530200020213     C                   UPDATE    FNBTT000
530300020213     C*
530400020213     C* SE INVENTARIO O IN CONSEGNA ANOMALIA: FUORI POSTO DEL TRANSITO
530500020213    5C  N05BARNPG        IFEQ      3
530600020213     C     BARSPG        ANDNE     'P'
530700020213     C     BARNPG        OREQ      4
530800020213     C     BARNPG        OREQ      8
530900020213     C                   Z-ADD     125           COMCAA
531000020213     C                   EXSR      CRTANM
531100020213    5C                   END
531200040430     c* Se il collo è in transito, posso chiudere i fuori posto in partenza
531300040430     C                   Z-ADD     120           COMCAA
531400040430     C                   EXSR      chiuan
531500110103     c*
531600040422     c*Chiudo eventuale anomalia di disguido presente sul collo, se
531700040422     c*  generate da un 2 livello dell'area di partenza
531800040503     C* SOLO SE SPUNTA DI PISTOLA REALE
531900040503     c                   if        fsl(y)<>'F'
532000040428     c                   eval      comcaa=200
532100040422     C*
532200040503     C     Kanm          chain(N)  FNANM000
532300040428     c                   if        %found(fnanm02l)
532400040428     C                   CLEAR                   DSLV55
532500040428     C                   MOVEL     'P'           D55TPT
532600040428     C                   Z-ADD     anmfle        D55LIN
532700040428     C                   Z-ADD     bardfv        D55DRF
532800040428     C                   EXSR      FNLV55
532900040428     c                   if        bpes=d55tfp
533000040428     c                   exsr      chiuan
533100040428     c                   endif
533200040428     c                   endif
533300040503     c                   endif
533400041012     c*
533500041012     c* Aggiorno anche la bolla arrivo con i dati del transito
533600041012     c                   EXSR      AGGARRIVO
533700110103     c*
533800020213     C                   ENDSR
533900020201     C**************************************************************************
534000020201     C* CONTROLLO COSA POSSO AGGIORNARE IN BASE A CHI SONO
534100020201     C**************************************************************************
534200020201     C     CTRAGG        BEGSR
534300020204     C                   CLEAR                   WAGPAR
534400020204     C                   CLEAR                   WAGARR
534500150622     C                   CLEAR                   Wbsptp
534600150622     C                   CLEAR                   Wbspspu
534700020213     C**
534800020213     C** CERCO TERMINAL DELLA LINEA DI ARRIVO
534900020213     C                   EXSR      TERARR
535000050603     c                   eval      Usalnatfa=lnatfa
535100020204     C*
535200070208     c*         Non considero se c'e'bolla transito
535300070208     c     wesbtt        ifne      'S'
535400150622     C* SE NON C'È BOLLA TRANSITO VERIFICO SE COLLI CHE PARTONO DA
535500150817     C*  più filiali se ho trovato la bolla in partenza non partita
535600150817     c*  e con data di entrata = 0
535700150622     c                   if        wesblt='S' and Blpdt1=0
535800150622     C                             and blpdse=0
535900150818     c* se si tratta di una spunta tipica partenza
536000150818     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
536100150818     c                             or barnpg=1
536200150622     c                   exsr      sr_fibsp
536300150622     c                   if        wbsptp<>' '      and
536400150622     c                             %lookup(bpes:sbsp)>0
536500150622     c                   eval      wbspspu='S'
536600150818     C                   MOVEL     'P'           WTIBOL            1
536700150622     c                   endif
536800150622     c                   endif
536900150818     c                   endif
537000020204     C*
537100020201     C*  1) P.O. EFFETTUA SPUNTA = LNP SPUNTA (DA BOLLA)
537200020204    2C     BPES          IFEQ      LNPB
537300020201     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.PARTENZA DELLA LNP SPUNTA
537400020201     C*     IN BASE ALLA DATA SPEDIZIONE SE TROVATA LA BOLLA
537500020204     C     BPES          OREQ      LNPTFP
537600020204     C                   MOVEL     'S'           WAGPAR            1
537700020204   X2C                   ELSE
537800020204     C*  3) TER.PARTENZA P.O. EFFETTUA SPUNTA (IN BASE ALLA DATA SPED
537900020204     C*     SE TROVATA LA BOLLA) = TER.PARTENZA LNP SPUNTA SEMPRE IN
538000020204     C*     BASE ALLA DATA SPEDIZIONE SE TROVATA BOLLA
538100021125     C                   MOVEL     'P'           WTITER            1
538200020201     C                   EXSR      TERPES
538300020215    3C     D55TFP        IFEQ      LNPTFP
538400020204     C                   MOVEL     'S'           WAGPAR            1
538500020204    3C                   ENDIF
538600020204    2C                   ENDIF
538700020201    1C                   ENDIF
538800020204     C**
538900020204     C** NON AGGIORNO BOLLE ARRIVO SE SPUNTA DELL'AREA PARTENZA,
539000020204     C*   SPUNTA TIPICA PARTENZA E P.O. SPUNTA = LNA SPUNTA O TFA SPU
539100021125     c* controllo solo se spunta tipica partenza: l'arrivo non mi
539200021125     c*  interessa. non so perchè lo avevo preso in considerazione!!
539300150817     c
539400150817     c* Non aggiorno nemmeno se bolla da "trasferire"
539500020204    1C     WAGPAR        IFEQ      'S'
539600150817     c     wbspspu       oreq      'S'
539700150817     c
539800021125     C**   BPES          ifeq      lnpb
539900021125    2C**   BPES          OREQ      BARLNA
540000021125     C**   BPES          OREQ      LNATFA
540100021125     C**   pestfa        OREQ      LNATFA
540200020204     C* PARTENZA NON DEFLUENZA
540300020204    3C     BARNPG        IFEQ      1
540400020204     C     WDEFL         ANDEQ     '0'
540500020204     C* ENTRATA
540600020204     C     BARNPG        OREQ      5
540700020204     C* I.M.P.
540800020204     C     BARNPG        OREQ      3
540900020204     C     BARSPG        ANDEQ     'P'
541000020204     C* I.M.I.
541100020204     C     BARNPG        OREQ      3
541200020218     C     BARSPG        ANDEQ     'I'
541300020204     C                   GOTO      NOAGAR
541400020204    3C                   ENDIF
541500021125    2C**                 ENDIF
541600020204    1C                   ENDIF
541700020204     C**
541800021022     c     wesbtt        ifne      'S'
541900020204     C* AGGIORNO BOLLE ARRIVO SE :
542000020204     C*  1) P.O. EFFETTUA SPUNTA = LNA SPUNTA
542100020205    1C                   SELECT
542200020204     C     BPES          WHENEQ    BARLNA
542300020204     C                   MOVEL     'S'           WAGARR            1
542400020205    2C     WAGPAR        IFEQ      'S'
542500020204     C                   MOVEL     'A'           WWFVC
542600020204     C                   MOVEL     'A'           WSPLOC            1
542700020205    2C                   ENDIF
542800020204     C*  2) P.O. EFFETTUA SPUNTA E' IL TER.ARRIVO   DELLA LNA SPUNTA
542900020204     C*     IN BASE ALLA DATA SPUNTA
543000020204     C     BPES          WHENEQ    LNATFA
543100050603     C     BPES          oreq      bolTFA
543200020204     C                   MOVEL     'S'           WAGARR            1
543300020205    2C     WAGPAR        IFEQ      'S'
543400020204     C                   MOVEL     'A'           WWFVC
543500020204     C                   MOVEL     'T'           WSPLOC            1
543600020205    2C                   ENDIF
543700050603     c* imposto il terminal di arrivo =BPES
543800050603     c                   eval      Usalnatfa=BPES
543900020204   X1C                   OTHER
544000020204     C*  3) TER.ARRIVO P.O. EFFETTUA SPUNTA
544100020204     C*     = TER.ARRIVO LNA SPUNTA IN BASE ALLA DATA SPUNTA
544200020204    2C     PESTFA        IFEQ      LNATFA
544300050603    2C     PESTFA        oreq      bolTFA
544400020204     C                   MOVEL     'S'           WAGARR            1
544500020204     C                   MOVEL     'D'           WSPLOC
544600020204     C                   MOVEL     'A'           WWFVC
544700050603     c* imposto il terminal di arrivo =ter arrivo di BPES
544800050603     c                   eval      Usalnatfa=pestfa
544900110117   x2c                   else
545000110117     c
545100110117     c*  4) Se nessuna è vera ma chi spara è la capostipite della lna-£6
545200110117     c*     anche se è una incongruenza la considero valida ai fini dell'arrivo
545300110117     C* CARICO LA L6 DELLA FILIALE GESTIONE
545400110117    3C     BPES          IFNE      SAVPES
545500110117     C                   MOVEL     BPES          SAVPES
545600110117     C                   EXSR      CARL6
545700110117    3C                   ENDIF
545800110117    c
545900110117     C     BARLNA        LOOKUP    L6                                     34
546000110117    3c                   if        *in34
546100110117     C                   MOVEL     'S'           WAGARR            1
546200110117    4C     WAGPAR        IFEQ      'S'
546300110117     C                   MOVEL     'A'           WWFVC
546400110117     C                   MOVEL     'A'           WSPLOC            1
546500110117    4C                   ENDIF
546600110117    3C                   ENDIF
546700110117     c
546800020205    2C                   ENDIF
546900020205    1C                   ENDSL
547000021022     c*
547100021022    2C                   endif
547200020204     C**
547300070208     C     NOAGAR        ENDSR
547400150622     C**************************************************************************
547500150622     C*  vedo se lnp o ksc confermabile da più linee
547600150622     C**************************************************************************
547700150622     C     sr_fibsp      BEGSR
547800150622     c                   eval      WBspTp=*blanks
547900150622    2c                   if        blpccm>0
548000150622     c                   eval      w_clibo=blpccm
548100150622     c                   else
548200150622     c                   eval      w_clibo=blpksc
548300150622    2c                   endif
548400150622     c     w_clibo       chain     fibsp02l
548500150622     c                   if        %found(fibsp02l) and bspfpa='S'
548600150622     c                   eval      wBspTp='K'
548700150622     c****               if        not %found(fibsp02l) or bspfpa<>'S'
548800150622     c                   else
548900150817     c     LNPB          chain     fibsp01l
549000150622     c                   if        %found(fibsp01l) and bspfpa='S'
549100150622     c                   eval      wBspTp='L'
549200150622     c                   endif
549300150622     c                   endif
549400150622     c                   endsr
549500020201     C**************************************************************************
549600020201     C* TERMINAL DI PARTENZA E ARRIVO DEL P.O. EFFETTUA SPUNTA
549700020201     C**************************************************************************
549800020205     C     TERPES        BEGSR
549900020201     C* IMPOSTO TERMINAL PARTENZA E ARRIVO
550000020201     C                   CLEAR                   DSLV55
550100020205     C                   MOVEL     WTITER        D55TPT
550200020205     C                   MOVEL     LNPB          D55LNP
550300020205     C                   MOVEL     WDTRIF        D55DRF
550400020205     C                   MOVE      BPES          D55LIN
550500020205     C                   EXSR      FNLV55
550600020205     C                   ENDSR
550700020205     C**************************************************************************
550800020205     C* CALL A PGM FNLV55R
550900020205     C**************************************************************************
551000020205     C     FNLV55        BEGSR
551100020201     C                   CALL      'FNLV55R'
551200020201     C                   PARM                    DSLV55
551300020201     C**
551400020205     C     D55ERR        IFNE      ' '
551500020205     C                   MOVE      D55LIN        D55TFA
551600020205     C                   MOVE      D55LIN        D55TFP
551700020201     C                   END
551800020201     C                   ENDSR
551900041012     C**************************************************************************
552000041012     C* AGGIORNO CODICE ANOMALIA
552100041012     C**************************************************************************
552200041012     c     AGGCAN        BEGSR
552300041012    3C     BARCAN        IFNE      ' '
552400041012     C     BARCAN        ANDNE     'E'
552500070508     c     barcan        andne     artcan
552600041012     C**
552700070508     c* Aggiorno la testata e quindi scrivo AGB solo se artcan
552800070508     c*  è vuoto prima di questo aggiornamento
552900070508     c                   if        artcan=' '
553000041012     C                   MOVEL     'S'           WAGGF             1
553100070508     c                   endif
553200070508     C                   MOVEL     BARCAN        ARTCAN
553300041012     c                   endif
553400041012     c                   endsr
553500041012     C**************************************************************************
553600041012     C* AGGIORNO volume cml
553700041012     C**************************************************************************
553800041105     C     aGGVOL        BEGSR
553900041012    3C                   SELECT
554000041012    3C                   when      BARVUC = *zeros
554100041012      * NON SOSTITUISCO IL VOLUME ARRIVO SE LNA FEDEX
554200041012    3C                   when      ARTFVC = 'A' and ARTVUC > *zeros
554300041012    3C                                          and NTWBAR = 'FED'
554400041012      * IL VOLUME ARRIVO O TRANSITO LI SOSTITUISCO SE QUELLO DELLA
554500041012      * SPUNTA E' RILEVATO DALLA PARTENZA
554600041012    3C                   when       ARTVUC = *zeros  or
554700041012    3C                             (ARTVUC > *zeros and ARTFVC <> 'P'
554800041012     C                                              and WWFVC  =  'P')   or
554900041012    3C                             (ARTVUC > *zeros and ARTFVC =  'A'
555000041012     C                                              and WWFVC  =  'T')
555100041012     c                   movel     'S'           wfvc
555200041012     C                   MOVEL     'S'           WAGGF             1
555300041012     C                   Z-ADD     BARVUC        ARTVUC
555400041012     C                   MOVEL     WWFVC         ARTFVC
555500041012    3c                   ENDSL
555600041012     c                   ENDSR
555700041012     C**************************************************************************
555800041012     C* AGGIORNO Peso   cml
555900041012     C**************************************************************************
556000041012     c     AGGPES        BEGSR
556100041012    3C     BARPUC        IFGT      0
556200041012    4C     ARTPUC        IFEQ      0
556300041012     C* IL PESO ARRIVO O TRANSITO LI SOSTITUISCO SE QUELLO DELLA
556400041012     C* SPUNTA E' RILEVATO DALLA PARTENZA
556500041012     C     ARTPUC        ORNE      0
556600041012     C     WWFVC         ANDEQ     'P'
556700041012     C     ARTFPC        ANDNE     'P'
556800041012     C     ARTPUC        ORNE      0
556900041012     C     WWFVC         ANDEQ     'T'
557000041012     C     ARTFPC        ANDEQ     'A'
557100041012     c                   movel     'S'           wfpc
557200041012     C                   MOVEL     'S'           WAGGF             1
557300041012     C                   Z-ADD     BARPUC        ARTPUC
557400041012     C                   MOVEL     WWFVC         ARTFPC
557500041012    4C                   END
557600041012     C*
557700041012    3C                   END
557800041012     c                   ENDSR
557900091209     C**************************************************************************
558000091209     c*    Aggiorno fiar5 o lo scrivo se inesistente
558100091209     C**************************************************************************
558200091209     c     AggioFIAR5    BEGSR
558300091209     c
558400091209     c                   if        %found(fiar501l)
558500101129     c                   movel     datReale      §ar5arrdt
558600101129     c                   movel     oraReale      §ar5arrhm
558700091209     c                   movel(p)  dar5gen       ar5uni
558800091209      * se devo trasmettere alla sede sfleggo per la trasmissione
558900091209     c                   If        §Ar5Trse = 'S'
559000091209     c                   Clear                   Ar5Ft2
559100091209     c                   EndIf
559200091209     c                   update    fiar5000
559300091209      * Scrivo Fiar5
559400091209   x2c                   Else
559500091209     c                   Clear                   Fiar5000
559600091209     c                   Eval      Ar5Aas = ArtAas
559700091209     c                   Eval      Ar5Lnp = ArtLnp
559800091209     c                   Eval      Ar5Nrs = ArtNrs
559900091209     c                   Eval      Ar5Nsp = ArtNsp
560000091209     c                   Eval      Ar5Trd = 'GEN'
560100091209     c                   Move      WDATE1        Ar5Dac
560200091209     c                   time                    Ar5Orc
560300091209     c                   Clear                   Dar5Gen
560400101129     c                   movel     datReale      §ar5arrdt
560500101129     c                   movel     oraReale      §ar5arrhm
560600091209     c                   Movel(p)  DAr5Gen       Ar5Uni
560700091209     c                   Write     Fiar5000
560800091209    2c                   EndIf
560900091209     c                   ENDSR
561000041012     C**************************************************************************
561100041012     C* AGGIORNO anche la bolla di arrivo
561200041012     C**************************************************************************
561300041012     c     AGGARRIVO     BEGSR
561400041012     c* Aggiorno anche bolla  arrivi con dati del transito
561500130228     c                   clear                   aggioart          1
561600041012     C     KBAR          CHAIN     FNART27L
561700130228    1c                   if        %found(fnart27l)
561800041012     C                   MOVEL     'A'           ARTTBO
561900130228     c                   eval      Aggioart='S'
562000041012     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
562100041012     C* NE PRECEDENTE
562200041012     C     ARTSPE        IFNE      SAVSPE
562300041012     C     WAGGF         IFEQ      'S'
562400041012     C                   EXSR      WRTAGB
562500041012     C                   CLEAR                   WAGGF
562600041012     C                   ENDIF
562700041012     C                   MOVEL     ARTSPE        SAVSPE
562800041012     C                   ENDIF
562900110103     c*
563000041012     c* Codice anomalia: tengo sempre l'ultima anomalia
563100041012     C                   EXSR      AGGCAN
563200041012     C
563300041012     C* AGGIORNO DATA ENTRATA AL TRANSITO
563400041012     C                   Z-ADD     barDFV        ARTDET
563500041012     C                   MOVEL     barNPS        ARTPET
563600041012     C                   MOVEL     'S'           WAGGF             1
563700041012     C     barFGS        IFNE      ARTFLP
563800041012     C* FILIALE IN GESTIONE <> FILIALE TRANSITO
563900041012     C                   MOVEL     barFGS        ARTFLP
564000041012     C                   MOVEL     *ZEROS        ARTDUT
564100041012     C                   MOVEL     *ZEROS        ARTPUT
564200041012     C                   END
564300041012     c* peso e volume
564400041012     C     WWFVC         IFEQ      *BLANKS
564500041012     C                   MOVEL     'T'           WWFVC
564600041012     C                   END
564700041012     c                   EXSR      AGGVOL
564800041012     c                   EXSR      AGGpes
564900041012     c                   update    fnart000
565000130228     c
565100041013     c* Se trovata bolla, eseguo solo per disguido la CRTA60
565200160624     c* anomalie 60 e 61 eliminate
565300050322     c                   eval      savesart=wesart
565400041013     c                   movel     'S'           wesart
565500160624     c**                 if        wtibol='D'
565600160624     c**                 eval      wsavflg=dls45FLG
565700160624     c**                 eval      dls45FLG='6'
565800160624     c**                 exsr      CRTA60
565900160624     c**                 eval      dls45FLG=wsavflg
566000160624     c**                 endif
566100130228     c
566200130228    1c                   endif
566300041013     c* Chiudo una eventuale anomalia 15 aperta controllando le date anomali
566400041013     c*  e spunta
566500041013     c                   z-add     15            comcaa
566600041013     c                   eval      wcontrdfv='S'
566700041013     c                   eval      wstess='S'
566800041013     c                   eval      wchiu='S'
566900041013     c                   exsr      chiuan
567000130228     c
567100130228    1c                   if        Aggioart='S'
567200130228     c                   eval      wesart=savesart
567300130228     c                   endif
567400041013     c*
567500041012     c                   ENDSR
567600070208     C**************************************************************************
567700070208     C* Routine iniziale
567800070208     C**************************************************************************
567900070208     c     *inzsr        BEGSR
568000070208     C     KBAR          KLIST
568100070208     C                   KFLD                    BARLNP
568200070208     C                   KFLD                    BARLNA
568300070208     C                   KFLD                    BARNRS
568400070208     C                   KFLD                    BARNSC
568500070208     C     KBAR37        KLIST
568600070208     C                   KFLD                    bpes
568700070208     C                   KFLD                    BARLNP
568800070208     C                   KFLD                    BARLNA
568900070208     C                   KFLD                    BARNRS
569000070208     C                   KFLD                    BARNSC
569100160721     C     KBAR_t        KLIST
569200160721     C                   KFLD                    savflp
569300160721     C                   KFLD                    BARLNP
569400160721     C                   KFLD                    BARLNA
569500160721     C                   KFLD                    BARNRS
569600160721     C                   KFLD                    BARNSC
569700070208     C     KBAR1         KLIST
569800070208     C                   KFLD                    BARNPG
569900070208     C                   KFLD                    BARNfv
570000070208     C                   KFLD                    BARfgs
570100070208     C                   KFLD                    BARLNP
570200070208     C                   KFLD                    BARLNA
570300070208     C                   KFLD                    BARNRS
570400070208     C                   KFLD                    BARNSC
570500140616     C     KBAR_qd4      KLIST
570600140616     C                   KFLD                    kdel_npg
570700140616     C                   KFLD                    BARLNP
570800140616     C                   KFLD                    BARLNA
570900140616     C                   KFLD                    BARNRS
571000140616     C                   KFLD                    BARNSC
571100140616     C                   KFLD                    BARfgs
571200140616     C                   KFLD                    qd_dfs
571300140616     C                   KFLD                    qd_hms
571400070208     C     KBAR2         KLIST
571500070208     C                   KFLD                    BARNPG
571600070208     C                   KFLD                    BARLNP
571700070208     C                   KFLD                    BARLNA
571800070208     C                   KFLD                    BARNRS
571900070208     C                   KFLD                    BARNSC
572000070208     C                   KFLD                    BARfgs
572100140613     C     KBAR2_d       KLIST
572200140613     C                   KFLD                    kdel_npg
572300140613     C                   KFLD                    BARLNP
572400140613     C                   KFLD                    BARLNA
572500140613     C                   KFLD                    BARNRS
572600140613     C                   KFLD                    BARNSC
572700140613     C                   KFLD                    BARfgs
572800140616     C                   KFLD                    wdfs
572900070208     C     KBAR3         KLIST
573000070208     C                   KFLD                    WNPG
573100070208     C                   KFLD                    BARLNP
573200070208     C                   KFLD                    BARLNA
573300070208     C                   KFLD                    BARNRS
573400070208     C                   KFLD                    BARNSC
573500070208     C     KBRV7         KLIST
573600070208     C                   KFLD                    BARLNP
573700070208     C                   KFLD                    BARLNA
573800070208     C                   KFLD                    BARNRS
573900070208     C                   KFLD                    BARNSC
574000070208     C     KARB          KLIST
574100070208     C                   KFLD                    ARTAAS
574200070208     C                   KFLD                    ARTLNP
574300070208     C                   KFLD                    ARTNRS
574400070208     C                   KFLD                    ARTNSP
574500070208     C     KGCA          KLIST
574600070208     C                   KFLD                    ARTAAS
574700070208     C                   KFLD                    ARTLNP
574800070208     C                   KFLD                    ARTNRS
574900070208     C                   KFLD                    ARTNSP
575000070208     C                   KFLD                    KFRG
575100070208     C     KSAV          KLIST
575200070208     C                   KFLD                    SAVTBO
575300070208     C                   KFLD                    SAVAAS
575400070208     C                   KFLD                    SAVLNP
575500070208     C                   KFLD                    SAVNRS
575600070208     C                   KFLD                    SAVNSP
575700070208     C                   KFLD                    KAGB
575800070208     C     KEAGB         KLIST
575900070208     C                   KFLD                    WTBO
576000070208     C                   KFLD                    WDAAS
576100070208     C                   KFLD                    WDLNP
576200070208     C                   KFLD                    WDNRS
576300070208     C                   KFLD                    WDNSP
576400070208     C                   KFLD                    KAGB
576500070208     C     KBLT          KLIST
576600070208     C                   KFLD                    BLTAAS
576700070208     C                   KFLD                    BLTLNP
576800070208     C                   KFLD                    BLTNRS
576900070208     C                   KFLD                    BLTNSP
577000070208     C     KBTT          KLIST
577100070208     C                   KFLD                    BTTflp
577200070208     C                   KFLD                    BTTAAS
577300070208     C                   KFLD                    BTTLNP
577400070208     C                   KFLD                    BTTNRS
577500070208     C                   KFLD                    BTTNSP
577600070208     C     KANM          KLIST
577700070208     C                   KFLD                    BARLNP
577800070208     C                   KFLD                    BARLNA
577900070208     C                   KFLD                    BARNRS
578000070208     C                   KFLD                    BARNSC
578100070208     C                   KFLD                    COMCAA            3 0
578200070208     C     KEVS          KLIST
578300070208     C                   KFLD                    BARLNP
578400070208     C                   KFLD                    BARLNA
578500070208     C                   KFLD                    BARNRS
578600070208     C                   KFLD                    BARNSC
578700070208     C                   KFLD                    KCEV
578800070208     C     KFVV1         KLIST
578900070208     C                   KFLD                    WNPG
579000070208     C                   KFLD                    WNFV
579100070208     C                   KFLD                    WFGS
579200070208     C     KFVV          KLIST
579300070208     C                   KFLD                    WNPG
579400070208     C                   KFLD                    wdata
579500070208     C                   KFLD                    KTFA2
579600070208     C     KFVA1         KLIST
579700070208     C                   KFLD                    WFGS
579800070208     C                   KFLD                    WNFV
579900070208     C     KFGV1         KLIST
580000070208     C                   KFLD                    WNFV
580100070208     C                   KFLD                    tfpfgs
580200070208     c     KFGV2         KLIST
580300070208     C                   KFLD                    lnptfp
580400070208     C                   KFLD                    bardfv
580500070208     C     KFVA4         KLIST
580600070208     C                   KFLD                    KNPG4
580700070208     C                   KFLD                    KNFV4
580800070208     C                   KFLD                    KFGS4
580900070208     C     KFWA          KLIST
581000070208     C                   KFLD                    FVALNP
581100070208     C                   KFLD                    FVANFV
581200070208     C                   KFLD                    FVALAI
581300070208     C     KTAB          KLIST
581400070208     C                   KFLD                    CODUT
581500070208     C                   KFLD                    COD
581600070208     C                   KFLD                    KEY
581700070208     C     KTAB2         KLIST
581800070208     C                   KFLD                    CODUT
581900070208     C                   KFLD                    COD
582000130228     C     KTBE2         KLIST
582100130228     C                   KFLD                    KCOD
582200130228     C                   KFLD                    KKE1
582300130228     C                   KFLD                    KKE2
582400070208     C*
582500070208     C     KLBL          KLIST
582600070208     C                   KFLD                    LBLAAN
582700070208     C                   KFLD                    LBLLPN
582800070208     C                   KFLD                    LBLNRN
582900070208     C                   KFLD                    LBLNSN
583000070208     C     KDCT          KLIST
583100070208     C                   KFLD                    DCDAAC
583200070208     C                   KFLD                    DCDFIL
583300070208     C                   KFLD                    DCDNCA
583400070208     C     KBRVE2        KLIST
583500070208     C                   KFLD                    BRVNPG
583600070208     C                   KFLD                    BRVLNP
583700070208     C                   KFLD                    BRVLNA
583800070208     C                   KFLD                    BRVNRS
583900070208     C                   KFLD                    BRVNSC
584000070710     C     KBRVE1        KLIST
584100070710     C                   KFLD                    BRVNPG
584200070710     C                   KFLD                    BRVNfv
584300070710     C                   KFLD                    BRVfgs
584400070710     C                   KFLD                    BRVLNP
584500070710     C                   KFLD                    BRVLNA
584600070710     C                   KFLD                    BRVNRS
584700070710     C                   KFLD                    BRVNSC
584800070208     C     Kar5          KLIST
584900070208     C                   KFLD                    anmaas
585000070208     C                   KFLD                    anmLNp
585100070208     C                   KFLD                    anmNRS
585200070208     C                   KFLD                    anmnsp
585300070208     C                   KFLD                    Ktrd
585400091211     C     Kar52         KLIST
585500091211     C                   KFLD                    artaas
585600091211     C                   KFLD                    artLNp
585700091211     C                   KFLD                    artNRS
585800091211     C                   KFLD                    artnsp
585900091211     C                   KFLD                    Ktrd
586000070208     C*
586100070208     C     *LIKE         DEFINE    FVAdfv        Wpul050
586200070208     C     *LIKE         DEFINE    FVALNP        WFVALNP
586300070208     C     *LIKE         DEFINE    FVANFV        WFVANFV
586400070208     C     *LIKE         DEFINE    FVADFV        WFVADFV
586500070208     C     *LIKE         DEFINE    bardfv        Wdata
586600070208     C     *LIKE         DEFINE    wfgs          WWWfgs
586700070208     C     *LIKE         DEFINE    wfgs          tfpfgs
586800070208     C     *LIKE         DEFINE    wfgs          savfgs
586900070208     c                   clear                   savfgs
587000070208     C     *LIKE         DEFINE    FVVFLE        WFLE
587100070208     C     *LIKE         DEFINE    ARTFLP        SAVFLP
587200070208     C     *LIKE         DEFINE    BARDFV        DSPB
587300070208     C     *LIKE         DEFINE    BARDFV        COMDFV
587400070208     C     *LIKE         DEFINE    BARDFV        SVVDFV
587500070208     C     *LIKE         DEFINE    BLPTFP        LNPTFP
5876000702080    C     *LIKE         DEFINE    BARDFV        WDTRIF
587700070208     C     *LIKE         DEFINE    BARDFV        PRIDAT
587800070208     C     *LIKE         DEFINE    BARDFV        SA2DFV
587900070208     C     *LIKE         DEFINE    ANMCCH        WCCH
588000070208     C     *LIKE         DEFINE    GCPFRG        KFRG
588100070208     C                   MOVEL     '0'           KFRG
588200070208     C     *LIKE         DEFINE    AGBAGB        KAGB
588300070208     C     *LIKE         DEFINE    AGBFBS        WFBS
588400070208     C     *LIKE         DEFINE    AGBFVC        WFVC
588500070208     C     *LIKE         DEFINE    AGBFPC        WFPC
588600070208     C     *LIKE         DEFINE    ARBNDC        WABNDC
588700070208     C     *LIKE         DEFINE    ARTDCM        WATDCM
588800070208     C     *LIKE         DEFINE    ARbcca        WABcca
588900070208     C     *LIKE         DEFINE    ARbcca        savcca
589000070208     C     *LIKE         DEFINE    BARLNA        SVVLNA
589100070208     C     *LIKE         DEFINE    BARLNP        SVVLNP
589200070208     C     *LIKE         DEFINE    ARTDCM        WDCM
589300070208     C     *LIKE         DEFINE    ANMCAA        CAACRE
589400070208     C     *LIKE         DEFINE    ANMCAA        CAACIU
589500070208     C     *LIKE         DEFINE    FGVTTR        WTTR
589600140613     C     *LIKE         DEFINE    FGVFCF        WFCF
589700070208     C     *LIKE         DEFINE    EVSCEV        KCEV
589800070208     C     *LIKE         DEFINE    D33MAC        SAVMAC
589900070208     C     *LIKE         DEFINE    BRVFLE        BARFLE
590000070208     C     *LIKE         DEFINE    BRVFGS        SAVPES
590100070208     C     *LIKE         DEFINE    BRVFGS        KFGS4
590200070208     C     *LIKE         DEFINE    TBLCOD        COD
590300070208     C     *LIKE         DEFINE    TBLKEY        KEY
590400070208     C     *LIKE         DEFINE    BARLNA        LNATFA
590500070208     C     *LIKE         DEFINE    BARLNA        Usalnatfa
590600100713     C     *LIKE         DEFINE    BARLNA        WgenLNAtfa
590700070208     C     *LIKE         DEFINE    blptfa        bolTFA
590800070208     C     *LIKE         DEFINE    BARLNA        PESTFA
590900070208     C     *LIKE         DEFINE    BARLNA        PESTFP
591000070208     C     *LIKE         DEFINE    BARLNP        LNPB
591100070208     C     *LIKE         DEFINE    FVVFGS        WFGS
591200070208     C     *LIKE         DEFINE    FVVSPG        WSPG
591300070208     C     *LIKE         DEFINE    FVVNPG        WNPG
591400070208     C     *LIKE         DEFINE    FVVNPG        KNPG4
591500070208     C     *LIKE         DEFINE    FVVNFV        WNFV
591600070208     C     *LIKE         DEFINE    FVVNFV        KNFV4
591700070208     C     *LIKE         DEFINE    FVVFLE        WLIN
591800070208     C     *LIKE         DEFINE    FVVFLE        WLINPR
591900070208     C     *LIKE         DEFINE    FVVSPG        BARSPG
592000140616     C     *LIKE         DEFINE    FVVSPG        brvSPG
592100070208     C     *LIKE         DEFINE    FVVDFV        WDFV
592200070208     C     *LIKE         DEFINE    FVVDFV        SAVDFV
592300070208     C     *LIKE         DEFINE    FVVDFV        BARDFV
592400070208     C     *LIKE         DEFINE    FVVDFV        BRVDFV
592500070208     C     *LIKE         DEFINE    BARKEY        SAVKEY
592600070208     C     *LIKE         DEFINE    BRVKEY        SA1KEY
592700070208     C     *LIKE         DEFINE    ARBLNP        COMLNP
592800070208     C     *LIKE         DEFINE    ARBAAS        COMAAS
592900070208     C     *LIKE         DEFINE    ARBNSP        COMNSP
593000070208     C     *LIKE         DEFINE    ARBTFP        WPTFP
593100070208     C     *LIKE         DEFINE    ANMFL1        WFL1
593200070208     C     *LIKE         DEFINE    D55TFA        KTFA2
593300070208     C     *LIKE         DEFINE    D55TFA        SAVTFA
593400070208     C     *LIKE         DEFINE    ARTFVC        WWFVC
593500070208     C     *LIKE         DEFINE    ARTDAM        SAVDAM
593600070208     C     *LIKE         DEFINE    ARTDAM        WARDAM
593700070208     C     *LIKE         DEFINE    ARTDAM        WSVDAM
593800070208     C     *LIKE         DEFINE    FVADFV        SAVDAO
593900070208     C     *LIKE         DEFINE    FVALNP        WVALNP
594000070208     C     *LIKE         DEFINE    DCTTAD        SAVTAD
594100070208     C                   CLEAR                   SAVTAD
594200070208     C     *LIKE         DEFINE    §OGNTW        NTWBAR
594300070208     C     *LIKE         DEFINE    GI8INV        DATMIN
594400070208     C     *LIKE         DEFINE    BRVATR        SAWATR
594500080317     C     *LIKE         DEFINE    BRVTSU        SAWtsu
594600070208     C*
594700070208     C                   MOVEL     'DT'          KAGB
594800070208     c                   ENDSR
594900140613     C**************************************************************************
595000140613     C* Cerca spunta 4
595100140613     C**************************************************************************
595200140613     c     Cer_cons      BEGSR
595300140616     c* imposto data immiss.spunta della spunta che sto caricando
595400140616     c                   exsr      ImpoDFS
595500140616     c*
595600140616     C     KBAR2_d       setll     FNBRV05L
595700140616     C     KBAR2_d       reade(n)  FNBRV05L
595800140616    1c                   dow       not %eof(fnbrv05l)
595900140613     c* controllo data e ora immissione
596000140613     c                   clear                   brv_imm
596100140613     c                   clear                   brv_sca
596200140613     c                   z-add     brvhms        brv_imm
596300140613     c                   movel     brvdfs        brv_imm
596400140613     c                   z-add     brvhcs        brv_sca
596500140613     c                   movel     brvdcs        brv_sca
596600140613     c                   exsr      sceglidata
596700140613     c                   z-add     a_imm         b_imm
596800140613     c
596900140613     c* a parità di data devo tenere la più recente
597000140613     c                   clear                   brv_imm
597100140613     c                   clear                   brv_sca
597200140613     c                   z-add     barhms        brv_imm
597300140613     c                   movel     wDFS          brv_imm
597400140613     C                   Z-ADD     WDATE1        barDCS
597500140613     C                   time                    barHCS
597600140613     c                   z-add     barhcs        brv_sca
597700140613     c                   movel     bardcs        brv_sca
597800140613     c                   exsr      sceglidata
597900140613     c*
598000140613     c                   select
598100140616     c* quella trovata + recente --> non carico la spunta
598200140613    2c                   when      b_imm > A_IMM  and brvdfs=wdfs
598300140613     c
598400140613     c* Se anche la data foglio è = non carico la spunta ricevuta
598500140613    3C     BRvKEY        IFNE      SA1KEY
598600140613     C                   MOVEL     BRvKEY        SA1KEY
598700140613     C                   Z-ADD     BRvNPG        WNPG
598800140613     C                   Z-ADD     BRvNFV        WNFV
598900140613     C                   Z-ADD     BRvFGS        WFGS
599000140613     C                   movel     BRvnps        Wnps
599100140613     C                   EXSR      RILFOG
599200140613     c                   Z-ADD     WDFV          BRVDFV
599300140616     c                   movel     WFCF          BRVFCF
599400140616     c                   movel     Wspg          BRVspg
599500140613    3c                   endif
599600140613     c
599700140616     c                   if        barnpg=3 and
599800140616    3c                             brvdfv=bardfv
599900140616     c                   eval      amb_QD ='N'
600000140616     c                   endif
600100140616     c                   if        barnpg=4 and brvspg='A' and
600200140616    3c                             brvdfv=bardfv
600300140616     c                   eval      amb_QD ='N'
600400140616     c                   endif
600500140613     c
600600140616     c                   if        amb_qd='N'
600700140613     c* memorizzo la spunta nuova negli errori
600800140613     C                   MOVEL     'S'           WBRVE             1
600900140613     C                   EXSR      RIEMPI
601000140613     C                   CLEAR                   WBRVE
601100140613     C                   WRITE     FNBRVE
601200140616     c                   leave
601300140613    3c                   endif
601400140616     c
601500140616     c* quella di carico + recente --> cancello quella già presente
601600140613    2c                   when      A_imm > B_IMM  and brvdfs=wdfs
601700140613     c* Se anche la data foglio è = non carico la spunta ricevuta
601800140613    3C     BRvKEY        IFNE      SA1KEY
601900140613     C                   MOVEL     BRvKEY        SA1KEY
602000140613     C                   Z-ADD     BRvNPG        WNPG
602100140613     C                   Z-ADD     BRvNFV        WNFV
602200140613     C                   Z-ADD     BRvFGS        WFGS
602300140613     C                   movel     BRvnps        Wnps
602400140613     C                   EXSR      RILFOG
602500140613     c                   Z-ADD     WDFV          BRVDFV
602600140616     c                   movel     WFCF          BRVFCF
602700140616     c                   movel     Wspg          BRVspg
602800140613    3c                   endif
602900140613     c
603000140616     c                   if        barnpg=3 and
603100140616    3c                             brvdfv=bardfv and brvfcf=' '
603200140616     c                   eval      amb_QD ='S'
603300140613     c* mi slavo data e ora imm spunta 4 per la delete
603400140616     c                   eval      qd_npg=4
603500140616     c                   eval      qd_dfs=brvdfs
603600140616     c                   eval      qd_hms=brvhms
603700140616     c                   leave
603800140613     c                   endif
603900140616     c                   if        barnpg=4 and  brvspg='A' and
604000140616    3c                             brvdfv=bardfv and brvfcf=' '
604100140616     c                   eval      amb_QD ='S'
604200140616     c* mi slavo data e ora imm spunta 4 per la delete
604300140616     c                   eval      qd_npg=3
604400140616     c                   eval      qd_dfs=brvdfs
604500140616     c                   eval      qd_hms=brvhms
604600140616     c                   eval      qd_nfv=brvnfv
604700140616     c                   leave
604800140616     c                   endif
604900140613     c
605000140613    2c                   endsl
605100140616     c
605200140616     C     KBAR2_d       reade(n)  FNBRV05L
605300140616    1c                   enddo
605400140613     c
605500140613     c                   ENDSR
605600140613     C**************************************************************************
605700140613     C* Scelta data tra immisisone e caicamento
605800140613     C**************************************************************************
605900140613     c     Sceglidata    BEGSR
606000140613     c                   if         brv_imm>brv_sca and
606100140616     c                              %subst(%editc(brv_sca:'X'):1:8)>*zeros
606200140613     c                   eval       A_imm=brv_sca
606300140613     c                   else
606400140613     c                   eval       A_imm=brv_imm
606500140613     c                   endif
606600140613     c                   ENDSR
606700140613     C**************************************************************************
606800140613     C*    impostazione data immissione spunta
606900140613     C**************************************************************************
607000140613     c     ImpoDFS       BEGSR
607100140613     c* Se data immissione > di oggi, imposto la minore tra oggi e data fogl
607200140613     c                   if        BARDFS<=wdate1
607300140613     C                   Z-ADD     BARDFS        wDFS
607400140613     c                   else
607500140613     c                   if        wdate1<=bardfv
607600140613     c                   z-add     wdate1        wDFS
607700140613     c                   else
607800140613     c                   z-add     bardfv        wDFS
607900140613     c                   endif
608000140613     c                   endif
608100140613     c                   ENDSR
608200140616     C**************************************************************************
608300140616     C*    cancellazione spunta meno recente IMA o cons
608400140616     C**************************************************************************
608500140616     c     Canc_QD       BEGsr
608600140616     c                   eval      kdel_npg=qd_npg
608700140616    1c                   if        kdel_npg=4
608800140616     c     kbar_qd4      chain     fnbrv05l                             38
608900140616     c  n38              if        %found(fnbrv05l)
609000140616     C     KBRVE1        SETLL     FNBRVE1L                               33
609100140616    2C     *IN33         IFEQ      *OFF
609200140616     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
609300140616     C                   WRITE     FNBRVE
609400140616    2C                   ENDIF
609500140616     c
609600140616     c                   delete    fnbrv005
609700141029     c                   endif
609800140616     c
609900140616   x1c                   else
610000140616     c     kbar_qd4      setll     fnbrv05l
610100140616     c     kbar_qd4      reade     fnbrv05l
610200140616    2c                   dow       not %eof(fnbrv05l) and brvnfv<>qd_nfv
610300140616     c     kbar_qd4      reade     fnbrv05l
610400140616    2c                   enddo
610500140616    2c                   if        not %eof(fnbrv05l)
610600140616     C     KBRVE1        SETLL     FNBRVE1L                               33
610700140616    3C     *IN33         IFEQ      *OFF
610800140616     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
610900140616     C                   WRITE     FNBRVE
611000140616    3C                   ENDIF
611100140616     c                   delete    fnbrv005
611200140616    2c                   endif
611300140616     c
611400140616    1c                   endif
611500140616     c                   ENDSR
611600150622     C**************************************************************************
611700150622     C*    Aggiornamenti da tabella BSP
611800150622     C**************************************************************************
611900150622     c     DA_BSP        BEGsr
612000150622     c* Scrivo FNAGB per aggiornare la bolla
612100150622     C                   CLEAR                   FNAGB000
612200150622     C                   MOVEL     'P'           AGBTBO
612300150622     C                   MOVEL     'CB'          AGBAGB
612400150622     C                   MOVEL     blpAAS        AGBAAS
612500150622     C                   MOVEL     blpLNP        AGBLNP
612600150622     C                   MOVEL     blpNRS        AGBNRS
612700150622     C                   MOVEL     blpNSP        AGBNSP
612800150622     C                   WRITE     FNAGB000                             99
612900150622     c                   ENDSR
613000070208     C**************************************************************************
613100070208     C* Elaborazioni per chiusura pgm
613200070208     C**************************************************************************
613300070208     c     ELACHIUDI     BEGSR
613400070208     C* ELABORO L'ULTIMO DATO MEMORIZZATO
613500070208     C                   EXSR      WRTAGB
613600070208     c
613700070208     C                   CLEAR                   DSLV55
613800070208     C                   MOVEL     'C'           D55TLA
613900070208     C                   EXSR      FNLV55
614000070208     C* SCRITTURA ULTIMO RECORD IN FNSTA
614100070208     C   02              EXSR      WRTSTA
614200070208     C**
614300070208     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
614400070208     c                   clear                   fidn12ds
614500070208     c                   eval      i12tla = 'C'
614600070208     c                   call      'FIDN12R'
614700070208     c                   parm                    fidn12ds
614800161007     c
614900161007     c                   clear                   fnlgi1ds
615000161007     c                   eval      ilgi1tla = 'C'
615100161007     c                   movel     fnlgi1ds      kpjbu
615200161007     c                   call      'FNLGI1R'
615300161007     c                   parm                    kpjba
615400070208     c                   ENDSR
615500110103
615600110103      *---------------------------------------------------------------*
615700110103      *?Reperimento dati del job (Utente/Operativi)                  ?*
615800110103      *---------------------------------------------------------------*
615900110103     c     sr_DatiJob    BEGSR
616000110103      *
616100110103     c     *dtaara       define    §azute        AZUTEds
616200110103     c     *dtaara       define    §datiute      dDatiUte
616300110103      *
616400110103     c                   in(E)     *dtaara
616500110103     c                   IF        %Error or RSUT = *blanks
616600110103     c                   clear                   TIBS34ds
616700110103     c                   call      'TIBS34R'
616800110103     c                   parm                    TIBS34ds
616900110103     c                   in        *dtaara
617000110103     c                   ENDIF
617100110103      *
617200110103     c                   ENDSR
617300040601**         CM5
617400040601DLYJOB DLY(5)
617500980928**         MSG
617600070906CARICA SPUNTE:Fogli Inesitenti (FGS NPG NFV-NPS) FNLS45R
617700070906XXX X XXXXX-xx;  XXX X XXXXX-xx;  XXX X XXXXX-xx;  XXX X XXXXX-xx;
617800070906           -  ;             -  ;             -  ;             -  .
