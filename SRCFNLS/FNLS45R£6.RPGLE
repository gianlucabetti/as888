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
003001180115     FFIars01l  uF a E           K DISK
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
027200170505     D**** DS5APT        E DS
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
031800170509     D boldbr          s                   like(arbdbr)
031900130228     D KCOD            s                   like(tbecod)
032000130228     D Kke1            s                   like(tbeke1)
032100130228     D Kke2            s                   like(tbeke2)
032200130228     D SAWBRV          s                   like(brv)
032300080317     D WPES            s                   like(brvpes)
032400070208     D BPES            s                   like(brvpes)
032500070208     D BTFP            s                   like(brvFLE)
032600050322     D savesart        s                   like(wesart)
032700041105     D wprimotbo       s                   like(savtbo)
032800070221     D WSAVFLG         s                   like(dls45FLG)
032900140613     D wdfs            s                   like(brvdfs)
033000140616     D qd_nfv          s                   like(brvnfv)
033100140616     D qd_npg          s                   like(brvnpg)
033200140616     D qd_dfs          s                   like(brvdfs)
033300140616     D qd_hms          s                   like(brvhms)
033400140616     D bardcs          s                   like(brvdcs)
033500140616     D barhcs          s                   like(brvhcs)
033600140616     D barfcf          s                   like(fvvfcf)
033700140616     D brvfcf          s                   like(fvvfcf)
033800150622     d w_clibo         s                   Like(blpccm)
033900160726     d welasta_p       s              1
034000160726     d wBspTp          s              1
034100150622     d wBspspu         s              1
034200140618     d A_imm           s             14  0 inz
034300140618     d B_imm           s             14  0 inz
034400140618     d brv_imm         s             14  0 inz
034500140618     d brv_sca         s             14  0 inz
034600140618     d DatReale        s              8  0 inz
034700140618     d oraReale        s              4  0 inz
034800130228     d Indx            s              3  0
034900130228     d SAVfnrs         s              5  0 inz
035000130228     d Wflsnrs         s              5  0
035100041013     D WCONTRDFV       s              1    inz('N')
035200041105     D WAGBART         s              1    inz(' ')
035300070906     D WNPS            s              2    inz
035400080630     D WOK             s              1    inz
035500170505     D W138            s              1    inz
035600080630     D Secondi         s              9  0 inz
035700040621     D ktrd            s                   like(ar5trd)  inz('GEN')
035701180115     D karstr          s                   like(arstrc)  inz('J')
035800080630     D Timeold         s               t   timfmt(*iso)
035900080630     D Timenew         s               t   timfmt(*iso)
036000980928     D* DEFINIZIONE COSTANTI
036100980928     D CUTI            C                   CONST('EDP*      ')
036200960423     C*---------------------------------------------------------------*
036300921014     C* RIEPILOGO INDICATORI
036400970115     C*---------------------------------------------------------------*
036500921014     C* 02    - RICHIAMATO
036600930914     C* 03    - POSSO RIABILITARE LA BOLLA
036700921021     C* 05    - MODIFICA DEL SOLO NUMERO DI FOGLIO
036800000505     C* 28/29 - DI COMODO
036900070212     C* 30    - LETTURA FIBAR00F
037000070131     C* 31    - CHAIN FNBRV
037100950927     C* 32    - LETTURA FILE FNBLT FNART FNARB FNBTT
037200950927     C* 33/34 - DI COMODO
037300921014     C* 37    - NON ESISTE BOLLA TRANSITO
037400070131     C* 38    - ERRORE SU WRITE FNBRV e su update
037500921105     C* 39    - RECORD ALLOCATO
037600921014     C* 40    - SE NON RECORD ALLOCATI O ALLOCATI PER 3 VOLTE FINE PGM
037700950926     C* 41    - COMODO
037800030528     C* 45    - Errore su write fnagb
037900040310     C* 46    - lookuip su schiera NTS
038000970204     C* 96    - ESISTE GIA' A MENO L'ANOMALIA CHE DEVO CREARE
038100970204     C* 97    - USATO PER LA CHIUSURA ANOMALIE CON 145 E 146
038200080325     C* 98    - FIBAR allocato --> esco dal pgm
038300080325     C* 99    - DI COMODO
038400921014     C*---------------------------------------------------------------*
038500911016     C     *ENTRY        PLIST
038600070212     C                   PARM                    fnls45ds
038700070208     C                   PARM                    dsrecbrv
038800941214     C*---------------------------------------------------------------*
038900070221     c* tipo di richiamo in base a dls45FLG
039000070208     C*---------------------------------------------------------------*
039100930521     C* FLG = '2' MODIFICATO SOLO NUMERO FOGLIO
039200160624     c
039300160624     c* Eliminato tipo richiamo "6" in quanto eliminate anomalie 60 e 61
039400070221     C* FLG = '6' CREO SOLO ANOMALIA 61 (EXSR CRTA60) DA SPU DISGUIDO
039500160624     c
039600991022     C* FLG = 'D' CREO ANOMALIA DISGUIDO PER COLLO DA ME CONFERMABILE
039700930521     C* FLG = ' ' INSERIMENTO NUOVA SPUNTA O VARIATA SOLO ANOMALIA
039800930521     C* FLG = 'C' CHIUDO PGM CON LR E BASTA
039900050418     C* FLG = 'T' aggiorno bolla transito x disguido
040000070221     c*
040100070221     c* Richiami attualmente usati: '2'  ' '  'T'  'C'
040200070208     C*---------------------------------------------------------------*
040300070208     c* Eaborazioni per chiusura pgm
040400070221    1C     dls45FLG      IFEQ      'C'
040500070208     c                   EXSR      ELACHIUDI
040600040811     c*
040700930521     C                   SETON                                        LR
040800930521     C*
040900930521   X1C                   ELSE
041000941229     C*
041100941229     C* IMPOSTAZIONI INIZIALI
041200941229    2C     UNO           IFEQ      ' '
041300941229     C                   EXSR      CARTAB
041400941229    2C                   END
041500070208     c
041600070208     c* Impostazione di BPES
041700070208     c                   if        dls45pes>*zeros
041800070208     c                   movel     dls45pes      BPES
041900070208     c                   endif
042000070208     c                   if        dls45tfp>*zeros
042100070208     c                   movel     dls45tfp      BTFP
042200070208     c                   endif
042300070208     c
042400070208     c                   EXSR      IMPO
042500930521     C*
042600930521     C                   SETOFF                                       05
042700941229     C                   Z-ADD     1             B                 3 0
042800930521     C****
042900930521     C**** P G M   R I C H I A M A T O
043000930521     C****
043100070208    2C                   if        dls45ric='S'
043200070208     C                   SETON                                        02
043300070208     c
043400070208     C* nel secndo parametro passato ci sono tutti i campi della spunta
043500070208     c*  con la quale effettuare gli aggiornamenti
043600070212     C                   Z-ADD     d_brvnpg      BARNPG
043700070212     C                   Z-ADD     D_brvnfv      BARNFV
043800070212     C                   Z-ADD     d_brvlnp      BARLNP
043900070212     C                   Z-ADD     d_brvlna      BARLNA
044000070212     C                   Z-ADD     d_brvnrs      BARNRS
044100070212     C                   Z-ADD     d_brvnsc      BARNSC
044200070212     C                   MOVEL     d_brvcan      BARCAN
044300070212     C                   Z-ADD     d_brvnps      BARNPS
044400070212     C                   Z-ADD     d_brvvuc      BARVUC
044500070212     C                   Z-ADD     d_brvpuc      BARPUC
044600070212     C                   Z-ADD     d_brvdfs      BARDFS
044700070212     C                   Z-ADD     d_brvhms      BARHMS
044800070212     C                   Z-ADD     d_brvmis      BARmis
044900070212     C                   movel     d_brvtap      BARtap
045000070212     C                   Z-ADD     d_brvznc      BARZNC
045100070212     c                   movel     d_brvpru      barpru
045200070212     c                   movel     d_brvfgs      barfgs
045300101130     c                   movel     d_brvdcs      brvdcs
045400101130     c                   movel     d_brvhcs      brvhcs
045500070208     c
045600960530     C                   CLEAR                   SAVMAC
045700960529     C**
045800070208     c** 05 on - modificato solo numero di foglio viaggio
045900070221    3C     dls45FLG      IFEQ      '2'
046000921229     C                   SETON                                        05
046100020213     C                   ENDIF
046200960524     C*
046300070209     C* CONTROLLO CHE ESISTA COD.ANOMALIA NELLA TABELLA ANOMALIE
046400020204     C     BARCAN        IFNE      ' '
046500020204     C     BARCAN        ANDNE     ' '
046600020204     C     BARCAN        LOOKUP    CAN                                    34
046700020204     C  N34              CLEAR                   BARCAN
046800020204     C                   ENDIF
046900961213     C*
047000961213     C* LETTURA FOGLIO E IMPOSTAZIONE CAMPI DI COMOD
047100961213     C                   EXSR      BARFGV
047200960529     C**
047300960529     C* SE NON HO TROVATO IL FOGLIO ESCO IMPOSTANDO L'ERRORE
047400960529    3C     BARDFV        IFEQ      0
047500070906     C                   SETON                                        39
047600960529   X3C                   ELSE
047700991230     C     BARLNA        CHAIN     AZORG01L                           31
047800040303    4c                   if        *in31 or
047900040303     c                             orgfag<>'F' and orgfag<>'A'
048000040303     C                   CLEAR                   NTWBAR
048100040303   x4c                   else
048200040303     C                   MOVE      ORGDE3        OG143
048300040303     C                   MOVE      §OGNTW        NTWBAR
048400000523     C*
048500000523     C* SE DEVO CREARE ANOMALIA DI DISGUIDO, CHIAMO SOLTANTO CRTA52
048600070221    5C     dls45FLG      IFEQ      'D'
048700991022     C                   MOVEL     'D'           WTIBOL
048800000523   X5C                   ELSE
048900920424     C*
049000920424     C* SE TROVO RECORD ALLOCATI RITENTO 3 VOLTE
049100000523    6C     B             DOWLE     3
049200950707     C                   MOVE      'S'           OKAGG             1
049300911016     C                   EXSR      UPDARB
049400000523    7C     OKAGG         IFEQ      'N'
049500950707     C                   Z-ADD     4             B
049600000523   X7C                   ELSE
049700021112     C                   Z-ADD     4             B
049800000523    7C                   ENDIF
049900150824     c
050000000523    6C                   ENDDO
050100150824
050200150824     c* Colli che partono dapiù filiali: facico fare la write al
050300150824     c*  chiamante
050400150824     C                   IF        wbsptp<>' ' and wbspspu='S'
050500150824     c                             and wagpar=' '
050600150824     c                   eval      %subst(dls45flo:1:1)='B'
050700150824     c                   endif
050800150824     c
050900040303    5C                   ENDif
051000040303    4C                   ENDIF
051100960410     C*
051200960410     C* PER BOLLA ARRIVO NON LOCALE, VEDO SE CREARE ANOMALIA 50
051300001114     C*  COLLO ARRIVATO NON PARTITO
051400960529     C*  NON ELABORO SE COLLO LOCALE O SOLO DELLA LINEA DI PARTENZA
051500001115     C* (N.B. INFATTI, SE BOLLA ARRIVO LOCALE, NON C'E' TRASMISSIONE DI
051600001115     C*  F.V. E QUINDI QUESTA ROUTINE NON SAREBBE MAI IN GRADO DI
051700001115     C*  CREARE L'ANOMALIA 50  DAL MOMENTO CHE VA A CERCARE IL FOGLIO
051800001115     C*  QUESTO VALE ANCHE PER I COLLI 180 PER 89)
051900020218    5C  N05WTIBOL        IFNE      'P'
052000960530     C     WTIBOL        ANDNE     'S'
052100020214     C     WTIBOL        ANDNE     'R'
052200070221     C     dls45FLG      ANDNE     '6'
052300960410     C                   EXSR      CRTA50
052400000523    5C                   ENDIF
052500000523     C*
052600960530    3C                   ENDIF
052700911016     C                   GOTO      FINE
052800960529     C*
052900960529    2C                   ENDIF
053000930521     C****
053100930521     C**** P G M    N O N    R I C H I A M A T O
053200930521     C****
053300930521     C* APRO FILE
053400040601     c                   clear                   wconta            2 0
053500040601     c                   movel     'S'           wapri             1
053600070212     C                   OPEN(e)   FIBAR00F
053700040601     c* Attendo 5 secondi e riprovo max 9 volte poi esco con errore
053800040601     c                   dow       %error  and wconta<10
053900040601     C                   Z-ADD     15            LUNG
054000040601     C                   MOVEA(p)  CM5           COMMAN           80
054100040601     C                   CALL      'QCMDEXC'                            99
054200040601     C                   PARM                    COMMAN
054300040601     C                   PARM                    LUNG             15 5
054400040601     c
054500040601     c                   add       1             wconta
054600070212     C                   OPEN(e)   FIBAR00F
054700040601     c                   enddo
054800040601     c
054900040601     c* Se ancora errore vado a fine file
055000040601     c                   if        %error
055100040601     c                   eval      wapri='N'
055200040601     c                   goto      fine
055300040601     c                   endif
055400040601     c
055500920424     C                   SETON                                        40
055600941214     C                   Z-ADD     *ZERO         SA1KEY
055700960530     C                   CLEAR                   SAVMAC
055800070208     C                   MOVE      DLS45FLG2     FLGPRE            1
055900911014     C*
056000080325     C                   READ      FIBAR00F                             9830
056100911016     C*
056200930521    2C     *IN40         DOWEQ     '1'
056300080325    2C     *IN98         andeq     *off
056400920424     C                   SETOFF                                       40
056500930521    3C     *IN30         DOWEQ     '0'
056600080325    3C     *IN98         andeq     '0'
056700930602     C*
056800040419     C                   SETOFF                                       3839
056900930521    4C     BARLNP        IFGT      *ZERO
057000040419     C     BARLNA        ANDGT     *ZERO
057100960731     C     BARNSC        ANDGT     *ZERO
057200040224     C     BARNpg        ANDGT     *ZERO
057300070201     c* Per ora non si accettano fogli maggiori di 100.000
057400040601     C     BARNfv        ANDlT     100000
057500070212     c
057600070212     c* Ad ogni record imposto BPES, perchè per le spunte cat 1 di un
057700070212     c*  secondo livello, BPES diventa il terminal per motivi di
057800070212     c*  creazione della anoamlia 200, per cui potrebbe rimanare
057900070212     c*  sporco se dopo ci fossero spunte di altre categorie
058000070212     c                   movel     dls45pes      BPES
058100021015     C                   Z-ADD     BPES          BARFGS
058200070209     c
058300160511      * CLEARO PESO E VOLUME UNITARI SE SUPERIORI AL MAX PREVISTO
058400160511      * limiti per transpallet  (pistola 46)
058500160511      * LIMITI PER RILEVATORE PESO VOLUME PALLET (Pistola 48)
058600160511      * limite per VDL  ( tutte le altre pistole)
058700160511     c                   select
058800160511     c                   when      barnps= 48
058900030723     C     BARPUC        IFGT      §vpoMPP
059000030723     C                   CLEAR                   BARPUC
059100030723     C                   ENDIF
059200030723     C     BARVUC        IFGT      §vpoMVP
059300030723     C                   CLEAR                   BARVUC
059400030723     C                   ENDIF
059500160511     c
059600160511     c                   when      barnps= 46
059700160511     C     BARPUC        IFGT      §vpoMPT
059800160511     C                   CLEAR                   BARPUC
059900160511     C                   ENDIF
060000160511     c                   other
060100160511     C     BARPUC        IFGT      §vpoMPV
060200160511     C                   CLEAR                   BARPUC
060300160511     C                   ENDIF
060400160511     C     BARVUC        IFGT      §vpoMVV
060500160511     C                   CLEAR                   BARVUC
060600160511     C                   ENDIF
060700160511     C                   Endsl
060800160511     c
060900040805      * CLEARO PESO E VOLUME UNITARI SE INFERIORI AL MINIMO PREVISTO
061000040805      * I LIMITI VALGONO PER TUTTI I TIPI DI IMPIANTO
061100040805     C     BARPUC        IFLT      §vpoSPV
061200040805     C                   CLEAR                   BARPUC
061300040805     C                   ENDIF
061400040805     C     BARVUC        IFLT      §vpoSVV
061500040805     C                   CLEAR                   BARVUC
061600040805     C                   ENDIF
061700020204     C* CONTROLLO CHE ESISTA COD,ANOMALIA NELLA TABELLA ANOMALIE
061800020204     C     BARCAN        IFNE      ' '
061900020204     C     BARCAN        ANDNE     ' '
062000020204     C     BARCAN        LOOKUP    CAN                                    34
062100020204     C  N34              CLEAR                   BARCAN
062200020204     C                   ENDIF
062300040419     c
062400070906     c* Non carico se LNP inesistente
062500040419     C     BARLNP        CHAIN     AZORG01L                           31
062600040419     c* Non carico se LNA inesistente
062700070906   4bC     *IN31         IFEQ      *Off
062800040419     c     orgfag        andeq     'A'
062900040419     C     *IN31         oreq      *Off
063000040419     c     orgfag        andeq     'F'
063100980212     C*
063200991230     C     BARLNA        CHAIN     AZORG01L                           31
063300040419     c* non carico se LNA inesistente
063400040303    5C     *IN31         IFEQ      *ON
063500040224     c     orgfag        orne      'A'
063600040224     c     orgfag        andne     'F'
063700020212     C                   CLEAR                   NTWBAR
063800040303   x5C                   ELSE
063900020212     C                   MOVEL     ORGDE3        OG143
064000020212     C                   MOVEL     §OGNTW        NTWBAR
064100070912     c
064200070912     C* LETTURA FOGLIO DELLA SPUNTA E IMPOSTAZIONE CAMPI DI COMOD
064300070912     C                   EXSR      BARFGV
064400070912     c
064500070912     c* Non carico se numero foglio inesistente
064600070912   5ac                   if        bardfv>0
064700140616     c                   clear                   amb_QD            1
064800140613     c                   clear                   A_imm
064900140613     c                   clear                   B_imm
065000140613     c*
065100140613     c* se IMA e spunta consegna PIu' RECENTE --> cancello senza caricare
065200140613     c                   if        barnpg=3 and barspg='A'
065300140616     c                   eval      kdel_npg=4
065400140613     c                   exsr      Cer_cons
065500140613     c                   endif
065600140616     c                   if        barnpg=4
065700140616     c                   eval      kdel_npg=3
065800140616     c                   exsr      Cer_cons
065900140616     c                   endif
066000140613
066100140616   5cc                   if        amb_QD<>'N'
066200040303     c**
066300040303     C* VERIFICO IL FLAG NELLA DS7N
066400040303     C                   Z-ADD     1             WA                3 0
066500040310     C     BARNPG        LOOKUP    CAT(WA)                                46
066600040310    6C     *IN46         IFEQ      *OFF
066700040303     C     NST(WA)       ORNE      '1'
066800040303     c* Se tengo tutte le spunte, verifico spunta doppia per lo stesso NFV
066900070131     C     KBAR1         CHAIN     FNBRV01L                           31
067000040303   x6c                   else
067100911016     C*
067200040303     C* spunta doppia per stessa categoria e p.o. gestione
067300140613     C     KBAR2         CHAIN     FNBRV05L                           31
067400040303    6c                   endif
067500941229     C*
067600040303     C* Aggiorno bolle e spunta se:
067700040303     C* - spunta nuova (31 on)
067800040303     C* - spunta già esistente ma dati del record diversi(BAR1<>BRV)
067900040303     C* - spunta già presente ma nel giro prec.trovati record allocati(B>1)
068000070212     c* - trovati errori  (BARERR=E)
068100070912   5bC     *IN31         IFEQ      *ON
068200941229     C     BAR1          ORNE      BRV
068300001114     C     B             ORGT      1
068400070212     C     BARERR        OReq      'E'
068500040303     c**
068600040303     C* OPERAZIONI PER SPUNTA GIA' PRESENTE e da tenerne una
068700040303    6C     *IN31         IFEQ      *OFF
068800040303     C     nst(WA)       ANDEQ     '1'
068900010608     C     BAR1          ANDNE     BRV
069000010608     C                   EXSR      SRBRVE
069100040303    6C                   ENDIF
069200070710     c**
069300070710     C* OPERAZIONI PER SPUNTA GIA' PRESENTE e da tenerne una
069400070710    6C     *IN31         IFEQ      *OFF
069500070710     C     nst(WA)       ANDne     '1'
069600070710     C     BAR1          ANDNE     BRV
069700070710     c* la precedente spunta non ha l'anomalia
069800070710     c     brvcan        andeq     ' '
069900070710     c* nell'attuale è stata inserita
070000070710     c     barcan        andne     ' '
070100070822     c*  Carico solo se data/ora Immissione spunta sono < della precedente
070200070822     c     barimmis      andgt     brvimmis
070300070710     C                   EXSR      SRBRVE
070400070710    6C                   ENDIF
070500040303     C* RIEMPI I CAMPI
070600040303     C                   EXSR      RIEMPI
070700040303     C**
070800040303     C**  A G G I O R N A M E N T I
070900961213     C                   MOVE      'S'           OKAGG             1
071000911112     C                   EXSR      UPDARB
071100911014     C**
071200920424     C**  AGGIORNO SPUNTA
071300950707     C*   Non l'aggiorno se spunta partenza manuale e di collo già
071400950707     C*   consegnato
071500040303    6C     OKAGG         IFEQ      'S'
071600040303     C*
071700960521     C* SE NO  LA DEVO TRASMETTERE LA FLAGGO
071800961213    7C     WNOTRA        IFEQ      'S'
071900960521     C                   MOVEL     'N'           BRVFTR
072000960521     C                   MOVEL     *HIVAL        BRVDTR
072100991223     C                   MOVEL     *HIVAL        BRVHTR
072200960521     C                   CLEAR                   WNOTRA
072300961213    7C                   ENDIF
072400980115     C* SE SPUNTA PARTENZA RELATIVA AD UN FOGLIO VIAGGIO SU CUI NON E'
072500980115     C* PIU' PRESENTE LA LNA DEL COLLO -> FLAGGO LA SPUNTA
072600980205     C* (RIENTRA IN QUESTO CASO ANCHE UNA SPUNTA CON COD.ANOMALIA "E"
072700980205     C* CIOE' UNA SPUNTA LA CUI LINEA NON ERA PRESENTE SUL FV GIA' NEL
072800980205     C* MOMENTO IN CUI E' STATA FATTA)
072900040303    7C     BARNPG        IFEQ      1
073000980115     C                   MOVE      BARLNA        W003A             3
073100980313     C     W003A         LOOKUP    FFS                                    33
073200980115     C  N33              MOVEL     'L'           BRVFTR
073300980115     C  N33              MOVEL     *HIVAL        BRVDTR
073400991223     C  N33              MOVEL     *HIVAL        BRVHTR
073500980210     C   33BRVCAN        IFEQ      'E'
073600980210     C                   CLEAR                   BRVCAN
073700980210     C                   END
073800041020    7C                   ENDIF
073900041020     c
074000041020     c* Imposto brvatr='D' per disguido
074100041020     c                   if        wtibol='D'
074200041020     c                   eval      brvatr='D'
074300041020     c                   else
074400041020     c                   clear                   brvatr
074500041020    7C                   ENDIF
074600040303     c**
074700070131     C* AGGIORNO LF FNBRV01L
074800040310    7C  N31*IN46         IFEQ      *OFF
074900040303     C     NST(WA)       ORNE      '1'
075000070131     C                   UPDATE    FNBRV000
075100040303     C                   ELSE
075200140613     C* AGGIORNO LF FNBRV05L
075300140613     C                   UPDATE    FNBRV005
075400040303    7C                   END
075500010607     C*
075600070131     C   31              WRITE     FNBRV000                             38
075700140613     c
075800140613     c* Se previsto cancello la spunta "doppia" tra 3 e 4
075900140616    7c                   if        amb_QD='S'
076000140616     c                   exsr      Canc_QD
076100140616    7c                   endif
076200150824     c**
076300150824     c*  COLLO SPUNTABILE DA PIÙ FILIALI
076400150824     c**
076500150824     c** elaboro per "K-cli" con BPES <> LNPD ( in qeusto caso ho pulito
076600150824     c*                                        AGGPAR)
076700150824     c*  oppure  per "L-linea" se TFPLNP <>TFPPES
076800150824     c* lo faccio dopo la write di BRV perchè fnlv52r deve leggere la spunta per
076900150824     c*  determinare la nuova lnp della bolla
077000150824     C                   IF        wbsptp<>' ' and wbspspu='S'
077100150824     c                             and wagpar=' '
077200150824     c                   exsr      DA_BSP
077300150824     c                   endif
077400140613     c
077500040303   X6C                   ELSE
077600040303     C*
077700040310    7C  N31*IN46         IFEQ      *OFF
077800040303     C     NST(WA)       ORNE      '1'
077900070131     C                   UNLOCK    FNBRV01L
078000040303     C                   ELSE
078100140613     C                   UNLOCK    FNBRV05L
078200040303    7C                   END
078300040303    6C                   END
078400950927     C* PER BOLLA ARRIVO NON LOCALE, VEDO SE CREARE ANOMALIA 50
078500950927     C*  COLLO ARRIVATO NON PARTITO
078600960207     C*  NON ELABORO SE COLLO LOCALE O SOLO DELLA LINEA DI PARTENZA
078700020214    6C  N05WTIBOL        IFEQ      'D'
078800020214     C     WTIBOL        OREQ      'A'
078900020214     C     WTIBOL        OREQ      'T'
079000020218     C     WTIBOL        OREQ      'L'
079100950927     C                   EXSR      CRTA50
079200950927    6C                   ENDIF
079300950927     C*
079400070912   5bC                   ELSE
079500040303     C*
079600040310    6C  N31*IN46         IFEQ      *OFF
079700040303     C     NST(WA)       ORNE      '1'
079800070131     C                   UNLOCK    FNBRV01L
079900040303     C                   ELSE
080000140613     C                   UNLOCK    FNBRV05L
080100040303    6C                   END
080200070912   5bC                   END
080300140613   5cC                   END
080400110103     c*
080500070912   5ac                   else
080600070912     c* Scrivo la spunta errata in fibar00e
080700070912     c                   movel     bpes          barpes
080800070912     c                   movel     wdate1        bardim
080900070912     c                   write     barcoerr                             99
081000070912   5aC                   END
081100040303    5C                   END
081200070906   4bC                   END
081300040419    4C                   END
081400911112     C*
081500900619     C* CANCELLO RECORD LETTO
081600001114     C* Se spunta imi e non ho aggiornato blt perchè inesistente o se
081700021203     C* non ho aggiornato blp perchè record allocato la spunta non viene
081800001114     C* cancellata.
081900021107     c                   if        not *in38 and not *in39
082000021107     C                   DELETE    BARCO
082100021107     c                   else
082200021203     c* Se ci fosse errore ma passati 4 giorni dalla data di immissione
082300021203     c*  spunte, deleto lo stesso
082400021203     c                   if        brvdfs<=wpul050
082500021203     c                   DELETE    BARCO
082600021203     c                   else
082700070212     c                   eval      barerr='E'
082800021107     c                   update    BARCO
082900021107     c                   endif
083000021203     c                   endif
083100900618     C*
083200080325     C                   READ      FIBAR00F                             9830
083300930521    3C                   END
083400021112     C** 40              ADD       1             B
083500021112     C   40              z-add     4             B
083600920424     C* DOPO 3 VOLTE ESCO
083700930521    3C   40B             IFGT      3
083800920424     C                   SETOFF                                       40
083900930521   X3C                   ELSE
084000070212     C                   CLOSE     FIBAR00F
084100070212     C                   OPEN      FIBAR00F
084200070212     C                   READ      FIBAR00F                               30
084300930521    3C                   END
084400930521    2C                   END
084500970224     C* ELABORO L'ULTIMO DATO MEMORIZZATO
084600970224     C                   EXSR      WRTAGB
084700911016     C*
084800911016     C     FINE          TAG
084900930521     C** 02 OFF - PROGRAMMA NON RICHIAMATO
085000930521     C**
085100950926     C** LANCIO PGM DI AGGIORNAMENTO BOLLE
085200961212    2C     *IN02         IFEQ      *OFF
085300970814     C**
085400970814     C                   CLEAR                   DSLV55
085500970814     C                   MOVEL     'C'           D55TLA
085600020213     C                   EXSR      FNLV55
085700000705     C**
085800000705     C                   CLEAR                   DSLV53
085900000705     C                   MOVEL     'C'           D53TLA
086000000705     C                   CALL      'FNLV53R'
086100000705     C                   PARM                    DSLV53
086200930521     C*
086300040601     c                   if        wapri='S'
086400070212     C                   CLOSE     FIBAR00F
086500040601     c                   endif
086600930521     C*
086700981105     C* SE HO ALEMNO UN FOGLIO SCRITTO--> TRASMETTO
086800111018     C***  ERF(1)        IFNE      *BLANKS
086900111018     C***                EXSR      TRASMI
087000111018     C***                ENDIF
087100040811     c*
087200040811     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
087300040811     c                   clear                   fidn12ds
087400040811     c                   eval      i12tla = 'C'
087500040811     c                   call      'FIDN12R'
087600040811     c                   parm                    fidn12ds
087700040811     c*
087800930521     C                   SETON                                        LR
087900930521     C*
088000930521   X2C                   ELSE
088100930521     C**
088200930521     C** 02 ON  - PROGRAMMA RICHIAMATO
088300930521     C**
088400921020     C* SE NON HO AGGIORNATO IMPOSTO LA "E" DI ERRORE
088500960521     C     *IN39         IFEQ      *ON
088600070221     C                   MOVEL     'E'           dls45FLG
088700960521     C                   ELSE
088800960521     C*
088900950926     C* SE AGGIORNATO FACCIO SOTTOMETTERE IL PGM DI AGGIORNAMENTO BOLLE
089000070221     C                   MOVEL     'A'           dls45FLG
089100950707     C* Se non ho aggiornato perchè spunta partenza manuale di collo
089200950707     C* consegnato imposto "N" di aggiornamento non effettuato
089300960521     C     OKAGG         IFEQ      'N'
089400070221     C                   MOVE      'N'           dls45FLG
089500950707     C                   END
089600960521     C                   ENDIF
089700070208     C                   MOVE      DLS45FLG2     FLGPRE            1
089800930521     C*
089900930521     C                   RETURN
090000930521    2C                   END
090100930521    1C                   END
090200941214     C**************************************************************************
090300941214     C* RILEVO IL FOGLIO VIAGGIO PARTENZE/ARRIVI
090400941214     C**************************************************************************
090500941214     C     RILFOG        BEGSR
090600941214     C*
090700941214     C                   Z-ADD     *ZERO         WDFV
090800941229     C                   CLEAR                   WSPG
090900021014     C                   Z-ADD     0             WFLE
091000961122     C                   CLEAR                   WTTR
091100980115     C                   CLEAR                   FFG
091200140613     C                   CLEAR                   WFCF
091300021024     c*
091400021024     c* Prendo il terminal di partenza del p.o. in gestione
091500070212    1c                   if        savfgs<>wfgs
091600021024     C                   CLEAR                   DSLV55
091700021024     C                   MOVEL     'P'           D55TPT
091800021024     C                   MOVEL     WFGS          D55LIN
091900021024     C                   MOVEL     WDATE1        D55DRF
092000021024     C                   CALL      'FNLV55R'
092100021024     C                   PARM                    DSLV55
092200021024     C**
092300070212    2C     D55ERR        IFne      ' '
092400021024     C                   MOVE      wfgs          tfpfgs
092500021024     c                   else
092600021024     C                   MOVE      d55tfp        tfpfgs
092700070212    2c                   endif
092800021024     c                   eval      savfgs=wfgs
092900070212    1c                   endif
093000941214     C*
093100941214     C* ARRIVI O PARTENZE ?
093200970114    1C     WNPG          IFEQ      1
093300941214     C*
093400021014     C     KFGV1         CHAIN     FNFGV01L                           99
093500000505     C*
093600070209    2C     *IN99         IFEQ      *OFF
093700021014     C     KFGV1         CHAIN     FNFGW01L                           28
093800070209    3C     *IN28         IFEQ      *ON
093900000505     C     FGWATB        ORNE      ' '
094000000505     C                   CLEAR                   FGWFF3
094100000505     C                   CLEAR                   FGWFF4
094200000505     C                   CLEAR                   FGWFL3
094300000505     C                   CLEAR                   FGWFL4
094400070209    3C                   ENDIF
094500980928     C                   Z-ADD     FGVDFV        WDFV
094600021024     C                   Z-ADD     FGVLNP        WFgs
094700021024     C                   Z-ADD     FGVLNP        WFLE
094800980928     C                   MOVEL     FGVTTR        WTTR
094900140613     C                   MOVEL     FGVFCF        WFCF
095000980928     C**
095100070209   X2C                   ELSE
095200960925     C     KFVA1         CHAIN     FNFVA01L                           99
095300970114    3C     *IN99         IFEQ      *OFF
095400970114     C                   Z-ADD     FVADFV        WDFV
095500021014     C                   Z-ADD     FVALNP        WFLE
095600970114     C                   MOVEL     FVATTR        WTTR
095700140613     C                   MOVEL     'S'           WFCF
095800111018   x3C**                 ELSE
095900110103     c*
096000070209     C* Se non trovato invio msg di errore
096100111018     C**                 EXSR      INVMSG
096200070212    3C                   ENDIF
096300070212    2C                   ENDIF
096400941214     C*
096500970114   X1C                   ELSE
096600941214     C*
096700021014     C* ACCEDO DIRETTAMENTE AL FOGLIO CON P.O. GESTIONE
096800021014     C* SE FOGLIO NON TROVATO CERCO CON SIMFEL- PUÒ ESSERE SOLO UN
096900070212     C*  FOGLIO IMP SOLO SE RICHIAMO PER DATI DI FIBAR!!
097000021014     C     KFVV1         CHAIN     FNFVV01L                           99
097100070209    2C     *IN99         IFEQ      *ON
097200021014     C     WNPG          ANDEQ     3
097300021014     C     RICBAR        ANDEQ     'S'
097400021014     C                   Z-ADD     SIMFEL        WFGS
097500021014     C     KFVV1         CHAIN     FNFVV01L                           99
097600021014     C  N99FVVSPG        IFNE      'P'
097700021014     C                   SETON                                        99
097800021014     C                   ENDIF
097900070209    2C                   ENDIF
098000021014     C*
098100070209    2C     *IN99         IFEQ      *OFF
098200980928     C                   Z-ADD     FVVDFV        WDFV
098300980928     C                   MOVEL     FVVSPG        WSPG
098400021014     C                   MOVEL     FVVFGS        WFGS
098500140613     C                   MOVEL     FVVFCF        WFCF
098600111018     C**                 ELSE
098700111018     C**                 EXSR      INVMSG
098800070209    2C                   ENDIF
098900941214     C*
099000970114    1C                   ENDIF
099100021014     C**
099200021024     C* SE WFLE=0 prendo TERMINAL DI PARTENZA di fgs A UDATE
099300070223     c*   di fatto si tratta del TFP di BPES poteva che può differire
099400070223     c*   dal FGS solo in caso di spunte ion procedura arrivo per p.o.
099500070223     c*   con terminal di partenza <> dal terminal arrivo
099600021014    1C     WFLE          IFEQ      0
099700021024     C                   MOVE      tfpfgs        WFLE
099800021014    1C                   END
099900941214     C*
100000941214     C                   ENDSR
100100020213     C**************************************************************************
100200020213     C*    IMPOSTAZIONI DI ALCUNI CAMPI DI PASSAGGIO SE VUOTI
100300020213     C**************************************************************************
100400020213     C     IMPO          BEGSR
100500150824     c                   eval      %subst(dls45flo:1:1)=' '
100600110103     c*
100700070208     C* BPES--> SE NON passato    E' SIMFEL
100800020213     C     BPES          IFEQ      0
100900020213     C                   Z-ADD     SIMFEL        BPES
101000070208     c                   if        btfp=0
101100070208     C                   Z-ADD     SIMFEL        BTFP
101200020213     C                   ENDIF
101300070208     C                   ENDIF
101400020213     C**
101500070208     C* BTFP--> SE NON passato ma c'è BPES, è terminal partenza di BPES
101600070208     c                   if        BTFP=0
101700020213     C                   CLEAR                   DSLV55
101800020213     C                   MOVEL     'P'           D55TPT
101900020213     C                   Z-ADD     BPES          D55LIN
102000020213     C                   Z-ADD     WDATE1        D55DRF
102100020213     C                   EXSR      FNLV55
102200110103     c*
102300020213     C                   Z-ADD     D55TFP        BTFP
102400020213     C                   ENDIF
102500110103     c*
102600070209     c* Se BPES non passato, reimposto anche il campo di input
102700070212     c                   if        DLS45PES<=*zeros
102800070209     c                   movel     bpes          dls45pes
102900070209     c                   endif
103000110103     c*
103100020213     C                   ENDSR
103200961213     C**************************************************************************
103300961213     C*    RIEMPI I CAMPI DEL RECORD DA SCRIVERE
103400961213     C**************************************************************************
103500961213     C     RIEMPI        BEGSR
103600110103     c*
103700080317     C* CONTROLLO SE E' PISTOLA MANUALE
103800080317     C                   EXSR      PISMAN
103900080317     c
104000961213     C* SE NON C'E' AGGIUNGO ALTRIMENTI AGGIORNO
104100080317    1C     WBRVE         IFEQ      'S'
104200070131     C* Se routine richiamata per scrittura di FNBRVe devo sempre clear
104300010608     c* are brvatr
104400010608     C                   CLEAR                   BRVATR
104500070201     C                   CLEAR                   BRVtsu
104600080317   x1C                   ELSE
104700961213     C   31              CLEAR                   BRVATR
104800070201     C   31              CLEAR                   BRVtsu
104900110103     c*
105000080317     c** Conteggio le spunte risparate + volte
105100080317     c** ignoro tutte le spunte manuali
105200100709     c** ignoro se spunta con errore (già caricata)
105300080422    1c                   if        not *in31  and (rps(y)<>'M' or
105400080422     c                             rps(y)='M' and tps(y)='C')
105500100709     c                             and barerr<>'E'
105600110103     c*
105700080317    2c                   if        nst(WA)<>'1'  or
105800080317     c* Se spunta unica  ma stesso numero foglio --> conto doppio
105900080317     c                             (nst(WA)= '1' and brvnfv=barnfv)
106000110103     c*
106100080630     c* Non considero doppia nemmeno se spunta cat 3 o 4 pistola 49
106200080630     c*  sparate entro i 2 sec.
106300080630     c                   eval      wok='S'
106400080630    3c                   if        (barnpg=3 or barnpg=4) and barnps=49
106500080630     c                             and brvnps=49 and brvdfs=bardfs
106600080630     c                   move      barhms        Timenew
106700080630     c                   move      brvhms        Timeold
106800080630     c     Timenew       subdur    Timeold       secondi:*s
106900080701     c                   mllzo     1             secondi
107000080630    4c                   if        secondi<=§vposecdop
107100080630     c                   eval      wok='N'
107200080630    4c                   endif
107300080630    3c                   endif
107400110103     c*
107500080630   2AC                   IF        WOK='S'
107600080317    3c                   if        brvtsu=' '
107700080317     c                   eval      brvtsu='1'
107800080317   x3c                   else
107900080317     c                   movel     brvtsu        wtsu              1 0
108000080317    4c                   if        wtsu<9
108100080317     c                   eval      wtsu=wtsu+1
108200080317     c                   movel     wtsu          brvtsu
108300080317    4c                   endif
108400110103     c*
108500080317    3c                   endif
108600080630   2Ac                   endif
108700110103     c*
108800080317   x2c                   else
108900080317     c* Prendo data foglio
109000080317    3C     BRvKEY        IFNE      SA1KEY
109100080317     C                   MOVEL     BRvKEY        SA1KEY
109200080317     C                   Z-ADD     BRvNPG        WNPG
109300080317     C                   Z-ADD     BRvNFV        WNFV
109400080317     C                   Z-ADD     BRvFGS        WFGS
109500080317     C                   movel     BRvnps        Wnps
109600080317     C                   EXSR      RILFOG
109700080317     c                   Z-ADD     WDFV          BRVDFV
109800140616     c                   movel     WFCF          BRVFCF
109900140616     c                   movel     Wspg          BRVspg
110000080317    3c                   endif
110100110103     c*
110200080317     c* Se spunta unica, foglio diverso ma stessa data -->
110300080317     c*                         mantengo TSU
110400080317     c*                                    data diversa --> lo pulisco
110500080317    3c                   if        bardfv<>brvdfv
110600080317     c                   clear                   brvtsu
110700080317    3c                   endif
110800110103     c*
110900080317    2c                   endif
111000080317    1c                   endif
111100110103     c*
111200080317    1C                   ENDIF
111300110103     c*
111400070201     C                   CLEAR                   BRVnvr
111500961213     C* FILIALE GESTIONE LA PRENDO DAL FOGLIO
111600961213     C                   CLEAR                   BRVDTR
111700961213     C                   CLEAR                   BRVFTR
111800991223     C                   CLEAR                   BRVHTR
111900961213     C                   Z-ADD     BARFGS        BRVFGS
112000961213     C                   Z-ADD     BARNPG        BRVNPG
112100990708     C                   Z-ADD     BARNFV        BRVNFV
112200040428     C* P.O. CHE HA EFFETTUATO LA SPUNTA (l'originale contenuto il WPES
112300040428     c*  e non più il bpes, per le spunte cat.1)
112400040428     C                   Z-ADD     WPES          BRVPES
112500961213     C                   MOVEL     BARLNP        BRVLNP
112600961213     C                   MOVEL     BARLNA        BRVLNA
112700961213     C                   MOVEL     BARNRS        BRVNRS
112800961213     C                   MOVEL     BARNSC        BRVNSC
112900961213     C                   Z-ADD     BARZNC        BRVZNC
113000961213     C**
113100961213     C* SE SONO IN IMMISSIONE LA AGGIORNO, IN VARIAZIONE SOLO
113200970829     C*  SE E' PISTOLA CON PRIORITA' < O UGUALE ALLA PRESENTE
113300961213     C*
113400961213     C* SE L'ANOMALIA GIA' ESISTE, NON LA TOLGO SE SU QUESTA SPUNTA
113500961213     C*  NON C'E'
113600040324    1C     *IN31         IFEQ      *ON
113700010608     C     WBRVE         OREQ      'S'
113800961213     C                   MOVEL     BARCAN        BRVCAN
113900961213     C                   MOVEL     BARNPS        BRVNPS
114000070212     C                   MOVEL     BARtap        BRVtap
114100040324     C                   Z-ADD     BARVUC        BRVVUC
114200040324     C                   Z-ADD     BARPUC        BRVPUC
114300040324   x1C                   ELSE
114400961213     C* CODICE ANOMALIA
114500961213     C     BARCAN        IFNE      ' '
114600980205     C     BRVCAN        OREQ      'E'
114700961213     C                   MOVEL     BARCAN        BRVCAN
114800961213     C                   ENDIF
114900110103     c*
115000970829     C* CERCO LA PRIORITA' DELLA PISTOLA GIA' PRESENTE
115100970829     C                   MOVEL     BRVNPS        ALF2
115200970829     C                   EXSR      RICRAG
115300970829     C* PISTOLA CON PRIORITA' < O = ALLA PRESENTE: E' + IMPORTANTE
115400970829     C     OPS(Y)        IFLE      OPS(Z)
115500961213     C                   MOVEL     BARNPS        BRVNPS
115600070212     C                   MOVEL     BARtap        BRVtap
115700961213     C                   ELSE
115800961213     C                   MOVEL     BRVNPS        BARNPS
115900070212     C                   MOVEL     BRVtap        BARtap
116000961213     C                   ENDIF
116100110103     c*
116200041011     c* Peso e volume prendo se pieno e se non c'e' già
116300041011     c                   if        barpuc>0
116400041011     c                   if        *in31 or (not *in31 and brvpuc=0)
116500040324     C                   Z-ADD     BARPUC        BRVPUC
116600040324     c                   endif
116700041011     c                   endif
116800040324     c                   if        barvuc>0
116900041011     c                   if        *in31 or (not *in31 and brvvuc=0)
117000040324     C                   Z-ADD     BARvUC        BRVvUC
117100040324     c                   endif
117200041011     c                   endif
117300110103     c*
117400040324    1C                   ENDIF
117500961213     C*
117600070212     c* Se la spunta era già stata caricata con errore (di solito
117700070212     c*  per la non creazione della anomalia 50) non aggiorno la data/ora
117800070212     c*  caricamento spunta
117900070212     c                   if        barerr<>'E' or *in31
118000961213     C                   Z-ADD     WDATE1        BRVDCS
118100070131     C                   time                    BRVHCS
118200070212     c                   endif
118300140613     c* impostazione data caricamento spunte
118400140613     c                   exsr      impoDFS
118500140613     c                   z-add     wdfs          brvdfs
118600140613     c
118700070131     c                   clear                   brvhms
118800070131     C                   movel     BARHMS        BRVHMS
118900070212     c                   z-add     barmis        brvmis
119000961213     C                   Z-ADD     BARFLE        BRVFLE
119100070212     C                   movel     BARpru        BRVpru
119200080314     c
119300080314     c
119400961213     C                   ENDSR
119500941206     C**************************************************************************
119600941206     C*    CARICO TABELLE
119700941206     C**************************************************************************
119800941229     C     CARTAB        BEGSR
119900930521     C*
120000930521     C* PARAMETRI UTENTE
120100110103     C                   Z-ADD     1             CODUT             1 0
120200110103      *
120300110103      * Reperimento dati utente/aziendali
120400110103     c                   exsr      sr_DatiJob
120500970320     C*
120600970320     C                   CLEAR                   SAVBOL
120700961212     C**
120800961212     C**  CARICO TABELLA 7J - CODICI PISTOLA
120900961122    1C     *IN02         IFEQ      '0'
121000921229     C                   MOVEL     '7J'          COD
121100990108     C                   Z-ADD     1             X                 4 0
121200921229     C     KTAB2         SETLL     TABEL
121300950927     C     KTAB2         READE     TABEL                                  99
121400921229     C*
121500961122    2C     *IN99         DOWEQ     *OFF
121600961122    3C     TBLFLG        IFEQ      ' '
121700921229     C                   MOVEL     TBLKEY        NPS(X)
121800921229     C                   MOVEL     TBLUNI        DS7J
121900921229     C                   MOVEL     §7JRPS        RPS(X)
122000000210     C                   MOVEL     §7JTPS        TPS(X)
122100000210     C                   MOVEL     §7JORD        OPS(X)
122200000316     C                   MOVEL     §7JINV        INV(X)
122300040503     C                   MOVEL     §7Jfsl        fsl(X)
122400921229     C                   ADD       1             X
122500961122    3C                   END
122600921229     C*
122700950927     C     KTAB2         READE     TABEL                                  99
122800961122    2C                   ENDDO
122900961122    1C                   ENDIF
123000961122     C**
123100961212     C** CARICO TABELLA  TV - TIPI TRAINO DEFLUENZA
123200961122     C                   MOVEL     'TV'          COD
123300990108     C                   Z-ADD     1             X
123400961122     C     KTAB2         SETLL     TABEL
123500961122     C     KTAB2         READE     TABEL                                  99
123600961122     C*
123700961122    2C     *IN99         DOWEQ     *OFF
123800961122    3C     TBLFLG        IFEQ      ' '
123900961122     C                   MOVEL     TBLUNI        DSTV
124000961122    4C     §TVDEF        IFEQ      'S'
124100961122     C                   MOVEL     TBLKEY        TTR(X)
124200961122     C                   ADD       1             X
124300961122    4C                   ENDIF
124400961122    3C                   ENDIF
124500961122     C*
124600961122     C     KTAB2         READE     TABEL                                  99
124700961122    2C                   ENDDO
124800961212     C**
124900961212     C* CARICO LA TABELLA 7N - DELLE CATEGORIE SPUNTE
125000941214     C                   MOVEL     '7N'          COD
125100941214     C                   Z-ADD     1             X
125200941214     C     KTAB2         SETLL     TABEL
125300950927     C     KTAB2         READE     TABEL                                  99
125400941214     C*
125500950927     C     *IN99         DOWEQ     '0'
125600941214     C     TBLFLG        IFEQ      ' '
125700941214     C                   MOVEL     TBLKEY        CAT(X)
125800941214     C                   MOVEL     TBLUNI        DS7N
125900941214     C                   MOVEL     §7NNST        NST(X)
126000941214     C                   ADD       1             X
126100941214     C                   ENDIF
126200941214     C*
126300950927     C     KTAB2         READE     TABEL                                  99
126400941214     C                   ENDDO
126500961212     C**
126600960702     C** CARICO TABELLA 5F - FILIALI CONFERMA BOLLE DA DISK
126700960702     C                   MOVEL     '5F'          COD
126800960702     C                   Z-ADD     0             X
126900960702     C     KTAB2         SETLL     TABEL
127000960702     C     KTAB2         READE     TABEL                                  99
127100960702     C*
127200960702    1C     *IN99         DOWEQ     '0'
127300960702    2C     TBLFLG        IFEQ      ' '
127400070201     C                   MOVEL     TBLKEY        W003A             3
127500070201     C     W003A         IFNE      '   '
127600960702     C                   ADD       1             X
127700960702     C                   MOVEL     TBLKEY        S5F(X)
127800970828     C* MEMORIZZO LA FIL CHE CONFERMA
127900970828     C                   MOVEL     TBLUNI        F5F(X)
128000110103     c*
128100130618     C     X             IFGE      9000
128200070201     C                   SETON                                        99
128300070201     C                   ENDIF
128400110103     c*
128500970828     C                   ENDIF
128600960702    2C                   ENDIF
128700960702     C*
128800960702     C  N99KTAB2         READE     TABEL                                  99
128900960702    1C                   ENDDO
129000961212     C**
129100020204     C** CARICO TABELLA 3E - CODICI ANOMALIA VALIDI SU SPUNTE
129200020204     C                   MOVEL     '3E'          COD
129300961212     C                   Z-ADD     0             X
129400961212     C     KTAB2         SETLL     TABEL
129500961212     C     KTAB2         READE     TABEL                                  99
129600961212     C*
129700961212    1C     *IN99         DOWEQ     *OFF
129800961212    2C     TBLFLG        IFEQ      ' '
129900961212     C                   ADD       1             X
130000020204     C                   MOVEL     TBLKEY        CAN(X)
130100961212     C     X             IFGE      200
130200961212     C                   SETON                                        99
130300961212     C                   ENDIF
130400961212    2C                   ENDIF
130500961212     C*
130600961212     C  N99KTAB2         READE     TABEL                                  99
130700961212    1C                   ENDDO
130800020502     C*****
130900020502     C* RILEVO DATA E ORA PER SCRIVERLO NELLE SPUNTE
131000020502     C                   TIME                    WTIME            14 0
131100020502     C                   MOVE      WTIME         WDATE             8 0
131200020502     C                   Z-ADD     WDATE         G02DAT
131300020502     C                   Z-ADD     *ZERO         G02INV
131400020502     C                   MOVEL     *BLANKS       G02ERR
131500020502     C                   CALL      'XSRDA8'
131600020502     C                   PARM                    WLBDAT
131700020502     C                   Z-ADD     G02INV        WDATE1            8 0
131800030725      *
131900030725      * CHAIN A VARIABILI PARTENZE x PRENDERE MAX PESO E VOLUME CARICABILI
132000030725     C                   EXSR      CERVPO
132100970522     C*
132200970522     C* CHAIN A VARIABILI ARRIVI PER VEDERE A QUALE LIVELLO SI CHIUDE
132300970522     C* L'IDD (LIV MAX.)
132400970522     C                   MOVEL     '7A'          COD
132500970522     C                   MOVEL(P)  '2'           KEY
132600970522     C     KTAB          CHAIN     TABEL                              99
132700970522     C  N99              MOVEL     TBLUNI        DS7A2
132800000927     C* CHAIN A GG PULIZIA PER CREAZIONE ANOMLIA 10 E 50
132900000927     C                   MOVEL     '5A'          COD
133000000927     C                   MOVEL(P)  '1'           KEY
133100000927     C     KTAB          CHAIN     TABEL                              99
133200000927     C  N99              MOVEL     TBLUNI        DS5A
133300000927     C*
133400170505     C***                MOVEL     '5A'          COD
133500170505     C***                MOVEL(P)  'PT'          KEY
133600170505     C**   KTAB          CHAIN     TABEL                              99
133700170505     C**N99              MOVEL     TBLUNI        DS5APT
133800000927     C*
133900000927     C* PREDO GG MINORI TRA PULIZIA ARB E PULIZIA ARB POSTE
134000170505     C***  §5AARB        IFLT      §5APT5
134100170505
134200000927     C                   Z-ADD     §5AARB        WGGPUL            3 0
134300170505
134400170505     C***                ELSE
134500170505     C***                Z-ADD     §5APT5        WGGPUL            3 0
134600170505     C***                ENDIF
134700000927     C**
134800000927      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA BOLLE ARRIVI
134900000927     C                   MOVE      *BLANKS       $DAT
135000000927     C                   MOVE      *BLANKS       $ERR
135100000927     C                   MOVEL     'FNLR84R'     $PGM
135200000927     C                   CALL      'TRUL49C'                            28
135300000927     C                   PARM                    $PGM             10
135400000927     C                   PARM                    $DAT              8
135500000927     C                   PARM                    $ERR              1
135600000927     C*
135700000928    3C     $ERR          IFEQ      '1'
135800000927     C     *IN28         OREQ      *ON
135900000928     C     $DAT          ORLE      *ZEROS
136000000927     C                   Z-ADD     WDATE1        G02INV
136100000927     C                   ELSE
136200000928     C                   MOVEL     $DAT          G02INV
136300000927    3C                   ENDIF
136400000927     C*
136500000927      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI
136600000927      * DI PULIZIA BOLLE ARRIVI
136700000927     C                   Z-ADD     *ZEROS        G02DAT
136800000927     C                   Z-ADD     *ZEROS        G02TGI
136900000927     C                   MOVEL     '3'           G02ERR
137000000927     C                   CALL      'XSRDA8'
137100000927     C                   PARM                    WLBDAT
137200000927      *
137300000927     C                   SUB       WGGPUL        G02TGI
137400000928     C                   Z-ADD     *ZEROS        GI8DAT
137500000928     C                   Z-ADD     *ZEROS        GI8INV
137600000928     C                   Z-ADD     G02TGI        GI8TGI
137700000927     C                   CALL      'XSRGI8'
137800000927     C                   PARM                    WGIDAT
137900000927      *
138000000928     C                   Z-ADD     GI8INV        PRIDAT
138100961212     C**
138200070209     c** udate - 4 --> sono i giorni che devo tenere le spunte dentro al
138300070212     c**               FIBAR se non riesco a creare l'anomalia collo
138400021203     c**               arrivato non part
138500021203     C                   Z-ADD     WDATE1        G02INV
138600021203     C                   Z-ADD     *ZEROS        G02DAT
138700021203     C                   Z-ADD     *ZEROS        G02TGI
138800021203     C                   MOVEL     '3'           G02ERR
138900021203     C                   CALL      'XSRDA8'
139000021203     C                   PARM                    WLBDAT
139100021203      *
139200021203     C                   SUB       4             G02TGI
139300021203     C                   Z-ADD     *ZEROS        GI8DAT
139400021203     C                   Z-ADD     *ZEROS        GI8INV
139500021203     C                   Z-ADD     G02TGI        GI8TGI
139600021203     C                   CALL      'XSRGI8'
139700021203     C                   PARM                    WGIDAT
139800021203     C                   Z-ADD     GI8INV        wpul050
139900961212     C**
140000021203     C**
140100930521     C                   MOVEL     '1'           UNO               1
140200930521     C                   MOVEL     'AA'          SAVNPS            2
140300960411     C*
140400960411     C                   MOVE      ' '           OPN9              1
140500961212     C*
140600021014     C                   CLEAR                   DSBS55
140700971113     C                   MOVEL     'L'           I50TLA
140800021014     C                   CALL      'TIBS55R'
140900021014     C                   PARM                    DSBS55
141000971113     C**
141100971113     C                   CLEAR                   KPJBA
141200021014     C                   MOVEL     'EDPCED'      WCED
141300021014     C                   MOVEL     SIMFEL        WTER
141400021014     C                   MOVEL     WPROF         KNMUS
141500971113     C                   MOVEL     O50PSI        KNSIF
141600970829     C                   MOVEL     'DSP01'       KNMTD
141700961212     C                   MOVEL     'N'           KSTJO
141800961212     C                   MOVEL     'B'           KTPAZ
141900961212     C                   MOVEL     'AZNAUTO'     KNMEB
142000961212     C                   MOVEL     'J'           KEXCN
142100961212     C                   MOVEL     'P5'          KCOJB
142200961212     C                   MOVEL     'N'           KCANC
142300000927     C*
142400000927     C                   CLEAR                   SA2DFV
142500001109     C**
142600001109     C** CARICO TABELLA 7C - ANOMALIE CREABILI DA CAT.SPUNTA GENERICA
142700001109     C                   MOVEL     '7C'          COD
142800001109     C                   Z-ADD     0             X
142900001109     C     KTAB2         SETLL     TABEL
143000001109     C     KTAB2         READE     TABEL                                  99
143100001109     C*
143200001109    1C     *IN99         DOWEQ     *OFF
143300001109    2C     TBLFLG        IFEQ      ' '
143400001109     C                   MOVEL     TBLUNI        DS7C
143500001109    3C     §7CCCG        IFEQ      'S'
143600001109     C                   ADD       1             X
143700001109     C                   MOVEL     TBLKEY        ACG(X)
143800001109    3C                   ENDIF
143900001109    2C                   ENDIF
144000001109     C*
144100001109     C     KTAB2         READE     TABEL                                  99
144200001109    1C                   ENDDO
144300110103      *
144400091209      * Aggancio tabella AR5 per record 'GEN'
144500091209     c                   Clear                   dAr5
144600091209     c                   Clear                   dsbs02
144700091209     c                   Eval      T02Mod = 'C'
144800091209     c                   Eval      T02Sif = knsif
144900091209     c                   Eval      T02Cod = 'AR5'
145000091209     c                   Movel(p)  'GEN'         T02Ke1
145100091209     c                   Call      'TIBS02R'
145200091209     c                   Parm                    Kpjba
145300091209     c                   Parm                    dsbs02
145400091209     c                   If        T02Err = *Blanks
145500091209     c                   Movel     T02Uni        dAr5
145600091209     c                   EndIf
145700170125     C*
145800170125     C* CARICO TABELLA DECOFIIMG PER le filiali con New IMA
145900170125     c                   clear                   AllNewIMA         1
146000170125     c                   clear                   trulvpods
146100170125     c                   eval      ivpoke1 = 'DECOFIIPG'
146200170125     c                   call      'TRULVPOR'
146300170125     c                   parm                    trulvpods
146400170125     c                   if         %lookup('999':skFilOk)>0
146500170125     C                   EVAL       AllNewIMA='S'
146600170125     c                   endif
146700170125
146800911014     C                   ENDSR
146900941206     C**************************************************************************
147000941206     C* CREO ANOMALIA
147100941206     C**************************************************************************
147200911016     C     CRTANM        BEGSR
147300000320     C                   CLEAR                   WCCH
147400970521     C*
147500970521     C                   CLEAR                   WANM10
147600941206     C*
147700970204    1C     COMCAA        IFNE      645
147800990518     C     KANM          CHAIN     FNANM000                           96
147900970204   X1C                   ELSE
148000961205     C*
148100990518     C     KANM          CHAIN(N)  FNANM000                           96
148200961205     C* PER L'ANOMALIA 645 LEGGO FINTANTO CHE NON TROVO UNA ANOMALIA
148300961205     C*  CON DATA = ALLA DATA SPUNTA: SE C'E' NON CREO
148400970204    2C     *IN96         DOWEQ     *OFF
148500990518     C     ANMDAO        ANDNE     BARDFV
148600990518     C     KANM          READE(N)  FNANM000                               96
148700970204    2C                   ENDDO
148800961205     C*
148900970204    1C                   ENDIF
149000000811     C*
149100000811    1C     *IN96         IFEQ      *ON
149200000811     C*
149300000811     C     COMCAA        ORNE      645
149400000811     C     ANMAIE        ANDEQ     'I'
149500000811     C* PER ORA SOLO SULLE INTERNE RICREO ANOMLIA SE PRESENTE
149600970204     C*  - SE CHIUSE SEMPRE
149700040923     C*  - SE APERTA SOLO LA DATA E' < DELLA ESISTENTE
149800040923     c* Per la 200 la ricreo solo se e' chiusa senza guardare la data creaz
149900961205     C*
150000970204     C                   MOVEL     'S'           WWRITE
150100970204    2C     *IN96         IFEQ      *OFF
150200970204     C     ANMDCH        ANDEQ     0
150300970204     C     ANMDAO        ANDGE     BARDFV
150400041005    2C     *IN96         oreq      *OFF
150500041005     C     ANMDCH        andeq     0
150600040923     C     comcaa        ANDeq     200
150700970204     C                   MOVEL     ' '           WWRITE            1
150800970204    2C                   ENDIF
150900970204     C**
151000070215    2C     WWRITE        IFEQ      'S'
151100990518     C                   CLEAR                   FNANM000
151200950927     C                   EXSR      RIEANM
151300000614     C**
151400001109     C* CREO SOLO SE ANOMALIA :
151500001109     C* CREABILE DA QUALUNQUE CATEGORIA SPUNTA
151600070215    3C     §7CCCG        ifeq      'S'
151700001109     C* NON CREABILE DA CATEG. GENERICA E LA CATEGORIA NON È GENERICA
151800070215     C     BARSPG        orne      'I'
151900950927     C*
152000950927     C                   MOVE      COMLNP        ANMLNP
152100990518     C                   Z-ADD     COMAAS        ANMAAS
152200950927     C                   MOVE      COMNSP        ANMNSP
152300960524     C                   MOVE      BARDFV        ANMDAO
152400990518     C                   Z-ADD     BARNFV        ANMNFV
152500950927     C                   MOVEL     BARFGS        ANMCDU
152600040621     c* imposto se spedizione di valore
152700070215    4c                   if        anmnsp>0
152800091209     c     kar5          chain(n)  fiar501l
152900070215    5c                   if        %found(fiar501l)
153000040621     c                   movel     ar5uni        dar5gen
153100040621     c                   else
153200040621     c                   clear                   dar5gen
153300070215    5c                   endif
153400040621     c                   movel     §ar5bva       anmft4
153500070215    4c                   endif
153600110103     c*
153700040429     c* 22/4/4 --> al posto di anmfgs imposto BPES il p.o. che spara
153800040429     c*            che solo per le spunte partenza è sempre barfgs
153900040429     C                   MOVEL     bpes          ANMFLE
154000070215    4C     ANMCAA        IFEQ      10
154100150205    4C     ANMCAA        oreq      410
154200070215     C                   z-add     BARZNC        ANMfl4
154300070215    4C                   ENDIF
154400911112     C*
154500921020     C* SE ESTERNA SCRIVO FIL A CUI TRASMETTERE
154600070215    4C     §7CAIE        IFEQ      'E'
154700070215    5C     COMLNP        IFGT      0
154800960528     C                   MOVEL     COMLNP        ANMFL1
154900960528     C                   ELSE
155000960704     C                   MOVEL     LNPB          ANMFL1
155100070215    5C                   ENDIF
155200070215    4C                   ENDIF
155300911016     C*
155400970204     C* SE INTERNA AUTOCHIUSA E C'E' GIA' NON FACCIO NULLA
155500070215    4C     *IN96         IFEQ      *ON
155600970204     C     §7CCHA        ORNE      'S'
155700960430     C* SE LA KEY E' PER SEGNACOLLO ED IL NUMERO DI SEGNACOLLO E' 0
155800960430     C*  NON CREO L'ANOMALIA (ERRORE DA TERMINALINO)
155900070215    5C     §7CKEY        IFNE      'S'
156000960430     C     §7CKEY        OREQ      'S'
156100960430     C     ANMSCN        ANDGT     0
156200990922     C*
156300990922     C* LA DATA I APERTURA ANOMALIA CI DEVE ESSERE , ALTRIMENTI
156400990922     C*  E' MEGLIO NON CREARE L'ANOMALIA
156500070215    6C     ANMDAO        IFGT      0
156600990518     C   96              WRITE     FNANM000
156700990518     C  N96              UPDATE    FNANM000
156800070215    6C                   ENDIF
156900070215    5C                   ENDIF
157000961205     C*
157100970204     C* SE CREO ANOAMLIA 145 O 146 CHIUDO LE ANOMALIE INTERNE 140 E 130
157200070215    5C     ANMCAA        IFEQ      145
157300970204     C     ANMCAA        OREQ      146
157400970204     C                   Z-ADD     130           COMCAA
157500970204     C                   EXSR      CHIUAN
157600970204     C                   Z-ADD     140           COMCAA
157700970204     C                   EXSR      CHIUAN
157800070215    5C                   ENDIF
157900070215    4C                   ENDIF
158000970204     C*
158100070215    3C                   ENDIF
158200070215    2C                   ENDIF
158300970521   X1C                   ELSE
158400110103     c*
158500970521     C* OPERAZIONI PER ANOMALIA 10 GIA' ESISTENTE MA CHIUSA:
158600970521    2C     COMCAA        IFEQ      10
158700970522     C* Se anomalia 10 a liv massimo --> memorizzo per creaz.anom.410
158800970522    3C     ANMLID        IFEQ      §7ALCI
158900970521     C                   MOVE      '1'           WANM10            1
159000970522    3C                   END
159100970522     C* Se anomalia chiusa (non pratica IDD):
159200970522     C* 1) sflaggo e chiudo l'anomalia mettendo cch='PR' se è a liv max
159300970522    3C     ANMDCH        IFGT      *ZEROS
159400970522     C     ANMCCH        ANDNE     'PR'
159500970522     C*
159600970522    4C                   SELECT
159700970522     C     ANMLID        WHENEQ    §7ALCI
159800970522     C     ANMCCH        ANDEQ     'AN'
159900970522     C                   MOVEL     'PR'          ANMCCH
160000970522     C                   MOVEL     'D'           ANMFSC
160100970522     C                   CLEAR                   ANMFT1
160200970522     C                   CLEAR                   ANMFT2
160300970522     C     ANMLID        WHENLT    §7ALCI
160400970523     C                   CLEAR                   ANMCCH
160500970522     C                   CLEAR                   ANMDCH
160600970522     C                   CLEAR                   ANMFSC
160700970522    4C                   ENDSL
160800990518     C                   UPDATE    FNANM000
160900970522    3C                   ENDIF
161000970522    2C                   ENDIF
161100970204    1C                   ENDIF
161200970204     C*
161300990518     C                   UNLOCK    FNANM02L
161400911016     C                   ENDSR
161500960613     C**************************************************************************
161600960613     C*    CONTROLLO IL RAGGRUPPAMENTO DELLA PISTOLA
161700960613     C**************************************************************************
161800960613     C     PISMAN        BEGSR
161900960613     C*
162000961213     C                   Z-ADD     1             Y                 3 0
162100961213     C*
162200960613    1C     *IN02         IFEQ      *OFF
162300960613     C     BARNPS        LOOKUP    NPS(Y)                                 34
162400960613     C  N34              Z-ADD     1             Y
162500960613     C*
162600960613   X1C                   ELSE
162700960613     C*
162800960613     C                   MOVEL     '7J'          COD
162900960613     C                   MOVEL(P)  BARNPS        KEY
163000960613     C     KTAB          CHAIN     TABEL                              34
163100000316     C     *IN34         IFEQ      *OFF
163200000316     C                   MOVEL     TBLUNI        DS7J
163300000316     C                   MOVEL     §7JRPS        RPS(Y)
163400000316     C                   MOVEL     §7JORD        OPS(Y)
163500000316     C                   MOVEL     §7JTPS        TPS(Y)
163600000316     C                   MOVEL     §7JINV        INV(Y)
163700040503     C                   MOVEL     §7Jfsl        fsl(Y)
163800000316     C                   ELSE
163900000316     C                   MOVEL     'M'           RPS(Y)
164000000316     C                   MOVEL     999           OPS(Y)
164100000316     C                   MOVEL     'B'           TPS(Y)
164200000316     C                   MOVEL     ' '           INV(Y)
164300040503     C                   MOVEL     ' '           fsl(Y)
164400960613    1C                   END
164500000316    1C                   END
164600960613     C                   ENDSR
164700941206     C**************************************************************************
164800020213     C*    A G G I O R N O    R E C O R D    B O L L E
164900941206     C**************************************************************************
165000980212     C     UPDARB        BEGSR
165100960521     C                   CLEAR                   WNOTRA
165200961210     C                   CLEAR                   W60X
165300961211     C                   CLEAR                   WESAN
165400961210     C                   MOVEL     'S'           W620
165500961211     C                   MOVEL     'S'           WCREA
165600021106     C                   CLEAR                   WDCM
165700021106     C                   CLEAR                   WBOAR
165800021106     C                   CLEAR                   WATDCM
165900040430     C                   CLEAR                   WABCCA
166000040430     C                   CLEAR                   WABNDC
166100161007     C                   CLEAR                   gcpdgc            8 0
166200021106     C                   MOVEL     ' '           WGIAC             1
166300941206     C*
166400921229     C* TROVO TIPO E RAGGRUPPAMENTO PISTOLE DI QUELLA CARICATA
166500960613     C                   EXSR      PISMAN
166600921229     C*
166700960523     C* CARICO LA L6 DELLA FILIALE GESTIONE
166800020213     C     BPES          IFNE      SAVPES
166900020213     C                   MOVEL     BPES          SAVPES
167000961209     C                   EXSR      CARL6
167100960523     C                   ENDIF
167200921013     C*
167300041012     C                   SETOFF                                         0332
167400950922     C                   CLEAR                   COMLNP
167500950922     C                   CLEAR                   COMAAS
167600950922     C                   CLEAR                   COMNSP
167700950922     C                   CLEAR                   WSPLOC
167800950927     C                   CLEAR                   WTIBOL
167900970423     C                   CLEAR                   WWFVC
168000020124     C                   CLEAR                   LNPB
168100020124     C                   CLEAR                   WTAB5F
168200020131     C                   CLEAR                   WESART
168300020131     C                   CLEAR                   WESBLT
168400020131     C                   CLEAR                   WESBTT
168500130228     c                   clear                   bolxco
168600160421     c                   clear                   bolznc
168700170509     c                   clear                   boldbr
168800021022     c*
168900021125     C* TERMINAL partenza DI BPES
169000021125     C                   MOVEL     'P'           WTITER
169100021022     C                   MOVEL     BARDFV        WDTRIF
169200021022     C                   EXSR      TERPES
169300021022     C                   Z-ADD     D55TFP        PESTFP
169400021203     c*
169500040422     c* Se trovo la bolla transito x p.o. spunta --> aggiorno transito, senz
169600021203     c*  controllo i terminal (c'e' un problema a livello di eccezioni ter
169700021203     C     KBAR37        CHAIN     FNBTT37L                           3739
169800021203     C   39              GOTO      ENDUPD
169900021203     C  N37KBTT          CHAIN     FNBTP11L                           37
170000021203     C**
170100021203    1C     *IN37         IFEQ      *OFF
170200021203     C                   MOVEL     'S'           WESBTT            1
170300021203     C                   MOVEL     BTTLNP        LNPB
170400021203     C                   MOVEL     BTPDSP        DSPB
170500021203     C                   MOVEL     BTPDSP        WDTRIF
170600021203     C                   MOVEL     BTPTFP        LNPTFP
170700050603     C                   MOVEL     BTPTFa        bolTFa
170800170509     C                   MOVE      BTPdbr        boldbr
170900040621     C                   MOVE      BTTLNP        COMLNP
171000040621     C                   MOVE      BTTAAS        COMAAS
171100040621     C                   MOVE      BTTNSP        COMNSP
171200021203   X1C                   ELSE
171300021203     C                   MOVEL     'N'           WESBTT
171400020124     C**
171500020124     C** VEDO SE TROVO LA BOLLA IN BLT
171600020124     C     KBAR          CHAIN     FNBLT27L                           3239
171700020213     C   39              GOTO      ENDUPD
171800020213     C  N32KBLT          CHAIN(N)  FNBLP01L                           32
171900020124     C**
172000021203    2C     *IN32         IFEQ      *OFF
172100020131     C                   MOVEL     'S'           WESBLT            1
172200020124     C                   MOVEL     BLTLNP        LNPB
172300020201     C                   MOVEL     BLPTFP        LNPTFP
172400050603     C                   MOVEL     BLPTFa        bolTFa
172500020201     C                   MOVEL     BLPDSP        WDTRIF
172600020214     C                   MOVEL     BLPDSP        DSPB
172700130301     c                   movel     BLPxco        bolxco            1
172800160421     c                   movel     BLPznc        bolznc            2
172900170509     C                   MOVE      BLPdbr        boldbr
173000040621     C                   MOVE      BLTLNP        COMLNP
173100040621     C                   MOVE      BLTAAS        COMAAS
173200040621     C                   MOVE      BLTNSP        COMNSP
173300021203   X2C                   ELSE
173400020213     C                   MOVEL     'N'           WESBLT
173500020124     C** SE NON TROVO LA BOLLA PARTENZA CERCO NEGLI ARRIVI
173600020124     C     KBAR          CHAIN     FNART27L                           3239
173700020213     C   39              GOTO      ENDUPD
173800050223     C  N32KARB          CHAIN(N)  FNARB01L                           32
173900020124     C**
174000021203    3C     *IN32         IFEQ      *OFF
174100020201     C                   MOVEL     'S'           WESART            1
174200020124     C                   MOVEL     ARTLNP        LNPB
174300020214     C                   MOVEL     ARBDSP        DSPB
174400020201     C                   MOVEL     ARBDSP        WDTRIF
174500020201     C                   MOVEL     ARBTFP        LNPTFP
174600050603     C                   MOVEL     ARBTFa        boltfa
174700130301     c                   movel     ARBxco        bolxco            1
174800160421     c                   movel     ARBznc        bolznc
174900170509     C                   MOVE      ARBdbr        boldbr
175000040621     C                   MOVE      ARTLNP        COMLNP
175100040621     C                   MOVE      ARTAAS        COMAAS
175200040621     C                   MOVE      ARTNSP        COMNSP
175300021203   X3C                   ELSE
175400020213     C                   MOVEL     'N'           WESART
175500020131     C** IMPOSTO LNPB = BARLNP
175600020131     C                   MOVEL     BARLNP        LNPB
175700020214     C                   Z-ADD     BARDFV        DSPB
175800050603     c                   clear                   boltfa
175900020124     C**
176000020124     C** SE NON TROVATA MAI LA BOLLA, CERCO SE ESISTE LA EVENTUALE
176100020124     C*   SPUNTA PARTENZA E LA TABELLA 5F
176200020205    4C     BARNRS        IFGT      *ZEROS
176300020124     C                   EXSR      RICLNP
176400020213    4C                   ENDIF
176500021105     c* prendo terminal di partenza della linea partenza calcolata
176600070222     c                   if        bardfv=0
176700021105     C                   Z-ADD     BARDFV        WDTRIF
176800070222     c                   else
176900070222     c                   z-add     wdate1        wdtrif
177000070222     c                   endif
177100070222     c
177200021105     C                   EXSR      TERPAR
177300020213    3C                   ENDIF
177400020213    2C                   ENDIF
177500020213    1C                   ENDIF
177600020204     C**
177700020215     C** CONTROLLO TRA P.O. SPUNTA E LINEE SPUNTE CHE AGGIONAMENTI
177800020213     C**  POSSO FARE
177900021125     c*
178000050215     C* TERMINAL arrivo DI BPES
178100021125     C                   MOVEL     'A'           WTITER
178200021125     C                   MOVEL     BARDFV        WDTRIF
178300021125     C                   EXSR      TERPES
178400021125     C                   Z-ADD     D55TFA        PESTFA
178500021125     c
178600020204     C                   EXSR      CTRAGG
178700020213     C**
178800020213     C** A G G I O R N A M E N T O   B O L L E   P A R T E N Z A
178900020213    1C     WAGPAR        IFEQ      'S'
179000021203    2C     WESblt        IFEQ      ' '
179100021203     C     KBAR          CHAIN     FNBLT27L                           3239
179200021203     C   39              GOTO      ENDUPD
179300021203     C  N32              MOVEL     'S'           WESBLT            1
179400021203     C   32              MOVEL     'N'           WESBLT            1
179500021203    2C                   ENDIF
179600021203     c**
179700021203    2C     WESBLT        ifeq      'S'
179800150817     c* Aggiorno solo se la bolla NON parte da + filiali
179900150817     c* oppure sel la bolla per LINEA parte da + filiali ma è mia bolla
180000150817     c* oppure se chi spara è la linea partenza della bolla che parte da + fil
180100150622     c                   if        wbsptp=' ' or wbsptp='L'
180200150622     c                             or bpes=lnpb
180300020213     C                   EXSR      AGGBLT
180400150622     c                   else
180500150622     c                   clear                   wagPAR
180600150622     c                   endif
180700150622     c
180800021203    3C     WEND          IFEQ      '1'
180900020213     C                   GOTO      ENDUPD
181000021203    3C                   ENDIF
181100021203    2C                   ENDIF
181200020213     C*
181300021203    2C     WESBLT        ifeq      'N'
181400020204     C                   MOVEL     'N'           WBOPAR            1
181500020204     C                   MOVEL     'P'           WTIBOL            1
181600021203    2C                   ENDIF
181700021203    1C                   ENDIF
181800020213     C*
181900020213    1C     WAGPAR        IFNE      'S'
182000020213     C     WESBLT        ANDEQ     'S'
182100020213     C                   UNLOCK    FNBLT27L
182200020213    1C                   ENDIF
182300020213     C**
182400020213     C* A G G I O R N O   B O L L E   A R R I V I
182500020213     C* A PRESCINDERE DAL FATTO CHE TROVI O MENO LA BOLLA
182600020213    1C     WAGARR        IFEQ      'S'
182700020220     C* LEGGO LE BOLLE ARRIVI ANCHE PER FLAG=6.
182800020220     C*  SI TRATTA DI SPUNTA DI DISGUIDO PER CUI NON AGGIORNEREBBE
182900020220     C*  LA BOLLA ARRIVI MA MI SERVE PER ENTRARE IN CTRA60
183000070221    1C     dls45FLG      OREQ      '6'
183100020213     C* SE NON HO ANCORA CHAINATO IL DETTAGLIO COLLI LO FACCIO ORA
183200020213    2C     WESART        IFEQ      ' '
183300020213     C     KBAR          CHAIN     FNART27L                           3239
183400020218     C   39              GOTO      ENDUPD
183500020213     C  N32              MOVEL     'S'           WESART            1
183600020214     C   32              MOVEL     'N'           WESART            1
183700020213    2C                   ENDIF
183800020213     C*
183900020213     C                   EXSR      AGGART
184000020213    1C                   ENDIF
184100020213     C*
184200020213    1C     WAGARR        IFNE      'S'
184300020213     C     WESART        ANDEQ     'S'
184400020213     C                   UNLOCK    FNART27L
184500020213    1C                   ENDIF
184600040428     c*
184700040428     c*Chiudo eventuale anomalia di disguido presente sul collo, se
184800040428     c*  di area <> dal'anomalia
184900040503     C* SOLO SE SPUNTA DI PISTOLA REALE
185000040503     c                   if        fsl(y)<>'F'
185100040428     c                   eval      comcaa=200
185200040428     C*
185300040503     C     Kanm          chain(N)  FNANM000
185400040428     c                   if        %found(fnanm02l)
185500040428     C                   CLEAR                   DSLV55
185600040428     C                   MOVEL     'P'           D55TPT
185700040428     C                   Z-ADD     anmfle        D55LIN
185800040428     C                   Z-ADD     bardfv        D55DRF
185900040428     C                   EXSR      FNLV55
186000040428     c                   if        pestfp<>d55tfp  or
186100040428     c                             bpes=anmlna
186200040428     c                   exsr      chiuan
186300040428     c                   endif
186400040428     c                   endif
186500040503     c                   endif
186600921013     C**
186700960531     C** T R A N S I T I  /  D I S G U I D I   I N   A R E A   E  N O N
186800921013     C**
186900070221    0C     dls45FLG      IFNE      '6'
187000911112     C*
187100960522     C* NON E' UNA SPUNTA DEL TERMINAL DI ARRIVO
187200050603    1C     Usalnatfa     IFNE      BPES
187300021022     c     wesbtt        oreq      'S'
187400960522     C*
187500960522     C* SE NEMMENO I 2 TERMINAL DI ARRIVO CORRISPONDONO
187600960522     C* --> T R A N S I T O
187700050603    2C     Usalnatfa     IFNE      PESTFA
187800021022     c     wesbtt        oreq      'S'
187900960531     C**
188000020214     C** I TRANSITI SONO ANCORA DA SISTEMARE
188100020214     C**  QUESTE 2 SPECIFICHE SONO DA ELIMINARE: INFATTI LA BOLLA POTRA
188200020214     C**  ESSERE SIA IN BLT CHE IN BTT
188300020205    3C     WAGPAR        IFEQ      ' '
188400960703     C     WBOPAR        OREQ      'N'
188500960522     C**
188600040422     C     WESBTT        ifeq      'N'
188700960521     C**
188800960612     C* SE E' SPUNTA ENTRATA O PARTENZA E NON HO TROVATO NE' LA BOLLA
188900960521     C*  PARTENZA NE' LA BOLLA TRANSITO, PER SERIE =0
189000960521     C*  IMPOSTO NELLA SPUNTA FLAG TRASMISSIONE "N" E 99999999
189100960612     C*  PER NON AUTOGENERARE LA SPUNTA PARTENZA O NON TRASMETTERLA
189200001123     C* IDEM PER SPUNTA IMI (PER BOLLE POSTE AUTOGENERA ANCHE DA SPUNTE
189300001123     C* IMI)
189400960522    5C     BARNPG        IFEQ      5
189500020205     C     BARNPG        OREQ      1
189600001123     C     BARNPG        OREQ      3
189700001123     C     BARSPG        ANDEQ     'I'
189800020205    6C     BARNRS        IFEQ      0
189900020205     C     WAGPAR        ANDEQ     'S'
190000960521     C     WBOPAR        ANDEQ     'N'
190100960521     C                   MOVEL     'S'           WNOTRA            1
190200020205    6C                   ENDIF
190300960612    5C                   ENDIF
190400040120     c
190500911016     C*
190600110124     C* SE NON E' BOLLA LOCALE e nemmeno in £6 : DISGUIDO
190700150817     c*  non è disguido nemmeno se collo che parte da altra filiale
190800150817     c*  ( la bolla deve essere spostata di lnp)
190900020205    5C     WAGPAR        IFEQ      ' '
191000110124    5C     WAGarr        andeq     ' '
191100150817    5C     WBSPSPU       andeq     ' '
191200150817     c
191300020215     C     BARFLE        IFEQ      BTFP
191400960522     C                   Z-ADD     200           COMCAA
191500960404     C  N05              EXSR      CRTANM
191600020215     C                   ENDIF
191700020215     C* DEVO COMUNQUE DIRE CHE SI TRATTA DI UN DISGUIDO
191800960404     C                   MOVEL     'D'           WTIBOL
191900041012     c
192000041012     c* Aggiorno anche la bolla in  arrivo
192100041012     c                   EXSR      AGGARRIVO
192200960522    5C                   ENDIF
192300960522     C**
192400911016     C* 37 OFF - TROVATA AGGIORNO BOLLA TRANSITO
192500960522   X4C                   ELSE
19260002021418   C                   EXSR      AGGBTT
192700960522    4C                   ENDIF
192800040430     c*
192900040430     c* Eventuale spunta di disguido o transito, chiude fuori posto
193000040430     c*  di bolla partenza
193100040430     C                   Z-ADD     120           COMCAA
193200040430     C                   EXSR      CHIUAN
193300970204     C*
193400970204   X3C                   ELSE
193500970204     C* SE NON E' COLLO DELLA AREA DI ARRIVO E COLLO IN PARTENZA
193600970204     C*  VEDO SE CREARE L'ANOMALIA FUORI POSTO BOLLA IN PARTENZA
193700970204     C                   EXSR      ANM120
193800990120     C** VERIFICO LA PRESENZA DI UNA C.A. PER CREARE EVENTUALI ANOM
193900000523     C** IGNORO SE E' SPUNTA CAT 4-8 SOLO IN ARRIVO(IN PART.SEMPRE)
194000990121     C                   MOVEL     'S'           WCAT4             1
194100990126     C*     GESTISCO IL RITROVAMENTO COLLO
194200990126     C                   MOVEL     'S'           WRITRO            1
194300990126     C*     GESTISCO LA CREAZIONE ANOMALIE
194400990126     C                   MOVEL     'S'           WCREAN            1
194500990120     C                   EXSR      CTRDAN
194600990120     C**
194700990120     C** SE NON TROVATA CA DI COLLO
194800990120     C* VERIFICO SE LA SPEDIZIONE E' SU UNA CA CHIUSA --> CREO
194900990120    4C     WTRDAN        IFEQ      ' '
195000040811      *
195100040811      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
195200040811      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
195300040811      * legate alla spedizione che passo
195400040811     c                   clear                   fidn12ds
195500170505     c***                eval      i12aas = BLTAAS
195600170505     c***                eval      i12lnp = BLTLNP
195700170505     c***                eval      i12nrs = BLTNRS
195800170505     c***                eval      i12nsp = BLTNSP
195900170505     c* per evitare che i campi siamo sporchi, uso i salvati
196000170505     c                   eval      i12aas = comAAS
196100170505     c                   eval      i12lnp = comLNP
196200170505     c                   eval      i12nrs = barNRS
196300170505     c                   eval      i12nsp = comNSP
196400170505     c                   eval      i12fel = 'E'
196500040811     c*
196600040811     c                   call      'FIDN12R'
196700040811     c                   parm                    fidn12ds
196800040811     c*
196900040811     c                   setoff                                       33
197000040811     c*
197100040811     c* se non ci sono errori
197200040811     c                   if        o12err <> ' '
197300040811     c                   seton                                        33
197400040811     c                   else
197500040811     c* se numero di CA trovate maggiore di zero
197600040811     c                   if        o12nca = 0
197700040811     c                   seton                                        33
197800040811     c                   else
197900040811     c                   z-add     1             jj                2 0
198000040811     c                   dow       jj <= o12nca and *in33 = *off
198100040811     C     skDch(jj)     IFGT      0
198200040811     C*
198300040811     C* Valorizzo i campi DCT* utilizzati nella routine CRE151 con quelli
198400040811     C* ritornati dal *pgm d utilità FIDN12R
198500040811     C                   movel     skCch(jj)     DCTCCH
198600040811     C*
198700040811     C                   EXSR      CRE151
198800040811     C                   SETON                                        33
198900040811     C                   ENDIF
199000040811     c*
199100040811     c                   add       1             jj
199200040811     c                   enddo
199300040811     c*
199400040811     c                   seton                                        33
199500040811     c*
199600040811     c                   endif
199700040811     c                   endif
199800040811     C**
199900990120    4C                   ENDIF
200000990120     C**
200100960531    3C                   ENDIF
200200960522     C*
200300960531   X2C                   ELSE
200400970203     C*  D I S G U I D O   I N   A R E A
200500970203     C*  (SE NON E' UNA MIA BOLLA PARTENZA)
200600961021     C                   SETOFF                                       88
200700961122     C                   CLEAR                   WSTESS
200800961028     C*
200900020213    3C  N05BPES          IFNE      BARLNA
201000050930     c* al posto di BPES uso barfgs per le spunte IMP: mi fa più comodo
201100050930     c*  usare direttamente il terminal di partenza di chi spara perchè
201200050930     c*  l'IMP lo fa solo il terminal di partenza ma può essere usato
201300050930     c*  anche dai 2 livello
201400050930     C     barfgs        ANDNE     LNPB
201500960522     C     BARNPG        ANDGT     1
201600050930     C     BARNPG        ANDne     5
201700050930     c* Se spunta entrata escludo se di tratta di un aggiornamento
201800050930     c*  in partenza e non in arrivo (per cui il p.o. fa parte
201900050930     c*  dell'area di partenza
202000050930    3C     BPES          orne      BARLNA
202100050930     C     barfgs        ANDNE     LNPB
202200050930     C     BARNPG        ANDeq     5
202300050930     c     wagarr        andeq     'S'
202400050930     c
202500980727     C** CREO ANOMALIE SE COLLO SPUNTATO IN IMA ANCHE SE DA ME CONFERMA
202600020213    3C     BPES          ORNE      BARLNA
202700020213     C     BPES          ANDEQ     LNPB
202800980727     C     BARNPG        ANDEQ     3
202900980727     C     BARSPG        ANDEQ     'A'
203000960523     C*
203100020213     C* NON DEVE ESSERE PRESENTE NEMMENO IN L6 DEL P.O. SPUNTA
203200960523     C     BARLNA        LOOKUP    L6                                     88
203300980127     C**
203400960531    4C     *IN88         IFEQ      *OFF
203500961202     C                   Z-ADD     146           CAACRE
203600961202     C                   Z-ADD     145           CAACIU
203700961202     C                   EXSR      ANM14X
203800960531    4C                   ENDIF
203900960531    3C                   ENDIF
204000960522     C*
204100960523     C* ALTRIMENTI  LA CHIUDO
204200960531    3C     *IN88         IFEQ      *ON
204300960523     C     BARNPG        OREQ      1
204400020213     C     BPES          OREQ      BARLNA
204500961122     C*
204600000523     C* SE CHI SPARA E' LA FILIALE FINALE DI ARRIVO, ME LO MEMORIZZO
204700961122     C*  MI SERVE PER LA CHIUSURA
204800961122    4C     *IN88         IFEQ      *ON
204900020213     C     BPES          OREQ      BARLNA
205000961122     C                   MOVEL     'S'           WSTESS
205100961122    4C                   ENDIF
205200961122     C**
205300960522     C                   Z-ADD     146           COMCAA
205400960522     C                   EXSR      CHIUAN
205500961122     C*
205600960522     C                   Z-ADD     145           COMCAA
205700960522     C                   EXSR      CHIUAN
205800970204     C*
205900970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
206000970204     C                   EXSR      ANMARR
206100960531    3C                   ENDIF
206200961021     C*
206300970203     C* CREO E/O CHIUDO LE ANOMALIE CHE RIGUARDANO L'AREA DI ARRIVO
206400970203     C                   EXSR      ANMSEM
206500960531    2C                   ENDIF
206600911016     C*
206700960531   X1C                   ELSE
206800970203     C* T E R M I N A L   D I   A R R I V O
206900961122     C                   SETOFF                                       88
207000961122     C                   CLEAR                   WSTESS
207100970203     C*
207200970203     C* CREO E/O CHIUDO LE ANOMALIE CHE RIGUARDANO L'AREA DI ARRIVO
207300970203     C                   EXSR      ANMSEM
207400970204     C*
207500970204     C* CHIUDO EVENTUALE ANOMALIA BOLLA ARRIVI
207600970204    2C     BARNPG        IFEQ      2
207700970204     C                   Z-ADD     130           COMCAA
207800970204     C                   EXSR      CHIUAN
207900970204    2C                   ENDIF
208000940330     C*
208100940330     C* SONO TERMINAL DI ARRIVO-ANOMALIA:INOLTRARE COLLO ALL'ARRIVO
208200960531    2C     WTIBOL        IFNE      'A'
208300020218    2C     WTIBOL        ANDNE     'L'
208400960522     C                   MOVEL     'R'           WTIBOL
208500960531    2C                   ENDIF
208600960522     C*
208700960522     C* SOLO SE NON E' UN MIO COLLO
208800020213    2C  N05BPES          IFNE      BARLNA
208900091211     c
209000091211     C* CARICO LA L6 DELLA FILIALE GESTIONE
209100091211     C     BPES          IFNE      SAVPES
209200091211     C                   MOVEL     BPES          SAVPES
209300091211     C                   EXSR      CARL6
209400091211     C                   ENDIF
209500091211     c
209600960523     C* NON DEVE ESSERE PRESENTE NEMMENO IN L6
209700960531     C     BARLNA        LOOKUP    L6                                     88
209800960531    3C     *IN88         IFEQ      *OFF
209900960523     C*
210000960531    4C     BARNPG        IFGT      2
210100960522     C     BARNPG        ANDLT     5
210200000523     C     BARNPG        OREQ      8
210300961202     C                   Z-ADD     145           CAACRE
210400961202     C                   Z-ADD     146           CAACIU
210500961202     C                   EXSR      ANM14X
210600960522     C*
210700960531   X4C                   ELSE
210800961211     C* SE TROVO LA 145 E LA POSSO CHIUDERE CREO SE POSSO LA 620
210900961211     C*  SE NON TROVO LA 145 DEMANDO L'EVENTUALE CREAZIONE DELLA 620
211000961211     C*  ALLA CHIUSURA DELLA EVENTUALE 146, CHE POTREBBE ANCHE NON
211100961211     C*  ESSERCI. SIA CHE CI SIA DA CHIUDERE CHE NON CI SIA, A QUESTO
211200961211     C*  PUNTO LA 620 LA CREO
211300960522     C                   Z-ADD     145           COMCAA
211400961211     C* SOLO SE ESISTE L'ANOMALIA CREO LA 620
211500961211     C                   MOVEL     'S'           WESAN             1
211600961211     C*
211700960522     C                   EXSR      CHIUAN
211800961211     C                   CLEAR                   WESAN
211900961029     C* SE C'E' CHIUDO ANCHE IL DISGUIDO IN AREA
212000961029     C                   Z-ADD     146           COMCAA
212100961211     C*
212200961211     C   97              MOVEL     'S'           W620
212300961211     C  N97              MOVEL     'N'           W620
212400961029     C                   EXSR      CHIUAN
212500961211     C                   MOVEL     'S'           W620
212600960523    4C                   ENDIF
212700961021   X3C                   ELSE
212800961122     C*
212900020213     C* SE CHI SPARA E' LA FILIALE FINALE DI ARRIVO, ME LO MEMORIZZO
213000961122     C*  MI SERVE PER LA CHIUSURA
213100961122     C                   MOVEL     'S'           WSTESS            1
213200961021     C                   Z-ADD     146           COMCAA
213300961021     C                   EXSR      CHIUAN
213400970204     C*
213500970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
213600970204     C                   EXSR      ANMARR
213700960522    3C                   ENDIF
213800961122     C**
213900961021   X2C                   ELSE
214000970204     C* CHI SPARA E' LA FILIALE FINALE DI ARRIVO
214100961122     C                   MOVEL     'S'           WSTESS
214200961021     C                   Z-ADD     146           COMCAA
214300961021     C                   EXSR      CHIUAN
214400970204     C*
214500970204     C* VEDO SE CREARE O CHIUDERE EVENTUALI ANOMALIA INTERNE
214600970204     C                   EXSR      ANMARR
214700960523    2C                   ENDIF
214800911016     C*
214900950922    1C                   END
215000961213    0C                   ENDIF
215100021022     c
215200021203     c                   unlock    fnbtt37l
215300970203     C**
215400970203     C** SE FLAG = 2 MODIFICO ANOMALIE METTENDO NUOVO NUMERO DI FOGLIO
215500970203     C**
215600970203     C     *IN05         IFEQ      *ON
215700970203     C*
215800921021     C                   EXSR      RICFAS
215900921021     C*
216000990518     C     KBAR          SETLL     FNANM000
216100990518     C     KBAR          READE     FNANM000                               99
216200921021     C*
216300960423    2C     *IN99         DOWEQ     '0'
216400070208    3C     ANMNFV        IFEQ      DLS45NFVO
216500000516     C* SE NON C'E' SOTTO CATEGORIA DEVONO ESSERE UGUALI
216600000516    4C     BARSPG        IFEQ      ' '
216700921021     C     ANMFSA        ANDEQ     COMFAS
216800000516     C* SE     C'E' SOTTO CATEGORIA, DEVE ESSERE UGUALE AD UNA DELLE
216900000516     C*  TANTE
217000000516     C     ANMFSA        OREQ      WFAS1
217100000516     C     ANMFSA        OREQ      WFAS2
217200000516     C     ANMFSA        OREQ      WFAS3
217300000516     C     ANMFSA        OREQ      WFAS4
217400000516     C     ANMFSA        OREQ      WFAS5
217500921021     C                   Z-ADD     BARNFV        ANMNFV
217600960524     C                   Z-ADD     BARDFV        ANMDAO
217700000516     C                   MOVEL     COMFAS        ANMFSA
217800990518     C                   UPDATE    FNANM000
217900000516    4C                   ENDIF
218000000516    3C                   ENDIF
218100921021     C*
218200990518     C     KBAR          READE     FNANM000                               99
218300000516    2C                   ENDDO
218400920424     C*
218500920424     C                   Z-ADD     3             B
218600021107    1C                   END
218700920424     C     ENDUPD        TAG
218800000209     C*
218900110907    1C     *IN39         IFEQ      *ON
219000000209     C                   SETON                                        40
219100110907   x1C                   ELSE
219200000209     C*
219300000210     C* ELABORO SOLO SPUNTA DA MACCHINONE
219400110907     c*  non lo faccio se modifica di numero foglio da video
219500110907    2c  N05RPS(Y)        ifne      'A'
219600000209     C* AGGIORNO FILE  FISGN SE C'E'
219700000209     C     KBAR          CHAIN     FISGN03L                           33
219800110907    3C     *IN33         DOWEQ     *OFF
219900110907    4C     SGNST2        IFLT      9
220000000209     C                   ADD       1             SGNST2
220100001228     C                   CLEAR                   SGNT6A
220200001228     C                   CLEAR                   SGNT6B
220300001228     C                   CLEAR                   SGNT6C
220400001228     C                   CLEAR                   SGNT6D
220500001228     C                   CLEAR                   SGNT6E
220600001228     C                   CLEAR                   SGNT6F
220700080624     c                   eval      sgndatora = %char(%timestamp:*iso0)
220800000209     C                   UPDATE    FISGN000
220900110907    4C                   ENDIF
221000001020     C     KBAR          READE     FISGN03L                               33
221100110907    3C                   ENDDO
221200110907     c
221300110907     c* Una qualsiasi spunta REALE chiude eventuale anomalia interna
221400110907     c*  115 - COLLO MAI AFFIDATO
221500110929     C                   MOVEL     'AR'          SAVCCH
221600110907     C                   Z-ADD     115           COMCAA
221700110907     C                   EXSR      CHIUAN
221800110907     c
221900110907     c
222000110907    2C                   ENDIF
222100110907    1C                   ENDIF
222200941206     C*
222300020215     C                   ENDSR
222400961118     C**************************************************************************
222500961205     C* CREO ANOMALIA 600 PER L'EXPORT   COLLO A TERRA
222600961118     C**************************************************************************
222700961118     C     ANM600        BEGSR
222800961210     C*
222900961210     C* PER ESTERO CREO ANOMALIA 645 E 600
223000020212    7C     NTWBAR        IFEQ      'EEX'
223100060322    7C     NTWBAR        oreq      'DPD'
223200060322    7C     NTWBAR        oreq      'FED'
223300961210     C                   Z-ADD     645           COMCAA
223400961210     C                   EXSR      CRTANM
223500961210     C*
223600961210    8C     W60X          IFEQ      'S'
223700961209     C* SE ESISTE LA 620  LA DELETO
223800961118     C                   Z-ADD     620           COMCAA
223900001110     C     COMCAA        LOOKUP    ACG                                    34
224000001110     C     BARSPG        IFNE      'I'
224100001110     C     *IN34         OREQ      *ON
224200990518     C     KANM          DELETE    FNANM02L                           34
224300001110     C                   ENDIF
224400961118     C                   Z-ADD     600           COMCAA
224500961118     C                   EXSR      CRTANM
224600961210     C                   CLEAR                   W60X
224700961210    8C                   ENDIF
224800961210    7C                   ENDIF
224900961210     C                   ENDSR
225000960702     C**************************************************************************
225100960702     C* CERCO LNP BOLLA SE COLLO CON SERIE > 0
225200960702     C**************************************************************************
225300960702     C     RICLNP        BEGSR
225400970814     C                   SETOFF                                       33
225500960702     C*
225600020201     C* CONTROLLO SE COLLO CONFERMABILE DAL TERMINAL DEL P.O. EFFETTUA
225700020201     C*  SPUNTA
225800020201     C                   MOVEL     PESTFP        W008A             8
225900960702     C                   MOVE      BARNRS        W005A             5
226000960702     C                   MOVEL     BARLNP        W005A
226100960702     C                   MOVE      W005A         W008A
226200990108     C                   Z-ADD     1             YY                4 0
226300970828     C     W008A         LOOKUP    S5F(YY)                                33
226400020131     C**
226500020131     C* SE NON TROVATO CERCO COME SERIE = 99
226600020131     C     *IN33         IFEQ      *OFF
226700020131     C                   MOVE      99            W008A
226800020131     C                   Z-ADD     1             YY                4 0
226900020131     C     W008A         LOOKUP    S5F(YY)                                33
227000020131     C                   ENDIF
227100020131     C**
227200020131    1C     *IN33         IFEQ      *ON
227300020131     C                   MOVEL     'S'           WTAB5F            1
227400020131     C* CERCO SPUNTA PARTENZA SE C'E' PRENDO LA FGS DI QUEST'ULTIMA
227500020201     C*  E LA DATA DI RIFERIMENTO E' LA DATA DI QUESTA SPUNTA
227600960702     C                   Z-ADD     1             WNPG
227700070131     C     KBAR3         SETLL     FNBRV09L
227800070131     C     KBAR3         READE     FNBRV09L                               33
227900020131    2C     *IN33         DOWEQ     *OFF
228000040303    3C     BR9ATR        IFEQ      *BLANKS
228100040323     c* Escludo le defluenze e le spunte di simfel(potrebbe quindi essere co
228200040323     c*  lo in transito da me e quindi non partito da me)
228300040323    4C     BR9KEY        IFNE      SA1KEY
228400040323     C                   MOVEL     BR9KEY        SA1KEY
228500040323     C                   Z-ADD     BR9NPG        WNPG
228600040323     C                   Z-ADD     BR9NFV        WNFV
228700040323     C                   Z-ADD     BR9FGS        WFGS
228800070906     C                   movel     BR9nps        Wnps
228900040323     C                   EXSR      RILFOG
229000040323     c                   Z-ADD     WDFV          BRVDFV
229100140616     c                   movel     WFCF          BRVFCF
229200140616     c                   movel     Wspg          BRVspg
229300040323    5C     WTTR          IFNE      *BLANKS
229400040323     C     WTTR          LOOKUP    TTR                                    34
229500040323     C                   MOVE      *IN34         W9DEFL            1
229600040323     C                   ELSE
229700040323     C                   MOVE      '0'           W9DEFL            1
229800040323    5C                   ENDIF
229900040323    4C                   ENDIF
230000040323    4c                   if        w9defl='0' and br9fgs<>simfel
230100110103     c*
230200040303     C                   MOVE      BR9PES        LNPB
230300970814     C                   CLEAR                   WTAB5F
230400020214     C                   Z-ADD     BRVDFV        DSPB
230500020214     C                   Z-ADD     BRVDFV        WDTRIF
230600960702     C                   SETON                                        33
230700020201    4C                   END
230800020131    3C                   END
230900070131     C  N33KBAR3         READE     FNBRV09L                               33
231000020131    2C                   ENDDO
231100020204     C**
231200020131    2C     WTAB5F        IFEQ      'S'
231300020218     C* CERCO SE C'E' IL P.O. CHE HA SPUNTATO, ALTRIMENTI IL PRIMO
231400020218     C                   MOVEA     F5F(YY)       WPO
231500020218     C                   Z-ADD     1             ZZ
231600020218     C                   CLEAR                   WTROVA
231700020218    3C                   DO        4             ZZ                3 0
231800020218    4C     WPO(ZZ)       IFGT      *ZEROS
231900020218     C                   MOVEL     WPO(ZZ)       W0030             3 0
232000020218    5C     W0030         IFEQ      BPES
232100020218     C                   MOVEL     W0030         LNPB
232200020218     C                   MOVEL     'S'           WTROVA
232300020218    5C                   ENDIF
232400020218    4C                   ENDIF
232500020218    3C                   ENDDO
232600020218     C**
232700020218     C* SE NON C'E' P.O. = BPES PRENDO IL PRIMO
232800020218     C     WTROVA        IFEQ      ' '
232900020218     C                   MOVEL     WPO(1)        LNPB
233000020218     C                   ENDIF
233100020214     C                   MOVEL     BARDFV        DSPB
233200020214     C                   MOVEL     BARDFV        WDTRIF
233300020131    2C                   ENDIF
233400970814    1C                   ENDIF
233500960702     C                   ENDSR
233600941206     C**************************************************************************
233700941206     C* CERCO IL RAGGRUPPAMENTO PISTOLA
233800941206     C**************************************************************************
233900921229     C     RICRAG        BEGSR
234000921229     C*
234100930305     C     SAVNPS        IFNE      ALF2
234200921229     C*
234300950515     C* HO CARICATO TUTTA SCHIERA
234400921229     C     *IN02         IFEQ      '0'
234500970417     C                   Z-ADD     1             Z                 3 0
234600930305     C                   MOVEL     ALF2          NUM2              2 0
234700970417     C     NUM2          LOOKUP    NPS(Z)                                 34
234800970417     C  N34              Z-ADD     1             Z
234900921229     C*
235000921229     C                   ELSE
235100921229     C*
235200921229     C* NON HO CARICATO LA SCHIERA --> FACCIO CHAIN
235300921229     C                   MOVEL     '7J'          COD
235400921229     C                   MOVEL     *BLANKS       KEY
235500930305     C                   MOVEL     ALF2          KEY
235600921229     C     KTAB          CHAIN     TABEL                              34
235700001023     C                   Z-ADD     2             Z
235800000316     C     *IN34         IFEQ      *OFF
235900000316     C                   MOVEL     TBLUNI        DS7J
236000000316     C                   MOVEL     §7JRPS        RPS(Z)
236100000316     C                   MOVEL     §7JORD        OPS(Z)
236200000316     C                   MOVEL     §7JTPS        TPS(Z)
236300000316     C                   MOVEL     §7JINV        INV(Z)
236400040503     C                   MOVEL     §7Jfsl        fsl(Z)
236500000316     C                   ELSE
236600000316     C                   MOVEL     'M'           RPS(Z)
236700000316     C                   MOVEL     999           OPS(Z)
236800000316     C                   MOVEL     'B'           TPS(Z)
236900000316     C                   MOVEL     ' '           INV(Z)
237000040503     C                   MOVEL     ' '           fsl(Z)
237100000316    1C                   END
237200921229     C                   END
237300921229     C*
237400930305     C                   MOVEL     ALF2          SAVNPS
237500921229     C                   END
237600941206     C*
237700921229     C                   ENDSR
237800941206     C**************************************************************************
237900941206     C* CHIUDO ANOMALIA
238000941206     C**************************************************************************
238100911018     C     CHIUAN        BEGSR
238200961211     C                   SETOFF                                       97
238300941206     C*
238400990518     C     KANM          CHAIN(N)  FNANM000                           97
238500961121     C**
238600961211    1C     *IN97         IFEQ      *OFF
238700970117     C**
238800970117     C** ANOMALIA 145/146: SE DATA SPUNTA > DATA ANOAMLIA
238900970117     C*  DEVO CONTROLLARE CHE NON CI SIANO ALTRE SPUNTE
239000970117     C*  CHE MI IMPEDISCONO LA CHIUSURA
239100970117     C     ANMCAA        IFEQ      145
239200970117     C     ANMCAA        OREQ      146
239300041013     c* Imposto econtrdfv per anomalie 145 146 che la deve sempre cotrollare
239400041013     c                   eval      wcontrdfv='S'
239500990518     C     BARDFV        IFGT      ANMDAO
239600970117     C                   EXSR      CTRSPU
239700970117     C                   ENDIF
239800970117     C                   ENDIF
239900961121     C**
240000041013     c     Wcontrdfv     ifne      'S'
240100041013     C* PER CATEGORIA no arrivi e partenza CHIUDO SE DATA >=
240200961122     C     WSTESS        OREQ      'S'
240300990518     C     BARDFV        ANDEQ     ANMDAO
240400041013     C     BARNPG        ANDne     2
240500041013     C     BARNPG        ANDne     1
240600041013     C* PER LE CATEGORIE arrivi SOLO SE DATA > e la posso chiudere
240700990518     C     BARDFV        ORGT      ANMDAO
240800970117     C     WCHIU         ANDEQ     'S'
240900961211     C*
241000961211     C     WCREA         OREQ      'N'
241100020213     C     BARFLE        ANDNE     BTFP
241200961121     C**
241300960416     C                   CLEAR                   DSLR33
241400960524     C                   MOVE      BARDFV        D33DCH
241500960514    2C     BARKEY        IFNE      SAVFOG
241600911125     C                   EXSR      RICFAS
241700960514    2C                   END
241800960514     C*
241900960416     C                   MOVEL     COMFAS        D33FSC
242000960416     C                   MOVEL     SAVCCH        D33CCH
242100960530     C                   MOVEL     SAVMAC        D33MAC
242200960416     C                   MOVE      ANMNR2        D33NRR
242300960416     C*
242400990712     C                   CALL      'FNLR33R'
242500960416     C                   PARM                    DSLR33
242600961209     C*
242700961209     C* PER ANOMALIE 145 E 146 CREO SE E' DA CREARE L'ANOMALIA 620
242800020212     C*  AOLO SE NON SONO IN CREAZIONE DELL 600
242900020212    2C     NTWBAR        IFEQ      'EEX'
243000961210     C     W620          ANDEQ     'S'
243100060322    2C     NTWBAR        oreq      'DPD'
243200060322     C     W620          ANDEQ     'S'
243300060322    2C     NTWBAR        oreq      'FED'
243400060322     C     W620          ANDEQ     'S'
243500961210     C*
243600961210    3C     ANMCAA        IFEQ      145
243700961209     C     ANMCAA        OREQ      146
243800961210     C*
243900961210    4C     W60X          IFEQ      'S'
244000961210     C                   Z-ADD     600           COMCAA
244100001110     C     COMCAA        LOOKUP    ACG                                    34
244200001110    5C     BARSPG        IFNE      'I'
244300001110     C     *IN34         OREQ      *ON
244400990518     C     KANM          DELETE    FNANM02L                           34
244500001110    5C                   ENDIF
244600961209     C                   Z-ADD     620           COMCAA
244700961209     C                   EXSR      CRTANM
244800961209     C                   CLEAR                   W60X
244900961210    4C                   ENDIF
245000961209     C*
245100961209     C* VEDO SE CANCELLARE L'ANOMALIA 645
245200961209     C                   Z-ADD     645           COMCAA
245300001110     C     COMCAA        LOOKUP    ACG                                    34
245400001110   4AC     BARSPG        IFNE      'I'
245500001110     C     *IN34         OREQ      *ON
245600990518     C     KANM          CHAIN     FNANM02L                           34
245700961210    4C     *IN34         DOWEQ     *OFF
245800990518    5C     ANMDAO        IFEQ      BARDFV
245900990518     C                   DELETE    FNANM000
246000961209     C                   SETON                                        34
246100961209     C                   ELSE
246200990518     C     KANM          READE     FNANM02L                               34
246300961210    5C                   ENDIF
246400961210    4C                   ENDDO
246500001110   4AC                   ENDIF
246600961210    3C                   ENDIF
246700961209     C*
246800961210    2C                   ENDIF
246900960514     C*
247000960514     C* L'ANOMALIA 05 PUO' ESSERCI PIU' VOLTE
247100961209    2C     ANMCAA        IFEQ      005
247200980709     C                   SETOFF                                       34
247300960514    3C     *IN34         DOWEQ     *OFF
247400990518     C     KANM          READE(N)  FNANM000                               34
247500960514     C                   MOVE      ANMNR2        D33NRR
247600960514     C*
247700960514     C  N34              CALL      'FNLR33R'
247800960514     C                   PARM                    DSLR33
247900960514    3C                   ENDDO
248000911018     C*
248100960514    2C                   ENDIF
248200961121   1AC                   ENDIF
248300961209   X1C                   ELSE
248400961209     C*
248500961211     C* SOLO PER ANOMALIE 145 E 146
248600961210    2C     COMCAA        IFEQ      145
248700961210     C     COMCAA        OREQ      146
248800991230     C* SE DEVO CREARE EVENTUALE 620 PER LNA ESTERA
248900020212    3C     W60X          IFEQ      'S'
249000961210     C     W620          ANDEQ     'S'
249100060322     C* E LA DEVO CREARE SIA SE TROVO CHE SE NON TROVO L'ANOMALIA 145
249200060322     C*  O 146 DA CHIUDERE  (WESAN =' ')
249300060322     C     WESAN         ANDEQ     ' '
249400110103     c*
249500060322    4C     NTWBAR        ifeq      'EEX'
249600060322     C     NTWBAR        oreq      'DPD'
249700060322     C     NTWBAR        oreq      'EEX'
249800961209     C                   Z-ADD     620           COMCAA
249900961209     C                   EXSR      CRTANM
250000961209     C                   CLEAR                   W60X
250100060322    4C                   ENDIF
250200060322    3C                   ENDIF
250300961210    2C                   ENDIF
250400961210    1C                   ENDIF
250500110103     c*
250600041013     c                   eval      wcontrdfv='N'
250700911018     C*
250800110929     c                   clear                   savmac
250900110929     c                   clear                   savcch
251000911018     C                   ENDSR
251100941206     C**************************************************************************
251200941206     C*    RICERCA FASE
251300941206     C**************************************************************************
251400911125     C     RICFAS        BEGSR
251500941206     C*
251600921020     C                   MOVEL     '7N'          COD
251700921020     C                   MOVEL     *BLANKS       KEY
251800921020     C                   MOVEL     BARNPG        KEY
251900921105     C     KTAB          CHAIN     TABEL                              41
252000921105     C  N41              MOVEL     TBLUNI        DS7N
252100000516     C*
252200000516     C     BARSPG        IFNE      ' '
252300000516     C                   MOVEL     §7NFS1        WFAS1             1
252400000516     C                   MOVEL     §7NFS2        WFAS2             1
252500000516     C                   MOVEL     §7NFS3        WFAS3             1
252600000516     C                   MOVEL     §7NFS4        WFAS4             1
252700000516     C                   MOVEL     §7NFS5        WFAS5             1
252800000516     C                   ENDIF
252900921021     C*
253000960524     C     BARSPG        IFEQ      §7NSC1
253100921021     C                   MOVEL     §7NFS1        COMFAS            1
253200921021     C                   ELSE
253300960524     C     BARSPG        IFEQ      §7NSC2
253400921021     C                   MOVEL     §7NFS2        COMFAS
253500921021     C                   ELSE
253600960524     C     BARSPG        IFEQ      §7NSC3
253700921021     C                   MOVEL     §7NFS3        COMFAS
253800921021     C                   ELSE
253900960524     C     BARSPG        IFEQ      §7NSC4
254000921021     C                   MOVEL     §7NFS4        COMFAS
254100921021     C                   ELSE
254200921021     C                   MOVEL     §7NFS5        COMFAS
254300921021     C                   END
254400921021     C                   END
254500921021     C                   END
254600921021     C                   END
254700921020     C*
254800961217     C                   MOVEL     BARKEY        SAVFOG            8 0
254900941206     C*
255000911125     C                   ENDSR
255100950927     C**************************************************************************
255200950927     C*    CONTROLLO SE CREARE ANOMALIA 50: COLLO ARRIVATO NON PARTITO
255300130228     C*                      O ANOMALIA 55/56/57: DISGUIDO - ESTERNA
255400950927     C**************************************************************************
255500950927     C     CRTA50        BEGSR
255600000320     C                   CLEAR                   WCCH
255700020214     C* PER ANOMALIA 50 NON CREO SE ESISTE GIA' LA DATA DI PARTENZA
255800020214     C*  BOLLE TRANSITO
255900020214     C     WTIBOL        IFEQ      'T'
256000020214     C     COMDFV        ANDGT     0
256100020214     C                   GOTO      NOA50
256200020214     C                   ENDIF
256300020214     C**
256400020214     C     WTIBOL        IFEQ      'A'
256500020218     C     WTIBOL        OREQ      'L'
256600050215     c* Per l'anoamlia 50 basterebbe questo test senza leggere le spunte,
256700050215     c*  le spunte le devo leggere lo stesso per i manca record
256800020214     C     WESART        IFEQ      'S'
256900020214     C     COMDFV        ANDGT     0
257000020214     C                   GOTO      NOA50
257100020214     C                   ENDIF
257200020214     C* SE E' COLLO IN ARRIVO E BOLLA CONSEGNATA --> NON CREO
257300020214     C     WESART        IFEQ      'S'
257400020214     C     ARTDCM        ANDGT     0
257500020214     C                   GOTO      NOA50
257600020214     C                   ENDIF
257700020218     C* SE BOLLA LOCALE E NON C'E' LA BOLLA NON FACCIO NIENTE
257800020218     C     WTIBOL        IFEQ      'L'
257900020218     C     WBOPAR        ANDEQ     'N'
258000020218     C                   GOTO      NOA50
258100020218     C                   ENDIF
258200020214     C                   ENDIF
258300020214     C**
258400950927     C* VERIFICO SE ESISTE GIA'
258500040429     c* ora la esistenza è all'interno dello stesso p.o. e non in assoluto
258600950927    1C     WTIBOL        IFEQ      'D'
258700950927     C                   Z-ADD     55            COMCAA
258800950927     C                   ELSE
258900950927     C                   Z-ADD     50            COMCAA
259000950927    1C                   ENDIF
259100990409     C* SE LA CATEGORIA E' IMA IMG O CONS INVECE CHE 55 CREO LA 56
259200020214    1C     COMCAA        IFEQ      55
259300020214    2C     BARNPG        IFEQ      4
259400990412     C     BARNPG        OREQ      3
259500990409     C     BARSPG        ANDEQ     'A'
259600990412     C     BARNPG        OREQ      3
259700990409     C     BARSPG        ANDEQ     'G'
259800161021     C     BARNPG        OREQ      3
259900161021     C     BARSPG        ANDEQ     'Z'
260000990409     C                   Z-ADD     56            COMCAA
260100020214    2C                   ENDIF
260200020214    1C                   ENDIF
260300130228     c* se Anomalia 55 a collo che può partire da diverse filiali
260400130228     c*  e chi spunta è una di qeuste --> non creo 55 ma 57 Disguido AUTORIZZ
260500130228     c*  solo per ategorie di spunta partenza
260600130228     c                   if        comcaa=55  and
260700130228     c                             (bolxco='W' or
260800130228     c                             (wesbtt='N' and wesblt='N' and wesart='N'
260900130228     c                              and barnrs>0))
261000130228     c* se si tratta di spunta partenza --> creo la 57
261100130228     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
261200130228     c                             or barnpg=1
261300130228     c                   exsr      CtrXCO
261400130228     c                   if        Indx>0
261500130301     c                   z-add     57            comcaa
261600130228     c                   endif
261700130228     c                   endif
261800130228     c                   endif
261900130301     c
262000130301     c* In caso di anomalia 50 manca bolla --> verifico se non è da creare
262100130301     c*  per collo spuntabile in questo TFP e "locale"
262200130301     c                   if        comcaa=50 and
262300130301     c                             (bolxco='W' or
262400130301     c                             (wesbtt='N' and wesblt='N' and wesart='N'
262500130301     c                              and barnrs>0))
262600130301     c* se si tratta di spunta di partenza --> non creo la 50 ma la 57
262700130301     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
262800130301     c                             or barnpg=1
262900130301     c                   exsr      CtrXCO
263000130301     c                   if        Indx>0
263100130301     c                   z-add     57            comcaa
263200130301     c                   endif
263300130301     c                   endif
263400130301     c                   endif
263500971009     C*
263600130228     c* per anomalia 55 o 56 57 valida l'anomalia per p.o. spunta
263700040429     c* per anomalia 50:
263800040429     c* per bolle transito solo se spara il p.o. transito
263900040429     c* per bolle arrivi, se trovo anomalia nell'area di arrivo(e non
264000040429     c*  di un eventuale p.o. transito)
264100040429     c                   clear                   wtrova
264200040429     C     KANM          SETLL     FNANM000
264300040504     C     KANM          reade(N)  FNANM000                               34
264400040429    1c                   dow       not*in34 and wtrova=*blanks
264500040429    2c                   if        anmcaa<>50 or wtibol='T'
264600040429    3c                   if        anmfle=bpes
264700040429     c                   eval      wtrova='1'
264800040429   x3c                   else
264900040504     C     KANM          reade(N)  FNANM000                               34
265000040429    3c                   endif
265100110103     c*
265200040429   x2c                   else
265300040429     C                   CLEAR                   DSLV55
265400040429     C                   MOVEL     'A'           D55TPT
265500040429     C                   Z-ADD     anmfle        D55LIN
265600040429     C                   Z-ADD     lnpb          D55lnp
265700040429     C                   Z-ADD     bardfv        D55DRF
265800040429     C                   EXSR      FNLV55
265900040429    3c                   if        d55tfa=pestfa
266000040429     c                   eval      wtrova='1'
266100040429   x3c                   else
266200040504     C     KANM          reade(N)  FNANM000                               34
266300040429    3c                   endif
266400040429    2c                   endif
266500110103     c*
266600040429    1c                   enddo
266700990412     C*
266800990412     C* PER ANOMALIA 55 VERIFICO ANCHE CHE NON CI SIA LA 56
266900990412     C*  PERCHE' DOPO AVER SPARATO UN COLLO IN IMA PUO' ESSERE
267000990412     C*  SPARATO IN PARTENZA PER FARLO RIPARTIRE
267100040429    1C     *IN34         IFEQ      *On
267200990412     C     COMCAA        ANDEQ     55
267300990412     C                   Z-ADD     56            COMCAA
267400040429     C     KANM          SETLL     FNANM000
267500040504     C     KANM          reade(N)  FNANM000                               34
267600040429     c                   dow       not*in34 and wtrova=*blanks
267700040429     c                   if        anmfle=bpes
267800040429     c                   eval      wtrova='1'
267900040429     c                   else
268000040504     C     KANM          reade(N)  FNANM000                               34
268100040429     c                   endif
268200040429     c                   enddo
268300990412     C                   Z-ADD     55            COMCAA
268400990412    1C                   ENDIF
268500950927     C*
268600040429    1C     *IN34         IFEQ      *On
268700970828     C* NON CREO ANOMALIA 55 SE C'E' UNA SPUNTA PARTENZA DI ALTRO AS
268800970828     C*  PERCHE' NON SI TRATTA PIU' DI UN DISGUIDO MA O E' COLLO
268900990409     C*  IN TRANSITO DI CUI NON HO RICEVUTO LA BOLLA
269000990409     C*  O E' COLLO DISGUIDATO IN ALTRA FIL CHE PER ANDARE IN ARRIVO
269100970828     C*  DEVE PASSARE DA ALTRA FIL ANCORA
269200970828     C*  LA 200 VIENE INVECE CREATA LO STESSO PERCHE' UNA ANOMALIA
269300970828     C*  GESTIONALE CHE SERVE PER FAR RIPARTIRE AUTOMATICAMENTE IL COLL
269400970828     C*  ANCHE SE NON VIENE SPARATO NE' IN PARTENZA NE' IN ENTRATA
269500000316     C                   EXSR      CNOIDD
269600950927    1C                   ENDIF
269700020214     C     NOA50         ENDSR
269800000316     C**************************************************************************
269900000316     C* RIEMPO ALCUNI CAMPI ANOMALIA NO IDD
270000000316     C**************************************************************************
270100000316     C     CNOIDD        BEGSR
270200160624     C** ANOMALIE 50 -->
270300160725     C* VERIFICO SE ESISTE UNA SPUNTA PARTENZA PER QUEL COLLO
270400160624     C*  SE SI NON CREO ANOMALIA PER 50
270500130228     C*  ANOMALIE 55/56/57 --> CONSIDERO SOLO LA SPUNTA "D-DISGUIDO"
270600020214     C*                     PER NON CREARLE
270700000316     C                   Z-ADD     1             WNPG
270800000317     C                   CLEAR                   WTROVA
270900021121     C* CAMPI DI COMODO PER ANOM 50 per transito su stessa £1
271000021121     C                   CLEAR                   WFVALNP
271100021121     C                   CLEAR                   WFVANFV
271200021121     C                   CLEAR                   WFVADFV
271300110103     c*
271400070131     C     KBAR3         CHAIN     FNBRV09L                           99
271500020214    1C     *IN99         DOWEQ     *OFF
271600000317     C     WTROVA        ANDEQ     ' '
271700050215     c                   clear                   wrileggi          1
271800020214     C*****
271900020214     C* SOLO SE LA SPUNTA TROVATA E' DI DISGUIDO, ALTRIMENTI SE NORMALE
272000020214     C*  E' UNA SPUNTA PREVISTA, QUINDI DA IGNORARE
272100020214    2C     COMCAA        IFEQ      55
272200020214     C     COMCAA        OREQ      56
272300130228     C     COMCAA        OREQ      57
272400040303    3C     BR9FGS        IFNE      BTFP
272500040429     c* verifico tramite pgm fnlv53r se spunta di foglio diretto a me(BPES)
272600040429     c*  se è diretto a me non creo anomalia
272700040429     c*  Questo vale sia per colli in disguido che colli in transito
272800040429     c*  manca record bolla partenza
272900040430     c                   clear                   dslv53
273000040504     c                   movel     'T'           d53tfo
273100040429     C                   MOVEL     BR9NPG        D53NPG
273200040429     C                   Z-ADD     BR9NFV        D53NFV
273300040429     C                   MOVEL     BR9FGS        D53FGS
273400040429     C                   MOVEL     BR9FLE        D53FLF
273500040504     C                   MOVEL     'A'           D53ABB
273600040429     C                   MOVEL     br9lna        D53LNA
273700050603     C                   MOVEL     Usalnatfa     D53TFA
273800040504     C                   MOVEL     bpes          D53lai
273900040429     C                   CALL      'FNLV53R'
274000040429     C                   PARM                    DSLV53
274100040429     c* se non ci sono errori --> OK esco dal ciclo
274200040429    4c                   if        d53err=*blanks
274300000317     C                   MOVEL     '1'           WTROVA            1
274400050418   x4c                   else
274500050418     C                   eval      wrileggi='1'
274600040429    4c                   endif
274700020214   X3C                   ELSE
274800050215     C                   eval      wrileggi='1'
274900020214    3C                   ENDIF
275000020214   X2C                   ELSE
275100020214     C**
275200031204     c** bolla non locale: cerco spunta non mia
275300031204   2ac     WSPLOC        ifeq      *blanks
275400040303    3C     BR9FGS        IFNE      BTFP
275500050329     c* Deve essere una spunta trasmessa, altrimenti ignoro (scaricata
275600050329     c*  tardi)
275700050329     c     br9ftr        andne     *blanks
275800110103     c*
275900050222    4c                   if        comcaa=50
276000050215     c                   clear                   dslv53
276100050322     c                   if        wtibol='T'
276200050322     c                   movel     'T'           d53tfo
276300050322     c                   else
276400050215     c                   movel     'A'           d53tfo
276500050322     c                   endif
276600050215     C                   MOVEL     BR9NPG        D53NPG
276700050215     C                   Z-ADD     BR9NFV        D53NFV
276800050215     C                   MOVEL     BR9FGS        D53FGS
276900050215     C                   MOVEL     BR9FLE        D53FLF
277000050215     C                   MOVEL     'A'           D53ABB
277100050215     C                   MOVEL     br9lna        D53LNA
277200050603     C                   MOVEL     Usalnatfa     D53TFA
277300050215     C                   MOVEL     pestfa        D53lai
277400050215     C                   CALL      'FNLV53R'
277500050215     C                   PARM                    DSLV53
277600050215     c* se non ci sono errori --> OK esco dal ciclo
277700050222    5c                   if        d53err=*blanks
277800020214     C                   MOVEL     '1'           WTROVA            1
277900050222   X5C                   ELSE
278000050418     c* Se si tratta di errore di foglio non trovato e provengo da
278100050418     c*  aggiornamento bolla transito Ex disguido (richiamo da fnlr13r)
278200050418     c*  il foglio si potrebbe trovare in fnfva01r, er cui conttrollo da
278300050418     c*  FNFGV
278400070221    6c                   if        d53err='2' and dls45FLG='T'
278500050418     c                   movel     'G'           d53tfo
278600050418     C                   CALL      'FNLV53R'
278700050418     C                   PARM                    DSLV53
278800050418     c* se non ci sono errori --> OK esco dal ciclo
278900050418    7c                   if        d53err=*blanks
279000050418     C                   MOVEL     '1'           WTROVA            1
279100050418   x7c                   else
279200050418     C                   eval      wrileggi='1'
279300050418    7c                   endif
279400050418   x6c                   else
279500050418     C                   eval      wrileggi='1'
279600050418    6c                   endif
279700050222    5C                   ENDIF
279800110103     c*
279900050222     c* Per anom 60/61 se trovata spunta --> ok
280000160624   X4C**                 ELSE
280100160624     C**                 MOVEL     '1'           WTROVA            1
280200050222     c                   endif
280300110103     c*
280400020214   X3C                   ELSE
280500050215     C                   eval      wrileggi='1'
280600020214    3C                   ENDIF
280700110103     c*
280800031204   2ac                   else
280900031204     c** bolla     locale: cerco spunta mia
281000040303    3C     BR9FGS        IFeq      BTFP
281100031204     C                   MOVEL     '1'           WTROVA            1
281200031204   X3C                   ELSE
281300050215     C                   eval      wrileggi='1'
281400031204    3C                   ENDIF
281500031204   2aC                   ENDIF
281600031204    2C                   ENDIF
281700050215     c
281800050215     c                   if        wrileggi='1'
281900070131     C     KBAR3         READE     FNBRV09L                               99
282000050215     c                   endif
282100020214    1C                   ENDDO
282200160725     c*
282300160725     c* Se non trovata e si tratta di bolla locale: cerco spunta
282400160725     c*  entrata
282500160725     c                   if        wtrova=' ' and wsploc=*blanks
282600160725     C                   Z-ADD     5             WNPG
282700160725     C     KBAR3         chain     FNBRV09L
282800160725     c                   if        %found(fnbrv09l) and br9fgs=btfp
282900160725     C                   MOVEL     '1'           WTROVA            1
283000160725     c                   setoff                                       99
283100160725     c                   endif
283200160725     c                   endif
283300000316     C*
283400000316     C* 99 ON  - SPUNTA NON TROVATA --> CREO ANOMALIA PER 50/55/56
283500000316    0C     *IN99         IFEQ      *ON
283600000316     C     COMCAA        ANDNE     60
283700000614     C     COMCAA        ANDNE     61
283800000316     C* 99 OFF - SPUNTA     TROVATA --> CREO ANOMALIA PER 60
283900160624     C**   *IN99         OREQ      *OFF
284000160624     C**   COMCAA        ANDEQ     60
284100160624     C**   *IN99         OREQ      *OFF
284200160624     C**   COMCAA        ANDEQ     61
284300000316     C                   CLEAR                   FNANM000
284400170505     c***
284500000316     C* MEMORIZZO NUMERO SPEDIZIONE SU ANOMALIA SE C'E'
284600170505    1C***                SELECT
284700170505     C***  WESBTT        wheneq    'S'
284800170505     C***                Z-ADD     BTTAAS        ANMAAS
284900170505     C***                Z-ADD     BTTLNP        ANMLNP
285000170505     C***                Z-ADD     BTTNSP        ANMNSP
285100170505     C***  WESART        wheneq    'S'
285200170505     C***                Z-ADD     ARTAAS        ANMAAS
285300170505     C***                Z-ADD     ARTLNP        ANMLNP
285400170505     C***                Z-ADD     ARTNSP        ANMNSP
285500170505     C***  WESblt        wheneq    'S'
285600170505     C***                Z-ADD     bltAAS        ANMAAS
285700170505     C***                Z-ADD     bltLNP        ANMLNP
285800170505     C***                Z-ADD     bltNSP        ANMNSP
285900170505    1C***                ENDSL
286000170505
286100170505     c                   if        comnsp>0
286200170505     C                   Z-ADD     comAAS        ANMAAS
286300170505     C                   Z-ADD     comLNP        ANMLNP
286400170505     C                   Z-ADD     comNSP        ANMNSP
286500170505     c                   endif
286600040621     c* Se c'e' la spedizione verifico se spedizione di valore
286700110103     c*
286800040621     C* linea partenza segnacollo
286900000316     C                   MOVEL     BARLNP        ANMFLS
287000000316     C* PER ANOMALIA 50
287100000316    1C     COMCAA        IFEQ      50
287200000316     C                   Z-ADD     BARFGS        ANMFLE
287300040908     C* PER BOLLA LOCALE non c'e' abbinamento di foglio per cui deduco le
287400040908     c*  date damettere sulla anomalia
287500040908     c                   clear                   wnofva            1
287600040908     c                   if        wtibol='L'
287700040908     c                   eval      wnofva='S'
287800040908     c                   endif
287900170505     c* Per sicurezza, per essere sicura di non aver sporcato i campi di ARB
288000170505     c*  uso i campi salvati
288100170505     c****               if        wesart='S' and arbtfp=arbtfa
288200170505     c                   if        wesart='S' and lnptfp=boltfa
288300040908     c                   eval      wnofva='S'
288400040908     c                   endif
288500050603     c                   if        wesart='N' and lnptfp=Usalnatfa
288600040908     c                   eval      wnofva='S'
288700040908     c                   endif
288800040908     c
288900040908   1ac                   if        wnofva='S'
289000040908    2c                   if        wesart='S'
289100170509     C                   Z-ADD     bolDBR        SAVDAO
289200170509     C                   Z-ADD     bolDBR        ANMDAO
289300020218     C                   Z-ADD     99998         ANMNFV
289400020218     C                   Z-ADD     LNPTFP        ANMCDU
289500020218     C                   Z-ADD     LNPTFP        WFL1
289600020218     C                   MOVEL     'L'           WFLGT
289700040908     c*
289800040908   x2c                   else
289900040908     c* Se non c'e' bolla cerco il 1° foglio locale con data < data spunta
290000040908     c     kfgv2         setll     fnfgv02l
290100040908     c     lnptfp        readpe    fnfgv02l
290200040908    3c                   dow       not %eof(fnfgv02l)  and
290300040908     c                             fgvttr <>'L'
290400040908     c     lnptfp        readpe    fnfgv02l
290500040908    3c                   enddo
290600110103     c*
290700040908    3c                   if        not %eof(fnfgv02l)
290800040908     C                   Z-ADD     fgvdfv        SAVDAO
290900040908     C                   Z-ADD     fgvdfv        ANMDAO
291000040908     C                   Z-ADD     99998         ANMNFV
291100040908     C                   Z-ADD     LNPTFP        ANMCDU
291200040908     C                   Z-ADD     LNPTFP        WFL1
291300040908     C                   MOVEL     'L'           WFLGT
291400040908    3c                   endif
291500040908    2c                   endif
291600110103     c*
291700040908  x1aC                   ELSE
291800070208     C* SE TROVA LA BOLLA IN BLP C'E' SEMPRE LNP BOLLA
291900020214     C*  ALTRIMENTI LA 5F ALTRIMENTI BARLNP
292000020214     C                   MOVEL     LNPB          WLIN
292100020214     C* IMPOSTO LA DATA DI RIFERIMENTO PER LA RICERCA DEL TERMINAL
292200020214     C*  DI PARTENZA: LA PRIMA VOLTA E' LA DATA DELLA BOLLA SE C'E'
292300020214     C                   MOVE      DSPB          WDTRIF
292400000316     C*
292500000316     C* CERCO IL NUMERO DI FV SU CUI DOVREBBE ESSERE PARTITO IL COLLO
292600000316     C                   MOVE      *BLANKS       WFLGT             1
292700000316     C                   CLEAR                   WLINPR
292800000316     C                   CLEAR                   WVALNP
292900000316    2C     WLIN          DOWGT     0
293000000316     C                   EXSR      RICFV
293100000316     C* MEMORIZZO IL P.O. DI TRANSITO E LA LINEA DI PARTENZA DEL FV
293200000316     C     WLIN          IFGT      *ZEROS
293300000316     C                   Z-ADD     WLIN          WLINPR
293400000316     C                   Z-ADD     FVALNP        WVALNP
293500000316     C                   ENDIF
293600000316    2C                   ENDDO
293700000316    2C     ANMCDU        IFGT      0
293800000316     C                   MOVEL     ANMCDU        WFL1
293900000316   X2C                   ELSE
294000070208     C* SE TROVA LA BOLLA IN BLP C'E' SEMPRE LNP BOLLA
294100020214     C*  ALTRIMENTI LA 5F ALTRIMENTI BARLNP
294200000316     C                   MOVEL     LNPB          WFL1
294300000316    2C                   ENDIF
294400020218   1AC                   ENDIF
294500000316     C*
294600000316   X1C                   ELSE
294700160624     C* PER ANOMALIA 55/56/57/      (NO IDD LE ALTRE)
294800000316     C                   Z-ADD     BARDFV        ANMDAO
294900000316     C                   MOVEL     BARFGS        ANMFLE
295000000316     C                   MOVEL     LNPB          WFL1
295100000316     C                   Z-ADD     BARNFV        ANMNFV
295200000316     C                   MOVEL     BARFGS        ANMCDU
295300000316    1C                   ENDIF
295400000316     C* SCRIVO L'ANOMALIA
295500000316     C                   EXSR      RIEANM
295600040621     c*
295700040621     c                   if        anmnsp>0
295800091209     c     kar5          chain(n)  fiar501l
295900040621    3c                   if        %found(fiar501l)
296000040621     c                   movel     ar5uni        dar5gen
296100040621     c                   else
296200040621     c                   clear                   dar5gen
296300040621    3c                   endif
296400040621     c                   movel     §ar5bva       anmft4
296500040621     c                   endif
296600000614     C*
296700000316     C* ESTERNA
296800000614    1C     §7CAIE        IFEQ      'E'
296900000316     C                   MOVEL     WFL1          ANMFL1
297000000614    1C                   ENDIF
297100000316     C* SCRIVO L'ANOMALIA SOLO SE HO TROVATO IL FOGLIO PARTENZA PER"50"
297200000614    1C     ANMCAA        IFNE      50
297300000316     C     ANMCAA        OREQ      50
297400000316     C     ANMNFV        ANDGT     0
297500000316     C* SE LA KEY E' PER SEGNACOLLO ED IL NUMERO E' 0 NON CREO L'ANOMAL
297600000316    2C     §7CKEY        IFNE      'S'
297700000316     C     §7CKEY        OREQ      'S'
297800000316     C     ANMSCN        ANDGT     0
297900001110     C************************
298000001110     C* CREO ANOMALIA SOLO SE:
298100001110     C************************
298200070215     C* ANOMALIA CREABILE ANCHE DA CAT.SPUNTA GENERICA
298300070215     C*                                 (ANCHE DA SPUNTA IMI)
298400070215     C     §7CCCG        ifeq      'S'
298500070215     C* CATEGORIA SPUNTA NON GENERICA   (NON È UNA SPUNTA IMI)
298600070215     C     BARSPG        orne      'I'
298700000316     C                   WRITE     FNANM000
298800001110     C*
298900001110     C* CHIUDO SE POSSIBILE UNA EVENTUALE 55 GIA' CREATA E POI CREO
299000001110     C*  LA 56
299100001110    3C     COMCAA        IFEQ      56
299200001110     C                   Z-ADD     55            COMCAA
299300001110     C                   MOVEL     'S'           SAVMAC
299400001110     C                   EXSR      CHIUAN
299500001110     C                   CLEAR                   SAVMAC
299600001110    3C                   ENDIF
299700000614    3C                   ENDIF
299800000614     C*
299900000316     C* PER ANOMALIA 50 CREATA AGGIORNO ANCHE LA DATA DI PARTENZA
300000000316     C* O LA DATA DI USCITA DAL TRANSITO
300100000316     C     ANMCAA        IFEQ      50
300200000316     C                   EXSR      AGGPAR
300300000316     C                   ENDIF
300400160727     C     ANMCAA        IFEQ      55
300500160727     C     ANMCAA        oreq      56
300600160727     C                   EXSR      AGGPAR_d
300700160727     C                   ENDIF
300800000316     C*
300900000316    2C                   ENDIF
301000020222     C*
301100020222     C                   ELSE
301200020222     C* SE NON TROVATO FOGLIO ATTENDO CHE ARRIVI (CI DEVE ESSERE PRIMA
301300031204     C*  O POI) se non locale
301400060103     c* Solo se non richiamato
301500060103     c  N02wtibol        ifne      'L'
301600020222     C                   SETON                                        3940
301700031204     c                   endif
301800000614    1C                   ENDIF
301900000614    0C                   ENDIF
302000000316     C                   ENDSR
302100130228     C**************************************************************************
302200130228     C* controllo se bolla con colli chde partono da divderse filiali
302300130228     C**************************************************************************
302400130228     C     CtrXCO        BEGSR
302500130228     c                   eval      Wflsnrs=(BARLNP*100)+BARnrs
302600130228
302700130228    1c                   if        wflsnrs<>savfnrs
302800130228     c                   clear                   xcotfp
302900130228     C                   MOVEL     'XCO'         kcod
303000130228     C                   eval      kke1='ALTRITFP'
303100130228     C                   eval      kke2=%editc(wflsnrs:'X')
303200130228     c     ktbe2         chain     tntbe01l
303300130228    2c                   if        %found(tntbe01l) AND tbeatb=' '
303400130228     C                   MOVEA     TBeUNI        xcotfp
303500130228    2c                   endif
303600130228     c
303700130228     c                   eval      savfnrs=wflsnrs
303800130228    1c                   endif
303900130228    c
304000130228     C* SE la fil spunte è compresa --> creo la 57
304100130228     c                   movel     barfgs        alfafgs           3
304200130228     c                   eval      Indx=%lookup(alfafgs:xcotfp)
304300130228     c
304400130228     c*
304500130228     C                   ENDSR
304600950927     C**************************************************************************
304700950927     C* RICERCA FV SU CUI E' PARTITA LA SPUNTA
304800950927     C**************************************************************************
304900950927     C     RICFV         BEGSR
305000950927     C                   CLEAR                   WMODL
305100021121     C                   movel     'N'           Wnoabb2           1
305200021121     C                   movel     'N'           Wnoabb6           1
305300950927     C* PRENDO IL TERMINAL DI PARTENZA DELLA LINEA DI PARTENZA
305400970814     C                   CLEAR                   DSLV55
305500970814     C                   MOVEL     'P'           D55TPT
305600970814     C                   MOVEL     WLIN          D55LIN
305700020214     C                   MOVEL     WDTRIF        D55DRF
305800020213     C                   EXSR      FNLV55
305900970814     C                   MOVEL     D55TFP        WPTFP
306000110103     c*
306100100713     C* PRENDO IL TERMINAL DI arrivo generico della linea
306200100713     C                   CLEAR                   DSLV55
306300100713     C                   MOVEL     'A'           D55TPT
306400100713     C                   MOVEL     barlna        D55LIN
306500100713     C                   MOVEL     WDTRIF        D55DRF
306600100713     C                   EXSR      FNLV55
306700100713     C                   MOVEL     D55TFa        WGENlnaTFA
306800021121     c
306900021121     c* se WPTFP = simfel ed ho salvato i campi del foglio illuminato
307000021121     c*  trovato, li considero validi e non rieseguo la routine
307100021121     c                   if        wptfp=simfel  and wfvalnp>0
307200021121     C                   MOVEL     wFVALNP       ANMCDU
307300021121     C                   Z-ADD     wFVALNP       SAVFLP
307400021121     C                   Z-ADD     wFVANFV       ANMNFV
307500021121     C                   Z-ADD     wFVADFV       ANMDAO
307600021121     C                   Z-ADD     wFVADFV       SAVDAO
307700021121     C                   CLEAR                   WLIN
307800021121     C                   MOVEL     '1'           WMODL             1
307900021121     c                   else
308000950927     C*
308100950927     C* CONTROLLO I FV ABBINATI, SE E' FOGLIO ARRIVI
308200020214     C     BARNPG        IFEQ      2
308300950927     C                   Z-ADD     BARNPG        KNPG4
308400950927     C                   Z-ADD     BARNFV        KNFV4
308500020214     C                   Z-ADD     BARFGS        KFGS4
308600950927     C                   EXSR      REDFVA
308700020214     C                   ENDIF
308800950927     C*
308900950927     C* SE NON L'HO TROVATO CERCO UN ALTRO F.ARRIVI DEL GIORNO
309000950927     C     WMODL         IFEQ      ' '
309100021121     c                   eval      wdata=bardfv
309200950927     C                   EXSR      MEMFAR
309300021121     C* MEMORIZZO LA NUOVA DATA
309400021121     C                   MOVEL     BARDFV        SAVDFV
309500110103     C                   Z-ADD     1             I                 3 0
309600950927     C*
309700950927    2C     PG2(I)        DOWGT     0
309800950927     C     ANMNFV        ANDEQ     0
309900950927     C                   Z-ADD     2             KNPG4
310000950927     C                   Z-ADD     PG2(I)        KNFV4
310100020214     C                   Z-ADD     SAVTFA        KFGS4
310200950927     C                   EXSR      REDFVA
310300021121     c*
310400021121     c* Memorizzo se nessuno dei fogli 2 ha dei fogli partenza abbinati
310500021121     c                   if        wabbinati=' '
310600021121     c                   clear                   wnoabb2
310700021121     c                   endif
310800950927     C                   ADD       1             I
310900950927    2C                   ENDDO
311000950927    1C                   ENDIF
311100110103     c*
311200021121     c* Se non ho fogli abbianti e non si tratta di cat arrivi, ritento
311300021121     c*  la ricerca dei fogli arrivi
311400021121    1c     WMODL         IFEQ      ' '
311500110103    2c                   if        barnpg<> 2 and
311600110103     c                             wnoabb2 ='N' and
311700021121     c                             wsavdata=bardfv
311800021121     c* Data - 1
311900021121     C                   MOVEL     bardfv        G02INV
312000021121     C                   Z-ADD     *ZEROS        G02DAT
312100021121     C                   Z-ADD     *ZEROS        G02TGI
312200021121     C                   MOVEL     '3'           G02ERR
312300021121     C                   CALL      'XSRDA8'
312400021121     C                   PARM                    WLBDAT
312500021121      *
312600021121     C                   SUB       1             G02TGI
312700021121     C                   Z-ADD     *ZEROS        GI8DAT
312800021121     C                   Z-ADD     *ZEROS        GI8INV
312900021121     C                   Z-ADD     G02TGI        GI8TGI
313000021121     C                   CALL      'XSRGI8'
313100021121     C                   PARM                    WGIDAT
313200021121      *
313300021121     C                   Z-ADD     GI8INV        wdata
313400021121      *
313500021121     C                   EXSR      MEMFAR
313600021121     C* MEMORIZZO LA NUOVA DATA
313700021121     c                   if        wsavdata>0
313800021121     C                   MOVEL     wsavdata      SAVDFV
313900021121     c                   else
314000021121     C                   MOVEL     BARDFV        SAVDFV
314100021121     c                   endif
314200110103     c*
314300021121     C                   Z-ADD     1             I
314400021121     C*
314500021121    3C     PG2(I)        DOWGT     0
314600021121     C     ANMNFV        ANDEQ     0
314700021121     C                   Z-ADD     2             KNPG4
314800021121     C                   Z-ADD     PG2(I)        KNFV4
314900021121     C                   Z-ADD     SAVTFA        KFGS4
315000021121     C                   EXSR      REDFVA
315100021121     C                   ADD       1             I
315200021121    3C                   ENDDO
315300021121    2C                   ENDIF
315400021121    1c                   endif
315500110103     c*
315600100713     c* se il FV non ancora trovato e si dovrebbe trattare di bolla locale
315700100713     c*  tengo i dati del foglio illuiminato
315800100713     c     WMODL         ifeq      ' '
315900100713     c     wgenlnatfa    andeq     wlin
316000100713     C                   MOVEL     wFVALNP       ANMCDU
316100100713     C                   Z-ADD     wFVALNP       SAVFLP
316200100713     C                   Z-ADD     wFVANFV       ANMNFV
316300100713     C                   Z-ADD     wFVADFV       ANMDAO
316400100713     C                   Z-ADD     wFVADFV       SAVDAO
316500100713     C                   CLEAR                   WLIN
316600100713     C                   MOVEL     '1'           WMODL             1
316700100713     c                   endif
316800950927     C*
316900950927     C* SE IL FV ANCORA NON C'E' CERCO SE ANCORA DA ABBINARE
317000021121    1C     WMODL         IFEQ      ' '
317100950927     C                   CLEAR                   KNFV4
317200950927     C                   CLEAR                   KNPG4
317300020214     C                   MOVE      SAVTFA        KFGS4
317400000223     C* Determino BARDFV-4 gg. che è la data minima che deve avere un
317500000223     C* foglio ancora da abbinare. Se la data foglio è più bassa di
317600000223     C* tale data il foglio verrà scartato
317700000223     C                   MOVE      *ZEROS        DATMIN
317800000223     C                   CLEAR                   WLBDAT
317900000223     C                   CLEAR                   WGIDAT
318000000223     C                   Z-ADD     BARDFV        G02INV
318100000223     C                   MOVEL     '3'           G02ERR
318200000223     C                   CALL      'XSRDA8'
318300000223     C                   PARM                    WLBDAT
318400000223     C     G02TGI        SUB       4             GI8TGI
318500000223     C                   CALL      'XSRGI8'
318600000223     C                   PARM                    WGIDAT
318700000223     C                   Z-ADD     GI8INV        DATMIN
318800950927     C                   EXSR      REDFVA
318900950927     C* SE ANCORA NON HO TROVATO NIENTE --> ESCO DAL CICLO
319000950927    2C     WMODL         IFEQ      ' '
319100950927     C                   CLEAR                   WLIN
319200950927    2C                   ENDIF
319300950927    1C                   ENDIF
319400990827     C*
319500990827     C* PULISCO WLIN PER NON CONTINUARE NELLA RICERCA DEL FOGLIO
319600990827     C* SE LA LINEA DI ARRIVO RISULTA TRANSITARE
319700990827     C* DA UN P.O. CHE  E' LO STESSO DAL QUALE IL COLLO ERA PARTITO
319800990827     C* (PER ES.: 53/31 CHE TRANSITA DA 062 E 062 LO FA TRANSITARE DA
319900990827     C* 53 (CASO CHE SI VERIFICA DURANTE IL PASSAGGIO DALLE TRAZIONI
320000990827     C* RIDOTTE ALLE TRAZIONI NORMALI))
320100990827     C     WLINPR        IFGT      *ZEROS
320200990827     C     WLIN          ANDGT     *ZEROS
320300990827     C     WLIN          ANDEQ     WVALNP
320400990827     C                   CLEAR                   WLIN
320500990827     C                   ENDIF
320600021121     C                   ENDIF
320700950927     C*
320800950927     C                   ENDSR
320900950927     C**************************************************************************
321000950927     C* LETTURA DEI DEI FOGLI VIAGGIO PARTENZA
321100950927     C**************************************************************************
321200950927     C     REDFVA        BEGSR
321300950927     C                   CLEAR                   WMODL
321400021121     C                   CLEAR                   Wabbinati         1
321500950927     C     KFVA4         SETLL     FNFVA000
321600950927     C     KFVA4         READE     FNFVA000                               99
321700021121     c* se 99 on - nessun foglio abbinato --> memorizzo
321800021121     c                   if        *in99
321900021121     c                   eval      wabbinati='N'
322000021121     c                   endif
322100950927     C*
322200950927    1C     *IN99         DOWEQ     *OFF
322300950927    2C     FVALNP        IFEQ      WPTFP
322400950927     C* DATA FOGLIO DEVE ESSERE <= ALLA DATA DI ARRIVO
322500960524     C     FVADFV        ANDLE     BARDFV
322600000223     C     KNPG4         ANDGT     *ZEROS
322700000223     C* SE STO CERCANDO FOGLIO ANCORA DA ABBINARE SCARTO SE E' DI DATA
322800000223     C* INFERIORE ALLA DATA MINIMA (DATA ARRIVO COLLO MENO QUATTRO GG.)
322900000223     C     FVALNP        OREQ      WPTFP
323000000223     C     FVADFV        ANDLE     BARDFV
323100000223     C     FVADFV        ANDGE     DATMIN
323200000223     C     KNPG4         ANDEQ     *ZEROS
323300960207     C*
323400950927     C* SE SI TRATTA DI UN FOGLIO DIRETTO TUTTO A POSTO
323500100712     c* ok anche se locale
323600950927    3C     FVAATR        IFEQ      ' '
323700100712     c     fvattr        oreq      'L'
323800950927     C                   MOVEL     FVALNP        ANMCDU
323900020221     C                   Z-ADD     FVALNP        SAVFLP
324000990518     C                   Z-ADD     FVANFV        ANMNFV
324100950927     C                   Z-ADD     FVADFV        ANMDAO
324200980120     C                   Z-ADD     FVADFV        SAVDAO
324300950927     C                   CLEAR                   WLIN
324400950927     C                   MOVEL     '1'           WMODL             1
324500980120     C* SE IL PRIMO FV TROVATO E' UN DIRETTO LO MEMORIZZO PER SAPERE SE
324600980120     C* DOVRO' AGGIOR.LA DATA DI PARTENZA O LA DATA USCITA DAL TRANSITO
324700980120     C     WFLGT         IFEQ      *BLANKS
324800980120     C                   MOVE      'D'           WFLGT
324900980120     C                   END
325000950927     C                   SETON                                        99
325100960207     C*
325200950927   X3C                   ELSE
325300960207     C* SE E' FOGLIO ILLUMINATO
325400950927     C* DEVO VEDERE DA DOVE E' TRANSITO E MANDARE AL TRANSITO LA SEGNA
325500950927     C*  LAZIONE
325600000505     C     KFWA          CHAIN     FNFWA01L                           28
325700000505     C     *IN28         IFEQ      *ON
325800000505     C     FWAATB        ORNE      ' '
325900000505     C                   CLEAR                   FWAFF3
326000000505     C                   CLEAR                   FWAFF4
326100000505     C                   CLEAR                   FWAFL3
326200000505     C                   CLEAR                   FWAFL4
326300000505     C                   ENDIF
326400000505     C**
326500950927     C                   Z-ADD     FVALNA        LNA2
326600950927     C                   Z-ADD     1             X
326700951002     C                   MOVEL     BARLNA        W003A             3
326800950927     C     W003A         LOOKUP    FFV(X)                                 99
326900960207     C* NON E' IL MIO TERMINAL
327000960902     C* ESCLUDO I FOGLI LOCALI
327100021125     c* in relatà elaboro anche i fogli locali, altrimenti come faccio a
327200021125     c*  determinare un collo arrivo no part, del terminal per la lna?
327300021125     c*  ES. 43  x 132
327400960207    4C     *IN99         IFEQ      *ON
327500021125     C***  FVATTR        ANDNE     'L'
327600050603     C                   MOVEL     Usalnatfa     W003A             3
327700960522    5C     FLP(X)        IFNE      W003A
327800960207     C                   MOVEL     FLP(X)        WLIN
327900020214     C                   MOVE      FVADFV        WDTRIF
328000960207     C                   MOVEL     '1'           WMODL             1
328100980120     C* SE IL PRIMO FV TROVATO E' UN TRANSITO LO MEMORIZZO PER SAPERE
328200980120     C* SE DOVRò AGGIOR. DATA DI PARTENZA O DATA USCITA DAL TRANSITO
328300980120     C     WFLGT         IFEQ      *BLANKS
328400980120     C                   MOVE      'T'           WFLGT
328500021121     c* Memorizzo lo stesso i campi che mi servono perchè se poi di tratta
328600021121     c*  di transito "locale" cioè su stesso as (vedi 82 con 113)
328700021121     c*  devo comunque tenere i dati del foglio illuminato non essendoci
328800021121     c*  abbinamento del foglio locale su stessa £1
328900021121     c*  Il problema verrà a morire quando diventeranno dei 1 livello
329000021121     c*  come devono essere
329100021121     c*
329200021121     c                   z-add     FVALNP        WFVALNP
329300021121     c                   z-add     FVANFV        WFVANFV
329400021121     c                   z-add     FVADFV        WFVADFV
329500980120     C                   END
329600960207   X5C                   ELSE
329700960207     C* SE E' FOGLIO ILLUMINATO PER 2 LIVELLO IN ARRIVO TUTTO A POSTO
329800960207     C                   MOVEL     FVALNP        ANMCDU
329900990518     C                   Z-ADD     FVANFV        ANMNFV
330000960207     C                   Z-ADD     FVADFV        ANMDAO
330100980120     C                   Z-ADD     FVADFV        SAVDAO
330200020221     C                   Z-ADD     FVALNP        SAVFLP
330300960207     C                   CLEAR                   WLIN
330400960207     C                   MOVEL     '1'           WMODL             1
330500980120     C* SE IL PRIMO FV TROVATO E' UN DIRETTO LO MEMORIZZO PER SAPERE SE
330600980120     C* DOVRO' AGGIOR.LA DATA DI PARTENZA O LA DATA USCITA DAL TRANSITO
330700980120     C     WFLGT         IFEQ      *BLANKS
330800980120     C                   MOVE      'D'           WFLGT
330900980120     C                   END
331000960207    5C                   ENDIF
331100960207     C*
331200960207   X4C                   ELSE
331300960207     C     KFVA4         READE     FNFVA000                               99
331400960207    4C                   ENDIF
331500950927    3C                   ENDIF
331600950927     C*
331700950927   X2C                   ELSE
331800950927     C     KFVA4         READE     FNFVA000                               99
331900950927    2C                   ENDIF
332000950927    1C                   ENDDO
332100950927     C                   ENDSR
332200980119     C**************************************************************************
332300980119     C* AGGIORNO DATA PART. O DI USCITA DA TRANS. A FRONTE DI ANOM.50
332400980119     C**************************************************************************
332500980119     C     AGGPAR        BEGSR
332600980119     C     KBAR          CHAIN     FNART27L                           3239
332700980119     C   39              GOTO      ENDPAR
332800160722     c
332900160722    1c     *IN32         IFEQ      *OFF
333000160722     c* Spunta di bolla in arrivo ( anche locale)
333100160722    2C                   if        WTIBOL='A'  or WTIBOL='L'
333200160722
333300980120    3C                   SELECT
333400980120     C     WFLGT         WHENEQ    'D'
333500980120     C                   Z-ADD     SAVDAO        ARTDFV
333600041201     C                   Z-ADD     anmnfv        ARTnFV
333700980120     C                   Z-ADD     88            ARTNPP
333800041202     C                   movel     'S'           ARTABN
333900020218     C     WFLGT         WHENEQ    'L'
334000020218     C                   Z-ADD     SAVDAO        ARTDFV
334100041201     C                   Z-ADD     anmnfv        ARTnFV
334200020218     C                   Z-ADD     90            ARTNPP
334300041202     C                   movel     'S'           ARTABN
334400980120     C     WFLGT         WHENEQ    'T'
334500980120     C                   Z-ADD     SAVDAO        ARTDUT
334600980120     C                   Z-ADD     89            ARTPUT
334700020221     C                   Z-ADD     SAVFLP        ARTFLP
334800041202     C                   movel     'S'           ARTABN
334900980120    3C                   ENDSL
335000160722    2c                   endif
335100160722     c*
335200160722     c* spunta al transito: devo aggiornare anche  in FNART
335300160722     c*  la data di partenza se non c'è
335400160722    2c                   if        WTIBOL='T'
335500160722     c
335600160722    3c                   if        artdfv=0
335700160722     C                   Z-ADD     SAVDAO        ARTDFV
335800160722     C                   Z-ADD     anmnfv        ARTnFV
335900160722     C                   Z-ADD     88            ARTNPP
336000160722    3c                   endif
336100160722    2c                   endif
336200160720
336300980120     C                   UPDATE    FNART000
336400980120     C                   MOVEL     'A'           WTBO              1
336500980120     C                   MOVEL     ARTSPE        WDSPE
336600980120     C                   EXSR      WR1AGB
336700160722    1C                   endif
336800160720
336900160722     C                   if        WTIBOL='T'
337000021203     C     KBAR37        CHAIN     FNBTT37L                           3739
337100980119     C   39              GOTO      ENDPAR
337200161108    2C     *IN37         IFEQ      *OFF
337300980120     C                   Z-ADD     SAVDAO        BTTDFV
337400980120     C                   Z-ADD     88            BTTNPP
337500980120     C                   UPDATE    FNBTT000
337600980120     C                   MOVEL     'T'           WTBO              1
337700980120     C                   MOVEL     BTTSPE        WDSPE
337800980120     C                   EXSR      WR1AGB
337900980120    2C                   END
338000160722    1C                   endif
338100160720
338200160720     C*  AGGIORNO ANCHE LA BOLLA PARTENZA
338300020218    1C     WTIBOL        IFEQ      'L'
338400160720     C     WTIBOL        oreq      'A'
338500160721     C     WTIBOL        oreq      'T'
338600020218     C     KBAR          CHAIN     FNBLT27L                           3239
338700020218     C   39              GOTO      ENDPAR
338800020218    2C     *IN32         IFEQ      *OFF
338900160721
339000160721     c                   if        wtibol<>'T'
339100160721     c                   z-add     anmdch        bltdam
339200160721     c                   else
339300160721     c                   z-add     anmdch        bltdet
339400160721     c                   z-add     bpes          bltflp
339500160722     c                   clear                   bltdut
339600160721     c                   endif
339700160720
339800160721    3C                   IF        WFLGT<>'T' AND
339900160721     c                             (bltdfv=0 or bltdfv>savdao) or
340000160721     c                             wflgt= 'T' and bltdfv=0
340100160721     c
340200160720     C                   Z-ADD     SAVDAO        bltDFV
340300160726     c                   eval      welasta_p='S'
340400160720
340500160721    4c                   select
340600160720     C     BLTDSE        WHENEQ    0
340700160720     c                   if        wflgt='D'
340800160720     C                   MOVEL     88            BLTNPP
340900160721     c                   else
341000160720     C                   Z-ADD     90            bltNPP
341100160720     c                   endif
341200160720     C     BLTDSE        WHENGT    0
341300160720     C     BLTDSE        ANDLT     BLTDFV
341400160720     C                   MOVEL     96            BLTNPP
341500160720     C     BLTDSE        WHENGT    0
341600160720     C     BLTDSE        ANDGE     BLTDFV
341700160720     C                   MOVEL     98            BLTNPP
341800160721    4C                   ENDSL
341900160720
342000160721    3c                   endif
342100160720
342200160722    3C                   if        wflgt='T'  and WTIBOL<>'T' and
342300160722     c                             (bltdut=0 or bltdut>savdao)
342400160720     C                   Z-ADD     SAVDAO        bltDUT
342500160722     C                   Z-ADD     bpes          bltFLP
342600160721    3c                   endif
342700160720     c
342800020218     C                   UPDATE    FNBLT000
342900020218     C                   MOVEL     'P'           WTBO              1
343000020218     C                   MOVEL     BLTSPE        WDSPE
343100020218     C                   EXSR      WR1AGB
343200160721     c
343300160721     c* se c'è transito, aggiorno anche BTT
343400160721    3C     WFLGT         ifeq      'T'
343500160721     C     KBAR_t        CHAIN     FNBTT37L                           3239
343600160721    4C     *IN32         IFEQ      *OFF
343700160721     C     *IN39         andeq     *OFF
343800160721
343900160726     c                   eval      welasta_p='S'
344000160721     C                   Z-ADD     SAVDAO        bttDUT
344100160721     c                   z-add     anmdch        bttdam
344200160721     C                   SELECT
344300160721     C     BTTDET        WHENEQ    0
344400160721     C                   MOVEL     89            BTTPUT
344500160721     C     BTTDET        WHENGT    0
344600160721     C     BTTPET        ANDEQ     89
344700160721     C                   MOVEL     89            BTTPUT
344800160721     C     BTTDET        WHENGT    0
344900160721     C     BTTDET        ANDLT     BTTDUT
345000160721     C                   MOVEL     96            BTTPUT
345100160721     C     BTTDET        WHENGT    0
345200160721     C     BTTDET        ANDGE     BTTDUT
345300160721     C                   MOVEL     98            BTTPUT
345400160721     C                   ENDSL
345500160721     C*
345600160721     C     BTTDET        IFEQ      0
345700160721     C                   Z-ADD     ANMDAO        BTTDET
345800160721     C                   MOVEL     89            BTTPET
345900160721     C                   ENDIF
346000160721     C                   UPDATE    FNBTT000
346100160721     C                   MOVEL     'T'           WTBO              1
346200160721     C                   MOVEL     BLTSPE        WDSPE
346300160721     C                   EXSR      WR1AGB
346400160721    4C                   ENDif
346500160721    3C                   ENDif
346600160720
346700020218    2C                   END
346800020218    1C                   END
346900160721
347000980119     C     ENDPAR        TAG
347100160721
347200980119     C   39              SETON                                        40
347300160726     c
347400160726     c                   clear                   welasta_p
347500160726     c
347600980119     C                   ENDSR
347700160727     C**************************************************************************
347800160727     C* AGGIORNO DATA entrata al transito per DIsguido
347900160727     C**************************************************************************
348000160727     C     AGGPAR_d      BEGSR
348100160727     C     KBAR          CHAIN     FNBLT27L                           3239
348200160727    2C     *IN32         IFEQ      *OFF
348300160727    2C     *IN39         andeq     *OFF
348400160727     C                   Z-ADD     SAVDAO        bltDET
348500160727     c                   clear                   bltdut
348600160727     c                   movel     barfgs        bltflp
348700160727     c                   update    fnblt000
348800160727
348900160727     C                   MOVEL     'P'           WTBO              1
349000160727     C                   MOVEL     BLTSPE        WDSPE
349100160727     C                   EXSR      WR1AGB
349200160727     c                   endif
349300160727     c
349400160727     C                   ENDSR
349500950927     C**************************************************************************
349600950927     C* MEMORIZZO I FOGLI ARRIVI CETEGORIA 2 E 6 DEL GIORNO
349700950927     C**************************************************************************
349800950927     C     MEMFAR        BEGSR
349900021121     c                   clear                   wsavdata          8 0
350000021113     c
350100021113     c* se è collo di bolla transito
350200021113     c*  devo usare simfel, il suo p.o. di transito
350300021113     c                   if        wtibol<>'T'
350400050603     C                   MOVEL     usalnatfa     KTFA2
350500021113     c                   else
350600021113     C                   z-add     simfel        KTFA2
350700021113     c                   endif
350800021113     c
350900950927     C* RIMEMORIZZO I FV SOLO SE E' CAMBIATA LA  DATA
351000021121    1C     wdata         IFNE      SAVDFV
351100970113     C     KTFA2         ORNE      SAVTFA
351200021107     c
351300021121     c* Cerco fogli cat 2 e 6  se non trovati in data BARDFV, li cerco
351400021107     c*  in data bardfv - 1
351500021113     c* faccio max 8 tentativi (1 settimana  tenendo conto che ci possono
351600021113     c*  essere dei festivi)
351700950927     C                   CLEAR                   PG2
351800021113     c                   z-add     0             wgiorni           2 0
351900021113     c
352000021113    2c                   dow       wdata>0  and wgiorni<8
352100021113     c                   add       1             wgiorni
352200950927     C*
352300950927     C                   Z-ADD     2             WNPG
352400950927     C     KFVV          SETLL     FNFVV002
352500950927     C     KFVV          READE     FNFVV002                               33
352600950927     C                   Z-ADD     1             I
352700950927     C*
352800950927     C     *IN33         DOWEQ     *OFF
352900950927     C                   Z-ADD     FV2NFV        PG2(I)
353000950927     C                   ADD       1             I
353100950927     C     KFVV          READE     FNFVV002                               33
353200950927     C                   ENDDO
353300021107     c*
353400021107     c                   xfoot     pg2           wpg2             13 0
353500110103     c                   if        wpg2>0
353600021121     c                   eval      wsavdata=wdata
353700021107     c                   clear                   wdata
353800021107     c                   else
353900021107     c* Data - 1
354000021107     C                   MOVEL     wdata         G02INV
354100021107     C                   Z-ADD     *ZEROS        G02DAT
354200021107     C                   Z-ADD     *ZEROS        G02TGI
354300021107     C                   MOVEL     '3'           G02ERR
354400021107     C                   CALL      'XSRDA8'
354500021107     C                   PARM                    WLBDAT
354600021107      *
354700021107     C                   SUB       1             G02TGI
354800021107     C                   Z-ADD     *ZEROS        GI8DAT
354900021107     C                   Z-ADD     *ZEROS        GI8INV
355000021107     C                   Z-ADD     G02TGI        GI8TGI
355100021107     C                   CALL      'XSRGI8'
355200021107     C                   PARM                    WGIDAT
355300021107      *
355400021107     C                   Z-ADD     GI8INV        wdata
355500021107     c                   endif
355600021107     c                   enddo
355700021121     c*
355800970113     C                   MOVEL     KTFA2         SAVTFA
355900021107    1C                   ENDIF
356000950927     C                   ENDSR
356100950927     C**************************************************************************
356200950927     C* RIEMPO PARTE DEI CAMPI DELLA ANOMALIA
356300950927     C**************************************************************************
356400950927     C     RIEANM        BEGSR
356500950927     C                   MOVEL     BARLNP        ANMFLS
356600950927     C                   MOVEL     BARLNA        ANMLNA
356700950927     C                   MOVEL     BARNRS        ANMNRS
356800950927     C                   MOVEL     BARNSC        ANMSCN
356900950927     C                   MOVEL     BARNPS        ANMNPS
357000950927     C                   Z-ADD     COMCAA        ANMCAA
357100040621     c                   z-add     999           anmfl4
357200040621     c* imposto in anmflo data e ora creazione spunta
357300050707     C                   TIME                    TIMEanm          14 0
357400050707     c                   movel     timeanm       wora4             4 0
357500050707     C                   move      timeanm       G02DAT
357600050707     C                   Z-ADD     *ZERO         G02INV
357700050707     C                   MOVEL     *BLANKS       G02ERR
357800050707     C                   CALL      'XSRDA8'
357900050707     C                   PARM                    WLBDAT
358000050707     C                   move      G02INV        WDAT6             6 0
358100040621     c                   movel     wdat6         anmflo
358200040621     c                   move      wora4         anmflo
358300950927     C* FASE APERTURA
358400950927     C     BARKEY        IFNE      SAVFOG
358500950927     C                   EXSR      RICFAS
358600950927     C                   END
358700950927     C                   MOVEL     COMFAS        ANMFSA
358800950927     C* CODICE ANOMALIA
358900950927     C     COMCAA        IFNE      SAVCAA
359000950927     C                   MOVEL     '7C'          COD
359100950927     C                   MOVEL(P)  COMCAA        KEY
359200950927     C     KTAB          CHAIN     TABEL                              34
359300950927     C   34              CLEAR                   DS7C
359400950927     C  N34              MOVEL     TBLUNI        DS7C
359500950927     C                   MOVEL     COMCAA        SAVCAA            3 0
359600950927     C                   ENDIF
359700950927     C*
359800950927     C                   MOVEL     §7CTAN        ANMTAN
359900950927     C                   MOVEL     §7CAIE        ANMAIE
360000950927     C                   MOVEL     §7CSAN        ANMSAN
360100950927     C                   MOVEL     §7CLID        ANMLID
360200950927     C* AUTOCHIUSA
360300950927     C     §7CCHA        IFEQ      'S'
360400960524     C                   MOVE      BARDFV        ANMDCH
360500950927     C                   MOVEL     COMFAS        ANMFSC
360600000320     C                   MOVEL     WCCH          ANMCCH
360700950927     C                   END
360800950927     C                   ENDSR
360900960523     C**************************************************************************
361000960523     C* CARICO LA L6 DELLA FILIALE GESTIONE FOGLIO
361100960523     C**************************************************************************
361200960523     C     CARL6         BEGSR
361300960523     C                   CLEAR                   DSUL06
361400960523     C                   MOVE      '£6'          D06COD
361500020213     C                   MOVEL     SAVPES        D06KEY
361600960523     C                   MOVEL     DSUL06        KPJBU
361700960523     C*
361800960523     C                   CALL      'TRUL06R'
361900960523     C                   PARM                    KPJBA
362000960523     C                   MOVEL     KPJBU         DSUL06
362100960523     C                   MOVEA     LIN           L6
362200960523     C                   ENDSR
362300960523     C**************************************************************************
362400960523     C* SE ESISTE GIA' UNA SPUNTA DELLA LNA, NON CREO ANOMALIE 145-146
362500960523     C**************************************************************************
362600960523     C     CTRSPU        BEGSR
362700960523     C                   MOVEL     'S'           WCREA
362800970117     C*
362900970117     C                   MOVEL     'S'           WCHIU             1
363000970117     C*"WCHIU" MI SERVE PER DETERMINARE SE POSSO CHIUDERE UNA ANOMALIA
363100970117     C* 145/146 CHE HA DATA ANOMALIA < DELLA DATA DELLA SPUNTA CHE
363200970117     C*  STO ELABORANDO MA POTREBBE COMUNQUE ESSERCI UNA SPUNTA
363300970117     C*  CHE MI MANTIENE APERTA IN DATA > TALE ANOMALIA
363400970117     C*  1) SE E' PRESENTE UN'ALTRA SPUNTA CON DATA > NON CHIUDO
363500970117     C*  2) SE E' PRESENTE UNA SPUNTA CON DATA = CATEGORIA 3 4 5
363600020204     C*     NON CHIUDO SE LA SPUNTA CHE STO SCARICANDO E' CAT 1 2 6
363700970117     C*  3) SE E' PRESENTE UNA SPUNTA CON DATA = CATEGORIA 3 4 5
363800970117     C*     E E' UNA SPUNTA DELLA LNA SE LA SPUNTA CHE STO SCARICANDO
363900970117     C*     E' CATEGORIA 3 4 5
364000960531     C**
364100020204    1C     WAGARR        IFEQ      'S'
364200960531     C**
364300070131     C     KBRV7         CHAIN     FNBRV07L                           33
364400960531    2C     *IN33         DOWEQ     *OFF
364500960531    3C     BR7KEY        IFNE      SA1KEY
364600960527     C                   MOVEL     BR7KEY        SA1KEY
364700960527     C                   Z-ADD     BR7NPG        WNPG
364800960527     C                   Z-ADD     BR7NFV        WNFV
364900021014     C                   Z-ADD     BR7FGS        WFGS
365000070906     C                   movel     BR7nps        Wnps
365100021014     C                   CLEAR                   RICBAR
365200960523     C                   EXSR      RILFOG
365300960523     C                   Z-ADD     WDFV          BRVDFV
365400140616     C                   movel     WFCF          BRVFCF
365500140616     C                   movel     Wspg          BRVspg
365600960531    3C                   ENDIF
365700040224     c* Terminal di arrivo della spunta letta
365800040224     C                   CLEAR                   DSLV55
365900040224     C                   MOVEL     'A'           D55TPT
366000040224     C                   MOVEL     LNPB          D55LNP
366100040224     C                   MOVEL     brvdfv        D55DRF
366200040224     C                   MOVE      Br7pes        D55LIN
366300040224     C                   EXSR      FNLV55
366400050603     c                   if        d55tfa=Usalnatfa
366500960531     C**
366600961210    3C     BRVDFV        IFGT      BARDFV
366700960523     C                   MOVEL     'N'           WCREA             1
366800970117     C                   MOVEL     'N'           WCHIU             1
366900961209   X3C                   ELSE
367000961209     C*
367100961210    4C     BRVDFV        IFEQ      BARDFV
367200961210     C     BR7NPG        ANDGE     3
367300961210     C     BR7NPG        ANDLE     5
367400970117     C*
367500970117     C     BARNPG        IFEQ      1
367600970117     C     BARNPG        OREQ      2
367700970117     C                   MOVEL     'N'           WCHIU
367800970117     C                   ENDIF
367900961209     C*
368000020213    5C     BR7PES        IFEQ      BARLNA
368100961209     C                   MOVEL     'N'           WCREA             1
368200970117     C*
368300970117     C     BARNPG        IFNE      1
368400970117     C     BARNPG        ANDNE     2
368500970117     C                   MOVEL     'N'           WCHIU
368600970117     C                   ENDIF
368700970117     C*
368800961209   X5C                   ELSE
368900020214    6C     BR7PES        IFNE      SAVPES
369000020214     C                   MOVEL     BR7PES        SAVPES
369100961209     C                   EXSR      CARL6
369200961209    6C                   ENDIF
369300961209     C*
369400961209     C     BARLNA        LOOKUP    L6                                     34
369500961209     C   34              MOVEL     'N'           WCREA             1
369600970117     C*
369700970117    6C   34BARNPG        IFNE      1
369800970117     C     BARNPG        ANDNE     2
369900970117     C                   MOVEL     'N'           WCHIU
370000970117    6C                   ENDIF
370100970117     C*
370200961209    5C                   ENDIF
370300961209    4C                   ENDIF
370400961209    3C                   ENDIF
370500040224    3C                   ENDIF
370600961209     C*
370700961209    3C     WCREA         IFEQ      'N'
370800961209     C                   SETON                                        33
370900961210     C                   ELSE
371000070131     C     KBRV7         READE     FNBRV07L                               33
371100961209    3C                   ENDIF
371200040224     C**
371300960531    2C                   ENDDO
371400960531     C*
371500960531    1C                   ENDIF
371600960523     C*
371700960523     C                   ENDSR
371800961202     C**************************************************************************
371900961202     C* CREO ANOMALIA 145 O 146 E CHIUDO L'ALTRA
372000961202     C**************************************************************************
372100961202     C     ANM14X        BEGSR
372200961202     C* LA CREO SOLO SE NON ESISTE UNA SPUNTA DELLA LNA CON DATA=>
372300961210     C     WDCM          IFGT      0
372400961210     C     WDCM          ANDGE     BARDFV
372500961210     C                   MOVEL     'N'           WCREA
372600961210     C                   ELSE
372700961202     C                   EXSR      CTRSPU
372800961210     C                   ENDIF
372900961202     C*
373000961202    5C     WCREA         IFEQ      'S'
373100020213     C     BARFLE        ANDEQ     BTFP
373200020213     C     BARFLE        ORNE      BTFP
373300961202     C                   Z-ADD     CAACRE        COMCAA
373400961202     C                   EXSR      CRTANM
373500961202     C* SOLO PER EXPORT
373600961202     C*
373700961202     C     CAACRE        IFEQ      146
373800961202    6C     WTIBOL        ANDNE     'A'
373900961202     C                   MOVEL     'S'           WTIBOL
374000961202    6C                   ENDIF
374100961202    5C                   ENDIF
374200961202     C*
374300961202     C* SE SPUNTA DA ALTRO AS MA NON DOVEVO CREARE IL COLLO DA INOLTRAR
374400961202     C*  LA CHIUDO SUBITO. ANCHE SE IL COLLO E' CONSEGNATO SENZA SPUNTE
374500020213    5C     BARFLE        IFNE      BTFP
374600961202    6C     WCREA         IFNE      'S'
374700961202     C                   Z-ADD     CAACRE        COMCAA
374800961211     C* IN QUESTO CASO NON DEVO CREARE L'ANOMALIA 620 IN QUANTO
374900961211     C*  LA SPEDIZIONE E' GIA' STATA CONSEGNA A COMUNQUE E' PRESENTE
375000961211     C*  UNA SPUNTA CON DATA >.
375100961211     C                   MOVEL     'N'           W620
375200961202     C                   EXSR      CHIUAN
375300961211     C                   MOVEL     'S'           W620
375400961211     C*
375500961209   X6C                   ELSE
375600961210     C                   EXSR      ANM600
375700961202    6C                   ENDIF
375800961209     C*
375900961202   X5C                   ELSE
376000961210    6C     WCREA         IFEQ      'S'
376100961209     C* PER ESTERO CREO ANOMALIA 645 E 600
376200961209     C                   EXSR      ANM600
376300961210    6C                   ENDIF
376400961210    5C                   ENDIF
376500961202     C*
376600961210     C                   MOVEL     'N'           W620              1
376700961202     C                   Z-ADD     CAACIU        COMCAA
376800961202     C                   EXSR      CHIUAN
376900961210     C                   MOVEL     'S'           W620              1
377000961202     C                   ENDSR
377100961213     C**************************************************************************
377200961213     C* LETTURA FOGLIO DELLA SPUNTA CHE STO CARICANDO E IMPOSTAZIONE
377300961213     C*  CAMPI DI COMOD
377400961213     C**************************************************************************
377500961213     C     BARFGV        BEGSR
377600961213     C*
377700961213    3C     BARKEY        IFNE      SAVKEY
377800961213     C                   MOVEL     BARKEY        SAVKEY
377900961213     C                   Z-ADD     BARNPG        WNPG
378000961213     C                   Z-ADD     BARNFV        WNFV
378100021014     C                   Z-ADD     BARFGS        WFGS
378200070906     C                   movel     BARnps        Wnps
378300021014     C                   MOVEL     'S'           RICBAR            1
378400961213     C                   EXSR      RILFOG
378500980313     C                   MOVEA     FFG           FFS
378600961213     C                   Z-ADD     WDFV          BARDFV
378700961213     C                   MOVEL     WSPG          BARSPG
378800021025     C                   MOVEL     WFGS          wwwFGS
378900021023     C                   Z-ADD     WFLE          BARFLE
379000140616     C                   movel     WFCF          BARFCF
379100970521     C*
379200970521     C     WTTR          IFNE      *BLANKS
379300970520     C     WTTR          LOOKUP    TTR                                    34
379400970520     C                   MOVE      *IN34         WDEFL             1
379500970521     C                   ELSE
379600970521     C                   MOVE      '0'           WDEFL             1
379700970521     C                   ENDIF
379800970521     C*
379900961213    3C                   ENDIF
380000021025     C* IL P.O. GESTIONE LO DEVO SEMPRE IMPOSTARE PERCHÈ PUÒ CAMBIARE
380100021031     C                   MOVEL     WWWFGS        BARFGS
380200070209     c
380300070209     c* Se si tratta di spunta partenza, il BPES deve essere il P.O foglio
380400070209     c*  per problemi con eventuale creazione di anomalie 200
380500070209     c                   eval      wpes=bpes
380600070209     c                   if        barnpg=1
380700070209     c                   eval      bpes=barfgs
380800070209     c                   endif
380900961213     C                   ENDSR
381000970203     C**************************************************************************
381100970203     C* ANOMALIE CHE DEVO SEMPRE CREARE O SEMPRE CHIUDERE SE COLLO
381200970203     C*  SPARATO NELL'AREA DI ARRIVO (COME TERMINAL DI ARRIVO)
381300970203     C**************************************************************************
381400970203     C     ANMSEM        BEGSR
381500970203     C*
381600970203     C* COLLO/BOLLA ARRIVI PRESENTE
381700970204     C* SE CONSEGNATO IL COLLO CREO SEMPRE ANOMALIA --> COLLO CONSEGNAT
381800970204    1C  N05WBOAR         IFEQ      'S'
381900990120    2C     BARDFV        IFGT      WATDCM
382000970204     C     WATDCM        ANDGT     0
382100170328     c* oppure senza consegna ma con cons anomala "S"
382200170328     c     watdcm        oreq      0
382300170328     c     wabcca        andeq     'S'
382400170505     c
382500170505     c*
382600170505     c* se bolla con cons anomala legata, la bolla figlia
382700170505     c*  DEVE essere borderizzata per dare anomalia 138
382800170505     c*  per semplificare leggo della bolla figlia solo il 1° dettaglio colli
382900170505     c                   eval      w138='S'
383000170505    3c                   if        wabcca<>' '  and comnsp>0
383100170505     c     kbolla        chain     fnlbl02l
383200170505    4c                   if        %found(fnlbl02l)
383300170505     c     klbl          chain(n)  fnblp01l
383400170505    5c                   if        %found(fnblp01l) and
383500170505     c                              blpft2=' '
383600170505     c                   eval      w138='N'
383700170505    5c                   endif
383800170505    4c                   endif
383900170505    3c                   endif
384000170505     c
384100170505     c                   if        w138='S'
384200970203     C                   Z-ADD     138           COMCAA
384300970203     C                   EXSR      CRTANM
384400170505     c                   endif
384500170505     c
384600990120    2C                   ENDIF
384700170505     c
384800000705     C* SE COLLO CAT.8 SEGNALA SE LA RELATIVA DISTINTA 4 E'
384900000705     C*  GIA' CHIUSA CON ANOMALIA 139
385000040430     c                   if        watdcm>0  and barnpg=8
385100000705     C* SE NON E' CHIUSA VEDO SE IN FASE DI CHIUSURA: CHIAMO FNLV53R
385200000705     C                   CLEAR                   DSLV53
385300000705     C                   MOVEL     'V'           D53TFO
385400020213     C                   MOVEL     BTFP          D53FEL
385500000705     C                   Z-ADD     BARNFV        D53NFV
385600000705     C                   MOVEL     4             D53NPG
385700000705     C                   MOVEL     BARFGS        D53FGS
385800000705     C                   MOVEL     BARFLE        D53FLF
385900000705     C                   CALL      'FNLV53R'
386000000705     C                   PARM                    DSLV53
386100000705     C*
386200000705     C* FOGLIO CHIUSO (S) O IN FASE DI CHIUSURA (F)
386300000705    3C     D53FCF        IFEQ      'S'
386400000705     C     D53FCF        OREQ      'F'
386500040430     c* non creo se la relativa consegna anomala non lo prevede
386600040430    4c                   if        WABCCA<>*blanks and wabcca<>savcca
386700040430     C                   MOVEL     '7O'          COD
386800040430     C                   MOVEL(P)  wabcca        KEY
386900040430     C     KTAB          CHAIN     TABEL                              41
387000040430     C  N41              MOVEL     TBLUNI        DS7o
387100040430     c   41              clear                   §7o139
387200040430     c                   eval      savcca=wabcca
387300040430    4c                   endif
387400040430     c
387500040430    4c                   if        §7o139=*blanks or wabcca=*blanks
387600000705     C                   Z-ADD     139           COMCAA
387700000705     C                   EXSR      CRTANM
387800040430    4C                   ENDIF
387900040430    3C                   ENDIF
388000000705    2C                   ENDIF
388100000705     C*
388200990120     C* COLLO NON CONSEGNATO
388300990121    2C     WATDCM        IFEQ      0
388400170328     c     wabcca        andne     'S'
388500990120     C* VERIFICO SE DEVO CREARE ANOMALIA SU DANNI
388600000523     C*  NON LO CREO PER LA CATEGORIA 4 E 8
388700990121     C                   MOVEL     'N'           WCAT4             1
388800990126     C* NON GESTISCO IL RITROVAMENTO COLLO
388900990126     C                   MOVEL     'N'           WRITRO
389000990126     C*     GESTISCO LA CREAZIONE ANOMALIE
389100990126     C                   MOVEL     'S'           WCREAN
389200990120     C                   EXSR      CTRDAN
389300990120     C** SE NON TROVATA CA DI COLLO
389400990120     C* VERIFICO SE LA SPEDIZIONE E' SU UNA CA CHIUSA --> CREO
389500990120    3C     WTRDAN        IFEQ      ' '
389600040811      *
389700040811      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
389800040811      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
389900040811      * legate alla spedizione che passo
390000040811     c                   clear                   fidn12ds
390100170505     c***                eval      i12aas = ARTAAS
390200170505     c***                eval      i12lnp = ARTLNP
390300170505     c***                eval      i12nrs = ARTNRS
390400170505     c***                eval      i12nsp = ARTNSP
390500170505     c                   eval      i12aas = comAAS
390600170505     c                   eval      i12lnp = comLNP
390700170505     c                   eval      i12nrs = barNRS
390800170505     c                   eval      i12nsp = comNSP
390900170505     c                   eval      i12fel = 'E'
391000040811     c*
391100040811     c                   call      'FIDN12R'
391200040811     c                   parm                    fidn12ds
391300040811     c*
391400040811     c                   setoff                                       33
391500040811     c*
391600040811     c* se non ci sono errori
391700040811     c                   if        o12err <> ' '
391800040811     c                   seton                                        33
391900040811     c                   else
392000040811     c* se numero di CA trovate maggiore di zero
392100040811     c                   if        o12nca = 0
392200040811     c                   seton                                        33
392300040811     c                   else
392400040811     c                   z-add     1             jj                2 0
392500040811     c                   dow       jj <= o12nca and *in33 = *off
392600040811     C     skDch(jj)     IFGT      0
392700040811     C     BARNPG        IFNE      4
392800040811     C     BARNPG        ANDNE     8
392900040811     C*
393000040811     C* Valorizzo i campi DCT* utilizzati nella routine CRE151 con quelli
393100040811     C* ritornati dal *pgm d utilità FIDN12R
393200040811     C                   movel     skCch(jj)     DCTCCH
393300040811     C*
393400040811     C                   EXSR      CRE151
393500040811     C                   ELSE
393600040811     C* SE E' CAT 4 E 8 LA CHIUDO
393700040811     C                   Z-ADD     151           COMCAA
393800040811     C                   EXSR      CHIUAN
393900040811     C                   ENDIF
394000040811     C**
394100040811     C                   SETON                                        33
394200040811     C**
394300040811     C                   ENDIF
394400040811     c*
394500040811     c                   add       1             jj
394600040811     c                   enddo
394700040811     c*
394800040811     c                   seton                                        33
394900040811     c*
395000040811     c                   endif
395100040811     c                   endif
395200040811     C**
395300990120    3C                   ENDIF
395400990120     C**
395500990120    2C                   ENDIF
395600990120    1C                   ENDIF
395700021023     C*
395800970203     C* CHIUDO LE ANOMALIE DI COLLO MANCATE
395900020204    1C     WAGARR        IFEQ      'S'
396000970203     C* PER COLLO NON LOCALE
396100021023     C     WAGPAR        OREQ      ' '
396200970203     C* PER BOLLA NON TROVATA IN PARTENZA
396300970203     C     WBOPAR        OREQ      'N'
396400021023     C**
396500021023     C* NON AGGIORNO CON SPUNTA ENTRATA
396600021023     C* SE NON E' LOCALE O DISGUIDO IN AREA AGGIORNO SEMPRE
396700021023    2C     WSPLOC        IFEQ      ' '
396800021023     C     WSPLOC        OREQ      'D'
396900021023     C* SE E' LOCALE SOLO SE E' SPUNTA FATTA CON CAT.ARRIVI  DAL P.O.
397000021023     C*  LINEA ARRIVO O TERMINAL ARRIVO
397100021023     C     WSPLOC        OREQ      'A'
397200021023     C     BARNPG        ANDNE     5
397300021023     C     BARNPG        ANDNE     1
397400021023     C*
397500021023     C     WSPLOC        OREQ      'A'
397600021023     C     BARNPG        ANDEQ     1
397700021023     C     WDEFL         ANDEQ     '1'
397800021023     C*
397900021023     C     WSPLOC        OREQ      'T'
398000021023     C     BARNPG        ANDNE     5
398100021023     C     BARNPG        ANDNE     1
398200021023     C*
398300021023     C     WSPLOC        OREQ      'T'
398400021023     C     BARNPG        ANDEQ     1
398500021023     C     WDEFL         ANDEQ     '1'
398600021023     C*
398700970203     C*
398800970203     C* COLLO ARRIVATO MANCANTE --> SOLO SE SPARATO SUL SISTEMA
398900041018     c* L'anomalia 15 non la chiudo se altra spunta presente in cat.arrivi
399000041018     c*  con stessa data
399100041018     c                   eval      wcontrdfv='S'
399200041018     c                   eval      wstess='S'
399300041018     c                   eval      wchiu='S'
399400970203     C                   Z-ADD     15            COMCAA
399500970203     C                   MOVEL     'TR'          SAVCCH
399600970203     C                   EXSR      CHIUAN
399700970203     C*
399800970203     C* COLLO PARTITO NON ARRIVATO
399900970203     C                   MOVEL     'AR'          SAVCCH
400000970203     C                   Z-ADD     5             COMCAA
400100970203     C                   EXSR      CHIUAN
400200970203     C* MANCA COLLO MANCA BOLLA
400300970203     C                   MOVEL     'AR'          SAVCCH
400400970203     C                   Z-ADD     12            COMCAA
400500970203     C                   EXSR      CHIUAN
400600990521    2C                   ENDIF
400700990521    1C                   ENDIF
400800040430     c
400900040430     c* Chiudo fuori posto bolla partenza e fuori posto bolla transito
401000040430     C                   Z-ADD     120           COMCAA
401100040430     C                   EXSR      CHIUAN
401200040430     C                   Z-ADD     125           COMCAA
401300040430     C                   EXSR      CHIUAN
401400970203     C                   ENDSR
401500970204     C**************************************************************************
401600970204     C* ANOMALIA FUORI POSTO SEGNACOLLO BOLLA IN PARTENZA
401700970204     C**************************************************************************
401800970204     C     ANM120        BEGSR
401900970204     C                   Z-ADD     120           COMCAA
402000970204     C*
402100970204     C* VALGONO SOLTANTO LE SPUNTE SPARATE NELL'AREA DI PARTENZA
402200020213    1C  N05BARFLE        IFEQ      BTFP
402300970204     C* SPUNTA DEL TERMINAL DI PARTENZA
402400020213    2C     BARFGS        IFEQ      BTFP
402500970204     C* SE COLLO NON SPUNTATO NELLE CATEGORIE PARTENZA
402600970204    3C     BARNPG        IFNE      5
402700970204     C     BARNPG        ANDNE     1
402800970204     C     BARSPG        ANDNE     'P'
402900970204     C                   EXSR      CRTANM
403000970204   X3C                   ELSE
403100970204     C* SE COLLO     SPUNTATO NELLE CATEGORIE PARTENZA
403200970204     C                   EXSR      CHIUAN
403300970204    3C                   ENDIF
403400970204     C*
403500970204   X2C                   ELSE
403600970204     C* COLLO SPUNTATO DA UN 2 LIVELLO IN PARTENZA
403700970204     C* SE COLLO NON SPUNTATO NELLE CATEGORIE PARTENZA
403800970204    3C     BARNPG        IFNE      5
403900970204     C     BARSPG        ANDNE     'P'
404000970204     C                   EXSR      CRTANM
404100970204     C                   ELSE
404200970204     C* SE COLLO     SPUNTATO NELLE CATEGORIE PARTENZA
404300970204     C                   EXSR      CHIUAN
404400970204    3C                   ENDIF
404500970204    2C                   ENDIF
404600970204    1C                   ENDIF
404700970204     C                   ENDSR
404800970204     C**************************************************************************
404900970204     C* ANOMALIE CHE CREO SOLO SE HA SPARATO LA LINEA DI ARRIVO DEL COL
405000970204     C**************************************************************************
405100970204     C     ANMARR        BEGSR
405200970204     C*
405300970204     C* COLLO/BOLLA ARRIVI PRESENTE
405400970204    1C  N05WBOAR         IFEQ      'S'
405500990126     C                   MOVEL     'N'           WCAT4             1
405600990126     C*     GESTISCO IL RITROVAMENTO COLLO
405700990126     C                   MOVEL     'S'           WRITRO
405800020204     C* NON GESTISCO LA CREAZIONE ANOMALIE CHE GIA' FACCIO IN CHIUAN
405900990126     C                   MOVEL     'N'           WCREAN
406000990126     C                   EXSR      CTRDAN
406100970204     C*
406200970204    2C     WATDCM        IFEQ      0
406300170328     c* in caso di cons anomala "S" il collo potrebbe non avere la data di cons
406400170328     c     wabcca        andne     'S'
406500170328     c
406600970204     C     BARDFV        ORGT      WATDCM
406700170329     c     watdcm        andgt     0
406800970204     C*
406900970204    3C     WABNDC        IFNE      0
407000970204     C     WATDCM        ANDEQ     0
407100170328     c*
407200970204     C* BOLLA IN CONSEGNA
407300970204    4C     BARNPG        IFNE      4
407400000523    4C     BARNPG        ANDNE     8
407500970204     C* SE NO SPUNTA ARRIVI CREO ANOMALIA: BOLLA IN CONSEGNA FUORI POST
407600970204    5C     BARNPG        IFNE      2
407700970204     C                   Z-ADD     135           COMCAA
407800970204     C                   EXSR      CRTANM
407900970204    5C                   END
408000970204     C*
408100000607    4C                   ENDIF
408200000607     C**
408300000607    4C     BARNPG        IFEQ      4
408400970204     C*
408500970204     C*        ANOMALIA: "IN CONSEGNA SU ALTRA DISTINTA"
408600970204    5C     WABNDC        IFNE      BARNFV
408700970204     C                   Z-ADD     150           COMCAA
408800970204     C                   EXSR      CRTANM
408900000523     C*
409000970204   X5C                   ELSE
409100970204     C*
409200970204     C* CHIUDO ANOMALIA: "IN CONSEGNA SU ALTRA DISTINTA"
409300970204     C                   Z-ADD     150           COMCAA
409400970204     C                   MOVEL     '  '          SAVCCH            2
409500970204     C                   EXSR      CHIUAN
409600970204    5C                   ENDIF
409700970204    4C                   ENDIF
409800970204    3C                   ENDIF
409900970204     C*
410000970204     C* BOLLA ARRIVI FUORI POSTO
410100161201     c* In caso di wgiac = 'D' non devo dare il fuori posto
410200161201    3C     WGIAC         IFEQ      ' '
410300970204     C*
410400161201    4C     BARSPG        IFEQ      'G'
410500161201    4C     BARSPG        oreq      'Z'
410600970204     C                   Z-ADD     130           COMCAA
410700970204     C                   EXSR      CRTANM
410800970204    4C                   ENDIF
410900161021     c
411000970204     C* IN ENTRATA SOLO SE NON E' BOLLA LOCALE
411100970204    4C     BARNPG        IFEQ      5
411200970204     C     WTIBOL        ANDNE     'L'
411300970204     C     WABNDC        ANDEQ     0
411400970204     C                   Z-ADD     130           COMCAA
411500970204     C                   EXSR      CRTANM
411600970204    4C                   ENDIF
411700970204    3C                   ENDIF
411800970204     C*
411900970204     C* BOLLA GIACENTE FUORI POSTO
412000161201     c* per ora il fuori posto lo devo dare anche per le
412100161201     c*  giacenze a partare a mag giac. mentre con i 2gg in IMA
412200161201     c*  dovrò dare l'anomalia soltanto se già a IMG se spuntato in IMA
412300161201     c*  per le altre categorie errore sempre
412400970204    3C     BARSPG        IFNE      'G'
412500161021    3C     BARSPG        andne     'Z'
412600161201     C**** WGIAC         ANDEQ     'S'
412700161201     C     WGIAC         ANDne     ' '
412800970205     C     BARDFV        ANDGT     GCPDGC
412900170125     c* se WGIAC = "D" e filiale arrivo partita con procedura NEWIMA
413000170125     c*  non creao l'anomalia
413100170125    4c                   if        wgiac='D'  and allnewima<>'S'
413200170125     c                   movel     barlna        w003a
413300170125     c                   if         %lookup(w003a:skFilOk)>0
413400170125     c                   eval      allnewima='Y'
413500170125     c                   else
413600170125     c                   clear                   allnewima
413700170125     c                   endif
413800170125    4c                   endif
413900170125     c
414000170125    4c                   if        wgiac='S' or (barspg<>'A' and
414100170125     c                             barnpg<>2) or
414200170125     c                             (wgiac='D' and allnewima=' ')
414300970204     C                   Z-ADD     140           COMCAA
414400970204     C                   EXSR      CRTANM
414500170125    4c                   endif
414600170125     c
414700970204    3C                   ENDIF
414800970204     C*
414900970204    2C                   ENDIF
415000970204     C**
415100970204     C* C H I U D O
415200970204     C* BOLLA CONSEGNA
415300970204    2C     BARNPG        IFEQ      4
415400970204     C                   Z-ADD     135           COMCAA
415500970204     C                   EXSR      CHIUAN
415600970204    2C                   END
415700970204     C* BOLLA ARRIVI
415800970204    2C     BARNPG        IFEQ      2
415900970204     C     BARNPG        OREQ      4
416000000523     C     BARNPG        OREQ      8
416100970204     C     BARSPG        OREQ      'A'
416200970204     C                   Z-ADD     130           COMCAA
416300970204     C                   EXSR      CHIUAN
416400970204    2C                   END
416500970204     C* BOLLA GIACENZE
416600970204    2C     BARNPG        IFEQ      3
416700970204     C     BARSPG        ANDEQ     'G'
416800161028    2C     BARNPG        orEQ      3
416900161028     C     BARSPG        ANDEQ     'Z'
417000970204     C                   Z-ADD     140           COMCAA
417100970204     C                   EXSR      CHIUAN
417200970204    2C                   END
417300970204     C*
417400970204    1C                   ENDIF
417500970204     C                   ENDSR
417600970218     C**************************************************************************
417700970218     C* SCRITTURA DI FNAGB PER AGGIORNAMENTO  BOLLE
417800970218     C**************************************************************************
417900970217     C     WRTAGB        BEGSR
418000970217     C* LA PRIMA VOLTA NON SCRIVO
418100041105    1C     WAGGF         IFEQ      'S'
418200970217     C     KSAV          CHAIN     FNAGB01L                           34
418300970217     C   34              CLEAR                   FNAGB000
418400970218     C                   MOVEL     SAVTBO        AGBTBO
418500970217     C                   MOVEL     KAGB          AGBAGB
418600970217     C                   MOVEL     SAVAAS        AGBAAS
418700970217     C                   MOVEL     SAVLNP        AGBLNP
418800970217     C                   MOVEL     SAVNRS        AGBNRS
418900970217     C                   MOVEL     SAVNSP        AGBNSP
419000970217     C     WFBS          IFEQ      'S'
419100970217     C                   MOVEL     WFBS          AGBFBS
419200970217     C                   ENDIF
419300970217     C     WFVC          IFEQ      'S'
419400970217     C                   MOVEL     WFVC          AGBFVC
419500970217     C                   ENDIF
419600990728     C     WFPC          IFEQ      'S'
419700990728     C                   MOVEL     WFPC          AGBFPC
419800990728     C                   ENDIF
419900970217     C*
420000981124     C   34              WRITE     FNAGB000                             99
420100970217     C  N34              UPDATE    FNAGB000
420200041105     c
420300041105     c* Se previsto scrivo anche per l'altra bolla
420400041105     c                   eval      wprimotbo=savtbo
420500041105     c
420600041105     c                   if        wagbart='S'
420700041105     c                   eval      savtbo='A'
420800041105     c                   EXSR      ALTROAGB
420900041105     c                   endif
421000041105     c                   eval      savtbo=wprimotbo
421100160421     c
421200160421     c* Scrivo record per la zona
421300160421     c                   if        wznc<>*blanks
421400160421     C                   CLEAR                   FNAGB000
421500160421     C                   MOVEL     SAVTBO        AGBTBO
421600160421     C                   MOVEL     wznc          AGBAGB
421700160421     C                   MOVEL     SAVAAS        AGBAAS
421800160421     C                   MOVEL     SAVLNP        AGBLNP
421900160421     C                   MOVEL     SAVNRS        AGBNRS
422000160421     C                   MOVEL     SAVNSP        AGBNSP
422100160421     C                   WRITE     FNAGB000                             99
422200160421     c                   endif
422300041105     c
422400970217    1C                   ENDIF
422500970218     C*
422600970218     C                   CLEAR                   WFBS
422700970218     C                   CLEAR                   WFVC
422800990728     C                   CLEAR                   WFPC
422900160421     C                   CLEAR                   Wznc
423000041105     C                   CLEAR                   Wagbart
423100970217     C                   ENDSR
423200041105     C**************************************************************************
423300041105     c* scrivo fnagb per l'altro tipo bolla
423400041105     C**************************************************************************
423500041105     c     ALTROAGB      begsr
423600041105     C     KSAV          CHAIN     FNAGB01L                           34
423700041105     c                   eval      agbtbo=savtbo
423800041105     c                   if        *in34
423900041105     c                   clear                   agbfbs
424000041105     C                   WRITE     FNAGB000                             99
424100041105     c                   else
424200041105     c* Se record trovato aggiorno solo i campi del peso/volume
424300041105     C     WFVC          IFEQ      'S'
424400041105     C                   MOVEL     WFVC          AGBFVC
424500041105     C                   ENDIF
424600041105     C     WFPC          IFEQ      'S'
424700041105     C                   MOVEL     WFPC          AGBFPC
424800041105     C                   ENDIF
424900041105     c
425000041105     C                   UPDATE    FNAGB000
425100041105     c                   endif
425200041105     c                   endSR
425300980120     C**************************************************************************
425400980120     C* SCRITTURA DI FNAGB PER AGGIORNAMENTO  BOLLE
425500980120     C**************************************************************************
425600980120     C     WR1AGB        BEGSR
425700110103      *
425800030528     c                   setoff                                       45
425900110103      *
426000980120     C     KEAGB         CHAIN     FNAGB01L                           34
426100980120     C   34              CLEAR                   FNAGB000
426200980120     C                   MOVEL     WTBO          AGBTBO
426300980120     C                   MOVEL     KAGB          AGBAGB
426400980120     C                   MOVEL     WDAAS         AGBAAS
426500980120     C                   MOVEL     WDLNP         AGBLNP
426600980120     C                   MOVEL     WDNRS         AGBNRS
426700980120     C                   MOVEL     WDNSP         AGBNSP
426800160726     c*
426900160726     c* Se richiesto, passo flag di rielaborazione statistiche partenze
427000160726     c                   if        welasta_p='S'
427100160726     c                   clear                   dagbflo
427200160726     c                   eval      §agbdsf='S'
427300160726     c                   eval      agbflo=dagbflo
427400160726     c                   endif
427500980120     C*
427600030528     C   34              WRITE     FNAGB000                             45
427700980120     C  N34              UPDATE    FNAGB000
427800980120     C*
427900160726     c                   clear                   welasta_p
428000160726     C*
428100980120     C                   ENDSR
428200970609     C**************************************************************************
428300970609     C* SCRITTURA DI FNSTA PER RIELABORAZIONE STATISTICA ARRIVI
428400970609     C**************************************************************************
428500970609     C     WRTSTA        BEGSR
428600970609     C* LA PRIMA VOLTA NON SCRIVO
428700970609    1C     WARDAM        IFGT      0
428800970609     C* VECCHIA DATA
428900970609     C     WSVDAM        IFGT      0
429000970609     C                   CLEAR                   SA2TLA
429100070221     C     dls45FLG      IFEQ      'C'
429200970609     C                   MOVEL     'L'           SA2TLA
429300970609     C                   END
429400970609     C                   MOVEL     'A'           SA2TRC
429500970609     C                   Z-ADD     WSVDAM        SA2DTS
429600970609     C                   MOVE      *ZEROS        SA2LNA
429700970609     C                   CALL      'FNLSA2R3'
429800970609     C                   PARM                    SA2TLA            1
429900970609     C                   PARM                    SA2TRC            1
430000970609     C                   PARM                    SA2DTS            8 0
430100970609     C                   PARM                    SA2LNA            3 0
430200970609     C                   END
430300970609     C* NUOVA DATA
430400970609     C                   Z-ADD     WARDAM        SA2DTS
430500970624     C                   MOVEL     'A'           SA2TRC
430600970609     C                   CALL      'FNLSA2R3'
430700970609     C                   PARM                    SA2TLA            1
430800970609     C                   PARM                    SA2TRC            1
430900970609     C                   PARM                    SA2DTS            8 0
431000970609     C                   PARM                    SA2LNA            3 0
431100970609    1C                   ENDIF
431200970609     C*
431300970609     C                   ENDSR
431400980928     C**************************************************************************
431500980928     C* MEMORIZZO I FOGLI NON TROVATI DA INVIARE A SEDE CON MSG
431600980928     C**************************************************************************
431700111018     C**   INVMSG        BEGSR
431800980928     C*
431900111018     C**                 Z-ADD     1             EE                3 0
432000111018     C**                 SETON                                        95
432100111018     C**                 CLEAR                   WPRES
432200111018     C**                 MOVE      WNFV          WNFVA             5
432300111018    4C**   *IN95         DOWEQ     *ON
432400111018     C**   WPRES         ANDEQ     ' '
432500980928     C* IL FOGLIO CORRISPONDE: NON LO RIMEMORIZZO
432600111018     C**   WNFVA         LOOKUP    ERF(EE)                                95
432700111018    5C** 95WNPG          IFEQ      ERC(EE)
432800111018     C**   WFGS          ANDEQ     ERE(EE)
432900111018     C**   WNPS          ANDEQ     ERP(EE)
433000111018     C**                 MOVEL     'S'           WPRES             1
433100111018    5C**                 ENDIF
433200111018     C**                 ADD       1             EE
433300111018    4C**                 ENDDO
433400980928     C* SE NON E' PRESENTE RIMEMORIZZO
433500111018    4C**   WPRES         IFEQ      ' '
433600111018     C**                 Z-ADD     1             EE
433700111018     C**   '     '       LOOKUP    ERF(EE)                                95
433800111018     C** 95              MOVE      WNFVA         ERF(EE)
433900111018     C** 95              Z-ADD     WNPG          ERC(EE)
434000111018     C** 95              Z-ADD     WFGS          ERE(EE)
434100111018     C** 95              movel     Wnps          ERP(EE)
434200111018    5C**   EE            IFEQ      8
434300111018     C**                 EXSR      TRASMI
434400111018    5C**                 ENDIF
434500111018    4C**                 ENDIF
434600111018     C*+                 ENDSR
434700990120     C**************************************************************************
434800990120     C* SE DEVO CREO ANOMALIA 152 / 153
434900990120     C**************************************************************************
435000990120     C     CRT15X        BEGSR
435100990120     C** SE LA C.A. E' APERTA, VERIFICO SE AVARIA E RESO O DISTRUTTA
435200990120    1C     DCTDSD        IFEQ      'R'
435300990120     C     DCTDSD        OREQ      'D'
435400990126     C**
435500990126     C                   EXSR      CERTAD
435600990120     C* CONTROLLO SE TIPO ANOMALIA E' AVARIA
435700990120     C** CREO ANOMALIA 152 0 153
435800990120    2C     §TARGR        IFEQ      'V'
435900990120     C*
436000990120     C     DCTDSD        IFEQ      'R'
436100990120     C                   Z-ADD     152           COMCAA
436200990120     C                   ELSE
436300990120     C                   Z-ADD     153           COMCAA
436400990120     C                   ENDIF
436500990120     C**
436600990120     C                   EXSR      CRTANM
436700990120    2C                   ENDIF
436800990120    1C                   ENDIF
436900990120     C                   ENDSR
437000990120     C**************************************************************************
437100990120     C* CONTROLLO SE PRESENTE CA PER IL COLLO
437200990120     C**************************************************************************
437300990120     C     CTRDAN        BEGSR
437400990126     C*
437500990120     C                   CLEAR                   WTRDAN
437600990120     C     KBAR          CHAIN     FNDCD02L                           33
437700990126     C*
437800990120    1C     *IN33         IFEQ      *OFF
437900990120     C     DCDATB        ANDEQ     ' '
438000990120     C     KDCT          CHAIN     FNDCT01L                           33
438100050223      * se dctdt2 > 0 la c.a. non è valida x la filiale
438200050224     c                   If        Not *In33 and dctdt2 > 0
438300050224     c                   Eval      *In33 = *On
438400050224     c                   EndIf
438500050224     C
438600990120    2C     *IN33         IFEQ      *OFF
438700990120     C     DCTATB        ANDEQ     ' '
438800990126     C* TROVATA C.A. NON ANNULLATA
438900990120     C                   MOVEL     'S'           WTRDAN            1
439000990126     C**
439100990126     C* CONTROLLO SE DEVO IMPOSTARE FLAG COLLO RITROVATO
439200990126    3C     WRITRO        IFEQ      'S'
439300990126     C                   EXSR      CERTAD
439400990126    4C     §TARIT        IFEQ      'S'
439500990126     C     DCDDFV        ANDEQ     0
439600990126     C                   Z-ADD     BARDFV        DCDDFV
439700990126     C                   Z-ADD     BARFGS        DCDFGS
439800990126     C                   Z-ADD     BARNPS        DCDNPS
439900990126     C                   MOVEL     'S'           DCDFAR
440000990126     C                   UPDATE    FNDCD000
440100990126    4C                   ENDIF
440200990126    3C                   ENDIF
440300990126     C**
440400990126     C                   UNLOCK    FNDCD02L
440500990126     C**
440600990126     C** CONTROLLO SE DEVO CREARE LE ANOMALIE
440700990126    3C     WCREAN        IFEQ      'S'
440800990126     C**
440900990126     C* ANOMALIA 151 - COLLO DI CA CHIUSA
441000990126    4C     DCTDCH        IFGT      0
441100990121     C* IN ARRIVO NON LA CREO PER SPUNTA DI CATEGORIA 4
441200990126    5C     WCAT4         IFEQ      'N'
441300990121     C     BARNPG        ANDNE     4
441400000522     C     BARNPG        ANDNE     8
441500990121     C     WCAT4         OREQ      'S'
441600990120     C                   EXSR      CRE151
441700990126   X5C                   ELSE
441800990121     C                   Z-ADD     151           COMCAA
441900990121     C                   EXSR      CHIUAN
442000990126    5C                   ENDIF
442100990126   X4C                   ELSE
442200990120     C* ANOMALIE 152 O 153
442300990126    5C     DCDDCH        IFEQ      0
442400990120     C                   EXSR      CRT15X
442500990126    5C                   ENDIF
442600990126    4C                   ENDIF
442700990121    3C                   ENDIF
442800990120    2C                   ENDIF
442900990120     C**
443000990120   X1C                   ELSE
443100990120     C* ANNULLATO COME SE NON CI FOSSE
443200990120     C                   SETON                                        33
443300990120    1C                   ENDIF
443400990120     C                   ENDSR
443500990120     C**************************************************************************
443600990120     C* CREO ANOMALIA 151 SE LO PREVEDE LA CAUSALE CHIUSURA
443700990120     C**************************************************************************
443800990120     C     CRE151        BEGSR
443900990120     C     DCTCCH        IFNE      *BLANKS
444000990120     C                   CLEAR                   DSBS02
444100990120     C                   MOVEL     'C'           T02MOD
444200990120     C                   MOVEL     KNSIF         T02SIF
444300990121     C                   MOVEL     'CCH'         T02COD
444400990120     C                   MOVEL     DCTCCH        T02KE1
444500990120     C                   CALL      'TIBS02R'
444600990120     C                   PARM                    KPJBA
444700990120     C                   PARM                    DSBS02
444800990121    2C     T02ERR        IFEQ      ' '
444900990121     C                   MOVEL     T02UNI        DCCH
445000990121     C                   ELSE
445100990121     C                   MOVEL     'S'           §CHCRA
445200990121    2C                   ENDIF
445300990120     C                   ENDIF
445400990120     C**
445500990120     C* CREO
445600990120     C     DCTCCH        IFEQ      *BLANKS
445700990120     C     §CHCRA        OREQ      'S'
445800990120     C                   Z-ADD     151           COMCAA
445900990120     C                   EXSR      CRTANM
446000990120     C                   ENDIF
446100990120     C                   ENDSR
446200990126     C**************************************************************************
446300990126     C* CERCO TABELLA RECORD TIPO ANOMALIA TAD
446400990126     C**************************************************************************
446500990126     C     CERTAD        BEGSR
446600990126    1C     DCTTAD        IFNE      SAVTAD
446700990126     C                   CLEAR                   DSBS02
446800990126     C                   MOVEL     'C'           T02MOD
446900990126     C                   MOVEL     KNSIF         T02SIF
447000990126     C                   MOVEL     'TAD'         T02COD
447100990126     C                   MOVEL     DCTTAD        T02KE1
447200990126     C                   CALL      'TIBS02R'
447300990126     C                   PARM                    KPJBA
447400990126     C                   PARM                    DSBS02
447500990126     C**
447600990126    2C     T02ERR        IFEQ      ' '
447700990126     C                   MOVEL     T02UNI        DTAD
447800990126     C                   ELSE
447900990126     C                   MOVEL     'V'           §TARGR
448000990126     C                   MOVEL     'S'           §TARIT
448100990126    2C                   ENDIF
448200990126     C                   MOVEL     DCTTAD        SAVTAD
448300990126    1C                   ENDIF
448400990126     C                   ENDSR
448500030723     C**************************************************************************
448600030723     C* CERCO TABELLA VARIABILI PGM PARTENZE
448700030723     C**************************************************************************
448800030723     C     CERVPO        BEGSR
448900030723      *
449000030723      * CHAIN A VARIABILI PARTENZE PER PRENDERE MAX PESO E VOLUME CARICABILI
449100030723     C                   CLEAR                   DVPO
449200030723     C                   CLEAR                   DSBS02
449300030723     C                   MOVEL     'C'           T02MOD
449400030723     C                   MOVEL     KNSIF         T02SIF
449500030723     C                   MOVEL     'VPO'         T02COD
449600030723     C                   MOVEL     'VPO'         T02KE1
449700030723     C                   CALL      'TIBS02R'
449800030723     C                   PARM                    KPJBA
449900030723     C                   PARM                    DSBS02
450000030723     C**
450100030723    2C     T02ERR        IFEQ      ' '
450200030725     C                   MOVEL     T02UNI        DVPO
450300030723    2C                   ENDIF
450400110103      *
450500030723     C                   ENDSR
450600000927     C**************************************************************************
450700000927     C* CONTROLLO SE POSSO CREARE ANOMALIE DI MANCA RECOD SECONDO LA
450800000927     C*  ATA FOGLIO E I GIONRI DI PULIZIA
450900000927     C**************************************************************************
451000000927     C     CTRDAT        BEGSR
451100000927     C*
451200000927     C* A CAMBIO DATA RICONTROLLO
451300000927    1C     BARDFV        IFNE      SA2DFV
451400000927     C                   CLEAR                   WCRE10
451500000927     C*
451600000927     C* SE FOGLIO UGUALE A UDATE, VA SICURAMENTE BENE
451700000927    2C     BARDFV        IFEQ      WDATE1
451800000927     C                   MOVEL     'S'           WCRE10            1
451900000927   X2C                   ELSE
452000000927     C* CONTROLLO CON I GG DI PULIZIA
452100000927      * DATA FOGLIO MAGGIORE DATA ULTIMO UTILIZZO: POSSO CREARE ANOM
452200000927    3C     BARDFV        IFGE      PRIDAT
452300000927     C                   MOVEL     'S'           WCRE10            1
452400000927    3C                   ENDIF
452500000927    2C                   ENDIF
452600000927     C*
452700000927     C                   Z-ADD     BARDFV        SA2DFV
452800000927    1C                   ENDIF
452900130301     c*
453000130301     c* Se è da creare --> controllo se si tratta di fil che fa partire colli
453100130301     c*  di altro TFP e non la creo lo stesso
453200130301     c                   clear                   wlocalxco         1
453300130301     c
453400130301     c                   if        wcre10='S' and ( bolxco='W'  or
453500130301     c                             (wesbtt='N' and wesblt='N' and wesart='N'
453600130301     c                              and barnrs>0))
453700130301     c* se si tratta di spunta partenza --> creo la 57
453800130301     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
453900130301     c                             or barnpg=1
454000130301     c                   exsr      CtrXCO
454100130301     c                   if        Indx>0
454200130301     c                   eval      Wlocalxco='S'
454300130301     c                   endif
454400130301     c                   endif
454500130301     c                   endif
454600000927     C*
454700010607     C                   ENDSR
454800010607     C**************************************************************************
454900070131     C* MEMORIZZAZIONE SPUNTE DI CONSEGNA DOPPIE SU FNBRVE0F
455000010607     C**************************************************************************
455100010607     C     SRBRVE        BEGSR
455200070710     c* Se devo tenere una spuynta solo memeorizzo per categoria
455300070710     c*  e segnacollo
455400070710     c                   if        nst(WA)='1'
455500070710     c
455600070131     C     KBRVE2        SETLL     FNBRVE2L                               33
455700010607     C     *IN33         IFEQ      *OFF
455800010607     C* SPUNTA NON ESISTENTE: MEMORIZZO ANCHE LA VECCHIA
455900070131     C                   WRITE     FNBRVE
456000010607     C                   ENDIF
456100070131     C* Salvo campi di FNBRV che potrebbero rimanere invariati
456200070131     C* Li dovrò reimpostare dopo la scrittura di FNBRVe per poi
456300010608     C* fare i giusti aggiornamenti
456400010608     C                   MOVE      BRVATR        SAWATR
456500080317     C                   MOVE      BRVTSU        SAWTSU
456600080317     c                   movel     brv           sawbrv
456700010608     C* IN OGNI CASO MEMORIZZO LA SPUNTA NUOVA
456800010608     C                   MOVEL     'S'           WBRVE             1
456900010608     C                   EXSR      RIEMPI
457000010608     C                   CLEAR                   WBRVE
457100070131     C                   WRITE     FNBRVE
457200070131     C* Reimposto campi di FNBRV come da lettura iniziale
457300010608     C                   MOVE      SAWATR        BRVATR
457400080317     C                   MOVE      SAWtsu        BRVtsu
457500080317     C                   MOVE      SAWbrv        BRV
457600110103     c*
457700070710     c                   else
457800070710     c* Se devo memorizzare le spunte senza anomalia controllo per foglio
457900070710     C     KBRVE1        SETLL     FNBRVE1L                               33
458000070710     C     *IN33         IFEQ      *OFF
458100070710     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
458200070710     C                   WRITE     FNBRVE
458300070710     C                   ENDIF
458400070710     C                   ENDIF
458500070710     c*
458600010607     C                   ENDSR
458700020124     C**************************************************************************
458800020204     C* CERCO TERMINAL PARTENZA LNP COLLO
458900020124     C**************************************************************************
459000020204     C     TERPAR        BEGSR
459100020124     C                   CLEAR                   DSLV55
459200020124     C                   MOVEL     'P'           D55TPT
459300020124     C                   MOVEL     LNPB          D55LIN
459400020124     C                   MOVEL     BARDFV        D55DRF
459500020213     C                   EXSR      FNLV55
459600020204     C                   MOVE      D55TFP        LNPTFP
459700020204     C**
459800020204     C                   ENDSR
459900020204     C**************************************************************************
460000020204     C* CERCO TERMINAL ARRIVO LNA COLLO
460100020204     C**************************************************************************
460200020204     C     TERARR        BEGSR
460300020124     C**
460400020124     C* LINEA DI ARRIVO
460500020124     C     BARLNA        IFNE      SVVLNA
460600020124     C     LNPB          ORNE      SVVLNP
460700020204     C     BARDFV        ORNE      SVVDFV
460800020124     C*
460900020124     C* TEMINAL DI ARRIVO
461000020124     C                   CLEAR                   DSLV55
461100020124     C                   MOVEL     ' '           D55TPT
461200020124     C                   MOVEL     BARDFV        D55DRF
461300020124     C                   MOVEL     BARLNA        D55LIN
461400020124     C                   MOVEL     LNPB          D55LNP
461500020213     C                   EXSR      FNLV55
461600020124     C**
461700020213     C                   MOVEL     D55TFA        LNATFA
461800020124     C*
461900020124     C                   MOVEL     BARLNA        SVVLNA
462000020204     C                   MOVEL     LNPB          SVVLNP
462100020204     C                   MOVEL     BARDFV        SVVDFV
462200020124     C                   ENDIF
462300020124     C                   ENDSR
462400020124     C**************************************************************************
462500020124     C* AGGIORNO LE BOLLE PARTENZA
462600020124     C**************************************************************************
462700020124     C     AGGBLT        BEGSR
462800060223     C                   CLEAR                   WEND              1
462900020124     C* NON FACCIO AGGIORNAMENTI SE SPUNTA DI COLLO DISGUIDATO
463000070221    1C     dls45FLG      IFNE      '6'
463100020124     C*
463200020124     C                   MOVEL     'P'           WTIBOL            1
463300020124     C                   MOVEL     'S'           WBOPAR
463400020124     C*
463500020124     C* Se collo già consegnato e spunta partenza di pistola manuale
463600020124     C* ignoro la spunta
463700020124    3C     BARNPG        IFEQ      1
463800020124     C     BLTDCM        ANDNE     *ZEROS
463900020124     C     RPS(Y)        ANDEQ     'M'
464000020124     C                   MOVE      'N'           OKAGG
464100020201     C                   MOVEL     '1'           WEND
464200020204     C                   UNLOCK    FNBLT27L
464300020201     C                   GOTO      ENDBLT
464400020124    3C                   END
464500020204     C*  SE NON IMPOSTATO VOLUME E PESO DA PARTENZA
464600020204     C     WWFVC         IFEQ      ' '
464700020124     C                   MOVEL     'P'           WWFVC
464800020204     C                   ENDIF
464900020124     C*
465000020124     C                   MOVEL     'P'           BLTTBO
465100020124     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
465200020124     C* NE PRECEDENTE
465300020124     C     FLGPRE        IFNE      'A'
465400020124     C     BLTSPE        ANDNE     SAVSPE
465500020124     C     WAGGF         IFEQ      'S'
465600020124     C                   EXSR      WRTAGB
465700020124     C                   CLEAR                   WAGGF
465800020124     C                   ENDIF
465900020124     C                   MOVEL     BLTSPE        SAVSPE
466000020124     C                   ENDIF
466100020124     C*
466200020124    3C     BARCAN        IFNE      ' '
466300020124     C* NON MEMORIZZO ANOMALIA "E" SU DETTAGLIO COLLO PERCHE' LA NON
466400020124     C* INDICA UNA VERA E PROPRIA ANOMALIA MA SOLO UN ERRORE PER LINEA
466500020124     C* MANCANTE SUL FOGLIO AL MOMENTO DELLA SPUNTA DEL COLLO
466600020124     C     BARCAN        ANDNE     'E'
466700020124     C                   MOVEL     BARCAN        BLTCAN
466800020124     C                   MOVEL     'S'           WAGGF             1
466900020124    3C                   END
467000160421     c
467100160421     c* Se zona bolla <> zona spunta --> riaggiorno zona bolla
467200160422     c                   if        bolznc<>'  ' and
467300160429     c                             bolznc<>%editc(barznc:'X')  and
467400160503     c                             barnpg<>8
467500160503     c                   if        tps(Y)= 'L' or tps(y)='C'
467600160421     c                   movel     barznc        wznc
467700160421     C                   MOVEL     'S'           WAGGF             1
467800160421     c                   endif
467900160503     c                   endif
468000020124     C*
468100041105     c                   clear                   wagvolart         1
468200041105     c                   clear                   wagpesart         1
468300041105     c
468400020124    3C     BARVUC        IFGT      0
468500020124     C* AGGIORNO IL VOLUME CML SOLO SE IL VOLUME SUL COLLO:
468600020124     C* - E' = 0
468700020124     C* - E' DELL'ARRIVO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
468800020124     C**- E' DEL TRANSITO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
468900020124    4C     BLTVUC        IFEQ      0
469000020124     C     BLTVUC        ORNE      0
469100020214     C     BLTFVC        ANDNE     'P'
469200020124     C     WWFVC         ANDEQ     'P'
469300041013     C                   MOVEL     'S'           WFVC
469400020124     C                   Z-ADD     BARVUC        BLTVUC
469500041105     c                   eval      wagvolart='S'
469600020124     C                   MOVEL     'S'           WAGGF             1
469700020124     C                   MOVEL     WWFVC         BLTFVC
469800020124    4C                   END
469900020124     C*
470000020124    3C                   END
470100020124     C*STESSO GIRO DEL VOLUME ANCHE PER IL PESO
470200020124    3C     BARPUC        IFGT      0
470300020124     C* AGGIORNO IL VOLUME CML SOLO SE IL VOLUME SUL COLLO:
470400020124     C* - E' = 0
470500020124     C* - E' DELL'ARRIVO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
470600020124     C**- E' DEL TRANSITO E QUELLO DELLA SPUNTA E' DELLA PARTENZA
470700020124    4C     BLTPUC        IFEQ      0
470800020124     C     BLTPUC        ORNE      0
470900020214     C     BLTFPC        ANDNE     'P'
471000020124     C     WWFVC         ANDEQ     'P'
471100041012     c                   movel     'S'           wfpc
471200020124     C                   Z-ADD     BARPUC        BLTPUC
471300041105     c                   eval      wagpesart='S'
471400020124     C                   MOVEL     'S'           WAGGF             1
471500020124     C                   MOVEL     WWFVC         BLTFPC
471600020124    4C                   END
471700020124     C*
471800020124    3C                   END
471900020124     C*
472000020124     C* SPUNTA LOCALE IN DISGUIDO --> AGGIORNO LA DATA ENTRATA TRANSITO
472100020124    3C     WSPLOC        IFEQ      'D'
472200020124     C                   MOVEL     BARDFV        BLTDET
472300020124     C                   MOVEL     BARFGS        BLTFLP
472400020124     C                   CLEAR                   BLTDUT
472500020124     C                   MOVEL     'S'           WAGGF             1
472600020124     C*
472700020124   X3C                   ELSE
472800020124     C*
472900020124     C* SE NON C'E' AGGIUNGO DATA E PISTOLA
473000020124     C* SOLO SE NON E' SPUNTA DISGUIDO IN AREA
473100020124    4C     BLTDSE        IFEQ      0
473200020124     C                   MOVEL     BARDFV        BLTDSE
473300020124     C                   MOVEL     BARNPS        BLTNPE
473400020124     C                   MOVEL     'S'           WAGGF             1
473500020124   X4C                   ELSE
473600020124     C*
473700020124     C* SE E' QUESTA LA SPUNTA CHE HA AGGIORNATO IL COLLO RIAGGIORNO
473800020124    5C   05BLTNPE        IFEQ      BARNPS
473900020124     C                   MOVEL     BARDFV        BLTDSE
474000020124     C                   MOVEL     'S'           WAGGF             1
474100020124    5C                   ENDIF
474200020124     C*
474300020124     C* RIAGGIRNO COLLO SE PRECEDENTE SPUNTA NON VALIDA E ATTUALE VALID
474400020124    5C  N05BLTNPE        IFNE      BARNPS
474500020124     C     RPS(Y)        ANDEQ     'L'
474600020124     C                   MOVEL     BLTNPE        ALF2
474700020124     C*
474800020124     C* CERCO RAGGRUPPAMENTO PISTOLA
474900020124     C                   EXSR      RICRAG
475000020124     C*
475100020124    6C     RPS(Z)        IFEQ      'M'
475200020124     C     RPS(Z)        OREQ      'A'
475300020124     C                   MOVEL     BARDFV        BLTDSE
475400020124     C                   MOVEL     BARNPS        BLTNPE
475500020124     C                   MOVEL     'S'           WAGGF             1
475600020124    6C                   END
475700020124    5C                   END
475800020124     C*
475900020124    4C                   END
476000020124    3C                   END
476100020124     C*
476200041105     C*  Se FLG3 = A non devo aggiornare fnblt perchè richiamo solo per
476300041105     c*  gli arrivi
476400070208    3C     dls45flg2     IFNE      'A'
476500020124     C                   UPDATE    FNBLT000
476600041105     c* Se aggiornato peso e volume e collo partito, aggiorno anche ART
476700041105     c                   if        bltdfv>0 and
476800160421     c                             (wagvolart='S' or wagpesaRT='S')
476900041105     C     KBAR          CHAIN     FNART27L                           3434
477000041105     C                   IF        NOT *IN34
477100041105     c* VOLUME
477200041105     C                   if        wagvolart='S'
477300041105     c* Se lan fedex non aggiorno
477400041105     c                   if        artvuc=0 or ntwbar<>'FED'
477500041105     c                   eval      wagbart='S'
477600041105     c                   eval      artvuc=bltvuc
477700041105     c                   eval      artfvc=bltfvc
477800041105     c                   endif
477900041105     c                   endif
478000041105     C                   if        wagpesart='S'
478100041105     c                   eval      wagbart='S'
478200041105     c                   eval      artpuc=bltpuc
478300041105     c                   eval      artfpc=bltfpc
478400041105     c                   endif
478500160421     c
478600041105     c                   update    fnart000
478700041105     C                   ENDIF
478800041105     C                   ENDIF
478900041105     C
479000020124     C**
479100020124     C* SE RICHIAMATO AGGIORNO SUBITO LA BOLLA
479200110103    5c   02              if        wfpc='S' or wfvc='S'
479300160421     c                             or wznc<>*blanks
479400020124     C                   SETOFF                                       3489
479500020124     C     KBLT          CHAIN     FNBLP01L                           3489
479600041021     c                   if        not *in34 and not *in89
479700041012     c                   if        wfvc='S'
479800041012     C                   MOVEL     99999         BLPNCR
479900041012     c                   endif
480000041012     c                   if        wfpc='S'
480100041012     C                   MOVEL     99999         BLPNCp
480200041012     c                   endif
480300160421     c                   if        wznc<>*blanks
480400160421     c                   movel     wznc          blpznc
480500160421     c                   endif
480600041012     c
480700041012     C                   UPDATE    FNBLP000
480800050223     c
480900050223     c* Aggiorno subito anche la bolla arrivo
481000050224     c                   if        wagbart='S'
481100050223     C     KBLT          CHAIN     FNarb01L                           3489
481200050223     C                   MOVEL     'E'           arbfbs
481300160421     c                   if        wznc<>*blanks
481400160421     c                   movel     wznc          arbznc
481500160421     c                   endif
481600050224     c  n89
481700050224     cann34              update    fnarb000
481800050224     c                   clear                   wagbart
481900050224     c                   endif
482000041012     c
482100041012     c                   clear                   wfpc
482200041012     c                   clear                   wfvc
482300160421     c                   clear                   wznc
482400041012     c                   endif
482500020124    5C                   ENDIF
482600020124     C**
482700020124   X3C                   ELSE
482800020124     C                   UNLOCK    FNBLT27L
482900020124    3C                   ENDIF
483000020124     C*
483100020213     C*CHIUDO EVENTUALI ANOMALIE DI DISGUIDO,SE COLLO NON DEL TFP
483200020124     C     BARNRS        IFGT      *ZEROS
483300020214     C*
483400020213     C                   CLEAR                   DSLV55
483500020213     C                   MOVEL     'P'           D55TPT
483600020213     C                   Z-ADD     BARLNP        D55LIN
483700020214     C                   Z-ADD     DSPB          D55DRF
483800020213     C                   EXSR      FNLV55
483900020131     C**
484000020213     C     D55TFP        IFNE      LNPTFP
484100020124     C                   MOVEL     'S'           SAVMAC
484200020124     C                   Z-ADD     200           COMCAA
484300020124     C                   EXSR      CHIUAN
484400020124     C*
484500020124     C                   Z-ADD     55            COMCAA
484600020124     C                   EXSR      CHIUAN
484700020124     C*
484800020124     C                   Z-ADD     56            COMCAA
484900020124     C                   EXSR      CHIUAN
485000020131     C                   ENDIF
485100020124     C*
485200020124     C                   CLEAR                   SAVMAC
485300020124     C                   END
485400020124     C*
485500020124   X1C                   ELSE
485600020124     C                   UNLOCK    FNBLT27L
485700020124    1C                   ENDIF
485800020124     C**
485900020201     C     ENDBLT        ENDSR
486000020131     C**************************************************************************
486100020131     C* AGGIORNO LE BOLLE ARRIVO
486200020131     C**************************************************************************
486300020131     C     AGGART        BEGSR
486400020201     C                   CLEAR                   WEND
486500020131     C*
486600020131     C     WTIBOL        IFEQ      'P'
486700020131     C                   MOVEL     'L'           WTIBOL
486800020131     C                   ELSE
486900020131     C                   MOVEL     'A'           WTIBOL
487000020131     C                   ENDIF
487100020131     C*
487200020131     C* E S I S T E   B O L L A   A R R I V O
487300020131    2C     WESART        IFEQ      'S'
487400020131     C* SE FLG=6 SVINCOLO RECORD
487500070221     C     dls45FLG      IFEQ      '6'
487600020131     C                   UNLOCK    FNART27L
487700020131     C                   ENDIF
487800020131     C**
487900020131     C     BARNPS        IFEQ      89
488000020131     C                   UNLOCK    FNART27L
488100020131     C                   ELSE
488200020214     C     ARTFLP        IFEQ      0
488300020214     C                   Z-ADD     ARTDFV        COMDFV
488400020214     C                   ELSE
488500020214     C                   Z-ADD     ARTDUT        COMDFV
488600020214     C                   ENDIF
488700020131     C                   MOVE      ARTDCM        WATDCM
488800020131     C                   MOVE      ARTDAM        SAVDAM
488900020131     C                   MOVEL     'S'           WBOAR             1
489000020131     C*
489100020131     C* SE FLG=6 CREO ANOMALIA 61
489200160624     c* Anomalia 61 eliminata
489300160624     C***  dls45FLG      IFEQ      '6'
489400160624     C***                EXSR      CRTA60
489500160624     C***                MOVEL     '1'           WEND
489600160624     C***                GOTO      ENDART
489700160624     C***                ENDIF
489800020131     C*
489900020131     C     WWFVC         IFEQ      *BLANKS
490000020131     C                   MOVEL     'A'           WWFVC
490100020131     C                   END
490200020131     C*
490300020131     C* LA PRIMA VOLTA MEMORIZZO LA SPEDIZIONE
490400020131     C                   MOVEL     'A'           ARTTBO
490500020131     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
490600020131     C* NE PRECEDENTE
490700020131     C     FLGPRE        IFNE      'A'
490800020131     C     ARTSPE        ANDNE     SAVSPE
490900020131     C     WAGGF         IFEQ      'S'
491000020131     C                   EXSR      WRTAGB
491100020131     C                   CLEAR                   WAGGF
491200020131     C                   ENDIF
491300020131     C                   MOVEL     ARTSPE        SAVSPE
491400020131     C                   ENDIF
491500160422     c* zona bolla
491600160422     c                   if        bolznc<>'  '   and
491700160429     c                             bolznc<>%editc(barznc:'X') and
491800160503     c                             barnpg<>8
491900160503     c                   if        tps(Y)= 'L' or tps(y)='C'
492000160422     c                   movel     barznc        wznc
492100160422     C                   MOVEL     'S'           WAGGF             1
492200160422     c                   endif
492300160503     c                   endif
492400020131     C*
492500020131     C* CODICE ANOMALIA
492600041012     c                   exsr      AGGCAN
492700020131     C*
492800020131     C* VOLUME DA MACCHINA SMISTACOLLI
492900041012     C                   EXSR      AGGVOL
493000030604     C*
493100020131     C* PESO DA MACCHINA SMISTACOLLI
493200041012     C                   EXSR      AGGpes
493300110103     c*
493400110103     c*
493500091209     c* Se chi spara è la linea di arrivo del collo o il suo gestore
493600091209     c* --> aggiorno la data più bassa di arrivo 1° collo
493700091211     C* CARICO LA L6 DELLA FILIALE GESTIONE
493800091211     C     BPES          IFNE      SAVPES
493900091211     C                   MOVEL     BPES          SAVPES
494000091211     C                   EXSR      CARL6
494100091211     C                   ENDIF
494200110103      *
494300091211     C     BARLNA        LOOKUP    L6                                     88
494400150615     c*
494500150817     c* non aggiorno più data e ora arrivo alla filiale finale di arrivo
494600150817     c*  er l'estero perchè creeremo apposito campo per
494700150817     c*  indicare la data di arrivo all'hub/terminal di arrivo
494800150622     c***                if        not *in88 and  boltfa=BPES  and
494900150622     c***                          (ntwbar='EEX' or ntwbar='DPD' or
495000150622     c***                          ntwbar='FED')
495100150622     c***                seton                                        88
495200150622     c***                endif
495300110103      *
495400150817      * tolto test su data spedizione dal 2009/12 in poi
495500150817    3c                   if        *in88 or artlna=bpes
495600091211     c     kar52         chain     fiar501l
495700091209    4c                   if        %found(fiar501l)
495800091209     c                   movel     ar5uni        dar5gen
495900091209     c                   else
496000091209     c                   clear                   dar5gen
496100091209    4c                   endif
496200110103     c*
496300091209     c                   clear                   warrdt
496400091209     c                   clear                   warrhm
496500091209    4c                   if        §ar5arrdt>*zeros
496600091209     c                   movel     §ar5arrdt     warrdt            8 0
496700091209    5c                   if        §ar5arrhm>*zeros
496800091209     c                   movel     §ar5arrhm     warrhm            4 0
496900091209    5c                   endif
497000091209    4c                   endif
497100110103     c*
497200101129     c* Uso data e ora immissione se non superiore all'ora caricamento
497300150817     c                   if        (bardfs<brvdcs) or
497400150817     c                             (bardfs=brvdcs and barhms<brvhcs)
497500101130     c                   movel     bardfs        datReale
497600101130     c                   movel     barhms        oraReale
497700101129     c                   else
497800101129     c                   movel     brvdcs        datReale
497900101129     c                   movel     brvhcs        oraReale
498000101129     c                   endif
498100110103     c*
498200091211     c                   if         warrdt=0  or
498300101129    4c                             (warrdt>0 and warrdt>datReale) or
498400101129     c                             (warrdt=datReale and warrhm>oraReale)
498500091209     c                   exsr      AggioFIAR5
498600110103     c*
498700091209   x4c                   else
498800091209     c                   unlock    fiar501l
498900091209    4c                   endif
499000091209    3c                   endif
499001180115      **
499002180115      * Se chi spunta è il terminal di arrivo --> verifico se spunta incompatibi
499100091209      **
499101180115     c                   if        bpes=bpltfa and (brvspg='J'  or barcan='J')
499102180115     c     kbar
499103180115     c                   ENDIF
499104180115      *
499200020131    3C     ARTDAM        IFEQ      0
499300020131     C                   MOVEL     BARDFV        ARTDAM
499400020131     C                   MOVEL     'S'           WAGGF             1
499500020131     C                   MOVEL     BARNPS        ARTNAP
499600050509     c* Imposto flag ARTFL2= 'P' se catagoria di spunta è tipica processo
499700050509     c*  operativo partenza
499800050509     c                   if        barnpg=5 or (barnpg=1 and wdefl=' ') or
499900050509     c                             (barnpg=3  and barspg='P')
500000050509     c                   eval      artfl2='P'
500100050509     c                   else
500200050509     c                   eval      artfl2=' '
500300050509     c                   endif
500400020131     C*
500500020131     C                   MOVEL     ' '           ARTDAB
500600020131     C                   SETON                                        03
500700020131     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
500800160624     c* ANOMALIE 60 E 61 ELIMINATE
500900160624     C***                EXSR      CRTA60
501000020131     C*
501100020131   X3C                   ELSE
501200020131    4C   05ARTNAP        IFEQ      BARNPS
501300020131     C                   MOVEL     BARDFV        ARTDAM
501400020131     C                   MOVEL     'S'           WAGGF             1
501500020131    4C                   ENDIF
501600020131     C*
501700020131     C* 'D' DI DISABILITAZIONE COLLO :
501800020131     C*   SE RISPUNTATO LANCIO PGM DI ABILITAZIONE X RIABILITARE BOLLA
501900020131    4C  N05ARTDAB        IFEQ      'S'
502000020131     C                   SETON                                        03
502100020131     C                   MOVEL     ' '           ARTDAB
502200040324     C                   MOVEL     'S'           WAGGF             1
502300020131    4C                   ENDIF
502400020131     C*
502500020131    4C  N05ARTNAP        IFNE      BARNPS
502600020131     C     RPS(Y)        ANDEQ     'L'
502700020131     C                   MOVEL     ARTNAP        ALF2              2
502800020131     C*
502900020131     C* CERCO RAGGRUPPAMENTO PISTOLA
503000020131     C                   EXSR      RICRAG
503100020131     C*
503200020131    5C     RPS(Z)        IFEQ      'M'
503300020131     C     RPS(Z)        OREQ      'A'
503400020131     C                   MOVEL     'S'           WAGGF             1
503500020131     C                   MOVEL     BARDFV        ARTDAM
503600020213     C                   MOVEL     BARNPS        ARTNAP
503700050509     c* Imposto flag ARTFL2= 'P' se catagoria di spunta è tipica processo
503800050509     c*  operativo partenza
503900050509     c                   if        barnpg=5 or (barnpg=1 and wdefl=' ') or
504000050509     c                             (barnpg=3  and barspg='P')
504100050509     c                   eval      artfl2='P'
504200050509     c                   else
504300050509     c                   eval      artfl2=' '
504400050509     c                   endif
504500020131     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
504600160624     c* anomalie 60 e 61 eliminate
504700160624     C***                EXSR      CRTA60
504800020131     C*
504900020131    5C                   ENDIF
505000020131    4C                   ENDIF
505100020131     C*
505200020131    3C                   END
505300020131     C*
505400020131     C                   Z-ADD     ARTDCM        WDCM
505500020131     C**
505600070208    3C     dls45flg2     IFNE      'A'
505700020131     C                   UPDATE    FNART000
505800020131     C                   ELSE
505900020131     C                   UNLOCK    FNART27L
506000020131    3C                   ENDIF
506100020131     C*
506200050223     C     KARB          CHAIN(N)  FNARB01L                           32
506300020131     C*
506400020131    3C     *IN32         IFEQ      *OFF
506500020131     C*
506600070208    4C     dls45flg2     IFNE      'A'
506700020131     C* SOLO SE PGM RICHIAMATO:
506800020131     C* SE VARIATA DATA DI ARRIVO SULLA BOLLA RIELABORO LA STATISTICA
506900020131    5C     *IN02         IFEQ      *ON
507000020131     C     ARTDAM        ANDNE     SAVDAM
507100020131    6C     ARTDAM        IFNE      WARDAM
507200020131     C     SAVDAM        ORNE      WSVDAM
507300020131     C                   EXSR      WRTSTA
507400020131     C                   MOVE      ARTDAM        WARDAM
507500020131     C                   MOVE      SAVDAM        WSVDAM
507600020131    6C                   END
507700020131    5C                   END
507800020131     C*
507900020131     C                   MOVEL     ARBNDC        WABNDC
508000040430     C                   MOVEL     ARBcca        WABcca
508100020131     C   03              MOVEL     'S'           WFBS
508200020131     C*
508300020131    4C                   ENDIF
508400020131     C**
508500020131     C** VEDO SE BOLLA GIACENTE
508600020131    4C     ARBFBC        IFEQ      'G'
508700050331     C     KGCA          CHAIN     Tigcp21L                           34
508800161007     c  n34              z-add     gcpmgc        gcpdgc
508900161007     c  n34              movel     gcpagc        gcpdgc
509000161007    5C  N34GCPFAS        IFLT      35
509100161007    5C     GCPFAS        oreq      36
509200161007     c* Verifico tramite driver se il collo dove essere in IMG
509300161007     c                   clear                   fnlgi1ds
509400161007     c                   eval      ilgi1aas=arbaas
509500161007     c                   eval      ilgi1lnp=arblnp
509600161007     c                   eval      ilgi1nrs=arbnrs
509700161007     c                   eval      ilgi1nsp=arbnsp
509800161007     c                   select
509900161007     c                   when      arbncp=arbncl
510000161007     c                   eval      ilgi1pkf=arbpkc
510100161007     c                   when      arbpkc>arbpkf
510200161007     c                   eval      ilgi1pkf=arbpkc
510300161007     c                   other
510400161007     c                   eval      ilgi1pkf=arbpkf
510500161007     c                   endsl
510600161007     c                   movel     fnlgi1ds      kpjbu
510700161007     c                   call      'FNLGI1R'
510800161007     c                   parm                    kpjba
510900161007     c                   parm                    tigcp00ds
511000161007     c                   movel     kpjbu         fnlgi1ds
511100161021     c* In questo caso la sped potrebbe essere sia in IMA che in IMG
511200170125     c*  quindi non devo dare alcun fuori posto con la nuova procedura
511300170125     c*  anche se ha Y (ovvero portato nel giorno in IMG:
511400170125     c*  potrebbe mancare un collo della sped spuntato succesivamente)
511500170125   5ac****               if        olgi1AMG='S'
511600170125     c
511700170125   5ac                   if        olgi1AMG='S' or olgi1AMG='Y'
511800161021     C                   MOVEL     'D'           WGIAC
511900161021  x5ac                   else
512000170125   5bc****               if        olgi1AMG='Y'  or olgi1inmg='S'
512100170125   5bc                   if        olgi1inmg='S'
512200161201     C                   MOVEL     'S'           WGIAC
512300161201   5bc                   endif
512400161201   5ac                   endif
512500161201     c
512600161201   5ac                   if        wgiac<>' '
512700020131     C* NON LA CONSIDERO GIACENTE SE IN QUESTO MOMENTO C'E' UNA BOLLA
512800020131     C*  FIGLIA IN CONSEGNA O TUTTE LE FIGLIE SONO BLOCCATE
512900020131     C                   MOVEL     'S'           WFIBLO            1
513000020131     C                   CLEAR                   WESFI
513100020131     C     KARB          SETLL     FNLBL02L
513200020131     C     KARB          READE     FNLBL02L                               29
513300020131     C**
513400020131    6C     *IN29         DOWEQ     *OFF
513500020131     C*
513600050223     C     KLBL          CHAIN(N)  FNARB01L                           28
513700020131    7C     *IN28         IFEQ      *OFF
513800020131     C                   MOVEL     'S'           WESFI             1
513900020131     C* FIGLIA IN CONSEGNA
514000020131     C* NON LA CONSDIERO SE E' CONSEGNATA
514100020131   7AC     ARBDCM        IFEQ      0
514200020131    8C     ARBNDC        IFNE      0
514300020131     C                   SETON                                        29
514400020131     C                   CLEAR                   WGIAC
514500020131   X8C                   ELSE
514600020131     C*
514700020131     C* SE NON E' NEMMENO BLOCCATA MEMORIZZO
514800020131    9C     ARBFBC        IFNE      'B'
514900020131     C                   MOVEL     'N'           WFIBLO
515000020131    9C                   ENDIF
515100020131    8C                   ENDIF
515200020131   7AC                   ENDIF
515300170125    7C                   ENDIF
515400020131     C*
515500020131     C  N29KARB          READE     FNLBL02L                               29
515600020131    6C                   ENDDO
515700020131     C*
515800020131     C* SE TUTTE LE FIGLIE SONO BLOCCATE --> ESCLUDO
515900161201    6C     WGIAC         IFNE      ' '
516000020131     C     WESFI         ANDEQ     'S'
516100020131     C     WFIBLO        ANDEQ     'S'
516200020131     C                   CLEAR                   WGIAC
516300020131    6C                   ENDIF
516400020131     C*
516500161021   5aC                   ENDIF
516600161007    5C                   ENDIF
516700020131    4C                   ENDIF
516800020131    3C                   ENDIF
516900020131     C*
517000020131     C** COLLO EXPORT: VEDO SE CREARE L'ANOMALIA COLLO INVIATO PARTNER
517100020131     C*  MA RISPARATO IN IMA O IN PARTENZA/ARRIVO
517200020131     C*  NON CREO ANOMALIA SE E' DATA =99999999 PERCHE' SI TRATTA DI
517300020131     C*  DIROTTAMENTO E QUINDI VALGONO I COLLI DELLA FIGLIA E NON
517400020131     C*  DELLA MAMMA
517500020212    3C     NTWBAR        IFEQ      'EEX'
517600060322    3C     NTWBAR        oreq      'DPD'
517700060322    3C     NTWBAR        oreq      'FED'
517800060322    4C     BARDFV        ifGT      ARTDTR
517900020131     C     ARTDTR        ANDGT     0
518000020131     C     ARTDTR        ANDNE     *HIVAL
518100020131     C** CREO ANOMALIA 600 O 620
518200020131     C                   MOVEL     'S'           W60X              1
518300060322    4C                   ENDIF
518400060322    3C                   ENDIF
518500020131    2C                   ENDIF
518600020131     C*
518700020131   X2C                   ELSE
518800020131     C*
518900070221   2AC     dls45FLG      IFNE      '6'
519000020131     C** CREO LE ANOMALIE DI MANCA BOLLA SOLO SE NON SONO PASSATI I
519100020131     C*  GG DIPULIZIA BOLLE ARRIVI (PER EVITARE MANCA RECORD ERRATI)
519200020131     C                   EXSR      CTRDAT
519300020131     C**
519400020131   2BC     WCRE10        IFEQ      'S'
519500130301     c     wlocalxco     andne     'S'
519600020131     C**
519700020131     C* ANOMALIA: "MANCA BOLLA" --> ANCHE PER SPEDIZIONE LOCALE SE
519800020131     C*  E' SPARATA NELLE CATEGORIE ARRIVI
519900020131     C*  IL FOGLIO DI DEFLUENZA E' COME UNA CATEGORIA ARRIVI
520000020131    3C  N05BARNPG        IFNE      5
520100020131     C     BARSPG        ANDNE     'P'
520200020131     C     BARSPG        ANDNE     'I'
520300020131     C     WTIBOL        ANDEQ     'L'
520400020131     C     WTIBOL        OREQ      'A'
520500020131     C* LA CATEGORIA 1 LA CONSIDERO SOLTANTO SE E' DEFLUENZA
520600020131     C*
520700020131    4C     BARNPG        IFNE      1
520800020131     C     WTIBOL        OREQ      'A'
520900020131     C     WDEFL         OREQ      '1'
521000020131     C                   Z-ADD     10            COMCAA
521100020131     C                   EXSR      CRTANM
521200020131     C* Se l'anomalia 10 esiste già a livello max. creo anom. 410 per
521300020131     C* segnalare il persistere del manca record
521400020131     C     WANM10        IFEQ      '1'
521500020131     C                   Z-ADD     410           COMCAA
521600020131     C                   EXSR      CRTANM
521700020131     C                   END
521800020131    4C                   ENDIF
521900020131    3C                   ENDIF
522000020131     C*
522100020131     C* SE LNA ESTERA VERIFICO SE C'E' EVENTO 2 PER CREARE ANOM.600/620
522200020212    3C     NTWBAR        IFEQ      'EEX'
522300060322    3C     NTWBAR        oreq      'DPD'
522400060322    3C     NTWBAR        oreq      'FED'
522500020131     C*
522600020131     C* SE LNA ESTERA APRO FILE FNEVS02L
522700020131    4C     WAPER         IFEQ      ' '
522800020131     C                   OPEN      FNEVS02L
522900020131     C                   MOVEL     '1'           WAPER             1
523000020131    4C                   ENDIF
523100020131     C*
523200020131     C                   MOVEL     'E2 '         KCEV
523300020131     C     KEVS          SETGT     FNEVS02L
523400020131     C     KEVS          READPE    FNEVS02L                               34
523500020131    4C     *IN34         IFEQ      *OFF
523600020131     C     EVSDEV        ANDLT     BARDFV
523700020131     C* CREO ANOMALIA 600 E 620
523800020131     C                   MOVEL     'S'           W60X
523900020131    4C                   ENDIF
524000020131    3C                   ENDIF
524100020131   2BC                   ENDIF
524200020131   2AC                   ENDIF
524300020131    2C                   ENDIF
524400020131     C**
524500020201     C     ENDART        ENDSR
524600020213     C**************************************************************************
524700020213     C* AGGIORNO LE BOLLE TRANSITO
524800020213     C**************************************************************************
524900020213     C     AGGBTT        BEGSR
525000020213     C                   MOVEL     'T'           WTIBOL
525100020214     C* MEMORIZZO CHE C'E' BOLLA TRANSITO, MI SERVE PER CRTA50
525200020214     C                   MOVE      BTTDFV        COMDFV
525300020213     C* LA PRIMA VOLTA MEMORIZZO LA SPEDIZIONE
525400020213     C                   MOVEL     'T'           BTTTBO
525500020213     C* A CAMBIO SPEDIZIONE VEDO SE DEVO SCRIVERE AGB PER LA SPEDIZIO-
525600020213     C* NE PRECEDENTE
525700020213     C     BTTSPE        IFNE      SAVSPE
525800020213     C     WAGGF         IFEQ      'S'
525900020213     C                   EXSR      WRTAGB
526000020213     C                   CLEAR                   WAGGF
526100020213     C                   ENDIF
526200020213     C                   MOVEL     BTTSPE        SAVSPE
526300020213     C                   ENDIF
526400020213     C*
526500020213    5C     BARCAN        IFNE      ' '
526600020213     C     BARCAN        ANDNE     'E'
526700020213     C                   MOVEL     BARCAN        BTTCAN
526800020213     C                   MOVEL     'S'           WAGGF             1
526900020213    5C                   END
527000020213     C*
527100020213    5C     BARVUC        IFGT      0
527200020213     C*
527300020213    6C     BTTVUC        IFEQ      0
527400041013     c                   movel     'S'           WFVC
527500020213     C                   Z-ADD     BARVUC        BTTVUC
527600020213     C                   MOVEL     'S'           WAGGF             1
527700020213     C                   MOVEL     'T'           BTTFVC
527800020213    6C                   END
527900020213     C*
528000020213    5C                   END
528100020213     C*PESO DA CML
528200020213    5C     BARPUC        IFGT      0
528300020213     C*
528400020213    6C     BTTPUC        IFEQ      0
528500041013     c                   movel     'S'           WFPC
528600020213     C                   Z-ADD     BARPUC        BTTPUC
528700020213     C                   MOVEL     'S'           WAGGF             1
528800020213     C                   MOVEL     'T'           BTTFPC
528900020213    6C                   END
529000020213     C*
529100020213    5C                   END
529200020213     C*
529300020213    5C     BTTDET        IFEQ      0
529400020213     C                   MOVEL     'S'           WAGGF             1
529500020213     C                   MOVEL     BARDFV        BTTDET
529600020213     C                   MOVEL     BARNPS        BTTPET
529700020213     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
529800020213     C*  SOLO SE NON E' PISTOLA FASULLA
529900160624     C**                 MOVEL     'TT'          WCCH
530000160624     C**   OPS(Y)        IFLT      040
530100160624     C**                 EXSR      CRTA60
530200160624     C**                 CLEAR                   WCCH
530300160624     C**                 ENDIF
530400020213     C*
530500020213   X5C                   ELSE
530600020213     C*
530700020213    6C   05BTTPET        IFEQ      BARNPS
530800020213     C                   MOVEL     'S'           WAGGF             1
530900020213     C                   MOVEL     BARDFV        BTTDET
531000020213    6C                   END
531100020213     C*
531200020213    6C  N05BTTPET        IFNE      BARNPS
531300020213     C     RPS(Y)        ANDEQ     'L'
531400020213     C                   MOVEL     BTTPET        ALF2
531500020213     C*
531600020213     C* CERCO RAGGRUPPAMENTO PISTOLA
531700020213     C                   EXSR      RICRAG
531800020213     C*
531900020213    7C     RPS(Z)        IFEQ      'M'
532000020213     C     RPS(Z)        OREQ      'A'
532100020213     C                   MOVEL     BARDFV        BTTDET
532200020213     C                   MOVEL     'S'           WAGGF             1
532300020213     C                   MOVEL     BARNPS        BTTPET
532400160624     c
532500020213     C* CREO ANOMALIA COLLO ARRIVATO DA INVIARE IN PARTENZA
532600160624     c* Anomalia 61 eliminara
532700160624     C**                 MOVEL     'TT'          WCCH
532800160624     C**                 EXSR      CRTA60
532900160624     C**                 CLEAR                   WCCH
533000020213    7C                   END
533100020213    6C                   END
533200020213     C*
533300020213    5C                   END
533400020213     C*
533500020213     C                   UPDATE    FNBTT000
533600020213     C*
533700020213     C* SE INVENTARIO O IN CONSEGNA ANOMALIA: FUORI POSTO DEL TRANSITO
533800020213    5C  N05BARNPG        IFEQ      3
533900020213     C     BARSPG        ANDNE     'P'
534000020213     C     BARNPG        OREQ      4
534100020213     C     BARNPG        OREQ      8
534200020213     C                   Z-ADD     125           COMCAA
534300020213     C                   EXSR      CRTANM
534400020213    5C                   END
534500040430     c* Se il collo è in transito, posso chiudere i fuori posto in partenza
534600040430     C                   Z-ADD     120           COMCAA
534700040430     C                   EXSR      chiuan
534800110103     c*
534900040422     c*Chiudo eventuale anomalia di disguido presente sul collo, se
535000040422     c*  generate da un 2 livello dell'area di partenza
535100040503     C* SOLO SE SPUNTA DI PISTOLA REALE
535200040503     c                   if        fsl(y)<>'F'
535300040428     c                   eval      comcaa=200
535400040422     C*
535500040503     C     Kanm          chain(N)  FNANM000
535600040428     c                   if        %found(fnanm02l)
535700040428     C                   CLEAR                   DSLV55
535800040428     C                   MOVEL     'P'           D55TPT
535900040428     C                   Z-ADD     anmfle        D55LIN
536000040428     C                   Z-ADD     bardfv        D55DRF
536100040428     C                   EXSR      FNLV55
536200040428     c                   if        bpes=d55tfp
536300040428     c                   exsr      chiuan
536400040428     c                   endif
536500040428     c                   endif
536600040503     c                   endif
536700041012     c*
536800041012     c* Aggiorno anche la bolla arrivo con i dati del transito
536900041012     c                   EXSR      AGGARRIVO
537000110103     c*
537100020213     C                   ENDSR
537200020201     C**************************************************************************
537300020201     C* CONTROLLO COSA POSSO AGGIORNARE IN BASE A CHI SONO
537400020201     C**************************************************************************
537500020201     C     CTRAGG        BEGSR
537600020204     C                   CLEAR                   WAGPAR
537700020204     C                   CLEAR                   WAGARR
537800150622     C                   CLEAR                   Wbsptp
537900150622     C                   CLEAR                   Wbspspu
538000020213     C**
538100020213     C** CERCO TERMINAL DELLA LINEA DI ARRIVO
538200020213     C                   EXSR      TERARR
538300050603     c                   eval      Usalnatfa=lnatfa
538400020204     C*
538500070208     c*         Non considero se c'e'bolla transito
538600070208     c     wesbtt        ifne      'S'
538700150622     C* SE NON C'È BOLLA TRANSITO VERIFICO SE COLLI CHE PARTONO DA
538800150817     C*  più filiali se ho trovato la bolla in partenza non partita
538900150817     c*  e con data di entrata = 0
539000170224     c                   if        wesblt='S' and blpdse=0
539100170224     c                   if        Blpdt1=0  or blpft1='N' and blpft2='N'
539200150818     c* se si tratta di una spunta tipica partenza
539300150818     c                   if        barnpg=5 or (barnpg=3 and barspg='P')
539400150818     c                             or barnpg=1
539500150622     c                   exsr      sr_fibsp
539600150622     c                   if        wbsptp<>' '      and
539700150622     c                             %lookup(bpes:sbsp)>0
539800150622     c                   eval      wbspspu='S'
539900150818     C                   MOVEL     'P'           WTIBOL            1
540000150622     c                   endif
540100170224     c                   endif
540200170224
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
572000170505     C     Kbolla        KLIST
572100170505     C                   KFLD                    comaas
572200170505     C                   KFLD                    comlnp
572300170505     C                   KFLD                    BARNRS
572400170505     C                   KFLD                    comnsp
572500070208     C     KBAR37        KLIST
572600070208     C                   KFLD                    bpes
572700070208     C                   KFLD                    BARLNP
572800070208     C                   KFLD                    BARLNA
572900070208     C                   KFLD                    BARNRS
573000070208     C                   KFLD                    BARNSC
573100160721     C     KBAR_t        KLIST
573200160721     C                   KFLD                    savflp
573300160721     C                   KFLD                    BARLNP
573400160721     C                   KFLD                    BARLNA
573500160721     C                   KFLD                    BARNRS
573600160721     C                   KFLD                    BARNSC
573700070208     C     KBAR1         KLIST
573800070208     C                   KFLD                    BARNPG
573900070208     C                   KFLD                    BARNfv
574000070208     C                   KFLD                    BARfgs
574100070208     C                   KFLD                    BARLNP
574200070208     C                   KFLD                    BARLNA
574300070208     C                   KFLD                    BARNRS
574400070208     C                   KFLD                    BARNSC
574500140616     C     KBAR_qd4      KLIST
574600140616     C                   KFLD                    kdel_npg
574700140616     C                   KFLD                    BARLNP
574800140616     C                   KFLD                    BARLNA
574900140616     C                   KFLD                    BARNRS
575000140616     C                   KFLD                    BARNSC
575100140616     C                   KFLD                    BARfgs
575200140616     C                   KFLD                    qd_dfs
575300140616     C                   KFLD                    qd_hms
575400070208     C     KBAR2         KLIST
575500070208     C                   KFLD                    BARNPG
575600070208     C                   KFLD                    BARLNP
575700070208     C                   KFLD                    BARLNA
575800070208     C                   KFLD                    BARNRS
575900070208     C                   KFLD                    BARNSC
576000070208     C                   KFLD                    BARfgs
576100140613     C     KBAR2_d       KLIST
576200140613     C                   KFLD                    kdel_npg
576300140613     C                   KFLD                    BARLNP
576400140613     C                   KFLD                    BARLNA
576500140613     C                   KFLD                    BARNRS
576600140613     C                   KFLD                    BARNSC
576700140613     C                   KFLD                    BARfgs
576800140616     C                   KFLD                    wdfs
576900070208     C     KBAR3         KLIST
577000070208     C                   KFLD                    WNPG
577100070208     C                   KFLD                    BARLNP
577200070208     C                   KFLD                    BARLNA
577300070208     C                   KFLD                    BARNRS
577400070208     C                   KFLD                    BARNSC
577500070208     C     KBRV7         KLIST
577600070208     C                   KFLD                    BARLNP
577700070208     C                   KFLD                    BARLNA
577800070208     C                   KFLD                    BARNRS
577900070208     C                   KFLD                    BARNSC
578000070208     C     KARB          KLIST
578100070208     C                   KFLD                    ARTAAS
578200070208     C                   KFLD                    ARTLNP
578300070208     C                   KFLD                    ARTNRS
578400070208     C                   KFLD                    ARTNSP
578500070208     C     KGCA          KLIST
578600070208     C                   KFLD                    ARTAAS
578700070208     C                   KFLD                    ARTLNP
578800070208     C                   KFLD                    ARTNRS
578900070208     C                   KFLD                    ARTNSP
579000070208     C                   KFLD                    KFRG
579100070208     C     KSAV          KLIST
579200070208     C                   KFLD                    SAVTBO
579300070208     C                   KFLD                    SAVAAS
579400070208     C                   KFLD                    SAVLNP
579500070208     C                   KFLD                    SAVNRS
579600070208     C                   KFLD                    SAVNSP
579700070208     C                   KFLD                    KAGB
579800070208     C     KEAGB         KLIST
579900070208     C                   KFLD                    WTBO
580000070208     C                   KFLD                    WDAAS
580100070208     C                   KFLD                    WDLNP
580200070208     C                   KFLD                    WDNRS
580300070208     C                   KFLD                    WDNSP
580400070208     C                   KFLD                    KAGB
580500070208     C     KBLT          KLIST
580600070208     C                   KFLD                    BLTAAS
580700070208     C                   KFLD                    BLTLNP
580800070208     C                   KFLD                    BLTNRS
580900070208     C                   KFLD                    BLTNSP
581000070208     C     KBTT          KLIST
581100070208     C                   KFLD                    BTTflp
581200070208     C                   KFLD                    BTTAAS
581300070208     C                   KFLD                    BTTLNP
581400070208     C                   KFLD                    BTTNRS
581500070208     C                   KFLD                    BTTNSP
581600070208     C     KANM          KLIST
581700070208     C                   KFLD                    BARLNP
581800070208     C                   KFLD                    BARLNA
581900070208     C                   KFLD                    BARNRS
582000070208     C                   KFLD                    BARNSC
582100070208     C                   KFLD                    COMCAA            3 0
582200070208     C     KEVS          KLIST
582300070208     C                   KFLD                    BARLNP
582400070208     C                   KFLD                    BARLNA
582500070208     C                   KFLD                    BARNRS
582600070208     C                   KFLD                    BARNSC
582700070208     C                   KFLD                    KCEV
582800070208     C     KFVV1         KLIST
582900070208     C                   KFLD                    WNPG
583000070208     C                   KFLD                    WNFV
583100070208     C                   KFLD                    WFGS
583200070208     C     KFVV          KLIST
583300070208     C                   KFLD                    WNPG
583400070208     C                   KFLD                    wdata
583500070208     C                   KFLD                    KTFA2
583600070208     C     KFVA1         KLIST
583700070208     C                   KFLD                    WFGS
583800070208     C                   KFLD                    WNFV
583900070208     C     KFGV1         KLIST
584000070208     C                   KFLD                    WNFV
584100070208     C                   KFLD                    tfpfgs
584200070208     c     KFGV2         KLIST
584300070208     C                   KFLD                    lnptfp
584400070208     C                   KFLD                    bardfv
584500070208     C     KFVA4         KLIST
584600070208     C                   KFLD                    KNPG4
584700070208     C                   KFLD                    KNFV4
584800070208     C                   KFLD                    KFGS4
584900070208     C     KFWA          KLIST
585000070208     C                   KFLD                    FVALNP
585100070208     C                   KFLD                    FVANFV
585200070208     C                   KFLD                    FVALAI
585300070208     C     KTAB          KLIST
585400070208     C                   KFLD                    CODUT
585500070208     C                   KFLD                    COD
585600070208     C                   KFLD                    KEY
585700070208     C     KTAB2         KLIST
585800070208     C                   KFLD                    CODUT
585900070208     C                   KFLD                    COD
586000130228     C     KTBE2         KLIST
586100130228     C                   KFLD                    KCOD
586200130228     C                   KFLD                    KKE1
586300130228     C                   KFLD                    KKE2
586400070208     C*
586500070208     C     KLBL          KLIST
586600070208     C                   KFLD                    LBLAAN
586700070208     C                   KFLD                    LBLLPN
586800070208     C                   KFLD                    LBLNRN
586900070208     C                   KFLD                    LBLNSN
587000070208     C     KDCT          KLIST
587100070208     C                   KFLD                    DCDAAC
587200070208     C                   KFLD                    DCDFIL
587300070208     C                   KFLD                    DCDNCA
587400070208     C     KBRVE2        KLIST
587500070208     C                   KFLD                    BRVNPG
587600070208     C                   KFLD                    BRVLNP
587700070208     C                   KFLD                    BRVLNA
587800070208     C                   KFLD                    BRVNRS
587900070208     C                   KFLD                    BRVNSC
588000070710     C     KBRVE1        KLIST
588100070710     C                   KFLD                    BRVNPG
588200070710     C                   KFLD                    BRVNfv
588300070710     C                   KFLD                    BRVfgs
588400070710     C                   KFLD                    BRVLNP
588500070710     C                   KFLD                    BRVLNA
588600070710     C                   KFLD                    BRVNRS
588700070710     C                   KFLD                    BRVNSC
588800070208     C     Kar5          KLIST
588900070208     C                   KFLD                    anmaas
589000070208     C                   KFLD                    anmLNp
589100070208     C                   KFLD                    anmNRS
589200070208     C                   KFLD                    anmnsp
589300070208     C                   KFLD                    Ktrd
589400091211     C     Kar52         KLIST
589500091211     C                   KFLD                    artaas
589600091211     C                   KFLD                    artLNp
589700091211     C                   KFLD                    artNRS
589800091211     C                   KFLD                    artnsp
589900091211     C                   KFLD                    Ktrd
589901180115     C     Kars          KLIST
589902180115     C                   KFLD                    artaas
589903180115     C                   KFLD                    artLNp
589904180115     C                   KFLD                    artNRS
589905180115     C                   KFLD                    artnsp
589906180115     C                   KFLD                    Karstr
590000070208     C*
590100070208     C     *LIKE         DEFINE    FVAdfv        Wpul050
590200070208     C     *LIKE         DEFINE    FVALNP        WFVALNP
590300070208     C     *LIKE         DEFINE    FVANFV        WFVANFV
590400070208     C     *LIKE         DEFINE    FVADFV        WFVADFV
590500070208     C     *LIKE         DEFINE    bardfv        Wdata
590600070208     C     *LIKE         DEFINE    wfgs          WWWfgs
590700070208     C     *LIKE         DEFINE    wfgs          tfpfgs
590800070208     C     *LIKE         DEFINE    wfgs          savfgs
590900070208     c                   clear                   savfgs
591000070208     C     *LIKE         DEFINE    FVVFLE        WFLE
591100070208     C     *LIKE         DEFINE    ARTFLP        SAVFLP
591200070208     C     *LIKE         DEFINE    BARDFV        DSPB
591300070208     C     *LIKE         DEFINE    BARDFV        COMDFV
591400070208     C     *LIKE         DEFINE    BARDFV        SVVDFV
591500070208     C     *LIKE         DEFINE    BLPTFP        LNPTFP
5916000702080    C     *LIKE         DEFINE    BARDFV        WDTRIF
591700070208     C     *LIKE         DEFINE    BARDFV        PRIDAT
591800070208     C     *LIKE         DEFINE    BARDFV        SA2DFV
591900070208     C     *LIKE         DEFINE    ANMCCH        WCCH
592000070208     C     *LIKE         DEFINE    GCPFRG        KFRG
592100070208     C                   MOVEL     '0'           KFRG
592200070208     C     *LIKE         DEFINE    AGBAGB        KAGB
592300070208     C     *LIKE         DEFINE    AGBFBS        WFBS
592400070208     C     *LIKE         DEFINE    AGBFVC        WFVC
592500070208     C     *LIKE         DEFINE    AGBFPC        WFPC
592600070208     C     *LIKE         DEFINE    ARBNDC        WABNDC
592700070208     C     *LIKE         DEFINE    ARTDCM        WATDCM
592800070208     C     *LIKE         DEFINE    ARbcca        WABcca
592900070208     C     *LIKE         DEFINE    ARbcca        savcca
593000070208     C     *LIKE         DEFINE    BARLNA        SVVLNA
593100070208     C     *LIKE         DEFINE    BARLNP        SVVLNP
593200070208     C     *LIKE         DEFINE    ARTDCM        WDCM
593300070208     C     *LIKE         DEFINE    ANMCAA        CAACRE
593400070208     C     *LIKE         DEFINE    ANMCAA        CAACIU
593500070208     C     *LIKE         DEFINE    FGVTTR        WTTR
593600140613     C     *LIKE         DEFINE    FGVFCF        WFCF
593700070208     C     *LIKE         DEFINE    EVSCEV        KCEV
593800070208     C     *LIKE         DEFINE    D33MAC        SAVMAC
593900070208     C     *LIKE         DEFINE    BRVFLE        BARFLE
594000070208     C     *LIKE         DEFINE    BRVFGS        SAVPES
594100070208     C     *LIKE         DEFINE    BRVFGS        KFGS4
594200070208     C     *LIKE         DEFINE    TBLCOD        COD
594300070208     C     *LIKE         DEFINE    TBLKEY        KEY
594400070208     C     *LIKE         DEFINE    BARLNA        LNATFA
594500070208     C     *LIKE         DEFINE    BARLNA        Usalnatfa
594600100713     C     *LIKE         DEFINE    BARLNA        WgenLNAtfa
594700070208     C     *LIKE         DEFINE    blptfa        bolTFA
594800070208     C     *LIKE         DEFINE    BARLNA        PESTFA
594900070208     C     *LIKE         DEFINE    BARLNA        PESTFP
595000070208     C     *LIKE         DEFINE    BARLNP        LNPB
595100070208     C     *LIKE         DEFINE    FVVFGS        WFGS
595200070208     C     *LIKE         DEFINE    FVVSPG        WSPG
595300070208     C     *LIKE         DEFINE    FVVNPG        WNPG
595400070208     C     *LIKE         DEFINE    FVVNPG        KNPG4
595500070208     C     *LIKE         DEFINE    FVVNFV        WNFV
595600070208     C     *LIKE         DEFINE    FVVNFV        KNFV4
595700070208     C     *LIKE         DEFINE    FVVFLE        WLIN
595800070208     C     *LIKE         DEFINE    FVVFLE        WLINPR
595900070208     C     *LIKE         DEFINE    FVVSPG        BARSPG
596000140616     C     *LIKE         DEFINE    FVVSPG        brvSPG
596100070208     C     *LIKE         DEFINE    FVVDFV        WDFV
596200070208     C     *LIKE         DEFINE    FVVDFV        SAVDFV
596300070208     C     *LIKE         DEFINE    FVVDFV        BARDFV
596400070208     C     *LIKE         DEFINE    FVVDFV        BRVDFV
596500070208     C     *LIKE         DEFINE    BARKEY        SAVKEY
596600070208     C     *LIKE         DEFINE    BRVKEY        SA1KEY
596700070208     C     *LIKE         DEFINE    ARBLNP        COMLNP
596800070208     C     *LIKE         DEFINE    ARBAAS        COMAAS
596900070208     C     *LIKE         DEFINE    ARBNSP        COMNSP
597000070208     C     *LIKE         DEFINE    ARBTFP        WPTFP
597100070208     C     *LIKE         DEFINE    ANMFL1        WFL1
597200070208     C     *LIKE         DEFINE    D55TFA        KTFA2
597300070208     C     *LIKE         DEFINE    D55TFA        SAVTFA
597400070208     C     *LIKE         DEFINE    ARTFVC        WWFVC
597500070208     C     *LIKE         DEFINE    ARTDAM        SAVDAM
597600070208     C     *LIKE         DEFINE    ARTDAM        WARDAM
597700070208     C     *LIKE         DEFINE    ARTDAM        WSVDAM
597800070208     C     *LIKE         DEFINE    FVADFV        SAVDAO
597900070208     C     *LIKE         DEFINE    FVALNP        WVALNP
598000070208     C     *LIKE         DEFINE    DCTTAD        SAVTAD
598100070208     C                   CLEAR                   SAVTAD
598200070208     C     *LIKE         DEFINE    §OGNTW        NTWBAR
598300070208     C     *LIKE         DEFINE    GI8INV        DATMIN
598400070208     C     *LIKE         DEFINE    BRVATR        SAWATR
598500080317     C     *LIKE         DEFINE    BRVTSU        SAWtsu
598600070208     C*
598700070208     C                   MOVEL     'DT'          KAGB
598800070208     c                   ENDSR
598900140613     C**************************************************************************
599000140613     C* Cerca spunta 4
599100140613     C**************************************************************************
599200140613     c     Cer_cons      BEGSR
599300140616     c* imposto data immiss.spunta della spunta che sto caricando
599400140616     c                   exsr      ImpoDFS
599500140616     c*
599600140616     C     KBAR2_d       setll     FNBRV05L
599700140616     C     KBAR2_d       reade(n)  FNBRV05L
599800140616    1c                   dow       not %eof(fnbrv05l)
599900140613     c* controllo data e ora immissione
600000140613     c                   clear                   brv_imm
600100140613     c                   clear                   brv_sca
600200140613     c                   z-add     brvhms        brv_imm
600300140613     c                   movel     brvdfs        brv_imm
600400140613     c                   z-add     brvhcs        brv_sca
600500140613     c                   movel     brvdcs        brv_sca
600600140613     c                   exsr      sceglidata
600700140613     c                   z-add     a_imm         b_imm
600800140613     c
600900140613     c* a parità di data devo tenere la più recente
601000140613     c                   clear                   brv_imm
601100140613     c                   clear                   brv_sca
601200140613     c                   z-add     barhms        brv_imm
601300140613     c                   movel     wDFS          brv_imm
601400140613     C                   Z-ADD     WDATE1        barDCS
601500140613     C                   time                    barHCS
601600140613     c                   z-add     barhcs        brv_sca
601700140613     c                   movel     bardcs        brv_sca
601800140613     c                   exsr      sceglidata
601900140613     c*
602000140613     c                   select
602100140616     c* quella trovata + recente --> non carico la spunta
602200140613    2c                   when      b_imm > A_IMM  and brvdfs=wdfs
602300140613     c
602400140613     c* Se anche la data foglio è = non carico la spunta ricevuta
602500140613    3C     BRvKEY        IFNE      SA1KEY
602600140613     C                   MOVEL     BRvKEY        SA1KEY
602700140613     C                   Z-ADD     BRvNPG        WNPG
602800140613     C                   Z-ADD     BRvNFV        WNFV
602900140613     C                   Z-ADD     BRvFGS        WFGS
603000140613     C                   movel     BRvnps        Wnps
603100140613     C                   EXSR      RILFOG
603200140613     c                   Z-ADD     WDFV          BRVDFV
603300140616     c                   movel     WFCF          BRVFCF
603400140616     c                   movel     Wspg          BRVspg
603500140613    3c                   endif
603600140613     c
603700140616     c                   if        barnpg=3 and
603800140616    3c                             brvdfv=bardfv
603900140616     c                   eval      amb_QD ='N'
604000140616     c                   endif
604100140616     c                   if        barnpg=4 and brvspg='A' and
604200140616    3c                             brvdfv=bardfv
604300140616     c                   eval      amb_QD ='N'
604400140616     c                   endif
604500140613     c
604600140616     c                   if        amb_qd='N'
604700140613     c* memorizzo la spunta nuova negli errori
604800140613     C                   MOVEL     'S'           WBRVE             1
604900140613     C                   EXSR      RIEMPI
605000140613     C                   CLEAR                   WBRVE
605100140613     C                   WRITE     FNBRVE
605200140616     c                   leave
605300140613    3c                   endif
605400140616     c
605500140616     c* quella di carico + recente --> cancello quella già presente
605600140613    2c                   when      A_imm > B_IMM  and brvdfs=wdfs
605700140613     c* Se anche la data foglio è = non carico la spunta ricevuta
605800140613    3C     BRvKEY        IFNE      SA1KEY
605900140613     C                   MOVEL     BRvKEY        SA1KEY
606000140613     C                   Z-ADD     BRvNPG        WNPG
606100140613     C                   Z-ADD     BRvNFV        WNFV
606200140613     C                   Z-ADD     BRvFGS        WFGS
606300140613     C                   movel     BRvnps        Wnps
606400140613     C                   EXSR      RILFOG
606500140613     c                   Z-ADD     WDFV          BRVDFV
606600140616     c                   movel     WFCF          BRVFCF
606700140616     c                   movel     Wspg          BRVspg
606800140613    3c                   endif
606900140613     c
607000140616     c                   if        barnpg=3 and
607100140616    3c                             brvdfv=bardfv and brvfcf=' '
607200140616     c                   eval      amb_QD ='S'
607300140613     c* mi slavo data e ora imm spunta 4 per la delete
607400140616     c                   eval      qd_npg=4
607500140616     c                   eval      qd_dfs=brvdfs
607600140616     c                   eval      qd_hms=brvhms
607700140616     c                   leave
607800140613     c                   endif
607900140616     c                   if        barnpg=4 and  brvspg='A' and
608000140616    3c                             brvdfv=bardfv and brvfcf=' '
608100140616     c                   eval      amb_QD ='S'
608200140616     c* mi slavo data e ora imm spunta 4 per la delete
608300140616     c                   eval      qd_npg=3
608400140616     c                   eval      qd_dfs=brvdfs
608500140616     c                   eval      qd_hms=brvhms
608600140616     c                   eval      qd_nfv=brvnfv
608700140616     c                   leave
608800140616     c                   endif
608900140613     c
609000140613    2c                   endsl
609100140616     c
609200140616     C     KBAR2_d       reade(n)  FNBRV05L
609300140616    1c                   enddo
609400140613     c
609500140613     c                   ENDSR
609600140613     C**************************************************************************
609700140613     C* Scelta data tra immisisone e caicamento
609800140613     C**************************************************************************
609900140613     c     Sceglidata    BEGSR
610000140613     c                   if         brv_imm>brv_sca and
610100140616     c                              %subst(%editc(brv_sca:'X'):1:8)>*zeros
610200140613     c                   eval       A_imm=brv_sca
610300140613     c                   else
610400140613     c                   eval       A_imm=brv_imm
610500140613     c                   endif
610600140613     c                   ENDSR
610700140613     C**************************************************************************
610800140613     C*    impostazione data immissione spunta
610900140613     C**************************************************************************
611000140613     c     ImpoDFS       BEGSR
611100140613     c* Se data immissione > di oggi, imposto la minore tra oggi e data fogl
611200140613     c                   if        BARDFS<=wdate1
611300140613     C                   Z-ADD     BARDFS        wDFS
611400140613     c                   else
611500140613     c                   if        wdate1<=bardfv
611600140613     c                   z-add     wdate1        wDFS
611700140613     c                   else
611800140613     c                   z-add     bardfv        wDFS
611900140613     c                   endif
612000140613     c                   endif
612100140613     c                   ENDSR
612200140616     C**************************************************************************
612300140616     C*    cancellazione spunta meno recente IMA o cons
612400140616     C**************************************************************************
612500140616     c     Canc_QD       BEGsr
612600140616     c                   eval      kdel_npg=qd_npg
612700140616    1c                   if        kdel_npg=4
612800140616     c     kbar_qd4      chain     fnbrv05l                             38
612900140616     c  n38              if        %found(fnbrv05l)
613000140616     C     KBRVE1        SETLL     FNBRVE1L                               33
613100140616    2C     *IN33         IFEQ      *OFF
613200140616     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
613300140616     C                   WRITE     FNBRVE
613400140616    2C                   ENDIF
613500140616     c
613600140616     c                   delete    fnbrv005
613700141029     c                   endif
613800140616     c
613900140616   x1c                   else
614000140616     c     kbar_qd4      setll     fnbrv05l
614100140616     c     kbar_qd4      reade     fnbrv05l
614200140616    2c                   dow       not %eof(fnbrv05l) and brvnfv<>qd_nfv
614300140616     c     kbar_qd4      reade     fnbrv05l
614400140616    2c                   enddo
614500140616    2c                   if        not %eof(fnbrv05l)
614600140616     C     KBRVE1        SETLL     FNBRVE1L                               33
614700140616    3C     *IN33         IFEQ      *OFF
614800140616     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
614900140616     C                   WRITE     FNBRVE
615000140616    3C                   ENDIF
615100140616     c                   delete    fnbrv005
615200140616    2c                   endif
615300140616     c
615400140616    1c                   endif
615500140616     c                   ENDSR
615600150622     C**************************************************************************
615700150622     C*    Aggiornamenti da tabella BSP
615800150622     C**************************************************************************
615900150622     c     DA_BSP        BEGsr
616000150622     c* Scrivo FNAGB per aggiornare la bolla
616100150622     C                   CLEAR                   FNAGB000
616200150622     C                   MOVEL     'P'           AGBTBO
616300150622     C                   MOVEL     'CB'          AGBAGB
616400150622     C                   MOVEL     blpAAS        AGBAAS
616500150622     C                   MOVEL     blpLNP        AGBLNP
616600150622     C                   MOVEL     blpNRS        AGBNRS
616700150622     C                   MOVEL     blpNSP        AGBNSP
616800150622     C                   WRITE     FNAGB000                             99
616900150622     c                   ENDSR
617000070208     C**************************************************************************
617100070208     C* Elaborazioni per chiusura pgm
617200070208     C**************************************************************************
617300070208     c     ELACHIUDI     BEGSR
617400070208     C* ELABORO L'ULTIMO DATO MEMORIZZATO
617500070208     C                   EXSR      WRTAGB
617600070208     c
617700070208     C                   CLEAR                   DSLV55
617800070208     C                   MOVEL     'C'           D55TLA
617900070208     C                   EXSR      FNLV55
618000070208     C* SCRITTURA ULTIMO RECORD IN FNSTA
618100070208     C   02              EXSR      WRTSTA
618200070208     C**
618300070208     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
618400070208     c                   clear                   fidn12ds
618500070208     c                   eval      i12tla = 'C'
618600070208     c                   call      'FIDN12R'
618700070208     c                   parm                    fidn12ds
618800161007     c
618900161007     c                   clear                   fnlgi1ds
619000161007     c                   eval      ilgi1tla = 'C'
619100161007     c                   movel     fnlgi1ds      kpjbu
619200161007     c                   call      'FNLGI1R'
619300161007     c                   parm                    kpjba
619400070208     c                   ENDSR
619500110103
619600110103      *---------------------------------------------------------------*
619700110103      *?Reperimento dati del job (Utente/Operativi)                  ?*
619800110103      *---------------------------------------------------------------*
619900110103     c     sr_DatiJob    BEGSR
620000110103      *
620100110103     c     *dtaara       define    §azute        AZUTEds
620200110103     c     *dtaara       define    §datiute      dDatiUte
620300110103      *
620400110103     c                   in(E)     *dtaara
620500110103     c                   IF        %Error or RSUT = *blanks
620600110103     c                   clear                   TIBS34ds
620700110103     c                   call      'TIBS34R'
620800110103     c                   parm                    TIBS34ds
620900110103     c                   in        *dtaara
621000110103     c                   ENDIF
621100110103      *
621200110103     c                   ENDSR
621300040601**         CM5
621400040601DLYJOB DLY(5)
621500980928**         MSG
621600070906CARICA SPUNTE:Fogli Inesitenti (FGS NPG NFV-NPS) FNLS45R
621700070906XXX X XXXXX-xx;  XXX X XXXXX-xx;  XXX X XXXXX-xx;  XXX X XXXXX-xx;
621800070906           -  ;             -  ;             -  ;             -  .
