000100151221     /*END
000200040224      *---------------------------------------------------------------*
000300040610      * INTERROGAZIONI  RICHIESTE  ASSISTENZA CLIENTI                 *
000400040224      *---------------------------------------------------------------*
000500040224
000600040224     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700131018     h dftactgrp(*no) actgrp(*new)
000800150417     h alwnull(*inputonly)
000900040224
001000040224      *---------------------------------------------------------------*
001100040224
001200040224     fFITGD01L  if   e           k disk
001300150324     fFITGD32L  if   e           k disk    rename(fitgd000:fitgd02)
001400040225     fFITGR01L  if   e           k disk
001500150319     fFITGM01L  if   e           k disk
001600040224     fAZORG01L  if   e           K disk
001700040226     fAZUTE01L  if   e           K disk
001800040419     fFNARB01L  if   e           k disk    usropn
001900170228     fFNBLP01L  if   e           k disk    usropn
002000040419     fFNORM01L  if   e           k disk    usropn
002100040224      *
002200150504     fFIDNA2D   cf   e             workstn sfile(FIA2S02:S01nrr)
002300140219     f                                     sfile(FIA2S03:S03nrr)
002400160104     f                                     ignore( FIA2W01 )
002500040224
002600140221       //--------------------------------------------------------------
002700140221       //?Definizione costanti.                                        ?
002800140221       //--------------------------------------------------------------
002900140221
003000140221       // -?Attributi di Visualizzazione?
003100150507     d c_DspAtrNorm    c                   const(x'20')
003200150507     d c_DspAtr_RI     c                   const(x'21')
003300150507     d c_DspAtr_HI     c                   const(x'22')
003400150507     d c_DspAtr_BL     c                   const(x'28')
003500150507     d c_DspAtrWht     c                   const(x'22')
003600150507     d c_DspAtrWht_RI  c                   const(x'23')
003700150507     d c_DspAtrWht_UL  c                   const(x'26')
003800150507     d c_DspAtrRed     c                   const(x'28')
003900150507     d c_DspAtrRed_RI  c                   const(x'29')
004000150507     d c_DspAtrRed_BL  c                   const(x'2A')
004100150507     d c_DspAtrRed_UL  c                   const(x'2C')
004200150312
004300150312       // -?Numero massimo di record gestibili in un SubFile?
004400150312     d c_MaxRec        c                   const(9999)
004500150428
004600150428       // -?Sede?
004700150428     d c_Sede          c                   const(046)
004800140221
004900040224      *---------------------------------------------------------------*
005000040224
005100040224      *
005200040224      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
005300040224      *
005400040224      *
005500040224      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
005600040224      *
005700040224     d KPJBA         e ds
005800040224      * - Parametri per gestione reclami già esistenti
005900040224     d FIDNA0DS      e ds
006000040302      * - Parametri ritorno dai PGM di Interrogazione BOLLE/ORM
006100040225     d FIDNA2DS      e ds
006200040302      * - Parametri che si passa ai pgm di interrogazione oggetti
006300040302     d FIDNA1DS      e ds
006400040227      * - Parametri per il recupero delle note
006500040227     d FIDNB0DS      e ds
006600041008      * - Parametri da passare al pgm di visualizzazione Bolle di sede
006700041008     D TNSB50DS      E DS
006800041215      * - Ds per Anagrafiche Clienti
006900041215     d TIBS69DS      E DS
007000041215     d DS_cnaco      E DS                  extname(CNACO00F)
007100041215     d DS_cnind      E DS                  extname(CNIND00F)
007200041215     d DS_cnclp      E DS                  extname(CNCLP00F)
007300041215     d DS_fncls      E DS                  extname(FNCLS00F)
007400041008      *
007500151221      * -?Gestione Telefonate - Flag Avanzamento Fase R.A.?
007600151221     d dTGDfar       e ds                  inz  qualified
007700151221      *
007800151221      * -?Gestione Telefonate - Flag operativi di FITGD00F.TGDFLO?
007900151221     d dTGD01        e ds                  inz  qualified
008000151221      *
008100151221      * -?Gestione Telefonate - Riferimenti Chiamante: Attesa Risposta?
008200151221     d dTGR01a       e ds                  inz  qualified
008300040224      *
008400040224      * - Parametri x Controllo profilo utenti
008500040224     d TIBS34DS      e ds                  inz
008600040224      * - Ds di riferimento al file esterno AZUTE00F
008700040224     d AZUTEDS       e ds                  extname(AZUTE00F)
008800040224      * - Ds per dati organigramma
008900040224     d DDatiUte      e ds
009000040224      * - Tabelle
009100040224     d TIBS02ds      e ds                  inz
009200131018     d tntbMARds1    e ds                  inz
009300160118     d tntbPRRds     e ds                  inz
009400150320     d tntbUGRds     e ds                  inz
009500140925
009600140926      // -?Reperimento Disposizioni di Consegna da WEB
009700141029     d FNLRY09DS2    e ds                  inz
009800140926
009900140926      // -?Caricamento Filiali gestite dall'utente
010000140926     d TRUL31DS      e ds                  inz
010100140926
010200040224      *
010300040331     d DPRR          e ds
010400040224     d DMAR          e ds
010500040224     d DCHR          e ds
010600040224     d DCMU          e ds
010700040224     d DCOR          e ds
010800040224     d DOGR          e ds
010900040224     d DCCR          e ds
011000040615     d DUGR          e ds
011100041012     d DREC          e ds
011200130806     d Dute01        e ds
011300150417      *
011400150417       // -?Dati reperiti via SQL?
011500150417     d FITGD_ds      e ds                  extname(FITGD00F)
011600150417     d                                     inz
011700160706      *
011800160706      * -?Parametri da passare al pgm interrogaz. singola Bolla di Filiale?
011900160706     d ParamU6ds1      ds                  inz   qualified
012000160706      ** ?CAMPI DI INPUT:?
012100160706     d  V2Caas                14     17  0 inz
012200160706     d  COMlnp                18     20  0 inz
012300160706     d  V2Cnrs                21     22  0 inz
012400160706     d  V2Cnsp                23     29  0 inz
012500160706      ** ?CAMPI DI OUTPUT:?
012600160706      * ?. Flag '1'=Premuto F3=Fine?
012700160706     D  PARf03                30     30
012800160706      ** ?CAMPI DI INPUT:?
012900160706      * ?. Modalità di richiamo?
013000160706     D  PARflg                31     31
013100160706      * ?. Flag '1'= RICH DA PGM GIAC.?
013200160706     D  PARgia               144    144
013300160706      ** ?CAMPI DI OUTPUT:?
013400160706     d  PARrma               149    163                                         x F22=PerfectOrder
013500160706     d  PARxco               164    164                                         x F22=PerfectOrder
013600160706      ** ?CAMPI DI INPUT:?
013700160706      * ?. Tipo lancio: "C" -           CHIUSO CON LR?
013800160706      *                ?"L" - ELABORO E CHIUDO CON LR?
013900160706      *                ?" " - ELABORO E CHIUDO IN RETRN?
014000160706     d  PARtla               165    165
014100170119      ** ?CAMPI DI OUTPUT:?
014200170119      * ?. Codice di errore: "1" = Sped. non trovata?
014300170119     d  PARerr               166    166
014400170119     d  PARflo               167    256
014500160706      *
014600160706      * -?Altri Parametri da passare al pgm interrogaz. Bolle di Filiale?
014700160706     d fnlrU6ds      e ds                  inz
014800160706      *
014900040224      *
015000040224     d Status         sds
015100040224     d  SDSpgm           *proc
015200040224     d  PARMS            *parms
015300040224      *
015400040224     d WLBdat          ds                  inz
015500040224     d  G02dat                 1      8  0 inz
015600040224     d  G02inv                 9     16  0 inz
015700040224     d  G02err                17     17    inz('3')
015800040224     d  G02tgi                18     22  0 inz
015900040224      * - DS per girare le date
016000040224     D                 DS
016100040224     D  anno4                  1      4  0
016200040224     D  anno2                  3      4  0
016300040224     D  mese                   5      6  0
016400040224     D  giorno                 7      8  0
016500040224     D  data8                  1      8  0
016600040224
016700040224      * - DS Parametri per ricerca Punto Operativo
016800040224     D Dstnsd          DS
016900040224     D  §cod                          3
017000040224     D  §tip                          1
017100040224     D  §des                         25
017200040224     D  §pra                          3
017300040301      * _ DS Numero spedizione
017400040226     D                 DS
017500040226     D  v01lnp                        3  0
017600040226     D  v01nrs                        2  0
017700040226     D  v01nsp                        7  0
017800040226     D  v01ans                        4  0
017900040227     D  spevid                 1     16  0 inz
018000040226     D                 DS
018100040226     D  vw1lnp                        3  0
018200040226     D  vw1nrs                        2  0
018300040226     D  vw1nsp                        7  0
018400040226     D  vw1ans                        4  0
018500040227     D  W_spevid               1     16  0 inz
018600040227     D                 DS
018700040227     D  WRKlnp                        3
018800040227     D  WRKnrs                        2
018900040227     D  WRKnsp                        7
019000040227     D  WRKann                        4
019100040227     D  WRKSPE                 1     16
019200040301     D                 DS
019300040301     D  v02lnp                        3  0
019400040301     D  v02nrs                        2  0
019500040301     D  v02nsp                        7  0
019600040301     D  v02ann                        4  0
019700040301     D  spevid2                1     16  0 inz
019800040301      * _ DS Numero ORM
019900040226     D                 DS
020000040226     D  v01poe                        3  0
020100040226     D  v01nso                        2  0
020200040226     D  v01nor                        7  0
020300040226     D  v01nrv                        2  0
020400040227     D  ormvid                 1     14  0 inz
020500040226     D                 DS
020600040226     D  vw1poe                        3  0
020700040226     D  vw1nso                        2  0
020800040226     D  vw1nor                        7  0
020900040226     D  vw1nrv                        2  0
021000040227     D  W_ormvid               1     14  0 inz
021100040227     D                 DS
021200040227     D  WRKpoe                        3
021300040227     D  WRKnso                        2
021400040227     D  WRKnor                        7
021500040227     D  WRKnrv                        2
021600040227     D  WRKORM                 1     14
021700040301     D                 DS
021800040301     D  v02poe                        3  0
021900040301     D  v02nso                        2  0
022000040301     D  v02orm                        7  0
022100040301     D  v02nrv                        2  0
022200040301     D  ormvid2                1     14  0 inz
022300040302      *
022400040302      * - Parametri da passare al pgm di visualizzazione ORM
022500040302     D Parm01          DS
022600040302     D  ppoe                          3  0
022700040302     D  pnor                          7  0
022800040302     D  pnsr                          2  0
022900040302     D  pnrv                          2  0
023000040302     D  psce                          1
023100040302     D  pfgs                          3  0
023200040302     D  ppor                          3  0
023300040302     D  pdtr                          8  0
023400040302     D  pmdb                         10
023500040302     D  pprb                         10
023600040302     D  pdts                          8  0
023700040302     D  prmp                          1
023800040302     D  pbrc                          1
023900040302     D  pref                          2
024000040302     D  pmio                          1
024100040302      *
024200140926      // -?Parametri per ricerca Programmi chiamanti
024300140926     d dsStack         ds
024400140926     d  skStack                      10    Dim(100)
024500150317
024600150317       // -?Filiali dell'Abilitazione Manutenzione Bolle di Filiale?
024700150317     d sk_POg_ds1      ds                  inz
024800150317     d   sk_POg1                      3  0 dim(250)  inz
024900150317
025000150317       // -?Filiali dell'Abilitazione Variazione R.A. clienti?
025100150317     d sk_POg_ds2      ds                  inz
025200150317     d   sk_POg2                      3  0 dim(250)  inz
025300040224
025400040224      *
025500040224      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
025600040224      *
025700040224      * - Campi di comodo
025800040224     d w0140           s             14  0
025900040224     d wData           s              8  0
026000040224     d DateU           s              8  0
026100140526     d ww              s              3  0 inz
026200040225     d wmod            s              1
026300040225     d Wddar           s              8  0
026400040225     d Wadar           s              8  0
026500040531     d Wddcr           s              8  0
026600040531     d Wadcr           s              8  0
026700040227     d s01nrr          s              4  0
026800140219     d S03nrr          s              4  0
026900040227     d Wrecok          s              1
027000040304     d Wlrsc           s              3  0
027100040505     d Wlrpg           s              3  0
027200040315     d W_f03           s              1
027300040331     d W_ute           s              3
027400040505     d W_rpggen        s              1
027500041215     d wtibs69r        s              1
027600040419     d SavIn05         s              1    inz(*off)
027700150323     d $Ord_Desc       s               n   inz(*on)
027800150506     d $InzSFL         s               n   inz(*on)
027900151022     d xx              s              4  0 inz
028000151022     d yy              s              4  0 inz
028100150610
028200150610     d wCountRA        s              5  0 inz
028300040225
028400040225     d W_pot           s                   like(orgfil)
028500040225     d W_pog           s                   like(orgfil)
028600040603     d W_por           s                   like(orgfil)
028700040227     d W_ogg           s                   like(tgdogg)
028800040505     d W_rpg           s                   like(v01rpg)
028900040505     d W_rpgmax        s                   like(v01rpg)
029000150603     d W1Togg          s                   like(V1Togg)   inz
029100040315     d v01tor          s                   like(tgdtor)
029200140926     d wabi            s                   like(UTEaut)
029300160201     d WrepR           s                   like(dTGR01a.§TGRrepR)  inz
029400160201     d WfilR           s                   like(dTGD01.§TGDfilR)   inz
029500150417
029600150417       // -?Campi per l'estrazione SQL dei dati per il subfile?
029700150417     d wSQL            s           7000    inz  varying
029800150417     d $First          s               n   inz(*on)
029900160129     d $First2         s               n   inz(*on)
030000140926
030100140926      // -?Campi per procedura esterna
030200140926     d  pStack         s           1000
030300140926     d  pStartPoint    s              3  0
030400140926     d  pEntries       s              3  0
030500140220
030600140220       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
030700140220
030800140220       //--------------------------------------------------------------
030900140220       // -?Constants?
031000140220       //--------------------------------------------------------------
031100140220       // -?MaxKey - Maximum number of key fields allowed by this program?
031200140220     d c_MaxKey        c                   const(3)
031300140220       // -?Sort order:?
031400140220       //  ?1 = Sort in an ascending sequence?
031500140220       //  ?2 = Sort in a descending sequence?
031600140220     d c_Ascendente    c                   const(1)
031700140220     d c_Discendente   c                   const(2)
031800140220       // -?Key data type:?
031900140220       //  ? 0 = Signed binary?
032000140220       //  ? 2 = Signed zoned decimal?
032100140220       //  ? 3 = Signed packed decimal?
032200140220       //  ? 6 = Character with no national language sort sequence applied,?
032300140220       //  ?     if specified?
032400140220       //  ? 7 = Unsigned packed decimal?
032500140220       //  ?     All numbers will have the sign forced positive ('F'X).?
032600140220       //  ? 8 = Unsigned zoned decimal?
032700140220       //  ?     All numbers will have the sign forced positive ('F'X).?
032800140220       //  ? 9 = Unsigned binary?
032900140220       //  ?14 = Date in form of DD/MM/YY?
033000140220       //  ?15 = Date in form of DD.MM.YYYY?
033100140220     d c_Numero        c                   const(2)
033200140220     d c_Carattere     c                   const(6)
033300140220     d c_NumIntero     c                   const(8)
033400140220       //
033500140220     d c_Put           c                   const(1)
033600140220     d c_EndPut        c                   const(2)
033700140220     d c_Get           c                   const(3)
033800140220
033900140220       //--------------------------------------------------------------
034000140220       // -?Data Structures?
034100140220       //  ?SflRcd     - The entire subfile record to pass to the sort?
034200140220       //  ?QLGSCB     - The sort request block for the QLGSORT API?
034300140220       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
034400140220       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
034500140220       //  ?QUSEC      - Error structure for the QLGSORT API?
034600140220       //--------------------------------------------------------------
034700150317     d SflRcd          ds                  inz
034800150325     d   VS1ant                                                                   1-  4: 4
034900150325     d   VS1nut                                                                   5- 13: 9
035000150325     d   VS1pdt                                                                  14- 16: 3
035100150325     d   VS1pot                                                                  17- 19: 3
035200150325     d   VS1ges                                                                  20- 20: 1
035300150325     d   VS1dar                                                                  21- 26: 6
035400150417     d   VS1rpr                                                                  27- 36: 10
035500150417     d   VS1rpg                                                                  37- 46: 10
035600150417     d   VS1pog                                                                  47- 49: 3
035700150417     d   VS1rsc                                                                  50- 77: 28
035800150417     d   VS1tch                                                                  78- 78: 1
035900150417     d   VSDmad                                                                  79- 96: 18
036000151221     d   HS1prt2                                                                 97- 97: 1
036100150417     d   HS1dac                                                                  98-105: 8
036200150417     d   VS1orc                                                                 106-109: 4
036300150417     d   VS1ogg                                                                 110-135: 26
036400150417     d   VS1cdt                                                                 136-138: 3
036500150417     d   VS2dcd                                                                 139-146: 8
036600150417     d   VS1no1                                                                 147-181: 35
036700150417     d   VS1no2                                                                 182-216: 35
036800150325     d*//VS1nrr                                                                 -------: 13
036900150417     d   VSHin06                                                                217-217: 1
037000150417     d   VS1opz                                                                 218-218: 1
037100150417     d   VSDmad2                                                                219-247: 29
037200150417     d   VS1rsc2                                                                248-268: 21
037300150417     d   VS1txt1                                                                269-343: 75
037400150417     d   VS1txt2                                                                344-418: 75
037500151221     d   VS1prt                                                                 419-419: 1
037600151221     d   DspAtrPRT                                                              420-420: 1
037700151221     d   DspAtrDAR                                                              421-421: 1
037800151221     d   DspAtrRSC                                                              422-422: 1
037900140220     d   Selected                     1a   inz
038000140220      /copy QSYSINC/QRPGLESRC,QLGSORT
038100140220     d QLGKL                         16    dim(c_MaxKey)
038200140220     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
038300140220     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
038400140220     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
038500140220     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
038600140220      /copy QSYSINC/QRPGLESRC,QLGSRTIO
038700140220      /copy QSYSINC/QRPGLESRC,QUSEC
038800140220
038900140220       //--------------------------------------------------------------
039000140220       // -?Standalone fields?
039100140220       //  ?Nrr        - Relative record number for subfile?
039200140220       //  ?SizeList   - Size of list?
039300140220       //  ?ReturnSize - Size of list returned by sort API?
039400140220       //--------------------------------------------------------------
039500140220     d NotUsed         s             16a   inz
039600170228     d ReturnSize      s             10i 0 inz
039700170228     d SizeList        s             10i 0 inz
039800140220     d RrnLast         s              5i 0 inz
039900140220     d Nrr             s              5i 0 inz
040000140220
040100140220       //--------------------------------------------------------------
040200140220       //?Definizione prototipi procedure usate.                       ?
040300140220       //--------------------------------------------------------------
040400140220
040500140220       // -?Ordinamento subfile?
040600140220      /copy gaitrasrc/srcProtoPR,QLGSRTIO
040700140925
040800140925       // -?Reperimento Disposizioni di consegna da WEB
040900140925     d fnlry09r        pr                  extpgm('FNLRY09R')
041000141013     d  kpjba                              likeds(kpjba)
041100140926
041200140926       // -?Carica filiali abilitate
041300140926      /copy gaitrasrc/srcprotopr,trul31r
041400150428
041500150428       // -?Ricerca/Controllo tabelle (TNTBE)?
041600150428      /copy gaitrasrc/srcProtoPR,TIBS02R
041700150508
041800150508       // -?Selezione tab. "RGR" = Reparto Gestione R.A.?
041900150508     d tntbRGRds     e ds                  inz
042000150508     d tntbRGRr        pr                  extpgm('TNTBRGRR')
042100150508     d   kpjba                             likeds(kpjba)
042200140926
042300140926       // -?Pgm per ricerca Programmi chiamati
042400140926     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
042500140926     d  pStack                     1000
042600140926     d  pStartPoint                   3  0
042700140926     d  pEntries                      3  0
042800140220
042900040224      *
043000070601      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
043100040224      *
043200040224
043300040224      *
043400040224      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
043500040224      *
043600040301      * - file FITGD01L
043700040421     c     ktgdan        klist
043800040421     c                   kfld                    v01ant
043900040421     c                   kfld                    v01nut
044000040421      *
044100040301     c     ktgd01        klist
044200040421     c                   kfld                    vs1ant
044300040301     c                   kfld                    vs1nut
044400040301     c                   kfld                    vs1pdt
044500150324      * - file FITGD32L
044600150324     c     ktgd02        klist
044700040226     c                   kfld                    ia2tor
044800040226     c                   kfld                    ia2ogg
044900040226
045000040226     c     ktgdv2        klist
045100040226     c                   kfld                    v01tor
045200040226     c                   kfld                    W_ogg
045300040421      * - file FITGR01L
045400040421     c     ktgr          klist
045500040421     c                   kfld                    vs1ant
045600040421     c                   kfld                    vs1nut
045700040419      * - file FNORM01L
045800040419     c     k04orm01      klist
045900040419     c                   kfld                    V02poe
046000040419     c                   kfld                    V02nso
046100040419     c                   kfld                    V02orm
046200040419     c                   kfld                    V02nrv
046300040226      *
046400040224
046500040224      *===============================================================*
046600040224      *  RIEPILOGO INDICATORI                                         *
046700040224      *---------------------------------------------------------------*
046800040224      *  01    - Visualizza numero spedizione                         *
046900040224      *  02    - Visualizza numero Orm                                *
047000040224      *  03    - Visualizza chiave generica                           *
047100040301      *  04    - Visualizza dati relativa alla chiusura               *
047200040419      *  05    - Pgm richiamato in modalità interrogazione            *
047300040304      *  06    - Progressivo chiuso condiziona campi in SFL           *
047400041012      *  07    - Utente abilitato a modificare tutto                  *
047500140224      *  08    - Più di un rec. nel subfile S03 (per F11=Ordinamento) *
047600150506      *  09    - Visualizza F5=Refresh per S02/C02                    *
047700150325      *  10    - Posizionamento cursore su Rag.Soc. Chiamante         *
047800150626      *  12    - Comodo                                               *
047900160202      *  14    - Sono in Sede                                         *
048000040405      *  15    - Pgm richiamato                                       *
048100150319      *  16    - Abilita F11=Variazioni in D02                        *
048200140926      *  19    - Abilita F19-Disposizioni di Consegna                 *
048300040224      *  20    - SFLDSPCTL in C01                                     *
048400040224      *  21    - SFLDSP    in C01                                     *
048500040224      *  21    - SFLEND    in C01                                     *
048600040224      *  22    - SFLCLR    in C01                                     *
048700040301      *  29    - SFLCLR    in C01                                     *
048800040226      *  30/49 - Errori campi a video                                 *
048900150325      *  50    - Posizionamento cursore su Stato R.A.                 *
049000150325      *  51    - Posizionamento cursore su Filiale Apertura           *
049100150325      *  52    - Posizionamento cursore su Filiale Gestione           *
049200150325      *  53    - Posizionamento cursore su Tipo Comunicazione         *
049300040315      *  54    - ERRORE tipo oggetto                                  *
049400150325      *  55    - Posizionamento cursore su Motivo Apertura            *
049500150325      *  56    - Posizionamento cursore su Codice Chiusura            *
049600040331      *  57/61 - Errori campi a video                                 *
049700150325      *  62    - Posizionamento cursore su Utente Gestione            *
049800040407      *  63/65 - Errore campi a video                                 *
049900150910      *  68    - "Vecchio" errore: tolto a *PGM ma lasciato nel *DSPF *
050000140217      *  69/71 - Errore su opzione                                    *
050100150626      *  72/80 - Errori campi a video                                 *
050200150325      *  81    - Posizionamento cursore su Filiale Responsabile       *
050300040224      *  90    - Errore                                               *
050400040224      *===============================================================*
050500040224
050600040224     c                   do        *hival
050700140221
050800140225     c                   if        Not *in90
050900140221     c                   clear                   V1Togg
051000140221     c                   write     fia2t01
051100140225     c                   endif
051200040225
051300040225      * se sono stato richiamato salto questa videata
051400040225
051500040225     c                   if        wmod = ' '
051600040315     c                   setoff                                       90
051700040225     c                   exfmt     fia2d01
051800040315
051900040331     c                   setoff                                       506210
052000040331     c                   setoff                                       515256
052100040315     c                   setoff                                       535455
052200040531     c                   setoff                                       81
052300150312
052400150312sel 3c                   Select
052500040401
052600040401      * F3 = Fine
052700040401
052800150312     c                   When      *inkc
052900040401     c                   leave
053000040225
053100040401      * F10 = Immissione
053200040225
053300150312w   3c                   When      *inkj
053400161027     c                   call      'FIDNA1R'
053500040401     c                   parm                    kpjba
053600040401     c                   iter
053700041215
053800150610      * F1=Interrogazione piano dei conti
053900041215
054000150610w   3c                   When      *inKA
054100041215     c                   exsr      SR_ricalf
054200041215     c                   iter
054300150312
054400150312e   3c                   EndSl
054500041215
054600040225
054700040225      * Controllo video selezioni
054800040225
054900040225     c                   exsr      SR_contr1
055000040225
055100040225     c                   endif
055200040225
055300040225      * se non ci sono errori emetto il subfile
055400040225
055500040225     c                   if        not *in90
055600040227     c                   exsr      SR_GESSBF
055700040225     c                   endif
055800040225
055900040225      * al ritorno dal subfile se sono stato richiamato vado a fine PGM
056000040225
056100040225     c                   if        Wmod = 'R'
056200040225     c                   leave
056300040225     c                   endif
056400040225
056500040225     c                   enddo
056600041215
056700041215     C                   IF        Wtibs69r  <>  *BLANKS
056800041215     C                   eval      I69TLA  = 'C'
056900041215     C                   call      'TIBS69R'
057000041215     C                   parm                    TIBS69DS
057100041215     C                   ENDIF
057200040225
057300040225     c                   seton                                        LR
057400040225      *---------------------------------------------------------------*
057500040225      * Routine Controllo videata selezione                           *
057600040225      *---------------------------------------------------------------*
057700040227     c     SR_contr1     BegSR
057800040225      *
057900040225
058000150319      *?Filiale in gestione?
058100150319
058200150319     C                   clear                   vd1pog
058300150319    2c                   If        v01pog <> *blanks
058400150319      *
058500150319      * Ricerca
058600150319     c     '?'           scan      v01pog
058700150319    3c                   if        %found
058800150319     C                   clear                   dstnsd
058900150319     C                   call      'TNSD24R'
059000150319     C                   parm                    §cod
059100150319     C                   parm                    §tip
059200150319     C                   parm                    §des
059300150319     C                   parm                    §pra
059400150319     C                   movel     §cod          v01pog
059500150319     C                   movel     §des          vd1pog
059600150319     c                   eval      *in90 = *on
059700150319     C                   eval      *IN52 = *on
059800150319     c                   leavesr
059900150319    3C                   endif
060000150319    2C                   endif
060100150319      *
060200150319    2C     v01pog        ifeq      *blanks
060300150319     C                   move      *all'0'       v01pog
060400150319    2C                   endif
060500150319      *
060600150626     C                   testn                   v01pog               12
060700150319     C                   move      v01pog        w001a             1
060800150319      *
060900150626    2C     *in12         ifeq      *off
061000150319     C     w001a         orlt      '0'
061100150319     c                   eval      *in90 = *on
061200150319     C                   eval      *IN44 = *on
061300150319     c                   leavesr
061400150319    2C                   endif
061500160219      *
061600160219     c                   move      v01pog        W_pog
061700150319      * verifico se P.O. esistente
061800151023    2c                   if        W_pog > 0
061900151023     c                   IF        w_pog = 999 and simfel = 0
062000151023     c                   eval      *in90 = *on
062100151023     C                   eval      *IN44 = *on
062200151023     c                   leavesr
062300151023    3c                   endif
062400151023     c                   IF        w_pog <> 999
062500160104     C     W_pog         chain     azorg
062600150319    3C                   if        %found(azorg01l)
062700150319     C                   movel     orgdes        vd1pog
062800150319      *  errore
062900150319     C                   ELSE
063000150319     c                   eval      *in90 = *on
063100150319     C                   eval      *IN44 = *on
063200150319     c                   leavesr
063300150319    3c                   endif
063400151023     c                   ENDIF
063500150319      *
063600150319    2c                   endif
063700150417      *
063800150417      *?Reparto gestione?
063900150417      *
064000150417     c                   Select
064100150417      * -?Vuoto?
064200150417     c                   When      V01rpr = *blank
064300150417      * -?Interrogazione?
064400150417     c                   When      %scan( '?' : V01rpr ) > *zero
064500150417     c                   clear                   V01rpr
064600150508     c                   clear                   tntbRGRds
064700150508     c                   eval      iRGRopz = 1
064800150508     c                   eval      kpjbu   = tntbRGRds
064900150508     c                   call      'TNTBRGRR'
065000150420     c                   parm                    KPJBA
065100150508     c                   eval      tntbRGRds = kpjbu
065200150508    4c                   If        oRGRerr <> *on  and
065300150508     c                             oRGRfxx =  *blank
065400150508     c                   movel     oRGRcod       V01rpr
065500150420     c                   Else
065600150420     c                   clear                   V01rpr
065700150420    4c                   EndIf
065800150417     c                   eval      *in90 = *on
065900150417     c                   leavesr
066000150417      * -?Controllo?
066100150417     c                   Other
066200150420     c                   clear                   TIBS02ds
066300150420     c                   eval      T02mod = 'C'
066400150420     c                   eval      T02sif = KNSIF
066500150420     c                   eval      T02cod = 'RGR'
066600150420     c                   eval      T02ke1 = V01rpr
066700150420     c                   call      'TIBS02R'
066800150420     c                   parm                    KPJBA
066900150420     c                   parm                    TIBS02ds
067000150423    4c                   If        T02err <> *blanks
067100150420     c                   eval      *in90 = *on
067200150420     c                   eval      *in67 = *on
067300150420     c                   leavesr
067400150420    4c                   EndIf
067500150417     c                   EndSl
067600150319      *
067700150319      *?Utente in gestione?
067800150319      *
067900150319     c                   eval      W_rpggen = ' '
068000150319     c                   clear                   W_rpg
068100150319     c                   eval      W_rpgmax = *all'9'
068200150319     c                   clear                   Wlrpg
068300150319    2c                   If         v01rpg <> *blanks
068400150319      * verifico se richiesto utente in gestione generico controllo    se inserito "*"
068500150319     c                   eval      Wlrpg = %scan('*':V01rpg)
068600150319     c                   if        Wlrpg > 0
068700150319     c                   sub       1             Wlrpg
068800150319      * flag di ricerca generica utente
068900150319     c                   eval      W_rpggen = 'Y'
069000150319      * campo chiave per lettura file per utente
069100150319     c                   eval      W_rpg = %subst(v01rpg:1:Wlrpg)
069200150319     c                   eval      %subst(W_rpgmax:1:Wlrpg) =
069300150319     c                              %subst(v01rpg:1:Wlrpg)
069400150319     c                   endif
069500150319      * se non è stata richiesta la ricerca parziale
069600150319   3ac                   if        W_rpggen = ' '
069700160104     c     v01rpg        chain     azute000
069800150319      *
069900150319    4c                   if        not %found(azute01l)
070000150319     c                   eval      *in90 = *on
070100150319     C                   eval      *IN63 = *on
070200150319     c                   leavesr
070300150319    4C                   ENDIF
070400150319      *
070500150319   3ac                   endif
070600150319      *
070700150319    2C                   endif
070800150319
070900150324      *?Stato (ex Priorità) reclamo?
071000150319
071100150319     c                   clear                   VD1prt
071200150319    2c                   If        v01prt <> *blanks
071300150319     c                   clear                   tibs02ds
071400150319      *
071500150319      * Ricerca
071600150319    3c                   if        v01prt = '?'
071700160118     c*//                clear                   tibs02ds
071800160118     C*//                MOVEL     'R'           t02mod
071900160118     C*//                MOVEL     knsif         t02sif
072000160118     C*//                MOVEL     'PRR'         t02cod
072100150319      *
072200160118     C*//                CALL      'TIBS02R'
072300160118     C*//                PARM                    KPJBA
072400160118     C*//                PARM                    TIBS02DS
072500150319      *
072600160118    4C*//                IF        T02err = *BLANKS
072700160118     C*//                movel     T02uni        DPRR
072800160118     C*//                movel     T02ke1        v01prt
072900160118     C*//                movel     §PRRdesc      vd1prt
073000160118     c                   clear                   tntbPRRds
073100160118     c                   eval      iPRRopz = '1'
073200160118     c                   movel(P)  tntbPRRds     KPJBU
073300160118     c                   call      'TNTBPRRR'
073400160118     c                   parm                    KPJBA
073500160118     c                   movel     KPJBU         tntbPRRds
073600160118      *
073700160118     c                   IF        oPRRfxx = *blanks  and
073800160118     c                             oPRRerr = *off
073900160118     c                   movel     oPRRcod       V01prt
074000160118     C                   movel     oPRRdes       VD1prt
074100150319      *  errore (non trovato nulla pulisco il '?' )
074200150319     C                   ELSE
074300150319     c                   clear                   V01prt
074400150319    4C                   ENDIF
074500150319      *
074600150319     c                   eval      *in90 = *on
074700150325     C                   eval      *in50 = *on
074800150319     c                   leavesr
074900150319      *
075000150319   x3C                   ELSE
075100150319      *
075200150319      * Controllo
075300150319     c                   clear                   tibs02ds
075400150319     C                   MOVEL     'C'           t02mod
075500150319     C                   MOVEL     knsif         t02sif
075600150319     C                   MOVEL     'PRR'         t02cod
075700150319     C                   MOVEL     v01PRT        t02ke1
075800150319      *
075900150319     C                   CALL      'TIBS02R'
076000150319     C                   PARM                    KPJBA
076100150319     C                   PARM                    TIBS02DS
076200150319      *
076300150319    4C                   IF        t02err = *BLANKS
076400150319     C                   movel     T02uni        DPRR
076500150319     C                   movel     §PRRdesc      vd1prt
076600150319      *
076700150319     C                   ELSE
076800150319     c                   eval      *in90 = *on
076900150319     C                   eval      *IN59 = *on
077000150319     c                   leavesr
077100150319    4C                   ENDIF
077200150319      *
077300150319    3c                   endif
077400150319      *
077500150319    2C                   endif
077600150319
077700150319      *?Data Chiamata DAL?
077800150319
077900150319     C                   clear                   WDDAR
078000150319    2c                   If        vd1dar > 0
078100150319     C                   clear                   wlbdat
078200150319     C                   z-add     vd1dar        G02dat
078300150319     C                   call      'XSRDA8'
078400150319     C                   parm                    wlbdat
078500150319    3C                   if        G02err = '1'
078600150319     c                   eval      *in90 = *on
078700150319     C                   eval      *IN41 = *on
078800150319     C                   leavesr
078900150319     C                   else
079000150319     C                   z-add     G02dat        vd1dar
079100150319     C                   z-add     G02inv        Wddar
079200150319    3C                   endif
079300150319
079400150319    2C                   endif
079500150319
079600150319      *?Data Chiamata AL?
079700150319
079800150319     C                   clear                   WADAR
079900150319    2c                   If        va1dar > 0
080000150319     C                   clear                   wlbdat
080100150319     C                   z-add     va1dar        G02dat
080200150319     C                   call      'XSRDA8'
080300150319     C                   parm                    wlbdat
080400150319    3C                   if        G02err = '1'
080500150319     c                   eval      *in90 = *on
080600150319     C                   eval      *IN42 = *on
080700150319     C                   leavesr
080800150319     C                   else
080900150319     C                   z-add     G02dat        va1dar
081000150319     C                   z-add     G02inv        Wadar
081100150319    3C                   endif
081200150319
081300150319    2C                   endif
081400150319      * se data dal è valorizzata e data al no imposto la stessa data anche in data al
081500150319    2c                   If        va1dar = 0 and vd1dar > 0
081600150319     c                   eval      va1dar = vd1dar
081700150319     c                   eval      wadar = wddar
081800150319    2c                   endif
081900150319      * verifico se data al è maggiore o uguale di data dal
082000150319    2c                   if        wadar < wddar
082100150319     c                   eval      *in90 = *on
082200150319     C                   eval      *IN43 = *on
082300150319     C                   leavesr
082400150319    2c                   endif
082500150319
082600150319      *?Data Chiusura DAL?
082700150319
082800150319     C                   clear                   WDDCR
082900150319    2c                   If        vd1dcr > 0
083000150319     C                   clear                   wlbdat
083100150319     C                   z-add     vd1dcr        G02dat
083200150319     C                   call      'XSRDA8'
083300150319     C                   parm                    wlbdat
083400150319    3C                   if        G02err = '1'
083500150319     c                   eval      *in90 = *on
083600150319     C                   eval      *IN72 = *on
083700150319     C                   leavesr
083800150319     C                   else
083900150319     C                   z-add     G02dat        vd1dcr
084000150319     C                   z-add     G02inv        Wddcr
084100150319    3C                   endif
084200150319
084300150319    2C                   endif
084400150319
084500150319      *?Data Chiusura AL?
084600150319
084700150319     C                   clear                   WADCR
084800150319    2c                   If        va1dcr > 0
084900150319     C                   clear                   wlbdat
085000150319     C                   z-add     va1dcr        G02dat
085100150319     C                   call      'XSRDA8'
085200150319     C                   parm                    wlbdat
085300150319    3C                   if        G02err = '1'
085400150319     c                   eval      *in90 = *on
085500150319     C                   eval      *IN74 = *on
085600150319     C                   leavesr
085700150319     C                   else
085800150319     C                   z-add     G02dat        va1dcr
085900150319     C                   z-add     G02inv        Wadcr
086000150319    3C                   endif
086100150319
086200150319    2C                   endif
086300150319      * se data dal è valorizzata e data al no imposto la stessa data anche in data al
086400150319    2c                   If        va1dcr = 0 and vd1dcr > 0
086500150319     c                   eval      va1dcr = vd1dcr
086600150319     c                   eval      wadcr = wddcr
086700150319    2c                   endif
086800150319      * verifico se data al è maggiore o uguale di data dal
086900150319    2c                   if        wadcr < wddcr
087000150319     c                   eval      *in90 = *on
087100150319     C                   eval      *IN75 = *on
087200150319     C                   leavesr
087300150319    2c                   endif
087400150319      *
087500150319      *?se non sono stati richiesti i solo reclami aperti?
087600150319      *   ?non inserire date chiusura?
087700150319      *
087800160120    2c                   If        (v01ape = 'SI' or V01att = 'S')
087900150319     c                             and wddcr > 0
088000150319     c                   eval      *in90 = *on
088100160120     c                   if        V01ape  = 'SI'
088200150319     C                   eval      *IN73 = *on
088300160120     c                   else
088400160120     c                   eval      *in46 = *on
088500160120     c                   endif
088600150319     c                   leavesr
088700150319    2c                   endif
088800150319      *
088900150319      *?se selezionato chiamante solo per la parte relativa al contenuto?
089000150319      *   ?deve esistere?
089100150319      *
089200150320    2c                   If        v01rpc = 'X' and v01rsc = *blank
089300150319     c                   eval      *in90 = *on
089400150319     C                   eval      *IN58 = *on
089500150319     c                   leavesr
089600150319     c                   endif
089700150319      *
089800150319    2c                   If        v01rsc <> *blank
089900150319     c                   clear                   Wlrsc
090000150319     c     ' '           checkr    V01rsc        Wlrsc
090100150319    2c                   endif
090200150319
090300150319      *?Codice Cliente?
090400150319      *?(recupero categoria del cliente)?
090500150319
090600150319     c                   if        v01ksc > 0
090700150319
090800150319      * aggancio anagrafico
090900150319
091000150319     C                   clear                   TIBS69DS
091100150319     c                   z-add     v01ksc        I69kac
091200150319     C                   call      'TIBS69R'
091300150319     C                   parm                    tibs69DS
091400150319     C                   parm                    DS_cnaco
091500150319     C                   parm                    DS_cnind
091600150319     C                   parm                    DS_cnclp
091700150319     C                   parm                    DS_fncls
091800150319     C                   eval      wtibs69r = '1'
091900150319     c                   If         O69ERR =  '1'
092000150319     C                   eval      *IN90 = *on
092100150319     C                   eval      *IN66 = *on
092200150319     c                   leavesr
092300150319     c                   Else
092400150319     c                   movel     ACOrag        vd1ksc
092500150319     c                   endif
092600150319
092700150319     c                   Endif
092800150319
092900150319      * Controllo la ragione sociale del cliente
093000150319
093100150319     c                   if        vd1ksc <> *blanks and v01ksc = *zeros
093200150319     C                   eval      *IN90 = *on
093300150319     C                   eval      *IN23 = *on
093400150319     c                   leavesr
093500150319     c                   endif
093600150319
093700150319      *?Motivo apertura reclamo?
093800150319
093900150319     c                   clear                   VD1mad
094000150319    2c                   If        v01mad <> *blanks
094100150319     c                   clear                   tibs02ds
094200150319      *
094300150319      * Ricerca
094400150319     c     '?'           scan      v01mad
094500150319    3c                   if        %found
094600150319     c                   clear                   tntbMARds1
094700150319     c                   movel(p)  tntbMARds1    KPJBU
094800150319      *
094900150319     c                   call      'TNTBMARR1'
095000150319     c                   parm                    KPJBA
095100150319      *
095200150319     c                   movel     KPJBU         tntbMARds1
095300150319    4c                   IF        oMARfxx = *blank  and  oMARerr = *off
095400150319     c                   eval      V01mad  = oMARcod
095500150319     C                   eval      VD1mad  = oMARdes
095600150319      *  errore (non trovato nulla pulisco il '?' )
095700150319     C                   ELSE
095800150319     c                   clear                   V01mad
095900150319    4C                   ENDIF
096000150319      *
096100150319     c                   eval      *in90 = *on
096200150325     C                   eval      *in55 = *on
096300150319     c                   leavesr
096400150319      *
096500150319   x3C                   ELSE
096600150319      *
096700150319      * Controllo
096800150319     c                   clear                   tibs02ds
096900150319     C                   MOVEL     'C'           t02mod
097000150319     C                   MOVEL     knsif         t02sif
097100150319     C                   MOVEL     'MAR'         t02cod
097200150319     C                   MOVEL     v01mad        t02ke1
097300150319      *
097400150319     C                   CALL      'TIBS02R'
097500150319     C                   PARM                    KPJBA
097600150319     C                   PARM                    TIBS02DS
097700150319      *
097800150319    4C                   IF        t02err = *BLANKS
097900150319     C                   movel     T02uni        DMAR
098000150319     C                   movel     §MARdesc      vd1mad
098100150319      *
098200150319     C                   ELSE
098300150319     c                   eval      *in90 = *on
098400150319     C                   eval      *IN47 = *on
098500150319     c                   leavesr
098600150319    4C                   ENDIF
098700150319      *
098800150319    3c                   endif
098900150319      *
099000150319    2C                   endif
099100150319
099200150319      *?CODICE di CHIUSURA del reclamo?
099300150319
099400150319     c                   clear                   v1dcdt
099500150319    2c                   If        v01cdt <> *blanks
099600150319     c                   clear                   tibs02ds
099700150319      *
099800150319      * Ricerca
099900150319     c     '?'           scan      v01cdt
100000150319    3c                   if        %found
100100150319     c                   clear                   tibs02ds
100200150319     C                   MOVEL     'R'           t02mod
100300150319     C                   MOVEL     knsif         t02sif
100400150319     C                   MOVEL     'CHR'         t02cod
100500150319      *
100600150319     C                   CALL      'TIBS02R'
100700150319     C                   PARM                    KPJBA
100800150319     C                   PARM                    TIBS02DS
100900150319      *
101000150319    4C                   IF        T02err = *BLANKS
101100150319     C                   movel     T02uni        DCHR
101200150319     C                   movel     T02ke1        v01cdt
101300150319     C                   movel     §chrdesc      v1dcdt
101400150319      *  errore (non è stato selezionato nulla pulisco  il campo e mi posiziono)
101500150319     C                   ELSE
101600150319     C                   clear                   v01cdt
101700150319     C                   clear                   v1dcdt
101800150319    4C                   ENDIF
101900150319      *
102000150319     c                   eval      *in90 = *on
102100150319     C                   eval      *IN56 = *on
102200150319     c                   leavesr
102300150319      *
102400150319   x3C                   ELSE
102500150319      *
102600150319      * Controllo
102700150319     c                   clear                   tibs02ds
102800150319     C                   MOVEL     'C'           t02mod
102900150319     C                   MOVEL     knsif         t02sif
103000150319     C                   MOVEL     'CHR'         t02cod
103100150319     C                   MOVEL     v01cdt        t02ke1
103200150319      *
103300150319     C                   CALL      'TIBS02R'
103400150319     C                   PARM                    KPJBA
103500150319     C                   PARM                    TIBS02DS
103600150319      *
103700150319    4C                   IF        t02err = *BLANKS
103800150319     C                   movel     T02uni        DCHR
103900150319     C                   movel     §CHRdesc      v1dcdt
104000150319      *
104100150319     C                   ELSE
104200150319     c                   eval      *in90 = *on
104300150319     C                   eval      *IN48 = *on
104400150319     c                   leavesr
104500150319    4C                   ENDIF
104600150319      *
104700150319    3c                   endif
104800150319      *
104900150319    2C                   endif
105000150319
105100150319      *?se richieste le sole aperte e codice chiusura valorizzato dare errore?
105200150319
105300160120    2c                   If        v1dcdt <> *blanks and (v01ape = 'SI'
105400160120     c                                                or  V01att = 'S')
105500150319     c                   eval      *in90 = *on
105600160120     c                   if        V01ape  = 'SI'
105700150319     C                   eval      *IN65 = *on
105800160120     c                   else
105900160120     c                   eval      *in64 = *on
106000160120     c                   endif
106100150319     c                   leavesr
106200150319    2C                   Endif
106300040331
106400150319      *?Filiale apertura reclamo?
106500040331
106600150319     C                   clear                   vd1pot
106700040331    2c                   If        v01pot <> *blanks
106800040331      *
106900040331      * Ricerca
107000040331     c     '?'           scan      v01pot
107100040331    3c                   if        %found
107200040331     C                   clear                   dstnsd
107300040331     C                   call      'TNSD24R'
107400040331     C                   parm                    §cod
107500040331     C                   parm                    §tip
107600040331     C                   parm                    §des
107700040331     C                   parm                    §pra
107800040331     C                   movel     §cod          v01pot
107900150319     C                   movel     §des          vd1pot
108000040331     c                   eval      *in90 = *on
108100040331     C                   eval      *IN51 = *on
108200150319     c                   leavesr
108300040331    3C                   endif
108400040331    2C                   endif
108500040331      *
108600040331    2C     v01pot        ifeq      *blanks
108700040331     C                   move      *all'0'       v01pot
108800040331    2C                   endif
108900040331      *
109000150626     C                   testn                   v01pot               12
109100040331     C                   move      v01pot        w001a             1
109200040331      *
109300040331     c                   move      v01pot        W_pot
109400040331      *
109500150626    2C     *in12         ifeq      *off
109600040331     C     w001a         orlt      '0'
109700040331     c                   eval      *in90 = *on
109800040331     C                   eval      *IN40 = *on
109900150319     c                   leavesr
110000040331    2C                   endif
110100040331      * verifico se P.O. esistente
110200151023    2c                   if        W_pot > 0
110300151023     c                   IF        w_pot = 999 and simfel = 0
110400151023     c                   eval      *in90 = *on
110500151023     C                   eval      *IN40 = *on
110600151023     c                   leavesr
110700151023    3c                   endif
110800151023     c                   IF        w_pot <> 999
110900160104     C     W_pot         chain     azorg
111000040331    3C                   if        %found(azorg01l)
111100150319     C                   movel     orgdes        vd1pot
111200040331      *  errore
111300040331     C                   ELSE
111400040331     c                   eval      *in90 = *on
111500040331     C                   eval      *IN40 = *on
111600150319     c                   leavesr
111700040331    3c                   endif
111800151023     c                   ENDIF
111900040331      *
112000040331    2c                   endif
112100150319
112200150319      *?se non è stato inserito la Fil. Apertura?
112300150319      *   ?la posso recuperare dall'utente?
112400150319     c                   If        v01uta <> *blanks
112500160104     c     v01uta        chain     Azute000
112600150319     c                   if        not %found(azute01l)
112700150319      *
112800150319     c                   eval      *in90 = *on
112900150319     c                   eval      *in49 = *on
113000150319     c                   leavesr
113100150319     C                   ELSE
113200150319     c                   if         v01pot = *blank  or v01pot = *zeros
113300150319     c                   movel     utefil        v01pot
113400150319     c                   endif
113500150319     c                   endif
113600150319      *
113700150319     c                   endif
113800150319
113900150319      *?Filiale responsabile?
114000150319
114100150319     C                   clear                   vd1por
114200150319    2c                   If        v01por <> *blanks
114300150319      *
114400150319      * Ricerca
114500150319     c     '?'           scan      v01por
114600150319    3c                   if        %found
114700150319     C                   clear                   dstnsd
114800150319     C                   call      'TNSD24R'
114900150319     C                   parm                    §cod
115000150319     C                   parm                    §tip
115100150319     C                   parm                    §des
115200150319     C                   parm                    §pra
115300150319     C                   movel     §cod          v01por
115400150319     C                   movel     §des          vd1por
115500150319     c                   eval      *in90 = *on
115600150319     C                   eval      *IN81 = *on
115700150319     c                   leavesr
115800150319    3C                   endif
115900150319    2C                   endif
116000150319      *
116100150319    2C     v01por        ifeq      *blanks
116200150319     C                   move      *all'0'       v01por
116300150319    2C                   endif
116400150319      *
116500150626     C                   testn                   v01por               12
116600150319     C                   move      v01por        w001a             1
116700150319      *
116800150319     c                   move      v01por        W_por
116900150319      *
117000150626    2C     *in12         ifeq      *off
117100150319     C     w001a         orlt      '0'
117200150319     c                   eval      *in90 = *on
117300150319     C                   eval      *IN76 = *on
117400150319     c                   leavesr
117500150319    2C                   endif
117600150319      * verifico se P.O. esistente
117700151022    2c                   if        W_por > 0
117800160104     C     W_por         chain     azorg
117900150319    3C                   if        %found(azorg01l)
118000150319     C                   movel     orgdes        vd1por
118100150319      *  errore
118200150319     C                   ELSE
118300150319     c                   eval      *in90 = *on
118400150319     C                   eval      *IN76 = *on
118500150319     c                   leavesr
118600150319    3c                   endif
118700150319      * verifico se inserite date chiusura
118800150319     c                   if        wddcr = 0
118900150319     c                   eval      *in90 = *on
119000150319     C                   eval      *IN77 = *on
119100150319     c                   leavesr
119200150319    3c                   endif
119300150319      *
119400150319    2c                   endif
119500150319
119600150319      * -?La selezione delle R.A. aperte da altre Filiali RICHIEDE?
119700150319      *  ?l'inserimento della Fil. Responsabile?
119800150319if  2c                   if        V01aap = 'S'  and  W_por = *zero
119900150319     c                   eval      *in90 = *on
120000150319     C                   eval      *in36 = *on
120100150319     c                   leavesr
120200150319e   2c                   endif
120300150319
120400150319      *?Tipo comunicazione?
120500150319
120600150319     c                   clear                   vd1tco
120700150319    2c                   If        v01tco <> *blank
120800150319      * Ricerca
120900150319     c     '?'           scan      v01tco
121000150319    3c                   if        %found
121100150319     c                   clear                   tibs02ds
121200150319     C                   MOVEL     'R'           t02mod
121300150319     C                   MOVEL     knsif         t02sif
121400150319     C                   MOVEL     'COR'         t02cod
121500150319      *
121600150319     C                   CALL      'TIBS02R'
121700150319     C                   PARM                    KPJBA
121800150319     C                   PARM                    TIBS02DS
121900150319      *
122000150319    4C                   IF        T02err = *BLANKS
122100150319     C                   movel     T02uni        DCOR
122200150319     C                   movel     T02ke1        v01tco
122300150319     C                   movel     §CORdesc      vd1tco
122400150319      *  errore (non ritorna nulla dalla selezione pulisco il campo)
122500150319     C                   ELSE
122600150319     c                   clear                   v01tco
122700150319    4C                   ENDIF
122800150319      *
122900150319     c                   eval      *in90 = *on
123000150319     C                   eval      *IN53 = *on
123100150319     c                   leavesr
123200150319      *
123300150319   x3C                   ELSE
123400150319      *
123500150319      * Controllo
123600150319     c                   clear                   tibs02ds
123700150319     C                   MOVEL     'C'           t02mod
123800150319     C                   MOVEL     knsif         t02sif
123900150319     C                   MOVEL     'COR'         t02cod
124000150319     C                   MOVEL     v01tco        t02ke1
124100150319      *
124200150319     C                   CALL      'TIBS02R'
124300150319     C                   PARM                    KPJBA
124400150319     C                   PARM                    TIBS02DS
124500150319      *
124600150319    4C                   IF        t02err = *BLANKS
124700150319     C                   movel     T02uni        DCOR
124800150319     C                   movel     T02ke1        v01tco
124900150319     C                   movel     §CORdesc      vd1tco
125000150319      *
125100150319     C                   ELSE
125200150319     c                   eval      *in90 = *on
125300150319     C                   eval      *IN45 = *on
125400150319     c                   leavesr
125500150319    4C                   ENDIF
125600150319      *
125700150319    3c                   endif
125800150319      *
125900150319    2C                   endif
126000040225
126100150319      *?Numero reclamo?
126200040225
126300040225    2c                   If        v01nut > 0
126400040421      * verifico se inserito anno reclamo
126500040421     c                   If        v01ant = 0
126600040421     c                   seton                                        3890
126700150319     c                   leavesr
126800050112     C                   else
126900050112      *
127000050112      *   sistemo anno di due cifre
127100050112     C                   if        v01ant < 100
127200050112     C                   ADD       2000          v01ant
127300050112     C                   endif
127400050112      *
127500040421     c                   endif
127600040225      * verifico se esiste numero reclamo
127700160104     c     Ktgdan        chain     fitgd000
127800050112     c                   if        not %found(fitgd01l)
127900040225     c                   seton                                        3990
128000150319     c                   leavesr
128100040225     c                   endif
128200040305
128300040225    2c                   endif
128400150319
128500150319      *?Tipo oggetto?
128600040527
128700040527     c                   clear                   v01tor
128800040527     c                   clear                   W_ogg
128900040527
129000150319      *?OGGETTO?
129100150319
129200040527    2c                   if        v01ogs = 'X'
129300040527     c                   eval      v01tor = 'S'
129400040527     c                   endif
129500040527
129600150319
129700150319      * -?Spedizione?
129800150626     c                   if        V01ogs = *blanks  and  SpeVid <> *zeros
129900150626     c                   eval      *in90 = *on
130000150626     c                   eval      *in78 = *on
130100150626     c                   leavesr
130200150626     c                   endif
130300040527    2c                   if        v01ogs = 'X' and spevid <> *zeros
130400040527
130500040527      * se non è stato inserito nulla non controllo
130600040527
130700040527      * verifico se inserito linea partenza
130800040527    3c                   if        v01lnp = *zeros
130900040527     c                   eval      *in90 = *on
131000040527     c                   eval      *in30 = *on
131100150319     c                   leavesr
131200040527    3c                   endif
131300040527      * verifico se inserito numero spedizione
131400040527    3c                   if        v01nsp = *zeros
131500040527     c                   eval      *in90 = *on
131600040527     c                   eval      *in31 = *on
131700150319     c                   leavesr
131800040527    3c                   endif
131900040527      * verifico se inserito anno e se è di 2
132000040527    3c                   if        v01ans = *zeros
132100040527     c                   eval      *in90 = *on
132200040527     c                   eval      *in33 = *on
132300150319     c                   leavesr
132400040527    3c                   endif
132500040527      *
132600040527    3c                   If        v01ans > 60 and v01ans < 100
132700040527     c                   add       1900          V01ans
132800040527    3c                   endif
132900040527
133000040527    3c                   If        v01ans <=  60 and v01ans < 100
133100040527     c                   add       2000          V01ans
133200040527    3c                   endif
133300040527      * preparo i campi della chiave
133400040527     c                   clear                   W_spevid
133500040527     c                   eval      W_spevid = spevid
133600040527     c                   movel(p)  W_spevid      W_ogg
133700040527      *
133800040527    2c                   endif
133900040527
134000040527    2c                   if        v01ogo = 'X' and v01tor <> ' '
134100040527     c                   eval      *in90 = *on
134200040527     c                   eval      *in54 = *on
134300150319     c                   leavesr
134400040527     c                   endif
134500040527
134600040527     c                   If        v01ogo  = 'X'
134700040527     c                   eval      v01tor = 'O'
134800040527     c                   endif
134900040527
135000150319
135100150319      * -?ORM?
135200150626     c                   if        V01ogo = *blanks  and  OrmVid <> *zeros
135300150626     c                   eval      *in90 = *on
135400150626     c                   eval      *in79 = *on
135500150626     c                   leavesr
135600150626     c                   endif
135700040527    2c                   if        v01ogo = 'X' and ormvid <> *zeros
135800040527
135900040527      * se non è stato inserito nulla non controllo
136000040527
136100040527      * verifico se inserito P.O. emissione
136200040527     c                   if        v01poe = *zeros
136300040527     c                   eval      *in90 = *on
136400040527     c                   eval      *in34 = *on
136500150319     c                   leavesr
136600040527     c                   endif
136700040527      * verifico se inserito numero ORM
136800040527     c                   if        v01nor = *zeros
136900040527     c                   eval      *in90 = *on
137000040527     c                   eval      *in35 = *on
137100150319     c                   leavesr
137200040527     c                   endif
137300040527      * preparo i campi della chiave del file
137400040527     c                   clear                   W_ormvid
137500040527     c                   eval      W_ormvid = ormvid
137600040527     c                   movel(p)  W_ormvid      W_ogg
137700040527      *
137800040527    2c                   endif
137900040527      *
138000040527
138100040527    2c                   if        v01ogg = 'X' and v01tor <> ' '
138200040527     c                   eval      *in90 = *on
138300040527     c                   eval      *in54 = *on
138400150319     c                   leavesr
138500040527     c                   endif
138600040527
138700040527     c                   If        v01ogg  = 'X'
138800040527     c                   eval      v01tor = 'G'
138900040527     c                   endif
139000040527
139100150319      * -?GENERICO?
139200150626     c                   if        V01ogg = *blanks  and  V01gen <> *blanks
139300150626     c                   eval      *in90 = *on
139400150626     c                   eval      *in80 = *on
139500150626     c                   leavesr
139600150626     c                   endif
139700040527     c                   If        v01ogg = 'X' and v01GEN <> *blanks
139800040527     c                   movel(p)  v01gen        W_ogg
139900040527     c                   endif
140000040603      *
140100150319      *?se non sono stati richiesti i solo reclami aperti?
140200150319      *   ?sono obbligatorie le date?
140300150319      *?SE NON È STATO INSERITO IL SOLO NUMERO RECLAMO?
140400150319      *?SE NON È STATO INSERITO L'OGGETTO?
140500040603      *
140600040603    2c                   If        v01ape <> 'SI' and v01nut = 0 and
140700040603     c                              W_ogg = *blank
140800040603     c                             and wadar = 0 and wddar = 0
140900040603     c                             and wadcr = 0 and wddcr = 0
141000040603     c                   eval      *in90 = *on
141100040603     C                   eval      *IN57 = *on
141200150319     c                   leavesr
141300040603    2c                   endif
141400050114
141500040227
141600040227      * verifico se inserita almeno una selezione
141700040225
141800040331     c                   If        v01nut = 0 and
141900040331     c                             vd1dar = 0 and
142000040331     c                             va1dar = 0 and
142100040603     c                             vd1dcr = 0 and
142200040603     c                             va1dcr = 0 and
142300040302     c                             (v01pot = *blanks or V01pot  = *zeros) and
142400040401     c                             v01uta = *blank  and
142500040603     c                             (v01por = *blanks or V01por  = *zeros) and
142600040331     c                             v01mad = *blank and
142700040331     c                             v01prt = *blank and
142800040331     c                             v01cdt = *blank and
142900040331     c                             v01ape = *blank and
143000040331     c                             v01rpg = *blank and
143100040331     c                             v01tco = *blank and
143200040331     c                             v01rsc = *blank and
143300040227     c                             v01tor = *blank
143400040227     c                   eval      *in90 = *on
143500040227     c                   eval      *in60 = *on
143600150319     c                   leavesr
143700040227     c                   endif
143800040225
143900040225     c                   Endsr
144000040226      *---------------------------------------------------------------*
144100040226      * Routine Gestione SUBFILE                                      *
144200040226      *---------------------------------------------------------------*
144300040226     c     SR_GESSBF     BegSR
144400040226
144500040226      * richiamo la routine di caricamento del subfile
144600150506     c                   eval      $InzSFL = *off
144700150506     c                   exsr      SR_CARSBF
144800150506      *
144900140926      /free
145000140926       //?Abilito F19=Disposizioni di Consegna
145100140926         exsr AbiF19;
145200140926      /end-free
145300140926
145400040227      * se è stato caricato almeno un record nel subfile emetto la videta
145500040227     c                   dow       *in90 = *off
145600150506      *
145700150506     c                   if        $InzSFL = *on
145800150506     c                   exsr      sr_CarSBF
145900150506     c                   eval      $InzSFL = *off
146000150616      * -?Se 90 acceso (errore/nessun rec.) esco dal ciclo di emissione Sfl?
146100150616     c                   If        *in90
146200150616     c                   leave
146300150616     c                   EndIf
146400150506     c                   endif
146500150603     c                   eval      V1Togg = W1Togg
146600040227      *
146700140219     c                   write     FIA2T01
146800040227     c                   write     fia2z02
146900140221      *
147000150507      * -?Posizionamento cursore nel subfile?
147100150507     c                   if        V2CrtnCsr > *zero
147200150507     c                   z-add     V2CrtnCsr     V2Cnrr
147300150507     c                   endif
147400150507      *
147500140526     c                   IF        wMod = 'R'  and  (iA2tor = 'S'
147600140526     c                                          or   iA2tor = 'O')
147700140221     c                   exfmt     FIA2C03
147800140221     c                   ELSE
147900040227     c                   exfmt     fia2c02
148000140220     c                   ENDIF
148100150506      *
148200150506     c                   Select
148300150506      *
148400040227      * f12 = Ritorno
148500150506     c                   When      *inkl
148600040227     c                   leave
148700150506      *
148800150506      * -?F5 = Refresh (da C02/S02)?
148900150506     c                   When      *inKE
149000150506     c                   eval      $InzSFL = *on
149100150506     c                   iter
149200150506      *
149300140220      * F11 = Ordinamento DI/Ascendente x R.A.
149400150506     c                   When      *inKK  and  wMod = 'R'
149500140221     c                   if        Not  $Ord_Desc
149600140221     c                   eval      $Ord_Desc = *on
149700140221     c                   eval      Z02F11    = 'F11=Ord.Ascendente '
149800140221     c                   else
149900140221     c                   eval      $Ord_Desc = *off
150000140221     c                   eval      Z02F11    = 'F11=Ord.Discendente'
150100140221     c                   endif
150200140220     c                   exsr      sr_OrdinamS03
150300140220     c                   iter
150400140926
150500140926      /free
150600150506         //?F19=Disposizioni di Consegna
150700150506           When  *inkt;
150800150506             exsr sr_F19;
150900140926      /end-free
151000150506
151100150506     c                   EndSl
151200140926
151300040227      * Lettura subfile
151400040227     c                   Exsr      SR_readsf
151500040317      * se 90 accxeso errore riemetto la videata
151600040317     c                   If        *in90
151700040317     c                   setoff                                       90
151800040317     c                   iter
151900040317     c                   endif
152000040305      * se è stato dato f3 dai programmi chiamati ho KL acceso e vado a fine
152100040315     c                   if        w_f03 = 'S'
152200040305     c                   leave
152300040305     c                   endif
152400040227
152500040227     c                   enddo
152600040227      *
152700040226     c                   Endsr
152800040226      *---------------------------------------------------------------*
152900040226      * Routine Caricamento SUBFILE                                   *
153000040226      *---------------------------------------------------------------*
153100040226     c     SR_CARSBF     BegSR
153200040226
153300040226      * pulizia del subfile
153400040226
153500040316     c                   setoff                                       2021
153600040226     c                   seton                                        22
153700140526     c                   if        wMod = 'R'  and  (iA2tor = 'S'
153800140526     c                                          or   iA2tor = 'O')
153900140219     c                   write     FIA2C03
154000140219     c                   else
154100040226     c                   write     fia2c02
154200140219     c                   endif
154300040226     c                   setoff                                       22
154400040227
154500140219     c                   clear                   V1Togg
154600150603     c                   clear                   W1Togg
154700140219
154800040227     c                   z-add     0             s01nrr
154900140219     c                   z-add     0             S03nrr
155000040227
155100040227     c                   SELECT
155200040227
155300150320      * se sono stato richiamato leggo per oggetto -------------------*
155400040226
155500040227     c                   When      wmod = 'R'
155600140526     c                   select
155700140526     c                   when      iA2tor = 'S'
155800140219     c                   movel     iA2ogg        WRKspe
155900150603     c                   eval      W1Togg = 'Sped. ' + %trim(WRKlnp) +  '/' +
156000140526     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
156100140219     c                             ' ' + WRKann
156200140526     c                   when      iA2tor = 'O'
156300140526     c                   movel     iA2ogg        WRKorm
156400150603     c                   eval      W1Togg = 'O.R.M. ' + %trim(WRKpoe) +  '/' +
156500140526     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
156600140526     c                             '/' + WRKnrv
156700140526     c                   endsl
156800150603     c                   if        W1Togg <> *blank
156900150603     c                   eval      ww = ( ( %size(W1Togg) -
157000150603     c                                      %len( %trim(W1Togg) ) )
157100140526     c                                    / 2 )
157200140526     c                   if        ww > *zero
157300150603     c                   eval      %subst( W1Togg : ww + 1 ) = W1Togg
157400150603     c                   eval      %subst( W1Togg : 1 : ww ) = *blanks
157500140526     c                   endif
157600140526     c                   endif
157700160104     c     ktgd02        setll     fitgd02
157800040226      *
157900040226     c                   do        *hival
158000160104     c     ktgd02        reade     fitgd02
158100040226
158200150324     c                   if        %eof(fitgd32l)
158300040226     c                   leave
158400040226     c                   endif
158500040226
158600040227      * salto i controlli del record e imposto record OK
158700040227
158800040227     c                   eval      Wrecok = 'Y'
158900040227
159000040226     c                   exsr      SR_wrtsbf
159100041018     c   28              leave
159200040226
159300040226     c                   enddo
159400150319
159500150428      /free
159600150420
159700150420         // -?Selezioni a video?
159800150420         Other;
159900150420           // -?Lettura FITGD00F e Scrittura record nel subfile S02?
160000150420           exsr  sr_ReadFITGD;
160100150420
160200150420      /end-free
160300150420
160400150420      *---------------------------------------------------------------*
160500150420      *
160600150420     c                   ENDSL
160700040227
160800150506     c                   eval      *in09 = ( wMod <> 'R' )
160900140221     c                   clear                   Z02F11
161000140224     c                   eval      *in08 = *off
161100140221
161200040304      * se non è stata caricata neppure una riga di subfile do errore
161300150319sel 1c                   Select
161400040227
161500150319w   1c                   When      s01nrr = 0  and  S03nrr = 0
161600040227     c                   eval      *in90 = *on
161700040227     c                   eval      *in61 = *on
161800040227      *
161900150319x   1c                   Other
162000040227      *
162100040227      * accendo indicatori di visualizzazione SUB_file
162200040227     c                   seton                                        2120
162300040317     c                   z-add     1             v2cnrr
162400150508     c                   eval      V2CrtnCsr = V2Cnrr
162500150420sel 2c                   Select
162600150420      *
162700150420w   2c                   When      wMod = 'R'
162800140220     c                   eval      RrnLast = S03nrr
162900140224     c                   eval      *in08  = (S03nrr > 1)
163000150319if  3c                   if        S03nrr > 1
163100140220     c                   exsr      sr_OrdinamS03
163200150319e   3c                   endif
163300150420      *
163400150428     c                   Other
163500150420     c                   eval      RrnLast = S01nrr
163600150420if  3c                   if        S01nrr > 1
163700150420     c                   exsr      sr_OrdinamS02
163800150420e   3c                   endif
163900150420      *
164000150420e   2c                   EndSl
164100150319
164200150319e   1c                   EndSl
164300040227
164400040227     c                   endsr
164500150417
164600150417      /free
164700150417
164800150417       //--------------------------------------------------------------
164900150417       //?Lettura file FITGD00F via SQL                                ?
165000150417       //--------------------------------------------------------------
165100150420       BEGSR  sr_ReadFITGD;
165200150417
165300150417         // -?Preparazione SQL da eseguire?
165400150417         exsr  sr_PrepSQL;
165500150417         exec sql   prepare S1   from :wSQL;
165600150417
165700150417         // -?Dichiarazione / Apertura del cursore?
165800150417         exec sql   declare C1   cursor for S1;
165900150417         if  SQLcode < *zero;
166000150417           *in28 = *on;
166100150417           leavesr;
166200150417         endif;
166300150417         exec sql   open C1;
166400150417         if  SQLcode < *zero;
166500150417           *in28 = *on;
166600150417           leavesr;
166700150417         endif;
166800150417
166900150417         // -?Ciclo di Lettura file / Scrittura rec. nel subfile?
167000160129         *in28 = *off;
167100150417         DoU  SQLcode < *zero  or  SQLcode = 100;
167200150417
167300150417           clear  FITGD_ds;
167400150417           exec sql   fetch next   from C1   into :FITGD_ds;
167500150417
167600150417           Select;
167700150417             // -?Fine File?
167800150417             When  SQLcode = 100;
167900150417               leave;
168000150417             // -?Errore SQL?
168100150417             When  SQLcode < *zero;
168200150417               *in28 = *on;
168300150417               leave;
168400150417             // -?Elaborazione singolo record?
168500150417             Other;
168600150417               eval  Wrecok = 'Y';
168700150417               exsr  SR_wrtsbf;
168800150417               if  *in28;
168900150417                 leave;
169000150417               endif;
169100150417           EndSl;
169200150417
169300150417         EndDo;
169400150417
169500150417         // -?Chiusura del cursore?
169600150417         exec sql   close C1;
169700150417
169800150417       ENDSR;
169900150417
170000150417       //--------------------------------------------------------------
170100150417       //?Preparazione stringa SQL da eseguire per la lettura.         ?
170200150417       //--------------------------------------------------------------
170300150417       BEGSR  sr_PrepSQL;
170400150417
170500150417         $First = *on;
170600150417
170700150417         wSQL  = 'select * +
170800150417                    from FITGD00F';
170900150626
171000150626         If  V01nut <> *zero  or
171100150626             W_ogg <> *blank;
171200150626           exsr  sr_PrepSQL_RA_Ogg;
171300150626         Else;
171400150626           exsr  sr_PrepSQL_Selez;
171500150626         EndIf;
171600150626
171700150626         wSQL += ' for fetch only';
171800150626
171900150626       ENDSR;
172000150626
172100150626       //--------------------------------------------------------------
172200150626       //?Parzializzaz. SQL NON per singola R.A. e/o singolo Oggetto.  ?
172300150626       //--------------------------------------------------------------
172400150626       BEGSR  sr_PrepSQL_Selez;
172500151221
172600151221         // -?Selezione R.A. in ATTESA DI RISPOSTA?
172700151221         If  V01att = 'S';
172800151221           wSQL += ' left outer join FITGR00F on +
172900151221                     TGDant = TGRant and +
173000151221                     TGDnut = TGRnut and +
173100151221                     TGDpdt = TGRpdt and +
173200151221                     TGRtri = ''A''';
173300151221         EndIf;
173400160120
173500160120         //- - - - - - - - - - - - - - - - - - - - - - - - - - - -
173600150417
173700150417         // -?Selezione Filiale Gestione?
173800160120         IF  V01pog <> *zero;
173900160120
174000160120           select;
174100160120             when  $First  and  V01att = 'S';
174200160120               wSQL  += ' where ((';
174300160120               $First = *off;
174400160120             when  $First  and  V01att = *blank;
174500160120               wSQL  += ' where (';
174600160120               $First = *off;
174700160120             other;
174800160120               wSQL  += ' and ';
174900160120           endsl;
175000160120
175100160120           If  V01pog <> '999';
175200160120             wSQL += 'TGDpog = ' + %trim( V01pog );
175300160120           Else;
175400160120             wSQL += 'TGDpog in (';
175500151022             yy = 0;
175600151022             xx = 1;
175700160120             For xx  By 1  To %elem(sk_pog2);
175800160120               If sk_pog2(xx) <> *zeros;
175900160120                 if yy > 0;
176000151022                   wSQL += ', ';
176100160120                 else;
176200151022                   yy = 1;
176300160120                 endif;
176400151022                 wSQL += %editc(sk_pog2(xx):'X');
176500160120               EndIf;
176600160120             EndFor;
176700151022             wSQL += ')';
176800160120           EndIf;
176900160120
177000160120         ENDIF;
177100150417
177200150420         // -?Selezione Reparto Gestione o Utente Gestione?
177300160120         IF  V01rpr <> *blank  or  V01rpg <> *blank;
177400160120
177500160120           select;
177600160120             when  $First  and  V01att = 'S';
177700160120               wSQL  += ' where ((';
177800160120               $First = *off;
177900160120             when  $First  and  V01att = *blank;
178000160120               wSQL  += ' where (';
178100160120               $First = *off;
178200160120             other;
178300160120               wSQL  += ' and ';
178400160120           endsl;
178500160120
178600150420           wSQL += '(';
178700160120
178800150420           if  V01rpr <> *blank;
178900150420             wSQL += 'TGDrpr = ''' + V01rpr + '''';
179000150420           endif;
179100150420           if  V01rpr <> *blank  and  V01rpg <> *blank;
179200150420             wSQL += ' or ';
179300150420           endif;
179400160120
179500150420           if  V01rpg <> *blank;
179600150423             if  W_rpgGen = 'Y';
179700150423               wSQL += 'TGDrpg like ''' + %trimR( w_RPG ) + '%''';
179800150423             else;
179900150423               wSQL += 'TGDrpg = ''' + V01rpg + '''';
180000150423             endif;
180100150420           endif;
180200160120
180300150420           wSQL += ')';
180400160120
180500160120         ENDIF;
180600160120
180700160120         //- - - - - - - - - - - - - - - - - - - - - - - - - - - -
180800160120
180900160120         // -?Selezione R.A. in ATTESA DI RISPOSTA?
181000160120         IF  V01att = 'S';
181100160120
181200160120           if  $First;
181300160120             wSQL  += ' where (';
181400160120             $First = *off;
181500160120           else;
181600160120             wSQL  += ') OR (';
181700160120           endif;
181800160120
181900160129           clear  $First2;
182000160129
182100160120           If  V01pog <> *zero;
182200160129             if  Not $First2;
182300160129               $First2 = *on;
182400160129             else;
182500160129               wSQL   += ' and ';
182600160129             endif;
182700160120             If  V01pog <> '999';
182800160120               wSQL += 'TGDpog <> ' + %trim( V01pog );
182900160120             Else;
183000160120               wSQL += 'TGDpog Not in (';
183100160120               yy = 0;
183200160120               xx = 1;
183300160120               For xx  By 1  To %elem(sk_pog2);
183400160120                 If sk_pog2(xx) <> *zeros;
183500160120                   if yy > 0;
183600160120                     wSQL += ', ';
183700160120                   else;
183800160120                     yy = 1;
183900160120                   endif;
184000160120                   wSQL += %editc(sk_pog2(xx):'X');
184100160120                 EndIf;
184200160120               EndFor;
184300160120               wSQL += ')';
184400160120             EndIf;
184500160120           EndIf;
184600160120
184700160120           If  V01ape <> 'SI';
184800160129             if  Not $First2;
184900160129               $First2 = *on;
185000160129             else;
185100160129               wSQL   += ' and ';
185200160129             endif;
185300160129             wSQL  += 'TGDcdt = '' ''';
185400160120           EndIf;
185500160120
185600160120           If  V01pog <> *zero;
185700160129             if  Not $First2;
185800160129               $First2 = *on;
185900160129             else;
186000160129               wSQL   += ' and ';
186100160129             endif;
186200160120             If  V01pog <> '999';
186300160129               wSQL += 'substr(TGDflo, 2, 3) = ''' +
186400160120                       %trim( V01pog ) + '''';
186500160120             Else;
186600160129               wSQL += 'substr(TGDflo, 2, 3) in (''';
186700160120               yy = 0;
186800160120               xx = 1;
186900160120               For xx by 1 to %elem(sk_POg2);
187000160120                 if sk_POg2(xx) <> *zero;
187100160120                   if yy > 0;
187200160120                     wSQL += ''', ''';
187300160120                   else;
187400160120                     yy = 1;
187500160120                   endif;
187600160120                   wSQL += %editc( sk_POg2(xx) : 'X' );
187700160120                 endif;
187800160120               EndFor;
187900160120               wSQL += ''')';
188000160120             EndIf;
188100160120           EndIf;
188200160120
188300160129           if  Not $First2;
188400160129             $First2 = *on;
188500160129           else;
188600160129             wSQL   += ' and ';
188700160129           endif;
188800160129           wSQL += 'substr(TGDfar, 1, 1) = ''S''';
188900160120
189000160120           if  V01rpr <> *blank;
189100160129             if  Not $First2;
189200160129               $First2 = *on;
189300160129             else;
189400160129               wSQL   += ' and ';
189500160129             endif;
189600160129             wSQL += 'substr(TGRrif, 1, 10) = ''' +
189700160120                     V01rpr + '''';
189800160120           endif;
189900160120
190000160120         ENDIF;
190100160120
190200160120         if  $First = *off;
190300160120           if  V01att = 'S';
190400160120             wSQL  += '))';
190500160120           else;
190600160120             wSQL  += ')';
190700160120           endif;
190800160120         endif;
190900160120
191000160120         //- - - - - - - - - - - - - - - - - - - - - - - - - - - -
191100150417
191200150417         // -?Selezione SOLO R.A. Aperte?
191300150417         If  V01ape = 'SI';
191400150417           if  $First;
191500160120             wSQL  += ' where ';
191600150417             $First = *off;
191700150417           else;
191800150417             wSQL  += ' and ';
191900150417           endif;
192000150417           wSQL += 'TGDcdt = '' ''';
192100150417         EndIf;
192200150417
192300150417         // -?Selezione Stato (ex Priorità)?
192400150417         If  V01prt <> *blank;
192500150417           if  $First;
192600160120             wSQL  += ' where ';
192700150417             $First = *off;
192800150417           else;
192900150417             wSQL  += ' and ';
193000150417           endif;
193100150417           wSQL += 'TGDprt = ''' + %trim( V01prt ) + '''';
193200150417         EndIf;
193300150417
193400150417         // -?Parzializzazione per Data Richiesta?
193500150417         If  Wddar  <> *zero;
193600150417           if  $First;
193700160120             wSQL  += ' where ';
193800150417             $First = *off;
193900150417           else;
194000150417             wSQL  += ' and ';
194100150417           endif;
194200150417           wSQL += 'TGDdar >= ' + %trim( %editc( Wddar : '3' ) );
194300150417         EndIf;
194400150417         If  Wadar  <> *zero;
194500150417           if  $First;
194600160120             wSQL  += ' where ';
194700150417             $First = *off;
194800150417           else;
194900150417             wSQL  += ' and ';
195000150417           endif;
195100150417           wSQL += 'TGDdar <= ' + %trim( %editc( Wadar : '3' ) );
195200150417         EndIf;
195300150417
195400150417         // -?Parzializzazione per Data Chiusura?
195500150417         If  Wddcr  <> *zero;
195600150417           if  $First;
195700160120             wSQL  += ' where ';
195800150417             $First = *off;
195900150417           else;
196000150417             wSQL  += ' and ';
196100150417           endif;
196200150417           wSQL += 'TGDdcd >= ' + %trim( %editc( Wddcr : '3' ) );
196300150417         EndIf;
196400150417         If  Wadcr  <> *zero;
196500150417           if  $First;
196600160120             wSQL  += ' where ';
196700150417             $First = *off;
196800150417           else;
196900150417             wSQL  += ' and ';
197000150417           endif;
197100150417           wSQL += 'TGDdcd <= ' + %trim( %editc( Wadcr : '3' ) );
197200150417         EndIf;
197300150417
197400150417         // -?Selezione Ragione Sociale Chiamante?
197500150420         If  V01rsc <> *blank;
197600150420           If  V01rpc = *blank;
197700150420             if  $First;
197800160120               wSQL  += ' where ';
197900150420               $First = *off;
198000150420             else;
198100150420               wSQL  += ' and ';
198200150420             endif;
198300150420             wSQL += 'TGDrsc like ''' + %trimR( V01rsc ) + '%''';
198400150420           Else;
198500150420             if  $First;
198600160120               wSQL  += ' where ';
198700150420               $First = *off;
198800150420             else;
198900150420               wSQL  += ' and ';
199000150420             endif;
199100150420             wSQL += 'TGDrsc like ''%' + %trimR( V01rsc ) + '%''';
199200150420           EndIf;
199300150417         EndIf;
199400150417
199500150417         // -?Selezione Codice Chiamante?
199600150417         If  V01ksc <> *zero;
199700150417           if  $First;
199800160120             wSQL  += ' where ';
199900150417             $First = *off;
200000150417           else;
200100150417             wSQL  += ' and ';
200200150417           endif;
200300150417           wSQL += 'TGDksc = ' + %trim( %editc( V01ksc : '3' ) );
200400150417         EndIf;
200500150417
200600150417         // -?Selezione Motivo Apertura?
200700150417         If  V01mad <> *blank;
200800150417           if  $First;
200900160120             wSQL  += ' where ';
201000150417             $First = *off;
201100150417           else;
201200150417             wSQL  += ' and ';
201300150417           endif;
201400150420           wSQL += 'TGDmad = ''' + V01mad + '''';
201500150417         EndIf;
201600150417
201700150417         // -?Selezione Codice Chiusura?
201800150417         If  V01cdt <> *blank;
201900150417           if  $First;
202000160120             wSQL  += ' where ';
202100150417             $First = *off;
202200150417           else;
202300150417             wSQL  += ' and ';
202400150417           endif;
202500150420           wSQL += 'TGDcdt = ''' + V01cdt + '''';
202600150417         EndIf;
202700150417
202800150417         // -?Selezione Filiale Apertura?
202900150417         If  W_pot <> *zero;
203000150417           if  $First;
203100160120             wSQL  += ' where ';
203200150417             $First = *off;
203300150417           else;
203400150417             wSQL  += ' and ';
203500150417           endif;
203600151022           IF  w_pot <> 999;
203700150417           wSQL += 'TGDpot = ' + %trim( %editc( W_pot : '3' ) );
203800151022           ELSE;
203900160119             wSQL += 'TGDpot in (';
204000151022             yy = 0;
204100151022             xx = 1;
204200151022             FOR xx by 1 to %elem(sk_pog2);
204300151022               IF sk_pog2(xx) <> *zeros;
204400151022                 IF yy > 0;
204500151022                   wSQL += ', ';
204600151022                 ELSE;
204700151022                   yy = 1;
204800151022                 ENDIF;
204900151022                 wSQL += %editc(sk_pog2(xx):'X');
205000151022               ENDIF;
205100151022             ENDFOR;
205200151022             wSQL += ')';
205300151022           ENDIF;
205400150417         EndIf;
205500150417
205600150417         // -?Selezione Utente Apertura?
205700150417         If  V01uta <> *blank;
205800150417           if  $First;
205900160120             wSQL  += ' where ';
206000150417             $First = *off;
206100150417           else;
206200150417             wSQL  += ' and ';
206300150417           endif;
206400150420           wSQL += 'TGDpru = ''' + V01uta + '''';
206500150417         EndIf;
206600150417
206700150417         // -?Selezione Filiale Responsabile?
206800150417         If  W_por <> *zero;
206900150417           if  $First;
207000160120             wSQL  += ' where ';
207100151221             $First = *off;
207200151221           else;
207300151221             wSQL  += ' and ';
207400151221           endif;
207500151221           wSQL += 'TGDpor = ' + %trim( %editc( W_por : '3' ) );
207600151221         EndIf;
207700151221
207800160104         // -?Selezione Aperte da Altre Filiali?
207900150420         //  ?(diverse dalla Filiale Responsabile)?
208000150417         If  W_por <> *zero  and  V01aap <> *blank;
208100150417           if  $First;
208200160120             wSQL  += ' where ';
208300150417             $First = *off;
208400150417           else;
208500150417             wSQL  += ' and ';
208600150417           endif;
208700150417           wSQL += 'TGDpot <> ' + %trim( %editc( W_por : '3' ) );
208800150417         EndIf;
208900150417
209000150417         // -?Selezione Tipo Comunicazione?
209100150417         If  V01tco <> *blank;
209200150417           if  $First;
209300160120             wSQL  += ' where ';
209400150417             $First = *off;
209500150417           else;
209600150417             wSQL  += ' and ';
209700150417           endif;
209800150417           wSQL += 'TGDtco = ''' + V01tco + '''';
209900150417         EndIf;
210000150626
210100150626         // -?Selezione Tipo Oggetto?
210200150626         If  V01tor <> *blank;
210300150626           if  $First;
210400160120             wSQL  += ' where ';
210500150626             $First = *off;
210600150626           else;
210700150626             wSQL  += ' and ';
210800150626           endif;
210900150626           wSQL += 'TGDtor = ''' + V01tor + '''';
211000150626         EndIf;
211100150626
211200150626       ENDSR;
211300150626       //--------------------------------------------------------------
211400150626       //?Parzializzaz. SQL per singola R.A. e/o singolo Oggetto.      ?
211500150626       //--------------------------------------------------------------
211600150626       BEGSR  sr_PrepSQL_RA_Ogg;
211700150519
211800150519         // -?Selezione Richiesta Assistenza?
211900150519         If  V01nut <> *zero;
212000150519           if  $First;
212100150519             wSQL  += ' where ';
212200150519             $First = *off;
212300150519           else;
212400150519             wSQL  += ' and ';
212500150519           endif;
212600150519           wSQL += 'TGDant = ' + %trim( %editc( V01ant : '3' ) ) + ' and +
212700150519                    TGDnut = ' + %trim( %editc( V01nut : '3' ) );
212800150519         EndIf;
212900150417
213000150417         // -?Selezione Oggetto?
213100150417         If  V01tor <> *blank;
213200150417           if  $First;
213300150417             wSQL  += ' where ';
213400150417             $First = *off;
213500150417           else;
213600150417             wSQL  += ' and ';
213700150417           endif;
213800150417           wSQL += 'TGDtor = ''' + V01tor + '''';
213900150417         EndIf;
214000150417         If  W_ogg <> *blank;
214100150417           if  $First;
214200150417             wSQL  += ' where ';
214300150417             $First = *off;
214400150417           else;
214500150417             wSQL  += ' and ';
214600150417           endif;
214700150417           wSQL += 'TGDogg = ''' + W_ogg + '''';
214800150417         EndIf;
214900150417
215000150417       ENDSR;
215100150417
215200150417      /end-free
215300150417
215400040227      *---------------------------------------------------------------*
215500040227      * Routine Scrittura records nel subfile                         *
215600040227      *---------------------------------------------------------------*
215700150319     c     SR_wrtsbf     BegSR
215800160129      *
215900140221      * se record NON giusto NON scrivo
216000140219     c                   if        Wrecok <> 'Y'
216100140219     c                   leavesr
216200140219     c                   endif
216300040227      *
216400140526     c                   if        wMod = 'R'  and  (iA2tor = 'S'
216500140526     c                                          or   iA2tor = 'O')
216600140219     c                   add       1             S03nrr
216700140219     c                   else
216800040227     c                   add       1             s01nrr
216900140219     c                   endif
217000151221
217100151221     c                   eval      dTGDfar = TGDfar
217200151221     c                   eval      dTGD01  = TGDflo
217300040227
217400040227     c                   clear                   vs1opz
217500040421     c                   z-add     tgdant        vs1ant                         Anno   reclamo
217600040304     c                   z-add     tgdnut        vs1nut                         Numero reclamo
217700040304     c                   z-add     tgdpdt        vs1pdt                         progressivo riga
217800041012      * verifico se Reclamo manutenzionabile  solo se non sono un utente autorizzato (07 *on)
217900040615      * p.o. gestione
218000150317     c                   z-add     TGDpog        VS1pog                         P.O. gestione
218100150910     c*//*               If        tgdpog <> dutpou and not *in07
218200150910     c                   If        Not *in07  and
218300150910     c                             %lookup( TGDpog : sk_POg2 ) = *zero
218400040317     c                   eval      vs1ges = 'N'
218500040317     c                   else
218600040317     c                   clear                   vs1ges
218700040317     c                   endif
218800040317      *
218900040227     c                   z-add     tgdpot        vs1pot                         P.O. apertura
219000150325     c*//                eval      vs1nrr  = (%editc(tgdnut:'Z') + '/' +        Numnero reclamo
219100150325     c*//                          %editc(tgdpdt:'Z'))                          progres. riga
219200040227      *
219300150417     c                   z-add     tgdDAC        HS1dac
219400140423     c                   z-add     tgdDAC        data8
219500040227     c                   move      anno2         vs1dar
219600040227     c                   move      mese          mmgg              4 0
219700040227     c                   movel     mmgg          vs1dar
219800040227     c                   movel     giorno        vs1dar                         Data apertura
219900150325      *
220000150325     c                   eval      VS1orc = (tgdORC / 100)
220100040227      *
220200150417     c                   movel     TGDrpr        VS1rpr                         reparto gestione
220300040402     c                   movel     tgdrpg        vs1rpg                         utente gestione
220400040227     c                   movel     tgdrsc        vs1rsc                         cliente
220500140403     c                   movel     tgdrsc        vs1rsc2                        cliente
220600040227      * chi chiama rispetto all'oggetto
220700040304     C                   movel     tgdtch        vs1tch                         chi chiama
220800150324      * stato (ex priorità)
220900150324     C                   movel     tgdprt        vs1prt                         stato
221000151221     c                   movel     TGDprt        HS1prt2                        Stato
221100151221     c                   if        dTGDfar.§TGDatt = 'S'  and
221200160204     c                             dTGDfar.§TGDris = ' '  and
221300160119     c                             ( ( V01pog <> '999'    and
221400160119     c                                 V01pog <> %editc( TGDpog : 'X' ) )  or
221500160119     c                               ( V01pog =  '999'    and
221600160119     c                                 %lookup( TGDpog : sk_POg2 ) = *zero ) )
221700151222     c*// Non Fil.Risp.:           %lookup( dTGD01.§TGDfilR : sk_POg2 ) = *zero
221800151221     c                   eval      HS1prt2 = '5'
221900151222     c                   eval      VS1prt  = 'A'
222000151221     c                   endif
222100040227      * - OGGETTO
222200040227     c                   clear                   vs1ogg
222300040227      * ---------- SPEDIZIONE -----------
222400040227     c                   if        tgdtor = 'S'
222500040227      *
222600040227     c                   movel     tgdogg        WRKspe
222700040227     c                   eval      vs1ogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
222800040227     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
222900040227     c                             ' ' + WRKann
223000040227      *
223100040227     c                   endif
223200040227      * ----------    ORM     -----------
223300040227     c                   if        tgdtor = 'O'
223400040227     c                   movel     tgdogg        WRKorm
223500040227      *
223600040227     c                   eval      vs1ogg = 'ORM ' + %trim(WRKpoe) +  '/' +
223700040227     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
223800040227     c                             '/' + %trim(WRKnrv)
223900040227      *
224000040227     c                   endif
224100040227      * ----------  GENERICO  -----------
224200040227     c                   if        tgdtor = 'G'
224300040227     c                   EVAL      VS1ogg = TGDogg
224400040227     c                   endif
224500040227
224600040227      * MOTIVO RECLAMO
224700040227     c                   clear                   tibs02ds
224800040227     C                   MOVEL     'C'           t02mod
224900040227     C                   MOVEL     knsif         t02sif
225000040227     C                   MOVEL     'MAR'         t02cod
225100040227     C                   MOVEL     tgdmad        t02ke1
225200040227      *
225300040227     C                   CALL      'TIBS02R'
225400040227     C                   PARM                    KPJBA
225500040227     C                   PARM                    TIBS02DS
225600040227      *
225700040227     C                   movel     T02uni        DMAR
225800040227     C                   movel     §MARdesc      vsdmad
225900140403     C                   eval      VSDmad2 = TGDmad + ' ' + §MARdesc
226000140217      * VERIFICO SE MOTIVO TECNICO NON ABILITO LA MANUTENZIONE MA SOLO LA VISUALIZZAZIONE
226100140217     C                   IF        VS1GES = ' '   AND
226200140217     C                             §MARUTUT <> ' '
226300140217     C                   EVAL      VS1GES = 'V'
226400140217     C                   ENDIF
226500040227      * CHIUSURA RECLAMO
226600040304     C                   clear                   vs1cdt
226700040304     c                   setoff                                       06
226800040227     c                   If        tgdcdt <> *blank
226900040304     c                   eval      *in06 = *on
227000040304     C                   movel     tgdcdt        vs1cdt
227100040304      * data chiusura
227200040304     c                   clear                   WLBdat
227300040304     c                   eval      G02inv = tgddcd
227400040304     c                   eval      G02err = '3'
227500040304     c                   call      'XSRDA8'
227600040304     c                   parm                    WLBdat
227700040304     c                   z-add     G02dat        vs2dcd                         data chiuusra
227800040227     c                   endif
227900040227      * recupero le note
228000040227      *
228100040227     C                   clear                   FIDNB0DS
228200040421     c                   z-add     vs1ant        iB0ant
228300040227     c                   z-add     vs1nut        iB0nut
228400040227     c                   z-add     tgdpdt        iB0pdt
228500040227     c                   movel     'GEN'         iB0tIN
228600040227     c                   movel     'R'           iB0flm
228700040227      *
228800040227     C                   CALL      'FIDNB0R'
228900040227     C                   PARM                    kpjba
229000040227     C                   PARM                    FIDNB0DS
229100040227      *
229200040227      * valorizzo le prime 4 note a video
229300040227      *
229400040227     c                   movel     DB0NO1        vs1no1
229500040227     c                   movel     DB0NO2        vs1no2
229600141217      /free
229700141217       //?Imposto nel sbf l'indicatore che condiziona la visualizzazione dei
229800141217       //?dati di chiusura
229900141217         VSHin06 = *in06;
230000150428       //?Imposto nel sbf le condizioni che fanno:?
230100150428       //?- lampeggiare il campo STATO (ex Priorità) - se 0?
230200150428       //?- evidenziare in rosso data/ora creazione - se STATO = 0?
230300150428       //?- evidenziare in bianco rag.soc. chiamante - se STATO <> 0?
230400150417         if  VS1prt = '0';
230500150420           DspAtrPRT = c_DspAtrRed_BL;
230600150428           DspAtrDAR = c_DspAtrRed;
230700150428           DspAtrRSC = c_DspAtrRed_UL;
230800150417         else;
230900150417           DspAtrPRT = c_DspAtrNorm;
231000150428           DspAtrDAR = c_DspAtrNorm;
231100150428           DspAtrRSC = c_DspAtrWht_UL;
231200150417         endif;
231300141217      /end-free
231400141217
231500040227      *
231600140526     c                   if        wMod = 'R'  and  (iA2tor = 'S'
231700140526     c                                          or   iA2tor = 'O')
231800140221     c                   exsr      sr_PrepTXT
231900140219     c                   write     FIA2S03
232000140219     c                   else
232100040227     c                   write     fia2s02
232200140219     c                   endif
232300041018      * controllo il numero massimo di record nel subfile
232400140219     c                   If        S01nrr >= 9998   or
232500140219     c                             S03nrr >= 9998
232600041018     c                   eval      *in28 = *on
232700041018     C                   endif
232800040227      *
232900040227     c                   endsr
233000140220
233100140220      /free
233200140221
233300140221       //--------------------------------------------------------------
233400140221       //?Preparazione campi VS1TXT1 e VS1TXT2 del subfile S03.        ?
233500140221       //--------------------------------------------------------------
233600140221       BEGSR  sr_PrepTXT;
233700140221
233800140221         //*...+....1....+....2....+
233900140221         //.Chiusa --- il --/--/----
234000140224
234100140224         clear  VS1txt1;
234200140224         clear  VS1txt2;
234300140221
234400140221         Select;
234500140221
234600140221           // -?Reclamo NON chiuso?
234700140221           When  TGDcdt = *blank;
234800140221             // -?Valorizzazione delle 4 note?
234900140221             VS1txt1 = DB0no1 + ' ' + DB0no2;
235000140221             VS1txt2 = DB0no3 + ' ' + DB0no4;
235100140221
235200140224           // -?Reclamo chiuso e senza note?
235300140221           When  TGDcdt <> *blank  and  DB0no1 = *blank  and
235400140221                 DB0no2 =  *blank  and  DB0no3 = *blank;
235500140221             evalR VS1txt1 = c_DspAtr_HI  + 'Chiusa' +
235600140221                             c_DspAtrNorm + VS1cdt +
235700140221                             c_DspAtr_HI  + 'il' +
235800140221                             c_DspAtrNorm + %editc( VS2dcd : 'Y' );
235900140224           // -?Reclamo chiuso e con solo una nota?
236000140224           When  TGDcdt <> *blank  and  DB0no1 <> *blank  and
236100140224                 DB0no2 =  *blank  and  DB0no3 =  *blank;
236200140224             VS1txt1 = DB0no1;
236300140224             %subst( VS1txt1 : %len(VS1txt1) - 24 ) =
236400140224                             c_DspAtr_HI  + 'Chiusa' +
236500140224                             c_DspAtrNorm + VS1cdt +
236600140224                             c_DspAtr_HI  + 'il' +
236700140224                             c_DspAtrNorm + %editc( VS2dcd : 'Y' );
236800140224           // -?Reclamo chiuso e con note?
236900140221           When  TGDcdt <> *blank;
237000140221             VS1txt1 = DB0no1 + ' ' + DB0no2;
237100140221             VS1txt2 = DB0no3;
237200140221             %subst( VS1txt2 : %len(VS1txt2) - 24 ) =
237300140221                             c_DspAtr_HI  + 'Chiusa' +
237400140221                             c_DspAtrNorm + VS1cdt +
237500140221                             c_DspAtr_HI  + 'il' +
237600140221                             c_DspAtrNorm + %editc( VS2dcd : 'Y' );
237700140221
237800140221         EndSl;
237900140221
238000140221       ENDSR;
238100150316
238200150316       //--------------------------------------------------------------
238300150316       //?Ordinamento subfile S02.                                     ?
238400150316       //?  This subroutine sorts the subfile records.                 ?
238500150316       //--------------------------------------------------------------
238600150316       BEGSR  sr_OrdinamS02;
238700150316
238800150316         // -?Impostazione NRR dell'ultimo record del subfile          ?
238900150316         //  ?(già fatto in sr_CarSBF:                                 ?
239000150316         //  ? S02NRR può essere variato, se già inserita un'opzione)  ?
239100150316         //RrnLast  = S02nrr;
239200150316
239300150316         V2Cnrr = 1;
239400150316         clear  S01nrr;
239500150316
239600150317         //*in28 = *off;
239700150316         *in69 = *off;
239800150316         *in70 = *off;
239900150316         *in71 = *off;
240000150316
240100150316         //?___________________________________________________________?
240200150316         //?Initialize the key fields to sort on.?
240300150316         //?There is one extra field not in the subfile -- Selected --?
240400150316         //?that is added to the record. This field is used to place?
240500150316         //?selected records in front of nonselected records.?
240600150316         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
240700150316
240800150316         clear  QLGKL;
240900150316         clear  QLGSKL;
241000150316         clear  QLGSCB;
241100150316         clear  QLGSCB00;
241200150316
241300150316         // -?Gestione della scelta ordinamento:?
241400150316         //Select;
241500150316
241600150417           // -?Ord. x Stato (ex Priorità) + Data e Ora Creazione / Reparto / Utente?
241700150316           //When  ???;
241800150317             // -?3 campi chiave?
241900150317             QLGNBRK  = 3;
242000150417             // -?1° campo: Stato (ex Priorità) + Data e Ora Creazione?
242100151221             //            ?(HS1PRT2 + HS1DAC + VS1ORC)?
242200150417             //            ?a posizone   97, 13 byte, char, ascending.?
242300150325             QLGSP    = %size(VS1ant) + %size(VS1nut) + %size(VS1pdt) +
242400150325                        %size(VS1pot) + %size(VS1ges) + %size(VS1dar) +
242500150417                        %size(VS1rpr) + %size(VS1rpg) + %size(VS1pog) +
242600150417                        %size(VS1rsc) + %size(VS1tch) + %size(VSDmad) +
242700150325                        1;
242800151221             QLGSS    = %size(HS1prt2) + %size(HS1dac) + %size(VS1orc);
242900150316             QLGDT    = c_Carattere;
243000150316             QLGSO    = c_Ascendente;
243100150316             QLGKL(1) = QLGSKL;
243200150417             // -?2° campo: Reparto (VS1rpr)?
243300150417             //            ?a posizone   27, 10 byte, char, ascending.?
243400150417             QLGSP    = %size(VS1ant) + %size(VS1nut) + %size(VS1pdt) +
243500150417                        %size(VS1pot) + %size(VS1ges) + %size(VS1dar) +
243600150417                        1;
243700150417             QLGSS    = %size(VS1rpr);
243800150417             QLGDT    = c_Carattere;
243900150316             QLGSO    = c_Ascendente;
244000150316             QLGKL(2) = QLGSKL;
244100150417             // -?3° campo: Utente gestione (VS1rpg)?
244200150417             //            ?a posizone   37, 10 byte, char, ascending.?
244300150417             QLGSP    = %size(VS1ant) + %size(VS1nut) + %size(VS1pdt) +
244400150417                        %size(VS1pot) + %size(VS1ges) + %size(VS1dar) +
244500150417                        %size(VS1rpr) + 1;
244600150417             QLGSS    = %size(VS1rpg);
244700150417             QLGDT    = c_Carattere;
244800150316             QLGSO    = c_Ascendente;
244900150316             QLGKL(3) = QLGSKL;
245000150316
245100150316         //EndSl;
245200150316
245300150316         // -?Load other sort parameters.?
245400150316         QLGLB   = 80 + 16 * c_MaxKey;
245500150317         QLGRL   = %size(SflRcd) - 1;
245600150316         QLGRT   = 8;
245700150316         QLGOKL  = 80;
245800150316         QLGLKE  = 16;
245900150316         QLGLSS  = 290;
246000150316         // -?Initialize Sort I/O API fields.?
246100150316         QLGRL00 = QLGRL;
246200150316         QLGRC00 = 1;
246300150316         clear  QUSEI;
246400150316         QUSBPRV = %size(QUSEC);
246500150316
246600150316         // -?First step - Initialize the sort routine.?
246700150316         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
246800150316                      SizeList : ReturnSize : QUSEC );
246900150316
247000150316         // -?Next step - Write records to I/O routine.?
247100150316         QLGRT00  = c_Put;
247200150316         For  Nrr = 1  To  RrnLast;
247300150316           chain  Nrr  FIA2S02;
247400150316           // -?Solo le righe con Selected = 'Y' sono riordinate,?
247500150316           //  ?quindi per fare un ordinamento di tutte le righe?
247600150316           //  ?metto 'Y' sempre.?
247700150316           Selected = 'Y';
247800150316           clear  QUSEI;
247900150316           QUSBPRV  = %size(QUSEC);
248000150317           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
248100150317                         SizeList : NotUsed : QUSEC );
248200150316         EndFor;
248300150316
248400150316         // -?Next step - Signal end of input, clear subfile for reload.?
248500150316         QLGRT00 = c_EndPut;
248600150316         clear  QUSEI;
248700150316         QUSBPRV = %size(QUSEC);
248800150317         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
248900150317                       SizeList : NotUsed : QUSEC );
249000150316
249100150316         // -?Pulizia subfile?
249200150316         *in20 = *off;
249300150316         *in21 = *off;
249400150316         *in22 = *on;
249500150316         write  FIA2C02;
249600150316         *in22 = *off;
249700150316
249800150316         // -?Final step - Write the records back to the subfile.?
249900150316         QLGRT00  = c_Get;
250000150316         For  Nrr = 1  To RrnLast;
250100150316           clear  QUSEI;
250200150316           QUSBPRV = %size(QUSEC);
250300150317           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
250400150316                         QLGRL00  : NotUsed : QUSEC );
250500150316           S01nrr = Nrr;
250600150320           *in06  = VSHin06;
250700150316           write  FIA2S02;
250800150316         EndFor;
250900150316
251000150316         // -?Visualizzazione del SFL (se ci sono dati)?
251100150316         *in20 = (S01nrr > *zero);
251200150316         *in21 = *in20;
251300150316
251400150316       ENDSR;
251500140220
251600140220       //--------------------------------------------------------------
251700140220       //?Ordinamento subfile S03.                                     ?
251800140220       //?  This subroutine sorts the subfile records.                 ?
251900140220       //--------------------------------------------------------------
252000140220       BEGSR  sr_OrdinamS03;
252100140220
252200140220         // -?Impostazione NRR dell'ultimo record del subfile          ?
252300140220         //  ?(già fatto in sr_CarSBF:                                 ?
252400140220         //  ? S03NRR può essere variato, se già inserita un'opzione)  ?
252500140220         //RrnLast  = S03nrr;
252600140220
252700140220         V2Cnrr = 1;
252800140220         clear  S03nrr;
252900140220
253000150317         //*in28 = *off;
253100140220         *in69 = *off;
253200140220         *in70 = *off;
253300140220         *in71 = *off;
253400140220
253500140220         //?___________________________________________________________?
253600140220         //?Initialize the key fields to sort on.?
253700140220         //?There is one extra field not in the subfile -- Selected --?
253800140220         //?that is added to the record. This field is used to place?
253900140220         //?selected records in front of nonselected records.?
254000140220         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
254100140220
254200140220         clear  QLGKL;
254300140220         clear  QLGSKL;
254400140220         clear  QLGSCB;
254500140220         clear  QLGSCB00;
254600140220
254700140220         // -?Gestione della scelta ordinamento:?
254800140220         Select;
254900140220
255000150617           // -?Ordinamento per Numero R.A. DIscendente?
255100150323           When  $Ord_Desc;
255200140220             // -?3 campi chiave?
255300140220             QLGNBRK  = 3;
255400150617             // -?1° campo: Data e Ora Creazione?
255500150617             //            ?(HS1DAC + VS1ORC)?
255600150617             //            ?a posizone   98, 12 byte, int, descending.?
255700150617             QLGSP    = %size(VS1ant) + %size(VS1nut) + %size(VS1pdt) +
255800150617                        %size(VS1pot) + %size(VS1ges) + %size(VS1dar) +
255900150617                        %size(VS1rpr) + %size(VS1rpg) + %size(VS1pog) +
256000150617                        %size(VS1rsc) + %size(VS1tch) + %size(VSDmad) +
256100150617                        %size(VS1prt) + 1;
256200150617             QLGSS    = %size(HS1dac) + %size(VS1orc);
256300150617             QLGDT    = c_NumIntero;
256400150617             QLGSO    = c_Discendente;
256500150617             QLGKL(1) = QLGSKL;
256600150617             // -?2° campo: Anno e Numero Richiesta?
256700150617             //            ?(VS1ANT + VS1NUT)?
256800150617             //            ?a posizone    1, 13 byte, int, descending.?
256900150325             QLGSP    = 1;
257000150617             QLGSS    = %size(VS1ant) + %size(VS1nut);
257100140220             QLGDT    = c_NumIntero;
257200140220             QLGSO    = c_Discendente;
257300150617             QLGKL(2) = QLGSKL;
257400140220             // -?3° campo: Progressivo Riga (VS1pdt)?
257500150325             //            ?a posizone   14,  3 byte, int, descending.?
257600150325             QLGSP    = %size(VS1ant) + %size(VS1nut) + 1;
257700140220             QLGSS    = %size(VS1pdt);
257800140220             QLGDT    = c_NumIntero;
257900140220             QLGSO    = c_Discendente;
258000140220             QLGKL(3) = QLGSKL;
258100140220
258200150617           // -?Ordinamento per Numero R.A. Ascendente?
258300150323           When  Not $Ord_Desc;
258400140220             // -?3 campi chiave?
258500140220             QLGNBRK  = 3;
258600150617             // -?1° campo: Data e Ora Creazione?
258700150617             //            ?(HS1DAC + VS1ORC)?
258800150617             //            ?a posizone   98, 12 byte, int, ascending.?
258900150617             QLGSP    = %size(VS1ant) + %size(VS1nut) + %size(VS1pdt) +
259000150617                        %size(VS1pot) + %size(VS1ges) + %size(VS1dar) +
259100150617                        %size(VS1rpr) + %size(VS1rpg) + %size(VS1pog) +
259200150617                        %size(VS1rsc) + %size(VS1tch) + %size(VSDmad) +
259300150617                        %size(VS1prt) + 1;
259400150617             QLGSS    = %size(HS1dac) + %size(VS1orc);
259500150617             QLGDT    = c_NumIntero;
259600150617             QLGSO    = c_Ascendente;
259700150617             QLGKL(1) = QLGSKL;
259800150617             // -?2° campo: Anno e Numero Richiesta?
259900150617             //            ?(VS1ANT + VS1NUT)?
260000150617             //            ?a posizone    1, 13 byte, int, ascending.?
260100150617             QLGSP    = 1;
260200150617             QLGSS    = %size(VS1ant) + %size(VS1nut);
260300150617             QLGDT    = c_NumIntero;
260400140220             QLGSO    = c_Ascendente;
260500150617             QLGKL(2) = QLGSKL;
260600140220             // -?3° campo: Progressivo Riga (VS1pdt)?
260700150617             //            ?a posizone   14,  3 byte, int, ascending.?
260800150325             QLGSP    = %size(VS1ant) + %size(VS1nut) + 1;
260900140220             QLGSS    = %size(VS1pdt);
261000140220             QLGDT    = c_NumIntero;
261100140220             QLGSO    = c_Ascendente;
261200140220             QLGKL(3) = QLGSKL;
261300140220
261400140220         EndSl;
261500140220
261600140220         // -?Load other sort parameters.?
261700140220         QLGLB   = 80 + 16 * c_MaxKey;
261800150317         QLGRL   = %size(SflRcd) - 1;
261900140220         QLGRT   = 8;
262000140220         QLGOKL  = 80;
262100140220         QLGLKE  = 16;
262200140220         QLGLSS  = 290;
262300140220         // -?Initialize Sort I/O API fields.?
262400140220         QLGRL00 = QLGRL;
262500140220         QLGRC00 = 1;
262600140220         clear  QUSEI;
262700140220         QUSBPRV = %size(QUSEC);
262800140220
262900140220         // -?First step - Initialize the sort routine.?
263000140220         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
263100140220                      SizeList : ReturnSize : QUSEC );
263200140220
263300140220         // -?Next step - Write records to I/O routine.?
263400140220         QLGRT00  = c_Put;
263500140220         For  Nrr = 1  To  RrnLast;
263600140220           chain  Nrr  FIA2S03;
263700140220           // -?Solo le righe con Selected = 'Y' sono riordinate,?
263800140220           //  ?quindi per fare un ordinamento di tutte le righe?
263900140220           //  ?metto 'Y' sempre.?
264000140220           Selected = 'Y';
264100140220           clear  QUSEI;
264200140220           QUSBPRV  = %size(QUSEC);
264300150317           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
264400150317                         SizeList : NotUsed : QUSEC );
264500140220         EndFor;
264600140220
264700140220         // -?Next step - Signal end of input, clear subfile for reload.?
264800140220         QLGRT00 = c_EndPut;
264900140220         clear  QUSEI;
265000140220         QUSBPRV = %size(QUSEC);
265100150317         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
265200150317                       SizeList : NotUsed : QUSEC );
265300140220
265400140220         // -?Pulizia subfile?
265500140220         *in20 = *off;
265600140220         *in21 = *off;
265700140220         *in22 = *on;
265800140220         write  FIA2C03;
265900140220         *in22 = *off;
266000140220
266100140220         // -?Final step - Write the records back to the subfile.?
266200140220         QLGRT00  = c_Get;
266300140220         For  Nrr = 1  To RrnLast;
266400140220           clear  QUSEI;
266500140220           QUSBPRV = %size(QUSEC);
266600150317           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
266700140220                         QLGRL00  : NotUsed : QUSEC );
266800140220           S03nrr = Nrr;
266900140220           write  FIA2S03;
267000140220         EndFor;
267100140220
267200140220         // -?Visualizzazione del SFL (se ci sono dati)?
267300140220         *in20 = (S03nrr > *zero);
267400140220         *in21 = *in20;
267500140220
267600140220         // -?Impostazione nuova descrizione tasto F11=Ord. x ...?
267700140220         Select;
267800140220           When  $Ord_Desc;
267900140221             Z02F11 = 'F11=Ord.Ascendente ';
268000140220           When  Not  $Ord_Desc;
268100140221             Z02F11 = 'F11=Ord.Discendente';
268200140220         EndSl;
268300140220
268400140220       ENDSR;
268500140220
268600140220      /end-free
268700140220
268800040227      *---------------------------------------------------------------*
268900040227      * Routine Lettura  SUBFILE                                      *
269000040227      *---------------------------------------------------------------*
269100040227     c     SR_READSF     BegSR
269200040227
269300040315     c                   eval      W_f03 = ' '
269400040315
269500040227     c                   do        *hival
269600040227      *
269700150319     c                   if        wMod = 'R'  and  (iA2tor = 'S'
269800140526     c                                          or   iA2tor = 'O')
269900140220      *
270000140220     c                   readc     FIA2S03
270100140220      *
270200150319     c                   else
270300140220      *
270400040227     c                   readc     FIA2S02
270500140220      *
270600150319     c                   endif
270700040227      *
270800150504     c                   if        %eof(fidna2d)
270900040227     c                   leave
271000040227     c                   endif
271100040227      *
271200040227      * se opzione = a 2 nel caso di programma richiamto in sola modalità
271300040227      * interrogazione do errore
271400040304     c                   If        vs1opz = '2' and Wmod = 'R' and IA2MOD = 'I'
271500040227     c                   eval      *in70 = *on
271600040227     c                   eval      *in90 = *on
271700150319     c                   if        wMod = 'R'  and  (iA2tor = 'S'
271800140526     c                                          or   iA2tor = 'O')
271900140220     c                   update    FIA2S03
272000140220     c                   z-add     S03nrr        v2cnrr
272100150319     c                   else
272200141217     c                   eval      *in06 = VSHin06
272300040317     c                   update    fia2s02
272400040317     c                   z-add     s01nrr        v2cnrr
272500150319     c                   endif
272600150507     c                   z-add     V2Cnrr        V2CrtnCsr
272700040227     c                   leave
272800040227     c                   endif
272900150506      *
273000040317      * se opzione = a 2 nel caso di reclamo non gestibile dal P.O. do errore
273100040317      *
273200040317     c                   If        vs1opz = '2' and vs1ges = 'N'
273300040317     c                   eval      *in71 = *on
273400040317     c                   eval      *in90 = *on
273500150319     c                   if        wMod = 'R'  and  (iA2tor = 'S'
273600140526     c                                          or   iA2tor = 'O')
273700140220     c                   update    FIA2S03
273800140220     c                   z-add     S03nrr        v2cnrr
273900150319     c                   else
274000141217     c                   eval      *in06 = VSHin06
274100040317     c                   update    fia2s02
274200040317     c                   z-add     s01nrr        v2cnrr
274300150319     c                   endif
274400150507     c                   z-add     V2Cnrr        V2CrtnCsr
274500040317     c                   leave
274600040317     c                   endif
274700150506      *
274800140217      * se opzione = a 2 nel caso di reclamo non gestibile xchè motivo apertura solo tecnico
274900140217      *
275000140217     c                   If        vs1opz = '2' and vs1ges = 'V'
275100140217     c                   eval      *in69 = *on
275200140217     c                   eval      *in90 = *on
275300150319     c                   if        wMod = 'R'  and  (iA2tor = 'S'
275400140526     c                                          or   iA2tor = 'O')
275500140220     c                   update    FIA2S03
275600140220     c                   z-add     S03nrr        v2cnrr
275700150319     c                   else
275800141217     c                   eval      *in06 = VSHin06
275900140217     c                   update    fia2s02
276000140217     c                   z-add     s01nrr        v2cnrr
276100150319     c                   endif
276200150507     c                   z-add     V2Cnrr        V2CrtnCsr
276300140217     c                   leave
276400140217     c                   endif
276500150506      *
276600150317      * se opzione = a 2 nel caso di sola interrogazione do errore
276700070601      *
276800070601     c                   If        vs1opz = '2' and *in05 = *on
276900070601     c                   eval      *in70 = *on
277000070601     c                   eval      *in90 = *on
277100150319     c                   if        wMod = 'R'  and  (iA2tor = 'S'
277200140526     c                                          or   iA2tor = 'O')
277300140220     c                   update    FIA2S03
277400140220     c                   z-add     S03nrr        v2cnrr
277500150319     c                   else
277600141217     c                   eval      *in06 = VSHin06
277700070601     c                   update    fia2s02
277800070601     c                   z-add     s01nrr        v2cnrr
277900150319     c                   endif
278000150507     c                   z-add     V2Cnrr        V2CrtnCsr
278100070601     c                   leave
278200150317     c                   endif
278300040227      *
278400040227      * se opzione = a 5 emetto la videata di visualizzazione
278500040227     c                   If        vs1opz = '5'
278600040227     c                   exsr      SR_visual
278700150506     c                   if        wMod = 'R'  and  (iA2tor = 'S'
278800150506     c                                          or   iA2tor = 'O')
278900150506     c                   z-add     S03nrr        V2Cnrr
279000150506     c                   else
279100150507     c                   z-add     S01nrr        V2Cnrr
279200150506     c                   endif
279300150507     c                   z-add     V2Cnrr        V2CrtnCsr
279400040227     c                   endif
279500040227      *
279600040227      * se opzione = a 2 richiamo il programma di manutenzione
279700040227     c                   If        vs1opz = '2'
279800040227     c                   clear                   fidna0ds
279900040227     c                   eval      ia0mod = 'M'
280000040421     c                   eval      ia0pot = vs1pot
280100040421     c                   eval      ia0ant = vs1ant
280200040227     c                   eval      ia0nut = vs1nut
280300040227     c                   eval      ia0pdt = vs1pdt
280400040227      *
280500161027     c                   CALL      'FIDNA1R'
280600040227     c                   PARM                    KPJBA
280700040227     c                   PARM                    FIDNA0ds
280800040315      * GESTISCO F3 DI RITORNO
280900040305     c                   if        oa0f03 = 'S'
281000040315     c                   eval      W_f03 = 'S'
281100040305     c                   leave
281200040305     c                   endif
281300150506     c                   eval      $InzSFL = *on
281400040227
281500040227     c                   endif
281600040227      *
281700040227     c                   clear                   vs1opz
281800150319     c                   if        wMod = 'R'  and  (iA2tor = 'S'
281900140526     c                                          or   iA2tor = 'O')
282000140220     c                   update    FIA2S03
282100150319     c                   else
282200141217     c                   eval      *in06 = VSHin06
282300040227     c                   update    fia2s02
282400150319     c                   endif
282500040227      *
282600040227     c                   Enddo
282700040227
282800040227     c                   Endsr
282900150312
283000040302      *---------------------------------------------------------------*
283100040302      * Routine visualizzazione dettaglio                             *
283200040302      *---------------------------------------------------------------*
283300040302     c     SR_visual     BegSR
283400040302
283500040302      * caricamento dei dati a video
283600040302
283700040302     c                   Exsr      SR_carv02
283800040302
283900140221     c                   clear                   V1Togg
284000040302     c                   write     fia2t01
284100040302     c                   do        *hival
284200040302
284300040302     c                   exfmt     fia2d02
284400150319
284500150319     c                   SELECT
284600040302
284700040302      * F12 - RITORNO
284800040302
284900150319     C                   WHEN      *INKL
285000040302     c                   leave
285100040302
285200040302      * F18 - NOTE
285300040302
285400150319     c                   WHEN      *INKS
285500040302      *
285600040302     C                   clear                   FIDNB0DS
285700040305     c                   z-add     v02pot        iB0pot
285800040421     c                   z-add     v02ant        iB0ant
285900040302     c                   z-add     v02nut        iB0nut
286000040302     c                   z-add     v02pdt        iB0pdt
286100040305
286200040305     c                   z-add     tgddar        ib0dar
286300040305     c                   z-add     v02orr        ib0orr
286400040305
286500040305     c                   move      v02mad        ib0mad
286600040305     c                   movel     vd2mad        ib0dma
286700040305
286800040305     c                   movel     vs1ogg        Ib0ogg
286900040302     c                   movel     'GEN'         iB0tIN
287000040303     c                   movel     'I'           iB0flm
287100040302      *
287200040302     C                   CALL      'FIDNB0R'
287300040302     C                   PARM                    kpjba
287400040302     C                   PARM                    FIDNB0DS
287500150319
287600150403      * F11 = Storico
287700150319
287800150319     c                   WHEN      *INKK
287900150319      *
288000150319     c                   clear                   FIDNA0ds
288100150319     c                   eval      iA0pot = V02pot
288200150319     c                   eval      iA0ant = V02ant
288300150319     c                   eval      iA0nut = V02nut
288400150319     c                   eval      iA0pdt = V02pdt
288500150319     c                   call      'FIDNA3R'
288600150319     c                   parm                    KPJBA
288700150319     c                   parm                    FIDNA0ds
288800040302
288900040405      * F02 = Interrogazione Oggetto
289000040302
289100150319     c                   WHEN      *INKB
289200040302
289300040302      * SPEDIZIONI
289400040302
289500041008      * in filiale
289600150319     c                   If        *IN01 and
289700150319     c                             DUTlpo <> 'S'
289800040419
289900160706     c                   clear                   KPJBU
290000160706     c                   clear                   ParamU6ds1
290100160706     c                   eval      ParamU6ds1.V2Caas = V02ann
290200160706     c                   eval      ParamU6ds1.COMlnp = V02lnp
290300160706     c                   eval      ParamU6ds1.V2Cnrs = V02nrs
290400160706     c                   eval      ParamU6ds1.V2Cnsp = V02nsp
290500160706     c                   eval      KPJBU = ParamU6ds1
290600160706      * preparo l'altro parametro da passare
290700160706     c                   clear                   fnlrU6ds
290800160706     c*//                eval      iLRU6sel = 'S'
290900160706     c                   eval      iLRU6ra  = 'S'
291000160706      * -?Campo IA1ins già impostato nella subr. sr_CountRA?
291100160706     c                   eval      eLRU6ins = iA1ins
291200160706      *
291300160706     c                   call      'FNLRU6R1'
291400160706     c                   parm                    KPJBA
291500160706     c                   parm                    fnlrU6ds
291600160706     c                   movel     kpjbu         ParamU6ds1
291700160706      * spedizione non più esitente
291800160706     c                   if        oLRU6err = 'E'
291900160706     c                   eval      *in54 = *on
292000160706     c*//                movel     oLRU6msg      V1Cmsg
292100160706     c                   iter
292200160706     c                   endif
292300041011
292400041011     C                   ENDIF
292500150319
292600041008      * in sede
292700150319     c                   If        *IN01 and
292800150319     c                             DUTlpo = 'S'
292900041008
293000041008     c                   clear                   TNSB50DS
293100041008     C                   movel     'R05'         I50OP0
293200041008     C                   z-add     V02ann        D50aas
293300041008     C                   z-add     V02lnp        D50lnp
293400041008     C                   z-add     V02nrs        D50nrs
293500041008     C                   z-add     V02nsp        D50nsp
293600041008     C                   movel(P)  TNSB50DS      KPJBU
293700041008      *
293800041008      * se richiamato
293900150610      *//* -?Campo IA1ins già impostato nella subr. sr_CountRA?
294000150610     c*//                clear                   fidna1ds
294100150610     c*//                eval      IA1INS = 'N'
294200041008      *
294300041008     C                   call      'TNSB50R'
294400041008     C                   parm                    KPJBA
294500041008     C                   parm                    FIDNA1DS
294600041008     c                   endif
294700040302
294800040302      * ORM
294900040302
295000150319     c                   If        *IN02
295100040419      *
295200040419     *** * interrogo direttamente con il richiano di FIOR07R
295300040419     *** * => NON più "direttamente"
295400040419     c                   if        NOT %open(FNORM01L)
295500040419     c                   open      FNORM01L
295600040419     c                   endif
295700040419      *
295800040419      * verifica dell'esistenza su FNORM
295900040419     c     k04orm01      setll     FNORM000
296000040419     c                   if        NOT %equal(FNORM01L)
296100040419     c                   eval      *in54 = *on
296200040419     c                   iter
296300040419     c                   endif
296400040419      *
296500040419      * interrogo con il richiamo di FIOR07R
296600040302     C                   clear                   PARM01
296700040302     C                   move      v02poe        ppoe
296800040305     C                   move      v02nso        pnsr
296900040305     C                   move      v02orm        pnor
297000040302     C                   move      v02nrv        pnrv
297100040302     C                   move      '5'           psce
297200040302      * attenzione non muovo nulla nel campo del p.o. gestione .... vediamo se funziona !!!!!!
297300040302     C                   movel(p)  PARM01        KPJBU
297400040305
297500150610      *//* -?Campo IA1ins già impostato nella subr. sr_CountRA?
297600150610     c*//                clear                   fidna1ds
297700150610     c*//                eval      IA1INS = 'N'
297800040305
297900040302
298000040302     C                   call      'FIOR07R'
298100040302     C                   parm                    KPJBA
298200040305     C                   parm                    FIDNA1DS
298300040302
298400040302     c                   endif
298500150319
298600150319     c                   ENDSL
298700040302
298800040302     c                   enddo
298900040302
299000040302     c                   Endsr
299100040227      *---------------------------------------------------------------*
299200040302      * Routine caricamento seconda videata                           *
299300040227      *---------------------------------------------------------------*
299400040302     c     SR_CARV02     BegSR
299500040301
299600040301      * resetto qualche indicatore
299700040301     c                   setoff                                       042901
299800040301     c                   setoff                                       0203
299900150319     c                   eval      *in16 = *off
300000050120      * pulisco alcuni campi
300100050120     c                   clear                   vd2ksc
300200151221     c                   clear                   dTGDfar
300300151221     c                   clear                   dTGD01
300400151221     c                   clear                   dTGR01a
300500040227
300600040301      * aggancio il record da visualizzare
300700040301
300800160104     c     ktgd01        chain     fitgd000
300900040301
301000151221if  1c                   If        %found(fitgd01l)
301100151221
301200151221     c                   eval      dTGDfar = TGDfar
301300151221     c                   eval      dTGD01  = TGDflo
301400040301      *
301500040301     c                   z-add     tgdpot        v02pot                         P.O. apertura
301600040421     c                   z-add     tgdant        v02ant                         Anno   reclamo
301700040301     c                   z-add     tgdnut        v02nut                         Numero reclamo
301800040301     c                   z-add     tgdpdt        v02pdt                         progressivo riga
301900040301      *
302000040301     c                   movel     tgdtco        v02tco                         tipo comunicazione
302100040301      * decodifica
302200040301     C                   clear                   vd2tco
302300040301     c                   clear                   tibs02ds
302400040301     C                   MOVEL     'C'           t02mod
302500040301     C                   MOVEL     knsif         t02sif
302600040301     C                   MOVEL     'COR'         t02cod
302700040301     C                   MOVEL     v02tco        t02ke1
302800040301      *
302900040301     C                   CALL      'TIBS02R'
303000040301     C                   PARM                    KPJBA
303100040301     C                   PARM                    TIBS02DS
303200040301      *
303300040419     C                   IF        t02err = *BLANKS
303400040301     C                   movel     T02uni        DCOR
303500040301     C                   movel     §CORdesc      vd2tco                         decod. tp comunic
303600040301     c                   endif
303700040301      * data apertura
303800040301     c                   clear                   WLBdat
303900040301     c                   eval      G02inv = tgddar
304000040301     c                   eval      G02err = '3'
304100040301     c                   call      'XSRDA8'
304200040301     c                   parm                    WLBdat
304300040301     c                   z-add     G02dat        v02dar                         data ricez.comunicaz
304400040301      *
304500040301     c                   z-add     tgdorr        v02orr                         ora  ricez.comunicaz
304600040301      * data creazione
304700040301     c                   clear                   WLBdat
304800040301     c                   eval      G02inv = tgddac
304900040301     c                   eval      G02err = '3'
305000040301     c                   call      'XSRDA8'
305100040301     c                   parm                    WLBdat
305200040301     c                   z-add     G02dat        v02dac                         data creazione
305300040301      *
305400040301     c                   z-add     tgdorc        v02orc                         ora  creazione
305500040301      * utente
305600040301     c                   movel     tgdpru        v02pru                         utente creazione
305700040301      *
305800050117     c                   movel     tgdksc        v02ksc                         codice cliente
305900050118     C                   clear                   VD2KSC
306000151221if  2c                   If        v02ksc > 0
306100050117     C                   clear                   TIBS69DS
306200050117     c                   z-add     v02ksc        I69kac
306300050117     C                   call      'TIBS69R'
306400050117     C                   parm                    tibs69DS
306500050117     C                   parm                    DS_cnaco
306600050117     C                   parm                    DS_cnind
306700050117     C                   parm                    DS_cnclp
306800050117     C                   parm                    DS_fncls
306900050117     C                   eval      wtibs69r = '1'
307000050117     c                   If         O69ERR =  ' '
307100050117     c                   movel     ACOrag        vd2ksc                         decodifica cliente
307200050117     c                   endif
307300151221e   2c                   endif
307400050117      *
307500040301     c                   movel     tgdrsc        v02rsc                         chiamante
307600040301     c                   movel     tgdclv        v02clv                         categoria chiamante
307700040301      * recupero i riferimenti reclamo
307800040301     c                   clear                   v2refe
307900040301     c                   clear                   v2ntel
308000040301     c                   clear                   v2mail
308100040301     c                   clear                   v2nfax
308200040301      *
308300160104     c     Ktgr          setll     fitgr000
308400151221do  2c                   do        *hival
308500160104     c     Ktgr          reade     fitgr000
308600040301
308700040301     c                   if        %eof(fitgr01l)
308800040301     c                   leave
308900040301     c                   endif
309000040301
309100040301      * referente
309200040301     c                   if        tgrtri = 'R'
309300040301     c                   eval      v2refe = tgrrif
309400040301     c                   endif
309500040301      * mail
309600040301     c                   if        tgrtri = 'M'
309700040303     c                   eval      v2mail = tgrrif
309800040301     c                   endif
309900040301      * numeri telefonoici
310000040301     c                   if        tgrtri = 'N'
310100040301     c                   movel     tgrrif        v2ntel
310200040301     c                   move      tgrrif        v2nfax
310300040301     c                   endif
310400151221      * -?Reparto Attesa Risposta?
310500151221     c                   if        TGRtri = 'A'  and
310600151221     c                             TGRpdt = TGDpdt
310700151221     c                   eval      dTGR01a  = TGRrif
310800151221     c                   endif
310900040301
311000151221e   2c                   enddo
311100040301      * oggetto
311200040301     c                   clear                   tibs02ds
311300040301     C                   MOVEL     'C'           t02mod
311400040301     C                   MOVEL     knsif         t02sif
311500040301     C                   MOVEL     'OGR'         t02cod
311600040301     C                   MOVEL     tgdtor        t02ke1
311700040301      *
311800040301     C                   CALL      'TIBS02R'
311900040301     C                   PARM                    KPJBA
312000040301     C                   PARM                    TIBS02DS
312100040301      *
312200040301     C                   movel     T02uni        DOGR
312300040301     C                   movel     §ogrdesc      vd2tor
312400040301      * spedizione
312500040301     c                   if        tgdtor = 'S'
312600040301     c                   eval      *in01 = *on
312700040301     c                   movel     tgdogg        spevid2
312800040301     c                   endif
312900040301      * ORM
313000040301     c                   if        tgdtor = 'O'
313100040301     c                   eval      *in02 = *on
313200040301     c                   movel     tgdogg        ormvid2
313300040301     c                   endif
313400040301      * GENERICO
313500040301     c                   if        tgdtor = 'G'
313600040301     c                   eval      *in03 = *on
313700040301     c                   movel     tgdogg        v02ogg
313800040301     c                   endif
313900040301      * CHI CHIAMA
314000040301     c                   clear                   tibs02ds
314100040301     C                   MOVEL     'C'           t02mod
314200040301     C                   MOVEL     knsif         t02sif
314300040301     C                   MOVEL     'CCR'         t02cod
314400040301     C                   MOVEL     tgdtch        t02ke1
314500040301      *
314600040301     C                   CALL      'TIBS02R'
314700040301     C                   PARM                    KPJBA
314800040301     C                   PARM                    TIBS02DS
314900040301      *
315000040301     C                   movel     T02uni        DCCR
315100040301     C                   movel     §CCRdesc      vd2tch
315200040301      * MOTIVO APERTURA
315300040301     c                   clear                   tibs02ds
315400040301     C                   MOVEL     'C'           t02mod
315500040301     C                   MOVEL     knsif         t02sif
315600040301     C                   MOVEL     'MAR'         t02cod
315700040301     C                   MOVEL     tgdmad        t02ke1
315800040301      *
315900040301     C                   CALL      'TIBS02R'
316000040301     C                   PARM                    KPJBA
316100040301     C                   PARM                    TIBS02DS
316200040301      *
316300040301     C                   movel     T02uni        DMAR
316400040301     C                   movel     tgdmad        v02mad
316500040301     C                   movel     §MARdesc      vd2mad
316600040301      * recupero le note
316700040301      *
316800040301     C                   clear                   FIDNB0DS
316900040421     c                   z-add     v02ant        iB0ant
317000040301     c                   z-add     v02nut        iB0nut
317100040302     c                   z-add     v02pdt        iB0pdt
317200040301     c                   movel     'GEN'         iB0tIN
317300040301     c                   movel     'R'           iB0flm
317400040301      *
317500040301     C                   CALL      'FIDNB0R'
317600040301     C                   PARM                    kpjba
317700040301     C                   PARM                    FIDNB0DS
317800040301      *
317900040301      * valorizzo le prime 4 note a video
318000040301      *
318100040301     c                   movel     DB0NO1        v02no1
318200040301     c                   movel     DB0NO2        v02no2
318300040301     c                   movel     DB0NO3        v02no3
318400040301     c                   movel     DB0NO4        v02no4
318500040301      * se ci sono altre note accendo il 29
318600040301     c                   eval      *in29 = (ob0piu = '+')
318700040302      * reparto che ha in gestione il reclamo
318800040302     c                   movel     tgdrpg        v02rpg
318900040618      * P.O.    che ha in gestione il reclamo
319000040618     c                   movel     tgdpog        v02pog
319100150324      * stato (ex priorità)
319200040302     c                   movel     tgdprt        v02prt
319300150325     c                   clear                   VD2prt
319400150325     c                   clear                   TIBS02ds
319500150325     c                   eval      T02mod = 'C'
319600150325     c                   eval      T02sif = knsif
319700150325     c                   eval      T02cod = 'PRR'
319800150325     c                   eval      T02ke1 = TGDprt
319900150325     c                   call      'TIBS02R'
320000150325     c                   parm                    KPJBA
320100150325     c                   parm                    TIBS02ds
320200150325     C                   eval      dPRR = T02uni
320300150325     C                   eval      VD2prt = §PRRdesc
320400151221      *//* responsabilità
320500151221     c*//                   movel     tgdrsp        v02rsp
320600151221      *
320700160129      /free
320800160129
320900160129         // -?Visualizzazione "Attesa o Ricevuta Risposta"?
321000160129         clear  V02AttRis;
321100160129         Select;
321200160129           When  dTGDfar.§TGDatt = 'S'  and  dTGDfar.§TGDris = 'S';
321300160201             exsr  sr_ReperimentoRispostaRicevuta;
321400160201             if  WrepR = *blank;
321500160203               V02AttRis = 'Risposta di Ritorno   RICEVUTA   dal Reparto '
321600160201                         + %trimR( dTGR01a.§TGRrepR ) + ' della Filiale '
321700160201                         + %editc( dTGD01.§TGDfilR : 'X' );
321800160201             else;
321900160203               V02AttRis = 'Risposta di Ritorno   RICEVUTA   dall''Utente '
322000160201                         + %trimR( WrepR ) + ' della Filiale '
322100160201                         + %editc( WfilR : 'X' );
322200160201             endif;
322300160129           When  dTGDfar.§TGDatt = 'S';
322400160203             V02AttRis = 'Risposta di Ritorno   ATTESA   dal Reparto '
322500160129                       + %trimR( dTGR01a.§TGRrepR ) + ' della Filiale '
322600160129                       + %editc( dTGD01.§TGDfilR : 'X' );
322700160129         EndSl;
322800160129
322900160129      /end-free
323000151221      *
323100160129      * P.O. responsabilità
323200040302     c                   clear                   vd2por
323300040302     c                   eval      v02por = tgdpor
323400040302
323500160104     C     tgdpor        chain     azorg
323600040302    3C                   if        %found(azorg01l)
323700040302     C                   movel     orgdes        vd2por
323800040302     c                   endif
323900040302      * chiuso
324000040302     c                   clear                   v02dcd
324100040302     c                   clear                   v02ocd
324200040302     c                   clear                   v02pct
324300040302     c                   clear                   v02utc
324400040302     c                   clear                   v02cdt
324500040302     c                   clear                   vd2cdt
324600040302      *
324700151221if  2c                   if        tgdcdt <> *blank
324800040302     c                   eval      *in04 = *on
324900040302      * data chiusura
325000040302     c                   clear                   WLBdat
325100040302     c                   eval      G02inv = tgddcd
325200040302     c                   eval      G02err = '3'
325300040302     c                   call      'XSRDA8'
325400040302     c                   parm                    WLBdat
325500040302     c                   z-add     G02dat        v02dcd                         data chiuusra
325600040302      *
325700040302     c                   z-add     tgdocd        v02ocd                         ora  chiusura
325800040302      * utente
325900040302     c                   movel     tgdutc        v02utc                         utente chiuusra
326000040302     c                   movel     tgdpct        v02pct                         P.O.   chiusura
326100040302      * causale
326200040302     c                   clear                   tibs02ds
326300040302     C                   MOVEL     'C'           t02mod
326400040302     C                   MOVEL     knsif         t02sif
326500040302     C                   MOVEL     'CHR'         t02cod
326600040302     C                   MOVEL     tgdcdt        t02ke1
326700040302      *
326800040302     C                   CALL      'TIBS02R'
326900040302     C                   PARM                    KPJBA
327000040302     C                   PARM                    TIBS02DS
327100040302      *
327200040302     C                   movel     T02uni        DCHR
327300040302     C                   movel     tgdcdt        v02cdt
327400040302     C                   movel     §CHRdesc      vd2cdt
327500040302      *
327600151221e   2c                   endif
327700040302
327800151221e   1c                   endif
327900150319
328000150403      * -?Controllo se attivare o no il tasto F11=Storico?
328100150319     c     ktgd01        setll     FITGM000
328200150319     c                   eval      *in16 = ( %equal(FITGM01L) )
328300150610
328400150610      * -?Attivo l'abilitazione del tasto F16=Gest.Rich. nei *pgm chiamati?
328500150611      *  ?SOLO SE:?
328600150611      *  ?· lanciato da menù?
328700150611      *  ?  (non già richiamato da altro *pgm via F16=Gest.Rich.)?
328800150611      *  ?e    SE?
328900150611      *  ?· trattasi di spedizione o O.R.M.?
329000150611      *  ?e    SE?
329100150611      *  ?· a fronte dello stesso oggetto ci sono più di una R.A.?
329200150611     c                   if        wMod <> 'R'  and  ( TGDtor = 'S'
329300150611     c                                           or    TGDtor = 'O' )
329400150610     c                   exsr      sr_CountRA
329500150611     c                   endif
329600040301
329700040227     c                   Endsr
329800150610
329900150610      /free
330000160201
330100160201       BEGSR  sr_ReperimentoRispostaRicevuta;
330200160201
330300160201         clear  WrepR;
330400160201         clear  WfilR;
330500160201
330600160204         setGT   ( V02ant : V02nut : V02pdt : 'A' )  FITGM000;
330700160204         readPE  ( V02ant : V02nut : V02pdt : 'A' )  FITGM000;
330800160201
330900160204         if  %eof( FITGM01L );
331000160201           leavesr;
331100160201         endif;
331200160201
331300160201         WrepR = TGMutn;
331400160201         WfilR = TGMpon;
331500160201
331600160201       ENDSR;
331700150610
331800150610       //--------------------------------------------------------------
331900150610       //?Conto quante RA ci sono a fronte dello stesso oggetto        ?
332000150610       //--------------------------------------------------------------
332100150610       BEGSR  sr_CountRA;
332200150610
332300150610         clear  fidnA1ds;
332400150610         clear  wCountRA;
332500150610
332600150610         exec sql   select count(*) into :wCountRA
332700150610                      from FITGD00F
332800150610                     where TGDtor = :TGDtor
332900150610                       and TGDogg = :TGDogg;
333000150610
333100150610         select;
333200150610           when  SQLcode = 100  or  SQLcode < *zero;
333300150610             iA1ins = 'N';
333400150610           when  wCountRA <= 1;
333500150610             iA1ins = 'N';
333600150610           when  wCountRA > 1;
333700150610             iA1ins = 'I';
333800150610         endsl;
333900150610
334000150610       ENDSR;
334100150610
334200150610      /end-free
334300150610
334400041215      *****************************************************************
334500041215      *  Ricerca alfabetica
334600041215      *****************************************************************
334700041215     c     SR_RICALF     begsr
334800041215      *
334900041215     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
335000050117     C                   Z-ADD     2             PAXkut                         tipo estrazione
335100041215     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
335200041215     C                   Z-ADD     DUTKCI        PAXkcc                         Capoconto Clienti
335300041215     c                   movel(P)  VD1ksc        PAXrag
335400041215     C                   z-add     1             PAXnum
335500041215      *
335600041215     C                   CALL      'XALFA3BR'
335700041215     C                   PARM                    PAXDUT           30
335800041215     C                   PARM                    PAXkut            1 0
335900041215     C                   PARM                    PAXrag           48
336000041215     C                   PARM                    PAXkcc            4 0
336100041215     C                   PARM                    PAXsta            1 0
336200041215     C                   PARM      *blanks       PAXflr           90
336300041215     C                   PARM      *blanks       PAXdit            3
336400041215     C                   PARM                    PAXnum            2 0
336500041215     C                   PARM                    PAXkcm           80
336600041215     C                   PARM                    PAXksm          140
336700041215     C                   PARM                    PAXkdm           60
336800041215      *
336900041215      * NON TROVATO O PREMUTO CMD12
337000041215    2C                   If        paxsta > -1
337100041215     C                   movel     PAXksm        V01KSC
337200041215     C                   movel     PAXrag        VD1KSC
337300041215    2C                   ENDIF
337400041215      *
337500041215     c                   endsr
337600140926
337700140926      /free
337800140926       //--------------------------------------------------------------
337900140926       //?Abilito F19=Disposizioni di Consegna.
338000140926       //--------------------------------------------------------------
338100140926       BEGSR AbiF19;
338200140926
338300140926         *in19 = *off;
338400150123
338500150123       //?SE sono in Sede: FNARB non c'è!
338600150123         if  DUTlpo = 'S';
338700150123           leavesr;
338800150123         endif;
338900140926
339000140926       //?Se devo emettere il subfile usato quando il pgm è richiamato
339100141007         IF  wmod = 'R' and IA2tor = 'S';
339200140926
339300140926         //?Controllo che il pgm non sia stato richiamato dal pgm
339400140926         //?gestione disposizioni di consegna
339500140926           XRTVSTKR (pStack:pStartPoint:pEntries);
339600140926           dsStack = pStack;
339700140926           IF  %lookup('FNLRY03R   ':skStack) > 0;
339800140926             leavesr;
339900140926           ENDIF;
340000140926
340100140926         //?Se non arriva da gestione disposizioni di consegna
340200140926         //?Verifico se ci sono disposizioni di consegna da gestire
340300141029           clear FNLRY09DS2;
340400140926           ILRY09tch = 'C';
340500140926           ILRY09aas = %dec(WRKann:4:0);
340600140926           ILRY09lnp = %dec(WRKlnp:3:0);
340700140926           ILRY09nrs = %dec(WRKnrs:2:0);
340800140926           ILRY09nsp = %dec(WRKnsp:7:0);
340900141029           ILRY09df12 = 'S';
341000141029           ILRY09df21 = 'N';
341100141029           ILRY09df06 = 'S';
341200141029           ILRY09ef12 = 'S';
341300141029           ILRY09ef21 = 'S';
341400141029           ILRY09ef06 = 'S';
341500141029           kpjbu = FNLRY09DS2;
341600141029           fnlry09r (kpjba);
341700141029           FNLRY09DS2 = kpjbu;
341800140929           IF  OLRY09esit = '0' or OLRY09err <> *blanks;
341900140926             leavesr;
342000140926           ENDIF;
342100140926
342200140926         //?Se ho trovato delle disposizioni di consegna ancora da gestire
342300140926         //?verifico se l'utente è abilitato alla manutenzione della bolla
342400140926           IF  not %open(FNARB01L);
342500140926             open FNARB01L;
342600140926           ENDIF;
342700160104           chain (ILRY09aas:ILRY09lnp:ILRY09nrs:ILRY09nsp) FNARB000;
342800170228
342900170228           IF  not %found(FNARB01L);
343000170228             if  not %open(FNBLP01L);
343100170228               open  FNBLP01L;
343200170228             endif;
343300170228             chain (ILRY09aas:ILRY09lnp:ILRY09nrs:ILRY09nsp) FNBLP000;
343400170228           ENDIF;
343500170228
343600170228           Select;
343700170228             // -?Bolla NON trovata (né in ARB né in BLP)?
343800170228             When  not %found(FNARB01L)   and
343900170228                   not %found(FNBLP01L);
344000170228               leavesr;
344100170228             // -?Bolla trovata ARB, ma utente NON abilitato alla LNA?
344200170228             When  %found(FNARB01L)  and
344300170228                   %lookup( ARBlna : sk_POg1 ) = 0;
344400170228               leavesr;
344500170228             // -?Bolla trovata BLP, ma utente NON abilitato alla LNA?
344600170228             When  Not %found(FNARB01L)  and
344700170228                   %found(FNBLP01L)      and
344800170228                   %lookup( BLPlna : sk_POg1 ) = 0;
344900170228               leavesr;
345000170228             // -?La bolla dev'essere chiusa in partenza per?
345100170228             //  ?merce MAI affidata?
345200170228             When  Not %found(FNARB01L)  and
345300170228                   %found(FNBLP01L)      and
345400170228                   ( BLPcca  = *blanks   or
345500170228                     BLPft2 <> 'N' );
345600170228               leavesr;
345700170228             When  %found(FNARB01L)      and
345800170228                   ( ARBcca  = *blanks   or
345900170228                     ARBft2 <> 'N' );
346000170228               leavesr;
346100170228           EndSl;
346200140926
346300140926         //?Se arrivo qua vuol dire che posso abilitare F19
346400140926           *in19 = *on;
346500140926         ENDIF;
346600140926
346700140926       ENDSR;
346800140926
346900140926       //--------------------------------------------------------------
347000140926       //?Eseguo F19=Disposizioni di Consegna.
347100140926       //--------------------------------------------------------------
347200140926       BEGSR sr_F19;
347300140926
347400140926       //?Richiamo pgm gestione disposizioni di consegna
347500141029         clear FNLRY09DS2;
347600140926         ILRY09tch = 'E';
347700140926         ILRY09aas = %dec(WRKann:4:0);
347800140926         ILRY09lnp = %dec(WRKlnp:3:0);
347900140926         ILRY09nrs = %dec(WRKnrs:2:0);
348000140926         ILRY09nsp = %dec(WRKnsp:7:0);
348100141029         ILRY09df12 = 'S';
348200141029         ILRY09df21 = 'N';
348300141029         ILRY09df06 = 'S';
348400141029         ILRY09ef12 = 'S';
348500141029         ILRY09ef21 = 'S';
348600141029         ILRY09ef06 = 'S';
348700141029         kpjbu = FNLRY09DS2;
348800141029         fnlry09r (kpjba);
348900141029         FNLRY09DS2 = kpjbu;
349000140926
349100140926       //?Ricarico il subfile
349200140926         exsr sr_carsbf;
349300140926
349400140926       //?Ricontrollo se abilitare o meno F19
349500140926         exsr AbiF19;
349600140926
349700140926       ENDSR;
349800140926
349900140926      /end-free
350000140926
350100040225      *---------------------------------------------------------------*
350200040225      * Routine Iniziale                                              *
350300040225      *---------------------------------------------------------------*
350400040225     c     *InzSR        BegSR
350500040225      *
350600040225     c     *Entry        plist
350700040225     c                   parm                    KPJBA
350800040225     c                   parm                    FIDNA2DS
350900040225      * Reperisco data corrente
351000040225     c                   time                    w0140
351100040225     c                   move      w0140         wData
351200040225     c                   clear                   WLBdat
351300040225     c                   eval      G02dat = wData
351400040225     c                   call      'XSRDA8'
351500040225     c                   parm                    WLBdat
351600040225     c                   z-add     G02inv        DateU
351700040225      *
351800040225      * Reperisco dati job
351900040225     c                   exsr      DatiJob
352000040225     c                   movel     SDSpgm        VTCpgm
352100160202
352200160202      * Se sono in Sede accendo 14
352300160202     c                   eval      *in14 = ( SIMfel = 0 )
352400160202
352500130806      * Imposto le abilitazioni dell'utente
352600130806     c                   eval      dute01 = UTEfaf
352700040617      * se utente partricolare recupero P.O. utenete da tabella UGR
352800040617     c                   movel     KNMUS         w_ute
352900040617     c                   clear                   tibs02ds
353000040617     C                   MOVEL     'C'           t02mod
353100040617     C                   MOVEL     knsif         t02sif
353200040617     C                   MOVEL     'UGR'         t02cod
353300040617     C                   MOVEL     W_ute         t02ke1
353400040617      *
353500040617     C                   CALL      'TIBS02R'
353600040617     C                   PARM                    KPJBA
353700040617     C                   PARM                    TIBS02DS
353800040617      *
353900040617     C                   IF        t02err = *BLANKS
354000040617      *
354100040617     c                   movel     t02uni        DUGR
354200040617      * se P.O. da forzare diverso da zero imposto in automatico
354300040617     c                   if        §UGRpofo > 0
354400040617     c                   eval      Dutpou = §UGRpofo
354500040617     c                   endif
354600040617      *
354700040617     c                   endif
354800041012      * RECUPERO I DATI STANDARD
354900041012     c                   clear                   tibs02ds
355000041012     C                   MOVEL     'C'           t02mod
355100041012     C                   MOVEL     knsif         t02sif
355200041012     C                   MOVEL     'REC'         t02cod
355300041012     C                   MOVEL     '1'           t02ke1
355400041012      *
355500041012     C                   CALL      'TIBS02R'
355600041012     C                   PARM                    KPJBA
355700041012     C                   PARM                    TIBS02DS
355800041012      *
355900041012     C                   IF        t02err = *BLANKS
356000041012     c                   movel     t02uni        DREC
356100041012     c                   endif
356200041012      * verifico se sono un utente che può modficare tutto senza controlli
356300130806     c                   if        §UTEvrac = 'S'
356400041012     c                   eval      *in07 = *on
356500041012     c                   endif
356600140926
356700140926      /free
356800150317
356900150317       //?Imposto abilitazione manutenzione bolle di filiale
357000140926       //?- se non c'è l'abilitazione
357100140926       //?  imposto abilitazione al terminal se utente di primo livello
357200140926       //?  imposto abilitazione alla filiale se utente di secondo livello
357300150428         clear  wAbi;
357400140926         IF  UTEaut = *blanks;
357500140926           IF  DUTlpo = '1';
357600140926             wabi = 'TP';
357700140926           ENDIF;
357800140926           IF  DUTlpo = '2';
357900140926             wabi = 'PO';
358000140926           ENDIF;
358100140926       //?- se c'è l'abilitazione la imposto
358200150428         ELSE;
358300140926         //?se presente imposto quella specifica delle bolle
358400140926           IF  §UTEmbf <> *blanks;
358500140926             wabi = §UTEmbf;
358600140926           ELSE;
358700140926         //?se non presente imposto quella generica del profilo
358800140926             wAbi = UTEaut;
358900140926           ENDIF;
359000140926         ENDIF;
359100140926
359200140926       //?- Carico le filiali abiliate
359300140926         clear TRUL31DS;
359400140926         I31abi = wabi;
359500140926         I31cdi = DUTdis;
359600140926         I31car = DUTare;
359700140926         I31cpo = DUTpou;
359800140926         callp TRUL31R (kpjba:trul31ds);
359900140926         IF  O31pog <= *zeros;
360000140926           O31pog = *all'0';
360100140926         ENDIF;
360200150317
360300150317         sk_POg_ds1 = O31pog;
360400150317
360500150317         // -?Imposto abilitaz. variazione Richiesta Assistenza clienti?
360600150428         clear  wAbi;
360700150317         Select;
360800150317           // -?se presente imposto l'abilitazione specifica?
360900150317           When  §UTErac <> *blank;
361000150317             wAbi = §UTErac;
361100150317           // -?se non presente imposto quella generica del profilo?
361200150317           When  UTEaut  <> *blank;
361300150317             wAbi = UTEaut;
361400150317           // -?se non c'è l'abilitazione & utente di 1° livello?
361500150317           //  ?imposto qualla al Terminal?
361600150317           When  DUTlpo  =  '1';
361700150317             wAbi = 'TP';
361800150317           // -?se non c'è l'abilitazione & utente di 2° livello?
361900150317           //  ?imposto qualla alla Filiale?
362000150317           When  DUTlpo  =  '2';
362100150317             wAbi = 'PO';
362200150317         EndSl;
362300150317
362400150723         // -?Carico le filiali abilitate?
362500150723         clear  sk_POg2;
362600150723         If  SIMfel <> *zero;
362700150723           clear TRUL31DS;
362800150723           I31abi = wabi;
362900150723           I31cdi = DUTdis;
363000150723           I31car = DUTare;
363100150723           I31cpo = DUTpou;
363200150723           TRUL31R ( kpjba : trul31ds );
363300150723           if  O31pog <= *zeros;
363400150723             O31pog = *all'0';
363500150723           endif;
363600150723           sk_POg_ds2 = O31pog;
363700150723         Else;
363800150723           sk_POg2(1) = c_Sede;
363900150723         EndIf;
364000150910
364100150910         if  sk_POg_ds2 <= *zero;
364200150910           sk_POg2(1) = DUTpou;
364300150910         endif;
364400150428
364500150428         // -?Aggiungo dUGR.§UGRpofo se utente in tab. "UGR"?
364600150428         clear  dUGR;
364700150428         clear  TIBS02ds;
364800150428         T02mod = 'C';
364900150428         T02sif = KNSIF;
365000150428         T02cod = 'UGR';
365100150428         T02ke1 = %subst( KNMUS : 1 : 3 );
365200150428         TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
365300150428         If  T02err = *blank;
365400150428           dUGR = T02uni;
365500150428           if  §UGRpofo > *zero;
365600150428             ww = %lookup( *zero : sk_POg2 );
365700150428             if  ww > *zero;
365800150428               sk_POg2(ww) = §UGRpofo;
365900150428             endif;
366000150428           endif;
366100150428         EndIf;
366200150317
366300140926      /end-free
366400140926
366500040617      *
366600040225      * Se ricevuto parametro per manutenzione  non valorizzo il p.o. emittente con quello
366700040225      * in linea
366800040225     c                   if        parms   > 1
366900040225      * modalità RICHIAMATO
367000040225     c                   eval      wmod   = 'R'
367100040304     c                   eval      *in05 = (ia2mod = 'I')
367200040405     c                   eval      *in15 = (ia2mod <> 'I')
367300040225     c                   else
367400040225      * modalità DA MENU
367500040225     c                   eval      wmod   = ' '
367600040331      *
367700040331     c                   eval      v01ape = 'SI'
367800160202     c                   if        SIMfel <> *zero
367900160129     c                   eval      V01att = 'S'
368000160202     c                   endif
368100150313     c                   eval      V01pog = %editc( DUTpou : 'X' )
368200150617     c                   if        DUTlpo <> 'S'
368300150422     c                   eval      V01rpr = 'RAC'
368400150617     c                   endif
368500150313     c                   eval      v01rpg = knmus
368600040421     c                   move      w0140         v01ant
368700151022      * se utente di secondo livello non imposto reparto e utente
368800151022     c                   IF        DUTlpo = '2'
368900151022     c                   clear                   V01rpr
369000151022     c                   clear                   V01rpg
369100151022     c                   ENDIF
369200040331      *
369300040225     c                   endif
369400040225      *
369500040225     c                   EndSr
369600040225
369700040225      *---------------------------------------------------------------*
369800040225      * Reperimento Dati del job (Utente/Operativi)                   *
369900040225      *---------------------------------------------------------------*
370000040225     c     DatiJob       BegSr
370100040225      *
370200040225     c     *dtaara       define    §azute        azuteds
370300040225     c     *dtaara       define    §datiute      ddatiute
370400040225      *
370500040225     c                   in(E)     *dtaara
370600040225     c                   IF        %ERROR or RSUT = *blanks
370700040225     c                   clear                   Tibs34Ds
370800040225     c                   call      'TIBS34R'
370900040225     c                   parm                    Tibs34Ds
371000040225     c                   in        *dtaara
371100040225     c                   ENDIF
371200040225      *
371300040225     c                   EndSr
