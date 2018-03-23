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
025300170719
025400170719       // -?Schiera Filiali Gestione R.A. ("£6")?
025500170719     d sk_£6_ds        ds                  inz
025600170719     d   sk_£6                        3s 0 inz  overlay(sk_£6_ds : 1)
025700170719     d                                     dim(30)
025800040224
025900040224      *
026000040224      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
026100040224      *
026200040224      * - Campi di comodo
026300040224     d w0140           s             14  0
026400040224     d wData           s              8  0
026500040224     d DateU           s              8  0
026600140526     d ww              s              3  0 inz
026700040225     d wmod            s              1
026800040225     d Wddar           s              8  0
026900040225     d Wadar           s              8  0
027000040531     d Wddcr           s              8  0
027100040531     d Wadcr           s              8  0
027200040227     d s01nrr          s              4  0
027300140219     d S03nrr          s              4  0
027400040227     d Wrecok          s              1
027500040304     d Wlrsc           s              3  0
027600040505     d Wlrpg           s              3  0
027700040315     d W_f03           s              1
027800040331     d W_ute           s              3
027900040505     d W_rpggen        s              1
028000041215     d wtibs69r        s              1
028100040419     d SavIn05         s              1    inz(*off)
028200150323     d $Ord_Desc       s               n   inz(*on)
028300150506     d $InzSFL         s               n   inz(*on)
028400151022     d xx              s              4  0 inz
028500151022     d yy              s              4  0 inz
028600150610
028700150610     d wCountRA        s              5  0 inz
028800040225
028900040225     d W_pot           s                   like(orgfil)
029000040225     d W_pog           s                   like(orgfil)
029100040603     d W_por           s                   like(orgfil)
029200040227     d W_ogg           s                   like(tgdogg)
029300040505     d W_rpg           s                   like(v01rpg)
029400040505     d W_rpgmax        s                   like(v01rpg)
029500150603     d W1Togg          s                   like(V1Togg)   inz
029600040315     d v01tor          s                   like(tgdtor)
029700140926     d wabi            s                   like(UTEaut)
029800160201     d WrepR           s                   like(dTGR01a.§TGRrepR)  inz
029900160201     d WfilR           s                   like(dTGD01.§TGDfilR)   inz
030000150417
030100150417       // -?Campi per l'estrazione SQL dei dati per il subfile?
030200150417     d wSQL            s           7000    inz  varying
030300150417     d $First          s               n   inz(*on)
030400160129     d $First2         s               n   inz(*on)
030500140926
030600140926      // -?Campi per procedura esterna
030700140926     d  pStack         s           1000
030800140926     d  pStartPoint    s              3  0
030900140926     d  pEntries       s              3  0
031000140220
031100140220       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
031200140220
031300140220       //--------------------------------------------------------------
031400140220       // -?Constants?
031500140220       //--------------------------------------------------------------
031600140220       // -?MaxKey - Maximum number of key fields allowed by this program?
031700140220     d c_MaxKey        c                   const(3)
031800140220       // -?Sort order:?
031900140220       //  ?1 = Sort in an ascending sequence?
032000140220       //  ?2 = Sort in a descending sequence?
032100140220     d c_Ascendente    c                   const(1)
032200140220     d c_Discendente   c                   const(2)
032300140220       // -?Key data type:?
032400140220       //  ? 0 = Signed binary?
032500140220       //  ? 2 = Signed zoned decimal?
032600140220       //  ? 3 = Signed packed decimal?
032700140220       //  ? 6 = Character with no national language sort sequence applied,?
032800140220       //  ?     if specified?
032900140220       //  ? 7 = Unsigned packed decimal?
033000140220       //  ?     All numbers will have the sign forced positive ('F'X).?
033100140220       //  ? 8 = Unsigned zoned decimal?
033200140220       //  ?     All numbers will have the sign forced positive ('F'X).?
033300140220       //  ? 9 = Unsigned binary?
033400140220       //  ?14 = Date in form of DD/MM/YY?
033500140220       //  ?15 = Date in form of DD.MM.YYYY?
033600140220     d c_Numero        c                   const(2)
033700140220     d c_Carattere     c                   const(6)
033800140220     d c_NumIntero     c                   const(8)
033900140220       //
034000140220     d c_Put           c                   const(1)
034100140220     d c_EndPut        c                   const(2)
034200140220     d c_Get           c                   const(3)
034300140220
034400140220       //--------------------------------------------------------------
034500140220       // -?Data Structures?
034600140220       //  ?SflRcd     - The entire subfile record to pass to the sort?
034700140220       //  ?QLGSCB     - The sort request block for the QLGSORT API?
034800140220       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
034900140220       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
035000140220       //  ?QUSEC      - Error structure for the QLGSORT API?
035100140220       //--------------------------------------------------------------
035200150317     d SflRcd          ds                  inz
035300150325     d   VS1ant                                                                   1-  4: 4
035400150325     d   VS1nut                                                                   5- 13: 9
035500150325     d   VS1pdt                                                                  14- 16: 3
035600150325     d   VS1pot                                                                  17- 19: 3
035700150325     d   VS1ges                                                                  20- 20: 1
035800150325     d   VS1dar                                                                  21- 26: 6
035900150417     d   VS1rpr                                                                  27- 36: 10
036000150417     d   VS1rpg                                                                  37- 46: 10
036100150417     d   VS1pog                                                                  47- 49: 3
036200150417     d   VS1rsc                                                                  50- 77: 28
036300150417     d   VS1tch                                                                  78- 78: 1
036400150417     d   VSDmad                                                                  79- 96: 18
036500151221     d   HS1prt2                                                                 97- 97: 1
036600150417     d   HS1dac                                                                  98-105: 8
036700150417     d   VS1orc                                                                 106-109: 4
036800150417     d   VS1ogg                                                                 110-135: 26
036900150417     d   VS1cdt                                                                 136-138: 3
037000150417     d   VS2dcd                                                                 139-146: 8
037100150417     d   VS1no1                                                                 147-181: 35
037200150417     d   VS1no2                                                                 182-216: 35
037300150325     d*//VS1nrr                                                                 -------: 13
037400150417     d   VSHin06                                                                217-217: 1
037500150417     d   VS1opz                                                                 218-218: 1
037600150417     d   VSDmad2                                                                219-247: 29
037700150417     d   VS1rsc2                                                                248-268: 21
037800150417     d   VS1txt1                                                                269-343: 75
037900150417     d   VS1txt2                                                                344-418: 75
038000151221     d   VS1prt                                                                 419-419: 1
038100151221     d   DspAtrPRT                                                              420-420: 1
038200151221     d   DspAtrDAR                                                              421-421: 1
038300151221     d   DspAtrRSC                                                              422-422: 1
038400140220     d   Selected                     1a   inz
038500140220      /copy QSYSINC/QRPGLESRC,QLGSORT
038600140220     d QLGKL                         16    dim(c_MaxKey)
038700140220     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
038800140220     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
038900140220     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
039000140220     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
039100140220      /copy QSYSINC/QRPGLESRC,QLGSRTIO
039200140220      /copy QSYSINC/QRPGLESRC,QUSEC
039300140220
039400140220       //--------------------------------------------------------------
039500140220       // -?Standalone fields?
039600140220       //  ?Nrr        - Relative record number for subfile?
039700140220       //  ?SizeList   - Size of list?
039800140220       //  ?ReturnSize - Size of list returned by sort API?
039900140220       //--------------------------------------------------------------
040000140220     d NotUsed         s             16a   inz
040100170228     d ReturnSize      s             10i 0 inz
040200170228     d SizeList        s             10i 0 inz
040300140220     d RrnLast         s              5i 0 inz
040400140220     d Nrr             s              5i 0 inz
040500140220
040600140220       //--------------------------------------------------------------
040700140220       //?Definizione prototipi procedure usate.                       ?
040800140220       //--------------------------------------------------------------
040900140220
041000140220       // -?Ordinamento subfile?
041100140220      /copy gaitrasrc/srcProtoPR,QLGSRTIO
041200140925
041300140925       // -?Reperimento Disposizioni di consegna da WEB
041400140925     d fnlry09r        pr                  extpgm('FNLRY09R')
041500141013     d  kpjba                              likeds(kpjba)
041600140926
041700140926       // -?Carica filiali abilitate
041800140926      /copy gaitrasrc/srcprotopr,trul31r
041900170719
042000170719       // -?Caricamento schiera Filiali?
042100170719     d TRUL06ds      e ds                  inz
042200170719     d   D06cod      e                     inz('£6')
042300170719      /copy gaitrasrc/srcProtoPR,TRUL06R
042400150428
042500150428       // -?Ricerca/Controllo tabelle (TNTBE)?
042600150428      /copy gaitrasrc/srcProtoPR,TIBS02R
042700150508
042800150508       // -?Selezione tab. "RGR" = Reparto Gestione R.A.?
042900150508     d tntbRGRds     e ds                  inz
043000150508     d tntbRGRr        pr                  extpgm('TNTBRGRR')
043100150508     d   kpjba                             likeds(kpjba)
043200140926
043300140926       // -?Pgm per ricerca Programmi chiamati
043400140926     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
043500140926     d  pStack                     1000
043600140926     d  pStartPoint                   3  0
043700140926     d  pEntries                      3  0
043800140220
043900040224      *
044000070601      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
044100040224      *
044200040224
044300040224      *
044400040224      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
044500040224      *
044600040301      * - file FITGD01L
044700040421     c     ktgdan        klist
044800040421     c                   kfld                    v01ant
044900040421     c                   kfld                    v01nut
045000040421      *
045100040301     c     ktgd01        klist
045200040421     c                   kfld                    vs1ant
045300040301     c                   kfld                    vs1nut
045400040301     c                   kfld                    vs1pdt
045500150324      * - file FITGD32L
045600150324     c     ktgd02        klist
045700040226     c                   kfld                    ia2tor
045800040226     c                   kfld                    ia2ogg
045900040226
046000040226     c     ktgdv2        klist
046100040226     c                   kfld                    v01tor
046200040226     c                   kfld                    W_ogg
046300040421      * - file FITGR01L
046400040421     c     ktgr          klist
046500040421     c                   kfld                    vs1ant
046600040421     c                   kfld                    vs1nut
046700040419      * - file FNORM01L
046800040419     c     k04orm01      klist
046900040419     c                   kfld                    V02poe
047000040419     c                   kfld                    V02nso
047100040419     c                   kfld                    V02orm
047200040419     c                   kfld                    V02nrv
047300040226      *
047400040224
047500040224      *===============================================================*
047600040224      *  RIEPILOGO INDICATORI                                         *
047700040224      *---------------------------------------------------------------*
047800040224      *  01    - Visualizza numero spedizione                         *
047900040224      *  02    - Visualizza numero Orm                                *
048000040224      *  03    - Visualizza chiave generica                           *
048100040301      *  04    - Visualizza dati relativa alla chiusura               *
048200040419      *  05    - Pgm richiamato in modalità interrogazione            *
048300040304      *  06    - Progressivo chiuso condiziona campi in SFL           *
048400041012      *  07    - Utente abilitato a modificare tutto                  *
048500140224      *  08    - Più di un rec. nel subfile S03 (per F11=Ordinamento) *
048600150506      *  09    - Visualizza F5=Refresh per S02/C02                    *
048700150325      *  10    - Posizionamento cursore su Rag.Soc. Chiamante         *
048800150626      *  12    - Comodo                                               *
048900160202      *  14    - Sono in Sede                                         *
049000040405      *  15    - Pgm richiamato                                       *
049100150319      *  16    - Abilita F11=Variazioni in D02                        *
049200140926      *  19    - Abilita F19-Disposizioni di Consegna                 *
049300040224      *  20    - SFLDSPCTL in C01                                     *
049400040224      *  21    - SFLDSP    in C01                                     *
049500040224      *  21    - SFLEND    in C01                                     *
049600040224      *  22    - SFLCLR    in C01                                     *
049700040301      *  29    - SFLCLR    in C01                                     *
049800040226      *  30/49 - Errori campi a video                                 *
049900150325      *  50    - Posizionamento cursore su Stato R.A.                 *
050000150325      *  51    - Posizionamento cursore su Filiale Apertura           *
050100150325      *  52    - Posizionamento cursore su Filiale Gestione           *
050200150325      *  53    - Posizionamento cursore su Tipo Comunicazione         *
050300040315      *  54    - ERRORE tipo oggetto                                  *
050400150325      *  55    - Posizionamento cursore su Motivo Apertura            *
050500150325      *  56    - Posizionamento cursore su Codice Chiusura            *
050600040331      *  57/61 - Errori campi a video                                 *
050700150325      *  62    - Posizionamento cursore su Utente Gestione            *
050800040407      *  63/65 - Errore campi a video                                 *
050900150910      *  68    - "Vecchio" errore: tolto a *PGM ma lasciato nel *DSPF *
051000140217      *  69/71 - Errore su opzione                                    *
051100150626      *  72/80 - Errori campi a video                                 *
051200150325      *  81    - Posizionamento cursore su Filiale Responsabile       *
051300040224      *  90    - Errore                                               *
051400040224      *===============================================================*
051500040224
051600040224     c                   do        *hival
051700140221
051800140225     c                   if        Not *in90
051900140221     c                   clear                   V1Togg
052000140221     c                   write     fia2t01
052100140225     c                   endif
052200040225
052300040225      * se sono stato richiamato salto questa videata
052400040225
052500040225     c                   if        wmod = ' '
052600040315     c                   setoff                                       90
052700040225     c                   exfmt     fia2d01
052800040315
052900040331     c                   setoff                                       506210
053000040331     c                   setoff                                       515256
053100040315     c                   setoff                                       535455
053200040531     c                   setoff                                       81
053300150312
053400150312sel 3c                   Select
053500040401
053600040401      * F3 = Fine
053700040401
053800150312     c                   When      *inkc
053900040401     c                   leave
054000040225
054100040401      * F10 = Immissione
054200040225
054300150312w   3c                   When      *inkj
054400161027     c                   call      'FIDNA1R'
054500040401     c                   parm                    kpjba
054600040401     c                   iter
054700041215
054800150610      * F1=Interrogazione piano dei conti
054900041215
055000150610w   3c                   When      *inKA
055100041215     c                   exsr      SR_ricalf
055200041215     c                   iter
055300150312
055400150312e   3c                   EndSl
055500041215
055600040225
055700040225      * Controllo video selezioni
055800040225
055900040225     c                   exsr      SR_contr1
056000040225
056100040225     c                   endif
056200040225
056300040225      * se non ci sono errori emetto il subfile
056400040225
056500040225     c                   if        not *in90
056600040227     c                   exsr      SR_GESSBF
056700040225     c                   endif
056800040225
056900040225      * al ritorno dal subfile se sono stato richiamato vado a fine PGM
057000040225
057100040225     c                   if        Wmod = 'R'
057200040225     c                   leave
057300040225     c                   endif
057400040225
057500040225     c                   enddo
057600041215
057700041215     C                   IF        Wtibs69r  <>  *BLANKS
057800041215     C                   eval      I69TLA  = 'C'
057900041215     C                   call      'TIBS69R'
058000041215     C                   parm                    TIBS69DS
058100041215     C                   ENDIF
058200040225
058300040225     c                   seton                                        LR
058400040225      *---------------------------------------------------------------*
058500040225      * Routine Controllo videata selezione                           *
058600040225      *---------------------------------------------------------------*
058700040227     c     SR_contr1     BegSR
058800040225      *
058900040225
059000150319      *?Filiale in gestione?
059100150319
059200150319     C                   clear                   vd1pog
059300150319    2c                   If        v01pog <> *blanks
059400150319      *
059500150319      * Ricerca
059600150319     c     '?'           scan      v01pog
059700150319    3c                   if        %found
059800150319     C                   clear                   dstnsd
059900150319     C                   call      'TNSD24R'
060000150319     C                   parm                    §cod
060100150319     C                   parm                    §tip
060200150319     C                   parm                    §des
060300150319     C                   parm                    §pra
060400150319     C                   movel     §cod          v01pog
060500150319     C                   movel     §des          vd1pog
060600150319     c                   eval      *in90 = *on
060700150319     C                   eval      *IN52 = *on
060800150319     c                   leavesr
060900150319    3C                   endif
061000150319    2C                   endif
061100150319      *
061200150319    2C     v01pog        ifeq      *blanks
061300150319     C                   move      *all'0'       v01pog
061400150319    2C                   endif
061500150319      *
061600150626     C                   testn                   v01pog               12
061700150319     C                   move      v01pog        w001a             1
061800150319      *
061900150626    2C     *in12         ifeq      *off
062000150319     C     w001a         orlt      '0'
062100150319     c                   eval      *in90 = *on
062200150319     C                   eval      *IN44 = *on
062300150319     c                   leavesr
062400150319    2C                   endif
062500160219      *
062600160219     c                   move      v01pog        W_pog
062700150319      * verifico se P.O. esistente
062800151023    2c                   if        W_pog > 0
062900151023     c                   IF        w_pog = 999 and simfel = 0
063000151023     c                   eval      *in90 = *on
063100151023     C                   eval      *IN44 = *on
063200151023     c                   leavesr
063300151023    3c                   endif
063400151023     c                   IF        w_pog <> 999
063500160104     C     W_pog         chain     azorg
063600150319    3C                   if        %found(azorg01l)
063700150319     C                   movel     orgdes        vd1pog
063800150319      *  errore
063900150319     C                   ELSE
064000150319     c                   eval      *in90 = *on
064100150319     C                   eval      *IN44 = *on
064200150319     c                   leavesr
064300150319    3c                   endif
064400151023     c                   ENDIF
064500170719      *
064600170719      /free
064700170719           // -?Caricamento "£6" della filiale-gestione (se inserita)?
064800170719           reset  TRUL06ds;
064900170719           if  w_POG > *zero  and  w_POG < 999;
065000170719             //*·D06cod   =  '£6'        //?(già così)?
065100170719             D06key   =  %editc( w_POG : 'X' );
065200170719             KPJBU    =  TRUL06ds;
065300170719             TRUL06R ( KPJBA );
065400170719             TRUL06ds = KPJBU;
065500170719             sk_£6_ds = D06lia;
065600170719           endif;
065700170719      /end-free
065800150319      *
065900150319    2c                   endif
066000150417      *
066100150417      *?Reparto gestione?
066200150417      *
066300150417     c                   Select
066400150417      * -?Vuoto?
066500150417     c                   When      V01rpr = *blank
066600150417      * -?Interrogazione?
066700150417     c                   When      %scan( '?' : V01rpr ) > *zero
066800150417     c                   clear                   V01rpr
066900150508     c                   clear                   tntbRGRds
067000150508     c                   eval      iRGRopz = 1
067100150508     c                   eval      kpjbu   = tntbRGRds
067200150508     c                   call      'TNTBRGRR'
067300150420     c                   parm                    KPJBA
067400150508     c                   eval      tntbRGRds = kpjbu
067500150508    4c                   If        oRGRerr <> *on  and
067600150508     c                             oRGRfxx =  *blank
067700150508     c                   movel     oRGRcod       V01rpr
067800150420     c                   Else
067900150420     c                   clear                   V01rpr
068000150420    4c                   EndIf
068100150417     c                   eval      *in90 = *on
068200150417     c                   leavesr
068300150417      * -?Controllo?
068400150417     c                   Other
068500150420     c                   clear                   TIBS02ds
068600150420     c                   eval      T02mod = 'C'
068700150420     c                   eval      T02sif = KNSIF
068800150420     c                   eval      T02cod = 'RGR'
068900150420     c                   eval      T02ke1 = V01rpr
069000150420     c                   call      'TIBS02R'
069100150420     c                   parm                    KPJBA
069200150420     c                   parm                    TIBS02ds
069300150423    4c                   If        T02err <> *blanks
069400150420     c                   eval      *in90 = *on
069500150420     c                   eval      *in67 = *on
069600150420     c                   leavesr
069700150420    4c                   EndIf
069800150417     c                   EndSl
069900150319      *
070000150319      *?Utente in gestione?
070100150319      *
070200150319     c                   eval      W_rpggen = ' '
070300150319     c                   clear                   W_rpg
070400150319     c                   eval      W_rpgmax = *all'9'
070500150319     c                   clear                   Wlrpg
070600150319    2c                   If         v01rpg <> *blanks
070700150319      * verifico se richiesto utente in gestione generico controllo    se inserito "*"
070800150319     c                   eval      Wlrpg = %scan('*':V01rpg)
070900150319     c                   if        Wlrpg > 0
071000150319     c                   sub       1             Wlrpg
071100150319      * flag di ricerca generica utente
071200150319     c                   eval      W_rpggen = 'Y'
071300150319      * campo chiave per lettura file per utente
071400150319     c                   eval      W_rpg = %subst(v01rpg:1:Wlrpg)
071500150319     c                   eval      %subst(W_rpgmax:1:Wlrpg) =
071600150319     c                              %subst(v01rpg:1:Wlrpg)
071700150319     c                   endif
071800150319      * se non è stata richiesta la ricerca parziale
071900150319   3ac                   if        W_rpggen = ' '
072000160104     c     v01rpg        chain     azute000
072100150319      *
072200150319    4c                   if        not %found(azute01l)
072300150319     c                   eval      *in90 = *on
072400150319     C                   eval      *IN63 = *on
072500150319     c                   leavesr
072600150319    4C                   ENDIF
072700150319      *
072800150319   3ac                   endif
072900150319      *
073000150319    2C                   endif
073100150319
073200150324      *?Stato (ex Priorità) reclamo?
073300150319
073400150319     c                   clear                   VD1prt
073500150319    2c                   If        v01prt <> *blanks
073600150319     c                   clear                   tibs02ds
073700150319      *
073800150319      * Ricerca
073900150319    3c                   if        v01prt = '?'
074000160118     c*//                clear                   tibs02ds
074100160118     C*//                MOVEL     'R'           t02mod
074200160118     C*//                MOVEL     knsif         t02sif
074300160118     C*//                MOVEL     'PRR'         t02cod
074400150319      *
074500160118     C*//                CALL      'TIBS02R'
074600160118     C*//                PARM                    KPJBA
074700160118     C*//                PARM                    TIBS02DS
074800150319      *
074900160118    4C*//                IF        T02err = *BLANKS
075000160118     C*//                movel     T02uni        DPRR
075100160118     C*//                movel     T02ke1        v01prt
075200160118     C*//                movel     §PRRdesc      vd1prt
075300160118     c                   clear                   tntbPRRds
075400160118     c                   eval      iPRRopz = '1'
075500160118     c                   movel(P)  tntbPRRds     KPJBU
075600160118     c                   call      'TNTBPRRR'
075700160118     c                   parm                    KPJBA
075800160118     c                   movel     KPJBU         tntbPRRds
075900160118      *
076000160118     c                   IF        oPRRfxx = *blanks  and
076100160118     c                             oPRRerr = *off
076200160118     c                   movel     oPRRcod       V01prt
076300160118     C                   movel     oPRRdes       VD1prt
076400150319      *  errore (non trovato nulla pulisco il '?' )
076500150319     C                   ELSE
076600150319     c                   clear                   V01prt
076700150319    4C                   ENDIF
076800150319      *
076900150319     c                   eval      *in90 = *on
077000150325     C                   eval      *in50 = *on
077100150319     c                   leavesr
077200150319      *
077300150319   x3C                   ELSE
077400150319      *
077500150319      * Controllo
077600150319     c                   clear                   tibs02ds
077700150319     C                   MOVEL     'C'           t02mod
077800150319     C                   MOVEL     knsif         t02sif
077900150319     C                   MOVEL     'PRR'         t02cod
078000150319     C                   MOVEL     v01PRT        t02ke1
078100150319      *
078200150319     C                   CALL      'TIBS02R'
078300150319     C                   PARM                    KPJBA
078400150319     C                   PARM                    TIBS02DS
078500150319      *
078600150319    4C                   IF        t02err = *BLANKS
078700150319     C                   movel     T02uni        DPRR
078800150319     C                   movel     §PRRdesc      vd1prt
078900150319      *
079000150319     C                   ELSE
079100150319     c                   eval      *in90 = *on
079200150319     C                   eval      *IN59 = *on
079300150319     c                   leavesr
079400150319    4C                   ENDIF
079500150319      *
079600150319    3c                   endif
079700150319      *
079800150319    2C                   endif
079900150319
080000150319      *?Data Chiamata DAL?
080100150319
080200150319     C                   clear                   WDDAR
080300150319    2c                   If        vd1dar > 0
080400150319     C                   clear                   wlbdat
080500150319     C                   z-add     vd1dar        G02dat
080600150319     C                   call      'XSRDA8'
080700150319     C                   parm                    wlbdat
080800150319    3C                   if        G02err = '1'
080900150319     c                   eval      *in90 = *on
081000150319     C                   eval      *IN41 = *on
081100150319     C                   leavesr
081200150319     C                   else
081300150319     C                   z-add     G02dat        vd1dar
081400150319     C                   z-add     G02inv        Wddar
081500150319    3C                   endif
081600150319
081700150319    2C                   endif
081800150319
081900150319      *?Data Chiamata AL?
082000150319
082100150319     C                   clear                   WADAR
082200150319    2c                   If        va1dar > 0
082300150319     C                   clear                   wlbdat
082400150319     C                   z-add     va1dar        G02dat
082500150319     C                   call      'XSRDA8'
082600150319     C                   parm                    wlbdat
082700150319    3C                   if        G02err = '1'
082800150319     c                   eval      *in90 = *on
082900150319     C                   eval      *IN42 = *on
083000150319     C                   leavesr
083100150319     C                   else
083200150319     C                   z-add     G02dat        va1dar
083300150319     C                   z-add     G02inv        Wadar
083400150319    3C                   endif
083500150319
083600150319    2C                   endif
083700150319      * se data dal è valorizzata e data al no imposto la stessa data anche in data al
083800150319    2c                   If        va1dar = 0 and vd1dar > 0
083900150319     c                   eval      va1dar = vd1dar
084000150319     c                   eval      wadar = wddar
084100150319    2c                   endif
084200150319      * verifico se data al è maggiore o uguale di data dal
084300150319    2c                   if        wadar < wddar
084400150319     c                   eval      *in90 = *on
084500150319     C                   eval      *IN43 = *on
084600150319     C                   leavesr
084700150319    2c                   endif
084800150319
084900150319      *?Data Chiusura DAL?
085000150319
085100150319     C                   clear                   WDDCR
085200150319    2c                   If        vd1dcr > 0
085300150319     C                   clear                   wlbdat
085400150319     C                   z-add     vd1dcr        G02dat
085500150319     C                   call      'XSRDA8'
085600150319     C                   parm                    wlbdat
085700150319    3C                   if        G02err = '1'
085800150319     c                   eval      *in90 = *on
085900150319     C                   eval      *IN72 = *on
086000150319     C                   leavesr
086100150319     C                   else
086200150319     C                   z-add     G02dat        vd1dcr
086300150319     C                   z-add     G02inv        Wddcr
086400150319    3C                   endif
086500150319
086600150319    2C                   endif
086700150319
086800150319      *?Data Chiusura AL?
086900150319
087000150319     C                   clear                   WADCR
087100150319    2c                   If        va1dcr > 0
087200150319     C                   clear                   wlbdat
087300150319     C                   z-add     va1dcr        G02dat
087400150319     C                   call      'XSRDA8'
087500150319     C                   parm                    wlbdat
087600150319    3C                   if        G02err = '1'
087700150319     c                   eval      *in90 = *on
087800150319     C                   eval      *IN74 = *on
087900150319     C                   leavesr
088000150319     C                   else
088100150319     C                   z-add     G02dat        va1dcr
088200150319     C                   z-add     G02inv        Wadcr
088300150319    3C                   endif
088400150319
088500150319    2C                   endif
088600150319      * se data dal è valorizzata e data al no imposto la stessa data anche in data al
088700150319    2c                   If        va1dcr = 0 and vd1dcr > 0
088800150319     c                   eval      va1dcr = vd1dcr
088900150319     c                   eval      wadcr = wddcr
089000150319    2c                   endif
089100150319      * verifico se data al è maggiore o uguale di data dal
089200150319    2c                   if        wadcr < wddcr
089300150319     c                   eval      *in90 = *on
089400150319     C                   eval      *IN75 = *on
089500150319     C                   leavesr
089600150319    2c                   endif
089700150319      *
089800150319      *?se non sono stati richiesti i solo reclami aperti?
089900150319      *   ?non inserire date chiusura?
090000150319      *
090100160120    2c                   If        (v01ape = 'SI' or V01att = 'S')
090200150319     c                             and wddcr > 0
090300150319     c                   eval      *in90 = *on
090400160120     c                   if        V01ape  = 'SI'
090500150319     C                   eval      *IN73 = *on
090600160120     c                   else
090700160120     c                   eval      *in46 = *on
090800160120     c                   endif
090900150319     c                   leavesr
091000150319    2c                   endif
091100150319      *
091200150319      *?se selezionato chiamante solo per la parte relativa al contenuto?
091300150319      *   ?deve esistere?
091400150319      *
091500150320    2c                   If        v01rpc = 'X' and v01rsc = *blank
091600150319     c                   eval      *in90 = *on
091700150319     C                   eval      *IN58 = *on
091800150319     c                   leavesr
091900150319     c                   endif
092000150319      *
092100150319    2c                   If        v01rsc <> *blank
092200150319     c                   clear                   Wlrsc
092300150319     c     ' '           checkr    V01rsc        Wlrsc
092400150319    2c                   endif
092500150319
092600150319      *?Codice Cliente?
092700150319      *?(recupero categoria del cliente)?
092800150319
092900150319     c                   if        v01ksc > 0
093000150319
093100150319      * aggancio anagrafico
093200150319
093300150319     C                   clear                   TIBS69DS
093400150319     c                   z-add     v01ksc        I69kac
093500150319     C                   call      'TIBS69R'
093600150319     C                   parm                    tibs69DS
093700150319     C                   parm                    DS_cnaco
093800150319     C                   parm                    DS_cnind
093900150319     C                   parm                    DS_cnclp
094000150319     C                   parm                    DS_fncls
094100150319     C                   eval      wtibs69r = '1'
094200150319     c                   If         O69ERR =  '1'
094300150319     C                   eval      *IN90 = *on
094400150319     C                   eval      *IN66 = *on
094500150319     c                   leavesr
094600150319     c                   Else
094700150319     c                   movel     ACOrag        vd1ksc
094800150319     c                   endif
094900150319
095000150319     c                   Endif
095100150319
095200150319      * Controllo la ragione sociale del cliente
095300150319
095400150319     c                   if        vd1ksc <> *blanks and v01ksc = *zeros
095500150319     C                   eval      *IN90 = *on
095600150319     C                   eval      *IN23 = *on
095700150319     c                   leavesr
095800150319     c                   endif
095900150319
096000150319      *?Motivo apertura reclamo?
096100150319
096200150319     c                   clear                   VD1mad
096300150319    2c                   If        v01mad <> *blanks
096400150319     c                   clear                   tibs02ds
096500150319      *
096600150319      * Ricerca
096700150319     c     '?'           scan      v01mad
096800150319    3c                   if        %found
096900150319     c                   clear                   tntbMARds1
097000150319     c                   movel(p)  tntbMARds1    KPJBU
097100150319      *
097200150319     c                   call      'TNTBMARR1'
097300150319     c                   parm                    KPJBA
097400150319      *
097500150319     c                   movel     KPJBU         tntbMARds1
097600150319    4c                   IF        oMARfxx = *blank  and  oMARerr = *off
097700150319     c                   eval      V01mad  = oMARcod
097800150319     C                   eval      VD1mad  = oMARdes
097900150319      *  errore (non trovato nulla pulisco il '?' )
098000150319     C                   ELSE
098100150319     c                   clear                   V01mad
098200150319    4C                   ENDIF
098300150319      *
098400150319     c                   eval      *in90 = *on
098500150325     C                   eval      *in55 = *on
098600150319     c                   leavesr
098700150319      *
098800150319   x3C                   ELSE
098900150319      *
099000150319      * Controllo
099100150319     c                   clear                   tibs02ds
099200150319     C                   MOVEL     'C'           t02mod
099300150319     C                   MOVEL     knsif         t02sif
099400150319     C                   MOVEL     'MAR'         t02cod
099500150319     C                   MOVEL     v01mad        t02ke1
099600150319      *
099700150319     C                   CALL      'TIBS02R'
099800150319     C                   PARM                    KPJBA
099900150319     C                   PARM                    TIBS02DS
100000150319      *
100100150319    4C                   IF        t02err = *BLANKS
100200150319     C                   movel     T02uni        DMAR
100300150319     C                   movel     §MARdesc      vd1mad
100400150319      *
100500150319     C                   ELSE
100600150319     c                   eval      *in90 = *on
100700150319     C                   eval      *IN47 = *on
100800150319     c                   leavesr
100900150319    4C                   ENDIF
101000150319      *
101100150319    3c                   endif
101200150319      *
101300150319    2C                   endif
101400150319
101500150319      *?CODICE di CHIUSURA del reclamo?
101600150319
101700150319     c                   clear                   v1dcdt
101800150319    2c                   If        v01cdt <> *blanks
101900150319     c                   clear                   tibs02ds
102000150319      *
102100150319      * Ricerca
102200150319     c     '?'           scan      v01cdt
102300150319    3c                   if        %found
102400150319     c                   clear                   tibs02ds
102500150319     C                   MOVEL     'R'           t02mod
102600150319     C                   MOVEL     knsif         t02sif
102700150319     C                   MOVEL     'CHR'         t02cod
102800150319      *
102900150319     C                   CALL      'TIBS02R'
103000150319     C                   PARM                    KPJBA
103100150319     C                   PARM                    TIBS02DS
103200150319      *
103300150319    4C                   IF        T02err = *BLANKS
103400150319     C                   movel     T02uni        DCHR
103500150319     C                   movel     T02ke1        v01cdt
103600150319     C                   movel     §chrdesc      v1dcdt
103700150319      *  errore (non è stato selezionato nulla pulisco  il campo e mi posiziono)
103800150319     C                   ELSE
103900150319     C                   clear                   v01cdt
104000150319     C                   clear                   v1dcdt
104100150319    4C                   ENDIF
104200150319      *
104300150319     c                   eval      *in90 = *on
104400150319     C                   eval      *IN56 = *on
104500150319     c                   leavesr
104600150319      *
104700150319   x3C                   ELSE
104800150319      *
104900150319      * Controllo
105000150319     c                   clear                   tibs02ds
105100150319     C                   MOVEL     'C'           t02mod
105200150319     C                   MOVEL     knsif         t02sif
105300150319     C                   MOVEL     'CHR'         t02cod
105400150319     C                   MOVEL     v01cdt        t02ke1
105500150319      *
105600150319     C                   CALL      'TIBS02R'
105700150319     C                   PARM                    KPJBA
105800150319     C                   PARM                    TIBS02DS
105900150319      *
106000150319    4C                   IF        t02err = *BLANKS
106100150319     C                   movel     T02uni        DCHR
106200150319     C                   movel     §CHRdesc      v1dcdt
106300150319      *
106400150319     C                   ELSE
106500150319     c                   eval      *in90 = *on
106600150319     C                   eval      *IN48 = *on
106700150319     c                   leavesr
106800150319    4C                   ENDIF
106900150319      *
107000150319    3c                   endif
107100150319      *
107200150319    2C                   endif
107300150319
107400150319      *?se richieste le sole aperte e codice chiusura valorizzato dare errore?
107500150319
107600160120    2c                   If        v1dcdt <> *blanks and (v01ape = 'SI'
107700160120     c                                                or  V01att = 'S')
107800150319     c                   eval      *in90 = *on
107900160120     c                   if        V01ape  = 'SI'
108000150319     C                   eval      *IN65 = *on
108100160120     c                   else
108200160120     c                   eval      *in64 = *on
108300160120     c                   endif
108400150319     c                   leavesr
108500150319    2C                   Endif
108600040331
108700150319      *?Filiale apertura reclamo?
108800040331
108900150319     C                   clear                   vd1pot
109000040331    2c                   If        v01pot <> *blanks
109100040331      *
109200040331      * Ricerca
109300040331     c     '?'           scan      v01pot
109400040331    3c                   if        %found
109500040331     C                   clear                   dstnsd
109600040331     C                   call      'TNSD24R'
109700040331     C                   parm                    §cod
109800040331     C                   parm                    §tip
109900040331     C                   parm                    §des
110000040331     C                   parm                    §pra
110100040331     C                   movel     §cod          v01pot
110200150319     C                   movel     §des          vd1pot
110300040331     c                   eval      *in90 = *on
110400040331     C                   eval      *IN51 = *on
110500150319     c                   leavesr
110600040331    3C                   endif
110700040331    2C                   endif
110800040331      *
110900040331    2C     v01pot        ifeq      *blanks
111000040331     C                   move      *all'0'       v01pot
111100040331    2C                   endif
111200040331      *
111300150626     C                   testn                   v01pot               12
111400040331     C                   move      v01pot        w001a             1
111500040331      *
111600040331     c                   move      v01pot        W_pot
111700040331      *
111800150626    2C     *in12         ifeq      *off
111900040331     C     w001a         orlt      '0'
112000040331     c                   eval      *in90 = *on
112100040331     C                   eval      *IN40 = *on
112200150319     c                   leavesr
112300040331    2C                   endif
112400040331      * verifico se P.O. esistente
112500151023    2c                   if        W_pot > 0
112600151023     c                   IF        w_pot = 999 and simfel = 0
112700151023     c                   eval      *in90 = *on
112800151023     C                   eval      *IN40 = *on
112900151023     c                   leavesr
113000151023    3c                   endif
113100151023     c                   IF        w_pot <> 999
113200160104     C     W_pot         chain     azorg
113300040331    3C                   if        %found(azorg01l)
113400150319     C                   movel     orgdes        vd1pot
113500040331      *  errore
113600040331     C                   ELSE
113700040331     c                   eval      *in90 = *on
113800040331     C                   eval      *IN40 = *on
113900150319     c                   leavesr
114000040331    3c                   endif
114100151023     c                   ENDIF
114200040331      *
114300040331    2c                   endif
114400150319
114500150319      *?se non è stato inserito la Fil. Apertura?
114600150319      *   ?la posso recuperare dall'utente?
114700150319     c                   If        v01uta <> *blanks
114800160104     c     v01uta        chain     Azute000
114900150319     c                   if        not %found(azute01l)
115000150319      *
115100150319     c                   eval      *in90 = *on
115200150319     c                   eval      *in49 = *on
115300150319     c                   leavesr
115400150319     C                   ELSE
115500150319     c                   if         v01pot = *blank  or v01pot = *zeros
115600150319     c                   movel     utefil        v01pot
115700150319     c                   endif
115800150319     c                   endif
115900150319      *
116000150319     c                   endif
116100150319
116200150319      *?Filiale responsabile?
116300150319
116400150319     C                   clear                   vd1por
116500150319    2c                   If        v01por <> *blanks
116600150319      *
116700150319      * Ricerca
116800150319     c     '?'           scan      v01por
116900150319    3c                   if        %found
117000150319     C                   clear                   dstnsd
117100150319     C                   call      'TNSD24R'
117200150319     C                   parm                    §cod
117300150319     C                   parm                    §tip
117400150319     C                   parm                    §des
117500150319     C                   parm                    §pra
117600150319     C                   movel     §cod          v01por
117700150319     C                   movel     §des          vd1por
117800150319     c                   eval      *in90 = *on
117900150319     C                   eval      *IN81 = *on
118000150319     c                   leavesr
118100150319    3C                   endif
118200150319    2C                   endif
118300150319      *
118400150319    2C     v01por        ifeq      *blanks
118500150319     C                   move      *all'0'       v01por
118600150319    2C                   endif
118700150319      *
118800150626     C                   testn                   v01por               12
118900150319     C                   move      v01por        w001a             1
119000150319      *
119100150319     c                   move      v01por        W_por
119200150319      *
119300150626    2C     *in12         ifeq      *off
119400150319     C     w001a         orlt      '0'
119500150319     c                   eval      *in90 = *on
119600150319     C                   eval      *IN76 = *on
119700150319     c                   leavesr
119800150319    2C                   endif
119900150319      * verifico se P.O. esistente
120000151022    2c                   if        W_por > 0
120100160104     C     W_por         chain     azorg
120200150319    3C                   if        %found(azorg01l)
120300150319     C                   movel     orgdes        vd1por
120400150319      *  errore
120500150319     C                   ELSE
120600150319     c                   eval      *in90 = *on
120700150319     C                   eval      *IN76 = *on
120800150319     c                   leavesr
120900150319    3c                   endif
121000150319      * verifico se inserite date chiusura
121100150319     c                   if        wddcr = 0
121200150319     c                   eval      *in90 = *on
121300150319     C                   eval      *IN77 = *on
121400150319     c                   leavesr
121500150319    3c                   endif
121600150319      *
121700150319    2c                   endif
121800150319
121900150319      * -?La selezione delle R.A. aperte da altre Filiali RICHIEDE?
122000150319      *  ?l'inserimento della Fil. Responsabile?
122100150319if  2c                   if        V01aap = 'S'  and  W_por = *zero
122200150319     c                   eval      *in90 = *on
122300150319     C                   eval      *in36 = *on
122400150319     c                   leavesr
122500150319e   2c                   endif
122600150319
122700150319      *?Tipo comunicazione?
122800150319
122900150319     c                   clear                   vd1tco
123000150319    2c                   If        v01tco <> *blank
123100150319      * Ricerca
123200150319     c     '?'           scan      v01tco
123300150319    3c                   if        %found
123400150319     c                   clear                   tibs02ds
123500150319     C                   MOVEL     'R'           t02mod
123600150319     C                   MOVEL     knsif         t02sif
123700150319     C                   MOVEL     'COR'         t02cod
123800150319      *
123900150319     C                   CALL      'TIBS02R'
124000150319     C                   PARM                    KPJBA
124100150319     C                   PARM                    TIBS02DS
124200150319      *
124300150319    4C                   IF        T02err = *BLANKS
124400150319     C                   movel     T02uni        DCOR
124500150319     C                   movel     T02ke1        v01tco
124600150319     C                   movel     §CORdesc      vd1tco
124700150319      *  errore (non ritorna nulla dalla selezione pulisco il campo)
124800150319     C                   ELSE
124900150319     c                   clear                   v01tco
125000150319    4C                   ENDIF
125100150319      *
125200150319     c                   eval      *in90 = *on
125300150319     C                   eval      *IN53 = *on
125400150319     c                   leavesr
125500150319      *
125600150319   x3C                   ELSE
125700150319      *
125800150319      * Controllo
125900150319     c                   clear                   tibs02ds
126000150319     C                   MOVEL     'C'           t02mod
126100150319     C                   MOVEL     knsif         t02sif
126200150319     C                   MOVEL     'COR'         t02cod
126300150319     C                   MOVEL     v01tco        t02ke1
126400150319      *
126500150319     C                   CALL      'TIBS02R'
126600150319     C                   PARM                    KPJBA
126700150319     C                   PARM                    TIBS02DS
126800150319      *
126900150319    4C                   IF        t02err = *BLANKS
127000150319     C                   movel     T02uni        DCOR
127100150319     C                   movel     T02ke1        v01tco
127200150319     C                   movel     §CORdesc      vd1tco
127300150319      *
127400150319     C                   ELSE
127500150319     c                   eval      *in90 = *on
127600150319     C                   eval      *IN45 = *on
127700150319     c                   leavesr
127800150319    4C                   ENDIF
127900150319      *
128000150319    3c                   endif
128100150319      *
128200150319    2C                   endif
128300040225
128400150319      *?Numero reclamo?
128500040225
128600040225    2c                   If        v01nut > 0
128700040421      * verifico se inserito anno reclamo
128800040421     c                   If        v01ant = 0
128900040421     c                   seton                                        3890
129000150319     c                   leavesr
129100050112     C                   else
129200050112      *
129300050112      *   sistemo anno di due cifre
129400050112     C                   if        v01ant < 100
129500050112     C                   ADD       2000          v01ant
129600050112     C                   endif
129700050112      *
129800040421     c                   endif
129900040225      * verifico se esiste numero reclamo
130000160104     c     Ktgdan        chain     fitgd000
130100050112     c                   if        not %found(fitgd01l)
130200040225     c                   seton                                        3990
130300150319     c                   leavesr
130400040225     c                   endif
130500040305
130600040225    2c                   endif
130700150319
130800150319      *?Tipo oggetto?
130900040527
131000040527     c                   clear                   v01tor
131100040527     c                   clear                   W_ogg
131200040527
131300150319      *?OGGETTO?
131400150319
131500040527    2c                   if        v01ogs = 'X'
131600040527     c                   eval      v01tor = 'S'
131700040527     c                   endif
131800040527
131900150319
132000150319      * -?Spedizione?
132100150626     c                   if        V01ogs = *blanks  and  SpeVid <> *zeros
132200150626     c                   eval      *in90 = *on
132300150626     c                   eval      *in78 = *on
132400150626     c                   leavesr
132500150626     c                   endif
132600040527    2c                   if        v01ogs = 'X' and spevid <> *zeros
132700040527
132800040527      * se non è stato inserito nulla non controllo
132900040527
133000040527      * verifico se inserito linea partenza
133100040527    3c                   if        v01lnp = *zeros
133200040527     c                   eval      *in90 = *on
133300040527     c                   eval      *in30 = *on
133400150319     c                   leavesr
133500040527    3c                   endif
133600040527      * verifico se inserito numero spedizione
133700040527    3c                   if        v01nsp = *zeros
133800040527     c                   eval      *in90 = *on
133900040527     c                   eval      *in31 = *on
134000150319     c                   leavesr
134100040527    3c                   endif
134200040527      * verifico se inserito anno e se è di 2
134300040527    3c                   if        v01ans = *zeros
134400040527     c                   eval      *in90 = *on
134500040527     c                   eval      *in33 = *on
134600150319     c                   leavesr
134700040527    3c                   endif
134800040527      *
134900040527    3c                   If        v01ans > 60 and v01ans < 100
135000040527     c                   add       1900          V01ans
135100040527    3c                   endif
135200040527
135300040527    3c                   If        v01ans <=  60 and v01ans < 100
135400040527     c                   add       2000          V01ans
135500040527    3c                   endif
135600040527      * preparo i campi della chiave
135700040527     c                   clear                   W_spevid
135800040527     c                   eval      W_spevid = spevid
135900040527     c                   movel(p)  W_spevid      W_ogg
136000040527      *
136100040527    2c                   endif
136200040527
136300040527    2c                   if        v01ogo = 'X' and v01tor <> ' '
136400040527     c                   eval      *in90 = *on
136500040527     c                   eval      *in54 = *on
136600150319     c                   leavesr
136700040527     c                   endif
136800040527
136900040527     c                   If        v01ogo  = 'X'
137000040527     c                   eval      v01tor = 'O'
137100040527     c                   endif
137200040527
137300150319
137400150319      * -?ORM?
137500150626     c                   if        V01ogo = *blanks  and  OrmVid <> *zeros
137600150626     c                   eval      *in90 = *on
137700150626     c                   eval      *in79 = *on
137800150626     c                   leavesr
137900150626     c                   endif
138000040527    2c                   if        v01ogo = 'X' and ormvid <> *zeros
138100040527
138200040527      * se non è stato inserito nulla non controllo
138300040527
138400040527      * verifico se inserito P.O. emissione
138500040527     c                   if        v01poe = *zeros
138600040527     c                   eval      *in90 = *on
138700040527     c                   eval      *in34 = *on
138800150319     c                   leavesr
138900040527     c                   endif
139000040527      * verifico se inserito numero ORM
139100040527     c                   if        v01nor = *zeros
139200040527     c                   eval      *in90 = *on
139300040527     c                   eval      *in35 = *on
139400150319     c                   leavesr
139500040527     c                   endif
139600040527      * preparo i campi della chiave del file
139700040527     c                   clear                   W_ormvid
139800040527     c                   eval      W_ormvid = ormvid
139900040527     c                   movel(p)  W_ormvid      W_ogg
140000040527      *
140100040527    2c                   endif
140200040527      *
140300040527
140400040527    2c                   if        v01ogg = 'X' and v01tor <> ' '
140500040527     c                   eval      *in90 = *on
140600040527     c                   eval      *in54 = *on
140700150319     c                   leavesr
140800040527     c                   endif
140900040527
141000040527     c                   If        v01ogg  = 'X'
141100040527     c                   eval      v01tor = 'G'
141200040527     c                   endif
141300040527
141400150319      * -?GENERICO?
141500150626     c                   if        V01ogg = *blanks  and  V01gen <> *blanks
141600150626     c                   eval      *in90 = *on
141700150626     c                   eval      *in80 = *on
141800150626     c                   leavesr
141900150626     c                   endif
142000040527     c                   If        v01ogg = 'X' and v01GEN <> *blanks
142100040527     c                   movel(p)  v01gen        W_ogg
142200040527     c                   endif
142300040603      *
142400150319      *?se non sono stati richiesti i solo reclami aperti?
142500150319      *   ?sono obbligatorie le date?
142600150319      *?SE NON È STATO INSERITO IL SOLO NUMERO RECLAMO?
142700150319      *?SE NON È STATO INSERITO L'OGGETTO?
142800040603      *
142900040603    2c                   If        v01ape <> 'SI' and v01nut = 0 and
143000040603     c                              W_ogg = *blank
143100040603     c                             and wadar = 0 and wddar = 0
143200040603     c                             and wadcr = 0 and wddcr = 0
143300040603     c                   eval      *in90 = *on
143400040603     C                   eval      *IN57 = *on
143500150319     c                   leavesr
143600040603    2c                   endif
143700050114
143800040227
143900040227      * verifico se inserita almeno una selezione
144000040225
144100040331     c                   If        v01nut = 0 and
144200040331     c                             vd1dar = 0 and
144300040331     c                             va1dar = 0 and
144400040603     c                             vd1dcr = 0 and
144500040603     c                             va1dcr = 0 and
144600040302     c                             (v01pot = *blanks or V01pot  = *zeros) and
144700040401     c                             v01uta = *blank  and
144800040603     c                             (v01por = *blanks or V01por  = *zeros) and
144900040331     c                             v01mad = *blank and
145000040331     c                             v01prt = *blank and
145100040331     c                             v01cdt = *blank and
145200040331     c                             v01ape = *blank and
145300040331     c                             v01rpg = *blank and
145400040331     c                             v01tco = *blank and
145500040331     c                             v01rsc = *blank and
145600040227     c                             v01tor = *blank
145700040227     c                   eval      *in90 = *on
145800040227     c                   eval      *in60 = *on
145900150319     c                   leavesr
146000040227     c                   endif
146100040225
146200040225     c                   Endsr
146300040226      *---------------------------------------------------------------*
146400040226      * Routine Gestione SUBFILE                                      *
146500040226      *---------------------------------------------------------------*
146600040226     c     SR_GESSBF     BegSR
146700040226
146800040226      * richiamo la routine di caricamento del subfile
146900150506     c                   eval      $InzSFL = *off
147000150506     c                   exsr      SR_CARSBF
147100150506      *
147200140926      /free
147300140926       //?Abilito F19=Disposizioni di Consegna
147400140926         exsr AbiF19;
147500140926      /end-free
147600140926
147700040227      * se è stato caricato almeno un record nel subfile emetto la videta
147800040227     c                   dow       *in90 = *off
147900150506      *
148000150506     c                   if        $InzSFL = *on
148100150506     c                   exsr      sr_CarSBF
148200150506     c                   eval      $InzSFL = *off
148300150616      * -?Se 90 acceso (errore/nessun rec.) esco dal ciclo di emissione Sfl?
148400150616     c                   If        *in90
148500150616     c                   leave
148600150616     c                   EndIf
148700150506     c                   endif
148800150603     c                   eval      V1Togg = W1Togg
148900040227      *
149000140219     c                   write     FIA2T01
149100040227     c                   write     fia2z02
149200140221      *
149300150507      * -?Posizionamento cursore nel subfile?
149400150507     c                   if        V2CrtnCsr > *zero
149500150507     c                   z-add     V2CrtnCsr     V2Cnrr
149600150507     c                   endif
149700150507      *
149800140526     c                   IF        wMod = 'R'  and  (iA2tor = 'S'
149900140526     c                                          or   iA2tor = 'O')
150000140221     c                   exfmt     FIA2C03
150100140221     c                   ELSE
150200040227     c                   exfmt     fia2c02
150300140220     c                   ENDIF
150400150506      *
150500150506     c                   Select
150600150506      *
150700040227      * f12 = Ritorno
150800150506     c                   When      *inkl
150900040227     c                   leave
151000150506      *
151100150506      * -?F5 = Refresh (da C02/S02)?
151200150506     c                   When      *inKE
151300150506     c                   eval      $InzSFL = *on
151400150506     c                   iter
151500150506      *
151600140220      * F11 = Ordinamento DI/Ascendente x R.A.
151700150506     c                   When      *inKK  and  wMod = 'R'
151800140221     c                   if        Not  $Ord_Desc
151900140221     c                   eval      $Ord_Desc = *on
152000140221     c                   eval      Z02F11    = 'F11=Ord.Ascendente '
152100140221     c                   else
152200140221     c                   eval      $Ord_Desc = *off
152300140221     c                   eval      Z02F11    = 'F11=Ord.Discendente'
152400140221     c                   endif
152500140220     c                   exsr      sr_OrdinamS03
152600140220     c                   iter
152700140926
152800140926      /free
152900150506         //?F19=Disposizioni di Consegna
153000150506           When  *inkt;
153100150506             exsr sr_F19;
153200140926      /end-free
153300150506
153400150506     c                   EndSl
153500140926
153600040227      * Lettura subfile
153700040227     c                   Exsr      SR_readsf
153800040317      * se 90 accxeso errore riemetto la videata
153900040317     c                   If        *in90
154000040317     c                   setoff                                       90
154100040317     c                   iter
154200040317     c                   endif
154300040305      * se è stato dato f3 dai programmi chiamati ho KL acceso e vado a fine
154400040315     c                   if        w_f03 = 'S'
154500040305     c                   leave
154600040305     c                   endif
154700040227
154800040227     c                   enddo
154900040227      *
155000040226     c                   Endsr
155100040226      *---------------------------------------------------------------*
155200040226      * Routine Caricamento SUBFILE                                   *
155300040226      *---------------------------------------------------------------*
155400040226     c     SR_CARSBF     BegSR
155500040226
155600040226      * pulizia del subfile
155700040226
155800040316     c                   setoff                                       2021
155900040226     c                   seton                                        22
156000140526     c                   if        wMod = 'R'  and  (iA2tor = 'S'
156100140526     c                                          or   iA2tor = 'O')
156200140219     c                   write     FIA2C03
156300140219     c                   else
156400040226     c                   write     fia2c02
156500140219     c                   endif
156600040226     c                   setoff                                       22
156700040227
156800140219     c                   clear                   V1Togg
156900150603     c                   clear                   W1Togg
157000140219
157100040227     c                   z-add     0             s01nrr
157200140219     c                   z-add     0             S03nrr
157300040227
157400040227     c                   SELECT
157500040227
157600150320      * se sono stato richiamato leggo per oggetto -------------------*
157700040226
157800040227     c                   When      wmod = 'R'
157900140526     c                   select
158000140526     c                   when      iA2tor = 'S'
158100140219     c                   movel     iA2ogg        WRKspe
158200150603     c                   eval      W1Togg = 'Sped. ' + %trim(WRKlnp) +  '/' +
158300140526     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
158400140219     c                             ' ' + WRKann
158500140526     c                   when      iA2tor = 'O'
158600140526     c                   movel     iA2ogg        WRKorm
158700150603     c                   eval      W1Togg = 'O.R.M. ' + %trim(WRKpoe) +  '/' +
158800140526     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
158900140526     c                             '/' + WRKnrv
159000140526     c                   endsl
159100150603     c                   if        W1Togg <> *blank
159200150603     c                   eval      ww = ( ( %size(W1Togg) -
159300150603     c                                      %len( %trim(W1Togg) ) )
159400140526     c                                    / 2 )
159500140526     c                   if        ww > *zero
159600150603     c                   eval      %subst( W1Togg : ww + 1 ) = W1Togg
159700150603     c                   eval      %subst( W1Togg : 1 : ww ) = *blanks
159800140526     c                   endif
159900140526     c                   endif
160000160104     c     ktgd02        setll     fitgd02
160100040226      *
160200040226     c                   do        *hival
160300160104     c     ktgd02        reade     fitgd02
160400040226
160500150324     c                   if        %eof(fitgd32l)
160600040226     c                   leave
160700040226     c                   endif
160800040226
160900040227      * salto i controlli del record e imposto record OK
161000040227
161100040227     c                   eval      Wrecok = 'Y'
161200040227
161300040226     c                   exsr      SR_wrtsbf
161400041018     c   28              leave
161500040226
161600040226     c                   enddo
161700150319
161800150428      /free
161900150420
162000150420         // -?Selezioni a video?
162100150420         Other;
162200150420           // -?Lettura FITGD00F e Scrittura record nel subfile S02?
162300150420           exsr  sr_ReadFITGD;
162400150420
162500150420      /end-free
162600150420
162700150420      *---------------------------------------------------------------*
162800150420      *
162900150420     c                   ENDSL
163000040227
163100150506     c                   eval      *in09 = ( wMod <> 'R' )
163200140221     c                   clear                   Z02F11
163300140224     c                   eval      *in08 = *off
163400140221
163500040304      * se non è stata caricata neppure una riga di subfile do errore
163600150319sel 1c                   Select
163700040227
163800150319w   1c                   When      s01nrr = 0  and  S03nrr = 0
163900040227     c                   eval      *in90 = *on
164000040227     c                   eval      *in61 = *on
164100040227      *
164200150319x   1c                   Other
164300040227      *
164400040227      * accendo indicatori di visualizzazione SUB_file
164500040227     c                   seton                                        2120
164600040317     c                   z-add     1             v2cnrr
164700150508     c                   eval      V2CrtnCsr = V2Cnrr
164800150420sel 2c                   Select
164900150420      *
165000150420w   2c                   When      wMod = 'R'
165100140220     c                   eval      RrnLast = S03nrr
165200140224     c                   eval      *in08  = (S03nrr > 1)
165300150319if  3c                   if        S03nrr > 1
165400140220     c                   exsr      sr_OrdinamS03
165500150319e   3c                   endif
165600150420      *
165700150428     c                   Other
165800150420     c                   eval      RrnLast = S01nrr
165900150420if  3c                   if        S01nrr > 1
166000150420     c                   exsr      sr_OrdinamS02
166100150420e   3c                   endif
166200150420      *
166300150420e   2c                   EndSl
166400150319
166500150319e   1c                   EndSl
166600040227
166700040227     c                   endsr
166800150417
166900150417      /free
167000150417
167100150417       //--------------------------------------------------------------
167200150417       //?Lettura file FITGD00F via SQL                                ?
167300150417       //--------------------------------------------------------------
167400150420       BEGSR  sr_ReadFITGD;
167500150417
167600150417         // -?Preparazione SQL da eseguire?
167700150417         exsr  sr_PrepSQL;
167800150417         exec sql   prepare S1   from :wSQL;
167900150417
168000150417         // -?Dichiarazione / Apertura del cursore?
168100150417         exec sql   declare C1   cursor for S1;
168200150417         if  SQLcode < *zero;
168300150417           *in28 = *on;
168400150417           leavesr;
168500150417         endif;
168600150417         exec sql   open C1;
168700150417         if  SQLcode < *zero;
168800150417           *in28 = *on;
168900150417           leavesr;
169000150417         endif;
169100150417
169200150417         // -?Ciclo di Lettura file / Scrittura rec. nel subfile?
169300160129         *in28 = *off;
169400150417         DoU  SQLcode < *zero  or  SQLcode = 100;
169500150417
169600150417           clear  FITGD_ds;
169700150417           exec sql   fetch next   from C1   into :FITGD_ds;
169800150417
169900150417           Select;
170000150417             // -?Fine File?
170100150417             When  SQLcode = 100;
170200150417               leave;
170300150417             // -?Errore SQL?
170400150417             When  SQLcode < *zero;
170500150417               *in28 = *on;
170600150417               leave;
170700150417             // -?Elaborazione singolo record?
170800150417             Other;
170900150417               eval  Wrecok = 'Y';
171000150417               exsr  SR_wrtsbf;
171100150417               if  *in28;
171200150417                 leave;
171300150417               endif;
171400150417           EndSl;
171500150417
171600150417         EndDo;
171700150417
171800150417         // -?Chiusura del cursore?
171900150417         exec sql   close C1;
172000150417
172100150417       ENDSR;
172200150417
172300150417       //--------------------------------------------------------------
172400150417       //?Preparazione stringa SQL da eseguire per la lettura.         ?
172500150417       //--------------------------------------------------------------
172600150417       BEGSR  sr_PrepSQL;
172700150417
172800150417         $First = *on;
172900150417
173000150417         wSQL  = 'select * +
173100150417                    from FITGD00F';
173200150626
173300150626         If  V01nut <> *zero  or
173400150626             W_ogg <> *blank;
173500150626           exsr  sr_PrepSQL_RA_Ogg;
173600150626         Else;
173700150626           exsr  sr_PrepSQL_Selez;
173800150626         EndIf;
173900150626
174000150626         wSQL += ' for fetch only';
174100150626
174200150626       ENDSR;
174300150626
174400150626       //--------------------------------------------------------------
174500150626       //?Parzializzaz. SQL NON per singola R.A. e/o singolo Oggetto.  ?
174600150626       //--------------------------------------------------------------
174700150626       BEGSR  sr_PrepSQL_Selez;
174800151221
174900151221         // -?Selezione R.A. in ATTESA DI RISPOSTA?
175000151221         If  V01att = 'S';
175100151221           wSQL += ' left outer join FITGR00F on +
175200151221                     TGDant = TGRant and +
175300151221                     TGDnut = TGRnut and +
175400151221                     TGDpdt = TGRpdt and +
175500151221                     TGRtri = ''A''';
175600151221         EndIf;
175700160120
175800160120         //- - - - - - - - - - - - - - - - - - - - - - - - - - - -
175900150417
176000150417         // -?Selezione Filiale Gestione?
176100160120         IF  V01pog <> *zero;
176200160120
176300160120           select;
176400160120             when  $First  and  V01att = 'S';
176500160120               wSQL  += ' where ((';
176600160120               $First = *off;
176700160120             when  $First  and  V01att = *blank;
176800160120               wSQL  += ' where (';
176900160120               $First = *off;
177000160120             other;
177100160120               wSQL  += ' and ';
177200160120           endsl;
177300160120
177400160120           If  V01pog <> '999';
177500170719             //*·wSQL += 'TGDpog = ' + %trim( V01pog );
177600170719             wSQL += 'TGDpog in (';
177700170719             For  xx = 1  By 1  To  %elem(sk_£6);
177800170719               select;
177900170719                 when  sk_£6(xx) <= *zero;
178000170719                   leave;
178100170719                 when  xx > 1;
178200170719                   wSQL += ', ';
178300170719               endsl;
178400170719               wSQL += %trim( %editc( sk_£6(xx) : '3' ) );
178500170719             EndFor;
178600170719             wSQL += ')';
178700160120           Else;
178800160120             wSQL += 'TGDpog in (';
178900151022             yy = 0;
179000151022             xx = 1;
179100160120             For xx  By 1  To %elem(sk_pog2);
179200160120               If sk_pog2(xx) <> *zeros;
179300160120                 if yy > 0;
179400151022                   wSQL += ', ';
179500160120                 else;
179600151022                   yy = 1;
179700160120                 endif;
179800151022                 wSQL += %editc(sk_pog2(xx):'X');
179900160120               EndIf;
180000160120             EndFor;
180100151022             wSQL += ')';
180200160120           EndIf;
180300160120
180400160120         ENDIF;
180500150417
180600150420         // -?Selezione Reparto Gestione o Utente Gestione?
180700160120         IF  V01rpr <> *blank  or  V01rpg <> *blank;
180800160120
180900160120           select;
181000160120             when  $First  and  V01att = 'S';
181100160120               wSQL  += ' where ((';
181200160120               $First = *off;
181300160120             when  $First  and  V01att = *blank;
181400160120               wSQL  += ' where (';
181500160120               $First = *off;
181600160120             other;
181700160120               wSQL  += ' and ';
181800160120           endsl;
181900160120
182000150420           wSQL += '(';
182100160120
182200150420           if  V01rpr <> *blank;
182300150420             wSQL += 'TGDrpr = ''' + V01rpr + '''';
182400150420           endif;
182500150420           if  V01rpr <> *blank  and  V01rpg <> *blank;
182600150420             wSQL += ' or ';
182700150420           endif;
182800160120
182900150420           if  V01rpg <> *blank;
183000150423             if  W_rpgGen = 'Y';
183100150423               wSQL += 'TGDrpg like ''' + %trimR( w_RPG ) + '%''';
183200150423             else;
183300150423               wSQL += 'TGDrpg = ''' + V01rpg + '''';
183400150423             endif;
183500150420           endif;
183600160120
183700150420           wSQL += ')';
183800160120
183900160120         ENDIF;
184000160120
184100160120         //- - - - - - - - - - - - - - - - - - - - - - - - - - - -
184200160120
184300160120         // -?Selezione R.A. in ATTESA DI RISPOSTA?
184400160120         IF  V01att = 'S';
184500160120
184600160120           if  $First;
184700160120             wSQL  += ' where (';
184800160120             $First = *off;
184900160120           else;
185000160120             wSQL  += ') OR (';
185100160120           endif;
185200160120
185300160129           clear  $First2;
185400160129
185500160120           If  V01pog <> *zero;
185600160129             if  Not $First2;
185700160129               $First2 = *on;
185800160129             else;
185900160129               wSQL   += ' and ';
186000160129             endif;
186100160120             If  V01pog <> '999';
186200170725               //*·wSQL += 'TGDpog <> ' + %trim( V01pog );
186300170725               wSQL += 'TGDpog NOT in (';
186400170725               For  xx = 1  By 1  To  %elem(sk_£6);
186500170725                 select;
186600170725                   when  sk_£6(xx) <= *zero;
186700170725                     leave;
186800170725                   when  xx > 1;
186900170725                     wSQL += ', ';
187000170725                 endsl;
187100170725                 wSQL += %trim( %editc( sk_£6(xx) : '3' ) );
187200170725               EndFor;
187300170725               wSQL += ')';
187400160120             Else;
187500160120               wSQL += 'TGDpog Not in (';
187600160120               yy = 0;
187700160120               xx = 1;
187800160120               For xx  By 1  To %elem(sk_pog2);
187900160120                 If sk_pog2(xx) <> *zeros;
188000160120                   if yy > 0;
188100160120                     wSQL += ', ';
188200160120                   else;
188300160120                     yy = 1;
188400160120                   endif;
188500160120                   wSQL += %editc(sk_pog2(xx):'X');
188600160120                 EndIf;
188700160120               EndFor;
188800160120               wSQL += ')';
188900160120             EndIf;
189000160120           EndIf;
189100160120
189200160120           If  V01ape <> 'SI';
189300160129             if  Not $First2;
189400160129               $First2 = *on;
189500160129             else;
189600160129               wSQL   += ' and ';
189700160129             endif;
189800160129             wSQL  += 'TGDcdt = '' ''';
189900160120           EndIf;
190000160120
190100160120           If  V01pog <> *zero;
190200160129             if  Not $First2;
190300160129               $First2 = *on;
190400160129             else;
190500160129               wSQL   += ' and ';
190600160129             endif;
190700160120             If  V01pog <> '999';
190800170725               //*·wSQL += 'substr(TGDflo, 2, 3) = ''' +
190900170725               //*·        %trim( V01pog ) + '''';
191000170725               wSQL += 'substr(TGDflo, 2, 3) in (';
191100170725               For  xx = 1  By 1  To  %elem(sk_£6);
191200170725                 select;
191300170725                   when  sk_£6(xx) <= *zero;
191400170725                     leave;
191500170725                   when  xx > 1;
191600170725                     wSQL += ', ';
191700170725                 endsl;
191800170725                 wSQL += '''' + %editc( sk_£6(xx) : 'X' ) + '''';
191900170725               EndFor;
192000170725               wSQL += ')';
192100160120             Else;
192200160129               wSQL += 'substr(TGDflo, 2, 3) in (''';
192300160120               yy = 0;
192400160120               xx = 1;
192500160120               For xx by 1 to %elem(sk_POg2);
192600160120                 if sk_POg2(xx) <> *zero;
192700160120                   if yy > 0;
192800160120                     wSQL += ''', ''';
192900160120                   else;
193000160120                     yy = 1;
193100160120                   endif;
193200160120                   wSQL += %editc( sk_POg2(xx) : 'X' );
193300160120                 endif;
193400160120               EndFor;
193500160120               wSQL += ''')';
193600160120             EndIf;
193700160120           EndIf;
193800160120
193900160129           if  Not $First2;
194000160129             $First2 = *on;
194100160129           else;
194200160129             wSQL   += ' and ';
194300160129           endif;
194400160129           wSQL += 'substr(TGDfar, 1, 1) = ''S''';
194500160120
194600160120           if  V01rpr <> *blank;
194700160129             if  Not $First2;
194800160129               $First2 = *on;
194900160129             else;
195000160129               wSQL   += ' and ';
195100160129             endif;
195200160129             wSQL += 'substr(TGRrif, 1, 10) = ''' +
195300160120                     V01rpr + '''';
195400160120           endif;
195500160120
195600160120         ENDIF;
195700160120
195800160120         if  $First = *off;
195900160120           if  V01att = 'S';
196000160120             wSQL  += '))';
196100160120           else;
196200160120             wSQL  += ')';
196300160120           endif;
196400160120         endif;
196500160120
196600160120         //- - - - - - - - - - - - - - - - - - - - - - - - - - - -
196700150417
196800150417         // -?Selezione SOLO R.A. Aperte?
196900150417         If  V01ape = 'SI';
197000150417           if  $First;
197100160120             wSQL  += ' where ';
197200150417             $First = *off;
197300150417           else;
197400150417             wSQL  += ' and ';
197500150417           endif;
197600150417           wSQL += 'TGDcdt = '' ''';
197700150417         EndIf;
197800150417
197900150417         // -?Selezione Stato (ex Priorità)?
198000150417         If  V01prt <> *blank;
198100150417           if  $First;
198200160120             wSQL  += ' where ';
198300150417             $First = *off;
198400150417           else;
198500150417             wSQL  += ' and ';
198600150417           endif;
198700150417           wSQL += 'TGDprt = ''' + %trim( V01prt ) + '''';
198800150417         EndIf;
198900150417
199000150417         // -?Parzializzazione per Data Richiesta?
199100150417         If  Wddar  <> *zero;
199200150417           if  $First;
199300160120             wSQL  += ' where ';
199400150417             $First = *off;
199500150417           else;
199600150417             wSQL  += ' and ';
199700150417           endif;
199800150417           wSQL += 'TGDdar >= ' + %trim( %editc( Wddar : '3' ) );
199900150417         EndIf;
200000150417         If  Wadar  <> *zero;
200100150417           if  $First;
200200160120             wSQL  += ' where ';
200300150417             $First = *off;
200400150417           else;
200500150417             wSQL  += ' and ';
200600150417           endif;
200700150417           wSQL += 'TGDdar <= ' + %trim( %editc( Wadar : '3' ) );
200800150417         EndIf;
200900150417
201000150417         // -?Parzializzazione per Data Chiusura?
201100150417         If  Wddcr  <> *zero;
201200150417           if  $First;
201300160120             wSQL  += ' where ';
201400150417             $First = *off;
201500150417           else;
201600150417             wSQL  += ' and ';
201700150417           endif;
201800150417           wSQL += 'TGDdcd >= ' + %trim( %editc( Wddcr : '3' ) );
201900150417         EndIf;
202000150417         If  Wadcr  <> *zero;
202100150417           if  $First;
202200160120             wSQL  += ' where ';
202300150417             $First = *off;
202400150417           else;
202500150417             wSQL  += ' and ';
202600150417           endif;
202700150417           wSQL += 'TGDdcd <= ' + %trim( %editc( Wadcr : '3' ) );
202800150417         EndIf;
202900150417
203000150417         // -?Selezione Ragione Sociale Chiamante?
203100150420         If  V01rsc <> *blank;
203200150420           If  V01rpc = *blank;
203300150420             if  $First;
203400160120               wSQL  += ' where ';
203500150420               $First = *off;
203600150420             else;
203700150420               wSQL  += ' and ';
203800150420             endif;
203900150420             wSQL += 'TGDrsc like ''' + %trimR( V01rsc ) + '%''';
204000150420           Else;
204100150420             if  $First;
204200160120               wSQL  += ' where ';
204300150420               $First = *off;
204400150420             else;
204500150420               wSQL  += ' and ';
204600150420             endif;
204700150420             wSQL += 'TGDrsc like ''%' + %trimR( V01rsc ) + '%''';
204800150420           EndIf;
204900150417         EndIf;
205000150417
205100150417         // -?Selezione Codice Chiamante?
205200150417         If  V01ksc <> *zero;
205300150417           if  $First;
205400160120             wSQL  += ' where ';
205500150417             $First = *off;
205600150417           else;
205700150417             wSQL  += ' and ';
205800150417           endif;
205900150417           wSQL += 'TGDksc = ' + %trim( %editc( V01ksc : '3' ) );
206000150417         EndIf;
206100150417
206200150417         // -?Selezione Motivo Apertura?
206300150417         If  V01mad <> *blank;
206400150417           if  $First;
206500160120             wSQL  += ' where ';
206600150417             $First = *off;
206700150417           else;
206800150417             wSQL  += ' and ';
206900150417           endif;
207000150420           wSQL += 'TGDmad = ''' + V01mad + '''';
207100150417         EndIf;
207200150417
207300150417         // -?Selezione Codice Chiusura?
207400150417         If  V01cdt <> *blank;
207500150417           if  $First;
207600160120             wSQL  += ' where ';
207700150417             $First = *off;
207800150417           else;
207900150417             wSQL  += ' and ';
208000150417           endif;
208100150420           wSQL += 'TGDcdt = ''' + V01cdt + '''';
208200150417         EndIf;
208300150417
208400150417         // -?Selezione Filiale Apertura?
208500150417         If  W_pot <> *zero;
208600150417           if  $First;
208700160120             wSQL  += ' where ';
208800150417             $First = *off;
208900150417           else;
209000150417             wSQL  += ' and ';
209100150417           endif;
209200151022           IF  w_pot <> 999;
209300150417           wSQL += 'TGDpot = ' + %trim( %editc( W_pot : '3' ) );
209400151022           ELSE;
209500160119             wSQL += 'TGDpot in (';
209600151022             yy = 0;
209700151022             xx = 1;
209800151022             FOR xx by 1 to %elem(sk_pog2);
209900151022               IF sk_pog2(xx) <> *zeros;
210000151022                 IF yy > 0;
210100151022                   wSQL += ', ';
210200151022                 ELSE;
210300151022                   yy = 1;
210400151022                 ENDIF;
210500151022                 wSQL += %editc(sk_pog2(xx):'X');
210600151022               ENDIF;
210700151022             ENDFOR;
210800151022             wSQL += ')';
210900151022           ENDIF;
211000150417         EndIf;
211100150417
211200150417         // -?Selezione Utente Apertura?
211300150417         If  V01uta <> *blank;
211400150417           if  $First;
211500160120             wSQL  += ' where ';
211600150417             $First = *off;
211700150417           else;
211800150417             wSQL  += ' and ';
211900150417           endif;
212000150420           wSQL += 'TGDpru = ''' + V01uta + '''';
212100150417         EndIf;
212200150417
212300150417         // -?Selezione Filiale Responsabile?
212400150417         If  W_por <> *zero;
212500150417           if  $First;
212600160120             wSQL  += ' where ';
212700151221             $First = *off;
212800151221           else;
212900151221             wSQL  += ' and ';
213000151221           endif;
213100151221           wSQL += 'TGDpor = ' + %trim( %editc( W_por : '3' ) );
213200151221         EndIf;
213300151221
213400160104         // -?Selezione Aperte da Altre Filiali?
213500150420         //  ?(diverse dalla Filiale Responsabile)?
213600150417         If  W_por <> *zero  and  V01aap <> *blank;
213700150417           if  $First;
213800160120             wSQL  += ' where ';
213900150417             $First = *off;
214000150417           else;
214100150417             wSQL  += ' and ';
214200150417           endif;
214300150417           wSQL += 'TGDpot <> ' + %trim( %editc( W_por : '3' ) );
214400150417         EndIf;
214500150417
214600150417         // -?Selezione Tipo Comunicazione?
214700150417         If  V01tco <> *blank;
214800150417           if  $First;
214900160120             wSQL  += ' where ';
215000150417             $First = *off;
215100150417           else;
215200150417             wSQL  += ' and ';
215300150417           endif;
215400150417           wSQL += 'TGDtco = ''' + V01tco + '''';
215500150417         EndIf;
215600150626
215700150626         // -?Selezione Tipo Oggetto?
215800150626         If  V01tor <> *blank;
215900150626           if  $First;
216000160120             wSQL  += ' where ';
216100150626             $First = *off;
216200150626           else;
216300150626             wSQL  += ' and ';
216400150626           endif;
216500150626           wSQL += 'TGDtor = ''' + V01tor + '''';
216600150626         EndIf;
216700150626
216800150626       ENDSR;
216900150626       //--------------------------------------------------------------
217000150626       //?Parzializzaz. SQL per singola R.A. e/o singolo Oggetto.      ?
217100150626       //--------------------------------------------------------------
217200150626       BEGSR  sr_PrepSQL_RA_Ogg;
217300150519
217400150519         // -?Selezione Richiesta Assistenza?
217500150519         If  V01nut <> *zero;
217600150519           if  $First;
217700150519             wSQL  += ' where ';
217800150519             $First = *off;
217900150519           else;
218000150519             wSQL  += ' and ';
218100150519           endif;
218200150519           wSQL += 'TGDant = ' + %trim( %editc( V01ant : '3' ) ) + ' and +
218300150519                    TGDnut = ' + %trim( %editc( V01nut : '3' ) );
218400150519         EndIf;
218500150417
218600150417         // -?Selezione Oggetto?
218700150417         If  V01tor <> *blank;
218800150417           if  $First;
218900150417             wSQL  += ' where ';
219000150417             $First = *off;
219100150417           else;
219200150417             wSQL  += ' and ';
219300150417           endif;
219400150417           wSQL += 'TGDtor = ''' + V01tor + '''';
219500150417         EndIf;
219600150417         If  W_ogg <> *blank;
219700150417           if  $First;
219800150417             wSQL  += ' where ';
219900150417             $First = *off;
220000150417           else;
220100150417             wSQL  += ' and ';
220200150417           endif;
220300150417           wSQL += 'TGDogg = ''' + W_ogg + '''';
220400150417         EndIf;
220500150417
220600150417       ENDSR;
220700150417
220800150417      /end-free
220900150417
221000040227      *---------------------------------------------------------------*
221100040227      * Routine Scrittura records nel subfile                         *
221200040227      *---------------------------------------------------------------*
221300150319     c     SR_wrtsbf     BegSR
221400160129      *
221500140221      * se record NON giusto NON scrivo
221600140219     c                   if        Wrecok <> 'Y'
221700140219     c                   leavesr
221800140219     c                   endif
221900040227      *
222000140526     c                   if        wMod = 'R'  and  (iA2tor = 'S'
222100140526     c                                          or   iA2tor = 'O')
222200140219     c                   add       1             S03nrr
222300140219     c                   else
222400040227     c                   add       1             s01nrr
222500140219     c                   endif
222600151221
222700151221     c                   eval      dTGDfar = TGDfar
222800151221     c                   eval      dTGD01  = TGDflo
222900040227
223000040227     c                   clear                   vs1opz
223100040421     c                   z-add     tgdant        vs1ant                         Anno   reclamo
223200040304     c                   z-add     tgdnut        vs1nut                         Numero reclamo
223300040304     c                   z-add     tgdpdt        vs1pdt                         progressivo riga
223400041012      * verifico se Reclamo manutenzionabile  solo se non sono un utente autorizzato (07 *on)
223500040615      * p.o. gestione
223600150317     c                   z-add     TGDpog        VS1pog                         P.O. gestione
223700150910     c*//*               If        tgdpog <> dutpou and not *in07
223800150910     c                   If        Not *in07  and
223900150910     c                             %lookup( TGDpog : sk_POg2 ) = *zero
224000040317     c                   eval      vs1ges = 'N'
224100040317     c                   else
224200040317     c                   clear                   vs1ges
224300040317     c                   endif
224400040317      *
224500040227     c                   z-add     tgdpot        vs1pot                         P.O. apertura
224600150325     c*//                eval      vs1nrr  = (%editc(tgdnut:'Z') + '/' +        Numnero reclamo
224700150325     c*//                          %editc(tgdpdt:'Z'))                          progres. riga
224800040227      *
224900150417     c                   z-add     tgdDAC        HS1dac
225000140423     c                   z-add     tgdDAC        data8
225100040227     c                   move      anno2         vs1dar
225200040227     c                   move      mese          mmgg              4 0
225300040227     c                   movel     mmgg          vs1dar
225400040227     c                   movel     giorno        vs1dar                         Data apertura
225500150325      *
225600150325     c                   eval      VS1orc = (tgdORC / 100)
225700040227      *
225800150417     c                   movel     TGDrpr        VS1rpr                         reparto gestione
225900040402     c                   movel     tgdrpg        vs1rpg                         utente gestione
226000040227     c                   movel     tgdrsc        vs1rsc                         cliente
226100140403     c                   movel     tgdrsc        vs1rsc2                        cliente
226200040227      * chi chiama rispetto all'oggetto
226300040304     C                   movel     tgdtch        vs1tch                         chi chiama
226400150324      * stato (ex priorità)
226500150324     C                   movel     tgdprt        vs1prt                         stato
226600151221     c                   movel     TGDprt        HS1prt2                        Stato
226700151221     c                   if        dTGDfar.§TGDatt = 'S'  and
226800160204     c                             dTGDfar.§TGDris = ' '  and
226900160119     c                             ( ( V01pog <> '999'    and
227000170726     c*//*                             V01pog <> %editc( TGDpog : 'X' ) )  or
227100170726     c                                 %lookup( TGDpog : sk_£6 )
227200170726     c                                        =  *zero )  or
227300160119     c                               ( V01pog =  '999'    and
227400160119     c                                 %lookup( TGDpog : sk_POg2 ) = *zero ) )
227500151222     c*// Non Fil.Risp.:           %lookup( dTGD01.§TGDfilR : sk_POg2 ) = *zero
227600151221     c                   eval      HS1prt2 = '5'
227700151222     c                   eval      VS1prt  = 'A'
227800151221     c                   endif
227900040227      * - OGGETTO
228000040227     c                   clear                   vs1ogg
228100040227      * ---------- SPEDIZIONE -----------
228200040227     c                   if        tgdtor = 'S'
228300040227      *
228400040227     c                   movel     tgdogg        WRKspe
228500040227     c                   eval      vs1ogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
228600040227     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
228700040227     c                             ' ' + WRKann
228800040227      *
228900040227     c                   endif
229000040227      * ----------    ORM     -----------
229100040227     c                   if        tgdtor = 'O'
229200040227     c                   movel     tgdogg        WRKorm
229300040227      *
229400040227     c                   eval      vs1ogg = 'ORM ' + %trim(WRKpoe) +  '/' +
229500040227     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
229600040227     c                             '/' + %trim(WRKnrv)
229700040227      *
229800040227     c                   endif
229900040227      * ----------  GENERICO  -----------
230000040227     c                   if        tgdtor = 'G'
230100040227     c                   EVAL      VS1ogg = TGDogg
230200040227     c                   endif
230300040227
230400040227      * MOTIVO RECLAMO
230500040227     c                   clear                   tibs02ds
230600040227     C                   MOVEL     'C'           t02mod
230700040227     C                   MOVEL     knsif         t02sif
230800040227     C                   MOVEL     'MAR'         t02cod
230900040227     C                   MOVEL     tgdmad        t02ke1
231000040227      *
231100040227     C                   CALL      'TIBS02R'
231200040227     C                   PARM                    KPJBA
231300040227     C                   PARM                    TIBS02DS
231400040227      *
231500040227     C                   movel     T02uni        DMAR
231600040227     C                   movel     §MARdesc      vsdmad
231700140403     C                   eval      VSDmad2 = TGDmad + ' ' + §MARdesc
231800140217      * VERIFICO SE MOTIVO TECNICO NON ABILITO LA MANUTENZIONE MA SOLO LA VISUALIZZAZIONE
231900140217     C                   IF        VS1GES = ' '   AND
232000140217     C                             §MARUTUT <> ' '
232100140217     C                   EVAL      VS1GES = 'V'
232200140217     C                   ENDIF
232300040227      * CHIUSURA RECLAMO
232400040304     C                   clear                   vs1cdt
232500040304     c                   setoff                                       06
232600040227     c                   If        tgdcdt <> *blank
232700040304     c                   eval      *in06 = *on
232800040304     C                   movel     tgdcdt        vs1cdt
232900040304      * data chiusura
233000040304     c                   clear                   WLBdat
233100040304     c                   eval      G02inv = tgddcd
233200040304     c                   eval      G02err = '3'
233300040304     c                   call      'XSRDA8'
233400040304     c                   parm                    WLBdat
233500040304     c                   z-add     G02dat        vs2dcd                         data chiuusra
233600040227     c                   endif
233700040227      * recupero le note
233800040227      *
233900040227     C                   clear                   FIDNB0DS
234000040421     c                   z-add     vs1ant        iB0ant
234100040227     c                   z-add     vs1nut        iB0nut
234200040227     c                   z-add     tgdpdt        iB0pdt
234300040227     c                   movel     'GEN'         iB0tIN
234400040227     c                   movel     'R'           iB0flm
234500040227      *
234600040227     C                   CALL      'FIDNB0R'
234700040227     C                   PARM                    kpjba
234800040227     C                   PARM                    FIDNB0DS
234900040227      *
235000040227      * valorizzo le prime 4 note a video
235100040227      *
235200040227     c                   movel     DB0NO1        vs1no1
235300040227     c                   movel     DB0NO2        vs1no2
235400141217      /free
235500141217       //?Imposto nel sbf l'indicatore che condiziona la visualizzazione dei
235600141217       //?dati di chiusura
235700141217         VSHin06 = *in06;
235800150428       //?Imposto nel sbf le condizioni che fanno:?
235900150428       //?- lampeggiare il campo STATO (ex Priorità) - se 0?
236000150428       //?- evidenziare in rosso data/ora creazione - se STATO = 0?
236100150428       //?- evidenziare in bianco rag.soc. chiamante - se STATO <> 0?
236200150417         if  VS1prt = '0';
236300150420           DspAtrPRT = c_DspAtrRed_BL;
236400150428           DspAtrDAR = c_DspAtrRed;
236500150428           DspAtrRSC = c_DspAtrRed_UL;
236600150417         else;
236700150417           DspAtrPRT = c_DspAtrNorm;
236800150428           DspAtrDAR = c_DspAtrNorm;
236900150428           DspAtrRSC = c_DspAtrWht_UL;
237000150417         endif;
237100141217      /end-free
237200141217
237300040227      *
237400140526     c                   if        wMod = 'R'  and  (iA2tor = 'S'
237500140526     c                                          or   iA2tor = 'O')
237600140221     c                   exsr      sr_PrepTXT
237700140219     c                   write     FIA2S03
237800140219     c                   else
237900040227     c                   write     fia2s02
238000140219     c                   endif
238100041018      * controllo il numero massimo di record nel subfile
238200140219     c                   If        S01nrr >= 9998   or
238300140219     c                             S03nrr >= 9998
238400041018     c                   eval      *in28 = *on
238500041018     C                   endif
238600040227      *
238700040227     c                   endsr
238800140220
238900140220      /free
239000140221
239100140221       //--------------------------------------------------------------
239200140221       //?Preparazione campi VS1TXT1 e VS1TXT2 del subfile S03.        ?
239300140221       //--------------------------------------------------------------
239400140221       BEGSR  sr_PrepTXT;
239500140221
239600140221         //*...+....1....+....2....+
239700140221         //.Chiusa --- il --/--/----
239800140224
239900140224         clear  VS1txt1;
240000140224         clear  VS1txt2;
240100140221
240200140221         Select;
240300140221
240400140221           // -?Reclamo NON chiuso?
240500140221           When  TGDcdt = *blank;
240600140221             // -?Valorizzazione delle 4 note?
240700140221             VS1txt1 = DB0no1 + ' ' + DB0no2;
240800140221             VS1txt2 = DB0no3 + ' ' + DB0no4;
240900140221
241000140224           // -?Reclamo chiuso e senza note?
241100140221           When  TGDcdt <> *blank  and  DB0no1 = *blank  and
241200140221                 DB0no2 =  *blank  and  DB0no3 = *blank;
241300140221             evalR VS1txt1 = c_DspAtr_HI  + 'Chiusa' +
241400140221                             c_DspAtrNorm + VS1cdt +
241500140221                             c_DspAtr_HI  + 'il' +
241600140221                             c_DspAtrNorm + %editc( VS2dcd : 'Y' );
241700140224           // -?Reclamo chiuso e con solo una nota?
241800140224           When  TGDcdt <> *blank  and  DB0no1 <> *blank  and
241900140224                 DB0no2 =  *blank  and  DB0no3 =  *blank;
242000140224             VS1txt1 = DB0no1;
242100140224             %subst( VS1txt1 : %len(VS1txt1) - 24 ) =
242200140224                             c_DspAtr_HI  + 'Chiusa' +
242300140224                             c_DspAtrNorm + VS1cdt +
242400140224                             c_DspAtr_HI  + 'il' +
242500140224                             c_DspAtrNorm + %editc( VS2dcd : 'Y' );
242600140224           // -?Reclamo chiuso e con note?
242700140221           When  TGDcdt <> *blank;
242800140221             VS1txt1 = DB0no1 + ' ' + DB0no2;
242900140221             VS1txt2 = DB0no3;
243000140221             %subst( VS1txt2 : %len(VS1txt2) - 24 ) =
243100140221                             c_DspAtr_HI  + 'Chiusa' +
243200140221                             c_DspAtrNorm + VS1cdt +
243300140221                             c_DspAtr_HI  + 'il' +
243400140221                             c_DspAtrNorm + %editc( VS2dcd : 'Y' );
243500140221
243600140221         EndSl;
243700140221
243800140221       ENDSR;
243900150316
244000150316       //--------------------------------------------------------------
244100150316       //?Ordinamento subfile S02.                                     ?
244200150316       //?  This subroutine sorts the subfile records.                 ?
244300150316       //--------------------------------------------------------------
244400150316       BEGSR  sr_OrdinamS02;
244500150316
244600150316         // -?Impostazione NRR dell'ultimo record del subfile          ?
244700150316         //  ?(già fatto in sr_CarSBF:                                 ?
244800150316         //  ? S02NRR può essere variato, se già inserita un'opzione)  ?
244900150316         //RrnLast  = S02nrr;
245000150316
245100150316         V2Cnrr = 1;
245200150316         clear  S01nrr;
245300150316
245400150317         //*in28 = *off;
245500150316         *in69 = *off;
245600150316         *in70 = *off;
245700150316         *in71 = *off;
245800150316
245900150316         //?___________________________________________________________?
246000150316         //?Initialize the key fields to sort on.?
246100150316         //?There is one extra field not in the subfile -- Selected --?
246200150316         //?that is added to the record. This field is used to place?
246300150316         //?selected records in front of nonselected records.?
246400150316         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
246500150316
246600150316         clear  QLGKL;
246700150316         clear  QLGSKL;
246800150316         clear  QLGSCB;
246900150316         clear  QLGSCB00;
247000150316
247100150316         // -?Gestione della scelta ordinamento:?
247200150316         //Select;
247300150316
247400150417           // -?Ord. x Stato (ex Priorità) + Data e Ora Creazione / Reparto / Utente?
247500150316           //When  ???;
247600150317             // -?3 campi chiave?
247700150317             QLGNBRK  = 3;
247800150417             // -?1° campo: Stato (ex Priorità) + Data e Ora Creazione?
247900151221             //            ?(HS1PRT2 + HS1DAC + VS1ORC)?
248000150417             //            ?a posizone   97, 13 byte, char, ascending.?
248100150325             QLGSP    = %size(VS1ant) + %size(VS1nut) + %size(VS1pdt) +
248200150325                        %size(VS1pot) + %size(VS1ges) + %size(VS1dar) +
248300150417                        %size(VS1rpr) + %size(VS1rpg) + %size(VS1pog) +
248400150417                        %size(VS1rsc) + %size(VS1tch) + %size(VSDmad) +
248500150325                        1;
248600151221             QLGSS    = %size(HS1prt2) + %size(HS1dac) + %size(VS1orc);
248700150316             QLGDT    = c_Carattere;
248800150316             QLGSO    = c_Ascendente;
248900150316             QLGKL(1) = QLGSKL;
249000150417             // -?2° campo: Reparto (VS1rpr)?
249100150417             //            ?a posizone   27, 10 byte, char, ascending.?
249200150417             QLGSP    = %size(VS1ant) + %size(VS1nut) + %size(VS1pdt) +
249300150417                        %size(VS1pot) + %size(VS1ges) + %size(VS1dar) +
249400150417                        1;
249500150417             QLGSS    = %size(VS1rpr);
249600150417             QLGDT    = c_Carattere;
249700150316             QLGSO    = c_Ascendente;
249800150316             QLGKL(2) = QLGSKL;
249900150417             // -?3° campo: Utente gestione (VS1rpg)?
250000150417             //            ?a posizone   37, 10 byte, char, ascending.?
250100150417             QLGSP    = %size(VS1ant) + %size(VS1nut) + %size(VS1pdt) +
250200150417                        %size(VS1pot) + %size(VS1ges) + %size(VS1dar) +
250300150417                        %size(VS1rpr) + 1;
250400150417             QLGSS    = %size(VS1rpg);
250500150417             QLGDT    = c_Carattere;
250600150316             QLGSO    = c_Ascendente;
250700150316             QLGKL(3) = QLGSKL;
250800150316
250900150316         //EndSl;
251000150316
251100150316         // -?Load other sort parameters.?
251200150316         QLGLB   = 80 + 16 * c_MaxKey;
251300150317         QLGRL   = %size(SflRcd) - 1;
251400150316         QLGRT   = 8;
251500150316         QLGOKL  = 80;
251600150316         QLGLKE  = 16;
251700150316         QLGLSS  = 290;
251800150316         // -?Initialize Sort I/O API fields.?
251900150316         QLGRL00 = QLGRL;
252000150316         QLGRC00 = 1;
252100150316         clear  QUSEI;
252200150316         QUSBPRV = %size(QUSEC);
252300150316
252400150316         // -?First step - Initialize the sort routine.?
252500150316         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
252600150316                      SizeList : ReturnSize : QUSEC );
252700150316
252800150316         // -?Next step - Write records to I/O routine.?
252900150316         QLGRT00  = c_Put;
253000150316         For  Nrr = 1  To  RrnLast;
253100150316           chain  Nrr  FIA2S02;
253200150316           // -?Solo le righe con Selected = 'Y' sono riordinate,?
253300150316           //  ?quindi per fare un ordinamento di tutte le righe?
253400150316           //  ?metto 'Y' sempre.?
253500150316           Selected = 'Y';
253600150316           clear  QUSEI;
253700150316           QUSBPRV  = %size(QUSEC);
253800150317           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
253900150317                         SizeList : NotUsed : QUSEC );
254000150316         EndFor;
254100150316
254200150316         // -?Next step - Signal end of input, clear subfile for reload.?
254300150316         QLGRT00 = c_EndPut;
254400150316         clear  QUSEI;
254500150316         QUSBPRV = %size(QUSEC);
254600150317         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
254700150317                       SizeList : NotUsed : QUSEC );
254800150316
254900150316         // -?Pulizia subfile?
255000150316         *in20 = *off;
255100150316         *in21 = *off;
255200150316         *in22 = *on;
255300150316         write  FIA2C02;
255400150316         *in22 = *off;
255500150316
255600150316         // -?Final step - Write the records back to the subfile.?
255700150316         QLGRT00  = c_Get;
255800150316         For  Nrr = 1  To RrnLast;
255900150316           clear  QUSEI;
256000150316           QUSBPRV = %size(QUSEC);
256100150317           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
256200150316                         QLGRL00  : NotUsed : QUSEC );
256300150316           S01nrr = Nrr;
256400150320           *in06  = VSHin06;
256500150316           write  FIA2S02;
256600150316         EndFor;
256700150316
256800150316         // -?Visualizzazione del SFL (se ci sono dati)?
256900150316         *in20 = (S01nrr > *zero);
257000150316         *in21 = *in20;
257100150316
257200150316       ENDSR;
257300140220
257400140220       //--------------------------------------------------------------
257500140220       //?Ordinamento subfile S03.                                     ?
257600140220       //?  This subroutine sorts the subfile records.                 ?
257700140220       //--------------------------------------------------------------
257800140220       BEGSR  sr_OrdinamS03;
257900140220
258000140220         // -?Impostazione NRR dell'ultimo record del subfile          ?
258100140220         //  ?(già fatto in sr_CarSBF:                                 ?
258200140220         //  ? S03NRR può essere variato, se già inserita un'opzione)  ?
258300140220         //RrnLast  = S03nrr;
258400140220
258500140220         V2Cnrr = 1;
258600140220         clear  S03nrr;
258700140220
258800150317         //*in28 = *off;
258900140220         *in69 = *off;
259000140220         *in70 = *off;
259100140220         *in71 = *off;
259200140220
259300140220         //?___________________________________________________________?
259400140220         //?Initialize the key fields to sort on.?
259500140220         //?There is one extra field not in the subfile -- Selected --?
259600140220         //?that is added to the record. This field is used to place?
259700140220         //?selected records in front of nonselected records.?
259800140220         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
259900140220
260000140220         clear  QLGKL;
260100140220         clear  QLGSKL;
260200140220         clear  QLGSCB;
260300140220         clear  QLGSCB00;
260400140220
260500140220         // -?Gestione della scelta ordinamento:?
260600140220         Select;
260700140220
260800150617           // -?Ordinamento per Numero R.A. DIscendente?
260900150323           When  $Ord_Desc;
261000140220             // -?3 campi chiave?
261100140220             QLGNBRK  = 3;
261200150617             // -?1° campo: Data e Ora Creazione?
261300150617             //            ?(HS1DAC + VS1ORC)?
261400150617             //            ?a posizone   98, 12 byte, int, descending.?
261500150617             QLGSP    = %size(VS1ant) + %size(VS1nut) + %size(VS1pdt) +
261600150617                        %size(VS1pot) + %size(VS1ges) + %size(VS1dar) +
261700150617                        %size(VS1rpr) + %size(VS1rpg) + %size(VS1pog) +
261800150617                        %size(VS1rsc) + %size(VS1tch) + %size(VSDmad) +
261900150617                        %size(VS1prt) + 1;
262000150617             QLGSS    = %size(HS1dac) + %size(VS1orc);
262100150617             QLGDT    = c_NumIntero;
262200150617             QLGSO    = c_Discendente;
262300150617             QLGKL(1) = QLGSKL;
262400150617             // -?2° campo: Anno e Numero Richiesta?
262500150617             //            ?(VS1ANT + VS1NUT)?
262600150617             //            ?a posizone    1, 13 byte, int, descending.?
262700150325             QLGSP    = 1;
262800150617             QLGSS    = %size(VS1ant) + %size(VS1nut);
262900140220             QLGDT    = c_NumIntero;
263000140220             QLGSO    = c_Discendente;
263100150617             QLGKL(2) = QLGSKL;
263200140220             // -?3° campo: Progressivo Riga (VS1pdt)?
263300150325             //            ?a posizone   14,  3 byte, int, descending.?
263400150325             QLGSP    = %size(VS1ant) + %size(VS1nut) + 1;
263500140220             QLGSS    = %size(VS1pdt);
263600140220             QLGDT    = c_NumIntero;
263700140220             QLGSO    = c_Discendente;
263800140220             QLGKL(3) = QLGSKL;
263900140220
264000150617           // -?Ordinamento per Numero R.A. Ascendente?
264100150323           When  Not $Ord_Desc;
264200140220             // -?3 campi chiave?
264300140220             QLGNBRK  = 3;
264400150617             // -?1° campo: Data e Ora Creazione?
264500150617             //            ?(HS1DAC + VS1ORC)?
264600150617             //            ?a posizone   98, 12 byte, int, ascending.?
264700150617             QLGSP    = %size(VS1ant) + %size(VS1nut) + %size(VS1pdt) +
264800150617                        %size(VS1pot) + %size(VS1ges) + %size(VS1dar) +
264900150617                        %size(VS1rpr) + %size(VS1rpg) + %size(VS1pog) +
265000150617                        %size(VS1rsc) + %size(VS1tch) + %size(VSDmad) +
265100150617                        %size(VS1prt) + 1;
265200150617             QLGSS    = %size(HS1dac) + %size(VS1orc);
265300150617             QLGDT    = c_NumIntero;
265400150617             QLGSO    = c_Ascendente;
265500150617             QLGKL(1) = QLGSKL;
265600150617             // -?2° campo: Anno e Numero Richiesta?
265700150617             //            ?(VS1ANT + VS1NUT)?
265800150617             //            ?a posizone    1, 13 byte, int, ascending.?
265900150617             QLGSP    = 1;
266000150617             QLGSS    = %size(VS1ant) + %size(VS1nut);
266100150617             QLGDT    = c_NumIntero;
266200140220             QLGSO    = c_Ascendente;
266300150617             QLGKL(2) = QLGSKL;
266400140220             // -?3° campo: Progressivo Riga (VS1pdt)?
266500150617             //            ?a posizone   14,  3 byte, int, ascending.?
266600150325             QLGSP    = %size(VS1ant) + %size(VS1nut) + 1;
266700140220             QLGSS    = %size(VS1pdt);
266800140220             QLGDT    = c_NumIntero;
266900140220             QLGSO    = c_Ascendente;
267000140220             QLGKL(3) = QLGSKL;
267100140220
267200140220         EndSl;
267300140220
267400140220         // -?Load other sort parameters.?
267500140220         QLGLB   = 80 + 16 * c_MaxKey;
267600150317         QLGRL   = %size(SflRcd) - 1;
267700140220         QLGRT   = 8;
267800140220         QLGOKL  = 80;
267900140220         QLGLKE  = 16;
268000140220         QLGLSS  = 290;
268100140220         // -?Initialize Sort I/O API fields.?
268200140220         QLGRL00 = QLGRL;
268300140220         QLGRC00 = 1;
268400140220         clear  QUSEI;
268500140220         QUSBPRV = %size(QUSEC);
268600140220
268700140220         // -?First step - Initialize the sort routine.?
268800140220         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
268900140220                      SizeList : ReturnSize : QUSEC );
269000140220
269100140220         // -?Next step - Write records to I/O routine.?
269200140220         QLGRT00  = c_Put;
269300140220         For  Nrr = 1  To  RrnLast;
269400140220           chain  Nrr  FIA2S03;
269500140220           // -?Solo le righe con Selected = 'Y' sono riordinate,?
269600140220           //  ?quindi per fare un ordinamento di tutte le righe?
269700140220           //  ?metto 'Y' sempre.?
269800140220           Selected = 'Y';
269900140220           clear  QUSEI;
270000140220           QUSBPRV  = %size(QUSEC);
270100150317           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
270200150317                         SizeList : NotUsed : QUSEC );
270300140220         EndFor;
270400140220
270500140220         // -?Next step - Signal end of input, clear subfile for reload.?
270600140220         QLGRT00 = c_EndPut;
270700140220         clear  QUSEI;
270800140220         QUSBPRV = %size(QUSEC);
270900150317         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
271000150317                       SizeList : NotUsed : QUSEC );
271100140220
271200140220         // -?Pulizia subfile?
271300140220         *in20 = *off;
271400140220         *in21 = *off;
271500140220         *in22 = *on;
271600140220         write  FIA2C03;
271700140220         *in22 = *off;
271800140220
271900140220         // -?Final step - Write the records back to the subfile.?
272000140220         QLGRT00  = c_Get;
272100140220         For  Nrr = 1  To RrnLast;
272200140220           clear  QUSEI;
272300140220           QUSBPRV = %size(QUSEC);
272400150317           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
272500140220                         QLGRL00  : NotUsed : QUSEC );
272600140220           S03nrr = Nrr;
272700140220           write  FIA2S03;
272800140220         EndFor;
272900140220
273000140220         // -?Visualizzazione del SFL (se ci sono dati)?
273100140220         *in20 = (S03nrr > *zero);
273200140220         *in21 = *in20;
273300140220
273400140220         // -?Impostazione nuova descrizione tasto F11=Ord. x ...?
273500140220         Select;
273600140220           When  $Ord_Desc;
273700140221             Z02F11 = 'F11=Ord.Ascendente ';
273800140220           When  Not  $Ord_Desc;
273900140221             Z02F11 = 'F11=Ord.Discendente';
274000140220         EndSl;
274100140220
274200140220       ENDSR;
274300140220
274400140220      /end-free
274500140220
274600040227      *---------------------------------------------------------------*
274700040227      * Routine Lettura  SUBFILE                                      *
274800040227      *---------------------------------------------------------------*
274900040227     c     SR_READSF     BegSR
275000040227
275100040315     c                   eval      W_f03 = ' '
275200040315
275300040227     c                   do        *hival
275400040227      *
275500150319     c                   if        wMod = 'R'  and  (iA2tor = 'S'
275600140526     c                                          or   iA2tor = 'O')
275700140220      *
275800140220     c                   readc     FIA2S03
275900140220      *
276000150319     c                   else
276100140220      *
276200040227     c                   readc     FIA2S02
276300140220      *
276400150319     c                   endif
276500040227      *
276600150504     c                   if        %eof(fidna2d)
276700040227     c                   leave
276800040227     c                   endif
276900040227      *
277000040227      * se opzione = a 2 nel caso di programma richiamto in sola modalità
277100040227      * interrogazione do errore
277200040304     c                   If        vs1opz = '2' and Wmod = 'R' and IA2MOD = 'I'
277300040227     c                   eval      *in70 = *on
277400040227     c                   eval      *in90 = *on
277500150319     c                   if        wMod = 'R'  and  (iA2tor = 'S'
277600140526     c                                          or   iA2tor = 'O')
277700140220     c                   update    FIA2S03
277800140220     c                   z-add     S03nrr        v2cnrr
277900150319     c                   else
278000141217     c                   eval      *in06 = VSHin06
278100040317     c                   update    fia2s02
278200040317     c                   z-add     s01nrr        v2cnrr
278300150319     c                   endif
278400150507     c                   z-add     V2Cnrr        V2CrtnCsr
278500040227     c                   leave
278600040227     c                   endif
278700150506      *
278800040317      * se opzione = a 2 nel caso di reclamo non gestibile dal P.O. do errore
278900040317      *
279000040317     c                   If        vs1opz = '2' and vs1ges = 'N'
279100040317     c                   eval      *in71 = *on
279200040317     c                   eval      *in90 = *on
279300150319     c                   if        wMod = 'R'  and  (iA2tor = 'S'
279400140526     c                                          or   iA2tor = 'O')
279500140220     c                   update    FIA2S03
279600140220     c                   z-add     S03nrr        v2cnrr
279700150319     c                   else
279800141217     c                   eval      *in06 = VSHin06
279900040317     c                   update    fia2s02
280000040317     c                   z-add     s01nrr        v2cnrr
280100150319     c                   endif
280200150507     c                   z-add     V2Cnrr        V2CrtnCsr
280300040317     c                   leave
280400040317     c                   endif
280500150506      *
280600140217      * se opzione = a 2 nel caso di reclamo non gestibile xchè motivo apertura solo tecnico
280700140217      *
280800140217     c                   If        vs1opz = '2' and vs1ges = 'V'
280900140217     c                   eval      *in69 = *on
281000140217     c                   eval      *in90 = *on
281100150319     c                   if        wMod = 'R'  and  (iA2tor = 'S'
281200140526     c                                          or   iA2tor = 'O')
281300140220     c                   update    FIA2S03
281400140220     c                   z-add     S03nrr        v2cnrr
281500150319     c                   else
281600141217     c                   eval      *in06 = VSHin06
281700140217     c                   update    fia2s02
281800140217     c                   z-add     s01nrr        v2cnrr
281900150319     c                   endif
282000150507     c                   z-add     V2Cnrr        V2CrtnCsr
282100140217     c                   leave
282200140217     c                   endif
282300150506      *
282400150317      * se opzione = a 2 nel caso di sola interrogazione do errore
282500070601      *
282600070601     c                   If        vs1opz = '2' and *in05 = *on
282700070601     c                   eval      *in70 = *on
282800070601     c                   eval      *in90 = *on
282900150319     c                   if        wMod = 'R'  and  (iA2tor = 'S'
283000140526     c                                          or   iA2tor = 'O')
283100140220     c                   update    FIA2S03
283200140220     c                   z-add     S03nrr        v2cnrr
283300150319     c                   else
283400141217     c                   eval      *in06 = VSHin06
283500070601     c                   update    fia2s02
283600070601     c                   z-add     s01nrr        v2cnrr
283700150319     c                   endif
283800150507     c                   z-add     V2Cnrr        V2CrtnCsr
283900070601     c                   leave
284000150317     c                   endif
284100040227      *
284200040227      * se opzione = a 5 emetto la videata di visualizzazione
284300040227     c                   If        vs1opz = '5'
284400040227     c                   exsr      SR_visual
284500150506     c                   if        wMod = 'R'  and  (iA2tor = 'S'
284600150506     c                                          or   iA2tor = 'O')
284700150506     c                   z-add     S03nrr        V2Cnrr
284800150506     c                   else
284900150507     c                   z-add     S01nrr        V2Cnrr
285000150506     c                   endif
285100150507     c                   z-add     V2Cnrr        V2CrtnCsr
285200040227     c                   endif
285300040227      *
285400040227      * se opzione = a 2 richiamo il programma di manutenzione
285500040227     c                   If        vs1opz = '2'
285600040227     c                   clear                   fidna0ds
285700040227     c                   eval      ia0mod = 'M'
285800040421     c                   eval      ia0pot = vs1pot
285900040421     c                   eval      ia0ant = vs1ant
286000040227     c                   eval      ia0nut = vs1nut
286100040227     c                   eval      ia0pdt = vs1pdt
286200040227      *
286300161027     c                   CALL      'FIDNA1R'
286400040227     c                   PARM                    KPJBA
286500040227     c                   PARM                    FIDNA0ds
286600040315      * GESTISCO F3 DI RITORNO
286700040305     c                   if        oa0f03 = 'S'
286800040315     c                   eval      W_f03 = 'S'
286900040305     c                   leave
287000040305     c                   endif
287100150506     c                   eval      $InzSFL = *on
287200040227
287300040227     c                   endif
287400040227      *
287500040227     c                   clear                   vs1opz
287600150319     c                   if        wMod = 'R'  and  (iA2tor = 'S'
287700140526     c                                          or   iA2tor = 'O')
287800140220     c                   update    FIA2S03
287900150319     c                   else
288000141217     c                   eval      *in06 = VSHin06
288100040227     c                   update    fia2s02
288200150319     c                   endif
288300040227      *
288400040227     c                   Enddo
288500040227
288600040227     c                   Endsr
288700150312
288800040302      *---------------------------------------------------------------*
288900040302      * Routine visualizzazione dettaglio                             *
289000040302      *---------------------------------------------------------------*
289100040302     c     SR_visual     BegSR
289200040302
289300040302      * caricamento dei dati a video
289400040302
289500040302     c                   Exsr      SR_carv02
289600040302
289700140221     c                   clear                   V1Togg
289800040302     c                   write     fia2t01
289900040302     c                   do        *hival
290000040302
290100040302     c                   exfmt     fia2d02
290200150319
290300150319     c                   SELECT
290400040302
290500040302      * F12 - RITORNO
290600040302
290700150319     C                   WHEN      *INKL
290800040302     c                   leave
290900040302
291000040302      * F18 - NOTE
291100040302
291200150319     c                   WHEN      *INKS
291300040302      *
291400040302     C                   clear                   FIDNB0DS
291500040305     c                   z-add     v02pot        iB0pot
291600040421     c                   z-add     v02ant        iB0ant
291700040302     c                   z-add     v02nut        iB0nut
291800040302     c                   z-add     v02pdt        iB0pdt
291900040305
292000040305     c                   z-add     tgddar        ib0dar
292100040305     c                   z-add     v02orr        ib0orr
292200040305
292300040305     c                   move      v02mad        ib0mad
292400040305     c                   movel     vd2mad        ib0dma
292500040305
292600040305     c                   movel     vs1ogg        Ib0ogg
292700040302     c                   movel     'GEN'         iB0tIN
292800040303     c                   movel     'I'           iB0flm
292900040302      *
293000040302     C                   CALL      'FIDNB0R'
293100040302     C                   PARM                    kpjba
293200040302     C                   PARM                    FIDNB0DS
293300150319
293400150403      * F11 = Storico
293500150319
293600150319     c                   WHEN      *INKK
293700150319      *
293800150319     c                   clear                   FIDNA0ds
293900150319     c                   eval      iA0pot = V02pot
294000150319     c                   eval      iA0ant = V02ant
294100150319     c                   eval      iA0nut = V02nut
294200150319     c                   eval      iA0pdt = V02pdt
294300150319     c                   call      'FIDNA3R'
294400150319     c                   parm                    KPJBA
294500150319     c                   parm                    FIDNA0ds
294600040302
294700040405      * F02 = Interrogazione Oggetto
294800040302
294900150319     c                   WHEN      *INKB
295000040302
295100040302      * SPEDIZIONI
295200040302
295300041008      * in filiale
295400150319     c                   If        *IN01 and
295500150319     c                             DUTlpo <> 'S'
295600040419
295700160706     c                   clear                   KPJBU
295800160706     c                   clear                   ParamU6ds1
295900160706     c                   eval      ParamU6ds1.V2Caas = V02ann
296000160706     c                   eval      ParamU6ds1.COMlnp = V02lnp
296100160706     c                   eval      ParamU6ds1.V2Cnrs = V02nrs
296200160706     c                   eval      ParamU6ds1.V2Cnsp = V02nsp
296300160706     c                   eval      KPJBU = ParamU6ds1
296400160706      * preparo l'altro parametro da passare
296500160706     c                   clear                   fnlrU6ds
296600160706     c*//                eval      iLRU6sel = 'S'
296700160706     c                   eval      iLRU6ra  = 'S'
296800160706      * -?Campo IA1ins già impostato nella subr. sr_CountRA?
296900160706     c                   eval      eLRU6ins = iA1ins
297000160706      *
297100160706     c                   call      'FNLRU6R1'
297200160706     c                   parm                    KPJBA
297300160706     c                   parm                    fnlrU6ds
297400160706     c                   movel     kpjbu         ParamU6ds1
297500160706      * spedizione non più esitente
297600160706     c                   if        oLRU6err = 'E'
297700160706     c                   eval      *in54 = *on
297800160706     c*//                movel     oLRU6msg      V1Cmsg
297900160706     c                   iter
298000160706     c                   endif
298100041011
298200041011     C                   ENDIF
298300150319
298400041008      * in sede
298500150319     c                   If        *IN01 and
298600150319     c                             DUTlpo = 'S'
298700041008
298800041008     c                   clear                   TNSB50DS
298900041008     C                   movel     'R05'         I50OP0
299000041008     C                   z-add     V02ann        D50aas
299100041008     C                   z-add     V02lnp        D50lnp
299200041008     C                   z-add     V02nrs        D50nrs
299300041008     C                   z-add     V02nsp        D50nsp
299400041008     C                   movel(P)  TNSB50DS      KPJBU
299500041008      *
299600041008      * se richiamato
299700150610      *//* -?Campo IA1ins già impostato nella subr. sr_CountRA?
299800150610     c*//                clear                   fidna1ds
299900150610     c*//                eval      IA1INS = 'N'
300000041008      *
300100041008     C                   call      'TNSB50R'
300200041008     C                   parm                    KPJBA
300300041008     C                   parm                    FIDNA1DS
300400041008     c                   endif
300500040302
300600040302      * ORM
300700040302
300800150319     c                   If        *IN02
300900040419      *
301000040419     *** * interrogo direttamente con il richiano di FIOR07R
301100040419     *** * => NON più "direttamente"
301200040419     c                   if        NOT %open(FNORM01L)
301300040419     c                   open      FNORM01L
301400040419     c                   endif
301500040419      *
301600040419      * verifica dell'esistenza su FNORM
301700040419     c     k04orm01      setll     FNORM000
301800040419     c                   if        NOT %equal(FNORM01L)
301900040419     c                   eval      *in54 = *on
302000040419     c                   iter
302100040419     c                   endif
302200040419      *
302300040419      * interrogo con il richiamo di FIOR07R
302400040302     C                   clear                   PARM01
302500040302     C                   move      v02poe        ppoe
302600040305     C                   move      v02nso        pnsr
302700040305     C                   move      v02orm        pnor
302800040302     C                   move      v02nrv        pnrv
302900040302     C                   move      '5'           psce
303000040302      * attenzione non muovo nulla nel campo del p.o. gestione .... vediamo se funziona !!!!!!
303100040302     C                   movel(p)  PARM01        KPJBU
303200040305
303300150610      *//* -?Campo IA1ins già impostato nella subr. sr_CountRA?
303400150610     c*//                clear                   fidna1ds
303500150610     c*//                eval      IA1INS = 'N'
303600040305
303700040302
303800040302     C                   call      'FIOR07R'
303900040302     C                   parm                    KPJBA
304000040305     C                   parm                    FIDNA1DS
304100040302
304200040302     c                   endif
304300150319
304400150319     c                   ENDSL
304500040302
304600040302     c                   enddo
304700040302
304800040302     c                   Endsr
304900040227      *---------------------------------------------------------------*
305000040302      * Routine caricamento seconda videata                           *
305100040227      *---------------------------------------------------------------*
305200040302     c     SR_CARV02     BegSR
305300040301
305400040301      * resetto qualche indicatore
305500040301     c                   setoff                                       042901
305600040301     c                   setoff                                       0203
305700150319     c                   eval      *in16 = *off
305800050120      * pulisco alcuni campi
305900050120     c                   clear                   vd2ksc
306000151221     c                   clear                   dTGDfar
306100151221     c                   clear                   dTGD01
306200151221     c                   clear                   dTGR01a
306300040227
306400040301      * aggancio il record da visualizzare
306500040301
306600160104     c     ktgd01        chain     fitgd000
306700040301
306800151221if  1c                   If        %found(fitgd01l)
306900151221
307000151221     c                   eval      dTGDfar = TGDfar
307100151221     c                   eval      dTGD01  = TGDflo
307200040301      *
307300040301     c                   z-add     tgdpot        v02pot                         P.O. apertura
307400040421     c                   z-add     tgdant        v02ant                         Anno   reclamo
307500040301     c                   z-add     tgdnut        v02nut                         Numero reclamo
307600040301     c                   z-add     tgdpdt        v02pdt                         progressivo riga
307700040301      *
307800040301     c                   movel     tgdtco        v02tco                         tipo comunicazione
307900040301      * decodifica
308000040301     C                   clear                   vd2tco
308100040301     c                   clear                   tibs02ds
308200040301     C                   MOVEL     'C'           t02mod
308300040301     C                   MOVEL     knsif         t02sif
308400040301     C                   MOVEL     'COR'         t02cod
308500040301     C                   MOVEL     v02tco        t02ke1
308600040301      *
308700040301     C                   CALL      'TIBS02R'
308800040301     C                   PARM                    KPJBA
308900040301     C                   PARM                    TIBS02DS
309000040301      *
309100040419     C                   IF        t02err = *BLANKS
309200040301     C                   movel     T02uni        DCOR
309300040301     C                   movel     §CORdesc      vd2tco                         decod. tp comunic
309400040301     c                   endif
309500040301      * data apertura
309600040301     c                   clear                   WLBdat
309700040301     c                   eval      G02inv = tgddar
309800040301     c                   eval      G02err = '3'
309900040301     c                   call      'XSRDA8'
310000040301     c                   parm                    WLBdat
310100040301     c                   z-add     G02dat        v02dar                         data ricez.comunicaz
310200040301      *
310300040301     c                   z-add     tgdorr        v02orr                         ora  ricez.comunicaz
310400040301      * data creazione
310500040301     c                   clear                   WLBdat
310600040301     c                   eval      G02inv = tgddac
310700040301     c                   eval      G02err = '3'
310800040301     c                   call      'XSRDA8'
310900040301     c                   parm                    WLBdat
311000040301     c                   z-add     G02dat        v02dac                         data creazione
311100040301      *
311200040301     c                   z-add     tgdorc        v02orc                         ora  creazione
311300040301      * utente
311400040301     c                   movel     tgdpru        v02pru                         utente creazione
311500040301      *
311600050117     c                   movel     tgdksc        v02ksc                         codice cliente
311700050118     C                   clear                   VD2KSC
311800151221if  2c                   If        v02ksc > 0
311900050117     C                   clear                   TIBS69DS
312000050117     c                   z-add     v02ksc        I69kac
312100050117     C                   call      'TIBS69R'
312200050117     C                   parm                    tibs69DS
312300050117     C                   parm                    DS_cnaco
312400050117     C                   parm                    DS_cnind
312500050117     C                   parm                    DS_cnclp
312600050117     C                   parm                    DS_fncls
312700050117     C                   eval      wtibs69r = '1'
312800050117     c                   If         O69ERR =  ' '
312900050117     c                   movel     ACOrag        vd2ksc                         decodifica cliente
313000050117     c                   endif
313100151221e   2c                   endif
313200050117      *
313300040301     c                   movel     tgdrsc        v02rsc                         chiamante
313400040301     c                   movel     tgdclv        v02clv                         categoria chiamante
313500040301      * recupero i riferimenti reclamo
313600040301     c                   clear                   v2refe
313700040301     c                   clear                   v2ntel
313800040301     c                   clear                   v2mail
313900040301     c                   clear                   v2nfax
314000040301      *
314100160104     c     Ktgr          setll     fitgr000
314200151221do  2c                   do        *hival
314300160104     c     Ktgr          reade     fitgr000
314400040301
314500040301     c                   if        %eof(fitgr01l)
314600040301     c                   leave
314700040301     c                   endif
314800040301
314900040301      * referente
315000040301     c                   if        tgrtri = 'R'
315100040301     c                   eval      v2refe = tgrrif
315200040301     c                   endif
315300040301      * mail
315400040301     c                   if        tgrtri = 'M'
315500040303     c                   eval      v2mail = tgrrif
315600040301     c                   endif
315700040301      * numeri telefonoici
315800040301     c                   if        tgrtri = 'N'
315900040301     c                   movel     tgrrif        v2ntel
316000040301     c                   move      tgrrif        v2nfax
316100040301     c                   endif
316200151221      * -?Reparto Attesa Risposta?
316300151221     c                   if        TGRtri = 'A'  and
316400151221     c                             TGRpdt = TGDpdt
316500151221     c                   eval      dTGR01a  = TGRrif
316600151221     c                   endif
316700040301
316800151221e   2c                   enddo
316900040301      * oggetto
317000040301     c                   clear                   tibs02ds
317100040301     C                   MOVEL     'C'           t02mod
317200040301     C                   MOVEL     knsif         t02sif
317300040301     C                   MOVEL     'OGR'         t02cod
317400040301     C                   MOVEL     tgdtor        t02ke1
317500040301      *
317600040301     C                   CALL      'TIBS02R'
317700040301     C                   PARM                    KPJBA
317800040301     C                   PARM                    TIBS02DS
317900040301      *
318000040301     C                   movel     T02uni        DOGR
318100040301     C                   movel     §ogrdesc      vd2tor
318200040301      * spedizione
318300040301     c                   if        tgdtor = 'S'
318400040301     c                   eval      *in01 = *on
318500040301     c                   movel     tgdogg        spevid2
318600040301     c                   endif
318700040301      * ORM
318800040301     c                   if        tgdtor = 'O'
318900040301     c                   eval      *in02 = *on
319000040301     c                   movel     tgdogg        ormvid2
319100040301     c                   endif
319200040301      * GENERICO
319300040301     c                   if        tgdtor = 'G'
319400040301     c                   eval      *in03 = *on
319500040301     c                   movel     tgdogg        v02ogg
319600040301     c                   endif
319700040301      * CHI CHIAMA
319800040301     c                   clear                   tibs02ds
319900040301     C                   MOVEL     'C'           t02mod
320000040301     C                   MOVEL     knsif         t02sif
320100040301     C                   MOVEL     'CCR'         t02cod
320200040301     C                   MOVEL     tgdtch        t02ke1
320300040301      *
320400040301     C                   CALL      'TIBS02R'
320500040301     C                   PARM                    KPJBA
320600040301     C                   PARM                    TIBS02DS
320700040301      *
320800040301     C                   movel     T02uni        DCCR
320900040301     C                   movel     §CCRdesc      vd2tch
321000040301      * MOTIVO APERTURA
321100040301     c                   clear                   tibs02ds
321200040301     C                   MOVEL     'C'           t02mod
321300040301     C                   MOVEL     knsif         t02sif
321400040301     C                   MOVEL     'MAR'         t02cod
321500040301     C                   MOVEL     tgdmad        t02ke1
321600040301      *
321700040301     C                   CALL      'TIBS02R'
321800040301     C                   PARM                    KPJBA
321900040301     C                   PARM                    TIBS02DS
322000040301      *
322100040301     C                   movel     T02uni        DMAR
322200040301     C                   movel     tgdmad        v02mad
322300040301     C                   movel     §MARdesc      vd2mad
322400040301      * recupero le note
322500040301      *
322600040301     C                   clear                   FIDNB0DS
322700040421     c                   z-add     v02ant        iB0ant
322800040301     c                   z-add     v02nut        iB0nut
322900040302     c                   z-add     v02pdt        iB0pdt
323000040301     c                   movel     'GEN'         iB0tIN
323100040301     c                   movel     'R'           iB0flm
323200040301      *
323300040301     C                   CALL      'FIDNB0R'
323400040301     C                   PARM                    kpjba
323500040301     C                   PARM                    FIDNB0DS
323600040301      *
323700040301      * valorizzo le prime 4 note a video
323800040301      *
323900040301     c                   movel     DB0NO1        v02no1
324000040301     c                   movel     DB0NO2        v02no2
324100040301     c                   movel     DB0NO3        v02no3
324200040301     c                   movel     DB0NO4        v02no4
324300040301      * se ci sono altre note accendo il 29
324400040301     c                   eval      *in29 = (ob0piu = '+')
324500040302      * reparto che ha in gestione il reclamo
324600040302     c                   movel     tgdrpg        v02rpg
324700040618      * P.O.    che ha in gestione il reclamo
324800040618     c                   movel     tgdpog        v02pog
324900150324      * stato (ex priorità)
325000040302     c                   movel     tgdprt        v02prt
325100150325     c                   clear                   VD2prt
325200150325     c                   clear                   TIBS02ds
325300150325     c                   eval      T02mod = 'C'
325400150325     c                   eval      T02sif = knsif
325500150325     c                   eval      T02cod = 'PRR'
325600150325     c                   eval      T02ke1 = TGDprt
325700150325     c                   call      'TIBS02R'
325800150325     c                   parm                    KPJBA
325900150325     c                   parm                    TIBS02ds
326000150325     C                   eval      dPRR = T02uni
326100150325     C                   eval      VD2prt = §PRRdesc
326200151221      *//* responsabilità
326300151221     c*//                   movel     tgdrsp        v02rsp
326400151221      *
326500160129      /free
326600160129
326700160129         // -?Visualizzazione "Attesa o Ricevuta Risposta"?
326800160129         clear  V02AttRis;
326900160129         Select;
327000160129           When  dTGDfar.§TGDatt = 'S'  and  dTGDfar.§TGDris = 'S';
327100160201             exsr  sr_ReperimentoRispostaRicevuta;
327200160201             if  WrepR = *blank;
327300160203               V02AttRis = 'Risposta di Ritorno   RICEVUTA   dal Reparto '
327400160201                         + %trimR( dTGR01a.§TGRrepR ) + ' della Filiale '
327500160201                         + %editc( dTGD01.§TGDfilR : 'X' );
327600160201             else;
327700160203               V02AttRis = 'Risposta di Ritorno   RICEVUTA   dall''Utente '
327800160201                         + %trimR( WrepR ) + ' della Filiale '
327900160201                         + %editc( WfilR : 'X' );
328000160201             endif;
328100160129           When  dTGDfar.§TGDatt = 'S';
328200160203             V02AttRis = 'Risposta di Ritorno   ATTESA   dal Reparto '
328300160129                       + %trimR( dTGR01a.§TGRrepR ) + ' della Filiale '
328400160129                       + %editc( dTGD01.§TGDfilR : 'X' );
328500160129         EndSl;
328600160129
328700160129      /end-free
328800151221      *
328900160129      * P.O. responsabilità
329000040302     c                   clear                   vd2por
329100040302     c                   eval      v02por = tgdpor
329200040302
329300160104     C     tgdpor        chain     azorg
329400040302    3C                   if        %found(azorg01l)
329500040302     C                   movel     orgdes        vd2por
329600040302     c                   endif
329700040302      * chiuso
329800040302     c                   clear                   v02dcd
329900040302     c                   clear                   v02ocd
330000040302     c                   clear                   v02pct
330100040302     c                   clear                   v02utc
330200040302     c                   clear                   v02cdt
330300040302     c                   clear                   vd2cdt
330400040302      *
330500151221if  2c                   if        tgdcdt <> *blank
330600040302     c                   eval      *in04 = *on
330700040302      * data chiusura
330800040302     c                   clear                   WLBdat
330900040302     c                   eval      G02inv = tgddcd
331000040302     c                   eval      G02err = '3'
331100040302     c                   call      'XSRDA8'
331200040302     c                   parm                    WLBdat
331300040302     c                   z-add     G02dat        v02dcd                         data chiuusra
331400040302      *
331500040302     c                   z-add     tgdocd        v02ocd                         ora  chiusura
331600040302      * utente
331700040302     c                   movel     tgdutc        v02utc                         utente chiuusra
331800040302     c                   movel     tgdpct        v02pct                         P.O.   chiusura
331900040302      * causale
332000040302     c                   clear                   tibs02ds
332100040302     C                   MOVEL     'C'           t02mod
332200040302     C                   MOVEL     knsif         t02sif
332300040302     C                   MOVEL     'CHR'         t02cod
332400040302     C                   MOVEL     tgdcdt        t02ke1
332500040302      *
332600040302     C                   CALL      'TIBS02R'
332700040302     C                   PARM                    KPJBA
332800040302     C                   PARM                    TIBS02DS
332900040302      *
333000040302     C                   movel     T02uni        DCHR
333100040302     C                   movel     tgdcdt        v02cdt
333200040302     C                   movel     §CHRdesc      vd2cdt
333300040302      *
333400151221e   2c                   endif
333500040302
333600151221e   1c                   endif
333700150319
333800150403      * -?Controllo se attivare o no il tasto F11=Storico?
333900150319     c     ktgd01        setll     FITGM000
334000150319     c                   eval      *in16 = ( %equal(FITGM01L) )
334100150610
334200150610      * -?Attivo l'abilitazione del tasto F16=Gest.Rich. nei *pgm chiamati?
334300150611      *  ?SOLO SE:?
334400150611      *  ?· lanciato da menù?
334500150611      *  ?  (non già richiamato da altro *pgm via F16=Gest.Rich.)?
334600150611      *  ?e    SE?
334700150611      *  ?· trattasi di spedizione o O.R.M.?
334800150611      *  ?e    SE?
334900150611      *  ?· a fronte dello stesso oggetto ci sono più di una R.A.?
335000150611     c                   if        wMod <> 'R'  and  ( TGDtor = 'S'
335100150611     c                                           or    TGDtor = 'O' )
335200150610     c                   exsr      sr_CountRA
335300150611     c                   endif
335400040301
335500040227     c                   Endsr
335600150610
335700150610      /free
335800160201
335900160201       BEGSR  sr_ReperimentoRispostaRicevuta;
336000160201
336100160201         clear  WrepR;
336200160201         clear  WfilR;
336300160201
336400160204         setGT   ( V02ant : V02nut : V02pdt : 'A' )  FITGM000;
336500160204         readPE  ( V02ant : V02nut : V02pdt : 'A' )  FITGM000;
336600160201
336700160204         if  %eof( FITGM01L );
336800160201           leavesr;
336900160201         endif;
337000160201
337100160201         WrepR = TGMutn;
337200160201         WfilR = TGMpon;
337300160201
337400160201       ENDSR;
337500150610
337600150610       //--------------------------------------------------------------
337700150610       //?Conto quante RA ci sono a fronte dello stesso oggetto        ?
337800150610       //--------------------------------------------------------------
337900150610       BEGSR  sr_CountRA;
338000150610
338100150610         clear  fidnA1ds;
338200150610         clear  wCountRA;
338300150610
338400150610         exec sql   select count(*) into :wCountRA
338500150610                      from FITGD00F
338600150610                     where TGDtor = :TGDtor
338700150610                       and TGDogg = :TGDogg;
338800150610
338900150610         select;
339000150610           when  SQLcode = 100  or  SQLcode < *zero;
339100150610             iA1ins = 'N';
339200150610           when  wCountRA <= 1;
339300150610             iA1ins = 'N';
339400150610           when  wCountRA > 1;
339500150610             iA1ins = 'I';
339600150610         endsl;
339700150610
339800150610       ENDSR;
339900150610
340000150610      /end-free
340100150610
340200041215      *****************************************************************
340300041215      *  Ricerca alfabetica
340400041215      *****************************************************************
340500041215     c     SR_RICALF     begsr
340600041215      *
340700041215     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
340800050117     C                   Z-ADD     2             PAXkut                         tipo estrazione
340900041215     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
341000041215     C                   Z-ADD     DUTKCI        PAXkcc                         Capoconto Clienti
341100041215     c                   movel(P)  VD1ksc        PAXrag
341200041215     C                   z-add     1             PAXnum
341300041215      *
341400041215     C                   CALL      'XALFA3BR'
341500041215     C                   PARM                    PAXDUT           30
341600041215     C                   PARM                    PAXkut            1 0
341700041215     C                   PARM                    PAXrag           48
341800041215     C                   PARM                    PAXkcc            4 0
341900041215     C                   PARM                    PAXsta            1 0
342000041215     C                   PARM      *blanks       PAXflr           90
342100041215     C                   PARM      *blanks       PAXdit            3
342200041215     C                   PARM                    PAXnum            2 0
342300041215     C                   PARM                    PAXkcm           80
342400041215     C                   PARM                    PAXksm          140
342500041215     C                   PARM                    PAXkdm           60
342600041215      *
342700041215      * NON TROVATO O PREMUTO CMD12
342800041215    2C                   If        paxsta > -1
342900041215     C                   movel     PAXksm        V01KSC
343000041215     C                   movel     PAXrag        VD1KSC
343100041215    2C                   ENDIF
343200041215      *
343300041215     c                   endsr
343400140926
343500140926      /free
343600140926       //--------------------------------------------------------------
343700140926       //?Abilito F19=Disposizioni di Consegna.
343800140926       //--------------------------------------------------------------
343900140926       BEGSR AbiF19;
344000140926
344100140926         *in19 = *off;
344200150123
344300150123       //?SE sono in Sede: FNARB non c'è!
344400150123         if  DUTlpo = 'S';
344500150123           leavesr;
344600150123         endif;
344700140926
344800140926       //?Se devo emettere il subfile usato quando il pgm è richiamato
344900141007         IF  wmod = 'R' and IA2tor = 'S';
345000140926
345100140926         //?Controllo che il pgm non sia stato richiamato dal pgm
345200140926         //?gestione disposizioni di consegna
345300140926           XRTVSTKR (pStack:pStartPoint:pEntries);
345400140926           dsStack = pStack;
345500140926           IF  %lookup('FNLRY03R   ':skStack) > 0;
345600140926             leavesr;
345700140926           ENDIF;
345800140926
345900140926         //?Se non arriva da gestione disposizioni di consegna
346000140926         //?Verifico se ci sono disposizioni di consegna da gestire
346100141029           clear FNLRY09DS2;
346200140926           ILRY09tch = 'C';
346300140926           ILRY09aas = %dec(WRKann:4:0);
346400140926           ILRY09lnp = %dec(WRKlnp:3:0);
346500140926           ILRY09nrs = %dec(WRKnrs:2:0);
346600140926           ILRY09nsp = %dec(WRKnsp:7:0);
346700141029           ILRY09df12 = 'S';
346800141029           ILRY09df21 = 'N';
346900141029           ILRY09df06 = 'S';
347000141029           ILRY09ef12 = 'S';
347100141029           ILRY09ef21 = 'S';
347200141029           ILRY09ef06 = 'S';
347300141029           kpjbu = FNLRY09DS2;
347400141029           fnlry09r (kpjba);
347500141029           FNLRY09DS2 = kpjbu;
347600140929           IF  OLRY09esit = '0' or OLRY09err <> *blanks;
347700140926             leavesr;
347800140926           ENDIF;
347900140926
348000140926         //?Se ho trovato delle disposizioni di consegna ancora da gestire
348100140926         //?verifico se l'utente è abilitato alla manutenzione della bolla
348200140926           IF  not %open(FNARB01L);
348300140926             open FNARB01L;
348400140926           ENDIF;
348500160104           chain (ILRY09aas:ILRY09lnp:ILRY09nrs:ILRY09nsp) FNARB000;
348600170228
348700170228           IF  not %found(FNARB01L);
348800170228             if  not %open(FNBLP01L);
348900170228               open  FNBLP01L;
349000170228             endif;
349100170228             chain (ILRY09aas:ILRY09lnp:ILRY09nrs:ILRY09nsp) FNBLP000;
349200170228           ENDIF;
349300170228
349400170228           Select;
349500170228             // -?Bolla NON trovata (né in ARB né in BLP)?
349600170228             When  not %found(FNARB01L)   and
349700170228                   not %found(FNBLP01L);
349800170228               leavesr;
349900170228             // -?Bolla trovata ARB, ma utente NON abilitato alla LNA?
350000170228             When  %found(FNARB01L)  and
350100170228                   %lookup( ARBlna : sk_POg1 ) = 0;
350200170228               leavesr;
350300170228             // -?Bolla trovata BLP, ma utente NON abilitato alla LNA?
350400170228             When  Not %found(FNARB01L)  and
350500170228                   %found(FNBLP01L)      and
350600170228                   %lookup( BLPlna : sk_POg1 ) = 0;
350700170228               leavesr;
350800170228             // -?La bolla dev'essere chiusa in partenza per?
350900170228             //  ?merce MAI affidata?
351000170228             When  Not %found(FNARB01L)  and
351100170228                   %found(FNBLP01L)      and
351200170228                   ( BLPcca  = *blanks   or
351300170228                     BLPft2 <> 'N' );
351400170228               leavesr;
351500170228           EndSl;
351600140926
351700140926         //?Se arrivo qua vuol dire che posso abilitare F19
351800140926           *in19 = *on;
351900140926         ENDIF;
352000140926
352100140926       ENDSR;
352200140926
352300140926       //--------------------------------------------------------------
352400140926       //?Eseguo F19=Disposizioni di Consegna.
352500140926       //--------------------------------------------------------------
352600140926       BEGSR sr_F19;
352700140926
352800140926       //?Richiamo pgm gestione disposizioni di consegna
352900141029         clear FNLRY09DS2;
353000140926         ILRY09tch = 'E';
353100140926         ILRY09aas = %dec(WRKann:4:0);
353200140926         ILRY09lnp = %dec(WRKlnp:3:0);
353300140926         ILRY09nrs = %dec(WRKnrs:2:0);
353400140926         ILRY09nsp = %dec(WRKnsp:7:0);
353500141029         ILRY09df12 = 'S';
353600141029         ILRY09df21 = 'N';
353700141029         ILRY09df06 = 'S';
353800141029         ILRY09ef12 = 'S';
353900141029         ILRY09ef21 = 'S';
354000141029         ILRY09ef06 = 'S';
354100141029         kpjbu = FNLRY09DS2;
354200141029         fnlry09r (kpjba);
354300141029         FNLRY09DS2 = kpjbu;
354400140926
354500140926       //?Ricarico il subfile
354600140926         exsr sr_carsbf;
354700140926
354800140926       //?Ricontrollo se abilitare o meno F19
354900140926         exsr AbiF19;
355000140926
355100140926       ENDSR;
355200140926
355300140926      /end-free
355400140926
355500040225      *---------------------------------------------------------------*
355600040225      * Routine Iniziale                                              *
355700040225      *---------------------------------------------------------------*
355800040225     c     *InzSR        BegSR
355900040225      *
356000040225     c     *Entry        plist
356100040225     c                   parm                    KPJBA
356200040225     c                   parm                    FIDNA2DS
356300040225      * Reperisco data corrente
356400040225     c                   time                    w0140
356500040225     c                   move      w0140         wData
356600040225     c                   clear                   WLBdat
356700040225     c                   eval      G02dat = wData
356800040225     c                   call      'XSRDA8'
356900040225     c                   parm                    WLBdat
357000040225     c                   z-add     G02inv        DateU
357100040225      *
357200040225      * Reperisco dati job
357300040225     c                   exsr      DatiJob
357400040225     c                   movel     SDSpgm        VTCpgm
357500160202
357600160202      * Se sono in Sede accendo 14
357700160202     c                   eval      *in14 = ( SIMfel = 0 )
357800160202
357900130806      * Imposto le abilitazioni dell'utente
358000130806     c                   eval      dute01 = UTEfaf
358100040617      * se utente partricolare recupero P.O. utenete da tabella UGR
358200040617     c                   movel     KNMUS         w_ute
358300040617     c                   clear                   tibs02ds
358400040617     C                   MOVEL     'C'           t02mod
358500040617     C                   MOVEL     knsif         t02sif
358600040617     C                   MOVEL     'UGR'         t02cod
358700040617     C                   MOVEL     W_ute         t02ke1
358800040617      *
358900040617     C                   CALL      'TIBS02R'
359000040617     C                   PARM                    KPJBA
359100040617     C                   PARM                    TIBS02DS
359200040617      *
359300040617     C                   IF        t02err = *BLANKS
359400040617      *
359500040617     c                   movel     t02uni        DUGR
359600040617      * se P.O. da forzare diverso da zero imposto in automatico
359700040617     c                   if        §UGRpofo > 0
359800040617     c                   eval      Dutpou = §UGRpofo
359900040617     c                   endif
360000040617      *
360100040617     c                   endif
360200041012      * RECUPERO I DATI STANDARD
360300041012     c                   clear                   tibs02ds
360400041012     C                   MOVEL     'C'           t02mod
360500041012     C                   MOVEL     knsif         t02sif
360600041012     C                   MOVEL     'REC'         t02cod
360700041012     C                   MOVEL     '1'           t02ke1
360800041012      *
360900041012     C                   CALL      'TIBS02R'
361000041012     C                   PARM                    KPJBA
361100041012     C                   PARM                    TIBS02DS
361200041012      *
361300041012     C                   IF        t02err = *BLANKS
361400041012     c                   movel     t02uni        DREC
361500041012     c                   endif
361600041012      * verifico se sono un utente che può modficare tutto senza controlli
361700130806     c                   if        §UTEvrac = 'S'
361800041012     c                   eval      *in07 = *on
361900041012     c                   endif
362000140926
362100140926      /free
362200150317
362300150317       //?Imposto abilitazione manutenzione bolle di filiale
362400140926       //?- se non c'è l'abilitazione
362500140926       //?  imposto abilitazione al terminal se utente di primo livello
362600140926       //?  imposto abilitazione alla filiale se utente di secondo livello
362700150428         clear  wAbi;
362800140926         IF  UTEaut = *blanks;
362900140926           IF  DUTlpo = '1';
363000140926             wabi = 'TP';
363100140926           ENDIF;
363200140926           IF  DUTlpo = '2';
363300140926             wabi = 'PO';
363400140926           ENDIF;
363500140926       //?- se c'è l'abilitazione la imposto
363600150428         ELSE;
363700140926         //?se presente imposto quella specifica delle bolle
363800140926           IF  §UTEmbf <> *blanks;
363900140926             wabi = §UTEmbf;
364000140926           ELSE;
364100140926         //?se non presente imposto quella generica del profilo
364200140926             wAbi = UTEaut;
364300140926           ENDIF;
364400140926         ENDIF;
364500140926
364600140926       //?- Carico le filiali abiliate
364700140926         clear TRUL31DS;
364800140926         I31abi = wabi;
364900140926         I31cdi = DUTdis;
365000140926         I31car = DUTare;
365100140926         I31cpo = DUTpou;
365200140926         callp TRUL31R (kpjba:trul31ds);
365300140926         IF  O31pog <= *zeros;
365400140926           O31pog = *all'0';
365500140926         ENDIF;
365600150317
365700150317         sk_POg_ds1 = O31pog;
365800150317
365900150317         // -?Imposto abilitaz. variazione Richiesta Assistenza clienti?
366000150428         clear  wAbi;
366100150317         Select;
366200150317           // -?se presente imposto l'abilitazione specifica?
366300150317           When  §UTErac <> *blank;
366400150317             wAbi = §UTErac;
366500150317           // -?se non presente imposto quella generica del profilo?
366600150317           When  UTEaut  <> *blank;
366700150317             wAbi = UTEaut;
366800150317           // -?se non c'è l'abilitazione & utente di 1° livello?
366900150317           //  ?imposto qualla al Terminal?
367000150317           When  DUTlpo  =  '1';
367100150317             wAbi = 'TP';
367200150317           // -?se non c'è l'abilitazione & utente di 2° livello?
367300150317           //  ?imposto qualla alla Filiale?
367400150317           When  DUTlpo  =  '2';
367500150317             wAbi = 'PO';
367600150317         EndSl;
367700150317
367800150723         // -?Carico le filiali abilitate?
367900150723         clear  sk_POg2;
368000150723         If  SIMfel <> *zero;
368100150723           clear TRUL31DS;
368200150723           I31abi = wabi;
368300150723           I31cdi = DUTdis;
368400150723           I31car = DUTare;
368500150723           I31cpo = DUTpou;
368600150723           TRUL31R ( kpjba : trul31ds );
368700150723           if  O31pog <= *zeros;
368800150723             O31pog = *all'0';
368900150723           endif;
369000150723           sk_POg_ds2 = O31pog;
369100150723         Else;
369200150723           sk_POg2(1) = c_Sede;
369300150723         EndIf;
369400150910
369500150910         if  sk_POg_ds2 <= *zero;
369600150910           sk_POg2(1) = DUTpou;
369700150910         endif;
369800150428
369900150428         // -?Aggiungo dUGR.§UGRpofo se utente in tab. "UGR"?
370000150428         clear  dUGR;
370100150428         clear  TIBS02ds;
370200150428         T02mod = 'C';
370300150428         T02sif = KNSIF;
370400150428         T02cod = 'UGR';
370500150428         T02ke1 = %subst( KNMUS : 1 : 3 );
370600150428         TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
370700150428         If  T02err = *blank;
370800150428           dUGR = T02uni;
370900150428           if  §UGRpofo > *zero;
371000150428             ww = %lookup( *zero : sk_POg2 );
371100150428             if  ww > *zero;
371200150428               sk_POg2(ww) = §UGRpofo;
371300150428             endif;
371400150428           endif;
371500150428         EndIf;
371600150317
371700140926      /end-free
371800140926
371900040617      *
372000040225      * Se ricevuto parametro per manutenzione  non valorizzo il p.o. emittente con quello
372100040225      * in linea
372200040225     c                   if        parms   > 1
372300040225      * modalità RICHIAMATO
372400040225     c                   eval      wmod   = 'R'
372500040304     c                   eval      *in05 = (ia2mod = 'I')
372600040405     c                   eval      *in15 = (ia2mod <> 'I')
372700040225     c                   else
372800040225      * modalità DA MENU
372900040225     c                   eval      wmod   = ' '
373000040331      *
373100040331     c                   eval      v01ape = 'SI'
373200160202     c                   if        SIMfel <> *zero
373300160129     c                   eval      V01att = 'S'
373400160202     c                   endif
373500150313     c                   eval      V01pog = %editc( DUTpou : 'X' )
373600150617     c                   if        DUTlpo <> 'S'
373700150422     c                   eval      V01rpr = 'RAC'
373800150617     c                   endif
373900150313     c                   eval      v01rpg = knmus
374000040421     c                   move      w0140         v01ant
374100151022      * se utente di secondo livello non imposto reparto e utente
374200151022     c                   IF        DUTlpo = '2'
374300151022     c                   clear                   V01rpr
374400151022     c                   clear                   V01rpg
374500151022     c                   ENDIF
374600040331      *
374700040225     c                   endif
374800040225      *
374900040225     c                   EndSr
375000040225
375100040225      *---------------------------------------------------------------*
375200040225      * Reperimento Dati del job (Utente/Operativi)                   *
375300040225      *---------------------------------------------------------------*
375400040225     c     DatiJob       BegSr
375500040225      *
375600040225     c     *dtaara       define    §azute        azuteds
375700040225     c     *dtaara       define    §datiute      ddatiute
375800040225      *
375900040225     c                   in(E)     *dtaara
376000040225     c                   IF        %ERROR or RSUT = *blanks
376100040225     c                   clear                   Tibs34Ds
376200040225     c                   call      'TIBS34R'
376300040225     c                   parm                    Tibs34Ds
376400040225     c                   in        *dtaara
376500040225     c                   ENDIF
376600040225      *
376700040225     c                   EndSr
