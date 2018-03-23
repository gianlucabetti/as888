000100080206      //--------------------------------------------------------------
000200100713      //?TRMK89R - Stampa Attività commerciale
000300080206      //--------------------------------------------------------------
000400080206
000500100505     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600090807     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100100308     fTABEL00F  if   e           k disk
001200130917     fAZCMM01L  if   e           k disk
001300100308     fTFNTC01L  if   e           k disk
001400100308     fTIATC01L  if   e           k disk    rename(tiatc000:w_tiatc)
001500100308     f                                     prefix(w_)
001600100309     fTIATL01L  if   e           k disk
001700100309     fTIATL02L  if   e           k disk    rename(tiatl000:tiatl02)
001800100308     fTICPI01L  if   e           k disk
001900100520     fTICPN05L  if   e           k disk
002000100505     fTICPN04L  if   e           k disk    rename(ticpn000:ticpn04)
002100110803     fTIVII01L  if   e           k disk
002200110803     fTIVIS05L  if   e           k disk
002300110803     fTIVOF01L  if   e           k disk
002400100308     fTNCPO01L  if   e           k disk
002500100528     fTNTBE01L  if   e           k disk
002600100521     fTRMK89P   o    e             printer oflind(*in99)
002700100310     f                                     infds(prtinfo)
002800100310
002900080206      //---------------------------------------------------------------
003000090406      //?Definizione costanti.
003100080206      //---------------------------------------------------------------
003200080206
003300080206      //---------------------------------------------------------------
003400080206      //?Definizione schiere.
003500080206      //---------------------------------------------------------------
003600090806
003700100304      // - Agenti
003800100429     d $Age            s              7  0 dim(3500)
003900100429
004000100429      // - Agenti Unificanti
004100100429     d $AgeU           s              7  0 dim(3500)
004200100513
004300100513     d §MK86TATds      ds            10    inz
004400100513     d   $TAT                  1     10    inz  dim(10)
004500080206
004600110422      // causali da stampare
004700110422     d $STP            s              5    dim(24) ctdata perrcd(1)
004800110422
004900110422      // causali da sommare in stampa
005000110422     d $cau            s              4    dim(24) ctdata perrcd(1)
005100130917
005200080206      //---------------------------------------------------------------
005300080206      //?Definizione aree dati.
005400080206      //---------------------------------------------------------------
005500080206
005600080206      // - Dati utente
005700080206     d §AzUte        e ds                  extname(AZUTE00F)
005800080206     d                                     dtaara
005900080206     d §DatiUte      e ds                  extname(dDatiUte)
006000080206     d                                     dtaara
006100080206
006200080206      //---------------------------------------------------------------
006300080206      //?Definizione strutture dati.
006400080206      //---------------------------------------------------------------
006500100310
006600100310     d PrtINFO         ds
006700100310     d  CurLine              367    368i 0
006800080206
006900080206      // - Status
007000080206     d Psds           sds
007100080206     d   SDSpgm          *proc
007200080206
007300080206      // - Parametri ricevuti
007400080206     d KPJBA         e ds
007500100521     d TRMK88DS      e ds
007600100521     d TRMK86DS      e ds
007700080206
007800100308      // - Ricerca/Controllo tabelle
007900100308     d TIBS02ds      e ds                  inz
008000100308
008100080206      // - Reperimento dati utente
008200100305     d TIBS34DS      e ds
008300100308
008400100308       // - Reperimento dati anagrafici
008500100308     d TIBS69ds      e ds
008600100308     d DS_cnaco      e ds                  inz extname(CNACO00F)
008700100308     d DS_cnind      e ds                  inz extname(CNIND00F)
008800100308     d DS_cnclp      e ds                  inz extname(CNCLP00F)
008900100308     d DS_fncls      e ds                  inz extname(FNCLS00F)
009000100429
009100100429      // - controllo abilitazioni
009200100429     d TNTAA1DS      e ds                  inz
009300100429
009400100429      // - Carico Filiali abilitate all'utente
009500100429     d TRUL31DS      e ds
009600100429     d POG                    10    759    DIM(250)
009700110120      // - Carico Aree abilitate all'utente
009800110120     d TRUL31DS2     e ds
009900110120     d CAR                    22    171    DIM(50)
010000100308
010100100308      // - Tabella CCO = Causale contatto
010200100308     d dCCO          e ds                  inz
010300100712
010400100712      // - Tabella IFS = Sottotipi INFO commerciali
010500100712     d dIFS          e ds                  inz
010600100308
010700100308      // - Tabella TTA = Tipo attività
010800100308     d dTTA          e ds                  inz
010900100308
011000110120      // - Tabella 05 = Codici Area
011100110120     d ds05          e ds                  inz
011200110120
011300100308      // - Tabella 1L = Categoria Merceologica
011400100308     d ds1L          e ds                  inz
011500090807
011600090807      // File attività
011700100305     d TIATC00F      e ds                  extname(TIATC00F)
011800100305
011900130917      *// File trattativa
012000130917     d*// TIVIS00F      e ds                  extname(TIVIS00F)
012100090629
012200100521      // File statistica consuntiva
012300100521     d WFRCA00F      e ds                  extname(WFRCA00F)
012400100521
012500100521      // File statistica preventiva
012600100521     d WFRPA00F      e ds                  extname(WFRPA00F)
012700100522
012800110422
012900110422     d Causalids       ds
013000110422     d  Causa01                       5
013100110422     d  Causa02                       5
013200110422     d  Causa03                       5
013300110422     d  Causa04                       5
013400110422     d  Causa05                       5
013500110422     d  Causa06                       5
013600110422     d  Causa07                       5
013700110422     d  Causa08                       5
013800110422     d  Causa09                       5
013900110422     d  Causa10                       5
014000110422     d  Causa11                       5
014100110422     d  Causa12                       5
014200110422     d  Causa13                       5
014300110422     d  Causa14                       5
014400110422     d  Causa15                       5
014500110422     d  Causa16                       5
014600110422     d  Causa17                       5
014700110422     d  Causa18                       5
014800110422     d  Causa19                       5
014900110422     d  Causa20                       5
015000110422     d  Causa21                       5
015100110422     d  Causa22                       5
015200110422     d  Causa23                       5
015300110422     d  Causa24                       5
015400110426
015500110426     d Cauwds          ds           120    inz
015600110426     d   $CAAstp                      5    inz  dim(24)
015700110422     d
015800110422     d Cauineds        ds
015900110422     d  Cauine01                      5
016000110422     d  Cauine02                      5
016100110422     d  Cauine03                      5
016200110422     d  Cauine04                      5
016300110422     d  Cauine05                      5
016400110422     d  Cauine06                      5
016500110422     d  Cauine07                      5
016600110422     d  Cauine08                      5
016700110422     d  Cauine09                      5
016800110422     d  Cauine10                      5
016900110422     d  Cauine11                      5
017000110422     d  Cauine12                      5
017100110422     d  Cauine13                      5
017200110422     d  Cauine14                      5
017300110422     d  Cauine15                      5
017400110422     d  Cauine16                      5
017500110422     d  Cauine17                      5
017600110422     d  Cauine18                      5
017700110422     d  Cauine19                      5
017800110422     d  Cauine20                      5
017900110422     d  Cauine21                      5
018000110422     d  Cauine22                      5
018100110422     d  Cauine23                      5
018200110422     d  Cauine24                      5
018300110422     d
018400110422     d ContaCau        ds
018500110422     d  uno                           2
018600110422     d  due                           2
018700110422     d
018800110422     d RCACaapmds      ds            70    inz
018900110422     d   $CAAPM                       2    inz  dim(35)
019000110422
019100110422     d RCANrapmds      ds                  inz
019200110422     d   $NRAPM                       5s 0 inz  dim(35)
019300110422
019400110422     d Qtapmds         ds
019500110422     d  Qtapm01                       5  0 inz
019600110422     d  Qtapm02                       5  0 inz
019700110422     d  Qtapm03                       5  0 inz
019800110422     d  Qtapm04                       5  0 inz
019900110422     d  Qtapm05                       5  0 inz
020000110422     d  Qtapm06                       5  0 inz
020100110422     d  Qtapm07                       5  0 inz
020200110422     d  Qtapm08                       5  0 inz
020300110422     d  Qtapm09                       5  0 inz
020400110422     d  Qtapm10                       5  0 inz
020500110422     d  Qtapm11                       5  0 inz
020600110422     d  Qtapm12                       5  0 inz
020700110422     d  Qtapm13                       5  0 inz
020800110422     d  Qtapm14                       5  0 inz
020900110422     d  Qtapm15                       5  0 inz
021000110422     d  Qtapm16                       5  0 inz
021100110422     d  Qtapm17                       5  0 inz
021200110422     d  Qtapm18                       5  0 inz
021300110422     d  Qtapm19                       5  0 inz
021400110422     d  Qtapm20                       5  0 inz
021500110422     d  Qtapm21                       5  0 inz
021600110422     d  Qtapm22                       5  0 inz
021700110422     d  Qtapm23                       5  0 inz
021800110422     d  Qtapm24                       5  0 inz
021900110422
022000110422     d Qtapmwds        ds
022100110422     d   $QtaPM                       5s 0 inz  dim(24)
022200110422     d
022300110422     d RCACaapgds      ds            70    inz
022400110422     d   $CAAPG                       2    inz  dim(35)
022500110422
022600110422     d RCANrapgds      ds                  inz
022700110422     d   $NRAPG                       5s 0 inz  dim(35)
022800110422
022900110422     d Qtapgds         ds
023000110422     d  Qtapg01                       5  0 inz
023100110422     d  Qtapg02                       5  0 inz
023200110422     d  Qtapg03                       5  0 inz
023300110422     d  Qtapg04                       5  0 inz
023400110422     d  Qtapg05                       5  0 inz
023500110422     d  Qtapg06                       5  0 inz
023600110422     d  Qtapg07                       5  0 inz
023700110422     d  Qtapg08                       5  0 inz
023800110422     d  Qtapg09                       5  0 inz
023900110422     d  Qtapg10                       5  0 inz
024000110422     d  Qtapg11                       5  0 inz
024100110422     d  Qtapg12                       5  0 inz
024200110422     d  Qtapg13                       5  0 inz
024300110422     d  Qtapg14                       5  0 inz
024400110422     d  Qtapg15                       5  0 inz
024500110422     d  Qtapg16                       5  0 inz
024600110422     d  Qtapg17                       5  0 inz
024700110422     d  Qtapg18                       5  0 inz
024800110422     d  Qtapg19                       5  0 inz
024900110422     d  Qtapg20                       5  0 inz
025000110422     d  Qtapg21                       5  0 inz
025100110422     d  Qtapg22                       5  0 inz
025200110422     d  Qtapg23                       5  0 inz
025300110422     d  Qtapg24                       5  0 inz
025400110422
025500110422     d Qtapgwds        ds
025600110422     d   $QtaPG                       5s 0 inz  dim(24)
025700110422     d
025800110422     d RCACaappds      ds            70    inz
025900110422     d   $CAAPP                       2    inz  dim(35)
026000110422
026100110422     d RCANrappds      ds                  inz
026200110422     d   $NRAPP                       5s 0 inz  dim(35)
026300110422
026400110422     d Qtappds         ds
026500110422     d  Qtapp01                       5  0 inz
026600110422     d  Qtapp02                       5  0 inz
026700110422     d  Qtapp03                       5  0 inz
026800110422     d  Qtapp04                       5  0 inz
026900110422     d  Qtapp05                       5  0 inz
027000110422     d  Qtapp06                       5  0 inz
027100110422     d  Qtapp07                       5  0 inz
027200110422     d  Qtapp08                       5  0 inz
027300110422     d  Qtapp09                       5  0 inz
027400110422     d  Qtapp10                       5  0 inz
027500110422     d  Qtapp11                       5  0 inz
027600110422     d  Qtapp12                       5  0 inz
027700110422     d  Qtapp13                       5  0 inz
027800110422     d  Qtapp14                       5  0 inz
027900110422     d  Qtapp15                       5  0 inz
028000110422     d  Qtapp16                       5  0 inz
028100110422     d  Qtapp17                       5  0 inz
028200110422     d  Qtapp18                       5  0 inz
028300110422     d  Qtapp19                       5  0 inz
028400110422     d  Qtapp20                       5  0 inz
028500110422     d  Qtapp21                       5  0 inz
028600110422     d  Qtapp22                       5  0 inz
028700110422     d  Qtapp23                       5  0 inz
028800110422     d  Qtapp24                       5  0 inz
028900110422
029000110422     d Qtappwds        ds
029100110422     d   $QtaPP                       5s 0 inz  dim(24)
029200110422     d
029300110422     d RCACaacfds      ds            70    inz
029400110422     d   $CAACF                       2    inz  dim(35)
029500110422
029600110422     d RCANracfds      ds                  inz
029700110422     d   $NRACF                       5s 0 inz  dim(35)
029800110422
029900110422     d Qtacfds         ds
030000110422     d  Qtacf01                       5  0 inz
030100110422     d  Qtacf02                       5  0 inz
030200110422     d  Qtacf03                       5  0 inz
030300110422     d  Qtacf04                       5  0 inz
030400110422     d  Qtacf05                       5  0 inz
030500110422     d  Qtacf06                       5  0 inz
030600110422     d  Qtacf07                       5  0 inz
030700110422     d  Qtacf08                       5  0 inz
030800110422     d  Qtacf09                       5  0 inz
030900110422     d  Qtacf10                       5  0 inz
031000110422     d  Qtacf11                       5  0 inz
031100110422     d  Qtacf12                       5  0 inz
031200110422     d  Qtacf13                       5  0 inz
031300110422     d  Qtacf14                       5  0 inz
031400110422     d  Qtacf15                       5  0 inz
031500110422     d  Qtacf16                       5  0 inz
031600110422     d  Qtacf17                       5  0 inz
031700110422     d  Qtacf18                       5  0 inz
031800110422     d  Qtacf19                       5  0 inz
031900110422     d  Qtacf20                       5  0 inz
032000110422     d  Qtacf21                       5  0 inz
032100110422     d  Qtacf22                       5  0 inz
032200110422     d  Qtacf23                       5  0 inz
032300110422     d  Qtacf24                       5  0 inz
032400110422
032500110422     d Qtacfwds        ds
032600110422     d   $QtaCF                       5s 0 inz  dim(24)
032700110422     d
032800110422     d RCACaaptds      ds            70    inz
032900110422     d   $CAAPT                       2    inz  dim(35)
033000110422
033100110422     d RCANraptds      ds                  inz
033200110422     d   $NRAPT                       5s 0 inz  dim(35)
033300110422
033400110422     d Qtaptds         ds
033500110422     d  Qtapt01                       5  0 inz
033600110422     d  Qtapt02                       5  0 inz
033700110422     d  Qtapt03                       5  0 inz
033800110422     d  Qtapt04                       5  0 inz
033900110422     d  Qtapt05                       5  0 inz
034000110422     d  Qtapt06                       5  0 inz
034100110422     d  Qtapt07                       5  0 inz
034200110422     d  Qtapt08                       5  0 inz
034300110422     d  Qtapt09                       5  0 inz
034400110422     d  Qtapt10                       5  0 inz
034500110422     d  Qtapt11                       5  0 inz
034600110422     d  Qtapt12                       5  0 inz
034700110422     d  Qtapt13                       5  0 inz
034800110422     d  Qtapt14                       5  0 inz
034900110422     d  Qtapt15                       5  0 inz
035000110422     d  Qtapt16                       5  0 inz
035100110422     d  Qtapt17                       5  0 inz
035200110422     d  Qtapt18                       5  0 inz
035300110422     d  Qtapt19                       5  0 inz
035400110422     d  Qtapt20                       5  0 inz
035500110422     d  Qtapt21                       5  0 inz
035600110422     d  Qtapt22                       5  0 inz
035700110422     d  Qtapt23                       5  0 inz
035800110422     d  Qtapt24                       5  0 inz
035900110422
036000110422     d Qtaptwds        ds
036100110422     d   $QtaPT                       5s 0 inz  dim(24)
036200110422     d
036300110422     d RCACaactds      ds            70    inz
036400110422     d   $CAACT                       2    inz  dim(35)
036500110422
036600110422     d RCANractds      ds                  inz
036700110422     d   $NRACT                       5s 0 inz  dim(35)
036800110422
036900110422     d Qtactds         ds
037000110422     d  Qtact01                       5  0 inz
037100110422     d  Qtact02                       5  0 inz
037200110422     d  Qtact03                       5  0 inz
037300110422     d  Qtact04                       5  0 inz
037400110422     d  Qtact05                       5  0 inz
037500110422     d  Qtact06                       5  0 inz
037600110422     d  Qtact07                       5  0 inz
037700110422     d  Qtact08                       5  0 inz
037800110422     d  Qtact09                       5  0 inz
037900110422     d  Qtact10                       5  0 inz
038000110422     d  Qtact11                       5  0 inz
038100110422     d  Qtact12                       5  0 inz
038200110422     d  Qtact13                       5  0 inz
038300110422     d  Qtact14                       5  0 inz
038400110422     d  Qtact15                       5  0 inz
038500110422     d  Qtact16                       5  0 inz
038600110422     d  Qtact17                       5  0 inz
038700110422     d  Qtact18                       5  0 inz
038800110422     d  Qtact19                       5  0 inz
038900110422     d  Qtact20                       5  0 inz
039000110422     d  Qtact21                       5  0 inz
039100110422     d  Qtact22                       5  0 inz
039200110422     d  Qtact23                       5  0 inz
039300110422     d  Qtact24                       5  0 inz
039400110422
039500110422     d Qtactwds        ds
039600110422     d   $QtaCT                       5s 0 inz  dim(24)
039700110422     d
039800110422     d RCACaapptds     ds            70    inz
039900110422     d   $CAAPPT                      2    inz  dim(35)
040000110422
040100110422     d RCANrapptds     ds                  inz
040200110422     d   $NRAPPT                      5s 0 inz  dim(35)
040300110422
040400110422     d Qtapptds        ds
040500110422     d  Qtappt01                      5  0 inz
040600110422     d  Qtappt02                      5  0 inz
040700110422     d  Qtappt03                      5  0 inz
040800110422     d  Qtappt04                      5  0 inz
040900110422     d  Qtappt05                      5  0 inz
041000110422     d  Qtappt06                      5  0 inz
041100110422     d  Qtappt07                      5  0 inz
041200110422     d  Qtappt08                      5  0 inz
041300110422     d  Qtappt09                      5  0 inz
041400110422     d  Qtappt10                      5  0 inz
041500110422     d  Qtappt11                      5  0 inz
041600110422     d  Qtappt12                      5  0 inz
041700110422     d  Qtappt13                      5  0 inz
041800110422     d  Qtappt14                      5  0 inz
041900110422     d  Qtappt15                      5  0 inz
042000110422     d  Qtappt16                      5  0 inz
042100110422     d  Qtappt17                      5  0 inz
042200110422     d  Qtappt18                      5  0 inz
042300110422     d  Qtappt19                      5  0 inz
042400110422     d  Qtappt20                      5  0 inz
042500110422     d  Qtappt21                      5  0 inz
042600110422     d  Qtappt22                      5  0 inz
042700110422     d  Qtappt23                      5  0 inz
042800110422     d  Qtappt24                      5  0 inz
042900110422
043000110422     d Qtapptwds       ds
043100110422     d   $QtaPPT                      5s 0 inz  dim(24)
043200110422     d
043300110422     d RCACaaids       ds            70    inz
043400110422     d   $CAAI                        2    inz  dim(35)
043500110422
043600110422     d RCANraids       ds                  inz
043700110422     d   $NRAI                        5s 0 inz  dim(35)
043800110422
043900110422     d Qtainds         ds
044000110422     d  Qtain01                       5  0 inz
044100110422     d  Qtain02                       5  0 inz
044200110422     d  Qtain03                       5  0 inz
044300110422     d  Qtain04                       5  0 inz
044400110422     d  Qtain05                       5  0 inz
044500110422     d  Qtain06                       5  0 inz
044600110422     d  Qtain07                       5  0 inz
044700110422     d  Qtain08                       5  0 inz
044800110422     d  Qtain09                       5  0 inz
044900110422     d  Qtain10                       5  0 inz
045000110422     d  Qtain11                       5  0 inz
045100110422     d  Qtain12                       5  0 inz
045200110422     d  Qtain13                       5  0 inz
045300110422     d  Qtain14                       5  0 inz
045400110422     d  Qtain15                       5  0 inz
045500110422     d  Qtain16                       5  0 inz
045600110422     d  Qtain17                       5  0 inz
045700110422     d  Qtain18                       5  0 inz
045800110422     d  Qtain19                       5  0 inz
045900110422     d  Qtain20                       5  0 inz
046000110422     d  Qtain21                       5  0 inz
046100110422     d  Qtain22                       5  0 inz
046200110422     d  Qtain23                       5  0 inz
046300110422     d  Qtain24                       5  0 inz
046400110422
046500110422     d Qtainwds        ds
046600110422     d   $QtaIN                       5s 0 inz  dim(24)
046700110422     d
046800110426
046900110426      // schiere di lavoro per i totali generali
047000110426     d WRKCaads        ds
047100110426     d   $WrkCaa                      2    inz  dim(35)
047200110426
047300110426     d WRKNrads        ds
047400110426     d   $WrkNra                      5s 0 inz  dim(35)
047500080206      //---------------------------------------------------------------
047600080206      //?Definizione variabili globali.
047700080206      //---------------------------------------------------------------
047800080206
047900100525      // - Fl
048000100305     d $End            s               n   inz(*off)
048100100305     d $EndAge         s               n   inz(*off)
048200100305     d $RcdOk          s               n   inz(*off)
048300080206
048400080206      // - Indici di schiera
048500100429     d xx              s              4  0 inz
048600100429     d yy              s              4  0 inz
048700100429     d zz              s              4  0 inz
048800110422     d xpm             s              4  0 inz
048900110422     d xpg             s              4  0 inz
049000110422     d xpp             s              4  0 inz
049100110422     d xcf             s              4  0 inz
049200110422     d xpt             s              4  0 inz
049300110422     d xct             s              4  0 inz
049400110422     d xppt            s              4  0 inz
049500110422     d xin             s              4  0 inz
049600080206
049700090806       // - Stringa SQL da eseguire
049800090806     d wSQL            s           2048    Varying        inz
049900080206
050000090401      // - Campi di comodo data
050100100305     d wDate_EUR       s               d   datfmt(*eur)
050200100305     d wDate_ISO       s               d   datfmt(*iso)
050300100305
050400100305      // - Campi di comodo
050500100308     d sav_ATCdad      s                   like(ATCdad)
050600100309     d wAff            s              1  0 inz
050700100308     d wCatMer         s              5  0 inz
050800100309     d wCmm            s                   like(ATCcmm)
050900100923     d wConc           s              2  0 inz
051000100308     d wNota           s              1  0 inz
051100100305     d wTrat           s            196    inz(*all'-')
051200100521     d TipoStampa      s              1
051300100522     d mkcmm           s                   like(ATCcmm)
051400100525     d WTot_M_FP       s              7  0 inz
051500100525     d WTot_M_RA       s              7  0 inz
051600100525     d WTot_M_RC       s              7  0 inz
051700100525     d WTot_M_UF       s              7  0 inz
051800100525     d wTot_H          s              5  0 inz
051900100525     d wTot_R          s              5  0 inz
052000100526     d W_fra           s              6  0 inz
052100100526     d W_frc           s              6  0 inz
052200100526     d W_fuf           s              6  0 inz
052300100526     d W_feriehh       s              6  0 inz
052400100525
052500100525       // - Campi di totali
052600100525     d t_tepm          s                   like(rcatepm)
052700110426     d t_tepg          s                   like(rcatepm)
052800110426     d t_tepp          s                   like(rcatepm)
052900110426     d t_tecft         s                   like(rcatepm)
053000110426     d t_tept          s                   like(rcatepm)
053100110426     d t_tect          s                   like(rcatepm)
053200110426     d t_teppt         s                   like(rcatepm)
053300110426     d t_tepost        s                   like(rcatepm)
053400110426     d t_tepoa         s                   like(rcatepm)
053500110426     d t_tecaldi       s                   like(rcatepm)
053600110426     d t_tecalda       s                   like(rcatepm)
053700110426     d t_teposve       s                   like(rcatepm)
053800110426     d t_tepv32a       s                   like(rcatepm)
053900110426     d t_temante       s                   like(rcatepm)
054000110426     d t_tepc40a       s                   like(rcatepm)
054100110426     d t_teincre       s                   like(rcatepm)
054200110426     d t_tepc50a       s                   like(rcatepm)
054300110426     d t_telese        s                   like(rcatepm)
054400110426     d t_inevase       s                   like(rcatepm)
054500100609     d t_patn          s                   like(rcapatn)
054600100525     d t_patm          s                   like(rcapatm)
054700100525     d t_pata          s                   like(rcapata)
054800100525     d t_patc          s                   like(rcapatc)
054900110119     d t_pati          s                   like(rcapati)
055000110119     d t_patr          s                   like(rcapatr)
055100100622     d t_apptn         s                   like(rcaapptn)
055200100622     d t_apptm         s                   like(rcaapptm)
055300100622     d t_appta         s                   like(rcaappta)
055400100622     d t_apptc         s                   like(rcaapptc)
055500110119     d t_appti         s                   like(rcaappti)
055600110119     d t_apptr         s                   like(rcaapptr)
055700100525     d t_aptn          s                   like(rcaaptn)
055800100525     d t_aptm          s                   like(rcaaptm)
055900100525     d t_apta          s                   like(rcaapta)
056000100525     d t_aptc          s                   like(rcaaptc)
056100110119     d t_apti          s                   like(rcaapti)
056200110119     d t_aptr          s                   like(rcaaptr)
056300100525     d t_aftn          s                   like(rcaaftn)
056400100525     d t_aftm          s                   like(rcaaftm)
056500100525     d t_afta          s                   like(rcaafta)
056600100525     d t_aftc          s                   like(rcaaftc)
056700110119     d t_afti          s                   like(rcaafti)
056800110119     d t_aftr          s                   like(rcaaftr)
056900100525     d t_ap24          s                   like(rcaap24)
057000100525     d t_feriegg       s                   like(rcaferiegg)
057100100629     d T_totapppr      s                   like(totapppr)
057200100629     d T_totapppr1     s                   like(totapppr1)
057300100526     d T_totapp        s                   like(totapp)
057400100526     d T_tottel        s                   like(tottel)
057500100526     d T_appm          s                   like(rpaappm)
057600100526     d T_appn          s                   like(rpaappn)
057700100526     d T_appa          s                   like(rpaappa)
057800100526     d T_appc          s                   like(rpaappc)
057900110119     d T_appi          s                   like(rpaappi)
058000110119     d T_appr          s                   like(rpaappr)
058100100526     d T_affm          s                   like(rpaaffm)
058200100526     d T_affn          s                   like(rpaaffn)
058300100526     d T_affa          s                   like(rpaaffa)
058400100526     d T_affc          s                   like(rpaaffc)
058500110119     d T_affi          s                   like(rpaaffi)
058600110119     d T_affr          s                   like(rpaaffr)
058700100526     d T_atc19         s                   like(rpatc19)
058800100526     d T_atc20         s                   like(rpatc20)
058900100526     d T_atc21         s                   like(rpatc21)
059000100526     d T_atc22         s                   like(rpatc22)
059100100526     d T_atc24         s                   like(rpatc24)
059200100526     d T_atc25         s                   like(rpatc25)
059300100526     d T_atc26         s                   like(rpatc26)
059400100526     d T_atc27         s                   like(rpatc27)
059500100526     d T_atc28         s                   like(rpatc28)
059600100526     d T_atc30         s                   like(rpatc30)
059700100526     d T_atc31         s                   like(rpatc31)
059800100526     d T_atc32         s                   like(rpatc32)
059900100526     d T_atc33         s                   like(rpatc33)
060000110117     d T_atc44         s                   like(rpatc44)
060100100526     d T_atc60         s                   like(rpatc60)
060200110426     d T_atc70         s                   like(rpatc70)
060300110426     d T_atc71         s                   like(rpatc71)
060400110426     d T_atc73         s                   like(rpatc73)
060500110426     d T_atc74         s                   like(rpatc74)
060600110426     d T_atc75         s                   like(rpatc75)
060700100526     d T_off           s                   like(rpaoff)
060800100526     d T_fegg          s                   like(rpafegg)
060900100429
061000090508      //---------------------------------------------------------------
061100090807      //?Definizione procedure esterne.
061200090508      //---------------------------------------------------------------
061300100120
061400100120      //---------------------------------------------------------------
061500100120      //?prototipi
061600100120      //---------------------------------------------------------------
061700090807
061800080626      /copy gaitrasrc/srcprotopr,tibs02r
061900080626      /copy gaitrasrc/srcprotopr,tibs34r
062000100308      /copy gaitrasrc/srcprotopr,tibs69r
062100100429      /copy gaitrasrc/srcprotopr,tntaa1c
062200110120
062300110120      // - Filiali e aree abilitate
062400110120     d trul31r         pr                  extpgm('TRUL31R')
062500110120     d  kpjba                              likeds(KPJBA)
062600110120     d  trul31ds                           likeds(trul31ds)
062700110120     d  trul31ds2                          likeds(trul31ds2)
062800090807
062900080206      //---------------------------------------------------------------
063000080206      //?Definizione key-list.
063100080206      //---------------------------------------------------------------
063200090807
063300090807       // - File TABEL00F
063400090807     d k03tabel      e ds                  extname(TABEL00F:*key)
063500090807     d                                     prefix(k_)
063600090807     d                                     inz
063700080206
063800080206      //---------------------------------------------------------------
063900080206      //?Riepilogo indicatori.
064000080206      //---------------------------------------------------------------
064100090807
064200110120      // 09    : Condizione stampa selezione per Area
064300100504      // 10    : HI tipo attività se da eseguire
064400100514      // 11    : Se acceso non stampa in testata le attività escluse
064500100528      // 12    : Se acceso non stampa in testata il commerciale
064600100528      // 13    : Se acceso stampa nella consuntiva selezione dei commerciali
064700100528      // 14    : Se acceso non stampa in testata le date
064800110120      // 20    : Di cui altri commerciali
064900110120      // 30    : Stampa affiancamenti nella preventiva
065000110120      // 31    : Stampa affiancamenti nella preventiva
065100110120      // 32    : Stampa affiancamenti nella preventiva
065200110120      // 33    : Stampa affiancamenti nella preventiva
065300110120      // 34    : Stampa affiancamenti nella preventiva
065400100308      // 99    : Salto pagina
065500090807
065600080206      //---------------------------------------------------------------
065700080206
065800080206      //---------------------------------------------------------------
065900080206      //?M A I N - L I N E
066000080206      //---------------------------------------------------------------
066100080206
066200080206     c     *Entry        plist
066300080206     c                   parm                    KPJBA
066400080206
066500080206      /free
066600080206
066700090807       //?Operazioni iniziali
066800080206       exsr RoutInz;
066900100305
067000100521       // Ciclo principale
067100100521       //?Ciclo per sk unificanti e stampo
067200100521       zz = 1;
067300100521       FOR zz by 1 to %elem($AgeU);
067400100521       IF  $AgeU(zz) <> *zeros;
067500100521
067600100521       // verifico che cosa devo fare
067700100521        SELECT ;
067800100521       // se sono stata richiamata per la sola stampa agenda
067900100521         When i88tcpe =  ' ' ;
068000100521             tipostampa = 'A' ;
068100100522             exsr sr_intesta ;
068200100521             Exsr Agenda ;
068300100305
068400100521       // se sono stata richiamata per le statistiche
068500100521       // CONSUNTIVE
068600100522         When i88tcpe = 'C'  ;
068700100521       //    Stampa agenda consuntiva
068800100521             If i88dett = 'S' ;
068900100521                clear trmk86ds ;
069000100521                imk86cmm = i88cmmu;
069100100521                Imk86datai = i88deci ;
069200100521                Imk86dataf = i88decf ;
069300100521                Imk86ioc   = i88dscmm;
069400100521                Imk86iec   = i88dscmmie;
069500100521                Imk86iot   = i88dstat;
069600100521                Imk86tat   = i88dstatc ;
069700100521                tipostampa = 'A' ;
069800100522                exsr sr_intesta ;
069900100521                Exsr Agenda ;
070000100521             Endif ;
070100100521       //    Stampa statistica commerciale
070200100521             If i88totc = 'S' ;
070300100521                tipostampa = 'C' ;
070400100521                Exsr Stat_consun ;
070500100521             Endif ;
070600100521
070700100521       // PREVENTIVE
070800100522         When i88tcpe =  'P'  ;
070900100521       //    Stampa agenda preventiva
071000100521             If i88dett = 'S' ;
071100100521                clear trmk86ds ;
071200100521                imk86cmm = i88cmmu;
071300100521                Imk86datai = i88depi ;
071400100521                Imk86dataf = i88depf ;
071500100521                Imk86ioc   = i88dscmm;
071600100521                Imk86iec   = i88dscmmie;
071700100521                Imk86iot   = i88dstat;
071800100521                Imk86tat   = i88dstatc ;
071900100521                tipostampa = 'A' ;
072000100522                exsr sr_intesta ;
072100100521                Exsr Agenda ;
072200100521             Endif ;
072300100521       //    Stampa statistica commerciale
072400100521             If i88totc = 'S' ;
072500100521                tipostampa = 'P' ;
072600100522                Exsr Stat_preven ;
072700100521             Endif ;
072800100522
072900100522       // se sono stata richiamata per le statistiche
073000100522       // CONSUNTIVE E PREVENTIVE
073100100522         When i88tcpe = 'E'  ;
073200100522       //    Stampa agenda consuntiva
073300100522             If i88dett = 'S' ;
073400100522                clear trmk86ds ;
073500100522                imk86cmm = i88cmmu;
073600100522                Imk86datai = i88deci ;
073700100522                Imk86dataf = i88decf ;
073800100522                Imk86ioc   = i88dscmm;
073900100522                Imk86iec   = i88dscmmie;
074000100522                Imk86iot   = i88dstat;
074100100522                Imk86tat   = i88dstatc ;
074200100522                tipostampa = 'A' ;
074300100522                exsr sr_intesta ;
074400100522                Exsr Agenda ;
074500100522             Endif ;
074600100522       //    Stampa statistica commerciale
074700100522             If i88totc = 'S' ;
074800100522                tipostampa = 'C' ;
074900100522                Exsr Stat_consun ;
075000100522             Endif ;
075100100522
075200100522       // PREVENTIVE
075300100522       //    Stampa agenda preventiva
075400100522             If i88dett = 'S' ;
075500100522                clear trmk86ds ;
075600100522                imk86cmm = i88cmmu;
075700100522                Imk86datai = i88depi ;
075800100522                Imk86dataf = i88depf ;
075900100522                Imk86ioc   = i88dscmm;
076000100522                Imk86iec   = i88dscmmie;
076100100522                Imk86iot   = i88dstat;
076200100522                Imk86tat   = i88dstatc ;
076300100522                tipostampa = 'A' ;
076400100522                exsr sr_intesta ;
076500100522                Exsr Agenda ;
076600100522             Endif ;
076700100522       //    Stampa statistica commerciale
076800100522             If i88totc = 'S' ;
076900100522                tipostampa = 'P' ;
077000100522                Exsr Stat_preven ;
077100100522             Endif ;
077200100521
077300100521          ENDSL ;
077400100521         ENDIF;
077500100521
077600100521       ENDFOR;
077700100525       // verifico se devo stampare i totali generali
077800100525       If i88totg = 'S' ;
077900100525          exsr SR_TotGen ;
078000100525       Endif ;
078100100525
078200100305       //?Stampo la fine
078300100702       // decofico i campi da stampare alla fine
078400110120         //?Stampo l'area richiesta
078500110120         IF  I88car > 0;
078600110120           clear ds05;
078700110120           k_tblcod = '05';
078800110120           k_tblkey = %editc(I88car:'X');
078900110120           chain  %kds(K03tabel) TABEL00F;
079000110120           IF  %found( TABEL00F );
079100110120             ds05 = TBLuni;
079200110120           ENDIF;
079300110120           pDesCar = §05des;
079400110120         ENDIF;
079500110120
079600110120         //?Stampo che richieste tutte le aree
079700110120         IF  I88car = 0;
079800110120           pDesCar = 'Tutte';
079900110120         ENDIF;
080000110120
080100110120         //?Stampo l'unificante richiesto
080200130917         clear  pDesCmmu;
080300100702         IF  I88cmmu  > 0;
080400130917           chain  (i88cmmu)  AZCMM000;
080500130917           IF  %found( AZCMM01L );
080600130917             pDesCmmu = CMMdes;
080700100702           ENDIF;
080800100702         else ;
080900100702           pDesCmmu = 'Tutti' ;
081000100702         ENDIF;
081100100702         //?Imposto data inizio e fine stampa
081200100707         If i88deci <> *zeros ;
081300100702            wDate_ISO = %date(I88deci:*iso);
081400100702            wDate_EUR = wDate_ISO;
081500100702            pDataic   = %dec(wDate_EUR);
081600100702            wDate_ISO = %date(I88decf:*iso);
081700100702            wDate_EUR = wDate_ISO;
081800100702            pDatafc   = %dec(wDate_EUR);
081900100707         else ;
082000100707            clear pdatafc ;
082100100707            clear pdataic ;
082200100707         endif ;
082300100707         If i88depi <> *zeros ;
082400100707            wDate_ISO = %date(I88depi:*iso);
082500100707            wDate_EUR = wDate_ISO;
082600100707            pDataip   = %dec(wDate_EUR);
082700100707            wDate_ISO = %date(I88depf:*iso);
082800100707            wDate_EUR = wDate_ISO;
082900100707            pDatafp   = %dec(wDate_EUR);
083000100707         else ;
083100100707            clear pdatafp ;
083200100707            clear pdataip ;
083300100707         endif ;
083400100702        // attività incluse / escluse
083500100702            i88tat1 = %subst(i88dstatc: 1: 1) ;
083600100702            i88tat2 = %subst(i88dstatc: 2: 1) ;
083700100702            i88tat3 = %subst(i88dstatc: 3: 1) ;
083800100702            i88tat4 = %subst(i88dstatc: 4: 1) ;
083900100702            i88tat5 = %subst(i88dstatc: 5: 1) ;
084000100702
084100100528       // se ho richiesto solo il dettaglio agenda stampo solo fine stampa
084200100528       If i88totc = 'N' and i88totg = 'N' ;
084300100713          write MK89FINE3;
084400100528       else ;
084500100528       // stampo la legenda delle causali
084600100528          tbecod = 'CCO' ;
084700100528          *in11 = *on ;
084800100528          *in12 = *on ;
084900100528          *in13 = *off ;
085000100528          *in14 = *on  ;
085100100528          tipostampa = 'F' ;
085200100528          exsr sr_StpSalto;
085300100528          write mk89fine1 ;
085400100528          setll tbecod tntbe01l ;
085500100528          reade tbecod tntbe01l ;
085600100621          dow not %eof(tntbe01l) ;
085700100528              dcco = tbeuni ;
085800100621              if §ccosta = 'S' and pcodcc1 = '  ' ;
085900100621                 pcodcc1 = tbeke1 ;
086000100621                 pdescc1 = §ccodes;
086100100622                 reade tbecod tntbe01l ;
086200100621                 iter ;
086300100528              endif ;
086400100621              if §ccosta = 'S' and pcodcc2 = '  ' ;
086500100621                 pcodcc2 = tbeke1 ;
086600100621                 pdescc2 = §ccodes;
086700100622                 reade tbecod tntbe01l ;
086800100621                 iter ;
086900100621              endif ;
087000100621              if §ccosta = 'S' and pcodcc3 = '  ' ;
087100100621                 pcodcc3 = tbeke1 ;
087200100621                 pdescc3 = §ccodes;
087300100621                 write mk89fine2 ;
087400100621                 clear pcodcc1 ;
087500100621                 clear pdescc1 ;
087600100621                 clear pcodcc2 ;
087700100621                 clear pdescc2 ;
087800100621                 clear pcodcc3 ;
087900100621                 clear pdescc3 ;
088000100621              endif ;
088100100622              reade tbecod tntbe01l ;
088200100528          enddo ;
088300100621          If pcodcc1 <> '  ' or pcodcc2 <> '  ' or pcodcc3 <> '  ' ;
088400100621                 write mk89fine2 ;
088500100621                 clear pcodcc1 ;
088600100621                 clear pdescc1 ;
088700100621                 clear pcodcc2 ;
088800100621                 clear pdescc2 ;
088900100621                 clear pcodcc3 ;
089000100621                 clear pdescc3 ;
089100100621          endif ;
089200100528          write MK89FINE;
089300100713          write MK89FINE3;
089400100528       endif ;
089500080206
089600090807       //?Operazioni finali
089700080206       exsr RoutEnd;
089800080206
089900080206       //--------------------------------------------------------------
090000080206       //?Operazioni iniziali.
090100080206       //--------------------------------------------------------------
090200080206       BEGSR RoutInz;
090300090807
090400100522         // verifico se sto lanciando la sola stampa agenda oppure anche la statistica
090500100522         // devo valorizzare la ds che mi serve al momento
090600100522         If %subst(kpjbu:1:1) <> '0' ;
090700100522            trmk88ds = kpjbu ;
090800100522            MKcmm   = i88cmmU ;
090900100522         else ;
091000100522            trmk86ds = kpjbu ;
091100100522            MKcmm   = imk86cmm;
091200100522         endif ;
091300100522
091400090807         //?Reperimento dati job
091500090807         exsr DatiJob;
091600090807
091700090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
091800090806
091900100305         //?Imposto nome programma
092000100305         NomePgm = SDSpgm;
092100100305
092200100305         //?Imposto data e ora elaborazione
092300100505         DataEla = %dec(%date():*eur);
092400100305         OraEla  = %dec(%time());
092500100514
092600100429         clear $AgeU;
092700100429
092800100429         //?Carico in sk l'unificante richiesto
092900100522         IF  MKcmm > 0;
093000100522           $AgeU(01) = MKcmm;
093100100429         ENDIF;
093200100429
093300100429         //?Carico gli unificanti delle filiali gestite dall'utente
093400100522         IF  MKcmm = 0;
093500100429           exsr sr_CarAgeUni;
093600100429           sorta $AgeU;
093700100429         ENDIF;
093800100305
093900090714
094000080206       ENDSR;
094100100521
094200100521       //--------------------------------------------------------------
094300100521       //?Elaboro AGENDA
094400100521       //--------------------------------------------------------------
094500100521       BEGSR Agenda     ;
094600100521
094700100525           wCmm = $AgeU(zz);
094800100525           exsr sr_DecCmm;
094900100525         // verifico se devo includere od escludere dei commerciali
095000100525           select ;
095100100525         // se omessi gli interni
095200130917            when imk86ioc = 'O'  and imk86iec = 'I' and
095300130917                 (CMMfun =  'COMIN' or  CMMfun =  'ASC' or  CMMfun =  'SA');
095400100525         // se omessi gli esterni
095500130917            when imk86ioc = 'O'  and imk86iec = 'E' and
095600130917                  CMMfun <> 'COMIN' and CMMfun <> 'ASC' and CMMfun <> 'SA';
095700100525         // se inclusi gli esterni
095800130917            when imk86ioc = 'I'  and imk86iec = 'E' and
095900130917                 (CMMfun =  'COMIN' or  CMMfun =  'ASC' or  CMMfun =  'SA');
096000100525         // se inclusi gli interni
096100130917            when imk86ioc = 'I'  and imk86iec = 'I' and
096200130917                  CMMfun <> 'COMIN' and CMMfun <> 'ASC' and CMMfun <> 'SA';
096300100525          other ;
096400100525
096500100521           clear sav_atcdad;
096600100521
096700100521         //?Carico sk agenti dell'unificante
096800100521           exsr sr_CarAge;
096900100521
097000100521        //?Stampo le attività
097100100521           exsr sr_StpAttivita;
097200100521
097300100525          endsl ;
097400100521       ENDSR ;
097500100521
097600100522       //--------------------------------------------------------------
097700100522       //?Imposto i campi della testata
097800100522       //--------------------------------------------------------------
097900100522       BEGSR sr_intesta;
098000100522
098100100527          clear ptatio ;
098200100525
098300100522         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
098400100522         If imk86tat =  *blanks  ;
098500100522            *in11 = *on ;
098600100522         endif ;
098700100522         // imposto la schiera dei tipi attività da stampare
098800100522         §mk86tatds = imk86tat ;
098900100522         // li decodifico per la stampa
099000100522         If imk86iot = 'O' ;
099100100522            PINCOMI = 'ESCLUSE' ;
099200100522         else ;
099300100522            PINCOMI = 'INCLUSE' ;
099400100522         endif ;
099500100522         // decodifico le singole attività incluse o escluse
099600100522         xx = 1;
099700100522         FOR xx by 1 to %elem($tat) ;
099800100522         If $tat(xx) <> ' ' ;
099900100522           clear TIBS02ds;
100000100522           clear dTTA;
100100100522           T02mod = 'C';
100200100522           T02cod = 'TTA';
100300100522           T02ke1 = $tat(xx) ;
100400100522           T02sif = KNSIF;
100500100522           TNTBE_RicercaControllo (kpjba : tibs02ds);
100600100522           IF  T02err = *blanks;
100700100522             dTTA = T02uni;
100800100522           ENDIF;
100900100522           if Ptatio = *blanks ;
101000100522              Ptatio  = §ttadesb ;
101100100522           else ;
101200100522              Ptatio  = %trim(Ptatio) + '/' + §ttadesb ;
101300100522           endif ;
101400100522         endif ;
101500100522         ENDFOR ;
101600100522
101700100522         //?Imposto data inizio e fine stampa
101800100522         wDate_ISO = %date(IMK86datai:*iso);
101900100522         wDate_EUR = wDate_ISO;
102000100522         pDatai    = %dec(wDate_EUR);
102100100522         wDate_ISO = %date(IMK86dataf:*iso);
102200100522         wDate_EUR = wDate_ISO;
102300100522         pDataf    = %dec(wDate_EUR);
102400100522
102500100522       ENDSR ;
102600100522
102700100308       //--------------------------------------------------------------
102800100308       //?Stampo il salto pagina.
102900100308       //--------------------------------------------------------------
103000100308       BEGSR sr_StpSalto;
103100100429
103200100429         //?Decodifico agente
103300100527         IF TipoStampa = 'A' or Tipostampa = 'C' or Tipostampa = 'P' ;
103400100526            wCmm = $AgeU(zz);
103500100526            pCodCmm = $AgeU(zz);
103600100526            exsr sr_DecCmm;
103700130917            pDesCmm = CMMdes;
103800100527         Endif;
103900100308
104000100521         // Imposto titolo della testata
104100100609         IF TipoStampa = 'A' OR Tipostampa = 'F' ;
104200100609            Titolo='             STATISTICA ATTIVITÀ COMMERCIALE              ';
104300100609         endif ;
104400100521
104500100521         IF TipoStampa = 'C' ;
104600100609            Titolo='        STATISTICA ATTIVITÀ COMMERCIALE CONSUNTIVA        ';
104700100527            Clear Ptatio ;
104800100521         endif ;
104900100521
105000100521         IF TipoStampa = 'P' ;
105100100609            Titolo='        STATISTICA ATTIVITÀ COMMERCIALE PREVENTIVA        ';
105200100527            Clear Ptatio ;
105300100521         endif ;
105400100609         IF TipoStampa = 'T' ;
105500100609            Titolo='STATISTICA ATTIVITÀ COMMERCIALE CONSUNTIVA COMM.LI INTERNI-
105600100610       /ESTERNI' ;
105700100527            Clear Ptatio ;
105800100525         endif ;
105900100609         IF TipoStampa = 'I' ;
106000100609            Titolo='STATISTICA ATTIVITÀ COMMERCIALE CONSUNTIVA COMM.LI INTERNI';
106100100609            Clear Ptatio ;
106200100609         endif ;
106300100609         IF TipoStampa = 'E' ;
106400100609            Titolo='STATISTICA ATTIVITÀ COMMERCIALE CONSUNTIVA COMM.LI ESTERNI';
106500100609            Clear Ptatio ;
106600100609         endif ;
106700100521
106800100526         IF TipoStampa = '2' ;
106900100609            Titolo='STATISTICA ATTIVITÀ COMMERCIALE PREVENTIVA COMM.LI INTERNI';
107000100527            Clear Ptatio ;
107100100526         endif ;
107200100527
107300100527         IF TipoStampa = '3' ;
107400100609            Titolo='STATISTICA ATTIVITÀ COMMERCIALE PREVENTIVA COMM.LI ESTERNI';
107500100527            Clear Ptatio ;
107600100527         endif ;
107700100528
107800100528         IF TipoStampa = 'F' ;
107900100528            Titolo = '    STATISTICA COMMERCIALE CRM     ';
108000100528            Clear Ptatio ;
108100100528         endif ;
108200100526
108300100308         //?Stampo la testata
108400100521         write MK89PAGINA;
108500100308         wrkriga = '*' + wTrat + '*';
108600100521         write MK89SEPARA;
108700100521         write MK89TESTA;
108800100521         write MK89SEPARA;
108900100309         //?Stampo le intestazioni delle colonne
109000100521         If Tipostampa = 'A' ;
109100100521            write MK89INTCOL;
109200100521         endif ;
109300100305
109400100308       ENDSR;
109500100429
109600100305       //--------------------------------------------------------------
109700100305       //?Stampo le attività.
109800100305       //--------------------------------------------------------------
109900100305       BEGSR sr_StpAttivita;
110000100305
110100100305         $End = *off;
110200100305
110300100305         //?Imposto la stringa per SQL
110400100305         exsr  sr_PrepSQL;
110500100305
110600100305         //?Dichiarazione cursore
110700100305         exec sql
110800100305           prepare S1   from :wSQL;
110900100305         exec sql
111000100305           declare ATT  cursor for S1;
111100100305
111200100305         //?Apertura del cursore
111300100305         exec sql
111400100305           open ATT;
111500100305
111600100305         IF sqlcode < 0;
111700100305           $End = *on;
111800100305         ENDIF;
111900100305
112000100305         DOW not $End;
112100100305           exec sql
112200100305             fetch next from ATT into :atctat, :atccpo, :atcksc,
112300100308                                      :atcatn, :atcatnp,
112400100309                                      :atcdad, :atchda, :atcnrv,
112500100331                                      :atcoii, :atcofi, :atccad,
112600100525                                      :atcco3, :atcdco,
112700100525                                      :atccac, :atcest;
112800100305           IF sqlcod = 100 or sqlcod < 0;
112900100305             $End = *on;
113000100305             leave;
113100100305           ENDIF;
113200100305
113300100305         //?Controllo se ok il rcd che sto leggendo per l'agenda
113400100305           exsr sr_CtrRcdOk;
113500100305           IF  not $RcdOk;
113600100305             iter;
113700100305           ENDIF;
113800100305
113900100309         //?Stampo la separazione per data attività
114000100305           IF atcdad <> sav_atcdad;
114100100308           //?Salto pagina
114200100505             IF  CurLine >= 60 or sav_atcdad = 0;
114300100308               exsr sr_StpSalto;
114400100308               *in99 = *off;
114500100308             ENDIF;
114600100308             clear wrkriga;
114700100521             write MK89SEPARA;
114800100308             wDate_ISO  = %date(atcdad:*iso);
114900100308             wDate_EUR  = wDate_ISO;
115000100308             pData      = %dec(wDate_EUR);
115100100521             write MK89GIORNO;
115200100305             sav_atcdad = atcdad;
115300100305           ENDIF;
115400100309
115500100506           if  *in99;
115600100506             exsr sr_StpSalto;
115700100506             *in99 = *off;
115800100506           endif;
115900100521           clear MK89RIGA;
116000100521           clear MK89RIGAUF;
116100100305
116200100309         //?Imposto il tipo attività
116300100309           clear TIBS02ds;
116400100309           clear dTTA;
116500100309           T02mod = 'C';
116600100309           T02cod = 'TTA';
116700100309           T02ke1 = ATCtat;
116800100309           T02sif = KNSIF;
116900100309           TNTBE_RicercaControllo (kpjba : tibs02ds);
117000100309           IF  T02err = *blanks;
117100100309             dTTA = T02uni;
117200100309           ENDIF;
117300100309           pDesTat  = §TTAdesb;
117400100309           pDesTatu = §TTAdesb;
117500100504         //?se attvità da eseguire imposto che il tipo attività è da stampare in HI
117600100504           IF  ATCdco > 0;
117700100504             *in10 = *off;
117800100504           ELSE;
117900100504             *in10 = *on;
118000100504           ENDIF;
118100100309
118200100309           pOra  = ATChda / 100;
118300100309
118400100309         //?Attività legata a cliente
118500100309           IF  §TTALEG = 'S';
118600100305         //?Stampo le attività a dettaglio
118700100309             exsr sr_StpDettaglio;
118800100712         //?Stampo i concorrenti
118900100712             exsr sr_StpConc;
119000100309         //?Stampo l'eventuale affiancamento
119100100309             exsr sr_StpAffianca;
119200100309           ENDIF;
119300100309
119400100309         //?Attività NON legata a cliente
119500100309           IF  §TTAleg <> 'S';
119600100309         //?Stampo le attività a dettaglio
119700100309             exsr sr_StpDetUFF;
119800100309           ENDIF;
119900100305
120000100305         ENDDO;
120100100305
120200100305         exec sql
120300100305           close ATT;
120400100305
120500100305       ENDSR;
120600100305
120700100305       //--------------------------------------------------------------
120800100305       //?Stampo le attività a dettaglio.
120900100305       //--------------------------------------------------------------
121000100305       BEGSR sr_StpDettaglio;
121100100308
121200100308         clear pRagSoc;
121300100504         clear pCodKsc;
121400100308         clear pTel;
121500100308         clear wCatMer;
121600100308         clear pClv;
121700100308         clear pVia;
121800100308         clear pLoc;
121900100308         clear pCap;
122000100331         clear pDesCo3;
122100100504         clear pCausale;
122200100308
122300100308         SELECT;
122400100308
122500100308         //?Dati cliente
122600100308           WHEN  ATCksc > 0;
122700100504             pCodKsc = ATCksc;
122800100308             clear tibs69ds;
122900100308             I69kac = ATCksc;
123000100308             I69kin = ATCksc;
123100100308             I69kcp = ATCksc;
123200100308             tibs69r (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
123300100308             IF  O69err = *blanks;
123400100308               pRagSoc = ACOrag;
123500100308               wCatMer = ACOitc;
123600100308               pVia    = INDvia;
123700100308               pLoc    = INDcit;
123800100308               IF  INDsta = *blanks;
123900100308                 pLoc  = %trim(pLoc) + ' ' + INDprv;
124000100308               ELSE;
124100100308                 pLoc  = %trim(pLoc) + ' ' + INDsta;
124200100308               ENDIF;
124300100308               pCap    = %subst(INDcae:1:5);
124400100309               pTel    = INDtel;
124500100309               pClv    = CLPclv;
124600100308             ENDIF;
124700100308
124800100308         //?Dati potenziale
124900100308           WHEN  ATCcpo <> 0;
125000100308             chain ATCcpo TNCPO01L;
125100100308             IF  %found(TNCPO01L);
125200100308               pRagSoc = CPOrag;
125300100308               pVia    = CPOvia;
125400100308               pLoc    = CPOcit;
125500100308               IF  CPOnaz = *blanks;
125600100308                 pLoc  = %trim(pLoc) + ' ' + CPOprv;
125700100308               ELSE;
125800100308                 pLoc  = %trim(pLoc) + ' ' + CPOnaz;
125900100308               ENDIF;
126000100308               pCap    = %subst(CPOcap:1:5);
126100100309               pTel    = CPOtel;
126200100308               wCatMer = CPOsct;
126300100309               pClv    = CPOftr;
126400100308             ENDIF;
126500100308         ENDSL;
126600100308
126700100308         //?Decodifico categoria commerciale
126800100308         clear ds1L;
126900100308         k_tblkut = 1;
127000100308         k_tblcod = '1L';
127100100308         k_tblkey = %editc(wCatMer:'X');
127200100308         chain  %kds(K03tabel) TABEL00F;
127300100308         IF  %found( TABEL00F );
127400100308           ds1L = TBLuni;
127500100308         ENDIF;
127600100308         pDesCat = §1Ldes;
127700100331
127800100331         //?Decodifica commerciale (chi ha preso appuntamento)
127900100712         //?se appuntamento o affiancamento (CHIODO 91/92 e A)
128000100712         IF  ATCtat = 'A' and (ATCcad = '91' or ATCcad = '92');
128100100331           wCmm = ATCco3;
128200100331           exsr sr_DecCmm;
128300130917           pDesCo3 = CMMdes;
128400100331         ENDIF;
128500100504
128600100504         //?CHIODO
128700100504         //?se attvità telefonata devo stampare anche la causale
128800100504         clear pCausale;
128900100504         IF  ATCtat = 'T';
129000100504           pCausale = ATCcad;
129100100504         ENDIF;
129200100308
129300100308         //?Responsabile trasporti
129400100308         clear pRespTra;
129500100308         clear NTCnk1;
129600100308         clear NTCnk2;
129700100308         NTCtnt = '05';
129800100507         SELECT;
129900100507           WHEN  ATCksc > 0;
130000100507             NTCapl = 'C';
130100100507             NTCnk1 = %editc(DUTkci:'X') + %editc(ATCksc:'X');
130200100507           WHEN  ATCnrv > 0;
130300100507             NTCapl = 'T';
130400100507             NTCnk1 = %editc(DUTkci:'X') + %editc(ATCnrv:'X');
130500100507           OTHER;
130600100507             NTCapl = 'P' ;
130700100507             NTCnk1 = %editc(CPOcpo:'X');
130800100507         ENDSL;
130900100507         chain (NTCapl:NTCnk1:NTCnk2:NTCtnt) TFNTC01L;
131000100308         IF  %found(TFNTC01L);
131100100308           pRespTra = NTCrnt;
131200100308         ENDIF;
131300100308
131400100308         //?Dati Info Comm.
131500100308         //?vado con key fissa sulla riga del fatturato annuo presunto 'SPT'
131600100308         clear pFatCpo;
131700100308         clear pSpedCpo;
131800100308         chain (ATCcpo:'SPT') TICPI01L;
131900100308         IF  %found(TICPI01L);
132000100319           pFatCpo  = 'Sp.Trasp.: ' + ' ' + %trim(%editc(CPIpft:'2'));
132100100308           pSpedCpo = 'Sped.Anno: ' + ' ' + %trim(%editc(CPIsna:'2'));
132200100308         ENDIF;
132300100308
132400100308         //?Dati Trattativa
132500100505         clear pTpv;
132600100505         clear pNrv;
132700100308         clear pOfferta;
132800100309         clear pFatNrv;
132900100309         clear pSpedNrv;
133000110119         clear pAuScNrv;
133100100308         IF  ATCnrv > 0;
133200100308           chain ATCnrv TIVIS05L;
133300100308           IF  %found(TIVIS05L) and VISffz <> 'S';
133400100505             pNrv = VISnrv;
133500100505             pTpv = VIStpv;
133600100308           //?controllo se c'è offerta
133700100506             setll ATCnrv TIVOF01L;
133800100506             reade ATCnrv TIVOF01L;
133900100506             DOW  not %EoF(TIVOF01L);
134000100506               IF  VOFeso <> '*';
134100100506                 pOfferta = 'OFF';
134200100506                 leave;
134300100506               ENDIF;
134400100506               reade ATCnrv TIVOF01L;
134500100506             ENDDO;
134600100308           ENDIF;
134700100308           //?Dati Info trattativa
134800100308           //?vado con key fissa sulla riga del fatturato 'TRT'
134900100308           clear pFatNrv;
135000100308           clear pSpedNrv;
135100100308           chain (ATCnrv:'TRT') TIVII01L;
135200100308           IF  %found(TIVII01L);
135300110119             pFatNrv  = 'Valore    : ' + ' ' + %trim(%editc(VIIpft:'K'));
135400100308             pSpedNrv = 'Spedizioni: ' + ' ' + %trim(%editc(VIIsna:'2'));
135500100308           ENDIF;
135600110119           //?Se trattativa di tipo 'A'
135700110119           //?cerco anche 'A/S'
135800110119           IF  VIStpv = 'A';
135900110119             clear pAuScNrv;
136000110119             chain (ATCnrv:'A/S') TIVII01L;
136100110119             IF  %found(TIVII01L);
136200110518               pAuScNrv = 'Aum./Sco. : ' + ' ' + %trim(%editc(VIIvald:'K'))
136300110119                        + ' %';
136400110119             ENDIF;
136500110119           ENDIF;
136600100308         ENDIF;
136700100308
136800100308         //?Note attività (2 righe)
136900100520         clear pNota01;
137000100520         clear pNota02;
137100100520         clear pNota03;
137200100520         clear pNota04;
137300100520
137400100520         //?Prima provo con note legate al progressivo attuale
137500100308         wNota = 1;
137600100520         CPNcpo  = ATCcpo;
137700100520         CPNatn  = ATCatn;
137800100520         CPNatnp = ATCatnp;
137900100520         setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
138000100520         reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
138100100520
138200100520         DOW  not %eof(TICPN05L) and wNota <= 2;
138300100520           IF  wNota = 1;
138400100520             pNota01 = %subst(CPNnot:1:45);
138500100520             pNota02 = %subst(CPNnot:46:45);
138600100520           ENDIF;
138700100520           IF  wNota = 2;
138800100520             pNota03 = %subst(CPNnot:1:45);
138900100520             pNota04 = %subst(CPNnot:46:45);
139000100520           ENDIF;
139100100520
139200100520           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
139300100520           wNota += 1;
139400100520         ENDDO;
139500100520
139600100520         //?Se non ho trovato note, provo con note legate al progressivo -1
139700100520         IF  pNota01 = *blanks and pNota02 = *blanks and
139800100520             pNota03 = *blanks and pNota04 = *blanks;
139900100520           CPNatnp = ATCatnp - 1;
140000100520           setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
140100100520           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
140200100308
140300100520           DOW  not %eof(TICPN05L) and wNota <= 2;
140400100520             IF  wNota = 1;
140500100520               pNota01 = %subst(CPNnot:1:45);
140600100520               pNota02 = %subst(CPNnot:46:45);
140700100520             ENDIF;
140800100520             IF  wNota = 2;
140900100520               pNota03 = %subst(CPNnot:1:45);
141000100520               pNota04 = %subst(CPNnot:46:45);
141100100520             ENDIF;
141200100520
141300100520             reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
141400100520             wNota += 1;
141500100520           ENDDO;
141600100520         ENDIF;
141700100505
141800100505         //?Se non ci sono note legate all'attività controllo se ci sono note legate alla trattativa
141900100505         IF  ATCnrv > 0 and pNota01 = *blanks and pNota02 = *blanks and
142000100505                            pNota03 = *blanks and pNota04 = *blanks;
142100100505           CPNksc = ATCksc;
142200100505           CPNnrv = ATCnrv;
142300100505           setll (CPNcpo:CPNksc:CPNnrv) TICPN04L;
142400100505           reade (CPNcpo:CPNksc:CPNnrv) TICPN04L;
142500100505
142600100505           DOW  not %eof(TICPN04L) and wNota <= 2;
142700100505             IF  wNota = 1;
142800100505               pNota01 = %subst(CPNnot:1:45);
142900100505               pNota02 = %subst(CPNnot:46:45);
143000100505             ENDIF;
143100100505             IF  wNota = 2;
143200100505               pNota03 = %subst(CPNnot:1:45);
143300100505               pNota04 = %subst(CPNnot:46:45);
143400100505             ENDIF;
143500100505
143600100505             reade (CPNcpo:CPNksc:CPNnrv) TICPN04L;
143700100505             wNota += 1;
143800100505           ENDDO;
143900100505         ENDIF;
144000100308
144100100308         //?Salto pagina
144200100506         //IF  CurLine >= 64;
144300100506         IF  *in99;
144400100308           exsr sr_StpSalto;
144500100310           clear wrkriga;
144600100521           write MK89SEPARA;
144700100308           *in99 = *off;
144800100308         ENDIF;
144900100305
145000100521         write MK89RIGA;
145100100309
145200100309       ENDSR;
145300100712
145400100712       //--------------------------------------------------------------
145500100712       //?Stampo i Concorrenti.
145600100712       //--------------------------------------------------------------
145700100712       BEGSR sr_StpConc;
145800100712
145900100712         clear pConc;
146000100712         wConc = 1;
146100100712
146200100712         //?Cerco concorrenti
146300100712         setll (ATCcpo:'10') TICPI01L;
146400100712         reade (ATCcpo:'10') TICPI01L;
146500100712         DOW not %eof(TICPI01L);
146600100712           IF  CPIval > 0 and CPIspf <> '_BAR';
146700100712         //?imposto il campo da stampare
146800100712             IF  pConc = *blanks;
146900100712               pConc = 'Corrieri Utilizzati:';
147000100712             ENDIF;
147100100712             clear TIBS02ds;
147200100712             clear dIFS;
147300100712             T02mod = 'C';
147400100712             T02cod = 'IFS';
147500100712             T02ke1 = CPItpf;
147600100712             T02ke2 = CPIspf;
147700100712             T02sif = KNSIF;
147800100712             TNTBE_RicercaControllo (kpjba : tibs02ds);
147900100712             IF  T02err = *blanks;
148000100712               dIFS = T02uni;
148100100712             ENDIF;
148200100712             IF  wConc > 1;
148300100712             //?Aggiungo concorrente fino al massimo di 186 byte
148400100712               IF  (%len(%trim(pConc)) + %len(%trim(§IFSdes)) + 2) > 185;
148500100712                 Pconc = %trim(Pconc) + ' Altri...';
148600100712                 leave;
148700100712               ENDIF;
148800100712               pConc = %trim(pConc) + ',';
148900100712             ENDIF;
149000100712             pConc = %trim(pConc) + ' ' + §IFSdes;
149100100712             wConc += 1;
149200100712           ENDIF;
149300100712           reade (ATCcpo:'10') TICPI01L;
149400100712         ENDDO;
149500100712
149600100712         //?Salto pagina
149700100712         IF  CurLine >= 65;
149800100712           exsr sr_StpSalto;
149900100712           *in99 = *off;
150000100712         ENDIF;
150100100712
150200100712         IF  pConc <> *blanks;
150300100712           write MK89CONC;
150400100712         ENDIF;
150500100712
150600100712       ENDSR;
150700100712
150800100309
150900100309       //--------------------------------------------------------------
151000100309       //?Stampo le attività a dettaglio UFFICIO.
151100100309       //--------------------------------------------------------------
151200100309       BEGSR sr_StpDetUFF;
151300100309
151400100309         clear pAttUff;
151500100309
151600100309         //?decodifico attività
151700100309         clear TIBS02ds;
151800100309         clear dCCO;
151900100309         T02mod = 'C';
152000100309         T02cod = 'CCO';
152100100309         T02ke1 = ATCcad;
152200100309         T02sif = KNSIF;
152300100309         TNTBE_RicercaControllo (kpjba : tibs02ds);
152400100309         IF  T02err = *blanks;
152500100309           dCCO = T02uni;
152600100309         ENDIF;
152700100309
152800100309         //?CHIODO --> non stampo la decodifica della causale se FERIE
152900100309         IF  ATCtat <> 'F';
153000100309           pAttUff = §CCOdes;
153100100309           pAttUff = %trim(pAttUff) + ' Dalle ' +
153200100309                     %subst(%editc(ATCoii:'X'):1:2) +
153300100309                     ':' + %subst(%editc(ATCoii:'X'):3:2) +
153400100309                     ' Alle ' +
153500100309                     %subst(%editc(ATCofi:'X'):1:2) + ':' +
153600100309                     %subst(%editc(ATCofi:'X'):3:2);
153700100309         ELSE;
153800100309           pAttUff = 'Dalle ' +
153900100309                     %subst(%editc(ATCoii:'X'):1:2) +
154000100309                     ':' + %subst(%editc(ATCoii:'X'):3:2) +
154100100309                     ' Alle ' +
154200100309                     %subst(%editc(ATCofi:'X'):1:2) + ':' +
154300100309                     %subst(%editc(ATCofi:'X'):3:2);
154400100309         ENDIF;
154500100712
154600100712         //?Note attività (1 riga)
154700100712         clear pNota01;
154800100712         clear pNota02;
154900100712
155000100712         //?Leggo le note legate al progressivo attuale
155100100712         wNota = 1;
155200100712         clear CPNcpo;
155300100712         CPNatn  = ATCatn;
155400100712         CPNatnp = ATCatnp;
155500100712         setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
155600100712         reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
155700100712
155800100712         DOW  not %eof(TICPN05L) and wNota <= 1;
155900100712           IF  wNota = 1;
156000100712             pNota01 = %subst(CPNnot:1:45);
156100100712             pNota02 = %subst(CPNnot:46:45);
156200100712           ENDIF;
156300100712
156400100712           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
156500100712           wNota += 1;
156600100712         ENDDO;
156700100712
156800100712         //?Se non ho trovato note, provo con note legate al progressivo -1
156900100712         IF  pNota01 = *blanks and pNota02 = *blanks;
157000100712           CPNatnp = ATCatnp - 1;
157100100712           setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
157200100712           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
157300100712
157400100712           DOW  not %eof(TICPN05L) and wNota <= 1;
157500100712             IF  wNota = 1;
157600100712               pNota01 = %subst(CPNnot:1:45);
157700100712               pNota02 = %subst(CPNnot:46:45);
157800100712             ENDIF;
157900100712
158000100712             reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
158100100712             wNota += 1;
158200100712           ENDDO;
158300100712         ENDIF;
158400100309
158500100309         //?Salto pagina
158600100309         IF  *in99;
158700100309           exsr sr_StpSalto;
158800100309           *in99 = *off;
158900100309         ENDIF;
159000100309
159100100521         write MK89RIGAUF;
159200100309
159300100309       ENDSR;
159400100309
159500100309       //--------------------------------------------------------------
159600100309       //?Stampo l'eventuale affiancamento.
159700100309       //--------------------------------------------------------------
159800100309       BEGSR sr_StpAffianca;
159900100309
160000100309         clear pAffianca;
160100100309         wAff = 1;
160200100309
160300100309         SELECT;
160400100309         //?se appuntamento (CHIODO 91 e A)
160500100309           WHEN  ATCtat = 'A' and ATCcad = '91';
160600100309         //?controllo se c'è affiancamento
160700100309             ATLatl  = ATCatn;
160800100309             ATLatlp = ATCatnp;
160900100309             setll (ATLatl:ATLatlp) TIATL02L;
161000100309             reade (ATLatl:ATLatlp) TIATL02L;
161100100309             DOW  not %eof(TIATL02L);
161200100309         //?imposto il campo da stampare
161300100309               IF  pAffianca = *blanks;
161400100705                 pAffianca = '  Affiancato da:';
161500100309               ENDIF;
161600100309         //?leggo attività affiancamento e imposto il nome dell'agente sul campo da stampare
161700100309               w_ATCtat  = ATCtat;
161800100309               w_ATCatn  = ATLatn;
161900100309               w_ATCatnp = ATLatnp;
162000100309               chain (w_ATCtat:w_ATCatn:w_ATCatnp) TIATC01L;
162100100309               IF  %found(TIATC01L);
162200100309                 wCmm = w_ATCcmm;
162300100309                 exsr sr_DecCmm;
162400100309                 IF  wAff > 1;
162500100319             //?Aggiungo nome agente fino al massimo di 196 byte
162600130917                   IF  (%len(%trim(pAffianca)) + %len(%trim(CMMdes)) + 2) > 196;
162700100309                     leave;
162800100309                   ENDIF;
162900100309                   pAffianca = %trim(pAffianca) + ',';
163000100309                 ENDIF;
163100130917                 pAffianca = %trim(pAffianca) + ' ' + CMMdes;
163200100309               ENDIF;
163300100309               wAff += 1;
163400100309               reade (ATLatl:ATLatlp) TIATL02L;
163500100309             ENDDO;
163600100309         //?Stampo l'affiancamento
163700100309             IF  pAffianca <> *blanks;
163800100309             //?Salto pagina
163900100309               IF  *in99;
164000100309                 exsr sr_StpSalto;
164100100309                 *in99 = *off;
164200100309               ENDIF;
164300100521               write MK89AFF;
164400100309             ENDIF;
164500100309
164600100309         //?se affiancamento (CHIODO 92 e A)
164700100309           WHEN  ATCtat = 'A' and ATCcad = '92';
164800100309         //?controllo l'affiancamento
164900100309             ATLatn  = ATCatn;
165000100309             ATLatnp = ATCatnp;
165100100309             chain (ATLatn:ATLatnp) TIATL01L;
165200100309             IF  %found(TIATL01L);
165300100309         //?imposto il campo da stampare
165400100309               IF  pAffianca = *blanks;
165500100705                 pAffianca = '  In affiancamento a:';
165600100309               ENDIF;
165700100309         //?leggo attività affiancamento e imposto il nome dell'agente sul campo da stampare
165800100309               w_ATCtat  = ATCtat;
165900100309               w_ATCatn  = ATLatl;
166000100309               w_ATCatnp = ATLatlp;
166100100309               chain (w_ATCtat:w_ATCatn:w_ATCatnp) TIATC01L;
166200100309               IF  %found(TIATC01L);
166300100309                 wCmm = w_ATCcmm;
166400100309                 exsr sr_DecCmm;
166500130917                 pAffianca = %trim(pAffianca) + ' ' + CMMdes;
166600100309               ENDIF;
166700100309             ENDIF;
166800100309         //?Stampo l'affiancamento
166900100309             IF  pAffianca <> *blanks;
167000100309             //?Salto pagina
167100100309               IF  *in99;
167200100309                 exsr sr_StpSalto;
167300100309                 *in99 = *off;
167400100309               ENDIF;
167500100521               write MK89AFF;
167600100309             ENDIF;
167700100309
167800100309         ENDSL;
167900100309
168000100309       ENDSR;
168100080206
168200100521
168300100521       //--------------------------------------------------------------
168400100521       //?Stampa Statistica Consuntiva
168500100521       //--------------------------------------------------------------
168600100521       BEGSR Stat_consun ;
168700100521
168800100521       // cerco il record del consuntivo
168900100521
169000100521       wcmm = $ageu(zz) ;
169100100521         exec sql
169200100521          declare CON cursor for
169300100521          select *
169400100521          from WFRCA00f where RCArgf = :wcmm      ;
169500100521
169600100521         exec sql
169700100521           open CON;
169800100521           IF sqlcode >= 0;
169900100521
170000100521           exec sql
170100100521             fetch next from CON into :WFRCA00F ;
170200100521             IF sqlcod = 100 or sqlcod < 0;
170300100521             Else ;
170400100524        // stampo consuntivo per commerciale unificante se non escluso
170500100524           select ;
170600100524         // se omessi gli interni
170700101006            when i88cscmm = 'O'  and i88cscmmie = 'I' and (RCAFUN = 'COMIN'
170800101006                 or RCAfun = 'ASC' or RCAfun = 'SA');
170900100524         // se omessi gli esterni
171000101006            when i88cscmm = 'O'  and i88cscmmie = 'E' and RCAFUN <> 'COMIN'
171100101006                 and RCAfun <> 'ASC' and RCAfun <> 'SA';
171200100524         // se inclusi gli esterni
171300101006            when i88cscmm = 'I'  and i88cscmmie = 'E' and (RCAFUN =  'COMIN'
171400101006                 or RCAfun = 'ASC' or RCAfun = 'SA');
171500100524         // se inclusi gli interni
171600101006            when i88cscmm = 'I'  and i88cscmmie = 'I' and RCAFUN <> 'COMIN'
171700101006                 and RCAfun <> 'ASC' and RCAfun <> 'SA';
171800100524          other ;
171900100521
172000110422         // preparo le causali da stampare
172100110426           $caastp = $stp;
172200110426           causalids = Cauwds ;
172300110426           cauineds = Cauwds ;
172400110428         // pulisco i campi in stampa e di lavoro
172500110426           clear Qtapmds ;
172600110426           clear Qtapgds ;
172700110426           clear Qtappds ;
172800110426           clear Qtacfds ;
172900110426           clear Qtaptds ;
173000110426           clear Qtactds ;
173100110426           clear Qtapptds ;
173200110426           clear Qtainds ;
173300110428
173400110428           clear Qtapmwds ;
173500110428           clear Qtapgwds ;
173600110428           clear Qtappwds ;
173700110428           clear Qtacfwds ;
173800110428           clear Qtaptwds ;
173900110428           clear Qtactwds ;
174000110428           clear Qtapptwds ;
174100110428           clear Qtainwds ;
174200110422
174300110422         // telefonate potenziali mai trattati
174400110422           Rcacaapmds = Rcacaapm ;
174500110422           Rcanrapmds = Rcanrapm ;
174600110422
174700110422           xx = 1 ;
174800110422           For xx by 1 to %elem($cau) ;
174900110426               contacau = $cau(xx);
175000110422               If uno <> ' ';
175100110422                  xpm = %lookup(uno: $Caapm) ;
175200110422                  If xpm > 0;
175300110422                     $qtaPm(xx) += $Nrapm(xpm) ;
175400110422                  Endif ;
175500110422               Endif ;
175600110422               If due <> ' ';
175700110422                  xpm = %lookup(due: $Caapm) ;
175800110422                  If xpm > 0;
175900110422                     $qtaPm(xx) += $Nrapm(xpm) ;
176000110422                  Endif ;
176100110422               Endif ;
176200110422           Endfor ;
176300110422           Qtapmds = Qtapmwds ;
176400110422
176500110422         // telefonate potenziali già trattati
176600110422           Rcacaapgds = Rcacaapg ;
176700110422           Rcanrapgds = Rcanrapg ;
176800110422
176900110422           xx = 1 ;
177000110422           For xx by 1 to %elem($cau) ;
177100110426               contacau = $cau(xx);
177200110422               If uno <> ' ';
177300110422                  xpg = %lookup(uno: $Caapg) ;
177400110422                  If xpg > 0;
177500110422                     $qtaPg(xx) += $Nrapg(xpg) ;
177600110422                  Endif ;
177700110422               Endif ;
177800110422               If due <> ' ';
177900110422                  xpg = %lookup(due: $Caapg) ;
178000110422                  If xpg > 0;
178100110422                     $qtaPg(xx) += $Nrapg(xpg) ;
178200110422                  Endif ;
178300110422               Endif ;
178400110422           Endfor ;
178500110422           Qtapgds = Qtapgwds ;
178600110422
178700110422         // telefonate potenziali persi
178800110422           Rcacaappds = Rcacaapp ;
178900110422           Rcanrappds = Rcanrapp ;
179000110422
179100110422           xx = 1 ;
179200110422           For xx by 1 to %elem($cau) ;
179300110426               contacau = $cau(xx);
179400110422               If uno <> ' ';
179500110422                  xpp = %lookup(uno: $Caapp) ;
179600110422                  If xpp > 0;
179700110422                     $qtaPp(xx) += $Nrapp(xpp) ;
179800110422                  Endif ;
179900110422               Endif ;
180000110422               If due <> ' ';
180100110422                  xpp = %lookup(due: $Caapp) ;
180200110422                  If xpp > 0;
180300110422                     $qtaPp(xx) += $Nrapp(xpp) ;
180400110422                  Endif ;
180500110422               Endif ;
180600110422           Endfor ;
180700110422           Qtappds = Qtappwds ;
180800110422
180900110422         // telefonate clienti fuori trattativa
181000110422           Rcacaacfds = Rcacaacf ;
181100110422           Rcanracfds = Rcanracf ;
181200110422
181300110422           xx = 1 ;
181400110422           For xx by 1 to %elem($cau) ;
181500110426               contacau = $cau(xx);
181600110422               If uno <> ' ';
181700110422                  xcf = %lookup(uno: $Caapp) ;
181800110422                  If xcf > 0;
181900110422                     $qtaCf(xx) += $Nracf(xcf) ;
182000110422                  Endif ;
182100110422               Endif ;
182200110422               If due <> ' ';
182300110422                  xcf = %lookup(due: $Caacf) ;
182400110422                  If xcf > 0;
182500110422                     $qtaCf(xx) += $Nracf(xcf) ;
182600110422                  Endif ;
182700110422               Endif ;
182800110422           Endfor ;
182900110422           Qtacfds = Qtacfwds ;
183000110422
183100110422         // telefonate potenziali dentro trattativa
183200110422           Rcacaaptds = Rcacaapt ;
183300110422           Rcanraptds = Rcanrapt ;
183400110422
183500110422           xx = 1 ;
183600110422           For xx by 1 to %elem($cau) ;
183700110426               contacau = $cau(xx);
183800110422               If uno <> ' ';
183900110422                  xpt = %lookup(uno: $Caapt) ;
184000110422                  If xpt > 0;
184100110422                     $qtaPt(xx) += $Nrapt(xpt) ;
184200110422                  Endif ;
184300110422               Endif ;
184400110422               If due <> ' ';
184500110422                  xpt = %lookup(due: $Caapt) ;
184600110422                  If xpt > 0;
184700110422                     $qtaPt(xx) += $Nrapt(xpt) ;
184800110422                  Endif ;
184900110422               Endif ;
185000110422           Endfor ;
185100110422           Qtaptds = Qtaptwds ;
185200110422
185300110422         // telefonate clienti    dentro trattativa
185400110422           Rcacaactds = Rcacaact ;
185500110422           Rcanractds = Rcanract ;
185600110422
185700110422           xx = 1 ;
185800110422           For xx by 1 to %elem($cau) ;
185900110426               contacau = $cau(xx);
186000110422               If uno <> ' ';
186100110422                  xct = %lookup(uno: $Caact) ;
186200110422                  If xct > 0;
186300110422                     $qtaCt(xx) += $Nract(xct) ;
186400110422                  Endif ;
186500110422               Endif ;
186600110422               If due <> ' ';
186700110422                  xct = %lookup(due: $Caact) ;
186800110422                  If xct > 0;
186900110422                     $qtact(xx) += $Nract(xct) ;
187000110422                  Endif ;
187100110422               Endif ;
187200110422           Endfor ;
187300110422           Qtactds = Qtactwds ;
187400110422
187500110422         // telefonate potenziali persi dentro trattativa
187600110422           Rcacaapptds = Rcacaappt ;
187700110422           Rcanrapptds = Rcanrappt ;
187800110422
187900110422           xx = 1 ;
188000110422           For xx by 1 to %elem($cau) ;
188100110426               contacau = $cau(xx);
188200110422               If uno <> ' ';
188300110422                  xppt = %lookup(uno: $Caappt) ;
188400110422                  If xppt > 0;
188500110422                     $qtaPpt(xx) += $Nrappt(xppt) ;
188600110422                  Endif ;
188700110422               Endif ;
188800110422               If due <> ' ';
188900110422                  xppt = %lookup(due: $Caappt) ;
189000110422                  If xppt > 0;
189100110422                     $qtaPpt(xx) += $Nrappt(xppt) ;
189200110422                  Endif ;
189300110422               Endif ;
189400110422           Endfor ;
189500110422           Qtapptds = Qtapptwds ;
189600110422
189700110422         // telefonate inevase
189800110422           Rcacaaids = Rcacaai ;
189900110422           Rcanraids = Rcanrai ;
190000110422
190100110422           xx = 1 ;
190200110422           For xx by 1 to %elem($cau) ;
190300110426               contacau = $cau(xx);
190400110422               If uno <> ' ';
190500110422                  xin = %lookup(uno: $Caai) ;
190600110422                  If xin > 0;
190700110422                     $qtain(xx) += $Nrai(xin) ;
190800110422                  Endif ;
190900110422               Endif ;
191000110422               If due <> ' ';
191100110422                  xin = %lookup(due: $Caai) ;
191200110422                  If xin > 0;
191300110422                     $qtain(xx) += $Nrai(xin) ;
191400110422                  Endif ;
191500110422               Endif ;
191600110422           Endfor ;
191700110422           Qtainds = Qtainwds ;
191800110422
191900100622        // totali appuntamenti
192000110119           totapppr = Rcaapptn + Rcaappta + Rcaapptm + Rcaapptc +
192100110119                      Rcaappti + Rcaapptr ;
192200100629        // totali appuntamenti di cui il primo
192300110119           totapppr1 = Rcapatn + Rcapata  + Rcapatm + Rcapatc  +
192400110119                       Rcapati + Rcapatr  ;
192500100628        // totali telefonate post offerta di altri
192600100628           Tottepoa = (rcatepo30a + Rcatepo33a) ;
192700110426        // totali telefonate caldi di altri
192800110428           Tottecalda = (rcatepc31a + Rcatepc44a) ;
192900100819        // totali appuntamenti eseguiti
193000110119           totappes = Rcaaptn + Rcaapta + Rcaaptm + Rcaaptc +
193100110119                      Rcaapti + Rcaaptr ;
193200100819        // totali affiancamenti eseguiti
193300110119           totaffes = Rcaaftn + Rcaafta + Rcaaftm + Rcaaftc +
193400110119                      Rcaafti + Rcaaftr  ;
193500100521           tipostampa = 'C' ;
193600100524         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
193700100524            *in11 = *on ;
193800100524
193900100524         //?Imposto data inizio e fine stampa
194000100524            wDate_ISO = %date(I88deci:*iso);
194100100524            wDate_EUR = wDate_ISO;
194200100524            pDatai    = %dec(wDate_EUR);
194300100524            wDate_ISO = %date(I88decf:*iso);
194400100524            wDate_EUR = wDate_ISO;
194500100524            pDataf    = %dec(wDate_EUR);
194600100622          // calcolo l'incidenza  degli appuntamenti dulle telefonate effettuate
194700100622            exsr sr_incidenza ;
194800100622          // prima verifico che tutte i fattori siano valorizzati
194900100524            exsr sr_StpSalto;
195000100524            write mk89rigcon ;
195100100524           ENDSL;
195200100521          endif ;
195300100521          endif ;
195400100521
195500100521         exec sql close CON;
195600100521
195700100521       ENDSR;
195800100521
195900100622       //--------------------------------------------------------------
196000100622       //?Calcolo incidenza appuntamenti presi su telefonate Potenziali
196100100622       //--------------------------------------------------------------
196200100622       BEGSR Sr_incidenza ;
196300100622
196400100622          // prima verifico che tutti i fattori siano valorizzati
196500110422         If Rcapatn > 0 and (rcatepm > 0 or rcatepg > 0 or rcatepp > 0 ) ;
196600110422            Incide = ((100 * rcapatn) / (rcatepm + rcatepg + rcatepp)) ;
196700100622         else ;
196800100622            clear Incide ;
196900100622         endif ;
197000100622
197100100622       ENDSR;
197200100622
197300100522       //--------------------------------------------------------------
197400100522       //?Stampa Statistica Preventiva
197500100522       //--------------------------------------------------------------
197600100522       BEGSR Stat_preven ;
197700100522
197800100522       // cerco il record del preventivo
197900100524         wrkriga1= '*' + wTrat + '*';
198000100522
198100100522       wcmm = $ageu(zz) ;
198200100522         exec sql
198300100522          declare PRE cursor for
198400100522          select *
198500100522          from WFRPA00f where RPArgf = :wcmm      ;
198600100522
198700100522         exec sql
198800100522           open PRE;
198900100522           IF sqlcode >= 0;
199000100522
199100100522           exec sql
199200100522             fetch next from PRE into :WFRPA00F ;
199300100522             IF sqlcod = 100 or sqlcod < 0;
199400100522             Else ;
199500100524        // stampo preventivo per commerciale unificante se non escluso
199600100524           select ;
199700100524         // se omessi gli interni
199800101006            when i88cscmm = 'O'  and i88cscmmie = 'I' and (RPAFUN = 'COMIN'
199900101006                 or RPAfun = 'ASC' or RPAfun = 'SA');
200000100524         // se omessi gli esterni
200100101006            when i88cscmm = 'O'  and i88cscmmie = 'E' and RPAFUN <> 'COMIN'
200200101006                 and RPAfun <> 'ASC' and RPAfun <> 'SA';
200300100524         // se inclusi gli esterni
200400101006            when i88cscmm = 'I'  and i88cscmmie = 'E' and (RPAFUN =  'COMIN'
200500101006                 or RPAfun = 'ASC' or RPAfun = 'SA');
200600100524         // se inclusi gli interni
200700101006            when i88cscmm = 'I'  and i88cscmmie = 'I' and RPAFUN <> 'COMIN'
200800101006                 and RPAfun <> 'ASC' and RPAfun <> 'SA';
200900100524          other ;
201000100524         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
201100100524            *in11 = *on ;
201200100524
201300100524         //?Imposto data inizio e fine stampa
201400100527            wDate_ISO = %date(I88depi:*iso);
201500100524            wDate_EUR = wDate_ISO;
201600100524            pDatai    = %dec(wDate_EUR);
201700100527            wDate_ISO = %date(I88depf:*iso);
201800100524            wDate_EUR = wDate_ISO;
201900100524            pDataf    = %dec(wDate_EUR);
202000100524
202100100522           tipostampa = 'P' ;
202200100522           exsr sr_StpSalto;
202300100522        // calcolo totali appuntamenti
202400110119           totapp = (rpaappm + rpaappn + rpaappa + rpaappc +
202500110119                     rpaappi + rpaappr);
202600100522        // calcolo totali telefonate
202700100524           tottel = (rpatc19 + rpatc20 + rpatc21 + rpatc22 + rpatc24 +
202800100524                     rpatc25 + rpatc26 + rpatc27 + rpatc28 + rpatc30 +
202900110422                     rpatc31 + rpatc32 + rpatc33 + rpatc44 + rpatc60 +
203000110422                     rpatc70 + rpatc71 + rpatc73 + rpatc74 + rpatc75);
203100100524        // Verifico se affiancamenti valorizzati
203200110119           *in30 = rpaaffn > 0 ;
203300110119           *in31 = rpaaffi > 0 ;
203400110119           *in32 = rpaaffa > 0 ;
203500110119           *in33 = rpaaffm > 0 ;
203600110119           *in34 = rpaaffc > 0 ;
203700110119           *in35 = rpaaffr > 0 ;
203800100524           write Mk89rigpre ;
203900100526           write Mk89rigpr1 ;
204000100524          ENDSL ;
204100100522          endif ;
204200100522          endif ;
204300100522
204400100522         exec sql close PRE;
204500100522
204600100522       ENDSR;
204700100522
204800100525       //--------------------------------------------------------------
204900100525       //?Stampa Totali generali Statistica
205000100525       //--------------------------------------------------------------
205100100526       BEGSR SR_Totgen  ;
205200100525
205300110426         // pulisco i campi in stampa
205400110426           clear Qtapmds ;
205500110426           clear Qtapgds ;
205600110426           clear Qtappds ;
205700110426           clear Qtacfds ;
205800110426           clear Qtaptds ;
205900110426           clear Qtactds ;
206000110426           clear Qtapptds ;
206100110426           clear Qtainds ;
206200110428           clear Qtapmwds ;
206300110428           clear Qtapgwds ;
206400110428           clear Qtappwds ;
206500110428           clear Qtacfwds ;
206600110428           clear Qtaptwds ;
206700110428           clear Qtactwds ;
206800110428           clear Qtapptwds ;
206900110428           clear Qtainwds ;
207000110426
207100110426         // preparo le causali da stampare
207200110426           $caastp = $stp;
207300110426           causalids = Cauwds ;
207400110426           cauineds = Cauwds ;
207500110426
207600100528         wrkriga1= '*' + wTrat + '*';
207700100622         *in20 = *on ;
207800100525       // verifico se ho lanciato il consuntivo
207900100526       If i88tcpe = 'C' or i88tcpe = 'E' ;
208000100525       //?Ciclo per sk unificanti e stampo
208100100525           zz = 1;
208200100526           FOR zz by 1 to %elem($AgeU);
208300100526           IF  $AgeU(zz) <> *zeros;
208400100525
208500100525               wcmm = $ageu(zz) ;
208600100525               exec sql
208700100525                 declare COG cursor for
208800100525                 select *
208900100525                 from WFRCA00f where RCArgf = :wcmm      ;
209000100525
209100100525                 exec sql
209200100525                 open COG;
209300100526                 IF sqlcode >= 0;
209400100525
209500100525                    exec sql
209600100525                    fetch next from COG into :WFRCA00F ;
209700100526                    IF sqlcod = 100 or sqlcod < 0;
209800100525                    Else ;
209900100525        // stampo statistica generale facendo attenzione alle inclusioni/esclusioni
210000100525                     select ;
210100100525         // se omessi gli interni
210200101007                       when i88tscmm='O' and i88tscmmie='I' and (RCAFUN='COMIN'
210300101007                       or RCAfun = 'ASC' or RCAfun = 'SA');
210400100526         // se omessi gli esterni
210500101007                       when i88tscmm='O' and i88tscmmie='E' and RCAFUN<>'COMIN'
210600101007                       and RCAfun <> 'ASC' and RCAfun <> 'SA';
210700100525         // se inclusi gli esterni
210800101007                       when i88tscmm='I' and i88tscmmie='E' and (RCAFUN='COMIN'
210900101007                       or RCAfun = 'ASC' or RCAfun = 'SA');
211000100525         // se inclusi gli interni
211100101007                       when i88tscmm='I' and i88tscmmie='I' and RCAFUN<>'COMIN'
211200101007                       and RCAfun <> 'ASC' and RCAfun <> 'SA';
211300100525                     other ;
211400110426
211500110426         // telefonate potenziali mai trattati
211600110426           Wrkcaads = Rcacaapm ;
211700110426           Wrknrads = Rcanrapm ;
211800110426
211900110426           xx = 1 ;
212000110426           For xx by 1 to %elem($cau) ;
212100110426               contacau = $cau(xx);
212200110426               If uno <> ' ';
212300110426                  yy = %lookup(uno: $wrkCaa) ;
212400110426                  If yy > 0;
212500110426                     $qtaPm(xx) += $WrkNra(yy) ;
212600110426                  Endif ;
212700110426               Endif ;
212800110426               If due <> ' ';
212900111017                  yy  = %lookup(due: $wrkCaa) ;
213000110426                  If yy > 0;
213100110426                     $qtaPm(xx) += $WrkNra(yy) ;
213200110426                  Endif ;
213300110426               Endif ;
213400110426           Endfor ;
213500100525
213600110426         // telefonate potenziali già trattati
213700110426           Wrkcaads = Rcacaapg ;
213800110426           Wrknrads = Rcanrapg ;
213900110426
214000110426           xx = 1 ;
214100110426           For xx by 1 to %elem($cau) ;
214200110426               contacau = $cau(xx);
214300110426               If uno <> ' ';
214400110426                  yy = %lookup(uno: $wrkCaa) ;
214500110426                  If yy > 0;
214600110428                     $qtaPg(xx) += $WrkNra(yy) ;
214700110426                  Endif ;
214800110426               Endif ;
214900110426               If due <> ' ';
215000111017                  yy  = %lookup(due: $wrkCaa) ;
215100110426                  If yy > 0;
215200110428                     $qtaPg(xx) += $WrkNra(yy) ;
215300110426                  Endif ;
215400110426               Endif ;
215500110426           Endfor ;
215600110426
215700110426         // telefonate potenziali persi
215800110426           Wrkcaads = Rcacaapp ;
215900110426           Wrknrads = Rcanrapp ;
216000110426
216100110426           xx = 1 ;
216200110426           For xx by 1 to %elem($cau) ;
216300110426               contacau = $cau(xx);
216400110426               If uno <> ' ';
216500110426                  yy = %lookup(uno: $wrkCaa) ;
216600110426                  If yy > 0;
216700110426                     $qtaPp(xx) += $WrkNra(yy) ;
216800110426                  Endif ;
216900110426               Endif ;
217000110426               If due <> ' ';
217100110426                  yy = %lookup(due: $wrkCaa) ;
217200110426                  If yy > 0;
217300110426                     $qtaPp(xx) += $WrkNra(yy) ;
217400110426                  Endif ;
217500110426               Endif ;
217600110426           Endfor ;
217700100609
217800110426         // telefonate clienti fuori trattativa
217900110426           Wrkcaads = Rcacaacf ;
218000110426           Wrknrads = Rcanracf ;
218100110426
218200110426           xx = 1 ;
218300110426           For xx by 1 to %elem($cau) ;
218400110426               contacau = $cau(xx);
218500110426               If uno <> ' ';
218600110426                  yy = %lookup(uno: $wrkCaa) ;
218700110426                  If yy > 0;
218800110426                     $qtaCf(xx) += $WrkNra(yy) ;
218900110426                  Endif ;
219000110426               Endif ;
219100110426               If due <> ' ';
219200110426                  yy = %lookup(due: $wrkCaa) ;
219300110426                  If yy > 0;
219400110426                     $qtaCf(xx) += $WrkNra(yy) ;
219500110426                  Endif ;
219600110426               Endif ;
219700110426           Endfor ;
219800110426
219900110426         // telefonate potenziali dentro trattativa
220000110426           Wrkcaads = Rcacaapt ;
220100110426           Wrknrads = Rcanrapt ;
220200110426
220300110426           xx = 1 ;
220400110426           For xx by 1 to %elem($cau) ;
220500110426               contacau = $cau(xx);
220600110426               If uno <> ' ';
220700110426                  yy = %lookup(uno: $wrkCaa) ;
220800110426                  If yy > 0;
220900110426                     $qtaPt(xx) += $WrkNra(yy) ;
221000110426                  Endif ;
221100110426               Endif ;
221200110426               If due <> ' ';
221300110426                  yy = %lookup(due: $wrkCaa) ;
221400110426                  If yy > 0;
221500110426                     $qtaPt(xx) += $WrkNra(yy) ;
221600110426                  Endif ;
221700110426               Endif ;
221800110426           Endfor ;
221900110426
222000110426         // telefonate clienti    dentro trattativa
222100110426           Wrkcaads = Rcacaact ;
222200110426           Wrknrads = Rcanract ;
222300110426
222400110426           xx = 1 ;
222500110426           For xx by 1 to %elem($cau) ;
222600110426               contacau = $cau(xx);
222700110426               If uno <> ' ';
222800110426                  yy = %lookup(uno: $wrkCaa) ;
222900110426                  If yy > 0;
223000110426                     $qtaCt(xx) += $WrkNra(yy) ;
223100110426                  Endif ;
223200110426               Endif ;
223300110426               If due <> ' ';
223400110426                  yy = %lookup(due: $wrkCaa) ;
223500110426                  If yy > 0;
223600110426                     $qtaCt(xx) += $WrkNra(yy) ;
223700110426                  Endif ;
223800110426               Endif ;
223900110426           Endfor ;
224000110426
224100110426         // telefonate potenziali persi dentro trattativa
224200110426           Wrkcaads = Rcacaappt ;
224300110426           Wrknrads = Rcanrappt ;
224400110426
224500110426           xx = 1 ;
224600110426           For xx by 1 to %elem($cau) ;
224700110426               contacau = $cau(xx);
224800110426               If uno <> ' ';
224900110426                  yy = %lookup(uno: $wrkCaa) ;
225000110426                  If yy > 0;
225100110426                     $qtaPpt(xx) += $WrkNra(yy) ;
225200110426                  Endif ;
225300110426               Endif ;
225400110426               If due <> ' ';
225500110426                  yy = %lookup(due: $wrkCaa) ;
225600110426                  If yy > 0;
225700110426                     $qtaPpt(xx) += $WrkNra(yy) ;
225800110426                  Endif ;
225900110426               Endif ;
226000110426           Endfor ;
226100110426
226200110426         // telefonate inevase
226300110426           Wrkcaads = Rcacaai ;
226400110426           Wrknrads = Rcanrai ;
226500110426
226600110426           xx = 1 ;
226700110426           For xx by 1 to %elem($cau) ;
226800110426               contacau = $cau(xx);
226900110426               If uno <> ' ';
227000110426                  yy = %lookup(uno: $wrkCaa) ;
227100110426                  If yy > 0;
227200110426                     $qtaIn(xx) += $WrkNra(yy) ;
227300110426                  Endif ;
227400110426               Endif ;
227500110426               If due <> ' ';
227600110426                  yy = %lookup(due: $wrkCaa) ;
227700110426                  If yy > 0;
227800110426                     $qtaIn(xx) += $WrkNra(yy) ;
227900110426                  Endif ;
228000110426               Endif ;
228100110426           Endfor ;
228200110426
228300100526                      T_tepm += rcatepm ;
228400110426                      T_tepg += rcatepg ;
228500110426                      T_tepp += rcatepp ;
228600110426                      T_tecft += rcatecft ;
228700110426                      T_tept += rcatept ;
228800110426                      T_tect += rcatect ;
228900110426                      T_teppt += rcateppt ;
229000110426                      T_tepost += rcatepost ;
229100110426                      T_tepoa   += rcatepo30a;
229200110426                      T_tepoa   += rcatepo33a;
229300110426                      T_tecaldi += rcatecaldi;
229400110426                      T_tecalda += rcatepc31a ;
229500110426                      T_tecalda += rcatepc44a ;
229600110426                      T_teposve += rcateposve ;
229700100526                      T_tepv32a += rcatepv32a ;
229800110426                      T_temante += rcatemante;
229900100526                      T_tepc40a += rcatepc40a;
230000110426                      T_teincre += rcateincre;
230100110426                      T_tepc50a += rcatepc50a;
230200110426                      T_telese  += rcatelese ;
230300110426                      T_inevase += rcainevase;
230400100609                      T_patn    += rcapatn   ;
230500100526                      T_patm    += rcapatm   ;
230600100526                      T_pata    += rcapata   ;
230700100526                      T_patc    += rcapatc   ;
230800110119                      T_pati    += rcapati   ;
230900110119                      T_patr    += rcapatr   ;
231000100622                      T_apptn    += rcaapptn   ;
231100100622                      T_apptm    += rcaapptm   ;
231200100622                      T_appta    += rcaappta   ;
231300100622                      T_apptc    += rcaapptc   ;
231400110119                      T_appti    += rcaappti   ;
231500110119                      T_apptr    += rcaapptr   ;
231600100526                      T_aptn    += rcaaptn   ;
231700100526                      T_aptm    += rcaaptm   ;
231800100526                      T_apta    += rcaapta   ;
231900100526                      T_aptc    += rcaaptc   ;
232000110119                      T_apti    += rcaapti   ;
232100110119                      T_aptr    += rcaaptr   ;
232200100526                      T_aftn    += rcaaftn   ;
232300100526                      T_aftm    += rcaaftm   ;
232400100526                      T_afta    += rcaafta   ;
232500100526                      T_aftc    += rcaaftc   ;
232600110119                      T_afti    += rcaafti   ;
232700110119                      T_aftr    += rcaaftr   ;
232800100526                      T_ap24    += rcaap24   ;
232900100526                      T_feriegg += rcaferiegg;
233000100526                      W_feriehh = Rcaferiehh * 100 ;
233100100526                      W_fra     = Rcaufra * 100 ;
233200100526                      W_frc     = Rcaufrc * 100 ;
233300100526                      W_fuf     = Rcaufuf * 100 ;
233400100527                      wTot_H = %div(w_feriehh : 100);
233500100527                      wTot_R = %rem(w_feriehh : 100);
233600100526                      wTot_M_FP +=(Wtot_H * 60);
233700100527                      wTot_M_FP +=(Wtot_r);
233800100527                      wTot_H = %div(w_fra : 100);
233900100527                      wTot_R = %rem(w_fra : 100);
234000100526                      wTot_M_RA +=(Wtot_H * 60);
234100100527                      wTot_M_RA +=(Wtot_r);
234200100527                      wTot_H = %div(w_frc : 100);
234300100527                      wTot_R = %rem(w_frc : 100);
234400100526                      wTot_M_RC +=(Wtot_H * 60);
234500100527                      wTot_M_RC +=(Wtot_r);
234600100527                      wTot_H = %div(w_fuf : 100);
234700100527                      wTot_R = %rem(w_fuf : 100);
234800100526                      wTot_M_UF +=(Wtot_H * 60);
234900100527                      wTot_M_UF +=(Wtot_r );
235000100629        // totali appuntamenti
235100110119                      T_totapppr=Rcaapptn+Rcaappta+Rcaapptm+Rcaapptc+
235200110119                                 Rcaappti+Rcaapptr+T_totapppr;
235300100629        // totali appuntamenti di cui il primo
235400110119                      T_totapppr1=Rcapatn+Rcapata+Rcapatm+Rcapatc+
235500110119                                  Rcapati+Rcapatr+T_totapppr1  ;
235600100525                     ENDSL;
235700100526                    endif ;
235800100526                 endif ;
235900100706                 exec sql close COG;
236000100526              endif ;
236100100526           ENDFOR;
236200100525
236300110426          // preparo le schiere in stampa
236400110426           Qtapmds = Qtapmwds ;
236500110426           Qtapgds = Qtapgwds ;
236600110426           Qtappds = Qtappwds ;
236700110426           Qtacfds = Qtacfwds ;
236800110426           Qtaptds = Qtaptwds ;
236900110426           Qtactds = QtactWds ;
237000110426           Qtapptds = Qtapptwds ;
237100110426           Qtainds = Qtainwds ;
237200110426
237300100525        // imposto i campi da stampare
237400100526           rcatepm   =T_tepm   ;
237500110426           rcatepg   =T_tepg   ;
237600110428           rcatepp   =T_tepp   ;
237700110426           rcatecft  =T_tecft  ;
237800110426           rcatept   =T_tept   ;
237900110426           rcatect   =T_tect   ;
238000110426           rcateppt  =T_teppt  ;
238100110426           rcatepost =T_tepost ;
238200110426           Tottepoa  =T_tepoa  ;
238300110426           rcatecaldi=T_tecaldi;
238400110426           Tottecalda=T_tecalda;
238500110426           rcateposve=T_teposve;
238600100526           rcatepv32a =T_tepv32a ;
238700110426           rcatemante=T_temante;
238800100526           rcatepc40a =T_tepc40a ;
238900110426           rcateincre=T_teincre;
239000110426           rcatepc50a =T_tepc50a ;
239100110426           rcatelese=T_telese ;
239200110426           rcainevase=T_inevase ;
239300100609           rcapatn    =T_patn    ;
239400100609           rcapatm    =T_patm    ;
239500100609           rcapata    =T_pata    ;
239600100609           rcapatc    =T_patc    ;
239700110119           rcapati    =T_pati    ;
239800110119           rcapatr    =T_patr    ;
239900100622           rcaapptn    =T_apptn    ;
240000100622           rcaapptm    =T_apptm    ;
240100100622           rcaappta    =T_appta    ;
240200100622           rcaapptc    =T_apptc    ;
240300110119           rcaappti    =T_appti    ;
240400110119           rcaapptr    =T_apptr    ;
240500100622           rcaaptn    =T_aptn    ;
240600100622           rcaaptm    =T_aptm    ;
240700100622           rcaapta    =T_apta    ;
240800100622           rcaaptc    =T_aptc    ;
240900110119           rcaapti    =T_apti    ;
241000110119           rcaaptr    =T_aptr    ;
241100100609           rcaaftn    =T_aftn    ;
241200100609           rcaaftm    =T_aftm    ;
241300100609           rcaafta    =T_afta    ;
241400100609           rcaaftc    =T_aftc    ;
241500110119           rcaafti    =T_afti    ;
241600110119           rcaaftr    =T_aftr    ;
241700100609           rcaap24    =T_ap24    ;
241800100609           rcaferiegg=T_feriegg ;
241900100525
242000100525         // ferie
242100100525             wTot_H = %div(wTot_M_FP : 60);
242200100525             wTot_R = %rem(wTot_M_FP : 60);
242300100525             Rcaferiehh = wtot_h;
242400100525             Rcaferiehh += (Wtot_r / 100);
242500100525         // riunioni d'area
242600100525             wTot_H = %div(wTot_M_RA : 60);
242700100525             wTot_R = %rem(wTot_M_RA : 60);
242800100525             Rcaufra    = wtot_h;
242900100525             Rcaufra    += (Wtot_r / 100);
243000100525         // riunioni commerciale
243100100525             wTot_H = %div(wTot_M_RC : 60);
243200100525             wTot_R = %rem(wTot_M_RC : 60);
243300100525             Rcaufrc    = wtot_h;
243400100525             Rcaufrc    += (Wtot_r / 100);
243500100525         // attività ufficio
243600100525             wTot_H = %div(wTot_M_UF : 60);
243700100525             wTot_R = %rem(wTot_M_UF : 60);
243800100525             Rcaufuf    = wtot_h;
243900100525             Rcaufuf    += (Wtot_r / 100);
244000100629        // totali appuntamenti presi
244100100629             Totapppr = T_totapppr ;
244200100629        // totali appuntamenti presi di cui primo
244300100629             Totapppr1 = T_totapppr1 ;
244400100629          // calcolo l'incidenza  degli appuntamenti dulle telefonate effettuate
244500100622            exsr sr_incidenza ;
244600100819        // totali appuntamenti eseguiti
244700110119             Totappes = Rcaaptn + Rcaapta + Rcaaptm + Rcaaptc +
244800110119                        Rcaapti + Rcaaptr ;
244900100819        // totali affiancamenti eseguiti
245000110119             Totaffes = Rcaaftn + Rcaafta + Rcaaftm + Rcaaftc +
245100110119                        Rcaafti + Rcaaftr ;
245200100525
245300100525         //?Imposto data inizio e fine stampa
245400100525           wDate_ISO = %date(I88deci:*iso);
245500100525           wDate_EUR = wDate_ISO;
245600100525           pDatai    = %dec(wDate_EUR);
245700100525           wDate_ISO = %date(I88decf:*iso);
245800100525           wDate_EUR = wDate_ISO;
245900100525           pDataf    = %dec(wDate_EUR);
246000100525         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
246100100525           *in11 = *on ;
246200100525           *in12 = *on ;
246300100527           clear ptatio ;
246400100528         // imposto nel titolo di chi si tratta
246500100609           If (i88cscmm = 'O' and i88cscmmie = ' ') or
246600100610              (i88cscmm = 'I' and i88cscmmie = ' ') OR
246700100610              (i88cscmm = 'I' and i88cscmmie = 'T') ;
246800100609              tipostampa = 'T' ;
246900100528           endif ;
247000100528
247100100528           If (i88cscmm = 'O' and i88cscmmie = 'I') or
247200100528              (i88cscmm = 'I' and i88cscmmie = 'E') ;
247300100609              tipostampa = 'E' ;
247400100528           endif ;
247500100528
247600100528           If (i88cscmm = 'O' and i88cscmmie = 'E') or
247700100528              (i88cscmm = 'I' and i88cscmmie = 'I') ;
247800100609              tipostampa = 'I' ;
247900100528           endif ;
248000100528           *in13 = *on;
248100100525           exsr sr_StpSalto;
248200100525           write mk89rigcon ;
248300100525
248400100525       endif ;
248500100525
248600100526       // verifico se ho lanciato il preventivo
248700100526       If i88tcpe = 'P' or i88tcpe = 'E' ;
248800100526         //?Imposto data inizio e fine stampa
248900100526           wDate_ISO = %date(I88depi:*iso);
249000100526           wDate_EUR = wDate_ISO;
249100100526           pDatai    = %dec(wDate_EUR);
249200100526           wDate_ISO = %date(I88depf:*iso);
249300100526           wDate_EUR = wDate_ISO;
249400100526           pDataf    = %dec(wDate_EUR);
249500100526         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
249600100526           *in11 = *on ;
249700100526           *in12 = *on ;
249800100528           *in13 = *off ;
249900100527            clear ptatio ;
250000100527       //?Ciclo di lettura dei totali preventivi in ordine ragione sociale
250100100527       // elaboro prima i commerciali interni e poi gli esterni se selezionati
250200100527       // i totali di entrambi
250300100527
250400100527         // se inclusi gli interni oppure
250500100527           If (i88tscmm='I' and i88tscmmie='I') or
250600100527         // se omessi gli esterni  oppure
250700100527               (i88tscmm='O' and i88tscmmie='E') or
250800100527         // non ci sono inclusioni e omissioni
250900100609               (i88tscmm='O' and i88tscmmie=' ') or
251000100610               (i88tscmm='I' and i88tscmmie=' ') OR
251100100610               (i88tscmm='I' and i88tscmmie='T') ;
251200100527
251300100527         // pulisco alcuni campoi di totale
251400100527               clear Wtot_m_fp ;
251500100527               clear Wtot_m_uf ;
251600100527               tipostampa = '2' ;
251700100527               *in99 = *on ;
251800100527               $End = *off ;
251900100526               exec sql
252000100527                 declare PRGI cursor for
252100100526                 select *
252200101007                 from WFRPA00f where RPAfun = 'COMIN' or RPAfun = 'ASC'
252300101007                                  or RPAfun = 'SA' order by RPAdrg ;
252400100526
252500100526                 exec sql
252600100527                 open PRGI;
252700100527
252800100527                 IF sqlcode >= 0;
252900100527
253000100527                 Dow not $End ;
253100100526
253200100526                    exec sql
253300100527                    fetch next from PRGI into :WFRPA00F ;
253400100526                    IF sqlcod = 100 or sqlcod < 0;
253500100527                       $End = *on;
253600100527                       leave;
253700100527                    Endif;
253800100526
253900100526        // calcolo totali appuntamenti
254000110119                      totapp = (rpaappm + rpaappn + rpaappa + rpaappc +
254100110119                                rpaappi + rpaappr);
254200100526        // calcolo totali telefonate
254300100526                      tottel=(rpatc19 + rpatc20 + rpatc21 + rpatc22 + rpatc24 +
254400100526                            rpatc25 + rpatc26 + rpatc27 + rpatc28 + rpatc30 +
254500110426                            rpatc31 + rpatc32 + rpatc33 + rpatc44 + rpatc60 +
254600110426                            rpatc70 + rpatc71 + rpatc73 + rpatc74 + rpatc75);
254700100526        // Verifico se affiancamenti valorizzati
254800110119                      *in30 = rpaaffn > 0 ;
254900110119                      *in31 = rpaaffi > 0 ;
255000110119                      *in32 = rpaaffa > 0 ;
255100110119                      *in33 = rpaaffm > 0 ;
255200110119                      *in34 = rpaaffc > 0 ;
255300110119                      *in35 = rpaaffr > 0 ;
255400100526         //?Salto pagina
255500100526                      IF  *in99 = *on   ;
255600100526                          exsr sr_StpSalto;
255700100526                          write Mk89rigpre ;
255800100526                          *in99 = *off;
255900100526                      ENDIF;
256000100526                      write Mk89rigpr1 ;
256100100526                      T_totapp +=totapp ;
256200100526                      T_tottel +=tottel ;
256300100526                      T_appm += rpaappm ;
256400100526                      T_appn += rpaappn ;
256500100526                      T_appa += rpaappa ;
256600100526                      T_appc += rpaappc ;
256700110119                      T_appi += rpaappi ;
256800110119                      T_appr += rpaappr ;
256900100526                      T_affm += rpaaffm ;
257000100526                      T_affn += rpaaffn ;
257100100526                      T_affa += rpaaffa ;
257200100526                      T_affc += rpaaffc ;
257300110119                      T_affi += rpaaffi ;
257400110119                      T_affr += rpaaffr ;
257500100526                      T_atc19 += rpatc19 ;
257600100526                      T_atc20 += rpatc20 ;
257700100526                      T_atc21 += rpatc21 ;
257800100526                      T_atc22 += rpatc22 ;
257900100526                      T_atc24 += rpatc24 ;
258000100526                      T_atc25 += rpatc25 ;
258100100526                      T_atc26 += rpatc26 ;
258200100526                      T_atc27 += rpatc27 ;
258300100526                      T_atc28 += rpatc28 ;
258400100526                      T_atc30 += rpatc30 ;
258500100526                      T_atc31 += rpatc31 ;
258600100526                      T_atc32 += rpatc32 ;
258700100526                      T_atc33 += rpatc33 ;
258800110117                      T_atc44 += rpatc44 ;
258900110426                      T_atc70 += rpatc70 ;
259000110426                      T_atc71 += rpatc71 ;
259100110426                      T_atc73 += rpatc73 ;
259200110426                      T_atc74 += rpatc74 ;
259300110426                      T_atc75 += rpatc75 ;
259400100526                      T_off   += rpaoff   ;
259500100526                      T_fegg  += rpafegg  ;
259600100526                      W_feriehh = Rpaferie * 100 ;
259700100526                      W_fuf     = Rpauff  * 100 ;
259800100527                      wTot_H = %div(w_feriehh : 100);
259900100527                      wTot_R = %rem(w_feriehh : 100);
260000100526                      wTot_M_FP +=(Wtot_H * 60);
260100100527                      wTot_M_FP +=(Wtot_r);
260200100527                      wTot_H = %div(w_fuf : 100);
260300100527                      wTot_R = %rem(w_fuf : 100);
260400100526                      wTot_M_UF +=(Wtot_H * 60);
260500100527                      wTot_M_UF +=(Wtot_r);
260600100527                   ENDDO ;
260700100527
260800100527                   exec sql close PRGI;
260900100526
261000100527                   totapp = T_totapp ;
261100100527                   Tottel = T_tottel ;
261200100527                   rpaappm  = T_appm ;
261300100527                   rpaappn  = T_appn ;
261400100527                   rpaappa  = T_appa ;
261500100527                   rpaappc  = T_appc ;
261600110119                   rpaappi  = T_appi ;
261700110119                   rpaappr  = T_appr ;
261800100527                   rpaaffm  = T_affm ;
261900100527                   rpaaffn  = T_affn ;
262000100527                   rpaaffa  = T_affa ;
262100100527                   rpaaffc  = T_affc ;
262200110119                   rpaaffi  = T_affi ;
262300110119                   rpaaffr  = T_affr ;
262400100527                   rpatc19= T_atc19 ;
262500100527                   rpatc20= T_atc20 ;
262600100527                   rpatc21= T_atc21 ;
262700100527                   rpatc22 = T_atc22 ;
262800100527                   rpatc24 = T_atc24 ;
262900100527                   rpatc25 = T_atc25 ;
263000100527                   rpatc26 = T_atc26 ;
263100100527                   rpatc27 = T_atc27 ;
263200100527                   rpatc28 = T_atc28 ;
263300100527                   rpatc30 = T_atc30 ;
263400100527                   rpatc31 = T_atc31 ;
263500100527                   rpatc32 = T_atc32 ;
263600100527                   rpatc33 = T_atc33 ;
263700110117                   rpatc44 = T_atc44 ;
263800100527                   rpatc60 = T_atc60 ;
263900110426                   rpatc70 = T_atc70 ;
264000110426                   rpatc71 = T_atc71 ;
264100110426                   rpatc73 = T_atc73 ;
264200110426                   rpatc74 = T_atc74 ;
264300110426                   rpatc75 = T_atc75 ;
264400100527                   rpaoff   = T_off   ;
264500100527                   rpafegg  = T_fegg  ;
264600100527               // ferie
264700100527                   wTot_H = %div(wTot_M_FP : 60);
264800100527                   wTot_R = %rem(wTot_M_FP : 60);
264900100527                   Rpaferie = wtot_h;
265000100527                   Rpaferie += (Wtot_r / 100);
265100100527               // attività ufficio
265200100527                   wTot_H = %div(wTot_M_UF : 60);
265300100527                   wTot_R = %rem(wTot_M_UF : 60);
265400100527                   Rpauff     = wtot_h;
265500100527                   Rpauff     += (Wtot_r / 100);
265600100527
265700100527              // Verifico se affiancamenti valorizzati
265800110119                   *in30 = rpaaffn > 0 ;
265900110119                   *in31 = rpaaffi > 0 ;
266000110119                   *in32 = rpaaffa > 0 ;
266100110119                   *in33 = rpaaffm > 0 ;
266200110119                   *in34 = rpaaffc > 0 ;
266300110119                   *in35 = rpaaffr > 0 ;
266400100526         //?Salto pagina
266500100527                   IF  *in99 = *on   ;
266600100527                       exsr sr_StpSalto;
266700100527                       write mk89rigpre ;
266800100527                       *in99 = *off;
266900100527                   ENDIF;
267000100527                   rpadrg = ' TOTALE GENERALE   ' ;
267100100527                   write mk89rigpr1 ;
267200100527                 endif ;
267300100527         endif ;
267400100526
267500100527         // se inclusi gli esterni oppure
267600100527           If (i88tscmm='I' and i88tscmmie='E') or
267700100527         // se omessi gli interni
267800100527               (i88tscmm='O' and i88tscmmie='I') or
267900100527         // non ci sono inclusioni e omissioni
268000100609               (i88tscmm='O' and i88tscmmie=' ') or
268100100610               (i88tscmm='I' and i88tscmmie=' ') OR
268200100610               (i88tscmm='I' and i88tscmmie='T') ;
268300100527
268400100527         // pulisco alcuni campoi di totale
268500100527               clear Wtot_m_fp ;
268600100527               clear Wtot_m_uf ;
268700100527               clear  T_totapp  ;
268800100527               clear  T_tottel  ;
268900100527               clear  T_appm  ;
269000100527               clear  T_appn  ;
269100100527               clear  T_appa  ;
269200100527               clear  T_appc  ;
269300110119               clear  T_appi  ;
269400110119               clear  T_appr  ;
269500100527               clear  T_affm ;
269600100527               clear  T_affn  ;
269700100527               clear  T_affa  ;
269800100527               clear  T_affc  ;
269900110119               clear  T_affi  ;
270000110119               clear  T_affr  ;
270100100527               clear  T_atc19 ;
270200100527               clear  T_atc20  ;
270300100527               clear  T_atc21  ;
270400100527               clear  T_atc22  ;
270500100527               clear  T_atc24  ;
270600100527               clear  T_atc25  ;
270700100527               clear  T_atc26  ;
270800100527               clear  T_atc27  ;
270900100527               clear  T_atc28  ;
271000100527               clear  T_atc30  ;
271100100527               clear  T_atc31  ;
271200100527               clear  T_atc32  ;
271300100527               clear  T_atc33  ;
271400110117               clear  T_atc44  ;
271500100527               clear  T_atc60  ;
271600110426               clear  T_atc70  ;
271700110426               clear  T_atc71  ;
271800110426               clear  T_atc73  ;
271900110426               clear  T_atc74  ;
272000110426               clear  T_atc75  ;
272100100527               clear  T_off    ;
272200100527               clear  T_fegg   ;
272300100527               tipostampa = '3' ;
272400100527               *in99 = *on ;
272500100527               $End = *off ;
272600100527               exec sql
272700100527                 declare PRGE cursor for
272800100527                 select *
272900101007                 from WFRPA00f where RPAfun <> 'COMIN' and RPAfun <> 'ASC'
273000101007                                 and RPAfun <> 'SA' order by RPAdrg ;
273100100527
273200100527                 exec sql
273300100527                 open PRGE;
273400100527                 IF sqlcode >= 0;
273500100527
273600100527                 Dow not $End ;
273700100527
273800100527                    exec sql
273900100527                    fetch next from PRGE into :WFRPA00F ;
274000100527                    IF sqlcod = 100 or sqlcod < 0;
274100100527                       $End = *on;
274200100527                       leave;
274300100527                    Endif;
274400100527
274500100527        // calcolo totali appuntamenti
274600110119                      totapp = (rpaappm + rpaappn + rpaappa + rpaappc +
274700110119                                rpaappi + rpaappr );
274800100527        // calcolo totali telefonate
274900100527                      tottel=(rpatc19 + rpatc20 + rpatc21 + rpatc22 + rpatc24 +
275000100527                            rpatc25 + rpatc26 + rpatc27 + rpatc28 + rpatc30 +
275100110426                            rpatc31 + rpatc32 + rpatc33 + rpatc44 + rpatc60 +
275200110426                            rpatc70 + rpatc71 + rpatc73 + rpatc74 + rpatc75);
275300100527        // Verifico se affiancamenti valorizzati
275400110119                      *in30 = rpaaffn > 0 ;
275500110119                      *in31 = rpaaffi > 0 ;
275600110119                      *in32 = rpaaffa > 0 ;
275700110119                      *in33 = rpaaffm > 0 ;
275800110119                      *in34 = rpaaffc > 0 ;
275900110119                      *in35 = rpaaffr > 0 ;
276000100527         //?Salto pagina
276100100527                      IF  *in99 = *on   ;
276200100527                          exsr sr_StpSalto;
276300100527                          write Mk89rigpre ;
276400100527                          *in99 = *off;
276500100527                      ENDIF;
276600100527                      write Mk89rigpr1 ;
276700100527                      T_totapp +=totapp ;
276800100527                      T_tottel +=tottel ;
276900100527                      T_appm += rpaappm ;
277000100527                      T_appn += rpaappn ;
277100100527                      T_appa += rpaappa ;
277200100527                      T_appc += rpaappc ;
277300110119                      T_appi += rpaappi ;
277400110119                      T_appr += rpaappr ;
277500100527                      T_affm += rpaaffm ;
277600100527                      T_affn += rpaaffn ;
277700100527                      T_affa += rpaaffa ;
277800100527                      T_affc += rpaaffc ;
277900110119                      T_affi += rpaaffi ;
278000110119                      T_affr += rpaaffr ;
278100100527                      T_atc19 += rpatc19 ;
278200100527                      T_atc20 += rpatc20 ;
278300100527                      T_atc21 += rpatc21 ;
278400100527                      T_atc22 += rpatc22 ;
278500100527                      T_atc24 += rpatc24 ;
278600100527                      T_atc25 += rpatc25 ;
278700100527                      T_atc26 += rpatc26 ;
278800100527                      T_atc27 += rpatc27 ;
278900100527                      T_atc28 += rpatc28 ;
279000100527                      T_atc30 += rpatc30 ;
279100100527                      T_atc31 += rpatc31 ;
279200100527                      T_atc32 += rpatc32 ;
279300100527                      T_atc33 += rpatc33 ;
279400110117                      T_atc44 += rpatc44 ;
279500100527                      T_atc60 += rpatc60 ;
279600110426                      T_atc70 += rpatc70 ;
279700110426                      T_atc71 += rpatc71 ;
279800110426                      T_atc73 += rpatc73 ;
279900110426                      T_atc74 += rpatc74 ;
280000110426                      T_atc75 += rpatc75 ;
280100100527                      T_off   += rpaoff   ;
280200100527                      T_fegg  += rpafegg  ;
280300100527                      W_feriehh = Rpaferie * 100 ;
280400100527                      W_fuf     = Rpauff  * 100 ;
280500100527                      wTot_H = %div(w_feriehh : 100);
280600100527                      wTot_R = %rem(w_feriehh : 100);
280700100527                      wTot_M_FP +=(Wtot_H * 60);
280800100527                      wTot_M_FP +=(Wtot_r);
280900100527                      wTot_H = %div(w_fuf : 100);
281000100527                      wTot_R = %rem(w_fuf : 100);
281100100527                      wTot_M_UF +=(Wtot_H * 60);
281200100527                      wTot_M_UF +=(Wtot_r);
281300100527                   ENDDO ;
281400100527
281500100527                     exec sql close PRGE;
281600100527
281700100527                   totapp = T_totapp ;
281800100527                   Tottel = T_tottel ;
281900100527                   rpaappm  = T_appm ;
282000100527                   rpaappn  = T_appn ;
282100100527                   rpaappa  = T_appa ;
282200100527                   rpaappc  = T_appc ;
282300110119                   rpaappi  = T_appi ;
282400110119                   rpaappr  = T_appr ;
282500100527                   rpaaffm  = T_affm ;
282600100527                   rpaaffn  = T_affn ;
282700100527                   rpaaffa  = T_affa ;
282800100527                   rpaaffc  = T_affc ;
282900110119                   rpaaffi  = T_affi ;
283000110119                   rpaaffr  = T_affr ;
283100100527                   rpatc19= T_atc19 ;
283200100527                   rpatc20= T_atc20 ;
283300100527                   rpatc21= T_atc21 ;
283400100527                   rpatc22 = T_atc22 ;
283500100527                   rpatc24 = T_atc24 ;
283600100527                   rpatc25 = T_atc25 ;
283700100527                   rpatc26 = T_atc26 ;
283800100527                   rpatc27 = T_atc27 ;
283900100527                   rpatc28 = T_atc28 ;
284000100527                   rpatc30 = T_atc30 ;
284100100527                   rpatc31 = T_atc31 ;
284200100527                   rpatc32 = T_atc32 ;
284300100527                   rpatc33 = T_atc33 ;
284400110117                   rpatc44 = T_atc44 ;
284500100527                   rpatc60 = T_atc60 ;
284600110426                   rpatc70 = T_atc70 ;
284700110426                   rpatc71 = T_atc71 ;
284800110426                   rpatc73 = T_atc73 ;
284900110426                   rpatc74 = T_atc74 ;
285000110426                   rpatc75 = T_atc75 ;
285100100527                   rpaoff   = T_off   ;
285200100527                   rpafegg  = T_fegg  ;
285300100527               // ferie
285400100527                   wTot_H = %div(wTot_M_FP : 60);
285500100527                   wTot_R = %rem(wTot_M_FP : 60);
285600100527                   Rpaferie = wtot_h;
285700100527                   Rpaferie += (Wtot_r / 100);
285800100527               // attività ufficio
285900100527                   wTot_H = %div(wTot_M_UF : 60);
286000100527                   wTot_R = %rem(wTot_M_UF : 60);
286100100527                   Rpauff     = wtot_h;
286200100527                   Rpauff     += (Wtot_r / 100);
286300100527
286400100527              // Verifico se affiancamenti valorizzati
286500110119                   *in30 = rpaaffn > 0 ;
286600110119                   *in31 = rpaaffi > 0 ;
286700110119                   *in32 = rpaaffa > 0 ;
286800110119                   *in33 = rpaaffm > 0 ;
286900110119                   *in34 = rpaaffc > 0 ;
287000110119                   *in35 = rpaaffr > 0 ;
287100100527         //?Salto pagina
287200100527                   IF  *in99 = *on   ;
287300100527                       exsr sr_StpSalto;
287400100527                       write mk89rigpre ;
287500100527                       *in99 = *off;
287600100527                   ENDIF;
287700100527                   rpadrg = ' TOTALE GENERALE   ' ;
287800100527                   write mk89rigpr1 ;
287900100527                 endif ;
288000100527         endif ;
288100100526       endif ;
288200100525       ENDSR;
288300100525
288400080206       //--------------------------------------------------------------
288500080206       //?Reperimento Dati del job (Utente/Operativi).
288600080206       //--------------------------------------------------------------
288700080206       BEGSR DatiJob;
288800080206
288900080206         in(E) §AzUte;
289000080206         if NOT %error;
289100080206           in(E) §DatiUte;
289200080206         endif;
289300080206         if %error or RSut = *blanks;
289400080206           clear TIBS34ds;
289500080206           tibs34r(tibs34ds);
289600080206           in §AzUte;
289700080206           in §DatiUte;
289800080206         endif;
289900100429
290000100429         //?Richiamo pgm di controllo autorizzazioni utente così mi torna
290100100429         //?il codice autorizzazione relativo alle tariffe clienti
290200100429         //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
290300100429         reset TNTAA1DS;
290400100429         ITAA1caut = '§utegtc';
290500100429         kpjbu     = TNTAA1DS;
290600100429         tntaa1c (kpjba);
290700100429         TNTAA1DS = kpjbu;
290800100429
290900100429         //?Se utente abilitato come 'AZ' mi carico la sk delle filiali di area
291000100429         IF  OTAA1cabi = 'AZ' and DUTpou <> 046;
291100100429           OTAA1cabi = 'RA';
291200100429         ENDIF;
291300100429
291400100429         //?Carico filiali abilitate all'utente
291500100429         //?Non controllo se caricate o no le filiali
291600100429         //?tanto se non ho caricato nessuna filiale e la richiesta
291700100429         //?è per tutti i commerciali, non stampo niente
291800100429         clear TRUL31DS;
291900110120         clear TRUL31DS2;
292000110120         *in09 = *off;
292100100429         I31abi = OTAA1cabi;
292200100429         I31cdi = DUTdis;
292300100429         I31car = DUTare;
292400100429         I31cpo = DUTpou;
292500110120         trul31r (kpjba : trul31ds : trul31ds2);
292600110120         IF  car(2) > *zeros;
292700110120           *in09 = *on;
292800110120         ENDIF;
292900080206
293000080206       ENDSR;
293100100305
293200100305       //--------------------------------------------------------------
293300100305       //?Carico Sk Agenti.
293400100305       //--------------------------------------------------------------
293500100305       BEGSR sr_CarAGE;
293600100305
293700100305         clear xx;
293800100305         clear $Age;
293900100429         $EndAge = *off;
294000100305
294100100305         exec sql
294200100305          declare AGE cursor for
294300130917          select CMMcod, CMMuni
294400130917          from AZCMM00F
294500130917          order by CMMcod;
294600100305
294700100305         exec sql
294800100305           open AGE;
294900100305           IF sqlcode < 0;
295000100305             $EndAge = *on;
295100100305           ENDIF;
295200100305
295300100305         DOW not $EndAge;
295400100305           exec sql
295500130917             fetch next from AGE into :CMMcod, :CMMuni;
295600100305             IF sqlcod = 100 or sqlcod < 0;
295700100305               $EndAge = *on;
295800100305               leave;
295900100305             ENDIF;
296000130917             IF  CMMuni = $AgeU(zz);
296100100305               xx += 1;
296200130917               $Age(xx) = CMMcod;
296300100305             ENDIF;
296400100305         ENDDO;
296500100305
296600100305         exec sql close AGE;
296700100305
296800100305         //?Se non ho cricato niente memorizzo almeno il codice richiesto
296900100305         IF $Age(01) = 0;
297000100429           $Age(01) = $AgeU(zz);
297100100305         ENDIF;
297200100305
297300100305       ENDSR;
297400100429
297500100429       //--------------------------------------------------------------
297600100429       //?Carico sk agenti unificanti.
297700100429       //--------------------------------------------------------------
297800100429       BEGSR sr_CarAgeUni;
297900100429
298000100429         clear xx;
298100100429         clear $AGEU;
298200100429         $EndAge = *off;
298300100429
298400100429           //?Imposto la stringa per SQL
298500130917           wSQL = 'select CMMcod +
298600130917                   from AZCMM00F +
298700130917                   where substr(digits(CMMuni), 1, 3) in (';
298800100429
298900100429           yy = 0;
299000100429           xx = 1;
299100100429           FOR xx by 1 to %elem(pog);
299200100429             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
299300100429               IF yy > 0;
299400100429                 wSQL += ', ';
299500100429               ELSE;
299600100429                 yy = 1;
299700100429               ENDIF;
299800100429               wSQL += '''';
299900100429               wSQL += pog(xx);
300000100429               wSQL += '''';
300100100429             ENDIF;
300200100429           ENDFOR;
300300100505
300400100505         //?Carico TUTTI gli agenti unificanti
300500130917           wSQL += ') and CMMcod = CMMuni +
300600130917                      and CMMpar = '' ''';
300700100429
300800130917         wSQL += ' order by CMMcod +
300900100505                   for fetch only';
301000100429
301100100429         //?Dichiarazione cursore
301200100429         exec sql
301300100429           prepare A1   from :wSQL;
301400100429         exec sql
301500100429           declare AGEU cursor for A1;
301600100429
301700100429         //?Apertura del cursore
301800100429         exec sql
301900100429           open AGEU;
302000100429
302100100429         IF sqlcode < 0;
302200100429           $EndAge = *on;
302300100429         ENDIF;
302400100429
302500100429         yy = 0;
302600100429         DOW not $EndAge;
302700100429           exec sql
302800130917             fetch next from AGEU into :CMMcod;
302900100429           IF sqlcod = 100 or sqlcod < 0;
303000100429             $EndAge = *on;
303100100429             leave;
303200100429           ENDIF;
303300100429
303400130917           yy += 1;
303500130917           $AGEU(yy) = CMMcod;
303600100429
303700100429         ENDDO;
303800100429
303900100429         exec sql
304000100429           close AGEU;
304100100429
304200100429       ENDSR;
304300100305
304400100305       //--------------------------------------------------------------
304500100305       //?Preparazione stringa SQL.
304600100305       //--------------------------------------------------------------
304700100305       BEGSR sr_PrepSQL;
304800100305
304900100308         wSQL = 'select atctat, atccpo, atcksc, +
305000100309                 atcatn, atcatnp, atcdad, atchda, atcnrv, +
305100100628                 atcoii, atcofi, atccad, atcco3, atcdco, +
305200100525                 atccac, atcest +
305300100305                  from TIATC00F +
305400100305                  where atccmm in(';
305500100305
305600100305         yy = 0;
305700100305         xx = 1;
305800100305         FOR xx by 1 to %elem($Age);
305900100305           IF $Age(xx) <> *zeros;
306000100305             IF yy > 0;
306100100305               wSQL += ', ';
306200100305             ELSE;
306300100305               yy = 1;
306400100305             ENDIF;
306500100305             wSQL += %editc($Age(xx):'X');
306600100305           ENDIF;
306700100305         ENDFOR;
306800100305
306900100308         wSQL += ') and atcdad between ' + %editc(IMK86datai:'X') +
307000100308                 ' and ' + %editc(IMK86dataf:'X');
307100100305
307200100305         wSQL += ' order by atcdad, atchda, atctat +
307300100305                   for fetch only';
307400100305
307500100305       ENDSR;
307600100305
307700100305       //--------------------------------------------------------------
307800100305       //?Controllo se il rcd che sto leggendo è OK per l'agenda.
307900100305       //--------------------------------------------------------------
308000100305       BEGSR Sr_CtrRcdOk;
308100100305
308200100305         $RcdOk = *on;
308300100513         // se c'è inclusione o omissione di alcuni tipi attività
308400100513         // controllo
308500100513         SELECT ;
308600100513            When imk86iot = 'I' and %lookup(atctat : $tat) = 0 ;
308700100513                 $RcdOk = *off;
308800100513                 leavesr ;
308900100513            When imk86iot = 'O' and %lookup(atctat : $tat) > 0 ;
309000100513                 $RcdOk = *off;
309100100513                 leavesr ;
309200100513         ENDSL ;
309300100305         //?se attività legata a trattativa
309400100305         //?la trattativa NON deve essere fittizia
309500100305         IF  atcnrv > 0;
309600100305           exec sql
309700100305             select visffz into :visffz from tivis05l
309800100305             where visnrv = : atcnrv;
309900100305         //?non trovo il rcd corrispondente
310000100305           IF  sqlcod <> 0;
310100100305             leavesr;
310200100305           ENDIF;
310300100305         //?se trattativa fittizia non carico a video
310400100305           IF  visffz = 'S';
310500100305             $RcdOk = *off;
310600100305           ENDIF;
310700100305         ENDIF;
310800100525
310900100525         //?CHIODO
311000100712         //?MODIFICA CONTROLLO 06 Luglio 2010 ora controllo solo esito senza causale
311100100712         //?Se appuntamento '91' o affiancamento '92'  e esito 'N'
311200100712         IF  (ATCcad = '91' or ATCcad = '92') and ATCest = 'N';
311300100712           $RcdOk = *off;
311400100712         ENDIF;
311500100525         //?Se attvità qualsiasi chiusa con attività 'AA'
311600100525         //?non devo caricare in agenda
311700100525         IF  ATCcac = 'AA';
311800100525           $RcdOk = *off;
311900100525         ENDIF;
312000100305
312100100305       ENDSR;
312200100309
312300100309       //--------------------------------------------------------------
312400100309       //?Decodifico agente.
312500100309       //--------------------------------------------------------------
312600100309       BEGSR sr_DecCmm;
312700100309
312800130917         chain  (wCmm)  AZCMM000;
312900130917         IF  NOT  %found( AZCMM01L );
313000130917           clear CMMdes;
313100130917           clear CMMfun;
313200100309         ENDIF;
313300100309
313400100309       ENDSR;
313500090807
313600080206       //--------------------------------------------------------------
313700080206       //?Operazioni finali.
313800080206       //--------------------------------------------------------------
313900080206       BEGSR RoutEnd;
314000090521
314100080206         *inLR = *on;
314200080206         return;
314300080206
314400080206       ENDSR;
314500080206
314600080206      /end-free
314700110422** CAUSALI IN STAMPA CONSUNTIVO
31480011042214/15
31490011042219/20
315000110422 21
315100110422 22
315200110422 24
315300110422 25
315400110422 26
315500110422 27
315600110422 28
31570011042230/33
315800110422 32
31590011051831/44
316000110422 41
316100110422 42
316200110422 60
31630011042270/75
316400110422 71
316500110518 72
31660011042273/74
316700110422 80
316800110422 81
316900110422 91
317000110422
317100110531
317200110422** CAUSALI CORRISPONDENTI AD OGNI BUCO DI STAMPA
3173001104221415
3174001104221920
31750011042221
31760011042222
31770011042224
31780011042225
31790011042226
31800011042227
31810011042228
3182001104223033
31830011042232
3184001105183144
31850011042241
31860011042242
31870011042260
3188001104227075
31890011042271
31900011051872
3191001104227374
31920011042280
31930011042281
31940011042291
319500110422
319600110531
