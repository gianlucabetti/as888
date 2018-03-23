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
002500140715     fTNCPO11L  if   e           k disk
002600100528     fTNTBE01L  if   e           k disk
002700100521     fTRMK89P   o    e             printer oflind(*in99)
002800100310     f                                     infds(prtinfo)
002900100310
003000080206      //---------------------------------------------------------------
003100090406      //?Definizione costanti.
003200080206      //---------------------------------------------------------------
003300080206
003400080206      //---------------------------------------------------------------
003500080206      //?Definizione schiere.
003600080206      //---------------------------------------------------------------
003700090806
003800100304      // - Agenti
003900100429     d $Age            s              7  0 dim(3500)
004000100429
004100100429      // - Agenti Unificanti
004200100429     d $AgeU           s              7  0 dim(3500)
004300100513
004400100513     d §MK86TATds      ds            10    inz
004500100513     d   $TAT                  1     10    inz  dim(10)
004600080206
004700110422      // causali da stampare
004800110422     d $STP            s              5    dim(24) ctdata perrcd(1)
004900110422
005000110422      // causali da sommare in stampa
005100110422     d $cau            s              4    dim(24) ctdata perrcd(1)
005200130917
005300080206      //---------------------------------------------------------------
005400080206      //?Definizione aree dati.
005500080206      //---------------------------------------------------------------
005600080206
005700080206      // - Dati utente
005800080206     d §AzUte        e ds                  extname(AZUTE00F)
005900080206     d                                     dtaara
006000080206     d §DatiUte      e ds                  extname(dDatiUte)
006100080206     d                                     dtaara
006200080206
006300080206      //---------------------------------------------------------------
006400080206      //?Definizione strutture dati.
006500080206      //---------------------------------------------------------------
006600100310
006700100310     d PrtINFO         ds
006800100310     d  CurLine              367    368i 0
006900080206
007000080206      // - Status
007100080206     d Psds           sds
007200080206     d   SDSpgm          *proc
007300080206
007400080206      // - Parametri ricevuti
007500080206     d KPJBA         e ds
007600100521     d TRMK88DS      e ds
007700100521     d TRMK86DS      e ds
007800080206
007900100308      // - Ricerca/Controllo tabelle
008000100308     d TIBS02ds      e ds                  inz
008100100308
008200080206      // - Reperimento dati utente
008300100305     d TIBS34DS      e ds
008400100308
008500100308       // - Reperimento dati anagrafici
008600100308     d TIBS69ds      e ds
008700100308     d DS_cnaco      e ds                  inz extname(CNACO00F)
008800100308     d DS_cnind      e ds                  inz extname(CNIND00F)
008900100308     d DS_cnclp      e ds                  inz extname(CNCLP00F)
009000100308     d DS_fncls      e ds                  inz extname(FNCLS00F)
009100100429
009200100429      // - controllo abilitazioni
009300100429     d TNTAA1DS      e ds                  inz
009400100429
009500100429      // - Carico Filiali abilitate all'utente
009600100429     d TRUL31DS      e ds
009700100429     d POG                    10    759    DIM(250)
009800110120      // - Carico Aree abilitate all'utente
009900110120     d TRUL31DS2     e ds
010000110120     d CAR                    22    171    DIM(50)
010100100308
010200100308      // - Tabella CCO = Causale contatto
010300100308     d dCCO          e ds                  inz
010400100712
010500100712      // - Tabella IFS = Sottotipi INFO commerciali
010600100712     d dIFS          e ds                  inz
010700100308
010800100308      // - Tabella TTA = Tipo attività
010900100308     d dTTA          e ds                  inz
011000100308
011100110120      // - Tabella 05 = Codici Area
011200110120     d ds05          e ds                  inz
011300110120
011400100308      // - Tabella 1L = Categoria Merceologica
011500100308     d ds1L          e ds                  inz
011600090807
011700090807      // File attività
011800100305     d TIATC00F      e ds                  extname(TIATC00F)
011900100305
012000130917      *// File trattativa
012100130917     d*// TIVIS00F      e ds                  extname(TIVIS00F)
012200090629
012300100521      // File statistica consuntiva
012400100521     d WFRCA00F      e ds                  extname(WFRCA00F)
012500100521
012600100521      // File statistica preventiva
012700100521     d WFRPA00F      e ds                  extname(WFRPA00F)
012800100522
012900110422
013000110422     d Causalids       ds
013100110422     d  Causa01                       5
013200110422     d  Causa02                       5
013300110422     d  Causa03                       5
013400110422     d  Causa04                       5
013500110422     d  Causa05                       5
013600110422     d  Causa06                       5
013700110422     d  Causa07                       5
013800110422     d  Causa08                       5
013900110422     d  Causa09                       5
014000110422     d  Causa10                       5
014100110422     d  Causa11                       5
014200110422     d  Causa12                       5
014300110422     d  Causa13                       5
014400110422     d  Causa14                       5
014500110422     d  Causa15                       5
014600110422     d  Causa16                       5
014700110422     d  Causa17                       5
014800110422     d  Causa18                       5
014900110422     d  Causa19                       5
015000110422     d  Causa20                       5
015100110422     d  Causa21                       5
015200110422     d  Causa22                       5
015300110422     d  Causa23                       5
015400110422     d  Causa24                       5
015500110426
015600110426     d Cauwds          ds           120    inz
015700110426     d   $CAAstp                      5    inz  dim(24)
015800110422     d
015900110422     d Cauineds        ds
016000110422     d  Cauine01                      5
016100110422     d  Cauine02                      5
016200110422     d  Cauine03                      5
016300110422     d  Cauine04                      5
016400110422     d  Cauine05                      5
016500110422     d  Cauine06                      5
016600110422     d  Cauine07                      5
016700110422     d  Cauine08                      5
016800110422     d  Cauine09                      5
016900110422     d  Cauine10                      5
017000110422     d  Cauine11                      5
017100110422     d  Cauine12                      5
017200110422     d  Cauine13                      5
017300110422     d  Cauine14                      5
017400110422     d  Cauine15                      5
017500110422     d  Cauine16                      5
017600110422     d  Cauine17                      5
017700110422     d  Cauine18                      5
017800110422     d  Cauine19                      5
017900110422     d  Cauine20                      5
018000110422     d  Cauine21                      5
018100110422     d  Cauine22                      5
018200110422     d  Cauine23                      5
018300110422     d  Cauine24                      5
018400110422     d
018500110422     d ContaCau        ds
018600110422     d  uno                           2
018700110422     d  due                           2
018800110422     d
018900110422     d RCACaapmds      ds            70    inz
019000110422     d   $CAAPM                       2    inz  dim(35)
019100110422
019200110422     d RCANrapmds      ds                  inz
019300110422     d   $NRAPM                       5s 0 inz  dim(35)
019400110422
019500110422     d Qtapmds         ds
019600110422     d  Qtapm01                       5  0 inz
019700110422     d  Qtapm02                       5  0 inz
019800110422     d  Qtapm03                       5  0 inz
019900110422     d  Qtapm04                       5  0 inz
020000110422     d  Qtapm05                       5  0 inz
020100110422     d  Qtapm06                       5  0 inz
020200110422     d  Qtapm07                       5  0 inz
020300110422     d  Qtapm08                       5  0 inz
020400110422     d  Qtapm09                       5  0 inz
020500110422     d  Qtapm10                       5  0 inz
020600110422     d  Qtapm11                       5  0 inz
020700110422     d  Qtapm12                       5  0 inz
020800110422     d  Qtapm13                       5  0 inz
020900110422     d  Qtapm14                       5  0 inz
021000110422     d  Qtapm15                       5  0 inz
021100110422     d  Qtapm16                       5  0 inz
021200110422     d  Qtapm17                       5  0 inz
021300110422     d  Qtapm18                       5  0 inz
021400110422     d  Qtapm19                       5  0 inz
021500110422     d  Qtapm20                       5  0 inz
021600110422     d  Qtapm21                       5  0 inz
021700110422     d  Qtapm22                       5  0 inz
021800110422     d  Qtapm23                       5  0 inz
021900110422     d  Qtapm24                       5  0 inz
022000110422
022100110422     d Qtapmwds        ds
022200110422     d   $QtaPM                       5s 0 inz  dim(24)
022300110422     d
022400110422     d RCACaapgds      ds            70    inz
022500110422     d   $CAAPG                       2    inz  dim(35)
022600110422
022700110422     d RCANrapgds      ds                  inz
022800110422     d   $NRAPG                       5s 0 inz  dim(35)
022900110422
023000110422     d Qtapgds         ds
023100110422     d  Qtapg01                       5  0 inz
023200110422     d  Qtapg02                       5  0 inz
023300110422     d  Qtapg03                       5  0 inz
023400110422     d  Qtapg04                       5  0 inz
023500110422     d  Qtapg05                       5  0 inz
023600110422     d  Qtapg06                       5  0 inz
023700110422     d  Qtapg07                       5  0 inz
023800110422     d  Qtapg08                       5  0 inz
023900110422     d  Qtapg09                       5  0 inz
024000110422     d  Qtapg10                       5  0 inz
024100110422     d  Qtapg11                       5  0 inz
024200110422     d  Qtapg12                       5  0 inz
024300110422     d  Qtapg13                       5  0 inz
024400110422     d  Qtapg14                       5  0 inz
024500110422     d  Qtapg15                       5  0 inz
024600110422     d  Qtapg16                       5  0 inz
024700110422     d  Qtapg17                       5  0 inz
024800110422     d  Qtapg18                       5  0 inz
024900110422     d  Qtapg19                       5  0 inz
025000110422     d  Qtapg20                       5  0 inz
025100110422     d  Qtapg21                       5  0 inz
025200110422     d  Qtapg22                       5  0 inz
025300110422     d  Qtapg23                       5  0 inz
025400110422     d  Qtapg24                       5  0 inz
025500110422
025600110422     d Qtapgwds        ds
025700110422     d   $QtaPG                       5s 0 inz  dim(24)
025800110422     d
025900110422     d RCACaappds      ds            70    inz
026000110422     d   $CAAPP                       2    inz  dim(35)
026100110422
026200110422     d RCANrappds      ds                  inz
026300110422     d   $NRAPP                       5s 0 inz  dim(35)
026400110422
026500110422     d Qtappds         ds
026600110422     d  Qtapp01                       5  0 inz
026700110422     d  Qtapp02                       5  0 inz
026800110422     d  Qtapp03                       5  0 inz
026900110422     d  Qtapp04                       5  0 inz
027000110422     d  Qtapp05                       5  0 inz
027100110422     d  Qtapp06                       5  0 inz
027200110422     d  Qtapp07                       5  0 inz
027300110422     d  Qtapp08                       5  0 inz
027400110422     d  Qtapp09                       5  0 inz
027500110422     d  Qtapp10                       5  0 inz
027600110422     d  Qtapp11                       5  0 inz
027700110422     d  Qtapp12                       5  0 inz
027800110422     d  Qtapp13                       5  0 inz
027900110422     d  Qtapp14                       5  0 inz
028000110422     d  Qtapp15                       5  0 inz
028100110422     d  Qtapp16                       5  0 inz
028200110422     d  Qtapp17                       5  0 inz
028300110422     d  Qtapp18                       5  0 inz
028400110422     d  Qtapp19                       5  0 inz
028500110422     d  Qtapp20                       5  0 inz
028600110422     d  Qtapp21                       5  0 inz
028700110422     d  Qtapp22                       5  0 inz
028800110422     d  Qtapp23                       5  0 inz
028900110422     d  Qtapp24                       5  0 inz
029000110422
029100110422     d Qtappwds        ds
029200110422     d   $QtaPP                       5s 0 inz  dim(24)
029300110422     d
029400110422     d RCACaacfds      ds            70    inz
029500110422     d   $CAACF                       2    inz  dim(35)
029600110422
029700110422     d RCANracfds      ds                  inz
029800110422     d   $NRACF                       5s 0 inz  dim(35)
029900110422
030000110422     d Qtacfds         ds
030100110422     d  Qtacf01                       5  0 inz
030200110422     d  Qtacf02                       5  0 inz
030300110422     d  Qtacf03                       5  0 inz
030400110422     d  Qtacf04                       5  0 inz
030500110422     d  Qtacf05                       5  0 inz
030600110422     d  Qtacf06                       5  0 inz
030700110422     d  Qtacf07                       5  0 inz
030800110422     d  Qtacf08                       5  0 inz
030900110422     d  Qtacf09                       5  0 inz
031000110422     d  Qtacf10                       5  0 inz
031100110422     d  Qtacf11                       5  0 inz
031200110422     d  Qtacf12                       5  0 inz
031300110422     d  Qtacf13                       5  0 inz
031400110422     d  Qtacf14                       5  0 inz
031500110422     d  Qtacf15                       5  0 inz
031600110422     d  Qtacf16                       5  0 inz
031700110422     d  Qtacf17                       5  0 inz
031800110422     d  Qtacf18                       5  0 inz
031900110422     d  Qtacf19                       5  0 inz
032000110422     d  Qtacf20                       5  0 inz
032100110422     d  Qtacf21                       5  0 inz
032200110422     d  Qtacf22                       5  0 inz
032300110422     d  Qtacf23                       5  0 inz
032400110422     d  Qtacf24                       5  0 inz
032500110422
032600110422     d Qtacfwds        ds
032700110422     d   $QtaCF                       5s 0 inz  dim(24)
032800110422     d
032900110422     d RCACaaptds      ds            70    inz
033000110422     d   $CAAPT                       2    inz  dim(35)
033100110422
033200110422     d RCANraptds      ds                  inz
033300110422     d   $NRAPT                       5s 0 inz  dim(35)
033400110422
033500110422     d Qtaptds         ds
033600110422     d  Qtapt01                       5  0 inz
033700110422     d  Qtapt02                       5  0 inz
033800110422     d  Qtapt03                       5  0 inz
033900110422     d  Qtapt04                       5  0 inz
034000110422     d  Qtapt05                       5  0 inz
034100110422     d  Qtapt06                       5  0 inz
034200110422     d  Qtapt07                       5  0 inz
034300110422     d  Qtapt08                       5  0 inz
034400110422     d  Qtapt09                       5  0 inz
034500110422     d  Qtapt10                       5  0 inz
034600110422     d  Qtapt11                       5  0 inz
034700110422     d  Qtapt12                       5  0 inz
034800110422     d  Qtapt13                       5  0 inz
034900110422     d  Qtapt14                       5  0 inz
035000110422     d  Qtapt15                       5  0 inz
035100110422     d  Qtapt16                       5  0 inz
035200110422     d  Qtapt17                       5  0 inz
035300110422     d  Qtapt18                       5  0 inz
035400110422     d  Qtapt19                       5  0 inz
035500110422     d  Qtapt20                       5  0 inz
035600110422     d  Qtapt21                       5  0 inz
035700110422     d  Qtapt22                       5  0 inz
035800110422     d  Qtapt23                       5  0 inz
035900110422     d  Qtapt24                       5  0 inz
036000110422
036100110422     d Qtaptwds        ds
036200110422     d   $QtaPT                       5s 0 inz  dim(24)
036300110422     d
036400110422     d RCACaactds      ds            70    inz
036500110422     d   $CAACT                       2    inz  dim(35)
036600110422
036700110422     d RCANractds      ds                  inz
036800110422     d   $NRACT                       5s 0 inz  dim(35)
036900110422
037000110422     d Qtactds         ds
037100110422     d  Qtact01                       5  0 inz
037200110422     d  Qtact02                       5  0 inz
037300110422     d  Qtact03                       5  0 inz
037400110422     d  Qtact04                       5  0 inz
037500110422     d  Qtact05                       5  0 inz
037600110422     d  Qtact06                       5  0 inz
037700110422     d  Qtact07                       5  0 inz
037800110422     d  Qtact08                       5  0 inz
037900110422     d  Qtact09                       5  0 inz
038000110422     d  Qtact10                       5  0 inz
038100110422     d  Qtact11                       5  0 inz
038200110422     d  Qtact12                       5  0 inz
038300110422     d  Qtact13                       5  0 inz
038400110422     d  Qtact14                       5  0 inz
038500110422     d  Qtact15                       5  0 inz
038600110422     d  Qtact16                       5  0 inz
038700110422     d  Qtact17                       5  0 inz
038800110422     d  Qtact18                       5  0 inz
038900110422     d  Qtact19                       5  0 inz
039000110422     d  Qtact20                       5  0 inz
039100110422     d  Qtact21                       5  0 inz
039200110422     d  Qtact22                       5  0 inz
039300110422     d  Qtact23                       5  0 inz
039400110422     d  Qtact24                       5  0 inz
039500110422
039600110422     d Qtactwds        ds
039700110422     d   $QtaCT                       5s 0 inz  dim(24)
039800110422     d
039900110422     d RCACaapptds     ds            70    inz
040000110422     d   $CAAPPT                      2    inz  dim(35)
040100110422
040200110422     d RCANrapptds     ds                  inz
040300110422     d   $NRAPPT                      5s 0 inz  dim(35)
040400110422
040500110422     d Qtapptds        ds
040600110422     d  Qtappt01                      5  0 inz
040700110422     d  Qtappt02                      5  0 inz
040800110422     d  Qtappt03                      5  0 inz
040900110422     d  Qtappt04                      5  0 inz
041000110422     d  Qtappt05                      5  0 inz
041100110422     d  Qtappt06                      5  0 inz
041200110422     d  Qtappt07                      5  0 inz
041300110422     d  Qtappt08                      5  0 inz
041400110422     d  Qtappt09                      5  0 inz
041500110422     d  Qtappt10                      5  0 inz
041600110422     d  Qtappt11                      5  0 inz
041700110422     d  Qtappt12                      5  0 inz
041800110422     d  Qtappt13                      5  0 inz
041900110422     d  Qtappt14                      5  0 inz
042000110422     d  Qtappt15                      5  0 inz
042100110422     d  Qtappt16                      5  0 inz
042200110422     d  Qtappt17                      5  0 inz
042300110422     d  Qtappt18                      5  0 inz
042400110422     d  Qtappt19                      5  0 inz
042500110422     d  Qtappt20                      5  0 inz
042600110422     d  Qtappt21                      5  0 inz
042700110422     d  Qtappt22                      5  0 inz
042800110422     d  Qtappt23                      5  0 inz
042900110422     d  Qtappt24                      5  0 inz
043000110422
043100110422     d Qtapptwds       ds
043200110422     d   $QtaPPT                      5s 0 inz  dim(24)
043300110422     d
043400110422     d RCACaaids       ds            70    inz
043500110422     d   $CAAI                        2    inz  dim(35)
043600110422
043700110422     d RCANraids       ds                  inz
043800110422     d   $NRAI                        5s 0 inz  dim(35)
043900110422
044000110422     d Qtainds         ds
044100110422     d  Qtain01                       5  0 inz
044200110422     d  Qtain02                       5  0 inz
044300110422     d  Qtain03                       5  0 inz
044400110422     d  Qtain04                       5  0 inz
044500110422     d  Qtain05                       5  0 inz
044600110422     d  Qtain06                       5  0 inz
044700110422     d  Qtain07                       5  0 inz
044800110422     d  Qtain08                       5  0 inz
044900110422     d  Qtain09                       5  0 inz
045000110422     d  Qtain10                       5  0 inz
045100110422     d  Qtain11                       5  0 inz
045200110422     d  Qtain12                       5  0 inz
045300110422     d  Qtain13                       5  0 inz
045400110422     d  Qtain14                       5  0 inz
045500110422     d  Qtain15                       5  0 inz
045600110422     d  Qtain16                       5  0 inz
045700110422     d  Qtain17                       5  0 inz
045800110422     d  Qtain18                       5  0 inz
045900110422     d  Qtain19                       5  0 inz
046000110422     d  Qtain20                       5  0 inz
046100110422     d  Qtain21                       5  0 inz
046200110422     d  Qtain22                       5  0 inz
046300110422     d  Qtain23                       5  0 inz
046400110422     d  Qtain24                       5  0 inz
046500110422
046600110422     d Qtainwds        ds
046700110422     d   $QtaIN                       5s 0 inz  dim(24)
046800110422     d
046900110426
047000110426      // schiere di lavoro per i totali generali
047100110426     d WRKCaads        ds
047200110426     d   $WrkCaa                      2    inz  dim(35)
047300110426
047400110426     d WRKNrads        ds
047500110426     d   $WrkNra                      5s 0 inz  dim(35)
047600080206      //---------------------------------------------------------------
047700080206      //?Definizione variabili globali.
047800080206      //---------------------------------------------------------------
047900080206
048000100525      // - Fl
048100100305     d $End            s               n   inz(*off)
048200100305     d $EndAge         s               n   inz(*off)
048300100305     d $RcdOk          s               n   inz(*off)
048400080206
048500080206      // - Indici di schiera
048600100429     d xx              s              4  0 inz
048700100429     d yy              s              4  0 inz
048800100429     d zz              s              4  0 inz
048900110422     d xpm             s              4  0 inz
049000110422     d xpg             s              4  0 inz
049100110422     d xpp             s              4  0 inz
049200110422     d xcf             s              4  0 inz
049300110422     d xpt             s              4  0 inz
049400110422     d xct             s              4  0 inz
049500110422     d xppt            s              4  0 inz
049600110422     d xin             s              4  0 inz
049700080206
049800090806       // - Stringa SQL da eseguire
049900090806     d wSQL            s           2048    Varying        inz
050000080206
050100090401      // - Campi di comodo data
050200100305     d wDate_EUR       s               d   datfmt(*eur)
050300100305     d wDate_ISO       s               d   datfmt(*iso)
050400100305
050500100305      // - Campi di comodo
050600100308     d sav_ATCdad      s                   like(ATCdad)
050700100309     d wAff            s              1  0 inz
050800100308     d wCatMer         s              5  0 inz
050900100309     d wCmm            s                   like(ATCcmm)
051000100923     d wConc           s              2  0 inz
051100100308     d wNota           s              1  0 inz
051200100305     d wTrat           s            196    inz(*all'-')
051300100521     d TipoStampa      s              1
051400100522     d mkcmm           s                   like(ATCcmm)
051500180219     d wpft            s              9s 0 inz
051600100525     d WTot_M_FP       s              7  0 inz
051700100525     d WTot_M_RA       s              7  0 inz
051800100525     d WTot_M_RC       s              7  0 inz
051900100525     d WTot_M_UF       s              7  0 inz
052000100525     d wTot_H          s              5  0 inz
052100100525     d wTot_R          s              5  0 inz
052200100526     d W_fra           s              6  0 inz
052300100526     d W_frc           s              6  0 inz
052400100526     d W_fuf           s              6  0 inz
052500100526     d W_feriehh       s              6  0 inz
052600100525
052700100525       // - Campi di totali
052800100525     d t_tepm          s                   like(rcatepm)
052900110426     d t_tepg          s                   like(rcatepm)
053000110426     d t_tepp          s                   like(rcatepm)
053100110426     d t_tecft         s                   like(rcatepm)
053200110426     d t_tept          s                   like(rcatepm)
053300110426     d t_tect          s                   like(rcatepm)
053400110426     d t_teppt         s                   like(rcatepm)
053500110426     d t_tepost        s                   like(rcatepm)
053600110426     d t_tepoa         s                   like(rcatepm)
053700110426     d t_tecaldi       s                   like(rcatepm)
053800110426     d t_tecalda       s                   like(rcatepm)
053900110426     d t_teposve       s                   like(rcatepm)
054000110426     d t_tepv32a       s                   like(rcatepm)
054100110426     d t_temante       s                   like(rcatepm)
054200110426     d t_tepc40a       s                   like(rcatepm)
054300110426     d t_teincre       s                   like(rcatepm)
054400110426     d t_tepc50a       s                   like(rcatepm)
054500110426     d t_telese        s                   like(rcatepm)
054600110426     d t_inevase       s                   like(rcatepm)
054700100609     d t_patn          s                   like(rcapatn)
054800100525     d t_patm          s                   like(rcapatm)
054900100525     d t_pata          s                   like(rcapata)
055000100525     d t_patc          s                   like(rcapatc)
055100110119     d t_pati          s                   like(rcapati)
055200110119     d t_patr          s                   like(rcapatr)
055300100622     d t_apptn         s                   like(rcaapptn)
055400100622     d t_apptm         s                   like(rcaapptm)
055500100622     d t_appta         s                   like(rcaappta)
055600100622     d t_apptc         s                   like(rcaapptc)
055700110119     d t_appti         s                   like(rcaappti)
055800110119     d t_apptr         s                   like(rcaapptr)
055900100525     d t_aptn          s                   like(rcaaptn)
056000100525     d t_aptm          s                   like(rcaaptm)
056100100525     d t_apta          s                   like(rcaapta)
056200100525     d t_aptc          s                   like(rcaaptc)
056300110119     d t_apti          s                   like(rcaapti)
056400110119     d t_aptr          s                   like(rcaaptr)
056500100525     d t_aftn          s                   like(rcaaftn)
056600100525     d t_aftm          s                   like(rcaaftm)
056700100525     d t_afta          s                   like(rcaafta)
056800100525     d t_aftc          s                   like(rcaaftc)
056900110119     d t_afti          s                   like(rcaafti)
057000110119     d t_aftr          s                   like(rcaaftr)
057100100525     d t_ap24          s                   like(rcaap24)
057200100525     d t_feriegg       s                   like(rcaferiegg)
057300100629     d T_totapppr      s                   like(totapppr)
057400100629     d T_totapppr1     s                   like(totapppr1)
057500100526     d T_totapp        s                   like(totapp)
057600100526     d T_tottel        s                   like(tottel)
057700100526     d T_appm          s                   like(rpaappm)
057800100526     d T_appn          s                   like(rpaappn)
057900100526     d T_appa          s                   like(rpaappa)
058000100526     d T_appc          s                   like(rpaappc)
058100110119     d T_appi          s                   like(rpaappi)
058200110119     d T_appr          s                   like(rpaappr)
058300100526     d T_affm          s                   like(rpaaffm)
058400100526     d T_affn          s                   like(rpaaffn)
058500100526     d T_affa          s                   like(rpaaffa)
058600100526     d T_affc          s                   like(rpaaffc)
058700110119     d T_affi          s                   like(rpaaffi)
058800110119     d T_affr          s                   like(rpaaffr)
058900100526     d T_atc19         s                   like(rpatc19)
059000100526     d T_atc20         s                   like(rpatc20)
059100100526     d T_atc21         s                   like(rpatc21)
059200100526     d T_atc22         s                   like(rpatc22)
059300100526     d T_atc24         s                   like(rpatc24)
059400100526     d T_atc25         s                   like(rpatc25)
059500100526     d T_atc26         s                   like(rpatc26)
059600100526     d T_atc27         s                   like(rpatc27)
059700100526     d T_atc28         s                   like(rpatc28)
059800100526     d T_atc30         s                   like(rpatc30)
059900100526     d T_atc31         s                   like(rpatc31)
060000100526     d T_atc32         s                   like(rpatc32)
060100100526     d T_atc33         s                   like(rpatc33)
060200110117     d T_atc44         s                   like(rpatc44)
060300100526     d T_atc60         s                   like(rpatc60)
060400110426     d T_atc70         s                   like(rpatc70)
060500110426     d T_atc71         s                   like(rpatc71)
060600110426     d T_atc73         s                   like(rpatc73)
060700110426     d T_atc74         s                   like(rpatc74)
060800110426     d T_atc75         s                   like(rpatc75)
060900100526     d T_off           s                   like(rpaoff)
061000100526     d T_fegg          s                   like(rpafegg)
061100100429
061200090508      //---------------------------------------------------------------
061300090807      //?Definizione procedure esterne.
061400090508      //---------------------------------------------------------------
061500100120
061600100120      //---------------------------------------------------------------
061700100120      //?prototipi
061800100120      //---------------------------------------------------------------
061900090807
062000080626      /copy gaitrasrc/srcprotopr,tibs02r
062100080626      /copy gaitrasrc/srcprotopr,tibs34r
062200100308      /copy gaitrasrc/srcprotopr,tibs69r
062300100429      /copy gaitrasrc/srcprotopr,tntaa1c
062400110120
062500110120      // - Filiali e aree abilitate
062600110120     d trul31r         pr                  extpgm('TRUL31R')
062700110120     d  kpjba                              likeds(KPJBA)
062800110120     d  trul31ds                           likeds(trul31ds)
062900110120     d  trul31ds2                          likeds(trul31ds2)
063000090807
063100080206      //---------------------------------------------------------------
063200080206      //?Definizione key-list.
063300080206      //---------------------------------------------------------------
063400090807
063500090807       // - File TABEL00F
063600090807     d k03tabel      e ds                  extname(TABEL00F:*key)
063700090807     d                                     prefix(k_)
063800090807     d                                     inz
063900080206
064000080206      //---------------------------------------------------------------
064100080206      //?Riepilogo indicatori.
064200080206      //---------------------------------------------------------------
064300090807
064400110120      // 09    : Condizione stampa selezione per Area
064500100504      // 10    : HI tipo attività se da eseguire
064600100514      // 11    : Se acceso non stampa in testata le attività escluse
064700100528      // 12    : Se acceso non stampa in testata il commerciale
064800100528      // 13    : Se acceso stampa nella consuntiva selezione dei commerciali
064900100528      // 14    : Se acceso non stampa in testata le date
065000110120      // 20    : Di cui altri commerciali
065100110120      // 30    : Stampa affiancamenti nella preventiva
065200110120      // 31    : Stampa affiancamenti nella preventiva
065300110120      // 32    : Stampa affiancamenti nella preventiva
065400110120      // 33    : Stampa affiancamenti nella preventiva
065500110120      // 34    : Stampa affiancamenti nella preventiva
065600100308      // 99    : Salto pagina
065700090807
065800080206      //---------------------------------------------------------------
065900080206
066000080206      //---------------------------------------------------------------
066100080206      //?M A I N - L I N E
066200080206      //---------------------------------------------------------------
066300080206
066400080206     c     *Entry        plist
066500080206     c                   parm                    KPJBA
066600080206
066700080206      /free
066800080206
066900090807       //?Operazioni iniziali
067000080206       exsr RoutInz;
067100100305
067200100521       // Ciclo principale
067300100521       //?Ciclo per sk unificanti e stampo
067400100521       zz = 1;
067500100521       FOR zz by 1 to %elem($AgeU);
067600100521       IF  $AgeU(zz) <> *zeros;
067700100521
067800100521       // verifico che cosa devo fare
067900100521        SELECT ;
068000100521       // se sono stata richiamata per la sola stampa agenda
068100100521         When i88tcpe =  ' ' ;
068200100521             tipostampa = 'A' ;
068300100522             exsr sr_intesta ;
068400100521             Exsr Agenda ;
068500100305
068600100521       // se sono stata richiamata per le statistiche
068700100521       // CONSUNTIVE
068800100522         When i88tcpe = 'C'  ;
068900100521       //    Stampa agenda consuntiva
069000100521             If i88dett = 'S' ;
069100100521                clear trmk86ds ;
069200100521                imk86cmm = i88cmmu;
069300100521                Imk86datai = i88deci ;
069400100521                Imk86dataf = i88decf ;
069500100521                Imk86ioc   = i88dscmm;
069600100521                Imk86iec   = i88dscmmie;
069700100521                Imk86iot   = i88dstat;
069800100521                Imk86tat   = i88dstatc ;
069900100521                tipostampa = 'A' ;
070000100522                exsr sr_intesta ;
070100100521                Exsr Agenda ;
070200100521             Endif ;
070300100521       //    Stampa statistica commerciale
070400100521             If i88totc = 'S' ;
070500100521                tipostampa = 'C' ;
070600100521                Exsr Stat_consun ;
070700100521             Endif ;
070800100521
070900100521       // PREVENTIVE
071000100522         When i88tcpe =  'P'  ;
071100100521       //    Stampa agenda preventiva
071200100521             If i88dett = 'S' ;
071300100521                clear trmk86ds ;
071400100521                imk86cmm = i88cmmu;
071500100521                Imk86datai = i88depi ;
071600100521                Imk86dataf = i88depf ;
071700100521                Imk86ioc   = i88dscmm;
071800100521                Imk86iec   = i88dscmmie;
071900100521                Imk86iot   = i88dstat;
072000100521                Imk86tat   = i88dstatc ;
072100100521                tipostampa = 'A' ;
072200100522                exsr sr_intesta ;
072300100521                Exsr Agenda ;
072400100521             Endif ;
072500100521       //    Stampa statistica commerciale
072600100521             If i88totc = 'S' ;
072700100521                tipostampa = 'P' ;
072800100522                Exsr Stat_preven ;
072900100521             Endif ;
073000100522
073100100522       // se sono stata richiamata per le statistiche
073200100522       // CONSUNTIVE E PREVENTIVE
073300100522         When i88tcpe = 'E'  ;
073400100522       //    Stampa agenda consuntiva
073500100522             If i88dett = 'S' ;
073600100522                clear trmk86ds ;
073700100522                imk86cmm = i88cmmu;
073800100522                Imk86datai = i88deci ;
073900100522                Imk86dataf = i88decf ;
074000100522                Imk86ioc   = i88dscmm;
074100100522                Imk86iec   = i88dscmmie;
074200100522                Imk86iot   = i88dstat;
074300100522                Imk86tat   = i88dstatc ;
074400100522                tipostampa = 'A' ;
074500100522                exsr sr_intesta ;
074600100522                Exsr Agenda ;
074700100522             Endif ;
074800100522       //    Stampa statistica commerciale
074900100522             If i88totc = 'S' ;
075000100522                tipostampa = 'C' ;
075100100522                Exsr Stat_consun ;
075200100522             Endif ;
075300100522
075400100522       // PREVENTIVE
075500100522       //    Stampa agenda preventiva
075600100522             If i88dett = 'S' ;
075700100522                clear trmk86ds ;
075800100522                imk86cmm = i88cmmu;
075900100522                Imk86datai = i88depi ;
076000100522                Imk86dataf = i88depf ;
076100100522                Imk86ioc   = i88dscmm;
076200100522                Imk86iec   = i88dscmmie;
076300100522                Imk86iot   = i88dstat;
076400100522                Imk86tat   = i88dstatc ;
076500100522                tipostampa = 'A' ;
076600100522                exsr sr_intesta ;
076700100522                Exsr Agenda ;
076800100522             Endif ;
076900100522       //    Stampa statistica commerciale
077000100522             If i88totc = 'S' ;
077100100522                tipostampa = 'P' ;
077200100522                Exsr Stat_preven ;
077300100522             Endif ;
077400100521
077500100521          ENDSL ;
077600100521         ENDIF;
077700100521
077800100521       ENDFOR;
077900100525       // verifico se devo stampare i totali generali
078000100525       If i88totg = 'S' ;
078100100525          exsr SR_TotGen ;
078200100525       Endif ;
078300100525
078400100305       //?Stampo la fine
078500100702       // decofico i campi da stampare alla fine
078600110120         //?Stampo l'area richiesta
078700110120         IF  I88car > 0;
078800110120           clear ds05;
078900110120           k_tblcod = '05';
079000110120           k_tblkey = %editc(I88car:'X');
079100110120           chain  %kds(K03tabel) TABEL00F;
079200110120           IF  %found( TABEL00F );
079300110120             ds05 = TBLuni;
079400110120           ENDIF;
079500110120           pDesCar = §05des;
079600110120         ENDIF;
079700110120
079800110120         //?Stampo che richieste tutte le aree
079900110120         IF  I88car = 0;
080000110120           pDesCar = 'Tutte';
080100110120         ENDIF;
080200110120
080300110120         //?Stampo l'unificante richiesto
080400130917         clear  pDesCmmu;
080500100702         IF  I88cmmu  > 0;
080600130917           chain  (i88cmmu)  AZCMM000;
080700130917           IF  %found( AZCMM01L );
080800130917             pDesCmmu = CMMdes;
080900100702           ENDIF;
081000100702         else ;
081100100702           pDesCmmu = 'Tutti' ;
081200100702         ENDIF;
081300100702         //?Imposto data inizio e fine stampa
081400100707         If i88deci <> *zeros ;
081500100702            wDate_ISO = %date(I88deci:*iso);
081600100702            wDate_EUR = wDate_ISO;
081700100702            pDataic   = %dec(wDate_EUR);
081800100702            wDate_ISO = %date(I88decf:*iso);
081900100702            wDate_EUR = wDate_ISO;
082000100702            pDatafc   = %dec(wDate_EUR);
082100100707         else ;
082200100707            clear pdatafc ;
082300100707            clear pdataic ;
082400100707         endif ;
082500100707         If i88depi <> *zeros ;
082600100707            wDate_ISO = %date(I88depi:*iso);
082700100707            wDate_EUR = wDate_ISO;
082800100707            pDataip   = %dec(wDate_EUR);
082900100707            wDate_ISO = %date(I88depf:*iso);
083000100707            wDate_EUR = wDate_ISO;
083100100707            pDatafp   = %dec(wDate_EUR);
083200100707         else ;
083300100707            clear pdatafp ;
083400100707            clear pdataip ;
083500100707         endif ;
083600100702        // attività incluse / escluse
083700100702            i88tat1 = %subst(i88dstatc: 1: 1) ;
083800100702            i88tat2 = %subst(i88dstatc: 2: 1) ;
083900100702            i88tat3 = %subst(i88dstatc: 3: 1) ;
084000100702            i88tat4 = %subst(i88dstatc: 4: 1) ;
084100100702            i88tat5 = %subst(i88dstatc: 5: 1) ;
084200100702
084300100528       // se ho richiesto solo il dettaglio agenda stampo solo fine stampa
084400100528       If i88totc = 'N' and i88totg = 'N' ;
084500100713          write MK89FINE3;
084600100528       else ;
084700100528       // stampo la legenda delle causali
084800100528          tbecod = 'CCO' ;
084900100528          *in11 = *on ;
085000100528          *in12 = *on ;
085100100528          *in13 = *off ;
085200100528          *in14 = *on  ;
085300100528          tipostampa = 'F' ;
085400100528          exsr sr_StpSalto;
085500100528          write mk89fine1 ;
085600100528          setll tbecod tntbe01l ;
085700100528          reade tbecod tntbe01l ;
085800100621          dow not %eof(tntbe01l) ;
085900100528              dcco = tbeuni ;
086000100621              if §ccosta = 'S' and pcodcc1 = '  ' ;
086100100621                 pcodcc1 = tbeke1 ;
086200100621                 pdescc1 = §ccodes;
086300100622                 reade tbecod tntbe01l ;
086400100621                 iter ;
086500100528              endif ;
086600100621              if §ccosta = 'S' and pcodcc2 = '  ' ;
086700100621                 pcodcc2 = tbeke1 ;
086800100621                 pdescc2 = §ccodes;
086900100622                 reade tbecod tntbe01l ;
087000100621                 iter ;
087100100621              endif ;
087200100621              if §ccosta = 'S' and pcodcc3 = '  ' ;
087300100621                 pcodcc3 = tbeke1 ;
087400100621                 pdescc3 = §ccodes;
087500100621                 write mk89fine2 ;
087600100621                 clear pcodcc1 ;
087700100621                 clear pdescc1 ;
087800100621                 clear pcodcc2 ;
087900100621                 clear pdescc2 ;
088000100621                 clear pcodcc3 ;
088100100621                 clear pdescc3 ;
088200100621              endif ;
088300100622              reade tbecod tntbe01l ;
088400100528          enddo ;
088500100621          If pcodcc1 <> '  ' or pcodcc2 <> '  ' or pcodcc3 <> '  ' ;
088600100621                 write mk89fine2 ;
088700100621                 clear pcodcc1 ;
088800100621                 clear pdescc1 ;
088900100621                 clear pcodcc2 ;
089000100621                 clear pdescc2 ;
089100100621                 clear pcodcc3 ;
089200100621                 clear pdescc3 ;
089300100621          endif ;
089400100528          write MK89FINE;
089500100713          write MK89FINE3;
089600100528       endif ;
089700080206
089800090807       //?Operazioni finali
089900080206       exsr RoutEnd;
090000080206
090100080206       //--------------------------------------------------------------
090200080206       //?Operazioni iniziali.
090300080206       //--------------------------------------------------------------
090400080206       BEGSR RoutInz;
090500090807
090600100522         // verifico se sto lanciando la sola stampa agenda oppure anche la statistica
090700100522         // devo valorizzare la ds che mi serve al momento
090800100522         If %subst(kpjbu:1:1) <> '0' ;
090900100522            trmk88ds = kpjbu ;
091000100522            MKcmm   = i88cmmU ;
091100100522         else ;
091200100522            trmk86ds = kpjbu ;
091300100522            MKcmm   = imk86cmm;
091400100522         endif ;
091500100522
091600090807         //?Reperimento dati job
091700090807         exsr DatiJob;
091800090807
091900090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
092000090806
092100100305         //?Imposto nome programma
092200100305         NomePgm = SDSpgm;
092300100305
092400100305         //?Imposto data e ora elaborazione
092500100505         DataEla = %dec(%date():*eur);
092600100305         OraEla  = %dec(%time());
092700100514
092800100429         clear $AgeU;
092900100429
093000100429         //?Carico in sk l'unificante richiesto
093100100522         IF  MKcmm > 0;
093200100522           $AgeU(01) = MKcmm;
093300100429         ENDIF;
093400100429
093500100429         //?Carico gli unificanti delle filiali gestite dall'utente
093600100522         IF  MKcmm = 0;
093700100429           exsr sr_CarAgeUni;
093800100429           sorta $AgeU;
093900100429         ENDIF;
094000100305
094100090714
094200080206       ENDSR;
094300100521
094400100521       //--------------------------------------------------------------
094500100521       //?Elaboro AGENDA
094600100521       //--------------------------------------------------------------
094700100521       BEGSR Agenda     ;
094800100521
094900100525           wCmm = $AgeU(zz);
095000100525           exsr sr_DecCmm;
095100100525         // verifico se devo includere od escludere dei commerciali
095200100525           select ;
095300100525         // se omessi gli interni
095400130917            when imk86ioc = 'O'  and imk86iec = 'I' and
095500130917                 (CMMfun =  'COMIN' or  CMMfun =  'ASC' or  CMMfun =  'SA');
095600100525         // se omessi gli esterni
095700130917            when imk86ioc = 'O'  and imk86iec = 'E' and
095800130917                  CMMfun <> 'COMIN' and CMMfun <> 'ASC' and CMMfun <> 'SA';
095900100525         // se inclusi gli esterni
096000130917            when imk86ioc = 'I'  and imk86iec = 'E' and
096100130917                 (CMMfun =  'COMIN' or  CMMfun =  'ASC' or  CMMfun =  'SA');
096200100525         // se inclusi gli interni
096300130917            when imk86ioc = 'I'  and imk86iec = 'I' and
096400130917                  CMMfun <> 'COMIN' and CMMfun <> 'ASC' and CMMfun <> 'SA';
096500100525          other ;
096600100525
096700100521           clear sav_atcdad;
096800100521
096900100521         //?Carico sk agenti dell'unificante
097000100521           exsr sr_CarAge;
097100100521
097200100521        //?Stampo le attività
097300100521           exsr sr_StpAttivita;
097400100521
097500100525          endsl ;
097600100521       ENDSR ;
097700100521
097800100522       //--------------------------------------------------------------
097900100522       //?Imposto i campi della testata
098000100522       //--------------------------------------------------------------
098100100522       BEGSR sr_intesta;
098200100522
098300100527          clear ptatio ;
098400100525
098500100522         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
098600100522         If imk86tat =  *blanks  ;
098700100522            *in11 = *on ;
098800100522         endif ;
098900100522         // imposto la schiera dei tipi attività da stampare
099000100522         §mk86tatds = imk86tat ;
099100100522         // li decodifico per la stampa
099200100522         If imk86iot = 'O' ;
099300100522            PINCOMI = 'ESCLUSE' ;
099400100522         else ;
099500100522            PINCOMI = 'INCLUSE' ;
099600100522         endif ;
099700100522         // decodifico le singole attività incluse o escluse
099800100522         xx = 1;
099900100522         FOR xx by 1 to %elem($tat) ;
100000100522         If $tat(xx) <> ' ' ;
100100100522           clear TIBS02ds;
100200100522           clear dTTA;
100300100522           T02mod = 'C';
100400100522           T02cod = 'TTA';
100500100522           T02ke1 = $tat(xx) ;
100600100522           T02sif = KNSIF;
100700100522           TNTBE_RicercaControllo (kpjba : tibs02ds);
100800100522           IF  T02err = *blanks;
100900100522             dTTA = T02uni;
101000100522           ENDIF;
101100100522           if Ptatio = *blanks ;
101200100522              Ptatio  = §ttadesb ;
101300100522           else ;
101400100522              Ptatio  = %trim(Ptatio) + '/' + §ttadesb ;
101500100522           endif ;
101600100522         endif ;
101700100522         ENDFOR ;
101800100522
101900100522         //?Imposto data inizio e fine stampa
102000100522         wDate_ISO = %date(IMK86datai:*iso);
102100100522         wDate_EUR = wDate_ISO;
102200100522         pDatai    = %dec(wDate_EUR);
102300100522         wDate_ISO = %date(IMK86dataf:*iso);
102400100522         wDate_EUR = wDate_ISO;
102500100522         pDataf    = %dec(wDate_EUR);
102600100522
102700100522       ENDSR ;
102800100522
102900100308       //--------------------------------------------------------------
103000100308       //?Stampo il salto pagina.
103100100308       //--------------------------------------------------------------
103200100308       BEGSR sr_StpSalto;
103300100429
103400100429         //?Decodifico agente
103500100527         IF TipoStampa = 'A' or Tipostampa = 'C' or Tipostampa = 'P' ;
103600100526            wCmm = $AgeU(zz);
103700100526            pCodCmm = $AgeU(zz);
103800100526            exsr sr_DecCmm;
103900130917            pDesCmm = CMMdes;
104000100527         Endif;
104100100308
104200100521         // Imposto titolo della testata
104300100609         IF TipoStampa = 'A' OR Tipostampa = 'F' ;
104400100609            Titolo='             STATISTICA ATTIVITÀ COMMERCIALE              ';
104500100609         endif ;
104600100521
104700100521         IF TipoStampa = 'C' ;
104800100609            Titolo='        STATISTICA ATTIVITÀ COMMERCIALE CONSUNTIVA        ';
104900100527            Clear Ptatio ;
105000100521         endif ;
105100100521
105200100521         IF TipoStampa = 'P' ;
105300100609            Titolo='        STATISTICA ATTIVITÀ COMMERCIALE PREVENTIVA        ';
105400100527            Clear Ptatio ;
105500100521         endif ;
105600100609         IF TipoStampa = 'T' ;
105700100609            Titolo='STATISTICA ATTIVITÀ COMMERCIALE CONSUNTIVA COMM.LI INTERNI-
105800100610       /ESTERNI' ;
105900100527            Clear Ptatio ;
106000100525         endif ;
106100100609         IF TipoStampa = 'I' ;
106200100609            Titolo='STATISTICA ATTIVITÀ COMMERCIALE CONSUNTIVA COMM.LI INTERNI';
106300100609            Clear Ptatio ;
106400100609         endif ;
106500100609         IF TipoStampa = 'E' ;
106600100609            Titolo='STATISTICA ATTIVITÀ COMMERCIALE CONSUNTIVA COMM.LI ESTERNI';
106700100609            Clear Ptatio ;
106800100609         endif ;
106900100521
107000100526         IF TipoStampa = '2' ;
107100100609            Titolo='STATISTICA ATTIVITÀ COMMERCIALE PREVENTIVA COMM.LI INTERNI';
107200100527            Clear Ptatio ;
107300100526         endif ;
107400100527
107500100527         IF TipoStampa = '3' ;
107600100609            Titolo='STATISTICA ATTIVITÀ COMMERCIALE PREVENTIVA COMM.LI ESTERNI';
107700100527            Clear Ptatio ;
107800100527         endif ;
107900100528
108000100528         IF TipoStampa = 'F' ;
108100100528            Titolo = '    STATISTICA COMMERCIALE CRM     ';
108200100528            Clear Ptatio ;
108300100528         endif ;
108400100526
108500100308         //?Stampo la testata
108600100521         write MK89PAGINA;
108700100308         wrkriga = '*' + wTrat + '*';
108800100521         write MK89SEPARA;
108900100521         write MK89TESTA;
109000100521         write MK89SEPARA;
109100100309         //?Stampo le intestazioni delle colonne
109200100521         If Tipostampa = 'A' ;
109300100521            write MK89INTCOL;
109400100521         endif ;
109500100305
109600100308       ENDSR;
109700100429
109800100305       //--------------------------------------------------------------
109900100305       //?Stampo le attività.
110000100305       //--------------------------------------------------------------
110100100305       BEGSR sr_StpAttivita;
110200100305
110300100305         $End = *off;
110400100305
110500100305         //?Imposto la stringa per SQL
110600100305         exsr  sr_PrepSQL;
110700100305
110800100305         //?Dichiarazione cursore
110900100305         exec sql
111000100305           prepare S1   from :wSQL;
111100100305         exec sql
111200100305           declare ATT  cursor for S1;
111300100305
111400100305         //?Apertura del cursore
111500100305         exec sql
111600100305           open ATT;
111700100305
111800100305         IF sqlcode < 0;
111900100305           $End = *on;
112000100305         ENDIF;
112100100305
112200100305         DOW not $End;
112300100305           exec sql
112400100305             fetch next from ATT into :atctat, :atccpo, :atcksc,
112500100308                                      :atcatn, :atcatnp,
112600100309                                      :atcdad, :atchda, :atcnrv,
112700100331                                      :atcoii, :atcofi, :atccad,
112800100525                                      :atcco3, :atcdco,
112900100525                                      :atccac, :atcest;
113000100305           IF sqlcod = 100 or sqlcod < 0;
113100100305             $End = *on;
113200100305             leave;
113300100305           ENDIF;
113400100305
113500100305         //?Controllo se ok il rcd che sto leggendo per l'agenda
113600100305           exsr sr_CtrRcdOk;
113700100305           IF  not $RcdOk;
113800100305             iter;
113900100305           ENDIF;
114000100305
114100100309         //?Stampo la separazione per data attività
114200100305           IF atcdad <> sav_atcdad;
114300100308           //?Salto pagina
114400100505             IF  CurLine >= 60 or sav_atcdad = 0;
114500100308               exsr sr_StpSalto;
114600100308               *in99 = *off;
114700100308             ENDIF;
114800100308             clear wrkriga;
114900100521             write MK89SEPARA;
115000100308             wDate_ISO  = %date(atcdad:*iso);
115100100308             wDate_EUR  = wDate_ISO;
115200100308             pData      = %dec(wDate_EUR);
115300100521             write MK89GIORNO;
115400100305             sav_atcdad = atcdad;
115500100305           ENDIF;
115600100309
115700100506           if  *in99;
115800100506             exsr sr_StpSalto;
115900100506             *in99 = *off;
116000100506           endif;
116100100521           clear MK89RIGA;
116200100521           clear MK89RIGAUF;
116300100305
116400100309         //?Imposto il tipo attività
116500100309           clear TIBS02ds;
116600100309           clear dTTA;
116700100309           T02mod = 'C';
116800100309           T02cod = 'TTA';
116900100309           T02ke1 = ATCtat;
117000100309           T02sif = KNSIF;
117100100309           TNTBE_RicercaControllo (kpjba : tibs02ds);
117200100309           IF  T02err = *blanks;
117300100309             dTTA = T02uni;
117400100309           ENDIF;
117500100309           pDesTat  = §TTAdesb;
117600100309           pDesTatu = §TTAdesb;
117700100504         //?se attvità da eseguire imposto che il tipo attività è da stampare in HI
117800100504           IF  ATCdco > 0;
117900100504             *in10 = *off;
118000100504           ELSE;
118100100504             *in10 = *on;
118200100504           ENDIF;
118300100309
118400100309           pOra  = ATChda / 100;
118500100309
118600100309         //?Attività legata a cliente
118700100309           IF  §TTALEG = 'S';
118800100305         //?Stampo le attività a dettaglio
118900100309             exsr sr_StpDettaglio;
119000100712         //?Stampo i concorrenti
119100100712             exsr sr_StpConc;
119200100309         //?Stampo l'eventuale affiancamento
119300100309             exsr sr_StpAffianca;
119400100309           ENDIF;
119500100309
119600100309         //?Attività NON legata a cliente
119700100309           IF  §TTAleg <> 'S';
119800100309         //?Stampo le attività a dettaglio
119900100309             exsr sr_StpDetUFF;
120000100309           ENDIF;
120100100305
120200100305         ENDDO;
120300100305
120400100305         exec sql
120500100305           close ATT;
120600100305
120700100305       ENDSR;
120800100305
120900100305       //--------------------------------------------------------------
121000100305       //?Stampo le attività a dettaglio.
121100100305       //--------------------------------------------------------------
121200100305       BEGSR sr_StpDettaglio;
121300100308
121400100308         clear pRagSoc;
121500100504         clear pCodKsc;
121600100308         clear pTel;
121700100308         clear wCatMer;
121800100308         clear pClv;
121900100308         clear pVia;
122000100308         clear pLoc;
122100100308         clear pCap;
122200100331         clear pDesCo3;
122300100504         clear pCausale;
122400100308
122500100308         SELECT;
122600100308
122700100308         //?Dati cliente
122800100308           WHEN  ATCksc > 0;
122900100504             pCodKsc = ATCksc;
123000100308             clear tibs69ds;
123100100308             I69kac = ATCksc;
123200100308             I69kin = ATCksc;
123300100308             I69kcp = ATCksc;
123400100308             tibs69r (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
123500100308             IF  O69err = *blanks;
123600100308               pRagSoc = ACOrag;
123700100308               wCatMer = ACOitc;
123800100308               pVia    = INDvia;
123900100308               pLoc    = INDcit;
124000100308               IF  INDsta = *blanks;
124100100308                 pLoc  = %trim(pLoc) + ' ' + INDprv;
124200100308               ELSE;
124300100308                 pLoc  = %trim(pLoc) + ' ' + INDsta;
124400100308               ENDIF;
124500100308               pCap    = %subst(INDcae:1:5);
124600100309               pTel    = INDtel;
124700100309               pClv    = CLPclv;
124800100308             ENDIF;
124900100308
125000100308         //?Dati potenziale
125100100308           WHEN  ATCcpo <> 0;
125200100308             chain ATCcpo TNCPO01L;
125300100308             IF  %found(TNCPO01L);
125400100308               pRagSoc = CPOrag;
125500100308               pVia    = CPOvia;
125600100308               pLoc    = CPOcit;
125700100308               IF  CPOnaz = *blanks;
125800100308                 pLoc  = %trim(pLoc) + ' ' + CPOprv;
125900100308               ELSE;
126000100308                 pLoc  = %trim(pLoc) + ' ' + CPOnaz;
126100100308               ENDIF;
126200100308               pCap    = %subst(CPOcap:1:5);
126300100308               wCatMer = CPOsct;
126400100309               pClv    = CPOftr;
126500100308             ENDIF;
126600140715             chain  ATCcpo  TNCPO100;
126700140715             If  %found(TNCPO11L);
126800140715               pTel    = CPO1tel;
126900140715             EndIf;
127000100308         ENDSL;
127100100308
127200100308         //?Decodifico categoria commerciale
127300100308         clear ds1L;
127400100308         k_tblkut = 1;
127500100308         k_tblcod = '1L';
127600100308         k_tblkey = %editc(wCatMer:'X');
127700100308         chain  %kds(K03tabel) TABEL00F;
127800100308         IF  %found( TABEL00F );
127900100308           ds1L = TBLuni;
128000100308         ENDIF;
128100100308         pDesCat = §1Ldes;
128200100331
128300100331         //?Decodifica commerciale (chi ha preso appuntamento)
128400100712         //?se appuntamento o affiancamento (CHIODO 91/92 e A)
128500100712         IF  ATCtat = 'A' and (ATCcad = '91' or ATCcad = '92');
128600100331           wCmm = ATCco3;
128700100331           exsr sr_DecCmm;
128800130917           pDesCo3 = CMMdes;
128900100331         ENDIF;
129000100504
129100100504         //?CHIODO
129200100504         //?se attvità telefonata devo stampare anche la causale
129300100504         clear pCausale;
129400100504         IF  ATCtat = 'T';
129500100504           pCausale = ATCcad;
129600100504         ENDIF;
129700100308
129800100308         //?Responsabile trasporti
129900100308         clear pRespTra;
130000100308         clear NTCnk1;
130100100308         clear NTCnk2;
130200100308         NTCtnt = '05';
130300100507         SELECT;
130400100507           WHEN  ATCksc > 0;
130500100507             NTCapl = 'C';
130600100507             NTCnk1 = %editc(DUTkci:'X') + %editc(ATCksc:'X');
130700100507           WHEN  ATCnrv > 0;
130800100507             NTCapl = 'T';
130900100507             NTCnk1 = %editc(DUTkci:'X') + %editc(ATCnrv:'X');
131000100507           OTHER;
131100100507             NTCapl = 'P' ;
131200100507             NTCnk1 = %editc(CPOcpo:'X');
131300100507         ENDSL;
131400100507         chain (NTCapl:NTCnk1:NTCnk2:NTCtnt) TFNTC01L;
131500100308         IF  %found(TFNTC01L);
131600100308           pRespTra = NTCrnt;
131700100308         ENDIF;
131800100308
131900100308         //?Dati Info Comm.
132000100308         //?vado con key fissa sulla riga del fatturato annuo presunto 'SPT'
132100100308         clear pFatCpo;
132200100308         clear pSpedCpo;
132300100308         chain (ATCcpo:'SPT') TICPI01L;
132400100308         IF  %found(TICPI01L);
132500180219           wpft = CPIpft;
132600180219           pFatCpo  = 'Sp.Trasp.: ' + %trim(%editc(wpft:'2'));
132700180219           pSpedCpo = 'Sped.Anno: ' + %trim(%editc(CPIsna:'2'));
132800100308         ENDIF;
132900100308
133000100308         //?Dati Trattativa
133100100505         clear pTpv;
133200100505         clear pNrv;
133300100308         clear pOfferta;
133400100309         clear pFatNrv;
133500100309         clear pSpedNrv;
133600110119         clear pAuScNrv;
133700100308         IF  ATCnrv > 0;
133800100308           chain ATCnrv TIVIS05L;
133900100308           IF  %found(TIVIS05L) and VISffz <> 'S';
134000100505             pNrv = VISnrv;
134100100505             pTpv = VIStpv;
134200100308           //?controllo se c'è offerta
134300100506             setll ATCnrv TIVOF01L;
134400100506             reade ATCnrv TIVOF01L;
134500100506             DOW  not %EoF(TIVOF01L);
134600100506               IF  VOFeso <> '*';
134700100506                 pOfferta = 'OFF';
134800100506                 leave;
134900100506               ENDIF;
135000100506               reade ATCnrv TIVOF01L;
135100100506             ENDDO;
135200100308           ENDIF;
135300100308           //?Dati Info trattativa
135400100308           //?vado con key fissa sulla riga del fatturato 'TRT'
135500100308           clear pFatNrv;
135600100308           clear pSpedNrv;
135700100308           chain (ATCnrv:'TRT') TIVII01L;
135800100308           IF  %found(TIVII01L);
135900180219             wpft = VIIpft;
136000180219             pFatNrv  = 'Valore    : ' + %trim(%editc(wpft:'K'));
136100180219             pSpedNrv = 'Spedizioni: ' + %trim(%editc(VIIsna:'2'));
136200100308           ENDIF;
136300110119           //?Se trattativa di tipo 'A'
136400110119           //?cerco anche 'A/S'
136500110119           IF  VIStpv = 'A';
136600110119             clear pAuScNrv;
136700110119             chain (ATCnrv:'A/S') TIVII01L;
136800110119             IF  %found(TIVII01L);
136900110518               pAuScNrv = 'Aum./Sco. : ' + ' ' + %trim(%editc(VIIvald:'K'))
137000110119                        + ' %';
137100110119             ENDIF;
137200110119           ENDIF;
137300100308         ENDIF;
137400100308
137500100308         //?Note attività (2 righe)
137600100520         clear pNota01;
137700100520         clear pNota02;
137800100520         clear pNota03;
137900100520         clear pNota04;
138000100520
138100100520         //?Prima provo con note legate al progressivo attuale
138200100308         wNota = 1;
138300100520         CPNcpo  = ATCcpo;
138400100520         CPNatn  = ATCatn;
138500100520         CPNatnp = ATCatnp;
138600100520         setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
138700100520         reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
138800100520
138900100520         DOW  not %eof(TICPN05L) and wNota <= 2;
139000100520           IF  wNota = 1;
139100100520             pNota01 = %subst(CPNnot:1:45);
139200100520             pNota02 = %subst(CPNnot:46:45);
139300100520           ENDIF;
139400100520           IF  wNota = 2;
139500100520             pNota03 = %subst(CPNnot:1:45);
139600100520             pNota04 = %subst(CPNnot:46:45);
139700100520           ENDIF;
139800100520
139900100520           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
140000100520           wNota += 1;
140100100520         ENDDO;
140200100520
140300100520         //?Se non ho trovato note, provo con note legate al progressivo -1
140400100520         IF  pNota01 = *blanks and pNota02 = *blanks and
140500100520             pNota03 = *blanks and pNota04 = *blanks;
140600100520           CPNatnp = ATCatnp - 1;
140700100520           setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
140800100520           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
140900100308
141000100520           DOW  not %eof(TICPN05L) and wNota <= 2;
141100100520             IF  wNota = 1;
141200100520               pNota01 = %subst(CPNnot:1:45);
141300100520               pNota02 = %subst(CPNnot:46:45);
141400100520             ENDIF;
141500100520             IF  wNota = 2;
141600100520               pNota03 = %subst(CPNnot:1:45);
141700100520               pNota04 = %subst(CPNnot:46:45);
141800100520             ENDIF;
141900100520
142000100520             reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
142100100520             wNota += 1;
142200100520           ENDDO;
142300100520         ENDIF;
142400100505
142500100505         //?Se non ci sono note legate all'attività controllo se ci sono note legate alla trattativa
142600100505         IF  ATCnrv > 0 and pNota01 = *blanks and pNota02 = *blanks and
142700100505                            pNota03 = *blanks and pNota04 = *blanks;
142800100505           CPNksc = ATCksc;
142900100505           CPNnrv = ATCnrv;
143000100505           setll (CPNcpo:CPNksc:CPNnrv) TICPN04L;
143100100505           reade (CPNcpo:CPNksc:CPNnrv) TICPN04L;
143200100505
143300100505           DOW  not %eof(TICPN04L) and wNota <= 2;
143400100505             IF  wNota = 1;
143500100505               pNota01 = %subst(CPNnot:1:45);
143600100505               pNota02 = %subst(CPNnot:46:45);
143700100505             ENDIF;
143800100505             IF  wNota = 2;
143900100505               pNota03 = %subst(CPNnot:1:45);
144000100505               pNota04 = %subst(CPNnot:46:45);
144100100505             ENDIF;
144200100505
144300100505             reade (CPNcpo:CPNksc:CPNnrv) TICPN04L;
144400100505             wNota += 1;
144500100505           ENDDO;
144600100505         ENDIF;
144700100308
144800100308         //?Salto pagina
144900100506         //IF  CurLine >= 64;
145000100506         IF  *in99;
145100100308           exsr sr_StpSalto;
145200100310           clear wrkriga;
145300100521           write MK89SEPARA;
145400100308           *in99 = *off;
145500100308         ENDIF;
145600100305
145700100521         write MK89RIGA;
145800100309
145900100309       ENDSR;
146000100712
146100100712       //--------------------------------------------------------------
146200100712       //?Stampo i Concorrenti.
146300100712       //--------------------------------------------------------------
146400100712       BEGSR sr_StpConc;
146500100712
146600100712         clear pConc;
146700100712         wConc = 1;
146800100712
146900100712         //?Cerco concorrenti
147000100712         setll (ATCcpo:'10') TICPI01L;
147100100712         reade (ATCcpo:'10') TICPI01L;
147200100712         DOW not %eof(TICPI01L);
147300100712           IF  CPIval > 0 and CPIspf <> '_BAR';
147400100712         //?imposto il campo da stampare
147500100712             IF  pConc = *blanks;
147600100712               pConc = 'Corrieri Utilizzati:';
147700100712             ENDIF;
147800100712             clear TIBS02ds;
147900100712             clear dIFS;
148000100712             T02mod = 'C';
148100100712             T02cod = 'IFS';
148200100712             T02ke1 = CPItpf;
148300100712             T02ke2 = CPIspf;
148400100712             T02sif = KNSIF;
148500100712             TNTBE_RicercaControllo (kpjba : tibs02ds);
148600100712             IF  T02err = *blanks;
148700100712               dIFS = T02uni;
148800100712             ENDIF;
148900100712             IF  wConc > 1;
149000100712             //?Aggiungo concorrente fino al massimo di 186 byte
149100100712               IF  (%len(%trim(pConc)) + %len(%trim(§IFSdes)) + 2) > 185;
149200100712                 Pconc = %trim(Pconc) + ' Altri...';
149300100712                 leave;
149400100712               ENDIF;
149500100712               pConc = %trim(pConc) + ',';
149600100712             ENDIF;
149700100712             pConc = %trim(pConc) + ' ' + §IFSdes;
149800100712             wConc += 1;
149900100712           ENDIF;
150000100712           reade (ATCcpo:'10') TICPI01L;
150100100712         ENDDO;
150200100712
150300100712         //?Salto pagina
150400100712         IF  CurLine >= 65;
150500100712           exsr sr_StpSalto;
150600100712           *in99 = *off;
150700100712         ENDIF;
150800100712
150900100712         IF  pConc <> *blanks;
151000100712           write MK89CONC;
151100100712         ENDIF;
151200100712
151300100712       ENDSR;
151400100712
151500100309
151600100309       //--------------------------------------------------------------
151700100309       //?Stampo le attività a dettaglio UFFICIO.
151800100309       //--------------------------------------------------------------
151900100309       BEGSR sr_StpDetUFF;
152000100309
152100100309         clear pAttUff;
152200100309
152300100309         //?decodifico attività
152400100309         clear TIBS02ds;
152500100309         clear dCCO;
152600100309         T02mod = 'C';
152700100309         T02cod = 'CCO';
152800100309         T02ke1 = ATCcad;
152900100309         T02sif = KNSIF;
153000100309         TNTBE_RicercaControllo (kpjba : tibs02ds);
153100100309         IF  T02err = *blanks;
153200100309           dCCO = T02uni;
153300100309         ENDIF;
153400100309
153500100309         //?CHIODO --> non stampo la decodifica della causale se FERIE
153600100309         IF  ATCtat <> 'F';
153700100309           pAttUff = §CCOdes;
153800100309           pAttUff = %trim(pAttUff) + ' Dalle ' +
153900100309                     %subst(%editc(ATCoii:'X'):1:2) +
154000100309                     ':' + %subst(%editc(ATCoii:'X'):3:2) +
154100100309                     ' Alle ' +
154200100309                     %subst(%editc(ATCofi:'X'):1:2) + ':' +
154300100309                     %subst(%editc(ATCofi:'X'):3:2);
154400100309         ELSE;
154500100309           pAttUff = 'Dalle ' +
154600100309                     %subst(%editc(ATCoii:'X'):1:2) +
154700100309                     ':' + %subst(%editc(ATCoii:'X'):3:2) +
154800100309                     ' Alle ' +
154900100309                     %subst(%editc(ATCofi:'X'):1:2) + ':' +
155000100309                     %subst(%editc(ATCofi:'X'):3:2);
155100100309         ENDIF;
155200100712
155300100712         //?Note attività (1 riga)
155400100712         clear pNota01;
155500100712         clear pNota02;
155600100712
155700100712         //?Leggo le note legate al progressivo attuale
155800100712         wNota = 1;
155900100712         clear CPNcpo;
156000100712         CPNatn  = ATCatn;
156100100712         CPNatnp = ATCatnp;
156200100712         setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
156300100712         reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
156400100712
156500100712         DOW  not %eof(TICPN05L) and wNota <= 1;
156600100712           IF  wNota = 1;
156700100712             pNota01 = %subst(CPNnot:1:45);
156800100712             pNota02 = %subst(CPNnot:46:45);
156900100712           ENDIF;
157000100712
157100100712           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
157200100712           wNota += 1;
157300100712         ENDDO;
157400100712
157500100712         //?Se non ho trovato note, provo con note legate al progressivo -1
157600100712         IF  pNota01 = *blanks and pNota02 = *blanks;
157700100712           CPNatnp = ATCatnp - 1;
157800100712           setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
157900100712           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
158000100712
158100100712           DOW  not %eof(TICPN05L) and wNota <= 1;
158200100712             IF  wNota = 1;
158300100712               pNota01 = %subst(CPNnot:1:45);
158400100712               pNota02 = %subst(CPNnot:46:45);
158500100712             ENDIF;
158600100712
158700100712             reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
158800100712             wNota += 1;
158900100712           ENDDO;
159000100712         ENDIF;
159100100309
159200100309         //?Salto pagina
159300100309         IF  *in99;
159400100309           exsr sr_StpSalto;
159500100309           *in99 = *off;
159600100309         ENDIF;
159700100309
159800100521         write MK89RIGAUF;
159900100309
160000100309       ENDSR;
160100100309
160200100309       //--------------------------------------------------------------
160300100309       //?Stampo l'eventuale affiancamento.
160400100309       //--------------------------------------------------------------
160500100309       BEGSR sr_StpAffianca;
160600100309
160700100309         clear pAffianca;
160800100309         wAff = 1;
160900100309
161000100309         SELECT;
161100100309         //?se appuntamento (CHIODO 91 e A)
161200100309           WHEN  ATCtat = 'A' and ATCcad = '91';
161300100309         //?controllo se c'è affiancamento
161400100309             ATLatl  = ATCatn;
161500100309             ATLatlp = ATCatnp;
161600100309             setll (ATLatl:ATLatlp) TIATL02L;
161700100309             reade (ATLatl:ATLatlp) TIATL02L;
161800100309             DOW  not %eof(TIATL02L);
161900100309         //?imposto il campo da stampare
162000100309               IF  pAffianca = *blanks;
162100100705                 pAffianca = '  Affiancato da:';
162200100309               ENDIF;
162300100309         //?leggo attività affiancamento e imposto il nome dell'agente sul campo da stampare
162400100309               w_ATCtat  = ATCtat;
162500100309               w_ATCatn  = ATLatn;
162600100309               w_ATCatnp = ATLatnp;
162700100309               chain (w_ATCtat:w_ATCatn:w_ATCatnp) TIATC01L;
162800100309               IF  %found(TIATC01L);
162900100309                 wCmm = w_ATCcmm;
163000100309                 exsr sr_DecCmm;
163100100309                 IF  wAff > 1;
163200100319             //?Aggiungo nome agente fino al massimo di 196 byte
163300130917                   IF  (%len(%trim(pAffianca)) + %len(%trim(CMMdes)) + 2) > 196;
163400100309                     leave;
163500100309                   ENDIF;
163600100309                   pAffianca = %trim(pAffianca) + ',';
163700100309                 ENDIF;
163800130917                 pAffianca = %trim(pAffianca) + ' ' + CMMdes;
163900100309               ENDIF;
164000100309               wAff += 1;
164100100309               reade (ATLatl:ATLatlp) TIATL02L;
164200100309             ENDDO;
164300100309         //?Stampo l'affiancamento
164400100309             IF  pAffianca <> *blanks;
164500100309             //?Salto pagina
164600100309               IF  *in99;
164700100309                 exsr sr_StpSalto;
164800100309                 *in99 = *off;
164900100309               ENDIF;
165000100521               write MK89AFF;
165100100309             ENDIF;
165200100309
165300100309         //?se affiancamento (CHIODO 92 e A)
165400100309           WHEN  ATCtat = 'A' and ATCcad = '92';
165500100309         //?controllo l'affiancamento
165600100309             ATLatn  = ATCatn;
165700100309             ATLatnp = ATCatnp;
165800100309             chain (ATLatn:ATLatnp) TIATL01L;
165900100309             IF  %found(TIATL01L);
166000100309         //?imposto il campo da stampare
166100100309               IF  pAffianca = *blanks;
166200100705                 pAffianca = '  In affiancamento a:';
166300100309               ENDIF;
166400100309         //?leggo attività affiancamento e imposto il nome dell'agente sul campo da stampare
166500100309               w_ATCtat  = ATCtat;
166600100309               w_ATCatn  = ATLatl;
166700100309               w_ATCatnp = ATLatlp;
166800100309               chain (w_ATCtat:w_ATCatn:w_ATCatnp) TIATC01L;
166900100309               IF  %found(TIATC01L);
167000100309                 wCmm = w_ATCcmm;
167100100309                 exsr sr_DecCmm;
167200130917                 pAffianca = %trim(pAffianca) + ' ' + CMMdes;
167300100309               ENDIF;
167400100309             ENDIF;
167500100309         //?Stampo l'affiancamento
167600100309             IF  pAffianca <> *blanks;
167700100309             //?Salto pagina
167800100309               IF  *in99;
167900100309                 exsr sr_StpSalto;
168000100309                 *in99 = *off;
168100100309               ENDIF;
168200100521               write MK89AFF;
168300100309             ENDIF;
168400100309
168500100309         ENDSL;
168600100309
168700100309       ENDSR;
168800080206
168900100521
169000100521       //--------------------------------------------------------------
169100100521       //?Stampa Statistica Consuntiva
169200100521       //--------------------------------------------------------------
169300100521       BEGSR Stat_consun ;
169400100521
169500100521       // cerco il record del consuntivo
169600100521
169700100521       wcmm = $ageu(zz) ;
169800100521         exec sql
169900100521          declare CON cursor for
170000100521          select *
170100100521          from WFRCA00f where RCArgf = :wcmm      ;
170200100521
170300100521         exec sql
170400100521           open CON;
170500100521           IF sqlcode >= 0;
170600100521
170700100521           exec sql
170800100521             fetch next from CON into :WFRCA00F ;
170900100521             IF sqlcod = 100 or sqlcod < 0;
171000100521             Else ;
171100100524        // stampo consuntivo per commerciale unificante se non escluso
171200100524           select ;
171300100524         // se omessi gli interni
171400101006            when i88cscmm = 'O'  and i88cscmmie = 'I' and (RCAFUN = 'COMIN'
171500101006                 or RCAfun = 'ASC' or RCAfun = 'SA');
171600100524         // se omessi gli esterni
171700101006            when i88cscmm = 'O'  and i88cscmmie = 'E' and RCAFUN <> 'COMIN'
171800101006                 and RCAfun <> 'ASC' and RCAfun <> 'SA';
171900100524         // se inclusi gli esterni
172000101006            when i88cscmm = 'I'  and i88cscmmie = 'E' and (RCAFUN =  'COMIN'
172100101006                 or RCAfun = 'ASC' or RCAfun = 'SA');
172200100524         // se inclusi gli interni
172300101006            when i88cscmm = 'I'  and i88cscmmie = 'I' and RCAFUN <> 'COMIN'
172400101006                 and RCAfun <> 'ASC' and RCAfun <> 'SA';
172500100524          other ;
172600100521
172700110422         // preparo le causali da stampare
172800110426           $caastp = $stp;
172900110426           causalids = Cauwds ;
173000110426           cauineds = Cauwds ;
173100110428         // pulisco i campi in stampa e di lavoro
173200110426           clear Qtapmds ;
173300110426           clear Qtapgds ;
173400110426           clear Qtappds ;
173500110426           clear Qtacfds ;
173600110426           clear Qtaptds ;
173700110426           clear Qtactds ;
173800110426           clear Qtapptds ;
173900110426           clear Qtainds ;
174000110428
174100110428           clear Qtapmwds ;
174200110428           clear Qtapgwds ;
174300110428           clear Qtappwds ;
174400110428           clear Qtacfwds ;
174500110428           clear Qtaptwds ;
174600110428           clear Qtactwds ;
174700110428           clear Qtapptwds ;
174800110428           clear Qtainwds ;
174900110422
175000110422         // telefonate potenziali mai trattati
175100110422           Rcacaapmds = Rcacaapm ;
175200110422           Rcanrapmds = Rcanrapm ;
175300110422
175400110422           xx = 1 ;
175500110422           For xx by 1 to %elem($cau) ;
175600110426               contacau = $cau(xx);
175700110422               If uno <> ' ';
175800110422                  xpm = %lookup(uno: $Caapm) ;
175900110422                  If xpm > 0;
176000110422                     $qtaPm(xx) += $Nrapm(xpm) ;
176100110422                  Endif ;
176200110422               Endif ;
176300110422               If due <> ' ';
176400110422                  xpm = %lookup(due: $Caapm) ;
176500110422                  If xpm > 0;
176600110422                     $qtaPm(xx) += $Nrapm(xpm) ;
176700110422                  Endif ;
176800110422               Endif ;
176900110422           Endfor ;
177000110422           Qtapmds = Qtapmwds ;
177100110422
177200110422         // telefonate potenziali già trattati
177300110422           Rcacaapgds = Rcacaapg ;
177400110422           Rcanrapgds = Rcanrapg ;
177500110422
177600110422           xx = 1 ;
177700110422           For xx by 1 to %elem($cau) ;
177800110426               contacau = $cau(xx);
177900110422               If uno <> ' ';
178000110422                  xpg = %lookup(uno: $Caapg) ;
178100110422                  If xpg > 0;
178200110422                     $qtaPg(xx) += $Nrapg(xpg) ;
178300110422                  Endif ;
178400110422               Endif ;
178500110422               If due <> ' ';
178600110422                  xpg = %lookup(due: $Caapg) ;
178700110422                  If xpg > 0;
178800110422                     $qtaPg(xx) += $Nrapg(xpg) ;
178900110422                  Endif ;
179000110422               Endif ;
179100110422           Endfor ;
179200110422           Qtapgds = Qtapgwds ;
179300110422
179400110422         // telefonate potenziali persi
179500110422           Rcacaappds = Rcacaapp ;
179600110422           Rcanrappds = Rcanrapp ;
179700110422
179800110422           xx = 1 ;
179900110422           For xx by 1 to %elem($cau) ;
180000110426               contacau = $cau(xx);
180100110422               If uno <> ' ';
180200110422                  xpp = %lookup(uno: $Caapp) ;
180300110422                  If xpp > 0;
180400110422                     $qtaPp(xx) += $Nrapp(xpp) ;
180500110422                  Endif ;
180600110422               Endif ;
180700110422               If due <> ' ';
180800110422                  xpp = %lookup(due: $Caapp) ;
180900110422                  If xpp > 0;
181000110422                     $qtaPp(xx) += $Nrapp(xpp) ;
181100110422                  Endif ;
181200110422               Endif ;
181300110422           Endfor ;
181400110422           Qtappds = Qtappwds ;
181500110422
181600110422         // telefonate clienti fuori trattativa
181700110422           Rcacaacfds = Rcacaacf ;
181800110422           Rcanracfds = Rcanracf ;
181900110422
182000110422           xx = 1 ;
182100110422           For xx by 1 to %elem($cau) ;
182200110426               contacau = $cau(xx);
182300110422               If uno <> ' ';
182400110422                  xcf = %lookup(uno: $Caapp) ;
182500110422                  If xcf > 0;
182600110422                     $qtaCf(xx) += $Nracf(xcf) ;
182700110422                  Endif ;
182800110422               Endif ;
182900110422               If due <> ' ';
183000110422                  xcf = %lookup(due: $Caacf) ;
183100110422                  If xcf > 0;
183200110422                     $qtaCf(xx) += $Nracf(xcf) ;
183300110422                  Endif ;
183400110422               Endif ;
183500110422           Endfor ;
183600110422           Qtacfds = Qtacfwds ;
183700110422
183800110422         // telefonate potenziali dentro trattativa
183900110422           Rcacaaptds = Rcacaapt ;
184000110422           Rcanraptds = Rcanrapt ;
184100110422
184200110422           xx = 1 ;
184300110422           For xx by 1 to %elem($cau) ;
184400110426               contacau = $cau(xx);
184500110422               If uno <> ' ';
184600110422                  xpt = %lookup(uno: $Caapt) ;
184700110422                  If xpt > 0;
184800110422                     $qtaPt(xx) += $Nrapt(xpt) ;
184900110422                  Endif ;
185000110422               Endif ;
185100110422               If due <> ' ';
185200110422                  xpt = %lookup(due: $Caapt) ;
185300110422                  If xpt > 0;
185400110422                     $qtaPt(xx) += $Nrapt(xpt) ;
185500110422                  Endif ;
185600110422               Endif ;
185700110422           Endfor ;
185800110422           Qtaptds = Qtaptwds ;
185900110422
186000110422         // telefonate clienti    dentro trattativa
186100110422           Rcacaactds = Rcacaact ;
186200110422           Rcanractds = Rcanract ;
186300110422
186400110422           xx = 1 ;
186500110422           For xx by 1 to %elem($cau) ;
186600110426               contacau = $cau(xx);
186700110422               If uno <> ' ';
186800110422                  xct = %lookup(uno: $Caact) ;
186900110422                  If xct > 0;
187000110422                     $qtaCt(xx) += $Nract(xct) ;
187100110422                  Endif ;
187200110422               Endif ;
187300110422               If due <> ' ';
187400110422                  xct = %lookup(due: $Caact) ;
187500110422                  If xct > 0;
187600110422                     $qtact(xx) += $Nract(xct) ;
187700110422                  Endif ;
187800110422               Endif ;
187900110422           Endfor ;
188000110422           Qtactds = Qtactwds ;
188100110422
188200110422         // telefonate potenziali persi dentro trattativa
188300110422           Rcacaapptds = Rcacaappt ;
188400110422           Rcanrapptds = Rcanrappt ;
188500110422
188600110422           xx = 1 ;
188700110422           For xx by 1 to %elem($cau) ;
188800110426               contacau = $cau(xx);
188900110422               If uno <> ' ';
189000110422                  xppt = %lookup(uno: $Caappt) ;
189100110422                  If xppt > 0;
189200110422                     $qtaPpt(xx) += $Nrappt(xppt) ;
189300110422                  Endif ;
189400110422               Endif ;
189500110422               If due <> ' ';
189600110422                  xppt = %lookup(due: $Caappt) ;
189700110422                  If xppt > 0;
189800110422                     $qtaPpt(xx) += $Nrappt(xppt) ;
189900110422                  Endif ;
190000110422               Endif ;
190100110422           Endfor ;
190200110422           Qtapptds = Qtapptwds ;
190300110422
190400110422         // telefonate inevase
190500110422           Rcacaaids = Rcacaai ;
190600110422           Rcanraids = Rcanrai ;
190700110422
190800110422           xx = 1 ;
190900110422           For xx by 1 to %elem($cau) ;
191000110426               contacau = $cau(xx);
191100110422               If uno <> ' ';
191200110422                  xin = %lookup(uno: $Caai) ;
191300110422                  If xin > 0;
191400110422                     $qtain(xx) += $Nrai(xin) ;
191500110422                  Endif ;
191600110422               Endif ;
191700110422               If due <> ' ';
191800110422                  xin = %lookup(due: $Caai) ;
191900110422                  If xin > 0;
192000110422                     $qtain(xx) += $Nrai(xin) ;
192100110422                  Endif ;
192200110422               Endif ;
192300110422           Endfor ;
192400110422           Qtainds = Qtainwds ;
192500110422
192600100622        // totali appuntamenti
192700110119           totapppr = Rcaapptn + Rcaappta + Rcaapptm + Rcaapptc +
192800110119                      Rcaappti + Rcaapptr ;
192900100629        // totali appuntamenti di cui il primo
193000110119           totapppr1 = Rcapatn + Rcapata  + Rcapatm + Rcapatc  +
193100110119                       Rcapati + Rcapatr  ;
193200100628        // totali telefonate post offerta di altri
193300100628           Tottepoa = (rcatepo30a + Rcatepo33a) ;
193400110426        // totali telefonate caldi di altri
193500110428           Tottecalda = (rcatepc31a + Rcatepc44a) ;
193600100819        // totali appuntamenti eseguiti
193700110119           totappes = Rcaaptn + Rcaapta + Rcaaptm + Rcaaptc +
193800110119                      Rcaapti + Rcaaptr ;
193900100819        // totali affiancamenti eseguiti
194000110119           totaffes = Rcaaftn + Rcaafta + Rcaaftm + Rcaaftc +
194100110119                      Rcaafti + Rcaaftr  ;
194200100521           tipostampa = 'C' ;
194300100524         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
194400100524            *in11 = *on ;
194500100524
194600100524         //?Imposto data inizio e fine stampa
194700100524            wDate_ISO = %date(I88deci:*iso);
194800100524            wDate_EUR = wDate_ISO;
194900100524            pDatai    = %dec(wDate_EUR);
195000100524            wDate_ISO = %date(I88decf:*iso);
195100100524            wDate_EUR = wDate_ISO;
195200100524            pDataf    = %dec(wDate_EUR);
195300100622          // calcolo l'incidenza  degli appuntamenti dulle telefonate effettuate
195400100622            exsr sr_incidenza ;
195500100622          // prima verifico che tutte i fattori siano valorizzati
195600100524            exsr sr_StpSalto;
195700100524            write mk89rigcon ;
195800100524           ENDSL;
195900100521          endif ;
196000100521          endif ;
196100100521
196200100521         exec sql close CON;
196300100521
196400100521       ENDSR;
196500100521
196600100622       //--------------------------------------------------------------
196700100622       //?Calcolo incidenza appuntamenti presi su telefonate Potenziali
196800100622       //--------------------------------------------------------------
196900100622       BEGSR Sr_incidenza ;
197000100622
197100100622          // prima verifico che tutti i fattori siano valorizzati
197200110422         If Rcapatn > 0 and (rcatepm > 0 or rcatepg > 0 or rcatepp > 0 ) ;
197300110422            Incide = ((100 * rcapatn) / (rcatepm + rcatepg + rcatepp)) ;
197400100622         else ;
197500100622            clear Incide ;
197600100622         endif ;
197700100622
197800100622       ENDSR;
197900100622
198000100522       //--------------------------------------------------------------
198100100522       //?Stampa Statistica Preventiva
198200100522       //--------------------------------------------------------------
198300100522       BEGSR Stat_preven ;
198400100522
198500100522       // cerco il record del preventivo
198600100524         wrkriga1= '*' + wTrat + '*';
198700100522
198800100522       wcmm = $ageu(zz) ;
198900100522         exec sql
199000100522          declare PRE cursor for
199100100522          select *
199200100522          from WFRPA00f where RPArgf = :wcmm      ;
199300100522
199400100522         exec sql
199500100522           open PRE;
199600100522           IF sqlcode >= 0;
199700100522
199800100522           exec sql
199900100522             fetch next from PRE into :WFRPA00F ;
200000100522             IF sqlcod = 100 or sqlcod < 0;
200100100522             Else ;
200200100524        // stampo preventivo per commerciale unificante se non escluso
200300100524           select ;
200400100524         // se omessi gli interni
200500101006            when i88cscmm = 'O'  and i88cscmmie = 'I' and (RPAFUN = 'COMIN'
200600101006                 or RPAfun = 'ASC' or RPAfun = 'SA');
200700100524         // se omessi gli esterni
200800101006            when i88cscmm = 'O'  and i88cscmmie = 'E' and RPAFUN <> 'COMIN'
200900101006                 and RPAfun <> 'ASC' and RPAfun <> 'SA';
201000100524         // se inclusi gli esterni
201100101006            when i88cscmm = 'I'  and i88cscmmie = 'E' and (RPAFUN =  'COMIN'
201200101006                 or RPAfun = 'ASC' or RPAfun = 'SA');
201300100524         // se inclusi gli interni
201400101006            when i88cscmm = 'I'  and i88cscmmie = 'I' and RPAFUN <> 'COMIN'
201500101006                 and RPAfun <> 'ASC' and RPAfun <> 'SA';
201600100524          other ;
201700100524         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
201800100524            *in11 = *on ;
201900100524
202000100524         //?Imposto data inizio e fine stampa
202100100527            wDate_ISO = %date(I88depi:*iso);
202200100524            wDate_EUR = wDate_ISO;
202300100524            pDatai    = %dec(wDate_EUR);
202400100527            wDate_ISO = %date(I88depf:*iso);
202500100524            wDate_EUR = wDate_ISO;
202600100524            pDataf    = %dec(wDate_EUR);
202700100524
202800100522           tipostampa = 'P' ;
202900100522           exsr sr_StpSalto;
203000100522        // calcolo totali appuntamenti
203100110119           totapp = (rpaappm + rpaappn + rpaappa + rpaappc +
203200110119                     rpaappi + rpaappr);
203300100522        // calcolo totali telefonate
203400100524           tottel = (rpatc19 + rpatc20 + rpatc21 + rpatc22 + rpatc24 +
203500100524                     rpatc25 + rpatc26 + rpatc27 + rpatc28 + rpatc30 +
203600110422                     rpatc31 + rpatc32 + rpatc33 + rpatc44 + rpatc60 +
203700110422                     rpatc70 + rpatc71 + rpatc73 + rpatc74 + rpatc75);
203800100524        // Verifico se affiancamenti valorizzati
203900110119           *in30 = rpaaffn > 0 ;
204000110119           *in31 = rpaaffi > 0 ;
204100110119           *in32 = rpaaffa > 0 ;
204200110119           *in33 = rpaaffm > 0 ;
204300110119           *in34 = rpaaffc > 0 ;
204400110119           *in35 = rpaaffr > 0 ;
204500100524           write Mk89rigpre ;
204600100526           write Mk89rigpr1 ;
204700100524          ENDSL ;
204800100522          endif ;
204900100522          endif ;
205000100522
205100100522         exec sql close PRE;
205200100522
205300100522       ENDSR;
205400100522
205500100525       //--------------------------------------------------------------
205600100525       //?Stampa Totali generali Statistica
205700100525       //--------------------------------------------------------------
205800100526       BEGSR SR_Totgen  ;
205900100525
206000110426         // pulisco i campi in stampa
206100110426           clear Qtapmds ;
206200110426           clear Qtapgds ;
206300110426           clear Qtappds ;
206400110426           clear Qtacfds ;
206500110426           clear Qtaptds ;
206600110426           clear Qtactds ;
206700110426           clear Qtapptds ;
206800110426           clear Qtainds ;
206900110428           clear Qtapmwds ;
207000110428           clear Qtapgwds ;
207100110428           clear Qtappwds ;
207200110428           clear Qtacfwds ;
207300110428           clear Qtaptwds ;
207400110428           clear Qtactwds ;
207500110428           clear Qtapptwds ;
207600110428           clear Qtainwds ;
207700110426
207800110426         // preparo le causali da stampare
207900110426           $caastp = $stp;
208000110426           causalids = Cauwds ;
208100110426           cauineds = Cauwds ;
208200110426
208300100528         wrkriga1= '*' + wTrat + '*';
208400100622         *in20 = *on ;
208500100525       // verifico se ho lanciato il consuntivo
208600100526       If i88tcpe = 'C' or i88tcpe = 'E' ;
208700100525       //?Ciclo per sk unificanti e stampo
208800100525           zz = 1;
208900100526           FOR zz by 1 to %elem($AgeU);
209000100526           IF  $AgeU(zz) <> *zeros;
209100100525
209200100525               wcmm = $ageu(zz) ;
209300100525               exec sql
209400100525                 declare COG cursor for
209500100525                 select *
209600100525                 from WFRCA00f where RCArgf = :wcmm      ;
209700100525
209800100525                 exec sql
209900100525                 open COG;
210000100526                 IF sqlcode >= 0;
210100100525
210200100525                    exec sql
210300100525                    fetch next from COG into :WFRCA00F ;
210400100526                    IF sqlcod = 100 or sqlcod < 0;
210500100525                    Else ;
210600100525        // stampo statistica generale facendo attenzione alle inclusioni/esclusioni
210700100525                     select ;
210800100525         // se omessi gli interni
210900101007                       when i88tscmm='O' and i88tscmmie='I' and (RCAFUN='COMIN'
211000101007                       or RCAfun = 'ASC' or RCAfun = 'SA');
211100100526         // se omessi gli esterni
211200101007                       when i88tscmm='O' and i88tscmmie='E' and RCAFUN<>'COMIN'
211300101007                       and RCAfun <> 'ASC' and RCAfun <> 'SA';
211400100525         // se inclusi gli esterni
211500101007                       when i88tscmm='I' and i88tscmmie='E' and (RCAFUN='COMIN'
211600101007                       or RCAfun = 'ASC' or RCAfun = 'SA');
211700100525         // se inclusi gli interni
211800101007                       when i88tscmm='I' and i88tscmmie='I' and RCAFUN<>'COMIN'
211900101007                       and RCAfun <> 'ASC' and RCAfun <> 'SA';
212000100525                     other ;
212100110426
212200110426         // telefonate potenziali mai trattati
212300110426           Wrkcaads = Rcacaapm ;
212400110426           Wrknrads = Rcanrapm ;
212500110426
212600110426           xx = 1 ;
212700110426           For xx by 1 to %elem($cau) ;
212800110426               contacau = $cau(xx);
212900110426               If uno <> ' ';
213000110426                  yy = %lookup(uno: $wrkCaa) ;
213100110426                  If yy > 0;
213200110426                     $qtaPm(xx) += $WrkNra(yy) ;
213300110426                  Endif ;
213400110426               Endif ;
213500110426               If due <> ' ';
213600111017                  yy  = %lookup(due: $wrkCaa) ;
213700110426                  If yy > 0;
213800110426                     $qtaPm(xx) += $WrkNra(yy) ;
213900110426                  Endif ;
214000110426               Endif ;
214100110426           Endfor ;
214200100525
214300110426         // telefonate potenziali già trattati
214400110426           Wrkcaads = Rcacaapg ;
214500110426           Wrknrads = Rcanrapg ;
214600110426
214700110426           xx = 1 ;
214800110426           For xx by 1 to %elem($cau) ;
214900110426               contacau = $cau(xx);
215000110426               If uno <> ' ';
215100110426                  yy = %lookup(uno: $wrkCaa) ;
215200110426                  If yy > 0;
215300110428                     $qtaPg(xx) += $WrkNra(yy) ;
215400110426                  Endif ;
215500110426               Endif ;
215600110426               If due <> ' ';
215700111017                  yy  = %lookup(due: $wrkCaa) ;
215800110426                  If yy > 0;
215900110428                     $qtaPg(xx) += $WrkNra(yy) ;
216000110426                  Endif ;
216100110426               Endif ;
216200110426           Endfor ;
216300110426
216400110426         // telefonate potenziali persi
216500110426           Wrkcaads = Rcacaapp ;
216600110426           Wrknrads = Rcanrapp ;
216700110426
216800110426           xx = 1 ;
216900110426           For xx by 1 to %elem($cau) ;
217000110426               contacau = $cau(xx);
217100110426               If uno <> ' ';
217200110426                  yy = %lookup(uno: $wrkCaa) ;
217300110426                  If yy > 0;
217400110426                     $qtaPp(xx) += $WrkNra(yy) ;
217500110426                  Endif ;
217600110426               Endif ;
217700110426               If due <> ' ';
217800110426                  yy = %lookup(due: $wrkCaa) ;
217900110426                  If yy > 0;
218000110426                     $qtaPp(xx) += $WrkNra(yy) ;
218100110426                  Endif ;
218200110426               Endif ;
218300110426           Endfor ;
218400100609
218500110426         // telefonate clienti fuori trattativa
218600110426           Wrkcaads = Rcacaacf ;
218700110426           Wrknrads = Rcanracf ;
218800110426
218900110426           xx = 1 ;
219000110426           For xx by 1 to %elem($cau) ;
219100110426               contacau = $cau(xx);
219200110426               If uno <> ' ';
219300110426                  yy = %lookup(uno: $wrkCaa) ;
219400110426                  If yy > 0;
219500110426                     $qtaCf(xx) += $WrkNra(yy) ;
219600110426                  Endif ;
219700110426               Endif ;
219800110426               If due <> ' ';
219900110426                  yy = %lookup(due: $wrkCaa) ;
220000110426                  If yy > 0;
220100110426                     $qtaCf(xx) += $WrkNra(yy) ;
220200110426                  Endif ;
220300110426               Endif ;
220400110426           Endfor ;
220500110426
220600110426         // telefonate potenziali dentro trattativa
220700110426           Wrkcaads = Rcacaapt ;
220800110426           Wrknrads = Rcanrapt ;
220900110426
221000110426           xx = 1 ;
221100110426           For xx by 1 to %elem($cau) ;
221200110426               contacau = $cau(xx);
221300110426               If uno <> ' ';
221400110426                  yy = %lookup(uno: $wrkCaa) ;
221500110426                  If yy > 0;
221600110426                     $qtaPt(xx) += $WrkNra(yy) ;
221700110426                  Endif ;
221800110426               Endif ;
221900110426               If due <> ' ';
222000110426                  yy = %lookup(due: $wrkCaa) ;
222100110426                  If yy > 0;
222200110426                     $qtaPt(xx) += $WrkNra(yy) ;
222300110426                  Endif ;
222400110426               Endif ;
222500110426           Endfor ;
222600110426
222700110426         // telefonate clienti    dentro trattativa
222800110426           Wrkcaads = Rcacaact ;
222900110426           Wrknrads = Rcanract ;
223000110426
223100110426           xx = 1 ;
223200110426           For xx by 1 to %elem($cau) ;
223300110426               contacau = $cau(xx);
223400110426               If uno <> ' ';
223500110426                  yy = %lookup(uno: $wrkCaa) ;
223600110426                  If yy > 0;
223700110426                     $qtaCt(xx) += $WrkNra(yy) ;
223800110426                  Endif ;
223900110426               Endif ;
224000110426               If due <> ' ';
224100110426                  yy = %lookup(due: $wrkCaa) ;
224200110426                  If yy > 0;
224300110426                     $qtaCt(xx) += $WrkNra(yy) ;
224400110426                  Endif ;
224500110426               Endif ;
224600110426           Endfor ;
224700110426
224800110426         // telefonate potenziali persi dentro trattativa
224900110426           Wrkcaads = Rcacaappt ;
225000110426           Wrknrads = Rcanrappt ;
225100110426
225200110426           xx = 1 ;
225300110426           For xx by 1 to %elem($cau) ;
225400110426               contacau = $cau(xx);
225500110426               If uno <> ' ';
225600110426                  yy = %lookup(uno: $wrkCaa) ;
225700110426                  If yy > 0;
225800110426                     $qtaPpt(xx) += $WrkNra(yy) ;
225900110426                  Endif ;
226000110426               Endif ;
226100110426               If due <> ' ';
226200110426                  yy = %lookup(due: $wrkCaa) ;
226300110426                  If yy > 0;
226400110426                     $qtaPpt(xx) += $WrkNra(yy) ;
226500110426                  Endif ;
226600110426               Endif ;
226700110426           Endfor ;
226800110426
226900110426         // telefonate inevase
227000110426           Wrkcaads = Rcacaai ;
227100110426           Wrknrads = Rcanrai ;
227200110426
227300110426           xx = 1 ;
227400110426           For xx by 1 to %elem($cau) ;
227500110426               contacau = $cau(xx);
227600110426               If uno <> ' ';
227700110426                  yy = %lookup(uno: $wrkCaa) ;
227800110426                  If yy > 0;
227900110426                     $qtaIn(xx) += $WrkNra(yy) ;
228000110426                  Endif ;
228100110426               Endif ;
228200110426               If due <> ' ';
228300110426                  yy = %lookup(due: $wrkCaa) ;
228400110426                  If yy > 0;
228500110426                     $qtaIn(xx) += $WrkNra(yy) ;
228600110426                  Endif ;
228700110426               Endif ;
228800110426           Endfor ;
228900110426
229000100526                      T_tepm += rcatepm ;
229100110426                      T_tepg += rcatepg ;
229200110426                      T_tepp += rcatepp ;
229300110426                      T_tecft += rcatecft ;
229400110426                      T_tept += rcatept ;
229500110426                      T_tect += rcatect ;
229600110426                      T_teppt += rcateppt ;
229700110426                      T_tepost += rcatepost ;
229800110426                      T_tepoa   += rcatepo30a;
229900110426                      T_tepoa   += rcatepo33a;
230000110426                      T_tecaldi += rcatecaldi;
230100110426                      T_tecalda += rcatepc31a ;
230200110426                      T_tecalda += rcatepc44a ;
230300110426                      T_teposve += rcateposve ;
230400100526                      T_tepv32a += rcatepv32a ;
230500110426                      T_temante += rcatemante;
230600100526                      T_tepc40a += rcatepc40a;
230700110426                      T_teincre += rcateincre;
230800110426                      T_tepc50a += rcatepc50a;
230900110426                      T_telese  += rcatelese ;
231000110426                      T_inevase += rcainevase;
231100100609                      T_patn    += rcapatn   ;
231200100526                      T_patm    += rcapatm   ;
231300100526                      T_pata    += rcapata   ;
231400100526                      T_patc    += rcapatc   ;
231500110119                      T_pati    += rcapati   ;
231600110119                      T_patr    += rcapatr   ;
231700100622                      T_apptn    += rcaapptn   ;
231800100622                      T_apptm    += rcaapptm   ;
231900100622                      T_appta    += rcaappta   ;
232000100622                      T_apptc    += rcaapptc   ;
232100110119                      T_appti    += rcaappti   ;
232200110119                      T_apptr    += rcaapptr   ;
232300100526                      T_aptn    += rcaaptn   ;
232400100526                      T_aptm    += rcaaptm   ;
232500100526                      T_apta    += rcaapta   ;
232600100526                      T_aptc    += rcaaptc   ;
232700110119                      T_apti    += rcaapti   ;
232800110119                      T_aptr    += rcaaptr   ;
232900100526                      T_aftn    += rcaaftn   ;
233000100526                      T_aftm    += rcaaftm   ;
233100100526                      T_afta    += rcaafta   ;
233200100526                      T_aftc    += rcaaftc   ;
233300110119                      T_afti    += rcaafti   ;
233400110119                      T_aftr    += rcaaftr   ;
233500100526                      T_ap24    += rcaap24   ;
233600100526                      T_feriegg += rcaferiegg;
233700100526                      W_feriehh = Rcaferiehh * 100 ;
233800100526                      W_fra     = Rcaufra * 100 ;
233900100526                      W_frc     = Rcaufrc * 100 ;
234000100526                      W_fuf     = Rcaufuf * 100 ;
234100100527                      wTot_H = %div(w_feriehh : 100);
234200100527                      wTot_R = %rem(w_feriehh : 100);
234300100526                      wTot_M_FP +=(Wtot_H * 60);
234400100527                      wTot_M_FP +=(Wtot_r);
234500100527                      wTot_H = %div(w_fra : 100);
234600100527                      wTot_R = %rem(w_fra : 100);
234700100526                      wTot_M_RA +=(Wtot_H * 60);
234800100527                      wTot_M_RA +=(Wtot_r);
234900100527                      wTot_H = %div(w_frc : 100);
235000100527                      wTot_R = %rem(w_frc : 100);
235100100526                      wTot_M_RC +=(Wtot_H * 60);
235200100527                      wTot_M_RC +=(Wtot_r);
235300100527                      wTot_H = %div(w_fuf : 100);
235400100527                      wTot_R = %rem(w_fuf : 100);
235500100526                      wTot_M_UF +=(Wtot_H * 60);
235600100527                      wTot_M_UF +=(Wtot_r );
235700100629        // totali appuntamenti
235800110119                      T_totapppr=Rcaapptn+Rcaappta+Rcaapptm+Rcaapptc+
235900110119                                 Rcaappti+Rcaapptr+T_totapppr;
236000100629        // totali appuntamenti di cui il primo
236100110119                      T_totapppr1=Rcapatn+Rcapata+Rcapatm+Rcapatc+
236200110119                                  Rcapati+Rcapatr+T_totapppr1  ;
236300100525                     ENDSL;
236400100526                    endif ;
236500100526                 endif ;
236600100706                 exec sql close COG;
236700100526              endif ;
236800100526           ENDFOR;
236900100525
237000110426          // preparo le schiere in stampa
237100110426           Qtapmds = Qtapmwds ;
237200110426           Qtapgds = Qtapgwds ;
237300110426           Qtappds = Qtappwds ;
237400110426           Qtacfds = Qtacfwds ;
237500110426           Qtaptds = Qtaptwds ;
237600110426           Qtactds = QtactWds ;
237700110426           Qtapptds = Qtapptwds ;
237800110426           Qtainds = Qtainwds ;
237900110426
238000100525        // imposto i campi da stampare
238100100526           rcatepm   =T_tepm   ;
238200110426           rcatepg   =T_tepg   ;
238300110428           rcatepp   =T_tepp   ;
238400110426           rcatecft  =T_tecft  ;
238500110426           rcatept   =T_tept   ;
238600110426           rcatect   =T_tect   ;
238700110426           rcateppt  =T_teppt  ;
238800110426           rcatepost =T_tepost ;
238900110426           Tottepoa  =T_tepoa  ;
239000110426           rcatecaldi=T_tecaldi;
239100110426           Tottecalda=T_tecalda;
239200110426           rcateposve=T_teposve;
239300100526           rcatepv32a =T_tepv32a ;
239400110426           rcatemante=T_temante;
239500100526           rcatepc40a =T_tepc40a ;
239600110426           rcateincre=T_teincre;
239700110426           rcatepc50a =T_tepc50a ;
239800110426           rcatelese=T_telese ;
239900110426           rcainevase=T_inevase ;
240000100609           rcapatn    =T_patn    ;
240100100609           rcapatm    =T_patm    ;
240200100609           rcapata    =T_pata    ;
240300100609           rcapatc    =T_patc    ;
240400110119           rcapati    =T_pati    ;
240500110119           rcapatr    =T_patr    ;
240600100622           rcaapptn    =T_apptn    ;
240700100622           rcaapptm    =T_apptm    ;
240800100622           rcaappta    =T_appta    ;
240900100622           rcaapptc    =T_apptc    ;
241000110119           rcaappti    =T_appti    ;
241100110119           rcaapptr    =T_apptr    ;
241200100622           rcaaptn    =T_aptn    ;
241300100622           rcaaptm    =T_aptm    ;
241400100622           rcaapta    =T_apta    ;
241500100622           rcaaptc    =T_aptc    ;
241600110119           rcaapti    =T_apti    ;
241700110119           rcaaptr    =T_aptr    ;
241800100609           rcaaftn    =T_aftn    ;
241900100609           rcaaftm    =T_aftm    ;
242000100609           rcaafta    =T_afta    ;
242100100609           rcaaftc    =T_aftc    ;
242200110119           rcaafti    =T_afti    ;
242300110119           rcaaftr    =T_aftr    ;
242400100609           rcaap24    =T_ap24    ;
242500100609           rcaferiegg=T_feriegg ;
242600100525
242700100525         // ferie
242800100525             wTot_H = %div(wTot_M_FP : 60);
242900100525             wTot_R = %rem(wTot_M_FP : 60);
243000100525             Rcaferiehh = wtot_h;
243100100525             Rcaferiehh += (Wtot_r / 100);
243200100525         // riunioni d'area
243300100525             wTot_H = %div(wTot_M_RA : 60);
243400100525             wTot_R = %rem(wTot_M_RA : 60);
243500100525             Rcaufra    = wtot_h;
243600100525             Rcaufra    += (Wtot_r / 100);
243700100525         // riunioni commerciale
243800100525             wTot_H = %div(wTot_M_RC : 60);
243900100525             wTot_R = %rem(wTot_M_RC : 60);
244000100525             Rcaufrc    = wtot_h;
244100100525             Rcaufrc    += (Wtot_r / 100);
244200100525         // attività ufficio
244300100525             wTot_H = %div(wTot_M_UF : 60);
244400100525             wTot_R = %rem(wTot_M_UF : 60);
244500100525             Rcaufuf    = wtot_h;
244600100525             Rcaufuf    += (Wtot_r / 100);
244700100629        // totali appuntamenti presi
244800100629             Totapppr = T_totapppr ;
244900100629        // totali appuntamenti presi di cui primo
245000100629             Totapppr1 = T_totapppr1 ;
245100100629          // calcolo l'incidenza  degli appuntamenti dulle telefonate effettuate
245200100622            exsr sr_incidenza ;
245300100819        // totali appuntamenti eseguiti
245400110119             Totappes = Rcaaptn + Rcaapta + Rcaaptm + Rcaaptc +
245500110119                        Rcaapti + Rcaaptr ;
245600100819        // totali affiancamenti eseguiti
245700110119             Totaffes = Rcaaftn + Rcaafta + Rcaaftm + Rcaaftc +
245800110119                        Rcaafti + Rcaaftr ;
245900100525
246000100525         //?Imposto data inizio e fine stampa
246100100525           wDate_ISO = %date(I88deci:*iso);
246200100525           wDate_EUR = wDate_ISO;
246300100525           pDatai    = %dec(wDate_EUR);
246400100525           wDate_ISO = %date(I88decf:*iso);
246500100525           wDate_EUR = wDate_ISO;
246600100525           pDataf    = %dec(wDate_EUR);
246700100525         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
246800100525           *in11 = *on ;
246900100525           *in12 = *on ;
247000100527           clear ptatio ;
247100100528         // imposto nel titolo di chi si tratta
247200100609           If (i88cscmm = 'O' and i88cscmmie = ' ') or
247300100610              (i88cscmm = 'I' and i88cscmmie = ' ') OR
247400100610              (i88cscmm = 'I' and i88cscmmie = 'T') ;
247500100609              tipostampa = 'T' ;
247600100528           endif ;
247700100528
247800100528           If (i88cscmm = 'O' and i88cscmmie = 'I') or
247900100528              (i88cscmm = 'I' and i88cscmmie = 'E') ;
248000100609              tipostampa = 'E' ;
248100100528           endif ;
248200100528
248300100528           If (i88cscmm = 'O' and i88cscmmie = 'E') or
248400100528              (i88cscmm = 'I' and i88cscmmie = 'I') ;
248500100609              tipostampa = 'I' ;
248600100528           endif ;
248700100528           *in13 = *on;
248800100525           exsr sr_StpSalto;
248900100525           write mk89rigcon ;
249000100525
249100100525       endif ;
249200100525
249300100526       // verifico se ho lanciato il preventivo
249400100526       If i88tcpe = 'P' or i88tcpe = 'E' ;
249500100526         //?Imposto data inizio e fine stampa
249600100526           wDate_ISO = %date(I88depi:*iso);
249700100526           wDate_EUR = wDate_ISO;
249800100526           pDatai    = %dec(wDate_EUR);
249900100526           wDate_ISO = %date(I88depf:*iso);
250000100526           wDate_EUR = wDate_ISO;
250100100526           pDataf    = %dec(wDate_EUR);
250200100526         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
250300100526           *in11 = *on ;
250400100526           *in12 = *on ;
250500100528           *in13 = *off ;
250600100527            clear ptatio ;
250700100527       //?Ciclo di lettura dei totali preventivi in ordine ragione sociale
250800100527       // elaboro prima i commerciali interni e poi gli esterni se selezionati
250900100527       // i totali di entrambi
251000100527
251100100527         // se inclusi gli interni oppure
251200100527           If (i88tscmm='I' and i88tscmmie='I') or
251300100527         // se omessi gli esterni  oppure
251400100527               (i88tscmm='O' and i88tscmmie='E') or
251500100527         // non ci sono inclusioni e omissioni
251600100609               (i88tscmm='O' and i88tscmmie=' ') or
251700100610               (i88tscmm='I' and i88tscmmie=' ') OR
251800100610               (i88tscmm='I' and i88tscmmie='T') ;
251900100527
252000100527         // pulisco alcuni campoi di totale
252100100527               clear Wtot_m_fp ;
252200100527               clear Wtot_m_uf ;
252300100527               tipostampa = '2' ;
252400100527               *in99 = *on ;
252500100527               $End = *off ;
252600100526               exec sql
252700100527                 declare PRGI cursor for
252800100526                 select *
252900101007                 from WFRPA00f where RPAfun = 'COMIN' or RPAfun = 'ASC'
253000101007                                  or RPAfun = 'SA' order by RPAdrg ;
253100100526
253200100526                 exec sql
253300100527                 open PRGI;
253400100527
253500100527                 IF sqlcode >= 0;
253600100527
253700100527                 Dow not $End ;
253800100526
253900100526                    exec sql
254000100527                    fetch next from PRGI into :WFRPA00F ;
254100100526                    IF sqlcod = 100 or sqlcod < 0;
254200100527                       $End = *on;
254300100527                       leave;
254400100527                    Endif;
254500100526
254600100526        // calcolo totali appuntamenti
254700110119                      totapp = (rpaappm + rpaappn + rpaappa + rpaappc +
254800110119                                rpaappi + rpaappr);
254900100526        // calcolo totali telefonate
255000100526                      tottel=(rpatc19 + rpatc20 + rpatc21 + rpatc22 + rpatc24 +
255100100526                            rpatc25 + rpatc26 + rpatc27 + rpatc28 + rpatc30 +
255200110426                            rpatc31 + rpatc32 + rpatc33 + rpatc44 + rpatc60 +
255300110426                            rpatc70 + rpatc71 + rpatc73 + rpatc74 + rpatc75);
255400100526        // Verifico se affiancamenti valorizzati
255500110119                      *in30 = rpaaffn > 0 ;
255600110119                      *in31 = rpaaffi > 0 ;
255700110119                      *in32 = rpaaffa > 0 ;
255800110119                      *in33 = rpaaffm > 0 ;
255900110119                      *in34 = rpaaffc > 0 ;
256000110119                      *in35 = rpaaffr > 0 ;
256100100526         //?Salto pagina
256200100526                      IF  *in99 = *on   ;
256300100526                          exsr sr_StpSalto;
256400100526                          write Mk89rigpre ;
256500100526                          *in99 = *off;
256600100526                      ENDIF;
256700100526                      write Mk89rigpr1 ;
256800100526                      T_totapp +=totapp ;
256900100526                      T_tottel +=tottel ;
257000100526                      T_appm += rpaappm ;
257100100526                      T_appn += rpaappn ;
257200100526                      T_appa += rpaappa ;
257300100526                      T_appc += rpaappc ;
257400110119                      T_appi += rpaappi ;
257500110119                      T_appr += rpaappr ;
257600100526                      T_affm += rpaaffm ;
257700100526                      T_affn += rpaaffn ;
257800100526                      T_affa += rpaaffa ;
257900100526                      T_affc += rpaaffc ;
258000110119                      T_affi += rpaaffi ;
258100110119                      T_affr += rpaaffr ;
258200100526                      T_atc19 += rpatc19 ;
258300100526                      T_atc20 += rpatc20 ;
258400100526                      T_atc21 += rpatc21 ;
258500100526                      T_atc22 += rpatc22 ;
258600100526                      T_atc24 += rpatc24 ;
258700100526                      T_atc25 += rpatc25 ;
258800100526                      T_atc26 += rpatc26 ;
258900100526                      T_atc27 += rpatc27 ;
259000100526                      T_atc28 += rpatc28 ;
259100100526                      T_atc30 += rpatc30 ;
259200100526                      T_atc31 += rpatc31 ;
259300100526                      T_atc32 += rpatc32 ;
259400100526                      T_atc33 += rpatc33 ;
259500110117                      T_atc44 += rpatc44 ;
259600110426                      T_atc70 += rpatc70 ;
259700110426                      T_atc71 += rpatc71 ;
259800110426                      T_atc73 += rpatc73 ;
259900110426                      T_atc74 += rpatc74 ;
260000110426                      T_atc75 += rpatc75 ;
260100100526                      T_off   += rpaoff   ;
260200100526                      T_fegg  += rpafegg  ;
260300100526                      W_feriehh = Rpaferie * 100 ;
260400100526                      W_fuf     = Rpauff  * 100 ;
260500100527                      wTot_H = %div(w_feriehh : 100);
260600100527                      wTot_R = %rem(w_feriehh : 100);
260700100526                      wTot_M_FP +=(Wtot_H * 60);
260800100527                      wTot_M_FP +=(Wtot_r);
260900100527                      wTot_H = %div(w_fuf : 100);
261000100527                      wTot_R = %rem(w_fuf : 100);
261100100526                      wTot_M_UF +=(Wtot_H * 60);
261200100527                      wTot_M_UF +=(Wtot_r);
261300100527                   ENDDO ;
261400100527
261500100527                   exec sql close PRGI;
261600100526
261700100527                   totapp = T_totapp ;
261800100527                   Tottel = T_tottel ;
261900100527                   rpaappm  = T_appm ;
262000100527                   rpaappn  = T_appn ;
262100100527                   rpaappa  = T_appa ;
262200100527                   rpaappc  = T_appc ;
262300110119                   rpaappi  = T_appi ;
262400110119                   rpaappr  = T_appr ;
262500100527                   rpaaffm  = T_affm ;
262600100527                   rpaaffn  = T_affn ;
262700100527                   rpaaffa  = T_affa ;
262800100527                   rpaaffc  = T_affc ;
262900110119                   rpaaffi  = T_affi ;
263000110119                   rpaaffr  = T_affr ;
263100100527                   rpatc19= T_atc19 ;
263200100527                   rpatc20= T_atc20 ;
263300100527                   rpatc21= T_atc21 ;
263400100527                   rpatc22 = T_atc22 ;
263500100527                   rpatc24 = T_atc24 ;
263600100527                   rpatc25 = T_atc25 ;
263700100527                   rpatc26 = T_atc26 ;
263800100527                   rpatc27 = T_atc27 ;
263900100527                   rpatc28 = T_atc28 ;
264000100527                   rpatc30 = T_atc30 ;
264100100527                   rpatc31 = T_atc31 ;
264200100527                   rpatc32 = T_atc32 ;
264300100527                   rpatc33 = T_atc33 ;
264400110117                   rpatc44 = T_atc44 ;
264500100527                   rpatc60 = T_atc60 ;
264600110426                   rpatc70 = T_atc70 ;
264700110426                   rpatc71 = T_atc71 ;
264800110426                   rpatc73 = T_atc73 ;
264900110426                   rpatc74 = T_atc74 ;
265000110426                   rpatc75 = T_atc75 ;
265100100527                   rpaoff   = T_off   ;
265200100527                   rpafegg  = T_fegg  ;
265300100527               // ferie
265400100527                   wTot_H = %div(wTot_M_FP : 60);
265500100527                   wTot_R = %rem(wTot_M_FP : 60);
265600100527                   Rpaferie = wtot_h;
265700100527                   Rpaferie += (Wtot_r / 100);
265800100527               // attività ufficio
265900100527                   wTot_H = %div(wTot_M_UF : 60);
266000100527                   wTot_R = %rem(wTot_M_UF : 60);
266100100527                   Rpauff     = wtot_h;
266200100527                   Rpauff     += (Wtot_r / 100);
266300100527
266400100527              // Verifico se affiancamenti valorizzati
266500110119                   *in30 = rpaaffn > 0 ;
266600110119                   *in31 = rpaaffi > 0 ;
266700110119                   *in32 = rpaaffa > 0 ;
266800110119                   *in33 = rpaaffm > 0 ;
266900110119                   *in34 = rpaaffc > 0 ;
267000110119                   *in35 = rpaaffr > 0 ;
267100100526         //?Salto pagina
267200100527                   IF  *in99 = *on   ;
267300100527                       exsr sr_StpSalto;
267400100527                       write mk89rigpre ;
267500100527                       *in99 = *off;
267600100527                   ENDIF;
267700100527                   rpadrg = ' TOTALE GENERALE   ' ;
267800100527                   write mk89rigpr1 ;
267900100527                 endif ;
268000100527         endif ;
268100100526
268200100527         // se inclusi gli esterni oppure
268300100527           If (i88tscmm='I' and i88tscmmie='E') or
268400100527         // se omessi gli interni
268500100527               (i88tscmm='O' and i88tscmmie='I') or
268600100527         // non ci sono inclusioni e omissioni
268700100609               (i88tscmm='O' and i88tscmmie=' ') or
268800100610               (i88tscmm='I' and i88tscmmie=' ') OR
268900100610               (i88tscmm='I' and i88tscmmie='T') ;
269000100527
269100100527         // pulisco alcuni campoi di totale
269200100527               clear Wtot_m_fp ;
269300100527               clear Wtot_m_uf ;
269400100527               clear  T_totapp  ;
269500100527               clear  T_tottel  ;
269600100527               clear  T_appm  ;
269700100527               clear  T_appn  ;
269800100527               clear  T_appa  ;
269900100527               clear  T_appc  ;
270000110119               clear  T_appi  ;
270100110119               clear  T_appr  ;
270200100527               clear  T_affm ;
270300100527               clear  T_affn  ;
270400100527               clear  T_affa  ;
270500100527               clear  T_affc  ;
270600110119               clear  T_affi  ;
270700110119               clear  T_affr  ;
270800100527               clear  T_atc19 ;
270900100527               clear  T_atc20  ;
271000100527               clear  T_atc21  ;
271100100527               clear  T_atc22  ;
271200100527               clear  T_atc24  ;
271300100527               clear  T_atc25  ;
271400100527               clear  T_atc26  ;
271500100527               clear  T_atc27  ;
271600100527               clear  T_atc28  ;
271700100527               clear  T_atc30  ;
271800100527               clear  T_atc31  ;
271900100527               clear  T_atc32  ;
272000100527               clear  T_atc33  ;
272100110117               clear  T_atc44  ;
272200100527               clear  T_atc60  ;
272300110426               clear  T_atc70  ;
272400110426               clear  T_atc71  ;
272500110426               clear  T_atc73  ;
272600110426               clear  T_atc74  ;
272700110426               clear  T_atc75  ;
272800100527               clear  T_off    ;
272900100527               clear  T_fegg   ;
273000100527               tipostampa = '3' ;
273100100527               *in99 = *on ;
273200100527               $End = *off ;
273300100527               exec sql
273400100527                 declare PRGE cursor for
273500100527                 select *
273600101007                 from WFRPA00f where RPAfun <> 'COMIN' and RPAfun <> 'ASC'
273700101007                                 and RPAfun <> 'SA' order by RPAdrg ;
273800100527
273900100527                 exec sql
274000100527                 open PRGE;
274100100527                 IF sqlcode >= 0;
274200100527
274300100527                 Dow not $End ;
274400100527
274500100527                    exec sql
274600100527                    fetch next from PRGE into :WFRPA00F ;
274700100527                    IF sqlcod = 100 or sqlcod < 0;
274800100527                       $End = *on;
274900100527                       leave;
275000100527                    Endif;
275100100527
275200100527        // calcolo totali appuntamenti
275300110119                      totapp = (rpaappm + rpaappn + rpaappa + rpaappc +
275400110119                                rpaappi + rpaappr );
275500100527        // calcolo totali telefonate
275600100527                      tottel=(rpatc19 + rpatc20 + rpatc21 + rpatc22 + rpatc24 +
275700100527                            rpatc25 + rpatc26 + rpatc27 + rpatc28 + rpatc30 +
275800110426                            rpatc31 + rpatc32 + rpatc33 + rpatc44 + rpatc60 +
275900110426                            rpatc70 + rpatc71 + rpatc73 + rpatc74 + rpatc75);
276000100527        // Verifico se affiancamenti valorizzati
276100110119                      *in30 = rpaaffn > 0 ;
276200110119                      *in31 = rpaaffi > 0 ;
276300110119                      *in32 = rpaaffa > 0 ;
276400110119                      *in33 = rpaaffm > 0 ;
276500110119                      *in34 = rpaaffc > 0 ;
276600110119                      *in35 = rpaaffr > 0 ;
276700100527         //?Salto pagina
276800100527                      IF  *in99 = *on   ;
276900100527                          exsr sr_StpSalto;
277000100527                          write Mk89rigpre ;
277100100527                          *in99 = *off;
277200100527                      ENDIF;
277300100527                      write Mk89rigpr1 ;
277400100527                      T_totapp +=totapp ;
277500100527                      T_tottel +=tottel ;
277600100527                      T_appm += rpaappm ;
277700100527                      T_appn += rpaappn ;
277800100527                      T_appa += rpaappa ;
277900100527                      T_appc += rpaappc ;
278000110119                      T_appi += rpaappi ;
278100110119                      T_appr += rpaappr ;
278200100527                      T_affm += rpaaffm ;
278300100527                      T_affn += rpaaffn ;
278400100527                      T_affa += rpaaffa ;
278500100527                      T_affc += rpaaffc ;
278600110119                      T_affi += rpaaffi ;
278700110119                      T_affr += rpaaffr ;
278800100527                      T_atc19 += rpatc19 ;
278900100527                      T_atc20 += rpatc20 ;
279000100527                      T_atc21 += rpatc21 ;
279100100527                      T_atc22 += rpatc22 ;
279200100527                      T_atc24 += rpatc24 ;
279300100527                      T_atc25 += rpatc25 ;
279400100527                      T_atc26 += rpatc26 ;
279500100527                      T_atc27 += rpatc27 ;
279600100527                      T_atc28 += rpatc28 ;
279700100527                      T_atc30 += rpatc30 ;
279800100527                      T_atc31 += rpatc31 ;
279900100527                      T_atc32 += rpatc32 ;
280000100527                      T_atc33 += rpatc33 ;
280100110117                      T_atc44 += rpatc44 ;
280200100527                      T_atc60 += rpatc60 ;
280300110426                      T_atc70 += rpatc70 ;
280400110426                      T_atc71 += rpatc71 ;
280500110426                      T_atc73 += rpatc73 ;
280600110426                      T_atc74 += rpatc74 ;
280700110426                      T_atc75 += rpatc75 ;
280800100527                      T_off   += rpaoff   ;
280900100527                      T_fegg  += rpafegg  ;
281000100527                      W_feriehh = Rpaferie * 100 ;
281100100527                      W_fuf     = Rpauff  * 100 ;
281200100527                      wTot_H = %div(w_feriehh : 100);
281300100527                      wTot_R = %rem(w_feriehh : 100);
281400100527                      wTot_M_FP +=(Wtot_H * 60);
281500100527                      wTot_M_FP +=(Wtot_r);
281600100527                      wTot_H = %div(w_fuf : 100);
281700100527                      wTot_R = %rem(w_fuf : 100);
281800100527                      wTot_M_UF +=(Wtot_H * 60);
281900100527                      wTot_M_UF +=(Wtot_r);
282000100527                   ENDDO ;
282100100527
282200100527                     exec sql close PRGE;
282300100527
282400100527                   totapp = T_totapp ;
282500100527                   Tottel = T_tottel ;
282600100527                   rpaappm  = T_appm ;
282700100527                   rpaappn  = T_appn ;
282800100527                   rpaappa  = T_appa ;
282900100527                   rpaappc  = T_appc ;
283000110119                   rpaappi  = T_appi ;
283100110119                   rpaappr  = T_appr ;
283200100527                   rpaaffm  = T_affm ;
283300100527                   rpaaffn  = T_affn ;
283400100527                   rpaaffa  = T_affa ;
283500100527                   rpaaffc  = T_affc ;
283600110119                   rpaaffi  = T_affi ;
283700110119                   rpaaffr  = T_affr ;
283800100527                   rpatc19= T_atc19 ;
283900100527                   rpatc20= T_atc20 ;
284000100527                   rpatc21= T_atc21 ;
284100100527                   rpatc22 = T_atc22 ;
284200100527                   rpatc24 = T_atc24 ;
284300100527                   rpatc25 = T_atc25 ;
284400100527                   rpatc26 = T_atc26 ;
284500100527                   rpatc27 = T_atc27 ;
284600100527                   rpatc28 = T_atc28 ;
284700100527                   rpatc30 = T_atc30 ;
284800100527                   rpatc31 = T_atc31 ;
284900100527                   rpatc32 = T_atc32 ;
285000100527                   rpatc33 = T_atc33 ;
285100110117                   rpatc44 = T_atc44 ;
285200100527                   rpatc60 = T_atc60 ;
285300110426                   rpatc70 = T_atc70 ;
285400110426                   rpatc71 = T_atc71 ;
285500110426                   rpatc73 = T_atc73 ;
285600110426                   rpatc74 = T_atc74 ;
285700110426                   rpatc75 = T_atc75 ;
285800100527                   rpaoff   = T_off   ;
285900100527                   rpafegg  = T_fegg  ;
286000100527               // ferie
286100100527                   wTot_H = %div(wTot_M_FP : 60);
286200100527                   wTot_R = %rem(wTot_M_FP : 60);
286300100527                   Rpaferie = wtot_h;
286400100527                   Rpaferie += (Wtot_r / 100);
286500100527               // attività ufficio
286600100527                   wTot_H = %div(wTot_M_UF : 60);
286700100527                   wTot_R = %rem(wTot_M_UF : 60);
286800100527                   Rpauff     = wtot_h;
286900100527                   Rpauff     += (Wtot_r / 100);
287000100527
287100100527              // Verifico se affiancamenti valorizzati
287200110119                   *in30 = rpaaffn > 0 ;
287300110119                   *in31 = rpaaffi > 0 ;
287400110119                   *in32 = rpaaffa > 0 ;
287500110119                   *in33 = rpaaffm > 0 ;
287600110119                   *in34 = rpaaffc > 0 ;
287700110119                   *in35 = rpaaffr > 0 ;
287800100527         //?Salto pagina
287900100527                   IF  *in99 = *on   ;
288000100527                       exsr sr_StpSalto;
288100100527                       write mk89rigpre ;
288200100527                       *in99 = *off;
288300100527                   ENDIF;
288400100527                   rpadrg = ' TOTALE GENERALE   ' ;
288500100527                   write mk89rigpr1 ;
288600100527                 endif ;
288700100527         endif ;
288800100526       endif ;
288900100525       ENDSR;
289000100525
289100080206       //--------------------------------------------------------------
289200080206       //?Reperimento Dati del job (Utente/Operativi).
289300080206       //--------------------------------------------------------------
289400080206       BEGSR DatiJob;
289500080206
289600080206         in(E) §AzUte;
289700080206         if NOT %error;
289800080206           in(E) §DatiUte;
289900080206         endif;
290000080206         if %error or RSut = *blanks;
290100080206           clear TIBS34ds;
290200080206           tibs34r(tibs34ds);
290300080206           in §AzUte;
290400080206           in §DatiUte;
290500080206         endif;
290600100429
290700100429         //?Richiamo pgm di controllo autorizzazioni utente così mi torna
290800100429         //?il codice autorizzazione relativo alle tariffe clienti
290900100429         //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
291000100429         reset TNTAA1DS;
291100100429         ITAA1caut = '§utegtc';
291200100429         kpjbu     = TNTAA1DS;
291300100429         tntaa1c (kpjba);
291400100429         TNTAA1DS = kpjbu;
291500100429
291600150910         //?Se utente abilitato come 'AZ' mi carico
291700150910         //?la sk delle filiali del distretto
291800100429         IF  OTAA1cabi = 'AZ' and DUTpou <> 046;
291900150910           OTAA1cabi = 'DI';
292000100429         ENDIF;
292100100429
292200100429         //?Carico filiali abilitate all'utente
292300100429         //?Non controllo se caricate o no le filiali
292400100429         //?tanto se non ho caricato nessuna filiale e la richiesta
292500100429         //?è per tutti i commerciali, non stampo niente
292600150921         //?Se richiesta un'area carico SOLO le filiali di quell'area
292700150921         IF  I88car > 0;
292800150921             clear TRUL31DS;
292900150921             clear TRUL31DS2;
293000150921             I31abi = 'AO';
293100150921             I31car = I88car ;
293200150921             trul31r (kpjba : trul31ds : trul31ds2);
293300150921         Else ;
293400150921             clear TRUL31DS;
293500150921             clear TRUL31DS2;
293600150921             if Dutpou = 046 ;
293700150921                I31abi = 'AZ'     ;
293800150921             else ;
293900150921                I31abi = OTAA1cabi;
294000150921                I31cdi = DUTdis;
294100150921                I31car = DUTare;
294200150921                I31cpo = DUTpou;
294300150921             Endif;
294400150921             trul31r (kpjba : trul31ds : trul31ds2);
294500150921         Endif;
294600110120         IF  car(2) > *zeros;
294700110120           *in09 = *on;
294800110120         ENDIF;
294900080206
295000080206       ENDSR;
295100100305
295200100305       //--------------------------------------------------------------
295300100305       //?Carico Sk Agenti.
295400100305       //--------------------------------------------------------------
295500100305       BEGSR sr_CarAGE;
295600100305
295700100305         clear xx;
295800100305         clear $Age;
295900100429         $EndAge = *off;
296000100305
296100100305         exec sql
296200100305          declare AGE cursor for
296300130917          select CMMcod, CMMuni
296400130917          from AZCMM00F
296500130917          order by CMMcod;
296600100305
296700100305         exec sql
296800100305           open AGE;
296900100305           IF sqlcode < 0;
297000100305             $EndAge = *on;
297100100305           ENDIF;
297200100305
297300100305         DOW not $EndAge;
297400100305           exec sql
297500130917             fetch next from AGE into :CMMcod, :CMMuni;
297600100305             IF sqlcod = 100 or sqlcod < 0;
297700100305               $EndAge = *on;
297800100305               leave;
297900100305             ENDIF;
298000130917             IF  CMMuni = $AgeU(zz);
298100100305               xx += 1;
298200130917               $Age(xx) = CMMcod;
298300100305             ENDIF;
298400100305         ENDDO;
298500100305
298600100305         exec sql close AGE;
298700100305
298800100305         //?Se non ho cricato niente memorizzo almeno il codice richiesto
298900100305         IF $Age(01) = 0;
299000100429           $Age(01) = $AgeU(zz);
299100100305         ENDIF;
299200100305
299300100305       ENDSR;
299400100429
299500100429       //--------------------------------------------------------------
299600100429       //?Carico sk agenti unificanti.
299700100429       //--------------------------------------------------------------
299800100429       BEGSR sr_CarAgeUni;
299900100429
300000100429         clear xx;
300100100429         clear $AGEU;
300200100429         $EndAge = *off;
300300100429
300400100429           //?Imposto la stringa per SQL
300500130917           wSQL = 'select CMMcod +
300600130917                   from AZCMM00F +
300700130917                   where substr(digits(CMMuni), 1, 3) in (';
300800100429
300900100429           yy = 0;
301000100429           xx = 1;
301100100429           FOR xx by 1 to %elem(pog);
301200100429             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
301300100429               IF yy > 0;
301400100429                 wSQL += ', ';
301500100429               ELSE;
301600100429                 yy = 1;
301700100429               ENDIF;
301800100429               wSQL += '''';
301900100429               wSQL += pog(xx);
302000100429               wSQL += '''';
302100100429             ENDIF;
302200100429           ENDFOR;
302300100505
302400100505         //?Carico TUTTI gli agenti unificanti
302500130917           wSQL += ') and CMMcod = CMMuni +
302600130917                      and CMMpar = '' ''';
302700100429
302800130917         wSQL += ' order by CMMcod +
302900100505                   for fetch only';
303000100429
303100100429         //?Dichiarazione cursore
303200100429         exec sql
303300100429           prepare A1   from :wSQL;
303400100429         exec sql
303500100429           declare AGEU cursor for A1;
303600100429
303700100429         //?Apertura del cursore
303800100429         exec sql
303900100429           open AGEU;
304000100429
304100100429         IF sqlcode < 0;
304200100429           $EndAge = *on;
304300100429         ENDIF;
304400100429
304500100429         yy = 0;
304600100429         DOW not $EndAge;
304700100429           exec sql
304800130917             fetch next from AGEU into :CMMcod;
304900100429           IF sqlcod = 100 or sqlcod < 0;
305000100429             $EndAge = *on;
305100100429             leave;
305200100429           ENDIF;
305300100429
305400130917           yy += 1;
305500130917           $AGEU(yy) = CMMcod;
305600100429
305700100429         ENDDO;
305800100429
305900100429         exec sql
306000100429           close AGEU;
306100100429
306200100429       ENDSR;
306300100305
306400100305       //--------------------------------------------------------------
306500100305       //?Preparazione stringa SQL.
306600100305       //--------------------------------------------------------------
306700100305       BEGSR sr_PrepSQL;
306800100305
306900100308         wSQL = 'select atctat, atccpo, atcksc, +
307000100309                 atcatn, atcatnp, atcdad, atchda, atcnrv, +
307100100628                 atcoii, atcofi, atccad, atcco3, atcdco, +
307200100525                 atccac, atcest +
307300100305                  from TIATC00F +
307400100305                  where atccmm in(';
307500100305
307600100305         yy = 0;
307700100305         xx = 1;
307800100305         FOR xx by 1 to %elem($Age);
307900100305           IF $Age(xx) <> *zeros;
308000100305             IF yy > 0;
308100100305               wSQL += ', ';
308200100305             ELSE;
308300100305               yy = 1;
308400100305             ENDIF;
308500100305             wSQL += %editc($Age(xx):'X');
308600100305           ENDIF;
308700100305         ENDFOR;
308800100305
308900100308         wSQL += ') and atcdad between ' + %editc(IMK86datai:'X') +
309000100308                 ' and ' + %editc(IMK86dataf:'X');
309100100305
309200100305         wSQL += ' order by atcdad, atchda, atctat +
309300100305                   for fetch only';
309400100305
309500100305       ENDSR;
309600100305
309700100305       //--------------------------------------------------------------
309800100305       //?Controllo se il rcd che sto leggendo è OK per l'agenda.
309900100305       //--------------------------------------------------------------
310000100305       BEGSR Sr_CtrRcdOk;
310100100305
310200100305         $RcdOk = *on;
310300100513         // se c'è inclusione o omissione di alcuni tipi attività
310400100513         // controllo
310500100513         SELECT ;
310600100513            When imk86iot = 'I' and %lookup(atctat : $tat) = 0 ;
310700100513                 $RcdOk = *off;
310800100513                 leavesr ;
310900100513            When imk86iot = 'O' and %lookup(atctat : $tat) > 0 ;
311000100513                 $RcdOk = *off;
311100100513                 leavesr ;
311200100513         ENDSL ;
311300100305         //?se attività legata a trattativa
311400100305         //?la trattativa NON deve essere fittizia
311500100305         IF  atcnrv > 0;
311600100305           exec sql
311700100305             select visffz into :visffz from tivis05l
311800100305             where visnrv = : atcnrv;
311900100305         //?non trovo il rcd corrispondente
312000100305           IF  sqlcod <> 0;
312100100305             leavesr;
312200100305           ENDIF;
312300100305         //?se trattativa fittizia non carico a video
312400100305           IF  visffz = 'S';
312500100305             $RcdOk = *off;
312600100305           ENDIF;
312700100305         ENDIF;
312800100525
312900100525         //?CHIODO
313000100712         //?MODIFICA CONTROLLO 06 Luglio 2010 ora controllo solo esito senza causale
313100100712         //?Se appuntamento '91' o affiancamento '92'  e esito 'N'
313200100712         IF  (ATCcad = '91' or ATCcad = '92') and ATCest = 'N';
313300100712           $RcdOk = *off;
313400100712         ENDIF;
313500100525         //?Se attvità qualsiasi chiusa con attività 'AA'
313600100525         //?non devo caricare in agenda
313700100525         IF  ATCcac = 'AA';
313800100525           $RcdOk = *off;
313900100525         ENDIF;
314000100305
314100100305       ENDSR;
314200100309
314300100309       //--------------------------------------------------------------
314400100309       //?Decodifico agente.
314500100309       //--------------------------------------------------------------
314600100309       BEGSR sr_DecCmm;
314700100309
314800130917         chain  (wCmm)  AZCMM000;
314900130917         IF  NOT  %found( AZCMM01L );
315000130917           clear CMMdes;
315100130917           clear CMMfun;
315200100309         ENDIF;
315300100309
315400100309       ENDSR;
315500090807
315600080206       //--------------------------------------------------------------
315700080206       //?Operazioni finali.
315800080206       //--------------------------------------------------------------
315900080206       BEGSR RoutEnd;
316000090521
316100080206         *inLR = *on;
316200080206         return;
316300080206
316400080206       ENDSR;
316500080206
316600080206      /end-free
316700110422** CAUSALI IN STAMPA CONSUNTIVO
31680011042214/15
31690011042219/20
317000110422 21
317100110422 22
317200110422 24
317300110422 25
317400110422 26
317500110422 27
317600110422 28
31770011042230/33
317800110422 32
31790011051831/44
318000110422 41
318100110422 42
318200110422 60
31830011042270/75
318400110422 71
318500110518 72
31860011042273/74
318700110422 80
318800110422 81
318900110422 91
319000110422
319100110531
319200110422** CAUSALI CORRISPONDENTI AD OGNI BUCO DI STAMPA
3193001104221415
3194001104221920
31950011042221
31960011042222
31970011042224
31980011042225
31990011042226
32000011042227
32010011042228
3202001104223033
32030011042232
3204001105183144
32050011042241
32060011042242
32070011042260
3208001104227075
32090011042271
32100011051872
3211001104227374
32120011042280
32130011042281
32140011042291
321500110422
321600110531
