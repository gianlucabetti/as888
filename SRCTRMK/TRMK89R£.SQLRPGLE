000100140715     /*END
000200080206      //--------------------------------------------------------------
000300100713      //?TRMK89R - Stampa Attività commerciale
000400080206      //--------------------------------------------------------------
000500080206
000600100505     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700090807     h dftactgrp(*no) actgrp(*caller)
000800080206
000900080206      //---------------------------------------------------------------
001000080206      //?Dichiarazione file.
001100080206      //---------------------------------------------------------------
001200100308     fTABEL00F  if   e           k disk
001300130917     fAZCMM01L  if   e           k disk
001400100308     fTFNTC01L  if   e           k disk
001500100308     fTIATC01L  if   e           k disk    rename(tiatc000:w_tiatc)
001600100308     f                                     prefix(w_)
001700100309     fTIATL01L  if   e           k disk
001800100309     fTIATL02L  if   e           k disk    rename(tiatl000:tiatl02)
001900100308     fTICPI01L  if   e           k disk
002000100520     fTICPN05L  if   e           k disk
002100100505     fTICPN04L  if   e           k disk    rename(ticpn000:ticpn04)
002200110803     fTIVII01L  if   e           k disk
002300110803     fTIVIS05L  if   e           k disk
002400110803     fTIVOF01L  if   e           k disk
002500100308     fTNCPO01L  if   e           k disk
002600140715     fTNCPO11L  if   e           k disk
002700100528     fTNTBE01L  if   e           k disk
002800100521     fTRMK89P   o    e             printer oflind(*in99)
002900100310     f                                     infds(prtinfo)
003000100310
003100080206      //---------------------------------------------------------------
003200090406      //?Definizione costanti.
003300080206      //---------------------------------------------------------------
003400080206
003500080206      //---------------------------------------------------------------
003600080206      //?Definizione schiere.
003700080206      //---------------------------------------------------------------
003800090806
003900100304      // - Agenti
004000100429     d $Age            s              7  0 dim(3500)
004100100429
004200100429      // - Agenti Unificanti
004300100429     d $AgeU           s              7  0 dim(3500)
004400100513
004500100513     d §MK86TATds      ds            10    inz
004600100513     d   $TAT                  1     10    inz  dim(10)
004700080206
004800110422      // causali da stampare
004900110422     d $STP            s              5    dim(24) ctdata perrcd(1)
005000110422
005100110422      // causali da sommare in stampa
005200110422     d $cau            s              4    dim(24) ctdata perrcd(1)
005300130917
005400080206      //---------------------------------------------------------------
005500080206      //?Definizione aree dati.
005600080206      //---------------------------------------------------------------
005700080206
005800080206      // - Dati utente
005900080206     d §AzUte        e ds                  extname(AZUTE00F)
006000080206     d                                     dtaara
006100080206     d §DatiUte      e ds                  extname(dDatiUte)
006200080206     d                                     dtaara
006300080206
006400080206      //---------------------------------------------------------------
006500080206      //?Definizione strutture dati.
006600080206      //---------------------------------------------------------------
006700100310
006800100310     d PrtINFO         ds
006900100310     d  CurLine              367    368i 0
007000080206
007100080206      // - Status
007200080206     d Psds           sds
007300080206     d   SDSpgm          *proc
007400080206
007500080206      // - Parametri ricevuti
007600080206     d KPJBA         e ds
007700100521     d TRMK88DS      e ds
007800100521     d TRMK86DS      e ds
007900080206
008000100308      // - Ricerca/Controllo tabelle
008100100308     d TIBS02ds      e ds                  inz
008200100308
008300080206      // - Reperimento dati utente
008400100305     d TIBS34DS      e ds
008500100308
008600100308       // - Reperimento dati anagrafici
008700100308     d TIBS69ds      e ds
008800100308     d DS_cnaco      e ds                  inz extname(CNACO00F)
008900100308     d DS_cnind      e ds                  inz extname(CNIND00F)
009000100308     d DS_cnclp      e ds                  inz extname(CNCLP00F)
009100100308     d DS_fncls      e ds                  inz extname(FNCLS00F)
009200100429
009300100429      // - controllo abilitazioni
009400100429     d TNTAA1DS      e ds                  inz
009500100429
009600100429      // - Carico Filiali abilitate all'utente
009700100429     d TRUL31DS      e ds
009800100429     d POG                    10    759    DIM(250)
009900110120      // - Carico Aree abilitate all'utente
010000110120     d TRUL31DS2     e ds
010100110120     d CAR                    22    171    DIM(50)
010200100308
010300100308      // - Tabella CCO = Causale contatto
010400100308     d dCCO          e ds                  inz
010500100712
010600100712      // - Tabella IFS = Sottotipi INFO commerciali
010700100712     d dIFS          e ds                  inz
010800100308
010900100308      // - Tabella TTA = Tipo attività
011000100308     d dTTA          e ds                  inz
011100100308
011200110120      // - Tabella 05 = Codici Area
011300110120     d ds05          e ds                  inz
011400110120
011500100308      // - Tabella 1L = Categoria Merceologica
011600100308     d ds1L          e ds                  inz
011700090807
011800090807      // File attività
011900100305     d TIATC00F      e ds                  extname(TIATC00F)
012000100305
012100130917      *// File trattativa
012200130917     d*// TIVIS00F      e ds                  extname(TIVIS00F)
012300090629
012400100521      // File statistica consuntiva
012500100521     d WFRCA00F      e ds                  extname(WFRCA00F)
012600100521
012700100521      // File statistica preventiva
012800100521     d WFRPA00F      e ds                  extname(WFRPA00F)
012900100522
013000110422
013100110422     d Causalids       ds
013200110422     d  Causa01                       5
013300110422     d  Causa02                       5
013400110422     d  Causa03                       5
013500110422     d  Causa04                       5
013600110422     d  Causa05                       5
013700110422     d  Causa06                       5
013800110422     d  Causa07                       5
013900110422     d  Causa08                       5
014000110422     d  Causa09                       5
014100110422     d  Causa10                       5
014200110422     d  Causa11                       5
014300110422     d  Causa12                       5
014400110422     d  Causa13                       5
014500110422     d  Causa14                       5
014600110422     d  Causa15                       5
014700110422     d  Causa16                       5
014800110422     d  Causa17                       5
014900110422     d  Causa18                       5
015000110422     d  Causa19                       5
015100110422     d  Causa20                       5
015200110422     d  Causa21                       5
015300110422     d  Causa22                       5
015400110422     d  Causa23                       5
015500110422     d  Causa24                       5
015600110426
015700110426     d Cauwds          ds           120    inz
015800110426     d   $CAAstp                      5    inz  dim(24)
015900110422     d
016000110422     d Cauineds        ds
016100110422     d  Cauine01                      5
016200110422     d  Cauine02                      5
016300110422     d  Cauine03                      5
016400110422     d  Cauine04                      5
016500110422     d  Cauine05                      5
016600110422     d  Cauine06                      5
016700110422     d  Cauine07                      5
016800110422     d  Cauine08                      5
016900110422     d  Cauine09                      5
017000110422     d  Cauine10                      5
017100110422     d  Cauine11                      5
017200110422     d  Cauine12                      5
017300110422     d  Cauine13                      5
017400110422     d  Cauine14                      5
017500110422     d  Cauine15                      5
017600110422     d  Cauine16                      5
017700110422     d  Cauine17                      5
017800110422     d  Cauine18                      5
017900110422     d  Cauine19                      5
018000110422     d  Cauine20                      5
018100110422     d  Cauine21                      5
018200110422     d  Cauine22                      5
018300110422     d  Cauine23                      5
018400110422     d  Cauine24                      5
018500110422     d
018600110422     d ContaCau        ds
018700110422     d  uno                           2
018800110422     d  due                           2
018900110422     d
019000110422     d RCACaapmds      ds            70    inz
019100110422     d   $CAAPM                       2    inz  dim(35)
019200110422
019300110422     d RCANrapmds      ds                  inz
019400110422     d   $NRAPM                       5s 0 inz  dim(35)
019500110422
019600110422     d Qtapmds         ds
019700110422     d  Qtapm01                       5  0 inz
019800110422     d  Qtapm02                       5  0 inz
019900110422     d  Qtapm03                       5  0 inz
020000110422     d  Qtapm04                       5  0 inz
020100110422     d  Qtapm05                       5  0 inz
020200110422     d  Qtapm06                       5  0 inz
020300110422     d  Qtapm07                       5  0 inz
020400110422     d  Qtapm08                       5  0 inz
020500110422     d  Qtapm09                       5  0 inz
020600110422     d  Qtapm10                       5  0 inz
020700110422     d  Qtapm11                       5  0 inz
020800110422     d  Qtapm12                       5  0 inz
020900110422     d  Qtapm13                       5  0 inz
021000110422     d  Qtapm14                       5  0 inz
021100110422     d  Qtapm15                       5  0 inz
021200110422     d  Qtapm16                       5  0 inz
021300110422     d  Qtapm17                       5  0 inz
021400110422     d  Qtapm18                       5  0 inz
021500110422     d  Qtapm19                       5  0 inz
021600110422     d  Qtapm20                       5  0 inz
021700110422     d  Qtapm21                       5  0 inz
021800110422     d  Qtapm22                       5  0 inz
021900110422     d  Qtapm23                       5  0 inz
022000110422     d  Qtapm24                       5  0 inz
022100110422
022200110422     d Qtapmwds        ds
022300110422     d   $QtaPM                       5s 0 inz  dim(24)
022400110422     d
022500110422     d RCACaapgds      ds            70    inz
022600110422     d   $CAAPG                       2    inz  dim(35)
022700110422
022800110422     d RCANrapgds      ds                  inz
022900110422     d   $NRAPG                       5s 0 inz  dim(35)
023000110422
023100110422     d Qtapgds         ds
023200110422     d  Qtapg01                       5  0 inz
023300110422     d  Qtapg02                       5  0 inz
023400110422     d  Qtapg03                       5  0 inz
023500110422     d  Qtapg04                       5  0 inz
023600110422     d  Qtapg05                       5  0 inz
023700110422     d  Qtapg06                       5  0 inz
023800110422     d  Qtapg07                       5  0 inz
023900110422     d  Qtapg08                       5  0 inz
024000110422     d  Qtapg09                       5  0 inz
024100110422     d  Qtapg10                       5  0 inz
024200110422     d  Qtapg11                       5  0 inz
024300110422     d  Qtapg12                       5  0 inz
024400110422     d  Qtapg13                       5  0 inz
024500110422     d  Qtapg14                       5  0 inz
024600110422     d  Qtapg15                       5  0 inz
024700110422     d  Qtapg16                       5  0 inz
024800110422     d  Qtapg17                       5  0 inz
024900110422     d  Qtapg18                       5  0 inz
025000110422     d  Qtapg19                       5  0 inz
025100110422     d  Qtapg20                       5  0 inz
025200110422     d  Qtapg21                       5  0 inz
025300110422     d  Qtapg22                       5  0 inz
025400110422     d  Qtapg23                       5  0 inz
025500110422     d  Qtapg24                       5  0 inz
025600110422
025700110422     d Qtapgwds        ds
025800110422     d   $QtaPG                       5s 0 inz  dim(24)
025900110422     d
026000110422     d RCACaappds      ds            70    inz
026100110422     d   $CAAPP                       2    inz  dim(35)
026200110422
026300110422     d RCANrappds      ds                  inz
026400110422     d   $NRAPP                       5s 0 inz  dim(35)
026500110422
026600110422     d Qtappds         ds
026700110422     d  Qtapp01                       5  0 inz
026800110422     d  Qtapp02                       5  0 inz
026900110422     d  Qtapp03                       5  0 inz
027000110422     d  Qtapp04                       5  0 inz
027100110422     d  Qtapp05                       5  0 inz
027200110422     d  Qtapp06                       5  0 inz
027300110422     d  Qtapp07                       5  0 inz
027400110422     d  Qtapp08                       5  0 inz
027500110422     d  Qtapp09                       5  0 inz
027600110422     d  Qtapp10                       5  0 inz
027700110422     d  Qtapp11                       5  0 inz
027800110422     d  Qtapp12                       5  0 inz
027900110422     d  Qtapp13                       5  0 inz
028000110422     d  Qtapp14                       5  0 inz
028100110422     d  Qtapp15                       5  0 inz
028200110422     d  Qtapp16                       5  0 inz
028300110422     d  Qtapp17                       5  0 inz
028400110422     d  Qtapp18                       5  0 inz
028500110422     d  Qtapp19                       5  0 inz
028600110422     d  Qtapp20                       5  0 inz
028700110422     d  Qtapp21                       5  0 inz
028800110422     d  Qtapp22                       5  0 inz
028900110422     d  Qtapp23                       5  0 inz
029000110422     d  Qtapp24                       5  0 inz
029100110422
029200110422     d Qtappwds        ds
029300110422     d   $QtaPP                       5s 0 inz  dim(24)
029400110422     d
029500110422     d RCACaacfds      ds            70    inz
029600110422     d   $CAACF                       2    inz  dim(35)
029700110422
029800110422     d RCANracfds      ds                  inz
029900110422     d   $NRACF                       5s 0 inz  dim(35)
030000110422
030100110422     d Qtacfds         ds
030200110422     d  Qtacf01                       5  0 inz
030300110422     d  Qtacf02                       5  0 inz
030400110422     d  Qtacf03                       5  0 inz
030500110422     d  Qtacf04                       5  0 inz
030600110422     d  Qtacf05                       5  0 inz
030700110422     d  Qtacf06                       5  0 inz
030800110422     d  Qtacf07                       5  0 inz
030900110422     d  Qtacf08                       5  0 inz
031000110422     d  Qtacf09                       5  0 inz
031100110422     d  Qtacf10                       5  0 inz
031200110422     d  Qtacf11                       5  0 inz
031300110422     d  Qtacf12                       5  0 inz
031400110422     d  Qtacf13                       5  0 inz
031500110422     d  Qtacf14                       5  0 inz
031600110422     d  Qtacf15                       5  0 inz
031700110422     d  Qtacf16                       5  0 inz
031800110422     d  Qtacf17                       5  0 inz
031900110422     d  Qtacf18                       5  0 inz
032000110422     d  Qtacf19                       5  0 inz
032100110422     d  Qtacf20                       5  0 inz
032200110422     d  Qtacf21                       5  0 inz
032300110422     d  Qtacf22                       5  0 inz
032400110422     d  Qtacf23                       5  0 inz
032500110422     d  Qtacf24                       5  0 inz
032600110422
032700110422     d Qtacfwds        ds
032800110422     d   $QtaCF                       5s 0 inz  dim(24)
032900110422     d
033000110422     d RCACaaptds      ds            70    inz
033100110422     d   $CAAPT                       2    inz  dim(35)
033200110422
033300110422     d RCANraptds      ds                  inz
033400110422     d   $NRAPT                       5s 0 inz  dim(35)
033500110422
033600110422     d Qtaptds         ds
033700110422     d  Qtapt01                       5  0 inz
033800110422     d  Qtapt02                       5  0 inz
033900110422     d  Qtapt03                       5  0 inz
034000110422     d  Qtapt04                       5  0 inz
034100110422     d  Qtapt05                       5  0 inz
034200110422     d  Qtapt06                       5  0 inz
034300110422     d  Qtapt07                       5  0 inz
034400110422     d  Qtapt08                       5  0 inz
034500110422     d  Qtapt09                       5  0 inz
034600110422     d  Qtapt10                       5  0 inz
034700110422     d  Qtapt11                       5  0 inz
034800110422     d  Qtapt12                       5  0 inz
034900110422     d  Qtapt13                       5  0 inz
035000110422     d  Qtapt14                       5  0 inz
035100110422     d  Qtapt15                       5  0 inz
035200110422     d  Qtapt16                       5  0 inz
035300110422     d  Qtapt17                       5  0 inz
035400110422     d  Qtapt18                       5  0 inz
035500110422     d  Qtapt19                       5  0 inz
035600110422     d  Qtapt20                       5  0 inz
035700110422     d  Qtapt21                       5  0 inz
035800110422     d  Qtapt22                       5  0 inz
035900110422     d  Qtapt23                       5  0 inz
036000110422     d  Qtapt24                       5  0 inz
036100110422
036200110422     d Qtaptwds        ds
036300110422     d   $QtaPT                       5s 0 inz  dim(24)
036400110422     d
036500110422     d RCACaactds      ds            70    inz
036600110422     d   $CAACT                       2    inz  dim(35)
036700110422
036800110422     d RCANractds      ds                  inz
036900110422     d   $NRACT                       5s 0 inz  dim(35)
037000110422
037100110422     d Qtactds         ds
037200110422     d  Qtact01                       5  0 inz
037300110422     d  Qtact02                       5  0 inz
037400110422     d  Qtact03                       5  0 inz
037500110422     d  Qtact04                       5  0 inz
037600110422     d  Qtact05                       5  0 inz
037700110422     d  Qtact06                       5  0 inz
037800110422     d  Qtact07                       5  0 inz
037900110422     d  Qtact08                       5  0 inz
038000110422     d  Qtact09                       5  0 inz
038100110422     d  Qtact10                       5  0 inz
038200110422     d  Qtact11                       5  0 inz
038300110422     d  Qtact12                       5  0 inz
038400110422     d  Qtact13                       5  0 inz
038500110422     d  Qtact14                       5  0 inz
038600110422     d  Qtact15                       5  0 inz
038700110422     d  Qtact16                       5  0 inz
038800110422     d  Qtact17                       5  0 inz
038900110422     d  Qtact18                       5  0 inz
039000110422     d  Qtact19                       5  0 inz
039100110422     d  Qtact20                       5  0 inz
039200110422     d  Qtact21                       5  0 inz
039300110422     d  Qtact22                       5  0 inz
039400110422     d  Qtact23                       5  0 inz
039500110422     d  Qtact24                       5  0 inz
039600110422
039700110422     d Qtactwds        ds
039800110422     d   $QtaCT                       5s 0 inz  dim(24)
039900110422     d
040000110422     d RCACaapptds     ds            70    inz
040100110422     d   $CAAPPT                      2    inz  dim(35)
040200110422
040300110422     d RCANrapptds     ds                  inz
040400110422     d   $NRAPPT                      5s 0 inz  dim(35)
040500110422
040600110422     d Qtapptds        ds
040700110422     d  Qtappt01                      5  0 inz
040800110422     d  Qtappt02                      5  0 inz
040900110422     d  Qtappt03                      5  0 inz
041000110422     d  Qtappt04                      5  0 inz
041100110422     d  Qtappt05                      5  0 inz
041200110422     d  Qtappt06                      5  0 inz
041300110422     d  Qtappt07                      5  0 inz
041400110422     d  Qtappt08                      5  0 inz
041500110422     d  Qtappt09                      5  0 inz
041600110422     d  Qtappt10                      5  0 inz
041700110422     d  Qtappt11                      5  0 inz
041800110422     d  Qtappt12                      5  0 inz
041900110422     d  Qtappt13                      5  0 inz
042000110422     d  Qtappt14                      5  0 inz
042100110422     d  Qtappt15                      5  0 inz
042200110422     d  Qtappt16                      5  0 inz
042300110422     d  Qtappt17                      5  0 inz
042400110422     d  Qtappt18                      5  0 inz
042500110422     d  Qtappt19                      5  0 inz
042600110422     d  Qtappt20                      5  0 inz
042700110422     d  Qtappt21                      5  0 inz
042800110422     d  Qtappt22                      5  0 inz
042900110422     d  Qtappt23                      5  0 inz
043000110422     d  Qtappt24                      5  0 inz
043100110422
043200110422     d Qtapptwds       ds
043300110422     d   $QtaPPT                      5s 0 inz  dim(24)
043400110422     d
043500110422     d RCACaaids       ds            70    inz
043600110422     d   $CAAI                        2    inz  dim(35)
043700110422
043800110422     d RCANraids       ds                  inz
043900110422     d   $NRAI                        5s 0 inz  dim(35)
044000110422
044100110422     d Qtainds         ds
044200110422     d  Qtain01                       5  0 inz
044300110422     d  Qtain02                       5  0 inz
044400110422     d  Qtain03                       5  0 inz
044500110422     d  Qtain04                       5  0 inz
044600110422     d  Qtain05                       5  0 inz
044700110422     d  Qtain06                       5  0 inz
044800110422     d  Qtain07                       5  0 inz
044900110422     d  Qtain08                       5  0 inz
045000110422     d  Qtain09                       5  0 inz
045100110422     d  Qtain10                       5  0 inz
045200110422     d  Qtain11                       5  0 inz
045300110422     d  Qtain12                       5  0 inz
045400110422     d  Qtain13                       5  0 inz
045500110422     d  Qtain14                       5  0 inz
045600110422     d  Qtain15                       5  0 inz
045700110422     d  Qtain16                       5  0 inz
045800110422     d  Qtain17                       5  0 inz
045900110422     d  Qtain18                       5  0 inz
046000110422     d  Qtain19                       5  0 inz
046100110422     d  Qtain20                       5  0 inz
046200110422     d  Qtain21                       5  0 inz
046300110422     d  Qtain22                       5  0 inz
046400110422     d  Qtain23                       5  0 inz
046500110422     d  Qtain24                       5  0 inz
046600110422
046700110422     d Qtainwds        ds
046800110422     d   $QtaIN                       5s 0 inz  dim(24)
046900110422     d
047000110426
047100110426      // schiere di lavoro per i totali generali
047200110426     d WRKCaads        ds
047300110426     d   $WrkCaa                      2    inz  dim(35)
047400110426
047500110426     d WRKNrads        ds
047600110426     d   $WrkNra                      5s 0 inz  dim(35)
047700080206      //---------------------------------------------------------------
047800080206      //?Definizione variabili globali.
047900080206      //---------------------------------------------------------------
048000080206
048100100525      // - Fl
048200100305     d $End            s               n   inz(*off)
048300100305     d $EndAge         s               n   inz(*off)
048400100305     d $RcdOk          s               n   inz(*off)
048500080206
048600080206      // - Indici di schiera
048700100429     d xx              s              4  0 inz
048800100429     d yy              s              4  0 inz
048900100429     d zz              s              4  0 inz
049000110422     d xpm             s              4  0 inz
049100110422     d xpg             s              4  0 inz
049200110422     d xpp             s              4  0 inz
049300110422     d xcf             s              4  0 inz
049400110422     d xpt             s              4  0 inz
049500110422     d xct             s              4  0 inz
049600110422     d xppt            s              4  0 inz
049700110422     d xin             s              4  0 inz
049800080206
049900090806       // - Stringa SQL da eseguire
050000090806     d wSQL            s           2048    Varying        inz
050100080206
050200090401      // - Campi di comodo data
050300100305     d wDate_EUR       s               d   datfmt(*eur)
050400100305     d wDate_ISO       s               d   datfmt(*iso)
050500100305
050600100305      // - Campi di comodo
050700100308     d sav_ATCdad      s                   like(ATCdad)
050800100309     d wAff            s              1  0 inz
050900100308     d wCatMer         s              5  0 inz
051000100309     d wCmm            s                   like(ATCcmm)
051100100923     d wConc           s              2  0 inz
051200100308     d wNota           s              1  0 inz
051300100305     d wTrat           s            196    inz(*all'-')
051400100521     d TipoStampa      s              1
051500100522     d mkcmm           s                   like(ATCcmm)
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
132500100319           pFatCpo  = 'Sp.Trasp.: ' + ' ' + %trim(%editc(CPIpft:'2'));
132600100308           pSpedCpo = 'Sped.Anno: ' + ' ' + %trim(%editc(CPIsna:'2'));
132700100308         ENDIF;
132800100308
132900100308         //?Dati Trattativa
133000100505         clear pTpv;
133100100505         clear pNrv;
133200100308         clear pOfferta;
133300100309         clear pFatNrv;
133400100309         clear pSpedNrv;
133500110119         clear pAuScNrv;
133600100308         IF  ATCnrv > 0;
133700100308           chain ATCnrv TIVIS05L;
133800100308           IF  %found(TIVIS05L) and VISffz <> 'S';
133900100505             pNrv = VISnrv;
134000100505             pTpv = VIStpv;
134100100308           //?controllo se c'è offerta
134200100506             setll ATCnrv TIVOF01L;
134300100506             reade ATCnrv TIVOF01L;
134400100506             DOW  not %EoF(TIVOF01L);
134500100506               IF  VOFeso <> '*';
134600100506                 pOfferta = 'OFF';
134700100506                 leave;
134800100506               ENDIF;
134900100506               reade ATCnrv TIVOF01L;
135000100506             ENDDO;
135100100308           ENDIF;
135200100308           //?Dati Info trattativa
135300100308           //?vado con key fissa sulla riga del fatturato 'TRT'
135400100308           clear pFatNrv;
135500100308           clear pSpedNrv;
135600100308           chain (ATCnrv:'TRT') TIVII01L;
135700100308           IF  %found(TIVII01L);
135800110119             pFatNrv  = 'Valore    : ' + ' ' + %trim(%editc(VIIpft:'K'));
135900100308             pSpedNrv = 'Spedizioni: ' + ' ' + %trim(%editc(VIIsna:'2'));
136000100308           ENDIF;
136100110119           //?Se trattativa di tipo 'A'
136200110119           //?cerco anche 'A/S'
136300110119           IF  VIStpv = 'A';
136400110119             clear pAuScNrv;
136500110119             chain (ATCnrv:'A/S') TIVII01L;
136600110119             IF  %found(TIVII01L);
136700110518               pAuScNrv = 'Aum./Sco. : ' + ' ' + %trim(%editc(VIIvald:'K'))
136800110119                        + ' %';
136900110119             ENDIF;
137000110119           ENDIF;
137100100308         ENDIF;
137200100308
137300100308         //?Note attività (2 righe)
137400100520         clear pNota01;
137500100520         clear pNota02;
137600100520         clear pNota03;
137700100520         clear pNota04;
137800100520
137900100520         //?Prima provo con note legate al progressivo attuale
138000100308         wNota = 1;
138100100520         CPNcpo  = ATCcpo;
138200100520         CPNatn  = ATCatn;
138300100520         CPNatnp = ATCatnp;
138400100520         setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
138500100520         reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
138600100520
138700100520         DOW  not %eof(TICPN05L) and wNota <= 2;
138800100520           IF  wNota = 1;
138900100520             pNota01 = %subst(CPNnot:1:45);
139000100520             pNota02 = %subst(CPNnot:46:45);
139100100520           ENDIF;
139200100520           IF  wNota = 2;
139300100520             pNota03 = %subst(CPNnot:1:45);
139400100520             pNota04 = %subst(CPNnot:46:45);
139500100520           ENDIF;
139600100520
139700100520           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
139800100520           wNota += 1;
139900100520         ENDDO;
140000100520
140100100520         //?Se non ho trovato note, provo con note legate al progressivo -1
140200100520         IF  pNota01 = *blanks and pNota02 = *blanks and
140300100520             pNota03 = *blanks and pNota04 = *blanks;
140400100520           CPNatnp = ATCatnp - 1;
140500100520           setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
140600100520           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
140700100308
140800100520           DOW  not %eof(TICPN05L) and wNota <= 2;
140900100520             IF  wNota = 1;
141000100520               pNota01 = %subst(CPNnot:1:45);
141100100520               pNota02 = %subst(CPNnot:46:45);
141200100520             ENDIF;
141300100520             IF  wNota = 2;
141400100520               pNota03 = %subst(CPNnot:1:45);
141500100520               pNota04 = %subst(CPNnot:46:45);
141600100520             ENDIF;
141700100520
141800100520             reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
141900100520             wNota += 1;
142000100520           ENDDO;
142100100520         ENDIF;
142200100505
142300100505         //?Se non ci sono note legate all'attività controllo se ci sono note legate alla trattativa
142400100505         IF  ATCnrv > 0 and pNota01 = *blanks and pNota02 = *blanks and
142500100505                            pNota03 = *blanks and pNota04 = *blanks;
142600100505           CPNksc = ATCksc;
142700100505           CPNnrv = ATCnrv;
142800100505           setll (CPNcpo:CPNksc:CPNnrv) TICPN04L;
142900100505           reade (CPNcpo:CPNksc:CPNnrv) TICPN04L;
143000100505
143100100505           DOW  not %eof(TICPN04L) and wNota <= 2;
143200100505             IF  wNota = 1;
143300100505               pNota01 = %subst(CPNnot:1:45);
143400100505               pNota02 = %subst(CPNnot:46:45);
143500100505             ENDIF;
143600100505             IF  wNota = 2;
143700100505               pNota03 = %subst(CPNnot:1:45);
143800100505               pNota04 = %subst(CPNnot:46:45);
143900100505             ENDIF;
144000100505
144100100505             reade (CPNcpo:CPNksc:CPNnrv) TICPN04L;
144200100505             wNota += 1;
144300100505           ENDDO;
144400100505         ENDIF;
144500100308
144600100308         //?Salto pagina
144700100506         //IF  CurLine >= 64;
144800100506         IF  *in99;
144900100308           exsr sr_StpSalto;
145000100310           clear wrkriga;
145100100521           write MK89SEPARA;
145200100308           *in99 = *off;
145300100308         ENDIF;
145400100305
145500100521         write MK89RIGA;
145600100309
145700100309       ENDSR;
145800100712
145900100712       //--------------------------------------------------------------
146000100712       //?Stampo i Concorrenti.
146100100712       //--------------------------------------------------------------
146200100712       BEGSR sr_StpConc;
146300100712
146400100712         clear pConc;
146500100712         wConc = 1;
146600100712
146700100712         //?Cerco concorrenti
146800100712         setll (ATCcpo:'10') TICPI01L;
146900100712         reade (ATCcpo:'10') TICPI01L;
147000100712         DOW not %eof(TICPI01L);
147100100712           IF  CPIval > 0 and CPIspf <> '_BAR';
147200100712         //?imposto il campo da stampare
147300100712             IF  pConc = *blanks;
147400100712               pConc = 'Corrieri Utilizzati:';
147500100712             ENDIF;
147600100712             clear TIBS02ds;
147700100712             clear dIFS;
147800100712             T02mod = 'C';
147900100712             T02cod = 'IFS';
148000100712             T02ke1 = CPItpf;
148100100712             T02ke2 = CPIspf;
148200100712             T02sif = KNSIF;
148300100712             TNTBE_RicercaControllo (kpjba : tibs02ds);
148400100712             IF  T02err = *blanks;
148500100712               dIFS = T02uni;
148600100712             ENDIF;
148700100712             IF  wConc > 1;
148800100712             //?Aggiungo concorrente fino al massimo di 186 byte
148900100712               IF  (%len(%trim(pConc)) + %len(%trim(§IFSdes)) + 2) > 185;
149000100712                 Pconc = %trim(Pconc) + ' Altri...';
149100100712                 leave;
149200100712               ENDIF;
149300100712               pConc = %trim(pConc) + ',';
149400100712             ENDIF;
149500100712             pConc = %trim(pConc) + ' ' + §IFSdes;
149600100712             wConc += 1;
149700100712           ENDIF;
149800100712           reade (ATCcpo:'10') TICPI01L;
149900100712         ENDDO;
150000100712
150100100712         //?Salto pagina
150200100712         IF  CurLine >= 65;
150300100712           exsr sr_StpSalto;
150400100712           *in99 = *off;
150500100712         ENDIF;
150600100712
150700100712         IF  pConc <> *blanks;
150800100712           write MK89CONC;
150900100712         ENDIF;
151000100712
151100100712       ENDSR;
151200100712
151300100309
151400100309       //--------------------------------------------------------------
151500100309       //?Stampo le attività a dettaglio UFFICIO.
151600100309       //--------------------------------------------------------------
151700100309       BEGSR sr_StpDetUFF;
151800100309
151900100309         clear pAttUff;
152000100309
152100100309         //?decodifico attività
152200100309         clear TIBS02ds;
152300100309         clear dCCO;
152400100309         T02mod = 'C';
152500100309         T02cod = 'CCO';
152600100309         T02ke1 = ATCcad;
152700100309         T02sif = KNSIF;
152800100309         TNTBE_RicercaControllo (kpjba : tibs02ds);
152900100309         IF  T02err = *blanks;
153000100309           dCCO = T02uni;
153100100309         ENDIF;
153200100309
153300100309         //?CHIODO --> non stampo la decodifica della causale se FERIE
153400100309         IF  ATCtat <> 'F';
153500100309           pAttUff = §CCOdes;
153600100309           pAttUff = %trim(pAttUff) + ' Dalle ' +
153700100309                     %subst(%editc(ATCoii:'X'):1:2) +
153800100309                     ':' + %subst(%editc(ATCoii:'X'):3:2) +
153900100309                     ' Alle ' +
154000100309                     %subst(%editc(ATCofi:'X'):1:2) + ':' +
154100100309                     %subst(%editc(ATCofi:'X'):3:2);
154200100309         ELSE;
154300100309           pAttUff = 'Dalle ' +
154400100309                     %subst(%editc(ATCoii:'X'):1:2) +
154500100309                     ':' + %subst(%editc(ATCoii:'X'):3:2) +
154600100309                     ' Alle ' +
154700100309                     %subst(%editc(ATCofi:'X'):1:2) + ':' +
154800100309                     %subst(%editc(ATCofi:'X'):3:2);
154900100309         ENDIF;
155000100712
155100100712         //?Note attività (1 riga)
155200100712         clear pNota01;
155300100712         clear pNota02;
155400100712
155500100712         //?Leggo le note legate al progressivo attuale
155600100712         wNota = 1;
155700100712         clear CPNcpo;
155800100712         CPNatn  = ATCatn;
155900100712         CPNatnp = ATCatnp;
156000100712         setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
156100100712         reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
156200100712
156300100712         DOW  not %eof(TICPN05L) and wNota <= 1;
156400100712           IF  wNota = 1;
156500100712             pNota01 = %subst(CPNnot:1:45);
156600100712             pNota02 = %subst(CPNnot:46:45);
156700100712           ENDIF;
156800100712
156900100712           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
157000100712           wNota += 1;
157100100712         ENDDO;
157200100712
157300100712         //?Se non ho trovato note, provo con note legate al progressivo -1
157400100712         IF  pNota01 = *blanks and pNota02 = *blanks;
157500100712           CPNatnp = ATCatnp - 1;
157600100712           setll (CPNcpo:CPNatn:CPNatnp) TICPN05L;
157700100712           reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
157800100712
157900100712           DOW  not %eof(TICPN05L) and wNota <= 1;
158000100712             IF  wNota = 1;
158100100712               pNota01 = %subst(CPNnot:1:45);
158200100712               pNota02 = %subst(CPNnot:46:45);
158300100712             ENDIF;
158400100712
158500100712             reade (CPNcpo:CPNatn:CPNatnp) TICPN05L;
158600100712             wNota += 1;
158700100712           ENDDO;
158800100712         ENDIF;
158900100309
159000100309         //?Salto pagina
159100100309         IF  *in99;
159200100309           exsr sr_StpSalto;
159300100309           *in99 = *off;
159400100309         ENDIF;
159500100309
159600100521         write MK89RIGAUF;
159700100309
159800100309       ENDSR;
159900100309
160000100309       //--------------------------------------------------------------
160100100309       //?Stampo l'eventuale affiancamento.
160200100309       //--------------------------------------------------------------
160300100309       BEGSR sr_StpAffianca;
160400100309
160500100309         clear pAffianca;
160600100309         wAff = 1;
160700100309
160800100309         SELECT;
160900100309         //?se appuntamento (CHIODO 91 e A)
161000100309           WHEN  ATCtat = 'A' and ATCcad = '91';
161100100309         //?controllo se c'è affiancamento
161200100309             ATLatl  = ATCatn;
161300100309             ATLatlp = ATCatnp;
161400100309             setll (ATLatl:ATLatlp) TIATL02L;
161500100309             reade (ATLatl:ATLatlp) TIATL02L;
161600100309             DOW  not %eof(TIATL02L);
161700100309         //?imposto il campo da stampare
161800100309               IF  pAffianca = *blanks;
161900100705                 pAffianca = '  Affiancato da:';
162000100309               ENDIF;
162100100309         //?leggo attività affiancamento e imposto il nome dell'agente sul campo da stampare
162200100309               w_ATCtat  = ATCtat;
162300100309               w_ATCatn  = ATLatn;
162400100309               w_ATCatnp = ATLatnp;
162500100309               chain (w_ATCtat:w_ATCatn:w_ATCatnp) TIATC01L;
162600100309               IF  %found(TIATC01L);
162700100309                 wCmm = w_ATCcmm;
162800100309                 exsr sr_DecCmm;
162900100309                 IF  wAff > 1;
163000100319             //?Aggiungo nome agente fino al massimo di 196 byte
163100130917                   IF  (%len(%trim(pAffianca)) + %len(%trim(CMMdes)) + 2) > 196;
163200100309                     leave;
163300100309                   ENDIF;
163400100309                   pAffianca = %trim(pAffianca) + ',';
163500100309                 ENDIF;
163600130917                 pAffianca = %trim(pAffianca) + ' ' + CMMdes;
163700100309               ENDIF;
163800100309               wAff += 1;
163900100309               reade (ATLatl:ATLatlp) TIATL02L;
164000100309             ENDDO;
164100100309         //?Stampo l'affiancamento
164200100309             IF  pAffianca <> *blanks;
164300100309             //?Salto pagina
164400100309               IF  *in99;
164500100309                 exsr sr_StpSalto;
164600100309                 *in99 = *off;
164700100309               ENDIF;
164800100521               write MK89AFF;
164900100309             ENDIF;
165000100309
165100100309         //?se affiancamento (CHIODO 92 e A)
165200100309           WHEN  ATCtat = 'A' and ATCcad = '92';
165300100309         //?controllo l'affiancamento
165400100309             ATLatn  = ATCatn;
165500100309             ATLatnp = ATCatnp;
165600100309             chain (ATLatn:ATLatnp) TIATL01L;
165700100309             IF  %found(TIATL01L);
165800100309         //?imposto il campo da stampare
165900100309               IF  pAffianca = *blanks;
166000100705                 pAffianca = '  In affiancamento a:';
166100100309               ENDIF;
166200100309         //?leggo attività affiancamento e imposto il nome dell'agente sul campo da stampare
166300100309               w_ATCtat  = ATCtat;
166400100309               w_ATCatn  = ATLatl;
166500100309               w_ATCatnp = ATLatlp;
166600100309               chain (w_ATCtat:w_ATCatn:w_ATCatnp) TIATC01L;
166700100309               IF  %found(TIATC01L);
166800100309                 wCmm = w_ATCcmm;
166900100309                 exsr sr_DecCmm;
167000130917                 pAffianca = %trim(pAffianca) + ' ' + CMMdes;
167100100309               ENDIF;
167200100309             ENDIF;
167300100309         //?Stampo l'affiancamento
167400100309             IF  pAffianca <> *blanks;
167500100309             //?Salto pagina
167600100309               IF  *in99;
167700100309                 exsr sr_StpSalto;
167800100309                 *in99 = *off;
167900100309               ENDIF;
168000100521               write MK89AFF;
168100100309             ENDIF;
168200100309
168300100309         ENDSL;
168400100309
168500100309       ENDSR;
168600080206
168700100521
168800100521       //--------------------------------------------------------------
168900100521       //?Stampa Statistica Consuntiva
169000100521       //--------------------------------------------------------------
169100100521       BEGSR Stat_consun ;
169200100521
169300100521       // cerco il record del consuntivo
169400100521
169500100521       wcmm = $ageu(zz) ;
169600100521         exec sql
169700100521          declare CON cursor for
169800100521          select *
169900100521          from WFRCA00f where RCArgf = :wcmm      ;
170000100521
170100100521         exec sql
170200100521           open CON;
170300100521           IF sqlcode >= 0;
170400100521
170500100521           exec sql
170600100521             fetch next from CON into :WFRCA00F ;
170700100521             IF sqlcod = 100 or sqlcod < 0;
170800100521             Else ;
170900100524        // stampo consuntivo per commerciale unificante se non escluso
171000100524           select ;
171100100524         // se omessi gli interni
171200101006            when i88cscmm = 'O'  and i88cscmmie = 'I' and (RCAFUN = 'COMIN'
171300101006                 or RCAfun = 'ASC' or RCAfun = 'SA');
171400100524         // se omessi gli esterni
171500101006            when i88cscmm = 'O'  and i88cscmmie = 'E' and RCAFUN <> 'COMIN'
171600101006                 and RCAfun <> 'ASC' and RCAfun <> 'SA';
171700100524         // se inclusi gli esterni
171800101006            when i88cscmm = 'I'  and i88cscmmie = 'E' and (RCAFUN =  'COMIN'
171900101006                 or RCAfun = 'ASC' or RCAfun = 'SA');
172000100524         // se inclusi gli interni
172100101006            when i88cscmm = 'I'  and i88cscmmie = 'I' and RCAFUN <> 'COMIN'
172200101006                 and RCAfun <> 'ASC' and RCAfun <> 'SA';
172300100524          other ;
172400100521
172500110422         // preparo le causali da stampare
172600110426           $caastp = $stp;
172700110426           causalids = Cauwds ;
172800110426           cauineds = Cauwds ;
172900110428         // pulisco i campi in stampa e di lavoro
173000110426           clear Qtapmds ;
173100110426           clear Qtapgds ;
173200110426           clear Qtappds ;
173300110426           clear Qtacfds ;
173400110426           clear Qtaptds ;
173500110426           clear Qtactds ;
173600110426           clear Qtapptds ;
173700110426           clear Qtainds ;
173800110428
173900110428           clear Qtapmwds ;
174000110428           clear Qtapgwds ;
174100110428           clear Qtappwds ;
174200110428           clear Qtacfwds ;
174300110428           clear Qtaptwds ;
174400110428           clear Qtactwds ;
174500110428           clear Qtapptwds ;
174600110428           clear Qtainwds ;
174700110422
174800110422         // telefonate potenziali mai trattati
174900110422           Rcacaapmds = Rcacaapm ;
175000110422           Rcanrapmds = Rcanrapm ;
175100110422
175200110422           xx = 1 ;
175300110422           For xx by 1 to %elem($cau) ;
175400110426               contacau = $cau(xx);
175500110422               If uno <> ' ';
175600110422                  xpm = %lookup(uno: $Caapm) ;
175700110422                  If xpm > 0;
175800110422                     $qtaPm(xx) += $Nrapm(xpm) ;
175900110422                  Endif ;
176000110422               Endif ;
176100110422               If due <> ' ';
176200110422                  xpm = %lookup(due: $Caapm) ;
176300110422                  If xpm > 0;
176400110422                     $qtaPm(xx) += $Nrapm(xpm) ;
176500110422                  Endif ;
176600110422               Endif ;
176700110422           Endfor ;
176800110422           Qtapmds = Qtapmwds ;
176900110422
177000110422         // telefonate potenziali già trattati
177100110422           Rcacaapgds = Rcacaapg ;
177200110422           Rcanrapgds = Rcanrapg ;
177300110422
177400110422           xx = 1 ;
177500110422           For xx by 1 to %elem($cau) ;
177600110426               contacau = $cau(xx);
177700110422               If uno <> ' ';
177800110422                  xpg = %lookup(uno: $Caapg) ;
177900110422                  If xpg > 0;
178000110422                     $qtaPg(xx) += $Nrapg(xpg) ;
178100110422                  Endif ;
178200110422               Endif ;
178300110422               If due <> ' ';
178400110422                  xpg = %lookup(due: $Caapg) ;
178500110422                  If xpg > 0;
178600110422                     $qtaPg(xx) += $Nrapg(xpg) ;
178700110422                  Endif ;
178800110422               Endif ;
178900110422           Endfor ;
179000110422           Qtapgds = Qtapgwds ;
179100110422
179200110422         // telefonate potenziali persi
179300110422           Rcacaappds = Rcacaapp ;
179400110422           Rcanrappds = Rcanrapp ;
179500110422
179600110422           xx = 1 ;
179700110422           For xx by 1 to %elem($cau) ;
179800110426               contacau = $cau(xx);
179900110422               If uno <> ' ';
180000110422                  xpp = %lookup(uno: $Caapp) ;
180100110422                  If xpp > 0;
180200110422                     $qtaPp(xx) += $Nrapp(xpp) ;
180300110422                  Endif ;
180400110422               Endif ;
180500110422               If due <> ' ';
180600110422                  xpp = %lookup(due: $Caapp) ;
180700110422                  If xpp > 0;
180800110422                     $qtaPp(xx) += $Nrapp(xpp) ;
180900110422                  Endif ;
181000110422               Endif ;
181100110422           Endfor ;
181200110422           Qtappds = Qtappwds ;
181300110422
181400110422         // telefonate clienti fuori trattativa
181500110422           Rcacaacfds = Rcacaacf ;
181600110422           Rcanracfds = Rcanracf ;
181700110422
181800110422           xx = 1 ;
181900110422           For xx by 1 to %elem($cau) ;
182000110426               contacau = $cau(xx);
182100110422               If uno <> ' ';
182200110422                  xcf = %lookup(uno: $Caapp) ;
182300110422                  If xcf > 0;
182400110422                     $qtaCf(xx) += $Nracf(xcf) ;
182500110422                  Endif ;
182600110422               Endif ;
182700110422               If due <> ' ';
182800110422                  xcf = %lookup(due: $Caacf) ;
182900110422                  If xcf > 0;
183000110422                     $qtaCf(xx) += $Nracf(xcf) ;
183100110422                  Endif ;
183200110422               Endif ;
183300110422           Endfor ;
183400110422           Qtacfds = Qtacfwds ;
183500110422
183600110422         // telefonate potenziali dentro trattativa
183700110422           Rcacaaptds = Rcacaapt ;
183800110422           Rcanraptds = Rcanrapt ;
183900110422
184000110422           xx = 1 ;
184100110422           For xx by 1 to %elem($cau) ;
184200110426               contacau = $cau(xx);
184300110422               If uno <> ' ';
184400110422                  xpt = %lookup(uno: $Caapt) ;
184500110422                  If xpt > 0;
184600110422                     $qtaPt(xx) += $Nrapt(xpt) ;
184700110422                  Endif ;
184800110422               Endif ;
184900110422               If due <> ' ';
185000110422                  xpt = %lookup(due: $Caapt) ;
185100110422                  If xpt > 0;
185200110422                     $qtaPt(xx) += $Nrapt(xpt) ;
185300110422                  Endif ;
185400110422               Endif ;
185500110422           Endfor ;
185600110422           Qtaptds = Qtaptwds ;
185700110422
185800110422         // telefonate clienti    dentro trattativa
185900110422           Rcacaactds = Rcacaact ;
186000110422           Rcanractds = Rcanract ;
186100110422
186200110422           xx = 1 ;
186300110422           For xx by 1 to %elem($cau) ;
186400110426               contacau = $cau(xx);
186500110422               If uno <> ' ';
186600110422                  xct = %lookup(uno: $Caact) ;
186700110422                  If xct > 0;
186800110422                     $qtaCt(xx) += $Nract(xct) ;
186900110422                  Endif ;
187000110422               Endif ;
187100110422               If due <> ' ';
187200110422                  xct = %lookup(due: $Caact) ;
187300110422                  If xct > 0;
187400110422                     $qtact(xx) += $Nract(xct) ;
187500110422                  Endif ;
187600110422               Endif ;
187700110422           Endfor ;
187800110422           Qtactds = Qtactwds ;
187900110422
188000110422         // telefonate potenziali persi dentro trattativa
188100110422           Rcacaapptds = Rcacaappt ;
188200110422           Rcanrapptds = Rcanrappt ;
188300110422
188400110422           xx = 1 ;
188500110422           For xx by 1 to %elem($cau) ;
188600110426               contacau = $cau(xx);
188700110422               If uno <> ' ';
188800110422                  xppt = %lookup(uno: $Caappt) ;
188900110422                  If xppt > 0;
189000110422                     $qtaPpt(xx) += $Nrappt(xppt) ;
189100110422                  Endif ;
189200110422               Endif ;
189300110422               If due <> ' ';
189400110422                  xppt = %lookup(due: $Caappt) ;
189500110422                  If xppt > 0;
189600110422                     $qtaPpt(xx) += $Nrappt(xppt) ;
189700110422                  Endif ;
189800110422               Endif ;
189900110422           Endfor ;
190000110422           Qtapptds = Qtapptwds ;
190100110422
190200110422         // telefonate inevase
190300110422           Rcacaaids = Rcacaai ;
190400110422           Rcanraids = Rcanrai ;
190500110422
190600110422           xx = 1 ;
190700110422           For xx by 1 to %elem($cau) ;
190800110426               contacau = $cau(xx);
190900110422               If uno <> ' ';
191000110422                  xin = %lookup(uno: $Caai) ;
191100110422                  If xin > 0;
191200110422                     $qtain(xx) += $Nrai(xin) ;
191300110422                  Endif ;
191400110422               Endif ;
191500110422               If due <> ' ';
191600110422                  xin = %lookup(due: $Caai) ;
191700110422                  If xin > 0;
191800110422                     $qtain(xx) += $Nrai(xin) ;
191900110422                  Endif ;
192000110422               Endif ;
192100110422           Endfor ;
192200110422           Qtainds = Qtainwds ;
192300110422
192400100622        // totali appuntamenti
192500110119           totapppr = Rcaapptn + Rcaappta + Rcaapptm + Rcaapptc +
192600110119                      Rcaappti + Rcaapptr ;
192700100629        // totali appuntamenti di cui il primo
192800110119           totapppr1 = Rcapatn + Rcapata  + Rcapatm + Rcapatc  +
192900110119                       Rcapati + Rcapatr  ;
193000100628        // totali telefonate post offerta di altri
193100100628           Tottepoa = (rcatepo30a + Rcatepo33a) ;
193200110426        // totali telefonate caldi di altri
193300110428           Tottecalda = (rcatepc31a + Rcatepc44a) ;
193400100819        // totali appuntamenti eseguiti
193500110119           totappes = Rcaaptn + Rcaapta + Rcaaptm + Rcaaptc +
193600110119                      Rcaapti + Rcaaptr ;
193700100819        // totali affiancamenti eseguiti
193800110119           totaffes = Rcaaftn + Rcaafta + Rcaaftm + Rcaaftc +
193900110119                      Rcaafti + Rcaaftr  ;
194000100521           tipostampa = 'C' ;
194100100524         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
194200100524            *in11 = *on ;
194300100524
194400100524         //?Imposto data inizio e fine stampa
194500100524            wDate_ISO = %date(I88deci:*iso);
194600100524            wDate_EUR = wDate_ISO;
194700100524            pDatai    = %dec(wDate_EUR);
194800100524            wDate_ISO = %date(I88decf:*iso);
194900100524            wDate_EUR = wDate_ISO;
195000100524            pDataf    = %dec(wDate_EUR);
195100100622          // calcolo l'incidenza  degli appuntamenti dulle telefonate effettuate
195200100622            exsr sr_incidenza ;
195300100622          // prima verifico che tutte i fattori siano valorizzati
195400100524            exsr sr_StpSalto;
195500100524            write mk89rigcon ;
195600100524           ENDSL;
195700100521          endif ;
195800100521          endif ;
195900100521
196000100521         exec sql close CON;
196100100521
196200100521       ENDSR;
196300100521
196400100622       //--------------------------------------------------------------
196500100622       //?Calcolo incidenza appuntamenti presi su telefonate Potenziali
196600100622       //--------------------------------------------------------------
196700100622       BEGSR Sr_incidenza ;
196800100622
196900100622          // prima verifico che tutti i fattori siano valorizzati
197000110422         If Rcapatn > 0 and (rcatepm > 0 or rcatepg > 0 or rcatepp > 0 ) ;
197100110422            Incide = ((100 * rcapatn) / (rcatepm + rcatepg + rcatepp)) ;
197200100622         else ;
197300100622            clear Incide ;
197400100622         endif ;
197500100622
197600100622       ENDSR;
197700100622
197800100522       //--------------------------------------------------------------
197900100522       //?Stampa Statistica Preventiva
198000100522       //--------------------------------------------------------------
198100100522       BEGSR Stat_preven ;
198200100522
198300100522       // cerco il record del preventivo
198400100524         wrkriga1= '*' + wTrat + '*';
198500100522
198600100522       wcmm = $ageu(zz) ;
198700100522         exec sql
198800100522          declare PRE cursor for
198900100522          select *
199000100522          from WFRPA00f where RPArgf = :wcmm      ;
199100100522
199200100522         exec sql
199300100522           open PRE;
199400100522           IF sqlcode >= 0;
199500100522
199600100522           exec sql
199700100522             fetch next from PRE into :WFRPA00F ;
199800100522             IF sqlcod = 100 or sqlcod < 0;
199900100522             Else ;
200000100524        // stampo preventivo per commerciale unificante se non escluso
200100100524           select ;
200200100524         // se omessi gli interni
200300101006            when i88cscmm = 'O'  and i88cscmmie = 'I' and (RPAFUN = 'COMIN'
200400101006                 or RPAfun = 'ASC' or RPAfun = 'SA');
200500100524         // se omessi gli esterni
200600101006            when i88cscmm = 'O'  and i88cscmmie = 'E' and RPAFUN <> 'COMIN'
200700101006                 and RPAfun <> 'ASC' and RPAfun <> 'SA';
200800100524         // se inclusi gli esterni
200900101006            when i88cscmm = 'I'  and i88cscmmie = 'E' and (RPAFUN =  'COMIN'
201000101006                 or RPAfun = 'ASC' or RPAfun = 'SA');
201100100524         // se inclusi gli interni
201200101006            when i88cscmm = 'I'  and i88cscmmie = 'I' and RPAFUN <> 'COMIN'
201300101006                 and RPAfun <> 'ASC' and RPAfun <> 'SA';
201400100524          other ;
201500100524         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
201600100524            *in11 = *on ;
201700100524
201800100524         //?Imposto data inizio e fine stampa
201900100527            wDate_ISO = %date(I88depi:*iso);
202000100524            wDate_EUR = wDate_ISO;
202100100524            pDatai    = %dec(wDate_EUR);
202200100527            wDate_ISO = %date(I88depf:*iso);
202300100524            wDate_EUR = wDate_ISO;
202400100524            pDataf    = %dec(wDate_EUR);
202500100524
202600100522           tipostampa = 'P' ;
202700100522           exsr sr_StpSalto;
202800100522        // calcolo totali appuntamenti
202900110119           totapp = (rpaappm + rpaappn + rpaappa + rpaappc +
203000110119                     rpaappi + rpaappr);
203100100522        // calcolo totali telefonate
203200100524           tottel = (rpatc19 + rpatc20 + rpatc21 + rpatc22 + rpatc24 +
203300100524                     rpatc25 + rpatc26 + rpatc27 + rpatc28 + rpatc30 +
203400110422                     rpatc31 + rpatc32 + rpatc33 + rpatc44 + rpatc60 +
203500110422                     rpatc70 + rpatc71 + rpatc73 + rpatc74 + rpatc75);
203600100524        // Verifico se affiancamenti valorizzati
203700110119           *in30 = rpaaffn > 0 ;
203800110119           *in31 = rpaaffi > 0 ;
203900110119           *in32 = rpaaffa > 0 ;
204000110119           *in33 = rpaaffm > 0 ;
204100110119           *in34 = rpaaffc > 0 ;
204200110119           *in35 = rpaaffr > 0 ;
204300100524           write Mk89rigpre ;
204400100526           write Mk89rigpr1 ;
204500100524          ENDSL ;
204600100522          endif ;
204700100522          endif ;
204800100522
204900100522         exec sql close PRE;
205000100522
205100100522       ENDSR;
205200100522
205300100525       //--------------------------------------------------------------
205400100525       //?Stampa Totali generali Statistica
205500100525       //--------------------------------------------------------------
205600100526       BEGSR SR_Totgen  ;
205700100525
205800110426         // pulisco i campi in stampa
205900110426           clear Qtapmds ;
206000110426           clear Qtapgds ;
206100110426           clear Qtappds ;
206200110426           clear Qtacfds ;
206300110426           clear Qtaptds ;
206400110426           clear Qtactds ;
206500110426           clear Qtapptds ;
206600110426           clear Qtainds ;
206700110428           clear Qtapmwds ;
206800110428           clear Qtapgwds ;
206900110428           clear Qtappwds ;
207000110428           clear Qtacfwds ;
207100110428           clear Qtaptwds ;
207200110428           clear Qtactwds ;
207300110428           clear Qtapptwds ;
207400110428           clear Qtainwds ;
207500110426
207600110426         // preparo le causali da stampare
207700110426           $caastp = $stp;
207800110426           causalids = Cauwds ;
207900110426           cauineds = Cauwds ;
208000110426
208100100528         wrkriga1= '*' + wTrat + '*';
208200100622         *in20 = *on ;
208300100525       // verifico se ho lanciato il consuntivo
208400100526       If i88tcpe = 'C' or i88tcpe = 'E' ;
208500100525       //?Ciclo per sk unificanti e stampo
208600100525           zz = 1;
208700100526           FOR zz by 1 to %elem($AgeU);
208800100526           IF  $AgeU(zz) <> *zeros;
208900100525
209000100525               wcmm = $ageu(zz) ;
209100100525               exec sql
209200100525                 declare COG cursor for
209300100525                 select *
209400100525                 from WFRCA00f where RCArgf = :wcmm      ;
209500100525
209600100525                 exec sql
209700100525                 open COG;
209800100526                 IF sqlcode >= 0;
209900100525
210000100525                    exec sql
210100100525                    fetch next from COG into :WFRCA00F ;
210200100526                    IF sqlcod = 100 or sqlcod < 0;
210300100525                    Else ;
210400100525        // stampo statistica generale facendo attenzione alle inclusioni/esclusioni
210500100525                     select ;
210600100525         // se omessi gli interni
210700101007                       when i88tscmm='O' and i88tscmmie='I' and (RCAFUN='COMIN'
210800101007                       or RCAfun = 'ASC' or RCAfun = 'SA');
210900100526         // se omessi gli esterni
211000101007                       when i88tscmm='O' and i88tscmmie='E' and RCAFUN<>'COMIN'
211100101007                       and RCAfun <> 'ASC' and RCAfun <> 'SA';
211200100525         // se inclusi gli esterni
211300101007                       when i88tscmm='I' and i88tscmmie='E' and (RCAFUN='COMIN'
211400101007                       or RCAfun = 'ASC' or RCAfun = 'SA');
211500100525         // se inclusi gli interni
211600101007                       when i88tscmm='I' and i88tscmmie='I' and RCAFUN<>'COMIN'
211700101007                       and RCAfun <> 'ASC' and RCAfun <> 'SA';
211800100525                     other ;
211900110426
212000110426         // telefonate potenziali mai trattati
212100110426           Wrkcaads = Rcacaapm ;
212200110426           Wrknrads = Rcanrapm ;
212300110426
212400110426           xx = 1 ;
212500110426           For xx by 1 to %elem($cau) ;
212600110426               contacau = $cau(xx);
212700110426               If uno <> ' ';
212800110426                  yy = %lookup(uno: $wrkCaa) ;
212900110426                  If yy > 0;
213000110426                     $qtaPm(xx) += $WrkNra(yy) ;
213100110426                  Endif ;
213200110426               Endif ;
213300110426               If due <> ' ';
213400111017                  yy  = %lookup(due: $wrkCaa) ;
213500110426                  If yy > 0;
213600110426                     $qtaPm(xx) += $WrkNra(yy) ;
213700110426                  Endif ;
213800110426               Endif ;
213900110426           Endfor ;
214000100525
214100110426         // telefonate potenziali già trattati
214200110426           Wrkcaads = Rcacaapg ;
214300110426           Wrknrads = Rcanrapg ;
214400110426
214500110426           xx = 1 ;
214600110426           For xx by 1 to %elem($cau) ;
214700110426               contacau = $cau(xx);
214800110426               If uno <> ' ';
214900110426                  yy = %lookup(uno: $wrkCaa) ;
215000110426                  If yy > 0;
215100110428                     $qtaPg(xx) += $WrkNra(yy) ;
215200110426                  Endif ;
215300110426               Endif ;
215400110426               If due <> ' ';
215500111017                  yy  = %lookup(due: $wrkCaa) ;
215600110426                  If yy > 0;
215700110428                     $qtaPg(xx) += $WrkNra(yy) ;
215800110426                  Endif ;
215900110426               Endif ;
216000110426           Endfor ;
216100110426
216200110426         // telefonate potenziali persi
216300110426           Wrkcaads = Rcacaapp ;
216400110426           Wrknrads = Rcanrapp ;
216500110426
216600110426           xx = 1 ;
216700110426           For xx by 1 to %elem($cau) ;
216800110426               contacau = $cau(xx);
216900110426               If uno <> ' ';
217000110426                  yy = %lookup(uno: $wrkCaa) ;
217100110426                  If yy > 0;
217200110426                     $qtaPp(xx) += $WrkNra(yy) ;
217300110426                  Endif ;
217400110426               Endif ;
217500110426               If due <> ' ';
217600110426                  yy = %lookup(due: $wrkCaa) ;
217700110426                  If yy > 0;
217800110426                     $qtaPp(xx) += $WrkNra(yy) ;
217900110426                  Endif ;
218000110426               Endif ;
218100110426           Endfor ;
218200100609
218300110426         // telefonate clienti fuori trattativa
218400110426           Wrkcaads = Rcacaacf ;
218500110426           Wrknrads = Rcanracf ;
218600110426
218700110426           xx = 1 ;
218800110426           For xx by 1 to %elem($cau) ;
218900110426               contacau = $cau(xx);
219000110426               If uno <> ' ';
219100110426                  yy = %lookup(uno: $wrkCaa) ;
219200110426                  If yy > 0;
219300110426                     $qtaCf(xx) += $WrkNra(yy) ;
219400110426                  Endif ;
219500110426               Endif ;
219600110426               If due <> ' ';
219700110426                  yy = %lookup(due: $wrkCaa) ;
219800110426                  If yy > 0;
219900110426                     $qtaCf(xx) += $WrkNra(yy) ;
220000110426                  Endif ;
220100110426               Endif ;
220200110426           Endfor ;
220300110426
220400110426         // telefonate potenziali dentro trattativa
220500110426           Wrkcaads = Rcacaapt ;
220600110426           Wrknrads = Rcanrapt ;
220700110426
220800110426           xx = 1 ;
220900110426           For xx by 1 to %elem($cau) ;
221000110426               contacau = $cau(xx);
221100110426               If uno <> ' ';
221200110426                  yy = %lookup(uno: $wrkCaa) ;
221300110426                  If yy > 0;
221400110426                     $qtaPt(xx) += $WrkNra(yy) ;
221500110426                  Endif ;
221600110426               Endif ;
221700110426               If due <> ' ';
221800110426                  yy = %lookup(due: $wrkCaa) ;
221900110426                  If yy > 0;
222000110426                     $qtaPt(xx) += $WrkNra(yy) ;
222100110426                  Endif ;
222200110426               Endif ;
222300110426           Endfor ;
222400110426
222500110426         // telefonate clienti    dentro trattativa
222600110426           Wrkcaads = Rcacaact ;
222700110426           Wrknrads = Rcanract ;
222800110426
222900110426           xx = 1 ;
223000110426           For xx by 1 to %elem($cau) ;
223100110426               contacau = $cau(xx);
223200110426               If uno <> ' ';
223300110426                  yy = %lookup(uno: $wrkCaa) ;
223400110426                  If yy > 0;
223500110426                     $qtaCt(xx) += $WrkNra(yy) ;
223600110426                  Endif ;
223700110426               Endif ;
223800110426               If due <> ' ';
223900110426                  yy = %lookup(due: $wrkCaa) ;
224000110426                  If yy > 0;
224100110426                     $qtaCt(xx) += $WrkNra(yy) ;
224200110426                  Endif ;
224300110426               Endif ;
224400110426           Endfor ;
224500110426
224600110426         // telefonate potenziali persi dentro trattativa
224700110426           Wrkcaads = Rcacaappt ;
224800110426           Wrknrads = Rcanrappt ;
224900110426
225000110426           xx = 1 ;
225100110426           For xx by 1 to %elem($cau) ;
225200110426               contacau = $cau(xx);
225300110426               If uno <> ' ';
225400110426                  yy = %lookup(uno: $wrkCaa) ;
225500110426                  If yy > 0;
225600110426                     $qtaPpt(xx) += $WrkNra(yy) ;
225700110426                  Endif ;
225800110426               Endif ;
225900110426               If due <> ' ';
226000110426                  yy = %lookup(due: $wrkCaa) ;
226100110426                  If yy > 0;
226200110426                     $qtaPpt(xx) += $WrkNra(yy) ;
226300110426                  Endif ;
226400110426               Endif ;
226500110426           Endfor ;
226600110426
226700110426         // telefonate inevase
226800110426           Wrkcaads = Rcacaai ;
226900110426           Wrknrads = Rcanrai ;
227000110426
227100110426           xx = 1 ;
227200110426           For xx by 1 to %elem($cau) ;
227300110426               contacau = $cau(xx);
227400110426               If uno <> ' ';
227500110426                  yy = %lookup(uno: $wrkCaa) ;
227600110426                  If yy > 0;
227700110426                     $qtaIn(xx) += $WrkNra(yy) ;
227800110426                  Endif ;
227900110426               Endif ;
228000110426               If due <> ' ';
228100110426                  yy = %lookup(due: $wrkCaa) ;
228200110426                  If yy > 0;
228300110426                     $qtaIn(xx) += $WrkNra(yy) ;
228400110426                  Endif ;
228500110426               Endif ;
228600110426           Endfor ;
228700110426
228800100526                      T_tepm += rcatepm ;
228900110426                      T_tepg += rcatepg ;
229000110426                      T_tepp += rcatepp ;
229100110426                      T_tecft += rcatecft ;
229200110426                      T_tept += rcatept ;
229300110426                      T_tect += rcatect ;
229400110426                      T_teppt += rcateppt ;
229500110426                      T_tepost += rcatepost ;
229600110426                      T_tepoa   += rcatepo30a;
229700110426                      T_tepoa   += rcatepo33a;
229800110426                      T_tecaldi += rcatecaldi;
229900110426                      T_tecalda += rcatepc31a ;
230000110426                      T_tecalda += rcatepc44a ;
230100110426                      T_teposve += rcateposve ;
230200100526                      T_tepv32a += rcatepv32a ;
230300110426                      T_temante += rcatemante;
230400100526                      T_tepc40a += rcatepc40a;
230500110426                      T_teincre += rcateincre;
230600110426                      T_tepc50a += rcatepc50a;
230700110426                      T_telese  += rcatelese ;
230800110426                      T_inevase += rcainevase;
230900100609                      T_patn    += rcapatn   ;
231000100526                      T_patm    += rcapatm   ;
231100100526                      T_pata    += rcapata   ;
231200100526                      T_patc    += rcapatc   ;
231300110119                      T_pati    += rcapati   ;
231400110119                      T_patr    += rcapatr   ;
231500100622                      T_apptn    += rcaapptn   ;
231600100622                      T_apptm    += rcaapptm   ;
231700100622                      T_appta    += rcaappta   ;
231800100622                      T_apptc    += rcaapptc   ;
231900110119                      T_appti    += rcaappti   ;
232000110119                      T_apptr    += rcaapptr   ;
232100100526                      T_aptn    += rcaaptn   ;
232200100526                      T_aptm    += rcaaptm   ;
232300100526                      T_apta    += rcaapta   ;
232400100526                      T_aptc    += rcaaptc   ;
232500110119                      T_apti    += rcaapti   ;
232600110119                      T_aptr    += rcaaptr   ;
232700100526                      T_aftn    += rcaaftn   ;
232800100526                      T_aftm    += rcaaftm   ;
232900100526                      T_afta    += rcaafta   ;
233000100526                      T_aftc    += rcaaftc   ;
233100110119                      T_afti    += rcaafti   ;
233200110119                      T_aftr    += rcaaftr   ;
233300100526                      T_ap24    += rcaap24   ;
233400100526                      T_feriegg += rcaferiegg;
233500100526                      W_feriehh = Rcaferiehh * 100 ;
233600100526                      W_fra     = Rcaufra * 100 ;
233700100526                      W_frc     = Rcaufrc * 100 ;
233800100526                      W_fuf     = Rcaufuf * 100 ;
233900100527                      wTot_H = %div(w_feriehh : 100);
234000100527                      wTot_R = %rem(w_feriehh : 100);
234100100526                      wTot_M_FP +=(Wtot_H * 60);
234200100527                      wTot_M_FP +=(Wtot_r);
234300100527                      wTot_H = %div(w_fra : 100);
234400100527                      wTot_R = %rem(w_fra : 100);
234500100526                      wTot_M_RA +=(Wtot_H * 60);
234600100527                      wTot_M_RA +=(Wtot_r);
234700100527                      wTot_H = %div(w_frc : 100);
234800100527                      wTot_R = %rem(w_frc : 100);
234900100526                      wTot_M_RC +=(Wtot_H * 60);
235000100527                      wTot_M_RC +=(Wtot_r);
235100100527                      wTot_H = %div(w_fuf : 100);
235200100527                      wTot_R = %rem(w_fuf : 100);
235300100526                      wTot_M_UF +=(Wtot_H * 60);
235400100527                      wTot_M_UF +=(Wtot_r );
235500100629        // totali appuntamenti
235600110119                      T_totapppr=Rcaapptn+Rcaappta+Rcaapptm+Rcaapptc+
235700110119                                 Rcaappti+Rcaapptr+T_totapppr;
235800100629        // totali appuntamenti di cui il primo
235900110119                      T_totapppr1=Rcapatn+Rcapata+Rcapatm+Rcapatc+
236000110119                                  Rcapati+Rcapatr+T_totapppr1  ;
236100100525                     ENDSL;
236200100526                    endif ;
236300100526                 endif ;
236400100706                 exec sql close COG;
236500100526              endif ;
236600100526           ENDFOR;
236700100525
236800110426          // preparo le schiere in stampa
236900110426           Qtapmds = Qtapmwds ;
237000110426           Qtapgds = Qtapgwds ;
237100110426           Qtappds = Qtappwds ;
237200110426           Qtacfds = Qtacfwds ;
237300110426           Qtaptds = Qtaptwds ;
237400110426           Qtactds = QtactWds ;
237500110426           Qtapptds = Qtapptwds ;
237600110426           Qtainds = Qtainwds ;
237700110426
237800100525        // imposto i campi da stampare
237900100526           rcatepm   =T_tepm   ;
238000110426           rcatepg   =T_tepg   ;
238100110428           rcatepp   =T_tepp   ;
238200110426           rcatecft  =T_tecft  ;
238300110426           rcatept   =T_tept   ;
238400110426           rcatect   =T_tect   ;
238500110426           rcateppt  =T_teppt  ;
238600110426           rcatepost =T_tepost ;
238700110426           Tottepoa  =T_tepoa  ;
238800110426           rcatecaldi=T_tecaldi;
238900110426           Tottecalda=T_tecalda;
239000110426           rcateposve=T_teposve;
239100100526           rcatepv32a =T_tepv32a ;
239200110426           rcatemante=T_temante;
239300100526           rcatepc40a =T_tepc40a ;
239400110426           rcateincre=T_teincre;
239500110426           rcatepc50a =T_tepc50a ;
239600110426           rcatelese=T_telese ;
239700110426           rcainevase=T_inevase ;
239800100609           rcapatn    =T_patn    ;
239900100609           rcapatm    =T_patm    ;
240000100609           rcapata    =T_pata    ;
240100100609           rcapatc    =T_patc    ;
240200110119           rcapati    =T_pati    ;
240300110119           rcapatr    =T_patr    ;
240400100622           rcaapptn    =T_apptn    ;
240500100622           rcaapptm    =T_apptm    ;
240600100622           rcaappta    =T_appta    ;
240700100622           rcaapptc    =T_apptc    ;
240800110119           rcaappti    =T_appti    ;
240900110119           rcaapptr    =T_apptr    ;
241000100622           rcaaptn    =T_aptn    ;
241100100622           rcaaptm    =T_aptm    ;
241200100622           rcaapta    =T_apta    ;
241300100622           rcaaptc    =T_aptc    ;
241400110119           rcaapti    =T_apti    ;
241500110119           rcaaptr    =T_aptr    ;
241600100609           rcaaftn    =T_aftn    ;
241700100609           rcaaftm    =T_aftm    ;
241800100609           rcaafta    =T_afta    ;
241900100609           rcaaftc    =T_aftc    ;
242000110119           rcaafti    =T_afti    ;
242100110119           rcaaftr    =T_aftr    ;
242200100609           rcaap24    =T_ap24    ;
242300100609           rcaferiegg=T_feriegg ;
242400100525
242500100525         // ferie
242600100525             wTot_H = %div(wTot_M_FP : 60);
242700100525             wTot_R = %rem(wTot_M_FP : 60);
242800100525             Rcaferiehh = wtot_h;
242900100525             Rcaferiehh += (Wtot_r / 100);
243000100525         // riunioni d'area
243100100525             wTot_H = %div(wTot_M_RA : 60);
243200100525             wTot_R = %rem(wTot_M_RA : 60);
243300100525             Rcaufra    = wtot_h;
243400100525             Rcaufra    += (Wtot_r / 100);
243500100525         // riunioni commerciale
243600100525             wTot_H = %div(wTot_M_RC : 60);
243700100525             wTot_R = %rem(wTot_M_RC : 60);
243800100525             Rcaufrc    = wtot_h;
243900100525             Rcaufrc    += (Wtot_r / 100);
244000100525         // attività ufficio
244100100525             wTot_H = %div(wTot_M_UF : 60);
244200100525             wTot_R = %rem(wTot_M_UF : 60);
244300100525             Rcaufuf    = wtot_h;
244400100525             Rcaufuf    += (Wtot_r / 100);
244500100629        // totali appuntamenti presi
244600100629             Totapppr = T_totapppr ;
244700100629        // totali appuntamenti presi di cui primo
244800100629             Totapppr1 = T_totapppr1 ;
244900100629          // calcolo l'incidenza  degli appuntamenti dulle telefonate effettuate
245000100622            exsr sr_incidenza ;
245100100819        // totali appuntamenti eseguiti
245200110119             Totappes = Rcaaptn + Rcaapta + Rcaaptm + Rcaaptc +
245300110119                        Rcaapti + Rcaaptr ;
245400100819        // totali affiancamenti eseguiti
245500110119             Totaffes = Rcaaftn + Rcaafta + Rcaaftm + Rcaaftc +
245600110119                        Rcaafti + Rcaaftr ;
245700100525
245800100525         //?Imposto data inizio e fine stampa
245900100525           wDate_ISO = %date(I88deci:*iso);
246000100525           wDate_EUR = wDate_ISO;
246100100525           pDatai    = %dec(wDate_EUR);
246200100525           wDate_ISO = %date(I88decf:*iso);
246300100525           wDate_EUR = wDate_ISO;
246400100525           pDataf    = %dec(wDate_EUR);
246500100525         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
246600100525           *in11 = *on ;
246700100525           *in12 = *on ;
246800100527           clear ptatio ;
246900100528         // imposto nel titolo di chi si tratta
247000100609           If (i88cscmm = 'O' and i88cscmmie = ' ') or
247100100610              (i88cscmm = 'I' and i88cscmmie = ' ') OR
247200100610              (i88cscmm = 'I' and i88cscmmie = 'T') ;
247300100609              tipostampa = 'T' ;
247400100528           endif ;
247500100528
247600100528           If (i88cscmm = 'O' and i88cscmmie = 'I') or
247700100528              (i88cscmm = 'I' and i88cscmmie = 'E') ;
247800100609              tipostampa = 'E' ;
247900100528           endif ;
248000100528
248100100528           If (i88cscmm = 'O' and i88cscmmie = 'E') or
248200100528              (i88cscmm = 'I' and i88cscmmie = 'I') ;
248300100609              tipostampa = 'I' ;
248400100528           endif ;
248500100528           *in13 = *on;
248600100525           exsr sr_StpSalto;
248700100525           write mk89rigcon ;
248800100525
248900100525       endif ;
249000100525
249100100526       // verifico se ho lanciato il preventivo
249200100526       If i88tcpe = 'P' or i88tcpe = 'E' ;
249300100526         //?Imposto data inizio e fine stampa
249400100526           wDate_ISO = %date(I88depi:*iso);
249500100526           wDate_EUR = wDate_ISO;
249600100526           pDatai    = %dec(wDate_EUR);
249700100526           wDate_ISO = %date(I88depf:*iso);
249800100526           wDate_EUR = wDate_ISO;
249900100526           pDataf    = %dec(wDate_EUR);
250000100526         // imposto l'INDICATORE che mi impedisce l'uscita della scritta
250100100526           *in11 = *on ;
250200100526           *in12 = *on ;
250300100528           *in13 = *off ;
250400100527            clear ptatio ;
250500100527       //?Ciclo di lettura dei totali preventivi in ordine ragione sociale
250600100527       // elaboro prima i commerciali interni e poi gli esterni se selezionati
250700100527       // i totali di entrambi
250800100527
250900100527         // se inclusi gli interni oppure
251000100527           If (i88tscmm='I' and i88tscmmie='I') or
251100100527         // se omessi gli esterni  oppure
251200100527               (i88tscmm='O' and i88tscmmie='E') or
251300100527         // non ci sono inclusioni e omissioni
251400100609               (i88tscmm='O' and i88tscmmie=' ') or
251500100610               (i88tscmm='I' and i88tscmmie=' ') OR
251600100610               (i88tscmm='I' and i88tscmmie='T') ;
251700100527
251800100527         // pulisco alcuni campoi di totale
251900100527               clear Wtot_m_fp ;
252000100527               clear Wtot_m_uf ;
252100100527               tipostampa = '2' ;
252200100527               *in99 = *on ;
252300100527               $End = *off ;
252400100526               exec sql
252500100527                 declare PRGI cursor for
252600100526                 select *
252700101007                 from WFRPA00f where RPAfun = 'COMIN' or RPAfun = 'ASC'
252800101007                                  or RPAfun = 'SA' order by RPAdrg ;
252900100526
253000100526                 exec sql
253100100527                 open PRGI;
253200100527
253300100527                 IF sqlcode >= 0;
253400100527
253500100527                 Dow not $End ;
253600100526
253700100526                    exec sql
253800100527                    fetch next from PRGI into :WFRPA00F ;
253900100526                    IF sqlcod = 100 or sqlcod < 0;
254000100527                       $End = *on;
254100100527                       leave;
254200100527                    Endif;
254300100526
254400100526        // calcolo totali appuntamenti
254500110119                      totapp = (rpaappm + rpaappn + rpaappa + rpaappc +
254600110119                                rpaappi + rpaappr);
254700100526        // calcolo totali telefonate
254800100526                      tottel=(rpatc19 + rpatc20 + rpatc21 + rpatc22 + rpatc24 +
254900100526                            rpatc25 + rpatc26 + rpatc27 + rpatc28 + rpatc30 +
255000110426                            rpatc31 + rpatc32 + rpatc33 + rpatc44 + rpatc60 +
255100110426                            rpatc70 + rpatc71 + rpatc73 + rpatc74 + rpatc75);
255200100526        // Verifico se affiancamenti valorizzati
255300110119                      *in30 = rpaaffn > 0 ;
255400110119                      *in31 = rpaaffi > 0 ;
255500110119                      *in32 = rpaaffa > 0 ;
255600110119                      *in33 = rpaaffm > 0 ;
255700110119                      *in34 = rpaaffc > 0 ;
255800110119                      *in35 = rpaaffr > 0 ;
255900100526         //?Salto pagina
256000100526                      IF  *in99 = *on   ;
256100100526                          exsr sr_StpSalto;
256200100526                          write Mk89rigpre ;
256300100526                          *in99 = *off;
256400100526                      ENDIF;
256500100526                      write Mk89rigpr1 ;
256600100526                      T_totapp +=totapp ;
256700100526                      T_tottel +=tottel ;
256800100526                      T_appm += rpaappm ;
256900100526                      T_appn += rpaappn ;
257000100526                      T_appa += rpaappa ;
257100100526                      T_appc += rpaappc ;
257200110119                      T_appi += rpaappi ;
257300110119                      T_appr += rpaappr ;
257400100526                      T_affm += rpaaffm ;
257500100526                      T_affn += rpaaffn ;
257600100526                      T_affa += rpaaffa ;
257700100526                      T_affc += rpaaffc ;
257800110119                      T_affi += rpaaffi ;
257900110119                      T_affr += rpaaffr ;
258000100526                      T_atc19 += rpatc19 ;
258100100526                      T_atc20 += rpatc20 ;
258200100526                      T_atc21 += rpatc21 ;
258300100526                      T_atc22 += rpatc22 ;
258400100526                      T_atc24 += rpatc24 ;
258500100526                      T_atc25 += rpatc25 ;
258600100526                      T_atc26 += rpatc26 ;
258700100526                      T_atc27 += rpatc27 ;
258800100526                      T_atc28 += rpatc28 ;
258900100526                      T_atc30 += rpatc30 ;
259000100526                      T_atc31 += rpatc31 ;
259100100526                      T_atc32 += rpatc32 ;
259200100526                      T_atc33 += rpatc33 ;
259300110117                      T_atc44 += rpatc44 ;
259400110426                      T_atc70 += rpatc70 ;
259500110426                      T_atc71 += rpatc71 ;
259600110426                      T_atc73 += rpatc73 ;
259700110426                      T_atc74 += rpatc74 ;
259800110426                      T_atc75 += rpatc75 ;
259900100526                      T_off   += rpaoff   ;
260000100526                      T_fegg  += rpafegg  ;
260100100526                      W_feriehh = Rpaferie * 100 ;
260200100526                      W_fuf     = Rpauff  * 100 ;
260300100527                      wTot_H = %div(w_feriehh : 100);
260400100527                      wTot_R = %rem(w_feriehh : 100);
260500100526                      wTot_M_FP +=(Wtot_H * 60);
260600100527                      wTot_M_FP +=(Wtot_r);
260700100527                      wTot_H = %div(w_fuf : 100);
260800100527                      wTot_R = %rem(w_fuf : 100);
260900100526                      wTot_M_UF +=(Wtot_H * 60);
261000100527                      wTot_M_UF +=(Wtot_r);
261100100527                   ENDDO ;
261200100527
261300100527                   exec sql close PRGI;
261400100526
261500100527                   totapp = T_totapp ;
261600100527                   Tottel = T_tottel ;
261700100527                   rpaappm  = T_appm ;
261800100527                   rpaappn  = T_appn ;
261900100527                   rpaappa  = T_appa ;
262000100527                   rpaappc  = T_appc ;
262100110119                   rpaappi  = T_appi ;
262200110119                   rpaappr  = T_appr ;
262300100527                   rpaaffm  = T_affm ;
262400100527                   rpaaffn  = T_affn ;
262500100527                   rpaaffa  = T_affa ;
262600100527                   rpaaffc  = T_affc ;
262700110119                   rpaaffi  = T_affi ;
262800110119                   rpaaffr  = T_affr ;
262900100527                   rpatc19= T_atc19 ;
263000100527                   rpatc20= T_atc20 ;
263100100527                   rpatc21= T_atc21 ;
263200100527                   rpatc22 = T_atc22 ;
263300100527                   rpatc24 = T_atc24 ;
263400100527                   rpatc25 = T_atc25 ;
263500100527                   rpatc26 = T_atc26 ;
263600100527                   rpatc27 = T_atc27 ;
263700100527                   rpatc28 = T_atc28 ;
263800100527                   rpatc30 = T_atc30 ;
263900100527                   rpatc31 = T_atc31 ;
264000100527                   rpatc32 = T_atc32 ;
264100100527                   rpatc33 = T_atc33 ;
264200110117                   rpatc44 = T_atc44 ;
264300100527                   rpatc60 = T_atc60 ;
264400110426                   rpatc70 = T_atc70 ;
264500110426                   rpatc71 = T_atc71 ;
264600110426                   rpatc73 = T_atc73 ;
264700110426                   rpatc74 = T_atc74 ;
264800110426                   rpatc75 = T_atc75 ;
264900100527                   rpaoff   = T_off   ;
265000100527                   rpafegg  = T_fegg  ;
265100100527               // ferie
265200100527                   wTot_H = %div(wTot_M_FP : 60);
265300100527                   wTot_R = %rem(wTot_M_FP : 60);
265400100527                   Rpaferie = wtot_h;
265500100527                   Rpaferie += (Wtot_r / 100);
265600100527               // attività ufficio
265700100527                   wTot_H = %div(wTot_M_UF : 60);
265800100527                   wTot_R = %rem(wTot_M_UF : 60);
265900100527                   Rpauff     = wtot_h;
266000100527                   Rpauff     += (Wtot_r / 100);
266100100527
266200100527              // Verifico se affiancamenti valorizzati
266300110119                   *in30 = rpaaffn > 0 ;
266400110119                   *in31 = rpaaffi > 0 ;
266500110119                   *in32 = rpaaffa > 0 ;
266600110119                   *in33 = rpaaffm > 0 ;
266700110119                   *in34 = rpaaffc > 0 ;
266800110119                   *in35 = rpaaffr > 0 ;
266900100526         //?Salto pagina
267000100527                   IF  *in99 = *on   ;
267100100527                       exsr sr_StpSalto;
267200100527                       write mk89rigpre ;
267300100527                       *in99 = *off;
267400100527                   ENDIF;
267500100527                   rpadrg = ' TOTALE GENERALE   ' ;
267600100527                   write mk89rigpr1 ;
267700100527                 endif ;
267800100527         endif ;
267900100526
268000100527         // se inclusi gli esterni oppure
268100100527           If (i88tscmm='I' and i88tscmmie='E') or
268200100527         // se omessi gli interni
268300100527               (i88tscmm='O' and i88tscmmie='I') or
268400100527         // non ci sono inclusioni e omissioni
268500100609               (i88tscmm='O' and i88tscmmie=' ') or
268600100610               (i88tscmm='I' and i88tscmmie=' ') OR
268700100610               (i88tscmm='I' and i88tscmmie='T') ;
268800100527
268900100527         // pulisco alcuni campoi di totale
269000100527               clear Wtot_m_fp ;
269100100527               clear Wtot_m_uf ;
269200100527               clear  T_totapp  ;
269300100527               clear  T_tottel  ;
269400100527               clear  T_appm  ;
269500100527               clear  T_appn  ;
269600100527               clear  T_appa  ;
269700100527               clear  T_appc  ;
269800110119               clear  T_appi  ;
269900110119               clear  T_appr  ;
270000100527               clear  T_affm ;
270100100527               clear  T_affn  ;
270200100527               clear  T_affa  ;
270300100527               clear  T_affc  ;
270400110119               clear  T_affi  ;
270500110119               clear  T_affr  ;
270600100527               clear  T_atc19 ;
270700100527               clear  T_atc20  ;
270800100527               clear  T_atc21  ;
270900100527               clear  T_atc22  ;
271000100527               clear  T_atc24  ;
271100100527               clear  T_atc25  ;
271200100527               clear  T_atc26  ;
271300100527               clear  T_atc27  ;
271400100527               clear  T_atc28  ;
271500100527               clear  T_atc30  ;
271600100527               clear  T_atc31  ;
271700100527               clear  T_atc32  ;
271800100527               clear  T_atc33  ;
271900110117               clear  T_atc44  ;
272000100527               clear  T_atc60  ;
272100110426               clear  T_atc70  ;
272200110426               clear  T_atc71  ;
272300110426               clear  T_atc73  ;
272400110426               clear  T_atc74  ;
272500110426               clear  T_atc75  ;
272600100527               clear  T_off    ;
272700100527               clear  T_fegg   ;
272800100527               tipostampa = '3' ;
272900100527               *in99 = *on ;
273000100527               $End = *off ;
273100100527               exec sql
273200100527                 declare PRGE cursor for
273300100527                 select *
273400101007                 from WFRPA00f where RPAfun <> 'COMIN' and RPAfun <> 'ASC'
273500101007                                 and RPAfun <> 'SA' order by RPAdrg ;
273600100527
273700100527                 exec sql
273800100527                 open PRGE;
273900100527                 IF sqlcode >= 0;
274000100527
274100100527                 Dow not $End ;
274200100527
274300100527                    exec sql
274400100527                    fetch next from PRGE into :WFRPA00F ;
274500100527                    IF sqlcod = 100 or sqlcod < 0;
274600100527                       $End = *on;
274700100527                       leave;
274800100527                    Endif;
274900100527
275000100527        // calcolo totali appuntamenti
275100110119                      totapp = (rpaappm + rpaappn + rpaappa + rpaappc +
275200110119                                rpaappi + rpaappr );
275300100527        // calcolo totali telefonate
275400100527                      tottel=(rpatc19 + rpatc20 + rpatc21 + rpatc22 + rpatc24 +
275500100527                            rpatc25 + rpatc26 + rpatc27 + rpatc28 + rpatc30 +
275600110426                            rpatc31 + rpatc32 + rpatc33 + rpatc44 + rpatc60 +
275700110426                            rpatc70 + rpatc71 + rpatc73 + rpatc74 + rpatc75);
275800100527        // Verifico se affiancamenti valorizzati
275900110119                      *in30 = rpaaffn > 0 ;
276000110119                      *in31 = rpaaffi > 0 ;
276100110119                      *in32 = rpaaffa > 0 ;
276200110119                      *in33 = rpaaffm > 0 ;
276300110119                      *in34 = rpaaffc > 0 ;
276400110119                      *in35 = rpaaffr > 0 ;
276500100527         //?Salto pagina
276600100527                      IF  *in99 = *on   ;
276700100527                          exsr sr_StpSalto;
276800100527                          write Mk89rigpre ;
276900100527                          *in99 = *off;
277000100527                      ENDIF;
277100100527                      write Mk89rigpr1 ;
277200100527                      T_totapp +=totapp ;
277300100527                      T_tottel +=tottel ;
277400100527                      T_appm += rpaappm ;
277500100527                      T_appn += rpaappn ;
277600100527                      T_appa += rpaappa ;
277700100527                      T_appc += rpaappc ;
277800110119                      T_appi += rpaappi ;
277900110119                      T_appr += rpaappr ;
278000100527                      T_affm += rpaaffm ;
278100100527                      T_affn += rpaaffn ;
278200100527                      T_affa += rpaaffa ;
278300100527                      T_affc += rpaaffc ;
278400110119                      T_affi += rpaaffi ;
278500110119                      T_affr += rpaaffr ;
278600100527                      T_atc19 += rpatc19 ;
278700100527                      T_atc20 += rpatc20 ;
278800100527                      T_atc21 += rpatc21 ;
278900100527                      T_atc22 += rpatc22 ;
279000100527                      T_atc24 += rpatc24 ;
279100100527                      T_atc25 += rpatc25 ;
279200100527                      T_atc26 += rpatc26 ;
279300100527                      T_atc27 += rpatc27 ;
279400100527                      T_atc28 += rpatc28 ;
279500100527                      T_atc30 += rpatc30 ;
279600100527                      T_atc31 += rpatc31 ;
279700100527                      T_atc32 += rpatc32 ;
279800100527                      T_atc33 += rpatc33 ;
279900110117                      T_atc44 += rpatc44 ;
280000100527                      T_atc60 += rpatc60 ;
280100110426                      T_atc70 += rpatc70 ;
280200110426                      T_atc71 += rpatc71 ;
280300110426                      T_atc73 += rpatc73 ;
280400110426                      T_atc74 += rpatc74 ;
280500110426                      T_atc75 += rpatc75 ;
280600100527                      T_off   += rpaoff   ;
280700100527                      T_fegg  += rpafegg  ;
280800100527                      W_feriehh = Rpaferie * 100 ;
280900100527                      W_fuf     = Rpauff  * 100 ;
281000100527                      wTot_H = %div(w_feriehh : 100);
281100100527                      wTot_R = %rem(w_feriehh : 100);
281200100527                      wTot_M_FP +=(Wtot_H * 60);
281300100527                      wTot_M_FP +=(Wtot_r);
281400100527                      wTot_H = %div(w_fuf : 100);
281500100527                      wTot_R = %rem(w_fuf : 100);
281600100527                      wTot_M_UF +=(Wtot_H * 60);
281700100527                      wTot_M_UF +=(Wtot_r);
281800100527                   ENDDO ;
281900100527
282000100527                     exec sql close PRGE;
282100100527
282200100527                   totapp = T_totapp ;
282300100527                   Tottel = T_tottel ;
282400100527                   rpaappm  = T_appm ;
282500100527                   rpaappn  = T_appn ;
282600100527                   rpaappa  = T_appa ;
282700100527                   rpaappc  = T_appc ;
282800110119                   rpaappi  = T_appi ;
282900110119                   rpaappr  = T_appr ;
283000100527                   rpaaffm  = T_affm ;
283100100527                   rpaaffn  = T_affn ;
283200100527                   rpaaffa  = T_affa ;
283300100527                   rpaaffc  = T_affc ;
283400110119                   rpaaffi  = T_affi ;
283500110119                   rpaaffr  = T_affr ;
283600100527                   rpatc19= T_atc19 ;
283700100527                   rpatc20= T_atc20 ;
283800100527                   rpatc21= T_atc21 ;
283900100527                   rpatc22 = T_atc22 ;
284000100527                   rpatc24 = T_atc24 ;
284100100527                   rpatc25 = T_atc25 ;
284200100527                   rpatc26 = T_atc26 ;
284300100527                   rpatc27 = T_atc27 ;
284400100527                   rpatc28 = T_atc28 ;
284500100527                   rpatc30 = T_atc30 ;
284600100527                   rpatc31 = T_atc31 ;
284700100527                   rpatc32 = T_atc32 ;
284800100527                   rpatc33 = T_atc33 ;
284900110117                   rpatc44 = T_atc44 ;
285000100527                   rpatc60 = T_atc60 ;
285100110426                   rpatc70 = T_atc70 ;
285200110426                   rpatc71 = T_atc71 ;
285300110426                   rpatc73 = T_atc73 ;
285400110426                   rpatc74 = T_atc74 ;
285500110426                   rpatc75 = T_atc75 ;
285600100527                   rpaoff   = T_off   ;
285700100527                   rpafegg  = T_fegg  ;
285800100527               // ferie
285900100527                   wTot_H = %div(wTot_M_FP : 60);
286000100527                   wTot_R = %rem(wTot_M_FP : 60);
286100100527                   Rpaferie = wtot_h;
286200100527                   Rpaferie += (Wtot_r / 100);
286300100527               // attività ufficio
286400100527                   wTot_H = %div(wTot_M_UF : 60);
286500100527                   wTot_R = %rem(wTot_M_UF : 60);
286600100527                   Rpauff     = wtot_h;
286700100527                   Rpauff     += (Wtot_r / 100);
286800100527
286900100527              // Verifico se affiancamenti valorizzati
287000110119                   *in30 = rpaaffn > 0 ;
287100110119                   *in31 = rpaaffi > 0 ;
287200110119                   *in32 = rpaaffa > 0 ;
287300110119                   *in33 = rpaaffm > 0 ;
287400110119                   *in34 = rpaaffc > 0 ;
287500110119                   *in35 = rpaaffr > 0 ;
287600100527         //?Salto pagina
287700100527                   IF  *in99 = *on   ;
287800100527                       exsr sr_StpSalto;
287900100527                       write mk89rigpre ;
288000100527                       *in99 = *off;
288100100527                   ENDIF;
288200100527                   rpadrg = ' TOTALE GENERALE   ' ;
288300100527                   write mk89rigpr1 ;
288400100527                 endif ;
288500100527         endif ;
288600100526       endif ;
288700100525       ENDSR;
288800100525
288900080206       //--------------------------------------------------------------
289000080206       //?Reperimento Dati del job (Utente/Operativi).
289100080206       //--------------------------------------------------------------
289200080206       BEGSR DatiJob;
289300080206
289400080206         in(E) §AzUte;
289500080206         if NOT %error;
289600080206           in(E) §DatiUte;
289700080206         endif;
289800080206         if %error or RSut = *blanks;
289900080206           clear TIBS34ds;
290000080206           tibs34r(tibs34ds);
290100080206           in §AzUte;
290200080206           in §DatiUte;
290300080206         endif;
290400100429
290500100429         //?Richiamo pgm di controllo autorizzazioni utente così mi torna
290600100429         //?il codice autorizzazione relativo alle tariffe clienti
290700100429         //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
290800100429         reset TNTAA1DS;
290900100429         ITAA1caut = '§utegtc';
291000100429         kpjbu     = TNTAA1DS;
291100100429         tntaa1c (kpjba);
291200100429         TNTAA1DS = kpjbu;
291300100429
291400150910         //?Se utente abilitato come 'AZ' mi carico
291500150910         //?la sk delle filiali del distretto
291600100429         IF  OTAA1cabi = 'AZ' and DUTpou <> 046;
291700150910           OTAA1cabi = 'DI';
291800100429         ENDIF;
291900100429
292000100429         //?Carico filiali abilitate all'utente
292100100429         //?Non controllo se caricate o no le filiali
292200100429         //?tanto se non ho caricato nessuna filiale e la richiesta
292300100429         //?è per tutti i commerciali, non stampo niente
292400150921         //?Se richiesta un'area carico SOLO le filiali di quell'area
292500150921         IF  I88car > 0;
292600150921             clear TRUL31DS;
292700150921             clear TRUL31DS2;
292800150921             I31abi = 'AO';
292900150921             I31car = I88car ;
293000150921             trul31r (kpjba : trul31ds : trul31ds2);
293100150921         Else ;
293200150921             clear TRUL31DS;
293300150921             clear TRUL31DS2;
293400150921             if Dutpou = 046 ;
293500150921                I31abi = 'AZ'     ;
293600150921             else ;
293700150921                I31abi = OTAA1cabi;
293800150921                I31cdi = DUTdis;
293900150921                I31car = DUTare;
294000150921                I31cpo = DUTpou;
294100150921             Endif;
294200150921             trul31r (kpjba : trul31ds : trul31ds2);
294300150921         Endif;
294400110120         IF  car(2) > *zeros;
294500110120           *in09 = *on;
294600110120         ENDIF;
294700080206
294800080206       ENDSR;
294900100305
295000100305       //--------------------------------------------------------------
295100100305       //?Carico Sk Agenti.
295200100305       //--------------------------------------------------------------
295300100305       BEGSR sr_CarAGE;
295400100305
295500100305         clear xx;
295600100305         clear $Age;
295700100429         $EndAge = *off;
295800100305
295900100305         exec sql
296000100305          declare AGE cursor for
296100130917          select CMMcod, CMMuni
296200130917          from AZCMM00F
296300130917          order by CMMcod;
296400100305
296500100305         exec sql
296600100305           open AGE;
296700100305           IF sqlcode < 0;
296800100305             $EndAge = *on;
296900100305           ENDIF;
297000100305
297100100305         DOW not $EndAge;
297200100305           exec sql
297300130917             fetch next from AGE into :CMMcod, :CMMuni;
297400100305             IF sqlcod = 100 or sqlcod < 0;
297500100305               $EndAge = *on;
297600100305               leave;
297700100305             ENDIF;
297800130917             IF  CMMuni = $AgeU(zz);
297900100305               xx += 1;
298000130917               $Age(xx) = CMMcod;
298100100305             ENDIF;
298200100305         ENDDO;
298300100305
298400100305         exec sql close AGE;
298500100305
298600100305         //?Se non ho cricato niente memorizzo almeno il codice richiesto
298700100305         IF $Age(01) = 0;
298800100429           $Age(01) = $AgeU(zz);
298900100305         ENDIF;
299000100305
299100100305       ENDSR;
299200100429
299300100429       //--------------------------------------------------------------
299400100429       //?Carico sk agenti unificanti.
299500100429       //--------------------------------------------------------------
299600100429       BEGSR sr_CarAgeUni;
299700100429
299800100429         clear xx;
299900100429         clear $AGEU;
300000100429         $EndAge = *off;
300100100429
300200100429           //?Imposto la stringa per SQL
300300130917           wSQL = 'select CMMcod +
300400130917                   from AZCMM00F +
300500130917                   where substr(digits(CMMuni), 1, 3) in (';
300600100429
300700100429           yy = 0;
300800100429           xx = 1;
300900100429           FOR xx by 1 to %elem(pog);
301000100429             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
301100100429               IF yy > 0;
301200100429                 wSQL += ', ';
301300100429               ELSE;
301400100429                 yy = 1;
301500100429               ENDIF;
301600100429               wSQL += '''';
301700100429               wSQL += pog(xx);
301800100429               wSQL += '''';
301900100429             ENDIF;
302000100429           ENDFOR;
302100100505
302200100505         //?Carico TUTTI gli agenti unificanti
302300130917           wSQL += ') and CMMcod = CMMuni +
302400130917                      and CMMpar = '' ''';
302500100429
302600130917         wSQL += ' order by CMMcod +
302700100505                   for fetch only';
302800100429
302900100429         //?Dichiarazione cursore
303000100429         exec sql
303100100429           prepare A1   from :wSQL;
303200100429         exec sql
303300100429           declare AGEU cursor for A1;
303400100429
303500100429         //?Apertura del cursore
303600100429         exec sql
303700100429           open AGEU;
303800100429
303900100429         IF sqlcode < 0;
304000100429           $EndAge = *on;
304100100429         ENDIF;
304200100429
304300100429         yy = 0;
304400100429         DOW not $EndAge;
304500100429           exec sql
304600130917             fetch next from AGEU into :CMMcod;
304700100429           IF sqlcod = 100 or sqlcod < 0;
304800100429             $EndAge = *on;
304900100429             leave;
305000100429           ENDIF;
305100100429
305200130917           yy += 1;
305300130917           $AGEU(yy) = CMMcod;
305400100429
305500100429         ENDDO;
305600100429
305700100429         exec sql
305800100429           close AGEU;
305900100429
306000100429       ENDSR;
306100100305
306200100305       //--------------------------------------------------------------
306300100305       //?Preparazione stringa SQL.
306400100305       //--------------------------------------------------------------
306500100305       BEGSR sr_PrepSQL;
306600100305
306700100308         wSQL = 'select atctat, atccpo, atcksc, +
306800100309                 atcatn, atcatnp, atcdad, atchda, atcnrv, +
306900100628                 atcoii, atcofi, atccad, atcco3, atcdco, +
307000100525                 atccac, atcest +
307100100305                  from TIATC00F +
307200100305                  where atccmm in(';
307300100305
307400100305         yy = 0;
307500100305         xx = 1;
307600100305         FOR xx by 1 to %elem($Age);
307700100305           IF $Age(xx) <> *zeros;
307800100305             IF yy > 0;
307900100305               wSQL += ', ';
308000100305             ELSE;
308100100305               yy = 1;
308200100305             ENDIF;
308300100305             wSQL += %editc($Age(xx):'X');
308400100305           ENDIF;
308500100305         ENDFOR;
308600100305
308700100308         wSQL += ') and atcdad between ' + %editc(IMK86datai:'X') +
308800100308                 ' and ' + %editc(IMK86dataf:'X');
308900100305
309000100305         wSQL += ' order by atcdad, atchda, atctat +
309100100305                   for fetch only';
309200100305
309300100305       ENDSR;
309400100305
309500100305       //--------------------------------------------------------------
309600100305       //?Controllo se il rcd che sto leggendo è OK per l'agenda.
309700100305       //--------------------------------------------------------------
309800100305       BEGSR Sr_CtrRcdOk;
309900100305
310000100305         $RcdOk = *on;
310100100513         // se c'è inclusione o omissione di alcuni tipi attività
310200100513         // controllo
310300100513         SELECT ;
310400100513            When imk86iot = 'I' and %lookup(atctat : $tat) = 0 ;
310500100513                 $RcdOk = *off;
310600100513                 leavesr ;
310700100513            When imk86iot = 'O' and %lookup(atctat : $tat) > 0 ;
310800100513                 $RcdOk = *off;
310900100513                 leavesr ;
311000100513         ENDSL ;
311100100305         //?se attività legata a trattativa
311200100305         //?la trattativa NON deve essere fittizia
311300100305         IF  atcnrv > 0;
311400100305           exec sql
311500100305             select visffz into :visffz from tivis05l
311600100305             where visnrv = : atcnrv;
311700100305         //?non trovo il rcd corrispondente
311800100305           IF  sqlcod <> 0;
311900100305             leavesr;
312000100305           ENDIF;
312100100305         //?se trattativa fittizia non carico a video
312200100305           IF  visffz = 'S';
312300100305             $RcdOk = *off;
312400100305           ENDIF;
312500100305         ENDIF;
312600100525
312700100525         //?CHIODO
312800100712         //?MODIFICA CONTROLLO 06 Luglio 2010 ora controllo solo esito senza causale
312900100712         //?Se appuntamento '91' o affiancamento '92'  e esito 'N'
313000100712         IF  (ATCcad = '91' or ATCcad = '92') and ATCest = 'N';
313100100712           $RcdOk = *off;
313200100712         ENDIF;
313300100525         //?Se attvità qualsiasi chiusa con attività 'AA'
313400100525         //?non devo caricare in agenda
313500100525         IF  ATCcac = 'AA';
313600100525           $RcdOk = *off;
313700100525         ENDIF;
313800100305
313900100305       ENDSR;
314000100309
314100100309       //--------------------------------------------------------------
314200100309       //?Decodifico agente.
314300100309       //--------------------------------------------------------------
314400100309       BEGSR sr_DecCmm;
314500100309
314600130917         chain  (wCmm)  AZCMM000;
314700130917         IF  NOT  %found( AZCMM01L );
314800130917           clear CMMdes;
314900130917           clear CMMfun;
315000100309         ENDIF;
315100100309
315200100309       ENDSR;
315300090807
315400080206       //--------------------------------------------------------------
315500080206       //?Operazioni finali.
315600080206       //--------------------------------------------------------------
315700080206       BEGSR RoutEnd;
315800090521
315900080206         *inLR = *on;
316000080206         return;
316100080206
316200080206       ENDSR;
316300080206
316400080206      /end-free
316500110422** CAUSALI IN STAMPA CONSUNTIVO
31660011042214/15
31670011042219/20
316800110422 21
316900110422 22
317000110422 24
317100110422 25
317200110422 26
317300110422 27
317400110422 28
31750011042230/33
317600110422 32
31770011051831/44
317800110422 41
317900110422 42
318000110422 60
31810011042270/75
318200110422 71
318300110518 72
31840011042273/74
318500110422 80
318600110422 81
318700110422 91
318800110422
318900110531
319000110422** CAUSALI CORRISPONDENTI AD OGNI BUCO DI STAMPA
3191001104221415
3192001104221920
31930011042221
31940011042222
31950011042224
31960011042225
31970011042226
31980011042227
31990011042228
3200001104223033
32010011042232
3202001105183144
32030011042241
32040011042242
32050011042260
3206001104227075
32070011042271
32080011051872
3209001104227374
32100011042280
32110011042281
32120011042291
321300110422
321400110531
