000100110414      *PARMS DFTACTGRP(*NO) ACTGRP(*caller) OPTION(*NOXREF)
000200090320      *PARMS CLOSQLCSR(*ENDMOD)
000300090320      *PARMS DYNUSRPRF(*OWNER)
000400100524     /*PRM dbgview(*source)
000500100524     /*PRM commit(*none)
000600100524     /*END
000700080206      //--------------------------------------------------------------
000800090609      //?TRMK18R - GESTIONE CONTATTI / TELEFONATE / APPUNTAMENTI
000900080206      //--------------------------------------------------------------
001000080206
001100090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001200090601     h dftactgrp(*no) actgrp(*caller)
001300080206
001400080206      //---------------------------------------------------------------
001500080206      //?Dichiarazione file.
001600080206      //---------------------------------------------------------------
001700050704
001800080208      // - Anagrafica clienti potenziali
001900090609     fTNCPO01L  If   e           k disk
002000091020      // - Note potenziali / attività
002100100520     fTICPN05L  if   e           k disk
002200100617      // - Archivio Trattative commerciale
002300100617     fTIVIS05L  if   e           k disk
002400141124      // -?Campagne Commerciali?
002500141124     fTICMP01L  if   e           k disk
002600130808      // -?Anagrafica Commerciali?
002700130808     fAZCMM01L  if   e           k disk
002800080208      // - Organigramma filiale/aziendale
002900080206     fAZORG01L  if   e           k disk
003000100720      // - Tiatc00f
003100100720     fTIATC01l  if   e           k disk
003200091130      // - Nuove Tabelle
003300091130     fTNTBE01L  if   e           k disk
003400080206
003500090317      // - Video Gestione contatti telefonate potenziali clienti
003600090303     fTRMK18D   cf   e             workstn
003700090303     f                                     sfile(MK18S01 : S01nrr)
003800080208     f                                     indds(IndDspF)
003900080206     f                                     infds(InfDspF)
004000090601     f                                     usropn
004100080206
004200080206      //---------------------------------------------------------------
004300090406      //?Definizione costanti.
004400080206      //---------------------------------------------------------------
004500050519
004600080207      // - Numero di record in ciascuna videata di subfile
004700090609     d c_SflPag        c                   const(17)
004800080207
004900080207      // - Tasti funzionali a video
005000080207     d c_F01           c                   const(x'31')
005100080207     d c_F02           c                   const(x'32')
005200080207     d c_F03           c                   const(x'33')
005300090406     d c_F04           c                   const(x'34')
005400080207     d c_F05           c                   const(x'35')
005500080207     d c_F06           c                   const(x'36')
005600080207     d c_F07           c                   const(x'37')
005700080207     d c_F08           c                   const(x'38')
005800080207     d c_F09           c                   const(x'39')
005900080207     d c_F10           c                   const(x'3A')
006000090303     d c_F11           c                   const(x'3B')
006100080207     d c_F12           c                   const(x'3C')
006200080207     d c_F13           c                   const(x'B1')
006300080207     d c_F14           c                   const(x'B2')
006400080207     d c_F15           c                   const(x'B3')
006500080207     d c_F16           c                   const(x'B4')
006600080207     d c_F17           c                   const(x'B5')
006700080207     d c_F18           c                   const(x'B6')
006800080207     d c_F19           c                   const(x'B7')
006900080207     d c_F20           c                   const(x'B8')
007000080207     d c_F21           c                   const(x'B9')
007100080207     d c_F22           c                   const(x'BA')
007200080207     d c_F23           c                   const(x'BB')
007300080207     d c_F24           c                   const(x'BC')
007400080207     d c_Enter         c                   const(x'F1')
007500080207     d c_RollDown      c                   const(x'F4')
007600080207     d c_RollUp        c                   const(x'F5')
007700080214
007800080214      // - Attributi di visualizzazione
007900080215      //  -  DspAtr() - Normale
008000080214     d C_dspatr_Norm   c                   const(x'20')
008100080215      //  -  DspAtr(RI)
008200080214     d C_dspatr_RI     c                   const(x'21')
008300081021     d C_dspatr_HI     c                   const(x'22')
008400080215      //  -  DspAtr(ND)
008500080214     d C_dspatr_ND     c                   const(x'27')
008600080215      //  -  DspAtr(BL) / Color(Red)
008700080214     d C_dspatr_BL     c                   const(x'28')
008800100209
008900100209     d Digits          c                   const('0123456789')
009000080206
009100080206      //---------------------------------------------------------------
009200080206      //?Definizione schiere.
009300080206      //---------------------------------------------------------------
009400100209
009500100318      // - Agenti Unificanti in gestione all'utente
009600100210     d $AgeU           s              7    dim(3500)
009700100210
009800100318      // - Agenti
009900100318     d $Age            s              7    dim(3500)
010000080206
010100080206      // - Messaggi di errore
010200150219     d $Msg            s             78    dim(19) ctdata perrcd(1)
010300100209
010400091130      // - Attività abilitati
010500091130     d $tta            s              1    dim(10)
010600080206
010700080206      //---------------------------------------------------------------
010800080206      //?Definizione aree dati.
010900080206      //---------------------------------------------------------------
011000080206
011100080206      // - Dati utente
011200080206     d §AzUte        e ds                  extname(AZUTE00F)
011300080206     d                                     dtaara
011400080206     d §DatiUte      e ds                  extname(dDatiUte)
011500080206     d                                     dtaara
011600090515     d tiatcDS       e ds                  ExtName(tiatc00f)
011700080206
011800080206      //---------------------------------------------------------------
011900080206      //?Definizione strutture dati.
012000080206      //---------------------------------------------------------------
012100080206
012200080206      // - Status
012300080206     d Psds           sds
012400080206     d   SDSpgm          *proc
012500080206
012600080206      // - InfDS
012700080206     d InfDspF         ds
012800080207     d  dsp_aid              369    369a                                        AID byte
012900130808     d**sfl_rrn              376    377i 0                                      Subfile rrn
013000130808     d**min_nrr              378    379i 0                                      Subfile min rrn
013100130808     d**num_rcds             380    381i 0                                      Subfile num rcds
013200080206
013300080206      // - Indicatori su DspF
013400080208     d IndDspF         ds
013500100617        // - Indicatori per ordinamento sbfl
013600100617     d  ORD_Xdata                     1n   overlay(IndDspF : 07)
013700100617     d  ORD_Xprovinc                  1n   overlay(IndDspF : 08)
013800100617     d  ORD_Xcausale                  1n   overlay(IndDspF : 09)
013900100617        // - Indicatori di visualizzazione
014000100617     d  da_eseguire                   1n   overlay(IndDspF : 10)
014100100617     d  Coldcosca                     1n   overlay(IndDspF : 19)
014200110803     d  Sede                          1n   overlay(IndDspF : 20)
014300100617        // - Indicatori di emissione messaggio di errore
014400100617     d  errMessage                    1n   overlay(IndDspF : 28)
014500080206        // - Indicatori di gestione del subfile
014600080206     d  SflDsp_N                      1n   overlay(IndDspF : 30)
014700080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
014800080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
014900080206     d  SflEnd                        1n   overlay(IndDspF : 33)
015000090422        // - Indicatori di errore 1° videata
015100090317     d  PosCurCmg                     1n   overlay(IndDspF : 50)
015200090317     d  PosCurCac                     1n   overlay(IndDspF : 51)
015300090317     d  PosCurAsc                     1n   overlay(IndDspF : 52)
015400090317     d  PosCurCcm                     1n   overlay(IndDspF : 53)
015500090317     d  PosCurCp1                     1n   overlay(IndDspF : 54)
015600090317     d  PosCurEda                     1n   overlay(IndDspF : 55)
015700090317     d  PosCurEal                     1n   overlay(IndDspF : 56)
015800090317     d  PosCurFlt                     1n   overlay(IndDspF : 57)
015900100121     d  PosCurKsc                     1n   overlay(IndDspF : 58)
016000141124     d  PosCurNCM                      n   overlay(IndDspF : 59)
016100090529        // - Indicatori di errore subfile
016200090529     d  PosCurOpz                     1n   overlay(IndDspF : 50)
016300090422        // - Indicatori di errore generico
016400080206     d  errGenerico                   1n   overlay(IndDspF : 99)
016500080206
016600090407
016700090407      //  codice cliente potenziale a video
016800090427     d V01cpo          ds                  inz
016900090427     d   V01cp1                            inz
017000090427     d   V01cp2                            inz
017100090319
017200090319     d wlbdat          ds                  inz
017300090319     d  g02dat                 1      8  0
017400090319     d  g02inv                 9     16  0
017500090319     d  g02err                17     17
017600090319     d  g02tgi                18     22  0
017700080206
017800080206      // - Parametri ricevuti
017900080206     d KPJBA         e ds
018000080206
018100080206      // - Reperimento dati utente
018200080206     d TIBS34ds      e ds
018300081020     d TRUL31DS      e ds
018400081020     d POG                    10    759    DIM(250)
018500100121
018600100121       // - Reperimento dati anagrafici
018700100121     d TIBS69ds      e ds
018800100121     d DS_cnaco      e ds                  inz extname(CNACO00F)
018900100121     d DS_cnind      e ds                  inz extname(CNIND00F)
019000100121     d DS_cnclp      e ds                  inz extname(CNCLP00F)
019100100121     d DS_fncls      e ds                  inz extname(FNCLS00F)
019200080206
019300080206      // - Ricerca/Controllo tabelle
019400080206     d TIBS02ds      e ds                  inz
019500080206     d   T02Mod      e                     inz('C')
019600100209
019700100209      // - controllo abilitazioni
019800100209     d TNTAA1DS      e ds                  inz
019900090521
020000090609      // - Ricerca Causali contatto
020100090521     d Tntb74ds      e ds                  inz
020200090529
020300090402      // - Tabella CCO = Causale contatto
020400090402     d dcco          e ds                  inz
020500090402
020600091130      // - Tabella TTA = Tipo Attività
020700091130     d dtta          e ds                  inz
020800090423
020900090609      // - Gestione contatti
021000090609     d TRMK19ds      e ds                  inz
021100090928
021200090420      // - Interrogazione anagrafica clienti potenziali
021300091104     d TRMK01ds      e ds                  inz
021400100430
021500100430      // -  Interrogazione agenda
021600100430     d TRMK82DS      e ds                  inz
021700090629
021800100223      // - Gestione trattative commerciali
021900100223     d TNTA88ds      e ds                  inz
022000100223
022100080206      //---------------------------------------------------------------
022200080206      //?Definizione variabili globali.
022300080206      //---------------------------------------------------------------
022400080206
022500080206      // - Flags booleani
022600100210     d $End            s               n   inz(*off)
022700080208     d $Fine           s               n   inz(*off)
022800080208     d $InzD01         s               n   inz(*on)
022900080208     d $InzS01         s               n   inz(*on)
023000080206     d $ErrGrave       s               n   inz(*off)
023100080207     d $EoF            s               n   inz(*off)
023200080208     d $RecOK          s               n   inz(*off)
023300090601     d $parz_x_Loc     s               n   inz(*off)
023400100617     d $parz_x_Cap     s               n   inz(*off)
023500090609     d $parz_x_Cca     s               n   inz(*off)
023600100617     d $parz_x_Cmm     s               n   inz(*off)
023700100317     d $Uni            s               n   inz(*off)
023800100318     d $UteNoAbi       s               n   inz(*off)
023900090609
024000080206
024100080206      // - Indici di schiera
024200100120     d xx              s              4  0 inz
024300100210     d yy              s              4  0 inz
024400100916     d ww              s              4  0 inz
024500100916     d zz              s              4  0 inz
024600080206
024700080207      // - Campi associati al video
024800080207     d $Video          s              2    inz('D1')
024900080208     d S01nrr          s              4  0 inz
025000080207     d W_SflPag        s              3  0 inz
025100130808     d*// wPag            s              4  0 inz
025200130808     d*// wRig            s              3  0 inz
025300090601
025400090601     d dsp_mod         s             10I 0
025500080206
025600090401      // - Campi di comodo data
025700090401     d  data_iso       s               d   Datfmt(*iso)
025800090401     d  data_dmy       s               d   Datfmt(*dmy)
025900090401
026000080206      // - Campi di comodo
026100090427     d  w01asc         s              8  0
026200090427     d  w01eda         s              8  0
026300090427     d  w01eal         s              8  0
026400090714     d  datacor        s              8  0
026500100802     d  wabi           s              2
026600100318     d  wage           s              7
026700090323     d  $Sel           s              1    inz                                  "E"=eseguite
026800111227     d wOggi           s              8s 0
026900090323     d                                                                          " "=da eseguire
027000150102     d*//  Prgcontatto    s                   like(atcatnp)
027100090929     d  Cpo_scelto     s             11  0
027200100121     d  Ksc_scelto     s                   like(acoksc)
027300100319
027400100319     d  sav_C01loc     s                   like(C01loc)
027500100617     d  sav_C01cap     s                   like(C01cap)
027600100319     d  sav_C01cca     s                   like(C01cca)
027700100617     d  sav_C01cmm     s                   like(C01cmm)
027800080206
027900100209       // - Stringhe SQL da eseguire
028000101102     d StringaSql      s           2048    Varying        inz
028100100209     d wSQL            s           2048    Varying        inz
028200090323
028300090508      //---------------------------------------------------------------
028400090508      //?Ordinamento SBFL
028500090508      //---------------------------------------------------------------
028600090508      // ----------------------------------------------------------------------
028700090508      //?Costanti per ordinamento subfile
028800090508      // ----------------------------------------------------------------------
028900090625     d MaxKey          c                   4
029000090508     d Ascendente      c                   1
029100090508     d Discendente     c                   2
029200090508     d Carattere       c                   6
029300090508     d Numerico        c                   8
029400090508     d Put             c                   1
029500090508     d EndPut          c                   2
029600090508     d Get             c                   3
029700090508      **********************************************************************
029800090508      * Campi utili:
029900090508      *     nrr        - Numero relativo di record del Subfile
030000090508      *     SizeList   - Dimensione della lista
030100090508      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
030200090508      **********************************************************************
030300090508     d NotUsed         s             16A
030400090508     d ReturnSize      s              9B 0
030500090508     d SizeList        s              9B 0
030600090508     d RrnLast         s              5I 0
030700150102     d WrkSort         s              1  0 inz(0)
030800090508      **********************************************************************
030900090508      * Data Structures
031000090508      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
031100090508      *     QLGSCB     - The sort request block for the QLGSORT API
031200090508      *     QLGSCB00   - The sort request block for the QLGSRTIO API
031300090508      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
031400090508      *     QUSEC      - Error structure for the QLGSORT API
031500090508      **********************************************************************
031600090508     d sflrcd          ds
031700090508     d  sh1dco
031800090508     d  s01hco
031900090601     d  s01prv
032000090625     d  sh1ord
032100090508     d  s01ccm
032200090508     d  s01ccmd
032300090601     d  s01cac
032400090527     d  s01loc
032500090508     d  sh1tco
032600090508     d  sh1nco
032700090508     d  sh1ncop
032800090508     d  sh1fatto
032900090508     d  sh1cac
033000090508     d  s01rag
033100090515     d  s01tco
033200090508     d  s01dco
033300090508     d  s01cacd
033400100617     d  sh1cpo
033500090601     d  sh1loc
033600090629     d  s01no1
033700090713     d  sh1n19
033800091019     d  s01ast
033900100617     d  sh1ksc
034000100105     d  sh1opz
034100100308     d  s01cic
034200100316     d  sh1nrv
034300100617     d  SH1cap
034400100617     d  SH1cmm
034500141125     d  S01ncm
034600090508     d  selected                      1A
034700090508
034800090508      /COPY QSYSINC/QRPGLESRC,QLGSORT
034900090508     d QLGKL                         16    DIM(MaxKey)
035000090508     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
035100090508     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
035200090508     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
035300090508     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
035400090508
035500090508      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
035600090508      /COPY QSYSINC/QRPGLESRC,QUSEC
035700090508
035800080208
035900090508      //---------------------------------------------------------------
036000090508      //?Definizione procedure usate.
036100090508      //---------------------------------------------------------------
036200100802
036300141201       // - Interrogazione anagrafica clienti
036400150112     d TRKC20ds      e ds                  inz
036500150112     d TRKC20R         pr                  extpgm('TRKC20R')
036600141201     d  kpjba                              likeds(kpjba)
036700150112     d  TRKC20ds                           likeds(TRKC20ds)
036800100408
036900100408      *GestioneInterrogazione_potenziale...
037000100408     d TRMK01R         pr                  extpgm('TRMK01R')
037100100408     d  kpjba                              likeds(kpjba)
037200100408     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
037300100430
037400100430       // - Pgm Agenda
037500100430     D TRMK82R         pr                  extpgm('TRMK82R')
037600100430     D  kpjba                              likeds(kpjba)
037700080206
037800080626      //---------------------------------------------------------------
037900080626      //?prototipi
038000080626      //---------------------------------------------------------------
038100080626      /copy gaitrasrc/srcprotopr,tibs02r
038200080626      /copy gaitrasrc/srcprotopr,tibs34r
038300100121      /copy gaitrasrc/srcprotopr,tibs69r
038400100209      /copy gaitrasrc/srcprotopr,tntaa1c
038500100223      /copy gaitrasrc/srcprotopr,tnta88r
038600090609      /copy gaitrasrc/srcprotopr,trmk19r
038700130808      /copy gaitrasrc/srcprotoPI,TRMK43R_1
038800130808      /copy gaitrasrc/srcprotoPR,TRMK43R
038900081020      /copy gaitrasrc/srcprotopr,trul31r
039000090406      /copy gaitrasrc/srcprotopr,xsrda8
039100090521      /copy gaitrasrc/srcprotopr,tntb74r
039200141125
039300141125       // -?Interrogazione Campagne?
039400150112     d TRKC02ds      e ds                  inz
039500150112     d trkc02r         pr                  extpgm('TRKC02R')
039600141125     d   kpjba                             likeds(kpjba)
039700150112     d   TRKC02ds                          likeds(TRKC02ds)
039800141111
039900141111       // -?Interrogazione clienti in Campagna?
040000150112     d*// TRKC21ds      e ds                  inz
040100150112     d*// wSaveNCM        s                   inz  like(oKC21ncm)
040200150112      *///copy gaitrasrc/srcProtoPR,TRKC21R
040300150112     d*// TRKC41R         pr                  extpgm('TRKC41R')
040400150102     d*//   KPJBA                             likeds(KPJBA)
040500150112     d*//   TRKC21ds                          likeds(TRKC21ds)
040600141125
040700141125       // -?Controllo se cliente in Campagna?
040800150112     d*// TRKC70ds      e ds                  inz
040900150112     d*// TRKC70R         pr                  extpgm('TRKC70R')
041000150102     d*//   kpjba                             likeds(kpjba)
041100150112     d*//   TRKC70ds                          likeds(TRKC70ds)
041200150112     d TRKC73ds      e ds                  inz
041300150206     d   sk_oKC73nc           24     93  0 inz  dim(10)
041400150206     d   sk_oKC73dc           94    493    inz  dim(10)
041500150112     d TRKC73R         pr                  extpgm('TRKC73R')
041600150102     d   kpjba                             likeds(kpjba)
041700150112     d   TRKC73ds                          likeds(TRKC73ds)
041800100408
041900090601      /copy gaitrasrc/srcprotopr,QsnQryModS
042000090508
042100090508     d Qlgsort_pr      pr                  extpgm('QLGSORT')
042200090508     d  pr_QLGSCB                          like(Qlgscb)
042300090508     d  pr_NotUsed1                        like(NotUsed)
042400090508     d  pr_NotUsed2                        like(NotUsed)
042500090508     d  pr_SizeList                        like(SizeList)
042600090508     d  pr_ReturnSize                      like(ReturnSize)
042700090508     d  pr_QUSEC                           like(Qusec)
042800090508     d                                     options(*varsize)
042900090508
043000090508     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
043100090508     d  pr_QLGSCB00                        like(QLGSCB00)
043200090508     d  pr_SflRcd                          like(SflRcd)
043300090508     d  pr_NotUsed1                        like(NotUsed)
043400090508     d  pr_SizeList                        like(SizeList)
043500090508     d  pr_NotUsed2                        like(NotUsed)
043600090508     d  pr_QUSEC                           like(Qusec)
043700090508     d                                     options(*varsize)
043800090508
043900090508     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
044000090508     d  pr_QLGSCB00                        like(QLGSCB00)
044100090508     d  pr_NotUsed1                        like(NotUsed)
044200090508     d  pr_SflRcd                          like(SflRcd)
044300090508     d  pr_SizeList                        like(SizeList)
044400090508     d  pr_NotUsed2                        like(NotUsed)
044500090508     d  pr_QUSEC                           like(Qusec)
044600090508     d                                     options(*varsize)
044700141124
044800080206      //---------------------------------------------------------------
044900080206      //?Definizione key-list.
045000080206      //---------------------------------------------------------------
045100080206
045200080206
045300080206      //---------------------------------------------------------------
045400080206      //?Riepilogo indicatori.
045500080206      //---------------------------------------------------------------
045600090518      // - ordinamenti del subfile
045700090518      // 07    : Ordinamento per data
045800090601      // 08    : Ordinamento per provincia
045900090629      // 09    : Ordinamento per priorità causale
046000090421      // - visualizzazioni particolari
046100090629      // 10    : Emissione subfile per le sole attività da eseguire
046200090518      // 13    : Indica un'attività già eseguita
046300090518      // 15    : Manutenzione di un'attività
046400090518      // 16    : Visualizza un'attività
046500090518      // 17    : Identifica un'attività telefonica
046600090518      // 18    : Identifica un'attività di appuntamento
046700090518      // - Comuni
046800080207      // 28    : Emissione messaggio di errore a video
046900080207      // - C01/S01
047000080207      // 30    : Condiziona SFLDSP    (*not)
047100080207      // 31    : Condiziona SFLDSPCTL (*not)
047200080207      // 30+31 : Condiziona SFLCLR
047300080207      // 32    : Condiziona SFLNXTCHG in S01
047400080207      // 33    : Condiziona SFLEND
047500090518      // 50    : Errore: Posizione cursore
047600080207      // - D01
047700090318      // 50    : Errore: Commerciale CHI sono errato
047800090318      // 51    : Errore: Causale contatto errata
047900090318      // 52    : Errore: Data contatto da effettuare errata
048000090318      // 53    : Errore: Commerciale ...... errat8
048100090318      // 54    : Errore: Cliente potenziale errato
048200090318      // 55    : Errore: Data contatti effettuati.dal... errata
048300090318      // 56    : Errore: Data contatti effettuati..al... errata
048400090318      // 57    : Errore: Filiale appart. .. errata
048500080207      // - Comuni
048600080207      // 99    : Generico di Errore
048700080206      //---------------------------------------------------------------
048800080206
048900080206      //---------------------------------------------------------------
049000080206      //?M A I N - L I N E
049100080206      //---------------------------------------------------------------
049200080206
049300080206     c     *Entry        plist
049400080206     c                   parm                    KPJBA
049500080206
049600080206      /free
049700080206
049800080206       // Operazioni iniziali
049900080206       exsr RoutInz;
050000090601
050100090601       // Operazioni iniziali controllo se video accetta 132 caratteri
050200090601       if not %open(trmk18d);
050300090601           dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
050400090609           if dsp_mod = 1 ;
050500090609              open trmk18d;
050600090609           else ;
050700090609       // Vado a fine in quanto il video non supporta i 132
050800090609              eval $fine = *on ;
050900090609           endif ;
051000090601       endif;
051100080206
051200080206       // Gestione video
051300080206       DOW $Fine = *off;
051400080206         select;
051500080206           when $Video = 'D1';
051600080206             exsr GesD01;
051700080206           when $Video = 'S1';
051800080206             exsr GesS01;
051900090428           other;
052000080206             $Fine = *on;
052100080206         endsl;
052200080206       ENDDO;
052300080206
052400080206       // Operazioni finali
052500080206       exsr RoutEnd;
052600080206
052700080206       //--------------------------------------------------------------
052800080206       //?Operazioni iniziali.
052900080206       //--------------------------------------------------------------
053000080206       BEGSR RoutInz;
053100100427
053200100427         exec sql  set option  dynusrprf = *owner,
053300100427                               closqlcsr = *endmod;
053400080206
053500080206         // Impostazione campi "fissi"
053600080208         T01pgm = SDSpgm;
053700080206
053800080206         // Reperimento dati job
053900080206         exsr DatiJob;
054000080206
054100100209         // Controllo se utente autorizzato alla funzione
054200100209         reset TNTAA1DS;
054300100209         ITAA1caut = '§utegtc';
054400100209         kpjbu     = TNTAA1DS;
054500100209         tntaa1c (kpjba);
054600100209         TNTAA1DS = kpjbu;
054700100209
054800100209         IF  OTAA1fabi = 'N';
054900080206           $ErrGrave   = *on;
055000080206           errMessage  = *on;
055100080206           errGenerico = *on;
055200080206           V1Dmsg = $Msg(01);
055300080206           leavesr;
055400080206         endif;
055500100802
055600100802         wabi = OTAA1cabi;
055700100209
055800100209         clear TRUL31DS;
055900100209         I31abi = OTAA1cabi;
056000100209         I31cdi = DUTdis;
056100100209         I31car = DUTare;
056200100209         I31cpo = DUTpou;
056300100209         callp trul31r (kpjba : trul31ds);
056400100209         IF  O31pog <= *zeros;
056500100209           $ErrGrave   = *on;
056600100209           errMessage  = *on;
056700100209           errGenerico = *on;
056800100209           V1Dmsg = $Msg(01);
056900100209           leavesr;
057000100209         ENDIF;
057100081020
057200090714          // Reperisco data corrente
057300090714             data_iso = (%date());
057400090714             datacor = %dec(data_iso);
057500090714
057600091130          // Recupero i tipi attività che si possono gestire in questo PGM
057700091130         xx=0                     ;
057800091130         setll ('TTA') tntbe01l   ;
057900091130         READE ('TTA') tntbe01l   ;
058000091130
058100091130 2       dow not %eof(tntbe01l)   ;
058200091130 3       if tbeatb=' '    ;
058300091130           dtta=tbeuni    ;
058400091130           If §ttaleg = 'S' ;
058500091130           xx=xx+1        ;
058600091130           $tta(xx)=tbeke1 ;
058700091130           endif ;
058800091130 3       endif ;
058900091130
059000091130         READE ('TTA') tntbe01l   ;
059100091130 2       enddo    ;
059200091130
059300110803          // Verifico se sono utente di sede
059400110803         Sede = (Dutpou = 046);
059500100210         //?Carico sk con commerciali unificanti gestiti dall'utente
059600100210         //?e sk con figli degli unificanti caricati
059700100209         exsr CarAge;
059800111227
059900111227         //?Data del giorno
060000111227         wOggi = %dec(%date());
060100091130
060200080206       ENDSR;
060300080206
060400080206       //--------------------------------------------------------------
060500080206       //?Reperimento Dati del job (Utente/Operativi).
060600080206       //--------------------------------------------------------------
060700080206       BEGSR DatiJob;
060800080206
060900080206         in(E) §AzUte;
061000080206         if NOT %error;
061100080206           in(E) §DatiUte;
061200080206         endif;
061300080206         if %error or RSut = *blanks;
061400080206           clear TIBS34ds;
061500080206           tibs34r(tibs34ds);
061600080206           in §AzUte;
061700080206           in §DatiUte;
061800080206         endif;
061900080206
062000080206       ENDSR;
062100100209
062200100209       //--------------------------------------------------------------
062300100318       //?Carico sk agenti unificanti.
062400100209       //--------------------------------------------------------------
062500100209       BEGSR CarAge;
062600100209
062700100210         //?Carico gli agenti unificanti delle filiali gestite dall'utente
062800100209         $End = *off;
062900100210         clear $AGEU;
063000100209
063100100209         //?Imposto la stringa per SQL
063200130808         wSQL = 'select CMMcod +
063300130808                 from AZCMM00F +
063400130808                 where substr(digits(CMMuni), 1, 3) in (';
063500100209
063600100209         yy = 0;
063700100209         xx = 1;
063800100209         FOR xx by 1 to %elem(pog);
063900100209           IF pog(xx) <> *zeros and pog(xx) <> *blanks;
064000100209             IF yy > 0;
064100100209               wSQL += ', ';
064200100209             ELSE;
064300100209               yy = 1;
064400100209             ENDIF;
064500100209             wSQL += '''';
064600100209             wSQL += pog(xx);
064700100209             wSQL += '''';
064800100209           ENDIF;
064900100209         ENDFOR;
065000100209
065100130808         wSQL += ') and CMMcod = CMMuni +
065200130808                    and CMMpar = '' ''';
065300100209
065400130808         wSQL += ' order by CMMcod +
065500100209                   for fetch only';
065600100209
065700100209         //?Dichiarazione cursore
065800100209         exec sql
065900100210           prepare A1   from :wSQL;
066000100209         exec sql
066100100210           declare AGEU cursor for A1;
066200100209
066300100209         //?Apertura del cursore
066400100209         exec sql
066500100210           open AGEU;
066600100209
066700100209         IF sqlcode < 0;
066800100209           $End = *on;
066900100209         ENDIF;
067000100209
067100100209         yy = 0;
067200100209         DOW not $End;
067300100209           exec sql
067400130808             fetch next from AGEU into :CMMcod;
067500100209           IF sqlcod = 100 or sqlcod < 0;
067600100209             $End = *on;
067700100209             leave;
067800100209           ENDIF;
067900100209
068000130808           yy += 1;
068100130808           $AGEU(yy) = %editc( CMMcod : 'X' );
068200100209
068300100209         ENDDO;
068400100209
068500100209         exec sql
068600100210           close AGEU;
068700100209
068800100209       ENDSR;
068900080206
069000080206       //--------------------------------------------------------------
069100080206       //?Gestione videata D01
069200080206       //--------------------------------------------------------------
069300080206       BEGSR GesD01;
069400080206
069500080206         // Inizializzazione videata
069600080206         if $InzD01 = *on;
069700080206           exsr InzD01;
069800080206           $InzD01 = *off;
069900080214         else;
070000080225           if  errGenerico = *off;
070100090318           // per adesso non prevedo nulla
070200080225           endif;
070300080206         endif;
070400080206
070500080206         // Emissione testata
070600100427         //if  errGenerico = *off;
070700090317           write MK18T01;
070800100427         //endif;
070900080206
071000080206         // Emissione videata
071100090318         exfmt MK18D01;
071200080206         errMessage   = *off;
071300080206         errGenerico  = *off;
071400080206         clear V1Dmsg;
071500080207
071600080206         select;
071700080206         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
071800080206           when  $ErrGrave = *on;
071900080206             $Fine  = *on;
072000080206         // F3=Fine
072100080208           when dsp_aid = c_F03;
072200080206             exsr F03D01;
072300141111
072400090317         // F07=Prima telefonata Potenziale
072500090317           when dsp_aid = c_F07;
072600090317             exsr F07D01;
072700081021
072800091019         // F04=Prima telefonata Cliente
072900091019           when dsp_aid = c_F04;
073000091019             exsr F04D01;
073100100430
073200100430         //?F11 = Agenda
073300100430           when dsp_aid = c_F11;
073400100430             exsr F11D01;
073500091019
073600080206         // Enter
073700080206           other;
073800080206             exsr CtrD01;
073900080206             if errGenerico;
074000080206               leavesr;
074100080206             endif;
074200090428             $Video = 'S1';
074300080206             $InzS01 = *on;
074400100319             clear sav_C01loc;
074500100617             clear sav_C01cap;
074600100319             clear sav_C01cca;
074700100617             clear sav_C01cmm;
074800100503
074900110218         //?Imposto ordinamento per causale
075000110218            // ORD_Xdata = *on ;
075100110218             ORD_Xcausale = *on ;
075200080206         endsl;
075300080206
075400080206       ENDSR;
075500080206
075600080206       //--------------------------------------------------------------
075700080206       //?Inizializzazione videata D01
075800080206       //--------------------------------------------------------------
075900080206       BEGSR InzD01;
076000080206
076100090506           v01asc = *date ;
076200080206
076300080206       ENDSR;
076400080206
076500080206       //--------------------------------------------------------------
076600080206       //?Gestione tasto funzionale F3 da videata D01
076700080206       //?F3=Fine
076800080206       //--------------------------------------------------------------
076900080206       BEGSR F03D01;
077000080206
077100080207         // Chiusura del programma
077200080206         $Fine = *on;
077300080206
077400080206
077500080206       ENDSR;
077600080206
077700080206       //--------------------------------------------------------------
077800090609       //?Gestione tasto funzionale F07 da videata D01  richiamo il pgm
077900090609       //?che gestisce i contatti
078000090317       //?F07=Prima telefonata potenziale
078100080206       //--------------------------------------------------------------
078200090317       BEGSR F07D01;
078300100408
078400100408         // Richiamo il pgm interrogazione potenziali
078500090609             clear trmk19ds;
078600090609             imk19cmt = 'S';
078700090609             imk19fle = 'P' ;
078800090610             imk19tco = 'T' ;
078900090611                     if v01cmg <> *blanks ;
079000090611                        imk19com = %dec(v01cmg :7:0);
079100090611                        imk19comd = v01cmgd ;
079200090611                     endif ;
079300100408
079400100408             clear trmk01ds;
079500100408             mk01k11='1'   ;
079600100408             mk01imm='S'   ;
079700100408             mk01K10='B'   ;
079800100408             kpjbu=trmk01ds;
079900120201
080000120201             IF sede;
080100120201               clear kpjbu;
080200120201               Trmk01r ( kpjba );
080300120201             ELSE;
080400100408             Trmk01r ( kpjba : trmk19ds );
080500120201             ENDIF;
080600120201
080700100408             trmk01ds=kpjbu  ;
080800100408
080900100408         // Se restituito errore => lo si visualizza
081000100408         if  mk01err = 'E';
081100100408           errMessage  = *on;
081200100408           errGenerico = *on;
081300100408           V1Dmsg = mk01m10  ;
081400100408           V01cpo = %editc(mk01cpo : 'X');
081500100408           leavesr;
081600100408         endif;
081700100408
081800090407
081900080206       ENDSR;
082000080206
082100091019       //--------------------------------------------------------------
082200091019       //?Gestione tasto funzionale F04 da videata D01  richiamo il pgm
082300091019       //?che gestisce i contatti
082400141201       //?F04=Interrogazione clienti.
082500091019       //--------------------------------------------------------------
082600091019       BEGSR F04D01;
082700100802
082800100802       // Richiamo il pgm interrogazione clienti
082900150112         clear  TRKC20ds;
083000150112         iKC20ric = 'A';
083100150112         iKC20abi = wABI;
083200141201
083300141201         clear  kpjbu;
083400141201
083500150112         trkc20r ( kpjba : TRKC20ds );
083600091019
083700091019       ENDSR;
083800100430
083900100430       //--------------------------------------------------------------
084000100430       //?Gestione tasto funzionale F11 da videata D01
084100100430       //?F11=Agenda
084200100430       //--------------------------------------------------------------
084300100430       BEGSR F11D01;
084400100430
084500100430         clear TRMK82DS;
084600100430         IMK82rich = 'A';
084700100430
084800100524         SELECT;
084900100524         WHEN  V01ccm <> *blanks and V01ccm <> *zeros and
085000100524               %check(digits:V01ccm) = 0;
085100100430           IMK82cage = %dec(V01ccm:7:0);
085200100524         WHEN  V01cmg <> *blanks and V01cmg <> *zeros and
085300100430             %check(digits:V01cmg) = 0;
085400100430           IMK82cage = %dec(V01cmg:7:0);
085500100524         ENDSL;
085600100430
085700100430         kpjbu = TRMK82DS;
085800100430
085900100430         trmk82r (kpjba);
086000100430
086100100430       ENDSR;
086200091019
086300080206       //--------------------------------------------------------------
086400080206       //?Controllo videata D01
086500080206       //--------------------------------------------------------------
086600080206       BEGSR CtrD01;
086700081017
086800100617         %subst(IndDspF : 50) = *off;
086900100524
087000100524         clear $AGE;
087100100524         clear wage;
087200100309
087300100309         // Controllo codice commerciale (chi sono ) "?"
087400100316           clear v01cmgd;
087500100309           If    %scan('?' : v01cmg) > 0  ;
087600100309             errGenerico = *on;
087700100309             PosCurCmg   = *on;
087800130808             clear  TRMK43ds;
087900130808             iMK43fil  = DUTpou;
088000130808             TRMK43R (kpjba : TRMK43ds);
088100130808             if  oMK43err = *off  and  oMK43fxx = *blank;
088200130808               v01cmg  = %editc( oMK43cmm : 'X' );
088300130808               v01cmgd = oMK43des;
088400130808             endif;
088500100309             leavesr;
088600100309           endif;
088700100309
088800100309         // Controllo codice commerciale (chi sono )
088900100309           If    v01cmg <> *blanks and v01cmg <> *zeros;
089000100309
089100100309             IF  %check(digits:V01cmg) > 0;
089200100309               ErrMessage  = *on;
089300100309               ErrGenerico = *on;
089400100309               PosCurCMG   = *on;
089500100309               V1Dmsg      = $Msg(03);
089600100309               leavesr;
089700100309             ENDIF;
089800100309
089900100309           //?utente abilitato al commerciale
090000100309             clear TNTAA1DS;
090100100309             ITAA1caut = '§utegtc';
090200100309             ITAA1cmm  = %dec(V01cmg:7:0);
090300100309             kpjbu     = tntaa1ds;
090400100309             tntaa1c (kpjba);
090500100309             TNTAA1DS = kpjbu;
090600100309             IF  OTAA1fabi = 'N';
090700100309               ErrMessage  = *on;
090800100309               ErrGenerico = *on;
090900100309               PosCurCMG   = *on;
091000100309               V1Dmsg      = $Msg(03);
091100100309               leavesr;
091200100309             ENDIF;
091300100309
091400130808             CMMcod = %int(V01cmg);
091500130808             chain  (CMMcod)  AZCMM000;
091600130808             if  NOT  %found(AZCMM01L);
091700100309               errMessage  = *on;
091800100309               errGenerico = *on;
091900100309               PosCurCmg   = *on;
092000100309               V1Dmsg = $Msg(03);
092100100309               leavesr;
092200100309             endif;
092300100309
092400130808             V01cmgd = CMMdes;
092500110614
092600110614           //?Commerciale senza particolarità
092700130808             IF  CMMpar <> *blanks;
092800110614               ErrMessage  = *on;
092900110614               ErrGenerico = *on;
093000110614               PosCurCMG   = *on;
093100110614               V1Dmsg      = $Msg(03);
093200110614               leavesr;
093300110614             ENDIF;
093400111228
093500111228       //?Commerciale "valido" data inizio e fine attività
093600130808         IF  CMMdtIni > wOggi  or  CMMdtFin < wOggi;
093700111228           ErrMessage = *on;
093800111228           ErrGenerico = *on;
093900111228           PosCurCmg   = *on;
094000111228           V1Dmsg = $Msg(03);
094100111228           leavesr;
094200111228         ENDIF;
094300111228
094400100309           endif;
094500100309
094600081017         // Controllo selezioni inserite
094700090317         // se inserite le date Da effettuare al e le date Effettuate dal al
094800090317         // do errore
094900100524
095000100524         // Controllo commerciale che esegue contatto
095100100524         clear v01ccmd ;
095200100524         If    %scan('?' : v01Ccm) > 0  ;
095300100524           errGenerico = *on;
095400100524           PosCurCcm   = *on;
095500130808           clear  TRMK43ds;
095600130808           iMK43fil  = DUTpou;
095700130808           TRMK43R (kpjba : TRMK43ds);
095800130808           if  oMK43err = *off  and  oMK43fxx = *blank;
095900130808             v01ccm  = %editc( oMK43cmm : 'X' );
096000130808             v01ccmd = oMK43des;
096100130808           endif;
096200100524           leavesr;
096300100524         EndIf;
096400100524
096500100524         IF  V01ccm <> *blanks and v01ccm <> *zeros ;
096600100524
096700100524           if  %check(digits:V01ccm) > 0;
096800100524             ErrMessage  = *on;
096900100524             ErrGenerico = *on;
097000100524             PosCurCCM   = *on;
097100100524             V1Dmsg      = $Msg(03);
097200100524             leavesr;
097300100524           endif;
097400100524
097500100524           //?utente abilitato al commerciale
097600100524           clear TNTAA1DS;
097700100524           ITAA1caut = '§utegtc';
097800100524           ITAA1cmm  = %dec(V01ccm:7:0);
097900100524           kpjbu     = tntaa1ds;
098000100524           tntaa1c (kpjba);
098100100524           TNTAA1DS = kpjbu;
098200100524           if  OTAA1fabi = 'N';
098300100524             ErrMessage  = *on;
098400100524             ErrGenerico = *on;
098500100524             PosCurCCM   = *on;
098600100524             V1Dmsg      = $Msg(03);
098700100524             leavesr;
098800100524           endif;
098900100524
099000130808           CMMcod = %int(V01ccm);
099100130808           chain  (CMMcod)  AZCMM000;
099200130808           if  NOT  %found(AZCMM01L);
099300100524             errMessage  = *on;
099400100524             errGenerico = *on;
099500100524             PosCurCcm   = *on;
099600100524             V1Dmsg = $Msg(03);
099700100524             leavesr;
099800100524           endif;
099900100524
100000130808           V01ccmd = CMMdes;
100100110614
100200110614           //?Commerciale senza particolarità
100300130808           IF  CMMpar <> *blanks;
100400110614             ErrMessage  = *on;
100500110614             ErrGenerico = *on;
100600110614             PosCurCCM   = *on;
100700110614             V1Dmsg      = $Msg(03);
100800110614             leavesr;
100900110614           ENDIF;
101000100524
101100100524           //?commerciale è mio come Unificante
101200100524           $Uni = *off;
101300100524           if  %lookup(V01ccm:$AGEU) > 0 and
101400100524               %lookup(%subst(V01ccm:1:3):pog) > 0;
101500100524             $Uni = *on;
101600100524             //?Carico sk con tutti gli agenti abilitati
101700100524             wage = V01ccm;
101800100524             exsr CarAgeA;
101900100524           endif;
102000100524           //?commerciale è mio ma non Unificante
102100100524           if not $Uni;
102200100524             //?Carico sk con tutti i fratelli
102300130808             wage = %editc(CMMuni:'X');
102400100524             exsr CarAgeA;
102500100524           endif;
102600100524
102700100524         ENDIF;
102800100524
102900100524
103000100524         //?Almeno uno tra commerciale, cliente, potenziale e filiale
103100100524         //?deve essere inserito
103200100524           IF  (V01ccm = *blanks or V01ccm = *zeros) and
103300100524               (V01cp1 = *blanks or V01cp1 = *zeros) and
103400100524                V01cp2 = *zeros and
103500100524               (V01ksc = *blanks or V01ksc = *zeros) and
103600141125                V01flt = *zeros;
103700141125               //-(V01ncm = *blanks or V01ncm = *zeros);    ?NO: solo NCM appesantisce troppo!?
103800100524             ErrMessage  = *on;
103900100524             ErrGenerico = *on;
104000100524             PosCurCCM   = *on;
104100100524             V1Dmsg      = $Msg(14);
104200100524             leavesr;
104300100524           ENDIF;
104400100524
104500100524
104600100524
104700100524         // controllo data limite selezione contatti da fare
104800100524         clear w01asc;
104900100524         If    V01asc <> 0;
105000100524           clear wlbdat;
105100100524           g02dat = v01asc;
105200100524           xsrda8(wlbdat);
105300100524           if g02err = '1';
105400100524             errMessage  = *on;
105500100524             errGenerico = *on;
105600100524             PosCurAsc   = *on;
105700100524             V1Dmsg = $Msg(04);
105800100524             leavesr;
105900100524           endif;
106000100524
106100100524           v01asc = g02dat;
106200100524           w01asc = g02inv;
106300100524         endif;
106400100524
106500100524
106600100524         //?Se NON inserite le date da efettuare ed effettuate
106700100524         //?errore
106800100524           IF  V01asc = *zero and V01eda = *zeros and V01eal = *zeros;
106900100524             ErrMessage  = *on;
107000100524             ErrGenerico = *on;
107100100524             PosCurAsc   = *on;
107200100524             V1Dmsg      = $Msg(02);
107300100524             leavesr;
107400100524           ENDIF;
107500100524
107600100524           If      V01ASC <> *zero  and (V01EDA <> 0 or V01EAL <> 0);
107700100524             errMessage  = *on;
107800100524             errGenerico = *on;
107900100524             PosCurAsc   = *on;
108000100524             V1Dmsg      = $Msg(02);
108100100524             leavesr;
108200100524           Endif ;
108300100524
108400100524
108500100524         // - controllo / ricerca Causale contatto
108600100524         If    %scan('?' : V01cac) > *zero;
108700100524           errGenerico = *on;
108800100524           PosCurCac   = *on;
108900100524           clear  dCCO;
109000100524           clear  Tntb74ds;
109100100628           //iccouti = 'S' ;
109200100628           iccouti = 'X' ;
109300100524           TNTB74R  (kpjba : tntb74ds);
109400121128           if  occoerr = *off   ;
109500100524             dcco = occouni ;
109600100524             V01cac = Occoke1;
109700100524             V01cacd = §ccodes ;
109800100524             errGenerico = *on;
109900100524             leavesr;
110000100524           else;
110100100524             errMessage  = *on;
110200100524             errGenerico = *on;
110300100524             V1Dmsg = T02msg;
110400100524             leavesr;
110500100524           endif;
110600100524         endif;
110700100524
110800100524         If    V01cac <> *blanks ;
110900100524           clear  dCCO;
111000100524           clear  TIBS02ds;
111100100524           T02mod = 'C';
111200100524           T02cod = 'CCO';
111300100524           T02ke1 = v01cac ;
111400100524           T02sif = KNSIF;
111500100524           TNTBE_RicercaControllo  (kpjba : tibs02ds);
111600100524           if  T02err = *blanks;
111700100524             dcco = t02uni ;
111800100524             V01cacd = §ccodes ;
111900100524           else;
112000100524             errMessage  = *on;
112100100524             errGenerico = *on;
112200100524             PosCurCac   = *on;
112300100524             V1Dmsg = T02msg;
112400100524             leavesr;
112500100524           endif;
112600100524         else ;
112700100524           clear v01cacd ;
112800100524         endif;
112900100524
113000100524
113100100524         // Controllo codice potenziale
113200100524         clear cpo_scelto;
113300100524         If    %scan('?' : v01Cp1) > 0  ;
113400100524           errGenerico = *on;
113500100524           PosCurCp1   = *on;
113600100524           clear trmk01ds;
113700100524           mk01k11='2'   ;
113800100524           kpjbu=trmk01ds;
113900100524           TRMK01R (kpjba) ;
114000100524           trmk01ds=kpjbu  ;
114100100524           if  mk01cpo>0;
114200100524             V01cpo = %editc(mk01cpo : 'X');
114300100524             chain     mk01cpo  TNCPO01l;
114400100524             if %found(tncpo01l) ;
114500100524             //  eval v01cpod = CpoRag ;
114600100524             endif ;
114700100524           else ;
114800100524             clear v01cp1;
114900100524           endif ;
115000100524         EndIf;
115100100524
115200100524         if  V01cp2 <> *zeros and V01cp1 = *blanks;
115300100524           V01cp1 = '000';
115400100524         endif;
115500100524
115600100524         //If    V01cp1 <>  *zeros and v01cp2 <> *zeros ;
115700100524         If    V01cp1 <>  *blanks and v01cp2 <> *zeros ;
115800100524
115900100524           CPOcpo = %int(V01cpo);
116000100524           chain     CPOcpo  TNCPO000;
116100100524
116200100524           // Codice potenziale NON trovato
116300100524           if  NOT  %found(TNCPO01L);
116400100524               errGenerico = *on;
116500100524               PosCurCp1   = *on;
116600100524               errMessage  = *on;
116700100524               V1Dmsg = $Msg(08);
116800100524             leavesr;
116900100524           endif;
117000100524
117100100524           cpo_scelto = %int(v01cpo) ;
117200100524
117300100524         endif;
117400100524
117500100524
117600100524         //?Se immesso potenziale e cliente
117700100524         //?errore di incongruenza dati
117800100524           IF  V01cp1 <> *zeros and v01cp1 <> *blanks and
117900100524               V01cp2 <> *zeros and
118000100524               V01ksc <> *zeros and V01ksc <> *blanks;
118100100524             ErrMessage  = *on;
118200100524             ErrGenerico = *on;
118300100524             //PosCurCP1   = *on;
118400100524             PosCurKSC   = *on;
118500100524             V1Dmsg      = $Msg(16);
118600100524             leavesr;
118700100524           ENDIF;
118800100524
118900100524
119000100524         //?Controllo cliente
119100100524         if  V01ksc = *zeros;
119200100524           clear V01ksc;
119300100524         endif;
119400100524         clear Ksc_scelto;
119500100524         clear V01dksc;
119600100524         IF  %scan('?' : V01ksc) > *zero;
119700100524           ErrGenerico = *on;
119800100524           PosCurKsc   = *on;
119900100802           //?Ricerca clienti
120000141201           clear  V01ksc;
120100150112           clear  TRKC20ds;
120200150112           //iKC20ric = *blank;
120300150112           iKC20abi = wABI;
120400141201           clear  kpjbu;
120500150112           trkc20r ( kpjba : TRKC20ds );
120600150112           if  oKC20fxx = *blank  and  oKC20err = *blank;
120700150112             V01ksc = %editc( KC20ksc : 'X' );
120800141201           endif;
120900141201         ENDIF;
121000100524
121100100524         //?Codice valido
121200100524         IF  V01ksc <> *zeros and V01ksc <> *blanks;
121300100524           //?utente abilitato al cliente
121400100524           clear TNTAA1DS;
121500100524           ITAA1caut = '§utegtc';
121600100524           ITAA1ksc  = %dec(V01ksc:7:0);
121700100524           kpjbu     = tntaa1ds;
121800100524           tntaa1c (kpjba);
121900100524           TNTAA1DS = kpjbu;
122000100524           if  OTAA1fabi = 'N';
122100100524             ErrMessage  = *on;
122200100524             ErrGenerico = *on;
122300100524             PosCurKsc   = *on;
122400100524             V1Dmsg      = $Msg(12);
122500100524             leavesr;
122600100524           endif;
122700100524
122800100524           clear tibs69ds;
122900100524           clear ACOrag;
123000100524           I69kac = %dec(V01ksc:7:0);
123100100524           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
123200100524           if  O69err <> *blanks;
123300100524             ErrMessage  = *on;
123400100524             ErrGenerico = *on;
123500100524             PosCurKsc   = *on;
123600100524             V1Dmsg      = $Msg(12);
123700100524           endif;
123800100524           V01dksc = ACOrag;
123900100524           if  ErrGenerico;
124000100524             leavesr;
124100100524           endif;
124200100524           Ksc_scelto = I69kac;
124300100524         ENDIF;
124400100524
124500090319
124600090319         // controllo data limite dal selezione contatti fatti
124700100524         clear w01eda;
124800100524         If    V01eda <> 0;
124900100524           clear wlbdat;
125000100524           g02dat = v01eda;
125100100524           xsrda8(wlbdat);
125200100524           if g02err = '1';
125300100524             errMessage  = *on;
125400100524             errGenerico = *on;
125500100524             PosCurEda   = *on;
125600100524             V1Dmsg = $Msg(04);
125700100524             leavesr;
125800100524           endif;
125900100524
126000100524           v01eda = g02dat;
126100100524           w01eda = g02inv;
126200090406
126300090406         endif;
126400090319
126500100524         // se data limite al è uguale a zero imposto la data del giorno
126600100524         If   v01eda <> 0 and  V01eal = 0 ;
126700100524           v01eal = *date;
126800100524         endif ;
126900090319
127000090319         // controllo data limite al selezione contatti fatti
127100100524         clear w01eal;
127200100524         If    V01eal <> 0;
127300100524           clear wlbdat;
127400100524           g02dat = v01eal;
127500100524           xsrda8(wlbdat);
127600100524           if g02err = '1';
127700100524             errMessage  = *on;
127800100524             errGenerico = *on;
127900100524             PosCurEal   = *on;
128000100524             V1Dmsg = $Msg(04);
128100100524             leavesr;
128200100524           endif;
128300100524
128400100524           v01eal = g02dat;
128500100524           w01eal = g02inv;
128600100524         endif;
128700090319
128800090319         // Controllo se data congruenti
128900100524         if  w01eda > w01eal;
129000100524           errMessage  = *on;
129100100524           errGenerico = *on;
129200100524           PosCurEal   = *on;
129300100524           V1Dmsg = $Msg(05);
129400100524           leavesr;
129500090319         endif;
129600090319
129700090319         // Controllo Filiale gestione telefonate
129800100524         IF  v01flt > 0 ;
129900100524           chain v01flt azorg01l;
130000100524           If not %found(azorg01l);
130100100524             errMessage  = *on;
130200100524             errGenerico = *on;
130300100524             PosCurFlt   = *on;
130400100524             V1Dmsg = $Msg(06);
130500100524             leavesr;
130600100524           endif;
130700100524           // verifico se filiale abilitata all'utente
130800100209           clear TNTAA1DS;
130900100209           ITAA1caut = '§utegtc';
131000100209           ITAA1fil  = V01flt;
131100100209           kpjbu     = tntaa1ds;
131200100209           tntaa1c (kpjba);
131300100209           TNTAA1DS = kpjbu;
131400100524           if  OTAA1fabi = 'N';
131500100524             errMessage  = *on;
131600100524             errGenerico = *on;
131700100524             PosCurFlt   = *on;
131800100524             V1Dmsg = $Msg(06);
131900100524             leavesr;
132000100524           endif;
132100100524         ENDIF;
132200141124
132300141125         // -?Ricerca/Controllo Campagna Commerciale?
132400141125         clear  V01ncmD;
132500141125
132600141125         // ·?Ricerca?
132700141125         If  %scan( '?' : V01ncm ) > *zero;
132800141125           clear  V01ncm;
132900150112           clear  TRKC02ds;
133000150112           iKC02ric = 'R';
133100150112           trkc02r ( kpjba : TRKC02ds );
133200150112           if  oKC02ncm <> *zero;
133300150112             V01ncm = %editc( oKC02ncm : '3' );
133400141125           endif;
133500141125           PosCurNCM   = *on;
133600141125           errGenerico = *on;
133700150112           if  oKC02err = *on;
133800141125             ErrMessage = *on;
133900150112             V1Dmsg = oKC02msg;
134000141125             leavesr;
134100141125           endif;
134200141125         EndIf;
134300141125
134400141125         // ·?Controllo?
134500141125         if  V01ncm = *blank;
134600141125           V01ncm = *zero;
134700141125         endif;
134800141125
134900141125         if  %check( Digits : %trim( V01ncm ) ) > *zero;
135000141125           errGenerico = *on;
135100141125           errMessage  = *on;
135200141125           PosCurNCM   = *on;
135300141125           V1Dmsg = $Msg(13);
135400141125           leavesr;
135500141125         endif;
135600141124
135700141125         if  V01ncm <> *blank  and  %int( %trim( V01ncm ) ) <> *zero;
135800141125           chain  ( %int( %trim( V01ncm ) ) )  TICMP000;
135900141125           if  Not %found(TICMP01L);
136000141125             errMessage  = *on;
136100141125             errGenerico = *on;
136200141125             PosCurNCM   = *on;
136300141125             V1Dmsg = $Msg(13);
136400141125             leavesr;
136500141125           endif;
136600141125           V01ncmD = CMPdes;
136700141125         endif;
136800141125
136900141125         if  V01ncm <> *blank;
137000141125           evalR V01ncm = %editc( %int( %trim( V01ncm ) ) : 'Z' );
137100141125         endif;
137200150219
137300150220         if  V01ncm <> *blank  and  %int( %trim( V01ncm ) ) <> *zero  and
137400150220             ( wOggi < CMPdic  or   wOggi > CMPdfc );
137500150219           errMessage  = *on;
137600150219           errGenerico = *on;
137700150219           PosCurNCM   = *on;
137800150219           V1Dmsg = $Msg(19);
137900150219           leavesr;
138000150219         endif;
138100080213
138200080206       ENDSR;
138300100318
138400100318       //--------------------------------------------------------------
138500100318       //?Carico sk agenti abilitati.
138600100318       //--------------------------------------------------------------
138700100318       BEGSR CarAgeA;
138800100318
138900100318         $End = *off;
139000100318       //?Carico per mio unificante
139100100318         IF  $Uni;
139200100318         //?Imposto la stringa per SQL
139300130808           wSQL = 'select CMMcod +
139400130808                   from AZCMM00F +
139500130808                   where CMMuni = ';
139600100318           wSQL += wage;
139700130808           wSQL += ' order by CMMcod for fetch only';
139800100318         ENDIF;
139900100318       //?Carico fratelli per mio commerciale
140000100318         IF  not $Uni;
140100100318         //?Imposto la stringa per SQL
140200130808           wSQL = 'select CMMcod +
140300130808                   from AZCMM00F +
140400130808                   where substr(digits(CMMcod), 1, 3) in (';
140500100318
140600100318           yy = 0;
140700100318           xx = 1;
140800100318           FOR xx by 1 to %elem(pog);
140900100318             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
141000100318               IF yy > 0;
141100100318                 wSQL += ', ';
141200100318               ELSE;
141300100318                 yy = 1;
141400100318               ENDIF;
141500100318               wSQL += '''';
141600100318               wSQL += pog(xx);
141700100318               wSQL += '''';
141800100318             ENDIF;
141900100318           ENDFOR;
142000100318
142100130808           wSQL += ') and CMMuni = ' + wage +
142200130808                    ' and CMMpar = '' ''';
142300100318
142400130808           wSQL += ' order by CMMcod +
142500100318                     for fetch only';
142600100318         ENDIF;
142700100318
142800100318       //?Dichiarazione cursore
142900100318         exec sql
143000100318           prepare A2   from :wSQL;
143100100318         exec sql
143200100318           declare AGE cursor for A2;
143300100318       //?Apertura del cursore
143400100318         exec sql
143500100318           open AGE;
143600100318         IF sqlcode < 0;
143700100318           $End = *on;
143800100318         ENDIF;
143900100318
144000100318         xx = 0;
144100100318         DOW not $End;
144200100318           exec sql
144300130808             fetch next from AGE into :CMMcod;
144400100318           IF sqlcod = 100 or sqlcod < 0;
144500100318             $End = *on;
144600100318             leave;
144700100318           ENDIF;
144800100318
144900130808           xx += 1;
145000130808           $AGE(xx) = %editc( CMMcod : 'X' );
145100100318
145200100318         ENDDO;
145300100318
145400100318         exec sql
145500100318           close AGE;
145600100318
145700100318       ENDSR;
145800080206
145900090421       //--------------------------------------------------------------
146000080206       //?Gestione videata S01
146100080206       //--------------------------------------------------------------
146200080206       BEGSR GesS01;
146300090629
146400080207         // Inizializzazione videata
146500080207         if  $InzS01 = *on;
146600090629         // imposto l'ordinamento per priorità causale
146700100503          //   ORD_Xcausale = *on ;
146800080207            exsr InzS01;
146900080207            $InzS01  = *off;
147000080207         endif;
147100080207
147200080207         // Posizionamento cursore
147300080207         if  C01csr  > *zero;
147400080207           C01rcd = C01csr;
147500080207         else;
147600080207         // Se non è stato caricato nulla si riemette la videata D01
147700080207           C01rcd = 1;
147800080207           $Video = 'D1';
147900080207           errMessage  = *on;
148000080207           errGenerico = *on;
148100090319           PosCurCmg   = *on;
148200090319           V1Dmsg = $Msg(07);
148300080207           leavesr;
148400080207         endif;
148500080207
148600080207         // Emissione Testata e Piede con tasti funzionali abilitati
148700090319           write  MK18T01;
148800090319           write  MK18P01;
148900090429
149000090429        // emetto la scritta già eseguite in basae alle richieste
149100090429         if $sel = 'E' ;
149200090429            da_eseguire = *off;
149300090429         else ;
149400090429            da_eseguire = *on ;
149500090429         endif;
149600080207
149700080207         // Emissione videata
149800090319         exfmt  MK18C01;
149900080207         reset errMessage;
150000080207         reset errGenerico;
150100080207         clear V1Dmsg;
150200080207
150300080207         SELECT;
150400080207
150500080207         // - F3=Fine
150600080207           WHEN  dsp_aid = c_F03;
150700080207             exsr F03D01;
150800080207
150900080207         // - F12=Ritorno
151000080207           WHEN  dsp_aid = c_F12;
151100080207             exsr F12S01;
151200090319
151300090505         // - F07=ordinamento per data
151400090629           WHEN  dsp_aid = c_F17;
151500090625             exsr sr_orddata ;
151600080626
151700090609         // - F08=ordinamento per provincia
151800090629           WHEN  dsp_aid = c_F18;
151900090625             exsr sr_ordprov ;
152000090625
152100090910         // - F19=ordinamento per priorità causale
152200090629           WHEN  dsp_aid = c_F19;
152300090625             exsr sr_ordcaus ;
152400090319
152500080207         // - Roll-Up
152600080207           WHEN  dsp_aid = c_RollUp;
152700080207
152800080207         // Invio
152900080207           OTHER;
153000090928
153100090527         // verifico se esistono campi di parzializzazione
153200090527             exsr CtrC01 ;
153300090527             if  errGenerico = *on;
153400090527               leavesr;
153500090527             endif;
153600080207             exsr OpzS01;
153700080207             if  errGenerico = *on;
153800080207               leavesr;
153900080207             endif;
154000080207
154100080207         ENDSL;
154200080207
154300080207       ENDSR;
154400080207
154500080207       //--------------------------------------------------------------
154600080207       //?Inizializzazione videata S01
154700080207       //--------------------------------------------------------------
154800080207       BEGSR InzS01;
154900080207
155000090508       // Pulizia subfile
155100090508         exsr pulsfl1;
155200100319
155300100319       //?Parzializzazioni
155400100319         c01cca = sav_c01cca;
155500100319         c01loc = sav_c01loc;
155600100617         c01cap = sav_c01cap;
155700100617         c01cmm = sav_c01cmm;
155800100319         IF  C01loc = *blanks;
155900100319           $parz_X_loc = *off ;
156000100319         ENDIF;
156100100617         IF  C01cap = *blanks;
156200100617           $parz_X_cap = *off ;
156300100617         ENDIF;
156400100319         IF  C01cca = *blanks;
156500100319           $parz_X_cca = *off ;
156600100319         ENDIF;
156700100617         IF  C01cmm = *zeros;
156800100617           $parz_X_cmm = *off ;
156900100319         ENDIF;
157000090508
157100080207       // Caricamento dei dati da presentare nel sfl
157200080207         $EoF = *off;
157300080213         select;
157400100317
157500100317         // - Selezionati i contatti da eseguire
157600090427           when  v01asc > 0 ;
157700090320         //  Preparo la stringa sql
157800090515                 StringaSql = 'select * from tiatc00f where atcdco = 0 +
157900090515                 and atcdad <= ' +  %editc(w01asc:'X') ;
158000090323                 $Sel = ' ' ;
158100090323         // se richiesta la causale contatto seleziono per causale da contattare
158200090427           If v01cac <> *blank;
158300090924                 StringaSql = %trim(Stringasql) + ' and atccad = ' +
158400090924                 '''' + v01cac +'''';
158500090323             endif;
158600090320
158700100317         // - Selezionati i contatti eseguiti
158800090427           when  v01eal > 0 ;
158900090320         //  Preparo la stringa sql
159000090515                 StringaSql = 'select * from tiatc00f where atcdco >= ' +
159100090427                  %editc(w01eda:'X') +
159200090722                 ' and atcdco <= ' + %editc(w01eal:'X') + ' and atcdco > 0' ;
159300090323                 $Sel = 'E' ;
159400090323         // se richiesta la causale contatto seleziono per causale contatto
159500090427           If v01cac <> *blank;
159600100316                 StringaSql = %trim(Stringasql) + ' and atccac = ' +
159700100316                 '''' + v01cac + '''';
159800090323             endif;
159900090323           endsl;
160000100916
160100100916       //?se NON è stata richiesta una causale specifica elaboro SOLO
160200100916       //?i tipi attività che hanno un legame con i clienti
160300100916         IF  V01cac = *blanks;
160400100916           zz = 0;
160500100916           ww = 1;
160600100916           StringaSql = %trim(StringaSql) + ' and ATCtat in(';
160700100916           FOR ww by 1 to %elem($TTA);
160800100916             IF $TTA(ww) <> *blanks;
160900100916               IF zz > 0;
161000100916                 StringaSql += ', ';
161100100916               ELSE;
161200100916                 zz = 1;
161300100916               ENDIF;
161400100916               StringaSql += '''';
161500100916               StringaSql += $TTA(ww);
161600100916               StringaSql += '''';
161700100916             ENDIF;
161800100916           ENDFOR;
161900100916           StringaSql += ')';
162000100916         ENDIF;
162100100916
162200100916       //?E' stato richiesto un commerciale
162300100916         IF  V01ccm > *zeros;
162400100917           zz = 0;
162500100917           ww = 1;
162600100916         //?sto elaborando le attività da eseguire
162700100916         //?controllo con commerciale che deve eseguire l'attività
162800100916           IF  $Sel = *blanks;
162900100916             StringaSql = %trim(StringaSql) + ' and ATCcmm in(';
163000100916             FOR ww by 1 to %elem($AGE);
163100100916               IF $AGE(ww) <> *blanks;
163200100916                 IF zz > 0;
163300100916                   StringaSql += ', ';
163400100916                 ELSE;
163500100916                   zz = 1;
163600100916                 ENDIF;
163700100916                 //StringaSql += '''';
163800100916                 StringaSql += $AGE(ww);
163900100916                 //StringaSql += '''';
164000100916               ENDIF;
164100100916             ENDFOR;
164200100916             StringaSql += ')';
164300100916           ENDIF;
164400100916       //?sto elaborando le attività eseguite
164500100916       //?controllo con commerciale che ha eseguito l'attività
164600100916           IF  $Sel = 'E';
164700100916             StringaSql = %trim(StringaSql) + ' and ATCcco in(';
164800100916             FOR ww by 1 to %elem($AGE);
164900100916               IF $AGE(ww) <> *blanks;
165000100916                 IF zz > 0;
165100100916                   StringaSql += ', ';
165200100916                 ELSE;
165300100916                   zz = 1;
165400100916                 ENDIF;
165500100916                 //StringaSql += '''';
165600100916                 StringaSql += $AGE(ww);
165700100916                 //StringaSql += '''';
165800100916               ENDIF;
165900100916             ENDFOR;
166000100916             StringaSql += ')';
166100100916           ENDIF;
166200100916         ENDIF;
166300100917
166400100917       //?E' stata richiesta la filiale
166500100917         IF  V01flt <> 000;
166600100917         //?escludo le attività che non le appartengono
166700100917         //?il controllo è fatto con la filiale commerciale
166800100917         //?sto elaborando le attività da eseguire
166900100917         //?controllo con commerciale che deve eseguire l'attività
167000100917           IF  $Sel = *blanks;
167100100917             StringaSql = %trim(StringaSql) + ' and  ' +
167200100917            'substr(digits(ATCcmm), 1, 3) = ' + %editc(V01flt:'X');
167300100917           ENDIF;
167400100917       //?sto elaborando le attività eseguite
167500100917       //?controllo con commerciale che ha eseguito l'attività
167600100917           IF  $Sel = 'E';
167700100917             StringaSql = %trim(StringaSql) + ' and  ' +
167800100917            'substr(digits(ATCcco), 1, 3) = ' + %editc(V01flt:'X');
167900100917           ENDIF;
168000100917         ENDIF;
168100090323
168200100317         // è stato selezionato un potenziale all'interno di contatti
168300090929           If cpo_scelto  > *zeros  ;
168400090401                 StringaSql = %trim(Stringasql) + ' and  +
168500090929                  atccpo  = ' + %editc(cpo_scelto:'X');
168600090323           endif;
168700100121
168800100317         // è stato selezionato un cliente all'interno di contatti
168900100121           IF  Ksc_scelto  > *zeros  ;
169000100121                 StringaSql = %trim(Stringasql) + ' and  +
169100100121                  atcksc  = ' + %editc(Ksc_scelto:'X');
169200100121           ENDIF;
169300100210
169400090506
169500100317         // imposto l'ordinamento per data e ora da eseguire
169600100317           If $sel = ' ';
169700090515              stringasql = %trim(stringasql) + ' order by atcdad , atchda' ;
169800100317           endif;
169900090506
170000100317         // imposto l'ordinamento per data e ora eseguiti
170100100317           If $sel = 'E';
170200090515              stringasql = %trim(stringasql) + ' order by atcdco , atchco';
170300090506           endif;
170400090506
170500090506         stringasql = %trim(stringaSql) + ' for fetch only';
170600090323
170700090323
170800090323         exec sql prepare S1 from :StringaSql;
170900090323
171000090323         exec sql declare C1 cursor for S1;
171100090323
171200090424         exec sql open C1 ;
171300090424
171400080213         exsr sr_ReadRec;
171500080207
171600090507         dow  $EoF   = *off ;
171700080207           exsr RollUpS01;
171800080207         enddo;
171900080207
172000090424         exec sql close C1 ;
172100090625
172200090625       //  Valorizzo il numero massimo di record caricati
172300090625         rrnlast = s01nrr ;
172400090424
172500090625       // Eseguo l'ordinamento in base all'ordinamento selezionato
172600090625
172700090625         If Ord_Xcausale = *on ;
172800090625            exsr sr_ordcaus;
172900090625         endif ;
173000090625
173100090625         If Ord_Xdata    = *on ;
173200090625            exsr sr_orddata;
173300090625         endif ;
173400090625
173500090625         If Ord_Xprovinc = *on ;
173600090625            exsr sr_ordprov;
173700090625         endif ;
173800090625
173900090625
174000080207       // Impaginazione subfile
174100080207       // -> forza l'impaginazione sul 1° rec. del subfile
174200080207         if S01nrr > *zero;
174300080207           C01rcd  = 1;
174400080207           C01csr  = 1;
174500080207         else;
174600080207           clear C01rcd;
174700080207         endif;
174800090508
174900080207       ENDSR;
175000080207
175100090508       //--------------------------------------------------------------
175200090508       //?Pulizia SFL01
175300090508       //--------------------------------------------------------------
175400090508       BEGSR Pulsfl1;
175500090508
175600090508       // Pulizia subfile
175700090508         SflDsp_N    = *on;
175800090508         SflDspCtl_N = *on;
175900090508         write  MK18C01;
176000090508         SflDspCtl_N = *off;
176100090508         SflEnd      = *off;
176200090508
176300090508         clear W_SflPag;
176400090508         clear C01rcd;
176500090508         clear C01csr;
176600090508         clear S01nrr;
176700090508         clear V1Dmsg;
176800090508         errMessage  = *off;
176900090508         errGenerico = *off;
177000100617         %subst(IndDspF : 50) = *off;
177100090508
177200090508       ENDSR ;
177300090508
177400080207       //--------------------------------------------------------------
177500090507       //?Caricamento di tutto il sfl
177600080207       //--------------------------------------------------------------
177700080207       BEGSR RollUpS01;
177800080207
177900090507         clear  s01nrr;
178000080207         SflNxtChg = *off;
178100080207
178200080207         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
178300090507         DOW  $EoF   = *off  and
178400100121              S01nrr < *hival;
178500080207
178600080207         // - Caricamento dati nel record del subfile
178700080213           exsr  RieS01;
178800090601           If $recok = *on ;
178900090601              S01nrr += 1;
179000090601              write MK18S01;
179100090601           endif ;
179200100121         // se superati 9000 record eof e fine
179300100121           If s01nrr = 9000 ;
179400100121              $eof = *on ;
179500100121              leave ;
179600100121           endif ;
179700080207
179800080207         // - Lettura record successivo nell'archivio
179900080213           exsr sr_ReadRec;
180000080207
180100080207         ENDDO;
180200080207
180300080207         // Visualizzazione del SFL (se ci sono dati)
180400080207         SflDsp_N = (S01nrr <= *zeros);
180500080207
180600080207         // Attivazione (eventuale) del SFLEND
180700090507         SflEnd  =  *on;
180800080207
180900080207         // Posizionamento cursore al 1° rcd della pagina
181000080207         if  S01nrr > *zero;
181100090507         //  wPag   = S01nrr / C_SflPag;
181200090507         //  wRig   = S01nrr - (C_SflPag * wPag);
181300090507         //  C01rcd = wPag * C_SflPag;
181400090507         //  if  wRig > *zeros;
181500090507         //    C01rcd = C01rcd + 1;
181600090507         //  else;
181700090507         //    C01rcd = C01rcd - C_SflPag + 1;
181800090507         //  endif;
181900090507             c01rcd = 1;
182000080207         else;
182100080207           clear  C01rcd;
182200080207         endif;
182300080207
182400080207         C01csr = C01rcd;
182500091014
182600080207
182700080207       ENDSR;
182800080213
182900080213       //--------------------------------------------------------------
183000090323       //?Lettura record successivo
183100080213       //--------------------------------------------------------------
183200080213       BEGSR sr_ReadRec;
183300090323
183400090323         $RecOK  = *off;
183500090323
183600090515         exec sql Fetch C1 into :tiatcds;
183700090323
183800090323         if  Sqlcod = 100 ;
183900090323              $Eof  = *on;
184000090323         endif;
184100090323         If  Sqlcod = 0 ;
184200141125              exsr  sr_SelezRec;
184300090323         endif;
184400090407         if  Sqlcod  < 0 ;
184500090407              $Eof  = *on;
184600090407         endif;
184700141124
184800080213       ENDSR;
184900141124
185000141124       //--------------------------------------------------------------
185100141124       //?Selezione singolo record.
185200141124       //--------------------------------------------------------------
185300141124       BEGSR  sr_SelezRec;
185400141124
185500141125         // -?Reperimento del Numero Campagna (visualizzato nel subfile)?
185600141125         //  ?per il cliente (o meglio: per il suo unificante)?
185700150112         clear  TRKC73ds;
185800141125         if  ATCksc > *zero;
185900150112           iKC73ksu = ATCksc;
186000150112           iKC73dat = %dec( %date() );
186100150112           trkc73r ( kpjba : TRKC73ds );
186200141125         endif;
186300141125
186400141125         // -?Filtro per Numero Campagna (se immesso):?
186500141125         //  ?Se NON tornato il numero campagna  o?
186600141125         //  ?Se numero campagna diverso => scarto?
186700141125         if  V01ncm   <> *blank  and
186800141125             V01ncm   <> *zero   and
186900150112             %lookup( %int( %trim( V01ncm ) ) : sk_oKC73nc ) = *zero;
187000141125           leavesr;
187100141125         endif;
187200080207
187300141124         $RecOK  = *on;
187400141124
187500141124       ENDSR;
187600080207
187700080207       //--------------------------------------------------------------
187800080207       //?Caricamento singolo record nel SubFile S01
187900080207       //--------------------------------------------------------------
188000080208       BEGSR RieS01;
188100090910
188200090323         clear  MK18S01;
188300080211
188400090323         // Reperimento dati del potenziale
188500100617         sh1cpo = atccpo ;
188600090323         // Ragione sociale
188700090611         chain     atccpo tncpo01l;
188800090323         If   %found(tncpo01l);
188900090323              s01rag = cporag ;
189000090527              s01loc = cpocit ;
189100090601              sh1loc = cpocit ;
189200090527              s01prv = cpoprv ;
189300100308              s01cic = cpoftr ;
189400100617              SH1cap = CPOcap;
189500090323         endif ;
189600100617
189700100617         // Reperimento commerciale trattativa (se trattativa)
189800100617         if  ATCnrv <> *zero;
189900100617           chain  ATCnrv  TIVIS000;
190000100617           if  %found(TIVIS05L);
190100100617             SH1cmm = VIScmm;
190200100617           endif;
190300100617         endif;
190400090323
190500091019         // verifico se c'è il codice del cliente valorizzato lo segnalo
190600091019         If atcksc <> 0 ;
190700091019              s01ast = '*' ;
190800100617              sh1ksc = atcksc ;
190900100308              clear s01cic  ;
191000100308              clear tibs69ds;
191100100503              I69kac = atcksc;
191200100617              I69kin = atcksc;
191300100308              I69kcp = atcksc ;
191400100308              TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
191500100308           IF  O69err = *blanks;
191600100503              s01rag  = acorag;
191700100617              s01cic  = clpclv;
191800100617              SH1cap  = INDcae;
191900100917              s01loc  = INDcit ;
192000100917              sh1loc  = INDcit ;
192100100617              if  ATCnrv = *zero;
192200100617                SH1cmm = CLPage;
192300100617              endif;
192400100308           Endif ;
192500091019         endif ;
192600100617
192700100617         // - Se NON reperito né il comm.le trattativa né il comm.le
192800100617         //   cliente (CNACO), prende il comm.le potenziale (CPOCMM)
192900100617         if  SH1cmm = *zero   and   %found(TNCPO01L);
193000100617           SH1cmm = CPOcmm;
193100100617         endif;
193200080211
193300080207
193400090406         // COMMERCIALE. In caso di telefonate ancora da eseguire prendo commerciale
193500090323         // che deve eseguire il contatto altrimenti il commerciale che ha eseguito
193600090323         // il contatto ... controllo il tipo Selezione
193700100318         // Contatti eseguiti
193800100318         if  $sel = 'E'  and atccco > 0;
193900090323         // commerciale
194000090515             s01ccm = atccco;
194100090323         // data e ora contatto e causale eseguito
194200090515         If  atcdco > 0 ;
194300090515             data_dmy = %date(atcdco:*iso) ;
194400090401             s01dco = %dec(data_dmy) ;
194500090515             sh1dco = atcdco ;
194600090515             s01hco = %dec(%subst(%EDITC(atchco:'X'):1:4):4:0);
194700090515             s01cac = atccac ;
194800090515             sH1cac = atccac ;
194900090424             sh1fatto = 'S' ;
195000090402         endif ;
195100090406         endif ;
195200090323
195300100318         // Contatti da eseguire
195400100318         if  $sel = ' '  and atccco = 0;
195500090515             s01ccm = atccmm;
195600090402         // data e ora contatto e causale da eseguire
195700090515         If  atcdad > 0 ;
195800090515             data_dmy = %date(atcdad:*iso) ;
195900090402             s01dco = %dec(data_dmy) ;
196000100429             if atcdad >= datacor ;
196100090713                Coldcosca = *on ;
196200090713                Sh1n19 = '1' ;
196300090713             else ;
196400090713                Coldcosca = *off;
196500090713                Sh1n19 = '0' ;
196600090713             endif ;
196700090515             sh1dco = atcdad ;
196800090515             s01hco = %dec(%subst(%EDITC(atchda:'X'):1:4):4:0);
196900090515             sh1cac = atccad ;
197000090515             s01cac = atccad ;
197100090713         // verifico se già scaduto rispetto ad oggi
197200090402         endif ;
197300090323         endif;
197400090323
197500091020         // se ci sono delle parzializzazioni vedo se devo scrivere il record nel subfile oppure
197600091020         // no
197700091020
197800091020       // Località
197900091020
198000091020             If  $parz_x_loc = *on and %scan(%trimr(c01loc):sh1loc) = 0 ;
198100091020                 $RecOK  = *off;
198200091020                 leavesr ;
198300091020             endif ;
198400100617
198500100617       // Cap
198600100617
198700100617             If  $parz_x_cap = *on and SH1cap <> C01cap;
198800100617                 $RecOK  = *off;
198900100617                 leavesr ;
199000100617             endif ;
199100091020
199200091020       // Causale
199300091020
199400091020             If  $parz_x_cca = *on and s01cac <> c01cca  ;
199500091020                 $RecOK  = *off;
199600091020                 leavesr ;
199700091020             endif ;
199800091020
199900091020       // Commerciale
200000091020
200100100617             If  $parz_x_cmm = *on and SH1cmm <> c01cmm  ;
200200091020                 $RecOK  = *off;
200300091020                 leavesr ;
200400091020             endif ;
200500091020
200600090323         // decodifica del commerciale
200700130808             chain  (S01ccm)  AZCMM000;
200800130808             if  %found(AZCMM01L);
200900130808               s01ccmd = CMMdes;
201000090323             endif;
201100090323
201200090402         // decodifica della causale
201300090402             clear  dCCO;
201400090402             clear  TIBS02ds;
201500090402             T02mod = 'C';
201600090402             T02cod = 'CCO';
201700090402             T02ke1 = sh1cac;
201800090402             T02sif = KNSIF;
201900090402             TNTBE_RicercaControllo  (kpjba : tibs02ds);
202000090402             if  T02err = *blanks;
202100090402               dcco = t02uni ;
202200090505               s01cacd = §ccodes ;
202300100105         // valorizzo il campo delle opzioni inibite
202400100105               sh1opz = §ccoopz ;
202500100105
202600090625               if §ccoord <> *blanks ;
202700090625                  sh1ord  = §ccoord ;
202800090625               else ;
202900090625                  sh1ord  = '99'    ;
203000090625               endif ;
203100090402             endif ;
203200080207
203300090515             sh1tco = atctat ;
203400090515             sh1nco = atcatn ;
203500090515             sh1ncop= atcatnp;
203600100316             sh1nrv = atcnrv;
203700090428
203800090514         // tipo attività
203900090514
204000090811                 s01tco = atctat ;
204100090514
204200090629           // note contatto PRECEDENTE
204300091020                 cpncpo = cpocpo ;
204400091020                 cpnatn = atcatn ;
204500091020           // se l'attività non è stata eseguita prendo la nota precedente
204600091020                 if atcdco = 0    ;
204700091020                    cpnatnp  = atcatnp - 1;
204800091020                 else ;
204900091020                    cpnatnp  = atcatnp ;
205000091020                 endif ;
205100091020                 cpnpno = 1 ;
205200100520                 chain (CPNcpo : CPNatn : CPNatnp : CPNpno) TICPN05L ;
205300100520                 if   %found(TICPN05L);
205400091020                      s01no1 =  cpnnot  ;
205500091020                 endif ;
205600141125
205700141125         // -?Numero Campagna?
205800150112         if  oKC73nc2 = *zero;
205900150112           S01ncm = %editc( oKC73nc1 : 'Z' );
206000150102         else;
206100150102           S01ncm = '   +   ';
206200150102         endif;
206300090601
206400080207       ENDSR;
206500080207
206600090527       //--------------------------------------------------------------
206700090609       //?Controllo i dati della parzializzazione C01
206800090527       //--------------------------------------------------------------
206900090527       BEGSR CtrC01;
207000090527
207100090527       // Parzializzazione del subfile
207200090529
207300090529       // Località
207400100319          if  C01loc <> sav_C01loc;
207500090601             $parz_x_Loc = *on ;
207600090601             errGenerico  = *on;
207700090601             $InzS01  = *on ;
207800100319             sav_C01loc = C01loc;
207900090601          endif ;
208000100617
208100100617       // C.a.p.
208200100617          if  C01cap <> sav_C01cap;
208300100617             $parz_x_Cap = *on ;
208400100617             errGenerico  = *on;
208500100617             $InzS01  = *on ;
208600100617             sav_C01cap = C01cap;
208700100617          endif ;
208800090529
208900090601       // Causale
209000100319          if  C01cca <> sav_C01cca;
209100090601             $parz_x_Cca = *on ;
209200090601             errGenerico  = *on;
209300090601             $InzS01  = *on ;
209400100319             sav_C01cca = C01cca;
209500090601          endif ;
209600090601
209700100617       // Commerciale cliente
209800100617          if  C01cmm <> sav_C01cmm;
209900100617             $parz_x_Cmm = *on ;
210000090601             errGenerico  = *on;
210100090601             $InzS01  = *on ;
210200100617             sav_C01cmm = C01cmm;
210300090601          endif ;
210400090527
210500090527       ENDSR ;
210600080207       //--------------------------------------------------------------
210700080207       //?Gestione tasto funzionale F12 da videata S01
210800080207       //?F12=Ritorno
210900080207       //--------------------------------------------------------------
211000080207       BEGSR F12S01;
211100080207
211200080207         // Ritorno alla videata D01
211300080207         $Video = 'D1';
211400080207
211500080207       ENDSR;
211600080207
211700090406       //--------------------------------------------------------------
211800090505       //?Gestione tasto funzionale F07 da videata S01
211900090505       //?F07=Ordinamento per data
212000090406       //--------------------------------------------------------------
212100090625       BEGSR Sr_orddata;
212200090406
212300090508           Ord_Xdata = *on ;
212400090601           Ord_Xprovinc = *off ;
212500090625           Ord_Xcausale = *off ;
212600090508
212700090508       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
212800090508       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
212900090508       // il campo è usato per selezionare i records dando un ordine a quelli
213000090508       // selezionati davanti ai non selezionati.
213100090508         clear QLGSCB;
213200090508         clear QLGSCB00;
213300090508
213400090625       // 3 campi chiave x Data + ora contatto + ordinamento causale
213500090625         QLGNBRK = 3;
213600090508
213700090508       // imposto la posizione della Data     sul subfile e la lunghezza
213800090508       // l'ordinamento è su un campo numerico e deve essere
213900090508       // ascending
214000090508         QLGSP = 1 ;
214100090508         QLGSS = %SIZE(sh1dco);
214200090508         QLGDT = Numerico  ;
214300090508         QLGSO = Ascendente;
214400090508         QLGKL(1) = QLGSKL;
214500090508
214600090508       // imposto la posizione dell'Ora       sul subfile e la lunghezza
214700090508       // l'ordinamento è su un campo numerico e deve essere
214800090508       // ascending
214900090508         QLGSP = 1 + %size(sh1dco) ;
215000090508         QLGSS = %SIZE(s01hco);
215100090508         QLGDT = Numerico  ;
215200090508         QLGSO = Ascendente;
215300090508         QLGKL(2) = QLGSKL;
215400090508
215500090625       // imposto la posizione dell'Ordinamento causale sul subfile e la lunghezza
215600090625       // l'ordinamento è su un campo alfanumerico e deve essere
215700090625       // ascending
215800090625         QLGSP = 1 + %size(sh1dco) + %size(s01hco) + %size(s01prv);
215900090625         QLGSS = %SIZE(sh1ord);
216000090625         QLGDT = Carattere ;
216100090625         QLGSO = Ascendente;
216200090625         QLGKL(3) = QLGSKL;
216300090625
216400090508       // Load other sort parameters.
216500090508         QLGLB = 80 + 16 * MaxKey;
216600090508         QLGRL = %SIZE(sflrcd) - 1;
216700090508         QLGRT = 8;
216800090508         QLGOKL = 80;
216900090508         QLGLKE = 16;
217000090508         QLGLSS = 290;
217100090508
217200090508       // Initialize Sort I/O API fields.
217300090508         QLGRL00 = QLGRL;
217400090508         QLGRC00 = 1;
217500090508         clear QUSEI;
217600090508         QUSBPRV = %SIZE(QUSEC);
217700090508
217800090508       // First step - Initialize the sort routine.
217900090508         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
218000090508
218100090508       // Next step - Write records to I/O routine.
218200090508         QLGRT00 = Put;
218300090508         for xx = 1 to rrnlast;
218400090508          chain xx MK18S01;
218500090508
218600090508       // solo le righe con Selected = 'Y' sono riordinate,
218700090508       // quindi per fare un ordinamento di tutte le righe
218800090508       // metto 'Y' sempre.
218900090508          selected  = 'Y';
219000090508          clear QUSEI;
219100090508          QUSBPRV = %SIZE(QUSEC);
219200090508          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
219300090508         endfor;
219400090508
219500090508       // Next step - Signal end of input, clear subfile for reload.
219600090508         QLGRT00 = EndPut;
219700090508         clear QUSEI;
219800090508         QUSBPRV = %SIZE(QUSEC);
219900090508         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
220000090508       // pulizia SFL
220100090508         exsr pulsfl1;
220200090508
220300090508       // Final step - Write the records back to the subfile.
220400090508         QLGRT00 = Get;
220500090508         for xx = 1 to rrnlast;
220600090508          clear QUSEI;
220700090508          QUSBPRV = %SIZE(QUSEC);
220800090508          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
220900090508          s01nrr = xx;
221000090713          coldcosca = (sh1n19 = '1');
221100090508          write Mk18s01;
221200090508         endfor;
221300090508
221400090508         // Visualizzazione del SFL (se ci sono dati)
221500090508         SflDsp_N = (S01nrr <= *zeros);
221600090508
221700090508         // Attivazione (eventuale) del SFLEND
221800090508         SflEnd  =  *on;
221900090508
222000090508         // Posizionamento cursore al 1° rcd della pagina
222100090508         if  S01nrr > *zero;
222200090508             c01rcd = 1;
222300090508         else;
222400090508             clear  C01rcd;
222500090508         endif;
222600090508
222700090508         C01csr = C01rcd;
222800090508
222900090508
223000090406       ENDSR;
223100090406
223200090406       //--------------------------------------------------------------
223300090910       //?Gestione tasto funzionale F18 da videata S01
223400090910       //?F18=Ordinamento per provincia
223500090406       //--------------------------------------------------------------
223600090625       BEGSR sr_ordprov ;
223700090406
223800090625           Ord_Xdata = *off ;
223900090625           Ord_Xcausale = *off ;
224000090601           Ord_Xprovinc = *on  ;
224100090508
224200090508       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
224300090508       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
224400090508       // il campo è usato per selezionare i records dando un ordine a quelli
224500090508       // selezionati davanti ai non selezionati.
224600090508         clear QLGSCB;
224700090508         clear QLGSCB00;
224800090508
224900090625       // 4 campi chiave x Provincia + ordinamento causale + Data + ora contatto
225000090625         QLGNBRK = 4;
225100090508
225200090601       // imposto la posizione della Provincia  sul subfile e la lunghezza
225300090508       // l'ordinamento è su un campo numerico e deve essere
225400090508       // ascending
225500090508         QLGSP = 1 + %size(sh1dco)+ %size(s01hco) ;
225600090601         QLGSS = %SIZE(s01prv);
225700090508         QLGDT = Carattere ;
225800090508         QLGSO = Ascendente;
225900090508         QLGKL(1) = QLGSKL;
226000090508
226100090625
226200090625       // imposto la posizione dell'Ordinamento causale sul subfile e la lunghezza
226300090625       // l'ordinamento è su un campo alfanumerico e deve essere
226400090625       // ascending
226500090625         QLGSP = 1 + %size(sh1dco) + %size(s01hco) + %size(s01prv);
226600090625         QLGSS = %SIZE(sh1ord);
226700090625         QLGDT = Carattere ;
226800090625         QLGSO = Ascendente;
226900090625         QLGKL(2) = QLGSKL;
227000090625
227100090625       // imposto la posizione della Data     sul subfile e la lunghezza
227200090625       // l'ordinamento è su un campo numerico e deve essere
227300090625       // ascending
227400090625         QLGSP = 1 ;
227500090625         QLGSS = %SIZE(sh1dco);
227600090625         QLGDT = Numerico  ;
227700090625         QLGSO = Ascendente;
227800090625         QLGKL(3) = QLGSKL;
227900090625
228000090625       // imposto la posizione dell'Ora       sul subfile e la lunghezza
228100090625       // l'ordinamento è su un campo numerico e deve essere
228200090625       // ascending
228300090625         QLGSP = 1 + %size(sh1dco) ;
228400090625         QLGSS = %SIZE(s01hco);
228500090625         QLGDT = Numerico  ;
228600090625         QLGSO = Ascendente;
228700090625         QLGKL(4) = QLGSKL;
228800090625
228900090508       // Load other sort parameters.
229000090508         QLGLB = 80 + 16 * MaxKey;
229100090508         QLGRL = %SIZE(sflrcd) - 1;
229200090508         QLGRT = 8;
229300090508         QLGOKL = 80;
229400090508         QLGLKE = 16;
229500090508         QLGLSS = 290;
229600090508
229700090508       // Initialize Sort I/O API fields.
229800090508         QLGRL00 = QLGRL;
229900090508         QLGRC00 = 1;
230000090508         clear QUSEI;
230100090508         QUSBPRV = %SIZE(QUSEC);
230200090508
230300090508       // First step - Initialize the sort routine.
230400090508         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
230500090508
230600090508       // Next step - Write records to I/O routine.
230700090508         QLGRT00 = Put;
230800090508         for xx = 1 to rrnlast;
230900090508          chain xx MK18S01;
231000090508
231100090508       // solo le righe con Selected = 'Y' sono riordinate,
231200090508       // quindi per fare un ordinamento di tutte le righe
231300090508       // metto 'Y' sempre.
231400090508          selected  = 'Y';
231500090508          clear QUSEI;
231600090508          QUSBPRV = %SIZE(QUSEC);
231700090508          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
231800090508         endfor;
231900090508
232000090508       // Next step - Signal end of input, clear subfile for reload.
232100090508         QLGRT00 = EndPut;
232200090508         clear QUSEI;
232300090508         QUSBPRV = %SIZE(QUSEC);
232400090508         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
232500090508       // pulizia SFL
232600090508         exsr pulsfl1;
232700090508
232800090508       // Final step - Write the records back to the subfile.
232900090508         QLGRT00 = Get;
233000090508         for xx = 1 to rrnlast;
233100090508          clear QUSEI;
233200090508          QUSBPRV = %SIZE(QUSEC);
233300090508          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
233400090508          s01nrr = xx;
233500090713          coldcosca = (sh1n19 = '1');
233600090508          write Mk18s01;
233700090508         endfor;
233800090508
233900090508         // Visualizzazione del SFL (se ci sono dati)
234000090508         SflDsp_N = (S01nrr <= *zeros);
234100090508
234200090508         // Attivazione (eventuale) del SFLEND
234300090508         SflEnd  =  *on;
234400090508
234500090508         // Posizionamento cursore al 1° rcd della pagina
234600090508         if  S01nrr > *zero;
234700090508             c01rcd = 1;
234800090508         else;
234900090508             clear  C01rcd;
235000090508         endif;
235100090508
235200090508         C01csr = C01rcd;
235300090406
235400090406       ENDSR;
235500090625
235600090625       //--------------------------------------------------------------
235700090910       //?Gestione tasto funzionale F19 da videata S01
235800090910       //?F19=Ordinamento per ordinamento causale
235900090625       //--------------------------------------------------------------
236000090625       BEGSR sr_ordcaus ;
236100090625
236200090625           Ord_Xdata = *off ;
236300090625           Ord_Xcausale = *on  ;
236400090625           Ord_Xprovinc = *off ;
236500090625
236600090625       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
236700090625       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
236800090625       // il campo è usato per selezionare i records dando un ordine a quelli
236900090625       // selezionati davanti ai non selezionati.
237000090625         clear QLGSCB;
237100090625         clear QLGSCB00;
237200090625
237300090625       // 3 campi chiave x Ordinamento causale + data contatto + ora contatto
237400090625         QLGNBRK = 3;
237500090625
237600090625       // imposto la posizione dell'Ordinamento causale sul subfile e la lunghezza
237700090625       // l'ordinamento è su un campo numerico e deve essere
237800090625       // ascending
237900090625         QLGSP = 1 + %size(sh1dco)+ %size(s01hco) + %size(s01prv) ;
238000090625         QLGSS = %SIZE(sh1ord);
238100090625         QLGDT = Carattere ;
238200090625         QLGSO = Ascendente;
238300090625         QLGKL(1) = QLGSKL;
238400090625
238500090625       // imposto la posizione della Data     sul subfile e la lunghezza
238600090625       // l'ordinamento è su un campo numerico e deve essere
238700090625       // ascending
238800090625         QLGSP = 1 ;
238900090625         QLGSS = %SIZE(sh1dco);
239000090625         QLGDT = Numerico  ;
239100090625         QLGSO = Ascendente;
239200090625         QLGKL(2) = QLGSKL;
239300090625
239400090625       // imposto la posizione dell'Ora       sul subfile e la lunghezza
239500090625       // l'ordinamento è su un campo numerico e deve essere
239600090625       // ascending
239700090625         QLGSP = 1 + %size(sh1dco) ;
239800090625         QLGSS = %SIZE(s01hco);
239900090625         QLGDT = Numerico  ;
240000090625         QLGSO = Ascendente;
240100090625         QLGKL(3) = QLGSKL;
240200090625
240300090625       // Load other sort parameters.
240400090625         QLGLB = 80 + 16 * MaxKey;
240500090625         QLGRL = %SIZE(sflrcd) - 1;
240600090625         QLGRT = 8;
240700090625         QLGOKL = 80;
240800090625         QLGLKE = 16;
240900090625         QLGLSS = 290;
241000090625
241100090625       // Initialize Sort I/O API fields.
241200090625         QLGRL00 = QLGRL;
241300090625         QLGRC00 = 1;
241400090625         clear QUSEI;
241500090625         QUSBPRV = %SIZE(QUSEC);
241600090625
241700090625       // First step - Initialize the sort routine.
241800090625         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
241900090625
242000090625       // Next step - Write records to I/O routine.
242100090625         QLGRT00 = Put;
242200090625         for xx = 1 to rrnlast;
242300090625          chain xx MK18S01;
242400090625
242500090625       // solo le righe con Selected = 'Y' sono riordinate,
242600090625       // quindi per fare un ordinamento di tutte le righe
242700090625       // metto 'Y' sempre.
242800090625          selected  = 'Y';
242900090625          clear QUSEI;
243000090625          QUSBPRV = %SIZE(QUSEC);
243100090625          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
243200090625         endfor;
243300090625
243400090625       // Next step - Signal end of input, clear subfile for reload.
243500090625         QLGRT00 = EndPut;
243600090625         clear QUSEI;
243700090625         QUSBPRV = %SIZE(QUSEC);
243800090625         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
243900090625       // pulizia SFL
244000090625         exsr pulsfl1;
244100090625
244200090625       // Final step - Write the records back to the subfile.
244300090625         QLGRT00 = Get;
244400090625         for xx = 1 to rrnlast;
244500090625          clear QUSEI;
244600090625          QUSBPRV = %SIZE(QUSEC);
244700090625          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
244800090625          s01nrr = xx;
244900090713          coldcosca = (sh1n19 = '1');
245000090625          write Mk18s01;
245100090625         endfor;
245200090625
245300090625         // Visualizzazione del SFL (se ci sono dati)
245400090625         SflDsp_N = (S01nrr <= *zeros);
245500090625
245600090625         // Attivazione (eventuale) del SFLEND
245700090625         SflEnd  =  *on;
245800090625
245900090625         // Posizionamento cursore al 1° rcd della pagina
246000090625         if  S01nrr > *zero;
246100090625             c01rcd = 1;
246200090625         else;
246300090625             clear  C01rcd;
246400090625         endif;
246500090625
246600090625         C01csr = C01rcd;
246700090625
246800090625       ENDSR;
246900090625
247000080207       //--------------------------------------------------------------
247100080207       //?Gestione opzioni subfile
247200080207       //--------------------------------------------------------------
247300080207       BEGSR OpzS01;
247400080207
247500090406         readc MK18S01;
247600080207
247700090406         DOW  NOT  %eof(TRMK18D);
247800080207
247900100617           %subst(IndDspF : 50) = *off;
248000080207           SflNxtChg = *off;
248100080207           C01rcd    = S01nrr;
248200100318
248300100318       //?Controllo se utente abilitato all'attività
248400100318           $UteNoAbi = *off;
248500100318           exsr CtrAbiAtt;
248600080207
248700100720             // verifico se nel frattempo è stata eseguita da altro utente
248800100720                If   Sh1fatto = ' ';
248900100720                   chain (sh1tco:sh1nco:sh1ncop) tiatc01l ;
249000100720                   If %found(tiatc01l) and atcdco > 0 ;
249100100720                      sh1fatto = 'S' ;
249200100720                   endif ;
249300100720                endif ;
249400100720
249500080207           SELECT;
249600080207
249700080207             // - Nessuna opzione
249800080208             WHEN  S01opz  = *blank;
249900100105
250000110804             // - SOLO opzione 5 IN CASO di utenete di sede
250100110804             WHEN  S01opz <> '5' and Sede ;
250200110804                   errMessage  = *on;
250300110804                   errGenerico = *on;
250400110804                   PosCurOpz   = *on;
250500110804                   V1Dmsg = $Msg(18);
250600110804
250700100105             // - Nessuna opzione
250800100105             WHEN  S01opz <> *blank and sh1opz <> *blanks and
250900100105                   %scan(s01opz:sh1opz) > 0 ;
251000100105                   errMessage  = *on;
251100100105                   errGenerico = *on;
251200100105                   PosCurOpz   = *on;
251300100105                   V1Dmsg = $Msg(11);
251400080208
251500080208             // - Precedente segnalazione di errore
251600080208             WHEN  S01opz  = 'E';
251700080208               clear S01opz;
251800080207
251900090514             // - G = Gestione
252000090514             WHEN  S01opz  = 'G';
252100090514             // verifico se imposto G per un contatto già eseguito
252200100720                If   Sh1fatto = 'S';
252300090514                     errMessage  = *on;
252400090514                     errGenerico = *on;
252500090514                     PosCurOpz   = *on;
252600090609                     V1Dmsg = $Msg(09);
252700090514                else ;
252800100318              //?Se l'utente non è abilitato all'attività  solo interrogazione
252900100318                IF  $UteNoAbi;
253000100318                  ErrMessage  = *on;
253100100318                  ErrGenerico = *on;
253200100318                  PosCurOPZ   = *on;
253300100318                  V1Dmsg      = $Msg(15);
253400100318                ELSE;
253500100312
253600100223            // verifico il tipo attività ed in base a questo richiamo un programma
253700100223            // invece di un'altro
253800100316                     if sh1nrv > 0;
253900100223                        exsr Calltnta88r;
254000100223                     else ;
254100100223                        exsr Calltrmk19  ;
254200100223                     endif ;
254300100721                     If errMessage = *off;
254400100720                        $inzs01 = *on ;
254500100720                     endif ;
254600100312                  ENDIF;
254700090514                endif ;
254800090514
254900090514             // - 2 = Manutenzione
255000090514             WHEN  S01opz  = '2';
255100090514
255200090514             // verifico se imposto 2 per un contatto già eseguito
255300090514                If   Sh1fatto = 'S';
255400090514                     errMessage  = *on;
255500090514                     errGenerico = *on;
255600090514                     PosCurOpz   = *on;
255700090609                     V1Dmsg = $Msg(09);
255800090514                else ;
255900100312
256000100312              //?Se l'utente non è abilitato al potenziale solo interrogazione
256100100318                  IF  $UteNoAbi;
256200100312                    ErrMessage  = *on;
256300100312                    ErrGenerico = *on;
256400100312                    PosCurOPZ   = *on;
256500100312                    V1Dmsg      = $Msg(15);
256600100312                  ELSE;
256700090611                     exsr Calltrmk19  ;
256800100721                     If errMessage = *off;
256900100720                        $inzs01 = *on ;
257000100720                     Endif ;
257100100312                  ENDIF;
257200090514                endif;
257300090514
257400090514             // - 5 = Visualizzazione
257500090514             WHEN  S01opz  = '5';
257600090611                exsr Calltrmk19  ;
257700090514
257800080207             // - ? = Opzione NON valida
257900080207             OTHER;
258000080207               errMessage  = *on;
258100080207               errGenerico = *on;
258200080207               PosCurOpz   = *on;
258300090609               V1Dmsg = $Msg(10);
258400080207
258500080207           ENDSL;
258600080207
258700080207           // Aggiornamento sfl
258800080207           select;
258900080207             when  errMessage  = *on;
259000080207               SflNxtChg = *on;
259100080207               C01csr    = C01rcd;
259200080207             when  errGenerico = *on;
259300080208               C01csr    = C01rcd;
259400080208               clear  S01opz;
259500080207             other;
259600080207               C01csr    = C01rcd;
259700080208               clear  S01opz;
259800080207           endsl;
259900080207
260000090720           coldcosca = (sh1n19 = '1');
260100090406           update MK18S01;
260200080207
260300080208           if  errMessage = *on  or  errGenerico = *on;
260400080207             leave;
260500080207           endif;
260600080207
260700090406           readc MK18S01;
260800080207
260900080207         ENDDO;
261000080207
261100080207       ENDSR;
261200100318
261300100318       //--------------------------------------------------------------
261400100318       //?Controllo se utente abilitato all'attività.
261500100318       //--------------------------------------------------------------
261600100318       BEGSR CtrAbiAtt;
261700100318
261800100318       //?abilitato come commerciale
261900100318         clear TNTAA1DS;
262000100318         ITAA1caut = '§utegtc';
262100100318         ITAA1cmm  = S01ccm;
262200100318         kpjbu     = TNTAA1DS;
262300100318         tntaa1c (kpjba);
262400100318         TNTAA1DS = kpjbu;
262500100325         IF  OTAA1fabi <> 'N';
262600100325           leavesr;
262700100325         ENDIF;
262800100318
262900100318       //?abilitato come cliente
263000100617         IF  SH1ksc > 0;
263100100325           clear TNTAA1DS;
263200100325           ITAA1caut = '§utegtc';
263300100617           ITAA1ksc  = %dec(SH1ksc:7:0);
263400100325           kpjbu     = tntaa1ds;
263500100325           tntaa1c (kpjba);
263600100325           TNTAA1DS = kpjbu;
263700100325           IF  OTAA1fabi <> 'N';
263800100325             leavesr;
263900100325           ENDIF;
264000100325         ENDIF;
264100100325
264200100325       //?se arrivo a questo punto vuol dire che l'utente non è abilitato
264300100325       //?al commerciale o al cliente
264400100325         $UteNoAbi = *on;
264500100318
264600100318       ENDSR;
264700080207
264800090611       //--------------------------------------------------------------
264900090611       //?Richiamo trmk19r Gestione dettaglio contatti
265000090611       //--------------------------------------------------------------
265100090611       BEGSR Calltrmk19 ;
265200100312
265300090611
265400090611                     clear trmk19ds ;
265500090611                     imk19fle = s01opz ;
265600090611                     imk19cmt = 'S' ;
265700090611                     imk19fatt  = sh1fatto ;
265800090611                     imk19tco = s01tco ;
265900100617                     imk19cpo = sh1cpo ;
266000100617                     imk19ksc = sh1ksc ;
266100090611                     imk19tat = sh1tco ;
266200090611                     imk19atn = sh1nco ;
266300090611                     imk19atnp= sh1ncop;
266400090611                     if v01cmg <> *blanks ;
266500090611                        imk19com = %dec(v01cmg :7:0);
266600090611                        imk19comd = v01cmgd ;
266700090611                     endif ;
266800100312
266900110805              //?Se l'utente non è abilitato
267000100312              //?imposto IMK19fpr in modo da non abilitare dei tasti funzione
267100100316              //?in interrogazione attività, come già previsto dal pgm
267200100316              //?CHIODO 'V'  non ancora utilizzato
267300110805                  IF  $UteNoAbi ;
267400100312                    IMK19fpr = 'V';
267500100312                  ENDIF;
267600100312
267700090611                     trmk19r (kpjba : trmk19ds) ;
267800100720              // se ritorna l'errore lo emetto a video
267900100720                 If omk19err =  'E' ;
268000100720                     errMessage  = *on;
268100100720                     errGenerico = *on;
268200100720                     PosCurOpz   = *on;
268300100720                     V1Dmsg = omk19msg;
268400100720                endif;
268500090611
268600090611       endsr;
268700100223
268800100223       //--------------------------------------------------------------
268900100223       //?Richiamo TNTA88R Gestione trattative
269000100223       //--------------------------------------------------------------
269100100223       BEGSR Calltnta88r;
269200100223
269300100223             clear tnta88ds ;
269400100223             ita88cmt = 'N';
269500100223             ita88fle = 'G' ;
269600100316             ita88nrv = sh1nrv ;
269700100303                     if v01cmg <> *blanks ;
269800100303                        ita88com = %dec(v01cmg :7:0);
269900100303                        ita88comd = v01cmgd ;
270000100303                     endif ;
270100100223             Tnta88r ( kpjba : tnta88ds );
270200100223
270300100223       endsr;
270400080206       //--------------------------------------------------------------
270500080206       //?Operazioni finali.
270600080206       //--------------------------------------------------------------
270700080206       BEGSR RoutEnd;
270800090521
270900080206         *inLR = *on;
271000080206         return;
271100080206
271200080206       ENDSR;
271300080206
271400080206      /end-free
271500080206       //--------------------------------------------------------------
271600080206       //?Schiere a tempo di compilazione.
271700080206       //--------------------------------------------------------------
271800080206
271900080206** - $MSG -------------------------------------------------------------------*
272000100316Utente non abilitato alla funzione                                             01
272100100318Inserire "Da effettuare" oppure "Effettuate"                                   02
272200100209Codice commerciale errato o non in gestione all'utente                         03
272300090319Data errata                                                                    04 1
272400090319Data al deve essere maggiore o uguale a data dal                               05 4
272500100209Filiale errata o non in gestione all'utente                                    06
272600100316Non è stata trovata nessuna Attività                                           07
272700080208Potenziale inesistente                                                         08
272800100316Opzione non valida per attività già eseguite                                   09
272900090609Opzione errata                                                                 10
273000100105Opzione non valida per questa attività                                         11
273100100209Codice cliente errato o non in gestione all'utente                             12
273200150219Campagna inesistente                                                           13
273300141124Inserire almeno un dato tra comm.le, potenziale, cliente, filiale o campagna   14
273400100325Utente non abilitato. L'attività si può solo interrogare                       15
273500100316Inserire il Potenziale o il Cliente, non è possibile immettere entrambi.       16
273600141124Funzione abilitata per utente di filiale con NUOVA GESTIONE TRATTATIVE         17 NOT USED!
273700110804Opzione non valida.E' possibile solo la VISUALIZZAZIONE                        18
273800150220Campagna non ancora attiva o già scaduta                                       19
