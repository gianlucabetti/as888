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
010600160406
010700160406      // -?Causali Contatto Marketing?
010800160406     d sk_CCO_k        s                   like(ATCcad)  dim(99)  inz
010900160406     d sk_CCO_d        s                   like(ds_CCO)  dim(99)  inz
011000080206
011100080206      //---------------------------------------------------------------
011200080206      //?Definizione aree dati.
011300080206      //---------------------------------------------------------------
011400080206
011500080206      // - Dati utente
011600080206     d §AzUte        e ds                  extname(AZUTE00F)
011700080206     d                                     dtaara
011800080206     d §DatiUte      e ds                  extname(dDatiUte)
011900080206     d                                     dtaara
012000090515     d tiatcDS       e ds                  ExtName(tiatc00f)
012100080206
012200080206      //---------------------------------------------------------------
012300080206      //?Definizione strutture dati.
012400080206      //---------------------------------------------------------------
012500080206
012600080206      // - Status
012700080206     d Psds           sds
012800080206     d   SDSpgm          *proc
012900080206
013000080206      // - InfDS
013100080206     d InfDspF         ds
013200080207     d  dsp_aid              369    369a                                        AID byte
013300130808     d**sfl_rrn              376    377i 0                                      Subfile rrn
013400130808     d**min_nrr              378    379i 0                                      Subfile min rrn
013500130808     d**num_rcds             380    381i 0                                      Subfile num rcds
013600080206
013700080206      // - Indicatori su DspF
013800080208     d IndDspF         ds
013900100617        // - Indicatori per ordinamento sbfl
014000100617     d  ORD_Xdata                     1n   overlay(IndDspF : 07)
014100100617     d  ORD_Xprovinc                  1n   overlay(IndDspF : 08)
014200100617     d  ORD_Xcausale                  1n   overlay(IndDspF : 09)
014300100617        // - Indicatori di visualizzazione
014400100617     d  da_eseguire                   1n   overlay(IndDspF : 10)
014500100617     d  Coldcosca                     1n   overlay(IndDspF : 19)
014600110803     d  Sede                          1n   overlay(IndDspF : 20)
014700100617        // - Indicatori di emissione messaggio di errore
014800100617     d  errMessage                    1n   overlay(IndDspF : 28)
014900080206        // - Indicatori di gestione del subfile
015000080206     d  SflDsp_N                      1n   overlay(IndDspF : 30)
015100080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
015200080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
015300080206     d  SflEnd                        1n   overlay(IndDspF : 33)
015400090422        // - Indicatori di errore 1° videata
015500090317     d  PosCurCmg                     1n   overlay(IndDspF : 50)
015600090317     d  PosCurCac                     1n   overlay(IndDspF : 51)
015700090317     d  PosCurAsc                     1n   overlay(IndDspF : 52)
015800090317     d  PosCurCcm                     1n   overlay(IndDspF : 53)
015900090317     d  PosCurCp1                     1n   overlay(IndDspF : 54)
016000090317     d  PosCurEda                     1n   overlay(IndDspF : 55)
016100090317     d  PosCurEal                     1n   overlay(IndDspF : 56)
016200090317     d  PosCurFlt                     1n   overlay(IndDspF : 57)
016300100121     d  PosCurKsc                     1n   overlay(IndDspF : 58)
016400141124     d  PosCurNCM                      n   overlay(IndDspF : 59)
016500090529        // - Indicatori di errore subfile
016600090529     d  PosCurOpz                     1n   overlay(IndDspF : 50)
016700090422        // - Indicatori di errore generico
016800080206     d  errGenerico                   1n   overlay(IndDspF : 99)
016900080206
017000090407
017100090407      //  codice cliente potenziale a video
017200090427     d V01cpo          ds                  inz
017300090427     d   V01cp1                            inz
017400090427     d   V01cp2                            inz
017500090319
017600090319     d wlbdat          ds                  inz
017700090319     d  g02dat                 1      8  0
017800090319     d  g02inv                 9     16  0
017900090319     d  g02err                17     17
018000090319     d  g02tgi                18     22  0
018100160406
018200160406      // -?Dati di singola Causale Contatto Marketing?
018300160406     d ds_CCO          ds                  inz   qualified
018400160406     d   §CCOdes                           inz   like(§CCOdes)
018500160406     d   §CCOord                           inz   like(§CCOord)
018600160406     d   §CCOopz                           inz   like(§CCOopz)
018700080206
018800080206      // - Parametri ricevuti
018900080206     d KPJBA         e ds
019000080206
019100080206      // - Reperimento dati utente
019200080206     d TIBS34ds      e ds
019300081020     d TRUL31DS      e ds
019400081020     d POG                    10    759    DIM(250)
019500100121
019600100121       // - Reperimento dati anagrafici
019700100121     d TIBS69ds      e ds
019800100121     d DS_cnaco      e ds                  inz extname(CNACO00F)
019900100121     d DS_cnind      e ds                  inz extname(CNIND00F)
020000100121     d DS_cnclp      e ds                  inz extname(CNCLP00F)
020100100121     d DS_fncls      e ds                  inz extname(FNCLS00F)
020200080206
020300080206      // - Ricerca/Controllo tabelle
020400080206     d TIBS02ds      e ds                  inz
020500080206     d   T02Mod      e                     inz('C')
020600100209
020700100209      // - controllo abilitazioni
020800100209     d TNTAA1DS      e ds                  inz
020900090521
021000090609      // - Ricerca Causali contatto
021100090521     d Tntb74ds      e ds                  inz
021200090529
021300090402      // - Tabella CCO = Causale contatto
021400090402     d dcco          e ds                  inz
021500090402
021600091130      // - Tabella TTA = Tipo Attività
021700091130     d dtta          e ds                  inz
021800090423
021900090609      // - Gestione contatti
022000090609     d TRMK19ds      e ds                  inz
022100090928
022200090420      // - Interrogazione anagrafica clienti potenziali
022300091104     d TRMK01ds      e ds                  inz
022400100430
022500100430      // -  Interrogazione agenda
022600100430     d TRMK82DS      e ds                  inz
022700090629
022800100223      // - Gestione trattative commerciali
022900100223     d TNTA88ds      e ds                  inz
023000100223
023100080206      //---------------------------------------------------------------
023200080206      //?Definizione variabili globali.
023300080206      //---------------------------------------------------------------
023400080206
023500080206      // - Flags booleani
023600100210     d $End            s               n   inz(*off)
023700080208     d $Fine           s               n   inz(*off)
023800080208     d $InzD01         s               n   inz(*on)
023900080208     d $InzS01         s               n   inz(*on)
024000080206     d $ErrGrave       s               n   inz(*off)
024100080207     d $EoF            s               n   inz(*off)
024200080208     d $RecOK          s               n   inz(*off)
024300090601     d $parz_x_Loc     s               n   inz(*off)
024400100617     d $parz_x_Cap     s               n   inz(*off)
024500090609     d $parz_x_Cca     s               n   inz(*off)
024600100617     d $parz_x_Cmm     s               n   inz(*off)
024700100317     d $Uni            s               n   inz(*off)
024800100318     d $UteNoAbi       s               n   inz(*off)
024900090609
025000080206
025100080206      // - Indici di schiera
025200100120     d xx              s              4  0 inz
025300100210     d yy              s              4  0 inz
025400100916     d ww              s              4  0 inz
025500100916     d zz              s              4  0 inz
025600080206
025700080207      // - Campi associati al video
025800080207     d $Video          s              2    inz('D1')
025900080208     d S01nrr          s              4  0 inz
026000080207     d W_SflPag        s              3  0 inz
026100130808     d*// wPag            s              4  0 inz
026200130808     d*// wRig            s              3  0 inz
026300090601
026400090601     d dsp_mod         s             10I 0
026500080206
026600090401      // - Campi di comodo data
026700090401     d  data_iso       s               d   Datfmt(*iso)
026800090401     d  data_dmy       s               d   Datfmt(*dmy)
026900090401
027000080206      // - Campi di comodo
027100090427     d  w01asc         s              8  0
027200090427     d  w01eda         s              8  0
027300090427     d  w01eal         s              8  0
027400090714     d  datacor        s              8  0
027500100802     d  wabi           s              2
027600100318     d  wage           s              7
027700090323     d  $Sel           s              1    inz                                  "E"=eseguite
027800111227     d wOggi           s              8s 0
027900090323     d                                                                          " "=da eseguire
028000150102     d*//  Prgcontatto    s                   like(atcatnp)
028100090929     d  Cpo_scelto     s             11  0
028200100121     d  Ksc_scelto     s                   like(acoksc)
028300100319
028400100319     d  sav_C01loc     s                   like(C01loc)
028500100617     d  sav_C01cap     s                   like(C01cap)
028600100319     d  sav_C01cca     s                   like(C01cca)
028700100617     d  sav_C01cmm     s                   like(C01cmm)
028800080206
028900100209       // - Stringhe SQL da eseguire
029000101102     d StringaSql      s           2048    Varying        inz
029100100209     d wSQL            s           2048    Varying        inz
029200090323
029300090508      //---------------------------------------------------------------
029400090508      //?Ordinamento SBFL
029500090508      //---------------------------------------------------------------
029600090508      // ----------------------------------------------------------------------
029700090508      //?Costanti per ordinamento subfile
029800090508      // ----------------------------------------------------------------------
029900090625     d MaxKey          c                   4
030000090508     d Ascendente      c                   1
030100090508     d Discendente     c                   2
030200090508     d Carattere       c                   6
030300090508     d Numerico        c                   8
030400090508     d Put             c                   1
030500090508     d EndPut          c                   2
030600090508     d Get             c                   3
030700090508      **********************************************************************
030800090508      * Campi utili:
030900090508      *     nrr        - Numero relativo di record del Subfile
031000090508      *     SizeList   - Dimensione della lista
031100090508      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
031200090508      **********************************************************************
031300090508     d NotUsed         s             16A
031400090508     d ReturnSize      s              9B 0
031500090508     d SizeList        s              9B 0
031600090508     d RrnLast         s              5I 0
031700150102     d WrkSort         s              1  0 inz(0)
031800090508      **********************************************************************
031900090508      * Data Structures
032000090508      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
032100090508      *     QLGSCB     - The sort request block for the QLGSORT API
032200090508      *     QLGSCB00   - The sort request block for the QLGSRTIO API
032300090508      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
032400090508      *     QUSEC      - Error structure for the QLGSORT API
032500090508      **********************************************************************
032600090508     d sflrcd          ds
032700090508     d  sh1dco
032800090508     d  s01hco
032900090601     d  s01prv
033000090625     d  sh1ord
033100090508     d  s01ccm
033200090508     d  s01ccmd
033300090601     d  s01cac
033400090527     d  s01loc
033500090508     d  sh1tco
033600090508     d  sh1nco
033700090508     d  sh1ncop
033800090508     d  sh1fatto
033900090508     d  sh1cac
034000090508     d  s01rag
034100090515     d  s01tco
034200090508     d  s01dco
034300090508     d  s01cacd
034400100617     d  sh1cpo
034500090601     d  sh1loc
034600090629     d  s01no1
034700090713     d  sh1n19
034800091019     d  s01ast
034900100617     d  sh1ksc
035000100105     d  sh1opz
035100100308     d  s01cic
035200100316     d  sh1nrv
035300100617     d  SH1cap
035400100617     d  SH1cmm
035500141125     d  S01ncm
035600090508     d  selected                      1A
035700090508
035800090508      /COPY QSYSINC/QRPGLESRC,QLGSORT
035900090508     d QLGKL                         16    DIM(MaxKey)
036000090508     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
036100090508     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
036200090508     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
036300090508     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
036400090508
036500090508      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
036600090508      /COPY QSYSINC/QRPGLESRC,QUSEC
036700090508
036800080208
036900090508      //---------------------------------------------------------------
037000090508      //?Definizione procedure usate.
037100090508      //---------------------------------------------------------------
037200100802
037300141201       // - Interrogazione anagrafica clienti
037400150112     d TRKC20ds      e ds                  inz
037500150112     d TRKC20R         pr                  extpgm('TRKC20R')
037600141201     d  kpjba                              likeds(kpjba)
037700150112     d  TRKC20ds                           likeds(TRKC20ds)
037800100408
037900100408      *GestioneInterrogazione_potenziale...
038000100408     d TRMK01R         pr                  extpgm('TRMK01R')
038100100408     d  kpjba                              likeds(kpjba)
038200100408     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
038300100430
038400100430       // - Pgm Agenda
038500100430     D TRMK82R         pr                  extpgm('TRMK82R')
038600100430     D  kpjba                              likeds(kpjba)
038700080206
038800080626      //---------------------------------------------------------------
038900080626      //?prototipi
039000080626      //---------------------------------------------------------------
039100080626      /copy gaitrasrc/srcprotopr,tibs02r
039200080626      /copy gaitrasrc/srcprotopr,tibs34r
039300100121      /copy gaitrasrc/srcprotopr,tibs69r
039400100209      /copy gaitrasrc/srcprotopr,tntaa1c
039500100223      /copy gaitrasrc/srcprotopr,tnta88r
039600090609      /copy gaitrasrc/srcprotopr,trmk19r
039700130808      /copy gaitrasrc/srcprotoPI,TRMK43R_1
039800130808      /copy gaitrasrc/srcprotoPR,TRMK43R
039900081020      /copy gaitrasrc/srcprotopr,trul31r
040000090406      /copy gaitrasrc/srcprotopr,xsrda8
040100090521      /copy gaitrasrc/srcprotopr,tntb74r
040200141125
040300141125       // -?Interrogazione Campagne?
040400150112     d TRKC02ds      e ds                  inz
040500150112     d trkc02r         pr                  extpgm('TRKC02R')
040600141125     d   kpjba                             likeds(kpjba)
040700150112     d   TRKC02ds                          likeds(TRKC02ds)
040800141111
040900141111       // -?Interrogazione clienti in Campagna?
041000150112     d*// TRKC21ds      e ds                  inz
041100150112     d*// wSaveNCM        s                   inz  like(oKC21ncm)
041200150112      *///copy gaitrasrc/srcProtoPR,TRKC21R
041300150112     d*// TRKC41R         pr                  extpgm('TRKC41R')
041400150102     d*//   KPJBA                             likeds(KPJBA)
041500150112     d*//   TRKC21ds                          likeds(TRKC21ds)
041600141125
041700141125       // -?Controllo se cliente in Campagna?
041800150112     d*// TRKC70ds      e ds                  inz
041900150112     d*// TRKC70R         pr                  extpgm('TRKC70R')
042000150102     d*//   kpjba                             likeds(kpjba)
042100150112     d*//   TRKC70ds                          likeds(TRKC70ds)
042200150112     d TRKC73ds      e ds                  inz
042300150206     d   sk_oKC73nc           24     93  0 inz  dim(10)
042400150206     d   sk_oKC73dc           94    493    inz  dim(10)
042500150112     d TRKC73R         pr                  extpgm('TRKC73R')
042600150102     d   kpjba                             likeds(kpjba)
042700150112     d   TRKC73ds                          likeds(TRKC73ds)
042800100408
042900090601      /copy gaitrasrc/srcprotopr,QsnQryModS
043000090508
043100090508     d Qlgsort_pr      pr                  extpgm('QLGSORT')
043200090508     d  pr_QLGSCB                          like(Qlgscb)
043300090508     d  pr_NotUsed1                        like(NotUsed)
043400090508     d  pr_NotUsed2                        like(NotUsed)
043500090508     d  pr_SizeList                        like(SizeList)
043600090508     d  pr_ReturnSize                      like(ReturnSize)
043700090508     d  pr_QUSEC                           like(Qusec)
043800090508     d                                     options(*varsize)
043900090508
044000090508     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
044100090508     d  pr_QLGSCB00                        like(QLGSCB00)
044200090508     d  pr_SflRcd                          like(SflRcd)
044300090508     d  pr_NotUsed1                        like(NotUsed)
044400090508     d  pr_SizeList                        like(SizeList)
044500090508     d  pr_NotUsed2                        like(NotUsed)
044600090508     d  pr_QUSEC                           like(Qusec)
044700090508     d                                     options(*varsize)
044800090508
044900090508     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
045000090508     d  pr_QLGSCB00                        like(QLGSCB00)
045100090508     d  pr_NotUsed1                        like(NotUsed)
045200090508     d  pr_SflRcd                          like(SflRcd)
045300090508     d  pr_SizeList                        like(SizeList)
045400090508     d  pr_NotUsed2                        like(NotUsed)
045500090508     d  pr_QUSEC                           like(Qusec)
045600090508     d                                     options(*varsize)
045700141124
045800080206      //---------------------------------------------------------------
045900080206      //?Definizione key-list.
046000080206      //---------------------------------------------------------------
046100080206
046200080206
046300080206      //---------------------------------------------------------------
046400080206      //?Riepilogo indicatori.
046500080206      //---------------------------------------------------------------
046600090518      // - ordinamenti del subfile
046700090518      // 07    : Ordinamento per data
046800090601      // 08    : Ordinamento per provincia
046900090629      // 09    : Ordinamento per priorità causale
047000090421      // - visualizzazioni particolari
047100090629      // 10    : Emissione subfile per le sole attività da eseguire
047200090518      // 13    : Indica un'attività già eseguita
047300090518      // 15    : Manutenzione di un'attività
047400090518      // 16    : Visualizza un'attività
047500090518      // 17    : Identifica un'attività telefonica
047600090518      // 18    : Identifica un'attività di appuntamento
047700090518      // - Comuni
047800080207      // 28    : Emissione messaggio di errore a video
047900080207      // - C01/S01
048000080207      // 30    : Condiziona SFLDSP    (*not)
048100080207      // 31    : Condiziona SFLDSPCTL (*not)
048200080207      // 30+31 : Condiziona SFLCLR
048300080207      // 32    : Condiziona SFLNXTCHG in S01
048400080207      // 33    : Condiziona SFLEND
048500090518      // 50    : Errore: Posizione cursore
048600080207      // - D01
048700090318      // 50    : Errore: Commerciale CHI sono errato
048800090318      // 51    : Errore: Causale contatto errata
048900090318      // 52    : Errore: Data contatto da effettuare errata
049000090318      // 53    : Errore: Commerciale ...... errat8
049100090318      // 54    : Errore: Cliente potenziale errato
049200090318      // 55    : Errore: Data contatti effettuati.dal... errata
049300090318      // 56    : Errore: Data contatti effettuati..al... errata
049400090318      // 57    : Errore: Filiale appart. .. errata
049500080207      // - Comuni
049600080207      // 99    : Generico di Errore
049700080206      //---------------------------------------------------------------
049800080206
049900080206      //---------------------------------------------------------------
050000080206      //?M A I N - L I N E
050100080206      //---------------------------------------------------------------
050200080206
050300080206     c     *Entry        plist
050400080206     c                   parm                    KPJBA
050500080206
050600080206      /free
050700080206
050800080206       // Operazioni iniziali
050900080206       exsr RoutInz;
051000090601
051100090601       // Operazioni iniziali controllo se video accetta 132 caratteri
051200090601       if not %open(trmk18d);
051300090601           dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
051400090609           if dsp_mod = 1 ;
051500090609              open trmk18d;
051600090609           else ;
051700090609       // Vado a fine in quanto il video non supporta i 132
051800090609              eval $fine = *on ;
051900090609           endif ;
052000090601       endif;
052100080206
052200080206       // Gestione video
052300080206       DOW $Fine = *off;
052400080206         select;
052500080206           when $Video = 'D1';
052600080206             exsr GesD01;
052700080206           when $Video = 'S1';
052800080206             exsr GesS01;
052900090428           other;
053000080206             $Fine = *on;
053100080206         endsl;
053200080206       ENDDO;
053300080206
053400080206       // Operazioni finali
053500080206       exsr RoutEnd;
053600080206
053700080206       //--------------------------------------------------------------
053800080206       //?Operazioni iniziali.
053900080206       //--------------------------------------------------------------
054000080206       BEGSR RoutInz;
054100100427
054200100427         exec sql  set option  dynusrprf = *owner,
054300100427                               closqlcsr = *endmod;
054400080206
054500080206         // Impostazione campi "fissi"
054600080208         T01pgm = SDSpgm;
054700080206
054800080206         // Reperimento dati job
054900080206         exsr DatiJob;
055000080206
055100100209         // Controllo se utente autorizzato alla funzione
055200100209         reset TNTAA1DS;
055300100209         ITAA1caut = '§utegtc';
055400100209         kpjbu     = TNTAA1DS;
055500100209         tntaa1c (kpjba);
055600100209         TNTAA1DS = kpjbu;
055700100209
055800100209         IF  OTAA1fabi = 'N';
055900080206           $ErrGrave   = *on;
056000080206           errMessage  = *on;
056100080206           errGenerico = *on;
056200080206           V1Dmsg = $Msg(01);
056300080206           leavesr;
056400080206         endif;
056500100802
056600100802         wabi = OTAA1cabi;
056700100209
056800100209         clear TRUL31DS;
056900100209         I31abi = OTAA1cabi;
057000100209         I31cdi = DUTdis;
057100100209         I31car = DUTare;
057200100209         I31cpo = DUTpou;
057300100209         callp trul31r (kpjba : trul31ds);
057400100209         IF  O31pog <= *zeros;
057500100209           $ErrGrave   = *on;
057600100209           errMessage  = *on;
057700100209           errGenerico = *on;
057800100209           V1Dmsg = $Msg(01);
057900100209           leavesr;
058000100209         ENDIF;
058100081020
058200090714          // Reperisco data corrente
058300090714             data_iso = (%date());
058400090714             datacor = %dec(data_iso);
058500090714
058600091130          // Recupero i tipi attività che si possono gestire in questo PGM
058700091130         xx=0                     ;
058800091130         setll ('TTA') tntbe01l   ;
058900091130         READE ('TTA') tntbe01l   ;
059000091130
059100091130 2       dow not %eof(tntbe01l)   ;
059200091130 3       if tbeatb=' '    ;
059300091130           dtta=tbeuni    ;
059400091130           If §ttaleg = 'S' ;
059500091130           xx=xx+1        ;
059600091130           $tta(xx)=tbeke1 ;
059700091130           endif ;
059800091130 3       endif ;
059900091130
060000091130         READE ('TTA') tntbe01l   ;
060100091130 2       enddo    ;
060200160406
060300160406         // -?Caricamento dati delle Causali Contatto Marketing?
060400160406         clear  xx;
060500160406         clear  sk_CCO_k;
060600160406         clear  sk_CCO_d;
060700160406         clear  ds_CCO;
060800160406         setll  ('CCO')  TNTBE000;
060900160406         reade  ('CCO')  TNTBE000;
061000160406
061100160406         DoW  Not  %eof(TNTBE01L);
061200160406           if  TBEatb = *blanks;
061300160406             dCCO = TBEuni;
061400160406             ds_CCO.§CCOdes = §CCOdes;
061500160406             ds_CCO.§CCOord = §CCOord;
061600160406             ds_CCO.§CCOopz = §CCOopz;
061700160406             xx += 1;
061800160406             sk_CCO_k(xx) = TBEke1;
061900160406             sk_CCO_d(xx) = ds_CCO;
062000160406           endif;
062100160406           reade  ('CCO')  TNTBE000;
062200160406         EndDo;
062300091130
062400110803          // Verifico se sono utente di sede
062500110803         Sede = (Dutpou = 046);
062600100210         //?Carico sk con commerciali unificanti gestiti dall'utente
062700100210         //?e sk con figli degli unificanti caricati
062800100209         exsr CarAge;
062900111227
063000111227         //?Data del giorno
063100111227         wOggi = %dec(%date());
063200091130
063300080206       ENDSR;
063400080206
063500080206       //--------------------------------------------------------------
063600080206       //?Reperimento Dati del job (Utente/Operativi).
063700080206       //--------------------------------------------------------------
063800080206       BEGSR DatiJob;
063900080206
064000080206         in(E) §AzUte;
064100080206         if NOT %error;
064200080206           in(E) §DatiUte;
064300080206         endif;
064400080206         if %error or RSut = *blanks;
064500080206           clear TIBS34ds;
064600080206           tibs34r(tibs34ds);
064700080206           in §AzUte;
064800080206           in §DatiUte;
064900080206         endif;
065000080206
065100080206       ENDSR;
065200100209
065300100209       //--------------------------------------------------------------
065400100318       //?Carico sk agenti unificanti.
065500100209       //--------------------------------------------------------------
065600100209       BEGSR CarAge;
065700100209
065800100210         //?Carico gli agenti unificanti delle filiali gestite dall'utente
065900100209         $End = *off;
066000100210         clear $AGEU;
066100100209
066200100209         //?Imposto la stringa per SQL
066300130808         wSQL = 'select CMMcod +
066400130808                 from AZCMM00F +
066500130808                 where substr(digits(CMMuni), 1, 3) in (';
066600100209
066700100209         yy = 0;
066800100209         xx = 1;
066900100209         FOR xx by 1 to %elem(pog);
067000100209           IF pog(xx) <> *zeros and pog(xx) <> *blanks;
067100100209             IF yy > 0;
067200100209               wSQL += ', ';
067300100209             ELSE;
067400100209               yy = 1;
067500100209             ENDIF;
067600100209             wSQL += '''';
067700100209             wSQL += pog(xx);
067800100209             wSQL += '''';
067900100209           ENDIF;
068000100209         ENDFOR;
068100100209
068200130808         wSQL += ') and CMMcod = CMMuni +
068300130808                    and CMMpar = '' ''';
068400100209
068500130808         wSQL += ' order by CMMcod +
068600100209                   for fetch only';
068700100209
068800100209         //?Dichiarazione cursore
068900100209         exec sql
069000100210           prepare A1   from :wSQL;
069100100209         exec sql
069200100210           declare AGEU cursor for A1;
069300100209
069400100209         //?Apertura del cursore
069500100209         exec sql
069600100210           open AGEU;
069700100209
069800100209         IF sqlcode < 0;
069900100209           $End = *on;
070000100209         ENDIF;
070100100209
070200100209         yy = 0;
070300100209         DOW not $End;
070400100209           exec sql
070500130808             fetch next from AGEU into :CMMcod;
070600100209           IF sqlcod = 100 or sqlcod < 0;
070700100209             $End = *on;
070800100209             leave;
070900100209           ENDIF;
071000100209
071100130808           yy += 1;
071200130808           $AGEU(yy) = %editc( CMMcod : 'X' );
071300100209
071400100209         ENDDO;
071500100209
071600100209         exec sql
071700100210           close AGEU;
071800100209
071900100209       ENDSR;
072000080206
072100080206       //--------------------------------------------------------------
072200080206       //?Gestione videata D01
072300080206       //--------------------------------------------------------------
072400080206       BEGSR GesD01;
072500080206
072600080206         // Inizializzazione videata
072700080206         if $InzD01 = *on;
072800080206           exsr InzD01;
072900080206           $InzD01 = *off;
073000080214         else;
073100080225           if  errGenerico = *off;
073200090318           // per adesso non prevedo nulla
073300080225           endif;
073400080206         endif;
073500080206
073600080206         // Emissione testata
073700100427         //if  errGenerico = *off;
073800090317           write MK18T01;
073900100427         //endif;
074000080206
074100080206         // Emissione videata
074200090318         exfmt MK18D01;
074300080206         errMessage   = *off;
074400080206         errGenerico  = *off;
074500080206         clear V1Dmsg;
074600080207
074700080206         select;
074800080206         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
074900080206           when  $ErrGrave = *on;
075000080206             $Fine  = *on;
075100080206         // F3=Fine
075200080208           when dsp_aid = c_F03;
075300080206             exsr F03D01;
075400141111
075500090317         // F07=Prima telefonata Potenziale
075600090317           when dsp_aid = c_F07;
075700090317             exsr F07D01;
075800081021
075900091019         // F04=Prima telefonata Cliente
076000091019           when dsp_aid = c_F04;
076100091019             exsr F04D01;
076200100430
076300100430         //?F11 = Agenda
076400100430           when dsp_aid = c_F11;
076500100430             exsr F11D01;
076600091019
076700080206         // Enter
076800080206           other;
076900080206             exsr CtrD01;
077000080206             if errGenerico;
077100080206               leavesr;
077200080206             endif;
077300090428             $Video = 'S1';
077400080206             $InzS01 = *on;
077500100319             clear sav_C01loc;
077600100617             clear sav_C01cap;
077700100319             clear sav_C01cca;
077800100617             clear sav_C01cmm;
077900100503
078000110218         //?Imposto ordinamento per causale
078100110218            // ORD_Xdata = *on ;
078200110218             ORD_Xcausale = *on ;
078300080206         endsl;
078400080206
078500080206       ENDSR;
078600080206
078700080206       //--------------------------------------------------------------
078800080206       //?Inizializzazione videata D01
078900080206       //--------------------------------------------------------------
079000080206       BEGSR InzD01;
079100080206
079200090506           v01asc = *date ;
079300080206
079400080206       ENDSR;
079500080206
079600080206       //--------------------------------------------------------------
079700080206       //?Gestione tasto funzionale F3 da videata D01
079800080206       //?F3=Fine
079900080206       //--------------------------------------------------------------
080000080206       BEGSR F03D01;
080100080206
080200080207         // Chiusura del programma
080300080206         $Fine = *on;
080400080206
080500080206
080600080206       ENDSR;
080700080206
080800080206       //--------------------------------------------------------------
080900090609       //?Gestione tasto funzionale F07 da videata D01  richiamo il pgm
081000090609       //?che gestisce i contatti
081100090317       //?F07=Prima telefonata potenziale
081200080206       //--------------------------------------------------------------
081300090317       BEGSR F07D01;
081400100408
081500100408         // Richiamo il pgm interrogazione potenziali
081600090609             clear trmk19ds;
081700090609             imk19cmt = 'S';
081800090609             imk19fle = 'P' ;
081900090610             imk19tco = 'T' ;
082000090611                     if v01cmg <> *blanks ;
082100090611                        imk19com = %dec(v01cmg :7:0);
082200090611                        imk19comd = v01cmgd ;
082300090611                     endif ;
082400100408
082500100408             clear trmk01ds;
082600100408             mk01k11='1'   ;
082700100408             mk01imm='S'   ;
082800100408             mk01K10='B'   ;
082900100408             kpjbu=trmk01ds;
083000120201
083100120201             IF sede;
083200120201               clear kpjbu;
083300120201               Trmk01r ( kpjba );
083400120201             ELSE;
083500100408             Trmk01r ( kpjba : trmk19ds );
083600120201             ENDIF;
083700120201
083800100408             trmk01ds=kpjbu  ;
083900100408
084000100408         // Se restituito errore => lo si visualizza
084100100408         if  mk01err = 'E';
084200100408           errMessage  = *on;
084300100408           errGenerico = *on;
084400100408           V1Dmsg = mk01m10  ;
084500100408           V01cpo = %editc(mk01cpo : 'X');
084600100408           leavesr;
084700100408         endif;
084800100408
084900090407
085000080206       ENDSR;
085100080206
085200091019       //--------------------------------------------------------------
085300091019       //?Gestione tasto funzionale F04 da videata D01  richiamo il pgm
085400091019       //?che gestisce i contatti
085500141201       //?F04=Interrogazione clienti.
085600091019       //--------------------------------------------------------------
085700091019       BEGSR F04D01;
085800100802
085900100802       // Richiamo il pgm interrogazione clienti
086000150112         clear  TRKC20ds;
086100150112         iKC20ric = 'A';
086200150112         iKC20abi = wABI;
086300141201
086400141201         clear  kpjbu;
086500141201
086600150112         trkc20r ( kpjba : TRKC20ds );
086700091019
086800091019       ENDSR;
086900100430
087000100430       //--------------------------------------------------------------
087100100430       //?Gestione tasto funzionale F11 da videata D01
087200100430       //?F11=Agenda
087300100430       //--------------------------------------------------------------
087400100430       BEGSR F11D01;
087500100430
087600100430         clear TRMK82DS;
087700100430         IMK82rich = 'A';
087800100430
087900100524         SELECT;
088000100524         WHEN  V01ccm <> *blanks and V01ccm <> *zeros and
088100100524               %check(digits:V01ccm) = 0;
088200100430           IMK82cage = %dec(V01ccm:7:0);
088300100524         WHEN  V01cmg <> *blanks and V01cmg <> *zeros and
088400100430             %check(digits:V01cmg) = 0;
088500100430           IMK82cage = %dec(V01cmg:7:0);
088600100524         ENDSL;
088700100430
088800100430         kpjbu = TRMK82DS;
088900100430
089000100430         trmk82r (kpjba);
089100100430
089200100430       ENDSR;
089300091019
089400080206       //--------------------------------------------------------------
089500080206       //?Controllo videata D01
089600080206       //--------------------------------------------------------------
089700080206       BEGSR CtrD01;
089800081017
089900100617         %subst(IndDspF : 50) = *off;
090000100524
090100100524         clear $AGE;
090200100524         clear wage;
090300100309
090400100309         // Controllo codice commerciale (chi sono ) "?"
090500100316           clear v01cmgd;
090600100309           If    %scan('?' : v01cmg) > 0  ;
090700100309             errGenerico = *on;
090800100309             PosCurCmg   = *on;
090900130808             clear  TRMK43ds;
091000130808             iMK43fil  = DUTpou;
091100130808             TRMK43R (kpjba : TRMK43ds);
091200130808             if  oMK43err = *off  and  oMK43fxx = *blank;
091300130808               v01cmg  = %editc( oMK43cmm : 'X' );
091400130808               v01cmgd = oMK43des;
091500130808             endif;
091600100309             leavesr;
091700100309           endif;
091800100309
091900100309         // Controllo codice commerciale (chi sono )
092000100309           If    v01cmg <> *blanks and v01cmg <> *zeros;
092100100309
092200100309             IF  %check(digits:V01cmg) > 0;
092300100309               ErrMessage  = *on;
092400100309               ErrGenerico = *on;
092500100309               PosCurCMG   = *on;
092600100309               V1Dmsg      = $Msg(03);
092700100309               leavesr;
092800100309             ENDIF;
092900100309
093000100309           //?utente abilitato al commerciale
093100100309             clear TNTAA1DS;
093200100309             ITAA1caut = '§utegtc';
093300100309             ITAA1cmm  = %dec(V01cmg:7:0);
093400100309             kpjbu     = tntaa1ds;
093500100309             tntaa1c (kpjba);
093600100309             TNTAA1DS = kpjbu;
093700100309             IF  OTAA1fabi = 'N';
093800100309               ErrMessage  = *on;
093900100309               ErrGenerico = *on;
094000100309               PosCurCMG   = *on;
094100100309               V1Dmsg      = $Msg(03);
094200100309               leavesr;
094300100309             ENDIF;
094400100309
094500130808             CMMcod = %int(V01cmg);
094600130808             chain  (CMMcod)  AZCMM000;
094700130808             if  NOT  %found(AZCMM01L);
094800100309               errMessage  = *on;
094900100309               errGenerico = *on;
095000100309               PosCurCmg   = *on;
095100100309               V1Dmsg = $Msg(03);
095200100309               leavesr;
095300100309             endif;
095400100309
095500130808             V01cmgd = CMMdes;
095600110614
095700110614           //?Commerciale senza particolarità
095800130808             IF  CMMpar <> *blanks;
095900110614               ErrMessage  = *on;
096000110614               ErrGenerico = *on;
096100110614               PosCurCMG   = *on;
096200110614               V1Dmsg      = $Msg(03);
096300110614               leavesr;
096400110614             ENDIF;
096500111228
096600111228       //?Commerciale "valido" data inizio e fine attività
096700130808         IF  CMMdtIni > wOggi  or  CMMdtFin < wOggi;
096800111228           ErrMessage = *on;
096900111228           ErrGenerico = *on;
097000111228           PosCurCmg   = *on;
097100111228           V1Dmsg = $Msg(03);
097200111228           leavesr;
097300111228         ENDIF;
097400111228
097500100309           endif;
097600100309
097700081017         // Controllo selezioni inserite
097800090317         // se inserite le date Da effettuare al e le date Effettuate dal al
097900090317         // do errore
098000100524
098100100524         // Controllo commerciale che esegue contatto
098200100524         clear v01ccmd ;
098300100524         If    %scan('?' : v01Ccm) > 0  ;
098400100524           errGenerico = *on;
098500100524           PosCurCcm   = *on;
098600130808           clear  TRMK43ds;
098700130808           iMK43fil  = DUTpou;
098800130808           TRMK43R (kpjba : TRMK43ds);
098900130808           if  oMK43err = *off  and  oMK43fxx = *blank;
099000130808             v01ccm  = %editc( oMK43cmm : 'X' );
099100130808             v01ccmd = oMK43des;
099200130808           endif;
099300100524           leavesr;
099400100524         EndIf;
099500100524
099600100524         IF  V01ccm <> *blanks and v01ccm <> *zeros ;
099700100524
099800100524           if  %check(digits:V01ccm) > 0;
099900100524             ErrMessage  = *on;
100000100524             ErrGenerico = *on;
100100100524             PosCurCCM   = *on;
100200100524             V1Dmsg      = $Msg(03);
100300100524             leavesr;
100400100524           endif;
100500100524
100600100524           //?utente abilitato al commerciale
100700100524           clear TNTAA1DS;
100800100524           ITAA1caut = '§utegtc';
100900100524           ITAA1cmm  = %dec(V01ccm:7:0);
101000100524           kpjbu     = tntaa1ds;
101100100524           tntaa1c (kpjba);
101200100524           TNTAA1DS = kpjbu;
101300100524           if  OTAA1fabi = 'N';
101400100524             ErrMessage  = *on;
101500100524             ErrGenerico = *on;
101600100524             PosCurCCM   = *on;
101700100524             V1Dmsg      = $Msg(03);
101800100524             leavesr;
101900100524           endif;
102000100524
102100130808           CMMcod = %int(V01ccm);
102200130808           chain  (CMMcod)  AZCMM000;
102300130808           if  NOT  %found(AZCMM01L);
102400100524             errMessage  = *on;
102500100524             errGenerico = *on;
102600100524             PosCurCcm   = *on;
102700100524             V1Dmsg = $Msg(03);
102800100524             leavesr;
102900100524           endif;
103000100524
103100130808           V01ccmd = CMMdes;
103200110614
103300110614           //?Commerciale senza particolarità
103400130808           IF  CMMpar <> *blanks;
103500110614             ErrMessage  = *on;
103600110614             ErrGenerico = *on;
103700110614             PosCurCCM   = *on;
103800110614             V1Dmsg      = $Msg(03);
103900110614             leavesr;
104000110614           ENDIF;
104100100524
104200100524           //?commerciale è mio come Unificante
104300100524           $Uni = *off;
104400100524           if  %lookup(V01ccm:$AGEU) > 0 and
104500100524               %lookup(%subst(V01ccm:1:3):pog) > 0;
104600100524             $Uni = *on;
104700100524             //?Carico sk con tutti gli agenti abilitati
104800100524             wage = V01ccm;
104900100524             exsr CarAgeA;
105000100524           endif;
105100100524           //?commerciale è mio ma non Unificante
105200100524           if not $Uni;
105300100524             //?Carico sk con tutti i fratelli
105400130808             wage = %editc(CMMuni:'X');
105500100524             exsr CarAgeA;
105600100524           endif;
105700100524
105800100524         ENDIF;
105900100524
106000100524
106100100524         //?Almeno uno tra commerciale, cliente, potenziale e filiale
106200100524         //?deve essere inserito
106300100524           IF  (V01ccm = *blanks or V01ccm = *zeros) and
106400100524               (V01cp1 = *blanks or V01cp1 = *zeros) and
106500100524                V01cp2 = *zeros and
106600100524               (V01ksc = *blanks or V01ksc = *zeros) and
106700141125                V01flt = *zeros;
106800141125               //-(V01ncm = *blanks or V01ncm = *zeros);    ?NO: solo NCM appesantisce troppo!?
106900100524             ErrMessage  = *on;
107000100524             ErrGenerico = *on;
107100100524             PosCurCCM   = *on;
107200100524             V1Dmsg      = $Msg(14);
107300100524             leavesr;
107400100524           ENDIF;
107500100524
107600100524
107700100524
107800100524         // controllo data limite selezione contatti da fare
107900100524         clear w01asc;
108000100524         If    V01asc <> 0;
108100100524           clear wlbdat;
108200100524           g02dat = v01asc;
108300100524           xsrda8(wlbdat);
108400100524           if g02err = '1';
108500100524             errMessage  = *on;
108600100524             errGenerico = *on;
108700100524             PosCurAsc   = *on;
108800100524             V1Dmsg = $Msg(04);
108900100524             leavesr;
109000100524           endif;
109100100524
109200100524           v01asc = g02dat;
109300100524           w01asc = g02inv;
109400100524         endif;
109500100524
109600100524
109700100524         //?Se NON inserite le date da efettuare ed effettuate
109800100524         //?errore
109900100524           IF  V01asc = *zero and V01eda = *zeros and V01eal = *zeros;
110000100524             ErrMessage  = *on;
110100100524             ErrGenerico = *on;
110200100524             PosCurAsc   = *on;
110300100524             V1Dmsg      = $Msg(02);
110400100524             leavesr;
110500100524           ENDIF;
110600100524
110700100524           If      V01ASC <> *zero  and (V01EDA <> 0 or V01EAL <> 0);
110800100524             errMessage  = *on;
110900100524             errGenerico = *on;
111000100524             PosCurAsc   = *on;
111100100524             V1Dmsg      = $Msg(02);
111200100524             leavesr;
111300100524           Endif ;
111400100524
111500100524
111600100524         // - controllo / ricerca Causale contatto
111700100524         If    %scan('?' : V01cac) > *zero;
111800100524           errGenerico = *on;
111900100524           PosCurCac   = *on;
112000100524           clear  dCCO;
112100100524           clear  Tntb74ds;
112200100628           //iccouti = 'S' ;
112300100628           iccouti = 'X' ;
112400100524           TNTB74R  (kpjba : tntb74ds);
112500121128           if  occoerr = *off   ;
112600100524             dcco = occouni ;
112700100524             V01cac = Occoke1;
112800100524             V01cacd = §ccodes ;
112900100524             errGenerico = *on;
113000100524             leavesr;
113100100524           else;
113200100524             errMessage  = *on;
113300100524             errGenerico = *on;
113400100524             V1Dmsg = T02msg;
113500100524             leavesr;
113600100524           endif;
113700100524         endif;
113800100524
113900100524         If    V01cac <> *blanks ;
114000100524           clear  dCCO;
114100100524           clear  TIBS02ds;
114200100524           T02mod = 'C';
114300100524           T02cod = 'CCO';
114400100524           T02ke1 = v01cac ;
114500100524           T02sif = KNSIF;
114600100524           TNTBE_RicercaControllo  (kpjba : tibs02ds);
114700100524           if  T02err = *blanks;
114800100524             dcco = t02uni ;
114900100524             V01cacd = §ccodes ;
115000100524           else;
115100100524             errMessage  = *on;
115200100524             errGenerico = *on;
115300100524             PosCurCac   = *on;
115400100524             V1Dmsg = T02msg;
115500100524             leavesr;
115600100524           endif;
115700100524         else ;
115800100524           clear v01cacd ;
115900100524         endif;
116000100524
116100100524
116200100524         // Controllo codice potenziale
116300100524         clear cpo_scelto;
116400100524         If    %scan('?' : v01Cp1) > 0  ;
116500100524           errGenerico = *on;
116600100524           PosCurCp1   = *on;
116700100524           clear trmk01ds;
116800100524           mk01k11='2'   ;
116900100524           kpjbu=trmk01ds;
117000100524           TRMK01R (kpjba) ;
117100100524           trmk01ds=kpjbu  ;
117200100524           if  mk01cpo>0;
117300100524             V01cpo = %editc(mk01cpo : 'X');
117400100524             chain     mk01cpo  TNCPO01l;
117500100524             if %found(tncpo01l) ;
117600100524             //  eval v01cpod = CpoRag ;
117700100524             endif ;
117800100524           else ;
117900100524             clear v01cp1;
118000100524           endif ;
118100100524         EndIf;
118200100524
118300100524         if  V01cp2 <> *zeros and V01cp1 = *blanks;
118400100524           V01cp1 = '000';
118500100524         endif;
118600100524
118700100524         //If    V01cp1 <>  *zeros and v01cp2 <> *zeros ;
118800100524         If    V01cp1 <>  *blanks and v01cp2 <> *zeros ;
118900100524
119000100524           CPOcpo = %int(V01cpo);
119100100524           chain     CPOcpo  TNCPO000;
119200100524
119300100524           // Codice potenziale NON trovato
119400100524           if  NOT  %found(TNCPO01L);
119500100524               errGenerico = *on;
119600100524               PosCurCp1   = *on;
119700100524               errMessage  = *on;
119800100524               V1Dmsg = $Msg(08);
119900100524             leavesr;
120000100524           endif;
120100100524
120200100524           cpo_scelto = %int(v01cpo) ;
120300100524
120400100524         endif;
120500100524
120600100524
120700100524         //?Se immesso potenziale e cliente
120800100524         //?errore di incongruenza dati
120900100524           IF  V01cp1 <> *zeros and v01cp1 <> *blanks and
121000100524               V01cp2 <> *zeros and
121100100524               V01ksc <> *zeros and V01ksc <> *blanks;
121200100524             ErrMessage  = *on;
121300100524             ErrGenerico = *on;
121400100524             //PosCurCP1   = *on;
121500100524             PosCurKSC   = *on;
121600100524             V1Dmsg      = $Msg(16);
121700100524             leavesr;
121800100524           ENDIF;
121900100524
122000100524
122100100524         //?Controllo cliente
122200100524         if  V01ksc = *zeros;
122300100524           clear V01ksc;
122400100524         endif;
122500100524         clear Ksc_scelto;
122600100524         clear V01dksc;
122700100524         IF  %scan('?' : V01ksc) > *zero;
122800100524           ErrGenerico = *on;
122900100524           PosCurKsc   = *on;
123000100802           //?Ricerca clienti
123100141201           clear  V01ksc;
123200150112           clear  TRKC20ds;
123300150112           //iKC20ric = *blank;
123400150112           iKC20abi = wABI;
123500141201           clear  kpjbu;
123600150112           trkc20r ( kpjba : TRKC20ds );
123700150112           if  oKC20fxx = *blank  and  oKC20err = *blank;
123800150112             V01ksc = %editc( KC20ksc : 'X' );
123900141201           endif;
124000141201         ENDIF;
124100100524
124200100524         //?Codice valido
124300100524         IF  V01ksc <> *zeros and V01ksc <> *blanks;
124400100524           //?utente abilitato al cliente
124500100524           clear TNTAA1DS;
124600100524           ITAA1caut = '§utegtc';
124700100524           ITAA1ksc  = %dec(V01ksc:7:0);
124800100524           kpjbu     = tntaa1ds;
124900100524           tntaa1c (kpjba);
125000100524           TNTAA1DS = kpjbu;
125100100524           if  OTAA1fabi = 'N';
125200100524             ErrMessage  = *on;
125300100524             ErrGenerico = *on;
125400100524             PosCurKsc   = *on;
125500100524             V1Dmsg      = $Msg(12);
125600100524             leavesr;
125700100524           endif;
125800100524
125900100524           clear tibs69ds;
126000100524           clear ACOrag;
126100100524           I69kac = %dec(V01ksc:7:0);
126200100524           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
126300100524           if  O69err <> *blanks;
126400100524             ErrMessage  = *on;
126500100524             ErrGenerico = *on;
126600100524             PosCurKsc   = *on;
126700100524             V1Dmsg      = $Msg(12);
126800100524           endif;
126900100524           V01dksc = ACOrag;
127000100524           if  ErrGenerico;
127100100524             leavesr;
127200100524           endif;
127300100524           Ksc_scelto = I69kac;
127400100524         ENDIF;
127500100524
127600090319
127700090319         // controllo data limite dal selezione contatti fatti
127800100524         clear w01eda;
127900100524         If    V01eda <> 0;
128000100524           clear wlbdat;
128100100524           g02dat = v01eda;
128200100524           xsrda8(wlbdat);
128300100524           if g02err = '1';
128400100524             errMessage  = *on;
128500100524             errGenerico = *on;
128600100524             PosCurEda   = *on;
128700100524             V1Dmsg = $Msg(04);
128800100524             leavesr;
128900100524           endif;
129000100524
129100100524           v01eda = g02dat;
129200100524           w01eda = g02inv;
129300090406
129400090406         endif;
129500090319
129600100524         // se data limite al è uguale a zero imposto la data del giorno
129700100524         If   v01eda <> 0 and  V01eal = 0 ;
129800100524           v01eal = *date;
129900100524         endif ;
130000090319
130100090319         // controllo data limite al selezione contatti fatti
130200100524         clear w01eal;
130300100524         If    V01eal <> 0;
130400100524           clear wlbdat;
130500100524           g02dat = v01eal;
130600100524           xsrda8(wlbdat);
130700100524           if g02err = '1';
130800100524             errMessage  = *on;
130900100524             errGenerico = *on;
131000100524             PosCurEal   = *on;
131100100524             V1Dmsg = $Msg(04);
131200100524             leavesr;
131300100524           endif;
131400100524
131500100524           v01eal = g02dat;
131600100524           w01eal = g02inv;
131700100524         endif;
131800090319
131900090319         // Controllo se data congruenti
132000100524         if  w01eda > w01eal;
132100100524           errMessage  = *on;
132200100524           errGenerico = *on;
132300100524           PosCurEal   = *on;
132400100524           V1Dmsg = $Msg(05);
132500100524           leavesr;
132600090319         endif;
132700090319
132800090319         // Controllo Filiale gestione telefonate
132900100524         IF  v01flt > 0 ;
133000100524           chain v01flt azorg01l;
133100100524           If not %found(azorg01l);
133200100524             errMessage  = *on;
133300100524             errGenerico = *on;
133400100524             PosCurFlt   = *on;
133500100524             V1Dmsg = $Msg(06);
133600100524             leavesr;
133700100524           endif;
133800100524           // verifico se filiale abilitata all'utente
133900100209           clear TNTAA1DS;
134000100209           ITAA1caut = '§utegtc';
134100100209           ITAA1fil  = V01flt;
134200100209           kpjbu     = tntaa1ds;
134300100209           tntaa1c (kpjba);
134400100209           TNTAA1DS = kpjbu;
134500100524           if  OTAA1fabi = 'N';
134600100524             errMessage  = *on;
134700100524             errGenerico = *on;
134800100524             PosCurFlt   = *on;
134900100524             V1Dmsg = $Msg(06);
135000100524             leavesr;
135100100524           endif;
135200100524         ENDIF;
135300141124
135400141125         // -?Ricerca/Controllo Campagna Commerciale?
135500141125         clear  V01ncmD;
135600141125
135700141125         // ·?Ricerca?
135800141125         If  %scan( '?' : V01ncm ) > *zero;
135900141125           clear  V01ncm;
136000150112           clear  TRKC02ds;
136100150112           iKC02ric = 'R';
136200150112           trkc02r ( kpjba : TRKC02ds );
136300150112           if  oKC02ncm <> *zero;
136400150112             V01ncm = %editc( oKC02ncm : '3' );
136500141125           endif;
136600141125           PosCurNCM   = *on;
136700141125           errGenerico = *on;
136800150112           if  oKC02err = *on;
136900141125             ErrMessage = *on;
137000150112             V1Dmsg = oKC02msg;
137100141125             leavesr;
137200141125           endif;
137300141125         EndIf;
137400141125
137500141125         // ·?Controllo?
137600141125         if  V01ncm = *blank;
137700141125           V01ncm = *zero;
137800141125         endif;
137900141125
138000141125         if  %check( Digits : %trim( V01ncm ) ) > *zero;
138100141125           errGenerico = *on;
138200141125           errMessage  = *on;
138300141125           PosCurNCM   = *on;
138400141125           V1Dmsg = $Msg(13);
138500141125           leavesr;
138600141125         endif;
138700141124
138800141125         if  V01ncm <> *blank  and  %int( %trim( V01ncm ) ) <> *zero;
138900141125           chain  ( %int( %trim( V01ncm ) ) )  TICMP000;
139000141125           if  Not %found(TICMP01L);
139100141125             errMessage  = *on;
139200141125             errGenerico = *on;
139300141125             PosCurNCM   = *on;
139400141125             V1Dmsg = $Msg(13);
139500141125             leavesr;
139600141125           endif;
139700141125           V01ncmD = CMPdes;
139800141125         endif;
139900141125
140000141125         if  V01ncm <> *blank;
140100141125           evalR V01ncm = %editc( %int( %trim( V01ncm ) ) : 'Z' );
140200141125         endif;
140300150219
140400150220         if  V01ncm <> *blank  and  %int( %trim( V01ncm ) ) <> *zero  and
140500150220             ( wOggi < CMPdic  or   wOggi > CMPdfc );
140600150219           errMessage  = *on;
140700150219           errGenerico = *on;
140800150219           PosCurNCM   = *on;
140900150219           V1Dmsg = $Msg(19);
141000150219           leavesr;
141100150219         endif;
141200080213
141300080206       ENDSR;
141400100318
141500100318       //--------------------------------------------------------------
141600100318       //?Carico sk agenti abilitati.
141700100318       //--------------------------------------------------------------
141800100318       BEGSR CarAgeA;
141900100318
142000100318         $End = *off;
142100100318       //?Carico per mio unificante
142200100318         IF  $Uni;
142300100318         //?Imposto la stringa per SQL
142400130808           wSQL = 'select CMMcod +
142500130808                   from AZCMM00F +
142600130808                   where CMMuni = ';
142700100318           wSQL += wage;
142800130808           wSQL += ' order by CMMcod for fetch only';
142900100318         ENDIF;
143000100318       //?Carico fratelli per mio commerciale
143100100318         IF  not $Uni;
143200100318         //?Imposto la stringa per SQL
143300130808           wSQL = 'select CMMcod +
143400130808                   from AZCMM00F +
143500130808                   where substr(digits(CMMcod), 1, 3) in (';
143600100318
143700100318           yy = 0;
143800100318           xx = 1;
143900100318           FOR xx by 1 to %elem(pog);
144000100318             IF pog(xx) <> *zeros and pog(xx) <> *blanks;
144100100318               IF yy > 0;
144200100318                 wSQL += ', ';
144300100318               ELSE;
144400100318                 yy = 1;
144500100318               ENDIF;
144600100318               wSQL += '''';
144700100318               wSQL += pog(xx);
144800100318               wSQL += '''';
144900100318             ENDIF;
145000100318           ENDFOR;
145100100318
145200130808           wSQL += ') and CMMuni = ' + wage +
145300130808                    ' and CMMpar = '' ''';
145400100318
145500130808           wSQL += ' order by CMMcod +
145600100318                     for fetch only';
145700100318         ENDIF;
145800100318
145900100318       //?Dichiarazione cursore
146000100318         exec sql
146100100318           prepare A2   from :wSQL;
146200100318         exec sql
146300100318           declare AGE cursor for A2;
146400100318       //?Apertura del cursore
146500100318         exec sql
146600100318           open AGE;
146700100318         IF sqlcode < 0;
146800100318           $End = *on;
146900100318         ENDIF;
147000100318
147100100318         xx = 0;
147200100318         DOW not $End;
147300100318           exec sql
147400130808             fetch next from AGE into :CMMcod;
147500100318           IF sqlcod = 100 or sqlcod < 0;
147600100318             $End = *on;
147700100318             leave;
147800100318           ENDIF;
147900100318
148000130808           xx += 1;
148100130808           $AGE(xx) = %editc( CMMcod : 'X' );
148200100318
148300100318         ENDDO;
148400100318
148500100318         exec sql
148600100318           close AGE;
148700100318
148800100318       ENDSR;
148900080206
149000090421       //--------------------------------------------------------------
149100080206       //?Gestione videata S01
149200080206       //--------------------------------------------------------------
149300080206       BEGSR GesS01;
149400090629
149500080207         // Inizializzazione videata
149600080207         if  $InzS01 = *on;
149700090629         // imposto l'ordinamento per priorità causale
149800100503          //   ORD_Xcausale = *on ;
149900080207            exsr InzS01;
150000080207            $InzS01  = *off;
150100080207         endif;
150200080207
150300080207         // Posizionamento cursore
150400080207         if  C01csr  > *zero;
150500080207           C01rcd = C01csr;
150600080207         else;
150700080207         // Se non è stato caricato nulla si riemette la videata D01
150800080207           C01rcd = 1;
150900080207           $Video = 'D1';
151000080207           errMessage  = *on;
151100080207           errGenerico = *on;
151200090319           PosCurCmg   = *on;
151300090319           V1Dmsg = $Msg(07);
151400080207           leavesr;
151500080207         endif;
151600080207
151700080207         // Emissione Testata e Piede con tasti funzionali abilitati
151800090319           write  MK18T01;
151900090319           write  MK18P01;
152000090429
152100090429        // emetto la scritta già eseguite in basae alle richieste
152200090429         if $sel = 'E' ;
152300090429            da_eseguire = *off;
152400090429         else ;
152500090429            da_eseguire = *on ;
152600090429         endif;
152700080207
152800080207         // Emissione videata
152900090319         exfmt  MK18C01;
153000080207         reset errMessage;
153100080207         reset errGenerico;
153200080207         clear V1Dmsg;
153300080207
153400080207         SELECT;
153500080207
153600080207         // - F3=Fine
153700080207           WHEN  dsp_aid = c_F03;
153800080207             exsr F03D01;
153900080207
154000080207         // - F12=Ritorno
154100080207           WHEN  dsp_aid = c_F12;
154200080207             exsr F12S01;
154300090319
154400090505         // - F07=ordinamento per data
154500090629           WHEN  dsp_aid = c_F17;
154600090625             exsr sr_orddata ;
154700080626
154800090609         // - F08=ordinamento per provincia
154900090629           WHEN  dsp_aid = c_F18;
155000090625             exsr sr_ordprov ;
155100090625
155200090910         // - F19=ordinamento per priorità causale
155300090629           WHEN  dsp_aid = c_F19;
155400090625             exsr sr_ordcaus ;
155500090319
155600080207         // - Roll-Up
155700080207           WHEN  dsp_aid = c_RollUp;
155800080207
155900080207         // Invio
156000080207           OTHER;
156100090928
156200090527         // verifico se esistono campi di parzializzazione
156300090527             exsr CtrC01 ;
156400090527             if  errGenerico = *on;
156500090527               leavesr;
156600090527             endif;
156700080207             exsr OpzS01;
156800080207             if  errGenerico = *on;
156900080207               leavesr;
157000080207             endif;
157100080207
157200080207         ENDSL;
157300080207
157400080207       ENDSR;
157500080207
157600080207       //--------------------------------------------------------------
157700080207       //?Inizializzazione videata S01
157800080207       //--------------------------------------------------------------
157900080207       BEGSR InzS01;
158000080207
158100090508       // Pulizia subfile
158200090508         exsr pulsfl1;
158300100319
158400100319       //?Parzializzazioni
158500100319         c01cca = sav_c01cca;
158600100319         c01loc = sav_c01loc;
158700100617         c01cap = sav_c01cap;
158800100617         c01cmm = sav_c01cmm;
158900100319         IF  C01loc = *blanks;
159000100319           $parz_X_loc = *off ;
159100100319         ENDIF;
159200100617         IF  C01cap = *blanks;
159300100617           $parz_X_cap = *off ;
159400100617         ENDIF;
159500100319         IF  C01cca = *blanks;
159600100319           $parz_X_cca = *off ;
159700100319         ENDIF;
159800100617         IF  C01cmm = *zeros;
159900100617           $parz_X_cmm = *off ;
160000100319         ENDIF;
160100090508
160200080207       // Caricamento dei dati da presentare nel sfl
160300080207         $EoF = *off;
160400080213         select;
160500100317
160600100317         // - Selezionati i contatti da eseguire
160700090427           when  v01asc > 0 ;
160800090320         //  Preparo la stringa sql
160900090515                 StringaSql = 'select * from tiatc00f where atcdco = 0 +
161000090515                 and atcdad <= ' +  %editc(w01asc:'X') ;
161100090323                 $Sel = ' ' ;
161200090323         // se richiesta la causale contatto seleziono per causale da contattare
161300090427           If v01cac <> *blank;
161400090924                 StringaSql = %trim(Stringasql) + ' and atccad = ' +
161500090924                 '''' + v01cac +'''';
161600090323             endif;
161700090320
161800100317         // - Selezionati i contatti eseguiti
161900090427           when  v01eal > 0 ;
162000090320         //  Preparo la stringa sql
162100090515                 StringaSql = 'select * from tiatc00f where atcdco >= ' +
162200090427                  %editc(w01eda:'X') +
162300090722                 ' and atcdco <= ' + %editc(w01eal:'X') + ' and atcdco > 0' ;
162400090323                 $Sel = 'E' ;
162500090323         // se richiesta la causale contatto seleziono per causale contatto
162600090427           If v01cac <> *blank;
162700100316                 StringaSql = %trim(Stringasql) + ' and atccac = ' +
162800100316                 '''' + v01cac + '''';
162900090323             endif;
163000090323           endsl;
163100100916
163200100916       //?se NON è stata richiesta una causale specifica elaboro SOLO
163300100916       //?i tipi attività che hanno un legame con i clienti
163400100916         IF  V01cac = *blanks;
163500100916           zz = 0;
163600100916           ww = 1;
163700100916           StringaSql = %trim(StringaSql) + ' and ATCtat in(';
163800100916           FOR ww by 1 to %elem($TTA);
163900100916             IF $TTA(ww) <> *blanks;
164000100916               IF zz > 0;
164100100916                 StringaSql += ', ';
164200100916               ELSE;
164300100916                 zz = 1;
164400100916               ENDIF;
164500100916               StringaSql += '''';
164600100916               StringaSql += $TTA(ww);
164700100916               StringaSql += '''';
164800100916             ENDIF;
164900100916           ENDFOR;
165000100916           StringaSql += ')';
165100100916         ENDIF;
165200100916
165300100916       //?E' stato richiesto un commerciale
165400100916         IF  V01ccm > *zeros;
165500100917           zz = 0;
165600100917           ww = 1;
165700100916         //?sto elaborando le attività da eseguire
165800100916         //?controllo con commerciale che deve eseguire l'attività
165900100916           IF  $Sel = *blanks;
166000100916             StringaSql = %trim(StringaSql) + ' and ATCcmm in(';
166100100916             FOR ww by 1 to %elem($AGE);
166200100916               IF $AGE(ww) <> *blanks;
166300100916                 IF zz > 0;
166400100916                   StringaSql += ', ';
166500100916                 ELSE;
166600100916                   zz = 1;
166700100916                 ENDIF;
166800100916                 //StringaSql += '''';
166900100916                 StringaSql += $AGE(ww);
167000100916                 //StringaSql += '''';
167100100916               ENDIF;
167200100916             ENDFOR;
167300100916             StringaSql += ')';
167400100916           ENDIF;
167500100916       //?sto elaborando le attività eseguite
167600100916       //?controllo con commerciale che ha eseguito l'attività
167700100916           IF  $Sel = 'E';
167800100916             StringaSql = %trim(StringaSql) + ' and ATCcco in(';
167900100916             FOR ww by 1 to %elem($AGE);
168000100916               IF $AGE(ww) <> *blanks;
168100100916                 IF zz > 0;
168200100916                   StringaSql += ', ';
168300100916                 ELSE;
168400100916                   zz = 1;
168500100916                 ENDIF;
168600100916                 //StringaSql += '''';
168700100916                 StringaSql += $AGE(ww);
168800100916                 //StringaSql += '''';
168900100916               ENDIF;
169000100916             ENDFOR;
169100100916             StringaSql += ')';
169200100916           ENDIF;
169300100916         ENDIF;
169400100917
169500100917       //?E' stata richiesta la filiale
169600100917         IF  V01flt <> 000;
169700100917         //?escludo le attività che non le appartengono
169800100917         //?il controllo è fatto con la filiale commerciale
169900100917         //?sto elaborando le attività da eseguire
170000100917         //?controllo con commerciale che deve eseguire l'attività
170100100917           IF  $Sel = *blanks;
170200100917             StringaSql = %trim(StringaSql) + ' and  ' +
170300100917            'substr(digits(ATCcmm), 1, 3) = ' + %editc(V01flt:'X');
170400100917           ENDIF;
170500100917       //?sto elaborando le attività eseguite
170600100917       //?controllo con commerciale che ha eseguito l'attività
170700100917           IF  $Sel = 'E';
170800100917             StringaSql = %trim(StringaSql) + ' and  ' +
170900100917            'substr(digits(ATCcco), 1, 3) = ' + %editc(V01flt:'X');
171000100917           ENDIF;
171100100917         ENDIF;
171200090323
171300100317         // è stato selezionato un potenziale all'interno di contatti
171400090929           If cpo_scelto  > *zeros  ;
171500090401                 StringaSql = %trim(Stringasql) + ' and  +
171600090929                  atccpo  = ' + %editc(cpo_scelto:'X');
171700090323           endif;
171800100121
171900100317         // è stato selezionato un cliente all'interno di contatti
172000100121           IF  Ksc_scelto  > *zeros  ;
172100100121                 StringaSql = %trim(Stringasql) + ' and  +
172200100121                  atcksc  = ' + %editc(Ksc_scelto:'X');
172300100121           ENDIF;
172400100210
172500090506
172600100317         // imposto l'ordinamento per data e ora da eseguire
172700100317           If $sel = ' ';
172800090515              stringasql = %trim(stringasql) + ' order by atcdad , atchda' ;
172900100317           endif;
173000090506
173100100317         // imposto l'ordinamento per data e ora eseguiti
173200100317           If $sel = 'E';
173300090515              stringasql = %trim(stringasql) + ' order by atcdco , atchco';
173400090506           endif;
173500090506
173600090506         stringasql = %trim(stringaSql) + ' for fetch only';
173700090323
173800090323
173900090323         exec sql prepare S1 from :StringaSql;
174000090323
174100090323         exec sql declare C1 cursor for S1;
174200090323
174300090424         exec sql open C1 ;
174400090424
174500080213         exsr sr_ReadRec;
174600080207
174700090507         dow  $EoF   = *off ;
174800080207           exsr RollUpS01;
174900080207         enddo;
175000080207
175100090424         exec sql close C1 ;
175200090625
175300090625       //  Valorizzo il numero massimo di record caricati
175400090625         rrnlast = s01nrr ;
175500090424
175600090625       // Eseguo l'ordinamento in base all'ordinamento selezionato
175700090625
175800090625         If Ord_Xcausale = *on ;
175900090625            exsr sr_ordcaus;
176000090625         endif ;
176100090625
176200090625         If Ord_Xdata    = *on ;
176300090625            exsr sr_orddata;
176400090625         endif ;
176500090625
176600090625         If Ord_Xprovinc = *on ;
176700090625            exsr sr_ordprov;
176800090625         endif ;
176900090625
177000090625
177100080207       // Impaginazione subfile
177200080207       // -> forza l'impaginazione sul 1° rec. del subfile
177300080207         if S01nrr > *zero;
177400080207           C01rcd  = 1;
177500080207           C01csr  = 1;
177600080207         else;
177700080207           clear C01rcd;
177800080207         endif;
177900090508
178000080207       ENDSR;
178100080207
178200090508       //--------------------------------------------------------------
178300090508       //?Pulizia SFL01
178400090508       //--------------------------------------------------------------
178500090508       BEGSR Pulsfl1;
178600090508
178700090508       // Pulizia subfile
178800090508         SflDsp_N    = *on;
178900090508         SflDspCtl_N = *on;
179000090508         write  MK18C01;
179100090508         SflDspCtl_N = *off;
179200090508         SflEnd      = *off;
179300090508
179400090508         clear W_SflPag;
179500090508         clear C01rcd;
179600090508         clear C01csr;
179700090508         clear S01nrr;
179800090508         clear V1Dmsg;
179900090508         errMessage  = *off;
180000090508         errGenerico = *off;
180100100617         %subst(IndDspF : 50) = *off;
180200090508
180300090508       ENDSR ;
180400090508
180500080207       //--------------------------------------------------------------
180600090507       //?Caricamento di tutto il sfl
180700080207       //--------------------------------------------------------------
180800080207       BEGSR RollUpS01;
180900080207
181000090507         clear  s01nrr;
181100080207         SflNxtChg = *off;
181200080207
181300080207         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
181400090507         DOW  $EoF   = *off  and
181500100121              S01nrr < *hival;
181600080207
181700080207         // - Caricamento dati nel record del subfile
181800080213           exsr  RieS01;
181900090601           If $recok = *on ;
182000090601              S01nrr += 1;
182100090601              write MK18S01;
182200090601           endif ;
182300100121         // se superati 9000 record eof e fine
182400100121           If s01nrr = 9000 ;
182500100121              $eof = *on ;
182600100121              leave ;
182700100121           endif ;
182800080207
182900080207         // - Lettura record successivo nell'archivio
183000080213           exsr sr_ReadRec;
183100080207
183200080207         ENDDO;
183300080207
183400080207         // Visualizzazione del SFL (se ci sono dati)
183500080207         SflDsp_N = (S01nrr <= *zeros);
183600080207
183700080207         // Attivazione (eventuale) del SFLEND
183800090507         SflEnd  =  *on;
183900080207
184000080207         // Posizionamento cursore al 1° rcd della pagina
184100080207         if  S01nrr > *zero;
184200090507         //  wPag   = S01nrr / C_SflPag;
184300090507         //  wRig   = S01nrr - (C_SflPag * wPag);
184400090507         //  C01rcd = wPag * C_SflPag;
184500090507         //  if  wRig > *zeros;
184600090507         //    C01rcd = C01rcd + 1;
184700090507         //  else;
184800090507         //    C01rcd = C01rcd - C_SflPag + 1;
184900090507         //  endif;
185000090507             c01rcd = 1;
185100080207         else;
185200080207           clear  C01rcd;
185300080207         endif;
185400080207
185500080207         C01csr = C01rcd;
185600091014
185700080207
185800080207       ENDSR;
185900080213
186000080213       //--------------------------------------------------------------
186100090323       //?Lettura record successivo
186200080213       //--------------------------------------------------------------
186300080213       BEGSR sr_ReadRec;
186400090323
186500090323         $RecOK  = *off;
186600090323
186700090515         exec sql Fetch C1 into :tiatcds;
186800090323
186900090323         if  Sqlcod = 100 ;
187000090323              $Eof  = *on;
187100090323         endif;
187200090323         If  Sqlcod = 0 ;
187300141125              exsr  sr_SelezRec;
187400090323         endif;
187500090407         if  Sqlcod  < 0 ;
187600090407              $Eof  = *on;
187700090407         endif;
187800141124
187900080213       ENDSR;
188000141124
188100141124       //--------------------------------------------------------------
188200141124       //?Selezione singolo record.
188300141124       //--------------------------------------------------------------
188400141124       BEGSR  sr_SelezRec;
188500141124
188600141125         // -?Reperimento del Numero Campagna (visualizzato nel subfile)?
188700141125         //  ?per il cliente (o meglio: per il suo unificante)?
188800150112         clear  TRKC73ds;
188900141125         if  ATCksc > *zero;
189000150112           iKC73ksu = ATCksc;
189100150112           iKC73dat = %dec( %date() );
189200150112           trkc73r ( kpjba : TRKC73ds );
189300141125         endif;
189400141125
189500141125         // -?Filtro per Numero Campagna (se immesso):?
189600141125         //  ?Se NON tornato il numero campagna  o?
189700141125         //  ?Se numero campagna diverso => scarto?
189800141125         if  V01ncm   <> *blank  and
189900141125             V01ncm   <> *zero   and
190000150112             %lookup( %int( %trim( V01ncm ) ) : sk_oKC73nc ) = *zero;
190100141125           leavesr;
190200141125         endif;
190300080207
190400141124         $RecOK  = *on;
190500141124
190600141124       ENDSR;
190700080207
190800080207       //--------------------------------------------------------------
190900080207       //?Caricamento singolo record nel SubFile S01
191000080207       //--------------------------------------------------------------
191100080208       BEGSR RieS01;
191200090910
191300090323         clear  MK18S01;
191400080211
191500090323         // Reperimento dati del potenziale
191600100617         sh1cpo = atccpo ;
191700090323         // Ragione sociale
191800090611         chain     atccpo tncpo01l;
191900090323         If   %found(tncpo01l);
192000090323              s01rag = cporag ;
192100090527              s01loc = cpocit ;
192200090601              sh1loc = cpocit ;
192300090527              s01prv = cpoprv ;
192400100308              s01cic = cpoftr ;
192500100617              SH1cap = CPOcap;
192600090323         endif ;
192700100617
192800100617         // Reperimento commerciale trattativa (se trattativa)
192900100617         if  ATCnrv <> *zero;
193000100617           chain  ATCnrv  TIVIS000;
193100100617           if  %found(TIVIS05L);
193200100617             SH1cmm = VIScmm;
193300100617           endif;
193400100617         endif;
193500090323
193600091019         // verifico se c'è il codice del cliente valorizzato lo segnalo
193700091019         If atcksc <> 0 ;
193800091019              s01ast = '*' ;
193900100617              sh1ksc = atcksc ;
194000100308              clear s01cic  ;
194100100308              clear tibs69ds;
194200100503              I69kac = atcksc;
194300100617              I69kin = atcksc;
194400100308              I69kcp = atcksc ;
194500100308              TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
194600100308           IF  O69err = *blanks;
194700100503              s01rag  = acorag;
194800100617              s01cic  = clpclv;
194900100617              SH1cap  = INDcae;
195000100917              s01loc  = INDcit ;
195100100917              sh1loc  = INDcit ;
195200100617              if  ATCnrv = *zero;
195300100617                SH1cmm = CLPage;
195400100617              endif;
195500100308           Endif ;
195600091019         endif ;
195700100617
195800100617         // - Se NON reperito né il comm.le trattativa né il comm.le
195900100617         //   cliente (CNACO), prende il comm.le potenziale (CPOCMM)
196000100617         if  SH1cmm = *zero   and   %found(TNCPO01L);
196100100617           SH1cmm = CPOcmm;
196200100617         endif;
196300080211
196400080207
196500090406         // COMMERCIALE. In caso di telefonate ancora da eseguire prendo commerciale
196600090323         // che deve eseguire il contatto altrimenti il commerciale che ha eseguito
196700090323         // il contatto ... controllo il tipo Selezione
196800100318         // Contatti eseguiti
196900100318         if  $sel = 'E'  and atccco > 0;
197000090323         // commerciale
197100090515             s01ccm = atccco;
197200090323         // data e ora contatto e causale eseguito
197300090515         If  atcdco > 0 ;
197400090515             data_dmy = %date(atcdco:*iso) ;
197500090401             s01dco = %dec(data_dmy) ;
197600090515             sh1dco = atcdco ;
197700090515             s01hco = %dec(%subst(%EDITC(atchco:'X'):1:4):4:0);
197800090515             s01cac = atccac ;
197900090515             sH1cac = atccac ;
198000090424             sh1fatto = 'S' ;
198100090402         endif ;
198200090406         endif ;
198300090323
198400100318         // Contatti da eseguire
198500100318         if  $sel = ' '  and atccco = 0;
198600090515             s01ccm = atccmm;
198700090402         // data e ora contatto e causale da eseguire
198800090515         If  atcdad > 0 ;
198900090515             data_dmy = %date(atcdad:*iso) ;
199000090402             s01dco = %dec(data_dmy) ;
199100100429             if atcdad >= datacor ;
199200090713                Coldcosca = *on ;
199300090713                Sh1n19 = '1' ;
199400090713             else ;
199500090713                Coldcosca = *off;
199600090713                Sh1n19 = '0' ;
199700090713             endif ;
199800090515             sh1dco = atcdad ;
199900090515             s01hco = %dec(%subst(%EDITC(atchda:'X'):1:4):4:0);
200000090515             sh1cac = atccad ;
200100090515             s01cac = atccad ;
200200090713         // verifico se già scaduto rispetto ad oggi
200300090402         endif ;
200400090323         endif;
200500090323
200600091020         // se ci sono delle parzializzazioni vedo se devo scrivere il record nel subfile oppure
200700091020         // no
200800091020
200900091020       // Località
201000091020
201100091020             If  $parz_x_loc = *on and %scan(%trimr(c01loc):sh1loc) = 0 ;
201200091020                 $RecOK  = *off;
201300091020                 leavesr ;
201400091020             endif ;
201500100617
201600100617       // Cap
201700100617
201800100617             If  $parz_x_cap = *on and SH1cap <> C01cap;
201900100617                 $RecOK  = *off;
202000100617                 leavesr ;
202100100617             endif ;
202200091020
202300091020       // Causale
202400091020
202500091020             If  $parz_x_cca = *on and s01cac <> c01cca  ;
202600091020                 $RecOK  = *off;
202700091020                 leavesr ;
202800091020             endif ;
202900091020
203000091020       // Commerciale
203100091020
203200100617             If  $parz_x_cmm = *on and SH1cmm <> c01cmm  ;
203300091020                 $RecOK  = *off;
203400091020                 leavesr ;
203500091020             endif ;
203600091020
203700090323         // decodifica del commerciale
203800130808             chain  (S01ccm)  AZCMM000;
203900130808             if  %found(AZCMM01L);
204000130808               s01ccmd = CMMdes;
204100090323             endif;
204200090323
204300090402         // decodifica della causale
204400160406             yy = %lookup( SH1cac : sk_CCO_k );
204500160406             If  yy > *zero;
204600160406               ds_CCO  = sk_CCO_d(yy);
204700160406               S01cacD = ds_CCO.§CCOdes;
204800160406               SH1opz  = ds_CCO.§CCOopz;
204900160406               if  ds_CCO.§CCOord <> *blanks;
205000160406                 SH1ord  = ds_CCO.§CCOord;
205100160406               else;
205200160406                 SH1ord  = '99';
205300160406               endif;
205400160406             EndIf;
205500080207
205600090515             sh1tco = atctat ;
205700090515             sh1nco = atcatn ;
205800090515             sh1ncop= atcatnp;
205900100316             sh1nrv = atcnrv;
206000090428
206100090514         // tipo attività
206200090514
206300090811                 s01tco = atctat ;
206400090514
206500090629           // note contatto PRECEDENTE
206600091020                 cpncpo = cpocpo ;
206700091020                 cpnatn = atcatn ;
206800091020           // se l'attività non è stata eseguita prendo la nota precedente
206900091020                 if atcdco = 0    ;
207000091020                    cpnatnp  = atcatnp - 1;
207100091020                 else ;
207200091020                    cpnatnp  = atcatnp ;
207300091020                 endif ;
207400091020                 cpnpno = 1 ;
207500100520                 chain (CPNcpo : CPNatn : CPNatnp : CPNpno) TICPN05L ;
207600100520                 if   %found(TICPN05L);
207700091020                      s01no1 =  cpnnot  ;
207800091020                 endif ;
207900141125
208000141125         // -?Numero Campagna?
208100150112         if  oKC73nc2 = *zero;
208200150112           S01ncm = %editc( oKC73nc1 : 'Z' );
208300150102         else;
208400150102           S01ncm = '   +   ';
208500150102         endif;
208600090601
208700080207       ENDSR;
208800080207
208900090527       //--------------------------------------------------------------
209000090609       //?Controllo i dati della parzializzazione C01
209100090527       //--------------------------------------------------------------
209200090527       BEGSR CtrC01;
209300090527
209400090527       // Parzializzazione del subfile
209500090529
209600090529       // Località
209700100319          if  C01loc <> sav_C01loc;
209800090601             $parz_x_Loc = *on ;
209900090601             errGenerico  = *on;
210000090601             $InzS01  = *on ;
210100100319             sav_C01loc = C01loc;
210200090601          endif ;
210300100617
210400100617       // C.a.p.
210500100617          if  C01cap <> sav_C01cap;
210600100617             $parz_x_Cap = *on ;
210700100617             errGenerico  = *on;
210800100617             $InzS01  = *on ;
210900100617             sav_C01cap = C01cap;
211000100617          endif ;
211100090529
211200090601       // Causale
211300100319          if  C01cca <> sav_C01cca;
211400090601             $parz_x_Cca = *on ;
211500090601             errGenerico  = *on;
211600090601             $InzS01  = *on ;
211700100319             sav_C01cca = C01cca;
211800090601          endif ;
211900090601
212000100617       // Commerciale cliente
212100100617          if  C01cmm <> sav_C01cmm;
212200100617             $parz_x_Cmm = *on ;
212300090601             errGenerico  = *on;
212400090601             $InzS01  = *on ;
212500100617             sav_C01cmm = C01cmm;
212600090601          endif ;
212700090527
212800090527       ENDSR ;
212900080207       //--------------------------------------------------------------
213000080207       //?Gestione tasto funzionale F12 da videata S01
213100080207       //?F12=Ritorno
213200080207       //--------------------------------------------------------------
213300080207       BEGSR F12S01;
213400080207
213500080207         // Ritorno alla videata D01
213600080207         $Video = 'D1';
213700080207
213800080207       ENDSR;
213900080207
214000090406       //--------------------------------------------------------------
214100090505       //?Gestione tasto funzionale F07 da videata S01
214200090505       //?F07=Ordinamento per data
214300090406       //--------------------------------------------------------------
214400090625       BEGSR Sr_orddata;
214500090406
214600090508           Ord_Xdata = *on ;
214700090601           Ord_Xprovinc = *off ;
214800090625           Ord_Xcausale = *off ;
214900090508
215000090508       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
215100090508       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
215200090508       // il campo è usato per selezionare i records dando un ordine a quelli
215300090508       // selezionati davanti ai non selezionati.
215400090508         clear QLGSCB;
215500090508         clear QLGSCB00;
215600090508
215700090625       // 3 campi chiave x Data + ora contatto + ordinamento causale
215800090625         QLGNBRK = 3;
215900090508
216000090508       // imposto la posizione della Data     sul subfile e la lunghezza
216100090508       // l'ordinamento è su un campo numerico e deve essere
216200090508       // ascending
216300090508         QLGSP = 1 ;
216400090508         QLGSS = %SIZE(sh1dco);
216500090508         QLGDT = Numerico  ;
216600090508         QLGSO = Ascendente;
216700090508         QLGKL(1) = QLGSKL;
216800090508
216900090508       // imposto la posizione dell'Ora       sul subfile e la lunghezza
217000090508       // l'ordinamento è su un campo numerico e deve essere
217100090508       // ascending
217200090508         QLGSP = 1 + %size(sh1dco) ;
217300090508         QLGSS = %SIZE(s01hco);
217400090508         QLGDT = Numerico  ;
217500090508         QLGSO = Ascendente;
217600090508         QLGKL(2) = QLGSKL;
217700090508
217800090625       // imposto la posizione dell'Ordinamento causale sul subfile e la lunghezza
217900090625       // l'ordinamento è su un campo alfanumerico e deve essere
218000090625       // ascending
218100090625         QLGSP = 1 + %size(sh1dco) + %size(s01hco) + %size(s01prv);
218200090625         QLGSS = %SIZE(sh1ord);
218300090625         QLGDT = Carattere ;
218400090625         QLGSO = Ascendente;
218500090625         QLGKL(3) = QLGSKL;
218600090625
218700090508       // Load other sort parameters.
218800090508         QLGLB = 80 + 16 * MaxKey;
218900090508         QLGRL = %SIZE(sflrcd) - 1;
219000090508         QLGRT = 8;
219100090508         QLGOKL = 80;
219200090508         QLGLKE = 16;
219300090508         QLGLSS = 290;
219400090508
219500090508       // Initialize Sort I/O API fields.
219600090508         QLGRL00 = QLGRL;
219700090508         QLGRC00 = 1;
219800090508         clear QUSEI;
219900090508         QUSBPRV = %SIZE(QUSEC);
220000090508
220100090508       // First step - Initialize the sort routine.
220200090508         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
220300090508
220400090508       // Next step - Write records to I/O routine.
220500090508         QLGRT00 = Put;
220600090508         for xx = 1 to rrnlast;
220700090508          chain xx MK18S01;
220800090508
220900090508       // solo le righe con Selected = 'Y' sono riordinate,
221000090508       // quindi per fare un ordinamento di tutte le righe
221100090508       // metto 'Y' sempre.
221200090508          selected  = 'Y';
221300090508          clear QUSEI;
221400090508          QUSBPRV = %SIZE(QUSEC);
221500090508          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
221600090508         endfor;
221700090508
221800090508       // Next step - Signal end of input, clear subfile for reload.
221900090508         QLGRT00 = EndPut;
222000090508         clear QUSEI;
222100090508         QUSBPRV = %SIZE(QUSEC);
222200090508         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
222300090508       // pulizia SFL
222400090508         exsr pulsfl1;
222500090508
222600090508       // Final step - Write the records back to the subfile.
222700090508         QLGRT00 = Get;
222800090508         for xx = 1 to rrnlast;
222900090508          clear QUSEI;
223000090508          QUSBPRV = %SIZE(QUSEC);
223100090508          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
223200090508          s01nrr = xx;
223300090713          coldcosca = (sh1n19 = '1');
223400090508          write Mk18s01;
223500090508         endfor;
223600090508
223700090508         // Visualizzazione del SFL (se ci sono dati)
223800090508         SflDsp_N = (S01nrr <= *zeros);
223900090508
224000090508         // Attivazione (eventuale) del SFLEND
224100090508         SflEnd  =  *on;
224200090508
224300090508         // Posizionamento cursore al 1° rcd della pagina
224400090508         if  S01nrr > *zero;
224500090508             c01rcd = 1;
224600090508         else;
224700090508             clear  C01rcd;
224800090508         endif;
224900090508
225000090508         C01csr = C01rcd;
225100090508
225200090508
225300090406       ENDSR;
225400090406
225500090406       //--------------------------------------------------------------
225600090910       //?Gestione tasto funzionale F18 da videata S01
225700090910       //?F18=Ordinamento per provincia
225800090406       //--------------------------------------------------------------
225900090625       BEGSR sr_ordprov ;
226000090406
226100090625           Ord_Xdata = *off ;
226200090625           Ord_Xcausale = *off ;
226300090601           Ord_Xprovinc = *on  ;
226400090508
226500090508       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
226600090508       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
226700090508       // il campo è usato per selezionare i records dando un ordine a quelli
226800090508       // selezionati davanti ai non selezionati.
226900090508         clear QLGSCB;
227000090508         clear QLGSCB00;
227100090508
227200090625       // 4 campi chiave x Provincia + ordinamento causale + Data + ora contatto
227300090625         QLGNBRK = 4;
227400090508
227500090601       // imposto la posizione della Provincia  sul subfile e la lunghezza
227600090508       // l'ordinamento è su un campo numerico e deve essere
227700090508       // ascending
227800090508         QLGSP = 1 + %size(sh1dco)+ %size(s01hco) ;
227900090601         QLGSS = %SIZE(s01prv);
228000090508         QLGDT = Carattere ;
228100090508         QLGSO = Ascendente;
228200090508         QLGKL(1) = QLGSKL;
228300090508
228400090625
228500090625       // imposto la posizione dell'Ordinamento causale sul subfile e la lunghezza
228600090625       // l'ordinamento è su un campo alfanumerico e deve essere
228700090625       // ascending
228800090625         QLGSP = 1 + %size(sh1dco) + %size(s01hco) + %size(s01prv);
228900090625         QLGSS = %SIZE(sh1ord);
229000090625         QLGDT = Carattere ;
229100090625         QLGSO = Ascendente;
229200090625         QLGKL(2) = QLGSKL;
229300090625
229400090625       // imposto la posizione della Data     sul subfile e la lunghezza
229500090625       // l'ordinamento è su un campo numerico e deve essere
229600090625       // ascending
229700090625         QLGSP = 1 ;
229800090625         QLGSS = %SIZE(sh1dco);
229900090625         QLGDT = Numerico  ;
230000090625         QLGSO = Ascendente;
230100090625         QLGKL(3) = QLGSKL;
230200090625
230300090625       // imposto la posizione dell'Ora       sul subfile e la lunghezza
230400090625       // l'ordinamento è su un campo numerico e deve essere
230500090625       // ascending
230600090625         QLGSP = 1 + %size(sh1dco) ;
230700090625         QLGSS = %SIZE(s01hco);
230800090625         QLGDT = Numerico  ;
230900090625         QLGSO = Ascendente;
231000090625         QLGKL(4) = QLGSKL;
231100090625
231200090508       // Load other sort parameters.
231300090508         QLGLB = 80 + 16 * MaxKey;
231400090508         QLGRL = %SIZE(sflrcd) - 1;
231500090508         QLGRT = 8;
231600090508         QLGOKL = 80;
231700090508         QLGLKE = 16;
231800090508         QLGLSS = 290;
231900090508
232000090508       // Initialize Sort I/O API fields.
232100090508         QLGRL00 = QLGRL;
232200090508         QLGRC00 = 1;
232300090508         clear QUSEI;
232400090508         QUSBPRV = %SIZE(QUSEC);
232500090508
232600090508       // First step - Initialize the sort routine.
232700090508         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
232800090508
232900090508       // Next step - Write records to I/O routine.
233000090508         QLGRT00 = Put;
233100090508         for xx = 1 to rrnlast;
233200090508          chain xx MK18S01;
233300090508
233400090508       // solo le righe con Selected = 'Y' sono riordinate,
233500090508       // quindi per fare un ordinamento di tutte le righe
233600090508       // metto 'Y' sempre.
233700090508          selected  = 'Y';
233800090508          clear QUSEI;
233900090508          QUSBPRV = %SIZE(QUSEC);
234000090508          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
234100090508         endfor;
234200090508
234300090508       // Next step - Signal end of input, clear subfile for reload.
234400090508         QLGRT00 = EndPut;
234500090508         clear QUSEI;
234600090508         QUSBPRV = %SIZE(QUSEC);
234700090508         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
234800090508       // pulizia SFL
234900090508         exsr pulsfl1;
235000090508
235100090508       // Final step - Write the records back to the subfile.
235200090508         QLGRT00 = Get;
235300090508         for xx = 1 to rrnlast;
235400090508          clear QUSEI;
235500090508          QUSBPRV = %SIZE(QUSEC);
235600090508          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
235700090508          s01nrr = xx;
235800090713          coldcosca = (sh1n19 = '1');
235900090508          write Mk18s01;
236000090508         endfor;
236100090508
236200090508         // Visualizzazione del SFL (se ci sono dati)
236300090508         SflDsp_N = (S01nrr <= *zeros);
236400090508
236500090508         // Attivazione (eventuale) del SFLEND
236600090508         SflEnd  =  *on;
236700090508
236800090508         // Posizionamento cursore al 1° rcd della pagina
236900090508         if  S01nrr > *zero;
237000090508             c01rcd = 1;
237100090508         else;
237200090508             clear  C01rcd;
237300090508         endif;
237400090508
237500090508         C01csr = C01rcd;
237600090406
237700090406       ENDSR;
237800090625
237900090625       //--------------------------------------------------------------
238000090910       //?Gestione tasto funzionale F19 da videata S01
238100090910       //?F19=Ordinamento per ordinamento causale
238200090625       //--------------------------------------------------------------
238300090625       BEGSR sr_ordcaus ;
238400090625
238500090625           Ord_Xdata = *off ;
238600090625           Ord_Xcausale = *on  ;
238700090625           Ord_Xprovinc = *off ;
238800090625
238900090625       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
239000090625       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
239100090625       // il campo è usato per selezionare i records dando un ordine a quelli
239200090625       // selezionati davanti ai non selezionati.
239300090625         clear QLGSCB;
239400090625         clear QLGSCB00;
239500090625
239600090625       // 3 campi chiave x Ordinamento causale + data contatto + ora contatto
239700090625         QLGNBRK = 3;
239800090625
239900090625       // imposto la posizione dell'Ordinamento causale sul subfile e la lunghezza
240000090625       // l'ordinamento è su un campo numerico e deve essere
240100090625       // ascending
240200090625         QLGSP = 1 + %size(sh1dco)+ %size(s01hco) + %size(s01prv) ;
240300090625         QLGSS = %SIZE(sh1ord);
240400090625         QLGDT = Carattere ;
240500090625         QLGSO = Ascendente;
240600090625         QLGKL(1) = QLGSKL;
240700090625
240800090625       // imposto la posizione della Data     sul subfile e la lunghezza
240900090625       // l'ordinamento è su un campo numerico e deve essere
241000090625       // ascending
241100090625         QLGSP = 1 ;
241200090625         QLGSS = %SIZE(sh1dco);
241300090625         QLGDT = Numerico  ;
241400090625         QLGSO = Ascendente;
241500090625         QLGKL(2) = QLGSKL;
241600090625
241700090625       // imposto la posizione dell'Ora       sul subfile e la lunghezza
241800090625       // l'ordinamento è su un campo numerico e deve essere
241900090625       // ascending
242000090625         QLGSP = 1 + %size(sh1dco) ;
242100090625         QLGSS = %SIZE(s01hco);
242200090625         QLGDT = Numerico  ;
242300090625         QLGSO = Ascendente;
242400090625         QLGKL(3) = QLGSKL;
242500090625
242600090625       // Load other sort parameters.
242700090625         QLGLB = 80 + 16 * MaxKey;
242800090625         QLGRL = %SIZE(sflrcd) - 1;
242900090625         QLGRT = 8;
243000090625         QLGOKL = 80;
243100090625         QLGLKE = 16;
243200090625         QLGLSS = 290;
243300090625
243400090625       // Initialize Sort I/O API fields.
243500090625         QLGRL00 = QLGRL;
243600090625         QLGRC00 = 1;
243700090625         clear QUSEI;
243800090625         QUSBPRV = %SIZE(QUSEC);
243900090625
244000090625       // First step - Initialize the sort routine.
244100090625         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
244200090625
244300090625       // Next step - Write records to I/O routine.
244400090625         QLGRT00 = Put;
244500090625         for xx = 1 to rrnlast;
244600090625          chain xx MK18S01;
244700090625
244800090625       // solo le righe con Selected = 'Y' sono riordinate,
244900090625       // quindi per fare un ordinamento di tutte le righe
245000090625       // metto 'Y' sempre.
245100090625          selected  = 'Y';
245200090625          clear QUSEI;
245300090625          QUSBPRV = %SIZE(QUSEC);
245400090625          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
245500090625         endfor;
245600090625
245700090625       // Next step - Signal end of input, clear subfile for reload.
245800090625         QLGRT00 = EndPut;
245900090625         clear QUSEI;
246000090625         QUSBPRV = %SIZE(QUSEC);
246100090625         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
246200090625       // pulizia SFL
246300090625         exsr pulsfl1;
246400090625
246500090625       // Final step - Write the records back to the subfile.
246600090625         QLGRT00 = Get;
246700090625         for xx = 1 to rrnlast;
246800090625          clear QUSEI;
246900090625          QUSBPRV = %SIZE(QUSEC);
247000090625          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
247100090625          s01nrr = xx;
247200090713          coldcosca = (sh1n19 = '1');
247300090625          write Mk18s01;
247400090625         endfor;
247500090625
247600090625         // Visualizzazione del SFL (se ci sono dati)
247700090625         SflDsp_N = (S01nrr <= *zeros);
247800090625
247900090625         // Attivazione (eventuale) del SFLEND
248000090625         SflEnd  =  *on;
248100090625
248200090625         // Posizionamento cursore al 1° rcd della pagina
248300090625         if  S01nrr > *zero;
248400090625             c01rcd = 1;
248500090625         else;
248600090625             clear  C01rcd;
248700090625         endif;
248800090625
248900090625         C01csr = C01rcd;
249000090625
249100090625       ENDSR;
249200090625
249300080207       //--------------------------------------------------------------
249400080207       //?Gestione opzioni subfile
249500080207       //--------------------------------------------------------------
249600080207       BEGSR OpzS01;
249700080207
249800090406         readc MK18S01;
249900080207
250000090406         DOW  NOT  %eof(TRMK18D);
250100080207
250200100617           %subst(IndDspF : 50) = *off;
250300080207           SflNxtChg = *off;
250400080207           C01rcd    = S01nrr;
250500100318
250600100318       //?Controllo se utente abilitato all'attività
250700100318           $UteNoAbi = *off;
250800100318           exsr CtrAbiAtt;
250900080207
251000100720             // verifico se nel frattempo è stata eseguita da altro utente
251100100720                If   Sh1fatto = ' ';
251200100720                   chain (sh1tco:sh1nco:sh1ncop) tiatc01l ;
251300100720                   If %found(tiatc01l) and atcdco > 0 ;
251400100720                      sh1fatto = 'S' ;
251500100720                   endif ;
251600100720                endif ;
251700100720
251800080207           SELECT;
251900080207
252000080207             // - Nessuna opzione
252100080208             WHEN  S01opz  = *blank;
252200100105
252300110804             // - SOLO opzione 5 IN CASO di utenete di sede
252400110804             WHEN  S01opz <> '5' and Sede ;
252500110804                   errMessage  = *on;
252600110804                   errGenerico = *on;
252700110804                   PosCurOpz   = *on;
252800110804                   V1Dmsg = $Msg(18);
252900110804
253000100105             // - Nessuna opzione
253100100105             WHEN  S01opz <> *blank and sh1opz <> *blanks and
253200100105                   %scan(s01opz:sh1opz) > 0 ;
253300100105                   errMessage  = *on;
253400100105                   errGenerico = *on;
253500100105                   PosCurOpz   = *on;
253600100105                   V1Dmsg = $Msg(11);
253700080208
253800080208             // - Precedente segnalazione di errore
253900080208             WHEN  S01opz  = 'E';
254000080208               clear S01opz;
254100080207
254200090514             // - G = Gestione
254300090514             WHEN  S01opz  = 'G';
254400090514             // verifico se imposto G per un contatto già eseguito
254500100720                If   Sh1fatto = 'S';
254600090514                     errMessage  = *on;
254700090514                     errGenerico = *on;
254800090514                     PosCurOpz   = *on;
254900090609                     V1Dmsg = $Msg(09);
255000090514                else ;
255100100318              //?Se l'utente non è abilitato all'attività  solo interrogazione
255200100318                IF  $UteNoAbi;
255300100318                  ErrMessage  = *on;
255400100318                  ErrGenerico = *on;
255500100318                  PosCurOPZ   = *on;
255600100318                  V1Dmsg      = $Msg(15);
255700100318                ELSE;
255800100312
255900100223            // verifico il tipo attività ed in base a questo richiamo un programma
256000100223            // invece di un'altro
256100100316                     if sh1nrv > 0;
256200100223                        exsr Calltnta88r;
256300100223                     else ;
256400100223                        exsr Calltrmk19  ;
256500100223                     endif ;
256600100721                     If errMessage = *off;
256700100720                        $inzs01 = *on ;
256800100720                     endif ;
256900100312                  ENDIF;
257000090514                endif ;
257100090514
257200090514             // - 2 = Manutenzione
257300090514             WHEN  S01opz  = '2';
257400090514
257500090514             // verifico se imposto 2 per un contatto già eseguito
257600090514                If   Sh1fatto = 'S';
257700090514                     errMessage  = *on;
257800090514                     errGenerico = *on;
257900090514                     PosCurOpz   = *on;
258000090609                     V1Dmsg = $Msg(09);
258100090514                else ;
258200100312
258300100312              //?Se l'utente non è abilitato al potenziale solo interrogazione
258400100318                  IF  $UteNoAbi;
258500100312                    ErrMessage  = *on;
258600100312                    ErrGenerico = *on;
258700100312                    PosCurOPZ   = *on;
258800100312                    V1Dmsg      = $Msg(15);
258900100312                  ELSE;
259000090611                     exsr Calltrmk19  ;
259100100721                     If errMessage = *off;
259200100720                        $inzs01 = *on ;
259300100720                     Endif ;
259400100312                  ENDIF;
259500090514                endif;
259600090514
259700090514             // - 5 = Visualizzazione
259800090514             WHEN  S01opz  = '5';
259900090611                exsr Calltrmk19  ;
260000090514
260100080207             // - ? = Opzione NON valida
260200080207             OTHER;
260300080207               errMessage  = *on;
260400080207               errGenerico = *on;
260500080207               PosCurOpz   = *on;
260600090609               V1Dmsg = $Msg(10);
260700080207
260800080207           ENDSL;
260900080207
261000080207           // Aggiornamento sfl
261100080207           select;
261200080207             when  errMessage  = *on;
261300080207               SflNxtChg = *on;
261400080207               C01csr    = C01rcd;
261500080207             when  errGenerico = *on;
261600080208               C01csr    = C01rcd;
261700080208               clear  S01opz;
261800080207             other;
261900080207               C01csr    = C01rcd;
262000080208               clear  S01opz;
262100080207           endsl;
262200080207
262300090720           coldcosca = (sh1n19 = '1');
262400090406           update MK18S01;
262500080207
262600080208           if  errMessage = *on  or  errGenerico = *on;
262700080207             leave;
262800080207           endif;
262900080207
263000090406           readc MK18S01;
263100080207
263200080207         ENDDO;
263300080207
263400080207       ENDSR;
263500100318
263600100318       //--------------------------------------------------------------
263700100318       //?Controllo se utente abilitato all'attività.
263800100318       //--------------------------------------------------------------
263900100318       BEGSR CtrAbiAtt;
264000100318
264100100318       //?abilitato come commerciale
264200100318         clear TNTAA1DS;
264300100318         ITAA1caut = '§utegtc';
264400100318         ITAA1cmm  = S01ccm;
264500100318         kpjbu     = TNTAA1DS;
264600100318         tntaa1c (kpjba);
264700100318         TNTAA1DS = kpjbu;
264800100325         IF  OTAA1fabi <> 'N';
264900100325           leavesr;
265000100325         ENDIF;
265100100318
265200100318       //?abilitato come cliente
265300100617         IF  SH1ksc > 0;
265400100325           clear TNTAA1DS;
265500100325           ITAA1caut = '§utegtc';
265600100617           ITAA1ksc  = %dec(SH1ksc:7:0);
265700100325           kpjbu     = tntaa1ds;
265800100325           tntaa1c (kpjba);
265900100325           TNTAA1DS = kpjbu;
266000100325           IF  OTAA1fabi <> 'N';
266100100325             leavesr;
266200100325           ENDIF;
266300100325         ENDIF;
266400100325
266500100325       //?se arrivo a questo punto vuol dire che l'utente non è abilitato
266600100325       //?al commerciale o al cliente
266700100325         $UteNoAbi = *on;
266800100318
266900100318       ENDSR;
267000080207
267100090611       //--------------------------------------------------------------
267200090611       //?Richiamo trmk19r Gestione dettaglio contatti
267300090611       //--------------------------------------------------------------
267400090611       BEGSR Calltrmk19 ;
267500100312
267600090611
267700090611                     clear trmk19ds ;
267800090611                     imk19fle = s01opz ;
267900090611                     imk19cmt = 'S' ;
268000090611                     imk19fatt  = sh1fatto ;
268100090611                     imk19tco = s01tco ;
268200100617                     imk19cpo = sh1cpo ;
268300100617                     imk19ksc = sh1ksc ;
268400090611                     imk19tat = sh1tco ;
268500090611                     imk19atn = sh1nco ;
268600090611                     imk19atnp= sh1ncop;
268700090611                     if v01cmg <> *blanks ;
268800090611                        imk19com = %dec(v01cmg :7:0);
268900090611                        imk19comd = v01cmgd ;
269000090611                     endif ;
269100100312
269200110805              //?Se l'utente non è abilitato
269300100312              //?imposto IMK19fpr in modo da non abilitare dei tasti funzione
269400100316              //?in interrogazione attività, come già previsto dal pgm
269500100316              //?CHIODO 'V'  non ancora utilizzato
269600110805                  IF  $UteNoAbi ;
269700100312                    IMK19fpr = 'V';
269800100312                  ENDIF;
269900100312
270000090611                     trmk19r (kpjba : trmk19ds) ;
270100100720              // se ritorna l'errore lo emetto a video
270200100720                 If omk19err =  'E' ;
270300100720                     errMessage  = *on;
270400100720                     errGenerico = *on;
270500100720                     PosCurOpz   = *on;
270600100720                     V1Dmsg = omk19msg;
270700100720                endif;
270800090611
270900090611       endsr;
271000100223
271100100223       //--------------------------------------------------------------
271200100223       //?Richiamo TNTA88R Gestione trattative
271300100223       //--------------------------------------------------------------
271400100223       BEGSR Calltnta88r;
271500100223
271600100223             clear tnta88ds ;
271700100223             ita88cmt = 'N';
271800100223             ita88fle = 'G' ;
271900100316             ita88nrv = sh1nrv ;
272000100303                     if v01cmg <> *blanks ;
272100100303                        ita88com = %dec(v01cmg :7:0);
272200100303                        ita88comd = v01cmgd ;
272300100303                     endif ;
272400100223             Tnta88r ( kpjba : tnta88ds );
272500100223
272600100223       endsr;
272700080206       //--------------------------------------------------------------
272800080206       //?Operazioni finali.
272900080206       //--------------------------------------------------------------
273000080206       BEGSR RoutEnd;
273100090521
273200080206         *inLR = *on;
273300080206         return;
273400080206
273500080206       ENDSR;
273600080206
273700080206      /end-free
273800080206       //--------------------------------------------------------------
273900080206       //?Schiere a tempo di compilazione.
274000080206       //--------------------------------------------------------------
274100080206
274200080206** - $MSG -------------------------------------------------------------------*
274300100316Utente non abilitato alla funzione                                             01
274400100318Inserire "Da effettuare" oppure "Effettuate"                                   02
274500100209Codice commerciale errato o non in gestione all'utente                         03
274600090319Data errata                                                                    04 1
274700090319Data al deve essere maggiore o uguale a data dal                               05 4
274800100209Filiale errata o non in gestione all'utente                                    06
274900100316Non è stata trovata nessuna Attività                                           07
275000080208Potenziale inesistente                                                         08
275100100316Opzione non valida per attività già eseguite                                   09
275200090609Opzione errata                                                                 10
275300100105Opzione non valida per questa attività                                         11
275400100209Codice cliente errato o non in gestione all'utente                             12
275500150219Campagna inesistente                                                           13
275600141124Inserire almeno un dato tra comm.le, potenziale, cliente, filiale o campagna   14
275700100325Utente non abilitato. L'attività si può solo interrogare                       15
275800100316Inserire il Potenziale o il Cliente, non è possibile immettere entrambi.       16
275900141124Funzione abilitata per utente di filiale con NUOVA GESTIONE TRATTATIVE         17 NOT USED!
276000110804Opzione non valida.E' possibile solo la VISUALIZZAZIONE                        18
276100150220Campagna non ancora attiva o già scaduta                                       19
