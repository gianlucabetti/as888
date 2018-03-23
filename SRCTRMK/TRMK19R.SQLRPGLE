000100090610      //--------------------------------------------------------------
000200090610      //?TRMK19R - GESTIONE CONTATTI / TELEFONATE / APPUNTAMENTI
000300090610      //--------------------------------------------------------------
000400090610
000500090610     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090610     h dftactgrp(*no) actgrp(*caller)
000700090610
000800090610      //---------------------------------------------------------------
000900090610      //?Dichiarazione file.
001000090610      //---------------------------------------------------------------
001100090610
001200090610      // - Archivio Attività commerciale
001300090610     fTIATC01L  Uf a e           k disk    commit(flgcom) usropn
001400091210      // - Archivio Attività commerciale legami
001500091214     fTiatl01l  if   e           k disk    rename(tiatl000:tiatl01)
001600091210     fTiatl02l  if   e           k disk
001700090610      // - Anagrafica clienti potenziali
001800100115     fTNCPO01L  uf   e           k disk
001900140715     fTNCPO11L  if   e           k disk
002000090610      // - File contatti clienti e clienti potenziali
002100090610     fTFNTC01L  if   e           k disk
002200091119      // - Note  clienti potenziali
002300091119     fTICPN00F  if a e             disk    commit(flgcom) usropn
002400091113      // - Trattative
002500100520     fTIVIS05L  uf   e           k disk
002600100225      // - Offerte legate alla trattativa
002700101220     fTIVOF11L  if   e           k disk
002800110208      // - Offerte legate alla trattativa tariffa 999
002900110208     fTIVOF01L  uf   e           k disk    rename(tivof000:tivof999)
003000130809      // -?Anagrafica Commerciali?
003100130809     fAZCMM01L  if   e           k disk
003200100302      // - Anagrafica clienti
003300100302     fCNACO16L  if   e           k disk    rename(cnaco000:cnaco16)
003400090610
003500090610      // - Video Gestione contatti telefonate potenziali clienti
003600090610     fTRMK19D   cf   e             workstn
003700090610     f                                     indds(IndDspF)
003800090610     f                                     infds(InfDspF)
003900090610     f                                     usropn
004000090610
004100090610      //---------------------------------------------------------------
004200090610      //?Definizione costanti.
004300090610      //---------------------------------------------------------------
004400090610
004500090610      // - Tasti funzionali a video
004600090610     d c_F01           c                   const(x'31')
004700090610     d c_F02           c                   const(x'32')
004800090610     d c_F03           c                   const(x'33')
004900090610     d c_F04           c                   const(x'34')
005000090610     d c_F05           c                   const(x'35')
005100090610     d c_F06           c                   const(x'36')
005200090610     d c_F07           c                   const(x'37')
005300090610     d c_F08           c                   const(x'38')
005400090610     d c_F09           c                   const(x'39')
005500090610     d c_F10           c                   const(x'3A')
005600090610     d c_F11           c                   const(x'3B')
005700090610     d c_F12           c                   const(x'3C')
005800090610     d c_F13           c                   const(x'B1')
005900090610     d c_F14           c                   const(x'B2')
006000090610     d c_F15           c                   const(x'B3')
006100090610     d c_F16           c                   const(x'B4')
006200090610     d c_F17           c                   const(x'B5')
006300090610     d c_F18           c                   const(x'B6')
006400090610     d c_F19           c                   const(x'B7')
006500090610     d c_F20           c                   const(x'B8')
006600090610     d c_F21           c                   const(x'B9')
006700090610     d c_F22           c                   const(x'BA')
006800090610     d c_F23           c                   const(x'BB')
006900090610     d c_F24           c                   const(x'BC')
007000090610     d c_Enter         c                   const(x'F1')
007100090610     d c_RollDown      c                   const(x'F4')
007200090610     d c_RollUp        c                   const(x'F5')
007300090610
007400090610      // - Attributi di visualizzazione
007500090610      //  -  DspAtr() - Normale
007600090610     d C_dspatr_Norm   c                   const(x'20')
007700090610      //  -  DspAtr(RI)
007800090610     d C_dspatr_RI     c                   const(x'21')
007900090610     d C_dspatr_HI     c                   const(x'22')
008000090610      //  -  DspAtr(ND)
008100090610     d C_dspatr_ND     c                   const(x'27')
008200090610      //  -  DspAtr(BL) / Color(Red)
008300090610     d C_dspatr_BL     c                   const(x'28')
008400090611
008500090611      // - Titoli a video
008600091216     d Tit_primo       c                   const('        GESTIONE         ')
008700090611
008800091216     d Tit_visua       c                   const('     VISUALIZZAZIONE     ')
008900090611
009000091216     d Tit_manut       c                   const('      MANUTENZIONE       ')
009100090611
009200091216     d Tit_gesti       c                   const('        GESTIONE         ')
009300091216
009400091216      // - Titoli a video D02 parte bassa
009500100215     d c_PrimaAtt      c                   const('ATTIVITA'' IN ESECUZIONE')
009600091216     d c_Attivita      c                   const('ATTIVITA'' ESEGUITA')
009700100108
009800100108     d Digits          c                   const('0123456789')
009900090611
010000090610      //---------------------------------------------------------------
010100090610      //?Definizione schiere.
010200090610      //---------------------------------------------------------------
010300090610
010400090610      // - Messaggi di errore
010500130809     d $Msg            s             78    dim(36) ctdata perrcd(1)
010600110707      // - Causali che richiedono info commerciali e che non chiudono trattativa
010700100329     d $Cac            s              2    dim(20) ctdata perrcd(20)
010800100329      // - Corrispondente alle causali senza trattativa
010900100329     d $tra            s              2    dim(20) ctdata perrcd(20)
011000100329      // - Corrispondente alle causali apertura trattativa
011100100329     d $Apt            s              2    dim(20) ctdata perrcd(20)
011200121018      // - Tipi trattativa possibili per ogni attività
011300101220     d §ccoTPVds       ds            10    inz
011400101220     d   $Tpv                         1    inz  dim(10)
011500100225       // - Tipi attività possibili per ogni Causale Attività
011600100225     d §ccoTAAds       ds            10    inz
011700100225     d   $Taa                  1     10    inz  dim(10)
011800110502       // - Categorie potenziali abilitate
011900110502     d §ccoCpods       ds            10    inz
012000110502     d   $Cpo                  1      4    inz  dim(4)
012100090610
012200091020      // - Clienti
012300091020     d ksa             s              4    Dim(30)
012400091020     d ksc             s              7  0 Dim(30)
012500090610
012600090610      //---------------------------------------------------------------
012700090610      //?Definizione aree dati.
012800090610      //---------------------------------------------------------------
012900090610
013000090610      // - Dati utente
013100090610     d §AzUte        e ds                  extname(AZUTE00F)
013200090610     d                                     dtaara
013300090610     d §DatiUte      e ds                  extname(dDatiUte)
013400090610     d                                     dtaara
013500090610
013600090610      //---------------------------------------------------------------
013700090610      //?Definizione strutture dati.
013800090610      //---------------------------------------------------------------
013900090610
014000090610      // - Status
014100090610     d Psds           sds
014200090610     d   SDSpgm          *proc
014300090610
014400090610      // - InfDS
014500090610     d InfDspF         ds
014600090610     d  dsp_aid              369    369a                                        AID byte
014700090610
014800090610      // - Indicatori su DspF
014900090610     d IndDspF         ds
015000090610        // - Indicatori di errore 1° videata
015100090610     d  errMessage                    1n   overlay(IndDspF : 28)
015200090610     d  PosCurCom02                   1n   overlay(IndDspF : 60)
015300090610     d  PosCurDat02                   1n   overlay(IndDspF : 61)
015400090610     d  PosCurOra02                   1n   overlay(IndDspF : 62)
015500090610     d  PosCurCae02                   1n   overlay(IndDspF : 64)
015600090610     d  PosCurScd02                   1n   overlay(IndDspF : 65)
015700090610     d  PosCurSco02                   1n   overlay(IndDspF : 66)
015800090610     d  PosCurCmi02                   1n   overlay(IndDspF : 67)
015900090610     d  PosCurOii02                   1n   overlay(IndDspF : 68)
016000090610     d  PosCurOfi02                   1n   overlay(IndDspF : 69)
016100091216     d  PosCurNo102                   1n   overlay(IndDspF : 75)
016200100114     d  PosCurFatto                   1n   overlay(IndDspF : 81)
016300090610        // - Indicatori di errore window
016400090610     d  PosCurDsc03                   1n   overlay(IndDspF : 71)
016500090610     d  PosCurOsc03                   1n   overlay(IndDspF : 72)
016600090610     d  PosCurCom03                   1n   overlay(IndDspF : 73)
016700090701     d  PosCurOii03                   1n   overlay(IndDspF : 79)
016800090701     d  PosCurOfi03                   1n   overlay(IndDspF : 80)
016900090610        // - Indicatori di errore generico
017000090610     d  errGenerico                   1n   overlay(IndDspF : 99)
017100090610        // - Indicatori di visualizzazione
017200150831     d  F04Attivo                     1n   overlay(IndDspF : 04)
017300091215     d  primocontatto                 1n   overlay(IndDspF : 10)
017400090610     d  altrenote                     1n   overlay(IndDspF : 11)
017500090610     d  giafatto                      1n   overlay(IndDspF : 13)
017600090610     d  piunote                       1n   overlay(IndDspF : 14)
017700090610     d  manutenzione                  1n   overlay(IndDspF : 15)
017800090610     d  visualizza                    1n   overlay(IndDspF : 16)
017900090610     d  telefonata                    1n   overlay(IndDspF : 17)
018000090610     d  appuntamento                  1n   overlay(IndDspF : 18)
018100090623     d  vis_nonfatto                  1n   overlay(IndDspF : 19)
018200090612     d  potenziali                    1n   overlay(IndDspF : 20)
018300100928     d  Gestione                      1n   overlay(IndDspF : 21)
018400090630     d  prox_telefon                  1n   overlay(IndDspF : 22)
018500090630     d  prox_appunta                  1n   overlay(IndDspF : 23)
018600100309     d  vis_esito                     1n   overlay(IndDspF : 24)
018700090714     d  vis_trattativ                 1n   overlay(IndDspF : 25)
018800091021     d  cliente                       1n   overlay(IndDspF : 26)
018900091021     d  tratta_new                    1n   overlay(IndDspF : 27)
019000091022     d  affiancamento                 1n   overlay(IndDspF : 29)
019100091105     d  ric_visualizz                 1n   overlay(IndDspF : 30)
019200091105     d  ric_agenda                    1n   overlay(IndDspF : 31)
019300091105     d  ric_trattativ                 1n   overlay(IndDspF : 32)
019400100519     d  F16Attivo                     1n   overlay(IndDspF : 33)
019500091113     d  Trattativa                    1n   overlay(IndDspF : 34)
019600091118     d  Nuova                         1n   overlay(IndDspF : 35)
019700091210     d  F20Affianca                   1n   overlay(IndDspF : 37)
019800091214     d  ConAffianca                   1n   overlay(IndDspF : 38)
019900100108     d  Giornototale                  1n   overlay(IndDspF : 39)
020000100803     d  NO_F21                        1n   overlay(IndDspF : 40)
020100090610
020200090610     d WindDspF        ds                  inz
020300090610     d   WindDspFa             1     49    inz(*zero)
020400090610     d   WindDspFb            50     99    inz(*zero)
020500090610
020600090610      //  codice cliente potenziale a video 2
020700090610     d V02cpo          ds                  inz
020800090610     d   V02cp1                            inz
020900090610     d   V02cp2                            inz
021000090610      //  ora esecuzione contatto
021100090610     d SAV02ora        ds                  inz
021200090610     d   ora                   1      2  0
021300090610     d   minuti                3      4  0
021400090610
021500090610     d wlbdat          ds                  inz
021600090610     d  g02dat                 1      8  0
021700090610     d  g02inv                 9     16  0
021800090610     d  g02err                17     17
021900090610     d  g02tgi                18     22  0
022000090610
022100090610      // - Parametri ricevuti
022200090610     d KPJBA         e ds
022300090610     d TRMK19DS      e ds
022400090610
022500090610      // - Reperimento dati utente
022600090610     d TIBS34ds      e ds
022700090610
022800090610       // - Reperimento dati anagrafici
022900090610     d TIBS69ds      e ds
023000090610     d DS_cnaco      e ds                  inz extname(CNACO00F)
023100090610     d DS_cnind      e ds                  inz extname(CNIND00F)
023200090610     d DS_cnclp      e ds                  inz extname(CNCLP00F)
023300090610     d DS_fncls      e ds                  inz extname(FNCLS00F)
023400090610
023500090610      // - Ricerca/Controllo tabelle
023600090610     d TIBS02ds      e ds                  inz
023700090610     d   T02Mod      e                     inz('C')
023800100309
023900090610      // - Ricerca Causale attività
024000090610     d Tntb74ds      e ds                  inz
024100100309
024200100309      // - FLO del file attività
024300100309     d datc01        e ds                  inz
024400120622
024500120622      // - CPORST file TNCPO
024600120622     d dCPO01        e ds                  inz
024700101013
024800101013      // - FLO del file TRATTATIVA
024900101013     d dvis01        e ds                  inz
025000090610
025100090610      // - Tabella CCO = Causale contatto
025200090610     d dcco          e ds                  inz
025300110502
025400110502      // - Tabella CPO = Categoria Potenziale
025500110502     d dcpo          e ds                  inz
025600090610
025700091119      // - Tabella TTA = Tipo attività
025800091119     d dtta          e ds                  inz
025900100614
026000100614      // - Tabella TTR = Tipo trattativa
026100100614     d dttr          e ds                  inz
026200090610
026300090610      // - Ricerca ultimo numero contatti
026400090610     d trul33ds      e ds                  inz
026500100413
026600100413      // - Ricerca ultimo numero trattativa
026700100413     d trul48ds      e ds                  inz
026800100413
026900110429      // - Interrogaizone Info Commerciale
027000090610     d TRMK50ds      e ds                  inz
027100100928
027200100928      // - Interrogazione attività
027300100928     d TRMK21DS      e ds                  inz
027400090610
027500100921      // - Invio materiale illustrativo
027600100921     d TRMK24ds      e ds                  inz
027700090610      // - Interrogazione anagrafica clienti potenziali
027800100409     d TRMK01ds      e ds
027900110225
028000110225      // - Controllo se potenziale lavora
028100110225     d TRMK04ds      e ds                  inz
028200110304
028300110304      // - Calcolo categoria potenziale
028400110304     d TRMK05ds      e ds
028500110225
028600100409      // - Interrogazione anagrafica clienti potenziali
028700100409     d TRMK01ds1     e ds                  extname(trmk01ds) prefix(W_)
028800090610      // -  Gestione Note clienti/potenziali
028900090610     d TRMK20ds      e ds                  inz
029000091210      // -  Interrogazione/Manutenzione affiancamento
029100091210     d TRMK22ds      e ds
029200100329      // -  Invio mail conferma appuntamento
029300100915     d TRMK23ds1     e ds
029400110207      // -  Gestione attività blocco clienti
029500110207     d TRMK25ds      e ds
029600091027      // -  Interrogazione agenda
029700091027     d TRMK82ds      e ds                  inz
029800091216      // -  Controllo se commerciale impegnato
029900091216     d TRMK84ds      e ds
030000091215      // -  Controllo se ci sono altre attività aperte
030100091215     d TRMK85ds      e ds                  inz
030200100308      // - Gestione Contatti clienti/potenziali
030300090610     d TNTA12DS      e ds                  inz
030400100430     d  esi_ta16c      s              1
030500091021      // - Gestione anagrafica clienti
030600091021     d tnta60ds      e ds                  inz
030700101221      // - Gestione Data caldo senza offerte
030800101221     d TNTA63DS      e ds                  inz
030900091015      // - Gestione trattative commerciali
031000091015     d TNTA88ds      e ds                  inz
031100091210      // - Gestione esito offerta
031200091210     d TNTA56ds      e ds                  inz
031300100330      // - Controllo validità delle date
031400100330     d Xgiolavds     e ds                  inz
031500091019      // - Controllo abilitazioni
031600091019     d Tntaa1ds      e ds                  inz
031700100924      // - Gestione/interrogazione Informazioni trattativa
031800100924     d TNTA41ds      e ds                  inz
031900091112      // - Controllo se ci sono trattative aperte per lo stesso potenziale
032000091112     d Tnta43ds      e ds                  inz
032100091216      // - Controllo se commerciale diverso da commerciale trattativa
032200091216     d Tnta45ds      e ds                  inz
032300160804
032400160804       // -?Storicizzazione variazioni
032500160804     d TRMK30DS      e ds
032600160804     d TNCPO_30      e ds                  extname(TNCPO00F) inz
032700160804     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
032800160804     d TICPI_30      e ds                  extname(TICPI00F) inz
032900091019
033000090610      //---------------------------------------------------------------
033100090610      //?Definizione variabili globali.
033200090610      //---------------------------------------------------------------
033300090610
033400090610      // - Flags booleani
033500091215     d $EoF            s               n   inz(*off)
033600100517     d $Fine           s               n   inz(*off)
033700100517     d $Fine5          s               n   inz(*off)
033800100517     d $Fine6          s               n   inz(*off)
033900100517     d $Fine7          s               n   inz(*off)
034000100614     d $Fine8          s               n   inz(*off)
034100110225     d $Fine9          s               n   inz(*off)
034200090610     d $InzD02         s               n   inz(*on)
034300100108     d $InzD04         s               n   inz(*on)
034400090610     d $InzW03         s               n   inz(*on)
034500090610     d $ErrGrave       s               n   inz(*off)
034600091113     d $NumOK          s               n   inz(*off)
034700090610     d $prima          s               n   inz(*off)
034800090610     d $dasbf          s               n   inz(*off)
034900091116     d $new_Trattati   s               n   inz(*off)
035000091216     d $ForzaImpegno   s               n   inz(*off)
035100100310     d $ForzaComm      s               n   inz(*off)
035200100422     d $ForzaComm3     s               n   inz(*off)
035300100310     d $ForzaComin     s               n   inz(*off)
035400100318     d $ForzaComin2    s               n   inz(*off)
035500100308     d $Chiedi_conf    s               n   inz(*off)
035600100322     d $Gia_codifica   s               n   inz(*off)
035700110225     d $Lavora         s               n   inz(*off)
035800100614     d $ModAffianca    s               n   inz(*off)
035900100519     d $OkDel          s               n   inz(*off)
036000110405     d $Cli_non_sped   s               n   inz
036100090610
036200090610      // - Indici di schiera
036300090610     d Pos             s              3  0 inz
036400090610
036500090610      // - Campi associati al video
036600090610     d $Video          s              2    inz('D2')
036700090610
036800090610     d dsp_mod         s             10I 0
036900090610
037000090610      // - Campi di comodo data
037100090610     d  data_eur       s               d   Datfmt(*eur)
037200090610     d  data_iso       s               d   Datfmt(*iso)
037300090610
037400090610      // - Campi di comodo
037500091020     d  savcli         s                   like(acoksc)
037600140929     d  wchiudo_tr     s              1    inz
037700090610     d  w02dat         s              8  0
037800090610     d  w02scd         s              8  0
037900100108     d  w04dad         s              8  0
038000090610     d  ww03dsc        s              8  0
038100091113     d  newnrv         s              7  0
038200100504     d  W_vistpv       s                   like(vistpv)
038300091119     d  destina        s            120
038400100329     d  info_cac       s                   like(atccac)
038500100329     d  info_nrv       s                   like(atcnrv)
038600100329     d  info_nra       s                   like(atcnra)
038700100311     d  sav_v02cmi     s                   like(v02cmi)
038800100318     d  sav_v02cmi2    s                   like(v02cmi)
038900100311     d  sav_w03com     s                   like(w03com)
039000100422     d  sav_w03comt    s                   like(w03com)
039100100616     d app_atccmm      s                   like(ATCcmm)
039200101221     d sav_DataHot     s                   like(OTA63dpa)
039300090610
039400110412     d parmcbl         s              3A
039500110412
039600091218     d sav_atcdad      s                   like(atcdad)
039700111102     d sav_atchda      s                   like(atchda)
039800111102     d sav_atcoii      s                   like(atcoii)
039900111102     d sav_atcofi      s                   like(atcofi)
040000111102     d sav_atccmm      s                   like(atccmm)
040100091214     d sav_atlatnp     s                   like(atlatnp)
040200091218     d wdata           s              8  0 inz
040300140411     d wHhMm           s              4  0 inz
040400091216     d wpos            s              5i 0 inz
040500100108     d wtitolo         s             55A
040600100118     d sav_newnrv      s                   like(newnrv)
040700100118     d sav_newtrat     s                   like($new_trattati)
040800111223     d wOggi           s              8s 0
040900090610
041000090610      //---------------------------------------------------------------
041100090610      //?Definizione procedure usate.
041200090610      //---------------------------------------------------------------
041300091210
041400100409     d TRMK02C         pr                  extpgm('TRMK02C')
041500100409     d  kpjba                              likeds(kpjba)
041600100409     d  trmk01ds                           likeds(trmk01ds)
041700110225
041800110225      // - Controllo se potenziale lavora
041900110225     d Trmk04R         pr                  extpgm('TRMK04R')
042000110225     d  KPJBA                              likeds(KPJBA)
042100110225     d  TRMK04ds                           likeds(TRMK04ds)
042200110301     d  TRMK85ds                           likeds(TRMK85ds) options(*nopass)
042300110304
042400110304      // - Calcolo categoria potenziale
042500110304     d Trmk05r         pr                  extpgm('TRMK05R')
042600110304     d  kpjba                              likeds(KPJBA)
042700110304     d  trmk05ds                           likeds(TRMK05ds)
042800100409
042900091210     d TRMK22R         pr                  extpgm('TRMK22R')
043000091210     d  kpjba                              likeds(kpjba)
043100091210     d  trmk22ds                           likeds(trmk22ds)
043200110207
043300110207     d TRMK25R         pr                  extpgm('TRMK25R')
043400110207     d  kpjba                              likeds(kpjba)
043500110207     d  trmk25ds                           likeds(trmk25ds)
043600100928
043700100928      // - Interrogazione attività
043800100928     d trmk21r         pr                  extpgm('TRMK21R')
043900100928     d  kpjba                              likeds(KPJBA)
044000100928     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
044100100921
044200100921     d trmk24r         pr                  extpgm('TRMK24R')
044300100921     d   kpjba                             likeds(KPJBA)
044400100921     d   trmk24ds                          likeds(TRMK24ds)
044500090610
044600091026     D TRMK82R         pr                  extpgm('TRMK82R')
044700091113     D  kpjba                              likeds(kpjba)
044800091216
044900091216     d TRMK84R         pr                  extpgm('TRMK84R')
045000091216     d  trmk84ds                           likeds(trmk84ds)
045100091215
045200091215     d TRMK85R         pr                  extpgm('TRMK85R')
045300091215     d  trmk85ds                           likeds(trmk85ds)
045400100430
045500091112     D Tnta43R         pr                  extpgm('TNTA43R')
045600091113     D  tnta43ds                           likeds(tnta43ds)
045700091216
045800091216     d TNTA45R         pr                  extpgm('TNTA45R')
045900091216     d  tnta45ds                           likeds(tnta45ds)
046000101221
046100101221      // - Gestione data caldo
046200101221     d Tnta63r         pr                  extpgm('TNTA63R')
046300101221     d  kpjba                              likeds(kpjba)
046400101221     d  tnta63ds                           likeds(tnta63ds)
046500100330
046600100413     d TRUL48R         pr                  extpgm('TRUL48R')
046700100413     d  kpjba                              likeds(kpjba)
046800100413     d  trul48ds                           likeds(trul48ds)
046900150806
047000150806       // -?Interrogazione Campagna per Cliente
047100150806     d TRKC09DS      e ds                  inz
047200150806     d TRKC09R         pr                  extpgm('TRKC09R')
047300150806     d   kpjba                             likeds(kpjba)
047400150806     d   trkc09ds                          likeds(TRKC09DS)
047500150126
047600150126       // -?Controllo se cliente in (almeno una) Campagna?
047700150126     d TRKC73ds      e ds                  inz
047800150126     d TRKC73R         pr                  extpgm('TRKC73R')
047900150126     d   kpjba                             likeds(kpjba)
048000150126     d   trkc73ds                          likeds(TRKC73ds)
048100100413
048200100330      // - Aggiunge/Toglie gg/mm dalla data
048300100330     d Xgiolav         pr                  extpgm('XGIOLAV')
048400100330     d  xgiolavds                          likeds(xgiolavds)
048500160804
048600160804       // -?Storicizzazione Variazioni Potenziale
048700160804     d TRMK30R         pr                  extpgm('TRMK30R')
048800160804     d  trmk30ds                           likeds(trmk30ds)
048900160804     d  tncpo_30                           likeds(tncpo_30)
049000160804     d  tncpo1_30                          likeds(tncpo1_30)
049100160804     d  ticpi_30                           likeds(ticpi_30)
049200100330
049300091112      //---------------------------------------------------------------
049400090610      //?prototipi
049500090610      //---------------------------------------------------------------
049600090610      /copy gaitrasrc/srcprotopr,tibs02r
049700090610      /copy gaitrasrc/srcprotopr,tibs34r
049800090610      /copy gaitrasrc/srcprotopr,tibs69r
049900091019      /copy gaitrasrc/srcprotopr,tntaa1c
050000091019      /copy gaitrasrc/srcprotopr,tnta12r
050100100924      /copy gaitrasrc/srcprotopr,tnta41r
050200091210      /copy gaitrasrc/srcprotopr,tnta56r
050300091210      /copy gaitrasrc/srcprotopr,tnta60r
050400091210      /copy gaitrasrc/srcprotopr,tnta88r
050500090701      /copy gaitrasrc/srcprotopr,tntb74r
050600090610      /copy gaitrasrc/srcprotopr,trmk20r
050700100915      /copy gaitrasrc/srcprotopr,trmk23r1
050800130809      /copy gaitrasrc/srcprotoPI,TRMK43R_1
050900130809      /copy gaitrasrc/srcprotoPR,TRMK43R
051000090610      /copy gaitrasrc/srcprotopr,trmk50r
051100090610      /copy gaitrasrc/srcprotopr,trul33r
051200090610      /copy gaitrasrc/srcprotopr,xsrda8
051300090611      /copy gaitrasrc/srcprotopr,QsnQryModS
051400090610
051500090610      //---------------------------------------------------------------
051600090610      //?Definizione key-list.
051700090610      //---------------------------------------------------------------
051800090610
051900091210       // - File TIATL02L
052000091210     d k02tiatl      e ds                  extname(TIATL02L:*key)
052100091210     d                                     prefix(k_)
052200091210     d                                     inz
052300090610
052400090610      //---------------------------------------------------------------
052500090610      //?Riepilogo indicatori.
052600090610      //---------------------------------------------------------------
052700090610      // - visualizzazioni particolari
052800150126      // 04    : Attiva F4=Campagna
052900090610      // 10    : Emissione particolare prima telefonata
053000090610      // 11    : Emissione "Altre note" nel caso in cui ci siano + di 2 righe
053100090610      // 13    : Indica un'attività già eseguita
053200090610      // 14    : Emissione "+" nel caso in cui ci siano + di 2 note
053300090610      // 15    : Manutenzione di un'attività
053400090610      // 16    : Visualizza un'attività
053500090610      // 17    : Identifica un'attività telefonica
053600090610      // 18    : Identifica un'attività di appuntamento
053700090623      // 19    : Visualizza un'attività non fatta
053800091105      // 20    : Disabilita F1 Anagrafica se richiamato dal pgm dei potenziali e cliente a zero
053900100928      // 21    : Gestione di un'attività
054000090630      // 22    : Identifica una telefonata nella window
054100090630      // 23    : Identifica un appuntamento nella window
054200100309      // 24    : Visualizza l'esito solo se esiste
054300091019      // 25    : Visualizza la trattativa
054400091019      // 26    : Visualizza il codice cliente in testata ed pilota F1 Anagrafica Cliente
054500091022      // 27    : Visualizza il numero trattativa nuovo
054600091022      // 29    : Visualizza se appuntamento di affiancamento
054700091105      // 30    : No tasti funzionali - F12 se richiamato in visualizzazione da altri pgm - trmk18
054800091105      // 31    : Disabilito il tasto funzionale f11 agenda se richiamato da Agenda
054900091105      // 32    : Disabilito il tasto funzionale f09 Trattativa se richiamato da Trattativa
055000091113      // 34    : Visualizzo la trattativa legata all'attività futura che vado a scadenziare
055100091118      // 35    : Visualizzo NUOVA se la trattativa è nuova
055200091119      // 36    : Segnalo la presenza di più trattative
055300091215      // 37    : Abilita F20=Affiancamento
055400091215      // 38    : Visualizza che appuntamento è con affiancamento
055500090610      // - Comuni
055600090610      // 28    : Emissione messaggio di errore a video
055700090610      // - D02
055800090610      // 60    : Errore: Commerciale che esegue l'attività
055900090610      // 61    : Errore: Data attività
056000090610      // 62    : Errore: Ora attività
056100090610      // 64    : Errore: Causale attività
056200090610      // 65    : Errore: Scadenza data attività
056300090610      // 66    : Errore: Scadenza ora attività
056400090610      // 67    : Errore: Commerciale che edeve eseguire l'attività
056500090610      // 68    : Errore: Ora inizio impegno appuntamento attività
056600090610      // 69    : Errore: Ora fine   impegno appuntamento attività
056700091216      // 75    : Posizionamento cursore su campo Nota
056800091215      // - W03
056900091215      // 71    : Errore: Data appuntamento
057000091215      // 72    : Errore: Ora  appuntamento
057100091215      // 73    : Errore: Commerciale appuntamento
057200091215      // 79    : Errore: Ora inizio impegno
057300091215      // 80    : Errore: Ora fine   impegno
057400090610      // - Comuni
057500090610      // 99    : Generico di Errore
057600090610      //---------------------------------------------------------------
057700090610
057800090610      //---------------------------------------------------------------
057900090610      //?M A I N - L I N E
058000090610      //---------------------------------------------------------------
058100090610
058200090610     c     *Entry        plist
058300090610     c                   parm                    KPJBA
058400090610     c                   parm                    trmk19ds
058500100409     c                   parm                    trmk01ds1
058600100409
058700090610
058800090610      /free
058900090610
059000090611       // Operazioni iniziali controllo se video accetta 132 caratteri
059100090611       if not %open(trmk19d);
059200090611           dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
059300090611           If dsp_mod <> 0 ;
059400090611              open trmk19d;
059500090611           else ;
059600090611              eval $fine = *on ;
059700090611           endif;
059800090611       endif;
059900090610
060000090610       // Operazioni iniziali
060100090610       exsr RoutInz;
060200090610
060300100616       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
060400100616
060500091021       clear savcli ;
060600091021
060700091019       // Verifico se si tratta di primo contatto potenziale
060800090611       if imk19fle = 'P' ;
060900091019          exsr Primo_Po  ;
061000090611       endif ;
061100091019
061200091019       // Verifico se si tratta di primo contatto Cliente
061300091019       if imk19fle = 'C' ;
061400091019          exsr Primo_Cli ;
061500100427          If omk19err <> ' ' ;
061600100427             $fine = *on ;
061700100427          endif ;
061800091019       endif ;
061900090701
062000100108
062100100108       // Se richiamato da Agenda verifico se attività legata a cliente
062200100108       // o attività d'ufficio
062300100108       IF  IMK19fpr = 'A';
062400100108         clear dTTA;
062500100108         clear TIBS02ds;
062600100108         T02mod = 'C';
062700100108         T02cod = 'TTA';
062800100108         T02ke1 = IMK19tco ;
062900100108         T02sif = KNSIF;
063000100108         TNTBE_RicercaControllo  (kpjba : tibs02ds);
063100100108         IF  T02err = *blanks;
063200100108           dTTA = t02uni ;
063300100108         ENDIF;
063400100108         IF  §TTAleg <> 'S';
063500100108           $InzD02 = *off;
063600100108           $InzW03 = *off;
063700100108           $InzD04 = *on;
063800100108           $Video = 'D4';
063900100108         ENDIF;
064000100108       ENDIF;
064100090611
064200090610       // Gestione video
064300090610       DOW $Fine = *off;
064400090610         select;
064500090610           when $Video = 'D2';
064600090610             exsr GesD02;
064700100224           when $Video = 'W5';
064800100224             exsr GesW05;
064900090610           when $Video = 'W3';
065000090610             exsr GesW03;
065100100108           when $Video = 'D4';
065200100108             exsr GesD04;
065300100614           when $Video = 'W8';
065400100614             exsr GesW08;
065500110225           when $Video = 'W9';
065600110225             exsr GesW09;
065700110530           when $Video = 'W0';
065800110530             exsr GesW10;
065900090610           other;
066000090610             $Fine = *on;
066100090610         endsl;
066200090610       ENDDO;
066300090610
066400090610
066500090610       // Operazioni finali
066600090610       exsr RoutEnd;
066700090611
066800090611       //--------------------------------------------------------------
066900091019       //?Gestione Primocontatto  Potenziale
067000090611       //--------------------------------------------------------------
067100091019       BEGSR Primo_Po ;
067200090611
067300090611        // se potenziale valido recupero i dati del potenziale da immettere nella prima
067400090611        // parte della videata
067500090611
067600100409         Cpocpo = W_mk01cpo ;
067700100409         W_mk01F06 = ' ' ;
067800090611         primocontatto = *on ;
067900090611         $Video = 'D2';
068000090611         $InzD02 = *on;
068100090611         $prima  = *on;
068200090611
068300090611       ENDSR;
068400090610
068500091019       //--------------------------------------------------------------
068600091019       //?Gestione Primocontatto  Cliente
068700091019       //--------------------------------------------------------------
068800091019       BEGSR Primo_Cli ;
068900100802
069000100802        // se cliente valido recupero i dati del cliente da immettere nella prima
069100100802        // parte della videata
069200100802
069300100802         savcli = IMK19ksc;
069400100802         clear  tibs69ds;
069500100802         I69kac = savcli;
069600100802         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
069700100802         if acolib = 0 ;
069800100802            omk19ksc = savcli ;
069900100802            OMK19ERR = 'E' ;
070000100802            OMK19MSG = 'Manca Potenziale sul cliente impossibile gestire nuove +
070100100802            attività' ;
070200100802         else ;
070300100802            primocontatto = *on ;
070400100802            $Video = 'D2';
070500100802            $InzD02 = *on;
070600100802            $prima  = *on;
070700100802         endif ;
070800091019
070900091019
071000091019       ENDSR;
071100091019
071200090610       //--------------------------------------------------------------
071300090610       //?Gestione videata D02
071400090610       //--------------------------------------------------------------
071500090610       BEGSR GesD02;
071600090610
071700090610         // Inizializzazione videata
071800090610         if $InzD02 = *on;
071900090610           exsr InzD02;
072000090610           $InzD02 = *off;
072100090610         endif;
072200110301
072300110301       //?Se dopo la INZ del video non devo emettere la videata ma la win con
072400110301       //?il messaggio di annullo attività esco dalla routine
072500110527         IF  $Video = 'W9';
072600110301           leavesr;
072700110301         ENDIF;
072800110530       //?Se dopo la INZ del video non devo emettere la videata ma la win con
072900110530       //?il messaggio di avviso attività da annullare esco dalla routine
073000110530         IF  $Video = 'W0';
073100110530           leavesr;
073200110530         ENDIF;
073300101221
073400101221         clear sav_DataHot;
073500090610
073600090610         // Emissione testata
073700090610           write MK19T01;
073800090610
073900090610         // Emissione videata
074000090610         exfmt MK19D02;
074100090610         errMessage   = *off;
074200090610         errGenerico  = *off;
074300090610         clear V1Dmsg;
074400090610
074500090610         select;
074600090610         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
074700090610           when  $ErrGrave = *on;
074800090610             $Fine  = *on;
074900090610         // F12 = Ritorno
075000090610           when dsp_aid = c_F12;
075100090610             exsr F12D02;
075200090611             leavesr ;
075300090701         // F21 = invio mail
075400090701           when dsp_aid = c_F21;
075500100921                If V02rtr = *blanks or v02mrt = *blanks ;
075600100921                   errMessage  = *on;
075700100921                   errGenerico = *on;
075800100921                   V1Dmsg = $Msg(32);
075900100921                   leavesr;
076000100921                 endif;
076100090701             exsr F21d02;
076200090701             leavesr ;
076300091021         // F01 = Anagrafica
076400090610           when dsp_aid = c_F01;
076500090610             exsr F01D02;
076600150126           // -?F4=Campagna?
076700150126           when  dsp_aid = c_F04;
076800150126             exsr  F04D02;
076900090610         // F18 = Note
077000090610           when dsp_aid = c_F18;
077100090610             exsr F18D02;
077200090610         // F02 = Contatti
077300090610           when dsp_aid = c_F02;
077400090610             exsr F02D02;
077500100511             leavesr ;
077600090610         // F14 = Informazioni commerciali
077700090610           when dsp_aid = c_F14;
077800090610             exsr F14D02;
077900091026         // F09 = Trattativa
078000091026           when dsp_aid = c_F09;
078100091026             exsr F09D02;
078200091026         // F11 = Agenda
078300091026           when dsp_aid = c_F11;
078400091026             exsr F11D02;
078500091026             leavesr ;
078600091210         //?F20 = Affiancamento
078700091210           when dsp_aid = c_F20;
078800091210             exsr F20D02;
078900091210             leavesr ;
079000100301         // F16 = Annullamento abilitato solo in manutenzione da trattativa
079100100224           when dsp_aid = c_F16;
079200100224             exsr F16D02;
079300100224             leavesr ;
079400100928         //?F05 = Attività
079500100930         //  when dsp_aid = c_F05;
079600100930         //    exsr F05D02;
079700100930         //    leavesr ;
079800090610
079900090610         // Enter
080000090610           other;
080100090610         // Se non si tratta di visualizzazione
080200090610            If visualizza = *off ;
080300090610               exsr CtrD02;
080400090610               if errGenerico = *on ;
080500090610                   leavesr;
080600090610               endif;
080700090610            endif;
080800090610           endsl;
080900090610
081000100329         // se dato f8 conferma appuntamento + invio mail prima verifico se ha indirizzo mail
081100100329         // altrimenti errore
081200100329           If   dsp_aid = c_F08 ;
081300100329                If V02rtr = *blanks or v02mrt = *blanks ;
081400100329                   errMessage  = *on;
081500100329                   errGenerico = *on;
081600100329                   PosCurCmi02 = *on;
081700100329                   V1Dmsg = $Msg(32);
081800100329                   leavesr;
081900100329                 endif;
082000100329                    exsr F06D02 ;
082100100329
082200100329         // Ritorno al programma precedente
082300100329                    $fine  = *on ;
082400100329                    leavesr ;
082500100329           endif ;
082600100329
082700100329         // se dato f6 conferma
082800090610           If   dsp_aid = c_F06 ;
082900090610
083000090610         // -...- VISUALIZZAZIONE vado via
083100090610                If  visualizza   = *on ;
083200090610                    leavesr ;
083300090610                endif ;
083400090610
083500090610         // -...- MANUTENZIONE
083600090610                If  manutenzione = *on ;
083700090610                    exsr F06D02 ;
083800090610                    leavesr ;
083900090610                endif ;
084000090610
084100090610         // -...- GESTIONE
084200100308
084300100308         // verifico se risulta necessario fare delle domandine
084400100308
084500100308         // chiedo esito attività/trattativa    se causale lo richiede
084600100308             If  §ccores = 'S' ;
084700100308                  exsr Ges_Esito  ;
084800100308             endif ;
084900101221
085000101221         //?Se attività prevede la richiesta della data caldo senza offerta
085100101221         //?richiamo il pgm
085200101221             IF  §CCOhot = 'S';
085300101221               exsr Ric_TA63;
085400101221         //?se F12 o errore devo tornare alla videata dell'esito attività
085500101221               IF  OTA63f12 = 'S' or OTA63err <> *blanks;
085600101221                 leavesr;
085700101221               ENDIF;
085800101221         //?memorizzo la data caldo senza offerta
085900101221               sav_DataHot = OTA63dpa;
086000101221             ENDIF;
086100110301
086200110301            SELECT;
086300110301
086400110301         //?Se attività prevede il richiamo al programma di gestione anagrafica clienti
086500110301         //?richiamo il pgm
086600110301             WHEN  §CCOpgmblc <> *blanks;
086700110301           //?verifico se l'utente può gestire il cliente
086800110301               reset TNTAA1DS;
086900110302               ITAA1ksc  = V02cli;
087000110301               ITAA1caut = '§UTECLI';
087100110301               kpjbu     = TNTAA1DS;
087200110301               tntaa1c (kpjba);
087300110301               TNTAA1DS = kpjbu;
087400110301               IF  OTAA1fabi = 'N';
087500110301                 chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
087600110307                 V1Dmsg = $msg(04);
087700110301                 errMessage  = *on;
087800110301                 errGenerico = *on;
087900110301                 leavesr;
088000110301               ENDIF;
088100110301               clear TNTA60DS;
088200110301               TA60flg = 'M';
088300110301               TA60cli = V02cli;
088400110412               Parmcbl = atcprg ;
088500110412               tnta60r (kpjba:tnta60ds:Parmcbl);
088600110301               IF  TA60f03 <> *blanks or TA60err <> *blanks;
088700110301                  chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
088800110504                  If TA60err <> *blanks ;
088900110504                     errMessage  = *on;
089000110504                     errGenerico = *on;
089100110504                     V1Dmsg = ta60msg;
089200110504                  Endif ;
089300110301                  leavesr;
089400110301               ENDIF;
089500100308
089600110207        // se attività richiede la creazione di una nuova attività ma richiede anche
089700110207        // il richiamo al blocco del cliente richiamo il pgm TRMK25R
089800110301             WHEN  §ccoblc <> ' ' ;
089900110207                   exsr Ric_trmk25 ;
090000110207                   IF  OMK25f12 = 'S' or OMK25err <> *blanks;
090100110207                       chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
090200110207                       leavesr;
090300110207                   ENDIF;
090400100329         // Emetto la window  W03  se causale necessita la creazione di un nuovo contatto
090500110301             WHEN   §ccoupm <> ' ' ;
090600100520         //?se attività legata a trattativa e chiude trattativa a potenziale codificato
090700100520         //?non devo creare nuova attività
090800100520                  IF  Vis_trattativ and §CCOcht = 'S' and $Gia_Codifica;
090900100924                      §ccoupm = ' ';
091000100520                  ELSE;
091100100520
091200091214         //?Se appuntamento con affiancamento prima di emettere la win per nuova attività
091300091214         //?devo esitare prima l'affiancamento
091400100311                    IF  ConAffianca;
091500100311                        exsr Conf_TRMK22 ;
091600091214           //?se errore o F12 devo tornare alla videata dell'esito appuntamento
091700100311                        IF  OMK22f12 = 'S' or OMK22err <> *blanks;
091800100311                            leavesr;
091900100311                        ENDIF;
092000100311                    ENDIF;
092100090610                    $Video = 'W3';
092200090610                    $inzw03 = *on ;
092300100311                    leavesr ;
092400100520                  ENDIF;
092500110301
092600110301             ENDSL;
092700100311
092800100311         // registro la scrittura del contatto senza creare il contatto successivo
092900090610                    exsr regis_atc ;
093000091214
093100091214         //?Se appuntamento con affiancamento devo esitare anche l'eventuale affiancamento
093200100311                    IF  ConAffianca;
093300100721                        exsr Ric_TRMK22 ;
093400100311           //?se errore o F12 devo tornare alla videata dell'esito appuntamento
093500100311                        IF  OMK22f12 = 'S' or OMK22err <> *blanks;
093600100721                            chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
093700100311                            leavesr;
093800100311                        ENDIF;
093900100311                    ENDIF;
094000100311
094100100311         // chiedo conferma chiusura trattativa
094200100311             If  $chiedi_conf = *on ;
094300110622                  exsr Ges_Chiudotr ;
094400100311         // rolbk se ci sono errori o non richiesto annullamento trattativa
094500100503                  if (imk19cmt = 'S' and omk19err <> ' ') or w06sino = 'N' ;
094600100311                        rolbk ;
094700100311                        $inzd02 = *on ;
094800100311                        leavesr ;
094900100311                     endif ;
095000100311             endif ;
095100091214
095200091210         // verifico se devo richiamare il pgm apertura trattativa
095300091116                    if §ccoapt <> *blank  ;
095400091015         // richiamo il programma gestione trattativa
095500091015                          exsr Ric_TNTA88 ;
095600091022                          If ota88f12 = 'S' ;
095700091022                             $inzd02 = *on ;
095800091022                             leavesr ;
095900091022                          endif ;
096000091015                    endif ;
096100100311
096200091210         // verifico se devo richiamare il pgm esito offerta
096300091210                    if §ccoeso <> *blank  ;
096400091210         // richiamo il programma esito offerta
096500091210                          exsr Ric_TNTA56 ;
096600100308                          If ota56f12 = 'S' or ota56Err = 'S' ;
096700091210                             $inzd02 = *on ;
096800091210                             leavesr ;
096900091210                          endif ;
097000091210                    endif ;
097100091016
097200100311         // comitto se richiesto e se non ci sono errori
097300091016            if imk19cmt = 'S' and omk19err = ' ';
097400091016               commit ;
097500091016            endif ;
097600091016
097700100419         // Richiedo Info Commerciali
097800100419                exsr Sr_infocomm ;
097900091016
098000110412         // alla fine di tutto se il potenziale è in categoria Eliminabile
098100110412         // richiamo il TRMK05 che mi ricalcola la categoria
098200110412           IF  CPOfls = 'E';
098300110412               clear trmk05ds;
098400110412               IMK05cpo = CPOcpo;
098500110412               trmk05r (kpjba : TRMK05DS);
098600110412         //?se categoria variata aggiorno potenziale
098700110412               IF  OMK05err = *blanks and OMK05cat <> CPOfls;
098800110412                   chain(e) CPOcpo TNCPO01L;
098900110412                   IF  not %error and %found(TNCPO01L);
099000110412                       CPOfls = OMK05cat;
099100110412                       update TNCPO000;
099200110412                   ENDIF;
099300110412               ENDIF;
099400110412           ENDIF;
099500110412
099600100329         // Ritorno al programma precedente
099700091016                $fine  = *on ;
099800090610                endif;
099900091015
100000090610       ENDSR;
100100100308
100200100308       //--------------------------------------------------------------
100300110622       //?Gestione Richiesta conferma chiusura Trattativa
100400100308       //--------------------------------------------------------------
100500110622       BEGSR Ges_Chiudotr ;
100600100308
100700100308             clear w06sino ;
100800100517             $fine6 = *off ;
100900100517
101000100517          dow $fine6 = *off ;
101100100308             exfmt mk19w06 ;
101200100517
101300100517       // N  = Nulla
101400100517             If w06sino = 'N' ;
101500100517                leavesr ;
101600100517             endif ;
101700100517
101800100517       // S  = Richiamo il pgm chiusura trattativa
101900100308
102000100503             If w06sino = 'S' ;
102100100520
102200100520               chain V02vis TIVIS05L;
102300100520               IF  %found(TIVIS05L);
102400100520                 VISesi = V02cae;
102500100520                 VISdch = %dec(%date());
102600101013        // se ho richiesto l'esito della attività lo scrivo nei FLO
102700101013                 If §ccores = 'S' ;
102800101013                    clear dvis01 ;
102900101013                    dvis01 = visflo ;
103000101013                    §visesito = W07esi ;
103100101013                    visflo = dvis01 ;
103200101013                 endif ;
103300100520                 update TIVIS000;
103400100520               ENDIF;
103500110208
103600110208        // in caso di trattativa con tariffe 999 vado ad aggiornare i dati dell'offerta / caldo
103700110208             chain (V02vis:999:0) tivof01l ;
103800110208             If %found(tivof01l) ;
103900110208             // in caso di richiesta esito e l'esito è positivo convalido l'offerta
104000110208                If §ccores = 'S' and W07esi = 'P' ;
104100110208                   vofdco = %dec(%date()) ;
104200110208                   vofeso = 'C' ;
104300110208                endif;
104400110208             // in caso di richiesta esito e l'esito è negativo annullo l'offerta
104500110208                If §ccores = 'S' and W07esi = 'N' ;
104600110208                   vofeso = '*' ;
104700110208                endif;
104800110208             // in caso di chiusura senza richiesta di esito annullo l'offerta
104900110208                If §ccores <> 'S'  ;
105000110208                   vofeso = '*' ;
105100110208                endif;
105200110208                update tivof999 ;
105300110208             Endif ;
105400110208
105500100924         // richiamo il programma delle info trattative se inserite
105600110622               If visinfot <> ' ' or (ATCCAC = '41' and vistpv = 'I');
105700100924                   exsr Sr_infotrat ;
105800120706         // se al ritorno dal PGM INFO trattattive sono state inserite ed il flag INFOT
105900120706         // sul file TRATTATIVE è a blank lo aggiorno
106000120706                   If o41ifotot <> ' ' and visinfot = ' ' ;
106100120706                      chain V02vis TIVIS05L;
106200120706                      IF  %found(TIVIS05L);
106300120706                          VISinfot = o41ifotot ;
106400120706                          update TIVIS000;
106500120706                      ENDIF;
106600120706                   endif ;
106700110622         // se in uscita visinfot è uguale a " " e causale 41 di tratattiva di tipo I
106800110622         // richiamo info trat
106900110622               dow o41ifotot = ' ' and atccac = '41' and vistpv =  'I' ;
107000110622                   exsr Sr_infotrat ;
107100110622                   If o41ifotot <> ' ' ;
107200110622                      chain V02vis TIVIS05L;
107300110622                      IF  %found(TIVIS05L);
107400110622                          VISinfot = o41ifotot ;
107500110622                          update TIVIS000;
107600110622                      ENDIF;
107700110622                   endif ;
107800110622               enddo;
107900100924               Endif ;
108000100311
108100100924         // richiamo il programma delle info commerciali
108200140929               Wchiudo_tr = 'S' ;
108300100924               exsr F14D02 ;
108400100924
108500100517             leavesr ;
108600100308             endif ;
108700100517           enddo ;
108800100308
108900100308       Endsr ;
109000100308
109100100924       //--------------------------------------------------------------
109200100924       //?Gestione Info trattativa
109300100924       //--------------------------------------------------------------
109400100924       BEGSR Sr_infotrat ;
109500100924
109600100924         // Richiamo il pgm informazioni Trattativa
109700100924         clear  TNTA41ds;
109800100924         I41nrv    = visnrv;
109900100924         I41rag    = visrag;
110000100924         I41ifotot = VISinfot;
110100100924         I41mod    = 'G';
110200100924         I41pgm    = 'TRMK19R';
110300101220         I41tpv    = VIStpv;
110400100924
110500100924         tnta41r (kpjba : TNTA41ds);
110600100924
110700100924       ENDSR;
110800100308       //--------------------------------------------------------------
110900100308       //?Gestione Richiesta Esito Attività/trattativa
111000100308       //--------------------------------------------------------------
111100100308       BEGSR Ges_Esito  ;
111200100308
111300100308             clear w07esi  ;
111400100517             $fine7 = *off ;
111500100517
111600100517           Dow $fine7 = *off ;
111700100308             exfmt mk19w07 ;
111800100517             If w07esi =  'P' ;
111900100517                leavesr ;
112000100517             endif ;
112100100517             If w07esi =  'N' ;
112200100517                leavesr ;
112300100517             endif ;
112400100517           enddo ;
112500100308
112600100308       Endsr ;
112700090610
112800090610       //--------------------------------------------------------------
112900090610       //?Caricamento videata telefonata D02
113000090610       //--------------------------------------------------------------
113100090610       BEGSR Inzd02;
113200090610
113300090610          clear mk19d02 ;
113400090610
113500090610       // imposto indicatori di visualizzazione
113600090610          Visualizza = (Imk19Fle = '5');
113700090610          Manutenzione = (Imk19Fle = '2');
113800100928          Gestione     = (Imk19Fle = 'G');
113900091119          Telefonata   = (Imk19tco = 'T') or (imk19tco = 'O') ;
114000090610          Appuntamento = (IMK19tco = 'A');
114100110405
114200110405          $cli_non_sped = *off;
114300110405
114400091119             // tipo attivita
114500091119                  clear  dTTA;
114600091119                  clear  TIBS02ds;
114700091119                  T02mod = 'C';
114800091119                  T02cod = 'TTA';
114900091119                  T02ke1 = Imk19tco ;
115000091119                  T02sif = KNSIF;
115100091119                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
115200091119                  if  T02err = *blanks;
115300091119                      dtta = t02uni ;
115400091119                  endif;
115500091216
115600091216         //?Centro titolo alto
115700091216         clear wpos;
115800091216         wpos = (%len(v02tit) - %len(§ttatit)) / 2;
115900091216         %subst(v02tit:wpos:%len(§ttatit)) = §ttatit;
116000091216
116100090623          If visualizza = *on ;
116200090623             Vis_nonfatto = (Imk19Fatt = ' ');
116300090623          else ;
116400090623             Vis_nonfatto = *off ;
116500090623          endif ;
116600090612
116700090612       // Verifico se è stato richiamato dal Programma di gestione Potenziali
116800091105              Potenziali = (imk19fpr = 'P') ;
116900091105       // Verifico se è stato richiamato dal Programma Agenda
117000091105              Ric_agenda = (imk19fpr = 'A') ;
117100091105       // Verifico se è stato richiamato dal Programma Gestione trattative
117200091105              Ric_trattativ = (imk19fpr = 'T') ;
117300091105       // Verifico se è stato richiamato in visualizzazione non dal TRMK18
117400091105              Ric_visualizz = (imk19fpr <> ' ') ;
117500100803       // Verifico se è stato richiamato con flag di provenienza "V" solo visualizzazione
117600110805       // Verifico se è utente di sede non utilizza F21
117700131127       // Verifico se è stata chiamata dall'azione MK01 non utilizza F21
117800131127              NO_F21 = (imk19fpr = 'V') or (Dutpou= 046) or KCDAZ = 'MK01';
117900091105
118000090611       // imposto testata
118100091020          If  imk19fle = 'P' or imk19fle = 'C';
118200090611              v01tit = tit_primo ;
118300090611          endif ;
118400090610
118500090611          If  imk19fle = '5' ;
118600090611              v01tit = tit_visua ;
118700090611          endif ;
118800090611
118900090611          If  imk19fle = '2' ;
119000090611              v01tit = tit_manut ;
119100090611          endif ;
119200090611
119300090611          If  imk19fle = 'G' ;
119400090611              v01tit = tit_gesti ;
119500090611          endif ;
119600090611
119700091026       // imposto il codice cliente
119800091026          v02cli  = savcli ;
119900091026
120000090610       // se provengo da sbf devo fare gli agganci con i dati passati dal subfile
120100091026          If Imk19fle <>  'P' and imk19fle <> 'C';
120200090610             cpocpo = imk19cpo ;
120300091026             v02cli = imk19ksc ;
120400090610          endif ;
120500090610
120600090610             altrenote = *off  ;
120700090610             piunote   = *off  ;
120800091218             clear sav_atcdad;
120900111102             clear sav_atchda;
121000111102             clear sav_atcoii;
121100111102             clear sav_atcofi;
121200111102             clear sav_atccmm;
121300090610
121400090610       // recupero i dati anagrafici ed informativi del potenziale
121500090610
121600090610          exsr RecD02 ;
121700090610
121800090610        // se contatto non inserito manualmente oppure primo contatto recupero
121900090610        // dati dal file contatti
122000090610         If  $prima = *off  ;
122100090610             If manutenzione = *off and visualizza = *off ;
122200090611                chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
122300090610             else  ;
122400090611                chain(n) (imk19tat:imk19atn:imk19atnp) tiatc01l ;
122500090610             endif ;
122600090610             If   %found(tiatc01l);
122700140411               // -?Data/Ora/Utente inserimento?
122800140414               clear  Data_Eur;
122900140414               clear  wData;
123000140414               clear  wHhMm;
123100140414               if  ATCdim > *zero;
123200140414                 Data_Eur = %date( ATCdim : *iso );
123300140414                 wData    = %dec( Data_Eur );
123400140414               endif;
123500140414               wHhMm = %int( ATChim / 100 );
123600140414               %subst( V02tit : %len(V02tit) - 32 ) = 'Ins. ' +
123700140414                       %editc( wData : 'Y' ) + ' ' +
123800140414                       %editw( wHhMm : '  :  ' ) + ' ' +
123900140414                       ATCpri;
124000090610             // data scadenza contatto
124100090610                  If atcdad > 0 ;
124200090610                     data_eur = %date(atcdad:*iso);
124300090610                     v02scd   = %dec(data_eur) ;
124400091211                     w02scd   = atcdad;
124500091218                     sav_atcdad = atcdad;
124600111102                     sav_atchda = atchda;
124700111102                     sav_atcoii = atcoii;
124800111102                     sav_atcofi = atcofi;
124900111102                     sav_atccmm = atccmm;
125000090610                  endif ;
125100090610             // ora  scadenza contatto
125200090610                  If atchda > 0 ;
125300090610                     v02sco = %dec(%subst(%EDITC(atchda:'X'):1:4):4:0);
125400090610                  endif ;
125500090610             // causale contatto
125600090610                  clear  dCCO;
125700090610                  clear  TIBS02ds;
125800090610                  T02mod = 'C';
125900090610                  T02cod = 'CCO';
126000090610                  T02ke1 = atccad;
126100090610                  T02sif = KNSIF;
126200090610                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
126300090610                  if  T02err = *blanks;
126400090610                      dcco = t02uni ;
126500090714                      v02cau  = atccad  ;
126600090714                      v02caud = §ccodes ;
126700110405             // Verifico se causale su cliente che non spedisce
126800110405                      If §ccocns = 'S' ;
126900110405                         $cli_non_sped = *on ;
127000110405                      Endif;
127100090610                  endif;
127200091021          // verifico se affiancamento
127300091021                  affiancamento = (atccad = '92') ;
127400100616            //?cerco il commerciale dell'Appuntamento
127500100615                  IF  Affiancamento;
127600100616                    exec sql
127700100616                    SELECT ATCcmm into :app_ATCcmm from TIATL00F
127800100616                    JOIN TIATC00F on
127900100616                    ATLatl = ATCatn and ATLatlp = ATCatnp
128000100616                    WHERE ATLatn = :IMK19atn and ATLatnp = :IMK19atnp;
128100100616                    clear V02dcmma;
128200100616              //?trovo l'appuntamento decodifico il commerciale
128300100616                    IF  sqlcod = 0;
128400130809                      chain (app_ATCcmm) AZCMM000;
128500130809                      IF  %found(AZCMM01L);
128600130809                        V02dcmma = CMMdes;
128700100616                      ENDIF;
128800100616                    ENDIF;
128900100615                  ENDIF;
129000091021
129100090610                  v02cmi = %editc(atccmm : 'X') ;
129200090610         // decodifica del commerciale
129300130809                  clear V02cmiD;
129400130809                  chain (ATCcmm) AZCMM000;
129500130809                  if  %found(AZCMM01L);
129600130809                    v02cmid = CMMdes;
129700090610                  endif;
129800090703           // ora inizio impegno
129900090703                  v02oii = atcoii ;
130000090703           // ora fine   impegno
130100090703                  v02ofi = atcofi ;
130200090610           // numero trattativa
130300090610                  v02vis = atcnrv ;
130400090610           // cliente e decodifica
130500091026                  v02cli = atcksc ;
130600090610                  clear  tibs69ds;
130700091026                  I69kac = v02cli;
130800090610                  TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
130900091026                  v02clid = ACOrag;
131000090610           // note contatto PRECEDENTE
131100090610                clear trmk20ds ;
131200090610                imk20tla = 'L';
131300090610                imk20flm = 'R';
131400090610                imk20cpo = cpocpo ;
131500090610                imk20rsc = v02cpd ;
131600090611                imk20tat = imk19tat ;
131700090611                imk20atn = imk19atn ;
131800090611                imk20atnp= imk19atnp -1;
131900090616                Trmk20r ( kpjba : trmk20ds );
132000090610                v02no1o =  emk20no1 ;
132100090610                v02no2o =  emk20no2 ;
132200090610                piunote = (omk20piu = '+');
132300090610
132400090610             endif ;
132500090610          // se si tratta di un contatto già eseguito
132600090611             if Imk19fatt = 'S' ;
132700090610         //  proteggo i campi di input
132800090610                giafatto = *on ;
132900090610
133000090610             // verifco    si tratta di un 1° contatto se data scadenza = 0
133100090610                primocontatto = (v02scd = 0) ;
133200090610
133300140411               // -?Data/Ora/Utente attività eseguita?
133400140414               clear  Data_Eur;
133500140414               clear  wData;
133600140414               clear  wHhMm;
133700140414               if  ATCdco > *zero;
133800140414                 Data_Eur = %date( ATCdco : *iso );
133900140414                 wData    = %dec( Data_Eur );
134000140414               endif;
134100140414               wHhMm = %int( ATChco / 100 );
134200140414               %subst( V02tit2 : %len(V02tit2) - 27 ) =
134300140414                       %editc( wData : 'Y' ) + ' ' +
134400140414                       %editw( wHhMm : '  :  ' ) + ' ' +
134500140414                       ATCpru;
134600090610             // recupero i dati relativi all'esecuzione del contatto
134700090610                v02com = %editc(atccco:'X') ;
134800130809                clear  V02comD;
134900130809                chain (ATCcco) AZCMM000;
135000130809                if  %found(AZCMM01L);
135100130809                  V02comd = CMMdes;
135200090610                endif;
135300090610             // data contatto
135400090610                data_eur=%date(atcdco:*iso);
135500090610                v02dat=%dec(data_eur);
135600090610             // ora  contatto
135700090610                v02ora=%dec(%subst(%editc(atchco:'X'):1:4):4:0);
135800090610             // causale  contatto
135900090610                v02cae = atccac ;
136000090610                clear  dCCO;
136100090610                clear  TIBS02ds;
136200090610                T02mod = 'C';
136300090610                T02cod = 'CCO';
136400090610                T02ke1 = v02cae ;
136500090610                T02sif = KNSIF;
136600090610                TNTBE_RicercaControllo  (kpjba : tibs02ds);
136700090610                if  T02err = *blanks;
136800090610                    dcco = t02uni ;
136900090610                    V02caed = §ccodes ;
137000090610                endif;
137100100223             // se appuntamento visualizzo  l'esito
137200100223                if atctat = 'A' ;
137300100223                   v02fatto = atcest ;
137400100223                endif ;
137500091021             // verifico generata una nuova trattativa da questa attività
137600091021                tratta_new = (atcnra > 0) ;
137700091021                v02nra = atcnra ;
137800100309             // verifico se c'è l'esito
137900100309                datc01 = atcflo ;
138000100309                vis_esito = (§atcesito <> ' ') ;
138100100309                If §atcesito = 'P'  ;
138200100309                   v02esi = 'POSITIVO' ;
138300100309                else ;
138400100309                   v02esi = 'NEGATIVO' ;
138500100309                endif ;
138600090610             // recupero note
138700090610                clear trmk20ds ;
138800090610                imk20tla = 'L';
138900090610                imk20flm = 'R';
139000090610                imk20cpo = cpocpo ;
139100090610                imk20rsc = v02cpd ;
139200090611                imk20tat = imk19tat ;
139300090611                imk20atn = imk19atn ;
139400090611                imk20atnp= imk19atnp;
139500090616                Trmk20r ( kpjba : trmk20ds );
139600090610                v02no1 =  emk20no1 ;
139700090610                v02no2 =  emk20no2 ;
139800090610                altrenote = (omk20piu = '+');
139900090610             endif ;
140000090610
140100090610         else ;
140200090610             primocontatto = *on ;
140300090610         endif ;
140400090610         // propongo alcuni campi  se si tratta di un contatto ancora da eseguire
140500090623         // se non si tratta di visualizzazione
140600090623
140700091214         If  Not Visualizza and
140800091214             ((IMK19fatt = *blanks and $dasbf) or Not $dasbf);
140900091214         //?Se non è appuntamento
141000091214           IF Not Appuntamento;
141100090610             // commerciale chi sono
141200091214                If imk19com > *zeros;
141300090714                   v02com  = %editc(imk19com : 'X')  ;
141400090714                   v02comd = imk19comd ;
141500090714                endif ;
141600090610            //  data e ora del momento
141700090610                v02dat = *date ;
141800090610                v02ora = %dec(%subst(%EDITC(%dec(%time()):'X'):1:4):4:0);
141900091214         //?Se è appuntamento
142000091214           ELSE;
142100091214         //?imposto il commerciale, data e ora dell'appuntamento
142200091214         //?inoltre vengono protetti il commerciale e la data
142300091214             v02com  = v02cmi;
142400091214             v02comd = v02cmid;
142500091214             v02dat  = v02scd;
142600091214             v02ora  = v02sco;
142700110302           ENDIF;
142800091214         // flag giafatto a 0
142900091214           giafatto = *off ;
143000090610         endif ;
143100090714
143200090714        // verifico se visualizzare oppure no cliente e trattativa
143300090714
143400091026         cliente   = (v02cli > 0) ;
143500090714         vis_trattativ = (v02vis > 0) ;
143600091119
143700091119        // verifico se esistono trattativa e lo visualizzo
143800100118        // oltre a quella legata all'attività in carico
143900091119                   clear tnta43ds ;
144000091119                   ita43cpo = cpocpo ;
144100091119                   ita43ksc = v02cli ;
144200091119                   ita43nrv = v02vis ;
144300091119                   Tnta43r (tnta43ds) ;
144400100118            // se l'attività in carico non ha la trattativa
144500100118            // e mi torna un n. di trattativa imposto la scritta
144600100118            IF  v02vis = 0 and ota43nrv > 0;
144700100118              evalr v02segn = 'TRATTATIVE APERTE';
144800100118            ENDIF;
144900100118            // se l'attività in carico ha la trattativa
145000100118            // e mi torna un n. di trattativa imposto la scritta
145100100119            IF  v02vis > 0 and ota43nrv > 0 and v02vis <> ota43nrv;
145200100118              evalr v02segn = 'ALTRE TRATTATIVE APERTE';
145300100118            ENDIF;
145400091216
145500091216         //?Centro titolo basso
145600091216         clear wpos;
145700091216         IF  Not PrimoContatto;
145800091216         wpos = (%len(v02tit2) - %len(c_Attivita)) / 2;
145900091216         %subst(v02tit2:wpos:%len(c_Attivita)) = c_Attivita;
146000091216         ENDIF;
146100091216         IF  PrimoContatto;
146200091216         wpos = (%len(v02tit2) - %len(c_PrimaAtt)) / 2;
146300091216         %subst(v02tit2:wpos:%len(c_PrimaAtt)) = c_PrimaAtt;
146400091216         ENDIF;
146500091216
146600091216          If visualizza = *on ;
146700091216             Vis_nonfatto = (Imk19Fatt = ' ');
146800091216          else ;
146900091216             Vis_nonfatto = *off ;
147000091216          endif ;
147100091215
147200091215         //?verifico se per potenziale/cliente ci sono attività aperte
147300100118         //?ma solo se non è già impostato il campo con trattativa aperte
147400100118         IF  v02segn = ' ';
147500091215           clear TRMK85ds;
147600091215           IMK85cpo = cpocpo;
147700091215           IMK85ksc = v02cli;
147800100118           IMK85nrv = v02vis;
147900100129           IMK85atn = IMK19atn;
148000091215           trmk85r (trmk85ds);
148100100118         //?se torna errore <> "9" ho trovato delle attività aperte
148200091215           IF  OMK85err <> *blanks and OMK85err <> '9';
148300091215         //?imposto la costante a video
148400100118             evalr v02segn = 'ALTRE ATTIVITA'' APERTE';
148500091215           ENDIF;
148600091215         ENDIF;
148700091215
148800091214         //?verifico se appuntamento con Affiancamento
148900091214         ConAffianca = *off;
149000091214         F20Affianca = *off;
149100091214         IF  Appuntamento and Not Affiancamento;
149200091214           k_atlatl  = IMK19atn;
149300091214           k_atlatlp = IMK19atnp;
149400091214           chain  %kds(K02tiatl) TIATL02L;
149500091214           IF  %found(Tiatl02l);
149600091216             ConAffianca = *on;
149700091214           ENDIF;
149800091210         ENDIF;
149900091216
150000091216         //?Non abilito F20=Affiancamento in gestione
150100091216         IF  Appuntamento and Manutenzione;
150200091216           F20Affianca = *on;
150300091216         ENDIF;
150400091216         IF  ConAffianca and Visualizza;
150500091216           F20Affianca = *on;
150600091216         ENDIF;
150700100519
150800100519         //?Abilito F16=Annullamento
150900100519         F16Attivo = *off;
151000100519         $okdel = *off;
151100100519         SELECT;
151200110214         //?se codice attività impedisce l'annullamento dell'attività disabilito F16
151300110519         //?e il cliente è perso e non ha attività
151400110519           WHEN  §ccoann = 'S' and Manutenzione ;
151500110519                 If Cpofls <> 'P' or (Cpofls = 'P' and v02segn <> *blank) ;
151600110519                    F16Attivo = *on;
151700110519                    $okdel = *on;
151800110519                 Endif ;
151900100519         //?se sono in gestione attività richiamato da gestione trattativa
152000100520           WHEN  Manutenzione and Ric_Trattativ;
152100100519             F16Attivo = *on;
152200100519             $okdel = *on;
152300110908         //?se sono in gestione attività non legata a trattativa
152400110908         //?attività su potenziale perso e ci sono altre attività aperte
152500110908           WHEN  Manutenzione and not Vis_Trattativ and CPOfls = 'P' and
152600110908                 V02segn <> *blanks;
152700110908             F16Attivo = *on;
152800100519         //?se sono in gestione attività non legata a trattativa
152900100519         //?attività su potenziale mai codificato e ci sono altre attività aperte
153000110225           WHEN  Manutenzione and not Vis_Trattativ and CPOfls = 'M' and
153100110225                 V02segn <> *blanks;
153200100519             F16Attivo = *on;
153300100520         //?se sono in gestione attività non legata a trattativa
153400100520         //?attività su potenziale codificato
153500110225           WHEN  Manutenzione and not Vis_Trattativ and CPOfls = 'C';
153600100520             F16Attivo = *on;
153700100519         //?se sono in gestione attività non legata a trattativa
153800100519         //?attività su cliente
153900100520           WHEN  Manutenzione and not Vis_Trattativ and v02cli > 0;
154000100519             F16Attivo = *on;
154100100519         ENDSL;
154200150831
154300150831         // -?Se attività su Cliente visualizzo il tasto F4?
154400150831         F04Attivo = (IMK19ksc <> *zeros);
154500091216
154600091216         //?Se trattativa a video cerco la data della trattativa
154700091216         clear V02VisDat;
154800100504         clear w_vistpv ;
154900100614         clear V02VisTtr;
155000091216         IF  Vis_Trattativ;
155100100520           chain(n)  V02vis tivis05l;
155200091216           IF  %found(tivis05l);
155300091216             data_eur  = %date(visdat:*iso);
155400091216             V02VisDat = %dec(data_eur);
155500100504             w_vistpv = vistpv ;
155600100614         //?decodifico il tipo trattativa
155700100614             clear dTTR;
155800100614             clear TIBS02ds;
155900100614             T02mod = 'C';
156000100614             T02cod = 'TTR';
156100100614             T02ke1 = w_vistpv;
156200100614             T02sif = KNSIF;
156300100614             TNTBE_RicercaControllo  (kpjba : tibs02ds);
156400100614             IF  T02err = *blanks;
156500100614                 dTTR = t02uni;
156600100614                 V02VisTtr = §ttrdes;
156700100614             ENDIF;
156800091216           ENDIF;
156900091216         ENDIF;
157000091216
157100100114         PosCurFatto = *off;
157200091216         PosCurNo102 = *off;
157300091216         PosCurCom02 = *off;
157400091216
157500091216         //?Se non è manutenzione e non sto visualizzando un non fatto
157600100111         //?e non è interrogazione
157700100111         IF  Not Manutenzione and Not Vis_NonFatto and Not Visualizza;
157800100114           SELECT;
157900100114         //?Se il commerciale non è stato proposto posiziono il cursore sul commerciale
158000100114             WHEN  v02com = *zeros or V02com = *Blanks;
158100100114               PosCurCom02 = *on;
158200100114         //?Se è appuntamento mi posiziono sul 'Effettuato s/n'
158300100114             WHEN  Appuntamento;
158400100114               PosCurFatto = *on;
158500100114             OTHER;
158600100503         //?altrimenti posiziono il cursore sulla causale
158700100503               PosCurCae02 = *on;
158800100114           ENDSL;
158900091216         ENDIF;
159000100325
159100100325        // Recupero informazioni responsabile trasporti
159200100325
159300100325         exsr respD02 ;
159400091216
159500100310         $ForzaImpegno = *off;
159600100310         $ForzaComm    = *off;
159700100422         $ForzaComm3   = *off;
159800100310         $ForzaComin   = *off;
159900100318         $ForzaComin2  = *off;
160000100311         clear sav_v02cmi ;
160100100318         clear sav_v02cmi2;
160200100311         clear sav_w03com ;
160300100422         clear sav_w03comt;
160400090714
160500110225       //?Se sto gestendo un'attività ancora da fare
160600110225       //?e l'attività prevede il controllo del cliente "perso"
160700110301           IF  IMK19fatt <> 'S' and §CCOccp = 'S' and Gestione;
160800110527       // se attività 70 faccio un controllo se 71 un altro
160900110530             If  atccad = '71' ;
161000110225         //?Controllo se potenziale lavora o no
161100110527                 clear TRMK04DS;
161200110527                 clear TRMK85DS;
161300110527                 IMK04cpo = CPOcpo;
161400110527                 IMK85cpo = cpocpo;
161500110527                 IMK85ksc = v02cli;
161600110527                 IMK85nrv = v02vis;
161700110527                 IMK85atn = IMK19atn;
161800110527                 trmk04r (kpjba : TRMK04DS : TRMK85DS);
161900110225         //?Se il potenziale ha ripreso a lavorare devo emettere una win dove avviso
162000110527         //?di verificare le attività e trattative aperte
162100110527                 IF  OMK04lav = 'SI';
162200110527                    $Lavora = *on;
162300110527                    $InzD02 = *off;
162400110527                    $Video = 'W0';
162500110527                 ENDIF;
162600110527             ENDIF;
162700110527       // se attività 70 faccio un altro controllo
162800110530             If  atccad = '70' ;
162900110527         //?Controllo se potenziale lavora o no
163000110527                 clear TRMK04DS;
163100110530                 clear TRMK85DS;
163200110527                 IMK04cpo = CPOcpo;
163300110530                 trmk04r (kpjba : TRMK04DS : TRMK85DS);
163400110530         //?Se il potenziale ha ripreso a lavorare devo emettere una win nella quale
163500110530         //?avvisa che ci sono trattative aperte e annulla l'attività in corso
163600110530                 IF  OMK04lav = 'SI';
163700110527                    $Lavora = *on;
163800110527                    $InzD02 = *off;
163900110527                    $Video = 'W9';
164000110527                 ENDIF;
164100110527             ENDIF;
164200110225           ENDIF;
164300110214
164400090610       ENDSR;
164500090610
164600090610       //--------------------------------------------------------------
164700090610       //?Recupero dati anagrafici / resp / mail / stato potenziale
164800090610       //--------------------------------------------------------------
164900090610       BEGSR RecD02;
165000090610
165100090610
165200091019        // Recupero dati anagrafico potenziale o cliente in base ai dati passati
165300090610
165400091021         exsr AnagD02 ;
165500090610
165600110502              $Gia_Codifica = *off ;
165700100322           // verifico se codificato su altri codici
165800110502           //   chain cpocpo  CNACO16L;
165900110502           //    IF  %found(CNACO16L);
166000110502           //        $Gia_Codifica = *on ;
166100110502           //     endif ;
166200110502           // considero un potenziale già codificato se il falf del potenziale è uguale a "C"
166300110502              If cpofls = 'C' ;
166400110502                 $gia_codifica = *on ;
166500110502              Endif ;
166600110502           // decodifico la categoria del potenziale
166700110502                  clear  dCPO;
166800110502                  clear  TIBS02ds;
166900110502                  T02mod = 'C';
167000110502                  T02cod = 'CPO';
167100110502                  T02ke1 = cpofls ;
167200110502                  T02sif = KNSIF;
167300110502                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
167400110502                  if  T02err = *blanks;
167500110502                      dcpo = t02uni ;
167600110502                  endif;
167700090610
167800090610
167900090610       ENDSR;
168000090610
168100090610       //--------------------------------------------------------------
168200091021       //?Recupero dati potenziale/cliente
168300090610       //--------------------------------------------------------------
168400091021       BEGSR AnagD02 ;
168500091020       // se esite il codice cliente recupero i dati anagrafici da lui
168600091020             If v02cli <> *zeros ;
168700091021                cliente = *on ;
168800091020                clear  tibs69ds;
168900091020                I69kac = v02cli;
169000091020                I69kin = v02cli;
169100091020                I69kcp = v02cli;
169200091020                TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
169300091020                v02clid = acorag ;
169400091020                cpocpo  = acolib ;
169500091020                v02cpi = indvia ;
169600091020                v02cpl = indcit ;
169700091020                v02cpt = indtel ;
169800091020                v02cpf = indtlf ;
169900091020                v02cpc = %editc(indcap:'X') ;
170000091020                v02cpp = indprv ;
170100091020                v02cpn = indsta ;
170200091020                v02cmp = clpage ;
170300091020             endif ;
170400091020
170500120622          clear dCPO01;
170600090610             chain(N)  cpocpo  TNCPO000;
170700140715             chain     cpocpo  TNCPO100;
170800140715             if  NOT %found(TNCPO11L);
170900140715               clear  CPO1tel;
171000140715               clear  CPO1fax;
171100140715             endif;
171200090610       // carico dati del potenziale
171300090610             v02cpo = %editc(cpocpo:'X');
171400090610             v02cpd = cporag ;
171500120622             dCPO01 = CPOrst;
171600091020       // se non esite il codice cliente recupero i dati anagrafici dal potenziale
171700091020             If v02cli =  *zeros ;
171800091021                cliente = *off;
171900091020                v02cpi = cpovia ;
172000091020                v02cpl = cpocit ;
172100140715                v02cpt = CPO1tel;
172200140715                v02cpf = CPO1fax;
172300091020                v02cpc = cpocap ;
172400091020                v02cpp = cpoprv ;
172500091020                v02cpn = cponaz ;
172600091020                v02cmp = cpocmm ;
172700091020             endif ;
172800090610
172900090610
173000091020        // recupero dedodifica dle commerciale
173100130809         clear  V02cmpD;
173200130809         chain (v02cmp) AZCMM000;
173300130809         if  %found(AZCMM01L);
173400130809           V02cmpd = CMMdes;
173500090610         endif;
173600090610
173700090610         ENDSR ;
173800090610
173900090610       //--------------------------------------------------------------
174000090610       //?Recupero responsabile trasporti e mail
174100090610       //--------------------------------------------------------------
174200090610       BEGSR RespD02 ;
174300090610
174400100511             clear v02rtr ;
174500100511             clear v02mrt ;
174600090610       // carico responsabile trasporti
174700100326             SELECT ;
174800100326
174900100326       // se c'è il cliente recupero dal cliente
175000100408             when v02cli > 0 ;
175100100326                 clear ntcnk1 ;
175200100326                 ntcapl = 'C' ;
175300100408                 ntcnk1 = %editc(dutkci: 'X') + %editc(v02cli:'X');
175400100329       // se aperta trattativa recupero da questa
175500100408             when v02nra > 0 ;
175600100329                 clear ntcnk1 ;
175700100329                 ntcapl = 'T' ;
175800100408                 ntcnk1 = %editc(dutkci: 'X') + %editc(v02nra:'X');
175900100326       // se c'è trattativa recupero da questa
176000100408             when v02vis > 0 ;
176100100326                 clear ntcnk1 ;
176200100326                 ntcapl = 'T' ;
176300100408                 ntcnk1 = %editc(dutkci: 'X') + %editc(v02vis:'X');
176400100326             other ;
176500100325                 clear ntcnk1 ;
176600100325                 ntcapl = 'P' ;
176700100325                 ntcnk1 = %editc(cpocpo:'X');
176800100326             endsl ;
176900100326
177000090610             ntctnt = '05';
177100090610             clear  ntcnk2 ;
177200091020          setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
177300091020          READE    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
177400090610          dow not %eof(tfntc01l)  ;
177500100329             vh2rtr = ntcrnt ;
177600090610             v02rtr = ntcrnt ;
177700091020             READE    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
177800090610             If  not %eof(tfntc01l) ;
177900090610               v02rtr = %trim(v02rtr) + ', ' + %trim(ntcrnt) ;
178000090610               leave ;
178100090610             else;
178200090610               leave ;
178300090610             endif ;
178400090610          enddo ;
178500090610
178600090610       // carico mail responsabile trasporti
178700090610             ntctnt = '06';
178800090610             clear  ntcnk2 ;
178900091020          chain    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
179000090610          if  %found(tfntc01l)  ;
179100090610             v02mrt = ntcrnt ;
179200090610          endif ;
179300090610
179400090610       ENDSR;
179500090610
179600090611       //--------------------------------------------------------------
179700090611       //?Controllo videata D02
179800090611       //--------------------------------------------------------------
179900090611       BEGSR CtrD02;
180000090611
180100090611            WindDspF = IndDspF;
180200090611            reset WindDspFb;
180300090611            IndDspF  = WindDspF;
180400090611
180500100308            $chiedi_conf = *off ;
180600090611
180700100721         // verifico se nel frattempo hanno già esitato l'attività
180800100721                UNLOCK TIATC01L;
180900100721                chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
181000100721                IF %FOUND(TIATC01L) AND ATCDCO > 0 ;
181100100721                   errMessage  = *on;
181200100721                   errGenerico = *on;
181300100721                   PosCurCom02 = *on;
181400100721                   V1Dmsg = $Msg(07);
181500100721                   LEAVESR ;
181600100721                ENDIF ;
181700090611         // Se sono in manutenzione devo fare i controlli per la sola parte realtiva alla
181800090611         // scadenza dell'attività
181900090611
182000090611           If manutenzione = *on ;
182100090611              exsr Ctrd02Scd ;
182200090611              leavesr;
182300090611           endif ;
182400090611
182500090611         // Controllo codice commerciale (? Ricerca)
182600090611           If    %scan('?' : v02com) > 0  ;
182700090611             errGenerico = *on;
182800090611             PosCurCom02 = *on;
182900130809             clear  TRMK43ds;
183000130809             iMK43fil  = DUTpou;
183100130809             TRMK43R (kpjba : TRMK43ds);
183200130809             if  oMK43err = *off  and  oMK43fxx = *blank;
183300130809               v02com  = %editc( oMK43cmm : 'X' );
183400130809               v02comd = oMK43des;
183500130809             endif;
183600090611             leavesr;
183700090611           endif;
183800090611
183900100210         // Commerciale che esegue il contatto  obbligatorio
184000100210           If  v02com = *blanks or v02com = *zeros;
184100100210             errMessage  = *on;
184200100210             errGenerico = *on;
184300100210             PosCurCom02 = *on;
184400100210             V1Dmsg = $Msg(02);
184500100210             leavesr;
184600100210           endif;
184700100210
184800100210         // Controllo commerciale che esegue il contatto
184900100210             IF  %check(digits:V02com) > 0;
185000100210               ErrMessage  = *on;
185100100210               ErrGenerico = *on;
185200100210               PosCurCom02 = *on;
185300100210               V1Dmsg      = $Msg(02);
185400100210               leavesr;
185500100210             ENDIF;
185600100210
185700100210           //?utente abilitato al commerciale
185800100210             clear TNTAA1DS;
185900120810             ITAA1tipo = 'M';
186000120810             ITAA1caut = '§utegtc';
186100100210             ITAA1cmm  = %dec(V02com:7:0);
186200100519             ITAA1abi = 'RA';
186300100210             kpjbu     = tntaa1ds;
186400100210             tntaa1c (kpjba);
186500100210             TNTAA1DS = kpjbu;
186600100210             IF  OTAA1fabi = 'N';
186700100210               ErrMessage  = *on;
186800100210               ErrGenerico = *on;
186900100210               PosCurCom02 = *on;
187000100210               V1Dmsg      = $Msg(02);
187100100210               leavesr;
187200100210             ENDIF;
187300100210
187400130809             CMMcod = %int(V02com);
187500130809             chain (CMMcod) AZCMM000;
187600130809             if  NOT  %found(AZCMM01L);
187700090611               errMessage  = *on;
187800090611               errGenerico = *on;
187900090611               PosCurCom02 = *on;
188000090611               V1Dmsg = $Msg(02);
188100090611               leavesr;
188200100210             endif;
188300130809             V02comd = CMMdes;
188400100416
188500110525
188600130809             IF  CMMPAR <> ' ' ;
188700110525               ErrMessage  = *on;
188800110525               ErrGenerico = *on;
188900110525               PosCurCom02 = *on;
189000110525               V1Dmsg      = $Msg(02);
189100110525              leavesr;
189200110525             ENDIF;
189300111223
189400111223       //?Commerciale "valido" data inizio e fine attività
189500130809         IF  CMMdtIni > wOggi  or
189600130809             CMMdtFin < wOggi;
189700111223           ErrMessage = *on;
189800111223           ErrGenerico = *on;
189900111223           PosCurCom02 = *on;
190000111223           V1Dmsg = $Msg(02);
190100111223           leavesr;
190200111223         ENDIF;
190300110525
190400100416         // Se appuntamento devo controllare anche se è stato inserito il FATTO
190500100416           IF  Appuntamento and V02fatto = *blanks;
190600100416             ErrGenerico = *on;
190700100416             ErrMessage  = *on;
190800100416             PosCurFatto = *on;
190900100416             V1Dmsg      = $msg(21);
191000100416             leavesr;
191100100416           ENDIF;
191200100416
191300090611
191400100601         // controllo data esecuzione contatto solo se non sono in appuntamento non eseguito
191500171221           If    V02dat  > 0;
191600090611             clear wlbdat;
191700090611             g02dat = v02dat;
191800090611             xsrda8(wlbdat);
191900090611             if g02err = '1';
192000090611               errMessage  = *on;
192100090611               errGenerico = *on;
192200090611               PosCurDat02 = *on;
192300090611               V1Dmsg = $Msg(03);
192400090611               leavesr;
192500090611             endif;
192600090611
192700090611             v02dat = g02dat;
192800090611             w02dat = g02inv;
192900090611           else ;
193000090611             errMessage  = *on;
193100090611             errGenerico = *on;
193200090611             PosCurDat02 = *on;
193300090611             V1Dmsg = $Msg(03);
193400171222             if  V02dat < *zeros;
193500171222               V1Dmsg = %trimR( V1Dmsg ) + ' (negativa)';
193600171222             endif;
193700090611             leavesr;
193800090611           endif;
193900100118
194000100601           If  (appuntamento and v02fatto =  'S') or appuntamento = *off ;
194100100118         // controllo che la data non sia inferiore al 2000 o superiore al 2039
194200100118         IF  w02dat < 20000101 or w02dat > 20391231;
194300100118           errMessage  = *on;
194400100118           errGenerico = *on;
194500100118           PosCurDat02 = *on;
194600100118           V1Dmsg = $Msg(03);
194700100118           leavesr;
194800100118         ENDIF;
194900090611
195000100330       // controllo che la data non sia maggiore di oggi ed inferiore a 3 gg lavorativi da oggi
195100100330         clear XGIOLAVDS;
195200100330         IXGLdata = %dec(%date());
195300100330         IXGLsub  = 'S';
195400100330         IXGLgg   = 3;
195500100330         IXGLpa   = 'P';
195600100330         IXGLlav  = 'S';
195700100330         IXGLfil = %dec(%subst(V02com:1:3):3:0);
195800100330         XGIOLAV (xgiolavds);
195900100330         IF  w02dat < OXGLdata;
196000100330           ErrMessage  = *on;
196100100330           ErrGenerico = *on;
196200100330           PosCurDAT02 = *on;
196300100330           V1dmsg      = $Msg(03);
196400100330           V1dmsg      = %trim(V1dmsg) + ' ' +
196500100330           'Non può essere inferiore a 3 gg.lavorativi';
196600100330           leavesr;
196700100330         ENDIF;
196800100330
196900100330         // controllo con la data del giorno
197000100330            data_iso = (%date()) ;
197100100408         IF  w02dat > %dec(data_iso) ;
197200100330           ErrMessage  = *on;
197300100330           ErrGenerico = *on;
197400100330           PosCurDAT02 = *on;
197500100330           V1dmsg      = $Msg(03);
197600100330           V1dmsg      = %trim(V1dmsg) + ' ' +
197700100330           'Non può essere maggiore di oggi';
197800100330           leavesr;
197900100330         ENDIF;
198000100330
198100090611         // controllo ora  esecuzione contatto
198200171222           If V02ora <> 0 ;
198300090611              sav02ora = %editc(v02ora:'X');
198400171222              If  V02ora < *zeros  or  ora > 23 or minuti > 59;
198500090611                   errMessage  = *on;
198600090611                   errGenerico = *on;
198700090611                   PosCurora02 = *on;
198800090611                   V1Dmsg = $Msg(05);
198900171222                   if  V02ora < *zeros;
199000171222                     V1Dmsg = %trimR( V1Dmsg ) + ' (negativa)';
199100171222                   endif;
199200090611                   leavesr;
199300090611              endif;
199400091104           else ;
199500091104              errMessage  = *on;
199600091104              errGenerico = *on;
199700091104              PosCurora02 = *on;
199800091104              V1Dmsg = $Msg(05);
199900091104              leavesr;
200000090611           endif;
200100100114
200200100416           endif ;
200300100114
200400091116
200500090611         // - controllo / ricerca Causale contatto
200600090611           If    %scan('?' : V02cae) > *zero;
200700090611             errGenerico = *on;
200800090611             PosCurCae02 = *on;
200900090611             clear  dCCO ;
201000090611             clear  Tntb74ds;
201100091218             iccouti = 'S';
201200100225          // imposto anche il tipo attività che sto eseguendo x selezionare le sole
201300100225          // causali possibili
201400100225             iccoese = imk19tat ;
201500100322          // imposto anche se sto eseguendo attività con il solo codice cliente oppure
201600100322          // potenziale mai codificato , e se c'è trattativa
201700110429          //   If v02cli > 0 ;
201800110429          //      iccocod = 'C' ;
201900110429          //   else ;
202000110429          //      if $Gia_codifica = *on ;
202100110429          //         iccocod = 'C' ;
202200110429          //      else ;
202300110429          //         iccocod = 'P' ;
202400110429          //      endif ;
202500110429          //   endif;
202600100322
202700110429          // imposto la cateoria del potenziale
202800110502               iccocod = cpofls ;
202900110429
203000100322             If v02vis > 0 ;
203100100322                iccotra = 'S' ;
203200100505                iccotpv = W_vistpv ;
203300100322             else ;
203400100322                iccotra = 'N' ;
203500100322             endif ;
203600100324           // verifico le offerte
203700100330             iccooff = 'N';
203800101220             setll v02vis tivof11l ;
203900101220             reade v02vis tivof11l ;
204000101220             dow not %eof(tivof11l) ;
204100100421                 if vofeso <> 'H' and vofeso <> '*';
204200100330                    iccooff = 'S';
204300100330                    leave ;
204400100330                 endif ;
204500101220                 reade v02vis tivof11l ;
204600100330             enddo ;
204700110412           // passo la causale al pgm di ricerca
204800100421                iccocco = atccad ;
204900100322
205000090611             TNTB74R  (kpjba : tntb74ds);
205100090611             if  occoerr = *off   ;
205200090611               V02cae = Occoke1;
205300091218               dcco   = Occouni;
205400090611               V02caed = §ccodes ;
205500090611               errGenerico = *on;
205600090611               leavesr;
205700090611             else;
205800090611               errMessage  = *on;
205900090611               errGenerico = *on;
206000090611               V1Dmsg = T02msg;
206100090611               leavesr;
206200090611             endif;
206300090611           endif;
206400090611
206500090611           If    V02cae <> *blanks  ;
206600090611             clear  dCCO;
206700090611             clear  TIBS02ds;
206800090611             T02mod = 'C';
206900090611             T02cod = 'CCO';
207000090611             T02ke1 = v02cae ;
207100090611             T02sif = KNSIF;
207200090611             TNTBE_RicercaControllo  (kpjba : tibs02ds);
207300090611             if  T02err = *blanks;
207400090611               dcco = t02uni ;
207500100225               §ccoTAAds = §CCOtaa;
207600100505               §ccoTPVds = §CCOtpv;
207700110502               §ccoCPOds = §CCOcpo;
207800090611               V02caed = §ccodes ;
207900090702           // verifico se causale contatto può essere utilizzata dall'utente
208000090702                If   §ccouti = 'N' ;
208100090702                   errMessage  = *on;
208200090702                   errGenerico = *on;
208300090702                   PosCurCae02 = *on;
208400090702                   V1Dmsg = $msg(14);
208500090702                   leavesr;
208600090702                endif;
208700090611             else;
208800090611               errMessage  = *on;
208900090611               errGenerico = *on;
209000090611               PosCurCae02 = *on;
209100090611               V1Dmsg = T02msg;
209200090611               leavesr;
209300090611             endif;
209400090611           else;
209500090611             errMessage  = *on;
209600090611             errGenerico = *on;
209700090611             PosCurCae02 = *on;
209800090611             V1Dmsg = $msg(08);
209900090611             leavesr;
210000090611           endif;
210100110405
210200110405           // verifico se causale può essere utilizzata in base al flag cliente che non spedisce
210300110405           If  $cli_non_sped = *on and §CCOucns = 'N' ;
210400110405                   errMessage  = *on;
210500110405                   errGenerico = *on;
210600110405                   PosCurCae02 = *on;
210700110502                   IF ATCcad <> '  ' ;
210800110502                      V1Dmsg = $msg(36);
210900110502                      %subst(V1Dmsg:12:2) = V02cae;
211000110502                      %subst(V1Dmsg:66:2) = ATCcad;
211100110502                   else ;
211200110502                      V1Dmsg = $msg(14);
211300110502                   Endif ;
211400110405                   leavesr;
211500110405           endif;
211600110502
211700110502           // verifico se causale può essere utilizzata in base al flag cliente che non spedisce
211800110502           If  $cli_non_sped = *off and §CCOucns = 'S' ;
211900110502                   errMessage  = *on;
212000110502                   errGenerico = *on;
212100110502                   PosCurCae02 = *on;
212200110502                   IF ATCcad <> '  ' ;
212300110502                      V1Dmsg = $msg(36);
212400110502                      %subst(V1Dmsg:12:2) = V02cae;
212500110502                      %subst(V1Dmsg:66:2) = ATCcad;
212600110502                   else ;
212700110502                      V1Dmsg = $msg(14);
212800110502                   Endif ;
212900110502                   leavesr;
213000110502           endif;
213100100421
213200110321         //?Controllo se la causale di esecuzione può essere utilizzata per l'attività che sto
213300110321         //?gestendo
213400110321           IF  §CCOcae <> *blanks and §CCOcae <> ATCcad;
213500110321             ErrMessage  = *on;
213600110321             ErrGenerico = *on;
213700110321             PosCurCae02 = *on;
213800110502             IF ATCcad <> '  ' ;
213900110502                V1Dmsg = $msg(36);
214000110502                %subst(V1Dmsg:12:2) = V02cae;
214100110502                %subst(V1Dmsg:66:2) = ATCcad;
214200110502             else ;
214300110502                V1Dmsg = $msg(14);
214400110502             Endif ;
214500110321             leavesr;
214600110321           ENDIF;
214700100421
214800110428           // verifico se causale da esitare è la 32 deve usare solo la 42
214900110428              If atccad = '32' and v02cae <> '42' ;
215000110428                errMessage  = *on;
215100110428                errGenerico = *on;
215200110428                PosCurCae02 = *on;
215300110428                V1Dmsg = $msg(33);
215400110428                leavesr;
215500110428              endif ;
215600110428
215700110428           // verifico se causale da esitare è la 71 deve usare solo la 72
215800110428              If atccad = '71' and v02cae <> '72' ;
215900110428                errMessage  = *on;
216000110428                errGenerico = *on;
216100110428                PosCurCae02 = *on;
216200110502                IF ATCcad <> '  ' ;
216300110502                   V1Dmsg = $msg(36);
216400110502                   %subst(V1Dmsg:12:2) = V02cae;
216500110502                   %subst(V1Dmsg:66:2) = ATCcad;
216600110502                else ;
216700110502                   V1Dmsg = $msg(14);
216800110502                Endif ;
216900110428                leavesr;
217000110428              endif ;
217100110428
217200100225           // verifico se causale può essere utilizzata per esitare questo tipo attività
217300100225           If  §CCOtaa <> *blanks and
217400100225                 %lookup(imk19tat : $Taa) = *zero ;
217500100225                   errMessage  = *on;
217600100225                   errGenerico = *on;
217700100225                   PosCurCae02 = *on;
217800100225                   V1Dmsg = $msg(23);
217900100225                   leavesr;
218000100225           endif;
218100100322
218200100322           // verifico se causale può essere utilizzata in presenza di trattative con offerte
218300100322           If  §CCOoff = 'S' and v02vis > 0 ;
218400101220                   setll v02vis tivof11l ;
218500100322                   If not %equal ;
218600100322                      errMessage  = *on;
218700100322                      errGenerico = *on;
218800100322                      PosCurCae02 = *on;
218900100322                      V1Dmsg = $msg(31);
219000100322                      leavesr;
219100100421                   else ;
219200101220                      reade v02vis tivof11l ;
219300101220                      dow not %eof(tivof11l) ;
219400100421                          if vofeso  =  '*' ;
219500100421                             errMessage = *on ;
219600100421                          else ;
219700100421                             errMessage = *off;
219800100421                             leave ;
219900100421                          endif ;
220000101220                      reade v02vis tivof11l ;
220100100421                      enddo ;
220200100421                   // se ho trovato tutti annullati emetto errore
220300100421                      If errMessage  = *on;
220400100421                         errGenerico = *on;
220500100421                         PosCurCae02 = *on;
220600100421                         V1Dmsg = $msg(31);
220700100421                         leavesr;
220800100421                      endif ;
220900100322                   endif ;
221000100322           endif;
221100100322
221200100322           // verifico se causale non può essere utilizzata in presenza di trattative con offerte
221300100225           If  §CCOoff = 'N' and v02vis > 0 ;
221400101220                   setll v02vis tivof11l ;
221500101220                   reade v02vis tivof11l ;
221600101220                   dow not %eof(tivof11l) ;
221700100421                       if vofeso <> 'H' and vofeso <> '*' ;
221800100309                          errMessage  = *on;
221900100309                          errGenerico = *on;
222000100309                          PosCurCae02 = *on;
222100100309                          V1Dmsg = $msg(24);
222200100309                          leavesr;
222300100309                       endif ;
222400101220                   reade v02vis tivof11l ;
222500100309                   enddo ;
222600100225           endif;
222700100505           // verifico se causale in presenza di trattativa può essere usata per solo un
222800100505           // alcuni tipi trattativa
222900100505           If      §ccotpv <> *blanks  and %lookup(W_vistpv : $tpv) = *zero ;
223000100505                          errMessage  = *on;
223100100505                          errGenerico = *on;
223200100505                          PosCurCae02 = *on;
223300100505                          V1Dmsg = %trimr($msg(35)) + ' ' + W_vistpv ;
223400100505                          leavesr;
223500100505           endif ;
223600100308
223700100308           // verifico se causale può essere utilizzata solo per codificati/mai codificati o tutti
223800100308
223900100308           // solo causali per clienti codificati
224000110502           // If  §CCOcod = 'C' and V02cli = *zeros ;
224100110502           If  §CCOcpo <> *blanks and %lookup(cpofls:$cpo)= 0 ;
224200100308               errMessage  = *on;
224300100308               errGenerico = *on;
224400100308               PosCurCae02 = *on;
224500100308               V1Dmsg = $msg(25);
224600110504               v1dmsg = %subst(V1Dmsg:  1: 40) + '"' + §cpodesc + '"' ;
224700100308               leavesr;
224800100308           endif ;
224900090611
225000100308
225100100308           // verifico se causale non deve essere legata a trattativa
225200100308           If  §CCOtra = 'N' and V02vis >  *zeros ;
225300100308               errMessage  = *on;
225400100308               errGenerico = *on;
225500100308               PosCurCae02 = *on;
225600100308               V1Dmsg = $msg(27);
225700100308               leavesr;
225800100308           endif;
225900100308
226000100309            // verifico se causale chiude trattativa e l'attività è legata a trattativa
226100100309             if  §ccoCHT = 'S' and v02vis > 0 ;
226200100520                   chain(n)  V02vis tivis05l;
226300100308                   // se trattativa già esitata errore
226400100308                   IF  %found(tivis05l) and visesi = 'P ' ;
226500100308                       errMessage  = *on;
226600100308                       errGenerico = *on;
226700100308                       PosCurCae02 = *on;
226800100308                       V1Dmsg = $msg(28);
226900100308                       leavesr ;
227000100308                   else ;
227100100308                       $chiedi_conf = *on ;
227200100308                   endif ;
227300100308             endif ;
227400090611
227500091118            // pulisco il flag della trattativa nuova se non devo aprire trattativa
227600091118                if §ccoapt = ' ' ;
227700091118                    $new_trattati = *off ;
227800091118                endif ;
227900100308            // verifico se causale vuole le note
228000100308            If §cconot = 'S' and v02no1 = *blanks and v02no2 = *blanks ;
228100100308                   errMessage  = *on;
228200100308                   errGenerico = *on;
228300100308                   PosCurNo102 = *on;
228400100308                   V1Dmsg = $msg(29);
228500100308                   leavesr;
228600100308            endif ;
228700100318
228800100318            // verifico se causale non deve essere legata a trattativa do errore
228900100520                if    §ccotra = 'S' and v02vis = 0 and §ccoapt <> 'S';
229000100318                   errMessage  = *on;
229100100318                   errGenerico = *on;
229200100318                   PosCurCae02 = *on;
229300100318                   V1Dmsg = $msg(30);
229400100318                   leavesr;
229500100318                endif ;
229600100318
229700100329
229800100329            // verifico se causale legata a trattativa, se non è un'attività con trattativa
229900100329            // la creo nuova
230000100329                if    (§ccotra = 'S' and v02vis = 0) or
230100100329                      (§ccotra <>'S' and v02vis = 0) ;
230200100329                  clear newnrv;
230300100329                  clear sav_newnrv;
230400100329                endif ;
230500100329
230600100318            // verifico se causale legata a trattativa, se non è un'attività con trattativa
230700100318            // la creo nuova altrimenti propongo la trattativa dell'attività
230800100318                if    §ccotra = 'S' and v02vis > 0 ;
230900100318                      newnrv = v02vis   ;
231000100318                      sav_newnrv = v02vis   ;
231100100318                      leavesr;
231200100318                endif ;
231300100118
231400100118            // verifico se causale apre sempre trattativa e se ce nè già do errore
231500100118                if    §ccoapt = 'S' and v02vis > 0 ;
231600100118                   errMessage  = *on;
231700100118                   errGenerico = *on;
231800100118                   PosCurCae02 = *on;
231900100118                   V1Dmsg = $msg(19);
232000100118                   leavesr;
232100100118                endif ;
232200100118
232300100118            // se passati tutti i controlli e devo aprire trattativa recupero
232400100118            // il numero trattativa
232500100118            // se non avevo già salvato prima il n. recuperato
232600100202            if newnrv = 0 and §ccoapt = 'S'  and sav_newnrv = 0;
232700091113            exsr Sr_Recnum ;
232800091113            if  $ErrGrave = *on;
232900091113                leavesr;
233000091113            endif;
233100091113            endif;
233200100118
233300100118            if sav_newnrv > 0 and §ccoapt = 'S';
233400100118              newnrv = sav_newnrv;
233500100118              $new_trattati = sav_newtrat;
233600100118            endif;
233700100118
233800090611       ENDSR;
233900090611
234000091113       //--------------------------------------------------------------
234100091113       //?Recupero numero trattativa nuova
234200091113       //--------------------------------------------------------------
234300091113       BEGSR sr_Recnum;
234400091113
234500100413                reset $NumOk ;
234600100413
234700100413                RESET trul48ds;
234800100413                trul48r(kpjba:trul48ds);
234900100413
235000100413                If o48Err = 0;
235100100413                   newnrv  = o48nrv;
235200100413                   sav_newnrv  = o48nrv;
235300100413                   $NumOk = *on ;
235400100413                else ;
235500100413                   $ErrGrave   = *on;
235600100413                   errMessage  = *on;
235700100413                   errGenerico = *on;
235800100413                   V1Dmsg = o48msg ;
235900100413                   leavesr;
236000100413                Endif;
236100100413
236200091116        // se trovato un nuovo numero lo segnalo
236300091116                 If   $NumOk = *on ;
236400091116                   $New_trattati = *on ;
236500100118                   sav_newtrat = *on ;
236600091116                 endif ;
236700091113
236800091113       endsr;
236900091113
237000090611       //--------------------------------------------------------------
237100090611       //?Controllo videata D02         SCADENZA ATTIVITA'
237200090611       //--------------------------------------------------------------
237300090611       BEGSR CtrD02Scd ;
237400090611
237500090611         // controllo data attività da fare
237600171221           If    V02scd  > 0;
237700090611             clear wlbdat;
237800090611             g02dat = v02scd;
237900090611             xsrda8(wlbdat);
238000090611             if g02err = '1';
238100090611               errMessage  = *on;
238200090611               errGenerico = *on;
238300090611               PosCurScd02 = *on;
238400090611               V1Dmsg = $Msg(03);
238500090611               leavesr;
238600090611             endif;
238700090611
238800090611             v02Scd = g02dat;
238900090611             w02Scd = g02inv;
239000090611           else ;
239100090611             errMessage  = *on;
239200090611             errGenerico = *on;
239300090611             PosCurScd02 = *on;
239400090611             V1Dmsg = $Msg(03);
239500171222             if  V02scd < *zeros;
239600171222               V1Dmsg = %trimR( V1Dmsg ) + ' (negativa)';
239700171222             endif;
239800090611             leavesr;
239900090611           endif;
240000100118
240100100118         // controllo che la data non sia inferiore al 2000 o superiore al 2039
240200100118         IF  w02scd < 20000101 or w02scd > 20391231;
240300100118           errMessage  = *on;
240400100118           errGenerico = *on;
240500100118           PosCurscd02 = *on;
240600100118           V1Dmsg = $Msg(03);
240700100118           leavesr;
240800100118         ENDIF;
240900090611
241000090611         // controllo ora  esecuzione contatto
241100171221           If V02Sco <> 0 ;
241200090611              sav02ora = %editc(v02Sco:'X');
241300171221              If  V02sco < *zero  or  ora > 23 or minuti > 59;
241400090611                   errMessage  = *on;
241500090611                   errGenerico = *on;
241600090611                   PosCurSco02 = *on;
241700090611                   V1Dmsg = $Msg(05);
241800171222                   if  V02sco < *zeros;
241900171222                     V1Dmsg = %trimR( V1Dmsg ) + ' (negativa)';
242000171222                   endif;
242100090611                   leavesr;
242200090611              endif;
242300090611           endif;
242400090611
242500090611         // Controllo codice commerciale (? Ricerca)
242600090611           If    %scan('?' : v02cmi) > 0  ;
242700090611             errGenerico = *on;
242800090611             PosCurCmi02 = *on;
242900130809             iMK43fil  = DUTpou;
243000130809             TRMK43R (kpjba : TRMK43ds);
243100130809             if  oMK43err = *off  and  oMK43fxx = *blank;
243200130809               v02cmi  = %editc( oMK43cmm : 'X' );
243300130809               v02cmid = oMK43des;
243400130809             endif;
243500090611             leavesr;
243600090611           endif;
243700100210
243800100210         // Commerciale che esegue il contatto obbligatorio
243900100210           If  v02cmi = *blanks or v02cmi = *zeros;
244000100210             errMessage  = *on;
244100100210             errGenerico = *on;
244200100210             PosCurCmi02 = *on;
244300100210             V1Dmsg = $Msg(02);
244400100210             leavesr;
244500100210           endif;
244600090611
244700090611         // Controllo codice commerciale che esegue il contatto
244800100210             IF  %check(digits:V02cmi) > 0;
244900100210               ErrMessage  = *on;
245000100210               ErrGenerico = *on;
245100100210               PosCurCmi02 = *on;
245200100210               V1Dmsg      = $Msg(02);
245300100210               leavesr;
245400100210             ENDIF;
245500100210
245600100210
245700100210           //?utente abilitato al commerciale
245800100721           //?lo controllo solo se attività NON legata a trattative e NON su cliente
245900100721             IF  V02vis <> 0 or V02cli <> 0;
246000100210             clear TNTAA1DS;
246100120810             ITAA1tipo = 'M';
246200120810             ITAA1caut = '§utegtc';
246300100210             ITAA1cmm  = %dec(V02cmi:7:0);
246400100519             ITAA1abi = 'RA';
246500100210             kpjbu     = tntaa1ds;
246600100210             tntaa1c (kpjba);
246700100210             TNTAA1DS = kpjbu;
246800100210             IF  OTAA1fabi = 'N';
246900100210               ErrMessage  = *on;
247000100210               ErrGenerico = *on;
247100100210               PosCurCmi02 = *on;
247200100210               V1Dmsg      = $Msg(02);
247300100210               leavesr;
247400100210             ENDIF;
247500100721             ENDIF;
247600100210
247700130809             CMMcod = %int(V02cmi);
247800130809             chain (CMMcod) AZCMM000;
247900130809             if  NOT  %found(AZCMM01L);
248000090611               errMessage  = *on;
248100090611               errGenerico = *on;
248200090611               PosCurCmi02 = *on;
248300090611               V1Dmsg = $Msg(02);
248400090611               leavesr;
248500100210             endif;
248600130809             V02cmid = CMMdes;
248700110525
248800130809             If  CMMPar <> ' ' ;
248900110525               ErrMessage  = *on;
249000110525               ErrGenerico = *on;
249100110525               PosCurCmi02 = *on;
249200110525               V1Dmsg      = $Msg(02);
249300110525              leavesr;
249400110525             ENDIF;
249500111223
249600111223       //?Commerciale "valido" data inizio e fine attività
249700130809         IF  CMMdtIni > wOggi  or
249800130809             CMMdtFin < wOggi;
249900111223           ErrMessage = *on;
250000111223           ErrGenerico = *on;
250100111223           PosCurCmi02 = *on;
250200111223           V1Dmsg = $Msg(02);
250300111223           leavesr;
250400111223         ENDIF;
250500100317
250600100317
250700100317         // Controllo se causale vuole controllo su commerciale interno
250800100318          // imposto flag di forzabile
250900100318               If sav_v02cmi2 <> v02cmi ;
251000100318                  $forzacomin2 = *off ;
251100100318               endif ;
251200130809            If §ccocoi <> ' ' and (CMMfun =  'COMIN' or CMMfun = 'ASC'
251300130809                                                     or CMMfun = 'SA');
251400100317         // verifico se msg forzabile
251500100318               If §ccocoi = 'F' and $forzacomin2= *off ;
251600100317                  errMessage  = *on;
251700100317                  errGenerico = *on;
251800100318                  PosCurCmi02 = *on;
251900100318                  V1Dmsg = %trimr($Msg(06)) + ' Enter per forzare' ;
252000100318                  $forzacomin2= *on ;
252100100318                  sav_v02cmi2 = v02cmi ;
252200100317                  leavesr;
252300100318               Endif;
252400100318               If §ccocoi <> 'F' ;
252500100317                  errMessage  = *on;
252600100317                  errGenerico = *on;
252700100318                  PosCurCmi02 = *on;
252800100318                  V1dmsg = $Msg(06) ;
252900100317                  leavesr;
253000100317               Endif ;
253100100317             endif;
253200100126
253300100330
253400100330       // controllo che la data non sia inferiore a 3 gg lavorativi da oggi
253500100330         clear XGIOLAVDS;
253600100330         IXGLdata = %dec(%date());
253700100330         IXGLsub  = 'S';
253800100330         IXGLgg   = 3;
253900100330         IXGLpa   = 'P';
254000100330         IXGLlav  = 'S';
254100100330         IXGLfil = %dec(%subst(V02cmi:1:3):3:0);
254200100330         XGIOLAV (xgiolavds);
254300100330         IF  w02scd < OXGLdata;
254400100330           ErrMessage  = *on;
254500100330           ErrGenerico = *on;
254600100330           PosCurScd02 = *on;
254700100330           V1dmsg      = $Msg(03);
254800100330           V1dmsg      = %trim(V1dmsg) + ' ' +
254900100330           'Non può essere inferiore a 3 gg.lavorativi';
255000100330           leavesr;
255100100330         ENDIF;
255200100330
255300100330       // controllo che la data non sia maggiore a 14 mesi  da oggi
255400100330         clear XGIOLAVDS;
255500100330         IXGLdata = %dec(%date());
255600100330         IXGLadd  = 'S';
255700100330         IXGLmm   = 14;
255800100330         IXGLpa   = 'P';
255900100330         IXGLfil = %dec(%subst(V02cmi:1:3):3:0);
256000100330         XGIOLAV (xgiolavds);
256100100330         IF  w02scd > OXGLdata;
256200100330           ErrMessage  = *on;
256300100330           ErrGenerico = *on;
256400100330           PosCurScd02 = *on;
256500100330           V1dmsg      = $Msg(03);
256600100330           V1dmsg      = %trim(V1dmsg) + ' ' +
256700100330           'Non può essere maggiore di 14 mesi';
256800100330           leavesr;
256900100330         ENDIF;
257000100330
257100100330
257200090804         // In caso di appuntamento controllo anche i campi seguenti
257300090804
257400090804       If Appuntamento = *on ;
257500090804
257600100310         //?Verifico se il commerciale dell'attività è diverso dal
257700100310         //?commerciale della trattativa
257800100310         //?se attività legata a trattativa
257900100310         // solo se si tratta di appuntamento controllo il commerciale
258000100310         // inoltre se vengo da trattativa il messaggio è forzabile
258100100310         IF  vis_trattativ  ;
258200100310           clear tnta45ds ;
258300100310           ITA45tip = 'C';
258400100310           ITA45nrv = v02vis;
258500100310           ITA45cmm = %dec(v02cmi:7:0);
258600100310           tnta45r (tnta45ds) ;
258700100310           IF  OTA45err = 'C';
258800100311          // imposto flag di forzabile
258900100311               If sav_v02cmi <> v02cmi ;
259000100311                  $forzacomm = *off ;
259100100311               endif ;
259200100310       // se vengo da trattativa il msg è forzabile
259300100310             If $forzacomm = *off ;
259400100310                ErrMessage  = *on;
259500100310                ErrGenerico = *on;
259600100310                PosCurCmi02 = *on;
259700100310                If imk19fpr = 'T' ;
259800100310                   V1Dmsg = %trimr(OTA45msg) + ' Enter per forzare';
259900100310                   $forzacomm = *on ;
260000100311                   sav_v02cmi = v02cmi ;
260100100310                else ;
260200100310                   V1Dmsg = OTA45msg;
260300100310                leavesr;
260400100310                Endif ;
260500100310             Endif ;
260600100310           ENDIF;
260700100310         ENDIF;
260800100310
260900090804        // verifico l'inserimento delle ore di impegno
261000090804
261100090804         // controllo ora  inizio impegno
261200171222           If V02oii  <> 0 ;
261300090804              sav02ora = %editc(V02oii:'X');
261400171222              If  V02oii < *zeros  or  ora > 23 or minuti > 59;
261500090804                   errMessage  = *on;
261600090804                   errGenerico = *on;
261700090804                   PosCurOii02 = *on;
261800090804                   V1Dmsg = $Msg(05);
261900171222                   if  V02oii < *zeros;
262000171222                     V1Dmsg = %trimR( V1Dmsg ) + ' (negativa)';
262100171222                   endif;
262200090804                   leavesr;
262300090804              endif;
262400091026           else ;
262500091026              errMessage  = *on;
262600091026              errGenerico = *on;
262700091026              PosCurOii02 = *on;
262800091111              V1Dmsg = $Msg(17);
262900091026              leavesr;
263000090804           endif;
263100090804
263200090804         // controllo ora  fine   impegno
263300171222           If V02ofi <> 0 ;
263400090804              sav02ora = %editc(V02ofi:'X');
263500171222              If  V02ofi < *zeros  or  ora > 23 or minuti > 59;
263600090804                   errMessage  = *on;
263700090804                   errGenerico = *on;
263800090804                   PosCurOfi02 = *on;
263900090804                   V1Dmsg = $Msg(05);
264000171222                   if  V02ofi < *zeros;
264100171222                     V1Dmsg = %trimR( V1Dmsg ) + ' (negativa)';
264200171222                   endif;
264300090804                   leavesr;
264400090804              endif;
264500091026           else ;
264600091026              errMessage  = *on;
264700091026              errGenerico = *on;
264800091026              PosCurOfi02 = *on;
264900091111              V1Dmsg = $Msg(17);
265000091026              leavesr;
265100090804           endif;
265200090804
265300090804         // controllo ora  inizio se congruente con fine impegno
265400090804           If V02oii > 0 and V02ofi = 0;
265500090804              errMessage  = *on;
265600090804              errGenerico = *on;
265700090804              PosCurOfi02 = *on;
265800090804              V1Dmsg = $Msg(13);
265900090804              leavesr;
266000090804           endif;
266100090804
266200090804         // controllo ora  inizio se congruente con fine impegno
266300090804           If V02oii > 0 and V02ofi > 0 and V02oii > V02ofi ;
266400090804              errMessage  = *on;
266500090804              errGenerico = *on;
266600090804              PosCurOfi02 = *on;
266700090804              V1Dmsg = $Msg(13);
266800090804              leavesr;
266900090804           endif;
267000090804
267100090804         // verifico se inizioe fine impegno è congruente con ora appuntamento
267200090804           If V02oii > 0 and V02ofi > 0 and (V02oii > V02sco or
267300090804              V02ofi < V02sco) ;
267400090804              errMessage  = *on;
267500090804              errGenerico = *on;
267600090804              PosCurOfi02 = *on;
267700090804              V1Dmsg = $Msg(15);
267800090804              leavesr;
267900090804           endif ;
268000091216
268100091216         //?Verifico se il commerciale è libero
268200100322         // gli passo anche l'attività del momento altrimenti non la esclude
268300091216           clear trmk84ds;
268400091216           IMK84cmm = %dec(v02cmi:7:0);
268500091216           IMK84dad = w02scd;
268600091216           IMK84oii = v02oii;
268700091216           IMK84ofi = v02ofi;
268800100322           IMK84tat = imk19tat ;
268900100322           IMK84atn = imk19atn ;
269000100322           IMK84atnp= imk19atnp;
269100091216           trmk84r (trmk84ds);
269200091216         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
269300091216           IF  OMK84err <> *blanks and OMK84err <> '9' and
269400091216               Not $ForzaImpegno;
269500091216             ErrGenerico = *on;
269600091216             ErrMessage  = *on;
269700091216             PosCurScd02 = *on;
269800091216             V1Dmsg = OMK84msg;
269900091216             $ForzaImpegno = *on;
270000091216             leavesr;
270100091216           ENDIF;
270200090611
270300090804       endif;
270400090804
270500090611       ENDSR ;
270600090611
270700090611       //--------------------------------------------------------------
270800090611       //?Gestione videata W03
270900090611       //--------------------------------------------------------------
271000090611       BEGSR GesW03;
271100090611
271200090611         // Inizializzazione videata
271300090611         if $InzW03 = *on;
271400090611           exsr InzW03;
271500090611           $InzW03 = *off;
271600090611         endif;
271700090611
271800090611
271900090611         // Emissione videata
272000090611         exfmt MK19W03;
272100090611         errMessage   = *off;
272200090611         errGenerico  = *off;
272300090611         clear W03msg;
272400090611
272500090611         select;
272600090611         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
272700090611           when  $ErrGrave = *on;
272800090611             $Fine  = *on;
272900100927
273000100927       //?F02 = Rubrica
273100100927           WHEN dsp_aid = c_F02;
273200100927             exsr F02D02;
273300100927             leavesr;
273400100927
273500090611         // F12 = Ritorno
273600090611           when dsp_aid = c_F12;
273700090611             exsr F12W03;
273800090611
273900091026         // F11 = Agenda
274000091026           when dsp_aid = c_F11;
274100091027             exsr F11w03;
274200090611
274300090611         // Enter
274400090611           other;
274500090611           exsr CtrW03;
274600090611           if errGenerico = *on ;
274700090611               leavesr;
274800090611           endif;
274900090611         endsl;
275000100329
275100100329         // se dato f8 conferma (solo in caso di appuntamento )
275200100329           If   dsp_aid = c_F08 ;
275300100329                If V02rtr = *blanks or v02mrt = *blanks ;
275400100329                   errMessage  = *on;
275500100329                   errGenerico = *on;
275600100329                   PosCurCom03 = *on;
275700100329                   W03msg = $Msg(32);
275800100329                   leavesr;
275900100329                 endif;
276000100329           endif ;
276100100329
276200100329
276300090611
276400100329         // se dato f6 conferma  / f8 conferma + mail appuntamento
276500100309
276600100309         // se dato f6 sulla videata di creazione nuova attività e non solo invio  lettera
276700100920           If    dsp_aid = c_F06  or
276800100920                 dsp_aid = c_F08  ;
276900090611                   exsr f06w03d02;
277000091210                   IF  OMK22f12 = 'S';
277100091210                     leavesr;
277200091210                   ENDIF;
277300100309
277400100309         // comitto se richiesto e se non ci sono errori
277500100309                 If   imk19cmt = 'S' and omk19err = ' ';
277600100309                      commit ;
277700100309                 endif ;
277800100115
277900100921         // se F6
278000100115           // devo aggiornare il commerciale sul potenziale nel caso non sia già
278100100115           // impostato
278200100517                chain(n)  atccpo  TNCPO01L;
278300100610                IF  not %found(TNCPO01L) or
278400100610                    (%found(tncpo01l) and cpocmm > 0) ;
278500100115                  leavesr;
278600100115                ENDIF;
278700100517           // se devo aggiornare il commerciale aggancio in aggiornamento
278800100517           // il file tncpo e ne verifico l'allocazione
278900100517                chain(e) atccpo TNCPO01l ;
279000100517             If not %error;
279100160804             //?Salvo l'immagine precedente del potenziale
279200160804               exsr ImmaginePrima;
279300100920                Cpocmm = %dec(v02com:7:0);
279400100115                Cpoflt = %dec(%subst(%editc(cpocmm:'X'):1:3):3:0);
279500100115                update TNCPO000;
279600160804             //?Scrivo la variazione fatta sul potenziale
279700160804               exsr ScriviVariazione;
279800100517             ENDIF;
279900100329
280000100329           endif;
280100100329
280200090611       ENDSR;
280300090611
280400090611
280500090611       //--------------------------------------------------------------
280600090611       //?Caricamento videata conferma W03
280700090611       //--------------------------------------------------------------
280800090611       BEGSR InzW03;
280900090611
281000090611          clear mk19w03 ;
281100090611
281200090611             W03cae = v02cae ;
281300090611             W03caed= v02caed;
281400091113             If newnrv > 0 ;
281500091113                w03nrv = newnrv ;
281600091113             else ;
281700091113                W03nrv = v02vis ;
281800091113             endif ;
281900100311         // se la causale chiude evntualmente la  trattativa non lego il numero trattativa
282000100311         // all'attività futura
282100100311             If §ccocht = 'S' and w03nrv > 0 ;
282200100311                clear w03nrv ;
282300100311             endif ;
282400091113
282500091113             trattativa = (w03nrv > 0);
282600091118        // Se trattativa nuova evidenzio
282700091118             Nuova = ($new_trattati = *on and trattativa = *on) ;
282800090611
282900091021       // in caso di futuro contatto appuntamento non propongo nulla
283000091021        If §ccotat <> 'A' ;
283100091021
283200100924       // calcolo data scadenza se segno causale <> da '='
283300100924            If §ccoupm = '+' or §ccoupm = '-' ;
283400100924               clear XGIOLAVDS;
283500100924               IXGLdata = %dec(%date());
283600100924               If §ccoupm = '+' ;
283700100924                  IXGLadd  = 'S';
283800100924               Else ;
283900100924                  IXGLsub  = 'S';
284000100924               Endif ;
284100100924               IXGLgg   = §CCOgio;
284200100924               IXGLpa   = 'P';
284300100924               If  V02com > *zeros;
284400100924                  IXGLfil = %dec(%subst(V02com:1:3):3:0);
284500100924               else ;
284600100924                  IXGLfil = DUTpou;
284700100924               Endif;
284800100924               XGIOLAV (xgiolavds);
284900100924               If  OXGLerr = *blanks;
285000100924                   data_eur = %date(OXGLdata:*iso);
285100100924                   W03dsc   = %dec(data_eur);
285200100924               Endif;
285300100924            Endif ;
285400100924       // calcolo data scadenza se segno causale = ad '='
285500100924            If §ccoupm = '=' ;
285600101111                   W03dsc   = *date;
285700100924            Endif ;
285800090611
285900090611       // propongo l'ora del momento
286000090611             w03osc = %dec(%subst(%EDITC(%dec(%time()):'X'):1:4):4:0);
286100090611
286200090611       // propongo lo stesso commerciale che ha eseguito il contatto
286300090611             w03com  = v02com  ;
286400090611             w03comd = v02comd ;
286500091021
286600091021        endif ;
286700091021
286800100326       // in caso di futuro contatto appuntamento propongo commerciale trattativa
286900100326        If §ccotat = 'A' ;
287000100326       // propongo lo stesso commerciale della trattativa
287100100329       // richiamo il tnta45 con com mercilae 9999999 così mi torna il commerciale trattativa
287200100326           clear tnta45ds ;
287300100326           ITA45tip = 'C';
287400100326           ITA45nrv = v02vis;
287500100329           ITA45CMM = 9999999 ;
287600100326           tnta45r (tnta45ds) ;
287700100326           IF  OTA45err = 'C';
287800100326             w03com  = %editc(ota45cmm:'X')  ;
287900130809             clear  w03comD;
288000130809             chain (oTA45cmm) AZCMM000;
288100130809             if  %found(AZCMM01L);
288200130809               w03comd = CMMdes;
288300130809             endif;
288400100326           Endif ;
288500100326
288600100326        Endif ;
288700100326
288800090630       // verifico se il prossimo contatto ha una tipologia telefonata o appuntamento
288900100210             prox_telefon= (§ccotat = 'T' or §ccotat = 'O');
289000091015             prox_appunta= (§ccotat = 'A');
289100101221
289200101221       //?Se impostata la data caldo senza offerta la devo proporre
289300101221           IF  sav_DataHot <> 0;
289400101221             data_eur = %date(sav_DataHot:*iso);
289500101221             W03dsc   = %dec(data_eur);
289600101221           ENDIF;
289700091210
289800091210         //?imposto fisso 'N' per affiancamento
289900091210         W03aff = 'N';
290000100504
290100100504       //?Se appuntamento e commerciale già presente posiziono il cursore sulla data
290200100504         IF  prox_appunta and w03com > *zeros;
290300100504           PosCurDsc03 = *on;
290400100504         ENDIF;
290500091216
290600091216         $ForzaImpegno = *off;
290700090630
290800090611       ENDSR;
290900090611
291000090611       //--------------------------------------------------------------
291100090611       //?Controllo videata W03
291200090611       //--------------------------------------------------------------
291300090611       BEGSR CtrW03;
291400090611
291500090611            WindDspF = IndDspF;
291600090611            reset WindDspFb;
291700090611            IndDspF  = WindDspF;
291800090611
291900090611         // Controllo codice commerciale (? Ricerca)
292000090611           If    %scan('?' : W03com) > 0  ;
292100090611             errGenerico = *on;
292200090611             PosCurCom03 = *on;
292300130809             clear  TRMK43ds;
292400130809             iMK43fil  = DUTpou;
292500130809             TRMK43R (kpjba : TRMK43ds);
292600130809             if  oMK43err = *off  and  oMK43fxx = *blank;
292700130809               W03com  = %editc( oMK43cmm : 'X' );
292800130809               W03comd = oMK43des;
292900130809             endif;
293000090611             leavesr;
293100090611           endif;
293200090611
293300100210         // Commerciale che esegue il contatto obbligatorio
293400100210           If  W03com = *blanks or w03com = *zeros;
293500100210             errMessage  = *on;
293600100210             errGenerico = *on;
293700100210             PosCurCom03 = *on;
293800100210             W03msg = $Msg(02);
293900100210             leavesr;
294000100210           endif;
294100100210
294200090611         // Controllo codice commerciale che esegue il contatto
294300100210             IF  %check(digits:w03com) > 0;
294400100210               ErrMessage  = *on;
294500100210               ErrGenerico = *on;
294600100210               PosCurCom03 = *on;
294700100210               W03msg = $Msg(02);
294800100210               leavesr;
294900100210             ENDIF;
295000100210
295100100210           //?utente abilitato al commerciale
295200100210             clear TNTAA1DS;
295300120810             ITAA1tipo = 'M';
295400120810             ITAA1caut = '§utegtc';
295500100210             ITAA1cmm  = %dec(w03com:7:0);
295600100519             ITAA1abi = 'RA';
295700100210             kpjbu     = tntaa1ds;
295800100210             tntaa1c (kpjba);
295900100210             TNTAA1DS = kpjbu;
296000100210             IF  OTAA1fabi = 'N';
296100100210               ErrMessage  = *on;
296200100210               ErrGenerico = *on;
296300100210               PosCurCom03 = *on;
296400100210               w03msg      = $Msg(02);
296500100210               leavesr;
296600100210             ENDIF;
296700100210
296800130809             CMmcod = %int(W03com);
296900130809             chain (CMMcod) AZCMM000;
297000130809             if  NOT  %found(AZCMM01L);
297100090611               errMessage  = *on;
297200090611               errGenerico = *on;
297300090611               PosCurCom03 = *on;
297400090611               W03msg = $Msg(02);
297500090611               leavesr;
297600100210             endif;
297700130809             W03comd = CMMdes;
297800100311
297900130809             If  CMMpar <> ' ' ;
298000110525               ErrMessage  = *on;
298100110525               ErrGenerico = *on;
298200110525               PosCurCom03 = *on;
298300110525               W03msg = $Msg(02);
298400110525              leavesr;
298500110525             ENDIF;
298600111223
298700111223       //?Commerciale "valido" data inizio e fine attività
298800130809         IF  CMMdtIni > wOggi  or
298900130809             CMMdtFin < wOggi;
299000111223           ErrMessage = *on;
299100111223           ErrGenerico = *on;
299200111223           PosCurCom03 = *on;
299300111223           W03msg = $Msg(02);
299400111223           leavesr;
299500111223         ENDIF;
299600100310
299700100318            If sav_w03com <> w03com ;
299800100318               $forzacomin = *off ;
299900100318            endif ;
300000100310         // Controllo se causale vuole controllo su commerciale interno
300100130809            If §ccocoi <> ' ' and (CMMfun =  'COMIN' or CMMfun = 'ASC'
300200130809                                                     or CMMfun = 'SA');
300300100310         // verifico se msg forzabile
300400100310               If §ccocoi = 'F' and $forzacomin = *off ;
300500100310                  errMessage  = *on;
300600100310                  errGenerico = *on;
300700100310                  PosCurCom03 = *on;
300800100310                  W03msg = %trimr($Msg(06)) + ' Enter per forzare' ;
300900100310                  $forzacomin = *on ;
301000100311                  sav_w03com = w03com ;
301100100310                  leavesr;
301200100318               Endif ;
301300100318               If §ccocoi <>'F' ;
301400100310                  errMessage  = *on;
301500100310                  errGenerico = *on;
301600100310                  PosCurCom03 = *on;
301700100310                  W03msg = $Msg(06) ;
301800100310                  leavesr;
301900100310               Endif ;
302000100310             endif;
302100100202
302200100202         //?Se sto pianificando una nuova attività che apre trattativa
302300100202         //?devo controllare che non ci sia già una trattativa aperte
302400100202         //?per lo stesso potenziale e commerciale
302500100202           IF  §CCOapt = 'S' and $New_Trattati;
302600100202             clear tnta43ds ;
302700100202             ITA43cpo = cpocpo;
302800100202             ITA43ksc = v02cli;
302900100202             ITA43nrv = w03nrv;
303000100202             ITA43cmm = %dec(w03com:7:0);
303100100202             tnta43r (TNTA43DS) ;
303200100202             IF  OTA43err = '+';
303300100202               ErrMessage  = *on;
303400100202               ErrGenerico = *on;
303500100202               PosCurCom03 = *on;
303600100202               W03msg = OTA43msg;
303700100202               leavesr;
303800100202             ENDIF;
303900100202           ENDIF;
304000100326       If Prox_appunta = *on ;
304100100326
304200100326         //?Verifico se il commerciale dell'appuntamento è diverso dal
304300100326         //?commerciale della trattativa
304400100422
304500100422            If sav_w03comt <> w03com ;
304600100422               $forzacomm3 = *off ;
304700100422            endif ;
304800100326         clear tnta45ds ;
304900100326         ITA45tip = 'C';
305000100326         ITA45nrv = w03nrv;
305100100326         ITA45cmm = %dec(w03com:7:0);
305200100326         tnta45r (tnta45ds) ;
305300100326         IF  OTA45err = 'C';
305400100422       // se vengo da trattativa il msg è forzabile
305500100422             If $forzacomm3= *off ;
305600100422                ErrMessage  = *on;
305700100422                ErrGenerico = *on;
305800100422                PosCurCom03 = *on;
305900100422                If imk19fpr = 'T' ;
306000100422                   W03msg = %trimr(OTA45msg) + ' Enter per forzare';
306100100422                   $forzacomm3= *on ;
306200100422                   sav_w03comt= w03com ;
306300100422                else ;
306400100422                   w03msg = OTA45msg;
306500100422                leavesr;
306600100422                Endif ;
306700100422             endif ;
306800100326         ENDIF;
306900100326         ENDIF;
307000100326
307100100504         //?Verifico se LA FILIALE DI APPARTENENZA DEL COMMERCIALE E' UGUALE
307200100504         //?ALLA FILIALE DI APPARTENENZA DEL COMMERCIALE CLIENTE SE ESISTE
307300100504         //?E SE E' PRIMO APPUNTAMENTO
307400100504
307500100504            If %SUBST(w03com: 1: 3) <> %subst(%editc(v02cmp:'X'): 1:3) and
307600100504               $new_trattati = *on and v02cli > 0 ;
307700100504                ErrMessage  = *on;
307800100504                ErrGenerico = *on;
307900100504                PosCurCom03 = *on;
308000100504                W03msg = $Msg(34);
308100100504                leavesr;
308200100504             endif;
308300090702
308400100326         // controllo data scadenza contatto
308500171221           If    w03dsc  > 0;
308600100326             clear wlbdat;
308700100326             g02dat = w03dsc;
308800100326             xsrda8(wlbdat);
308900100326             if g02err = '1';
309000100326               errMessage  = *on;
309100100326               errGenerico = *on;
309200100326               PosCurDsc03 = *on;
309300100326               W03msg = $Msg(03);
309400100326               leavesr;
309500100326             endif;
309600100326
309700100326             w03dsc = g02dat;
309800100326             WW03dsc = g02inv;
309900100326           else ;
310000100326             errMessage  = *on;
310100100326             errGenerico = *on;
310200100326             PosCurDsc03 = *on;
310300100326             W03msg = $Msg(03);
310400171222             if  W03dsc < *zeros;
310500171222               W03msg = %trimR( W03msg ) + ' (negativa)';
310600171222             endif;
310700100326             leavesr;
310800100326           endif;
310900100326
311000100326         // controllo che la data non sia inferiore al 2000 o superiore al 2039
311100100326         IF  ww03dsc < 20000101 or ww03dsc > 20391231;
311200100326           errMessage  = *on;
311300100326           errGenerico = *on;
311400100326           PosCurDsc03 = *on;
311500100326           W03msg = $Msg(03);
311600100326           leavesr;
311700100326         ENDIF;
311800100330
311900100330       // controllo che la data non sia inferiore a 3 gg lavorativi da oggi
312000100330         clear XGIOLAVDS;
312100100330         IXGLdata = %dec(%date());
312200100330         IXGLsub  = 'S';
312300100330         IXGLgg   = 3;
312400100330         IXGLpa   = 'P';
312500100330         IXGLlav  = 'S';
312600100330         IXGLfil = %dec(%subst(W03com:1:3):3:0);
312700100330         XGIOLAV (xgiolavds);
312800100330         IF  ww03dsc < OXGLdata;
312900100330             errMessage  = *on;
313000100330             errGenerico = *on;
313100100330             PosCurDsc03 = *on;
313200100330             W03msg = $Msg(03);
313300100330             W03msg = %trim(W03msg) + ' ' +
313400100330                      'Non può essere inferiore a 3 gg.lavorativi';
313500100330             leavesr;
313600100330         ENDIF;
313700100330
313800100330       // controllo che la data non sia maggiore  a 14 mesi da oggi
313900100330         clear XGIOLAVDS;
314000100330         IXGLdata = %dec(%date());
314100100330         IXGLadd  = 'S';
314200100330         IXGLmm   = 14;
314300100330         IXGLpa   = 'P';
314400100330         IXGLfil = %dec(%subst(W03com:1:3):3:0);
314500100330         XGIOLAV (xgiolavds);
314600100330         IF  ww03dsc > OXGLdata;
314700100330             errMessage  = *on;
314800100330             errGenerico = *on;
314900100330             PosCurDsc03 = *on;
315000100330             W03msg = $Msg(03);
315100100330             W03msg = %trim(W03msg) + ' ' +
315200100330                    'Non può essere maggiore di 14 mesi';
315300100330             leavesr;
315400100330         ENDIF;
315500100330
315600100326
315700100326         // controllo ora  esecuzione contatto
315800171222           If W03osc <> 0 ;
315900100326              sav02ora = %editc(W03osc:'X');
316000171222              If  W03osc < *zeros  or  ora > 23 or minuti > 59;
316100100326                   errMessage  = *on;
316200100326                   errGenerico = *on;
316300100326                   PosCurOsc03 = *on;
316400100326                   W03msg = $Msg(05);
316500171222                   if  W03osc < *zeros;
316600171222                     W03msg = %trimR( W03msg ) + ' (negativa)';
316700171222                   endif;
316800100326                   leavesr;
316900100326              endif;
317000100326           else ;
317100100326             errMessage  = *on;
317200100326             errGenerico = *on;
317300100326             PosCurOsc03 = *on;
317400100326             W03msg = $Msg(05);
317500100326             leavesr;
317600100326           endif;
317700100326
317800090702         // In caso di appuntamento controllo anche i campi seguenti
317900090702
318000090702       If Prox_appunta = *on ;
318100091216
318200090702        // verifico l'inserimento delle ore di impegno
318300090702
318400090702         // controllo ora  inizio impegno
318500171222           If W03oii <> 0 ;
318600090702              sav02ora = %editc(W03oii:'X');
318700171222              If  W03oii < *zeros  or  ora > 23 or minuti > 59;
318800090702                   errMessage  = *on;
318900090702                   errGenerico = *on;
319000090702                   PosCurOii03 = *on;
319100090702                   w03msg = $Msg(05);
319200171222                   if  W03oii < *zeros;
319300171222                     W03msg = %trimR( W03msg ) + ' (negativa)';
319400171222                   endif;
319500090702                   leavesr;
319600090702              endif;
319700091216           else ;
319800091216             errMessage  = *on;
319900091216             errGenerico = *on;
320000091216             PosCurOii03 = *on;
320100091216             w03msg = $Msg(05);
320200091216             leavesr;
320300090702           endif;
320400090702
320500091216         // controllo ora  fine   impegno
320600171222           If W03ofi <> 0 ;
320700090702              sav02ora = %editc(W03ofi:'X');
320800171222              If  W03ofi < *zeros  or  ora > 23 or minuti > 59;
320900090702                   errMessage  = *on;
321000090702                   errGenerico = *on;
321100090702                   PosCurOfi03 = *on;
321200090702                   w03msg = $Msg(05);
321300171222                   if  W03ofi < *zeros;
321400171222                     W03msg = %trimR( W03msg ) + ' (negativa)';
321500171222                   endif;
321600090702                   leavesr;
321700090702              endif;
321800091216           else ;
321900091216             errMessage  = *on;
322000091216             errGenerico = *on;
322100091216             PosCurOfi03 = *on;
322200091216             w03msg = $Msg(05);
322300091216             leavesr;
322400090702           endif;
322500090702
322600090702         // controllo ora  inizio se congruente con fine impegno
322700090702           If W03oii > 0 and W03ofi = 0;
322800090702              errMessage  = *on;
322900090702              errGenerico = *on;
323000090702              PosCurOfi03 = *on;
323100090702              w03msg = $Msg(13);
323200090702              leavesr;
323300090702           endif;
323400090702
323500090702         // controllo ora  inizio se congruente con fine impegno
323600090702           If W03oii > 0 and W03ofi > 0 and W03oii > w03ofi ;
323700090702              errMessage  = *on;
323800090702              errGenerico = *on;
323900090702              PosCurOfi03 = *on;
324000090702              w03msg = $Msg(13);
324100090702              leavesr;
324200090702           endif;
324300090804
324400090804         // verifico se inizioe fine impegno è congruente con ora appuntamento
324500090804           If W03oii > 0 and W03ofi > 0 and (W03oii > W03osc or
324600090804              W03ofi < W03osc) ;
324700090804              errMessage  = *on;
324800090804              errGenerico = *on;
324900090804              PosCurOfi03 = *on;
325000090804              w03msg = $Msg(15);
325100090804              leavesr;
325200090804           endif ;
325300091216
325400091216         //?Verifico se il commerciale è libero
325500091216           clear trmk84ds;
325600091216           IMK84cmm = %dec(w03com:7:0);
325700091216           IMK84dad = ww03dsc;
325800091216           IMK84oii = w03oii;
325900091216           IMK84ofi = w03ofi;
326000091216           trmk84r (trmk84ds);
326100091216         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
326200091216           IF  OMK84err <> *blanks and OMK84err <> '9' and
326300091216               Not $ForzaImpegno;
326400091216             ErrGenerico = *on;
326500091216             ErrMessage  = *on;
326600091216             PosCurDsc03 = *on;
326700091216             w03msg = OMK84msg;
326800091216             $ForzaImpegno = *on;
326900091216             leavesr;
327000091216           ENDIF;
327100090702
327200090702       endif;
327300090702
327400090702       ENDSR;
327500100108
327600100108       //--------------------------------------------------------------
327700100108       //?Gestione videata D04.
327800100108       //--------------------------------------------------------------
327900100108       BEGSR GesD04;
328000100108
328100100108         //?Inizializzazione videata
328200100108         IF  $InzD04;
328300100108           exsr InzD04;
328400100108           $InzD04 = *off;
328500100108         ENDIF;
328600100108
328700100108         //?Emissione testata
328800100108         write MK19T01;
328900100108
329000100108         //?Emissione videata
329100100108         exfmt MK19D04;
329200100108         ErrMessage  = *off;
329300100108         ErrGenerico = *off;
329400100108         clear V1Dmsg;
329500100108
329600100108         SELECT;
329700100108
329800100108         //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
329900100108           WHEN  $ErrGrave = *on;
330000100108             $Fine = *on;
330100100108             OMK19err = '1';
330200100108             OMK19msg = V1Dmsg;
330300100108
330400100108         //?F06 = Conferma
330500100108           WHEN  dsp_aid = c_F06;
330600100108             exsr CtrD04;
330700100108             IF  ErrGenerico;
330800100108               leavesr;
330900100108             ENDIF;
331000100108             exsr F06D04;
331100100108
331200100108         //?F12 = Ritorno
331300100108           WHEN  dsp_aid = c_F12;
331400100108             exsr F12D02;
331500100108             leavesr ;
331600100112
331700100112         //?F16 = Annulla
331800100112           WHEN  dsp_aid = c_F16;
331900100112             exsr F16D04;
332000100112             leavesr ;
332100100108
332200100108         //?F18 = Note
332300100108           WHEN dsp_aid = c_F18;
332400100121             exsr F18D04;
332500100108
332600100108         //?Enter
332700100108           OTHER;
332800100108         //?Se manutenzione controllo i dati
332900100108             IF  Not Visualizza;
333000100108               exsr CtrD04;
333100100108               IF  ErrGenerico;
333200100108                 leavesr;
333300100108               ENDIF;
333400100108             ENDIF;
333500100108
333600100108         ENDSL;
333700100108
333800100108       ENDSR;
333900100108
334000100108       //--------------------------------------------------------------
334100100108       //?Caricamento videata telefonata D04.
334200100108       //--------------------------------------------------------------
334300100108       BEGSR InzD04;
334400100108
334500100108         clear MK19D04;
334600100108
334700100108       //?Indicatori di comodo
334800100108         altrenote    = *off  ;
334900100108         piunote      = *off  ;
335000100108         GiornoTotale = *off;
335100100108
335200100108       //?Imposto se visualizzazione
335300100108         IF  IMK19fle = '5';
335400100108           V01tit     = tit_visua;
335500100108           Visualizza = *on;
335600100108         ENDIF;
335700100108
335800100108       //?Imposto se manutenzione
335900100108         IF  IMK19fle = '2';
336000100108           V01tit       = tit_manut;
336100100108           Manutenzione = *on;
336200100108         ENDIF;
336300100519
336400100519         //?Abilito F16=Annullamento solo in manutenzione
336500100519         F16Attivo = Manutenzione;
336600100108
336700100108       //?Aggancio dati attività
336800100108           chain(n) (imk19tat:imk19atn:imk19atnp) tiatc01l;
336900100108
337000100108       //?Non trovo vado a fine
337100100108         IF  not %Found(tiatc01l);
337200100108           $ErrGrave   = *on;
337300100108           ErrMessage  = *on;
337400100108           ErrGenerico = *on;
337500100108           V1Dmsg = 'Dati attività non trovati';
337600100108           leavesr;
337700100108         ENDIF;
337800100108
337900100108       //?Ricerco descrizione causale
338000100108         clear dCCO;
338100100108         clear TIBS02ds;
338200100108         T02mod = 'C';
338300100108         T02cod = 'CCO';
338400100108         T02ke1 = atccad;
338500100108         T02sif = KNSIF;
338600100108         TNTBE_RicercaControllo  (kpjba : tibs02ds);
338700100108         IF  T02err = *blanks;
338800100108           dCCO = t02uni;
338900100108         ENDIF;
339000100108
339100100108       //?Imposto titolo alto
339200100108         clear wtitolo;
339300100108         wtitolo = §TTAtit;
339400100113         wtitolo = %trim(wtitolo) + '   ' + atccad + ' - ' + §CCOdes;
339500100108         clear wpos;
339600100108         wpos = (%len(v04tit) - %len(wtitolo)) / 2;
339700100108         %subst(v04tit:wpos:%len(wtitolo)) = wtitolo;
339800100108
339900100108       //?Verifico se parziale o tutta la giornata
340000100108         IF  atccad = 'FT';
340100100108           GiornoTotale = *on;
340200100108         ENDIF;
340300100108
340400100108       //?Data attività
340500100108         IF  atcdad > 0;
340600100108           data_eur = %date(atcdad:*iso);
340700100108           V04dad   = %dec(data_eur);
340800100108           w04dad   = atcdad;
340900100108         ENDIF;
341000100108
341100100108       //?Commerciale
341200100108         V04cmm = %editc(atccmm : 'X');
341300130809         clear  V04cmmD;
341400130809         chain (ATCcmm) AZCMM000;
341500130809         IF   %found(AZCMM01L);
341600130809           V04cmmd = CMMdes;
341700100108         ENDIF;
341800100108
341900100108       //?Impegno
342000100108         V04oii = atcoii;
342100100108         V04ofi = atcofi;
342200100108
342300100108       //?Note
342400100108         clear trmk20ds ;
342500100108         IMK20tla = 'L';
342600100108         IMK20flm = 'R';
342700100108         IMK20tat = IMK19tat ;
342800100108         IMK20atn = IMK19atn ;
342900100108         IMK20atnp= IMK19atnp -1;
343000100108         trmk20r ( kpjba : trmk20ds );
343100100108         V04no1o = emk20no1;
343200100108         V04no2o = emk20no2;
343300100108         piunote = (omk20piu = '+');
343400100108
343500100108       ENDSR;
343600100108
343700100108       //--------------------------------------------------------------
343800100108       //?Controllo videata D04.
343900100108       //--------------------------------------------------------------
344000100108       BEGSR CtrD04;
344100100108
344200100108         WindDspF = IndDspF;
344300100108         reset WindDspFb;
344400100108         IndDspF  = WindDspF;
344500100108
344600100108       //?Data attività
344700100108         IF  V04dad = 0;
344800100108           ErrMessage  = *on;
344900100108           ErrGenerico = *on;
345000100108           PosCurScd02 = *on;
345100100108           V1Dmsg      = $Msg(03);
345200100108           leavesr;
345300100108         ENDIF;
345400100108
345500100108         clear wlbdat;
345600100108         g02dat = V04dad;
345700100108         xsrda8(wlbdat);
345800100108         IF  g02err = '1';
345900100108           ErrMessage  = *on;
346000100108           ErrGenerico = *on;
346100100108           PosCurScd02 = *on;
346200100108           V1Dmsg      = $Msg(03);
346300100108           leavesr;
346400100108         ENDIF;
346500100108
346600100108         V04dad = g02dat;
346700100108         W04dad = g02inv;
346800100118
346900100118       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
347000100118         IF  W04dad < 20000101 or W04dad > 20391231;
347100100118           ErrMessage  = *on;
347200100118           ErrGenerico = *on;
347300100118           PosCurScd02 = *on;
347400100118           V1Dmsg      = $Msg(03);
347500100118           leavesr;
347600100118         ENDIF;
347700100108
347800100108       //?Impegno solo se non è intera giornata
347900100108         IF  not GiornoTotale;
348000171222           IF  V04oii = 0;
348100100108             ErrMessage  = *on;
348200100108             ErrGenerico = *on;
348300100108             PosCurOii02 = *on;
348400100108             V1Dmsg      = $Msg(17);
348500100108             leavesr;
348600100108           ENDIF;
348700100108           sav02ora = %editc(V04oii:'X');
348800171222           IF  V04oii < *zeros  or  ora > 23 or minuti > 59;
348900100108             ErrMessage  = *on;
349000100108             ErrGenerico = *on;
349100100108             PosCurOii02 = *on;
349200100108             V1Dmsg      = $Msg(05);
349300171222             if  V04oii < *zeros;
349400171222               V1Dmsg = %trimR( V1Dmsg ) + ' (negativa)';
349500171222             endif;
349600100108             leavesr;
349700100108           ENDIF;
349800100108
349900171222           IF  V04ofi = 0 ;
350000100108             ErrMessage  = *on;
350100100108             ErrGenerico = *on;
350200100108             PosCurOfi02 = *on;
350300100108             V1Dmsg      = $Msg(17);
350400100108             leavesr;
350500100108           ENDIF;
350600100108           sav02ora = %editc(V04ofi:'X');
350700171222           IF  V04ofi < *zeros  or  ora > 23 or minuti > 59;
350800100108             ErrMessage  = *on;
350900100108             ErrGenerico = *on;
351000100108             PosCurOfi02 = *on;
351100100108             V1Dmsg      = $Msg(05);
351200171222             if  V04ofi < *zeros;
351300171222               V1Dmsg = %trimR( V1Dmsg ) + ' (negativa)';
351400171222             endif;
351500100108             leavesr;
351600100108           ENDIF;
351700100108
351800100108         //?controllo ora  inizio se congruente con fine impegno
351900100108           IF  V04oii > 0 and V04ofi = 0;
352000100108             ErrMessage  = *on;
352100100108             ErrGenerico = *on;
352200100108             PosCurOfi02 = *on;
352300100108             V1Dmsg      = $Msg(13);
352400100108             leavesr;
352500100108           ENDIF;
352600100108
352700100108         //?controllo ora  inizio se congruente con fine impegno
352800100108           IF  V04oii > 0 and V04ofi > 0 and V04oii > V04ofi ;
352900100108             ErrMessage  = *on;
353000100108             ErrGenerico = *on;
353100100108             PosCurOfi02 = *on;
353200100108             V1Dmsg      = $Msg(13);
353300100108             leavesr;
353400100108           ENDIF;
353500100108         ENDIF;
353600100108
353700100108       //?Codice commerciale
353800100108         IF  V04cmm = *blanks or V04cmm = *zeros;
353900100108           ErrMessage  = *on;
354000100108           ErrGenerico = *on;
354100100108           PosCurCmi02 = *on;
354200100108           V1Dmsg      = $Msg(02);
354300100108           leavesr;
354400100108         ENDIF;
354500100108
354600100108         IF  %scan('?' : V04cmm) > 0  ;
354700100108           ErrGenerico = *on;
354800100108           PosCurCmi02 = *on;
354900130809           clear  TRMK43ds;
355000130809           iMK43solU = 'S';
355100130809           iMK43fil  = DUTpou;
355200130809           TRMK43R (kpjba : TRMK43ds);
355300130809           if  oMK43err = *off  and  oMK43fxx = *blank;
355400130809             V04cmm  = %editc( oMK43cmm : 'X' );
355500130809             V04cmmd = oMK43des;
355600130809           endif;
355700100108           leavesr;
355800100108         ENDIF;
355900100108
356000100108         IF  %check(digits:V04cmm) > 0;
356100100108           ErrMessage  = *on;
356200100108           ErrGenerico = *on;
356300100108           PosCurCmi02 = *on;
356400100108           V1Dmsg      = $Msg(02);
356500100108           leavesr;
356600100108         ENDIF;
356700100210
356800100210         //?utente abilitato al commerciale
356900100210         clear TNTAA1DS;
357000120810         ITAA1tipo = 'M';
357100120810         ITAA1caut = '§utegtc';
357200100210         ITAA1cmm  = %dec(V04cmm:7:0);
357300100519         ITAA1abi = 'RA';
357400100210         kpjbu     = tntaa1ds;
357500100210         tntaa1c (kpjba);
357600100210         TNTAA1DS = kpjbu;
357700100210         IF  OTAA1fabi = 'N';
357800100210           ErrMessage  = *on;
357900100210           ErrGenerico = *on;
358000100210           PosCurCmi02 = *on;
358100100210           V1Dmsg      = $Msg(02);
358200100210           leavesr;
358300100210         ENDIF;
358400100108
358500130809         CMMcod = %int(V04cmm);
358600130809         chain (CMMcod) AZCMM000;
358700130809         IF  not %found(AZCMM01L);
358800100108           ErrMessage  = *on;
358900100108           ErrGenerico = *on;
359000100108           PosCurCmi02 = *on;
359100100108           V1Dmsg      = $Msg(02);
359200100108           leavesr;
359300100108         ENDIF;
359400100504
359500130809         V04cmmd = CMMdes;
359600100210
359700100210       //?Commerciale  -  solo unificante
359800130809         IF  %dec(V04cmm:7:0) <> CMMuni;
359900100210           ErrMessage  = *on;
360000100210           ErrGenerico = *on;
360100100210           PosCurCmi02 = *on;
360200100210           V1Dmsg      = 'Commerciale errato. ' +
360300100210                         'Immettere solo commerciali UNIFICANTI!';
360400100210           leavesr;
360500100210         ENDIF;
360600100108
360700100108       ENDSR;
360800100108
360900100108       //--------------------------------------------------------------
361000100108       //?Gestione tasto funzionale F06 da videata D04.
361100100108       //?F06=Conferma
361200100108       //--------------------------------------------------------------
361300100108       BEGSR F06D04;
361400100108
361500100108       //?Registro le varizioni
361600100108         chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
361700100108         IF  %Found(tiatc01l);
361800100108           atcdad = w04dad ;
361900100108           IF  GiornoTotale;
362000100108             atchda = 090000;
362100100108           ELSE;
362200100108             atchda = V04oii * 100;
362300100108           ENDIF;
362400100108           atccmm = %dec(V04cmm:7:0);
362500100108           atcoii = V04oii;
362600100108           atcofi = V04ofi;
362700100108           update tiatc000;
362800100108         ENDIF;
362900100108
363000100108       //?Registro le note
363100100108         clear trmk20ds;
363200100108         IMK20tla = 'L';
363300100108         IMK20flm = 'C';
363400100215         IMK20cmt = '1';
363500100108         IMK20tat = atctat;
363600100108         IMK20atn = atcatn;
363700100108         IMK20atnp= atcatnp - 1;
363800100108         trmk20r ( kpjba : trmk20ds );
363900100108
364000100108       //?Comitto se richiesto e se non ci sono errori
364100100108         IF  IMK19cmt = 'S' and OMK19err = *blanks;
364200100108           commit;
364300100108         ENDIF;
364400100108
364500100108       //?Ritorno al programma chiamante
364600100108         $fine = *on;
364700100108
364800100108       ENDSR;
364900100112
365000100112       //--------------------------------------------------------------
365100100112       //?Gestione tasto funzionale F16 da videata D04.
365200100112       //?F16=Annulla
365300100112       //--------------------------------------------------------------
365400100112       BEGSR F16D04;
365500100112
365600121029       //?Se ci sono note legate all'attività da pulire le cancello
365700121029         exsr Del_ticpn;
365800100112
365900100112       //?Registro le varizioni
366000100112         chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
366100100112         IF  %Found(tiatc01l);
366200100112           delete tiatc000;
366300100112         ENDIF;
366400100112
366500100112       //?Comitto se richiesto e se non ci sono errori
366600100112         IF  IMK19cmt = 'S' and OMK19err = *blanks;
366700100112           commit;
366800100112         ENDIF;
366900100112
367000100112       //?Ritorno al programma chiamante
367100100112         $fine = *on;
367200100112
367300100112       ENDSR;
367400100121
367500100121       //--------------------------------------------------------------
367600100121       //?Gestione tasto funzionale F18 da videata D04
367700100121       //?F18=Note
367800100121       //--------------------------------------------------------------
367900100121       BEGSR F18D04;
368000100121
368100100121         clear trmk20ds;
368200100121       //?Se interrogazion o attività già chiusa
368300100121       //?richiamo in interrogazione
368400100121         IF  GiaFatto or Visualizza;
368500110510           IMK20tla  = 'L';
368600100121           IMK20flm = 'I';
368700100121         ENDIF;
368800100121         IMK20tat = atctat;
368900100121         IMK20atn = atcatn;
369000100121         Trmk20r ( kpjba : trmk20ds );
369100100929
369200100929       //?Se sono in manutenzione devo recuperare la note come prima
369300100929         IF  not GiaFatto and not Visualizza;
369400100929           IMK20tla  = 'L';
369500100929           IMK20flm  = 'R';
369600100929           IMK20tat  = IMK19tat ;
369700100929           IMK20atn  = IMK19atn ;
369800100929           IMK20atnp = IMK19atnp -1;
369900100929           Trmk20r ( kpjba : trmk20ds );
370000100929           v04no1o   =  emk20no1 ;
370100100929           v04no2o   =  emk20no2 ;
370200100929           piunote   = (omk20piu = '+');
370300100929         ENDIF;
370400100121
370500100121       ENDSR;
370600100224
370700100224       //--------------------------------------------------------------
370800100224       //?Gestione videata W05
370900100224       //--------------------------------------------------------------
371000100224       BEGSR GesW05;
371100100224
371200100224         // Inizializzazione videata
371300100224         clear w05ann ;
371400100224
371500100224         // verifico se c'è affiancamento  richiamo il trmk22 in modalità annullamento
371600100224         If Conaffianca = *on ;
371700100224            clear trmk22ds ;
371800100224            IMK22fcmt = '1';
371900100224            IMK22cmt = 'N';
372000100224            IMK22fmt = '4';
372100100224            IMK22fle = 'D';
372200100224            IMK22cac = v02cae;
372300100224            IMK22cmm  = %dec(v02com:7:0);
372400100224            IMK22dad  = w02scd;
372500100224            IMK22hda  = v02ora * 100;
372600100224            IMK22oii  = v02oii;
372700100224            IMK22ofi  = v02ofi;
372800100224            IMK22tat  = IMK19tat;
372900100224            IMK22atn  = IMK19atn;
373000100224            IMK22atnp = IMK19atnp;
373100100224
373200100224            trmk22r (kpjba : TRMK22ds);
373300100224
373400100224         // se ritorna errore lo visualizzo e faccio rollback
373500100224            IF  OMK22err <> *blanks;
373600100224                ErrMessage  = *on;
373700100224                ErrGenerico = *on;
373800100224                V1dmsg = OMK22msg;
373900100224                IF  IMK19cmt = 'S';
374000100224                    rolbk;
374100100224                ENDIF;
374200100224                leavesr;
374300100224            Endif;
374400100224
374500100224         // se F12 da gestione affiancamento rollback
374600100224            If  IMK19cmt = 'S' and OMK22F12 = 'S';
374700100224                rolbk;
374800100224         // Ritorno alla videata D02
374900100224                $Video = 'D2';
375000100224                leavesr;
375100100224            Endif;
375200100224
375300100224            Conaffianca = *off ;
375400100224         Endif ;
375500100224
375600100517         $fine5 = *off ;
375700100224         // Emissione videata
375800100517         dow $fine5 = *off ;
375900100224         exfmt MK19W05;
376000100224         errMessage   = *off;
376100100224         errGenerico  = *off;
376200100224         clear W05msg;
376300100224
376400100225         // se dato f6 conferma e risposto "SI"
376500100503           If   dsp_aid = c_F06 and w05ann = 'S' ;
376600100519         //?Se posso deletare
376700100519             IF $OkDel;
376800100224                     delete (imk19tat:imk19atn:imk19atnp) tiatc01l ;
376900100224         // se l'attività era un progressivo uno vado a cancellare le note
377000100224         // con progressivo 0
377100100224                     If imk19atnp = 1 ;
377200100224                        clear trmk20ds ;
377300100224                        imk20tla = 'L';
377400100224                        imk20flm = 'D';
377500100224                        imk20cpo = cpocpo ;
377600100224                        imk20rsc = v02cpd ;
377700100224                        imk20tat = imk19tat ;
377800100224                        imk20atn = imk19atn ;
377900100224                        imk20atnp= imk19atnp -1;
378000100224                        Trmk20r ( kpjba : trmk20ds );
378100100224                     endif ;
378200100224         // comitto se richiesto e se non ci sono errori
378300100224                IF  IMK19cmt = 'S';
378400100224                    commit ;
378500110412         // Verifico la categoria del potenziale
378600110412                    clear trmk05ds;
378700110412                    IMK05cpo = CPOcpo;
378800110412                    trmk05r (kpjba : TRMK05DS);
378900110412         //?se categoria variata aggiorno potenziale
379000110412                    IF  OMK05err = *blanks and OMK05cat <> CPOfls;
379100110412                        chain(e) CPOcpo TNCPO01L;
379200110412                        IF  not %error and %found(TNCPO01L);
379300110412                            CPOfls = OMK05cat;
379400110412                            update TNCPO000;
379500110412                        ENDIF;
379600110412                    ENDIF;
379700100224                    $Fine = *on;
379800100517                    leavesr ;
379900100224                ENDIF;
380000101014                $fine5 = *on;
380100101014                $Fine = *on;
380200100519              ELSE;
380300100520         //?CHIODO
380400100520         //?Non posso deletare  quindi chiudo attività con causale 'AA'
380500100520                chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
380600100520                IF  %Found(TIATC01L);
380700100520                  ATCdco = %dec(%date());
380800100520                  ATChco = %dec(%time());
380900100520                  IF  IMK19com > *zeros;
381000100520                    ATCcco = IMK19com;
381100100520                  ELSE;
381200100520                    ATCcco = %dec(V02cmi:7:0);
381300100520                  ENDIF;
381400100520                  ATCpru = knmus;
381500100520                  ATCcac = 'AA';
381600100520                  update TIATC000;
381700100520                ENDIF;
381800100520
381900100520               // se ci sono delle note le registro
382000100520               if v02no1 <> *blanks  or v02no2 <> *blanks ;
382100100520                clear trmk20ds ;
382200100520                imk20tla = 'L';
382300100520                imk20flm = 'C';
382400100520                imk20fno = 'S';
382500100520                imk20cmt = '1';
382600100520                imk20cpo = cpocpo ;
382700100520                If v02cli > 0 ;
382800100520                  imk20ksc = v02cli ;
382900100520                endif ;
383000100520                imk20tat = atctat ;
383100100520                imk20atn = atcatn ;
383200100520                imk20atnp= atcatnp;
383300100520                imk20rsc = v02cpd ;
383400100520                emk20no1 = v02no1 ;
383500100520                emk20no2 = v02no2 ;
383600100520                emk20dim = atcdco ;
383700100520                emk20him = atchco ;
383800100520                Trmk20r ( kpjba : trmk20ds );
383900100520               endif ;
384000100520               IF  IMK19cmt = 'S';
384100100520                   commit ;
384200110412         // Verifico la categoria del potenziale
384300110412                    clear trmk05ds;
384400110412                    IMK05cpo = CPOcpo;
384500110412                    trmk05r (kpjba : TRMK05DS);
384600110412         //?se categoria variata aggiorno potenziale
384700110412                    IF  OMK05err = *blanks and OMK05cat <> CPOfls;
384800110412                        chain(e) CPOcpo TNCPO01L;
384900110412                        IF  not %error and %found(TNCPO01L);
385000110412                            CPOfls = OMK05cat;
385100110412                            update TNCPO000;
385200110412                        ENDIF;
385300110412                    ENDIF;
385400100520                   $Fine = *on;
385500100520                   leavesr ;
385600100520               ENDIF;
385700101014                $fine5 = *on;
385800101014                $Fine = *on;
385900100520
386000100519              ENDIF;
386100100224           endif;
386200100519
386300100519
386400100225         // se dato f6 conferma e risposto "NO"
386500100503           If   dsp_aid = c_F06 and w05ann = 'N' ;
386600100225         // bollback
386700100225                IF  IMK19cmt = 'S';
386800100225                    rolbk ;
386900100225                    $Fine = *on;
387000100517                    leavesr ;
387100100225                ENDIF;
387200101014                $fine5 = *on;
387300101014                $Fine = *on;
387400100225           endif;
387500100519
387600100517         enddo;
387700100224
387800100224       ENDSR;
387900100614
388000100614       //--------------------------------------------------------------
388100100614       //?Gestione videata W08
388200100614       //--------------------------------------------------------------
388300100614       BEGSR GesW08;
388400100614
388500100614         //?Inizializzazione videata
388600100614         w08sino = 'N';
388700100614         $Fine8 = *off;
388800100614
388900100614         //?Emissione videata
389000100614         DOW  not $Fine8;
389100100614           exfmt MK19W08;
389200100614
389300100614         //?F06-Conferma
389400100614           IF  dsp_aid = c_F06;
389500100614         //?Perdo modifiche
389600100614             IF  W08sino = 'S';
389700100614               $Fine8 = *on;
389800100614               $Video = 'D2';
389900100614               $Modaffianca = *off;
390000100614               exsr F12D02;
390100100614             ENDIF;
390200100614         //?Non Perdo modifiche
390300100614             IF  W08sino = 'N';
390400100614               $Fine8 = *on;
390500100614               $Video = 'D2';
390600100614               $inzd02 = *on ;
390700100614             ENDIF;
390800100614           ENDIF;
390900100614         ENDDO;
391000100614
391100100614       ENDSR;
391200110225
391300110225       //--------------------------------------------------------------
391400110225       //?Gestione videata W09
391500110225       //--------------------------------------------------------------
391600110225       BEGSR GesW09;
391700110225
391800110225         //?Inizializzazione videata
391900110225         $Fine9 = *off;
392000110225
392100110225         //?Emissione videata
392200110225         DOW  not $Fine9;
392300110225           write MK19T01;
392400110225           write MK19D02;
392500110225           exfmt MK19W09;
392600110225
392700110225         //?Annullo l'attività e torno al TRMK18R
392800110301           IF  *inkf;
392900110225             $Fine9 = *on;
393000110225             $Fine  = *on ;
393100110225             chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
393200110225             IF  %Found(TIATC01L);
393300110301               delete TIATC000;
393400110412         // se l'attività era un progressivo uno vado a cancellare le note
393500110412         // con progressivo 0
393600110412                     If imk19atnp = 1 ;
393700110412                        clear trmk20ds ;
393800110412                        imk20tla = 'L';
393900110412                        imk20flm = 'D';
394000110412                        imk20cpo = cpocpo ;
394100110412                        imk20rsc = v02cpd ;
394200110412                        imk20tat = imk19tat ;
394300110412                        imk20atn = imk19atn ;
394400110412                        imk20atnp= imk19atnp -1;
394500110412                        Trmk20r ( kpjba : trmk20ds );
394600110412                     endif ;
394700110412         // comitto se richiesto e se non ci sono errori
394800110412                IF  IMK19cmt = 'S';
394900110412                    commit ;
395000110412                    clear trmk05ds;
395100110412                    IMK05cpo = CPOcpo;
395200110412                    trmk05r (kpjba : TRMK05DS);
395300110412         //?se categoria variata aggiorno potenziale
395400110412                    IF  OMK05err = *blanks and OMK05cat <> CPOfls;
395500110412                        chain(e) CPOcpo TNCPO01L;
395600110412                        IF  not %error and %found(TNCPO01L);
395700110412                            CPOfls = OMK05cat;
395800110412                            update TNCPO000;
395900110412                        ENDIF;
396000110412                    ENDIF;
396100110412                ENDIF;
396200110225             ENDIF;
396300110225           ENDIF;
396400110225         ENDDO;
396500110225
396600110225       ENDSR;
396700100224
396800110527       //--------------------------------------------------------------
396900110527       //?Gestione videata W10
397000110527       //--------------------------------------------------------------
397100110527       BEGSR GesW10;
397200110527
397300110527         //?Emissione videata
397400110530           write MK19T01;
397500110530           write MK19D02;
397600110527           exfmt MK19W10;
397700110530           $Video = 'D2';
397800110527
397900110527       ENDSR;
398000090611       //--------------------------------------------------------------
398100090611       //?Gestione tasto funzionale F12 da videata W03
398200090611       //?F12=Ritorno
398300090611       //--------------------------------------------------------------
398400090611       BEGSR F12W03;
398500090611
398600090611         // Ritorno alla videata D02
398700090611            $Video = 'D2';
398800100118
398900100202         //?Pulisco campo comodo n. trattativa e commerciale
399000100118         clear newnrv;
399100100202         clear w03com;
399200090611
399300090611       ENDSR;
399400090611
399500090610       //--------------------------------------------------------------
399600090610       //?Gestione tasto funzionale F12 da videata D02
399700090610       //?F12=Ritorno
399800090610       //--------------------------------------------------------------
399900090610       BEGSR F12D02;
400000100614
400100100614       //?Se modificato affiancamento devo avvisare che le modifiche andranno perse
400200100614         IF  $ModAffianca;
400300100614           $Video = 'W8';
400400100614           leavesr;
400500100614         ENDIF;
400600090610
400700091127         // verifico se si tratta della prima telefonata da cliente o potenziale
400800091127         // ritorno ai subfile di selezione
400900091127
401000091127        If imk19fle = 'C' ;
401100100802        // exsr Primo_cli ;
401200100802        // leavesr ;
401300091127        endif ;
401400091127
401500091127        If imk19fle = 'P' ;
401600100409           W_mk01f12 = '1' ;
401700100409        // exsr Primo_po  ;
401800100409        // leavesr ;
401900091127        endif ;
402000090611         // Ritorno al programma chiamante
402100100422
402200100422         omk19err = '1' ;
402300100422
402400090611         $fine = *on ;
402500090611
402600090610       ENDSR;
402700090610
402800090610
402900090610       //--------------------------------------------------------------
403000090610       //?Gestione tasto funzionale F14 da videata D02
403100090610       //?F14=Informazioni Commerciali
403200090610       //--------------------------------------------------------------
403300090610       BEGSR F14D02;
403400090610
403500090610         // Richiamo il pgm informazioni Commerciali
403600090610         clear     trmk50ds   ;
403700090610         i50cpo=cpocpo ;
403800120313         If imk19fle <> '5' ;
403900100329            i50mod = 'G' ;
404000100329         else ;
404100100329            i50mod=  'I' ;
404200100329         endif ;
404300090610         i50rag=v02cpd   ;
404400090610         i50pgm='TRMK19R' ;
404500120124
404600120124       //?Se il richiamo delle INFO è stato pilotato da attività 72 devo passare
404700120124       //?il campo I50atb = 0 per obbligare l'azzeramento della % affido a BRT
404800120124         IF  info_cac = '72';
404900120208           I50atb = '0';
405000120125         //?questo lo faccio però solo se sto bloccando l'ultimo cliente della catena
405100120125         //?se ne ho ancora dei NON bloccati non passo il dato al trmk50r
405200120125           setll cpocpo CNACO16L;
405300120125           reade cpocpo CNACO16L;
405400120125           DOW not %eof(CNACO16L);
405500120125             IF  ACOabl = *blanks;
405600120125               clear I50atb;
405700120125               leave;
405800120125             ENDIF;
405900120125             reade cpocpo CNACO16L;
406000120125           ENDDO;
406100120125
406200120124         ENDIF;
406300120524
406400120524       //?Se il richiamo delle INFO è stato pilotato da attività 41 con esito positivo
406500120524       //?su trattativa di tipo I=Incremento devo impostare
406600120524       //?il campo I50atb = % per obbligare l'aumento della % affido a BRT se = 0
406700120524         IF  info_cac = '41' and §VISesito = 'P' and VIStpv = 'I';
406800120524           I50atb = '%';
406900120524         ENDIF;
407000140929
407100140929        // Se chiusa trattativa chiedo INFO obbligatorie indipendentemente dalla causale
407200140929        // e dal tipo trattativa come richiesto dal presidio vendite
407300140929         IF  wchiudo_tr = 'S' ;
407400140929             i50obl = 'S';
407500140929         Endif ;
407600120124
407700090610         TRMK50R  (kpjba:trmk50ds) ;
407800140929
407900140929         clear Wchiudo_tr ;
408000090610
408100090610       ENDSR;
408200090610
408300090610       //--------------------------------------------------------------
408400090610       //?Gestione tasto funzionale F18 da videata D02
408500090610       //?F18=Note
408600090610       //--------------------------------------------------------------
408700090610       BEGSR F18D02;
408800090610
408900090610         // Richiamo il pgm gestione note TRMK20R
409000090610         clear trmk20ds ;
409100090610         if giafatto = *on or visualizza = *on  ;
409200090610            imk20flm = 'I';
409300110510            imk20tla = 'L';
409400090610         else ;
409500090610            imk20flm = 'M';
409600090610         endif ;
409700091105         // se valorizzato il cliente passo il cliente
409800091105         If v02cli > 0 ;
409900091105            imk20ksc = v02cli ;
410000091105            imk20rsc = v02clid ;
410100091105         else ;
410200091105            imk20cpo = cpocpo ;
410300091105            imk20rsc = v02cpd ;
410400091027         endif ;
410500100318         If imk19fle = '2' ;
410600100318            imk20flm = ' ';
410700100318            imk20tat = imk19tat ;
410800100318            imk20atn = imk19atn ;
410900100318            imk20atnp= imk19atnp -1;
411000100318         else;
411100100318            emk20no1 = v02no1 ;
411200100318            emk20no2 = v02no2 ;
411300100318         endif ;
411400090616         Trmk20r ( kpjba : trmk20ds );
411500100318         // se dato f18 mentre sono in manutenzione devo valorizzare le note parte alta
411600100318         // del video ....
411700100318         If imk19fle = '2' ;
411800100318                clear trmk20ds ;
411900100318                imk20tla = 'L';
412000100318                imk20flm = 'R';
412100100318                imk20cpo = cpocpo ;
412200100318                imk20rsc = v02cpd ;
412300100318                imk20tat = imk19tat ;
412400100318                imk20atn = imk19atn ;
412500100318                imk20atnp= imk19atnp -1;
412600100318                Trmk20r ( kpjba : trmk20ds );
412700100318                v02no1o =  emk20no1 ;
412800100318                v02no2o =  emk20no2 ;
412900100318                piunote = (omk20piu = '+');
413000100318
413100100318         else;
413200100318            v02no1 =  emk20no1 ;
413300100318            v02no2 =  emk20no2 ;
413400100318            altrenote = (omk20piu = '+');
413500100318         endif ;
413600090610
413700090610       ENDSR;
413800090610
413900090610       //--------------------------------------------------------------
414000090610       //?Gestione tasto funzionale F01 da videata D02
414100091021       //?F01=Anagrafica
414200090610       //--------------------------------------------------------------
414300090610       BEGSR F01D02;
414400090610
414500091021       If cliente = *off ;
414600091021         // Richiamo il pgm gestione  potenziali se non c'è il cliente valorizzato
414700091104         clear trmk01ds   ;
414800100409         clear kpjbu ;
414900091027         // se sono in visualizzazione imposto interrogazione
415000091027         if imk19fle <> '5' ;
415100091104            mk01k10 ='V'   ;
415200091027         else ;
415300091104            mk01k10 ='N'   ;
415400091027         endif ;
415500091104         mk01cpo =cpocpo ;
415600100409         TRMK02c  (kpjba:Trmk01ds);
415700090610         // Se al ritorno dal pgm c'è errore richiamo il pgm dei potenziali in interrogazione
415800091104         If  mk01err <> ' ' ;
415900091104             clear trmk01ds   ;
416000091104             mk01k10 ='N'   ;
416100091104             mk01cpo =cpocpo ;
416200100409             TRMK02c  (kpjba:Trmk01ds);
416300090610         endif ;
416400091021       else ;
416500091021         // Richiamo il pgm gestione cliente
416600091021
416700091021          clear Tnta60ds ;
416800091021          ta60flg = 'I';
416900091105          ta60nrv = v02cli ;
417000091105          ta60int = 'S';
417100091021
417200110428          tnta60r (kpjba:tnta60ds) ;
417300091021
417400091021       // gestisco i dati di ritorno
417500091021
417600091021          if ta60err <> *blanks ;
417700091021             errMessage  = *on;
417800091021             errGenerico = *on;
417900091021             V1Dmsg = ta60msg;
418000091021             leavesr;
418100091021          EndIf ;
418200091021       endif ;
418300090610
418400091022         // recupero i dati del potenziale/clienti
418500091021         exsr AnagD02 ;
418600091022         // recupero i dati del potenziale/clienti  responsabili trasporti
418700091022         exsr RespD02 ;
418800091022
418900090610
419000090610       ENDSR;
419100150126
419200150126       //--------------------------------------------------------------
419300150126       //?Gestione tasto funzionale F04 da videata D02                 ?
419400150126       //?F4=Campagna                                                  ?
419500150126       //--------------------------------------------------------------
419600150126       BEGSR F04D02;
419700150806
419800150807         clear TRKC09DS;
419900150807         IKC09ksu = IMK19ksc;
420000150807         trkc09r (kpjba:TRKC09DS);
420100150126
420200150126       ENDSR;
420300100928
420400100928       //--------------------------------------------------------------
420500100928       //?Gestione tasto funzionale F05 da videata D02
420600100928       //?F05=Attività
420700100928       //--------------------------------------------------------------
420800100928       BEGSR F05D02;
420900100928
421000100928         clear TRMK21DS;
421100100928         I21mod = '9';
421200100928         I21cpo = %dec(V02cpo:11:0);
421300100928         I21rsc = V02cpd;
421400100928         IF  v02cli > 0;
421500100928           I21ksc = V02cli;
421600100928           I21rsc = V02clid;
421700100928         ENDIF;
421800100928         kpjbu = TRMK21DS;
421900100928         trmk21r (kpjba);
422000100928         TRMK21DS = kpjbu;
422100100928         IF  O21err <> *blanks;
422200100928           ErrGenerico = *on;
422300100928           ErrMessage  = *on;
422400100928           V1Dmsg      = O21msg;
422500100928           leavesr;
422600100928         ENDIF;
422700100928
422800100928       ENDSR;
422900090701
423000091026       //--------------------------------------------------------------
423100091026       //?Gestione tasto funzionale F09 da videata D02
423200091210       //?F09=Trattativa
423300091026       //--------------------------------------------------------------
423400091026       BEGSR F09D02;
423500091026
423600100202         // Richiamo la trattativa
423700091026             clear tnta88ds ;
423800091026             ita88cmt = 'N';
423900091118             ita88fle = 'G' ;
424000100202             IF  visualizza;
424100100202             ita88fle = '5' ;
424200100202             ENDIF;
424300091026             ita88fpr = 'A' ;
424400091026             ita88nrv = v02vis ;
424500100225             ita88com = imk19com ;
424600100225             ita88comd= imk19comd ;
424700091026             Tnta88r ( kpjba : tnta88ds );
424800091026
424900091026       ENDSR;
425000091026
425100091026       //--------------------------------------------------------------
425200091026       //?Gestione tasto funzionale F11 da videata D02
425300091026       //?F11=Agenda
425400091026       //--------------------------------------------------------------
425500091026       BEGSR F11D02;
425600091026
425700091026         // Richiamo il pgm visualizzazione agenda
425800091027
425900091027            clear trmk82ds ;
426000100120            imk82rich = 'A' ;
426100091027         // definisco il commerciale da passare
426200091027
426300091027         // vince il commerciale che sta eseguendo l'attività dopo viene
426400091027         // il commerciale che dovrebbe eseguire l'attività  e per ultimo
426500091027         // il commerciale del cliente / potenziale
426600091027
426700091027            select ;
426800100329            when v02com <> *blanks ;
426900100329                imk82cage = %dec(v02com:7:0) ;
427000100329            when v02cmi <> *blanks ;
427100100329                imk82cage = %dec(v02cmi:7:0) ;
427200091111            when v02cmp <> *zeros  ;
427300091111                imk82cage = v02cmp ;
427400091027            endsl ;
427500091027
427600091027            kpjbu = trmk82ds ;
427700091027
427800091027            Trmk82r (kpjba);
427900091026
428000091026       ENDSR;
428100091026
428200091027       //--------------------------------------------------------------
428300091027       //?Gestione tasto funzionale F11 da videata W03
428400091027       //?F11=Agenda
428500091027       //--------------------------------------------------------------
428600091027       BEGSR F11W03;
428700091027
428800091027         // Richiamo il pgm visualizzazione agenda
428900091027
429000091027            clear trmk82ds ;
429100100120            imk82rich = 'A' ;
429200091027         // definisco il commerciale da passare
429300091027
429400091027         // vince uno dei due/tre commerciali valorizzati nella window
429500091027         // il commerciale che sta eseguendo l'attività dopo viene
429600091027         // il commerciale che dovrebbe eseguire l'attività  e per ultimo
429700091027         // il commerciale del cliente / potenziale
429800091027
429900091027            select ;
430000091027            when w03com <> *blanks ;
430100091111                imk82cage = %dec(w03com:7:0) ;
430200091027            when v02cmi <> *blanks ;
430300091111                imk82cage = %dec(v02cmi:7:0) ;
430400091027            when v02cmp <> *zeros  ;
430500091111                imk82cage = v02cmp ;
430600091027            endsl ;
430700091027
430800091027            // se data scadenza valorizzata la passo nella DS
430900091027            If w03dsc > 0 ;
431000091111                imk82data = ww03dsc ;
431100091027            endif ;
431200091027
431300091027            kpjbu = trmk82ds ;
431400091027
431500091027            Trmk82r (kpjba);
431600091027
431700091027       ENDSR;
431800100224
431900100224       //--------------------------------------------------------------
432000100224       //?Gestione tasto funzionale F16 da videata D02
432100100224       //?F16=Annullamento
432200100224       //--------------------------------------------------------------
432300100224       BEGSR F16D02;
432400100224
432500100224         // emetto una windows per la richiesta di conferma annullamento
432600100224          $video = 'W5';
432700100224
432800100224       ENDSR;
432900100224
433000091210       //--------------------------------------------------------------
433100091210       //?Gestione tasto funzionale F20 da videata D02
433200091210       //?F20=Affiancamento
433300091210       //--------------------------------------------------------------
433400091210       BEGSR F20D02;
433500091210
433600091210         clear TRMK22ds;
433700091210         IMK22fmt = '4';
433800091210
433900091210         SELECT;
434000091210           WHEN  Manutenzione;
434100091215             IMK22fcmt = '1';
434200091215             IMK22cmt = 'N';
434300091210             IMK22fle = '2';
434400091210           WHEN  Visualizza;
434500091210             IMK22fle = '5';
434600091210         ENDSL;
434700091210
434800091210         IMK22cmm  = %dec(v02cmi:7:0);
434900091211         IMK22dad  = w02scd;
435000091210         IMK22hda  = v02sco * 100;
435100091210         IMK22oii  = v02oii;
435200091210         IMK22ofi  = v02ofi;
435300091216         IMK22cac  = v02cae;
435400100114         IMK22esi  = v02fatto;
435500091210         IMK22tat  = IMK19tat;
435600091210         IMK22atn  = IMK19atn;
435700091210         IMK22atnp = IMK19atnp;
435800091210
435900091210         trmk22r (kpjba : TRMK22ds);
436000091210
436100091210         //?Se ritorna errore lo visualizzo
436200091210         IF  OMK22err <> *blanks;
436300091210           ErrMessage  = *on;
436400091210           ErrGenerico = *on;
436500091210           V1Dmsg      = OMK22msg;
436600091210         ENDIF;
436700100614
436800100614         //?Salvo che ho fatto una modifica all'affiancamento
436900100614         //?in modo da avvisare l'utente se fa F12 o F3 che le modifiche
437000100614         //?appena fatte andranno perse
437100100614         IF  OMK22f12 = *blanks;
437200100614           $ModAffianca = *on;
437300100614         ENDIF;
437400100224
437500100224        // Verifico se inseriti affiancamenti
437600100224           k_atlatl  = IMK19atn;
437700100224           k_atlatlp = IMK19atnp;
437800100224           chain  %kds(K02tiatl) TIATL02L;
437900100224           IF  %found(Tiatl02l);
438000100224             ConAffianca = *on;
438100100422           ELSE ;
438200100422             ConAffianca = *off ;
438300100224           ENDIF;
438400091210
438500091210       ENDSR;
438600091027
438700090701       //--------------------------------------------------------------
438800100920       //?Gestione tasto funzionale F21 da videata D02 x richiamo TRMK24
438900100920       //?F21=Invio materiale illustrativo
439000090701       //--------------------------------------------------------------
439100090701       BEGSR F21D02;
439200090701
439300100921         // inizializzo alcuni campi per invio lettera
439400100921          clear trmk24ds ;
439500100921          If v02cli > 0 ;
439600100921               Imk24Tde = 'C' ;
439700100921               Imk24Cde = v02cli ;
439800100921          else ;
439900100921               Imk24Tde = 'P' ;
440000100921               Imk24Cde = %dec(v02cpo:11:0) ;
440100100921          endif ;
440200100921
440300100921        // recupero i dati del responsabile trasporti
440400100921          exsr respd02 ;
440500100921          Imk24rst = vh2rtr ;
440600100921        // imposto indirizzo mail immesso nella window
440700100921          Imk24mai = v02mrt ;
440800100921
440900100921         // imposto il commerciale che firmerà la mail, il comm. del potenziale se > 0
441000100921          If v02cmp > 0 ;
441100100921               Imk24cmm = v02cmp ;
441200100921          else ;
441300100921               Imk24cmm = %dec(v02com:7:0);
441400100921          endif ;
441500100921
441600100921          trmk24r ( kpjba : TRMK24ds );
441700100921
441800100921       // Scrivo nota di invio mail se inviata
441900100921          if omk24fxx = '06' ;
442000100921             clear ticpn000 ;
442100100921
442200100921             cpncpo=cpocpo;
442300100921             cpnksc=v02cli;
442400100921             data_iso = (%date()) ;
442500100921             cpndim = %dec(data_iso) ;
442600100921             cpnhim = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
442700100921             cpnsns='S'   ;
442800100921             cpnpos=dutpou;
442900100921             cpnpru=knmus;
443000100921             cpnpno = 1;
443100100921         // imposto il campo nota
443200100921
443300100921             destina = %trim(Omk24mai) ;
443400100921
443500100921             cpnnot='Inviato testo lettera ' + Omk24tlt + ' a nome di '
443600100921                  + %editc(Omk24cmm:'X')  + ' ' + %trim(Omk24cmd) + ' a ' ;
443700100921             write ticpn000;
443800100921             cpnpno+=1;
443900100921             cpnnot=%trim(destina) ;
444000100921             write ticpn000;
444100100924             IF  IMK19cmt = 'S' and OMK19err = *blanks;
444200100924                 commit;
444300100924             ENDIF;
444400100921
444500100921           endif ;
444600090701
444700090701       ENDSR;
444800090701
444900090610       //--------------------------------------------------------------
445000090610       //?Gestione tasto funzionale F02 da videata D02
445100090610       //?F02=Contatti
445200090610       //--------------------------------------------------------------
445300090610       BEGSR F02D02;
445400090610
445500100202         // Richiamo i contatti
445600090610         clear tnta12ds   ;
445700100202         IF  Visualizza;
445800100202           ta12ric = 'V';
445900100202         ENDIF;
446000091118         If v02cli > *zeros ;
446100091118            ta12apl='C'   ;
446200091118            ta12ksc=v02cli ;
446300091118            ta12rag=v02clid  ;
446400091118         else ;
446500091118         If v02vis > *zeros ;
446600091118            ta12apl='T'   ;
446700091118            ta12nrv=v02vis ;
446800091118         else ;
446900091118            ta12apl='P'   ;
447000091118            ta12pot=cpocpo ;
447100091118            ta12rag=v02cpd   ;
447200091105         endif ;
447300091118         endif ;
447400090610         TNTA12R  (kpjba:TNTA12ds);
447500090610         If ta12err <> ' ';
447600090610            errMessage  = *on;
447700090610            errGenerico = *on;
447800090610            V1Dmsg = ta12msg ;
447900090610            leavesr;
448000090610         endif;
448100090610
448200090610         // recupero i dati del potenziale responsabili trasporti
448300090610         exsr RespD02 ;
448400090610
448500090610       ENDSR;
448600090610
448700090610
448800090610       //--------------------------------------------------------------
448900090610       //?Gestione tasto funzionale F06 da videata D02
449000090610       //?F06=Conferma
449100090610       //--------------------------------------------------------------
449200090610       BEGSR F06D02;
449300090610
449400090610        // registro i dati variati sul file
449500090611            Chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
449600090610            If %found(tiatc01l) ;
449700090610               atcdad   = W02scd  ;
449800090610               atchda = v02sco * 100 ;
449900090610               atccmm  = %dec(v02cmi:7:0);
450000090804               if  atctat = 'A' ;
450100090804                   atcoii = V02oii ;
450200090804                   atcofi = V02ofi ;
450300090804               endif ;
450400090610               update tiatc000  ;
450500090610            endif ;
450600100409
450700100409       // Se appuntamento con affiancamento richiamo la gestione dell'affiancamento
450800100614       //?solo se non ero già andata in manutenzione e dato F6
450900111102       // oppure è già andato in manutenzione ma sono stati modificati i dati dell'appuntamento
451000111102         If atctat = 'A' and ConAffianca = *on and (not $ModAffianca or
451100111102         ($ModAffianca and ((sav_atcdad <> 0 and  sav_atcdad <> atcdad ) or
451200111102         (sav_atchda <> 0 and  sav_atchda <> atchda ) or
451300111102         (sav_atcoii <> 0 and  sav_atcoii <> atcoii ) or
451400111102         (sav_atcofi <> 0 and  sav_atcofi <> atcofi ) or
451500111102         (sav_atccmm <> 0 and  sav_atccmm <> atccmm ))));
451600100412           exsr F20d02 ;
451700100409           IF  OMK22f12 = 'S' or OMK22err <> *blanks;
451800100412               IF  IMK19cmt = 'S' ;
451900100412                   rolbk ;
452000100412               ENDIF;
452100100409             leavesr;
452200100409           ENDIF;
452300100409         ENDIF ;
452400091218
452500091218        //?Se è stata modificata la data dell'appuntamento devo scrivere una nota
452600091218         IF  sav_atcdad <> 0 and sav_atcdad <> atcdad and atctat = 'A';
452700091218           data_eur = %date(sav_atcdad:*iso);
452800091218           wdata    = %dec(data_eur);
452900091218           clear trmk20ds;
453000091218           imk20tla = 'L';
453100091218           imk20flm = 'C';
453200091218           imk20fno = 'S';
453300100215           IMK20cmt = '1';
453400091218           imk20cpo = cpocpo;
453500091218           IF  v02cli > 0;
453600091218             imk20ksc = v02cli;
453700091218           ENDIF;
453800091218            imk20tat = atctat;
453900091218            imk20atn = atcatn;
454000091218            imk20atnp= atcatnp - 1;
454100091218            imk20nrv = atcnrv;
454200091222            emk20no1 = 'Data appuntamento variata, precedente: ' +
454300091218                       %editc(wdata:'Y');
454400091218            Trmk20r ( kpjba : trmk20ds );
454500091218
454600091218         ENDIF;
454700100409
454800100915       // Se è stato richiesta la conferma con mail richiamo il pgm TRMK23R1
454900100419           If   dsp_aid = c_F08 ;
455000100915        // richiamo con tipo richiamo blank in quanto in questa fase deve fare sia la richiesta
455100100915        // della mail che inviarla
455200100915                clear  TRMK23ds1;
455300100419                exsr Ric_Trmk23 ;
455400100419           Endif ;
455500100419
455600100304       //?Comitto se richiesto e se non ci sono errori
455700100304         IF  IMK19cmt = 'S' and OMK19err = *blanks;
455800100304           commit;
455900100304         ENDIF;
456000090610
456100100329         $Fine   = *on ;
456200100329
456300090610       ENDSR;
456400090610
456500090610       //--------------------------------------------------------------
456600090610       //?Gestione tasto funzionale F06 da videata D02 + W03
456700090610       //?F06=Conferma
456800090610       //--------------------------------------------------------------
456900090610       BEGSR F06W03D02;
457000090610
457100090610        // registro il contatto in tiatc
457200090610            exsr regis_atc ;
457300090610
457400090610       // scrivo il contatto in scadenza
457500090610         // pulisco i campi che non devono essere valorizzati
457600090610            clear atcest ;
457700090610            clear atcdco ;
457800090610            clear atchco ;
457900090610            clear atccco ;
458000090610            clear atcpru ;
458100090610            clear atccac ;
458200100310            clear atcoii ;
458300100310            clear atcofi ;
458400090702            clear atcco3 ;
458500091021            clear atccnw ;
458600091021            clear atcnra ;
458700100802            clear atcflo ;
458800090610
458900090702            // valorizzo il tipo contatto con quello indicato in tabella causali contatto
459000090702            atctat   = §ccotat ;
459100091214            IF  ConAffianca;
459200091214              atcatnp = sav_atlatnp + 1;
459300091214            ELSE;
459400090610            atcatnp  = atcatnp + 1 ;
459500091214            ENDIF;
459600090610            atccad   = W03cae   ;
459700090610            atcdad   = WW03dsc  ;
459800090610            atchda = w03osc * 100 ;
459900090610            atccmm  = %dec(W03com:7:0);
460000090703            // solo se è un appuntamento scrivo le ore di impegno  e il commerciale che inserisce
460100090703            // l'appuntamento
460200090702            if  prox_appunta = *on ;
460300090702                atcoii = w03oii ;
460400090702                atcofi = w03ofi ;
460500090702                atcco3 = %dec(v02com:7:0) ;
460600090702            endif ;
460700091023            // valorizzo data ora e utente immissione
460800091023            data_iso = (%date()) ;
460900091023            atcdim = %dec(data_iso) ;
461000091023            atchim = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
461100091023            atcpri = knmus ;
461200091113            atcnrv = w03nrv ;
461300100302            exsr Sr_ValCnw ;
461400100510            // se si tratta di un'attività 80 che apre una nuova trattativa siglo con atcatb= '*'
461500100510            // in modo tale venga riconosciuta in un'eventuale richiesta di cancellazione delle 80
461600100510            // dal programma trmk17r nel momento della creazione di una nuova attività dalla stampa
461700100510            // o invio mail tariffa. Questo flag dovrà poi essere pulito nel momento in cui ritorno
461800100510            // dalla creazione della trattativa
461900100723            // 21/07/10 non lo faccio + in quanto facciamo il commit nel tnta88r (gestione trattati-
462000100723            // va) così anche nel caso si voglia cancellare una 80 anche in fase di apertura di una
462100100723            // nuova trattativa si può fare
462200100723            //If atccad = '80' and  $new_trattati = *on ;
462300100721            // atcatb = '*' ;
462400100510            // mi salvo la chiave dell'attività da pulire dopo
462500100723            //   K80_atctat  = atctat ;
462600100723            //   K80_atcatn  = atcatn ;
462700100723            //   k80_atcatnp = atcatnp;
462800100723            // endif ;
462900110302
463000110302            //?Memorizzo la categoria del potenziale all'apertura dell'attività
463100110302              dATC01 = ATCflo;
463200110302              §ATCcapo1 = CPOfls;
463300110302              ATCflo = dATC01;
463400090610            write tiatc000  ;
463500121018
463600121018         //?Aggiorno il potenziale --> data primo contatto e categoria
463700121018           exsr agg_CPO;
463800091210
463900091210         //?Se appuntamento controllo se devo richiamare gestione affiancamento
464000091210         IF  prox_appunta and w03aff = 'S';
464100091210           exsr Ric_TRMK22 ;
464200091210           //?se errore o F12 devo tornare alla Window dell'appuntamento
464300100113           //?e devo riagganciare il TIATC perchè è già stato aggiornato
464400091210           IF  OMK22f12 = 'S' or OMK22err <> *blanks;
464500100113                chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
464600091210             leavesr;
464700091210           ENDIF;
464800091210         ENDIF;
464900100507
465000100507         // se dato f8 conferma (solo in caso di appuntamento )
465100100507         //?Richiamo pgm per richiesta dati mail
465200100507         If   dsp_aid = c_F08 ;
465300100915             clear  TRMK23ds1;
465400100915             // richiamo il programma con R in quanto voglio solo i dati della mail senza inviare
465500100915             // cosa che si farà nella fase finale di conferma
465600100915             Imk23rei = 'R';
465700100915             exsr Ric_trmk23 ;
465800100507         ENDIF;
465900090610
466000091113         // verifico se c'è' un pgm da richiamare in base alla causale
466100100419           If  §ccoapt = 'S' and $new_trattati = *on ;
466200091113         // richiamo il programma gestione trattativa
466300100510               exsr Ric_TNTA88 ;
466400100510         // Se l'apertura della trattativa è stata pilotata dalla 80 verifico
466500100510         // se devo pulire il flag
466600100723            // 21/07/10 non lo faccio + in quanto facciamo il commit nel tnta88r (gestione trattati-
466700100723            // va) così anche nel caso si voglia cancellare una 80 anche in fase di apertura di una
466800100723            // nuova trattativa si può fare
466900100723         //    If atccad = '80' and  $new_trattati = *on ;
467000100723         // // aggancio il record attività' per pulire il flag
467100100723         //        chain (k80_atctat:K80_atcatn:K80_atcatnp) tiatc01l ;
467200100723         //        If %found(tiatc01l) ;
467300100723         //           atcatb = ' ' ;
467400100723         //           update tiatc000 ;
467500100723         //           commit ;
467600100723         //        endif ;
467700100723         //        clear  K80_atctat  ;
467800100723         //        clear  K80_atcatn  ;
467900100723         //        clear  k80_atcatnp ;
468000100723         //    endif ;
468100091113                          If ota88f12 = 'S' ;
468200091113                             $inzd02 = *on ;
468300091113                             $video  = 'D2';
468400091113                             leavesr ;
468500091113                          endif ;
468600091113           endif ;
468700100311
468800100311         // chiedo conferma chiusura trattativa
468900100311             If  $chiedi_conf = *on ;
469000110622                  exsr Ges_Chiudotr ;
469100100311         // rolbk se ci sono errori o non richiesto annullamento trattativa
469200100503                  if (imk19cmt = 'S' and omk19err <> ' ') or w06sino = 'N' ;
469300100311                        rolbk ;
469400100311                        $inzd02 = *on ;
469500100311                        $Video = 'D2';
469600100311                        leavesr ;
469700100311                     endif ;
469800100311             endif ;
469900100419
470000100419         // se dato f8 conferma (solo in caso di appuntamento )
470100100419           If   dsp_aid = c_F08 ;
470200100915          // faccio il richiamo con solo invio mail in quanto ho già i dati della mail nella ds
470300100915                Imk23rei = 'I';
470400100419                exsr Ric_trmk23 ;
470500100419           endif ;
470600100419
470700100419
470800100419         // Prima di tornare al pgm chiamante verifico se devo chiedere infoCommerciali
470900100419
471000100419             exsr Sr_InfoComm ;
471100110412
471200110502         // richiamo il TRMK05 che mi ricalcola la categoria se in categoria eliminabile
471300121018          If CPOfls = 'E';
471400110412               clear trmk05ds;
471500110412               IMK05cpo = CPOcpo;
471600110412               trmk05r (kpjba : TRMK05DS);
471700110412         //?se categoria variata aggiorno potenziale
471800110412               IF  OMK05err = *blanks and OMK05cat <> CPOfls;
471900110412                   chain(e) CPOcpo TNCPO01L;
472000110412                   IF  not %error and %found(TNCPO01L);
472100110412                       CPOfls = OMK05cat;
472200110412                       update TNCPO000;
472300110412                   ENDIF;
472400110412               ENDIF;
472500110502          ENDIF;
472600100311
472700090611         // Ritorno al programma precedente
472800090611
472900090611            $fine  = *on ;
473000090610
473100090610       ENDSR;
473200121018
473300121018       //--------------------------------------------------------------
473400121018       //?Aggiorno dati su TNCPO.
473500121018       //--------------------------------------------------------------
473600121018       BEGSR agg_CPO;
473700121018
473800121018       //?Imposto data primo contatto sul Potenziale
473900121018         IF  §CCOdpc <> 'N' and (§CPOdtpcon = *blanks or §CPOdtpcon = *zeros);
474000121018           clear dCPO01;
474100121018           chain(e) CPOcpo TNCPO01L;
474200121018           IF  not %error and %found(TNCPO01L);
474300121018             dCPO01 = CPOrst;
474400121018             §CPOdtpcon = %editc(W02dat:'X');
474500121018             CPOrst = dCPO01;
474600121018             update TNCPO000;
474700121018           ENDIF;
474800121018         ENDIF;
474900121018
475000121018       //?Se attività 14/15 ricalcolo la categoria del potenziale
475100121018         IF ATCcac = '14' or ATCcac = '15';
475200121018           clear trmk05ds;
475300121018           IMK05cpo = CPOcpo;
475400121018           trmk05r (kpjba : TRMK05DS);
475500121018         //?se categoria variata aggiorno potenziale
475600121018           IF  OMK05err = *blanks and OMK05cat <> CPOfls;
475700121018             chain(e) CPOcpo TNCPO01L;
475800121018             IF  not %error and %found(TNCPO01L);
475900121018               CPOfls = OMK05cat;
476000121018               update TNCPO000;
476100121018             ENDIF;
476200121018           ENDIF;
476300121018         ENDIF;
476400121018
476500121018       ENDSR;
476600090701
476700091210       //--------------------------------------------------------------
476800091210       //?Rchiamo il programma di gestione esito offerta  TNTA56R
476900091210       //--------------------------------------------------------------
477000091210       BEGSR Ric_Tnta56;
477100091210
477200091210             clear tnta56ds ;
477300091210             ita56cmt = 'S';
477400091210             ita56fle = 'G' ;
477500091210             ita56fpr = 'A' ;
477600091210             ita56nrv = v02vis ;
477700100111             ita56tat = imk19tat;
477800100111             ita56atn = imk19atn;
477900100111             ita56atnp= imk19atnp;
478000100225             ita56com = imk19com ;
478100100225             ita56comd= imk19comd ;
478200091210             Tnta56r ( kpjba : tnta56ds );
478300091210
478400091210         // Se restituito errore => e c'è messaggio lo restituisco ...
478500091210         if  ota56err = 'E';
478600091210           errMessage  = *on;
478700091210           errGenerico = *on;
478800091210           V1Dmsg = ota56msg ;
478900091210
479000091210           If imk19cmt = 'S' ;
479100091210              rolbk ;
479200091210           endif ;
479300091210
479400091210           leavesr;
479500091210         endif;
479600091210
479700091210          // se dato f12 ripulisco gli aggiornamenti
479800091210
479900091210            If imk19cmt = 'S' and ota56F12 = 'S' ;
480000091210               rolbk ;
480100091210            endif ;
480200091210
480300091210         // se non ci sono errori e non è stato premuto F12
480400091210         //  confermo tutto quanto scritto fino ad ora
480500091210
480600091210           If ota56err = ' ' and ota56f12 = ' '  ;
480700091210             commit ;
480800091210           endif ;
480900091210
481000091210       ENDSR  ;
481100090701
481200091015       //--------------------------------------------------------------
481300091015       //?Rchiamo il programma di gestione trattative TNTA88R
481400091015       //--------------------------------------------------------------
481500091015       BEGSR Ric_Tnta88;
481600091015
481700091015             clear tnta88ds ;
481800091015             ita88cmt = 'S';
481900091015             ita88fle = 'P' ;
482000091016             ita88fpr = 'A' ;
482100091015             ita88cpo = cpocpo   ;
482200091027             ita88ksc = v02cli ;
482300091113             ita88nrv = newnrv ;
482400091027             if ita88ksc > 0 ;
482500091027                ita88fle = 'C' ;
482600091027             endif ;
482700091015             ita88cmmi = %dec(v02com: 7: 0);
482800091023             If w03com <> *blank ;
482900091023                ita88cmm = %dec(w03com: 7: 0);
483000091023             else ;
483100091027                ita88cmm = v02cmp ;
483200091023             endif ;
483300100225             ita88com = imk19com ;
483400100225             ita88comd= imk19comd ;
483500091015             Tnta88r ( kpjba : tnta88ds );
483600091015
483700091016         // Se restituito errore => e c'è messaggio lo restituisco ...
483800091015         if  ota88err = 'E';
483900091015           errMessage  = *on;
484000091015           errGenerico = *on;
484100091015           V1Dmsg = ota88msg ;
484200091016
484300091016           If imk19cmt = 'S' ;
484400091016              rolbk ;
484500100416              exsr Del_ticpn ;
484600091016           endif ;
484700091016
484800091015           leavesr;
484900091015         endif;
485000091015
485100091016          // se dato f12 ripulisco gli aggiornamenti
485200091016
485300091016            If imk19cmt = 'S' and ota88F12 = 'S' ;
485400091016               rolbk ;
485500100416               exsr Del_ticpn ;
485600091016            endif ;
485700091016
485800091113         // se non ci sono errori e non è stato premuto F12
485900091113         //  confermo tutto quanto scritto fino ad ora
486000091113
486100091113           If ota88err = ' ' and ota88f12 = ' '  ;
486200091016             commit ;
486300091016           endif ;
486400091016
486500091015       ENDSR  ;
486600101221
486700101221       //--------------------------------------------------------------
486800101221       //?Richiamo pgm gestione data caldo senza offerte.
486900101221       //--------------------------------------------------------------
487000101221       BEGSR Ric_Ta63;
487100101221
487200101221         clear TNTA63ds;
487300101221         ITA63fmt  = '4';
487400110118         ITA63fle  = 'I';
487500101221         ITA63nrv  = V02vis;
487600101221
487700101221         tnta63r (kpjba : TNTA63ds);
487800101221
487900101221       ENDSR  ;
488000101221
488100100416       //--------------------------------------------------------------
488200100416       //?Deleto le note che non riesce a fare il pgm con il rolbk
488300100416       //--------------------------------------------------------------
488400100416       BEGSR Del_ticpn;
488500100416                        clear trmk20ds ;
488600100416                        imk20tla = 'L';
488700100416                        imk20flm = 'D';
488800100416                        imk20cpo = cpocpo ;
488900100416                        imk20rsc = v02cpd ;
489000100416                        imk20tat = imk19tat ;
489100100416                        imk20atn = imk19atn ;
489200100416                        imk20atnp= imk19atnp ;
489300100416                        Trmk20r ( kpjba : trmk20ds );
489400100416       ENDSR  ;
489500090610       //--------------------------------------------------------------
489600090610       //?Registro i dati nel file tiatc
489700090610       //--------------------------------------------------------------
489800090610       BEGSR Regis_atc;
489900110304
490000090610         // se un primo contatto stacco un nuovo numero
490100090610         If primocontatto = *on ;
490200090610
490300090610            clear tiatc000 ;
490400090610
490500090610            RESET trul33ds;
490600090610            i33tla = 'L'  ;
490700090610            i33ope = 0    ;
490800090610            i33Cnu = 070  ;
490900090610            i33num = 1    ;
491000090610            i33Aaa = *YEAR;
491100090610            kpjbu = trul33ds;
491200090610            trul33r(kpjba);
491300090610            trul33ds = kpjbu;
491400090610            IF o33Err = 0;
491500090610              atcatn  = o33Nrf;
491600090610            ENDIF;
491700090610
491800090610       // imposto a zero il progressivo telefonata
491900090610            atcatnp  = 1 ;
492000090610       // imposto i campi che vengono inseriti solo nel primo contatto
492100090610            atctat = 'T' ;
492200090610            atccpo = %dec(v02cpo:11:0) ;
492300091021            atcksc = v02cli ;
492400090610         endif ;
492500090610        // Aggiorno un contatto in scadenza
492600090610       // questi si possono inserire anche se non sono i primi contatti
492700100114         // se appuntamento e sto esitando un'attività pianificata imposto come
492800100114         // esito il fatto o non fatto impostato a video
492900100114           IF  Appuntamento and
493000100114               Not Manutenzione and Not Vis_NonFatto and Not Visualizza and
493100100114               v02fatto <> *blanks;
493200100114             atcest = V02fatto;
493300100114           ELSE;
493400091215            atcest = §ccoesi;
493500100114          ENDIF;
493600090610            atcdco = w02dat ;
493700090610            atchco = v02ora * 100 ;
493800090610            atccco = %dec(v02com:7:0);
493900090610            atcpru = knmus  ;
494000090610            atccac = v02cae ;
494100091113
494200091113        // se devo creare trattativa da causale aggiorno il nuovo numero
494300091116         If §ccoapt = 'S' and newnrv > 0 and $new_trattati = *on ;
494400091113            atcnra = newnrv ;
494500091113         endif ;
494600100309
494700100309        // se ho richiesto l'esito della attività lo scrivo nei FLO
494800100309         If §ccores = 'S' ;
494900100309            clear datc01 ;
495000100309            datc01 = atcflo ;
495100100309            §atcesito = W07esi ;
495200100309            atcflo = datc01 ;
495300100309         endif ;
495400100309
495500121018        // imposto che è stato fatto contatto su potenziale mai contattato
495600121018         IF §CPOdtpcon = *blanks or §CPOdtpcon = *zeros;
495700100802            clear datc01 ;
495800100802            datc01 = atcflo ;
495900100802            §atcmaitr = 'S'    ;
496000100802            atcflo = datc01 ;
496100100802         endif ;
496200100802
496300090610        // scrivo il primo contatto ed aggiorno uno in scadenza
496400090610         If primocontatto = *on ;
496500091023            data_iso = (%date()) ;
496600091023            atcdim = %dec(data_iso) ;
496700091023            atchim = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
496800091023            atcpri = knmus ;
496900100302            exsr SR_valcnw ;
497000110302          //?Memorizzo la categoria del potenziale all'apertura dell'attività
497100110302            dATC01 = ATCflo;
497200110413            §ATCcapo2 = CPOfls;
497300110302            ATCflo = dATC01;
497400090610            write tiatc000  ;
497500090610         else ;
497600110302          //?Memorizzo la categoria del potenziale all'esecuzione dell'attività
497700110302            dATC01 = ATCflo;
497800110302            §ATCcapo2 = CPOfls;
497900110302            ATCflo = dATC01;
498000090610            update tiatc000 ;
498100090610         endif;
498200090610
498300090610         // se ci sono delle note le registro
498400090611         if v02no1 <> *blanks  or v02no2 <> *blanks ;
498500090610            clear trmk20ds ;
498600090610            imk20tla = 'L';
498700090610            imk20flm = 'C';
498800090610            imk20fno = 'S';
498900100215            imk20cmt = '1';
499000090610            imk20cpo = cpocpo ;
499100091027            If v02cli > 0 ;
499200091027               imk20ksc = v02cli ;
499300091027            endif ;
499400090610            imk20tat = atctat ;
499500090610            imk20atn = atcatn ;
499600090610            imk20atnp= atcatnp;
499700090610            imk20rsc = v02cpd ;
499800090610            emk20no1 = v02no1 ;
499900090610            emk20no2 = v02no2 ;
500000090610            emk20dim = atcdco ;
500100090610            emk20him = atchco ;
500200090616            Trmk20r ( kpjba : trmk20ds );
500300090610         endif ;
500400090610
500500121018       // solo nel caso in cui non si deve andare
500600110502         if §ccoupm =  ' ';
500700121018         //?Aggiorno il potenziale --> data primo contatto e categoria
500800121018           exsr agg_CPO;
500900090610         endif ;
501000090610
501100100329       // valorizzo i campi per il controllo delle info commerciali
501200100329         info_cac = atccac ;
501300100329         info_nrv = atcnrv ;
501400100329         info_nra = atcnra ;
501500100329
501600090610       ENDSR ;
501700091214
501800100302       //--------------------------------------------------------------
501900100302       //?Valorizzo il campo del cliente potenziale o codificato
502000100302       //--------------------------------------------------------------
502100100302       BEGSR Sr_ValCnw;
502200100302
502300100302       // Verifico se l'attività creata è su opotenziale o su cliente codificato
502400100302
502500110502       //   IF  atcksc = 0;
502600110502       //    IF  $gia_codifica = *on ;
502700110502       //      atccnw = 'C';
502800110502       //    ELSE;
502900110502       //      atccnw = 'P';
503000110502       //    ENDIF;
503100110502       //  ELSE;
503200110502       //   atccnw = 'C';
503300110502       //   ENDIF;
503400110502
503500110502       //  Imposto il flag di ATCCNW in base alla categoria del potenziale
503600110502       // se categoria = a "C" metto "C" altrimenti "P"
503700110502            If cpofls = 'C' ;
503800110502               atccnw = 'C' ;
503900110502            Else ;
504000110502               atccnw = 'P' ;
504100110502            Endif ;
504200100302
504300100302       ENDSR  ;
504400091214       //--------------------------------------------------------------
504500091214       //?Confermo esito affiancamento
504600091214       //--------------------------------------------------------------
504700091214       BEGSR Conf_Trmk22;
504800091214
504900091214         clear TRMK22ds;
505000091215         IMK22fcmt = '1';
505100091215         IMK22cmt = 'N';
505200091214         IMK22fmt = '4';
505300091214         IMK22fle = 'G';
505400091215         IMK22cac = v02cae;
505500100416         IMK22esi = V02fatto ;
505600091214         IMK22cmm  = %dec(v02com:7:0);
505700091214         IMK22dad  = w02dat;
505800091215         IMK22hda  = v02ora * 100;
505900091214         IMK22oii  = v02oii;
506000091214         IMK22ofi  = v02ofi;
506100091214         IMK22tat  = IMK19tat;
506200091214         IMK22atn  = IMK19atn;
506300091214         IMK22atnp = IMK19atnp;
506400091214
506500091214         trmk22r (kpjba : TRMK22ds);
506600091214
506700091214         //?Se ritorna errore lo visualizzo e faccio rollback
506800091214         IF  OMK22err <> *blanks;
506900091214           ErrMessage  = *on;
507000091214           ErrGenerico = *on;
507100091214           V1dmsg = OMK22msg;
507200091214           IF  IMK19cmt = 'S';
507300100721          // rolbk;
507400091214           ENDIF;
507500091214           leavesr;
507600091214         ENDIF;
507700091214
507800091214         //?Se F12 da gestione affiancamento rollback
507900091214         IF  IMK19cmt = 'S' and OMK22F12 = 'S';
508000100721         //rolbk;
508100091214           leavesr;
508200091214         ENDIF;
508300091214
508400091214         //?Se OK cerco ultimo progressivo
508500091214         clear sav_atlatnp;
508600091215         $EoF = *off;
508700091214         setll IMK19atn tiatl01l;
508800091215         DOW  not $EoF;
508900091214           reade IMK19atn tiatl01l;
509000091215           IF  %eof(tiatl01l);
509100091215             $EoF = *on;
509200091215             leave;
509300091215           ENDIF;
509400091215           sav_atlatnp = atlatnp;
509500091214         ENDDO;
509600091214
509700091214       ENDSR;
509800091210
509900091210       //--------------------------------------------------------------
510000091210       //?Richiamo il programma di gestione affiancamento TRMK22R
510100091210       //--------------------------------------------------------------
510200091210       BEGSR Ric_Trmk22;
510300091210
510400091210         clear TRMK22ds;
510500091215         IMK22fcmt = '1';
510600091215         IMK22cmt = 'N';
510700091210         IMK22fmt = '4';
510800091210         IF  Prox_Appunta;
510900091210           IMK22fle = 'I';
511000091210         ELSE;
511100091210           IMK22fle = 'G';
511200091210           IMK22cac = atccac;
511300100416           IMK22esi = v02fatto ;
511400091210         ENDIF;
511500100924         IF  §ccoupm <> ' ';
511600100412             IMK22cmm  = %dec(w03com:7:0);
511700100412         ELSE;
511800100412             IMK22cmm  = %dec(v02com:7:0);
511900100412         ENDIF;
512000091210         IMK22dad  = atcdad;
512100091210         IMK22hda  = atchda;
512200091210         IMK22oii  = atcoii;
512300091210         IMK22ofi  = atcofi;
512400091210         IMK22tat  = atctat;
512500091210         IMK22atn  = atcatn;
512600091210         IMK22atnp = atcatnp;
512700091210
512800091210         trmk22r (kpjba : TRMK22ds);
512900091210
513000091210         //?Se ritorna errore lo visualizzo e faccio rollback
513100091210         IF  OMK22err <> *blanks;
513200091210           ErrMessage  = *on;
513300091210           ErrGenerico = *on;
513400091210           IF  Prox_Appunta;
513500091210             W03msg = OMK22msg;
513600091210           ELSE;
513700091210             V1dmsg = OMK22msg;
513800091210           ENDIF;
513900091210           IF  IMK19cmt = 'S';
514000091210             rolbk;
514100091210           ENDIF;
514200091210           leavesr;
514300091210         ENDIF;
514400091210
514500091210         //?Se F12 da gestione affiancamento rollback
514600091210         IF  IMK19cmt = 'S' and OMK22F12 = 'S';
514700091210           rolbk;
514800091210         ENDIF;
514900091210
515000100622         //?se non ci sono errori e provengo dalla videata W03 e dopo
515100100622         //?questo programma devo richiamare la trattativa non committo
515200100622         //?perchè se F12 dalla trattativa devo poter fare il rolbk
515300100622         IF  prox_appunta  and w03aff = 'S' and
515400100622             §ccoapt = 'S' and $new_trattati;
515500100622           leavesr;
515600100622         ENDIF;
515700091210         //?Se non ci sono errori e non è stato premuto F12
515800091210         //?confermo tutto
515900091210         IF  OMK22err = *blanks and OMK22f12 = *blanks;
516000100721         // dopo una lunga e attenta osservazione di alcuni comportamenti del programma
516100100721         // abbiamo verificato che questo commit creava dei problemi tolto xchè ci pensa
516200100721         /// il programma + avanti a committare il tutto
516300100721         //  commit;
516400091210         ENDIF;
516500091210
516600091210       ENDSR;
516700091113
516800100329       //--------------------------------------------------------------
516900100329       //?Richiamo programma invio mail conferma appuntamento TRMK23R
517000100329       //--------------------------------------------------------------
517100100329       BEGSR Ric_Trmk23;
517200100507
517300100329
517400100915         If  Imk23rei <> 'I';
517500100915             Imk23f12 = 'S'    ;
517600100915             Imk23cma = atcco3 ;
517700100915             Imk23cmm = atccmm ;
517800100915             Imk23dta = atcdad ;
517900100915             Imk23ora = %dec(%subst(%editc(atchda:'X'):1:4):4:0);
518000100329        // recupero i dati del responsabile trasporti
518100100915             exsr respd02 ;
518200100915             Imk23rst = vh2rtr ;
518300100511        // imposto indirizzo mail immesso nella window
518400100915             Imk23emr = v02mrt ;
518500100915         Endif ;
518600100915
518700100915         trmk23r1 ( kpjba : TRMK23ds1 );
518800100915
518900100915         ATCco3 = Imk23cma;
519000100915
519100100915
519200100329
519300100329       ENDSR;
519400110207
519500110207       //--------------------------------------------------------------
519600110207       //?Richiamo il programma di gestione blocco cliente TRMK25R
519700110207       //--------------------------------------------------------------
519800110207       BEGSR Ric_Trmk25;
519900110207
520000110207         clear TRMK25ds;
520100110207         IMK25fcmt = '1';
520200110207         IMK25cmt = 'N';
520300110207         IMK25fmt = '4';
520400110207         IMK25fle = 'I';
520500110207         IMK25cmm  = %dec(v02com:7:0);
520600110209         IMK25cpo  = imk19cpo ;
520700110208         IMK25cac  = v02cae ;
520800110207
520900110207         trmk25r (kpjba : TRMK25ds);
521000110207
521100110207         //?Se ritorna errore lo visualizzo e faccio rollback
521200110207         IF  OMK25err <> *blanks;
521300110207           ErrMessage  = *on;
521400110207           ErrGenerico = *on;
521500110207           V1dmsg = OMK25msg;
521600110207           IF  IMK19cmt = 'S';
521700110207             rolbk;
521800110207           ENDIF;
521900110207           leavesr;
522000110207         ENDIF;
522100110207
522200110207         //?Se F12 da gestione affiancamento rollback
522300110207         IF  IMK19cmt = 'S' and OMK25F12 = 'S';
522400110207           rolbk;
522500110207         ENDIF;
522600110207
522700110207       ENDSR;
522800110207
522900100329
523000100329
523100100329       //--------------------------------------------------------------
523200100329       //?Gestione Richiesta Info Commerciali solo per alcune causali
523300100329       //?e solo in particolari situazioni
523400100329       //--------------------------------------------------------------
523500100329       BEGSR Sr_infocomm;
523600100329
523700100329       // verifico la causale esito attività è tra quelle che richiedono l'inserimento
523800100329       // delle info commerciali
523900120313
524000120313       If info_cac = *blanks ;
524100120313          leavesr ;
524200120313       endif ;
524300100329
524400100329       pos = %lookup(info_cac : $cac) ;
524500100329       select ;
524600100329       // se causale non presente si va a fine
524700100329       when  pos = 0  ;
524800100329       // se presente trattativa e non vuole info vado a fine
524900100329       when  $TRA(pos) = 'NO' and info_nrv > 0 ;
525000100329       // se richiede info solo se aperta trattativa e non è stata aperta vado a fine
525100100329       when $APT(pos) = 'SI' and info_nra = 0 ;
525200100329
525300100329       other ;
525400100329           exsr F14D02 ;
525500100329       endsl ;
525600100329
525700100329       Endsr ;
525800160804
525900160804       //--------------------------------------------------------------
526000160804       //?Scrivo l'immagine Precedente anagrafica Potenziale
526100160804       //--------------------------------------------------------------
526200160804       BEGSR ImmaginePrima;
526300160804
526400160804         clear TRMK30DS;
526500160804         IMK30immag = 'P';
526600160804         IMK30tvp = 'E';
526700160804         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
526800160804
526900160804       ENDSR;
527000160804
527100160804       //--------------------------------------------------------------
527200160804       //?Scrivo Storico Variazioni.
527300160804       //--------------------------------------------------------------
527400160804       BEGSR ScriviVariazione;
527500160804
527600160804         clear TRMK30DS;
527700160804         IMK30pru = knmus;
527800160804         IMK30noj = knmeb;
527900160804         IMK30pgm = 'TRMK19R';
528000160804         IMK30immag = 'D';
528100160804         IMK30cta = 'M';
528200160804         IMK30tvp = 'E';
528300160804
528400160804         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
528500160804
528600160804       ENDSR;
528700100329
528800090610       //--------------------------------------------------------------
528900090610       //?Operazioni iniziali.
529000090610       //--------------------------------------------------------------
529100090610       BEGSR RoutInz;
529200090610
529300090610         // Impostazione campi "fissi"
529400090610         T01pgm = SDSpgm;
529500090610
529600090610         // Reperimento dati job
529700090610         exsr DatiJob;
529800090610
529900110805         // Controllo se utente di sede
530000100210         // Controllo se utente autorizzato alla funzione
530100100210         reset TNTAA1DS;
530200100210         ITAA1caut = '§utegtc';
530300100210         kpjbu     = TNTAA1DS;
530400100210         tntaa1c (kpjba);
530500100210         TNTAA1DS = kpjbu;
530600100210
530700131127         // ATTENZIONE FLAG DI RITORNO UTILIZZATO ALL'INTERNO DEL PGM
530800100329
530900131127         IF  OTAA1fabi = 'N' and kcdaz <> 'MK01' ;
531000090610           $ErrGrave   = *on;
531100090610           errMessage  = *on;
531200090610           errGenerico = *on;
531300090610           V1Dmsg = $Msg(01);
531400090610           leavesr;
531500090610         endif;
531600090610
531700090610         // verifico se abilitare il commit oppure no
531800090610
531900090610            flgcom = (imk19cmt = 'S') ;
532000090610
532100090610            open tiatc01l ;
532200091119            open ticpn00f ;
532300090610
532400100721
532500100721             // verifico se già stato eseguito in fase di modifica o gestione
532600100721            chain(n) (imk19tat:imk19atn:imk19atnp) tiatc01l ;
532700100721            If  (imk19fle = 'G' or imk19fle = '2') and atcdco > 0 ;
532800100721                omk19err = 'E' ;
532900100721                $fine  = *on;
533000100721                omk19msg = 'Attività eseguita da altro utente' ;
533100100721                leavesr;
533200100721            endif;
533300111223
533400111223         //?Data del giorno
533500111223         wOggi = %dec(%date());
533600090610
533700090610       ENDSR;
533800090610
533900090610       //--------------------------------------------------------------
534000090610       //?Reperimento Dati del job (Utente/Operativi).
534100090610       //--------------------------------------------------------------
534200090610       BEGSR DatiJob;
534300090610
534400090610         in(E) §AzUte;
534500090610         if NOT %error;
534600090610           in(E) §DatiUte;
534700090610         endif;
534800090610         if %error or RSut = *blanks;
534900090610           clear TIBS34ds;
535000090610           tibs34r(tibs34ds);
535100090610           in §AzUte;
535200090610           in §DatiUte;
535300090610         endif;
535400090610
535500090610       ENDSR;
535600090610
535700090610       //--------------------------------------------------------------
535800090610       //?Operazioni finali.
535900090610       //--------------------------------------------------------------
536000090610       BEGSR RoutEnd;
536100090610
536200090610
536300100422         // comitto se richiesto e se non ci sono errori
536400100422            if imk19cmt = 'S' and omk19err = ' ';
536500100422               commit ;
536600100422            endif ;
536700100422         // rolbk  se richiesto e se  ci sono errori
536800100422            if imk19cmt = 'S' and omk19err <> ' ';
536900100422               rolbk  ;
537000100422            endif ;
537100130531
537200130531         //?Chiudo il TRMK20R
537300130531            clear TRMK20DS;
537400130531            IMK20tla = 'C';
537500130531            trmk20r ( kpjba : trmk20ds );
537600160804
537700160804       //?Chiudo file storicizzazione variazioni potenziale
537800160804         clear TRMK30DS;
537900160804         clear tncpo_30;
538000160804         clear tncpo1_30;
538100160804         clear ticpi_30;
538200160804         IMK30tla = 'C';
538300160804         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
538400130531
538500090610         *inLR = *on;
538600090610         return;
538700090610
538800090610       ENDSR;
538900090610
539000090610      /end-free
539100090610       //--------------------------------------------------------------
539200090610       //?Schiere a tempo di compilazione.
539300090610       //--------------------------------------------------------------
539400090610
539500090610** - $MSG -------------------------------------------------------------------*
539600100210Utente non abilitato alla funzione                                             01
539700100210Codice commerciale errato o non in gestione all'utente                         02
539800090611Data errata                                                                    03
539900110307Utente non autorizzato al cliente. Riassegnare l'attività a comm. abilitato.   04
540000090611Ora errata                                                                     05
540100100310Attività non possibile per Commerciale Interno.                                06
540200100721Attività già eseguita da altro utente uscire dal programma                     07
540300100308Obbligatorio inserire la causale attività                                      08
540400100920.....................                                                          09
540500100921.......................                                                        10
540600100920.....................                                                          11
540700121018.......................                                                        12
540800090702Ora inizio impegno incongruente con ora fine impegno                           13
540900090702Causale non utilizzabile                                                       14
541000090804Ore inizio e fine impegno incongruenti con ora appuntamento                    15
541100100210..........                                                                     16
541200091026Inserire ore inizio e fine impegno                                             17
541300091027Non è stato selezionato nessun cliente                                         18
541400091112Non si può utilizzare questa causale perchè trattativa già esistente           19
541500091112Non si può utilizzare questa causale perchè esiste già una trattativa          20
541600100114Immettere se Appuntamento effettuato.                                          21
541700100224Non si può annullare unica attività aperta della trattativa                    22
541800100225Causale non utilizzabile per questo tipo attività                              23
541900100225Causale non utilizzabile in quanto attività legata a trattativa con offerte    24
542000110502Causale non utilizzabile su potenziali                                         25
542100110502............   LIBERO                                                          26
542200100308Causale non utilizzabile legata ad una trattativa                              27
542300100308Presente trattativa già esitata in modo positivo. Causale inutilizzabile       28
542400100308Obbligatorio inserire delle note                                               29
542500100520Causale utilizzabile SOLO in presenza di trattativa                            30
542600100322Causale non utilizzabile in presenza di trattativa senza offerte               31
542700100329Obbligatorio inserire il Nome e indirizzo e-mail Resp.Trasporti                32
542800100421In caso di esecuzione di un'attività Post vendita utilizzare la causale 42     33
542900100504Fil. del codice commerciale incongruente con la fil. del cliente               34
543000100505Causale non utilizzabile per trattativa di tipo                                35
543100110321La causale xx non è utilizzabile per l'esecuzione di un'attività xx            36
543200100329** - $CAC -------------------------------------------------------------------*
543300120124242526284041719172                       Causali info comm
543400100329** - $TRA -------------------------------------------------------------------*
543500120104NONONONO  NO  NO                         NO = no legate a trattative
543600100329** - $APT -------------------------------------------------------------------*
543700120104              SI                         SI = solo se in apertura trattativa
