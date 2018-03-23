000100130809     /*END
000200090610      //--------------------------------------------------------------
000300090610      //?TRMK19R - GESTIONE CONTATTI / TELEFONATE / APPUNTAMENTI
000400090610      //--------------------------------------------------------------
000500090610
000600090610     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700090610     h dftactgrp(*no) actgrp(*caller)
000800090610
000900090610      //---------------------------------------------------------------
001000090610      //?Dichiarazione file.
001100090610      //---------------------------------------------------------------
001200090610
001300090610      // - Archivio Attività commerciale
001400090610     fTIATC01L  Uf a e           k disk    commit(flgcom) usropn
001500091210      // - Archivio Attività commerciale legami
001600091214     fTiatl01l  if   e           k disk    rename(tiatl000:tiatl01)
001700091210     fTiatl02l  if   e           k disk
001800090610      // - Anagrafica clienti potenziali
001900100115     fTNCPO01L  uf   e           k disk
002000140715     fTNCPO11L  if   e           k disk
002100090610      // - File contatti clienti e clienti potenziali
002200090610     fTFNTC01L  if   e           k disk
002300091119      // - Note  clienti potenziali
002400091119     fTICPN00F  if a e             disk    commit(flgcom) usropn
002500091113      // - Trattative
002600100520     fTIVIS05L  uf   e           k disk
002700100225      // - Offerte legate alla trattativa
002800101220     fTIVOF11L  if   e           k disk
002900110208      // - Offerte legate alla trattativa tariffa 999
003000110208     fTIVOF01L  uf   e           k disk    rename(tivof000:tivof999)
003100130809      // -?Anagrafica Commerciali?
003200130809     fAZCMM01L  if   e           k disk
003300100302      // - Anagrafica clienti
003400100302     fCNACO16L  if   e           k disk    rename(cnaco000:cnaco16)
003500090610
003600090610      // - Video Gestione contatti telefonate potenziali clienti
003700090610     fTRMK19D   cf   e             workstn
003800090610     f                                     indds(IndDspF)
003900090610     f                                     infds(InfDspF)
004000090610     f                                     usropn
004100090610
004200090610      //---------------------------------------------------------------
004300090610      //?Definizione costanti.
004400090610      //---------------------------------------------------------------
004500090610
004600090610      // - Tasti funzionali a video
004700090610     d c_F01           c                   const(x'31')
004800090610     d c_F02           c                   const(x'32')
004900090610     d c_F03           c                   const(x'33')
005000090610     d c_F04           c                   const(x'34')
005100090610     d c_F05           c                   const(x'35')
005200090610     d c_F06           c                   const(x'36')
005300090610     d c_F07           c                   const(x'37')
005400090610     d c_F08           c                   const(x'38')
005500090610     d c_F09           c                   const(x'39')
005600090610     d c_F10           c                   const(x'3A')
005700090610     d c_F11           c                   const(x'3B')
005800090610     d c_F12           c                   const(x'3C')
005900090610     d c_F13           c                   const(x'B1')
006000090610     d c_F14           c                   const(x'B2')
006100090610     d c_F15           c                   const(x'B3')
006200090610     d c_F16           c                   const(x'B4')
006300090610     d c_F17           c                   const(x'B5')
006400090610     d c_F18           c                   const(x'B6')
006500090610     d c_F19           c                   const(x'B7')
006600090610     d c_F20           c                   const(x'B8')
006700090610     d c_F21           c                   const(x'B9')
006800090610     d c_F22           c                   const(x'BA')
006900090610     d c_F23           c                   const(x'BB')
007000090610     d c_F24           c                   const(x'BC')
007100090610     d c_Enter         c                   const(x'F1')
007200090610     d c_RollDown      c                   const(x'F4')
007300090610     d c_RollUp        c                   const(x'F5')
007400090610
007500090610      // - Attributi di visualizzazione
007600090610      //  -  DspAtr() - Normale
007700090610     d C_dspatr_Norm   c                   const(x'20')
007800090610      //  -  DspAtr(RI)
007900090610     d C_dspatr_RI     c                   const(x'21')
008000090610     d C_dspatr_HI     c                   const(x'22')
008100090610      //  -  DspAtr(ND)
008200090610     d C_dspatr_ND     c                   const(x'27')
008300090610      //  -  DspAtr(BL) / Color(Red)
008400090610     d C_dspatr_BL     c                   const(x'28')
008500090611
008600090611      // - Titoli a video
008700091216     d Tit_primo       c                   const('        GESTIONE         ')
008800090611
008900091216     d Tit_visua       c                   const('     VISUALIZZAZIONE     ')
009000090611
009100091216     d Tit_manut       c                   const('      MANUTENZIONE       ')
009200090611
009300091216     d Tit_gesti       c                   const('        GESTIONE         ')
009400091216
009500091216      // - Titoli a video D02 parte bassa
009600100215     d c_PrimaAtt      c                   const('ATTIVITA'' IN ESECUZIONE')
009700091216     d c_Attivita      c                   const('ATTIVITA'' ESEGUITA')
009800100108
009900100108     d Digits          c                   const('0123456789')
010000090611
010100090610      //---------------------------------------------------------------
010200090610      //?Definizione schiere.
010300090610      //---------------------------------------------------------------
010400090610
010500090610      // - Messaggi di errore
010600130809     d $Msg            s             78    dim(36) ctdata perrcd(1)
010700110707      // - Causali che richiedono info commerciali e che non chiudono trattativa
010800100329     d $Cac            s              2    dim(20) ctdata perrcd(20)
010900100329      // - Corrispondente alle causali senza trattativa
011000100329     d $tra            s              2    dim(20) ctdata perrcd(20)
011100100329      // - Corrispondente alle causali apertura trattativa
011200100329     d $Apt            s              2    dim(20) ctdata perrcd(20)
011300121018      // - Tipi trattativa possibili per ogni attività
011400101220     d §ccoTPVds       ds            10    inz
011500101220     d   $Tpv                         1    inz  dim(10)
011600100225       // - Tipi attività possibili per ogni Causale Attività
011700100225     d §ccoTAAds       ds            10    inz
011800100225     d   $Taa                  1     10    inz  dim(10)
011900110502       // - Categorie potenziali abilitate
012000110502     d §ccoCpods       ds            10    inz
012100110502     d   $Cpo                  1      4    inz  dim(4)
012200090610
012300091020      // - Clienti
012400091020     d ksa             s              4    Dim(30)
012500091020     d ksc             s              7  0 Dim(30)
012600090610
012700090610      //---------------------------------------------------------------
012800090610      //?Definizione aree dati.
012900090610      //---------------------------------------------------------------
013000090610
013100090610      // - Dati utente
013200090610     d §AzUte        e ds                  extname(AZUTE00F)
013300090610     d                                     dtaara
013400090610     d §DatiUte      e ds                  extname(dDatiUte)
013500090610     d                                     dtaara
013600090610
013700090610      //---------------------------------------------------------------
013800090610      //?Definizione strutture dati.
013900090610      //---------------------------------------------------------------
014000090610
014100090610      // - Status
014200090610     d Psds           sds
014300090610     d   SDSpgm          *proc
014400090610
014500090610      // - InfDS
014600090610     d InfDspF         ds
014700090610     d  dsp_aid              369    369a                                        AID byte
014800090610
014900090610      // - Indicatori su DspF
015000090610     d IndDspF         ds
015100090610        // - Indicatori di errore 1° videata
015200090610     d  errMessage                    1n   overlay(IndDspF : 28)
015300090610     d  PosCurCom02                   1n   overlay(IndDspF : 60)
015400090610     d  PosCurDat02                   1n   overlay(IndDspF : 61)
015500090610     d  PosCurOra02                   1n   overlay(IndDspF : 62)
015600090610     d  PosCurCae02                   1n   overlay(IndDspF : 64)
015700090610     d  PosCurScd02                   1n   overlay(IndDspF : 65)
015800090610     d  PosCurSco02                   1n   overlay(IndDspF : 66)
015900090610     d  PosCurCmi02                   1n   overlay(IndDspF : 67)
016000090610     d  PosCurOii02                   1n   overlay(IndDspF : 68)
016100090610     d  PosCurOfi02                   1n   overlay(IndDspF : 69)
016200091216     d  PosCurNo102                   1n   overlay(IndDspF : 75)
016300100114     d  PosCurFatto                   1n   overlay(IndDspF : 81)
016400090610        // - Indicatori di errore window
016500090610     d  PosCurDsc03                   1n   overlay(IndDspF : 71)
016600090610     d  PosCurOsc03                   1n   overlay(IndDspF : 72)
016700090610     d  PosCurCom03                   1n   overlay(IndDspF : 73)
016800090701     d  PosCurOii03                   1n   overlay(IndDspF : 79)
016900090701     d  PosCurOfi03                   1n   overlay(IndDspF : 80)
017000090610        // - Indicatori di errore generico
017100090610     d  errGenerico                   1n   overlay(IndDspF : 99)
017200090610        // - Indicatori di visualizzazione
017300150126     d  F04Attivo                     1n   overlay(IndDspF : 04)
017400091215     d  primocontatto                 1n   overlay(IndDspF : 10)
017500090610     d  altrenote                     1n   overlay(IndDspF : 11)
017600090610     d  giafatto                      1n   overlay(IndDspF : 13)
017700090610     d  piunote                       1n   overlay(IndDspF : 14)
017800090610     d  manutenzione                  1n   overlay(IndDspF : 15)
017900090610     d  visualizza                    1n   overlay(IndDspF : 16)
018000090610     d  telefonata                    1n   overlay(IndDspF : 17)
018100090610     d  appuntamento                  1n   overlay(IndDspF : 18)
018200090623     d  vis_nonfatto                  1n   overlay(IndDspF : 19)
018300090612     d  potenziali                    1n   overlay(IndDspF : 20)
018400100928     d  Gestione                      1n   overlay(IndDspF : 21)
018500090630     d  prox_telefon                  1n   overlay(IndDspF : 22)
018600090630     d  prox_appunta                  1n   overlay(IndDspF : 23)
018700100309     d  vis_esito                     1n   overlay(IndDspF : 24)
018800090714     d  vis_trattativ                 1n   overlay(IndDspF : 25)
018900091021     d  cliente                       1n   overlay(IndDspF : 26)
019000091021     d  tratta_new                    1n   overlay(IndDspF : 27)
019100091022     d  affiancamento                 1n   overlay(IndDspF : 29)
019200091105     d  ric_visualizz                 1n   overlay(IndDspF : 30)
019300091105     d  ric_agenda                    1n   overlay(IndDspF : 31)
019400091105     d  ric_trattativ                 1n   overlay(IndDspF : 32)
019500100519     d  F16Attivo                     1n   overlay(IndDspF : 33)
019600091113     d  Trattativa                    1n   overlay(IndDspF : 34)
019700091118     d  Nuova                         1n   overlay(IndDspF : 35)
019800091210     d  F20Affianca                   1n   overlay(IndDspF : 37)
019900091214     d  ConAffianca                   1n   overlay(IndDspF : 38)
020000100108     d  Giornototale                  1n   overlay(IndDspF : 39)
020100100803     d  NO_F21                        1n   overlay(IndDspF : 40)
020200090610
020300090610     d WindDspF        ds                  inz
020400090610     d   WindDspFa             1     49    inz(*zero)
020500090610     d   WindDspFb            50     99    inz(*zero)
020600090610
020700090610      //  codice cliente potenziale a video 2
020800090610     d V02cpo          ds                  inz
020900090610     d   V02cp1                            inz
021000090610     d   V02cp2                            inz
021100090610      //  ora esecuzione contatto
021200090610     d SAV02ora        ds                  inz
021300090610     d   ora                   1      2  0
021400090610     d   minuti                3      4  0
021500090610
021600090610     d wlbdat          ds                  inz
021700090610     d  g02dat                 1      8  0
021800090610     d  g02inv                 9     16  0
021900090610     d  g02err                17     17
022000090610     d  g02tgi                18     22  0
022100090610
022200090610      // - Parametri ricevuti
022300090610     d KPJBA         e ds
022400090610     d TRMK19DS      e ds
022500090610
022600090610      // - Reperimento dati utente
022700090610     d TIBS34ds      e ds
022800090610
022900090610       // - Reperimento dati anagrafici
023000090610     d TIBS69ds      e ds
023100090610     d DS_cnaco      e ds                  inz extname(CNACO00F)
023200090610     d DS_cnind      e ds                  inz extname(CNIND00F)
023300090610     d DS_cnclp      e ds                  inz extname(CNCLP00F)
023400090610     d DS_fncls      e ds                  inz extname(FNCLS00F)
023500090610
023600090610      // - Ricerca/Controllo tabelle
023700090610     d TIBS02ds      e ds                  inz
023800090610     d   T02Mod      e                     inz('C')
023900100309
024000090610      // - Ricerca Causale attività
024100090610     d Tntb74ds      e ds                  inz
024200100309
024300100309      // - FLO del file attività
024400100309     d datc01        e ds                  inz
024500120622
024600120622      // - CPORST file TNCPO
024700120622     d dCPO01        e ds                  inz
024800101013
024900101013      // - FLO del file TRATTATIVA
025000101013     d dvis01        e ds                  inz
025100090610
025200090610      // - Tabella CCO = Causale contatto
025300090610     d dcco          e ds                  inz
025400110502
025500110502      // - Tabella CPO = Categoria Potenziale
025600110502     d dcpo          e ds                  inz
025700090610
025800091119      // - Tabella TTA = Tipo attività
025900091119     d dtta          e ds                  inz
026000100614
026100100614      // - Tabella TTR = Tipo trattativa
026200100614     d dttr          e ds                  inz
026300090610
026400090610      // - Ricerca ultimo numero contatti
026500090610     d trul33ds      e ds                  inz
026600100413
026700100413      // - Ricerca ultimo numero trattativa
026800100413     d trul48ds      e ds                  inz
026900100413
027000110429      // - Interrogaizone Info Commerciale
027100090610     d TRMK50ds      e ds                  inz
027200100928
027300100928      // - Interrogazione attività
027400100928     d TRMK21DS      e ds                  inz
027500090610
027600100921      // - Invio materiale illustrativo
027700100921     d TRMK24ds      e ds                  inz
027800090610      // - Interrogazione anagrafica clienti potenziali
027900100409     d TRMK01ds      e ds
028000110225
028100110225      // - Controllo se potenziale lavora
028200110225     d TRMK04ds      e ds                  inz
028300110304
028400110304      // - Calcolo categoria potenziale
028500110304     d TRMK05ds      e ds
028600110225
028700100409      // - Interrogazione anagrafica clienti potenziali
028800100409     d TRMK01ds1     e ds                  extname(trmk01ds) prefix(W_)
028900090610      // -  Gestione Note clienti/potenziali
029000090610     d TRMK20ds      e ds                  inz
029100091210      // -  Interrogazione/Manutenzione affiancamento
029200091210     d TRMK22ds      e ds
029300100329      // -  Invio mail conferma appuntamento
029400100915     d TRMK23ds1     e ds
029500110207      // -  Gestione attività blocco clienti
029600110207     d TRMK25ds      e ds
029700091027      // -  Interrogazione agenda
029800091027     d TRMK82ds      e ds                  inz
029900091216      // -  Controllo se commerciale impegnato
030000091216     d TRMK84ds      e ds
030100091215      // -  Controllo se ci sono altre attività aperte
030200091215     d TRMK85ds      e ds                  inz
030300100308      // - Gestione Contatti clienti/potenziali
030400090610     d TNTA12DS      e ds                  inz
030500100430     d  esi_ta16c      s              1
030600091021      // - Gestione anagrafica clienti
030700091021     d tnta60ds      e ds                  inz
030800101221      // - Gestione Data caldo senza offerte
030900101221     d TNTA63DS      e ds                  inz
031000091015      // - Gestione trattative commerciali
031100091015     d TNTA88ds      e ds                  inz
031200091210      // - Gestione esito offerta
031300091210     d TNTA56ds      e ds                  inz
031400100330      // - Controllo validità delle date
031500100330     d Xgiolavds     e ds                  inz
031600091019      // - Controllo abilitazioni
031700091019     d Tntaa1ds      e ds                  inz
031800100924      // - Gestione/interrogazione Informazioni trattativa
031900100924     d TNTA41ds      e ds                  inz
032000091112      // - Controllo se ci sono trattative aperte per lo stesso potenziale
032100091112     d Tnta43ds      e ds                  inz
032200091216      // - Controllo se commerciale diverso da commerciale trattativa
032300091216     d Tnta45ds      e ds                  inz
032400091019
032500090610      //---------------------------------------------------------------
032600090610      //?Definizione variabili globali.
032700090610      //---------------------------------------------------------------
032800090610
032900090610      // - Flags booleani
033000091215     d $EoF            s               n   inz(*off)
033100100517     d $Fine           s               n   inz(*off)
033200100517     d $Fine5          s               n   inz(*off)
033300100517     d $Fine6          s               n   inz(*off)
033400100517     d $Fine7          s               n   inz(*off)
033500100614     d $Fine8          s               n   inz(*off)
033600110225     d $Fine9          s               n   inz(*off)
033700090610     d $InzD02         s               n   inz(*on)
033800100108     d $InzD04         s               n   inz(*on)
033900090610     d $InzW03         s               n   inz(*on)
034000090610     d $ErrGrave       s               n   inz(*off)
034100091113     d $NumOK          s               n   inz(*off)
034200090610     d $prima          s               n   inz(*off)
034300090610     d $dasbf          s               n   inz(*off)
034400091116     d $new_Trattati   s               n   inz(*off)
034500091216     d $ForzaImpegno   s               n   inz(*off)
034600100310     d $ForzaComm      s               n   inz(*off)
034700100422     d $ForzaComm3     s               n   inz(*off)
034800100310     d $ForzaComin     s               n   inz(*off)
034900100318     d $ForzaComin2    s               n   inz(*off)
035000100308     d $Chiedi_conf    s               n   inz(*off)
035100100322     d $Gia_codifica   s               n   inz(*off)
035200110225     d $Lavora         s               n   inz(*off)
035300100614     d $ModAffianca    s               n   inz(*off)
035400100519     d $OkDel          s               n   inz(*off)
035500110405     d $Cli_non_sped   s               n   inz
035600090610
035700090610      // - Indici di schiera
035800090610     d Pos             s              3  0 inz
035900090610
036000090610      // - Campi associati al video
036100090610     d $Video          s              2    inz('D2')
036200090610
036300090610     d dsp_mod         s             10I 0
036400090610
036500090610      // - Campi di comodo data
036600090610     d  data_eur       s               d   Datfmt(*eur)
036700090610     d  data_iso       s               d   Datfmt(*iso)
036800090610
036900090610      // - Campi di comodo
037000091020     d  savcli         s                   like(acoksc)
037100140929     d  wchiudo_tr     s              1    inz
037200090610     d  w02dat         s              8  0
037300090610     d  w02scd         s              8  0
037400100108     d  w04dad         s              8  0
037500090610     d  ww03dsc        s              8  0
037600091113     d  newnrv         s              7  0
037700100504     d  W_vistpv       s                   like(vistpv)
037800091119     d  destina        s            120
037900100329     d  info_cac       s                   like(atccac)
038000100329     d  info_nrv       s                   like(atcnrv)
038100100329     d  info_nra       s                   like(atcnra)
038200100311     d  sav_v02cmi     s                   like(v02cmi)
038300100318     d  sav_v02cmi2    s                   like(v02cmi)
038400100311     d  sav_w03com     s                   like(w03com)
038500100422     d  sav_w03comt    s                   like(w03com)
038600100616     d app_atccmm      s                   like(ATCcmm)
038700101221     d sav_DataHot     s                   like(OTA63dpa)
038800090610
038900110412     d parmcbl         s              3A
039000110412
039100091218     d sav_atcdad      s                   like(atcdad)
039200111102     d sav_atchda      s                   like(atchda)
039300111102     d sav_atcoii      s                   like(atcoii)
039400111102     d sav_atcofi      s                   like(atcofi)
039500111102     d sav_atccmm      s                   like(atccmm)
039600091214     d sav_atlatnp     s                   like(atlatnp)
039700091218     d wdata           s              8  0 inz
039800140411     d wHhMm           s              4  0 inz
039900091216     d wpos            s              5i 0 inz
040000100108     d wtitolo         s             55A
040100100118     d sav_newnrv      s                   like(newnrv)
040200100118     d sav_newtrat     s                   like($new_trattati)
040300111223     d wOggi           s              8s 0
040400090610
040500090610      //---------------------------------------------------------------
040600090610      //?Definizione procedure usate.
040700090610      //---------------------------------------------------------------
040800091210
040900100409     d TRMK02C         pr                  extpgm('TRMK02C')
041000100409     d  kpjba                              likeds(kpjba)
041100100409     d  trmk01ds                           likeds(trmk01ds)
041200110225
041300110225      // - Controllo se potenziale lavora
041400110225     d Trmk04R         pr                  extpgm('TRMK04R')
041500110225     d  KPJBA                              likeds(KPJBA)
041600110225     d  TRMK04ds                           likeds(TRMK04ds)
041700110301     d  TRMK85ds                           likeds(TRMK85ds) options(*nopass)
041800110304
041900110304      // - Calcolo categoria potenziale
042000110304     d Trmk05r         pr                  extpgm('TRMK05R')
042100110304     d  kpjba                              likeds(KPJBA)
042200110304     d  trmk05ds                           likeds(TRMK05ds)
042300100409
042400091210     d TRMK22R         pr                  extpgm('TRMK22R')
042500091210     d  kpjba                              likeds(kpjba)
042600091210     d  trmk22ds                           likeds(trmk22ds)
042700110207
042800110207     d TRMK25R         pr                  extpgm('TRMK25R')
042900110207     d  kpjba                              likeds(kpjba)
043000110207     d  trmk25ds                           likeds(trmk25ds)
043100100928
043200100928      // - Interrogazione attività
043300100928     d trmk21r         pr                  extpgm('TRMK21R')
043400100928     d  kpjba                              likeds(KPJBA)
043500100928     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
043600100921
043700100921     d trmk24r         pr                  extpgm('TRMK24R')
043800100921     d   kpjba                             likeds(KPJBA)
043900100921     d   trmk24ds                          likeds(TRMK24ds)
044000090610
044100091026     D TRMK82R         pr                  extpgm('TRMK82R')
044200091113     D  kpjba                              likeds(kpjba)
044300091216
044400091216     d TRMK84R         pr                  extpgm('TRMK84R')
044500091216     d  trmk84ds                           likeds(trmk84ds)
044600091215
044700091215     d TRMK85R         pr                  extpgm('TRMK85R')
044800091215     d  trmk85ds                           likeds(trmk85ds)
044900100430
045000091112     D Tnta43R         pr                  extpgm('TNTA43R')
045100091113     D  tnta43ds                           likeds(tnta43ds)
045200091216
045300091216     d TNTA45R         pr                  extpgm('TNTA45R')
045400091216     d  tnta45ds                           likeds(tnta45ds)
045500101221
045600101221      // - Gestione data caldo
045700101221     d Tnta63r         pr                  extpgm('TNTA63R')
045800101221     d  kpjba                              likeds(kpjba)
045900101221     d  tnta63ds                           likeds(tnta63ds)
046000100330
046100100413     d TRUL48R         pr                  extpgm('TRUL48R')
046200100413     d  kpjba                              likeds(kpjba)
046300100413     d  trul48ds                           likeds(trul48ds)
046400150126
046500150126       // -?Avanzamento Campagna (per visualizzazione)?
046600150126     d TRKC07ds      e ds                  inz
046700150126     d   iKC07ric    e                     inz('5')
046800150126     d TRKC07R         pr                  extpgm('TRKC07R')
046900150126     d   kpjba                             likeds(KPJBA)
047000150126     d   trkc07ds                          likeds(TRKC07ds)
047100150126
047200150126       // -?Controllo se cliente in (almeno una) Campagna?
047300150126     d TRKC73ds      e ds                  inz
047400150126     d TRKC73R         pr                  extpgm('TRKC73R')
047500150126     d   kpjba                             likeds(kpjba)
047600150126     d   trkc73ds                          likeds(TRKC73ds)
047700100413
047800100330      // - Aggiunge/Toglie gg/mm dalla data
047900100330     d Xgiolav         pr                  extpgm('XGIOLAV')
048000100330     d  xgiolavds                          likeds(xgiolavds)
048100100330
048200091112      //---------------------------------------------------------------
048300090610      //?prototipi
048400090610      //---------------------------------------------------------------
048500090610      /copy gaitrasrc/srcprotopr,tibs02r
048600090610      /copy gaitrasrc/srcprotopr,tibs34r
048700090610      /copy gaitrasrc/srcprotopr,tibs69r
048800091019      /copy gaitrasrc/srcprotopr,tntaa1c
048900091019      /copy gaitrasrc/srcprotopr,tnta12r
049000100924      /copy gaitrasrc/srcprotopr,tnta41r
049100091210      /copy gaitrasrc/srcprotopr,tnta56r
049200091210      /copy gaitrasrc/srcprotopr,tnta60r
049300091210      /copy gaitrasrc/srcprotopr,tnta88r
049400090701      /copy gaitrasrc/srcprotopr,tntb74r
049500090610      /copy gaitrasrc/srcprotopr,trmk20r
049600100915      /copy gaitrasrc/srcprotopr,trmk23r1
049700130809      /copy gaitrasrc/srcprotoPI,TRMK43R_1
049800130809      /copy gaitrasrc/srcprotoPR,TRMK43R
049900090610      /copy gaitrasrc/srcprotopr,trmk50r
050000090610      /copy gaitrasrc/srcprotopr,trul33r
050100090610      /copy gaitrasrc/srcprotopr,xsrda8
050200090611      /copy gaitrasrc/srcprotopr,QsnQryModS
050300090610
050400090610      //---------------------------------------------------------------
050500090610      //?Definizione key-list.
050600090610      //---------------------------------------------------------------
050700090610
050800091210       // - File TIATL02L
050900091210     d k02tiatl      e ds                  extname(TIATL02L:*key)
051000091210     d                                     prefix(k_)
051100091210     d                                     inz
051200090610
051300090610      //---------------------------------------------------------------
051400090610      //?Riepilogo indicatori.
051500090610      //---------------------------------------------------------------
051600090610      // - visualizzazioni particolari
051700150126      // 04    : Attiva F4=Campagna
051800090610      // 10    : Emissione particolare prima telefonata
051900090610      // 11    : Emissione "Altre note" nel caso in cui ci siano + di 2 righe
052000090610      // 13    : Indica un'attività già eseguita
052100090610      // 14    : Emissione "+" nel caso in cui ci siano + di 2 note
052200090610      // 15    : Manutenzione di un'attività
052300090610      // 16    : Visualizza un'attività
052400090610      // 17    : Identifica un'attività telefonica
052500090610      // 18    : Identifica un'attività di appuntamento
052600090623      // 19    : Visualizza un'attività non fatta
052700091105      // 20    : Disabilita F1 Anagrafica se richiamato dal pgm dei potenziali e cliente a zero
052800100928      // 21    : Gestione di un'attività
052900090630      // 22    : Identifica una telefonata nella window
053000090630      // 23    : Identifica un appuntamento nella window
053100100309      // 24    : Visualizza l'esito solo se esiste
053200091019      // 25    : Visualizza la trattativa
053300091019      // 26    : Visualizza il codice cliente in testata ed pilota F1 Anagrafica Cliente
053400091022      // 27    : Visualizza il numero trattativa nuovo
053500091022      // 29    : Visualizza se appuntamento di affiancamento
053600091105      // 30    : No tasti funzionali - F12 se richiamato in visualizzazione da altri pgm - trmk18
053700091105      // 31    : Disabilito il tasto funzionale f11 agenda se richiamato da Agenda
053800091105      // 32    : Disabilito il tasto funzionale f09 Trattativa se richiamato da Trattativa
053900091113      // 34    : Visualizzo la trattativa legata all'attività futura che vado a scadenziare
054000091118      // 35    : Visualizzo NUOVA se la trattativa è nuova
054100091119      // 36    : Segnalo la presenza di più trattative
054200091215      // 37    : Abilita F20=Affiancamento
054300091215      // 38    : Visualizza che appuntamento è con affiancamento
054400090610      // - Comuni
054500090610      // 28    : Emissione messaggio di errore a video
054600090610      // - D02
054700090610      // 60    : Errore: Commerciale che esegue l'attività
054800090610      // 61    : Errore: Data attività
054900090610      // 62    : Errore: Ora attività
055000090610      // 64    : Errore: Causale attività
055100090610      // 65    : Errore: Scadenza data attività
055200090610      // 66    : Errore: Scadenza ora attività
055300090610      // 67    : Errore: Commerciale che edeve eseguire l'attività
055400090610      // 68    : Errore: Ora inizio impegno appuntamento attività
055500090610      // 69    : Errore: Ora fine   impegno appuntamento attività
055600091216      // 75    : Posizionamento cursore su campo Nota
055700091215      // - W03
055800091215      // 71    : Errore: Data appuntamento
055900091215      // 72    : Errore: Ora  appuntamento
056000091215      // 73    : Errore: Commerciale appuntamento
056100091215      // 79    : Errore: Ora inizio impegno
056200091215      // 80    : Errore: Ora fine   impegno
056300090610      // - Comuni
056400090610      // 99    : Generico di Errore
056500090610      //---------------------------------------------------------------
056600090610
056700090610      //---------------------------------------------------------------
056800090610      //?M A I N - L I N E
056900090610      //---------------------------------------------------------------
057000090610
057100090610     c     *Entry        plist
057200090610     c                   parm                    KPJBA
057300090610     c                   parm                    trmk19ds
057400100409     c                   parm                    trmk01ds1
057500100409
057600090610
057700090610      /free
057800090610
057900090611       // Operazioni iniziali controllo se video accetta 132 caratteri
058000090611       if not %open(trmk19d);
058100090611           dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
058200090611           If dsp_mod <> 0 ;
058300090611              open trmk19d;
058400090611           else ;
058500090611              eval $fine = *on ;
058600090611           endif;
058700090611       endif;
058800090610
058900090610       // Operazioni iniziali
059000090610       exsr RoutInz;
059100090610
059200100616       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
059300100616
059400091021       clear savcli ;
059500091021
059600091019       // Verifico se si tratta di primo contatto potenziale
059700090611       if imk19fle = 'P' ;
059800091019          exsr Primo_Po  ;
059900090611       endif ;
060000091019
060100091019       // Verifico se si tratta di primo contatto Cliente
060200091019       if imk19fle = 'C' ;
060300091019          exsr Primo_Cli ;
060400100427          If omk19err <> ' ' ;
060500100427             $fine = *on ;
060600100427          endif ;
060700091019       endif ;
060800090701
060900100108
061000100108       // Se richiamato da Agenda verifico se attività legata a cliente
061100100108       // o attività d'ufficio
061200100108       IF  IMK19fpr = 'A';
061300100108         clear dTTA;
061400100108         clear TIBS02ds;
061500100108         T02mod = 'C';
061600100108         T02cod = 'TTA';
061700100108         T02ke1 = IMK19tco ;
061800100108         T02sif = KNSIF;
061900100108         TNTBE_RicercaControllo  (kpjba : tibs02ds);
062000100108         IF  T02err = *blanks;
062100100108           dTTA = t02uni ;
062200100108         ENDIF;
062300100108         IF  §TTAleg <> 'S';
062400100108           $InzD02 = *off;
062500100108           $InzW03 = *off;
062600100108           $InzD04 = *on;
062700100108           $Video = 'D4';
062800100108         ENDIF;
062900100108       ENDIF;
063000090611
063100090610       // Gestione video
063200090610       DOW $Fine = *off;
063300090610         select;
063400090610           when $Video = 'D2';
063500090610             exsr GesD02;
063600100224           when $Video = 'W5';
063700100224             exsr GesW05;
063800090610           when $Video = 'W3';
063900090610             exsr GesW03;
064000100108           when $Video = 'D4';
064100100108             exsr GesD04;
064200100614           when $Video = 'W8';
064300100614             exsr GesW08;
064400110225           when $Video = 'W9';
064500110225             exsr GesW09;
064600110530           when $Video = 'W0';
064700110530             exsr GesW10;
064800090610           other;
064900090610             $Fine = *on;
065000090610         endsl;
065100090610       ENDDO;
065200090610
065300090610
065400090610       // Operazioni finali
065500090610       exsr RoutEnd;
065600090611
065700090611       //--------------------------------------------------------------
065800091019       //?Gestione Primocontatto  Potenziale
065900090611       //--------------------------------------------------------------
066000091019       BEGSR Primo_Po ;
066100090611
066200090611        // se potenziale valido recupero i dati del potenziale da immettere nella prima
066300090611        // parte della videata
066400090611
066500100409         Cpocpo = W_mk01cpo ;
066600100409         W_mk01F06 = ' ' ;
066700090611         primocontatto = *on ;
066800090611         $Video = 'D2';
066900090611         $InzD02 = *on;
067000090611         $prima  = *on;
067100090611
067200090611       ENDSR;
067300090610
067400091019       //--------------------------------------------------------------
067500091019       //?Gestione Primocontatto  Cliente
067600091019       //--------------------------------------------------------------
067700091019       BEGSR Primo_Cli ;
067800100802
067900100802        // se cliente valido recupero i dati del cliente da immettere nella prima
068000100802        // parte della videata
068100100802
068200100802         savcli = IMK19ksc;
068300100802         clear  tibs69ds;
068400100802         I69kac = savcli;
068500100802         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
068600100802         if acolib = 0 ;
068700100802            omk19ksc = savcli ;
068800100802            OMK19ERR = 'E' ;
068900100802            OMK19MSG = 'Manca Potenziale sul cliente impossibile gestire nuove +
069000100802            attività' ;
069100100802         else ;
069200100802            primocontatto = *on ;
069300100802            $Video = 'D2';
069400100802            $InzD02 = *on;
069500100802            $prima  = *on;
069600100802         endif ;
069700091019
069800091019
069900091019       ENDSR;
070000091019
070100090610       //--------------------------------------------------------------
070200090610       //?Gestione videata D02
070300090610       //--------------------------------------------------------------
070400090610       BEGSR GesD02;
070500090610
070600090610         // Inizializzazione videata
070700090610         if $InzD02 = *on;
070800090610           exsr InzD02;
070900090610           $InzD02 = *off;
071000090610         endif;
071100110301
071200110301       //?Se dopo la INZ del video non devo emettere la videata ma la win con
071300110301       //?il messaggio di annullo attività esco dalla routine
071400110527         IF  $Video = 'W9';
071500110301           leavesr;
071600110301         ENDIF;
071700110530       //?Se dopo la INZ del video non devo emettere la videata ma la win con
071800110530       //?il messaggio di avviso attività da annullare esco dalla routine
071900110530         IF  $Video = 'W0';
072000110530           leavesr;
072100110530         ENDIF;
072200101221
072300101221         clear sav_DataHot;
072400090610
072500090610         // Emissione testata
072600090610           write MK19T01;
072700090610
072800090610         // Emissione videata
072900090610         exfmt MK19D02;
073000090610         errMessage   = *off;
073100090610         errGenerico  = *off;
073200090610         clear V1Dmsg;
073300090610
073400090610         select;
073500090610         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
073600090610           when  $ErrGrave = *on;
073700090610             $Fine  = *on;
073800090610         // F12 = Ritorno
073900090610           when dsp_aid = c_F12;
074000090610             exsr F12D02;
074100090611             leavesr ;
074200090701         // F21 = invio mail
074300090701           when dsp_aid = c_F21;
074400100921                If V02rtr = *blanks or v02mrt = *blanks ;
074500100921                   errMessage  = *on;
074600100921                   errGenerico = *on;
074700100921                   V1Dmsg = $Msg(32);
074800100921                   leavesr;
074900100921                 endif;
075000090701             exsr F21d02;
075100090701             leavesr ;
075200091021         // F01 = Anagrafica
075300090610           when dsp_aid = c_F01;
075400090610             exsr F01D02;
075500150126           // -?F4=Campagna?
075600150126           when  dsp_aid = c_F04;
075700150126             exsr  F04D02;
075800090610         // F18 = Note
075900090610           when dsp_aid = c_F18;
076000090610             exsr F18D02;
076100090610         // F02 = Contatti
076200090610           when dsp_aid = c_F02;
076300090610             exsr F02D02;
076400100511             leavesr ;
076500090610         // F14 = Informazioni commerciali
076600090610           when dsp_aid = c_F14;
076700090610             exsr F14D02;
076800091026         // F09 = Trattativa
076900091026           when dsp_aid = c_F09;
077000091026             exsr F09D02;
077100091026         // F11 = Agenda
077200091026           when dsp_aid = c_F11;
077300091026             exsr F11D02;
077400091026             leavesr ;
077500091210         //?F20 = Affiancamento
077600091210           when dsp_aid = c_F20;
077700091210             exsr F20D02;
077800091210             leavesr ;
077900100301         // F16 = Annullamento abilitato solo in manutenzione da trattativa
078000100224           when dsp_aid = c_F16;
078100100224             exsr F16D02;
078200100224             leavesr ;
078300100928         //?F05 = Attività
078400100930         //  when dsp_aid = c_F05;
078500100930         //    exsr F05D02;
078600100930         //    leavesr ;
078700090610
078800090610         // Enter
078900090610           other;
079000090610         // Se non si tratta di visualizzazione
079100090610            If visualizza = *off ;
079200090610               exsr CtrD02;
079300090610               if errGenerico = *on ;
079400090610                   leavesr;
079500090610               endif;
079600090610            endif;
079700090610           endsl;
079800090610
079900100329         // se dato f8 conferma appuntamento + invio mail prima verifico se ha indirizzo mail
080000100329         // altrimenti errore
080100100329           If   dsp_aid = c_F08 ;
080200100329                If V02rtr = *blanks or v02mrt = *blanks ;
080300100329                   errMessage  = *on;
080400100329                   errGenerico = *on;
080500100329                   PosCurCmi02 = *on;
080600100329                   V1Dmsg = $Msg(32);
080700100329                   leavesr;
080800100329                 endif;
080900100329                    exsr F06D02 ;
081000100329
081100100329         // Ritorno al programma precedente
081200100329                    $fine  = *on ;
081300100329                    leavesr ;
081400100329           endif ;
081500100329
081600100329         // se dato f6 conferma
081700090610           If   dsp_aid = c_F06 ;
081800090610
081900090610         // -...- VISUALIZZAZIONE vado via
082000090610                If  visualizza   = *on ;
082100090610                    leavesr ;
082200090610                endif ;
082300090610
082400090610         // -...- MANUTENZIONE
082500090610                If  manutenzione = *on ;
082600090610                    exsr F06D02 ;
082700090610                    leavesr ;
082800090610                endif ;
082900090610
083000090610         // -...- GESTIONE
083100100308
083200100308         // verifico se risulta necessario fare delle domandine
083300100308
083400100308         // chiedo esito attività/trattativa    se causale lo richiede
083500100308             If  §ccores = 'S' ;
083600100308                  exsr Ges_Esito  ;
083700100308             endif ;
083800101221
083900101221         //?Se attività prevede la richiesta della data caldo senza offerta
084000101221         //?richiamo il pgm
084100101221             IF  §CCOhot = 'S';
084200101221               exsr Ric_TA63;
084300101221         //?se F12 o errore devo tornare alla videata dell'esito attività
084400101221               IF  OTA63f12 = 'S' or OTA63err <> *blanks;
084500101221                 leavesr;
084600101221               ENDIF;
084700101221         //?memorizzo la data caldo senza offerta
084800101221               sav_DataHot = OTA63dpa;
084900101221             ENDIF;
085000110301
085100110301            SELECT;
085200110301
085300110301         //?Se attività prevede il richiamo al programma di gestione anagrafica clienti
085400110301         //?richiamo il pgm
085500110301             WHEN  §CCOpgmblc <> *blanks;
085600110301           //?verifico se l'utente può gestire il cliente
085700110301               reset TNTAA1DS;
085800110302               ITAA1ksc  = V02cli;
085900110301               ITAA1caut = '§UTECLI';
086000110301               kpjbu     = TNTAA1DS;
086100110301               tntaa1c (kpjba);
086200110301               TNTAA1DS = kpjbu;
086300110301               IF  OTAA1fabi = 'N';
086400110301                 chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
086500110307                 V1Dmsg = $msg(04);
086600110301                 errMessage  = *on;
086700110301                 errGenerico = *on;
086800110301                 leavesr;
086900110301               ENDIF;
087000110301               clear TNTA60DS;
087100110301               TA60flg = 'M';
087200110301               TA60cli = V02cli;
087300110412               Parmcbl = atcprg ;
087400110412               tnta60r (kpjba:tnta60ds:Parmcbl);
087500110301               IF  TA60f03 <> *blanks or TA60err <> *blanks;
087600110301                  chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
087700110504                  If TA60err <> *blanks ;
087800110504                     errMessage  = *on;
087900110504                     errGenerico = *on;
088000110504                     V1Dmsg = ta60msg;
088100110504                  Endif ;
088200110301                  leavesr;
088300110301               ENDIF;
088400100308
088500110207        // se attività richiede la creazione di una nuova attività ma richiede anche
088600110207        // il richiamo al blocco del cliente richiamo il pgm TRMK25R
088700110301             WHEN  §ccoblc <> ' ' ;
088800110207                   exsr Ric_trmk25 ;
088900110207                   IF  OMK25f12 = 'S' or OMK25err <> *blanks;
089000110207                       chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
089100110207                       leavesr;
089200110207                   ENDIF;
089300100329         // Emetto la window  W03  se causale necessita la creazione di un nuovo contatto
089400110301             WHEN   §ccoupm <> ' ' ;
089500100520         //?se attività legata a trattativa e chiude trattativa a potenziale codificato
089600100520         //?non devo creare nuova attività
089700100520                  IF  Vis_trattativ and §CCOcht = 'S' and $Gia_Codifica;
089800100924                      §ccoupm = ' ';
089900100520                  ELSE;
090000100520
090100091214         //?Se appuntamento con affiancamento prima di emettere la win per nuova attività
090200091214         //?devo esitare prima l'affiancamento
090300100311                    IF  ConAffianca;
090400100311                        exsr Conf_TRMK22 ;
090500091214           //?se errore o F12 devo tornare alla videata dell'esito appuntamento
090600100311                        IF  OMK22f12 = 'S' or OMK22err <> *blanks;
090700100311                            leavesr;
090800100311                        ENDIF;
090900100311                    ENDIF;
091000090610                    $Video = 'W3';
091100090610                    $inzw03 = *on ;
091200100311                    leavesr ;
091300100520                  ENDIF;
091400110301
091500110301             ENDSL;
091600100311
091700100311         // registro la scrittura del contatto senza creare il contatto successivo
091800090610                    exsr regis_atc ;
091900091214
092000091214         //?Se appuntamento con affiancamento devo esitare anche l'eventuale affiancamento
092100100311                    IF  ConAffianca;
092200100721                        exsr Ric_TRMK22 ;
092300100311           //?se errore o F12 devo tornare alla videata dell'esito appuntamento
092400100311                        IF  OMK22f12 = 'S' or OMK22err <> *blanks;
092500100721                            chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
092600100311                            leavesr;
092700100311                        ENDIF;
092800100311                    ENDIF;
092900100311
093000100311         // chiedo conferma chiusura trattativa
093100100311             If  $chiedi_conf = *on ;
093200110622                  exsr Ges_Chiudotr ;
093300100311         // rolbk se ci sono errori o non richiesto annullamento trattativa
093400100503                  if (imk19cmt = 'S' and omk19err <> ' ') or w06sino = 'N' ;
093500100311                        rolbk ;
093600100311                        $inzd02 = *on ;
093700100311                        leavesr ;
093800100311                     endif ;
093900100311             endif ;
094000091214
094100091210         // verifico se devo richiamare il pgm apertura trattativa
094200091116                    if §ccoapt <> *blank  ;
094300091015         // richiamo il programma gestione trattativa
094400091015                          exsr Ric_TNTA88 ;
094500091022                          If ota88f12 = 'S' ;
094600091022                             $inzd02 = *on ;
094700091022                             leavesr ;
094800091022                          endif ;
094900091015                    endif ;
095000100311
095100091210         // verifico se devo richiamare il pgm esito offerta
095200091210                    if §ccoeso <> *blank  ;
095300091210         // richiamo il programma esito offerta
095400091210                          exsr Ric_TNTA56 ;
095500100308                          If ota56f12 = 'S' or ota56Err = 'S' ;
095600091210                             $inzd02 = *on ;
095700091210                             leavesr ;
095800091210                          endif ;
095900091210                    endif ;
096000091016
096100100311         // comitto se richiesto e se non ci sono errori
096200091016            if imk19cmt = 'S' and omk19err = ' ';
096300091016               commit ;
096400091016            endif ;
096500091016
096600100419         // Richiedo Info Commerciali
096700100419                exsr Sr_infocomm ;
096800091016
096900110412         // alla fine di tutto se il potenziale è in categoria Eliminabile
097000110412         // richiamo il TRMK05 che mi ricalcola la categoria
097100110412           IF  CPOfls = 'E';
097200110412               clear trmk05ds;
097300110412               IMK05cpo = CPOcpo;
097400110412               trmk05r (kpjba : TRMK05DS);
097500110412         //?se categoria variata aggiorno potenziale
097600110412               IF  OMK05err = *blanks and OMK05cat <> CPOfls;
097700110412                   chain(e) CPOcpo TNCPO01L;
097800110412                   IF  not %error and %found(TNCPO01L);
097900110412                       CPOfls = OMK05cat;
098000110412                       update TNCPO000;
098100110412                   ENDIF;
098200110412               ENDIF;
098300110412           ENDIF;
098400110412
098500100329         // Ritorno al programma precedente
098600091016                $fine  = *on ;
098700090610                endif;
098800091015
098900090610       ENDSR;
099000100308
099100100308       //--------------------------------------------------------------
099200110622       //?Gestione Richiesta conferma chiusura Trattativa
099300100308       //--------------------------------------------------------------
099400110622       BEGSR Ges_Chiudotr ;
099500100308
099600100308             clear w06sino ;
099700100517             $fine6 = *off ;
099800100517
099900100517          dow $fine6 = *off ;
100000100308             exfmt mk19w06 ;
100100100517
100200100517       // N  = Nulla
100300100517             If w06sino = 'N' ;
100400100517                leavesr ;
100500100517             endif ;
100600100517
100700100517       // S  = Richiamo il pgm chiusura trattativa
100800100308
100900100503             If w06sino = 'S' ;
101000100520
101100100520               chain V02vis TIVIS05L;
101200100520               IF  %found(TIVIS05L);
101300100520                 VISesi = V02cae;
101400100520                 VISdch = %dec(%date());
101500101013        // se ho richiesto l'esito della attività lo scrivo nei FLO
101600101013                 If §ccores = 'S' ;
101700101013                    clear dvis01 ;
101800101013                    dvis01 = visflo ;
101900101013                    §visesito = W07esi ;
102000101013                    visflo = dvis01 ;
102100101013                 endif ;
102200100520                 update TIVIS000;
102300100520               ENDIF;
102400110208
102500110208        // in caso di trattativa con tariffe 999 vado ad aggiornare i dati dell'offerta / caldo
102600110208             chain (V02vis:999:0) tivof01l ;
102700110208             If %found(tivof01l) ;
102800110208             // in caso di richiesta esito e l'esito è positivo convalido l'offerta
102900110208                If §ccores = 'S' and W07esi = 'P' ;
103000110208                   vofdco = %dec(%date()) ;
103100110208                   vofeso = 'C' ;
103200110208                endif;
103300110208             // in caso di richiesta esito e l'esito è negativo annullo l'offerta
103400110208                If §ccores = 'S' and W07esi = 'N' ;
103500110208                   vofeso = '*' ;
103600110208                endif;
103700110208             // in caso di chiusura senza richiesta di esito annullo l'offerta
103800110208                If §ccores <> 'S'  ;
103900110208                   vofeso = '*' ;
104000110208                endif;
104100110208                update tivof999 ;
104200110208             Endif ;
104300110208
104400100924         // richiamo il programma delle info trattative se inserite
104500110622               If visinfot <> ' ' or (ATCCAC = '41' and vistpv = 'I');
104600100924                   exsr Sr_infotrat ;
104700120706         // se al ritorno dal PGM INFO trattattive sono state inserite ed il flag INFOT
104800120706         // sul file TRATTATIVE è a blank lo aggiorno
104900120706                   If o41ifotot <> ' ' and visinfot = ' ' ;
105000120706                      chain V02vis TIVIS05L;
105100120706                      IF  %found(TIVIS05L);
105200120706                          VISinfot = o41ifotot ;
105300120706                          update TIVIS000;
105400120706                      ENDIF;
105500120706                   endif ;
105600110622         // se in uscita visinfot è uguale a " " e causale 41 di tratattiva di tipo I
105700110622         // richiamo info trat
105800110622               dow o41ifotot = ' ' and atccac = '41' and vistpv =  'I' ;
105900110622                   exsr Sr_infotrat ;
106000110622                   If o41ifotot <> ' ' ;
106100110622                      chain V02vis TIVIS05L;
106200110622                      IF  %found(TIVIS05L);
106300110622                          VISinfot = o41ifotot ;
106400110622                          update TIVIS000;
106500110622                      ENDIF;
106600110622                   endif ;
106700110622               enddo;
106800100924               Endif ;
106900100311
107000100924         // richiamo il programma delle info commerciali
107100140929               Wchiudo_tr = 'S' ;
107200100924               exsr F14D02 ;
107300100924
107400100517             leavesr ;
107500100308             endif ;
107600100517           enddo ;
107700100308
107800100308       Endsr ;
107900100308
108000100924       //--------------------------------------------------------------
108100100924       //?Gestione Info trattativa
108200100924       //--------------------------------------------------------------
108300100924       BEGSR Sr_infotrat ;
108400100924
108500100924         // Richiamo il pgm informazioni Trattativa
108600100924         clear  TNTA41ds;
108700100924         I41nrv    = visnrv;
108800100924         I41rag    = visrag;
108900100924         I41ifotot = VISinfot;
109000100924         I41mod    = 'G';
109100100924         I41pgm    = 'TRMK19R';
109200101220         I41tpv    = VIStpv;
109300100924
109400100924         tnta41r (kpjba : TNTA41ds);
109500100924
109600100924       ENDSR;
109700100308       //--------------------------------------------------------------
109800100308       //?Gestione Richiesta Esito Attività/trattativa
109900100308       //--------------------------------------------------------------
110000100308       BEGSR Ges_Esito  ;
110100100308
110200100308             clear w07esi  ;
110300100517             $fine7 = *off ;
110400100517
110500100517           Dow $fine7 = *off ;
110600100308             exfmt mk19w07 ;
110700100517             If w07esi =  'P' ;
110800100517                leavesr ;
110900100517             endif ;
111000100517             If w07esi =  'N' ;
111100100517                leavesr ;
111200100517             endif ;
111300100517           enddo ;
111400100308
111500100308       Endsr ;
111600090610
111700090610       //--------------------------------------------------------------
111800090610       //?Caricamento videata telefonata D02
111900090610       //--------------------------------------------------------------
112000090610       BEGSR Inzd02;
112100090610
112200090610          clear mk19d02 ;
112300090610
112400090610       // imposto indicatori di visualizzazione
112500090610          Visualizza = (Imk19Fle = '5');
112600090610          Manutenzione = (Imk19Fle = '2');
112700100928          Gestione     = (Imk19Fle = 'G');
112800091119          Telefonata   = (Imk19tco = 'T') or (imk19tco = 'O') ;
112900090610          Appuntamento = (IMK19tco = 'A');
113000110405
113100110405          $cli_non_sped = *off;
113200110405
113300091119             // tipo attivita
113400091119                  clear  dTTA;
113500091119                  clear  TIBS02ds;
113600091119                  T02mod = 'C';
113700091119                  T02cod = 'TTA';
113800091119                  T02ke1 = Imk19tco ;
113900091119                  T02sif = KNSIF;
114000091119                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
114100091119                  if  T02err = *blanks;
114200091119                      dtta = t02uni ;
114300091119                  endif;
114400091216
114500091216         //?Centro titolo alto
114600091216         clear wpos;
114700091216         wpos = (%len(v02tit) - %len(§ttatit)) / 2;
114800091216         %subst(v02tit:wpos:%len(§ttatit)) = §ttatit;
114900091216
115000090623          If visualizza = *on ;
115100090623             Vis_nonfatto = (Imk19Fatt = ' ');
115200090623          else ;
115300090623             Vis_nonfatto = *off ;
115400090623          endif ;
115500090612
115600090612       // Verifico se è stato richiamato dal Programma di gestione Potenziali
115700091105              Potenziali = (imk19fpr = 'P') ;
115800091105       // Verifico se è stato richiamato dal Programma Agenda
115900091105              Ric_agenda = (imk19fpr = 'A') ;
116000091105       // Verifico se è stato richiamato dal Programma Gestione trattative
116100091105              Ric_trattativ = (imk19fpr = 'T') ;
116200091105       // Verifico se è stato richiamato in visualizzazione non dal TRMK18
116300091105              Ric_visualizz = (imk19fpr <> ' ') ;
116400100803       // Verifico se è stato richiamato con flag di provenienza "V" solo visualizzazione
116500110805       // Verifico se è utente di sede non utilizza F21
116600131127       // Verifico se è stata chiamata dall'azione MK01 non utilizza F21
116700131127              NO_F21 = (imk19fpr = 'V') or (Dutpou= 046) or KCDAZ = 'MK01';
116800091105
116900090611       // imposto testata
117000091020          If  imk19fle = 'P' or imk19fle = 'C';
117100090611              v01tit = tit_primo ;
117200090611          endif ;
117300090610
117400090611          If  imk19fle = '5' ;
117500090611              v01tit = tit_visua ;
117600090611          endif ;
117700090611
117800090611          If  imk19fle = '2' ;
117900090611              v01tit = tit_manut ;
118000090611          endif ;
118100090611
118200090611          If  imk19fle = 'G' ;
118300090611              v01tit = tit_gesti ;
118400090611          endif ;
118500090611
118600091026       // imposto il codice cliente
118700091026          v02cli  = savcli ;
118800091026
118900090610       // se provengo da sbf devo fare gli agganci con i dati passati dal subfile
119000091026          If Imk19fle <>  'P' and imk19fle <> 'C';
119100090610             cpocpo = imk19cpo ;
119200091026             v02cli = imk19ksc ;
119300090610          endif ;
119400090610
119500090610             altrenote = *off  ;
119600090610             piunote   = *off  ;
119700091218             clear sav_atcdad;
119800111102             clear sav_atchda;
119900111102             clear sav_atcoii;
120000111102             clear sav_atcofi;
120100111102             clear sav_atccmm;
120200090610
120300090610       // recupero i dati anagrafici ed informativi del potenziale
120400090610
120500090610          exsr RecD02 ;
120600090610
120700090610        // se contatto non inserito manualmente oppure primo contatto recupero
120800090610        // dati dal file contatti
120900090610         If  $prima = *off  ;
121000090610             If manutenzione = *off and visualizza = *off ;
121100090611                chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
121200090610             else  ;
121300090611                chain(n) (imk19tat:imk19atn:imk19atnp) tiatc01l ;
121400090610             endif ;
121500090610             If   %found(tiatc01l);
121600140411               // -?Data/Ora/Utente inserimento?
121700140414               clear  Data_Eur;
121800140414               clear  wData;
121900140414               clear  wHhMm;
122000140414               if  ATCdim > *zero;
122100140414                 Data_Eur = %date( ATCdim : *iso );
122200140414                 wData    = %dec( Data_Eur );
122300140414               endif;
122400140414               wHhMm = %int( ATChim / 100 );
122500140414               %subst( V02tit : %len(V02tit) - 32 ) = 'Ins. ' +
122600140414                       %editc( wData : 'Y' ) + ' ' +
122700140414                       %editw( wHhMm : '  :  ' ) + ' ' +
122800140414                       ATCpri;
122900090610             // data scadenza contatto
123000090610                  If atcdad > 0 ;
123100090610                     data_eur = %date(atcdad:*iso);
123200090610                     v02scd   = %dec(data_eur) ;
123300091211                     w02scd   = atcdad;
123400091218                     sav_atcdad = atcdad;
123500111102                     sav_atchda = atchda;
123600111102                     sav_atcoii = atcoii;
123700111102                     sav_atcofi = atcofi;
123800111102                     sav_atccmm = atccmm;
123900090610                  endif ;
124000090610             // ora  scadenza contatto
124100090610                  If atchda > 0 ;
124200090610                     v02sco = %dec(%subst(%EDITC(atchda:'X'):1:4):4:0);
124300090610                  endif ;
124400090610             // causale contatto
124500090610                  clear  dCCO;
124600090610                  clear  TIBS02ds;
124700090610                  T02mod = 'C';
124800090610                  T02cod = 'CCO';
124900090610                  T02ke1 = atccad;
125000090610                  T02sif = KNSIF;
125100090610                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
125200090610                  if  T02err = *blanks;
125300090610                      dcco = t02uni ;
125400090714                      v02cau  = atccad  ;
125500090714                      v02caud = §ccodes ;
125600110405             // Verifico se causale su cliente che non spedisce
125700110405                      If §ccocns = 'S' ;
125800110405                         $cli_non_sped = *on ;
125900110405                      Endif;
126000090610                  endif;
126100091021          // verifico se affiancamento
126200091021                  affiancamento = (atccad = '92') ;
126300100616            //?cerco il commerciale dell'Appuntamento
126400100615                  IF  Affiancamento;
126500100616                    exec sql
126600100616                    SELECT ATCcmm into :app_ATCcmm from TIATL00F
126700100616                    JOIN TIATC00F on
126800100616                    ATLatl = ATCatn and ATLatlp = ATCatnp
126900100616                    WHERE ATLatn = :IMK19atn and ATLatnp = :IMK19atnp;
127000100616                    clear V02dcmma;
127100100616              //?trovo l'appuntamento decodifico il commerciale
127200100616                    IF  sqlcod = 0;
127300130809                      chain (app_ATCcmm) AZCMM000;
127400130809                      IF  %found(AZCMM01L);
127500130809                        V02dcmma = CMMdes;
127600100616                      ENDIF;
127700100616                    ENDIF;
127800100615                  ENDIF;
127900091021
128000090610                  v02cmi = %editc(atccmm : 'X') ;
128100090610         // decodifica del commerciale
128200130809                  clear V02cmiD;
128300130809                  chain (ATCcmm) AZCMM000;
128400130809                  if  %found(AZCMM01L);
128500130809                    v02cmid = CMMdes;
128600090610                  endif;
128700090703           // ora inizio impegno
128800090703                  v02oii = atcoii ;
128900090703           // ora fine   impegno
129000090703                  v02ofi = atcofi ;
129100090610           // numero trattativa
129200090610                  v02vis = atcnrv ;
129300090610           // cliente e decodifica
129400091026                  v02cli = atcksc ;
129500090610                  clear  tibs69ds;
129600091026                  I69kac = v02cli;
129700090610                  TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
129800091026                  v02clid = ACOrag;
129900090610           // note contatto PRECEDENTE
130000090610                clear trmk20ds ;
130100090610                imk20tla = 'L';
130200090610                imk20flm = 'R';
130300090610                imk20cpo = cpocpo ;
130400090610                imk20rsc = v02cpd ;
130500090611                imk20tat = imk19tat ;
130600090611                imk20atn = imk19atn ;
130700090611                imk20atnp= imk19atnp -1;
130800090616                Trmk20r ( kpjba : trmk20ds );
130900090610                v02no1o =  emk20no1 ;
131000090610                v02no2o =  emk20no2 ;
131100090610                piunote = (omk20piu = '+');
131200090610
131300090610             endif ;
131400090610          // se si tratta di un contatto già eseguito
131500090611             if Imk19fatt = 'S' ;
131600090610         //  proteggo i campi di input
131700090610                giafatto = *on ;
131800090610
131900090610             // verifco    si tratta di un 1° contatto se data scadenza = 0
132000090610                primocontatto = (v02scd = 0) ;
132100090610
132200140411               // -?Data/Ora/Utente attività eseguita?
132300140414               clear  Data_Eur;
132400140414               clear  wData;
132500140414               clear  wHhMm;
132600140414               if  ATCdco > *zero;
132700140414                 Data_Eur = %date( ATCdco : *iso );
132800140414                 wData    = %dec( Data_Eur );
132900140414               endif;
133000140414               wHhMm = %int( ATChco / 100 );
133100140414               %subst( V02tit2 : %len(V02tit2) - 27 ) =
133200140414                       %editc( wData : 'Y' ) + ' ' +
133300140414                       %editw( wHhMm : '  :  ' ) + ' ' +
133400140414                       ATCpru;
133500090610             // recupero i dati relativi all'esecuzione del contatto
133600090610                v02com = %editc(atccco:'X') ;
133700130809                clear  V02comD;
133800130809                chain (ATCcco) AZCMM000;
133900130809                if  %found(AZCMM01L);
134000130809                  V02comd = CMMdes;
134100090610                endif;
134200090610             // data contatto
134300090610                data_eur=%date(atcdco:*iso);
134400090610                v02dat=%dec(data_eur);
134500090610             // ora  contatto
134600090610                v02ora=%dec(%subst(%editc(atchco:'X'):1:4):4:0);
134700090610             // causale  contatto
134800090610                v02cae = atccac ;
134900090610                clear  dCCO;
135000090610                clear  TIBS02ds;
135100090610                T02mod = 'C';
135200090610                T02cod = 'CCO';
135300090610                T02ke1 = v02cae ;
135400090610                T02sif = KNSIF;
135500090610                TNTBE_RicercaControllo  (kpjba : tibs02ds);
135600090610                if  T02err = *blanks;
135700090610                    dcco = t02uni ;
135800090610                    V02caed = §ccodes ;
135900090610                endif;
136000100223             // se appuntamento visualizzo  l'esito
136100100223                if atctat = 'A' ;
136200100223                   v02fatto = atcest ;
136300100223                endif ;
136400091021             // verifico generata una nuova trattativa da questa attività
136500091021                tratta_new = (atcnra > 0) ;
136600091021                v02nra = atcnra ;
136700100309             // verifico se c'è l'esito
136800100309                datc01 = atcflo ;
136900100309                vis_esito = (§atcesito <> ' ') ;
137000100309                If §atcesito = 'P'  ;
137100100309                   v02esi = 'POSITIVO' ;
137200100309                else ;
137300100309                   v02esi = 'NEGATIVO' ;
137400100309                endif ;
137500090610             // recupero note
137600090610                clear trmk20ds ;
137700090610                imk20tla = 'L';
137800090610                imk20flm = 'R';
137900090610                imk20cpo = cpocpo ;
138000090610                imk20rsc = v02cpd ;
138100090611                imk20tat = imk19tat ;
138200090611                imk20atn = imk19atn ;
138300090611                imk20atnp= imk19atnp;
138400090616                Trmk20r ( kpjba : trmk20ds );
138500090610                v02no1 =  emk20no1 ;
138600090610                v02no2 =  emk20no2 ;
138700090610                altrenote = (omk20piu = '+');
138800090610             endif ;
138900090610
139000090610         else ;
139100090610             primocontatto = *on ;
139200090610         endif ;
139300090610         // propongo alcuni campi  se si tratta di un contatto ancora da eseguire
139400090623         // se non si tratta di visualizzazione
139500090623
139600091214         If  Not Visualizza and
139700091214             ((IMK19fatt = *blanks and $dasbf) or Not $dasbf);
139800091214         //?Se non è appuntamento
139900091214           IF Not Appuntamento;
140000090610             // commerciale chi sono
140100091214                If imk19com > *zeros;
140200090714                   v02com  = %editc(imk19com : 'X')  ;
140300090714                   v02comd = imk19comd ;
140400090714                endif ;
140500090610            //  data e ora del momento
140600090610                v02dat = *date ;
140700090610                v02ora = %dec(%subst(%EDITC(%dec(%time()):'X'):1:4):4:0);
140800091214         //?Se è appuntamento
140900091214           ELSE;
141000091214         //?imposto il commerciale, data e ora dell'appuntamento
141100091214         //?inoltre vengono protetti il commerciale e la data
141200091214             v02com  = v02cmi;
141300091214             v02comd = v02cmid;
141400091214             v02dat  = v02scd;
141500091214             v02ora  = v02sco;
141600110302           ENDIF;
141700091214         // flag giafatto a 0
141800091214           giafatto = *off ;
141900090610         endif ;
142000090714
142100090714        // verifico se visualizzare oppure no cliente e trattativa
142200090714
142300091026         cliente   = (v02cli > 0) ;
142400090714         vis_trattativ = (v02vis > 0) ;
142500091119
142600091119        // verifico se esistono trattativa e lo visualizzo
142700100118        // oltre a quella legata all'attività in carico
142800091119                   clear tnta43ds ;
142900091119                   ita43cpo = cpocpo ;
143000091119                   ita43ksc = v02cli ;
143100091119                   ita43nrv = v02vis ;
143200091119                   Tnta43r (tnta43ds) ;
143300100118            // se l'attività in carico non ha la trattativa
143400100118            // e mi torna un n. di trattativa imposto la scritta
143500100118            IF  v02vis = 0 and ota43nrv > 0;
143600100118              evalr v02segn = 'TRATTATIVE APERTE';
143700100118            ENDIF;
143800100118            // se l'attività in carico ha la trattativa
143900100118            // e mi torna un n. di trattativa imposto la scritta
144000100119            IF  v02vis > 0 and ota43nrv > 0 and v02vis <> ota43nrv;
144100100118              evalr v02segn = 'ALTRE TRATTATIVE APERTE';
144200100118            ENDIF;
144300091216
144400091216         //?Centro titolo basso
144500091216         clear wpos;
144600091216         IF  Not PrimoContatto;
144700091216         wpos = (%len(v02tit2) - %len(c_Attivita)) / 2;
144800091216         %subst(v02tit2:wpos:%len(c_Attivita)) = c_Attivita;
144900091216         ENDIF;
145000091216         IF  PrimoContatto;
145100091216         wpos = (%len(v02tit2) - %len(c_PrimaAtt)) / 2;
145200091216         %subst(v02tit2:wpos:%len(c_PrimaAtt)) = c_PrimaAtt;
145300091216         ENDIF;
145400091216
145500091216          If visualizza = *on ;
145600091216             Vis_nonfatto = (Imk19Fatt = ' ');
145700091216          else ;
145800091216             Vis_nonfatto = *off ;
145900091216          endif ;
146000091215
146100091215         //?verifico se per potenziale/cliente ci sono attività aperte
146200100118         //?ma solo se non è già impostato il campo con trattativa aperte
146300100118         IF  v02segn = ' ';
146400091215           clear TRMK85ds;
146500091215           IMK85cpo = cpocpo;
146600091215           IMK85ksc = v02cli;
146700100118           IMK85nrv = v02vis;
146800100129           IMK85atn = IMK19atn;
146900091215           trmk85r (trmk85ds);
147000100118         //?se torna errore <> "9" ho trovato delle attività aperte
147100091215           IF  OMK85err <> *blanks and OMK85err <> '9';
147200091215         //?imposto la costante a video
147300100118             evalr v02segn = 'ALTRE ATTIVITA'' APERTE';
147400091215           ENDIF;
147500091215         ENDIF;
147600091215
147700091214         //?verifico se appuntamento con Affiancamento
147800091214         ConAffianca = *off;
147900091214         F20Affianca = *off;
148000091214         IF  Appuntamento and Not Affiancamento;
148100091214           k_atlatl  = IMK19atn;
148200091214           k_atlatlp = IMK19atnp;
148300091214           chain  %kds(K02tiatl) TIATL02L;
148400091214           IF  %found(Tiatl02l);
148500091216             ConAffianca = *on;
148600091214           ENDIF;
148700091210         ENDIF;
148800091216
148900091216         //?Non abilito F20=Affiancamento in gestione
149000091216         IF  Appuntamento and Manutenzione;
149100091216           F20Affianca = *on;
149200091216         ENDIF;
149300091216         IF  ConAffianca and Visualizza;
149400091216           F20Affianca = *on;
149500091216         ENDIF;
149600100519
149700100519         //?Abilito F16=Annullamento
149800100519         F16Attivo = *off;
149900100519         $okdel = *off;
150000100519         SELECT;
150100110214         //?se codice attività impedisce l'annullamento dell'attività disabilito F16
150200110519         //?e il cliente è perso e non ha attività
150300110519           WHEN  §ccoann = 'S' and Manutenzione ;
150400110519                 If Cpofls <> 'P' or (Cpofls = 'P' and v02segn <> *blank) ;
150500110519                    F16Attivo = *on;
150600110519                    $okdel = *on;
150700110519                 Endif ;
150800100519         //?se sono in gestione attività richiamato da gestione trattativa
150900100520           WHEN  Manutenzione and Ric_Trattativ;
151000100519             F16Attivo = *on;
151100100519             $okdel = *on;
151200110908         //?se sono in gestione attività non legata a trattativa
151300110908         //?attività su potenziale perso e ci sono altre attività aperte
151400110908           WHEN  Manutenzione and not Vis_Trattativ and CPOfls = 'P' and
151500110908                 V02segn <> *blanks;
151600110908             F16Attivo = *on;
151700100519         //?se sono in gestione attività non legata a trattativa
151800100519         //?attività su potenziale mai codificato e ci sono altre attività aperte
151900110225           WHEN  Manutenzione and not Vis_Trattativ and CPOfls = 'M' and
152000110225                 V02segn <> *blanks;
152100100519             F16Attivo = *on;
152200100520         //?se sono in gestione attività non legata a trattativa
152300100520         //?attività su potenziale codificato
152400110225           WHEN  Manutenzione and not Vis_Trattativ and CPOfls = 'C';
152500100520             F16Attivo = *on;
152600100519         //?se sono in gestione attività non legata a trattativa
152700100519         //?attività su cliente
152800100520           WHEN  Manutenzione and not Vis_Trattativ and v02cli > 0;
152900100519             F16Attivo = *on;
153000100519         ENDSL;
153100150126
153200150126         // -?Se Cliente in Campagna visualizzo il tasto F4?
153300150126         F04Attivo = *off;
153400150126         clear  TRKC73ds;
153500150126         if  iMK19ksc <> *zero;
153600150126           iKC73ksu = iMK19ksc;
153700150126           iKC73dat = %dec( %date() );
153800150126           trkc73r ( kpjba : TRKC73ds );
153900150126           // -?Se torna almeno un numero campagna?
154000150126           F04Attivo = ( oKC73nc1 <> *zero );
154100150126         endif;
154200091216
154300091216         //?Se trattativa a video cerco la data della trattativa
154400091216         clear V02VisDat;
154500100504         clear w_vistpv ;
154600100614         clear V02VisTtr;
154700091216         IF  Vis_Trattativ;
154800100520           chain(n)  V02vis tivis05l;
154900091216           IF  %found(tivis05l);
155000091216             data_eur  = %date(visdat:*iso);
155100091216             V02VisDat = %dec(data_eur);
155200100504             w_vistpv = vistpv ;
155300100614         //?decodifico il tipo trattativa
155400100614             clear dTTR;
155500100614             clear TIBS02ds;
155600100614             T02mod = 'C';
155700100614             T02cod = 'TTR';
155800100614             T02ke1 = w_vistpv;
155900100614             T02sif = KNSIF;
156000100614             TNTBE_RicercaControllo  (kpjba : tibs02ds);
156100100614             IF  T02err = *blanks;
156200100614                 dTTR = t02uni;
156300100614                 V02VisTtr = §ttrdes;
156400100614             ENDIF;
156500091216           ENDIF;
156600091216         ENDIF;
156700091216
156800100114         PosCurFatto = *off;
156900091216         PosCurNo102 = *off;
157000091216         PosCurCom02 = *off;
157100091216
157200091216         //?Se non è manutenzione e non sto visualizzando un non fatto
157300100111         //?e non è interrogazione
157400100111         IF  Not Manutenzione and Not Vis_NonFatto and Not Visualizza;
157500100114           SELECT;
157600100114         //?Se il commerciale non è stato proposto posiziono il cursore sul commerciale
157700100114             WHEN  v02com = *zeros or V02com = *Blanks;
157800100114               PosCurCom02 = *on;
157900100114         //?Se è appuntamento mi posiziono sul 'Effettuato s/n'
158000100114             WHEN  Appuntamento;
158100100114               PosCurFatto = *on;
158200100114             OTHER;
158300100503         //?altrimenti posiziono il cursore sulla causale
158400100503               PosCurCae02 = *on;
158500100114           ENDSL;
158600091216         ENDIF;
158700100325
158800100325        // Recupero informazioni responsabile trasporti
158900100325
159000100325         exsr respD02 ;
159100091216
159200100310         $ForzaImpegno = *off;
159300100310         $ForzaComm    = *off;
159400100422         $ForzaComm3   = *off;
159500100310         $ForzaComin   = *off;
159600100318         $ForzaComin2  = *off;
159700100311         clear sav_v02cmi ;
159800100318         clear sav_v02cmi2;
159900100311         clear sav_w03com ;
160000100422         clear sav_w03comt;
160100090714
160200110225       //?Se sto gestendo un'attività ancora da fare
160300110225       //?e l'attività prevede il controllo del cliente "perso"
160400110301           IF  IMK19fatt <> 'S' and §CCOccp = 'S' and Gestione;
160500110527       // se attività 70 faccio un controllo se 71 un altro
160600110530             If  atccad = '71' ;
160700110225         //?Controllo se potenziale lavora o no
160800110527                 clear TRMK04DS;
160900110527                 clear TRMK85DS;
161000110527                 IMK04cpo = CPOcpo;
161100110527                 IMK85cpo = cpocpo;
161200110527                 IMK85ksc = v02cli;
161300110527                 IMK85nrv = v02vis;
161400110527                 IMK85atn = IMK19atn;
161500110527                 trmk04r (kpjba : TRMK04DS : TRMK85DS);
161600110225         //?Se il potenziale ha ripreso a lavorare devo emettere una win dove avviso
161700110527         //?di verificare le attività e trattative aperte
161800110527                 IF  OMK04lav = 'SI';
161900110527                    $Lavora = *on;
162000110527                    $InzD02 = *off;
162100110527                    $Video = 'W0';
162200110527                 ENDIF;
162300110527             ENDIF;
162400110527       // se attività 70 faccio un altro controllo
162500110530             If  atccad = '70' ;
162600110527         //?Controllo se potenziale lavora o no
162700110527                 clear TRMK04DS;
162800110530                 clear TRMK85DS;
162900110527                 IMK04cpo = CPOcpo;
163000110530                 trmk04r (kpjba : TRMK04DS : TRMK85DS);
163100110530         //?Se il potenziale ha ripreso a lavorare devo emettere una win nella quale
163200110530         //?avvisa che ci sono trattative aperte e annulla l'attività in corso
163300110530                 IF  OMK04lav = 'SI';
163400110527                    $Lavora = *on;
163500110527                    $InzD02 = *off;
163600110527                    $Video = 'W9';
163700110527                 ENDIF;
163800110527             ENDIF;
163900110225           ENDIF;
164000110214
164100090610       ENDSR;
164200090610
164300090610       //--------------------------------------------------------------
164400090610       //?Recupero dati anagrafici / resp / mail / stato potenziale
164500090610       //--------------------------------------------------------------
164600090610       BEGSR RecD02;
164700090610
164800090610
164900091019        // Recupero dati anagrafico potenziale o cliente in base ai dati passati
165000090610
165100091021         exsr AnagD02 ;
165200090610
165300110502              $Gia_Codifica = *off ;
165400100322           // verifico se codificato su altri codici
165500110502           //   chain cpocpo  CNACO16L;
165600110502           //    IF  %found(CNACO16L);
165700110502           //        $Gia_Codifica = *on ;
165800110502           //     endif ;
165900110502           // considero un potenziale già codificato se il falf del potenziale è uguale a "C"
166000110502              If cpofls = 'C' ;
166100110502                 $gia_codifica = *on ;
166200110502              Endif ;
166300110502           // decodifico la categoria del potenziale
166400110502                  clear  dCPO;
166500110502                  clear  TIBS02ds;
166600110502                  T02mod = 'C';
166700110502                  T02cod = 'CPO';
166800110502                  T02ke1 = cpofls ;
166900110502                  T02sif = KNSIF;
167000110502                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
167100110502                  if  T02err = *blanks;
167200110502                      dcpo = t02uni ;
167300110502                  endif;
167400090610
167500090610
167600090610       ENDSR;
167700090610
167800090610       //--------------------------------------------------------------
167900091021       //?Recupero dati potenziale/cliente
168000090610       //--------------------------------------------------------------
168100091021       BEGSR AnagD02 ;
168200091020       // se esite il codice cliente recupero i dati anagrafici da lui
168300091020             If v02cli <> *zeros ;
168400091021                cliente = *on ;
168500091020                clear  tibs69ds;
168600091020                I69kac = v02cli;
168700091020                I69kin = v02cli;
168800091020                I69kcp = v02cli;
168900091020                TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
169000091020                v02clid = acorag ;
169100091020                cpocpo  = acolib ;
169200091020                v02cpi = indvia ;
169300091020                v02cpl = indcit ;
169400091020                v02cpt = indtel ;
169500091020                v02cpf = indtlf ;
169600091020                v02cpc = %editc(indcap:'X') ;
169700091020                v02cpp = indprv ;
169800091020                v02cpn = indsta ;
169900091020                v02cmp = clpage ;
170000091020             endif ;
170100091020
170200120622          clear dCPO01;
170300090610             chain(N)  cpocpo  TNCPO000;
170400140715             chain     cpocpo  TNCPO100;
170500140715             if  NOT %found(TNCPO11L);
170600140715               clear  CPO1tel;
170700140715               clear  CPO1fax;
170800140715             endif;
170900090610       // carico dati del potenziale
171000090610             v02cpo = %editc(cpocpo:'X');
171100090610             v02cpd = cporag ;
171200120622             dCPO01 = CPOrst;
171300091020       // se non esite il codice cliente recupero i dati anagrafici dal potenziale
171400091020             If v02cli =  *zeros ;
171500091021                cliente = *off;
171600091020                v02cpi = cpovia ;
171700091020                v02cpl = cpocit ;
171800140715                v02cpt = CPO1tel;
171900140715                v02cpf = CPO1fax;
172000091020                v02cpc = cpocap ;
172100091020                v02cpp = cpoprv ;
172200091020                v02cpn = cponaz ;
172300091020                v02cmp = cpocmm ;
172400091020             endif ;
172500090610
172600090610
172700091020        // recupero dedodifica dle commerciale
172800130809         clear  V02cmpD;
172900130809         chain (v02cmp) AZCMM000;
173000130809         if  %found(AZCMM01L);
173100130809           V02cmpd = CMMdes;
173200090610         endif;
173300090610
173400090610         ENDSR ;
173500090610
173600090610       //--------------------------------------------------------------
173700090610       //?Recupero responsabile trasporti e mail
173800090610       //--------------------------------------------------------------
173900090610       BEGSR RespD02 ;
174000090610
174100100511             clear v02rtr ;
174200100511             clear v02mrt ;
174300090610       // carico responsabile trasporti
174400100326             SELECT ;
174500100326
174600100326       // se c'è il cliente recupero dal cliente
174700100408             when v02cli > 0 ;
174800100326                 clear ntcnk1 ;
174900100326                 ntcapl = 'C' ;
175000100408                 ntcnk1 = %editc(dutkci: 'X') + %editc(v02cli:'X');
175100100329       // se aperta trattativa recupero da questa
175200100408             when v02nra > 0 ;
175300100329                 clear ntcnk1 ;
175400100329                 ntcapl = 'T' ;
175500100408                 ntcnk1 = %editc(dutkci: 'X') + %editc(v02nra:'X');
175600100326       // se c'è trattativa recupero da questa
175700100408             when v02vis > 0 ;
175800100326                 clear ntcnk1 ;
175900100326                 ntcapl = 'T' ;
176000100408                 ntcnk1 = %editc(dutkci: 'X') + %editc(v02vis:'X');
176100100326             other ;
176200100325                 clear ntcnk1 ;
176300100325                 ntcapl = 'P' ;
176400100325                 ntcnk1 = %editc(cpocpo:'X');
176500100326             endsl ;
176600100326
176700090610             ntctnt = '05';
176800090610             clear  ntcnk2 ;
176900091020          setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
177000091020          READE    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
177100090610          dow not %eof(tfntc01l)  ;
177200100329             vh2rtr = ntcrnt ;
177300090610             v02rtr = ntcrnt ;
177400091020             READE    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
177500090610             If  not %eof(tfntc01l) ;
177600090610               v02rtr = %trim(v02rtr) + ', ' + %trim(ntcrnt) ;
177700090610               leave ;
177800090610             else;
177900090610               leave ;
178000090610             endif ;
178100090610          enddo ;
178200090610
178300090610       // carico mail responsabile trasporti
178400090610             ntctnt = '06';
178500090610             clear  ntcnk2 ;
178600091020          chain    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
178700090610          if  %found(tfntc01l)  ;
178800090610             v02mrt = ntcrnt ;
178900090610          endif ;
179000090610
179100090610       ENDSR;
179200090610
179300090611       //--------------------------------------------------------------
179400090611       //?Controllo videata D02
179500090611       //--------------------------------------------------------------
179600090611       BEGSR CtrD02;
179700090611
179800090611            WindDspF = IndDspF;
179900090611            reset WindDspFb;
180000090611            IndDspF  = WindDspF;
180100090611
180200100308            $chiedi_conf = *off ;
180300090611
180400100721         // verifico se nel frattempo hanno già esitato l'attività
180500100721                UNLOCK TIATC01L;
180600100721                chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
180700100721                IF %FOUND(TIATC01L) AND ATCDCO > 0 ;
180800100721                   errMessage  = *on;
180900100721                   errGenerico = *on;
181000100721                   PosCurCom02 = *on;
181100100721                   V1Dmsg = $Msg(07);
181200100721                   LEAVESR ;
181300100721                ENDIF ;
181400090611         // Se sono in manutenzione devo fare i controlli per la sola parte realtiva alla
181500090611         // scadenza dell'attività
181600090611
181700090611           If manutenzione = *on ;
181800090611              exsr Ctrd02Scd ;
181900090611              leavesr;
182000090611           endif ;
182100090611
182200090611         // Controllo codice commerciale (? Ricerca)
182300090611           If    %scan('?' : v02com) > 0  ;
182400090611             errGenerico = *on;
182500090611             PosCurCom02 = *on;
182600130809             clear  TRMK43ds;
182700130809             iMK43fil  = DUTpou;
182800130809             TRMK43R (kpjba : TRMK43ds);
182900130809             if  oMK43err = *off  and  oMK43fxx = *blank;
183000130809               v02com  = %editc( oMK43cmm : 'X' );
183100130809               v02comd = oMK43des;
183200130809             endif;
183300090611             leavesr;
183400090611           endif;
183500090611
183600100210         // Commerciale che esegue il contatto  obbligatorio
183700100210           If  v02com = *blanks or v02com = *zeros;
183800100210             errMessage  = *on;
183900100210             errGenerico = *on;
184000100210             PosCurCom02 = *on;
184100100210             V1Dmsg = $Msg(02);
184200100210             leavesr;
184300100210           endif;
184400100210
184500100210         // Controllo commerciale che esegue il contatto
184600100210             IF  %check(digits:V02com) > 0;
184700100210               ErrMessage  = *on;
184800100210               ErrGenerico = *on;
184900100210               PosCurCom02 = *on;
185000100210               V1Dmsg      = $Msg(02);
185100100210               leavesr;
185200100210             ENDIF;
185300100210
185400100210           //?utente abilitato al commerciale
185500100210             clear TNTAA1DS;
185600120810             ITAA1tipo = 'M';
185700120810             ITAA1caut = '§utegtc';
185800100210             ITAA1cmm  = %dec(V02com:7:0);
185900100519             ITAA1abi = 'RA';
186000100210             kpjbu     = tntaa1ds;
186100100210             tntaa1c (kpjba);
186200100210             TNTAA1DS = kpjbu;
186300100210             IF  OTAA1fabi = 'N';
186400100210               ErrMessage  = *on;
186500100210               ErrGenerico = *on;
186600100210               PosCurCom02 = *on;
186700100210               V1Dmsg      = $Msg(02);
186800100210               leavesr;
186900100210             ENDIF;
187000100210
187100130809             CMMcod = %int(V02com);
187200130809             chain (CMMcod) AZCMM000;
187300130809             if  NOT  %found(AZCMM01L);
187400090611               errMessage  = *on;
187500090611               errGenerico = *on;
187600090611               PosCurCom02 = *on;
187700090611               V1Dmsg = $Msg(02);
187800090611               leavesr;
187900100210             endif;
188000130809             V02comd = CMMdes;
188100100416
188200110525
188300130809             IF  CMMPAR <> ' ' ;
188400110525               ErrMessage  = *on;
188500110525               ErrGenerico = *on;
188600110525               PosCurCom02 = *on;
188700110525               V1Dmsg      = $Msg(02);
188800110525              leavesr;
188900110525             ENDIF;
189000111223
189100111223       //?Commerciale "valido" data inizio e fine attività
189200130809         IF  CMMdtIni > wOggi  or
189300130809             CMMdtFin < wOggi;
189400111223           ErrMessage = *on;
189500111223           ErrGenerico = *on;
189600111223           PosCurCom02 = *on;
189700111223           V1Dmsg = $Msg(02);
189800111223           leavesr;
189900111223         ENDIF;
190000110525
190100100416         // Se appuntamento devo controllare anche se è stato inserito il FATTO
190200100416           IF  Appuntamento and V02fatto = *blanks;
190300100416             ErrGenerico = *on;
190400100416             ErrMessage  = *on;
190500100416             PosCurFatto = *on;
190600100416             V1Dmsg      = $msg(21);
190700100416             leavesr;
190800100416           ENDIF;
190900100416
191000090611
191100100601         // controllo data esecuzione contatto solo se non sono in appuntamento non eseguito
191200090611           If    V02dat <> 0;
191300090611             clear wlbdat;
191400090611             g02dat = v02dat;
191500090611             xsrda8(wlbdat);
191600090611             if g02err = '1';
191700090611               errMessage  = *on;
191800090611               errGenerico = *on;
191900090611               PosCurDat02 = *on;
192000090611               V1Dmsg = $Msg(03);
192100090611               leavesr;
192200090611             endif;
192300090611
192400090611             v02dat = g02dat;
192500090611             w02dat = g02inv;
192600090611           else ;
192700090611             errMessage  = *on;
192800090611             errGenerico = *on;
192900090611             PosCurDat02 = *on;
193000090611             V1Dmsg = $Msg(03);
193100090611             leavesr;
193200090611           endif;
193300100118
193400100601           If  (appuntamento and v02fatto =  'S') or appuntamento = *off ;
193500100118         // controllo che la data non sia inferiore al 2000 o superiore al 2039
193600100118         IF  w02dat < 20000101 or w02dat > 20391231;
193700100118           errMessage  = *on;
193800100118           errGenerico = *on;
193900100118           PosCurDat02 = *on;
194000100118           V1Dmsg = $Msg(03);
194100100118           leavesr;
194200100118         ENDIF;
194300090611
194400100330       // controllo che la data non sia maggiore di oggi ed inferiore a 3 gg lavorativi da oggi
194500100330         clear XGIOLAVDS;
194600100330         IXGLdata = %dec(%date());
194700100330         IXGLsub  = 'S';
194800100330         IXGLgg   = 3;
194900100330         IXGLpa   = 'P';
195000100330         IXGLlav  = 'S';
195100100330         IXGLfil = %dec(%subst(V02com:1:3):3:0);
195200100330         XGIOLAV (xgiolavds);
195300100330         IF  w02dat < OXGLdata;
195400100330           ErrMessage  = *on;
195500100330           ErrGenerico = *on;
195600100330           PosCurDAT02 = *on;
195700100330           V1dmsg      = $Msg(03);
195800100330           V1dmsg      = %trim(V1dmsg) + ' ' +
195900100330           'Non può essere inferiore a 3 gg.lavorativi';
196000100330           leavesr;
196100100330         ENDIF;
196200100330
196300100330         // controllo con la data del giorno
196400100330            data_iso = (%date()) ;
196500100408         IF  w02dat > %dec(data_iso) ;
196600100330           ErrMessage  = *on;
196700100330           ErrGenerico = *on;
196800100330           PosCurDAT02 = *on;
196900100330           V1dmsg      = $Msg(03);
197000100330           V1dmsg      = %trim(V1dmsg) + ' ' +
197100100330           'Non può essere maggiore di oggi';
197200100330           leavesr;
197300100330         ENDIF;
197400100330
197500090611         // controllo ora  esecuzione contatto
197600090611           If V02ora <> 0 ;
197700090611              sav02ora = %editc(v02ora:'X');
197800120105              If  ora > 23 or minuti > 59;
197900090611                   errMessage  = *on;
198000090611                   errGenerico = *on;
198100090611                   PosCurora02 = *on;
198200090611                   V1Dmsg = $Msg(05);
198300090611                   leavesr;
198400090611              endif;
198500091104           else ;
198600091104              errMessage  = *on;
198700091104              errGenerico = *on;
198800091104              PosCurora02 = *on;
198900091104              V1Dmsg = $Msg(05);
199000091104              leavesr;
199100090611           endif;
199200100114
199300100416           endif ;
199400100114
199500091116
199600090611         // - controllo / ricerca Causale contatto
199700090611           If    %scan('?' : V02cae) > *zero;
199800090611             errGenerico = *on;
199900090611             PosCurCae02 = *on;
200000090611             clear  dCCO ;
200100090611             clear  Tntb74ds;
200200091218             iccouti = 'S';
200300100225          // imposto anche il tipo attività che sto eseguendo x selezionare le sole
200400100225          // causali possibili
200500100225             iccoese = imk19tat ;
200600100322          // imposto anche se sto eseguendo attività con il solo codice cliente oppure
200700100322          // potenziale mai codificato , e se c'è trattativa
200800110429          //   If v02cli > 0 ;
200900110429          //      iccocod = 'C' ;
201000110429          //   else ;
201100110429          //      if $Gia_codifica = *on ;
201200110429          //         iccocod = 'C' ;
201300110429          //      else ;
201400110429          //         iccocod = 'P' ;
201500110429          //      endif ;
201600110429          //   endif;
201700100322
201800110429          // imposto la cateoria del potenziale
201900110502               iccocod = cpofls ;
202000110429
202100100322             If v02vis > 0 ;
202200100322                iccotra = 'S' ;
202300100505                iccotpv = W_vistpv ;
202400100322             else ;
202500100322                iccotra = 'N' ;
202600100322             endif ;
202700100324           // verifico le offerte
202800100330             iccooff = 'N';
202900101220             setll v02vis tivof11l ;
203000101220             reade v02vis tivof11l ;
203100101220             dow not %eof(tivof11l) ;
203200100421                 if vofeso <> 'H' and vofeso <> '*';
203300100330                    iccooff = 'S';
203400100330                    leave ;
203500100330                 endif ;
203600101220                 reade v02vis tivof11l ;
203700100330             enddo ;
203800110412           // passo la causale al pgm di ricerca
203900100421                iccocco = atccad ;
204000100322
204100090611             TNTB74R  (kpjba : tntb74ds);
204200090611             if  occoerr = *off   ;
204300090611               V02cae = Occoke1;
204400091218               dcco   = Occouni;
204500090611               V02caed = §ccodes ;
204600090611               errGenerico = *on;
204700090611               leavesr;
204800090611             else;
204900090611               errMessage  = *on;
205000090611               errGenerico = *on;
205100090611               V1Dmsg = T02msg;
205200090611               leavesr;
205300090611             endif;
205400090611           endif;
205500090611
205600090611           If    V02cae <> *blanks  ;
205700090611             clear  dCCO;
205800090611             clear  TIBS02ds;
205900090611             T02mod = 'C';
206000090611             T02cod = 'CCO';
206100090611             T02ke1 = v02cae ;
206200090611             T02sif = KNSIF;
206300090611             TNTBE_RicercaControllo  (kpjba : tibs02ds);
206400090611             if  T02err = *blanks;
206500090611               dcco = t02uni ;
206600100225               §ccoTAAds = §CCOtaa;
206700100505               §ccoTPVds = §CCOtpv;
206800110502               §ccoCPOds = §CCOcpo;
206900090611               V02caed = §ccodes ;
207000090702           // verifico se causale contatto può essere utilizzata dall'utente
207100090702                If   §ccouti = 'N' ;
207200090702                   errMessage  = *on;
207300090702                   errGenerico = *on;
207400090702                   PosCurCae02 = *on;
207500090702                   V1Dmsg = $msg(14);
207600090702                   leavesr;
207700090702                endif;
207800090611             else;
207900090611               errMessage  = *on;
208000090611               errGenerico = *on;
208100090611               PosCurCae02 = *on;
208200090611               V1Dmsg = T02msg;
208300090611               leavesr;
208400090611             endif;
208500090611           else;
208600090611             errMessage  = *on;
208700090611             errGenerico = *on;
208800090611             PosCurCae02 = *on;
208900090611             V1Dmsg = $msg(08);
209000090611             leavesr;
209100090611           endif;
209200110405
209300110405           // verifico se causale può essere utilizzata in base al flag cliente che non spedisce
209400110405           If  $cli_non_sped = *on and §CCOucns = 'N' ;
209500110405                   errMessage  = *on;
209600110405                   errGenerico = *on;
209700110405                   PosCurCae02 = *on;
209800110502                   IF ATCcad <> '  ' ;
209900110502                      V1Dmsg = $msg(36);
210000110502                      %subst(V1Dmsg:12:2) = V02cae;
210100110502                      %subst(V1Dmsg:66:2) = ATCcad;
210200110502                   else ;
210300110502                      V1Dmsg = $msg(14);
210400110502                   Endif ;
210500110405                   leavesr;
210600110405           endif;
210700110502
210800110502           // verifico se causale può essere utilizzata in base al flag cliente che non spedisce
210900110502           If  $cli_non_sped = *off and §CCOucns = 'S' ;
211000110502                   errMessage  = *on;
211100110502                   errGenerico = *on;
211200110502                   PosCurCae02 = *on;
211300110502                   IF ATCcad <> '  ' ;
211400110502                      V1Dmsg = $msg(36);
211500110502                      %subst(V1Dmsg:12:2) = V02cae;
211600110502                      %subst(V1Dmsg:66:2) = ATCcad;
211700110502                   else ;
211800110502                      V1Dmsg = $msg(14);
211900110502                   Endif ;
212000110502                   leavesr;
212100110502           endif;
212200100421
212300110321         //?Controllo se la causale di esecuzione può essere utilizzata per l'attività che sto
212400110321         //?gestendo
212500110321           IF  §CCOcae <> *blanks and §CCOcae <> ATCcad;
212600110321             ErrMessage  = *on;
212700110321             ErrGenerico = *on;
212800110321             PosCurCae02 = *on;
212900110502             IF ATCcad <> '  ' ;
213000110502                V1Dmsg = $msg(36);
213100110502                %subst(V1Dmsg:12:2) = V02cae;
213200110502                %subst(V1Dmsg:66:2) = ATCcad;
213300110502             else ;
213400110502                V1Dmsg = $msg(14);
213500110502             Endif ;
213600110321             leavesr;
213700110321           ENDIF;
213800100421
213900110428           // verifico se causale da esitare è la 32 deve usare solo la 42
214000110428              If atccad = '32' and v02cae <> '42' ;
214100110428                errMessage  = *on;
214200110428                errGenerico = *on;
214300110428                PosCurCae02 = *on;
214400110428                V1Dmsg = $msg(33);
214500110428                leavesr;
214600110428              endif ;
214700110428
214800110428           // verifico se causale da esitare è la 71 deve usare solo la 72
214900110428              If atccad = '71' and v02cae <> '72' ;
215000110428                errMessage  = *on;
215100110428                errGenerico = *on;
215200110428                PosCurCae02 = *on;
215300110502                IF ATCcad <> '  ' ;
215400110502                   V1Dmsg = $msg(36);
215500110502                   %subst(V1Dmsg:12:2) = V02cae;
215600110502                   %subst(V1Dmsg:66:2) = ATCcad;
215700110502                else ;
215800110502                   V1Dmsg = $msg(14);
215900110502                Endif ;
216000110428                leavesr;
216100110428              endif ;
216200110428
216300100225           // verifico se causale può essere utilizzata per esitare questo tipo attività
216400100225           If  §CCOtaa <> *blanks and
216500100225                 %lookup(imk19tat : $Taa) = *zero ;
216600100225                   errMessage  = *on;
216700100225                   errGenerico = *on;
216800100225                   PosCurCae02 = *on;
216900100225                   V1Dmsg = $msg(23);
217000100225                   leavesr;
217100100225           endif;
217200100322
217300100322           // verifico se causale può essere utilizzata in presenza di trattative con offerte
217400100322           If  §CCOoff = 'S' and v02vis > 0 ;
217500101220                   setll v02vis tivof11l ;
217600100322                   If not %equal ;
217700100322                      errMessage  = *on;
217800100322                      errGenerico = *on;
217900100322                      PosCurCae02 = *on;
218000100322                      V1Dmsg = $msg(31);
218100100322                      leavesr;
218200100421                   else ;
218300101220                      reade v02vis tivof11l ;
218400101220                      dow not %eof(tivof11l) ;
218500100421                          if vofeso  =  '*' ;
218600100421                             errMessage = *on ;
218700100421                          else ;
218800100421                             errMessage = *off;
218900100421                             leave ;
219000100421                          endif ;
219100101220                      reade v02vis tivof11l ;
219200100421                      enddo ;
219300100421                   // se ho trovato tutti annullati emetto errore
219400100421                      If errMessage  = *on;
219500100421                         errGenerico = *on;
219600100421                         PosCurCae02 = *on;
219700100421                         V1Dmsg = $msg(31);
219800100421                         leavesr;
219900100421                      endif ;
220000100322                   endif ;
220100100322           endif;
220200100322
220300100322           // verifico se causale non può essere utilizzata in presenza di trattative con offerte
220400100225           If  §CCOoff = 'N' and v02vis > 0 ;
220500101220                   setll v02vis tivof11l ;
220600101220                   reade v02vis tivof11l ;
220700101220                   dow not %eof(tivof11l) ;
220800100421                       if vofeso <> 'H' and vofeso <> '*' ;
220900100309                          errMessage  = *on;
221000100309                          errGenerico = *on;
221100100309                          PosCurCae02 = *on;
221200100309                          V1Dmsg = $msg(24);
221300100309                          leavesr;
221400100309                       endif ;
221500101220                   reade v02vis tivof11l ;
221600100309                   enddo ;
221700100225           endif;
221800100505           // verifico se causale in presenza di trattativa può essere usata per solo un
221900100505           // alcuni tipi trattativa
222000100505           If      §ccotpv <> *blanks  and %lookup(W_vistpv : $tpv) = *zero ;
222100100505                          errMessage  = *on;
222200100505                          errGenerico = *on;
222300100505                          PosCurCae02 = *on;
222400100505                          V1Dmsg = %trimr($msg(35)) + ' ' + W_vistpv ;
222500100505                          leavesr;
222600100505           endif ;
222700100308
222800100308           // verifico se causale può essere utilizzata solo per codificati/mai codificati o tutti
222900100308
223000100308           // solo causali per clienti codificati
223100110502           // If  §CCOcod = 'C' and V02cli = *zeros ;
223200110502           If  §CCOcpo <> *blanks and %lookup(cpofls:$cpo)= 0 ;
223300100308               errMessage  = *on;
223400100308               errGenerico = *on;
223500100308               PosCurCae02 = *on;
223600100308               V1Dmsg = $msg(25);
223700110504               v1dmsg = %subst(V1Dmsg:  1: 40) + '"' + §cpodesc + '"' ;
223800100308               leavesr;
223900100308           endif ;
224000090611
224100100308
224200100308           // verifico se causale non deve essere legata a trattativa
224300100308           If  §CCOtra = 'N' and V02vis >  *zeros ;
224400100308               errMessage  = *on;
224500100308               errGenerico = *on;
224600100308               PosCurCae02 = *on;
224700100308               V1Dmsg = $msg(27);
224800100308               leavesr;
224900100308           endif;
225000100308
225100100309            // verifico se causale chiude trattativa e l'attività è legata a trattativa
225200100309             if  §ccoCHT = 'S' and v02vis > 0 ;
225300100520                   chain(n)  V02vis tivis05l;
225400100308                   // se trattativa già esitata errore
225500100308                   IF  %found(tivis05l) and visesi = 'P ' ;
225600100308                       errMessage  = *on;
225700100308                       errGenerico = *on;
225800100308                       PosCurCae02 = *on;
225900100308                       V1Dmsg = $msg(28);
226000100308                       leavesr ;
226100100308                   else ;
226200100308                       $chiedi_conf = *on ;
226300100308                   endif ;
226400100308             endif ;
226500090611
226600091118            // pulisco il flag della trattativa nuova se non devo aprire trattativa
226700091118                if §ccoapt = ' ' ;
226800091118                    $new_trattati = *off ;
226900091118                endif ;
227000100308            // verifico se causale vuole le note
227100100308            If §cconot = 'S' and v02no1 = *blanks and v02no2 = *blanks ;
227200100308                   errMessage  = *on;
227300100308                   errGenerico = *on;
227400100308                   PosCurNo102 = *on;
227500100308                   V1Dmsg = $msg(29);
227600100308                   leavesr;
227700100308            endif ;
227800100318
227900100318            // verifico se causale non deve essere legata a trattativa do errore
228000100520                if    §ccotra = 'S' and v02vis = 0 and §ccoapt <> 'S';
228100100318                   errMessage  = *on;
228200100318                   errGenerico = *on;
228300100318                   PosCurCae02 = *on;
228400100318                   V1Dmsg = $msg(30);
228500100318                   leavesr;
228600100318                endif ;
228700100318
228800100329
228900100329            // verifico se causale legata a trattativa, se non è un'attività con trattativa
229000100329            // la creo nuova
229100100329                if    (§ccotra = 'S' and v02vis = 0) or
229200100329                      (§ccotra <>'S' and v02vis = 0) ;
229300100329                  clear newnrv;
229400100329                  clear sav_newnrv;
229500100329                endif ;
229600100329
229700100318            // verifico se causale legata a trattativa, se non è un'attività con trattativa
229800100318            // la creo nuova altrimenti propongo la trattativa dell'attività
229900100318                if    §ccotra = 'S' and v02vis > 0 ;
230000100318                      newnrv = v02vis   ;
230100100318                      sav_newnrv = v02vis   ;
230200100318                      leavesr;
230300100318                endif ;
230400100118
230500100118            // verifico se causale apre sempre trattativa e se ce nè già do errore
230600100118                if    §ccoapt = 'S' and v02vis > 0 ;
230700100118                   errMessage  = *on;
230800100118                   errGenerico = *on;
230900100118                   PosCurCae02 = *on;
231000100118                   V1Dmsg = $msg(19);
231100100118                   leavesr;
231200100118                endif ;
231300100118
231400100118            // se passati tutti i controlli e devo aprire trattativa recupero
231500100118            // il numero trattativa
231600100118            // se non avevo già salvato prima il n. recuperato
231700100202            if newnrv = 0 and §ccoapt = 'S'  and sav_newnrv = 0;
231800091113            exsr Sr_Recnum ;
231900091113            if  $ErrGrave = *on;
232000091113                leavesr;
232100091113            endif;
232200091113            endif;
232300100118
232400100118            if sav_newnrv > 0 and §ccoapt = 'S';
232500100118              newnrv = sav_newnrv;
232600100118              $new_trattati = sav_newtrat;
232700100118            endif;
232800100118
232900090611       ENDSR;
233000090611
233100091113       //--------------------------------------------------------------
233200091113       //?Recupero numero trattativa nuova
233300091113       //--------------------------------------------------------------
233400091113       BEGSR sr_Recnum;
233500091113
233600100413                reset $NumOk ;
233700100413
233800100413                RESET trul48ds;
233900100413                trul48r(kpjba:trul48ds);
234000100413
234100100413                If o48Err = 0;
234200100413                   newnrv  = o48nrv;
234300100413                   sav_newnrv  = o48nrv;
234400100413                   $NumOk = *on ;
234500100413                else ;
234600100413                   $ErrGrave   = *on;
234700100413                   errMessage  = *on;
234800100413                   errGenerico = *on;
234900100413                   V1Dmsg = o48msg ;
235000100413                   leavesr;
235100100413                Endif;
235200100413
235300091116        // se trovato un nuovo numero lo segnalo
235400091116                 If   $NumOk = *on ;
235500091116                   $New_trattati = *on ;
235600100118                   sav_newtrat = *on ;
235700091116                 endif ;
235800091113
235900091113       endsr;
236000091113
236100090611       //--------------------------------------------------------------
236200090611       //?Controllo videata D02         SCADENZA ATTIVITA'
236300090611       //--------------------------------------------------------------
236400090611       BEGSR CtrD02Scd ;
236500090611
236600090611         // controllo data attività da fare
236700090611           If    V02scd <> 0;
236800090611             clear wlbdat;
236900090611             g02dat = v02scd;
237000090611             xsrda8(wlbdat);
237100090611             if g02err = '1';
237200090611               errMessage  = *on;
237300090611               errGenerico = *on;
237400090611               PosCurScd02 = *on;
237500090611               V1Dmsg = $Msg(03);
237600090611               leavesr;
237700090611             endif;
237800090611
237900090611             v02Scd = g02dat;
238000090611             w02Scd = g02inv;
238100090611           else ;
238200090611             errMessage  = *on;
238300090611             errGenerico = *on;
238400090611             PosCurScd02 = *on;
238500090611             V1Dmsg = $Msg(03);
238600090611             leavesr;
238700090611           endif;
238800100118
238900100118         // controllo che la data non sia inferiore al 2000 o superiore al 2039
239000100118         IF  w02scd < 20000101 or w02scd > 20391231;
239100100118           errMessage  = *on;
239200100118           errGenerico = *on;
239300100118           PosCurscd02 = *on;
239400100118           V1Dmsg = $Msg(03);
239500100118           leavesr;
239600100118         ENDIF;
239700090611
239800090611         // controllo ora  esecuzione contatto
239900090611           If V02Sco <> 0 ;
240000090611              sav02ora = %editc(v02Sco:'X');
240100120105              If  ora > 23 or minuti > 59;
240200090611                   errMessage  = *on;
240300090611                   errGenerico = *on;
240400090611                   PosCurSco02 = *on;
240500090611                   V1Dmsg = $Msg(05);
240600090611                   leavesr;
240700090611              endif;
240800090611           endif;
240900090611
241000090611         // Controllo codice commerciale (? Ricerca)
241100090611           If    %scan('?' : v02cmi) > 0  ;
241200090611             errGenerico = *on;
241300090611             PosCurCmi02 = *on;
241400130809             iMK43fil  = DUTpou;
241500130809             TRMK43R (kpjba : TRMK43ds);
241600130809             if  oMK43err = *off  and  oMK43fxx = *blank;
241700130809               v02cmi  = %editc( oMK43cmm : 'X' );
241800130809               v02cmid = oMK43des;
241900130809             endif;
242000090611             leavesr;
242100090611           endif;
242200100210
242300100210         // Commerciale che esegue il contatto obbligatorio
242400100210           If  v02cmi = *blanks or v02cmi = *zeros;
242500100210             errMessage  = *on;
242600100210             errGenerico = *on;
242700100210             PosCurCmi02 = *on;
242800100210             V1Dmsg = $Msg(02);
242900100210             leavesr;
243000100210           endif;
243100090611
243200090611         // Controllo codice commerciale che esegue il contatto
243300100210             IF  %check(digits:V02cmi) > 0;
243400100210               ErrMessage  = *on;
243500100210               ErrGenerico = *on;
243600100210               PosCurCmi02 = *on;
243700100210               V1Dmsg      = $Msg(02);
243800100210               leavesr;
243900100210             ENDIF;
244000100210
244100100210
244200100210           //?utente abilitato al commerciale
244300100721           //?lo controllo solo se attività NON legata a trattative e NON su cliente
244400100721             IF  V02vis <> 0 or V02cli <> 0;
244500100210             clear TNTAA1DS;
244600120810             ITAA1tipo = 'M';
244700120810             ITAA1caut = '§utegtc';
244800100210             ITAA1cmm  = %dec(V02cmi:7:0);
244900100519             ITAA1abi = 'RA';
245000100210             kpjbu     = tntaa1ds;
245100100210             tntaa1c (kpjba);
245200100210             TNTAA1DS = kpjbu;
245300100210             IF  OTAA1fabi = 'N';
245400100210               ErrMessage  = *on;
245500100210               ErrGenerico = *on;
245600100210               PosCurCmi02 = *on;
245700100210               V1Dmsg      = $Msg(02);
245800100210               leavesr;
245900100210             ENDIF;
246000100721             ENDIF;
246100100210
246200130809             CMMcod = %int(V02cmi);
246300130809             chain (CMMcod) AZCMM000;
246400130809             if  NOT  %found(AZCMM01L);
246500090611               errMessage  = *on;
246600090611               errGenerico = *on;
246700090611               PosCurCmi02 = *on;
246800090611               V1Dmsg = $Msg(02);
246900090611               leavesr;
247000100210             endif;
247100130809             V02cmid = CMMdes;
247200110525
247300130809             If  CMMPar <> ' ' ;
247400110525               ErrMessage  = *on;
247500110525               ErrGenerico = *on;
247600110525               PosCurCmi02 = *on;
247700110525               V1Dmsg      = $Msg(02);
247800110525              leavesr;
247900110525             ENDIF;
248000111223
248100111223       //?Commerciale "valido" data inizio e fine attività
248200130809         IF  CMMdtIni > wOggi  or
248300130809             CMMdtFin < wOggi;
248400111223           ErrMessage = *on;
248500111223           ErrGenerico = *on;
248600111223           PosCurCmi02 = *on;
248700111223           V1Dmsg = $Msg(02);
248800111223           leavesr;
248900111223         ENDIF;
249000100317
249100100317
249200100317         // Controllo se causale vuole controllo su commerciale interno
249300100318          // imposto flag di forzabile
249400100318               If sav_v02cmi2 <> v02cmi ;
249500100318                  $forzacomin2 = *off ;
249600100318               endif ;
249700130809            If §ccocoi <> ' ' and (CMMfun =  'COMIN' or CMMfun = 'ASC'
249800130809                                                     or CMMfun = 'SA');
249900100317         // verifico se msg forzabile
250000100318               If §ccocoi = 'F' and $forzacomin2= *off ;
250100100317                  errMessage  = *on;
250200100317                  errGenerico = *on;
250300100318                  PosCurCmi02 = *on;
250400100318                  V1Dmsg = %trimr($Msg(06)) + ' Enter per forzare' ;
250500100318                  $forzacomin2= *on ;
250600100318                  sav_v02cmi2 = v02cmi ;
250700100317                  leavesr;
250800100318               Endif;
250900100318               If §ccocoi <> 'F' ;
251000100317                  errMessage  = *on;
251100100317                  errGenerico = *on;
251200100318                  PosCurCmi02 = *on;
251300100318                  V1dmsg = $Msg(06) ;
251400100317                  leavesr;
251500100317               Endif ;
251600100317             endif;
251700100126
251800100330
251900100330       // controllo che la data non sia inferiore a 3 gg lavorativi da oggi
252000100330         clear XGIOLAVDS;
252100100330         IXGLdata = %dec(%date());
252200100330         IXGLsub  = 'S';
252300100330         IXGLgg   = 3;
252400100330         IXGLpa   = 'P';
252500100330         IXGLlav  = 'S';
252600100330         IXGLfil = %dec(%subst(V02cmi:1:3):3:0);
252700100330         XGIOLAV (xgiolavds);
252800100330         IF  w02scd < OXGLdata;
252900100330           ErrMessage  = *on;
253000100330           ErrGenerico = *on;
253100100330           PosCurScd02 = *on;
253200100330           V1dmsg      = $Msg(03);
253300100330           V1dmsg      = %trim(V1dmsg) + ' ' +
253400100330           'Non può essere inferiore a 3 gg.lavorativi';
253500100330           leavesr;
253600100330         ENDIF;
253700100330
253800100330       // controllo che la data non sia maggiore a 14 mesi  da oggi
253900100330         clear XGIOLAVDS;
254000100330         IXGLdata = %dec(%date());
254100100330         IXGLadd  = 'S';
254200100330         IXGLmm   = 14;
254300100330         IXGLpa   = 'P';
254400100330         IXGLfil = %dec(%subst(V02cmi:1:3):3:0);
254500100330         XGIOLAV (xgiolavds);
254600100330         IF  w02scd > OXGLdata;
254700100330           ErrMessage  = *on;
254800100330           ErrGenerico = *on;
254900100330           PosCurScd02 = *on;
255000100330           V1dmsg      = $Msg(03);
255100100330           V1dmsg      = %trim(V1dmsg) + ' ' +
255200100330           'Non può essere maggiore di 14 mesi';
255300100330           leavesr;
255400100330         ENDIF;
255500100330
255600100330
255700090804         // In caso di appuntamento controllo anche i campi seguenti
255800090804
255900090804       If Appuntamento = *on ;
256000090804
256100100310         //?Verifico se il commerciale dell'attività è diverso dal
256200100310         //?commerciale della trattativa
256300100310         //?se attività legata a trattativa
256400100310         // solo se si tratta di appuntamento controllo il commerciale
256500100310         // inoltre se vengo da trattativa il messaggio è forzabile
256600100310         IF  vis_trattativ  ;
256700100310           clear tnta45ds ;
256800100310           ITA45tip = 'C';
256900100310           ITA45nrv = v02vis;
257000100310           ITA45cmm = %dec(v02cmi:7:0);
257100100310           tnta45r (tnta45ds) ;
257200100310           IF  OTA45err = 'C';
257300100311          // imposto flag di forzabile
257400100311               If sav_v02cmi <> v02cmi ;
257500100311                  $forzacomm = *off ;
257600100311               endif ;
257700100310       // se vengo da trattativa il msg è forzabile
257800100310             If $forzacomm = *off ;
257900100310                ErrMessage  = *on;
258000100310                ErrGenerico = *on;
258100100310                PosCurCmi02 = *on;
258200100310                If imk19fpr = 'T' ;
258300100310                   V1Dmsg = %trimr(OTA45msg) + ' Enter per forzare';
258400100310                   $forzacomm = *on ;
258500100311                   sav_v02cmi = v02cmi ;
258600100310                else ;
258700100310                   V1Dmsg = OTA45msg;
258800100310                leavesr;
258900100310                Endif ;
259000100310             Endif ;
259100100310           ENDIF;
259200100310         ENDIF;
259300100310
259400090804        // verifico l'inserimento delle ore di impegno
259500090804
259600090804         // controllo ora  inizio impegno
259700090804           If V02oii <> 0 ;
259800090804              sav02ora = %editc(V02oii:'X');
259900120105              If  ora > 23 or minuti > 59;
260000090804                   errMessage  = *on;
260100090804                   errGenerico = *on;
260200090804                   PosCurOii02 = *on;
260300090804                   V1Dmsg = $Msg(05);
260400090804                   leavesr;
260500090804              endif;
260600091026           else ;
260700091026              errMessage  = *on;
260800091026              errGenerico = *on;
260900091026              PosCurOii02 = *on;
261000091111              V1Dmsg = $Msg(17);
261100091026              leavesr;
261200090804           endif;
261300090804
261400090804         // controllo ora  fine   impegno
261500090804           If V02ofi <> 0 ;
261600090804              sav02ora = %editc(V02ofi:'X');
261700120105              If  ora > 23 or minuti > 59;
261800090804                   errMessage  = *on;
261900090804                   errGenerico = *on;
262000090804                   PosCurOfi02 = *on;
262100090804                   V1Dmsg = $Msg(05);
262200090804                   leavesr;
262300090804              endif;
262400091026           else ;
262500091026              errMessage  = *on;
262600091026              errGenerico = *on;
262700091026              PosCurOfi02 = *on;
262800091111              V1Dmsg = $Msg(17);
262900091026              leavesr;
263000090804           endif;
263100090804
263200090804         // controllo ora  inizio se congruente con fine impegno
263300090804           If V02oii > 0 and V02ofi = 0;
263400090804              errMessage  = *on;
263500090804              errGenerico = *on;
263600090804              PosCurOfi02 = *on;
263700090804              V1Dmsg = $Msg(13);
263800090804              leavesr;
263900090804           endif;
264000090804
264100090804         // controllo ora  inizio se congruente con fine impegno
264200090804           If V02oii > 0 and V02ofi > 0 and V02oii > V02ofi ;
264300090804              errMessage  = *on;
264400090804              errGenerico = *on;
264500090804              PosCurOfi02 = *on;
264600090804              V1Dmsg = $Msg(13);
264700090804              leavesr;
264800090804           endif;
264900090804
265000090804         // verifico se inizioe fine impegno è congruente con ora appuntamento
265100090804           If V02oii > 0 and V02ofi > 0 and (V02oii > V02sco or
265200090804              V02ofi < V02sco) ;
265300090804              errMessage  = *on;
265400090804              errGenerico = *on;
265500090804              PosCurOfi02 = *on;
265600090804              V1Dmsg = $Msg(15);
265700090804              leavesr;
265800090804           endif ;
265900091216
266000091216         //?Verifico se il commerciale è libero
266100100322         // gli passo anche l'attività del momento altrimenti non la esclude
266200091216           clear trmk84ds;
266300091216           IMK84cmm = %dec(v02cmi:7:0);
266400091216           IMK84dad = w02scd;
266500091216           IMK84oii = v02oii;
266600091216           IMK84ofi = v02ofi;
266700100322           IMK84tat = imk19tat ;
266800100322           IMK84atn = imk19atn ;
266900100322           IMK84atnp= imk19atnp;
267000091216           trmk84r (trmk84ds);
267100091216         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
267200091216           IF  OMK84err <> *blanks and OMK84err <> '9' and
267300091216               Not $ForzaImpegno;
267400091216             ErrGenerico = *on;
267500091216             ErrMessage  = *on;
267600091216             PosCurScd02 = *on;
267700091216             V1Dmsg = OMK84msg;
267800091216             $ForzaImpegno = *on;
267900091216             leavesr;
268000091216           ENDIF;
268100090611
268200090804       endif;
268300090804
268400090611       ENDSR ;
268500090611
268600090611       //--------------------------------------------------------------
268700090611       //?Gestione videata W03
268800090611       //--------------------------------------------------------------
268900090611       BEGSR GesW03;
269000090611
269100090611         // Inizializzazione videata
269200090611         if $InzW03 = *on;
269300090611           exsr InzW03;
269400090611           $InzW03 = *off;
269500090611         endif;
269600090611
269700090611
269800090611         // Emissione videata
269900090611         exfmt MK19W03;
270000090611         errMessage   = *off;
270100090611         errGenerico  = *off;
270200090611         clear W03msg;
270300090611
270400090611         select;
270500090611         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
270600090611           when  $ErrGrave = *on;
270700090611             $Fine  = *on;
270800100927
270900100927       //?F02 = Rubrica
271000100927           WHEN dsp_aid = c_F02;
271100100927             exsr F02D02;
271200100927             leavesr;
271300100927
271400090611         // F12 = Ritorno
271500090611           when dsp_aid = c_F12;
271600090611             exsr F12W03;
271700090611
271800091026         // F11 = Agenda
271900091026           when dsp_aid = c_F11;
272000091027             exsr F11w03;
272100090611
272200090611         // Enter
272300090611           other;
272400090611           exsr CtrW03;
272500090611           if errGenerico = *on ;
272600090611               leavesr;
272700090611           endif;
272800090611         endsl;
272900100329
273000100329         // se dato f8 conferma (solo in caso di appuntamento )
273100100329           If   dsp_aid = c_F08 ;
273200100329                If V02rtr = *blanks or v02mrt = *blanks ;
273300100329                   errMessage  = *on;
273400100329                   errGenerico = *on;
273500100329                   PosCurCom03 = *on;
273600100329                   W03msg = $Msg(32);
273700100329                   leavesr;
273800100329                 endif;
273900100329           endif ;
274000100329
274100100329
274200090611
274300100329         // se dato f6 conferma  / f8 conferma + mail appuntamento
274400100309
274500100309         // se dato f6 sulla videata di creazione nuova attività e non solo invio  lettera
274600100920           If    dsp_aid = c_F06  or
274700100920                 dsp_aid = c_F08  ;
274800090611                   exsr f06w03d02;
274900091210                   IF  OMK22f12 = 'S';
275000091210                     leavesr;
275100091210                   ENDIF;
275200100309
275300100309         // comitto se richiesto e se non ci sono errori
275400100309                 If   imk19cmt = 'S' and omk19err = ' ';
275500100309                      commit ;
275600100309                 endif ;
275700100115
275800100921         // se F6
275900100115           // devo aggiornare il commerciale sul potenziale nel caso non sia già
276000100115           // impostato
276100100517                chain(n)  atccpo  TNCPO01L;
276200100610                IF  not %found(TNCPO01L) or
276300100610                    (%found(tncpo01l) and cpocmm > 0) ;
276400100115                  leavesr;
276500100115                ENDIF;
276600100517           // se devo aggiornare il commerciale aggancio in aggiornamento
276700100517           // il file tncpo e ne verifico l'allocazione
276800100517                chain(e) atccpo TNCPO01l ;
276900100517             If not %error;
277000100920                Cpocmm = %dec(v02com:7:0);
277100100115                Cpoflt = %dec(%subst(%editc(cpocmm:'X'):1:3):3:0);
277200100115                update TNCPO000;
277300100517             ENDIF;
277400100329
277500100329           endif;
277600100329
277700090611       ENDSR;
277800090611
277900090611
278000090611       //--------------------------------------------------------------
278100090611       //?Caricamento videata conferma W03
278200090611       //--------------------------------------------------------------
278300090611       BEGSR InzW03;
278400090611
278500090611          clear mk19w03 ;
278600090611
278700090611             W03cae = v02cae ;
278800090611             W03caed= v02caed;
278900091113             If newnrv > 0 ;
279000091113                w03nrv = newnrv ;
279100091113             else ;
279200091113                W03nrv = v02vis ;
279300091113             endif ;
279400100311         // se la causale chiude evntualmente la  trattativa non lego il numero trattativa
279500100311         // all'attività futura
279600100311             If §ccocht = 'S' and w03nrv > 0 ;
279700100311                clear w03nrv ;
279800100311             endif ;
279900091113
280000091113             trattativa = (w03nrv > 0);
280100091118        // Se trattativa nuova evidenzio
280200091118             Nuova = ($new_trattati = *on and trattativa = *on) ;
280300090611
280400091021       // in caso di futuro contatto appuntamento non propongo nulla
280500091021        If §ccotat <> 'A' ;
280600091021
280700100924       // calcolo data scadenza se segno causale <> da '='
280800100924            If §ccoupm = '+' or §ccoupm = '-' ;
280900100924               clear XGIOLAVDS;
281000100924               IXGLdata = %dec(%date());
281100100924               If §ccoupm = '+' ;
281200100924                  IXGLadd  = 'S';
281300100924               Else ;
281400100924                  IXGLsub  = 'S';
281500100924               Endif ;
281600100924               IXGLgg   = §CCOgio;
281700100924               IXGLpa   = 'P';
281800100924               If  V02com > *zeros;
281900100924                  IXGLfil = %dec(%subst(V02com:1:3):3:0);
282000100924               else ;
282100100924                  IXGLfil = DUTpou;
282200100924               Endif;
282300100924               XGIOLAV (xgiolavds);
282400100924               If  OXGLerr = *blanks;
282500100924                   data_eur = %date(OXGLdata:*iso);
282600100924                   W03dsc   = %dec(data_eur);
282700100924               Endif;
282800100924            Endif ;
282900100924       // calcolo data scadenza se segno causale = ad '='
283000100924            If §ccoupm = '=' ;
283100101111                   W03dsc   = *date;
283200100924            Endif ;
283300090611
283400090611       // propongo l'ora del momento
283500090611             w03osc = %dec(%subst(%EDITC(%dec(%time()):'X'):1:4):4:0);
283600090611
283700090611       // propongo lo stesso commerciale che ha eseguito il contatto
283800090611             w03com  = v02com  ;
283900090611             w03comd = v02comd ;
284000091021
284100091021        endif ;
284200091021
284300100326       // in caso di futuro contatto appuntamento propongo commerciale trattativa
284400100326        If §ccotat = 'A' ;
284500100326       // propongo lo stesso commerciale della trattativa
284600100329       // richiamo il tnta45 con com mercilae 9999999 così mi torna il commerciale trattativa
284700100326           clear tnta45ds ;
284800100326           ITA45tip = 'C';
284900100326           ITA45nrv = v02vis;
285000100329           ITA45CMM = 9999999 ;
285100100326           tnta45r (tnta45ds) ;
285200100326           IF  OTA45err = 'C';
285300100326             w03com  = %editc(ota45cmm:'X')  ;
285400130809             clear  w03comD;
285500130809             chain (oTA45cmm) AZCMM000;
285600130809             if  %found(AZCMM01L);
285700130809               w03comd = CMMdes;
285800130809             endif;
285900100326           Endif ;
286000100326
286100100326        Endif ;
286200100326
286300090630       // verifico se il prossimo contatto ha una tipologia telefonata o appuntamento
286400100210             prox_telefon= (§ccotat = 'T' or §ccotat = 'O');
286500091015             prox_appunta= (§ccotat = 'A');
286600101221
286700101221       //?Se impostata la data caldo senza offerta la devo proporre
286800101221           IF  sav_DataHot <> 0;
286900101221             data_eur = %date(sav_DataHot:*iso);
287000101221             W03dsc   = %dec(data_eur);
287100101221           ENDIF;
287200091210
287300091210         //?imposto fisso 'N' per affiancamento
287400091210         W03aff = 'N';
287500100504
287600100504       //?Se appuntamento e commerciale già presente posiziono il cursore sulla data
287700100504         IF  prox_appunta and w03com > *zeros;
287800100504           PosCurDsc03 = *on;
287900100504         ENDIF;
288000091216
288100091216         $ForzaImpegno = *off;
288200090630
288300090611       ENDSR;
288400090611
288500090611       //--------------------------------------------------------------
288600090611       //?Controllo videata W03
288700090611       //--------------------------------------------------------------
288800090611       BEGSR CtrW03;
288900090611
289000090611            WindDspF = IndDspF;
289100090611            reset WindDspFb;
289200090611            IndDspF  = WindDspF;
289300090611
289400090611         // Controllo codice commerciale (? Ricerca)
289500090611           If    %scan('?' : W03com) > 0  ;
289600090611             errGenerico = *on;
289700090611             PosCurCom03 = *on;
289800130809             clear  TRMK43ds;
289900130809             iMK43fil  = DUTpou;
290000130809             TRMK43R (kpjba : TRMK43ds);
290100130809             if  oMK43err = *off  and  oMK43fxx = *blank;
290200130809               W03com  = %editc( oMK43cmm : 'X' );
290300130809               W03comd = oMK43des;
290400130809             endif;
290500090611             leavesr;
290600090611           endif;
290700090611
290800100210         // Commerciale che esegue il contatto obbligatorio
290900100210           If  W03com = *blanks or w03com = *zeros;
291000100210             errMessage  = *on;
291100100210             errGenerico = *on;
291200100210             PosCurCom03 = *on;
291300100210             W03msg = $Msg(02);
291400100210             leavesr;
291500100210           endif;
291600100210
291700090611         // Controllo codice commerciale che esegue il contatto
291800100210             IF  %check(digits:w03com) > 0;
291900100210               ErrMessage  = *on;
292000100210               ErrGenerico = *on;
292100100210               PosCurCom03 = *on;
292200100210               W03msg = $Msg(02);
292300100210               leavesr;
292400100210             ENDIF;
292500100210
292600100210           //?utente abilitato al commerciale
292700100210             clear TNTAA1DS;
292800120810             ITAA1tipo = 'M';
292900120810             ITAA1caut = '§utegtc';
293000100210             ITAA1cmm  = %dec(w03com:7:0);
293100100519             ITAA1abi = 'RA';
293200100210             kpjbu     = tntaa1ds;
293300100210             tntaa1c (kpjba);
293400100210             TNTAA1DS = kpjbu;
293500100210             IF  OTAA1fabi = 'N';
293600100210               ErrMessage  = *on;
293700100210               ErrGenerico = *on;
293800100210               PosCurCom03 = *on;
293900100210               w03msg      = $Msg(02);
294000100210               leavesr;
294100100210             ENDIF;
294200100210
294300130809             CMmcod = %int(W03com);
294400130809             chain (CMMcod) AZCMM000;
294500130809             if  NOT  %found(AZCMM01L);
294600090611               errMessage  = *on;
294700090611               errGenerico = *on;
294800090611               PosCurCom03 = *on;
294900090611               W03msg = $Msg(02);
295000090611               leavesr;
295100100210             endif;
295200130809             W03comd = CMMdes;
295300100311
295400130809             If  CMMpar <> ' ' ;
295500110525               ErrMessage  = *on;
295600110525               ErrGenerico = *on;
295700110525               PosCurCom03 = *on;
295800110525               W03msg = $Msg(02);
295900110525              leavesr;
296000110525             ENDIF;
296100111223
296200111223       //?Commerciale "valido" data inizio e fine attività
296300130809         IF  CMMdtIni > wOggi  or
296400130809             CMMdtFin < wOggi;
296500111223           ErrMessage = *on;
296600111223           ErrGenerico = *on;
296700111223           PosCurCom03 = *on;
296800111223           W03msg = $Msg(02);
296900111223           leavesr;
297000111223         ENDIF;
297100100310
297200100318            If sav_w03com <> w03com ;
297300100318               $forzacomin = *off ;
297400100318            endif ;
297500100310         // Controllo se causale vuole controllo su commerciale interno
297600130809            If §ccocoi <> ' ' and (CMMfun =  'COMIN' or CMMfun = 'ASC'
297700130809                                                     or CMMfun = 'SA');
297800100310         // verifico se msg forzabile
297900100310               If §ccocoi = 'F' and $forzacomin = *off ;
298000100310                  errMessage  = *on;
298100100310                  errGenerico = *on;
298200100310                  PosCurCom03 = *on;
298300100310                  W03msg = %trimr($Msg(06)) + ' Enter per forzare' ;
298400100310                  $forzacomin = *on ;
298500100311                  sav_w03com = w03com ;
298600100310                  leavesr;
298700100318               Endif ;
298800100318               If §ccocoi <>'F' ;
298900100310                  errMessage  = *on;
299000100310                  errGenerico = *on;
299100100310                  PosCurCom03 = *on;
299200100310                  W03msg = $Msg(06) ;
299300100310                  leavesr;
299400100310               Endif ;
299500100310             endif;
299600100202
299700100202         //?Se sto pianificando una nuova attività che apre trattativa
299800100202         //?devo controllare che non ci sia già una trattativa aperte
299900100202         //?per lo stesso potenziale e commerciale
300000100202           IF  §CCOapt = 'S' and $New_Trattati;
300100100202             clear tnta43ds ;
300200100202             ITA43cpo = cpocpo;
300300100202             ITA43ksc = v02cli;
300400100202             ITA43nrv = w03nrv;
300500100202             ITA43cmm = %dec(w03com:7:0);
300600100202             tnta43r (TNTA43DS) ;
300700100202             IF  OTA43err = '+';
300800100202               ErrMessage  = *on;
300900100202               ErrGenerico = *on;
301000100202               PosCurCom03 = *on;
301100100202               W03msg = OTA43msg;
301200100202               leavesr;
301300100202             ENDIF;
301400100202           ENDIF;
301500100326       If Prox_appunta = *on ;
301600100326
301700100326         //?Verifico se il commerciale dell'appuntamento è diverso dal
301800100326         //?commerciale della trattativa
301900100422
302000100422            If sav_w03comt <> w03com ;
302100100422               $forzacomm3 = *off ;
302200100422            endif ;
302300100326         clear tnta45ds ;
302400100326         ITA45tip = 'C';
302500100326         ITA45nrv = w03nrv;
302600100326         ITA45cmm = %dec(w03com:7:0);
302700100326         tnta45r (tnta45ds) ;
302800100326         IF  OTA45err = 'C';
302900100422       // se vengo da trattativa il msg è forzabile
303000100422             If $forzacomm3= *off ;
303100100422                ErrMessage  = *on;
303200100422                ErrGenerico = *on;
303300100422                PosCurCom03 = *on;
303400100422                If imk19fpr = 'T' ;
303500100422                   W03msg = %trimr(OTA45msg) + ' Enter per forzare';
303600100422                   $forzacomm3= *on ;
303700100422                   sav_w03comt= w03com ;
303800100422                else ;
303900100422                   w03msg = OTA45msg;
304000100422                leavesr;
304100100422                Endif ;
304200100422             endif ;
304300100326         ENDIF;
304400100326         ENDIF;
304500100326
304600100504         //?Verifico se LA FILIALE DI APPARTENENZA DEL COMMERCIALE E' UGUALE
304700100504         //?ALLA FILIALE DI APPARTENENZA DEL COMMERCIALE CLIENTE SE ESISTE
304800100504         //?E SE E' PRIMO APPUNTAMENTO
304900100504
305000100504            If %SUBST(w03com: 1: 3) <> %subst(%editc(v02cmp:'X'): 1:3) and
305100100504               $new_trattati = *on and v02cli > 0 ;
305200100504                ErrMessage  = *on;
305300100504                ErrGenerico = *on;
305400100504                PosCurCom03 = *on;
305500100504                W03msg = $Msg(34);
305600100504                leavesr;
305700100504             endif;
305800090702
305900100326         // controllo data scadenza contatto
306000100326           If    w03dsc <> 0;
306100100326             clear wlbdat;
306200100326             g02dat = w03dsc;
306300100326             xsrda8(wlbdat);
306400100326             if g02err = '1';
306500100326               errMessage  = *on;
306600100326               errGenerico = *on;
306700100326               PosCurDsc03 = *on;
306800100326               W03msg = $Msg(03);
306900100326               leavesr;
307000100326             endif;
307100100326
307200100326             w03dsc = g02dat;
307300100326             WW03dsc = g02inv;
307400100326           else ;
307500100326             errMessage  = *on;
307600100326             errGenerico = *on;
307700100326             PosCurDsc03 = *on;
307800100326             W03msg = $Msg(03);
307900100326             leavesr;
308000100326           endif;
308100100326
308200100326         // controllo che la data non sia inferiore al 2000 o superiore al 2039
308300100326         IF  ww03dsc < 20000101 or ww03dsc > 20391231;
308400100326           errMessage  = *on;
308500100326           errGenerico = *on;
308600100326           PosCurDsc03 = *on;
308700100326           W03msg = $Msg(03);
308800100326           leavesr;
308900100326         ENDIF;
309000100330
309100100330       // controllo che la data non sia inferiore a 3 gg lavorativi da oggi
309200100330         clear XGIOLAVDS;
309300100330         IXGLdata = %dec(%date());
309400100330         IXGLsub  = 'S';
309500100330         IXGLgg   = 3;
309600100330         IXGLpa   = 'P';
309700100330         IXGLlav  = 'S';
309800100330         IXGLfil = %dec(%subst(W03com:1:3):3:0);
309900100330         XGIOLAV (xgiolavds);
310000100330         IF  ww03dsc < OXGLdata;
310100100330             errMessage  = *on;
310200100330             errGenerico = *on;
310300100330             PosCurDsc03 = *on;
310400100330             W03msg = $Msg(03);
310500100330             W03msg = %trim(W03msg) + ' ' +
310600100330                      'Non può essere inferiore a 3 gg.lavorativi';
310700100330             leavesr;
310800100330         ENDIF;
310900100330
311000100330       // controllo che la data non sia maggiore  a 14 mesi da oggi
311100100330         clear XGIOLAVDS;
311200100330         IXGLdata = %dec(%date());
311300100330         IXGLadd  = 'S';
311400100330         IXGLmm   = 14;
311500100330         IXGLpa   = 'P';
311600100330         IXGLfil = %dec(%subst(W03com:1:3):3:0);
311700100330         XGIOLAV (xgiolavds);
311800100330         IF  ww03dsc > OXGLdata;
311900100330             errMessage  = *on;
312000100330             errGenerico = *on;
312100100330             PosCurDsc03 = *on;
312200100330             W03msg = $Msg(03);
312300100330             W03msg = %trim(W03msg) + ' ' +
312400100330                    'Non può essere maggiore di 14 mesi';
312500100330             leavesr;
312600100330         ENDIF;
312700100330
312800100326
312900100326         // controllo ora  esecuzione contatto
313000100326           If W03osc <> 0 ;
313100100326              sav02ora = %editc(W03osc:'X');
313200120105              If  ora > 23 or minuti > 59;
313300100326                   errMessage  = *on;
313400100326                   errGenerico = *on;
313500100326                   PosCurOsc03 = *on;
313600100326                   W03msg = $Msg(05);
313700100326                   leavesr;
313800100326              endif;
313900100326           else ;
314000100326             errMessage  = *on;
314100100326             errGenerico = *on;
314200100326             PosCurOsc03 = *on;
314300100326             W03msg = $Msg(05);
314400100326             leavesr;
314500100326           endif;
314600100326
314700090702         // In caso di appuntamento controllo anche i campi seguenti
314800090702
314900090702       If Prox_appunta = *on ;
315000091216
315100090702        // verifico l'inserimento delle ore di impegno
315200090702
315300090702         // controllo ora  inizio impegno
315400090702           If W03oii <> 0 ;
315500090702              sav02ora = %editc(W03oii:'X');
315600120105              If  ora > 23 or minuti > 59;
315700090702                   errMessage  = *on;
315800090702                   errGenerico = *on;
315900090702                   PosCurOii03 = *on;
316000090702                   w03msg = $Msg(05);
316100090702                   leavesr;
316200090702              endif;
316300091216           else ;
316400091216             errMessage  = *on;
316500091216             errGenerico = *on;
316600091216             PosCurOii03 = *on;
316700091216             w03msg = $Msg(05);
316800091216             leavesr;
316900090702           endif;
317000090702
317100091216         // controllo ora  fine   impegno
317200090702           If W03ofi <> 0 ;
317300090702              sav02ora = %editc(W03ofi:'X');
317400120105              If  ora > 23 or minuti > 59;
317500090702                   errMessage  = *on;
317600090702                   errGenerico = *on;
317700090702                   PosCurOfi03 = *on;
317800090702                   w03msg = $Msg(05);
317900090702                   leavesr;
318000090702              endif;
318100091216           else ;
318200091216             errMessage  = *on;
318300091216             errGenerico = *on;
318400091216             PosCurOfi03 = *on;
318500091216             w03msg = $Msg(05);
318600091216             leavesr;
318700090702           endif;
318800090702
318900090702         // controllo ora  inizio se congruente con fine impegno
319000090702           If W03oii > 0 and W03ofi = 0;
319100090702              errMessage  = *on;
319200090702              errGenerico = *on;
319300090702              PosCurOfi03 = *on;
319400090702              w03msg = $Msg(13);
319500090702              leavesr;
319600090702           endif;
319700090702
319800090702         // controllo ora  inizio se congruente con fine impegno
319900090702           If W03oii > 0 and W03ofi > 0 and W03oii > w03ofi ;
320000090702              errMessage  = *on;
320100090702              errGenerico = *on;
320200090702              PosCurOfi03 = *on;
320300090702              w03msg = $Msg(13);
320400090702              leavesr;
320500090702           endif;
320600090804
320700090804         // verifico se inizioe fine impegno è congruente con ora appuntamento
320800090804           If W03oii > 0 and W03ofi > 0 and (W03oii > W03osc or
320900090804              W03ofi < W03osc) ;
321000090804              errMessage  = *on;
321100090804              errGenerico = *on;
321200090804              PosCurOfi03 = *on;
321300090804              w03msg = $Msg(15);
321400090804              leavesr;
321500090804           endif ;
321600091216
321700091216         //?Verifico se il commerciale è libero
321800091216           clear trmk84ds;
321900091216           IMK84cmm = %dec(w03com:7:0);
322000091216           IMK84dad = ww03dsc;
322100091216           IMK84oii = w03oii;
322200091216           IMK84ofi = w03ofi;
322300091216           trmk84r (trmk84ds);
322400091216         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
322500091216           IF  OMK84err <> *blanks and OMK84err <> '9' and
322600091216               Not $ForzaImpegno;
322700091216             ErrGenerico = *on;
322800091216             ErrMessage  = *on;
322900091216             PosCurDsc03 = *on;
323000091216             w03msg = OMK84msg;
323100091216             $ForzaImpegno = *on;
323200091216             leavesr;
323300091216           ENDIF;
323400090702
323500090702       endif;
323600090702
323700090702       ENDSR;
323800100108
323900100108       //--------------------------------------------------------------
324000100108       //?Gestione videata D04.
324100100108       //--------------------------------------------------------------
324200100108       BEGSR GesD04;
324300100108
324400100108         //?Inizializzazione videata
324500100108         IF  $InzD04;
324600100108           exsr InzD04;
324700100108           $InzD04 = *off;
324800100108         ENDIF;
324900100108
325000100108         //?Emissione testata
325100100108         write MK19T01;
325200100108
325300100108         //?Emissione videata
325400100108         exfmt MK19D04;
325500100108         ErrMessage  = *off;
325600100108         ErrGenerico = *off;
325700100108         clear V1Dmsg;
325800100108
325900100108         SELECT;
326000100108
326100100108         //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
326200100108           WHEN  $ErrGrave = *on;
326300100108             $Fine = *on;
326400100108             OMK19err = '1';
326500100108             OMK19msg = V1Dmsg;
326600100108
326700100108         //?F06 = Conferma
326800100108           WHEN  dsp_aid = c_F06;
326900100108             exsr CtrD04;
327000100108             IF  ErrGenerico;
327100100108               leavesr;
327200100108             ENDIF;
327300100108             exsr F06D04;
327400100108
327500100108         //?F12 = Ritorno
327600100108           WHEN  dsp_aid = c_F12;
327700100108             exsr F12D02;
327800100108             leavesr ;
327900100112
328000100112         //?F16 = Annulla
328100100112           WHEN  dsp_aid = c_F16;
328200100112             exsr F16D04;
328300100112             leavesr ;
328400100108
328500100108         //?F18 = Note
328600100108           WHEN dsp_aid = c_F18;
328700100121             exsr F18D04;
328800100108
328900100108         //?Enter
329000100108           OTHER;
329100100108         //?Se manutenzione controllo i dati
329200100108             IF  Not Visualizza;
329300100108               exsr CtrD04;
329400100108               IF  ErrGenerico;
329500100108                 leavesr;
329600100108               ENDIF;
329700100108             ENDIF;
329800100108
329900100108         ENDSL;
330000100108
330100100108       ENDSR;
330200100108
330300100108       //--------------------------------------------------------------
330400100108       //?Caricamento videata telefonata D04.
330500100108       //--------------------------------------------------------------
330600100108       BEGSR InzD04;
330700100108
330800100108         clear MK19D04;
330900100108
331000100108       //?Indicatori di comodo
331100100108         altrenote    = *off  ;
331200100108         piunote      = *off  ;
331300100108         GiornoTotale = *off;
331400100108
331500100108       //?Imposto se visualizzazione
331600100108         IF  IMK19fle = '5';
331700100108           V01tit     = tit_visua;
331800100108           Visualizza = *on;
331900100108         ENDIF;
332000100108
332100100108       //?Imposto se manutenzione
332200100108         IF  IMK19fle = '2';
332300100108           V01tit       = tit_manut;
332400100108           Manutenzione = *on;
332500100108         ENDIF;
332600100519
332700100519         //?Abilito F16=Annullamento solo in manutenzione
332800100519         F16Attivo = Manutenzione;
332900100108
333000100108       //?Aggancio dati attività
333100100108           chain(n) (imk19tat:imk19atn:imk19atnp) tiatc01l;
333200100108
333300100108       //?Non trovo vado a fine
333400100108         IF  not %Found(tiatc01l);
333500100108           $ErrGrave   = *on;
333600100108           ErrMessage  = *on;
333700100108           ErrGenerico = *on;
333800100108           V1Dmsg = 'Dati attività non trovati';
333900100108           leavesr;
334000100108         ENDIF;
334100100108
334200100108       //?Ricerco descrizione causale
334300100108         clear dCCO;
334400100108         clear TIBS02ds;
334500100108         T02mod = 'C';
334600100108         T02cod = 'CCO';
334700100108         T02ke1 = atccad;
334800100108         T02sif = KNSIF;
334900100108         TNTBE_RicercaControllo  (kpjba : tibs02ds);
335000100108         IF  T02err = *blanks;
335100100108           dCCO = t02uni;
335200100108         ENDIF;
335300100108
335400100108       //?Imposto titolo alto
335500100108         clear wtitolo;
335600100108         wtitolo = §TTAtit;
335700100113         wtitolo = %trim(wtitolo) + '   ' + atccad + ' - ' + §CCOdes;
335800100108         clear wpos;
335900100108         wpos = (%len(v04tit) - %len(wtitolo)) / 2;
336000100108         %subst(v04tit:wpos:%len(wtitolo)) = wtitolo;
336100100108
336200100108       //?Verifico se parziale o tutta la giornata
336300100108         IF  atccad = 'FT';
336400100108           GiornoTotale = *on;
336500100108         ENDIF;
336600100108
336700100108       //?Data attività
336800100108         IF  atcdad > 0;
336900100108           data_eur = %date(atcdad:*iso);
337000100108           V04dad   = %dec(data_eur);
337100100108           w04dad   = atcdad;
337200100108         ENDIF;
337300100108
337400100108       //?Commerciale
337500100108         V04cmm = %editc(atccmm : 'X');
337600130809         clear  V04cmmD;
337700130809         chain (ATCcmm) AZCMM000;
337800130809         IF   %found(AZCMM01L);
337900130809           V04cmmd = CMMdes;
338000100108         ENDIF;
338100100108
338200100108       //?Impegno
338300100108         V04oii = atcoii;
338400100108         V04ofi = atcofi;
338500100108
338600100108       //?Note
338700100108         clear trmk20ds ;
338800100108         IMK20tla = 'L';
338900100108         IMK20flm = 'R';
339000100108         IMK20tat = IMK19tat ;
339100100108         IMK20atn = IMK19atn ;
339200100108         IMK20atnp= IMK19atnp -1;
339300100108         trmk20r ( kpjba : trmk20ds );
339400100108         V04no1o = emk20no1;
339500100108         V04no2o = emk20no2;
339600100108         piunote = (omk20piu = '+');
339700100108
339800100108       ENDSR;
339900100108
340000100108       //--------------------------------------------------------------
340100100108       //?Controllo videata D04.
340200100108       //--------------------------------------------------------------
340300100108       BEGSR CtrD04;
340400100108
340500100108         WindDspF = IndDspF;
340600100108         reset WindDspFb;
340700100108         IndDspF  = WindDspF;
340800100108
340900100108       //?Data attività
341000100108         IF  V04dad = 0;
341100100108           ErrMessage  = *on;
341200100108           ErrGenerico = *on;
341300100108           PosCurScd02 = *on;
341400100108           V1Dmsg      = $Msg(03);
341500100108           leavesr;
341600100108         ENDIF;
341700100108
341800100108         clear wlbdat;
341900100108         g02dat = V04dad;
342000100108         xsrda8(wlbdat);
342100100108         IF  g02err = '1';
342200100108           ErrMessage  = *on;
342300100108           ErrGenerico = *on;
342400100108           PosCurScd02 = *on;
342500100108           V1Dmsg      = $Msg(03);
342600100108           leavesr;
342700100108         ENDIF;
342800100108
342900100108         V04dad = g02dat;
343000100108         W04dad = g02inv;
343100100118
343200100118       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
343300100118         IF  W04dad < 20000101 or W04dad > 20391231;
343400100118           ErrMessage  = *on;
343500100118           ErrGenerico = *on;
343600100118           PosCurScd02 = *on;
343700100118           V1Dmsg      = $Msg(03);
343800100118           leavesr;
343900100118         ENDIF;
344000100108
344100100108       //?Impegno solo se non è intera giornata
344200100108         IF  not GiornoTotale;
344300100108           IF  V04oii = 0;
344400100108             ErrMessage  = *on;
344500100108             ErrGenerico = *on;
344600100108             PosCurOii02 = *on;
344700100108             V1Dmsg      = $Msg(17);
344800100108             leavesr;
344900100108           ENDIF;
345000100108           sav02ora = %editc(V04oii:'X');
345100120105           IF  ora > 23 or minuti > 59;
345200100108             ErrMessage  = *on;
345300100108             ErrGenerico = *on;
345400100108             PosCurOii02 = *on;
345500100108             V1Dmsg      = $Msg(05);
345600100108             leavesr;
345700100108           ENDIF;
345800100108
345900100108           IF  V04ofi = 0 ;
346000100108             ErrMessage  = *on;
346100100108             ErrGenerico = *on;
346200100108             PosCurOfi02 = *on;
346300100108             V1Dmsg      = $Msg(17);
346400100108             leavesr;
346500100108           ENDIF;
346600100108           sav02ora = %editc(V04ofi:'X');
346700120105           IF  ora > 23 or minuti > 59;
346800100108             ErrMessage  = *on;
346900100108             ErrGenerico = *on;
347000100108             PosCurOfi02 = *on;
347100100108             V1Dmsg      = $Msg(05);
347200100108             leavesr;
347300100108           ENDIF;
347400100108
347500100108         //?controllo ora  inizio se congruente con fine impegno
347600100108           IF  V04oii > 0 and V04ofi = 0;
347700100108             ErrMessage  = *on;
347800100108             ErrGenerico = *on;
347900100108             PosCurOfi02 = *on;
348000100108             V1Dmsg      = $Msg(13);
348100100108             leavesr;
348200100108           ENDIF;
348300100108
348400100108         //?controllo ora  inizio se congruente con fine impegno
348500100108           IF  V04oii > 0 and V04ofi > 0 and V04oii > V04ofi ;
348600100108             ErrMessage  = *on;
348700100108             ErrGenerico = *on;
348800100108             PosCurOfi02 = *on;
348900100108             V1Dmsg      = $Msg(13);
349000100108             leavesr;
349100100108           ENDIF;
349200100108         ENDIF;
349300100108
349400100108       //?Codice commerciale
349500100108         IF  V04cmm = *blanks or V04cmm = *zeros;
349600100108           ErrMessage  = *on;
349700100108           ErrGenerico = *on;
349800100108           PosCurCmi02 = *on;
349900100108           V1Dmsg      = $Msg(02);
350000100108           leavesr;
350100100108         ENDIF;
350200100108
350300100108         IF  %scan('?' : V04cmm) > 0  ;
350400100108           ErrGenerico = *on;
350500100108           PosCurCmi02 = *on;
350600130809           clear  TRMK43ds;
350700130809           iMK43solU = 'S';
350800130809           iMK43fil  = DUTpou;
350900130809           TRMK43R (kpjba : TRMK43ds);
351000130809           if  oMK43err = *off  and  oMK43fxx = *blank;
351100130809             V04cmm  = %editc( oMK43cmm : 'X' );
351200130809             V04cmmd = oMK43des;
351300130809           endif;
351400100108           leavesr;
351500100108         ENDIF;
351600100108
351700100108         IF  %check(digits:V04cmm) > 0;
351800100108           ErrMessage  = *on;
351900100108           ErrGenerico = *on;
352000100108           PosCurCmi02 = *on;
352100100108           V1Dmsg      = $Msg(02);
352200100108           leavesr;
352300100108         ENDIF;
352400100210
352500100210         //?utente abilitato al commerciale
352600100210         clear TNTAA1DS;
352700120810         ITAA1tipo = 'M';
352800120810         ITAA1caut = '§utegtc';
352900100210         ITAA1cmm  = %dec(V04cmm:7:0);
353000100519         ITAA1abi = 'RA';
353100100210         kpjbu     = tntaa1ds;
353200100210         tntaa1c (kpjba);
353300100210         TNTAA1DS = kpjbu;
353400100210         IF  OTAA1fabi = 'N';
353500100210           ErrMessage  = *on;
353600100210           ErrGenerico = *on;
353700100210           PosCurCmi02 = *on;
353800100210           V1Dmsg      = $Msg(02);
353900100210           leavesr;
354000100210         ENDIF;
354100100108
354200130809         CMMcod = %int(V04cmm);
354300130809         chain (CMMcod) AZCMM000;
354400130809         IF  not %found(AZCMM01L);
354500100108           ErrMessage  = *on;
354600100108           ErrGenerico = *on;
354700100108           PosCurCmi02 = *on;
354800100108           V1Dmsg      = $Msg(02);
354900100108           leavesr;
355000100108         ENDIF;
355100100504
355200130809         V04cmmd = CMMdes;
355300100210
355400100210       //?Commerciale  -  solo unificante
355500130809         IF  %dec(V04cmm:7:0) <> CMMuni;
355600100210           ErrMessage  = *on;
355700100210           ErrGenerico = *on;
355800100210           PosCurCmi02 = *on;
355900100210           V1Dmsg      = 'Commerciale errato. ' +
356000100210                         'Immettere solo commerciali UNIFICANTI!';
356100100210           leavesr;
356200100210         ENDIF;
356300100108
356400100108       ENDSR;
356500100108
356600100108       //--------------------------------------------------------------
356700100108       //?Gestione tasto funzionale F06 da videata D04.
356800100108       //?F06=Conferma
356900100108       //--------------------------------------------------------------
357000100108       BEGSR F06D04;
357100100108
357200100108       //?Registro le varizioni
357300100108         chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
357400100108         IF  %Found(tiatc01l);
357500100108           atcdad = w04dad ;
357600100108           IF  GiornoTotale;
357700100108             atchda = 090000;
357800100108           ELSE;
357900100108             atchda = V04oii * 100;
358000100108           ENDIF;
358100100108           atccmm = %dec(V04cmm:7:0);
358200100108           atcoii = V04oii;
358300100108           atcofi = V04ofi;
358400100108           update tiatc000;
358500100108         ENDIF;
358600100108
358700100108       //?Registro le note
358800100108         clear trmk20ds;
358900100108         IMK20tla = 'L';
359000100108         IMK20flm = 'C';
359100100215         IMK20cmt = '1';
359200100108         IMK20tat = atctat;
359300100108         IMK20atn = atcatn;
359400100108         IMK20atnp= atcatnp - 1;
359500100108         trmk20r ( kpjba : trmk20ds );
359600100108
359700100108       //?Comitto se richiesto e se non ci sono errori
359800100108         IF  IMK19cmt = 'S' and OMK19err = *blanks;
359900100108           commit;
360000100108         ENDIF;
360100100108
360200100108       //?Ritorno al programma chiamante
360300100108         $fine = *on;
360400100108
360500100108       ENDSR;
360600100112
360700100112       //--------------------------------------------------------------
360800100112       //?Gestione tasto funzionale F16 da videata D04.
360900100112       //?F16=Annulla
361000100112       //--------------------------------------------------------------
361100100112       BEGSR F16D04;
361200100112
361300121029       //?Se ci sono note legate all'attività da pulire le cancello
361400121029         exsr Del_ticpn;
361500100112
361600100112       //?Registro le varizioni
361700100112         chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
361800100112         IF  %Found(tiatc01l);
361900100112           delete tiatc000;
362000100112         ENDIF;
362100100112
362200100112       //?Comitto se richiesto e se non ci sono errori
362300100112         IF  IMK19cmt = 'S' and OMK19err = *blanks;
362400100112           commit;
362500100112         ENDIF;
362600100112
362700100112       //?Ritorno al programma chiamante
362800100112         $fine = *on;
362900100112
363000100112       ENDSR;
363100100121
363200100121       //--------------------------------------------------------------
363300100121       //?Gestione tasto funzionale F18 da videata D04
363400100121       //?F18=Note
363500100121       //--------------------------------------------------------------
363600100121       BEGSR F18D04;
363700100121
363800100121         clear trmk20ds;
363900100121       //?Se interrogazion o attività già chiusa
364000100121       //?richiamo in interrogazione
364100100121         IF  GiaFatto or Visualizza;
364200110510           IMK20tla  = 'L';
364300100121           IMK20flm = 'I';
364400100121         ENDIF;
364500100121         IMK20tat = atctat;
364600100121         IMK20atn = atcatn;
364700100121         Trmk20r ( kpjba : trmk20ds );
364800100929
364900100929       //?Se sono in manutenzione devo recuperare la note come prima
365000100929         IF  not GiaFatto and not Visualizza;
365100100929           IMK20tla  = 'L';
365200100929           IMK20flm  = 'R';
365300100929           IMK20tat  = IMK19tat ;
365400100929           IMK20atn  = IMK19atn ;
365500100929           IMK20atnp = IMK19atnp -1;
365600100929           Trmk20r ( kpjba : trmk20ds );
365700100929           v04no1o   =  emk20no1 ;
365800100929           v04no2o   =  emk20no2 ;
365900100929           piunote   = (omk20piu = '+');
366000100929         ENDIF;
366100100121
366200100121       ENDSR;
366300100224
366400100224       //--------------------------------------------------------------
366500100224       //?Gestione videata W05
366600100224       //--------------------------------------------------------------
366700100224       BEGSR GesW05;
366800100224
366900100224         // Inizializzazione videata
367000100224         clear w05ann ;
367100100224
367200100224         // verifico se c'è affiancamento  richiamo il trmk22 in modalità annullamento
367300100224         If Conaffianca = *on ;
367400100224            clear trmk22ds ;
367500100224            IMK22fcmt = '1';
367600100224            IMK22cmt = 'N';
367700100224            IMK22fmt = '4';
367800100224            IMK22fle = 'D';
367900100224            IMK22cac = v02cae;
368000100224            IMK22cmm  = %dec(v02com:7:0);
368100100224            IMK22dad  = w02scd;
368200100224            IMK22hda  = v02ora * 100;
368300100224            IMK22oii  = v02oii;
368400100224            IMK22ofi  = v02ofi;
368500100224            IMK22tat  = IMK19tat;
368600100224            IMK22atn  = IMK19atn;
368700100224            IMK22atnp = IMK19atnp;
368800100224
368900100224            trmk22r (kpjba : TRMK22ds);
369000100224
369100100224         // se ritorna errore lo visualizzo e faccio rollback
369200100224            IF  OMK22err <> *blanks;
369300100224                ErrMessage  = *on;
369400100224                ErrGenerico = *on;
369500100224                V1dmsg = OMK22msg;
369600100224                IF  IMK19cmt = 'S';
369700100224                    rolbk;
369800100224                ENDIF;
369900100224                leavesr;
370000100224            Endif;
370100100224
370200100224         // se F12 da gestione affiancamento rollback
370300100224            If  IMK19cmt = 'S' and OMK22F12 = 'S';
370400100224                rolbk;
370500100224         // Ritorno alla videata D02
370600100224                $Video = 'D2';
370700100224                leavesr;
370800100224            Endif;
370900100224
371000100224            Conaffianca = *off ;
371100100224         Endif ;
371200100224
371300100517         $fine5 = *off ;
371400100224         // Emissione videata
371500100517         dow $fine5 = *off ;
371600100224         exfmt MK19W05;
371700100224         errMessage   = *off;
371800100224         errGenerico  = *off;
371900100224         clear W05msg;
372000100224
372100100225         // se dato f6 conferma e risposto "SI"
372200100503           If   dsp_aid = c_F06 and w05ann = 'S' ;
372300100519         //?Se posso deletare
372400100519             IF $OkDel;
372500100224                     delete (imk19tat:imk19atn:imk19atnp) tiatc01l ;
372600100224         // se l'attività era un progressivo uno vado a cancellare le note
372700100224         // con progressivo 0
372800100224                     If imk19atnp = 1 ;
372900100224                        clear trmk20ds ;
373000100224                        imk20tla = 'L';
373100100224                        imk20flm = 'D';
373200100224                        imk20cpo = cpocpo ;
373300100224                        imk20rsc = v02cpd ;
373400100224                        imk20tat = imk19tat ;
373500100224                        imk20atn = imk19atn ;
373600100224                        imk20atnp= imk19atnp -1;
373700100224                        Trmk20r ( kpjba : trmk20ds );
373800100224                     endif ;
373900100224         // comitto se richiesto e se non ci sono errori
374000100224                IF  IMK19cmt = 'S';
374100100224                    commit ;
374200110412         // Verifico la categoria del potenziale
374300110412                    clear trmk05ds;
374400110412                    IMK05cpo = CPOcpo;
374500110412                    trmk05r (kpjba : TRMK05DS);
374600110412         //?se categoria variata aggiorno potenziale
374700110412                    IF  OMK05err = *blanks and OMK05cat <> CPOfls;
374800110412                        chain(e) CPOcpo TNCPO01L;
374900110412                        IF  not %error and %found(TNCPO01L);
375000110412                            CPOfls = OMK05cat;
375100110412                            update TNCPO000;
375200110412                        ENDIF;
375300110412                    ENDIF;
375400100224                    $Fine = *on;
375500100517                    leavesr ;
375600100224                ENDIF;
375700101014                $fine5 = *on;
375800101014                $Fine = *on;
375900100519              ELSE;
376000100520         //?CHIODO
376100100520         //?Non posso deletare  quindi chiudo attività con causale 'AA'
376200100520                chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
376300100520                IF  %Found(TIATC01L);
376400100520                  ATCdco = %dec(%date());
376500100520                  ATChco = %dec(%time());
376600100520                  IF  IMK19com > *zeros;
376700100520                    ATCcco = IMK19com;
376800100520                  ELSE;
376900100520                    ATCcco = %dec(V02cmi:7:0);
377000100520                  ENDIF;
377100100520                  ATCpru = knmus;
377200100520                  ATCcac = 'AA';
377300100520                  update TIATC000;
377400100520                ENDIF;
377500100520
377600100520               // se ci sono delle note le registro
377700100520               if v02no1 <> *blanks  or v02no2 <> *blanks ;
377800100520                clear trmk20ds ;
377900100520                imk20tla = 'L';
378000100520                imk20flm = 'C';
378100100520                imk20fno = 'S';
378200100520                imk20cmt = '1';
378300100520                imk20cpo = cpocpo ;
378400100520                If v02cli > 0 ;
378500100520                  imk20ksc = v02cli ;
378600100520                endif ;
378700100520                imk20tat = atctat ;
378800100520                imk20atn = atcatn ;
378900100520                imk20atnp= atcatnp;
379000100520                imk20rsc = v02cpd ;
379100100520                emk20no1 = v02no1 ;
379200100520                emk20no2 = v02no2 ;
379300100520                emk20dim = atcdco ;
379400100520                emk20him = atchco ;
379500100520                Trmk20r ( kpjba : trmk20ds );
379600100520               endif ;
379700100520               IF  IMK19cmt = 'S';
379800100520                   commit ;
379900110412         // Verifico la categoria del potenziale
380000110412                    clear trmk05ds;
380100110412                    IMK05cpo = CPOcpo;
380200110412                    trmk05r (kpjba : TRMK05DS);
380300110412         //?se categoria variata aggiorno potenziale
380400110412                    IF  OMK05err = *blanks and OMK05cat <> CPOfls;
380500110412                        chain(e) CPOcpo TNCPO01L;
380600110412                        IF  not %error and %found(TNCPO01L);
380700110412                            CPOfls = OMK05cat;
380800110412                            update TNCPO000;
380900110412                        ENDIF;
381000110412                    ENDIF;
381100100520                   $Fine = *on;
381200100520                   leavesr ;
381300100520               ENDIF;
381400101014                $fine5 = *on;
381500101014                $Fine = *on;
381600100520
381700100519              ENDIF;
381800100224           endif;
381900100519
382000100519
382100100225         // se dato f6 conferma e risposto "NO"
382200100503           If   dsp_aid = c_F06 and w05ann = 'N' ;
382300100225         // bollback
382400100225                IF  IMK19cmt = 'S';
382500100225                    rolbk ;
382600100225                    $Fine = *on;
382700100517                    leavesr ;
382800100225                ENDIF;
382900101014                $fine5 = *on;
383000101014                $Fine = *on;
383100100225           endif;
383200100519
383300100517         enddo;
383400100224
383500100224       ENDSR;
383600100614
383700100614       //--------------------------------------------------------------
383800100614       //?Gestione videata W08
383900100614       //--------------------------------------------------------------
384000100614       BEGSR GesW08;
384100100614
384200100614         //?Inizializzazione videata
384300100614         w08sino = 'N';
384400100614         $Fine8 = *off;
384500100614
384600100614         //?Emissione videata
384700100614         DOW  not $Fine8;
384800100614           exfmt MK19W08;
384900100614
385000100614         //?F06-Conferma
385100100614           IF  dsp_aid = c_F06;
385200100614         //?Perdo modifiche
385300100614             IF  W08sino = 'S';
385400100614               $Fine8 = *on;
385500100614               $Video = 'D2';
385600100614               $Modaffianca = *off;
385700100614               exsr F12D02;
385800100614             ENDIF;
385900100614         //?Non Perdo modifiche
386000100614             IF  W08sino = 'N';
386100100614               $Fine8 = *on;
386200100614               $Video = 'D2';
386300100614               $inzd02 = *on ;
386400100614             ENDIF;
386500100614           ENDIF;
386600100614         ENDDO;
386700100614
386800100614       ENDSR;
386900110225
387000110225       //--------------------------------------------------------------
387100110225       //?Gestione videata W09
387200110225       //--------------------------------------------------------------
387300110225       BEGSR GesW09;
387400110225
387500110225         //?Inizializzazione videata
387600110225         $Fine9 = *off;
387700110225
387800110225         //?Emissione videata
387900110225         DOW  not $Fine9;
388000110225           write MK19T01;
388100110225           write MK19D02;
388200110225           exfmt MK19W09;
388300110225
388400110225         //?Annullo l'attività e torno al TRMK18R
388500110301           IF  *inkf;
388600110225             $Fine9 = *on;
388700110225             $Fine  = *on ;
388800110225             chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
388900110225             IF  %Found(TIATC01L);
389000110301               delete TIATC000;
389100110412         // se l'attività era un progressivo uno vado a cancellare le note
389200110412         // con progressivo 0
389300110412                     If imk19atnp = 1 ;
389400110412                        clear trmk20ds ;
389500110412                        imk20tla = 'L';
389600110412                        imk20flm = 'D';
389700110412                        imk20cpo = cpocpo ;
389800110412                        imk20rsc = v02cpd ;
389900110412                        imk20tat = imk19tat ;
390000110412                        imk20atn = imk19atn ;
390100110412                        imk20atnp= imk19atnp -1;
390200110412                        Trmk20r ( kpjba : trmk20ds );
390300110412                     endif ;
390400110412         // comitto se richiesto e se non ci sono errori
390500110412                IF  IMK19cmt = 'S';
390600110412                    commit ;
390700110412                    clear trmk05ds;
390800110412                    IMK05cpo = CPOcpo;
390900110412                    trmk05r (kpjba : TRMK05DS);
391000110412         //?se categoria variata aggiorno potenziale
391100110412                    IF  OMK05err = *blanks and OMK05cat <> CPOfls;
391200110412                        chain(e) CPOcpo TNCPO01L;
391300110412                        IF  not %error and %found(TNCPO01L);
391400110412                            CPOfls = OMK05cat;
391500110412                            update TNCPO000;
391600110412                        ENDIF;
391700110412                    ENDIF;
391800110412                ENDIF;
391900110225             ENDIF;
392000110225           ENDIF;
392100110225         ENDDO;
392200110225
392300110225       ENDSR;
392400100224
392500110527       //--------------------------------------------------------------
392600110527       //?Gestione videata W10
392700110527       //--------------------------------------------------------------
392800110527       BEGSR GesW10;
392900110527
393000110527         //?Emissione videata
393100110530           write MK19T01;
393200110530           write MK19D02;
393300110527           exfmt MK19W10;
393400110530           $Video = 'D2';
393500110527
393600110527       ENDSR;
393700090611       //--------------------------------------------------------------
393800090611       //?Gestione tasto funzionale F12 da videata W03
393900090611       //?F12=Ritorno
394000090611       //--------------------------------------------------------------
394100090611       BEGSR F12W03;
394200090611
394300090611         // Ritorno alla videata D02
394400090611            $Video = 'D2';
394500100118
394600100202         //?Pulisco campo comodo n. trattativa e commerciale
394700100118         clear newnrv;
394800100202         clear w03com;
394900090611
395000090611       ENDSR;
395100090611
395200090610       //--------------------------------------------------------------
395300090610       //?Gestione tasto funzionale F12 da videata D02
395400090610       //?F12=Ritorno
395500090610       //--------------------------------------------------------------
395600090610       BEGSR F12D02;
395700100614
395800100614       //?Se modificato affiancamento devo avvisare che le modifiche andranno perse
395900100614         IF  $ModAffianca;
396000100614           $Video = 'W8';
396100100614           leavesr;
396200100614         ENDIF;
396300090610
396400091127         // verifico se si tratta della prima telefonata da cliente o potenziale
396500091127         // ritorno ai subfile di selezione
396600091127
396700091127        If imk19fle = 'C' ;
396800100802        // exsr Primo_cli ;
396900100802        // leavesr ;
397000091127        endif ;
397100091127
397200091127        If imk19fle = 'P' ;
397300100409           W_mk01f12 = '1' ;
397400100409        // exsr Primo_po  ;
397500100409        // leavesr ;
397600091127        endif ;
397700090611         // Ritorno al programma chiamante
397800100422
397900100422         omk19err = '1' ;
398000100422
398100090611         $fine = *on ;
398200090611
398300090610       ENDSR;
398400090610
398500090610
398600090610       //--------------------------------------------------------------
398700090610       //?Gestione tasto funzionale F14 da videata D02
398800090610       //?F14=Informazioni Commerciali
398900090610       //--------------------------------------------------------------
399000090610       BEGSR F14D02;
399100090610
399200090610         // Richiamo il pgm informazioni Commerciali
399300090610         clear     trmk50ds   ;
399400090610         i50cpo=cpocpo ;
399500120313         If imk19fle <> '5' ;
399600100329            i50mod = 'G' ;
399700100329         else ;
399800100329            i50mod=  'I' ;
399900100329         endif ;
400000090610         i50rag=v02cpd   ;
400100090610         i50pgm='TRMK19R' ;
400200120124
400300120124       //?Se il richiamo delle INFO è stato pilotato da attività 72 devo passare
400400120124       //?il campo I50atb = 0 per obbligare l'azzeramento della % affido a BRT
400500120124         IF  info_cac = '72';
400600120208           I50atb = '0';
400700120125         //?questo lo faccio però solo se sto bloccando l'ultimo cliente della catena
400800120125         //?se ne ho ancora dei NON bloccati non passo il dato al trmk50r
400900120125           setll cpocpo CNACO16L;
401000120125           reade cpocpo CNACO16L;
401100120125           DOW not %eof(CNACO16L);
401200120125             IF  ACOabl = *blanks;
401300120125               clear I50atb;
401400120125               leave;
401500120125             ENDIF;
401600120125             reade cpocpo CNACO16L;
401700120125           ENDDO;
401800120125
401900120124         ENDIF;
402000120524
402100120524       //?Se il richiamo delle INFO è stato pilotato da attività 41 con esito positivo
402200120524       //?su trattativa di tipo I=Incremento devo impostare
402300120524       //?il campo I50atb = % per obbligare l'aumento della % affido a BRT se = 0
402400120524         IF  info_cac = '41' and §VISesito = 'P' and VIStpv = 'I';
402500120524           I50atb = '%';
402600120524         ENDIF;
402700140929
402800140929        // Se chiusa trattativa chiedo INFO obbligatorie indipendentemente dalla causale
402900140929        // e dal tipo trattativa come richiesto dal presidio vendite
403000140929         IF  wchiudo_tr = 'S' ;
403100140929             i50obl = 'S';
403200140929         Endif ;
403300120124
403400090610         TRMK50R  (kpjba:trmk50ds) ;
403500140929
403600140929         clear Wchiudo_tr ;
403700090610
403800090610       ENDSR;
403900090610
404000090610       //--------------------------------------------------------------
404100090610       //?Gestione tasto funzionale F18 da videata D02
404200090610       //?F18=Note
404300090610       //--------------------------------------------------------------
404400090610       BEGSR F18D02;
404500090610
404600090610         // Richiamo il pgm gestione note TRMK20R
404700090610         clear trmk20ds ;
404800090610         if giafatto = *on or visualizza = *on  ;
404900090610            imk20flm = 'I';
405000110510            imk20tla = 'L';
405100090610         else ;
405200090610            imk20flm = 'M';
405300090610         endif ;
405400091105         // se valorizzato il cliente passo il cliente
405500091105         If v02cli > 0 ;
405600091105            imk20ksc = v02cli ;
405700091105            imk20rsc = v02clid ;
405800091105         else ;
405900091105            imk20cpo = cpocpo ;
406000091105            imk20rsc = v02cpd ;
406100091027         endif ;
406200100318         If imk19fle = '2' ;
406300100318            imk20flm = ' ';
406400100318            imk20tat = imk19tat ;
406500100318            imk20atn = imk19atn ;
406600100318            imk20atnp= imk19atnp -1;
406700100318         else;
406800100318            emk20no1 = v02no1 ;
406900100318            emk20no2 = v02no2 ;
407000100318         endif ;
407100090616         Trmk20r ( kpjba : trmk20ds );
407200100318         // se dato f18 mentre sono in manutenzione devo valorizzare le note parte alta
407300100318         // del video ....
407400100318         If imk19fle = '2' ;
407500100318                clear trmk20ds ;
407600100318                imk20tla = 'L';
407700100318                imk20flm = 'R';
407800100318                imk20cpo = cpocpo ;
407900100318                imk20rsc = v02cpd ;
408000100318                imk20tat = imk19tat ;
408100100318                imk20atn = imk19atn ;
408200100318                imk20atnp= imk19atnp -1;
408300100318                Trmk20r ( kpjba : trmk20ds );
408400100318                v02no1o =  emk20no1 ;
408500100318                v02no2o =  emk20no2 ;
408600100318                piunote = (omk20piu = '+');
408700100318
408800100318         else;
408900100318            v02no1 =  emk20no1 ;
409000100318            v02no2 =  emk20no2 ;
409100100318            altrenote = (omk20piu = '+');
409200100318         endif ;
409300090610
409400090610       ENDSR;
409500090610
409600090610       //--------------------------------------------------------------
409700090610       //?Gestione tasto funzionale F01 da videata D02
409800091021       //?F01=Anagrafica
409900090610       //--------------------------------------------------------------
410000090610       BEGSR F01D02;
410100090610
410200091021       If cliente = *off ;
410300091021         // Richiamo il pgm gestione  potenziali se non c'è il cliente valorizzato
410400091104         clear trmk01ds   ;
410500100409         clear kpjbu ;
410600091027         // se sono in visualizzazione imposto interrogazione
410700091027         if imk19fle <> '5' ;
410800091104            mk01k10 ='V'   ;
410900091027         else ;
411000091104            mk01k10 ='N'   ;
411100091027         endif ;
411200091104         mk01cpo =cpocpo ;
411300100409         TRMK02c  (kpjba:Trmk01ds);
411400090610         // Se al ritorno dal pgm c'è errore richiamo il pgm dei potenziali in interrogazione
411500091104         If  mk01err <> ' ' ;
411600091104             clear trmk01ds   ;
411700091104             mk01k10 ='N'   ;
411800091104             mk01cpo =cpocpo ;
411900100409             TRMK02c  (kpjba:Trmk01ds);
412000090610         endif ;
412100091021       else ;
412200091021         // Richiamo il pgm gestione cliente
412300091021
412400091021          clear Tnta60ds ;
412500091021          ta60flg = 'I';
412600091105          ta60nrv = v02cli ;
412700091105          ta60int = 'S';
412800091021
412900110428          tnta60r (kpjba:tnta60ds) ;
413000091021
413100091021       // gestisco i dati di ritorno
413200091021
413300091021          if ta60err <> *blanks ;
413400091021             errMessage  = *on;
413500091021             errGenerico = *on;
413600091021             V1Dmsg = ta60msg;
413700091021             leavesr;
413800091021          EndIf ;
413900091021       endif ;
414000090610
414100091022         // recupero i dati del potenziale/clienti
414200091021         exsr AnagD02 ;
414300091022         // recupero i dati del potenziale/clienti  responsabili trasporti
414400091022         exsr RespD02 ;
414500091022
414600090610
414700090610       ENDSR;
414800150126
414900150126       //--------------------------------------------------------------
415000150126       //?Gestione tasto funzionale F04 da videata D02                 ?
415100150126       //?F4=Campagna                                                  ?
415200150126       //--------------------------------------------------------------
415300150126       BEGSR F04D02;
415400150126
415500150126         reset  TRKC07ds;
415600150126         //IKC07ric = '5';         ?(già così)?
415700150126         iKC07ncm = oKC73nc1;
415800150126         iKC07ksu = oKC73ksu;
415900150126
416000150126         trkc07r ( kpjba : TRKC07ds );
416100150126
416200150126       ENDSR;
416300100928
416400100928       //--------------------------------------------------------------
416500100928       //?Gestione tasto funzionale F05 da videata D02
416600100928       //?F05=Attività
416700100928       //--------------------------------------------------------------
416800100928       BEGSR F05D02;
416900100928
417000100928         clear TRMK21DS;
417100100928         I21mod = '9';
417200100928         I21cpo = %dec(V02cpo:11:0);
417300100928         I21rsc = V02cpd;
417400100928         IF  v02cli > 0;
417500100928           I21ksc = V02cli;
417600100928           I21rsc = V02clid;
417700100928         ENDIF;
417800100928         kpjbu = TRMK21DS;
417900100928         trmk21r (kpjba);
418000100928         TRMK21DS = kpjbu;
418100100928         IF  O21err <> *blanks;
418200100928           ErrGenerico = *on;
418300100928           ErrMessage  = *on;
418400100928           V1Dmsg      = O21msg;
418500100928           leavesr;
418600100928         ENDIF;
418700100928
418800100928       ENDSR;
418900090701
419000091026       //--------------------------------------------------------------
419100091026       //?Gestione tasto funzionale F09 da videata D02
419200091210       //?F09=Trattativa
419300091026       //--------------------------------------------------------------
419400091026       BEGSR F09D02;
419500091026
419600100202         // Richiamo la trattativa
419700091026             clear tnta88ds ;
419800091026             ita88cmt = 'N';
419900091118             ita88fle = 'G' ;
420000100202             IF  visualizza;
420100100202             ita88fle = '5' ;
420200100202             ENDIF;
420300091026             ita88fpr = 'A' ;
420400091026             ita88nrv = v02vis ;
420500100225             ita88com = imk19com ;
420600100225             ita88comd= imk19comd ;
420700091026             Tnta88r ( kpjba : tnta88ds );
420800091026
420900091026       ENDSR;
421000091026
421100091026       //--------------------------------------------------------------
421200091026       //?Gestione tasto funzionale F11 da videata D02
421300091026       //?F11=Agenda
421400091026       //--------------------------------------------------------------
421500091026       BEGSR F11D02;
421600091026
421700091026         // Richiamo il pgm visualizzazione agenda
421800091027
421900091027            clear trmk82ds ;
422000100120            imk82rich = 'A' ;
422100091027         // definisco il commerciale da passare
422200091027
422300091027         // vince il commerciale che sta eseguendo l'attività dopo viene
422400091027         // il commerciale che dovrebbe eseguire l'attività  e per ultimo
422500091027         // il commerciale del cliente / potenziale
422600091027
422700091027            select ;
422800100329            when v02com <> *blanks ;
422900100329                imk82cage = %dec(v02com:7:0) ;
423000100329            when v02cmi <> *blanks ;
423100100329                imk82cage = %dec(v02cmi:7:0) ;
423200091111            when v02cmp <> *zeros  ;
423300091111                imk82cage = v02cmp ;
423400091027            endsl ;
423500091027
423600091027            kpjbu = trmk82ds ;
423700091027
423800091027            Trmk82r (kpjba);
423900091026
424000091026       ENDSR;
424100091026
424200091027       //--------------------------------------------------------------
424300091027       //?Gestione tasto funzionale F11 da videata W03
424400091027       //?F11=Agenda
424500091027       //--------------------------------------------------------------
424600091027       BEGSR F11W03;
424700091027
424800091027         // Richiamo il pgm visualizzazione agenda
424900091027
425000091027            clear trmk82ds ;
425100100120            imk82rich = 'A' ;
425200091027         // definisco il commerciale da passare
425300091027
425400091027         // vince uno dei due/tre commerciali valorizzati nella window
425500091027         // il commerciale che sta eseguendo l'attività dopo viene
425600091027         // il commerciale che dovrebbe eseguire l'attività  e per ultimo
425700091027         // il commerciale del cliente / potenziale
425800091027
425900091027            select ;
426000091027            when w03com <> *blanks ;
426100091111                imk82cage = %dec(w03com:7:0) ;
426200091027            when v02cmi <> *blanks ;
426300091111                imk82cage = %dec(v02cmi:7:0) ;
426400091027            when v02cmp <> *zeros  ;
426500091111                imk82cage = v02cmp ;
426600091027            endsl ;
426700091027
426800091027            // se data scadenza valorizzata la passo nella DS
426900091027            If w03dsc > 0 ;
427000091111                imk82data = ww03dsc ;
427100091027            endif ;
427200091027
427300091027            kpjbu = trmk82ds ;
427400091027
427500091027            Trmk82r (kpjba);
427600091027
427700091027       ENDSR;
427800100224
427900100224       //--------------------------------------------------------------
428000100224       //?Gestione tasto funzionale F16 da videata D02
428100100224       //?F16=Annullamento
428200100224       //--------------------------------------------------------------
428300100224       BEGSR F16D02;
428400100224
428500100224         // emetto una windows per la richiesta di conferma annullamento
428600100224          $video = 'W5';
428700100224
428800100224       ENDSR;
428900100224
429000091210       //--------------------------------------------------------------
429100091210       //?Gestione tasto funzionale F20 da videata D02
429200091210       //?F20=Affiancamento
429300091210       //--------------------------------------------------------------
429400091210       BEGSR F20D02;
429500091210
429600091210         clear TRMK22ds;
429700091210         IMK22fmt = '4';
429800091210
429900091210         SELECT;
430000091210           WHEN  Manutenzione;
430100091215             IMK22fcmt = '1';
430200091215             IMK22cmt = 'N';
430300091210             IMK22fle = '2';
430400091210           WHEN  Visualizza;
430500091210             IMK22fle = '5';
430600091210         ENDSL;
430700091210
430800091210         IMK22cmm  = %dec(v02cmi:7:0);
430900091211         IMK22dad  = w02scd;
431000091210         IMK22hda  = v02sco * 100;
431100091210         IMK22oii  = v02oii;
431200091210         IMK22ofi  = v02ofi;
431300091216         IMK22cac  = v02cae;
431400100114         IMK22esi  = v02fatto;
431500091210         IMK22tat  = IMK19tat;
431600091210         IMK22atn  = IMK19atn;
431700091210         IMK22atnp = IMK19atnp;
431800091210
431900091210         trmk22r (kpjba : TRMK22ds);
432000091210
432100091210         //?Se ritorna errore lo visualizzo
432200091210         IF  OMK22err <> *blanks;
432300091210           ErrMessage  = *on;
432400091210           ErrGenerico = *on;
432500091210           V1Dmsg      = OMK22msg;
432600091210         ENDIF;
432700100614
432800100614         //?Salvo che ho fatto una modifica all'affiancamento
432900100614         //?in modo da avvisare l'utente se fa F12 o F3 che le modifiche
433000100614         //?appena fatte andranno perse
433100100614         IF  OMK22f12 = *blanks;
433200100614           $ModAffianca = *on;
433300100614         ENDIF;
433400100224
433500100224        // Verifico se inseriti affiancamenti
433600100224           k_atlatl  = IMK19atn;
433700100224           k_atlatlp = IMK19atnp;
433800100224           chain  %kds(K02tiatl) TIATL02L;
433900100224           IF  %found(Tiatl02l);
434000100224             ConAffianca = *on;
434100100422           ELSE ;
434200100422             ConAffianca = *off ;
434300100224           ENDIF;
434400091210
434500091210       ENDSR;
434600091027
434700090701       //--------------------------------------------------------------
434800100920       //?Gestione tasto funzionale F21 da videata D02 x richiamo TRMK24
434900100920       //?F21=Invio materiale illustrativo
435000090701       //--------------------------------------------------------------
435100090701       BEGSR F21D02;
435200090701
435300100921         // inizializzo alcuni campi per invio lettera
435400100921          clear trmk24ds ;
435500100921          If v02cli > 0 ;
435600100921               Imk24Tde = 'C' ;
435700100921               Imk24Cde = v02cli ;
435800100921          else ;
435900100921               Imk24Tde = 'P' ;
436000100921               Imk24Cde = %dec(v02cpo:11:0) ;
436100100921          endif ;
436200100921
436300100921        // recupero i dati del responsabile trasporti
436400100921          exsr respd02 ;
436500100921          Imk24rst = vh2rtr ;
436600100921        // imposto indirizzo mail immesso nella window
436700100921          Imk24mai = v02mrt ;
436800100921
436900100921         // imposto il commerciale che firmerà la mail, il comm. del potenziale se > 0
437000100921          If v02cmp > 0 ;
437100100921               Imk24cmm = v02cmp ;
437200100921          else ;
437300100921               Imk24cmm = %dec(v02com:7:0);
437400100921          endif ;
437500100921
437600100921          trmk24r ( kpjba : TRMK24ds );
437700100921
437800100921       // Scrivo nota di invio mail se inviata
437900100921          if omk24fxx = '06' ;
438000100921             clear ticpn000 ;
438100100921
438200100921             cpncpo=cpocpo;
438300100921             cpnksc=v02cli;
438400100921             data_iso = (%date()) ;
438500100921             cpndim = %dec(data_iso) ;
438600100921             cpnhim = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
438700100921             cpnsns='S'   ;
438800100921             cpnpos=dutpou;
438900100921             cpnpru=knmus;
439000100921             cpnpno = 1;
439100100921         // imposto il campo nota
439200100921
439300100921             destina = %trim(Omk24mai) ;
439400100921
439500100921             cpnnot='Inviato testo lettera ' + Omk24tlt + ' a nome di '
439600100921                  + %editc(Omk24cmm:'X')  + ' ' + %trim(Omk24cmd) + ' a ' ;
439700100921             write ticpn000;
439800100921             cpnpno+=1;
439900100921             cpnnot=%trim(destina) ;
440000100921             write ticpn000;
440100100924             IF  IMK19cmt = 'S' and OMK19err = *blanks;
440200100924                 commit;
440300100924             ENDIF;
440400100921
440500100921           endif ;
440600090701
440700090701       ENDSR;
440800090701
440900090610       //--------------------------------------------------------------
441000090610       //?Gestione tasto funzionale F02 da videata D02
441100090610       //?F02=Contatti
441200090610       //--------------------------------------------------------------
441300090610       BEGSR F02D02;
441400090610
441500100202         // Richiamo i contatti
441600090610         clear tnta12ds   ;
441700100202         IF  Visualizza;
441800100202           ta12ric = 'V';
441900100202         ENDIF;
442000091118         If v02cli > *zeros ;
442100091118            ta12apl='C'   ;
442200091118            ta12ksc=v02cli ;
442300091118            ta12rag=v02clid  ;
442400091118         else ;
442500091118         If v02vis > *zeros ;
442600091118            ta12apl='T'   ;
442700091118            ta12nrv=v02vis ;
442800091118         else ;
442900091118            ta12apl='P'   ;
443000091118            ta12pot=cpocpo ;
443100091118            ta12rag=v02cpd   ;
443200091105         endif ;
443300091118         endif ;
443400090610         TNTA12R  (kpjba:TNTA12ds);
443500090610         If ta12err <> ' ';
443600090610            errMessage  = *on;
443700090610            errGenerico = *on;
443800090610            V1Dmsg = ta12msg ;
443900090610            leavesr;
444000090610         endif;
444100090610
444200090610         // recupero i dati del potenziale responsabili trasporti
444300090610         exsr RespD02 ;
444400090610
444500090610       ENDSR;
444600090610
444700090610
444800090610       //--------------------------------------------------------------
444900090610       //?Gestione tasto funzionale F06 da videata D02
445000090610       //?F06=Conferma
445100090610       //--------------------------------------------------------------
445200090610       BEGSR F06D02;
445300090610
445400090610        // registro i dati variati sul file
445500090611            Chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
445600090610            If %found(tiatc01l) ;
445700090610               atcdad   = W02scd  ;
445800090610               atchda = v02sco * 100 ;
445900090610               atccmm  = %dec(v02cmi:7:0);
446000090804               if  atctat = 'A' ;
446100090804                   atcoii = V02oii ;
446200090804                   atcofi = V02ofi ;
446300090804               endif ;
446400090610               update tiatc000  ;
446500090610            endif ;
446600100409
446700100409       // Se appuntamento con affiancamento richiamo la gestione dell'affiancamento
446800100614       //?solo se non ero già andata in manutenzione e dato F6
446900111102       // oppure è già andato in manutenzione ma sono stati modificati i dati dell'appuntamento
447000111102         If atctat = 'A' and ConAffianca = *on and (not $ModAffianca or
447100111102         ($ModAffianca and ((sav_atcdad <> 0 and  sav_atcdad <> atcdad ) or
447200111102         (sav_atchda <> 0 and  sav_atchda <> atchda ) or
447300111102         (sav_atcoii <> 0 and  sav_atcoii <> atcoii ) or
447400111102         (sav_atcofi <> 0 and  sav_atcofi <> atcofi ) or
447500111102         (sav_atccmm <> 0 and  sav_atccmm <> atccmm ))));
447600100412           exsr F20d02 ;
447700100409           IF  OMK22f12 = 'S' or OMK22err <> *blanks;
447800100412               IF  IMK19cmt = 'S' ;
447900100412                   rolbk ;
448000100412               ENDIF;
448100100409             leavesr;
448200100409           ENDIF;
448300100409         ENDIF ;
448400091218
448500091218        //?Se è stata modificata la data dell'appuntamento devo scrivere una nota
448600091218         IF  sav_atcdad <> 0 and sav_atcdad <> atcdad and atctat = 'A';
448700091218           data_eur = %date(sav_atcdad:*iso);
448800091218           wdata    = %dec(data_eur);
448900091218           clear trmk20ds;
449000091218           imk20tla = 'L';
449100091218           imk20flm = 'C';
449200091218           imk20fno = 'S';
449300100215           IMK20cmt = '1';
449400091218           imk20cpo = cpocpo;
449500091218           IF  v02cli > 0;
449600091218             imk20ksc = v02cli;
449700091218           ENDIF;
449800091218            imk20tat = atctat;
449900091218            imk20atn = atcatn;
450000091218            imk20atnp= atcatnp - 1;
450100091218            imk20nrv = atcnrv;
450200091222            emk20no1 = 'Data appuntamento variata, precedente: ' +
450300091218                       %editc(wdata:'Y');
450400091218            Trmk20r ( kpjba : trmk20ds );
450500091218
450600091218         ENDIF;
450700100409
450800100915       // Se è stato richiesta la conferma con mail richiamo il pgm TRMK23R1
450900100419           If   dsp_aid = c_F08 ;
451000100915        // richiamo con tipo richiamo blank in quanto in questa fase deve fare sia la richiesta
451100100915        // della mail che inviarla
451200100915                clear  TRMK23ds1;
451300100419                exsr Ric_Trmk23 ;
451400100419           Endif ;
451500100419
451600100304       //?Comitto se richiesto e se non ci sono errori
451700100304         IF  IMK19cmt = 'S' and OMK19err = *blanks;
451800100304           commit;
451900100304         ENDIF;
452000090610
452100100329         $Fine   = *on ;
452200100329
452300090610       ENDSR;
452400090610
452500090610       //--------------------------------------------------------------
452600090610       //?Gestione tasto funzionale F06 da videata D02 + W03
452700090610       //?F06=Conferma
452800090610       //--------------------------------------------------------------
452900090610       BEGSR F06W03D02;
453000090610
453100090610        // registro il contatto in tiatc
453200090610            exsr regis_atc ;
453300090610
453400090610       // scrivo il contatto in scadenza
453500090610         // pulisco i campi che non devono essere valorizzati
453600090610            clear atcest ;
453700090610            clear atcdco ;
453800090610            clear atchco ;
453900090610            clear atccco ;
454000090610            clear atcpru ;
454100090610            clear atccac ;
454200100310            clear atcoii ;
454300100310            clear atcofi ;
454400090702            clear atcco3 ;
454500091021            clear atccnw ;
454600091021            clear atcnra ;
454700100802            clear atcflo ;
454800090610
454900090702            // valorizzo il tipo contatto con quello indicato in tabella causali contatto
455000090702            atctat   = §ccotat ;
455100091214            IF  ConAffianca;
455200091214              atcatnp = sav_atlatnp + 1;
455300091214            ELSE;
455400090610            atcatnp  = atcatnp + 1 ;
455500091214            ENDIF;
455600090610            atccad   = W03cae   ;
455700090610            atcdad   = WW03dsc  ;
455800090610            atchda = w03osc * 100 ;
455900090610            atccmm  = %dec(W03com:7:0);
456000090703            // solo se è un appuntamento scrivo le ore di impegno  e il commerciale che inserisce
456100090703            // l'appuntamento
456200090702            if  prox_appunta = *on ;
456300090702                atcoii = w03oii ;
456400090702                atcofi = w03ofi ;
456500090702                atcco3 = %dec(v02com:7:0) ;
456600090702            endif ;
456700091023            // valorizzo data ora e utente immissione
456800091023            data_iso = (%date()) ;
456900091023            atcdim = %dec(data_iso) ;
457000091023            atchim = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
457100091023            atcpri = knmus ;
457200091113            atcnrv = w03nrv ;
457300100302            exsr Sr_ValCnw ;
457400100510            // se si tratta di un'attività 80 che apre una nuova trattativa siglo con atcatb= '*'
457500100510            // in modo tale venga riconosciuta in un'eventuale richiesta di cancellazione delle 80
457600100510            // dal programma trmk17r nel momento della creazione di una nuova attività dalla stampa
457700100510            // o invio mail tariffa. Questo flag dovrà poi essere pulito nel momento in cui ritorno
457800100510            // dalla creazione della trattativa
457900100723            // 21/07/10 non lo faccio + in quanto facciamo il commit nel tnta88r (gestione trattati-
458000100723            // va) così anche nel caso si voglia cancellare una 80 anche in fase di apertura di una
458100100723            // nuova trattativa si può fare
458200100723            //If atccad = '80' and  $new_trattati = *on ;
458300100721            // atcatb = '*' ;
458400100510            // mi salvo la chiave dell'attività da pulire dopo
458500100723            //   K80_atctat  = atctat ;
458600100723            //   K80_atcatn  = atcatn ;
458700100723            //   k80_atcatnp = atcatnp;
458800100723            // endif ;
458900110302
459000110302            //?Memorizzo la categoria del potenziale all'apertura dell'attività
459100110302              dATC01 = ATCflo;
459200110302              §ATCcapo1 = CPOfls;
459300110302              ATCflo = dATC01;
459400090610            write tiatc000  ;
459500121018
459600121018         //?Aggiorno il potenziale --> data primo contatto e categoria
459700121018           exsr agg_CPO;
459800091210
459900091210         //?Se appuntamento controllo se devo richiamare gestione affiancamento
460000091210         IF  prox_appunta and w03aff = 'S';
460100091210           exsr Ric_TRMK22 ;
460200091210           //?se errore o F12 devo tornare alla Window dell'appuntamento
460300100113           //?e devo riagganciare il TIATC perchè è già stato aggiornato
460400091210           IF  OMK22f12 = 'S' or OMK22err <> *blanks;
460500100113                chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
460600091210             leavesr;
460700091210           ENDIF;
460800091210         ENDIF;
460900100507
461000100507         // se dato f8 conferma (solo in caso di appuntamento )
461100100507         //?Richiamo pgm per richiesta dati mail
461200100507         If   dsp_aid = c_F08 ;
461300100915             clear  TRMK23ds1;
461400100915             // richiamo il programma con R in quanto voglio solo i dati della mail senza inviare
461500100915             // cosa che si farà nella fase finale di conferma
461600100915             Imk23rei = 'R';
461700100915             exsr Ric_trmk23 ;
461800100507         ENDIF;
461900090610
462000091113         // verifico se c'è' un pgm da richiamare in base alla causale
462100100419           If  §ccoapt = 'S' and $new_trattati = *on ;
462200091113         // richiamo il programma gestione trattativa
462300100510               exsr Ric_TNTA88 ;
462400100510         // Se l'apertura della trattativa è stata pilotata dalla 80 verifico
462500100510         // se devo pulire il flag
462600100723            // 21/07/10 non lo faccio + in quanto facciamo il commit nel tnta88r (gestione trattati-
462700100723            // va) così anche nel caso si voglia cancellare una 80 anche in fase di apertura di una
462800100723            // nuova trattativa si può fare
462900100723         //    If atccad = '80' and  $new_trattati = *on ;
463000100723         // // aggancio il record attività' per pulire il flag
463100100723         //        chain (k80_atctat:K80_atcatn:K80_atcatnp) tiatc01l ;
463200100723         //        If %found(tiatc01l) ;
463300100723         //           atcatb = ' ' ;
463400100723         //           update tiatc000 ;
463500100723         //           commit ;
463600100723         //        endif ;
463700100723         //        clear  K80_atctat  ;
463800100723         //        clear  K80_atcatn  ;
463900100723         //        clear  k80_atcatnp ;
464000100723         //    endif ;
464100091113                          If ota88f12 = 'S' ;
464200091113                             $inzd02 = *on ;
464300091113                             $video  = 'D2';
464400091113                             leavesr ;
464500091113                          endif ;
464600091113           endif ;
464700100311
464800100311         // chiedo conferma chiusura trattativa
464900100311             If  $chiedi_conf = *on ;
465000110622                  exsr Ges_Chiudotr ;
465100100311         // rolbk se ci sono errori o non richiesto annullamento trattativa
465200100503                  if (imk19cmt = 'S' and omk19err <> ' ') or w06sino = 'N' ;
465300100311                        rolbk ;
465400100311                        $inzd02 = *on ;
465500100311                        $Video = 'D2';
465600100311                        leavesr ;
465700100311                     endif ;
465800100311             endif ;
465900100419
466000100419         // se dato f8 conferma (solo in caso di appuntamento )
466100100419           If   dsp_aid = c_F08 ;
466200100915          // faccio il richiamo con solo invio mail in quanto ho già i dati della mail nella ds
466300100915                Imk23rei = 'I';
466400100419                exsr Ric_trmk23 ;
466500100419           endif ;
466600100419
466700100419
466800100419         // Prima di tornare al pgm chiamante verifico se devo chiedere infoCommerciali
466900100419
467000100419             exsr Sr_InfoComm ;
467100110412
467200110502         // richiamo il TRMK05 che mi ricalcola la categoria se in categoria eliminabile
467300121018          If CPOfls = 'E';
467400110412               clear trmk05ds;
467500110412               IMK05cpo = CPOcpo;
467600110412               trmk05r (kpjba : TRMK05DS);
467700110412         //?se categoria variata aggiorno potenziale
467800110412               IF  OMK05err = *blanks and OMK05cat <> CPOfls;
467900110412                   chain(e) CPOcpo TNCPO01L;
468000110412                   IF  not %error and %found(TNCPO01L);
468100110412                       CPOfls = OMK05cat;
468200110412                       update TNCPO000;
468300110412                   ENDIF;
468400110412               ENDIF;
468500110502          ENDIF;
468600100311
468700090611         // Ritorno al programma precedente
468800090611
468900090611            $fine  = *on ;
469000090610
469100090610       ENDSR;
469200121018
469300121018       //--------------------------------------------------------------
469400121018       //?Aggiorno dati su TNCPO.
469500121018       //--------------------------------------------------------------
469600121018       BEGSR agg_CPO;
469700121018
469800121018       //?Imposto data primo contatto sul Potenziale
469900121018         IF  §CCOdpc <> 'N' and (§CPOdtpcon = *blanks or §CPOdtpcon = *zeros);
470000121018           clear dCPO01;
470100121018           chain(e) CPOcpo TNCPO01L;
470200121018           IF  not %error and %found(TNCPO01L);
470300121018             dCPO01 = CPOrst;
470400121018             §CPOdtpcon = %editc(W02dat:'X');
470500121018             CPOrst = dCPO01;
470600121018             update TNCPO000;
470700121018           ENDIF;
470800121018         ENDIF;
470900121018
471000121018       //?Se attività 14/15 ricalcolo la categoria del potenziale
471100121018         IF ATCcac = '14' or ATCcac = '15';
471200121018           clear trmk05ds;
471300121018           IMK05cpo = CPOcpo;
471400121018           trmk05r (kpjba : TRMK05DS);
471500121018         //?se categoria variata aggiorno potenziale
471600121018           IF  OMK05err = *blanks and OMK05cat <> CPOfls;
471700121018             chain(e) CPOcpo TNCPO01L;
471800121018             IF  not %error and %found(TNCPO01L);
471900121018               CPOfls = OMK05cat;
472000121018               update TNCPO000;
472100121018             ENDIF;
472200121018           ENDIF;
472300121018         ENDIF;
472400121018
472500121018       ENDSR;
472600090701
472700091210       //--------------------------------------------------------------
472800091210       //?Rchiamo il programma di gestione esito offerta  TNTA56R
472900091210       //--------------------------------------------------------------
473000091210       BEGSR Ric_Tnta56;
473100091210
473200091210             clear tnta56ds ;
473300091210             ita56cmt = 'S';
473400091210             ita56fle = 'G' ;
473500091210             ita56fpr = 'A' ;
473600091210             ita56nrv = v02vis ;
473700100111             ita56tat = imk19tat;
473800100111             ita56atn = imk19atn;
473900100111             ita56atnp= imk19atnp;
474000100225             ita56com = imk19com ;
474100100225             ita56comd= imk19comd ;
474200091210             Tnta56r ( kpjba : tnta56ds );
474300091210
474400091210         // Se restituito errore => e c'è messaggio lo restituisco ...
474500091210         if  ota56err = 'E';
474600091210           errMessage  = *on;
474700091210           errGenerico = *on;
474800091210           V1Dmsg = ota56msg ;
474900091210
475000091210           If imk19cmt = 'S' ;
475100091210              rolbk ;
475200091210           endif ;
475300091210
475400091210           leavesr;
475500091210         endif;
475600091210
475700091210          // se dato f12 ripulisco gli aggiornamenti
475800091210
475900091210            If imk19cmt = 'S' and ota56F12 = 'S' ;
476000091210               rolbk ;
476100091210            endif ;
476200091210
476300091210         // se non ci sono errori e non è stato premuto F12
476400091210         //  confermo tutto quanto scritto fino ad ora
476500091210
476600091210           If ota56err = ' ' and ota56f12 = ' '  ;
476700091210             commit ;
476800091210           endif ;
476900091210
477000091210       ENDSR  ;
477100090701
477200091015       //--------------------------------------------------------------
477300091015       //?Rchiamo il programma di gestione trattative TNTA88R
477400091015       //--------------------------------------------------------------
477500091015       BEGSR Ric_Tnta88;
477600091015
477700091015             clear tnta88ds ;
477800091015             ita88cmt = 'S';
477900091015             ita88fle = 'P' ;
478000091016             ita88fpr = 'A' ;
478100091015             ita88cpo = cpocpo   ;
478200091027             ita88ksc = v02cli ;
478300091113             ita88nrv = newnrv ;
478400091027             if ita88ksc > 0 ;
478500091027                ita88fle = 'C' ;
478600091027             endif ;
478700091015             ita88cmmi = %dec(v02com: 7: 0);
478800091023             If w03com <> *blank ;
478900091023                ita88cmm = %dec(w03com: 7: 0);
479000091023             else ;
479100091027                ita88cmm = v02cmp ;
479200091023             endif ;
479300100225             ita88com = imk19com ;
479400100225             ita88comd= imk19comd ;
479500091015             Tnta88r ( kpjba : tnta88ds );
479600091015
479700091016         // Se restituito errore => e c'è messaggio lo restituisco ...
479800091015         if  ota88err = 'E';
479900091015           errMessage  = *on;
480000091015           errGenerico = *on;
480100091015           V1Dmsg = ota88msg ;
480200091016
480300091016           If imk19cmt = 'S' ;
480400091016              rolbk ;
480500100416              exsr Del_ticpn ;
480600091016           endif ;
480700091016
480800091015           leavesr;
480900091015         endif;
481000091015
481100091016          // se dato f12 ripulisco gli aggiornamenti
481200091016
481300091016            If imk19cmt = 'S' and ota88F12 = 'S' ;
481400091016               rolbk ;
481500100416               exsr Del_ticpn ;
481600091016            endif ;
481700091016
481800091113         // se non ci sono errori e non è stato premuto F12
481900091113         //  confermo tutto quanto scritto fino ad ora
482000091113
482100091113           If ota88err = ' ' and ota88f12 = ' '  ;
482200091016             commit ;
482300091016           endif ;
482400091016
482500091015       ENDSR  ;
482600101221
482700101221       //--------------------------------------------------------------
482800101221       //?Richiamo pgm gestione data caldo senza offerte.
482900101221       //--------------------------------------------------------------
483000101221       BEGSR Ric_Ta63;
483100101221
483200101221         clear TNTA63ds;
483300101221         ITA63fmt  = '4';
483400110118         ITA63fle  = 'I';
483500101221         ITA63nrv  = V02vis;
483600101221
483700101221         tnta63r (kpjba : TNTA63ds);
483800101221
483900101221       ENDSR  ;
484000101221
484100100416       //--------------------------------------------------------------
484200100416       //?Deleto le note che non riesce a fare il pgm con il rolbk
484300100416       //--------------------------------------------------------------
484400100416       BEGSR Del_ticpn;
484500100416                        clear trmk20ds ;
484600100416                        imk20tla = 'L';
484700100416                        imk20flm = 'D';
484800100416                        imk20cpo = cpocpo ;
484900100416                        imk20rsc = v02cpd ;
485000100416                        imk20tat = imk19tat ;
485100100416                        imk20atn = imk19atn ;
485200100416                        imk20atnp= imk19atnp ;
485300100416                        Trmk20r ( kpjba : trmk20ds );
485400100416       ENDSR  ;
485500090610       //--------------------------------------------------------------
485600090610       //?Registro i dati nel file tiatc
485700090610       //--------------------------------------------------------------
485800090610       BEGSR Regis_atc;
485900110304
486000090610         // se un primo contatto stacco un nuovo numero
486100090610         If primocontatto = *on ;
486200090610
486300090610            clear tiatc000 ;
486400090610
486500090610            RESET trul33ds;
486600090610            i33tla = 'L'  ;
486700090610            i33ope = 0    ;
486800090610            i33Cnu = 070  ;
486900090610            i33num = 1    ;
487000090610            i33Aaa = *YEAR;
487100090610            kpjbu = trul33ds;
487200090610            trul33r(kpjba);
487300090610            trul33ds = kpjbu;
487400090610            IF o33Err = 0;
487500090610              atcatn  = o33Nrf;
487600090610            ENDIF;
487700090610
487800090610       // imposto a zero il progressivo telefonata
487900090610            atcatnp  = 1 ;
488000090610       // imposto i campi che vengono inseriti solo nel primo contatto
488100090610            atctat = 'T' ;
488200090610            atccpo = %dec(v02cpo:11:0) ;
488300091021            atcksc = v02cli ;
488400090610         endif ;
488500090610        // Aggiorno un contatto in scadenza
488600090610       // questi si possono inserire anche se non sono i primi contatti
488700100114         // se appuntamento e sto esitando un'attività pianificata imposto come
488800100114         // esito il fatto o non fatto impostato a video
488900100114           IF  Appuntamento and
489000100114               Not Manutenzione and Not Vis_NonFatto and Not Visualizza and
489100100114               v02fatto <> *blanks;
489200100114             atcest = V02fatto;
489300100114           ELSE;
489400091215            atcest = §ccoesi;
489500100114          ENDIF;
489600090610            atcdco = w02dat ;
489700090610            atchco = v02ora * 100 ;
489800090610            atccco = %dec(v02com:7:0);
489900090610            atcpru = knmus  ;
490000090610            atccac = v02cae ;
490100091113
490200091113        // se devo creare trattativa da causale aggiorno il nuovo numero
490300091116         If §ccoapt = 'S' and newnrv > 0 and $new_trattati = *on ;
490400091113            atcnra = newnrv ;
490500091113         endif ;
490600100309
490700100309        // se ho richiesto l'esito della attività lo scrivo nei FLO
490800100309         If §ccores = 'S' ;
490900100309            clear datc01 ;
491000100309            datc01 = atcflo ;
491100100309            §atcesito = W07esi ;
491200100309            atcflo = datc01 ;
491300100309         endif ;
491400100309
491500121018        // imposto che è stato fatto contatto su potenziale mai contattato
491600121018         IF §CPOdtpcon = *blanks or §CPOdtpcon = *zeros;
491700100802            clear datc01 ;
491800100802            datc01 = atcflo ;
491900100802            §atcmaitr = 'S'    ;
492000100802            atcflo = datc01 ;
492100100802         endif ;
492200100802
492300090610        // scrivo il primo contatto ed aggiorno uno in scadenza
492400090610         If primocontatto = *on ;
492500091023            data_iso = (%date()) ;
492600091023            atcdim = %dec(data_iso) ;
492700091023            atchim = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
492800091023            atcpri = knmus ;
492900100302            exsr SR_valcnw ;
493000110302          //?Memorizzo la categoria del potenziale all'apertura dell'attività
493100110302            dATC01 = ATCflo;
493200110413            §ATCcapo2 = CPOfls;
493300110302            ATCflo = dATC01;
493400090610            write tiatc000  ;
493500090610         else ;
493600110302          //?Memorizzo la categoria del potenziale all'esecuzione dell'attività
493700110302            dATC01 = ATCflo;
493800110302            §ATCcapo2 = CPOfls;
493900110302            ATCflo = dATC01;
494000090610            update tiatc000 ;
494100090610         endif;
494200090610
494300090610         // se ci sono delle note le registro
494400090611         if v02no1 <> *blanks  or v02no2 <> *blanks ;
494500090610            clear trmk20ds ;
494600090610            imk20tla = 'L';
494700090610            imk20flm = 'C';
494800090610            imk20fno = 'S';
494900100215            imk20cmt = '1';
495000090610            imk20cpo = cpocpo ;
495100091027            If v02cli > 0 ;
495200091027               imk20ksc = v02cli ;
495300091027            endif ;
495400090610            imk20tat = atctat ;
495500090610            imk20atn = atcatn ;
495600090610            imk20atnp= atcatnp;
495700090610            imk20rsc = v02cpd ;
495800090610            emk20no1 = v02no1 ;
495900090610            emk20no2 = v02no2 ;
496000090610            emk20dim = atcdco ;
496100090610            emk20him = atchco ;
496200090616            Trmk20r ( kpjba : trmk20ds );
496300090610         endif ;
496400090610
496500121018       // solo nel caso in cui non si deve andare
496600110502         if §ccoupm =  ' ';
496700121018         //?Aggiorno il potenziale --> data primo contatto e categoria
496800121018           exsr agg_CPO;
496900090610         endif ;
497000090610
497100100329       // valorizzo i campi per il controllo delle info commerciali
497200100329         info_cac = atccac ;
497300100329         info_nrv = atcnrv ;
497400100329         info_nra = atcnra ;
497500100329
497600090610       ENDSR ;
497700091214
497800100302       //--------------------------------------------------------------
497900100302       //?Valorizzo il campo del cliente potenziale o codificato
498000100302       //--------------------------------------------------------------
498100100302       BEGSR Sr_ValCnw;
498200100302
498300100302       // Verifico se l'attività creata è su opotenziale o su cliente codificato
498400100302
498500110502       //   IF  atcksc = 0;
498600110502       //    IF  $gia_codifica = *on ;
498700110502       //      atccnw = 'C';
498800110502       //    ELSE;
498900110502       //      atccnw = 'P';
499000110502       //    ENDIF;
499100110502       //  ELSE;
499200110502       //   atccnw = 'C';
499300110502       //   ENDIF;
499400110502
499500110502       //  Imposto il flag di ATCCNW in base alla categoria del potenziale
499600110502       // se categoria = a "C" metto "C" altrimenti "P"
499700110502            If cpofls = 'C' ;
499800110502               atccnw = 'C' ;
499900110502            Else ;
500000110502               atccnw = 'P' ;
500100110502            Endif ;
500200100302
500300100302       ENDSR  ;
500400091214       //--------------------------------------------------------------
500500091214       //?Confermo esito affiancamento
500600091214       //--------------------------------------------------------------
500700091214       BEGSR Conf_Trmk22;
500800091214
500900091214         clear TRMK22ds;
501000091215         IMK22fcmt = '1';
501100091215         IMK22cmt = 'N';
501200091214         IMK22fmt = '4';
501300091214         IMK22fle = 'G';
501400091215         IMK22cac = v02cae;
501500100416         IMK22esi = V02fatto ;
501600091214         IMK22cmm  = %dec(v02com:7:0);
501700091214         IMK22dad  = w02dat;
501800091215         IMK22hda  = v02ora * 100;
501900091214         IMK22oii  = v02oii;
502000091214         IMK22ofi  = v02ofi;
502100091214         IMK22tat  = IMK19tat;
502200091214         IMK22atn  = IMK19atn;
502300091214         IMK22atnp = IMK19atnp;
502400091214
502500091214         trmk22r (kpjba : TRMK22ds);
502600091214
502700091214         //?Se ritorna errore lo visualizzo e faccio rollback
502800091214         IF  OMK22err <> *blanks;
502900091214           ErrMessage  = *on;
503000091214           ErrGenerico = *on;
503100091214           V1dmsg = OMK22msg;
503200091214           IF  IMK19cmt = 'S';
503300100721          // rolbk;
503400091214           ENDIF;
503500091214           leavesr;
503600091214         ENDIF;
503700091214
503800091214         //?Se F12 da gestione affiancamento rollback
503900091214         IF  IMK19cmt = 'S' and OMK22F12 = 'S';
504000100721         //rolbk;
504100091214           leavesr;
504200091214         ENDIF;
504300091214
504400091214         //?Se OK cerco ultimo progressivo
504500091214         clear sav_atlatnp;
504600091215         $EoF = *off;
504700091214         setll IMK19atn tiatl01l;
504800091215         DOW  not $EoF;
504900091214           reade IMK19atn tiatl01l;
505000091215           IF  %eof(tiatl01l);
505100091215             $EoF = *on;
505200091215             leave;
505300091215           ENDIF;
505400091215           sav_atlatnp = atlatnp;
505500091214         ENDDO;
505600091214
505700091214       ENDSR;
505800091210
505900091210       //--------------------------------------------------------------
506000091210       //?Richiamo il programma di gestione affiancamento TRMK22R
506100091210       //--------------------------------------------------------------
506200091210       BEGSR Ric_Trmk22;
506300091210
506400091210         clear TRMK22ds;
506500091215         IMK22fcmt = '1';
506600091215         IMK22cmt = 'N';
506700091210         IMK22fmt = '4';
506800091210         IF  Prox_Appunta;
506900091210           IMK22fle = 'I';
507000091210         ELSE;
507100091210           IMK22fle = 'G';
507200091210           IMK22cac = atccac;
507300100416           IMK22esi = v02fatto ;
507400091210         ENDIF;
507500100924         IF  §ccoupm <> ' ';
507600100412             IMK22cmm  = %dec(w03com:7:0);
507700100412         ELSE;
507800100412             IMK22cmm  = %dec(v02com:7:0);
507900100412         ENDIF;
508000091210         IMK22dad  = atcdad;
508100091210         IMK22hda  = atchda;
508200091210         IMK22oii  = atcoii;
508300091210         IMK22ofi  = atcofi;
508400091210         IMK22tat  = atctat;
508500091210         IMK22atn  = atcatn;
508600091210         IMK22atnp = atcatnp;
508700091210
508800091210         trmk22r (kpjba : TRMK22ds);
508900091210
509000091210         //?Se ritorna errore lo visualizzo e faccio rollback
509100091210         IF  OMK22err <> *blanks;
509200091210           ErrMessage  = *on;
509300091210           ErrGenerico = *on;
509400091210           IF  Prox_Appunta;
509500091210             W03msg = OMK22msg;
509600091210           ELSE;
509700091210             V1dmsg = OMK22msg;
509800091210           ENDIF;
509900091210           IF  IMK19cmt = 'S';
510000091210             rolbk;
510100091210           ENDIF;
510200091210           leavesr;
510300091210         ENDIF;
510400091210
510500091210         //?Se F12 da gestione affiancamento rollback
510600091210         IF  IMK19cmt = 'S' and OMK22F12 = 'S';
510700091210           rolbk;
510800091210         ENDIF;
510900091210
511000100622         //?se non ci sono errori e provengo dalla videata W03 e dopo
511100100622         //?questo programma devo richiamare la trattativa non committo
511200100622         //?perchè se F12 dalla trattativa devo poter fare il rolbk
511300100622         IF  prox_appunta  and w03aff = 'S' and
511400100622             §ccoapt = 'S' and $new_trattati;
511500100622           leavesr;
511600100622         ENDIF;
511700091210         //?Se non ci sono errori e non è stato premuto F12
511800091210         //?confermo tutto
511900091210         IF  OMK22err = *blanks and OMK22f12 = *blanks;
512000100721         // dopo una lunga e attenta osservazione di alcuni comportamenti del programma
512100100721         // abbiamo verificato che questo commit creava dei problemi tolto xchè ci pensa
512200100721         /// il programma + avanti a committare il tutto
512300100721         //  commit;
512400091210         ENDIF;
512500091210
512600091210       ENDSR;
512700091113
512800100329       //--------------------------------------------------------------
512900100329       //?Richiamo programma invio mail conferma appuntamento TRMK23R
513000100329       //--------------------------------------------------------------
513100100329       BEGSR Ric_Trmk23;
513200100507
513300100329
513400100915         If  Imk23rei <> 'I';
513500100915             Imk23f12 = 'S'    ;
513600100915             Imk23cma = atcco3 ;
513700100915             Imk23cmm = atccmm ;
513800100915             Imk23dta = atcdad ;
513900100915             Imk23ora = %dec(%subst(%editc(atchda:'X'):1:4):4:0);
514000100329        // recupero i dati del responsabile trasporti
514100100915             exsr respd02 ;
514200100915             Imk23rst = vh2rtr ;
514300100511        // imposto indirizzo mail immesso nella window
514400100915             Imk23emr = v02mrt ;
514500100915         Endif ;
514600100915
514700100915         trmk23r1 ( kpjba : TRMK23ds1 );
514800100915
514900100915         ATCco3 = Imk23cma;
515000100915
515100100915
515200100329
515300100329       ENDSR;
515400110207
515500110207       //--------------------------------------------------------------
515600110207       //?Richiamo il programma di gestione blocco cliente TRMK25R
515700110207       //--------------------------------------------------------------
515800110207       BEGSR Ric_Trmk25;
515900110207
516000110207         clear TRMK25ds;
516100110207         IMK25fcmt = '1';
516200110207         IMK25cmt = 'N';
516300110207         IMK25fmt = '4';
516400110207         IMK25fle = 'I';
516500110207         IMK25cmm  = %dec(v02com:7:0);
516600110209         IMK25cpo  = imk19cpo ;
516700110208         IMK25cac  = v02cae ;
516800110207
516900110207         trmk25r (kpjba : TRMK25ds);
517000110207
517100110207         //?Se ritorna errore lo visualizzo e faccio rollback
517200110207         IF  OMK25err <> *blanks;
517300110207           ErrMessage  = *on;
517400110207           ErrGenerico = *on;
517500110207           V1dmsg = OMK25msg;
517600110207           IF  IMK19cmt = 'S';
517700110207             rolbk;
517800110207           ENDIF;
517900110207           leavesr;
518000110207         ENDIF;
518100110207
518200110207         //?Se F12 da gestione affiancamento rollback
518300110207         IF  IMK19cmt = 'S' and OMK25F12 = 'S';
518400110207           rolbk;
518500110207         ENDIF;
518600110207
518700110207       ENDSR;
518800110207
518900100329
519000100329
519100100329       //--------------------------------------------------------------
519200100329       //?Gestione Richiesta Info Commerciali solo per alcune causali
519300100329       //?e solo in particolari situazioni
519400100329       //--------------------------------------------------------------
519500100329       BEGSR Sr_infocomm;
519600100329
519700100329       // verifico la causale esito attività è tra quelle che richiedono l'inserimento
519800100329       // delle info commerciali
519900120313
520000120313       If info_cac = *blanks ;
520100120313          leavesr ;
520200120313       endif ;
520300100329
520400100329       pos = %lookup(info_cac : $cac) ;
520500100329       select ;
520600100329       // se causale non presente si va a fine
520700100329       when  pos = 0  ;
520800100329       // se presente trattativa e non vuole info vado a fine
520900100329       when  $TRA(pos) = 'NO' and info_nrv > 0 ;
521000100329       // se richiede info solo se aperta trattativa e non è stata aperta vado a fine
521100100329       when $APT(pos) = 'SI' and info_nra = 0 ;
521200100329
521300100329       other ;
521400100329           exsr F14D02 ;
521500100329       endsl ;
521600100329
521700100329       Endsr ;
521800100329
521900090610       //--------------------------------------------------------------
522000090610       //?Operazioni iniziali.
522100090610       //--------------------------------------------------------------
522200090610       BEGSR RoutInz;
522300090610
522400090610         // Impostazione campi "fissi"
522500090610         T01pgm = SDSpgm;
522600090610
522700090610         // Reperimento dati job
522800090610         exsr DatiJob;
522900090610
523000110805         // Controllo se utente di sede
523100100210         // Controllo se utente autorizzato alla funzione
523200100210         reset TNTAA1DS;
523300100210         ITAA1caut = '§utegtc';
523400100210         kpjbu     = TNTAA1DS;
523500100210         tntaa1c (kpjba);
523600100210         TNTAA1DS = kpjbu;
523700100210
523800131127         // ATTENZIONE FLAG DI RITORNO UTILIZZATO ALL'INTERNO DEL PGM
523900100329
524000131127         IF  OTAA1fabi = 'N' and kcdaz <> 'MK01' ;
524100090610           $ErrGrave   = *on;
524200090610           errMessage  = *on;
524300090610           errGenerico = *on;
524400090610           V1Dmsg = $Msg(01);
524500090610           leavesr;
524600090610         endif;
524700090610
524800090610         // verifico se abilitare il commit oppure no
524900090610
525000090610            flgcom = (imk19cmt = 'S') ;
525100090610
525200090610            open tiatc01l ;
525300091119            open ticpn00f ;
525400090610
525500100721
525600100721             // verifico se già stato eseguito in fase di modifica o gestione
525700100721            chain(n) (imk19tat:imk19atn:imk19atnp) tiatc01l ;
525800100721            If  (imk19fle = 'G' or imk19fle = '2') and atcdco > 0 ;
525900100721                omk19err = 'E' ;
526000100721                $fine  = *on;
526100100721                omk19msg = 'Attività eseguita da altro utente' ;
526200100721                leavesr;
526300100721            endif;
526400111223
526500111223         //?Data del giorno
526600111223         wOggi = %dec(%date());
526700100721
526800090610
526900090610       ENDSR;
527000090610
527100090610       //--------------------------------------------------------------
527200090610       //?Reperimento Dati del job (Utente/Operativi).
527300090610       //--------------------------------------------------------------
527400090610       BEGSR DatiJob;
527500090610
527600090610         in(E) §AzUte;
527700090610         if NOT %error;
527800090610           in(E) §DatiUte;
527900090610         endif;
528000090610         if %error or RSut = *blanks;
528100090610           clear TIBS34ds;
528200090610           tibs34r(tibs34ds);
528300090610           in §AzUte;
528400090610           in §DatiUte;
528500090610         endif;
528600090610
528700090610       ENDSR;
528800090610
528900090610       //--------------------------------------------------------------
529000090610       //?Operazioni finali.
529100090610       //--------------------------------------------------------------
529200090610       BEGSR RoutEnd;
529300090610
529400090610
529500100422         // comitto se richiesto e se non ci sono errori
529600100422            if imk19cmt = 'S' and omk19err = ' ';
529700100422               commit ;
529800100422            endif ;
529900100422         // rolbk  se richiesto e se  ci sono errori
530000100422            if imk19cmt = 'S' and omk19err <> ' ';
530100100422               rolbk  ;
530200100422            endif ;
530300130531
530400130531         //?Chiudo il TRMK20R
530500130531            clear TRMK20DS;
530600130531            IMK20tla = 'C';
530700130531            trmk20r ( kpjba : trmk20ds );
530800130531
530900090610         *inLR = *on;
531000090610         return;
531100090610
531200090610       ENDSR;
531300090610
531400090610      /end-free
531500090610       //--------------------------------------------------------------
531600090610       //?Schiere a tempo di compilazione.
531700090610       //--------------------------------------------------------------
531800090610
531900090610** - $MSG -------------------------------------------------------------------*
532000100210Utente non abilitato alla funzione                                             01
532100100210Codice commerciale errato o non in gestione all'utente                         02
532200090611Data errata                                                                    03
532300110307Utente non autorizzato al cliente. Riassegnare l'attività a comm. abilitato.   04
532400090611Ora errata                                                                     05
532500100310Attività non possibile per Commerciale Interno.                                06
532600100721Attività già eseguita da altro utente uscire dal programma                     07
532700100308Obbligatorio inserire la causale attività                                      08
532800100920.....................                                                          09
532900100921.......................                                                        10
533000100920.....................                                                          11
533100121018.......................                                                        12
533200090702Ora inizio impegno incongruente con ora fine impegno                           13
533300090702Causale non utilizzabile                                                       14
533400090804Ore inizio e fine impegno incongruenti con ora appuntamento                    15
533500100210..........                                                                     16
533600091026Inserire ore inizio e fine impegno                                             17
533700091027Non è stato selezionato nessun cliente                                         18
533800091112Non si può utilizzare questa causale perchè trattativa già esistente           19
533900091112Non si può utilizzare questa causale perchè esiste già una trattativa          20
534000100114Immettere se Appuntamento effettuato.                                          21
534100100224Non si può annullare unica attività aperta della trattativa                    22
534200100225Causale non utilizzabile per questo tipo attività                              23
534300100225Causale non utilizzabile in quanto attività legata a trattativa con offerte    24
534400110502Causale non utilizzabile su potenziali                                         25
534500110502............   LIBERO                                                          26
534600100308Causale non utilizzabile legata ad una trattativa                              27
534700100308Presente trattativa già esitata in modo positivo. Causale inutilizzabile       28
534800100308Obbligatorio inserire delle note                                               29
534900100520Causale utilizzabile SOLO in presenza di trattativa                            30
535000100322Causale non utilizzabile in presenza di trattativa senza offerte               31
535100100329Obbligatorio inserire il Nome e indirizzo e-mail Resp.Trasporti                32
535200100421In caso di esecuzione di un'attività Post vendita utilizzare la causale 42     33
535300100504Fil. del codice commerciale incongruente con la fil. del cliente               34
535400100505Causale non utilizzabile per trattativa di tipo                                35
535500110321La causale xx non è utilizzabile per l'esecuzione di un'attività xx            36
535600100329** - $CAC -------------------------------------------------------------------*
535700120124242526284041719172                       Causali info comm
535800100329** - $TRA -------------------------------------------------------------------*
535900120104NONONONO  NO  NO                         NO = no legate a trattative
536000100329** - $APT -------------------------------------------------------------------*
536100120104              SI                         SI = solo se in apertura trattativa
