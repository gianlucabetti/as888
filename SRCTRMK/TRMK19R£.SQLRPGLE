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
017300150831     d  F04Attivo                     1n   overlay(IndDspF : 04)
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
046400150806
046500150806       // -?Interrogazione Campagna per Cliente
046600150806     d TRKC09DS      e ds                  inz
046700150806     d TRKC09R         pr                  extpgm('TRKC09R')
046800150806     d   kpjba                             likeds(kpjba)
046900150806     d   trkc09ds                          likeds(TRKC09DS)
047000150126
047100150126       // -?Controllo se cliente in (almeno una) Campagna?
047200150126     d TRKC73ds      e ds                  inz
047300150126     d TRKC73R         pr                  extpgm('TRKC73R')
047400150126     d   kpjba                             likeds(kpjba)
047500150126     d   trkc73ds                          likeds(TRKC73ds)
047600100413
047700100330      // - Aggiunge/Toglie gg/mm dalla data
047800100330     d Xgiolav         pr                  extpgm('XGIOLAV')
047900100330     d  xgiolavds                          likeds(xgiolavds)
048000100330
048100091112      //---------------------------------------------------------------
048200090610      //?prototipi
048300090610      //---------------------------------------------------------------
048400090610      /copy gaitrasrc/srcprotopr,tibs02r
048500090610      /copy gaitrasrc/srcprotopr,tibs34r
048600090610      /copy gaitrasrc/srcprotopr,tibs69r
048700091019      /copy gaitrasrc/srcprotopr,tntaa1c
048800091019      /copy gaitrasrc/srcprotopr,tnta12r
048900100924      /copy gaitrasrc/srcprotopr,tnta41r
049000091210      /copy gaitrasrc/srcprotopr,tnta56r
049100091210      /copy gaitrasrc/srcprotopr,tnta60r
049200091210      /copy gaitrasrc/srcprotopr,tnta88r
049300090701      /copy gaitrasrc/srcprotopr,tntb74r
049400090610      /copy gaitrasrc/srcprotopr,trmk20r
049500100915      /copy gaitrasrc/srcprotopr,trmk23r1
049600130809      /copy gaitrasrc/srcprotoPI,TRMK43R_1
049700130809      /copy gaitrasrc/srcprotoPR,TRMK43R
049800090610      /copy gaitrasrc/srcprotopr,trmk50r
049900090610      /copy gaitrasrc/srcprotopr,trul33r
050000090610      /copy gaitrasrc/srcprotopr,xsrda8
050100090611      /copy gaitrasrc/srcprotopr,QsnQryModS
050200090610
050300090610      //---------------------------------------------------------------
050400090610      //?Definizione key-list.
050500090610      //---------------------------------------------------------------
050600090610
050700091210       // - File TIATL02L
050800091210     d k02tiatl      e ds                  extname(TIATL02L:*key)
050900091210     d                                     prefix(k_)
051000091210     d                                     inz
051100090610
051200090610      //---------------------------------------------------------------
051300090610      //?Riepilogo indicatori.
051400090610      //---------------------------------------------------------------
051500090610      // - visualizzazioni particolari
051600150126      // 04    : Attiva F4=Campagna
051700090610      // 10    : Emissione particolare prima telefonata
051800090610      // 11    : Emissione "Altre note" nel caso in cui ci siano + di 2 righe
051900090610      // 13    : Indica un'attività già eseguita
052000090610      // 14    : Emissione "+" nel caso in cui ci siano + di 2 note
052100090610      // 15    : Manutenzione di un'attività
052200090610      // 16    : Visualizza un'attività
052300090610      // 17    : Identifica un'attività telefonica
052400090610      // 18    : Identifica un'attività di appuntamento
052500090623      // 19    : Visualizza un'attività non fatta
052600091105      // 20    : Disabilita F1 Anagrafica se richiamato dal pgm dei potenziali e cliente a zero
052700100928      // 21    : Gestione di un'attività
052800090630      // 22    : Identifica una telefonata nella window
052900090630      // 23    : Identifica un appuntamento nella window
053000100309      // 24    : Visualizza l'esito solo se esiste
053100091019      // 25    : Visualizza la trattativa
053200091019      // 26    : Visualizza il codice cliente in testata ed pilota F1 Anagrafica Cliente
053300091022      // 27    : Visualizza il numero trattativa nuovo
053400091022      // 29    : Visualizza se appuntamento di affiancamento
053500091105      // 30    : No tasti funzionali - F12 se richiamato in visualizzazione da altri pgm - trmk18
053600091105      // 31    : Disabilito il tasto funzionale f11 agenda se richiamato da Agenda
053700091105      // 32    : Disabilito il tasto funzionale f09 Trattativa se richiamato da Trattativa
053800091113      // 34    : Visualizzo la trattativa legata all'attività futura che vado a scadenziare
053900091118      // 35    : Visualizzo NUOVA se la trattativa è nuova
054000091119      // 36    : Segnalo la presenza di più trattative
054100091215      // 37    : Abilita F20=Affiancamento
054200091215      // 38    : Visualizza che appuntamento è con affiancamento
054300090610      // - Comuni
054400090610      // 28    : Emissione messaggio di errore a video
054500090610      // - D02
054600090610      // 60    : Errore: Commerciale che esegue l'attività
054700090610      // 61    : Errore: Data attività
054800090610      // 62    : Errore: Ora attività
054900090610      // 64    : Errore: Causale attività
055000090610      // 65    : Errore: Scadenza data attività
055100090610      // 66    : Errore: Scadenza ora attività
055200090610      // 67    : Errore: Commerciale che edeve eseguire l'attività
055300090610      // 68    : Errore: Ora inizio impegno appuntamento attività
055400090610      // 69    : Errore: Ora fine   impegno appuntamento attività
055500091216      // 75    : Posizionamento cursore su campo Nota
055600091215      // - W03
055700091215      // 71    : Errore: Data appuntamento
055800091215      // 72    : Errore: Ora  appuntamento
055900091215      // 73    : Errore: Commerciale appuntamento
056000091215      // 79    : Errore: Ora inizio impegno
056100091215      // 80    : Errore: Ora fine   impegno
056200090610      // - Comuni
056300090610      // 99    : Generico di Errore
056400090610      //---------------------------------------------------------------
056500090610
056600090610      //---------------------------------------------------------------
056700090610      //?M A I N - L I N E
056800090610      //---------------------------------------------------------------
056900090610
057000090610     c     *Entry        plist
057100090610     c                   parm                    KPJBA
057200090610     c                   parm                    trmk19ds
057300100409     c                   parm                    trmk01ds1
057400100409
057500090610
057600090610      /free
057700090610
057800090611       // Operazioni iniziali controllo se video accetta 132 caratteri
057900090611       if not %open(trmk19d);
058000090611           dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
058100090611           If dsp_mod <> 0 ;
058200090611              open trmk19d;
058300090611           else ;
058400090611              eval $fine = *on ;
058500090611           endif;
058600090611       endif;
058700090610
058800090610       // Operazioni iniziali
058900090610       exsr RoutInz;
059000090610
059100100616       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
059200100616
059300091021       clear savcli ;
059400091021
059500091019       // Verifico se si tratta di primo contatto potenziale
059600090611       if imk19fle = 'P' ;
059700091019          exsr Primo_Po  ;
059800090611       endif ;
059900091019
060000091019       // Verifico se si tratta di primo contatto Cliente
060100091019       if imk19fle = 'C' ;
060200091019          exsr Primo_Cli ;
060300100427          If omk19err <> ' ' ;
060400100427             $fine = *on ;
060500100427          endif ;
060600091019       endif ;
060700090701
060800100108
060900100108       // Se richiamato da Agenda verifico se attività legata a cliente
061000100108       // o attività d'ufficio
061100100108       IF  IMK19fpr = 'A';
061200100108         clear dTTA;
061300100108         clear TIBS02ds;
061400100108         T02mod = 'C';
061500100108         T02cod = 'TTA';
061600100108         T02ke1 = IMK19tco ;
061700100108         T02sif = KNSIF;
061800100108         TNTBE_RicercaControllo  (kpjba : tibs02ds);
061900100108         IF  T02err = *blanks;
062000100108           dTTA = t02uni ;
062100100108         ENDIF;
062200100108         IF  §TTAleg <> 'S';
062300100108           $InzD02 = *off;
062400100108           $InzW03 = *off;
062500100108           $InzD04 = *on;
062600100108           $Video = 'D4';
062700100108         ENDIF;
062800100108       ENDIF;
062900090611
063000090610       // Gestione video
063100090610       DOW $Fine = *off;
063200090610         select;
063300090610           when $Video = 'D2';
063400090610             exsr GesD02;
063500100224           when $Video = 'W5';
063600100224             exsr GesW05;
063700090610           when $Video = 'W3';
063800090610             exsr GesW03;
063900100108           when $Video = 'D4';
064000100108             exsr GesD04;
064100100614           when $Video = 'W8';
064200100614             exsr GesW08;
064300110225           when $Video = 'W9';
064400110225             exsr GesW09;
064500110530           when $Video = 'W0';
064600110530             exsr GesW10;
064700090610           other;
064800090610             $Fine = *on;
064900090610         endsl;
065000090610       ENDDO;
065100090610
065200090610
065300090610       // Operazioni finali
065400090610       exsr RoutEnd;
065500090611
065600090611       //--------------------------------------------------------------
065700091019       //?Gestione Primocontatto  Potenziale
065800090611       //--------------------------------------------------------------
065900091019       BEGSR Primo_Po ;
066000090611
066100090611        // se potenziale valido recupero i dati del potenziale da immettere nella prima
066200090611        // parte della videata
066300090611
066400100409         Cpocpo = W_mk01cpo ;
066500100409         W_mk01F06 = ' ' ;
066600090611         primocontatto = *on ;
066700090611         $Video = 'D2';
066800090611         $InzD02 = *on;
066900090611         $prima  = *on;
067000090611
067100090611       ENDSR;
067200090610
067300091019       //--------------------------------------------------------------
067400091019       //?Gestione Primocontatto  Cliente
067500091019       //--------------------------------------------------------------
067600091019       BEGSR Primo_Cli ;
067700100802
067800100802        // se cliente valido recupero i dati del cliente da immettere nella prima
067900100802        // parte della videata
068000100802
068100100802         savcli = IMK19ksc;
068200100802         clear  tibs69ds;
068300100802         I69kac = savcli;
068400100802         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
068500100802         if acolib = 0 ;
068600100802            omk19ksc = savcli ;
068700100802            OMK19ERR = 'E' ;
068800100802            OMK19MSG = 'Manca Potenziale sul cliente impossibile gestire nuove +
068900100802            attività' ;
069000100802         else ;
069100100802            primocontatto = *on ;
069200100802            $Video = 'D2';
069300100802            $InzD02 = *on;
069400100802            $prima  = *on;
069500100802         endif ;
069600091019
069700091019
069800091019       ENDSR;
069900091019
070000090610       //--------------------------------------------------------------
070100090610       //?Gestione videata D02
070200090610       //--------------------------------------------------------------
070300090610       BEGSR GesD02;
070400090610
070500090610         // Inizializzazione videata
070600090610         if $InzD02 = *on;
070700090610           exsr InzD02;
070800090610           $InzD02 = *off;
070900090610         endif;
071000110301
071100110301       //?Se dopo la INZ del video non devo emettere la videata ma la win con
071200110301       //?il messaggio di annullo attività esco dalla routine
071300110527         IF  $Video = 'W9';
071400110301           leavesr;
071500110301         ENDIF;
071600110530       //?Se dopo la INZ del video non devo emettere la videata ma la win con
071700110530       //?il messaggio di avviso attività da annullare esco dalla routine
071800110530         IF  $Video = 'W0';
071900110530           leavesr;
072000110530         ENDIF;
072100101221
072200101221         clear sav_DataHot;
072300090610
072400090610         // Emissione testata
072500090610           write MK19T01;
072600090610
072700090610         // Emissione videata
072800090610         exfmt MK19D02;
072900090610         errMessage   = *off;
073000090610         errGenerico  = *off;
073100090610         clear V1Dmsg;
073200090610
073300090610         select;
073400090610         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
073500090610           when  $ErrGrave = *on;
073600090610             $Fine  = *on;
073700090610         // F12 = Ritorno
073800090610           when dsp_aid = c_F12;
073900090610             exsr F12D02;
074000090611             leavesr ;
074100090701         // F21 = invio mail
074200090701           when dsp_aid = c_F21;
074300100921                If V02rtr = *blanks or v02mrt = *blanks ;
074400100921                   errMessage  = *on;
074500100921                   errGenerico = *on;
074600100921                   V1Dmsg = $Msg(32);
074700100921                   leavesr;
074800100921                 endif;
074900090701             exsr F21d02;
075000090701             leavesr ;
075100091021         // F01 = Anagrafica
075200090610           when dsp_aid = c_F01;
075300090610             exsr F01D02;
075400150126           // -?F4=Campagna?
075500150126           when  dsp_aid = c_F04;
075600150126             exsr  F04D02;
075700090610         // F18 = Note
075800090610           when dsp_aid = c_F18;
075900090610             exsr F18D02;
076000090610         // F02 = Contatti
076100090610           when dsp_aid = c_F02;
076200090610             exsr F02D02;
076300100511             leavesr ;
076400090610         // F14 = Informazioni commerciali
076500090610           when dsp_aid = c_F14;
076600090610             exsr F14D02;
076700091026         // F09 = Trattativa
076800091026           when dsp_aid = c_F09;
076900091026             exsr F09D02;
077000091026         // F11 = Agenda
077100091026           when dsp_aid = c_F11;
077200091026             exsr F11D02;
077300091026             leavesr ;
077400091210         //?F20 = Affiancamento
077500091210           when dsp_aid = c_F20;
077600091210             exsr F20D02;
077700091210             leavesr ;
077800100301         // F16 = Annullamento abilitato solo in manutenzione da trattativa
077900100224           when dsp_aid = c_F16;
078000100224             exsr F16D02;
078100100224             leavesr ;
078200100928         //?F05 = Attività
078300100930         //  when dsp_aid = c_F05;
078400100930         //    exsr F05D02;
078500100930         //    leavesr ;
078600090610
078700090610         // Enter
078800090610           other;
078900090610         // Se non si tratta di visualizzazione
079000090610            If visualizza = *off ;
079100090610               exsr CtrD02;
079200090610               if errGenerico = *on ;
079300090610                   leavesr;
079400090610               endif;
079500090610            endif;
079600090610           endsl;
079700090610
079800100329         // se dato f8 conferma appuntamento + invio mail prima verifico se ha indirizzo mail
079900100329         // altrimenti errore
080000100329           If   dsp_aid = c_F08 ;
080100100329                If V02rtr = *blanks or v02mrt = *blanks ;
080200100329                   errMessage  = *on;
080300100329                   errGenerico = *on;
080400100329                   PosCurCmi02 = *on;
080500100329                   V1Dmsg = $Msg(32);
080600100329                   leavesr;
080700100329                 endif;
080800100329                    exsr F06D02 ;
080900100329
081000100329         // Ritorno al programma precedente
081100100329                    $fine  = *on ;
081200100329                    leavesr ;
081300100329           endif ;
081400100329
081500100329         // se dato f6 conferma
081600090610           If   dsp_aid = c_F06 ;
081700090610
081800090610         // -...- VISUALIZZAZIONE vado via
081900090610                If  visualizza   = *on ;
082000090610                    leavesr ;
082100090610                endif ;
082200090610
082300090610         // -...- MANUTENZIONE
082400090610                If  manutenzione = *on ;
082500090610                    exsr F06D02 ;
082600090610                    leavesr ;
082700090610                endif ;
082800090610
082900090610         // -...- GESTIONE
083000100308
083100100308         // verifico se risulta necessario fare delle domandine
083200100308
083300100308         // chiedo esito attività/trattativa    se causale lo richiede
083400100308             If  §ccores = 'S' ;
083500100308                  exsr Ges_Esito  ;
083600100308             endif ;
083700101221
083800101221         //?Se attività prevede la richiesta della data caldo senza offerta
083900101221         //?richiamo il pgm
084000101221             IF  §CCOhot = 'S';
084100101221               exsr Ric_TA63;
084200101221         //?se F12 o errore devo tornare alla videata dell'esito attività
084300101221               IF  OTA63f12 = 'S' or OTA63err <> *blanks;
084400101221                 leavesr;
084500101221               ENDIF;
084600101221         //?memorizzo la data caldo senza offerta
084700101221               sav_DataHot = OTA63dpa;
084800101221             ENDIF;
084900110301
085000110301            SELECT;
085100110301
085200110301         //?Se attività prevede il richiamo al programma di gestione anagrafica clienti
085300110301         //?richiamo il pgm
085400110301             WHEN  §CCOpgmblc <> *blanks;
085500110301           //?verifico se l'utente può gestire il cliente
085600110301               reset TNTAA1DS;
085700110302               ITAA1ksc  = V02cli;
085800110301               ITAA1caut = '§UTECLI';
085900110301               kpjbu     = TNTAA1DS;
086000110301               tntaa1c (kpjba);
086100110301               TNTAA1DS = kpjbu;
086200110301               IF  OTAA1fabi = 'N';
086300110301                 chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
086400110307                 V1Dmsg = $msg(04);
086500110301                 errMessage  = *on;
086600110301                 errGenerico = *on;
086700110301                 leavesr;
086800110301               ENDIF;
086900110301               clear TNTA60DS;
087000110301               TA60flg = 'M';
087100110301               TA60cli = V02cli;
087200110412               Parmcbl = atcprg ;
087300110412               tnta60r (kpjba:tnta60ds:Parmcbl);
087400110301               IF  TA60f03 <> *blanks or TA60err <> *blanks;
087500110301                  chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
087600110504                  If TA60err <> *blanks ;
087700110504                     errMessage  = *on;
087800110504                     errGenerico = *on;
087900110504                     V1Dmsg = ta60msg;
088000110504                  Endif ;
088100110301                  leavesr;
088200110301               ENDIF;
088300100308
088400110207        // se attività richiede la creazione di una nuova attività ma richiede anche
088500110207        // il richiamo al blocco del cliente richiamo il pgm TRMK25R
088600110301             WHEN  §ccoblc <> ' ' ;
088700110207                   exsr Ric_trmk25 ;
088800110207                   IF  OMK25f12 = 'S' or OMK25err <> *blanks;
088900110207                       chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
089000110207                       leavesr;
089100110207                   ENDIF;
089200100329         // Emetto la window  W03  se causale necessita la creazione di un nuovo contatto
089300110301             WHEN   §ccoupm <> ' ' ;
089400100520         //?se attività legata a trattativa e chiude trattativa a potenziale codificato
089500100520         //?non devo creare nuova attività
089600100520                  IF  Vis_trattativ and §CCOcht = 'S' and $Gia_Codifica;
089700100924                      §ccoupm = ' ';
089800100520                  ELSE;
089900100520
090000091214         //?Se appuntamento con affiancamento prima di emettere la win per nuova attività
090100091214         //?devo esitare prima l'affiancamento
090200100311                    IF  ConAffianca;
090300100311                        exsr Conf_TRMK22 ;
090400091214           //?se errore o F12 devo tornare alla videata dell'esito appuntamento
090500100311                        IF  OMK22f12 = 'S' or OMK22err <> *blanks;
090600100311                            leavesr;
090700100311                        ENDIF;
090800100311                    ENDIF;
090900090610                    $Video = 'W3';
091000090610                    $inzw03 = *on ;
091100100311                    leavesr ;
091200100520                  ENDIF;
091300110301
091400110301             ENDSL;
091500100311
091600100311         // registro la scrittura del contatto senza creare il contatto successivo
091700090610                    exsr regis_atc ;
091800091214
091900091214         //?Se appuntamento con affiancamento devo esitare anche l'eventuale affiancamento
092000100311                    IF  ConAffianca;
092100100721                        exsr Ric_TRMK22 ;
092200100311           //?se errore o F12 devo tornare alla videata dell'esito appuntamento
092300100311                        IF  OMK22f12 = 'S' or OMK22err <> *blanks;
092400100721                            chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
092500100311                            leavesr;
092600100311                        ENDIF;
092700100311                    ENDIF;
092800100311
092900100311         // chiedo conferma chiusura trattativa
093000100311             If  $chiedi_conf = *on ;
093100110622                  exsr Ges_Chiudotr ;
093200100311         // rolbk se ci sono errori o non richiesto annullamento trattativa
093300100503                  if (imk19cmt = 'S' and omk19err <> ' ') or w06sino = 'N' ;
093400100311                        rolbk ;
093500100311                        $inzd02 = *on ;
093600100311                        leavesr ;
093700100311                     endif ;
093800100311             endif ;
093900091214
094000091210         // verifico se devo richiamare il pgm apertura trattativa
094100091116                    if §ccoapt <> *blank  ;
094200091015         // richiamo il programma gestione trattativa
094300091015                          exsr Ric_TNTA88 ;
094400091022                          If ota88f12 = 'S' ;
094500091022                             $inzd02 = *on ;
094600091022                             leavesr ;
094700091022                          endif ;
094800091015                    endif ;
094900100311
095000091210         // verifico se devo richiamare il pgm esito offerta
095100091210                    if §ccoeso <> *blank  ;
095200091210         // richiamo il programma esito offerta
095300091210                          exsr Ric_TNTA56 ;
095400100308                          If ota56f12 = 'S' or ota56Err = 'S' ;
095500091210                             $inzd02 = *on ;
095600091210                             leavesr ;
095700091210                          endif ;
095800091210                    endif ;
095900091016
096000100311         // comitto se richiesto e se non ci sono errori
096100091016            if imk19cmt = 'S' and omk19err = ' ';
096200091016               commit ;
096300091016            endif ;
096400091016
096500100419         // Richiedo Info Commerciali
096600100419                exsr Sr_infocomm ;
096700091016
096800110412         // alla fine di tutto se il potenziale è in categoria Eliminabile
096900110412         // richiamo il TRMK05 che mi ricalcola la categoria
097000110412           IF  CPOfls = 'E';
097100110412               clear trmk05ds;
097200110412               IMK05cpo = CPOcpo;
097300110412               trmk05r (kpjba : TRMK05DS);
097400110412         //?se categoria variata aggiorno potenziale
097500110412               IF  OMK05err = *blanks and OMK05cat <> CPOfls;
097600110412                   chain(e) CPOcpo TNCPO01L;
097700110412                   IF  not %error and %found(TNCPO01L);
097800110412                       CPOfls = OMK05cat;
097900110412                       update TNCPO000;
098000110412                   ENDIF;
098100110412               ENDIF;
098200110412           ENDIF;
098300110412
098400100329         // Ritorno al programma precedente
098500091016                $fine  = *on ;
098600090610                endif;
098700091015
098800090610       ENDSR;
098900100308
099000100308       //--------------------------------------------------------------
099100110622       //?Gestione Richiesta conferma chiusura Trattativa
099200100308       //--------------------------------------------------------------
099300110622       BEGSR Ges_Chiudotr ;
099400100308
099500100308             clear w06sino ;
099600100517             $fine6 = *off ;
099700100517
099800100517          dow $fine6 = *off ;
099900100308             exfmt mk19w06 ;
100000100517
100100100517       // N  = Nulla
100200100517             If w06sino = 'N' ;
100300100517                leavesr ;
100400100517             endif ;
100500100517
100600100517       // S  = Richiamo il pgm chiusura trattativa
100700100308
100800100503             If w06sino = 'S' ;
100900100520
101000100520               chain V02vis TIVIS05L;
101100100520               IF  %found(TIVIS05L);
101200100520                 VISesi = V02cae;
101300100520                 VISdch = %dec(%date());
101400101013        // se ho richiesto l'esito della attività lo scrivo nei FLO
101500101013                 If §ccores = 'S' ;
101600101013                    clear dvis01 ;
101700101013                    dvis01 = visflo ;
101800101013                    §visesito = W07esi ;
101900101013                    visflo = dvis01 ;
102000101013                 endif ;
102100100520                 update TIVIS000;
102200100520               ENDIF;
102300110208
102400110208        // in caso di trattativa con tariffe 999 vado ad aggiornare i dati dell'offerta / caldo
102500110208             chain (V02vis:999:0) tivof01l ;
102600110208             If %found(tivof01l) ;
102700110208             // in caso di richiesta esito e l'esito è positivo convalido l'offerta
102800110208                If §ccores = 'S' and W07esi = 'P' ;
102900110208                   vofdco = %dec(%date()) ;
103000110208                   vofeso = 'C' ;
103100110208                endif;
103200110208             // in caso di richiesta esito e l'esito è negativo annullo l'offerta
103300110208                If §ccores = 'S' and W07esi = 'N' ;
103400110208                   vofeso = '*' ;
103500110208                endif;
103600110208             // in caso di chiusura senza richiesta di esito annullo l'offerta
103700110208                If §ccores <> 'S'  ;
103800110208                   vofeso = '*' ;
103900110208                endif;
104000110208                update tivof999 ;
104100110208             Endif ;
104200110208
104300100924         // richiamo il programma delle info trattative se inserite
104400110622               If visinfot <> ' ' or (ATCCAC = '41' and vistpv = 'I');
104500100924                   exsr Sr_infotrat ;
104600120706         // se al ritorno dal PGM INFO trattattive sono state inserite ed il flag INFOT
104700120706         // sul file TRATTATIVE è a blank lo aggiorno
104800120706                   If o41ifotot <> ' ' and visinfot = ' ' ;
104900120706                      chain V02vis TIVIS05L;
105000120706                      IF  %found(TIVIS05L);
105100120706                          VISinfot = o41ifotot ;
105200120706                          update TIVIS000;
105300120706                      ENDIF;
105400120706                   endif ;
105500110622         // se in uscita visinfot è uguale a " " e causale 41 di tratattiva di tipo I
105600110622         // richiamo info trat
105700110622               dow o41ifotot = ' ' and atccac = '41' and vistpv =  'I' ;
105800110622                   exsr Sr_infotrat ;
105900110622                   If o41ifotot <> ' ' ;
106000110622                      chain V02vis TIVIS05L;
106100110622                      IF  %found(TIVIS05L);
106200110622                          VISinfot = o41ifotot ;
106300110622                          update TIVIS000;
106400110622                      ENDIF;
106500110622                   endif ;
106600110622               enddo;
106700100924               Endif ;
106800100311
106900100924         // richiamo il programma delle info commerciali
107000140929               Wchiudo_tr = 'S' ;
107100100924               exsr F14D02 ;
107200100924
107300100517             leavesr ;
107400100308             endif ;
107500100517           enddo ;
107600100308
107700100308       Endsr ;
107800100308
107900100924       //--------------------------------------------------------------
108000100924       //?Gestione Info trattativa
108100100924       //--------------------------------------------------------------
108200100924       BEGSR Sr_infotrat ;
108300100924
108400100924         // Richiamo il pgm informazioni Trattativa
108500100924         clear  TNTA41ds;
108600100924         I41nrv    = visnrv;
108700100924         I41rag    = visrag;
108800100924         I41ifotot = VISinfot;
108900100924         I41mod    = 'G';
109000100924         I41pgm    = 'TRMK19R';
109100101220         I41tpv    = VIStpv;
109200100924
109300100924         tnta41r (kpjba : TNTA41ds);
109400100924
109500100924       ENDSR;
109600100308       //--------------------------------------------------------------
109700100308       //?Gestione Richiesta Esito Attività/trattativa
109800100308       //--------------------------------------------------------------
109900100308       BEGSR Ges_Esito  ;
110000100308
110100100308             clear w07esi  ;
110200100517             $fine7 = *off ;
110300100517
110400100517           Dow $fine7 = *off ;
110500100308             exfmt mk19w07 ;
110600100517             If w07esi =  'P' ;
110700100517                leavesr ;
110800100517             endif ;
110900100517             If w07esi =  'N' ;
111000100517                leavesr ;
111100100517             endif ;
111200100517           enddo ;
111300100308
111400100308       Endsr ;
111500090610
111600090610       //--------------------------------------------------------------
111700090610       //?Caricamento videata telefonata D02
111800090610       //--------------------------------------------------------------
111900090610       BEGSR Inzd02;
112000090610
112100090610          clear mk19d02 ;
112200090610
112300090610       // imposto indicatori di visualizzazione
112400090610          Visualizza = (Imk19Fle = '5');
112500090610          Manutenzione = (Imk19Fle = '2');
112600100928          Gestione     = (Imk19Fle = 'G');
112700091119          Telefonata   = (Imk19tco = 'T') or (imk19tco = 'O') ;
112800090610          Appuntamento = (IMK19tco = 'A');
112900110405
113000110405          $cli_non_sped = *off;
113100110405
113200091119             // tipo attivita
113300091119                  clear  dTTA;
113400091119                  clear  TIBS02ds;
113500091119                  T02mod = 'C';
113600091119                  T02cod = 'TTA';
113700091119                  T02ke1 = Imk19tco ;
113800091119                  T02sif = KNSIF;
113900091119                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
114000091119                  if  T02err = *blanks;
114100091119                      dtta = t02uni ;
114200091119                  endif;
114300091216
114400091216         //?Centro titolo alto
114500091216         clear wpos;
114600091216         wpos = (%len(v02tit) - %len(§ttatit)) / 2;
114700091216         %subst(v02tit:wpos:%len(§ttatit)) = §ttatit;
114800091216
114900090623          If visualizza = *on ;
115000090623             Vis_nonfatto = (Imk19Fatt = ' ');
115100090623          else ;
115200090623             Vis_nonfatto = *off ;
115300090623          endif ;
115400090612
115500090612       // Verifico se è stato richiamato dal Programma di gestione Potenziali
115600091105              Potenziali = (imk19fpr = 'P') ;
115700091105       // Verifico se è stato richiamato dal Programma Agenda
115800091105              Ric_agenda = (imk19fpr = 'A') ;
115900091105       // Verifico se è stato richiamato dal Programma Gestione trattative
116000091105              Ric_trattativ = (imk19fpr = 'T') ;
116100091105       // Verifico se è stato richiamato in visualizzazione non dal TRMK18
116200091105              Ric_visualizz = (imk19fpr <> ' ') ;
116300100803       // Verifico se è stato richiamato con flag di provenienza "V" solo visualizzazione
116400110805       // Verifico se è utente di sede non utilizza F21
116500131127       // Verifico se è stata chiamata dall'azione MK01 non utilizza F21
116600131127              NO_F21 = (imk19fpr = 'V') or (Dutpou= 046) or KCDAZ = 'MK01';
116700091105
116800090611       // imposto testata
116900091020          If  imk19fle = 'P' or imk19fle = 'C';
117000090611              v01tit = tit_primo ;
117100090611          endif ;
117200090610
117300090611          If  imk19fle = '5' ;
117400090611              v01tit = tit_visua ;
117500090611          endif ;
117600090611
117700090611          If  imk19fle = '2' ;
117800090611              v01tit = tit_manut ;
117900090611          endif ;
118000090611
118100090611          If  imk19fle = 'G' ;
118200090611              v01tit = tit_gesti ;
118300090611          endif ;
118400090611
118500091026       // imposto il codice cliente
118600091026          v02cli  = savcli ;
118700091026
118800090610       // se provengo da sbf devo fare gli agganci con i dati passati dal subfile
118900091026          If Imk19fle <>  'P' and imk19fle <> 'C';
119000090610             cpocpo = imk19cpo ;
119100091026             v02cli = imk19ksc ;
119200090610          endif ;
119300090610
119400090610             altrenote = *off  ;
119500090610             piunote   = *off  ;
119600091218             clear sav_atcdad;
119700111102             clear sav_atchda;
119800111102             clear sav_atcoii;
119900111102             clear sav_atcofi;
120000111102             clear sav_atccmm;
120100090610
120200090610       // recupero i dati anagrafici ed informativi del potenziale
120300090610
120400090610          exsr RecD02 ;
120500090610
120600090610        // se contatto non inserito manualmente oppure primo contatto recupero
120700090610        // dati dal file contatti
120800090610         If  $prima = *off  ;
120900090610             If manutenzione = *off and visualizza = *off ;
121000090611                chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
121100090610             else  ;
121200090611                chain(n) (imk19tat:imk19atn:imk19atnp) tiatc01l ;
121300090610             endif ;
121400090610             If   %found(tiatc01l);
121500140411               // -?Data/Ora/Utente inserimento?
121600140414               clear  Data_Eur;
121700140414               clear  wData;
121800140414               clear  wHhMm;
121900140414               if  ATCdim > *zero;
122000140414                 Data_Eur = %date( ATCdim : *iso );
122100140414                 wData    = %dec( Data_Eur );
122200140414               endif;
122300140414               wHhMm = %int( ATChim / 100 );
122400140414               %subst( V02tit : %len(V02tit) - 32 ) = 'Ins. ' +
122500140414                       %editc( wData : 'Y' ) + ' ' +
122600140414                       %editw( wHhMm : '  :  ' ) + ' ' +
122700140414                       ATCpri;
122800090610             // data scadenza contatto
122900090610                  If atcdad > 0 ;
123000090610                     data_eur = %date(atcdad:*iso);
123100090610                     v02scd   = %dec(data_eur) ;
123200091211                     w02scd   = atcdad;
123300091218                     sav_atcdad = atcdad;
123400111102                     sav_atchda = atchda;
123500111102                     sav_atcoii = atcoii;
123600111102                     sav_atcofi = atcofi;
123700111102                     sav_atccmm = atccmm;
123800090610                  endif ;
123900090610             // ora  scadenza contatto
124000090610                  If atchda > 0 ;
124100090610                     v02sco = %dec(%subst(%EDITC(atchda:'X'):1:4):4:0);
124200090610                  endif ;
124300090610             // causale contatto
124400090610                  clear  dCCO;
124500090610                  clear  TIBS02ds;
124600090610                  T02mod = 'C';
124700090610                  T02cod = 'CCO';
124800090610                  T02ke1 = atccad;
124900090610                  T02sif = KNSIF;
125000090610                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
125100090610                  if  T02err = *blanks;
125200090610                      dcco = t02uni ;
125300090714                      v02cau  = atccad  ;
125400090714                      v02caud = §ccodes ;
125500110405             // Verifico se causale su cliente che non spedisce
125600110405                      If §ccocns = 'S' ;
125700110405                         $cli_non_sped = *on ;
125800110405                      Endif;
125900090610                  endif;
126000091021          // verifico se affiancamento
126100091021                  affiancamento = (atccad = '92') ;
126200100616            //?cerco il commerciale dell'Appuntamento
126300100615                  IF  Affiancamento;
126400100616                    exec sql
126500100616                    SELECT ATCcmm into :app_ATCcmm from TIATL00F
126600100616                    JOIN TIATC00F on
126700100616                    ATLatl = ATCatn and ATLatlp = ATCatnp
126800100616                    WHERE ATLatn = :IMK19atn and ATLatnp = :IMK19atnp;
126900100616                    clear V02dcmma;
127000100616              //?trovo l'appuntamento decodifico il commerciale
127100100616                    IF  sqlcod = 0;
127200130809                      chain (app_ATCcmm) AZCMM000;
127300130809                      IF  %found(AZCMM01L);
127400130809                        V02dcmma = CMMdes;
127500100616                      ENDIF;
127600100616                    ENDIF;
127700100615                  ENDIF;
127800091021
127900090610                  v02cmi = %editc(atccmm : 'X') ;
128000090610         // decodifica del commerciale
128100130809                  clear V02cmiD;
128200130809                  chain (ATCcmm) AZCMM000;
128300130809                  if  %found(AZCMM01L);
128400130809                    v02cmid = CMMdes;
128500090610                  endif;
128600090703           // ora inizio impegno
128700090703                  v02oii = atcoii ;
128800090703           // ora fine   impegno
128900090703                  v02ofi = atcofi ;
129000090610           // numero trattativa
129100090610                  v02vis = atcnrv ;
129200090610           // cliente e decodifica
129300091026                  v02cli = atcksc ;
129400090610                  clear  tibs69ds;
129500091026                  I69kac = v02cli;
129600090610                  TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
129700091026                  v02clid = ACOrag;
129800090610           // note contatto PRECEDENTE
129900090610                clear trmk20ds ;
130000090610                imk20tla = 'L';
130100090610                imk20flm = 'R';
130200090610                imk20cpo = cpocpo ;
130300090610                imk20rsc = v02cpd ;
130400090611                imk20tat = imk19tat ;
130500090611                imk20atn = imk19atn ;
130600090611                imk20atnp= imk19atnp -1;
130700090616                Trmk20r ( kpjba : trmk20ds );
130800090610                v02no1o =  emk20no1 ;
130900090610                v02no2o =  emk20no2 ;
131000090610                piunote = (omk20piu = '+');
131100090610
131200090610             endif ;
131300090610          // se si tratta di un contatto già eseguito
131400090611             if Imk19fatt = 'S' ;
131500090610         //  proteggo i campi di input
131600090610                giafatto = *on ;
131700090610
131800090610             // verifco    si tratta di un 1° contatto se data scadenza = 0
131900090610                primocontatto = (v02scd = 0) ;
132000090610
132100140411               // -?Data/Ora/Utente attività eseguita?
132200140414               clear  Data_Eur;
132300140414               clear  wData;
132400140414               clear  wHhMm;
132500140414               if  ATCdco > *zero;
132600140414                 Data_Eur = %date( ATCdco : *iso );
132700140414                 wData    = %dec( Data_Eur );
132800140414               endif;
132900140414               wHhMm = %int( ATChco / 100 );
133000140414               %subst( V02tit2 : %len(V02tit2) - 27 ) =
133100140414                       %editc( wData : 'Y' ) + ' ' +
133200140414                       %editw( wHhMm : '  :  ' ) + ' ' +
133300140414                       ATCpru;
133400090610             // recupero i dati relativi all'esecuzione del contatto
133500090610                v02com = %editc(atccco:'X') ;
133600130809                clear  V02comD;
133700130809                chain (ATCcco) AZCMM000;
133800130809                if  %found(AZCMM01L);
133900130809                  V02comd = CMMdes;
134000090610                endif;
134100090610             // data contatto
134200090610                data_eur=%date(atcdco:*iso);
134300090610                v02dat=%dec(data_eur);
134400090610             // ora  contatto
134500090610                v02ora=%dec(%subst(%editc(atchco:'X'):1:4):4:0);
134600090610             // causale  contatto
134700090610                v02cae = atccac ;
134800090610                clear  dCCO;
134900090610                clear  TIBS02ds;
135000090610                T02mod = 'C';
135100090610                T02cod = 'CCO';
135200090610                T02ke1 = v02cae ;
135300090610                T02sif = KNSIF;
135400090610                TNTBE_RicercaControllo  (kpjba : tibs02ds);
135500090610                if  T02err = *blanks;
135600090610                    dcco = t02uni ;
135700090610                    V02caed = §ccodes ;
135800090610                endif;
135900100223             // se appuntamento visualizzo  l'esito
136000100223                if atctat = 'A' ;
136100100223                   v02fatto = atcest ;
136200100223                endif ;
136300091021             // verifico generata una nuova trattativa da questa attività
136400091021                tratta_new = (atcnra > 0) ;
136500091021                v02nra = atcnra ;
136600100309             // verifico se c'è l'esito
136700100309                datc01 = atcflo ;
136800100309                vis_esito = (§atcesito <> ' ') ;
136900100309                If §atcesito = 'P'  ;
137000100309                   v02esi = 'POSITIVO' ;
137100100309                else ;
137200100309                   v02esi = 'NEGATIVO' ;
137300100309                endif ;
137400090610             // recupero note
137500090610                clear trmk20ds ;
137600090610                imk20tla = 'L';
137700090610                imk20flm = 'R';
137800090610                imk20cpo = cpocpo ;
137900090610                imk20rsc = v02cpd ;
138000090611                imk20tat = imk19tat ;
138100090611                imk20atn = imk19atn ;
138200090611                imk20atnp= imk19atnp;
138300090616                Trmk20r ( kpjba : trmk20ds );
138400090610                v02no1 =  emk20no1 ;
138500090610                v02no2 =  emk20no2 ;
138600090610                altrenote = (omk20piu = '+');
138700090610             endif ;
138800090610
138900090610         else ;
139000090610             primocontatto = *on ;
139100090610         endif ;
139200090610         // propongo alcuni campi  se si tratta di un contatto ancora da eseguire
139300090623         // se non si tratta di visualizzazione
139400090623
139500091214         If  Not Visualizza and
139600091214             ((IMK19fatt = *blanks and $dasbf) or Not $dasbf);
139700091214         //?Se non è appuntamento
139800091214           IF Not Appuntamento;
139900090610             // commerciale chi sono
140000091214                If imk19com > *zeros;
140100090714                   v02com  = %editc(imk19com : 'X')  ;
140200090714                   v02comd = imk19comd ;
140300090714                endif ;
140400090610            //  data e ora del momento
140500090610                v02dat = *date ;
140600090610                v02ora = %dec(%subst(%EDITC(%dec(%time()):'X'):1:4):4:0);
140700091214         //?Se è appuntamento
140800091214           ELSE;
140900091214         //?imposto il commerciale, data e ora dell'appuntamento
141000091214         //?inoltre vengono protetti il commerciale e la data
141100091214             v02com  = v02cmi;
141200091214             v02comd = v02cmid;
141300091214             v02dat  = v02scd;
141400091214             v02ora  = v02sco;
141500110302           ENDIF;
141600091214         // flag giafatto a 0
141700091214           giafatto = *off ;
141800090610         endif ;
141900090714
142000090714        // verifico se visualizzare oppure no cliente e trattativa
142100090714
142200091026         cliente   = (v02cli > 0) ;
142300090714         vis_trattativ = (v02vis > 0) ;
142400091119
142500091119        // verifico se esistono trattativa e lo visualizzo
142600100118        // oltre a quella legata all'attività in carico
142700091119                   clear tnta43ds ;
142800091119                   ita43cpo = cpocpo ;
142900091119                   ita43ksc = v02cli ;
143000091119                   ita43nrv = v02vis ;
143100091119                   Tnta43r (tnta43ds) ;
143200100118            // se l'attività in carico non ha la trattativa
143300100118            // e mi torna un n. di trattativa imposto la scritta
143400100118            IF  v02vis = 0 and ota43nrv > 0;
143500100118              evalr v02segn = 'TRATTATIVE APERTE';
143600100118            ENDIF;
143700100118            // se l'attività in carico ha la trattativa
143800100118            // e mi torna un n. di trattativa imposto la scritta
143900100119            IF  v02vis > 0 and ota43nrv > 0 and v02vis <> ota43nrv;
144000100118              evalr v02segn = 'ALTRE TRATTATIVE APERTE';
144100100118            ENDIF;
144200091216
144300091216         //?Centro titolo basso
144400091216         clear wpos;
144500091216         IF  Not PrimoContatto;
144600091216         wpos = (%len(v02tit2) - %len(c_Attivita)) / 2;
144700091216         %subst(v02tit2:wpos:%len(c_Attivita)) = c_Attivita;
144800091216         ENDIF;
144900091216         IF  PrimoContatto;
145000091216         wpos = (%len(v02tit2) - %len(c_PrimaAtt)) / 2;
145100091216         %subst(v02tit2:wpos:%len(c_PrimaAtt)) = c_PrimaAtt;
145200091216         ENDIF;
145300091216
145400091216          If visualizza = *on ;
145500091216             Vis_nonfatto = (Imk19Fatt = ' ');
145600091216          else ;
145700091216             Vis_nonfatto = *off ;
145800091216          endif ;
145900091215
146000091215         //?verifico se per potenziale/cliente ci sono attività aperte
146100100118         //?ma solo se non è già impostato il campo con trattativa aperte
146200100118         IF  v02segn = ' ';
146300091215           clear TRMK85ds;
146400091215           IMK85cpo = cpocpo;
146500091215           IMK85ksc = v02cli;
146600100118           IMK85nrv = v02vis;
146700100129           IMK85atn = IMK19atn;
146800091215           trmk85r (trmk85ds);
146900100118         //?se torna errore <> "9" ho trovato delle attività aperte
147000091215           IF  OMK85err <> *blanks and OMK85err <> '9';
147100091215         //?imposto la costante a video
147200100118             evalr v02segn = 'ALTRE ATTIVITA'' APERTE';
147300091215           ENDIF;
147400091215         ENDIF;
147500091215
147600091214         //?verifico se appuntamento con Affiancamento
147700091214         ConAffianca = *off;
147800091214         F20Affianca = *off;
147900091214         IF  Appuntamento and Not Affiancamento;
148000091214           k_atlatl  = IMK19atn;
148100091214           k_atlatlp = IMK19atnp;
148200091214           chain  %kds(K02tiatl) TIATL02L;
148300091214           IF  %found(Tiatl02l);
148400091216             ConAffianca = *on;
148500091214           ENDIF;
148600091210         ENDIF;
148700091216
148800091216         //?Non abilito F20=Affiancamento in gestione
148900091216         IF  Appuntamento and Manutenzione;
149000091216           F20Affianca = *on;
149100091216         ENDIF;
149200091216         IF  ConAffianca and Visualizza;
149300091216           F20Affianca = *on;
149400091216         ENDIF;
149500100519
149600100519         //?Abilito F16=Annullamento
149700100519         F16Attivo = *off;
149800100519         $okdel = *off;
149900100519         SELECT;
150000110214         //?se codice attività impedisce l'annullamento dell'attività disabilito F16
150100110519         //?e il cliente è perso e non ha attività
150200110519           WHEN  §ccoann = 'S' and Manutenzione ;
150300110519                 If Cpofls <> 'P' or (Cpofls = 'P' and v02segn <> *blank) ;
150400110519                    F16Attivo = *on;
150500110519                    $okdel = *on;
150600110519                 Endif ;
150700100519         //?se sono in gestione attività richiamato da gestione trattativa
150800100520           WHEN  Manutenzione and Ric_Trattativ;
150900100519             F16Attivo = *on;
151000100519             $okdel = *on;
151100110908         //?se sono in gestione attività non legata a trattativa
151200110908         //?attività su potenziale perso e ci sono altre attività aperte
151300110908           WHEN  Manutenzione and not Vis_Trattativ and CPOfls = 'P' and
151400110908                 V02segn <> *blanks;
151500110908             F16Attivo = *on;
151600100519         //?se sono in gestione attività non legata a trattativa
151700100519         //?attività su potenziale mai codificato e ci sono altre attività aperte
151800110225           WHEN  Manutenzione and not Vis_Trattativ and CPOfls = 'M' and
151900110225                 V02segn <> *blanks;
152000100519             F16Attivo = *on;
152100100520         //?se sono in gestione attività non legata a trattativa
152200100520         //?attività su potenziale codificato
152300110225           WHEN  Manutenzione and not Vis_Trattativ and CPOfls = 'C';
152400100520             F16Attivo = *on;
152500100519         //?se sono in gestione attività non legata a trattativa
152600100519         //?attività su cliente
152700100520           WHEN  Manutenzione and not Vis_Trattativ and v02cli > 0;
152800100519             F16Attivo = *on;
152900100519         ENDSL;
153000150831
153100150831         // -?Se attività su Cliente visualizzo il tasto F4?
153200150831         F04Attivo = (IMK19ksc <> *zeros);
153300091216
153400091216         //?Se trattativa a video cerco la data della trattativa
153500091216         clear V02VisDat;
153600100504         clear w_vistpv ;
153700100614         clear V02VisTtr;
153800091216         IF  Vis_Trattativ;
153900100520           chain(n)  V02vis tivis05l;
154000091216           IF  %found(tivis05l);
154100091216             data_eur  = %date(visdat:*iso);
154200091216             V02VisDat = %dec(data_eur);
154300100504             w_vistpv = vistpv ;
154400100614         //?decodifico il tipo trattativa
154500100614             clear dTTR;
154600100614             clear TIBS02ds;
154700100614             T02mod = 'C';
154800100614             T02cod = 'TTR';
154900100614             T02ke1 = w_vistpv;
155000100614             T02sif = KNSIF;
155100100614             TNTBE_RicercaControllo  (kpjba : tibs02ds);
155200100614             IF  T02err = *blanks;
155300100614                 dTTR = t02uni;
155400100614                 V02VisTtr = §ttrdes;
155500100614             ENDIF;
155600091216           ENDIF;
155700091216         ENDIF;
155800091216
155900100114         PosCurFatto = *off;
156000091216         PosCurNo102 = *off;
156100091216         PosCurCom02 = *off;
156200091216
156300091216         //?Se non è manutenzione e non sto visualizzando un non fatto
156400100111         //?e non è interrogazione
156500100111         IF  Not Manutenzione and Not Vis_NonFatto and Not Visualizza;
156600100114           SELECT;
156700100114         //?Se il commerciale non è stato proposto posiziono il cursore sul commerciale
156800100114             WHEN  v02com = *zeros or V02com = *Blanks;
156900100114               PosCurCom02 = *on;
157000100114         //?Se è appuntamento mi posiziono sul 'Effettuato s/n'
157100100114             WHEN  Appuntamento;
157200100114               PosCurFatto = *on;
157300100114             OTHER;
157400100503         //?altrimenti posiziono il cursore sulla causale
157500100503               PosCurCae02 = *on;
157600100114           ENDSL;
157700091216         ENDIF;
157800100325
157900100325        // Recupero informazioni responsabile trasporti
158000100325
158100100325         exsr respD02 ;
158200091216
158300100310         $ForzaImpegno = *off;
158400100310         $ForzaComm    = *off;
158500100422         $ForzaComm3   = *off;
158600100310         $ForzaComin   = *off;
158700100318         $ForzaComin2  = *off;
158800100311         clear sav_v02cmi ;
158900100318         clear sav_v02cmi2;
159000100311         clear sav_w03com ;
159100100422         clear sav_w03comt;
159200090714
159300110225       //?Se sto gestendo un'attività ancora da fare
159400110225       //?e l'attività prevede il controllo del cliente "perso"
159500110301           IF  IMK19fatt <> 'S' and §CCOccp = 'S' and Gestione;
159600110527       // se attività 70 faccio un controllo se 71 un altro
159700110530             If  atccad = '71' ;
159800110225         //?Controllo se potenziale lavora o no
159900110527                 clear TRMK04DS;
160000110527                 clear TRMK85DS;
160100110527                 IMK04cpo = CPOcpo;
160200110527                 IMK85cpo = cpocpo;
160300110527                 IMK85ksc = v02cli;
160400110527                 IMK85nrv = v02vis;
160500110527                 IMK85atn = IMK19atn;
160600110527                 trmk04r (kpjba : TRMK04DS : TRMK85DS);
160700110225         //?Se il potenziale ha ripreso a lavorare devo emettere una win dove avviso
160800110527         //?di verificare le attività e trattative aperte
160900110527                 IF  OMK04lav = 'SI';
161000110527                    $Lavora = *on;
161100110527                    $InzD02 = *off;
161200110527                    $Video = 'W0';
161300110527                 ENDIF;
161400110527             ENDIF;
161500110527       // se attività 70 faccio un altro controllo
161600110530             If  atccad = '70' ;
161700110527         //?Controllo se potenziale lavora o no
161800110527                 clear TRMK04DS;
161900110530                 clear TRMK85DS;
162000110527                 IMK04cpo = CPOcpo;
162100110530                 trmk04r (kpjba : TRMK04DS : TRMK85DS);
162200110530         //?Se il potenziale ha ripreso a lavorare devo emettere una win nella quale
162300110530         //?avvisa che ci sono trattative aperte e annulla l'attività in corso
162400110530                 IF  OMK04lav = 'SI';
162500110527                    $Lavora = *on;
162600110527                    $InzD02 = *off;
162700110527                    $Video = 'W9';
162800110527                 ENDIF;
162900110527             ENDIF;
163000110225           ENDIF;
163100110214
163200090610       ENDSR;
163300090610
163400090610       //--------------------------------------------------------------
163500090610       //?Recupero dati anagrafici / resp / mail / stato potenziale
163600090610       //--------------------------------------------------------------
163700090610       BEGSR RecD02;
163800090610
163900090610
164000091019        // Recupero dati anagrafico potenziale o cliente in base ai dati passati
164100090610
164200091021         exsr AnagD02 ;
164300090610
164400110502              $Gia_Codifica = *off ;
164500100322           // verifico se codificato su altri codici
164600110502           //   chain cpocpo  CNACO16L;
164700110502           //    IF  %found(CNACO16L);
164800110502           //        $Gia_Codifica = *on ;
164900110502           //     endif ;
165000110502           // considero un potenziale già codificato se il falf del potenziale è uguale a "C"
165100110502              If cpofls = 'C' ;
165200110502                 $gia_codifica = *on ;
165300110502              Endif ;
165400110502           // decodifico la categoria del potenziale
165500110502                  clear  dCPO;
165600110502                  clear  TIBS02ds;
165700110502                  T02mod = 'C';
165800110502                  T02cod = 'CPO';
165900110502                  T02ke1 = cpofls ;
166000110502                  T02sif = KNSIF;
166100110502                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
166200110502                  if  T02err = *blanks;
166300110502                      dcpo = t02uni ;
166400110502                  endif;
166500090610
166600090610
166700090610       ENDSR;
166800090610
166900090610       //--------------------------------------------------------------
167000091021       //?Recupero dati potenziale/cliente
167100090610       //--------------------------------------------------------------
167200091021       BEGSR AnagD02 ;
167300091020       // se esite il codice cliente recupero i dati anagrafici da lui
167400091020             If v02cli <> *zeros ;
167500091021                cliente = *on ;
167600091020                clear  tibs69ds;
167700091020                I69kac = v02cli;
167800091020                I69kin = v02cli;
167900091020                I69kcp = v02cli;
168000091020                TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
168100091020                v02clid = acorag ;
168200091020                cpocpo  = acolib ;
168300091020                v02cpi = indvia ;
168400091020                v02cpl = indcit ;
168500091020                v02cpt = indtel ;
168600091020                v02cpf = indtlf ;
168700091020                v02cpc = %editc(indcap:'X') ;
168800091020                v02cpp = indprv ;
168900091020                v02cpn = indsta ;
169000091020                v02cmp = clpage ;
169100091020             endif ;
169200091020
169300120622          clear dCPO01;
169400090610             chain(N)  cpocpo  TNCPO000;
169500140715             chain     cpocpo  TNCPO100;
169600140715             if  NOT %found(TNCPO11L);
169700140715               clear  CPO1tel;
169800140715               clear  CPO1fax;
169900140715             endif;
170000090610       // carico dati del potenziale
170100090610             v02cpo = %editc(cpocpo:'X');
170200090610             v02cpd = cporag ;
170300120622             dCPO01 = CPOrst;
170400091020       // se non esite il codice cliente recupero i dati anagrafici dal potenziale
170500091020             If v02cli =  *zeros ;
170600091021                cliente = *off;
170700091020                v02cpi = cpovia ;
170800091020                v02cpl = cpocit ;
170900140715                v02cpt = CPO1tel;
171000140715                v02cpf = CPO1fax;
171100091020                v02cpc = cpocap ;
171200091020                v02cpp = cpoprv ;
171300091020                v02cpn = cponaz ;
171400091020                v02cmp = cpocmm ;
171500091020             endif ;
171600090610
171700090610
171800091020        // recupero dedodifica dle commerciale
171900130809         clear  V02cmpD;
172000130809         chain (v02cmp) AZCMM000;
172100130809         if  %found(AZCMM01L);
172200130809           V02cmpd = CMMdes;
172300090610         endif;
172400090610
172500090610         ENDSR ;
172600090610
172700090610       //--------------------------------------------------------------
172800090610       //?Recupero responsabile trasporti e mail
172900090610       //--------------------------------------------------------------
173000090610       BEGSR RespD02 ;
173100090610
173200100511             clear v02rtr ;
173300100511             clear v02mrt ;
173400090610       // carico responsabile trasporti
173500100326             SELECT ;
173600100326
173700100326       // se c'è il cliente recupero dal cliente
173800100408             when v02cli > 0 ;
173900100326                 clear ntcnk1 ;
174000100326                 ntcapl = 'C' ;
174100100408                 ntcnk1 = %editc(dutkci: 'X') + %editc(v02cli:'X');
174200100329       // se aperta trattativa recupero da questa
174300100408             when v02nra > 0 ;
174400100329                 clear ntcnk1 ;
174500100329                 ntcapl = 'T' ;
174600100408                 ntcnk1 = %editc(dutkci: 'X') + %editc(v02nra:'X');
174700100326       // se c'è trattativa recupero da questa
174800100408             when v02vis > 0 ;
174900100326                 clear ntcnk1 ;
175000100326                 ntcapl = 'T' ;
175100100408                 ntcnk1 = %editc(dutkci: 'X') + %editc(v02vis:'X');
175200100326             other ;
175300100325                 clear ntcnk1 ;
175400100325                 ntcapl = 'P' ;
175500100325                 ntcnk1 = %editc(cpocpo:'X');
175600100326             endsl ;
175700100326
175800090610             ntctnt = '05';
175900090610             clear  ntcnk2 ;
176000091020          setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
176100091020          READE    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
176200090610          dow not %eof(tfntc01l)  ;
176300100329             vh2rtr = ntcrnt ;
176400090610             v02rtr = ntcrnt ;
176500091020             READE    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
176600090610             If  not %eof(tfntc01l) ;
176700090610               v02rtr = %trim(v02rtr) + ', ' + %trim(ntcrnt) ;
176800090610               leave ;
176900090610             else;
177000090610               leave ;
177100090610             endif ;
177200090610          enddo ;
177300090610
177400090610       // carico mail responsabile trasporti
177500090610             ntctnt = '06';
177600090610             clear  ntcnk2 ;
177700091020          chain    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
177800090610          if  %found(tfntc01l)  ;
177900090610             v02mrt = ntcrnt ;
178000090610          endif ;
178100090610
178200090610       ENDSR;
178300090610
178400090611       //--------------------------------------------------------------
178500090611       //?Controllo videata D02
178600090611       //--------------------------------------------------------------
178700090611       BEGSR CtrD02;
178800090611
178900090611            WindDspF = IndDspF;
179000090611            reset WindDspFb;
179100090611            IndDspF  = WindDspF;
179200090611
179300100308            $chiedi_conf = *off ;
179400090611
179500100721         // verifico se nel frattempo hanno già esitato l'attività
179600100721                UNLOCK TIATC01L;
179700100721                chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
179800100721                IF %FOUND(TIATC01L) AND ATCDCO > 0 ;
179900100721                   errMessage  = *on;
180000100721                   errGenerico = *on;
180100100721                   PosCurCom02 = *on;
180200100721                   V1Dmsg = $Msg(07);
180300100721                   LEAVESR ;
180400100721                ENDIF ;
180500090611         // Se sono in manutenzione devo fare i controlli per la sola parte realtiva alla
180600090611         // scadenza dell'attività
180700090611
180800090611           If manutenzione = *on ;
180900090611              exsr Ctrd02Scd ;
181000090611              leavesr;
181100090611           endif ;
181200090611
181300090611         // Controllo codice commerciale (? Ricerca)
181400090611           If    %scan('?' : v02com) > 0  ;
181500090611             errGenerico = *on;
181600090611             PosCurCom02 = *on;
181700130809             clear  TRMK43ds;
181800130809             iMK43fil  = DUTpou;
181900130809             TRMK43R (kpjba : TRMK43ds);
182000130809             if  oMK43err = *off  and  oMK43fxx = *blank;
182100130809               v02com  = %editc( oMK43cmm : 'X' );
182200130809               v02comd = oMK43des;
182300130809             endif;
182400090611             leavesr;
182500090611           endif;
182600090611
182700100210         // Commerciale che esegue il contatto  obbligatorio
182800100210           If  v02com = *blanks or v02com = *zeros;
182900100210             errMessage  = *on;
183000100210             errGenerico = *on;
183100100210             PosCurCom02 = *on;
183200100210             V1Dmsg = $Msg(02);
183300100210             leavesr;
183400100210           endif;
183500100210
183600100210         // Controllo commerciale che esegue il contatto
183700100210             IF  %check(digits:V02com) > 0;
183800100210               ErrMessage  = *on;
183900100210               ErrGenerico = *on;
184000100210               PosCurCom02 = *on;
184100100210               V1Dmsg      = $Msg(02);
184200100210               leavesr;
184300100210             ENDIF;
184400100210
184500100210           //?utente abilitato al commerciale
184600100210             clear TNTAA1DS;
184700120810             ITAA1tipo = 'M';
184800120810             ITAA1caut = '§utegtc';
184900100210             ITAA1cmm  = %dec(V02com:7:0);
185000100519             ITAA1abi = 'RA';
185100100210             kpjbu     = tntaa1ds;
185200100210             tntaa1c (kpjba);
185300100210             TNTAA1DS = kpjbu;
185400100210             IF  OTAA1fabi = 'N';
185500100210               ErrMessage  = *on;
185600100210               ErrGenerico = *on;
185700100210               PosCurCom02 = *on;
185800100210               V1Dmsg      = $Msg(02);
185900100210               leavesr;
186000100210             ENDIF;
186100100210
186200130809             CMMcod = %int(V02com);
186300130809             chain (CMMcod) AZCMM000;
186400130809             if  NOT  %found(AZCMM01L);
186500090611               errMessage  = *on;
186600090611               errGenerico = *on;
186700090611               PosCurCom02 = *on;
186800090611               V1Dmsg = $Msg(02);
186900090611               leavesr;
187000100210             endif;
187100130809             V02comd = CMMdes;
187200100416
187300110525
187400130809             IF  CMMPAR <> ' ' ;
187500110525               ErrMessage  = *on;
187600110525               ErrGenerico = *on;
187700110525               PosCurCom02 = *on;
187800110525               V1Dmsg      = $Msg(02);
187900110525              leavesr;
188000110525             ENDIF;
188100111223
188200111223       //?Commerciale "valido" data inizio e fine attività
188300130809         IF  CMMdtIni > wOggi  or
188400130809             CMMdtFin < wOggi;
188500111223           ErrMessage = *on;
188600111223           ErrGenerico = *on;
188700111223           PosCurCom02 = *on;
188800111223           V1Dmsg = $Msg(02);
188900111223           leavesr;
189000111223         ENDIF;
189100110525
189200100416         // Se appuntamento devo controllare anche se è stato inserito il FATTO
189300100416           IF  Appuntamento and V02fatto = *blanks;
189400100416             ErrGenerico = *on;
189500100416             ErrMessage  = *on;
189600100416             PosCurFatto = *on;
189700100416             V1Dmsg      = $msg(21);
189800100416             leavesr;
189900100416           ENDIF;
190000100416
190100090611
190200100601         // controllo data esecuzione contatto solo se non sono in appuntamento non eseguito
190300090611           If    V02dat <> 0;
190400090611             clear wlbdat;
190500090611             g02dat = v02dat;
190600090611             xsrda8(wlbdat);
190700090611             if g02err = '1';
190800090611               errMessage  = *on;
190900090611               errGenerico = *on;
191000090611               PosCurDat02 = *on;
191100090611               V1Dmsg = $Msg(03);
191200090611               leavesr;
191300090611             endif;
191400090611
191500090611             v02dat = g02dat;
191600090611             w02dat = g02inv;
191700090611           else ;
191800090611             errMessage  = *on;
191900090611             errGenerico = *on;
192000090611             PosCurDat02 = *on;
192100090611             V1Dmsg = $Msg(03);
192200090611             leavesr;
192300090611           endif;
192400100118
192500100601           If  (appuntamento and v02fatto =  'S') or appuntamento = *off ;
192600100118         // controllo che la data non sia inferiore al 2000 o superiore al 2039
192700100118         IF  w02dat < 20000101 or w02dat > 20391231;
192800100118           errMessage  = *on;
192900100118           errGenerico = *on;
193000100118           PosCurDat02 = *on;
193100100118           V1Dmsg = $Msg(03);
193200100118           leavesr;
193300100118         ENDIF;
193400090611
193500100330       // controllo che la data non sia maggiore di oggi ed inferiore a 3 gg lavorativi da oggi
193600100330         clear XGIOLAVDS;
193700100330         IXGLdata = %dec(%date());
193800100330         IXGLsub  = 'S';
193900100330         IXGLgg   = 3;
194000100330         IXGLpa   = 'P';
194100100330         IXGLlav  = 'S';
194200100330         IXGLfil = %dec(%subst(V02com:1:3):3:0);
194300100330         XGIOLAV (xgiolavds);
194400100330         IF  w02dat < OXGLdata;
194500100330           ErrMessage  = *on;
194600100330           ErrGenerico = *on;
194700100330           PosCurDAT02 = *on;
194800100330           V1dmsg      = $Msg(03);
194900100330           V1dmsg      = %trim(V1dmsg) + ' ' +
195000100330           'Non può essere inferiore a 3 gg.lavorativi';
195100100330           leavesr;
195200100330         ENDIF;
195300100330
195400100330         // controllo con la data del giorno
195500100330            data_iso = (%date()) ;
195600100408         IF  w02dat > %dec(data_iso) ;
195700100330           ErrMessage  = *on;
195800100330           ErrGenerico = *on;
195900100330           PosCurDAT02 = *on;
196000100330           V1dmsg      = $Msg(03);
196100100330           V1dmsg      = %trim(V1dmsg) + ' ' +
196200100330           'Non può essere maggiore di oggi';
196300100330           leavesr;
196400100330         ENDIF;
196500100330
196600090611         // controllo ora  esecuzione contatto
196700090611           If V02ora <> 0 ;
196800090611              sav02ora = %editc(v02ora:'X');
196900120105              If  ora > 23 or minuti > 59;
197000090611                   errMessage  = *on;
197100090611                   errGenerico = *on;
197200090611                   PosCurora02 = *on;
197300090611                   V1Dmsg = $Msg(05);
197400090611                   leavesr;
197500090611              endif;
197600091104           else ;
197700091104              errMessage  = *on;
197800091104              errGenerico = *on;
197900091104              PosCurora02 = *on;
198000091104              V1Dmsg = $Msg(05);
198100091104              leavesr;
198200090611           endif;
198300100114
198400100416           endif ;
198500100114
198600091116
198700090611         // - controllo / ricerca Causale contatto
198800090611           If    %scan('?' : V02cae) > *zero;
198900090611             errGenerico = *on;
199000090611             PosCurCae02 = *on;
199100090611             clear  dCCO ;
199200090611             clear  Tntb74ds;
199300091218             iccouti = 'S';
199400100225          // imposto anche il tipo attività che sto eseguendo x selezionare le sole
199500100225          // causali possibili
199600100225             iccoese = imk19tat ;
199700100322          // imposto anche se sto eseguendo attività con il solo codice cliente oppure
199800100322          // potenziale mai codificato , e se c'è trattativa
199900110429          //   If v02cli > 0 ;
200000110429          //      iccocod = 'C' ;
200100110429          //   else ;
200200110429          //      if $Gia_codifica = *on ;
200300110429          //         iccocod = 'C' ;
200400110429          //      else ;
200500110429          //         iccocod = 'P' ;
200600110429          //      endif ;
200700110429          //   endif;
200800100322
200900110429          // imposto la cateoria del potenziale
201000110502               iccocod = cpofls ;
201100110429
201200100322             If v02vis > 0 ;
201300100322                iccotra = 'S' ;
201400100505                iccotpv = W_vistpv ;
201500100322             else ;
201600100322                iccotra = 'N' ;
201700100322             endif ;
201800100324           // verifico le offerte
201900100330             iccooff = 'N';
202000101220             setll v02vis tivof11l ;
202100101220             reade v02vis tivof11l ;
202200101220             dow not %eof(tivof11l) ;
202300100421                 if vofeso <> 'H' and vofeso <> '*';
202400100330                    iccooff = 'S';
202500100330                    leave ;
202600100330                 endif ;
202700101220                 reade v02vis tivof11l ;
202800100330             enddo ;
202900110412           // passo la causale al pgm di ricerca
203000100421                iccocco = atccad ;
203100100322
203200090611             TNTB74R  (kpjba : tntb74ds);
203300090611             if  occoerr = *off   ;
203400090611               V02cae = Occoke1;
203500091218               dcco   = Occouni;
203600090611               V02caed = §ccodes ;
203700090611               errGenerico = *on;
203800090611               leavesr;
203900090611             else;
204000090611               errMessage  = *on;
204100090611               errGenerico = *on;
204200090611               V1Dmsg = T02msg;
204300090611               leavesr;
204400090611             endif;
204500090611           endif;
204600090611
204700090611           If    V02cae <> *blanks  ;
204800090611             clear  dCCO;
204900090611             clear  TIBS02ds;
205000090611             T02mod = 'C';
205100090611             T02cod = 'CCO';
205200090611             T02ke1 = v02cae ;
205300090611             T02sif = KNSIF;
205400090611             TNTBE_RicercaControllo  (kpjba : tibs02ds);
205500090611             if  T02err = *blanks;
205600090611               dcco = t02uni ;
205700100225               §ccoTAAds = §CCOtaa;
205800100505               §ccoTPVds = §CCOtpv;
205900110502               §ccoCPOds = §CCOcpo;
206000090611               V02caed = §ccodes ;
206100090702           // verifico se causale contatto può essere utilizzata dall'utente
206200090702                If   §ccouti = 'N' ;
206300090702                   errMessage  = *on;
206400090702                   errGenerico = *on;
206500090702                   PosCurCae02 = *on;
206600090702                   V1Dmsg = $msg(14);
206700090702                   leavesr;
206800090702                endif;
206900090611             else;
207000090611               errMessage  = *on;
207100090611               errGenerico = *on;
207200090611               PosCurCae02 = *on;
207300090611               V1Dmsg = T02msg;
207400090611               leavesr;
207500090611             endif;
207600090611           else;
207700090611             errMessage  = *on;
207800090611             errGenerico = *on;
207900090611             PosCurCae02 = *on;
208000090611             V1Dmsg = $msg(08);
208100090611             leavesr;
208200090611           endif;
208300110405
208400110405           // verifico se causale può essere utilizzata in base al flag cliente che non spedisce
208500110405           If  $cli_non_sped = *on and §CCOucns = 'N' ;
208600110405                   errMessage  = *on;
208700110405                   errGenerico = *on;
208800110405                   PosCurCae02 = *on;
208900110502                   IF ATCcad <> '  ' ;
209000110502                      V1Dmsg = $msg(36);
209100110502                      %subst(V1Dmsg:12:2) = V02cae;
209200110502                      %subst(V1Dmsg:66:2) = ATCcad;
209300110502                   else ;
209400110502                      V1Dmsg = $msg(14);
209500110502                   Endif ;
209600110405                   leavesr;
209700110405           endif;
209800110502
209900110502           // verifico se causale può essere utilizzata in base al flag cliente che non spedisce
210000110502           If  $cli_non_sped = *off and §CCOucns = 'S' ;
210100110502                   errMessage  = *on;
210200110502                   errGenerico = *on;
210300110502                   PosCurCae02 = *on;
210400110502                   IF ATCcad <> '  ' ;
210500110502                      V1Dmsg = $msg(36);
210600110502                      %subst(V1Dmsg:12:2) = V02cae;
210700110502                      %subst(V1Dmsg:66:2) = ATCcad;
210800110502                   else ;
210900110502                      V1Dmsg = $msg(14);
211000110502                   Endif ;
211100110502                   leavesr;
211200110502           endif;
211300100421
211400110321         //?Controllo se la causale di esecuzione può essere utilizzata per l'attività che sto
211500110321         //?gestendo
211600110321           IF  §CCOcae <> *blanks and §CCOcae <> ATCcad;
211700110321             ErrMessage  = *on;
211800110321             ErrGenerico = *on;
211900110321             PosCurCae02 = *on;
212000110502             IF ATCcad <> '  ' ;
212100110502                V1Dmsg = $msg(36);
212200110502                %subst(V1Dmsg:12:2) = V02cae;
212300110502                %subst(V1Dmsg:66:2) = ATCcad;
212400110502             else ;
212500110502                V1Dmsg = $msg(14);
212600110502             Endif ;
212700110321             leavesr;
212800110321           ENDIF;
212900100421
213000110428           // verifico se causale da esitare è la 32 deve usare solo la 42
213100110428              If atccad = '32' and v02cae <> '42' ;
213200110428                errMessage  = *on;
213300110428                errGenerico = *on;
213400110428                PosCurCae02 = *on;
213500110428                V1Dmsg = $msg(33);
213600110428                leavesr;
213700110428              endif ;
213800110428
213900110428           // verifico se causale da esitare è la 71 deve usare solo la 72
214000110428              If atccad = '71' and v02cae <> '72' ;
214100110428                errMessage  = *on;
214200110428                errGenerico = *on;
214300110428                PosCurCae02 = *on;
214400110502                IF ATCcad <> '  ' ;
214500110502                   V1Dmsg = $msg(36);
214600110502                   %subst(V1Dmsg:12:2) = V02cae;
214700110502                   %subst(V1Dmsg:66:2) = ATCcad;
214800110502                else ;
214900110502                   V1Dmsg = $msg(14);
215000110502                Endif ;
215100110428                leavesr;
215200110428              endif ;
215300110428
215400100225           // verifico se causale può essere utilizzata per esitare questo tipo attività
215500100225           If  §CCOtaa <> *blanks and
215600100225                 %lookup(imk19tat : $Taa) = *zero ;
215700100225                   errMessage  = *on;
215800100225                   errGenerico = *on;
215900100225                   PosCurCae02 = *on;
216000100225                   V1Dmsg = $msg(23);
216100100225                   leavesr;
216200100225           endif;
216300100322
216400100322           // verifico se causale può essere utilizzata in presenza di trattative con offerte
216500100322           If  §CCOoff = 'S' and v02vis > 0 ;
216600101220                   setll v02vis tivof11l ;
216700100322                   If not %equal ;
216800100322                      errMessage  = *on;
216900100322                      errGenerico = *on;
217000100322                      PosCurCae02 = *on;
217100100322                      V1Dmsg = $msg(31);
217200100322                      leavesr;
217300100421                   else ;
217400101220                      reade v02vis tivof11l ;
217500101220                      dow not %eof(tivof11l) ;
217600100421                          if vofeso  =  '*' ;
217700100421                             errMessage = *on ;
217800100421                          else ;
217900100421                             errMessage = *off;
218000100421                             leave ;
218100100421                          endif ;
218200101220                      reade v02vis tivof11l ;
218300100421                      enddo ;
218400100421                   // se ho trovato tutti annullati emetto errore
218500100421                      If errMessage  = *on;
218600100421                         errGenerico = *on;
218700100421                         PosCurCae02 = *on;
218800100421                         V1Dmsg = $msg(31);
218900100421                         leavesr;
219000100421                      endif ;
219100100322                   endif ;
219200100322           endif;
219300100322
219400100322           // verifico se causale non può essere utilizzata in presenza di trattative con offerte
219500100225           If  §CCOoff = 'N' and v02vis > 0 ;
219600101220                   setll v02vis tivof11l ;
219700101220                   reade v02vis tivof11l ;
219800101220                   dow not %eof(tivof11l) ;
219900100421                       if vofeso <> 'H' and vofeso <> '*' ;
220000100309                          errMessage  = *on;
220100100309                          errGenerico = *on;
220200100309                          PosCurCae02 = *on;
220300100309                          V1Dmsg = $msg(24);
220400100309                          leavesr;
220500100309                       endif ;
220600101220                   reade v02vis tivof11l ;
220700100309                   enddo ;
220800100225           endif;
220900100505           // verifico se causale in presenza di trattativa può essere usata per solo un
221000100505           // alcuni tipi trattativa
221100100505           If      §ccotpv <> *blanks  and %lookup(W_vistpv : $tpv) = *zero ;
221200100505                          errMessage  = *on;
221300100505                          errGenerico = *on;
221400100505                          PosCurCae02 = *on;
221500100505                          V1Dmsg = %trimr($msg(35)) + ' ' + W_vistpv ;
221600100505                          leavesr;
221700100505           endif ;
221800100308
221900100308           // verifico se causale può essere utilizzata solo per codificati/mai codificati o tutti
222000100308
222100100308           // solo causali per clienti codificati
222200110502           // If  §CCOcod = 'C' and V02cli = *zeros ;
222300110502           If  §CCOcpo <> *blanks and %lookup(cpofls:$cpo)= 0 ;
222400100308               errMessage  = *on;
222500100308               errGenerico = *on;
222600100308               PosCurCae02 = *on;
222700100308               V1Dmsg = $msg(25);
222800110504               v1dmsg = %subst(V1Dmsg:  1: 40) + '"' + §cpodesc + '"' ;
222900100308               leavesr;
223000100308           endif ;
223100090611
223200100308
223300100308           // verifico se causale non deve essere legata a trattativa
223400100308           If  §CCOtra = 'N' and V02vis >  *zeros ;
223500100308               errMessage  = *on;
223600100308               errGenerico = *on;
223700100308               PosCurCae02 = *on;
223800100308               V1Dmsg = $msg(27);
223900100308               leavesr;
224000100308           endif;
224100100308
224200100309            // verifico se causale chiude trattativa e l'attività è legata a trattativa
224300100309             if  §ccoCHT = 'S' and v02vis > 0 ;
224400100520                   chain(n)  V02vis tivis05l;
224500100308                   // se trattativa già esitata errore
224600100308                   IF  %found(tivis05l) and visesi = 'P ' ;
224700100308                       errMessage  = *on;
224800100308                       errGenerico = *on;
224900100308                       PosCurCae02 = *on;
225000100308                       V1Dmsg = $msg(28);
225100100308                       leavesr ;
225200100308                   else ;
225300100308                       $chiedi_conf = *on ;
225400100308                   endif ;
225500100308             endif ;
225600090611
225700091118            // pulisco il flag della trattativa nuova se non devo aprire trattativa
225800091118                if §ccoapt = ' ' ;
225900091118                    $new_trattati = *off ;
226000091118                endif ;
226100100308            // verifico se causale vuole le note
226200100308            If §cconot = 'S' and v02no1 = *blanks and v02no2 = *blanks ;
226300100308                   errMessage  = *on;
226400100308                   errGenerico = *on;
226500100308                   PosCurNo102 = *on;
226600100308                   V1Dmsg = $msg(29);
226700100308                   leavesr;
226800100308            endif ;
226900100318
227000100318            // verifico se causale non deve essere legata a trattativa do errore
227100100520                if    §ccotra = 'S' and v02vis = 0 and §ccoapt <> 'S';
227200100318                   errMessage  = *on;
227300100318                   errGenerico = *on;
227400100318                   PosCurCae02 = *on;
227500100318                   V1Dmsg = $msg(30);
227600100318                   leavesr;
227700100318                endif ;
227800100318
227900100329
228000100329            // verifico se causale legata a trattativa, se non è un'attività con trattativa
228100100329            // la creo nuova
228200100329                if    (§ccotra = 'S' and v02vis = 0) or
228300100329                      (§ccotra <>'S' and v02vis = 0) ;
228400100329                  clear newnrv;
228500100329                  clear sav_newnrv;
228600100329                endif ;
228700100329
228800100318            // verifico se causale legata a trattativa, se non è un'attività con trattativa
228900100318            // la creo nuova altrimenti propongo la trattativa dell'attività
229000100318                if    §ccotra = 'S' and v02vis > 0 ;
229100100318                      newnrv = v02vis   ;
229200100318                      sav_newnrv = v02vis   ;
229300100318                      leavesr;
229400100318                endif ;
229500100118
229600100118            // verifico se causale apre sempre trattativa e se ce nè già do errore
229700100118                if    §ccoapt = 'S' and v02vis > 0 ;
229800100118                   errMessage  = *on;
229900100118                   errGenerico = *on;
230000100118                   PosCurCae02 = *on;
230100100118                   V1Dmsg = $msg(19);
230200100118                   leavesr;
230300100118                endif ;
230400100118
230500100118            // se passati tutti i controlli e devo aprire trattativa recupero
230600100118            // il numero trattativa
230700100118            // se non avevo già salvato prima il n. recuperato
230800100202            if newnrv = 0 and §ccoapt = 'S'  and sav_newnrv = 0;
230900091113            exsr Sr_Recnum ;
231000091113            if  $ErrGrave = *on;
231100091113                leavesr;
231200091113            endif;
231300091113            endif;
231400100118
231500100118            if sav_newnrv > 0 and §ccoapt = 'S';
231600100118              newnrv = sav_newnrv;
231700100118              $new_trattati = sav_newtrat;
231800100118            endif;
231900100118
232000090611       ENDSR;
232100090611
232200091113       //--------------------------------------------------------------
232300091113       //?Recupero numero trattativa nuova
232400091113       //--------------------------------------------------------------
232500091113       BEGSR sr_Recnum;
232600091113
232700100413                reset $NumOk ;
232800100413
232900100413                RESET trul48ds;
233000100413                trul48r(kpjba:trul48ds);
233100100413
233200100413                If o48Err = 0;
233300100413                   newnrv  = o48nrv;
233400100413                   sav_newnrv  = o48nrv;
233500100413                   $NumOk = *on ;
233600100413                else ;
233700100413                   $ErrGrave   = *on;
233800100413                   errMessage  = *on;
233900100413                   errGenerico = *on;
234000100413                   V1Dmsg = o48msg ;
234100100413                   leavesr;
234200100413                Endif;
234300100413
234400091116        // se trovato un nuovo numero lo segnalo
234500091116                 If   $NumOk = *on ;
234600091116                   $New_trattati = *on ;
234700100118                   sav_newtrat = *on ;
234800091116                 endif ;
234900091113
235000091113       endsr;
235100091113
235200090611       //--------------------------------------------------------------
235300090611       //?Controllo videata D02         SCADENZA ATTIVITA'
235400090611       //--------------------------------------------------------------
235500090611       BEGSR CtrD02Scd ;
235600090611
235700090611         // controllo data attività da fare
235800090611           If    V02scd <> 0;
235900090611             clear wlbdat;
236000090611             g02dat = v02scd;
236100090611             xsrda8(wlbdat);
236200090611             if g02err = '1';
236300090611               errMessage  = *on;
236400090611               errGenerico = *on;
236500090611               PosCurScd02 = *on;
236600090611               V1Dmsg = $Msg(03);
236700090611               leavesr;
236800090611             endif;
236900090611
237000090611             v02Scd = g02dat;
237100090611             w02Scd = g02inv;
237200090611           else ;
237300090611             errMessage  = *on;
237400090611             errGenerico = *on;
237500090611             PosCurScd02 = *on;
237600090611             V1Dmsg = $Msg(03);
237700090611             leavesr;
237800090611           endif;
237900100118
238000100118         // controllo che la data non sia inferiore al 2000 o superiore al 2039
238100100118         IF  w02scd < 20000101 or w02scd > 20391231;
238200100118           errMessage  = *on;
238300100118           errGenerico = *on;
238400100118           PosCurscd02 = *on;
238500100118           V1Dmsg = $Msg(03);
238600100118           leavesr;
238700100118         ENDIF;
238800090611
238900090611         // controllo ora  esecuzione contatto
239000090611           If V02Sco <> 0 ;
239100090611              sav02ora = %editc(v02Sco:'X');
239200120105              If  ora > 23 or minuti > 59;
239300090611                   errMessage  = *on;
239400090611                   errGenerico = *on;
239500090611                   PosCurSco02 = *on;
239600090611                   V1Dmsg = $Msg(05);
239700090611                   leavesr;
239800090611              endif;
239900090611           endif;
240000090611
240100090611         // Controllo codice commerciale (? Ricerca)
240200090611           If    %scan('?' : v02cmi) > 0  ;
240300090611             errGenerico = *on;
240400090611             PosCurCmi02 = *on;
240500130809             iMK43fil  = DUTpou;
240600130809             TRMK43R (kpjba : TRMK43ds);
240700130809             if  oMK43err = *off  and  oMK43fxx = *blank;
240800130809               v02cmi  = %editc( oMK43cmm : 'X' );
240900130809               v02cmid = oMK43des;
241000130809             endif;
241100090611             leavesr;
241200090611           endif;
241300100210
241400100210         // Commerciale che esegue il contatto obbligatorio
241500100210           If  v02cmi = *blanks or v02cmi = *zeros;
241600100210             errMessage  = *on;
241700100210             errGenerico = *on;
241800100210             PosCurCmi02 = *on;
241900100210             V1Dmsg = $Msg(02);
242000100210             leavesr;
242100100210           endif;
242200090611
242300090611         // Controllo codice commerciale che esegue il contatto
242400100210             IF  %check(digits:V02cmi) > 0;
242500100210               ErrMessage  = *on;
242600100210               ErrGenerico = *on;
242700100210               PosCurCmi02 = *on;
242800100210               V1Dmsg      = $Msg(02);
242900100210               leavesr;
243000100210             ENDIF;
243100100210
243200100210
243300100210           //?utente abilitato al commerciale
243400100721           //?lo controllo solo se attività NON legata a trattative e NON su cliente
243500100721             IF  V02vis <> 0 or V02cli <> 0;
243600100210             clear TNTAA1DS;
243700120810             ITAA1tipo = 'M';
243800120810             ITAA1caut = '§utegtc';
243900100210             ITAA1cmm  = %dec(V02cmi:7:0);
244000100519             ITAA1abi = 'RA';
244100100210             kpjbu     = tntaa1ds;
244200100210             tntaa1c (kpjba);
244300100210             TNTAA1DS = kpjbu;
244400100210             IF  OTAA1fabi = 'N';
244500100210               ErrMessage  = *on;
244600100210               ErrGenerico = *on;
244700100210               PosCurCmi02 = *on;
244800100210               V1Dmsg      = $Msg(02);
244900100210               leavesr;
245000100210             ENDIF;
245100100721             ENDIF;
245200100210
245300130809             CMMcod = %int(V02cmi);
245400130809             chain (CMMcod) AZCMM000;
245500130809             if  NOT  %found(AZCMM01L);
245600090611               errMessage  = *on;
245700090611               errGenerico = *on;
245800090611               PosCurCmi02 = *on;
245900090611               V1Dmsg = $Msg(02);
246000090611               leavesr;
246100100210             endif;
246200130809             V02cmid = CMMdes;
246300110525
246400130809             If  CMMPar <> ' ' ;
246500110525               ErrMessage  = *on;
246600110525               ErrGenerico = *on;
246700110525               PosCurCmi02 = *on;
246800110525               V1Dmsg      = $Msg(02);
246900110525              leavesr;
247000110525             ENDIF;
247100111223
247200111223       //?Commerciale "valido" data inizio e fine attività
247300130809         IF  CMMdtIni > wOggi  or
247400130809             CMMdtFin < wOggi;
247500111223           ErrMessage = *on;
247600111223           ErrGenerico = *on;
247700111223           PosCurCmi02 = *on;
247800111223           V1Dmsg = $Msg(02);
247900111223           leavesr;
248000111223         ENDIF;
248100100317
248200100317
248300100317         // Controllo se causale vuole controllo su commerciale interno
248400100318          // imposto flag di forzabile
248500100318               If sav_v02cmi2 <> v02cmi ;
248600100318                  $forzacomin2 = *off ;
248700100318               endif ;
248800130809            If §ccocoi <> ' ' and (CMMfun =  'COMIN' or CMMfun = 'ASC'
248900130809                                                     or CMMfun = 'SA');
249000100317         // verifico se msg forzabile
249100100318               If §ccocoi = 'F' and $forzacomin2= *off ;
249200100317                  errMessage  = *on;
249300100317                  errGenerico = *on;
249400100318                  PosCurCmi02 = *on;
249500100318                  V1Dmsg = %trimr($Msg(06)) + ' Enter per forzare' ;
249600100318                  $forzacomin2= *on ;
249700100318                  sav_v02cmi2 = v02cmi ;
249800100317                  leavesr;
249900100318               Endif;
250000100318               If §ccocoi <> 'F' ;
250100100317                  errMessage  = *on;
250200100317                  errGenerico = *on;
250300100318                  PosCurCmi02 = *on;
250400100318                  V1dmsg = $Msg(06) ;
250500100317                  leavesr;
250600100317               Endif ;
250700100317             endif;
250800100126
250900100330
251000100330       // controllo che la data non sia inferiore a 3 gg lavorativi da oggi
251100100330         clear XGIOLAVDS;
251200100330         IXGLdata = %dec(%date());
251300100330         IXGLsub  = 'S';
251400100330         IXGLgg   = 3;
251500100330         IXGLpa   = 'P';
251600100330         IXGLlav  = 'S';
251700100330         IXGLfil = %dec(%subst(V02cmi:1:3):3:0);
251800100330         XGIOLAV (xgiolavds);
251900100330         IF  w02scd < OXGLdata;
252000100330           ErrMessage  = *on;
252100100330           ErrGenerico = *on;
252200100330           PosCurScd02 = *on;
252300100330           V1dmsg      = $Msg(03);
252400100330           V1dmsg      = %trim(V1dmsg) + ' ' +
252500100330           'Non può essere inferiore a 3 gg.lavorativi';
252600100330           leavesr;
252700100330         ENDIF;
252800100330
252900100330       // controllo che la data non sia maggiore a 14 mesi  da oggi
253000100330         clear XGIOLAVDS;
253100100330         IXGLdata = %dec(%date());
253200100330         IXGLadd  = 'S';
253300100330         IXGLmm   = 14;
253400100330         IXGLpa   = 'P';
253500100330         IXGLfil = %dec(%subst(V02cmi:1:3):3:0);
253600100330         XGIOLAV (xgiolavds);
253700100330         IF  w02scd > OXGLdata;
253800100330           ErrMessage  = *on;
253900100330           ErrGenerico = *on;
254000100330           PosCurScd02 = *on;
254100100330           V1dmsg      = $Msg(03);
254200100330           V1dmsg      = %trim(V1dmsg) + ' ' +
254300100330           'Non può essere maggiore di 14 mesi';
254400100330           leavesr;
254500100330         ENDIF;
254600100330
254700100330
254800090804         // In caso di appuntamento controllo anche i campi seguenti
254900090804
255000090804       If Appuntamento = *on ;
255100090804
255200100310         //?Verifico se il commerciale dell'attività è diverso dal
255300100310         //?commerciale della trattativa
255400100310         //?se attività legata a trattativa
255500100310         // solo se si tratta di appuntamento controllo il commerciale
255600100310         // inoltre se vengo da trattativa il messaggio è forzabile
255700100310         IF  vis_trattativ  ;
255800100310           clear tnta45ds ;
255900100310           ITA45tip = 'C';
256000100310           ITA45nrv = v02vis;
256100100310           ITA45cmm = %dec(v02cmi:7:0);
256200100310           tnta45r (tnta45ds) ;
256300100310           IF  OTA45err = 'C';
256400100311          // imposto flag di forzabile
256500100311               If sav_v02cmi <> v02cmi ;
256600100311                  $forzacomm = *off ;
256700100311               endif ;
256800100310       // se vengo da trattativa il msg è forzabile
256900100310             If $forzacomm = *off ;
257000100310                ErrMessage  = *on;
257100100310                ErrGenerico = *on;
257200100310                PosCurCmi02 = *on;
257300100310                If imk19fpr = 'T' ;
257400100310                   V1Dmsg = %trimr(OTA45msg) + ' Enter per forzare';
257500100310                   $forzacomm = *on ;
257600100311                   sav_v02cmi = v02cmi ;
257700100310                else ;
257800100310                   V1Dmsg = OTA45msg;
257900100310                leavesr;
258000100310                Endif ;
258100100310             Endif ;
258200100310           ENDIF;
258300100310         ENDIF;
258400100310
258500090804        // verifico l'inserimento delle ore di impegno
258600090804
258700090804         // controllo ora  inizio impegno
258800090804           If V02oii <> 0 ;
258900090804              sav02ora = %editc(V02oii:'X');
259000120105              If  ora > 23 or minuti > 59;
259100090804                   errMessage  = *on;
259200090804                   errGenerico = *on;
259300090804                   PosCurOii02 = *on;
259400090804                   V1Dmsg = $Msg(05);
259500090804                   leavesr;
259600090804              endif;
259700091026           else ;
259800091026              errMessage  = *on;
259900091026              errGenerico = *on;
260000091026              PosCurOii02 = *on;
260100091111              V1Dmsg = $Msg(17);
260200091026              leavesr;
260300090804           endif;
260400090804
260500090804         // controllo ora  fine   impegno
260600090804           If V02ofi <> 0 ;
260700090804              sav02ora = %editc(V02ofi:'X');
260800120105              If  ora > 23 or minuti > 59;
260900090804                   errMessage  = *on;
261000090804                   errGenerico = *on;
261100090804                   PosCurOfi02 = *on;
261200090804                   V1Dmsg = $Msg(05);
261300090804                   leavesr;
261400090804              endif;
261500091026           else ;
261600091026              errMessage  = *on;
261700091026              errGenerico = *on;
261800091026              PosCurOfi02 = *on;
261900091111              V1Dmsg = $Msg(17);
262000091026              leavesr;
262100090804           endif;
262200090804
262300090804         // controllo ora  inizio se congruente con fine impegno
262400090804           If V02oii > 0 and V02ofi = 0;
262500090804              errMessage  = *on;
262600090804              errGenerico = *on;
262700090804              PosCurOfi02 = *on;
262800090804              V1Dmsg = $Msg(13);
262900090804              leavesr;
263000090804           endif;
263100090804
263200090804         // controllo ora  inizio se congruente con fine impegno
263300090804           If V02oii > 0 and V02ofi > 0 and V02oii > V02ofi ;
263400090804              errMessage  = *on;
263500090804              errGenerico = *on;
263600090804              PosCurOfi02 = *on;
263700090804              V1Dmsg = $Msg(13);
263800090804              leavesr;
263900090804           endif;
264000090804
264100090804         // verifico se inizioe fine impegno è congruente con ora appuntamento
264200090804           If V02oii > 0 and V02ofi > 0 and (V02oii > V02sco or
264300090804              V02ofi < V02sco) ;
264400090804              errMessage  = *on;
264500090804              errGenerico = *on;
264600090804              PosCurOfi02 = *on;
264700090804              V1Dmsg = $Msg(15);
264800090804              leavesr;
264900090804           endif ;
265000091216
265100091216         //?Verifico se il commerciale è libero
265200100322         // gli passo anche l'attività del momento altrimenti non la esclude
265300091216           clear trmk84ds;
265400091216           IMK84cmm = %dec(v02cmi:7:0);
265500091216           IMK84dad = w02scd;
265600091216           IMK84oii = v02oii;
265700091216           IMK84ofi = v02ofi;
265800100322           IMK84tat = imk19tat ;
265900100322           IMK84atn = imk19atn ;
266000100322           IMK84atnp= imk19atnp;
266100091216           trmk84r (trmk84ds);
266200091216         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
266300091216           IF  OMK84err <> *blanks and OMK84err <> '9' and
266400091216               Not $ForzaImpegno;
266500091216             ErrGenerico = *on;
266600091216             ErrMessage  = *on;
266700091216             PosCurScd02 = *on;
266800091216             V1Dmsg = OMK84msg;
266900091216             $ForzaImpegno = *on;
267000091216             leavesr;
267100091216           ENDIF;
267200090611
267300090804       endif;
267400090804
267500090611       ENDSR ;
267600090611
267700090611       //--------------------------------------------------------------
267800090611       //?Gestione videata W03
267900090611       //--------------------------------------------------------------
268000090611       BEGSR GesW03;
268100090611
268200090611         // Inizializzazione videata
268300090611         if $InzW03 = *on;
268400090611           exsr InzW03;
268500090611           $InzW03 = *off;
268600090611         endif;
268700090611
268800090611
268900090611         // Emissione videata
269000090611         exfmt MK19W03;
269100090611         errMessage   = *off;
269200090611         errGenerico  = *off;
269300090611         clear W03msg;
269400090611
269500090611         select;
269600090611         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
269700090611           when  $ErrGrave = *on;
269800090611             $Fine  = *on;
269900100927
270000100927       //?F02 = Rubrica
270100100927           WHEN dsp_aid = c_F02;
270200100927             exsr F02D02;
270300100927             leavesr;
270400100927
270500090611         // F12 = Ritorno
270600090611           when dsp_aid = c_F12;
270700090611             exsr F12W03;
270800090611
270900091026         // F11 = Agenda
271000091026           when dsp_aid = c_F11;
271100091027             exsr F11w03;
271200090611
271300090611         // Enter
271400090611           other;
271500090611           exsr CtrW03;
271600090611           if errGenerico = *on ;
271700090611               leavesr;
271800090611           endif;
271900090611         endsl;
272000100329
272100100329         // se dato f8 conferma (solo in caso di appuntamento )
272200100329           If   dsp_aid = c_F08 ;
272300100329                If V02rtr = *blanks or v02mrt = *blanks ;
272400100329                   errMessage  = *on;
272500100329                   errGenerico = *on;
272600100329                   PosCurCom03 = *on;
272700100329                   W03msg = $Msg(32);
272800100329                   leavesr;
272900100329                 endif;
273000100329           endif ;
273100100329
273200100329
273300090611
273400100329         // se dato f6 conferma  / f8 conferma + mail appuntamento
273500100309
273600100309         // se dato f6 sulla videata di creazione nuova attività e non solo invio  lettera
273700100920           If    dsp_aid = c_F06  or
273800100920                 dsp_aid = c_F08  ;
273900090611                   exsr f06w03d02;
274000091210                   IF  OMK22f12 = 'S';
274100091210                     leavesr;
274200091210                   ENDIF;
274300100309
274400100309         // comitto se richiesto e se non ci sono errori
274500100309                 If   imk19cmt = 'S' and omk19err = ' ';
274600100309                      commit ;
274700100309                 endif ;
274800100115
274900100921         // se F6
275000100115           // devo aggiornare il commerciale sul potenziale nel caso non sia già
275100100115           // impostato
275200100517                chain(n)  atccpo  TNCPO01L;
275300100610                IF  not %found(TNCPO01L) or
275400100610                    (%found(tncpo01l) and cpocmm > 0) ;
275500100115                  leavesr;
275600100115                ENDIF;
275700100517           // se devo aggiornare il commerciale aggancio in aggiornamento
275800100517           // il file tncpo e ne verifico l'allocazione
275900100517                chain(e) atccpo TNCPO01l ;
276000100517             If not %error;
276100100920                Cpocmm = %dec(v02com:7:0);
276200100115                Cpoflt = %dec(%subst(%editc(cpocmm:'X'):1:3):3:0);
276300100115                update TNCPO000;
276400100517             ENDIF;
276500100329
276600100329           endif;
276700100329
276800090611       ENDSR;
276900090611
277000090611
277100090611       //--------------------------------------------------------------
277200090611       //?Caricamento videata conferma W03
277300090611       //--------------------------------------------------------------
277400090611       BEGSR InzW03;
277500090611
277600090611          clear mk19w03 ;
277700090611
277800090611             W03cae = v02cae ;
277900090611             W03caed= v02caed;
278000091113             If newnrv > 0 ;
278100091113                w03nrv = newnrv ;
278200091113             else ;
278300091113                W03nrv = v02vis ;
278400091113             endif ;
278500100311         // se la causale chiude evntualmente la  trattativa non lego il numero trattativa
278600100311         // all'attività futura
278700100311             If §ccocht = 'S' and w03nrv > 0 ;
278800100311                clear w03nrv ;
278900100311             endif ;
279000091113
279100091113             trattativa = (w03nrv > 0);
279200091118        // Se trattativa nuova evidenzio
279300091118             Nuova = ($new_trattati = *on and trattativa = *on) ;
279400090611
279500091021       // in caso di futuro contatto appuntamento non propongo nulla
279600091021        If §ccotat <> 'A' ;
279700091021
279800100924       // calcolo data scadenza se segno causale <> da '='
279900100924            If §ccoupm = '+' or §ccoupm = '-' ;
280000100924               clear XGIOLAVDS;
280100100924               IXGLdata = %dec(%date());
280200100924               If §ccoupm = '+' ;
280300100924                  IXGLadd  = 'S';
280400100924               Else ;
280500100924                  IXGLsub  = 'S';
280600100924               Endif ;
280700100924               IXGLgg   = §CCOgio;
280800100924               IXGLpa   = 'P';
280900100924               If  V02com > *zeros;
281000100924                  IXGLfil = %dec(%subst(V02com:1:3):3:0);
281100100924               else ;
281200100924                  IXGLfil = DUTpou;
281300100924               Endif;
281400100924               XGIOLAV (xgiolavds);
281500100924               If  OXGLerr = *blanks;
281600100924                   data_eur = %date(OXGLdata:*iso);
281700100924                   W03dsc   = %dec(data_eur);
281800100924               Endif;
281900100924            Endif ;
282000100924       // calcolo data scadenza se segno causale = ad '='
282100100924            If §ccoupm = '=' ;
282200101111                   W03dsc   = *date;
282300100924            Endif ;
282400090611
282500090611       // propongo l'ora del momento
282600090611             w03osc = %dec(%subst(%EDITC(%dec(%time()):'X'):1:4):4:0);
282700090611
282800090611       // propongo lo stesso commerciale che ha eseguito il contatto
282900090611             w03com  = v02com  ;
283000090611             w03comd = v02comd ;
283100091021
283200091021        endif ;
283300091021
283400100326       // in caso di futuro contatto appuntamento propongo commerciale trattativa
283500100326        If §ccotat = 'A' ;
283600100326       // propongo lo stesso commerciale della trattativa
283700100329       // richiamo il tnta45 con com mercilae 9999999 così mi torna il commerciale trattativa
283800100326           clear tnta45ds ;
283900100326           ITA45tip = 'C';
284000100326           ITA45nrv = v02vis;
284100100329           ITA45CMM = 9999999 ;
284200100326           tnta45r (tnta45ds) ;
284300100326           IF  OTA45err = 'C';
284400100326             w03com  = %editc(ota45cmm:'X')  ;
284500130809             clear  w03comD;
284600130809             chain (oTA45cmm) AZCMM000;
284700130809             if  %found(AZCMM01L);
284800130809               w03comd = CMMdes;
284900130809             endif;
285000100326           Endif ;
285100100326
285200100326        Endif ;
285300100326
285400090630       // verifico se il prossimo contatto ha una tipologia telefonata o appuntamento
285500100210             prox_telefon= (§ccotat = 'T' or §ccotat = 'O');
285600091015             prox_appunta= (§ccotat = 'A');
285700101221
285800101221       //?Se impostata la data caldo senza offerta la devo proporre
285900101221           IF  sav_DataHot <> 0;
286000101221             data_eur = %date(sav_DataHot:*iso);
286100101221             W03dsc   = %dec(data_eur);
286200101221           ENDIF;
286300091210
286400091210         //?imposto fisso 'N' per affiancamento
286500091210         W03aff = 'N';
286600100504
286700100504       //?Se appuntamento e commerciale già presente posiziono il cursore sulla data
286800100504         IF  prox_appunta and w03com > *zeros;
286900100504           PosCurDsc03 = *on;
287000100504         ENDIF;
287100091216
287200091216         $ForzaImpegno = *off;
287300090630
287400090611       ENDSR;
287500090611
287600090611       //--------------------------------------------------------------
287700090611       //?Controllo videata W03
287800090611       //--------------------------------------------------------------
287900090611       BEGSR CtrW03;
288000090611
288100090611            WindDspF = IndDspF;
288200090611            reset WindDspFb;
288300090611            IndDspF  = WindDspF;
288400090611
288500090611         // Controllo codice commerciale (? Ricerca)
288600090611           If    %scan('?' : W03com) > 0  ;
288700090611             errGenerico = *on;
288800090611             PosCurCom03 = *on;
288900130809             clear  TRMK43ds;
289000130809             iMK43fil  = DUTpou;
289100130809             TRMK43R (kpjba : TRMK43ds);
289200130809             if  oMK43err = *off  and  oMK43fxx = *blank;
289300130809               W03com  = %editc( oMK43cmm : 'X' );
289400130809               W03comd = oMK43des;
289500130809             endif;
289600090611             leavesr;
289700090611           endif;
289800090611
289900100210         // Commerciale che esegue il contatto obbligatorio
290000100210           If  W03com = *blanks or w03com = *zeros;
290100100210             errMessage  = *on;
290200100210             errGenerico = *on;
290300100210             PosCurCom03 = *on;
290400100210             W03msg = $Msg(02);
290500100210             leavesr;
290600100210           endif;
290700100210
290800090611         // Controllo codice commerciale che esegue il contatto
290900100210             IF  %check(digits:w03com) > 0;
291000100210               ErrMessage  = *on;
291100100210               ErrGenerico = *on;
291200100210               PosCurCom03 = *on;
291300100210               W03msg = $Msg(02);
291400100210               leavesr;
291500100210             ENDIF;
291600100210
291700100210           //?utente abilitato al commerciale
291800100210             clear TNTAA1DS;
291900120810             ITAA1tipo = 'M';
292000120810             ITAA1caut = '§utegtc';
292100100210             ITAA1cmm  = %dec(w03com:7:0);
292200100519             ITAA1abi = 'RA';
292300100210             kpjbu     = tntaa1ds;
292400100210             tntaa1c (kpjba);
292500100210             TNTAA1DS = kpjbu;
292600100210             IF  OTAA1fabi = 'N';
292700100210               ErrMessage  = *on;
292800100210               ErrGenerico = *on;
292900100210               PosCurCom03 = *on;
293000100210               w03msg      = $Msg(02);
293100100210               leavesr;
293200100210             ENDIF;
293300100210
293400130809             CMmcod = %int(W03com);
293500130809             chain (CMMcod) AZCMM000;
293600130809             if  NOT  %found(AZCMM01L);
293700090611               errMessage  = *on;
293800090611               errGenerico = *on;
293900090611               PosCurCom03 = *on;
294000090611               W03msg = $Msg(02);
294100090611               leavesr;
294200100210             endif;
294300130809             W03comd = CMMdes;
294400100311
294500130809             If  CMMpar <> ' ' ;
294600110525               ErrMessage  = *on;
294700110525               ErrGenerico = *on;
294800110525               PosCurCom03 = *on;
294900110525               W03msg = $Msg(02);
295000110525              leavesr;
295100110525             ENDIF;
295200111223
295300111223       //?Commerciale "valido" data inizio e fine attività
295400130809         IF  CMMdtIni > wOggi  or
295500130809             CMMdtFin < wOggi;
295600111223           ErrMessage = *on;
295700111223           ErrGenerico = *on;
295800111223           PosCurCom03 = *on;
295900111223           W03msg = $Msg(02);
296000111223           leavesr;
296100111223         ENDIF;
296200100310
296300100318            If sav_w03com <> w03com ;
296400100318               $forzacomin = *off ;
296500100318            endif ;
296600100310         // Controllo se causale vuole controllo su commerciale interno
296700130809            If §ccocoi <> ' ' and (CMMfun =  'COMIN' or CMMfun = 'ASC'
296800130809                                                     or CMMfun = 'SA');
296900100310         // verifico se msg forzabile
297000100310               If §ccocoi = 'F' and $forzacomin = *off ;
297100100310                  errMessage  = *on;
297200100310                  errGenerico = *on;
297300100310                  PosCurCom03 = *on;
297400100310                  W03msg = %trimr($Msg(06)) + ' Enter per forzare' ;
297500100310                  $forzacomin = *on ;
297600100311                  sav_w03com = w03com ;
297700100310                  leavesr;
297800100318               Endif ;
297900100318               If §ccocoi <>'F' ;
298000100310                  errMessage  = *on;
298100100310                  errGenerico = *on;
298200100310                  PosCurCom03 = *on;
298300100310                  W03msg = $Msg(06) ;
298400100310                  leavesr;
298500100310               Endif ;
298600100310             endif;
298700100202
298800100202         //?Se sto pianificando una nuova attività che apre trattativa
298900100202         //?devo controllare che non ci sia già una trattativa aperte
299000100202         //?per lo stesso potenziale e commerciale
299100100202           IF  §CCOapt = 'S' and $New_Trattati;
299200100202             clear tnta43ds ;
299300100202             ITA43cpo = cpocpo;
299400100202             ITA43ksc = v02cli;
299500100202             ITA43nrv = w03nrv;
299600100202             ITA43cmm = %dec(w03com:7:0);
299700100202             tnta43r (TNTA43DS) ;
299800100202             IF  OTA43err = '+';
299900100202               ErrMessage  = *on;
300000100202               ErrGenerico = *on;
300100100202               PosCurCom03 = *on;
300200100202               W03msg = OTA43msg;
300300100202               leavesr;
300400100202             ENDIF;
300500100202           ENDIF;
300600100326       If Prox_appunta = *on ;
300700100326
300800100326         //?Verifico se il commerciale dell'appuntamento è diverso dal
300900100326         //?commerciale della trattativa
301000100422
301100100422            If sav_w03comt <> w03com ;
301200100422               $forzacomm3 = *off ;
301300100422            endif ;
301400100326         clear tnta45ds ;
301500100326         ITA45tip = 'C';
301600100326         ITA45nrv = w03nrv;
301700100326         ITA45cmm = %dec(w03com:7:0);
301800100326         tnta45r (tnta45ds) ;
301900100326         IF  OTA45err = 'C';
302000100422       // se vengo da trattativa il msg è forzabile
302100100422             If $forzacomm3= *off ;
302200100422                ErrMessage  = *on;
302300100422                ErrGenerico = *on;
302400100422                PosCurCom03 = *on;
302500100422                If imk19fpr = 'T' ;
302600100422                   W03msg = %trimr(OTA45msg) + ' Enter per forzare';
302700100422                   $forzacomm3= *on ;
302800100422                   sav_w03comt= w03com ;
302900100422                else ;
303000100422                   w03msg = OTA45msg;
303100100422                leavesr;
303200100422                Endif ;
303300100422             endif ;
303400100326         ENDIF;
303500100326         ENDIF;
303600100326
303700100504         //?Verifico se LA FILIALE DI APPARTENENZA DEL COMMERCIALE E' UGUALE
303800100504         //?ALLA FILIALE DI APPARTENENZA DEL COMMERCIALE CLIENTE SE ESISTE
303900100504         //?E SE E' PRIMO APPUNTAMENTO
304000100504
304100100504            If %SUBST(w03com: 1: 3) <> %subst(%editc(v02cmp:'X'): 1:3) and
304200100504               $new_trattati = *on and v02cli > 0 ;
304300100504                ErrMessage  = *on;
304400100504                ErrGenerico = *on;
304500100504                PosCurCom03 = *on;
304600100504                W03msg = $Msg(34);
304700100504                leavesr;
304800100504             endif;
304900090702
305000100326         // controllo data scadenza contatto
305100100326           If    w03dsc <> 0;
305200100326             clear wlbdat;
305300100326             g02dat = w03dsc;
305400100326             xsrda8(wlbdat);
305500100326             if g02err = '1';
305600100326               errMessage  = *on;
305700100326               errGenerico = *on;
305800100326               PosCurDsc03 = *on;
305900100326               W03msg = $Msg(03);
306000100326               leavesr;
306100100326             endif;
306200100326
306300100326             w03dsc = g02dat;
306400100326             WW03dsc = g02inv;
306500100326           else ;
306600100326             errMessage  = *on;
306700100326             errGenerico = *on;
306800100326             PosCurDsc03 = *on;
306900100326             W03msg = $Msg(03);
307000100326             leavesr;
307100100326           endif;
307200100326
307300100326         // controllo che la data non sia inferiore al 2000 o superiore al 2039
307400100326         IF  ww03dsc < 20000101 or ww03dsc > 20391231;
307500100326           errMessage  = *on;
307600100326           errGenerico = *on;
307700100326           PosCurDsc03 = *on;
307800100326           W03msg = $Msg(03);
307900100326           leavesr;
308000100326         ENDIF;
308100100330
308200100330       // controllo che la data non sia inferiore a 3 gg lavorativi da oggi
308300100330         clear XGIOLAVDS;
308400100330         IXGLdata = %dec(%date());
308500100330         IXGLsub  = 'S';
308600100330         IXGLgg   = 3;
308700100330         IXGLpa   = 'P';
308800100330         IXGLlav  = 'S';
308900100330         IXGLfil = %dec(%subst(W03com:1:3):3:0);
309000100330         XGIOLAV (xgiolavds);
309100100330         IF  ww03dsc < OXGLdata;
309200100330             errMessage  = *on;
309300100330             errGenerico = *on;
309400100330             PosCurDsc03 = *on;
309500100330             W03msg = $Msg(03);
309600100330             W03msg = %trim(W03msg) + ' ' +
309700100330                      'Non può essere inferiore a 3 gg.lavorativi';
309800100330             leavesr;
309900100330         ENDIF;
310000100330
310100100330       // controllo che la data non sia maggiore  a 14 mesi da oggi
310200100330         clear XGIOLAVDS;
310300100330         IXGLdata = %dec(%date());
310400100330         IXGLadd  = 'S';
310500100330         IXGLmm   = 14;
310600100330         IXGLpa   = 'P';
310700100330         IXGLfil = %dec(%subst(W03com:1:3):3:0);
310800100330         XGIOLAV (xgiolavds);
310900100330         IF  ww03dsc > OXGLdata;
311000100330             errMessage  = *on;
311100100330             errGenerico = *on;
311200100330             PosCurDsc03 = *on;
311300100330             W03msg = $Msg(03);
311400100330             W03msg = %trim(W03msg) + ' ' +
311500100330                    'Non può essere maggiore di 14 mesi';
311600100330             leavesr;
311700100330         ENDIF;
311800100330
311900100326
312000100326         // controllo ora  esecuzione contatto
312100100326           If W03osc <> 0 ;
312200100326              sav02ora = %editc(W03osc:'X');
312300120105              If  ora > 23 or minuti > 59;
312400100326                   errMessage  = *on;
312500100326                   errGenerico = *on;
312600100326                   PosCurOsc03 = *on;
312700100326                   W03msg = $Msg(05);
312800100326                   leavesr;
312900100326              endif;
313000100326           else ;
313100100326             errMessage  = *on;
313200100326             errGenerico = *on;
313300100326             PosCurOsc03 = *on;
313400100326             W03msg = $Msg(05);
313500100326             leavesr;
313600100326           endif;
313700100326
313800090702         // In caso di appuntamento controllo anche i campi seguenti
313900090702
314000090702       If Prox_appunta = *on ;
314100091216
314200090702        // verifico l'inserimento delle ore di impegno
314300090702
314400090702         // controllo ora  inizio impegno
314500090702           If W03oii <> 0 ;
314600090702              sav02ora = %editc(W03oii:'X');
314700120105              If  ora > 23 or minuti > 59;
314800090702                   errMessage  = *on;
314900090702                   errGenerico = *on;
315000090702                   PosCurOii03 = *on;
315100090702                   w03msg = $Msg(05);
315200090702                   leavesr;
315300090702              endif;
315400091216           else ;
315500091216             errMessage  = *on;
315600091216             errGenerico = *on;
315700091216             PosCurOii03 = *on;
315800091216             w03msg = $Msg(05);
315900091216             leavesr;
316000090702           endif;
316100090702
316200091216         // controllo ora  fine   impegno
316300090702           If W03ofi <> 0 ;
316400090702              sav02ora = %editc(W03ofi:'X');
316500120105              If  ora > 23 or minuti > 59;
316600090702                   errMessage  = *on;
316700090702                   errGenerico = *on;
316800090702                   PosCurOfi03 = *on;
316900090702                   w03msg = $Msg(05);
317000090702                   leavesr;
317100090702              endif;
317200091216           else ;
317300091216             errMessage  = *on;
317400091216             errGenerico = *on;
317500091216             PosCurOfi03 = *on;
317600091216             w03msg = $Msg(05);
317700091216             leavesr;
317800090702           endif;
317900090702
318000090702         // controllo ora  inizio se congruente con fine impegno
318100090702           If W03oii > 0 and W03ofi = 0;
318200090702              errMessage  = *on;
318300090702              errGenerico = *on;
318400090702              PosCurOfi03 = *on;
318500090702              w03msg = $Msg(13);
318600090702              leavesr;
318700090702           endif;
318800090702
318900090702         // controllo ora  inizio se congruente con fine impegno
319000090702           If W03oii > 0 and W03ofi > 0 and W03oii > w03ofi ;
319100090702              errMessage  = *on;
319200090702              errGenerico = *on;
319300090702              PosCurOfi03 = *on;
319400090702              w03msg = $Msg(13);
319500090702              leavesr;
319600090702           endif;
319700090804
319800090804         // verifico se inizioe fine impegno è congruente con ora appuntamento
319900090804           If W03oii > 0 and W03ofi > 0 and (W03oii > W03osc or
320000090804              W03ofi < W03osc) ;
320100090804              errMessage  = *on;
320200090804              errGenerico = *on;
320300090804              PosCurOfi03 = *on;
320400090804              w03msg = $Msg(15);
320500090804              leavesr;
320600090804           endif ;
320700091216
320800091216         //?Verifico se il commerciale è libero
320900091216           clear trmk84ds;
321000091216           IMK84cmm = %dec(w03com:7:0);
321100091216           IMK84dad = ww03dsc;
321200091216           IMK84oii = w03oii;
321300091216           IMK84ofi = w03ofi;
321400091216           trmk84r (trmk84ds);
321500091216         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
321600091216           IF  OMK84err <> *blanks and OMK84err <> '9' and
321700091216               Not $ForzaImpegno;
321800091216             ErrGenerico = *on;
321900091216             ErrMessage  = *on;
322000091216             PosCurDsc03 = *on;
322100091216             w03msg = OMK84msg;
322200091216             $ForzaImpegno = *on;
322300091216             leavesr;
322400091216           ENDIF;
322500090702
322600090702       endif;
322700090702
322800090702       ENDSR;
322900100108
323000100108       //--------------------------------------------------------------
323100100108       //?Gestione videata D04.
323200100108       //--------------------------------------------------------------
323300100108       BEGSR GesD04;
323400100108
323500100108         //?Inizializzazione videata
323600100108         IF  $InzD04;
323700100108           exsr InzD04;
323800100108           $InzD04 = *off;
323900100108         ENDIF;
324000100108
324100100108         //?Emissione testata
324200100108         write MK19T01;
324300100108
324400100108         //?Emissione videata
324500100108         exfmt MK19D04;
324600100108         ErrMessage  = *off;
324700100108         ErrGenerico = *off;
324800100108         clear V1Dmsg;
324900100108
325000100108         SELECT;
325100100108
325200100108         //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
325300100108           WHEN  $ErrGrave = *on;
325400100108             $Fine = *on;
325500100108             OMK19err = '1';
325600100108             OMK19msg = V1Dmsg;
325700100108
325800100108         //?F06 = Conferma
325900100108           WHEN  dsp_aid = c_F06;
326000100108             exsr CtrD04;
326100100108             IF  ErrGenerico;
326200100108               leavesr;
326300100108             ENDIF;
326400100108             exsr F06D04;
326500100108
326600100108         //?F12 = Ritorno
326700100108           WHEN  dsp_aid = c_F12;
326800100108             exsr F12D02;
326900100108             leavesr ;
327000100112
327100100112         //?F16 = Annulla
327200100112           WHEN  dsp_aid = c_F16;
327300100112             exsr F16D04;
327400100112             leavesr ;
327500100108
327600100108         //?F18 = Note
327700100108           WHEN dsp_aid = c_F18;
327800100121             exsr F18D04;
327900100108
328000100108         //?Enter
328100100108           OTHER;
328200100108         //?Se manutenzione controllo i dati
328300100108             IF  Not Visualizza;
328400100108               exsr CtrD04;
328500100108               IF  ErrGenerico;
328600100108                 leavesr;
328700100108               ENDIF;
328800100108             ENDIF;
328900100108
329000100108         ENDSL;
329100100108
329200100108       ENDSR;
329300100108
329400100108       //--------------------------------------------------------------
329500100108       //?Caricamento videata telefonata D04.
329600100108       //--------------------------------------------------------------
329700100108       BEGSR InzD04;
329800100108
329900100108         clear MK19D04;
330000100108
330100100108       //?Indicatori di comodo
330200100108         altrenote    = *off  ;
330300100108         piunote      = *off  ;
330400100108         GiornoTotale = *off;
330500100108
330600100108       //?Imposto se visualizzazione
330700100108         IF  IMK19fle = '5';
330800100108           V01tit     = tit_visua;
330900100108           Visualizza = *on;
331000100108         ENDIF;
331100100108
331200100108       //?Imposto se manutenzione
331300100108         IF  IMK19fle = '2';
331400100108           V01tit       = tit_manut;
331500100108           Manutenzione = *on;
331600100108         ENDIF;
331700100519
331800100519         //?Abilito F16=Annullamento solo in manutenzione
331900100519         F16Attivo = Manutenzione;
332000100108
332100100108       //?Aggancio dati attività
332200100108           chain(n) (imk19tat:imk19atn:imk19atnp) tiatc01l;
332300100108
332400100108       //?Non trovo vado a fine
332500100108         IF  not %Found(tiatc01l);
332600100108           $ErrGrave   = *on;
332700100108           ErrMessage  = *on;
332800100108           ErrGenerico = *on;
332900100108           V1Dmsg = 'Dati attività non trovati';
333000100108           leavesr;
333100100108         ENDIF;
333200100108
333300100108       //?Ricerco descrizione causale
333400100108         clear dCCO;
333500100108         clear TIBS02ds;
333600100108         T02mod = 'C';
333700100108         T02cod = 'CCO';
333800100108         T02ke1 = atccad;
333900100108         T02sif = KNSIF;
334000100108         TNTBE_RicercaControllo  (kpjba : tibs02ds);
334100100108         IF  T02err = *blanks;
334200100108           dCCO = t02uni;
334300100108         ENDIF;
334400100108
334500100108       //?Imposto titolo alto
334600100108         clear wtitolo;
334700100108         wtitolo = §TTAtit;
334800100113         wtitolo = %trim(wtitolo) + '   ' + atccad + ' - ' + §CCOdes;
334900100108         clear wpos;
335000100108         wpos = (%len(v04tit) - %len(wtitolo)) / 2;
335100100108         %subst(v04tit:wpos:%len(wtitolo)) = wtitolo;
335200100108
335300100108       //?Verifico se parziale o tutta la giornata
335400100108         IF  atccad = 'FT';
335500100108           GiornoTotale = *on;
335600100108         ENDIF;
335700100108
335800100108       //?Data attività
335900100108         IF  atcdad > 0;
336000100108           data_eur = %date(atcdad:*iso);
336100100108           V04dad   = %dec(data_eur);
336200100108           w04dad   = atcdad;
336300100108         ENDIF;
336400100108
336500100108       //?Commerciale
336600100108         V04cmm = %editc(atccmm : 'X');
336700130809         clear  V04cmmD;
336800130809         chain (ATCcmm) AZCMM000;
336900130809         IF   %found(AZCMM01L);
337000130809           V04cmmd = CMMdes;
337100100108         ENDIF;
337200100108
337300100108       //?Impegno
337400100108         V04oii = atcoii;
337500100108         V04ofi = atcofi;
337600100108
337700100108       //?Note
337800100108         clear trmk20ds ;
337900100108         IMK20tla = 'L';
338000100108         IMK20flm = 'R';
338100100108         IMK20tat = IMK19tat ;
338200100108         IMK20atn = IMK19atn ;
338300100108         IMK20atnp= IMK19atnp -1;
338400100108         trmk20r ( kpjba : trmk20ds );
338500100108         V04no1o = emk20no1;
338600100108         V04no2o = emk20no2;
338700100108         piunote = (omk20piu = '+');
338800100108
338900100108       ENDSR;
339000100108
339100100108       //--------------------------------------------------------------
339200100108       //?Controllo videata D04.
339300100108       //--------------------------------------------------------------
339400100108       BEGSR CtrD04;
339500100108
339600100108         WindDspF = IndDspF;
339700100108         reset WindDspFb;
339800100108         IndDspF  = WindDspF;
339900100108
340000100108       //?Data attività
340100100108         IF  V04dad = 0;
340200100108           ErrMessage  = *on;
340300100108           ErrGenerico = *on;
340400100108           PosCurScd02 = *on;
340500100108           V1Dmsg      = $Msg(03);
340600100108           leavesr;
340700100108         ENDIF;
340800100108
340900100108         clear wlbdat;
341000100108         g02dat = V04dad;
341100100108         xsrda8(wlbdat);
341200100108         IF  g02err = '1';
341300100108           ErrMessage  = *on;
341400100108           ErrGenerico = *on;
341500100108           PosCurScd02 = *on;
341600100108           V1Dmsg      = $Msg(03);
341700100108           leavesr;
341800100108         ENDIF;
341900100108
342000100108         V04dad = g02dat;
342100100108         W04dad = g02inv;
342200100118
342300100118       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
342400100118         IF  W04dad < 20000101 or W04dad > 20391231;
342500100118           ErrMessage  = *on;
342600100118           ErrGenerico = *on;
342700100118           PosCurScd02 = *on;
342800100118           V1Dmsg      = $Msg(03);
342900100118           leavesr;
343000100118         ENDIF;
343100100108
343200100108       //?Impegno solo se non è intera giornata
343300100108         IF  not GiornoTotale;
343400100108           IF  V04oii = 0;
343500100108             ErrMessage  = *on;
343600100108             ErrGenerico = *on;
343700100108             PosCurOii02 = *on;
343800100108             V1Dmsg      = $Msg(17);
343900100108             leavesr;
344000100108           ENDIF;
344100100108           sav02ora = %editc(V04oii:'X');
344200120105           IF  ora > 23 or minuti > 59;
344300100108             ErrMessage  = *on;
344400100108             ErrGenerico = *on;
344500100108             PosCurOii02 = *on;
344600100108             V1Dmsg      = $Msg(05);
344700100108             leavesr;
344800100108           ENDIF;
344900100108
345000100108           IF  V04ofi = 0 ;
345100100108             ErrMessage  = *on;
345200100108             ErrGenerico = *on;
345300100108             PosCurOfi02 = *on;
345400100108             V1Dmsg      = $Msg(17);
345500100108             leavesr;
345600100108           ENDIF;
345700100108           sav02ora = %editc(V04ofi:'X');
345800120105           IF  ora > 23 or minuti > 59;
345900100108             ErrMessage  = *on;
346000100108             ErrGenerico = *on;
346100100108             PosCurOfi02 = *on;
346200100108             V1Dmsg      = $Msg(05);
346300100108             leavesr;
346400100108           ENDIF;
346500100108
346600100108         //?controllo ora  inizio se congruente con fine impegno
346700100108           IF  V04oii > 0 and V04ofi = 0;
346800100108             ErrMessage  = *on;
346900100108             ErrGenerico = *on;
347000100108             PosCurOfi02 = *on;
347100100108             V1Dmsg      = $Msg(13);
347200100108             leavesr;
347300100108           ENDIF;
347400100108
347500100108         //?controllo ora  inizio se congruente con fine impegno
347600100108           IF  V04oii > 0 and V04ofi > 0 and V04oii > V04ofi ;
347700100108             ErrMessage  = *on;
347800100108             ErrGenerico = *on;
347900100108             PosCurOfi02 = *on;
348000100108             V1Dmsg      = $Msg(13);
348100100108             leavesr;
348200100108           ENDIF;
348300100108         ENDIF;
348400100108
348500100108       //?Codice commerciale
348600100108         IF  V04cmm = *blanks or V04cmm = *zeros;
348700100108           ErrMessage  = *on;
348800100108           ErrGenerico = *on;
348900100108           PosCurCmi02 = *on;
349000100108           V1Dmsg      = $Msg(02);
349100100108           leavesr;
349200100108         ENDIF;
349300100108
349400100108         IF  %scan('?' : V04cmm) > 0  ;
349500100108           ErrGenerico = *on;
349600100108           PosCurCmi02 = *on;
349700130809           clear  TRMK43ds;
349800130809           iMK43solU = 'S';
349900130809           iMK43fil  = DUTpou;
350000130809           TRMK43R (kpjba : TRMK43ds);
350100130809           if  oMK43err = *off  and  oMK43fxx = *blank;
350200130809             V04cmm  = %editc( oMK43cmm : 'X' );
350300130809             V04cmmd = oMK43des;
350400130809           endif;
350500100108           leavesr;
350600100108         ENDIF;
350700100108
350800100108         IF  %check(digits:V04cmm) > 0;
350900100108           ErrMessage  = *on;
351000100108           ErrGenerico = *on;
351100100108           PosCurCmi02 = *on;
351200100108           V1Dmsg      = $Msg(02);
351300100108           leavesr;
351400100108         ENDIF;
351500100210
351600100210         //?utente abilitato al commerciale
351700100210         clear TNTAA1DS;
351800120810         ITAA1tipo = 'M';
351900120810         ITAA1caut = '§utegtc';
352000100210         ITAA1cmm  = %dec(V04cmm:7:0);
352100100519         ITAA1abi = 'RA';
352200100210         kpjbu     = tntaa1ds;
352300100210         tntaa1c (kpjba);
352400100210         TNTAA1DS = kpjbu;
352500100210         IF  OTAA1fabi = 'N';
352600100210           ErrMessage  = *on;
352700100210           ErrGenerico = *on;
352800100210           PosCurCmi02 = *on;
352900100210           V1Dmsg      = $Msg(02);
353000100210           leavesr;
353100100210         ENDIF;
353200100108
353300130809         CMMcod = %int(V04cmm);
353400130809         chain (CMMcod) AZCMM000;
353500130809         IF  not %found(AZCMM01L);
353600100108           ErrMessage  = *on;
353700100108           ErrGenerico = *on;
353800100108           PosCurCmi02 = *on;
353900100108           V1Dmsg      = $Msg(02);
354000100108           leavesr;
354100100108         ENDIF;
354200100504
354300130809         V04cmmd = CMMdes;
354400100210
354500100210       //?Commerciale  -  solo unificante
354600130809         IF  %dec(V04cmm:7:0) <> CMMuni;
354700100210           ErrMessage  = *on;
354800100210           ErrGenerico = *on;
354900100210           PosCurCmi02 = *on;
355000100210           V1Dmsg      = 'Commerciale errato. ' +
355100100210                         'Immettere solo commerciali UNIFICANTI!';
355200100210           leavesr;
355300100210         ENDIF;
355400100108
355500100108       ENDSR;
355600100108
355700100108       //--------------------------------------------------------------
355800100108       //?Gestione tasto funzionale F06 da videata D04.
355900100108       //?F06=Conferma
356000100108       //--------------------------------------------------------------
356100100108       BEGSR F06D04;
356200100108
356300100108       //?Registro le varizioni
356400100108         chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
356500100108         IF  %Found(tiatc01l);
356600100108           atcdad = w04dad ;
356700100108           IF  GiornoTotale;
356800100108             atchda = 090000;
356900100108           ELSE;
357000100108             atchda = V04oii * 100;
357100100108           ENDIF;
357200100108           atccmm = %dec(V04cmm:7:0);
357300100108           atcoii = V04oii;
357400100108           atcofi = V04ofi;
357500100108           update tiatc000;
357600100108         ENDIF;
357700100108
357800100108       //?Registro le note
357900100108         clear trmk20ds;
358000100108         IMK20tla = 'L';
358100100108         IMK20flm = 'C';
358200100215         IMK20cmt = '1';
358300100108         IMK20tat = atctat;
358400100108         IMK20atn = atcatn;
358500100108         IMK20atnp= atcatnp - 1;
358600100108         trmk20r ( kpjba : trmk20ds );
358700100108
358800100108       //?Comitto se richiesto e se non ci sono errori
358900100108         IF  IMK19cmt = 'S' and OMK19err = *blanks;
359000100108           commit;
359100100108         ENDIF;
359200100108
359300100108       //?Ritorno al programma chiamante
359400100108         $fine = *on;
359500100108
359600100108       ENDSR;
359700100112
359800100112       //--------------------------------------------------------------
359900100112       //?Gestione tasto funzionale F16 da videata D04.
360000100112       //?F16=Annulla
360100100112       //--------------------------------------------------------------
360200100112       BEGSR F16D04;
360300100112
360400121029       //?Se ci sono note legate all'attività da pulire le cancello
360500121029         exsr Del_ticpn;
360600100112
360700100112       //?Registro le varizioni
360800100112         chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
360900100112         IF  %Found(tiatc01l);
361000100112           delete tiatc000;
361100100112         ENDIF;
361200100112
361300100112       //?Comitto se richiesto e se non ci sono errori
361400100112         IF  IMK19cmt = 'S' and OMK19err = *blanks;
361500100112           commit;
361600100112         ENDIF;
361700100112
361800100112       //?Ritorno al programma chiamante
361900100112         $fine = *on;
362000100112
362100100112       ENDSR;
362200100121
362300100121       //--------------------------------------------------------------
362400100121       //?Gestione tasto funzionale F18 da videata D04
362500100121       //?F18=Note
362600100121       //--------------------------------------------------------------
362700100121       BEGSR F18D04;
362800100121
362900100121         clear trmk20ds;
363000100121       //?Se interrogazion o attività già chiusa
363100100121       //?richiamo in interrogazione
363200100121         IF  GiaFatto or Visualizza;
363300110510           IMK20tla  = 'L';
363400100121           IMK20flm = 'I';
363500100121         ENDIF;
363600100121         IMK20tat = atctat;
363700100121         IMK20atn = atcatn;
363800100121         Trmk20r ( kpjba : trmk20ds );
363900100929
364000100929       //?Se sono in manutenzione devo recuperare la note come prima
364100100929         IF  not GiaFatto and not Visualizza;
364200100929           IMK20tla  = 'L';
364300100929           IMK20flm  = 'R';
364400100929           IMK20tat  = IMK19tat ;
364500100929           IMK20atn  = IMK19atn ;
364600100929           IMK20atnp = IMK19atnp -1;
364700100929           Trmk20r ( kpjba : trmk20ds );
364800100929           v04no1o   =  emk20no1 ;
364900100929           v04no2o   =  emk20no2 ;
365000100929           piunote   = (omk20piu = '+');
365100100929         ENDIF;
365200100121
365300100121       ENDSR;
365400100224
365500100224       //--------------------------------------------------------------
365600100224       //?Gestione videata W05
365700100224       //--------------------------------------------------------------
365800100224       BEGSR GesW05;
365900100224
366000100224         // Inizializzazione videata
366100100224         clear w05ann ;
366200100224
366300100224         // verifico se c'è affiancamento  richiamo il trmk22 in modalità annullamento
366400100224         If Conaffianca = *on ;
366500100224            clear trmk22ds ;
366600100224            IMK22fcmt = '1';
366700100224            IMK22cmt = 'N';
366800100224            IMK22fmt = '4';
366900100224            IMK22fle = 'D';
367000100224            IMK22cac = v02cae;
367100100224            IMK22cmm  = %dec(v02com:7:0);
367200100224            IMK22dad  = w02scd;
367300100224            IMK22hda  = v02ora * 100;
367400100224            IMK22oii  = v02oii;
367500100224            IMK22ofi  = v02ofi;
367600100224            IMK22tat  = IMK19tat;
367700100224            IMK22atn  = IMK19atn;
367800100224            IMK22atnp = IMK19atnp;
367900100224
368000100224            trmk22r (kpjba : TRMK22ds);
368100100224
368200100224         // se ritorna errore lo visualizzo e faccio rollback
368300100224            IF  OMK22err <> *blanks;
368400100224                ErrMessage  = *on;
368500100224                ErrGenerico = *on;
368600100224                V1dmsg = OMK22msg;
368700100224                IF  IMK19cmt = 'S';
368800100224                    rolbk;
368900100224                ENDIF;
369000100224                leavesr;
369100100224            Endif;
369200100224
369300100224         // se F12 da gestione affiancamento rollback
369400100224            If  IMK19cmt = 'S' and OMK22F12 = 'S';
369500100224                rolbk;
369600100224         // Ritorno alla videata D02
369700100224                $Video = 'D2';
369800100224                leavesr;
369900100224            Endif;
370000100224
370100100224            Conaffianca = *off ;
370200100224         Endif ;
370300100224
370400100517         $fine5 = *off ;
370500100224         // Emissione videata
370600100517         dow $fine5 = *off ;
370700100224         exfmt MK19W05;
370800100224         errMessage   = *off;
370900100224         errGenerico  = *off;
371000100224         clear W05msg;
371100100224
371200100225         // se dato f6 conferma e risposto "SI"
371300100503           If   dsp_aid = c_F06 and w05ann = 'S' ;
371400100519         //?Se posso deletare
371500100519             IF $OkDel;
371600100224                     delete (imk19tat:imk19atn:imk19atnp) tiatc01l ;
371700100224         // se l'attività era un progressivo uno vado a cancellare le note
371800100224         // con progressivo 0
371900100224                     If imk19atnp = 1 ;
372000100224                        clear trmk20ds ;
372100100224                        imk20tla = 'L';
372200100224                        imk20flm = 'D';
372300100224                        imk20cpo = cpocpo ;
372400100224                        imk20rsc = v02cpd ;
372500100224                        imk20tat = imk19tat ;
372600100224                        imk20atn = imk19atn ;
372700100224                        imk20atnp= imk19atnp -1;
372800100224                        Trmk20r ( kpjba : trmk20ds );
372900100224                     endif ;
373000100224         // comitto se richiesto e se non ci sono errori
373100100224                IF  IMK19cmt = 'S';
373200100224                    commit ;
373300110412         // Verifico la categoria del potenziale
373400110412                    clear trmk05ds;
373500110412                    IMK05cpo = CPOcpo;
373600110412                    trmk05r (kpjba : TRMK05DS);
373700110412         //?se categoria variata aggiorno potenziale
373800110412                    IF  OMK05err = *blanks and OMK05cat <> CPOfls;
373900110412                        chain(e) CPOcpo TNCPO01L;
374000110412                        IF  not %error and %found(TNCPO01L);
374100110412                            CPOfls = OMK05cat;
374200110412                            update TNCPO000;
374300110412                        ENDIF;
374400110412                    ENDIF;
374500100224                    $Fine = *on;
374600100517                    leavesr ;
374700100224                ENDIF;
374800101014                $fine5 = *on;
374900101014                $Fine = *on;
375000100519              ELSE;
375100100520         //?CHIODO
375200100520         //?Non posso deletare  quindi chiudo attività con causale 'AA'
375300100520                chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
375400100520                IF  %Found(TIATC01L);
375500100520                  ATCdco = %dec(%date());
375600100520                  ATChco = %dec(%time());
375700100520                  IF  IMK19com > *zeros;
375800100520                    ATCcco = IMK19com;
375900100520                  ELSE;
376000100520                    ATCcco = %dec(V02cmi:7:0);
376100100520                  ENDIF;
376200100520                  ATCpru = knmus;
376300100520                  ATCcac = 'AA';
376400100520                  update TIATC000;
376500100520                ENDIF;
376600100520
376700100520               // se ci sono delle note le registro
376800100520               if v02no1 <> *blanks  or v02no2 <> *blanks ;
376900100520                clear trmk20ds ;
377000100520                imk20tla = 'L';
377100100520                imk20flm = 'C';
377200100520                imk20fno = 'S';
377300100520                imk20cmt = '1';
377400100520                imk20cpo = cpocpo ;
377500100520                If v02cli > 0 ;
377600100520                  imk20ksc = v02cli ;
377700100520                endif ;
377800100520                imk20tat = atctat ;
377900100520                imk20atn = atcatn ;
378000100520                imk20atnp= atcatnp;
378100100520                imk20rsc = v02cpd ;
378200100520                emk20no1 = v02no1 ;
378300100520                emk20no2 = v02no2 ;
378400100520                emk20dim = atcdco ;
378500100520                emk20him = atchco ;
378600100520                Trmk20r ( kpjba : trmk20ds );
378700100520               endif ;
378800100520               IF  IMK19cmt = 'S';
378900100520                   commit ;
379000110412         // Verifico la categoria del potenziale
379100110412                    clear trmk05ds;
379200110412                    IMK05cpo = CPOcpo;
379300110412                    trmk05r (kpjba : TRMK05DS);
379400110412         //?se categoria variata aggiorno potenziale
379500110412                    IF  OMK05err = *blanks and OMK05cat <> CPOfls;
379600110412                        chain(e) CPOcpo TNCPO01L;
379700110412                        IF  not %error and %found(TNCPO01L);
379800110412                            CPOfls = OMK05cat;
379900110412                            update TNCPO000;
380000110412                        ENDIF;
380100110412                    ENDIF;
380200100520                   $Fine = *on;
380300100520                   leavesr ;
380400100520               ENDIF;
380500101014                $fine5 = *on;
380600101014                $Fine = *on;
380700100520
380800100519              ENDIF;
380900100224           endif;
381000100519
381100100519
381200100225         // se dato f6 conferma e risposto "NO"
381300100503           If   dsp_aid = c_F06 and w05ann = 'N' ;
381400100225         // bollback
381500100225                IF  IMK19cmt = 'S';
381600100225                    rolbk ;
381700100225                    $Fine = *on;
381800100517                    leavesr ;
381900100225                ENDIF;
382000101014                $fine5 = *on;
382100101014                $Fine = *on;
382200100225           endif;
382300100519
382400100517         enddo;
382500100224
382600100224       ENDSR;
382700100614
382800100614       //--------------------------------------------------------------
382900100614       //?Gestione videata W08
383000100614       //--------------------------------------------------------------
383100100614       BEGSR GesW08;
383200100614
383300100614         //?Inizializzazione videata
383400100614         w08sino = 'N';
383500100614         $Fine8 = *off;
383600100614
383700100614         //?Emissione videata
383800100614         DOW  not $Fine8;
383900100614           exfmt MK19W08;
384000100614
384100100614         //?F06-Conferma
384200100614           IF  dsp_aid = c_F06;
384300100614         //?Perdo modifiche
384400100614             IF  W08sino = 'S';
384500100614               $Fine8 = *on;
384600100614               $Video = 'D2';
384700100614               $Modaffianca = *off;
384800100614               exsr F12D02;
384900100614             ENDIF;
385000100614         //?Non Perdo modifiche
385100100614             IF  W08sino = 'N';
385200100614               $Fine8 = *on;
385300100614               $Video = 'D2';
385400100614               $inzd02 = *on ;
385500100614             ENDIF;
385600100614           ENDIF;
385700100614         ENDDO;
385800100614
385900100614       ENDSR;
386000110225
386100110225       //--------------------------------------------------------------
386200110225       //?Gestione videata W09
386300110225       //--------------------------------------------------------------
386400110225       BEGSR GesW09;
386500110225
386600110225         //?Inizializzazione videata
386700110225         $Fine9 = *off;
386800110225
386900110225         //?Emissione videata
387000110225         DOW  not $Fine9;
387100110225           write MK19T01;
387200110225           write MK19D02;
387300110225           exfmt MK19W09;
387400110225
387500110225         //?Annullo l'attività e torno al TRMK18R
387600110301           IF  *inkf;
387700110225             $Fine9 = *on;
387800110225             $Fine  = *on ;
387900110225             chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
388000110225             IF  %Found(TIATC01L);
388100110301               delete TIATC000;
388200110412         // se l'attività era un progressivo uno vado a cancellare le note
388300110412         // con progressivo 0
388400110412                     If imk19atnp = 1 ;
388500110412                        clear trmk20ds ;
388600110412                        imk20tla = 'L';
388700110412                        imk20flm = 'D';
388800110412                        imk20cpo = cpocpo ;
388900110412                        imk20rsc = v02cpd ;
389000110412                        imk20tat = imk19tat ;
389100110412                        imk20atn = imk19atn ;
389200110412                        imk20atnp= imk19atnp -1;
389300110412                        Trmk20r ( kpjba : trmk20ds );
389400110412                     endif ;
389500110412         // comitto se richiesto e se non ci sono errori
389600110412                IF  IMK19cmt = 'S';
389700110412                    commit ;
389800110412                    clear trmk05ds;
389900110412                    IMK05cpo = CPOcpo;
390000110412                    trmk05r (kpjba : TRMK05DS);
390100110412         //?se categoria variata aggiorno potenziale
390200110412                    IF  OMK05err = *blanks and OMK05cat <> CPOfls;
390300110412                        chain(e) CPOcpo TNCPO01L;
390400110412                        IF  not %error and %found(TNCPO01L);
390500110412                            CPOfls = OMK05cat;
390600110412                            update TNCPO000;
390700110412                        ENDIF;
390800110412                    ENDIF;
390900110412                ENDIF;
391000110225             ENDIF;
391100110225           ENDIF;
391200110225         ENDDO;
391300110225
391400110225       ENDSR;
391500100224
391600110527       //--------------------------------------------------------------
391700110527       //?Gestione videata W10
391800110527       //--------------------------------------------------------------
391900110527       BEGSR GesW10;
392000110527
392100110527         //?Emissione videata
392200110530           write MK19T01;
392300110530           write MK19D02;
392400110527           exfmt MK19W10;
392500110530           $Video = 'D2';
392600110527
392700110527       ENDSR;
392800090611       //--------------------------------------------------------------
392900090611       //?Gestione tasto funzionale F12 da videata W03
393000090611       //?F12=Ritorno
393100090611       //--------------------------------------------------------------
393200090611       BEGSR F12W03;
393300090611
393400090611         // Ritorno alla videata D02
393500090611            $Video = 'D2';
393600100118
393700100202         //?Pulisco campo comodo n. trattativa e commerciale
393800100118         clear newnrv;
393900100202         clear w03com;
394000090611
394100090611       ENDSR;
394200090611
394300090610       //--------------------------------------------------------------
394400090610       //?Gestione tasto funzionale F12 da videata D02
394500090610       //?F12=Ritorno
394600090610       //--------------------------------------------------------------
394700090610       BEGSR F12D02;
394800100614
394900100614       //?Se modificato affiancamento devo avvisare che le modifiche andranno perse
395000100614         IF  $ModAffianca;
395100100614           $Video = 'W8';
395200100614           leavesr;
395300100614         ENDIF;
395400090610
395500091127         // verifico se si tratta della prima telefonata da cliente o potenziale
395600091127         // ritorno ai subfile di selezione
395700091127
395800091127        If imk19fle = 'C' ;
395900100802        // exsr Primo_cli ;
396000100802        // leavesr ;
396100091127        endif ;
396200091127
396300091127        If imk19fle = 'P' ;
396400100409           W_mk01f12 = '1' ;
396500100409        // exsr Primo_po  ;
396600100409        // leavesr ;
396700091127        endif ;
396800090611         // Ritorno al programma chiamante
396900100422
397000100422         omk19err = '1' ;
397100100422
397200090611         $fine = *on ;
397300090611
397400090610       ENDSR;
397500090610
397600090610
397700090610       //--------------------------------------------------------------
397800090610       //?Gestione tasto funzionale F14 da videata D02
397900090610       //?F14=Informazioni Commerciali
398000090610       //--------------------------------------------------------------
398100090610       BEGSR F14D02;
398200090610
398300090610         // Richiamo il pgm informazioni Commerciali
398400090610         clear     trmk50ds   ;
398500090610         i50cpo=cpocpo ;
398600120313         If imk19fle <> '5' ;
398700100329            i50mod = 'G' ;
398800100329         else ;
398900100329            i50mod=  'I' ;
399000100329         endif ;
399100090610         i50rag=v02cpd   ;
399200090610         i50pgm='TRMK19R' ;
399300120124
399400120124       //?Se il richiamo delle INFO è stato pilotato da attività 72 devo passare
399500120124       //?il campo I50atb = 0 per obbligare l'azzeramento della % affido a BRT
399600120124         IF  info_cac = '72';
399700120208           I50atb = '0';
399800120125         //?questo lo faccio però solo se sto bloccando l'ultimo cliente della catena
399900120125         //?se ne ho ancora dei NON bloccati non passo il dato al trmk50r
400000120125           setll cpocpo CNACO16L;
400100120125           reade cpocpo CNACO16L;
400200120125           DOW not %eof(CNACO16L);
400300120125             IF  ACOabl = *blanks;
400400120125               clear I50atb;
400500120125               leave;
400600120125             ENDIF;
400700120125             reade cpocpo CNACO16L;
400800120125           ENDDO;
400900120125
401000120124         ENDIF;
401100120524
401200120524       //?Se il richiamo delle INFO è stato pilotato da attività 41 con esito positivo
401300120524       //?su trattativa di tipo I=Incremento devo impostare
401400120524       //?il campo I50atb = % per obbligare l'aumento della % affido a BRT se = 0
401500120524         IF  info_cac = '41' and §VISesito = 'P' and VIStpv = 'I';
401600120524           I50atb = '%';
401700120524         ENDIF;
401800140929
401900140929        // Se chiusa trattativa chiedo INFO obbligatorie indipendentemente dalla causale
402000140929        // e dal tipo trattativa come richiesto dal presidio vendite
402100140929         IF  wchiudo_tr = 'S' ;
402200140929             i50obl = 'S';
402300140929         Endif ;
402400120124
402500090610         TRMK50R  (kpjba:trmk50ds) ;
402600140929
402700140929         clear Wchiudo_tr ;
402800090610
402900090610       ENDSR;
403000090610
403100090610       //--------------------------------------------------------------
403200090610       //?Gestione tasto funzionale F18 da videata D02
403300090610       //?F18=Note
403400090610       //--------------------------------------------------------------
403500090610       BEGSR F18D02;
403600090610
403700090610         // Richiamo il pgm gestione note TRMK20R
403800090610         clear trmk20ds ;
403900090610         if giafatto = *on or visualizza = *on  ;
404000090610            imk20flm = 'I';
404100110510            imk20tla = 'L';
404200090610         else ;
404300090610            imk20flm = 'M';
404400090610         endif ;
404500091105         // se valorizzato il cliente passo il cliente
404600091105         If v02cli > 0 ;
404700091105            imk20ksc = v02cli ;
404800091105            imk20rsc = v02clid ;
404900091105         else ;
405000091105            imk20cpo = cpocpo ;
405100091105            imk20rsc = v02cpd ;
405200091027         endif ;
405300100318         If imk19fle = '2' ;
405400100318            imk20flm = ' ';
405500100318            imk20tat = imk19tat ;
405600100318            imk20atn = imk19atn ;
405700100318            imk20atnp= imk19atnp -1;
405800100318         else;
405900100318            emk20no1 = v02no1 ;
406000100318            emk20no2 = v02no2 ;
406100100318         endif ;
406200090616         Trmk20r ( kpjba : trmk20ds );
406300100318         // se dato f18 mentre sono in manutenzione devo valorizzare le note parte alta
406400100318         // del video ....
406500100318         If imk19fle = '2' ;
406600100318                clear trmk20ds ;
406700100318                imk20tla = 'L';
406800100318                imk20flm = 'R';
406900100318                imk20cpo = cpocpo ;
407000100318                imk20rsc = v02cpd ;
407100100318                imk20tat = imk19tat ;
407200100318                imk20atn = imk19atn ;
407300100318                imk20atnp= imk19atnp -1;
407400100318                Trmk20r ( kpjba : trmk20ds );
407500100318                v02no1o =  emk20no1 ;
407600100318                v02no2o =  emk20no2 ;
407700100318                piunote = (omk20piu = '+');
407800100318
407900100318         else;
408000100318            v02no1 =  emk20no1 ;
408100100318            v02no2 =  emk20no2 ;
408200100318            altrenote = (omk20piu = '+');
408300100318         endif ;
408400090610
408500090610       ENDSR;
408600090610
408700090610       //--------------------------------------------------------------
408800090610       //?Gestione tasto funzionale F01 da videata D02
408900091021       //?F01=Anagrafica
409000090610       //--------------------------------------------------------------
409100090610       BEGSR F01D02;
409200090610
409300091021       If cliente = *off ;
409400091021         // Richiamo il pgm gestione  potenziali se non c'è il cliente valorizzato
409500091104         clear trmk01ds   ;
409600100409         clear kpjbu ;
409700091027         // se sono in visualizzazione imposto interrogazione
409800091027         if imk19fle <> '5' ;
409900091104            mk01k10 ='V'   ;
410000091027         else ;
410100091104            mk01k10 ='N'   ;
410200091027         endif ;
410300091104         mk01cpo =cpocpo ;
410400100409         TRMK02c  (kpjba:Trmk01ds);
410500090610         // Se al ritorno dal pgm c'è errore richiamo il pgm dei potenziali in interrogazione
410600091104         If  mk01err <> ' ' ;
410700091104             clear trmk01ds   ;
410800091104             mk01k10 ='N'   ;
410900091104             mk01cpo =cpocpo ;
411000100409             TRMK02c  (kpjba:Trmk01ds);
411100090610         endif ;
411200091021       else ;
411300091021         // Richiamo il pgm gestione cliente
411400091021
411500091021          clear Tnta60ds ;
411600091021          ta60flg = 'I';
411700091105          ta60nrv = v02cli ;
411800091105          ta60int = 'S';
411900091021
412000110428          tnta60r (kpjba:tnta60ds) ;
412100091021
412200091021       // gestisco i dati di ritorno
412300091021
412400091021          if ta60err <> *blanks ;
412500091021             errMessage  = *on;
412600091021             errGenerico = *on;
412700091021             V1Dmsg = ta60msg;
412800091021             leavesr;
412900091021          EndIf ;
413000091021       endif ;
413100090610
413200091022         // recupero i dati del potenziale/clienti
413300091021         exsr AnagD02 ;
413400091022         // recupero i dati del potenziale/clienti  responsabili trasporti
413500091022         exsr RespD02 ;
413600091022
413700090610
413800090610       ENDSR;
413900150126
414000150126       //--------------------------------------------------------------
414100150126       //?Gestione tasto funzionale F04 da videata D02                 ?
414200150126       //?F4=Campagna                                                  ?
414300150126       //--------------------------------------------------------------
414400150126       BEGSR F04D02;
414500150806
414600150807         clear TRKC09DS;
414700150807         IKC09ksu = IMK19ksc;
414800150807         trkc09r (kpjba:TRKC09DS);
414900150126
415000150126       ENDSR;
415100100928
415200100928       //--------------------------------------------------------------
415300100928       //?Gestione tasto funzionale F05 da videata D02
415400100928       //?F05=Attività
415500100928       //--------------------------------------------------------------
415600100928       BEGSR F05D02;
415700100928
415800100928         clear TRMK21DS;
415900100928         I21mod = '9';
416000100928         I21cpo = %dec(V02cpo:11:0);
416100100928         I21rsc = V02cpd;
416200100928         IF  v02cli > 0;
416300100928           I21ksc = V02cli;
416400100928           I21rsc = V02clid;
416500100928         ENDIF;
416600100928         kpjbu = TRMK21DS;
416700100928         trmk21r (kpjba);
416800100928         TRMK21DS = kpjbu;
416900100928         IF  O21err <> *blanks;
417000100928           ErrGenerico = *on;
417100100928           ErrMessage  = *on;
417200100928           V1Dmsg      = O21msg;
417300100928           leavesr;
417400100928         ENDIF;
417500100928
417600100928       ENDSR;
417700090701
417800091026       //--------------------------------------------------------------
417900091026       //?Gestione tasto funzionale F09 da videata D02
418000091210       //?F09=Trattativa
418100091026       //--------------------------------------------------------------
418200091026       BEGSR F09D02;
418300091026
418400100202         // Richiamo la trattativa
418500091026             clear tnta88ds ;
418600091026             ita88cmt = 'N';
418700091118             ita88fle = 'G' ;
418800100202             IF  visualizza;
418900100202             ita88fle = '5' ;
419000100202             ENDIF;
419100091026             ita88fpr = 'A' ;
419200091026             ita88nrv = v02vis ;
419300100225             ita88com = imk19com ;
419400100225             ita88comd= imk19comd ;
419500091026             Tnta88r ( kpjba : tnta88ds );
419600091026
419700091026       ENDSR;
419800091026
419900091026       //--------------------------------------------------------------
420000091026       //?Gestione tasto funzionale F11 da videata D02
420100091026       //?F11=Agenda
420200091026       //--------------------------------------------------------------
420300091026       BEGSR F11D02;
420400091026
420500091026         // Richiamo il pgm visualizzazione agenda
420600091027
420700091027            clear trmk82ds ;
420800100120            imk82rich = 'A' ;
420900091027         // definisco il commerciale da passare
421000091027
421100091027         // vince il commerciale che sta eseguendo l'attività dopo viene
421200091027         // il commerciale che dovrebbe eseguire l'attività  e per ultimo
421300091027         // il commerciale del cliente / potenziale
421400091027
421500091027            select ;
421600100329            when v02com <> *blanks ;
421700100329                imk82cage = %dec(v02com:7:0) ;
421800100329            when v02cmi <> *blanks ;
421900100329                imk82cage = %dec(v02cmi:7:0) ;
422000091111            when v02cmp <> *zeros  ;
422100091111                imk82cage = v02cmp ;
422200091027            endsl ;
422300091027
422400091027            kpjbu = trmk82ds ;
422500091027
422600091027            Trmk82r (kpjba);
422700091026
422800091026       ENDSR;
422900091026
423000091027       //--------------------------------------------------------------
423100091027       //?Gestione tasto funzionale F11 da videata W03
423200091027       //?F11=Agenda
423300091027       //--------------------------------------------------------------
423400091027       BEGSR F11W03;
423500091027
423600091027         // Richiamo il pgm visualizzazione agenda
423700091027
423800091027            clear trmk82ds ;
423900100120            imk82rich = 'A' ;
424000091027         // definisco il commerciale da passare
424100091027
424200091027         // vince uno dei due/tre commerciali valorizzati nella window
424300091027         // il commerciale che sta eseguendo l'attività dopo viene
424400091027         // il commerciale che dovrebbe eseguire l'attività  e per ultimo
424500091027         // il commerciale del cliente / potenziale
424600091027
424700091027            select ;
424800091027            when w03com <> *blanks ;
424900091111                imk82cage = %dec(w03com:7:0) ;
425000091027            when v02cmi <> *blanks ;
425100091111                imk82cage = %dec(v02cmi:7:0) ;
425200091027            when v02cmp <> *zeros  ;
425300091111                imk82cage = v02cmp ;
425400091027            endsl ;
425500091027
425600091027            // se data scadenza valorizzata la passo nella DS
425700091027            If w03dsc > 0 ;
425800091111                imk82data = ww03dsc ;
425900091027            endif ;
426000091027
426100091027            kpjbu = trmk82ds ;
426200091027
426300091027            Trmk82r (kpjba);
426400091027
426500091027       ENDSR;
426600100224
426700100224       //--------------------------------------------------------------
426800100224       //?Gestione tasto funzionale F16 da videata D02
426900100224       //?F16=Annullamento
427000100224       //--------------------------------------------------------------
427100100224       BEGSR F16D02;
427200100224
427300100224         // emetto una windows per la richiesta di conferma annullamento
427400100224          $video = 'W5';
427500100224
427600100224       ENDSR;
427700100224
427800091210       //--------------------------------------------------------------
427900091210       //?Gestione tasto funzionale F20 da videata D02
428000091210       //?F20=Affiancamento
428100091210       //--------------------------------------------------------------
428200091210       BEGSR F20D02;
428300091210
428400091210         clear TRMK22ds;
428500091210         IMK22fmt = '4';
428600091210
428700091210         SELECT;
428800091210           WHEN  Manutenzione;
428900091215             IMK22fcmt = '1';
429000091215             IMK22cmt = 'N';
429100091210             IMK22fle = '2';
429200091210           WHEN  Visualizza;
429300091210             IMK22fle = '5';
429400091210         ENDSL;
429500091210
429600091210         IMK22cmm  = %dec(v02cmi:7:0);
429700091211         IMK22dad  = w02scd;
429800091210         IMK22hda  = v02sco * 100;
429900091210         IMK22oii  = v02oii;
430000091210         IMK22ofi  = v02ofi;
430100091216         IMK22cac  = v02cae;
430200100114         IMK22esi  = v02fatto;
430300091210         IMK22tat  = IMK19tat;
430400091210         IMK22atn  = IMK19atn;
430500091210         IMK22atnp = IMK19atnp;
430600091210
430700091210         trmk22r (kpjba : TRMK22ds);
430800091210
430900091210         //?Se ritorna errore lo visualizzo
431000091210         IF  OMK22err <> *blanks;
431100091210           ErrMessage  = *on;
431200091210           ErrGenerico = *on;
431300091210           V1Dmsg      = OMK22msg;
431400091210         ENDIF;
431500100614
431600100614         //?Salvo che ho fatto una modifica all'affiancamento
431700100614         //?in modo da avvisare l'utente se fa F12 o F3 che le modifiche
431800100614         //?appena fatte andranno perse
431900100614         IF  OMK22f12 = *blanks;
432000100614           $ModAffianca = *on;
432100100614         ENDIF;
432200100224
432300100224        // Verifico se inseriti affiancamenti
432400100224           k_atlatl  = IMK19atn;
432500100224           k_atlatlp = IMK19atnp;
432600100224           chain  %kds(K02tiatl) TIATL02L;
432700100224           IF  %found(Tiatl02l);
432800100224             ConAffianca = *on;
432900100422           ELSE ;
433000100422             ConAffianca = *off ;
433100100224           ENDIF;
433200091210
433300091210       ENDSR;
433400091027
433500090701       //--------------------------------------------------------------
433600100920       //?Gestione tasto funzionale F21 da videata D02 x richiamo TRMK24
433700100920       //?F21=Invio materiale illustrativo
433800090701       //--------------------------------------------------------------
433900090701       BEGSR F21D02;
434000090701
434100100921         // inizializzo alcuni campi per invio lettera
434200100921          clear trmk24ds ;
434300100921          If v02cli > 0 ;
434400100921               Imk24Tde = 'C' ;
434500100921               Imk24Cde = v02cli ;
434600100921          else ;
434700100921               Imk24Tde = 'P' ;
434800100921               Imk24Cde = %dec(v02cpo:11:0) ;
434900100921          endif ;
435000100921
435100100921        // recupero i dati del responsabile trasporti
435200100921          exsr respd02 ;
435300100921          Imk24rst = vh2rtr ;
435400100921        // imposto indirizzo mail immesso nella window
435500100921          Imk24mai = v02mrt ;
435600100921
435700100921         // imposto il commerciale che firmerà la mail, il comm. del potenziale se > 0
435800100921          If v02cmp > 0 ;
435900100921               Imk24cmm = v02cmp ;
436000100921          else ;
436100100921               Imk24cmm = %dec(v02com:7:0);
436200100921          endif ;
436300100921
436400100921          trmk24r ( kpjba : TRMK24ds );
436500100921
436600100921       // Scrivo nota di invio mail se inviata
436700100921          if omk24fxx = '06' ;
436800100921             clear ticpn000 ;
436900100921
437000100921             cpncpo=cpocpo;
437100100921             cpnksc=v02cli;
437200100921             data_iso = (%date()) ;
437300100921             cpndim = %dec(data_iso) ;
437400100921             cpnhim = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
437500100921             cpnsns='S'   ;
437600100921             cpnpos=dutpou;
437700100921             cpnpru=knmus;
437800100921             cpnpno = 1;
437900100921         // imposto il campo nota
438000100921
438100100921             destina = %trim(Omk24mai) ;
438200100921
438300100921             cpnnot='Inviato testo lettera ' + Omk24tlt + ' a nome di '
438400100921                  + %editc(Omk24cmm:'X')  + ' ' + %trim(Omk24cmd) + ' a ' ;
438500100921             write ticpn000;
438600100921             cpnpno+=1;
438700100921             cpnnot=%trim(destina) ;
438800100921             write ticpn000;
438900100924             IF  IMK19cmt = 'S' and OMK19err = *blanks;
439000100924                 commit;
439100100924             ENDIF;
439200100921
439300100921           endif ;
439400090701
439500090701       ENDSR;
439600090701
439700090610       //--------------------------------------------------------------
439800090610       //?Gestione tasto funzionale F02 da videata D02
439900090610       //?F02=Contatti
440000090610       //--------------------------------------------------------------
440100090610       BEGSR F02D02;
440200090610
440300100202         // Richiamo i contatti
440400090610         clear tnta12ds   ;
440500100202         IF  Visualizza;
440600100202           ta12ric = 'V';
440700100202         ENDIF;
440800091118         If v02cli > *zeros ;
440900091118            ta12apl='C'   ;
441000091118            ta12ksc=v02cli ;
441100091118            ta12rag=v02clid  ;
441200091118         else ;
441300091118         If v02vis > *zeros ;
441400091118            ta12apl='T'   ;
441500091118            ta12nrv=v02vis ;
441600091118         else ;
441700091118            ta12apl='P'   ;
441800091118            ta12pot=cpocpo ;
441900091118            ta12rag=v02cpd   ;
442000091105         endif ;
442100091118         endif ;
442200090610         TNTA12R  (kpjba:TNTA12ds);
442300090610         If ta12err <> ' ';
442400090610            errMessage  = *on;
442500090610            errGenerico = *on;
442600090610            V1Dmsg = ta12msg ;
442700090610            leavesr;
442800090610         endif;
442900090610
443000090610         // recupero i dati del potenziale responsabili trasporti
443100090610         exsr RespD02 ;
443200090610
443300090610       ENDSR;
443400090610
443500090610
443600090610       //--------------------------------------------------------------
443700090610       //?Gestione tasto funzionale F06 da videata D02
443800090610       //?F06=Conferma
443900090610       //--------------------------------------------------------------
444000090610       BEGSR F06D02;
444100090610
444200090610        // registro i dati variati sul file
444300090611            Chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
444400090610            If %found(tiatc01l) ;
444500090610               atcdad   = W02scd  ;
444600090610               atchda = v02sco * 100 ;
444700090610               atccmm  = %dec(v02cmi:7:0);
444800090804               if  atctat = 'A' ;
444900090804                   atcoii = V02oii ;
445000090804                   atcofi = V02ofi ;
445100090804               endif ;
445200090610               update tiatc000  ;
445300090610            endif ;
445400100409
445500100409       // Se appuntamento con affiancamento richiamo la gestione dell'affiancamento
445600100614       //?solo se non ero già andata in manutenzione e dato F6
445700111102       // oppure è già andato in manutenzione ma sono stati modificati i dati dell'appuntamento
445800111102         If atctat = 'A' and ConAffianca = *on and (not $ModAffianca or
445900111102         ($ModAffianca and ((sav_atcdad <> 0 and  sav_atcdad <> atcdad ) or
446000111102         (sav_atchda <> 0 and  sav_atchda <> atchda ) or
446100111102         (sav_atcoii <> 0 and  sav_atcoii <> atcoii ) or
446200111102         (sav_atcofi <> 0 and  sav_atcofi <> atcofi ) or
446300111102         (sav_atccmm <> 0 and  sav_atccmm <> atccmm ))));
446400100412           exsr F20d02 ;
446500100409           IF  OMK22f12 = 'S' or OMK22err <> *blanks;
446600100412               IF  IMK19cmt = 'S' ;
446700100412                   rolbk ;
446800100412               ENDIF;
446900100409             leavesr;
447000100409           ENDIF;
447100100409         ENDIF ;
447200091218
447300091218        //?Se è stata modificata la data dell'appuntamento devo scrivere una nota
447400091218         IF  sav_atcdad <> 0 and sav_atcdad <> atcdad and atctat = 'A';
447500091218           data_eur = %date(sav_atcdad:*iso);
447600091218           wdata    = %dec(data_eur);
447700091218           clear trmk20ds;
447800091218           imk20tla = 'L';
447900091218           imk20flm = 'C';
448000091218           imk20fno = 'S';
448100100215           IMK20cmt = '1';
448200091218           imk20cpo = cpocpo;
448300091218           IF  v02cli > 0;
448400091218             imk20ksc = v02cli;
448500091218           ENDIF;
448600091218            imk20tat = atctat;
448700091218            imk20atn = atcatn;
448800091218            imk20atnp= atcatnp - 1;
448900091218            imk20nrv = atcnrv;
449000091222            emk20no1 = 'Data appuntamento variata, precedente: ' +
449100091218                       %editc(wdata:'Y');
449200091218            Trmk20r ( kpjba : trmk20ds );
449300091218
449400091218         ENDIF;
449500100409
449600100915       // Se è stato richiesta la conferma con mail richiamo il pgm TRMK23R1
449700100419           If   dsp_aid = c_F08 ;
449800100915        // richiamo con tipo richiamo blank in quanto in questa fase deve fare sia la richiesta
449900100915        // della mail che inviarla
450000100915                clear  TRMK23ds1;
450100100419                exsr Ric_Trmk23 ;
450200100419           Endif ;
450300100419
450400100304       //?Comitto se richiesto e se non ci sono errori
450500100304         IF  IMK19cmt = 'S' and OMK19err = *blanks;
450600100304           commit;
450700100304         ENDIF;
450800090610
450900100329         $Fine   = *on ;
451000100329
451100090610       ENDSR;
451200090610
451300090610       //--------------------------------------------------------------
451400090610       //?Gestione tasto funzionale F06 da videata D02 + W03
451500090610       //?F06=Conferma
451600090610       //--------------------------------------------------------------
451700090610       BEGSR F06W03D02;
451800090610
451900090610        // registro il contatto in tiatc
452000090610            exsr regis_atc ;
452100090610
452200090610       // scrivo il contatto in scadenza
452300090610         // pulisco i campi che non devono essere valorizzati
452400090610            clear atcest ;
452500090610            clear atcdco ;
452600090610            clear atchco ;
452700090610            clear atccco ;
452800090610            clear atcpru ;
452900090610            clear atccac ;
453000100310            clear atcoii ;
453100100310            clear atcofi ;
453200090702            clear atcco3 ;
453300091021            clear atccnw ;
453400091021            clear atcnra ;
453500100802            clear atcflo ;
453600090610
453700090702            // valorizzo il tipo contatto con quello indicato in tabella causali contatto
453800090702            atctat   = §ccotat ;
453900091214            IF  ConAffianca;
454000091214              atcatnp = sav_atlatnp + 1;
454100091214            ELSE;
454200090610            atcatnp  = atcatnp + 1 ;
454300091214            ENDIF;
454400090610            atccad   = W03cae   ;
454500090610            atcdad   = WW03dsc  ;
454600090610            atchda = w03osc * 100 ;
454700090610            atccmm  = %dec(W03com:7:0);
454800090703            // solo se è un appuntamento scrivo le ore di impegno  e il commerciale che inserisce
454900090703            // l'appuntamento
455000090702            if  prox_appunta = *on ;
455100090702                atcoii = w03oii ;
455200090702                atcofi = w03ofi ;
455300090702                atcco3 = %dec(v02com:7:0) ;
455400090702            endif ;
455500091023            // valorizzo data ora e utente immissione
455600091023            data_iso = (%date()) ;
455700091023            atcdim = %dec(data_iso) ;
455800091023            atchim = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
455900091023            atcpri = knmus ;
456000091113            atcnrv = w03nrv ;
456100100302            exsr Sr_ValCnw ;
456200100510            // se si tratta di un'attività 80 che apre una nuova trattativa siglo con atcatb= '*'
456300100510            // in modo tale venga riconosciuta in un'eventuale richiesta di cancellazione delle 80
456400100510            // dal programma trmk17r nel momento della creazione di una nuova attività dalla stampa
456500100510            // o invio mail tariffa. Questo flag dovrà poi essere pulito nel momento in cui ritorno
456600100510            // dalla creazione della trattativa
456700100723            // 21/07/10 non lo faccio + in quanto facciamo il commit nel tnta88r (gestione trattati-
456800100723            // va) così anche nel caso si voglia cancellare una 80 anche in fase di apertura di una
456900100723            // nuova trattativa si può fare
457000100723            //If atccad = '80' and  $new_trattati = *on ;
457100100721            // atcatb = '*' ;
457200100510            // mi salvo la chiave dell'attività da pulire dopo
457300100723            //   K80_atctat  = atctat ;
457400100723            //   K80_atcatn  = atcatn ;
457500100723            //   k80_atcatnp = atcatnp;
457600100723            // endif ;
457700110302
457800110302            //?Memorizzo la categoria del potenziale all'apertura dell'attività
457900110302              dATC01 = ATCflo;
458000110302              §ATCcapo1 = CPOfls;
458100110302              ATCflo = dATC01;
458200090610            write tiatc000  ;
458300121018
458400121018         //?Aggiorno il potenziale --> data primo contatto e categoria
458500121018           exsr agg_CPO;
458600091210
458700091210         //?Se appuntamento controllo se devo richiamare gestione affiancamento
458800091210         IF  prox_appunta and w03aff = 'S';
458900091210           exsr Ric_TRMK22 ;
459000091210           //?se errore o F12 devo tornare alla Window dell'appuntamento
459100100113           //?e devo riagganciare il TIATC perchè è già stato aggiornato
459200091210           IF  OMK22f12 = 'S' or OMK22err <> *blanks;
459300100113                chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
459400091210             leavesr;
459500091210           ENDIF;
459600091210         ENDIF;
459700100507
459800100507         // se dato f8 conferma (solo in caso di appuntamento )
459900100507         //?Richiamo pgm per richiesta dati mail
460000100507         If   dsp_aid = c_F08 ;
460100100915             clear  TRMK23ds1;
460200100915             // richiamo il programma con R in quanto voglio solo i dati della mail senza inviare
460300100915             // cosa che si farà nella fase finale di conferma
460400100915             Imk23rei = 'R';
460500100915             exsr Ric_trmk23 ;
460600100507         ENDIF;
460700090610
460800091113         // verifico se c'è' un pgm da richiamare in base alla causale
460900100419           If  §ccoapt = 'S' and $new_trattati = *on ;
461000091113         // richiamo il programma gestione trattativa
461100100510               exsr Ric_TNTA88 ;
461200100510         // Se l'apertura della trattativa è stata pilotata dalla 80 verifico
461300100510         // se devo pulire il flag
461400100723            // 21/07/10 non lo faccio + in quanto facciamo il commit nel tnta88r (gestione trattati-
461500100723            // va) così anche nel caso si voglia cancellare una 80 anche in fase di apertura di una
461600100723            // nuova trattativa si può fare
461700100723         //    If atccad = '80' and  $new_trattati = *on ;
461800100723         // // aggancio il record attività' per pulire il flag
461900100723         //        chain (k80_atctat:K80_atcatn:K80_atcatnp) tiatc01l ;
462000100723         //        If %found(tiatc01l) ;
462100100723         //           atcatb = ' ' ;
462200100723         //           update tiatc000 ;
462300100723         //           commit ;
462400100723         //        endif ;
462500100723         //        clear  K80_atctat  ;
462600100723         //        clear  K80_atcatn  ;
462700100723         //        clear  k80_atcatnp ;
462800100723         //    endif ;
462900091113                          If ota88f12 = 'S' ;
463000091113                             $inzd02 = *on ;
463100091113                             $video  = 'D2';
463200091113                             leavesr ;
463300091113                          endif ;
463400091113           endif ;
463500100311
463600100311         // chiedo conferma chiusura trattativa
463700100311             If  $chiedi_conf = *on ;
463800110622                  exsr Ges_Chiudotr ;
463900100311         // rolbk se ci sono errori o non richiesto annullamento trattativa
464000100503                  if (imk19cmt = 'S' and omk19err <> ' ') or w06sino = 'N' ;
464100100311                        rolbk ;
464200100311                        $inzd02 = *on ;
464300100311                        $Video = 'D2';
464400100311                        leavesr ;
464500100311                     endif ;
464600100311             endif ;
464700100419
464800100419         // se dato f8 conferma (solo in caso di appuntamento )
464900100419           If   dsp_aid = c_F08 ;
465000100915          // faccio il richiamo con solo invio mail in quanto ho già i dati della mail nella ds
465100100915                Imk23rei = 'I';
465200100419                exsr Ric_trmk23 ;
465300100419           endif ;
465400100419
465500100419
465600100419         // Prima di tornare al pgm chiamante verifico se devo chiedere infoCommerciali
465700100419
465800100419             exsr Sr_InfoComm ;
465900110412
466000110502         // richiamo il TRMK05 che mi ricalcola la categoria se in categoria eliminabile
466100121018          If CPOfls = 'E';
466200110412               clear trmk05ds;
466300110412               IMK05cpo = CPOcpo;
466400110412               trmk05r (kpjba : TRMK05DS);
466500110412         //?se categoria variata aggiorno potenziale
466600110412               IF  OMK05err = *blanks and OMK05cat <> CPOfls;
466700110412                   chain(e) CPOcpo TNCPO01L;
466800110412                   IF  not %error and %found(TNCPO01L);
466900110412                       CPOfls = OMK05cat;
467000110412                       update TNCPO000;
467100110412                   ENDIF;
467200110412               ENDIF;
467300110502          ENDIF;
467400100311
467500090611         // Ritorno al programma precedente
467600090611
467700090611            $fine  = *on ;
467800090610
467900090610       ENDSR;
468000121018
468100121018       //--------------------------------------------------------------
468200121018       //?Aggiorno dati su TNCPO.
468300121018       //--------------------------------------------------------------
468400121018       BEGSR agg_CPO;
468500121018
468600121018       //?Imposto data primo contatto sul Potenziale
468700121018         IF  §CCOdpc <> 'N' and (§CPOdtpcon = *blanks or §CPOdtpcon = *zeros);
468800121018           clear dCPO01;
468900121018           chain(e) CPOcpo TNCPO01L;
469000121018           IF  not %error and %found(TNCPO01L);
469100121018             dCPO01 = CPOrst;
469200121018             §CPOdtpcon = %editc(W02dat:'X');
469300121018             CPOrst = dCPO01;
469400121018             update TNCPO000;
469500121018           ENDIF;
469600121018         ENDIF;
469700121018
469800121018       //?Se attività 14/15 ricalcolo la categoria del potenziale
469900121018         IF ATCcac = '14' or ATCcac = '15';
470000121018           clear trmk05ds;
470100121018           IMK05cpo = CPOcpo;
470200121018           trmk05r (kpjba : TRMK05DS);
470300121018         //?se categoria variata aggiorno potenziale
470400121018           IF  OMK05err = *blanks and OMK05cat <> CPOfls;
470500121018             chain(e) CPOcpo TNCPO01L;
470600121018             IF  not %error and %found(TNCPO01L);
470700121018               CPOfls = OMK05cat;
470800121018               update TNCPO000;
470900121018             ENDIF;
471000121018           ENDIF;
471100121018         ENDIF;
471200121018
471300121018       ENDSR;
471400090701
471500091210       //--------------------------------------------------------------
471600091210       //?Rchiamo il programma di gestione esito offerta  TNTA56R
471700091210       //--------------------------------------------------------------
471800091210       BEGSR Ric_Tnta56;
471900091210
472000091210             clear tnta56ds ;
472100091210             ita56cmt = 'S';
472200091210             ita56fle = 'G' ;
472300091210             ita56fpr = 'A' ;
472400091210             ita56nrv = v02vis ;
472500100111             ita56tat = imk19tat;
472600100111             ita56atn = imk19atn;
472700100111             ita56atnp= imk19atnp;
472800100225             ita56com = imk19com ;
472900100225             ita56comd= imk19comd ;
473000091210             Tnta56r ( kpjba : tnta56ds );
473100091210
473200091210         // Se restituito errore => e c'è messaggio lo restituisco ...
473300091210         if  ota56err = 'E';
473400091210           errMessage  = *on;
473500091210           errGenerico = *on;
473600091210           V1Dmsg = ota56msg ;
473700091210
473800091210           If imk19cmt = 'S' ;
473900091210              rolbk ;
474000091210           endif ;
474100091210
474200091210           leavesr;
474300091210         endif;
474400091210
474500091210          // se dato f12 ripulisco gli aggiornamenti
474600091210
474700091210            If imk19cmt = 'S' and ota56F12 = 'S' ;
474800091210               rolbk ;
474900091210            endif ;
475000091210
475100091210         // se non ci sono errori e non è stato premuto F12
475200091210         //  confermo tutto quanto scritto fino ad ora
475300091210
475400091210           If ota56err = ' ' and ota56f12 = ' '  ;
475500091210             commit ;
475600091210           endif ;
475700091210
475800091210       ENDSR  ;
475900090701
476000091015       //--------------------------------------------------------------
476100091015       //?Rchiamo il programma di gestione trattative TNTA88R
476200091015       //--------------------------------------------------------------
476300091015       BEGSR Ric_Tnta88;
476400091015
476500091015             clear tnta88ds ;
476600091015             ita88cmt = 'S';
476700091015             ita88fle = 'P' ;
476800091016             ita88fpr = 'A' ;
476900091015             ita88cpo = cpocpo   ;
477000091027             ita88ksc = v02cli ;
477100091113             ita88nrv = newnrv ;
477200091027             if ita88ksc > 0 ;
477300091027                ita88fle = 'C' ;
477400091027             endif ;
477500091015             ita88cmmi = %dec(v02com: 7: 0);
477600091023             If w03com <> *blank ;
477700091023                ita88cmm = %dec(w03com: 7: 0);
477800091023             else ;
477900091027                ita88cmm = v02cmp ;
478000091023             endif ;
478100100225             ita88com = imk19com ;
478200100225             ita88comd= imk19comd ;
478300091015             Tnta88r ( kpjba : tnta88ds );
478400091015
478500091016         // Se restituito errore => e c'è messaggio lo restituisco ...
478600091015         if  ota88err = 'E';
478700091015           errMessage  = *on;
478800091015           errGenerico = *on;
478900091015           V1Dmsg = ota88msg ;
479000091016
479100091016           If imk19cmt = 'S' ;
479200091016              rolbk ;
479300100416              exsr Del_ticpn ;
479400091016           endif ;
479500091016
479600091015           leavesr;
479700091015         endif;
479800091015
479900091016          // se dato f12 ripulisco gli aggiornamenti
480000091016
480100091016            If imk19cmt = 'S' and ota88F12 = 'S' ;
480200091016               rolbk ;
480300100416               exsr Del_ticpn ;
480400091016            endif ;
480500091016
480600091113         // se non ci sono errori e non è stato premuto F12
480700091113         //  confermo tutto quanto scritto fino ad ora
480800091113
480900091113           If ota88err = ' ' and ota88f12 = ' '  ;
481000091016             commit ;
481100091016           endif ;
481200091016
481300091015       ENDSR  ;
481400101221
481500101221       //--------------------------------------------------------------
481600101221       //?Richiamo pgm gestione data caldo senza offerte.
481700101221       //--------------------------------------------------------------
481800101221       BEGSR Ric_Ta63;
481900101221
482000101221         clear TNTA63ds;
482100101221         ITA63fmt  = '4';
482200110118         ITA63fle  = 'I';
482300101221         ITA63nrv  = V02vis;
482400101221
482500101221         tnta63r (kpjba : TNTA63ds);
482600101221
482700101221       ENDSR  ;
482800101221
482900100416       //--------------------------------------------------------------
483000100416       //?Deleto le note che non riesce a fare il pgm con il rolbk
483100100416       //--------------------------------------------------------------
483200100416       BEGSR Del_ticpn;
483300100416                        clear trmk20ds ;
483400100416                        imk20tla = 'L';
483500100416                        imk20flm = 'D';
483600100416                        imk20cpo = cpocpo ;
483700100416                        imk20rsc = v02cpd ;
483800100416                        imk20tat = imk19tat ;
483900100416                        imk20atn = imk19atn ;
484000100416                        imk20atnp= imk19atnp ;
484100100416                        Trmk20r ( kpjba : trmk20ds );
484200100416       ENDSR  ;
484300090610       //--------------------------------------------------------------
484400090610       //?Registro i dati nel file tiatc
484500090610       //--------------------------------------------------------------
484600090610       BEGSR Regis_atc;
484700110304
484800090610         // se un primo contatto stacco un nuovo numero
484900090610         If primocontatto = *on ;
485000090610
485100090610            clear tiatc000 ;
485200090610
485300090610            RESET trul33ds;
485400090610            i33tla = 'L'  ;
485500090610            i33ope = 0    ;
485600090610            i33Cnu = 070  ;
485700090610            i33num = 1    ;
485800090610            i33Aaa = *YEAR;
485900090610            kpjbu = trul33ds;
486000090610            trul33r(kpjba);
486100090610            trul33ds = kpjbu;
486200090610            IF o33Err = 0;
486300090610              atcatn  = o33Nrf;
486400090610            ENDIF;
486500090610
486600090610       // imposto a zero il progressivo telefonata
486700090610            atcatnp  = 1 ;
486800090610       // imposto i campi che vengono inseriti solo nel primo contatto
486900090610            atctat = 'T' ;
487000090610            atccpo = %dec(v02cpo:11:0) ;
487100091021            atcksc = v02cli ;
487200090610         endif ;
487300090610        // Aggiorno un contatto in scadenza
487400090610       // questi si possono inserire anche se non sono i primi contatti
487500100114         // se appuntamento e sto esitando un'attività pianificata imposto come
487600100114         // esito il fatto o non fatto impostato a video
487700100114           IF  Appuntamento and
487800100114               Not Manutenzione and Not Vis_NonFatto and Not Visualizza and
487900100114               v02fatto <> *blanks;
488000100114             atcest = V02fatto;
488100100114           ELSE;
488200091215            atcest = §ccoesi;
488300100114          ENDIF;
488400090610            atcdco = w02dat ;
488500090610            atchco = v02ora * 100 ;
488600090610            atccco = %dec(v02com:7:0);
488700090610            atcpru = knmus  ;
488800090610            atccac = v02cae ;
488900091113
489000091113        // se devo creare trattativa da causale aggiorno il nuovo numero
489100091116         If §ccoapt = 'S' and newnrv > 0 and $new_trattati = *on ;
489200091113            atcnra = newnrv ;
489300091113         endif ;
489400100309
489500100309        // se ho richiesto l'esito della attività lo scrivo nei FLO
489600100309         If §ccores = 'S' ;
489700100309            clear datc01 ;
489800100309            datc01 = atcflo ;
489900100309            §atcesito = W07esi ;
490000100309            atcflo = datc01 ;
490100100309         endif ;
490200100309
490300121018        // imposto che è stato fatto contatto su potenziale mai contattato
490400121018         IF §CPOdtpcon = *blanks or §CPOdtpcon = *zeros;
490500100802            clear datc01 ;
490600100802            datc01 = atcflo ;
490700100802            §atcmaitr = 'S'    ;
490800100802            atcflo = datc01 ;
490900100802         endif ;
491000100802
491100090610        // scrivo il primo contatto ed aggiorno uno in scadenza
491200090610         If primocontatto = *on ;
491300091023            data_iso = (%date()) ;
491400091023            atcdim = %dec(data_iso) ;
491500091023            atchim = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
491600091023            atcpri = knmus ;
491700100302            exsr SR_valcnw ;
491800110302          //?Memorizzo la categoria del potenziale all'apertura dell'attività
491900110302            dATC01 = ATCflo;
492000110413            §ATCcapo2 = CPOfls;
492100110302            ATCflo = dATC01;
492200090610            write tiatc000  ;
492300090610         else ;
492400110302          //?Memorizzo la categoria del potenziale all'esecuzione dell'attività
492500110302            dATC01 = ATCflo;
492600110302            §ATCcapo2 = CPOfls;
492700110302            ATCflo = dATC01;
492800090610            update tiatc000 ;
492900090610         endif;
493000090610
493100090610         // se ci sono delle note le registro
493200090611         if v02no1 <> *blanks  or v02no2 <> *blanks ;
493300090610            clear trmk20ds ;
493400090610            imk20tla = 'L';
493500090610            imk20flm = 'C';
493600090610            imk20fno = 'S';
493700100215            imk20cmt = '1';
493800090610            imk20cpo = cpocpo ;
493900091027            If v02cli > 0 ;
494000091027               imk20ksc = v02cli ;
494100091027            endif ;
494200090610            imk20tat = atctat ;
494300090610            imk20atn = atcatn ;
494400090610            imk20atnp= atcatnp;
494500090610            imk20rsc = v02cpd ;
494600090610            emk20no1 = v02no1 ;
494700090610            emk20no2 = v02no2 ;
494800090610            emk20dim = atcdco ;
494900090610            emk20him = atchco ;
495000090616            Trmk20r ( kpjba : trmk20ds );
495100090610         endif ;
495200090610
495300121018       // solo nel caso in cui non si deve andare
495400110502         if §ccoupm =  ' ';
495500121018         //?Aggiorno il potenziale --> data primo contatto e categoria
495600121018           exsr agg_CPO;
495700090610         endif ;
495800090610
495900100329       // valorizzo i campi per il controllo delle info commerciali
496000100329         info_cac = atccac ;
496100100329         info_nrv = atcnrv ;
496200100329         info_nra = atcnra ;
496300100329
496400090610       ENDSR ;
496500091214
496600100302       //--------------------------------------------------------------
496700100302       //?Valorizzo il campo del cliente potenziale o codificato
496800100302       //--------------------------------------------------------------
496900100302       BEGSR Sr_ValCnw;
497000100302
497100100302       // Verifico se l'attività creata è su opotenziale o su cliente codificato
497200100302
497300110502       //   IF  atcksc = 0;
497400110502       //    IF  $gia_codifica = *on ;
497500110502       //      atccnw = 'C';
497600110502       //    ELSE;
497700110502       //      atccnw = 'P';
497800110502       //    ENDIF;
497900110502       //  ELSE;
498000110502       //   atccnw = 'C';
498100110502       //   ENDIF;
498200110502
498300110502       //  Imposto il flag di ATCCNW in base alla categoria del potenziale
498400110502       // se categoria = a "C" metto "C" altrimenti "P"
498500110502            If cpofls = 'C' ;
498600110502               atccnw = 'C' ;
498700110502            Else ;
498800110502               atccnw = 'P' ;
498900110502            Endif ;
499000100302
499100100302       ENDSR  ;
499200091214       //--------------------------------------------------------------
499300091214       //?Confermo esito affiancamento
499400091214       //--------------------------------------------------------------
499500091214       BEGSR Conf_Trmk22;
499600091214
499700091214         clear TRMK22ds;
499800091215         IMK22fcmt = '1';
499900091215         IMK22cmt = 'N';
500000091214         IMK22fmt = '4';
500100091214         IMK22fle = 'G';
500200091215         IMK22cac = v02cae;
500300100416         IMK22esi = V02fatto ;
500400091214         IMK22cmm  = %dec(v02com:7:0);
500500091214         IMK22dad  = w02dat;
500600091215         IMK22hda  = v02ora * 100;
500700091214         IMK22oii  = v02oii;
500800091214         IMK22ofi  = v02ofi;
500900091214         IMK22tat  = IMK19tat;
501000091214         IMK22atn  = IMK19atn;
501100091214         IMK22atnp = IMK19atnp;
501200091214
501300091214         trmk22r (kpjba : TRMK22ds);
501400091214
501500091214         //?Se ritorna errore lo visualizzo e faccio rollback
501600091214         IF  OMK22err <> *blanks;
501700091214           ErrMessage  = *on;
501800091214           ErrGenerico = *on;
501900091214           V1dmsg = OMK22msg;
502000091214           IF  IMK19cmt = 'S';
502100100721          // rolbk;
502200091214           ENDIF;
502300091214           leavesr;
502400091214         ENDIF;
502500091214
502600091214         //?Se F12 da gestione affiancamento rollback
502700091214         IF  IMK19cmt = 'S' and OMK22F12 = 'S';
502800100721         //rolbk;
502900091214           leavesr;
503000091214         ENDIF;
503100091214
503200091214         //?Se OK cerco ultimo progressivo
503300091214         clear sav_atlatnp;
503400091215         $EoF = *off;
503500091214         setll IMK19atn tiatl01l;
503600091215         DOW  not $EoF;
503700091214           reade IMK19atn tiatl01l;
503800091215           IF  %eof(tiatl01l);
503900091215             $EoF = *on;
504000091215             leave;
504100091215           ENDIF;
504200091215           sav_atlatnp = atlatnp;
504300091214         ENDDO;
504400091214
504500091214       ENDSR;
504600091210
504700091210       //--------------------------------------------------------------
504800091210       //?Richiamo il programma di gestione affiancamento TRMK22R
504900091210       //--------------------------------------------------------------
505000091210       BEGSR Ric_Trmk22;
505100091210
505200091210         clear TRMK22ds;
505300091215         IMK22fcmt = '1';
505400091215         IMK22cmt = 'N';
505500091210         IMK22fmt = '4';
505600091210         IF  Prox_Appunta;
505700091210           IMK22fle = 'I';
505800091210         ELSE;
505900091210           IMK22fle = 'G';
506000091210           IMK22cac = atccac;
506100100416           IMK22esi = v02fatto ;
506200091210         ENDIF;
506300100924         IF  §ccoupm <> ' ';
506400100412             IMK22cmm  = %dec(w03com:7:0);
506500100412         ELSE;
506600100412             IMK22cmm  = %dec(v02com:7:0);
506700100412         ENDIF;
506800091210         IMK22dad  = atcdad;
506900091210         IMK22hda  = atchda;
507000091210         IMK22oii  = atcoii;
507100091210         IMK22ofi  = atcofi;
507200091210         IMK22tat  = atctat;
507300091210         IMK22atn  = atcatn;
507400091210         IMK22atnp = atcatnp;
507500091210
507600091210         trmk22r (kpjba : TRMK22ds);
507700091210
507800091210         //?Se ritorna errore lo visualizzo e faccio rollback
507900091210         IF  OMK22err <> *blanks;
508000091210           ErrMessage  = *on;
508100091210           ErrGenerico = *on;
508200091210           IF  Prox_Appunta;
508300091210             W03msg = OMK22msg;
508400091210           ELSE;
508500091210             V1dmsg = OMK22msg;
508600091210           ENDIF;
508700091210           IF  IMK19cmt = 'S';
508800091210             rolbk;
508900091210           ENDIF;
509000091210           leavesr;
509100091210         ENDIF;
509200091210
509300091210         //?Se F12 da gestione affiancamento rollback
509400091210         IF  IMK19cmt = 'S' and OMK22F12 = 'S';
509500091210           rolbk;
509600091210         ENDIF;
509700091210
509800100622         //?se non ci sono errori e provengo dalla videata W03 e dopo
509900100622         //?questo programma devo richiamare la trattativa non committo
510000100622         //?perchè se F12 dalla trattativa devo poter fare il rolbk
510100100622         IF  prox_appunta  and w03aff = 'S' and
510200100622             §ccoapt = 'S' and $new_trattati;
510300100622           leavesr;
510400100622         ENDIF;
510500091210         //?Se non ci sono errori e non è stato premuto F12
510600091210         //?confermo tutto
510700091210         IF  OMK22err = *blanks and OMK22f12 = *blanks;
510800100721         // dopo una lunga e attenta osservazione di alcuni comportamenti del programma
510900100721         // abbiamo verificato che questo commit creava dei problemi tolto xchè ci pensa
511000100721         /// il programma + avanti a committare il tutto
511100100721         //  commit;
511200091210         ENDIF;
511300091210
511400091210       ENDSR;
511500091113
511600100329       //--------------------------------------------------------------
511700100329       //?Richiamo programma invio mail conferma appuntamento TRMK23R
511800100329       //--------------------------------------------------------------
511900100329       BEGSR Ric_Trmk23;
512000100507
512100100329
512200100915         If  Imk23rei <> 'I';
512300100915             Imk23f12 = 'S'    ;
512400100915             Imk23cma = atcco3 ;
512500100915             Imk23cmm = atccmm ;
512600100915             Imk23dta = atcdad ;
512700100915             Imk23ora = %dec(%subst(%editc(atchda:'X'):1:4):4:0);
512800100329        // recupero i dati del responsabile trasporti
512900100915             exsr respd02 ;
513000100915             Imk23rst = vh2rtr ;
513100100511        // imposto indirizzo mail immesso nella window
513200100915             Imk23emr = v02mrt ;
513300100915         Endif ;
513400100915
513500100915         trmk23r1 ( kpjba : TRMK23ds1 );
513600100915
513700100915         ATCco3 = Imk23cma;
513800100915
513900100915
514000100329
514100100329       ENDSR;
514200110207
514300110207       //--------------------------------------------------------------
514400110207       //?Richiamo il programma di gestione blocco cliente TRMK25R
514500110207       //--------------------------------------------------------------
514600110207       BEGSR Ric_Trmk25;
514700110207
514800110207         clear TRMK25ds;
514900110207         IMK25fcmt = '1';
515000110207         IMK25cmt = 'N';
515100110207         IMK25fmt = '4';
515200110207         IMK25fle = 'I';
515300110207         IMK25cmm  = %dec(v02com:7:0);
515400110209         IMK25cpo  = imk19cpo ;
515500110208         IMK25cac  = v02cae ;
515600110207
515700110207         trmk25r (kpjba : TRMK25ds);
515800110207
515900110207         //?Se ritorna errore lo visualizzo e faccio rollback
516000110207         IF  OMK25err <> *blanks;
516100110207           ErrMessage  = *on;
516200110207           ErrGenerico = *on;
516300110207           V1dmsg = OMK25msg;
516400110207           IF  IMK19cmt = 'S';
516500110207             rolbk;
516600110207           ENDIF;
516700110207           leavesr;
516800110207         ENDIF;
516900110207
517000110207         //?Se F12 da gestione affiancamento rollback
517100110207         IF  IMK19cmt = 'S' and OMK25F12 = 'S';
517200110207           rolbk;
517300110207         ENDIF;
517400110207
517500110207       ENDSR;
517600110207
517700100329
517800100329
517900100329       //--------------------------------------------------------------
518000100329       //?Gestione Richiesta Info Commerciali solo per alcune causali
518100100329       //?e solo in particolari situazioni
518200100329       //--------------------------------------------------------------
518300100329       BEGSR Sr_infocomm;
518400100329
518500100329       // verifico la causale esito attività è tra quelle che richiedono l'inserimento
518600100329       // delle info commerciali
518700120313
518800120313       If info_cac = *blanks ;
518900120313          leavesr ;
519000120313       endif ;
519100100329
519200100329       pos = %lookup(info_cac : $cac) ;
519300100329       select ;
519400100329       // se causale non presente si va a fine
519500100329       when  pos = 0  ;
519600100329       // se presente trattativa e non vuole info vado a fine
519700100329       when  $TRA(pos) = 'NO' and info_nrv > 0 ;
519800100329       // se richiede info solo se aperta trattativa e non è stata aperta vado a fine
519900100329       when $APT(pos) = 'SI' and info_nra = 0 ;
520000100329
520100100329       other ;
520200100329           exsr F14D02 ;
520300100329       endsl ;
520400100329
520500100329       Endsr ;
520600100329
520700090610       //--------------------------------------------------------------
520800090610       //?Operazioni iniziali.
520900090610       //--------------------------------------------------------------
521000090610       BEGSR RoutInz;
521100090610
521200090610         // Impostazione campi "fissi"
521300090610         T01pgm = SDSpgm;
521400090610
521500090610         // Reperimento dati job
521600090610         exsr DatiJob;
521700090610
521800110805         // Controllo se utente di sede
521900100210         // Controllo se utente autorizzato alla funzione
522000100210         reset TNTAA1DS;
522100100210         ITAA1caut = '§utegtc';
522200100210         kpjbu     = TNTAA1DS;
522300100210         tntaa1c (kpjba);
522400100210         TNTAA1DS = kpjbu;
522500100210
522600131127         // ATTENZIONE FLAG DI RITORNO UTILIZZATO ALL'INTERNO DEL PGM
522700100329
522800131127         IF  OTAA1fabi = 'N' and kcdaz <> 'MK01' ;
522900090610           $ErrGrave   = *on;
523000090610           errMessage  = *on;
523100090610           errGenerico = *on;
523200090610           V1Dmsg = $Msg(01);
523300090610           leavesr;
523400090610         endif;
523500090610
523600090610         // verifico se abilitare il commit oppure no
523700090610
523800090610            flgcom = (imk19cmt = 'S') ;
523900090610
524000090610            open tiatc01l ;
524100091119            open ticpn00f ;
524200090610
524300100721
524400100721             // verifico se già stato eseguito in fase di modifica o gestione
524500100721            chain(n) (imk19tat:imk19atn:imk19atnp) tiatc01l ;
524600100721            If  (imk19fle = 'G' or imk19fle = '2') and atcdco > 0 ;
524700100721                omk19err = 'E' ;
524800100721                $fine  = *on;
524900100721                omk19msg = 'Attività eseguita da altro utente' ;
525000100721                leavesr;
525100100721            endif;
525200111223
525300111223         //?Data del giorno
525400111223         wOggi = %dec(%date());
525500100721
525600090610
525700090610       ENDSR;
525800090610
525900090610       //--------------------------------------------------------------
526000090610       //?Reperimento Dati del job (Utente/Operativi).
526100090610       //--------------------------------------------------------------
526200090610       BEGSR DatiJob;
526300090610
526400090610         in(E) §AzUte;
526500090610         if NOT %error;
526600090610           in(E) §DatiUte;
526700090610         endif;
526800090610         if %error or RSut = *blanks;
526900090610           clear TIBS34ds;
527000090610           tibs34r(tibs34ds);
527100090610           in §AzUte;
527200090610           in §DatiUte;
527300090610         endif;
527400090610
527500090610       ENDSR;
527600090610
527700090610       //--------------------------------------------------------------
527800090610       //?Operazioni finali.
527900090610       //--------------------------------------------------------------
528000090610       BEGSR RoutEnd;
528100090610
528200090610
528300100422         // comitto se richiesto e se non ci sono errori
528400100422            if imk19cmt = 'S' and omk19err = ' ';
528500100422               commit ;
528600100422            endif ;
528700100422         // rolbk  se richiesto e se  ci sono errori
528800100422            if imk19cmt = 'S' and omk19err <> ' ';
528900100422               rolbk  ;
529000100422            endif ;
529100130531
529200130531         //?Chiudo il TRMK20R
529300130531            clear TRMK20DS;
529400130531            IMK20tla = 'C';
529500130531            trmk20r ( kpjba : trmk20ds );
529600130531
529700090610         *inLR = *on;
529800090610         return;
529900090610
530000090610       ENDSR;
530100090610
530200090610      /end-free
530300090610       //--------------------------------------------------------------
530400090610       //?Schiere a tempo di compilazione.
530500090610       //--------------------------------------------------------------
530600090610
530700090610** - $MSG -------------------------------------------------------------------*
530800100210Utente non abilitato alla funzione                                             01
530900100210Codice commerciale errato o non in gestione all'utente                         02
531000090611Data errata                                                                    03
531100110307Utente non autorizzato al cliente. Riassegnare l'attività a comm. abilitato.   04
531200090611Ora errata                                                                     05
531300100310Attività non possibile per Commerciale Interno.                                06
531400100721Attività già eseguita da altro utente uscire dal programma                     07
531500100308Obbligatorio inserire la causale attività                                      08
531600100920.....................                                                          09
531700100921.......................                                                        10
531800100920.....................                                                          11
531900121018.......................                                                        12
532000090702Ora inizio impegno incongruente con ora fine impegno                           13
532100090702Causale non utilizzabile                                                       14
532200090804Ore inizio e fine impegno incongruenti con ora appuntamento                    15
532300100210..........                                                                     16
532400091026Inserire ore inizio e fine impegno                                             17
532500091027Non è stato selezionato nessun cliente                                         18
532600091112Non si può utilizzare questa causale perchè trattativa già esistente           19
532700091112Non si può utilizzare questa causale perchè esiste già una trattativa          20
532800100114Immettere se Appuntamento effettuato.                                          21
532900100224Non si può annullare unica attività aperta della trattativa                    22
533000100225Causale non utilizzabile per questo tipo attività                              23
533100100225Causale non utilizzabile in quanto attività legata a trattativa con offerte    24
533200110502Causale non utilizzabile su potenziali                                         25
533300110502............   LIBERO                                                          26
533400100308Causale non utilizzabile legata ad una trattativa                              27
533500100308Presente trattativa già esitata in modo positivo. Causale inutilizzabile       28
533600100308Obbligatorio inserire delle note                                               29
533700100520Causale utilizzabile SOLO in presenza di trattativa                            30
533800100322Causale non utilizzabile in presenza di trattativa senza offerte               31
533900100329Obbligatorio inserire il Nome e indirizzo e-mail Resp.Trasporti                32
534000100421In caso di esecuzione di un'attività Post vendita utilizzare la causale 42     33
534100100504Fil. del codice commerciale incongruente con la fil. del cliente               34
534200100505Causale non utilizzabile per trattativa di tipo                                35
534300110321La causale xx non è utilizzabile per l'esecuzione di un'attività xx            36
534400100329** - $CAC -------------------------------------------------------------------*
534500120124242526284041719172                       Causali info comm
534600100329** - $TRA -------------------------------------------------------------------*
534700120104NONONONO  NO  NO                         NO = no legate a trattative
534800100329** - $APT -------------------------------------------------------------------*
534900120104              SI                         SI = solo se in apertura trattativa
