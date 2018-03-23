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
032300091019
032400090610      //---------------------------------------------------------------
032500090610      //?Definizione variabili globali.
032600090610      //---------------------------------------------------------------
032700090610
032800090610      // - Flags booleani
032900091215     d $EoF            s               n   inz(*off)
033000100517     d $Fine           s               n   inz(*off)
033100100517     d $Fine5          s               n   inz(*off)
033200100517     d $Fine6          s               n   inz(*off)
033300100517     d $Fine7          s               n   inz(*off)
033400100614     d $Fine8          s               n   inz(*off)
033500110225     d $Fine9          s               n   inz(*off)
033600090610     d $InzD02         s               n   inz(*on)
033700100108     d $InzD04         s               n   inz(*on)
033800090610     d $InzW03         s               n   inz(*on)
033900090610     d $ErrGrave       s               n   inz(*off)
034000091113     d $NumOK          s               n   inz(*off)
034100090610     d $prima          s               n   inz(*off)
034200090610     d $dasbf          s               n   inz(*off)
034300091116     d $new_Trattati   s               n   inz(*off)
034400091216     d $ForzaImpegno   s               n   inz(*off)
034500100310     d $ForzaComm      s               n   inz(*off)
034600100422     d $ForzaComm3     s               n   inz(*off)
034700100310     d $ForzaComin     s               n   inz(*off)
034800100318     d $ForzaComin2    s               n   inz(*off)
034900100308     d $Chiedi_conf    s               n   inz(*off)
035000100322     d $Gia_codifica   s               n   inz(*off)
035100110225     d $Lavora         s               n   inz(*off)
035200100614     d $ModAffianca    s               n   inz(*off)
035300100519     d $OkDel          s               n   inz(*off)
035400110405     d $Cli_non_sped   s               n   inz
035500090610
035600090610      // - Indici di schiera
035700090610     d Pos             s              3  0 inz
035800090610
035900090610      // - Campi associati al video
036000090610     d $Video          s              2    inz('D2')
036100090610
036200090610     d dsp_mod         s             10I 0
036300090610
036400090610      // - Campi di comodo data
036500090610     d  data_eur       s               d   Datfmt(*eur)
036600090610     d  data_iso       s               d   Datfmt(*iso)
036700090610
036800090610      // - Campi di comodo
036900091020     d  savcli         s                   like(acoksc)
036901140929     d  wchiudo_tr     s              1    inz
037000090610     d  w02dat         s              8  0
037100090610     d  w02scd         s              8  0
037200100108     d  w04dad         s              8  0
037300090610     d  ww03dsc        s              8  0
037400091113     d  newnrv         s              7  0
037500100504     d  W_vistpv       s                   like(vistpv)
037600091119     d  destina        s            120
037700100329     d  info_cac       s                   like(atccac)
037800100329     d  info_nrv       s                   like(atcnrv)
037900100329     d  info_nra       s                   like(atcnra)
038000100311     d  sav_v02cmi     s                   like(v02cmi)
038100100318     d  sav_v02cmi2    s                   like(v02cmi)
038200100311     d  sav_w03com     s                   like(w03com)
038300100422     d  sav_w03comt    s                   like(w03com)
038400100616     d app_atccmm      s                   like(ATCcmm)
038500101221     d sav_DataHot     s                   like(OTA63dpa)
038600090610
038700110412     d parmcbl         s              3A
038800110412
038900091218     d sav_atcdad      s                   like(atcdad)
039000111102     d sav_atchda      s                   like(atchda)
039100111102     d sav_atcoii      s                   like(atcoii)
039200111102     d sav_atcofi      s                   like(atcofi)
039300111102     d sav_atccmm      s                   like(atccmm)
039400091214     d sav_atlatnp     s                   like(atlatnp)
039500091218     d wdata           s              8  0 inz
039600140411     d wHhMm           s              4  0 inz
039700091216     d wpos            s              5i 0 inz
039800100108     d wtitolo         s             55A
039900100118     d sav_newnrv      s                   like(newnrv)
040000100118     d sav_newtrat     s                   like($new_trattati)
040400111223     d wOggi           s              8s 0
040500090610
040600090610      //---------------------------------------------------------------
040700090610      //?Definizione procedure usate.
040800090610      //---------------------------------------------------------------
040900091210
041000100409     d TRMK02C         pr                  extpgm('TRMK02C')
041100100409     d  kpjba                              likeds(kpjba)
041200100409     d  trmk01ds                           likeds(trmk01ds)
041300110225
041400110225      // - Controllo se potenziale lavora
041500110225     d Trmk04R         pr                  extpgm('TRMK04R')
041600110225     d  KPJBA                              likeds(KPJBA)
041700110225     d  TRMK04ds                           likeds(TRMK04ds)
041800110301     d  TRMK85ds                           likeds(TRMK85ds) options(*nopass)
041900110304
042000110304      // - Calcolo categoria potenziale
042100110304     d Trmk05r         pr                  extpgm('TRMK05R')
042200110304     d  kpjba                              likeds(KPJBA)
042300110304     d  trmk05ds                           likeds(TRMK05ds)
042400100409
042500091210     d TRMK22R         pr                  extpgm('TRMK22R')
042600091210     d  kpjba                              likeds(kpjba)
042700091210     d  trmk22ds                           likeds(trmk22ds)
042800110207
042900110207     d TRMK25R         pr                  extpgm('TRMK25R')
043000110207     d  kpjba                              likeds(kpjba)
043100110207     d  trmk25ds                           likeds(trmk25ds)
043200100928
043300100928      // - Interrogazione attività
043400100928     d trmk21r         pr                  extpgm('TRMK21R')
043500100928     d  kpjba                              likeds(KPJBA)
043600100928     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
043700100921
043800100921     d trmk24r         pr                  extpgm('TRMK24R')
043900100921     d   kpjba                             likeds(KPJBA)
044000100921     d   trmk24ds                          likeds(TRMK24ds)
044100090610
044200091026     D TRMK82R         pr                  extpgm('TRMK82R')
044300091113     D  kpjba                              likeds(kpjba)
044400091216
044500091216     d TRMK84R         pr                  extpgm('TRMK84R')
044600091216     d  trmk84ds                           likeds(trmk84ds)
044700091215
044800091215     d TRMK85R         pr                  extpgm('TRMK85R')
044900091215     d  trmk85ds                           likeds(trmk85ds)
045000100430
045100091112     D Tnta43R         pr                  extpgm('TNTA43R')
045200091113     D  tnta43ds                           likeds(tnta43ds)
045300091216
045400091216     d TNTA45R         pr                  extpgm('TNTA45R')
045500091216     d  tnta45ds                           likeds(tnta45ds)
045600101221
045700101221      // - Gestione data caldo
045800101221     d Tnta63r         pr                  extpgm('TNTA63R')
045900101221     d  kpjba                              likeds(kpjba)
046000101221     d  tnta63ds                           likeds(tnta63ds)
046100100330
046200100413     d TRUL48R         pr                  extpgm('TRUL48R')
046300100413     d  kpjba                              likeds(kpjba)
046400100413     d  trul48ds                           likeds(trul48ds)
046500100413
046600100330      // - Aggiunge/Toglie gg/mm dalla data
046700100330     d Xgiolav         pr                  extpgm('XGIOLAV')
046800100330     d  xgiolavds                          likeds(xgiolavds)
046900100330
047000091112      //---------------------------------------------------------------
047100090610      //?prototipi
047200090610      //---------------------------------------------------------------
047300090610      /copy gaitrasrc/srcprotopr,tibs02r
047400090610      /copy gaitrasrc/srcprotopr,tibs34r
047500090610      /copy gaitrasrc/srcprotopr,tibs69r
047600091019      /copy gaitrasrc/srcprotopr,tntaa1c
047700091019      /copy gaitrasrc/srcprotopr,tnta12r
047800100924      /copy gaitrasrc/srcprotopr,tnta41r
047900091210      /copy gaitrasrc/srcprotopr,tnta56r
048000091210      /copy gaitrasrc/srcprotopr,tnta60r
048100091210      /copy gaitrasrc/srcprotopr,tnta88r
048200090701      /copy gaitrasrc/srcprotopr,tntb74r
048300090610      /copy gaitrasrc/srcprotopr,trmk20r
048400100915      /copy gaitrasrc/srcprotopr,trmk23r1
048500130809      /copy gaitrasrc/srcprotoPI,TRMK43R_1
048600130809      /copy gaitrasrc/srcprotoPR,TRMK43R
048700090610      /copy gaitrasrc/srcprotopr,trmk50r
048800090610      /copy gaitrasrc/srcprotopr,trul33r
048900090610      /copy gaitrasrc/srcprotopr,xsrda8
049000090611      /copy gaitrasrc/srcprotopr,QsnQryModS
049100090610
049200090610      //---------------------------------------------------------------
049300090610      //?Definizione key-list.
049400090610      //---------------------------------------------------------------
049500090610
049600091210       // - File TIATL02L
049700091210     d k02tiatl      e ds                  extname(TIATL02L:*key)
049800091210     d                                     prefix(k_)
049900091210     d                                     inz
050000090610
050100090610      //---------------------------------------------------------------
050200090610      //?Riepilogo indicatori.
050300090610      //---------------------------------------------------------------
050400090610      // - visualizzazioni particolari
050500090610      // 10    : Emissione particolare prima telefonata
050600090610      // 11    : Emissione "Altre note" nel caso in cui ci siano + di 2 righe
050700090610      // 13    : Indica un'attività già eseguita
050800090610      // 14    : Emissione "+" nel caso in cui ci siano + di 2 note
050900090610      // 15    : Manutenzione di un'attività
051000090610      // 16    : Visualizza un'attività
051100090610      // 17    : Identifica un'attività telefonica
051200090610      // 18    : Identifica un'attività di appuntamento
051300090623      // 19    : Visualizza un'attività non fatta
051400091105      // 20    : Disabilita F1 Anagrafica se richiamato dal pgm dei potenziali e cliente a zero
051500100928      // 21    : Gestione di un'attività
051600090630      // 22    : Identifica una telefonata nella window
051700090630      // 23    : Identifica un appuntamento nella window
051800100309      // 24    : Visualizza l'esito solo se esiste
051900091019      // 25    : Visualizza la trattativa
052000091019      // 26    : Visualizza il codice cliente in testata ed pilota F1 Anagrafica Cliente
052100091022      // 27    : Visualizza il numero trattativa nuovo
052200091022      // 29    : Visualizza se appuntamento di affiancamento
052300091105      // 30    : No tasti funzionali - F12 se richiamato in visualizzazione da altri pgm - trmk18
052400091105      // 31    : Disabilito il tasto funzionale f11 agenda se richiamato da Agenda
052500091105      // 32    : Disabilito il tasto funzionale f09 Trattativa se richiamato da Trattativa
052600091113      // 34    : Visualizzo la trattativa legata all'attività futura che vado a scadenziare
052700091118      // 35    : Visualizzo NUOVA se la trattativa è nuova
052800091119      // 36    : Segnalo la presenza di più trattative
052900091215      // 37    : Abilita F20=Affiancamento
053000091215      // 38    : Visualizza che appuntamento è con affiancamento
053100090610      // - Comuni
053200090610      // 28    : Emissione messaggio di errore a video
053300090610      // - D02
053400090610      // 60    : Errore: Commerciale che esegue l'attività
053500090610      // 61    : Errore: Data attività
053600090610      // 62    : Errore: Ora attività
053700090610      // 64    : Errore: Causale attività
053800090610      // 65    : Errore: Scadenza data attività
053900090610      // 66    : Errore: Scadenza ora attività
054000090610      // 67    : Errore: Commerciale che edeve eseguire l'attività
054100090610      // 68    : Errore: Ora inizio impegno appuntamento attività
054200090610      // 69    : Errore: Ora fine   impegno appuntamento attività
054300091216      // 75    : Posizionamento cursore su campo Nota
054400091215      // - W03
054500091215      // 71    : Errore: Data appuntamento
054600091215      // 72    : Errore: Ora  appuntamento
054700091215      // 73    : Errore: Commerciale appuntamento
054800091215      // 79    : Errore: Ora inizio impegno
054900091215      // 80    : Errore: Ora fine   impegno
055000090610      // - Comuni
055100090610      // 99    : Generico di Errore
055200090610      //---------------------------------------------------------------
055300090610
055400090610      //---------------------------------------------------------------
055500090610      //?M A I N - L I N E
055600090610      //---------------------------------------------------------------
055700090610
055800090610     c     *Entry        plist
055900090610     c                   parm                    KPJBA
056000090610     c                   parm                    trmk19ds
056100100409     c                   parm                    trmk01ds1
056200100409
056300090610
056400090610      /free
056500090610
056600090611       // Operazioni iniziali controllo se video accetta 132 caratteri
056700090611       if not %open(trmk19d);
056800090611           dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
056900090611           If dsp_mod <> 0 ;
057000090611              open trmk19d;
057100090611           else ;
057200090611              eval $fine = *on ;
057300090611           endif;
057400090611       endif;
057500090610
057600090610       // Operazioni iniziali
057700090610       exsr RoutInz;
057800090610
057900100616       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
058000100616
058100091021       clear savcli ;
058200091021
058300091019       // Verifico se si tratta di primo contatto potenziale
058400090611       if imk19fle = 'P' ;
058500091019          exsr Primo_Po  ;
058600090611       endif ;
058700091019
058800091019       // Verifico se si tratta di primo contatto Cliente
058900091019       if imk19fle = 'C' ;
059000091019          exsr Primo_Cli ;
059100100427          If omk19err <> ' ' ;
059200100427             $fine = *on ;
059300100427          endif ;
059400091019       endif ;
059500090701
059600100108
059700100108       // Se richiamato da Agenda verifico se attività legata a cliente
059800100108       // o attività d'ufficio
059900100108       IF  IMK19fpr = 'A';
060000100108         clear dTTA;
060100100108         clear TIBS02ds;
060200100108         T02mod = 'C';
060300100108         T02cod = 'TTA';
060400100108         T02ke1 = IMK19tco ;
060500100108         T02sif = KNSIF;
060600100108         TNTBE_RicercaControllo  (kpjba : tibs02ds);
060700100108         IF  T02err = *blanks;
060800100108           dTTA = t02uni ;
060900100108         ENDIF;
061000100108         IF  §TTAleg <> 'S';
061100100108           $InzD02 = *off;
061200100108           $InzW03 = *off;
061300100108           $InzD04 = *on;
061400100108           $Video = 'D4';
061500100108         ENDIF;
061600100108       ENDIF;
061700090611
061800090610       // Gestione video
061900090610       DOW $Fine = *off;
062000090610         select;
062100090610           when $Video = 'D2';
062200090610             exsr GesD02;
062300100224           when $Video = 'W5';
062400100224             exsr GesW05;
062500090610           when $Video = 'W3';
062600090610             exsr GesW03;
062700100108           when $Video = 'D4';
062800100108             exsr GesD04;
062900100614           when $Video = 'W8';
063000100614             exsr GesW08;
063100110225           when $Video = 'W9';
063200110225             exsr GesW09;
063300110530           when $Video = 'W0';
063400110530             exsr GesW10;
063500090610           other;
063600090610             $Fine = *on;
063700090610         endsl;
063800090610       ENDDO;
063900090610
064000090610
064100090610       // Operazioni finali
064200090610       exsr RoutEnd;
064300090611
064400090611       //--------------------------------------------------------------
064500091019       //?Gestione Primocontatto  Potenziale
064600090611       //--------------------------------------------------------------
064700091019       BEGSR Primo_Po ;
064800090611
064900090611        // se potenziale valido recupero i dati del potenziale da immettere nella prima
065000090611        // parte della videata
065100090611
065200100409         Cpocpo = W_mk01cpo ;
065300100409         W_mk01F06 = ' ' ;
065400090611         primocontatto = *on ;
065500090611         $Video = 'D2';
065600090611         $InzD02 = *on;
065700090611         $prima  = *on;
065800090611
065900090611       ENDSR;
066000090610
066100091019       //--------------------------------------------------------------
066200091019       //?Gestione Primocontatto  Cliente
066300091019       //--------------------------------------------------------------
066400091019       BEGSR Primo_Cli ;
066500100802
066600100802        // se cliente valido recupero i dati del cliente da immettere nella prima
066700100802        // parte della videata
066800100802
066900100802         savcli = IMK19ksc;
067000100802         clear  tibs69ds;
067100100802         I69kac = savcli;
067200100802         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
067300100802         if acolib = 0 ;
067400100802            omk19ksc = savcli ;
067500100802            OMK19ERR = 'E' ;
067600100802            OMK19MSG = 'Manca Potenziale sul cliente impossibile gestire nuove +
067700100802            attività' ;
067800100802         else ;
067900100802            primocontatto = *on ;
068000100802            $Video = 'D2';
068100100802            $InzD02 = *on;
068200100802            $prima  = *on;
068300100802         endif ;
068400091019
068500091019
068600091019       ENDSR;
068700091019
068800090610       //--------------------------------------------------------------
068900090610       //?Gestione videata D02
069000090610       //--------------------------------------------------------------
069100090610       BEGSR GesD02;
069200090610
069300090610         // Inizializzazione videata
069400090610         if $InzD02 = *on;
069500090610           exsr InzD02;
069600090610           $InzD02 = *off;
069700090610         endif;
069800110301
069900110301       //?Se dopo la INZ del video non devo emettere la videata ma la win con
070000110301       //?il messaggio di annullo attività esco dalla routine
070100110527         IF  $Video = 'W9';
070200110301           leavesr;
070300110301         ENDIF;
070400110530       //?Se dopo la INZ del video non devo emettere la videata ma la win con
070500110530       //?il messaggio di avviso attività da annullare esco dalla routine
070600110530         IF  $Video = 'W0';
070700110530           leavesr;
070800110530         ENDIF;
070900101221
071000101221         clear sav_DataHot;
071100090610
071200090610         // Emissione testata
071300090610           write MK19T01;
071400090610
071500090610         // Emissione videata
071600090610         exfmt MK19D02;
071700090610         errMessage   = *off;
071800090610         errGenerico  = *off;
071900090610         clear V1Dmsg;
072000090610
072100090610         select;
072200090610         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
072300090610           when  $ErrGrave = *on;
072400090610             $Fine  = *on;
072500090610         // F12 = Ritorno
072600090610           when dsp_aid = c_F12;
072700090610             exsr F12D02;
072800090611             leavesr ;
072900090701         // F21 = invio mail
073000090701           when dsp_aid = c_F21;
073100100921                If V02rtr = *blanks or v02mrt = *blanks ;
073200100921                   errMessage  = *on;
073300100921                   errGenerico = *on;
073400100921                   V1Dmsg = $Msg(32);
073500100921                   leavesr;
073600100921                 endif;
073700090701             exsr F21d02;
073800090701             leavesr ;
073900091021         // F01 = Anagrafica
074000090610           when dsp_aid = c_F01;
074100090610             exsr F01D02;
074200090610         // F18 = Note
074300090610           when dsp_aid = c_F18;
074400090610             exsr F18D02;
074500090610         // F02 = Contatti
074600090610           when dsp_aid = c_F02;
074700090610             exsr F02D02;
074800100511             leavesr ;
074900090610         // F14 = Informazioni commerciali
075000090610           when dsp_aid = c_F14;
075100090610             exsr F14D02;
075200091026         // F09 = Trattativa
075300091026           when dsp_aid = c_F09;
075400091026             exsr F09D02;
075500091026         // F11 = Agenda
075600091026           when dsp_aid = c_F11;
075700091026             exsr F11D02;
075800091026             leavesr ;
075900091210         //?F20 = Affiancamento
076000091210           when dsp_aid = c_F20;
076100091210             exsr F20D02;
076200091210             leavesr ;
076300100301         // F16 = Annullamento abilitato solo in manutenzione da trattativa
076400100224           when dsp_aid = c_F16;
076500100224             exsr F16D02;
076600100224             leavesr ;
076700100928         //?F05 = Attività
076800100930         //  when dsp_aid = c_F05;
076900100930         //    exsr F05D02;
077000100930         //    leavesr ;
077100090610
077200090610         // Enter
077300090610           other;
077400090610         // Se non si tratta di visualizzazione
077500090610            If visualizza = *off ;
077600090610               exsr CtrD02;
077700090610               if errGenerico = *on ;
077800090610                   leavesr;
077900090610               endif;
078000090610            endif;
078100090610           endsl;
078200090610
078300100329         // se dato f8 conferma appuntamento + invio mail prima verifico se ha indirizzo mail
078400100329         // altrimenti errore
078500100329           If   dsp_aid = c_F08 ;
078600100329                If V02rtr = *blanks or v02mrt = *blanks ;
078700100329                   errMessage  = *on;
078800100329                   errGenerico = *on;
078900100329                   PosCurCmi02 = *on;
079000100329                   V1Dmsg = $Msg(32);
079100100329                   leavesr;
079200100329                 endif;
079300100329                    exsr F06D02 ;
079400100329
079500100329         // Ritorno al programma precedente
079600100329                    $fine  = *on ;
079700100329                    leavesr ;
079800100329           endif ;
079900100329
080000100329         // se dato f6 conferma
080100090610           If   dsp_aid = c_F06 ;
080200090610
080300090610         // -...- VISUALIZZAZIONE vado via
080400090610                If  visualizza   = *on ;
080500090610                    leavesr ;
080600090610                endif ;
080700090610
080800090610         // -...- MANUTENZIONE
080900090610                If  manutenzione = *on ;
081000090610                    exsr F06D02 ;
081100090610                    leavesr ;
081200090610                endif ;
081300090610
081400090610         // -...- GESTIONE
081500100308
081600100308         // verifico se risulta necessario fare delle domandine
081700100308
081800100308         // chiedo esito attività/trattativa    se causale lo richiede
081900100308             If  §ccores = 'S' ;
082000100308                  exsr Ges_Esito  ;
082100100308             endif ;
082200101221
082300101221         //?Se attività prevede la richiesta della data caldo senza offerta
082400101221         //?richiamo il pgm
082500101221             IF  §CCOhot = 'S';
082600101221               exsr Ric_TA63;
082700101221         //?se F12 o errore devo tornare alla videata dell'esito attività
082800101221               IF  OTA63f12 = 'S' or OTA63err <> *blanks;
082900101221                 leavesr;
083000101221               ENDIF;
083100101221         //?memorizzo la data caldo senza offerta
083200101221               sav_DataHot = OTA63dpa;
083300101221             ENDIF;
083400110301
083500110301            SELECT;
083600110301
083700110301         //?Se attività prevede il richiamo al programma di gestione anagrafica clienti
083800110301         //?richiamo il pgm
083900110301             WHEN  §CCOpgmblc <> *blanks;
084000110301           //?verifico se l'utente può gestire il cliente
084100110301               reset TNTAA1DS;
084200110302               ITAA1ksc  = V02cli;
084300110301               ITAA1caut = '§UTECLI';
084400110301               kpjbu     = TNTAA1DS;
084500110301               tntaa1c (kpjba);
084600110301               TNTAA1DS = kpjbu;
084700110301               IF  OTAA1fabi = 'N';
084800110301                 chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
084900110307                 V1Dmsg = $msg(04);
085000110301                 errMessage  = *on;
085100110301                 errGenerico = *on;
085200110301                 leavesr;
085300110301               ENDIF;
085400110301               clear TNTA60DS;
085500110301               TA60flg = 'M';
085600110301               TA60cli = V02cli;
085700110412               Parmcbl = atcprg ;
085800110412               tnta60r (kpjba:tnta60ds:Parmcbl);
085900110301               IF  TA60f03 <> *blanks or TA60err <> *blanks;
086000110301                  chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
086100110504                  If TA60err <> *blanks ;
086200110504                     errMessage  = *on;
086300110504                     errGenerico = *on;
086400110504                     V1Dmsg = ta60msg;
086500110504                  Endif ;
086600110301                  leavesr;
086700110301               ENDIF;
086800100308
086900110207        // se attività richiede la creazione di una nuova attività ma richiede anche
087000110207        // il richiamo al blocco del cliente richiamo il pgm TRMK25R
087100110301             WHEN  §ccoblc <> ' ' ;
087200110207                   exsr Ric_trmk25 ;
087300110207                   IF  OMK25f12 = 'S' or OMK25err <> *blanks;
087400110207                       chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
087500110207                       leavesr;
087600110207                   ENDIF;
087700100329         // Emetto la window  W03  se causale necessita la creazione di un nuovo contatto
087800110301             WHEN   §ccoupm <> ' ' ;
087900100520         //?se attività legata a trattativa e chiude trattativa a potenziale codificato
088000100520         //?non devo creare nuova attività
088100100520                  IF  Vis_trattativ and §CCOcht = 'S' and $Gia_Codifica;
088200100924                      §ccoupm = ' ';
088300100520                  ELSE;
088400100520
088500091214         //?Se appuntamento con affiancamento prima di emettere la win per nuova attività
088600091214         //?devo esitare prima l'affiancamento
088700100311                    IF  ConAffianca;
088800100311                        exsr Conf_TRMK22 ;
088900091214           //?se errore o F12 devo tornare alla videata dell'esito appuntamento
089000100311                        IF  OMK22f12 = 'S' or OMK22err <> *blanks;
089100100311                            leavesr;
089200100311                        ENDIF;
089300100311                    ENDIF;
089400090610                    $Video = 'W3';
089500090610                    $inzw03 = *on ;
089600100311                    leavesr ;
089700100520                  ENDIF;
089800110301
089900110301             ENDSL;
090000100311
090100100311         // registro la scrittura del contatto senza creare il contatto successivo
090200090610                    exsr regis_atc ;
090300091214
090400091214         //?Se appuntamento con affiancamento devo esitare anche l'eventuale affiancamento
090500100311                    IF  ConAffianca;
090600100721                        exsr Ric_TRMK22 ;
090700100311           //?se errore o F12 devo tornare alla videata dell'esito appuntamento
090800100311                        IF  OMK22f12 = 'S' or OMK22err <> *blanks;
090900100721                            chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
091000100311                            leavesr;
091100100311                        ENDIF;
091200100311                    ENDIF;
091300100311
091400100311         // chiedo conferma chiusura trattativa
091500100311             If  $chiedi_conf = *on ;
091600110622                  exsr Ges_Chiudotr ;
091700100311         // rolbk se ci sono errori o non richiesto annullamento trattativa
091800100503                  if (imk19cmt = 'S' and omk19err <> ' ') or w06sino = 'N' ;
091900100311                        rolbk ;
092000100311                        $inzd02 = *on ;
092100100311                        leavesr ;
092200100311                     endif ;
092300100311             endif ;
092400091214
092500091210         // verifico se devo richiamare il pgm apertura trattativa
092600091116                    if §ccoapt <> *blank  ;
092700091015         // richiamo il programma gestione trattativa
092800091015                          exsr Ric_TNTA88 ;
092900091022                          If ota88f12 = 'S' ;
093000091022                             $inzd02 = *on ;
093100091022                             leavesr ;
093200091022                          endif ;
093300091015                    endif ;
093400100311
093500091210         // verifico se devo richiamare il pgm esito offerta
093600091210                    if §ccoeso <> *blank  ;
093700091210         // richiamo il programma esito offerta
093800091210                          exsr Ric_TNTA56 ;
093900100308                          If ota56f12 = 'S' or ota56Err = 'S' ;
094000091210                             $inzd02 = *on ;
094100091210                             leavesr ;
094200091210                          endif ;
094300091210                    endif ;
094400091016
094500100311         // comitto se richiesto e se non ci sono errori
094600091016            if imk19cmt = 'S' and omk19err = ' ';
094700091016               commit ;
094800091016            endif ;
094900091016
095000100419         // Richiedo Info Commerciali
095100100419                exsr Sr_infocomm ;
095200091016
095300110412         // alla fine di tutto se il potenziale è in categoria Eliminabile
095400110412         // richiamo il TRMK05 che mi ricalcola la categoria
095500110412           IF  CPOfls = 'E';
095600110412               clear trmk05ds;
095700110412               IMK05cpo = CPOcpo;
095800110412               trmk05r (kpjba : TRMK05DS);
095900110412         //?se categoria variata aggiorno potenziale
096000110412               IF  OMK05err = *blanks and OMK05cat <> CPOfls;
096100110412                   chain(e) CPOcpo TNCPO01L;
096200110412                   IF  not %error and %found(TNCPO01L);
096300110412                       CPOfls = OMK05cat;
096400110412                       update TNCPO000;
096500110412                   ENDIF;
096600110412               ENDIF;
096700110412           ENDIF;
096800110412
096900100329         // Ritorno al programma precedente
097000091016                $fine  = *on ;
097100090610                endif;
097200091015
097300090610       ENDSR;
097400100308
097500100308       //--------------------------------------------------------------
097600110622       //?Gestione Richiesta conferma chiusura Trattativa
097700100308       //--------------------------------------------------------------
097800110622       BEGSR Ges_Chiudotr ;
097900100308
098000100308             clear w06sino ;
098100100517             $fine6 = *off ;
098200100517
098300100517          dow $fine6 = *off ;
098400100308             exfmt mk19w06 ;
098500100517
098600100517       // N  = Nulla
098700100517             If w06sino = 'N' ;
098800100517                leavesr ;
098900100517             endif ;
099000100517
099100100517       // S  = Richiamo il pgm chiusura trattativa
099200100308
099300100503             If w06sino = 'S' ;
099400100520
099500100520               chain V02vis TIVIS05L;
099600100520               IF  %found(TIVIS05L);
099700100520                 VISesi = V02cae;
099800100520                 VISdch = %dec(%date());
099900101013        // se ho richiesto l'esito della attività lo scrivo nei FLO
100000101013                 If §ccores = 'S' ;
100100101013                    clear dvis01 ;
100200101013                    dvis01 = visflo ;
100300101013                    §visesito = W07esi ;
100400101013                    visflo = dvis01 ;
100500101013                 endif ;
100600100520                 update TIVIS000;
100700100520               ENDIF;
100800110208
100900110208        // in caso di trattativa con tariffe 999 vado ad aggiornare i dati dell'offerta / caldo
101000110208             chain (V02vis:999:0) tivof01l ;
101100110208             If %found(tivof01l) ;
101200110208             // in caso di richiesta esito e l'esito è positivo convalido l'offerta
101300110208                If §ccores = 'S' and W07esi = 'P' ;
101400110208                   vofdco = %dec(%date()) ;
101500110208                   vofeso = 'C' ;
101600110208                endif;
101700110208             // in caso di richiesta esito e l'esito è negativo annullo l'offerta
101800110208                If §ccores = 'S' and W07esi = 'N' ;
101900110208                   vofeso = '*' ;
102000110208                endif;
102100110208             // in caso di chiusura senza richiesta di esito annullo l'offerta
102200110208                If §ccores <> 'S'  ;
102300110208                   vofeso = '*' ;
102400110208                endif;
102500110208                update tivof999 ;
102600110208             Endif ;
102700110208
102800100924         // richiamo il programma delle info trattative se inserite
102900110622               If visinfot <> ' ' or (ATCCAC = '41' and vistpv = 'I');
103000100924                   exsr Sr_infotrat ;
103100120706         // se al ritorno dal PGM INFO trattattive sono state inserite ed il flag INFOT
103200120706         // sul file TRATTATIVE è a blank lo aggiorno
103300120706                   If o41ifotot <> ' ' and visinfot = ' ' ;
103400120706                      chain V02vis TIVIS05L;
103500120706                      IF  %found(TIVIS05L);
103600120706                          VISinfot = o41ifotot ;
103700120706                          update TIVIS000;
103800120706                      ENDIF;
103900120706                   endif ;
104000110622         // se in uscita visinfot è uguale a " " e causale 41 di tratattiva di tipo I
104100110622         // richiamo info trat
104200110622               dow o41ifotot = ' ' and atccac = '41' and vistpv =  'I' ;
104300110622                   exsr Sr_infotrat ;
104400110622                   If o41ifotot <> ' ' ;
104500110622                      chain V02vis TIVIS05L;
104600110622                      IF  %found(TIVIS05L);
104700110622                          VISinfot = o41ifotot ;
104800110622                          update TIVIS000;
104900110622                      ENDIF;
105000110622                   endif ;
105100110622               enddo;
105200100924               Endif ;
105300100311
105400100924         // richiamo il programma delle info commerciali
105401140929               Wchiudo_tr = 'S' ;
105500100924               exsr F14D02 ;
105600100924
105700100517             leavesr ;
105800100308             endif ;
105900100517           enddo ;
106000100308
106100100308       Endsr ;
106200100308
106300100924       //--------------------------------------------------------------
106400100924       //?Gestione Info trattativa
106500100924       //--------------------------------------------------------------
106600100924       BEGSR Sr_infotrat ;
106700100924
106800100924         // Richiamo il pgm informazioni Trattativa
106900100924         clear  TNTA41ds;
107000100924         I41nrv    = visnrv;
107100100924         I41rag    = visrag;
107200100924         I41ifotot = VISinfot;
107300100924         I41mod    = 'G';
107400100924         I41pgm    = 'TRMK19R';
107500101220         I41tpv    = VIStpv;
107600100924
107700100924         tnta41r (kpjba : TNTA41ds);
107800100924
107900100924       ENDSR;
108000100308       //--------------------------------------------------------------
108100100308       //?Gestione Richiesta Esito Attività/trattativa
108200100308       //--------------------------------------------------------------
108300100308       BEGSR Ges_Esito  ;
108400100308
108500100308             clear w07esi  ;
108600100517             $fine7 = *off ;
108700100517
108800100517           Dow $fine7 = *off ;
108900100308             exfmt mk19w07 ;
109000100517             If w07esi =  'P' ;
109100100517                leavesr ;
109200100517             endif ;
109300100517             If w07esi =  'N' ;
109400100517                leavesr ;
109500100517             endif ;
109600100517           enddo ;
109700100308
109800100308       Endsr ;
109900090610
110000090610       //--------------------------------------------------------------
110100090610       //?Caricamento videata telefonata D02
110200090610       //--------------------------------------------------------------
110300090610       BEGSR Inzd02;
110400090610
110500090610          clear mk19d02 ;
110600090610
110700090610       // imposto indicatori di visualizzazione
110800090610          Visualizza = (Imk19Fle = '5');
110900090610          Manutenzione = (Imk19Fle = '2');
111000100928          Gestione     = (Imk19Fle = 'G');
111100091119          Telefonata   = (Imk19tco = 'T') or (imk19tco = 'O') ;
111200090610          Appuntamento = (IMK19tco = 'A');
111300110405
111400110405          $cli_non_sped = *off;
111500110405
111600091119             // tipo attivita
111700091119                  clear  dTTA;
111800091119                  clear  TIBS02ds;
111900091119                  T02mod = 'C';
112000091119                  T02cod = 'TTA';
112100091119                  T02ke1 = Imk19tco ;
112200091119                  T02sif = KNSIF;
112300091119                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
112400091119                  if  T02err = *blanks;
112500091119                      dtta = t02uni ;
112600091119                  endif;
112700091216
112800091216         //?Centro titolo alto
112900091216         clear wpos;
113000091216         wpos = (%len(v02tit) - %len(§ttatit)) / 2;
113100091216         %subst(v02tit:wpos:%len(§ttatit)) = §ttatit;
113200091216
113300090623          If visualizza = *on ;
113400090623             Vis_nonfatto = (Imk19Fatt = ' ');
113500090623          else ;
113600090623             Vis_nonfatto = *off ;
113700090623          endif ;
113800090612
113900090612       // Verifico se è stato richiamato dal Programma di gestione Potenziali
114000091105              Potenziali = (imk19fpr = 'P') ;
114100091105       // Verifico se è stato richiamato dal Programma Agenda
114200091105              Ric_agenda = (imk19fpr = 'A') ;
114300091105       // Verifico se è stato richiamato dal Programma Gestione trattative
114400091105              Ric_trattativ = (imk19fpr = 'T') ;
114500091105       // Verifico se è stato richiamato in visualizzazione non dal TRMK18
114600091105              Ric_visualizz = (imk19fpr <> ' ') ;
114700100803       // Verifico se è stato richiamato con flag di provenienza "V" solo visualizzazione
114800110805       // Verifico se è utente di sede non utilizza F21
114900131127       // Verifico se è stata chiamata dall'azione MK01 non utilizza F21
115000131127              NO_F21 = (imk19fpr = 'V') or (Dutpou= 046) or KCDAZ = 'MK01';
115100091105
115200090611       // imposto testata
115300091020          If  imk19fle = 'P' or imk19fle = 'C';
115400090611              v01tit = tit_primo ;
115500090611          endif ;
115600090610
115700090611          If  imk19fle = '5' ;
115800090611              v01tit = tit_visua ;
115900090611          endif ;
116000090611
116100090611          If  imk19fle = '2' ;
116200090611              v01tit = tit_manut ;
116300090611          endif ;
116400090611
116500090611          If  imk19fle = 'G' ;
116600090611              v01tit = tit_gesti ;
116700090611          endif ;
116800090611
116900091026       // imposto il codice cliente
117000091026          v02cli  = savcli ;
117100091026
117200090610       // se provengo da sbf devo fare gli agganci con i dati passati dal subfile
117300091026          If Imk19fle <>  'P' and imk19fle <> 'C';
117400090610             cpocpo = imk19cpo ;
117500091026             v02cli = imk19ksc ;
117600090610          endif ;
117700090610
117800090610             altrenote = *off  ;
117900090610             piunote   = *off  ;
118000091218             clear sav_atcdad;
118100111102             clear sav_atchda;
118200111102             clear sav_atcoii;
118300111102             clear sav_atcofi;
118400111102             clear sav_atccmm;
118500090610
118600090610       // recupero i dati anagrafici ed informativi del potenziale
118700090610
118800090610          exsr RecD02 ;
118900090610
119000090610        // se contatto non inserito manualmente oppure primo contatto recupero
119100090610        // dati dal file contatti
119200090610         If  $prima = *off  ;
119300090610             If manutenzione = *off and visualizza = *off ;
119400090611                chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
119500090610             else  ;
119600090611                chain(n) (imk19tat:imk19atn:imk19atnp) tiatc01l ;
119700090610             endif ;
119800090610             If   %found(tiatc01l);
119900140411               // -?Data/Ora/Utente inserimento?
120000140414               clear  Data_Eur;
120100140414               clear  wData;
120200140414               clear  wHhMm;
120300140414               if  ATCdim > *zero;
120400140414                 Data_Eur = %date( ATCdim : *iso );
120500140414                 wData    = %dec( Data_Eur );
120600140414               endif;
120700140414               wHhMm = %int( ATChim / 100 );
120800140414               %subst( V02tit : %len(V02tit) - 32 ) = 'Ins. ' +
120900140414                       %editc( wData : 'Y' ) + ' ' +
121000140414                       %editw( wHhMm : '  :  ' ) + ' ' +
121100140414                       ATCpri;
121200090610             // data scadenza contatto
121300090610                  If atcdad > 0 ;
121400090610                     data_eur = %date(atcdad:*iso);
121500090610                     v02scd   = %dec(data_eur) ;
121600091211                     w02scd   = atcdad;
121700091218                     sav_atcdad = atcdad;
121800111102                     sav_atchda = atchda;
121900111102                     sav_atcoii = atcoii;
122000111102                     sav_atcofi = atcofi;
122100111102                     sav_atccmm = atccmm;
122200090610                  endif ;
122300090610             // ora  scadenza contatto
122400090610                  If atchda > 0 ;
122500090610                     v02sco = %dec(%subst(%EDITC(atchda:'X'):1:4):4:0);
122600090610                  endif ;
122700090610             // causale contatto
122800090610                  clear  dCCO;
122900090610                  clear  TIBS02ds;
123000090610                  T02mod = 'C';
123100090610                  T02cod = 'CCO';
123200090610                  T02ke1 = atccad;
123300090610                  T02sif = KNSIF;
123400090610                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
123500090610                  if  T02err = *blanks;
123600090610                      dcco = t02uni ;
123700090714                      v02cau  = atccad  ;
123800090714                      v02caud = §ccodes ;
123900110405             // Verifico se causale su cliente che non spedisce
124000110405                      If §ccocns = 'S' ;
124100110405                         $cli_non_sped = *on ;
124200110405                      Endif;
124300090610                  endif;
124400091021          // verifico se affiancamento
124500091021                  affiancamento = (atccad = '92') ;
124600100616            //?cerco il commerciale dell'Appuntamento
124700100615                  IF  Affiancamento;
124800100616                    exec sql
124900100616                    SELECT ATCcmm into :app_ATCcmm from TIATL00F
125000100616                    JOIN TIATC00F on
125100100616                    ATLatl = ATCatn and ATLatlp = ATCatnp
125200100616                    WHERE ATLatn = :IMK19atn and ATLatnp = :IMK19atnp;
125300100616                    clear V02dcmma;
125400100616              //?trovo l'appuntamento decodifico il commerciale
125500100616                    IF  sqlcod = 0;
125600130809                      chain (app_ATCcmm) AZCMM000;
125700130809                      IF  %found(AZCMM01L);
125800130809                        V02dcmma = CMMdes;
125900100616                      ENDIF;
126000100616                    ENDIF;
126100100615                  ENDIF;
126200091021
126300090610                  v02cmi = %editc(atccmm : 'X') ;
126400090610         // decodifica del commerciale
126500130809                  clear V02cmiD;
126600130809                  chain (ATCcmm) AZCMM000;
126700130809                  if  %found(AZCMM01L);
126800130809                    v02cmid = CMMdes;
126900090610                  endif;
127000090703           // ora inizio impegno
127100090703                  v02oii = atcoii ;
127200090703           // ora fine   impegno
127300090703                  v02ofi = atcofi ;
127400090610           // numero trattativa
127500090610                  v02vis = atcnrv ;
127600090610           // cliente e decodifica
127700091026                  v02cli = atcksc ;
127800090610                  clear  tibs69ds;
127900091026                  I69kac = v02cli;
128000090610                  TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
128100091026                  v02clid = ACOrag;
128200090610           // note contatto PRECEDENTE
128300090610                clear trmk20ds ;
128400090610                imk20tla = 'L';
128500090610                imk20flm = 'R';
128600090610                imk20cpo = cpocpo ;
128700090610                imk20rsc = v02cpd ;
128800090611                imk20tat = imk19tat ;
128900090611                imk20atn = imk19atn ;
129000090611                imk20atnp= imk19atnp -1;
129100090616                Trmk20r ( kpjba : trmk20ds );
129200090610                v02no1o =  emk20no1 ;
129300090610                v02no2o =  emk20no2 ;
129400090610                piunote = (omk20piu = '+');
129500090610
129600090610             endif ;
129700090610          // se si tratta di un contatto già eseguito
129800090611             if Imk19fatt = 'S' ;
129900090610         //  proteggo i campi di input
130000090610                giafatto = *on ;
130100090610
130200090610             // verifco    si tratta di un 1° contatto se data scadenza = 0
130300090610                primocontatto = (v02scd = 0) ;
130400090610
130500140411               // -?Data/Ora/Utente attività eseguita?
130600140414               clear  Data_Eur;
130700140414               clear  wData;
130800140414               clear  wHhMm;
130900140414               if  ATCdco > *zero;
131000140414                 Data_Eur = %date( ATCdco : *iso );
131100140414                 wData    = %dec( Data_Eur );
131200140414               endif;
131300140414               wHhMm = %int( ATChco / 100 );
131400140414               %subst( V02tit2 : %len(V02tit2) - 27 ) =
131500140414                       %editc( wData : 'Y' ) + ' ' +
131600140414                       %editw( wHhMm : '  :  ' ) + ' ' +
131700140414                       ATCpru;
131800090610             // recupero i dati relativi all'esecuzione del contatto
131900090610                v02com = %editc(atccco:'X') ;
132000130809                clear  V02comD;
132100130809                chain (ATCcco) AZCMM000;
132200130809                if  %found(AZCMM01L);
132300130809                  V02comd = CMMdes;
132400090610                endif;
132500090610             // data contatto
132600090610                data_eur=%date(atcdco:*iso);
132700090610                v02dat=%dec(data_eur);
132800090610             // ora  contatto
132900090610                v02ora=%dec(%subst(%editc(atchco:'X'):1:4):4:0);
133000090610             // causale  contatto
133100090610                v02cae = atccac ;
133200090610                clear  dCCO;
133300090610                clear  TIBS02ds;
133400090610                T02mod = 'C';
133500090610                T02cod = 'CCO';
133600090610                T02ke1 = v02cae ;
133700090610                T02sif = KNSIF;
133800090610                TNTBE_RicercaControllo  (kpjba : tibs02ds);
133900090610                if  T02err = *blanks;
134000090610                    dcco = t02uni ;
134100090610                    V02caed = §ccodes ;
134200090610                endif;
134300100223             // se appuntamento visualizzo  l'esito
134400100223                if atctat = 'A' ;
134500100223                   v02fatto = atcest ;
134600100223                endif ;
134700091021             // verifico generata una nuova trattativa da questa attività
134800091021                tratta_new = (atcnra > 0) ;
134900091021                v02nra = atcnra ;
135000100309             // verifico se c'è l'esito
135100100309                datc01 = atcflo ;
135200100309                vis_esito = (§atcesito <> ' ') ;
135300100309                If §atcesito = 'P'  ;
135400100309                   v02esi = 'POSITIVO' ;
135500100309                else ;
135600100309                   v02esi = 'NEGATIVO' ;
135700100309                endif ;
135800090610             // recupero note
135900090610                clear trmk20ds ;
136000090610                imk20tla = 'L';
136100090610                imk20flm = 'R';
136200090610                imk20cpo = cpocpo ;
136300090610                imk20rsc = v02cpd ;
136400090611                imk20tat = imk19tat ;
136500090611                imk20atn = imk19atn ;
136600090611                imk20atnp= imk19atnp;
136700090616                Trmk20r ( kpjba : trmk20ds );
136800090610                v02no1 =  emk20no1 ;
136900090610                v02no2 =  emk20no2 ;
137000090610                altrenote = (omk20piu = '+');
137100090610             endif ;
137200090610
137300090610         else ;
137400090610             primocontatto = *on ;
137500090610         endif ;
137600090610         // propongo alcuni campi  se si tratta di un contatto ancora da eseguire
137700090623         // se non si tratta di visualizzazione
137800090623
137900091214         If  Not Visualizza and
138000091214             ((IMK19fatt = *blanks and $dasbf) or Not $dasbf);
138100091214         //?Se non è appuntamento
138200091214           IF Not Appuntamento;
138300090610             // commerciale chi sono
138400091214                If imk19com > *zeros;
138500090714                   v02com  = %editc(imk19com : 'X')  ;
138600090714                   v02comd = imk19comd ;
138700090714                endif ;
138800090610            //  data e ora del momento
138900090610                v02dat = *date ;
139000090610                v02ora = %dec(%subst(%EDITC(%dec(%time()):'X'):1:4):4:0);
139100091214         //?Se è appuntamento
139200091214           ELSE;
139300091214         //?imposto il commerciale, data e ora dell'appuntamento
139400091214         //?inoltre vengono protetti il commerciale e la data
139500091214             v02com  = v02cmi;
139600091214             v02comd = v02cmid;
139700091214             v02dat  = v02scd;
139800091214             v02ora  = v02sco;
139900110302           ENDIF;
140000091214         // flag giafatto a 0
140100091214           giafatto = *off ;
140200090610         endif ;
140300090714
140400090714        // verifico se visualizzare oppure no cliente e trattativa
140500090714
140600091026         cliente   = (v02cli > 0) ;
140700090714         vis_trattativ = (v02vis > 0) ;
140800091119
140900091119        // verifico se esistono trattativa e lo visualizzo
141000100118        // oltre a quella legata all'attività in carico
141100091119                   clear tnta43ds ;
141200091119                   ita43cpo = cpocpo ;
141300091119                   ita43ksc = v02cli ;
141400091119                   ita43nrv = v02vis ;
141500091119                   Tnta43r (tnta43ds) ;
141600100118            // se l'attività in carico non ha la trattativa
141700100118            // e mi torna un n. di trattativa imposto la scritta
141800100118            IF  v02vis = 0 and ota43nrv > 0;
141900100118              evalr v02segn = 'TRATTATIVE APERTE';
142000100118            ENDIF;
142100100118            // se l'attività in carico ha la trattativa
142200100118            // e mi torna un n. di trattativa imposto la scritta
142300100119            IF  v02vis > 0 and ota43nrv > 0 and v02vis <> ota43nrv;
142400100118              evalr v02segn = 'ALTRE TRATTATIVE APERTE';
142500100118            ENDIF;
142600091216
142700091216         //?Centro titolo basso
142800091216         clear wpos;
142900091216         IF  Not PrimoContatto;
143000091216         wpos = (%len(v02tit2) - %len(c_Attivita)) / 2;
143100091216         %subst(v02tit2:wpos:%len(c_Attivita)) = c_Attivita;
143200091216         ENDIF;
143300091216         IF  PrimoContatto;
143400091216         wpos = (%len(v02tit2) - %len(c_PrimaAtt)) / 2;
143500091216         %subst(v02tit2:wpos:%len(c_PrimaAtt)) = c_PrimaAtt;
143600091216         ENDIF;
143700091216
143800091216          If visualizza = *on ;
143900091216             Vis_nonfatto = (Imk19Fatt = ' ');
144000091216          else ;
144100091216             Vis_nonfatto = *off ;
144200091216          endif ;
144300091215
144400091215         //?verifico se per potenziale/cliente ci sono attività aperte
144500100118         //?ma solo se non è già impostato il campo con trattativa aperte
144600100118         IF  v02segn = ' ';
144700091215           clear TRMK85ds;
144800091215           IMK85cpo = cpocpo;
144900091215           IMK85ksc = v02cli;
145000100118           IMK85nrv = v02vis;
145100100129           IMK85atn = IMK19atn;
145200091215           trmk85r (trmk85ds);
145300100118         //?se torna errore <> "9" ho trovato delle attività aperte
145400091215           IF  OMK85err <> *blanks and OMK85err <> '9';
145500091215         //?imposto la costante a video
145600100118             evalr v02segn = 'ALTRE ATTIVITA'' APERTE';
145700091215           ENDIF;
145800091215         ENDIF;
145900091215
146000091214         //?verifico se appuntamento con Affiancamento
146100091214         ConAffianca = *off;
146200091214         F20Affianca = *off;
146300091214         IF  Appuntamento and Not Affiancamento;
146400091214           k_atlatl  = IMK19atn;
146500091214           k_atlatlp = IMK19atnp;
146600091214           chain  %kds(K02tiatl) TIATL02L;
146700091214           IF  %found(Tiatl02l);
146800091216             ConAffianca = *on;
146900091214           ENDIF;
147000091210         ENDIF;
147100091216
147200091216         //?Non abilito F20=Affiancamento in gestione
147300091216         IF  Appuntamento and Manutenzione;
147400091216           F20Affianca = *on;
147500091216         ENDIF;
147600091216         IF  ConAffianca and Visualizza;
147700091216           F20Affianca = *on;
147800091216         ENDIF;
147900100519
148000100519         //?Abilito F16=Annullamento
148100100519         F16Attivo = *off;
148200100519         $okdel = *off;
148300100519         SELECT;
148400110214         //?se codice attività impedisce l'annullamento dell'attività disabilito F16
148500110519         //?e il cliente è perso e non ha attività
148600110519           WHEN  §ccoann = 'S' and Manutenzione ;
148700110519                 If Cpofls <> 'P' or (Cpofls = 'P' and v02segn <> *blank) ;
148800110519                    F16Attivo = *on;
148900110519                    $okdel = *on;
149000110519                 Endif ;
149100100519         //?se sono in gestione attività richiamato da gestione trattativa
149200100520           WHEN  Manutenzione and Ric_Trattativ;
149300100519             F16Attivo = *on;
149400100519             $okdel = *on;
149500110908         //?se sono in gestione attività non legata a trattativa
149600110908         //?attività su potenziale perso e ci sono altre attività aperte
149700110908           WHEN  Manutenzione and not Vis_Trattativ and CPOfls = 'P' and
149800110908                 V02segn <> *blanks;
149900110908             F16Attivo = *on;
150000100519         //?se sono in gestione attività non legata a trattativa
150100100519         //?attività su potenziale mai codificato e ci sono altre attività aperte
150200110225           WHEN  Manutenzione and not Vis_Trattativ and CPOfls = 'M' and
150300110225                 V02segn <> *blanks;
150400100519             F16Attivo = *on;
150500100520         //?se sono in gestione attività non legata a trattativa
150600100520         //?attività su potenziale codificato
150700110225           WHEN  Manutenzione and not Vis_Trattativ and CPOfls = 'C';
150800100520             F16Attivo = *on;
150900100519         //?se sono in gestione attività non legata a trattativa
151000100519         //?attività su cliente
151100100520           WHEN  Manutenzione and not Vis_Trattativ and v02cli > 0;
151200100519             F16Attivo = *on;
151300100519         ENDSL;
151400091216
151500091216         //?Se trattativa a video cerco la data della trattativa
151600091216         clear V02VisDat;
151700100504         clear w_vistpv ;
151800100614         clear V02VisTtr;
151900091216         IF  Vis_Trattativ;
152000100520           chain(n)  V02vis tivis05l;
152100091216           IF  %found(tivis05l);
152200091216             data_eur  = %date(visdat:*iso);
152300091216             V02VisDat = %dec(data_eur);
152400100504             w_vistpv = vistpv ;
152500100614         //?decodifico il tipo trattativa
152600100614             clear dTTR;
152700100614             clear TIBS02ds;
152800100614             T02mod = 'C';
152900100614             T02cod = 'TTR';
153000100614             T02ke1 = w_vistpv;
153100100614             T02sif = KNSIF;
153200100614             TNTBE_RicercaControllo  (kpjba : tibs02ds);
153300100614             IF  T02err = *blanks;
153400100614                 dTTR = t02uni;
153500100614                 V02VisTtr = §ttrdes;
153600100614             ENDIF;
153700091216           ENDIF;
153800091216         ENDIF;
153900091216
154000100114         PosCurFatto = *off;
154100091216         PosCurNo102 = *off;
154200091216         PosCurCom02 = *off;
154300091216
154400091216         //?Se non è manutenzione e non sto visualizzando un non fatto
154500100111         //?e non è interrogazione
154600100111         IF  Not Manutenzione and Not Vis_NonFatto and Not Visualizza;
154700100114           SELECT;
154800100114         //?Se il commerciale non è stato proposto posiziono il cursore sul commerciale
154900100114             WHEN  v02com = *zeros or V02com = *Blanks;
155000100114               PosCurCom02 = *on;
155100100114         //?Se è appuntamento mi posiziono sul 'Effettuato s/n'
155200100114             WHEN  Appuntamento;
155300100114               PosCurFatto = *on;
155400100114             OTHER;
155500100503         //?altrimenti posiziono il cursore sulla causale
155600100503               PosCurCae02 = *on;
155700100114           ENDSL;
155800091216         ENDIF;
155900100325
156000100325        // Recupero informazioni responsabile trasporti
156100100325
156200100325         exsr respD02 ;
156300091216
156400100310         $ForzaImpegno = *off;
156500100310         $ForzaComm    = *off;
156600100422         $ForzaComm3   = *off;
156700100310         $ForzaComin   = *off;
156800100318         $ForzaComin2  = *off;
156900100311         clear sav_v02cmi ;
157000100318         clear sav_v02cmi2;
157100100311         clear sav_w03com ;
157200100422         clear sav_w03comt;
157300090714
157400110225       //?Se sto gestendo un'attività ancora da fare
157500110225       //?e l'attività prevede il controllo del cliente "perso"
157600110301           IF  IMK19fatt <> 'S' and §CCOccp = 'S' and Gestione;
157700110527       // se attività 70 faccio un controllo se 71 un altro
157800110530             If  atccad = '71' ;
157900110225         //?Controllo se potenziale lavora o no
158000110527                 clear TRMK04DS;
158100110527                 clear TRMK85DS;
158200110527                 IMK04cpo = CPOcpo;
158300110527                 IMK85cpo = cpocpo;
158400110527                 IMK85ksc = v02cli;
158500110527                 IMK85nrv = v02vis;
158600110527                 IMK85atn = IMK19atn;
158700110527                 trmk04r (kpjba : TRMK04DS : TRMK85DS);
158800110225         //?Se il potenziale ha ripreso a lavorare devo emettere una win dove avviso
158900110527         //?di verificare le attività e trattative aperte
159000110527                 IF  OMK04lav = 'SI';
159100110527                    $Lavora = *on;
159200110527                    $InzD02 = *off;
159300110527                    $Video = 'W0';
159400110527                 ENDIF;
159500110527             ENDIF;
159600110527       // se attività 70 faccio un altro controllo
159700110530             If  atccad = '70' ;
159800110527         //?Controllo se potenziale lavora o no
159900110527                 clear TRMK04DS;
160000110530                 clear TRMK85DS;
160100110527                 IMK04cpo = CPOcpo;
160200110530                 trmk04r (kpjba : TRMK04DS : TRMK85DS);
160300110530         //?Se il potenziale ha ripreso a lavorare devo emettere una win nella quale
160400110530         //?avvisa che ci sono trattative aperte e annulla l'attività in corso
160500110530                 IF  OMK04lav = 'SI';
160600110527                    $Lavora = *on;
160700110527                    $InzD02 = *off;
160800110527                    $Video = 'W9';
160900110527                 ENDIF;
161000110527             ENDIF;
161100110225           ENDIF;
161200110214
161300090610       ENDSR;
161400090610
161500090610       //--------------------------------------------------------------
161600090610       //?Recupero dati anagrafici / resp / mail / stato potenziale
161700090610       //--------------------------------------------------------------
161800090610       BEGSR RecD02;
161900090610
162000090610
162100091019        // Recupero dati anagrafico potenziale o cliente in base ai dati passati
162200090610
162300091021         exsr AnagD02 ;
162400090610
162500110502              $Gia_Codifica = *off ;
162600100322           // verifico se codificato su altri codici
162700110502           //   chain cpocpo  CNACO16L;
162800110502           //    IF  %found(CNACO16L);
162900110502           //        $Gia_Codifica = *on ;
163000110502           //     endif ;
163100110502           // considero un potenziale già codificato se il falf del potenziale è uguale a "C"
163200110502              If cpofls = 'C' ;
163300110502                 $gia_codifica = *on ;
163400110502              Endif ;
163500110502           // decodifico la categoria del potenziale
163600110502                  clear  dCPO;
163700110502                  clear  TIBS02ds;
163800110502                  T02mod = 'C';
163900110502                  T02cod = 'CPO';
164000110502                  T02ke1 = cpofls ;
164100110502                  T02sif = KNSIF;
164200110502                  TNTBE_RicercaControllo  (kpjba : tibs02ds);
164300110502                  if  T02err = *blanks;
164400110502                      dcpo = t02uni ;
164500110502                  endif;
164600090610
164700090610
164800090610       ENDSR;
164900090610
165000090610       //--------------------------------------------------------------
165100091021       //?Recupero dati potenziale/cliente
165200090610       //--------------------------------------------------------------
165300091021       BEGSR AnagD02 ;
165400091020       // se esite il codice cliente recupero i dati anagrafici da lui
165500091020             If v02cli <> *zeros ;
165600091021                cliente = *on ;
165700091020                clear  tibs69ds;
165800091020                I69kac = v02cli;
165900091020                I69kin = v02cli;
166000091020                I69kcp = v02cli;
166100091020                TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
166200091020                v02clid = acorag ;
166300091020                cpocpo  = acolib ;
166400091020                v02cpi = indvia ;
166500091020                v02cpl = indcit ;
166600091020                v02cpt = indtel ;
166700091020                v02cpf = indtlf ;
166800091020                v02cpc = %editc(indcap:'X') ;
166900091020                v02cpp = indprv ;
167000091020                v02cpn = indsta ;
167100091020                v02cmp = clpage ;
167200091020             endif ;
167300091020
167400120622          clear dCPO01;
167500090610             chain(N)  cpocpo  TNCPO000;
167600140715             chain     cpocpo  TNCPO100;
167700140715             if  NOT %found(TNCPO11L);
167800140715               clear  CPO1tel;
167900140715               clear  CPO1fax;
168000140715             endif;
168100090610       // carico dati del potenziale
168200090610             v02cpo = %editc(cpocpo:'X');
168300090610             v02cpd = cporag ;
168400120622             dCPO01 = CPOrst;
168500091020       // se non esite il codice cliente recupero i dati anagrafici dal potenziale
168600091020             If v02cli =  *zeros ;
168700091021                cliente = *off;
168800091020                v02cpi = cpovia ;
168900091020                v02cpl = cpocit ;
169000140715                v02cpt = CPO1tel;
169100140715                v02cpf = CPO1fax;
169200091020                v02cpc = cpocap ;
169300091020                v02cpp = cpoprv ;
169400091020                v02cpn = cponaz ;
169500091020                v02cmp = cpocmm ;
169600091020             endif ;
169700090610
169800090610
169900091020        // recupero dedodifica dle commerciale
170000130809         clear  V02cmpD;
170100130809         chain (v02cmp) AZCMM000;
170200130809         if  %found(AZCMM01L);
170300130809           V02cmpd = CMMdes;
170400090610         endif;
170500090610
170600090610         ENDSR ;
170700090610
170800090610       //--------------------------------------------------------------
170900090610       //?Recupero responsabile trasporti e mail
171000090610       //--------------------------------------------------------------
171100090610       BEGSR RespD02 ;
171200090610
171300100511             clear v02rtr ;
171400100511             clear v02mrt ;
171500090610       // carico responsabile trasporti
171600100326             SELECT ;
171700100326
171800100326       // se c'è il cliente recupero dal cliente
171900100408             when v02cli > 0 ;
172000100326                 clear ntcnk1 ;
172100100326                 ntcapl = 'C' ;
172200100408                 ntcnk1 = %editc(dutkci: 'X') + %editc(v02cli:'X');
172300100329       // se aperta trattativa recupero da questa
172400100408             when v02nra > 0 ;
172500100329                 clear ntcnk1 ;
172600100329                 ntcapl = 'T' ;
172700100408                 ntcnk1 = %editc(dutkci: 'X') + %editc(v02nra:'X');
172800100326       // se c'è trattativa recupero da questa
172900100408             when v02vis > 0 ;
173000100326                 clear ntcnk1 ;
173100100326                 ntcapl = 'T' ;
173200100408                 ntcnk1 = %editc(dutkci: 'X') + %editc(v02vis:'X');
173300100326             other ;
173400100325                 clear ntcnk1 ;
173500100325                 ntcapl = 'P' ;
173600100325                 ntcnk1 = %editc(cpocpo:'X');
173700100326             endsl ;
173800100326
173900090610             ntctnt = '05';
174000090610             clear  ntcnk2 ;
174100091020          setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
174200091020          READE    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
174300090610          dow not %eof(tfntc01l)  ;
174400100329             vh2rtr = ntcrnt ;
174500090610             v02rtr = ntcrnt ;
174600091020             READE    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
174700090610             If  not %eof(tfntc01l) ;
174800090610               v02rtr = %trim(v02rtr) + ', ' + %trim(ntcrnt) ;
174900090610               leave ;
175000090610             else;
175100090610               leave ;
175200090610             endif ;
175300090610          enddo ;
175400090610
175500090610       // carico mail responsabile trasporti
175600090610             ntctnt = '06';
175700090610             clear  ntcnk2 ;
175800091020          chain    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
175900090610          if  %found(tfntc01l)  ;
176000090610             v02mrt = ntcrnt ;
176100090610          endif ;
176200090610
176300090610       ENDSR;
176400090610
176500090611       //--------------------------------------------------------------
176600090611       //?Controllo videata D02
176700090611       //--------------------------------------------------------------
176800090611       BEGSR CtrD02;
176900090611
177000090611            WindDspF = IndDspF;
177100090611            reset WindDspFb;
177200090611            IndDspF  = WindDspF;
177300090611
177400100308            $chiedi_conf = *off ;
177500090611
177600100721         // verifico se nel frattempo hanno già esitato l'attività
177700100721                UNLOCK TIATC01L;
177800100721                chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
177900100721                IF %FOUND(TIATC01L) AND ATCDCO > 0 ;
178000100721                   errMessage  = *on;
178100100721                   errGenerico = *on;
178200100721                   PosCurCom02 = *on;
178300100721                   V1Dmsg = $Msg(07);
178400100721                   LEAVESR ;
178500100721                ENDIF ;
178600090611         // Se sono in manutenzione devo fare i controlli per la sola parte realtiva alla
178700090611         // scadenza dell'attività
178800090611
178900090611           If manutenzione = *on ;
179000090611              exsr Ctrd02Scd ;
179100090611              leavesr;
179200090611           endif ;
179300090611
179400090611         // Controllo codice commerciale (? Ricerca)
179500090611           If    %scan('?' : v02com) > 0  ;
179600090611             errGenerico = *on;
179700090611             PosCurCom02 = *on;
179800130809             clear  TRMK43ds;
179900130809             iMK43fil  = DUTpou;
180000130809             TRMK43R (kpjba : TRMK43ds);
180100130809             if  oMK43err = *off  and  oMK43fxx = *blank;
180200130809               v02com  = %editc( oMK43cmm : 'X' );
180300130809               v02comd = oMK43des;
180400130809             endif;
180500090611             leavesr;
180600090611           endif;
180700090611
180800100210         // Commerciale che esegue il contatto  obbligatorio
180900100210           If  v02com = *blanks or v02com = *zeros;
181000100210             errMessage  = *on;
181100100210             errGenerico = *on;
181200100210             PosCurCom02 = *on;
181300100210             V1Dmsg = $Msg(02);
181400100210             leavesr;
181500100210           endif;
181600100210
181700100210         // Controllo commerciale che esegue il contatto
181800100210             IF  %check(digits:V02com) > 0;
181900100210               ErrMessage  = *on;
182000100210               ErrGenerico = *on;
182100100210               PosCurCom02 = *on;
182200100210               V1Dmsg      = $Msg(02);
182300100210               leavesr;
182400100210             ENDIF;
182500100210
182600100210           //?utente abilitato al commerciale
182700100210             clear TNTAA1DS;
182800120810             ITAA1tipo = 'M';
182900120810             ITAA1caut = '§utegtc';
183000100210             ITAA1cmm  = %dec(V02com:7:0);
183100100519             ITAA1abi = 'RA';
183200100210             kpjbu     = tntaa1ds;
183300100210             tntaa1c (kpjba);
183400100210             TNTAA1DS = kpjbu;
183500100210             IF  OTAA1fabi = 'N';
183600100210               ErrMessage  = *on;
183700100210               ErrGenerico = *on;
183800100210               PosCurCom02 = *on;
183900100210               V1Dmsg      = $Msg(02);
184000100210               leavesr;
184100100210             ENDIF;
184200100210
184300130809             CMMcod = %int(V02com);
184400130809             chain (CMMcod) AZCMM000;
184500130809             if  NOT  %found(AZCMM01L);
184600090611               errMessage  = *on;
184700090611               errGenerico = *on;
184800090611               PosCurCom02 = *on;
184900090611               V1Dmsg = $Msg(02);
185000090611               leavesr;
185100100210             endif;
185200130809             V02comd = CMMdes;
185300100416
185400110525
185500130809             IF  CMMPAR <> ' ' ;
185600110525               ErrMessage  = *on;
185700110525               ErrGenerico = *on;
185800110525               PosCurCom02 = *on;
185900110525               V1Dmsg      = $Msg(02);
186000110525              leavesr;
186100110525             ENDIF;
186200111223
186300111223       //?Commerciale "valido" data inizio e fine attività
186400130809         IF  CMMdtIni > wOggi  or
186500130809             CMMdtFin < wOggi;
186600111223           ErrMessage = *on;
186700111223           ErrGenerico = *on;
186800111223           PosCurCom02 = *on;
186900111223           V1Dmsg = $Msg(02);
187000111223           leavesr;
187100111223         ENDIF;
187200110525
187300100416         // Se appuntamento devo controllare anche se è stato inserito il FATTO
187400100416           IF  Appuntamento and V02fatto = *blanks;
187500100416             ErrGenerico = *on;
187600100416             ErrMessage  = *on;
187700100416             PosCurFatto = *on;
187800100416             V1Dmsg      = $msg(21);
187900100416             leavesr;
188000100416           ENDIF;
188100100416
188200090611
188300100601         // controllo data esecuzione contatto solo se non sono in appuntamento non eseguito
188400090611           If    V02dat <> 0;
188500090611             clear wlbdat;
188600090611             g02dat = v02dat;
188700090611             xsrda8(wlbdat);
188800090611             if g02err = '1';
188900090611               errMessage  = *on;
189000090611               errGenerico = *on;
189100090611               PosCurDat02 = *on;
189200090611               V1Dmsg = $Msg(03);
189300090611               leavesr;
189400090611             endif;
189500090611
189600090611             v02dat = g02dat;
189700090611             w02dat = g02inv;
189800090611           else ;
189900090611             errMessage  = *on;
190000090611             errGenerico = *on;
190100090611             PosCurDat02 = *on;
190200090611             V1Dmsg = $Msg(03);
190300090611             leavesr;
190400090611           endif;
190500100118
190600100601           If  (appuntamento and v02fatto =  'S') or appuntamento = *off ;
190700100118         // controllo che la data non sia inferiore al 2000 o superiore al 2039
190800100118         IF  w02dat < 20000101 or w02dat > 20391231;
190900100118           errMessage  = *on;
191000100118           errGenerico = *on;
191100100118           PosCurDat02 = *on;
191200100118           V1Dmsg = $Msg(03);
191300100118           leavesr;
191400100118         ENDIF;
191500090611
191600100330       // controllo che la data non sia maggiore di oggi ed inferiore a 3 gg lavorativi da oggi
191700100330         clear XGIOLAVDS;
191800100330         IXGLdata = %dec(%date());
191900100330         IXGLsub  = 'S';
192000100330         IXGLgg   = 3;
192100100330         IXGLpa   = 'P';
192200100330         IXGLlav  = 'S';
192300100330         IXGLfil = %dec(%subst(V02com:1:3):3:0);
192400100330         XGIOLAV (xgiolavds);
192500100330         IF  w02dat < OXGLdata;
192600100330           ErrMessage  = *on;
192700100330           ErrGenerico = *on;
192800100330           PosCurDAT02 = *on;
192900100330           V1dmsg      = $Msg(03);
193000100330           V1dmsg      = %trim(V1dmsg) + ' ' +
193100100330           'Non può essere inferiore a 3 gg.lavorativi';
193200100330           leavesr;
193300100330         ENDIF;
193400100330
193500100330         // controllo con la data del giorno
193600100330            data_iso = (%date()) ;
193700100408         IF  w02dat > %dec(data_iso) ;
193800100330           ErrMessage  = *on;
193900100330           ErrGenerico = *on;
194000100330           PosCurDAT02 = *on;
194100100330           V1dmsg      = $Msg(03);
194200100330           V1dmsg      = %trim(V1dmsg) + ' ' +
194300100330           'Non può essere maggiore di oggi';
194400100330           leavesr;
194500100330         ENDIF;
194600100330
194700090611         // controllo ora  esecuzione contatto
194800090611           If V02ora <> 0 ;
194900090611              sav02ora = %editc(v02ora:'X');
195000120105              If  ora > 23 or minuti > 59;
195100090611                   errMessage  = *on;
195200090611                   errGenerico = *on;
195300090611                   PosCurora02 = *on;
195400090611                   V1Dmsg = $Msg(05);
195500090611                   leavesr;
195600090611              endif;
195700091104           else ;
195800091104              errMessage  = *on;
195900091104              errGenerico = *on;
196000091104              PosCurora02 = *on;
196100091104              V1Dmsg = $Msg(05);
196200091104              leavesr;
196300090611           endif;
196400100114
196500100416           endif ;
196600100114
196700091116
196800090611         // - controllo / ricerca Causale contatto
196900090611           If    %scan('?' : V02cae) > *zero;
197000090611             errGenerico = *on;
197100090611             PosCurCae02 = *on;
197200090611             clear  dCCO ;
197300090611             clear  Tntb74ds;
197400091218             iccouti = 'S';
197500100225          // imposto anche il tipo attività che sto eseguendo x selezionare le sole
197600100225          // causali possibili
197700100225             iccoese = imk19tat ;
197800100322          // imposto anche se sto eseguendo attività con il solo codice cliente oppure
197900100322          // potenziale mai codificato , e se c'è trattativa
198000110429          //   If v02cli > 0 ;
198100110429          //      iccocod = 'C' ;
198200110429          //   else ;
198300110429          //      if $Gia_codifica = *on ;
198400110429          //         iccocod = 'C' ;
198500110429          //      else ;
198600110429          //         iccocod = 'P' ;
198700110429          //      endif ;
198800110429          //   endif;
198900100322
199000110429          // imposto la cateoria del potenziale
199100110502               iccocod = cpofls ;
199200110429
199300100322             If v02vis > 0 ;
199400100322                iccotra = 'S' ;
199500100505                iccotpv = W_vistpv ;
199600100322             else ;
199700100322                iccotra = 'N' ;
199800100322             endif ;
199900100324           // verifico le offerte
200000100330             iccooff = 'N';
200100101220             setll v02vis tivof11l ;
200200101220             reade v02vis tivof11l ;
200300101220             dow not %eof(tivof11l) ;
200400100421                 if vofeso <> 'H' and vofeso <> '*';
200500100330                    iccooff = 'S';
200600100330                    leave ;
200700100330                 endif ;
200800101220                 reade v02vis tivof11l ;
200900100330             enddo ;
201000110412           // passo la causale al pgm di ricerca
201100100421                iccocco = atccad ;
201200100322
201300090611             TNTB74R  (kpjba : tntb74ds);
201400090611             if  occoerr = *off   ;
201500090611               V02cae = Occoke1;
201600091218               dcco   = Occouni;
201700090611               V02caed = §ccodes ;
201800090611               errGenerico = *on;
201900090611               leavesr;
202000090611             else;
202100090611               errMessage  = *on;
202200090611               errGenerico = *on;
202300090611               V1Dmsg = T02msg;
202400090611               leavesr;
202500090611             endif;
202600090611           endif;
202700090611
202800090611           If    V02cae <> *blanks  ;
202900090611             clear  dCCO;
203000090611             clear  TIBS02ds;
203100090611             T02mod = 'C';
203200090611             T02cod = 'CCO';
203300090611             T02ke1 = v02cae ;
203400090611             T02sif = KNSIF;
203500090611             TNTBE_RicercaControllo  (kpjba : tibs02ds);
203600090611             if  T02err = *blanks;
203700090611               dcco = t02uni ;
203800100225               §ccoTAAds = §CCOtaa;
203900100505               §ccoTPVds = §CCOtpv;
204000110502               §ccoCPOds = §CCOcpo;
204100090611               V02caed = §ccodes ;
204200090702           // verifico se causale contatto può essere utilizzata dall'utente
204300090702                If   §ccouti = 'N' ;
204400090702                   errMessage  = *on;
204500090702                   errGenerico = *on;
204600090702                   PosCurCae02 = *on;
204700090702                   V1Dmsg = $msg(14);
204800090702                   leavesr;
204900090702                endif;
205000090611             else;
205100090611               errMessage  = *on;
205200090611               errGenerico = *on;
205300090611               PosCurCae02 = *on;
205400090611               V1Dmsg = T02msg;
205500090611               leavesr;
205600090611             endif;
205700090611           else;
205800090611             errMessage  = *on;
205900090611             errGenerico = *on;
206000090611             PosCurCae02 = *on;
206100090611             V1Dmsg = $msg(08);
206200090611             leavesr;
206300090611           endif;
206400110405
206500110405           // verifico se causale può essere utilizzata in base al flag cliente che non spedisce
206600110405           If  $cli_non_sped = *on and §CCOucns = 'N' ;
206700110405                   errMessage  = *on;
206800110405                   errGenerico = *on;
206900110405                   PosCurCae02 = *on;
207000110502                   IF ATCcad <> '  ' ;
207100110502                      V1Dmsg = $msg(36);
207200110502                      %subst(V1Dmsg:12:2) = V02cae;
207300110502                      %subst(V1Dmsg:66:2) = ATCcad;
207400110502                   else ;
207500110502                      V1Dmsg = $msg(14);
207600110502                   Endif ;
207700110405                   leavesr;
207800110405           endif;
207900110502
208000110502           // verifico se causale può essere utilizzata in base al flag cliente che non spedisce
208100110502           If  $cli_non_sped = *off and §CCOucns = 'S' ;
208200110502                   errMessage  = *on;
208300110502                   errGenerico = *on;
208400110502                   PosCurCae02 = *on;
208500110502                   IF ATCcad <> '  ' ;
208600110502                      V1Dmsg = $msg(36);
208700110502                      %subst(V1Dmsg:12:2) = V02cae;
208800110502                      %subst(V1Dmsg:66:2) = ATCcad;
208900110502                   else ;
209000110502                      V1Dmsg = $msg(14);
209100110502                   Endif ;
209200110502                   leavesr;
209300110502           endif;
209400100421
209500110321         //?Controllo se la causale di esecuzione può essere utilizzata per l'attività che sto
209600110321         //?gestendo
209700110321           IF  §CCOcae <> *blanks and §CCOcae <> ATCcad;
209800110321             ErrMessage  = *on;
209900110321             ErrGenerico = *on;
210000110321             PosCurCae02 = *on;
210100110502             IF ATCcad <> '  ' ;
210200110502                V1Dmsg = $msg(36);
210300110502                %subst(V1Dmsg:12:2) = V02cae;
210400110502                %subst(V1Dmsg:66:2) = ATCcad;
210500110502             else ;
210600110502                V1Dmsg = $msg(14);
210700110502             Endif ;
210800110321             leavesr;
210900110321           ENDIF;
211000100421
211100110428           // verifico se causale da esitare è la 32 deve usare solo la 42
211200110428              If atccad = '32' and v02cae <> '42' ;
211300110428                errMessage  = *on;
211400110428                errGenerico = *on;
211500110428                PosCurCae02 = *on;
211600110428                V1Dmsg = $msg(33);
211700110428                leavesr;
211800110428              endif ;
211900110428
212000110428           // verifico se causale da esitare è la 71 deve usare solo la 72
212100110428              If atccad = '71' and v02cae <> '72' ;
212200110428                errMessage  = *on;
212300110428                errGenerico = *on;
212400110428                PosCurCae02 = *on;
212500110502                IF ATCcad <> '  ' ;
212600110502                   V1Dmsg = $msg(36);
212700110502                   %subst(V1Dmsg:12:2) = V02cae;
212800110502                   %subst(V1Dmsg:66:2) = ATCcad;
212900110502                else ;
213000110502                   V1Dmsg = $msg(14);
213100110502                Endif ;
213200110428                leavesr;
213300110428              endif ;
213400110428
213500100225           // verifico se causale può essere utilizzata per esitare questo tipo attività
213600100225           If  §CCOtaa <> *blanks and
213700100225                 %lookup(imk19tat : $Taa) = *zero ;
213800100225                   errMessage  = *on;
213900100225                   errGenerico = *on;
214000100225                   PosCurCae02 = *on;
214100100225                   V1Dmsg = $msg(23);
214200100225                   leavesr;
214300100225           endif;
214400100322
214500100322           // verifico se causale può essere utilizzata in presenza di trattative con offerte
214600100322           If  §CCOoff = 'S' and v02vis > 0 ;
214700101220                   setll v02vis tivof11l ;
214800100322                   If not %equal ;
214900100322                      errMessage  = *on;
215000100322                      errGenerico = *on;
215100100322                      PosCurCae02 = *on;
215200100322                      V1Dmsg = $msg(31);
215300100322                      leavesr;
215400100421                   else ;
215500101220                      reade v02vis tivof11l ;
215600101220                      dow not %eof(tivof11l) ;
215700100421                          if vofeso  =  '*' ;
215800100421                             errMessage = *on ;
215900100421                          else ;
216000100421                             errMessage = *off;
216100100421                             leave ;
216200100421                          endif ;
216300101220                      reade v02vis tivof11l ;
216400100421                      enddo ;
216500100421                   // se ho trovato tutti annullati emetto errore
216600100421                      If errMessage  = *on;
216700100421                         errGenerico = *on;
216800100421                         PosCurCae02 = *on;
216900100421                         V1Dmsg = $msg(31);
217000100421                         leavesr;
217100100421                      endif ;
217200100322                   endif ;
217300100322           endif;
217400100322
217500100322           // verifico se causale non può essere utilizzata in presenza di trattative con offerte
217600100225           If  §CCOoff = 'N' and v02vis > 0 ;
217700101220                   setll v02vis tivof11l ;
217800101220                   reade v02vis tivof11l ;
217900101220                   dow not %eof(tivof11l) ;
218000100421                       if vofeso <> 'H' and vofeso <> '*' ;
218100100309                          errMessage  = *on;
218200100309                          errGenerico = *on;
218300100309                          PosCurCae02 = *on;
218400100309                          V1Dmsg = $msg(24);
218500100309                          leavesr;
218600100309                       endif ;
218700101220                   reade v02vis tivof11l ;
218800100309                   enddo ;
218900100225           endif;
219000100505           // verifico se causale in presenza di trattativa può essere usata per solo un
219100100505           // alcuni tipi trattativa
219200100505           If      §ccotpv <> *blanks  and %lookup(W_vistpv : $tpv) = *zero ;
219300100505                          errMessage  = *on;
219400100505                          errGenerico = *on;
219500100505                          PosCurCae02 = *on;
219600100505                          V1Dmsg = %trimr($msg(35)) + ' ' + W_vistpv ;
219700100505                          leavesr;
219800100505           endif ;
219900100308
220000100308           // verifico se causale può essere utilizzata solo per codificati/mai codificati o tutti
220100100308
220200100308           // solo causali per clienti codificati
220300110502           // If  §CCOcod = 'C' and V02cli = *zeros ;
220400110502           If  §CCOcpo <> *blanks and %lookup(cpofls:$cpo)= 0 ;
220500100308               errMessage  = *on;
220600100308               errGenerico = *on;
220700100308               PosCurCae02 = *on;
220800100308               V1Dmsg = $msg(25);
220900110504               v1dmsg = %subst(V1Dmsg:  1: 40) + '"' + §cpodesc + '"' ;
221000100308               leavesr;
221100100308           endif ;
221200090611
221300100308
221400100308           // verifico se causale non deve essere legata a trattativa
221500100308           If  §CCOtra = 'N' and V02vis >  *zeros ;
221600100308               errMessage  = *on;
221700100308               errGenerico = *on;
221800100308               PosCurCae02 = *on;
221900100308               V1Dmsg = $msg(27);
222000100308               leavesr;
222100100308           endif;
222200100308
222300100309            // verifico se causale chiude trattativa e l'attività è legata a trattativa
222400100309             if  §ccoCHT = 'S' and v02vis > 0 ;
222500100520                   chain(n)  V02vis tivis05l;
222600100308                   // se trattativa già esitata errore
222700100308                   IF  %found(tivis05l) and visesi = 'P ' ;
222800100308                       errMessage  = *on;
222900100308                       errGenerico = *on;
223000100308                       PosCurCae02 = *on;
223100100308                       V1Dmsg = $msg(28);
223200100308                       leavesr ;
223300100308                   else ;
223400100308                       $chiedi_conf = *on ;
223500100308                   endif ;
223600100308             endif ;
223700090611
223800091118            // pulisco il flag della trattativa nuova se non devo aprire trattativa
223900091118                if §ccoapt = ' ' ;
224000091118                    $new_trattati = *off ;
224100091118                endif ;
224200100308            // verifico se causale vuole le note
224300100308            If §cconot = 'S' and v02no1 = *blanks and v02no2 = *blanks ;
224400100308                   errMessage  = *on;
224500100308                   errGenerico = *on;
224600100308                   PosCurNo102 = *on;
224700100308                   V1Dmsg = $msg(29);
224800100308                   leavesr;
224900100308            endif ;
225000100318
225100100318            // verifico se causale non deve essere legata a trattativa do errore
225200100520                if    §ccotra = 'S' and v02vis = 0 and §ccoapt <> 'S';
225300100318                   errMessage  = *on;
225400100318                   errGenerico = *on;
225500100318                   PosCurCae02 = *on;
225600100318                   V1Dmsg = $msg(30);
225700100318                   leavesr;
225800100318                endif ;
225900100318
226000100329
226100100329            // verifico se causale legata a trattativa, se non è un'attività con trattativa
226200100329            // la creo nuova
226300100329                if    (§ccotra = 'S' and v02vis = 0) or
226400100329                      (§ccotra <>'S' and v02vis = 0) ;
226500100329                  clear newnrv;
226600100329                  clear sav_newnrv;
226700100329                endif ;
226800100329
226900100318            // verifico se causale legata a trattativa, se non è un'attività con trattativa
227000100318            // la creo nuova altrimenti propongo la trattativa dell'attività
227100100318                if    §ccotra = 'S' and v02vis > 0 ;
227200100318                      newnrv = v02vis   ;
227300100318                      sav_newnrv = v02vis   ;
227400100318                      leavesr;
227500100318                endif ;
227600100118
227700100118            // verifico se causale apre sempre trattativa e se ce nè già do errore
227800100118                if    §ccoapt = 'S' and v02vis > 0 ;
227900100118                   errMessage  = *on;
228000100118                   errGenerico = *on;
228100100118                   PosCurCae02 = *on;
228200100118                   V1Dmsg = $msg(19);
228300100118                   leavesr;
228400100118                endif ;
228500100118
228600100118            // se passati tutti i controlli e devo aprire trattativa recupero
228700100118            // il numero trattativa
228800100118            // se non avevo già salvato prima il n. recuperato
228900100202            if newnrv = 0 and §ccoapt = 'S'  and sav_newnrv = 0;
229000091113            exsr Sr_Recnum ;
229100091113            if  $ErrGrave = *on;
229200091113                leavesr;
229300091113            endif;
229400091113            endif;
229500100118
229600100118            if sav_newnrv > 0 and §ccoapt = 'S';
229700100118              newnrv = sav_newnrv;
229800100118              $new_trattati = sav_newtrat;
229900100118            endif;
230000100118
230100090611       ENDSR;
230200090611
230300091113       //--------------------------------------------------------------
230400091113       //?Recupero numero trattativa nuova
230500091113       //--------------------------------------------------------------
230600091113       BEGSR sr_Recnum;
230700091113
230800100413                reset $NumOk ;
230900100413
231000100413                RESET trul48ds;
231100100413                trul48r(kpjba:trul48ds);
231200100413
231300100413                If o48Err = 0;
231400100413                   newnrv  = o48nrv;
231500100413                   sav_newnrv  = o48nrv;
231600100413                   $NumOk = *on ;
231700100413                else ;
231800100413                   $ErrGrave   = *on;
231900100413                   errMessage  = *on;
232000100413                   errGenerico = *on;
232100100413                   V1Dmsg = o48msg ;
232200100413                   leavesr;
232300100413                Endif;
232400100413
232500091116        // se trovato un nuovo numero lo segnalo
232600091116                 If   $NumOk = *on ;
232700091116                   $New_trattati = *on ;
232800100118                   sav_newtrat = *on ;
232900091116                 endif ;
233000091113
233100091113       endsr;
233200091113
233300090611       //--------------------------------------------------------------
233400090611       //?Controllo videata D02         SCADENZA ATTIVITA'
233500090611       //--------------------------------------------------------------
233600090611       BEGSR CtrD02Scd ;
233700090611
233800090611         // controllo data attività da fare
233900090611           If    V02scd <> 0;
234000090611             clear wlbdat;
234100090611             g02dat = v02scd;
234200090611             xsrda8(wlbdat);
234300090611             if g02err = '1';
234400090611               errMessage  = *on;
234500090611               errGenerico = *on;
234600090611               PosCurScd02 = *on;
234700090611               V1Dmsg = $Msg(03);
234800090611               leavesr;
234900090611             endif;
235000090611
235100090611             v02Scd = g02dat;
235200090611             w02Scd = g02inv;
235300090611           else ;
235400090611             errMessage  = *on;
235500090611             errGenerico = *on;
235600090611             PosCurScd02 = *on;
235700090611             V1Dmsg = $Msg(03);
235800090611             leavesr;
235900090611           endif;
236000100118
236100100118         // controllo che la data non sia inferiore al 2000 o superiore al 2039
236200100118         IF  w02scd < 20000101 or w02scd > 20391231;
236300100118           errMessage  = *on;
236400100118           errGenerico = *on;
236500100118           PosCurscd02 = *on;
236600100118           V1Dmsg = $Msg(03);
236700100118           leavesr;
236800100118         ENDIF;
236900090611
237000090611         // controllo ora  esecuzione contatto
237100090611           If V02Sco <> 0 ;
237200090611              sav02ora = %editc(v02Sco:'X');
237300120105              If  ora > 23 or minuti > 59;
237400090611                   errMessage  = *on;
237500090611                   errGenerico = *on;
237600090611                   PosCurSco02 = *on;
237700090611                   V1Dmsg = $Msg(05);
237800090611                   leavesr;
237900090611              endif;
238000090611           endif;
238100090611
238200090611         // Controllo codice commerciale (? Ricerca)
238300090611           If    %scan('?' : v02cmi) > 0  ;
238400090611             errGenerico = *on;
238500090611             PosCurCmi02 = *on;
238600130809             iMK43fil  = DUTpou;
238700130809             TRMK43R (kpjba : TRMK43ds);
238800130809             if  oMK43err = *off  and  oMK43fxx = *blank;
238900130809               v02cmi  = %editc( oMK43cmm : 'X' );
239000130809               v02cmid = oMK43des;
239100130809             endif;
239200090611             leavesr;
239300090611           endif;
239400100210
239500100210         // Commerciale che esegue il contatto obbligatorio
239600100210           If  v02cmi = *blanks or v02cmi = *zeros;
239700100210             errMessage  = *on;
239800100210             errGenerico = *on;
239900100210             PosCurCmi02 = *on;
240000100210             V1Dmsg = $Msg(02);
240100100210             leavesr;
240200100210           endif;
240300090611
240400090611         // Controllo codice commerciale che esegue il contatto
240500100210             IF  %check(digits:V02cmi) > 0;
240600100210               ErrMessage  = *on;
240700100210               ErrGenerico = *on;
240800100210               PosCurCmi02 = *on;
240900100210               V1Dmsg      = $Msg(02);
241000100210               leavesr;
241100100210             ENDIF;
241200100210
241300100210
241400100210           //?utente abilitato al commerciale
241500100721           //?lo controllo solo se attività NON legata a trattative e NON su cliente
241600100721             IF  V02vis <> 0 or V02cli <> 0;
241700100210             clear TNTAA1DS;
241800120810             ITAA1tipo = 'M';
241900120810             ITAA1caut = '§utegtc';
242000100210             ITAA1cmm  = %dec(V02cmi:7:0);
242100100519             ITAA1abi = 'RA';
242200100210             kpjbu     = tntaa1ds;
242300100210             tntaa1c (kpjba);
242400100210             TNTAA1DS = kpjbu;
242500100210             IF  OTAA1fabi = 'N';
242600100210               ErrMessage  = *on;
242700100210               ErrGenerico = *on;
242800100210               PosCurCmi02 = *on;
242900100210               V1Dmsg      = $Msg(02);
243000100210               leavesr;
243100100210             ENDIF;
243200100721             ENDIF;
243300100210
243400130809             CMMcod = %int(V02cmi);
243500130809             chain (CMMcod) AZCMM000;
243600130809             if  NOT  %found(AZCMM01L);
243700090611               errMessage  = *on;
243800090611               errGenerico = *on;
243900090611               PosCurCmi02 = *on;
244000090611               V1Dmsg = $Msg(02);
244100090611               leavesr;
244200100210             endif;
244300130809             V02cmid = CMMdes;
244400110525
244500130809             If  CMMPar <> ' ' ;
244600110525               ErrMessage  = *on;
244700110525               ErrGenerico = *on;
244800110525               PosCurCmi02 = *on;
244900110525               V1Dmsg      = $Msg(02);
245000110525              leavesr;
245100110525             ENDIF;
245200111223
245300111223       //?Commerciale "valido" data inizio e fine attività
245400130809         IF  CMMdtIni > wOggi  or
245500130809             CMMdtFin < wOggi;
245600111223           ErrMessage = *on;
245700111223           ErrGenerico = *on;
245800111223           PosCurCmi02 = *on;
245900111223           V1Dmsg = $Msg(02);
246000111223           leavesr;
246100111223         ENDIF;
246200100317
246300100317
246400100317         // Controllo se causale vuole controllo su commerciale interno
246500100318          // imposto flag di forzabile
246600100318               If sav_v02cmi2 <> v02cmi ;
246700100318                  $forzacomin2 = *off ;
246800100318               endif ;
246900130809            If §ccocoi <> ' ' and (CMMfun =  'COMIN' or CMMfun = 'ASC'
247000130809                                                     or CMMfun = 'SA');
247100100317         // verifico se msg forzabile
247200100318               If §ccocoi = 'F' and $forzacomin2= *off ;
247300100317                  errMessage  = *on;
247400100317                  errGenerico = *on;
247500100318                  PosCurCmi02 = *on;
247600100318                  V1Dmsg = %trimr($Msg(06)) + ' Enter per forzare' ;
247700100318                  $forzacomin2= *on ;
247800100318                  sav_v02cmi2 = v02cmi ;
247900100317                  leavesr;
248000100318               Endif;
248100100318               If §ccocoi <> 'F' ;
248200100317                  errMessage  = *on;
248300100317                  errGenerico = *on;
248400100318                  PosCurCmi02 = *on;
248500100318                  V1dmsg = $Msg(06) ;
248600100317                  leavesr;
248700100317               Endif ;
248800100317             endif;
248900100126
249000100330
249100100330       // controllo che la data non sia inferiore a 3 gg lavorativi da oggi
249200100330         clear XGIOLAVDS;
249300100330         IXGLdata = %dec(%date());
249400100330         IXGLsub  = 'S';
249500100330         IXGLgg   = 3;
249600100330         IXGLpa   = 'P';
249700100330         IXGLlav  = 'S';
249800100330         IXGLfil = %dec(%subst(V02cmi:1:3):3:0);
249900100330         XGIOLAV (xgiolavds);
250000100330         IF  w02scd < OXGLdata;
250100100330           ErrMessage  = *on;
250200100330           ErrGenerico = *on;
250300100330           PosCurScd02 = *on;
250400100330           V1dmsg      = $Msg(03);
250500100330           V1dmsg      = %trim(V1dmsg) + ' ' +
250600100330           'Non può essere inferiore a 3 gg.lavorativi';
250700100330           leavesr;
250800100330         ENDIF;
250900100330
251000100330       // controllo che la data non sia maggiore a 14 mesi  da oggi
251100100330         clear XGIOLAVDS;
251200100330         IXGLdata = %dec(%date());
251300100330         IXGLadd  = 'S';
251400100330         IXGLmm   = 14;
251500100330         IXGLpa   = 'P';
251600100330         IXGLfil = %dec(%subst(V02cmi:1:3):3:0);
251700100330         XGIOLAV (xgiolavds);
251800100330         IF  w02scd > OXGLdata;
251900100330           ErrMessage  = *on;
252000100330           ErrGenerico = *on;
252100100330           PosCurScd02 = *on;
252200100330           V1dmsg      = $Msg(03);
252300100330           V1dmsg      = %trim(V1dmsg) + ' ' +
252400100330           'Non può essere maggiore di 14 mesi';
252500100330           leavesr;
252600100330         ENDIF;
252700100330
252800100330
252900090804         // In caso di appuntamento controllo anche i campi seguenti
253000090804
253100090804       If Appuntamento = *on ;
253200090804
253300100310         //?Verifico se il commerciale dell'attività è diverso dal
253400100310         //?commerciale della trattativa
253500100310         //?se attività legata a trattativa
253600100310         // solo se si tratta di appuntamento controllo il commerciale
253700100310         // inoltre se vengo da trattativa il messaggio è forzabile
253800100310         IF  vis_trattativ  ;
253900100310           clear tnta45ds ;
254000100310           ITA45tip = 'C';
254100100310           ITA45nrv = v02vis;
254200100310           ITA45cmm = %dec(v02cmi:7:0);
254300100310           tnta45r (tnta45ds) ;
254400100310           IF  OTA45err = 'C';
254500100311          // imposto flag di forzabile
254600100311               If sav_v02cmi <> v02cmi ;
254700100311                  $forzacomm = *off ;
254800100311               endif ;
254900100310       // se vengo da trattativa il msg è forzabile
255000100310             If $forzacomm = *off ;
255100100310                ErrMessage  = *on;
255200100310                ErrGenerico = *on;
255300100310                PosCurCmi02 = *on;
255400100310                If imk19fpr = 'T' ;
255500100310                   V1Dmsg = %trimr(OTA45msg) + ' Enter per forzare';
255600100310                   $forzacomm = *on ;
255700100311                   sav_v02cmi = v02cmi ;
255800100310                else ;
255900100310                   V1Dmsg = OTA45msg;
256000100310                leavesr;
256100100310                Endif ;
256200100310             Endif ;
256300100310           ENDIF;
256400100310         ENDIF;
256500100310
256600090804        // verifico l'inserimento delle ore di impegno
256700090804
256800090804         // controllo ora  inizio impegno
256900090804           If V02oii <> 0 ;
257000090804              sav02ora = %editc(V02oii:'X');
257100120105              If  ora > 23 or minuti > 59;
257200090804                   errMessage  = *on;
257300090804                   errGenerico = *on;
257400090804                   PosCurOii02 = *on;
257500090804                   V1Dmsg = $Msg(05);
257600090804                   leavesr;
257700090804              endif;
257800091026           else ;
257900091026              errMessage  = *on;
258000091026              errGenerico = *on;
258100091026              PosCurOii02 = *on;
258200091111              V1Dmsg = $Msg(17);
258300091026              leavesr;
258400090804           endif;
258500090804
258600090804         // controllo ora  fine   impegno
258700090804           If V02ofi <> 0 ;
258800090804              sav02ora = %editc(V02ofi:'X');
258900120105              If  ora > 23 or minuti > 59;
259000090804                   errMessage  = *on;
259100090804                   errGenerico = *on;
259200090804                   PosCurOfi02 = *on;
259300090804                   V1Dmsg = $Msg(05);
259400090804                   leavesr;
259500090804              endif;
259600091026           else ;
259700091026              errMessage  = *on;
259800091026              errGenerico = *on;
259900091026              PosCurOfi02 = *on;
260000091111              V1Dmsg = $Msg(17);
260100091026              leavesr;
260200090804           endif;
260300090804
260400090804         // controllo ora  inizio se congruente con fine impegno
260500090804           If V02oii > 0 and V02ofi = 0;
260600090804              errMessage  = *on;
260700090804              errGenerico = *on;
260800090804              PosCurOfi02 = *on;
260900090804              V1Dmsg = $Msg(13);
261000090804              leavesr;
261100090804           endif;
261200090804
261300090804         // controllo ora  inizio se congruente con fine impegno
261400090804           If V02oii > 0 and V02ofi > 0 and V02oii > V02ofi ;
261500090804              errMessage  = *on;
261600090804              errGenerico = *on;
261700090804              PosCurOfi02 = *on;
261800090804              V1Dmsg = $Msg(13);
261900090804              leavesr;
262000090804           endif;
262100090804
262200090804         // verifico se inizioe fine impegno è congruente con ora appuntamento
262300090804           If V02oii > 0 and V02ofi > 0 and (V02oii > V02sco or
262400090804              V02ofi < V02sco) ;
262500090804              errMessage  = *on;
262600090804              errGenerico = *on;
262700090804              PosCurOfi02 = *on;
262800090804              V1Dmsg = $Msg(15);
262900090804              leavesr;
263000090804           endif ;
263100091216
263200091216         //?Verifico se il commerciale è libero
263300100322         // gli passo anche l'attività del momento altrimenti non la esclude
263400091216           clear trmk84ds;
263500091216           IMK84cmm = %dec(v02cmi:7:0);
263600091216           IMK84dad = w02scd;
263700091216           IMK84oii = v02oii;
263800091216           IMK84ofi = v02ofi;
263900100322           IMK84tat = imk19tat ;
264000100322           IMK84atn = imk19atn ;
264100100322           IMK84atnp= imk19atnp;
264200091216           trmk84r (trmk84ds);
264300091216         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
264400091216           IF  OMK84err <> *blanks and OMK84err <> '9' and
264500091216               Not $ForzaImpegno;
264600091216             ErrGenerico = *on;
264700091216             ErrMessage  = *on;
264800091216             PosCurScd02 = *on;
264900091216             V1Dmsg = OMK84msg;
265000091216             $ForzaImpegno = *on;
265100091216             leavesr;
265200091216           ENDIF;
265300090611
265400090804       endif;
265500090804
265600090611       ENDSR ;
265700090611
265800090611       //--------------------------------------------------------------
265900090611       //?Gestione videata W03
266000090611       //--------------------------------------------------------------
266100090611       BEGSR GesW03;
266200090611
266300090611         // Inizializzazione videata
266400090611         if $InzW03 = *on;
266500090611           exsr InzW03;
266600090611           $InzW03 = *off;
266700090611         endif;
266800090611
266900090611
267000090611         // Emissione videata
267100090611         exfmt MK19W03;
267200090611         errMessage   = *off;
267300090611         errGenerico  = *off;
267400090611         clear W03msg;
267500090611
267600090611         select;
267700090611         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
267800090611           when  $ErrGrave = *on;
267900090611             $Fine  = *on;
268000100927
268100100927       //?F02 = Rubrica
268200100927           WHEN dsp_aid = c_F02;
268300100927             exsr F02D02;
268400100927             leavesr;
268500100927
268600090611         // F12 = Ritorno
268700090611           when dsp_aid = c_F12;
268800090611             exsr F12W03;
268900090611
269000091026         // F11 = Agenda
269100091026           when dsp_aid = c_F11;
269200091027             exsr F11w03;
269300090611
269400090611         // Enter
269500090611           other;
269600090611           exsr CtrW03;
269700090611           if errGenerico = *on ;
269800090611               leavesr;
269900090611           endif;
270000090611         endsl;
270100100329
270200100329         // se dato f8 conferma (solo in caso di appuntamento )
270300100329           If   dsp_aid = c_F08 ;
270400100329                If V02rtr = *blanks or v02mrt = *blanks ;
270500100329                   errMessage  = *on;
270600100329                   errGenerico = *on;
270700100329                   PosCurCom03 = *on;
270800100329                   W03msg = $Msg(32);
270900100329                   leavesr;
271000100329                 endif;
271100100329           endif ;
271200100329
271300100329
271400090611
271500100329         // se dato f6 conferma  / f8 conferma + mail appuntamento
271600100309
271700100309         // se dato f6 sulla videata di creazione nuova attività e non solo invio  lettera
271800100920           If    dsp_aid = c_F06  or
271900100920                 dsp_aid = c_F08  ;
272000090611                   exsr f06w03d02;
272100091210                   IF  OMK22f12 = 'S';
272200091210                     leavesr;
272300091210                   ENDIF;
272400100309
272500100309         // comitto se richiesto e se non ci sono errori
272600100309                 If   imk19cmt = 'S' and omk19err = ' ';
272700100309                      commit ;
272800100309                 endif ;
272900100115
273000100921         // se F6
273100100115           // devo aggiornare il commerciale sul potenziale nel caso non sia già
273200100115           // impostato
273300100517                chain(n)  atccpo  TNCPO01L;
273400100610                IF  not %found(TNCPO01L) or
273500100610                    (%found(tncpo01l) and cpocmm > 0) ;
273600100115                  leavesr;
273700100115                ENDIF;
273800100517           // se devo aggiornare il commerciale aggancio in aggiornamento
273900100517           // il file tncpo e ne verifico l'allocazione
274000100517                chain(e) atccpo TNCPO01l ;
274100100517             If not %error;
274200100920                Cpocmm = %dec(v02com:7:0);
274300100115                Cpoflt = %dec(%subst(%editc(cpocmm:'X'):1:3):3:0);
274400100115                update TNCPO000;
274500100517             ENDIF;
274600100329
274700100329           endif;
274800100329
274900090611       ENDSR;
275000090611
275100090611
275200090611       //--------------------------------------------------------------
275300090611       //?Caricamento videata conferma W03
275400090611       //--------------------------------------------------------------
275500090611       BEGSR InzW03;
275600090611
275700090611          clear mk19w03 ;
275800090611
275900090611             W03cae = v02cae ;
276000090611             W03caed= v02caed;
276100091113             If newnrv > 0 ;
276200091113                w03nrv = newnrv ;
276300091113             else ;
276400091113                W03nrv = v02vis ;
276500091113             endif ;
276600100311         // se la causale chiude evntualmente la  trattativa non lego il numero trattativa
276700100311         // all'attività futura
276800100311             If §ccocht = 'S' and w03nrv > 0 ;
276900100311                clear w03nrv ;
277000100311             endif ;
277100091113
277200091113             trattativa = (w03nrv > 0);
277300091118        // Se trattativa nuova evidenzio
277400091118             Nuova = ($new_trattati = *on and trattativa = *on) ;
277500090611
277600091021       // in caso di futuro contatto appuntamento non propongo nulla
277700091021        If §ccotat <> 'A' ;
277800091021
277900100924       // calcolo data scadenza se segno causale <> da '='
278000100924            If §ccoupm = '+' or §ccoupm = '-' ;
278100100924               clear XGIOLAVDS;
278200100924               IXGLdata = %dec(%date());
278300100924               If §ccoupm = '+' ;
278400100924                  IXGLadd  = 'S';
278500100924               Else ;
278600100924                  IXGLsub  = 'S';
278700100924               Endif ;
278800100924               IXGLgg   = §CCOgio;
278900100924               IXGLpa   = 'P';
279000100924               If  V02com > *zeros;
279100100924                  IXGLfil = %dec(%subst(V02com:1:3):3:0);
279200100924               else ;
279300100924                  IXGLfil = DUTpou;
279400100924               Endif;
279500100924               XGIOLAV (xgiolavds);
279600100924               If  OXGLerr = *blanks;
279700100924                   data_eur = %date(OXGLdata:*iso);
279800100924                   W03dsc   = %dec(data_eur);
279900100924               Endif;
280000100924            Endif ;
280100100924       // calcolo data scadenza se segno causale = ad '='
280200100924            If §ccoupm = '=' ;
280300101111                   W03dsc   = *date;
280400100924            Endif ;
280500090611
280600090611       // propongo l'ora del momento
280700090611             w03osc = %dec(%subst(%EDITC(%dec(%time()):'X'):1:4):4:0);
280800090611
280900090611       // propongo lo stesso commerciale che ha eseguito il contatto
281000090611             w03com  = v02com  ;
281100090611             w03comd = v02comd ;
281200091021
281300091021        endif ;
281400091021
281500100326       // in caso di futuro contatto appuntamento propongo commerciale trattativa
281600100326        If §ccotat = 'A' ;
281700100326       // propongo lo stesso commerciale della trattativa
281800100329       // richiamo il tnta45 con com mercilae 9999999 così mi torna il commerciale trattativa
281900100326           clear tnta45ds ;
282000100326           ITA45tip = 'C';
282100100326           ITA45nrv = v02vis;
282200100329           ITA45CMM = 9999999 ;
282300100326           tnta45r (tnta45ds) ;
282400100326           IF  OTA45err = 'C';
282500100326             w03com  = %editc(ota45cmm:'X')  ;
282600130809             clear  w03comD;
282700130809             chain (oTA45cmm) AZCMM000;
282800130809             if  %found(AZCMM01L);
282900130809               w03comd = CMMdes;
283000130809             endif;
283100100326           Endif ;
283200100326
283300100326        Endif ;
283400100326
283500090630       // verifico se il prossimo contatto ha una tipologia telefonata o appuntamento
283600100210             prox_telefon= (§ccotat = 'T' or §ccotat = 'O');
283700091015             prox_appunta= (§ccotat = 'A');
283800101221
283900101221       //?Se impostata la data caldo senza offerta la devo proporre
284000101221           IF  sav_DataHot <> 0;
284100101221             data_eur = %date(sav_DataHot:*iso);
284200101221             W03dsc   = %dec(data_eur);
284300101221           ENDIF;
284400091210
284500091210         //?imposto fisso 'N' per affiancamento
284600091210         W03aff = 'N';
284700100504
284800100504       //?Se appuntamento e commerciale già presente posiziono il cursore sulla data
284900100504         IF  prox_appunta and w03com > *zeros;
285000100504           PosCurDsc03 = *on;
285100100504         ENDIF;
285200091216
285300091216         $ForzaImpegno = *off;
285400090630
285500090611       ENDSR;
285600090611
285700090611       //--------------------------------------------------------------
285800090611       //?Controllo videata W03
285900090611       //--------------------------------------------------------------
286000090611       BEGSR CtrW03;
286100090611
286200090611            WindDspF = IndDspF;
286300090611            reset WindDspFb;
286400090611            IndDspF  = WindDspF;
286500090611
286600090611         // Controllo codice commerciale (? Ricerca)
286700090611           If    %scan('?' : W03com) > 0  ;
286800090611             errGenerico = *on;
286900090611             PosCurCom03 = *on;
287000130809             clear  TRMK43ds;
287100130809             iMK43fil  = DUTpou;
287200130809             TRMK43R (kpjba : TRMK43ds);
287300130809             if  oMK43err = *off  and  oMK43fxx = *blank;
287400130809               W03com  = %editc( oMK43cmm : 'X' );
287500130809               W03comd = oMK43des;
287600130809             endif;
287700090611             leavesr;
287800090611           endif;
287900090611
288000100210         // Commerciale che esegue il contatto obbligatorio
288100100210           If  W03com = *blanks or w03com = *zeros;
288200100210             errMessage  = *on;
288300100210             errGenerico = *on;
288400100210             PosCurCom03 = *on;
288500100210             W03msg = $Msg(02);
288600100210             leavesr;
288700100210           endif;
288800100210
288900090611         // Controllo codice commerciale che esegue il contatto
289000100210             IF  %check(digits:w03com) > 0;
289100100210               ErrMessage  = *on;
289200100210               ErrGenerico = *on;
289300100210               PosCurCom03 = *on;
289400100210               W03msg = $Msg(02);
289500100210               leavesr;
289600100210             ENDIF;
289700100210
289800100210           //?utente abilitato al commerciale
289900100210             clear TNTAA1DS;
290000120810             ITAA1tipo = 'M';
290100120810             ITAA1caut = '§utegtc';
290200100210             ITAA1cmm  = %dec(w03com:7:0);
290300100519             ITAA1abi = 'RA';
290400100210             kpjbu     = tntaa1ds;
290500100210             tntaa1c (kpjba);
290600100210             TNTAA1DS = kpjbu;
290700100210             IF  OTAA1fabi = 'N';
290800100210               ErrMessage  = *on;
290900100210               ErrGenerico = *on;
291000100210               PosCurCom03 = *on;
291100100210               w03msg      = $Msg(02);
291200100210               leavesr;
291300100210             ENDIF;
291400100210
291500130809             CMmcod = %int(W03com);
291600130809             chain (CMMcod) AZCMM000;
291700130809             if  NOT  %found(AZCMM01L);
291800090611               errMessage  = *on;
291900090611               errGenerico = *on;
292000090611               PosCurCom03 = *on;
292100090611               W03msg = $Msg(02);
292200090611               leavesr;
292300100210             endif;
292400130809             W03comd = CMMdes;
292500100311
292600130809             If  CMMpar <> ' ' ;
292700110525               ErrMessage  = *on;
292800110525               ErrGenerico = *on;
292900110525               PosCurCom03 = *on;
293000110525               W03msg = $Msg(02);
293100110525              leavesr;
293200110525             ENDIF;
293300111223
293400111223       //?Commerciale "valido" data inizio e fine attività
293500130809         IF  CMMdtIni > wOggi  or
293600130809             CMMdtFin < wOggi;
293700111223           ErrMessage = *on;
293800111223           ErrGenerico = *on;
293900111223           PosCurCom03 = *on;
294000111223           W03msg = $Msg(02);
294100111223           leavesr;
294200111223         ENDIF;
294300100310
294400100318            If sav_w03com <> w03com ;
294500100318               $forzacomin = *off ;
294600100318            endif ;
294700100310         // Controllo se causale vuole controllo su commerciale interno
294800130809            If §ccocoi <> ' ' and (CMMfun =  'COMIN' or CMMfun = 'ASC'
294900130809                                                     or CMMfun = 'SA');
295000100310         // verifico se msg forzabile
295100100310               If §ccocoi = 'F' and $forzacomin = *off ;
295200100310                  errMessage  = *on;
295300100310                  errGenerico = *on;
295400100310                  PosCurCom03 = *on;
295500100310                  W03msg = %trimr($Msg(06)) + ' Enter per forzare' ;
295600100310                  $forzacomin = *on ;
295700100311                  sav_w03com = w03com ;
295800100310                  leavesr;
295900100318               Endif ;
296000100318               If §ccocoi <>'F' ;
296100100310                  errMessage  = *on;
296200100310                  errGenerico = *on;
296300100310                  PosCurCom03 = *on;
296400100310                  W03msg = $Msg(06) ;
296500100310                  leavesr;
296600100310               Endif ;
296700100310             endif;
296800100202
296900100202         //?Se sto pianificando una nuova attività che apre trattativa
297000100202         //?devo controllare che non ci sia già una trattativa aperte
297100100202         //?per lo stesso potenziale e commerciale
297200100202           IF  §CCOapt = 'S' and $New_Trattati;
297300100202             clear tnta43ds ;
297400100202             ITA43cpo = cpocpo;
297500100202             ITA43ksc = v02cli;
297600100202             ITA43nrv = w03nrv;
297700100202             ITA43cmm = %dec(w03com:7:0);
297800100202             tnta43r (TNTA43DS) ;
297900100202             IF  OTA43err = '+';
298000100202               ErrMessage  = *on;
298100100202               ErrGenerico = *on;
298200100202               PosCurCom03 = *on;
298300100202               W03msg = OTA43msg;
298400100202               leavesr;
298500100202             ENDIF;
298600100202           ENDIF;
298700100326       If Prox_appunta = *on ;
298800100326
298900100326         //?Verifico se il commerciale dell'appuntamento è diverso dal
299000100326         //?commerciale della trattativa
299100100422
299200100422            If sav_w03comt <> w03com ;
299300100422               $forzacomm3 = *off ;
299400100422            endif ;
299500100326         clear tnta45ds ;
299600100326         ITA45tip = 'C';
299700100326         ITA45nrv = w03nrv;
299800100326         ITA45cmm = %dec(w03com:7:0);
299900100326         tnta45r (tnta45ds) ;
300000100326         IF  OTA45err = 'C';
300100100422       // se vengo da trattativa il msg è forzabile
300200100422             If $forzacomm3= *off ;
300300100422                ErrMessage  = *on;
300400100422                ErrGenerico = *on;
300500100422                PosCurCom03 = *on;
300600100422                If imk19fpr = 'T' ;
300700100422                   W03msg = %trimr(OTA45msg) + ' Enter per forzare';
300800100422                   $forzacomm3= *on ;
300900100422                   sav_w03comt= w03com ;
301000100422                else ;
301100100422                   w03msg = OTA45msg;
301200100422                leavesr;
301300100422                Endif ;
301400100422             endif ;
301500100326         ENDIF;
301600100326         ENDIF;
301700100326
301800100504         //?Verifico se LA FILIALE DI APPARTENENZA DEL COMMERCIALE E' UGUALE
301900100504         //?ALLA FILIALE DI APPARTENENZA DEL COMMERCIALE CLIENTE SE ESISTE
302000100504         //?E SE E' PRIMO APPUNTAMENTO
302100100504
302200100504            If %SUBST(w03com: 1: 3) <> %subst(%editc(v02cmp:'X'): 1:3) and
302300100504               $new_trattati = *on and v02cli > 0 ;
302400100504                ErrMessage  = *on;
302500100504                ErrGenerico = *on;
302600100504                PosCurCom03 = *on;
302700100504                W03msg = $Msg(34);
302800100504                leavesr;
302900100504             endif;
303000090702
303100100326         // controllo data scadenza contatto
303200100326           If    w03dsc <> 0;
303300100326             clear wlbdat;
303400100326             g02dat = w03dsc;
303500100326             xsrda8(wlbdat);
303600100326             if g02err = '1';
303700100326               errMessage  = *on;
303800100326               errGenerico = *on;
303900100326               PosCurDsc03 = *on;
304000100326               W03msg = $Msg(03);
304100100326               leavesr;
304200100326             endif;
304300100326
304400100326             w03dsc = g02dat;
304500100326             WW03dsc = g02inv;
304600100326           else ;
304700100326             errMessage  = *on;
304800100326             errGenerico = *on;
304900100326             PosCurDsc03 = *on;
305000100326             W03msg = $Msg(03);
305100100326             leavesr;
305200100326           endif;
305300100326
305400100326         // controllo che la data non sia inferiore al 2000 o superiore al 2039
305500100326         IF  ww03dsc < 20000101 or ww03dsc > 20391231;
305600100326           errMessage  = *on;
305700100326           errGenerico = *on;
305800100326           PosCurDsc03 = *on;
305900100326           W03msg = $Msg(03);
306000100326           leavesr;
306100100326         ENDIF;
306200100330
306300100330       // controllo che la data non sia inferiore a 3 gg lavorativi da oggi
306400100330         clear XGIOLAVDS;
306500100330         IXGLdata = %dec(%date());
306600100330         IXGLsub  = 'S';
306700100330         IXGLgg   = 3;
306800100330         IXGLpa   = 'P';
306900100330         IXGLlav  = 'S';
307000100330         IXGLfil = %dec(%subst(W03com:1:3):3:0);
307100100330         XGIOLAV (xgiolavds);
307200100330         IF  ww03dsc < OXGLdata;
307300100330             errMessage  = *on;
307400100330             errGenerico = *on;
307500100330             PosCurDsc03 = *on;
307600100330             W03msg = $Msg(03);
307700100330             W03msg = %trim(W03msg) + ' ' +
307800100330                      'Non può essere inferiore a 3 gg.lavorativi';
307900100330             leavesr;
308000100330         ENDIF;
308100100330
308200100330       // controllo che la data non sia maggiore  a 14 mesi da oggi
308300100330         clear XGIOLAVDS;
308400100330         IXGLdata = %dec(%date());
308500100330         IXGLadd  = 'S';
308600100330         IXGLmm   = 14;
308700100330         IXGLpa   = 'P';
308800100330         IXGLfil = %dec(%subst(W03com:1:3):3:0);
308900100330         XGIOLAV (xgiolavds);
309000100330         IF  ww03dsc > OXGLdata;
309100100330             errMessage  = *on;
309200100330             errGenerico = *on;
309300100330             PosCurDsc03 = *on;
309400100330             W03msg = $Msg(03);
309500100330             W03msg = %trim(W03msg) + ' ' +
309600100330                    'Non può essere maggiore di 14 mesi';
309700100330             leavesr;
309800100330         ENDIF;
309900100330
310000100326
310100100326         // controllo ora  esecuzione contatto
310200100326           If W03osc <> 0 ;
310300100326              sav02ora = %editc(W03osc:'X');
310400120105              If  ora > 23 or minuti > 59;
310500100326                   errMessage  = *on;
310600100326                   errGenerico = *on;
310700100326                   PosCurOsc03 = *on;
310800100326                   W03msg = $Msg(05);
310900100326                   leavesr;
311000100326              endif;
311100100326           else ;
311200100326             errMessage  = *on;
311300100326             errGenerico = *on;
311400100326             PosCurOsc03 = *on;
311500100326             W03msg = $Msg(05);
311600100326             leavesr;
311700100326           endif;
311800100326
311900090702         // In caso di appuntamento controllo anche i campi seguenti
312000090702
312100090702       If Prox_appunta = *on ;
312200091216
312300090702        // verifico l'inserimento delle ore di impegno
312400090702
312500090702         // controllo ora  inizio impegno
312600090702           If W03oii <> 0 ;
312700090702              sav02ora = %editc(W03oii:'X');
312800120105              If  ora > 23 or minuti > 59;
312900090702                   errMessage  = *on;
313000090702                   errGenerico = *on;
313100090702                   PosCurOii03 = *on;
313200090702                   w03msg = $Msg(05);
313300090702                   leavesr;
313400090702              endif;
313500091216           else ;
313600091216             errMessage  = *on;
313700091216             errGenerico = *on;
313800091216             PosCurOii03 = *on;
313900091216             w03msg = $Msg(05);
314000091216             leavesr;
314100090702           endif;
314200090702
314300091216         // controllo ora  fine   impegno
314400090702           If W03ofi <> 0 ;
314500090702              sav02ora = %editc(W03ofi:'X');
314600120105              If  ora > 23 or minuti > 59;
314700090702                   errMessage  = *on;
314800090702                   errGenerico = *on;
314900090702                   PosCurOfi03 = *on;
315000090702                   w03msg = $Msg(05);
315100090702                   leavesr;
315200090702              endif;
315300091216           else ;
315400091216             errMessage  = *on;
315500091216             errGenerico = *on;
315600091216             PosCurOfi03 = *on;
315700091216             w03msg = $Msg(05);
315800091216             leavesr;
315900090702           endif;
316000090702
316100090702         // controllo ora  inizio se congruente con fine impegno
316200090702           If W03oii > 0 and W03ofi = 0;
316300090702              errMessage  = *on;
316400090702              errGenerico = *on;
316500090702              PosCurOfi03 = *on;
316600090702              w03msg = $Msg(13);
316700090702              leavesr;
316800090702           endif;
316900090702
317000090702         // controllo ora  inizio se congruente con fine impegno
317100090702           If W03oii > 0 and W03ofi > 0 and W03oii > w03ofi ;
317200090702              errMessage  = *on;
317300090702              errGenerico = *on;
317400090702              PosCurOfi03 = *on;
317500090702              w03msg = $Msg(13);
317600090702              leavesr;
317700090702           endif;
317800090804
317900090804         // verifico se inizioe fine impegno è congruente con ora appuntamento
318000090804           If W03oii > 0 and W03ofi > 0 and (W03oii > W03osc or
318100090804              W03ofi < W03osc) ;
318200090804              errMessage  = *on;
318300090804              errGenerico = *on;
318400090804              PosCurOfi03 = *on;
318500090804              w03msg = $Msg(15);
318600090804              leavesr;
318700090804           endif ;
318800091216
318900091216         //?Verifico se il commerciale è libero
319000091216           clear trmk84ds;
319100091216           IMK84cmm = %dec(w03com:7:0);
319200091216           IMK84dad = ww03dsc;
319300091216           IMK84oii = w03oii;
319400091216           IMK84ofi = w03ofi;
319500091216           trmk84r (trmk84ds);
319600091216         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
319700091216           IF  OMK84err <> *blanks and OMK84err <> '9' and
319800091216               Not $ForzaImpegno;
319900091216             ErrGenerico = *on;
320000091216             ErrMessage  = *on;
320100091216             PosCurDsc03 = *on;
320200091216             w03msg = OMK84msg;
320300091216             $ForzaImpegno = *on;
320400091216             leavesr;
320500091216           ENDIF;
320600090702
320700090702       endif;
320800090702
320900090702       ENDSR;
321000100108
321100100108       //--------------------------------------------------------------
321200100108       //?Gestione videata D04.
321300100108       //--------------------------------------------------------------
321400100108       BEGSR GesD04;
321500100108
321600100108         //?Inizializzazione videata
321700100108         IF  $InzD04;
321800100108           exsr InzD04;
321900100108           $InzD04 = *off;
322000100108         ENDIF;
322100100108
322200100108         //?Emissione testata
322300100108         write MK19T01;
322400100108
322500100108         //?Emissione videata
322600100108         exfmt MK19D04;
322700100108         ErrMessage  = *off;
322800100108         ErrGenerico = *off;
322900100108         clear V1Dmsg;
323000100108
323100100108         SELECT;
323200100108
323300100108         //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
323400100108           WHEN  $ErrGrave = *on;
323500100108             $Fine = *on;
323600100108             OMK19err = '1';
323700100108             OMK19msg = V1Dmsg;
323800100108
323900100108         //?F06 = Conferma
324000100108           WHEN  dsp_aid = c_F06;
324100100108             exsr CtrD04;
324200100108             IF  ErrGenerico;
324300100108               leavesr;
324400100108             ENDIF;
324500100108             exsr F06D04;
324600100108
324700100108         //?F12 = Ritorno
324800100108           WHEN  dsp_aid = c_F12;
324900100108             exsr F12D02;
325000100108             leavesr ;
325100100112
325200100112         //?F16 = Annulla
325300100112           WHEN  dsp_aid = c_F16;
325400100112             exsr F16D04;
325500100112             leavesr ;
325600100108
325700100108         //?F18 = Note
325800100108           WHEN dsp_aid = c_F18;
325900100121             exsr F18D04;
326000100108
326100100108         //?Enter
326200100108           OTHER;
326300100108         //?Se manutenzione controllo i dati
326400100108             IF  Not Visualizza;
326500100108               exsr CtrD04;
326600100108               IF  ErrGenerico;
326700100108                 leavesr;
326800100108               ENDIF;
326900100108             ENDIF;
327000100108
327100100108         ENDSL;
327200100108
327300100108       ENDSR;
327400100108
327500100108       //--------------------------------------------------------------
327600100108       //?Caricamento videata telefonata D04.
327700100108       //--------------------------------------------------------------
327800100108       BEGSR InzD04;
327900100108
328000100108         clear MK19D04;
328100100108
328200100108       //?Indicatori di comodo
328300100108         altrenote    = *off  ;
328400100108         piunote      = *off  ;
328500100108         GiornoTotale = *off;
328600100108
328700100108       //?Imposto se visualizzazione
328800100108         IF  IMK19fle = '5';
328900100108           V01tit     = tit_visua;
329000100108           Visualizza = *on;
329100100108         ENDIF;
329200100108
329300100108       //?Imposto se manutenzione
329400100108         IF  IMK19fle = '2';
329500100108           V01tit       = tit_manut;
329600100108           Manutenzione = *on;
329700100108         ENDIF;
329800100519
329900100519         //?Abilito F16=Annullamento solo in manutenzione
330000100519         F16Attivo = Manutenzione;
330100100108
330200100108       //?Aggancio dati attività
330300100108           chain(n) (imk19tat:imk19atn:imk19atnp) tiatc01l;
330400100108
330500100108       //?Non trovo vado a fine
330600100108         IF  not %Found(tiatc01l);
330700100108           $ErrGrave   = *on;
330800100108           ErrMessage  = *on;
330900100108           ErrGenerico = *on;
331000100108           V1Dmsg = 'Dati attività non trovati';
331100100108           leavesr;
331200100108         ENDIF;
331300100108
331400100108       //?Ricerco descrizione causale
331500100108         clear dCCO;
331600100108         clear TIBS02ds;
331700100108         T02mod = 'C';
331800100108         T02cod = 'CCO';
331900100108         T02ke1 = atccad;
332000100108         T02sif = KNSIF;
332100100108         TNTBE_RicercaControllo  (kpjba : tibs02ds);
332200100108         IF  T02err = *blanks;
332300100108           dCCO = t02uni;
332400100108         ENDIF;
332500100108
332600100108       //?Imposto titolo alto
332700100108         clear wtitolo;
332800100108         wtitolo = §TTAtit;
332900100113         wtitolo = %trim(wtitolo) + '   ' + atccad + ' - ' + §CCOdes;
333000100108         clear wpos;
333100100108         wpos = (%len(v04tit) - %len(wtitolo)) / 2;
333200100108         %subst(v04tit:wpos:%len(wtitolo)) = wtitolo;
333300100108
333400100108       //?Verifico se parziale o tutta la giornata
333500100108         IF  atccad = 'FT';
333600100108           GiornoTotale = *on;
333700100108         ENDIF;
333800100108
333900100108       //?Data attività
334000100108         IF  atcdad > 0;
334100100108           data_eur = %date(atcdad:*iso);
334200100108           V04dad   = %dec(data_eur);
334300100108           w04dad   = atcdad;
334400100108         ENDIF;
334500100108
334600100108       //?Commerciale
334700100108         V04cmm = %editc(atccmm : 'X');
334800130809         clear  V04cmmD;
334900130809         chain (ATCcmm) AZCMM000;
335000130809         IF   %found(AZCMM01L);
335100130809           V04cmmd = CMMdes;
335200100108         ENDIF;
335300100108
335400100108       //?Impegno
335500100108         V04oii = atcoii;
335600100108         V04ofi = atcofi;
335700100108
335800100108       //?Note
335900100108         clear trmk20ds ;
336000100108         IMK20tla = 'L';
336100100108         IMK20flm = 'R';
336200100108         IMK20tat = IMK19tat ;
336300100108         IMK20atn = IMK19atn ;
336400100108         IMK20atnp= IMK19atnp -1;
336500100108         trmk20r ( kpjba : trmk20ds );
336600100108         V04no1o = emk20no1;
336700100108         V04no2o = emk20no2;
336800100108         piunote = (omk20piu = '+');
336900100108
337000100108       ENDSR;
337100100108
337200100108       //--------------------------------------------------------------
337300100108       //?Controllo videata D04.
337400100108       //--------------------------------------------------------------
337500100108       BEGSR CtrD04;
337600100108
337700100108         WindDspF = IndDspF;
337800100108         reset WindDspFb;
337900100108         IndDspF  = WindDspF;
338000100108
338100100108       //?Data attività
338200100108         IF  V04dad = 0;
338300100108           ErrMessage  = *on;
338400100108           ErrGenerico = *on;
338500100108           PosCurScd02 = *on;
338600100108           V1Dmsg      = $Msg(03);
338700100108           leavesr;
338800100108         ENDIF;
338900100108
339000100108         clear wlbdat;
339100100108         g02dat = V04dad;
339200100108         xsrda8(wlbdat);
339300100108         IF  g02err = '1';
339400100108           ErrMessage  = *on;
339500100108           ErrGenerico = *on;
339600100108           PosCurScd02 = *on;
339700100108           V1Dmsg      = $Msg(03);
339800100108           leavesr;
339900100108         ENDIF;
340000100108
340100100108         V04dad = g02dat;
340200100108         W04dad = g02inv;
340300100118
340400100118       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
340500100118         IF  W04dad < 20000101 or W04dad > 20391231;
340600100118           ErrMessage  = *on;
340700100118           ErrGenerico = *on;
340800100118           PosCurScd02 = *on;
340900100118           V1Dmsg      = $Msg(03);
341000100118           leavesr;
341100100118         ENDIF;
341200100108
341300100108       //?Impegno solo se non è intera giornata
341400100108         IF  not GiornoTotale;
341500100108           IF  V04oii = 0;
341600100108             ErrMessage  = *on;
341700100108             ErrGenerico = *on;
341800100108             PosCurOii02 = *on;
341900100108             V1Dmsg      = $Msg(17);
342000100108             leavesr;
342100100108           ENDIF;
342200100108           sav02ora = %editc(V04oii:'X');
342300120105           IF  ora > 23 or minuti > 59;
342400100108             ErrMessage  = *on;
342500100108             ErrGenerico = *on;
342600100108             PosCurOii02 = *on;
342700100108             V1Dmsg      = $Msg(05);
342800100108             leavesr;
342900100108           ENDIF;
343000100108
343100100108           IF  V04ofi = 0 ;
343200100108             ErrMessage  = *on;
343300100108             ErrGenerico = *on;
343400100108             PosCurOfi02 = *on;
343500100108             V1Dmsg      = $Msg(17);
343600100108             leavesr;
343700100108           ENDIF;
343800100108           sav02ora = %editc(V04ofi:'X');
343900120105           IF  ora > 23 or minuti > 59;
344000100108             ErrMessage  = *on;
344100100108             ErrGenerico = *on;
344200100108             PosCurOfi02 = *on;
344300100108             V1Dmsg      = $Msg(05);
344400100108             leavesr;
344500100108           ENDIF;
344600100108
344700100108         //?controllo ora  inizio se congruente con fine impegno
344800100108           IF  V04oii > 0 and V04ofi = 0;
344900100108             ErrMessage  = *on;
345000100108             ErrGenerico = *on;
345100100108             PosCurOfi02 = *on;
345200100108             V1Dmsg      = $Msg(13);
345300100108             leavesr;
345400100108           ENDIF;
345500100108
345600100108         //?controllo ora  inizio se congruente con fine impegno
345700100108           IF  V04oii > 0 and V04ofi > 0 and V04oii > V04ofi ;
345800100108             ErrMessage  = *on;
345900100108             ErrGenerico = *on;
346000100108             PosCurOfi02 = *on;
346100100108             V1Dmsg      = $Msg(13);
346200100108             leavesr;
346300100108           ENDIF;
346400100108         ENDIF;
346500100108
346600100108       //?Codice commerciale
346700100108         IF  V04cmm = *blanks or V04cmm = *zeros;
346800100108           ErrMessage  = *on;
346900100108           ErrGenerico = *on;
347000100108           PosCurCmi02 = *on;
347100100108           V1Dmsg      = $Msg(02);
347200100108           leavesr;
347300100108         ENDIF;
347400100108
347500100108         IF  %scan('?' : V04cmm) > 0  ;
347600100108           ErrGenerico = *on;
347700100108           PosCurCmi02 = *on;
347800130809           clear  TRMK43ds;
347900130809           iMK43solU = 'S';
348000130809           iMK43fil  = DUTpou;
348100130809           TRMK43R (kpjba : TRMK43ds);
348200130809           if  oMK43err = *off  and  oMK43fxx = *blank;
348300130809             V04cmm  = %editc( oMK43cmm : 'X' );
348400130809             V04cmmd = oMK43des;
348500130809           endif;
348600100108           leavesr;
348700100108         ENDIF;
348800100108
348900100108         IF  %check(digits:V04cmm) > 0;
349000100108           ErrMessage  = *on;
349100100108           ErrGenerico = *on;
349200100108           PosCurCmi02 = *on;
349300100108           V1Dmsg      = $Msg(02);
349400100108           leavesr;
349500100108         ENDIF;
349600100210
349700100210         //?utente abilitato al commerciale
349800100210         clear TNTAA1DS;
349900120810         ITAA1tipo = 'M';
350000120810         ITAA1caut = '§utegtc';
350100100210         ITAA1cmm  = %dec(V04cmm:7:0);
350200100519         ITAA1abi = 'RA';
350300100210         kpjbu     = tntaa1ds;
350400100210         tntaa1c (kpjba);
350500100210         TNTAA1DS = kpjbu;
350600100210         IF  OTAA1fabi = 'N';
350700100210           ErrMessage  = *on;
350800100210           ErrGenerico = *on;
350900100210           PosCurCmi02 = *on;
351000100210           V1Dmsg      = $Msg(02);
351100100210           leavesr;
351200100210         ENDIF;
351300100108
351400130809         CMMcod = %int(V04cmm);
351500130809         chain (CMMcod) AZCMM000;
351600130809         IF  not %found(AZCMM01L);
351700100108           ErrMessage  = *on;
351800100108           ErrGenerico = *on;
351900100108           PosCurCmi02 = *on;
352000100108           V1Dmsg      = $Msg(02);
352100100108           leavesr;
352200100108         ENDIF;
352300100504
352400130809         V04cmmd = CMMdes;
352500100210
352600100210       //?Commerciale  -  solo unificante
352700130809         IF  %dec(V04cmm:7:0) <> CMMuni;
352800100210           ErrMessage  = *on;
352900100210           ErrGenerico = *on;
353000100210           PosCurCmi02 = *on;
353100100210           V1Dmsg      = 'Commerciale errato. ' +
353200100210                         'Immettere solo commerciali UNIFICANTI!';
353300100210           leavesr;
353400100210         ENDIF;
353500100108
353600100108       ENDSR;
353700100108
353800100108       //--------------------------------------------------------------
353900100108       //?Gestione tasto funzionale F06 da videata D04.
354000100108       //?F06=Conferma
354100100108       //--------------------------------------------------------------
354200100108       BEGSR F06D04;
354300100108
354400100108       //?Registro le varizioni
354500100108         chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
354600100108         IF  %Found(tiatc01l);
354700100108           atcdad = w04dad ;
354800100108           IF  GiornoTotale;
354900100108             atchda = 090000;
355000100108           ELSE;
355100100108             atchda = V04oii * 100;
355200100108           ENDIF;
355300100108           atccmm = %dec(V04cmm:7:0);
355400100108           atcoii = V04oii;
355500100108           atcofi = V04ofi;
355600100108           update tiatc000;
355700100108         ENDIF;
355800100108
355900100108       //?Registro le note
356000100108         clear trmk20ds;
356100100108         IMK20tla = 'L';
356200100108         IMK20flm = 'C';
356300100215         IMK20cmt = '1';
356400100108         IMK20tat = atctat;
356500100108         IMK20atn = atcatn;
356600100108         IMK20atnp= atcatnp - 1;
356700100108         trmk20r ( kpjba : trmk20ds );
356800100108
356900100108       //?Comitto se richiesto e se non ci sono errori
357000100108         IF  IMK19cmt = 'S' and OMK19err = *blanks;
357100100108           commit;
357200100108         ENDIF;
357300100108
357400100108       //?Ritorno al programma chiamante
357500100108         $fine = *on;
357600100108
357700100108       ENDSR;
357800100112
357900100112       //--------------------------------------------------------------
358000100112       //?Gestione tasto funzionale F16 da videata D04.
358100100112       //?F16=Annulla
358200100112       //--------------------------------------------------------------
358300100112       BEGSR F16D04;
358400100112
358500121029       //?Se ci sono note legate all'attività da pulire le cancello
358600121029         exsr Del_ticpn;
358700100112
358800100112       //?Registro le varizioni
358900100112         chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
359000100112         IF  %Found(tiatc01l);
359100100112           delete tiatc000;
359200100112         ENDIF;
359300100112
359400100112       //?Comitto se richiesto e se non ci sono errori
359500100112         IF  IMK19cmt = 'S' and OMK19err = *blanks;
359600100112           commit;
359700100112         ENDIF;
359800100112
359900100112       //?Ritorno al programma chiamante
360000100112         $fine = *on;
360100100112
360200100112       ENDSR;
360300100121
360400100121       //--------------------------------------------------------------
360500100121       //?Gestione tasto funzionale F18 da videata D04
360600100121       //?F18=Note
360700100121       //--------------------------------------------------------------
360800100121       BEGSR F18D04;
360900100121
361000100121         clear trmk20ds;
361100100121       //?Se interrogazion o attività già chiusa
361200100121       //?richiamo in interrogazione
361300100121         IF  GiaFatto or Visualizza;
361400110510           IMK20tla  = 'L';
361500100121           IMK20flm = 'I';
361600100121         ENDIF;
361700100121         IMK20tat = atctat;
361800100121         IMK20atn = atcatn;
361900100121         Trmk20r ( kpjba : trmk20ds );
362000100929
362100100929       //?Se sono in manutenzione devo recuperare la note come prima
362200100929         IF  not GiaFatto and not Visualizza;
362300100929           IMK20tla  = 'L';
362400100929           IMK20flm  = 'R';
362500100929           IMK20tat  = IMK19tat ;
362600100929           IMK20atn  = IMK19atn ;
362700100929           IMK20atnp = IMK19atnp -1;
362800100929           Trmk20r ( kpjba : trmk20ds );
362900100929           v04no1o   =  emk20no1 ;
363000100929           v04no2o   =  emk20no2 ;
363100100929           piunote   = (omk20piu = '+');
363200100929         ENDIF;
363300100121
363400100121       ENDSR;
363500100224
363600100224       //--------------------------------------------------------------
363700100224       //?Gestione videata W05
363800100224       //--------------------------------------------------------------
363900100224       BEGSR GesW05;
364000100224
364100100224         // Inizializzazione videata
364200100224         clear w05ann ;
364300100224
364400100224         // verifico se c'è affiancamento  richiamo il trmk22 in modalità annullamento
364500100224         If Conaffianca = *on ;
364600100224            clear trmk22ds ;
364700100224            IMK22fcmt = '1';
364800100224            IMK22cmt = 'N';
364900100224            IMK22fmt = '4';
365000100224            IMK22fle = 'D';
365100100224            IMK22cac = v02cae;
365200100224            IMK22cmm  = %dec(v02com:7:0);
365300100224            IMK22dad  = w02scd;
365400100224            IMK22hda  = v02ora * 100;
365500100224            IMK22oii  = v02oii;
365600100224            IMK22ofi  = v02ofi;
365700100224            IMK22tat  = IMK19tat;
365800100224            IMK22atn  = IMK19atn;
365900100224            IMK22atnp = IMK19atnp;
366000100224
366100100224            trmk22r (kpjba : TRMK22ds);
366200100224
366300100224         // se ritorna errore lo visualizzo e faccio rollback
366400100224            IF  OMK22err <> *blanks;
366500100224                ErrMessage  = *on;
366600100224                ErrGenerico = *on;
366700100224                V1dmsg = OMK22msg;
366800100224                IF  IMK19cmt = 'S';
366900100224                    rolbk;
367000100224                ENDIF;
367100100224                leavesr;
367200100224            Endif;
367300100224
367400100224         // se F12 da gestione affiancamento rollback
367500100224            If  IMK19cmt = 'S' and OMK22F12 = 'S';
367600100224                rolbk;
367700100224         // Ritorno alla videata D02
367800100224                $Video = 'D2';
367900100224                leavesr;
368000100224            Endif;
368100100224
368200100224            Conaffianca = *off ;
368300100224         Endif ;
368400100224
368500100517         $fine5 = *off ;
368600100224         // Emissione videata
368700100517         dow $fine5 = *off ;
368800100224         exfmt MK19W05;
368900100224         errMessage   = *off;
369000100224         errGenerico  = *off;
369100100224         clear W05msg;
369200100224
369300100225         // se dato f6 conferma e risposto "SI"
369400100503           If   dsp_aid = c_F06 and w05ann = 'S' ;
369500100519         //?Se posso deletare
369600100519             IF $OkDel;
369700100224                     delete (imk19tat:imk19atn:imk19atnp) tiatc01l ;
369800100224         // se l'attività era un progressivo uno vado a cancellare le note
369900100224         // con progressivo 0
370000100224                     If imk19atnp = 1 ;
370100100224                        clear trmk20ds ;
370200100224                        imk20tla = 'L';
370300100224                        imk20flm = 'D';
370400100224                        imk20cpo = cpocpo ;
370500100224                        imk20rsc = v02cpd ;
370600100224                        imk20tat = imk19tat ;
370700100224                        imk20atn = imk19atn ;
370800100224                        imk20atnp= imk19atnp -1;
370900100224                        Trmk20r ( kpjba : trmk20ds );
371000100224                     endif ;
371100100224         // comitto se richiesto e se non ci sono errori
371200100224                IF  IMK19cmt = 'S';
371300100224                    commit ;
371400110412         // Verifico la categoria del potenziale
371500110412                    clear trmk05ds;
371600110412                    IMK05cpo = CPOcpo;
371700110412                    trmk05r (kpjba : TRMK05DS);
371800110412         //?se categoria variata aggiorno potenziale
371900110412                    IF  OMK05err = *blanks and OMK05cat <> CPOfls;
372000110412                        chain(e) CPOcpo TNCPO01L;
372100110412                        IF  not %error and %found(TNCPO01L);
372200110412                            CPOfls = OMK05cat;
372300110412                            update TNCPO000;
372400110412                        ENDIF;
372500110412                    ENDIF;
372600100224                    $Fine = *on;
372700100517                    leavesr ;
372800100224                ENDIF;
372900101014                $fine5 = *on;
373000101014                $Fine = *on;
373100100519              ELSE;
373200100520         //?CHIODO
373300100520         //?Non posso deletare  quindi chiudo attività con causale 'AA'
373400100520                chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
373500100520                IF  %Found(TIATC01L);
373600100520                  ATCdco = %dec(%date());
373700100520                  ATChco = %dec(%time());
373800100520                  IF  IMK19com > *zeros;
373900100520                    ATCcco = IMK19com;
374000100520                  ELSE;
374100100520                    ATCcco = %dec(V02cmi:7:0);
374200100520                  ENDIF;
374300100520                  ATCpru = knmus;
374400100520                  ATCcac = 'AA';
374500100520                  update TIATC000;
374600100520                ENDIF;
374700100520
374800100520               // se ci sono delle note le registro
374900100520               if v02no1 <> *blanks  or v02no2 <> *blanks ;
375000100520                clear trmk20ds ;
375100100520                imk20tla = 'L';
375200100520                imk20flm = 'C';
375300100520                imk20fno = 'S';
375400100520                imk20cmt = '1';
375500100520                imk20cpo = cpocpo ;
375600100520                If v02cli > 0 ;
375700100520                  imk20ksc = v02cli ;
375800100520                endif ;
375900100520                imk20tat = atctat ;
376000100520                imk20atn = atcatn ;
376100100520                imk20atnp= atcatnp;
376200100520                imk20rsc = v02cpd ;
376300100520                emk20no1 = v02no1 ;
376400100520                emk20no2 = v02no2 ;
376500100520                emk20dim = atcdco ;
376600100520                emk20him = atchco ;
376700100520                Trmk20r ( kpjba : trmk20ds );
376800100520               endif ;
376900100520               IF  IMK19cmt = 'S';
377000100520                   commit ;
377100110412         // Verifico la categoria del potenziale
377200110412                    clear trmk05ds;
377300110412                    IMK05cpo = CPOcpo;
377400110412                    trmk05r (kpjba : TRMK05DS);
377500110412         //?se categoria variata aggiorno potenziale
377600110412                    IF  OMK05err = *blanks and OMK05cat <> CPOfls;
377700110412                        chain(e) CPOcpo TNCPO01L;
377800110412                        IF  not %error and %found(TNCPO01L);
377900110412                            CPOfls = OMK05cat;
378000110412                            update TNCPO000;
378100110412                        ENDIF;
378200110412                    ENDIF;
378300100520                   $Fine = *on;
378400100520                   leavesr ;
378500100520               ENDIF;
378600101014                $fine5 = *on;
378700101014                $Fine = *on;
378800100520
378900100519              ENDIF;
379000100224           endif;
379100100519
379200100519
379300100225         // se dato f6 conferma e risposto "NO"
379400100503           If   dsp_aid = c_F06 and w05ann = 'N' ;
379500100225         // bollback
379600100225                IF  IMK19cmt = 'S';
379700100225                    rolbk ;
379800100225                    $Fine = *on;
379900100517                    leavesr ;
380000100225                ENDIF;
380100101014                $fine5 = *on;
380200101014                $Fine = *on;
380300100225           endif;
380400100519
380500100517         enddo;
380600100224
380700100224       ENDSR;
380800100614
380900100614       //--------------------------------------------------------------
381000100614       //?Gestione videata W08
381100100614       //--------------------------------------------------------------
381200100614       BEGSR GesW08;
381300100614
381400100614         //?Inizializzazione videata
381500100614         w08sino = 'N';
381600100614         $Fine8 = *off;
381700100614
381800100614         //?Emissione videata
381900100614         DOW  not $Fine8;
382000100614           exfmt MK19W08;
382100100614
382200100614         //?F06-Conferma
382300100614           IF  dsp_aid = c_F06;
382400100614         //?Perdo modifiche
382500100614             IF  W08sino = 'S';
382600100614               $Fine8 = *on;
382700100614               $Video = 'D2';
382800100614               $Modaffianca = *off;
382900100614               exsr F12D02;
383000100614             ENDIF;
383100100614         //?Non Perdo modifiche
383200100614             IF  W08sino = 'N';
383300100614               $Fine8 = *on;
383400100614               $Video = 'D2';
383500100614               $inzd02 = *on ;
383600100614             ENDIF;
383700100614           ENDIF;
383800100614         ENDDO;
383900100614
384000100614       ENDSR;
384100110225
384200110225       //--------------------------------------------------------------
384300110225       //?Gestione videata W09
384400110225       //--------------------------------------------------------------
384500110225       BEGSR GesW09;
384600110225
384700110225         //?Inizializzazione videata
384800110225         $Fine9 = *off;
384900110225
385000110225         //?Emissione videata
385100110225         DOW  not $Fine9;
385200110225           write MK19T01;
385300110225           write MK19D02;
385400110225           exfmt MK19W09;
385500110225
385600110225         //?Annullo l'attività e torno al TRMK18R
385700110301           IF  *inkf;
385800110225             $Fine9 = *on;
385900110225             $Fine  = *on ;
386000110225             chain (imk19tat:imk19atn:imk19atnp) tiatc01l;
386100110225             IF  %Found(TIATC01L);
386200110301               delete TIATC000;
386300110412         // se l'attività era un progressivo uno vado a cancellare le note
386400110412         // con progressivo 0
386500110412                     If imk19atnp = 1 ;
386600110412                        clear trmk20ds ;
386700110412                        imk20tla = 'L';
386800110412                        imk20flm = 'D';
386900110412                        imk20cpo = cpocpo ;
387000110412                        imk20rsc = v02cpd ;
387100110412                        imk20tat = imk19tat ;
387200110412                        imk20atn = imk19atn ;
387300110412                        imk20atnp= imk19atnp -1;
387400110412                        Trmk20r ( kpjba : trmk20ds );
387500110412                     endif ;
387600110412         // comitto se richiesto e se non ci sono errori
387700110412                IF  IMK19cmt = 'S';
387800110412                    commit ;
387900110412                    clear trmk05ds;
388000110412                    IMK05cpo = CPOcpo;
388100110412                    trmk05r (kpjba : TRMK05DS);
388200110412         //?se categoria variata aggiorno potenziale
388300110412                    IF  OMK05err = *blanks and OMK05cat <> CPOfls;
388400110412                        chain(e) CPOcpo TNCPO01L;
388500110412                        IF  not %error and %found(TNCPO01L);
388600110412                            CPOfls = OMK05cat;
388700110412                            update TNCPO000;
388800110412                        ENDIF;
388900110412                    ENDIF;
389000110412                ENDIF;
389100110225             ENDIF;
389200110225           ENDIF;
389300110225         ENDDO;
389400110225
389500110225       ENDSR;
389600100224
389700110527       //--------------------------------------------------------------
389800110527       //?Gestione videata W10
389900110527       //--------------------------------------------------------------
390000110527       BEGSR GesW10;
390100110527
390200110527         //?Emissione videata
390300110530           write MK19T01;
390400110530           write MK19D02;
390500110527           exfmt MK19W10;
390600110530           $Video = 'D2';
390700110527
390800110527       ENDSR;
390900090611       //--------------------------------------------------------------
391000090611       //?Gestione tasto funzionale F12 da videata W03
391100090611       //?F12=Ritorno
391200090611       //--------------------------------------------------------------
391300090611       BEGSR F12W03;
391400090611
391500090611         // Ritorno alla videata D02
391600090611            $Video = 'D2';
391700100118
391800100202         //?Pulisco campo comodo n. trattativa e commerciale
391900100118         clear newnrv;
392000100202         clear w03com;
392100090611
392200090611       ENDSR;
392300090611
392400090610       //--------------------------------------------------------------
392500090610       //?Gestione tasto funzionale F12 da videata D02
392600090610       //?F12=Ritorno
392700090610       //--------------------------------------------------------------
392800090610       BEGSR F12D02;
392900100614
393000100614       //?Se modificato affiancamento devo avvisare che le modifiche andranno perse
393100100614         IF  $ModAffianca;
393200100614           $Video = 'W8';
393300100614           leavesr;
393400100614         ENDIF;
393500090610
393600091127         // verifico se si tratta della prima telefonata da cliente o potenziale
393700091127         // ritorno ai subfile di selezione
393800091127
393900091127        If imk19fle = 'C' ;
394000100802        // exsr Primo_cli ;
394100100802        // leavesr ;
394200091127        endif ;
394300091127
394400091127        If imk19fle = 'P' ;
394500100409           W_mk01f12 = '1' ;
394600100409        // exsr Primo_po  ;
394700100409        // leavesr ;
394800091127        endif ;
394900090611         // Ritorno al programma chiamante
395000100422
395100100422         omk19err = '1' ;
395200100422
395300090611         $fine = *on ;
395400090611
395500090610       ENDSR;
395600090610
395700090610
395800090610       //--------------------------------------------------------------
395900090610       //?Gestione tasto funzionale F14 da videata D02
396000090610       //?F14=Informazioni Commerciali
396100090610       //--------------------------------------------------------------
396200090610       BEGSR F14D02;
396300090610
396400090610         // Richiamo il pgm informazioni Commerciali
396500090610         clear     trmk50ds   ;
396600090610         i50cpo=cpocpo ;
396700120313         If imk19fle <> '5' ;
396800100329            i50mod = 'G' ;
396900100329         else ;
397000100329            i50mod=  'I' ;
397100100329         endif ;
397200090610         i50rag=v02cpd   ;
397300090610         i50pgm='TRMK19R' ;
397400120124
397500120124       //?Se il richiamo delle INFO è stato pilotato da attività 72 devo passare
397600120124       //?il campo I50atb = 0 per obbligare l'azzeramento della % affido a BRT
397700120124         IF  info_cac = '72';
397800120208           I50atb = '0';
397900120125         //?questo lo faccio però solo se sto bloccando l'ultimo cliente della catena
398000120125         //?se ne ho ancora dei NON bloccati non passo il dato al trmk50r
398100120125           setll cpocpo CNACO16L;
398200120125           reade cpocpo CNACO16L;
398300120125           DOW not %eof(CNACO16L);
398400120125             IF  ACOabl = *blanks;
398500120125               clear I50atb;
398600120125               leave;
398700120125             ENDIF;
398800120125             reade cpocpo CNACO16L;
398900120125           ENDDO;
399000120125
399100120124         ENDIF;
399200120524
399300120524       //?Se il richiamo delle INFO è stato pilotato da attività 41 con esito positivo
399400120524       //?su trattativa di tipo I=Incremento devo impostare
399500120524       //?il campo I50atb = % per obbligare l'aumento della % affido a BRT se = 0
399600120524         IF  info_cac = '41' and §VISesito = 'P' and VIStpv = 'I';
399700120524           I50atb = '%';
399800120524         ENDIF;
399801140929
399802140929        // Se chiusa trattativa chiedo INFO obbligatorie indipendentemente dalla causale
399803140929        // e dal tipo trattativa come richiesto dal presidio vendite
399804140929         IF  wchiudo_tr = 'S' ;
399805140929             i50obl = 'S';
399806140929         Endif ;
399900120124
400000090610         TRMK50R  (kpjba:trmk50ds) ;
400001140929
400003140929         clear Wchiudo_tr ;
400100090610
400200090610       ENDSR;
400300090610
400400090610       //--------------------------------------------------------------
400500090610       //?Gestione tasto funzionale F18 da videata D02
400600090610       //?F18=Note
400700090610       //--------------------------------------------------------------
400800090610       BEGSR F18D02;
400900090610
401000090610         // Richiamo il pgm gestione note TRMK20R
401100090610         clear trmk20ds ;
401200090610         if giafatto = *on or visualizza = *on  ;
401300090610            imk20flm = 'I';
401400110510            imk20tla = 'L';
401500090610         else ;
401600090610            imk20flm = 'M';
401700090610         endif ;
401800091105         // se valorizzato il cliente passo il cliente
401900091105         If v02cli > 0 ;
402000091105            imk20ksc = v02cli ;
402100091105            imk20rsc = v02clid ;
402200091105         else ;
402300091105            imk20cpo = cpocpo ;
402400091105            imk20rsc = v02cpd ;
402500091027         endif ;
402600100318         If imk19fle = '2' ;
402700100318            imk20flm = ' ';
402800100318            imk20tat = imk19tat ;
402900100318            imk20atn = imk19atn ;
403000100318            imk20atnp= imk19atnp -1;
403100100318         else;
403200100318            emk20no1 = v02no1 ;
403300100318            emk20no2 = v02no2 ;
403400100318         endif ;
403500090616         Trmk20r ( kpjba : trmk20ds );
403600100318         // se dato f18 mentre sono in manutenzione devo valorizzare le note parte alta
403700100318         // del video ....
403800100318         If imk19fle = '2' ;
403900100318                clear trmk20ds ;
404000100318                imk20tla = 'L';
404100100318                imk20flm = 'R';
404200100318                imk20cpo = cpocpo ;
404300100318                imk20rsc = v02cpd ;
404400100318                imk20tat = imk19tat ;
404500100318                imk20atn = imk19atn ;
404600100318                imk20atnp= imk19atnp -1;
404700100318                Trmk20r ( kpjba : trmk20ds );
404800100318                v02no1o =  emk20no1 ;
404900100318                v02no2o =  emk20no2 ;
405000100318                piunote = (omk20piu = '+');
405100100318
405200100318         else;
405300100318            v02no1 =  emk20no1 ;
405400100318            v02no2 =  emk20no2 ;
405500100318            altrenote = (omk20piu = '+');
405600100318         endif ;
405700090610
405800090610       ENDSR;
405900090610
406000090610       //--------------------------------------------------------------
406100090610       //?Gestione tasto funzionale F01 da videata D02
406200091021       //?F01=Anagrafica
406300090610       //--------------------------------------------------------------
406400090610       BEGSR F01D02;
406500090610
406600091021       If cliente = *off ;
406700091021         // Richiamo il pgm gestione  potenziali se non c'è il cliente valorizzato
406800091104         clear trmk01ds   ;
406900100409         clear kpjbu ;
407000091027         // se sono in visualizzazione imposto interrogazione
407100091027         if imk19fle <> '5' ;
407200091104            mk01k10 ='V'   ;
407300091027         else ;
407400091104            mk01k10 ='N'   ;
407500091027         endif ;
407600091104         mk01cpo =cpocpo ;
407700100409         TRMK02c  (kpjba:Trmk01ds);
407800090610         // Se al ritorno dal pgm c'è errore richiamo il pgm dei potenziali in interrogazione
407900091104         If  mk01err <> ' ' ;
408000091104             clear trmk01ds   ;
408100091104             mk01k10 ='N'   ;
408200091104             mk01cpo =cpocpo ;
408300100409             TRMK02c  (kpjba:Trmk01ds);
408400090610         endif ;
408500091021       else ;
408600091021         // Richiamo il pgm gestione cliente
408700091021
408800091021          clear Tnta60ds ;
408900091021          ta60flg = 'I';
409000091105          ta60nrv = v02cli ;
409100091105          ta60int = 'S';
409200091021
409300110428          tnta60r (kpjba:tnta60ds) ;
409400091021
409500091021       // gestisco i dati di ritorno
409600091021
409700091021          if ta60err <> *blanks ;
409800091021             errMessage  = *on;
409900091021             errGenerico = *on;
410000091021             V1Dmsg = ta60msg;
410100091021             leavesr;
410200091021          EndIf ;
410300091021       endif ;
410400090610
410500091022         // recupero i dati del potenziale/clienti
410600091021         exsr AnagD02 ;
410700091022         // recupero i dati del potenziale/clienti  responsabili trasporti
410800091022         exsr RespD02 ;
410900091022
411000090610
411100090610       ENDSR;
411200100928
411300100928       //--------------------------------------------------------------
411400100928       //?Gestione tasto funzionale F05 da videata D02
411500100928       //?F05=Attività
411600100928       //--------------------------------------------------------------
411700100928       BEGSR F05D02;
411800100928
411900100928         clear TRMK21DS;
412000100928         I21mod = '9';
412100100928         I21cpo = %dec(V02cpo:11:0);
412200100928         I21rsc = V02cpd;
412300100928         IF  v02cli > 0;
412400100928           I21ksc = V02cli;
412500100928           I21rsc = V02clid;
412600100928         ENDIF;
412700100928         kpjbu = TRMK21DS;
412800100928         trmk21r (kpjba);
412900100928         TRMK21DS = kpjbu;
413000100928         IF  O21err <> *blanks;
413100100928           ErrGenerico = *on;
413200100928           ErrMessage  = *on;
413300100928           V1Dmsg      = O21msg;
413400100928           leavesr;
413500100928         ENDIF;
413600100928
413700100928       ENDSR;
413800090701
413900091026       //--------------------------------------------------------------
414000091026       //?Gestione tasto funzionale F09 da videata D02
414100091210       //?F09=Trattativa
414200091026       //--------------------------------------------------------------
414300091026       BEGSR F09D02;
414400091026
414500100202         // Richiamo la trattativa
414600091026             clear tnta88ds ;
414700091026             ita88cmt = 'N';
414800091118             ita88fle = 'G' ;
414900100202             IF  visualizza;
415000100202             ita88fle = '5' ;
415100100202             ENDIF;
415200091026             ita88fpr = 'A' ;
415300091026             ita88nrv = v02vis ;
415400100225             ita88com = imk19com ;
415500100225             ita88comd= imk19comd ;
415600091026             Tnta88r ( kpjba : tnta88ds );
415700091026
415800091026       ENDSR;
415900091026
416000091026       //--------------------------------------------------------------
416100091026       //?Gestione tasto funzionale F11 da videata D02
416200091026       //?F11=Agenda
416300091026       //--------------------------------------------------------------
416400091026       BEGSR F11D02;
416500091026
416600091026         // Richiamo il pgm visualizzazione agenda
416700091027
416800091027            clear trmk82ds ;
416900100120            imk82rich = 'A' ;
417000091027         // definisco il commerciale da passare
417100091027
417200091027         // vince il commerciale che sta eseguendo l'attività dopo viene
417300091027         // il commerciale che dovrebbe eseguire l'attività  e per ultimo
417400091027         // il commerciale del cliente / potenziale
417500091027
417600091027            select ;
417700100329            when v02com <> *blanks ;
417800100329                imk82cage = %dec(v02com:7:0) ;
417900100329            when v02cmi <> *blanks ;
418000100329                imk82cage = %dec(v02cmi:7:0) ;
418100091111            when v02cmp <> *zeros  ;
418200091111                imk82cage = v02cmp ;
418300091027            endsl ;
418400091027
418500091027            kpjbu = trmk82ds ;
418600091027
418700091027            Trmk82r (kpjba);
418800091026
418900091026       ENDSR;
419000091026
419100091027       //--------------------------------------------------------------
419200091027       //?Gestione tasto funzionale F11 da videata W03
419300091027       //?F11=Agenda
419400091027       //--------------------------------------------------------------
419500091027       BEGSR F11W03;
419600091027
419700091027         // Richiamo il pgm visualizzazione agenda
419800091027
419900091027            clear trmk82ds ;
420000100120            imk82rich = 'A' ;
420100091027         // definisco il commerciale da passare
420200091027
420300091027         // vince uno dei due/tre commerciali valorizzati nella window
420400091027         // il commerciale che sta eseguendo l'attività dopo viene
420500091027         // il commerciale che dovrebbe eseguire l'attività  e per ultimo
420600091027         // il commerciale del cliente / potenziale
420700091027
420800091027            select ;
420900091027            when w03com <> *blanks ;
421000091111                imk82cage = %dec(w03com:7:0) ;
421100091027            when v02cmi <> *blanks ;
421200091111                imk82cage = %dec(v02cmi:7:0) ;
421300091027            when v02cmp <> *zeros  ;
421400091111                imk82cage = v02cmp ;
421500091027            endsl ;
421600091027
421700091027            // se data scadenza valorizzata la passo nella DS
421800091027            If w03dsc > 0 ;
421900091111                imk82data = ww03dsc ;
422000091027            endif ;
422100091027
422200091027            kpjbu = trmk82ds ;
422300091027
422400091027            Trmk82r (kpjba);
422500091027
422600091027       ENDSR;
422700100224
422800100224       //--------------------------------------------------------------
422900100224       //?Gestione tasto funzionale F16 da videata D02
423000100224       //?F16=Annullamento
423100100224       //--------------------------------------------------------------
423200100224       BEGSR F16D02;
423300100224
423400100224         // emetto una windows per la richiesta di conferma annullamento
423500100224          $video = 'W5';
423600100224
423700100224       ENDSR;
423800100224
423900091210       //--------------------------------------------------------------
424000091210       //?Gestione tasto funzionale F20 da videata D02
424100091210       //?F20=Affiancamento
424200091210       //--------------------------------------------------------------
424300091210       BEGSR F20D02;
424400091210
424500091210         clear TRMK22ds;
424600091210         IMK22fmt = '4';
424700091210
424800091210         SELECT;
424900091210           WHEN  Manutenzione;
425000091215             IMK22fcmt = '1';
425100091215             IMK22cmt = 'N';
425200091210             IMK22fle = '2';
425300091210           WHEN  Visualizza;
425400091210             IMK22fle = '5';
425500091210         ENDSL;
425600091210
425700091210         IMK22cmm  = %dec(v02cmi:7:0);
425800091211         IMK22dad  = w02scd;
425900091210         IMK22hda  = v02sco * 100;
426000091210         IMK22oii  = v02oii;
426100091210         IMK22ofi  = v02ofi;
426200091216         IMK22cac  = v02cae;
426300100114         IMK22esi  = v02fatto;
426400091210         IMK22tat  = IMK19tat;
426500091210         IMK22atn  = IMK19atn;
426600091210         IMK22atnp = IMK19atnp;
426700091210
426800091210         trmk22r (kpjba : TRMK22ds);
426900091210
427000091210         //?Se ritorna errore lo visualizzo
427100091210         IF  OMK22err <> *blanks;
427200091210           ErrMessage  = *on;
427300091210           ErrGenerico = *on;
427400091210           V1Dmsg      = OMK22msg;
427500091210         ENDIF;
427600100614
427700100614         //?Salvo che ho fatto una modifica all'affiancamento
427800100614         //?in modo da avvisare l'utente se fa F12 o F3 che le modifiche
427900100614         //?appena fatte andranno perse
428000100614         IF  OMK22f12 = *blanks;
428100100614           $ModAffianca = *on;
428200100614         ENDIF;
428300100224
428400100224        // Verifico se inseriti affiancamenti
428500100224           k_atlatl  = IMK19atn;
428600100224           k_atlatlp = IMK19atnp;
428700100224           chain  %kds(K02tiatl) TIATL02L;
428800100224           IF  %found(Tiatl02l);
428900100224             ConAffianca = *on;
429000100422           ELSE ;
429100100422             ConAffianca = *off ;
429200100224           ENDIF;
429300091210
429400091210       ENDSR;
429500091027
429600090701       //--------------------------------------------------------------
429700100920       //?Gestione tasto funzionale F21 da videata D02 x richiamo TRMK24
429800100920       //?F21=Invio materiale illustrativo
429900090701       //--------------------------------------------------------------
430000090701       BEGSR F21D02;
430100090701
430200100921         // inizializzo alcuni campi per invio lettera
430300100921          clear trmk24ds ;
430400100921          If v02cli > 0 ;
430500100921               Imk24Tde = 'C' ;
430600100921               Imk24Cde = v02cli ;
430700100921          else ;
430800100921               Imk24Tde = 'P' ;
430900100921               Imk24Cde = %dec(v02cpo:11:0) ;
431000100921          endif ;
431100100921
431200100921        // recupero i dati del responsabile trasporti
431300100921          exsr respd02 ;
431400100921          Imk24rst = vh2rtr ;
431500100921        // imposto indirizzo mail immesso nella window
431600100921          Imk24mai = v02mrt ;
431700100921
431800100921         // imposto il commerciale che firmerà la mail, il comm. del potenziale se > 0
431900100921          If v02cmp > 0 ;
432000100921               Imk24cmm = v02cmp ;
432100100921          else ;
432200100921               Imk24cmm = %dec(v02com:7:0);
432300100921          endif ;
432400100921
432500100921          trmk24r ( kpjba : TRMK24ds );
432600100921
432700100921       // Scrivo nota di invio mail se inviata
432800100921          if omk24fxx = '06' ;
432900100921             clear ticpn000 ;
433000100921
433100100921             cpncpo=cpocpo;
433200100921             cpnksc=v02cli;
433300100921             data_iso = (%date()) ;
433400100921             cpndim = %dec(data_iso) ;
433500100921             cpnhim = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
433600100921             cpnsns='S'   ;
433700100921             cpnpos=dutpou;
433800100921             cpnpru=knmus;
433900100921             cpnpno = 1;
434000100921         // imposto il campo nota
434100100921
434200100921             destina = %trim(Omk24mai) ;
434300100921
434400100921             cpnnot='Inviato testo lettera ' + Omk24tlt + ' a nome di '
434500100921                  + %editc(Omk24cmm:'X')  + ' ' + %trim(Omk24cmd) + ' a ' ;
434600100921             write ticpn000;
434700100921             cpnpno+=1;
434800100921             cpnnot=%trim(destina) ;
434900100921             write ticpn000;
435000100924             IF  IMK19cmt = 'S' and OMK19err = *blanks;
435100100924                 commit;
435200100924             ENDIF;
435300100921
435400100921           endif ;
435500090701
435600090701       ENDSR;
435700090701
435800090610       //--------------------------------------------------------------
435900090610       //?Gestione tasto funzionale F02 da videata D02
436000090610       //?F02=Contatti
436100090610       //--------------------------------------------------------------
436200090610       BEGSR F02D02;
436300090610
436400100202         // Richiamo i contatti
436500090610         clear tnta12ds   ;
436600100202         IF  Visualizza;
436700100202           ta12ric = 'V';
436800100202         ENDIF;
436900091118         If v02cli > *zeros ;
437000091118            ta12apl='C'   ;
437100091118            ta12ksc=v02cli ;
437200091118            ta12rag=v02clid  ;
437300091118         else ;
437400091118         If v02vis > *zeros ;
437500091118            ta12apl='T'   ;
437600091118            ta12nrv=v02vis ;
437700091118         else ;
437800091118            ta12apl='P'   ;
437900091118            ta12pot=cpocpo ;
438000091118            ta12rag=v02cpd   ;
438100091105         endif ;
438200091118         endif ;
438300090610         TNTA12R  (kpjba:TNTA12ds);
438400090610         If ta12err <> ' ';
438500090610            errMessage  = *on;
438600090610            errGenerico = *on;
438700090610            V1Dmsg = ta12msg ;
438800090610            leavesr;
438900090610         endif;
439000090610
439100090610         // recupero i dati del potenziale responsabili trasporti
439200090610         exsr RespD02 ;
439300090610
439400090610       ENDSR;
439500090610
439600090610
439700090610       //--------------------------------------------------------------
439800090610       //?Gestione tasto funzionale F06 da videata D02
439900090610       //?F06=Conferma
440000090610       //--------------------------------------------------------------
440100090610       BEGSR F06D02;
440200090610
440300090610        // registro i dati variati sul file
440400090611            Chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
440500090610            If %found(tiatc01l) ;
440600090610               atcdad   = W02scd  ;
440700090610               atchda = v02sco * 100 ;
440800090610               atccmm  = %dec(v02cmi:7:0);
440900090804               if  atctat = 'A' ;
441000090804                   atcoii = V02oii ;
441100090804                   atcofi = V02ofi ;
441200090804               endif ;
441300090610               update tiatc000  ;
441400090610            endif ;
441500100409
441600100409       // Se appuntamento con affiancamento richiamo la gestione dell'affiancamento
441700100614       //?solo se non ero già andata in manutenzione e dato F6
441800111102       // oppure è già andato in manutenzione ma sono stati modificati i dati dell'appuntamento
441900111102         If atctat = 'A' and ConAffianca = *on and (not $ModAffianca or
442000111102         ($ModAffianca and ((sav_atcdad <> 0 and  sav_atcdad <> atcdad ) or
442100111102         (sav_atchda <> 0 and  sav_atchda <> atchda ) or
442200111102         (sav_atcoii <> 0 and  sav_atcoii <> atcoii ) or
442300111102         (sav_atcofi <> 0 and  sav_atcofi <> atcofi ) or
442400111102         (sav_atccmm <> 0 and  sav_atccmm <> atccmm ))));
442500100412           exsr F20d02 ;
442600100409           IF  OMK22f12 = 'S' or OMK22err <> *blanks;
442700100412               IF  IMK19cmt = 'S' ;
442800100412                   rolbk ;
442900100412               ENDIF;
443000100409             leavesr;
443100100409           ENDIF;
443200100409         ENDIF ;
443300091218
443400091218        //?Se è stata modificata la data dell'appuntamento devo scrivere una nota
443500091218         IF  sav_atcdad <> 0 and sav_atcdad <> atcdad and atctat = 'A';
443600091218           data_eur = %date(sav_atcdad:*iso);
443700091218           wdata    = %dec(data_eur);
443800091218           clear trmk20ds;
443900091218           imk20tla = 'L';
444000091218           imk20flm = 'C';
444100091218           imk20fno = 'S';
444200100215           IMK20cmt = '1';
444300091218           imk20cpo = cpocpo;
444400091218           IF  v02cli > 0;
444500091218             imk20ksc = v02cli;
444600091218           ENDIF;
444700091218            imk20tat = atctat;
444800091218            imk20atn = atcatn;
444900091218            imk20atnp= atcatnp - 1;
445000091218            imk20nrv = atcnrv;
445100091222            emk20no1 = 'Data appuntamento variata, precedente: ' +
445200091218                       %editc(wdata:'Y');
445300091218            Trmk20r ( kpjba : trmk20ds );
445400091218
445500091218         ENDIF;
445600100409
445700100915       // Se è stato richiesta la conferma con mail richiamo il pgm TRMK23R1
445800100419           If   dsp_aid = c_F08 ;
445900100915        // richiamo con tipo richiamo blank in quanto in questa fase deve fare sia la richiesta
446000100915        // della mail che inviarla
446100100915                clear  TRMK23ds1;
446200100419                exsr Ric_Trmk23 ;
446300100419           Endif ;
446400100419
446500100304       //?Comitto se richiesto e se non ci sono errori
446600100304         IF  IMK19cmt = 'S' and OMK19err = *blanks;
446700100304           commit;
446800100304         ENDIF;
446900090610
447000100329         $Fine   = *on ;
447100100329
447200090610       ENDSR;
447300090610
447400090610       //--------------------------------------------------------------
447500090610       //?Gestione tasto funzionale F06 da videata D02 + W03
447600090610       //?F06=Conferma
447700090610       //--------------------------------------------------------------
447800090610       BEGSR F06W03D02;
447900090610
448000090610        // registro il contatto in tiatc
448100090610            exsr regis_atc ;
448200090610
448300090610       // scrivo il contatto in scadenza
448400090610         // pulisco i campi che non devono essere valorizzati
448500090610            clear atcest ;
448600090610            clear atcdco ;
448700090610            clear atchco ;
448800090610            clear atccco ;
448900090610            clear atcpru ;
449000090610            clear atccac ;
449100100310            clear atcoii ;
449200100310            clear atcofi ;
449300090702            clear atcco3 ;
449400091021            clear atccnw ;
449500091021            clear atcnra ;
449600100802            clear atcflo ;
449700090610
449800090702            // valorizzo il tipo contatto con quello indicato in tabella causali contatto
449900090702            atctat   = §ccotat ;
450000091214            IF  ConAffianca;
450100091214              atcatnp = sav_atlatnp + 1;
450200091214            ELSE;
450300090610            atcatnp  = atcatnp + 1 ;
450400091214            ENDIF;
450500090610            atccad   = W03cae   ;
450600090610            atcdad   = WW03dsc  ;
450700090610            atchda = w03osc * 100 ;
450800090610            atccmm  = %dec(W03com:7:0);
450900090703            // solo se è un appuntamento scrivo le ore di impegno  e il commerciale che inserisce
451000090703            // l'appuntamento
451100090702            if  prox_appunta = *on ;
451200090702                atcoii = w03oii ;
451300090702                atcofi = w03ofi ;
451400090702                atcco3 = %dec(v02com:7:0) ;
451500090702            endif ;
451600091023            // valorizzo data ora e utente immissione
451700091023            data_iso = (%date()) ;
451800091023            atcdim = %dec(data_iso) ;
451900091023            atchim = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
452000091023            atcpri = knmus ;
452100091113            atcnrv = w03nrv ;
452200100302            exsr Sr_ValCnw ;
452300100510            // se si tratta di un'attività 80 che apre una nuova trattativa siglo con atcatb= '*'
452400100510            // in modo tale venga riconosciuta in un'eventuale richiesta di cancellazione delle 80
452500100510            // dal programma trmk17r nel momento della creazione di una nuova attività dalla stampa
452600100510            // o invio mail tariffa. Questo flag dovrà poi essere pulito nel momento in cui ritorno
452700100510            // dalla creazione della trattativa
452800100723            // 21/07/10 non lo faccio + in quanto facciamo il commit nel tnta88r (gestione trattati-
452900100723            // va) così anche nel caso si voglia cancellare una 80 anche in fase di apertura di una
453000100723            // nuova trattativa si può fare
453100100723            //If atccad = '80' and  $new_trattati = *on ;
453200100721            // atcatb = '*' ;
453300100510            // mi salvo la chiave dell'attività da pulire dopo
453400100723            //   K80_atctat  = atctat ;
453500100723            //   K80_atcatn  = atcatn ;
453600100723            //   k80_atcatnp = atcatnp;
453700100723            // endif ;
453800110302
453900110302            //?Memorizzo la categoria del potenziale all'apertura dell'attività
454000110302              dATC01 = ATCflo;
454100110302              §ATCcapo1 = CPOfls;
454200110302              ATCflo = dATC01;
454300090610            write tiatc000  ;
454400121018
454500121018         //?Aggiorno il potenziale --> data primo contatto e categoria
454600121018           exsr agg_CPO;
454700091210
454800091210         //?Se appuntamento controllo se devo richiamare gestione affiancamento
454900091210         IF  prox_appunta and w03aff = 'S';
455000091210           exsr Ric_TRMK22 ;
455100091210           //?se errore o F12 devo tornare alla Window dell'appuntamento
455200100113           //?e devo riagganciare il TIATC perchè è già stato aggiornato
455300091210           IF  OMK22f12 = 'S' or OMK22err <> *blanks;
455400100113                chain (imk19tat:imk19atn:imk19atnp) tiatc01l ;
455500091210             leavesr;
455600091210           ENDIF;
455700091210         ENDIF;
455800100507
455900100507         // se dato f8 conferma (solo in caso di appuntamento )
456000100507         //?Richiamo pgm per richiesta dati mail
456100100507         If   dsp_aid = c_F08 ;
456200100915             clear  TRMK23ds1;
456300100915             // richiamo il programma con R in quanto voglio solo i dati della mail senza inviare
456400100915             // cosa che si farà nella fase finale di conferma
456500100915             Imk23rei = 'R';
456600100915             exsr Ric_trmk23 ;
456700100507         ENDIF;
456800090610
456900091113         // verifico se c'è' un pgm da richiamare in base alla causale
457000100419           If  §ccoapt = 'S' and $new_trattati = *on ;
457100091113         // richiamo il programma gestione trattativa
457200100510               exsr Ric_TNTA88 ;
457300100510         // Se l'apertura della trattativa è stata pilotata dalla 80 verifico
457400100510         // se devo pulire il flag
457500100723            // 21/07/10 non lo faccio + in quanto facciamo il commit nel tnta88r (gestione trattati-
457600100723            // va) così anche nel caso si voglia cancellare una 80 anche in fase di apertura di una
457700100723            // nuova trattativa si può fare
457800100723         //    If atccad = '80' and  $new_trattati = *on ;
457900100723         // // aggancio il record attività' per pulire il flag
458000100723         //        chain (k80_atctat:K80_atcatn:K80_atcatnp) tiatc01l ;
458100100723         //        If %found(tiatc01l) ;
458200100723         //           atcatb = ' ' ;
458300100723         //           update tiatc000 ;
458400100723         //           commit ;
458500100723         //        endif ;
458600100723         //        clear  K80_atctat  ;
458700100723         //        clear  K80_atcatn  ;
458800100723         //        clear  k80_atcatnp ;
458900100723         //    endif ;
459000091113                          If ota88f12 = 'S' ;
459100091113                             $inzd02 = *on ;
459200091113                             $video  = 'D2';
459300091113                             leavesr ;
459400091113                          endif ;
459500091113           endif ;
459600100311
459700100311         // chiedo conferma chiusura trattativa
459800100311             If  $chiedi_conf = *on ;
459900110622                  exsr Ges_Chiudotr ;
460000100311         // rolbk se ci sono errori o non richiesto annullamento trattativa
460100100503                  if (imk19cmt = 'S' and omk19err <> ' ') or w06sino = 'N' ;
460200100311                        rolbk ;
460300100311                        $inzd02 = *on ;
460400100311                        $Video = 'D2';
460500100311                        leavesr ;
460600100311                     endif ;
460700100311             endif ;
460800100419
460900100419         // se dato f8 conferma (solo in caso di appuntamento )
461000100419           If   dsp_aid = c_F08 ;
461100100915          // faccio il richiamo con solo invio mail in quanto ho già i dati della mail nella ds
461200100915                Imk23rei = 'I';
461300100419                exsr Ric_trmk23 ;
461400100419           endif ;
461500100419
461600100419
461700100419         // Prima di tornare al pgm chiamante verifico se devo chiedere infoCommerciali
461800100419
461900100419             exsr Sr_InfoComm ;
462000110412
462100110502         // richiamo il TRMK05 che mi ricalcola la categoria se in categoria eliminabile
462200121018          If CPOfls = 'E';
462300110412               clear trmk05ds;
462400110412               IMK05cpo = CPOcpo;
462500110412               trmk05r (kpjba : TRMK05DS);
462600110412         //?se categoria variata aggiorno potenziale
462700110412               IF  OMK05err = *blanks and OMK05cat <> CPOfls;
462800110412                   chain(e) CPOcpo TNCPO01L;
462900110412                   IF  not %error and %found(TNCPO01L);
463000110412                       CPOfls = OMK05cat;
463100110412                       update TNCPO000;
463200110412                   ENDIF;
463300110412               ENDIF;
463400110502          ENDIF;
463500100311
463600090611         // Ritorno al programma precedente
463700090611
463800090611            $fine  = *on ;
463900090610
464000090610       ENDSR;
464100121018
464200121018       //--------------------------------------------------------------
464300121018       //?Aggiorno dati su TNCPO.
464400121018       //--------------------------------------------------------------
464500121018       BEGSR agg_CPO;
464600121018
464700121018       //?Imposto data primo contatto sul Potenziale
464800121018         IF  §CCOdpc <> 'N' and (§CPOdtpcon = *blanks or §CPOdtpcon = *zeros);
464900121018           clear dCPO01;
465000121018           chain(e) CPOcpo TNCPO01L;
465100121018           IF  not %error and %found(TNCPO01L);
465200121018             dCPO01 = CPOrst;
465300121018             §CPOdtpcon = %editc(W02dat:'X');
465400121018             CPOrst = dCPO01;
465500121018             update TNCPO000;
465600121018           ENDIF;
465700121018         ENDIF;
465800121018
465900121018       //?Se attività 14/15 ricalcolo la categoria del potenziale
466000121018         IF ATCcac = '14' or ATCcac = '15';
466100121018           clear trmk05ds;
466200121018           IMK05cpo = CPOcpo;
466300121018           trmk05r (kpjba : TRMK05DS);
466400121018         //?se categoria variata aggiorno potenziale
466500121018           IF  OMK05err = *blanks and OMK05cat <> CPOfls;
466600121018             chain(e) CPOcpo TNCPO01L;
466700121018             IF  not %error and %found(TNCPO01L);
466800121018               CPOfls = OMK05cat;
466900121018               update TNCPO000;
467000121018             ENDIF;
467100121018           ENDIF;
467200121018         ENDIF;
467300121018
467400121018       ENDSR;
467500090701
467600091210       //--------------------------------------------------------------
467700091210       //?Rchiamo il programma di gestione esito offerta  TNTA56R
467800091210       //--------------------------------------------------------------
467900091210       BEGSR Ric_Tnta56;
468000091210
468100091210             clear tnta56ds ;
468200091210             ita56cmt = 'S';
468300091210             ita56fle = 'G' ;
468400091210             ita56fpr = 'A' ;
468500091210             ita56nrv = v02vis ;
468600100111             ita56tat = imk19tat;
468700100111             ita56atn = imk19atn;
468800100111             ita56atnp= imk19atnp;
468900100225             ita56com = imk19com ;
469000100225             ita56comd= imk19comd ;
469100091210             Tnta56r ( kpjba : tnta56ds );
469200091210
469300091210         // Se restituito errore => e c'è messaggio lo restituisco ...
469400091210         if  ota56err = 'E';
469500091210           errMessage  = *on;
469600091210           errGenerico = *on;
469700091210           V1Dmsg = ota56msg ;
469800091210
469900091210           If imk19cmt = 'S' ;
470000091210              rolbk ;
470100091210           endif ;
470200091210
470300091210           leavesr;
470400091210         endif;
470500091210
470600091210          // se dato f12 ripulisco gli aggiornamenti
470700091210
470800091210            If imk19cmt = 'S' and ota56F12 = 'S' ;
470900091210               rolbk ;
471000091210            endif ;
471100091210
471200091210         // se non ci sono errori e non è stato premuto F12
471300091210         //  confermo tutto quanto scritto fino ad ora
471400091210
471500091210           If ota56err = ' ' and ota56f12 = ' '  ;
471600091210             commit ;
471700091210           endif ;
471800091210
471900091210       ENDSR  ;
472000090701
472100091015       //--------------------------------------------------------------
472200091015       //?Rchiamo il programma di gestione trattative TNTA88R
472300091015       //--------------------------------------------------------------
472400091015       BEGSR Ric_Tnta88;
472500091015
472600091015             clear tnta88ds ;
472700091015             ita88cmt = 'S';
472800091015             ita88fle = 'P' ;
472900091016             ita88fpr = 'A' ;
473000091015             ita88cpo = cpocpo   ;
473100091027             ita88ksc = v02cli ;
473200091113             ita88nrv = newnrv ;
473300091027             if ita88ksc > 0 ;
473400091027                ita88fle = 'C' ;
473500091027             endif ;
473600091015             ita88cmmi = %dec(v02com: 7: 0);
473700091023             If w03com <> *blank ;
473800091023                ita88cmm = %dec(w03com: 7: 0);
473900091023             else ;
474000091027                ita88cmm = v02cmp ;
474100091023             endif ;
474200100225             ita88com = imk19com ;
474300100225             ita88comd= imk19comd ;
474400091015             Tnta88r ( kpjba : tnta88ds );
474500091015
474600091016         // Se restituito errore => e c'è messaggio lo restituisco ...
474700091015         if  ota88err = 'E';
474800091015           errMessage  = *on;
474900091015           errGenerico = *on;
475000091015           V1Dmsg = ota88msg ;
475100091016
475200091016           If imk19cmt = 'S' ;
475300091016              rolbk ;
475400100416              exsr Del_ticpn ;
475500091016           endif ;
475600091016
475700091015           leavesr;
475800091015         endif;
475900091015
476000091016          // se dato f12 ripulisco gli aggiornamenti
476100091016
476200091016            If imk19cmt = 'S' and ota88F12 = 'S' ;
476300091016               rolbk ;
476400100416               exsr Del_ticpn ;
476500091016            endif ;
476600091016
476700091113         // se non ci sono errori e non è stato premuto F12
476800091113         //  confermo tutto quanto scritto fino ad ora
476900091113
477000091113           If ota88err = ' ' and ota88f12 = ' '  ;
477100091016             commit ;
477200091016           endif ;
477300091016
477400091015       ENDSR  ;
477500101221
477600101221       //--------------------------------------------------------------
477700101221       //?Richiamo pgm gestione data caldo senza offerte.
477800101221       //--------------------------------------------------------------
477900101221       BEGSR Ric_Ta63;
478000101221
478100101221         clear TNTA63ds;
478200101221         ITA63fmt  = '4';
478300110118         ITA63fle  = 'I';
478400101221         ITA63nrv  = V02vis;
478500101221
478600101221         tnta63r (kpjba : TNTA63ds);
478700101221
478800101221       ENDSR  ;
478900101221
479000100416       //--------------------------------------------------------------
479100100416       //?Deleto le note che non riesce a fare il pgm con il rolbk
479200100416       //--------------------------------------------------------------
479300100416       BEGSR Del_ticpn;
479400100416                        clear trmk20ds ;
479500100416                        imk20tla = 'L';
479600100416                        imk20flm = 'D';
479700100416                        imk20cpo = cpocpo ;
479800100416                        imk20rsc = v02cpd ;
479900100416                        imk20tat = imk19tat ;
480000100416                        imk20atn = imk19atn ;
480100100416                        imk20atnp= imk19atnp ;
480200100416                        Trmk20r ( kpjba : trmk20ds );
480300100416       ENDSR  ;
480400090610       //--------------------------------------------------------------
480500090610       //?Registro i dati nel file tiatc
480600090610       //--------------------------------------------------------------
480700090610       BEGSR Regis_atc;
480800110304
480900090610         // se un primo contatto stacco un nuovo numero
481000090610         If primocontatto = *on ;
481100090610
481200090610            clear tiatc000 ;
481300090610
481400090610            RESET trul33ds;
481500090610            i33tla = 'L'  ;
481600090610            i33ope = 0    ;
481700090610            i33Cnu = 070  ;
481800090610            i33num = 1    ;
481900090610            i33Aaa = *YEAR;
482000090610            kpjbu = trul33ds;
482100090610            trul33r(kpjba);
482200090610            trul33ds = kpjbu;
482300090610            IF o33Err = 0;
482400090610              atcatn  = o33Nrf;
482500090610            ENDIF;
482600090610
482700090610       // imposto a zero il progressivo telefonata
482800090610            atcatnp  = 1 ;
482900090610       // imposto i campi che vengono inseriti solo nel primo contatto
483000090610            atctat = 'T' ;
483100090610            atccpo = %dec(v02cpo:11:0) ;
483200091021            atcksc = v02cli ;
483300090610         endif ;
483400090610        // Aggiorno un contatto in scadenza
483500090610       // questi si possono inserire anche se non sono i primi contatti
483600100114         // se appuntamento e sto esitando un'attività pianificata imposto come
483700100114         // esito il fatto o non fatto impostato a video
483800100114           IF  Appuntamento and
483900100114               Not Manutenzione and Not Vis_NonFatto and Not Visualizza and
484000100114               v02fatto <> *blanks;
484100100114             atcest = V02fatto;
484200100114           ELSE;
484300091215            atcest = §ccoesi;
484400100114          ENDIF;
484500090610            atcdco = w02dat ;
484600090610            atchco = v02ora * 100 ;
484700090610            atccco = %dec(v02com:7:0);
484800090610            atcpru = knmus  ;
484900090610            atccac = v02cae ;
485000091113
485100091113        // se devo creare trattativa da causale aggiorno il nuovo numero
485200091116         If §ccoapt = 'S' and newnrv > 0 and $new_trattati = *on ;
485300091113            atcnra = newnrv ;
485400091113         endif ;
485500100309
485600100309        // se ho richiesto l'esito della attività lo scrivo nei FLO
485700100309         If §ccores = 'S' ;
485800100309            clear datc01 ;
485900100309            datc01 = atcflo ;
486000100309            §atcesito = W07esi ;
486100100309            atcflo = datc01 ;
486200100309         endif ;
486300100309
486400121018        // imposto che è stato fatto contatto su potenziale mai contattato
486500121018         IF §CPOdtpcon = *blanks or §CPOdtpcon = *zeros;
486600100802            clear datc01 ;
486700100802            datc01 = atcflo ;
486800100802            §atcmaitr = 'S'    ;
486900100802            atcflo = datc01 ;
487000100802         endif ;
487100100802
487200090610        // scrivo il primo contatto ed aggiorno uno in scadenza
487300090610         If primocontatto = *on ;
487400091023            data_iso = (%date()) ;
487500091023            atcdim = %dec(data_iso) ;
487600091023            atchim = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
487700091023            atcpri = knmus ;
487800100302            exsr SR_valcnw ;
487900110302          //?Memorizzo la categoria del potenziale all'apertura dell'attività
488000110302            dATC01 = ATCflo;
488100110413            §ATCcapo2 = CPOfls;
488200110302            ATCflo = dATC01;
488300090610            write tiatc000  ;
488400090610         else ;
488500110302          //?Memorizzo la categoria del potenziale all'esecuzione dell'attività
488600110302            dATC01 = ATCflo;
488700110302            §ATCcapo2 = CPOfls;
488800110302            ATCflo = dATC01;
488900090610            update tiatc000 ;
489000090610         endif;
489100090610
489200090610         // se ci sono delle note le registro
489300090611         if v02no1 <> *blanks  or v02no2 <> *blanks ;
489400090610            clear trmk20ds ;
489500090610            imk20tla = 'L';
489600090610            imk20flm = 'C';
489700090610            imk20fno = 'S';
489800100215            imk20cmt = '1';
489900090610            imk20cpo = cpocpo ;
490000091027            If v02cli > 0 ;
490100091027               imk20ksc = v02cli ;
490200091027            endif ;
490300090610            imk20tat = atctat ;
490400090610            imk20atn = atcatn ;
490500090610            imk20atnp= atcatnp;
490600090610            imk20rsc = v02cpd ;
490700090610            emk20no1 = v02no1 ;
490800090610            emk20no2 = v02no2 ;
490900090610            emk20dim = atcdco ;
491000090610            emk20him = atchco ;
491100090616            Trmk20r ( kpjba : trmk20ds );
491200090610         endif ;
491300090610
491400121018       // solo nel caso in cui non si deve andare
491500110502         if §ccoupm =  ' ';
491600121018         //?Aggiorno il potenziale --> data primo contatto e categoria
491700121018           exsr agg_CPO;
491800090610         endif ;
491900090610
492000100329       // valorizzo i campi per il controllo delle info commerciali
492100100329         info_cac = atccac ;
492200100329         info_nrv = atcnrv ;
492300100329         info_nra = atcnra ;
492400100329
492500090610       ENDSR ;
492600091214
492700100302       //--------------------------------------------------------------
492800100302       //?Valorizzo il campo del cliente potenziale o codificato
492900100302       //--------------------------------------------------------------
493000100302       BEGSR Sr_ValCnw;
493100100302
493200100302       // Verifico se l'attività creata è su opotenziale o su cliente codificato
493300100302
493400110502       //   IF  atcksc = 0;
493500110502       //    IF  $gia_codifica = *on ;
493600110502       //      atccnw = 'C';
493700110502       //    ELSE;
493800110502       //      atccnw = 'P';
493900110502       //    ENDIF;
494000110502       //  ELSE;
494100110502       //   atccnw = 'C';
494200110502       //   ENDIF;
494300110502
494400110502       //  Imposto il flag di ATCCNW in base alla categoria del potenziale
494500110502       // se categoria = a "C" metto "C" altrimenti "P"
494600110502            If cpofls = 'C' ;
494700110502               atccnw = 'C' ;
494800110502            Else ;
494900110502               atccnw = 'P' ;
495000110502            Endif ;
495100100302
495200100302       ENDSR  ;
495300091214       //--------------------------------------------------------------
495400091214       //?Confermo esito affiancamento
495500091214       //--------------------------------------------------------------
495600091214       BEGSR Conf_Trmk22;
495700091214
495800091214         clear TRMK22ds;
495900091215         IMK22fcmt = '1';
496000091215         IMK22cmt = 'N';
496100091214         IMK22fmt = '4';
496200091214         IMK22fle = 'G';
496300091215         IMK22cac = v02cae;
496400100416         IMK22esi = V02fatto ;
496500091214         IMK22cmm  = %dec(v02com:7:0);
496600091214         IMK22dad  = w02dat;
496700091215         IMK22hda  = v02ora * 100;
496800091214         IMK22oii  = v02oii;
496900091214         IMK22ofi  = v02ofi;
497000091214         IMK22tat  = IMK19tat;
497100091214         IMK22atn  = IMK19atn;
497200091214         IMK22atnp = IMK19atnp;
497300091214
497400091214         trmk22r (kpjba : TRMK22ds);
497500091214
497600091214         //?Se ritorna errore lo visualizzo e faccio rollback
497700091214         IF  OMK22err <> *blanks;
497800091214           ErrMessage  = *on;
497900091214           ErrGenerico = *on;
498000091214           V1dmsg = OMK22msg;
498100091214           IF  IMK19cmt = 'S';
498200100721          // rolbk;
498300091214           ENDIF;
498400091214           leavesr;
498500091214         ENDIF;
498600091214
498700091214         //?Se F12 da gestione affiancamento rollback
498800091214         IF  IMK19cmt = 'S' and OMK22F12 = 'S';
498900100721         //rolbk;
499000091214           leavesr;
499100091214         ENDIF;
499200091214
499300091214         //?Se OK cerco ultimo progressivo
499400091214         clear sav_atlatnp;
499500091215         $EoF = *off;
499600091214         setll IMK19atn tiatl01l;
499700091215         DOW  not $EoF;
499800091214           reade IMK19atn tiatl01l;
499900091215           IF  %eof(tiatl01l);
500000091215             $EoF = *on;
500100091215             leave;
500200091215           ENDIF;
500300091215           sav_atlatnp = atlatnp;
500400091214         ENDDO;
500500091214
500600091214       ENDSR;
500700091210
500800091210       //--------------------------------------------------------------
500900091210       //?Richiamo il programma di gestione affiancamento TRMK22R
501000091210       //--------------------------------------------------------------
501100091210       BEGSR Ric_Trmk22;
501200091210
501300091210         clear TRMK22ds;
501400091215         IMK22fcmt = '1';
501500091215         IMK22cmt = 'N';
501600091210         IMK22fmt = '4';
501700091210         IF  Prox_Appunta;
501800091210           IMK22fle = 'I';
501900091210         ELSE;
502000091210           IMK22fle = 'G';
502100091210           IMK22cac = atccac;
502200100416           IMK22esi = v02fatto ;
502300091210         ENDIF;
502400100924         IF  §ccoupm <> ' ';
502500100412             IMK22cmm  = %dec(w03com:7:0);
502600100412         ELSE;
502700100412             IMK22cmm  = %dec(v02com:7:0);
502800100412         ENDIF;
502900091210         IMK22dad  = atcdad;
503000091210         IMK22hda  = atchda;
503100091210         IMK22oii  = atcoii;
503200091210         IMK22ofi  = atcofi;
503300091210         IMK22tat  = atctat;
503400091210         IMK22atn  = atcatn;
503500091210         IMK22atnp = atcatnp;
503600091210
503700091210         trmk22r (kpjba : TRMK22ds);
503800091210
503900091210         //?Se ritorna errore lo visualizzo e faccio rollback
504000091210         IF  OMK22err <> *blanks;
504100091210           ErrMessage  = *on;
504200091210           ErrGenerico = *on;
504300091210           IF  Prox_Appunta;
504400091210             W03msg = OMK22msg;
504500091210           ELSE;
504600091210             V1dmsg = OMK22msg;
504700091210           ENDIF;
504800091210           IF  IMK19cmt = 'S';
504900091210             rolbk;
505000091210           ENDIF;
505100091210           leavesr;
505200091210         ENDIF;
505300091210
505400091210         //?Se F12 da gestione affiancamento rollback
505500091210         IF  IMK19cmt = 'S' and OMK22F12 = 'S';
505600091210           rolbk;
505700091210         ENDIF;
505800091210
505900100622         //?se non ci sono errori e provengo dalla videata W03 e dopo
506000100622         //?questo programma devo richiamare la trattativa non committo
506100100622         //?perchè se F12 dalla trattativa devo poter fare il rolbk
506200100622         IF  prox_appunta  and w03aff = 'S' and
506300100622             §ccoapt = 'S' and $new_trattati;
506400100622           leavesr;
506500100622         ENDIF;
506600091210         //?Se non ci sono errori e non è stato premuto F12
506700091210         //?confermo tutto
506800091210         IF  OMK22err = *blanks and OMK22f12 = *blanks;
506900100721         // dopo una lunga e attenta osservazione di alcuni comportamenti del programma
507000100721         // abbiamo verificato che questo commit creava dei problemi tolto xchè ci pensa
507100100721         /// il programma + avanti a committare il tutto
507200100721         //  commit;
507300091210         ENDIF;
507400091210
507500091210       ENDSR;
507600091113
507700100329       //--------------------------------------------------------------
507800100329       //?Richiamo programma invio mail conferma appuntamento TRMK23R
507900100329       //--------------------------------------------------------------
508000100329       BEGSR Ric_Trmk23;
508100100507
508200100329
508300100915         If  Imk23rei <> 'I';
508400100915             Imk23f12 = 'S'    ;
508500100915             Imk23cma = atcco3 ;
508600100915             Imk23cmm = atccmm ;
508700100915             Imk23dta = atcdad ;
508800100915             Imk23ora = %dec(%subst(%editc(atchda:'X'):1:4):4:0);
508900100329        // recupero i dati del responsabile trasporti
509000100915             exsr respd02 ;
509100100915             Imk23rst = vh2rtr ;
509200100511        // imposto indirizzo mail immesso nella window
509300100915             Imk23emr = v02mrt ;
509400100915         Endif ;
509500100915
509600100915         trmk23r1 ( kpjba : TRMK23ds1 );
509700100915
509800100915         ATCco3 = Imk23cma;
509900100915
510000100915
510100100329
510200100329       ENDSR;
510300110207
510400110207       //--------------------------------------------------------------
510500110207       //?Richiamo il programma di gestione blocco cliente TRMK25R
510600110207       //--------------------------------------------------------------
510700110207       BEGSR Ric_Trmk25;
510800110207
510900110207         clear TRMK25ds;
511000110207         IMK25fcmt = '1';
511100110207         IMK25cmt = 'N';
511200110207         IMK25fmt = '4';
511300110207         IMK25fle = 'I';
511400110207         IMK25cmm  = %dec(v02com:7:0);
511500110209         IMK25cpo  = imk19cpo ;
511600110208         IMK25cac  = v02cae ;
511700110207
511800110207         trmk25r (kpjba : TRMK25ds);
511900110207
512000110207         //?Se ritorna errore lo visualizzo e faccio rollback
512100110207         IF  OMK25err <> *blanks;
512200110207           ErrMessage  = *on;
512300110207           ErrGenerico = *on;
512400110207           V1dmsg = OMK25msg;
512500110207           IF  IMK19cmt = 'S';
512600110207             rolbk;
512700110207           ENDIF;
512800110207           leavesr;
512900110207         ENDIF;
513000110207
513100110207         //?Se F12 da gestione affiancamento rollback
513200110207         IF  IMK19cmt = 'S' and OMK25F12 = 'S';
513300110207           rolbk;
513400110207         ENDIF;
513500110207
513600110207       ENDSR;
513700110207
513800100329
513900100329
514000100329       //--------------------------------------------------------------
514100100329       //?Gestione Richiesta Info Commerciali solo per alcune causali
514200100329       //?e solo in particolari situazioni
514300100329       //--------------------------------------------------------------
514400100329       BEGSR Sr_infocomm;
514500100329
514600100329       // verifico la causale esito attività è tra quelle che richiedono l'inserimento
514700100329       // delle info commerciali
514800120313
514900120313       If info_cac = *blanks ;
515000120313          leavesr ;
515100120313       endif ;
515200100329
515300100329       pos = %lookup(info_cac : $cac) ;
515400100329       select ;
515500100329       // se causale non presente si va a fine
515600100329       when  pos = 0  ;
515700100329       // se presente trattativa e non vuole info vado a fine
515800100329       when  $TRA(pos) = 'NO' and info_nrv > 0 ;
515900100329       // se richiede info solo se aperta trattativa e non è stata aperta vado a fine
516000100329       when $APT(pos) = 'SI' and info_nra = 0 ;
516100100329
516200100329       other ;
516300100329           exsr F14D02 ;
516400100329       endsl ;
516500100329
516600100329       Endsr ;
516700100329
516800090610       //--------------------------------------------------------------
516900090610       //?Operazioni iniziali.
517000090610       //--------------------------------------------------------------
517100090610       BEGSR RoutInz;
517200090610
517300090610         // Impostazione campi "fissi"
517400090610         T01pgm = SDSpgm;
517500090610
517600090610         // Reperimento dati job
517700090610         exsr DatiJob;
517800090610
517900110805         // Controllo se utente di sede
518000100210         // Controllo se utente autorizzato alla funzione
518100100210         reset TNTAA1DS;
518200100210         ITAA1caut = '§utegtc';
518300100210         kpjbu     = TNTAA1DS;
518400100210         tntaa1c (kpjba);
518500100210         TNTAA1DS = kpjbu;
518600100210
518700131127         // ATTENZIONE FLAG DI RITORNO UTILIZZATO ALL'INTERNO DEL PGM
518800100329
518900131127         IF  OTAA1fabi = 'N' and kcdaz <> 'MK01' ;
519000090610           $ErrGrave   = *on;
519100090610           errMessage  = *on;
519200090610           errGenerico = *on;
519300090610           V1Dmsg = $Msg(01);
519400090610           leavesr;
519500090610         endif;
519600090610
519700090610         // verifico se abilitare il commit oppure no
519800090610
519900090610            flgcom = (imk19cmt = 'S') ;
520000090610
520100090610            open tiatc01l ;
520200091119            open ticpn00f ;
520300090610
520400100721
520500100721             // verifico se già stato eseguito in fase di modifica o gestione
520600100721            chain(n) (imk19tat:imk19atn:imk19atnp) tiatc01l ;
520700100721            If  (imk19fle = 'G' or imk19fle = '2') and atcdco > 0 ;
520800100721                omk19err = 'E' ;
520900100721                $fine  = *on;
521000100721                omk19msg = 'Attività eseguita da altro utente' ;
521100100721                leavesr;
521200100721            endif;
521300111223
521400111223         //?Data del giorno
521500111223         wOggi = %dec(%date());
521600100721
521700090610
521800090610       ENDSR;
521900090610
522000090610       //--------------------------------------------------------------
522100090610       //?Reperimento Dati del job (Utente/Operativi).
522200090610       //--------------------------------------------------------------
522300090610       BEGSR DatiJob;
522400090610
522500090610         in(E) §AzUte;
522600090610         if NOT %error;
522700090610           in(E) §DatiUte;
522800090610         endif;
522900090610         if %error or RSut = *blanks;
523000090610           clear TIBS34ds;
523100090610           tibs34r(tibs34ds);
523200090610           in §AzUte;
523300090610           in §DatiUte;
523400090610         endif;
523500090610
523600090610       ENDSR;
523700090610
523800090610       //--------------------------------------------------------------
523900090610       //?Operazioni finali.
524000090610       //--------------------------------------------------------------
524100090610       BEGSR RoutEnd;
524200090610
524300090610
524400100422         // comitto se richiesto e se non ci sono errori
524500100422            if imk19cmt = 'S' and omk19err = ' ';
524600100422               commit ;
524700100422            endif ;
524800100422         // rolbk  se richiesto e se  ci sono errori
524900100422            if imk19cmt = 'S' and omk19err <> ' ';
525000100422               rolbk  ;
525100100422            endif ;
525200130531
525300130531         //?Chiudo il TRMK20R
525400130531            clear TRMK20DS;
525500130531            IMK20tla = 'C';
525600130531            trmk20r ( kpjba : trmk20ds );
525700130531
525800090610         *inLR = *on;
525900090610         return;
526000090610
526100090610       ENDSR;
526200090610
526300090610      /end-free
526400090610       //--------------------------------------------------------------
526500090610       //?Schiere a tempo di compilazione.
526600090610       //--------------------------------------------------------------
526700090610
526800090610** - $MSG -------------------------------------------------------------------*
526900100210Utente non abilitato alla funzione                                             01
527000100210Codice commerciale errato o non in gestione all'utente                         02
527100090611Data errata                                                                    03
527200110307Utente non autorizzato al cliente. Riassegnare l'attività a comm. abilitato.   04
527300090611Ora errata                                                                     05
527400100310Attività non possibile per Commerciale Interno.                                06
527500100721Attività già eseguita da altro utente uscire dal programma                     07
527600100308Obbligatorio inserire la causale attività                                      08
527700100920.....................                                                          09
527800100921.......................                                                        10
527900100920.....................                                                          11
528000121018.......................                                                        12
528100090702Ora inizio impegno incongruente con ora fine impegno                           13
528200090702Causale non utilizzabile                                                       14
528300090804Ore inizio e fine impegno incongruenti con ora appuntamento                    15
528400100210..........                                                                     16
528500091026Inserire ore inizio e fine impegno                                             17
528600091027Non è stato selezionato nessun cliente                                         18
528700091112Non si può utilizzare questa causale perchè trattativa già esistente           19
528800091112Non si può utilizzare questa causale perchè esiste già una trattativa          20
528900100114Immettere se Appuntamento effettuato.                                          21
529000100224Non si può annullare unica attività aperta della trattativa                    22
529100100225Causale non utilizzabile per questo tipo attività                              23
529200100225Causale non utilizzabile in quanto attività legata a trattativa con offerte    24
529300110502Causale non utilizzabile su potenziali                                         25
529400110502............   LIBERO                                                          26
529500100308Causale non utilizzabile legata ad una trattativa                              27
529600100308Presente trattativa già esitata in modo positivo. Causale inutilizzabile       28
529700100308Obbligatorio inserire delle note                                               29
529800100520Causale utilizzabile SOLO in presenza di trattativa                            30
529900100322Causale non utilizzabile in presenza di trattativa senza offerte               31
530000100329Obbligatorio inserire il Nome e indirizzo e-mail Resp.Trasporti                32
530100100421In caso di esecuzione di un'attività Post vendita utilizzare la causale 42     33
530200100504Fil. del codice commerciale incongruente con la fil. del cliente               34
530300100505Causale non utilizzabile per trattativa di tipo                                35
530400110321La causale xx non è utilizzabile per l'esecuzione di un'attività xx            36
530500100329** - $CAC -------------------------------------------------------------------*
530600120124242526284041719172                       Causali info comm
530700100329** - $TRA -------------------------------------------------------------------*
530800120104NONONONO  NO  NO                         NO = no legate a trattative
530900100329** - $APT -------------------------------------------------------------------*
531000120104              SI                         SI = solo se in apertura trattativa
