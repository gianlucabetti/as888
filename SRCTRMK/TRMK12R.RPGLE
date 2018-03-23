000100110516      //--------------------------------------------------------------
000200110516      //?TRMK12R - STAMPA ANAGRAFICA CLIENTI POTENZIALI
000300110516      //--------------------------------------------------------------
000400110516
000500110516     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600110516     h dftactgrp(*no) actgrp(*caller)
000700110516
000800110516      //---------------------------------------------------------------
000900110516      // Dichiarazione file.
001000110516      //---------------------------------------------------------------
001100110516
001200110516      // - Organigramma filiale/aziendale
001300110516     fAZORG01L  if   e           k disk
001400130808
001500130808      // -?Anagrafica Commerciali?
001600130808     fAZCMM01L  if   e           k disk
001700110516
001800110516      // - Tabelle
001900110516     fTABEL00F  if   e           k disk
002000110516     fTNTBE01L  if   e           k disk
002100110516
002200110516      // - Video Interrogazione anagrafica clienti potenziali
002300110516     fTRMK12D   cf   e             workstn
002400110516     f                                     indds(IndDspF)
002500110516     f                                     infds(InfDspF)
002600110516
002700110516      //---------------------------------------------------------------
002800110516      //?Definizione costanti.
002900110516      //---------------------------------------------------------------
003000110516
003100110516      // - Tasti funzionali a video
003200110516     d c_F01           c                   const(x'31')
003300110516     d c_F02           c                   const(x'32')
003400110516     d c_F03           c                   const(x'33')
003500110516     d c_F04           c                   const(x'34')
003600110516     d c_F05           c                   const(x'35')
003700110516     d c_F06           c                   const(x'36')
003800110516     d c_F07           c                   const(x'37')
003900110516     d c_F08           c                   const(x'38')
004000110516     d c_F09           c                   const(x'39')
004100110516     d c_F10           c                   const(x'3A')
004200110516     d c_F11           c                   const(x'3B')
004300110516     d c_F12           c                   const(x'3C')
004400110516     d c_F13           c                   const(x'B1')
004500110516     d c_F14           c                   const(x'B2')
004600110516     d c_F15           c                   const(x'B3')
004700110516     d c_F16           c                   const(x'B4')
004800110516     d c_F17           c                   const(x'B5')
004900110516     d c_F18           c                   const(x'B6')
005000110516     d c_F19           c                   const(x'B7')
005100110516     d c_F20           c                   const(x'B8')
005200110516     d c_F21           c                   const(x'B9')
005300110516     d c_F22           c                   const(x'BA')
005400110516     d c_F23           c                   const(x'BB')
005500110516     d c_F24           c                   const(x'BC')
005600110516     d c_Enter         c                   const(x'F1')
005700110516     d c_RollDown      c                   const(x'F4')
005800110516     d c_RollUp        c                   const(x'F5')
005900110517
006000110517     d Digits          c                   const('0123456789')
006100110516
006200110516      //---------------------------------------------------------------
006300110516      //?Definizione schiere.
006400110516      //---------------------------------------------------------------
006500110516
006600110516      // - Messaggi di errore
006700140923     d $Msg            s             78    dim(40) ctdata perrcd(1)
006800110516
006900110516      // - Importanza clienti da tabella IC
007000110516     d $IC             s              1    dim(10)
007100110516
007200110518      // - Attività
007300110518     d $CCO            s              2    dim(99)
007400110516
007500110516      // - Concorrenti possibili da tabella IFS
007600110523     d $CON            s              4    dim(30)
007700110523
007800110523      // - Categoria potenziale da tabella CPO
007900110523     d $CPO            s              1    dim(10)
008000110523     d $CPOS           s              1    dim(5)
008100110520
008200110520      // - Info comm. Ripartizione spesa --> "3T"
008300110520     d $IFO            s              3    dim(30)
008400110523
008500110523      // - Operatorie logistica tabella IFS
008600110523     d $LOG            s              4    dim(30)
008700110523
008800110523      // - Motivo non affidamento possibili da tabella IFS
008900110523     d $MOI            s              4    dim(30)
009000110523     d $DMOI           s             20    dim(30)
009100140923
009200140923      // - Paesi estero possibili da tabella IFS
009300140923     d $PAESI          s              4    dim(30)
009400140923     d $PAESID         s             25    dim(30)
009500110516
009600110516      //---------------------------------------------------------------
009700110516      //?Definizione aree dati.
009800110516      //---------------------------------------------------------------
009900110516
010000110516      // - Dati utente
010100110516     d §AzUte        e ds                  extname(AZUTE00F)
010200110516     d                                     dtaara
010300110516     d §DatiUte      e ds                  extname(dDatiUte)
010400110516     d                                     dtaara
010500110516
010600110516      //---------------------------------------------------------------
010700110516      //?Definizione strutture dati.
010800110516      //---------------------------------------------------------------
010900110516
011000110516      // - Status
011100110516     d Psds           sds
011200110516     d   SDSpgm          *proc
011300110516
011400110516      // - InfDS
011500110516     d InfDspF         ds
011600110516     d  dsp_aid              369    369a
011700110516
011800110516      // - Indicatori su DspF
011900110516     d IndDspF         ds
012000110516        // - Abilitazione tasti funzionali
012100110516     d  F08Attivo                     1n   overlay(IndDspF : 08)
012200110516        // - Indicatori di errore
012300110516     d  ErrMessage                    1n   overlay(IndDspF : 28)
012400110517     d  PosCurPrv                     1n   overlay(IndDspF : 50)
012500110520     d  PosCurSpta                    1n   overlay(IndDspF : 50)
012600110525     d  PosCurOspt                    1n   overlay(IndDspF : 50)
012700110517     d  PosCurCap                     1n   overlay(IndDspF : 51)
012800110520     d  PosCurRip1                    1n   overlay(IndDspF : 51)
012900110525     d  PosCurOrag                    1n   overlay(IndDspF : 51)
013000110518     d  PosCurNaz                     1n   overlay(IndDspF : 52)
013100110520     d  PosCurRip2                    1n   overlay(IndDspF : 52)
013200110525     d  PosCurOcmmu                   1n   overlay(IndDspF : 52)
013300110518     d  PosCurCmmu                    1n   overlay(IndDspF : 53)
013400110520     d  PosCurRip3                    1n   overlay(IndDspF : 53)
013500110518     d  PosCurCar                     1n   overlay(IndDspF : 54)
013600110520     d  PosCurRipda                   1n   overlay(IndDspF : 54)
013700110525     d  PosCurOcmm                    1n   overlay(IndDspF : 54)
013800110518     d  PosCurFil                     1n   overlay(IndDspF : 55)
013900110520     d  PosCurRipa                    1n   overlay(IndDspF : 55)
014000110525     d  PosCurOcar                    1n   overlay(IndDspF : 55)
014100110518     d  PosCurSct                     1n   overlay(IndDspF : 56)
014200140923     d  PosCurSptp                    1n   overlay(IndDspF : 56)
014300110520     d  PosCurPesa                    1n   overlay(IndDspF : 56)
014400110518     d  PosCurCic1                    1n   overlay(IndDspF : 57)
014500140923     d  PosCurNrfa                    1n   overlay(IndDspF : 57)
014600110520     d  PosCurPspa                    1n   overlay(IndDspF : 57)
014700110518     d  PosCurCic2                    1n   overlay(IndDspF : 58)
014800140923     d  PosCurNroa                    1n   overlay(IndDspF : 58)
014900110520     d  PosCurBarda                   1n   overlay(IndDspF : 58)
015000110518     d  PosCurCic3                    1n   overlay(IndDspF : 59)
015100140923     d  PosCurNera                    1n   overlay(IndDspF : 59)
015200110520     d  PosCurBara                    1n   overlay(IndDspF : 59)
015300110518     d  PosCurCic4                    1n   overlay(IndDspF : 60)
015400140923     d  PosCurNaea                    1n   overlay(IndDspF : 60)
015500110523     d  PosCurConc1                   1n   overlay(IndDspF : 60)
015600110518     d  PosCurCic5                    1n   overlay(IndDspF : 61)
015700140923     d  PosCurNtra                    1n   overlay(IndDspF : 61)
015800110523     d  PosCurConc2                   1n   overlay(IndDspF : 61)
015900110518     d  PosCurFls1                    1n   overlay(IndDspF : 62)
016000140923     d  PosCurPaesi1                  1n   overlay(IndDspF : 62)
016100110523     d  PosCurConc3                   1n   overlay(IndDspF : 62)
016200110518     d  PosCurFls2                    1n   overlay(IndDspF : 63)
016300140923     d  PosCurPaesi2                  1n   overlay(IndDspF : 63)
016400110523     d  PosCurConc4                   1n   overlay(IndDspF : 63)
016500110518     d  PosCurFls3                    1n   overlay(IndDspF : 64)
016600140923     d  PosCurPaesi3                  1n   overlay(IndDspF : 64)
016700110523     d  PosCurConc5                   1n   overlay(IndDspF : 64)
016800110518     d  PosCurFls4                    1n   overlay(IndDspF : 65)
016900140923     d  PosCurPaesi4                  1n   overlay(IndDspF : 65)
017000110523     d  PosCurConda                   1n   overlay(IndDspF : 65)
017100110523     d  PosCurFls5                    1n   overlay(IndDspF : 66)
017200140923     d  PosCurPaesi5                  1n   overlay(IndDspF : 66)
017300110523     d  PosCurCona                    1n   overlay(IndDspF : 66)
017400110523     d  PosCurCmm                     1n   overlay(IndDspF : 67)
017500110523     d  PosCurMoi1                    1n   overlay(IndDspF : 67)
017600110523     d  PosCurCco1                    1n   overlay(IndDspF : 68)
017700110523     d  PosCurMoi2                    1n   overlay(IndDspF : 68)
017800110523     d  PosCurCco2                    1n   overlay(IndDspF : 69)
017900110523     d  PosCurLogp                    1n   overlay(IndDspF : 69)
018000110523     d  PosCurCco3                    1n   overlay(IndDspF : 70)
018100110524     d  PosCurNifo                    1n   overlay(IndDspF : 70)
018200110523     d  PosCurCco4                    1n   overlay(IndDspF : 71)
018300110524     d  PosCurIfoal                   1n   overlay(IndDspF : 71)
018400110523     d  PosCurCco5                    1n   overlay(IndDspF : 72)
018500110523     d  PosCurDadd                    1n   overlay(IndDspF : 73)
018600110523     d  PosCurDada                    1n   overlay(IndDspF : 74)
018700140923     d  PosCurPexpda                  1n   overlay(IndDspF : 75)
018800140923     d  PosCurMqlda                   1n   overlay(IndDspF : 75)
018900140923     d  PosCurPexpa                   1n   overlay(IndDspF : 76)
019000140923     d  PosCurMqla                    1n   overlay(IndDspF : 76)
019100140923     d  PosCurAnnoda                  1n   overlay(IndDspF : 77)
019200140923     d  PosCurPplda                   1n   overlay(IndDspF : 77)
019300140923     d  PosCurAnnoa                   1n   overlay(IndDspF : 78)
019400140923     d  PosCurPpla                    1n   overlay(IndDspF : 78)
019500110516     d  ErrGenerico                   1n   overlay(IndDspF : 99)
019600110516
019700110516     d WindDspF        ds                  inz
019800110516     d   WindDspFa             1     49    inz(*zero)
019900110516     d   WindDspFb            50     99    inz(*zero)
020000110516
020100110516      // Campo unico per Importanza clienti
020200110516     d V01cic          ds                  inz
020300110516     d   V01cic1                           inz
020400110516     d   V01cic2                           inz
020500110516     d   V01cic3                           inz
020600110516     d   V01cic4                           inz
020700110516     d   V01cic5                           inz
020800110516
020900110516      // Campo unico per Categoria potenziale
021000110516     d V01fls          ds                  inz
021100110516     d   V01fls1                           inz
021200110516     d   V01fls2                           inz
021300110516     d   V01fls3                           inz
021400110516     d   V01fls4                           inz
021500110518     d   V01fls5                           inz
021600110516
021700110516      // Campo unico per Prossima Attività
021800110516     d V01cco          ds                  inz
021900110516     d   V01cco1                           inz
022000110516     d   V01cco2                           inz
022100110516     d   V01cco3                           inz
022200110516     d   V01cco4                           inz
022300110516     d   V01cco5                           inz
022400110520
022500110520      // Campo unico per Ripartizione spesa
022600110520     d V02rip          ds                  inz
022700110520     d   V02rip1                           inz
022800110520     d   V02rip2                           inz
022900110520     d   V02rip3                           inz
023000140923
023100140923      // Campo unico per Paesi Estero
023200140923     d V02paesi        ds                  inz
023300140923     d   V02paesi1                         inz
023400140923     d   V02paesi2                         inz
023500140923     d   V02paesi3                         inz
023600140923     d   V02paesi4                         inz
023700140923     d   V02paesi5                         inz
023800110516
023900110516      // Campo unico per Concorrenti
024000110516     d V02conc         ds                  inz
024100110516     d   V02conc1                          inz
024200110516     d   V02conc2                          inz
024300110516     d   V02conc3                          inz
024400110516     d   V02conc4                          inz
024500110516     d   V02conc5                          inz
024600110524
024700110524      // Campo unico per Motivo non affidamento
024800110524     d V02moi          ds                  inz
024900110524     d   V02moi1                           inz
025000110524     d   V02moi2                           inz
025100110519
025200110519     d wlbdat          ds                  inz
025300110519     d  g02dat                 1      8  0
025400110519     d  g02inv                 9     16  0
025500110519     d  g02err                17     17
025600110519     d  g02tgi                18     22  0
025700110516
025800110516      // - Parametri ricevuti
025900110516     d KPJBA         e ds
026000110516     d TRMK13ds      e ds                  inz
026100110516
026200110516      // - Reperimento dati utente
026300110516     d TIBS34ds      e ds
026400110524     d dUTE01        e ds
026500110516     d TRUL31DS      e ds
026600110516
026700110516      // - controllo abilitazioni
026800110516     d TNTAA1DS      e ds                  inz
026900110520
027000110520      // - Ricerca ripartizione spesa
027100110520     d TNTB70DS      e ds                  inz
027200110516
027300110516      // - Ricerca tabella CPO
027400110520     d TNTB87DS      e ds                  inz
027500140923
027600140923      // - Parametri per gestione parametri batch
027700140923     d TRUL01ds      e ds                  inz
027800140923     d   UL01opz     e                     inz(*on)
027900140923     d   UL01err     e                     inz(*off)
028000110523
028100110523      // - Tabella CPO = Categoria potenziale
028200110523     d dCPO          e ds                  inz
028300110520
028400110520      // Ripartizione spesa
028500110520     d dIFO          e ds
028600110523
028700110523      // Tabella sottocategorie IFS
028800110523     d dIFS          e ds
028900110516
029000110516      // - Tabella 05 = Codici Area
029100110516     d ds05          e ds                  inz
029200110516
029300110516      // - Tabella 1L = Categorie Merceologiche
029400110516     d ds1L          e ds                  inz
029500110516
029600110516      // - Controllo congruenza CAP e provincia
029700110516     d FNLV13ds      e ds
029800110516     d   I13af0      e                     inz('S')
029900110516     d   I13cng      e                     inz('S')
030000110516     d TISI95ds      e ds                  inz
030100110516     d   I95tcn      e                     inz('3')
030200110516
030300110516      //---------------------------------------------------------------
030400110516      //?Definizione variabili globali.
030500110516      //---------------------------------------------------------------
030600110516
030700110516      // - Flags booleani
030800110516     d $Fine           s               n   inz(*off)
030900110516     d $InzD01         s               n   inz(*on)
031000110516     d $InzD02         s               n   inz(*on)
031100110516     d $InzD03         s               n   inz(*on)
031200140923     d $InzD04         s               n   inz(*on)
031300110516     d $ErrGrave       s               n   inz(*off)
031400110516
031500110516      // - Indici di schiera
031600110516     d xx              s              3  0 inz
031700110516     d yy              s              3  0 inz
031800110516
031900110516      // - Campi associati al video
032000110516     d $Video          s              2    inz('D1')
032100110516
032200110519      // - Campi di comodo per leggere i file tabelle
032300110516     d C_tab           s                   like(TBEcod)
032400110516     d C_ke1           s                   like(TBEke1)
032500110516
032600110516     d idTabella       s              2
032700110516     d Ordinamento     s              1
032800110516     d idElemento      s              8
032900110516     d TastoUscita     s              1
033000110519
033100110519      // - Campi di comodo data
033200110524     d wdadd           s              8  0 inz
033300110519     d wdada           s              8  0 inz
033400110524     d wifoal          s              8  0 inz
033500110523
033600110523      // - Campi di comodo
033700110523     d wConta          s              2  0 inz
033800110516
033900110516      //---------------------------------------------------------------
034000110516      //?Definizione procedure usate.
034100110516      //---------------------------------------------------------------
034200110524
034300110524      // - Sottomissione lavoro batch
034400110524     d bch10           pr                  extpgm('BCH10')
034500110524     d  kpjba                              likeds(KPJBA)
034600110516
034700110516      // - Controllo cap/località
034800110516     d fnlv13r         pr                  extpgm('FNLV13R')
034900110516     d  kpjba                              likeds(kpjba)    const
035000110516     d  fnlv13ds                           likeds(FNLV13ds)
035100110516     d  tisi95ds                           likeds(TISI95ds)
035200110520
035300110520      // - Interrogazione ripartizione spesa
035400110520     d TNTB70R         pr                  extpgm('TNTB70R')
035500110520     d  kpjba                              likeds(kpjba)
035600110520     d  tntb70ds                           likeds(TNTB70ds)
035700110516
035800110520      // - Interrogazione tabella CPO
035900110516     d TNTB87R         pr                  extpgm('TNTB87R')
036000110516     d  tntb87ds                           likeds(TNTB87ds)
036100140923
036200140923      // - Gestione parametri di lancio
036300140923     d TRUL01R         pr                  extpgm('TRUL01R')
036400140923     d  trul01ds                           likeds(TRUL01DS)
036500110524
036600110524      // - Elaborazione
036700110524     d trmk13r         pr                  extpgm('TRMK13R')
036800110524     d  kpjba                              likeds(kpjba)    const
036900110516
037000110516      //---------------------------------------------------------------
037100110517      //?Prototipi.
037200110516      //---------------------------------------------------------------
037300110516      /copy gaitrasrc/srcprotopr,tibs34r
037400130808      /copy gaitrasrc/srcprotoPI,TRMK43R_1
037500130808      /copy gaitrasrc/srcprotoPR,TRMK43R
037600110516      /copy gaitrasrc/srcprotopr,tntaa1c
037700110516      /copy gaitrasrc/srcprotopr,trul19r
037800110516      /copy gaitrasrc/srcprotopr,trul31r
037900110519      /copy gaitrasrc/srcprotopr,xsrda8
038000110516
038100110516      //---------------------------------------------------------------
038200110516      //?Definizione key-list.
038300110516      //---------------------------------------------------------------
038400110517
038500110517       // - File TABEL00F
038600110517     d k03tabel      e ds                  extname(TABEL00F:*key)
038700110517     d                                     prefix(k_)
038800110517     d                                     inz
038900110516
039000110516      //---------------------------------------------------------------
039100110516      //?M A I N - L I N E
039200110516      //---------------------------------------------------------------
039300110516
039400110516     c     *Entry        plist
039500110516     c                   parm                    KPJBA
039600110516
039700110516      /free
039800110516
039900110516       //?Operazioni iniziali
040000110516       exsr RoutInz;
040100110516
040200110516       //?Gestione video
040300110516       DOW $Fine = *off;
040400110516         select;
040500110516           when $Video = 'D1';
040600110516             exsr GesD01;
040700110516           when $Video = 'D2';
040800110519             exsr GesD02;
040900110516           when $Video = 'D3';
041000110516             exsr GESD03;
041100140923           when $Video = 'D4';
041200140923             exsr GesD04;
041300110516           other;
041400110516             $Fine = *on;
041500110516         endsl;
041600110516       ENDDO;
041700110516
041800110516       //?Operazioni finali
041900110516       exsr RoutEnd;
042000110516
042100110516       //--------------------------------------------------------------
042200110516       //?Operazioni iniziali.
042300110516       //--------------------------------------------------------------
042400110516       BEGSR RoutInz;
042500110516
042600110516       //?Impostazione campi "fissi"
042700110516         T01pgm = SDSpgm;
042800110516
042900110516       //?Reperimento dati job
043000110516         exsr DatiJob;
043100110516
043200110516       //?Controllo se ok l'abilitazione dell'utente
043300110516         reset TNTAA1DS;
043400110516         ITAA1caut = '§utepot';
043500110516         clear kpjbu;
043600110516         kpjbu = TNTAA1DS;
043700110516         tntaa1c (kpjba);
043800110516         TNTAA1DS = kpjbu;
043900110516
044000110524         IF  OTAA1fabi = 'N';
044100110516           $ErrGrave = *on;
044200110524           leavesr;
044300110524         ENDIF;
044400110516
044500110524         clear TRUL31DS;
044600110524         I31abi = OTAA1cabi;
044700110524         I31cdi = DUTdis;
044800110524         I31car = DUTare;
044900110524         I31cpo = DUTpou;
045000110524         callp TRUL31R (kpjba:trul31ds);
045100110524         IF  O31pog <= *zeros;
045200110524           $ErrGrave = *on;
045300110524           leavesr;
045400110524         ENDIF;
045500110524
045600110524         dUTE01 = UTEfaf;
045700110524
045800110524         //?Abilito stampa info commerciale in base al profilo utente
045900110524         F08Attivo = (§UTEpotst = 'S');
046000110516
046100110518         //?Carico sk Codici Importanza
046200110516         clear xx;
046300110517         TBLkut = 1;
046400110516         TBLcod = 'IC';
046500110516         setll (TBLkut:TBLcod) TABEL00F;
046600110516         reade (TBLkut:TBLcod) TABEL00F;
046700110516
046800110516         DOW not %eof(TABEL00F);
046900110516           IF  TBLflg = *blanks;
047000110516             xx += 1;
047100110520             $IC(xx) = %subst(TBLkey:1:1);
047200110516           ENDIF;
047300110516           reade (TBLkut:TBLcod) TABEL00F;
047400110516         ENDDO;
047500110518
047600110518         //?Carico sk Attività
047700110518         clear xx;
047800110518         clear yy;
047900110518         C_Tab = 'CCO';
048000110518         setll (C_tab) TNTBE01L;
048100110518         reade (C_tab) TNTBE01L;
048200110518
048300110518         DOW not %eof(TNTBE01L);
048400110518           IF  TBEatb = *blanks;
048500110518             xx += 1;
048600110524             $CCO(xx) = %subst(TBEke1:1:3);
048700110518           ENDIF;
048800110518           reade (C_tab) TNTBE01L;
048900110518         ENDDO;
049000110516
049100110516         //?Carico sk Categoria potenziali
049200110516         clear xx;
049300110516         clear yy;
049400110516         C_Tab = 'CPO';
049500110516         setll (C_tab) TNTBE01L;
049600110516         reade (C_tab) TNTBE01L;
049700110516
049800110516         DOW not %eof(TNTBE01L);
049900110516           IF  TBEatb = *blanks;
050000110523             dCPO = TBEuni;
050100110516             xx += 1;
050200110520             $CPO(xx) = %subst(TBEke1:1:1);
050300110523             IF  §CPOstp = 'S';
050400110523               yy += 1;
050500110523               $CPOS(yy) = $CPO(xx);
050600110523             ENDIF;
050700110516           ENDIF;
050800110516           reade (C_tab) TNTBE01L;
050900110516         ENDDO;
051000110516
051100110516         //?Carico sk Concorrenti
051200110516         clear xx;
051300110516         C_Tab = 'IFS';
051400110516         C_ke1 = '10';
051500110516         setll (C_tab:C_ke1) TNTBE01L;
051600110516         reade (C_tab:C_ke1) TNTBE01L;
051700110516
051800110516         DOW not %eof(TNTBE01L);
051900110516           IF  TBEatb = *blanks;
052000110516             xx += 1;
052100110523             $CON(xx) = %subst(TBEke2:1:4);
052200110516           ENDIF;
052300110516           reade (C_tab:C_ke1) TNTBE01L;
052400110516         ENDDO;
052500140923
052600140923         //?Carico sk Paesi Estero
052700140923         clear xx;
052800140923         C_Tab = 'IFS';
052900140923         C_ke1 = '50';
053000140923         setll (C_tab:C_ke1) TNTBE01L;
053100140923         reade (C_tab:C_ke1) TNTBE01L;
053200140923
053300140923         DOW not %eof(TNTBE01L);
053400140923           IF  TBEatb = *blanks;
053500140923             xx += 1;
053600140923             $PAESI(xx)  = %subst(TBEke2:1:4);
053700140923             $PAESID(xx) = %subst(TBEuni:1:25);
053800140923           ENDIF;
053900140923           reade (C_tab:C_ke1) TNTBE01L;
054000140923         ENDDO;
054100110523
054200110523         //?Carico sk Operatori Logistica
054300110523         clear xx;
054400110523         C_Tab = 'IFS';
054500110523         C_ke1 = '40';
054600110523         setll (C_tab:C_ke1) TNTBE01L;
054700110523         reade (C_tab:C_ke1) TNTBE01L;
054800110523
054900110523         DOW not %eof(TNTBE01L);
055000110523           IF  TBEatb = *blanks;
055100110523             xx += 1;
055200110523             $LOG(xx) = %subst(TBEke2:1:4);
055300110523           ENDIF;
055400110523           reade (C_tab:C_ke1) TNTBE01L;
055500110523         ENDDO;
055600110523
055700110523         //?Carico sk Motivo non affidamento
055800110523         clear xx;
055900110523         C_Tab = 'IFS';
056000110523         C_ke1 = 'MOI';
056100110523         setll (C_tab:C_ke1) TNTBE01L;
056200110523         reade (C_tab:C_ke1) TNTBE01L;
056300110523
056400110523         DOW not %eof(TNTBE01L);
056500110523           IF  TBEatb = *blanks;
056600110523             xx += 1;
056700110523             $MOI(xx) = %subst(TBEke2:1:4);
056800110523             dIFS = TBEuni;
056900110523             $DMOI(xx) = §IFSdes;
057000110523           ENDIF;
057100110523           reade (C_tab:C_ke1) TNTBE01L;
057200110523         ENDDO;
057300110520
057400110520         //?Carico sk Ripartizione spesa (tipo 3T)
057500110520         clear xx;
057600110520         C_Tab = 'IFO';
057700110520         setll (C_tab) TNTBE01L;
057800110520         reade (C_tab) TNTBE01L;
057900110520
058000110520         DOW not %eof(TNTBE01L);
058100110520           IF  TBEatb = *blanks;
058200110520             dIFO = TBEuni;
058300110520             IF  §IFOifg = '3T';
058400110520               xx += 1;
058500110520               $IFO(xx) = %subst(TBEke1:1:3);
058600110520             ENDIF;
058700110520           ENDIF;
058800110520           reade (C_tab) TNTBE01L;
058900110520         ENDDO;
059000110516
059100110516       ENDSR;
059200080206
059300080206       //--------------------------------------------------------------
059400110517       //?Gestione videata D01.
059500080206       //--------------------------------------------------------------
059600080206       BEGSR GesD01;
059700080206
059800110517       //?Inizializzazione videata
059900110517         IF  $InzD01;
060000080206           exsr InzD01;
060100080206           $InzD01 = *off;
060200110517         ENDIF;
060300080206
060400110517       //?Emissione testata
060500110517         write MK12T01;
060600090702
060700110517       //?Se utente non abilitato messaggio ed esco dal pgm
060800110517         IF  $ErrGrave;
060900110517           ErrMessage  = *on;
061000110517           ErrGenerico = *on;
061100110517           V01msg = $Msg(01);
061200110517         ENDIF;
061300080206
061400110517       //?Emissione videata
061500110517         exfmt MK12D01;
061600110517         ErrMessage   = *off;
061700110517         ErrGenerico  = *off;
061800110517         clear V01msg;
061900080207
062000110517         SELECT;
062100110517
062200110517       //?Errore grave qualsiasi tasto venga premuto esce dal pgm
062300110517           WHEN  $ErrGrave;
062400080206             $Fine  = *on;
062500110308
062600110517       //?F3=Fine
062700110517           WHEN  dsp_aid = c_F03;
062800080206             exsr F03D01;
062900110308
063000110517       //?F06=Parametri di stampa
063100110517           WHEN  dsp_aid = c_F06;
063200110524             exsr CtrD01;
063300110524             IF  ErrGenerico;
063400110524               leavesr;
063500110524             ENDIF;
063600110517             exsr F06D01;
063700110308
063800110517       //?F08=Selezione Info Commerciali
063900110517           WHEN  dsp_aid = c_F08;
064000110524             exsr CtrD01;
064100110524             IF  ErrGenerico;
064200110524               leavesr;
064300110524             ENDIF;
064400110517             exsr F08D01;
064500110517
064600110517       //?Enter
064700110517           OTHER;
064800080206             exsr CtrD01;
064900110517             IF  ErrGenerico;
065000080206               leavesr;
065100110517             ENDIF;
065200110308
065300110517         ENDSL;
065400080206
065500080206       ENDSR;
065600110519
065700110519       //--------------------------------------------------------------
065800110519       //?Gestione videata D02.
065900110519       //--------------------------------------------------------------
066000110519       BEGSR GesD02;
066100110519
066200110519       //?Inizializzazione videata
066300110519         IF  $InzD02;
066400110519           exsr InzD02;
066500110519           $InzD02 = *off;
066600110519         ENDIF;
066700110519
066800110519       //?Emissione testata
066900110519         write MK12T01;
067000110519
067100110519       //?Emissione videata
067200110519         exfmt MK12D02;
067300110519         ErrMessage   = *off;
067400110519         ErrGenerico  = *off;
067500110519         clear V02msg;
067600110519
067700110519         SELECT;
067800110519
067900110519       //?F06=Parametri di stampa
068000110519           WHEN  dsp_aid = c_F06;
068100110524             exsr CtrD02;
068200110524             IF  ErrGenerico;
068300110524               leavesr;
068400110524             ENDIF;
068500110519             exsr F06D01;
068600140923
068700140923       //?F08=Selezione Info Commerciali
068800140923           WHEN  dsp_aid = c_F08;
068900140923             exsr CtrD02;
069000140923             IF  ErrGenerico;
069100140923               leavesr;
069200140923             ENDIF;
069300140923             exsr F08D02;
069400110519
069500110519       //?F12=Ritorno
069600110519           WHEN  dsp_aid = c_F12;
069700110519             exsr F12D02;
069800110519
069900110519       //?Enter
070000110519           OTHER;
070100110519             exsr CtrD02;
070200110519             IF  ErrGenerico;
070300110519               leavesr;
070400110519             ENDIF;
070500110519
070600110519         ENDSL;
070700110519
070800110519       ENDSR;
070900110519
071000110519       //--------------------------------------------------------------
071100110519       //?Gestione videata D03.
071200110519       //--------------------------------------------------------------
071300110519       BEGSR GesD03;
071400110519
071500110519       //?Inizializzazione videata
071600110519         IF  $InzD03;
071700110519           exsr InzD03;
071800110519           $InzD03 = *off;
071900110519         ENDIF;
072000110519
072100110519       //?Emissione testata
072200110519         write MK12T01;
072300110519
072400110519       //?Emissione videata
072500110519         exfmt MK12D03;
072600110519         ErrMessage   = *off;
072700110519         ErrGenerico  = *off;
072800110519         clear V03msg;
072900110519
073000110519         SELECT;
073100110519
073200110519       //?F06=Elabora
073300110519           WHEN  dsp_aid = c_F06;
073400110524             exsr CtrD03;
073500110524             IF  ErrGenerico;
073600110524               leavesr;
073700110524             ENDIF;
073800110519             exsr F06D03;
073900140923             IF  ErrGenerico;
074000140923               leavesr;
074100140923             ENDIF;
074200110519
074300110519       //?F12=Ritorno
074400110519           WHEN  dsp_aid = c_F12;
074500110519             exsr F12D03;
074600110519
074700110519       //?Enter
074800110519           OTHER;
074900110524             exsr CtrD03;
075000110519             IF  ErrGenerico;
075100110519               leavesr;
075200110519             ENDIF;
075300110519
075400110519         ENDSL;
075500110519
075600110519       ENDSR;
075700140923
075800140923       //--------------------------------------------------------------
075900140923       //?Gestione videata D04.
076000140923       //--------------------------------------------------------------
076100140923       BEGSR GesD04;
076200140923
076300140923       //?Inizializzazione videata
076400140923         IF  $InzD04;
076500140923           exsr InzD04;
076600140923           $InzD04 = *off;
076700140923         ENDIF;
076800140923
076900140923       //?Emissione testata
077000140923         write MK12T01;
077100140923
077200140923       //?Emissione videata
077300140923         exfmt MK12D04;
077400140923         ErrMessage   = *off;
077500140923         ErrGenerico  = *off;
077600140923         clear V02msg;
077700140923
077800140923         SELECT;
077900140923
078000140923       //?F06=Parametri di stampa
078100140923           WHEN  dsp_aid = c_F06;
078200140923             exsr CtrD04;
078300140923             IF  ErrGenerico;
078400140923               leavesr;
078500140923             ENDIF;
078600140923             exsr F06D01;
078700140923
078800140923       //?F12=Ritorno
078900140923           WHEN  dsp_aid = c_F12;
079000140923             exsr F12D04;
079100140923
079200140923       //?Enter
079300140923           OTHER;
079400140923             exsr CtrD04;
079500140923             IF  ErrGenerico;
079600140923               leavesr;
079700140923             ENDIF;
079800140923
079900140923         ENDSL;
080000140923
080100140923       ENDSR;
080200110519
080300110519       //--------------------------------------------------------------
080400110519       //?Operazioni finali.
080500110519       //--------------------------------------------------------------
080600110519       BEGSR RoutEnd;
080700110519
080800110519         *inLR = *on;
080900110519         return;
081000110519
081100110519       ENDSR;
081200110519
081300110519       //--------------------------------------------------------------
081400110519       //?Reperimento Dati del job (Utente/Operativi).
081500110519       //--------------------------------------------------------------
081600110519       BEGSR DatiJob;
081700110519
081800110519         in(E) §AzUte;
081900110519         if NOT %error;
082000110519           in(E) §DatiUte;
082100110519         endif;
082200110519         if %error or RSut = *blanks;
082300110519           clear TIBS34ds;
082400110519           tibs34r(tibs34ds);
082500110519           in §AzUte;
082600110519           in §DatiUte;
082700110519         endif;
082800110519
082900110519       ENDSR;
083000080206
083100080206       //--------------------------------------------------------------
083200110517       //?Inizializzazione videata D01.
083300080206       //--------------------------------------------------------------
083400080206       BEGSR InzD01;
083500080206
083600110517         clear MK12D01;
083700110517
083800110517         V01fsf = 'S';
083900110523
084000110523         //?Imposto le categorie potenziale da selezionare di default
084100110523         //?Mai codificato - Codificato - Perso
084200110523         V01fls1 = $CPOS(1);
084300110523         V01fls2 = $CPOS(2);
084400110523         V01fls3 = $CPOS(3);
084500110523         V01fls4 = $CPOS(4);
084600110523         V01fls5 = $CPOS(5);
084700140923
084800140923         //?Pulisco i formati video delle info potenziali
084900140923         clear MK12D02;
085000140923         clear MK12D04;
085100080206
085200080206       ENDSR;
085300080206
085400080206       //--------------------------------------------------------------
085500110517       //?Gestione tasto funzionale F3 da videata D01.
085600080206       //?F3=Fine
085700080206       //--------------------------------------------------------------
085800080206       BEGSR F03D01;
085900080206
086000080207         // Chiusura del programma
086100080206         $Fine = *on;
086200080206
086300080206       ENDSR;
086400080206
086500080206       //--------------------------------------------------------------
086600110517       //?Gestione tasto funzionale F6 da videata D01.
086700110517       //?F06=Parametri di stampa
086800080206       //--------------------------------------------------------------
086900110517       BEGSR F06D01;
087000080206
087100110517         $InzD01 = *off;
087200110519         $InzD02 = *off;
087300110517         $InzD03 = *on;
087400140923         $InzD04 = *off;
087500110517         $Video = 'D3';
087600080206
087700080206       ENDSR;
087800110517
087900110517       //--------------------------------------------------------------
088000110517       //?Gestione tasto funzionale F8 da videata D01.
088100110517       //?F08=Selezione Info Commerciali
088200110517       //--------------------------------------------------------------
088300110517       BEGSR F08D01;
088400110517
088500110517         $InzD01 = *off;
088600110517         $InzD02 = *on;
088700110519         $InzD03 = *off;
088800140923         $InzD04 = *off;
088900110517         $Video = 'D2';
089000110517
089100110517       ENDSR;
089200080206
089300080206       //--------------------------------------------------------------
089400110519       //?Controllo videata D01.
089500080206       //--------------------------------------------------------------
089600080206       BEGSR CtrD01;
089700080206
089800080206         WindDspF = IndDspF;
089900080206         reset WindDspFb;
090000080206         IndDspF  = WindDspF;
090100110308
090200110517       //?Controllo se richiesto almeno 1 dei dati obbligatori
090300110530         IF  V01cmmu = *blanks and
090400110530             V01prv = *blanks and V01cap = *blanks and
090500110517             V01naz = *blanks and V01loc = *blanks and
090600110524             V01car = *blanks and V01fil = *zeros;
090700110517           ErrMessage  = *on;
090800110517           ErrGenerico = *on;
090900110530           PosCurCmmu  = *on;
091000110517           V01msg      = $Msg(02);
091100110517           leavesr;
091200110517         ENDIF;
091300110530
091400110530       //?COMMERCIALE UNIFICANTE
091500110530         IF  V01cmmu <> *blanks;
091600110530           clear V01dcmmu;
091700110530
091800110530         //?Ricerca
091900110530           IF  %scan('?' : V01cmmu) > 0;
092000110530             ErrGenerico = *on;
092100110530             PosCurCmmu  = *on;
092200130808             clear  TRMK43ds;
092300130808             iMK43solU = 'S';
092400130808             iMK43fil  = DUTpou;
092500130808             TRMK43R (kpjba : TRMK43ds);
092600130808             if  oMK43err = *off  and  oMK43fxx = *blank;
092700130808               V01cmmu  = %editc( oMK43cmm : 'X' );
092800130808               V01dcmmu = oMK43des;
092900130808             endif;
093000110530           ENDIF;
093100110530
093200110530         //?Solo numeri
093300110530           IF  %check(digits:V01cmmu) > 0;
093400110530             ErrMessage  = *on;
093500110530             ErrGenerico = *on;
093600110530             PosCurCmmu  = *on;
093700110530             V01msg      = $Msg(05);
093800110530             leavesr;
093900110530           ENDIF;
094000110530
094100110530         //?Controllo e decodifico
094200130808           CMMcod = %int(V01cmmu);
094300130808           chain  (CMMcod)  AZCMM000;
094400130808           IF  not %found( AZCMM01L );
094500110530             ErrMessage  = *on;
094600110530             ErrGenerico = *on;
094700110530             PosCurCmmu  = *on;
094800110530             V01msg      = $Msg(05);
094900110530             leavesr;
095000110530           ENDIF;
095100110530
095200110530         //?utente abilitato al commerciale
095300110530           clear TNTAA1DS;
095400110530           ITAA1caut = '§utegtc';
095500110530           ITAA1cmm  = %dec(V01cmmu:7:0);
095600110530           kpjbu     = tntaa1ds;
095700110530           tntaa1c (kpjba);
095800110530           TNTAA1DS = kpjbu;
095900110530           IF  OTAA1fabi = 'N';
096000110530             ErrMessage  = *on;
096100110530             ErrGenerico = *on;
096200110530             PosCurCmmu  = *on;
096300110530             V01msg      = $Msg(05);
096400110530             leavesr;
096500110530           ENDIF;
096600110530
096700130808           V01dcmmu = CMMdes;
096800110530
096900110530         //?Controllo che non abbia particolarità.
097000130808           IF  CMMpar <> *blanks;
097100110530             ErrMessage  = *on;
097200110530             ErrGenerico = *on;
097300110530             PosCurCmmu  = *on;
097400110530             V01msg      = $Msg(05);
097500110530             leavesr;
097600110530           ENDIF;
097700110530
097800110530         //?Deve essere un commerciale unificante
097900130808           IF  %dec(V01cmmu:7:0) <> CMMuni;
098000110530             ErrMessage  = *on;
098100110530             ErrGenerico = *on;
098200110530             PosCurCmmu  = *on;
098300110530             V01msg      = $Msg(06);
098400110530             leavesr;
098500110530           ENDIF;
098600110530         ENDIF;
098700110517
098800110517       //?INDIRIZZO
098900110517       //?Se inserito il cap o la località è obbligatoria la provincia
099000110517         IF  (V01cap <> *blanks or  V01loc <> *blanks)  and
099100110524              V01prv  = *blanks and V01naz = *blanks;
099200110517           ErrMessage  = *on;
099300110517           ErrGenerico = *on;
099400110517           PosCurPrv   = *on;
099500110517           V01msg      = $Msg(03);
099600110517           leavesr;
099700110517         ENDIF;
099800110517
099900110517       //?Se inserito il cap e la località controllo se sono congruenti
100000110518       //?anche con la nazione
100100110517         IF  V01cap <> *blanks;
100200110517           reset FNLV13DS;
100300110517           reset TISI95DS;
100400110517           I95cap = V01cap;
100500110517           I95prv = V01prv;
100600110518           I95nar = V01naz;
100700110517           fnlv13r (kpjba : fnlv13ds : tisi95ds);
100800110517           IF  O13err <> *blanks;
100900110517             ErrMessage  = *on;
101000110517             ErrGenerico = *on;
101100110517             SELECT;
101200110517               WHEN  O13err = '4';
101300110517                 PosCurPrv   = *on;
101400110517               WHEN  O13err = '5';
101500110517                 PosCurCap   = *on;
101600110518               WHEN  O13err = '6';
101700110518                 PosCurNaz   = *on;
101800110517             ENDSL;
101900110517             V01msg = O13msg;
102000110517             leavesr;
102100110517           ENDIF;
102200110517         ENDIF;
102300110518
102400110518       //?NAZIONE
102500110518         IF  V01naz <> *blanks;
102600110518
102700110518         //?Ricerca
102800110518           IF  %scan('?' : V01naz) > 0;
102900110518             ErrGenerico = *on;
103000110518             PosCurNaz   = *on;
103100110524             idTabella = '15';
103200110518             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
103300110518                            tastoUscita);
103400110524             V01naz = idElemento;
103500110518           ENDIF;
103600110518
103700110518         //?Controllo
103800110518           k_TBLkut = 1;
103900110518           k_TBLcod = '15';
104000110518           k_TBLkey = V01naz;
104100110518           chain  %kds(K03tabel) TABEL00F;
104200110518           IF  not %found( TABEL00F ) or TBLflg <> *blanks;
104300110518             ErrMessage  = *on;
104400110518             ErrGenerico = *on;
104500110518             PosCurNaz   = *on;
104600110518             V01msg      = $Msg(04);
104700110518             leavesr;
104800110518           ENDIF;
104900110518         ENDIF;
105000110517
105100110517       //?AREA
105200110517         IF  V01car <> *blanks;
105300110517           clear V01dcar;
105400110517
105500110517         //?Ricerca
105600110517           IF  %scan('?' : V01car) > 0;
105700110517             ErrGenerico = *on;
105800110517             PosCurCar   = *on;
105900110517             idTabella = '05';
106000110517             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
106100110517                            tastoUscita);
106200110517             V01car = idElemento;
106300110517           ENDIF;
106400110517
106500110517         //?Solo numeri
106600110517           IF  %check(digits:V01car) > 0;
106700110517             ErrMessage  = *on;
106800110517             ErrGenerico = *on;
106900110517             PosCurCar   = *on;
107000110518             V01msg      = $Msg(07);
107100110517             leavesr;
107200110517           ENDIF;
107300110517
107400110517         //?Controllo e decodifico
107500110517           clear ds05;
107600110517           k_TBLkut = 1;
107700110517           k_TBLcod = '05';
107800110517           k_TBLkey = V01car;
107900110517           chain  %kds(K03tabel) TABEL00F;
108000110517           IF  not %found( TABEL00F ) or TBLflg <> *blanks;
108100110517             ErrMessage  = *on;
108200110517             ErrGenerico = *on;
108300110517             PosCurCar   = *on;
108400110518             V01msg      = $Msg(07);
108500110517             leavesr;
108600110517           ENDIF;
108700110517
108800110517           ds05    = TBLuni;
108900110517           V01dcar = §05des;
109000110517         ENDIF;
109100110518
109200110518       //?FILIALE APPARTENENZA
109300110518         IF  V01fil > 0;
109400110518           clear V01dfil;
109500110518           chain V01fil AZORG01L;
109600110518           IF  not %found(AZORG01L) or ORGfva <> *blanks;
109700110518             ErrMessage  = *on;
109800110518             ErrGenerico = *on;
109900110518             PosCurFil   = *on;
110000110518             V01msg      = $Msg(08);
110100110518             leavesr;
110200110518           ENDIF;
110300110518           V01dfil = ORGdes;
110400110518         ENDIF;
110500110518
110600110518       //?CATEGORIA MERCEOLOGICA
110700110518         IF  V01sct <> *blanks;
110800110518           clear V01dsct;
110900110518
111000110518         //?Ricerca
111100110518           IF  %scan('?' : V01sct) > 0;
111200110518             ErrGenerico = *on;
111300110518             PosCurSct   = *on;
111400110518             idTabella = '1L';
111500110518             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
111600110518                            tastoUscita);
111700110518             V01sct = idElemento;
111800110518           ENDIF;
111900110518
112000110518         //?Solo numeri
112100110518           IF  %check(digits:V01sct) > 0;
112200110518             ErrMessage  = *on;
112300110518             ErrGenerico = *on;
112400110518             PosCurSct   = *on;
112500110518             V01msg      = $Msg(09);
112600110518             leavesr;
112700110518           ENDIF;
112800110518
112900110518         //?Controllo e decodifico
113000110518           clear ds1L;
113100110518           k_TBLkut = 1;
113200110518           k_TBLcod = '1L';
113300110518           k_TBLkey = V01sct;
113400110518           chain  %kds(K03tabel) TABEL00F;
113500110518           IF  not %found( TABEL00F ) or TBLflg <> *blanks;
113600110518             ErrMessage  = *on;
113700110518             ErrGenerico = *on;
113800110518             PosCurSct   = *on;
113900110518             V01msg      = $Msg(09);
114000110518             leavesr;
114100110518           ENDIF;
114200110518
114300110518           ds1L    = TBLuni;
114400110518           V01dsct = §1Ldes;
114500110518         ENDIF;
114600110308
114700110518       //?IMPORTANZA CLIENTE
114800110518         IF  V01cic1 <> *blanks;
114900110518           IF  %scan('?' : V01cic1) > 0;
115000110308             ErrGenerico = *on;
115100110308             PosCurCic1  = *on;
115200110308             idTabella = 'IC';
115300110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
115400110308                            tastoUscita);
115500110309             V01cic1 = idElemento;
115600110308           ENDIF;
115700110518           IF  %lookup(V01cic1 : $Ic) = 0;
115800110308             ErrMessage  = *on;
115900110308             ErrGenerico = *on;
116000110308             PosCurCic1  = *on;
116100110518             V01msg = $Msg(10);
116200110308             leavesr;
116300110308           ENDIF;
116400110518         ENDIF;
116500110518         IF  V01cic2 <> *blanks;
116600110518           IF  %scan('?' : V01cic2) > 0;
116700110308             ErrGenerico = *on;
116800110308             PosCurCic2  = *on;
116900110308             idTabella = 'IC';
117000110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
117100110308                            tastoUscita);
117200110308             V01cic2 = idElemento;
117300110308           ENDIF;
117400110518           IF  %lookup(V01cic2 : $Ic) = 0;
117500110308             ErrMessage  = *on;
117600110308             ErrGenerico = *on;
117700110308             PosCurCic2  = *on;
117800110518             V01msg = $Msg(10);
117900110308             leavesr;
118000110308           ENDIF;
118100110518         ENDIF;
118200110518         IF  V01cic3 <> *blanks;
118300110518           IF  %scan('?' : V01cic3) > 0;
118400110308             ErrGenerico = *on;
118500110308             PosCurCic3  = *on;
118600110308             idTabella = 'IC';
118700110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
118800110308                            tastoUscita);
118900110308             V01cic3 = idElemento;
119000110308           ENDIF;
119100110308           IF  %lookup(V01cic3 : $Ic) = 0;
119200110308             ErrMessage  = *on;
119300110308             ErrGenerico = *on;
119400110308             PosCurCic3  = *on;
119500110518             V01msg = $Msg(10);
119600110308             leavesr;
119700110308           ENDIF;
119800110308         ENDIF;
119900110308         IF  V01cic4 <> *blanks;
120000110518           IF  %scan('?' : V01cic4) > 0;
120100110308             ErrGenerico = *on;
120200110308             PosCurCic4  = *on;
120300110308             idTabella = 'IC';
120400110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
120500110308                            tastoUscita);
120600110308             V01cic4 = idElemento;
120700110308           ENDIF;
120800110308           IF  %lookup(V01cic4 : $Ic) = 0;
120900110308             ErrMessage  = *on;
121000110308             ErrGenerico = *on;
121100110308             PosCurCic4  = *on;
121200110518             V01msg = $Msg(10);
121300110308             leavesr;
121400110308           ENDIF;
121500110308         ENDIF;
121600110308         IF  V01cic5 <> *blanks;
121700110518           IF  %scan('?' : V01cic5) > 0;
121800110308             ErrGenerico = *on;
121900110308             PosCurCic5  = *on;
122000110308             idTabella = 'IC';
122100110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
122200110308                            tastoUscita);
122300110308             V01cic5 = idElemento;
122400110308           ENDIF;
122500110308           IF  %lookup(V01cic5 : $Ic) = 0;
122600110308             ErrMessage  = *on;
122700110308             ErrGenerico = *on;
122800110308             PosCurCic5  = *on;
122900110518             V01msg = $Msg(10);
123000110308             leavesr;
123100110308           ENDIF;
123200110308         ENDIF;
123300110518
123400110518       //?CATEGORIA POTENZIALE
123500110407         IF  V01fls1 <> *blanks;
123600110407           IF  %scan('?' : V01fls1) > 0;
123700110308             ErrGenerico = *on;
123800110407             PosCurFls1  = *on;
123900110509             clear TNTB87ds;
124000110509             clear V01fls1;
124100110509             tntb87r  (tntb87ds);
124200110509             IF  TB87fxx = *blanks and TB87err = *off;
124300110509               V01fls1 = TB87cod;
124400110308             ENDIF;
124500110308           ENDIF;
124600110407           IF  %lookup(V01fls1 : $Cpo) = 0;
124700110314             ErrMessage  = *on;
124800110314             ErrGenerico = *on;
124900110407             PosCurFls1  = *on;
125000110518             V01msg = $Msg(11);
125100110314             leavesr;
125200110308           ENDIF;
125300110308         ENDIF;
125400110407         IF  V01fls2 <> *blanks;
125500110407           IF  %scan('?' : V01fls2) > 0;
125600110407             ErrGenerico = *on;
125700110407             PosCurFls2  = *on;
125800110509             clear TNTB87ds;
125900110509             clear V01fls2;
126000110509             tntb87r  (tntb87ds);
126100110509             IF  TB87fxx = *blanks and TB87err = *off;
126200110509               V01fls2 = TB87cod;
126300110509             ENDIF;
126400110407           ENDIF;
126500110407           IF  %lookup(V01fls2 : $Cpo) = 0;
126600110407             ErrMessage  = *on;
126700110407             ErrGenerico = *on;
126800110407             PosCurFls2  = *on;
126900110518             V01msg = $Msg(11);
127000110407             leavesr;
127100110407           ENDIF;
127200110407         ENDIF;
127300110407         IF  V01fls3 <> *blanks;
127400110407           IF  %scan('?' : V01fls3) > 0;
127500110407             ErrGenerico = *on;
127600110407             PosCurFls3  = *on;
127700110509             clear TNTB87ds;
127800110509             clear V01fls3;
127900110509             tntb87r  (tntb87ds);
128000110509             IF  TB87fxx = *blanks and TB87err = *off;
128100110509               V01fls3 = TB87cod;
128200110509             ENDIF;
128300110407           ENDIF;
128400110407           IF  %lookup(V01fls3 : $Cpo) = 0;
128500110407             ErrMessage  = *on;
128600110407             ErrGenerico = *on;
128700110407             PosCurFls3  = *on;
128800110518             V01msg = $Msg(11);
128900110407             leavesr;
129000110407           ENDIF;
129100110407         ENDIF;
129200110407         IF  V01fls4 <> *blanks;
129300110407           IF  %scan('?' : V01fls4) > 0;
129400110407             ErrGenerico = *on;
129500110407             PosCurFls4  = *on;
129600110509             clear TNTB87ds;
129700110509             clear V01fls4;
129800110509             tntb87r  (tntb87ds);
129900110509             IF  TB87fxx = *blanks and TB87err = *off;
130000110509               V01fls4 = TB87cod;
130100110509             ENDIF;
130200110407           ENDIF;
130300110407           IF  %lookup(V01fls4 : $Cpo) = 0;
130400110407             ErrMessage  = *on;
130500110407             ErrGenerico = *on;
130600110407             PosCurFls4  = *on;
130700110518             V01msg = $Msg(11);
130800110407             leavesr;
130900110407           ENDIF;
131000110407         ENDIF;
131100110407         IF  V01fls5 <> *blanks;
131200110407           IF  %scan('?' : V01fls5) > 0;
131300110407             ErrGenerico = *on;
131400110407             PosCurFls5  = *on;
131500110509             clear TNTB87ds;
131600110509             clear V01fls5;
131700110509             tntb87r  (tntb87ds);
131800110509             IF  TB87fxx = *blanks and TB87err = *off;
131900110509               V01fls5 = TB87cod;
132000110509             ENDIF;
132100110407           ENDIF;
132200110407           IF  %lookup(V01fls5 : $Cpo) = 0;
132300110407             ErrMessage  = *on;
132400110407             ErrGenerico = *on;
132500110407             PosCurFls5  = *on;
132600110518             V01msg = $Msg(11);
132700110407             leavesr;
132800110407           ENDIF;
132900110407         ENDIF;
133000140923
133100140923       //?% FATTURATO EXPORT
133200140923         IF  V01pexpda <> 0 or V01pexpa <> 0;
133300140923         //?Obbligatori tutti e 2 se immesso uno dei due
133400140923           IF  V01pexpda <> 0  and  V01pexpa = 0;
133500140923             ErrMessage  = *on;
133600140923             ErrGenerico = *on;
133700140923             PosCurPexpa = *on;
133800140923             V01msg      = $Msg(27);
133900140923             leavesr;
134000140923           ENDIF;
134100140923           IF  V01pexpda = 0  and  V01pexpa <> 0;
134200140923             ErrMessage  = *on;
134300140923             ErrGenerico = *on;
134400140923             PosCurPexpda= *on;
134500140923             V01msg      = $Msg(27);
134600140923             leavesr;
134700140923           ENDIF;
134800140923         //?Controllo congruenza tra "DA" e "A"
134900140923           IF  V01pexpda > V01pexpa;
135000140923             ErrMessage  = *on;
135100140923             ErrGenerico = *on;
135200140923             PosCurPexpda= *on;
135300140923             V01msg      = $Msg(28);
135400140923             leavesr;
135500140923           ENDIF;
135600140923         ENDIF;
135700140923
135800140923       //?ANNO INIZIO ATTIVITA'
135900140923         IF  V01annoda <> 0 or V01annoa <> 0;
136000140923         //?Obbligatori tutti e 2 se immesso uno dei due
136100140923           IF  V01annoda <> 0  and  V01annoa = 0;
136200140923             ErrMessage  = *on;
136300140923             ErrGenerico = *on;
136400140923             PosCurAnnoa = *on;
136500140923             V01msg      = $Msg(29);
136600140923             leavesr;
136700140923           ENDIF;
136800140923           IF  V01annoda = 0  and  V01annoa <> 0;
136900140923             ErrMessage  = *on;
137000140923             ErrGenerico = *on;
137100140923             PosCurAnnoda= *on;
137200140923             V01msg      = $Msg(29);
137300140923             leavesr;
137400140923           ENDIF;
137500140923         //?Controllo congruenza tra "DA" e "A"
137600140923           IF  V01annoda > V01annoa;
137700140923             ErrMessage  = *on;
137800140923             ErrGenerico = *on;
137900140923             PosCurAnnoda= *on;
138000140923             V01msg      = $Msg(30);
138100140923             leavesr;
138200140923           ENDIF;
138300140923         ENDIF;
138400110518
138500110518       //?COMMERCIALE PROSSIMA ATTIVITA'
138600110530         IF  V01cmm <> *blanks;
138700110518           clear V01dcmm;
138800110518
138900110518         //?Ricerca
139000110518           IF  %scan('?' : V01cmm) > 0;
139100110518             ErrGenerico = *on;
139200110518             PosCurCmm   = *on;
139300130808             clear  TRMK43ds;
139400130808             iMK43solU = 'S';
139500130808             iMK43fil  = DUTpou;
139600130808             TRMK43R (kpjba : TRMK43ds);
139700130808             if  oMK43err = *off  and  oMK43fxx = *blank;
139800130808               V01cmm  = %editc( oMK43cmm : 'X' );
139900130808               V01dcmm = oMK43des;
140000130808             endif;
140100110518           ENDIF;
140200110518
140300110518         //?Solo numeri
140400110518           IF  %check(digits:V01cmm) > 0;
140500110518             ErrMessage  = *on;
140600110518             ErrGenerico = *on;
140700110518             PosCurCmm   = *on;
140800110518             V01msg      = $Msg(05);
140900110518             leavesr;
141000110518           ENDIF;
141100110518
141200110518         //?Controllo e decodifico
141300130808           CMMcod = %int(V01cmm);
141400130808           chain  (CMMcod)  AZCMM000;
141500130808           IF  not %found( AZCMM01L );
141600110518             ErrMessage  = *on;
141700110518             ErrGenerico = *on;
141800110518             PosCurCmm   = *on;
141900110518             V01msg      = $Msg(05);
142000110518             leavesr;
142100110518           ENDIF;
142200110518
142300110518         //?utente abilitato al commerciale
142400110518           clear TNTAA1DS;
142500110518           ITAA1caut = '§utegtc';
142600110518           ITAA1cmm  = %dec(V01cmm:7:0);
142700110518           kpjbu     = tntaa1ds;
142800110518           tntaa1c (kpjba);
142900110518           TNTAA1DS = kpjbu;
143000110518           IF  OTAA1fabi = 'N';
143100110518             ErrMessage  = *on;
143200110518             ErrGenerico = *on;
143300110518             PosCurCmm   = *on;
143400110518             V01msg      = $Msg(05);
143500110518             leavesr;
143600110518           ENDIF;
143700110518
143800130808           V01dcmm = CMMdes;
143900110518
144000110518         //?Controllo che non abbia particolarità.
144100130808           IF  CMMpar <> *blanks;
144200110518             ErrMessage  = *on;
144300110518             ErrGenerico = *on;
144400110518             PosCurCmm   = *on;
144500110518             V01msg      = $Msg(05);
144600110518             leavesr;
144700110518           ENDIF;
144800110526
144900110526         //?Deve essere un commerciale unificante
145000130808           IF  %dec(V01cmm:7:0) <> CMMuni;
145100110526             ErrMessage  = *on;
145200110526             ErrGenerico = *on;
145300110526             PosCurCmm   = *on;
145400110526             V01msg      = $Msg(06);
145500110526             leavesr;
145600110526           ENDIF;
145700110526         ENDIF;
145800110518
145900110518       //?PROSSIMA ATTIVITA'
146000110518         IF  V01cco1 <> *blanks;
146100110524           IF  %lookup(V01cco1 : $CCO) = 0;
146200110518             ErrMessage  = *on;
146300110518             ErrGenerico = *on;
146400110518             PosCurCco1  = *on;
146500110518             V01msg = $Msg(12);
146600110518             leavesr;
146700110518           ENDIF;
146800110518         ENDIF;
146900110518         IF  V01cco2 <> *blanks;
147000110524           IF  %lookup(V01cco2 : $CCO) = 0;
147100110518             ErrMessage  = *on;
147200110518             ErrGenerico = *on;
147300110518             PosCurCco2  = *on;
147400110518             V01msg = $Msg(12);
147500110518             leavesr;
147600110518           ENDIF;
147700110518         ENDIF;
147800110518         IF  V01cco3 <> *blanks;
147900110524           IF  %lookup(V01cco3 : $CCO) = 0;
148000110518             ErrMessage  = *on;
148100110518             ErrGenerico = *on;
148200110518             PosCurCco3  = *on;
148300110518             V01msg = $Msg(12);
148400110518             leavesr;
148500110518           ENDIF;
148600110518         ENDIF;
148700110518         IF  V01cco4 <> *blanks;
148800110524           IF  %lookup(V01cco4 : $CCO) = 0;
148900110518             ErrMessage  = *on;
149000110518             ErrGenerico = *on;
149100110518             PosCurCco4  = *on;
149200110518             V01msg = $Msg(12);
149300110518             leavesr;
149400110518           ENDIF;
149500110518         ENDIF;
149600110518         IF  V01cco5 <> *blanks;
149700110524           IF  %lookup(V01cco5 : $CCO) = 0;
149800110518             ErrMessage  = *on;
149900110518             ErrGenerico = *on;
150000110518             PosCurCco5  = *on;
150100110518             V01msg = $Msg(12);
150200110518             leavesr;
150300110518           ENDIF;
150400110518         ENDIF;
150500110519
150600110519       //?DATA PROSSIMA ATTIVITA'
150700110524         clear wdadd;
150800110524         clear wdada;
150900110519         IF  V01dadd <> 0 or V01dada <> 0;
151000110519         //?Data prossima attività dal
151100110519           IF  V01dadd <> 0;
151200110519             clear wlbdat;
151300110519             G02dat = V01dadd;
151400110519             xsrda8(wlbdat);
151500110519             IF  G02err = '1';
151600110519               ErrMessage  = *on;
151700110519               ErrGenerico = *on;
151800110519               PosCurDadd  = *on;
151900110519               V01msg      = $Msg(13);
152000110519               leavesr;
152100110519             ENDIF;
152200110519             V01dadd = G02dat;
152300110519             wdadd   = G02inv;
152400110519           ENDIF;
152500110519         //?Data prossima attività al
152600110519           IF  V01dada <> 0;
152700110519             clear wlbdat;
152800110519             G02dat = V01dada;
152900110519             xsrda8(wlbdat);
153000110519             IF  G02err = '1';
153100110519               ErrMessage  = *on;
153200110519               ErrGenerico = *on;
153300110519               PosCurDada  = *on;
153400110519               V01msg      = $Msg(13);
153500110519               leavesr;
153600110519             ENDIF;
153700110519             V01dada = G02dat;
153800110519             wdada   = G02inv;
153900110519           ENDIF;
154000110519         //?"DAL" congruente con "AL"
154100110519           IF  wdadd > 0 and wdada = 0;
154200110519             ErrMessage  = *on;
154300110519             ErrGenerico = *on;
154400110519             PosCurDadd  = *on;
154500110519             V01msg      = $Msg(14);
154600110519             leavesr;
154700110519           ENDIF;
154800110519           IF  wdadd > 0 and wdada > 0 and wdadd > wdada;
154900110519             ErrMessage  = *on;
155000110519             ErrGenerico = *on;
155100110519             PosCurDada  = *on;
155200110519             V01msg      = $Msg(14);
155300110519             leavesr;
155400110519           ENDIF;
155500110519         ENDIF;
155600110519
155700110519       ENDSR;
155800110519
155900110519       //--------------------------------------------------------------
156000110519       //?Inizializzazione videata D02.
156100110519       //--------------------------------------------------------------
156200110519       BEGSR InzD02;
156300110519
156400110519         clear MK12D02;
156500110519
156600110519       ENDSR;
156700140923
156800140923       //--------------------------------------------------------------
156900140923       //?Gestione tasto funzionale F8 da videata D02.
157000140923       //?F08=Selezione Altre Info Commerciali
157100140923       //--------------------------------------------------------------
157200140923       BEGSR F08D02;
157300140923
157400140923         $InzD01 = *off;
157500140923         $InzD02 = *off;
157600140923         $InzD03 = *off;
157700140923         $InzD04 = *on;
157800140925         $Video = 'D4';
157900140923
158000140923       ENDSR;
158100110519
158200110519       //--------------------------------------------------------------
158300110519       //?Gestione tasto funzionale F12 da videata D02.
158400110519       //?F12=Ritorno
158500110519       //--------------------------------------------------------------
158600110519       BEGSR F12D02;
158700110519
158800110519         $InzD01 = *on;
158900110519         $InzD02 = *off;
159000110519         $InzD03 = *off;
159100140923         $InzD04 = *off;
159200110519         $Video = 'D1';
159300110519
159400110519       ENDSR;
159500110519
159600110519       //--------------------------------------------------------------
159700110519       //?Controllo videata D02.
159800110519       //--------------------------------------------------------------
159900110519       BEGSR CtrD02;
160000110519
160100110519         WindDspF = IndDspF;
160200110519         reset WindDspFb;
160300110519         IndDspF  = WindDspF;
160400110519
160500110519       //?SPESA TRASPORTO TOTALE
160600110519       //?la spesa DA non deve essere maggiore della spesa A
160700110519         IF  V02sptda > V02spta;
160800110519           ErrMessage  = *on;
160900110519           ErrGenerico = *on;
161000110524           PosCurSpta  = *on;
161100110519           V02msg      = $Msg(15);
161200110519           leavesr;
161300110519         ENDIF;
161400140923
161500140923       //?Spesa presunta
161600140923         IF  V02spta = *zeros and V02sptp <> *blanks;
161700140923           ErrMessage  = *on;
161800140923           ErrGenerico = *on;
161900140923           PosCurSptp  = *on;
162000140923           V02msg      = $Msg(32);
162100140923           leavesr;
162200140923         ENDIF;
162300110520
162400110520       //?RIPARTIZIONE
162500110520         IF  V02ripg <> *blanks and
162600110520            (V02rip1 <> *blanks or V02rip2 <> *blanks or
162700110520             V02rip3 <> *blanks);
162800110520           ErrMessage  = *on;
162900110520           ErrGenerico = *on;
163000110520           PosCurRip1  = *on;
163100110520           V02msg      = $Msg(16);
163200110520           leavesr;
163300110520         ENDIF;
163400110520         IF  V02rip1 <> *blanks;
163500110520           IF  %scan('?' : V02rip1) > 0;
163600110520             ErrGenerico = *on;
163700110520             PosCurRip1  = *on;
163800110520             clear TNTB70DS;
163900110520             ITB70ifg = '3T';
164000110520             tntb70r (kpjba : TNTB70DS);
164100110520             IF  OTB70spf <> *blanks;
164200110520               V02rip1 = OTB70spf;
164300110520             ENDIF;
164400110520           ENDIF;
164500110523           IF  %lookup(V02rip1 : $IFO) = 0;
164600110520             ErrMessage  = *on;
164700110520             ErrGenerico = *on;
164800110520             PosCurRip1  = *on;
164900110520             V02msg = $Msg(17);
165000110520             leavesr;
165100110520           ENDIF;
165200110520         ENDIF;
165300110520         IF  V02rip2 <> *blanks;
165400110520           IF  %scan('?' : V02rip2) > 0;
165500110520             ErrGenerico = *on;
165600110520             PosCurRip2  = *on;
165700110520             clear TNTB70DS;
165800110520             ITB70ifg = '3T';
165900110520             tntb70r (kpjba : TNTB70DS);
166000110520             IF  OTB70spf <> *blanks;
166100110520               V02rip2 = OTB70spf;
166200110520             ENDIF;
166300110520           ENDIF;
166400110523           IF  %lookup(V02rip2 : $IFO) = 0;
166500110520             ErrMessage  = *on;
166600110520             ErrGenerico = *on;
166700110520             PosCurRip2  = *on;
166800110520             V02msg = $Msg(17);
166900110520             leavesr;
167000110520           ENDIF;
167100110520         ENDIF;
167200110520         IF  V02rip3 <> *blanks;
167300110520           IF  %scan('?' : V02rip3) > 0;
167400110520             ErrGenerico = *on;
167500110520             PosCurRip3  = *on;
167600110520             clear TNTB70DS;
167700110520             ITB70ifg = '3T';
167800110520             tntb70r (kpjba : TNTB70DS);
167900110520             IF  OTB70spf <> *blanks;
168000110520               V02rip3 = OTB70spf;
168100110520             ENDIF;
168200110520           ENDIF;
168300110523           IF  %lookup(V02rip3 : $IFO) = 0;
168400110520             ErrMessage  = *on;
168500110520             ErrGenerico = *on;
168600110520             PosCurRip3  = *on;
168700110520             V02msg = $Msg(17);
168800110520             leavesr;
168900110520           ENDIF;
169000110520         ENDIF;
169100110520         IF  V02ripda > 100;
169200110520           ErrMessage  = *on;
169300110520           ErrGenerico = *on;
169400110520           PosCurRipda = *on;
169500110520           V02msg = $Msg(18);
169600110520           leavesr;
169700110520         ENDIF;
169800110520         IF  V02ripa > 100;
169900110520           ErrMessage  = *on;
170000110520           ErrGenerico = *on;
170100110520           PosCurRipa  = *on;
170200110520           V02msg = $Msg(18);
170300110520           leavesr;
170400110520         ENDIF;
170500110520         IF  V02ripda > V02ripa;
170600110520           ErrMessage  = *on;
170700110520           ErrGenerico = *on;
170800110520           PosCurRipa  = *on;
170900110520           V02msg = $Msg(15);
171000110520           leavesr;
171100110520         ENDIF;
171200140923
171300140923       //?NON AFFIDATO A BRT
171400140923         IF  V02nrfda > V02nrfa;
171500140923           ErrMessage  = *on;
171600140923           ErrGenerico = *on;
171700140923           PosCurNrfa  = *on;
171800140923           V02msg = $Msg(15);
171900140923           leavesr;
172000140923         ENDIF;
172100140923         IF  V02nroda > V02nroa;
172200140923           ErrMessage  = *on;
172300140923           ErrGenerico = *on;
172400140923           PosCurNroa  = *on;
172500140923           V02msg = $Msg(15);
172600140923           leavesr;
172700140923         ENDIF;
172800140923         IF  V02nerda > V02nera;
172900140923           ErrMessage  = *on;
173000140923           ErrGenerico = *on;
173100140923           PosCurNera  = *on;
173200140923           V02msg = $Msg(15);
173300140923           leavesr;
173400140923         ENDIF;
173500140923         IF  V02naeda > V02naea;
173600140923           ErrMessage  = *on;
173700140923           ErrGenerico = *on;
173800140923           PosCurNaea  = *on;
173900140923           V02msg = $Msg(15);
174000140923           leavesr;
174100140923         ENDIF;
174200140923         IF  V02ntrda > V02ntra;
174300140923           ErrMessage  = *on;
174400140923           ErrGenerico = *on;
174500140923           PosCurNtra  = *on;
174600140923           V02msg = $Msg(15);
174700140923           leavesr;
174800140923         ENDIF;
174900140923
175000140923       //?PAESI ESTERO
175100140923         IF  V02paesi1 <> *blanks;
175200140923           IF  %scan('?' : V02paesi1) > 0;
175300140923             ErrGenerico = *on;
175400140923             PosCurPaesi1 = *on;
175500140923             clear TNTB70DS;
175600140923             ITB70tpf = '50';
175700140923             tntb70r (kpjba : TNTB70DS);
175800140923             IF  OTB70spf <> *blanks;
175900140923               V02paesi1  = OTB70spf;
176000140923               V02paesi1d = OTB70dspf;
176100140923             ENDIF;
176200140923           ENDIF;
176300140923           xx = %lookup(V02paesi1 : $PAESI);
176400140923           IF  xx = *zeros;
176500140923             ErrMessage  = *on;
176600140923             ErrGenerico = *on;
176700140923             PosCurPaesi1 = *on;
176800140923             V02msg = $Msg(31);
176900140923             leavesr;
177000140923           ELSE;
177100140923             V02paesi1d = $PAESID(xx);
177200140923           ENDIF;
177300140923         ENDIF;
177400140923         IF  V02paesi2 <> *blanks;
177500140923           IF  %scan('?' : V02paesi2) > 0;
177600140923             ErrGenerico = *on;
177700140923             PosCurPaesi2 = *on;
177800140923             clear TNTB70DS;
177900140923             ITB70tpf = '50';
178000140923             tntb70r (kpjba : TNTB70DS);
178100140923             IF  OTB70spf <> *blanks;
178200140923               V02paesi2  = OTB70spf;
178300140923               V02paesi2d = OTB70dspf;
178400140923             ENDIF;
178500140923           ENDIF;
178600140923           xx = %lookup(V02paesi2 : $PAESI);
178700140923           IF  xx = *zeros;
178800140923             ErrMessage  = *on;
178900140923             ErrGenerico = *on;
179000140923             PosCurPaesi2 = *on;
179100140923             V02msg = $Msg(31);
179200140923             leavesr;
179300140923           ELSE;
179400140923             V02paesi2d = $PAESID(xx);
179500140923           ENDIF;
179600140923         ENDIF;
179700140923         IF  V02paesi3 <> *blanks;
179800140923           IF  %scan('?' : V02paesi3) > 0;
179900140923             ErrGenerico = *on;
180000140923             PosCurPaesi3 = *on;
180100140923             clear TNTB70DS;
180200140923             ITB70tpf = '50';
180300140923             tntb70r (kpjba : TNTB70DS);
180400140923             IF  OTB70spf <> *blanks;
180500140923               V02paesi3  = OTB70spf;
180600140923               V02paesi3d = OTB70dspf;
180700140923             ENDIF;
180800140923           ENDIF;
180900140923           xx = %lookup(V02paesi3 : $PAESI);
181000140923           IF  xx = *zeros;
181100140923             ErrMessage  = *on;
181200140923             ErrGenerico = *on;
181300140923             PosCurPaesi3 = *on;
181400140923             V02msg = $Msg(31);
181500140923             leavesr;
181600140923           ELSE;
181700140923             V02paesi3d = $PAESID(xx);
181800140923           ENDIF;
181900140923         ENDIF;
182000140923         IF  V02paesi4 <> *blanks;
182100140923           IF  %scan('?' : V02paesi4) > 0;
182200140923             ErrGenerico = *on;
182300140923             PosCurPaesi4 = *on;
182400140923             clear TNTB70DS;
182500140923             ITB70tpf = '50';
182600140923             tntb70r (kpjba : TNTB70DS);
182700140923             IF  OTB70spf <> *blanks;
182800140923               V02paesi4  = OTB70spf;
182900140923               V02paesi4d = OTB70dspf;
183000140923             ENDIF;
183100140923           ENDIF;
183200140923           xx = %lookup(V02paesi4 : $PAESI);
183300140923           IF  xx = *zeros;
183400140923             ErrMessage  = *on;
183500140923             ErrGenerico = *on;
183600140923             PosCurPaesi4 = *on;
183700140923             V02msg = $Msg(31);
183800140923             leavesr;
183900140923           ELSE;
184000140923             V02paesi4d = $PAESID(xx);
184100140923           ENDIF;
184200140923         ENDIF;
184300140923         IF  V02paesi5 <> *blanks;
184400140923           IF  %scan('?' : V02paesi5) > 0;
184500140923             ErrGenerico = *on;
184600140923             PosCurPaesi5 = *on;
184700140923             clear TNTB70DS;
184800140923             ITB70tpf = '50';
184900140923             tntb70r (kpjba : TNTB70DS);
185000140923             IF  OTB70spf <> *blanks;
185100140923               V02paesi5  = OTB70spf;
185200140923               V02paesi5d = OTB70dspf;
185300140923             ENDIF;
185400140923           ENDIF;
185500140923           xx = %lookup(V02paesi5 : $PAESI);
185600140923           IF  xx = *zeros;
185700140923             ErrMessage  = *on;
185800140923             ErrGenerico = *on;
185900140923             PosCurPaesi5 = *on;
186000140923             V02msg = $Msg(31);
186100140923             leavesr;
186200140923           ELSE;
186300140923             V02paesi5d = $PAESID(xx);
186400140923           ENDIF;
186500140923         ENDIF;
186600140923
186700140923       ENDSR;
186800140923
186900140923       //--------------------------------------------------------------
187000140923       //?Inizializzazione videata D04.
187100140923       //--------------------------------------------------------------
187200140923       BEGSR InzD04;
187300140923
187400140923         clear MK12D04;
187500140923
187600140923       ENDSR;
187700140923
187800140923       //--------------------------------------------------------------
187900140923       //?Gestione tasto funzionale F12 da videata D04.
188000140923       //?F12=Ritorno
188100140923       //--------------------------------------------------------------
188200140923       BEGSR F12D04;
188300140923
188400140923       //?Torno alla prima videata delle info
188500140923         $InzD01 = *off;
188600140923         $InzD02 = *on;
188700140923         $InzD03 = *off;
188800140923         $InzD04 = *off;
188900140923         $Video = 'D2';
189000140923
189100140923       ENDSR;
189200140923
189300140923       //--------------------------------------------------------------
189400140923       //?Controllo videata D04.
189500140923       //--------------------------------------------------------------
189600140923       BEGSR CtrD04;
189700140923
189800140923         WindDspF = IndDspF;
189900140923         reset WindDspFb;
190000140923         IndDspF  = WindDspF;
190100110520
190200110520       //?ALTRE CARATTERISTICHE
190300110520         IF  V02pesda > V02pesa;
190400110520           ErrMessage  = *on;
190500110520           ErrGenerico = *on;
190600110520           PosCurPesa  = *on;
190700110520           V02msg = $Msg(15);
190800110520           leavesr;
190900110520         ENDIF;
191000110520
191100110524       //?AFFIDATO A BRT
191200110520         IF  V02barda > 100;
191300110520           ErrMessage  = *on;
191400110520           ErrGenerico = *on;
191500110520           PosCurBarda = *on;
191600110520           V02msg = $Msg(18);
191700110520           leavesr;
191800110520         ENDIF;
191900110520         IF  V02bara > 100;
192000110520           ErrMessage  = *on;
192100110520           ErrGenerico = *on;
192200110520           PosCurBara  = *on;
192300110520           V02msg = $Msg(18);
192400110520           leavesr;
192500110520         ENDIF;
192600110520         IF  V02barda > V02bara;
192700110520           ErrMessage  = *on;
192800110520           ErrGenerico = *on;
192900110520           PosCurBara  = *on;
193000110520           V02msg = $Msg(15);
193100110520           leavesr;
193200110520         ENDIF;
193300110520
193400110520       //?CONCORRENTI
193500110523         clear wConta;
193600110523         IF  V02conc1 <> *blanks;
193700110523           IF  %scan('?' : V02conc1) > 0;
193800110523             ErrGenerico = *on;
193900110523             PosCurConc1 = *on;
194000110524             clear TNTB70DS;
194100110524             ITB70tpf = '10';
194200110524             tntb70r (kpjba : TNTB70DS);
194300110524             IF  OTB70spf <> *blanks;
194400110524               V02conc1 = OTB70spf;
194500110523             ENDIF;
194600110523           ENDIF;
194700110523           IF  %lookup(V02conc1 : $CON) = 0;
194800110523             ErrMessage  = *on;
194900110523             ErrGenerico = *on;
195000110523             PosCurConc1 = *on;
195100110523             V02msg = $Msg(19);
195200110523             leavesr;
195300110523           ENDIF;
195400110523           wConta += 1;
195500110523         ENDIF;
195600110523         IF  V02conc2 <> *blanks;
195700110523           IF  %scan('?' : V02conc2) > 0;
195800110523             ErrGenerico = *on;
195900110523             PosCurConc2 = *on;
196000110524             clear TNTB70DS;
196100110524             ITB70tpf = '10';
196200110524             tntb70r (kpjba : TNTB70DS);
196300110524             IF  OTB70spf <> *blanks;
196400110524               V02conc2 = OTB70spf;
196500110523             ENDIF;
196600110523           ENDIF;
196700110523           IF  %lookup(V02conc2 : $CON) = 0;
196800110523             ErrMessage  = *on;
196900110523             ErrGenerico = *on;
197000110523             PosCurConc2 = *on;
197100110523             V02msg = $Msg(19);
197200110523             leavesr;
197300110523           ENDIF;
197400110523           wConta += 1;
197500110523         ENDIF;
197600110523         IF  V02conc3 <> *blanks;
197700110523           IF  %scan('?' : V02conc3) > 0;
197800110523             ErrGenerico = *on;
197900110524             PosCurConc3 = *on;
198000110524             clear TNTB70DS;
198100110524             ITB70tpf = '10';
198200110524             tntb70r (kpjba : TNTB70DS);
198300110524             IF  OTB70spf <> *blanks;
198400110524               V02conc3 = OTB70spf;
198500110523             ENDIF;
198600110523           ENDIF;
198700110523           IF  %lookup(V02conc3 : $CON) = 0;
198800110523             ErrMessage  = *on;
198900110523             ErrGenerico = *on;
199000110523             PosCurConc3 = *on;
199100110523             V02msg = $Msg(19);
199200110523             leavesr;
199300110523           ENDIF;
199400110523           wConta += 1;
199500110523         ENDIF;
199600110523         IF  V02conc4 <> *blanks;
199700110523           IF  %scan('?' : V02conc4) > 0;
199800110523             ErrGenerico = *on;
199900110524             PosCurConc4 = *on;
200000110524             clear TNTB70DS;
200100110524             ITB70tpf = '10';
200200110524             tntb70r (kpjba : TNTB70DS);
200300110524             IF  OTB70spf <> *blanks;
200400110524               V02conc4 = OTB70spf;
200500110523             ENDIF;
200600110523           ENDIF;
200700110523           IF  %lookup(V02conc4 : $CON) = 0;
200800110523             ErrMessage  = *on;
200900110523             ErrGenerico = *on;
201000110524             PosCurConc4 = *on;
201100110523             V02msg = $Msg(19);
201200110523             leavesr;
201300110523           ENDIF;
201400110523           wConta += 1;
201500110523         ENDIF;
201600110523         IF  V02conc5 <> *blanks;
201700110523           IF  %scan('?' : V02conc5) > 0;
201800110523             ErrGenerico = *on;
201900110524             PosCurConc5 = *on;
202000110524             clear TNTB70DS;
202100110524             ITB70tpf = '10';
202200110524             tntb70r (kpjba : TNTB70DS);
202300110524             IF  OTB70spf <> *blanks;
202400110524               V02conc5 = OTB70spf;
202500110523             ENDIF;
202600110523           ENDIF;
202700110523           IF  %lookup(V02conc5 : $CON) = 0;
202800110523             ErrMessage  = *on;
202900110523             ErrGenerico = *on;
203000110524             PosCurConc5 = *on;
203100110523             V02msg = $Msg(19);
203200110523             leavesr;
203300110523           ENDIF;
203400110523           wConta += 1;
203500110523         ENDIF;
203600110523       //?Se immessa la % posso selezionare solo 1 concorrente
203700110523         IF  wConta > 1 and (V02conda > 0 or V02cona > 0);
203800110523           ErrMessage  = *on;
203900110523           ErrGenerico = *on;
204000110523           PosCurConc1 = *on;
204100110523           V02msg = $Msg(20);
204200110523           leavesr;
204300110523         ENDIF;
204400110523       //?Controllo la %
204500110523         IF  V02conda > 100;
204600110523           ErrMessage  = *on;
204700110523           ErrGenerico = *on;
204800110523           PosCurConda = *on;
204900110523           V02msg = $Msg(18);
205000110523           leavesr;
205100110523         ENDIF;
205200110523         IF  V02cona > 100;
205300110523           ErrMessage  = *on;
205400110523           ErrGenerico = *on;
205500110523           PosCurCona  = *on;
205600110523           V02msg = $Msg(18);
205700110523           leavesr;
205800110523         ENDIF;
205900110523         IF  V02conda > V02cona;
206000110523           ErrMessage  = *on;
206100110523           ErrGenerico = *on;
206200110523           PosCurCona  = *on;
206300110523           V02msg = $Msg(15);
206400110523           leavesr;
206500110523         ENDIF;
206600110523
206700110523       //?MOTIVO DI NON AFFIDAMENTO
206800110523         clear xx;
206900110523         clear V02dmoi1;
207000110523         IF  V02moi1 <> *blanks;
207100110523           IF  %scan('?' : V02moi1) > 0;
207200110523             ErrGenerico = *on;
207300110523             PosCurMoi1  = *on;
207400110523             clear TNTB70DS;
207500110523             ITB70tpf = 'MOI';
207600110523             tntb70r (kpjba : TNTB70DS);
207700110523             IF  OTB70spf <> *blanks;
207800110523               V02moi1  = OTB70spf;
207900110523               V02dmoi1 = OTB70dspf;
208000110523             ENDIF;
208100110523           ENDIF;
208200110523           xx = %lookup(V02moi1 : $MOI);
208300110523           IF  xx = 0;
208400110523             ErrMessage  = *on;
208500110523             ErrGenerico = *on;
208600110523             PosCurMoi1  = *on;
208700110523             V02msg = $Msg(21);
208800110523             leavesr;
208900110523           ENDIF;
209000110523           V02dmoi1 = $DMOI(xx);
209100110523         ENDIF;
209200110523         clear xx;
209300110523         clear V02dmoi2;
209400110523         IF  V02moi2 <> *blanks;
209500110523           IF  %scan('?' : V02moi2) > 0;
209600110523             ErrGenerico = *on;
209700110523             PosCurMoi2  = *on;
209800110523             clear TNTB70DS;
209900110523             ITB70tpf = 'MOI';
210000110523             tntb70r (kpjba : TNTB70DS);
210100110523             IF  OTB70spf <> *blanks;
210200110523               V02moi2  = OTB70spf;
210300110523               V02dmoi2 = OTB70dspf;
210400110523             ENDIF;
210500110523           ENDIF;
210600110523           xx = %lookup(V02moi2 : $MOI);
210700110523           IF  xx = 0;
210800110523             ErrMessage  = *on;
210900110523             ErrGenerico = *on;
211000110523             PosCurMoi2  = *on;
211100110523             V02msg = $Msg(21);
211200110523             leavesr;
211300110523           ENDIF;
211400110523           V02dmoi2 = $DMOI(xx);
211500110523         ENDIF;
211600110523       //?Non possono essere uguali
211700110523         IF  V02moi1 = V02moi2 and V02moi1 <> *blanks;
211800110523           ErrMessage  = *on;
211900110523           ErrGenerico = *on;
212000110523           PosCurMoi1  = *on;
212100110523           V02msg = $Msg(23);
212200110523           leavesr;
212300110523         ENDIF;
212400110523
212500110523       //?OPERATORI LOGISTICA
212600110523         IF  V02logp <> *blanks;
212700110523           IF  %scan('?' : V02logp) > 0;
212800110523             ErrGenerico = *on;
212900110523             PosCurLogp  = *on;
213000110524             clear TNTB70DS;
213100110524             ITB70tpf = '40';
213200110524             tntb70r (kpjba : TNTB70DS);
213300110524             IF  OTB70spf <> *blanks;
213400110524               V02logp = OTB70spf;
213500110523             ENDIF;
213600110523           ENDIF;
213700110523           IF  %lookup(V02logp : $LOG) = 0;
213800110523             ErrMessage  = *on;
213900110523             ErrGenerico = *on;
214000110523             PosCurLogp  = *on;
214100110523             V02msg = $Msg(23);
214200110523             leavesr;
214300110523           ENDIF;
214400110523         ENDIF;
214500140923
214600140923       //?Metri quadri
214700140923         IF  V02mqlda > V02mqla;
214800140923           ErrMessage  = *on;
214900140923           ErrGenerico = *on;
215000140923           PosCurMqla  = *on;
215100140923           V02msg = $Msg(33);
215200140923           leavesr;
215300140923         ENDIF;
215400140923
215500140923       //?Posti Pallet
215600140923         IF  V02pplda > V02ppla;
215700140923           ErrMessage  = *on;
215800140923           ErrGenerico = *on;
215900140923           PosCurPpla  = *on;
216000140923           V02msg = $Msg(34);
216100140923           leavesr;
216200140923         ENDIF;
216300110524
216400110524       //?INFO COMMERCIALI NON PRESENTI
216500110524         IF  (V02sptda > 0 or V02ripg <> *blanks or
216600110524              V02rip  <> *blanks or V02ripa > 0  or
216700140923              V02nrfa  > 0 or V02nroa > 0 or
216800140923              V02nera  > 0 or V02naea > 0 or
216900140923              V02ntra  > 0 or V02paesi <> *blanks or
217000110524              V02pri  <> *blanks or V02gdo <> *blanks or
217100110524              V02ecm  <> *blanks or V02hpr <> *blanks or
217200110524              V02sbx  <> *blanks or V02pesa > 0 or
217300140923              V02bara > 0 or
217400110524              V02conc <> *blanks or V02cona > 0 or
217500110524              V02moi  <> *blanks or V02logi <> *blanks or
217600110524              V02logo <> *blanks or V02logp <> *blanks or
217700140923              V02mqla  > 0  or V02ppla > 0 or
217800110524              V02ifoal > 0) and V02nifo = 'S';
217900110524           ErrMessage  = *on;
218000110524           ErrGenerico = *on;
218100110524           PosCurNifo  = *on;
218200110524           V02msg      = $Msg(24);
218300110524           leavesr;
218400110524          ENDIF;
218500110523
218600110523       //?DATA INFO COMMERCIALI
218700110524         IF  V02ifoal > 0;
218800110524           clear wifoal;
218900110524           clear wlbdat;
219000110524           G02dat = V02ifoal;
219100110524           xsrda8(wlbdat);
219200110524           IF  G02err = '1';
219300110524             ErrMessage  = *on;
219400110524             ErrGenerico = *on;
219500110524             PosCurIfoal = *on;
219600110524             V02msg      = $Msg(13);
219700110524             leavesr;
219800110524           ENDIF;
219900110524           V02ifoal = G02dat;
220000110524           wifoal   = G02inv;
220100110524         ENDIF;
220200110519
220300110519       ENDSR;
220400110524
220500110524       //--------------------------------------------------------------
220600110524       //?Inizializzazione videata D03.
220700110524       //--------------------------------------------------------------
220800110524       BEGSR InzD03;
220900110524
221000110524         clear MK12D03;
221100110524
221200110524         V03sifo = 'N';
221300110524         IF  F08Attivo;
221400110524           V03sifo = 'S';
221500110524         ENDIF;
221600110524
221700110524       ENDSR;
221800110524
221900110524       //--------------------------------------------------------------
222000110524       //?Gestione tasto funzionale F12 da videata D03.
222100110524       //?F12=Ritorno
222200110524       //--------------------------------------------------------------
222300110524       BEGSR F12D03;
222400110524
222500110524       //?Se abilitato alla stampa info torno alla seconda videata
222600110524         IF  F08Attivo;
222700110524           $InzD01 = *off;
222800110524           $InzD02 = *on;
222900110524           $InzD03 = *off;
223000140923           $InzD04 = *off;
223100110524           $Video = 'D2';
223200110524       //?Alrimenti torno alla prima videata
223300110524         ELSE;
223400110524           $InzD01 = *on;
223500110524           $InzD02 = *off;
223600110524           $InzD03 = *off;
223700140923           $InzD04 = *off;
223800110524           $Video = 'D1';
223900110524         ENDIF;
224000110524
224100110524       ENDSR;
224200110524
224300110524       //--------------------------------------------------------------
224400110524       //?Gestione tasto funzionale F6 da videata D03.
224500110524       //?F06=Elaborazione
224600110524       //--------------------------------------------------------------
224700110524       BEGSR F06D03;
224800110524
224900110524         clear TRMK13DS;
225000110524         IF  V01cmmu <> *blanks;
225100110524           D13cmmu  = %dec(V01cmmu:7:0);
225200110524         ENDIF;
225300110524         D13prv   = V01prv;
225400110530         D13loc   = V01loc;
225500110524         D13cap   = V01cap;
225600110524         D13naz   = V01naz;
225700110524         IF  V01car <> *blanks;
225800110524           D13car   = %dec(V01car:3:0);
225900110524         ENDIF;
226000110524         D13fil   = V01fil;
226100110524         IF  V01sct <> *blanks;
226200110524           D13sct   = %dec(V01sct:5:0);
226300110524         ENDIF;
226400110524         D13cic   = V01cic;
226500110524         D13fls   = V01fls;
226600110524         D13pst   = V01pst;
226700110524         D13fsf   = V01fsf;
226800110525         D13ttr   = V01ttr;
226900110524         IF  V01cmm <> *blanks;
227000110524           D13cmm   = %dec(V01cmm:7:0);
227100110524         ENDIF;
227200110524         D13cco   = V01cco;
227300110524         D13dadda = wdadd;
227400110524         D13dada  = wdada;
227500110524         D13sptda = V02sptda;
227600110524         D13spta  = V02spta;
227700110524         D13ripg  = V02ripg;
227800110524         D13rip   = V02rip;
227900110524         D13ripda = V02ripda;
228000110524         D13ripa  = V02ripa;
228100110524         D13pri   = V02pri;
228200110524         D13gdo   = V02gdo;
228300110524         D13ecm   = V02ecm;
228400110524         D13hpr   = V02hpr;
228500110524         D13sbx   = V02sbx;
228600110524         D13pesda = V02pesda;
228700110524         D13pesa  = V02pesa;
228800110524         D13barda = V02barda;
228900110524         D13bara  = V02bara;
229000110524         D13conc  = V02conc;
229100110524         D13conda = V02conda;
229200110524         D13cona  = V02cona;
229300110524         D13moi   = V02moi;
229400110524         D13logi  = V02logi;
229500110524         D13logo  = V02logo;
229600110524         D13logp  = V02logp;
229700110524         D13nifo  = V02nifo;
229800110524         D13ifoal = wifoal;
229900110525         D13ospt  = V03ospt;
230000110524         D13orag  = V03orag;
230100110525         D13ocmmu = V03ocmmU;
230200110525         D13ocmm  = V03ocmm;
230300110525         D13ocar  = V03ocar;
230400110524         D13osct  = V03osct;
230500110524         D13sifo  = V03sifo;
230600140923
230700140923         D13pexpda = V01pexpda;
230800140923         D13pexpa  = V01pexpa;
230900140923         D13annoda = V01annoda;
231000140923         D13annoa  = V01annoa;
231100140923
231200140923         D13sptp    = V02sptp;
231300140923         D13nrfda   = V02nrfda;
231400140923         D13nrfa    = V02nrfa;
231500140923         D13nroda   = V02nroda;
231600140923         D13nroa    = V02nroa;
231700140923         D13nerda   = V02nerda;
231800140923         D13nera    = V02nera;
231900140923         D13naeda   = V02naeda;
232000140923         D13naea    = V02naea;
232100140923         D13ntrda   = V02ntrda;
232200140923         D13ntra    = V02ntra;
232300140923         D13paesi   = V02paesi;
232400140923         D13mqlda   = V02mqlda;
232500140923         D13mqla    = V02mqla;
232600140923         D13pplda   = V02pplda;
232700140923         D13ppla    = V02ppla;
232800140923
232900140923       //?Imposto i parametri di lancio
233000140923         reset TRUL01DS;
233100140923         UL01azn = kcoaz;
233200140923         UL01prm = TRMK13DS;
233300140923         trul01r (trul01ds);
233400140923         IF  UL01err = *on;
233500140923           ErrMessage  = *on;
233600140923           ErrGenerico = *on;
233700140923           V03msg      = 'Riscontrati errori nei parametri di lancio';
233800140923           leavesr;
233900140923         ENDIF;
234000140923         kpjbu = %editc(UL01prg:'X');
234100140923         kpjbu = %trimr(kpjbu) + '-' +
234200140923                 %subst(UL01prm:1:%len(kpjbu) - 1 - %len(%trimr(kpjbu)));
234300110524
234400110524         kcoaz = 'MK13';
234500110524         kbuff = *blanks;
234600110524
234700110524       //?Richiamo interattivo
234800110524         IF  knmus = *all'1';
234900110524           TRMK13R(kpjba);
235000110524       //?Richiamo batch
235100110524         ELSE;
235200110524           BCH10(kpjba);
235300110524         ENDIF;
235400110524
235500110524       //?Chiusura del programma
235600110524         $Fine = *on;
235700110524
235800110524       ENDSR;
235900110524
236000110524       //--------------------------------------------------------------
236100110524       //?Controllo videata D03.
236200110524       //--------------------------------------------------------------
236300110524       BEGSR CtrD03;
236400110524
236500110524         WindDspF = IndDspF;
236600110524         reset WindDspFb;
236700110524         IndDspF  = WindDspF;
236800110524
236900110524       //?ORDINAMENTI
237000110524         //?obbligatori
237100110525         IF  V03ospt  = *blanks and V03orag = *blanks and
237200110525             V03ocmmu = *blanks and V03ocmm = *blanks and
237300110525             V03ocar  = *blanks and V03osct = *blanks;
237400110524           ErrMessage  = *on;
237500110524           ErrGenerico = *on;
237600110525           IF  F08Attivo;
237700110525             PosCurOspt = *on;
237800110525           ELSE;
237900110525             PosCurOrag = *on;
238000110525           ENDIF;
238100110524           V03msg      = $Msg(25);
238200110524           leavesr;
238300110524         ENDIF;
238400110524         //?possibile solo 1
238500110524         SELECT;
238600110525           WHEN  V03ospt <> *blanks and
238700110525                (V03orag <> *blanks or V03ocmmu <> *blanks or
238800110525                 V03ocmm <> *blanks or V03ocar  <> *blanks or
238900110525                 V03osct <> *blanks);
239000110524             ErrMessage  = *on;
239100110524             ErrGenerico = *on;
239200110525             IF  F08Attivo;
239300110525               PosCurOspt = *on;
239400110525             ELSE;
239500110525               PosCurOrag  = *on;
239600110525             ENDIF;
239700110524             V03msg      = $Msg(26);
239800110524             leavesr;
239900110525           WHEN  V03orag <> *blanks and
240000110525                (V03ocmmu <> *blanks or V03ocmm <> *blanks or
240100110525                 V03ocar  <> *blanks or V03osct <> *blanks);
240200110524             ErrMessage  = *on;
240300110524             ErrGenerico = *on;
240400110525             PosCurOrag  = *on;
240500110524             V03msg      = $Msg(26);
240600110524             leavesr;
240700110525           WHEN  V03ocmmu <> *blanks and
240800110525                (V03ocmm <> *blanks or
240900110525                 V03ocar  <> *blanks or V03osct <> *blanks);
241000110525             ErrMessage  = *on;
241100110525             ErrGenerico = *on;
241200110525             PosCurOcmmu = *on;
241300110525             V03msg      = $Msg(26);
241400110525             leavesr;
241500110525           WHEN  V03ocmm <> *blanks and
241600110525                (V03ocar  <> *blanks or V03osct <> *blanks);
241700110525             ErrMessage  = *on;
241800110525             ErrGenerico = *on;
241900110525             PosCurOcmm  = *on;
242000110525             V03msg      = $Msg(26);
242100110525             leavesr;
242200110525           WHEN  V03ocar <> *blanks and V03osct <> *blanks;
242300110525             ErrMessage  = *on;
242400110525             ErrGenerico = *on;
242500110525             PosCurOcar  = *on;
242600110525             V03msg      = $Msg(26);
242700110525             leavesr;
242800110524         ENDSL;
242900110524
243000110524       ENDSR;
243100080206
243200080206      /end-free
243300080206
243400080206       //--------------------------------------------------------------
243500080206       //?Schiere a tempo di compilazione.
243600080206       //--------------------------------------------------------------
243700080206
243800080206** - $MSG -------------------------------------------------------------------*
243900110308Utente non abilitato alla Funzione.                                           01
244000110530Immettere Commerciale  OPPURE Prov/Località  OPPURE Area  OPPURE Filiale.     02
244100110524Se parzializzazione per cap o località  immettere la provincia o la nazione.  03
244200110518Nazione errata                                                                04
244300110518Commerciale errato o non in gestione all'utente                               05
244400110518Richiedere solo commerciali UNIFICANTI!                                       06
244500110518Codice area errato                                                            07
244600110518Filiale di appartenenza errata.                                               08
244700110518Categoria merceologica errata.                                                09
244800110518Importanza cliente errata.                                                    10
244900110518Categoria Potenziale errata.                                                  11
245000110518Prossima attività errata.                                                     12
245100110519Data errata                                                                   13
245200110519Data "DAL" incongruente con data "AL"                                         14
245300110519valore " DA " non deve essere maggiore del valore " A "                       15
245400110520Per "RIPARTIZIONE SPESA" indicare o la tipologia generica o un tipo specifico 16
245500110520Codice Servizio di ripartizione spesa errato                                  17
245600110520La percentuale immessa non deve superare 100                                  18
245700110523Concorrente errato                                                            19
245800110523Se indicata la percentuale indicare solo UN concorrente                       20
245900110523Motivo non affidamento a noi errato                                           21
246000110523i due motivi non affidamento non possono essere uguali                        22
246100110523Codice operatore logistica errato                                             23
246200110524Impossibile parzializzare le INFO se richiesti potenziali senza info Comm.    24
246300110524Immettere un ordinamento                                                      25
246400110524Scegliere un solo ordinamento                                                 26
246500140923Immettere la % Fatturato Export                                               27
246600140923% Fatturato Export "DA" incongruente con % Fatturato Export "A"               28
246700140923Immettere Anno Inizio Attività                                                29
246800140923Anno Inizio Attivita "DA" incongruente con Anno Inizio Attività "A"           30
246900140923Paese Estero errato                                                           31
247000140923Richieste inclusione spesa presunta solo se richiesta la spesa traporti.      32
247100140923Mq "DA" incongruente con Mq "A"                                               33
247200140923Posti Pallet "DA" incongruente con Posti Pallet "A"                           34
