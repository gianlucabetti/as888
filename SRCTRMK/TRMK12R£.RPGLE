000100130808     /*END
000200110516      //--------------------------------------------------------------
000300110516      //?TRMK12R - STAMPA ANAGRAFICA CLIENTI POTENZIALI
000400110516      //--------------------------------------------------------------
000500110516
000600110516     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700110516     h dftactgrp(*no) actgrp(*caller)
000800110516
000900110516      //---------------------------------------------------------------
001000110516      // Dichiarazione file.
001100110516      //---------------------------------------------------------------
001200110516
001300110516      // - Organigramma filiale/aziendale
001400110516     fAZORG01L  if   e           k disk
001500130808
001600130808      // -?Anagrafica Commerciali?
001700130808     fAZCMM01L  if   e           k disk
001800110516
001900110516      // - Tabelle
002000110516     fTABEL00F  if   e           k disk
002100110516     fTNTBE01L  if   e           k disk
002200110516
002300110516      // - Video Interrogazione anagrafica clienti potenziali
002400110516     fTRMK12D   cf   e             workstn
002500110516     f                                     indds(IndDspF)
002600110516     f                                     infds(InfDspF)
002700110516
002800110516      //---------------------------------------------------------------
002900110516      //?Definizione costanti.
003000110516      //---------------------------------------------------------------
003100110516
003200110516      // - Tasti funzionali a video
003300110516     d c_F01           c                   const(x'31')
003400110516     d c_F02           c                   const(x'32')
003500110516     d c_F03           c                   const(x'33')
003600110516     d c_F04           c                   const(x'34')
003700110516     d c_F05           c                   const(x'35')
003800110516     d c_F06           c                   const(x'36')
003900110516     d c_F07           c                   const(x'37')
004000110516     d c_F08           c                   const(x'38')
004100110516     d c_F09           c                   const(x'39')
004200110516     d c_F10           c                   const(x'3A')
004300110516     d c_F11           c                   const(x'3B')
004400110516     d c_F12           c                   const(x'3C')
004500110516     d c_F13           c                   const(x'B1')
004600110516     d c_F14           c                   const(x'B2')
004700110516     d c_F15           c                   const(x'B3')
004800110516     d c_F16           c                   const(x'B4')
004900110516     d c_F17           c                   const(x'B5')
005000110516     d c_F18           c                   const(x'B6')
005100110516     d c_F19           c                   const(x'B7')
005200110516     d c_F20           c                   const(x'B8')
005300110516     d c_F21           c                   const(x'B9')
005400110516     d c_F22           c                   const(x'BA')
005500110516     d c_F23           c                   const(x'BB')
005600110516     d c_F24           c                   const(x'BC')
005700110516     d c_Enter         c                   const(x'F1')
005800110516     d c_RollDown      c                   const(x'F4')
005900110516     d c_RollUp        c                   const(x'F5')
006000110517
006100110517     d Digits          c                   const('0123456789')
006200110516
006300110516      //---------------------------------------------------------------
006400110516      //?Definizione schiere.
006500110516      //---------------------------------------------------------------
006600110516
006700110516      // - Messaggi di errore
006800110516     d $Msg            s             78    dim(30) ctdata perrcd(1)
006900110516
007000110516      // - Importanza clienti da tabella IC
007100110516     d $IC             s              1    dim(10)
007200110516
007300110518      // - Attività
007400110518     d $CCO            s              2    dim(99)
007500110516
007600110516      // - Concorrenti possibili da tabella IFS
007700110523     d $CON            s              4    dim(30)
007800110523
007900110523      // - Categoria potenziale da tabella CPO
008000110523     d $CPO            s              1    dim(10)
008100110523     d $CPOS           s              1    dim(5)
008200110520
008300110520      // - Info comm. Ripartizione spesa --> "3T"
008400110520     d $IFO            s              3    dim(30)
008500110523
008600110523      // - Operatorie logistica tabella IFS
008700110523     d $LOG            s              4    dim(30)
008800110523
008900110523      // - Motivo non affidamento possibili da tabella IFS
009000110523     d $MOI            s              4    dim(30)
009100110523     d $DMOI           s             20    dim(30)
009200110516
009300110516      //---------------------------------------------------------------
009400110516      //?Definizione aree dati.
009500110516      //---------------------------------------------------------------
009600110516
009700110516      // - Dati utente
009800110516     d §AzUte        e ds                  extname(AZUTE00F)
009900110516     d                                     dtaara
010000110516     d §DatiUte      e ds                  extname(dDatiUte)
010100110516     d                                     dtaara
010200110516
010300110516      //---------------------------------------------------------------
010400110516      //?Definizione strutture dati.
010500110516      //---------------------------------------------------------------
010600110516
010700110516      // - Status
010800110516     d Psds           sds
010900110516     d   SDSpgm          *proc
011000110516
011100110516      // - InfDS
011200110516     d InfDspF         ds
011300110516     d  dsp_aid              369    369a
011400110516
011500110516      // - Indicatori su DspF
011600110516     d IndDspF         ds
011700110516        // - Abilitazione tasti funzionali
011800110516     d  F08Attivo                     1n   overlay(IndDspF : 08)
011900110516        // - Indicatori di errore
012000110516     d  ErrMessage                    1n   overlay(IndDspF : 28)
012100110517     d  PosCurPrv                     1n   overlay(IndDspF : 50)
012200110520     d  PosCurSpta                    1n   overlay(IndDspF : 50)
012300110525     d  PosCurOspt                    1n   overlay(IndDspF : 50)
012400110517     d  PosCurCap                     1n   overlay(IndDspF : 51)
012500110520     d  PosCurRip1                    1n   overlay(IndDspF : 51)
012600110525     d  PosCurOrag                    1n   overlay(IndDspF : 51)
012700110518     d  PosCurNaz                     1n   overlay(IndDspF : 52)
012800110520     d  PosCurRip2                    1n   overlay(IndDspF : 52)
012900110525     d  PosCurOcmmu                   1n   overlay(IndDspF : 52)
013000110518     d  PosCurCmmu                    1n   overlay(IndDspF : 53)
013100110520     d  PosCurRip3                    1n   overlay(IndDspF : 53)
013200110518     d  PosCurCar                     1n   overlay(IndDspF : 54)
013300110520     d  PosCurRipda                   1n   overlay(IndDspF : 54)
013400110525     d  PosCurOcmm                    1n   overlay(IndDspF : 54)
013500110518     d  PosCurFil                     1n   overlay(IndDspF : 55)
013600110520     d  PosCurRipa                    1n   overlay(IndDspF : 55)
013700110525     d  PosCurOcar                    1n   overlay(IndDspF : 55)
013800110518     d  PosCurSct                     1n   overlay(IndDspF : 56)
013900110520     d  PosCurPesa                    1n   overlay(IndDspF : 56)
014000110518     d  PosCurCic1                    1n   overlay(IndDspF : 57)
014100110520     d  PosCurPspa                    1n   overlay(IndDspF : 57)
014200110518     d  PosCurCic2                    1n   overlay(IndDspF : 58)
014300110520     d  PosCurBarda                   1n   overlay(IndDspF : 58)
014400110518     d  PosCurCic3                    1n   overlay(IndDspF : 59)
014500110520     d  PosCurBara                    1n   overlay(IndDspF : 59)
014600110518     d  PosCurCic4                    1n   overlay(IndDspF : 60)
014700110523     d  PosCurConc1                   1n   overlay(IndDspF : 60)
014800110518     d  PosCurCic5                    1n   overlay(IndDspF : 61)
014900110523     d  PosCurConc2                   1n   overlay(IndDspF : 61)
015000110518     d  PosCurFls1                    1n   overlay(IndDspF : 62)
015100110523     d  PosCurConc3                   1n   overlay(IndDspF : 62)
015200110518     d  PosCurFls2                    1n   overlay(IndDspF : 63)
015300110523     d  PosCurConc4                   1n   overlay(IndDspF : 63)
015400110518     d  PosCurFls3                    1n   overlay(IndDspF : 64)
015500110523     d  PosCurConc5                   1n   overlay(IndDspF : 64)
015600110518     d  PosCurFls4                    1n   overlay(IndDspF : 65)
015700110523     d  PosCurConda                   1n   overlay(IndDspF : 65)
015800110523     d  PosCurFls5                    1n   overlay(IndDspF : 66)
015900110523     d  PosCurCona                    1n   overlay(IndDspF : 66)
016000110523     d  PosCurCmm                     1n   overlay(IndDspF : 67)
016100110523     d  PosCurMoi1                    1n   overlay(IndDspF : 67)
016200110523     d  PosCurCco1                    1n   overlay(IndDspF : 68)
016300110523     d  PosCurMoi2                    1n   overlay(IndDspF : 68)
016400110523     d  PosCurCco2                    1n   overlay(IndDspF : 69)
016500110523     d  PosCurLogp                    1n   overlay(IndDspF : 69)
016600110523     d  PosCurCco3                    1n   overlay(IndDspF : 70)
016700110524     d  PosCurNifo                    1n   overlay(IndDspF : 70)
016800110523     d  PosCurCco4                    1n   overlay(IndDspF : 71)
016900110524     d  PosCurIfoal                   1n   overlay(IndDspF : 71)
017000110523     d  PosCurCco5                    1n   overlay(IndDspF : 72)
017100110523     d  PosCurDadd                    1n   overlay(IndDspF : 73)
017200110523     d  PosCurDada                    1n   overlay(IndDspF : 74)
017300110516     d  ErrGenerico                   1n   overlay(IndDspF : 99)
017400110516
017500110516     d WindDspF        ds                  inz
017600110516     d   WindDspFa             1     49    inz(*zero)
017700110516     d   WindDspFb            50     99    inz(*zero)
017800110516
017900110516      // Campo unico per Importanza clienti
018000110516     d V01cic          ds                  inz
018100110516     d   V01cic1                           inz
018200110516     d   V01cic2                           inz
018300110516     d   V01cic3                           inz
018400110516     d   V01cic4                           inz
018500110516     d   V01cic5                           inz
018600110516
018700110516      // Campo unico per Categoria potenziale
018800110516     d V01fls          ds                  inz
018900110516     d   V01fls1                           inz
019000110516     d   V01fls2                           inz
019100110516     d   V01fls3                           inz
019200110516     d   V01fls4                           inz
019300110518     d   V01fls5                           inz
019400110516
019500110516      // Campo unico per Prossima Attività
019600110516     d V01cco          ds                  inz
019700110516     d   V01cco1                           inz
019800110516     d   V01cco2                           inz
019900110516     d   V01cco3                           inz
020000110516     d   V01cco4                           inz
020100110516     d   V01cco5                           inz
020200110520
020300110520      // Campo unico per Ripartizione spesa
020400110520     d V02rip          ds                  inz
020500110520     d   V02rip1                           inz
020600110520     d   V02rip2                           inz
020700110520     d   V02rip3                           inz
020800110516
020900110516      // Campo unico per Concorrenti
021000110516     d V02conc         ds                  inz
021100110516     d   V02conc1                          inz
021200110516     d   V02conc2                          inz
021300110516     d   V02conc3                          inz
021400110516     d   V02conc4                          inz
021500110516     d   V02conc5                          inz
021600110524
021700110524      // Campo unico per Motivo non affidamento
021800110524     d V02moi          ds                  inz
021900110524     d   V02moi1                           inz
022000110524     d   V02moi2                           inz
022100110519
022200110519     d wlbdat          ds                  inz
022300110519     d  g02dat                 1      8  0
022400110519     d  g02inv                 9     16  0
022500110519     d  g02err                17     17
022600110519     d  g02tgi                18     22  0
022700110516
022800110516      // - Parametri ricevuti
022900110516     d KPJBA         e ds
023000110516     d TRMK13ds      e ds                  inz
023100110516
023200110516      // - Reperimento dati utente
023300110516     d TIBS34ds      e ds
023400110524     d dUTE01        e ds
023500110516     d TRUL31DS      e ds
023600110516
023700110516      // - controllo abilitazioni
023800110516     d TNTAA1DS      e ds                  inz
023900110520
024000110520      // - Ricerca ripartizione spesa
024100110520     d TNTB70DS      e ds                  inz
024200110516
024300110516      // - Ricerca tabella CPO
024400110520     d TNTB87DS      e ds                  inz
024500110523
024600110523      // - Tabella CPO = Categoria potenziale
024700110523     d dCPO          e ds                  inz
024800110520
024900110520      // Ripartizione spesa
025000110520     d dIFO          e ds
025100110523
025200110523      // Tabella sottocategorie IFS
025300110523     d dIFS          e ds
025400110516
025500110516      // - Tabella 05 = Codici Area
025600110516     d ds05          e ds                  inz
025700110516
025800110516      // - Tabella 1L = Categorie Merceologiche
025900110516     d ds1L          e ds                  inz
026000110516
026100110516      // - Controllo congruenza CAP e provincia
026200110516     d FNLV13ds      e ds
026300110516     d   I13af0      e                     inz('S')
026400110516     d   I13cng      e                     inz('S')
026500110516     d TISI95ds      e ds                  inz
026600110516     d   I95tcn      e                     inz('3')
026700110516
026800110516      //---------------------------------------------------------------
026900110516      //?Definizione variabili globali.
027000110516      //---------------------------------------------------------------
027100110516
027200110516      // - Flags booleani
027300110516     d $Fine           s               n   inz(*off)
027400110516     d $InzD01         s               n   inz(*on)
027500110516     d $InzD02         s               n   inz(*on)
027600110516     d $InzD03         s               n   inz(*on)
027700110516     d $ErrGrave       s               n   inz(*off)
027800110516
027900110516      // - Indici di schiera
028000110516     d xx              s              3  0 inz
028100110516     d yy              s              3  0 inz
028200110516
028300110516      // - Campi associati al video
028400110516     d $Video          s              2    inz('D1')
028500110516
028600110519      // - Campi di comodo per leggere i file tabelle
028700110516     d C_tab           s                   like(TBEcod)
028800110516     d C_ke1           s                   like(TBEke1)
028900110516
029000110516     d idTabella       s              2
029100110516     d Ordinamento     s              1
029200110516     d idElemento      s              8
029300110516     d TastoUscita     s              1
029400110519
029500110519      // - Campi di comodo data
029600110524     d wdadd           s              8  0 inz
029700110519     d wdada           s              8  0 inz
029800110524     d wifoal          s              8  0 inz
029900110523
030000110523      // - Campi di comodo
030100110523     d wConta          s              2  0 inz
030200110516
030300110516      //---------------------------------------------------------------
030400110516      //?Definizione procedure usate.
030500110516      //---------------------------------------------------------------
030600110524
030700110524      // - Sottomissione lavoro batch
030800110524     d bch10           pr                  extpgm('BCH10')
030900110524     d  kpjba                              likeds(KPJBA)
031000110516
031100110516      // - Controllo cap/località
031200110516     d fnlv13r         pr                  extpgm('FNLV13R')
031300110516     d  kpjba                              likeds(kpjba)    const
031400110516     d  fnlv13ds                           likeds(FNLV13ds)
031500110516     d  tisi95ds                           likeds(TISI95ds)
031600110520
031700110520      // - Interrogazione ripartizione spesa
031800110520     d TNTB70R         pr                  extpgm('TNTB70R')
031900110520     d  kpjba                              likeds(kpjba)
032000110520     d  tntb70ds                           likeds(TNTB70ds)
032100110516
032200110520      // - Interrogazione tabella CPO
032300110516     d TNTB87R         pr                  extpgm('TNTB87R')
032400110516     d  tntb87ds                           likeds(TNTB87ds)
032500110524
032600110524      // - Elaborazione
032700110524     d trmk13r         pr                  extpgm('TRMK13R')
032800110524     d  kpjba                              likeds(kpjba)    const
032900110516
033000110516      //---------------------------------------------------------------
033100110517      //?Prototipi.
033200110516      //---------------------------------------------------------------
033300110516      /copy gaitrasrc/srcprotopr,tibs34r
033400130808      /copy gaitrasrc/srcprotoPI,TRMK43R_1
033500130808      /copy gaitrasrc/srcprotoPR,TRMK43R
033600110516      /copy gaitrasrc/srcprotopr,tntaa1c
033700110516      /copy gaitrasrc/srcprotopr,trul19r
033800110516      /copy gaitrasrc/srcprotopr,trul31r
033900110519      /copy gaitrasrc/srcprotopr,xsrda8
034000110516
034100110516      //---------------------------------------------------------------
034200110516      //?Definizione key-list.
034300110516      //---------------------------------------------------------------
034400110517
034500110517       // - File TABEL00F
034600110517     d k03tabel      e ds                  extname(TABEL00F:*key)
034700110517     d                                     prefix(k_)
034800110517     d                                     inz
034900110516
035000110516      //---------------------------------------------------------------
035100110516      //?M A I N - L I N E
035200110516      //---------------------------------------------------------------
035300110516
035400110516     c     *Entry        plist
035500110516     c                   parm                    KPJBA
035600110516
035700110516      /free
035800110516
035900110516       //?Operazioni iniziali
036000110516       exsr RoutInz;
036100110516
036200110516       //?Gestione video
036300110516       DOW $Fine = *off;
036400110516         select;
036500110516           when $Video = 'D1';
036600110516             exsr GesD01;
036700110516           when $Video = 'D2';
036800110519             exsr GesD02;
036900110516           when $Video = 'D3';
037000110516             exsr GESD03;
037100110516           other;
037200110516             $Fine = *on;
037300110516         endsl;
037400110516       ENDDO;
037500110516
037600110516       //?Operazioni finali
037700110516       exsr RoutEnd;
037800110516
037900110516       //--------------------------------------------------------------
038000110516       //?Operazioni iniziali.
038100110516       //--------------------------------------------------------------
038200110516       BEGSR RoutInz;
038300110516
038400110516       //?Impostazione campi "fissi"
038500110516         T01pgm = SDSpgm;
038600110516
038700110516       //?Reperimento dati job
038800110516         exsr DatiJob;
038900110516
039000110516       //?Controllo se ok l'abilitazione dell'utente
039100110516         reset TNTAA1DS;
039200110516         ITAA1caut = '§utepot';
039300110516         clear kpjbu;
039400110516         kpjbu = TNTAA1DS;
039500110516         tntaa1c (kpjba);
039600110516         TNTAA1DS = kpjbu;
039700110516
039800110524         IF  OTAA1fabi = 'N';
039900110516           $ErrGrave = *on;
040000110524           leavesr;
040100110524         ENDIF;
040200110516
040300110524         clear TRUL31DS;
040400110524         I31abi = OTAA1cabi;
040500110524         I31cdi = DUTdis;
040600110524         I31car = DUTare;
040700110524         I31cpo = DUTpou;
040800110524         callp TRUL31R (kpjba:trul31ds);
040900110524         IF  O31pog <= *zeros;
041000110524           $ErrGrave = *on;
041100110524           leavesr;
041200110524         ENDIF;
041300110524
041400110524         dUTE01 = UTEfaf;
041500110524
041600110524         //?Abilito stampa info commerciale in base al profilo utente
041700110524         F08Attivo = (§UTEpotst = 'S');
041800110516
041900110518         //?Carico sk Codici Importanza
042000110516         clear xx;
042100110517         TBLkut = 1;
042200110516         TBLcod = 'IC';
042300110516         setll (TBLkut:TBLcod) TABEL00F;
042400110516         reade (TBLkut:TBLcod) TABEL00F;
042500110516
042600110516         DOW not %eof(TABEL00F);
042700110516           IF  TBLflg = *blanks;
042800110516             xx += 1;
042900110520             $IC(xx) = %subst(TBLkey:1:1);
043000110516           ENDIF;
043100110516           reade (TBLkut:TBLcod) TABEL00F;
043200110516         ENDDO;
043300110518
043400110518         //?Carico sk Attività
043500110518         clear xx;
043600110518         clear yy;
043700110518         C_Tab = 'CCO';
043800110518         setll (C_tab) TNTBE01L;
043900110518         reade (C_tab) TNTBE01L;
044000110518
044100110518         DOW not %eof(TNTBE01L);
044200110518           IF  TBEatb = *blanks;
044300110518             xx += 1;
044400110524             $CCO(xx) = %subst(TBEke1:1:3);
044500110518           ENDIF;
044600110518           reade (C_tab) TNTBE01L;
044700110518         ENDDO;
044800110516
044900110516         //?Carico sk Categoria potenziali
045000110516         clear xx;
045100110516         clear yy;
045200110516         C_Tab = 'CPO';
045300110516         setll (C_tab) TNTBE01L;
045400110516         reade (C_tab) TNTBE01L;
045500110516
045600110516         DOW not %eof(TNTBE01L);
045700110516           IF  TBEatb = *blanks;
045800110523             dCPO = TBEuni;
045900110516             xx += 1;
046000110520             $CPO(xx) = %subst(TBEke1:1:1);
046100110523             IF  §CPOstp = 'S';
046200110523               yy += 1;
046300110523               $CPOS(yy) = $CPO(xx);
046400110523             ENDIF;
046500110516           ENDIF;
046600110516           reade (C_tab) TNTBE01L;
046700110516         ENDDO;
046800110516
046900110516         //?Carico sk Concorrenti
047000110516         clear xx;
047100110516         C_Tab = 'IFS';
047200110516         C_ke1 = '10';
047300110516         setll (C_tab:C_ke1) TNTBE01L;
047400110516         reade (C_tab:C_ke1) TNTBE01L;
047500110516
047600110516         DOW not %eof(TNTBE01L);
047700110516           IF  TBEatb = *blanks;
047800110516             xx += 1;
047900110523             $CON(xx) = %subst(TBEke2:1:4);
048000110516           ENDIF;
048100110516           reade (C_tab:C_ke1) TNTBE01L;
048200110516         ENDDO;
048300110523
048400110523         //?Carico sk Operatori Logistica
048500110523         clear xx;
048600110523         C_Tab = 'IFS';
048700110523         C_ke1 = '40';
048800110523         setll (C_tab:C_ke1) TNTBE01L;
048900110523         reade (C_tab:C_ke1) TNTBE01L;
049000110523
049100110523         DOW not %eof(TNTBE01L);
049200110523           IF  TBEatb = *blanks;
049300110523             xx += 1;
049400110523             $LOG(xx) = %subst(TBEke2:1:4);
049500110523           ENDIF;
049600110523           reade (C_tab:C_ke1) TNTBE01L;
049700110523         ENDDO;
049800110523
049900110523         //?Carico sk Motivo non affidamento
050000110523         clear xx;
050100110523         C_Tab = 'IFS';
050200110523         C_ke1 = 'MOI';
050300110523         setll (C_tab:C_ke1) TNTBE01L;
050400110523         reade (C_tab:C_ke1) TNTBE01L;
050500110523
050600110523         DOW not %eof(TNTBE01L);
050700110523           IF  TBEatb = *blanks;
050800110523             xx += 1;
050900110523             $MOI(xx) = %subst(TBEke2:1:4);
051000110523             dIFS = TBEuni;
051100110523             $DMOI(xx) = §IFSdes;
051200110523           ENDIF;
051300110523           reade (C_tab:C_ke1) TNTBE01L;
051400110523         ENDDO;
051500110520
051600110520         //?Carico sk Ripartizione spesa (tipo 3T)
051700110520         clear xx;
051800110520         C_Tab = 'IFO';
051900110520         setll (C_tab) TNTBE01L;
052000110520         reade (C_tab) TNTBE01L;
052100110520
052200110520         DOW not %eof(TNTBE01L);
052300110520           IF  TBEatb = *blanks;
052400110520             dIFO = TBEuni;
052500110520             IF  §IFOifg = '3T';
052600110520               xx += 1;
052700110520               $IFO(xx) = %subst(TBEke1:1:3);
052800110520             ENDIF;
052900110520           ENDIF;
053000110520           reade (C_tab) TNTBE01L;
053100110520         ENDDO;
053200110516
053300110516       ENDSR;
053400080206
053500080206       //--------------------------------------------------------------
053600110517       //?Gestione videata D01.
053700080206       //--------------------------------------------------------------
053800080206       BEGSR GesD01;
053900080206
054000110517       //?Inizializzazione videata
054100110517         IF  $InzD01;
054200080206           exsr InzD01;
054300080206           $InzD01 = *off;
054400110517         ENDIF;
054500080206
054600110517       //?Emissione testata
054700110517         write MK12T01;
054800090702
054900110517       //?Se utente non abilitato messaggio ed esco dal pgm
055000110517         IF  $ErrGrave;
055100110517           ErrMessage  = *on;
055200110517           ErrGenerico = *on;
055300110517           V01msg = $Msg(01);
055400110517         ENDIF;
055500080206
055600110517       //?Emissione videata
055700110517         exfmt MK12D01;
055800110517         ErrMessage   = *off;
055900110517         ErrGenerico  = *off;
056000110517         clear V01msg;
056100080207
056200110517         SELECT;
056300110517
056400110517       //?Errore grave qualsiasi tasto venga premuto esce dal pgm
056500110517           WHEN  $ErrGrave;
056600080206             $Fine  = *on;
056700110308
056800110517       //?F3=Fine
056900110517           WHEN  dsp_aid = c_F03;
057000080206             exsr F03D01;
057100110308
057200110517       //?F06=Parametri di stampa
057300110517           WHEN  dsp_aid = c_F06;
057400110524             exsr CtrD01;
057500110524             IF  ErrGenerico;
057600110524               leavesr;
057700110524             ENDIF;
057800110517             exsr F06D01;
057900110308
058000110517       //?F08=Selezione Info Commerciali
058100110517           WHEN  dsp_aid = c_F08;
058200110524             exsr CtrD01;
058300110524             IF  ErrGenerico;
058400110524               leavesr;
058500110524             ENDIF;
058600110517             exsr F08D01;
058700110517
058800110517       //?Enter
058900110517           OTHER;
059000080206             exsr CtrD01;
059100110517             IF  ErrGenerico;
059200080206               leavesr;
059300110517             ENDIF;
059400110308
059500110517         ENDSL;
059600080206
059700080206       ENDSR;
059800110519
059900110519       //--------------------------------------------------------------
060000110519       //?Gestione videata D02.
060100110519       //--------------------------------------------------------------
060200110519       BEGSR GesD02;
060300110519
060400110519       //?Inizializzazione videata
060500110519         IF  $InzD02;
060600110519           exsr InzD02;
060700110519           $InzD02 = *off;
060800110519         ENDIF;
060900110519
061000110519       //?Emissione testata
061100110519         write MK12T01;
061200110519
061300110519       //?Emissione videata
061400110519         exfmt MK12D02;
061500110519         ErrMessage   = *off;
061600110519         ErrGenerico  = *off;
061700110519         clear V02msg;
061800110519
061900110519         SELECT;
062000110519
062100110519       //?F06=Parametri di stampa
062200110519           WHEN  dsp_aid = c_F06;
062300110524             exsr CtrD02;
062400110524             IF  ErrGenerico;
062500110524               leavesr;
062600110524             ENDIF;
062700110519             exsr F06D01;
062800110519
062900110519       //?F12=Ritorno
063000110519           WHEN  dsp_aid = c_F12;
063100110519             exsr F12D02;
063200110519
063300110519       //?Enter
063400110519           OTHER;
063500110519             exsr CtrD02;
063600110519             IF  ErrGenerico;
063700110519               leavesr;
063800110519             ENDIF;
063900110519
064000110519         ENDSL;
064100110519
064200110519       ENDSR;
064300110519
064400110519       //--------------------------------------------------------------
064500110519       //?Gestione videata D03.
064600110519       //--------------------------------------------------------------
064700110519       BEGSR GesD03;
064800110519
064900110519       //?Inizializzazione videata
065000110519         IF  $InzD03;
065100110519           exsr InzD03;
065200110519           $InzD03 = *off;
065300110519         ENDIF;
065400110519
065500110519       //?Emissione testata
065600110519         write MK12T01;
065700110519
065800110519       //?Emissione videata
065900110519         exfmt MK12D03;
066000110519         ErrMessage   = *off;
066100110519         ErrGenerico  = *off;
066200110519         clear V03msg;
066300110519
066400110519         SELECT;
066500110519
066600110519       //?F06=Elabora
066700110519           WHEN  dsp_aid = c_F06;
066800110524             exsr CtrD03;
066900110524             IF  ErrGenerico;
067000110524               leavesr;
067100110524             ENDIF;
067200110519             exsr F06D03;
067300110519
067400110519       //?F12=Ritorno
067500110519           WHEN  dsp_aid = c_F12;
067600110519             exsr F12D03;
067700110519
067800110519       //?Enter
067900110519           OTHER;
068000110524             exsr CtrD03;
068100110519             IF  ErrGenerico;
068200110519               leavesr;
068300110519             ENDIF;
068400110519
068500110519         ENDSL;
068600110519
068700110519       ENDSR;
068800110519
068900110519       //--------------------------------------------------------------
069000110519       //?Operazioni finali.
069100110519       //--------------------------------------------------------------
069200110519       BEGSR RoutEnd;
069300110519
069400110519         *inLR = *on;
069500110519         return;
069600110519
069700110519       ENDSR;
069800110519
069900110519       //--------------------------------------------------------------
070000110519       //?Reperimento Dati del job (Utente/Operativi).
070100110519       //--------------------------------------------------------------
070200110519       BEGSR DatiJob;
070300110519
070400110519         in(E) §AzUte;
070500110519         if NOT %error;
070600110519           in(E) §DatiUte;
070700110519         endif;
070800110519         if %error or RSut = *blanks;
070900110519           clear TIBS34ds;
071000110519           tibs34r(tibs34ds);
071100110519           in §AzUte;
071200110519           in §DatiUte;
071300110519         endif;
071400110519
071500110519       ENDSR;
071600080206
071700080206       //--------------------------------------------------------------
071800110517       //?Inizializzazione videata D01.
071900080206       //--------------------------------------------------------------
072000080206       BEGSR InzD01;
072100080206
072200110517         clear MK12D01;
072300110517
072400110517         V01fsf = 'S';
072500110523
072600110523         //?Imposto le categorie potenziale da selezionare di default
072700110523         //?Mai codificato - Codificato - Perso
072800110523         V01fls1 = $CPOS(1);
072900110523         V01fls2 = $CPOS(2);
073000110523         V01fls3 = $CPOS(3);
073100110523         V01fls4 = $CPOS(4);
073200110523         V01fls5 = $CPOS(5);
073300080206
073400080206       ENDSR;
073500080206
073600080206       //--------------------------------------------------------------
073700110517       //?Gestione tasto funzionale F3 da videata D01.
073800080206       //?F3=Fine
073900080206       //--------------------------------------------------------------
074000080206       BEGSR F03D01;
074100080206
074200080207         // Chiusura del programma
074300080206         $Fine = *on;
074400080206
074500080206       ENDSR;
074600080206
074700080206       //--------------------------------------------------------------
074800110517       //?Gestione tasto funzionale F6 da videata D01.
074900110517       //?F06=Parametri di stampa
075000080206       //--------------------------------------------------------------
075100110517       BEGSR F06D01;
075200080206
075300110517         $InzD01 = *off;
075400110519         $InzD02 = *off;
075500110517         $InzD03 = *on;
075600110517         $Video = 'D3';
075700080206
075800080206       ENDSR;
075900110517
076000110517       //--------------------------------------------------------------
076100110517       //?Gestione tasto funzionale F8 da videata D01.
076200110517       //?F08=Selezione Info Commerciali
076300110517       //--------------------------------------------------------------
076400110517       BEGSR F08D01;
076500110517
076600110517         $InzD01 = *off;
076700110517         $InzD02 = *on;
076800110519         $InzD03 = *off;
076900110517         $Video = 'D2';
077000110517
077100110517       ENDSR;
077200080206
077300080206       //--------------------------------------------------------------
077400110519       //?Controllo videata D01.
077500080206       //--------------------------------------------------------------
077600080206       BEGSR CtrD01;
077700080206
077800080206         WindDspF = IndDspF;
077900080206         reset WindDspFb;
078000080206         IndDspF  = WindDspF;
078100110308
078200110517       //?Controllo se richiesto almeno 1 dei dati obbligatori
078300110530         IF  V01cmmu = *blanks and
078400110530             V01prv = *blanks and V01cap = *blanks and
078500110517             V01naz = *blanks and V01loc = *blanks and
078600110524             V01car = *blanks and V01fil = *zeros;
078700110517           ErrMessage  = *on;
078800110517           ErrGenerico = *on;
078900110530           PosCurCmmu  = *on;
079000110517           V01msg      = $Msg(02);
079100110517           leavesr;
079200110517         ENDIF;
079300110530
079400110530       //?COMMERCIALE UNIFICANTE
079500110530         IF  V01cmmu <> *blanks;
079600110530           clear V01dcmmu;
079700110530
079800110530         //?Ricerca
079900110530           IF  %scan('?' : V01cmmu) > 0;
080000110530             ErrGenerico = *on;
080100110530             PosCurCmmu  = *on;
080200130808             clear  TRMK43ds;
080300130808             iMK43solU = 'S';
080400130808             iMK43fil  = DUTpou;
080500130808             TRMK43R (kpjba : TRMK43ds);
080600130808             if  oMK43err = *off  and  oMK43fxx = *blank;
080700130808               V01cmmu  = %editc( oMK43cmm : 'X' );
080800130808               V01dcmmu = oMK43des;
080900130808             endif;
081000110530           ENDIF;
081100110530
081200110530         //?Solo numeri
081300110530           IF  %check(digits:V01cmmu) > 0;
081400110530             ErrMessage  = *on;
081500110530             ErrGenerico = *on;
081600110530             PosCurCmmu  = *on;
081700110530             V01msg      = $Msg(05);
081800110530             leavesr;
081900110530           ENDIF;
082000110530
082100110530         //?Controllo e decodifico
082200130808           CMMcod = %int(V01cmmu);
082300130808           chain  (CMMcod)  AZCMM000;
082400130808           IF  not %found( AZCMM01L );
082500110530             ErrMessage  = *on;
082600110530             ErrGenerico = *on;
082700110530             PosCurCmmu  = *on;
082800110530             V01msg      = $Msg(05);
082900110530             leavesr;
083000110530           ENDIF;
083100110530
083200110530         //?utente abilitato al commerciale
083300110530           clear TNTAA1DS;
083400110530           ITAA1caut = '§utegtc';
083500110530           ITAA1cmm  = %dec(V01cmmu:7:0);
083600110530           kpjbu     = tntaa1ds;
083700110530           tntaa1c (kpjba);
083800110530           TNTAA1DS = kpjbu;
083900110530           IF  OTAA1fabi = 'N';
084000110530             ErrMessage  = *on;
084100110530             ErrGenerico = *on;
084200110530             PosCurCmmu  = *on;
084300110530             V01msg      = $Msg(05);
084400110530             leavesr;
084500110530           ENDIF;
084600110530
084700130808           V01dcmmu = CMMdes;
084800110530
084900110530         //?Controllo che non abbia particolarità.
085000130808           IF  CMMpar <> *blanks;
085100110530             ErrMessage  = *on;
085200110530             ErrGenerico = *on;
085300110530             PosCurCmmu  = *on;
085400110530             V01msg      = $Msg(05);
085500110530             leavesr;
085600110530           ENDIF;
085700110530
085800110530         //?Deve essere un commerciale unificante
085900130808           IF  %dec(V01cmmu:7:0) <> CMMuni;
086000110530             ErrMessage  = *on;
086100110530             ErrGenerico = *on;
086200110530             PosCurCmmu  = *on;
086300110530             V01msg      = $Msg(06);
086400110530             leavesr;
086500110530           ENDIF;
086600110530         ENDIF;
086700110517
086800110517       //?INDIRIZZO
086900110517       //?Se inserito il cap o la località è obbligatoria la provincia
087000110517         IF  (V01cap <> *blanks or  V01loc <> *blanks)  and
087100110524              V01prv  = *blanks and V01naz = *blanks;
087200110517           ErrMessage  = *on;
087300110517           ErrGenerico = *on;
087400110517           PosCurPrv   = *on;
087500110517           V01msg      = $Msg(03);
087600110517           leavesr;
087700110517         ENDIF;
087800110517
087900110517       //?Se inserito il cap e la località controllo se sono congruenti
088000110518       //?anche con la nazione
088100110517         IF  V01cap <> *blanks;
088200110517           reset FNLV13DS;
088300110517           reset TISI95DS;
088400110517           I95cap = V01cap;
088500110517           I95prv = V01prv;
088600110518           I95nar = V01naz;
088700110517           fnlv13r (kpjba : fnlv13ds : tisi95ds);
088800110517           IF  O13err <> *blanks;
088900110517             ErrMessage  = *on;
089000110517             ErrGenerico = *on;
089100110517             SELECT;
089200110517               WHEN  O13err = '4';
089300110517                 PosCurPrv   = *on;
089400110517               WHEN  O13err = '5';
089500110517                 PosCurCap   = *on;
089600110518               WHEN  O13err = '6';
089700110518                 PosCurNaz   = *on;
089800110517             ENDSL;
089900110517             V01msg = O13msg;
090000110517             leavesr;
090100110517           ENDIF;
090200110517         ENDIF;
090300110518
090400110518       //?NAZIONE
090500110518         IF  V01naz <> *blanks;
090600110518
090700110518         //?Ricerca
090800110518           IF  %scan('?' : V01naz) > 0;
090900110518             ErrGenerico = *on;
091000110518             PosCurNaz   = *on;
091100110524             idTabella = '15';
091200110518             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
091300110518                            tastoUscita);
091400110524             V01naz = idElemento;
091500110518           ENDIF;
091600110518
091700110518         //?Controllo
091800110518           k_TBLkut = 1;
091900110518           k_TBLcod = '15';
092000110518           k_TBLkey = V01naz;
092100110518           chain  %kds(K03tabel) TABEL00F;
092200110518           IF  not %found( TABEL00F ) or TBLflg <> *blanks;
092300110518             ErrMessage  = *on;
092400110518             ErrGenerico = *on;
092500110518             PosCurNaz   = *on;
092600110518             V01msg      = $Msg(04);
092700110518             leavesr;
092800110518           ENDIF;
092900110518         ENDIF;
093000110517
093100110517       //?AREA
093200110517         IF  V01car <> *blanks;
093300110517           clear V01dcar;
093400110517
093500110517         //?Ricerca
093600110517           IF  %scan('?' : V01car) > 0;
093700110517             ErrGenerico = *on;
093800110517             PosCurCar   = *on;
093900110517             idTabella = '05';
094000110517             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
094100110517                            tastoUscita);
094200110517             V01car = idElemento;
094300110517           ENDIF;
094400110517
094500110517         //?Solo numeri
094600110517           IF  %check(digits:V01car) > 0;
094700110517             ErrMessage  = *on;
094800110517             ErrGenerico = *on;
094900110517             PosCurCar   = *on;
095000110518             V01msg      = $Msg(07);
095100110517             leavesr;
095200110517           ENDIF;
095300110517
095400110517         //?Controllo e decodifico
095500110517           clear ds05;
095600110517           k_TBLkut = 1;
095700110517           k_TBLcod = '05';
095800110517           k_TBLkey = V01car;
095900110517           chain  %kds(K03tabel) TABEL00F;
096000110517           IF  not %found( TABEL00F ) or TBLflg <> *blanks;
096100110517             ErrMessage  = *on;
096200110517             ErrGenerico = *on;
096300110517             PosCurCar   = *on;
096400110518             V01msg      = $Msg(07);
096500110517             leavesr;
096600110517           ENDIF;
096700110517
096800110517           ds05    = TBLuni;
096900110517           V01dcar = §05des;
097000110517         ENDIF;
097100110518
097200110518       //?FILIALE APPARTENENZA
097300110518         IF  V01fil > 0;
097400110518           clear V01dfil;
097500110518           chain V01fil AZORG01L;
097600110518           IF  not %found(AZORG01L) or ORGfva <> *blanks;
097700110518             ErrMessage  = *on;
097800110518             ErrGenerico = *on;
097900110518             PosCurFil   = *on;
098000110518             V01msg      = $Msg(08);
098100110518             leavesr;
098200110518           ENDIF;
098300110518           V01dfil = ORGdes;
098400110518         ENDIF;
098500110518
098600110518       //?CATEGORIA MERCEOLOGICA
098700110518         IF  V01sct <> *blanks;
098800110518           clear V01dsct;
098900110518
099000110518         //?Ricerca
099100110518           IF  %scan('?' : V01sct) > 0;
099200110518             ErrGenerico = *on;
099300110518             PosCurSct   = *on;
099400110518             idTabella = '1L';
099500110518             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
099600110518                            tastoUscita);
099700110518             V01sct = idElemento;
099800110518           ENDIF;
099900110518
100000110518         //?Solo numeri
100100110518           IF  %check(digits:V01sct) > 0;
100200110518             ErrMessage  = *on;
100300110518             ErrGenerico = *on;
100400110518             PosCurSct   = *on;
100500110518             V01msg      = $Msg(09);
100600110518             leavesr;
100700110518           ENDIF;
100800110518
100900110518         //?Controllo e decodifico
101000110518           clear ds1L;
101100110518           k_TBLkut = 1;
101200110518           k_TBLcod = '1L';
101300110518           k_TBLkey = V01sct;
101400110518           chain  %kds(K03tabel) TABEL00F;
101500110518           IF  not %found( TABEL00F ) or TBLflg <> *blanks;
101600110518             ErrMessage  = *on;
101700110518             ErrGenerico = *on;
101800110518             PosCurSct   = *on;
101900110518             V01msg      = $Msg(09);
102000110518             leavesr;
102100110518           ENDIF;
102200110518
102300110518           ds1L    = TBLuni;
102400110518           V01dsct = §1Ldes;
102500110518         ENDIF;
102600110308
102700110518       //?IMPORTANZA CLIENTE
102800110518         IF  V01cic1 <> *blanks;
102900110518           IF  %scan('?' : V01cic1) > 0;
103000110308             ErrGenerico = *on;
103100110308             PosCurCic1  = *on;
103200110308             idTabella = 'IC';
103300110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
103400110308                            tastoUscita);
103500110309             V01cic1 = idElemento;
103600110308           ENDIF;
103700110518           IF  %lookup(V01cic1 : $Ic) = 0;
103800110308             ErrMessage  = *on;
103900110308             ErrGenerico = *on;
104000110308             PosCurCic1  = *on;
104100110518             V01msg = $Msg(10);
104200110308             leavesr;
104300110308           ENDIF;
104400110518         ENDIF;
104500110518         IF  V01cic2 <> *blanks;
104600110518           IF  %scan('?' : V01cic2) > 0;
104700110308             ErrGenerico = *on;
104800110308             PosCurCic2  = *on;
104900110308             idTabella = 'IC';
105000110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
105100110308                            tastoUscita);
105200110308             V01cic2 = idElemento;
105300110308           ENDIF;
105400110518           IF  %lookup(V01cic2 : $Ic) = 0;
105500110308             ErrMessage  = *on;
105600110308             ErrGenerico = *on;
105700110308             PosCurCic2  = *on;
105800110518             V01msg = $Msg(10);
105900110308             leavesr;
106000110308           ENDIF;
106100110518         ENDIF;
106200110518         IF  V01cic3 <> *blanks;
106300110518           IF  %scan('?' : V01cic3) > 0;
106400110308             ErrGenerico = *on;
106500110308             PosCurCic3  = *on;
106600110308             idTabella = 'IC';
106700110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
106800110308                            tastoUscita);
106900110308             V01cic3 = idElemento;
107000110308           ENDIF;
107100110308           IF  %lookup(V01cic3 : $Ic) = 0;
107200110308             ErrMessage  = *on;
107300110308             ErrGenerico = *on;
107400110308             PosCurCic3  = *on;
107500110518             V01msg = $Msg(10);
107600110308             leavesr;
107700110308           ENDIF;
107800110308         ENDIF;
107900110308         IF  V01cic4 <> *blanks;
108000110518           IF  %scan('?' : V01cic4) > 0;
108100110308             ErrGenerico = *on;
108200110308             PosCurCic4  = *on;
108300110308             idTabella = 'IC';
108400110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
108500110308                            tastoUscita);
108600110308             V01cic4 = idElemento;
108700110308           ENDIF;
108800110308           IF  %lookup(V01cic4 : $Ic) = 0;
108900110308             ErrMessage  = *on;
109000110308             ErrGenerico = *on;
109100110308             PosCurCic4  = *on;
109200110518             V01msg = $Msg(10);
109300110308             leavesr;
109400110308           ENDIF;
109500110308         ENDIF;
109600110308         IF  V01cic5 <> *blanks;
109700110518           IF  %scan('?' : V01cic5) > 0;
109800110308             ErrGenerico = *on;
109900110308             PosCurCic5  = *on;
110000110308             idTabella = 'IC';
110100110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
110200110308                            tastoUscita);
110300110308             V01cic5 = idElemento;
110400110308           ENDIF;
110500110308           IF  %lookup(V01cic5 : $Ic) = 0;
110600110308             ErrMessage  = *on;
110700110308             ErrGenerico = *on;
110800110308             PosCurCic5  = *on;
110900110518             V01msg = $Msg(10);
111000110308             leavesr;
111100110308           ENDIF;
111200110308         ENDIF;
111300110518
111400110518       //?CATEGORIA POTENZIALE
111500110407         IF  V01fls1 <> *blanks;
111600110407           IF  %scan('?' : V01fls1) > 0;
111700110308             ErrGenerico = *on;
111800110407             PosCurFls1  = *on;
111900110509             clear TNTB87ds;
112000110509             clear V01fls1;
112100110509             tntb87r  (tntb87ds);
112200110509             IF  TB87fxx = *blanks and TB87err = *off;
112300110509               V01fls1 = TB87cod;
112400110308             ENDIF;
112500110308           ENDIF;
112600110407           IF  %lookup(V01fls1 : $Cpo) = 0;
112700110314             ErrMessage  = *on;
112800110314             ErrGenerico = *on;
112900110407             PosCurFls1  = *on;
113000110518             V01msg = $Msg(11);
113100110314             leavesr;
113200110308           ENDIF;
113300110308         ENDIF;
113400110407         IF  V01fls2 <> *blanks;
113500110407           IF  %scan('?' : V01fls2) > 0;
113600110407             ErrGenerico = *on;
113700110407             PosCurFls2  = *on;
113800110509             clear TNTB87ds;
113900110509             clear V01fls2;
114000110509             tntb87r  (tntb87ds);
114100110509             IF  TB87fxx = *blanks and TB87err = *off;
114200110509               V01fls2 = TB87cod;
114300110509             ENDIF;
114400110407           ENDIF;
114500110407           IF  %lookup(V01fls2 : $Cpo) = 0;
114600110407             ErrMessage  = *on;
114700110407             ErrGenerico = *on;
114800110407             PosCurFls2  = *on;
114900110518             V01msg = $Msg(11);
115000110407             leavesr;
115100110407           ENDIF;
115200110407         ENDIF;
115300110407         IF  V01fls3 <> *blanks;
115400110407           IF  %scan('?' : V01fls3) > 0;
115500110407             ErrGenerico = *on;
115600110407             PosCurFls3  = *on;
115700110509             clear TNTB87ds;
115800110509             clear V01fls3;
115900110509             tntb87r  (tntb87ds);
116000110509             IF  TB87fxx = *blanks and TB87err = *off;
116100110509               V01fls3 = TB87cod;
116200110509             ENDIF;
116300110407           ENDIF;
116400110407           IF  %lookup(V01fls3 : $Cpo) = 0;
116500110407             ErrMessage  = *on;
116600110407             ErrGenerico = *on;
116700110407             PosCurFls3  = *on;
116800110518             V01msg = $Msg(11);
116900110407             leavesr;
117000110407           ENDIF;
117100110407         ENDIF;
117200110407         IF  V01fls4 <> *blanks;
117300110407           IF  %scan('?' : V01fls4) > 0;
117400110407             ErrGenerico = *on;
117500110407             PosCurFls4  = *on;
117600110509             clear TNTB87ds;
117700110509             clear V01fls4;
117800110509             tntb87r  (tntb87ds);
117900110509             IF  TB87fxx = *blanks and TB87err = *off;
118000110509               V01fls4 = TB87cod;
118100110509             ENDIF;
118200110407           ENDIF;
118300110407           IF  %lookup(V01fls4 : $Cpo) = 0;
118400110407             ErrMessage  = *on;
118500110407             ErrGenerico = *on;
118600110407             PosCurFls4  = *on;
118700110518             V01msg = $Msg(11);
118800110407             leavesr;
118900110407           ENDIF;
119000110407         ENDIF;
119100110407         IF  V01fls5 <> *blanks;
119200110407           IF  %scan('?' : V01fls5) > 0;
119300110407             ErrGenerico = *on;
119400110407             PosCurFls5  = *on;
119500110509             clear TNTB87ds;
119600110509             clear V01fls5;
119700110509             tntb87r  (tntb87ds);
119800110509             IF  TB87fxx = *blanks and TB87err = *off;
119900110509               V01fls5 = TB87cod;
120000110509             ENDIF;
120100110407           ENDIF;
120200110407           IF  %lookup(V01fls5 : $Cpo) = 0;
120300110407             ErrMessage  = *on;
120400110407             ErrGenerico = *on;
120500110407             PosCurFls5  = *on;
120600110518             V01msg = $Msg(11);
120700110407             leavesr;
120800110407           ENDIF;
120900110407         ENDIF;
121000110518
121100110518       //?COMMERCIALE PROSSIMA ATTIVITA'
121200110530         IF  V01cmm <> *blanks;
121300110518           clear V01dcmm;
121400110518
121500110518         //?Ricerca
121600110518           IF  %scan('?' : V01cmm) > 0;
121700110518             ErrGenerico = *on;
121800110518             PosCurCmm   = *on;
121900130808             clear  TRMK43ds;
122000130808             iMK43solU = 'S';
122100130808             iMK43fil  = DUTpou;
122200130808             TRMK43R (kpjba : TRMK43ds);
122300130808             if  oMK43err = *off  and  oMK43fxx = *blank;
122400130808               V01cmm  = %editc( oMK43cmm : 'X' );
122500130808               V01dcmm = oMK43des;
122600130808             endif;
122700110518           ENDIF;
122800110518
122900110518         //?Solo numeri
123000110518           IF  %check(digits:V01cmm) > 0;
123100110518             ErrMessage  = *on;
123200110518             ErrGenerico = *on;
123300110518             PosCurCmm   = *on;
123400110518             V01msg      = $Msg(05);
123500110518             leavesr;
123600110518           ENDIF;
123700110518
123800110518         //?Controllo e decodifico
123900130808           CMMcod = %int(V01cmm);
124000130808           chain  (CMMcod)  AZCMM000;
124100130808           IF  not %found( AZCMM01L );
124200110518             ErrMessage  = *on;
124300110518             ErrGenerico = *on;
124400110518             PosCurCmm   = *on;
124500110518             V01msg      = $Msg(05);
124600110518             leavesr;
124700110518           ENDIF;
124800110518
124900110518         //?utente abilitato al commerciale
125000110518           clear TNTAA1DS;
125100110518           ITAA1caut = '§utegtc';
125200110518           ITAA1cmm  = %dec(V01cmm:7:0);
125300110518           kpjbu     = tntaa1ds;
125400110518           tntaa1c (kpjba);
125500110518           TNTAA1DS = kpjbu;
125600110518           IF  OTAA1fabi = 'N';
125700110518             ErrMessage  = *on;
125800110518             ErrGenerico = *on;
125900110518             PosCurCmm   = *on;
126000110518             V01msg      = $Msg(05);
126100110518             leavesr;
126200110518           ENDIF;
126300110518
126400130808           V01dcmm = CMMdes;
126500110518
126600110518         //?Controllo che non abbia particolarità.
126700130808           IF  CMMpar <> *blanks;
126800110518             ErrMessage  = *on;
126900110518             ErrGenerico = *on;
127000110518             PosCurCmm   = *on;
127100110518             V01msg      = $Msg(05);
127200110518             leavesr;
127300110518           ENDIF;
127400110526
127500110526         //?Deve essere un commerciale unificante
127600130808           IF  %dec(V01cmm:7:0) <> CMMuni;
127700110526             ErrMessage  = *on;
127800110526             ErrGenerico = *on;
127900110526             PosCurCmm   = *on;
128000110526             V01msg      = $Msg(06);
128100110526             leavesr;
128200110526           ENDIF;
128300110526         ENDIF;
128400110518
128500110518       //?PROSSIMA ATTIVITA'
128600110518         IF  V01cco1 <> *blanks;
128700110524           IF  %lookup(V01cco1 : $CCO) = 0;
128800110518             ErrMessage  = *on;
128900110518             ErrGenerico = *on;
129000110518             PosCurCco1  = *on;
129100110518             V01msg = $Msg(12);
129200110518             leavesr;
129300110518           ENDIF;
129400110518         ENDIF;
129500110518         IF  V01cco2 <> *blanks;
129600110524           IF  %lookup(V01cco2 : $CCO) = 0;
129700110518             ErrMessage  = *on;
129800110518             ErrGenerico = *on;
129900110518             PosCurCco2  = *on;
130000110518             V01msg = $Msg(12);
130100110518             leavesr;
130200110518           ENDIF;
130300110518         ENDIF;
130400110518         IF  V01cco3 <> *blanks;
130500110524           IF  %lookup(V01cco3 : $CCO) = 0;
130600110518             ErrMessage  = *on;
130700110518             ErrGenerico = *on;
130800110518             PosCurCco3  = *on;
130900110518             V01msg = $Msg(12);
131000110518             leavesr;
131100110518           ENDIF;
131200110518         ENDIF;
131300110518         IF  V01cco4 <> *blanks;
131400110524           IF  %lookup(V01cco4 : $CCO) = 0;
131500110518             ErrMessage  = *on;
131600110518             ErrGenerico = *on;
131700110518             PosCurCco4  = *on;
131800110518             V01msg = $Msg(12);
131900110518             leavesr;
132000110518           ENDIF;
132100110518         ENDIF;
132200110518         IF  V01cco5 <> *blanks;
132300110524           IF  %lookup(V01cco5 : $CCO) = 0;
132400110518             ErrMessage  = *on;
132500110518             ErrGenerico = *on;
132600110518             PosCurCco5  = *on;
132700110518             V01msg = $Msg(12);
132800110518             leavesr;
132900110518           ENDIF;
133000110518         ENDIF;
133100110519
133200110519       //?DATA PROSSIMA ATTIVITA'
133300110524         clear wdadd;
133400110524         clear wdada;
133500110519         IF  V01dadd <> 0 or V01dada <> 0;
133600110519         //?Data prossima attività dal
133700110519           IF  V01dadd <> 0;
133800110519             clear wlbdat;
133900110519             G02dat = V01dadd;
134000110519             xsrda8(wlbdat);
134100110519             IF  G02err = '1';
134200110519               ErrMessage  = *on;
134300110519               ErrGenerico = *on;
134400110519               PosCurDadd  = *on;
134500110519               V01msg      = $Msg(13);
134600110519               leavesr;
134700110519             ENDIF;
134800110519             V01dadd = G02dat;
134900110519             wdadd   = G02inv;
135000110519           ENDIF;
135100110519         //?Data prossima attività al
135200110519           IF  V01dada <> 0;
135300110519             clear wlbdat;
135400110519             G02dat = V01dada;
135500110519             xsrda8(wlbdat);
135600110519             IF  G02err = '1';
135700110519               ErrMessage  = *on;
135800110519               ErrGenerico = *on;
135900110519               PosCurDada  = *on;
136000110519               V01msg      = $Msg(13);
136100110519               leavesr;
136200110519             ENDIF;
136300110519             V01dada = G02dat;
136400110519             wdada   = G02inv;
136500110519           ENDIF;
136600110519         //?"DAL" congruente con "AL"
136700110519           IF  wdadd > 0 and wdada = 0;
136800110519             ErrMessage  = *on;
136900110519             ErrGenerico = *on;
137000110519             PosCurDadd  = *on;
137100110519             V01msg      = $Msg(14);
137200110519             leavesr;
137300110519           ENDIF;
137400110519           IF  wdadd > 0 and wdada > 0 and wdadd > wdada;
137500110519             ErrMessage  = *on;
137600110519             ErrGenerico = *on;
137700110519             PosCurDada  = *on;
137800110519             V01msg      = $Msg(14);
137900110519             leavesr;
138000110519           ENDIF;
138100110519         ENDIF;
138200110519
138300110519       ENDSR;
138400110519
138500110519       //--------------------------------------------------------------
138600110519       //?Inizializzazione videata D02.
138700110519       //--------------------------------------------------------------
138800110519       BEGSR InzD02;
138900110519
139000110519         clear MK12D02;
139100110519
139200110519       ENDSR;
139300110519
139400110519       //--------------------------------------------------------------
139500110519       //?Gestione tasto funzionale F12 da videata D02.
139600110519       //?F12=Ritorno
139700110519       //--------------------------------------------------------------
139800110519       BEGSR F12D02;
139900110519
140000110519         $InzD01 = *on;
140100110519         $InzD02 = *off;
140200110519         $InzD03 = *off;
140300110519         $Video = 'D1';
140400110519
140500110519       ENDSR;
140600110519
140700110519       //--------------------------------------------------------------
140800110519       //?Controllo videata D02.
140900110519       //--------------------------------------------------------------
141000110519       BEGSR CtrD02;
141100110519
141200110519         WindDspF = IndDspF;
141300110519         reset WindDspFb;
141400110519         IndDspF  = WindDspF;
141500110519
141600110519       //?SPESA TRASPORTO TOTALE
141700110519       //?la spesa DA non deve essere maggiore della spesa A
141800110519         IF  V02sptda > V02spta;
141900110519           ErrMessage  = *on;
142000110519           ErrGenerico = *on;
142100110524           PosCurSpta  = *on;
142200110519           V02msg      = $Msg(15);
142300110519           leavesr;
142400110519         ENDIF;
142500110520
142600110520       //?RIPARTIZIONE
142700110520         IF  V02ripg <> *blanks and
142800110520            (V02rip1 <> *blanks or V02rip2 <> *blanks or
142900110520             V02rip3 <> *blanks);
143000110520           ErrMessage  = *on;
143100110520           ErrGenerico = *on;
143200110520           PosCurRip1  = *on;
143300110520           V02msg      = $Msg(16);
143400110520           leavesr;
143500110520         ENDIF;
143600110520         IF  V02rip1 <> *blanks;
143700110520           IF  %scan('?' : V02rip1) > 0;
143800110520             ErrGenerico = *on;
143900110520             PosCurRip1  = *on;
144000110520             clear TNTB70DS;
144100110520             ITB70ifg = '3T';
144200110520             tntb70r (kpjba : TNTB70DS);
144300110520             IF  OTB70spf <> *blanks;
144400110520               V02rip1 = OTB70spf;
144500110520             ENDIF;
144600110520           ENDIF;
144700110523           IF  %lookup(V02rip1 : $IFO) = 0;
144800110520             ErrMessage  = *on;
144900110520             ErrGenerico = *on;
145000110520             PosCurRip1  = *on;
145100110520             V02msg = $Msg(17);
145200110520             leavesr;
145300110520           ENDIF;
145400110520         ENDIF;
145500110520         IF  V02rip2 <> *blanks;
145600110520           IF  %scan('?' : V02rip2) > 0;
145700110520             ErrGenerico = *on;
145800110520             PosCurRip2  = *on;
145900110520             clear TNTB70DS;
146000110520             ITB70ifg = '3T';
146100110520             tntb70r (kpjba : TNTB70DS);
146200110520             IF  OTB70spf <> *blanks;
146300110520               V02rip2 = OTB70spf;
146400110520             ENDIF;
146500110520           ENDIF;
146600110523           IF  %lookup(V02rip2 : $IFO) = 0;
146700110520             ErrMessage  = *on;
146800110520             ErrGenerico = *on;
146900110520             PosCurRip2  = *on;
147000110520             V02msg = $Msg(17);
147100110520             leavesr;
147200110520           ENDIF;
147300110520         ENDIF;
147400110520         IF  V02rip3 <> *blanks;
147500110520           IF  %scan('?' : V02rip3) > 0;
147600110520             ErrGenerico = *on;
147700110520             PosCurRip3  = *on;
147800110520             clear TNTB70DS;
147900110520             ITB70ifg = '3T';
148000110520             tntb70r (kpjba : TNTB70DS);
148100110520             IF  OTB70spf <> *blanks;
148200110520               V02rip3 = OTB70spf;
148300110520             ENDIF;
148400110520           ENDIF;
148500110523           IF  %lookup(V02rip3 : $IFO) = 0;
148600110520             ErrMessage  = *on;
148700110520             ErrGenerico = *on;
148800110520             PosCurRip3  = *on;
148900110520             V02msg = $Msg(17);
149000110520             leavesr;
149100110520           ENDIF;
149200110520         ENDIF;
149300110520         IF  V02ripda > 100;
149400110520           ErrMessage  = *on;
149500110520           ErrGenerico = *on;
149600110520           PosCurRipda = *on;
149700110520           V02msg = $Msg(18);
149800110520           leavesr;
149900110520         ENDIF;
150000110520         IF  V02ripa > 100;
150100110520           ErrMessage  = *on;
150200110520           ErrGenerico = *on;
150300110520           PosCurRipa  = *on;
150400110520           V02msg = $Msg(18);
150500110520           leavesr;
150600110520         ENDIF;
150700110520         IF  V02ripda > V02ripa;
150800110520           ErrMessage  = *on;
150900110520           ErrGenerico = *on;
151000110520           PosCurRipa  = *on;
151100110520           V02msg = $Msg(15);
151200110520           leavesr;
151300110520         ENDIF;
151400110520
151500110520       //?ALTRE CARATTERISTICHE
151600110520         IF  V02pesda > V02pesa;
151700110520           ErrMessage  = *on;
151800110520           ErrGenerico = *on;
151900110520           PosCurPesa  = *on;
152000110520           V02msg = $Msg(15);
152100110520           leavesr;
152200110520         ENDIF;
152300110520         IF  V02pspda > V02pspa;
152400110520           ErrMessage  = *on;
152500110520           ErrGenerico = *on;
152600110520           PosCurPspa  = *on;
152700110520           V02msg = $Msg(15);
152800110520           leavesr;
152900110520         ENDIF;
153000110520
153100110524       //?AFFIDATO A BRT
153200110520         IF  V02barda > 100;
153300110520           ErrMessage  = *on;
153400110520           ErrGenerico = *on;
153500110520           PosCurBarda = *on;
153600110520           V02msg = $Msg(18);
153700110520           leavesr;
153800110520         ENDIF;
153900110520         IF  V02bara > 100;
154000110520           ErrMessage  = *on;
154100110520           ErrGenerico = *on;
154200110520           PosCurBara  = *on;
154300110520           V02msg = $Msg(18);
154400110520           leavesr;
154500110520         ENDIF;
154600110520         IF  V02barda > V02bara;
154700110520           ErrMessage  = *on;
154800110520           ErrGenerico = *on;
154900110520           PosCurBara  = *on;
155000110520           V02msg = $Msg(15);
155100110520           leavesr;
155200110520         ENDIF;
155300110520
155400110520       //?CONCORRENTI
155500110523         clear wConta;
155600110523         IF  V02conc1 <> *blanks;
155700110523           IF  %scan('?' : V02conc1) > 0;
155800110523             ErrGenerico = *on;
155900110523             PosCurConc1 = *on;
156000110524             clear TNTB70DS;
156100110524             ITB70tpf = '10';
156200110524             tntb70r (kpjba : TNTB70DS);
156300110524             IF  OTB70spf <> *blanks;
156400110524               V02conc1 = OTB70spf;
156500110523             ENDIF;
156600110523           ENDIF;
156700110523           IF  %lookup(V02conc1 : $CON) = 0;
156800110523             ErrMessage  = *on;
156900110523             ErrGenerico = *on;
157000110523             PosCurConc1 = *on;
157100110523             V02msg = $Msg(19);
157200110523             leavesr;
157300110523           ENDIF;
157400110523           wConta += 1;
157500110523         ENDIF;
157600110523         IF  V02conc2 <> *blanks;
157700110523           IF  %scan('?' : V02conc2) > 0;
157800110523             ErrGenerico = *on;
157900110523             PosCurConc2 = *on;
158000110524             clear TNTB70DS;
158100110524             ITB70tpf = '10';
158200110524             tntb70r (kpjba : TNTB70DS);
158300110524             IF  OTB70spf <> *blanks;
158400110524               V02conc2 = OTB70spf;
158500110523             ENDIF;
158600110523           ENDIF;
158700110523           IF  %lookup(V02conc2 : $CON) = 0;
158800110523             ErrMessage  = *on;
158900110523             ErrGenerico = *on;
159000110523             PosCurConc2 = *on;
159100110523             V02msg = $Msg(19);
159200110523             leavesr;
159300110523           ENDIF;
159400110523           wConta += 1;
159500110523         ENDIF;
159600110523         IF  V02conc3 <> *blanks;
159700110523           IF  %scan('?' : V02conc3) > 0;
159800110523             ErrGenerico = *on;
159900110524             PosCurConc3 = *on;
160000110524             clear TNTB70DS;
160100110524             ITB70tpf = '10';
160200110524             tntb70r (kpjba : TNTB70DS);
160300110524             IF  OTB70spf <> *blanks;
160400110524               V02conc3 = OTB70spf;
160500110523             ENDIF;
160600110523           ENDIF;
160700110523           IF  %lookup(V02conc3 : $CON) = 0;
160800110523             ErrMessage  = *on;
160900110523             ErrGenerico = *on;
161000110523             PosCurConc3 = *on;
161100110523             V02msg = $Msg(19);
161200110523             leavesr;
161300110523           ENDIF;
161400110523           wConta += 1;
161500110523         ENDIF;
161600110523         IF  V02conc4 <> *blanks;
161700110523           IF  %scan('?' : V02conc4) > 0;
161800110523             ErrGenerico = *on;
161900110524             PosCurConc4 = *on;
162000110524             clear TNTB70DS;
162100110524             ITB70tpf = '10';
162200110524             tntb70r (kpjba : TNTB70DS);
162300110524             IF  OTB70spf <> *blanks;
162400110524               V02conc4 = OTB70spf;
162500110523             ENDIF;
162600110523           ENDIF;
162700110523           IF  %lookup(V02conc4 : $CON) = 0;
162800110523             ErrMessage  = *on;
162900110523             ErrGenerico = *on;
163000110524             PosCurConc4 = *on;
163100110523             V02msg = $Msg(19);
163200110523             leavesr;
163300110523           ENDIF;
163400110523           wConta += 1;
163500110523         ENDIF;
163600110523         IF  V02conc5 <> *blanks;
163700110523           IF  %scan('?' : V02conc5) > 0;
163800110523             ErrGenerico = *on;
163900110524             PosCurConc5 = *on;
164000110524             clear TNTB70DS;
164100110524             ITB70tpf = '10';
164200110524             tntb70r (kpjba : TNTB70DS);
164300110524             IF  OTB70spf <> *blanks;
164400110524               V02conc5 = OTB70spf;
164500110523             ENDIF;
164600110523           ENDIF;
164700110523           IF  %lookup(V02conc5 : $CON) = 0;
164800110523             ErrMessage  = *on;
164900110523             ErrGenerico = *on;
165000110524             PosCurConc5 = *on;
165100110523             V02msg = $Msg(19);
165200110523             leavesr;
165300110523           ENDIF;
165400110523           wConta += 1;
165500110523         ENDIF;
165600110523       //?Se immessa la % posso selezionare solo 1 concorrente
165700110523         IF  wConta > 1 and (V02conda > 0 or V02cona > 0);
165800110523           ErrMessage  = *on;
165900110523           ErrGenerico = *on;
166000110523           PosCurConc1 = *on;
166100110523           V02msg = $Msg(20);
166200110523           leavesr;
166300110523         ENDIF;
166400110523       //?Controllo la %
166500110523         IF  V02conda > 100;
166600110523           ErrMessage  = *on;
166700110523           ErrGenerico = *on;
166800110523           PosCurConda = *on;
166900110523           V02msg = $Msg(18);
167000110523           leavesr;
167100110523         ENDIF;
167200110523         IF  V02cona > 100;
167300110523           ErrMessage  = *on;
167400110523           ErrGenerico = *on;
167500110523           PosCurCona  = *on;
167600110523           V02msg = $Msg(18);
167700110523           leavesr;
167800110523         ENDIF;
167900110523         IF  V02conda > V02cona;
168000110523           ErrMessage  = *on;
168100110523           ErrGenerico = *on;
168200110523           PosCurCona  = *on;
168300110523           V02msg = $Msg(15);
168400110523           leavesr;
168500110523         ENDIF;
168600110523
168700110523       //?MOTIVO DI NON AFFIDAMENTO
168800110523         clear xx;
168900110523         clear V02dmoi1;
169000110523         IF  V02moi1 <> *blanks;
169100110523           IF  %scan('?' : V02moi1) > 0;
169200110523             ErrGenerico = *on;
169300110523             PosCurMoi1  = *on;
169400110523             clear TNTB70DS;
169500110523             ITB70tpf = 'MOI';
169600110523             tntb70r (kpjba : TNTB70DS);
169700110523             IF  OTB70spf <> *blanks;
169800110523               V02moi1  = OTB70spf;
169900110523               V02dmoi1 = OTB70dspf;
170000110523             ENDIF;
170100110523           ENDIF;
170200110523           xx = %lookup(V02moi1 : $MOI);
170300110523           IF  xx = 0;
170400110523             ErrMessage  = *on;
170500110523             ErrGenerico = *on;
170600110523             PosCurMoi1  = *on;
170700110523             V02msg = $Msg(21);
170800110523             leavesr;
170900110523           ENDIF;
171000110523           V02dmoi1 = $DMOI(xx);
171100110523         ENDIF;
171200110523         clear xx;
171300110523         clear V02dmoi2;
171400110523         IF  V02moi2 <> *blanks;
171500110523           IF  %scan('?' : V02moi2) > 0;
171600110523             ErrGenerico = *on;
171700110523             PosCurMoi2  = *on;
171800110523             clear TNTB70DS;
171900110523             ITB70tpf = 'MOI';
172000110523             tntb70r (kpjba : TNTB70DS);
172100110523             IF  OTB70spf <> *blanks;
172200110523               V02moi2  = OTB70spf;
172300110523               V02dmoi2 = OTB70dspf;
172400110523             ENDIF;
172500110523           ENDIF;
172600110523           xx = %lookup(V02moi2 : $MOI);
172700110523           IF  xx = 0;
172800110523             ErrMessage  = *on;
172900110523             ErrGenerico = *on;
173000110523             PosCurMoi2  = *on;
173100110523             V02msg = $Msg(21);
173200110523             leavesr;
173300110523           ENDIF;
173400110523           V02dmoi2 = $DMOI(xx);
173500110523         ENDIF;
173600110523       //?Non possono essere uguali
173700110523         IF  V02moi1 = V02moi2 and V02moi1 <> *blanks;
173800110523           ErrMessage  = *on;
173900110523           ErrGenerico = *on;
174000110523           PosCurMoi1  = *on;
174100110523           V02msg = $Msg(23);
174200110523           leavesr;
174300110523         ENDIF;
174400110523
174500110523       //?OPERATORI LOGISTICA
174600110523         IF  V02logp <> *blanks;
174700110523           IF  %scan('?' : V02logp) > 0;
174800110523             ErrGenerico = *on;
174900110523             PosCurLogp  = *on;
175000110524             clear TNTB70DS;
175100110524             ITB70tpf = '40';
175200110524             tntb70r (kpjba : TNTB70DS);
175300110524             IF  OTB70spf <> *blanks;
175400110524               V02logp = OTB70spf;
175500110523             ENDIF;
175600110523           ENDIF;
175700110523           IF  %lookup(V02logp : $LOG) = 0;
175800110523             ErrMessage  = *on;
175900110523             ErrGenerico = *on;
176000110523             PosCurLogp  = *on;
176100110523             V02msg = $Msg(23);
176200110523             leavesr;
176300110523           ENDIF;
176400110523         ENDIF;
176500110524
176600110524       //?INFO COMMERCIALI NON PRESENTI
176700110524         IF  (V02sptda > 0 or V02ripg <> *blanks or
176800110524              V02rip  <> *blanks or V02ripa > 0  or
176900110524              V02pri  <> *blanks or V02gdo <> *blanks or
177000110524              V02ecm  <> *blanks or V02hpr <> *blanks or
177100110524              V02sbx  <> *blanks or V02pesa > 0 or
177200110524              V02pspa  > 0 or V02bara > 0 or
177300110524              V02conc <> *blanks or V02cona > 0 or
177400110524              V02moi  <> *blanks or V02logi <> *blanks or
177500110524              V02logo <> *blanks or V02logp <> *blanks or
177600110524              V02ifoal > 0) and V02nifo = 'S';
177700110524           ErrMessage  = *on;
177800110524           ErrGenerico = *on;
177900110524           PosCurNifo  = *on;
178000110524           V02msg      = $Msg(24);
178100110524           leavesr;
178200110524          ENDIF;
178300110523
178400110523       //?DATA INFO COMMERCIALI
178500110524         IF  V02ifoal > 0;
178600110524           clear wifoal;
178700110524           clear wlbdat;
178800110524           G02dat = V02ifoal;
178900110524           xsrda8(wlbdat);
179000110524           IF  G02err = '1';
179100110524             ErrMessage  = *on;
179200110524             ErrGenerico = *on;
179300110524             PosCurIfoal = *on;
179400110524             V02msg      = $Msg(13);
179500110524             leavesr;
179600110524           ENDIF;
179700110524           V02ifoal = G02dat;
179800110524           wifoal   = G02inv;
179900110524         ENDIF;
180000110519
180100110519       ENDSR;
180200110524
180300110524       //--------------------------------------------------------------
180400110524       //?Inizializzazione videata D03.
180500110524       //--------------------------------------------------------------
180600110524       BEGSR InzD03;
180700110524
180800110524         clear MK12D03;
180900110524
181000110524         V03sifo = 'N';
181100110524         IF  F08Attivo;
181200110524           V03sifo = 'S';
181300110524         ENDIF;
181400110524
181500110524       ENDSR;
181600110524
181700110524       //--------------------------------------------------------------
181800110524       //?Gestione tasto funzionale F12 da videata D03.
181900110524       //?F12=Ritorno
182000110524       //--------------------------------------------------------------
182100110524       BEGSR F12D03;
182200110524
182300110524       //?Se abilitato alla stampa info torno alla seconda videata
182400110524         IF  F08Attivo;
182500110524           $InzD01 = *off;
182600110524           $InzD02 = *on;
182700110524           $InzD03 = *off;
182800110524           $Video = 'D2';
182900110524       //?Alrimenti torno alla prima videata
183000110524         ELSE;
183100110524           $InzD01 = *on;
183200110524           $InzD02 = *off;
183300110524           $InzD03 = *off;
183400110524           $Video = 'D1';
183500110524         ENDIF;
183600110524
183700110524       ENDSR;
183800110524
183900110524       //--------------------------------------------------------------
184000110524       //?Gestione tasto funzionale F6 da videata D03.
184100110524       //?F06=Elaborazione
184200110524       //--------------------------------------------------------------
184300110524       BEGSR F06D03;
184400110524
184500110524         clear TRMK13DS;
184600110524         IF  V01cmmu <> *blanks;
184700110524           D13cmmu  = %dec(V01cmmu:7:0);
184800110524         ENDIF;
184900110524         D13prv   = V01prv;
185000110530         D13loc   = V01loc;
185100110524         D13cap   = V01cap;
185200110524         D13naz   = V01naz;
185300110524         IF  V01car <> *blanks;
185400110524           D13car   = %dec(V01car:3:0);
185500110524         ENDIF;
185600110524         D13fil   = V01fil;
185700110524         IF  V01sct <> *blanks;
185800110524           D13sct   = %dec(V01sct:5:0);
185900110524         ENDIF;
186000110524         D13cic   = V01cic;
186100110524         D13fls   = V01fls;
186200110524         D13pst   = V01pst;
186300110524         D13fsf   = V01fsf;
186400110525         D13ttr   = V01ttr;
186500110524         IF  V01cmm <> *blanks;
186600110524           D13cmm   = %dec(V01cmm:7:0);
186700110524         ENDIF;
186800110524         D13cco   = V01cco;
186900110524         D13dadda = wdadd;
187000110524         D13dada  = wdada;
187100110524         D13sptda = V02sptda;
187200110524         D13spta  = V02spta;
187300110524         D13ripg  = V02ripg;
187400110524         D13rip   = V02rip;
187500110524         D13ripda = V02ripda;
187600110524         D13ripa  = V02ripa;
187700110524         D13pri   = V02pri;
187800110524         D13gdo   = V02gdo;
187900110524         D13ecm   = V02ecm;
188000110524         D13hpr   = V02hpr;
188100110524         D13sbx   = V02sbx;
188200110524         D13pesda = V02pesda;
188300110524         D13pesa  = V02pesa;
188400110524         D13pspda = V02pspda;
188500110524         D13pspa  = V02pspa;
188600110524         D13barda = V02barda;
188700110524         D13bara  = V02bara;
188800110524         D13conc  = V02conc;
188900110524         D13conda = V02conda;
189000110524         D13cona  = V02cona;
189100110524         D13moi   = V02moi;
189200110524         D13logi  = V02logi;
189300110524         D13logo  = V02logo;
189400110524         D13logp  = V02logp;
189500110524         D13nifo  = V02nifo;
189600110524         D13ifoal = wifoal;
189700110525         D13ospt  = V03ospt;
189800110524         D13orag  = V03orag;
189900110525         D13ocmmu = V03ocmmU;
190000110525         D13ocmm  = V03ocmm;
190100110525         D13ocar  = V03ocar;
190200110524         D13osct  = V03osct;
190300110524         D13sifo  = V03sifo;
190400110524
190500110524         kcoaz = 'MK13';
190600110524         kpjbu = TRMK13ds;
190700110524         kbuff = *blanks;
190800110524
190900110524       //?Richiamo interattivo
191000110524         IF  knmus = *all'1';
191100110524           TRMK13R(kpjba);
191200110524       //?Richiamo batch
191300110524         ELSE;
191400110524           BCH10(kpjba);
191500110524         ENDIF;
191600110524
191700110524       //?Chiusura del programma
191800110524         $Fine = *on;
191900110524
192000110524       ENDSR;
192100110524
192200110524       //--------------------------------------------------------------
192300110524       //?Controllo videata D03.
192400110524       //--------------------------------------------------------------
192500110524       BEGSR CtrD03;
192600110524
192700110524         WindDspF = IndDspF;
192800110524         reset WindDspFb;
192900110524         IndDspF  = WindDspF;
193000110524
193100110524       //?ORDINAMENTI
193200110524         //?obbligatori
193300110525         IF  V03ospt  = *blanks and V03orag = *blanks and
193400110525             V03ocmmu = *blanks and V03ocmm = *blanks and
193500110525             V03ocar  = *blanks and V03osct = *blanks;
193600110524           ErrMessage  = *on;
193700110524           ErrGenerico = *on;
193800110525           IF  F08Attivo;
193900110525             PosCurOspt = *on;
194000110525           ELSE;
194100110525             PosCurOrag = *on;
194200110525           ENDIF;
194300110524           V03msg      = $Msg(25);
194400110524           leavesr;
194500110524         ENDIF;
194600110524         //?possibile solo 1
194700110524         SELECT;
194800110525           WHEN  V03ospt <> *blanks and
194900110525                (V03orag <> *blanks or V03ocmmu <> *blanks or
195000110525                 V03ocmm <> *blanks or V03ocar  <> *blanks or
195100110525                 V03osct <> *blanks);
195200110524             ErrMessage  = *on;
195300110524             ErrGenerico = *on;
195400110525             IF  F08Attivo;
195500110525               PosCurOspt = *on;
195600110525             ELSE;
195700110525               PosCurOrag  = *on;
195800110525             ENDIF;
195900110524             V03msg      = $Msg(26);
196000110524             leavesr;
196100110525           WHEN  V03orag <> *blanks and
196200110525                (V03ocmmu <> *blanks or V03ocmm <> *blanks or
196300110525                 V03ocar  <> *blanks or V03osct <> *blanks);
196400110524             ErrMessage  = *on;
196500110524             ErrGenerico = *on;
196600110525             PosCurOrag  = *on;
196700110524             V03msg      = $Msg(26);
196800110524             leavesr;
196900110525           WHEN  V03ocmmu <> *blanks and
197000110525                (V03ocmm <> *blanks or
197100110525                 V03ocar  <> *blanks or V03osct <> *blanks);
197200110525             ErrMessage  = *on;
197300110525             ErrGenerico = *on;
197400110525             PosCurOcmmu = *on;
197500110525             V03msg      = $Msg(26);
197600110525             leavesr;
197700110525           WHEN  V03ocmm <> *blanks and
197800110525                (V03ocar  <> *blanks or V03osct <> *blanks);
197900110525             ErrMessage  = *on;
198000110525             ErrGenerico = *on;
198100110525             PosCurOcmm  = *on;
198200110525             V03msg      = $Msg(26);
198300110525             leavesr;
198400110525           WHEN  V03ocar <> *blanks and V03osct <> *blanks;
198500110525             ErrMessage  = *on;
198600110525             ErrGenerico = *on;
198700110525             PosCurOcar  = *on;
198800110525             V03msg      = $Msg(26);
198900110525             leavesr;
199000110524         ENDSL;
199100110524
199200110524       ENDSR;
199300080206
199400080206      /end-free
199500080206
199600080206       //--------------------------------------------------------------
199700080206       //?Schiere a tempo di compilazione.
199800080206       //--------------------------------------------------------------
199900080206
200000080206** - $MSG -------------------------------------------------------------------*
200100110308Utente non abilitato alla Funzione.                                           01
200200110530Immettere Commerciale  OPPURE Prov/Località  OPPURE Area  OPPURE Filiale.     02
200300110524Se parzializzazione per cap o località  immettere la provincia o la nazione.  03
200400110518Nazione errata                                                                04
200500110518Commerciale errato o non in gestione all'utente                               05
200600110518Richiedere solo commerciali UNIFICANTI!                                       06
200700110518Codice area errato                                                            07
200800110518Filiale di appartenenza errata.                                               08
200900110518Categoria merceologica errata.                                                09
201000110518Importanza cliente errata.                                                    10
201100110518Categoria Potenziale errata.                                                  11
201200110518Prossima attività errata.                                                     12
201300110519Data errata                                                                   13
201400110519Data "DAL" incongruente con data "AL"                                         14
201500110519valore " DA " non deve essere maggiore del valore " A "                       15
201600110520Per "RIPARTIZIONE SPESA" indicare o la tipologia generica o un tipo specifico 16
201700110520Codice Servizio di ripartizione spesa errato                                  17
201800110520La percentuale immessa non deve superare 100                                  18
201900110523Concorrente errato                                                            19
202000110523Se indicata la percentuale indicare solo UN concorrente                       20
202100110523Motivo non affidamento a noi errato                                           21
202200110523i due motivi non affidamento non possono essere uguali                        22
202300110523Codice operatore logistica errato                                             23
202400110524Impossibile parzializzare le INFO se richiesti potenziali senza info Comm.    24
202500110524Immettere un ordinamento                                                      25
202600110524Scegliere un solo ordinamento                                                 26
