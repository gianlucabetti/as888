000100080408       //--------------------------------------------------------------
000200080410       //?Gestione tabella "PSP" = Password SPC gestione PDA
000300080408       //--------------------------------------------------------------
000400080408
000500080409     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600110421     h dftactgrp(*no)
000700080408
000800080409      //---------------------------------------------------------------
000900080409      //?Dichiarazione file.
001000080409      //---------------------------------------------------------------
001100080409
001200080409      // - Organigramma
001300080409     fAZORG01L  if   e           k disk
001400080409
001500080409      // - Tabelle
001600080409     fTNTBE01L  Uf A e           k disk
001700080409
001800080410      // - Video Gestione tabella SPC
001900080410     fTNTB64D   cf   e             workstn
002000080410     f                                     sfile(TB64S01 : S01nrr)
002100080409     f                                     indds(IndDspF)
002200080409     f                                     infds(InfDspF)
002300080409
002400080409      //---------------------------------------------------------------
002500080409      //?Definizione costanti.
002600080409      //---------------------------------------------------------------
002700080409
002800080409      // Codice tabella in gestione
002900080409     d c_Tab           c                   const('PSP')
003000080414
003100080414      // Carattere separatore
003200080414     d c_Char          c                   const('#')
003300080409
003400080409      // - Numero di record in ciascuna videata di subfile
003500081118     d c_SflPag        c                   const(11)
003600081118
003700081118      // - Numero di password possibili
003800081118     d c_Pwd           c                   const(22)
003900080409
004000080409      // - Tasti funzionali a video
004100080409     d c_F01           c                   const(x'31')
004200080409     d c_F02           c                   const(x'32')
004300080409     d c_F03           c                   const(x'33')
004400080409     d c_F05           c                   const(x'35')
004500080409     d c_F06           c                   const(x'36')
004600080409     d c_F07           c                   const(x'37')
004700080409     d c_F08           c                   const(x'38')
004800080409     d c_F09           c                   const(x'39')
004900080409     d c_F10           c                   const(x'3A')
005000080409     d c_F12           c                   const(x'3C')
005100080409     d c_F13           c                   const(x'B1')
005200080409     d c_F14           c                   const(x'B2')
005300080409     d c_F15           c                   const(x'B3')
005400080409     d c_F16           c                   const(x'B4')
005500080409     d c_F17           c                   const(x'B5')
005600080409     d c_F18           c                   const(x'B6')
005700080409     d c_F19           c                   const(x'B7')
005800080409     d c_F20           c                   const(x'B8')
005900080409     d c_F21           c                   const(x'B9')
006000080409     d c_F22           c                   const(x'BA')
006100080409     d c_F23           c                   const(x'BB')
006200080409     d c_F24           c                   const(x'BC')
006300080409     d c_Enter         c                   const(x'F1')
006400080409     d c_RollDown      c                   const(x'F4')
006500080409     d c_RollUp        c                   const(x'F5')
006600080411
006700080414     d C_Digit         c                   const('1234567890')
006800080411
006900080411     d C_LenPwdPSP     c                   const(4)
007000080409
007100080409      //---------------------------------------------------------------
007200080409      //?Definizione schiere.
007300080409      //---------------------------------------------------------------
007400080409
007500080409      // - Messaggi di errore
007600080701     d $Msg            s             78    dim(18) ctdata perrcd(1)
007700080414
007800080414      // - Dati x SPC
007900080414     d $PSP            s                   like(dPSP_sing)
008000081118     d                                     dim(C_Pwd)    inz
008100080512     d $PwdSpc         s                   like(§PSPpwd01)
008200081118     d                                     dim(C_Pwd)    inz(*loval)
008300080409
008400080409      //---------------------------------------------------------------
008500080409      //?Definizione aree dati.
008600080409      //---------------------------------------------------------------
008700080409
008800080409      // - Dati utente
008900080409     d §AzUte        e ds                  extname(AZUTE00F)
009000080409     d                                     dtaara
009100080409     d §DatiUte      e ds                  extname(dDatiUte)
009200080409     d                                     dtaara
009300080409
009400080409      //---------------------------------------------------------------
009500080409      //?Definizione strutture dati.
009600080409      //---------------------------------------------------------------
009700080409
009800080409      // - Status
009900080409     d Psds           sds
010000080409     d   SDSpgm          *proc
010100080409
010200080409      // - InfDS
010300080409     d InfDspF         ds
010400080409     d  dsp_aid              369    369a                                        AID byte
010500080409     d  sfl_rrn              376    377i 0                                      Subfile rrn
010600080409     d  min_nrr              378    379i 0                                      Subfile min rrn
010700080409     d  num_rcds             380    381i 0                                      Subfile num rcds
010800080409
010900080409      // - Indicatori su DspF
011000080409     d IndDspF         ds
011100080410        // - Abilitazione tasti funzionali
011200080410     d  F6Attivo                      1n   overlay(IndDspF : 06)
011300090706     d  F8Attivo                      1n   overlay(IndDspF : 08)
011400080410     d  F18Attivo                     1n   overlay(IndDspF : 18)
011500080409        // - Indicatori di gestione del subfile
011600080409     d  SflDsp_N                      1n   overlay(IndDspF : 30)
011700080409     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
011800080409     d  SflNxtChg                     1n   overlay(IndDspF : 32)
011900080409     d  SflEnd                        1n   overlay(IndDspF : 33)
012000080410        // - Condizionamento visualizzazioni   &
012100080410        //   Condizionamento protezione
012200080410     d  NoProtectFgs                  1n   overlay(IndDspF : 40)
012300140519     d  VisualizzaPwd                  n   overlay(IndDspF : 41)
012400080414     d  NoProtectCod                  1n   overlay(IndDspF : 42)
012500080409        // - Indicatori di errore
012600080409     d  ErrMessage                    1n   overlay(IndDspF : 28)
012700080409     d  PosCurFGS                     1n   overlay(IndDspF : 50)
012800080409     d  PosCurOpz                     1n   overlay(IndDspF : 51)
012900080414     d  PosCurCOD                     1n   overlay(IndDspF : 52)
013000080409     d  PosCurDES                     1n   overlay(IndDspF : 53)
013100080409     d  PosCurPWDO                    1n   overlay(IndDspF : 54)
013200080409     d  PosCurPWDN1                   1n   overlay(IndDspF : 55)
013300080409     d  PosCurPWDN2                   1n   overlay(IndDspF : 56)
013400080409     d  ErrGenerico                   1n   overlay(IndDspF : 99)
013500080409
013600080409      // - Parametri ricevuti
013700080409     d KPJBA         e ds
013800080409
013900080409      // - Reperimento dati utente
014000080409     d TIBS34ds      e ds
014100080409     d dUte01        e ds
014200080409
014300080409      // - Caricamento filiali gestibili
014400080409     d TRUL31ds      e ds                  inz
014500080409     d   I31abi      e                     inz('AP')
014600080409     d   $Pog                 10    759  0 inz  dim(250)
014700080409     d
014800080410
014900080410      // - Dati record principale della tabella
015000080410     d TNTBEds       e ds                  inz  extname(TNTBE00F)
015100080410     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
015200080410     d                                          prefix(TBX:3)
015300080409
015400080409      // - Ricerca/Controllo tabelle
015500080409     d TIBS02ds      e ds                  inz
015600080409     d   T02Mod      e                     inz('C')
015700080409
015800080409      // - Tabella PSP = Password SPC gestione PDA
015900080409     d dPSP          e ds                  inz
016000080414
016100080414     d dPSP_sing       ds                  inz
016200080414     d  §PSPcod                            inz  like(§PSPcod01)
016300080414     d  §PSPdes                            inz  like(§PSPdes01)
016400080414     d  §PSPpwd                            inz  like(§PSPpwd01)
016500080414     d  §PSPchar                           inz  like(§PSPchar01)
016600080409
016700080409      // Campi di comodo per gestione indicatori a video
016800080409     d WindDspF        ds                  inz
016900080409     d   WindDspFa             1     49    inz(*zero)
017000080409     d   WindDspFb            50     99    inz(*zero)
017100080409
017200080409      //---------------------------------------------------------------
017300080409      //?Definizione variabili globali.
017400080409      //---------------------------------------------------------------
017500080409
017600080409      // - Flags booleani
017700080409     d $Fine           s               n   inz(*off)
017800080409     d $InzS01         s               n   inz(*on)
017900080409     d $InzD01         s               n   inz(*on)
018000080410     d $InzW01         s               n   inz(*on)
018100080409     d $Err            s               n   inz(*off)
018200080409
018300080409      // - Indici di schiera
018400080430     d wx              s              3  0 inz
018500080409     d xx              s              3  0 inz
018600080414     d yy              s              3  0 inz
018700080409
018800080409      // - Campi associati al video
018900080411     d $Video          s              2    inz('S1')
019000080409     d S01nrr          s              4  0 inz
019100080409     d W_SflPag        s              3  0 inz
019200080409     d wPag            s              4  0 inz
019300080409     d wRig            s              3  0 inz
019400080409
019500080409      // - Campi di comodo
019600080409     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
019700080409
019800080409      //---------------------------------------------------------------
019900080409      //?Definizione procedure usate.
020000080409      //---------------------------------------------------------------
020100080409
020200080409      // - Reperimento dati utente
020300080409     d tibs34r         pr                  extpgm('TIBS34R')
020400080409     d  tibs34ds                           likeds(TIBS34ds)
020500080409
020600080409      // - Caricamento filiali gestibili
020700080409     d trul31r         pr                  extpgm('TRUL31R')
020800080410     d  kpjba                              likeds(kpjba)    const
020900080409     d  trul31ds                           likeds(TRUL31ds)
021000080409
021100080409      // - Gestione tabelle
021200080409     d tibs02r         pr                  extpgm('TIBS02R')
021300080409     d  kpjba                              likeds(kpjba)    const
021400080409     d  tibs02ds                           likeds(TIBS02ds)
021500090706
021600090706      // - Gestione password sede
021700090706     d tntb64r1        pr                  extpgm('TNTB64R1')
021800090706     d  kpjba                              likeds(kpjba)    const
021900080409
022000080409      //---------------------------------------------------------------
022100080409      //?Definizione key-list.
022200080409      //---------------------------------------------------------------
022300080409
022400080409       // - File TNTBE01L
022500080410     d k05tntbe01    e ds                  extname(TNTBE01L:*key)
022600080411     d                                     prefix(k_)
022700080409     d                                     inz
022800080409
022900080409      //---------------------------------------------------------------
023000080409      //?Riepilogo indicatori.
023100080409      //---------------------------------------------------------------
023200080409      // - Comuni
023300080409      // 02    : Scelta opzione 2=Modifica
023400080409      // 05    : Scelta opzione 5=Visualizzazione
023500080409      // 28    : Emissione messaggio di errore a video
023600080409      // - C01/S01
023700080409      // 30    : Condiziona SFLDSP    (*not)
023800080409      // 31    : Condiziona SFLDSPCTL (*not)
023900080409      // 30+31 : Condiziona SFLCLR
024000080409      // 32    : Condiziona SFLNXTCHG in S01
024100080409      // 33    : Condiziona SFLEND
024200080409      // 40    : Condiziona la protezione di V1CFGS
024300080409      // 50    : Errore: Opzione errata
024400080409      // - D01
024500080409      // 51    : Errore: Codice del SPC     errato o mancante
024600080409      // 52    : Errore: Descrizione        mancante
024700080409      // 53    : Errore: Password attuale   errata o mancante
024800080409      // 54    : Errore: Nuova password (1) errata o mancante
024900080409      // 55    : Errore: Nuova password (2) errata o mancante
025000080409      // - Comuni
025100080409      // 99    : Generico di Errore
025200080409      //---------------------------------------------------------------
025300080409
025400080409      //---------------------------------------------------------------
025500080409      //?M A I N - L I N E
025600080409      //---------------------------------------------------------------
025700080409
025800080409     c     *Entry        plist
025900080409     c                   parm                    KPJBA
026000080409
026100080409      /free
026200080409
026300080409       // Operazioni iniziali
026400080409       exsr sr_RoutInz;
026500080409
026600080409       // Gestione video
026700080409       DOW $Fine = *off;
026800080409         select;
026900080409           when $Video = 'S1';
027000080409             exsr sr_GesS01;
027100080409           when $Video = 'D1';
027200080409             exsr sr_GesD01;
027300080409           when $Video = 'W1';
027400080410             exsr sr_GesW01;
027500080409           other;
027600080409             $Fine = *on;
027700080409         endsl;
027800080409       ENDDO;
027900080409
028000080409       // Operazioni finali
028100080409       exsr sr_RoutEnd;
028200080409
028300080409       //--------------------------------------------------------------
028400080409       //?Operazioni iniziali.
028500080409       //--------------------------------------------------------------
028600080409       BEGSR sr_RoutInz;
028700080409
028800080409         // Ricezione parametri
028900080409       //TNTB64ds = kpjbu;
029000080409
029100080409         // Impostazione campi "fissi"
029200080409         T01pgm = SDSpgm;
029300080409
029400080409         // Reperimento dati job
029500080409         exsr sr_DatiJob;
029600080409
029700080409         // Impostazione Filiale Gestione
029800080409         C1Cfgs = DUTpou;
029900080409         chain  C1Cfgs  AZORG;
030000080409         if  %found(AZORG01L)  and  ORGfva = *blank;
030100080409           C1Dfgs = ORGdes;
030200080409         endif;
030300090706
030400090706         // Abilitazione F8=Password Sede
030500090706         // solo per utente EDP
030600090706         F8Attivo = %subst(knmus : 1 : 3) = 'EDP';
030700080409
030800080409         // Abilitazione F18=Modifica Filiale Gestione
030900080526       ////   solo se la filiale utente è SIMFEL
031000080526       //F18Attivo = (DUTpou = DUTtfp);
031100080526         //   solo per gli utenti EDP* e POC*
031200080526         F18Attivo = (%subst(knmus : 1 : 3) = 'EDP'   or
031300080526                      %subst(knmus : 1 : 3) = 'POC');
031400080409
031500080409         // Caricamento filiali gestibili
031600080409         reset  TRUL31ds;
031700080409         I31cdi = DUTdis;
031800080409         I31car = DUTare;
031900080409         I31cpo = DUTpou;
032000080409         trul31r(kpjba : trul31ds);
032100080410
032200080410         // Aggancio dati generali della tabella in esame
032300080410         clear k_TBEcod;
032400080410         k_TBEke1 = *zero;
032500080410         %subst(k_TBEke1 : %len(k_TBEke1)-%len(C_tab)+1 : %len(C_tab))
032600080410                = C_tab;
032700080410         clear k_TBEke2;
032800080410         clear k_TBElin;
032900080410         k_TBEsif = KNSIF;
033000080411         chain  %kds(K05tntbe01) TNTBE000;
033100080410         if not %found(TNTBE01L);
033200080410           clear  k_TBEsif;
033300080411           chain  %kds(K05tntbe01) TNTBE000;
033400080410         endif;
033500080410         if %found(TNTBE01L);
033600080410           xTNTBEds = TNTBEds;
033700080410         else;
033800080410           clear xTNTBEds;
033900080410         endif;
034000080409
034100080409       ENDSR;
034200080409
034300080409       //--------------------------------------------------------------
034400080409       //?Reperimento Dati del job (Utente/Operativi).
034500080409       //--------------------------------------------------------------
034600080409       BEGSR sr_DatiJob;
034700080409
034800080409         in(E) §AzUte;
034900080409         if NOT %error;
035000080409           in(E) §DatiUte;
035100080409         endif;
035200080409         if %error or RSut = *blanks;
035300080409           clear TIBS34ds;
035400080409           tibs34r(tibs34ds);
035500080409           in §AzUte;
035600080409           in §DatiUte;
035700080409         endif;
035800080526
035900080526         dUte01 = UTEfaf;
036000080409
036100080409       ENDSR;
036200080409
036300080409       //--------------------------------------------------------------
036400080409       //?Gestione videata S01
036500080409       //--------------------------------------------------------------
036600080409       BEGSR sr_GesS01;
036700080409
036800080409         // Inizializzazione videata
036900080409         if  $InzS01   = *on;
037000080409           exsr sr_InzS01;
037100080409           $InzS01  = *off;
037200080409         endif;
037300080411
037400080411         // Emissione Testata e Piede con tasti funzionali abilitati
037500080411         if  errGenerico = *off;
037600090706           write TB64T01;
037700090706           write TB64P01;
037800080411         endif;
037900080409
038000080409         // Posizionamento cursore
038100080409         if  C1CsrRrn  > *zero;
038200080410           C1RcdNbr = C1CsrRrn;
038300080409         else;
038400080409           C1RcdNbr = 1;
038500080411           write TB64S00;              // (no rec.)
038600080409         endif;
038700080409
038800080409         // Emissione videata
038900080409         exfmt TB64C01;
039000080409       //C1RcdNbr = min_nrr;
039100080409         reset ErrMessage;
039200080409         reset ErrGenerico;
039300080409         clear V1Dmsg;
039400080409
039500080409         SELECT;
039600080526
039700080526         // - Utente NON abilitato
039800080701           WHEN  §UTEpwd <> 'S';
039900080526             exsr sr_F03S01;
040000080409
040100080409         // - F3=Fine
040200080409           WHEN  dsp_aid = c_F03;
040300080410             exsr sr_F03S01;
040400090706
040500090706         // - F8=Password SEDE
040600090706           WHEN  dsp_aid = c_F08;
040700090706             exsr sr_F08S01;
040800080409
040900080409         // - F12=Ritorno
041000080409         //WHEN  dsp_aid = c_F12;
041100080409         //  exsr sr_F12S01;
041200080409
041300080409         // - F18=Modifica Filiale in Gestione
041400080409           WHEN  dsp_aid = c_F18;
041500080410             F18Attivo    = *off;
041600080410             NoProtectFgs = *on;
041700080411             PosCurFGS    = *on;
041800080409
041900080410           WHEN NoProtectFgs = *on;
042000080409             exsr sr_CtrFGS;
042100080409             if ErrGenerico;
042200080409               leavesr;
042300080409             endif;
042400080411             F18Attivo    = *on;
042500080411             NoProtectFgs = *off;
042600080411             $InzS01      = *on;
042700080409
042800080409         ENDSL;
042900080409
043000080409         SELECT;
043100080409
043200150112         // - F5=Refresh
043300080409           WHEN  dsp_aid = c_F05;
043400080409             reset $InzS01;
043500080409
043600080409         // - F10=Inserimento
043700080409           WHEN  dsp_aid = c_F10;
043800080411             exsr sr_F10S01;
043900080409
044000080409         // - Roll-Up
044100080409           WHEN  dsp_aid = c_RollUp;
044200080409           // - Verifica se raggiunto il limite di records registrabili
044300080409           //   nel subfile (9999)
044400080409             if  S01nrr = *hival;
044500080409               ErrMessage  = *on;
044600080409               ErrGenerico = *on;
044700080409               V1Dmsg = $Msg(02);
044800080409             else;
044900080409               exsr sr_RollUpS01;
045000080409             endif;
045100080409
045200080409         // Invio
045300080411           WHEN  S01nrr = *zero;
045400080409           OTHER;
045500080409             exsr sr_OpzS01;
045600080409             if  ErrGenerico = *on;
045700080409               leavesr;
045800080409             endif;
045900080409
046000080409         ENDSL;
046100080409
046200080409       ENDSR;
046300080409
046400080409       //--------------------------------------------------------------
046500080409       //?Controllo Filiale di Gestione
046600080409       //--------------------------------------------------------------
046700080409       BEGSR sr_CtrFGS;
046800080409
046900080409         WindDspF = IndDspF;
047000080409         reset WindDspFb;
047100080409         IndDspF  = WindDspF;
047200080409
047300080410         clear C1Dfgs;
047400080409
047500080409         if  %subst(KNMUS : 1 : 3) <> 'EDP'   and
047600080410             %lookup(C1Cfgs : $Pog) = *zero;
047700080409            PosCurFGS   = *on;
047800080409            ErrMessage  = *on;
047900080409            ErrGenerico = *on;
048000080409            V1Dmsg      = $Msg(01);
048100080418            leavesr;
048200080409          endif;
048300090706
048400090706          // se inserita filiale 046 sempre errore, la psw di SEDE
048500090706          // va gestita solo da EDP e con un altro programma richiamato
048600090706          // con tasto funzione
048700090706          if  c1cfgs = 046;
048800090706            PosCurFGS   = *on;
048900090706            ErrMessage  = *on;
049000090706            ErrGenerico = *on;
049100090706            V1Dmsg      = $Msg(01);
049200090706            leavesr;
049300090706          endif;
049400080409
049500080409          // Decodifica
049600080409          chain  C1Cfgs  AZORG;
049700080409          if  %found(AZORG01L)  and  ORGfva = *blank;
049800080409            C1Dfgs = ORGdes;
049900080418          else;
050000080418            PosCurFGS   = *on;
050100080418            ErrMessage  = *on;
050200080418            ErrGenerico = *on;
050300080418            V1Dmsg      = $Msg(01);
050400080418            leavesr;
050500080409          endif;
050600080409
050700080409          reset  $InzS01;
050800080409
050900080409       ENDSR;
051000080409
051100080409       //--------------------------------------------------------------
051200080409       //?Inizializzazione videata S01
051300080409       //--------------------------------------------------------------
051400080409       BEGSR sr_InzS01;
051500080409
051600080409       // Pulizia subfile
051700080409         SflDsp_N    = *on;
051800080409         SflDspCtl_N = *on;
051900080409         write  TB64C01;
052000080409         SflDspCtl_N = *off;
052100080409         SflEnd      = *off;
052200080409
052300080409         clear W_SflPag;
052400080409         clear C1RcdNbr;
052500080409         clear C1CsrRrn;
052600080409         clear S01nrr;
052700080409         clear V1Dmsg;
052800080409         ErrMessage  = *off;
052900080409         ErrGenerico = *off;
053000080409         WindDspF    = IndDspF;
053100080409         reset WindDspFb;
053200080409         IndDspF     = WindDspF;
053300140519         VisualizzaPwd = *off;
053400080526
053500080526       // Utente NON abilitato
053600080701         if  §UTEpwd  <> 'S';
053700080526           ErrMessage  = *on;
053800080526         //ErrGenerico = *on;
053900080526           PosCurOpz   = *on;
054000080526           V1Dmsg = $Msg(16);
054100080526           leavesr;
054200080526         endif;
054300080409
054400080409       // Caricamento dei dati da presentare nel sfl
054500081118       // carico le prime 11 password
054600080411         k_TBEcod = c_Tab;
054700080411         k_TBEke1 = %editc(C1Cfgs : 'X');
054800081118         k_TBEke2 = '1';
054900080414         k_TBEsif = *blank;
055000080411         k_TBElin = *blank;
055100080414         chain  %kds(k05tntbe01)  TNTBE000;
055200080414
055300080512         reset $PwdSpc;
055400080414         clear $PSP;
055500080414         clear dPSP;
055600080414         if  %found(TNTBE01L);
055700080414           dPSP = TBEuni;
055800080414         endif;
055900080409
056000080414         exsr sr_RollUpS01;
056100081118
056200081118       // Caricamento dei dati da presentare nel sfl
056300081118       // carico le sucessive 11 password
056400081118         k_TBEcod = c_Tab;
056500081118         k_TBEke1 = %editc(C1Cfgs : 'X');
056600081118         k_TBEke2 = '2';
056700081118         k_TBEsif = *blank;
056800081118         k_TBElin = *blank;
056900081118         chain  %kds(k05tntbe01)  TNTBE000;
057000081118         clear dPSP;
057100081118         if  %found(TNTBE01L);
057200081118           dPSP = TBEuni;
057300081118         endif;
057400081118
057500081118       // se non ho caricato niente con le prime 11 password azzero le pagine
057600081118         if s01nrr = 0;
057700081118          clear w_sflpag;
057800081118         endif;
057900081118
058000081118         exsr sr_RollUpS01;
058100080409
058200080409       // Impaginazione subfile
058300080409       // -> forza l'impaginazione sul 1° rec. del subfile
058400080409         if S01nrr > *zero;
058500080409           C1RcdNbr  = 1;
058600080409           C1CsrRrn  = 1;
058700080409         else;
058800080409           clear C1RcdNbr;
058900080409         endif;
059000080409
059100080409       ENDSR;
059200080409
059300080409       //--------------------------------------------------------------
059400080409       //?Caricamento singola pagina S01
059500080409       //--------------------------------------------------------------
059600080414       BEGSR sr_RollUpS01;
059700080409
059800080409         S01nrr    = (W_SflPag * C_SflPag);
059900080409         W_SflPag += 1;
060000080409         SflNxtChg = *off;
060100080414
060200080418         clear xx;
060300080414
060400080414         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
060500080414         IF  %found(TNTBE01L);
060600080409
060700080418           // - Caricamento schiera $PSP
060800081118           DOW  xx     < C_SflPag   ;
060900080414
061000080414             dPSP_sing = %subst( dPSP :
061100080414                                 xx * (%len(dPSP_sing)) + 1 :
061200080414                                 %len(dPSP_sing) );
061300080418             xx   += 1;
061400080414             if  §PSPcod > *zero;
061500080414               $PSP( %int( §PSPcod ) ) = dPSP_sing;
061600080512               $PwdSpc( %int( §PSPcod ) ) = §PSPpwd;
061700080414             endif;
061800080414
061900080414           ENDDO;
062000080418
062100080418           // - Caricamento dati nel subfile
062200080418           clear xx;
062300081118           // se ho agganciato la key '2' inizio a caricare dalla 12
062400081118           if k_tbeke2 = '2';
062500081118            xx = 11;
062600081118           endif;
062700081118
062800081118           DOW  xx     < %elem($PSP)            and
062900080418                S01nrr < (W_SflPag * C_SflPag)  and
063000080418                S01nrr < *hival;
063100080418
063200080418             xx   += 1;
063300080418             dPSP_sing = $PSP(xx);
063400080418             if  §PSPcod > *zero;
063500080418               exsr  sr_RieS01;
063600080418               S01nrr += 1;
063700080418               write TB64S01;
063800080418             endif;
063900080418
064000080418           ENDDO;
064100080414
064200080414         ENDIF;
064300080409
064400080409         // Visualizzazione del SFL (se ci sono dati)
064500080409         SflDsp_N  = (S01nrr <= *zeros);
064600080409
064700080414         // Attivazione (comunque) del SFLEND
064800080414         SflEnd    = *on;
064900080409
065000080409         // Posizionamento cursore al 1° rcd della pagina
065100080409         if  S01nrr > *zero;
065200080409           wPag     = S01nrr / C_SflPag;
065300080409           wRig     = S01nrr - (C_SflPag * wPag);
065400080409           C1RcdNbr = wPag * C_SflPag;
065500080409           if  wRig > *zeros;
065600080409             C1RcdNbr += 1;
065700080409           else;
065800080409             C1RcdNbr = C1RcdNbr - C_SflPag + 1;
065900080409           endif;
066000080409         else;
066100080409           clear  C1RcdNbr;
066200080409         endif;
066300080409
066400080409         C1CsrRrn = C1RcdNbr;
066500080409
066600080409       ENDSR;
066700080409
066800080409       //--------------------------------------------------------------
066900080409       //?Caricamento singolo record nel SubFile S01
067000080409       //--------------------------------------------------------------
067100080409       BEGSR sr_RieS01;
067200080409
067300080409         clear  TB64S01;
067400080409
067500080409         // Campi di solo output
067600080414         S1pspcod = %int( §PSPcod );
067700080414         S1pspdes = §PSPdes;
067800140519         if  %subst( KNMUS : 1 :3 ) = 'EDP';
067900140519           S1pspPwd = §PSPpwd;
068000140519         endif;
068100140519
068200140519         VisualizzaPwd = ( %subst( KNMUS : 1 :3 ) = 'EDP' );
068300080409
068400080409       ENDSR;
068500080409
068600080409       //--------------------------------------------------------------
068700080409       //?Gestione tasto funzionale F3 da videata S01
068800080409       //?F3=Fine
068900080409       //--------------------------------------------------------------
069000080409       BEGSR sr_F03S01;
069100080409
069200080409         // Chiusura del programma
069300080409         $Fine = *on;
069400080409
069500080409       ENDSR;
069600090706
069700090706       //--------------------------------------------------------------
069800090706       //?Gestione tasto funzionale F8 da videata S01
069900090706       //?F8=Password SEDE
070000090706       //--------------------------------------------------------------
070100090706       BEGSR sr_F08S01;
070200090706
070300090706         // richiamo del programma per la gestione password di SEDE
070400090706         tntb64r1(kpjba);
070500090707         $InzS01  = *on;
070600090706
070700090706       ENDSR;
070800080409
070900080409       //--------------------------------------------------------------
071000080409       //?Gestione tasto funzionale F10 da videata S01
071100080409       //?F10=Immissione
071200080409       //--------------------------------------------------------------
071300080411       BEGSR sr_F10S01;
071400080409
071500080409         $Video = 'D1';
071600080409         reset $InzD01;
071700080409
071800080409       ENDSR;
071900080409
072000080409       //--------------------------------------------------------------
072100080409       //?Gestione opzioni subfile
072200080409       //--------------------------------------------------------------
072300080409       BEGSR sr_OpzS01;
072400080409
072500080409         readc TB64S01;
072600080409
072700080414         DOW  NOT  %eof(TNTB64D);
072800080409
072900080409           SflNxtChg = *off;
073000080409           WindDspF  = IndDspF;
073100080409           reset WindDspFb;
073200080409           IndDspF   = WindDspF;
073300080409           C1RcdNbr  = S01nrr;
073400080409
073500080409           SELECT;
073600080409
073700080409             // - Nessuna opzione
073800080410             WHEN  S1opzione = *zero;
073900080409
074000080409             // - Gestione singola opzione
074100080410             WHEN  S1opzione = 2   or
074200080410                   S1opzione = 5;
074300080409               $Video = 'D1';
074400080409               reset  $InzD01;
074500080410               dow  $Video = 'D1';
074600080410                 exsr sr_GesD01;
074700080410               enddo;
074800080409
074900080409             // - ? = Opzione NON valida
075000080409             OTHER;
075100080409               ErrMessage  = *on;
075200080409               ErrGenerico = *on;
075300080409               PosCurOpz   = *on;
075400080409               V1Dmsg = $Msg(03);
075500080409
075600080409           ENDSL;
075700080409
075800080409           // Aggiornamento sfl
075900080409           select;
076000080409             when  ErrMessage  = *on;
076100080409               SflNxtChg = *on;
076200080409               C1CsrRrn  = C1RcdNbr;
076300080409             when  ErrGenerico = *on;
076400080409               C1CsrRrn  = C1RcdNbr;
076500080410               clear  S1opzione;
076600080409             other;
076700080409               C1CsrRrn  = C1RcdNbr;
076800080410               clear  S1opzione;
076900080409           endsl;
077000080409
077100080409           update TB64S01;
077200080409
077300080409           if  errMessage = *on  or  errGenerico = *on;
077400080409             leave;
077500080409           endif;
077600080409
077700080410           readc TB64S01;
077800080409
077900080409         ENDDO;
078000080409
078100080409       ENDSR;
078200080409
078300080409       //--------------------------------------------------------------
078400080409       //?Gestione videata D01
078500080409       //--------------------------------------------------------------
078600080409       BEGSR sr_GesD01;
078700080409
078800080409         // Inizializzazione videata
078900080409         if $InzD01 = *on;
079000080409           exsr sr_InzD01;
079100080409           $InzD01 = *off;
079200080409         endif;
079300080409
079400080409         // Emissione videata
079500080411         write TB64T01;
079600080414         if  S1opzione = 5;
079700080411           write TB64D01;
079800080410           exfmt Protect;
079900080410         else;
080000080411           exfmt TB64D01;
080100080410         endif;
080200080414         ErrMessage  = *off;
080300080414         ErrGenerico = *off;
080400080409         clear V1Dmsg;
080500080409
080600080410         SELECT;
080700080409
080800080409           // F3=Fine
080900080410           WHEN dsp_aid = c_F03;
081000080411             exsr sr_F03D01;
081100080409
081200080409           // F12=Ritorno
081300080410           WHEN dsp_aid = c_F12;
081400080409             exsr sr_F12D01;
081500080409
081600080411           // Nessun controllo se visualizzazione
081700080414           WHEN S1opzione = 5;
081800080411
081900080414           // Enter o F6
082000080410           OTHER;
082100080410             // - Controllo dati
082200080414             exsr sr_CtrD01;
082300080414             if errGenerico;
082400080414               leavesr;
082500080414             endif;
082600080414
082700080411             // - Conferma dati:
082800080411             //   F6=Conferma
082900080414             If  dsp_aid = c_F06;
083000080411               $Video  = 'W1';
083100080411               reset  $InzW01;
083200080411               dow  $Video = 'W1';
083300080411                 exsr sr_GesW01;
083400080411               enddo;
083500080410             EndIf;
083600080409
083700080410         ENDSL;
083800080409
083900080409       ENDSR;
084000080409
084100080409       //--------------------------------------------------------------
084200080409       //?Inizializzazione videata D01
084300080409       //--------------------------------------------------------------
084400080409       BEGSR sr_InzD01;
084500080409
084600080409         clear TB64D01;
084700080418         clear dPSP_sing;
084800080410
084900080410         // REimpostazione indicatori per abilitazione tasti funzionali
085000080410         exsr Set_Ind_D01;
085100080409
085200080409         // Premuto F10=Inserimento
085300080409         if  dsp_aid = c_F10;
085400080409           D1DesOpz  = '  INSERIMENTO  ';
085500080414           D1cfgs    = C1Cfgs;
085600080414           D1dfgs    = C1Dfgs;
085700080414           NoProtectCod = *on;
085800080409           leavesr;
085900080409         endif;
086000080409
086100080409         // Selezionato record di TNTBE00F
086200080414         //  (già fatto nella subr. "sr_InzS01")
086300080414         //k_TBEcod = c_Tab;
086400080414         //k_TBEke1 = %editc(C1Cfgs : 'X');
086500080414         //clear k_TBEke2;
086600080414         //clear k_TBElin;
086700080414         //clear k_TBEsif;
086800080414         //chain  %kds(K05tntbe01) TNTBE000;
086900080414         //if  %found(TNTBE01L);
087000080414         //  dPSP = TBEuni;
087100080414         //endif;
087200080414
087300080418         //dPSP_sing = %subst( dPSP :
087400080418         //                    (S1pspcod - 1) * (%len(dPSP_sing)) + 1 :
087500080418         //                    %len(dPSP_sing) );
087600080418         dPSP_sing = $PSP(S1pspcod);
087700080409
087800080409         // Intestazione videata
087900080409         select;
088000080411           when  S1opzione = 2;
088100080409             D1DesOpz  = '   MODIFICA    ';
088200080414             NoProtectCod  = *off;
088300080411           when  S1opzione = 5;
088400080410             D1DesOpz  = 'VISUALIZZAZIONE';
088500080414           other;
088600080414             clear D1DesOpz;
088700080409         endsl;
088800080409
088900080411         //D1Cfgs   = %int( %subst( TBEke1 : 1 : 3 ) );
089000080409         D1cfgs   = C1Cfgs;
089100080409         D1dfgs   = C1Dfgs;
089200080414         D1pspcod = %int( §PSPcod );
089300080409         D1pspdes = §PSPdes;
089400080409
089500080409       ENDSR;
089600080410
089700080410       //--------------------------------------------------------------
089800080410       //?Settaggio indicatori nella videata D01 per abilitazione
089900080410       //?  tasti funzionali
090000080410       //--------------------------------------------------------------
090100080410       BEGSR Set_Ind_D01;
090200080410
090300080411         F6Attivo     = (S1opzione <> 5);
090400080410
090500080410       ENDSR;
090600080411
090700080411       //--------------------------------------------------------------
090800080411       //?Gestione tasto funzionale F3 da videata D01
090900080411       //?F3=Fine
091000080411       //--------------------------------------------------------------
091100080411       BEGSR sr_F03D01;
091200080411
091300090707         // Chiusura del programma
091400090707         $Video  = 'S1';
091500090707         $Fine = *on;
091600080411
091700080411       ENDSR;
091800080409
091900080409       //--------------------------------------------------------------
092000080411       //?Gestione tasto funzionale F12 da videata D01
092100080411       //?F12=Ritorno
092200080409       //--------------------------------------------------------------
092300080409       BEGSR sr_F12D01;
092400080409
092500080411         // Ritorno alla videata S01
092600080409         $Video = 'S1';
092700080409
092800080409       ENDSR;
092900080409
093000080409       //--------------------------------------------------------------
093100080409       //?Controllo videata D01
093200080409       //--------------------------------------------------------------
093300080409       BEGSR sr_CtrD01;
093400080409
093500080409         WindDspF = IndDspF;
093600080409         reset WindDspFb;
093700080409         IndDspF  = WindDspF;
093800080410
093900080410         // REimpostazione indicatori per abilitazione tasti funzionali
094000080410         exsr Set_Ind_D01;
094100080409
094200080409         SELECT;
094300080409
094400080410         // Controllo codice SPC
094500080414           WHEN  D1pspcod   =  *zero;
094600080414             ErrMessage  = *on;
094700080414             ErrGenerico = *on;
094800080414             PosCurCod   = *on;
094900080410             V1Dmsg      = $Msg(04);
095000080409             leavesr;
095100080414           WHEN  D1pspcod   <  *zero   or
095200081118                 D1pspcod   >  22;
095300080414             ErrMessage  = *on;
095400080414             ErrGenerico = *on;
095500080414             PosCurCod   = *on;
095600080410             V1Dmsg      = $Msg(05);
095700080410             leavesr;
095800080411
095900080411         ENDSL;
096000080411
096100080411         // Controllo inesistenza codice chiave se F10=Inserimento
096200080418         IF  NoProtectCod = *on;
096300080418           xx = D1pspcod;
096400080418           dPSP_sing = $PSP(D1pspcod);
096500080418           if  §PSPcod > *zero;
096600080411             errMessage  = *on;
096700080411             errGenerico = *on;
096800080414             PosCurCod   = *on;
096900080411             V1Dmsg      = $Msg(06);
097000080411             leavesr;
097100080411           endif;
097200080411         ENDIF;
097300080409
097400080411         SELECT;
097500080411
097600080410         // Controllo descrizione
097700080410           WHEN  D1PSPdes   =  *blank;
097800080409             errMessage  = *on;
097900080409             errGenerico = *on;
098000080410             PosCurDES   = *on;
098100080411             V1Dmsg      = $Msg(07);
098200080409             leavesr;
098300080409
098400080414         // Controllo Password SPC corrente
098500080410           WHEN  D1PSPpwdO  =  *blank   and
098600080414                 §PSPpwd    >  *zero;
098700080409             errMessage  = *on;
098800080409             errGenerico = *on;
098900080410             PosCurPWDo  = *on;
099000080411             V1Dmsg      = $Msg(08);
099100080411             clear  D1PSPpwdO;
099200080409             leavesr;
099300080414           WHEN  D1PSPpwdO  <> §PSPpwd   and
099400080414                 §PSPpwd    >  *zero;
099500080410             errMessage  = *on;
099600080410             errGenerico = *on;
099700080410             PosCurPWDo  = *on;
099800080411             V1Dmsg      = $Msg(09);
099900080411             clear  D1PSPpwdO;
100000080410             leavesr;
100100080411
100200080410         // Controllo Nuova password SPC
100300080410           WHEN  D1PSPpwdN1 =  *blank;
100400080410             errMessage  = *on;
100500080410             errGenerico = *on;
100600080410             PosCurPWDn1 = *on;
100700080411             V1Dmsg      = $Msg(10);
100800080411             clear  D1PSPpwdN1;
100900080410             leavesr;
101000080414           WHEN  D1PSPpwdN1 =  D1PSPpwdO;
101100080410             errMessage  = *on;
101200080410             errGenerico = *on;
101300080410             PosCurPWDn1 = *on;
101400080411             V1Dmsg      = $Msg(11);
101500080411             clear  D1PSPpwdN1;
101600080410             leavesr;
101700080411
101800080411           // - La password deve essere composta da 4 cifre:
101900080411           WHEN  %len( %trimr( D1PSPpwdN1 ) ) <> C_LenPwdPSP;
102000080411             errMessage  = *on;
102100080411             errGenerico = *on;
102200080411             PosCurPWDn1 = *on;
102300080411             V1Dmsg      = $Msg(12);
102400080411             clear  D1PSPpwdN1;
102500080411             leavesr;
102600080411
102700080414           WHEN  %checkr( C_Digit : D1PSPpwdN1 : C_LenPwdPSP ) > *zero;
102800080411             errMessage  = *on;
102900080411             errGenerico = *on;
103000080411             PosCurPWDn1 = *on;
103100080411             V1Dmsg      = $Msg(12);
103200080411             clear  D1PSPpwdN1;
103300080411             leavesr;
103400080512
103500080512           // - La password NON può coincidere con quella di un altro
103600080512           //   SPC della stessa filiale
103700080512           WHEN  %lookup( D1PSPpwdN1 : $PwdSpc ) > *zero;
103800080512             errMessage  = *on;
103900080512             errGenerico = *on;
104000080512             PosCurPWDn1 = *on;
104100080512             V1Dmsg      = $Msg(13);
104200080512             clear  D1PSPpwdN1;
104300080512             leavesr;
104400080410
104500080410         // Controllo Nuova password SPC (per conferma)
104600080410           WHEN  D1PSPpwdN2 =  *blank;
104700080410             errMessage  = *on;
104800080410             errGenerico = *on;
104900080410             PosCurPWDn2 = *on;
105000080512             V1Dmsg      = $Msg(14);
105100080410             leavesr;
105200080410           WHEN  D1PSPpwdN2 <> D1PSPpwdN1;
105300080410             errMessage  = *on;
105400080410             errGenerico = *on;
105500080410             PosCurPWDn2 = *on;
105600080512             V1Dmsg      = $Msg(15);
105700080410             leavesr;
105800080409
105900080410         ENDSL;
106000080409
106100080409       ENDSR;
106200080409
106300080409       //--------------------------------------------------------------
106400080410       //?Gestione trasmissione aggiornamento
106500080409       //--------------------------------------------------------------
106600080410       BEGSR sr_GesW01;
106700080409
106800080410         // Inizializzazione videata
106900080410         if $InzW01 = *on;
107000080410           exsr sr_InzW01;
107100080410           $InzW01 = *off;
107200080410         endif;
107300080410
107400080410         // Emissione window
107500080411         if  TBXftt = 'S';
107600080411           exfmt TB64W01;
107700080411           ErrMessage  = *off;
107800080411           ErrGenerico = *off;
107900080411           clear V1Dmsg;
108000110421           Select;
108100110421             // -?F12=Ritorno (gestito solo se emesso W01)?
108200110421             When  dsp_aid = c_F12;
108300110421               exsr  F12W01;
108400110421               leavesr;
108500110421             // -?Invio (gestito solo se emesso W01)?
108600110421             When  dsp_aid <> c_F06;
108700110421               leavesr;
108800110421           EndSl;
108900080411         endif;
109000080410
109100110421         // -?Aggiornamento TNTBE00F?
109200110421         exsr Upd_TNTBE;
109300110421         // Ritorno alla videata S01
109400110421         if  NOT  errGenerico;
109500110421           $Video  = 'S1';
109600110421           $InzS01 = *on;
109700110421         endif;
109800080410
109900080410       ENDSR;
110000080410
110100080410       //--------------------------------------------------------------
110200080410       //?Inizializzazione videata W01
110300080410       //--------------------------------------------------------------
110400080410       BEGSR sr_InzW01;
110500080410
110600080410         clear TB64W01;
110700080410
110800080414         if  NOT %found(TNTBE01L);
110900080414
111000080414           // Se immissione
111100080414           W1ftt  = TBXftt;
111200080410
111300080414         else;
111400080414
111500080414           // Se NON immissione:
111600080414           // visualizza i dati relativi all'ultima trasmissione
111700080414           W1ftt  = TBEftt;
111800080410           W1flt  = TBEflt;
111900080410           W1ftr  = TBEftr;
112000080410           if TBEdtr <> *zero;
112100080410             wDate_EUR = %date(TBEdtr : *iso);
112200080410             W1dtr     = %dec(wDate_EUR);
112300080410           endif;
112400080414
112500080410         endif;
112600080410
112700080410       ENDSR;
112800080410
112900080410       //--------------------------------------------------------------
113000080410       //?Gestione tasto funzionale F12 da videata W01
113100080410       //?F12=Ritorno
113200080410       //--------------------------------------------------------------
113300080410       BEGSR F12W01;
113400080410
113500110421         // Ritorno alla videata precedente
113600080410           $Video = 'D1';
113700080410
113800080410       ENDSR;
113900080410
114000080410       //--------------------------------------------------------------
114100080410       //?Aggiornamento singolo record TNTBE00F (tab. PSP)
114200080410       //--------------------------------------------------------------
114300080410       BEGSR Upd_TNTBE;
114400080410
114500080414         // Aggancio record di TNTBE00F
114600081118         // e scrivo/aggiorno le prime 11 password
114700081118           k_TBEcod  = c_Tab;
114800081118           k_TBEke1  = %editc(D1Cfgs   : 'X');
114900081118           k_TBEke2  = '1';
115000081118           clear k_TBElin;
115100081118           clear k_TBEsif;
115200081118
115300081118           chain  %kds(K05tntbe01) TNTBE000;
115400080410
115500080410         // Impostazione dati
115600080414         §PSPcod  = %editc(D1pspcod : 'X');
115700080414         §PSPdes  = D1pspdes;
115800080414         §PSPpwd  = %subst( D1psppwdN1 : 1 : %len(§PSPpwd) );
115900080414         §PSPchar = c_Char;
116000080414         $PSP(D1pspcod) = dPSP_sing;
116100080414
116200080414         clear dPSP;
116300080430         clear wx;
116400080414         clear yy;
116500080430
116600080430         // Individuazione dell'ultimo elemento per evitarne
116700080430         //   l'impostazione del carattere "#" finale
116800081118         for xx = 1 TO %elem($psp);
116900080430           dPSP_sing  = $PSP(xx);
117000080430           if  §PSPcod  > *zero;
117100080430             wx = xx;
117200080430           endif;
117300080430         endfor;
117400080415
117500080430         // Riempimento dati tabella
117600081118         FOR xx = 1 TO C_SflPag;
117700080415
117800080414           dPSP_sing  = $PSP(xx);
117900080415
118000080414           If  §PSPcod  > *zero;
118100080415
118200080414             yy += 1;
118300080414             select;
118400080414               when  yy = 1;
118500080414                 §PSPcod01  = §PSPcod;
118600080414                 §PSPdes01  = §PSPdes;
118700080414                 §PSPpwd01  = §PSPpwd;
118800080430                 if xx < wx;
118900080430                   §PSPchar01 = c_Char;
119000080430                 endif;
119100080414               when  yy = 2;
119200080414                 §PSPcod02  = §PSPcod;
119300080414                 §PSPdes02  = §PSPdes;
119400080414                 §PSPpwd02  = §PSPpwd;
119500080430                 if xx < wx;
119600080430                   §PSPchar02 = c_Char;
119700080430                 endif;
119800080414               when  yy = 3;
119900080414                 §PSPcod03  = §PSPcod;
120000080414                 §PSPdes03  = §PSPdes;
120100080414                 §PSPpwd03  = §PSPpwd;
120200080430                 if xx < wx;
120300080430                   §PSPchar03 = c_Char;
120400080430                 endif;
120500080414               when  yy = 4;
120600080414                 §PSPcod04  = §PSPcod;
120700080414                 §PSPdes04  = §PSPdes;
120800080414                 §PSPpwd04  = §PSPpwd;
120900080430                 if xx < wx;
121000080430                   §PSPchar04 = c_Char;
121100080430                 endif;
121200080414               when  yy = 5;
121300080414                 §PSPcod05  = §PSPcod;
121400080414                 §PSPdes05  = §PSPdes;
121500080414                 §PSPpwd05  = §PSPpwd;
121600080430                 if xx < wx;
121700080430                   §PSPchar05 = c_Char;
121800080430                 endif;
121900080414               when  yy = 6;
122000080414                 §PSPcod06  = §PSPcod;
122100080414                 §PSPdes06  = §PSPdes;
122200080414                 §PSPpwd06  = §PSPpwd;
122300080430                 if xx < wx;
122400080430                   §PSPchar06 = c_Char;
122500080430                 endif;
122600080414               when  yy = 7;
122700080414                 §PSPcod07  = §PSPcod;
122800080414                 §PSPdes07  = §PSPdes;
122900080414                 §PSPpwd07  = §PSPpwd;
123000080430                 if xx < wx;
123100080430                   §PSPchar07 = c_Char;
123200080430                 endif;
123300080414               when  yy = 8;
123400080414                 §PSPcod08  = §PSPcod;
123500080414                 §PSPdes08  = §PSPdes;
123600080414                 §PSPpwd08  = §PSPpwd;
123700080430                 if xx < wx;
123800080430                   §PSPchar08 = c_Char;
123900080430                 endif;
124000080414               when  yy = 9;
124100080414                 §PSPcod09  = §PSPcod;
124200080414                 §PSPdes09  = §PSPdes;
124300080414                 §PSPpwd09  = §PSPpwd;
124400080430                 if xx < wx;
124500080430                   §PSPchar09 = c_Char;
124600080430                 endif;
124700080414               when  yy = 10;
124800080414                 §PSPcod10  = §PSPcod;
124900080414                 §PSPdes10  = §PSPdes;
125000080414                 §PSPpwd10  = §PSPpwd;
125100080430                 if xx < wx;
125200080430                   §PSPchar10 = c_Char;
125300080430                 endif;
125400081118               when  yy = 11;
125500081118                 §PSPcod11  = §PSPcod;
125600081118                 §PSPdes11  = §PSPdes;
125700081118                 §PSPpwd11  = §PSPpwd;
125800081118                 if xx < wx;
125900081118                   §PSPchar11 = c_Char;
126000081118                 endif;
126100080414             endsl;
126200080415
126300080414           EndIf;
126400080415
126500080414         ENDFOR;
126600080410
126700080410         if  NOT %found(TNTBE01L);
126800080411           clear TNTBE000;
126900080411           TBEcod  = k_TBEcod;
127000080411           TBEke1  = k_TBEke1;
127100080411           TBEke2  = k_TBEke2;
127200080411           TBElin  = k_TBElin;
127300080411           TBEsif  = k_TBEsif;
127400080410         endif;
127500080415         TBEapl = TBXapl;
127600080415         TBEuni = dPSP;
127700080414         clear TBEatb;
127800080411         TBEftt = W1ftt;
127900080411         TBEFlt = W1flt;
128000080410         clear TBEftr;
128100080410       //clear TBEdtr;
128200080410
128300080410         // Aggiornamento record
128400080410         if  %found(TNTBE01L);
128500080410         //_______________
128600080410           UPDATE TNTBE000;
128700080410         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
128800080410         else;
128900080410         //_______________
129000080410           WRITE  TNTBE000;
129100080410         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
129200080410         endif;
129300081118
129400081118         // Aggancio record di TNTBE00F
129500081118         // e scrivo/aggiorno le sucessive 11 password
129600081118           k_TBEcod  = c_Tab;
129700081118           k_TBEke1  = %editc(D1Cfgs   : 'X');
129800081118           k_TBEke2  = '2';
129900081118           clear k_TBElin;
130000081118           clear k_TBEsif;
130100081118
130200081118           chain  %kds(K05tntbe01) TNTBE000;
130300081118
130400081118         // Impostazione dati
130500081118         §PSPcod  = %editc(D1pspcod : 'X');
130600081118         §PSPdes  = D1pspdes;
130700081118         §PSPpwd  = %subst( D1psppwdN1 : 1 : %len(§PSPpwd) );
130800081118         §PSPchar = c_Char;
130900081118         $PSP(D1pspcod) = dPSP_sing;
131000081118
131100081118         clear dPSP;
131200081118         clear yy;
131300081118
131400081118         // Riempimento dati tabella
131500090429         FOR xx = 12 TO %elem($psp);
131600081118
131700081118           dPSP_sing  = $PSP(xx);
131800081118
131900081118           If  §PSPcod  > *zero;
132000081118
132100081118             yy += 1;
132200081118             select;
132300081118               when  yy = 1;
132400081118                 §PSPcod01  = §PSPcod;
132500081118                 §PSPdes01  = §PSPdes;
132600081118                 §PSPpwd01  = §PSPpwd;
132700081118                 if xx < wx;
132800081118                   §PSPchar01 = c_Char;
132900081118                 endif;
133000081118               when  yy = 2;
133100081118                 §PSPcod02  = §PSPcod;
133200081118                 §PSPdes02  = §PSPdes;
133300081118                 §PSPpwd02  = §PSPpwd;
133400081118                 if xx < wx;
133500081118                   §PSPchar02 = c_Char;
133600081118                 endif;
133700081118               when  yy = 3;
133800081118                 §PSPcod03  = §PSPcod;
133900081118                 §PSPdes03  = §PSPdes;
134000081118                 §PSPpwd03  = §PSPpwd;
134100081118                 if xx < wx;
134200081118                   §PSPchar03 = c_Char;
134300081118                 endif;
134400081118               when  yy = 4;
134500081118                 §PSPcod04  = §PSPcod;
134600081118                 §PSPdes04  = §PSPdes;
134700081118                 §PSPpwd04  = §PSPpwd;
134800081118                 if xx < wx;
134900081118                   §PSPchar04 = c_Char;
135000081118                 endif;
135100081118               when  yy = 5;
135200081118                 §PSPcod05  = §PSPcod;
135300081118                 §PSPdes05  = §PSPdes;
135400081118                 §PSPpwd05  = §PSPpwd;
135500081118                 if xx < wx;
135600081118                   §PSPchar05 = c_Char;
135700081118                 endif;
135800081118               when  yy = 6;
135900081118                 §PSPcod06  = §PSPcod;
136000081118                 §PSPdes06  = §PSPdes;
136100081118                 §PSPpwd06  = §PSPpwd;
136200081118                 if xx < wx;
136300081118                   §PSPchar06 = c_Char;
136400081118                 endif;
136500081118               when  yy = 7;
136600081118                 §PSPcod07  = §PSPcod;
136700081118                 §PSPdes07  = §PSPdes;
136800081118                 §PSPpwd07  = §PSPpwd;
136900081118                 if xx < wx;
137000081118                   §PSPchar07 = c_Char;
137100081118                 endif;
137200081118               when  yy = 8;
137300081118                 §PSPcod08  = §PSPcod;
137400081118                 §PSPdes08  = §PSPdes;
137500081118                 §PSPpwd08  = §PSPpwd;
137600081118                 if xx < wx;
137700081118                   §PSPchar08 = c_Char;
137800081118                 endif;
137900081118               when  yy = 9;
138000081118                 §PSPcod09  = §PSPcod;
138100081118                 §PSPdes09  = §PSPdes;
138200081118                 §PSPpwd09  = §PSPpwd;
138300081118                 if xx < wx;
138400081118                   §PSPchar09 = c_Char;
138500081118                 endif;
138600081118               when  yy = 10;
138700081118                 §PSPcod10  = §PSPcod;
138800081118                 §PSPdes10  = §PSPdes;
138900081118                 §PSPpwd10  = §PSPpwd;
139000081118                 if xx < wx;
139100081118                   §PSPchar10 = c_Char;
139200081118                 endif;
139300081118               when  yy = 11;
139400081118                 §PSPcod11  = §PSPcod;
139500081118                 §PSPdes11  = §PSPdes;
139600081118                 §PSPpwd11  = §PSPpwd;
139700081118                 if xx < wx;
139800081118                   §PSPchar11 = c_Char;
139900081118                 endif;
140000081118             endsl;
140100081118
140200081118           EndIf;
140300081118
140400081118         ENDFOR;
140500081118
140600081118         if  NOT %found(TNTBE01L);
140700081118           clear TNTBE000;
140800081118           TBEcod  = k_TBEcod;
140900081118           TBEke1  = k_TBEke1;
141000081118           TBEke2  = k_TBEke2;
141100081118           TBElin  = k_TBElin;
141200081118           TBEsif  = k_TBEsif;
141300081118         endif;
141400081118         TBEapl = TBXapl;
141500081118         TBEuni = dPSP;
141600081118         clear TBEatb;
141700081118         TBEftt = W1ftt;
141800081118         TBEFlt = W1flt;
141900081118         clear TBEftr;
142000081118       //clear TBEdtr;
142100081118
142200081118         // Aggiornamento record
142300081118         if  %found(TNTBE01L);
142400081118         //_______________
142500081118           UPDATE TNTBE000;
142600081118         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
142700081118         else;
142800081118         //_______________
142900081118           WRITE  TNTBE000;
143000081118         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
143100081118         endif;
143200080409
143300080409       ENDSR;
143400080409
143500080409       //--------------------------------------------------------------
143600080409       //?Operazioni finali.
143700080409       //--------------------------------------------------------------
143800080409       BEGSR sr_RoutEnd;
143900080409
144000080409         *inLR = *on;
144100080409         return;
144200080409
144300080409       ENDSR;
144400080409
144500080409      /end-free
144600080409
144700080409       //--------------------------------------------------------------
144800080409       //?Schiere a tempo di compilazione.
144900080409       //--------------------------------------------------------------
145000080409
145100080409** - $MSG -------------------------------------------------------------------*
145200080409Filiale Gestione errata o non gestibile                                        01
145300080409Raggiunta l'ampiezza massima del subfile                                       02
145400080409Opzione errata                                                                 03
145500080410Codice SPC obbligatorio                                                        04
145600081118Codice SPC errato: può variare da 01 a 22                                      05
145700080411Codice SPC già inserito                                                        06
145800080411Descrizione obbligatoria                                                       07
145900080411Inserire la password attuale                                                   08
146000080411Password attuale errata                                                        09
146100080411Inserire la nuova password                                                     10
146200080411La nuova password deve essere diversa da quella attuale                        11
146300080411La password DEVE essere composta da 4 cifre                                    12
146400080512La password NON può coincidere con quella di un altro SPC                      13
146500080512Inserire la nuova password per la verifica                                     14
146600080512La password di verifica non coincide con la nuova appena inserita              15
146700080526Utente NON abilitato. Premere Invio per uscire dal programma.                  16
146800080701Codice SPC riservato e NON utilizzabile                                        17
146900080703Password errata                                                                18
