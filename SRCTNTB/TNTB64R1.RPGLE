000100080408       //--------------------------------------------------------------
000200090706       //?Gestione tabella "PSP" = Password SPC gestione PDA   SEDE
000300080408       //--------------------------------------------------------------
000400080408
000500080409     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600110421     h dftactgrp(*no)
000700080408
000800080409      //---------------------------------------------------------------
000900080409      //?Dichiarazione file.
001000080409      //---------------------------------------------------------------
001100080409
001200080409      // - Tabelle
001300080409     fTNTBE01L  Uf A e           k disk
001400080409
001500080410      // - Video Gestione tabella SPC
001600090706     fTNTB64D1  cf   e             workstn
001700080410     f                                     sfile(TB64S01 : S01nrr)
001800080409     f                                     indds(IndDspF)
001900080409     f                                     infds(InfDspF)
002000080409
002100080409      //---------------------------------------------------------------
002200080409      //?Definizione costanti.
002300080409      //---------------------------------------------------------------
002400080409
002500080409      // Codice tabella in gestione
002600080409     d c_Tab           c                   const('PSP')
002700080414
002800080414      // Carattere separatore
002900080414     d c_Char          c                   const('#')
003000080409
003100080409      // - Numero di record in ciascuna videata di subfile
003200090706     d c_SflPag        c                   const(01)
003300081118
003400081118      // - Numero di password possibili
003500090706     d c_Pwd           c                   const(01)
003600080409
003700080409      // - Tasti funzionali a video
003800080409     d c_F01           c                   const(x'31')
003900080409     d c_F02           c                   const(x'32')
004000080409     d c_F03           c                   const(x'33')
004100080409     d c_F05           c                   const(x'35')
004200080409     d c_F06           c                   const(x'36')
004300080409     d c_F07           c                   const(x'37')
004400080409     d c_F08           c                   const(x'38')
004500080409     d c_F09           c                   const(x'39')
004600080409     d c_F10           c                   const(x'3A')
004700080409     d c_F12           c                   const(x'3C')
004800080409     d c_F13           c                   const(x'B1')
004900080409     d c_F14           c                   const(x'B2')
005000080409     d c_F15           c                   const(x'B3')
005100080409     d c_F16           c                   const(x'B4')
005200080409     d c_F17           c                   const(x'B5')
005300080409     d c_F18           c                   const(x'B6')
005400080409     d c_F19           c                   const(x'B7')
005500080409     d c_F20           c                   const(x'B8')
005600080409     d c_F21           c                   const(x'B9')
005700080409     d c_F22           c                   const(x'BA')
005800080409     d c_F23           c                   const(x'BB')
005900080409     d c_F24           c                   const(x'BC')
006000080409     d c_Enter         c                   const(x'F1')
006100080409     d c_RollDown      c                   const(x'F4')
006200080409     d c_RollUp        c                   const(x'F5')
006300080411
006400080414     d C_Digit         c                   const('1234567890')
006500080409
006600080409      //---------------------------------------------------------------
006700080409      //?Definizione schiere.
006800080409      //---------------------------------------------------------------
006900080409
007000080409      // - Messaggi di errore
007100080701     d $Msg            s             78    dim(18) ctdata perrcd(1)
007200080409
007300080409      //---------------------------------------------------------------
007400080409      //?Definizione aree dati.
007500080409      //---------------------------------------------------------------
007600140520
007700140520       // -?Dati utente?
007800140520     d §AzUte        e ds                  extname(AZUTE00F)
007900140520     d                                     dtaara
008000140520     d §DatiUte      e ds                  extname(dDatiUte)
008100140520     d                                     dtaara
008200080409
008300080409      //---------------------------------------------------------------
008400080409      //?Definizione strutture dati.
008500080409      //---------------------------------------------------------------
008600080409
008700080409      // - Status
008800080409     d Psds           sds
008900080409     d   SDSpgm          *proc
009000080409
009100080409      // - InfDS
009200080409     d InfDspF         ds
009300080409     d  dsp_aid              369    369a                                        AID byte
009400080409     d  sfl_rrn              376    377i 0                                      Subfile rrn
009500080409     d  min_nrr              378    379i 0                                      Subfile min rrn
009600080409     d  num_rcds             380    381i 0                                      Subfile num rcds
009700080409
009800080409      // - Indicatori su DspF
009900080409     d IndDspF         ds
010000080410        // - Abilitazione tasti funzionali
010100080410     d  F6Attivo                      1n   overlay(IndDspF : 06)
010200080409        // - Indicatori di gestione del subfile
010300080409     d  SflDsp_N                      1n   overlay(IndDspF : 30)
010400080409     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
010500080409     d  SflNxtChg                     1n   overlay(IndDspF : 32)
010600080409     d  SflEnd                        1n   overlay(IndDspF : 33)
010700080410        // - Condizionamento visualizzazioni   &
010800080410        //   Condizionamento protezione
010900080410     d  NoProtectFgs                  1n   overlay(IndDspF : 40)
011000140519     d  VisualizzaPwd                  n   overlay(IndDspF : 41)
011100080414     d  NoProtectCod                  1n   overlay(IndDspF : 42)
011200080409        // - Indicatori di errore
011300080409     d  ErrMessage                    1n   overlay(IndDspF : 28)
011400080409     d  PosCurFGS                     1n   overlay(IndDspF : 50)
011500080409     d  PosCurOpz                     1n   overlay(IndDspF : 51)
011600080414     d  PosCurCOD                     1n   overlay(IndDspF : 52)
011700080409     d  PosCurDES                     1n   overlay(IndDspF : 53)
011800080409     d  PosCurPWDO                    1n   overlay(IndDspF : 54)
011900080409     d  PosCurPWDN1                   1n   overlay(IndDspF : 55)
012000080409     d  PosCurPWDN2                   1n   overlay(IndDspF : 56)
012100080409     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012200080409
012300080409      // - Parametri ricevuti
012400080409     d KPJBA         e ds
012500080410
012600080410      // - Dati record principale della tabella
012700080410     d TNTBEds       e ds                  inz  extname(TNTBE00F)
012800080410     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
012900080410     d                                          prefix(TBX:3)
013000080409
013100080409      // - Tabella PSP = Password SPC gestione PDA
013200090706     d dPSPs         e ds                  inz
013300080409
013400080409      // Campi di comodo per gestione indicatori a video
013500080409     d WindDspF        ds                  inz
013600080409     d   WindDspFa             1     49    inz(*zero)
013700080409     d   WindDspFb            50     99    inz(*zero)
013800080409
013900080409      //---------------------------------------------------------------
014000080409      //?Definizione variabili globali.
014100080409      //---------------------------------------------------------------
014200080409
014300080409      // - Flags booleani
014400080409     d $Fine           s               n   inz(*off)
014500080409     d $InzS01         s               n   inz(*on)
014600080409     d $InzD01         s               n   inz(*on)
014700080410     d $InzW01         s               n   inz(*on)
014800080409     d $Err            s               n   inz(*off)
014900080409
015000080409      // - Indici di schiera
015100080430     d wx              s              3  0 inz
015200080409     d xx              s              3  0 inz
015300080414     d yy              s              3  0 inz
015400080409
015500080409      // - Campi associati al video
015600080411     d $Video          s              2    inz('S1')
015700080409     d S01nrr          s              4  0 inz
015800080409     d W_SflPag        s              3  0 inz
015900080409     d wPag            s              4  0 inz
016000080409     d wRig            s              3  0 inz
016100080411
016200080411      // - Campi per salvataggio momentaneo dei dati
016300080411     d SAVatb          s                   like(TBEatb)   inz
016400080409
016500080409      // - Campi di comodo
016600080409     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
016700090707     d wpos            s              2  0 inz
016800080409
016900080409      //---------------------------------------------------------------
017000080409      //?Definizione procedure usate.
017100080409      //---------------------------------------------------------------
017200140520
017300140520       // -?Reperimento dati utente?
017400140520     d TIBS34ds      e ds
017500140520      /copy gaitrasrc/srcProtoPR,TIBS34R
017600080409
017700080409      //---------------------------------------------------------------
017800080409      //?Definizione key-list.
017900080409      //---------------------------------------------------------------
018000080409
018100080409       // - File TNTBE01L
018200080410     d k05tntbe01    e ds                  extname(TNTBE01L:*key)
018300080411     d                                     prefix(k_)
018400080409     d                                     inz
018500080409
018600080409      //---------------------------------------------------------------
018700080409      //?Riepilogo indicatori.
018800080409      //---------------------------------------------------------------
018900080409      // - Comuni
019000080409      // 02    : Scelta opzione 2=Modifica
019100080409      // 05    : Scelta opzione 5=Visualizzazione
019200080409      // 28    : Emissione messaggio di errore a video
019300080409      // - C01/S01
019400080409      // 30    : Condiziona SFLDSP    (*not)
019500080409      // 31    : Condiziona SFLDSPCTL (*not)
019600080409      // 30+31 : Condiziona SFLCLR
019700080409      // 32    : Condiziona SFLNXTCHG in S01
019800080409      // 33    : Condiziona SFLEND
019900080409      // 40    : Condiziona la protezione di V1CFGS
020000080409      // 50    : Errore: Opzione errata
020100080409      // - D01
020200080409      // 51    : Errore: Codice del SPC     errato o mancante
020300080409      // 52    : Errore: Descrizione        mancante
020400080409      // 53    : Errore: Password attuale   errata o mancante
020500080409      // 54    : Errore: Nuova password (1) errata o mancante
020600080409      // 55    : Errore: Nuova password (2) errata o mancante
020700080409      // - Comuni
020800080409      // 99    : Generico di Errore
020900080409      //---------------------------------------------------------------
021000080409
021100080409      //---------------------------------------------------------------
021200080409      //?M A I N - L I N E
021300080409      //---------------------------------------------------------------
021400080409
021500080409     c     *Entry        plist
021600080409     c                   parm                    KPJBA
021700080409
021800080409      /free
021900080409
022000080409       // Operazioni iniziali
022100080409       exsr sr_RoutInz;
022200080409
022300080409       // Gestione video
022400080409       DOW $Fine = *off;
022500080409         select;
022600080409           when $Video = 'S1';
022700080409             exsr sr_GesS01;
022800080409           when $Video = 'D1';
022900080409             exsr sr_GesD01;
023000080409           when $Video = 'W1';
023100080410             exsr sr_GesW01;
023200080409           other;
023300080409             $Fine = *on;
023400080409         endsl;
023500080409       ENDDO;
023600080409
023700080409       // Operazioni finali
023800080409       exsr sr_RoutEnd;
023900080409
024000080409       //--------------------------------------------------------------
024100080409       //?Operazioni iniziali.
024200080409       //--------------------------------------------------------------
024300080409       BEGSR sr_RoutInz;
024400140520
024500140520         // -?Reperimento dati job?
024600140520         exsr  sr_DatiJob;
024700080409
024800080409         // Impostazione campi "fissi"
024900080409         T01pgm = SDSpgm;
025000080409
025100080409         // Impostazione Filiale Gestione
025200090706         C1Cfgs = 046;
025300090706         C1Dfgs = 'SEDE';
025400080410
025500080410         // Aggancio dati generali della tabella in esame
025600080410         clear k_TBEcod;
025700080410         k_TBEke1 = *zero;
025800080410         %subst(k_TBEke1 : %len(k_TBEke1)-%len(C_tab)+1 : %len(C_tab))
025900080410                = C_tab;
026000080410         clear k_TBEke2;
026100080410         clear k_TBElin;
026200080410         k_TBEsif = KNSIF;
026300080411         chain  %kds(K05tntbe01) TNTBE000;
026400080410         if not %found(TNTBE01L);
026500080410           clear  k_TBEsif;
026600080411           chain  %kds(K05tntbe01) TNTBE000;
026700080410         endif;
026800080410         if %found(TNTBE01L);
026900080410           xTNTBEds = TNTBEds;
027000080410         else;
027100080410           clear xTNTBEds;
027200080410         endif;
027300080409
027400080409       ENDSR;
027500140520
027600140520       //--------------------------------------------------------------
027700140520       //?Reperimento Dati del job (Utente/Operativi).                 ?
027800140520       //--------------------------------------------------------------
027900140520       BEGSR  sr_DatiJob;
028000140520
028100140520         in(E) §AzUte;
028200140520         if NOT %error;
028300140520           in(E) §DatiUte;
028400140520         endif;
028500140520         if %error or RSut = *blanks;
028600140520           clear TIBS34ds;
028700140520           tibs34r ( tibs34ds );
028800140520           in §AzUte;
028900140520           in §DatiUte;
029000140520         endif;
029100140520
029200140520       ENDSR;
029300080409
029400080409       //--------------------------------------------------------------
029500080409       //?Gestione videata S01
029600080409       //--------------------------------------------------------------
029700080409       BEGSR sr_GesS01;
029800080409
029900080409         // Inizializzazione videata
030000080409         if  $InzS01   = *on;
030100080409           exsr sr_InzS01;
030200080409           $InzS01  = *off;
030300080409         endif;
030400080411
030500080411         // Emissione Testata e Piede con tasti funzionali abilitati
030600080411         if  errGenerico = *off;
030700080411           write TB64T01;
030800080411           write TB64P01;
030900080411         endif;
031000080409
031100080409         // Posizionamento cursore
031200080409         if  C1CsrRrn  > *zero;
031300080410           C1RcdNbr = C1CsrRrn;
031400080409         else;
031500080409           C1RcdNbr = 1;
031600080411           write TB64S00;              // (no rec.)
031700080409         endif;
031800080409
031900080409         // Emissione videata
032000080409         exfmt TB64C01;
032100080409       //C1RcdNbr = min_nrr;
032200080409         reset ErrMessage;
032300080409         reset ErrGenerico;
032400080409         clear V1Dmsg;
032500080409
032600080409         SELECT;
032700080409
032800080409         // - F3=Fine
032900080409           WHEN  dsp_aid = c_F03;
033000080410             exsr sr_F03S01;
033100080409
033200080409         ENDSL;
033300080409
033400080409         SELECT;
033500080409
033600150112         // - F5=Refresh
033700080409           WHEN  dsp_aid = c_F05;
033800080409             reset $InzS01;
033900080409
034000080409         // - F10=Inserimento
034100080409           WHEN  dsp_aid = c_F10;
034200080411             exsr sr_F10S01;
034300080409
034400080409         // - Roll-Up
034500080409           WHEN  dsp_aid = c_RollUp;
034600080409           // - Verifica se raggiunto il limite di records registrabili
034700080409           //   nel subfile (9999)
034800080409             if  S01nrr = *hival;
034900080409               ErrMessage  = *on;
035000080409               ErrGenerico = *on;
035100080409               V1Dmsg = $Msg(02);
035200080409             else;
035300080409               exsr sr_RollUpS01;
035400080409             endif;
035500080409
035600080409         // Invio
035700080411           WHEN  S01nrr = *zero;
035800080409           OTHER;
035900080409             exsr sr_OpzS01;
036000080409             if  ErrGenerico = *on;
036100080409               leavesr;
036200080409             endif;
036300080409
036400080409         ENDSL;
036500080409
036600080409       ENDSR;
036700080409
036800080409       //--------------------------------------------------------------
036900080409       //?Inizializzazione videata S01
037000080409       //--------------------------------------------------------------
037100080409       BEGSR sr_InzS01;
037200080409
037300080409       // Pulizia subfile
037400080409         SflDsp_N    = *on;
037500080409         SflDspCtl_N = *on;
037600080409         write  TB64C01;
037700080409         SflDspCtl_N = *off;
037800080409         SflEnd      = *off;
037900080409
038000080409         clear W_SflPag;
038100080409         clear C1RcdNbr;
038200080409         clear C1CsrRrn;
038300080409         clear S01nrr;
038400080409         clear V1Dmsg;
038500080409         ErrMessage  = *off;
038600080409         ErrGenerico = *off;
038700080409         WindDspF    = IndDspF;
038800080409         reset WindDspFb;
038900080409         IndDspF     = WindDspF;
039000140519         VisualizzaPwd = *off;
039100080409
039200080409       // Caricamento dei dati da presentare nel sfl
039300081118       // carico le prime 11 password
039400080411         k_TBEcod = c_Tab;
039500080411         k_TBEke1 = %editc(C1Cfgs : 'X');
039600090706         k_TBEke2 = *blank;
039700080414         k_TBEsif = *blank;
039800080411         k_TBElin = *blank;
039900080414         chain  %kds(k05tntbe01)  TNTBE000;
040000080414
040100090706         clear dPSPs;
040200080414         if  %found(TNTBE01L);
040300090706           dPSPs = TBEuni;
040400080414         endif;
040500080409
040600080414         exsr sr_RollUpS01;
040700081118
040800090706       // se non ho caricato niente azzero le pagine
040900081118         if s01nrr = 0;
041000081118          clear w_sflpag;
041100081118         endif;
041200080409
041300080409       // Impaginazione subfile
041400080409       // -> forza l'impaginazione sul 1° rec. del subfile
041500080409         if S01nrr > *zero;
041600080409           C1RcdNbr  = 1;
041700080409           C1CsrRrn  = 1;
041800080409         else;
041900080409           clear C1RcdNbr;
042000080409         endif;
042100080409
042200080409       ENDSR;
042300080409
042400080409       //--------------------------------------------------------------
042500080409       //?Caricamento singola pagina S01
042600080409       //--------------------------------------------------------------
042700080414       BEGSR sr_RollUpS01;
042800080409
042900080409         S01nrr    = (W_SflPag * C_SflPag);
043000080409         W_SflPag += 1;
043100080409         SflNxtChg = *off;
043200080414
043300080414         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
043400080414         IF  %found(TNTBE01L);
043500090706             if  §PSPcods > *zero;
043600080418               exsr  sr_RieS01;
043700080418               S01nrr += 1;
043800080418               write TB64S01;
043900080418             endif;
044000080414
044100080414         ENDIF;
044200080409
044300080409         // Visualizzazione del SFL (se ci sono dati)
044400080409         SflDsp_N  = (S01nrr <= *zeros);
044500080409
044600080414         // Attivazione (comunque) del SFLEND
044700080414         SflEnd    = *on;
044800080409
044900080409         // Posizionamento cursore al 1° rcd della pagina
045000080409         if  S01nrr > *zero;
045100080409           wPag     = S01nrr / C_SflPag;
045200080409           wRig     = S01nrr - (C_SflPag * wPag);
045300080409           C1RcdNbr = wPag * C_SflPag;
045400080409           if  wRig > *zeros;
045500080409             C1RcdNbr += 1;
045600080409           else;
045700080409             C1RcdNbr = C1RcdNbr - C_SflPag + 1;
045800080409           endif;
045900080409         else;
046000080409           clear  C1RcdNbr;
046100080409         endif;
046200080409
046300080409         C1CsrRrn = C1RcdNbr;
046400080409
046500080409       ENDSR;
046600080409
046700080409       //--------------------------------------------------------------
046800080409       //?Caricamento singolo record nel SubFile S01
046900080409       //--------------------------------------------------------------
047000080409       BEGSR sr_RieS01;
047100080409
047200080409         clear  TB64S01;
047300080409
047400080409         // Campi di solo output
047500090706         S1pspcod = %int( §PSPcods );
047600090706         S1pspdes = §PSPdess;
047700140519         if  %subst( KNMUS : 1 :3 ) = 'EDP';
047800140519           S1pspPwd = §PSPpwds;
047900140519         endif;
048000140519
048100140519         VisualizzaPwd = ( %subst( KNMUS : 1 :3 ) = 'EDP' );
048200080409
048300080409       ENDSR;
048400080409
048500080409       //--------------------------------------------------------------
048600080409       //?Gestione tasto funzionale F3 da videata S01
048700080409       //?F3=Fine
048800080409       //--------------------------------------------------------------
048900080409       BEGSR sr_F03S01;
049000080409
049100080409         // Chiusura del programma
049200080409         $Fine = *on;
049300080409
049400080409       ENDSR;
049500090706
049600080409       //--------------------------------------------------------------
049700080409       //?Gestione tasto funzionale F10 da videata S01
049800080409       //?F10=Immissione
049900080409       //--------------------------------------------------------------
050000080411       BEGSR sr_F10S01;
050100080409
050200080409         $Video = 'D1';
050300080409         reset $InzD01;
050400080409
050500080409       ENDSR;
050600080409
050700080409       //--------------------------------------------------------------
050800080409       //?Gestione opzioni subfile
050900080409       //--------------------------------------------------------------
051000080409       BEGSR sr_OpzS01;
051100080409
051200080409         readc TB64S01;
051300080409
051400090706         DOW  NOT  %eof(TNTB64D1);
051500080409
051600080409           SflNxtChg = *off;
051700080409           WindDspF  = IndDspF;
051800080409           reset WindDspFb;
051900080409           IndDspF   = WindDspF;
052000080409           C1RcdNbr  = S01nrr;
052100080409
052200080409           SELECT;
052300080409
052400080409             // - Nessuna opzione
052500080410             WHEN  S1opzione = *zero;
052600080409
052700080409             // - Gestione singola opzione
052800080410             WHEN  S1opzione = 2   or
052900080410                   S1opzione = 5;
053000080409               $Video = 'D1';
053100080409               reset  $InzD01;
053200080410               dow  $Video = 'D1';
053300080410                 exsr sr_GesD01;
053400080410               enddo;
053500080409
053600080409             // - ? = Opzione NON valida
053700080409             OTHER;
053800080409               ErrMessage  = *on;
053900080409               ErrGenerico = *on;
054000080409               PosCurOpz   = *on;
054100080409               V1Dmsg = $Msg(03);
054200080409
054300080409           ENDSL;
054400080409
054500080409           // Aggiornamento sfl
054600080409           select;
054700080409             when  ErrMessage  = *on;
054800080409               SflNxtChg = *on;
054900080409               C1CsrRrn  = C1RcdNbr;
055000080409             when  ErrGenerico = *on;
055100080409               C1CsrRrn  = C1RcdNbr;
055200080410               clear  S1opzione;
055300080409             other;
055400080409               C1CsrRrn  = C1RcdNbr;
055500080410               clear  S1opzione;
055600080409           endsl;
055700080409
055800080409           update TB64S01;
055900080409
056000080409           if  errMessage = *on  or  errGenerico = *on;
056100080409             leave;
056200080409           endif;
056300080409
056400080410           readc TB64S01;
056500080409
056600080409         ENDDO;
056700080409
056800080409       ENDSR;
056900080409
057000080409       //--------------------------------------------------------------
057100080409       //?Gestione videata D01
057200080409       //--------------------------------------------------------------
057300080409       BEGSR sr_GesD01;
057400080409
057500080409         // Inizializzazione videata
057600080409         if $InzD01 = *on;
057700080409           exsr sr_InzD01;
057800080409           $InzD01 = *off;
057900080409         endif;
058000080409
058100080409         // Emissione videata
058200080411         write TB64T01;
058300080414         if  S1opzione = 5;
058400080411           write TB64D01;
058500080410           exfmt Protect;
058600080410         else;
058700080411           exfmt TB64D01;
058800080410         endif;
058900080414         ErrMessage  = *off;
059000080414         ErrGenerico = *off;
059100080409         clear V1Dmsg;
059200080409
059300080410         SELECT;
059400080409
059500080409           // F3=Fine
059600080410           WHEN dsp_aid = c_F03;
059700080411             exsr sr_F03D01;
059800080409
059900080409           // F12=Ritorno
060000080410           WHEN dsp_aid = c_F12;
060100080409             exsr sr_F12D01;
060200080409
060300080411           // Nessun controllo se visualizzazione
060400080414           WHEN S1opzione = 5;
060500080411
060600080414           // Enter o F6
060700080410           OTHER;
060800080410             // - Controllo dati
060900080414             exsr sr_CtrD01;
061000080414             if errGenerico;
061100080414               leavesr;
061200080414             endif;
061300080414
061400080411             // - Conferma dati:
061500080411             //   F6=Conferma
061600080414             If  dsp_aid = c_F06;
061700080411               $Video  = 'W1';
061800080411               reset  $InzW01;
061900080411               dow  $Video = 'W1';
062000080411                 exsr sr_GesW01;
062100080411               enddo;
062200080410             EndIf;
062300080409
062400080410         ENDSL;
062500080409
062600080409       ENDSR;
062700080409
062800080409       //--------------------------------------------------------------
062900080409       //?Inizializzazione videata D01
063000080409       //--------------------------------------------------------------
063100080409       BEGSR sr_InzD01;
063200080409
063300080409         clear TB64D01;
063400080410
063500080410         // REimpostazione indicatori per abilitazione tasti funzionali
063600080410         exsr Set_Ind_D01;
063700080409
063800080409         // Premuto F10=Inserimento
063900080409         if  dsp_aid = c_F10;
064000080409           D1DesOpz  = '  INSERIMENTO  ';
064100080414           D1cfgs    = C1Cfgs;
064200080414           D1dfgs    = C1Dfgs;
064300080414           NoProtectCod = *on;
064400080409           leavesr;
064500080409         endif;
064600080409
064700080409         // Intestazione videata
064800080409         select;
064900080411           when  S1opzione = 2;
065000080409             D1DesOpz  = '   MODIFICA    ';
065100080414             NoProtectCod  = *off;
065200080411           when  S1opzione = 5;
065300080410             D1DesOpz  = 'VISUALIZZAZIONE';
065400080414           other;
065500080414             clear D1DesOpz;
065600080409         endsl;
065700080409
065800080409         D1cfgs   = C1Cfgs;
065900080409         D1dfgs   = C1Dfgs;
066000090706         D1pspcod = %int( §PSPcods );
066100090706         D1pspdes = §PSPdess;
066200080409
066300080409       ENDSR;
066400080410
066500080410       //--------------------------------------------------------------
066600080410       //?Settaggio indicatori nella videata D01 per abilitazione
066700080410       //?  tasti funzionali
066800080410       //--------------------------------------------------------------
066900080410       BEGSR Set_Ind_D01;
067000080410
067100080411         F6Attivo     = (S1opzione <> 5);
067200080410
067300080410       ENDSR;
067400080411
067500080411       //--------------------------------------------------------------
067600080411       //?Gestione tasto funzionale F3 da videata D01
067700080411       //?F3=Fine
067800080411       //--------------------------------------------------------------
067900080411       BEGSR sr_F03D01;
068000080411
068100090707         // Chiusura del programma
068200090707         $Video = 'S1';
068300090707         $Fine = *on;
068400080411
068500080411       ENDSR;
068600080409
068700080409       //--------------------------------------------------------------
068800080411       //?Gestione tasto funzionale F12 da videata D01
068900080411       //?F12=Ritorno
069000080409       //--------------------------------------------------------------
069100080409       BEGSR sr_F12D01;
069200080409
069300080411         // Ritorno alla videata S01
069400080409         $Video = 'S1';
069500080409
069600080409       ENDSR;
069700080409
069800080409       //--------------------------------------------------------------
069900080409       //?Controllo videata D01
070000080409       //--------------------------------------------------------------
070100080409       BEGSR sr_CtrD01;
070200080409
070300080409         WindDspF = IndDspF;
070400080409         reset WindDspFb;
070500080409         IndDspF  = WindDspF;
070600080410
070700080410         // REimpostazione indicatori per abilitazione tasti funzionali
070800080410         exsr Set_Ind_D01;
070900080409
071000080409         SELECT;
071100080409
071200080410         // Controllo codice SPC
071300080414           WHEN  D1pspcod   =  *zero;
071400080414             ErrMessage  = *on;
071500080414             ErrGenerico = *on;
071600080414             PosCurCod   = *on;
071700080410             V1Dmsg      = $Msg(04);
071800080409             leavesr;
071900090706           WHEN  D1pspcod  <>  99;
072000080414             ErrMessage  = *on;
072100080414             ErrGenerico = *on;
072200080414             PosCurCod   = *on;
072300080410             V1Dmsg      = $Msg(05);
072400080410             leavesr;
072500080411
072600080411         ENDSL;
072700090707
072800090707         // Controllo inesistenza codice chiave se F10=Inserimento
072900090707         IF  NoProtectCod = *on;
073000090707           k_TBEke1  = %editc(D1Cfgs   : 'X');
073100090707           chain(n)  %kds(K05tntbe01) TNTBE000;
073200090707           if  %found(TNTBE01L);
073300090707             ErrMessage  = *on;
073400090707             ErrGenerico = *on;
073500090707             PosCurCod   = *on;
073600090707             V1Dmsg      = $Msg(06);
073700090707             leavesr;
073800090707           endif;
073900090707         ENDIF;
074000080409
074100080411         SELECT;
074200080411
074300080410         // Controllo descrizione
074400080410           WHEN  D1PSPdes   =  *blank;
074500080409             errMessage  = *on;
074600080409             errGenerico = *on;
074700080410             PosCurDES   = *on;
074800080411             V1Dmsg      = $Msg(07);
074900080409             leavesr;
075000080409
075100080414         // Controllo Password SPC corrente
075200080410           WHEN  D1PSPpwdO  =  *blank   and
075300090706                 §PSPpwds   >  *zero;
075400080409             errMessage  = *on;
075500080409             errGenerico = *on;
075600080410             PosCurPWDo  = *on;
075700080411             V1Dmsg      = $Msg(08);
075800080411             clear  D1PSPpwdO;
075900080409             leavesr;
076000090706           WHEN  D1PSPpwdO  <> §PSPpwds  and
076100090706                 §PSPpwds   >  *zero;
076200080410             errMessage  = *on;
076300080410             errGenerico = *on;
076400080410             PosCurPWDo  = *on;
076500080411             V1Dmsg      = $Msg(09);
076600080411             clear  D1PSPpwdO;
076700080410             leavesr;
076800080411
076900080410         // Controllo Nuova password SPC
077000080410           WHEN  D1PSPpwdN1 =  *blank;
077100080410             errMessage  = *on;
077200080410             errGenerico = *on;
077300080410             PosCurPWDn1 = *on;
077400080411             V1Dmsg      = $Msg(10);
077500080411             clear  D1PSPpwdN1;
077600080410             leavesr;
077700080414           WHEN  D1PSPpwdN1 =  D1PSPpwdO;
077800080410             errMessage  = *on;
077900080410             errGenerico = *on;
078000080410             PosCurPWDn1 = *on;
078100080411             V1Dmsg      = $Msg(11);
078200080411             clear  D1PSPpwdN1;
078300080410             leavesr;
078400090707
078500090707         ENDSL;
078600080411
078700090707           // - La password deve contenere solo cifre, ammessi blank alla fine
078800090707           wpos =  %checkr( ' ' : D1PSPpwdN1 );
078900080411
079000090707         SELECT;
079100090707           WHEN  %checkr( C_Digit : D1PSPpwdN1 : wpos ) > *zero;
079200080411             errMessage  = *on;
079300080411             errGenerico = *on;
079400080411             PosCurPWDn1 = *on;
079500080411             V1Dmsg      = $Msg(12);
079600080411             clear  D1PSPpwdN1;
079700080411             leavesr;
079800080410
079900080410         // Controllo Nuova password SPC (per conferma)
080000080410           WHEN  D1PSPpwdN2 =  *blank;
080100080410             errMessage  = *on;
080200080410             errGenerico = *on;
080300080410             PosCurPWDn2 = *on;
080400080512             V1Dmsg      = $Msg(14);
080500080410             leavesr;
080600080410           WHEN  D1PSPpwdN2 <> D1PSPpwdN1;
080700080410             errMessage  = *on;
080800080410             errGenerico = *on;
080900080410             PosCurPWDn2 = *on;
081000080512             V1Dmsg      = $Msg(15);
081100080410             leavesr;
081200080409
081300080410         ENDSL;
081400080409
081500080409       ENDSR;
081600080409
081700080409       //--------------------------------------------------------------
081800080410       //?Gestione trasmissione aggiornamento
081900080409       //--------------------------------------------------------------
082000080410       BEGSR sr_GesW01;
082100080409
082200080410         // Inizializzazione videata
082300080410         if $InzW01 = *on;
082400080410           exsr sr_InzW01;
082500080410           $InzW01 = *off;
082600080410         endif;
082700080410
082800080410         // Emissione window
082900080411         if  TBXftt = 'S';
083000080411           exfmt TB64W01;
083100080411           ErrMessage  = *off;
083200080411           ErrGenerico = *off;
083300080411           clear V1Dmsg;
083400110421           Select;
083500110421             // -?F12=Ritorno (gestito solo se emesso W01)?
083600110421             When  dsp_aid = c_F12;
083700110421               exsr  F12W01;
083800110421               leavesr;
083900110421             // -?Invio (gestito solo se emesso W01)?
084000110421             When  dsp_aid <> c_F06;
084100110421               leavesr;
084200110421           EndSl;
084300080411         endif;
084400080410
084500110421         // -?Aggiornamento TNTBE00F?
084600110421         exsr Upd_TNTBE;
084700110421         // Ritorno alla videata S01
084800110421         if  NOT  errGenerico;
084900110421           $Video  = 'S1';
085000110421           $InzS01 = *on;
085100110421         endif;
085200080410
085300080410       ENDSR;
085400080410
085500080410       //--------------------------------------------------------------
085600080410       //?Inizializzazione videata W01
085700080410       //--------------------------------------------------------------
085800080410       BEGSR sr_InzW01;
085900080410
086000080410         clear TB64W01;
086100080410
086200080414         if  NOT %found(TNTBE01L);
086300080414
086400080414           // Se immissione
086500080414           W1ftt  = TBXftt;
086600080410
086700080414         else;
086800080414
086900080414           // Se NON immissione:
087000080414           // visualizza i dati relativi all'ultima trasmissione
087100080414           W1ftt  = TBEftt;
087200080410           W1flt  = TBEflt;
087300080410           W1ftr  = TBEftr;
087400080410           if TBEdtr <> *zero;
087500080410             wDate_EUR = %date(TBEdtr : *iso);
087600080410             W1dtr     = %dec(wDate_EUR);
087700080410           endif;
087800080414
087900080410         endif;
088000080410
088100080410       ENDSR;
088200080410
088300080410       //--------------------------------------------------------------
088400080410       //?Gestione tasto funzionale F12 da videata W01
088500080410       //?F12=Ritorno
088600080410       //--------------------------------------------------------------
088700080410       BEGSR F12W01;
088800080410
088900110421         // Ritorno alla videata precedente
089000080410           $Video = 'D1';
089100080410
089200080410       ENDSR;
089300080410
089400080410       //--------------------------------------------------------------
089500080410       //?Aggiornamento singolo record TNTBE00F (tab. PSP)
089600080410       //--------------------------------------------------------------
089700080410       BEGSR Upd_TNTBE;
089800080410
089900080414         // Aggancio record di TNTBE00F
090000081118           k_TBEcod  = c_Tab;
090100081118           k_TBEke1  = %editc(D1Cfgs   : 'X');
090200090706           clear k_TBEke2;
090300081118           clear k_TBElin;
090400081118           clear k_TBEsif;
090500090706           clear dPSPs;
090600081118
090700081118           chain  %kds(K05tntbe01) TNTBE000;
090800080410
090900080410         // Impostazione dati
091000090706         §PSPcods  = %editc(D1pspcod : 'X');
091100090706         §PSPdess  = D1pspdes;
091200090706         §PSPpwds  = %subst( D1psppwdN1 : 1 : %len(§PSPpwds) );
091300090706         §PSPchars = c_Char;
091400080410
091500080410         if  NOT %found(TNTBE01L);
091600080411           clear TNTBE000;
091700080411           TBEcod  = k_TBEcod;
091800080411           TBEke1  = k_TBEke1;
091900080411           TBEke2  = k_TBEke2;
092000080411           TBElin  = k_TBElin;
092100080411           TBEsif  = k_TBEsif;
092200080410         endif;
092300080415         TBEapl = TBXapl;
092400090706         TBEuni = dPSPs;
092500080414         clear TBEatb;
092600080411         TBEftt = W1ftt;
092700080411         TBEFlt = W1flt;
092800080410         clear TBEftr;
092900080410
093000080410         // Aggiornamento record
093100080410         if  %found(TNTBE01L);
093200080410         //_______________
093300080410           UPDATE TNTBE000;
093400080410         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
093500080410         else;
093600080410         //_______________
093700080410           WRITE  TNTBE000;
093800080410         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
093900080410         endif;
094000080409
094100080409       ENDSR;
094200080409
094300080409       //--------------------------------------------------------------
094400080409       //?Operazioni finali.
094500080409       //--------------------------------------------------------------
094600080409       BEGSR sr_RoutEnd;
094700080409
094800080409         *inLR = *on;
094900080409         return;
095000080409
095100080409       ENDSR;
095200080409
095300080409      /end-free
095400080409
095500080409       //--------------------------------------------------------------
095600080409       //?Schiere a tempo di compilazione.
095700080409       //--------------------------------------------------------------
095800080409
095900080409** - $MSG -------------------------------------------------------------------*
096000090706                                                                               01
096100080409Raggiunta l'ampiezza massima del subfile                                       02
096200080409Opzione errata                                                                 03
096300080410Codice SPC obbligatorio                                                        04
096400090706Codice SPC errato                                                              05
096500080411Codice SPC già inserito                                                        06
096600080411Descrizione obbligatoria                                                       07
096700080411Inserire la password attuale                                                   08
096800080411Password attuale errata                                                        09
096900080411Inserire la nuova password                                                     10
097000080411La nuova password deve essere diversa da quella attuale                        11
097100090706La password DEVE essere composta da cifre                                      12
097200090706                                                                               13
097300080512Inserire la nuova password per la verifica                                     14
097400080512La password di verifica non coincide con la nuova appena inserita              15
097500090706                                                                               16
097600090706                                                                               17
097700080703Password errata                                                                18
