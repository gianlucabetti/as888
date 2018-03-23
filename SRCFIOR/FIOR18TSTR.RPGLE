000100110428       //==============================================================
000200110428       //?TEST - STAMPA ORM SU STAMPANTE LASER IN MODELLO UNICO A4     ?
000300110428       //==============================================================
000400110428
000500110428       //--------------------------------------------------------------
000600110428       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700110428       //--------------------------------------------------------------
000800110428
000900110428     /*END
001000110428
001100110428       //--------------------------------------------------------------
001200110428       //?Specifiche di controllo.                                     ?
001300110428       //--------------------------------------------------------------
001400080424
001500080424     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600110428     h dftactgrp(*no)
001700080424
001800080424       //--------------------------------------------------------------
001900110428       //?Dichiarazione file.                                          ?
002000080424       //--------------------------------------------------------------
002100080709
002200110428       // -?Archivio "O.R.M."?
002300081209     fFNORM01L  if   e           k disk
002400080428
002500110428       // -?Video?
002600081209     fFIOR18TSTDcf   e             workstn
002700080428     f                                     indds(IndDspF)
002800080428     f                                     infds(InfDspF)
002900080424
003000080424       //--------------------------------------------------------------
003100110428       //?Definizione costanti.                                        ?
003200080424       //--------------------------------------------------------------
003300080424
003400110428       // -?Tasti funzionali a video?
003500080428     d c_F01           c                   const(x'31')
003600080428     d c_F02           c                   const(x'32')
003700080428     d c_F03           c                   const(x'33')
003800080429     d c_F04           c                   const(x'34')
003900080428     d c_F05           c                   const(x'35')
004000080428     d c_F06           c                   const(x'36')
004100080428     d c_F07           c                   const(x'37')
004200080428     d c_F08           c                   const(x'38')
004300080428     d c_F09           c                   const(x'39')
004400080428     d c_F10           c                   const(x'3A')
004500080429     d c_F11           c                   const(x'3B')
004600080428     d c_F12           c                   const(x'3C')
004700080428     d c_F13           c                   const(x'B1')
004800080428     d c_F14           c                   const(x'B2')
004900080428     d c_F15           c                   const(x'B3')
005000080428     d c_F16           c                   const(x'B4')
005100080428     d c_F17           c                   const(x'B5')
005200080428     d c_F18           c                   const(x'B6')
005300080428     d c_F19           c                   const(x'B7')
005400080428     d c_F20           c                   const(x'B8')
005500080428     d c_F21           c                   const(x'B9')
005600080428     d c_F22           c                   const(x'BA')
005700080428     d c_F23           c                   const(x'BB')
005800080428     d c_F24           c                   const(x'BC')
005900080428     d c_Enter         c                   const(x'F1')
006000080428     d c_RollDown      c                   const(x'F4')
006100080428     d c_RollUp        c                   const(x'F5')
006200080424
006300080424       //--------------------------------------------------------------
006400110428       //?Definizione schiere.                                         ?
006500080424       //--------------------------------------------------------------
006600080424
006700110428       // -?Messaggi di errore?
006800110428     d $Msg            s             78    dim( 4) ctdata perrcd(1)
006900080424
007000080424       //--------------------------------------------------------------
007100110428       //?Definizione aree dati.                                       ?
007200080424       //--------------------------------------------------------------
007300080424
007400110428       // -?Dati utente?
007500080424     d §AzUte        e ds                  extname(AZUTE00F)
007600080424     d                                     dtaara
007700080424     d §DatiUte      e ds                  extname(dDatiUte)
007800080424     d                                     dtaara
007900080424
008000080424       //--------------------------------------------------------------
008100110428       //?Definizione strutture dati.                                  ?
008200080424       //--------------------------------------------------------------
008300080424
008400110428       // -?Status ds?
008500080424     d Status         sds
008600080424     d  SDSpgm           *proc
008700080424     d  SDSprm           *parms
008800080424     d  SDSusr               254    263
008900080428
009000110428       // -?InfDS?
009100080428     d InfDspF         ds
009200080428     d  dsp_aid              369    369a
009300080428
009400110428       // -?Indicatori su DspF?
009500080428     d IndDspF         ds
009600110428         // -?Emissione messaggio di errore?
009700081209     d   ErrMessage                   1n   overlay(IndDspF : 28)
009800110428         // -?Posizionamento cursore & Segnalazione errore?
009900081209     d   PosCurORM                    1n   overlay(IndDspF : 50)
010000110428     d   PosCurPGM                     n   overlay(IndDspF : 51)
010100110428         // -?Riemissione videata?
010200081209     d   ErrGenerico                  1n   overlay(IndDspF : 99)
010300080429
010400110428       // -?Parametri per *pgm controllo/inversione data?
010500080429     d WLBdat          ds                  inz
010600080429     d  G08dat                        8  0 inz
010700080429     d  G08inv                        8  0 inz
010800080429     d  G08err                        1    inz('3')
010900080429     d  G08tgi                        5  0 inz
011000080428
011100110428       // -?Parametri ricevuti?
011200080424     d KPJBA         e ds
011300081209
011400110428       // -?Parametri per interrogazione O.R.M.?
011500151014     d FIOR010ds     e ds                  inz
011600080505
011700110428       // -?Dati ORM da stampare?
011800081209     d FIOR16ds      e ds                  inz
011900080424
012000080424       //--------------------------------------------------------------
012100110428       //?Definizione variabili globali.                               ?
012200080424       //--------------------------------------------------------------
012300080428
012400110428       // -?Flags booleani?
012500080428     d $Fine           s               n   inz(*off)
012600080429     d $InzD01         s               n   inz(*on)
012700080429
012800110428       // -?Campi di comodo?
012900080506     d SAVpgm          s                   inz  like(V1Cpgm)
013000081209     d SAVoqo          s                   inz  like(R16oqo)
013100081209     d SAVmdo          s                   inz  like(R16mdo)
013200081209     d SAVris          s                   inz  like(R16ris)
013300080424
013400080424       //--------------------------------------------------------------
013500110428       //?Definizione procedure usate.                                 ?
013600080424       //--------------------------------------------------------------
013700080424
013800110428       // -?Reperimento dati utente?
013900110428     d TIBS34ds      e ds
014000110428      /copy gaitrasrc/srcProtoPR,TIBS34R
014100080429
014200110428       // -?Interrogazione O.R.M.?
014300151014     d fior010r        pr                  extpgm('FIOR010R')
014400081209     d   kpjba                             likeds(KPJBA)
014500151014     d   fior010ds                         likeds(FIOR010ds)
014600080606
014700110428       // -?Stampa O.R.M.?
014800081209     d fior18r         pr                  extpgm('FIOR18R')
014900081209     d   kpjba                             likeds(KPJBA)
015000080424
015100080424       //--------------------------------------------------------------
015200110428       //?Definizione key-list.                                        ?
015300080424       //--------------------------------------------------------------
015400080424
015500110428       // -?File FNORM01L?
015600081209     d k04fnorm01    e ds                  extname(FNORM01L:*key)
015700080428     d                                     prefix(k_)
015800080428     d                                     inz
015900080424
016000080424       //--------------------------------------------------------------
016100110428       //?Specifiche di calcolo.                                       ?
016200080424       //--------------------------------------------------------------
016300080424
016400080424     c     *Entry        plist
016500080424     c                   parm                    KPJBA
016600080424
016700080424      /free
016800080424
016900110428       // -?Start?
017000080424       *inLR = *on;
017100080424
017200110428       // -?Elab?
017300080428       dow  $Fine = *off;
017400110428         exsr  sr_GesD01;
017500080428       enddo;
017600080424
017700110428       // -?End?
017800110428       exsr  sr_EndPgm;
017900080424       return;
018000080424
018100080424       //--------------------------------------------------------------
018200110428       //?Operazioni iniziali                                          ?
018300080424       //--------------------------------------------------------------
018400110428       BEGSR  *InzSR;
018500080424
018600110428         // -?Reperimento dati job?
018700110428         exsr  sr_DatiJob;
018800080506
018900110428         // -?Impostazione nome programma a video?
019000080506         VTDpgm = SDSpgm;
019100080424
019200080424       ENDSR;
019300080424
019400080424       //--------------------------------------------------------------
019500110428       //?Reperimento Dati del job (Utente/Operativi).                 ?
019600080424       //--------------------------------------------------------------
019700110428       BEGSR  sr_DatiJob;
019800080424
019900080424         in(E) §AzUte;
020000080424         if NOT %error;
020100080424           in(E) §DatiUte;
020200080424         endif;
020300080424         if %error or RSut = *blanks;
020400080424           clear TIBS34ds;
020500080424           tibs34r(tibs34ds);
020600080424           in §AzUte;
020700080424           in §DatiUte;
020800080424         endif;
020900080424
021000080424       ENDSR;
021100080428
021200080428       //--------------------------------------------------------------
021300110428       //?Gestione videata D01                                         ?
021400080428       //--------------------------------------------------------------
021500110428       BEGSR  sr_GesD01;
021600080428
021700110428         // -?Inizializzazione videata?
021800080428         if  $InzD01 = *on;
021900110428           SAVpgm = V1Cpgm;
022000110428           SAVoqo = V1Coqo;
022100110428           SAVmdo = V1Cmdo;
022200110428           SAVris = V1Cris;
022300081209           clear OR18D01;
022400081209           if  SAVpgm = *blank;
022500081209             V1Cpgm   = 'FIOR18R   ';
022600081209           else;
022700081209             V1Cpgm   = SAVpgm;
022800081209           endif;
022900081209           if  SAVoqo = *blank;
023000081209             V1Coqo   = 'QPRINTS   ';
023100080709           else;
023200081209             V1Coqo   = SAVoqo;
023300080709           endif;
023400081209           if  SAVmdo = *blank;
023500081209             V1Cmdo   = '*ORMA4    ';
023600080506           else;
023700081209             V1Cmdo   = SAVmdo;
023800080506           endif;
023900081209           if  SAVris = *blank;
024000081209             V1Cris   = 'R';
024100081209           else;
024200081209             V1Cris   = SAVris;
024300081209           endif;
024400110428           $InzD01 = *off;
024500080428         endif;
024600110428
024700110428         // -?Verifica correttezza del sistema informativo?
024800110428         if  %subst(knsif : 1 : 6) <> 'FILTRA';
024900110428           ErrGenerico = *on;
025000110428           ErrMessage  = *on;
025100110428           V1Dmsg = $Msg(01);
025200110428         endif;
025300110428
025400110428         // -?Emissione videata?
025500110428         write OR18T01;
025600110428         write OR18Z01;
025700080514
025800081209         exfmt OR18D01;
025900080514
026000080428         reset ErrGenerico;
026100110428         reset ErrMessage;
026200080428         clear V1Dmsg;
026300080514
026400080514         clear V1Dtxt;
026500080428
026600080428         SELECT;
026700110428
026800110428           // -?Sistema Informativo errato?
026900110428           WHEN  %subst(knsif : 1 : 6) <> 'FILTRA';
027000110428             $Fine = *on;
027100080428
027200110428           // -?F3=Fine?
027300080428           WHEN  dsp_aid = c_F03;
027400080428             $Fine = *on;
027500080428
027600110428           // -?F4=Interrogazione O.R.M.?
027700080428           WHEN  dsp_aid = c_F04;
027800151014             exsr Call_FIOR010R;
027900080924
028000110428           // -?F8=Chiusura spool?
028100080924           WHEN  dsp_aid = c_F08;
028200080924             exsr sr_EndPgm;
028300080428
028400110428           // -?Invio o F6=Conferma?
028500080428           OTHER;
028600110428             exsr  sr_CtrD01;
028700080429             select;
028800110428               // -?rilevato errore?
028900080429               when  ErrGenerico = *on;
029000110428               // -?non premuto F6?
029100080429               when  dsp_aid <> c_F06;
029200110428               // -?selezionata un singolo O.R.M.?
029300081209               when  V1Cpoe  <> *zero   and
029400081209                     V1Cnor  <> *zero;
029500110428                 exsr  Call_FIOR18R;
029600110428                 if  Not ErrGenerico;
029700110428                   reset  $InzD01;
029800110428                 endif;
029900080429             endsl;
030000080428
030100080428         ENDSL;
030200080428
030300080428       ENDSR;
030400080424
030500080424       //--------------------------------------------------------------
030600110428       //?Richiamo pgm. Interrogazione O.R.M.                          ?
030700080424       //--------------------------------------------------------------
030800151014       BEGSR  Call_FIOR010R;
030900080424
031000151014         clear  FIOR010ds;
031100151014         IOR010ric = '1';
031200081209         if  DUTlpo <> 'S';
031300151014           IOR010por = DUTpou;
031400081209         endif;
031500080424
031600151014         fior010r (kpjba : FIOR010ds);
031700080424
031800151014         if  OOR010f03 = *on  or
031900151014             OOR010f12 = *on  or
032000151014             OOR010err = *on;
032100080424           leavesr;
032200080424         endif;
032300080424
032400151014         V1Cpoe = OOR010poe;
032500151014         V1Cnsr = OOR010nsr;
032600151014         V1Cnor = OOR010nor;
032700151014         V1Cnrv = OOR010nrv;
032800080424
032900080424       ENDSR;
033000080428
033100080428       //--------------------------------------------------------------
033200110428       //?Richiamo *pgm per stampa singolo O.R.M.                      ?
033300080428       //--------------------------------------------------------------
033400110428       BEGSR  Call_FIOR18R;
033500081209
033600081209         clear  FIOR16ds;
033700081209         R16tla = *blank;
033800081209         R16poe = V1Cpoe;
033900081209         R16nsr = V1Cnsr;
034000081209         R16nor = V1Cnor;
034100081209         R16nrv = V1Cnrv;
034200081209         R16por = R16poe;
034300081209         R16dst = V1Cdst;
034400140424
034500140424         R16ris = V1Cris;
034600081209
034700081209         kpjbu = FIOR16ds;
034800080429
034900080505         select;
035000080606
035100081209           when V1Cpgm = 'FIOR18R   ';
035200081209             fior18r (kpjba);
035300081209
035400081209           other;
035500110428             ErrGenerico = *on;
035600110428             ErrMessage  = *on;
035700110428             PosCurPGM   = *on;
035800110428             V1Dmsg = $Msg(04);
035900110428             leavesr;
036000080606
036100080505         endsl;
036200081209
036300081209         FIOR16ds = kpjbu;
036400110428         V1Dtxt = 'Stampato O.R.M.';
036500080704         write  Protect;
036600081209         exfmt  OR18D02;
036700080923
036800110428         // -?F3=Fine?
036900080923         if  dsp_aid = c_F03;
037000080923           $Fine = *on;
037100080923           leavesr;
037200080923         endif;
037300080428
037400080428       ENDSR;
037500080428
037600080428       //--------------------------------------------------------------
037700110428       //?Controllo dati in videata D01                                ?
037800080428       //--------------------------------------------------------------
037900110428       BEGSR  sr_CtrD01;
038000080429
038100110428         %subst(IndDspF : 50) = *off;
038200080428
038300080428         SELECT;
038400080428
038500110428           // -?Nessun O.R.M. inserito?
038600081209           WHEN  V1Cpoe = *zero   and
038700081209                 V1Cnsr = *zero   and
038800081209                 V1Cnor = *zero;
038900080428             ErrGenerico = *on;
039000110428             ErrMessage  = *on;
039100081209             PosCurORM   = *on;
039200110428             V1Dmsg = $Msg(02);
039300080429             leavesr;
039400080428
039500110428           // -?Selezionata un singolo O.R.M.?
039600081209           WHEN  V1Cpoe <> *zero   or
039700081209                 V1Cnsr <> *zero   or
039800081209                 V1Cnor <> *zero;
039900081209             k_ORMpoe = V1Cpoe;
040000081209             k_ORMnsr = V1Cnsr;
040100081209             k_ORMnor = V1Cnor;
040200081209             k_ORMnrv = V1Cnrv;
040300081209             setll  %kds(k04fnorm01) FNORM000;
040400081209             if not %equal(FNORM01L);
040500081209               ErrGenerico = *on;
040600110428               ErrMessage  = *on;
040700081209               PosCurORM   = *on;
040800110428               V1Dmsg = $Msg(03);
040900081209               leavesr;
041000081209             endif;
041100080428
041200080428         ENDSL;
041300080506
041400110428         // -?Programma da richiamare di default?
041500080506         if  V1Cpgm = *blank;
041600081209           V1Cpgm   = 'FIOR18R   ';
041700080506         endif;
041800080428
041900080428       ENDSR;
042000080606
042100080606       //--------------------------------------------------------------
042200110428       //?Fine: chiusura dei files di stampa                           ?
042300080606       //--------------------------------------------------------------
042400110428       BEGSR  sr_EndPgm;
042500081209
042600110429         if  %subst(knsif : 1 : 6) = 'FILTRA'   and
042700110429             V1Cpgm = 'FIOR18R   ';
042800110429           clear   FIOR16ds;
042900110429           R16tla = 'C';
043000110429           kpjbu  = FIOR16ds;
043100110429           fior18r (kpjba);
043200110429         endif;
043300080606
043400080606       ENDSR;
043500080424
043600080424      /end-free
043700080428
043800110428** - $Msg:?Messaggi di Errore?-----------------------------------------------*
043900110428Programma NON eseguibile nel sistema informativo di SEDE. "Invio" per uscire.
044000081209Nessuna selezione inserita
044100081209O.R.M. inesistente
044200110428Programma NON previsto
