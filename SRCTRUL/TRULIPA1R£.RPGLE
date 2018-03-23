000100150413       //==============================================================
000200150413       //?TRULIPA * Creaz. file AZIPA* (Indice Pubbliche Amministraz.) ?
000300150413       //==============================================================
000400150413
000500150413       //--------------------------------------------------------------
000600150413       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150413       //--------------------------------------------------------------
000800150413
000900150413     /*PRM dbgview(*source)
001000150413     /*END
001100150413
001200150413       //--------------------------------------------------------------
001300150413       //?Specifiche di controllo.                                     ?
001400150413       //--------------------------------------------------------------
001500150413
001600150413     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150413     h dftactgrp(*no)
001800150413
001900150413       //--------------------------------------------------------------
002000150413       //?Dichiarazione file.                                          ?
002100150413       //--------------------------------------------------------------
002200150512
002300150512       // -?Video Gestione?
002400151015     fTRULIPA1D cf   e             workstn
002500150512     f                                     indds(IndDspF)
002600150512     f                                     infds(InfDspF)
002700150413
002800150413       //--------------------------------------------------------------
002900150413       //?Definizione costanti.                                        ?
003000150413       //--------------------------------------------------------------
003100150512
003200150512       // -?Tasti funzionali a video?
003300150512     d c_F01           c                   const(x'31')
003400150512     d c_F02           c                   const(x'32')
003500150512     d c_F03           c                   const(x'33')
003600150512     d c_F04           c                   const(x'34')
003700150512     d c_F05           c                   const(x'35')
003800150512     d c_F06           c                   const(x'36')
003900150512     d c_F07           c                   const(x'37')
004000150512     d c_F08           c                   const(x'38')
004100150512     d c_F09           c                   const(x'39')
004200150512     d c_F10           c                   const(x'3A')
004300150512     d c_F11           c                   const(x'3B')
004400150512     d c_F12           c                   const(x'3C')
004500150512     d c_F13           c                   const(x'B1')
004600150512     d c_F14           c                   const(x'B2')
004700150512     d c_F15           c                   const(x'B3')
004800150512     d c_F16           c                   const(x'B4')
004900150512     d c_F17           c                   const(x'B5')
005000150512     d c_F18           c                   const(x'B6')
005100150512     d c_F19           c                   const(x'B7')
005200150512     d c_F20           c                   const(x'B8')
005300150512     d c_F21           c                   const(x'B9')
005400150512     d c_F22           c                   const(x'BA')
005500150512     d c_F23           c                   const(x'BB')
005600150512     d c_F24           c                   const(x'BC')
005700150512     d c_Enter         c                   const(x'F1')
005800150512     d c_RollDown      c                   const(x'F4')
005900150512     d c_RollUp        c                   const(x'F5')
006000150413
006100150413       //--------------------------------------------------------------
006200150413       //?Definizione schiere.                                         ?
006300150413       //--------------------------------------------------------------
006400150413
006500160916       // -?Messaggi di errore?
006600160916     d sk_Msg          s             78    dim( 9)  ctdata  perrcd(1)
006700150413
006800150413       //--------------------------------------------------------------
006900150413       //?Definizione aree dati.                                       ?
007000150413       //--------------------------------------------------------------
007100150413
007200150413       // -?Dati utente?
007300150413     d §AzUte        e ds                  extname(AZUTE00F)
007400150413     d                                     dtaara
007500150413     d §DatiUte      e ds                  extname(dDatiUte)
007600150413     d                                     dtaara
007700150413
007800150413       //--------------------------------------------------------------
007900150413       //?Definizione strutture dati.                                  ?
008000150413       //--------------------------------------------------------------
008100150413
008200150413       // -?Status ds?
008300150413     d Status         sds
008400150413     d   SDSpgm          *proc
008500150413     d*//SDSprm          *parms
008600150413     d*//SDSdta              191    198
008700150413     d*//SDSjob              244    253
008800150413     d   SDSusr              254    263
008900150512
009000150512       // -?InfDS?
009100150512     d InfDspF         ds
009200150512     d   dsp_aid             369    369a
009300150512
009400150512       // -?Indicatori su DspF?
009500150512     d IndDspF         ds                  inz
009600151015         // -?Abilitazione tasti funzionali?
009700151015     d   $F6Attivo                     n   overlay(IndDspF : 06)
009800151015     d   $F12Attivo                    n   overlay(IndDspF : 12)
009900150512         // ·?Emissione messaggio di errore?
010000150512     d   $ErrMessage                   n   overlay(IndDspF : 28)
010100150512         // ·?Indicatori per Attibuti di visualizzazione?
010200150512         // ·?Posizionamento cursore & Segnalazione errore?
010300150512     d   $PosCurIP1                    n   overlay(IndDspF : 51)
010400150512     d   $PosCurIP2                    n   overlay(IndDspF : 52)
010500150512     d   $PosCurCRTF                   n   overlay(IndDspF : 53)
010600150512     d   $PosCurIPA                    n   overlay(IndDspF : 54)
010700150512     d   $PosCurCPYF                   n   overlay(IndDspF : 55)
010800151015     d   $PosCurDLTX                   n   overlay(IndDspF : 56)
010900150512         // ·?Riemissione videata?
011000150512     d   $ErrGenerico                  n   overlay(IndDspF : 99)
011100150413
011200150413       // -?Parametri ricevuti?
011300150413     d KPJBA         e ds
011400150413
011500150413       //--------------------------------------------------------------
011600150413       //?Definizione variabili globali.                               ?
011700150413       //--------------------------------------------------------------
011800150415
011900150415       // -?Flags booleani?
012000150512     d $Fine           s               n   inz
012100150512     d $InzD01         s               n   inz(*on)
012200150512
012300150512       // -?Variabili per la gestione del video?
012400150512     d $Video          s              2    inz('D1')
012500150413
012600150413       //--------------------------------------------------------------
012700150413       //?Definizione prototipi procedure usate.                       ?
012800150413       //--------------------------------------------------------------
012900150413
013000150413       // -?Reperimento dati utente?
013100150413     d TIBS34ds      e ds                  inz
013200150413      /copy gaitrasrc/srcProtoPR,TIBS34R
013300151015
013400151015       // -?Creazione *file iPA (Indice Pubbliche Amministrazioni)?
013500151015     d trulIPAds     e ds                  inz
013600151015     d trulIPAr        pr                  extpgm('TRULIPAR')
013700151015     d   trulIPAds                         likeds(TRULIPAds)
013800150413
013900150413       //--------------------------------------------------------------
014000150413       //?Definizione key-list.                                        ?
014100150413       //--------------------------------------------------------------
014200150413
014300150413
014400150413       //--------------------------------------------------------------
014500150413       //?Riepilogo indicatori utilizzati.                             ?
014600150413       //--------------------------------------------------------------
014700150413
014800150413
014900150413       //--------------------------------------------------------------
015000150413       //?M A I N - L I N E                                            ?
015100150413       //--------------------------------------------------------------
015200150413
015300150413     c     *Entry        plist
015400150413     c                   parm                    KPJBA
015500150413
015600150413      /free
015700150413
015800150413       // -?Operazioni iniziali?
015900150413       exsr  sr_RoutInz;
016000150512
016100150512       // -?Gestione videata?
016200150512       DoW  Not $Fine;
016300150512         select;
016400150512           when  $Video = 'D1';
016500150512             exsr  sr_GesD01;
016600150512           other;
016700150512             $Fine = *on;
016800150512         endsl;
016900150512       EndDo;
017000150413
017100150413       // -?Operazioni finali?
017200150413       exsr  sr_RoutEnd;
017300150413
017400150413       //--------------------------------------------------------------
017500150413       //?Operazioni iniziali.                                         ?
017600150413       //--------------------------------------------------------------
017700150413       BEGSR  sr_RoutInz;
017800150413
017900150413         // -?Impostazione chiusura?
018000150413         *inLR = *on;
018100150413
018200150413         // -?Reperimento dati job?
018300150413         exsr  sr_DatiJob;
018400150512
018500150512         // -?Impostazione nome programma a video?
018600150512         V1Tpgm = SDSpgm;
018700150413
018800150413       ENDSR;
018900150413
019000150413       //--------------------------------------------------------------
019100150413       //?Reperimento Dati del job (Utente/Operativi).                 ?
019200150413       //--------------------------------------------------------------
019300150413       BEGSR  sr_DatiJob;
019400150413
019500150413         in(e) §AzUte;
019600150413         if NOT %error;
019700150413           in(e) §DatiUte;
019800150413         endif;
019900150413         if %error or RSut = *blank;
020000150413           tibs34r ( tibs34ds );
020100150413           in §AzUte;
020200150413           in §DatiUte;
020300150413         endif;
020400150413
020500150413       ENDSR;
020600150512
020700150512       //--------------------------------------------------------------
020800150512       //?Gestione videata D01.                                        ?
020900150512       //--------------------------------------------------------------
021000150512       BEGSR  sr_GesD01;
021100150512
021200150512         // -?Inizializzazione videata?
021300150512         if  $InzD01 = *on;
021400150512           exsr  sr_InzD01;
021500150512           $InzD01 = *off;
021600150512         endif;
021700150512
021800150512         // -?Emissione Testata e Piede con tasti funzionali abilitati?
021900151015         $F6Attivo  = *on;
022000151015         $F12Attivo = *off;
022100151015         write  ulIPAt1;
022200151015         write  ulIPAp1;
022300150512
022400150512         // -?Emissione videata?
022500151015         exfmt  ulIPAd1;
022600150512
022700150512         clear  V1Dmsg;
022800150512         reset  $ErrMessage;
022900150512         reset  $ErrGenerico;
023000150512
023100150512         SELECT;
023200150512
023300150512           // -?F3=Fine?
023400150512           WHEN  dsp_aid = c_F03;
023500150512             $Fine  = *on;
023600150512
023700150512           // -?Enter; F6=Conferma?
023800150512           OTHER;
023900150512             exsr  sr_CtrD01;
024000150512             if  $ErrGenerico;
024100150512               leavesr;
024200150512             endif;
024300150512             // -?F6=Conferma?
024400150512             if  dsp_aid = c_F06;
024500151015               exsr  sr_CallTrulIPA;
024600150512             endif;
024700150512
024800150512         ENDSL;
024900150512
025000150512       ENDSR;
025100150512
025200150512       //--------------------------------------------------------------
025300150512       //?Inizializzazione videata D01.                                ?
025400150512       //--------------------------------------------------------------
025500150512       BEGSR  sr_InzD01;
025600150512
025700150512         // -?Spegnimento degli indicatori di errore?
025800150512         %subst(IndDspF : 50) = *off;
025900150512
026000150512         // -?Pulizia videata?
026100150512         clear  ULIPAD1;
026200150512
026300150512         // -?Impostazione directory/dati di default?
026400160915         ulIPAip1  = '/temp/iPA/amministrazioni.xls';
026500160915         ulIPAip2  = '/temp/iPA/ou.xls';
026600151015         ulIPAcrtF = 'S';
026700151015         ulIPAipa  = 'S';
026800151015         ulIPAcpyF = 'S';
026900151015         ulIPAdltX = 'S';
027000150512
027100150512       ENDSR;
027200150512
027300150512       //--------------------------------------------------------------
027400150512       //?Controllo dati videata D01.                                  ?
027500150512       //--------------------------------------------------------------
027600150512       BEGSR  sr_CtrD01;
027700150512
027800150512         // -?Spegnimento degli indicatori di errore?
027900150512         %subst(IndDspF : 50) = *off;
028000160916
028100160916         // -?TUTTI i controlli verranno eseguiti dal *pgm chiamato?
028200160916
028300160916         //*select;
028400160916         //*· // -?Importazione dati su AS/400?
028500160916         //*· when  ulIPAcrtF <> *blank  and  ulIPAcrtF <> 'S';
028600160916         //*·   $PocCurCRTF  = *on;
028700160916         //*·   $ErrMessage  = *on;
028800160916         //*·   $ErrGenerico = *on;
028900160916         //*·   V1Dmsg = sk_Msg(01);
029000160916         //*·   leavesr;
029100160916         //*· // -?Aggiornamento dati su AS/400?
029200160916         //*· when  ulIPAipa  <> *blank  and  ulIPAipa  <> 'S';
029300160916         //*·   $PocCurIPA   = *on;
029400160916         //*·   $ErrMessage  = *on;
029500160916         //*·   $ErrGenerico = *on;
029600160916         //*·   V1Dmsg = sk_Msg(01);
029700160916         //*·   leavesr;
029800160916         //*· // -?Fare copie di salvataggio?
029900160916         //*· when  ulIPAcpyF <> *blank  and  ulIPAcpyF <> 'S';
030000160916         //*·   $PocCurCPYF  = *on;
030100160916         //*·   $ErrMessage  = *on;
030200160916         //*·   $ErrGenerico = *on;
030300160916         //*·   V1Dmsg = sk_Msg(01);
030400160916         //*·   leavesr;
030500160916         //*· // -?Cancellazione dati scaricati?
030600160916         //*· when  ulIPAdltX <> *blank  and  ulIPAdltX <> 'S';
030700160916         //*·   $PocCurDLTX  = *on;
030800160916         //*·   $ErrMessage  = *on;
030900160916         //*·   $ErrGenerico = *on;
031000160916         //*·   V1Dmsg = sk_Msg(01);
031100160916         //*·   leavesr;
031200160916         //*endsl;
031300150512
031400150512       ENDSR;
031500150512
031600150512       //--------------------------------------------------------------
031700150513       //?Esecuzione delle operazioni richieste.                       ?
031800150512       //--------------------------------------------------------------
031900151015       BEGSR  sr_CallTrulIPA;
032000151015
032100151015         // -?Emissione videata di "attesa"?
032200151015         write  ulIPAt1;
032300151015         write  ulIPAp1;
032400151015         write  ulIPAd1;
032500151015         write  Protect;
032600151015         write  ulIPAd2;
032700150512
032800151015         // -?Richiamo *pgm Creazione file "Indice Pubbliche Amministrazioni"?
032900151015         trulIPAr ( trulIPAds );
033000151015
033100151015         // -?Emissione ultima videata (con il risultato)?
033200151015         $F6Attivo  = *off;
033300151015         $F12Attivo = *on;
033400151015
033500151015         write  ulIPAt1;
033600151015         write  ulIPAp1;
033700151015         write  ulIPAd1;
033800151015         write  Protect;
033900151015
034000151015         DoU  dsp_aid = c_F03  or  dsp_aid = c_F12;
034100151015
034200151015           exfmt  ulIPAd3;
034300151015
034400151015           // -?F3=Fine?
034500151015           if  dsp_aid = c_F03;
034600151015             $Fine = *on;
034700151015           endif;
034800151015
034900151015         EndDo;
035000150512
035100150512       ENDSR;
035200150413
035300150413       //--------------------------------------------------------------
035400150413       //?Operazioni finali.                                           ?
035500150413       //--------------------------------------------------------------
035600150413       BEGSR  sr_RoutEnd;
035700150413
035800150413         // -?Uscita dal *pgm?
035900150413         return;
036000150413
036100150413       ENDSR;
036200150413
036300150413      /end-free
036400160916
036500160916** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
036600160916Inserire "S" per SÌ, lasciare vuoto per NO                                      1
