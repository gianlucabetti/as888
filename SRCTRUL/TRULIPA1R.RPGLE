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
010700160916     d   $PosCurDLTI                   n   overlay(IndDspF : 55)
010800150512         // ·?Riemissione videata?
010900150512     d   $ErrGenerico                  n   overlay(IndDspF : 99)
011000150413
011100150413       // -?Parametri ricevuti?
011200150413     d KPJBA         e ds
011300150413
011400150413       //--------------------------------------------------------------
011500150413       //?Definizione variabili globali.                               ?
011600150413       //--------------------------------------------------------------
011700150415
011800150415       // -?Flags booleani?
011900150512     d $Fine           s               n   inz
012000150512     d $InzD01         s               n   inz(*on)
012100150512
012200150512       // -?Variabili per la gestione del video?
012300150512     d $Video          s              2    inz('D1')
012400150413
012500150413       //--------------------------------------------------------------
012600150413       //?Definizione prototipi procedure usate.                       ?
012700150413       //--------------------------------------------------------------
012800150413
012900150413       // -?Reperimento dati utente?
013000150413     d TIBS34ds      e ds                  inz
013100150413      /copy gaitrasrc/srcProtoPR,TIBS34R
013200151015
013300151015       // -?Creazione *file iPA (Indice Pubbliche Amministrazioni)?
013400151015     d trulIPAds     e ds                  inz
013500151015     d trulIPAr        pr                  extpgm('TRULIPAR')
013600160926     d   kpjba                             likeds(KPJBA)
013700160926     d   pIn_StaErr                   1a   const
013800160926     d                                     options(*nopass : *omit)
013900150413
014000150413       //--------------------------------------------------------------
014100150413       //?Definizione key-list.                                        ?
014200150413       //--------------------------------------------------------------
014300150413
014400150413
014500150413       //--------------------------------------------------------------
014600150413       //?Riepilogo indicatori utilizzati.                             ?
014700150413       //--------------------------------------------------------------
014800150413
014900150413
015000150413       //--------------------------------------------------------------
015100150413       //?M A I N - L I N E                                            ?
015200150413       //--------------------------------------------------------------
015300150413
015400150413     c     *Entry        plist
015500150413     c                   parm                    KPJBA
015600150413
015700150413      /free
015800150413
015900150413       // -?Operazioni iniziali?
016000150413       exsr  sr_RoutInz;
016100150512
016200150512       // -?Gestione videata?
016300150512       DoW  Not $Fine;
016400150512         select;
016500150512           when  $Video = 'D1';
016600150512             exsr  sr_GesD01;
016700150512           other;
016800150512             $Fine = *on;
016900150512         endsl;
017000150512       EndDo;
017100150413
017200150413       // -?Operazioni finali?
017300150413       exsr  sr_RoutEnd;
017400150413
017500150413       //--------------------------------------------------------------
017600150413       //?Operazioni iniziali.                                         ?
017700150413       //--------------------------------------------------------------
017800150413       BEGSR  sr_RoutInz;
017900150413
018000150413         // -?Impostazione chiusura?
018100150413         *inLR = *on;
018200150413
018300150413         // -?Reperimento dati job?
018400150413         exsr  sr_DatiJob;
018500150512
018600150512         // -?Impostazione nome programma a video?
018700150512         V1Tpgm = SDSpgm;
018800150413
018900150413       ENDSR;
019000150413
019100150413       //--------------------------------------------------------------
019200150413       //?Reperimento Dati del job (Utente/Operativi).                 ?
019300150413       //--------------------------------------------------------------
019400150413       BEGSR  sr_DatiJob;
019500150413
019600150413         in(e) §AzUte;
019700150413         if NOT %error;
019800150413           in(e) §DatiUte;
019900150413         endif;
020000150413         if %error or RSut = *blank;
020100150413           tibs34r ( tibs34ds );
020200150413           in §AzUte;
020300150413           in §DatiUte;
020400150413         endif;
020500150413
020600150413       ENDSR;
020700150512
020800150512       //--------------------------------------------------------------
020900150512       //?Gestione videata D01.                                        ?
021000150512       //--------------------------------------------------------------
021100150512       BEGSR  sr_GesD01;
021200150512
021300150512         // -?Inizializzazione videata?
021400150512         if  $InzD01 = *on;
021500150512           exsr  sr_InzD01;
021600150512           $InzD01 = *off;
021700150512         endif;
021800150512
021900150512         // -?Emissione Testata e Piede con tasti funzionali abilitati?
022000151015         $F6Attivo  = *on;
022100151015         $F12Attivo = *off;
022200151015         write  ulIPAt1;
022300151015         write  ulIPAp1;
022400150512
022500150512         // -?Emissione videata?
022600151015         exfmt  ulIPAd1;
022700150512
022800150512         clear  V1Dmsg;
022900150512         reset  $ErrMessage;
023000150512         reset  $ErrGenerico;
023100150512
023200150512         SELECT;
023300150512
023400150512           // -?F3=Fine?
023500150512           WHEN  dsp_aid = c_F03;
023600150512             $Fine  = *on;
023700150512
023800150512           // -?Enter; F6=Conferma?
023900150512           OTHER;
024000150512             exsr  sr_CtrD01;
024100150512             if  $ErrGenerico;
024200150512               leavesr;
024300150512             endif;
024400150512             // -?F6=Conferma?
024500150512             if  dsp_aid = c_F06;
024600151015               exsr  sr_CallTrulIPA;
024700150512             endif;
024800150512
024900150512         ENDSL;
025000150512
025100150512       ENDSR;
025200150512
025300150512       //--------------------------------------------------------------
025400150512       //?Inizializzazione videata D01.                                ?
025500150512       //--------------------------------------------------------------
025600150512       BEGSR  sr_InzD01;
025700150512
025800150512         // -?Spegnimento degli indicatori di errore?
025900150512         %subst(IndDspF : 50) = *off;
026000150512
026100150512         // -?Pulizia videata?
026200150512         clear  ULIPAD1;
026300150512
026400150512         // -?Impostazione directory/dati di default?
026500160920         ulIPAip1  = '/temp/iPA/amministrazioni.txt';
026600160920         ulIPAip2  = '/temp/iPA/ou.txt';
026700151015         ulIPAcrtF = 'S';
026800151015         ulIPAipa  = 'S';
026900160916         ulIPAdltI = 'S';
027000160926         //*·VD1staErr = 'S';
027100150512
027200150512       ENDSR;
027300150512
027400150512       //--------------------------------------------------------------
027500150512       //?Controllo dati videata D01.                                  ?
027600150512       //--------------------------------------------------------------
027700150512       BEGSR  sr_CtrD01;
027800150512
027900150512         // -?Spegnimento degli indicatori di errore?
028000150512         %subst(IndDspF : 50) = *off;
028100160916
028200160916         // -?TUTTI i controlli verranno eseguiti dal *pgm chiamato?
028300160916
028400160916         //*select;
028500160916         //*· // -?Importazione dati su AS/400?
028600160916         //*· when  ulIPAcrtF <> *blank  and  ulIPAcrtF <> 'S';
028700160916         //*·   $PocCurCRTF  = *on;
028800160916         //*·   $ErrMessage  = *on;
028900160916         //*·   $ErrGenerico = *on;
029000160916         //*·   V1Dmsg = sk_Msg(01);
029100160916         //*·   leavesr;
029200160916         //*· // -?Aggiornamento dati su AS/400?
029300160916         //*· when  ulIPAipa  <> *blank  and  ulIPAipa  <> 'S';
029400160916         //*·   $PocCurIPA   = *on;
029500160916         //*·   $ErrMessage  = *on;
029600160916         //*·   $ErrGenerico = *on;
029700160916         //*·   V1Dmsg = sk_Msg(01);
029800160916         //*·   leavesr;
029900160916         //*· // -?Cancellazione dati scaricati?
030000160916         //*· when  ulIPAdltI <> *blank  and  ulIPAdltI <> 'S';
030100160916         //*·   $PocCurDLTI  = *on;
030200160916         //*·   $ErrMessage  = *on;
030300160916         //*·   $ErrGenerico = *on;
030400160916         //*·   V1Dmsg = sk_Msg(01);
030500160916         //*·   leavesr;
030600160916         //*endsl;
030700150512
030800150512       ENDSR;
030900150512
031000150512       //--------------------------------------------------------------
031100150513       //?Esecuzione delle operazioni richieste.                       ?
031200150512       //--------------------------------------------------------------
031300151015       BEGSR  sr_CallTrulIPA;
031400151015
031500151015         // -?Emissione videata di "attesa"?
031600151015         write  ulIPAt1;
031700151015         write  ulIPAp1;
031800151015         write  ulIPAd1;
031900151015         write  Protect;
032000151015         write  ulIPAd2;
032100150512
032200151015         // -?Richiamo *pgm Creazione file "Indice Pubbliche Amministrazioni"?
032300160926         kpjbu = trulIPAds;
032400160926         if  VD1staErr = 'S';
032500160926           trulIPAr ( kpjba );
032600160926         else;
032700160926           trulIPAr ( kpjba : 'S' );
032800160926         endif;
032900160926         trulIPAds = kpjbu;
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
