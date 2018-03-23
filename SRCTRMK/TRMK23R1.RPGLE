000100100909       //==============================================================
000200100909       //?E-mail di conferma appuntamento: FILTRO                      ?
000300100909       //==============================================================
000400130827
000500130827       //--------------------------------------------------------------
000600130827       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130827       //--------------------------------------------------------------
000800130827
000900130827     /*END
001000100909
001100100909       //--------------------------------------------------------------
001200100909       //?Specifiche di controllo.                                     ?
001300100909       //--------------------------------------------------------------
001400100909
001500100909     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001600100909     h dftactgrp(*no) actgrp(*caller)
001700100910       // - Deve essere specificato DFTACTGRP(*NO) per un prototipo che
001800100910       //   non ha la parola chiave EXTPGM.
001900100910       //  ?(vedi API QSNDRTVMOD - Retrieve Display Mode)?
002000100909
002100100909       //---------------------------------------------------------------
002200100909       //?Dichiarazione file.
002300100909       //---------------------------------------------------------------
002400100909
002500130827      // -?Anagrafica Commerciali?
002600130827     fAZCMM01L  if   e           k disk
002700130827     fAZNTC01L  if   e           k disk
002800100909
002900100909     fTRMK23D1  cf   e             workstn usropn
003000100909     f                                     infds(InfDspF)
003100100909     f                                     indds(IndDspF)
003200100909
003300100909       //--------------------------------------------------------------
003400100909       //?Definizione costanti.                                        ?
003500100909       //--------------------------------------------------------------
003600100909
003700100909       // -?Tasti funzionali a video?
003800100909     d c_F01           c                   const(x'31')
003900100909     d c_F02           c                   const(x'32')
004000100909     d c_F03           c                   const(x'33')
004100100909     d c_F04           c                   const(x'34')
004200100909     d c_F05           c                   const(x'35')
004300100909     d c_F06           c                   const(x'36')
004400100909     d c_F07           c                   const(x'37')
004500100909     d c_F08           c                   const(x'38')
004600100909     d c_F09           c                   const(x'39')
004700100909     d c_F10           c                   const(x'3A')
004800100909     d c_F11           c                   const(x'3B')
004900100909     d c_F12           c                   const(x'3C')
005000100909     d c_F13           c                   const(x'B1')
005100100909     d c_F14           c                   const(x'B2')
005200100909     d c_F15           c                   const(x'B3')
005300100909     d c_F16           c                   const(x'B4')
005400100909     d c_F17           c                   const(x'B5')
005500100909     d c_F18           c                   const(x'B6')
005600100909     d c_F19           c                   const(x'B7')
005700100909     d c_F20           c                   const(x'B8')
005800100909     d c_F21           c                   const(x'B9')
005900100909     d c_F22           c                   const(x'BA')
006000100909     d c_F23           c                   const(x'BB')
006100100909     d c_F24           c                   const(x'BC')
006200100909     d c_Enter         c                   const(x'F1')
006300100909     d c_RollDown      c                   const(x'F4')
006400100909     d c_RollUp        c                   const(x'F5')
006500100909
006600100909       // -?Costante per controllo "caratteri solo numerici"?
006700100909     d c_Digits        c                   const('0123456789')
006800100909
006900100909       //--------------------------------------------------------------
007000100909       //?Definizione schiere.                                         ?
007100100909       //--------------------------------------------------------------
007200100909
007300100909       // -?Messaggi di errore?
007400120611     d $Msg            s             78    dim( 4) ctdata perrcd(1)
007500100909
007600100909       //--------------------------------------------------------------
007700100909       //?Definizione aree dati.                                       ?
007800100909       //--------------------------------------------------------------
007900100909
008000100909       // -?Dati utente?
008100100909     d §AzUte        e ds                  extname(AZUTE00F)
008200100909     d                                     dtaara
008300100909     d §DatiUte      e ds                  extname(dDatiUte)
008400100909     d                                     dtaara
008500100909
008600100909       //--------------------------------------------------------------
008700100909       //?Definizione strutture dati.                                  ?
008800100909       //--------------------------------------------------------------
008900100909
009000100909       // -?InfDS?
009100100909     d InfDspF         ds
009200100909     d   dsp_aid             369    369a
009300100909
009400100909       // -?Indicatori su DspF?
009500100909     d IndDspF         ds
009600100909         // -?Utilizzo del DspF in formato *DS4?
009700100909     d   FmtDspF_DS4                   n   overlay(IndDspF : 04)
009800100909         // -?Abilitazione tasti funzionali?
009900100909     d   F12Attivo                     n   overlay(IndDspF : 12)
010000100909         // -?Protezione campi?
010100100909     d   ProtectCMM                    n   overlay(IndDspF : 25)
010200100909         // -?Emissione messaggio di errore?
010300100909     d   ErrMessage                    n   overlay(IndDspF : 28)
010400100909         // -?Posizionamento cursore?
010500100910     d   PosCurCMM                     n   overlay(IndDspF : 50)
010600100910     d   PosCurRST                     n   overlay(IndDspF : 51)
010700100910     d   PosCurMAIL                    n   overlay(IndDspF : 52)
010800100909         // -?Indicatori di errore generico?
010900100909     d   ErrGenerico                   n   overlay(IndDspF : 99)
011000100909
011100100909       // -?Parametri ricevuti?
011200100909     d KPJBA         e ds
011300100909     d TRMK23ds1     e ds                  qualified
011400100909
011500100909       // -?Parametri per Reperimento dati utente?
011600100909     d TIBS34ds      e ds
011700100909     d dUte01        e ds
011800100909
011900100909       //--------------------------------------------------------------
012000100909       //?Definizione variabili globali.                               ?
012100100909       //--------------------------------------------------------------
012200100909
012300100909       // -?Flags booleani?
012400100909     d $Fine           s               n   inz(*off)
012500100909     d $InzW01         s               n   inz(*on)
012600100909
012700100909       // -?Variabili per la gestione del video?
012800100909     d $Video          s              2    inz('W1')
012900100909
013000100909       // -?Campi di comodo per la verifica di cambio codice?
013100100909     d SaveCMM         s                   like(W1Ccmm) inz(*loval)
013200111128
013300111128       // -?Campi di comodo
013400111128     d wOggi           s              8s 0
013500100909
013600100909       //---------------------------------------------------------------
013700100909       //?Definizione prototipi & relativi parametri                   ?
013800100909       //---------------------------------------------------------------
013900100909
014000100909       // -?Reperimento dati utente?
014100100909      /copy gaitrasrc/srcprotopr,TIBS34R
014200100909
014300100909       // -?Ricerca commerciali?
014400130827      /copy gaitrasrc/srcprotoPI,TRMK43R_1
014500130827      /copy gaitrasrc/srcprotoPR,TRMK43R
014600100909
014700100909       // -?Controllo abilitazioni utente?
014800100909     d TNTAA1ds      e ds                  inz
014900120810     d   iTAA1tipo   e                     inz('M')
015000120810     d   iTAA1caut   e                     inz('§utegtc')
015100100909     d   iTAA1abi    e                     inz('RA')
015200100909      /copy gaitrasrc/srcprotopr,TNTAA1C
015300100909
015400100909       // -?Controllo indirizzo e-mail?
015500100909     d dsEmail       e ds                  inz
015600100909      /copy gaitrasrc/srcprotopr,XEMAIL
015700100909
015800100909       // -?Invio e-mail di conferma appuntamento?
015900100910     d TRMK23ds      e ds                  inz   qualified
016000100910     d   Omk23err    e                     inz(*off)
016100100909      /copy gaitrasrc/srcprotopr,TRMK23R
016200100909
016300100909       // -?API QSNDRTVMOD (Retrieve Display Mode)?
016400100909     d QdspMode        s              1    inz
016500100909      /copy gaitrasrc/srcProtoPR,QSNRTVMOD
016600100909
016700100909       //--------------------------------------------------------------
016800100909       //?Definizione key-list.                                        ?
016900100909       //--------------------------------------------------------------
017000100909
017100130827       // -?File AZNTC01L?
017200130827     d k02azntc01    e ds                  extname(AZNTC01L : *key)
017300100909     d                                     prefix(k_)
017400100909
017500100909       //--------------------------------------------------------------
017600100909       //?M A I N - L I N E                                            ?
017700100909       //--------------------------------------------------------------
017800100909
017900100909     c     *Entry        plist
018000100909     c                   parm                    kpjba
018100100910     c                   parm                    TRMK23ds1
018200100909
018300100909      /free
018400100909
018500100909       // -?Operazioni iniziali?
018600100909       exsr  sr_RoutInz;
018700100909
018800100909       // -?Gestione window?
018900100909       DoW  Not  $Fine;
019000100909         select;
019100100910           when  TRMK23ds1.Imk23rei = 'I';
019200100910             exsr  sr_F06W01;
019300100909           when  $Video = 'W1';
019400100910             exsr  sr_GesW01;
019500100909           other;
019600100909             $Fine = *on;
019700100909         endsl;
019800100909       EndDo;
019900100909
020000100909       // -?Operazioni finali?
020100100909       exsr  sr_RoutEnd;
020200100909
020300100909       //--------------------------------------------------------------
020400100909       //?Operazioni iniziali.                                         ?
020500100909       //--------------------------------------------------------------
020600100909       BEGSR  sr_RoutInz;
020700100909
020800100909         *inLR = *on;
020900100909
021000100909         // -?Reperimento dati del lavoro?
021100100909         exsr  sr_DatiJob;
021200100909
021300100915         IF  TRMK23ds1.Imk23rei <> 'I';
021400100910
021500100910           // -?Verifica se visualizzare la finestra a 80 o 132 colonne?
021600100910           //  ?in base al formato utilizzato dal chiamante?
021700100910           //FmtDspF_DS4 = (TRMK23ds1.Imk23dsp = '4');
021800100910           // -?Reperimento formato attuale display?
021900100910           //  ?per verificare se visualizzare la finestra a 80 o 132 col.?
022000100910           RetrieveDisplayMode ( QdspMode : *omit : *omit );
022100100910           FmtDspF_DS4 = (QdspMode = '4');
022200100910           open  TRMK23D1;
022300100910
022400100910         ENDIF;
022500111128
022600111128         //?Data del giorno
022700111128         wOggi = %dec(%date());
022800100909
022900100909       ENDSR;
023000100909
023100100909       //--------------------------------------------------------------
023200100909       //?Reperimento Dati del job (Utente/Operativi).                 ?
023300100909       //--------------------------------------------------------------
023400100909       BEGSR  sr_DatiJob;
023500100909
023600100909         in(E) §AzUte;
023700100909         if NOT %error;
023800100909           in(E) §DatiUte;
023900100909         endif;
024000100909         if %error or RSut = *blanks;
024100100909           clear TIBS34ds;
024200100909           tibs34r(tibs34ds);
024300100909           in §AzUte;
024400100909           in §DatiUte;
024500100909         endif;
024600100909
024700100909       ENDSR;
024800100909
024900100909       //--------------------------------------------------------------
025000100909       //?Gestione videata W01.                                        ?
025100100909       //--------------------------------------------------------------
025200100909       BEGSR  sr_GesW01;
025300100909
025400100909         // -?Inizializzazione videata?
025500100909         if  $InzW01 = *on;
025600100909           exsr sr_InzW01;
025700100909           $InzW01 = *off;
025800100909         endif;
025900100909
026000100909         // -?Emissione videata?
026100100909         exfmt  MK23W01;
026200100909
026300100909         clear  ErrMessage;
026400100909         clear  ErrGenerico;
026500100909         clear  W1Dmsg;
026600100909
026700100909         Select;
026800100909
026900100909           // -?F12=Ritorno?
027000100909           When  dsp_aid = c_F12;
027100100909             exsr  sr_F12W01;
027200100909
027300100909           // -?Invio / F6=Conferma?
027400100909           Other;
027500100909             exsr  sr_CtrW01;
027600100909             select;
027700100909               when  ErrGenerico;
027800100909                 leavesr;
027900100910               when  dsp_aid = c_F06;
028000100909                 exsr  sr_F06W01;
028100100909             endsl;
028200100909
028300100909         EndSl;
028400100909
028500100909       ENDSR;
028600100909
028700100909       //--------------------------------------------------------------
028800100909       //?Inizializzazione videata W01.                                ?
028900100909       //--------------------------------------------------------------
029000100909       BEGSR  sr_InzW01;
029100100909
029200100909         clear  MK23W01;
029300100909
029400100909         // -?Impostazione attributi di visualizzazione?
029500100909         ProtectCMM = ( TRMK23ds1.Imk23cmaP = 'S' );
029600100909
029700100909         // -?Abilitazione tasti funzionali?
029800100909         F12Attivo = ( TRMK23ds1.Imk23f12 = *blank );
029900100909
030000100909         // -?Impostazione campi a video con i dati ricevuti?
030100100910         W1Ccmm = %editc( TRMK23ds1.Imk23cma : 'X' );
030200100910         W1Crst = TRMK23ds1.Imk23rst;
030300100910         W1mail = TRMK23ds1.Imk23emr;
030400100909
030500100909         // -?Decodifica commerciale ricevuto?
030600100909         exsr  sr_CtrW01;
030700100909         %subst(IndDspF : 50) = *off;
030800100909         clear  ErrMessage;
030900100909         clear  ErrGenerico;
031000100909         clear  W1Dmsg;
031100100909
031200100909       ENDSR;
031300100909
031400100909       //--------------------------------------------------------------
031500100909       //?Controllo videata W01.                                       ?
031600100909       //--------------------------------------------------------------
031700100909       BEGSR  sr_CtrW01;
031800100909
031900100909         %subst(IndDspF : 50) = *off;
032000100909
032100100909         // -?COMMERCIALE:?
032200100909         clear  W1Dcmm;
032300100909
032400100909         Select;
032500100909
032600100909           // -?Ricerca?
032700100909           When  %scan( '?' : W1Ccmm ) > *zero;
032800100909             ErrGenerico = *on;
032900100909             PosCurCMM   = *on;
033000130827             iMK43fil  = DUTpou;
033100130827             TRMK43R (kpjba : TRMK43ds);
033200130827             if  oMK43err = *off  and  oMK43fxx = *blank;
033300130827               W1Ccmm = %editc( oMK43cmm : 'X' );
033400130827               W1Dcmm = oMK43des;
033500130827             endif;
033600100909             leavesr;
033700100909
033800100909           // -?Controllo inserimento?
033900100909           When  W1Ccmm = *blank  or  W1Ccmm = *zero;
034000100909             ErrMessage  = *on;
034100100909             ErrGenerico = *on;
034200100909             PosCurCMM   = *on;
034300100909             W1Dmsg = $Msg(01);
034400100909             leavesr;
034500100909
034600100909           // -?Controllo numericità?
034700100909           When  %check( c_Digits : W1Ccmm ) > *zero;
034800100909             ErrMessage  = *on;
034900100909             ErrGenerico = *on;
035000100909             PosCurCMM   = *on;
035100100909             W1Dmsg = $Msg(01);
035200100909             leavesr;
035300100909
035400100909         EndSl;
035500100909
035600100909         If  W1Ccmm <> SaveCMM;
035700100909
035800130827           // -?Controllo esistenza in anagrafica?
035900130827           chain  (%int(W1Ccmm))  AZCMM000;
036000130827           if  NOT %found(AZCMM01L);
036100100909             ErrMessage  = *on;
036200100909             ErrGenerico = *on;
036300100909             PosCurCMM   = *on;
036400100909             W1Dmsg = $Msg(01);
036500100909             leavesr;
036600100909           endif;
036700110614
036800110614         // -?Controllo validità (no vari o clienti inattivi)?
036900130827           IF  CMMpar <> *blanks;
037000110614             ErrMessage  = *on;
037100110614             ErrGenerico = *on;
037200110614             PosCurCMM   = *on;
037300110614             W1Dmsg = $Msg(01);
037400110614             leavesr;
037500110614           ENDIF;
037600111128
037700111128         // -?Controllo data validità?
037800130827           IF  CMMdtIni > wOggi  or
037900130827               CMMdtFin < wOggi;
038000111128             ErrMessage  = *on;
038100111128             ErrGenerico = *on;
038200111128             PosCurCMM   = *on;
038300111128             W1Dmsg = $Msg(01);
038400111128             leavesr;
038500111128           ENDIF;
038600100909
038700100909           // -?Controllo abilitazione utente al commerciale?
038800100927           reset  TNTAA1ds;
038900100909           iTAA1cmm  = %int( W1Ccmm );
039000100909           kpjbu     = TNTAA1ds;
039100100909           TNTAA1C ( kpjba );
039200100909           TNTAA1ds  = kpjbu;
039300100909           if  OTAA1fabi = 'N';
039400100909             ErrMessage  = *on;
039500100909             ErrGenerico = *on;
039600100909             PosCurCMM   = *on;
039700100909             W1Dmsg = $Msg(01);
039800100909             leavesr;
039900100909           endif;
040000100909
040100100909           SaveCMM = W1Ccmm;
040200100909
040300100909         EndIf;
040400100909
040500130827         W1Dcmm = CMMdes;
040600100909
040700130827         //?Controllo se ci sono i dati in AZNTC per il commerciale?
040800130827         k_NTCcmm = %int(W1Ccmm);
040900130827         k_NTCtnt = '01';
041000130827         chain  %kds( k02azntc01 )  AZNTC01L;
041100130827         if  %found(AZNTC01L);
041200130827           k_NTCtnt = '02';
041300130827           chain  %kds( k02azntc01 )  AZNTC01L;
041400130827         endif;
041500130827         if  NOT %found(AZNTC01L);
041600120611           errMessage  = *on;
041700120611           errGenerico = *on;
041800120611           PosCurCmm   = *on;
041900120611           W1Dmsg = %replace( W1Ccmm : $Msg(04) :
042000120611                              %scan( '&CommLe' : $Msg(04) ) );
042100120611           leavesr;
042200120611         endif;
042300120611
042400100909
042500100910         // -?RESPONSABILE TRASPORTI:?
042600100910
042700100910         // -?Controllo inserimento?
042800100910         if  W1Crst = *blank;
042900100910           ErrMessage  = *on;
043000100910           ErrGenerico = *on;
043100100910           PosCurRST   = *on;
043200100910           W1Dmsg = $Msg(02);
043300100910           leavesr;
043400100910         endif;
043500100910
043600100909         // -?INDIRIZZO E-MAIL:?
043700100909
043800100909         // -?Controllo inserimento?
043900100909         if  W1mail = *blank;
044000100909           ErrMessage  = *on;
044100100909           ErrGenerico = *on;
044200100909           PosCurMAIL  = *on;
044300100910           W1Dmsg = $Msg(03);
044400100909           leavesr;
044500100909         endif;
044600100909
044700100909         // -?Controllo correttezza?
044800100909         clear dsEmail;
044900100909         §emlIndI = W1mail;
045000100909         xEmail ( dsEmail );
045100100909         if §emlErrO = *on;
045200100909           ErrMessage  = *on;
045300100909           ErrGenerico = *on;
045400100909           PosCurMAIL  = *on;
045500100909           W1Dmsg = §emlMsgO;
045600100909           leavesr;
045700100909         endif;
045800100909         W1mail = §emlIndO;
045900100909
046000100909       ENDSR;
046100100909
046200100909       //--------------------------------------------------------------
046300100909       //?Gestione tasto funzionale F6 da videata W01:                 ?
046400100909       //?F6=Conferma                                                  ?
046500100909       //--------------------------------------------------------------
046600100909       BEGSR  sr_F06W01;
046700100909
046800100909         // -?Reimpostazione parametri gestiti a video?
046900100910         if  TRMK23ds1.Imk23rei <> 'I';
047000100913           TRMK23ds1.Omk23rst  = W1Crst;                 //(1)?
047100100913           TRMK23ds1.Omk23emr  = W1mail;                 //(2)?
047200100913           TRMK23ds1.Omk23cma  = %int( W1Ccmm );         //(3)?
047300100910         endif;
047400100910
047500100910         // -?Uscita SENZA invio e-mail?
047600100910         if  TRMK23ds1.Imk23rei = 'R';
047700100910           TRMK23ds1.Omk23fxx = '06';
047800100910           $Fine = *on;
047900100910           leavesr;
048000100910         endif;
048100100909
048200100909         // -?Invio e-mail conferma appuntamento?
048300100910         reset  TRMK23ds;
048400100913         TRMK23ds.Imk23cma = TRMK23ds1.Omk23cma;         //(3)?
048500100910         TRMK23ds.Imk23cmm = TRMK23ds1.Imk23cmm;
048600100910         TRMK23ds.Imk23dta = TRMK23ds1.Imk23dta;
048700100910         TRMK23ds.Imk23ora = TRMK23ds1.Imk23ora;
048800100913         TRMK23ds.Imk23rst = TRMK23ds1.Omk23rst;         //(1)?
048900100913         TRMK23ds.Imk23emr = TRMK23ds1.Omk23emr;         //(2)?
049000100913
049100100909         kpjbu    = TRMK23ds;
049200100909         trmk23r ( kpjba );
049300100909         TRMK23ds = kpjbu;
049400100913
049500100913         TRMK23ds1.Omk23err = TRMK23ds.Omk23err;
049600100913         TRMK23ds1.Omk23msg = TRMK23ds.Omk23msg;
049700100909
049800100913         // -?Emissione messaggio di ERRORE ricevuto?
049900100909         if  TRMK23ds.Omk23err = *on;
050000100909           ErrMessage   = *on;
050100100909           ErrGenerico  = *on;
050200100909           W1Dmsg = TRMK23ds.Omk23msg;
050300100909           leavesr;
050400100909         endif;
050500100910
050600100910         // -?Restituzione tasto funzionale premuto?
050700100910         TRMK23ds1.Omk23fxx = '06';
050800100909
050900100909         // -?Chiusura del *pgm?
051000100909         $Fine = *on;
051100100909
051200100909       ENDSR;
051300100909
051400100909       //--------------------------------------------------------------
051500100909       //?Gestione tasto funzionale F12 da videata W01:                ?
051600100909       //?F12=Ritorno                                                  ?
051700100909       //--------------------------------------------------------------
051800100909       BEGSR  sr_F12W01;
051900100909
052000100909         // -?Restituzione tasto funzionale premuto?
052100100909         TRMK23ds1.Omk23fxx = '12';
052200100909
052300100909         // -?Chiusura del *pgm?
052400100909         $Fine = *on;
052500100909
052600100909       ENDSR;
052700100909
052800100909       //--------------------------------------------------------------
052900100909       //?Operazioni finali.                                           ?
053000100909       //--------------------------------------------------------------
053100100909       BEGSR  sr_RoutEnd;
053200100909
053300100909         // -?Uscita?
053400100909         return;
053500100909
053600100909       ENDSR;
053700100909
053800100909      /end-free
053900100909
054000100909       //--------------------------------------------------------------
054100100909       //?Messaggi di errore.                                          ?
054200100909       //--------------------------------------------------------------
054300100909
054400100909** -?$Msg?-------------------------------------------------------------------*
054500100909Commerciale errato o non in gestione all'utente                                 1
054600100910Nominativo del responsabile trasporti obbligatorio                              2
054700100910Indirizzo e-mail obbligatorio                                                   3
054800130827Dati del commerciale "&CommLe" non reperiti in "Note/Rubrica"                   4
