000100100909       //==============================================================
000200100909       //?E-mail di conferma appuntamento: FILTRO                      ?
000300100909       //==============================================================
000400100909
000500100909       //--------------------------------------------------------------
000600100909       //?Specifiche di controllo.                                     ?
000700100909       //--------------------------------------------------------------
000800100909
000900100909     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001000100909     h dftactgrp(*no) actgrp(*caller)
001100100910       // - Deve essere specificato DFTACTGRP(*NO) per un prototipo che
001200100910       //   non ha la parola chiave EXTPGM.
001300100910       //  ?(vedi API QSNDRTVMOD - Retrieve Display Mode)?
001400100909
001500100909       //---------------------------------------------------------------
001600100909       //?Dichiarazione file.
001700100909       //---------------------------------------------------------------
001800100909
001900100909       // -?Tabelle?
002000100909     fTABEL00F  if   e           k disk
002100120611     fTNTBE01L  if   e           k disk
002200100909
002300100909     fTRMK23D1  cf   e             workstn usropn
002400100909     f                                     infds(InfDspF)
002500100909     f                                     indds(IndDspF)
002600100909
002700100909       //--------------------------------------------------------------
002800100909       //?Definizione costanti.                                        ?
002900100909       //--------------------------------------------------------------
003000100909
003100100909       // -?Tasti funzionali a video?
003200100909     d c_F01           c                   const(x'31')
003300100909     d c_F02           c                   const(x'32')
003400100909     d c_F03           c                   const(x'33')
003500100909     d c_F04           c                   const(x'34')
003600100909     d c_F05           c                   const(x'35')
003700100909     d c_F06           c                   const(x'36')
003800100909     d c_F07           c                   const(x'37')
003900100909     d c_F08           c                   const(x'38')
004000100909     d c_F09           c                   const(x'39')
004100100909     d c_F10           c                   const(x'3A')
004200100909     d c_F11           c                   const(x'3B')
004300100909     d c_F12           c                   const(x'3C')
004400100909     d c_F13           c                   const(x'B1')
004500100909     d c_F14           c                   const(x'B2')
004600100909     d c_F15           c                   const(x'B3')
004700100909     d c_F16           c                   const(x'B4')
004800100909     d c_F17           c                   const(x'B5')
004900100909     d c_F18           c                   const(x'B6')
005000100909     d c_F19           c                   const(x'B7')
005100100909     d c_F20           c                   const(x'B8')
005200100909     d c_F21           c                   const(x'B9')
005300100909     d c_F22           c                   const(x'BA')
005400100909     d c_F23           c                   const(x'BB')
005500100909     d c_F24           c                   const(x'BC')
005600100909     d c_Enter         c                   const(x'F1')
005700100909     d c_RollDown      c                   const(x'F4')
005800100909     d c_RollUp        c                   const(x'F5')
005900100909
006000100909       // -?Costante per controllo "caratteri solo numerici"?
006100100909     d c_Digits        c                   const('0123456789')
006200100909
006300100909       //--------------------------------------------------------------
006400100909       //?Definizione schiere.                                         ?
006500100909       //--------------------------------------------------------------
006600100909
006700100909       // -?Messaggi di errore?
006800120611     d $Msg            s             78    dim( 4) ctdata perrcd(1)
006900100909
007000100909       //--------------------------------------------------------------
007100100909       //?Definizione aree dati.                                       ?
007200100909       //--------------------------------------------------------------
007300100909
007400100909       // -?Dati utente?
007500100909     d §AzUte        e ds                  extname(AZUTE00F)
007600100909     d                                     dtaara
007700100909     d §DatiUte      e ds                  extname(dDatiUte)
007800100909     d                                     dtaara
007900100909
008000100909       //--------------------------------------------------------------
008100100909       //?Definizione strutture dati.                                  ?
008200100909       //--------------------------------------------------------------
008300100909
008400100909       // -?InfDS?
008500100909     d InfDspF         ds
008600100909     d   dsp_aid             369    369a
008700100909
008800100909       // -?Indicatori su DspF?
008900100909     d IndDspF         ds
009000100909         // -?Utilizzo del DspF in formato *DS4?
009100100909     d   FmtDspF_DS4                   n   overlay(IndDspF : 04)
009200100909         // -?Abilitazione tasti funzionali?
009300100909     d   F12Attivo                     n   overlay(IndDspF : 12)
009400100909         // -?Protezione campi?
009500100909     d   ProtectCMM                    n   overlay(IndDspF : 25)
009600100909         // -?Emissione messaggio di errore?
009700100909     d   ErrMessage                    n   overlay(IndDspF : 28)
009800100909         // -?Posizionamento cursore?
009900100910     d   PosCurCMM                     n   overlay(IndDspF : 50)
010000100910     d   PosCurRST                     n   overlay(IndDspF : 51)
010100100910     d   PosCurMAIL                    n   overlay(IndDspF : 52)
010200100909         // -?Indicatori di errore generico?
010300100909     d   ErrGenerico                   n   overlay(IndDspF : 99)
010400100909
010500100909       // -?Parametri ricevuti?
010600100909     d KPJBA         e ds
010700100909     d TRMK23ds1     e ds                  qualified
010800100909
010900100909       // -?Parametri per Reperimento dati utente?
011000100909     d TIBS34ds      e ds
011100100909     d dUte01        e ds
011200100909
011300100909       // -?Tab. "01" = Agenti commerciali?
011400100909     d ds01          e ds                  inz
011500100909
011600100909       //--------------------------------------------------------------
011700100909       //?Definizione variabili globali.                               ?
011800100909       //--------------------------------------------------------------
011900100909
012000100909       // -?Flags booleani?
012100100909     d $Fine           s               n   inz(*off)
012200100909     d $InzW01         s               n   inz(*on)
012300100909
012400100909       // -?Variabili per la gestione del video?
012500100909     d $Video          s              2    inz('W1')
012600100909
012700100909       // -?Campi di comodo per la verifica di cambio codice?
012800100909     d SaveCMM         s                   like(W1Ccmm) inz(*loval)
012900111128
013000111128       // -?Campi di comodo
013100111128     d wOggi           s              8s 0
013200100909
013300100909       //---------------------------------------------------------------
013400100909       //?Definizione prototipi & relativi parametri                   ?
013500100909       //---------------------------------------------------------------
013600100909
013700100909       // -?Reperimento dati utente?
013800100909      /copy gaitrasrc/srcprotopr,TIBS34R
013900100909
014000100909       // -?Ricerca commerciali?
014100100909     d TB85Kfil        s              3    inz
014200100909     d TB85Key         s              8    inz
014300100909     d TB85Des1        s             25    inz
014400100909     d TB85Uni         s              1    inz('S')
014500100909      /copy gaitrasrc/srcprotopr,TRTB85R
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
017100100909       // -?File TABEL00F?
017200100909     d k03tabel00    e ds                  extname(TABEL00F : *key)
017300100909     d                                     prefix(k_)
017400120611
017500120611       // -?File TNTBE01L?
017600120611     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
017700120611     d                                     prefix(k_)   inz
017800100909
017900100909       //--------------------------------------------------------------
018000100909       //?M A I N - L I N E                                            ?
018100100909       //--------------------------------------------------------------
018200100909
018300100909     c     *Entry        plist
018400100909     c                   parm                    kpjba
018500100910     c                   parm                    TRMK23ds1
018600100909
018700100909      /free
018800100909
018900100909       // -?Operazioni iniziali?
019000100909       exsr  sr_RoutInz;
019100100909
019200100909       // -?Gestione window?
019300100909       DoW  Not  $Fine;
019400100909         select;
019500100910           when  TRMK23ds1.Imk23rei = 'I';
019600100910             exsr  sr_F06W01;
019700100909           when  $Video = 'W1';
019800100910             exsr  sr_GesW01;
019900100909           other;
020000100909             $Fine = *on;
020100100909         endsl;
020200100909       EndDo;
020300100909
020400100909       // -?Operazioni finali?
020500100909       exsr  sr_RoutEnd;
020600100909
020700100909       //--------------------------------------------------------------
020800100909       //?Operazioni iniziali.                                         ?
020900100909       //--------------------------------------------------------------
021000100909       BEGSR  sr_RoutInz;
021100100909
021200100909         *inLR = *on;
021300100909
021400100909         // -?Impostazione campi "fissi"?
021500100909         k_TBLkut = 1;
021600100909         k_TBLcod = '01';
021700100909
021800100909         // -?Reperimento dati del lavoro?
021900100909         exsr  sr_DatiJob;
022000100909
022100100915         IF  TRMK23ds1.Imk23rei <> 'I';
022200100910
022300100910           // -?Verifica se visualizzare la finestra a 80 o 132 colonne?
022400100910           //  ?in base al formato utilizzato dal chiamante?
022500100910           //FmtDspF_DS4 = (TRMK23ds1.Imk23dsp = '4');
022600100910           // -?Reperimento formato attuale display?
022700100910           //  ?per verificare se visualizzare la finestra a 80 o 132 col.?
022800100910           RetrieveDisplayMode ( QdspMode : *omit : *omit );
022900100910           FmtDspF_DS4 = (QdspMode = '4');
023000100910           open  TRMK23D1;
023100100910
023200100910         ENDIF;
023300111128
023400111128         //?Data del giorno
023500111128         wOggi = %dec(%date());
023600100909
023700100909       ENDSR;
023800100909
023900100909       //--------------------------------------------------------------
024000100909       //?Reperimento Dati del job (Utente/Operativi).                 ?
024100100909       //--------------------------------------------------------------
024200100909       BEGSR  sr_DatiJob;
024300100909
024400100909         in(E) §AzUte;
024500100909         if NOT %error;
024600100909           in(E) §DatiUte;
024700100909         endif;
024800100909         if %error or RSut = *blanks;
024900100909           clear TIBS34ds;
025000100909           tibs34r(tibs34ds);
025100100909           in §AzUte;
025200100909           in §DatiUte;
025300100909         endif;
025400100909
025500100909       ENDSR;
025600100909
025700100909       //--------------------------------------------------------------
025800100909       //?Gestione videata W01.                                        ?
025900100909       //--------------------------------------------------------------
026000100909       BEGSR  sr_GesW01;
026100100909
026200100909         // -?Inizializzazione videata?
026300100909         if  $InzW01 = *on;
026400100909           exsr sr_InzW01;
026500100909           $InzW01 = *off;
026600100909         endif;
026700100909
026800100909         // -?Emissione videata?
026900100909         exfmt  MK23W01;
027000100909
027100100909         clear  ErrMessage;
027200100909         clear  ErrGenerico;
027300100909         clear  W1Dmsg;
027400100909
027500100909         Select;
027600100909
027700100909           // -?F12=Ritorno?
027800100909           When  dsp_aid = c_F12;
027900100909             exsr  sr_F12W01;
028000100909
028100100909           // -?Invio / F6=Conferma?
028200100909           Other;
028300100909             exsr  sr_CtrW01;
028400100909             select;
028500100909               when  ErrGenerico;
028600100909                 leavesr;
028700100910               when  dsp_aid = c_F06;
028800100909                 exsr  sr_F06W01;
028900100909             endsl;
029000100909
029100100909         EndSl;
029200100909
029300100909       ENDSR;
029400100909
029500100909       //--------------------------------------------------------------
029600100909       //?Inizializzazione videata W01.                                ?
029700100909       //--------------------------------------------------------------
029800100909       BEGSR  sr_InzW01;
029900100909
030000100909         clear  MK23W01;
030100100909
030200100909         // -?Impostazione attributi di visualizzazione?
030300100909         ProtectCMM = ( TRMK23ds1.Imk23cmaP = 'S' );
030400100909
030500100909         // -?Abilitazione tasti funzionali?
030600100909         F12Attivo = ( TRMK23ds1.Imk23f12 = *blank );
030700100909
030800100909         // -?Impostazione campi a video con i dati ricevuti?
030900100910         W1Ccmm = %editc( TRMK23ds1.Imk23cma : 'X' );
031000100910         W1Crst = TRMK23ds1.Imk23rst;
031100100910         W1mail = TRMK23ds1.Imk23emr;
031200100909
031300100909         // -?Decodifica commerciale ricevuto?
031400100909         exsr  sr_CtrW01;
031500100909         %subst(IndDspF : 50) = *off;
031600100909         clear  ErrMessage;
031700100909         clear  ErrGenerico;
031800100909         clear  W1Dmsg;
031900100909
032000100909       ENDSR;
032100100909
032200100909       //--------------------------------------------------------------
032300100909       //?Controllo videata W01.                                       ?
032400100909       //--------------------------------------------------------------
032500100909       BEGSR  sr_CtrW01;
032600100909
032700100909         %subst(IndDspF : 50) = *off;
032800100909
032900100909         // -?COMMERCIALE:?
033000100909         clear  W1Dcmm;
033100100909
033200100909         Select;
033300100909
033400100909           // -?Ricerca?
033500100909           When  %scan( '?' : W1Ccmm ) > *zero;
033600100909             ErrGenerico = *on;
033700100909             PosCurCMM   = *on;
033800100909             TB85Kfil = %editc( DUTpou : 'X' );
033900100909             clear  TB85Key;
034000100909             clear  TB85Des1;
034100100909             trtb85r (kpjba : tb85kfil : tb85key : tb85des1);
034200100909             W1Ccmm = TB85key;
034300100909             W1Dcmm = TB85des1;
034400100909             leavesr;
034500100909
034600100909           // -?Controllo inserimento?
034700100909           When  W1Ccmm = *blank  or  W1Ccmm = *zero;
034800100909             ErrMessage  = *on;
034900100909             ErrGenerico = *on;
035000100909             PosCurCMM   = *on;
035100100909             W1Dmsg = $Msg(01);
035200100909             leavesr;
035300100909
035400100909           // -?Controllo numericità?
035500100909           When  %check( c_Digits : W1Ccmm ) > *zero;
035600100909             ErrMessage  = *on;
035700100909             ErrGenerico = *on;
035800100909             PosCurCMM   = *on;
035900100909             W1Dmsg = $Msg(01);
036000100909             leavesr;
036100100909
036200100909         EndSl;
036300100909
036400100909         If  W1Ccmm <> SaveCMM;
036500100909
036600100909           clear ds01;
036700100909
036800100909           // -?Controllo esistenza in tab. "01"?
036900100909           k_TBLkey = W1Ccmm;
037000100909           chain  %kds(k03tabel00)  TABEL;
037100100909           if  NOT %found(TABEL00F)
037200100909               or  TBLflg <> *blank;
037300100909             ErrMessage  = *on;
037400100909             ErrGenerico = *on;
037500100909             PosCurCMM   = *on;
037600100909             W1Dmsg = $Msg(01);
037700100909             leavesr;
037800100909           endif;
037900100909
038000100909           ds01 = TBLuni;
038100110614
038200110614         // -?Controllo validità (no vari o clienti inattivi)?
038300110614           IF  §01par <> *blanks;
038400110614             ErrMessage  = *on;
038500110614             ErrGenerico = *on;
038600110614             PosCurCMM   = *on;
038700110614             W1Dmsg = $Msg(01);
038800110614             leavesr;
038900110614           ENDIF;
039000111128
039100111128         // -?Controllo data validità?
039200111128           IF  §01dtdec > %editc(wOggi:'X') or
039300111128               §01dtfin < %editc(wOggi:'X');
039400111128             ErrMessage  = *on;
039500111128             ErrGenerico = *on;
039600111128             PosCurCMM   = *on;
039700111128             W1Dmsg = $Msg(01);
039800111128             leavesr;
039900111128           ENDIF;
040000100909
040100100909           // -?Controllo abilitazione utente al commerciale?
040200100927           reset  TNTAA1ds;
040300100909           iTAA1cmm  = %int( W1Ccmm );
040400100909           kpjbu     = TNTAA1ds;
040500100909           TNTAA1C ( kpjba );
040600100909           TNTAA1ds  = kpjbu;
040700100909           if  OTAA1fabi = 'N';
040800100909             ErrMessage  = *on;
040900100909             ErrGenerico = *on;
041000100909             PosCurCMM   = *on;
041100100909             W1Dmsg = $Msg(01);
041200100909             leavesr;
041300100909           endif;
041400100909
041500100909           SaveCMM = W1Ccmm;
041600100909
041700100909         EndIf;
041800100909
041900100909         W1Dcmm = §01age;
042000100909
042100120611         //?Controllo se c'è la tabella HTM del commerciale?
042200120611         k_TBEcod = 'HTM';
042300120611         k_TBEke1 = W1Ccmm;
042400120611         k_TBEke2 = 'CC';
042500120611         chain  %kds( k05tntbe01 : 3 )  TNTBE01L;
042600120611         if  NOT %found(TNTBE01L)   or   TBEatb <> *blank;
042700120611           errMessage  = *on;
042800120611           errGenerico = *on;
042900120611           PosCurCmm   = *on;
043000120611           W1Dmsg = %replace( W1Ccmm : $Msg(04) :
043100120611                              %scan( '&CommLe' : $Msg(04) ) );
043200120611           leavesr;
043300120611         endif;
043400120611
043500100909
043600100910         // -?RESPONSABILE TRASPORTI:?
043700100910
043800100910         // -?Controllo inserimento?
043900100910         if  W1Crst = *blank;
044000100910           ErrMessage  = *on;
044100100910           ErrGenerico = *on;
044200100910           PosCurRST   = *on;
044300100910           W1Dmsg = $Msg(02);
044400100910           leavesr;
044500100910         endif;
044600100910
044700100909         // -?INDIRIZZO E-MAIL:?
044800100909
044900100909         // -?Controllo inserimento?
045000100909         if  W1mail = *blank;
045100100909           ErrMessage  = *on;
045200100909           ErrGenerico = *on;
045300100909           PosCurMAIL  = *on;
045400100910           W1Dmsg = $Msg(03);
045500100909           leavesr;
045600100909         endif;
045700100909
045800100909         // -?Controllo correttezza?
045900100909         clear dsEmail;
046000100909         §emlIndI = W1mail;
046100100909         xEmail ( dsEmail );
046200100909         if §emlErrO = *on;
046300100909           ErrMessage  = *on;
046400100909           ErrGenerico = *on;
046500100909           PosCurMAIL  = *on;
046600100909           W1Dmsg = §emlMsgO;
046700100909           leavesr;
046800100909         endif;
046900100909         W1mail = §emlIndO;
047000100909
047100100909       ENDSR;
047200100909
047300100909       //--------------------------------------------------------------
047400100909       //?Gestione tasto funzionale F6 da videata W01:                 ?
047500100909       //?F6=Conferma                                                  ?
047600100909       //--------------------------------------------------------------
047700100909       BEGSR  sr_F06W01;
047800100909
047900100909         // -?Reimpostazione parametri gestiti a video?
048000100910         if  TRMK23ds1.Imk23rei <> 'I';
048100100913           TRMK23ds1.Omk23rst  = W1Crst;                 //(1)?
048200100913           TRMK23ds1.Omk23emr  = W1mail;                 //(2)?
048300100913           TRMK23ds1.Omk23cma  = %int( W1Ccmm );         //(3)?
048400100910         endif;
048500100910
048600100910         // -?Uscita SENZA invio e-mail?
048700100910         if  TRMK23ds1.Imk23rei = 'R';
048800100910           TRMK23ds1.Omk23fxx = '06';
048900100910           $Fine = *on;
049000100910           leavesr;
049100100910         endif;
049200100909
049300100909         // -?Invio e-mail conferma appuntamento?
049400100910         reset  TRMK23ds;
049500100913         TRMK23ds.Imk23cma = TRMK23ds1.Omk23cma;         //(3)?
049600100910         TRMK23ds.Imk23cmm = TRMK23ds1.Imk23cmm;
049700100910         TRMK23ds.Imk23dta = TRMK23ds1.Imk23dta;
049800100910         TRMK23ds.Imk23ora = TRMK23ds1.Imk23ora;
049900100913         TRMK23ds.Imk23rst = TRMK23ds1.Omk23rst;         //(1)?
050000100913         TRMK23ds.Imk23emr = TRMK23ds1.Omk23emr;         //(2)?
050100100913
050200100909         kpjbu    = TRMK23ds;
050300100909         trmk23r ( kpjba );
050400100909         TRMK23ds = kpjbu;
050500100913
050600100913         TRMK23ds1.Omk23err = TRMK23ds.Omk23err;
050700100913         TRMK23ds1.Omk23msg = TRMK23ds.Omk23msg;
050800100909
050900100913         // -?Emissione messaggio di ERRORE ricevuto?
051000100909         if  TRMK23ds.Omk23err = *on;
051100100909           ErrMessage   = *on;
051200100909           ErrGenerico  = *on;
051300100909           W1Dmsg = TRMK23ds.Omk23msg;
051400100909           leavesr;
051500100909         endif;
051600100910
051700100910         // -?Restituzione tasto funzionale premuto?
051800100910         TRMK23ds1.Omk23fxx = '06';
051900100909
052000100909         // -?Chiusura del *pgm?
052100100909         $Fine = *on;
052200100909
052300100909       ENDSR;
052400100909
052500100909       //--------------------------------------------------------------
052600100909       //?Gestione tasto funzionale F12 da videata W01:                ?
052700100909       //?F12=Ritorno                                                  ?
052800100909       //--------------------------------------------------------------
052900100909       BEGSR  sr_F12W01;
053000100909
053100100909         // -?Restituzione tasto funzionale premuto?
053200100909         TRMK23ds1.Omk23fxx = '12';
053300100909
053400100909         // -?Chiusura del *pgm?
053500100909         $Fine = *on;
053600100909
053700100909       ENDSR;
053800100909
053900100909       //--------------------------------------------------------------
054000100909       //?Operazioni finali.                                           ?
054100100909       //--------------------------------------------------------------
054200100909       BEGSR  sr_RoutEnd;
054300100909
054400100909         // -?Uscita?
054500100909         return;
054600100909
054700100909       ENDSR;
054800100909
054900100909      /end-free
055000100909
055100100909       //--------------------------------------------------------------
055200100909       //?Messaggi di errore.                                          ?
055300100909       //--------------------------------------------------------------
055400100909
055500100909** -?$Msg?-------------------------------------------------------------------*
055600100909Commerciale errato o non in gestione all'utente                                 1
055700100910Nominativo del responsabile trasporti obbligatorio                              2
055800100910Indirizzo e-mail obbligatorio                                                   3
055900120611Dati del commerciale "&CommLe" non reperiti in tab. "HTM"                       4
