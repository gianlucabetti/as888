000100100222       //==============================================================
000200100222       //?Invio e-mail al cliente con O.R.M. allegato                  ?
000300100222       //==============================================================
000400100222
000500100222       //--------------------------------------------------------------
000600100222       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700100222       //--------------------------------------------------------------
000800100222
000900100222     /*END
001000100222
001100100222       //--------------------------------------------------------------
001200100222       //?Specifiche di controllo.                                     ?
001300100222       //--------------------------------------------------------------
001400100222
001500100222     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600100324     h*// dftactgrp(*no)
001700100222
001800100222       //--------------------------------------------------------------
001900100222       //?Dichiarazione file.
002000100222       //--------------------------------------------------------------
002100100222
002200100222       // -?Ordini Ritiro Merce?
002300100222     fFNORM01L  if   e           k disk
002400100222
002500100222       // -?Ordini Ritiro Merce - Estensione?
002600100222     fFNORE01L  if   e           k disk
002700100324
002800100324       // -?Tabelle?
002900100324     fTNTBE01L  if   e           k disk
003000100324     fTABEL00F  if   e           k disk
003100100223
003200100223       // -?Organigramma filiale/aziendale?
003300100223     fAZORG01L  if   e           k disk
003400100222
003500100222       // -?Spool da inviare via e-mail:?
003600100224       //   ·?Corpo del messaggio di posta elettronica?
003700100222     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
003800100224       //   ·?Allegato al messaggio di posta elettronica?
003900100222     fFIOR52P   o    e             printer usropn
004000100222
004100100222       //--------------------------------------------------------------
004200100222       //?Definizione costanti.                                        ?
004300100222       //--------------------------------------------------------------
004400100222
004500100324       // -?Codice utente / Lingua italiana  in tabelle su TABEL00F
004600100324     d c_Kut           c                   const(1)
004700100222
004800100222       // -?Dati di default?
004900100223     d c_Mittente      c                   const('ced')
005000100222     d c_at            c                   const('@bartolini.it')
005100100222     d c_Sede          c                   const('046')
005200100222
005300100223       // -?Comandi di override al PrtF "corpo e-mail"?
005400100223     d c_CmdOvrPrtF_1  c                   const('OVRPRTF +
005500100222     d                                           file(PRTEMAIL) +
005600100222     d                                           pagesize(66 132) +
005700100222     d                                           lpi(6) cpi(10) +
005800100222     d                                           ovrscope(*actgrpdfn) +
005900100222     d                                           ')
006000100223     d c_CmdDltOvr_1   c                   const('DLTOVR +
006100100222     d                                            file(PRTEMAIL) +
006200100222     d                                            lvl(*actgrpdfn)')
006300100223
006400100223       // -?Comandi di override ai PrtF "allegato e-mail"?
006500100223     d c_CmdOvrPrtF_2  c                   const('OVRPRTF +
006600100223     d                                           file(FIOR52P) +
006700100222     d                                           ovrscope(*actgrpdfn) +
006800100222     d                                           ')
006900100223     d c_CmdDltOvr_2   c                   const('DLTOVR +
007000100223     d                                            file(FIOR52P) +
007100100222     d                                            lvl(*actgrpdfn)')
007200100222
007300100222       //--------------------------------------------------------------
007400100222       //?Definizione schiere.                                         ?
007500100222       //--------------------------------------------------------------
007600100222
007700100222
007800100222       //--------------------------------------------------------------
007900100222       //?Definizione aree dati.                                       ?
008000100222       //--------------------------------------------------------------
008100100222
008200100222       // -?Dati utente?
008300100222     d §AzUte        e ds                  extname(AZUTE00F)
008400100222     d                                     dtaara
008500100222     d §DatiUte      e ds                  extname(dDatiUte)
008600100222     d                                     dtaara
008700100222
008800100222       //--------------------------------------------------------------
008900100222       //?Definizione strutture dati.                                  ?
009000100222       //--------------------------------------------------------------
009100100222
009200100222       // -?Parametri ricevuti?
009300100222     d KPJBA         e ds
009400100224     d FIOR52ds      e ds
009500100222
009600100222       // -?Tab. "MRA" = Bart-Mailing x FIOR52R?
009700100222     d dMRAOR52      e ds                  inz
009800110504
009900110504       // -?Tabella VPO/BRT = Variabili Programmi Operativi x BRT?
010000110504     d dVPObrt       e ds                  inz
010100100317
010200100317       // -?Tipo servizio?
010300100317     d dOREgen       e ds                  inz  qualified
010400100317     d ds5E          e ds                  inz
010500100222
010600100222       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
010700100222     d TRTCM1ds      e ds                  inz
010800100222       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
010900100222     d   §CM1mitt    e                     inz('ced')
011000100222       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
011100100222     d   §CM1dst     e                     inz('stefano.merighi@bartolini.it')
011200100222       //  ?·§CM1tips = Tipo lettera via e-mail:?
011300100222       //   ?"LET" = testo allegato in corpo con logo?
011400100222       //           ?(richiede righe libere iniziali per il logo)?
011500100222       //   ?"COR" = testo integrato senza logo?
011600100222       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
011700100222       //   ?"SP1" = spool allegato (in formato PDF)?
011800100222     d   §CM1tips    e                     inz('OR1')
011900100222       //  ?·§CM1po   = Filiale?
012000100222     d   §CM1po      e                     inz('046')
012100100222       //  ?·§CM1var  = Oggetto e-mail?
012200100315     d*//§CM1var     e                     inz('*OBJM*+
012300100315     d*//                                  Ritiro per conto CEVA')
012400100315     d   §CM1var     e                     inz('*OBJM*')
012500100222       //  ?·§CM1sts  = Stato?
012600100222     d   §CM1sts     e                     inz('H')
012700100222       //  ?·§CM1sts  = Id processo?
012800100222     d   §CM1idp     e                     inz('3')
012900100222       //  ?·§CM1att  = Numero attach corrente?
013000100222     d   §CM1att     e                     inz('01')
013100100222       //  ?·§CM1totatt  = Numero attach totali?
013200100222     d   §CM1totatt  e                     inz('02')
013300100223
013400100223       // -?Codice a Barre dell'O.R.M.?
013500100223     d BARcodeORMds    ds            14    inz
013600100223     d  ORMpoe                             inz
013700100223     d  ORMnsr                             inz
013800100223     d  ORMnor                             inz
013900100223     d  ORMnrv                             inz
014000100222
014100100222       //--------------------------------------------------------------
014200100222       //?Definizione variabili.                                       ?
014300100222       //--------------------------------------------------------------
014400100222
014500100223       // -?Indici di schiera?
014600100223     d xx              s              3  0 inz
014700100223
014800100223       // -?Campi di stampa?
014900100223     d O_txt           s            132    inz
015000100226     d Save_IndEmail   s                   like(OREdati) inz
015100100226     d Save_Note       s                   like(OREdati) inz
015200100223
015300100223       // -?Campi di comodo?
015400100225     d w_OREtrc        s                   like(OREtrc)  inz
015500100223     d wDate_EUR       s               d   datfmt(*eur)  inz
015600100223     d wDate           s              8  0 inz
015700100222
015800100222       //--------------------------------------------------------------
015900100222       //?Definizione prototipi procedure.                             ?
016000100222       //--------------------------------------------------------------
016100100222
016200100222       // -?Reperimento dati utente?
016300110504     d TIBS34ds      e ds                  inz
016400100222      /copy gaitrasrc/srcProtoPR,TIBS34R
016500100223
016600100223       // -?Gestione numeratori in GRU?
016700100223     d TRUL33ds      e ds                  inz
016800100223     d   I33tla      e                     inz('L')
016900100223     d   I33ope      e                     inz(*zero)
017000100223     d   I33cnu      e                     inz(024)
017100100223     d   I33num      e                     inz(1)
017200100223      /copy gaitrasrc/srcProtoPR,TRUL33R
017300100223
017400100223       // -?Controllo/Decodifica cliente?
017500100223     d TIBS69ds      e ds                  qualified
017600100223     d                                     inz
017700100223     d ds_CNACO      e ds                  extname(CNACO00F) qualified
017800100223     d                                     inz
017900100223     d ds_CNIND      e ds                  extname(CNIND00F) qualified
018000100223     d                                     inz
018100100223     d ds_CNCLP      e ds                  extname(CNCLP00F) qualified
018200100223     d                                     inz
018300100223     d ds_FNCLS      e ds                  extname(FNCLS00F) qualified
018400100223     d                                     inz
018500100223      /copy gaitrasrc/srcProtoPR,TIBS69R
018600100222
018700100223       // -?Parametri API QCAPCMD (Process Commands)?
018800100223     d Qcmd            s           2048    inz  varying
018900100223      /copy qSysInc/qRpgleSrc,QCAPCMD
019000100223      /copy gaitrasrc/srcProtoPR,QCAPCMD
019100100223
019200100223       // -?Parametri gestione errori API.?
019300100223      /copy qsysinc/qrpglesrc,QUSEC
019400100222
019500100222       //--------------------------------------------------------------
019600100222       //?Definizione key-list.                                        ?
019700100222       //--------------------------------------------------------------
019800100222
019900100222       // -?File FNORM01L?
020000100222     d k04fnorm01    e ds                  extname(FNORM01L : *key)
020100100222     d                                     prefix(k_)   inz
020200100222
020300100222       // -?File FNORE01L?
020400100222     d k05fnore01    e ds                  extname(FNORE01L : *key)
020500100222     d                                     prefix(k_)   inz
020600100324
020700100324       // -?File TNTBE01L?
020800100324     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
020900100324     d                                     prefix(k_)   inz
021000100324
021100100324       // -?File TABEL00F?
021200100324     d k03tabel00    e ds                  extname(TABEL00F : *key)
021300100324     d                                     prefix(k_)   inz
021400100222
021500100222       //--------------------------------------------------------------
021600100222       //?M A I N - L I N E                                            ?
021700100222       //--------------------------------------------------------------
021800100222
021900100222     c     *Entry        plist
022000100222     c                   parm                    KPJBA
022100100224     c                   parm                    FIOR52ds
022200100222
022300100222      /free
022400100222
022500100222       // -?Operazioni iniziali?
022600100222       exsr  sr_RoutInz;
022700100222
022800100223       // -?Stampa (ed invio) e-mail?
022900100222       exsr  sr_PrtEmail;
023000100222
023100100222       // -?Operazioni finali?
023200100222       exsr  sr_RoutEnd;
023300100222
023400100222       //--------------------------------------------------------------
023500100222       //?Operazioni iniziali.                                         ?
023600100222       //--------------------------------------------------------------
023700100222       BEGSR  sr_RoutInz;
023800100222
023900100222         *inLR = *on;
024000100223
024100100223         // -?Impostazione parametri ricevuti?
024200100224         if  %parms() > 1;
024300100225           O52err = *off;
024400100224           clear  O52msg;
024500100224         endif;
024600100226
024700100226         // -?Pulizia inziale campi di comodo?
024800100226         clear  Save_IndEmail;
024900100222
025000100222         // -?Reperimento dati del lavoro?
025100100222         exsr  sr_DatiJob;
025200110504
025300110504         // -?Reperimento tab. "VPO"/"BRT"?
025400110504         exsr  sr_TabVPO;
025500100224
025600100226         // -?*-=»>  Solo TEST in stampa  <«=-*?
025700100224         if  %parms() = 1;
025800100224           open  PrtEMAIL;
025900100224           open  FIOR52P;
026000100224           exsr  sr_TEST;
026100100224           leavesr;
026200100224         endif;
026300100222
026400100223         // -?Reperimento tab. "MRA"?
026500100224         exsr  sr_TabMRA;
026600100223
026700100223         // -?Reperimento dati O.R.M.?
026800100224         exsr  sr_RepFNORM;
026900100223
027000100223         // -?Reperimento indirizzo email del destinatario da O.R.M.?
027100100224         exsr  sr_RepFNORE;
027200100224
027300100224         // -?Reperimento ultimi dati (decodifiche)?
027400100224         exsr  sr_AltriDati;
027500100223
027600100223         // -?Reperimento nuovo progressivo da numeratore "024"?
027700100224         exsr  sr_Num024;
027800100223
027900100223         // -?Override ai file di stampa (per impostarvi i dati per?
028000100223         //  ?l'invio via e-mail)?
028100100224         exsr sr_Override;
028200100223
028300100223         // -?Apertura file di stampa?
028400100223         open  PrtEMAIL;
028500100223         open  FIOR52P;
028600100222
028700100222       ENDSR;
028800100222
028900100222       //--------------------------------------------------------------
029000100222       //?Reperimento Dati del job (Utente/Operativi).                 ?
029100100222       //--------------------------------------------------------------
029200100222       BEGSR  sr_DatiJob;
029300100222
029400100222         in(E) §AzUte;
029500100222         if NOT %error;
029600100222           in(E) §DatiUte;
029700100222         endif;
029800100222         if %error or RSut = *blanks;
029900100222           clear TIBS34ds;
030000100222           tibs34r(tibs34ds);
030100100222           in §AzUte;
030200100222           in §DatiUte;
030300100222         endif;
030400100222
030500100222       ENDSR;
030600110504
030700110504       //--------------------------------------------------------------
030800110504       //?Reperimento dati da tab. "VPO"/"BRT":                        ?
030900110504       //?data di decorrenza stampa nuovo logo BRT.                    ?
031000110504       //--------------------------------------------------------------
031100110504       BEGSR  sr_TabVPO;
031200110504
031300110504         clear  dVPObrt;
031400110504
031500110504         k_TBEcod = 'VPO';
031600110504         k_TBEke1 = 'BRT';
031700110504         clear  k_TBEke2;
031800110504
031900110504         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
032000110504
032100110504         if  %found(TNTBE01L)   and   TBEatb = *blank;
032200110504           dVPObrt = TBEuni;
032300110504         endif;
032400110504
032500110504       ENDSR;
032600100224
032700100224       //--------------------------------------------------------------
032800100224       //?Reperimento dati da tab. "MRA".                              ?
032900100224       //--------------------------------------------------------------
033000100224       BEGSR  sr_TabMRA;
033100100224
033200100224         clear  dMRAor52;
033300100324
033400100324         k_TBEcod = 'MRA';
033500100324         k_TBEke1 = 'FIOR52R';
033600100324         k_TBEke2 = 'CEVA';
033700100324
033800100324         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
033900100324
034000100324         if  %found(TNTBE01L)   and   TBEatb = *blank;
034100100324           dMRAor52 = TBEuni;
034200100324         else;
034300100324           O52err = *on;
034400100324           O52msg = 'Parametri "' + %trimr(k_TBEke1) +
034500100324                    '" non reperiti in tab. "MRA"';
034600100324           exsr  sr_RoutEnd;
034700100324         endif;
034800100224
034900100224       ENDSR;
035000100224
035100100224       //--------------------------------------------------------------
035200100224       //?Reperimento dati O.R.M. (file FNORM00F)                      ?
035300100224       //--------------------------------------------------------------
035400100224       BEGSR  sr_RepFNORM;
035500100224
035600100224         //clear  k04fnorm01;          (già così)
035700100224         k_ORMpoe = I52poe;
035800100224         k_ORMnsr = I52nsr;
035900100224         k_ORMnor = I52nor;
036000100224         k_ORMnrv = I52nrv;
036100100224         chain  %kds( k04fnorm01 )  FNORM000;
036200100224         if  NOT %found(FNORM01L)   or   ORMatb <> *blank;
036300100224           O52err = *on;
036400100224           O52msg = 'Non trovato O.R.M. ' + %editc( I52poe : 'X' ) +
036500100224                    '-' + %trim( %editc( I52nsr : '3' ) ) +
036600100224                    '-' + %trim( %editc( I52nor : '3' ) ) +
036700100224                    '-' + %trim( %editc( I52nrv : '3' ) );
036800100224           exsr  sr_RoutEnd;
036900100224         endif;
037000100224
037100100224       ENDSR;
037200100224
037300100224       //--------------------------------------------------------------
037400100224       //?Reperimento dati aggiuntivi O.R.M. (file FNORE00F)           ?
037500100224       //--------------------------------------------------------------
037600100224       BEGSR  sr_RepFNORE;
037700100317
037800100317         // -?Reperimento estensione ORM?
037900100317         w_OREtrc = 'G ';
038000100317         exsr sr_ChainFNORE;
038100100317         dOREgen = OREdati;
038200100317         clear  ds5E;
038300100317         if  dOREgen.§OREtsp <> *blank;
038400100324           k_TBLkut = c_Kut;
038500100324           k_TBLcod = '5E';
038600100324           k_TBLkey = dOREgen.§OREtsp;
038700100324           chain  %kds( k03tabel00 )  TABEL;
038800100324           if  %found(TABEL00F)   and   TBLflg = *blank;
038900100324             ds5E = TBLuni;
039000100324           endif;
039100100317         endif;
039200100224
039300100225         // -?Reperimento indirizzo e-mail del destinatario?
039400100225         w_OREtrc = 'M ';
039500100225         exsr sr_ChainFNORE;
039600100226         if  %found(FNORE01L);
039700100226          Save_IndEmail = OREdati;
039800100226         else;
039900100224           O52err = *on;
040000100224           O52msg = 'Non trovato indirizzo e-mail del destinatario da ORM '+
040100100224                    %editc( I52poe : 'X' ) + '-' +
040200100224                    %trim( %editc( I52nsr : '3' ) ) + '-' +
040300100224                    %trim( %editc( I52nor : '3' ) ) + '-' +
040400100224                    %trim( %editc( I52nrv : '3' ) );
040500100226           exsr  sr_RoutEnd;
040600100224         endif;
040700100225
040800100225         // -?Reperimento note ritiro?
040900100225         w_OREtrc = 'N ';
041000100225         exsr sr_ChainFNORE;
041100100226         Save_Note = OREdati;
041200100224
041300100224       ENDSR;
041400100225
041500100225       //--------------------------------------------------------------
041600100225       //?Aggancio record specifico in dati aggiuntivi O.R.M.          ?
041700100225       //--------------------------------------------------------------
041800100225       BEGSR  sr_ChainFNORE;
041900100225
042000100225         clear  OREdati;
042100100225
042200100225         clear  k05fnore01;
042300100225         k_OREpoe = I52poe;
042400100225         k_OREnsr = I52nsr;
042500100225         k_OREnor = I52nor;
042600100225         k_OREnrv = I52nrv;
042700100225         k_OREtrc = w_OREtrc;
042800100225
042900100225         chain  %kds( k05fnore01 )  FNORE000;
043000100225
043100100225       ENDSR;
043200100224
043300100224       //--------------------------------------------------------------
043400100224       //?Reperimento altri dati (decodifiche).                        ?
043500100224       //--------------------------------------------------------------
043600100224       BEGSR  sr_AltriDati;
043700100224
043800100224         // -?Decodifica cliente pagante?
043900100224         clear  TIBS69ds;
044000100224         TIBS69ds.I69sif = knsif;
044100100224         TIBS69ds.I69kcc = DUTkci;
044200100224         TIBS69ds.I69kac = ORMksc;
044300100224         tibs69r( TIBS69ds :
044400100224                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
044500100224
044600100224         // -?Dati anagrafici della filiale di ritiro?
044700100224         chain  (ORMpor)  AZORG;
044800100224         if  NOT %found(AZORG01L)   or   ORGfva <> *blank;
044900100224           clear  ORGloc;
045000100224           clear  ORGind;
045100100224           clear  ORGpro;
045200100224           clear  ORGtel;
045300100224         endif;
045400100224
045500100224       ENDSR;
045600100224
045700100224       //--------------------------------------------------------------
045800100224       //?Reperimento progressivo contatore "024"                      ?
045900100224       //--------------------------------------------------------------
046000100224       BEGSR  sr_Num024;
046100100224
046200100224         reset  TRUL33ds;
046300100224
046400100224         kpjbu  = TRUL33ds;
046500100224         trul33r ( KPJBA );
046600100224         TRUL33ds = kpjbu;
046700100224
046800100224         if  O33err <> *zero;
046900100224           O52err = *on;
047000100224           O52msg = 'Fallito reperimento numeratore "' +
047100100224                    %editc(I33cnu : 'X') + '"';
047200100224           exsr  sr_RoutEnd;
047300100224         endif;
047400100224
047500100224       ENDSR;
047600100224
047700100224       //--------------------------------------------------------------
047800100224       //?Impostazione dati per stampa di prova.                       ?
047900100224       //--------------------------------------------------------------
048000100224       BEGSR  sr_TEST;
048100100224
048200100224         wDate_EUR = %date();
048300110504
048400110504         if  %dec( wDate_EUR : *iso ) > §VPOBRTdta;
048500110504           §Logo = 'LOGBBRT1';
048600110525           Pfib  = 'Firma Incaricato Bartolini BRT';
048700110504         else;
048800110504           §Logo = 'LOGOBAR1';
048900110504           Pfib  = 'Firma Incaricato Bartolini';
049000110504         endif;
049100110504
049200100224         ORMdar = %dec( wDate_EUR : *iso );
049300100224
049400100224         ORMorr = 6789;
049500100224
049600100224         ds_CNACO.ACOrag = *all'W';
049700100224
049800100224         ORGloc = *all'W';
049900100224         ORGind = *all'X';
050000100224         ORGpro = *all'W';
050100100224         ORGtel = '1234/67890123';
050200100224
050300100224         ORMpoe = 321;
050400100224         ORMnsr = 98;
050500100224         ORMnor = 1234567;
050600100224         ORMnrv = 89;
050700100224
050800100224         ORMpkg = 123456,7;
050900100224         ORMncl = 54321;
051000100224         ORMvlm = 98,765;
051100100224
051200100224         ORMrer = *all'W';
051300100224         ORMrsr = *all'W';
051400100224         ORMinr = *all'X';
051500100224         ORMcar = '12345____';
051600100224         ORMlor = *all'W';
051700100224         ORMprr = *all'W';
051800100224         ORMter = '1234/67890123456';
051900100224
052000100224         ORMrsc = *all'W';
052100100224         ORMinc = *all'X';
052200100224         ORMcac = '12345____';
052300100224         ORMloc = *all'W';
052400100224         ORMprc = *all'W';
052500100224
052600100224         ORMrfa = *all'W';
052700100317
052800100317         dOREgen.§OREtsp = 'Q';
052900100317         §5Edes = *all'W';
053000100224
053100100224         OREdati = *all'W';
053200100226         Save_Note = OREdati;
053300100224
053400100224         ORMrso = *all'W';
053500100224         ORMino = *all'X';
053600100224         ORMcao = '12345____';
053700100224         ORMloo = *all'W';
053800100224         ORMpro = *all'W';
053900100224
054000100224       ENDSR;
054100100223
054200100223       //--------------------------------------------------------------
054300100223       //?Preparazione ed invio e-mail.                                ?
054400100223       //--------------------------------------------------------------
054500100223       BEGSR  sr_PrtEmail;
054600100223
054700100223         wDate_EUR = %date( ORMdar : *iso );
054800100223         wDate = %dec( wDate_EUR );
054900100223
055000100223         // -?Stampa "corpo e-mail"?
055100100223         For  xx = 1  To  20;
055200100223           O_txt = '§§Tag' + %editc( xx : 'X') + '§§#';
055300100223           select;
055400100223             when  xx = 1;
055500100223               O_txt = %trimr(O_txt) + %trim( ORMrsr );
055600100223             when  xx = 2;
055700100223               O_txt = %trimr(O_txt) + %trim( ORMrso );
055800100223             when  xx = 3;
055900100223               O_txt = %trimr(O_txt) + %trim( ORMrsr );
056000100223             when  xx = 4;
056100100223               O_txt = %trimr(O_txt) + %trim( ORMinr );
056200100223             when  xx = 5;
056300100223               O_txt = %trimr(O_txt) + %trim( ORMcar );
056400100223             when  xx = 6;
056500100223               O_txt = %trimr(O_txt) + %trim( ORMlor );
056600100223             when  xx = 7;
056700100223               O_txt = %trimr(O_txt) + %trim( ORMprr );
056800100223             when  xx = 8;
056900100223               O_txt = %trimr(O_txt) + %trim( ORMrer );
057000100223             when  xx = 9;
057100100223               O_txt = %trimr(O_txt) + %triml( %editc( wDate : 'Y' ) );
057200100223             when  xx = 10;
057300100223               O_txt = %trimr(O_txt) + %triml( %editw( ORMorr : '0 :  ' ) );
057400100223             when  xx = 11;
057500100223               O_txt = %trimr(O_txt) + %triml( ORMrso );
057600100223             when  xx = 12;
057700100223               O_txt = %trimr(O_txt) + %triml( ds_CNACO.ACOrag );
057800100223             when  xx = 13;
057900100223               O_txt = %trimr(O_txt) + %triml( ORGloc );
058000100223             when  xx = 14;
058100100223               O_txt = %trimr(O_txt) + %triml( ORGind );
058200100223             when  xx = 15;
058300100223               O_txt = %trimr(O_txt) + %triml( ORGpro );
058400100223             when  xx = 16;
058500100223               O_txt = %trimr(O_txt) + %triml( ORGtel );
058600100223             when  xx = 17;
058700100223               O_txt = %trimr(O_txt) + %editc( ORMpoe : 'X' );
058800100223             when  xx = 18;
058900100223               O_txt = %trimr(O_txt) + %triml( %editc( ORMnsr : '3' ) );
059000100223             when  xx = 19;
059100100223               O_txt = %trimr(O_txt) + %triml( %editc( ORMnor : '3' ) );
059200100223             when  xx = 20;
059300100223               O_txt = %trimr(O_txt) + %triml( %editc( ORMnrv : '3' ) );
059400100223           endsl;
059500100223           O_txt = %trimr(O_txt) + '}{';
059600100223           except  PRTparm;
059700100223         EndFor;
059800100223
059900110504         // -?Impostazione variabili "allegato e-mail"?
060000110504         // -?Dati dell'O.R.M.?
060100100223         pBCO = BarCodeORMds;
060200100223         pDAR = wDate;
060300100223         pPKG = ORMpkg;
060400100223         pNCL = ORMncl;
060500100223         pVLM = ORMvlm;
060600100223         // -?Dati del mittente?
060700100223         pRER = ORMrer;
060800100223         pRSR = ORMrsr;
060900100223         pINR = ORMinr;
061000100223         pCAR = ORMcar;
061100100223         pLOR = ORMlor;
061200100223         pPRR = '(' + ORMprr + ')';
061300100223         pTER = ORMter;
061400100223         // -?Dati del destinatario?
061500100223         pRSC = ORMrsc;
061600100223         pINC = ORMinc;
061700100223         pCAC = ORMcac;
061800100223         pLOC = ORMloc;
061900100223         pPRC = '(' + ORMprc + ')';
062000100223         // -?Riferimento mittente?
062100100223         pRFA = ORMrfa;
062200100317         // -?Tipo servizio?
062300100317         if  dOREgen.§OREtsp <> *blank;
062400100317           pkTSP = 'Tipo servizio';
062500100317           pTSP = dOREgen.§OREtsp + ' - ' + §5Edes;
062600100317         endif;
062700100223         // -?Note ritiro?
062800100226         pNOTE = Save_Note;
062900100223         // -?Dati del commissionante?
063000100223         pRSO = ORMrso;
063100100223         pINO = ORMino;
063200100223         pCAO = ORMcao;
063300100223         pLOO = ORMloo;
063400100223         pPRO = '(' + ORMpro + ')';
063500110504         // -?Dati Bartolini/BRT (logo compreso)?
063600110504         wDate_EUR = %date();
063700110504         if  %dec( wDate_EUR : *iso ) > §VPOBRTdta;
063800110504           §Logo = 'LOGBBRT1';
063900110525           Pfib  = 'Firma Incaricato Bartolini BRT';
064000110504         else;
064100110504           §Logo = 'LOGOBAR1';
064200110504           Pfib  = 'Firma Incaricato Bartolini';
064300110504         endif;
064400100223
064500110504         // -?Stampa "allegato e-mail"?
064600100226         write  OR52corpo;
064700100223         write  OR52box;
064800100223         write  OR52piede;
064900100223
065000100223       ENDSR;
065100100222
065200100222       //--------------------------------------------------------------
065300100222       //?Override al file di stampa per impostarvi i dati per         ?
065400100223       //?  l'invio via e-mail.                                        ?
065500100222       //--------------------------------------------------------------
065600100222       BEGSR sr_Override;
065700100222
065800100222         reset TRTCM1ds;
065900100308         §CM1mitt = §MRAmitt;
066000100226         §CM1dst  = %trim(Save_IndEmail);
066100100223         §CM1po   = %editc( DUTpou : 'X' );
066200100329         §CM1var  = %trimr(§CM1var) + %trim(§MRAobjm) + ' - ORM ' +
066300100329                    %editc( ORMpoe : 'X' ) + '-' +
066400100329                    %triml( %editc( ORMnsr : '3' ) ) + '-' +
066500100329                    %triml( %editc( ORMnor : '3' ) ) + '-' +
066600100329                    %triml( %editc( ORMnrv : '3' ) );
066700100223         §CM1sts  = §MRAsts;
066800100223         §CM1idp  = §MRAidpro;
066900100223         §CM1prog = %subst( %editc( O33nri : 'X' ) :
067000100223                            %size(O33nri) - %size(§CM1prog) + 1 );
067100100222
067200100223         // -?Override al "corpo e-mail"?
067300100223         if  §MRAregCor <> *blank;
067400100223           §CM1tips = §MRAregCor;
067500100223           §CM1att  = '02';
067600100226           Qcmd = c_CmdOvrPrtF_1
067700100223                + ' outq(' + %trim(§MRAoutqi) + ')'
067800100222                + ' usrdfndta(''' + TRTCM1ds + ''')';
067900100223         else;
068000100226           Qcmd = c_CmdOvrPrtF_1;
068100100222         endif;
068200100222
068300100223         exsr  sr_ExecCmd;
068400100223
068500100223         if  Qusei <> *blank;
068600100223           O52err = *on;
068700100223           O52msg = 'Fallita override al file di stampa per +
068800100223                     "corpo e-mail"';
068900100223           exsr  sr_RoutEnd;
069000100223         endif;
069100100223
069200100223         // -?Override all'"allegato e-mail"?
069300100223         if  §MRAregAll <> *blank;
069400100223           §CM1tips = §MRAregAll;
069500100223           §CM1att  = '01';
069600100226           Qcmd = c_CmdOvrPrtF_2
069700100223                + ' outq(' + %trim(§MRAoutqi) + ')'
069800100223                + ' usrdfndta(''' + TRTCM1ds + ''')';
069900100223         else;
070000100226           Qcmd = c_CmdOvrPrtF_2;
070100100223         endif;
070200100223
070300100223         exsr  sr_ExecCmd;
070400100223
070500100223         if  Qusei <> *blank;
070600100223           O52err = *on;
070700100223           O52msg = 'Fallita override al file di stampa per +
070800100223                     "allegato e-mail"';
070900100223           exsr  sr_RoutEnd;
071000100223         endif;
071100100222
071200100222       ENDSR;
071300100222
071400100222       //--------------------------------------------------------------
071500100223       //?Chiusura file stampa & Cancellazione override.               ?
071600100222       //--------------------------------------------------------------
071700100222       BEGSR sr_DeleteOvr;
071800100222
071900100223         // -?Cancellazine override al "corpo e-mail"?
072000100223         if %open(PrtEMAIL);
072100100223           close PRTEMAIL;
072200100223           Qcmd = c_CmdDltOvr_1;
072300100223           exsr  sr_ExecCmd;
072400100223         endif;
072500100223
072600100223         // -?Cancellazione override all'"allegato e-mail"?
072700100222         if  %open(FIOR52P);
072800100222           close FIOR52P;
072900100223           Qcmd = c_CmdDltOvr_2;
073000100223           exsr  sr_ExecCmd;
073100100222         endif;
073200100222
073300100222       ENDSR;
073400100223
073500100223       //--------------------------------------------------------------
073600100223       //?Esecuzione del comando (già impostato).                      ?
073700100223       //--------------------------------------------------------------
073800100223       BEGSR  sr_ExecCmd;
073900100223
074000100223         clear Qcap0100;
074100100223         Qcabcsdh = *off;
074200100223         Qcapa    = *off;
074300100223         Qcacmdss = *off;
074400100223         Qcaerved = *allX'00';
074500100223
074600100223         clear Qusec;
074700100223         Qusbprv  = %size(Qusec);
074800100223
074900100223         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
075000100223                           %size(Qcap0100) : 'CPOP0100' : *omit :
075100100223                           0 : 0 : Qusec);
075200100223
075300100223         //if  Qusei <> *blank;
075400100223         //  ...;
075500100223         //endif;
075600100223
075700100223       ENDSR;
075800100222
075900100222       //--------------------------------------------------------------
076000100222       //?Operazioni finali.                                           ?
076100100222       //--------------------------------------------------------------
076200100222       BEGSR  sr_RoutEnd;
076300100222
076400100224         if  %parms() > 1;
076500100224
076600100224           // -?Chiusura file stampa & Cancellazione override?
076700100224           exsr sr_DeleteOvr;
076800100224
076900100224           // -?Chiusura applicazioni prima richiamate?
077000100224           clear  TIBS69ds;
077100100224           TIBS69ds.I69tla = 'C';
077200100224           tibs69r( tibs69ds :
077300100224                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
077400100224
077500100224         endif;
077600100223
077700100223         // -?Uscita?
077800100223         return;
077900100222
078000100222       ENDSR;
078100100222
078200100222      /end-free
078300100223
078400100223       //--------------------------------------------------------------
078500100223       //?Specifiche di stampa.                                        ?
078600100223       //--------------------------------------------------------------
078700100223
078800100223     oPrtEMAIL  e            PRTparm     1
078900100223     o                       O_txt
