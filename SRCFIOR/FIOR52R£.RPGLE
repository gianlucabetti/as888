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
009800100317
009900100317       // -?Tipo servizio?
010000100317     d dOREgen       e ds                  inz  qualified
010100100317     d ds5E          e ds                  inz
010200100222
010300100222       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
010400100222     d TRTCM1ds      e ds                  inz
010500100222       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
010600100222     d   §CM1mitt    e                     inz('ced')
010700100222       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
010800100222     d   §CM1dst     e                     inz('stefano.merighi@bartolini.it')
010900100222       //  ?·§CM1tips = Tipo lettera via e-mail:?
011000100222       //   ?"LET" = testo allegato in corpo con logo?
011100100222       //           ?(richiede righe libere iniziali per il logo)?
011200100222       //   ?"COR" = testo integrato senza logo?
011300100222       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
011400100222       //   ?"SP1" = spool allegato (in formato PDF)?
011500100222     d   §CM1tips    e                     inz('OR1')
011600100222       //  ?·§CM1po   = Filiale?
011700100222     d   §CM1po      e                     inz('046')
011800100222       //  ?·§CM1var  = Oggetto e-mail?
011900100315     d*//§CM1var     e                     inz('*OBJM*+
012000100315     d*//                                  Ritiro per conto CEVA')
012100100315     d   §CM1var     e                     inz('*OBJM*')
012200100222       //  ?·§CM1sts  = Stato?
012300100222     d   §CM1sts     e                     inz('H')
012400100222       //  ?·§CM1sts  = Id processo?
012500100222     d   §CM1idp     e                     inz('3')
012600100222       //  ?·§CM1att  = Numero attach corrente?
012700100222     d   §CM1att     e                     inz('01')
012800100222       //  ?·§CM1totatt  = Numero attach totali?
012900100222     d   §CM1totatt  e                     inz('02')
013000100223
013100100223       // -?Codice a Barre dell'O.R.M.?
013200100223     d BARcodeORMds    ds            14    inz
013300100223     d  ORMpoe                             inz
013400100223     d  ORMnsr                             inz
013500100223     d  ORMnor                             inz
013600100223     d  ORMnrv                             inz
013700100222
013800100222       //--------------------------------------------------------------
013900100222       //?Definizione variabili.                                       ?
014000100222       //--------------------------------------------------------------
014100100222
014200100223       // -?Indici di schiera?
014300100223     d xx              s              3  0 inz
014400100223
014500100223       // -?Campi di stampa?
014600100223     d O_txt           s            132    inz
014700100226     d Save_IndEmail   s                   like(OREdati) inz
014800100226     d Save_Note       s                   like(OREdati) inz
014900100223
015000100223       // -?Campi di comodo?
015100100225     d w_OREtrc        s                   like(OREtrc)  inz
015200100223     d wDate_EUR       s               d   datfmt(*eur)  inz
015300100223     d wDate           s              8  0 inz
015400100222
015500100222       //--------------------------------------------------------------
015600100222       //?Definizione prototipi procedure.                             ?
015700100222       //--------------------------------------------------------------
015800100222
015900100222       // -?Reperimento dati utente?
016000130627     d TIBS34ds      e ds                  inz
016100100222      /copy gaitrasrc/srcProtoPR,TIBS34R
016200100223
016300100223       // -?Gestione numeratori in GRU?
016400100223     d TRUL33ds      e ds                  inz
016500100223     d   I33tla      e                     inz('L')
016600100223     d   I33ope      e                     inz(*zero)
016700100223     d   I33cnu      e                     inz(024)
016800100223     d   I33num      e                     inz(1)
016900100223      /copy gaitrasrc/srcProtoPR,TRUL33R
017000100223
017100100223       // -?Controllo/Decodifica cliente?
017200100223     d TIBS69ds      e ds                  qualified
017300100223     d                                     inz
017400100223     d ds_CNACO      e ds                  extname(CNACO00F) qualified
017500100223     d                                     inz
017600100223     d ds_CNIND      e ds                  extname(CNIND00F) qualified
017700100223     d                                     inz
017800100223     d ds_CNCLP      e ds                  extname(CNCLP00F) qualified
017900100223     d                                     inz
018000100223     d ds_FNCLS      e ds                  extname(FNCLS00F) qualified
018100100223     d                                     inz
018200100223      /copy gaitrasrc/srcProtoPR,TIBS69R
018300100222
018400100223       // -?Parametri API QCAPCMD (Process Commands)?
018500100223     d Qcmd            s           2048    inz  varying
018600100223      /copy qSysInc/qRpgleSrc,QCAPCMD
018700100223      /copy gaitrasrc/srcProtoPR,QCAPCMD
018800100223
018900100223       // -?Parametri gestione errori API.?
019000100223      /copy qsysinc/qrpglesrc,QUSEC
019100100222
019200100222       //--------------------------------------------------------------
019300100222       //?Definizione key-list.                                        ?
019400100222       //--------------------------------------------------------------
019500100222
019600100222       // -?File FNORM01L?
019700100222     d k04fnorm01    e ds                  extname(FNORM01L : *key)
019800100222     d                                     prefix(k_)   inz
019900100222
020000100222       // -?File FNORE01L?
020100100222     d k05fnore01    e ds                  extname(FNORE01L : *key)
020200100222     d                                     prefix(k_)   inz
020300100324
020400100324       // -?File TNTBE01L?
020500100324     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
020600100324     d                                     prefix(k_)   inz
020700100324
020800100324       // -?File TABEL00F?
020900100324     d k03tabel00    e ds                  extname(TABEL00F : *key)
021000100324     d                                     prefix(k_)   inz
021100100222
021200100222       //--------------------------------------------------------------
021300100222       //?M A I N - L I N E                                            ?
021400100222       //--------------------------------------------------------------
021500100222
021600100222     c     *Entry        plist
021700100222     c                   parm                    KPJBA
021800100224     c                   parm                    FIOR52ds
021900100222
022000100222      /free
022100100222
022200100222       // -?Operazioni iniziali?
022300100222       exsr  sr_RoutInz;
022400100222
022500100223       // -?Stampa (ed invio) e-mail?
022600100222       exsr  sr_PrtEmail;
022700100222
022800100222       // -?Operazioni finali?
022900100222       exsr  sr_RoutEnd;
023000100222
023100100222       //--------------------------------------------------------------
023200100222       //?Operazioni iniziali.                                         ?
023300100222       //--------------------------------------------------------------
023400100222       BEGSR  sr_RoutInz;
023500100222
023600100222         *inLR = *on;
023700100223
023800100223         // -?Impostazione parametri ricevuti?
023900100224         if  %parms() > 1;
024000100225           O52err = *off;
024100100224           clear  O52msg;
024200100224         endif;
024300100226
024400100226         // -?Pulizia inziale campi di comodo?
024500100226         clear  Save_IndEmail;
024600100222
024700100222         // -?Reperimento dati del lavoro?
024800100222         exsr  sr_DatiJob;
024900100224
025000100226         // -?*-=»>  Solo TEST in stampa  <«=-*?
025100100224         if  %parms() = 1;
025200100224           open  PrtEMAIL;
025300100224           open  FIOR52P;
025400100224           exsr  sr_TEST;
025500100224           leavesr;
025600100224         endif;
025700100222
025800100223         // -?Reperimento tab. "MRA"?
025900100224         exsr  sr_TabMRA;
026000100223
026100100223         // -?Reperimento dati O.R.M.?
026200100224         exsr  sr_RepFNORM;
026300100223
026400100223         // -?Reperimento indirizzo email del destinatario da O.R.M.?
026500100224         exsr  sr_RepFNORE;
026600100224
026700100224         // -?Reperimento ultimi dati (decodifiche)?
026800100224         exsr  sr_AltriDati;
026900100223
027000100223         // -?Reperimento nuovo progressivo da numeratore "024"?
027100100224         exsr  sr_Num024;
027200100223
027300100223         // -?Override ai file di stampa (per impostarvi i dati per?
027400100223         //  ?l'invio via e-mail)?
027500100224         exsr sr_Override;
027600100223
027700100223         // -?Apertura file di stampa?
027800100223         open  PrtEMAIL;
027900100223         open  FIOR52P;
028000100222
028100100222       ENDSR;
028200100222
028300100222       //--------------------------------------------------------------
028400100222       //?Reperimento Dati del job (Utente/Operativi).                 ?
028500100222       //--------------------------------------------------------------
028600100222       BEGSR  sr_DatiJob;
028700100222
028800100222         in(E) §AzUte;
028900100222         if NOT %error;
029000100222           in(E) §DatiUte;
029100100222         endif;
029200100222         if %error or RSut = *blanks;
029300100222           clear TIBS34ds;
029400100222           tibs34r(tibs34ds);
029500100222           in §AzUte;
029600100222           in §DatiUte;
029700100222         endif;
029800100222
029900100222       ENDSR;
030000100224
030100100224       //--------------------------------------------------------------
030200100224       //?Reperimento dati da tab. "MRA".                              ?
030300100224       //--------------------------------------------------------------
030400100224       BEGSR  sr_TabMRA;
030500100224
030600100224         clear  dMRAor52;
030700100324
030800100324         k_TBEcod = 'MRA';
030900100324         k_TBEke1 = 'FIOR52R';
031000100324         k_TBEke2 = 'CEVA';
031100100324
031200100324         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
031300100324
031400100324         if  %found(TNTBE01L)   and   TBEatb = *blank;
031500100324           dMRAor52 = TBEuni;
031600100324         else;
031700100324           O52err = *on;
031800100324           O52msg = 'Parametri "' + %trimr(k_TBEke1) +
031900100324                    '" non reperiti in tab. "MRA"';
032000100324           exsr  sr_RoutEnd;
032100100324         endif;
032200100224
032300100224       ENDSR;
032400100224
032500100224       //--------------------------------------------------------------
032600100224       //?Reperimento dati O.R.M. (file FNORM00F)                      ?
032700100224       //--------------------------------------------------------------
032800100224       BEGSR  sr_RepFNORM;
032900100224
033000100224         //clear  k04fnorm01;          (già così)
033100100224         k_ORMpoe = I52poe;
033200100224         k_ORMnsr = I52nsr;
033300100224         k_ORMnor = I52nor;
033400100224         k_ORMnrv = I52nrv;
033500100224         chain  %kds( k04fnorm01 )  FNORM000;
033600100224         if  NOT %found(FNORM01L)   or   ORMatb <> *blank;
033700100224           O52err = *on;
033800100224           O52msg = 'Non trovato O.R.M. ' + %editc( I52poe : 'X' ) +
033900100224                    '-' + %trim( %editc( I52nsr : '3' ) ) +
034000100224                    '-' + %trim( %editc( I52nor : '3' ) ) +
034100100224                    '-' + %trim( %editc( I52nrv : '3' ) );
034200100224           exsr  sr_RoutEnd;
034300100224         endif;
034400100224
034500100224       ENDSR;
034600100224
034700100224       //--------------------------------------------------------------
034800100224       //?Reperimento dati aggiuntivi O.R.M. (file FNORE00F)           ?
034900100224       //--------------------------------------------------------------
035000100224       BEGSR  sr_RepFNORE;
035100100317
035200100317         // -?Reperimento estensione ORM?
035300100317         w_OREtrc = 'G ';
035400100317         exsr sr_ChainFNORE;
035500100317         dOREgen = OREdati;
035600100317         clear  ds5E;
035700100317         if  dOREgen.§OREtsp <> *blank;
035800100324           k_TBLkut = c_Kut;
035900100324           k_TBLcod = '5E';
036000100324           k_TBLkey = dOREgen.§OREtsp;
036100100324           chain  %kds( k03tabel00 )  TABEL;
036200100324           if  %found(TABEL00F)   and   TBLflg = *blank;
036300100324             ds5E = TBLuni;
036400100324           endif;
036500100317         endif;
036600100224
036700100225         // -?Reperimento indirizzo e-mail del destinatario?
036800100225         w_OREtrc = 'M ';
036900100225         exsr sr_ChainFNORE;
037000100226         if  %found(FNORE01L);
037100100226          Save_IndEmail = OREdati;
037200100226         else;
037300100224           O52err = *on;
037400100224           O52msg = 'Non trovato indirizzo e-mail del destinatario da ORM '+
037500100224                    %editc( I52poe : 'X' ) + '-' +
037600100224                    %trim( %editc( I52nsr : '3' ) ) + '-' +
037700100224                    %trim( %editc( I52nor : '3' ) ) + '-' +
037800100224                    %trim( %editc( I52nrv : '3' ) );
037900100226           exsr  sr_RoutEnd;
038000100224         endif;
038100100225
038200100225         // -?Reperimento note ritiro?
038300100225         w_OREtrc = 'N ';
038400100225         exsr sr_ChainFNORE;
038500100226         Save_Note = OREdati;
038600100224
038700100224       ENDSR;
038800100225
038900100225       //--------------------------------------------------------------
039000100225       //?Aggancio record specifico in dati aggiuntivi O.R.M.          ?
039100100225       //--------------------------------------------------------------
039200100225       BEGSR  sr_ChainFNORE;
039300100225
039400100225         clear  OREdati;
039500100225
039600100225         clear  k05fnore01;
039700100225         k_OREpoe = I52poe;
039800100225         k_OREnsr = I52nsr;
039900100225         k_OREnor = I52nor;
040000100225         k_OREnrv = I52nrv;
040100100225         k_OREtrc = w_OREtrc;
040200100225
040300100225         chain  %kds( k05fnore01 )  FNORE000;
040400100225
040500100225       ENDSR;
040600100224
040700100224       //--------------------------------------------------------------
040800100224       //?Reperimento altri dati (decodifiche).                        ?
040900100224       //--------------------------------------------------------------
041000100224       BEGSR  sr_AltriDati;
041100100224
041200100224         // -?Decodifica cliente pagante?
041300100224         clear  TIBS69ds;
041400100224         TIBS69ds.I69sif = knsif;
041500100224         TIBS69ds.I69kcc = DUTkci;
041600100224         TIBS69ds.I69kac = ORMksc;
041700100224         tibs69r( TIBS69ds :
041800100224                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
041900100224
042000100224         // -?Dati anagrafici della filiale di ritiro?
042100100224         chain  (ORMpor)  AZORG;
042200100224         if  NOT %found(AZORG01L)   or   ORGfva <> *blank;
042300100224           clear  ORGloc;
042400100224           clear  ORGind;
042500100224           clear  ORGpro;
042600100224           clear  ORGtel;
042700100224         endif;
042800100224
042900100224       ENDSR;
043000100224
043100100224       //--------------------------------------------------------------
043200100224       //?Reperimento progressivo contatore "024"                      ?
043300100224       //--------------------------------------------------------------
043400100224       BEGSR  sr_Num024;
043500100224
043600100224         reset  TRUL33ds;
043700100224
043800100224         kpjbu  = TRUL33ds;
043900100224         trul33r ( KPJBA );
044000100224         TRUL33ds = kpjbu;
044100100224
044200100224         if  O33err <> *zero;
044300100224           O52err = *on;
044400100224           O52msg = 'Fallito reperimento numeratore "' +
044500100224                    %editc(I33cnu : 'X') + '"';
044600100224           exsr  sr_RoutEnd;
044700100224         endif;
044800100224
044900100224       ENDSR;
045000100224
045100100224       //--------------------------------------------------------------
045200100224       //?Impostazione dati per stampa di prova.                       ?
045300100224       //--------------------------------------------------------------
045400100224       BEGSR  sr_TEST;
045500100224
045600100224         wDate_EUR = %date();
045700100224         ORMdar = %dec( wDate_EUR : *iso );
045800100224
045900100224         ORMorr = 6789;
046000100224
046100100224         ds_CNACO.ACOrag = *all'W';
046200100224
046300100224         ORGloc = *all'W';
046400100224         ORGind = *all'X';
046500100224         ORGpro = *all'W';
046600100224         ORGtel = '1234/67890123';
046700100224
046800100224         ORMpoe = 321;
046900100224         ORMnsr = 98;
047000100224         ORMnor = 1234567;
047100100224         ORMnrv = 89;
047200100224
047300100224         ORMpkg = 123456,7;
047400100224         ORMncl = 54321;
047500100224         ORMvlm = 98,765;
047600100224
047700100224         ORMrer = *all'W';
047800100224         ORMrsr = *all'W';
047900100224         ORMinr = *all'X';
048000100224         ORMcar = '12345____';
048100100224         ORMlor = *all'W';
048200100224         ORMprr = *all'W';
048300100224         ORMter = '1234/67890123456';
048400100224
048500100224         ORMrsc = *all'W';
048600100224         ORMinc = *all'X';
048700100224         ORMcac = '12345____';
048800100224         ORMloc = *all'W';
048900100224         ORMprc = *all'W';
049000100224
049100100224         ORMrfa = *all'W';
049200100317
049300100317         dOREgen.§OREtsp = 'Q';
049400100317         §5Edes = *all'W';
049500100224
049600100224         OREdati = *all'W';
049700100226         Save_Note = OREdati;
049800100224
049900100224         ORMrso = *all'W';
050000100224         ORMino = *all'X';
050100100224         ORMcao = '12345____';
050200100224         ORMloo = *all'W';
050300100224         ORMpro = *all'W';
050400100224
050500100224       ENDSR;
050600100223
050700100223       //--------------------------------------------------------------
050800100223       //?Preparazione ed invio e-mail.                                ?
050900100223       //--------------------------------------------------------------
051000100223       BEGSR  sr_PrtEmail;
051100100223
051200100223         wDate_EUR = %date( ORMdar : *iso );
051300100223         wDate = %dec( wDate_EUR );
051400100223
051500100223         // -?Stampa "corpo e-mail"?
051600100223         For  xx = 1  To  20;
051700100223           O_txt = '§§Tag' + %editc( xx : 'X') + '§§#';
051800100223           select;
051900100223             when  xx = 1;
052000100223               O_txt = %trimr(O_txt) + %trim( ORMrsr );
052100100223             when  xx = 2;
052200100223               O_txt = %trimr(O_txt) + %trim( ORMrso );
052300100223             when  xx = 3;
052400100223               O_txt = %trimr(O_txt) + %trim( ORMrsr );
052500100223             when  xx = 4;
052600100223               O_txt = %trimr(O_txt) + %trim( ORMinr );
052700100223             when  xx = 5;
052800100223               O_txt = %trimr(O_txt) + %trim( ORMcar );
052900100223             when  xx = 6;
053000100223               O_txt = %trimr(O_txt) + %trim( ORMlor );
053100100223             when  xx = 7;
053200100223               O_txt = %trimr(O_txt) + %trim( ORMprr );
053300100223             when  xx = 8;
053400100223               O_txt = %trimr(O_txt) + %trim( ORMrer );
053500100223             when  xx = 9;
053600100223               O_txt = %trimr(O_txt) + %triml( %editc( wDate : 'Y' ) );
053700100223             when  xx = 10;
053800100223               O_txt = %trimr(O_txt) + %triml( %editw( ORMorr : '0 :  ' ) );
053900100223             when  xx = 11;
054000100223               O_txt = %trimr(O_txt) + %triml( ORMrso );
054100100223             when  xx = 12;
054200100223               O_txt = %trimr(O_txt) + %triml( ds_CNACO.ACOrag );
054300100223             when  xx = 13;
054400100223               O_txt = %trimr(O_txt) + %triml( ORGloc );
054500100223             when  xx = 14;
054600100223               O_txt = %trimr(O_txt) + %triml( ORGind );
054700100223             when  xx = 15;
054800100223               O_txt = %trimr(O_txt) + %triml( ORGpro );
054900100223             when  xx = 16;
055000100223               O_txt = %trimr(O_txt) + %triml( ORGtel );
055100100223             when  xx = 17;
055200100223               O_txt = %trimr(O_txt) + %editc( ORMpoe : 'X' );
055300100223             when  xx = 18;
055400100223               O_txt = %trimr(O_txt) + %triml( %editc( ORMnsr : '3' ) );
055500100223             when  xx = 19;
055600100223               O_txt = %trimr(O_txt) + %triml( %editc( ORMnor : '3' ) );
055700100223             when  xx = 20;
055800100223               O_txt = %trimr(O_txt) + %triml( %editc( ORMnrv : '3' ) );
055900100223           endsl;
056000100223           O_txt = %trimr(O_txt) + '}{';
056100100223           except  PRTparm;
056200100223         EndFor;
056300100223
056400130627         // -?Impostazione variabili "allegato e-mail"?
056500130627         // -?Dati dell'O.R.M.?
056600100223         pBCO = BarCodeORMds;
056700100223         pDAR = wDate;
056800100223         pPKG = ORMpkg;
056900100223         pNCL = ORMncl;
057000100223         pVLM = ORMvlm;
057100100223         // -?Dati del mittente?
057200100223         pRER = ORMrer;
057300100223         pRSR = ORMrsr;
057400100223         pINR = ORMinr;
057500100223         pCAR = ORMcar;
057600100223         pLOR = ORMlor;
057700100223         pPRR = '(' + ORMprr + ')';
057800100223         pTER = ORMter;
057900100223         // -?Dati del destinatario?
058000100223         pRSC = ORMrsc;
058100100223         pINC = ORMinc;
058200100223         pCAC = ORMcac;
058300100223         pLOC = ORMloc;
058400100223         pPRC = '(' + ORMprc + ')';
058500100223         // -?Riferimento mittente?
058600100223         pRFA = ORMrfa;
058700100317         // -?Tipo servizio?
058800100317         if  dOREgen.§OREtsp <> *blank;
058900100317           pkTSP = 'Tipo servizio';
059000100317           pTSP = dOREgen.§OREtsp + ' - ' + §5Edes;
059100100317         endif;
059200100223         // -?Note ritiro?
059300100226         pNOTE = Save_Note;
059400100223         // -?Dati del commissionante?
059500100223         pRSO = ORMrso;
059600100223         pINO = ORMino;
059700100223         pCAO = ORMcao;
059800100223         pLOO = ORMloo;
059900100223         pPRO = '(' + ORMpro + ')';
060000100223
060100130627         // -?Stampa "allegato e-mail"?
060200100226         write  OR52corpo;
060300100223         write  OR52box;
060400100223         write  OR52piede;
060500100223
060600100223       ENDSR;
060700100222
060800100222       //--------------------------------------------------------------
060900100222       //?Override al file di stampa per impostarvi i dati per         ?
061000100223       //?  l'invio via e-mail.                                        ?
061100100222       //--------------------------------------------------------------
061200100222       BEGSR sr_Override;
061300100222
061400100222         reset TRTCM1ds;
061500100308         §CM1mitt = §MRAmitt;
061600100226         §CM1dst  = %trim(Save_IndEmail);
061700100223         §CM1po   = %editc( DUTpou : 'X' );
061800100329         §CM1var  = %trimr(§CM1var) + %trim(§MRAobjm) + ' - ORM ' +
061900100329                    %editc( ORMpoe : 'X' ) + '-' +
062000100329                    %triml( %editc( ORMnsr : '3' ) ) + '-' +
062100100329                    %triml( %editc( ORMnor : '3' ) ) + '-' +
062200100329                    %triml( %editc( ORMnrv : '3' ) );
062300100223         §CM1sts  = §MRAsts;
062400100223         §CM1idp  = §MRAidpro;
062500100223         §CM1prog = %subst( %editc( O33nri : 'X' ) :
062600100223                            %size(O33nri) - %size(§CM1prog) + 1 );
062700100222
062800100223         // -?Override al "corpo e-mail"?
062900100223         if  §MRAregCor <> *blank;
063000100223           §CM1tips = §MRAregCor;
063100100223           §CM1att  = '02';
063200100226           Qcmd = c_CmdOvrPrtF_1
063300100223                + ' outq(' + %trim(§MRAoutqi) + ')'
063400100222                + ' usrdfndta(''' + TRTCM1ds + ''')';
063500100223         else;
063600100226           Qcmd = c_CmdOvrPrtF_1;
063700100222         endif;
063800100222
063900100223         exsr  sr_ExecCmd;
064000100223
064100100223         if  Qusei <> *blank;
064200100223           O52err = *on;
064300100223           O52msg = 'Fallita override al file di stampa per +
064400100223                     "corpo e-mail"';
064500100223           exsr  sr_RoutEnd;
064600100223         endif;
064700100223
064800100223         // -?Override all'"allegato e-mail"?
064900100223         if  §MRAregAll <> *blank;
065000100223           §CM1tips = §MRAregAll;
065100100223           §CM1att  = '01';
065200100226           Qcmd = c_CmdOvrPrtF_2
065300100223                + ' outq(' + %trim(§MRAoutqi) + ')'
065400100223                + ' usrdfndta(''' + TRTCM1ds + ''')';
065500100223         else;
065600100226           Qcmd = c_CmdOvrPrtF_2;
065700100223         endif;
065800100223
065900100223         exsr  sr_ExecCmd;
066000100223
066100100223         if  Qusei <> *blank;
066200100223           O52err = *on;
066300100223           O52msg = 'Fallita override al file di stampa per +
066400100223                     "allegato e-mail"';
066500100223           exsr  sr_RoutEnd;
066600100223         endif;
066700100222
066800100222       ENDSR;
066900100222
067000100222       //--------------------------------------------------------------
067100100223       //?Chiusura file stampa & Cancellazione override.               ?
067200100222       //--------------------------------------------------------------
067300100222       BEGSR sr_DeleteOvr;
067400100222
067500100223         // -?Cancellazine override al "corpo e-mail"?
067600100223         if %open(PrtEMAIL);
067700100223           close PRTEMAIL;
067800100223           Qcmd = c_CmdDltOvr_1;
067900100223           exsr  sr_ExecCmd;
068000100223         endif;
068100100223
068200100223         // -?Cancellazione override all'"allegato e-mail"?
068300100222         if  %open(FIOR52P);
068400100222           close FIOR52P;
068500100223           Qcmd = c_CmdDltOvr_2;
068600100223           exsr  sr_ExecCmd;
068700100222         endif;
068800100222
068900100222       ENDSR;
069000100223
069100100223       //--------------------------------------------------------------
069200100223       //?Esecuzione del comando (già impostato).                      ?
069300100223       //--------------------------------------------------------------
069400100223       BEGSR  sr_ExecCmd;
069500100223
069600100223         clear Qcap0100;
069700100223         Qcabcsdh = *off;
069800100223         Qcapa    = *off;
069900100223         Qcacmdss = *off;
070000100223         Qcaerved = *allX'00';
070100100223
070200100223         clear Qusec;
070300100223         Qusbprv  = %size(Qusec);
070400100223
070500100223         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
070600100223                           %size(Qcap0100) : 'CPOP0100' : *omit :
070700100223                           0 : 0 : Qusec);
070800100223
070900100223         //if  Qusei <> *blank;
071000100223         //  ...;
071100100223         //endif;
071200100223
071300100223       ENDSR;
071400100222
071500100222       //--------------------------------------------------------------
071600100222       //?Operazioni finali.                                           ?
071700100222       //--------------------------------------------------------------
071800100222       BEGSR  sr_RoutEnd;
071900100222
072000100224         if  %parms() > 1;
072100100224
072200100224           // -?Chiusura file stampa & Cancellazione override?
072300100224           exsr sr_DeleteOvr;
072400100224
072500100224           // -?Chiusura applicazioni prima richiamate?
072600100224           clear  TIBS69ds;
072700100224           TIBS69ds.I69tla = 'C';
072800100224           tibs69r( tibs69ds :
072900100224                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
073000100224
073100100224         endif;
073200100223
073300100223         // -?Uscita?
073400100223         return;
073500100222
073600100222       ENDSR;
073700100222
073800100222      /end-free
073900100223
074000100223       //--------------------------------------------------------------
074100100223       //?Specifiche di stampa.                                        ?
074200100223       //--------------------------------------------------------------
074300100223
074400100223     oPrtEMAIL  e            PRTparm     1
074500100223     o                       O_txt
